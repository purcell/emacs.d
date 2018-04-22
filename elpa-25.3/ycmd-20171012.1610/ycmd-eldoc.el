;;; ycmd-eldoc.el --- Eldoc support for ycmd-mode    -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017  Peter Vasil

;; Author: Peter Vasil <mail@petervasil.net>
;; URL: https://github.com/abingham/emacs-ycmd
;; Version: 0.2
;; Package-Requires: ((ycmd "1.3") (deferred "0.5.1") (s "1.11.0") (dash "2.13.0") (let-alist "1.0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To use this package, add these lines to your init.el file:
;;
;;     (require 'ycmd-eldoc)
;;     (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)
;;

;;; Code:

(eval-when-compile
  (require 'let-alist))
(require 'eldoc)
(require 'ycmd)
(require 'deferred)
(require 'dash)
(require 's)

(defgroup ycmd-eldoc nil
  "Eldoc support for `ycmd-mode'."
  :group 'ycmd
  :group 'eldoc)

(defcustom ycmd-eldoc-always-semantic-server-query-modes
  '(not c-mode c++-mode objc-mode)
  "Modes for which `ycmd-eldoc' always queries semantic completion.

If t, the ycmd server query is always semantic.  If a list, server
query is semantic for all `major-mode' symbols in that list.  If
the `car' of the list is `not', server query is sematic for all
`major-mode' symbols _not_ in that list.  If nil, the server query
is only semantic after a semantic trigger."
  :type 'list)

(defvar-local ycmd-eldoc--cache (make-vector 2 nil))

(defvar-local ycmd-eldoc--cached-get-type-command 'none)

(defun ycmd-eldoc--documentation-function ()
  "Eldoc function for `ycmd-mode'."
  (when (and ycmd-mode (not (ycmd-parsing-in-progress-p)))
    (deferred:$
      (ycmd-eldoc--check-if-semantic-completer-exists-for-mode)
      (deferred:nextc it
        (lambda (response)
          (when response
            (ycmd-eldoc--info-at-point))))
      (deferred:nextc it
        (lambda (text)
          (eldoc-message text))))
    ;; Don't show deferred object as ElDoc message
    nil))

(defun ycmd-eldoc--check-if-semantic-completer-exists-for-mode ()
  "Return a deferred object whose return value is t if semantic completer exists."
  (deferred:$
    (deferred:next
      (lambda ()
        (ycmd-semantic-completer-available-p)))
    (deferred:nextc it
      (lambda (response)
        (when (and response (eq response 'none))
          (message (concat "No semantic completer exists for major-mode: `%s'."
                           " Ycmd ELDoc mode disabled in current buffer.")
                   major-mode)
          (ycmd-eldoc-mode -1))
        (eq response t)))))

(defun ycmd-eldoc-always-semantic-server-query-p ()
  "Check whether server query should be semantic."
  (pcase ycmd-eldoc-always-semantic-server-query-modes
    (`t t)
    (`(not . ,modes) (not (memq major-mode modes)))
    (modes (memq major-mode modes))))

(defmacro ycmd-eldoc--with-point-at-func-name (body)
  "Move cursor to function name and evluate BODY."
  (declare (indent 0) (debug t))
  `(save-excursion
     (ycmd-eldoc--goto-func-name)
     ,body))

(defun ycmd-eldoc--info-at-point ()
  "Get function info at point."
  (let ((symbol (ycmd-eldoc--with-point-at-func-name (symbol-at-point))))
    (if (and symbol (eq symbol (aref ycmd-eldoc--cache 0)))
        (aref ycmd-eldoc--cache 1)
      (deferred:$
        (deferred:next
          (lambda ()
            (when symbol
              (ycmd-eldoc--with-point-at-func-name
                (let ((ycmd-force-semantic-completion
                       (or ycmd-force-semantic-completion
                           (ycmd-eldoc-always-semantic-server-query-p))))
                  (ycmd-with-handled-server-exceptions
                      (ycmd-get-completions)))))))
        (deferred:nextc it
          (lambda (completions)
            (-when-let (candidates (cdr (assq 'completions completions)))
              (ycmd-eldoc--generate-message
               (symbol-name symbol) candidates))))
        (deferred:nextc it
          (lambda (text)
            (or text (ycmd-eldoc--get-type))))
        (deferred:nextc it
          (lambda (text)
            (when text
              (setq text (ycmd--fontify-code text))
              (ycmd-eldoc--cache-store symbol text))))))))

(defun ycmd-eldoc--cache-store (symbol text)
  "Store SYMBOL and TEXT to `ycmd-eldoc--cache'."
  (aset ycmd-eldoc--cache 0 symbol)
  ;; Store text only if we have a symbol for lookup
  (aset ycmd-eldoc--cache 1 (and symbol text))
  text)

;; Source: https://github.com/racer-rust/emacs-racer/blob/master/racer.el
(defun ycmd-eldoc--goto-func-name ()
  "If point is inside a function call, move to the function name.
foo(bar, |baz); -> foo|(bar, baz);"
  (let ((last-paren-pos (nth 1 (syntax-ppss)))
        (start-pos (point)))
    (when last-paren-pos
      ;; Move to just before the last paren.
      (goto-char last-paren-pos)
      ;; If we're inside a round paren, we're inside a function call.
      (unless (looking-at "(")
        ;; Otherwise, return to our start position, as point may have been on a
        ;; function already:
        ;; foo|(bar, baz);
        (goto-char start-pos)))))

(defun ycmd-eldoc--generate-message (symbol result)
  "Generate eldoc message for SYMBOL from RESULT."
  (-when-let* ((filtered-list
                (--filter
                 (let-alist it
                   (and (s-equals? .insertion_text symbol)
                        (or (not .extra_menu_info)
                            (not (-contains?
                                  '("[ID]" "[File]" "[Dir]" "[File&Dir]")
                                  .extra_menu_info)))))
                 result))
               (item (car filtered-list))
               (msg (or (cdr (assq 'detailed_info item))
                        (cdr (assq 'extra_menu_info item)))))
    (unless (s-blank? msg)
      (car (s-split-up-to "\n" msg 1)))))

(defun ycmd-eldoc--get-type ()
  "Get type at current position."
  (when ycmd-eldoc--cached-get-type-command
    (deferred:$
      (ycmd-eldoc--get-type-command-deferred)
      (deferred:nextc it
        (lambda (cmd)
          (when cmd
            (ycmd-with-handled-server-exceptions (ycmd--command-request cmd)
              (pcase-let ((`(,msg . ,is-type-p) (ycmd--get-message response)))
                (when is-type-p msg)))))))))

(defun ycmd-eldoc--get-type-command-deferred ()
  "Return a deferred object with the chached GetType command.
REQUEST-DATA is plist returned from `ycmd--get-request-data'."
  (if (eq ycmd-eldoc--cached-get-type-command 'none)
      (ycmd-with-handled-server-exceptions
          (ycmd--request (make-ycmd-request-data
                          :handler "defined_subcommands"))
        (setq ycmd-eldoc--cached-get-type-command
              ;; If GetTypeImprecise exists, use it in favor of GetType
              ;; because it doesn't reparse the file
              (car (-intersection '("GetTypeImprecise" "GetType")
                                  response))))
    (deferred:next nil ycmd-eldoc--cached-get-type-command)))

;;;###autoload
(defun ycmd-eldoc-setup ()
  "Setup eldoc for `ycmd-mode'."
  (interactive)
  (ycmd-eldoc-mode +1))
(make-obsolete 'ycmd-eldoc-setup 'ycmd-eldoc-mode "0.2")

(defun ycmd-eldoc--teardown ()
  "Reset `ycmd-eldoc--cache'."
  (ycmd-eldoc--cache-store nil nil)
  (setq ycmd-eldoc--cached-get-type-command 'none))

;;;###autoload
(define-minor-mode ycmd-eldoc-mode
  "Toggle ycmd eldoc mode."
  :lighter ""
  (cond
   (ycmd-eldoc-mode
    ;; For emacs < 25.1 where `eldoc-documentation-function' defaults to
    ;; nil. See also https://github.com/abingham/emacs-ycmd/issues/409
    (or eldoc-documentation-function
        (setq-local eldoc-documentation-function #'ignore))
    (add-function :before-until (local 'eldoc-documentation-function)
                  #'ycmd-eldoc--documentation-function)
    (eldoc-mode +1)
    (add-hook 'ycmd-after-teardown-hook
              #'ycmd-eldoc--teardown nil 'local))
   (t
    (eldoc-mode -1)
    (remove-function (local 'eldoc-documentation-function)
                     #'ycmd-eldoc--documentation-function)
    (remove-hook 'ycmd-after-teardown-hook
                 #'ycmd-eldoc--teardown 'local)
    (ycmd-eldoc--teardown))))

(provide 'ycmd-eldoc)

;;; ycmd-eldoc.el ends here
