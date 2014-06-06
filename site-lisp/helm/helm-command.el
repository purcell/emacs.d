;;; helm-command.el --- Helm execute-exended-command. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-mode)
(require 'helm-elisp)


(defgroup helm-command nil
  "Emacs command related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-M-x-requires-pattern 0
  "Value of requires-pattern for `helm-M-x'.
Show all candidates on startup when 0 (default)."
  :group 'helm-command
  :type 'boolean)

(defcustom helm-M-x-always-save-history nil
  "`helm-M-x' Save command in `extended-command-history' even when it fail."
  :group 'helm-command
  :type  'boolean)

(defcustom helm-M-x-reverse-history nil
  "The history source of `helm-M-x' appear in second position when non--nil."
  :group 'helm-command
  :type 'boolean)


;;; Faces
;;
;;
(defface helm-M-x-key '((t (:foreground "orange" :underline t)))
  "Face used in helm-M-x to show keybinding."
  :group 'helm-command)


(defvar helm-M-x-input-history nil)

(defvar helm-M-x-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c ?") 'helm-M-x-help)
    map)
  "Keymap for `helm-M-x'.")


(cl-defun helm-M-x-get-major-mode-command-alist (mode-map)
  "Return alist of MODE-MAP."
  (cl-loop for key being the key-seqs of mode-map using (key-bindings com)
        for str-key  = (key-description key)
        for ismenu   = (string-match "<menu-bar>" str-key)
        unless ismenu collect (cons str-key com)))

(defun helm-get-mode-map-from-mode (mode)
  "Guess the mode-map name according to MODE.
Some modes don't use conventional mode-map name
so we need to guess mode-map name. e.g python-mode ==> py-mode-map.
Return nil if no mode-map found."
  (cl-loop ;; Start with a conventional mode-map name.
        with mode-map    = (intern-soft (format "%s-map" mode))
        with mode-string = (symbol-name mode)
        with mode-name   = (replace-regexp-in-string "-mode" "" mode-string)
        while (not mode-map)
        for count downfrom (length mode-name)
        ;; Return when no result after parsing entire string.
        when (eq count 0) return nil
        for sub-name = (substring mode-name 0 count)
        do (setq mode-map (intern-soft (format "%s-map" (concat sub-name "-mode"))))
        finally return mode-map))

(defun helm-M-x-current-mode-map-alist ()
  "Return mode-map alist of current `major-mode'."
  (let ((map (helm-get-mode-map-from-mode major-mode)))
    (when (and map (boundp map))
      (helm-M-x-get-major-mode-command-alist (symbol-value map)))))


(defun helm-M-x-transformer (candidates _source)
  "filtered-candidate-transformer to show bindings in emacs commands.
Show global bindings and local bindings according to current `major-mode'."
  (with-helm-current-buffer
    (cl-loop with local-map = (helm-M-x-current-mode-map-alist)
          for cand in candidates
          for local-key  = (car (rassq cand local-map))
          for key        = (substitute-command-keys (format "\\[%s]" cand))
          collect
          (cons (cond ((and (string-match "^M-x" key) local-key)
                       (format "%s (%s)"
                               cand (propertize
                                     local-key
                                     'face 'helm-M-x-key)))
                      ((string-match "^M-x" key) cand)
                      (t (format "%s (%s)"
                                 cand (propertize
                                       key
                                       'face 'helm-M-x-key))))
                cand)
          into ls
          finally return
          (sort ls #'helm-generic-sort-fn))))

(defun helm-M-x--notify-prefix-arg ()
  ;; Notify a prefix-arg set AFTER calling M-x.
  (when prefix-arg
    (with-helm-window
      (helm-display-mode-line (helm-get-current-source) 'force))))

;;;###autoload
(defun helm-M-x ()
  "Preconfigured `helm' for Emacs commands.
It is `helm' replacement of regular `M-x' `execute-extended-command'.

Unlike regular `M-x' emacs vanilla `execute-extended-command' command,
the prefix args if needed, are passed AFTER starting `helm-M-x'.

You can get help on each command by persistent action."
  (interactive)
  (let* ((history (cl-loop for i in extended-command-history
                        when (commandp (intern i)) collect i))
         command sym-com in-help help-cand
         (helm--mode-line-display-prefarg t)
         (pers-help #'(lambda (candidate)
                        (let ((hbuf (get-buffer (help-buffer))))
                          (if (and in-help (string= candidate help-cand)
                                   (null helm-persistent-action-use-special-display))
                              (progn
                                ;; When M-x is started from a help buffer,
                                ;; Don't kill it as it is helm-current-buffer.
                                (unless (equal hbuf helm-current-buffer)
                                  (kill-buffer hbuf)
                                  (set-window-buffer (get-buffer-window hbuf)
                                                     helm-current-buffer))
                                (setq in-help nil))
                            (helm-describe-function candidate)
                            (setq in-help t))
                          (setq help-cand candidate))))
         (tm (run-at-time 1 0.1 'helm-M-x--notify-prefix-arg)))
    (setq current-prefix-arg nil)
    (unwind-protect
         (setq command (helm-comp-read
                        "M-x " obarray
                        :test 'commandp
                        :requires-pattern helm-M-x-requires-pattern
                        :name "Emacs Commands"
                        :buffer "*helm M-x*"
                        :persistent-action pers-help
                        :persistent-help "Describe this command"
                        :history history
                        :reverse-history helm-M-x-reverse-history
                        :del-input nil
                        :mode-line helm-M-x-mode-line
                        :must-match t
                        :nomark t
                        :keymap helm-M-x-map
                        :candidates-in-buffer t
                        :fc-transformer 'helm-M-x-transformer))
      (cancel-timer tm)
      (setq helm--mode-line-display-prefarg nil))
    (setq sym-com (intern command))
    (setq current-prefix-arg helm-current-prefix-arg)
    ;; Avoid having `this-command' set to *exit-minibuffer.
    (setq this-command sym-com
          ;; Handle C-x z (repeat) Issue #322
          real-this-command sym-com)
    (let ((prefix-arg current-prefix-arg))
      ;; This ugly construct is to save history even on error.
      (unless helm-M-x-always-save-history
        (command-execute sym-com 'record))
      (setq extended-command-history
            (cons command (delete command history)))
      (when helm-M-x-always-save-history
        (command-execute sym-com 'record)))))

(provide 'helm-command)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-command.el ends here
