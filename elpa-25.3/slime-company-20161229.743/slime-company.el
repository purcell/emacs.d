;;; slime-company.el --- slime completion backend for company mode -*-lexical-binding:t-*-
;;
;; Copyright (C) 2009-2015  Ole Arndt
;;
;; Author: Ole Arndt <anwyn@sugarshark.com>
;; Keywords: convenience, lisp, abbrev
;; Package-Version: 20161229.743
;; Version: 1.1
;; Package-Requires: ((slime "2.13") (company "0.9.0"))
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This is a backend implementation for the completion package
;; company-mode by Nikolaj Schumacher. More info about this package
;; is available at http://company-mode.github.io/
;;
;; As of version 1.0 this completion backend supports the normal and
;; the fuzzy completion modes of SLIME.
;;
;;; Installation:
;;
;;  Put this file somewhere into your load-path
;;  (or just into slime-path/contribs) and then call
;;
;;   (slime-setup '(slime-company))
;;
;; I also have the following, IMO more convenient key bindings for
;; company mode in my .emacs:
;;
;;   (define-key company-active-map (kbd "\C-n") 'company-select-next)
;;   (define-key company-active-map (kbd "\C-p") 'company-select-previous)
;;   (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
;;   (define-key company-active-map (kbd "M-.") 'company-show-location)
;;
;;; Code:

(require 'slime)
(require 'company)
(require 'cl-lib)
(require 'eldoc)

(eval-when-compile
  (require 'cl))

(define-slime-contrib slime-company
  "Interaction between slime and the company completion mode."
  (:license "GPL")
  (:authors "Ole Arndt <anwyn@sugarshark.com>")
  (:swank-dependencies swank-arglists)
  (:on-load
   (dolist (h '(slime-mode-hook slime-repl-mode-hook sldb-mode-hook))
     (add-hook h 'slime-company-maybe-enable)))
  (:on-unload
   (dolist (h '(slime-mode-hook slime-repl-mode-hook sldb-mode-hook))
     (remove-hook h 'slime-company-maybe-enable))
   (slime-company-disable)))

;;; ----------------------------------------------------------------------------
;;; * Customization

(defgroup slime-company nil
  "Interaction between slime and the company completion mode."
  :group 'company
  :group 'slime)

(defcustom slime-company-after-completion nil
  "What to do after a successful completion.
In addition to displaying the arglist slime-company will also do one of:

- `nil':  nothing,
- insert a space. Useful if space does not select the completion candidate.
  Works best if you also call `delete-horizontal-space' before closing
  parentheses to remove excess whitespace.
- call an arbitrary function with the completion string as the first parameter.
"
  :group 'slime-company
  :type '(choice
          (const :tag "Do nothing" nil)
          (const :tag "Insert space" slime-company-just-one-space)
          (function :tag "Custom function" nil)))

(defcustom slime-company-transform-arglist 'downcase
  "Before echoing the arglist it is passed to this function for transformation."
  :group 'slime-company
  :type '(choice
          (const :tag "Downcase" downcase)
          (const :tag "Do nothing" identity)
          (function :tag "Custom function" nil)))

(defcustom slime-company-completion 'simple
  "Which Slime completion to use: `simple' or `fuzzy'.

`simple' just displays the completion candidate,
`fuzzy' also displays the classification flags as an annotation,
alignment of annotations via `company-tooltip-align-annotations'
is recommended.
"
  :group 'slime-company
  :type '(choice
          (const simple)
          (const fuzzy)))

(defcustom slime-company-complete-in-comments-and-strings nil
  "Should slime-company also complete in comments and strings."
  :group 'slime-company
  :type 'boolean)

(defcustom slime-company-major-modes
  '(lisp-mode clojure-mode slime-repl-mode scheme-mode)
  "List of major modes in which slime-company should be active.
Slime-company actually calls `derived-mode-p' on this list, so it will
be active in derived modes as well."
  :group 'slime-company
  :type '(repeat symbol))

(defun slime-company-just-one-space (_)
  (just-one-space))

(defsubst slime-company-active-p ()
  "Test if the slime-company backend should be active in the current buffer."
  (apply #'derived-mode-p slime-company-major-modes))

;;; ----------------------------------------------------------------------------
;;; * Activation

(defun slime-company-maybe-enable ()
  (when (slime-company-active-p)
    (company-mode 1)
    (add-to-list 'company-backends 'company-slime)
    (unless (slime-find-contrib 'slime-fuzzy)
      (setq slime-company-completion 'simple))))

(defun slime-company-disable ()
  (setq company-backends (remove 'company-slime company-backends)))

;;; ----------------------------------------------------------------------------
;;; * Internals

(defun slime-company--fetch-candidates-async (prefix)
  (when (slime-connected-p)
    (ecase slime-company-completion
      (simple (slime-company--fetch-candidates-simple prefix))
      (fuzzy (slime-company--fetch-candidates-fuzzy prefix)))))

(defun slime-company--fetch-candidates-simple (prefix)
  (let ((slime-current-thread t))
    (lexical-let ((package (slime-current-package))
                  (prefix prefix))
      (cons :async (lambda (callback)
                     (lexical-let ((callback callback))
                       (slime-eval-async
                           `(swank:simple-completions ,prefix ',package)
                         (lambda (result)
                           (funcall callback (car result)))
                         package)))))))

(defun slime-company--fetch-candidates-fuzzy (prefix)
  (let ((slime-current-thread t))
    (lexical-let ((package (slime-current-package))
                  (prefix prefix))
      (cons :async
            (lambda (callback)
              (lexical-let ((callback callback))
                (slime-eval-async
                    `(swank:fuzzy-completions ,prefix ',package)
                  (lambda (result)
                    (funcall callback
                             (mapcar
                              (lambda (completion)
                                (cl-destructuring-bind (sym score _ flags)
                                    completion
                                  (propertize sym 'score score 'flags flags)))
                              (car result))))
                  package)))))))

(defun slime-company--fontify-buffer ()
  "Return a buffer in lisp-mode usable for fontifying lisp expressions."
  (let ((buffer-name " *slime-company-fontify*"))
    (or (get-buffer buffer-name)
        (with-current-buffer (get-buffer-create buffer-name)
          (unless (derived-mode-p 'lisp-mode)
            ;; Advice from slime: Just calling (lisp-mode) will turn slime-mode
            ;; on in that buffer, which may interfere with the calling function
            (setq major-mode 'lisp-mode)
            (lisp-mode-variables t))
          (current-buffer)))))

(defun slime-company--fontify (string)
  "Fontify STRING as `font-lock-mode' does in Lisp mode."
  ;; copied functionality from slime, trimmed somewhat
  (with-current-buffer (slime-company--fontify-buffer)
    (erase-buffer)
    (insert (funcall slime-company-transform-arglist string))
    (let ((font-lock-verbose nil))
      (font-lock-fontify-region (point-min) (point-max)))
    (goto-char (point-min))
    (buffer-substring (point-min) (point-max))))

(defun slime-company--format (doc)
  (let ((doc (slime-company--fontify doc)))
    (cond ((eq eldoc-echo-area-use-multiline-p t) doc)
	  (t (slime-oneliner (replace-regexp-in-string "[ \n\t]+" " " doc))))))

(defun slime-company--arglist (arg)
  (let ((arglist (slime-eval
                  `(swank:operator-arglist ,arg ,(slime-current-package)))))
    (when arglist
      (slime-company--format arglist))))

(defun slime-company--echo-arglist (arg)
  (slime-eval-async `(swank:operator-arglist ,arg ,(slime-current-package))
    (lambda (arglist)
      (when arglist
        (slime-message "%s" (slime-company--format arglist))))))

(defun slime-company--doc-buffer (candidate)
  (let ((doc (slime-eval `(swank:describe-symbol ,candidate))))
    (with-current-buffer (company-doc-buffer)
      (insert doc)
      (goto-char (point-min))
      (current-buffer))))

(defun slime-company--location (candidate)
  (let ((source-buffer (current-buffer)))
    (save-window-excursion
      (slime-edit-definition candidate)
      (let ((buffer (if (eq source-buffer (current-buffer))
                        slime-xref-last-buffer
                      (current-buffer))))
        (when (buffer-live-p buffer)
          (cons buffer (with-current-buffer buffer
                         (point))))))))

(defun slime-company--post-completion (candidate)
  (slime-company--echo-arglist candidate)
  (when slime-company-after-completion
    (funcall slime-company-after-completion candidate)))

;;; ----------------------------------------------------------------------------
;;; * Company backend function

(defun company-slime (command &optional arg &rest ignored)
  "Company mode backend for slime."
  (cl-case command
    (init
     (slime-company-active-p))
    (prefix
     (when (and (slime-company-active-p)
                (slime-connected-p)
                (or slime-company-complete-in-comments-and-strings
                    (null (company-in-string-or-comment))))
       (company-grab-symbol)))
    (candidates
     (slime-company--fetch-candidates-async (substring-no-properties arg)))
    (meta
     (slime-company--arglist (substring-no-properties arg)))
    (annotation
     (concat " " (get-text-property 0 'flags arg)))
    (doc-buffer
     (slime-company--doc-buffer (substring-no-properties arg)))
    (location
     (slime-company--location (substring-no-properties arg)))
    (post-completion
     (slime-company--post-completion (substring-no-properties arg)))
    (sorted
     (eq slime-company-completion 'fuzzy))))

(provide 'slime-company)

;;; slime-company.el ends here
