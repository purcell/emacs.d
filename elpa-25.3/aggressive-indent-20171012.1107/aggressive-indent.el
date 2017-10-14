;;; aggressive-indent.el --- Minor mode to aggressively keep your code always indented

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; URL: https://github.com/Malabarba/aggressive-indent-mode
;; Package-Version: 20171012.1107
;; Version: 1.8.4
;; Package-Requires: ((emacs "24.1") (cl-lib "0.5"))
;; Keywords: indent lisp maint tools
;; Prefix: aggressive-indent
;; Separator: -

;;; Commentary:
;;
;; `electric-indent-mode' is enough to keep your code nicely aligned when
;; all you do is type.  However, once you start shifting blocks around,
;; transposing lines, or slurping and barfing sexps, indentation is bound
;; to go wrong.
;;
;; `aggressive-indent-mode' is a minor mode that keeps your code always
;; indented.  It reindents after every change, making it more reliable
;; than `electric-indent-mode'.
;;
;; ### Instructions ###
;;
;; This package is available fom Melpa, you may install it by calling
;;
;;     M-x package-install RET aggressive-indent
;;
;; Then activate it with
;;
;;     (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;;     (add-hook 'css-mode-hook #'aggressive-indent-mode)
;;
;; You can use this hook on any mode you want, `aggressive-indent' is not
;; exclusive to emacs-lisp code.  In fact, if you want to turn it on for
;; every programming mode, you can do something like:
;;
;;     (global-aggressive-indent-mode 1)
;;     (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
;;
;; ### Manual Installation ###
;;
;; If you don't want to install from Melpa, you can download it manually,
;; place it in your `load-path' and require it with
;;
;;     (require 'aggressive-indent)

;;; Instructions:
;;
;; INSTALLATION
;;
;; This package is available fom Melpa, you may install it by calling
;; M-x package-install RET aggressive-indent.
;;
;; Then activate it with
;;     (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;;
;; You can also use an equivalent hook for another mode,
;; `aggressive-indent' is not exclusive to emacs-lisp code.
;;
;; Alternatively, you can download it manually, place it in your
;; `load-path' and require it with
;;
;;     (require 'aggressive-indent)

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Code:

(require 'cl-lib)

(defgroup aggressive-indent nil
  "Customization group for aggressive-indent."
  :prefix "aggressive-indent-"
  :group 'electricity
  :group 'indent)

(defun aggressive-indent-bug-report ()
  "Opens github issues page in a web browser.  Please send any bugs you find.
Please include your Emacs and `aggressive-indent' versions."
  (interactive)
  (message "Your `aggressive-indent-version' is: %s, and your emacs version is: %s.
Please include this in your report!"
           (eval-when-compile
             (ignore-errors
               (require 'lisp-mnt)
               (lm-version)))
           emacs-version)
  (browse-url "https://github.com/Malabarba/aggressive-indent-mode/issues/new"))

(defvar aggressive-indent-mode)

;;; Configuring indentarion
(defcustom aggressive-indent-dont-electric-modes nil
  "List of major-modes where `electric-indent' should be disabled."
  :type '(choice
          (const :tag "Never use `electric-indent-mode'." t)
          (repeat :tag "List of major-modes to avoid `electric-indent-mode'." symbol))
  :package-version '(aggressive-indent . "0.3.1"))

(defcustom aggressive-indent-excluded-modes
  '(inf-ruby-mode
    makefile-mode
    makefile-gmake-mode
    python-mode
    text-mode
    yaml-mode)
  "Modes in which `aggressive-indent-mode' should not be activated.
This variable is only used if `global-aggressive-indent-mode' is
active.  If the minor mode is turned on with the local command,
`aggressive-indent-mode', this variable is ignored."
  :type '(repeat symbol)
  :package-version '(aggressive-indent . "1.8.4"))

(defcustom aggressive-indent-protected-commands '(undo undo-tree-undo undo-tree-redo whitespace-cleanup)
  "Commands after which indentation will NOT be performed.
Aggressive indentation could break things like `undo' by locking
the user in a loop, so this variable is used to control which
commands will NOT be followed by a re-indent."
  :type '(repeat symbol)
  :package-version '(aggressive-indent . "0.1"))

(defcustom aggressive-indent-protected-current-commands
  '(query-replace-regexp query-replace)
  "Like `aggressive-indent-protected-commands', but for the current command.
For instance, with the default value, this variable prevents
indentation during `query-replace' (but not after)."
  :type '(repeat symbol)
  :package-version '(aggressive-indent . "1.8.4"))

(defcustom aggressive-indent-comments-too nil
  "If non-nil, aggressively indent in comments as well."
  :type 'boolean
  :package-version '(aggressive-indent . "0.3"))

(defcustom aggressive-indent-modes-to-prefer-defun
  '(emacs-lisp-mode lisp-mode scheme-mode clojure-mode)
  "List of major-modes in which indenting defun is preferred.
Add here any major modes with very good definitions of
`end-of-defun' and `beginning-of-defun', or modes which bug out
if you have `after-change-functions' (such as paredit).

If current major mode is derived from one of these,
`aggressive-indent' will call `aggressive-indent-indent-defun'
after every command.  Otherwise, it will call
`aggressive-indent-indent-region-and-on' after every buffer
change."
  :type '(repeat symbol)
  :package-version '(aggressive-indent . "0.3"))

;;; Preventing indentation
(defconst aggressive-indent--internal-dont-indent-if
  '((memq last-command aggressive-indent-protected-commands)
    (memq this-command aggressive-indent-protected-current-commands)
    (region-active-p)
    buffer-read-only
    undo-in-progress
    (null (buffer-modified-p))
    (and (boundp 'smerge-mode) smerge-mode)
    (equal (buffer-name) "*ediff-merge*")
    (let ((line (thing-at-point 'line)))
      (and (stringp line)
           ;; If the user is starting to type a comment.
           (stringp comment-start)
           (string-match (concat "\\`[[:blank:]]*"
                                 (substring comment-start 0 1)
                                 "[[:blank:]]*$")
                         line)))
    (let ((sp (syntax-ppss)))
      ;; Comments.
      (or (and (not aggressive-indent-comments-too) (elt sp 4))
          ;; Strings.
          (elt sp 3))))
  "List of forms which prevent indentation when they evaluate to non-nil.
This is for internal use only.  For user customization, use
`aggressive-indent-dont-indent-if' instead.")

(eval-after-load 'yasnippet
  '(when (boundp 'yas--active-field-overlay)
     (add-to-list 'aggressive-indent--internal-dont-indent-if
                  '(and
                    (overlayp yas--active-field-overlay)
                    (overlay-end yas--active-field-overlay))
                  'append)))
(eval-after-load 'company
  '(when (boundp 'company-candidates)
     (add-to-list 'aggressive-indent--internal-dont-indent-if
                  'company-candidates)))
(eval-after-load 'auto-complete
  '(when (boundp 'ac-completing)
     (add-to-list 'aggressive-indent--internal-dont-indent-if
                  'ac-completing)))
(eval-after-load 'multiple-cursors-core
  '(when (boundp 'multiple-cursors-mode)
     (add-to-list 'aggressive-indent--internal-dont-indent-if
                  'multiple-cursors-mode)))
(eval-after-load 'iedit
  '(when (boundp 'iedit-mode)
     (add-to-list 'aggressive-indent--internal-dont-indent-if
                  'iedit-mode)))
(eval-after-load 'evil
  '(when (boundp 'iedit-mode)
     (add-to-list 'aggressive-indent--internal-dont-indent-if
                  'iedit-mode)))
(eval-after-load 'coq
  '(add-to-list 'aggressive-indent--internal-dont-indent-if
                '(and (derived-mode-p 'coq-mode)
                      (not (string-match "\\.[[:space:]]*$"
                                         (thing-at-point 'line))))))
(eval-after-load 'ruby-mode
  '(add-to-list 'aggressive-indent--internal-dont-indent-if
                '(when (derived-mode-p 'ruby-mode)
                   (let ((line (thing-at-point 'line)))
                     (and (stringp line)
                          (string-match "\\b\\(begin\\|case\\|d\\(?:ef\\|o\\)\\|if\\) *$" line))))))

(defcustom aggressive-indent-dont-indent-if '()
  "List of variables and functions to prevent aggressive indenting.
This variable is a list where each element is a Lisp form.
As long as any one of these forms returns non-nil,
aggressive-indent will not perform any indentation.

See `aggressive-indent--internal-dont-indent-if' for usage examples.

Note that this is only used once, and only on the line where the
point is when we're about to start indenting.  In order to
prevent indentation of further lines, see
`aggressive-indent-stop-here-hook'."
  :type '(repeat sexp)
  :package-version '(aggressive-indent . "0.2"))

(defcustom aggressive-indent-stop-here-hook nil
  "A hook that runs on each line before it is indented.
If any function on this hook returns non-nil, it immediately
prevents indentation of the current line and any further
lines.

Note that aggressive-indent does indentation in two stages.  The
first stage indents the entire edited region, while the second
stage keeps indenting further lines until its own logic decide to
stop.  This hook only affects the second stage.  That is, it
effectly lets you add your own predicates to the logic that
decides when to stop.

In order to prevent indentation before the first stage, see
`aggressive-indent-dont-indent-if' instead."
  :type 'hook)

(defvar aggressive-indent--error-message "One of the forms in `aggressive-indent-dont-indent-if' had the following error, I've disabled it until you fix it: %S"
  "Error message thrown by `aggressive-indent-dont-indent-if'.")

(defvar aggressive-indent--has-errored nil
  "Keep track of whether `aggressive-indent-dont-indent-if' is throwing.
This is used to prevent an infinite error loop on the user.")

(defun aggressive-indent--run-user-hooks ()
  "Safely run forms in `aggressive-indent-dont-indent-if'.
If any of them errors out, we only report it once until it stops
erroring again."
  (and aggressive-indent-dont-indent-if
       (condition-case er
           (prog1 (eval (cons 'or aggressive-indent-dont-indent-if))
             (setq aggressive-indent--has-errored nil))
         (error (unless aggressive-indent--has-errored
                  (setq aggressive-indent--has-errored t)
                  (message aggressive-indent--error-message er))))))

;;; Indenting defun
(defcustom aggressive-indent-region-function #'indent-region
  "Function called to indent a region.
It is called with two arguments, the region beginning and end."
  :risky t
  :type 'function)

;;;###autoload
(defun aggressive-indent-indent-defun (&optional l r)
  "Indent current defun.
Throw an error if parentheses are unbalanced.
If L and R are provided, use them for finding the start and end of defun."
  (interactive)
  (let ((p (point-marker)))
    (set-marker-insertion-type p t)
    (funcall aggressive-indent-region-function
             (save-excursion
               (when l (goto-char l))
               (beginning-of-defun 1) (point))
             (save-excursion
               (when r (goto-char r))
               (end-of-defun 1) (point)))
    (goto-char p)))

(defun aggressive-indent--softly-indent-defun (&optional l r)
  "Indent current defun unobstrusively.
Like `aggressive-indent-indent-defun', but without errors or
messages.  L and R passed to `aggressive-indent-indent-defun'."
  (cl-letf (((symbol-function 'message) #'ignore))
    (ignore-errors (aggressive-indent-indent-defun l r))))

;;; Indenting region
(defun aggressive-indent--indent-current-balanced-line (column)
  "Indent current balanced line, if it starts at COLUMN.
Balanced line means anything contained in a sexp that starts at
the current line, or starts at the same line that one of these
sexps ends.

Return non-nil only if the line's indentation actually changed."
  (when (= (current-column) column)
    (unless (= (point)
               (progn (indent-according-to-mode)
                      (point)))
      (let ((line-end (line-end-position)))
        (forward-sexp 1)
        (comment-forward (point-max))
        ;; We know previous sexp finished on a previous line when
        ;; there's only be whitespace behind point.
        (while (progn
                 (skip-chars-backward "[:blank:]")
                 (not (looking-at "^")))
          (forward-sexp 1)
          (comment-forward (point-max)))
        (when (looking-at "^")
          (funcall aggressive-indent-region-function line-end (1- (point))))
        (skip-chars-forward "[:blank:]")))))

(defun aggressive-indent--extend-end-to-whole-sexps (beg end)
  "Return a point >= END, so that it covers whole sexps from BEG."
  (save-excursion
    (goto-char beg)
    (while (and (< (point) end)
                (not (eobp)))
      (forward-sexp 1))
    (point)))

;;;###autoload
(defun aggressive-indent-indent-region-and-on (l r)
  "Indent region between L and R, and then some.
Call `aggressive-indent-region-function' between L and R, and
then keep indenting until nothing more happens."
  (interactive "r")
  (let ((p (point-marker))
        was-begining-of-line)
    (set-marker-insertion-type p t)
    (unwind-protect
        (progn
          (unless (= l r)
            (when (= (char-before r) ?\n)
              (cl-decf r)))
          ;; If L is at the end of a line, skip that line.
          (unless (= l r)
            (when (= (char-after l) ?\n)
              (cl-incf l)))
          ;; Indent the affected region.
          (goto-char r)
          (unless (= l r) (funcall aggressive-indent-region-function l r))
          ;; And then we indent each following line until nothing happens.
          (forward-line 1)
          (skip-chars-forward "[:blank:]\n\r\xc")
          (let ((base-column (current-column)))
            (while (and (not (eobp))
                        (not (run-hook-with-args-until-success 'aggressive-indent-stop-here-hook))
                        (aggressive-indent--indent-current-balanced-line base-column)))))
      (goto-char p))))

(defun aggressive-indent--softly-indent-region-and-on (l r &rest _)
  "Indent region between L and R, and a bit more.
Like `aggressive-indent-indent-region-and-on', but without errors
or messages."
  (cl-letf (((symbol-function 'message) #'ignore))
    (ignore-errors (aggressive-indent-indent-region-and-on l r))))

;;; Tracking changes
(defvar aggressive-indent--changed-list nil
  "List of (left right) limit of regions changed in the last command loop.")
(make-variable-buffer-local 'aggressive-indent--changed-list)

(defun aggressive-indent--proccess-changed-list-and-indent ()
  "Indent the regions in `aggressive-indent--changed-list'."
  (let ((inhibit-modification-hooks t)
        (inhibit-point-motion-hooks t)
        (indent-function
         (if (cl-member-if #'derived-mode-p aggressive-indent-modes-to-prefer-defun)
             #'aggressive-indent--softly-indent-defun #'aggressive-indent--softly-indent-region-and-on)))
    ;; Take the 10 most recent changes.
    (let ((cell (nthcdr 10 aggressive-indent--changed-list)))
      (when cell (setcdr cell nil)))
    ;; (message "----------")
    (while aggressive-indent--changed-list
      ;; (message "%S" (car aggressive-indent--changed-list))
      (apply indent-function (car aggressive-indent--changed-list))
      (setq aggressive-indent--changed-list
            (cdr aggressive-indent--changed-list)))))

(defcustom aggressive-indent-sit-for-time 0.05
  "Time, in seconds, to wait before indenting.
If you feel aggressive-indent is causing Emacs to hang while
typing, try tweaking this number."
  :type 'float)

(defvar-local aggressive-indent--idle-timer nil
  "Idle timer used for indentation")

(defun aggressive-indent--indent-if-changed ()
  "Indent any region that changed in the last command loop."
  (when (and aggressive-indent-mode aggressive-indent--changed-list)
    (save-excursion
      (save-selected-window
        (unless (or (run-hook-wrapped 'aggressive-indent--internal-dont-indent-if #'eval)
                    (aggressive-indent--run-user-hooks))
          (while-no-input
            (aggressive-indent--proccess-changed-list-and-indent)))))
    (when (timerp aggressive-indent--idle-timer)
      (cancel-timer aggressive-indent--idle-timer))))

(defun aggressive-indent--keep-track-of-changes (l r &rest _)
  "Store the limits (L and R) of each change in the buffer."
  (when aggressive-indent-mode
    (push (list l r) aggressive-indent--changed-list)
    (when (timerp aggressive-indent--idle-timer)
      (cancel-timer aggressive-indent--idle-timer))
    (setq aggressive-indent--idle-timer
          (run-with-idle-timer aggressive-indent-sit-for-time t #'aggressive-indent--indent-if-changed))))

;;; Minor modes
;;;###autoload
(define-minor-mode aggressive-indent-mode
  nil nil " =>"
  `((,(kbd "C-c C-q") . aggressive-indent-indent-defun)
    ([backspace]
     menu-item "maybe-delete-indentation" ignore :filter
     (lambda (&optional _)
       (when (and (looking-back "^[[:blank:]]+")
                  ;; Wherever we don't want to indent, we probably also
                  ;; want the default backspace behavior.
                  (not (run-hook-wrapped 'aggressive-indent--internal-dont-indent-if #'eval))
                  (not (aggressive-indent--run-user-hooks)))
         #'delete-indentation))))
  (if aggressive-indent-mode
      (if (and global-aggressive-indent-mode
               (or (cl-member-if #'derived-mode-p aggressive-indent-excluded-modes)
                   (equal indent-line-function #'indent-relative)
                   (derived-mode-p 'text-mode)
                   (eq major-mode 'fundamental-mode)
                   buffer-read-only))
          (aggressive-indent-mode -1)
        ;; Should electric indent be ON or OFF?
        (if (or (eq aggressive-indent-dont-electric-modes t)
                (cl-member-if #'derived-mode-p aggressive-indent-dont-electric-modes))
            (aggressive-indent--local-electric nil)
          (aggressive-indent--local-electric t))
        (add-hook 'after-change-functions #'aggressive-indent--keep-track-of-changes nil 'local)
        (add-hook 'before-save-hook #'aggressive-indent--proccess-changed-list-and-indent nil 'local))
    ;; Clean the hooks
    (when (timerp aggressive-indent--idle-timer)
      (cancel-timer aggressive-indent--idle-timer))
    (remove-hook 'after-change-functions #'aggressive-indent--keep-track-of-changes 'local)
    (remove-hook 'before-save-hook #'aggressive-indent--proccess-changed-list-and-indent 'local)
    (remove-hook 'post-command-hook #'aggressive-indent--softly-indent-defun 'local)))

(defun aggressive-indent--local-electric (on)
  "Turn variable `electric-indent-mode' on or off locally, as per boolean ON."
  (if (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode (if on 1 -1))
    (set (make-local-variable 'electric-indent-mode) on)))

;;;###autoload
(define-globalized-minor-mode global-aggressive-indent-mode
  aggressive-indent-mode aggressive-indent-mode)

;;;###autoload
(defalias 'aggressive-indent-global-mode
  #'global-aggressive-indent-mode)

(provide 'aggressive-indent)
;;; aggressive-indent.el ends here
