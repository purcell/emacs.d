;;; ido-ubiquitous.el --- Use ido (nearly) everywhere.

;; Author: Ryan C. Thompson
;; URL: https://github.com/DarwinAwardWinner/ido-ubiquitous
;; Version: 1.5
;; Created: 2011-09-01
;; Keywords: convenience
;; EmacsWiki: InteractivelyDoThings

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; You may have seen the `ido-everywhere' variable in ido.el and got
;; excited that you could use ido completion for everything. Then you
;; were probably disappointed when you realized that it only applied
;; to *file names* and nothing else. Well, ido-ubiquitous is here to
;; fulfill the original promise and let you use ido completion for
;; (almost) any command that uses `completing-read' to offer you a
;; choice of several alternatives.

;; This even works in M-x, but for that, you might prefer the "smex"
;; package instead.

;; As of version 0.7, this package also makes a small modification to
;; ido's behavior so as to support a strange corner case of
;; `completing-read' that some functions rely on. Since the goal of
;; this package is to replace `completing-read' everywhere instead of
;; just selectively (as ido itself does), compatibility with all the
;; quriks of `completing-read' is important here.

;; If you find a case where enabling ido-ubiquitous causes a command
;; not to work correctly, please report it by creating an issue on
;; GitHub: https://github.com/DarwinAwardWinner/ido-ubiquitous/issues

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'ido)
(require 'advice)


(defvar ido-ubiquitous-orig-completing-read-function
  (bound-and-true-p completing-read-function)
  "The value of `completing-read-function' before ido-ubiquitous-mode was enabled.

This value will be restored when `ido-ubiquitous-mode' is
deactivated. It will also be used as a fallback if ido-ubiquitous
detects something that ido cannot handle.")

;;;###autoload
(defgroup ido-ubiquitous nil
  "Use ido for (almost) all completion."
  :group 'ido)

;;;###autoload
(define-minor-mode ido-ubiquitous-mode
  "Use `ido-completing-read' instead of `completing-read' almost everywhere.

  This mode has no effect unles `ido-mode' is also enabled.

  If this mode causes problems for a function, you can force the
  function to use the original completing read by using the macro
  `ido-ubiquitous-disable-in'. For example, if a
  function `foo' cannot work with ido-style completion, evaluate
  the following (for example by putting it in your .emacs file):

    (ido-ubiquitous-disable-in foo)"

  nil
  :global t
  :group 'ido-ubiquitous
  (when ido-ubiquitous-mode
    (unless (bound-and-true-p ido-mode)
      (warn "Ido-ubiquitous-mode enabled without ido mode.")))
  (if (and (boundp 'completing-read-function)
           ido-ubiquitous-orig-completing-read-function)
      ;; Emacs 24 and later
      (progn
        ;; Ensure emacs 23 code disabled
        (ad-disable-advice 'completing-read 'around 'ido-ubiquitous-legacy)
        (ad-activate 'completing-read)
        (setq completing-read-function
              (if ido-ubiquitous-mode
                  'completing-read-ido
                ido-ubiquitous-orig-completing-read-function)))
    ;; Emacs 23 and earlier
    (funcall (if ido-ubiquitous-mode 'ad-enable-advice 'ad-disable-advice)
             'completing-read 'around 'ido-ubiquitous-legacy)
    (ad-activate 'completing-read)))

;;;###autoload
(define-obsolete-variable-alias 'ido-ubiquitous
  'ido-ubiquitous-mode "0.8")
;;;###autoload
(define-obsolete-function-alias 'ido-ubiquitous
  'ido-ubiquitous-mode "0.8")

;;;###autoload
(defcustom ido-ubiquitous-command-exceptions '()
  "List of commands that should not be affected by `ido-ubiquitous'.

Even when `ido-ubiquitous' mode is enabled, these commands will
continue to use `completing-read' instead of
`ido-completing-read'.

Only *interactive* commands should go here. To disable
ido-ubiquitous in non-interactive functions, customize
`ido-ubiquitous-function-exceptions'.

Note: this feature depends on the variable `this-command' being
properly set to the name of the currently executing command.
Depending on how the command is onvoked, this may or may not
happen, so this feature may simply not work in some cases."
  :type '(repeat (symbol :tag "Command"))
  :group 'ido-ubiquitous)

;;;###autoload
(define-obsolete-variable-alias 'ido-ubiquitous-exceptions
  'ido-ubiquitous-command-exceptions "0.4")

(defvar ido-next-call-replaces-completing-read nil)
(defvar ido-this-call-replaces-completing-read nil)

;; Emacs 23-
(defadvice completing-read (around ido-ubiquitous-legacy activate)
  "Ido-based method for reading from the minibuffer with completion.
   See `completing-read' for the meaning of the arguments."
  (if (or inherit-input-method          ; Can't handle this arg
          (not ido-mode)
          (not ido-ubiquitous-mode)
          ;; Avoid infinite recursion from ido calling completing-read
          (boundp 'ido-cur-item)
          (memq this-command ido-ubiquitous-command-exceptions))
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      ;; Only use ido completion if there are actually any completions
      ;; to offer.
      (if allcomp
          (let ((ido-next-call-replaces-completing-read t))
            (setq ad-return-value
                  (ido-completing-read prompt allcomp
                                       nil require-match initial-input hist def)))
        ad-do-it))))

(ad-disable-advice 'completing-read 'around 'ido-ubiquitous-legacy)
(ad-activate 'completing-read)

;; Emacs 24+
(defun completing-read-ido (prompt collection &optional predicate
                                   require-match initial-input
                                   hist def inherit-input-method)
  "Ido-based method for reading from the minibuffer with completion.
See `completing-read' for the meaning of the arguments.

This function is a wrapper for `ido-completing-read' designed to
be used as the value of `completing-read-function'."
  (if (or inherit-input-method          ; Can't handle this arg
          (not ido-mode)
          (not ido-ubiquitous-mode)
          (memq this-command ido-ubiquitous-command-exceptions))
      (funcall ido-ubiquitous-orig-completing-read-function
               prompt collection predicate
               require-match initial-input
               hist def inherit-input-method)
    (let ((allcomp (all-completions "" collection predicate)))
      ;; Only use ido completion if there are actually any completions
      ;; to offer.
      (if allcomp
          (let ((ido-next-call-replaces-completing-read t))
            (ido-completing-read prompt allcomp
                                 nil require-match initial-input hist def))
        (funcall ido-ubiquitous-orig-completing-read-function
                 prompt collection predicate
                 require-match initial-input
                 hist def inherit-input-method)))))

(defadvice ido-completing-read (around detect-replacing-cr activate)
  ;; Determine whether this call to `ido-completing-read' was done
  ;; through the ido-ubiquitous wrapper `completing-read-ido'.
  (let* ((ido-this-call-replaces-completing-read ido-next-call-replaces-completing-read)
         (ido-next-call-replaces-completing-read nil))
    ;; Work around a bug in ido when both INITIAL-INPUT and DEF are provided
    ;; More info: https://github.com/technomancy/ido-ubiquitous/issues/18
    (when (and ido-this-call-replaces-completing-read
               def initial-input
               (not (string= initial-input "")))
      ;; Both default and initial input were provided. So keep the
      ;; initial input and preprocess the choices list to put the
      ;; default at the head, then proceed with default = nil.
      (setq choices (cons def (remove def choices))
            def nil))
    ad-do-it))

(defmacro ido-ubiquitous-disable-in (func)
  "Disable ido-ubiquitous in FUNC."
  (let ((docstring
         (format "Disable ido-ubiquitous in %s" func)))
    `(defadvice ,func (around disable-ido-ubiquitous activate)
       ,docstring
       (let (ido-ubiquitous-mode) ad-do-it))))

(define-obsolete-function-alias
  'disable-ido-ubiquitous-in
  'ido-ubiquitous-disable-in
  "0.4")

(defmacro ido-ubiquitous-enable-in (func)
  "Re-enable ido-ubiquitous in FUNC.

  This reverses the effect of a previous call to
  `ido-ubiquitous-disable-in'."
  `(when (ad-find-advice ',func 'around 'disable-ido-ubiquitous)
     (ad-disable-advice ',func 'around 'disable-ido-ubiquitous)
     (ad-activate ',func)))

(define-obsolete-function-alias
  'enable-ido-ubiquitous-in
  'ido-ubiquitous-enable-in
  "0.4")

;; Always disable ido-ubiquitous in `find-file' and similar functions,
;; because they are not supposed to use ido.
(defvar ido-ubiquitous-permanent-function-exceptions
  '(read-file-name
    read-file-name-internal
    read-buffer
    gnus-emacs-completing-read
    gnus-iswitchb-completing-read
    man)
  "Functions in which ido-ubiquitous should always be disabled.

If you want to disable ido in a specific function or command, do
not modify this variable. Instead, try `M-x customize-group
ido-ubiquitous.")

(dolist (func ido-ubiquitous-permanent-function-exceptions)
  (eval `(ido-ubiquitous-disable-in ,func)))

(defun ido-ubiquitous--set-difference (list1 list2)
  "Replacement for `set-difference' from `cl'."
  (apply #'nconc
         (mapcar (lambda (elt) (unless (memq elt list2) (list elt)))
                 list1)))

(defun ido-ubiquitous-set-function-exceptions (sym newval)
  (let* ((oldval (when (boundp sym) (eval sym))))
    ;; Filter out the permanent exceptions so we never act on them.
    (setq oldval (ido-ubiquitous--set-difference oldval ido-ubiquitous-permanent-function-exceptions))
    (setq newval (ido-ubiquitous--set-difference newval ido-ubiquitous-permanent-function-exceptions))
    ;; Re-enable ido-ubiquitous on all old functions, in case they
    ;; were removed from the list.
    (dolist (oldfun oldval)
      (eval `(ido-ubiquitous-enable-in ,oldfun)))
    ;; Set the new value
    (set-default sym newval)
    ;; Disable ido-ubiquitous on all new functions
    (dolist (newfun newval)
      (eval `(ido-ubiquitous-disable-in ,newfun)))))

;;;###autoload
(defcustom ido-ubiquitous-function-exceptions
  '(grep-read-files)
  "List of functions in which to disable ido-ubiquitous.

If you need to add a function to this list, please also file a
bug report at
https://github.com/DarwinAwardWinner/ido-ubiquitous/issues

Note that certain functions, such as `read-file-name', must
always have ido-ubiquitous disabled, and cannot be added
here. (They are effectively a permanent part of this list
already.)"
  :group 'ido-ubiquitous
  :type '(repeat :tag "Functions"
                 (symbol :tag "Function"))
  :set 'ido-ubiquitous-set-function-exceptions)

(defcustom ido-ubiquitous-enable-compatibility t
  "Allow ido to emulate a quirk of `completing-read'.

From the `completing-read' docstring:

> If the input is null, `completing-read' returns DEF, or the
> first element of the list of default values, or an empty string
> if DEF is nil, regardless of the value of REQUIRE-MATCH.

If this variable is non-nil, then ido-ubiquitous will attempt to
emulate this behavior. Specifically, if RET is pressed
immediately upon entering completion, an empty string will be
returned instead of the first element in the list. This behavior
is only enabled when ido is being used as a substitute for
`completing-read', and not when it is used directly.

This odd behavior is required for compatibility with an old-style
usage pattern whereby the default was requested by returning an
empty string. In this mode, the caller receives the empty string
and handles the default case manually, while `completing-read'
never has any knowledge of the default. This is a problem for
ido, which always returns the first element in the list when the
input is empty. Without knowledge of the default, it cannot
ensure that the default is first on the list, so returning the
first item is not the correct behavior. Instead, it must return
an empty string like `completing-read'.

When this mode is enabled, you can still select the first item on
the list by prefixing \"RET\" with \"C-u\"."
  :type 'boolean
  :group 'ido-ubiquitous)

;;;###autoload
(defcustom ido-ubiquitous-command-compatibility-exceptions '()
  "List of commands in which to disable compatibility.

See `ido-ubiquitous-enable-compatibility' for a description of
the compatibility behavior. If this behavior causes a command to
break, add that command to this list to disable compatibility
mode for just that command.

Only *interactive* commands should go here. To disable
compatibility mode in non-interactive functions, customize
`ido-ubiquitous-function-compatibility-exceptions'."
  :type '(repeat (symbol :tag "Command"))
  :group 'ido-ubiquitous)

(defvar ido-ubiquitous-initial-item nil
  "The first item selected when ido starts.")

(defadvice ido-read-internal (before clear-initial-item activate)
  (setq ido-ubiquitous-initial-item nil))

(defadvice ido-make-choice-list (after set-initial-item activate)
  (when (and ad-return-value (listp ad-return-value))
    (setq ido-ubiquitous-initial-item (car ad-return-value))))

(defadvice ido-next-match (after clear-initial-item activate)
  (setq ido-ubiquitous-initial-item nil))

(defadvice ido-prev-match (after clear-initial-item activate)
  (setq ido-ubiquitous-initial-item nil))

(defadvice ido-exit-minibuffer (around compatibility activate)
  "Emulate a quirk of `completing-read'.

> If the input is null, `completing-read' returns DEF, or the
> first element of the list of default values, or an empty string
> if DEF is nil, regardless of the value of REQUIRE-MATCH.

See `ido-ubiquitous-enable-compatibility', which controls whether
this advice has any effect."
  (if (and (eq ido-cur-item 'list)
           ido-ubiquitous-enable-compatibility
           ;; Only enable if we are replacing `completing-read'
           ido-this-call-replaces-completing-read
           ;; Disable in command exceptions
           (not (memq this-command ido-ubiquitous-command-compatibility-exceptions))
           ;; Input is empty
           (string= ido-text "")
           ;; Default is nil
           (null ido-default-item)
           ;; Prefix disables compatibility
           (not current-prefix-arg)
           (string= (car ido-cur-list)
                    ido-ubiquitous-initial-item))
      (ido-select-text)
    ad-do-it)
  (setq ido-ubiquitous-initial-item nil))

(defmacro ido-ubiquitous-disable-compatibility-in (func)
  "Disable ido-ubiquitous compatibility mode in FUNC."
  (let ((docstring
         (format "Disable ido-ubiquitous in %s" func)))
    `(defadvice ,func (around disable-ido-ubiquitous-compatibility activate)
       ,docstring
       (let (ido-ubiquitous-enable-compatibility) ad-do-it))))

(defmacro ido-ubiquitous-enable-compatibility-in (func)
  "Re-enable ido-ubiquitous comaptibility mode in FUNC.

  This reverses the effect of a previous call to
  `ido-ubiquitous-disable-compatibility-in'."
  `(when (ad-find-advice ',func 'around 'disable-ido-ubiquitous-compatibility)
     (ad-disable-advice ',func 'around 'disable-ido-ubiquitous-compatibility)
     (ad-activate ',func)))

(defun ido-ubiquitous-set-function-compatibility-exceptions (sym newval)
  (let* ((oldval (when (boundp sym) (eval sym))))
    ;; Re-enable compatibility on all old functions, in case they
    ;; were removed from the list.
    (dolist (oldfun oldval)
      (eval `(ido-ubiquitous-enable-compatibility-in ,oldfun)))
    ;; Set the new value
    (set-default sym newval)
    ;; Disable compatibility on all new functions
    (dolist (newfun newval)
      (eval `(ido-ubiquitous-disable-compatibility-in ,newfun)))))

;;;###autoload
(defcustom ido-ubiquitous-function-compatibility-exceptions
  '()
  "List of functions in which to disable ido-ubiquitous compatibility mode.

See `ido-ubiquitous-enable-compatibility' for a description of
the compatibility behavior. If this behavior causes a function to
break, add that function to this list to disable compatibility
mode for just that command.

If you need to add a function to this list, please also file a
bug report at
https://github.com/DarwinAwardWinner/ido-ubiquitous/issues"
  :group 'ido-ubiquitous
  :type '(repeat :tag "Functions"
                 (symbol :tag "Function"))
  :set 'ido-ubiquitous-set-function-exceptions)

(defun ido-ubiquitous-initialize ()
  "Do initial setup for ido-ubiquitous.

This only needs to be called once when the file is first loaded."
  ;; Clean up old versions of ido-ubiquitous (1.3 and earlier) that
  ;; defined advice on `completing-read' instead of modifying
  ;; `completing-read-function'.
  (when (ad-find-advice 'completing-read 'around 'ido-ubiquitous)
    (ad-remove-advice 'completing-read 'around 'ido-ubiquitous)
    (ad-activate 'completing-read))
  ;; Make sure all exceptions are activated
  (ido-ubiquitous-set-function-exceptions
   'ido-ubiquitous-function-exceptions
   ido-ubiquitous-function-exceptions)
  (ido-ubiquitous-set-function-compatibility-exceptions
   'ido-ubiquitous-function-compatibility-exceptions
   ido-ubiquitous-function-compatibility-exceptions)
  ;; Make sure the mode is turned on/off as specified by the value of
  ;; the mode variable
  (ido-ubiquitous-mode (if ido-ubiquitous-mode 1 0)))
(ido-ubiquitous-initialize)

(provide 'ido-ubiquitous) ;;; ido-ubiquitous.el ends here
