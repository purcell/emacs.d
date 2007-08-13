;;; mode-local.el --- Support for mode local facilities
;;
;; Copyright (C) 2004, 2005 David Ponce
;;
;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 27 Apr 2004
;; Keywords: syntax
;; X-RCS: $Id: mode-local.el,v 1.10 2006/01/30 12:51:20 ponced Exp $
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Each major mode will want to support a specific set of behaviors.
;; Usually generic behaviors that need just a little bit of local
;; specifics.
;;
;; This library permits the setting of override functions for tasks of
;; that nature, and also provides reasonable defaults.
;;
;; There are buffer local variables, and frame local variables.
;; This library give the illusion of mode specific variables.
;;
;; You should use a mode-local variable or override to allow extension
;; only if you expect a mode author to provide that extension.  If a
;; user might wish to customize a give variable or function then
;; the existing customization mechanism should be used.

;; To Do:
;; Allow customization of a variable for a specific mode?

;;; History:
;;

;;; Code:
(eval-when-compile (require 'cl))

;;; Compatibility
;;
(defun mode-local-define-derived-mode-needed-p ()
  "Return non-nil if mode local has to fix `define-derived-mode'.
That is, if `define-derived-mode' does not set `derived-mode-parent'."
  (let ((body (cdr (macroexpand '(define-derived-mode c p ""))))
        (bad t))
    (while (and body bad)
      (if (equal (car body) '(put 'c 'derived-mode-parent 'p))
          (setq bad nil)
        (setq body (cdr body))))
    bad))

(when (mode-local-define-derived-mode-needed-p)
  ;; Workaround a bug in some (XEmacs) versions of
  ;; `define-derived-mode' that don't set the `derived-mode-parent'
  ;; property, and break mode-local.
  (defadvice define-derived-mode
    (after mode-local-define-derived-mode activate)
    "Fix missing `derived-mode-parent' property on child."
    (unless (eq 'fundamental-mode (ad-get-arg 1))
      (let ((form (cdr ad-return-value)))
        (setq ad-return-value nil)
        (while form
          (and (eq 'defun (car-safe (car form)))
               (eq (ad-get-arg 0) (car (cdr-safe (car form))))
               (push `(or (get ',(ad-get-arg 0) 'derived-mode-parent)
                          (put ',(ad-get-arg 0) 'derived-mode-parent
                               ',(ad-get-arg 1)))
                     ad-return-value))
          (push (car form) ad-return-value)
          (setq form (cdr form)))
        (setq ad-return-value `(progn ,@(nreverse ad-return-value)))
        )))
  )

;;; Misc utilities
;;
(defun mode-local-map-file-buffers (function &optional predicate buffers)
  "Run FUNCTION on every file buffer found.
FUNCTION does not have arguments; when it is entered `current-buffer'
is the currently selected file buffer.
If optional argument PREDICATE is non nil, only select file buffers
for which the function PREDICATE return non-nil.
If optional argument BUFFERS is non-nil, it is a list of buffers to
walk through.  It defaults to `buffer-list'."
  (dolist (b (or buffers (buffer-list)))
    (and (buffer-live-p b) (buffer-file-name b)
         (with-current-buffer b
           (when (or (not predicate) (funcall predicate))
             (funcall function))))))

(defun mode-local-map-mode-buffers (function modes)
  "Run FUNCTION on every file buffer with major mode in MODES.
MODES can be a symbol or a list of symbols.
FUNCTION does not have arguments."
  (or (listp modes) (setq modes (list modes)))
  (mode-local-map-file-buffers
   function #'(lambda () (memq major-mode modes))))

;;; Hook machinery
;;
(defvar mode-local-init-hook nil
  "Hook run after a new file buffer is created.
The current buffer is the newly created file buffer.")

(defvar mode-local-changed-mode-buffers nil
  "List of buffers whose `major-mode' has changed recently.")

(defvar mode-local--init-mode nil)

(defsubst mode-local-initialized-p ()
  "Return non-nil if mode local is initialized in current buffer.
That is, if the current `major-mode' is equal to the major mode for
which mode local bindings have been activated."
  (eq mode-local--init-mode major-mode))

(defun mode-local-post-major-mode-change ()
  "`post-command-hook' run when there is a `major-mode' change.
This makes sure mode local init type stuff can occur."
  (remove-hook 'post-command-hook 'mode-local-post-major-mode-change)
  (let ((buffers mode-local-changed-mode-buffers))
    (setq mode-local-changed-mode-buffers nil)
    (mode-local-map-file-buffers
     #'(lambda ()
         ;; Make sure variables are set up for this mode.
         (activate-mode-local-bindings)
         (run-hooks 'mode-local-init-hook))
     #'(lambda ()
         (not (mode-local-initialized-p)))
     buffers)))

(defun mode-local-on-major-mode-change ()
  "Function called in `change-major-mode-hook'."
  (add-to-list 'mode-local-changed-mode-buffers (current-buffer))
  (add-hook 'post-command-hook 'mode-local-post-major-mode-change))

(add-hook 'find-file-hooks 'mode-local-post-major-mode-change)
(add-hook 'change-major-mode-hook 'mode-local-on-major-mode-change)

;;; Core bindings API
;;
(defsubst set-mode-local-parent (mode parent)
  "Set parent of major mode MODE to PARENT mode.
To work properly, this function should be called after PARENT mode
local variables have been defined."
  (put mode 'mode-local-parent parent)
  ;; Refresh mode bindings to get mode local variables inherited from
  ;; PARENT. To work properly, the following should be called after
  ;; PARENT mode local variables have been defined.
  (mode-local-map-mode-buffers mode 'activate-mode-local-bindings))

(defsubst get-mode-local-parent (mode)
  "Return the mode parent of the major mode MODE.
Return nil if MODE has no parent."
  (or (get mode 'mode-local-parent)
      (get mode 'derived-mode-parent)))

(defmacro define-child-mode (mode parent &optional docstring)
  "Make major mode MODE inherits behavior from PARENT mode.
DOCSTRING is optional and not used.
To work properly, this should be put after PARENT mode local variables
definition."
  `(set-mode-local-parent ',mode ',parent))

(defvar mode-local-symbol-table nil
  "Buffer local mode bindings.
These symbols provide a hook for a `major-mode' to specify specific
behaviors.  Use the function `mode-local-bind' to define new bindings.")
(make-variable-buffer-local 'mode-local-symbol-table)

(defvar mode-local-active-mode nil
  "Major mode in which bindings are active.")

(defsubst new-mode-local-bindings ()
  "Return a new empty mode bindings symbol table."
  (make-vector 13 0))

(defun mode-local-bind (bindings &optional plist mode)
  "Define BINDINGS in the specified environment.
BINDINGS is a list of (VARIABLE . VALUE).
Optional argument PLIST is a property list each VARIABLE symbol will
be set to.  The following properties have special meaning:

- `constant-flag' if non-nil, prevent to rebind variables.
- `mode-variable-flag' if non-nil, define mode variables.
- `override-flag' if non-nil, define override functions.

The `override-flag' and `mode-variable-flag' properties are mutually
exclusive.

If optional argument MODE is non-nil, it must be a major mode symbol.
BINDINGS will be defined globally for this major mode.  If MODE is
nil, BINDINGS will be defined locally in the current buffer, in
variable `mode-local-symbol-table'.  The later should be done in MODE
hook."
  ;; Check plist consistency
  (and (plist-get plist 'mode-variable-flag)
       (plist-get plist 'override-flag)
       (error "Bindings can't be both overrides and mode variables"))
  (let (table variable varname value binding)
    (if mode
        (progn
          ;; Install in given MODE symbol table.  Create a new one if
          ;; needed.
          (setq table (or (get mode 'mode-local-symbol-table)
                          (new-mode-local-bindings)))
          (put mode 'mode-local-symbol-table table))
      ;; Fail if trying to bind mode variables in local context!
      (if (plist-get plist 'mode-variable-flag)
          (error "Mode required to bind mode variables"))
      ;; Install in buffer local symbol table.  Create a new one if
      ;; needed.
      (setq table (or mode-local-symbol-table
                      (setq mode-local-symbol-table
                            (new-mode-local-bindings)))))
    (while bindings
      (setq binding  (car bindings)
            bindings (cdr bindings)
            varname  (symbol-name (car binding))
            value    (cdr binding))
      (if (setq variable (intern-soft varname table))
          ;; Binding already exists
          ;; Check rebind consistency
          (cond
           ((equal (symbol-value variable) value)
            ;; Just ignore rebind with the same value.
            )
           ((get variable 'constant-flag)
            (error "Can't change the value of constant `%s'"
                   variable))
           ((and (get variable 'mode-variable-flag)
                 (plist-get plist 'override-flag))
            (error "Can't rebind override `%s' as a mode variable"
                   variable))
           ((and (get variable 'override-flag)
                 (plist-get plist 'mode-variable-flag))
            (error "Can't rebind mode variable `%s' as an override"
                   variable))
           (t
            ;; Merge plist and assign new value
            (setplist variable (append plist (symbol-plist variable)))
            (set variable value)))
        ;; New binding
        (setq variable (intern varname table))
        ;; Set new plist and assign initial value
        (setplist variable plist)
        (set variable value)))
    ;; Return the symbol table used
    table))

(defsubst mode-local-symbol (symbol &optional mode)
  "Return the mode local symbol bound with SYMBOL's name.
Return nil if the  mode local symbol doesn't exist.
If optional argument MODE is nil, lookup first into locally bound
symbols, then in those bound in current `major-mode' and its parents.
If MODE is non-nil, lookup into symbols bound in that major mode and
its parents."
  (let ((name (symbol-name symbol)) bind)
    (or mode
        (setq mode mode-local-active-mode)
        (setq mode major-mode
              bind (and mode-local-symbol-table
                        (intern-soft name mode-local-symbol-table))))
    (while (and mode (not bind))
      (or (and (get mode 'mode-local-symbol-table)
               (setq bind (intern-soft
                           name (get mode 'mode-local-symbol-table))))
          (setq mode (get-mode-local-parent mode))))
    bind))

(defsubst mode-local-symbol-value (symbol &optional mode property)
  "Return the value of the mode local symbol bound with SYMBOL's name.
If optional argument MODE is non-nil, restrict lookup to that mode and
its parents (see the function `mode-local-symbol' for more details).
If optional argument PROPERTY is non-nil the mode local symbol must
have that property set.  Return nil if the symbol doesn't exist, or
doesn't have PROPERTY set."
  (and (setq symbol (mode-local-symbol symbol mode))
       (or (not property) (get symbol property))
       (symbol-value symbol)))

;;; Mode local variables
;;
(defun activate-mode-local-bindings (&optional mode)
  "Activate variables defined locally in MODE and its parents.
That is, copy mode local bindings into corresponding buffer local
variables.
If MODE is not specified it defaults to current `major-mode'.
Return the alist of buffer-local variables that have been changed.
Elements are (SYMBOL . PREVIOUS-VALUE), describing one variable."
  (let (modes table old-locals)
    (unless mode
      (set (make-local-variable 'mode-local--init-mode) major-mode)
      (setq mode major-mode))
    ;; Get MODE's parents & MODE in the right order.
    (while mode
      (setq modes (cons mode modes)
            mode  (get-mode-local-parent mode)))
    ;; Activate mode bindings following parent modes order.
    (dolist (mode modes)
      (when (setq table (get mode 'mode-local-symbol-table))
        (mapatoms
         #'(lambda (var)
             (when (get var 'mode-variable-flag)
               (let ((v (intern (symbol-name var))))
                 ;; Save the current buffer-local value of the
                 ;; mode-local variable.
                 (and (local-variable-p v (current-buffer))
                      (push (cons v (symbol-value v)) old-locals))
                 (set (make-local-variable v) (symbol-value var)))))
         table)))
    old-locals))

(defun deactivate-mode-local-bindings (&optional mode)
  "Deactivate variables defined locally in MODE and its parents.
That is, kill buffer local variables set from the corresponding mode
local bindings.
If MODE is not specified it defaults to current `major-mode'."
  (unless mode
    (kill-local-variable 'mode-local--init-mode)
    (setq mode major-mode))
  (let (table)
    (while mode
      (when (setq table (get mode 'mode-local-symbol-table))
        (mapatoms
         #'(lambda (var)
             (when (get var 'mode-variable-flag)
               (kill-local-variable (intern (symbol-name var)))))
         table))
      (setq mode (get-mode-local-parent mode)))))

(defmacro with-mode-local (mode &rest body)
   "With the local bindings of MODE, evaluate BODY.
The current mode bindings are saved, BODY is evaluated, and the saved
bindings are restored, even in case of an abnormal exit.
Value is what BODY returns."
   (let ((old-mode  (make-symbol "mode"))
         (old-locals (make-symbol "old-locals"))
         (local (make-symbol "local")))
     `(let ((,old-mode mode-local-active-mode)
            (,old-locals nil))
        (unwind-protect
            (progn
              (deactivate-mode-local-bindings ,old-mode)
              (setq mode-local-active-mode ',mode)
              ;; Save the previous value of buffer-local variables
              ;; changed by `activate-mode-local-bindings'.
              (setq ,old-locals (activate-mode-local-bindings ',mode))
              ,@body)
          (deactivate-mode-local-bindings ',mode)
          ;; Restore the previous value of buffer-local variables.
          (dolist (,local ,old-locals)
            (set (car ,local) (cdr ,local)))
          ;; Restore the mode local variables.
          (setq mode-local-active-mode ,old-mode)
          (activate-mode-local-bindings ,old-mode)))))
(put 'with-mode-local 'lisp-indent-function 1)

(defsubst mode-local-value (mode sym)
  "Return the value of the MODE local variable SYM."
  (or mode (error "Missing major mode symbol"))
  (mode-local-symbol-value sym mode 'mode-variable-flag))

(defmacro setq-mode-local (mode &rest args)
  "Assign new values to variables local in MODE.
MODE must be a major mode symbol.
ARGS is a list (SYM VAL SYM VAL ...).
The symbols SYM are variables; they are literal (not evaluated).
The values VAL are expressions; they are evaluated.
Set each SYM to the value of its VAL, locally in buffers already in
MODE, or in buffers switched to that mode.
Return the value of the last VAL."
  (when args
    (let (i ll bl sl tmp sym val)
      (setq i 0)
      (while args
        (setq tmp  (make-symbol (format "tmp%d" i))
              i    (1+ i)
              sym  (car args)
              val  (cadr args)
              ll   (cons (list tmp val) ll)
              bl   (cons `(cons ',sym ,tmp) bl)
              sl   (cons `(set (make-local-variable ',sym) ,tmp) sl)
              args (cddr args)))
      `(let* ,(nreverse ll)
         ;; Save mode bindings
         (mode-local-bind (list ,@bl) '(mode-variable-flag t) ',mode)
         ;; Assign to local variables in all existing buffers in MODE
         (mode-local-map-mode-buffers #'(lambda () ,@sl) ',mode)
         ;; Return the last value
         ,tmp)
      )))

(defmacro defvar-mode-local (mode sym val &optional docstring)
  "Define MODE local variable SYM with value VAL.
DOCSTRING is optional."
  `(progn
     (setq-mode-local ,mode ,sym ,val)
     (put (mode-local-symbol ',sym ',mode)
          'variable-documentation ,docstring)
     ',sym))
(put 'defvar-mode-local 'lisp-indent-function 'defun)

(defmacro defconst-mode-local (mode sym val &optional docstring)
  "Define MODE local constant SYM with value VAL.
DOCSTRING is optional."
  (let ((tmp (make-symbol "tmp")))
    `(let (,tmp)
       (setq-mode-local ,mode ,sym ,val)
       (setq ,tmp (mode-local-symbol ',sym ',mode))
       (put ,tmp 'constant-flag t)
       (put ,tmp 'variable-documentation ,docstring)
       ',sym)))
(put 'defconst-mode-local 'lisp-indent-function 'defun)

;;; Function overloading
;;
(defun make-obsolete-overload (old new)
  "Mark OLD overload as obsoleted by NEW overload."
  (put old 'overload-obsoleted-by new)
  (put old 'mode-local-overload t)
  (put new 'overload-obsolete old))

(defsubst overload-obsoleted-by (overload)
  "Get the overload symbol obsoleted by OVERLOAD.
Return the obsolete symbol or nil if not found."
  (get overload 'overload-obsolete))

(defsubst overload-that-obsolete (overload)
  "Return the overload symbol that obsoletes OVERLOAD.
Return the symbol found or nil if OVERLOAD is not obsolete."
  (get overload 'overload-obsoleted-by))

(defsubst fetch-overload (overload)
  "Return the current OVERLOAD function, or nil if not found.
First, lookup for OVERLOAD into locally bound mode local symbols, then
in those bound in current `major-mode' and its parents."
  (or (mode-local-symbol-value overload nil 'override-flag)
      ;; If an obsolete overload symbol exists, try it.
      (and (overload-obsoleted-by overload)
           (mode-local-symbol-value
            (overload-obsoleted-by overload) nil 'override-flag))))

(defun mode-local--override (name args body)
  "Return the form that handles overloading of function NAME.
ARGS are the arguments to the function.
BODY is code that would be run when there is no override defined.  The
default is to call the function `NAME-default' with the appropriate
arguments.
See also the function `define-overload'."
  (let* ((default (intern (format "%s-default" name)))
         (overargs (delq '&rest (delq '&optional (copy-sequence args))))
         (override (make-symbol "override")))
    `(let ((,override (fetch-overload ',name)))
       (if ,override
           (funcall ,override ,@overargs)
         ,@(or body `((,default ,@overargs)))))
    ))

(defun mode-local--expand-overrides (name args body)
  "Expand override forms that overload function NAME.
ARGS are the arguments to the function NAME.
BODY is code where override forms are searched for expansion.
Return result of expansion, or BODY if no expansion occurred.
See also the function `define-overload'."
  (let ((forms body)
        (ditto t)
        form xbody)
    (while forms
      (setq form (car forms))
      (cond
       ((atom form))
       ((eq (car form) :override)
        (setq form (mode-local--override name args (cdr form))))
       ((eq (car form) :override-with-args)
        (setq form (mode-local--override name (cadr form) (cddr form))))
       ((setq form (mode-local--expand-overrides name args form))))
      (setq ditto (and ditto (eq (car forms) form))
            xbody (cons form xbody)
            forms (cdr forms)))
    (if ditto body (nreverse xbody))))

(defun mode-local--overload-body (name args body)
  "Return the code that implements overloading of function NAME.
ARGS are the arguments to the function NAME.
BODY specifies the overload code.
See also the function `define-overload'."
  (let ((result (mode-local--expand-overrides name args body)))
    (if (eq body result)
        (list (mode-local--override name args body))
      result)))

(defmacro define-overload (name args docstring &rest body)
  "Define a new function, as with `defun' which can be overloaded.
NAME is the name of the function to create.
ARGS are the arguments to the function.
DOCSTRING is a documentation string to describe the function.  The
docstring will automatically had details about its overload symbol
appended to the end.
BODY is code that would be run when there is no override defined.  The
default is to call the function `NAME-default' with the appropriate
arguments.

BODY can also include an override form that specifies which part of
BODY is specifically overridden.  This permits to specify common code
run for both default and overridden implementations.
An override form is one of:

  1. (:override [OVERBODY])
  2. (:override-with-args OVERARGS [OVERBODY])

OVERBODY is the code that would be run when there is no override
defined.  The default is to call the function `NAME-default' with the
appropriate arguments deduced from ARGS.
OVERARGS is a list of arguments passed to the override and
`NAME-default' function, in place of those deduced from ARGS."
  `(eval-and-compile
     (defun ,name ,args
       ,docstring
       ,@(mode-local--overload-body name args body))
     (put ',name 'mode-local-overload t)))
(put :override-with-args 'lisp-indent-function 1)

(defsubst function-overload-p (symbol)
  "Return non-nil if SYMBOL is a function which can be overloaded."
  (and symbol (symbolp symbol) (get symbol 'mode-local-overload)))

(defmacro define-mode-local-override
  (name mode args docstring &rest body)
  "Define a mode specific override of the function overload NAME.
Has meaning only if NAME has been created with `define-overload'.
MODE is the major mode this override is being defined for.
ARGS are the function arguments, which should match those of the same
named function created with `define-overload'.
DOCSTRING is the documentation string.
BODY is the implementation of this function."
  (let ((newname (intern (format "%s-%s" name mode))))
    `(progn
       (eval-and-compile
	 (defun ,newname ,args
	   ,(format "%s\n\nOverride %s in `%s' buffers."
		    docstring name mode)
	   ;; The body for this implementation
	   ,@body)
         ;; For find-func to locate the definition of NEWNAME.
         (put ',newname 'definition-name ',name))
       (mode-local-bind '((,name . ,newname))
                        '(override-flag t)
                        ',mode))
    ))

;;; Help support
;;
(defun overload-docstring-extension (overload)
  "Return the doc string that augments the description of OVERLOAD."
  (let ((doc "\n\This function can be overloaded\
 (see `define-mode-local-override' for details).")
        (sym (overload-obsoleted-by overload)))
    (when sym
      (setq doc (format "%s\nIt makes the overload `%s' obsolete."
                        doc sym)))
    (setq sym (overload-that-obsolete overload))
    (when sym
      (setq doc (format "%s\nThis overload is obsoletes;\nUse `%s' instead."
                        doc sym)))
    doc))

(defun mode-local-augment-function-help (symbol)
  "Augment the *Help* buffer for SYMBOL.
SYMBOL is a function that can be overridden."
  (with-current-buffer "*Help*"
    (pop-to-buffer (current-buffer))
    (unwind-protect
	(progn
	  (toggle-read-only -1)
          (goto-char (point-min))
          (unless (re-search-forward "^$" nil t)
            (goto-char (point-max))
            (beginning-of-line)
            (forward-line -1))
          (insert (overload-docstring-extension symbol) "\n")
	  ;; NOTE TO SELF:
	  ;; LIST ALL LOADED OVERRIDES FOR SYMBOL HERE
	  )
      (toggle-read-only 1))))

;; Help for Overload functions.  Need to advise help.
(defadvice describe-function (around mode-local-help activate)
  "Display the full documentation of FUNCTION (a symbol).
Returns the documentation as a string, also."
  (prog1
      ad-do-it
    (if (function-overload-p (ad-get-arg 0))
	(mode-local-augment-function-help (ad-get-arg 0)))))

;; Help for mode-local bindings.
(defun mode-local-print-binding (symbol)
  "Print the SYMBOL binding."
  (let ((value (symbol-value symbol)))
    (princ (format "\n     `%s' value is\n       " symbol))
    (if (and value (symbolp value))
        (princ (format "`%s'" value))
      (let ((pt (point)))
        (pp value)
        (save-excursion
          (goto-char pt)
          (indent-sexp))))
    (or (bolp) (princ "\n"))))

(defun mode-local-print-bindings (table)
  "Print bindings in TABLE."
  (let (us ;; List of unpecified symbols
        mc ;; List of mode local constants
        mv ;; List of mode local variables
        ov ;; List of overloaded functions
        fo ;; List of final overloaded functions
        )
    ;; Order symbols by type
    (mapatoms
     #'(lambda (s)
         (add-to-list (cond
                       ((get s 'mode-variable-flag)
                        (if (get s 'constant-flag) 'mc 'mv))
                       ((get s 'override-flag)
                        (if (get s 'constant-flag) 'fo 'ov))
                       ('us))
                      s))
     table)
    ;; Print symbols by type
    (when us
      (princ "\n  !! Unpecified symbols\n")
      (mapc 'mode-local-print-binding us))
    (when mc
      (princ "\n  ** Mode local constants\n")
      (mapc 'mode-local-print-binding mc))
    (when mv
      (princ "\n  ** Mode local variables\n")
      (mapc 'mode-local-print-binding mv))
    (when fo
      (princ "\n  ** Final overloaded functions\n")
      (mapc 'mode-local-print-binding fo))
    (when ov
      (princ "\n  ** Overloaded functions\n")
      (mapc 'mode-local-print-binding ov))
    ))

(defun mode-local-describe-bindings-2 (buffer-or-mode)
  "Display mode local bindings active in BUFFER-OR-MODE."
  (let (table mode)
    (princ "Mode local bindings active in ")
    (cond
     ((bufferp buffer-or-mode)
      (with-current-buffer buffer-or-mode
        (setq table mode-local-symbol-table
              mode major-mode))
      (princ (format "%S\n" buffer-or-mode))
      )
     ((symbolp buffer-or-mode)
      (setq mode buffer-or-mode)
      (princ (format "`%s'\n" buffer-or-mode))
      )
     ((signal 'wrong-type-argument
              (list 'buffer-or-mode buffer-or-mode))))
    (when table
      (princ "\n- Buffer local\n")
      (mode-local-print-bindings table))
    (while mode
      (setq table (get mode 'mode-local-symbol-table))
      (when table
        (princ (format "\n- From `%s'\n" mode))
        (mode-local-print-bindings table))
      (setq mode (get-mode-local-parent mode)))))

(defun mode-local-describe-bindings-1 (buffer-or-mode &optional interactive-p)
  "Display mode local bindings active in BUFFER-OR-MODE.
Optional argument INTERACTIVE-P is non-nil if the calling command was
invoked interactively."
  (if (fboundp 'with-displaying-help-buffer)
      ;; XEmacs
      (with-displaying-help-buffer
       #'(lambda ()
           (with-current-buffer standard-output
             (mode-local-describe-bindings-2 buffer-or-mode)
             (when (fboundp 'frob-help-extents)
               (goto-char (point-min))
               (frob-help-extents standard-output)))))
    ;; GNU Emacs
    (when (fboundp 'help-setup-xref)
      (help-setup-xref
       (list 'mode-local-describe-bindings-1 buffer-or-mode)
       interactive-p))
    (with-output-to-temp-buffer "*Help*"
      (with-current-buffer standard-output
        (mode-local-describe-bindings-2 buffer-or-mode)))))

(defun describe-mode-local-bindings (buffer)
  "Display mode local bindings active in BUFFER."
  (interactive "b")
  (when (setq buffer (get-buffer buffer))
    (mode-local-describe-bindings-1 buffer (interactive-p))))

(defun describe-mode-local-bindings-in-mode (mode)
  "Display mode local bindings active in MODE hierarchy."
  (interactive
   (list (completing-read
          "Mode: " obarray
          #'(lambda (s) (get s 'mode-local-symbol-table))
          t (symbol-name major-mode))))
  (when (setq mode (intern-soft mode))
    (mode-local-describe-bindings-1 mode (interactive-p))))

;;; Font-lock support
;;
(defconst mode-local-font-lock-keywords
  (eval-when-compile
    (let* (
           ;; Variable declarations
           (kv (regexp-opt
                '(
                  "defconst-mode-local"
                  "defvar-mode-local"
                  ) t))
           ;; Function declarations
           (kf (regexp-opt
                '(
                  "define-mode-local-override"
                  "define-child-mode"
                  "define-overload"
                  ;;"make-obsolete-overload"
                  "with-mode-local"
                  ) t))
           ;; Regexp depths
           (kv-depth (regexp-opt-depth kv))
           (kf-depth (regexp-opt-depth kf))
           )
      `((,(concat
           ;; Declarative things
           "(\\(" kv "\\|" kf "\\)"
           ;; Whitespaces & names
           "\\>[ \t]*\\(\\sw+\\)?[ \t]*\\(\\sw+\\)?"
           )
         (1 font-lock-keyword-face)
         (,(+ 1 kv-depth kf-depth 1)
          (cond ((match-beginning 2)
                 font-lock-type-face)
                ((match-beginning ,(+ 1 kv-depth 1))
                 font-lock-function-name-face)
                )
          nil t)
         (,(+ 1 kv-depth kf-depth 1 1)
          (cond ((match-beginning 2)
                 font-lock-variable-name-face)
                )
          nil t)))
      ))
  "Highlighted keywords.")


;;; find-func support (Emacs 21.4, or perhaps 22.1)
;;
(condition-case nil
    ;; Try to get find-func so we can modify it.
    (require 'find-func)
  (error nil))

(when (boundp 'find-function-regexp)
  (unless (string-match "ine-overload" find-function-regexp)
    (if (string-match "(def\\\\(" find-function-regexp)
	(let ((end (match-end 0))
	      )
	  (setq find-function-regexp
		(concat (substring find-function-regexp 0 end)
			"ine-overload\\|ine-mode-local-override\\|"
			"ine-child-mode\\|"
			(substring find-function-regexp end)))))
    )
  ;; The regexp for variables is a little more kind.
  )

;; TODO: Add XEmacs support
(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords 'emacs-lisp-mode
                          mode-local-font-lock-keywords))

;;; edebug support
;;
(defun mode-local-setup-edebug-specs ()
  "Define edebug specification for mode local macros."
  (def-edebug-spec setq-mode-local
    (symbolp (&rest symbolp form))
    )
  (def-edebug-spec defvar-mode-local
    (&define symbolp name def-form [ &optional stringp ] )
    )
  (def-edebug-spec defconst-mode-local
    defvar-mode-local
    )
  (def-edebug-spec define-overload
    (&define name lambda-list stringp def-body)
    )
  (def-edebug-spec define-mode-local-override
    (&define name symbolp lambda-list stringp def-body)
    )
  )

(add-hook 'edebug-setup-hook 'mode-local-setup-edebug-specs)

(provide 'mode-local)

;;; mode-local.el ends here
