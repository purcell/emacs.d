 ;;; cl-indent.el --- enhanced lisp-indent mode

;; Copyright (C) 1987, 2000-2011 Free Software Foundation, Inc.

;; Author: Richard Mlynarik <mly@eddie.mit.edu>
;; Created: July 1987
;; Maintainer: FSF
;; Keywords: lisp, tools
;; Package: emacs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package supplies a single entry point, common-lisp-indent-function,
;; which performs indentation in the preferred style for Common Lisp code.
;; To enable it:
;;
;; (setq lisp-indent-function 'common-lisp-indent-function)

;;; Code:

(defgroup lisp-indent nil
  "Indentation in Lisp."
  :group 'lisp)

(defcustom lisp-indent-maximum-backtracking 6
  "Maximum depth to backtrack out from a sublist for structured indentation.
If this variable is 0, no backtracking will occur and forms such as `flet'
may not be correctly indented if this value is less than 4."
  :type 'integer
  :group 'lisp-indent)

(defcustom lisp-tag-indentation 1
  "Indentation of tags relative to containing list.
This variable is used by the function `lisp-indent-tagbody'."
  :type 'integer
  :group 'lisp-indent)

(defcustom lisp-tag-body-indentation 3
  "Indentation of non-tagged lines relative to containing list.
This variable is used by the function `lisp-indent-tagbody' to indent normal
lines (lines without tags).
The indentation is relative to the indentation of the parenthesis enclosing
the special form.  If the value is t, the body of tags will be indented
as a block at the same indentation as the first s-expression following
the tag.  In this case, any forms before the first tag are indented
by `lisp-body-indent'."
  :type 'integer
  :group 'lisp-indent)

(defcustom lisp-backquote-indentation t
  "Whether or not to indent backquoted lists as code.
If nil, indent backquoted lists as data, i.e., like quoted lists."
  :type 'boolean
  :group 'lisp-indent)

(defcustom lisp-loop-indent-subclauses t
  "Whether or not to indent loop subclauses."
  :type 'boolean
  :group 'lisp-indent)

(defcustom lisp-simple-loop-indentation 2
  "Indentation of forms in simple loop forms."
  :type 'integer
  :group 'lisp-indent)

(defcustom lisp-loop-indent-forms-like-keywords nil
  "Whether or not to indent loop subforms just like
loop keywords. Only matters when `lisp-loop-indent-subclauses'
is nil."
  :type 'boolean
  :group 'lisp-indent)

(defcustom lisp-align-keywords-in-calls t
  "Whether to align keyword arguments vertically or not.
If t (the default), keywords in contexts where no other
indentation rule takes precedence are aligned like this:

\(make-instance 'foo :bar t
                    :quux 42)

If nil, they are indented like any other function
call arguments:

\(make-instance 'foo :bar t
               :quux 42)"
  :type 'boolean
  :group 'lisp-indent)

(defcustom lisp-lambda-list-indentation t
  "Whether to indent lambda-lists specially. Defaults to t. Setting this to
nil makes `lisp-lambda-list-keyword-alignment',
`lisp-lambda-list-keyword-parameter-alignment', and
`lisp-lambda-list-keyword-parameter-indentation' meaningless, causing
lambda-lists to be indented as if they were data:

\(defun example (a b &optional o1 o2
                o3 o4
                &rest r
                &key k1 k2
                k3 k4)
  #|...|#)"
  :type 'boolean
  :group 'lisp-indent)

(defcustom lisp-lambda-list-keyword-alignment nil
  "Whether to vertically align lambda-list keywords together.
If nil (the default), keyworded lambda-list parts are aligned
with the initial mandatory arguments, like this:

\(defun foo (arg1 arg2 &rest rest
            &key key1 key2)
  #|...|#)

If non-nil, alignment is done with the first keyword
\(or falls back to the previous case), as in:

\(defun foo (arg1 arg2 &rest rest
                      &key key1 key2)
  #|...|#)"
  :type 'boolean
  :group 'lisp-indent)

(defcustom lisp-lambda-list-keyword-parameter-indentation 2
  "Indentation of lambda list keyword parameters.
See `lisp-lambda-list-keyword-parameter-alignment'
for more information."
  :type 'integer
  :group 'lisp-indent)

(defcustom lisp-lambda-list-keyword-parameter-alignment nil
  "Whether to vertically align lambda-list keyword parameters together.
If nil (the default), the parameters are aligned
with their corresponding keyword, plus the value of
`lisp-lambda-list-keyword-parameter-indentation', like this:

\(defun foo (arg1 arg2 &key key1 key2
                        key3 key4)
  #|...|#)

If non-nil, alignment is done with the first parameter
\(or falls back to the previous case), as in:

\(defun foo (arg1 arg2 &key key1 key2
                           key3 key4)
  #|...|#)"
  :type 'boolean
  :group 'lisp-indent)


(defvar lisp-indent-defun-method '(4 &lambda &body)
  "Defun-like indentation method.
This applies when the value of the `common-lisp-indent-function' property
is set to `defun'.")


;;;; Named styles.
;;;;
;;;; -*- common-lisp-style: foo -*-
;;;;
;;;; sets the style for the buffer.
;;;;
;;;; A Common Lisp style is a list of the form:
;;;;
;;;;  (NAME INHERIT VARIABLES INDENTATION HOOK DOCSTRING)
;;;;
;;;; where NAME is a symbol naming the style, INHERIT is the name of the style
;;;; it inherits from, VARIABLES is an alist specifying buffer local variables
;;;; for the style, and INDENTATION is an alist specifying non-standard
;;;; indentations for Common Lisp symbols. HOOK is a function to call when
;;;; activating the style. DOCSTRING is the documentation for the style.
;;;;
;;;; Convenience accessors `common-lisp-style-name', &co exist.
;;;;
;;;; `common-lisp-style' stores the name of the current style.
;;;;
;;;; `common-lisp-style-default' stores the name of the style to use when none
;;;; has been specified.
;;;;
;;;; `common-lisp-active-style' stores a cons of the list specifying the
;;;; current style, and a hash-table containing all indentation methods of
;;;; that style and any styles it inherits from. Whenever we're indenting, we
;;;; check that this is up to date, and recompute when necessary.
;;;;
;;;; Just setting the buffer local common-lisp-style will be enough to have
;;;; the style take effect. `common-lisp-set-style' can also be called
;;;; explicitly, however, and offers name completion, etc.

;;; Convenience accessors
(defun common-lisp-style-name (style) (first style))
(defun common-lisp-style-inherits (style) (second style))
(defun common-lisp-style-variables (style) (third style))
(defun common-lisp-style-indentation (style) (fourth style))
(defun common-lisp-style-hook (style) (fifth style))
(defun common-lisp-style-docstring (style) (sixth style))

(defun common-lisp-make-style (stylename inherits variables indentation hook
                               documentation)
  (list stylename inherits variables indentation hook documentation))

(defvar common-lisp-style nil)

;;; `define-common-lisp-style' updates the docstring of
;;; `common-lisp-style', using this as the base.
(put 'common-lisp-style 'common-lisp-style-base-doc
     "Name of the Common Lisp indentation style used in the current buffer.
Set this by giving eg.

  ;; -*- common-lisp-style: sbcl -*-

in the first line of the file, or by calling `common-lisp-set-style'. If
buffer has no style specified, but `common-lisp-style-default' is set, that
style is used instead. Use `define-common-lisp-style' to define new styles.")

(make-variable-buffer-local 'common-lisp-style)
(set-default 'common-lisp-style nil)

;;; `lisp-mode' kills all buffer-local variables. Setting the
;;; `permanent-local' property allows us to retain the style.
(put 'common-lisp-style 'permanent-local t)

;;; Mark as safe when the style doesn't evaluate arbitrary code.
(put 'common-lisp-style 'safe-local-variable 'common-lisp-safe-style-p)

;;; If style is being used, that's a sufficient invitation to snag
;;; the indentation function.
(defun common-lisp-lisp-mode-hook ()
  (let ((style (or common-lisp-style common-lisp-style-default)))
    (when style
      (set (make-local-variable 'lisp-indent-function)
           'common-lisp-indent-function)
      (common-lisp-set-style style))))
(add-hook 'lisp-mode-hook 'common-lisp-lisp-mode-hook)

;;; Common Lisp indentation style specifications.
(defvar common-lisp-styles (make-hash-table :test 'equal))

(defun common-lisp-delete-style (stylename)
  (remhash stylename common-lisp-styles))

(defun common-lisp-find-style (stylename)
  (let ((name (if (symbolp stylename)
                  (symbol-name stylename)
                stylename)))
    (or (gethash name common-lisp-styles)
        (error "Unknown Common Lisp style: %s" name))))

(defun common-lisp-safe-style-p (stylename)
  "True for known Common Lisp style without an :EVAL option.
Ie. styles that will not evaluate arbitrary code on activation."
  (let* ((style (ignore-errors (common-lisp-find-style stylename)))
         (base (common-lisp-style-inherits style)))
    (and style
         (not (common-lisp-style-hook style))
         (or (not base)
             (common-lisp-safe-style-p base)))))

(defun common-lisp-add-style (stylename inherits variables indentation hooks
                              documentation)
  ;; Invalidate indentation methods cached in common-lisp-active-style.
  (maphash (lambda (k v)
             (puthash k (copy-list v) common-lisp-styles))
           common-lisp-styles)
  ;; Add/Redefine the specified style.
  (puthash stylename
           (common-lisp-make-style stylename inherits variables indentation
                                   hooks documentation)
           common-lisp-styles)
  ;; Frob `common-lisp-style' docstring.
  (let ((doc (get 'common-lisp-style 'common-lisp-style-base-doc))
        (all nil))
    (setq doc (concat doc "\n\nAvailable styles are:\n"))
    (maphash (lambda (name style)
               (push (list name (common-lisp-style-docstring style)) all))
             common-lisp-styles)
    (dolist (info (sort all (lambda (a b) (string< (car a) (car b)))))
      (let ((style-name (first info))
            (style-doc (second info)))
        (if style-doc
            (setq doc (concat doc
                              "\n " style-name "\n"
                              "   " style-doc "\n"))
          (setq doc (concat doc
                            "\n " style-name " (undocumented)\n")))))
    (put 'common-lisp-style 'variable-documentation doc))
  stylename)

;;; Activate STYLENAME, adding its indentation methods to METHODS -- and
;;; recurse on style inherited from.
(defun common-lisp-activate-style (stylename methods)
  (let* ((style (common-lisp-find-style stylename))
         (basename (common-lisp-style-inherits style)))
    ;; Recurse on parent.
    (when basename
      (common-lisp-activate-style basename methods))
    ;; Copy methods
    (dolist (spec (common-lisp-style-indentation style))
      (puthash (first spec) (second spec) methods))
    ;; Bind variables.
    (dolist (var (common-lisp-style-variables style))
      (set (make-local-variable (first var)) (second var)))
    ;; Run hook.
    (let ((hook (common-lisp-style-hook style)))
      (when hook
        (funcall hook)))))

;;; When a style is being used, `common-lisp-active-style' holds a cons
;;;
;;;   (STYLE . METHODS)
;;;
;;; where STYLE is the list specifying the currently active style, and
;;; METHODS is the table of indentation methods --  including inherited
;;; ones -- for it. `common-lisp-active-style-methods' is reponsible
;;; for keeping this up to date.
(make-variable-buffer-local 'common-lisp-active-style)
(set-default 'common-lisp-active-style nil)

;;; Makes sure common-lisp-active-style corresponds to common-lisp-style, and
;;; pick up redefinitions, etc. Returns the method table for the currently
;;; active style.
(defun common-lisp-active-style-methods ()
  (let* ((name common-lisp-style)
         (style (when name (common-lisp-find-style name))))
    (if (eq style (car common-lisp-active-style))
        (cdr common-lisp-active-style)
      (when style
        (let ((methods (make-hash-table :test 'equal)))
          (common-lisp-activate-style name methods)
          (setq common-lisp-active-style (cons style methods))
          methods)))))

(defvar common-lisp-set-style-history nil)

(defun common-lisp-style-names ()
  (let (names)
    (maphash (lambda (k v)
               (push (cons k v) names))
             common-lisp-styles)
    names))

(defun common-lisp-set-style (stylename)
  "Set current buffer to use the Common Lisp style STYLENAME.
STYLENAME, a string, must be an existing Common Lisp style. Styles
are added (and updated) using `define-common-lisp-style'.

The buffer-local variable `common-lisp-style' will get set to STYLENAME.

A Common Lisp style is composed of local variables, indentation
specifications, and may also contain arbitrary elisp code to run upon
activation."
  (interactive
   (list (let ((completion-ignore-case t)
               (prompt "Specify Common Lisp indentation style: "))
           (completing-read prompt
                            (common-lisp-style-names) nil t nil
                            'common-lisp-set-style-history))))
  (setq common-lisp-style (common-lisp-style-name
                           (common-lisp-find-style stylename))
        common-lisp-active-style nil)
  ;; Actually activates the style.
  (common-lisp-active-style-methods)
  stylename)

(defmacro define-common-lisp-style (name documentation &rest options)
  "Define a Common Lisp indentation style.

NAME is the name of the style.

DOCUMENTATION is the docstring for the style, automatically added to the
docstring of `common-lisp-style'.

OPTIONS are:

 (:variables (name value) ...)

  Specifying the buffer local variables associated with the style.

 (:indentation (symbol spec) ...)

  Specifying custom indentations associated with the style. SPEC is
  a normal `common-lisp-indent-function' indentation specification.

 (:inherit style)

  Inherit variables and indentations from another Common Lisp style.

 (:eval form ...)

  Lisp code to evaluate when activating the style. This can be used to
  eg. activate other modes. It is possible that over the lifetime of
  a buffer same style gets activated multiple times, so code in :eval
  option should cope with that.
"
  (when (consp documentation)
    (setq options (cons documentation options)
          documentation nil))
  `(common-lisp-add-style ,name
                          ',(cadr (assoc :inherit options))
                          ',(cdr (assoc :variables options))
                          ',(cdr (assoc :indentation options))
                          ,(when (assoc :eval options)
                             `(lambda ()
                                ,@(cdr (assoc :eval options))))
                          ,documentation))

(define-common-lisp-style "basic"
  "This style merely gives all identation variables their default values,
   making it easy to create new styles that are proof against user
   customizations. It also adjusts comment indentation from default.
   All other predefined modes inherit from basic."
  (:variables
   (lisp-indent-maximum-backtracking 6)
   (lisp-tag-indentation 1)
   (lisp-tag-body-indentation 3)
   (lisp-backquote-indentation t)
   (lisp-loop-indent-subclauses t)
   (lisp-loop-indent-forms-like-keywords nil)
   (lisp-simple-loop-indentation 2)
   (lisp-align-keywords-in-calls t)
   (lisp-lambda-list-indentation t)
   (lisp-lambda-list-keyword-alignment nil)
   (lisp-lambda-list-keyword-parameter-indentation 2)
   (lisp-lambda-list-keyword-parameter-alignment nil)
   (lisp-indent-defun-method (4 &lambda &body))
   ;; Without these (;;foo would get a space inserted between
   ;; ( and ; by indent-sexp.
   (comment-indent-function (lambda () nil))))

(define-common-lisp-style "classic"
  "This style of indentation emulates the most striking features of 1995
   vintage cl-indent.el once included as part of Slime: IF indented by two
   spaces, and CASE clause bodies indentented more deeply than the keys."
  (:inherit "basic")
  (:variables
   (lisp-lambda-list-keyword-parameter-indentation 0))
  (:indentation
   (case (4 &rest (&whole 2 &rest 3)))
   (if   (4 2 2))))

(define-common-lisp-style "modern"
  "A good general purpose style. Turns on lambda-list keyword and keyword
   parameter alignment, and turns subclause aware loop indentation off.
   (Loop indentation so because simpler style is more prevalent in existing
   sources, not because it is necessarily preferred.)"
  (:inherit "basic")
  (:variables
   (lisp-lambda-list-keyword-alignment t)
   (lisp-lambda-list-keyword-parameter-alignment t)
   (lisp-lambda-list-keyword-parameter-indentation 0)
   (lisp-loop-indent-subclauses nil)))

(define-common-lisp-style "sbcl"
  "Style used in SBCL sources. A good if somewhat intrusive general purpose
   style based on the \"modern\" style. Adds indentation for a few SBCL
   specific constructs, sets indentation to use spaces instead of tabs,
   fill-column to 78, and activates whitespace-mode to show tabs and trailing
   whitespace."
  (:inherit "modern")
  (:eval
   (whitespace-mode 1))
  (:variables
   (whitespace-style (tabs trailing))
   (indent-tabs-mode nil)
   (comment-fill-column nil)
   (fill-column 78))
  (:indentation
   (def!constant       (as defconstant))
   (def!macro          (as defmacro))
   (def!method         (as defmethod))
   (def!struct         (as defstruct))
   (def!type           (as deftype))
   (defmacro-mundanely (as defmacro))
   (define-source-transform (as defun))
   (!def-type-translator (as defun))
   (!def-debug-command (as defun))))

(defcustom common-lisp-style-default nil
    "Name of the Common Lisp indentation style to use in lisp-mode buffers if
none has been specified."
  :type `(choice (const :tag "None" nil)
                 ,@(mapcar (lambda (spec)
                             `(const :tag ,(car spec) ,(car spec)))
                           (common-lisp-style-names))
                 (string :tag "Other"))
  :group 'lisp-indent)

;;;; The indentation specs are stored at three levels. In order of priority:
;;;;
;;;; 1. Indentation as set by current style, from the indentation table
;;;;    in the current style.
;;;;
;;;; 2. Globally set indentation, from the `common-lisp-indent-function'
;;;;    property of the symbol.
;;;;
;;;; 3. Per-package indentation derived by the system. A live Common Lisp
;;;;    system may (via Slime, eg.) add indentation specs to
;;;;    common-lisp-system-indentation, where they are associated with
;;;;    the package of the symbol. Then we run some lossy heuristics and
;;;;    find something that looks promising.
;;;;
;;;;    FIXME: for non-system packages the derived indentation should probably
;;;;    take precedence.

;;; This maps symbols into lists of (INDENT . PACKAGES) where INDENT is
;;; an indentation spec, and PACKAGES are the names of packages where this
;;; applies.
;;;
;;; We never add stuff here by ourselves: this is for things like Slime to
;;; fill.
(defvar common-lisp-system-indentation (make-hash-table :test 'equal))

(defun common-lisp-guess-current-package ()
  (let (pkg)
    (save-excursion
      (ignore-errors
        (when (let ((case-fold-search t))
                (search-backward "(in-package "))
          (re-search-forward "[ :\"]+")
          (let ((start (point)))
            (re-search-forward "[\":)]")
            (setf pkg (upcase (buffer-substring-no-properties
                               start (1- (point)))))))))
    pkg))

(defun common-lisp-current-package-function 'common-lisp-guess-current-package
  "Function used to the derive the package name to use for indentation at a
given point. Defaults to `common-lisp-guess-current-package'.")

(defun common-lisp-symbol-package (string)
  (if (and (stringp string) (string-match ":" string))
      (let ((p (match-beginning 0)))
        (if (eql 0 p)
            "KEYWORD"
          (upcase (substring string 0 p))))
    (funcall common-lisp-current-package-function)))

(defun common-lisp-get-indentation (name &optional full)
  "Retrieves the indentation information for NAME."
  (let ((method
         (or
          ;; From style
          (when common-lisp-style
            (gethash name (common-lisp-active-style-methods)))
          ;; From global settings.
          (get name 'common-lisp-indent-function)
          ;; From system derived information.
          (let ((system-info (gethash name common-lisp-system-indentation)))
            (if (not (cdr system-info))
                (caar system-info)
              (let ((guess nil)
                    (guess-n 0)
                    (package (common-lisp-symbol-package full)))
                (dolist (info system-info guess)
                  (let* ((pkgs (cdr info))
                         (n (length pkgs)))
                    (cond ((member package pkgs)
                           ;; This is it.
                           (return (car info)))
                          ((> n guess-n)
                           ;; If we can't find the real thing, go with the one
                           ;; accessible in most packages.
                           (setf guess (car info)
                                 guess-n n)))))))))))
    (if (and (consp method) (eq 'as (car method)))
        (common-lisp-get-indentation (cadr method))
      method)))

;;;; LOOP indentation, the simple version

(defun common-lisp-loop-type (loop-start)
  "Returns the type of the loop form at LOOP-START.
Possible types are SIMPLE, SIMPLE/SPLIT, EXTENDED, and EXTENDED/SPLIT. */SPLIT
refers to extended loops whose body does not start on the same line as the
opening parenthesis of the loop."
  (let (comment-split)
    (condition-case ()
        (save-excursion
          (goto-char loop-start)
          (let ((line (line-number-at-pos))
                (maybe-split t))
            (forward-char 1)
            (forward-sexp 1)
            (save-excursion
              (when (looking-at "\\s-*\\\n*;")
                (search-forward ";")
                (backward-char 1)
                (if (= line (line-number-at-pos))
                    (setq maybe-split nil)
                  (setq comment-split t))))
            (forward-sexp 1)
            (backward-sexp 1)
            (if (eql (char-after) ?\()
		(if (or (not maybe-split) (= line (line-number-at-pos)))
		    'simple
		    'simple/split)
              (if (or (not maybe-split) (= line (line-number-at-pos)))
		  'extended
		  'extended/split))))
      (error
       (if comment-split
           'simple/split
         'simple)))))

(defun common-lisp-trailing-comment ()
  (ignore-errors
    ;; If we had a trailing comment just before this, find it.
    (save-excursion
      (backward-sexp)
      (forward-sexp)
      (when (looking-at "\\s-*;")
        (search-forward ";")
        (1- (current-column))))))

(defun common-lisp-loop-part-indentation (indent-point state type)
  "Compute the indentation of loop form constituents."
  (let* ((loop-start (elt state 1))
         (loop-indentation (save-excursion
                             (goto-char loop-start)
                             (if (eq type 'extended/split)
                                 (- (current-column) 4)
                               (current-column))))
         (indent nil)
         (re "\\(\\(#?:\\)?\\sw+\\|)\\|\n\\)"))
    (goto-char indent-point)
    (back-to-indentation)
    (cond ((eq type 'simple/split)
           (+ loop-indentation lisp-simple-loop-indentation))
          ((eq type 'simple)
           (+ loop-indentation 6))
          ;; We are already in a body, with forms in it.
          ((and (not (looking-at re))
                (save-excursion
                  (while (and (ignore-errors (backward-sexp) t)
                              (not (looking-at re)))
                    (setq indent (current-column)))
                  (when (and indent
                             (looking-at
                              common-lisp-body-introducing-loop-macro-keyword))
                    t)))
           (list indent loop-start))
          ;; Keyword-style or comment outside body
          ((or lisp-loop-indent-forms-like-keywords
               (looking-at re)
               (looking-at ";"))
           (if (and (looking-at ";")
                    (let ((p (common-lisp-trailing-comment)))
                      (when p
                        (setq loop-indentation p))))
               (list loop-indentation loop-start)
             (list (+ loop-indentation 6) loop-start)))
          ;; Form-style
          (t
           (list (+ loop-indentation 9) loop-start)))))

;;;###autoload
(defun common-lisp-indent-function (indent-point state)
  "Function to indent the arguments of a Lisp function call.
This is suitable for use as the value of the variable
`lisp-indent-function'.  INDENT-POINT is the point at which the
indentation function is called, and STATE is the
`parse-partial-sexp' state at that position.  Browse the
`lisp-indent' customize group for options affecting the behavior
of this function.

If the indentation point is in a call to a Lisp function, that
function's common-lisp-indent-function property specifies how
this function should indent it.  Possible values for this
property are:

* defun, meaning indent according to `lisp-indent-defun-method';
  i.e., like (4 &lambda &body), as explained below.

* any other symbol, meaning a function to call.  The function should
  take the arguments: PATH STATE INDENT-POINT SEXP-COLUMN NORMAL-INDENT.
  PATH is a list of integers describing the position of point in terms of
  list-structure with respect to the containing lists.  For example, in
  ((a b c (d foo) f) g), foo has a path of (0 3 1).  In other words,
  to reach foo take the 0th element of the outermost list, then
  the 3rd element of the next list, and finally the 1st element.
  STATE and INDENT-POINT are as in the arguments to
  `common-lisp-indent-function'.  SEXP-COLUMN is the column of
  the open parenthesis of the innermost containing list.
  NORMAL-INDENT is the column the indentation point was
  originally in.  This function should behave like `lisp-indent-259'.

* an integer N, meaning indent the first N arguments like
  function arguments, and any further arguments like a body.
  This is equivalent to (4 4 ... &body).

* a list starting with `as' specifies an indirection: indentation is done as
  if the form being indented had started with the second element of the list.

* any other list.  The list element in position M specifies how to indent the
  Mth function argument.  If there are fewer elements than function arguments,
  the last list element applies to all remaining arguments.  The accepted list
  elements are:

  * nil, meaning the default indentation.

  * an integer, specifying an explicit indentation.

  * &lambda.  Indent the argument (which may be a list) by 4.

  * &rest.  When used, this must be the penultimate element.  The
    element after this one applies to all remaining arguments.

  * &body.  This is equivalent to &rest lisp-body-indent, i.e., indent
    all remaining elements by `lisp-body-indent'.

  * &whole.  This must be followed by nil, an integer, or a
    function symbol.  This indentation is applied to the
    associated argument, and as a base indent for all remaining
    arguments.  For example, an integer P means indent this
    argument by P, and all remaining arguments by P, plus the
    value specified by their associated list element.

  * a symbol.  A function to call, with the 6 arguments specified above.

  * a list, with elements as described above.  This applies when the
    associated function argument is itself a list.  Each element of the list
    specifies how to indent the associated argument.

For example, the function `case' has an indent property
\(4 &rest (&whole 2 &rest 1)), meaning:
  * indent the first argument by 4.
  * arguments after the first should be lists, and there may be any number
    of them.  The first list element has an offset of 2, all the rest
    have an offset of 2+1=3."
  (common-lisp-indent-function-1 indent-point state))

;;; XEmacs doesn't have looking-back, so we define a simple one. Faster to
;;; boot, and sufficient for our needs.
(defun common-lisp-looking-back (string)
  (let ((len (length string)))
    (dotimes (i len t)
      (unless (eql (elt string (- len i 1)) (char-before (- (point) i)))
        (return nil)))))

(defvar common-lisp-feature-expr-regexp "#!?\\(+\\|-\\)")

;;; Semi-feature-expression aware keyword check.
(defun common-lisp-looking-at-keyword ()
  (or (looking-at ":")
      (and (looking-at common-lisp-feature-expr-regexp)
           (save-excursion
             (forward-sexp)
             (skip-chars-forward " \t\n")
             (common-lisp-looking-at-keyword)))))

;;; Semi-feature-expression aware backwards movement for keyword
;;; argument pairs.
(defun common-lisp-backward-keyword-argument ()
  (ignore-errors
    (backward-sexp 2)
    (when (looking-at common-lisp-feature-expr-regexp)
      (cond ((ignore-errors
               (save-excursion
                 (backward-sexp 2)
                 (looking-at common-lisp-feature-expr-regexp)))
             (common-lisp-backward-keyword-argument))
            ((ignore-errors
               (save-excursion
                 (backward-sexp 1)
                 (looking-at ":")))
             (backward-sexp))))
    t))

(defun common-lisp-indent-function-1 (indent-point state)
  ;; If we're looking at a splice, move to the first comma.
  (when (or (common-lisp-looking-back ",") (common-lisp-looking-back ",@"))
    (when (re-search-backward "[^,@'],")
      (forward-char 1)))
  (let ((normal-indent (current-column)))
    ;; Walk up list levels until we see something
    ;;  which does special things with subforms.
    (let ((depth 0)
          ;; Path describes the position of point in terms of
          ;;  list-structure with respect to containing lists.
          ;; `foo' has a path of (0 3 1) in `((a b c (d foo) f) g)'.
          (path ())
          ;; set non-nil when somebody works out the indentation to use
          calculated
          ;; If non-nil, this is an indentation to use
          ;; if nothing else specifies it more firmly.
          tentative-calculated
          (last-point indent-point)
          ;; the position of the open-paren of the innermost containing list
          (containing-form-start (common-lisp-indent-parse-state-start state))
          ;; the column of the above
          sexp-column)
      ;; Move to start of innermost containing list
      (goto-char containing-form-start)
      (setq sexp-column (current-column))

      ;; Look over successively less-deep containing forms
      (while (and (not calculated)
                  (< depth lisp-indent-maximum-backtracking))
        (let ((containing-sexp (point)))
          (forward-char 1)
          (parse-partial-sexp (point) indent-point 1 t)
          ;; Move to the car of the relevant containing form
          (let (tem full function method tentative-defun)
            (if (not (looking-at "\\sw\\|\\s_"))
                ;; This form doesn't seem to start with a symbol
                (setq function nil method nil full nil)
              (setq tem (point))
              (forward-sexp 1)
              (setq full (downcase (buffer-substring-no-properties
                                        tem (point)))
                    function full)
              (goto-char tem)
              (setq tem (intern-soft function)
                    method (common-lisp-get-indentation tem))
              (cond ((and (null method)
                          (string-match ":[^:]+" function))
                     ;; The pleblisp package feature
                     (setq function (substring function
                                               (1+ (match-beginning 0)))
                           method (common-lisp-get-indentation
                                   (intern-soft function) full)))
                    ((and (null method))
                     ;; backwards compatibility
                     (setq method (common-lisp-get-indentation tem)))))
            (let ((n 0))
              ;; How far into the containing form is the current form?
              (if (< (point) indent-point)
                  (while (condition-case ()
                             (progn
                               (forward-sexp 1)
                               (if (>= (point) indent-point)
                                   nil
                                 (parse-partial-sexp (point)
                                                     indent-point 1 t)
                                 (setq n (1+ n))
                                 t))
                           (error nil))))
              (setq path (cons n path)))

            ;; Guess.
            (when (and (not method) function (null (cdr path)))
              ;; (package prefix was stripped off above)
              (cond ((and (string-match "\\`def" function)
                          (not (string-match "\\`default" function))
                          (not (string-match "\\`definition" function))
                          (not (string-match "\\`definer" function)))
                     (setq tentative-defun t))
                    ((string-match
                      (eval-when-compile
                        (concat "\\`\\("
                                (regexp-opt '("with" "without" "do"))
                                "\\)-"))
                      function)
                     (setq method '(&lambda &body)))))

            ;; #+ and #- cleverness.
            (save-excursion
              (goto-char indent-point)
              (backward-sexp)
              (let ((indent (current-column)))
                (when (or (looking-at common-lisp-feature-expr-regexp)
                          (ignore-errors
                            (backward-sexp)
                            (when (looking-at
                                   common-lisp-feature-expr-regexp)
                              (setq indent (current-column))
                              (let ((line (line-number-at-pos)))
                                (while
                                    (ignore-errors
                                      (backward-sexp 2)
                                      (and
                                       (= line (line-number-at-pos))
                                       (looking-at
                                        common-lisp-feature-expr-regexp)))
                                  (setq indent (current-column))))
                              t)))
                  (setq calculated (list indent containing-form-start)))))

            (cond ((and (or (eq (char-after (1- containing-sexp)) ?\')
                            (and (not lisp-backquote-indentation)
                                 (eq (char-after (1- containing-sexp)) ?\`)))
                        (not (eq (char-after (- containing-sexp 2)) ?\#)))
                   ;; No indentation for "'(...)" elements
                   (setq calculated (1+ sexp-column)))
                  ((eq (char-after (1- containing-sexp)) ?\#)
                   ;; "#(...)"
                   (setq calculated (1+ sexp-column)))
                  ((null method)
                   ;; If this looks like a call to a `def...' form,
                   ;; think about indenting it as one, but do it
                   ;; tentatively for cases like
                   ;; (flet ((defunp ()
                   ;;          nil)))
                   ;; Set both normal-indent and tentative-calculated.
                   ;; The latter ensures this value gets used
                   ;; if there are no relevant containing constructs.
                   ;; The former ensures this value gets used
                   ;; if there is a relevant containing construct
                   ;; but we are nested within the structure levels
                   ;; that it specifies indentation for.
                   (if tentative-defun
                       (setq tentative-calculated
                             (common-lisp-indent-call-method
                              function lisp-indent-defun-method
                              path state indent-point
                              sexp-column normal-indent)
                             normal-indent tentative-calculated)
                     (when lisp-align-keywords-in-calls
                       ;; No method so far. If we're looking at a keyword,
                       ;; align with the first keyword in this expression.
                       ;; This gives a reasonable indentation to most things
                       ;; with keyword arguments.
                       (save-excursion
                         (goto-char indent-point)
                         (back-to-indentation)
                         (when (common-lisp-looking-at-keyword)
                           (while (common-lisp-backward-keyword-argument)
                             (when (common-lisp-looking-at-keyword)
                               (setq calculated
                                     (list (current-column)
                                           containing-form-start)))))))))
                  ((integerp method)
                   ;; convenient top-level hack.
                   ;;  (also compatible with lisp-indent-function)
                   ;; The number specifies how many `distinguished'
                   ;;  forms there are before the body starts
                   ;; Equivalent to (4 4 ... &body)
                   (setq calculated (cond ((cdr path)
                                           normal-indent)
                                          ((<= (car path) method)
                                           ;; `distinguished' form
                                           (list (+ sexp-column 4)
                                                 containing-form-start))
                                          ((= (car path) (1+ method))
                                           ;; first body form.
                                           (+ sexp-column lisp-body-indent))
                                          (t
                                           ;; other body form
                                           normal-indent))))
                  (t
                   (setq calculated
                         (common-lisp-indent-call-method
                          function method path state indent-point
                          sexp-column normal-indent)))))
          (goto-char containing-sexp)
          (setq last-point containing-sexp)
          (unless calculated
            (condition-case ()
                (progn (backward-up-list 1)
                       (setq depth (1+ depth)))
              (error
               (setq depth lisp-indent-maximum-backtracking))))))

      (or calculated tentative-calculated
          ;; Fallback.
          ;;
          ;; Instead of punting directly to calculate-lisp-indent we
          ;; handle a few of cases it doesn't deal with:
          ;;
          ;; A: (foo (
          ;;          bar zot
          ;;          quux))
          ;;
          ;;    would align QUUX with ZOT.
          ;;
          ;; B:
          ;;   (foo (or x
          ;;            y) t
          ;;        z)
          ;;
          ;;   would align the Z with Y.
          ;;
          ;; C:
          ;;   (foo ;; Comment
          ;;        (bar)
          ;;        ;; Comment 2
          ;;        (quux))
          ;;
          ;;   would indent BAR and QUUX by one.
          (ignore-errors
            (save-excursion
              (goto-char indent-point)
              (back-to-indentation)
              (let ((p (point)))
                (goto-char containing-sexp)
                (down-list)
                (let ((one (current-column)))
                  (skip-chars-forward " \t")
                  (if (or (eolp) (looking-at ";"))
                      ;; A.
                      (list one containing-form-start)
                    (forward-sexp 2)
                    (backward-sexp)
                    (if (/= p (point))
                        ;; B.
                        (list (current-column) containing-form-start)
                      (backward-sexp)
                      (forward-sexp)
                      (let ((tmp (+ (current-column) 1)))
                        (skip-chars-forward " \t")
                        (if (looking-at ";")
                            ;; C.
                            (list tmp containing-form-start)))))))))))))


(defun common-lisp-indent-call-method (function method path state indent-point
                                       sexp-column normal-indent)
  (let ((lisp-indent-error-function function))
    (if (symbolp method)
        (funcall method
                 path state indent-point
                 sexp-column normal-indent)
      (lisp-indent-259 method path state indent-point
                       sexp-column normal-indent))))

;; Dynamically bound in common-lisp-indent-call-method.
(defvar lisp-indent-error-function)

(defun lisp-indent-report-bad-format (m)
  (error "%s has a badly-formed %s property: %s"
         ;; Love those free variable references!!
         lisp-indent-error-function 'common-lisp-indent-function m))


;; Lambda-list indentation is now done in LISP-INDENT-LAMBDA-LIST.
;; See also `lisp-lambda-list-keyword-alignment',
;; `lisp-lambda-list-keyword-parameter-alignment' and
;; `lisp-lambda-list-keyword-parameter-indentation' -- dvl

(defvar lisp-indent-lambda-list-keywords-regexp
  "&\\(\
optional\\|rest\\|key\\|allow-other-keys\\|aux\\|whole\\|body\\|\
environment\\|more\
\\)\\>"
  "Regular expression matching lambda-list keywords.")

(defun lisp-indent-lambda-list
    (indent-point sexp-column containing-form-start)
  (if (not lisp-lambda-list-indentation)
      (1+ sexp-column)
    (lisp-properly-indent-lambda-list
     indent-point sexp-column containing-form-start)))

(defun lisp-properly-indent-lambda-list
    (indent-point sexp-column containing-form-start)
  (let (limit)
    (cond
     ((save-excursion
        (goto-char indent-point)
        (back-to-indentation)
        (setq limit (point))
        (looking-at lisp-indent-lambda-list-keywords-regexp))
      ;; We're facing a lambda-list keyword.
      (if lisp-lambda-list-keyword-alignment
          ;; Align to the first keyword if any, or to the beginning of
          ;; the lambda-list.
          (save-excursion
            (goto-char containing-form-start)
            (down-list)
            (let ((key-indent nil)
                  (next t))
              (while (and next (< (point) indent-point))
                (if (looking-at lisp-indent-lambda-list-keywords-regexp)
                    (setq key-indent (current-column)
                          next nil)
                  (setq next (ignore-errors (forward-sexp) t))
                  (if next
                      (ignore-errors
                        (forward-sexp)
                        (backward-sexp)))))
              (or key-indent
                  (1+ sexp-column))))
        ;; Align to the beginning of the lambda-list.
        (1+ sexp-column)))
     (t
      ;; Otherwise, align to the first argument of the last lambda-list
      ;; keyword, the keyword itself, or the beginning of the
      ;; lambda-list.
      (save-excursion
        (goto-char indent-point)
        (let ((indent nil)
              (next t))
          (while (and next (> (point) containing-form-start))
            (setq next (ignore-errors (backward-sexp) t))
            (let* ((col (current-column))
                   (pos
                    (save-excursion
                      (ignore-errors (forward-sexp))
                      (skip-chars-forward " \t")
                      (if (eolp)
                          (+ col
                             lisp-lambda-list-keyword-parameter-indentation)
                        col))))
              (if (looking-at lisp-indent-lambda-list-keywords-regexp)
                  (setq indent
                        (if lisp-lambda-list-keyword-parameter-alignment
                            (or indent pos)
                          (+ col
                             lisp-lambda-list-keyword-parameter-indentation))
                        next nil)
                (setq indent col))))
          (or indent (1+ sexp-column))))))))

(defun common-lisp-lambda-list-initial-value-form-p (point)
  (let ((state 'x)
        (point (save-excursion
                 (goto-char point)
                 (back-to-indentation)
                 (point))))
    (save-excursion
      (backward-sexp)
      (ignore-errors (down-list 1))
      (while (and point (< (point) point))
        (cond ((or (looking-at "&key") (looking-at "&optional")
                   (looking-at "&aux"))
               (setq state 'key))
              ((looking-at lisp-indent-lambda-list-keywords-regexp)
               (setq state 'x)))
        (if (not (ignore-errors (forward-sexp) t))
            (setq point nil)
          (ignore-errors
            (forward-sexp)
            (backward-sexp))
          (cond ((> (point) point)
                 (backward-sexp)
                 (when (eq state 'var)
                   (setq state 'x))
                 (or (ignore-errors
                       (down-list 1)
                       (cond ((> (point) point)
                              (backward-up-list))
                             ((eq 'key state)
                              (setq state 'var)))
                       t)
                     (setq point nil)))
                 ((eq state 'var)
                  (setq state 'form))))))
      (eq 'form state)))

;; Blame the crufty control structure on dynamic scoping
;;  -- not on me!
(defun lisp-indent-259
    (method path state indent-point sexp-column normal-indent)
  (catch 'exit
    (let* ((p (cdr path))
           (containing-form-start (elt state 1))
           (n (1- (car path)))
           tem tail)
      (if (not (consp method))
          (lisp-indent-report-bad-format method))
      (while n
        ;; This while loop is for advancing along a method
        ;; until the relevant (possibly &rest/&body) pattern
        ;; is reached.
        ;; n is set to (1- n) and method to (cdr method)
        ;; each iteration.
        (setq tem (car method))

        (or (eq tem 'nil)             ;default indentation
            (eq tem '&lambda)         ;lambda list
            (and (eq tem '&body) (null (cdr method)))
            (and (eq tem '&rest)
                 (consp (cdr method))
                 (null (cddr method)))
            (integerp tem)            ;explicit indentation specified
            (and (consp tem)          ;destructuring
                 (or (consp (car tem))
                     (and (eq (car tem) '&whole)
                          (or (symbolp (cadr tem))
                              (integerp (cadr tem))))))
            (and (symbolp tem)        ;a function to call to do the work.
                 (null (cdr method)))
            (lisp-indent-report-bad-format method))
        (cond ((eq tem '&body)
               ;; &body means (&rest <lisp-body-indent>)
               (throw 'exit
                      (if (null p)
                          (+ sexp-column lisp-body-indent)
                        normal-indent)))
              ((eq tem '&rest)
               ;; this pattern holds for all remaining forms
               (setq tail (> n 0)
                     n 0
                     method (cdr method)))
              ((> n 0)
               ;; try next element of pattern
               (setq n (1- n)
                     method (cdr method))
               (if (< n 0)
                   ;; Too few elements in pattern.
                   (throw 'exit normal-indent)))
              ((eq tem 'nil)
               (throw 'exit (if (consp normal-indent)
                                normal-indent
                              (list normal-indent containing-form-start))))
              ((eq tem '&lambda)
               (throw 'exit
                      (cond ((not (common-lisp-looking-back ")"))
                             ;; If it's not a list at all, indent it
                             ;; like body instead.
                             (if (null p)
                                 (+ sexp-column lisp-body-indent)
                               normal-indent))
                            ((common-lisp-lambda-list-initial-value-form-p
                              indent-point)
                             (if (consp normal-indent)
                                 normal-indent
                               (list normal-indent containing-form-start)))
                            ((null p)
                             (list (+ sexp-column 4) containing-form-start))
                            (t
                             ;; Indentation within a lambda-list. -- dvl
                             (list (lisp-indent-lambda-list
                                    indent-point
                                    sexp-column
                                    containing-form-start)
                                   containing-form-start)))))
              ((integerp tem)
               (throw 'exit
                      (if (null p)         ;not in subforms
                          (list (+ sexp-column tem) containing-form-start)
                        normal-indent)))
              ((symbolp tem)          ;a function to call
               (throw 'exit
                      (funcall tem path state indent-point
                               sexp-column normal-indent)))
              (t
               ;; must be a destructing frob
               (if p
                   ;; descend
                   (setq method (cddr tem)
                         n (car p)
                         p (cdr p)
                         tail nil)
                 (let ((wholep (eq '&whole (car tem))))
                   (setq tem (cadr tem))
                   (throw 'exit
                          (cond (tail
                                 (if (and wholep (integerp tem)
                                          (save-excursion
                                            (goto-char indent-point)
                                            (back-to-indentation)
                                            (looking-at "\\sw")))
                                     ;; There's a further level of
                                     ;; destructuring, but we're looking at a
                                     ;; word -- indent to sexp.
                                     (+ sexp-column tem)
                                   normal-indent))
                                ((not tem)
                                 (list normal-indent
                                       containing-form-start))
                                ((integerp tem)
                                 (list (+ sexp-column tem)
                                       containing-form-start))
                                (t
                                 (funcall tem path state indent-point
                                          sexp-column normal-indent))))))))))))

(defun lisp-indent-tagbody (path state indent-point sexp-column normal-indent)
  (if (not (null (cdr path)))
      normal-indent
    (save-excursion
      (goto-char indent-point)
      (back-to-indentation)
      (list (cond ((looking-at "\\sw\\|\\s_")
                   ;; a tagbody tag
                   (+ sexp-column lisp-tag-indentation))
                  ((integerp lisp-tag-body-indentation)
                   (+ sexp-column lisp-tag-body-indentation))
                  ((eq lisp-tag-body-indentation 't)
                   (condition-case ()
                       (progn (backward-sexp 1) (current-column))
                     (error (1+ sexp-column))))
                  (t (+ sexp-column lisp-body-indent)))
;            (cond ((integerp lisp-tag-body-indentation)
;                   (+ sexp-column lisp-tag-body-indentation))
;                  ((eq lisp-tag-body-indentation 't)
;                   normal-indent)
;                  (t
;                   (+ sexp-column lisp-body-indent)))
            (elt state 1)
            ))))

(defun lisp-indent-do (path state indent-point sexp-column normal-indent)
  (if (>= (car path) 3)
      (let ((lisp-tag-body-indentation lisp-body-indent))
        (funcall (function lisp-indent-tagbody)
                 path state indent-point sexp-column normal-indent))
    (funcall (function lisp-indent-259)
             '((&whole nil &rest
                ;; the following causes weird indentation
                ;;(&whole 1 1 2 nil)
                )
               (&whole nil &rest 1))
             path state indent-point sexp-column normal-indent)))

(defun lisp-indent-defsetf
    (path state indent-point sexp-column normal-indent)
  (list
   (cond
    ;; Inside the lambda-list in a long-form defsetf.
    ((and (eql 2 (car path)) (cdr path))
     (lisp-indent-lambda-list indent-point sexp-column (elt state 1)))
    ;; Long form: has a lambda-list.
    ((or (cdr path)
         (save-excursion
           (goto-char (elt state 1))
           (ignore-errors
             (down-list)
             (forward-sexp 3)
             (backward-sexp)
             (looking-at "nil\\|("))))
     (+ sexp-column
        (case (car path)
          ((1 3) 4)
          (2 4)
          (t 2))))
    ;; Short form.
    (t
     (+ sexp-column
        (case (car path)
          (1 4)
          (2 4)
          (t 2)))))
   (elt state 1)))

(defun lisp-beginning-of-defmethod-qualifiers ()
  (let ((regexp-1 "(defmethod\\|(DEFMETHOD")
        (regexp-2 "(:method\\|(:METHOD"))
    (while (and (not (or (looking-at regexp-1)
                         (looking-at regexp-2)))
                (ignore-errors (backward-up-list) t)))
    (cond ((looking-at regexp-1)
           (forward-char)
           ;; Skip name.
           (forward-sexp 2)
           1)
          ((looking-at regexp-2)
           (forward-char)
           (forward-sexp 1)
           0))))

;; LISP-INDENT-DEFMETHOD now supports the presence of more than one method
;; qualifier and indents the method's lambda list properly. -- dvl
(defun lisp-indent-defmethod
    (path state indent-point sexp-column normal-indent)
  (lisp-indent-259
   (let ((nskip nil))
     (if (save-excursion
           (when (setq nskip (lisp-beginning-of-defmethod-qualifiers))
             (skip-chars-forward " \t\n")
             (while (looking-at "\\sw\\|\\s_")
               (incf nskip)
               (forward-sexp)
               (skip-chars-forward " \t\n"))
             t))
         (append (make-list nskip 4) '(&lambda &body))
       (common-lisp-get-indentation 'defun)))
   path state indent-point sexp-column normal-indent))

(defun lisp-indent-function-lambda-hack (path state indent-point
                                         sexp-column normal-indent)
  ;; indent (function (lambda () <newline> <body-forms>)) kludgily.
  (if (or (cdr path) ; wtf?
          (> (car path) 3))
      ;; line up under previous body form
      normal-indent
    ;; line up under function rather than under lambda in order to
    ;;  conserve horizontal space.  (Which is what #' is for.)
    (condition-case ()
        (save-excursion
          (backward-up-list 2)
          (forward-char 1)
          (if (looking-at "\\(lisp:+\\)?function\\(\\Sw\\|\\S_\\)")
              (+ lisp-body-indent -1 (current-column))
              (+ sexp-column lisp-body-indent)))
       (error (+ sexp-column lisp-body-indent)))))

(defun lisp-indent-loop (path state indent-point sexp-column normal-indent)
  (if (cdr path)
      normal-indent
    (let* ((loop-start (elt state 1))
           (type (common-lisp-loop-type loop-start)))
      (cond ((and lisp-loop-indent-subclauses
                  (member type '(extended extended/split)))
             (list (common-lisp-indent-loop-macro-1 state indent-point)
                   (common-lisp-indent-parse-state-start state)))
            (t
             (common-lisp-loop-part-indentation indent-point state type))))))

;;;; LOOP indentation, the complex version -- handles subclause indentation

;; Regexps matching various varieties of loop macro keyword ...
(defvar common-lisp-body-introducing-loop-macro-keyword
  "\\(#?:\\)?\\(do\\|finally\\|initially\\)"
  "Regexp matching loop macro keywords which introduce body-forms.")

;; This is so "and when" and "else when" get handled right
;; (not to mention "else do" !!!)
(defvar common-lisp-prefix-loop-macro-keyword
  "\\(#?:\\)?\\(and\\|else\\)"
  "Regexp matching loop macro keywords which are prefixes.")

(defvar common-lisp-indent-clause-joining-loop-macro-keyword
  "\\(#?:\\)?and"
  "Regexp matching 'and', and anything else there ever comes to be like it.")

;; This is handled right, but it's incomplete ...
;; (It could probably get arbitrarily long if I did *every* iteration-path)
(defvar common-lisp-indent-indented-loop-macro-keyword
  "\\(#?:\\)\
?\\(into\\|by\\|upto\\|downto\\|above\\|below\\|on\\|being\\|=\\|first\\|\
then\\|from\\|to\\)"
  "Regexp matching keywords introducing loop subclauses.
Always indented two.")

(defvar common-lisp-indenting-loop-macro-keyword
  "\\(#?:\\)?\\(when\\|unless\\|if\\)"
  "Regexp matching keywords introducing conditional clauses.
Cause subsequent clauses to be indented.")

(defvar common-lisp-loop-macro-else-keyword "\\(#?:\\)?else")

;;; Attempt to indent the loop macro ...

(defun common-lisp-indent-parse-state-depth (parse-state)
  (car parse-state))

(defun common-lisp-indent-parse-state-start (parse-state)
  (car (cdr parse-state)))

(defun common-lisp-indent-parse-state-prev (parse-state)
  (car (cdr (cdr parse-state))))

(defun common-lisp-indent-loop-macro-1 (parse-state indent-point)
  (catch 'return-indentation
    (save-excursion
      ;; Find first clause of loop macro, and use it to establish
      ;; base column for indentation
      (goto-char (common-lisp-indent-parse-state-start parse-state))
      (let ((loop-start-column (current-column)))
        (common-lisp-loop-advance-past-keyword-on-line)

        (when (eolp)
          (forward-line 1)
          (end-of-line)
          ;; If indenting first line after "(loop <newline>"
          ;; cop out ...
          (if (<= indent-point (point))
              (throw 'return-indentation (+ 2 loop-start-column)))
          (back-to-indentation))

        (let* ((case-fold-search t)
               (loop-macro-first-clause (point))
               (previous-expression-start
                (common-lisp-indent-parse-state-prev parse-state))
               (default-value (current-column))
               (loop-body-p nil)
               (loop-body-indentation nil)
               (indented-clause-indentation (+ 2 default-value)))
          ;; Determine context of this loop clause, starting with the
          ;; expression immediately preceding the line we're trying to indent
          (goto-char previous-expression-start)

          ;; Handle a body-introducing-clause which ends a line specially.
          (if (looking-at common-lisp-body-introducing-loop-macro-keyword)
              (let ((keyword-position (current-column)))
                (setq loop-body-p t)
                (setq loop-body-indentation
                      (if (common-lisp-loop-advance-past-keyword-on-line)
                          (current-column)
                        (back-to-indentation)
                        (if (/= (current-column) keyword-position)
                            (+ 2 (current-column))
                          (+ keyword-position 3)))))

            (back-to-indentation)
            (if (< (point) loop-macro-first-clause)
                (goto-char loop-macro-first-clause))
            ;; If there's an "and" or "else," advance over it.
            ;; If it is alone on the line, the next "cond" will treat it
            ;; as if there were a "when" and indent under it ...
            (let ((exit nil))
              (while (and (null exit)
                          (looking-at common-lisp-prefix-loop-macro-keyword))
                (if (null (common-lisp-loop-advance-past-keyword-on-line))
                    (progn (setq exit t)
                           (back-to-indentation)))))

            ;; Found start of loop clause preceding the one we're
            ;; trying to indent. Glean context ...
            (cond
             ((looking-at "(")
              ;; We're in the middle of a clause body ...
              (setq loop-body-p t)
              (setq loop-body-indentation (current-column)))
             ((looking-at common-lisp-body-introducing-loop-macro-keyword)
              (setq loop-body-p t)
              ;; Know there's something else on the line (or would
              ;; have been caught above)
              (common-lisp-loop-advance-past-keyword-on-line)
              (setq loop-body-indentation (current-column)))
             (t
              (setq loop-body-p nil)
              (if (or (looking-at common-lisp-indenting-loop-macro-keyword)
                      (looking-at common-lisp-prefix-loop-macro-keyword))
                  (setq default-value (+ 2 (current-column))))
              (setq indented-clause-indentation (+ 2 (current-column)))
              ;; We still need loop-body-indentation for "syntax errors" ...
              (goto-char previous-expression-start)
              (setq loop-body-indentation (current-column)))))

          ;; Go to first non-blank character of the line we're trying
          ;; to indent. (if none, wind up poised on the new-line ...)
          (goto-char indent-point)
          (back-to-indentation)
          (cond
           ((looking-at "(")
            ;; Clause body ...
            loop-body-indentation)
           ((or (eolp) (looking-at ";"))
            ;; Blank line.  If body-p, indent as body, else indent as
            ;; vanilla clause.
            (if loop-body-p
                loop-body-indentation
              (or (and (looking-at ";") (common-lisp-trailing-comment))
                  default-value)))
           ((looking-at common-lisp-indent-indented-loop-macro-keyword)
            indented-clause-indentation)
           ((looking-at common-lisp-indent-clause-joining-loop-macro-keyword)
            (let ((stolen-indent-column nil))
              (forward-line -1)
              (while (and (null stolen-indent-column)
                          (> (point) loop-macro-first-clause))
                (back-to-indentation)
                (if (and (< (current-column) loop-body-indentation)
                         (looking-at "\\(#?:\\)?\\sw"))
                    (progn
                      (if (looking-at common-lisp-loop-macro-else-keyword)
                          (common-lisp-loop-advance-past-keyword-on-line))
                      (setq stolen-indent-column
                            (current-column)))
                  (forward-line -1)))
              (if stolen-indent-column
                  stolen-indent-column
                default-value)))
           (t default-value)))))))

(defun common-lisp-loop-advance-past-keyword-on-line ()
  (forward-word 1)
  (while (and (looking-at "\\s-") (not (eolp)))
    (forward-char 1))
  (if (eolp)
      nil
    (current-column)))

;;;; IF* is not standard, but a plague upon the land
;;;; ...let's at least try to indent it.

(defvar common-lisp-indent-if*-keyword
  "threnret\\|elseif\\|then\\|else"
  "Regexp matching if* keywords")

(defun common-lisp-indent-if*
    (path parse-state indent-point sexp-column normal-indent)
  (list (common-lisp-indent-if*-1 parse-state indent-point)
	(common-lisp-indent-parse-state-start parse-state)))

(defun common-lisp-indent-if*-1 (parse-state indent-point)
  (catch 'return-indentation
    (save-excursion
      ;; Find first clause of if* macro, and use it to establish
      ;; base column for indentation
      (goto-char (common-lisp-indent-parse-state-start parse-state))
      (let ((if*-start-column (current-column)))
	(common-lisp-indent-if*-advance-past-keyword-on-line)
	(let* ((case-fold-search t)
	       (if*-first-clause (point))
	       (previous-expression-start
                (common-lisp-indent-parse-state-prev parse-state))
	       (default-value (current-column))
	       (if*-body-p nil)
	       (if*-body-indentation nil))
	  ;; Determine context of this if* clause, starting with the
	  ;; expression immediately preceding the line we're trying to indent
	  (goto-char previous-expression-start)
	  ;; Handle a body-introducing-clause which ends a line specially.
	  (back-to-indentation)
          (if (< (point) if*-first-clause)
              (goto-char if*-first-clause))
          ;; Found start of if* clause preceding the one we're trying
          ;; to indent. Glean context ...
          (cond
           ((looking-at common-lisp-indent-if*-keyword)
            (setq if*-body-p t)
            ;; Know there's something else on the line (or would
            ;; have been caught above)
            (common-lisp-indent-if*-advance-past-keyword-on-line)
            (setq if*-body-indentation (current-column)))
           ((looking-at "#'\\|'\\|(")
            ;; We're in the middle of a clause body ...
            (setq if*-body-p t)
            (setq if*-body-indentation (current-column)))
           (t
            (setq if*-body-p nil)
            ;; We still need if*-body-indentation for "syntax errors" ...
            (goto-char previous-expression-start)
            (setq if*-body-indentation (current-column))))

          ;; Go to first non-blank character of the line we're trying
          ;; to indent. (if none, wind up poised on the new-line ...)
          (goto-char indent-point)
          (back-to-indentation)
          (cond
           ((or (eolp) (looking-at ";"))
            ;; Blank line.  If body-p, indent as body, else indent as
            ;; vanilla clause.
            (if if*-body-p
                if*-body-indentation
              default-value))
           ((not (looking-at common-lisp-indent-if*-keyword))
            ;; Clause body ...
            if*-body-indentation)
           (t
            (- (+ 7 if*-start-column)
               (- (match-end 0) (match-beginning 0))))))))))

(defun common-lisp-indent-if*-advance-past-keyword-on-line ()
  (forward-word 1)
  (block move-forward
    (while (and (looking-at "\\s-") (not (eolp)))
      (forward-char 1)))
  (if (eolp)
      nil
    (current-column)))


;;;; Indentation specs for standard symbols, and a few semistandard ones.
(defun common-lisp-init-standard-indentation ()
  (let ((l '((block 1)
             (case        (4 &rest (&whole 2 &rest 1)))
             (ccase       (as case))
             (ecase       (as case))
             (typecase    (as case))
             (etypecase   (as case))
             (ctypecase   (as case))
             (catch 1)
             (cond        (&rest (&whole 2 &rest nil)))
             ;; for DEFSTRUCT
             (:constructor (4 &lambda))
             (defvar      (4 2 2))
             (defclass    (6 (&whole 4 &rest 1)
                             (&whole 2 &rest 1)
                             (&whole 2 &rest 1)))
             (defconstant (as defvar))
             (defcustom   (4 2 2 2))
             (defparameter     (as defvar))
             (defconst         (as defcustom))
             (define-condition (as defclass))
             (define-modify-macro (4 &lambda &body))
             (defsetf      lisp-indent-defsetf)
             (defun       (4 &lambda &body))
             (defgeneric  (4 &lambda &body))
             (define-setf-method   (as defun))
             (define-setf-expander (as defun))
             (defmacro     (as defun))
             (defsubst     (as defun))
             (deftype      (as defun))
             (defmethod   lisp-indent-defmethod)
             (defpackage  (4 2))
             (defstruct   ((&whole 4 &rest (&whole 2 &rest 1))
                           &rest (&whole 2 &rest 1)))
             (destructuring-bind (&lambda 4 &body))
             (do          lisp-indent-do)
             (do*         (as do))
             (dolist      ((&whole 4 2 1) &body))
             (dotimes     (as dolist))
             (eval-when   1)
             (flet        ((&whole 4 &rest (&whole 1 4 &lambda &body)) &body))
             (labels         (as flet))
             (macrolet       (as flet))
             (generic-flet   (as flet))
             (generic-labels (as flet))
             (handler-case (4 &rest (&whole 2 &lambda &body)))
             (restart-case (as handler-case))
             ;; single-else style (then and else equally indented)
             (if          (&rest nil))
             (if*         common-lisp-indent-if*)
             (lambda      (&lambda &rest lisp-indent-function-lambda-hack))
             (let         ((&whole 4 &rest (&whole 1 1 2)) &body))
             (let*         (as let))
             (compiler-let (as let))
             (handler-bind (as let))
             (restart-bind (as let))
             (locally 1)
             (loop           lisp-indent-loop)
             (:method        lisp-indent-defmethod) ; in `defgeneric'
             (multiple-value-bind ((&whole 6 &rest 1) 4 &body))
             (multiple-value-call (4 &body))
             (multiple-value-prog1 1)
             (multiple-value-setq (4 2))
             (multiple-value-setf (as multiple-value-setq))
             (named-lambda (4 &lambda &rest lisp-indent-function-lambda-hack))
             (pprint-logical-block (4 2))
             (print-unreadable-object ((&whole 4 1 &rest 1) &body))
             ;; Combines the worst features of BLOCK, LET and TAGBODY
             (prog        (&lambda &rest lisp-indent-tagbody))
             (prog* (as prog))
             (prog1 1)
             (prog2 2)
             (progn 0)
             (progv       (4 4 &body))
             (return 0)
             (return-from (nil &body))
             (symbol-macrolet (as let))
             (tagbody     lisp-indent-tagbody)
             (throw 1)
             (unless 1)
             (unwind-protect (5 &body))
             (when 1)
             (with-accessors          (as multiple-value-bind))
             (with-compilation-unit   ((&whole 4 &rest 1) &body))
             (with-condition-restarts (as multiple-value-bind))
             (with-output-to-string (4 2))
             (with-slots              (as multiple-value-bind))
             (with-standard-io-syntax (2)))))
    (dolist (el l)
      (let* ((name (car el))
             (spec (cdr el))
             (indentation
              (if (symbolp spec)
                  (error "Old style indirect indentation spec: %s" el)
                (when (cdr spec)
                  (error "Malformed indentation specification: %s" el))
                (car spec))))
        (unless (symbolp name)
          (error "Cannot set Common Lisp indentation of a non-symbol: %s"
                 name))
        (put name 'common-lisp-indent-function indentation)))))
(common-lisp-init-standard-indentation)

(defun common-lisp-indent-test (name bindings test)
  (with-temp-buffer
    (lisp-mode)
    (setq indent-tabs-mode nil)
    (common-lisp-set-style "common-lisp-indent-test")
    (dolist (bind bindings)
      (set (make-local-variable (car bind)) (cdr bind)))
    (insert test)
    (goto-char 0)
    ;; Find the first line with content.
    (skip-chars-forward " \t\n\r")
    ;; Mess up the indentation so we know reindentation works
    (save-excursion
      (while (not (eobp))
        (forward-line 1)
        (unless (looking-at "^$")
          (case (random 2)
            (0
             ;; Delete all leading whitespace -- except for comment lines.
             (while (and (looking-at " ") (not (looking-at " ;")))
               (delete-char 1)))
            (1
             ;; Insert whitespace random.
             (let ((n (1+ (random 24))))
               (while (> n 0) (decf n) (insert " "))))))))
    (let ((mess (buffer-string)))
      (when (equal mess test)
        (error "Could not mess up indentation?"))
      (indent-sexp)
      (if (equal (buffer-string) test)
          t
        ;; (let ((test-buffer (current-buffer)))
        ;;   (with-temp-buffer
        ;;     (insert test)
        ;;     (ediff-buffers (current-buffer) test-buffer)))
        (error "Bad indentation in test %s.\nMess: %s\nWanted: %s\nGot: %s"
               name
               mess
               test
               (buffer-string))))))

(defun common-lisp-run-indentation-tests (run)
  (define-common-lisp-style "common-lisp-indent-test"
    ;; Used to specify a few complex indentation specs for testing.
    (:inherit "basic")
    (:indentation
     (complex-indent.1 ((&whole 4 (&whole 1 1 1 1 (&whole 1 1) &rest 1)
                                &body) &body))
     (complex-indent.2 (4 (&whole 4 &rest 1) &body))
     (complex-indent.3 (4 &body))))
  (with-temp-buffer
    (insert-file "slime-cl-indent-test.txt")
    (goto-char 0)
    (let ((test-mark ";;; Test: ")
          (n 0)
          (test-to-run (or (eq t run) (format "%s" run))))
      (while (not (eobp))
        (if (looking-at test-mark)
            (let* ((name-start (progn (search-forward ": ") (point)))
                   (name-end (progn (end-of-line) (point)))
                   (test-name
                    (buffer-substring-no-properties name-start name-end))
                   (bindings nil))
              (forward-line 1)
              (while (looking-at ";")
                (when (looking-at ";; ")
                  (skip-chars-forward "; ")
                  (unless (eolp)
                    (let* ((var-start (point))
                           (val-start (progn (search-forward ": ") (point)))
                           (var
                            (intern (buffer-substring-no-properties
                                     var-start (- val-start 2))))
                           (val
                            (car (read-from-string
                                  (buffer-substring-no-properties
                                   val-start (progn (end-of-line) (point)))))))
                      (push (cons var val) bindings))))
                (forward-line 1))
              (let ((test-start (point)))
                (while (not (or (eobp) (looking-at test-mark)))
                  (forward-line 1))
                (when (or (eq t run) (equal test-to-run test-name))
                  (let ((test (buffer-substring-no-properties
                               test-start (point))))
                    (common-lisp-indent-test test-name bindings test)
                    (incf n)))))
          (forward-line 1)))
      (common-lisp-delete-style "common-lisp-indent-test")
      (message "%s tests OK." n))))

;;; Run all tests:
;;;   (common-lisp-run-indentation-tests t)
;;;
;;; Run specific test:
;;;   (common-lisp-run-indentation-tests 77)

;;; cl-indent.el ends here
