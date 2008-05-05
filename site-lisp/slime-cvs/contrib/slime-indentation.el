;;;; slime-indentation.el - cl-indent.el as a slime-contrib module

(defun slime-indentation-install ()
  (slime-eval-async '(swank:swank-require :swank-indentation)))

(defun slime-indentation-init ()
  (add-hook 'slime-connected-hook 'slime-indentation-install))

;; redefine this for cl-indent:method
(defun slime-handle-indentation-update (alist)
  "Update Lisp indent information for slime-indentation.el.

ALIST is a list of (SYMBOL-NAME . INDENT-SPEC) of proposed indentation
settings for `common-lisp-indent-function'. The appropriate property
is setup, unless the user already set one explicitly."
  (dolist (info alist)
    (let ((symbol (intern (car info)))
          (indent (cdr info)))
      (define-cl-indent (cons symbol
                              (etypecase indent
                                (number (list indent))
                                (cons (labels ((walk (indent)
                                                 (etypecase indent
                                                   ((or number null) indent)
                                                   (cons (cons (walk (car indent))
                                                               (walk (cdr indent))))
                                                   (string (intern (downcase indent))))))
                                        (list (walk indent))))
                                (string (intern (downcase indent))))))
      (run-hook-with-args 'slime-indentation-update-hooks symbol indent))))

;; $ITI: cl-indent.el,v 1.6 1995/09/10 14:13:34 schrod Exp $
;; ----------------------------------------------------------------------
;; Copyright (C) 1987, 1993 Free Software Foundation, Inc.
;; Written by Richard Mlynarik July 1987
;; Merged with cl-indent-patches.el by Marco Baringer (2007-11-14)
;; Documented and intensively modified by Joachim Schrod
;; <jschrod@acm.org>, history at end.
;; Send bug reports, gripes, patches to me.

;;
;; cl-indent.el  ---  highly configurable indentation for Lisp modes
;;

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;; ----------------------------------------------------------------------

;; USAGE:

;; This file delivers highly configurable indentation of Lisp code.
;; Eval (cl-indent) to use this indentation for a specific file,
;; (setq lisp-indent-function 'cl-indent:function) to use it for all
;; Lisp files.

;; The indentation for a specific form may be defined by
;; (define-cl-indent SPEC &optional MODE-METHODS). Indentation specs
;; for Common Lisp constructs are given already. Check the on-line
;; documentation of this function for more information.

;; Actually, the whole (`real') documentation of this source is stored
;; as the documentation strings of respective functions. Start with
;; #'define-cl-indent, you'll find references to all other relevant
;; functions.

;; It's also possible to specify specific indentations for a mode
;; (e.g., some special Lisp-mode) and even specific ones for a file.
;; File specific indentations are taken from the alist bound to
;; cl-indent:local-methods, you can set it in your `Local Variables'
;; section. Mode-specific methods are stored in hash tables, the mode
;; setup must bind cl-indent:mode-methods to the name of that hash
;; table.

;; You may want to override my global indentation specs. If you load
;; this file immediately, just issue some #'define-cl-indent calls. If
;; you use autoload, add an appropriate hook function to
;; 'cl-indent:load-hook.

;; I'm interested in feedback on this module. Do you use it, was it
;; useful to you? (Further development depends on the amount of people
;; who send feedback. :-)
;; Send email to <jschrod@acm.org>.



;; ------------------------------------------------------------

;;>> TODO

;; Urgently need better user documentation, it's hard to get a grasp
;; for the overall strategy how this package may be customized. One
;; has to look at too many function documentation strings.

;; Have to check if the usage of hash tables makes this package XEmacs
;; specific. If FSF Emacs doesn't have them, they might be emulated by
;; alists or obarrays. (I don't have FSF Emacs available, may somebody
;; please check this, maybe even send patches?)

;; Realize `parent method tables', to be able to inherit an indentation
;; method table. `(make-method-table &optional size parent)' ?! That's
;; needed before the CL specific indentation is moved to an own table,
;; as some modes (e.g., stil-mode) may inherit their indentation from
;; CL definitions.

;; Common Lisp specific indentation methods should be moved to a
;; method table, it's not good to have them globally for all kinds of
;; Lisp modes. How about a table for Elisp indentations as well?

;; special handling of keywords in forms, e.g.,
;;
;; :foo
;;   bar
;; :baz
;;   zap
;;
;; &key (like &body)??

;; &rest 1 in lambda-lists doesn't work, really want
;;
;;     (foo bar
;;      baz)
;;
;; not
;;
;;     (foo bar
;;          baz)
;;
;; Need something better than &rest for such cases. Perhaps a function
;; that just returns normal-point? Might work...


;;; ------------------------------------------------------------
;;;
;;; USER TOP-LEVEL FUNCTION
;;;

;;;###autoload
(defun cl-indent ()
  "Switch on Common Lisp indentation for the current buffer.
May also be used as hook function, e.g., in lisp-mode-hook.
If you want to do use this indentation for all Lisp buffers, incl.
Emacs Lisp code, simply eval
	(setq lisp-indent-function 'cl-indent:function)
You might want to do this in some setup file, e.g., in ~/.emacs ."
  (interactive)
  (make-local-variable 'lisp-indent-function)
  (setq lisp-indent-function 'cl-indent:function))



;;; ------------------------------------------------------------
;;;
;;; Configuration:
;;;

(defvar cl-indent::maximum-backtracking 3
  "Maximum depth to backtrack out from a sublist for structured indentation.
If this variable is 0, no backtracking will occur and forms such as flet
may not be correctly indented.")

(defvar cl-indent:tag-indentation 1
  "*Indentation of tags relative to containing list.
This variable is used by the function cl-indent:tagbody.")

(defvar cl-indent:tag-body-indentation 3
  "*Indentation of non-tagged lines relative to containing list.
This variable is used by the function cl-indent:tagbody to indent normal
lines (lines without tags).
The indentation is relative to the indentation of the parenthesis enclosing
he special form.  If the value is t, the body of tags will be indented
as a block at the same indentation as the first s-expression following
the tag.  In this case, any forms before the first tag are indented
by lisp-body-indent.")



;;; ============================================================
;;;
;;; compute the indentation of the current line
;;;


;;;###autoload
(defun common-lisp-indent-function (indent-point state)
  "Old name of #'cl-indent:function."
  (cl-indent:function indent-point state))

(make-obsolete #'common-lisp-indent-function #'cl-indent:function)


;;;###autoload
(defun cl-indent:function (indent-point state)
  "Compute the indentation of the current line of Common Lisp code.
INDENT-POINT is the current point. STATE is the result of a
#'parse-partial-sexp from the start of the current function to the
start of the line this function was called.

The indentation is determined by the expressions point is in.

When this function is called, the column of point may be used as the
normal indentation. Therefore we call this position _normal
point_. Actually, if the first element of the current expression is a
list, it's at the start of this element. Otherwise it's at the start
of first expression on the same line as the last complete expression.

Within a quoted list or a non-form list, all subsequent lines are
indented to the column directly after the opening parenthesis. Quoted
lists are those that are prefixed with ?\`, ?\', or ?\#. Note that the
quote must be immediately in front of the opening parenthesis. I.e.,
if you want to use automatic code indentation in a macro expansion
formulated with a backquoted list, add a blank between the backquote
and the expansion form.

Within a list form, the indentation is determined by the indentation
method associated to the form symbol. (See #'cl-indent::method.)

** If the indentation method is nil, the form is assumed to be a
function call, arguments are aligned beneath each other if the first
argument was written behind the function symbol, otherwise they're
aligned below the function symbol.

** If the indentation method is a symbol, a function must be bound to
that symbol that will compute the current indentation. Such a function
is named an _indentation function_ and is called with 5 arguments:

 (1) PATH is a list of numbers, the path from the top-level form to
     the current structural element (the first element is number 0).
     E.g., `foo' has a path of (0 3 1) in `((a b c (d foo) f) g)'.
 (2) STATE is passed.
 (3) INDENT-POINT is passed.
 (4) SEXP-COLUMN is the column where the innermost form starts.
 (5) NORMAL-INDENT is the column of normal point.

** If the indentation method is a list, this list specifies the form
structure and the indentation of each substructure. The possible list
structure and elements are described in #'cl-indent::form-method.

** If the indentation method is the number $n$, the first $n$
arguments are _distinguished arguments_; they are indented by 4
spaces. Further arguments are indented by lisp-body-indent. That's
roughly equivalent to '(4 4 ... &body)' with $n$ 4s.

** Furthermore values as described for #'lisp-indent-function may be
used for upward compatibility."
  (let ((normal-indent (current-column)))
    ;; Walk up list levels until we see something
    ;;  which does special things with subforms.
    (let ((depth 0)
          ;; Path describes the position of point in terms of
          ;;  list-structure with respect to contining lists.
          ;; `foo' has a path of (0 4 1) in `((a b c (d foo) f) g)'
          (path ())
          ;; set non-nil when somebody works out the indentation to use
          calculated
          (last-point indent-point)
          ;; the position of the open-paren of the innermost containing list
          (containing-form-start (elt state 1))
          ;; the column of the above
          sexp-column)
      ;; Move to start of innermost containing list
      (goto-char containing-form-start)
      (setq sexp-column (current-column))
      ;; Look over successively less-deep containing forms
      (while (and (not calculated)
                  (< depth cl-indent::maximum-backtracking))
        (let ((containing-sexp (point)))
          (forward-char 1)
          (parse-partial-sexp (point) indent-point 1 t)
          ;; Move to the car of the relevant containing form
          (let (tem function method)
            (if (not (looking-at "\\sw\\|\\s_"))
                ;; This form doesn't seem to start with a symbol
                (setq function nil method nil)
              (setq tem (point))
              (forward-sexp 1)
              (setq function (downcase (buffer-substring tem (point))))
              (goto-char tem)
              (setq tem (intern-soft function)
                    method (get tem 'cl-indent:method))
              (cond ((and (null method)
                          (string-match ":[^:]+" function))
                     ;; The pleblisp package feature
                     (setq function (substring function
                                               (1+ (match-beginning 0)))
                           method (get (intern-soft function)
                                       'cl-indent:method)))
                    ((and (null method))
                     ;; backwards compatibility
                     (setq method (get tem 'lisp-indent-function)))))
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

            ;; backwards compatibility.
            (cond ((null function))
                  ((null method)
                   (if (null (cdr path))
                       ;; (package prefix was stripped off above)
                       (setq method (cond ((string-match "\\`def"
                                                         function)
                                           '(4 (&whole 4 &rest 1) &body))
                                          ((string-match "\\`\\(with\\|do\\)-"
                                                         function)
                                           '(4 &body))))))
                  ;; backwards compatibility.  Bletch.
                  ((eq method 'defun)
                   (setq method '(4 (&whole 4 &rest 1) &body))))

            (cond ((and (eql (char-after (1- containing-sexp)) ?\') ; patched to only do this for ' and not `.  
                        (not (eql (char-after (- containing-sexp 2)) ?\#)))
                   ;; No indentation for "'(...)" elements
                   (setq calculated (1+ sexp-column)))
                  ((eql (char-after (1- containing-sexp)) ?\#)
                   ;; "#(...)"
                   (setq calculated (1+ sexp-column)))
                  ((null method))
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
                  ((symbolp method)
                   (setq calculated (funcall method
                                             path state indent-point
                                             sexp-column normal-indent)))
                  (t
                   (setq calculated (cl-indent::form-method
                                      method path state indent-point
                                      sexp-column normal-indent)))))

          (goto-char containing-sexp)
          (setq last-point containing-sexp)
          (if (not calculated)
              (condition-case ()
                   (progn (backward-up-list 1)
                          (setq depth (1+ depth)))
                (error (setq depth cl-indent::maximum-backtracking))))))
      calculated)))

(defun cl-indent::normal (state)
  "Compute normal indentation according to STATE and current position."
  ;; Actually, the current column (i.e., the normal point) _is_ a good
  ;; approximation for the normal indentation. But lists with a list
  ;; as the first element make problems if an &rest or an &body method
  ;; is in effect.
  ;;
  ;; There we can distinguish two cases:
  ;;
  ;;  1. ((foo) (bar)
  ;;		(baz))
  ;;  2. ((foo)
  ;;        (bar)
  ;;        (baz))
  ;;
  ;; Both are used in do result-forms, or in cond-forms. If
  ;; #'cl-indent:function is called in the baz line, the normal point
  ;; will be at (foo), i.e., (baz) would be aligned below (foo). (Of
  ;; course, if the body indentation is 1, both (bar) and (baz) are
  ;; aligned below (foo).) But I want to enable the specification of
  ;; alignments like those shown above -- if the user did change the
  ;; alignment for the first expression of a body then it should be
  ;; used further on, after all. (As usually, we have to assume that
  ;; the user knows what he does.)
  (let ((normal-point (point))
	(current-sexp (elt state 1)))
    ;; A necessary precondition for the special situation outlined
    ;; above is that the normal point is directly after the start of
    ;; the current expression and that a list is there. Only then we
    ;; have to calculate the normal indentation, otherwise we can use
    ;; the column of normal point.
    (if (and (= (1+ current-sexp) normal-point)
	     (looking-at "\\s("))
	;; OK. Let's determine first the first expression in the line
	;; with the last completed expression before the indentation point.
	(let ((last-sexp (elt state 2)))
	  (goto-char last-sexp)
	  (beginning-of-line)
	  (parse-partial-sexp (point) last-sexp 0 t)
	  (backward-prefix-chars)
	  ;; If we're now after the current expression, we're in case
	  ;; 2. We simply use the current column then.
	  (if (> (point) current-sexp)
	      (current-column)
	    ;; Here we have to care for case 1: We determine the
	    ;; second element of the list and use its column.
	    (goto-char normal-point)	; start of the first element!
	    (forward-sexp 1)
	    (parse-partial-sexp (point) last-sexp 0 t)
	    (current-column)))
      (current-column))))

(defun cl-indent::bad-method (m)
  (error "%s has a badly-formed indentation method: %s"
         ;; Love them free variable references!!
         function m))

;; Blame the crufty control structure on dynamic scoping
;;  -- not on me!
(defun cl-indent::form-method (method path state indent-point
			       sexp-column normal-indent)
  "Compute the current indentation according to METHOD.
The other arguments are those of an indentation function, see
#'cl-indent:function for further explanation.

METHOD is a list that specifies the indentation of a form:

    method-list-spec : '(' method-list ')'

    method-list	: method *  method-finish ?

    method	: indent-spec
		| method-sublist
			<< the subform must be a list that's indented
			   as specified >>

    indent-spec	: Number | Symbol | 'nil'
			<< indent this subform $Number spaces or compute its
			   indentation by the indentation function bound to
			   Symbol. 'nil' tells to use normal function
			   indentation. >>

    method-finish : '&rest' method
			<< indent the rest of this form as specified by
			   method. >>
		| '&body'
			<< equivalent to `(&rest ,lisp-body-indent).
			   I.e., Indent all following forms by
			   lisp-body-indent spaces. >>

    method-sublist : '(' '&whole' indent-spec method-list ')'
			<< This whole subform has a basic indentation, as
			   specified by indent-spec. The indentations from
			   method-list are added to this basic indentation. >>

FIXME (-js): Maybe only list structures up to a depth of
'cl-indent::maximum-backtracking are supported. Have to analyze the
code for this. If that's the case this variable should be a constant.
"
  (catch 'exit
    (let ((p path)
          (containing-form-start (elt state 1))
          n tem)
      ;; Isn't tail-recursion wonderful?
      (while p
        ;; This while loop is for destructuring.
        ;; p is set to (cdr p) each iteration.
        (if (not (consp method)) (cl-indent::bad-method method))
        (setq n (1- (car p))		; FIXME: that might result in -1 !?
              p (cdr p))
        (while n
          ;; This while loop is for advancing along a method
          ;; until the relevant (possibly &rest/&body) pattern
          ;; is reached.
          ;; n is set to (1- n) and method to (cdr method)
          ;; each iteration.
; (message "trying %s for %s %s" method p function) (sit-for 1)
          (setq tem (car method))
	  (cl-indent::check-method tem method)

          (cond ((eq tem '&body)
                 ;; &body means (&rest <lisp-body-indent>)
                 (throw 'exit
                        (if (null p)
                            (+ sexp-column lisp-body-indent)
                            normal-indent)))
                ((eq tem '&rest)
                 ;; this pattern holds for all remaining forms
                 (setf method (list (second method))
                       n 0))
                ((> n 0)
                 ;; try next element of pattern
                 (setq n (1- n)
                       method (cdr method))
                 (if (< n 0)
                     ;; Too few elements in pattern.
                     (throw 'exit normal-indent)))
                ((eq tem 'nil)
                 (throw 'exit (list normal-indent containing-form-start)))
;               ((eq tem '&lambda)
;                ;; abbrev for (&whole 4 &rest 1)
;                (throw 'exit
;                  (cond ((null p)
;                         (list (+ sexp-column 4) containing-form-start))
;                        ((null (cdr p))
;                         (+ sexp-column 1))
;                        (t normal-indent))))
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
                 (if (not (null p))
                     ;; descend
                     (setq method (cdr (cdr tem))
                           n nil)
                   (setq tem (car (cdr tem)))
                   (throw 'exit
                     (cond ((eq tem 'nil)
                            (list normal-indent
                                  containing-form-start))
                           ((integerp tem)
                            (list (+ sexp-column tem)
                                  containing-form-start))
                           (t
                            (funcall tem path state indent-point
                                     sexp-column normal-indent))))))))))))

(defun cl-indent::check-method (tem method)
  "Check validity of one indentation method element.
TEM is that indentation method and METHOD is the rest of the method list."
  (if (eq tem 'nil)
      ;; default indentation
      t
    (if (and (eq tem '&body)
             (null (cdr method)))
        t
      (if (and (eq tem '&rest)
               (consp (cdr method))
               (null (cdr (cdr method))))
          t
        (if (integerp tem)
            t
          (if (and (consp tem)			; destructuring
                   (eq (car tem) '&whole)
                   (or (symbolp (car (cdr tem)))
                       (integerp (car (cdr tem)))))
              t
            (if (and (symbolp tem)		; a function to call to do the work.
                     (null (cdr method)))
                t
              (cl-indent::bad-method method))))))))



;;; ------------------------------------------------------------
;;;
;;; A few indentation functions
;;;

(defun cl-indent:indent-tagbody (path state indent-point
				 sexp-column normal-indent)
  (if (not (null (cdr path)))
      normal-indent
    (save-excursion
      (goto-char indent-point)
      (beginning-of-line)
      (skip-chars-forward " \t")
      (list (cond ((looking-at "\\sw\\|\\s_")
                   ;; a tagbody tag
                   (+ sexp-column cl-indent:tag-indentation))
                  ((integerp cl-indent:tag-body-indentation)
                   (+ sexp-column cl-indent:tag-body-indentation))
                  ((eq cl-indent:tag-body-indentation 't)
                   (condition-case ()
                       (progn (backward-sexp 1) (current-column))
                     (error (1+ sexp-column))))
                  (t (+ sexp-column lisp-body-indent)))
;            (cond ((integerp cl-indent:tag-body-indentation)
;                   (+ sexp-column cl-indent:tag-body-indentation))
;                  ((eq cl-indent:tag-body-indentation 't)
;                   normal-indent)
;                  (t
;                   (+ sexp-column lisp-body-indent)))
            (elt state 1)
            ))))

(defun cl-indent:indent-do (path state indent-point
			    sexp-column normal-indent)
  (let ((cl-indent:tag-body-indentation lisp-body-indent))
    (funcall #'cl-indent:indent-tagbody
	     path state indent-point sexp-column normal-indent)))

(defun cl-indent:indent-function-lambda-hack (path state indent-point
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

(defun cl-indent:indent-defmethod (path state indent-point
				   sexp-column normal-indent)
  ;; Look for a method combination specifier...
  (let* ((combined (if (and (>= (car path) 3)
                            (null (cdr path)))
                       (save-excursion
                         (goto-char (second state))
                         (forward-char)
                         (forward-sexp)
                         (forward-sexp)
                         (forward-sexp)
                         (backward-sexp)
                         (if (looking-at ":")
                             t
                             nil))
                       nil))
	 (method (if combined
		     '(4 4 (&whole 4 &rest 1) &body)
		     '(4 (&whole 4 &rest 1) &body))))
    (funcall #'cl-indent::form-method
	     method
	     path state indent-point sexp-column normal-indent)))

(defun cl-indent:indent-defgeneric (path state indent-point
                                    sexp-column normal-indent)
  (let ((method '(4 4 &rest 2)))
    (when (= 2 (length path))
      (ignore-errors
        (save-excursion
          (save-match-data
            (let ((case-fold-search t))
              (goto-char (second state))
              (down-list)
              (skip-chars-forward " \t\n")
              (when (looking-at ":method\\W")
                (forward-sexp)
                (forward-sexp)
                (backward-sexp)
                (setq method (if (looking-at ":")
                                 '(4 (&whole 4 &rest 1) &body)
                                 '((&whole 4 &rest 1) &body))
                      path (cdr path)))
              (when (looking-at ":method-combination\\W")
                (setq method '(4 &rest 2)
                      path (cdr path))))))))
    (funcall #'cl-indent::form-method
             method
             path state indent-point sexp-column normal-indent)))

(defun cl-indent::line-number ()
  "Compatability implementation of  emacs23's line-number-at-pos."
  (cond 
    ((fboundp 'line-number-at-pos)
     (line-number-at-pos))
    ((fboundp 'line-number)
     (line-number nil t))
    ((fboundp 'count-lines)
     (count-lines (point-min) (point)))
    (t
     (error "Don't know how to count the number of lines from the start of the (narrowed) buffer to point."))))

(defun cl-indent:indent-cond (path state indent-point sexp-column normal-indent)
  "Handle indentation of cond.

Cond is either (&rest (&whole 2 1 &rest 1)) or (&rest (&whole 6 1
&rest 1)) depending on whether the first caluse is or isn't on
the same line as the cond symbol.

So if we have:

  (cond (a b)
        |

we line up the clauses after the cond symbol (6 space of
indentation). wherease if we have:

  (cond
    (a b)
    |)

we line up the clauses two space past the form's indentation."
  ;; i'd bet my left pinky there's a better way to implement this...
  (let (cond-line-number first-clause-line-number method here)
    (save-excursion
      ;; narrow to the aera we're interested in because
      ;; cl-indent::line-number can, especially on tramp files, be
      ;; very slow.
      (save-restriction
        (narrow-to-region (save-excursion
                            (backward-up-list)
                            (point))
                          (point))
        (setf here (point))
        (backward-up-list)
        (setf cond-line-number (cl-indent::line-number))
        (down-list)
        (forward-sexp 1)
        (setf first-clause-line-number 
              (progn
                (if (= 1 (first path))
                    ;; we're indenting the first form. use the current line.
                    (goto-char here)
                  ;; we're indenting some form which isn't the
                  ;; first. find out which the line the first clause
                  ;; starts on.
                  (forward-sexp 1)
                  (backward-sexp 1))
                (cl-indent::line-number)))))
    (cl-indent::form-method 
     (if (= cond-line-number first-clause-line-number)
         '(&rest (&whole 6 &rest 1))
         '(&rest (&whole 2 &rest 1)))
     path state indent-point sexp-column normal-indent)))


;;; ============================================================
;;;
;;; Define and retrieve indentation method
;;;

(defun cl-indent::method (function)
  "Returns the indentation method associated to FUNCTION (a string).
The indentation method is looked for subsequently as follows:

 (1) An indentation method is searched by #'cl-indent::get-method.

 (2) If FUNCTION is from a specific package, the package prefix is
     discarded and the indentation method from that FUNCTION name is
     used.

 (4) If FUNCTION starts with 'def', the indentation method \"defun\" is used.

 (5) If FUNCTION starts with 'while-' or 'do-', the indentation method
     1 (i.e., one distinguished argument) is used.

If the method determined that way is a string, it's replaced by the
current indentation method of the symbol named by that string."
  (let ((method
	 (cond ((cl-indent::get-method function))
	       ((string-match ":[^:]+" function)
		  (cl-indent::method (substring function
						(1+ (match-beginning 0)))))
	       ((string-match "\\`def" function) "defun")
	       ((string-match "\\`\\(with\\|do\\)-" function) 1))))
    (if (stringp method)
	(cl-indent::method method)
      method)))


(defvar cl-indent:local-methods nil
  "*Alist of source-local indentation methods.
Is typically set in a `Local Variables' section.")
(make-variable-buffer-local 'cl-indent:local-methods)

(defvar cl-indent:mode-methods nil
  "*Name of hash table with indentation methods for the current buffer.
Is typically set for a mode, during mode setup or in a mode hook.")
(make-variable-buffer-local 'cl-indent:mode-methods)


(defun cl-indent::get-method (function)
  "Retrieves an indentation method that is stored for FUNCTION (a string).

 (1) Indentation methods may be specified for the current source file,
     as an alist that's bound to cl-indent:local-methods . The alist
     car is the function symbol, the cdr is the indentation method.

 (2) Mode-specific indentation methods are stored in a hash table. The
     name of that hash table is bound to cl-indent:mode-methods .

 (3) Global indentation methods are stored as the value of the
     property 'cl-indent:method. If there is no such property, the
     property 'lisp-indent-function is checked, too, for compatibility."
  (let ((symbol (intern-soft function)))
    (or (cdr (assq symbol cl-indent:local-methods))
	;; An error will be signaled if the value of
	;; cl-indent:mode-methods is not a symbol naming an hash
	;; table. That's fine with me, other packages shouldn't mess
	;; around with my public names...
	(and cl-indent:mode-methods
	     (gethash symbol (symbol-value cl-indent:mode-methods)))
	(get symbol 'cl-indent:method)
	(get symbol 'lisp-indent-function))))


;;;
;;; Try to indent cl:loop
;;;

(defun cl-indent-parse-state-depth (parse-state)
  (car parse-state))

(defun cl-indent-parse-state-start (parse-state)
  (car (cdr parse-state)))

(defun cl-indent-parse-state-prev (parse-state)
  (car (cdr (cdr parse-state))))

;; Regexps matching various varieties of loop macro keyword ...
(defvar cl-indent-body-introducing-loop-macro-keyword
  "do\\|finally\\|initially\\|doing\\|collect\\|collecting\\|append\\|appending"
  "Regexp matching loop macro keywords which introduce body-forms")

;; This is so "and when" and "else when" get handled right
;; (not to mention "else do" !!!)
(defvar cl-indent-prefix-loop-macro-keyword
  "and\\|else"
  "Regexp matching loop macro keywords which are prefixes")

(defvar cl-indent-clause-joining-loop-macro-keyword
  "and"
  "Regexp matching 'and', and anything else there ever comes to be
like it ...")

;; This is handled right, but it's incomplete ...
;; (It could probably get arbitrarily long if I did *every* iteration-path)
(defvar cl-indent-indented-loop-macro-keyword
  "into\\|by\\|upto\\|downto\\|above\\|below\\|on\\|in\\|being\\|=\\|first\\|then\\|from\\|to"
  "Regexp matching keywords introducing loop subclauses.  Always indented two")

(defvar cl-indent-indenting-loop-macro-keyword
  "when\\|unless\\|if"
  "Regexp matching keywords introducing conditional clauses.
Cause subsequent clauses to be indented")

(defvar cl-indent-loop-macro-else-keyword "else")

;;; Attempt to indent the loop macro ...

(defun cl-indent::indent-loop-macro
    (path parse-state indent-point sexp-column normal-indent)
  (list (cl-indent-indent-loop-macro-1 parse-state indent-point)
	(cl-indent-parse-state-start parse-state)))

(defun cl-indent-indent-loop-macro-1 (parse-state indent-point)
  (catch 'return-indentation
    (save-excursion
      ;; Find first clause of loop macro, and use it to establish
      ;; base column for indentation
      (goto-char (cl-indent-parse-state-start parse-state))
      (let ((loop-start-column (current-column)))
	(cl-indent-loop-advance-past-keyword-on-line)
	(if (eolp)
	    (progn
	      (forward-line 1)
	      (end-of-line)
	      
	      ;; If indenting first line after "(loop <newline>"
	      ;; cop out ...
	      
	      (if (<= indent-point (point))
		  (throw 'return-indentation (+ 2 loop-start-column)))
	      (back-to-indentation)))
	
	(let* ((case-fold-search t)
	       (loop-macro-first-clause (point))
	       (previous-expression-start (cl-indent-parse-state-prev parse-state))
	       (default-value (current-column))
	       (loop-body-p nil)
	       (loop-body-indentation nil)
	       (indented-clause-indentation (+ 2 default-value)))
	  ;; Determine context of this loop clause, starting with the
	  ;; expression immediately preceding the line we're trying to indent
	  (goto-char previous-expression-start)
	  ;; Handle a body-introducing-clause which ends a line specially.
	  (if (looking-at cl-indent-body-introducing-loop-macro-keyword)
	      (let ((keyword-position (current-column)))
		(setq loop-body-p t)
		(setq loop-body-indentation
		      (if (cl-indent-loop-advance-past-keyword-on-line)
			  (current-column)
			(back-to-indentation)
			(if (/= (current-column) keyword-position)
			    (+ 2 (current-column))
			  (- keyword-position 3)))))
	      
	    (back-to-indentation)
	    (if (< (point) loop-macro-first-clause)
		(goto-char loop-macro-first-clause))
	    ;; If there's an "and" or "else," advance over it.
	    ;; If it is alone on the line, the next "cond" will treat it
	    ;; as if there were a "when" and indent under it ...
	    (let ((exit nil))
	      (while (and (null exit)
			  (looking-at cl-indent-prefix-loop-macro-keyword))
		(if (null (cl-indent-loop-advance-past-keyword-on-line))
		    (progn (setq exit t)
			   (back-to-indentation)))))
	    ;; Found start of loop clause preceding the one we're trying to indent.
	    ;; Glean context ...
	    (cond
	      ((looking-at "(")
	       ;; We're in the middle of a clause body ...
	       (setq loop-body-p t)
	       (setq loop-body-indentation (current-column)))
	      ((looking-at cl-indent-body-introducing-loop-macro-keyword)
	       (setq loop-body-p t)
	       ;; Know there's something else on the line (or would
	       ;; have been caught above)
	       (cl-indent-loop-advance-past-keyword-on-line)
	       (setq loop-body-indentation (current-column)))
	      (t
	       (setq loop-body-p nil)
	       (if (or (looking-at cl-indent-indenting-loop-macro-keyword)
		       (looking-at cl-indent-prefix-loop-macro-keyword))
		   (setq default-value (+ 2 (current-column))))
	       (setq indented-clause-indentation (+ 2 (current-column)))
	       ;; We still need loop-body-indentation for "syntax errors" ...
	       (goto-char previous-expression-start)
	       (setq loop-body-indentation (current-column)))))
      
	;; Go to first non-blank character of the line we're trying to indent.
	;; (if none, wind up poised on the new-line ...)
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
	     default-value))
	  ((looking-at cl-indent-indented-loop-macro-keyword)
	   indented-clause-indentation)
	  ((looking-at cl-indent-clause-joining-loop-macro-keyword)
	   (let ((stolen-indent-column nil))
	     (forward-line -1)
	     (while (and (null stolen-indent-column)
			 (> (point) loop-macro-first-clause))
	       (back-to-indentation)
	       (if (and (< (current-column) loop-body-indentation)
			(looking-at "\\sw"))
		   (progn
		     (if (looking-at cl-indent-loop-macro-else-keyword)
			 (cl-indent-loop-advance-past-keyword-on-line))
		     (setq stolen-indent-column
			   (current-column)))
		 (forward-line -1)))
	     (if stolen-indent-column
		 stolen-indent-column
	       default-value)))
	  (t default-value)))))))

(defun cl-indent-loop-advance-past-keyword-on-line ()
  (forward-word 1)
  (block move-forward
    (while (and (looking-at "\\s-") (not (eolp)))
      (forward-char 1)
      (when (looking-at "\\s<")
        ;; eat up the comment (sorry, this will fail for for lisp block comments
        (while (and (not (looking-at "\\s>")) (not (eolp)))
          (forward-char 1)))))
  (if (eolp)
      nil
    (current-column)))

;;;###autoload
(defun define-cl-indent (spec &optional mode-methods)
  "Define the cl-indent specification SPEC, maybe mode-specific.
The car of SPEC is the symbol for which the indentation shall be specified.
    If the cdr is a symbol, then this symbol shall be indented like
the other symbol is indented _currently_ (i.e., eager evaluation is
used, not lazy evaluation).
    Otherwise the cadr is taken as the indentation method. Check
#'cl-indent:function for documentation about indentation methods. Note
further that #'cl-indent::method interprets indentation methods that
are strings as aliases, i.e., the indentation method of the string is
looked up and returned (lazy evaluation).
    The optional argument MODE-METHODS may be bound to a hash table
where this (presumedly mode-specific) indentation method shall be
stored."
  (let* ((symbol (car spec))
	 (indent (cdr spec))
	 (method (if (symbolp indent)
		     ;; If an alias is defined, it might be mode-specific.
		     ;; Rebind cl-indent:mode-methods for lookup,
		     ;; that's possible as all symbols have dynamic
		     ;; scope in Emacs Lisp.
		     (let ((cl-indent:mode-methods (and mode-methods
							'mode-methods)))
		       (cl-indent::method (symbol-name indent)))
		   (car indent))))
    (if mode-methods
	(puthash symbol method mode-methods)
      (put symbol 'cl-indent:method method))))



;;; ------------------------------------------------------------
;;;
;;; issue specifications for Common Lisp forms
;;;

(mapcar #'define-cl-indent
	'((block 1)
	  (case		(4 &rest (&whole 2 &rest 3)))
	  (ccase . case) (ecase . case)
	  (typecase . case) (etypecase . case) (ctypecase . case)
	  (handler-bind . let)
	  (handler-case (4 &rest (&whole 2 4 &rest 2)))
	  (catch 1)
	  (cond		cl-indent:indent-cond)
	  (defvar	(4 2 2))
	  (defconstant . defvar) (defparameter . defvar)
	  (defclass	(6 6 (&whole 2 &rest 1) &rest 2))
	  (define-modify-macro
			(4 &body))
	  (defsetf	(4 (&whole 4 &rest 1) 4 &body))
	  (defun	(4 (&whole 4 &rest 1) &body))
	  (defmacro . defun) (define-setf-method . defun) (deftype . defun)
          (defgeneric   cl-indent:indent-defgeneric)
	  (defmethod	cl-indent:indent-defmethod)
	  (defstruct	((&whole 4 &rest (&whole 2 &rest 1))
			 &rest (&whole 2 &rest 1)))
	  (destructuring-bind
			((&whole 6 &rest 1) 4 &body))
	  (do		((&whole 4 &rest (&whole 1 &rest 2)) ; ((arg step incr))
			 (&whole 4 &rest 3) ; result: ((condition) (form) ...)
			 &rest cl-indent:indent-do))
	  (do* . do)
          (do-all-symbols (4 &body))
          (do-symbols (4 &body))
          (do-external-symbols (4 &body))
	  (dolist	((&whole 4 2 1) &body))
	  (dotimes . dolist)
	  (eval-when 1)
	  (flet		((&whole 4 &rest (&whole 1 (&whole 4 &rest 1) &body))
			 &body))
	  (labels . flet) (macrolet . flet)
	  (if		(&rest 2))
	  ;; FIXME: Which of those do I really want?
	  ;; (lambda	((&whole 4 &rest 1) &body))
	  (lambda	((&whole 4 &rest 1)
			 &rest cl-indent:indent-function-lambda-hack))
	  (let		((&whole 4 &rest (&whole 1 1 2)) &body))
	  (let* . let) (compiler-let . let)
	  (locally 1)
	  (loop cl-indent::indent-loop-macro)
	  (multiple-value-bind
			((&whole 6 &rest 1) 4 &body))
	  (multiple-value-call
			(4 &body))
	  (multiple-value-list 1)
	  (multiple-value-prog1 1)
	  (multiple-value-setq
			(4 2))
	  (print-unreadable-object 1)
	  ;; Combines the worst features of BLOCK, LET and TAGBODY
	  (prog		((&whole 4 &rest 1) &rest cl-indent:indent-tagbody))
	  (prog* . prog)
	  (prog1 1)
	  (prog2 2)
	  (progn 0)
	  (progv	(4 4 &body))
          (restart-case . handler-case)
	  (return 0)
	  (return-from	(nil &body))
	  (tagbody	cl-indent:indent-tagbody)
	  (throw 1)
	  (unless 1)
	  (unwind-protect
			(5 &body))
	  (values 0)
	  (when 1)
          (with-accessors (6 4 &body))
          (with-compilation-unit (4 &body))
          (with-hash-table-iterator (4 &body))
          (with-output-to-string (4 &body))
          (with-input-from-string . with-output-to-string)
          (with-open-file (4 &body))
          (with-open-stream . with-open-file)
          (with-package-iterator (4 &body))
          (with-simple-restart (4 &body))
          (with-slots (6 4 &body))))

;; OK, we're almost finished.
;;
;; Allow load-time configuration, e.g., redefinition of some global
;; method above.

(run-hooks 'cl-indent:load-hook)

(provide 'slime-indentation)


;;; ======================================================================
;;
;; $ITIlog: cl-indent.el,v $
;; Revision 1.6  1995/09/10  14:13:34  schrod
;;     Add aliassing of indentation methods.
;;     Discard unused variables. Quiet down the byte-compiler. Discard
;; duplicate indentation specs.
;;     `Define-as' specs in #'define-cl-indent uses the mode-specific
;; method table for lookup of the reference symbol's indentation method,
;; if a table was supplied.
;;
;; Revision 1.5  1995/08/14  16:49:05  schrod
;;     Provide 'cl-indent, this module may not be required otherwise.
;;
;; Revision 1.4  1995/07/24  18:16:50  schrod
;;     Did not work due to spurious closing brace.
;;
;; Revision 1.3  1995/01/17  11:13:25  schrod
;;     Add support for mode-specific and local indentation methods. Don't
;; need STIL indentation support any more, that's an own mode now.
;;     Provide a load hook to be able to adapt global indentation methods
;; to personal preferences.
;;     All form symbols are finally checked for global indentation
;; methods bound to 'lisp-indent-function, for upward compatibility to
;; standard Lisp indentation.
;;     Renamed all symbols to start with `cl-indent:'. Private symbols
;; use `::', similar to CL. #'define-cl-indent is an exception, as usual.
;; I hope that the new names are more meaningful, too.
;;     Added some pointers to function documentation to the usage
;; comments at the start. Mentioned additional future projects.
;;
;; Revision 1.2  1994/09/05  17:35:47  schrod
;;     Added documentation to every function.
;;     Added #'cl-indent and #'define-cl-indent as user-level functions.
;;     Renamed all functions from lisp-indent-* to cl-indent-* to avoid
;; name clashes with `normal' lisp-mode indentation. In particular,
;; rename #'lisp-indent-259 (what a ridiculous name to use for a
;; function!) to #'cl-indent-by-method.
;;     Introduced #'cl-indent-normal to compute the normal (default)
;; indentation, since #'current-column does not always deliver the
;; correct result.
;;     Introduced #'get-cl-indent-method to encapsulate the storage of an
;; indentation method. Might want to change this later anyhow, to support
;; mode- and file-specific indentation.
;;     Check for a correct method is in an own function now,
;; #'cl-indent-by-method was large enough already.
;;     #'lisp-indent-do is never called for the first two elements in a
;; path, this test (and the else form) could be discarded.
;;     Add support for more CL constructs (CLOS, CLCS, condition stuff
;;     Define STIL constructs, this should be discarded with the
;; introduction of mode-specific indentation methods.
;;
