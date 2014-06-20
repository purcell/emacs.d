
(define-slime-contrib slime-parse
  "Utility contrib containg functions to parse forms in a buffer."
  (:authors "Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>"
            "Tobias C. Rittweiler <tcr@freebits.de>")
  (:license "GPL"))

(defun slime-parse-form-until (limit form-suffix)
  "Parses form from point to `limit'."
  ;; For performance reasons, this function does not use recursion.
  (let ((todo (list (point))) ; stack of positions
        (sexps)               ; stack of expressions
        (cursexp)
        (curpos)
        (depth 1))            ; This function must be called from the
                              ; start of the sexp to be parsed.
    (while (and (setq curpos (pop todo))
                (progn
                  (goto-char curpos)
                  ;; (Here we also move over suppressed
                  ;; reader-conditionalized code! Important so CL-side
                  ;; of autodoc won't see that garbage.)
                  (ignore-errors (slime-forward-cruft))
                  (< (point) limit)))
      (setq cursexp (pop sexps))
      (cond
        ;; End of an sexp?
        ((or (looking-at "\\s)") (eolp))
         (decf depth)
         (push (nreverse cursexp) (car sexps)))
        ;; Start of a new sexp?
        ((looking-at "\\s'*\\s(")
         (let ((subpt (match-end 0)))
           (ignore-errors
             (forward-sexp)
             ;; (In case of error, we're at an incomplete sexp, and
             ;; nothing's left todo after it.)
             (push (point) todo))
           (push cursexp sexps)
           (push subpt todo)            ; to descend into new sexp
           (push nil sexps)
           (incf depth)))
        ;; In mid of an sexp..
        (t
         (let ((pt1 (point))
               (pt2 (condition-case e
                        (progn (forward-sexp) (point))
                      (scan-error
                       (fourth e)))))   ; end of sexp
           (push (buffer-substring-no-properties pt1 pt2) cursexp)
           (push pt2 todo)
           (push cursexp sexps)))))
    (when sexps
      (setf (car sexps) (nreconc form-suffix (car sexps)))
      (while (> depth 1)
        (push (nreverse (pop sexps)) (car sexps))
        (decf depth))
      (nreverse (car sexps)))))

(defun slime-compare-char-syntax (get-char-fn syntax &optional unescaped)
  "Returns t if the character that `get-char-fn' yields has
characer syntax of `syntax'. If `unescaped' is true, it's ensured
that the character is not escaped."
  (let ((char        (funcall get-char-fn (point)))
	(char-before (funcall get-char-fn (1- (point)))))
    (if (and char (eq (char-syntax char) (aref syntax 0)))
	(if unescaped
	    (or (null char-before)
		(not (eq (char-syntax char-before) ?\\)))
	    t)
        nil)))

(defconst slime-cursor-marker 'swank::%cursor-marker%)

(defun slime-parse-form-upto-point (&optional max-levels)
  (save-restriction
    ;; Don't parse more than 500 lines before point, so we don't spend
    ;; too much time. NB. Make sure to go to beginning of line, and
    ;; not possibly anywhere inside comments or strings.
    (narrow-to-region (line-beginning-position -500) (point-max))
    (save-excursion
      (let ((suffix (list slime-cursor-marker)))
        (cond ((slime-compare-char-syntax #'char-after "(" t)
               ;; We're at the start of some expression, so make sure
               ;; that SWANK::%CURSOR-MARKER% will come after that
               ;; expression. If the expression is not balanced, make
               ;; still sure that the marker does *not* come directly
               ;; after the preceding expression.
               (or (ignore-errors (forward-sexp) t)
                   (push "" suffix)))
              ((or (bolp) (slime-compare-char-syntax #'char-before " " t))
               ;; We're after some expression, so we have to make sure
               ;; that %CURSOR-MARKER% does *not* come directly after
               ;; that expression.
               (push "" suffix))
              ((slime-compare-char-syntax #'char-before "(" t)
               ;; We're directly after an opening parenthesis, so we
               ;; have to make sure that something comes before
               ;; %CURSOR-MARKER%.
               (push "" suffix))
              (t
               ;; We're at a symbol, so make sure we get the whole symbol.
               (slime-end-of-symbol)))
        (let ((pt (point)))
          (ignore-errors (up-list (if max-levels (- max-levels) -5)))
          (ignore-errors (down-list))
          (slime-parse-form-until pt suffix))))))

(let ((byte-compile-warnings '()))
  (mapc #'byte-compile
        '(slime-parse-form-upto-point
          slime-parse-form-until
          slime-compare-char-syntax
          )))

;;;; Test cases

(defun slime-check-buffer-form (result-form)
  (slime-test-expect 
   (format "Buffer form correct in `%s' (at %d)" (buffer-string) (point))
   result-form
   (slime-parse-form-upto-point 10)))

(def-slime-test form-up-to-point.1
    (buffer-sexpr result-form &optional skip-trailing-test-p)
    ""
    '(("(char= #\\(*HERE*"               ("char=" "#\\(" swank::%cursor-marker%))
      ("(char= #\\( *HERE*"              ("char=" "#\\(" "" swank::%cursor-marker%))
      ("(char= #\\) *HERE*"              ("char=" "#\\)" "" swank::%cursor-marker%))
      ("(char= #\\*HERE*"                ("char=" "#\\" swank::%cursor-marker%) t)
      ("(defun*HERE*"                    ("defun" swank::%cursor-marker%))
      ("(defun foo*HERE*"                ("defun" "foo" swank::%cursor-marker%))
      ("(defun foo (x y)*HERE*"          ("defun" "foo" ("x" "y") swank::%cursor-marker%))
      ("(defun foo (x y*HERE*"           ("defun" "foo" ("x" "y" swank::%cursor-marker%)))
      ("(apply 'foo*HERE*"               ("apply" "'foo" swank::%cursor-marker%))
      ("(apply #'foo*HERE*"              ("apply" "#'foo" swank::%cursor-marker%))
      ("(declare ((vector bit *HERE*"    ("declare" (("vector" "bit" "" swank::%cursor-marker%))))
      ("(with-open-file (*HERE*"         ("with-open-file" ("" swank::%cursor-marker%)))
      ("(((*HERE*"                       ((("" swank::%cursor-marker%))))
      ("(defun #| foo #| *HERE*"         ("defun" "" swank::%cursor-marker%))
      ("(defun #-(and) (bar) f*HERE*"    ("defun" "f" swank::%cursor-marker%))
      ("(remove-if #'(lambda (x)*HERE*"  ("remove-if" ("lambda" ("x") swank::%cursor-marker%)))
      ("`(remove-if ,(lambda (x)*HERE*"  ("remove-if" ("lambda" ("x") swank::%cursor-marker%)))
      ("`(remove-if ,@(lambda (x)*HERE*" ("remove-if" ("lambda" ("x") swank::%cursor-marker%))))
  (slime-check-top-level)
  (with-temp-buffer
    (lisp-mode)
    (insert buffer-sexpr)
    (search-backward "*HERE*")
    (delete-region (match-beginning 0) (match-end 0))
    (slime-check-buffer-form result-form)
    (unless skip-trailing-test-p
      (insert ")") (backward-char)
      (slime-check-buffer-form result-form))
    ))

(defun slime-trace-query (spec)
  "Ask the user which function to trace; SPEC is the default.
The result is a string."
  (cond ((null spec)
         (slime-read-from-minibuffer "(Un)trace: "))
        ((stringp spec)
         (slime-read-from-minibuffer "(Un)trace: " spec))
        ((symbolp spec)    ; `slime-extract-context' can return symbols.
         (slime-read-from-minibuffer "(Un)trace: " (prin1-to-string spec)))
        (t
         (destructure-case spec
           ((setf n)
            (slime-read-from-minibuffer "(Un)trace: " (prin1-to-string spec)))
           ((:defun n)
            (slime-read-from-minibuffer "(Un)trace: " (prin1-to-string n)))
           ((:defgeneric n)
            (let* ((name (prin1-to-string n))
                   (answer (slime-read-from-minibuffer "(Un)trace: " name)))
              (cond ((and (string= name answer)
                          (y-or-n-p (concat "(Un)trace also all " 
                                            "methods implementing " 
                                            name "? ")))
                     (prin1-to-string `(:defgeneric ,n)))
                    (t
                     answer))))
           ((:defmethod &rest _)
            (slime-read-from-minibuffer "(Un)trace: " (prin1-to-string spec)))
           ((:call caller callee)
            (let* ((callerstr (prin1-to-string caller))
                   (calleestr (prin1-to-string callee))
                   (answer (slime-read-from-minibuffer "(Un)trace: " 
                                                       calleestr)))
              (cond ((and (string= calleestr answer)
                          (y-or-n-p (concat "(Un)trace only when " calleestr
                                            " is called by " callerstr "? ")))
                     (prin1-to-string `(:call ,caller ,callee)))
                    (t
                     answer))))
           (((:labels :flet) &rest _)
            (slime-read-from-minibuffer "(Un)trace local function: "
                                        (prin1-to-string spec)))
           (t (error "Don't know how to trace the spec %S" spec))))))

(defun slime-extract-context ()
  "Parse the context for the symbol at point.  
Nil is returned if there's no symbol at point.  Otherwise we detect
the following cases (the . shows the point position):

 (defun n.ame (...) ...)                 -> (:defun name)
 (defun (setf n.ame) (...) ...)          -> (:defun (setf name))
 (defmethod n.ame (...) ...)             -> (:defmethod name (...))
 (defun ... (...) (labels ((n.ame (...)  -> (:labels (:defun ...) name)
 (defun ... (...) (flet ((n.ame (...)    -> (:flet (:defun ...) name)
 (defun ... (...) ... (n.ame ...) ...)   -> (:call (:defun ...) name)
 (defun ... (...) ... (setf (n.ame ...)  -> (:call (:defun ...) (setf name))

 (defmacro n.ame (...) ...)              -> (:defmacro name)
 (defsetf n.ame (...) ...)               -> (:defsetf name)
 (define-setf-expander n.ame (...) ...)  -> (:define-setf-expander name)
 (define-modify-macro n.ame (...) ...)   -> (:define-modify-macro name)
 (define-compiler-macro n.ame (...) ...) -> (:define-compiler-macro name)
 (defvar n.ame (...) ...)                -> (:defvar name)
 (defparameter n.ame ...)                -> (:defparameter name)
 (defconstant n.ame ...)                 -> (:defconstant name)
 (defclass n.ame ...)                    -> (:defclass name)
 (defstruct n.ame ...)                   -> (:defstruct name)
 (defpackage n.ame ...)                  -> (:defpackage name)
For other contexts we return the symbol at point."
  (let ((name (slime-symbol-at-point)))
    (if name
        (let ((symbol (read name)))
          (or (progn ;;ignore-errors 
                (slime-parse-context symbol))
              symbol)))))

(defun slime-parse-context (name)
  (save-excursion 
    (cond ((slime-in-expression-p '(defun *))          `(:defun ,name))
          ((slime-in-expression-p '(defmacro *))       `(:defmacro ,name))
          ((slime-in-expression-p '(defgeneric *))     `(:defgeneric ,name))
          ((slime-in-expression-p '(setf *))
           ;;a setf-definition, but which?
           (backward-up-list 1)
           (slime-parse-context `(setf ,name)))
          ((slime-in-expression-p '(defmethod *))
           (unless (looking-at "\\s ")
             (forward-sexp 1)) ; skip over the methodname
           (let (qualifiers arglist)
             (loop for e = (read (current-buffer))
                   until (listp e) do (push e qualifiers)
                   finally (setq arglist e))
             `(:defmethod ,name ,@qualifiers
                          ,(slime-arglist-specializers arglist))))
          ((and (symbolp name) 
                (slime-in-expression-p `(,name)))
           ;; looks like a regular call
           (let ((toplevel (ignore-errors (slime-parse-toplevel-form))))
             (cond ((slime-in-expression-p `(setf (*)))  ;a setf-call
                    (if toplevel
                        `(:call ,toplevel (setf ,name))
                      `(setf ,name)))
                   ((not toplevel)
                    name)
                   ((slime-in-expression-p `(labels ((*))))
                    `(:labels ,toplevel ,name))
                   ((slime-in-expression-p `(flet ((*))))
                    `(:flet ,toplevel ,name))
                   (t
                    `(:call ,toplevel ,name)))))
          ((slime-in-expression-p '(define-compiler-macro *))
           `(:define-compiler-macro ,name))
          ((slime-in-expression-p '(define-modify-macro *))
           `(:define-modify-macro ,name))
          ((slime-in-expression-p '(define-setf-expander *))
           `(:define-setf-expander ,name))
          ((slime-in-expression-p '(defsetf *))
           `(:defsetf ,name))
          ((slime-in-expression-p '(defvar *))       `(:defvar ,name))
          ((slime-in-expression-p '(defparameter *)) `(:defparameter ,name))
          ((slime-in-expression-p '(defconstant *))  `(:defconstant ,name))
          ((slime-in-expression-p '(defclass *))     `(:defclass ,name))
          ((slime-in-expression-p '(defpackage *))   `(:defpackage ,name))
          ((slime-in-expression-p '(defstruct *))
           `(:defstruct ,(if (consp name)
                             (car name)
                             name)))
          (t 
           name))))


(defun slime-in-expression-p (pattern)
  "A helper function to determine the current context.
The pattern can have the form:
 pattern ::= ()    ;matches always
           | (*)   ;matches inside a list
           | (<symbol> <pattern>)   ;matches if the first element in
				    ; the current list is <symbol> and
                                    ; if <pattern> matches.
           | ((<pattern>))          ;matches if we are in a nested list."
  (save-excursion
    (let ((path (reverse (slime-pattern-path pattern))))
      (loop for p in path
            always (ignore-errors 
                     (etypecase p
                       (symbol (slime-beginning-of-list) 
                               (eq (read (current-buffer)) p))
                       (number (backward-up-list p)
                               t)))))))

(defun slime-pattern-path (pattern)
  ;; Compute the path to the * in the pattern to make matching
  ;; easier. The path is a list of symbols and numbers.  A number
  ;; means "(down-list <n>)" and a symbol "(look-at <sym>)")
  (if (null pattern)
      '()
    (etypecase (car pattern)
      ((member *) '())
      (symbol (cons (car pattern) (slime-pattern-path (cdr pattern))))
      (cons (cons 1 (slime-pattern-path (car pattern)))))))

(defun slime-beginning-of-list (&optional up)
  "Move backward to the beginning of the current expression.
Point is placed before the first expression in the list."
  (backward-up-list (or up 1))
  (down-list 1)
  (skip-syntax-forward " "))

(defun slime-end-of-list (&optional up)
  (backward-up-list (or up 1))
  (forward-list 1)
  (down-list -1))

(defun slime-parse-toplevel-form ()
  (ignore-errors                        ; (foo)
    (save-excursion
      (goto-char (car (slime-region-for-defun-at-point)))
      (down-list 1)
      (forward-sexp 1)
      (slime-parse-context (read (current-buffer))))))
		 
(defun slime-arglist-specializers (arglist)
  (cond ((or (null arglist)
	     (member (first arglist) '(&optional &key &rest &aux)))
	 (list))
	((consp (first arglist))
	 (cons (second (first arglist))
	       (slime-arglist-specializers (rest arglist))))
	(t
	 (cons 't 
	       (slime-arglist-specializers (rest arglist))))))

(defun slime-definition-at-point (&optional only-functional)
  "Return object corresponding to the definition at point."
  (let ((toplevel (slime-parse-toplevel-form)))
    (if (or (symbolp toplevel)
            (and only-functional
                 (not (member (car toplevel)
                              '(:defun :defgeneric :defmethod
                                :defmacro :define-compiler-macro)))))
        (error "Not in a definition")
        (destructure-case toplevel
          (((:defun :defgeneric) symbol)
           (format "#'%s" symbol))
          (((:defmacro :define-modify-macro) symbol)
           (format "(macro-function '%s)" symbol))
          ((:define-compiler-macro symbol)
           (format "(compiler-macro-function '%s)" symbol))
          ((:defmethod symbol &rest args)
           (declare (ignore args))
           (format "#'%s" symbol))
          (((:defparameter :defvar :defconstant) symbol)
           (format "'%s" symbol))
          (((:defclass :defstruct) symbol)
           (format "(find-class '%s)" symbol))
          ((:defpackage symbol)
           (format "(or (find-package '%s) (error \"Package %s not found\"))"
                   symbol symbol))
          (t
           (error "Not in a definition"))))))

;; FIXME: not used here; move it away
(if (and (featurep 'emacs) (>= emacs-major-version 22))
    ;;  N.B. The 2nd, and 6th return value cannot be relied upon.
    (defsubst slime-current-parser-state ()
      ;; `syntax-ppss' does not save match data as it invokes
      ;; `beginning-of-defun' implicitly which does not save match
      ;; data. This issue has been reported to the Emacs maintainer on
      ;; Feb27.
      (syntax-ppss))
    (defsubst slime-current-parser-state ()
      (let ((original-pos (point)))
        (save-excursion
          (beginning-of-defun)
          (parse-partial-sexp (point) original-pos)))))

(defun slime-inside-string-p ()
  (nth 3 (slime-current-parser-state)))

(defun slime-inside-comment-p ()
  (nth 4 (slime-current-parser-state)))

(defun slime-inside-string-or-comment-p ()
  (let ((state (slime-current-parser-state)))
    (or (nth 3 state) (nth 4 state))))

;;; The following two functions can be handy when inspecting
;;; source-location while debugging `M-.'.
;;;
(defun slime-current-tlf-number ()
  "Return the current toplevel number."
  (interactive)
  (let ((original-pos (car (slime-region-for-defun-at-point)))
        (n 0))
    (save-excursion
      ;; We use this and no repeated `beginning-of-defun's to get
      ;; reader conditionals right.
      (goto-char (point-min))
      (while (progn (slime-forward-sexp)
                    (< (point) original-pos))
        (incf n)))
    n))

;;; This is similiar to `slime-enclosing-form-paths' in the
;;; `slime-parse' contrib except that this does not do any duck-tape
;;; parsing, and gets reader conditionals right.
(defun slime-current-form-path ()
  "Returns the path from the beginning of the current toplevel
form to the atom at point, or nil if we're in front of a tlf."
  (interactive)
  (let ((source-path nil))
    (save-excursion
      ;; Moving forward to get reader conditionals right.
      (loop for inner-pos = (point)
            for outer-pos = (nth-value 1 (slime-current-parser-state))
            while outer-pos do
            (goto-char outer-pos)
            (unless (eq (char-before) ?#) ; when at #(...) continue.
              (forward-char)
              (let ((n 0))
                (while (progn (slime-forward-sexp)
                              (< (point) inner-pos))
                  (incf n))
                (push n source-path)
                (goto-char outer-pos)))))
    source-path))

(provide 'slime-parse)
