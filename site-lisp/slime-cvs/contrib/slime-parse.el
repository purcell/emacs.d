;;; slime-parse.el --- parsing of Common Lisp source code
;;
;; Authors: Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>
;;          Tobias C. Rittweiler <tcr@freebits.de>
;;          and others
;; 
;; License: GNU GPL (same license as Emacs)
;;

(defun slime-incomplete-form-at-point ()
  "Looks for a ``raw form spec'' around point to be processed by
SWANK::PARSE-FORM-SPEC. It is similiar to
SLIME-INCOMPLETE-SEXP-AT-POINT but looks further back than just
one sexp to find out the context."
  (multiple-value-bind (operators arg-indices points)
      (slime-enclosing-form-specs)
    (if (null operators)
        ""
        (let ((op        (first operators))
	      (op-start  (first points))
	      (arg-index (first arg-indices)))
          (destructure-case (slime-ensure-list op)
            ((:declaration declspec) op)
            ((:type-specifier typespec) op)
            (t 
	     (slime-make-form-spec-from-string 
	      (concat (slime-incomplete-sexp-at-point) ")"))))))))

;; XXX: unused function
(defun slime-cl-symbol-external-ref-p (symbol)
  "Does SYMBOL refer to an external symbol?
FOO:BAR is an external reference.
FOO::BAR is not, and nor is BAR."
  (let ((name (if (stringp symbol) symbol (symbol-name symbol))))
    (and (string-match ":" name)
         (not (string-match "::" name)))))

(defun slime-cl-symbol-name (symbol)
  (let ((n (if (stringp symbol) symbol (symbol-name symbol))))
    (if (string-match ":\\([^:]*\\)$" n)
	(let ((symbol-part (match-string 1 n)))
          (if (string-match "^|\\(.*\\)|$" symbol-part)
              (match-string 1 symbol-part)
              symbol-part))
      n)))

(defun slime-cl-symbol-package (symbol &optional default)
  (let ((n (if (stringp symbol) symbol (symbol-name symbol))))
    (if (string-match "^\\([^:]*\\):" n)
	(match-string 1 n)
      default)))

;; XXX: unused function
(defun slime-qualify-cl-symbol (symbol-or-name)
  "Like `slime-qualify-cl-symbol-name', but interns the result."
  (intern (slime-qualify-cl-symbol-name symbol-or-name)))

(defun slime-qualify-cl-symbol-name (symbol-or-name)
  "Return a package-qualified symbol-name that indicates the CL symbol
SYMBOL. If SYMBOL doesn't already have a package prefix the current
package is used."
  (let ((s (if (stringp symbol-or-name)
               symbol-or-name
             (symbol-name symbol-or-name))))
    (if (slime-cl-symbol-package s)
        s
      (format "%s::%s"
              (let* ((package (slime-current-package)))
                ;; package is a string like ":cl-user" or "CL-USER".
                (if (and package (string-match "^:" package))
                    (substring package 1)
                  package))
              (slime-cl-symbol-name s)))))


(defun slime-parse-sexp-at-point (&optional n skip-blanks-p)
  "Return the sexp at point as a string, otherwise nil.
If N is given and greater than 1, a list of all such sexps
following the sexp at point is returned. (If there are not
as many sexps as N, a list with < N sexps is returned.)

If SKIP-BLANKS-P is true, leading whitespaces &c are skipped.
"
  (interactive "p") (or n (setq n 1))
  (flet ((sexp-at-point (first-choice)
           (let ((string (if (eq first-choice :symbol-first)
                             (or (slime-symbol-name-at-point)
                                 (thing-at-point 'sexp))
                             (or (thing-at-point 'sexp)
                                 (slime-symbol-name-at-point)))))
             (if string (substring-no-properties string) nil))))
    ;; `thing-at-point' depends upon the current syntax table; otherwise
    ;; keywords like `:foo' are not recognized as sexps. (This function
    ;; may be called from temporary buffers etc.)
    (with-syntax-table lisp-mode-syntax-table
      (save-excursion
        (when skip-blanks-p ; e.g. `( foo bat)' where point is after ?\(.
          (slime-forward-blanks))
        (let ((result nil))
          (dotimes (i n)
            ;; `foo(bar baz)' where point is at ?\( or ?\).
            (if (and (char-after) (member (char-syntax (char-after)) '(?\( ?\) ?\')))
                (push (sexp-at-point :sexp-first) result)
                (push (sexp-at-point :symbol-first) result))
            (ignore-errors (forward-sexp) (slime-forward-blanks))
            (save-excursion
              (unless (slime-point-moves-p (ignore-errors (forward-sexp)))
                (return))))
          (if (slime-length= result 1)
              (first result)
              (nreverse result)))))))

(defun slime-incomplete-sexp-at-point (&optional n)
  (interactive "p") (or n (setq n 1))
  (buffer-substring-no-properties (save-excursion (backward-up-list n) (point))
                                  (point)))


(defun slime-parse-extended-operator-name (user-point forms indices points)
  "Assume that point is directly at the operator that should be parsed.
USER-POINT is the value of `point' where the user was looking at.
OPS, INDICES and POINTS are updated to reflect the new values after
parsing, and are then returned back as multiple values."
  ;; OPS, INDICES and POINTS are like the finally returned values of
  ;; SLIME-ENCLOSING-FORM-SPECS except that they're in reversed order,
  ;; i.e. the leftmost (that is the latest) operator comes
  ;; first.
  (save-excursion
    (ignore-errors
      (let* ((current-op (first (first forms)))
             (op-name (upcase (slime-cl-symbol-name current-op)))
             (assoc (assoc op-name slime-extended-operator-name-parser-alist))
             (entry (cdr assoc))
             (parser (if (and entry (listp entry)) 
                         (apply (first entry) (rest entry))
                         entry)))
        (ignore-errors
          (forward-char (1+ (length current-op)))
          (slime-forward-blanks))
        (when parser
          (multiple-value-setq (forms indices points)
            (funcall parser op-name user-point forms indices points))))))
  (values forms indices points))


(defvar slime-extended-operator-name-parser-alist
  '(("MAKE-INSTANCE"  . (slime-make-extended-operator-parser/look-ahead 1))
    ("MAKE-CONDITION" . (slime-make-extended-operator-parser/look-ahead 1))
    ("ERROR"          . (slime-make-extended-operator-parser/look-ahead 1))
    ("SIGNAL"         . (slime-make-extended-operator-parser/look-ahead 1))
    ("WARN"           . (slime-make-extended-operator-parser/look-ahead 1))
    ("CERROR"         . (slime-make-extended-operator-parser/look-ahead 2))
    ("CHANGE-CLASS"   . (slime-make-extended-operator-parser/look-ahead 2))
    ("DEFMETHOD"      . (slime-make-extended-operator-parser/look-ahead 1))
    ("APPLY"          . (slime-make-extended-operator-parser/look-ahead 1))
    ("DECLARE"        . slime-parse-extended-operator/declare)
    ("DECLAIM"        . slime-parse-extended-operator/declare)
    ("PROCLAIM"       . slime-parse-extended-operator/declare)))

(defun slime-make-extended-operator-parser/look-ahead (steps)
  "Returns a parser that parses the current operator at point
plus STEPS-many additional sexps on the right side of the
operator."
  (lexical-let ((n steps))
    #'(lambda (name user-point current-forms current-indices current-points)
        (let ((old-forms (rest current-forms)))
          (let* ((args (slime-ensure-list (slime-parse-sexp-at-point n)))
                 (arg-specs (mapcar #'slime-make-form-spec-from-string args)))
            (setq current-forms (cons `(,name ,@arg-specs) old-forms))))
        (values current-forms current-indices current-points)
        )))

(defun slime-parse-extended-operator/declare
    (name user-point current-forms current-indices current-points)
  (when (string= (thing-at-point 'char) "(")
    (let ((orig-point (point)))
      (goto-char user-point)
      (slime-end-of-symbol)
      ;; Head of CURRENT-FORMS is "declare" at this point, but we're
      ;; interested in what comes next.
      (let* ((decl-ops     (rest current-forms))
             (decl-indices (rest current-indices))
             (decl-points  (rest current-points))
             (decl-pos     (1- (first decl-points)))
             (nesting      (slime-nesting-until-point decl-pos))
             (declspec-str (concat (slime-incomplete-sexp-at-point nesting)
                                   (make-string nesting ?\)))))
        (save-match-data ; `(declare ((foo ...))' or `(declare (type (foo ...)))' ?
          (if (or (eql 0 (string-match "\\s-*(\\((\\(\\sw\\|\\s_\\|\\s-\\)*)\\))$"
                                       declspec-str))
                  (eql 0 (string-match "\\s-*(type\\s-*\\((\\(\\sw\\|\\s_\\|\\s-\\)*)\\))$"
                                       declspec-str)))
              (let* ((typespec-str (match-string 1 declspec-str))
                     (typespec (slime-make-form-spec-from-string typespec-str)))
                (setq current-forms   (list `(:type-specifier ,typespec)))
                (setq current-indices (list (second decl-indices)))
                (setq current-points  (list (second decl-points))))
              (let ((declspec (slime-make-form-spec-from-string declspec-str)))
                (setq current-forms   (list `(,name) `(:declaration ,declspec)))
                (setq current-indices (list (first current-indices)
					    (first decl-indices)))
                (setq current-points  (list (first current-points)
					    (first decl-points)))))))))
  (values current-forms current-indices current-points))

(defun slime-nesting-until-point (target-point)
  "Returns the nesting level between current point and TARGET-POINT.
If TARGET-POINT could not be reached, 0 is returned. (As a result
TARGET-POINT should always be placed just before a `?\('.)"
  (save-excursion
    (let ((nesting 0))
      (while (> (point) target-point)
        (backward-up-list)
        (incf nesting))
      (if (= (point) target-point)
          nesting
          0))))

(defun slime-make-form-spec-from-string (string &optional strip-operator-p)
  "If STRIP-OPERATOR-P is T and STRING is the string
representation of a form, the string representation of this form
is stripped from the form. This can be important to avoid mutual
recursion between this function, `slime-enclosing-form-specs' and
`slime-parse-extended-operator-name'.

Examples:

  \"(foo (bar 1 (baz :quux)) 'toto)\" 

      => (\"foo\" (\"bar\" \"1\" (\"baz\" \":quux\")) \"'toto\")
"
  (cond ((slime-length= string 0) "")                    ; ""
	((equal string "()") '())                        ; "()"
	((eql (char-syntax (aref string 0)) ?\') string) ; "'(foo)", "#(foo)" &c
	((not (eql (aref string 0) ?\()) string)         ; "foo"
	(t                                               ; "(op arg1 arg2 ...)"
	 (with-temp-buffer
	   ;; Do NEVER ever try to activate `lisp-mode' here with
	   ;; `slime-use-autodoc-mode' enabled, as this function is used
	   ;; to compute the current autodoc itself.
	   (erase-buffer)
	   (insert string)
	   (when strip-operator-p ; `(OP arg1 arg2 ...)' ==> `(arg1 arg2 ...)'
	     (goto-char (point-min))
	     (when (string= (thing-at-point 'char) "(")
	       (ignore-errors (forward-char 1)
			      (forward-sexp)
			      (slime-forward-blanks))
	       (delete-region (point-min) (point))
	       (insert "(")))
	   (goto-char (1- (point-max))) ; `(OP arg1 ... argN|)'
	   (assert (eql (char-after) ?\)))
	   (multiple-value-bind (forms indices points)
	       (slime-enclosing-form-specs 1)
	     (if (null forms)
		 string
                (let ((n (first (last indices))))
		  (goto-char (1+ (point-min))) ; `(|OP arg1 ... argN)'
		  (mapcar #'(lambda (s)
			      (assert (not (equal s string))) ; trap against
			      (slime-make-form-spec-from-string s)) ;  endless recursion.
			  (slime-ensure-list
			   (slime-parse-sexp-at-point (1+ n) t))))))))))


(defun slime-enclosing-form-specs (&optional max-levels)
  "Return the list of ``raw form specs'' of all the forms 
containing point from right to left.

As a secondary value, return a list of indices: Each index tells
for each corresponding form spec in what argument position the
user's point is.

As tertiary value, return the positions of the operators that are
contained in the returned form specs. 

When MAX-LEVELS is non-nil, go up at most this many levels of
parens.

\(See SWANK::PARSE-FORM-SPEC for more information about what
exactly constitutes a ``raw form specs'')

Examples:

  A return value like the following

    (values  ((\"quux\") (\"bar\") (\"foo\")) (3 2 1) (p1 p2 p3))

  can be interpreted as follows:

    The user point is located in the 3rd argument position of a
    form with the operator name \"quux\" (which starts at P1.)
   
    This form is located in the 2nd argument position of a form
    with the operator name \"bar\" (which starts at P2.)

    This form again is in the 1st argument position of a form
    with the operator name \"foo\" (which itself begins at P3.)

  For instance, the corresponding buffer content could have looked
  like `(foo (bar arg1 (quux 1 2 |' where `|' denotes point.
"
  (let ((level 1)
        (parse-sexp-lookup-properties nil)
        (initial-point (point))
        (result '()) (arg-indices '()) (points '())) 
    ;; The expensive lookup of syntax-class text properties is only
    ;; used for interactive balancing of #<...> in presentations; we
    ;; do not need them in navigating through the nested lists.
    ;; This speeds up this function significantly.
    (ignore-errors
      (save-excursion
        ;; Make sure we get the whole thing at point.
        (if (not (slime-inside-string-p))
	    (slime-end-of-symbol)
	  (slime-beginning-of-string)
	  (forward-sexp))
        (save-restriction
          ;; Don't parse more than 20000 characters before point, so we don't spend
          ;; too much time.
          (narrow-to-region (max (point-min) (- (point) 20000)) (point-max))
          (narrow-to-region (save-excursion (beginning-of-defun) (point))
                            (min (1+ (point)) (point-max)))
          (while (or (not max-levels)
                     (<= level max-levels))
            (let ((arg-index 0))
              ;; Move to the beginning of the current sexp if not already there.
              (if (or (and (char-after)
                           (member (char-syntax (char-after)) '(?\( ?')))
                      (member (char-syntax (char-before)) '(?\  ?>)))
                  (incf arg-index))
              (ignore-errors (backward-sexp 1))
              (while (and (< arg-index 64)
                          (ignore-errors (backward-sexp 1) 
                                         (> (point) (point-min))))
                (incf arg-index))
              (backward-up-list 1)
              (when (member (char-syntax (char-after)) '(?\( ?')) 
                (incf level)
                (forward-char 1)
                (let ((name (slime-symbol-name-at-point)))
                  (cond
                    (name
                     (save-restriction
                       (widen) ; to allow looking-ahead/back in extended parsing.
                       (multiple-value-bind (new-result new-indices new-points)
                           (slime-parse-extended-operator-name initial-point
                                                               (cons `(,name) result) ; minimal form spec
                                                               (cons arg-index arg-indices)
                                                               (cons (point) points))
                         (setq result new-result)
                         (setq arg-indices new-indices)
                         (setq points new-points))))
                    (t
                     (push nil result)
                     (push arg-index arg-indices)
                     (push (point) points))))
                (backward-up-list 1)))))))
    (values 
     (nreverse result)
     (nreverse arg-indices)
     (nreverse points))))


(defun slime-ensure-list (thing)
  (if (listp thing) thing (list thing)))

(defun slime-inside-string-p ()
  (let* ((toplevel-begin (save-excursion (beginning-of-defun) (point)))
	 (parse-result (parse-partial-sexp toplevel-begin (point)))
	 (inside-string-p  (nth 3 parse-result))
	 (string-start-pos (nth 8 parse-result)))
    (and inside-string-p string-start-pos)))

(defun slime-beginning-of-string ()
  (let ((string-start-pos (slime-inside-string-p)))
    (if string-start-pos
	(goto-char string-start-pos)
	(error "We're not within a string"))))

(provide 'slime-parse)

