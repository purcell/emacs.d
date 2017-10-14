(require 'slime)
(require 'slime-parse)
(require 'cl-lib)

(define-slime-contrib slime-enclosing-context
  "Utilities on top of slime-parse."
  (:authors "Tobias C. Rittweiler <tcr@freebits.de>")
  (:license "GPL"))

(defun slime-parse-sexp-at-point (&optional n)
  "Returns the sexps at point as a list of strings, otherwise nil.
\(If there are not as many sexps as N, a list with < N sexps is
returned.\)
If SKIP-BLANKS-P is true, leading whitespaces &c are skipped.
"
  (interactive "p") (or n (setq n 1))
  (save-excursion
    (let ((result nil))
      (dotimes (i n)
        ;; Is there an additional sexp in front of us?
        (save-excursion
          (unless (slime-point-moves-p (ignore-errors (forward-sexp)))
            (cl-return)))
        (push (slime-sexp-at-point) result)
        ;; Skip current sexp
        (ignore-errors (forward-sexp) (skip-chars-forward "[:space:]")))
      (nreverse result))))

(defun slime-has-symbol-syntax-p (string)
  (if (and string (not (zerop (length string))))
      (member (char-syntax (aref string 0))
 '(?w ?_ ?\' ?\\))))

(defun slime-beginning-of-string ()
  (let* ((parser-state (slime-current-parser-state))
	 (inside-string-p  (nth 3 parser-state))
	 (string-start-pos (nth 8 parser-state)))
    (if inside-string-p
        (goto-char string-start-pos)
      (error "We're not within a string"))))

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
                  (cl-incf arg-index))
              (ignore-errors (backward-sexp 1))
              (while (and (< arg-index 64)
                          (ignore-errors (backward-sexp 1)
                                         (> (point) (point-min))))
                (cl-incf arg-index))
              (backward-up-list 1)
              (when (member (char-syntax (char-after)) '(?\( ?'))
                (cl-incf level)
                (forward-char 1)
                (let ((name (slime-symbol-at-point)))
                  (push (and name `(,name)) result)
                  (push arg-index arg-indices)
                  (push (point) points))
                (backward-up-list 1)))))))
    (cl-values
     (nreverse result)
     (nreverse arg-indices)
 (nreverse points))))

(defvar slime-variable-binding-ops-alist
  '((let &bindings &body)
    (let* &bindings &body)))

(defvar slime-function-binding-ops-alist
  '((flet &bindings &body)
    (labels &bindings &body)
    (macrolet &bindings &body)))

(defun slime-lookup-binding-op (op &optional binding-type)
  (cl-labels ((lookup-in (list) (cl-assoc op list :test 'cl-equalp :key 'symbol-name)))
    (cond ((eq binding-type :variable) (lookup-in slime-variable-binding-ops-alist))
	  ((eq binding-type :function) (lookup-in slime-function-binding-ops-alist))
	  (t (or (lookup-in slime-variable-binding-ops-alist)
		 (lookup-in slime-function-binding-ops-alist))))))

(defun slime-binding-op-p (op &optional binding-type)
  (and (slime-lookup-binding-op op binding-type) t))

(defun slime-binding-op-body-pos (op)
  (let ((special-lambda-list (slime-lookup-binding-op op)))
    (if special-lambda-list (cl-position '&body special-lambda-list))))

(defun slime-binding-op-bindings-pos (op)
  (let ((special-lambda-list (slime-lookup-binding-op op)))
    (if special-lambda-list (cl-position '&bindings special-lambda-list))))

(defun slime-enclosing-bound-names ()
  "Returns all bound function names as first value, and the
points where their bindings are established as second value."
  (cl-multiple-value-call #'slime-find-bound-names
                          (slime-enclosing-form-specs)))

(defun slime-find-bound-names (ops indices points)
  (let ((binding-names) (binding-start-points))
    (save-excursion
      (cl-loop for (op . nil) in ops
               for index in indices
               for point in points
               do (when (and (slime-binding-op-p op)
                             ;; Are the bindings of OP in scope?
                             (>= index (slime-binding-op-body-pos op)))
                    (goto-char point)
                    (forward-sexp (slime-binding-op-bindings-pos op))
                    (down-list)
                    (ignore-errors
                      (cl-loop
                       (down-list)
                       (push (slime-symbol-at-point) binding-names)
                       (push (save-excursion (backward-up-list) (point))
                             binding-start-points)
                       (up-list)))))
      (cl-values (nreverse binding-names) (nreverse binding-start-points)))))


(defun slime-enclosing-bound-functions ()
  (cl-multiple-value-call #'slime-find-bound-functions
                          (slime-enclosing-form-specs)))

(defun slime-find-bound-functions (ops indices points)
  (let ((names) (arglists) (start-points))
    (save-excursion
      (cl-loop for (op . nil) in ops
               for index in indices
               for point in points
               do (when (and (slime-binding-op-p op :function)
                             ;; Are the bindings of OP in scope?
                             (>= index (slime-binding-op-body-pos op)))
                    (goto-char point)
                    (forward-sexp (slime-binding-op-bindings-pos op))
                    (down-list)
                    ;; If we're at the end of the bindings, an error will
                    ;; be signalled by the `down-list' below.
                    (ignore-errors
                      (cl-loop
                       (down-list)
                       (cl-destructuring-bind (name arglist)
                           (slime-parse-sexp-at-point 2)
                         (cl-assert (slime-has-symbol-syntax-p name))
                         (cl-assert arglist)
                         (push name names)
                         (push arglist arglists)
                         (push (save-excursion (backward-up-list) (point))
                               start-points))
                       (up-list)))))
      (cl-values (nreverse names)
                 (nreverse arglists)
                 (nreverse start-points)))))


(defun slime-enclosing-bound-macros ()
  (cl-multiple-value-call #'slime-find-bound-macros
                          (slime-enclosing-form-specs)))

(defun slime-find-bound-macros (ops indices points)
  ;; Kludgy!
  (let ((slime-function-binding-ops-alist '((macrolet &bindings &body))))
    (slime-find-bound-functions ops indices points)))

(provide 'slime-enclosing-context)
