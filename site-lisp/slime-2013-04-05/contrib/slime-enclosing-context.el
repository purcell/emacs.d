
(define-slime-contrib slime-enclosing-context
  "Utilities on top of slime-parse."
  (:authors "Tobias C. Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:slime-dependencies slime-parse)
  (:on-load (error "This contrib does not work at the moment.")))

(defvar slime-variable-binding-ops-alist
  '((let &bindings &body)))

(defvar slime-function-binding-ops-alist
  '((flet &bindings &body) 
    (labels &bindings &body)
    (macrolet &bindings &body)))

(defun slime-lookup-binding-op (op &optional binding-type)
  (flet ((lookup-in (list) (assoc* op list :test 'equalp :key 'symbol-name)))
    (cond ((eq binding-type :variable) (lookup-in slime-variable-binding-ops-alist))
	  ((eq binding-type :function) (lookup-in slime-function-binding-ops-alist))
	  (t (or (lookup-in slime-variable-binding-ops-alist)
		 (lookup-in slime-function-binding-ops-alist))))))

(defun slime-binding-op-p (op &optional binding-type)
  (and (slime-lookup-binding-op op binding-type) t))

(defun slime-binding-op-body-pos (op)
  (when-let (special-lambda-list (slime-lookup-binding-op op))
    (position '&body special-lambda-list)))

(defun slime-binding-op-bindings-pos (op)
  (when-let (special-lambda-list (slime-lookup-binding-op op))
    (position '&bindings special-lambda-list)))


(defun slime-enclosing-bound-names ()
  "Returns all bound function names as first value, and the
points where their bindings are established as second value."
  (multiple-value-call #'slime-find-bound-names (slime-enclosing-form-specs)))

(defun slime-find-bound-names (ops indices points)
  (let ((binding-names) (binding-start-points))
    (save-excursion
      (loop for (op . nil) in ops
	    for index in indices
	    for point in points
	    do (when (and (slime-binding-op-p op) 
			  ;; Are the bindings of OP in scope?
			  (>= index (slime-binding-op-body-pos op)))
		 (goto-char point) 
		 (forward-sexp (slime-binding-op-bindings-pos op))
		 (down-list)
		 (ignore-errors
		   (loop 
		    (down-list) 
		    (push (slime-symbol-at-point) binding-names)
		    (push (save-excursion (backward-up-list) (point)) 
			  binding-start-points)
		    (up-list)))))
      (values (nreverse binding-names) (nreverse binding-start-points)))))


(defun slime-enclosing-bound-functions ()
  (multiple-value-call #'slime-find-bound-functions (slime-enclosing-form-specs)))

(defun slime-find-bound-functions (ops indices points)
  (let ((names) (arglists) (start-points))
    (save-excursion
      (loop for (op . nil) in ops
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
		   (loop
		    (down-list) 
		    (destructuring-bind (name arglist) 
                        (slime-parse-sexp-at-point 2)
		      (assert (slime-has-symbol-syntax-p name)) (assert arglist)
		      (push name names)
		      (push arglist arglists)
		      (push (save-excursion (backward-up-list) (point)) 
			    start-points))
		    (up-list)))))
      (values (nreverse names)
	      (nreverse arglists) 
	      (nreverse start-points)))))


(defun slime-enclosing-bound-macros ()
  (multiple-value-call #'slime-find-bound-macros (slime-enclosing-form-specs)))

(defun slime-find-bound-macros (ops indices points)
  ;; Kludgy!
  (let ((slime-function-binding-ops-alist '((macrolet &bindings &body))))
    (slime-find-bound-functions ops indices points)))

;;; Tests

(def-slime-test enclosing-context.1
    (buffer-sexpr wished-bound-names wished-bound-functions)
    "Check that finding local definitions work."
    '(("(flet ((,nil ()))
	 (let ((bar 13)
	       (,foo 42))
	   *HERE*))"
       ;; We used to return ,foo here, but we do not anymore.  We
       ;; still return ,nil for the `slime-enclosing-bound-functions',
       ;; though. The first one is used for local M-., whereas the
       ;; latter is used for local autodoc. It does not seem too
       ;; important for local M-. to work on such names. \(The reason
       ;; that it does not work anymore, is that
       ;; `slime-symbol-at-point' now does TRT and does not return a
       ;; leading comma anymore.\)
       ("bar" nil nil)
       ((",nil" "()")))
      ("(flet ((foo ()))
         (quux)
         (bar *HERE*))"
       ("foo")
       (("foo" "()"))))
  (slime-check-top-level)
  (with-temp-buffer
    (let ((tmpbuf (current-buffer)))
      (lisp-mode)
      (insert buffer-sexpr)
      (search-backward "*HERE*")
      (multiple-value-bind (bound-names points) 
	  (slime-enclosing-bound-names)
	(slime-check "Check enclosing bound names"
	  (loop for name in wished-bound-names
		always (member name bound-names))))
      (multiple-value-bind (fn-names fn-arglists points) 
	  (slime-enclosing-bound-functions)
	(slime-check "Check enclosing bound functions"
	  (loop for (name arglist) in wished-bound-functions
		always (and (member name fn-names)
			    (member arglist fn-arglists)))))
      )))

(provide 'slime-enclosing-context)
