;;; cider-compat.el --- Functions from newer Emacs versions for compatibility -*- lexical-binding: t -*-

;;; Commentary:

;; Pretty much everything here's copied from subr-x for compatibility with
;; Emacs 24.3 and 24.4.

;;; Code:

(eval-and-compile

  (unless (fboundp 'internal--thread-argument)
    (defmacro internal--thread-argument (first? &rest forms)
      "Internal implementation for `thread-first' and `thread-last'.
When Argument FIRST? is non-nil argument is threaded first, else
last.  FORMS are the expressions to be threaded."
      (pcase forms
        (`(,x (,f . ,args) . ,rest)
         `(internal--thread-argument
           ,first? ,(if first? `(,f ,x ,@args) `(,f ,@args ,x)) ,@rest))
        (`(,x ,f . ,rest) `(internal--thread-argument ,first? (,f ,x) ,@rest))
        (_ (car forms)))))

  (unless (fboundp 'thread-first)
    (defmacro thread-first (&rest forms)
      "Thread FORMS elements as the first argument of their successor.
Example:
    (thread-first
      5
      (+ 20)
      (/ 25)
      -
      (+ 40))
Is equivalent to:
    (+ (- (/ (+ 5 20) 25)) 40)
Note how the single `-' got converted into a list before
threading."
      (declare (indent 1)
               (debug (form &rest [&or symbolp (sexp &rest form)])))
      `(internal--thread-argument t ,@forms)))

  (unless (fboundp 'thread-last)
    (defmacro thread-last (&rest forms)
      "Thread FORMS elements as the last argument of their successor.
Example:
    (thread-last
      5
      (+ 20)
      (/ 25)
      -
      (+ 40))
Is equivalent to:
    (+ 40 (- (/ 25 (+ 20 5))))
Note how the single `-' got converted into a list before
threading."
      (declare (indent 1) (debug thread-first))
      `(internal--thread-argument nil ,@forms))))


(eval-and-compile

  (unless (fboundp 'internal--listify)

    (defsubst internal--listify (elt)
      "Wrap ELT in a list if it is not one."
      (if (not (listp elt))
          (list elt)
        elt)))

  (unless (fboundp 'internal--check-binding)

    (defsubst internal--check-binding (binding)
      "Check BINDING is properly formed."
      (when (> (length binding) 2)
        (signal
         'error
         (cons "`let' bindings can have only one value-form" binding)))
      binding))

  (unless (fboundp 'internal--build-binding-value-form)

    (defsubst internal--build-binding-value-form (binding prev-var)
      "Build the conditional value form for BINDING using PREV-VAR."
      `(,(car binding) (and ,prev-var ,(cadr binding)))))

  (unless (fboundp 'internal--build-binding)

    (defun internal--build-binding (binding prev-var)
      "Check and build a single BINDING with PREV-VAR."
      (thread-first
          binding
        internal--listify
        internal--check-binding
        (internal--build-binding-value-form prev-var))))

  (unless (fboundp 'internal--build-bindings)

    (defun internal--build-bindings (bindings)
      "Check and build conditional value forms for BINDINGS."
      (let ((prev-var t))
        (mapcar (lambda (binding)
                  (let ((binding (internal--build-binding binding prev-var)))
                    (setq prev-var (car binding))
                    binding))
                bindings))))  )

(eval-and-compile

  (unless (fboundp 'if-let)
    (defmacro if-let (bindings then &rest else)
      "Process BINDINGS and if all values are non-nil eval THEN, else ELSE.
Argument BINDINGS is a list of tuples whose car is a symbol to be
bound and (optionally) used in THEN, and its cadr is a sexp to be
evalled to set symbol's value.  In the special case you only want
to bind a single value, BINDINGS can just be a plain tuple."
      (declare (indent 2)
               (debug ([&or (&rest (symbolp form)) (symbolp form)] form body)))
      (when (and (<= (length bindings) 2)
                 (not (listp (car bindings))))
        ;; Adjust the single binding case
        (setq bindings (list bindings)))
      `(let* ,(internal--build-bindings bindings)
         (if ,(car (internal--listify (car (last bindings))))
             ,then
           ,@else)))    )

  (unless (fboundp 'when-let)
    (defmacro when-let (bindings &rest body)
      "Process BINDINGS and if all values are non-nil eval BODY.
Argument BINDINGS is a list of tuples whose car is a symbol to be
bound and (optionally) used in BODY, and its cadr is a sexp to be
evalled to set symbol's value.  In the special case you only want
to bind a single value, BINDINGS can just be a plain tuple."
      (declare (indent 1) (debug if-let))
      (list 'if-let bindings (macroexp-progn body)))    ))

(provide 'ensime-compat)
;;; ensime-compat.el ends here
