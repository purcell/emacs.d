(in-package :swank)

(defvar *application-hints-tables* '()
  "A list of hash tables mapping symbols to indentation hints (lists 
of symbols and numbers as per cl-indent.el). Applications can add hash 
tables to the list to change the auto indentation slime sends to 
emacs.")

(defun has-application-indentation-hint-p (symbol)
  (let ((default (load-time-value (gensym))))
    (dolist (table *application-hints-tables*)
      (let ((indentation (gethash symbol table default)))
        (unless (eq default indentation)
          (return-from has-application-indentation-hint-p
            (values indentation t))))))
  (values nil nil))

(defun application-indentation-hint (symbol)
  (let ((indentation (has-application-indentation-hint-p symbol)))
    (labels ((walk (indentation-spec)
               (etypecase indentation-spec
                 (null nil)
                 (number indentation-spec)
                 (symbol (symbol-name indentation-spec))
                 (cons (cons (walk (car indentation-spec))
                             (walk (cdr indentation-spec)))))))
      (walk indentation))))

;;; override swank version of this function
(defun symbol-indentation (symbol)
  "Return a form describing the indentation of SYMBOL. 

The form is to be used as the `common-lisp-indent-function' property 
in Emacs."
  (cond
    ((has-application-indentation-hint-p symbol)
     (application-indentation-hint symbol))
    ((and (macro-function symbol)
             (not (known-to-emacs-p symbol)))
     (let ((arglist (arglist symbol)))
       (etypecase arglist
         ((member :not-available)
          nil)
         (list
          (macro-indentation arglist)))))
    (t nil)))
