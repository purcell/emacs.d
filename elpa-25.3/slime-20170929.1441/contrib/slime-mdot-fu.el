(require 'slime)
(require 'cl-lib)

(define-slime-contrib slime-mdot-fu
  "Making M-. work on local functions."
  (:authors "Tobias C. Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:slime-dependencies slime-enclosing-context)
  (:on-load
   (add-hook 'slime-edit-definition-hooks 'slime-edit-local-definition))
  (:on-unload
   (remove-hook 'slime-edit-definition-hooks 'slime-edit-local-definition)))


(defun slime-edit-local-definition (name &optional where)
  "Like `slime-edit-definition', but tries to find the definition
in a local function binding near point."
  (interactive (list (slime-read-symbol-name "Name: ")))
  (cl-multiple-value-bind (binding-name point)
      (cl-multiple-value-call #'cl-some #'(lambda (binding-name point)
                                            (when (cl-equalp binding-name name)
                                              (cl-values binding-name point)))
                              (slime-enclosing-bound-names))
    (when (and binding-name point)
      (slime-edit-definition-cont
       `((,binding-name
	  ,(make-slime-buffer-location (buffer-name (current-buffer)) point)))
       name
       where))))

(provide 'slime-mdot-fu)
