(eval-and-compile
  (require 'slime))

(define-slime-contrib slime-fancy-inspector
  "Fancy inspector for CLOS objects."
  (:authors "Marco Baringer <mb@bese.it> and others")
  (:license "GPL")
  (:slime-dependencies slime-parse)
  (:swank-dependencies swank-fancy-inspector)
  (:on-load
   (add-hook 'slime-edit-definition-hooks 'slime-edit-inspector-part))
  (:on-unload
   (remove-hook 'slime-edit-definition-hooks 'slime-edit-inspector-part)))

(defun slime-inspect-definition ()
  "Inspect definition at point"
  (interactive)
  (slime-inspect (slime-definition-at-point)))

(defun slime-disassemble-definition ()
  "Disassemble definition at point"
  (interactive)
  (slime-eval-describe `(swank:disassemble-form
                         ,(slime-definition-at-point t))))

(defun slime-edit-inspector-part (name &optional where)
  (and (eq major-mode 'slime-inspector-mode)
       (cl-destructuring-bind (&optional property value)
           (slime-inspector-property-at-point)
         (when (eq property 'slime-part-number)
           (let ((location (slime-eval `(swank:find-definition-for-thing
                                         (swank:inspector-nth-part ,value))))
                 (name (format "Inspector part %s" value)))
             (when (and (consp location)
                        (not (eq (car location) :error)))
               (slime-edit-definition-cont
                (list (make-slime-xref :dspec `(,name)
                                       :location location))
                name
                where)))))))

(provide 'slime-fancy-inspector)
