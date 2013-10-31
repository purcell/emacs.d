
(define-slime-contrib slime-indentation
  "Patched version of cl-indent.el as a slime-contrib module"
  (:swank-dependencies swank-indentation))

(load "slime-cl-indent.el")

(setq common-lisp-current-package-function 'slime-current-package)

(defun slime-update-system-indentation (symbol indent packages)
  (let ((list (gethash symbol common-lisp-system-indentation))
        (ok nil))
    (if (not list)
        (puthash symbol (list (cons indent packages))
                 common-lisp-system-indentation)
      (dolist (spec list)
        (cond ((equal (car spec) indent)
               (dolist (p packages)
                 (unless (member p (cdr spec))
                   (push p (cdr spec))))
               (setf ok t))
              (t
               (setf (cdr spec)
                     (set-difference (cdr spec) packages :test 'equal)))))
      (unless ok
        (puthash symbol (cons (cons indent packages)
                              list)
                 common-lisp-system-indentation)))))

(provide 'slime-indentation)
