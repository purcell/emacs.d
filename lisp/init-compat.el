;;----------------------------------------------------------------------------
;; Allow recent packages to safely pass an arg to 'called-interactively-p
;; in older Emacsen, including 23.1.
;;----------------------------------------------------------------------------
(let ((fn (symbol-function 'called-interactively-p)))
  (when (and (subrp fn) (zerop (cdr-safe (subr-arity fn))))
    (message "Warning: overriding called-interactively-p to support an argument.")
    (fset 'sanityinc/called-interactively-p fn)
    (defun called-interactively-p (&optional kind)
      "Overridden; see `sanityinc/called-interactively-p' for the wrapped function."
      (sanityinc/called-interactively-p))))


;;----------------------------------------------------------------------------
;; Restore removed var alias, used by ruby-electric-brace and others
;;----------------------------------------------------------------------------
(unless (boundp 'last-command-char)
  (defvaralias 'last-command-char 'last-command-event))


(provide 'init-compat)
