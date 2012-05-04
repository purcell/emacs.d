;;----------------------------------------------------------------------------
;; Provide a version of Emacs 24's 'string-prefix-p in older emacsen
;;----------------------------------------------------------------------------
(unless (fboundp 'string-prefix-p)
  (defun string-prefix-p (str1 str2 &optional ignore-case)
    "Return non-nil if STR1 is a prefix of STR2.
If IGNORE-CASE is non-nil, the comparison is done without paying attention
to case differences."
    (eq t (compare-strings str1 nil nil
                           str2 0 (length str1) ignore-case))))


;;----------------------------------------------------------------------------
;; Allow recent packages to safely pass an arg to 'called-interactively-p
;; in older Emacsen, including 23.1.
;;----------------------------------------------------------------------------
(let ((fn (symbol-function 'called-interactively-p)))
  (when (zerop (cdr-safe (subr-arity fn)))
    (message "Warning: overriding called-interactively-p to support an argument.")
    (fset 'sanityinc/called-interactively-p fn)
    (defun called-interactively-p (&optional kind)
      "Overridden; see `sanityinc/called-interactively-p' for the wrapped function."
      (sanityinc/called-interactively-p))))

(when (< emacs-major-version 24)
  ;; Help package.el work in older Emacsen, where there's no TRASH arg
  ;; for 'delete-directory
  (message "Warning: overriding delete-directory to support TRASH argument.")
  (fset 'sanityinc/delete-directory (symbol-function 'delete-directory))
  (defun delete-directory (directory &optional recursive trash)
    "Overridden: see `sanityinc/delete-directory' for the wrapped function"
    (sanityinc/delete-directory directory recursive)))

(provide 'init-compat)
