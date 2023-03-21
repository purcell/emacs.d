;;; package --- Summary

;;;  Commentary:
;;

;;; Code:
;; (package-install-file (concat user-emacs-directory "/site-lisp/emacs-prisma-mode/"))

;; (require-package "prisma-mode") ;; best not to include the ending “.el” or “.elc”

(add-to-list 'auto-mode-alist '("\\.\\prisma\\'" . prisma-mode))

(provide 'init-prisma)
;;; init-prisma.el ends here
