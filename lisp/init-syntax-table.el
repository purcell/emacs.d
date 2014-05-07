;;; Package --- Modify syntax table
;;; Commentary:
;;; Code:

;; Example : Add '_' in syntax table (the word definition will include '_').
;;
;; (dolist (hook '(makefile-gmake-mode-hook
;;		makefile-bsdmake-mode-hook
;;		c-mode-common-hook))
;;   (add-hook hook
;;	    (lambda () (modify-syntax-entry ?_ "w"))))

(provide 'init-syntax-table)
;;; init-syntax-table.el ends here
