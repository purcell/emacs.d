(require-package 'auctex)
(require-package 'cdlatex)

;;----------------------------------------------------------------------------
;; cdlatex
;;----------------------------------------------------------------------------
(add-hook 'LaTeX-mode-hook
	(lambda ()
		(cdlatex-mode t)))

(provide 'init-latex)
