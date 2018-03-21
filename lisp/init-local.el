;;; Personal modifications.

;;; Adds personal programming preferences.
(require 'google-c-style)

(add-hook 'python-mode-hook
	  (function (lambda ()
		      (setq indent-tabs-mode nil
			                                tab-width 2))))
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hxx\\'" . c++-mode))
(provide 'init-local)
