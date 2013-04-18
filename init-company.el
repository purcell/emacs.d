(add-hook 'after-init-hook 'global-company-mode)
;;(company-mode 1)
;; don't know why company-clang-modes does not include c++-mode by default
(global-set-key (kbd "C-c o") 'company-complete)
(setq company-require-match nil)

(setq company-clang-modes '(c-mode objc-mode c++-mode))
(provide 'init-company)
