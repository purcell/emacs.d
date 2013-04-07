(add-hook 'after-init-hook 'global-company-mode)
;; don't know why company-clang-modes does not include c++-mode by default
(global-set-key (kbd "C-c o") 'company-complete)

(setq company-clang-modes '(c-mode objc-mode c++-mode))
(provide 'init-company)
