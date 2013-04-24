(add-hook 'after-init-hook 'global-company-mode)
;;(company-mode 1)
;; don't know why company-clang-modes does not include c++-mode by default
(global-set-key (kbd "C-c o") 'company-complete)
(setq company-require-match nil)

(mapc #'evil-declare-change-repeat
      '(company-complete-common
        company-select-next
        company-select-previous
        company-complete-selection
        ))

(setq company-clang-modes '(c-mode objc-mode c++-mode))
(provide 'init-company)
