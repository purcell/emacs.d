(require 'init-company)

(when (maybe-require-package 'go-mode)
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook 'flycheck-mode)

  (require-package `company-go)
  (add-hook 'go-mode-hook 'company-mode))

(provide 'init-go)
