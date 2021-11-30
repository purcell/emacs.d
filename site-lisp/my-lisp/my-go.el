;;; my-go.el --- Provide custom configurations
;;; Commentary:
;;; Code:

(defun my-go-mode-hook ()
  (setq tab-width 4)
  (setq indent-tabs-mode 1))
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Go LSP
(add-hook 'go-mode-hook #'lsp-deferred)
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(provide 'my-go)
;;; End
