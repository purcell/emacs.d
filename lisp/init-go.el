;;; init-go.el --- Support for the Go language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'go-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
  (reformatter-define go-format
    :program "goimports"
    :args '("/dev/stdin")))

(provide 'init-go)
;;; init-go.el ends here
