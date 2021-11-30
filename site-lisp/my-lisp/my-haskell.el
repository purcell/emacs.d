;;; my-haskell.el --- Provide custom configurations
;;; Commentary:
;;; Code:

;; Haskell LSP
(require-package 'lsp-haskell)
(require 'lsp)
(require 'lsp-haskell)
;; Hooks so haskell and literate haskell major modes trigger LSP setup
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(provide 'my-haskell)
;; End
