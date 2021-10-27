;;; my-lsp.el --- Provide custom configurations
;;; Commentary:
;;; Code:

;; LSP
(require-package 'lsp-mode)
(require-package 'lsp-ui)
(require-package 'lsp-treemacs)
(require-package 'dap-mode)
(require-package 'which-key)

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (XXX-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui
  :commands  lsp-ui-mode
  :defer t
  :config
  (setq lsp-ui-imenu-auto-refresh t))
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; This feature does not support terminal so disable
(setq dap-auto-configure-features '(sessions locals controls tooltip))
(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))

;; optional if you want which-key integration
(use-package which-key
  :config
  (which-key-mode))

;; lsp-treemacs with evil
(require-package 'treemacs-evil)
(require 'treemacs-evil)

(provide 'my-lsp)
;;; End
