;;; keybindings.el --- Provide my Keybinding configurations
;;; Commentary:
;;; Code:

;; DAP mode keybindings
(add-hook 'lsp-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "<f5> <f5>") 'dap-debug)))


(provide 'keybindings)
;; End
