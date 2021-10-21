;;; keybindings.el --- Provide my Keybinding configurations
;;; Commentary:
;;; Code:

;; DAP mode keybindings
(add-hook 'python-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "<f5> <f5>") 'dap-debug)
            (define-key evil-normal-state-local-map (kbd "<f5> <f6>") 'dap-hydra)))


(provide 'keybindings)
;; End
