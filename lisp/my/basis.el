;;; basis.el --- Provide custom configurations
;;; Commentary:
;;; Code:


(setq frame-resize-pixelwise t)
(setq warning-minimum-level :emergency)

(require-package 'use-package)
(require-package 'alarm-clock)

;; Solarized
(setq x-underline-at-descent-line t)

;; Evil
(require-package 'evil)
(require 'evil)
(evil-mode 1)

(require-package 'neotree)
(setq neo-smart-open t)
(global-set-key (kbd "C-x t") 'neotree-toggle)
(setq projectile-switch-project-action 'neotree-projectile-action)
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
            (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
            (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
            (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
            (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)))

;; sr-speedbar
(require-package 'sr-speedbar)
(defun sr-speedbar-toggle-fixed-size ()
  "Toggle sr-speedbar in fixed size."
  (interactive)
  (sr-speedbar-toggle)
  (with-current-buffer sr-speedbar-buffer-name
    (setq window-size-fixed 'width)))
(global-set-key (kbd "C-x s") 'sr-speedbar-toggle-fixed-size)

;; syntax highlight
(global-font-lock-mode 1)
(transient-mark-mode t)

(require 'my-company)
(require 'my-lsp)

(provide 'basis)
;; End
