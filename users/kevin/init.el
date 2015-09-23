
;; added by lujianmei

;;----------------------------------------------------------------------------
;; Config for smartparents
;;----------------------------------------------------------------------------
;; Default setup of smartparens



(require-package 'smartparens)


(require 'smartparens-config)
(setq sp-autoescape-string-quote nil)
(--each '(css-mode-hook
          restclient-mode-hook
          js-mode-hook
          java-mode
          ruby-mode
          markdown-mode
          groovy-mode)
  (add-hook it 'turn-on-smartparens-mode))



(require-package 'expand-region)
(require-package 'multiple-cursors)
(require-package 'jump-char)
;;(require 'eproject)
(require-package 'smart-forward)
(require-package 'change-inner)
(require-package 'multifiles)
(require-package 'tabbar)
(require-package 'highlight)
(require-package 'w3m)
(require-package 'dired+)
(require-package 'helm-swoop)
(require-package 'helm-projectile)

;; config for smart search
(require-package 'avy)
(require-package 'ace-isearch)
(require 'ace-isearch)


(require 'misc)
(require 'markdown-mode)

;; add smart swap buffers in multi-windows
(require-package 'swap-buffers)
(global-set-key (kbd "C-x 5") 'swap-buffers)

;;----------------------------------------------------------------------------
;; Config for helm-projectile, helm-swoop
;;----------------------------------------------------------------------------

;; https://github.com/ShingoFukuyama/helm-swoop

;; using projectile to manage projects
(projectile-global-mode)

;;(add-hook 'ruby-mode-hook 'projectile-mode)
;; enable cache for big project
;;(setq projectile-enable-caching t)
;; using F5 for finding files in project
(global-set-key [f5] 'projectile-find-file)

;;in order to allow for the occasions where you want to select the top-level directory.
(setq projectile-find-dir-includes-top-level t)

;; using helm for project find 
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile-find-file)
;;(setq projectile-switch-project-action 'helm-projectile) ;; another choice



;; Don't use expand-region fast keys
(setq expand-region-fast-keys-enabled nil)

;; Show expand-region command used
(setq er--show-expansion-message t)

;; Browse kill ring

(require-package 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; Smart M-x is smart
(require-package 'smex)

(smex-initialize)


;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)



;;----------------------------------------------------------------------------
;; Mutt config
;;----------------------------------------------------------------------------
;; Mutt support.
(setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))



;;----------------------------------------------------------------------------
;; Base config
;;----------------------------------------------------------------------------
(display-time)

;; maximize frame when open first
(toggle-frame-maximized)

(tool-bar-mode 0)  
(menu-bar-mode 0)  
(scroll-bar-mode 0)



;;(setq debug-on-error nil)



(provide 'init)
;;; init.el ends here

