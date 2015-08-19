;; add zencoding by lujianmei
(display-time)

;; Set path to dependencies
;;(setq site-lisp-dir (expand-file-name "site-lisp/" user-emacs-directory))


;; Tern.JS
(add-to-list 'load-path "~/.emacs.d/site-lisp/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'auto-complete
  '(eval-after-load 'tern
     '(progn
        (require 'tern-auto-complete)
        (tern-ac-setup))))


;;设置窗口位置为屏库左上角(0,0)
;;(set-frame-position (selected-frame) 6 0)
;;设置宽和高,大家可以调整这个参数来适应自己屏幕大小，下面的数字是显示字符数量
;;(add-to-list 'default-frame-alist '(width  . 165))
;;(add-to-list 'default-frame-alist '(height . 43))

;; maximize frame when open first
(toggle-frame-maximized)
;;(split-window-right 50)



(require 'dired-view)
;; Enable Dired-view hook for Dired Mode
(add-hook 'dired-mode-hook 'dired-view-minor-mode-on)
;; hide those hidden files/directories
(add-hook 'dired-mode-hook
          (lambda ()
            (setq dired-omit-files "^#\\|^\\..*") ; omit all hidden file which starts with `.'
            (dired-omit-mode 1)))                 ; initially omit unintrested files, set quick-key for this omit function



;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))


;; Default setup of smartparens
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

(require 'expand-region)
(require 'multiple-cursors)
(require 'jump-char)
;;(require 'eproject)
(require 'smart-forward)
(require 'change-inner)
(require 'multifiles)
(require 'tabbar)

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
(setq projectile-switch-project-action 'helm-projectile) ;; another choice





;; Don't use expand-region fast keys
(setq expand-region-fast-keys-enabled nil)

;; Show expand-region command used
(setq er--show-expansion-message t)

;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; Smart M-x is smart
(require 'smex)
(smex-initialize)


;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)




;;(add-to-list 'load-path "~/Emacs/zencoding/")
;;(require 'zencoding-mode)
;;(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes



(tool-bar-mode 0)  
(menu-bar-mode 0)  
(scroll-bar-mode 0)


;; add helm-css-scss config https://github.com/ShingoFukuyama/helm-css-scss
(require 'helm-css-scss)
;; Allow comment inserting depth at each end of a brace
(setq helm-css-scss-insert-close-comment-depth 2)
;; If this value is t, split window appears inside the current window
(setq helm-css-scss-split-with-multiple-windows nil)
;; Split direction. 'split-window-vertically or 'split-window-horizontally
(setq helm-css-scss-split-direction 'split-window-vertically)

;; Set local keybind map for css-mode / scss-mode / less-css-mode
(dolist ($hook '(css-mode-hook scss-mode-hook less-css-mode-hook))
  (add-hook
   $hook (lambda ()
           (local-set-key (kbd "s-i") 'helm-css-scss)
           (local-set-key (kbd "s-I") 'helm-css-scss-back-to-last-point))))

(define-key isearch-mode-map (kbd "s-i") 'helm-css-scss-from-isearch)
(define-key helm-css-scss-map (kbd "s-i") 'helm-css-scss-multi-from-helm-css-scss)


;;(setq debug-on-error nil)




(autoload 'wget "wget" "wget interface for Emacs." t)
(autoload 'wget-web-page "wget" "wget interface to download whole web page." t)


;; Mutt support.
(setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))



;; Emmet
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
;; preview is the default action
(setq emmet-preview-default t)
;; Emmet for helm
(require 'helm-emmet)
;; Emmet for autocomplete
(require 'ac-emmet) ;; Not necessary if using ELPA package
(add-hook 'sgml-mode-hook 'ac-emmet-html-setup)
(add-hook 'css-mode-hook 'ac-emmet-css-setup)

;; set Symbolic link to Git-controlled source file; follow link? (y or n) as yes always
(setq vc-follow-symlinks t)

(provide 'init)
;;; init.el ends here

