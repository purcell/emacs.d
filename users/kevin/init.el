
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
;; Config for ios development
;;----------------------------------------------------------------------------
(require-package 'swift-mode)
(require-package 'osx-plist)

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
;; load my config
;;----------------------------------------------------------------------------


;;----------------------------------------------------------------------------
;; Mutt config
;;----------------------------------------------------------------------------
;; Mutt support.
(setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))


;;----------------------------------------------------------------------------
;; Base Using config
;;----------------------------------------------------------------------------
;; query switch to root, support when this file is not writable, auto ask switch to root
;; (add-hook 'find-file-hook
;;           (when (and (eq 0 (nth 2 (file-attributes buffer-file-name)))
;;                      (not (file-writable-p buffer-file-name))
;;                      (y-or-n-p "Switch to root ? "))
;;             (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; ;; don't let the cursor go into minibuffer prompt
;; (setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))


;; reference | http://github.com/milkypostman/dotemacs/init.el
;; (defun rename-current-buffer-file ()
;;   "Rename current buffer file."
;;   (interactive)
;;   (let ((oldname (buffer-file-name)))
;;     (if (null oldname)
;;         (error "Not a file buffer.")
;;       (let ((newname (read-file-name "New name: " nil oldname)))
;;         (if (get-file-buffer newname)
;;             (error "A buffer named %s already exists." newname)
;;           (rename-file oldname newname 0)
;;           (rename-buffer newname)
;;           (set-visited-file-name newname)
;;           (set-buffer-modified-p nil)
;;           (message "Successfully renamed to %s." (file-name-nondirectory newname)))))))
;;(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(defun my-find-file-as-root ()
  "Like `find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

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

