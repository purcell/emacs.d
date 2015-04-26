;; add zencoding by lujianmei
(display-time)

;; Set path to dependencies
;;(setq site-lisp-dir (expand-file-name "site-lisp/" user-emacs-directory))

;; Tern.JS
(add-to-list 'load-path "~/.emacs.d/site-lisp/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
;;(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
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




;; add css for org-mode export to html files
                                        ; Use org.css from the norang website for export document stylesheets
;;(setq org-html-head-extra "<link rel=\"stylesheet\" href=\"./org.css\" type=\"text/css\" />")
(setq org-html-head-extra "<style type=\"text/css\">body{margin:
1em; border-right: 5px solid #bbb; border-bottom: 5px solid #bbb;
padding: 0; background: #ddd none repeat scroll 0 0; border: 1px
solid #000; margin: 0; padding: 2em; color: #000;
font-family: \"Bitstream Vera Sans\", Verdana, sans-serif;
font-size: 85%;}code{color: #00f;}div#content{border: 1px solid
#bbb; background: #fff; margin: 0; padding: 2em;}a{color: #139;
text-decoration: none; padding: 1px;}a:hover{color:
#900;}#table-of-contents{margin: 1em 0; padding:
.1em;}div#content div#org-div-home-and-up{background: #369;
color: #fff;}div#org-div-home-and-up
a:link,div#org-div-home-and-up a:visited{color: #fff; background:
#369;}div#org-div-home-and-up a:hover{color:
#900;}div.title{margin: -1em -1em 0; font-size: 200%;
font-weight: bold; background: #369; color: #fff; padding: .75em
1em; font-family: \"BitStream Vera Sans\", Verdana; letter-spacing:
.1em;}h1{background: #369 none repeat scroll 0 0; color: #fff;
font-family: \"BitStream Vera Sans\", Verdana; font-size: 200%;
font-weight: bold; letter-spacing: 0.1em; margin: -1em -1em .2em;
padding: 0.75em 1em;}h2{font-size: 180%; border-bottom: 1px solid
#ccc; padding: .2em;}h3{font-size: 120%; border-bottom: 1px solid
#eee;}h4{font-size: 110%; border-bottom: 1px solid
#eee;}tt{color: #00f;}.verbatim{margin: .5em 0;}pre{border: 1px
solid #ccc; background: #eee; padding: .5em; overflow:
auto;}.verbatim pre{margin: 0;}.verbatim-caption{border: 1px
solid #ccc; border-bottom: 0; background: #fff; display: block;
font-size: 80%; padding: .2em;}div#postamble{text-align: left;
color: #888; font-size: 80%; padding: 0; margin: 0;}div#postamble
p{padding: 0; margin: 0;}div#postamble a{color:
#888;}div#postamble a:hover{color: #900;}table{font-size: 100%;
border-collapse: collapse; margin: .5em 0;}th, td{border: 1px
solid #777; padding: .3em; margin: 2px;}th{background:
#eee;}span.underline{text-decoration:
underline;}.fixme{background: #ff0; font-weight:
bold;}.ra{text-align: right;}</style>")


;; set parent node into DONE when all sub-tasks are done in org mode
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

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




(add-to-list 'load-path "~/Emacs/zencoding/")
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes



(tool-bar-mode 0)  
(menu-bar-mode 0)  
(scroll-bar-mode 0)

(provide 'init)
;;; init.el ends here

