;;; 个人设置
;;设置个人信息
(setq user-full-name "Joseph Young")
(setq user-mail-address "2t@live.com")

;;; 显示菜单栏
(menu-bar-mode 1)

;; ;;光标不闪
;; (blink-cursor-mode -1)

;;取消警告声音
(setq visible-bell 0)

;;Emacs可以打开和显示图片
                                        ;(auto-image-file-mode)

;;页面在3行滚动
(setq scroll-margin 3
      scroll-conservatively 10000)

;;高亮当前行且显示行号
(global-set-key [f8] 'global-hl-line-mode)
;; (global-set-key [f8] 'h1AndG1mode)
;; (defun hlAndGlmode ()
;;   (interactive)
;;   (global-hl-line-mode 1)
;;   (global-linum-mode 1))

;; 显示行号
;; (global-set-key [f7] 'global-linum-mode)

;; ;;在标题栏显示buffer的名字
;; (setq frame-title-format "emacs@%b")

;; ;;sentence-end识别中文标点
;; (setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
;; (setq sentence-end-double-space nil)

;;wangyin的按%匹配括号,修改为按#号补全
(global-set-key (kbd "#") 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; ;;browse-kill-ring.el  可视化的killring
;; (require 'browse-kill-ring)
;; (global-set-key (kbd "C-c k") 'browse-kill-ring)
;; (browse-kill-ring-default-keybindings)

;;用一个很大的killring
;; (setq kill-ring-max 200)

;; ;;wangyin的临时记号－按C-.做记号，按C-,跳转
;; (global-set-key [(control ?\.)] 'ska-point-to-register)
;; (global-set-key [(control ?\,)] 'ska-jump-to-register)
;; (defun ska-point-to-register()
;;   "Store cursorposition _fast_ in a register.
;; Use ska-jump-to-register to jump back to the stored
;; position."
;;   (interactive)
;;   (setq zmacs-region-stays t)
;;   (point-to-register 8))
;; (defun ska-jump-to-register()
;;   "Switches between current cursorposition and position
;; that was stored with ska-point-to-register."
;;   (interactive)
;;   (setq zmacs-region-stays t)
;;   (let ((tmp (point-marker)))
;;     (jump-to-register 8)
;;     (set-register 8 tmp)))

;; ;;光标到下一个某字母
;; (defun wy-go-to-char (n char)
;;   "Move forward to Nth occurence of CHAR.
;; Typing `wy-go-to-char-key' again will move forwad to the next Nth
;; occurence of CHAR."
;;   (interactive "p\ncGo to char: ")
;;   (search-forward (string char) nil nil n)
;;   (while (char-equal (read-char)
;;                   char)
;;     (search-forward (string char) nil nil n))
;;   (setq unread-command-events (list last-input-event)))
;; (define-key global-map (kbd "C-c a") 'wy-go-to-char)

;; ;;ibuffer.el
;; (require 'ibuffer)
;; (global-set-key (kbd "C-x C-b") 'ibuffer)

;;系统剪切板
;; (global-set-key (kbd "C-c c") 'clipboard-kill-ring-save)
;; (global-set-key (kbd "C-c v") 'clipboard-yank)

;;利用自带的hippie-expand进行代码补全
(global-set-key[(meta ?/)] 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; ;;tabber.el list all buffers不开窗口就不启用
;; (if(not window-system) nil
;; (require 'tabbar)
;; (tabbar-mode)
;; (global-set-key (kbd "s-b") 'tabbar-backward-group)
;; (global-set-key (kbd "s-m") 'tabbar-forward-group)
;; (global-set-key (kbd "s-p") 'tabbar-backward)
;; (global-set-key (kbd "s-n") 'tabbar-forward)
;; )

;; ;; w3m设置，并显示图片
;; (require 'w3m-load)
;; (setq w3m-home-page "http://www.baidu.com")
;; (setq w3m-default-display-inline-images t)
;; (setq w3m-command-arguments '("-cookie" "-F"))
;; (setq w3m-use-cookies t)

;;在minibuffer显示时间
;; (display-time-mode 1)                   ;;启用时间显示设置
;; (setq display-time-24hr-format t)       ;;时间使用24小时制
;; (setq display-time-day-and-date t)      ;;时间显示包括日期和具体时间
;; (setq display-time-use-mail-icon t)     ;;时间栏旁边启用邮件设置
;; (setq display-time-interval 10)         ;;时间的变化频率
;; (setq display-time-format " %a, %d %b %H:%M ")

;;remember mode 设置
(autoload 'remember "remember" nil t)
(define-key global-map [f12] 'remember)
(setq remember-data-file "C:/Users/Joseph/OneDrive/文档/notes.org")
(global-set-key (kbd "C-<f12>") 'my-remmeber-file)
(defun my-remmeber-file()
  (interactive)
  (find-file "~/Dropbox/Documents/notes.org"))

;;自动添加括号
;; (electric-pair-mode t)
(yas-global-mode 1)
(require 'init-pyim)
(provide 'init-local)
