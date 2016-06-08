;;; package --- Summary
;;; Commentary:
;;; Code:
(add-to-list 'load-path "/home/justin/.emacs.d/site-lisp/")
(require 'go-mode-autoloads)

(set-language-environment 'utf-8)
;; Set Default Font
;(add-to-list 'default-frame-alist '(font .  "WenQuanYi Micro Hei Mono-24" ))
;(add-to-list 'default-frame-alist '(font .  "Source Code Pro-20" ))
(add-to-list 'default-frame-alist '(font .  "DejaVu Sans Mono-18" ))

;;; Enable line number.
(global-linum-mode t)
;; Disable menubar
(menu-bar-mode nil)

;; set folder for auto-save
(setq
   backup-by-copying t ; 自动备份
   backup-directory-alist '(("." . "~/.saves")) ; 自动备份在目录"~/.saves"下
   delete-old-versions t ; 自动删除旧的备份文件
   kept-new-versions 6 ; 保留最近的6个备份文件
   kept-old-versions 2 ; 保留最早的2个备份文件
   version-control t) ; 多次备份

(add-to-list 'exec-path "/usr/local/bin")
(setenv "GOPATH" (concat "/usr/local/go" ":" "/home/justin/workspace/golang"))
(setenv "PATH" (concat (getenv "PATH") ":" "/usr/local/bin" ":" "/usr/local/go/bin" ":" "/Users/justin/bin"))
(add-hook 'before-save-hook 'gofmt-before-save)

;; use Meta-Return keys on Mac OS X to toggle maxmize full screen.
(when *is-a-mac*
  (global-set-key (kbd "<M-RET>") 'toggle-frame-fullscreen))

;; use C-tab to do auto complete
;; (ac-set-trigger-key "TAB")
;; (ac-set-trigger-key "<tab>")
;; Auto complete
(global-auto-complete-mode nil)
; Start auto-completion after 2 characters of a word
(setq ac-auto-start 2)
; case sensitivity is important when finding matches
(setq ac-ignore-case nil)

;; Use Ctr-H to do backspace
(global-set-key [(control h)] 'delete-backward-char)
;; Disable Shift-Space key
(global-set-key (kbd "S-SPC") nil)

;; turn on soft wrapping mode for org mode
(add-hook 'org-mode-hook
  (lambda () (setq truncate-lines nil)))

;; Enable yasnippet globally.
(require 'yasnippet)
(yas-global-mode 1)
;; use Ctrl-TAB to trigger yas
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)

;; Find File At Point
(setq ido-use-filename-at-point 'guess)
;; Set default tab width is 4
(setq-default tab-width 4)

(put 'dired-find-alternate-file 'disabled nil)

;; js2-mode
(custom-set-variables
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 )

;; Default indentation is usually 2 spaces, changing to 4.
(add-hook 'html-mode-hook
          (lambda () (set (make-local-variable 'sgml-basic-offset) 4)))

(provide 'init-local)
;;; init-local.el ends here
