(global-linum-mode 1)

;; Setting English Font
(set-face-attribute
   'default nil :font "Ubuntu Mono 12")
    
;; Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "文泉驿等宽微米黑" :size 14)))
(provide 'init-local)
