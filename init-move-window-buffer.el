;; http://tapoueh.org/emacs/switch-window.html
(global-set-key (kbd "C-x o") 'switch-window)

;; move window
(require 'window-numbering)
(custom-set-faces '(window-numbering-face ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold)))))
(window-numbering-mode 1)

;; buffer-move.el
(global-set-key (kbd "C-c C-b C-k")     'buf-move-up)
(global-set-key (kbd "C-c C-b C-j")   'buf-move-down)
(global-set-key (kbd "C-c C-b C-h")   'buf-move-left)
(global-set-key (kbd "C-c C-b C-l")  'buf-move-right)

(provide 'init-move-window-buffer)
