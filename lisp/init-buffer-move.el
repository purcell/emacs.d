;;; Package --- Move buffer (Not move cursor)
(require-package 'buffer-move)
(require 'buffer-move)

;; buffer-move.el
(global-set-key (kbd "C-c C-b <up>")     'buf-move-up)
(global-set-key (kbd "C-c C-b <down>")   'buf-move-down)
(global-set-key (kbd "C-c C-b <left>")   'buf-move-left)
(global-set-key (kbd "C-c C-b <right>")  'buf-move-right)

(provide 'init-buffer-move)
