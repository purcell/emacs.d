;;; init-windows.el --- Working with windows within frames -*- lexical-binding: t -*-
2;;; Commentary:

;; This is not about the "Windows" OS, but rather Emacs's "windows"
;; concept: these are the panels within an Emacs frame which contain
;; buffers.

;;; Code:

(require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)


(provide 'init-windows)
;;; init-windows.el ends here
