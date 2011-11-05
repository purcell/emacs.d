(eval-after-load "viper"
  '(progn
     ;; C-z is usually 'iconify-or-deiconify-frame, but viper uses it to toggle
     ;; vi/emacs input modes, causing confusion in non-viper buffers

     (global-unset-key "\C-z")
     (setq viper-mode t)
     (setq viper-custom-file-name (convert-standard-filename "~/.emacs.d/.viper"))
     (define-key viper-insert-global-user-map [kp-delete] 'viper-delete-char)
     (define-key viper-insert-global-user-map (kbd "C-n") 'dabbrev-expand)
     (define-key viper-insert-global-user-map (kbd "C-p") 'dabbrev-expand)

     ;; Stop C-u from clobbering prefix-arg -- I always use C-b/C-f to scroll

     (define-key viper-vi-basic-map "\C-u" nil)

     ;; Vim-style searching of the symbol at point, made easy by highlight-symbol

     (autoload 'highlight-symbol-next "highlight-symbol" "Highlight symbol at point")
     (autoload 'highlight-symbol-prev "highlight-symbol" "Highlight symbol at point")
     (setq highlight-symbol-on-navigation-p t)
     (define-key viper-vi-global-user-map "*" 'highlight-symbol-next)
     (define-key viper-vi-global-user-map "#" 'highlight-symbol-prev)))


;; Work around a problem in Cocoa emacs, wherein setting the cursor coloring
;; is incredibly slow; viper sets the cursor very frequently in insert mode
(when *is-cocoa-emacs*
  (eval-after-load "viper"
    '(defun viper-change-cursor-color (new-color &optional frame))))


(provide 'init-viper)
