(autoload 'sr-speedbar-toggle "sr-speedbar")

;; sr-speedbar config
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-skip-other-window-p t)
(setq sr-speedbar-auto-refresh t)

(setq sr-speedbar-width-console 30)
(setq sr-speedbar-width-x 30)

;; regular speedbar config
(setq speedbar-show-unknown-files t)
(setq speedbar-verbosity-level 0)
;(setq speedbar-use-images nil)

(defun speedbar-edit-line-and-switch-to-window ()
  (interactive)
  (speedbar-edit-line)
  (other-window 1))

(global-set-key (kbd "C-c j") 'sr-speedbar-toggle)
(provide 'init-sr-speedbar)
