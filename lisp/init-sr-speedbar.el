;;; Package --- mode make SpeedBar show in Current Frame by SebastianRose.
(require-package 'sr-speedbar)
(require 'sr-speedbar)

(autoload 'sr-speedbar-toggle "sr-speedbar")

;; sr-speedbar config
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-skip-other-window-p t)
;; no auto-refresh
(setq sr-speedbar-auto-refresh nil)

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

(provide 'init-sr-speedbar)
