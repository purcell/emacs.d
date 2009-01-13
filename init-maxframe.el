(require 'maxframe)
(when *is-cocoa-emacs*
  (fset 'maximize-frame 'x-maximize-frame)
  (fset 'restore-frame 'x-restore-frame))
(when *macbook-pro-support-enabled*
  (setq mf-max-width 1440)
  (setq mf-max-height 900)
  (setq mf-offset-x 0))
(add-hook 'window-setup-hook 'maximize-frame t)

(defun maximized-p (&optional frame)
  (and (<= (abs (- (mf-max-display-pixel-width) (frame-pixel-width frame))) (frame-char-width frame))
       (<= (abs (- (mf-max-display-pixel-height) (+ mf-display-padding-height (frame-pixel-height frame)))) (+ 5 (frame-char-height frame)))))
