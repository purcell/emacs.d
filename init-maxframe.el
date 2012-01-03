(require 'maxframe)
(when *is-cocoa-emacs*
  (fset 'maximize-frame 'x-maximize-frame)
  (fset 'restore-frame 'x-restore-frame))
(when *macbook-pro-support-enabled*
  (setq mf-max-width 1440
        mf-max-height 894
        mf-display-padding-width 4
        mf-offset-x 0
        mf-display-padding-height (if ns-auto-hide-menu-bar
                                      23
                                    (+ 22 23))))

(defun maybe-maximize-frame (&optional frame)
  (with-selected-frame frame
    (if window-system (maximize-frame))))

(add-hook 'after-make-frame-functions 'maybe-maximize-frame)
(add-hook 'after-init-hook 'maybe-maximize-frame)

(defun maximized-p (&optional frame)
  (or (not (with-selected-frame frame window-system))
      (and (<= (abs (- (mf-max-display-pixel-width) (frame-pixel-width frame))) (frame-char-width frame))
           (<= (abs (- (mf-max-display-pixel-height) (+ mf-display-padding-height (frame-pixel-height frame)))) (+ 5 (frame-char-height frame))))))


(provide 'init-maxframe)
