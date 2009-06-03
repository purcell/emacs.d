(require 'maxframe)
(when *is-cocoa-emacs*
  (fset 'maximize-frame 'x-maximize-frame)
  (fset 'restore-frame 'x-restore-frame))
(when *macbook-pro-support-enabled*
  (setq mf-max-width 1440)
  (setq mf-max-height 900)
  (setq mf-offset-x 0))

(defun maybe-maximize-frame (&optional frame)
  (let* ((prev-frame (selected-frame))
         (f (or frame prev-frame)))
    (select-frame f)
    (prog1
        (if window-system (maximize-frame))
      (select-frame prev-frame))))

(add-hook 'after-make-frame-functions 'maybe-maximize-frame)
(add-hook 'after-init-hook 'maybe-maximize-frame)

(defun maximized-p (&optional frame)
  (and (<= (abs (- (mf-max-display-pixel-width) (frame-pixel-width frame))) (frame-char-width frame))
       (<= (abs (- (mf-max-display-pixel-height) (+ mf-display-padding-height (frame-pixel-height frame)))) (+ 5 (frame-char-height frame)))))


(provide 'init-maxframe)
