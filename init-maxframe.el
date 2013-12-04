(require-package 'maxframe)

(autoload 'mf-max-display-pixel-width "maxframe" "" nil)
(autoload 'mf-max-display-pixel-height "maxframe" "" nil)
(autoload 'maximize-frame "maxframe" "" t)
(autoload 'restore-frame "maxframe" "" t)

(when *is-a-mac*
  (after-load 'maxframe
    (fset 'maximize-frame 'x-maximize-frame)
    (fset 'restore-frame 'x-restore-frame))
  (setq mf-display-padding-width 4
        mf-offset-x 0
        mf-offset-y 0
        mf-display-padding-height (if (when (boundp 'ns-auto-hide-menu-bar)
                                        ns-auto-hide-menu-bar)
                                      23
                                    (+ 27 23))))

(defvar sanityinc/prev-frame nil "The selected frame before invoking `make-frame-command'.")
(defadvice make-frame-command (before sanityinc/note-previous-frame activate)
  "Record the selected frame before creating a new one interactively."
  (setq sanityinc/prev-frame (selected-frame)))

(defun sanityinc/maybe-maximize-frame (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (when (and window-system
               sanityinc/prev-frame
               (sanityinc/maximized-p sanityinc/prev-frame))
      (maximize-frame))))

(add-hook 'after-make-frame-functions 'sanityinc/maybe-maximize-frame)
(add-hook 'after-init-hook 'sanityinc/maybe-maximize-frame)

(defadvice maximize-frame (around skip-if-fullscreen (&optional frame) activate)
  (unless (sanityinc/maximized-p frame)
    ad-do-it))

(defadvice restore-frame (around skip-if-fullscreen (&optional frame) activate)
  (when (sanityinc/maximized-p frame)
    ad-do-it))

(defun within-p (a b delta)
  (<= (abs (- b a)) delta))

(defun sanityinc/maximized-p (&optional frame)
  (or (not (with-selected-frame (or frame (selected-frame)) window-system))
      (eq 'fullboth (frame-parameter frame 'fullscreen))
      (and (within-p (mf-max-display-pixel-width)
                     (frame-pixel-width frame)
                     (frame-char-width frame))
           (within-p (mf-max-display-pixel-height)
                     (+ mf-display-padding-height (frame-pixel-height frame))
                     (frame-char-height frame)))))


(provide 'init-maxframe)
