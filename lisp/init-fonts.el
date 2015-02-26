;;; Character sets

(defcustom sanityinc/force-default-font-for-symbols nil
  "When non-nil, force Emacs to use your default font for symbols."
  :type 'boolean)

(defun sanityinc/maybe-use-default-font-for-symbols ()
  "Force Emacs to render symbols using the default font, if so configured."
  (when sanityinc/force-default-font-for-symbols
    (set-fontset-font "fontset-default" 'symbol (face-attribute 'default :family))))

(add-hook 'after-init-hook 'sanityinc/maybe-use-default-font-for-symbols)


;;; Changing font sizes

(defun sanityinc/increment-default-font-height (delta)
  "Adjust the default font height by DELTA on every graphical frame.
The pixel size of the frame will be kept approximately the same,
to the extent possible, as with the function `set-frame-font'.
DELTA should be a multiple of 10, to match the units used by
the :height face attribute."
  (interactive "nIncrement (e.g. 10, -5)? ")
  (unless (display-multi-font-p (selected-frame))
    (error "Cannot be applied from a non-graphical frame"))
  (let* ((cur-height (face-attribute 'default :height))
         (new-height (+ cur-height delta)))
    ;; Modify the special "user" theme, which is always combined
    ;; with any other loaded theme(s).  An alternative approach
    ;; would be modifying the default face's face-override-spec
    ;; property (see `face-spec-set'), but that produces more
    ;; redraws
    (custom-push-theme 'theme-face 'default 'user 'set `((t (:height ,new-height))))
    (dolist (f (frame-list))
      (when (display-multi-font-p f)
        (let ((pixel-height (* (frame-parameter f 'height)
                               (frame-char-height f)))
              (pixel-width  (* (frame-parameter f 'width)
                               (frame-char-width f))))
          (face-spec-recalc 'default f)
          (unless (frame-parameter f 'fullscreen)
            (modify-frame-parameters
             f
             `((height . ,(round pixel-height (frame-char-height f)))
               (width . ,(round pixel-width  (frame-char-width f))))))))
      (with-selected-frame f
        (run-hooks 'after-setting-font-hook)))
    (message "Default font size is now %d" (/ new-height 10))))

(defun sanityinc/increase-default-font-height ()
  (interactive)
  (sanityinc/increment-default-font-height 10))

(defun sanityinc/decrease-default-font-height ()
  (interactive)
  (sanityinc/increment-default-font-height -10))

(global-set-key (kbd "C-M-=") 'sanityinc/increase-default-font-height)
(global-set-key (kbd "C-M--") 'sanityinc/decrease-default-font-height)



(provide 'init-fonts)
