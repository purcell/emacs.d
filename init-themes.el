(defcustom window-system-color-theme 'color-theme-sanityinc-solarized-dark
  "Color theme to use in window-system frames"
  :type 'symbol)

(defcustom tty-color-theme 'color-theme-terminal
  "Color theme to use in TTY frames"
  :type 'symbol)


(defun color-theme-terminal ()
  (interactive)
  (color-theme-sanityinc-solarized-dark))


(defun apply-best-color-theme-for-frame-type (frame)
  (with-selected-frame frame
    (if window-system
      (funcall window-system-color-theme)
      (funcall tty-color-theme))))

(defun reapply-color-themes ()
  (interactive)
  (mapcar 'apply-best-color-theme-for-frame-type (frame-list)))

(defun light ()
  (interactive)
  (setq window-system-color-theme 'color-theme-sanityinc-solarized-light)
  (reapply-color-themes))

(defun dark ()
  (interactive)
  (setq window-system-color-theme 'color-theme-sanityinc-solarized-dark)
  (reapply-color-themes))

(set-variable 'color-theme-is-global nil)
(add-hook 'after-make-frame-functions 'apply-best-color-theme-for-frame-type)
(add-hook 'after-init-hook 'reapply-color-themes)
(apply-best-color-theme-for-frame-type (selected-frame))

(provide 'init-themes)
