(defvar *window-system-color-theme* 'color-theme-sanityinc-solarized-dark
  "Color theme to use in window-system frames")
(defvar *tty-color-theme* 'color-theme-terminal
  "Color theme to use in TTY frames")


(require 'color-theme-autoloads)
(autoload 'color-theme-vivid-chalk "vivid-chalk" "A dark color theme" t)
(autoload 'color-theme-sanityinc-light "color-theme-sanityinc" "A light color theme" t)
(autoload 'color-theme-sanityinc-dark "color-theme-sanityinc" "A dark color theme" t)
(autoload 'color-theme-sanityinc-solarized-light "color-theme-sanityinc-solarized" "A light color theme" t)
(autoload 'color-theme-sanityinc-solarized-dark "color-theme-sanityinc-solarized" "A dark color theme" t)

(color-theme-initialize)

(defun color-theme-terminal ()
  (interactive)
  (color-theme-sanityinc-solarized-dark))


(defun apply-best-color-theme-for-frame-type (frame)
  (let ((prev-frame (selected-frame)))
    (select-frame frame)
    (prog1
        (if window-system
            (preserving-default-font-size
             (funcall *window-system-color-theme*))
          (funcall *tty-color-theme*))
      (select-frame prev-frame))))

(defun reapply-color-themes ()
  (interactive)
  (mapcar 'apply-best-color-theme-for-frame-type (frame-list)))

(defun light ()
  (interactive)
  (setq *window-system-color-theme* 'color-theme-sanityinc-solarized-light)
  (reapply-color-themes))

(defun dark ()
  (interactive)
  (setq *window-system-color-theme* 'color-theme-sanityinc-solarized-dark)
  (reapply-color-themes))

(set-variable 'color-theme-is-global nil)
(add-hook 'after-make-frame-functions 'apply-best-color-theme-for-frame-type)
(apply-best-color-theme-for-frame-type (selected-frame))

(provide 'init-themes)
