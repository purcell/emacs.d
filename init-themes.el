(require 'color-theme-autoloads)
(autoload 'color-theme-zenburn "zenburn" "A low contrast color theme" t)
(autoload 'color-theme-twilight "color-theme-twilight" "A dark color theme" t)
(autoload 'color-theme-vivid-chalk "vivid-chalk" "A dark color theme" t)
(color-theme-initialize)
;; (color-theme-pierson) ; Light, favourite
;; (color-theme-high-contrast)
;; (color-theme-snowish)
;; (color-theme-marquardt)
;; (color-theme-clarity) ; dark
;; (color-theme-dark-laptop) ; dark
;; (color-theme-billw) ; dark
;; (color-theme-oswald) ; dark
;; (color-theme-zenburn) ; dark, low contrast
;; (color-theme-standard)

(defun light-colors ()
  (interactive)
  (preserving-default-font-size
   (color-theme-pierson)))

(defun high-contrast ()
  (interactive)
  (preserving-default-font-size
   (color-theme-vivid-chalk)
   (set-face-attribute 'highlight nil :background "white" :foreground "black")))

(defun low-contrast ()
  (interactive)
  (preserving-default-font-size
   (color-theme-zenburn)))

(defun medium-contrast ()
  (interactive)
  (preserving-default-font-size
   (color-theme-taylor)))

(defun very-low-contrast ()
  (interactive)
  (preserving-default-font-size
   (color-theme-zenburn)
   (set-face-attribute 'default nil :background "#1f1f1f")))

(high-contrast)



(provide 'init-themes)
