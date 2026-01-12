;; windows-config.el - windows specific settings  -*- lexical-binding: t; -*-

;; Windows paths with forward slashes
(setq default-directory "c:/Users/xxx/")

;; Default browser on Windows
(setq browse-url-browser-function 'browse-url-default-windows-browser)

;; Fix performance issues on Windows
(setq w32-get-true-file-attributes nil)
(setq inhibit-compacting-font-caches t)

;; Set cursor color
(set-face-attribute 'cursor nil :background "#d00000")

(provide 'init-local-windows)
