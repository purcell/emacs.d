(require-package 'dired+)

(setq diredp-hide-details-initially-flag nil)
(setq global-dired-hide-details-mode -1)

(after-load 'dired
  (require 'dired+)
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file))

(provide 'init-dired)
