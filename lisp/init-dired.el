(require-package 'dired+)

(setq diredp-hide-details-initially-flag nil)

(after-load 'dired
  (require 'dired+)
  (when (fboundp 'global-dired-hide-details-mode)
    (global-dired-hide-details-mode -1))
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file))

(provide 'init-dired)
