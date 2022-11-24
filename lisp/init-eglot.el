;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; For Emacs >= 27
(setq read-process-output-max (* 1024 1024))


(when (maybe-require-package 'eglot)
  (maybe-require-package 'consult-eglot))

(with-eval-after-load 'eglot
  (add-hook 'prog-mode-hook 'eglot-ensure))

(provide 'init-eglot)
;;; init-eglot.el ends here
