;;; skewer-setup.el --- automatic setup for the Skewer minor modes -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; This exists as a separate file so that Skewer need not be fully
;; loaded just to use this setup function.

;;; Code:

;;;###autoload
(defun skewer-setup ()
  "Fully integrate Skewer into js2-mode, css-mode, and html-mode buffers."
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

(provide 'skewer-setup)

;;; skewer-setup.el ends here
