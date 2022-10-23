;;; init-copilot.el --- Copilot support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(load-file "/Users/drmarshall/.emacs.d/site-lisp/copilot/copilot.el")

(when (maybe-require-package 'copilot)
  (require-package 'dash)
  (require-package 's)
  (require-package 'editorconfig)
  (require-package 'company)

  (with-eval-after-load 'copilot
    (add-hook 'prog-mode-hook 'copilot-mode))

  (with-eval-after-load 'company
    ;; disable inline previews
    (delq 'company-preview-if-just-one-frontend company-frontends))

  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)  )

(provide 'init-copilot)
;;; init-copilot.el ends here
