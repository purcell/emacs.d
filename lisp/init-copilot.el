;;; init-copilot.el --- Copilot support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(load-file "/Users/drmarshall/.emacs.d/site-lisp/copilot/copilot.el")

;;(maybe-require-package 'copilot)
(maybe-require-package 'dash)
(maybe-require-package 's)
(maybe-require-package 'editorconfig)
(maybe-require-package 'company)

(add-hook 'prog-mode-hook 'copilot-mode)

(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))

(defun my/copilot-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (indent-for-tab-command)))

(with-eval-after-load 'copilot
  (define-key copilot-mode-map (kbd "<tab>") #'my/copilot-tab))
(define-key copilot-completion-map (kbd "C-c C-a") 'copilot-accept-completion)

(provide 'init-copilot)
;;; init-copilot.el ends here
