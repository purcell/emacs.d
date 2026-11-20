;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(when (maybe-require-package 'eglot)
  (setq-default eglot-extend-to-xref t)
  (with-eval-after-load 'eglot
    ;; Eglot 1.26 moved code action indicators into the `refactor'
    ;; package, making `eglot-code-action-*' obsolete aliases.  Its
    ;; `refactor-suggestion-mode' always shows the mode-line indicator.
    (if (boundp 'refactor-indicator)
        (setq refactor-indicator "✓"
              refactor-indications '(eldoc-hint))
      (setq eglot-code-action-indicator "✓"
            eglot-code-action-indications '(eldoc-hint mode-line))))
  (defun sanityinc/disable-eglot-semantic-tokens ()
    (eglot-semantic-tokens-mode -1))
  (add-hook 'eglot-managed-mode-hook #'sanityinc/disable-eglot-semantic-tokens)
  (maybe-require-package 'consult-eglot))



(provide 'init-eglot)
;;; init-eglot.el ends here
