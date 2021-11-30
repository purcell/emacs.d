;;; init-direnv.el --- Integrate with direnv -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'envrc)
  (defun sanityinc/maybe-enable-envrc-global-mode ()
    "Enable `envrc-global-mode' if `direnv' is installed."
    (when (executable-find "direnv")
      (envrc-global-mode)))

  (with-eval-after-load 'envrc
    (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map))
  (add-hook 'after-init-hook 'sanityinc/maybe-enable-envrc-global-mode))

(provide 'init-direnv)

;;; init-direnv.el ends here
