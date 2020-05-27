;;; init-direnv.el --- Integrate with direnv -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun sanityinc/maybe-enable-envrc-global-mode ()
  (when (executable-find "direnv")
    (envrc-global-mode)))

(when (maybe-require-package 'envrc)
  (add-hook 'after-init-hook 'sanityinc/maybe-enable-envrc-global-mode))

(provide 'init-direnv)

;;; init-direnv.el ends here
