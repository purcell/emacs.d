;;; init-direnv.el --- Integrate with direnv -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'envrc)
  (with-eval-after-load 'envrc
    (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map))
  (add-hook 'after-init-hook 'envrc-global-mode))

(provide 'init-direnv)

;;; init-direnv.el ends here
