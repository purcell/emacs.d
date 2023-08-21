;;; init-nix.el --- Support for the Nix package manager -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (or (maybe-require-package 'nix-ts-mode)
          (maybe-require-package 'nix-mode))
  (maybe-require-package 'nixpkgs-fmt)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((nix-mode nix-ts-mode) . ("nil")))))

(provide 'init-nix)
;;; init-nix.el ends here
