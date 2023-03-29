;;; init-nix.el --- Support for the Nix package manager -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'nix-mode)
  (maybe-require-package 'nixpkgs-fmt)
  (maybe-require-package 'nix-sandbox)
  (maybe-require-package 'nix-buffer)

  (when (maybe-require-package 'nixos-options)
    ;; TODO: write a CAPF backend based on company-nixos-options
    ))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((nix-mode) . ("nil"))))

(provide 'init-nix)
;;; init-nix.el ends here
