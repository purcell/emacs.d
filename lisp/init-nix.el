;;; init-nix.el --- Support for the Nix package manager -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(if (maybe-require-package 'nix-ts-mode)
    (when (and (fboundp 'treesit-ready-p) (treesit-ready-p 'nix t))
      (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode)))
  (maybe-require-package 'nix-mode))

(with-eval-after-load 'eglot
  ;; Prefer nixd to nil, and enable in nix-ts-mode too
  (add-to-list 'eglot-server-programs
               `((nix-mode nix-ts-mode) . ,(eglot-alternatives '("nixd" "nil")))))

(maybe-require-package 'nixpkgs-fmt)
(maybe-require-package 'nixfmt)

(provide 'init-nix)
;;; init-nix.el ends here
