;;; init-nix.el --- Support for the Nix package manager -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(if (maybe-require-package 'nix-ts-mode)
    (progn
      (defun sanityinc/set-nix-ts-auto-mode ()
        (when (and (fboundp 'treesit-ready-p)
                   (treesit-ready-p 'nix t)
                   (fboundp 'nix-ts-mode))
          (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))))
      (add-hook 'after-init-hook 'sanityinc/set-nix-ts-auto-mode))
  (maybe-require-package 'nix-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((nix-mode nix-ts-mode) . ("nil"))))

(maybe-require-package 'nixpkgs-fmt)

(provide 'init-nix)
;;; init-nix.el ends here
