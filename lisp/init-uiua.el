;;; init-uiua.el --- Support for the Uiua programming language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(if (maybe-require-package 'uiua-ts-mode)
    (progn
      ;; TODO: handle duplication w.r.t. nix-ts-mode
      (defun sanityinc/set-uiua-ts-auto-mode ()
        (when (and (fboundp 'treesit-ready-p)
                   (treesit-ready-p 'uiua t)
                   (fboundp 'uiua-ts-mode))
          (add-to-list 'auto-mode-alist '("\\.ua\\'" . uiua-ts-mode))))
      (add-hook 'after-init-hook 'sanityinc/set-uiua-ts-auto-mode))
  (maybe-require-package 'uiua-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((uiua-mode uiua-ts-mode) . ("uiua" "lsp"))))

(maybe-require-package 'nixpkgs-fmt)

(provide 'init-uiua)
;;; init-uiua.el ends here
