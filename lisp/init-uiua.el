;;; init-uiua.el --- Support for the Uiua programming language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(if (maybe-require-package 'uiua-ts-mode)
    (when (and (fboundp 'treesit-ready-p) (treesit-ready-p 'uiua t))
      (add-to-list 'major-mode-remap-alist '(uiua-mode . uiua-ts-mode)))
  (maybe-require-package 'uiua-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((uiua-mode uiua-ts-mode) . ("uiua" "lsp"))))

(maybe-require-package 'nixpkgs-fmt)

(provide 'init-uiua)
;;; init-uiua.el ends here
