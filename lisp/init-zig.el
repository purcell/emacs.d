;;; init-zig.el --- Support for the Zig language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(if (and (maybe-require-package 'zig-ts-mode)
         (fboundp 'treesit-ready-p) (treesit-ready-p 'zig))
    (progn
      (add-to-list 'auto-mode-alist '("\\.\\(zig\\|zon\\)\\'" . zig-ts-mode))
      (with-eval-after-load 'eglot
        (add-to-list 'eglot-server-programs '(zig-ts-mode . ("zls")))))
  (require-package 'zig-mode))


(provide 'init-zig)
;;; init-zig.el ends here
