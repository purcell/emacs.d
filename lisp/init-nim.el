;;; init-nim.el --- Nim programming support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'nim-mode)
  (when (maybe-require-package 'flycheck-nim)
    (with-eval-after-load 'nim-mode
      (with-eval-after-load 'flycheck
        (require 'flycheck-nim)))))

(provide 'init-nim)

;;; init-nim.el ends here
