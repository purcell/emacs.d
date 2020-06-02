;;; init-nim.el --- Nim programming support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'nim-mode)
  (when (maybe-require-package 'flycheck-nim)
    (after-load 'nim-mode
      (after-load 'flycheck
        (require 'flycheck-nim)))))

(provide 'init-nim)

;;; init-nim.el ends here
