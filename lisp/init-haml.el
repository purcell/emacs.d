;;; init-haml.el --- Haml template support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'haml-mode)

(with-eval-after-load 'haml-mode
  (define-key haml-mode-map (kbd "C-o") 'open-line))

(provide 'init-haml)
;;; init-haml.el ends here
