;;; my-ccpp.el --- Provide custom configurations
;;; Commentary:
;;; Code:

;; Tab setting
(defun my-c++-mode-hook ()
  (setq c-basic-offset 4)
  (setq c-indent-level 4)
  (setq tab-width 4)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(defun my-c-mode-hook ()
  (setq c-indent-level 4)
  (setq tab-width 4)
  (setq c-basic-offset 4))
(add-hook 'c-mode-hook 'my-c-mode-hook)

;; Fold
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; C LSP
(add-hook 'c-mode-hook #'lsp)
(require-package 'ccls)

;; CPP LSP
(add-hook 'c++-mode-hook #'lsp)

(provide 'my-ccpp)
;; End
