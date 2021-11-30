;;; my-company.el --- Provide custom configurations
;;; Commentary:
;;; Code:

;; Company
(require-package 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;; Company-clang
(setq company-backends (delete 'company-semantic company-backends))
;;(define-key c-mode-map  [(tab)] 'company-complete)
;;(define-key c++-mode-map  [(tab)] 'company-complete)

(provide 'my-company)
;; End
