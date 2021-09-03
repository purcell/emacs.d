;;; my-coq.el --- Provide custom configurations
;;; Commentary:
;;; Code:

;; Coq
(require-package 'proof-general)
(require-package 'company-coq)
(require-package 'coq-commenter)
(add-hook 'coq-mode-hook #'company-coq-mode)
(add-hook 'coq-mode-hook 'coq-commenter-mode)

(provide 'my-coq)
;; End
