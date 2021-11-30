;;; my-ts.el --- Provide custom configurations
;;; Commentary:
;;; Code:


(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 4)))

;; TypeScript
(require-package 'tide)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; Typescript LSP
(add-hook 'typescript-mode-hook #'lsp)

;; Typescript LSP
(add-hook 'typescript-mode-hook #'lsp)
;; Angular LSP
(setq lsp-clients-angular-language-server-command
      '("node"
        "/usr/local/lib/node_modules/@angular/language-server"
        "--ngProbeLocations"
        "/usr/local/lib/node_modules"
        "--tsProbeLocations"
        "/usr/local/lib/node_modules"
        "--stdio"))

(provide 'my-ts)
;; End
