(require-package 'ledger-mode)

(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

(when (maybe-require-package 'flycheck-ledger)
  (after-load 'flycheck
    (after-load 'ledger-mode
      (require 'flycheck-ledger))))

(after-load 'ledger-mode
  (define-key ledger-mode-map (kbd "RET") 'newline)
  (define-key ledger-mode-map (kbd "C-o") 'open-line))

(setq ledger-highlight-xact-under-point nil
      ledger-use-iso-dates nil)

(after-load 'ledger-mode
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-copy-env "LEDGER_FILE")))

(add-hook 'ledger-mode-hook 'goto-address-prog-mode)

(provide 'init-ledger)
