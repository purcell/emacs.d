(require-package 'haml-mode)

(after-load 'haml-mode
  (define-key haml-mode-map (kbd "C-o") 'open-line)
  (when (fboundp 'electric-indent-mode)
    (add-hook 'haml-mode-hook (lambda () (electric-indent-mode -1)))))

(provide 'init-haml)
