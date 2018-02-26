(when (maybe-require-package 'rust-mode)
  (when (maybe-require-package 'racer)
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)))

(when (maybe-require-package 'flycheck-rust)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'init-rust)
