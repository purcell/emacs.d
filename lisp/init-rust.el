(when (maybe-require-package 'rust-mode)
  (when (maybe-require-package 'racer)
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)))


(provide 'init-rust)
