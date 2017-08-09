(when (maybe-require-package 'projectile-rails)
  (after-load 'projectile
    (add-hook 'projectile-mode-hook 'projectile-rails-on)
    (after-load 'guide-key
      (add-to-list 'guide-key/guide-key-sequence "C-c r"))))


(provide 'init-rails)
