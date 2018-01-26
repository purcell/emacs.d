(when (maybe-require-package 'projectile-rails)
  (add-hook 'projectile-mode-hook
            (lambda () (projectile-rails-global-mode projectile-mode)))
  (after-load 'projectile
    (after-load 'guide-key
      (add-to-list 'guide-key/guide-key-sequence "C-c r")
      (add-to-list 'guide-key/guide-key-sequence "C-c r !")
      (add-to-list 'guide-key/guide-key-sequence "C-c r g"))))


(provide 'init-rails)
