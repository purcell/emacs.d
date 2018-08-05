(when (maybe-require-package 'projectile-rails)
  (add-hook 'projectile-mode-hook
            (lambda () (projectile-rails-global-mode projectile-mode))))


(provide 'init-rails)
