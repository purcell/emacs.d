;;; init-rails.el --- Ruby on Rails support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'projectile-rails)
  (add-hook 'projectile-mode-hook
            (lambda () (projectile-rails-global-mode projectile-mode))))


(provide 'init-rails)
;;; init-rails.el ends here
