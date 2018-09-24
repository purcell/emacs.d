(when (maybe-require-package 'projectile)
  (add-hook 'after-init-hook 'projectile-mode)

  (after-load 'projectile
    (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)

    ;; Shorter modeline
    (setq-default projectile-mode-line-lighter " Proj"))

  (maybe-require-package 'ibuffer-projectile))


(provide 'init-projectile)
