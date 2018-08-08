(when (maybe-require-package 'projectile)
  (add-hook 'after-init-hook 'projectile-mode)

  ;; Shorter modeline
  (setq-default projectile-mode-line-prefix " Proj")

  (after-load 'projectile
    (setq-default
     projectile-mode-line
     '(:eval
       (if (file-remote-p default-directory)
           " Proj"
         (format " Proj[%s]" (projectile-project-name)))))
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    )

  (maybe-require-package 'ibuffer-projectile))

(provide 'init-projectile)
