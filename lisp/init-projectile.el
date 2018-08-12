(when (maybe-require-package 'projectile)
  (add-hook 'after-init-hook 'projectile-mode)

  (setq projectile-keymap-prefix (kbd "C-c C-p"))

  ;; Shorter modeline
  (after-load 'projectile
    (setq-default
     projectile-mode-line
     '(:eval
       (if (file-remote-p default-directory)
           " Proj"
         (format " Proj[%s]" (projectile-project-name)))))))


(provide 'init-projectile)
