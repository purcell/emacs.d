(when (maybe-require-package 'php-mode)
  (maybe-require-package 'smarty-mode)

  (when (maybe-require-package 'company-php)
    (after-load 'company
      (push 'company-ac-php-backend company-backends))))

(provide 'init-php)
