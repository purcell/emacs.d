(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(defun moz-custom-setup ()
  (moz-minor-mode 1)
  )
(add-hook 'javascript-mode-hook 'moz-custom-setup)
(add-hook 'js2-mode-hook 'moz-custom-setup)
(add-hook 'html-mode-hook 'moz-custom-setup)

(eval-after-load 'moz
  '(progn
     (global-set-key (kbd "C-x p")
                     (lambda ()
                       (interactive)
                       (comint-send-string (inferior-moz-process)
                                           "BrowserReload();")))
     ))
(provide 'init-moz)
