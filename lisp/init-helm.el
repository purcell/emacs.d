(when (maybe-require-package 'helm)
  (when (maybe-require-package 'helm-smex)
    (global-set-key [remap execute-extended-command] #'helm-smex)
    (global-set-key (kbd "M-X") #'helm-smex-major-mode-commands)))

(provide 'init-helm)
