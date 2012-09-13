(when (and *is-a-mac* window-system)
  (exec-path-from-shell-initialize))

(provide 'init-exec-path)
