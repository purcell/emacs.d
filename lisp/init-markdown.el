(when (maybe-require-package 'markdown-mode)
  (after-load 'whitespace-cleanup-mode
    (push 'markdown-mode whitespace-cleanup-mode-ignore-modes)))


(provide 'init-markdown)
