(when (maybe-require-package 'diff-hl)
  (add-hook 'prog-mode-hook 'diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'diff-hl-mode)

  (after-load 'diff-hl
    (define-key diff-hl-mode-map
      (kbd "<left-fringe> <mouse-1>")
      'diff-hl-diff-goto-hunk)))

(maybe-require-package 'browse-at-remote)

(provide 'init-vc)
