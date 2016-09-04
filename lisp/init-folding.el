(when (maybe-require-package 'origami)
  (add-hook 'prog-mode-hook 'origami-mode)
  (after-load 'origami
    (define-key origami-mode-map (kbd "C-c f") 'origami-recursively-toggle-node)
    (define-key origami-mode-map (kbd "C-c F") 'origami-toggle-all-nodes)))


(provide 'init-folding)
