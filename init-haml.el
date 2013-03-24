(require-package 'haml-mode)
(require-package 'flymake-haml)

(defun maybe-flymake-haml-load ()
  "Activate flymake-haml as necessary, but not in derived modes."
  (when (eq major-mode 'haml-mode)
    (flymake-haml-load)))

(add-hook 'haml-mode-hook 'maybe-flymake-haml-load)

(provide 'init-haml)
