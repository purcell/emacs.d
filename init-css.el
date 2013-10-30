;; Colourise CSS colour literals
;; web-mode does not like rainbow-mode
(dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
  (add-hook hook 'rainbow-mode)
  )


(defun maybe-flymake-css-load ()
  "Activate flymake-css as necessary, but not in derived modes."
  (when (eq major-mode 'css-mode)
    (flymake-css-load)))
(add-hook 'css-mode-hook 'maybe-flymake-css-load)


(add-hook 'sass-mode-hook 'flymake-sass-load)
(add-hook 'scss-mode-hook 'flymake-sass-load)
(setq-default scss-compile-at-save nil)


(eval-after-load 'auto-complete
  '(progn
     (dolist (hook '(css-mode-hook sass-mode-hook scss-mode-hook))
       (add-hook hook 'ac-css-mode-setup))))

(provide 'init-css)
