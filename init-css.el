(eval-after-load 'mmm-vars
  '(progn
     (mmm-add-group
      'html-css
      '((css-cdata
         :submode css-mode
         :face mmm-code-submode-face
         :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
         :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>"
         :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                      @ "\n" _ "\n" @ "</script>" @)))
        (css
         :submode css-mode
         :face mmm-code-submode-face
         :front "<style[^>]*>[ \t]*\n?"
         :back "[ \t]*</style>"
         :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                      @ "\n" _ "\n" @ "</style>" @)))
        (css-inline
         :submode css-mode
         :face mmm-code-submode-face
         :front "style=\""
         :back "\"")))
     (dolist (mode (list 'html-mode 'nxml-mode))
       (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css))))



;; Colourise CSS colour literals
(dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
  (add-hook hook 'rainbow-mode))


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
