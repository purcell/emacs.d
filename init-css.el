(eval-after-load "mmm-vars"
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
       (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?$" 'html-css))))



;; Colourise CSS colour literals

(add-hook 'css-mode-hook 'rainbow-turn-on)
(add-hook 'html-mode-hook 'rainbow-turn-on)
(add-hook 'sass-mode-hook 'rainbow-turn-on)

(provide 'init-css)
