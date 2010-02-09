(add-hook 'css-mode-hook 'ac-css-keywords-setup)

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
;; Inspired by http://xahlee.org/emacs/emacs_html.html, but much extended

(require 'hexl)
(defun choose-contrasting-colour (hex-colour)
  (if (> (hexl-hex-string-to-integer (substring hex-colour 1))
         (hexl-hex-string-to-integer "777777"))
      "#000000"
    "#ffffff"))

(defun three-hex-colour-to-six (three)
  (let ((six "#"))
    (progn
      (dolist (c (string-to-list (substring three 1)))
        (setq six (concat six (string c c))))
      six)))

(defun hexcolour-font-lock-face (hexcolour)
  (list :background hexcolour :foreground (choose-contrasting-colour hexcolour)))

(setq hexcolour-keywords
  '(("\\b#[abcdef[:digit:]]\\{3\\}\\b"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (hexcolour-font-lock-face
                (three-hex-colour-to-six (match-string-no-properties 0))))))
    ("\\b#[abcdef[:digit:]]\\{6\\}\\b"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (hexcolour-font-lock-face
                (match-string-no-properties 0)))))))

(defun hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolour-keywords))

(add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)
(add-hook 'html-mode-hook 'hexcolour-add-to-font-lock)
(add-hook 'sass-mode-hook 'hexcolour-add-to-font-lock)


(provide 'init-css)
