;; @see http://orgmode.org/worg/org-contrib/org-mime.html
;; demo video: http://vimeo.com/album/1970594/video/13158054
(autoload 'org-mime-htmlize "org-mime" nil t)
(autoload 'org-mime-org-buffer-htmlize "org-mime" nil t)

(add-hook 'message-mode-hook
          (lambda ()
            (local-set-key "\C-c\M-o" 'org-mime-htmlize)))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\C-c\M-o" 'org-mime-org-buffer-htmlize)))

(add-hook 'org-mime-html-hook
          (lambda ()
            (org-mime-change-element-style
             "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                           "#E6E1DC" "#232323"))
            (org-mime-change-element-style
             "blockquote" "border-left: 2px solid gray; padding-left: 4px;")
            ))

(provide 'init-org-mime)
