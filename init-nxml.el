(load-library "rng-auto")
(add-to-list 'auto-mode-alist
              (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
                    'nxml-mode))
(setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
(fset 'html-mode 'nxml-mode)
(fset 'xml-mode 'nxml-mode)
(add-hook 'nxml-mode-hook (lambda ()
                            (set (make-local-variable 'ido-use-filename-at-point) nil)))
(setq nxml-slash-auto-complete-flag t)


;; There are newer schema files here: http://syntax.whattf.org//relaxng/
(eval-after-load 'rng-loc
  '(progn
     (require 'whattf-dt)
     (push (expand-file-name "schemas.xml" (directory-of-library "whattf-dt"))
           rng-schema-locating-files)))


;;----------------------------------------------------------------------------
;; Integration with tidy for html + xml
;;----------------------------------------------------------------------------
(add-hook 'nxml-mode-hook (lambda () (tidy-build-menu nxml-mode-map)))
(add-hook 'html-mode-hook (lambda () (tidy-build-menu html-mode-map)))


(add-auto-mode 'html-mode "\\.(jsp|tmpl)$")


(provide 'init-nxml)
