;; ;; Can't get this to work...
;; (eval-after-load "mmm-mode"
;;   '(progn
;;      (load-library "javascript")
;;      (load-library "css-mode")
;;      (require 'mmm-sample)
;;      (add-to-list 'mmm-mode-ext-classes-alist '(nxml-mode nil html-js))
;;      (add-to-list 'mmm-mode-ext-classes-alist '(nxml-mode nil embedded-css))))
(autoload 'flymake-js-load "flymake-js" "On-the-fly syntax checking of javascript" t)
(add-hook 'javascript-mode-hook '(lambda () (flymake-js-load)))

;; Spiffy new js2-mode from Steve Yegge (http://code.google.com/p/js2-mode/)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-use-font-lock-faces t)
(setq js2-mode-must-byte-compile nil)

(eval-after-load "mmm-mode"
  '(progn
     (mmm-add-group
      'html-js2
      '((js-script-cdata
         :submode js2-mode
         :face mmm-code-submode-face
         :front "<script[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
         :back "[ \t]*\\(//\\)?]]>[ \t\n]*</script>"
         :insert ((?j js-tag nil @ "<script language=\"JavaScript\">"
                      @ "\n" _ "\n" @ "</script>" @)))
        (js-script
         :submode js2-mode
         :face mmm-code-submode-face
         :front "<script[^>]*>[ \t]*\n?"
         :back "[ \t]*</script>"
         :insert ((?j js-tag nil @ "<script language=\"JavaScript\">"
                      @ "\n" _ "\n" @ "</script>" @)))
        (js-inline
         :submode js2-mode
         :face mmm-code-submode-face
         :front "on\w+=\""
         :back "\"")))
     (mmm-add-mode-ext-class 'nxml-mode "\\.r?html\\(\\.erb\\)?$" 'html-js2)))
