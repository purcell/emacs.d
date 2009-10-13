(defvar preferred-javascript-mode 'js2-mode)
(defvar preferred-javascript-mode-hook 'js2-mode-hook)
(defvar preferred-mmm-javascript-mode 'js-mode)
(defvar preferred-javascript-indent-level 2)

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
(setq auto-mode-alist (cons `("\\.js$" . ,preferred-javascript-mode)
                            (loop for entry in auto-mode-alist
                                  unless (eq preferred-javascript-mode (cdr entry))
                                  collect entry)))



;; On-the-fly syntax checking
(autoload 'flymake-js-load "flymake-js" "On-the-fly syntax checking of javascript" t)
(unless (eq 'js2-mode-hook preferred-javascript-mode-hook)
  (add-hook preferred-javascript-mode-hook 'flymake-js-load))


;; js2-mode
(autoload 'js2-mode "js2-mode" "Steve Yegge's javascript IDE mode")
(setq js2-use-font-lock-faces t)
(setq js2-mode-must-byte-compile nil)
(setq js2-basic-offset preferred-javascript-indent-level)


;; js-mode
(autoload 'js-mode "js" "An enhanced version of javascript-mode")
(setq js-indent-level preferred-javascript-indent-level)


;; standard javascript-mode
(setq javascript-indent-level preferred-javascript-indent-level)


;; MMM submode regions in html
(eval-after-load "mmm-vars"
  `(progn
     (mmm-add-group
      'html-js
      '((js-script-cdata
         :submode ,preferred-mmm-javascript-mode
         :face mmm-code-submode-face
         :front "<script[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
         :back "[ \t]*\\(//\\)?]]>[ \t\n]*</script>"
         :insert ((?j js-tag nil @ "<script language=\"JavaScript\">"
                      @ "\n" _ "\n" @ "</script>" @)))
        (js-script
         :submode ,preferred-mmm-javascript-mode
         :face mmm-code-submode-face
         :front "<script[^>]*>[ \t]*\n?"
         :back "[ \t]*</script>"
         :insert ((?j js-tag nil @ "<script language=\"JavaScript\">"
                      @ "\n" _ "\n" @ "</script>" @)))
        (js-inline
         :submode ,preferred-mmm-javascript-mode
         :face mmm-code-submode-face
         :front "on\w+=\""
         :back "\"")))
     (dolist (mode (list 'html-mode 'nxml-mode))
       (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?$" 'html-js))))


(require 'js-comint)
(setq inferior-js-program-command "/opt/local/bin/js")
(add-hook preferred-javascript-mode-hook
          '(lambda ()
             (local-set-key "\C-x\C-e" 'js-send-last-sexp)
             (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
             (local-set-key "\C-cb" 'js-send-buffer)
             (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
             (local-set-key "\C-cl" 'js-load-file-and-go)
             ))


(provide 'init-javascript)
