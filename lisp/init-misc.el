;;----------------------------------------------------------------------------
;; Misc config - yet to be placed in separate files
;;----------------------------------------------------------------------------
(add-auto-mode 'tcl-mode "Portfile\\'")
(fset 'yes-or-no-p 'y-or-n-p)

(dolist (hook (if (fboundp 'prog-mode)
                  '(prog-mode-hook ruby-mode-hook)
                '(find-file-hooks)))
  (add-hook hook 'goto-address-prog-mode))
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(setq goto-address-mail-face 'link)

(setq-default regex-tool-backend 'perl)

(add-auto-mode 'conf-mode "Procfile")

;;----------------------------------------------------------------------------
;; Some great functions for finding the matching parenthesis
;;----------------------------------------------------------------------------
(defun goto-match-paren (arg)
  "Go to the matching if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)))

(defun goto-matching-ruby-block (arg)
  (cond
   ;; are we at an end keyword?
   ((equal (current-word) "end")
    (ruby-beginning-of-block))
   ;; or are we at a keyword itself?
   ((string-match (current-word) "\\(for\\|while\\|until\\|if\\|class\\|module\\|case\\|unless\\|def\\|begin\\|do\\)")
    (ruby-end-of-block))))


(defun dispatch-goto-matching (arg)
  "Go to the matching if on a parenthesis or something similar, similar to vi style of % "
  (interactive "p")
  (if (or
       (looking-at "[\[\(\{]")
       (looking-at "[\]\)\}]")
       (looking-back "[\[\(\{]" 1)
       (looking-back "[\]\)\}]" 1))

      (goto-match-paren arg)

    (when (eq major-mode 'ruby-mode)
      (goto-matching-ruby-block arg))))

(global-set-key "\M--" 'dispatch-goto-matching)

(provide 'init-misc)
