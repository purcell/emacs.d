;; pretty lambda (see also slime) ->  "λ"
;;  'greek small letter lambda' / utf8 cebb / unicode 03bb -> \u03BB / mule?!
;; in greek-iso8859-7 -> 107  >  86 ec
(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    'font-lock-keyword-face))))))

(autoload 'paredit-mode "paredit-beta"
  "Minor mode for pseudo-structurally editing Lisp code." t)

(defun enable-paredit (keymap)
  (paredit-mode +1)
  (define-key keymap (kbd "RET") 'paredit-newline))

(add-hook 'emacs-lisp-mode-hook 'pretty-lambdas)
(add-hook 'emacs-lisp-mode-hook (lambda () (enable-paredit emacs-lisp-mode-map)))
