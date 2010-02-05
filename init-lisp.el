;; pretty lambda (see also slime) ->  "λ"
;;  'greek small letter lambda' / utf8 cebb / unicode 03bb -> \u03BB / mule?!
;; in greek-iso8859-7 -> 107  >  86 ec
(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    'font-lock-keyword-face))))))

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(autoload 'enable-paredit-mode "paredit" "Turn on paredit mode" t)

(add-hook 'emacs-lisp-mode-hook 'pretty-lambdas)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)


(defun set-up-hippie-expand-for-elisp ()
  (make-variable-buffer-local 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t))


;; ----------------------------------------------------------------------------
;; Auto-complete tweaks for emacs-lisp mode
;; ----------------------------------------------------------------------------

(defface ac-symbol-menu-face
  '((t (:background "lightgray" :foreground "darkgreen")))
  "Face for slime candidate menu."
  :group 'auto-complete)

(defface ac-symbol-selection-face
  '((t (:background "darkgreen" :foreground "white")))
  "Face for the slime selected candidate."
  :group 'auto-complete)

;; Modify ac-source-symbols to add colours
(eval-after-load "auto-complete"
  '(progn
     (setq ac-source-symbols
       '((candidates
          . (lambda ()
              (all-completions ac-prefix obarray)))
         (candidate-face . ac-symbol-menu-face)
         (selection-face . ac-symbol-selection-face)))))

(defun set-up-ac-for-elisp ()
  (add-to-list 'ac-sources 'ac-source-symbols t))

(add-hook 'paredit-mode-hook
          (lambda ()
            (define-key paredit-mode-map (kbd "RET") 'paredit-newline)))

(add-hook 'emacs-lisp-mode-hook 'set-up-hippie-expand-for-elisp)
(add-hook 'emacs-lisp-mode-hook 'set-up-ac-for-elisp)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

(defun warn-disabled-command ()
  (interactive)
  (message "Command disabled")
  (ding))

(eval-after-load "paredit"
  '(progn
     ;; These are handy everywhere, not just in lisp modes
     (global-set-key (kbd "M-(") 'paredit-wrap-round)
     (global-set-key (kbd "M-[") 'paredit-wrap-square)
     (global-set-key (kbd "M-{") 'paredit-wrap-curly)

     (global-set-key (kbd "M-)") 'paredit-close-round-and-newline)
     (global-set-key (kbd "M-]") 'paredit-close-square-and-newline)
     (global-set-key (kbd "M-}") 'paredit-close-curly-and-newline)

     ;; Disable kill-sentence, which is easily confused with the kill-sexp
     ;; binding, but doesn't preserve sexp structure
     (define-key paredit-mode-map (kbd "M-K") 'warn-disabled-command)
     (define-key paredit-mode-map (kbd "M-k") 'warn-disabled-command)))

(provide 'init-lisp)
