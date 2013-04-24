;;----------------------------------------------------------------------------
;; Add spell-checking in comments for all programming language modes
;;----------------------------------------------------------------------------
(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                scheme-mode-hook
                clojure-mode-hook
                ruby-mode-hook
                yaml-mode
                python-mode-hook
                shell-mode-hook
                php-mode-hook
                css-mode-hook
                haskell-mode-hook
                caml-mode-hook
                nxml-mode-hook
                crontab-mode-hook
                perl-mode-hook
                tcl-mode-hook
                js2-mode-hook))
  (add-hook hook 'flyspell-prog-mode))
(add-hook 'nxml-mode-hook
          (lambda ()
            (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)))

;; you can also use "M-x ispell-word" or hotkey "M-$". It pop up a multiple choice
;; @see http://frequal.com/Perspectives/EmacsTip03-FlyspellAutoCorrectWord.html
(global-set-key (kbd "C-c s") 'flyspell-auto-correct-word)

(provide 'init-flyspell)
