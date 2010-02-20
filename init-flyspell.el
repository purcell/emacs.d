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
                javascript-mode-hook))
  (add-hook hook 'flyspell-prog-mode))
(add-hook 'nxml-mode-hook
          (lambda ()
            (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)))


(provide 'init-flyspell)
