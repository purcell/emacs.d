;; flyspell set up for web-mode
(defun web-mode-flyspefll-verify ()
  (let ((f (get-text-property (- (point) 1) 'face)))
    (not (memq f '(web-mode-html-attr-value-face
                   web-mode-html-tag-face
                   web-mode-html-attr-name-face
                   web-mode-doctype-face
                   web-mode-keyword-face
                   web-mode-function-name-face
                   web-mode-variable-name-face
                   web-mode-css-property-name-face
                   web-mode-css-selector-face
                   web-mode-css-color-face
                   web-mode-type-face
                   )
               ))
    ))
(put 'web-mode 'flyspell-mode-predicate 'web-mode-flyspefll-verify)

(require 'flyspell-lazy)
(flyspell-lazy-mode 1)
;; better performance
(setq flyspell-issue-message-flag nil)

;; if (aspell installed) { use aspell}
;; else if (hunspell installed) { use hunspell }
;; whatever spell checker I use, I always use English dictionary
;; I prefer use aspell because:
;; 1. aspell is older
;; 2. looks Kevin Atkinson still get some roadmap for aspell:
;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
(if (executable-find "aspell")
  (progn
    (setq ispell-program-name "aspell"
          ;; ispell-extra-args '("--sug-mode=ultra")
          ;; force the English dictionary
          ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")
          )
    )
  (if (executable-find "hunspell")
    (progn
      (setq ispell-program-name "hunspell"
            ispell-extra-args '("-D en_US")
            )
      )))


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
                c++-mode-hook
                c-mode-hook
                lua-mode-hook
                crontab-mode-hook
                perl-mode-hook
                tcl-mode-hook
                js2-mode-hook))
  (add-hook hook 'flyspell-prog-mode))

;; you can also use "M-x ispell-word" or hotkey "M-$". It pop up a multiple choice
;; @see http://frequal.com/Perspectives/EmacsTip03-FlyspellAutoCorrectWord.html
(global-set-key (kbd "C-c s") 'flyspell-auto-correct-word)

(provide 'init-spelling)
