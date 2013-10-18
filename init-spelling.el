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

;; at least at emacs 24.2.1, the *function* flyspell-large-region is buggy!
;; we set the *flag* flyspell-large-region to the maximum int to make sure that
;; function is never called.
;; Besides, *package* flyspell-lazy should be removed because it manipulates on the
;; flag.
(setq flyspell-large-region most-positive-fixnum)

(require 'init-flyspell)


(provide 'init-spelling)
