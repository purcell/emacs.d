(require 'flyspell-lazy)
(flyspell-lazy-mode 1)

(when (executable-find "aspell")
  (setq ispell-program-name "aspell"
        ;; ispell-extra-args '("--sug-mode=ultra")
        ;; force the English dictionary
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")
        ))
(require 'init-flyspell)


(provide 'init-spelling)
