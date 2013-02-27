(when (executable-find "aspell")
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra")))

(when (executable-find ispell-program-name)
  (require 'init-flyspell))

(provide 'init-spelling)
