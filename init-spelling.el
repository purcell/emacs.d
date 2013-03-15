(require 'ispell)

(when (executable-find ispell-program-name)
  (require 'init-flyspell))

(provide 'init-spelling)
