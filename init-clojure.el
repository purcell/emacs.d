(require 'swank-clojure-autoload)

(setq clojure-src-root (expand-file-name "~/Projects/External"))
(eval-after-load 'clojure-mode '(clojure-slime-config))

(defun slime-clojure ()
  "Fire up slime running the swank-clojure backend"
  (interactive)
  (slime 'clojure))

(provide 'init-clojure)
