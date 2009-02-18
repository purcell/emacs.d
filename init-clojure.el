(require 'swank-clojure-autoload)
(swank-clojure-config
 (setq swank-clojure-jar-path (expand-file-name "~/Projects/External/clojure/clojure.jar")))

(add-auto-mode 'lisp-mode "\\.clj$")

(defun slime-clojure ()
  "Fire up slime running the swank-clojure backend"
  (interactive)
  (slime 'clojure))


(provide 'init-clojure)
