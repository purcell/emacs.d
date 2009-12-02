(add-hook 'clojure-mode-hook 'pretty-lambdas)
(add-hook 'clojure-mode-hook (lambda () (enable-paredit clojure-mode-map)))


(setq clojure-src-root (expand-file-name "~/Projects/External"))

(setq swank-clojure-jar-path (concat clojure-src-root "/clojure/clojure.jar")
      swank-clojure-extra-vm-args (list "-Xmx1024m")
      swank-clojure-extra-classpaths (list (concat clojure-src-root "/clojure-contrib/clojure-contrib.jar")))

(require 'swank-clojure-autoload)

(eval-after-load "slime"
  '(progn
     ;; Ensure we get a REPL
     (slime-setup '(slime-repl))))


(defun slime-clojure ()
  "Fire up slime running the swank-clojure backend"
  (interactive)
  (slime 'clojure))

(provide 'init-clojure)
