(add-hook 'clojure-mode-hook 'enable-paredit-mode)

(setq clojure-src-root (expand-file-name "~/Projects/External"))
(setq swank-clojure-extra-vm-args (list "-server"
                                        "-Xmx1024m"
                                        "-XX:+UseConcMarkSweepGC"
                                        "-XX:+UseCompressedOops"
                                        "-XX:+DoEscapeAnalysis"))

(add-hook 'slime-repl-mode-hook 'slime-redirect-inferior-output)

(eval-after-load "viper"
  '(add-to-list 'viper-vi-state-mode-list 'clojure-mode))

(eval-after-load "gist"
  '(add-to-list 'gist-supported-modes-alist '(clojure-mode . ".clj")))

(provide 'init-clojure)
