(add-hook 'clojure-mode-hook (lambda () (enable-paredit clojure-mode-map)))

(setq clojure-src-root (expand-file-name "~/Projects/External"))
(setq swank-clojure-extra-vm-args (list "-server"
                                        "-Xmx1024m"
                                        "-XX:+UseConcMarkSweepGC"
                                        "-XX:+UseCompressedOops"
                                        "-XX:+DoEscapeAnalysis"))

(eval-after-load "viper"
  '(add-to-list 'viper-vi-state-mode-list 'clojure-mode))

(provide 'init-clojure)
