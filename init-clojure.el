(add-hook 'clojure-mode-hook 'enable-paredit-mode)

(setq clojure-src-root (expand-file-name "~/Projects/External"))
(setq swank-clojure-extra-vm-args (list "-server"
                                        "-Xmx1024m"
                                        "-XX:+UseConcMarkSweepGC"
                                        "-XX:+UseCompressedOops"
                                        "-XX:+DoEscapeAnalysis"))

(defun slime-clojure-repl-setup ()
  (when (string-equal "clojure" (slime-connection-name))
    (message "Setting up repl for clojure")
    (slime-redirect-inferior-output)
    (when (and (featurep 'paredit) paredit-mode (>= paredit-version 21))
      (define-key paredit-mode-map "{" 'paredit-open-curly)
      (define-key paredit-mode-map "}" 'paredit-close-curly))))

(add-hook 'slime-repl-mode-hook 'slime-clojure-repl-setup)

(eval-after-load "viper"
  '(add-to-list 'viper-vi-state-mode-list 'clojure-mode))

(eval-after-load "gist"
  '(add-to-list 'gist-supported-modes-alist '(clojure-mode . ".clj")))

(provide 'init-clojure)
