;; Autoloads and basic wiring
(autoload 'clojure-mode "clojure-mode" "Major mode for editing Clojure code." t nil)
(autoload 'clojure-test-mode "clojure-test-mode" "A minor mode for running Clojure tests." t nil)
(eval-after-load "clojure-mode"
  '(progn
     (require 'clojure-test-mode)))
(autoload 'swank-clojure-init "swank-clojure" "" nil nil)
(autoload 'swank-clojure-slime-mode-hook "swank-clojure" "" nil nil)
(autoload 'swank-clojure-cmd "swank-clojure" "" nil nil)
(defadvice slime-read-interactive-args (before add-clojure)
  (require 'assoc)
  (aput 'slime-lisp-implementations 'clojure (list (swank-clojure-cmd) :init 'swank-clojure-init)))
(autoload 'swank-clojure-project "swank-clojure" "" t nil)
(add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)




(add-hook 'clojure-mode-hook 'enable-paredit-mode)

(setq clojure-src-root (expand-file-name "~/Projects/External"))
(setq swank-clojure-extra-vm-args (list "-server"
                                        "-Xmx1024m"
                                        "-XX:+UseConcMarkSweepGC"
                                        "-XX:+UseCompressedOops"
                                        "-XX:+DoEscapeAnalysis"))

;;------
;; Patch from michalmarczyk, http://gist.github.com/337280

;;; all code in this function lifted from the clojure-mode function
;;; from clojure-mode.el
(defun clojure-font-lock-setup ()
  (interactive)
  (set (make-local-variable 'lisp-indent-function)
       'clojure-indent-function)
  (set (make-local-variable 'lisp-doc-string-elt-property)
       'clojure-doc-string-elt)
  (set (make-local-variable 'font-lock-multiline) t)

  (add-to-list 'font-lock-extend-region-functions
               'clojure-font-lock-extend-region-def t)

  (when clojure-mode-font-lock-comment-sexp
    (add-to-list 'font-lock-extend-region-functions
                 'clojure-font-lock-extend-region-comment t)
    (make-local-variable 'clojure-font-lock-keywords)
    (add-to-list 'clojure-font-lock-keywords
                 'clojure-font-lock-mark-comment t)
    (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil))

  (setq font-lock-defaults
        '(clojure-font-lock-keywords    ; keywords
          nil nil
          (("+-*/.<>=!?$%_&~^:@" . "w")) ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . lisp-font-lock-syntactic-face-function))))

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (font-lock-mode nil)
            (clojure-font-lock-setup)
            (font-lock-mode t)))
;;------


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
