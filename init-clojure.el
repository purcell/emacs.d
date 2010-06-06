;; Autoloads and basic wiring
(autoload 'clojure-mode "clojure-mode" "Major mode for editing Clojure code." t nil)
(autoload 'clojure-test-mode "clojure-test-mode" "A minor mode for running Clojure tests." t nil)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

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


;;------
;; Patch from michalmarczyk, http://gist.github.com/337280

(defadvice slime-repl-emit (after sr-emit-ad activate)
  (with-current-buffer (slime-output-buffer)
    (add-text-properties slime-output-start slime-output-end
                         '(font-lock-face slime-repl-output-face
                                          rear-nonsticky (font-lock-face)))))

(defadvice slime-repl-insert-prompt (after sr-prompt-ad activate)
  (with-current-buffer (slime-output-buffer)
    (let ((inhibit-read-only t))
      (add-text-properties slime-repl-prompt-start-mark (point-max)
                           '(font-lock-face slime-repl-prompt-face
                                            rear-nonsticky
                                            (slime-repl-prompt
                                             read-only
                                             font-lock-face
                                             intangible))))))
;;------

(add-hook 'clojure-mode-hook 'font-lock-mode) ;; because it doesn't turn on in Emacs 24

(defun slime-clojure-repl-setup ()
  (when (string-equal "clojure" (slime-connection-name))
    (message "Setting up repl for clojure")
    (clojure-mode-font-lock-setup)
    (when (slime-inferior-process)
      (slime-redirect-inferior-output))
    (swank-clojure-slime-repl-modify-syntax)))

(add-hook 'slime-repl-mode-hook 'slime-clojure-repl-setup)




(defun lein-swank ()
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory "project.clj")))
    (when (not default-directory)
      (error "Not in a Leiningen project."))
    ;; you can customize slime-port using .dir-locals.el
    (let ((proc (start-process "lein-swank" nil "lein" "swank" (number-to-string slime-port))))
      (when proc
	(process-put proc :output nil)
	(set-process-sentinel proc (lambda (proc event)
				     (message "%s%s: `%S'" 
					      (process-get proc :output)
					      proc (replace-regexp-in-string "\n" "" event))))
	(set-process-filter proc
			    (lambda (proc output)
			      ;; record last line of output until connected (possible error message)
			      (process-put proc :output (concat (process-get proc :output) output))
			      (when (string-match "Connection opened on" output)
				(slime-connect "localhost" slime-port)
				;; no need to further process output
				(set-process-filter proc nil))))
	(message "Starting swank server...")))))




(eval-after-load "viper"
  '(add-to-list 'viper-vi-state-mode-list 'clojure-mode))

(eval-after-load "gist"
  '(add-to-list 'gist-supported-modes-alist '(clojure-mode . ".clj")))

(provide 'init-clojure)
