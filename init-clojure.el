;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slime with Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use technomancy's bag of fancy clojure/slime tricks
(eval-after-load 'slime
  '(progn
     (require 'durendal)
     (durendal-enable t)
     (durendal-disable-slime-repl-font-lock)))

(defun slime-clojure-repl-setup ()
  "Some REPL setup additional to that in durendal."
  (when (string-equal (slime-lisp-implementation-name) "clojure")
    (when (slime-inferior-process)
      (message "Setting up repl for clojure")
      (slime-redirect-inferior-output))

    (set-syntax-table clojure-mode-syntax-table)
    (setq lisp-indent-function 'clojure-indent-function)
    (let (font-lock-mode)
      (clojure-mode-font-lock-setup))))

(add-hook 'slime-repl-mode-hook 'slime-clojure-repl-setup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nrepl with Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc clojure tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'clojure-mode-hook 'sanityinc/lisp-setup)

(defmacro defclojureface (name color desc &optional others)
  `(defface ,name '((((class color)) (:foreground ,color ,@others))) ,desc :group 'faces))

(defclojureface clojure-parens       "DimGrey"   "Clojure parens")
(defclojureface clojure-braces       "#49b2c7"   "Clojure braces")
(defclojureface clojure-brackets     "SteelBlue" "Clojure brackets")
(defclojureface clojure-keyword      "khaki"     "Clojure keywords")
(defclojureface clojure-java-call    "#4bcf68"   "Clojure Java calls")
(defclojureface clojure-special      "#b8bb00"   "Clojure special")
(defclojureface clojure-double-quote "#b8bb00"   "Clojure special" (:background "unspecified"))

(defun tweak-clojure-syntax ()
  (dolist (x '((("#?['`]*(\\|)"       . 'clojure-parens))
               (("#?\\^?{\\|}"        . 'clojure-brackets))
               (("\\[\\|\\]"          . 'clojure-braces))
               ((":\\w+#?"            . 'clojure-keyword))
               (("#?\""               0 'clojure-double-quote prepend))
               (("nil\\|true\\|false\\|%[1-9]?" . 'clojure-special))
               (("(\\(\\.[^ \n)]*\\|[^ \n)]+\\.\\|new\\)\\([ )\n]\\|$\\)" 1 'clojure-java-call))
               ))
    (font-lock-add-keywords nil x)))

(add-hook 'clojure-mode-hook 'tweak-clojure-syntax)



;; Use clojure-mode for clojurescript, since clojurescript-mode
;; pulls in Slime
(add-auto-mode 'clojure-mode "\\.cljs\\'")

(autoload 'cljsbuild-mode "cljsbuild-mode")

(require 'ansi-color)

(defun cljsbuild--insertion-filter (proc string)
  "When PROC sends STRING, apply ansi color codes and insert into buffer."
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	(goto-char (process-mark proc))
	(insert (ansi-color-apply string))
	(set-marker (process-mark proc) (point)))
      (when moving
        (goto-char (process-mark proc))))))

(defun cljsbuild-auto ()
  "Run \"lein cljsbuild auto\" in a background buffer."
  (interactive)
  (unless (locate-dominating-file default-directory "project.clj")
    (error "Not inside a leiningen project"))
  (with-current-buffer (get-buffer-create "*cljsbuild*")
    (when (get-buffer-process (current-buffer))
      (error "Lein cljsbuild is already running"))
    (buffer-disable-undo)
    (let* ((proc (start-process "cljsbuild"
                                (current-buffer)
                                "lein" "cljsbuild" "auto")))
      (cljsbuild-mode)
      ;; Colorize output
      (set-process-filter proc 'cljsbuild--insertion-filter)
      (font-lock-mode)
      (message "Started cljsbuild."))))


(provide 'init-clojure)
