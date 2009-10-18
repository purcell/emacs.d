;;; clojure-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (clojure-install clojure-mode) "clojure-mode" "clojure-mode.el"
;;;;;;  (19162 54496))
;;; Generated autoloads from clojure-mode.el

(autoload 'clojure-mode "clojure-mode" "\
Major mode for editing Clojure code - similar to Lisp mode..
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{clojure-mode-map}

Entry to this mode calls the value of `clojure-mode-hook'
if that value is non-nil.

\(fn)" t nil)

(defcustom clojure-src-root (expand-file-name "~/src") "\
Directory that contains checkouts for clojure, clojure-contrib,
slime, and swank-clojure. This value is used by `clojure-install'
and `clojure-slime-config'." :type (quote string) :group (quote clojure-mode))

(defun clojure-slime-config (&optional src-root) "\
Load Clojure SLIME support out of the `clojure-src-root' directory.

Since there's no single conventional place to keep Clojure, this
is bundled up as a function so that you can call it after you've set
`clojure-src-root' in your personal config." (if src-root (setq clojure-src-root src-root)) (add-to-list (quote load-path) (concat clojure-src-root "/slime")) (add-to-list (quote load-path) (concat clojure-src-root "/slime/contrib")) (add-to-list (quote load-path) (concat clojure-src-root "/swank-clojure")) (require (quote slime-autoloads)) (require (quote swank-clojure-autoload)) (slime-setup (quote (slime-fancy))) (setq swank-clojure-classpath (list (concat clojure-src-root "/clojure/clojure.jar") (concat clojure-src-root "/clojure-contrib/clojure-contrib.jar"))))

(autoload 'clojure-install "clojure-mode" "\
Perform the initial Clojure install along with Emacs support libs.

This requires git, a JVM, ant, and an active Internet connection.

\(fn SRC-ROOT)" t nil)

(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

;;;***

;;;### (autoloads nil nil ("clojure-mode-pkg.el") (19162 54496 157357))

;;;***

(provide 'clojure-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; clojure-mode-autoloads.el ends here
