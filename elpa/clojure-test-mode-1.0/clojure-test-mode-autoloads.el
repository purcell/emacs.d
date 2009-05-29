;;; clojure-test-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (clojure-test-maybe-enable clojure-test-mode) "clojure-test-mode"
;;;;;;  "clojure-test-mode.el" (18974 22138))
;;; Generated autoloads from clojure-test-mode.el

(autoload 'clojure-test-mode "clojure-test-mode" "\
A minor mode for running Clojure tests.

\(fn &optional ARG)" t nil)

(autoload 'clojure-test-maybe-enable "clojure-test-mode" "\
Enable clojure-test-mode if the current buffer contains Clojure tests.

\(fn)" nil nil)

(add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)

;;;***

;;;### (autoloads nil nil ("clojure-test-mode-pkg.el") (18974 22138
;;;;;;  287165))

;;;***

(provide 'clojure-test-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; clojure-test-mode-autoloads.el ends here
