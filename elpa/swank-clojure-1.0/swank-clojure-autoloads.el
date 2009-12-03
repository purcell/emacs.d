;;; swank-clojure-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (swank-clojure-project swank-clojure-cmd swank-clojure-init)
;;;;;;  "swank-clojure" "swank-clojure.el" (19223 50949))
;;; Generated autoloads from swank-clojure.el

(autoload 'swank-clojure-init "swank-clojure" "\
Not documented

\(fn FILE ENCODING)" nil nil)

(autoload 'swank-clojure-cmd "swank-clojure" "\
Create the command to start clojure according to current settings.

\(fn)" nil nil)

(defadvice slime-read-interactive-args (before add-clojure) (require 'assoc) (aput 'slime-lisp-implementations 'clojure (list (swank-clojure-cmd) :init 'swank-clojure-init)))

(autoload 'swank-clojure-project "swank-clojure" "\
Setup classpath for a clojure project and starts a new SLIME session.
  Kills existing SLIME session, if any.

\(fn PATH)" t nil)

;;;***

;;;### (autoloads nil nil ("swank-clojure-pkg.el") (19223 50949 322781))

;;;***

(provide 'swank-clojure-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; swank-clojure-autoloads.el ends here
