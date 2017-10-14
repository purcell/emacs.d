;;; clojure-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "clojure-mode" "clojure-mode.el" (23009 25364
;;;;;;  0 0))
;;; Generated autoloads from clojure-mode.el

(autoload 'clojure-mode "clojure-mode" "\
Major mode for editing Clojure code.

\\{clojure-mode-map}

\(fn)" t nil)

(autoload 'clojure-unwind "clojure-mode" "\
Unwind thread at point or above point by one level.
Return nil if there are no more levels to unwind.

\(fn)" t nil)

(autoload 'clojure-unwind-all "clojure-mode" "\
Fully unwind thread at point or above point.

\(fn)" t nil)

(autoload 'clojure-thread "clojure-mode" "\
Thread by one more level an existing threading macro.

\(fn)" t nil)

(autoload 'clojure-thread-first-all "clojure-mode" "\
Fully thread the form at point using ->.

When BUT-LAST is non-nil, the last expression is not threaded.
Default value is `clojure-thread-all-but-last'.

\(fn BUT-LAST)" t nil)

(autoload 'clojure-thread-last-all "clojure-mode" "\
Fully thread the form at point using ->>.

When BUT-LAST is non-nil, the last expression is not threaded.
Default value is `clojure-thread-all-but-last'.

\(fn BUT-LAST)" t nil)

(autoload 'clojure-cycle-privacy "clojure-mode" "\
Make public the current private def, or vice-versa.
See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-cycle-privacy

\(fn)" t nil)

(autoload 'clojure-convert-collection-to-list "clojure-mode" "\
Convert collection at (point) to list.

\(fn)" t nil)

(autoload 'clojure-convert-collection-to-quoted-list "clojure-mode" "\
Convert collection at (point) to quoted list.

\(fn)" t nil)

(autoload 'clojure-convert-collection-to-map "clojure-mode" "\
Convert collection at (point) to map.

\(fn)" t nil)

(autoload 'clojure-convert-collection-to-vector "clojure-mode" "\
Convert collection at (point) to vector.

\(fn)" t nil)

(autoload 'clojure-convert-collection-to-set "clojure-mode" "\
Convert collection at (point) to set.

\(fn)" t nil)

(autoload 'clojure-cycle-if "clojure-mode" "\
Change a surrounding if to if-not, or vice-versa.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-cycle-if

\(fn)" t nil)

(autoload 'clojure-cycle-when "clojure-mode" "\
Change a surrounding when to when-not, or vice-versa.

\(fn)" t nil)

(autoload 'clojure-let-backward-slurp-sexp "clojure-mode" "\
Slurp the s-expression before the let form into the let form.
With a numberic prefix argument slurp the previous N s-expression into the let form.

\(fn &optional N)" t nil)

(autoload 'clojure-let-forward-slurp-sexp "clojure-mode" "\
Slurp the next s-expression after the let form into the let form.
With a numeric prefix argument slurp the next N s-expressions into the let form.

\(fn &optional N)" t nil)

(autoload 'clojure-introduce-let "clojure-mode" "\
Create a let form, binding the form at point.
With a numeric prefix argument the let is introduced N lists up.

\(fn &optional N)" t nil)

(autoload 'clojure-move-to-let "clojure-mode" "\
Move the form at point to a binding in the nearest let.

\(fn)" t nil)

(autoload 'clojurescript-mode "clojure-mode" "\
Major mode for editing ClojureScript code.

\\{clojurescript-mode-map}

\(fn)" t nil)

(autoload 'clojurec-mode "clojure-mode" "\
Major mode for editing ClojureC code.

\\{clojurec-mode-map}

\(fn)" t nil)

(autoload 'clojurex-mode "clojure-mode" "\
Major mode for editing ClojureX code.

\\{clojurex-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode))

(add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojurec-mode))

(add-to-list 'auto-mode-alist '("\\.cljx\\'" . clojurex-mode))

(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))

(add-to-list 'auto-mode-alist '("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; clojure-mode-autoloads.el ends here
