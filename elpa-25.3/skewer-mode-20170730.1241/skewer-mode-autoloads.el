;;; skewer-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "skewer-bower" "skewer-bower.el" (23009 23276
;;;;;;  0 0))
;;; Generated autoloads from skewer-bower.el

(autoload 'skewer-bower-refresh "skewer-bower" "\
Update the package listing and packages synchronously.

\(fn)" t nil)

(autoload 'skewer-bower-load "skewer-bower" "\
Dynamically load a library from bower into the current page.

\(fn PACKAGE &optional VERSION)" t nil)

;;;***

;;;### (autoloads nil "skewer-css" "skewer-css.el" (23009 23276 0
;;;;;;  0))
;;; Generated autoloads from skewer-css.el

(autoload 'skewer-css-mode "skewer-css" "\
Minor mode for interactively loading new CSS rules.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "skewer-html" "skewer-html.el" (23009 23276
;;;;;;  0 0))
;;; Generated autoloads from skewer-html.el

(autoload 'skewer-html-mode "skewer-html" "\
Minor mode for interactively loading new HTML.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "skewer-mode" "skewer-mode.el" (23009 23276
;;;;;;  0 0))
;;; Generated autoloads from skewer-mode.el

(autoload 'list-skewer-clients "skewer-mode" "\
List the attached browsers in a buffer.

\(fn)" t nil)

(autoload 'skewer-mode "skewer-mode" "\
Minor mode for interacting with a browser.

\(fn &optional ARG)" t nil)

(autoload 'run-skewer "skewer-mode" "\
Attach a browser to Emacs for a skewer JavaScript REPL. Uses
`browse-url' to launch a browser.

\(fn)" t nil)

(autoload 'skewer-run-phantomjs "skewer-mode" "\
Connect an inferior PhantomJS process to Skewer, returning the process.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "skewer-repl" "skewer-repl.el" (23009 23276
;;;;;;  0 0))
;;; Generated autoloads from skewer-repl.el

(autoload 'skewer-repl--response-hook "skewer-repl" "\
Catches all browser messages logging some to the REPL.

\(fn RESPONSE)" nil nil)

(autoload 'skewer-repl "skewer-repl" "\
Start a JavaScript REPL to be evaluated in the visiting browser.

\(fn)" t nil)

(eval-after-load 'skewer-mode '(progn (add-hook 'skewer-response-hook #'skewer-repl--response-hook) (add-hook 'skewer-repl-mode-hook #'skewer-repl-mode-compilation-shell-hook) (define-key skewer-mode-map (kbd "C-c C-z") #'skewer-repl)))

;;;***

;;;### (autoloads nil "skewer-setup" "skewer-setup.el" (23009 23276
;;;;;;  0 0))
;;; Generated autoloads from skewer-setup.el

(autoload 'skewer-setup "skewer-setup" "\
Fully integrate Skewer into js2-mode, css-mode, and html-mode buffers.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("cache-table.el" "skewer-mode-pkg.el")
;;;;;;  (23009 23276 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; skewer-mode-autoloads.el ends here
