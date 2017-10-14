;;; js-comint-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "js-comint" "js-comint.el" (23009 23274 0 0))
;;; Generated autoloads from js-comint.el

(autoload 'js-do-use-nvm "js-comint" "\
Enable nvm.

\(fn)" nil nil)

(autoload 'js-comint-select-node-version "js-comint" "\
Use a given VERSION of node from nvm.

\(fn &optional VERSION)" t nil)

(autoload 'js-comint-add-module-path "js-comint" "\
Add a directory to `js-comint-module-paths'.

\(fn)" t nil)

(autoload 'js-comint-delete-module-path "js-comint" "\
Delete a directory from `js-comint-module-paths'.

\(fn)" t nil)

(autoload 'js-comint-save-setup "js-comint" "\
Save current setup to `.dir-locals.el'

\(fn)" t nil)

(autoload 'js-comint-reset-repl "js-comint" "\
Kill existing REPL process if possible.  Create a new
Javascript REPL process.  The environment variable `NODE_PATH'
is setup by `js-comint-module-paths' before the process starts.

\(fn)" t nil)

(autoload 'js-comint-clear "js-comint" "\
Clear the Javascript REPL.

\(fn)" t nil)

(autoload 'js-comint-repl "js-comint" "\
Run an Javascript process.  The environment variable `NODE_PATH'
is setup by `js-comint-module-paths' before the process
starts.

\(fn CMD)" t nil)

(autoload 'js-comint-send-string "js-comint" "\


\(fn STR)" nil nil)

(autoload 'js-comint-send-region "js-comint" "\
Send the current region to the inferior Javascript process.
If no region selected, you could manually input javascript expression.

\(fn)" t nil)

(defalias 'js-send-region 'js-comint-send-region)

(autoload 'js-comint-send-last-sexp "js-comint" "\
Send the previous sexp to the inferior Javascript process.  `evil-mode' friendly.

\(fn)" t nil)

(defalias 'js-send-last-sexp 'js-comint-send-last-sexp)

(autoload 'js-comint-send-buffer "js-comint" "\
Send the buffer to the inferior Javascript process.

\(fn)" t nil)

(defalias 'js-send-buffer 'js-comint-send-buffer)

(autoload 'js-comint-load-file "js-comint" "\
Load a file in the javascript interpreter.

\(fn FILENAME)" t nil)

(defalias 'js-load-file 'js-comint-load-file)

(autoload 'js-comint-switch-to-repl "js-comint" "\
Switch to the javascript process buffer.
With argument, position cursor at end of buffer.

\(fn EOB-P)" t nil)

(autoload 'js-comint-mode "js-comint" "\


\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; js-comint-autoloads.el ends here
