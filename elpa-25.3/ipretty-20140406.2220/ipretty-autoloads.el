;;; ipretty-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ipretty" "ipretty.el" (23009 25349 0 0))
;;; Generated autoloads from ipretty.el

(autoload 'ipretty-last-sexp "ipretty" "\
Pretty-print the last sexp into the current buffer.
When TRUNCATE is non-nil or with a prefix argument, long output
is truncated. See the documentation of `eval-print-last-sexp' for
more information on what affects truncation.

\(fn &optional TRUNCATE)" t nil)

(autoload 'ipretty-last-sexp-other-buffer "ipretty" "\
Display the last sexp pretty-printed in other buffer.
If BUFFER-NAME (a string)is provided it will be used to name the
buffer, otherwise the default `*pp-display-expression*' is used.

\(fn &optional BUFFER-NAME)" t nil)

(defvar ipretty-mode nil "\
Non-nil if Ipretty mode is enabled.
See the `ipretty-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ipretty-mode'.")

(custom-autoload 'ipretty-mode "ipretty" nil)

(autoload 'ipretty-mode "ipretty" "\
Toggle ipretty mode globally.
   With no argument, this command toggles the mode.
   Non-null prefix argument turns on the mode.
   Null prefix argument turns off the mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ipretty-autoloads.el ends here
