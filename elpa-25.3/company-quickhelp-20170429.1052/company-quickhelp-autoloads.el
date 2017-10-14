;;; company-quickhelp-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "company-quickhelp" "company-quickhelp.el"
;;;;;;  (23009 21823 0 0))
;;; Generated autoloads from company-quickhelp.el

(autoload 'company-quickhelp-local-mode "company-quickhelp" "\
Provides documentation popups for `company-mode' using `pos-tip'.

\(fn &optional ARG)" t nil)

(defvar company-quickhelp-mode nil "\
Non-nil if Company-Quickhelp mode is enabled.
See the `company-quickhelp-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `company-quickhelp-mode'.")

(custom-autoload 'company-quickhelp-mode "company-quickhelp" nil)

(autoload 'company-quickhelp-mode "company-quickhelp" "\
Toggle Company-Quickhelp-Local mode in all buffers.
With prefix ARG, enable Company-Quickhelp mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Company-Quickhelp-Local mode is enabled in all buffers where
`company-quickhelp-local-mode' would do it.
See `company-quickhelp-local-mode' for more information on Company-Quickhelp-Local mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; company-quickhelp-autoloads.el ends here
