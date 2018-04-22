;;; intero-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "intero" "intero.el" (23009 25324 0 0))
;;; Generated autoloads from intero.el

(autoload 'intero-mode "intero" "\
Minor mode for Intero.

\\{intero-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'intero-mode-whitelist "intero" "\
Run intero-mode when the current project is in `intero-whitelist'.

\(fn)" t nil)

(autoload 'intero-mode-blacklist "intero" "\
Run intero-mode unless the current project is in `intero-blacklist'.

\(fn)" t nil)

(defvar intero-global-mode nil "\
Non-nil if Intero-Global mode is enabled.
See the `intero-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `intero-global-mode'.")

(custom-autoload 'intero-global-mode "intero" nil)

(autoload 'intero-global-mode "intero" "\
Toggle Intero mode in all buffers.
With prefix ARG, enable Intero-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Intero mode is enabled in all buffers where
`intero-mode-maybe' would do it.
See `intero-mode' for more information on Intero mode.

\(fn &optional ARG)" t nil)

(autoload 'intero-highlight-uses-mode "intero" "\
Minor mode for highlighting and jumping between uses.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; intero-autoloads.el ends here
