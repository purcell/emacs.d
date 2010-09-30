;;; smart-tab-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (global-smart-tab-mode smart-tab-mode smart-tab-mode-on
;;;;;;  smart-tab) "smart-tab" "smart-tab.el" (19620 58025))
;;; Generated autoloads from smart-tab.el

(autoload 'smart-tab "smart-tab" "\
Try to 'do the smart thing' when tab is pressed.
`smart-tab' attempts to expand the text before the point or
indent the current line or selection.

In a regular buffer, `smart-tab' will attempt to expand with
either `hippie-expand' or `dabbrev-expand', depending on the
value of `smart-tab-using-hippie-expand'. If the mark is active,
or PREFIX is \\[universal-argument], then `smart-tab' will indent
the region or the current line (if the mark is not active).

\(fn PREFIX)" t nil)

(autoload 'smart-tab-mode-on "smart-tab" "\
Turn on `smart-tab-mode'.

\(fn)" nil nil)

(autoload 'smart-tab-mode "smart-tab" "\
Enable `smart-tab' to be used in place of tab.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

\(fn &optional ARG)" t nil)

(defvar global-smart-tab-mode nil "\
Non-nil if Global-Smart-Tab mode is enabled.
See the command `global-smart-tab-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-smart-tab-mode'.")

(custom-autoload 'global-smart-tab-mode "smart-tab" nil)

(autoload 'global-smart-tab-mode "smart-tab" "\
Toggle Smart-Tab mode in every possible buffer.
With prefix ARG, turn Global-Smart-Tab mode on if and only if
ARG is positive.
Smart-Tab mode is enabled in all buffers where
`smart-tab-mode-on' would do it.
See `smart-tab-mode' for more information on Smart-Tab mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("smart-tab-pkg.el") (19620 58025 191139))

;;;***

(provide 'smart-tab-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; smart-tab-autoloads.el ends here
