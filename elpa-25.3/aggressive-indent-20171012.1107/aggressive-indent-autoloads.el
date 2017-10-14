;;; aggressive-indent-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "aggressive-indent" "aggressive-indent.el"
;;;;;;  (23009 25352 0 0))
;;; Generated autoloads from aggressive-indent.el

(autoload 'aggressive-indent-indent-defun "aggressive-indent" "\
Indent current defun.
Throw an error if parentheses are unbalanced.
If L and R are provided, use them for finding the start and end of defun.

\(fn &optional L R)" t nil)

(autoload 'aggressive-indent-indent-region-and-on "aggressive-indent" "\
Indent region between L and R, and then some.
Call `aggressive-indent-region-function' between L and R, and
then keep indenting until nothing more happens.

\(fn L R)" t nil)

(autoload 'aggressive-indent-mode "aggressive-indent" "\
Toggle Aggressive-Indent mode on or off.
With a prefix argument ARG, enable Aggressive-Indent mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{aggressive-indent-mode-map}

\(fn &optional ARG)" t nil)

(defvar global-aggressive-indent-mode nil "\
Non-nil if Global Aggressive-Indent mode is enabled.
See the `global-aggressive-indent-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-aggressive-indent-mode'.")

(custom-autoload 'global-aggressive-indent-mode "aggressive-indent" nil)

(autoload 'global-aggressive-indent-mode "aggressive-indent" "\
Toggle Aggressive-Indent mode in all buffers.
With prefix ARG, enable Global Aggressive-Indent mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Aggressive-Indent mode is enabled in all buffers where
`aggressive-indent-mode' would do it.
See `aggressive-indent-mode' for more information on Aggressive-Indent mode.

\(fn &optional ARG)" t nil)

(defalias 'aggressive-indent-global-mode #'global-aggressive-indent-mode)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; aggressive-indent-autoloads.el ends here
