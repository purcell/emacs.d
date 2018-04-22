;;; highlight-escape-sequences-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "highlight-escape-sequences" "highlight-escape-sequences.el"
;;;;;;  (23009 21845 0 0))
;;; Generated autoloads from highlight-escape-sequences.el

(autoload 'turn-on-hes-mode "highlight-escape-sequences" "\
Turn on highlighting of escape sequences.

\(fn)" t nil)

(autoload 'turn-off-hes-mode "highlight-escape-sequences" "\
Turn off highlighting of escape sequences

\(fn)" t nil)

(defvar hes-mode nil "\
Non-nil if Hes mode is enabled.
See the `hes-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `hes-mode'.")

(custom-autoload 'hes-mode "highlight-escape-sequences" nil)

(autoload 'hes-mode "highlight-escape-sequences" "\
Toggle highlighting of escape sequences.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; highlight-escape-sequences-autoloads.el ends here
