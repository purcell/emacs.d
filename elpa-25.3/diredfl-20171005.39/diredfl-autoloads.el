;;; diredfl-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "diredfl" "diredfl.el" (23009 21806 0 0))
;;; Generated autoloads from diredfl.el

(autoload 'diredfl-mode "diredfl" "\
Enable additional font locking in `dired-mode'.

\(fn &optional ARG)" t nil)

(defvar diredfl-global-mode nil "\
Non-nil if Diredfl-Global mode is enabled.
See the `diredfl-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `diredfl-global-mode'.")

(custom-autoload 'diredfl-global-mode "diredfl" nil)

(autoload 'diredfl-global-mode "diredfl" "\
Toggle Diredfl mode in all buffers.
With prefix ARG, enable Diredfl-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Diredfl mode is enabled in all buffers where
`(lambda nil (when (derived-mode-p (quote dired-mode)) (diredfl-mode)))' would do it.
See `diredfl-mode' for more information on Diredfl mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; diredfl-autoloads.el ends here
