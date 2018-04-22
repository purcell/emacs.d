;;; whitespace-cleanup-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "whitespace-cleanup-mode" "whitespace-cleanup-mode.el"
;;;;;;  (23009 21848 0 0))
;;; Generated autoloads from whitespace-cleanup-mode.el

(let ((loads (get 'whitespace-cleanup 'custom-loads))) (if (member '"whitespace-cleanup-mode" loads) nil (put 'whitespace-cleanup 'custom-loads (cons '"whitespace-cleanup-mode" loads))))

(autoload 'whitespace-cleanup-mode "whitespace-cleanup-mode" "\
Automatically call `whitespace-cleanup' on save.

\(fn &optional ARG)" t nil)

(put 'whitespace-cleanup-mode 'safe-local-variable 'booleanp)

(defvar global-whitespace-cleanup-mode nil "\
Non-nil if Global Whitespace-Cleanup mode is enabled.
See the `global-whitespace-cleanup-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-whitespace-cleanup-mode'.")

(custom-autoload 'global-whitespace-cleanup-mode "whitespace-cleanup-mode" nil)

(autoload 'global-whitespace-cleanup-mode "whitespace-cleanup-mode" "\
Toggle Whitespace-Cleanup mode in all buffers.
With prefix ARG, enable Global Whitespace-Cleanup mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Whitespace-Cleanup mode is enabled in all buffers where
`turn-on-whitespace-cleanup-mode' would do it.
See `whitespace-cleanup-mode' for more information on Whitespace-Cleanup mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; whitespace-cleanup-mode-autoloads.el ends here
