;;; crontab-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "crontab-mode" "crontab-mode.el" (23009 22550
;;;;;;  0 0))
;;; Generated autoloads from crontab-mode.el

(autoload 'crontab-mode "crontab-mode" "\
Major mode for editing crontabs.
Defines commands for getting and applying crontabs for hosts.
Sets up command `font-lock-mode'.

\\{crontab-mode-map}

\(fn)" t nil)

(autoload 'crontab-get "crontab-mode" "\
Get the crontab for the HOST into a buffer.

\(fn HOST)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; crontab-mode-autoloads.el ends here
