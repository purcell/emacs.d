;;; simple-httpd-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "simple-httpd" "simple-httpd.el" (23009 23275
;;;;;;  0 0))
;;; Generated autoloads from simple-httpd.el

(autoload 'httpd-start "simple-httpd" "\
Start the web server process. If the server is already
running, this will restart the server. There is only one server
instance per Emacs instance.

\(fn)" t nil)

(autoload 'httpd-stop "simple-httpd" "\
Stop the web server if it is currently running, otherwise do nothing.

\(fn)" t nil)

(autoload 'httpd-serve-directory "simple-httpd" "\
Start the web server with given `directory' as `httpd-root'.

\(fn DIRECTORY)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; simple-httpd-autoloads.el ends here
