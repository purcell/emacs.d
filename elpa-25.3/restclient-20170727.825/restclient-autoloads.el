;;; restclient-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "restclient" "restclient.el" (23009 23618 0
;;;;;;  0))
;;; Generated autoloads from restclient.el

(autoload 'restclient-http-send-current "restclient" "\
Sends current request.
Optional argument RAW don't reformat response if t.
Optional argument STAY-IN-WINDOW do not move focus to response buffer if t.

\(fn &optional RAW STAY-IN-WINDOW)" t nil)

(autoload 'restclient-http-send-current-raw "restclient" "\
Sends current request and get raw result (no reformatting or syntax highlight of XML, JSON or images).

\(fn)" t nil)

(autoload 'restclient-http-send-current-stay-in-window "restclient" "\
Send current request and keep focus in request window.

\(fn)" t nil)

(autoload 'restclient-mode "restclient" "\
Turn on restclient mode.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; restclient-autoloads.el ends here
