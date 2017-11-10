;;; ensime-http.el --- Support for ENSIME HTTP Server

;; Copyright (C) 2015 Sam Halliday

;; Author: Sam Halliday <Sam.Halliday@gmail.com>

;;; Commentary:
;;
;;  The ENSIME server exposes HTTP REST and WebSockets on a port
;;  designated by the CACHE/http file. This file provides support to
;;  access the server using this mechanism. It is hoped to entirely
;;  replace the SWANK TCP/IP protocol with the SWANKY WebSockets
;;  protocol.
;;
;;; Code:

(defun ensime-project-docs ()
  "Open the project's documentation splash screen in a browser."
  (interactive)
  (browse-url (ensime--normalise-url "docs")))

(defun ensime--normalise-url (url-or-path)
  "Convert URL-OR-PATH into a full URL.
URL is absolute but PATH is relative to the ENSIME server's HTTP port for this buffer."
  (if (string-prefix-p "http" url-or-path)
      url-or-path
    (concat (ensime--http-for-buffer) "/" url-or-path)))

(defun ensime--http-for-buffer ()
  "The ENSIME HTTP server location as `http://localhost:port' for the current buffer."
  (let* ((config (ensime-config-for-buffer))
         (cache-dir (ensime--get-cache-dir config))
         (port (ensime--read-portfile (concat cache-dir "/http"))))
    (concat "http://localhost:" (format "%S" port))))


(provide 'ensime-http)

;;; ensime-http.el ends here
