;; do NOT start elnode-web-server by default, prefer the manual way
(setq elnode-do-init nil)

(autoload 'elnode-stop "elnode")

(setq elnodeext-port 4444)

(defun elnodeext-stop-webserver ()
  (interactive)
  (elnode-stop elnodeext-port)
  )

(defun elnodeext-restart-webserver ()
  "(re)start webserver at http://localhost:4444 in the directory of current buffer"
  (interactive)
  (let ((webdir (read-directory-name "Root directory of web server: " (file-name-directory buffer-file-name))))
    (elnode-stop elnodeext-port)
    (elnode-start (elnode-webserver-handler-maker webdir) :port elnodeext-port :host "localhost")
    (message "directory %s served at http://localhost:%d" webdir elnodeext-port)
    )
  )

(global-set-key (kbd "C-c , w") 'elnodeext-restart-webserver)
(global-set-key (kbd "C-c , s") 'elnodeext-stop-webserver)

(provide 'init-elnode)