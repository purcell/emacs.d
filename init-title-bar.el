(defvar *user*    (user-login-name) "user login name")
(defvar *hostname*
  (let ((n (system-name))) (substring n 0 (string-match "\\." n))) "unqualified host name")

(defun network-location ()
  "Report the network location of this computer; only implemented for Macs"
  (when *is-a-mac*
    (let ((scselect (shell-command-to-string "/usr/sbin/scselect")))
      (if (string-match "^ \\* .*(\\(.*\\))$" scselect)
          (match-string 1 scselect)))))

(defun concise-network-location ()
  (let ((l (network-location)))
    (if (and l (not (string-equal "Automatic" l)))
        (concat "[" l "]")
      "")))

(defun concise-buffer-file-name ()
  (when (buffer-file-name)
    (replace-regexp-in-string (regexp-quote (getenv "HOME")) "~" (buffer-file-name))))
(setq frame-title-format '("%b - " *user* "@" *hostname*
                           (:eval (concise-network-location)) " - "
                           (:eval (concise-buffer-file-name))))

(provide 'init-title-bar)