;; You can autoload, but at the end of this block we'll
;; connect to two networks anyway.
(require 'rcirc)

;; Stop rcirc from printing /away message
(defun rcirc-handler-301 (process cmd sender args)
  "/away message handler.")

;; Use fly spell check
(add-hook 'rcirc-mode-hook (lambda ()
                             (flyspell-mode 1)))
;; Keep input line at bottom.
(add-hook 'rcirc-mode-hook
          (lambda ()
            (set (make-local-variable 'scroll-conservatively)
                 8192)))
;; Reconnect
(eval-after-load 'rcirc
  '(defun-rcirc-command reconnect (arg)
     "Reconnect the server process."
     (interactive "i")
     (unless process
       (error "There's no process for this target"))
     (let* ((server (car (process-contact process)))
            (port (process-contact process :service))
            (nick (rcirc-nick process))
            channels query-buffers)
       (dolist (buf (buffer-list))
         (with-current-buffer buf
           (when (eq process (rcirc-buffer-process))
             (remove-hook 'change-major-mode-hook
                          'rcirc-change-major-mode-hook)
             (if (rcirc-channel-p rcirc-target)
                 (setq channels (cons rcirc-target channels))
               (setq query-buffers (cons buf query-buffers))))))
       (delete-process process)
       (rcirc-connect server port nick
                      rcirc-default-user-name
                      rcirc-default-user-full-name
                      channels))))

;; Turn the debugging mode on
;(setq rcirc-debug-flag t)

;; Adjust the colors of one of the faces.
(set-face-foreground 'rcirc-my-nick "red" nil)

;; Include date in time stamp.
(setq rcirc-time-format "%Y-%m-%d %H:%M ")

;; Change user info
(setq rcirc-default-nick "chenbin0")
(setq rcirc-default-user-name "chenbin0")
(setq rcirc-default-full-name "Chen Bin")

(let ((auth-file "~/private/.auth-rcirc.el"))
   (when (file-readable-p auth-file)
        (load auth-file)))

;; Join these channels at startup.
(if (< emacs-major-version 23)
  (setq rcirc-startup-channels-alist nil)
  (setq rcirc-server-alist
        '(("irc.freenode.net" :channels ("#emacs"))))
  )

(defun freenode ()
  (interactive)
  (rcirc nil))

(defun bitlbee ()
  (interactive)
  (rcirc-connect
    "localhost" "6667" rcirc-default-nick rcirc-default-user-name
    rcirc-default-full-name
    "&bitlbee"))

(defun rbitlbee ()
  (interactive)
  (rcirc-connect
    "im.bitlbee.org" "6667" rcirc-default-nick rcirc-default-user-name
    rcirc-default-full-name
    "&bitlbee"))

;; Notifications
(defun th-rcirc-notification (process sender response target text)
  (let ((my-nick (rcirc-nick process)))
    (when (and (string= response "PRIVMSG")
               (not (string= sender my-nick))
               (not (and (string= sender  "root")
                         (string= target  "&bitlbee")))
               (or
                ;; BitlBee IM messages
                (string-match "localhost" (format "%s" process))
                ;; Messages that mention my name
                (string-match my-nick text)))
      (notify "rcirc" target))))

(add-hook 'rcirc-print-hooks 'th-rcirc-notification)

 ;  Minimal logging
(add-hook 'rcirc-print-hooks 'rcirc-write-log)
(defvar my-rcirc-log-dir "~/.irclog/")
(defun rcirc-write-log (process sender response target text)
  (with-temp-buffer
    ;; Sometimes TARGET is a buffer :-(
    (when (bufferp target)
      (setq target (with-current-buffer buffer rcirc-target)))
    ;; Sometimes buffer is not anything at all!
    (unless (or (null target) (string= target ""))
      ;; Print the line into the temp buffer.
      (insert (format-time-string "%Y-%m-%d %H:%M "))
      (insert (format "%-16s " (rcirc-user-nick sender)))
      (unless (string= response "PRIVMSG")
        (insert "/" (downcase response) " "))
      (insert text "\n")
      ;; Append the line to the appropriate logfile.
      (let* ((coding-system-for-write 'no-conversion)
             (log-dir (concat my-rcirc-log-dir
                              (downcase target)
                              (format-time-string "/%Y/%m/")))
             (log-file (format-time-string "%d")))
        (unless (file-exists-p log-dir)
          (make-directory log-dir t))
        (write-region (point-min) (point-max)
                      (concat log-dir log-file)
                      t 'quietly)))))

(provide 'init-rcirc)
