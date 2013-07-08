;; save a list of open files in ~/.emacs.d/.emacs.desktop
(setq desktop-path (list user-emacs-directory))
(desktop-save-mode 1)
(defadvice desktop-read (around trace-desktop-errors activate)
  (let ((debug-on-error t))
    ad-do-it))

(defadvice desktop-read (around time-restore activate)
    (let ((start-time (current-time)))
      (prog1
          ad-do-it
        (message "Desktop restored in %.2fms"
                 (sanityinc/time-subtract-millis (current-time)
                                                 start-time)))))

(defadvice desktop-create-buffer (around time-create activate)
  (let ((start-time (current-time))
        (filename (ad-get-arg 1)))
    (prog1
        ad-do-it
      (message "Desktop: %.2fms to restore %s"
               (sanityinc/time-subtract-millis (current-time)
                                               start-time)
               (when filename
		 (abbreviate-file-name filename))))))

;;----------------------------------------------------------------------------
;; Restore histories and registers after saving
;;----------------------------------------------------------------------------
(require-package 'session)

(setq session-save-file (expand-file-name ".session" user-emacs-directory))
(add-hook 'after-init-hook 'session-initialize)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (ido-last-directory-list  . 100)
                (ido-work-directory-list  . 100)
                (ido-work-file-list       . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (comint-input-ring        . 50)
                (shell-command-history    . 50)
                desktop-missing-file-warning
                tags-file-name
                register-alist)))

(when (eval-when-compile (>= emacs-major-version 24))
  (require-package 'frame-restore)
  (frame-restore))

(provide 'init-sessions)
