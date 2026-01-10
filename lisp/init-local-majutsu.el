;;; init-local-majutsu.el --- Majutsu configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Majutsu Git porcelain configuration converted from Doom Emacs

;;; Code:

(defun init-local-majutsu--read-remote ()
  "Prompt for a Git remote when multiple remotes exist.
Return nil to let jj pick the default remote."
  (unless (require 'majutsu nil t)
    (user-error "Majutsu is not available"))
  (require 'majutsu-git nil t)
  (let ((remotes (when (fboundp 'majutsu-git--remote-names)
                   (majutsu-git--remote-names
                    (ignore-errors (majutsu--toplevel-safe))))))
    (when (> (length remotes) 1)
      (completing-read
       (format "Push remote (default %s): " (car remotes))
       remotes nil nil nil nil (car remotes)))))

(defun init-local-majutsu--normalize-remote (remote)
  "Return REMOTE as a non-empty string, or nil."
  (when (and (stringp remote) (not (string= remote "")))
    remote))

(defun init-local-majutsu--message-output (output)
  "Echo OUTPUT when it is non-empty."
  (when (and (stringp output) (not (string= output "")))
    (message "%s" output)))

(unless (fboundp 'majutsu-run-jj-async)
  (defun majutsu-run-jj-async (args success-callback error-callback)
    "Run jj with ARGS asynchronously.
Invoke SUCCESS-CALLBACK or ERROR-CALLBACK with the command output."
    (unless (require 'majutsu nil t)
      (user-error "Majutsu is not available"))
    (require 'ansi-color)
    (let* ((default-directory (majutsu--toplevel-safe))
           (buffer (generate-new-buffer " *majutsu-jj*"))
           (process (apply #'start-file-process "majutsu-jj"
                           buffer majutsu-executable args)))
      (set-process-query-on-exit-flag process nil)
      (set-process-sentinel
       process
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (let* ((proc-buffer (process-buffer proc))
                  (output (when (buffer-live-p proc-buffer)
                            (with-current-buffer proc-buffer
                              (ansi-color-filter-apply (buffer-string)))))
                  (exit-code (process-exit-status proc)))
             (when (buffer-live-p proc-buffer)
               (kill-buffer proc-buffer))
             (if (and (eq (process-status proc) 'exit) (zerop exit-code))
                 (when success-callback
                   (funcall success-callback output))
               (when error-callback
                 (funcall error-callback output)))))))
      process)))

(defun init-local-majutsu--push-bookmark-main (remote)
  "Push bookmark `main' to REMOTE using jj."
  (let* ((remote (init-local-majutsu--normalize-remote remote))
         (push-args (append '("git" "push")
                            (and remote (list "--remote" remote))
                            '("--bookmark" "main")))
         (success-msg (if remote
                          (format "Pushed bookmark 'main' to %s" remote)
                        "Pushed bookmark 'main'")))
    (majutsu-with-toplevel
      (majutsu-start-jj
       push-args
       success-msg
       (lambda (_process exit-code)
         (if (zerop exit-code)
             (when (fboundp 'majutsu-log-refresh)
               (majutsu-log-refresh))
           (message "Push failed")))))))

(defun majutsu-bookmark-main-and-push (&optional remote)
  "Set bookmark `main' to `@-' and push it to REMOTE using jj.
When called interactively, prompt for REMOTE if multiple remotes exist.
When REMOTE is nil, rely on jj's default remote selection."
  (interactive
   (list (init-local-majutsu--read-remote)))
  (require 'majutsu)
  (majutsu-with-toplevel
    (majutsu-start-jj
     '("bookmark" "set" "main" "-r" "@-")
     nil
     (lambda (_process exit-code)
       (if (zerop exit-code)
           (init-local-majutsu--push-bookmark-main remote)
         (message "Failed to set bookmark 'main'"))))))

(use-package majutsu
  :ensure t
  :vc (:url "https://github.com/0WD0/majutsu")
  :commands (majutsu majutsu-commit majutsu-describe majutsu-diff)

  :init
  ;; Majutsu defines EIEIO classes inheriting from `magit-section'.  If the
  ;; class isn't loaded yet, EIEIO will error ("magit-section is not a class").
  (require 'magit-section)
  ;; Keybindings: SPC j prefix (evil leader) - set before loading
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'global
      (kbd "SPC j j") #'majutsu
      (kbd "SPC j c") #'majutsu-commit
      (kbd "SPC j D") #'majutsu-describe
      (kbd "SPC j d") #'majutsu-diff
      (kbd "SPC j p") #'majutsu-bookmark-main-and-push)))

(with-eval-after-load 'majutsu
  ;; Majutsu renamed a few entry points in 0.3.0; keep compatibility with older
  ;; muscle memory and stale autoloads.
  (when (fboundp 'majutsu-dispatch)
    (defalias 'majutsu-mode-transient #'majutsu-dispatch))
  (when (fboundp 'majutsu-log-goto-@)
    (defalias 'majutsu-goto-current #'majutsu-log-goto-@)))

(provide 'init-local-majutsu)
;;; init-local-majutsu.el ends here
