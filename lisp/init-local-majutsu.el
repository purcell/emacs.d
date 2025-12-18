;;; init-local-majutsu.el --- Majutsu configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Majutsu Git porcelain configuration converted from Doom Emacs

;;; Code:

(defun init-local-majutsu--read-remote ()
  "Prompt for a Git remote when multiple remotes exist.
Return nil to let jj pick the default remote."
  (unless (require 'majutsu nil t)
    (user-error "Majutsu is not available"))
  (let ((remotes (majutsu--get-git-remotes)))
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

(defun init-local-majutsu--push-bookmark-main (remote)
  "Push bookmark `main' to REMOTE using jj."
  (let* ((remote (init-local-majutsu--normalize-remote remote))
         (push-args (append '("git" "push")
                            (and remote (list "--remote" remote))
                            '("--bookmark" "main")))
         (success-msg (if remote
                          (format "Pushed bookmark 'main' to %s" remote)
                        "Pushed bookmark 'main'")))
    (majutsu-run-jj-async
     push-args
     (lambda (result)
       (let ((ok (if (fboundp 'majutsu--handle-push-result)
                     (majutsu--handle-push-result push-args result success-msg)
                   (progn
                     (init-local-majutsu--message-output result)
                     t))))
         (when ok
           (when (fboundp 'majutsu-log-refresh)
             (majutsu-log-refresh)))))
     (lambda (err)
       (if (fboundp 'majutsu--handle-push-result)
           (majutsu--handle-push-result push-args err success-msg)
         (init-local-majutsu--message-output err))
       (message "Push failed")))))

(defun majutsu-bookmark-main-and-push (&optional remote)
  "Set bookmark `main' to `@-' and push it to REMOTE using jj.
When called interactively, prompt for REMOTE if multiple remotes exist.
When REMOTE is nil, rely on jj's default remote selection."
  (interactive
   (list (init-local-majutsu--read-remote)))
  (require 'majutsu)
  (majutsu--root)
  (majutsu-run-jj-async
   '("bookmark" "set" "main" "-r" "@-")
   (lambda (result)
     (init-local-majutsu--message-output result)
     (init-local-majutsu--push-bookmark-main remote))
   (lambda (err)
     (init-local-majutsu--message-output err)
     (message "Failed to set bookmark 'main'"))))

(use-package majutsu
  :ensure t
  :vc (:url "https://github.com/0WD0/majutsu"
            :rev "c6f8fa784b30783ccdccb34ed1fcb192ef31a385")
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
