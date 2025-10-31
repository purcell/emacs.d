;;; init-local-majutsu.el --- Majutsu configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Majutsu Git porcelain configuration converted from Doom Emacs

;;; Code:

(defun majutsu-bookmark-main-and-push (&optional remote)
  "Set bookmark `main' to `@-' and push it to REMOTE using jj.
When called interactively, prompt for REMOTE if multiple remotes exist.
When REMOTE is nil, rely on jj's default remote selection."
  (interactive
   (list
    (when (require 'majutsu nil t)
      (let ((remotes (majutsu--get-git-remotes)))
        (when (> (length remotes) 1)
          (completing-read
           (format "Push remote (default %s): " (car remotes))
           remotes nil nil nil nil (car remotes)))))))
  (require 'majutsu)
  (unless (majutsu--root)
    (user-error "Not in a majutsu repository"))
  (let ((default-directory (majutsu--root)))
    ;; Set bookmark 'main' to @-
    (let* ((set-args '("bookmark" "set" "main" "--revision=@-"))
           (set-result (apply #'majutsu--run-command set-args)))
      (if (majutsu--handle-command-result
           set-args set-result
           "Set bookmark 'main' to @-"
           "Failed to set bookmark 'main'")
          ;; Push bookmark to remote
          (let* ((push-args (append '("git" "push")
                                    (and remote (list "--remote" remote))
                                    '("--bookmark" "main")))
                 (push-result (apply #'majutsu--run-command push-args))
                 (success-msg (if remote
                                  (format "Pushed bookmark 'main' to %s" remote)
                                "Pushed bookmark 'main'")))
            (when (majutsu--handle-push-result push-args push-result success-msg)
              (majutsu-log-refresh)))
        (message "Bookmark set failed, skipping push")))))

(use-package majutsu
  :ensure t
  :vc (:url "https://github.com/0WD0/majutsu"
            :rev :newest)
  :commands (majutsu majutsu-log
                     majutsu-rebase-transient majutsu-bookmark-transient majutsu-git-transient
                     majutsu-commit majutsu-describe majutsu-diff majutsu-diffedit-emacs majutsu-diffedit-smerge
                     majutsu-log-refresh majutsu-mode-transient majutsu-squash-transient
                     majutsu-abandon majutsu-undo majutsu-new majutsu-enter-dwim majutsu-goto-current
                     majutsu-edit-changeset majutsu-redo majutsu-log-transient)

  :init
  ;; Keybindings: SPC j prefix (evil leader) - set before loading
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'global
      (kbd "SPC j j") #'majutsu
      (kbd "SPC j c") #'majutsu-commit
      (kbd "SPC j D") #'majutsu-describe
      (kbd "SPC j d") #'majutsu-diff
      (kbd "SPC j p") #'majutsu-bookmark-main-and-push)))

(with-eval-after-load 'majutsu
  (with-eval-after-load 'evil
    ;; Disable evil-snipe in majutsu buffers if evil-snipe is loaded
    (when (fboundp 'turn-off-evil-snipe-mode)
      (add-hook 'majutsu-mode-hook #'turn-off-evil-snipe-mode))
    (when (fboundp 'turn-off-evil-snipe-override-mode)
      (add-hook 'majutsu-mode-hook #'turn-off-evil-snipe-override-mode))

    ;; Define keybindings only if the keymap exists
    (when (boundp 'majutsu-mode-map)
      ;; Define keybindings for normal and visual states
      (evil-define-key '(normal visual) majutsu-mode-map
        (kbd "g")     #'majutsu-git-transient
        (kbd ".")     #'majutsu-goto-current
        (kbd "R")     #'majutsu-log-refresh
        ;; (kbd "g r")   #'majutsu-log-refresh
        (kbd "c")     #'majutsu-commit
        (kbd "e")     #'majutsu-edit-changeset
        (kbd "u")     #'majutsu-undo
        (kbd "C-r")   #'majutsu-redo
        (kbd "s")     #'majutsu-squash-transient
        (kbd "l")     #'majutsu-log-transient
        (kbd "d")     #'majutsu-describe
        (kbd "x")     #'majutsu-abandon
        (kbd "b")     #'majutsu-bookmark-transient
        (kbd "r")     #'majutsu-rebase-transient
        (kbd "D")     #'majutsu-diff
        (kbd "E")     #'majutsu-diffedit-emacs
        (kbd "M")     #'majutsu-diffedit-smerge
        (kbd "?")     #'majutsu-mode-transient
        (kbd "]")     #'magit-section-forward-sibling
        (kbd "[")     #'magit-section-backward-sibling)

      ;; RET binding for normal state only
      (evil-define-key 'normal majutsu-mode-map
        (kbd "RET")   #'majutsu-enter-dwim)

      ;; Force Evil to update the keymap state
      (evil-normalize-keymaps))))

(provide 'init-local-majutsu)
;;; init-local-majutsu.el ends here
