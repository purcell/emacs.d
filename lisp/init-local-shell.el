;;; Package --- shell settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; (use-package eat
;;   :ensure t
;;   :config
;;   (eat-compile-terminfo)       ;; optional but advised
;;   (setq eat-kill-buffer-on-exit t))

(use-package eshell
  :ensure nil
  :defer t
  :hook ((eshell-directory-change . gopar/sync-dir-in-buffer-name)
         (eshell-mode . gopar/eshell-specific-outline-regexp)
         (eshell-mode . gopar/eshell-setup-keybinding)
         (eshell-mode . (lambda ()
                          (setq-local completion-styles '(basic)) ; maybe emacs21?
                          (setq-local corfu-count 10)
                          (setq-local corfu-auto nil)
                          (setq-local corfu-preview-current nil)
                          (setq-local completion-at-point-functions '(pcomplete-completions-at-point cape-file)))))
  :custom
  (eshell-scroll-to-bottom-on-input t)
  (eshell-highlight-prompt t)
  (eshell-history-size 1024)
  (eshell-hist-ignoredups t)
  (eshell-input-filter 'gopar/eshell-input-filter)
  (eshell-cd-on-directory t)
  (eshell-list-files-after-cd nil)
  (eshell-pushd-dunique t)
  (eshell-last-dir-unique t)
  (eshell-last-dir-ring-size 32)
  :config
  (advice-add #'eshell-add-input-to-history
              :around
              #'gopar/adviced-eshell-add-input-to-history)

  :init
  (defun gopar/eshell-setup-keybinding ()
    ;; Workaround since bind doesn't work w/ eshell??
    (define-key eshell-mode-map (kbd "C-c >") 'gopar/eshell-redirect-to-buffer)
    (define-key eshell-hist-mode-map (kbd "M-r") 'consult-history)
    ;; Align with zsh habit: M-l accepts current completion (company popup).
    (when (fboundp 'company-complete-selection)
      (define-key eshell-mode-map (kbd "M-l") 'company-complete-selection)))

  (defun gopar/adviced-eshell-add-input-to-history (orig-fun &rest r)
    "Cd to relative paths aren't that useful in history. Change to absolute paths."
    (require 'seq)
    (let* ((input (nth 0 r))
           (args (progn
                   (set-text-properties 0 (length input) nil input)
                   (split-string input))))
      (if (and (equal "cd" (nth 0 args))
               (not (seq-find (lambda (item)
                                ;; Don't rewrite "cd /ssh:" in history.
                                (string-prefix-p "/ssh:" item))
                              args))
               (not (seq-find (lambda (item)
                                ;; Don't rewrite "cd -" in history.
                                (string-equal "-" item))
                              args)))
          (apply orig-fun (list (format "cd %s"
                                        (expand-file-name (concat default-directory
                                                                  (nth 1 args))))))
        (apply orig-fun r))))

  (defun gopar/eshell-input-filter (input)
    "Do not save on the following:
       - empty lines
       - commands that start with a space, `ls`/`l`/`lsd`"
    (and
     (eshell-input-filter-default input)
     (eshell-input-filter-initial-space input)
     (not (string-prefix-p "ls " input))
     (not (string-prefix-p "lsd " input))
     (not (string-prefix-p "l " input))))

  (defun eshell/cat-with-syntax-highlighting (filename)
    "Like cat(1) but with syntax highlighting.
Stole from aweshell"
    (let ((existing-buffer (get-file-buffer filename))
          (buffer (find-file-noselect filename)))
      (eshell-print
       (with-current-buffer buffer
         (if (fboundp 'font-lock-ensure)
             (font-lock-ensure)
           (with-no-warnings
             (font-lock-fontify-buffer)))
         (let ((contents (buffer-string)))
           (remove-text-properties 0 (length contents) '(read-only nil) contents)
           contents)))
      (unless existing-buffer
        (kill-buffer buffer))
      nil))
  (advice-add 'eshell/cat :override #'eshell/cat-with-syntax-highlighting)

  (defun gopar/sync-dir-in-buffer-name ()
    "Update eshell buffer to show directory path.
Stolen from aweshell."
    (let* ((root (projectile-project-root))
           (root-name (projectile-project-name root)))
      (if root-name
          (rename-buffer (format "*eshell %s* %s" root-name (s-chop-prefix root default-directory)) t)
        (rename-buffer (format "*eshell %s*" default-directory) t))))

  (defun gopar/eshell-redirect-to-buffer (buffer)
    "Auto create command for redirecting to buffer."
    (interactive (list (read-buffer "Redirect to buffer: ")))
    (insert (format " >>> #<%s>" buffer)))

  (defun gopar/eshell-specific-outline-regexp ()
    (setq-local outline-regexp eshell-prompt-regexp)))

(use-package eshell-syntax-highlighting
  :ensure t
  :after eshell
  :hook (eshell-first-time-mode . eshell-syntax-highlighting-global-mode)
  :init
  (defface eshell-syntax-highlighting-invalid-face
    '((t :inherit diff-error))
    "Face used for invalid Eshell commands."
    :group 'eshell-syntax-highlighting))

(require 'eshell)
(require 'em-dirs)

(defun eshell/pure-git-branch ()
  "Returns the current git branch."
  (let ((branch (car (loop for match in (split-string (shell-command-to-string "git branch") "\n")
                           when (string-match "^\*" match)
                           collect match))))
    (if (not (eq branch nil))
        (concat " " (substring branch 2))
      "")))

(defun eshell/pure-git-dirty ()
  "Returns * if the git status is dirty."
  (let ((status (shell-command-to-string "git status --porcelain")))
    (if (string-match-p "." status)
        " *"
      "")))

(use-package eshell-git-prompt
  :after eshell
  :ensure t)

;; (use-package eshell-prompt-extras
;;   :ensure t
;;   :after esh-opt
;;   :config
;;   (setq eshell-highlight-prompt nil
;;         eshell-prompt-function 'epe-theme-dakrone))

(use-package eshell-z
  :after eshell
  :ensure t)

(add-hook 'eshell-mode-hook
          (defun my-eshell-mode-hook ()
            (require 'eshell-z)))

(defun eshell-current-directory (&optional directory)
  "Open eshell current `default-directory' or DIRECTORY."
  (interactive)
  (let ((current-dir (or directory default-directory))
        (eshell-buffer (or (get-buffer "*eshell*")
                           (eshell))))
    (switch-to-buffer eshell-buffer)
    (eshell/cd current-dir)
    (eshell-next-prompt)
    ;; Regenerate prompt to show current directory.
    ;; Avoid sending any half written input commands
    (if (eobp)
        (eshell-send-input nil nil nil)
      (move-end-of-line nil)
      (eshell-kill-input)
      (eshell-send-input nil nil nil)
      (yank))))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  ;; If you have use-package-hook-name-suffix set to nil, uncomment and use the
  ;; line below instead:
  ;; :hook (eshell-mode-hook . esh-autosuggest-mode)
  :ensure t)

(provide 'init-local-shell)
;;; init-local-shell.el ends here
