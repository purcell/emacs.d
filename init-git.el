(setq magit-save-some-buffers nil
      magit-process-popup-time 10
      magit-completing-read-function 'magit-ido-completing-read)

(defun magit-status-somedir ()
  (interactive)
  (let ((current-prefix-arg t))
    (magit-status default-directory)))

(global-set-key [(meta f12)] 'magit-status)
(global-set-key [(shift meta f12)] 'magit-status-somedir)

(when *is-a-mac*
  (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)]))))

(eval-after-load 'magit
  '(progn
     (require 'magit-svn)))

(autoload 'rebase-mode "rebase-mode")
(add-to-list 'auto-mode-alist '("git-rebase-todo" . rebase-mode))

(add-to-list 'auto-mode-alist '("\\(?:\\.gitconfig\\|\\.gitmodules\\|config\\)$" . conf-mode))

;;----------------------------------------------------------------------------
;; git-svn conveniences
;;----------------------------------------------------------------------------
(eval-after-load 'compile
  '(progn
     (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
                         '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
       (add-to-list 'compilation-error-regexp-alist-alist defn))
     (dolist (defn '(git-svn-updated git-svn-needs-update))
       (add-to-list 'compilation-error-regexp-alist defn))))

(defvar git-svn--available-commands nil "Cached list of git svn subcommands")

(defun git-svn (dir)
  "Run git svn"
  (interactive "DSelect directory: ")
  (unless git-svn--available-commands
    (setq git-svn--available-commands
          (string-all-matches "^  \\([a-z\\-]+\\) +" (shell-command-to-string "git svn help") 1)))
  (let* ((default-directory (vc-git-root dir))
         (compilation-buffer-name-function (lambda (major-mode-name) "*git-svn*")))
    (compile (concat "git svn "
                     (ido-completing-read "git-svn command: " git-svn--available-commands nil t)))))



(provide 'init-git)
