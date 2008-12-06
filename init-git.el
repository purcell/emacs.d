;;(setq load-path (cons (expand-file-name "/usr/share/doc/git-core/contrib/emacs") load-path))
;; Downloaded from http://git.kernel.org/?p=git/git.git ;a=tree;hb=HEAD;f=contrib/emacs
(require 'vc-git)
(when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
(require 'git)
(autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t)


(require 'magit)
(defun magit-status-here ()
  (interactive)
  (magit-status default-directory))
(global-set-key [(meta f12)] 'magit-status-here)
(global-set-key [(shift meta f12)] 'magit-status)

;;----------------------------------------------------------------------------
;; git-svn conveniences
;;----------------------------------------------------------------------------
(eval-after-load "compile"
  '(progn
     (mapcar (lambda (defn) (add-to-list 'compilation-error-regexp-alist-alist defn))
             (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
                   '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
     (mapcar (lambda (defn) (add-to-list 'compilation-error-regexp-alist defn))
             (list 'git-svn-updated 'git-svn-needs-update))))

(defun git-svn (dir)
  (interactive "DSelect directory: ")
  (let* ((default-directory (git-get-top-dir dir))
         (compilation-buffer-name-function (lambda (major-mode-name) "*git-svn*")))
    (compile (concat "git svn "
                     (ido-completing-read "git-svn command: "
                                          (list "rebase" "dcommit" "fetch" "log") nil t)))))


;; My config requires a local GitHub config if it can find it.
;; See http://github.com/blog/180-local-github-config
(require 'gist)
