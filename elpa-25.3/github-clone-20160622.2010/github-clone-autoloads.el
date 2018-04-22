;;; github-clone-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "github-clone" "github-clone.el" (23009 21889
;;;;;;  0 0))
;;; Generated autoloads from github-clone.el

(autoload 'github-clone-add-parent-remote "github-clone" "\
Obtain the parent of CHILD-REMOTE and add it as a remote.

\(fn CHILD-REMOTE)" t nil)

(autoload 'github-clone-add-source-remote "github-clone" "\
Obtain the original ancestor of CHILD-REMOTE and add it as a remote.

\(fn CHILD-REMOTE)" t nil)

(autoload 'github-clone-fork-remote "github-clone" "\
Fork REMOTE to the current user.

\(fn &optional REMOTE)" t nil)

(autoload 'github-clone-add-existing-remote "github-clone" "\
Add a remote that is an existing fork of SELECTED-REMOTE-NAME.

When USE-SOURCE is set, use the source remote of SELECTED-REMOTE-NAME

\(fn &optional SELECTED-REMOTE-NAME USE-SOURCE)" t nil)

(autoload 'github-clone "github-clone" "\
Fork and clone USER-REPO-URL into DIRECTORY.

USER-REPO-URL can be any of the forms:

  repository
  user/repository
  organization/repository
  https://github.com/user/repository
  git@github.com:user/repository.git
  https://github.com/user/repository.el.git

It will immediately clone the repository (as the origin) to
DIRECTORY.  Then it prompts to fork the repository and add a
remote named after the github username to the fork.

\(fn USER-REPO-URL DIRECTORY)" t nil)

(autoload 'eshell/github-clone "github-clone" "\
An eshell alias for `github-clone'.

Fork and clone USER-REPO-URL into DIRECTORY, which defaults to
the current directory in eshell (`default-directory').

\(fn USER-REPO-URL &optional DIRECTORY)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; github-clone-autoloads.el ends here
