;;; bug-reference-github.el --- Automatically set `bug-reference-url-format' in Github repositories.

;; Copyright (C) 2012, 2013 Arne Jørgensen

;; Author: Arne Jørgensen <arne@arnested.dk>
;; URL: https://github.com/arnested/bug-reference-github
;; Created: October 29, 2012
;; Version: 0.2.0
;; Keywords: programming, tools

;; This software is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Automatically set `bug-reference-url-format' and enable
;; `bug-reference-prog-mode' buffers from Github repositories.

;; What it does is:

;; 1. If `bug-reference-url-format' is not set and this appears to be
;;     part of a git working copy (we can locate a .git/config).

;; 2. Find the git remote repository (run 'git ls-remote --get-url').

;; 3. If the remote matches github.com set `bug-reference-url-format' to
;;     the correct Github issue URL (we set it buffer locally).

;; 4. Enable `bug-reference-prog-mode'.

;; To have `bug-reference-github' check every opened file:

;; (add-hook 'find-file-hook 'bug-reference-github-set-url-format)

;; or to check just `prog-mode' buffers (i.e. most programming major modes):

;; (add-hook 'prog-mode-hook 'bug-reference-github-set-url-format)

;;; Code:

(require 'vc-git)
(require 'bug-reference)

(defvar bug-reference-github-domains (list "github.com")
  "A list of GitHub domains.")

;;;###autoload
(defun bug-reference-github-set-url-format ()
  "Automatically set `bug-reference-url-format'.
Automatically set `bug-reference-url-format' and enable
`bug-reference-prog-mode' buffers from Github repositories.

What it does is:

1. If `bug-reference-url-format' is not set and this appears to be
    part of a git working copy (we can locate a .git/config).

2. Find the git remote repository (run 'git ls-remote --get-url').

3. If the remote matches github.com set `bug-reference-url-format' to
    the correct Github issue URL (we set it buffer locally).

4. Enable `bug-reference-prog-mode'."
  (unless bug-reference-url-format
    (when (vc-git-root (or (buffer-file-name) default-directory))
      (let ((remote (shell-command-to-string "git ls-remote --get-url")))
        (when (string-match (concat (regexp-opt bug-reference-github-domains t) "[/:]\\(.+?\\)\\(\\.git\\)?$") remote)
          (set (make-local-variable 'bug-reference-url-format)
               (concat "https://" (match-string-no-properties 1 remote) "/" (match-string-no-properties 2 remote) "/issues/%s"))
          (bug-reference-prog-mode))))))



(provide 'bug-reference-github)

;; Local Variables:
;; coding: utf-8
;; End:

;;; bug-reference-github.el ends here
