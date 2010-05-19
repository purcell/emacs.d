;;; gist.el --- Emacs integration for gist.github.com

;; Author: Christian Neukirchen <purl.org/net/chneukirchen>
;; Maintainer: Chris Wanstrath <chris@ozmm.org>
;; Contributors:
;; Will Farrington <wcfarrington@gmail.com>
;; Michael Ivey
;; Phil Hagelberg
;; Dan McKinley
;; Version: 0.4
;; Created: 21 Jul 2008
;; Keywords: gist git github paste pastie pastebin

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; Uses your local GitHub config if it can find it.
;; See http://github.com/blog/180-local-github-config

;;; Code:

(eval-when-compile (require 'cl))
(require 'xml)

(defvar github-user nil
  "If non-nil, will be used as your GitHub username without checking
git-config(1).")
(defvar github-token nil
  "If non-nil, will be used as your GitHub token without checking
git-config(1).")

(defvar gist-view-gist nil
  "If non-nil, automatically use `browse-url' to view gists after they're
posted.")

(defvar gist-supported-modes-alist '((action-script-mode . "as")
                                     (c-mode . "c")
                                     (c++-mode . "cpp")
                                     (clojure-mode . "clj")
                                     (common-lisp-mode . "lisp")
                                     (css-mode . "css")
                                     (diff-mode . "diff")
                                     (emacs-lisp-mode . "el")
                                     (erlang-mode . "erl")
                                     (haskell-mode . "hs")
                                     (html-mode . "html")
                                     (io-mode . "io")
                                     (java-mode . "java")
                                     (javascript-mode . "js")
                                     (jde-mode . "java")
                                     (js2-mode . "js")
                                     (lua-mode . "lua")
                                     (ocaml-mode . "ml")
                                     (objective-c-mode . "m")
                                     (perl-mode . "pl")
                                     (php-mode . "php")
                                     (python-mode . "py")
                                     (ruby-mode . "rb")
                                     (text-mode . "txt")
                                     (scala-mode . "scala")
                                     (sql-mode . "sql")
                                     (scheme-mode . "scm")
                                     (smalltalk-mode . "st")
                                     (sh-mode . "sh")
                                     (tcl-mode . "tcl")
                                     (tex-mode . "tex")
                                     (xml-mode . "xml")))



(defun* gist-request (url callback &optional params)
  "Makes a request to `url' asynchronously, notifying `callback' when
complete. The github parameters are included in the request. Optionally
accepts additional POST `params' as a list of (key . value) conses."
  (github-with-auth-info login token
    (let ((url-request-data (gist-make-query-string
                             `(("login" . ,login)
                               ("token" . ,token) ,@params)))
          (url-max-redirecton 5)
          (url-request-method "POST"))
      (url-retrieve url callback))))

;;;###autoload
(defun gist-region (begin end &optional private &optional callback)
  "Post the current region as a new paste at gist.github.com
Copies the URL into the kill ring.

With a prefix argument, makes a private paste."
  (interactive "r\nP")
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (name (file-name-nondirectory file))
         (ext (or (cdr (assoc major-mode gist-supported-modes-alist))
                  (file-name-extension file)
                  "txt")))
    (gist-request
     "http://gist.github.com/gists"
     (or callback 'gist-created-callback)
     `(,@(if private '(("action_button" . "private")))
       ("file_ext[gistfile1]" . ,(concat "." ext))
       ("file_name[gistfile1]" . ,name)
       ("file_contents[gistfile1]" . ,(buffer-substring begin end))))))

(defun gist-created-callback (status)
  (let ((location (cadr status)))
    (message "Paste created: %s" location)
    (when gist-view-gist
      (browse-url location))
    (kill-new location)
    (kill-buffer (current-buffer))))

(defun gist-make-query-string (params)
  "Returns a query string constructed from PARAMS, which should be
a list with elements of the form (KEY . VALUE). KEY and VALUE
should both be strings."
  (mapconcat
   (lambda (param)
     (concat (url-hexify-string (car param)) "="
             (url-hexify-string (cdr param))))
   params "&"))

;;;###autoload
(defun gist-region-private (begin end)
  "Post the current region as a new private paste at gist.github.com
Copies the URL into the kill ring."
  (interactive "r")
  (gist-region begin end t))

(defun github-config (key)
  "Returns a GitHub specific value from the global Git config."
  (let ((strip (lambda (string)
                 (if (> (length string) 0)
                     (substring string 0 (- (length string) 1)))))
        (git (executable-find "git")))
  (funcall strip (shell-command-to-string
                  (concat git " config --global github." key)))))

(defun github-set-config (key value)
  "Sets a GitHub specific value to the global Git config."
  (let ((git (executable-find "git")))
    (shell-command-to-string
     (format git " config --global github.%s %s" key value))))

(defun github-auth-info ()
  "Returns the user's GitHub authorization information.
Searches for a GitHub username and token in the global git config,
and returns (USERNAME . TOKEN). If nothing is found, prompts
for the info then sets it to the git config."
  (interactive)

  ;; If we've been called within a scope that already has this
  ;; defined, don't take the time to get it again.
  (if (boundp '*github-auth-info*)
      *github-auth-info*

    (let* ((user (or github-user (github-config "user")))
           (token (or github-token (github-config "token"))))

      (when (not user)
        (setq user (read-string "GitHub username: "))
        (github-set-config "user" user))

      (when (not token)
        (setq token (read-string "GitHub API token: "))
        (github-set-config "token" token))

      (cons user token))))

(defmacro github-with-auth-info (login token &rest body)
  "Binds the github authentication credentials to `login' and `token'.
The credentials are retrieved at most once within the body of this macro."
  (declare (indent 2))
  `(let ((*github-auth-info* (github-auth-info)))
     (destructuring-bind (,login . ,token) *github-auth-info*
       ,@body)))

;;;###autoload
(defun gist-buffer (&optional private)
  "Post the current buffer as a new paste at gist.github.com.
Copies the URL into the kill ring.

With a prefix argument, makes a private paste."
  (interactive "P")
  (gist-region (point-min) (point-max) private))

;;;###autoload
(defun gist-buffer-private ()
  "Post the current buffer as a new private paste at gist.github.com.
Copies the URL into the kill ring."
  (interactive)
  (gist-region-private (point-min) (point-max)))

;;;###autoload
(defun gist-region-or-buffer (&optional private)
  "Post either the current region, or if mark is not set, the current buffer as a new paste at gist.github.com
Copies the URL into the kill ring.

With a prefix argument, makes a private paste."
  (interactive "P")
  (condition-case nil
      (gist-region (point) (mark) private)
      (mark-inactive (gist-buffer private))))

;;;###autoload
(defun gist-region-or-buffer-private ()
  "Post either the current region, or if mark is not set, the current buffer as a new private paste at gist.github.com
Copies the URL into the kill ring."
  (interactive)
  (condition-case nil
      (gist-region-private (point) (mark))
      (mark-inactive (gist-buffer-private))))

(defvar gist-fetch-url "http://gist.github.com/%d.txt"
  "Raw Gist content URL format")

;;;###autoload
(defun gist-list ()
  "Displays a list of all of the current user's gists in a new buffer."
  (interactive)
  (message "Retrieving list of your gists...")
  (github-with-auth-info login token
    (gist-request
     (format "http://gist.github.com/api/v1/xml/gists/%s" login)
     'gist-lists-retrieved-callback)))

(defun gist-lists-retrieved-callback (status)
  "Called when the list of gists has been retrieved. Parses the result
and displays the list."
  (goto-char (point-min))
  (search-forward "<?xml")
  (let ((gists (gist-xml-cleanup
                     (xml-parse-region (match-beginning 0) (point-max)))))
    (kill-buffer (current-buffer))
    (with-current-buffer (get-buffer-create "*gists*")
      (goto-char (point-min))
      (save-excursion
        (kill-region (point-min) (point-max))
        (gist-insert-list-header)
        (mapc 'gist-insert-gist-link (xml-node-children (car gists)))

        ;; remove the extra newline at the end
        (delete-backward-char 1))

      ;; skip header
      (forward-line)
      (toggle-read-only t)
      (set-window-buffer nil (current-buffer)))))

(defun gist-insert-list-header ()
  "Creates the header line in the gist list buffer."
  (save-excursion
    (insert "  ID          Created                        "
            "Visibility  Description \n"))
  (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
    (overlay-put ov 'face 'header-line))
  (forward-line))

(defun gist-insert-gist-link (gist)
  "Inserts a button that will open the given gist when pressed."
  (let* ((data (gist-parse-gist gist))
         (repo (string-to-number (car data))))
    (mapc '(lambda (x) (insert (format "  %s    " x))) data)
    (make-text-button (line-beginning-position) (line-end-position)
                      'repo repo
                      'action 'gist-fetch-button
                      'face 'default))
  (insert "\n"))

(defun gist-fetch-button (button)
  "Called when a gist button has been pressed. Fetches and displays the gist."
  (gist-fetch (button-get button 'repo)))

(defun gist-parse-gist (gist)
  "Returns a list of the gist's attributes for display, given the xml list
for the gist."
  (let ((repo (gist-child-text 'repo gist))
        (created-at (gist-child-text 'created-at gist))
        (description (gist-child-text 'description gist))
        (public (if (string= (gist-child-text 'public gist) "true")
                    "public"
                  "private")))
    (list repo created-at public description)))

(defun gist-child-text (sym node)
  "Retrieves the text content of a child of a <gist> element."
  (let* ((children (xml-node-children node)))
    (car (xml-node-children (assq sym children)))))

(defun gist-xml-cleanup (xml-list)
  "Removes empty strings or whitespace nodes from the `xml-list'.
Borrowed from rss.el."
  (mapcar 'gist-xml-cleanup-node xml-list))

(defun gist-xml-cleanup-node (node)
  "Recursively removes whitespace and empty strings from the given xml `node'.
Borrowed from rss.el."
  (apply 'list
         (xml-node-name node)
         (xml-node-attributes node)
         (let (new)
           (dolist (child (xml-node-children node))
             (if (stringp child)
                 (or (string-match "\\`[ \t\n]+\\'" child)
                     (push child new))
               (push (gist-xml-cleanup-node child) new)))
           (nreverse new))))

;;;###autoload
(defun gist-fetch (id)
  "Fetches a Gist and inserts it into a new buffer
If the Gist already exists in a buffer, switches to it"
  (interactive "nGist ID: ")

  (let* ((gist-buffer-name (format "*gist %d*" id))
         (gist-buffer (get-buffer gist-buffer-name)))
    (if (bufferp gist-buffer)
      (switch-to-buffer-other-window gist-buffer)
      (progn
        (message "Fetching Gist %d..." id)
        (setq gist-buffer
              (url-retrieve-synchronously (format gist-fetch-url id)))
        (with-current-buffer gist-buffer
          (rename-buffer gist-buffer-name t)
          (goto-char (point-min))
          (search-forward-regexp "\n\n")
          (delete-region (point-min) (point))
          (set-buffer-modified-p nil))
        (switch-to-buffer-other-window gist-buffer)))))

(provide 'gist)
;;; gist.el ends here.