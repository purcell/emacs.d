;;; gist.el --- Emacs integration for gist.github.com

;; Author: Christian Neukirchen <purl.org/net/chneukirchen>
;; Maintainer: Chris Wanstrath <chris@ozmm.org>
;; Contributors: 
;; Will Farrington <wcfarrington@gmail.com>
;; Michael Ivey
;; Phil Hagelberg
;; Version: 0.3
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

(defvar github-username "")
(defvar github-api-key "")

(defvar gist-supported-modes-alist '((action-script-mode . "as")
                                     (c-mode . "c")
                                     (c++-mode . "cpp")
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
                                     (perl-mode "pl")
                                     (php-mode . "php")
                                     (python-mode . "py")
                                     (ruby-mode . "rb")
                                     (text-mode . "txt")
                                     (sql-mode . "sql")
                                     (scheme-mode . "scm")
                                     (smalltalk-mode . "st")
                                     (sh-mode . "sh")
                                     (tcl-mode . "tcl")
                                     (tex-mode . "tex")
                                     (xml-mode . "xml")))

(defvar gist-view-gist nil
  "If non-nil, automatically use `browse-url' to view gists after they're posted.")

;;;###autoload
(defun gist-region (begin end &optional private)
  "Post the current region as a new paste at gist.github.com
Copies the URL into the kill ring.

With a prefix argument, makes a private paste."
  (interactive "r\nP")
  (destructuring-bind (login . token) (github-auth-info)
    (let* ((file (or (buffer-file-name) (buffer-name)))
           (name (file-name-nondirectory file))
           (ext (or (cdr (assoc major-mode gist-supported-modes-alist))
                    (file-name-extension file)
                    "txt"))
           (url-max-redirections 0)
           (url-request-method "POST")
           (url-request-data
            (gist-make-query-string
             `(,@(if private '(("private" . "1")))
               ("login" . ,login)
               ("token" . ,token)
               ("file_ext[gistfile1]" . ,(concat "." ext))
               ("file_name[gistfile1]" . ,name)
               ("file_contents[gistfile1]" . ,(buffer-substring begin end))))))
      (with-current-buffer (url-retrieve-synchronously "http://gist.github.com/gists")
        (re-search-backward "^Location: \\(.*\\)$")
        (message "Paste created: %s" (match-string 1))
        (if gist-view-gist (browse-url (match-string 1)))
        (kill-new (match-string 1))
        (kill-buffer (current-buffer))))))

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
                     (substring string 0 (- (length string) 1))))))
  (funcall strip (shell-command-to-string
                  (concat "git config --global github." key)))))

(defun github-set-config (key value)
  "Sets a GitHub specific value to the global Git config."
  (shell-command-to-string (format "git config --global github.%s %s" key value)))

(defun github-auth-info ()
  "Returns the user's GitHub authorization information.
Searches for a GitHub username and token in the global git config,
and returns (USERNAME . TOKEN). If nothing is found, prompts
for the info then sets it to the git config."
  (interactive)

  (let* ((user (github-config "user"))
         (token (github-config "token")))

    (when (not user)
      (setq user (read-string "GitHub username: "))
      (github-set-config "user" user))

    (when (not token)
      (setq token (read-string "GitHub API token: "))
      (github-set-config "token" token))

    (cons user token)))

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
          (beginning-of-buffer)
          (search-forward-regexp "\n\n")
          (delete-region (point-min) (point))
          (set-buffer-modified-p nil))
        (switch-to-buffer-other-window gist-buffer)))))

(provide 'gist)
;;; gist.el ends here.