;;; w3m-mail.el --- an interface to mail-user-agent for sending web pages

;; Copyright (C) 2006, 2009 TSUCHIYA Masatoshi

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: w3m, WWW, hypermedia

;; This file is a part of emacs-w3m.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This module provides the `w3m-mail' command which enables you to
;; send web pages as mails respecting those content types (typically
;; text/html).  Currently this program works if and only if you set
;; the `mail-user-agent' variable to one of the following agents:
;;      `gnus-user-agent'
;;      `message-user-agent'
;;      `mew-user-agent'
;;      `vm-user-agent'
;;      `wl-user-agent'
;; To send the page you are looking at, type `M-x w3m-mail' or click
;; the menu button, fill message headers properly, and type `C-c C-c'.

;;; Code:

(require 'w3m)

(defcustom w3m-mail-subject '("Emailing:" url)
  "A list of strings and symbols used to generate the subject header.
Valid symbols include `url' which is replaced with the url of the page
and `title' which is replaced with the page title.  You can also use
just a string for this variable."
  :group 'w3m
  :type '(radio (editable-list :format "\n%v%i\n"
			       (radio-button-choice
				(const :format "%v " url)
				(const :format "%v " title)
				string))
		string
		(const :format "no subject" nil)))

(defvar w3m-mail-user-agent-compose-function-alist
  (let ((alist '((gnus-user-agent . w3m-mail-compose-with-mml)
		 (message-user-agent . w3m-mail-compose-with-mml)
		 (mew-user-agent . w3m-mail-compose-with-mew)
		 (vm-user-agent . w3m-mail-compose-with-vm)
		 (wl-user-agent . w3m-mail-compose-with-semi)))
	composer)
    (delq nil (mapcar (lambda (agent)
			(if (setq composer (cdr (assq agent alist)))
			    (cons agent composer)))
		      w3m-mail-user-agents)))
  "Alist of mail user agents and functions to compose a mail.
The function will be called with the arguments `source', `url',
`charset', `content-type', `to', `subject', and `other-headers'; where
`source' is a string containing the page source, `url' is the url of
the page, `charset' is a charset that the page uses, `content-type' is
the one such as \"text/html\", and the rest are the same as those of
`compose-mail'.")

(eval-when-compile
  (autoload 'message-add-action "message")
  (autoload 'mml-insert-empty-tag "mml")
  (autoload 'vm-mime-attach-buffer "vm-mime")
  (condition-case nil
      (require 'mime-edit)
    (error
     (dolist (symbol '(encode-mime-charset-region
		       detect-mime-charset-region
		       std11-wrap-as-quoted-string
		       mime-find-file-type
		       mime-edit-insert-tag
		       mime-edit-define-encoding
		       mime-encode-region))
       (defalias symbol 'ignore)))))

(eval-and-compile
  (autoload 'mm-find-mime-charset-region "mm-util")
  (autoload 'w3m-mail-compose-with-mew "mew-w3m"
    "Compose a mail using Mew." t))

(defun w3m-mail-make-subject ()
  "Return a string used for the Subject header."
  (cond ((consp w3m-mail-subject)
	 (w3m-replace-in-string
	  (w3m-replace-in-string
	   (mapconcat (lambda (elem)
			(cond ((eq elem 'url) w3m-current-url)
			      ((eq elem 'title) w3m-current-title)
			      ((stringp elem) elem)
			      (t (format "%s" elem))))
		      w3m-mail-subject
		      " ")
	   "[\t\n ]+" " ")
	  "\\(?:\\` \\| \\'\\)" ""))
	((stringp w3m-mail-subject) w3m-mail-subject)
	(t "(no subject)")))

(defun w3m-mail-compute-base-url ()
  "Compute a base url of the page if it is not provided."
  (let ((url (substring w3m-current-url 15)))
    (unless (string-match "\\`about:" url)
      (save-excursion
	(goto-char (point-min))
	(let ((case-fold-search t)
	      start end)
	  (unless (and (setq start (search-forward "<head>" nil t))
		       (setq end (search-forward "</head>" nil t))
		       (progn
			 (goto-char start)
			 (re-search-forward "<base[\t\n\r ]+" end t))
		       (w3m-parse-attributes (href) (> (length href) 0)))
	    (substring (w3m-expand-url "x" url) 0 -1)))))))

(defun w3m-mail-embed-base-url (source base-url)
  "Embed BASE-URL in SOURCE."
  (with-temp-buffer
    (w3m-static-unless (featurep 'xemacs)
      (set-buffer-multibyte t))
    (setq case-fold-search t)
    (insert source)
    (goto-char (point-min))
    (while (search-forward "\r\n" nil t)
      (replace-match "\n"))
    (goto-char (point-min))
    (let ((nohead t)
	  (points (list (point-min) (point-min)))
	  (margin 0))
      (when (re-search-forward "\\(<html>\\)[\t\n ]*" nil t)
	(setq points (list (match-end 1) (match-end 0))
	      margin (current-column)))
      (when (re-search-forward "\\(<head>\\)[\t\n ]*" nil t)
	(setq nohead nil
	      points (list (match-end 1) (match-end 0))
	      margin (current-column)))
      (setq margin (make-string margin ? ))
      (goto-char (car points))
      (apply 'delete-region points)
      (if nohead
	  (insert "\n" margin "<head><base href=\"" base-url "\"></head>\n"
		  margin)
	(insert "\n" margin "<base href=\"" base-url "\">\n" margin)))
    (buffer-string)))

(defun w3m-mail-goto-body-and-clear-body ()
  "Go to the beginning of the body and clear the body."
  (goto-char (point-min))
  (if (re-search-forward (concat "^\\(?:"
				 (regexp-quote mail-header-separator)
				 "\\)?\n")
			 nil 'move)
      (delete-region (point) (point-max))
    (insert (if (bolp) "\n" "\n\n"))))

(defun w3m-mail-position-point (bob)
  "Go to empty or bogus header, otherwise the beginning of the body BOB."
  (goto-char (point-min))
  (when (re-search-forward "^\\(Subject: \\)(no subject)\\|\
^\\([0-9A-Za-z-]+: ?\\)[\t ]*\n\\(?:[\t ]+\n\\)*[^\t ]"
			   bob 'move)
    (goto-char (or (match-end 1) (match-end 2)))))

(defun w3m-mail-compose-with-mml (source url charset content-type
					 to subject other-headers)
  "Compose a mail using MML."
  (let ((buffer (generate-new-buffer " *w3m-mail*")))
    (with-current-buffer buffer
      (w3m-static-unless (featurep 'xemacs)
	(set-buffer-multibyte (not (string-match "\\`image/" content-type))))
      (insert source))
    (if (eq mail-user-agent 'gnus-user-agent)
	(progn
	  (require 'gnus)
	  (let (gnus-newsgroup-name)
	    (compose-mail to subject other-headers)))
      (compose-mail to subject other-headers))
    (message-add-action `(kill-buffer ,buffer) 'exit 'kill 'postpone 'send)
    (w3m-mail-goto-body-and-clear-body)
    (w3m-mail-position-point
     (prog1
	 (point)
       (mml-insert-empty-tag
	'part
	'type content-type
	'buffer (buffer-name buffer)
	;; Use the base64 encoding if the body contains non-ASCII text
	;; or very long lines which might be broken by MTAs.
	'encoding "base64"
	'charset (when charset (symbol-name charset))
	'disposition "inline"
	'description url)))))

;; This function is implemented in mew-w3m.el.
;; (defun w3m-mail-compose-with-mew (source url charset content-type
;;                                         to subject other-headers)
;;   "Compose a mail using Mew.")

(defun w3m-mail-compose-with-vm (source url charset content-type
					to subject other-headers)
  "Compose a mail using VM."
  (let* ((coding (and charset (w3m-charset-to-coding-system charset)))
	 (multibytep (and (not coding)
			  (or charset
			      (and (not (string-match "\\`image/"
						      content-type))
				   (w3m-static-if (featurep 'xemacs)
				       (string-match "[^\000-\177]" source)
				     (multibyte-string-p source))))))
	 (buffer (generate-new-buffer " *w3m-mail*")))
    (with-current-buffer buffer
      (w3m-static-unless (featurep 'xemacs)
	(set-buffer-multibyte (and (not coding) multibytep)))
      (cond (coding
	     (insert (encode-coding-string source coding)))
	    (multibytep
	     (insert source)
	     (when (and (setq charset (car (mm-find-mime-charset-region
					    (point-min) (point-max))))
			(setq coding (w3m-charset-to-coding-system charset)))
	       (w3m-static-if (featurep 'xemacs)
		   (encode-coding-region (point-min) (point-max) coding)
		 (insert (prog1
			     (encode-coding-string (buffer-string) coding)
			   (erase-buffer)
			   (set-buffer-multibyte nil))))))
	    (t
	     (insert source))))
    (require 'vm-startup)
    (compose-mail to subject other-headers)
    (add-to-list 'mail-send-actions `(kill-buffer ,buffer))
    (w3m-add-local-hook 'kill-buffer-hook `(lambda nil (kill-buffer ,buffer)))
    (w3m-mail-goto-body-and-clear-body)
    (w3m-mail-position-point
     (prog1
	 (point)
       (vm-mime-attach-buffer buffer content-type
			      (when charset (symbol-name charset))
			      url)))))

(defun w3m-mail-compose-with-semi (source url charset content-type
					  to subject other-headers)
  "Compose a mail using SEMI."
  (require 'mime-edit)
  (let* ((content-type (and content-type
			    (split-string (downcase content-type) "/")))
	 (basename (file-name-nondirectory (w3m-url-strip-query url)))
	 (filename (cond
		    ((and (string-match "^[\t ]*$" basename)
			  (equal content-type '("text" "html")))
		     "index.html")
		    ((string-match "^[\t ]*$" basename)
		     "dummy")
		    (t
		     basename)))
	 (type (or (nth 0 content-type) "text"))
	 (subtype (or (nth 1 content-type) "html"))
	 parameters
	 (encoding "base64")
	 (disposition-type "inline")
	 disposition-params
	 (guess (mime-find-file-type filename))
	 (textp (string= type "text")))
    (when (and guess
	       (string= (nth 0 guess) type)
	       (string= (nth 1 guess) subtype))
      (setq parameters (nth 2 guess)
	    encoding (or (nth 3 guess) encoding)
	    disposition-type (or (nth 4 guess) disposition-type)
	    disposition-params (nth 5 guess)))
    (compose-mail to subject other-headers)
    (w3m-mail-goto-body-and-clear-body)
    (let ((parameters-to-string
	   (lambda (parameters)
	     (when parameters
	       (mapconcat
		(lambda (parameter)
		  (concat "; " (car parameter)
			  "=" (if (eq (cdr parameter) 'file)
				  (std11-wrap-as-quoted-string filename)
				(cdr parameter))))
		parameters
		""))))
	  (body (point))
	  (edit-buffer (current-buffer))
	  work-buffer)
      (with-temp-buffer
	(if textp
	    (progn
	      (insert source)
	      (unless charset
		(setq charset (detect-mime-charset-region (point-min)
							  (point-max))))
	      (when charset
		(setq parameters (cons (cons "charset" (symbol-name charset))
				       parameters))
		(encode-mime-charset-region (point-min) (point-max) charset)))
	  (set-buffer-multibyte nil)
	  (insert source))
	(mime-encode-region (point-min) (point-max) encoding)
	(setq work-buffer (current-buffer))
	(set-buffer edit-buffer)
	(mime-edit-insert-tag
	 type subtype
	 (concat (funcall parameters-to-string parameters)
		 "\nContent-Disposition: " disposition-type
		 (funcall parameters-to-string disposition-params)
		 "\nContent-Description: " url))
	(mime-edit-define-encoding encoding)
	(save-restriction
	  (narrow-to-region (point) (point))
	  (insert-buffer-substring work-buffer)
	  (unless (bolp)
	    (insert "\n"))
	  (when (or (string= disposition-type "attachment")
		    (not (member encoding '("7bit" "8bit" "binary"))))
	    (add-text-properties
	     (point-min) (point-max) '(invisible t mime-edit-invisible t)))))
      (w3m-mail-position-point body))))

(defun w3m-mail (&optional headers)
  "Send a web page as a mail.
By default the subject is generated according to `w3m-mail-subject'.
The optional HEADERS is a list in which each element is a cons of the
symbol of a header name and a string.  Here is an example to use this
function:

\(w3m-mail '((To . \"foo@bar\") (Subject . \"The emacs-w3m home page\")))"
  (interactive (unless (eq major-mode 'w3m-mode)
		 (error "`%s' must be invoked from an emacs-w3m buffer"
			this-command)))
  (let ((composer (cdr (assq mail-user-agent
			     w3m-mail-user-agent-compose-function-alist)))
	;; Don't move the history position.
	(w3m-history-reuse-history-elements t)
	source base url charset content-type to subject)
    (cond
     ((not composer)
      (error "`%s' is not supported (yet) by `w3m-mail'" mail-user-agent))
     ((not w3m-current-url)
      (error "The source for this page is not available"))
     ((string-match "\\`about://source/" w3m-current-url)
      (setq source (buffer-string)
	    base (w3m-mail-compute-base-url))
      (w3m-view-source)
      (setq url w3m-current-url
	    charset (w3m-coding-system-to-charset w3m-current-coding-system)
	    content-type (or (w3m-arrived-content-type w3m-current-url)
			     (w3m-content-type w3m-current-url)))
      (w3m-view-source))
     ((string-match "\\`about://header/" w3m-current-url)
      (w3m-view-source)
      (setq source (buffer-string)
	    base (w3m-mail-compute-base-url))
      (w3m-view-source)
      (setq url w3m-current-url
	    charset (w3m-coding-system-to-charset w3m-current-coding-system)
	    content-type (or (w3m-arrived-content-type w3m-current-url)
			     (w3m-content-type w3m-current-url)))
      (w3m-view-header))
     (t
      (setq url w3m-current-url
	    charset (w3m-coding-system-to-charset w3m-current-coding-system)
	    content-type (or (w3m-arrived-content-type w3m-current-url)
			     (w3m-content-type w3m-current-url)))
      (w3m-view-source)
      (setq source (buffer-string)
	    base (w3m-mail-compute-base-url))
      (w3m-view-source)))
    (when (and base (string= "text/html" content-type))
      (setq source (w3m-mail-embed-base-url source base)))
    (setq to (or (assq 'To headers) (assq 'to headers))
	  subject (or (assq 'Subject headers) (assq 'subject headers)))
    (when (or to subject)
      (setq headers (delq to (delq subject (copy-sequence headers)))
	    to (cdr to)
	    subject (cdr subject)))
    (unless subject
      (setq subject (let ((w3m-current-url url)) (w3m-mail-make-subject))))
    (funcall composer source url charset content-type to subject headers)))

;;; w3m-mail.el ends here
