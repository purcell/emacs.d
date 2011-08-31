;;; sb-tigris.el --- shimbun backend for tigris.org ML

;; Copyright (C) 2005, 2007, 2009 Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>

;; Author: Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>
;; Keywords: news

;; This file is a part of shimbun.

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

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'shimbun)

(luna-define-class shimbun-tigris (shimbun) ())

(defvar shimbun-tigris-server-name "tigris.org")
(defvar shimbun-tigris-url-regexp "http://%s.tigris.org/servlets/SummarizeList?listName=%s")
(defcustom shimbun-tigris-group-alist
  '(("subversion"  . ("announce" "dev" "issues" "svn" "svn-breakage" "users"
		      "l10n-es" "l10n-fr"))
    ("rapidsvn"    . ("users" "issues" "announce" "dev" "cvs"))
    ("tortoisesvn" . ("dev" "announce" "patches"))
    ("scarab"      . ("announce" "dev" "users" "cvs" "issues"))
    ("xmlbasedsrs" . ("dev" "cvs" "announce" "users"))
    ("argouml"     . ("announce" "issues" "cvs" "users" "dev" "modules-dev"
		      "mda" "user-group-sweden" "users-espanol"))
    ("eyebrowse"   . ("announce" "dev" "users" "cvs" "issues"))
    ("binarycloud" . ("users" "issues" "cvs" "dev"))
    ("phpcreate"   . ("dev" "announce" "issues" "users"))
    ("lptools"     . ("dev" "cvs" "announce" "issues" "users"))
    ("maxq"        . ("users" "dev" "issues"))
    ("aut"         . ("dev" "cvs" "announce" "issues" "users"))
    ("current"     . ("dev" "cvs" "announce" "issues" "users"))
    ("readyset"    . ("dev" "cvs" "issues" "announce"))
    ("gef"         . ("issues" "dev" "users" "cvs" "announce"))
    ("axion"       . ("dev" "cvs" "announce" "issues" "users"))
    ("style"       . ("dev" "cvs" "announce" "issues" "users"))
    ("sstree"      . ("issues" "dev" "cvs"))
    ("readings"    . ("discuss" "issues" "announce" "cvs" "suggest"))
    ("spin"        . ("dev" "cvs" "announce" "issues" "users"))
    ("elmuth"      . ("users" "dev" "cvs" "announce" "issues"))
    ("ankhsvn"     . ("issues" "users" "announce" "cvs" "svn-commit"
		      "Draco-build"))
    )
  "*List of mailing lists serverd by Tigris.org."
  :group 'shimbun
  :type '(repeat
	  (cons
	   :format "%v" :indent 2
	   (string :format "Project Name: %v\n" :size 0)
	   (repeat
	    (string :format "ML: %v\n" :size 0))))
  )

(defmacro shimbun-tigris-get-project (shimbun)
  `(nth 0 (split-string
	   (shimbun-current-group-internal ,shimbun)
	   "\\.")))

(defmacro shimbun-tigris-get-ml-name (shimbun)
  `(nth 1 (split-string
	   (shimbun-current-group-internal ,shimbun)
	   "\\.")))

(luna-define-method shimbun-groups ((shimbun shimbun-tigris))
  "return groups : project.ml-name"
  (let (groups)
    (dolist (project shimbun-tigris-group-alist)
      (let ((mls (cdr project)))
	(dolist (ml-name mls)
	  (push (concat (car project) "." ml-name) groups))))
    groups))

(luna-define-method shimbun-index-url ((shimbun shimbun-tigris))
  (shimbun-expand-url
   (format shimbun-tigris-url-regexp
	   (shimbun-tigris-get-project shimbun)
	   (shimbun-tigris-get-ml-name shimbun)
	   )))

(defun shimbun-tigris-remove-amp (url)
  "Remove URL &amp; -> &"
  (save-match-data
    (while (string-match "&amp;" url)
      (setq url (replace-match "&" nil nil url))))
  url)

(luna-define-method shimbun-get-headers ((shimbun shimbun-tigris)
					 &optional range)
  (save-excursion
    (let ((case-fold-search t)
	  (pages (shimbun-header-index-pages range))
	  (link-regexp
	   (format "http://%s.tigris.org/servlets/BrowseList\\?listName=%s&\\(amp;\\)?by=date&\\(amp;\\)?.*"
		   (shimbun-tigris-get-project shimbun)
		   (shimbun-tigris-get-ml-name shimbun)))
	  indexes headers)
      (goto-char (point-min))
      (search-forward "inclsummarizeby") ;; top of links
      (while (re-search-forward "<a +href=\"\\([^\"]+\\)\"" nil t)
	(let ((url (match-string 1)))
	  (when (and url
		     (string-match link-regexp url))
	    (push (concat (shimbun-tigris-remove-amp url) "&paged=false") indexes))))
      (setq indexes (nreverse indexes))
      (catch 'stop
	(let ((count 0) url)
	  (while (and indexes
		      (if pages (<= (incf count) pages) t))
	    (erase-buffer)
	    (setq url (pop indexes))
	    (shimbun-retrieve-url url)
	    (goto-char (point-min))
	    (while (re-search-forward "<tr[^>]*>\\s *\
<td[^>]*>\\([^<]+\\)</td>\\s *\
<td[^>]*>\\s *<a *href=\"\\([^\"]+\\)\"[^>]*>\\([^<]+\\)</a>\\s *</td>\\s *\
<td[^>]*>\\([^<]+\\)</td>\\s *\
</tr>" nil t)
	      (let* ((from (match-string-no-properties 1))
		     (url (shimbun-tigris-remove-amp (match-string-no-properties 2)))
		     (title (match-string-no-properties 3))
		     (date (match-string-no-properties 4))
		     (count 0)
		     id)
		(when (string-match ".*msgNo=\\([0-9]+\\).*" url)
		  (setq count (string-to-number (match-string 1 url))))
		(setq id (format "<%d%%%s.%s.%s>"
				 count
				 (shimbun-tigris-get-ml-name shimbun)
				 (shimbun-tigris-get-project shimbun)
				 shimbun-tigris-server-name
				 ))
		(if (and (stringp date)
			 (string-match "\\([0-9]*\\)-\\([0-9]*\\)-\\([0-9]*\\)" date))
		    (setq date (shimbun-make-date-string
				(string-to-number (match-string 1 date))
				(string-to-number (match-string 2 date))
				(string-to-number (match-string 3 date))))
		  (setq date nil))
		;; change to raw page
		(setq url (shimbun-expand-url
			   (concat url "&raw=true")))
		(when (shimbun-search-id shimbun id)
		  (throw 'stop nil))
		(push (shimbun-create-header
		       count title
		       from
		       date
		       id "" 0 0 url)
		      headers))))))
      headers)))

(luna-define-method shimbun-make-contents ((shimbun shimbun-tigris)
					   header)
  (let ((case-fold-search t)
	(beg nil)
	(end nil))
    (when (search-forward "<PRE>")
      (forward-line 1)
      (beginning-of-line)
      (setq beg (point))
      (delete-region (point-min) beg))
    (when (search-forward "</PRE>")
      (forward-line -1)
      (end-of-line)
      (setq end (point))
      (delete-region end (point-max)))
    ;; replace "&lt;" -> "<"
    (goto-char (point-min))
    (while (search-forward "&lt;" nil t)
      (replace-match "<"))
    ;; header edit if deleted
    (goto-char (point-min))
    (when (and beg end)
      (when (re-search-forward "^$" nil t) ;; end of header
	(goto-char (match-end 0))
	(when (re-search-backward "^Message-ID" nil t)
	  (replace-match "X-Original-Message-ID"))) ;; replace msg-id to x-ori-msg-id
      (goto-char (point-min))
      (insert (concat "Message-ID: " (shimbun-header-id header) "\n")) ;; add msg-id
      (insert (concat "Xref: " (shimbun-header-xref header) "\n")) ;; add Xref
      ))
  (buffer-string))

(provide 'sb-tigris)

;;; sb-tigris.el ends here
