;;; sb-geocrawler.el --- shimbun backend for geocrawler.com.

;; Copyright (C) 2002, 2003, 2005 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
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

;;; Commentary:

;; Before reading any group which is archived at geocrawler.com, call
;; `shimbun-geocrawler-add-group'.

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-geocrawler (shimbun) ())

(defvar shimbun-geocrawler-url "http://www.geocrawler.com/archives/")

(defcustom shimbun-geocrawler-group-alist nil
  "Table of mailing lists which is archived by geocrawler.com."
  :group 'shimbun
  :type '(repeat
	  (group :indent 0
		 (string :format "Name: %v\n" :size 0)
		 (string :format "   List ID: %v\n" :size 0)
		 (radio :format "  Reply-To: %v"
			(const :format "None " nil)
			(string :format "Address: %v\n" :size 0))
		 (radio :format "    X-Face: %v"
			(const :format "None " nil)
			(string :format "%t: %v\n" :size 0)))))

(defvar shimbun-geocrawler-content-start "<P>&nbsp;<P>")
(defvar shimbun-geocrawler-content-end "</BODY>")

(luna-define-method shimbun-groups ((shimbun shimbun-geocrawler))
  (mapcar 'car shimbun-geocrawler-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-geocrawler))
  (concat shimbun-geocrawler-url
	  "3/"
	  (nth 1 (assoc (shimbun-current-group-internal shimbun)
			 shimbun-geocrawler-group-alist))
	  "/"))

(luna-define-method shimbun-reply-to ((shimbun shimbun-geocrawler))
  (nth 2 (assoc (shimbun-current-group-internal shimbun)
		shimbun-geocrawler-group-alist)))

(luna-define-method shimbun-x-face ((shimbun shimbun-geocrawler))
  (nth 3 (assoc (shimbun-current-group-internal shimbun)
		shimbun-geocrawler-group-alist)))

(luna-define-method shimbun-headers ((shimbun shimbun-geocrawler)
				     &optional range)
  (shimbun-geocrawler-headers shimbun range))

(defun shimbun-geocrawler-headers (shimbun &optional range)
  (cond
   ((eq range 'last) (setq range 1))
   ((eq range 'all) (setq range nil)))
  (let ((url (shimbun-index-url shimbun))
	(years)
	(headers)
	(newest-index t)
	(case-fold-search t))
    (catch 'stop
      (with-temp-buffer
	(shimbun-retrieve-url url)
	(while (re-search-forward
		"<a href=\"\\([0-9][0-9][0-9][0-9]\\)\">" nil t)
	  (push (match-string 1) years))
	(dolist (year years)
	  (let ((url (concat url year "/"))
		(indexes))
	    (erase-buffer)
	    (shimbun-retrieve-url url)
	    (while (re-search-forward
		    "<a href=\"\\(\\([1-9]\\|1[012]\\)/0/\\)\">" nil t)
	      (push (match-string 1) indexes))
	    (dolist (index indexes)
	      (and range
		   (< (setq range (1- range)) 0)
		   (throw 'stop nil))
	      (let ((url (concat url index)))
		(while (when url
			 (erase-buffer)
			 (shimbun-retrieve-url url newest-index))
		  (setq newest-index nil)
		  (while (re-search-forward
			  "<a href=\"\\([0-9]+\\)/\"><img src=\"/img/msg.gif\"[^>]*> *&nbsp; *"
			  nil t)
		    (let ((xref
			   (concat
			    "http://www.geocrawler.com/mail/msg_raw.php3?msg_id="
			    (match-string 1)))
			  (id (concat "<" (match-string 1) "@geocrawler.com>"))
			  (eol (point-at-eol)))
		      (when (shimbun-search-id shimbun id)
			(throw 'stop nil))
		      (push (shimbun-make-header
			     0
			     (shimbun-mime-encode-string
			      (buffer-substring
			       (point)
			       (progn
				 (search-forward "</td><td>" nil eol)
				 (match-beginning 0))))
			     (shimbun-mime-encode-string
			      (buffer-substring
			       (point)
			       (progn
				 (search-forward "</td><td>" nil eol)
				 (match-beginning 0))))
			     (when (looking-at "\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)&nbsp;\\([0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\)</td>")
			       (shimbun-make-date-string
				(string-to-number (match-string 3))
				(string-to-number (match-string 1))
				(string-to-number (match-string 2))
				(match-string 4)))
			     id nil nil nil xref)
			    headers)))
		  (setq url
			(when (re-search-forward
			       "<a href=\"\\(../[0-9]+\\)\"><b>Next Results"
			       nil t)
			  (shimbun-expand-url (concat (match-string 1) "/")
					      url))))))))))
    headers))

(eval-and-compile
  ;; This is mainly for avoiding a compile warning for old Emacsen.
  (autoload 'customize-save-variable "cus-edit"))

(defun shimbun-geocrawler-add-group ()
  "Add an group to `shimbun-geocrawler-group-alist' interactively."
  (interactive)
  (let ((url (shimbun-expand-url "/lists/3/" shimbun-geocrawler-url))
	(categories)
	(groups)
	(case-fold-search t))
  (with-temp-buffer
    (shimbun-retrieve-url url)
    (while (re-search-forward
	    "<a href=\"\\([^\"]+\\)\"><img src=\"/img/cfolder.png" nil t)
      (push (cons (match-string 1) nil) categories))
    (erase-buffer)
    (shimbun-retrieve-url
     (shimbun-expand-url
      (car (assoc (completing-read "Category: " categories nil t) categories))
      url))
    (while (re-search-forward
	    "<a href=\"\\([0-9]+\\)/0/\"><img src=\"/img/cfolder.png\"[^>]*> &nbsp;"
	    nil t)
      (let* ((id (match-string 1))
	     (group (buffer-substring
		     (point)
		     (progn
		       (search-forward "</a>" nil t)
		       (match-beginning 0)))))
	(push (cons group id) groups)))
    (let ((ginfo (assoc (completing-read "Group: " groups nil t) groups)))
      (if (assoc (car ginfo) shimbun-geocrawler-group-alist)
	  (message "%s has already been registerd." (car ginfo))
	(customize-save-variable
	 'shimbun-geocrawler-group-alist
	 (sort (cons (list (car ginfo) (cdr ginfo) nil nil)
		     shimbun-geocrawler-group-alist)
	       (lambda (a b)
		 (string< (downcase (car a))
			  (downcase (car b)))))))))))

(provide 'sb-geocrawler)

;;; sb-geocrawler.el ends here
