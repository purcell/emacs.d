;;; sb-2ch.el --- shimbun backend for 2ch.net -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005 by (not 1)

;; Author: (not 1)
;; Keywords: 2ch

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; April 2002
;; Modified by Yuuichi Teranishi <teranisi@gohome.org> so that it can read
;; current read.cgi outputs.

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'static))

(require 'shimbun)

(eval-and-compile
  (luna-define-class shimbun-2ch (shimbun) (content-hash))
  (luna-define-internal-accessors 'shimbun-2ch))

(defcustom shimbun-2ch-group-alist nil
  "*An alist of groups and their URLs."
  :group 'shimbun
  :type '(repeat
	  (cons :format "%v" :indent 4
		(string :tag "Name")
		(string :tag "      URL"))))

(defvar shimbun-2ch-coding-system 'shift_jis)
(defvar shimbun-2ch-content-hash-length 31)

(luna-define-method initialize-instance :after ((shimbun shimbun-2ch)
						&rest init-args)
  (shimbun-2ch-set-content-hash-internal
   shimbun
   (make-vector shimbun-2ch-content-hash-length 0))
  shimbun)

(luna-define-method shimbun-groups ((shimbun shimbun-2ch))
  (mapcar 'car shimbun-2ch-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-2ch))
  (cdr (assoc (shimbun-current-group-internal shimbun)
	      shimbun-2ch-group-alist)))

(defun shimbun-2ch-parse-page (shimbun last ita sure headers
				       &optional no-break)
  "Parse 2ch page.
If LAST is non-nil, add article No.1 to the header structure.
ITA is the name string of the ita.
SURE is the id string of the thread.
HEADERS is the header structure list to be appended.
If optional NO-BREAK is non-nil, don't stop even when header found."
  (let ((case-fold-search t)
	(url (shimbun-index-url shimbun))
	num uname uaddr uid subject date id
	references body st point from)
    (goto-char (point-max))
    (while (re-search-backward "<dt>\\([0-9]+\\) ：" nil t)
      (goto-char (match-end 0))
      (setq point (match-beginning 0)
	    num (string-to-number (match-string 1)))
      (cond
       ((looking-at "<a href=\"mailto:\\([^\"]+\\)\"><b>\\([^<]+\\)<")
	(setq uname (match-string 2)
	      uaddr (match-string 1)))
       ((looking-at "<font color=green><b>\\([^<]+\\)<")
	(setq uname (match-string 1)))
       ((looking-at "<b>\\([^<]+\\)<")
	(setq uaddr (match-string 1))))
      (when (re-search-forward "\
：\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)\\(\([^\)]\)\\)? \\([0-9]+:[0-9]+\\)"
			       nil t)
	(setq date (shimbun-make-date-string (string-to-number
					      (match-string 1))
					     (string-to-number
					      (match-string 2))
					     (string-to-number
					      (match-string 3))
					     (match-string 5))))
      (when (looking-at " \\(ID:[^ <]+\\)")
	(setq uid (match-string 1)))
      (setq from (mapconcat
		  'identity
		  (delq nil (list uname (if uid (concat "(" uid ")"))
				  (concat "<" uaddr ">")))
		  " "))
      (setq id (format "<%s.%s@%s.2ch.net>" num sure ita))
      (if (and (not no-break)
	       (/= num 1)
	       (shimbun-search-id shimbun id))
	  (throw 'stop headers))
      (search-forward "<dd>" nil t)
      (setq st (match-end 0))
      (when (re-search-forward "\\(<dt>\\|</dl>\\)" nil t)
	(setq body (buffer-substring st (match-beginning 0)))
	(set (intern id (shimbun-2ch-content-hash-internal shimbun))
	     body)
	(setq references nil)
	(setq subject
	      (with-temp-buffer
		(insert body)
		(goto-char (point-min))
		(when (or (re-search-forward "<a href=\\([0-9]+\\)" nil t)
			  (re-search-forward ">>\\([0-9]+\\)[^0-9]" nil t)
			  (re-search-forward "[^a-z]>\\([0-9]+\\)[^0-9]" nil t)
			  (re-search-forward "＞\\([0-9]+\\)[^0-9]" nil t)
			  (re-search-forward "&gt;\\([0-9]+\\)[^0-9]" nil t))
		  (setq references
			(format "<%s.%s@%s.2ch.net>"
				(match-string 1) sure ita))
		  (if (string= references id)
		      (setq references nil)))
		(goto-char (point-min))
		(while (re-search-forward "</?[A-Za-z_][^>]*>" nil t)
		  (delete-region (match-beginning 0) (match-end 0)))
		(shimbun-mime-encode-string
		 (static-if (fboundp 'truncate-string-to-width)
		     (truncate-string-to-width (buffer-string)
					       (- 80 (length "subject: ")))
		   (truncate-string (buffer-string)
				    (- 80 (length "subject: ")))))))
	(when (if (eq num 1) (if last t) t)
	  (push (shimbun-make-header
		 num
		 subject
		 (shimbun-mime-encode-string from)
		 date id references 0 0 (file-name-as-directory url))
		headers)))
      (goto-char point))
    headers))

(defun shimbun-2ch-request-article (shimbun header)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let* ((url (shimbun-index-url shimbun))
	   (number (shimbun-header-number header))
	   (index (format "%d-%d" (max 1 (- number 49))
			  (+ number 50))))
      (unless (shimbun-retrieve-url (concat
				     url
				     "/" index "/")
				    'reload 'binary)
	(error "Retrieve failed: %s" url))
      (set-buffer-multibyte t)
      (decode-coding-region (point-min) (point-max)
			    (shimbun-coding-system-internal shimbun))
      (unless (string-match ".*/read.cgi/\\([^/]+\\)/\\([0-9]+\\)" url)
	(error "\
Unfortunately, the url name format might have been changed in 2ch"))
      (shimbun-2ch-parse-page shimbun
			      (save-match-data
				(string-match "^1-" index))
			      (match-string 1 url);; ita
			      (match-string 2 url);; sure
			      nil 'no-break))))

(luna-define-method shimbun-headers ((shimbun shimbun-2ch) &optional range)
  (cond
   ((eq range 'all) (setq range nil))
   ((eq range 'last) (setq range 1)))
  (let ((first t)
	(count 0)
	(url (shimbun-index-url shimbun))
	headers
	(indices (list "l50"))
	ita sure)
    (catch 'stop
      (while indices
	(message "Reading %s/%s/..." url (car indices))
	(with-temp-buffer
	  (set-buffer-multibyte nil)
	  (unless (shimbun-retrieve-url (concat
					 url
					 "/" (car indices) "/")
					'reload 'binary)
	    (error "Retrieve failed: %s"
		   (concat url "/" (car indices) "/")))
	  (set-buffer-multibyte t)
	  (decode-coding-region (point-min) (point-max)
				(shimbun-coding-system-internal shimbun))
	  (when first
	    (unless (string-match ".*/read.cgi/\\([^/]+\\)/\\([0-9]+\\)" url)
	      (error "\
Unfortunately, the url name format might have been changed in 2ch"))
	    (setq ita (match-string 1 url)
		  sure (match-string 2 url)))
	  (setq headers (shimbun-2ch-parse-page shimbun
						(string-match "^1-"
							      (car indices))
						ita sure
						headers))
	  (when (and headers first)
	    (setq first nil)
	    (let ((cur (shimbun-header-number (car headers)))
		  last)
	      (while (not (eq cur 1))
		(setq last cur)
		(setq cur (max 1 (- cur 100)))
		(setq indices
		      (cons (format "%d-%d" cur (max (- last 1) 1))
			    indices))))
	    (setq indices (nreverse indices))))
	(when (and range
		   (eq (incf count) range))
	  (setq indices nil))
	(setq indices (cdr indices)))
      headers)))

(luna-define-method shimbun-article ((shimbun shimbun-2ch) header
				     &optional outbuf)
  (when (shimbun-current-group-internal shimbun)
    (with-current-buffer (or outbuf (current-buffer))
      (let ((sym (intern-soft (shimbun-header-id header)
			      (shimbun-2ch-content-hash-internal
			       shimbun))))
	(unless sym
	  (shimbun-2ch-request-article shimbun header)
	  (setq sym (intern-soft (shimbun-header-id header)
				 (shimbun-2ch-content-hash-internal
				  shimbun))))
	(shimbun-header-insert shimbun header)
	(insert "Content-Type: " "text/html"
		"; charset=SHIFT_JIS\n"
		"Content-Transfer-Encoding: 8bit\n"
		"MIME-Version: 1.0\n\n")
	(w3m-insert-string (encode-coding-string (symbol-value sym)
						 (mime-charset-to-coding-system
						  "SHIFT_JIS")))))))

(provide 'sb-2ch)

;;; sb-2ch.el ends here
