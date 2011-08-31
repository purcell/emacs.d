;;; sb-slashdot-jp.el --- shimbun backend for slashdot.jp -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2003, 2004, 2005, 2006, 2007
;; NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Keywords: news
;; Created: Jun 14, 2003

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

;;; History:

;; This backend was created by Yuuichi Teranishi <teranisi@gohome.org>
;; at July 5th, 2001.

;; Because the site desgin had been changed, this backend was
;; completly rewritten by TSUCHIYA Masatoshi <tsuchiya@namazu.org> at
;; February 28th, 2002.

;; NAKAJIMA Mikio <minakaji@namazu.org> created a new backend,
;; sb-slashdot-jp-rss.el, at July 15th, 2003.  It was an alternative
;; backend of slashdot.jp based on RSS.

;; In order to reduce the cost to maintain both backends, the backend
;; based on the traditional approach was succeeded by the backend
;; based on RSS at January 18th, 2004.

;;; Code:

(require 'shimbun)
(require 'sb-rss)

(defcustom shimbun-slashdot-jp-comment-arguments
  '((threshold . 1)
    (mode . nested)
    (commentsort . 0))
  "*Arguments to view comment pages."
  :group 'shimbun
  :type '(list
	  (cons :tag "Score threshold" :format "%t: %v"
		(const :tag "" threshold) integer)
	  (cons :tag "Threading mode" :format "%t: %v"
		(const :tag "" mode)
		(choice (const flat)
			(const nested)
			(const nocomment)
			(const thread)))
	  (cons :tag "Sorting order" :format "%t: %v"
		(const :tag "" commentsort)
		(choice (const :tag "Oldest first" 0)
			(const :tag "Newest first" 1)
			(const :tag "Highest scores first" 3)
			(const :tag "Oldest first (Ignore threads)" 4)
			(const :tag "Newest first (Ignore threads)" 5)))))

(defcustom shimbun-slashdot-jp-group-alist
  '(("story"	   . "http://slashdot.jp/index.rss")
    ("askslashdot" . "http://slashdot.jp/askslashdot.rss")
    ("bookreview"  . "http://slashdot.jp/books.rss")
    ("bsd"	   . "http://slashdot.jp/bsd.rss")
    ("developers"  . "http://slashdot.jp/developers.rss")
    ("interview"   . "http://slashdot.jp/interview.rss")
    ("linux"       . "http://slashdot.jp/linux.rss")
    ("mac"	   . "http://slashdot.jp/mac.rss")
    ("mobile"	   . "http://slashdot.jp/mobile.rss")
    ("science"	   . "http://slashdot.jp/science.rss")
    ("security"	   . "http://slashdot.jp/security.rss")
    ("slash"	   . "http://slashdot.jp/slash.rss")
    ("it"          . "http://slashdot.jp/it.rss")
    ("hardware"    . "http://slashdot.jp/hardware.rss")
    ("diary.oliver" .
     "http://slashdot.jp/~Oliver/journal/rss"))
  "*Alist of slashdot groups and their RSS feeds."
  :group 'shimbun
  :type '(repeat
	  (cons :format "%v" :indent 4
		(string :format "Group name: %v\n" :size 0)
		(string :format "        RSS URL: %v\n" :size 0))))

(luna-define-class shimbun-slashdot-jp (shimbun-rss) ())

(defvar shimbun-slashdot-jp-from-address "slashmaster@slashdot.jp")
;;(defvar shimbun-slashdot-jp-coding-system 'euc-japan)
(defvar shimbun-slashdot-jp-content-start
  "<!-- start template: ID [0-9]+, \
\\(dispStory;[^;]+\\|\\(bluebox\\|generic\\|greypage\\|liquid\\|slashdotjp\
\\|yellowpage\\);journal\\);default -->\n")
(defvar shimbun-slashdot-jp-content-end
  "<!-- end template: ID [0-9]+, \
\\(dispStory;[^;]+\\|\\(bluebox\\|generic\\|greypage\\|liquid\\|slashdotjp\
\\|yellowpage\\);journal\\);default -->\n")

(luna-define-method shimbun-groups ((shimbun shimbun-slashdot-jp))
  (mapcar 'car shimbun-slashdot-jp-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-slashdot-jp))
  (cdr (assoc (shimbun-current-group-internal shimbun)
	      shimbun-slashdot-jp-group-alist)))

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-slashdot-jp) url date)
  (cond
   ((string-match
     "\\`http://slashdot\\.jp/\\([a-zA-Z0-9]+\\)?/?article\\.pl\\?sid=\\([/0-9]+\\)\\(&\\|\\'\\)"
     url)
    (if (match-string-no-properties 1 url)
	(concat "<" (match-string-no-properties 1 url)
		"%" (match-string-no-properties 2 url) "@slashdot.jp>")
      (concat "<" (match-string-no-properties 2 url) "@slashdot.jp>")))
   ((or (string-match
	 "\\`http://slashdot\\.jp/journal\\.pl\\?op=display&uid=\\([0-9]+\\)&id=\\([0-9]+\\)"
	 url)
	(string-match
	 "\\`http://slashdot\\.jp/~\\([^/]+\\)/journal/\\([0-9]+\\)\\(\\?from=rss\\)?"
	 url))
    (concat "<" (match-string-no-properties 2 url)
	    "%" (match-string-no-properties 1 url) "@slashdot.jp>"))
   (t (error "Cannot find message-id base"))))

(luna-define-method shimbun-get-headers :around
  ((shimbun shimbun-slashdot-jp) &optional range)
  (let ((headers (luna-call-next-method)))
    (dolist (head headers)
      (let ((xref (shimbun-header-xref head)))
	(if (not (string-match "journal" xref))
	    ;; article
	    (shimbun-header-set-xref head
				     (concat xref "&mode=nocomment"))
	  (if (string-match
	       "\\`http://slashdot\\.jp/~\\([^/]+\\)/journal/\\([0-9]+\\)\\(\\?from=rss\\)?"
	       xref)
	      (shimbun-header-set-xref head
				       (concat
					"http://slashdot.jp/~"
					(match-string 1 xref)
					"/journal/"
					(match-string 2 xref)
					"?theme=generic&mode=nocomment"))
	    (shimbun-header-set-xref head
				     (concat xref
					     "&theme=generic&mode=nocomment"))))))

    headers))

(defun shimbun-slashdot-jp-comment-url (url)
  (when (string-match "\\`http://slashdot\\.jp/\\([a-zA-Z0-9]+\\)?/?article\\.pl" url)
    (mapconcat 'identity
	       (cons (if (string-match "&mode=nocomment\\'" url)
			 (substring url 0 (match-beginning 0))
		       url)
		     (mapcar (lambda (x)
			       (format "%s=%s" (car x) (cdr x)))
			     shimbun-slashdot-jp-comment-arguments))
	       "&")))

(luna-define-method shimbun-make-contents :before
  ((shimbun shimbun-slashdot-jp) header)
  (goto-char (point-min))
  (if (string-match
       "\\`http://slashdot\\.jp/\\([a-zA-Z0-9]+\\)?/?article\\.pl"
       (shimbun-header-xref header))
      ;; article
      (when (re-search-forward "<table[^>]*class=\"titlebar\"[^>]*>[ \t\n]*\
<tr[^>]*>[ \t\n]*<td[^>]*>[ \t\n]*<font[^>]*>\\([^<]+\\)</font>" nil t)
	(shimbun-header-set-subject header (match-string-no-properties 1)))
    ;; journal
    (when (re-search-forward
	   "<a +href=\"?/?[^/]*/journal/[0-9]+\"?>\\([^<]+\\)</a>" nil t)
      (shimbun-header-set-subject header (match-string-no-properties 1))))
  (goto-char (point-min)))

(luna-define-method shimbun-clear-contents :around
  ((shimbun shimbun-slashdot-jp) header)
  (when (luna-call-next-method)
    (shimbun-remove-tags "<!-- begin ad code -->" "<!-- end ad code -->")
    (shimbun-remove-tags "<script" "</script>")
    (shimbun-remove-tags "<noscript" "</noscript>")
    (let ((url (shimbun-slashdot-jp-comment-url (shimbun-header-xref header))))
      (when url
	(goto-char (point-max))
	(insert "\n<p align=left>[<a href=\"" url "\">もっと読む…</a>]</p>")))
    t))

(provide 'sb-slashdot-jp)

;;; sb-slashdot-jp.el ends here
