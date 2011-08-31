;;; sb-ibm-dev.el --- shimbun backend for www-6.ibm.com/ja/developerworks -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2003, 2005, 2007 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
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

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'shimbun)

(luna-define-class shimbun-ibm-dev (shimbun) ())

(defvar shimbun-ibm-dev-url "http://www.ibm.com/jp/developerworks/")
(defvar shimbun-ibm-dev-groups
  '("autonomic" "java" "linux" "opensource" "webservices" "xml"))
(defvar shimbun-ibm-dev-coding-system 'japanese-shift-jis-unix)
(defvar shimbun-ibm-dev-content-start "<!--Contents-->")
(defvar shimbun-ibm-dev-content-end "<!--// Contents-->")

(luna-define-method shimbun-index-url ((shimbun shimbun-ibm-dev))
  (shimbun-expand-url (concat (shimbun-current-group shimbun) "/library.html")
		      (shimbun-url-internal shimbun)))

(luna-define-method shimbun-from-address ((shimbun shimbun-ibm-dev))
  (concat "IBM developerWorks (" (shimbun-current-group shimbun) ")"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-ibm-dev)
					 &optional range)
  (catch 'stop
    (let ((case-fold-search t)
	  (base (shimbun-index-url shimbun))
	  (indexes)
	  (headers))
      (let ((pages (shimbun-header-index-pages range)))
	(goto-char (point-min))
	(while (when (or (not pages)
			 (< (length indexes) pages))
		 (re-search-forward "<a +class=\"[^\"]+\" +\
href=\"\\(library[0-9]*\\.s?html\\)\">[0-9]+年</a>" nil t))
	  (push (shimbun-expand-url (match-string 1) base) indexes)))
      (let ((pattern (format "/jp/developerworks/%s/"
			     (regexp-quote (shimbun-current-group shimbun)))))
	(dolist (index (nreverse indexes))
	  (unless (string= index base)
	    (erase-buffer)
	    (shimbun-fetch-url shimbun index))
	  (goto-char (point-min))
	  (while (re-search-forward
		  "<a href=\"\\([^\"]+\\)\"><b>\\([^<>]+\\)</b></a>" nil t)
	    (let ((url (shimbun-expand-url (match-string 1) index))
		  (subject (match-string 2)))
	      (when (string-match pattern url)
		(let ((id (concat "<" (md5 url)
				  "%" (shimbun-current-group shimbun)
				  "@" (shimbun-server shimbun)
				  ".shimbun.namazu.org>")))
		  (when (shimbun-search-id shimbun id)
		    (throw 'stop headers))
		  (push (shimbun-create-header nil subject
					       (shimbun-from-address shimbun)
					       nil id "" 0 0 url)
			headers)))))))
      headers)))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-ibm-dev)
						    header)
  (goto-char (point-min))
  (when (re-search-forward "<meta name=\"DC.Date\" scheme=\"iso8601\" \
content=\"\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)\" />" nil t)
    (shimbun-header-set-date header
			     (shimbun-make-date-string
			      (string-to-number (match-string 1))
			      (string-to-number (match-string 2))
			      (string-to-number (match-string 3)))))
  (when (re-search-forward "<a href=\"#author[0-9]+\">\\([^<>]+\\)</a>" nil t)
    (let ((name (match-string 1)))
      (shimbun-header-set-from header
			       (if (looking-at
				    "[^\n]*<a href=\"mailto:\\([^\"?]+\\)[\"?]>")
				   (concat name " <" (match-string 1) ">")
				 name))))
  (when (luna-call-next-method)
    (shimbun-remove-tags "<!-- LEFTNAV_BEGIN -->" "<!-- LEFTNAV_END -->")
    (shimbun-remove-tags "<script" "</script>")
    t))

(provide 'sb-ibm-dev)

;;; sb-ibm-dev.el ends here
