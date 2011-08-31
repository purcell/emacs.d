;;; sb-makanai.el --- shimbun backend for www.makanai.com -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006
;; MIYOSHI Masanori <miyoshi@meadowy.org>

;; Author: MIYOSHI Masanori <miyoshi@meadowy.org>
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

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>.

;;; Code:

(require 'shimbun)
(require 'sb-text) ;; For `shimbun-shallow-rendering'.

(luna-define-class shimbun-makanai (shimbun) ())

(defvar shimbun-makanai-url "http://www.makanai.com/")
(defvar shimbun-makanai-server-name "makanai")
(defvar shimbun-makanai-groups '("f1news"))
(defvar shimbun-makanai-group-alist
  '(("f1news" . "http://www1.wisnet.ne.jp/~matunaga/news/")))
(defvar shimbun-makanai-from-address  "matunaga@mail1.wisnet.ne.jp")
(defvar shimbun-makanai-content-start "</font></h3><blockquote>")
(defvar shimbun-makanai-content-end "</blockquote><br><br>")

(defsubst shimbun-makanai-base-url (shimbun)
  (cdr (assoc (shimbun-current-group-internal shimbun)
	      shimbun-makanai-group-alist)))

(luna-define-method shimbun-index-url ((shimbun shimbun-makanai))
  (concat (shimbun-makanai-base-url shimbun)
	  "000.html"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-makanai)
					 &optional range)
  (let ((case-fold-search t) headers)
    (when (re-search-forward "<title>F1gpnews</title>" nil t)
      (while (re-search-forward "<a href=\"\\(\\([0-9]+\\)\.html\\)\">\\([^<]+\\)</a><br>" nil t)
	(let ((url (match-string 1))
	      (id (match-string 2))
	      (subject (match-string 3)))
	  (save-excursion
	    (when (re-search-backward "<div>\\([0-9]+\\)年\\([0-9]+\\)月\\([0-9]+\\)日(.*)</div><blockquote>" nil t)
	      (let ((year (match-string 1))
		    (month (match-string 2))
		    (day (match-string 3))
		    date)
		(setq date (shimbun-make-date-string
			    (string-to-number year)
			    (string-to-number month)
			    (string-to-number day)))
		(setq id (format "<%s.%s.%s.%s.%s@www.makanai.com>"
				 year month day id
				 (shimbun-current-group-internal shimbun)))
		(push (shimbun-create-header
		       0
		       subject
		       (shimbun-from-address shimbun)
		       date id "" 0 0
		       (concat (shimbun-makanai-base-url shimbun) url))
		      headers)))))))
    headers))

(provide 'sb-makanai)

;;; sb-makanai.el ends here
