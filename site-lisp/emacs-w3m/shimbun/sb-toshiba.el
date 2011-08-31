;;; sb-toshiba.el --- shimbun backend for TOSHIBA Linux users ML

;; Copyright (C) 2001, 2005 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

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

;; This is shimbun backend for TOSHIBA Linux users ML.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'shimbun)
(require 'sb-fml)

(luna-define-class shimbun-toshiba (shimbun-fml) ())

(defvar shimbun-toshiba-url "http://linux.toshiba-dme.co.jp/ML/tlinux-users-j/")
(defvar shimbun-toshiba-groups '("linux-users-j"))
(defvar shimbun-toshiba-from-address "tlinux-users-j@linux.toshiba-dme.co.jp")

(luna-define-method shimbun-get-headers ((shimbun shimbun-toshiba)
					 &optional range)
  (let ((case-fold-search t)
	(pages (shimbun-header-index-pages range))
	(count 0)
	headers auxs aux)
    (goto-char (point-min))
    (while (and (if pages (<= (incf count) pages) t)
		(re-search-forward "<a href=\"\\([0-9]+\\(\\.week\\|\\.month\\)?\\)/index.html\">" nil t))
      (setq auxs (append auxs (list (match-string 1)))))
    (catch 'stop
      (while auxs
	(with-temp-buffer
	  (shimbun-retrieve-url
	   (concat (shimbun-index-url shimbun) (setq aux (car auxs)) "/")
	   'reload)
	  (let ((case-fold-search t)
		id url date subject)
	    (goto-char (point-min))
	    (while (re-search-forward
		    "<LI><A HREF=\"\\([0-9]+\\.html\\)\">Article .*</A>\
 <DIV><SPAN CLASS=article>Article <SPAN CLASS=article-value>\\([0-9]+\\)\
</SPAN></SPAN> at <SPAN CLASS=Date-value>\\([^<]*\\)</SPAN>\
 <SPAN CLASS=Subject>Subject: <SPAN CLASS=Subject-value>\\([^<]*\\)\
</SPAN></SPAN></DIV>"
		    nil t)
	      (setq url (concat (shimbun-index-url shimbun)
				aux "/" (match-string 1))
		    id (format "<%s%05d%%%s>"
			       aux
			       (string-to-number (match-string 2))
			       (shimbun-current-group-internal shimbun))
		    date (shimbun-fml-parse-time (match-string 3))
		    subject (match-string 4))
	      (if (shimbun-search-id shimbun id)
		  (throw 'stop nil))
	      (forward-line 1)
	      (push (shimbun-create-header 0 subject (shimbun-from-address shimbun) date id "" 0 0 url)
		    headers)))
	  (setq auxs (cdr auxs)))))
    headers))

(provide 'sb-toshiba)

;;; sb-toshiba.el ends here
