;;; sb-yahoo-sports.el --- shimbun backend for sports.yahoo.co.jp -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2005
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

;; Original code was sb-yahoo.el which is written by
;; Kazuyoshi KOREEDA <Kazuyoshi.Koreeda@rdmg.mgcs.mei.co.jp>

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-yahoo-sports (shimbun) ())

(defvar shimbun-yahoo-sports-url "http://sports.yahoo.co.jp/")

(defvar shimbun-yahoo-sports-groups-alist
  '(("baseball" "npb" "http://sports.yahoo.co.jp")
    ("keiba" "horse" "http://keiba.yahoo.co.jp")
    ("F1" "f1" "http://sports.yahoo.co.jp")
    ("NBA" "nba" "http://sports.yahoo.co.jp")
    ("NFL" "nfl" "http://sports.yahoo.co.jp")
    ("rugby" "rugby" "http://sports.yahoo.co.jp")))

(defvar shimbun-yahoo-sports-groups
  (mapcar 'car shimbun-yahoo-sports-groups-alist))

(defvar shimbun-yahoo-sports-from-address "webmaster@sports.yahoo.co.jp")
(defvar shimbun-yahoo-sports-content-start
  "<!-+ ?PHOTO\\( MODULE \\)?-+>\\(.\\|\n\\)+<!-+ ?/?PHOTO\\( MODULE \\)?-+>\n+")
(defvar shimbun-yahoo-sports-content-end
  "\n+\\(</font>\\(</td></tr></table>\\|\n+<p>\\)\\|</body>\\)")

(defvar shimbun-yahoo-sports-x-face-alist
  '(("default" . "X-Face: \"Qj}=TahP*`:b#4o_o63:I=\"~wbql=kpF1a>Sp62\
fpAsVY`saZV[b*GqI!u|i|xKPjNh&P=\n R?n}rh38mkp_:')h=Bh:Rk>0pYF\\I?f\\\
PvPs3>/KG:03n47U?FC[?DNAR4QAQxE3L;m!L10OM$-]kF\n YD\\]-^qzd#'{(o2cu,\
(}CMi|3b9JDQ(^D\\:@DE}d2+0S2G{VS@E*1Og7Vj#35[77\"z9XBq9$1uF$+W\n u")))
(defvar shimbun-yahoo-sports-expiration-days 7)

(defun shimbun-yahoo-sports-get-base-url (group)
  (nth 2 (assoc group shimbun-yahoo-sports-groups-alist)))

(luna-define-method shimbun-index-url ((shimbun shimbun-yahoo-sports))
  (let ((group (shimbun-current-group-internal shimbun)))
    (concat (nth 2 (assoc group shimbun-yahoo-sports-groups-alist))
	    "/hl?c="
	    (nth 1 (assoc group shimbun-yahoo-sports-groups-alist))
	    "&p=0")))

(luna-define-method shimbun-get-headers ((shimbun shimbun-yahoo-sports)
					 &optional range)
  (let ((case-fold-search t)
	headers)
    (catch 'stop
      (while t
	(while (re-search-forward "<a href=\\(/hl\\?c=[^&]+&a=\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)-\\([0-9]+\\)-[^>]+\\)>\\([^<]+\\)</a>.+\\([0-9][0-9]\\):\\([0-9][0-9]\\)" nil t)
	  (let ((url (match-string 1))
		(year (match-string 2))
		(month (match-string 3))
		(day (match-string 4))
		(no (match-string 5))
		(subject (match-string 6))
		(hour (string-to-number (match-string 7)))
		(min (string-to-number (match-string 8)))
		id time)
	    (setq id (format "<%s%s%s%s.%s@sports.yahoo.co.jp>"
			     year month day no
			     (shimbun-current-group-internal shimbun)))
	    (if (shimbun-search-id shimbun id)
		(throw 'stop nil))
	    (setq time (format "%02d:%02d" hour min))
	    (push (shimbun-make-header
		   0
		   subject
		   (shimbun-from-address shimbun)
		   (shimbun-make-date-string (string-to-number year)
					     (string-to-number month)
					     (string-to-number day) time)
		   id "" 0 0 
		   (concat (shimbun-yahoo-sports-get-base-url
			    (shimbun-current-group-internal shimbun))
			   url))
		  headers)))
	(if (re-search-forward
	     "<a href=\\(/hl\\?c=[^&]+&[^>]+\\)>次のページ</a>" nil t)
	    (progn
	      (shimbun-retrieve-url
	       (prog1
		   (concat (shimbun-yahoo-sports-get-base-url
			    (shimbun-current-group-internal shimbun))
			   (match-string 1))
		 (erase-buffer))
	       t)
	      (goto-char (point-min)))
	  (throw 'stop nil))))
    headers))

(provide 'sb-yahoo-sports)

;;; sb-yahoo-sports.el ends here
