;;; sb-wincefan.el --- shimbun backend for WindowsCE FAN -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2003, 2004, 2005 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
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

(require 'shimbun)

(luna-define-class shimbun-wincefan (shimbun) ())

(defconst shimbun-wincefan-url "http://www.wince.ne.jp/")
(defvar shimbun-wincefan-groups '("news"))
(defconst shimbun-wincefan-from-address "webmaster@wince.ne.jp")
(defvar shimbun-wincefan-coding-system 'japanese-shift-jis)
(defvar shimbun-wincefan-content-start "\\(<!-- *ソフト詳細情報 *-->\\|<B>■トピックスの内容<\/B>\\)")
(defvar shimbun-wincefan-content-end "<!-- \\/*\\(記事\\|ソフト詳細情報\\)-->")
(defvar shimbun-wincefan-expiration-days 14)

(luna-define-method shimbun-get-headers ((shimbun shimbun-wincefan)
					 &optional outbuf)
  (let* ((from (shimbun-from-address shimbun))
	 (group (shimbun-current-group-internal shimbun))
	 (baseurl (when (string-match "^http://\\([^/]+\\)/*$"
				      shimbun-wincefan-url)
		    (match-string 1 shimbun-wincefan-url)))
	 (case-fold-search t)
	 url headers)
    (catch 'stop
      (if (not (re-search-forward "^<!--What's New-->" nil t nil))
	  (throw 'stop nil)
	(delete-region (point-min) (point)))
      (if (not (re-search-forward "^<!--//Whats' New-->" nil t nil))
	  (throw 'stop nil)
	(delete-region (point-max) (point)))
      (goto-char (point-min))
      (while (re-search-forward
	      ;;<FONT SIZE="-1" COLOR="#008000">1月30日(日)</FONT>
	      "<FONT [^<>]+>\\([0-9]+\\)月\\([0-9]+\\)日(\\(月\\|火\\|水\\|木\\|金\\|土\\|日\\))<\/FONT>"
	      nil t nil)
	(let* ((month (string-to-number (match-string-no-properties 1)))
	       (day (string-to-number (match-string-no-properties 2)))
	       (year (string-to-number (substring (current-time-string) 20)))
	       (date (shimbun-make-date-string year month day))
	       (rawdate	(string-to-number
			 (format "%04d%02d%02d"year month day)))
	       (end (save-excursion
		      (or (re-search-forward
			   "<FONT [^<>]+>\\([0-9]+\\)月\\([0-9]+\\)日(\\(月\\|火\\|水\\|木\\|金\\|土\\|日\\))<\/FONT>"
			   nil t nil)
			  (point-max))))
	       (count -1)
	       subject id)
	  (while (re-search-forward
		  ;;<TD><A HREF="/frame.asp?/soft2002/detail.asp?PID=3141" TARGET="_top"><FONT SIZE="-1" >くらどぅ vA20(ラジェンダ用 ToDo)</FONT></A></TD>
		  ;;<TD><A HREF="/NewsLink/" TARGET="_top"><FONT SIZE="-1" COLOR="#FF0000">【News】</FONT><BR></A><A HREF="http://www.wince.ne.jp/snap/cnBoard.asp?PID=1376" TARGET="_top"><FONT SIZE="-1" >シャープが HDD 非搭載の廉価版 SL ザウルス「SL-C1000」を発売</FONT><BR></A><FONT SIZE="-1" ></FONT></TD>
		  "<A HREF=\"\\([^\"]+\\)\" TARGET=\"_top\"><FONT SIZE=[^<>]+>\\([^<>]+\\)<\\/FONT>"
		  end t nil)
	    (setq url (match-string-no-properties 1)
		  subject (match-string-no-properties 2))
	    ;; ("/NewsLink/" "【News】")
	    (when (not (and (string-match "^/.*/$" url)
			    (string-match "^【.*】$" subject)))
	      (setq count (1+ count))
	      ;; building ID
	      (setq id (format "<%02d%08d%%%s@%s>" count rawdate group baseurl))
	      (if (shimbun-search-id shimbun id)
		  (throw 'stop nil))
	      ;; not to use frame
	      (when (string-match "/frame\\.asp\\?" url)
		(setq url (concat (substring url 0 (match-beginning 0))
				  (substring url (match-end 0)))))
	      (setq url (shimbun-expand-url url shimbun-wincefan-url))
	      (push (shimbun-make-header
		     0 (shimbun-mime-encode-string subject)
		     from date id "" 0 0 url)
		    headers))))))
    headers))
    ;;(nreverse headers)))

(luna-define-method shimbun-make-contents :before
  ((shimbun shimbun-wincefan) header)
  (let ((case-fold-search t))
    (save-excursion
      (while (re-search-forward "<IMG SRC=\\(\"?/[^ ]+\\)" nil t nil)
	(let ((url (shimbun-expand-url (match-string-no-properties 1)
				       shimbun-wincefan-url)))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert "<IMG SRC=" url))))))

(provide 'sb-wincefan)

;;; sb-wincefan.el ends here
