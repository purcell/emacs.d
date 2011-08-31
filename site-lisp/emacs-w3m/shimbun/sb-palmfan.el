;;; sb-palmfan.el --- shimbun backend class for palmfan web site. -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2002, 2003, 2005 NAKAJIMA Mikio <minakaji@namazu.org>

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

;;; Code:
(require 'shimbun)

(eval-and-compile
  (luna-define-class shimbun-palmfan (shimbun) (content-hash))
  (luna-define-internal-accessors 'shimbun-palmfan))

(defvar shimbun-palmfan-content-hash-length 31)
(defvar shimbun-palmfan-url "http://www.palmfan.com")
(defvar shimbun-palmfan-coding-system 'japanese-shift-jis-mac)
(defconst shimbun-palmfan-group-path-alist
  '(("news" . "")
    ;; Revival day?
    ;; ("palmwarefan" . "PWF/")
    ;; not yet
    ;;("nm502i" . "cgi/tnote.cgi?book=book2")
    ;;("hotsync" . "cgi/tnote.cgi?book=book3")
    ))

(defvar shimbun-palmfan-groups
  (mapcar 'car shimbun-palmfan-group-path-alist))

(defconst shimbun-palmfan-palmwarefan-date-regexp
  "<!-- \\([0-9][0-9][0-9][0-9]\\)/\\([0-9][0-9]*\\)/\\([0-9][0-9]*\\) -->$")

(defconst shimbun-palmfan-month-alist
  '(("January" . 1) ("February" . 2) ("March" . 3) ("April" . 4)
    ("May" . 5) ("June" . 6) ("July" . 7) ("August" . 8)
    ("September" . 9) ("October" . 10) ("November" . 11) ("December" . 12)
    ("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4)
    ("May" . 5) ("Jun" . 6) ("Jul" . 7) ("Aug" . 8)
    ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))

(defvar shimbun-palmfan-expiration-days 6)

(defvar shimbun-palmfan-x-face-alist
  '(("default" . "X-Face: \"<kvsju9lZL34FJ5jQUOZ|uEZf2(W2aw>dU62umIQL4j!$\
eGluPC0(*l4^GB8v\n diqCIs\\6@p\\TN#{@;s*NMI'@\\[8Z8M*:5g}\
`c9yC}F6e\\}DqeZo!LB>(hEF|P+U.b|#\n >$@]5@PdGIwuU4=`imfei\
i$PdWyuHC8!1=KH'r,R=fV])N6uQS")))

(luna-define-method initialize-instance :after ((shimbun shimbun-palmfan)
						&rest init-args)
  (shimbun-palmfan-set-content-hash-internal
   shimbun
   (make-vector shimbun-palmfan-content-hash-length 0))
  shimbun)

(luna-define-method shimbun-reply-to ((shimbun shimbun-palmfan))
  (let ((group (shimbun-current-group-internal shimbun)))
    (cond ((string= group "palmwarefan")
	   "brian@palmfan.com")
	  (t
	   "hirose@palmfan.com"))))

(luna-define-method shimbun-index-url ((shimbun shimbun-palmfan))
  (concat (shimbun-url-internal shimbun)
	  "/"
	  (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-palmfan-group-path-alist))))

(luna-define-method shimbun-headers ((shimbun shimbun-palmfan)
				     &optional range)
  (let ((group (shimbun-current-group-internal shimbun)))
    (cond ((string= group "news")
	   (shimbun-palmfan-news-headers shimbun range))
	  ((string= group "palmwarefan")
	   (shimbun-palmfan-palmwarefan-headers shimbun range))
	  (t
	   (shimbun-palmfan-bbs-headers shimbun range)))))

(defun shimbun-palmfan-palmwarefan-headers (shimbun &optional range)
  (let* ((case-fold-search t)
	 (url (shimbun-index-url shimbun))
	 (idbase (concat "palmwarefan."
			 (if (string-match "^http://\\([^/]+\\)/" url)
			     (match-string 1 url)
			   url)))
	 headers)
    (with-temp-buffer
      (shimbun-retrieve-url url 'no-cache 'no-decode)
      (decode-coding-region
       (point-min) (point-max)
       (shimbun-coding-system-internal shimbun))
      (set-buffer-multibyte t)
      (subst-char-in-region (point-min) (point-max) ?\t ?  t)
      (goto-char (point-min))
      (when (and (re-search-forward
		  "^<!--Palmware Release Information-->$" nil t nil)
		 (re-search-forward
		  shimbun-palmfan-palmwarefan-date-regexp nil t nil))
	(beginning-of-line 1)
	(delete-region (point-min) (point)))
      (when (re-search-forward "^<!--Palmware Release Infomation 終了--><BR>$"
			       nil t nil)
	(beginning-of-line 1)
	(delete-region (point) (point-max)))
      (goto-char (point-max))
      (catch 'stop
	(let ((count 0)
	      lastdate)
	  (while (search-backward "</TABLE>" nil t nil)
	    (let ((start (point))
		  end year month day date)
	      (re-search-backward shimbun-palmfan-palmwarefan-date-regexp)
	      (setq year (string-to-number (match-string 1))
		    month (string-to-number (match-string 2))
		    day (string-to-number (match-string 3))
		    date (shimbun-make-date-string year month day)
		    end (progn (search-forward "<TABLE" start)
			       (beginning-of-line)
			       (point)))
	      (if (and lastdate (string= lastdate date))
		  (setq count (1+ count))
		(setq count 0
		      lastdate date))
	      (goto-char start)
	      (re-search-backward
	       ;;<TD colspan="2"><S><B>SilverScreen 2.7</B></S><IMG src="img/i/jloc.gif" alt="日本語ローカライザあり" width="31" height="12"><IMG src="img/i/65k.gif" alt="65K色カラー対応" width="31" height="12"><IMG src="img/i/clie_jog.gif" alt="CLIE ジョグダイアル対応" width="31" height="12"><IMG src="img/i/clie_hires.gif" alt="CLIE ハイレゾ対応" width="31" height="12"><IMG src="img/i/clie_nrhires.gif" alt="CLIE NR ハイレゾ対応" width="31" height="12"><IMG src="img/i/i_vfs.gif" alt="VFS対応" width="31" height="12"></TD>
	       ;;<TD colspan="2"><A href="http://hotspace.jp/%7Ehirock/"><B>PtFtp 0.1.0</B></A><IMG src="img/i/jmenu.gif" alt="日本語メニュー" width="31" height="12"><IMG src="img/i/256.gif" alt="256色カラー対応" width="31" height="12"></TD>
	       "<TD colspan=[^>]+>\\(<A href=\"\\(http://[^>]+\\)\">\\)*\\(<S>\\)*<B>\\([^<]+\\)</B>\\(</S>\\)*\\(</A>\\)*\\(<IMG src=\"\\(.+\\)\">\\)*"
	       end)
	      (let (subject addition id body)
		(setq id (format "<%02d%04d%02d%02d@%s>" count year month day idbase))
		(when (shimbun-search-id shimbun id)
		  (throw 'stop nil))
		(setq subject (match-string 4)
		      addition (match-string 7)
		      body (buffer-substring-no-properties start end))
		;; move file size to SUBJECT
		;;<TD align="center" width="45">8KB</TD>
		(when (string-match "<TD \\( *nowrap *\\)*align=\"[^>]+>\\([0-9]+KB*\\)</TD>" body)
		  (setq subject (concat subject "/" (match-string 2 body)) ; move to subject
			body (concat (substring body 0 (match-beginning 0))
				     (substring body (match-end 0)))))
		;; move price to SUBJECT
		;;<TD align="center" width="50">Freeware</TD>
		(when (string-match "<TD \\( *nowrap *\\)*align=\"[^>]+>\\([^<]+\\)</TD>" body)
		  (setq subject (concat subject "/" (match-string 2 body)) ; move to subject
			body (concat (substring body 0 (match-beginning 0))
				     (substring body (match-end 0)))))
		;; remove duplicated information
		;;<TD colspan="2" align="center">05/16/02</TD>
		(when (string-match
		       "<TD colspan=\"[^>]+>[0-9][0-9]/[0-9][0-9]/[0-9][0-9]</TD>"
		       body)
		  (setq body (concat (substring body 0 (match-beginning 0))
				     "<P>" ; insert return
				     (substring body (match-end 0)))))
                ;; expand relative path
                ;;<TD><IMG src="img/i/etsuko.gif" alt="●" width="32" height="32"></TD>
                ;;(while (string-match "<IMG src=\"\\(img\\)/" body)
                ;;  (setq body (concat (substring body 0 (match-beginning 1))
                ;;                     url "img"
                ;;                     (substring body (match-end 1)))))
                ;; remove table tags -- should be transacted in the last step
		(while (string-match "</*T\\(R\\|D\\)[^>]*>" body)
		  (setq body (concat (substring body 0 (match-beginning 0))
				     (substring body (match-end 0)))))
		(set (intern id (shimbun-palmfan-content-hash-internal shimbun))
		     body)
		(when addition
		  (while (string-match "alt=\"\\([^\"]+\\)\"" addition)
		    (setq subject (concat subject "/" (match-string 1 addition))
			  addition (substring addition (match-end 0)))))
		(push (shimbun-make-header 0
					   (shimbun-mime-encode-string subject)
					   (shimbun-from-address shimbun)
					   date id "" 0 0 url)
		      headers))))))
    (nreverse headers))))

(defun shimbun-palmfan-bbs-headers (shimbun &optional range)
  ;; not yet
  )

(defun shimbun-palmfan-news-headers (shimbun &optional range)
  (let* ((case-fold-search t)
	 (url (shimbun-index-url shimbun))
	 (idbase (if (string-match "^http://\\([^/]+\\)/" url)
		     (match-string 1 url)
		   url))
	 (from "hirose@palmfan.com")
	 (first-article t)
	 headers)
    (with-temp-buffer
      (shimbun-retrieve-url url 'no-cache 'no-decode)
      (decode-coding-region
       (point-min) (point-max)
       (shimbun-coding-system-internal shimbun))
      (set-buffer-multibyte t)
      (subst-char-in-region (point-min) (point-max) ?\t ?  t)
      (goto-char (point-min))
      (when (re-search-forward "^<!--スポンサー・バナーここまで-->$" nil t nil)
	(forward-line 1)
	(beginning-of-line 1)
	(delete-region (point-min) (point)))
      (when (re-search-forward "■過去記事一覧■<BR>$" nil t nil)
	(beginning-of-line 1)
	(delete-region (point) (point-max)))
      (goto-char (point-min))
      (catch 'stop
	(let (end)
	  (while (or first-article
		     (re-search-forward "<!-- *日付 *-->" nil t nil))
	    (let ((start (point))
		  (count -1)
		  month day year date)
	      (if first-article
		  (setq date (shimbun-palmfan-get-first-article-date)
			start (point)
			first-article nil)
		(setq date (shimbun-palmfan-pickup-date)))
	      (setq year (car date)
		    month (car (cdr date))
		    day (car (cdr (cdr date))))
	      (setq end (if (re-search-forward "<!-- *日付 *-->" nil t nil)
			    (progn
			      (goto-char (match-beginning 0))
			      (forward-char -1)
			      (point))
			  (point-max)))
	      (setq date (format "%02d %s %04d 00:00 +0900" day month year))
	      (goto-char start)
	      (while (or (re-search-forward
			  "^<!-- \\(トピック\\|ソフト\\)タイトル -->$" end t nil)
			 ;; <FONT color="#0000AF">●</FONT><B>ひとりごと</B>
			 ;; <FONT color="#0000AF">●</FONT><B>DCF・Exif・JPEGについて</B>
			 (re-search-forward
			  "^<FONT color=\"#0000AF\">●</FONT><B>\\(.+\\)</B>" end t nil))
		(let (subject id others body)
		  (if (not (member (match-string 1) '("トピック" "ソフト")))
		      (progn
			(setq subject (match-string 1))
			(unless (string= others "ひとりごと")
			  ;;<FONT color="#0000AF">●</FONT><B>DCF・Exif・JPEGについて</B>
			  (setq others t)))
		    (setq subject (buffer-substring-no-properties
				   (progn (forward-char 1) (point))
				   (progn (re-search-forward "<BLOCKQUOTE>" end t nil)
					  (beginning-of-line 1) (point)))))
		  (when (or others
			    (re-search-forward "^<!--\\(本文\\|コメント\\|ひとりごと本文\\)-->$" end t nil))
		    (setq body (buffer-substring-no-properties
				(point) (search-forward "</BLOCKQUOTE>" end))
			  count (1+ count)
			  id (format "<%02d%04d%02d%02d@%s>" count year
				     (cdr (assoc month shimbun-palmfan-month-alist))
				     day idbase))
		    (if (shimbun-search-id shimbun id)
			(throw 'stop nil))
		    (when (string-match "^[\n\t ]*\\(.*\\)[\n\t ]*$" subject)
		      (setq subject (match-string 1 subject)))
		    (let ((case-fold-search t))
		      (when (string-match "<A href=.*</A>" subject)
			(setq body (concat "<P>" subject "</P>" body))))
		    (with-temp-buffer
		      (insert subject)
		      (shimbun-remove-markup)
		      (setq subject (buffer-string)))
		    (set (intern id (shimbun-palmfan-content-hash-internal shimbun))
			 body)
		    (push (shimbun-make-header
			   0 (shimbun-mime-encode-string subject)
			   from date id "" 0 0 url)
			  headers))))))))
      headers)))

(defun shimbun-palmfan-get-first-article-date ()
  (let (first-date first-article date)
    (setq first-date (re-search-forward "<!-- *日付 *-->" nil t nil))
    (goto-char (point-min))
    (setq first-article
	  (re-search-forward "^<!-- \\(トピック\\|ソフト\\)タイトル -->$"
			     nil t nil))
    (goto-char first-date)
    (setq date (shimbun-palmfan-pickup-date))
    (goto-char first-article)
    (beginning-of-line)
    (forward-char -1)
  (if (and first-date first-article
	   (> first-date first-article))
      ;; XXX it cannot understand non-exsistent day...
      (setcar (cdr (cdr date)) (1+ (car (cdr (cdr date))))))
  date))

(defun shimbun-palmfan-pickup-date ()
  (let ((start (point))
	date-end year month day)
    (setq date-end (re-search-forward "^</B>" nil t nil))
    (goto-char start)
    (catch 'stop
      ;;2003年 3月 5日水曜日
      (if (re-search-forward "[0-9][0-9][0-9][0-9]" date-end t)
	  (setq year (string-to-number (match-string 0)))
	(throw 'stop nil))
      (goto-char start)
      (if (or (re-search-forward "\\([0-9][0-9]*\\) *日" date-end t)
	      (re-search-forward " \\([0-9][0-9]?\\)[,.]*" date-end t))
	  (setq day (string-to-number (match-string 1)))
	(throw 'stop nil))
      (goto-char start)
      (if (re-search-forward "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\|[0-9]+ *月\\)" date-end t)
	  (setq month (match-string 1))
	(throw 'stop nil))
      (when (string-match "\\([0-9]+\\) *月" month)
	(setq month (car (rassoc (string-to-number (match-string 1 month))
				 (reverse shimbun-palmfan-month-alist)))))
      (list year month day))))

(luna-define-method shimbun-article
  ((shimbun shimbun-palmfan) header &optional outbuf)
  (let (string)
    (with-current-buffer (or outbuf (current-buffer))
      (with-temp-buffer
	(let ((sym (intern-soft (shimbun-header-id header)
				(shimbun-palmfan-content-hash-internal
				 shimbun))))
	  (when (and (boundp sym) (symbol-value sym))
	    (insert (symbol-value sym))
	    (goto-char (point-min))
	    (insert "<html>\n<head>\n<base href=\""
		    (shimbun-header-xref header) "\">\n</head>\n<body>\n")
	    (goto-char (point-max))
	    (insert "\n</body>\n</html>\n")
	    (encode-coding-string
	     (buffer-string)
	     (mime-charset-to-coding-system "ISO-2022-JP"))
	    (shimbun-make-mime-article shimbun header)
	    (setq string (buffer-string)))))
      (when string
	(w3m-insert-string string)))))

(provide 'sb-palmfan)

;;; sb-palmfan.el ends here
