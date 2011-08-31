;;; sb-gendai-net.el --- shimbun backend for Gendai Net -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2006, 2007 Katsumi Yamaoka

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
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

;; Gendai Net is a service providing information made and edited based
;; on Nikkan Gendai, an evening paper in Japan.

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-gendai-net (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-gendai-net-top-level-domain "gendai.net"
  "Name of the top level domain for Gendai Net.")

(defvar shimbun-gendai-net-url
  (concat "http://" shimbun-gendai-net-top-level-domain "/")
  "Name of the parent url.")

(defvar shimbun-gendai-net-server-name "日刊ゲンダイ")

(defvar shimbun-gendai-net-from-address "nobody@example.com")

(defvar shimbun-gendai-net-content-start
  "<!-- shimbun-gendai-net-content-start -->")

(defvar shimbun-gendai-net-content-end
  "<!-- shimbun-gendai-net-content-end -->")

(defvar shimbun-gendai-net-group-table
  '(("today" "今日のゲンダイ" "")
    ("syakai" "社会")
    ("sports" "スポーツ")
    ("geino" "芸能")
    ("wadai" "話題")
    ("kenko" "健康")
    ("syoku" "食・レジャー")
    ("book" "書籍")))

(defvar shimbun-gendai-net-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAAYBAMAAABO02PvAAAAGFBMVEX+6ctRUVH7qDX416T
 6rUL///8QEBCirL0OZ3JBAAABHUlEQVQoz3XQvY7CMAwA4PCTY2ZoWJHpA6ACe1TInoqzsrYD7po
 OSV7/HK5CXNB5SGR/imVHYBGnSz5XR1HUbWvy1Rz+gfNVoJVC4iagtQlRCIaNz0Xb1q12NEilqLp
 zYjqiwK2+oQUTd7FSY+zXc/JgaOoVaIXdqEI35sRMsuszHLDVtN0yuDEnxk3qHXYviMOUYVmfQA9
 o/RNqHpd+W90BwKiw6DOs4QaGQsyAZ93qBU+Y4Q48riIa8ouTBYMpYZLW8x+tpEs+iSPvcYMLfgZ
 vDvV7YZIzoBXyHeIL5jhfbxerRkdVAQ2A5n/hdUo4NPsYuuqjVWOW+0j0KCAxrPfRe1nANuQXX9W
 ibDU9wRHJ2P+BDQppJSaPbp7qB8ukoUp54knLAAAAAElFTkSuQmCC")))

(defvar shimbun-gendai-net-expiration-days 7)

(luna-define-method shimbun-groups ((shimbun shimbun-gendai-net))
  (mapcar 'car shimbun-gendai-net-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-gendai-net))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-gendai-net-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-gendai-net))
  (let ((group (shimbun-current-group-internal shimbun)))
    (shimbun-expand-url
     (or (nth 2 (assoc group shimbun-gendai-net-group-table))
	 (format "?m=new&g=%s" group))
     (shimbun-url-internal shimbun))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-gendai-net)
					 &optional range)
  (let ((pages (shimbun-header-index-pages range)))
    (if (string-equal (shimbun-current-group-internal shimbun) "today")
	(shimbun-gendai-net-get-headers-today shimbun pages)
      (shimbun-gendai-net-get-headers-default shimbun pages))))

(defun shimbun-gendai-net-get-headers-today (shimbun pages)
  (let ((regexp1
	 (eval-when-compile
	   (let ((s0 "[\t\n\r ]*")
		 (s1 "[\t\n\r ]+"))
	     (concat "<a" s1 "href=\""
		     ;; 1. url
		     "\\(http://gendai\\.net/\\?td="
		     ;; 2. year
		     "\\(20[0-9][0-9]\\)"
		     ;; 3. month
		     "\\([01][0-9]\\)"
		     ;; 4. day
		     "\\([0-3][0-9]\\)\\)"
		     "\"" s0 ">" s0 "[01]?[0-9]月[0-3]?[0-9]日" s0 "</a>"))))
	(regexp2
	 (eval-when-compile
	   (let ((s0 "[\t\n\r ]*")
		 (s1 "[\t\n\r ]+"))
	     (concat "<a" s1 "href=\""
		     ;; 1. url
		     "\\("
		     ;; 2. base
		     "\\(http://gendai\\.net/\\?m=view&c="
		     ;; 3. c
		     "\\([0-9]+\\)"
		     "&no="
		     "\\)"
		     ;; 4. no
		     "\\([0-9]+\\)"
		     "\\)"
		     "\"" s0 ">" s0
		     ;; 5. subject
		     "\\([^<]+\\)"
		     s0 "</a>"))))
	(group (shimbun-current-group-internal shimbun))
	(from (concat "ゲンダイネット (" (shimbun-current-group-name shimbun)
		      ")"))
	year month indices index day date num url base c subject increase id
	headers)
    (while (re-search-forward regexp1 nil t)
      (setq year (string-to-number (match-string 2))
	    month (string-to-number (match-string 3)))
      (if indices
	  (push (list (match-string 1) year month
		      (string-to-number (match-string 4)))
		indices)
	(save-match-data
	  (when (re-search-backward
		 (eval-when-compile
		   (let ((s0 "[\t\n\r ]*")
			 (s1 "[\t\n\r ]+"))
		     (concat "<font" s1 "[^>]+>" s0 "<b>" s0
			     "\\([01]?[0-9]\\)月\\([0-3]?[0-9]\\)日"
			     s0 "</b>" s0 "</font>")))
		 nil t)
	    (if (= month 1)
		(when (= (setq month (string-to-number (match-string 1))) 12)
		  (setq year (1- year)))
	      (setq month (string-to-number (match-string 1))))
	    (setq indices (list (list nil year month
				      (string-to-number (match-string 2)))))))
	(goto-char (match-end 0))
	(push (list (match-string 1)
		    (string-to-number (match-string 2))
		    (string-to-number (match-string 3))
		    (string-to-number (match-string 4)))
	      indices)))
    (setq indices (nreverse (if pages
				(last indices pages)
			      indices)))
    (while indices
      (setq index (pop indices)
	    year (nth 1 index)
	    month (nth 2 index)
	    day (nth 3 index)
	    index (car index)
	    date (shimbun-make-date-string year month day)
	    num 0)
      (when index
	(erase-buffer)
	(shimbun-retrieve-url index))
      (goto-char (point-max))
      (while (re-search-backward regexp2 nil t)
	(setq url (match-string 1)
	      base (match-string 2)
	      c (match-string 3)
	      subject (match-string 5))
	(setq increase (string-to-number (match-string 4)))
	(setq increase (> increase (prog1 num (setq num increase))))
	(setq id (format "<%d%02d%02d.%s.%d%%%s.%s>"
			 year month day c num group
			 shimbun-gendai-net-top-level-domain))
	(push (shimbun-create-header 0 subject from date id "" 0 0 url)
	      headers))
      (when (re-search-backward
	     (eval-when-compile
	       (let ((s0 "[\t\n\r ]*"))
		 (concat "<td>" s0 "<b>" s0 "\\([^<]+\\)"
			 s0 "</b>" s0 "</td")))
	     nil t)
	(setq subject (match-string 1)
	      id (format "<%d%02d%02d.%s.%d%%%s.%s>"
			 year month day c
			 (if increase (1+ num) (1- num))
			 group shimbun-gendai-net-top-level-domain))
	(push (shimbun-create-header
	       0 subject from date id "" 0 0
	       (format "%s%d" base (if increase (1+ num) (1- num))))
	      headers)))
    headers))

(defun shimbun-gendai-net-get-headers-default (shimbun pages)
  (let* ((regexp1
	  (eval-when-compile
	    (let ((s0 "[\t\n\r ]*"))
	      (concat ">" s0
		      ;; 1. month
		      "\\([01]?[0-9]\\)"
		      "月"
		      ;; 2. day
		      "\\([0-3]?[0-9]\\)"
		      "日\\(?:&nbsp;\\|" s0 "\\)?掲載" s0 "<"))))
	 (group (shimbun-current-group-internal shimbun))
	 (regexp2
	  (format
	   (eval-when-compile
	     (let ((s0 "[\t\n\r ]*")
		   (s1 "[\t\n\r ]+"))
	       (concat "<a" s1 "href=\""
		       ;; 1. url
		       "\\(http://gendai\\.net/\\?m=view&g=%s&c="
		       ;; 2. c
		       "\\([A-Z]*[0-9]+\\)"
		       "&no="
		       ;; 3. no
		       "\\([0-9]+\\)"
		       "\\)"
		       "\"" s0 ">"
		       ;; 4. subject
		       "\\([^<]+\\)"
		       s0 "</a>")))
	   group))
	 (regexp3
	  (format
	   (eval-when-compile
	     (let ((s0 "[\t\n\r ]*")
		   (s1 "[\t\n\r ]+"))
	       (concat "<a" s1 "href=\""
		       "\\(http://gendai\\.net/\\?m=new&g=%s&c=&s=&p=[0-9]+\\)"
		       "\"" s0 ">" s0 "[0-9]+" s0 "</a>")))
	   group))
	 (ctime (shimbun-decode-time nil 32400))
	 (from (concat "ゲンダイネット (" (shimbun-current-group-name shimbun)
		       ")"))
	 (count 0)
	 md start month day year date end url c num subject id headers
	 backnumbers)
    (catch 'stop
      (while t
	(while (cond ((eq md 'end)
		      nil)
		     (md
		      (set-match-data md)
		      (goto-char (match-end 0)))
		     (t
		      (re-search-forward regexp1 nil t)))
	  (setq start (match-end 0)
		month (string-to-number (match-string 1))
		day (string-to-number (match-string 2))
		year (cond ((and (= month 12)
				 (= (nth 4 ctime) 1))
			    (1- (nth 5 ctime)))
			   ((and (= month 1)
				 (= (nth 4 ctime) 12))
			    (1+ (nth 5 ctime)))
			   (t
			    (nth 5 ctime)))
		date (shimbun-make-date-string year month day))
	  (if (re-search-forward regexp1 nil t)
	      (setq end (match-beginning 0)
		    md (match-data))
	    (setq end nil
		  md 'end))
	  (goto-char start)
	  (while (re-search-forward regexp2 end t)
	    (setq url (match-string 1)
		  c (match-string 2)
		  num (match-string 3)
		  subject (match-string 4)
		  id (format "<%d%02d%02d.%s.%s%%%s.%s>"
			     year month day c num group
			     shimbun-gendai-net-top-level-domain))
	    (if (shimbun-search-id shimbun id)
		;;(throw 'stop nil)
		(setq backnumbers 'stop)
	      (push (shimbun-create-header 0 subject from date id "" 0 0 url)
		    headers))))
	(cond ((eq backnumbers 'stop)
	       (throw 'stop nil))
	      ((null backnumbers)
	       (while (re-search-forward regexp3 nil t)
		 (unless (member (setq id (match-string 1)) backnumbers)
		   (setq backnumbers (nconc backnumbers (list id)))))))
	(if (and backnumbers
		 (or (not pages)
		     (< (setq count (1+ count)) pages)))
	    (progn
	      (erase-buffer)
	      (shimbun-retrieve-url (car backnumbers))
	      (when (and (cdr backnumbers)
			 (re-search-forward
			  (concat
			   "<a[\t\n\r ]+href=\""
			   (regexp-quote (cadr backnumbers))
			   "\"[\t\n\r ]*>[\t\n\r ]*[0-9]+[\t\n\r ]*</a>")
			  nil t))
		(while (re-search-forward regexp3 nil t)
		  (unless (member (setq id (match-string 1)) backnumbers)
		    (setq backnumbers (nconc backnumbers (list id)))))
		(goto-char (point-min)))
	      (unless (setq backnumbers (cdr backnumbers))
		(setq backnumbers 'stop))
	      (setq md nil))
	  (throw 'stop nil))))
    headers))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-gendai-net)
						   header)
  (let (start)
    (if (if (string-equal (shimbun-current-group-internal shimbun) "today")
	    (when (and (re-search-forward "<!-+[\t\n\r ]*記事部[\t\n\r ]*-+>"
					  nil t)
		       (re-search-forward "<span[\t\n\r ]+[^>]+>[\t\n\r ]*"
					  nil t))
	      (setq start (match-end 0))
	      (re-search-forward "[\t\n\r ]*</span>" nil t))
	  (when (re-search-forward
		 "<!-+[\t\n\r ]*記事部[\t\n\r ]*-+>[\t\n\r ]*"
		 nil t)
	    (setq start (match-end 0))
	    (when (re-search-forward "<img[\t\n\r ]+src=[^>]+>\
\\(?:[\t\n\r ]*<[^>]+>[\t\n\r ]*\\)?"
				     nil t)
	      (setq start (match-end 0)))
	    (re-search-forward "[\t\n\r ]*</span>" nil t)))
	(progn
	  (goto-char (match-beginning 0))
	  (insert shimbun-gendai-net-content-end)
	  (goto-char start)
	  (insert shimbun-gendai-net-content-start))
      (erase-buffer)
      (insert "<html><body>\
This article seems to have been canceled or expired.\
</body></html>\n")))
  (goto-char (point-min)))

(luna-define-method shimbun-clear-contents :around ((shimbun
						     shimbun-gendai-net)
						    header)
  (when (luna-call-next-method)
    ;; Break long lines.
    (unless (shimbun-prefer-text-plain-internal shimbun)
      (shimbun-break-long-japanese-lines))
    t))

(provide 'sb-gendai-net)

;;; sb-gendai-net.el ends here
