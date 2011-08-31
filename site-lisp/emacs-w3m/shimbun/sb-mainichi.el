;;; sb-mainichi.el --- shimbun backend for Mainichi jp -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
;; Koichiro Ohba <koichiro@meadowy.org>

;; Author: Koichiro Ohba <koichiro@meadowy.org>
;;         Katsumi Yamaoka <yamaoka@jpl.org>
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

;; Original code was sb-yomiuri.el which is written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org> and
;; Yuuichi Teranishi <teranisi@gohome.org>

;;; Code:

(require 'sb-rss)
(require 'sb-multi)

(luna-define-class shimbun-mainichi (shimbun-japanese-newspaper
				     shimbun-multi shimbun-rss) ())

(defvar shimbun-mainichi-url "http://mainichi.jp/")

(defvar shimbun-mainichi-top-level-domain "mainichi.jp")

(defvar shimbun-mainichi-server-name "毎日新聞")

(defvar shimbun-mainichi-prefer-text-plain nil
  "*Non-nil means prefer text/plain articles rather than html articles.")

(defvar shimbun-mainichi-group-table
  '(("flash" "ニュース速報"
     "http://mainichi.jp/rss/etc/flash.rss")
    ("sports" "スポーツ"
     "http://mainichi.jp/rss/etc/sports.rss")
    ("entertainment" "エンターテインメント"
     "http://mainichi.jp/rss/etc/enta.rss")
    ("mantan" "アニメ・マンガ・ゲーム"
     "http://mainichi.jp/rss/etc/mantan.rss")
    ("electronics" "ＩＴ・家電"
     "http://mainichi.jp/rss/etc/electronics.rss")
    ("weekly" "英語を学ぶ"
     "http://mainichi.jp/rss/etc/weekly.rss")
    ;; Non-RSS groups.
    ("opinion.editorial" "社説"
     "http://mainichi.jp/select/opinion/editorial/archive/")
    ("opinion.yoroku" "余録"
     "http://mainichi.jp/select/opinion/yoroku/archive/")
    ("opinion.hasshinbako" "発信箱"
     "http://mainichi.jp/select/opinion/hasshinbako/archive/")
    ("opinion.eye" "記者の目"
     "http://mainichi.jp/select/opinion/eye/archive/")
    ("opinion.hito" "ひと"
     "http://mainichi.jp/select/opinion/hito/archive/")
    ("opinion.kinji" "近事片々"
     "http://mainichi.jp/select/opinion/kinji/archive/")
    ("opinion.yuraku" "憂楽帳"
     "http://mainichi.jp/select/opinion/yuraku/archive/")
    ("opinion.closeup" "クローズアップ"
     "http://mainichi.jp/select/opinion/closeup/archive/")
    ("opinion.kaisetsu" "土曜解説"
     "http://mainichi.jp/select/opinion/kaisetsu/")
    ("opinion.newsup" "ニュースＵＰ"
     "http://mainichi.jp/select/opinion/newsup/")
    ("entertainment.art" "芸術・文化"
     "http://mainichi.jp/enta/art/archive/")))

(defvar shimbun-mainichi-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAABwAAAAcBAMAAACAI8KnAAAABGdBTUEAALGPC/xhBQAAABh
 QTFRFC2zfDGzfEnDgJn3iU5fnj7vvzuH4+vz++nlsOQAAAC90RVh0U29mdHdhcmUAWFYgVmVyc2l
 vbiAzLjEwYStGTG1hc2sgIFJldjogMTIvMjkvOTQbx6p8AAABA0lEQVR4nGWRPW+DMBiED5vuqK0
 60yppVwQ0rFTYZo348poSA3uDzd/vCxNNT14en1/5zgZYEoCF69rk2zhWPR6GADwCl864tsaHFUJ
 dweTJuMcQZZ0kRQxkqnaHik2DbBwdVuPgtPG7WTcuhPdshdM5f7lp4SpyXUPoazu1i6HZpbY6R3a
 ZhAW8ztmZsDxPqf0Cb6zsVzQjJQA/2GNE2OWHbqaQvEggI7wFfOmxk1esLUL2GrJg2yBkrTSDqvB
 eJKmhqtNpttk3sllICskmdbXlGdkPNcd/TIuuvOxcM65IsxvSa2Q79w7V8AfL2u1nY9ZquuiWfK7
 1BSVNQzxF9B+40y/ui1KdNxt0ugAAAAd0SU1FB9QEDQAjJMA7GTQAAAAASUVORK5CYII=")))

(defvar shimbun-mainichi-expiration-days 7)

(luna-define-method shimbun-groups ((shimbun shimbun-mainichi))
  (mapcar 'car shimbun-mainichi-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-mainichi))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-mainichi-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-mainichi))
  (nth 2 (assoc (shimbun-current-group-internal shimbun)
		shimbun-mainichi-group-table)))

(luna-define-method shimbun-headers :around ((shimbun shimbun-mainichi)
					     &optional range)
  (if (string-match "\\.rss\\'" (shimbun-index-url shimbun))
      ;; Use the function defined in sb-rss.el.
      (luna-call-next-method)
    ;; Use the default function defined in shimbun.el.
    (funcall (intern "shimbun-headers"
		     (luna-class-obarray (luna-find-class 'shimbun)))
	     shimbun range)))

(luna-define-method shimbun-get-headers :around ((shimbun shimbun-mainichi)
						 &optional range)
  (let ((from (concat shimbun-mainichi-server-name " ("
		      (shimbun-current-group-name shimbun) ")"))
	headers)
    (if (string-match "\\.rss\\'" (shimbun-index-url shimbun))
	(progn
	  (shimbun-strip-cr)
	  (goto-char (point-min))
	  (while (and (search-forward "<title><![CDATA[AD:" nil t)
		      (re-search-backward "<item[\t\n ]*" nil t)
		      (shimbun-end-of-tag "item" t))
	    (replace-match "\n"))
	  (dolist (header (setq headers (luna-call-next-method)) headers)
	    (shimbun-header-set-from header from)))
      (shimbun-mainichi-get-headers shimbun range from))))

(luna-define-method shimbun-rss-build-message-id :around ((shimbun
							   shimbun-mainichi)
							  url &optional date)
  ;; Don't strip string following "?" or "#" in url.  See sb-rss.el.
  (concat "<" (md5 url) "%" (shimbun-current-group shimbun)
	  "@" (shimbun-server shimbun) ".shimbun.namazu.org>"))

(defun shimbun-mainichi-get-headers (shimbun range from)
  "Get headers for non-RSS groups."
  (let ((rgrp (mapconcat 'identity
			 (nreverse (split-string
				    (shimbun-current-group-internal shimbun)
				    "\\."))
			 "."))
	(count 0)
	(index (shimbun-index-url shimbun))
	(pages (shimbun-header-index-pages range))
	end id headers indices idx)
    (catch 'stop
      (while t
	(shimbun-strip-cr)
	(goto-char (point-min))
	(when (and (re-search-forward "\
<table[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"MidashiList\""
				      nil t)
		   (shimbun-end-of-tag "table"))
	  (goto-char (match-beginning 0))
	  (setq end (match-end 0))
	  (while (re-search-forward
		  (eval-when-compile
		    (concat
		     "<a[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*href=\""
		     ;; 1. url
		     "\\([^\"]+/"
		     ;; 2. serial number
		     "\\("
		     ;; 3. year
		     "\\(20[0-9][0-9]\\)"
		     ;; 4. month
		     "\\([01][0-9]\\)"
		     ;; 5. day
		     "\\([0-3][0-9]\\)"
		     "[^.]+\\)\\.html\\)\""
		     "\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]*>[\t\n ]*"
		     ;; 6. subject
		     "\\([^<]+\\)"))
		  end t)
	    (setq id (concat "<" (match-string 2) "." rgrp
			     "%" shimbun-mainichi-top-level-domain ">"))
	    (if (shimbun-search-id shimbun id)
		(unless (zerop count)
		  (throw 'stop nil))
	      (push (shimbun-create-header
		     0 (match-string 6) from
		     (shimbun-make-date-string
		      (string-to-number (match-string 3))
		      (string-to-number (match-string 4))
		      (string-to-number (match-string 5)))
		     id "" 0 0
		     (shimbun-expand-url (match-string 1) index))
		    headers))))
	(push index indices)
	(if (and (or (not pages)
		     (< (setq count (1+ count)) pages))
		 (progn
		   (goto-char (point-min))
		   (re-search-forward "\
<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"CalendarBar\""
				      nil t))
		 (shimbun-end-of-tag "div")
		 (progn
		   (setq end (match-beginning 0)
			 idx nil)
		   (while (and (not idx)
			       (re-search-backward "\
<a[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*href=\"\\([^\"]+\\)\""
						   end t))
		     (when (member (setq idx (shimbun-expand-url
					      (match-string 1) index))
				   indices)
		       (setq idx nil)))
		   idx))
	    (progn
	      (setq index idx)
	      (erase-buffer)
	      (shimbun-fetch-url shimbun index t)
	      (goto-char (point-min)))
	  (throw 'stop nil))))
    headers))

(luna-define-method shimbun-multi-next-url ((shimbun shimbun-mainichi)
					    header url)
  (shimbun-mainichi-multi-next-url shimbun header url))

(defun shimbun-mainichi-multi-next-url (shimbun header url)
  (goto-char (point-min))
  (when (and (re-search-forward "\
<div[\t\n ]\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"PageBtn\""
				nil t)
	     (shimbun-end-of-tag "div")
	     (re-search-backward "\
<a[\t\n ]+href=\"\\([^\"]+\\)\"[^>]*>[\t\n ]*次へ[^<]*</a>"
				 (match-beginning 0) t))
    (shimbun-expand-url (match-string 1) url)))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-mainichi)
						    header)
  (shimbun-mainichi-clear-contents shimbun header))

(defun shimbun-mainichi-clear-contents (shimbun header)
  (shimbun-strip-cr)
  (goto-char (point-min))
  (let ((group (shimbun-current-group-internal shimbun))
	(hankaku (shimbun-japanese-hankaku shimbun))
	(case-fold-search t)
	arts)
    (when (string-equal group "weekly")
      (when (and (re-search-forward "\
<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"Words\""
				    nil t)
		 (shimbun-end-of-tag "div"))
	(setq arts (list (match-string 2))))
      (when (and (re-search-forward "\
<\\([^\t\n >]+\\)[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"NewsTitle\""
				    nil t)
		 (shimbun-end-of-tag (match-string 1)))
	(setq arts (nconc arts (list (concat "<p>" (match-string 2) "</p>"))))))
    (while (and (re-search-forward "\
<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"\\(?:NewsBody\\|Credit\\)\""
				   nil t)
		(shimbun-end-of-tag "div"))
      (push (match-string 2) arts))
    (erase-buffer)
    (if arts
	(progn
	  (dolist (art (nreverse arts))
	    (insert art "\n"))

	  ;; Break continuous lines in yoroku articles.
	  (when (or (string-equal group "opinion.yoroku")
		    (string-match "\\`余録[:：]"
				  (shimbun-header-subject header t)))
	    (goto-char (point-min))
	    (while (re-search-forward
		    (eval-when-compile
		      (concat "[▲"
			      (condition-case nil
				  (list (make-char 'mule-unicode-2500-33ff
						   33 114))
				(error nil))
			      "]"))
		    nil t)
	      (replace-match "。</p>\n<p>　")))

	  ;; Convert Japanese zenkaku ASCII chars into hankaku.
	  (when (and hankaku (not (memq hankaku '(header subject))))
	    (shimbun-japanese-hankaku-buffer t))

	  (if (shimbun-prefer-text-plain-internal shimbun)
	      (progn
		;; Replace image tags with text.
		(goto-char (point-min))
		(while (and (re-search-forward "\\(<img[\t\n ]+\
\\(?:[^\t\n >]+[\t\n ]+\\)*alt=\"\\)[^\"]+"
					       nil t)
			    (shimbun-end-of-tag nil t))
		  (replace-match "\n&lt;写真&gt;\n")))
	    ;; Break long lines.
	    (shimbun-break-long-japanese-lines))
	  t)

      (insert "<html><body>\
この記事はもうありません。<br>\n\
\(さもなければ通常とは異なる形式を使っているか、<br>\n\
&nbsp;または取得に失敗したのかもしれません。)</body></html>\n")
      nil)))

(luna-define-method shimbun-multi-clear-contents :around ((shimbun
							   shimbun-mainichi)
							  header
							  has-previous-page
							  has-next-page)
  (shimbun-mainichi-multi-clear-contents shimbun header
					 has-previous-page has-next-page))

(defun shimbun-mainichi-multi-clear-contents (shimbun header
						      has-previous-page
						      has-next-page)
  (when (luna-call-next-method)
    (when has-previous-page
      (goto-char (point-min))
      (insert "&#012;\n"))
    t))

(provide 'sb-mainichi)

;;; sb-mainichi.el ends here
