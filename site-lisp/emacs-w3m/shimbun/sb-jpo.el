;;; sb-jpo.el --- shimbun backend for http://www.jpo.go.jp -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2003, 2004, 2005 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Keywords: news
;; Created: Apr. 28, 2003

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
;;
;; This shimbun backend categorizes the Japan Patent Office web site
;; to virtual four news groups.  Following is table of a site map
;; (summaerized page of http://www.jpo.go.jp/sitemap/index.htm) and
;; mapped virtual groups.
;;
;; ■制度の紹介                          ...lawguide
;; ■出願から審査、審判、登録まで        ...details
;; ■特許庁の紹介
;; ■特許庁の取り組み
;;    ■プレス発表                       ...news
;;    ■法令改正のお知らせ               ...revision
;;    ■広報の広場                       ...news
;;    ■...                              ...details
;;
;; ■資料室                              ...details
;; ■お問い合わせ
;; ■更新履歴
;; ■クイックガイド
;; ■調達情報・公募情報
;; ■意見募集：パブリック・コメント
;; ■特許電子図書館
;; ■独立行政法人　工業所有権総合情報館
;; ■関連ホームページリンク
;; ■旧特許庁ホームページ
;; ■電子政府の総合窓口（総務省行政管理局）
;; ■このサイトについて

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-jpo (shimbun) ())

(defconst shimbun-jpo-url "http://www.jpo.go.jp/")
(defvar shimbun-jpo-groups
  '("news" ;■プレス発表   ■広報の広場
    "revision" ;■法令改正のお知らせ
    "lawguide" ;■制度の紹介
    "details" ; ■出願から審査、審判、登録まで  ■特許庁の取り組み  ■資料室
    ))
(defvar shimbun-jpo-from-address "webmaster@jpo.go.jp")
(defvar shimbun-jpo-coding-system 'japanese-shift-jis)
(defvar shimbun-jpo-content-start "<body [^\n]+>")
(defvar shimbun-jpo-content-end "<\/body>")

;;(luna-define-method shimbun-reply-to ((shimbun shimbun-jpo))
;;  (shimbun-from-address-internal shimbun))
(defvar shimbun-jpo-debugging t)

(defun shimbun-jpo-retrieve-url (url &optional no-cache no-decode)
  (if shimbun-jpo-debugging
      (let (w3m-async-exec)
	(shimbun-retrieve-url url no-cache no-decode))
    (shimbun-retrieve-url url no-cache no-decode)))

(luna-define-method shimbun-headers ((shimbun shimbun-jpo) &optional range)
  (shimbun-jpo-headers shimbun))

(defun shimbun-jpo-headers (shimbun)
  (let ((group (shimbun-current-group-internal shimbun))
	(url shimbun-jpo-url)
	headers)
    (with-temp-buffer
      (if (string= group "news")
	  (progn
	    (setq url (concat url "torikumi/torikumi_list.htm"))
	    (shimbun-jpo-retrieve-url url 'reload)
	    (setq headers (shimbun-jpo-headers-1
			   shimbun url "\\(hiroba/.*\\)"))
	    (erase-buffer)
	    (setq url (concat shimbun-jpo-url
			      "torikumi/puresu/puresu_list.htm"))
	    (shimbun-jpo-retrieve-url url 'reload)
	    (setq headers (nconc headers (shimbun-jpo-headers-1 shimbun url))))
	(if (string= group "details")
	    (setq headers (shimbun-jpo-headers-group-details shimbun))
	  (if (string= group "revision")
	      (setq url (concat url "torikumi/kaisei/kaisei2/kaisei_list.htm"))
	    (if (string= group "lawguide")
		(setq url (concat url "seido/seido_list.htm"))
	      (error "unknown group %s" group)))
	  (shimbun-jpo-retrieve-url url 'reload)
	  (setq headers (shimbun-jpo-headers-1 shimbun url)))))
    headers))

(defun shimbun-jpo-headers-1 (shimbun origurl &optional urlregexp unmatchregexp)
  (let ((case-fold-search t)
	(from (shimbun-from-address shimbun))
	(group (shimbun-current-group-internal shimbun))
	(regexp (format "<td><font color=\"[#0-9A-Z]+\"><a href=\"\\(%s\\.html*\\)\">\\(.*\\)</a>[　 ]*\\([.0-9]+\\)" (or urlregexp "\\(.*\\)")))
	(urlprefix
	 (when (string-match "^\\(http:\/\/.+\\/\\)[^\/]+\\.html*" origurl)
	   (match-string 1 origurl)))
	headers id pagename subject tempdate date url)
    ;; <td><font color="#2346AB"><a href="h1504_pat_kijitu.htm">特許法等の一部を改正する法律の一部の施行期日を定める政令案について</a>　2003.4.21</font></td>
    ;; getting URL and SUBJECT
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch 'next
	(setq pagename (match-string-no-properties 1)
	      subject (match-string-no-properties 3)
	      date (match-string-no-properties 4))
	(when (and unmatchregexp (string-match unmatchregexp pagename))
	  (throw 'next nil))
	(setq url (shimbun-expand-url pagename urlprefix)
	      subject (with-temp-buffer
			  (insert subject)
			  (shimbun-remove-markup)
			  (buffer-string)))
	;; getting DATE
	(if (not (string-match
		  "\\([0-9]+\\)\\.\\([0-9]+\\)\\(\\.[0-9]+\\)?" date))
	    (throw 'next nil) ; unknown date format
	  (setq tempdate (list (string-to-number (match-string 1 date))
			       (string-to-number (match-string 2 date))))
	  (setq date (nconc tempdate 
			    (list
			     (if (not (match-string 3 date))
				1
			       (string-to-number
				(substring (match-string 3 date) 1)))))))
	;; building ID
	(setq id (format
		  "<%04d%02d%02d%%%s%%%s@jpo>"
		  (car date) (nth 1 date) (nth 2 date)
		  (if (string-match "^http:\/\/.+\\/\\([^\/]+\\.html*\\)"
				    pagename)
		      (match-string 1 pagename)
		    pagename)
		  group))
	(unless (shimbun-search-id shimbun id)
	  (setq date (apply 'shimbun-make-date-string date))
	  (push (shimbun-make-header
		 0 (shimbun-mime-encode-string subject)
		 from date id "" 0 0 url)
		headers))
	(forward-line 1)))
    headers))

(defun shimbun-jpo-headers-group-details (shimbun)
  (let ((case-fold-search t)
	(urllist '("torikumi/torikumi_list.htm"
		   "tetuzuki/tetuzuki_list.htm"
		   "shiryou/shiryou_list.htm"))
	(exceptions-alist
	 (list (cons "torikumi/" (list "kaisei/" "puresu/" "hiroba/"))))
	url headers pages urlprefix temp exceptions)
    (while urllist
      (when (string-match "\\/" (car urllist))
	(setq urlprefix (substring (car urllist) 0 (1+ (match-beginning 0)))))
      (setq url (shimbun-expand-url (car urllist) shimbun-jpo-url))
      (erase-buffer)
      (shimbun-jpo-retrieve-url url 'reload)
      (setq exceptions (cdr (assoc urlprefix exceptions-alist)))
      (goto-char (point-min))
      ;; gathering header information of articles in the current page.
      (setq headers (nconc headers
			   (shimbun-jpo-headers-1
			    shimbun url nil
			    (when exceptions
			      (concat "\\(" 
				      (mapconcat 'regexp-quote exceptions "\\|")
				      "\\)")))))
      (goto-char (point-min))
      (while (re-search-forward
	      ;;<td><font color="#2346AB"><a href="puresu/puresu_list.htm">プレス発表</a></font></td>
	      "<td><font color=\"[#0-9A-Z]+\"><a href=\"\\(.*\\.htm\\)\">[^<>]+<\/a><\/font><\/td>"
	      nil t nil)
	;; getting sub-categories.
	(catch 'next
	  (setq temp (match-string 1))
	  (dolist (ex exceptions)
	    (when (string-match ex temp)
	      (throw 'next nil)))
	  (setq pages (cons (shimbun-expand-url
			     temp
			     (concat shimbun-jpo-url urlprefix)) pages))))
      (while pages
	(setq url (car pages))
	(erase-buffer)
	(shimbun-jpo-retrieve-url url 'reload)
	;; getting header information of articles in pages of the sub-categories.
	(setq headers (nconc
		       headers
		       (shimbun-jpo-headers-1 shimbun url)))
	(setq pages (cdr pages)))
      (setq urllist (cdr urllist)))
    headers))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-jpo)
						    header)
  (when (luna-call-next-method)
    (goto-char (point-min))
    (when (re-search-forward
	   ;;Ｅ−mail：<a href="mailto:PA0A00@jpo.go.jp">PA0A00@jpo.go.jp<br>
	   ;;E-mail：<a href="mailto:PA0420@jpo.go.jp">PA0420@jpo.go.jp</a></font>
	   ;;　E-mail:<a href="mailto:PA0A00@jpo.go.jp"> PA0A00@jpo.go.jp</a></font>
	   "\\(電子メール\\|[ＥｅEe][−-]*[ＭｍMm][ＡａAa][ＩｉIi][ＬｌLl]\\)[：:　] *<a href=\"mailto:\\(.*@.*jpo.go.jp\\)\"> *\\2"
	   nil t nil)
      (shimbun-header-set-from header (match-string 2)))
    (shimbun-jpo-cleanup-article)
    t))

(defun shimbun-jpo-cleanup-article ()
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-min))
      ;; <td align="center"><a href="#top"><img src="images/gotop.gif" width="89" height="13" border="0" vspace="10" alt="ページの先頭へ"></a></td>
      (while (re-search-forward
	      "<img src=\"images/gotop.gif\" .*alt=\"ページの先頭へ\">"
	      nil t nil)
	(delete-region (progn (beginning-of-line) (point)) (progn (end-of-line) (point))))
      (goto-char (point-min))
      ;; <td align="left"><a href="../../../indexj.htm" target="_top">HOME</a> &gt; <a href="../../torikumi_list.htm">特許庁の取り組み（特許法第３０条等新規性の喪失の例外）の適用に関して）</a> &gt;<br><br></td>
      ;; <td align="left"><a href="../../indexj.htm" target="_top">HOME</a> &gt; <a href="../torikumi_list.htm">特許庁
      (while (re-search-forward
	      "<td align=\"left\"><a href=\"\\(\\.\\./\\)+indexj.htm\" target=\"_top\">HOME<\/a> *\\&gt;"
	      nil t nil)
	(delete-region (match-beginning 0) (progn (end-of-line) (point))))
      (goto-char (point-min))
      (while (re-search-forward 
	      "<tr>\n+<td align=\"left\"><img src=\"\\(\\.\\./\\)?images/title\\.gif\" *[^<>]+\">\\(<\/a>\\)?<\/td>\n+<\/tr>"
	      nil t nil)
	(delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (while (re-search-forward
	      "^\\(<td>\\)?<a href=\"http://www.adobe.co.jp/products/acrobat/readstep.html"
	      nil t nil)
	(delete-region (match-beginning 0) (progn (end-of-line) (point))))
      (goto-char (point-min))
      (while (re-search-forward
	      ;; PDFファイルを初めてお使いになる方は、Adobe Acrobat Readerダウンロードページへ   
	      "Adobe Acrobat Reader *ダウンロードページ"
	      nil t nil)
	(delete-region (progn (beginning-of-line) (point)) (progn (end-of-line) (point))))
      (goto-char (point-min))
      (while (re-search-forward 
	      "<tr>\n+<td align=\"center\"><a href=\"#top\">\
<img src=\"\\(\\.\\.\/\\)?images/gotop\\.gif\" [^<>]+\">\\(<\/a>\\)?<\/td>\n+<\/tr>"
	      nil t nil)
	(delete-region (match-beginning 0) (match-end 0))))))

(provide 'sb-jpo)

;;; sb-jpo.el ends here
