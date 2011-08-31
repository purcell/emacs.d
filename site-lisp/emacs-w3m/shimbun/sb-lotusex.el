;;; sb-lotusex.el --- shimbun backend for http://tsuruo.dominohosting.biz -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2003, 2004 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Keywords: news
;; Created: May. 22, 2003

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

(luna-define-class shimbun-lotusex (shimbun) ())

(defconst shimbun-lotusex-url "http://tsuruo.dominohosting.biz/members/tsuruo/")

(defvar shimbun-lotusex-groups-alist
  '(
    ("news" . "notes/nhome.nsf/PlainAllPage!OpenPage")
    ;;ノーツデータベース陳列室
    ("library" . "notes/nhome.nsf/LiblaryPage!OpenPage")
    ;; ノーツ操作室
    ("operation" . "notes/nhome.nsf/OperatePage!OpenPage")
    ;; ノーツデータベース作成超入門講座
    ("primer" . "notes/nhome.nsf/PrimerPage!OpenPage")
    ;; ノーツ技術室
    ("tips" . "notes/nhome.nsf/TipsPage!OpenPage")
    ;;  ノーツ活用研究室
    ("practical" . "notes/nhome.nsf/PracticalPage!OpenPage")
    ;;  ノーツ陳列館質問室
    ("qanda" . "notes/nhome.nsf/QuestionPage!OpenPage")
    ;; ノーツ陳列館休憩室
    ("lounge" . "notes/nhome.nsf/RestPage!OpenPage")
    ;; ノーツ陳列館外壁（ドミノWEB版）
    ("bbs")
    ))

(defvar shimbun-lotusex-groups
  (mapcar 'car shimbun-lotusex-groups-alist))
(defvar shimbun-lotusex-from-address "webmaster@tsuruo.dominohosting.biz")
(defvar shimbun-lotusex-coding-system 'iso-2022-jp)
(defvar shimbun-lotusex-content-start "<table CELLPADDING=\"2\"[>]+>")
(defvar shimbun-lotusex-content-end "<table CELLPADDING=\"2\"[>]+>")

(defun shimbun-lotusex-make-date-string (date-string &optional time)
  (when (string-match
	 "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)"
	 date-string)
    (shimbun-make-date-string
     (string-to-number
      (match-string-no-properties 1 date-string))
     (string-to-number
      (match-string-no-properties 2 date-string))
     (string-to-number
      (match-string-no-properties 3 date-string))
     time)))

;;(luna-define-method shimbun-reply-to ((shimbun shimbun-lotusex))
;;  (shimbun-from-address-internal shimbun))

(luna-define-method shimbun-index-url ((shimbun shimbun-lotusex))
  (let ((group (shimbun-current-group-internal shimbun)))
    (if (not (string= group "bbs"))
	(concat shimbun-lotusex-url
		(cdr (assoc group shimbun-lotusex-groups-alist)))
      "http://d.dominodeveloper.net/members/tsuruo/bbs/NotesBBS.nsf/wMainViewPage!OpenPage")))

(luna-define-method shimbun-get-headers
  ((shimbun shimbun-lotusex) &optional outbuf)
  (let ((group (shimbun-current-group-internal shimbun)))
    (if (string= group "bbs")
	(shimbun-lotusex-bbs-headers shimbun)
      (shimbun-lotusex-headers shimbun group))))

(defun shimbun-lotusex-bbs-headers (shimbun)
  (let (url from date subject end time ampm id headers
	     case-fold-search)
    (subst-char-in-region (point-min) (point-max) ?\t ?  t)
    (goto-char (point-min))
    (catch 'exit
      (while (re-search-forward
	      "<SPAN STYLE=\"font-size: 12px\">\\([0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]\\)(\\(月\\|火\\|水\\|木\\|金\\|土\\|日\\))</SPAN>"
	      nil t nil)
	(setq date (match-string-no-properties 1))
	(save-excursion
	  (setq end (or (re-search-forward
			 "<SPAN STYLE=\"font-size: 12px\">\\([0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]\\)(\\(月\\|火\\|水\\|木\\|金\\|土\\|日\\))</SPAN>"
			 nil t nil)
			(point-max))))
	(while (re-search-forward
		"<a href=\"\\(/members/tsuruo/bbs/NotesBBS\\.nsf/[/a-zA-Z0-9]+!OpenDocument\\)\".+> *\\[[0-9]+\\]\\(.+\\)\\[ +\\(.+\\) +,\\([0-9][0-9]:[0-9][0-9]\\) \\(AM\\|PM\\) ]</SPAN>"
		end t nil)
	  (setq url (concat "http://d.dominodeveloper.net"
			    (match-string-no-properties 1))
		subject (match-string-no-properties 2)
		from (match-string-no-properties 3)
		time (match-string-no-properties 4)
		ampm (match-string-no-properties 5)
		id (format "<%s%%%s%%%s@lotusex>"
			   date
			   (when (string-match "\\/\\([a-zA-Z0-9]+\\)\\!OpenDocument" url)
			     (match-string 1 url))
			   "bbs"))
	  (when (shimbun-search-id shimbun id)
	    (throw 'exit nil))
	  (when (and (string= ampm "PM")
		     (string-match ":" time))
	    (setq time (format
			"%2d:%s"
			(+ (string-to-number (substring time 0 (match-beginning 0)))
			   12)
			(substring time (match-end 0)))))
	  (when (string-match "^ +\\(.+\\)" subject)
	    (setq subject (match-string 1 subject)))
	  (when (string-match "^\\(.+\\) +$" subject)
	    (setq subject (match-string 1 subject)))
	  (when (string-match "^ +\\(.+\\)" from)
	    (setq from (match-string 1 from)))
	  (when (string-match "^\\(.+\\) +$" from)
	    (setq from (match-string 1 from)))
	  (push (shimbun-make-header
		 0 (shimbun-mime-encode-string subject)
		 (shimbun-mime-encode-string from)
		 (shimbun-lotusex-make-date-string date time)
		 id "" 0 0 url)
		headers))))
    headers))

(defun shimbun-lotusex-headers (shimbun group)
  (let (url from date subject id headers
	     case-fold-search)
    (subst-char-in-region (point-min) (point-max) ?\t ?  t)
    (goto-char (point-min))
    (catch 'exit
      (while (re-search-forward
	      "<a href=\"/members/tsuruo/\\(notes/nhome\\.nsf/[/a-zA-Z0-9]+!OpenDocument\\)\".+>[0-9A-Z]+\\(.+\\)(\\([0-9]+/[0-9]+/[0-9]+\\)) *\\(- 【.+】\\)?</SPAN>"
	      nil t nil)
	(setq url (match-string-no-properties 1)
	      subject (match-string-no-properties 2)
	      from shimbun-lotusex-from-address
	      date (match-string-no-properties 3)
	      subject (concat subject (match-string-no-properties 4))
	      id (format "<%s%%%s%%%s@lotusex>"
			 date
			 (when (string-match "\\/\\([a-zA-Z0-9]+\\)\\!OpenDocument" url)
			   (match-string 1 url))
			 group))
	(when (shimbun-search-id shimbun id)
	  (throw 'exit nil))
	(setq date (shimbun-lotusex-make-date-string date)
	      url (concat shimbun-lotusex-url url))
	(when (string-match "^ +\\(.+\\)" subject)
	  (setq subject (match-string 1 subject)))
	(when (string-match "^\\(.+\\) +$" subject)
	  (setq subject (match-string 1 subject)))
	(when (string-match "^ +\\(.+\\)" from)
	  (setq from (match-string 1 from)))
	(when (string-match "^\\(.+\\) +$" from)
	  (setq from (match-string 1 from)))
	(push (shimbun-make-header
	       0 (shimbun-mime-encode-string subject)
	       (shimbun-mime-encode-string from)
	       date id "" 0 0 url)
	      headers)))
    headers))

(luna-define-method shimbun-make-contents :before
  ((shimbun shimbun-lotusex) header)
  (shimbun-lotusex-make-contents shimbun header))

(defun shimbun-lotusex-make-contents (shimbun header)
  (let ((group (shimbun-current-group-internal shimbun))
	start)
    (when (string= group "bbs")
      (save-excursion
	(goto-char (point-min))
	(when (and (re-search-forward "^<form action=\"\">" nil t)
		   (setq start (point))
		   (re-search-forward "^</form>$" nil t))
	  (delete-region (match-beginning 0) (point-max))
	  (delete-region (point-min) start))))))

(provide 'sb-lotusex)
;;; sb-lotusex.el ends here
