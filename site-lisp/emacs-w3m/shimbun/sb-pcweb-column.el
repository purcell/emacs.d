;;; sb-pcweb-column.el --- shimbun backend for PC WEB COLUMN Square

;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007
;; OHASHI Akira <bg66@koka-in.org>

;; Author: OHASHI Akira <bg66@koka-in.org>
;;         Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>
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

(luna-define-class shimbun-pcweb-column (shimbun) ())

(defvar shimbun-pcweb-column-url "http://journal.mycom.co.jp/column/")

(defvar shimbun-pcweb-column-groups
  '(;; Under a series
    "jsr" "yume" "hreceipe" "kita" "shonanlife" "kaden" "nemurenai" "komono"
    "js" "en" "motherboard" "svalley" "architecture" "motorlife" "nihongoprog"
    "objc" "ide" "music" "itsecurity" "soundvisual" "osx" "sopinion" "ebook"
    "orerobo" "zsh" "rikei" "lifehack" "world" "guutara" "volt" "textclean"
    "person" "web20" "system"
    ;; Series end
    "itshihonron" "yetanother" "asia" "benri" "bytes" "game" "hitech" "java"
    "jisakuparts" "scramble" "toolexp" "winvista" "winxp" "interview" "ityougo"
    "kimeuchi" "stratesys" "toyagain"))

(defvar shimbun-pcweb-column-from-address "pcmail@pc.mycom.co.jp")
(defvar shimbun-pcweb-column-content-start
  "<!-- #BeginEditable \"contents\" -->")
(defvar shimbun-pcweb-column-content-end "<!-- #EndEditable -->")
(defvar shimbun-pcweb-column-coding-system 'shift_jis)

(luna-define-method shimbun-index-url ((shimbun shimbun-pcweb-column))
  (concat (shimbun-url-internal shimbun)
	  (shimbun-current-group-internal shimbun) "/"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-pcweb-column)
					 &optional range)
  (let ((case-fold-search t)
	(headers)
	(pattern
	 (format
	  "<a href=\"\\(/column/%s/\\([0-9][0-9][0-9]\\)\
/?\\(index.html?\\)?\\)\">\\([^<]+\\)</a>"
	  (regexp-quote (shimbun-current-group shimbun)))))
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (let ((url (match-string 1))
	    (num (match-string 2))
	    (subject (match-string 4))
	    id)
	;; do not modify for compatibility.
	(setq id (format "<%s.%s.column@pcweb.mycom.co.jp>"
			 num (shimbun-current-group-internal shimbun)))
	(push (shimbun-make-header
	       0
	       (shimbun-mime-encode-string subject)
	       (shimbun-from-address shimbun)
	       "" id "" 0 0
	       (shimbun-expand-url url (shimbun-url-internal shimbun)))
	      headers)))
    headers))

(luna-define-method shimbun-article :before
  ((shimbun shimbun-pcweb-column) header &optional outbuf)
  (shimbun-header-set-xref header
			   (shimbun-replace-in-string
			    (shimbun-header-xref header)
			    (format "/%s\\([0-9]+\\)\\.html\\'"
				    (regexp-quote
				     (shimbun-current-group shimbun)))
			    "/\\1/")))

(luna-define-method shimbun-make-contents :before
  ((shimbun shimbun-pcweb-column) header)
  (let (case-fold-search)
    (goto-char (point-min))
    (when (re-search-forward "<!-- #BeginEditable \"author\" -->\
\\(<p[^>]*>\\)?\\([^<]+\\)\\(</p>\\)?<!-- #EndEditable -->" nil t)
      (shimbun-header-set-from header (match-string 2)))
    (goto-char (point-min))
    (when (re-search-forward "<!-- #BeginEditable \"[Cc]ontentsDate\" -->\
\\(<p[^>]*>\\)?\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)\\(</p>\\)?\
<!-- #EndEditable -->" nil t)
      (shimbun-header-set-date header
			       (shimbun-make-date-string
				(string-to-number (match-string 2))
				(string-to-number (match-string 3))
				(string-to-number (match-string 4)))))))

(provide 'sb-pcweb-column)

;;; sb-pcweb-column.el ends here
