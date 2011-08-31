;;; sb-rediff.el --- shimbun backend for rediff.com

;; Copyright (C) 2004, 2005, 2006 S V N Vishwanathan <vishketan@yahoo.com>

;; Author: S V N Vishwanathan <vishketan@yahoo.com>
;; Keywords: news
;; Created: Nov 22, 2004

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
(require 'sb-rss)

(luna-define-class shimbun-rediff (shimbun-rss) ())

(defvar shimbun-rediff-url "http://www.rediff.com/rss/newsrss.xml")
(defvar shimbun-rediff-groups '("news"))
(defvar shimbun-rediff-from-address "news@rediff.com")
(defvar shimbun-rediff-content-start "<BR></FONT>")
(defvar shimbun-rediff-content-end
  "\\(</P>\\)?</FONT>\\(</FONT>\\)?</TD></TR><TR><TD>")

(defconst shimbun-rediff-month-alist
  '(("January" . 1) ("February" . 2) ("March" . 3) ("April" . 4)
    ("May" . 5) ("June" . 6) ("July" . 7) ("August" . 8)
    ("September" . 9) ("October" . 10) ("November" . 11) ("December" . 12)))

;; Print version has less ads

(luna-define-method shimbun-article :before
  ((shimbun shimbun-rediff) header &optional outbuf)
  (let ((url (shimbun-article-url shimbun header)))
    (unless
	(string-match
	 "http://www.rediff.com/rss/redirect.php\\?url=\
http://www.rediff.com/\\(.+\\.htm\\)"
	 url)
      (error "Malformed URL? %s" url))
    (shimbun-header-set-xref
     header
     (concat
      "http://in.rediff.com/cms/print.jsp?docpath="
      (match-string-no-properties 1 url)))))

;; Tags to strip from the print version

(luna-define-method shimbun-clear-contents :before
  ((shimbun shimbun-rediff) header)
  (when (luna-call-next-method)
    (shimbun-remove-tags "<A class=\"\" [^>]+>" "</A>")
    (shimbun-remove-tags "<A target=new [^>]+>" "</A>")
    (shimbun-remove-tags "<P><STRONG>" "</STRONG></A>")
    (shimbun-remove-tags "<P><STRONG>Also read:" "</STRONG>\\(</P>\\)?")
    (shimbun-remove-tags "<UL[^>]*><LI[^>]*>" "</LI></UL>")
    (shimbun-remove-tags
     "<TABLE cellSpacing=0 cellPadding=0 width=200 align=left border=0>"
     "</TABLE></TD></TR></TABLE>")))

;; The default header has no date string
;; We need to parse it from the contents and set the header

(luna-define-method shimbun-make-contents :before
  ((shimbun shimbun-rediff) header)
;;; <DEBUG>
;;  (shimbun-rediff-make-contents shimbun header))
;;
;;(defun shimbun-rediff-make-contents (shimbun header)
;;; </DEBUG>
  (setq case-fold-search nil)
  (when (re-search-forward
	 "\\(January\\|February\\|March\\|April\\|May\\|June\
\\|July\\|August\\|September\\|October\\|November\\|December\\)[ ]+\
\\([0-3][0-9]\\),[ ]+\\(20[0-9][0-9]\\) | \\([0-2][0-9]:[0-6][0-9]\\) IST"
	 nil t)
    (shimbun-header-set-date
     header
     (shimbun-make-date-string
      (string-to-number (match-string-no-properties 3))
      (cdr (assoc (match-string-no-properties 1) shimbun-rediff-month-alist))
      (string-to-number (match-string-no-properties 2))
      (match-string-no-properties 4)
      "+05:30"))
    (goto-char (point-min))))

(provide 'sb-rediff)

;;; sb-rediff.el ends here
