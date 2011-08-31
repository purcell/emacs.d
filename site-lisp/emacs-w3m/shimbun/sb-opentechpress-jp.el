;;; sb-opentechpress-jp.el --- shimbun backend for japan.linux.com -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2006, 2007, 2009 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

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

;;; Code:

(require 'shimbun)
(require 'sb-multi)
(require 'sb-rss)

(luna-define-class shimbun-opentechpress-jp (shimbun-multi shimbun-rss) ())

(defvar shimbun-opentechpress-jp-table
  '(("general" . "http://opentechpress.jp/index.rss")
    ("enterprise" . "http://opentechpress.jp/enterprise.rss")
    ("opensource" . "http://opentechpress.jp/opensource.rss")
    ("developer" . "http://opentechpress.jp/developer.rss")
    ("security" . "http://opentechpress.jp/security.rss")
    ("news" . "http://opentechpress.jp/news.rss")
    ("pr" . "http://opentechpress.jp/pr.rss")))

(defvar shimbun-opentechpress-jp-content-start
  "<div class=\"article\">")
(defvar shimbun-opentechpress-jp-content-end
  "</div><!-- end: class=\"article\" -->")

(defvar shimbun-opentechpress-jp-ignored-subject "^PR:")

(luna-define-method initialize-instance :after ((shimbun
						 shimbun-opentechpress-jp)
						&rest init-args)
  (shimbun-rss-initialize-ignored-subject shimbun))

(luna-define-method shimbun-groups ((shimbun shimbun-opentechpress-jp))
  (mapcar 'car shimbun-opentechpress-jp-table))

(luna-define-method shimbun-index-url ((shimbun shimbun-opentechpress-jp))
  (cdr (assoc (shimbun-current-group shimbun) shimbun-opentechpress-jp-table)))

(luna-define-method shimbun-rss-build-message-id ((shimbun
						   shimbun-opentechpress-jp)
						  url &optional date)
  (concat "<" (md5 (if (string-match "[?#]" url)
		       (substring url 0 (match-beginning 0))
		     url))
	  "+" (when (string-match "[?&]sid=\\([^&]+\\)\\(&\\|\\'\\)" url)
		(match-string 1 url))
	  "%" (shimbun-current-group shimbun)
	  "@" (shimbun-server shimbun) ".shimbun.namazu.org>"))

(luna-define-method shimbun-multi-next-url ((shimbun shimbun-opentechpress-jp)
					    header url)
  (goto-char (point-min))
  (when (re-search-forward "<a href=\"\\([^\"]+\\)\" title=\"次のページ\""
			   nil t)
    (shimbun-expand-url (shimbun-decode-entities-string (match-string 1))
			url)))

(luna-define-method shimbun-multi-clear-contents ((shimbun
						   shimbun-opentechpress-jp)
						  header
						  has-previous-page
						  has-next-page)
  (when (shimbun-clear-contents shimbun header)
    (when has-previous-page
      (goto-char (point-min))
      (when (search-forward "<div id=\"article-body\">" nil t)
	(delete-region (point-min) (match-beginning 0))))
    (when has-next-page
      (goto-char (point-max))
      (when (search-backward "</div><!-- id=\"article-body\" -->" nil t)
	(delete-region (match-end 0) (point-max))))
    (shimbun-remove-tags "<div class=\"pagemenu\">"
			 "</div><!-- class=\"pagemenu\" -->")
    (shimbun-remove-tags "<span class=\"pagemenu\">"
			 "</span><!-- class=\"pagemenu\" -->")
    t))

(luna-define-method shimbun-clear-contents :before ((shimbun
						     shimbun-opentechpress-jp)
						    header)
  (shimbun-remove-tags "<SCRIPT" "</SCRIPT>")
  (shimbun-remove-tags "<NOSCRIPT" "</NOSCRIPT>")
  (shimbun-remove-tags "<div id=\"story-action\">"
		       "</div><!-- class=\"story-action\" -->"))

(provide 'sb-opentechpress-jp)

;;; sb-opentechpress-jp.el ends here
