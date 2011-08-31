;;; sb-msdn.el --- shimbun backend for MSDN

;; Copyright (C) 2004, 2005, 2006 Yoichi NAKAYAMA <yoichi@geiin.org>

;; Author: Yoichi NAKAYAMA <yoichi@geiin.org>
;; Keywords: news
;; Created: Sep 14, 2004

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

(luna-define-class shimbun-msdn (shimbun-rss) ())

(defvar shimbun-msdn-group-alist
  '(("all" "http://msdn.microsoft.com/rss.xml")
    ("netframework" "http://msdn.microsoft.com/netframework/rss.xml")
    ("architecture" "http://msdn.microsoft.com/architecture/rss.xml")
    ("asp.net" "http://msdn.microsoft.com/asp.net/rss.xml")
    ("data" "http://msdn.microsoft.com/data/rss.xml")
    ("longhorn" "http://msdn.microsoft.com/longhorn/rss.xml")
    ("mobility" "http://msdn.microsoft.com/mobility/rss.xml")
    ("subscriptions" "http://msdn.microsoft.com/subscriptions/rss.xml")
    ("msdntv" "http://msdn.microsoft.com/msdntv/rss.xml")
    ("office" "http://msdn.microsoft.com/office/rss.xml")
    ("security" "http://msdn.microsoft.com/security/rss.xml")
    ("sql" "http://msdn.microsoft.com/sql/rss.xml")
    ("theshow" "http://msdn.microsoft.com/theshow/rss.xml")
    ("vbasic" "http://msdn.microsoft.com/vbasic/rss.xml")
    ("vcsharp" "http://msdn.microsoft.com/vcsharp/rss.xml")
    ("visualc" "http://msdn.microsoft.com/visualc/rss.xml")
    ("vfoxpro" "http://msdn.microsoft.com/vfoxpro/rss.xml")
    ("vjsharp" "http://msdn.microsoft.com/vjsharp/rss.xml")
    ("vstudio" "http://msdn.microsoft.com/vstudio/rss.xml")
    ("vs2005" "http://msdn.microsoft.com/vs2005/rss.xml")
    ("webservices" "http://msdn.microsoft.com/webservices/rss.xml")
    ("embedded" "http://msdn.microsoft.com/embedded/rss.xml")
    ("xml" "http://msdn.microsoft.com/xml/rss.xml")
    ("japan.msdn" "http://www.microsoft.com/japan/msdn/rss.xml")
    ("japan.msdn-us" "http://www.microsoft.com/japan/msdn/aboutmsdn/us/rss.xml"))
  "Alist of readable groups and URLs of their RSSs.
List is available at:
 http://msdn.microsoft.com/aboutmsdn/rss/
 http://www.microsoft.com/japan/msdn/aboutmsdn/rss/")

(defvar shimbun-msdn-from-address "nobody@microsoft.com")
(defvar shimbun-msdn-content-start "\\(<!--\\(pull table\\|BEGIN_CONTENT\\| Begin Content \\)-->\\|<table id=\"hpcontenttable\".*>\\)")
(defvar shimbun-msdn-content-end "\\(</body>\\|<!--\\(END_CONTENT\\| End Content \\)-->\\)")

(luna-define-method shimbun-groups ((shimbun shimbun-msdn))
  (mapcar 'car shimbun-msdn-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-msdn))
  (nth 1 (assoc (shimbun-current-group shimbun) shimbun-msdn-group-alist)))

(luna-define-method shimbun-rss-process-date ((shimbun shimbun-msdn) date)
  date)

(luna-define-method shimbun-article-url ((shimbun shimbun-msdn) header)
  (let ((url (shimbun-article-base-url shimbun header)))
    (if (string-match "\\`http://msdn\\.microsoft\\.com/\
library/default\\.asp\\?url=\\(.+\\.asp\\)\\'" url)
	(concat "http://msdn.microsoft.com" (match-string 1 url))
      url)))

(provide 'sb-msdn)

;;; sb-msdn.el ends here
