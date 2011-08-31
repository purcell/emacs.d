;;; sb-yahoo-auctions.el --- shimbun backend for Yahoo! AUCTIONS

;; Copyright (C) 2005, 2006, 2008 ARISAWA Akihiro <ari@mbf.sphere.ne.jp>

;; Author: ARISAWA Akihiro <ari@mbf.sphere.ne.jp>
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
(require 'sb-rss)

(luna-define-class shimbun-yahoo-auctions (shimbun-rss) ())

(defcustom shimbun-yahoo-auctions-group-alist nil
  "*An alist of Yahoo! AUCTIONS group definition.
Each element looks like (NAME URL).
NAME is a shimbun group name.
URL is the URL for category or search result."
  :group 'shimbun
  :type '(repeat (cons :format "%v"
		       (string :tag "Group name")
		       (string :tag "URL"))))

(defvar shimbun-yahoo-auctions-content-start "<!--\nITEMSUMMARY\n-->")
(defvar shimbun-yahoo-auctions-content-end "<!--\nS\n-->")

(luna-define-method shimbun-groups ((shimbun shimbun-yahoo-auctions))
  (mapcar 'car shimbun-yahoo-auctions-group-alist))

(luna-define-method shimbun-group-p ((shimbun shimbun-yahoo-auctions) group)
  t)

(luna-define-method shimbun-from-address ((shimbun shimbun-yahoo-auctions))
  (format "Yahoo!オークション (%s)" (shimbun-current-group shimbun)))

(luna-define-method shimbun-index-url ((shimbun shimbun-yahoo-auctions))
  (let* ((group (shimbun-current-group shimbun))
	 (elem (assoc group shimbun-yahoo-auctions-group-alist)))
    (if elem
	(cdr elem)
      (concat "http://search.auctions.yahoo.co.jp/search_rss?p="
	      (shimbun-url-encode-string group 'euc-japan)))))

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-yahoo-auctions) url date)
  (unless (string-match "\\([^/]+\\)$" url)
    (error "Cannot find message-id base"))
  (format "<%s@auctions.yahoo.co.jp>" (match-string 1 url)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-yahoo-auctions)
					 &optional range)
  (nreverse (shimbun-rss-get-headers shimbun range t t)))

(luna-define-method shimbun-article-url :around
  ((shimbun shimbun-yahoo-auctions) header)
  (shimbun-real-url (luna-call-next-method)))

(provide 'sb-yahoo-auctions)

;;; sb-yahoo-auctions.el ends here
