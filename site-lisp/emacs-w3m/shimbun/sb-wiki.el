;;; sb-wiki.el --- shimbun backend for wiki

;; Copyright (C) 2003, 2004, 2006 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Keywords: news
;; Created: Jun 19, 2003

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
;;  This program currently supports PukiWiki and Hiki.
;;  For more detail about PukiWiki, see http://pukiwiki.org/index.php.
;;  See http://www.namaraii.com/hiki/?FrontPage about Hiki.

;;; Code:

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-wiki (shimbun-rss) ())

(defcustom shimbun-wiki-group-alist
  '(("pukiwiki"
     "http://pukiwiki.org/index.php?cmd=rss10"
     "webmaster@pukiwiki.org"
     nil
     "\n<h3 id=\""
     "</address>")
    ("hiki"
     "http://www.namaraii.com/hiki/?c=rss"
     "webmaster@namaraii.com"
     nil
     "<div class=\"section\">"
     "<div class=\"sidebar\">"))
  "*An alist of Wiki shimbun group definition.
Each element is a list such as
   \(NAME URL ADDRESS X-FACE CONTENT-START CONTENT-END\).
NAME is a shimbun group name.
URL is the URL for Wiki access point of the group.
Note that sb-wiki.el supports only RSS version 1.0 (0.91 does not
have date tags).
ADDRESS is the e-mail address for the diary owner.
Optional X-FACE is a string for X-Face field.
Optional CONTENT-START is a regexp string that represents content
start of each article.
Optional CONTENT-END is a regexp string that represents content
start of each article."
  :group 'shimbun
  :type '(repeat
	  (group (string :tag "Group name")
		 (string :tag "URL")
		 (string :tag "Site owner's mail address")
		 (choice (string :tag "X-Face")
			 (const :tag "No X-Face" nil))
		 (regexp :tag "Content beginning pattern")
		 (regexp :tag "Content end pattern"))))

(luna-define-method shimbun-headers :before ((shimbun shimbun-wiki)
					     &rest range)
  (shimbun-set-from-address-internal
   shimbun
   (nth 2 (assoc (shimbun-current-group-internal shimbun)
		 shimbun-wiki-group-alist)))
  shimbun)

(luna-define-method shimbun-x-face ((shimbun shimbun-wiki))
  (or (shimbun-x-face-internal shimbun)
      (shimbun-set-x-face-internal
       shimbun
       (or
	(nth 3 (assoc (shimbun-current-group-internal shimbun)
		      shimbun-wiki-group-alist))
	(cdr (assoc "default" (shimbun-x-face-alist-internal shimbun)))
	shimbun-x-face))))

(luna-define-method shimbun-groups ((shimbun shimbun-wiki))
  (mapcar 'car shimbun-wiki-group-alist))

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-wiki) url date)
  (let (page host datedesc)
    (unless (string-match "http:\\/\\/\\([^\/]+\\)\\/.+\\?\\(.+\\)" url)
      (error "Cannot find message-id base"))
    (setq host (match-string-no-properties 1 url)
	  page (match-string-no-properties 2 url))
    (unless (string-match "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)T\\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)" date)
      (error "Cannot find message-id base"))
    (setq datedesc (concat (match-string-no-properties 1 date)
			   (match-string-no-properties 2 date)
			   (match-string-no-properties 3 date)
			   (match-string-no-properties 4 date)
			   (match-string-no-properties 5 date)))
  (format "<%s%%%s@%s>" datedesc page host)))

(luna-define-method shimbun-index-url ((shimbun shimbun-wiki))
  (cadr (assoc (shimbun-current-group-internal shimbun)
	       shimbun-wiki-group-alist)))

(luna-define-method shimbun-clear-contents ((shimbun shimbun-wiki) header)
  (let* ((case-fold-search t)
	 (alist (assoc (shimbun-current-group shimbun)
		       shimbun-wiki-group-alist))
	 (cstart (nth 4 alist))
	 (cend (nth 5 alist))
	 start)
    (when (and cstart cend
	       (re-search-forward cstart nil t)
	       (setq start (point))
	       (re-search-forward cend nil t))
      (delete-region (match-beginning 0) (point-max))
      (delete-region (point-min) start)
      t)))

(provide 'sb-wiki)

;;; sb-wiki.el ends here
