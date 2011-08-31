;;; sb-n24-de.el --- shimbun backend for <http://www.n24.de/>

;; Copyright (C) 2004 Andreas Seltenreich <seltenreich@gmx.de>

;; Author: Andreas Seltenreich <seltenreich@gmx.de>
;; Keywords: news
;; Created: May 23, 2004

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

(luna-define-class shimbun-n24-de (shimbun-rss) ())

(defvar shimbun-n24-de-groups
  '("wirtschaft"
    "nachrichten"
    "boulevard"
    "sport"
    "netnews"
    "politik"
    "boerse"))

(defvar shimbun-n24-de-content-start "<!--bild mit bildteaser anfang-->")
(defvar shimbun-n24-de-content-end "\n<br>\n<br>\n")
(defvar shimbun-n24-de-from-address "redaktion@n24.de")

(luna-define-method shimbun-headers :before ((shimbun shimbun-n24-de)
					     &rest range)
  shimbun)

(luna-define-method shimbun-groups ((shimbun shimbun-n24-de))
  shimbun-n24-de-groups)

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-n24-de) url date)
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

(luna-define-method shimbun-index-url ((shimbun shimbun-n24-de))
  (concat "http://www.n24.de/rss/?rubrik="
	  (shimbun-current-group-internal shimbun)))

(provide 'sb-n24-de)

;;; sb-n24-de.el ends here
