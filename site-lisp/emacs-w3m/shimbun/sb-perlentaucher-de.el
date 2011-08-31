;;; sb-perlentaucher-de.el --- perlentaucher.de shimbun backend

;; Copyright (C) 2008, 2009 David Engster

;; Author: David Engster <dengste@eml.cc>
;; Keywords: news

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

(luna-define-class shimbun-perlentaucher-de (shimbun-rss) ())

(defvar shimbun-perlentaucher-de-url "http://rss.perlentaucher.de")
(defvar shimbun-perlentaucher-de-groups '("aktuell"))
(defvar shimbun-perlentaucher-de-from-address "invalid@perlentaucher.de")
(defvar shimbun-perlentaucher-de-content-start "<div class=\"col_middle\">")
(defvar shimbun-perlentaucher-de-content-end "<div class=\"col_right\">")
(defvar shimbun-perlentaucher-de-url-regexp
  "rss.feedsportal.com/.*/\\([^/]+\\)/story.*\\.htm")

(defvar shimbun-perlentaucher-de-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAAA8AAAAPAgMAAABGuH3ZAAAADFBMVEUAern/+/D///8aGhp
 47OwqAAAALklEQVQI12P4////H4aroaExDBcYGGSgBKsDkJCaACSylqAQYDGwLFwxWC/IFAD6jBr
 V/YdWgAAAAABJRU5ErkJggg==")))

(luna-define-method shimbun-index-url ((shimbun shimbun-perlentaucher-de))
  shimbun-perlentaucher-de-url)

(luna-define-method shimbun-clear-contents :before ((shimbun
						     shimbun-perlentaucher-de)
						    header)
  (shimbun-remove-tags "<div class=\"tools\">" "Merkzettel</a></li></ul>")
  (shimbun-remove-tags "<div class=\"box2 jumper\">" "</div>")
  (shimbun-remove-tags "<a href=\"#top\">" "</a>"))



(luna-define-method shimbun-rss-build-message-id ((shimbun
						   shimbun-perlentaucher-de)
						  url date)
  (unless (string-match shimbun-perlentaucher-de-url-regexp url)
    (error "Cannot find message-id base"))
  (concat "<" (match-string 1 url)
	  "." (shimbun-current-group-internal shimbun) "@perlentaucher.de>"))

(provide 'sb-perlentaucher-de)

;;; sb-perlentaucher-de.el ends here
