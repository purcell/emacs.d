;;; sb-debugmagazin-de.el --- de-bug.de shimbun backend

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

(luna-define-class shimbun-debugmagazin-de (shimbun-rss) ())

(defvar shimbun-debugmagazin-de-group-url-regexp
  '(("frontpage" "http://feeds.feedburner.com/DebugAllInOne"
     ".*/\\(.*\\)\\.html")
    ("musik" "http://feeds.feedburner.com/DebugMusik"
     "musik/\\(.*\\)\\.html")
    ("reviews" "http://feeds.feedburner.com/DebugReviews"
     "reviews/\\(.*\\)\\.html")
    ("magazin" "http://feeds.feedburner.com/DebugMagazin"
     ".*/\\(.*\\)\\.html")
    ("medien" "http://feeds.feedburner.com/DebugMedien"
     ".*/\\(.*\\)\\.html")
    ("podcast" "http://de-bug.de/pod/feed"
     ".*/\\(.*\\)\\.html")
    ("musiktechnik" "http://feeds.feedburner.com/de-bug/SSgQ"
     ".*/\\(.*\\)\\.html")
    ("screen" "http://feeds.feedburner.com/DebugScreen"
     ".*/\\(.*\\)\\.html")
    ("gadgets" "http://feeds.feedburner.com/DebugGadgets"
     ".*/\\(.*\\)\\.html")
    ("games" "http://feeds.feedburner.com/DebugGames"
     ".*/\\(.*\\)\\.html")
    ("mode" "http://feeds.feedburner.com/DebugMode"
     ".*/\\(.*\\)\\.html")))

(defvar shimbun-debugmagazin-de-groups
  (mapcar 'car shimbun-debugmagazin-de-group-url-regexp))
(defvar shimbun-debugmagazin-de-from-address "invalid@debugmagazin.de")

(defvar shimbun-debugmagazin-de-content-start
  "<div class=\"post\"\\( id=\"post-[0-9]+\"\\)?>")

(defvar shimbun-debugmagazin-de-content-end
  "\\(?:<!-- Comment Loop -->\\|<!-- Comment Form -->\\|<div id=\"respond\">\\)")

(defvar shimbun-debugmagazin-de-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAAD1BMVEUAAADAwMD/AACAgID
 ////NQiVkAAAAP0lEQVQI12NwgQIGOMOBQYSBBcxwZGFxUQIxXFjQRWBqsGpncXBmYAErdnAAiTC
 bABnOcBEWkBoBRmcGE0ztACqGGS6ml7Z1AAAAAElFTkSuQmCC")))

(luna-define-method shimbun-index-url ((shimbun shimbun-debugmagazin-de))
  (let ((group (shimbun-current-group-internal shimbun)))
    (cadr (assoc group shimbun-debugmagazin-de-group-url-regexp))))

(luna-define-method shimbun-rss-build-message-id ((shimbun
						   shimbun-debugmagazin-de)
						  url date)
  (let* ((group (shimbun-current-group-internal shimbun))
	 (urlregexp
	  (car (last (assoc group
			    shimbun-debugmagazin-de-group-url-regexp)))))
    (unless (string-match urlregexp url)
      (error "Cannot find message-id base"))
    (concat "<" (match-string 1 url) "." group "@debugmagazin.de>")))

(luna-define-method shimbun-get-headers :around ((shimbun
						  shimbun-debugmagazin-de)
						 &optional range)
  ;; Show the group name in the From header.
  (let ((group (shimbun-current-group-internal shimbun))
	(headers (luna-call-next-method))
	from)
    (dolist (header headers headers)
      (setq from (shimbun-header-from header)
	    from (mapconcat (lambda (w)
			      (if (string-match "\\`[^<>@]+@[^<>@]+\\'" w)
				  (concat "<" w ">")
				w))
			    (delete "" (split-string from))
			    " "))
      (unless (string-match (concat "(" (regexp-quote group) ")") from)
	(shimbun-header-set-from header (concat from " (" group ")"))))))

(provide 'sb-debugmagazin-de)

;;; sb-debugmagazin-de.el ends here
