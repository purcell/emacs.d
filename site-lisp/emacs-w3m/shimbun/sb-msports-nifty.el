;;; sb-msports-nifty.el --- shimbun backend for motorsports.nifty.com

;; Copyright (C) 2004, 2005, 2006 MIYOSHI Masanori <miyoshi@meadowy.org>

;; Author: MIYOSHI Masanori <miyoshi@meadowy.org>
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

;; This back end generates text/plain articles unless failing to
;; extract contents.

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;;; Code:

(require 'shimbun)
(require 'sb-text) ;; For `shimbun-shallow-rendering'.

(luna-define-class shimbun-msports-nifty (shimbun) ())

(defvar shimbun-msports-nifty-url "http://forum.nifty.com/fmotor/")
(defvar shimbun-msports-nifty-server-name "@nifty:モータースポーツ")
(defvar shimbun-msports-nifty-group-alist
  '(("F1" . "f1")
    ("IRL" . "cart")
    ("WRC" . "wrc")
    ("Europe" . "europe")
    ("USA" . "usa")))
(defvar shimbun-msports-nifty-from-address "motorsports_post@nifty.com")
(defvar shimbun-msports-nifty-content-start "<div class=\"entry-body-text\">")
(defvar shimbun-msports-nifty-content-end "<!-- New Menu End -->")

(luna-define-method shimbun-groups ((shimbun shimbun-msports-nifty))
  (mapcar 'car shimbun-msports-nifty-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-msports-nifty))
  (concat (shimbun-url-internal shimbun)
	  (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-msports-nifty-group-alist))
	  "/main.htm"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-msports-nifty)
					 &optional range)
  (let ((case-fold-search t) headers)
    (goto-char (point-min))
    (while (re-search-forward
	    "<A HREF=\"\\(http://.*/\\([0-9]+\\)/[0-9][0-9]\\([0-9][0-9]\\)\\([0-9][0-9]\\)_\\(.*\\)\.htm\\)\"[^>]*>☆　\\([^<]+\\)<" nil t)
      (let ((url (match-string 1))
	    (year (match-string 2))
	    (month (match-string 3))
	    (day (match-string 4))
	    (id (match-string 5))
	    (subject (match-string 6))
	    date)
	(setq id (format "<%s%s%s%s%%%s%%msports@nifty.com>"
			 year month day
			 id (shimbun-current-group-internal shimbun)))
	(setq date (shimbun-make-date-string
		    (string-to-number year)
		    (string-to-number month)
		    (string-to-number day)))
	(push (shimbun-create-header
	       0
	       subject
	       (shimbun-from-address shimbun)
	       date id "" 0 0 url)
	      headers)))
    headers))

(luna-define-method shimbun-make-contents ((shimbun shimbun-msports-nifty)
					   header)
  (let ((id (shimbun-header-id header)))
    (setq id (substring id 9 20))	; extract anchor
    (re-search-forward (format "<a id=\"%s\"></a>" id) nil t)
    (delete-region (point-min) (point))
    (shimbun-header-insert-and-buffer-string
     shimbun header "UTF-8"
     (if (shimbun-clear-contents shimbun header)
	 (progn (shimbun-shallow-rendering) nil)
       t))))

(provide 'sb-msports-nifty)

;;; sb-msportsn-nifty.el ends here
