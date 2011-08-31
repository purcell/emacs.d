;;; sb-airs.el --- shimbun backend for lists.airs.net

;; Copyright (C) 2001, 2002, 2003, 2005, 2007
;; Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi  <teranisi@gohome.org>
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

;; Original was nnshimbun-airs.el on http://homepage2.nifty.com/strlcat/

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'shimbun)
(require 'sb-mhonarc)

(luna-define-class shimbun-airs (shimbun-mhonarc) ())

(defconst shimbun-airs-group-path-alist
  '(("semi-gnus-ja" "semi-gnus/archive" "semi-gnus-ja@meadowy.org")
    ("wl" "wl/archive" "wl@lists.airs.net")
    ("wl-en" "wl-en/archive" "wl-en@lists.airs.net")))

(defvar shimbun-airs-url "http://lists.airs.net/")
(defvar shimbun-airs-groups (mapcar 'car shimbun-airs-group-path-alist))
(defvar shimbun-airs-reverse-flag nil)
(defvar shimbun-airs-litemplate-regexp
  "<STRONG><a name=\"\\([0-9]+\\)\" href=\"\\(msg[0-9]+.html\\)\">\\([^<]+\\)</a></STRONG> <EM>\\([^<]+\\)</EM>")

(defmacro shimbun-airs-concat-url (shimbun url)
  `(concat (shimbun-url-internal ,shimbun)
	   (nth 1 (assoc (shimbun-current-group-internal ,shimbun)
			 shimbun-airs-group-path-alist))
	   "/"
	   ,url))

(luna-define-method shimbun-index-url ((shimbun shimbun-airs))
  (shimbun-airs-concat-url shimbun "index.html"))

(luna-define-method shimbun-reply-to ((shimbun shimbun-airs))
  (nth 2 (assoc (shimbun-current-group-internal shimbun)
		shimbun-airs-group-path-alist)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-airs)
					 &optional range)
  (let ((case-fold-search t)
	(pages (shimbun-header-index-pages range))
	(count 0)
	headers months month url)
    (goto-char (point-min))
    (catch 'stop
      (while (and (if pages (<= (incf count) pages) t)
		  (re-search-forward
		   "<A HREF=\"\\([12][0-9][0-9][0-9][01][0-9]\\)/\">"
		   nil t))
	(push (match-string 1) months))
      (setq months (nreverse months))
      (while months
	(setq month (pop months)
	      url (shimbun-airs-concat-url shimbun (concat month "/")))
	(erase-buffer)
	(shimbun-retrieve-url url t)
	(shimbun-mhonarc-get-headers shimbun url headers month)))
    headers))

(provide 'sb-airs)

;;; sb-airs.el ends here
