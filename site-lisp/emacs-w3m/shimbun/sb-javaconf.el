;;; sb-javaconf.el --- shimbun backend class for java-conference archive.

;; Copyright (C) 2001, 2002, 2003, 2004, 2007 ABE Yasushi <yasushi@stbbs.net>

;; Author: ABE Yasushi <yasushi@stbbs.net>
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

(eval-when-compile
  (require 'cl))

(require 'shimbun)
(require 'sb-mhonarc)

(luna-define-class shimbun-javaconf (shimbun-mhonarc) ())

(defvar shimbun-javaconf-url "http://www.java-conf.gr.jp/archives/")
(defvar shimbun-javaconf-groups '("servlet-ml" "business-ml" "duke-in-the-box-ml"
				  "jfriends-ml" "JGT-ml" "jini-ml" "ejb-ml" "cm-ml"
				  "horb-ml" "talk-ml"))
(defvar shimbun-javaconf-reverse-flag nil)
(defvar shimbun-javaconf-litemplate-regexp
  "<strong><a NAME=\"\\([0-9]+\\)\" HREF=\"\\(msg[0-9]+\.html\\)\">\\([^<]+\\)\n</a></strong> <em>\\([^<]+\\)\n</em>")

(defmacro shimbun-javaconf-concat-url (shimbun url)
  `(concat (shimbun-url-internal ,shimbun)
	   (shimbun-current-group-internal ,shimbun)
	   "/"
	   ,url))

(luna-define-method shimbun-get-headers ((shimbun shimbun-javaconf)
					 &optional range)
  (let* ((group (shimbun-current-group-internal shimbun))
	 (regexp (format "<a href=\"\\(%s/\\(mail[0-9]+\.html\\)?\\)\"" (regexp-quote group)))
	 (case-fold-search t)
	 (pages (shimbun-header-index-pages range))
	 (count 0)
	 (indeces)
	 (headers))
    (while (re-search-forward regexp nil t)
      (push (shimbun-expand-url (match-string 1) shimbun-javaconf-url) indeces))
    (catch 'stop
      (dolist (elem indeces)
	(delete-region (point-min) (point-max))
	(unless (if pages (<= (incf count) pages) t)
	  (throw 'stop headers))
	(shimbun-retrieve-url elem t)
	(goto-char (point-min))
	(shimbun-mhonarc-get-headers shimbun elem headers)))
      headers))

(provide 'sb-javaconf)

;;; sb-javaconf.el ends here
