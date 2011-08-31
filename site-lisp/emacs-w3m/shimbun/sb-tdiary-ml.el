;;; sb-tdiary-ml.el --- shimbun backend for www.tDiary.org

;; Copyright (C) 2003, 2007 Koichiro Ohba  <koichiro@meadowy.org>

;; Author: Koichiro Ohba  <koichiro@meadowy.org>
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

;; Original code was sb-airs.el which is written by
;; Yuuichi Teranishi <teranisi@gohome.org>

;;; Code:

(require 'shimbun)
(require 'sb-mhonarc)

(luna-define-class shimbun-tdiary-ml (shimbun-mhonarc) ())

(defconst shimbun-tdiary-ml-group-path-alist
  '(("devel" "archive/devel" "tDiary-devel@lists.sourceforge.net")
    ("theme" "archive/theme" "tDiary-theme@lists.sourceforge.net")))

(defvar shimbun-tdiary-ml-url "http://www.tdiary.org/")
(defvar shimbun-tdiary-ml-groups (mapcar 'car shimbun-tdiary-ml-group-path-alist))
(defvar shimbun-tdiary-ml-reverse-flag nil)
(defvar shimbun-tdiary-ml-litemplate-regexp
  "<strong><a name=\"\\([0-9]+\\)\" href=\"\\(msg[0-9]+.html\\)\">\\([^<]+\\)</a></strong>,\n      <em>\\([^<]+\\)</em>")

(defmacro shimbun-tdiary-ml-concat-url (shimbun url)
  `(concat (shimbun-url-internal ,shimbun)
	   (nth 1 (assoc (shimbun-current-group-internal ,shimbun)
			 shimbun-tdiary-ml-group-path-alist))
	   "/"
	   ,url))

(luna-define-method shimbun-index-url ((shimbun shimbun-tdiary-ml))
  (shimbun-tdiary-ml-concat-url shimbun "index.html"))

(luna-define-method shimbun-reply-to ((shimbun shimbun-tdiary-ml))
  (nth 2 (assoc (shimbun-current-group-internal shimbun)
		shimbun-tdiary-ml-group-path-alist)))

(provide 'sb-tdiary-ml)

;;; sb-tdiary-ml.el ends here
