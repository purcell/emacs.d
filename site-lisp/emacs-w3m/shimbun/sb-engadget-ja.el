;;; sb-engadget-ja.el --- shimbun backend for japanese.engadget.com -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2005, 2006, 2007 Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>

;; Author: Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>
;; Keywords: news
;; Created: July 1, 2005

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

(defvar shimbun-engadget-ja-group-alist
  '(("top" ."http://japanese.engadget.com/rss.xml")))

(luna-define-class shimbun-engadget-ja (shimbun-rss) ())

(defvar shimbun-engadget-ja-content-start
  (eval-when-compile
    (regexp-opt '("<div id=\"incontent\">"
		"<div id=\"content\">"))))
(defvar shimbun-engadget-ja-content-end
  (eval-when-compile
    (regexp-opt '("<h3>Recent Posts</h3>"
		"<h3 id=\"recentheadlines\">Recent Posts</h3>"
		"<h3 id=\"recentheadlines\">最近の記事</h3>"
		"<a name=\"comments\"></a>"))))

(luna-define-method shimbun-groups ((shimbun shimbun-engadget-ja))
  (mapcar 'car shimbun-engadget-ja-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-engadget-ja))
  (cdr (assoc (shimbun-current-group-internal shimbun)
	      shimbun-engadget-ja-group-alist)))

(provide 'sb-engadget-ja)

;;; sb-engadget-ja.el ends here
