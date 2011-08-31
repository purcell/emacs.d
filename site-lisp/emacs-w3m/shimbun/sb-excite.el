;;; sb-excite.el --- shimbun backend for excite -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2004, 2005, 2006 Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>

;; Author: Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>
;; Keywords: news
;; Created: Dec 24, 2004

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

(luna-define-class shimbun-excite (shimbun-rss) ())

(defvar shimbun-excite-top-level-domain "excite.co.jp"
  "Name of the top level domain for the excite.")

(defvar shimbun-excite-url
  (concat "http://www." shimbun-excite-top-level-domain "/News/")
  "Name of the parent url.")

(defvar shimbun-excite-group-alist
  '(("bit-koneta" . "xml/rss_excite_news_bit_index_utf_8.dcg")
    ("world-odd"  . "xml/rss_excite_news_odd_index_utf_8.dcg")))

(defvar shimbun-excite-from-address (concat "nobody@"
					    shimbun-excite-top-level-domain))
(defvar shimbun-excite-content-start
  "<font[^>]*>\\[ *[0-9]*年[0-9]*月[0-9]*日 *[0-9]*時[0-9]*分 *\\]</font>")
(defvar shimbun-excite-content-end
  "<center")

;(defvar shimbun-excite-expiration-days 14)

(defvar shimbun-excite-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAAD4AAAAZCAMAAABetm34AAAAe1BMVEUAAACaHA4WCganBSTMA
 ABJBQM1Cw26AwR8CQzfADlfCw4gERh7e3sICwjv7+8vLyiUkpIJCxMzMzNZWVn///9yb27FxMTe3t5
 SSU9mZmYgIiC1tbWjq5tCRkrc09bq7Oc+Rjatra2+vb1/h3jMzMyNjIsAAQju9/eZmZlRX0WQAAABo
 ElEQVR42qVT53rCMAwUlJ1FYmUHh4ApvP8TViNpA19LGfoRW4pPupNlgFubwEs2ner6ce0+bD1uduU
 9C58qfL54HKhH50v+rlT7+hnZK4Ww3I2QWDzFfSIlJz/dWi6e0r1ewTu2WA46NlzXezHNdLPmpvlXw
 cAPNeD5fyOjbUzdn82oaX4y/mEQMaU1Q8zvVC7KippA7S/qYBxPmqbZ0Yq/w43N85oqQ9oSnpCWOe5
 t2+Ydh3dZlhlIqThiVnK+Q57bgWCIYuweNQvSx2kUy4H8SX3KbHV3EnRNu88IMQSRV0FZb6k2RduUT
 w3wVBK6Utw9yB9VhIZDotfPj5YTdRTYQeOci75b12t3A6AY4GKxZLOYJT08uen8CC7GMWgZWVWeohN
 fSTHvLZyF1AgeQgwNrR4DBLHj6OXiLDuVwXMdjknZq+qIRwh46S6HVrmlw7k4MSViZazjSdMLyUbkm
 77fiQJCKE9GGBtT0I0bF8pFWexkVPdG1cdRFMXKzVTQb0xEc1rE4/lRrkDyX3oygV5+4O7O9r/m4fm
 t1394+OQXiQ8ls/z5EToAAAAASUVORK5CYII=")))

(luna-define-method shimbun-groups ((shimbun shimbun-excite))
  (mapcar 'car shimbun-excite-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-excite))
  (concat shimbun-excite-url
	  (cdr  (assoc  (shimbun-current-group-internal shimbun)
			shimbun-excite-group-alist))))

(luna-define-method shimbun-rss-build-message-id :around
  ((shimbun shimbun-excite) url date)
  (if (string-match
       (concat (regexp-quote shimbun-excite-url)
	       "\\([^/]\\)*/\\([0-9]+\\)\.html?")
       url)
      (luna-call-next-method)
    nil))

(luna-define-method shimbun-get-headers :around ((shimbun shimbun-excite)
						 &optional range)
  (let ((headers (luna-call-next-method)))
    (dolist (header headers)
      (shimbun-header-set-from header (shimbun-from-address shimbun)))
    headers))

(luna-define-method shimbun-clear-contents :before
  ((shimbun shimbun-excite) header)
  (shimbun-strip-cr)
  (shimbun-remove-tags "<script" "</script>")
  (shimbun-remove-tags "<noscript" "</noscript>"))

(provide 'sb-excite)

;;; sb-excite.el ends here
