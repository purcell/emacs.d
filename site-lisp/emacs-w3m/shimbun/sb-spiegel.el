;;; sb-spiegel.el --- spiegel online shimbun backend

;; Copyright (C) 2004, 2006, 2008 David Hansen

;; Author: David Hansen <david.hansen@physik.fu-berlin.de>
;; Keywords: news

;; This file is a part of shimbun.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-spiegel (shimbun-rss) ())

(defvar shimbun-spiegel-url
  "http://www.spiegel.de/schlagzeilen/rss/0,5291,,00.xml")
(defvar shimbun-spiegel-groups '("news"))
(defvar shimbun-spiegel-from-address  "spiegel_online@spiegel.de")
(defvar shimbun-spiegel-content-start "<div id=\"spMainContent\">")
(defvar shimbun-spiegel-content-end "<div class=\"spArticleCredit\">")
(defvar shimbun-spiegel-x-face-alist
  '(("default" . "X-Face: \"F#SZ#pUmtu/<qtxz=G'w#244Hp7}y|vSO?j@i?6g-uGJ2&a/g#\
U96H{_VK#k&,3O\"L)6;Z823T4;}r1R,rLedLu2hQ%biKv(LR@VJXA6XRJ`0xk!I'k!c<uH6R!.+}S\
l1}uY-+WD)So~O]jsKT@}|?Z%!fVwHBde3rd5WLW^^I]UM*z>/K|u59;;-")))

(luna-define-method shimbun-index-url ((shimbun shimbun-spiegel))
  shimbun-spiegel-url)

(luna-define-method shimbun-get-headers :around ((shimbun shimbun-spiegel)
						 &optional range)
  (mapcar
   (lambda (header)
     (let ((url (shimbun-header-xref header)))
       (when (string-match "\\([0-9]+\\),[0-9]+\\.html" url)
	 (shimbun-header-set-xref header
				  (replace-match "druck-\\1" t nil url 1))))
     header)
   (luna-call-next-method)))

(provide 'sb-spiegel)

;;; sb-spiegel.el ends here
