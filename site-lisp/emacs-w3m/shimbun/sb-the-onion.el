;;; sb-the-onion.el --- The Onion shimbun backend

;; Copyright (C) 2006 David Hansen

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

(luna-define-class shimbun-the-onion (shimbun-rss) ())

(defvar shimbun-the-onion-url "http://www.theonion.com/content/feeds/daily")
(defvar shimbun-the-onion-groups '("news"))
(defvar shimbun-the-onion-from-address  "invalid@theonion.com")
(defvar shimbun-the-onion-content-start "<!-- begin content -->")
(defvar shimbun-the-onion-content-end "<!-- end content -->")

(luna-define-method shimbun-index-url ((shimbun shimbun-the-onion))
  shimbun-the-onion-url)

(luna-define-method shimbun-get-headers :around
  ((shimbun shimbun-the-onion) &optional range)
  (mapcar
   (lambda (header)
     (let ((url (shimbun-header-xref header)))
       (when (string-match "[0-9]+\\(\\?.*=RSS\\)" url)
	 (shimbun-header-set-xref
	  header (concat (substring url 0 (match-beginning 1)) "/print/"))))
     header)
   (luna-call-next-method)))

(provide 'sb-the-onion)

;;; sb-the-onion.el ends here
