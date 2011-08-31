;;; sb-infoshop.el --- infoshop shimbun backend

;; Copyright (C) 2005, 2006, 2007 David Hansen

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

(luna-define-class shimbun-infoshop (shimbun-rss) ())

(defvar shimbun-infoshop-url
  "http://www.infoshop.org/inews/backend/news.rdf")
(defvar shimbun-infoshop-groups '("news"))
(defvar shimbun-infoshop-from-address  "invalid@infoshop.org")
(defvar shimbun-infoshop-coding-system
  (or (shimbun-find-coding-system 'windows-1252)
      (shimbun-find-coding-system 'iso-8859-1))
  "Coding system used to decode article contents.")

(luna-define-method shimbun-index-url ((shimbun shimbun-infoshop))
  shimbun-infoshop-url)

(luna-define-method shimbun-rss-build-message-id :around
    ((shimbun shimbun-infoshop) url &optional date)
  (if (string-match "\\?story=\\(.*+\\)$" url)
      (concat "<" (match-string 1 url) "@news.infoshop.org>")
    (luna-call-next-method)))

(luna-define-method shimbun-get-headers :around ((shimbun shimbun-infoshop)
						 &optional range)
  (mapcar
   (lambda (header)
     (shimbun-header-set-xref header (concat (shimbun-header-xref header)
					     "&mode=print"))
     header)
   (luna-call-next-method)))

(provide 'sb-infoshop)

;;; sb-infoshop.el ends here
