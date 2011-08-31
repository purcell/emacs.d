;;; sb-coldsync.el --- shimbun backend for www.coldsync.org

;; Copyright (C) 2003, 2004 NAKAJIMA Mikio  <minakaji@namazu.org>

;; Author: 2003 NAKAJIMA Mikio  <minakaji@namazu.org>
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

;;; Code:

(require 'shimbun)
(require 'sb-mhonarc)

(luna-define-class shimbun-coldsync (shimbun-mhonarc) ())

(defvar shimbun-coldsync-url "http://www.thedotin.net/maillists/coldsync-hackers/maillist.html")
(defvar shimbun-coldsync-groups '("main"))
(defvar shimbun-coldsync-reverse-flag nil)

(provide 'sb-coldsync)

;;; sb-coldsync.el ends here
