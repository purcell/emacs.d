;;; sb-pilot-mailsync.el --- shimbun backend for http://lists.gnu-designs.com/pipermail/pilot-mailsync/

;; Copyright (C) 2003, 2004, 2005 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
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

(require 'shimbun)
(require 'sb-mailman)

(luna-define-class shimbun-pilot-mailsync (shimbun-mailman) ())

(defvar shimbun-pilot-mailsync-url "http://lists.gnu-designs.com/pipermail/pilot-mailsync/")

(defvar shimbun-pilot-mailsync-groups '("main"))

(luna-define-method shimbun-reply-to ((shimbun shimbun-pilot-mailsync))
  "Return the mailing list address."
  "pilot-mailsync@pilot-mailsync.sourcefubar.net")

(provide 'sb-pilot-mailsync)
;;; sb-pilot-mailsync.el ends here
