;;; sb-asahi-html.el --- shimbun backend for asahi.com (HTML version)

;; Copyright (C) 2001, 2002, 2003, 2004, 2005
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

;; This module is now semi-obsolete.  You can use sb-asahi.el to read
;; html articles by putting the following line in your init file.
;;
;;(setq shimbun-asahi-prefer-text-plain nil)

;;; Code:

(require 'sb-asahi)

(luna-define-class shimbun-asahi-html (shimbun-asahi) ())

(defconst shimbun-asahi-html-prefer-text-plain nil
  "Non-nil means prefer text/plain articles rather than html articles.")

(provide 'sb-asahi-html)

;;; sb-asahi-html.el ends here
