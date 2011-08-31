;;; sb-macosx-jp.el --- shimbun backend for macosx-jp

;; Copyright (C) 2002 OHASHI Akira <bg66@koka-in.org>

;; Author: OHASHI Akira <bg66@koka-in.org>
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
(require 'sb-fml)

(luna-define-class shimbun-macosx-jp (shimbun-fml) ())

(defvar shimbun-macosx-jp-url "http://www.tech-arts.co.jp/macosx/")
(defvar shimbun-macosx-jp-groups
  '("macosx-jp" "macosx-dev-jp" "macosx-ws-jp" "webobjects-jp"))

(luna-define-method shimbun-index-url ((shimbun shimbun-macosx-jp))
  (concat (shimbun-url-internal shimbun)
	  (shimbun-current-group-internal shimbun) "/htdocs/"))
(luna-define-method shimbun-reply-to ((shimbun shimbun-macosx-jp))
  (concat (shimbun-current-group-internal shimbun) "@ml.tech-arts.co.jp"))

(provide 'sb-macosx-jp)

;;; sb-macosx-jp.el ends here
