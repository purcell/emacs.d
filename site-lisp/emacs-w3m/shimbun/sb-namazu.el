;;; sb-namazu.el --- shimbun backend for namazu.org

;; Copyright (C) 2001, 2002, 2003, 2004 Akihiro Arisawa  <ari@mbf.sphere.ne.jp>

;; Author: Akihiro Arisawa <ari@mbf.sphere.ne.jp>
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
(require 'sb-mailman)

(luna-define-class shimbun-namazu (shimbun-mailman-ja) ())

(defvar shimbun-namazu-url "http://www.namazu.org/pipermail/")

(defvar shimbun-namazu-groups
  '("kakasi-commits" "kakasi-dev" "migemo" "namazu-devel-en" "namazu-devel-ja"
    "namazu-users-en" "namazu-users-ja" "namazu-win32-users-ja" "sary"))

(luna-define-method shimbun-index-url ((shimbun shimbun-namazu))
  (shimbun-expand-url
   (concat (shimbun-current-group-internal shimbun) "/")
   (shimbun-url-internal shimbun)))

(luna-define-method shimbun-reply-to ((shimbun shimbun-namazu))
  (concat (shimbun-current-group-internal shimbun)
	  "@namazu.org"))

(provide 'sb-namazu)

;;; sb-namazu.el ends here
