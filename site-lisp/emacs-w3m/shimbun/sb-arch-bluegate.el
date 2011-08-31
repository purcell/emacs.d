;;; sb-arch-bluegate.el --- shimbun backend for arch.bluegate.org

;; Copyright (C) 2005 Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>

;; Author: Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>
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

(luna-define-class shimbun-arch-bluegate (shimbun-mailman-ja) ())

(defvar shimbun-arch-bluegate-url "http://arch.bluegate.org/pipermail/")

(defvar shimbun-arch-bluegate-groups
  '("subversion-jp" "arch-jp" "mailman" "viewarch"))

(luna-define-method shimbun-index-url ((shimbun shimbun-arch-bluegate))
  (shimbun-expand-url
   (concat (shimbun-current-group-internal shimbun) "/")
   (shimbun-url-internal shimbun)))

(luna-define-method shimbun-reply-to ((shimbun shimbun-arch-bluegate))
  (concat (shimbun-current-group-internal shimbun)
	  "@m.bluegate.org"))

(provide 'sb-arch-bluegate)

;;; sb-arch-bluegate.el ends here
