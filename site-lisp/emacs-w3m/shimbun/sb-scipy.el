;;; sb-scipy.el --- shimbun backend for scipy mailing lists

;; Copyright (C) 2005, 2006 S V N Vishwanathan <vishketan@yahoo.com>

;; Author: S V N Vishwanathan <vishketan@yahoo.com>
;;         Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>
;; Keywords: news, scipy

;; This file is not a part of shimbun.

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

(luna-define-class shimbun-scipy (shimbun-mailman) ())

(defvar shimbun-scipy-url "http://www.scipy.net/pipermail/")
(defvar shimbun-scipy-groups
  '("astropy" "ipython-user" "ipython-dev" "scipy-user" "scipy-dev"
    "scipy-testlog" "scipy-chaco" "scipy-cvs"))

(luna-define-method shimbun-index-url ((shimbun shimbun-scipy))
  (shimbun-expand-url
   (concat
    (shimbun-current-group-internal shimbun)
    "/")
   (shimbun-url-internal shimbun)))

(luna-define-method shimbun-reply-to ((shimbun shimbun-scipy))
  (concat
   (shimbun-current-group-internal shimbun)
   "@scipy.net"))

(provide 'sb-scipy)

;;; sb-scipy.el ends here
