;;; sb-security-memo.el --- shimbun backend for security-memo ML.

;; Copyright (C) 2001, 2003, 2004 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
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

(luna-define-class shimbun-security-memo (shimbun-fml) ())

(defvar shimbun-security-memo-url "http://memo.st.ryukoku.ac.jp/")
(defvar shimbun-security-memo-group-alist
  '(("memo" . "archive")
    ("free-memo" . "free-memo/archive")
    ("social-memo" . "social-memo/archive")))

(defvar shimbun-security-memo-groups
  (mapcar 'car shimbun-security-memo-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-security-memo))
  (concat
   (shimbun-expand-url
    (cdr (assoc (shimbun-current-group-internal shimbun)
		shimbun-security-memo-group-alist))
    (shimbun-url-internal shimbun))
   "/"))

(luna-define-method shimbun-reply-to ((shimbun shimbun-security-memo))
  "Return the mailing list address."
  (concat (shimbun-current-group-internal shimbun)
	  "@memo.st.ryukoku.ac.jp"))

(provide 'sb-security-memo)

;;; sb-security-memo.el ends here
