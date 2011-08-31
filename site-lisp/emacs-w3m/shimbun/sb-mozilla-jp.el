;;; sb-moz-users.el --- shimbun backend for moz-users

;; Copyright (C) 2003 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Keywords: news
;; Created: Jun 7, 2003

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
;; You have to put the four lines below to ~/.w3m/passwd;
;;
;;   machine www.mozilla.gr.jp
;;   path /ml/logs/moz-users/
;;   login mozilla
;;   passwd mozilla
;;
;; Note that modes of ~/.w3m/passwd should be 0600 (or 0400) otherwise
;; w3m ignores it.
;;
;;; Code:

(require 'shimbun)
(require 'sb-fml)

(luna-define-class shimbun-mozilla-jp (shimbun-fml) ())

(defvar shimbun-mozilla-jp-url "http://www.mozilla.gr.jp/ml/logs/moz-users/")
(defvar shimbun-mozilla-jp-groups '("users"))

(provide 'sb-mozilla-jp)

;;; sb-mozilla-jp.el ends here
