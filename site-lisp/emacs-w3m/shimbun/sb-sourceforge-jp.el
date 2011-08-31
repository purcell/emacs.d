;;; sb-sourceforge-jp.el --- shimbun backend for lists.sourceforge.jp

;; Copyright (C) 2003, 2004, 2005, 2007
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

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
(require 'sb-mailman)

(luna-define-class shimbun-sourceforge-jp (shimbun-mailman-ja) ())

(defcustom shimbun-sourceforge-jp-mailing-lists
  '(("aime-devel")
    ("anthy-dev")
    ("canna-dev")
    ("iiimf-skk-devel-ja" . "iiimf-skk-devel.ja")
    ("iiimf-skk-devel-en" . "iiimf-skk-devel.en")
    ("iiimf-skk-users-ja" . "iiimf-skk-users.ja")
    ("iiimf-skk-users-en" . "iiimf-skk-users.en")
    ("iiimf-skk-cvs-commit" . "iiimf-skk-cvs-commit")
    ("macemacsjp-users")
    ("macemacsjp-english")
    ("ntemacsjp-users"))
  "*List of mailing lists serverd by SourceForge-JP."
  :group 'shimbun
  :type '(repeat
	  (cons
	   :format "%v" :indent 2
	   (string :format "Group Name: %v\n" :size 0)
	   (radio
	    :format "Mailing List Name: %v"
	    (const :format "Same as Group Name " nil)
	    (string :format "%t: %v\n" :size 0)))))

(defconst shimbun-sourceforge-jp-base-url
  "http://lists.sourceforge.jp/pipermail/"
  "Base URL of archives served by SourceForge-JP.")

(defconst shimbun-sourceforge-jp-coding-system 'euc-japan
  "Coding system used for archives of SourceForge-JP.")

(luna-define-method shimbun-groups ((shimbun shimbun-sourceforge-jp))
  (mapcar 'car shimbun-sourceforge-jp-mailing-lists))

(luna-define-method shimbun-index-url ((shimbun shimbun-sourceforge-jp))
  (let ((pair (assoc (shimbun-current-group-internal shimbun)
		     shimbun-sourceforge-jp-mailing-lists)))
    (concat
     (shimbun-expand-url (or (cdr pair) (car pair))
			 shimbun-sourceforge-jp-base-url)
     "/")
    ))

(provide 'sb-sourceforge-jp)

;;; sb-sourceforge-jp.el ends here
