;;; sb-f1fan.el --- shimbun backend for www.ksky.ne.jp/~tahara/f1/  -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005
;; MIYOSHI Masanori <miyoshi@meadowy.org>

;; Author: MIYOSHI Masanori <miyoshi@meadowy.org>
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

;; This back end generates text/plain articles unless failing to
;; extract contents.

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-f1fan (shimbun) ())

(defvar shimbun-f1fan-url "http://www.ksky.ne.jp/~tahara/f1/")
(defvar shimbun-f1fan-server-name "F1ファン")
(defvar shimbun-f1fan-groups '("news"))
(defvar shimbun-f1fan-from-address "tahara@ps.ksky.ne.jp")
(defvar shimbun-f1fan-content-start "<blockquote>")
(defvar shimbun-f1fan-content-end  "</blockquote>")
(defvar shimbun-f1fan-coding-system 'shift_jis)

(luna-define-method shimbun-index-url ((shimbun shimbun-f1fan))
  (concat
   (shimbun-url-internal shimbun)
   "News"
   (format-time-string "%Y")
   "/news-new.html"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-f1fan)
					 &optional range)
  (let ((case-fold-search t) headers)
    (goto-char (point-min))
    (while (re-search-forward "<a href=\\.\\(/\\([0-9]+\\)/\\([0-9]+\\)\\.html\\)>\\([^<]+\\)</a><br>" nil t)
      (let ((url (match-string 1))
	    ;;(month (match-string 2))
	    (id (match-string 3))
	    (subject (match-string 4)))
	(setq id (format "<%s.%s.tahara@ps.ksky.ne.jp>"
			id (shimbun-current-group-internal shimbun)))
	(push (shimbun-create-header
	       0
	       subject
	       (shimbun-from-address shimbun)
	       nil
	       id "" 0 0 (concat
			       (shimbun-url-internal shimbun)
			       "News"
			       (format-time-string "%Y")
			       url))
	      headers)))
    headers))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-f1fan)
						    header)
  (when (luna-call-next-method)
    (goto-char (point-min))
    (skip-chars-forward "\r\n")
    (delete-region (point-min) (point))
    t))

(luna-define-method shimbun-make-contents ((shimbun shimbun-f1fan)
					   header)
  (shimbun-header-insert-and-buffer-string
   shimbun header nil
   ;; When cleaning has been succeeded, this article is treated as a
   ;; text/plain message.  Otherwise, it is treated as a text/html
   ;; message.
   (not (shimbun-clear-contents shimbun header))))

(provide 'sb-f1fan)

;;; sb-f1fan.el ends here
