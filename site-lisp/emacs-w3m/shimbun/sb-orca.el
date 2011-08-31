;;; sb-orca.el --- shimbun backend for www.orca.med.or.jp ML archive

;; Copyright (C) 2002, 2003, 2005 Masamichi Goudge M.D. <Matanuki@Goudge.org>

;; Author: Masamichi Goudge M.D. <Matanuki@Goudge.org>
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

(eval-when-compile
  (require 'cl))

(require 'shimbun)
(require 'sb-mhonarc)

(luna-define-class shimbun-orca (shimbun-mhonarc) ())

(defvar shimbun-orca-url "http://ml.orca.med.or.jp/")
(defvar shimbun-orca-groups '("orca-users" "orca-dev" "orca-tech" "orca-announce"))
(defvar shimbun-orca-reverse-flag nil)
(defvar shimbun-orca-litemplate-regexp
  "<LI>[^<]*<STRONG><A NAME=\"\\([0-9]+\\)\" HREF=\"\\(msg[0-9]+.html\\)\">\
\\([^<]+\\)</A></STRONG> <EM>\\([^<]+\\)</EM>\n?</LI>")

(luna-define-method shimbun-index-url ((shimbun shimbun-orca))
  (concat (shimbun-url-internal shimbun)
	  (shimbun-current-group-internal shimbun) "/index.html"))

(luna-define-method shimbun-reply-to ((shimbun shimbun-orca))
  (concat (shimbun-current-group-internal shimbun) "@orca"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-orca)
					 &optional range)
  (let ((url (shimbun-index-url shimbun))
	(pages (shimbun-header-index-pages range))
	(count 0)
	headers)
    (catch 'stop
      (shimbun-mhonarc-get-headers shimbun url headers)
      (goto-char (point-min))
      (while (and (if pages (< (incf count) pages) t)
		  (re-search-forward
		   "<A HREF=\"\\(mail[0-9]+\\.html\\)\">Prev Page</A>"
		   nil t))
	(setq url (shimbun-expand-url (match-string 1) url))
	(erase-buffer)
	(shimbun-retrieve-url url)
	(shimbun-mhonarc-get-headers shimbun url headers)
	(goto-char (point-min)))
      headers)))

(provide 'sb-orca)

;;; sb-orca.el ends here
