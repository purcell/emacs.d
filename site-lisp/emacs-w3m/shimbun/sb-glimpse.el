;;; sb-glimpse.el --- shimbun backend class for Glimpse archive

;; Copyright (C) 2001, 2002, 2003 Akihiro Arisawa <ari@mbf.sphere.ne.jp>

;; Author: Akihiro Arisawa    <ari@mbf.sphere.ne.jp>
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

(eval-when-compile
  (require 'cl))

(require 'shimbun)
(require 'sb-mhonarc)

(luna-define-class shimbun-glimpse (shimbun-mhonarc) ())

(luna-define-method shimbun-index-url ((shimbun shimbun-glimpse))
  (concat (shimbun-url-internal shimbun)
	  (shimbun-current-group-internal shimbun) "/"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-glimpse)
					 &optional range)
  (let ((case-fold-search t)
	(path (if (string-match "http://[^/]+\\(/.*\\)"
				(shimbun-index-url shimbun))
		  (match-string 1 (shimbun-index-url shimbun))
		"/"))
	(pages (shimbun-header-index-pages range))
	(count 0)
	headers auxs)
    (while (and (if pages (<= (incf count) pages) t)
		(re-search-forward
		 (concat "<A HREF=\"" path
			 "\\([12][0-9][0-9][0-9][0-1][0-9]\\)/\">\\[Index\\]")
		 nil t))
      (setq auxs (append auxs (list (match-string 1)))))
    (catch 'stop
      (dolist (aux auxs)
	(let ((url (shimbun-expand-url (concat aux "/")
				       (shimbun-index-url shimbun))))
	  (shimbun-retrieve-url url 'reload)
	  (shimbun-mhonarc-get-headers shimbun url headers aux))))
    headers))

(provide 'sb-glimpse)

;;; sb-glimpse.el ends here
