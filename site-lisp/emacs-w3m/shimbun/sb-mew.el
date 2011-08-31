;;; sb-mew.el --- shimbun backend for mew.org

;; Copyright (C) 2001, 2002, 2005 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 2001, 2002, 2005 Akihiro Arisawa   <ari@mbf.sphere.ne.jp>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Akihiro Arisawa    <ari@mbf.sphere.ne.jp>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>
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

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'shimbun)
(require 'sb-mhonarc)
(luna-define-class shimbun-mew (shimbun-mhonarc) ())

(defvar shimbun-mew-url "http://www.mew.org/ml/")

(defconst shimbun-mew-group-url-alist
  '(("mew-dist" . "mew-dist-3.1")
    ("mew-dist-3.1" . "mew-dist-3.1")
    ("mew-dist-2.0" . "mew-dist-2.0")
    ("mew-dist-1.94" . "mew-dist-1.94")
    ("mew-dist-old" . "mew-dist-1.94")
    ("mew-win32" . "mew-win32-3.1")
    ("mew-win32-3.1" . "mew-win32-3.1")
    ("mew-win32-2.0" . "mew-win32-2.0")
    ("mew-win32-0" . "mew-win32-0")
    ("mew-win32-old" . "mew-win32-0")
    ("mew-int" . "mew-int-3.1")
    ("mew-int-3.1" . "mew-int-3.1")
    ("mew-int-2.0" . "mew-int-2.0")
    ("mew-int-0" . "mew-int-0")
    ("mew-int-old" . "mew-int-0")
    ("mgp-users" . "mgp-users")
    ("mgp-users-jp" . "mgp-users-jp")))

(defvar shimbun-mew-groups (mapcar 'car shimbun-mew-group-url-alist))
(defvar shimbun-mew-reverse-flag t)
(defvar shimbun-mew-litemplate-regexp
  "<STRONG><a name=\"\\([0-9]+\\)\" href=\"\\(msg[0-9]+.html\\)\">\\([^<]+\\)</a></STRONG>&nbsp;\\([^<]+\\)</LI>")

(luna-define-method shimbun-index-url ((shimbun shimbun-mew))
  (concat
   (shimbun-url-internal shimbun) "/"
   (cdr (assoc (shimbun-current-group-internal shimbun)
	       shimbun-mew-group-url-alist)) "/"))

(luna-define-method shimbun-reply-to ((shimbun shimbun-mew))
  (concat (shimbun-current-group-internal shimbun)
	  "@mew.org"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-mew)
					 &optional range)
  (let ((url (shimbun-index-url shimbun))
	(pages (shimbun-header-index-pages range))
	(count 0)
	headers aux)
    (catch 'stop
      (shimbun-mhonarc-get-headers shimbun url headers)
      (while (and (if pages (< (incf count) pages) t)
		  (re-search-forward
		   "<A HREF=\"\\(mail[0-9]+.html\\)\">Prev Page</A>"
		   nil t)
		  (not (string-equal (match-string 1) aux)))
	(setq aux (match-string 1)
	      url (shimbun-expand-url aux url))
	(erase-buffer)
	(shimbun-retrieve-url url)
	(shimbun-mhonarc-get-headers shimbun url headers))
      headers)))

(provide 'sb-mew)

;;; sb-mew.el ends here
