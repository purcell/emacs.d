;;; sb-emacs-w3m.el --- shimbun backend for emacs-w3m ml

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

(eval-when-compile
  (require 'cl))

(require 'shimbun)
(require 'sb-mhonarc)

(luna-define-class shimbun-emacs-w3m (shimbun-mhonarc) ())

(defvar shimbun-emacs-w3m-url "http://emacs-w3m.namazu.org/ml/")

(defvar shimbun-emacs-w3m-groups '("emacs-w3m"))

(defvar shimbun-emacs-w3m-reverse-flag t)
(defvar shimbun-emacs-w3m-litemplate-regexp
  "<Strong><A NAME=\"\\([0-9]+\\)\" HREF=\"\\(msg[0-9]+.html\\)\"> \\([^<]+\\)</a></Strong> <EM>\\([^<]+\\)</EM>")

(luna-define-method shimbun-reply-to ((shimbun shimbun-emacs-w3m))
  "emacs-w3m@namazu.org")

(luna-define-method shimbun-get-headers ((shimbun shimbun-emacs-w3m)
					 &optional range)
  (let ((url (shimbun-index-url shimbun))
	(pages (shimbun-header-index-pages range))
	(count 0)
	headers aux)
    (catch 'stop
      (shimbun-mhonarc-get-headers shimbun url headers)
      (while (and (if pages (< (incf count) pages) t)
		  (re-search-forward
		   "<A \\(REL=\"next\" \\)?href=\"\\(mail[0-9]+.html\\)\">Next Index</A>"
		   nil t)
		  (not (string-equal (match-string 2) aux)))
	(setq aux (match-string 2)
	      url (shimbun-expand-url aux url))
	(erase-buffer)
	(shimbun-retrieve-url url)
	(shimbun-mhonarc-get-headers shimbun url headers))
      headers)))

(provide 'sb-emacs-w3m)

;;; sb-emacs-w3m.el ends here
