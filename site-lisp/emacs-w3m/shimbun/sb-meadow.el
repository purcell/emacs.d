;;; sb-meadow.el --- shimbun backend for meadow-ml

;; Copyright (C) 2001, 2002, 2003, 2004
;; Akihiro Arisawa <ari@mbf.sphere.ne.jp>
;; Copyright (C) 2001, 2002, 2003, 2004
;; Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Akihiro Arisawa    <ari@mbf.sphere.ne.jp>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>,
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

;; Original code was sb-mew.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org> and
;; Yuuichi Teranishi <teranisi@gohome.org>.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'shimbun)
(require 'sb-mhonarc)
(luna-define-class shimbun-meadow (shimbun-mhonarc) ())

(defvar shimbun-meadow-url "http://www.ysnb.net/meadow/")
(defvar shimbun-meadow-groups '("meadow-develop" "meadow-users-jp"))
(defvar shimbun-meadow-reverse-flag nil)
(defvar shimbun-meadow-litemplate-regexp
  "<STRONG><A NAME=\"\\([0-9]+\\)\" HREF=\"\\(msg[0-9]+.html\\)\">\\([^<]+\\)</a> \\([^<]+\\)</STRONG>")

(luna-define-method shimbun-headers ((shimbun shimbun-meadow)
				     &optional range)
  (with-temp-buffer
    (shimbun-retrieve-url shimbun-meadow-url)
    (let* ((group (shimbun-current-group-internal shimbun))
	   (regexp (format
		    "<a href=\"\\(%s/\\([1-9][0-9][0-9][0-9]\\)/\\)\""
		    (regexp-quote group)))
	   (case-fold-search t)
	   (pages (shimbun-header-index-pages range))
	   (count 0)
	   (indexes) ; This should be `indices' ;-).
	   (headers))
      (while (re-search-forward regexp nil t)
	(push (cons (match-string 2)
		    (shimbun-expand-url (match-string 1) shimbun-meadow-url))
	      indexes))
      (catch 'stop
	(dolist (elem indexes)
	  (delete-region (point-min) (point-max))
	  (shimbun-retrieve-url (cdr elem) t)
	  (goto-char (point-min))
	  (if (re-search-forward
	       "<A[^>]*HREF=\"mail\\([0-9]+\\)\\.html\">\\[?Last Page\\]?</A>"
	       nil t)
	      (let ((aux (string-to-number (match-string 1)))
		    url)
		(while (> aux 0)
		  (setq url (if (= aux 1)
				(cdr elem)
			      (shimbun-expand-url (format "mail%d.html" aux) (cdr elem))))
		  (delete-region (point-min) (point-max))
		  (shimbun-retrieve-url url)
		  (unless (if pages (<= (incf count) pages) t)
		    (throw 'stop headers))
		  (shimbun-mhonarc-get-headers shimbun url headers (car elem))
		  (setq aux (1- aux))))
	    (shimbun-mhonarc-get-headers shimbun (cdr elem)
					 headers (car elem))))
	headers))))

(luna-define-method shimbun-mhonarc-get-subject-value ((shimbun shimbun-meadow))
  (shimbun-decode-entities-string (shimbun-mhonarc-header-value)))

(luna-define-method shimbun-mhonarc-get-from-r13-value ((shimbun shimbun-meadow))
  (shimbun-mhonarc-rot13-decode
   (shimbun-decode-entities-string (shimbun-mhonarc-header-value))))

(provide 'sb-meadow)

;;; sb-meadow.el ends here
