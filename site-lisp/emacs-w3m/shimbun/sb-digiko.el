;;; sb-digiko.el --- shimbun backend for digiko-ML.

;; Copyright (C) 2001, 2002, 2003, 2004, 2007
;; Akihiro Arisawa <ari@mbf.sphere.ne.jp>

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

(luna-define-class shimbun-digiko (shimbun-mhonarc) ())

(defvar shimbun-digiko-url "http://yar-3.net/digiko/")
(defvar shimbun-digiko-groups '("digiko"))
(defvar shimbun-digiko-reverse-flag t)
(defvar shimbun-digiko-litemplate-regexp
  "<STRONG><a name=\"\\([0-9]+\\)\" href=\"\\(msg[0-9]+.html\\)\">\\([^<]+\\)</a></STRONG>\n<UL><LI><EM>From</EM>: \\(.+\\) \\(\\(Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\), [ 0-9]+ [A-Z][a-z][a-z] [0-9]+ [0-2][0-9]:[0-5][0-9]:[0-5][0-9] .*\\)</LI>")

(defmacro shimbun-digiko-get-headers (shimbun url headers)
  `(let ((case-fold-search t))
     (goto-char (point-min))
     (while (re-search-forward
	     (shimbun-mhonarc-litemplate-regexp-internal ,shimbun)
	     nil t)
       (let ((id (format "<%s%%%s>" (match-string 1)
			 (shimbun-current-group-internal ,shimbun)))
	     (xref (shimbun-expand-url (match-string 2) ,url))
	     (subject (shimbun-mhonarc-replace-newline-to-space
		       (match-string 3)))
	     (from (shimbun-mhonarc-replace-newline-to-space
		    (match-string 4)))
	     (date (match-string 5)))
	 (if (shimbun-search-id ,shimbun id)
	     (throw 'stop ,headers)
	   (push (shimbun-make-header 0
				      (shimbun-mime-encode-string subject)
				      (shimbun-mime-encode-string from)
				      date id "" 0 0 xref)
		 ,headers))))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-digiko)
					 &optional range)
  (let ((url (shimbun-index-url shimbun))
	(pages (shimbun-header-index-pages range))
	(count 0)
	headers aux)
    (catch 'stop
      (shimbun-digiko-get-headers shimbun url headers)
      (goto-char (point-min))
      (while (and (if pages (< (incf count) pages) t)
		  (re-search-forward
		   "<a href=\"\\(mail[0-9]+.html\\)\">Next Page</a>"
		   nil t)
		  (not (string-equal (match-string 1) aux)))
	(setq aux (match-string 1)
	      url (shimbun-expand-url aux url))
	(erase-buffer)
	(shimbun-retrieve-url url)
	(shimbun-digiko-get-headers shimbun url headers)
	(goto-char (point-min))))
    headers))

(provide 'sb-digiko)

;;; sb-digiko.el ends here
