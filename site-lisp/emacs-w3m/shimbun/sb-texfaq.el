;;; sb-texfaq.el --- shimbun backend for TeX Q&A Bullettein Board.

;; Copyright (C) 2002, 2004 Hidetaka Iwai <tyuyu@mb6.seikyou.ne.jp>

;; Author: Hidetaka Iwai <tyuyu@mb6.seikyou.ne.jp>,
;;         TSUCHIYA Masatoshi <tsuchiya@namazu.org>
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

(require 'shimbun)

(luna-define-class shimbun-texfaq (shimbun) ())

(defvar shimbun-texfaq-url "http://oku.edu.mie-u.ac.jp/~okumura/texfaq/qa/")
(defvar shimbun-texfaq-groups '("qanda"))
(defvar shimbun-texfaq-content-start "</h2>\n")
(defvar shimbun-texfaq-content-end  "\n<hr>\n<p>")

(defun shimbun-texfaq-make-id (shimbun string)
  (concat "<"
	  string
	  "."
	  (shimbun-current-group-internal shimbun)
	  "@"
	  (save-match-data
	    (let ((url (shimbun-index-url shimbun)))
	      (if (string-match "\\`[^:/]+://\\([^/]+\\)/" url)
		  (match-string 1 url)
		(error "Cannot extract host name from %s" url))))
	  ">"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-texfaq)
					 &optional range)
  (let ((case-fold-search t) headers)
    (catch 'found
      (goto-char (point-min))
      (while (re-search-forward "[0-9]+: \
\\([0-9][0-9][0-9][0-9]\\)-\\([0-1][0-9]\\)-\\([0-3][0-9]\\) \
\\([0-2][0-9]:[0-5][0-9]:[0-5][0-9]\\) \
<a href=\"\\(\\([0-9]+\\)\\.html\\)\">\\([^<]+\\)</a>\\([^<]+\\)<br>" nil t)
	(let ((date (shimbun-make-date-string
		     (string-to-number (match-string 1))
		     (string-to-number (match-string 2))
		     (string-to-number (match-string 3))
		     (match-string 4)))
	      (subject (match-string 7))
	      (author (match-string 8))
	      (url (shimbun-expand-url (match-string 5)
				       (shimbun-index-url shimbun)))
	      (id (shimbun-texfaq-make-id shimbun (match-string 6))))
	  (when (shimbun-search-id shimbun id)
	    (throw 'found headers))
	  (push (shimbun-create-header 0 subject author date id "" 0 0 url)
		headers)))
      headers)))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-texfaq)
						   header)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
	   "<hr>\n<a href=\"\\([0-9]+\\)\\.html\">&gt;&gt;[0-9]+</a>" nil t)
      (shimbun-header-set-references
       header
       (shimbun-texfaq-make-id shimbun (match-string 1))))))

(provide 'sb-texfaq)

;;; sb-texfaq.el ends here
