;;; sb-japantimes.el --- shimbun backend for www.japantimes.co.jp

;; Author: Hidetaka Iwai <tyuyu@mb6.seikyou.ne.jp>,
;;         KASUGA Toru <gen@qb3.so-net.ne.jp>

;; Keywords: news

;;; Copyright:

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

(require 'shimbun)

(luna-define-class shimbun-japantimes (shimbun) ())

(defvar shimbun-japantimes-url
  "http://www.japantimes.co.jp/")
(defvar shimbun-japantimes-groups
  '("general" "business"))
(defvar shimbun-japantimes-from-address
  "webmaster@japantimes.co.jp")
(defvar shimbun-japantimes-content-start
  "<div id=\"deck\">\\|<div id=\"mainbody\">")
(defvar shimbun-japantimes-content-end
  "^</div>")

(defvar shimbun-japantimes-group-table
  '(("general" "news.htm" "nn")
    ("business" "news.htm" "nb")))
(defvar shimbun-japantimes-expiration-days 7)

(luna-define-method shimbun-index-url ((shimbun shimbun-japantimes))
  (concat (shimbun-url-internal shimbun)
	  (nth 1 (assoc (shimbun-current-group-internal shimbun)
			shimbun-japantimes-group-table))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-japantimes)
					 &optional range)
  (let ((regexp
	 (format
	  (eval-when-compile
	    (let ((s0 "[\t\n ]*")
		  (s1 "[\t\n ]+"))
	      (concat "<a href=\""
		      ;; 1. url
		      "\\(http://search.japantimes.co.jp/cgi-bin/"
		      ;; 2.
		      "\\(%s\\)"
		      ;; 3. year
		      "\\(20[0-9][0-9]\\)"
		      s0
		      ;; 4. month
		      "\\([0-1][0-9]\\)"
		      s0
		      ;; 5. day
		      "\\([0-3][0-9]\\)"
		      s0
		      ;; 6. tag
		      "\\([a-z][0-9]\\)"
		      s0 "\\.html\\)\">"
		      ;; 7. subject
		      s0 "\\([^[<>]+\\)"
		      s0 "</a>")))
	  (nth 2 (assoc (shimbun-current-group-internal shimbun)
			shimbun-japantimes-group-table))))
	(case-fold-search t)
	url serial year month day tag subject id date headers)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (setq url (match-string 1)
	    serial (match-string 2)
	    year (match-string 3)
	    month (match-string 4)
	    day (match-string 5)
	    tag (match-string 6)
	    subject (match-string 7)
	    id (format "<%s%s%s%s%s%%%s.japantimes.co.jp>"
		       serial
		       year
		       month
		       day
		       tag
		       (shimbun-current-group-internal shimbun))
	    date (shimbun-make-date-string
		  (string-to-number year)
		  (string-to-number month)
		  (string-to-number day)))
      (push (shimbun-make-header
	     0
	     (shimbun-mime-encode-string subject)
	     (shimbun-from-address shimbun)
	     date id "" 0 0 url)
	    headers))
    headers))

(provide 'sb-japantimes)

;;; sb-japantimes.el ends here
