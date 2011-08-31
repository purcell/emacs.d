;;; sb-slashdot.el --- slashdot.org shimbun backend

;; Copyright (C) 2008, 2009 David Engster

;; Author: David Engster <dengste@eml.cc>
;; Keywords: news

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

(luna-define-class shimbun-slashdot (shimbun) ())

(defvar shimbun-slashdot-group-url
  '(("frontpage" "http://slashdot.org")
    ("apple" "http://apple.slashdot.org")
    ("askslashdot" "http://ask.slashdot.org")
    ("books" "http://books.slashdot.org")
    ("developers" "http://developers.slashdot.org")
    ("games" "http://games.slashdot.org")
    ("hardware" "http://hardware.slashdot.org")
    ("interviews" "http://interviews.slashdot.org")
    ("IT" "http://it.slashdot.org")
    ("linux" "http://linux.slashdot.org")
    ("mobile" "http://mobile.slashdot.org")
    ("politics" "http://politics.slashdot.org")
    ("science" "http://science.slashdot.org")
    ("YRO" "http://yro.slashdot.org")))

(defvar shimbun-slashdot-url "http://www.slashdot.org")

(defvar shimbun-slashdot-get-comments t
  "Flag if comments should be retrieved.")

(defvar shimbun-slashdot-comment-threshold 3
  "Threshold for displayed comments.")

(defvar shimbun-slashdot-comment-display "flat"
  "Display type of comments.
Can be 'flat', 'thread', or 'nested'.")

(defvar shimbun-slashdot-regexp-section-id-subject
  "<\\s-*h3\\s-+class=\"story\"[^\0]*?<a [^>]*?href=\"\
/*\\([a-zA-Z]+\\)?\\.?slashdot.org/\\([a-z]+?\\)/\\(.+\\)/\\(.+?\\)\
\"[^>]*class=.datitle.[^>]*>\\(.*?\\)</a>")

(defvar shimbun-slashdot-regexp-author-time
  "Posted[\t \n]+by[^a-zA-Z]*\\(.*\\)[^\0]*?on\\s-+[a-zA-Z]+\\s-+\
\\([a-zA-Z]+\\)\\s-+\\([0-9]+\\).+@\\([0-9]+\\):\\([0-9]+\\)\\(AM\\|PM\\)")

(defvar shimbun-slashdot-groups
  (mapcar 'car shimbun-slashdot-group-url))

(defvar shimbun-slashdot-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAABAAAAAQAQMAAAAlPW0iAAAABlBMVEUAgID////5Zpl0AAA
 AKElEQVQI12P4/58BiP7Zg9CfehD68R+EPgLRcYbHzSB0HIiOM4BVAgB9+xqjH78TVQAAAABJRU5
 ErkJggg==")))

(defvar shimbun-slashdot-retry-fetching 1)

(luna-define-method shimbun-index-url ((shimbun shimbun-slashdot))
  (let ((group (shimbun-current-group-internal shimbun)))
    (cadr (assoc group shimbun-slashdot-group-url))))

(luna-define-method shimbun-get-headers
  ((shimbun shimbun-slashdot) &optional range)
  (shimbun-slashdot-get-headers shimbun))

(defun shimbun-slashdot-get-headers (shimbun)
  (let ((from "Slashdot <invalid@slashdot.org>")
	(allmonths '("january" "february" "march" "april" "may" "june"
		     "july" "august" "september" "october" "november"
		     "december"))
	month day hour minute date ampm id url subject headers section)
    ;; Make article URL
    (while (re-search-forward shimbun-slashdot-regexp-section-id-subject
			      nil t)
      (setq section (match-string 1)
	    id (match-string 3)
	    url (concat "http://" (if section (concat section ".") "")
			"slashdot.org/article.pl?sid=" id
			"&simpledesign=1&lowbandwidth=1")
	    subject (match-string 5))
      (if (null shimbun-slashdot-get-comments)
	  (setq url (concat url "&no_d2=1&threshold=5"))
	(setq url (concat url "&no_d2=1&threshold="
			  (number-to-string shimbun-slashdot-comment-threshold)
			  "&mode=" shimbun-slashdot-comment-display
			  "&commentsort=0&pid=0")))
      ;; Make section prettier
      (when section
	(when (string= section "ask")
	  (setq section "askslashdot"))
	(setq subject (concat
		       (if (< (length section) 4)
			   (upcase section)
			 (capitalize section))
		       ": " subject)))
      (while (string-match "</?[a-zA-Z]+?>" subject)
	(setq subject (replace-match "\"" t t subject)))
      (when (re-search-forward shimbun-slashdot-regexp-author-time
			       nil t)
	(setq from (match-string 1)
	      month (match-string 2)
	      day (match-string 3)
	      hour (match-string 4)
	      minute (match-string 5)
	      ampm (match-string 6))
	(setq month (- 13 (length (member (downcase month) allmonths))))
	;; US->European time conversion
	(cond
	 ((and (string= ampm "PM")
	       (not (string= hour "12")))
	  (setq hour
		(number-to-string (+ (string-to-number hour) 12))))
	 ((and (string= ampm "AM")
	       (string= hour "12"))
	  (setq hour "00")))
	;; remove link from author name if necessary
	(when (string-match ">\\(.*\\)</a>" from)
	  (setq from (match-string 1 from)))
	(while (string-match "/" id)
	  (setq id (replace-match "" t t id)))
	(setq date (shimbun-make-date-string
		    ;; Hey, my first year 2100 bug!
		    (string-to-number (concat "20" (substring id 0 2)))
		    month (string-to-number day)
		    (format "%s:%s" hour minute)
		    ;; Maybe we should derive this from current-time-zone?
		    "+0000"))
	(setq id (concat "<" section id "@slashdot.org>"))
	(unless (shimbun-search-id shimbun id)
	  (push (shimbun-make-header
		 0 (shimbun-mime-encode-string subject)
		 (shimbun-mime-encode-string from)
		 date id "" 0 0 url)
		headers))))
    headers))

(luna-define-method shimbun-clear-contents :around ((shimbun
						     shimbun-slashdot)
						    header)
  (goto-char (point-min))
  (shimbun-remove-tags "<html>" "<div class=\"intro\".*?>")
  (if (null shimbun-slashdot-get-comments)
      (shimbun-remove-tags "<div class=\"commentBox\".*?>" "</html>")
    (re-search-forward "<a name=\"topcomment\">" nil t)
    (insert "\n<br><br>&#012\n")
    (shimbun-remove-tags "<div id=\"footer\">" "</html>")
    (shimbun-remove-tags "<div class=\"commentwrap\"" "<a name=\"topcomment\">")
    ;; convert quote tags to italics
    (goto-char (point-min))
    (while (re-search-forward "\
\\(<[ ]*div[ ]+class=[\"']quote[\"'][ ]*>\\|<[ ]*blockquote[ ]*>\\)"
			      nil t)
      (let ((str (match-string 0)))
	(replace-match "<i>")
	(if (string-match "class" str)
	    (re-search-forward "</div>")
	  (re-search-forward "</blockquote>"))
	(replace-match "</i>")))))

(provide 'sb-slashdot)

;;; sb-slashdot.el ends here
