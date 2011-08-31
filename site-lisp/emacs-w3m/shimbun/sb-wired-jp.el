;;; sb-wired-jp.el --- shimbun backend for Hotwired Japan -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2005, 2006 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
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
(require 'sb-rss)

(luna-define-class shimbun-wired-jp (shimbun-rss shimbun) ())

(defvar shimbun-wired-jp-group-table
  '(("news" "http://hotwired.goo.ne.jp/news/index.rdf" nil
     "<!-- articles -->" "<!-- /articles -->")
    ("business" nil "http://hotwired.goo.ne.jp/news/business/"
     "<!-- articles -->" "<!-- /articles -->")
    ("culture" nil "http://hotwired.goo.ne.jp/news/culture/"
     "<!-- articles -->" "<!-- /articles -->")
    ("technology" nil "http://hotwired.goo.ne.jp/news/technology/"
     "<!-- articles -->" "<!-- /articles -->")
    ("blog.takahashi" "http://blog.goo.ne.jp/hwj-takahashi/index.rdf" nil
     "<div class=\"content3\">" "<!-- COMMENT MODULE -->")
    ("blog.sasaki" "http://blog.goo.ne.jp/hwj-sasaki/index.rdf" nil
     "<div class=\"content3\">" "<!-- COMMENT MODULE -->")
    ("blog.ogura" "http://blog.goo.ne.jp/hwj-ogura/index.rdf" nil
     "<div class=\"content3\">" "<!-- COMMENT MODULE -->")))

(defvar shimbun-wired-jp-server-name "Hotwired Japan")
(defvar shimbun-wired-jp-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAAQBAMAAACigOGCAAAABGdBTUEAALGPC/xhBQAAADB
 QTFRFAAAAAgIBBgcFCgsJDA0MDhANEhYOHDALK0kQOGEUSn8aYaYies4piugulfkymf8zRL4klQA
 AAC90RVh0U29mdHdhcmUAWFYgVmVyc2lvbiAzLjEwYStGTG1hc2sgIFJldjogMTIvMjkvOTQbx6p
 8AAABfUlEQVR42h3MTyiDcRzH8e+ImT3KckBRNmGHPRfPRdpt5sBNe3bgynJRi5Q1jfL/T1moqSm
 i/Nth6jkMI8lh2pJQ6/c8K26PUTju+Un7+T5+h1effu9+P/D5fN6tbnRwQVec9+o3c17A3VucE0W
 x53Mc7c0NoQM5ETwed4hJLsE9wiSP4AmxTZfQtVachf7hsfiL2pkc22GqEBGu2W3nBSqBM7h3FdB
 4NnpyedZO2tNB1ZFrllefwTm5l2lW+N++8+iJjbS8HxUch22aXQV+e18+TNbnbWQ5ZiVtNEsdCTu
 VCPBf+/l00kJqyFLMQmrpIm34qKPZO+C/NzAAKc1HYkAqaOCHk016aLxcl88wlJAD1EhTlJup0L+
 qioYVmwIESCINpJymNO7IqLW+QdXK7lWHhiGZk/GdIhe43/L3oArmCX/8XoUUxFkBfWBP5ozhlT2
 CoRpC7AasMIVaMJ8adHcBj7k4jTawMDryv0NFvx7KjqtR40UTalL0XSnDH6TavYxiVdRgAAAAB3R
 JTUUH1AoWBQgh+MK8xgAAAABJRU5ErkJggg==")))

(luna-define-method shimbun-groups ((shimbun shimbun-wired-jp))
  (mapcar 'car shimbun-wired-jp-group-table))

(luna-define-method shimbun-index-url ((shimbun shimbun-wired-jp))
  (let ((elem (assoc (shimbun-current-group shimbun)
		     shimbun-wired-jp-group-table)))
    (or (nth 1 elem) (nth 2 elem))))

(luna-define-method shimbun-headers :around ((shimbun shimbun-wired-jp)
					     &optional range)
  (if (nth 1 (assoc (shimbun-current-group shimbun)
		    shimbun-wired-jp-group-table))
      (luna-call-next-method)
    (with-temp-buffer
      (shimbun-fetch-url shimbun (shimbun-index-url shimbun) t)
      (shimbun-wired-jp-get-headers shimbun))))

(defun shimbun-wired-jp-get-headers (shimbun)
  (let ((headers)
	(base (shimbun-index-url shimbun))
	(regexp (format "<a href=\"\\(/news/%s/story/[0-9]+\\.html\\)\">"
			(shimbun-current-group shimbun))))
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let ((url (shimbun-expand-url (match-string 1) base)))
	(push (shimbun-create-header
	       0
	       (buffer-substring (point)
				 (if (search-forward "</a>" nil t)
				     (match-beginning 0)
				   (point-at-eol)))
	       (format "%s (%s)"
		       (shimbun-server-name shimbun)
		       (shimbun-current-group shimbun))
	       ""
	       (shimbun-rss-build-message-id shimbun url)
	       "" 0 0 url)
	      headers)))
    headers))

(luna-define-method shimbun-article-url ((shimbun shimbun-wired-jp) header)
  (let ((url (shimbun-article-base-url shimbun header)))
    (if (string-match "/news/[a-z]+/story/\\([0-9]+\\.html\\)\\'" url)
	(shimbun-expand-url (concat "/news/print/" (match-string 1 url)) url)
      url)))

(luna-define-method shimbun-clear-contents ((shimbun shimbun-wired-jp) header)
  (let ((case-fold-search t)
	(start)
	(elem (assoc (shimbun-current-group shimbun)
		     shimbun-wired-jp-group-table)))
    (when (string-match "/news/print/[0-9]+\\.html\\'"
			(shimbun-article-url shimbun header))
      (goto-char (point-min))
      (when (re-search-forward "<font[^>]*>\\([0-9]+\\)年\\([0-9]+\\)月\
\\([0-9]+\\)日 +\\([0-9]+\\):\\([0-9]+\\)\\([ap]m\\) +\\([A-Z]+\\)</font>"
			       nil t)
	(setq start (match-end 0))
	(shimbun-header-set-date header
				 (shimbun-make-date-string
				  (string-to-number (match-string 1))
				  (string-to-number (match-string 2))
				  (string-to-number (match-string 3))
				  (format
				   "%2d:%s"
				   (+ (string-to-number (match-string 4))
				      (if (string= "pm" (match-string 6))
					  12 0))
				   (match-string 5))
				  (cdr (assoc (match-string 7)
					      '(("JT" . "+0900")
						("PT" . "-0700"))))))
	(forward-line -1)
	(when (looking-at "<b><font[^>]+>\\([^<>]+\\)</font></b><br>")
	  (shimbun-header-set-from header (match-string 1)))))
    (goto-char (point-min))
    (when (and (or start
		   (when (re-search-forward (nth 3 elem) nil t)
		     (setq start (point))))
	       (re-search-forward (nth 4 elem) nil t))
      (delete-region (match-beginning 0) (point-max))
      (delete-region (point-min) start)
      t)))

(provide 'sb-wired-jp)

;;; sb-wired-jp.el ends here
