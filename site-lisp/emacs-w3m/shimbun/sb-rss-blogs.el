;;; sb-rss-blogs.el --- Back end for RSS feeds with unpublished content

;; Copyright (C) 2008, 2010 David Engster

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
(require 'sb-rss)

(luna-define-class shimbun-rss-blogs (shimbun-rss) ())

(defvar shimbun-rss-blogs-group-url-regexp
  '(("Example: Wordpress" "http://emacs.wordpress.com/feed/")
    ("Example: w3m"
     "http://sourceforge.net/export/rss2_projnews.php?group_id=39518"
     "<a name=\"content\">" "<h3 class=\"titlebar\">")
    ("Example: w3m without removal"
     "http://sourceforge.net/export/rss2_projnews.php?group_id=39518"
     'none))
  "Names and corresponding URLs of RSS feeds.
This is an alist containing the names and corresponding URLs for each
RSS feed.  Additionally, you can specify two regular expressions for
the beginning and end of the actual content.  If you just use
the symbol `none' here, no HTML filtering will be done whatsoever.
On the other hand, if you don't give any further parameters besides
the URL, the shimbun will automatically try to deal with the following
blog engines: Google Blogger/Blogspot (including comment feeds),
WordPress, and TypePad.")

;; Content start/end regular expressions for different blog engines
(defconst shimbun-rss-blogs-blogger-content-start-regexp
  "<div class='post-body entry-content'>")
(defconst shimbun-rss-blogs-blogger-content-end-regexp
  "<a name='comments'>")
(defconst shimbun-rss-blogs-wordpress-content-start-regexp
  "\\(<div class=\"post\".*?>\\|<div[^>]*content.*>\\)")
(defconst shimbun-rss-blogs-wordpress-content-end-regexp
  "\\(<!-- You can start editing here. -->\\|<a name=\"comment.*?\".*?>\
\\|<[^>]*?id=[^>]*comment.*?>\\)")
(defconst shimbun-rss-blogs-typepad-content-start-regexp
  "<div class=\"entry-content\">")
(defconst shimbun-rss-blogs-typepad-content-end-regexp
  "\\(<a id=\"comments\">\\|<!-- technorati tags -->\
\\|<!-- post footer links -->\\|<!-- sidebar -->\\)")

(luna-define-method shimbun-groups ((shimbun shimbun-rss-blogs))
  (mapcar 'car shimbun-rss-blogs-group-url-regexp))

(defvar shimbun-rss-blogs-from-address "invalid@nomail.invalid")

(luna-define-method shimbun-index-url ((shimbun shimbun-rss-blogs))
  (let ((group (shimbun-current-group-internal shimbun)))
    (cadr (assoc group shimbun-rss-blogs-group-url-regexp))))

(luna-define-method shimbun-rss-build-message-id ((shimbun
						   shimbun-rss-blogs)
						  url date)
  (let* ((group (shimbun-current-group-internal shimbun)))
    (concat "<" (md5 (concat url)) "." group "@rss-blogs>")))

(luna-define-method shimbun-get-headers :around ((shimbun
						  shimbun-rss-blogs)
						 &optional range)
  (let ((group (shimbun-current-group-internal shimbun))
	(headers (luna-call-next-method))
	(type (sb-rss-blogs-guess-type-from-rss))
	from)
    (cond
     ((eq type 'blogger)
      (dolist (header headers headers)
	(setq from (shimbun-header-from header))
	(setq from (progn (string-match "(\\(.+\\))" from)
			  (match-string 1 from)))
	(shimbun-header-set-from header from)))
     (t
      headers))))

(defun sb-rss-blogs-guess-type-from-rss ()
  "Analyze 'generator' tag in RSS feed for known CMS."
  (save-excursion
    (goto-char (point-min))
    (when (or (re-search-forward
	       "<[ ]*generator[ ]*>\\(.+\\)<[ ]*/generator[ ]*>" nil t)
	      (re-search-forward "generator=[\"']\\(.+?\\)[\"']" nil t))
      (let ((type (match-string 1)))
	(cond
	 ((string-match "blogger" type)
	  'blogger)
	 ((string-match "WordPress" type)
	  'wordpress)
	 ((string-match "TypePad" type)
	  'typepad)
	 (t
	  nil))))))

(defun shimbun-rss-blogs-guess-type-from-html ()
  "Analyze 'generator' tag in HTML page for known CMS."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "<[ \t]*meta.*name=[\"']generator[\"'].*$" nil t)
	(let ((type (match-string 0)))
	  (cond
	   ((string-match "WordPress" type)
	    'wordpress)
	   ((string-match "blogger" type)
	    'blogger)
	   ((string-match "typepad" type)
	    'typepad)
	   (t
	    nil)))
      nil)))

(luna-define-method shimbun-clear-contents :before ((shimbun
						     shimbun-rss-blogs)
						    header)
  (let ((type (shimbun-rss-blogs-guess-type-from-html))
	(url (shimbun-header-xref header))
	(startend
	 (cdr-safe
	  (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-rss-blogs-group-url-regexp))))
	content-start content-end)
    (unless (eq (car-safe startend) 'none)
      (cond
       ;; manual tags
       ((and (= (length startend) 2)
	     (stringp (car startend))
	     (stringp (cadr startend)))
	(setq content-start (car startend)
	      content-end (cadr startend)))
       ;; blogger
       ((eq type 'blogger)
	(if (string-match "showComment=[0-9]+" url)
	    (save-excursion
	      ;; this is a comment
	      (search-forward (match-string 0 url))
	      (re-search-backward "<a name='.+?'>")
	      (setq content-start (match-string 0)
		    content-end "<!-- google_ad_section_end -->"))
	  ;; this is a normal posting
	  (setq content-start shimbun-rss-blogs-blogger-content-start-regexp
		content-end shimbun-rss-blogs-blogger-content-end-regexp)))
       ;; wordpress
       ((eq type 'wordpress)
	(setq content-start shimbun-rss-blogs-wordpress-content-start-regexp
	      content-end shimbun-rss-blogs-wordpress-content-end-regexp))
       ;; typepad
       ((eq type 'typepad)
	(setq content-start shimbun-rss-blogs-typepad-content-start-regexp
	      content-end shimbun-rss-blogs-typepad-content-end-regexp)))
      (when content-start
	(shimbun-remove-tags "<title>" content-start))
      (when content-end
	(shimbun-remove-tags content-end "</html>")))))

(provide 'sb-rss-blogs)

;;; sb-rss-blogs.el ends here
