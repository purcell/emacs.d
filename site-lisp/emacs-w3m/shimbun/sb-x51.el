;;; sb-x51.el --- shimbun backend for x51.org -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2004, 2005, 2006 Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>

;; Author: Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>
;; Keywords: news blog
;; Created: Feb 21, 2004

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

(eval-when-compile (require 'cl))
(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-x51 (shimbun-rss) ())

(defvar shimbun-x51-group-alist
  '(("top"        . "http://x51.org/index.rdf") ;; Top-RDF
    ("anima"      . "http://anima.x51.org/index.rdf")
    ("enema"      . "http://enema.x51.org/index.rdf")
    ("art"        . "http://x51.org/x/art/")
    ("blow"       . "http://x51.org/x/blow/")
    ("crime"      . "http://x51.org/x/crime/")
    ("disaster"   . "http://x51.org/x/disaster/")
    ("edge"       . "http://x51.org/x/edge/")
    ("ghost"      . "http://x51.org/x/ghost/")
    ("info"       . "http://x51.org/x/info/")
    ("life"       . "http://x51.org/x/life/")
    ("love"       . "http://x51.org/x/love/")
    ("media"      . "http://x51.org/x/media/")
    ("medical"    . "http://x51.org/x/medical/")
    ("oparts"     . "http://x51.org/x/oparts/")
    ("phallic"    . "http://x51.org/x/phallic/")
    ("psychic"    . "http://x51.org/x/psychic/")
    ("religion"   . "http://x51.org/x/religion/")
    ("science"    . "http://x51.org/x/science/")
    ("ufo"        . "http://x51.org/x/ufo/")
    ("uma"        . "http://x51.org/x/uma/")
    ("xfiles"     . "http://x51.org/x/xfiles/")))

(defvar shimbun-x51-obsolete-groups
  '("auction" "cabal" "homme" "military" "news" "northkorea" "story")
  "Obsolete group names.")

(defvar shimbun-x51-server-name "x51.org")
(defvar shimbun-x51-from-address "webmaster@x51.org")
(defvar shimbun-x51-auther "X51")
(defvar shimbun-x51-coding-system 'utf-8)
(defvar shimbun-x51-content-start "<!-- Article -->\\|<div class=\"blogbody\">")
(defvar shimbun-x51-content-end "<!---/ Article --->\\|<div class=\"comments-body\">")

;; X-Face create from banner
(defvar shimbun-x51-x-face-alist
  '(("default" ."X-Face: \"15Ng%Hp0)P[AP!e1^W3KhKMVHC*AcXANx^CaW]\
!dzRSN]tO68A5{`1RzK`g+0Yo$0q2RFM\n 7m?9-o[R6ou-[9X$JI1HYc>A-a[+DGgI")))

(luna-define-method shimbun-groups ((shimbun shimbun-x51))
  (append
   shimbun-x51-obsolete-groups
   (mapcar 'car shimbun-x51-group-alist)))

(defmacro shimbun-x51-concat-url (shimbun url)
  `(concat (cdr (assoc (shimbun-current-group-internal ,shimbun)
		       shimbun-x51-group-alist))
	   ,url))

(luna-define-method shimbun-index-url ((shimbun shimbun-x51))
  (shimbun-x51-concat-url shimbun ""))

(luna-define-method shimbun-get-headers :around ((shimbun shimbun-x51)
						 &optional range)
  (let* ((case-fold-search t)
	 (url (shimbun-index-url shimbun))
	 headers)
    (cond
     ((member (shimbun-current-group-internal shimbun)
	      shimbun-x51-obsolete-groups)
      (setq headers '()))
     ((member (shimbun-current-group-internal shimbun)
	      '("top" "anima" "enema"))
      (setq headers (luna-call-next-method)))	;; call parent method
     (t
      (let* ((pages (shimbun-header-index-pages range))
	     (beg (point-min))
	     (end (point-max))
	     indexes)
	(push (concat url "index.php?page=1") indexes) ;; push page 1
	(when (if pages (< 1 pages) t)
	  (goto-char (point-min))
	  (when (search-forward "<div class=\"middlebar\">" nil t)
	    (setq beg (point)))
	  (when (search-forward "</div>" nil t)
	    (setq end (point)))
	  (save-excursion
	    (narrow-to-region beg end)
	    (goto-char (point-min))
	    (let* ((count 2))
	      (while (and (if pages (<= count pages) t)
			  (re-search-forward
			   (format "<a href=\"[^?]*\\?page=%d\"" count)
			   nil t))
		;; push linked for page 2-end
		(push (format "%s%s%d" url "?page=" count) indexes)
		(incf count)))
	    (widen)))
	(setq indexes (nreverse indexes))
	(catch 'stop
	  (dolist (index indexes)
	    (erase-buffer)
	    (shimbun-retrieve-url index t) ;; retrieve target page
	    (goto-char (point-min))
	    (setq beg (point-min)
		  end (point-max))
	    (when (re-search-forward "<!-- *top article *-->" nil t)
	      (setq beg (match-end 0)))
	    (when (re-search-forward "<!--/ *middle bar *-->" nil t)
	      (setq end (match-beginning 0)))
	    (save-excursion
	      (narrow-to-region beg end)
	      ;; get header source
	      (goto-char (point-min))
	      (let (title url date id)
		(while (re-search-forward
			"<a +href=\"\\(http://x51\\.org/x/\
\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\.php\\)\"\
 +class=\"title[^\"]*\">\\([^<]*\\)</a>"
			nil t)
		  (setq url (match-string-no-properties 1)
			title (match-string-no-properties 6)
			date (shimbun-make-date-string
			      (string-to-number (match-string 2))
			      (string-to-number (match-string 3))
			      (string-to-number (match-string 4))))
		  (setq id (shimbun-rss-build-message-id shimbun url date))
		  ;; check old id
		  (when (shimbun-search-id shimbun id)
		    (throw 'stop nil))
		  ;; create header & push header
		  (push (shimbun-create-header
			 0
			 title
			 (shimbun-from-address shimbun)
			 date
			 id "" 0 0 url)
			headers))
		(widen))))))
      headers))))

;; normalize date
(defun shimbun-x51-prepare-article (shimbun header)
  "Adjust a date header if there's a correct information available."
  (let* ((case-fold-search t)
	 (start (re-search-forward (shimbun-content-start shimbun) nil t))
	 (end (and start
		   (re-search-forward (shimbun-content-end shimbun) nil t)
		   (prog1
		       (match-beginning 0)
		     (goto-char start)))))
    ;; Posted by : X51 | 2004&#24180;05&#26376;22&#26085; 23:15 年月日
    (when (re-search-forward
	   "Posted by[^|]*\|\
 ?\\([0-9]*\\)\\(&#24180;\\|年\\)\
\\([0-9]*\\)\\(&#26376;\\|月\\)\
\\([0-9]*\\)\\(&#26085;\\|日\\)\
 ?\\([012][0-9]:[0-5][0-9]\\)"
	   end t)
      (shimbun-header-set-date
       header
       (shimbun-make-date-string
	(string-to-number (match-string 1))
	(string-to-number (match-string 3))
	(string-to-number (match-string 5))
	(match-string 7)))
      (goto-char (point-min)))))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-x51)
						   header)
  (shimbun-x51-prepare-article shimbun header))

(luna-define-method shimbun-clear-contents :before ((shimbun shimbun-x51)
						    header)
  (shimbun-strip-cr)
  (shimbun-remove-tags "<script" "</script>")
  (shimbun-remove-tags "<noscript" "</noscript>")
  (shimbun-remove-tags "<div class=\"notes\"" "</div>")
  (shimbun-remove-tags "<div class=\"line\"" "</div>")
  (shimbun-remove-tags "<div class=\"middlebar\"" "</div>"))

(provide 'sb-x51)

;;; sb-x51.el ends here
