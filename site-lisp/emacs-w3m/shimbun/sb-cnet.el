;;; sb-cnet.el --- shimbun backend for CNET

;; Copyright (C) 2004, 2006, 2009 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>,
;;         Katsumi Yamaoka    <yamaoka@jpl.org>
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

;; The original code for CNET Japan was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;; It was rewritten as a part of Shimbun library by
;; Yuuichi Teranishi <teranisi@gohome.org> at November 19th, 2001.

;; All stuffs were rewritten for the original house of CNET by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org> at January 11th, 2004.

;;; Code:

(require 'shimbun)
(require 'sb-rss)
(eval-when-compile
  (require 'cl)
  ;; `multiple-value-bind' requires the 2nd argument to be multiple-value,
  ;; not a list, in particular for XEmacs 21.5.  `values-list' does it,
  ;; but is a run-time cl function in XEmacs 21.4 and Emacs 21.
  (when (eq 'identity (symbol-function 'values-list))
    (define-compiler-macro values-list (arg)
      arg)))

(luna-define-class shimbun-cnet (shimbun-rss shimbun) ())

(defvar shimbun-cnet-group-alist
  '(("news" . "http://news.com.com/2547-1_3-0-20.xml")
    ("enterprise.software" . "http://news.com.com/2547-7343_3-0-10.xml")
    ("enterprise.hardware" . "http://news.com.com/2547-1001_3-0-10.xml")
    ("security" . "http://news.com.com/2547-1009_3-0-10.xml")
    ("networking" . "http://news.com.com/2547-1035_3-0-10.xml")
    ("personal.technology" . "http://news.com.com/2547-1040_3-0-10.xml")
    ("newsmakers" . "http://news.com.com/2547-1082_3-0.xml")
    ("perspectives" . "http://news.com.com/2547-1071_3-0.xml"))
  "Alist of readable groups and URLs of their RSSs.
For more detail, see <URL:http://news.com.com/2009-1090-980549.html>.")

(defvar shimbun-cnet-server-name "CNET")
(defvar shimbun-cnet-from-address  "webmaster@news.com.com")
(defvar shimbun-cnet-x-face-alist
  '(("default" . "X-Face: 0p7.+XId>z%:!$ahe?x%+AEm37Abvn]n\
*GGh+>v=;[3`a{1lqO[$,~3C3xU_ri>[JwJ!9l0\n ~Y`b*eXAQ:*q=bBI\
_=ro*?]4:|n>]ZiLZ2LEo^2nr('C<+`lO~/!R[lH'N'4X&%\\I}8T!wt")))

(luna-define-method shimbun-groups ((shimbun shimbun-cnet))
  (mapcar 'car shimbun-cnet-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-cnet))
  (cdr (assoc (shimbun-current-group shimbun) shimbun-cnet-group-alist)))

(luna-define-method shimbun-rss-process-date ((shimbun shimbun-cnet) string)
  "Convert a slightly corrupted date string to a date string in right format."
  (multiple-value-bind (sec min hour day month year dow dst zone)
      (values-list (decode-time (shimbun-time-parse-string string)))
    (setq zone (/ zone 60))
    (shimbun-make-date-string year month day
			      (format "%02d:%02d" hour min)
			      (format "%s%02d%02d"
				      (if (>= zone 0) "+" "-")
				      (/ zone 60)
				      (% zone 60)))))

(defun shimbun-cnet-extract-body ()
  "Extract a body of an article.
In CNET site, bodies are surrounded by either <div id=\"story\"> or
<div id=\"blogs\">.  This function removes the outside parts of the
body."
  (catch 'found
    (goto-char (point-min))
    (when (re-search-forward "<div id=\"\\(story\\|blogs\\)\">" nil t)
      (let ((start (match-beginning 0))
	    (level 1))
	(while (re-search-forward "<\\(div\\|\\(/div>\\)\\)" nil t)
	  (if (match-beginning 2)
	      (decf level)
	    (incf level))
	  (when (zerop level)
	    (delete-region (point) (point-max))
	    (delete-region (point-min) start)
	    (throw 'found t)))))))

(defun shimbun-cnet-remove-footer ()
  "Remove a footer.
In CNET, articles contain either \"Related quotes\" section or
\"Whitepaper\" section.  This function removes these sections and the
following part."
  (goto-char (point-min))
  (when (re-search-forward "<div id=\"story\\(q\\|wht\\)\"" nil t)
    (delete-region (match-beginning 0) (point-max)))
  (let ((level 0))
    (while (re-search-backward "<\\(div\\|\\(/div>\\)\\)" nil t)
      (if (match-beginning 2)
	  (decf level)
	(incf level)))
    (goto-char (point-max))
    (while (>= (decf level) 0)
      (insert "</div>\n"))))

(defun shimbun-cnet-remove-useless-tags ()
  (shimbun-remove-tags "<script" "</script>")
  (shimbun-remove-tags "<noscript" "</noscript>")
  (shimbun-remove-tags "<a href=\"[^\"]+\\?tag=st_util_print\">" "</a>")
  (shimbun-remove-tags "<a href=\"[^\"]+\\?tag=st_util_email\">" "</a>")
  (shimbun-remove-tags "<a onclick" "</a>")
  (shimbun-remove-tags "<a href=\"javascript" "</a>")
  (shimbun-remove-tags "<newselement type=\"table\">" "</newselement>"))

(luna-define-method shimbun-clear-contents ((shimbun shimbun-cnet) header)
  (shimbun-strip-cr)
  (when (shimbun-cnet-extract-body)
    (shimbun-cnet-remove-useless-tags)
    (shimbun-cnet-remove-footer)
    (goto-char (point-min))
    (when (re-search-forward
	   "<img src=\"[^\"]+/story_related.jpg\"[^>]*/>" nil t)
      (insert "Related stories\n")
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (when (re-search-forward
	   "<a href=\"mailto:\\([^\\?]+\\)\\?subject=" nil t)
      (shimbun-header-set-from header (match-string 1)))
    t))

(provide 'sb-cnet)

;;; sb-cnet.el ends here
