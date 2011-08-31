;;; sb-mailman.el --- shimbun backend class for mailman archiver -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2002, 2003 NAKAJIMA Mikio <minakaji@namazu.org>
;; Copyright (C) 2002, 2008 Katsumi Yamaoka <yamaoka@jpl.org>
;; Copyright (C) 2005       Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>

;; Authors: NAKAJIMA Mikio  <minakaji@namazu.org>,
;;          Katsumi Yamaoka <yamaoka@jpl.org>,
;;          Tsuyoshi CHO    <tsuyoshi_cho@ybb.ne.jp>
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

;; Mailman is the GNU Mailing List Manager.
;; See http://www.gnu.org/software/mailman/index.html for its detail.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'shimbun)

(luna-define-class shimbun-mailman (shimbun) ())

(defun shimbun-mailman-make-contents (shimbun header)
  (subst-char-in-region (point-min) (point-max) ?\t ?\  t)
  (shimbun-decode-entities)
  (goto-char (point-min))
  (let ((end (search-forward "<!--beginarticle-->")))
    (goto-char (point-min))
    (search-forward "</HEAD>")
    (when (re-search-forward "<H1>\\([^\n]+\\)\\(\n +\\)?</H1>" end t nil)
      (shimbun-header-set-subject
       header
       (shimbun-mime-encode-string (match-string 1))))
    (when (re-search-forward "<B>\\([^\n]+\\)\\(\n +\\)?</B> *\n +\
<A HREF=\"[^\n]+\n +TITLE=\"[^\n]+\">\\([^\n]+\\)"
			     end t nil)
      (shimbun-header-set-from
       header
       (shimbun-mime-encode-string (concat (match-string 1)
					   " <" (match-string 3) ">")))
      (when (re-search-forward "<I>\\([^\n]+\\)</I>" end t nil)
	(shimbun-header-set-date header (match-string 1)))
      (delete-region (point-min) end)
      (delete-region (search-forward "<!--endarticle-->") (point-max))
      (shimbun-header-insert-and-buffer-string shimbun header nil t))))

(luna-define-method shimbun-make-contents ((shimbun shimbun-mailman) header)
  (shimbun-mailman-make-contents shimbun header))

(defun shimbun-mailman-headers (shimbun range)
  (with-temp-buffer
    (let* ((index-url (shimbun-index-url shimbun))
	   (group (shimbun-current-group-internal shimbun))
	   (suffix (if (string-match "^http://\\([^/]+\\)/" index-url)
		       (match-string 1 index-url)
		     index-url))
	   auxs aux id url subject from headers)
      (shimbun-retrieve-url (shimbun-expand-url "index.html" index-url)
			    'reload)
      (setq case-fold-search t)
      (let ((pages (shimbun-header-index-pages range))
	    (count 0))
	(while (and (if pages (<= (incf count) pages) t)
		    (re-search-forward "\
<a href=\"\\(20[0-9][0-9]\\(?:q[1-4]\\)?\
\\|20[0-9][0-9]-\\(?:January\\|February\\|March\\|April\\|May\\|June\
\\|July\\|August\\|September\\|October\\|November\\|December\\)\
\\)/date.html\">"
				       nil t))
	  (push (match-string 1) auxs)))
      (setq auxs (nreverse auxs))
      (catch 'stop
	(while auxs
	  (erase-buffer)
	  (shimbun-retrieve-url (shimbun-expand-url
				 (concat (setq aux (car auxs)) "/date.html")
				 index-url)
				'reload)
	  (subst-char-in-region (point-min) (point-max) ?\t ?\  t)
	  (goto-char (point-max))
	  (while (re-search-backward "<LI><A HREF=\"\\(\\([0-9]+\\)\\.html\\)\
\">\\([^\n]+\\)\n</A><A NAME=\"[0-9]+\">&nbsp;</A>\n *<I>\\([^\n]+\\)\n</I>"
				     nil t)
	    (setq id (format "<%06d.%s@%s>"
			     (string-to-number (match-string 2))
			     group
			     suffix))
	    (when (shimbun-search-id shimbun id)
	      (throw 'stop nil))
	    (setq url (shimbun-expand-url (concat aux "/" (match-string 1))
					  index-url)
		  subject (match-string 3)
		  from (match-string 4))
	    (setq subject (with-temp-buffer
			    (insert subject)
			    (shimbun-remove-markup)
			    (shimbun-decode-entities)
			    (buffer-string)))
	    (push (shimbun-make-header
		   0 (shimbun-mime-encode-string subject)
		   (shimbun-mime-encode-string from)
		   "" id "" 0 0 url)
		  headers))
	  (setq auxs (cdr auxs))))
      headers)))

(luna-define-method shimbun-headers ((shimbun shimbun-mailman) &optional range)
  (shimbun-mailman-headers shimbun range))


;;; Derived class for mailing list archives written in Japanese

(luna-define-class shimbun-mailman-ja (shimbun-mailman) ())

(defun shimbun-mailman-ja-make-contents (shimbun header)
  (subst-char-in-region (point-min) (point-max) ?\t ?\  t)
  (shimbun-decode-entities)
  (goto-char (point-min))
  (let ((end (search-forward "<!--beginarticle-->"))
	name address date)
    (goto-char (point-min))
    (search-forward "</HEAD>")
    (when (re-search-forward "<H1>\\([^\n]+\\)\\(\n +\\)?</H1>" end t nil)
      (shimbun-header-set-subject
       header
       (shimbun-mime-encode-string (match-string 1))))
    (when (re-search-forward "<B>\\([^\n]+\\)\\(\n +\\)?</B> *\n +\
<A HREF=\"[^\n]+\n +TITLE=\"[^\n]+\">\\([^\n]+\\)"
			     end t nil)
      (setq name (match-string 1)
	    address (match-string 3))
      ;; Yoshiki.Ohshima ＠ acm.org
      (when (string-match " \\(＠\\|at\\) " name)
	(setq name (concat (substring name 0 (match-beginning 0))
			   "@"
			   (substring name (match-end 0)))))
      (when (string-match " \\(＠\\|at\\) " address)
	(setq address (concat (substring address 0 (match-beginning 0))
			      "@"
			      (substring address (match-end 0)))))
      (shimbun-header-set-from
       header
       (shimbun-mime-encode-string (concat name " <" address ">")))

      (when (re-search-forward "<I>\\([0-9][0-9][0-9][0-9]\\)年\
 *\\([0-9][0-9]*\\)月\
 *\\([0-9][0-9]*\\)日\
 (\\(月\\|火\\|水\\|木\\|金\\|土\\|日\\))\
 \\([:0-9]+\\)\
 \\([A-Z]+\\)</I>"
			       end t nil)
	;; <I>Sat, 12 Apr 2003 17:29:51 +0900 (JST)</I> ;; mailman original
	;; <I>2003年 4月 11日 (金) 02:43:25 CEST</I> ;; squeak-ja
	(setq date (shimbun-make-date-string
		    (string-to-number (match-string-no-properties 1))
		    (string-to-number (match-string-no-properties 2))
		    (string-to-number (match-string-no-properties 3))
		    (match-string-no-properties 5)
		    (match-string-no-properties 6)))
	(shimbun-header-set-date header date))
      (delete-region (point-min) end)
      (delete-region (search-forward "<!--endarticle-->") (point-max))
      (shimbun-header-insert-and-buffer-string shimbun header nil t))))

(luna-define-method shimbun-make-contents ((shimbun shimbun-mailman-ja) header)
  (shimbun-mailman-ja-make-contents shimbun header))

(provide 'sb-mailman)

;;; sb-mailman.el ends here
