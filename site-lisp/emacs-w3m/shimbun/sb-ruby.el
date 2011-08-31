;;; sb-ruby.el --- shimbun backend class for ruby ML archiver.

;; Copyright (C) 2001, 2002, 2003, 2005, 2009
;; NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
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

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'shimbun)

(luna-define-class shimbun-ruby (shimbun) ())

(defvar shimbun-ruby-url "http://www.ruby-talk.org/")
(defconst shimbun-ruby-group-path-alist
  '(("comp.lang.ruby" . "ruby/comp.lang.ruby")
    ("fj.comp.lang.ruby" . "ruby/fj.comp.lang.ruby")
    ("ruby-dev" . "ruby/ruby-dev")
    ("ruby-ext" . "ruby/ruby-ext")
    ("ruby-list" . "ruby/ruby-list")
    ("ruby-math" . "ruby/ruby-math")
    ("ruby-talk" . "ruby/ruby-talk")))

(defvar shimbun-ruby-groups (mapcar 'car shimbun-ruby-group-path-alist))

;;(luna-define-method shimbun-reply-to ((shimbun shimbun-ruby))
;;
;;)

(defun shimbun-ruby-parse-time (str)
  (save-match-data
    (if (string-match
	 "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\) \\([0-9]+:[0-9]+:[0-9]+\\)"
	 str)
	(shimbun-make-date-string (string-to-number (match-string 1 str))
				  (string-to-number (match-string 2 str))
				  (string-to-number (match-string 3 str))
				  (match-string 4 str))
      str)))

(luna-define-method shimbun-index-url ((shimbun shimbun-ruby))
  (shimbun-expand-url
   "index.shtml"
   (concat (shimbun-url-internal shimbun)
	   (cdr (assoc (shimbun-current-group-internal shimbun)
		       shimbun-ruby-group-path-alist))
	   "/")))

(luna-define-method shimbun-get-headers ((shimbun shimbun-ruby)
					 &optional range)
  (let ((case-fold-search t)
	(pages (shimbun-header-index-pages range))
	(count 0)
	headers auxs aux)
    ;; Use entire archive.
    (while (and (if pages (<= (incf count) pages) t)
		(re-search-forward "<a href=\"\\([0-9]+-[0-9]+.shtml\\)\">" nil t))
      (setq auxs (append auxs (list (match-string 1)))))
    (setq auxs (nreverse auxs))
    (catch 'stop
      (while auxs
	(with-temp-buffer
	  (shimbun-retrieve-url
	   (shimbun-expand-url
	    (setq aux (car auxs))
	    (shimbun-index-url shimbun)))
	  (subst-char-in-region (point-min) (point-max) ?\t ?  t)
	  (let ((case-fold-search t)
		id url date subject from)
	    (goto-char (point-max))
	    (while (re-search-backward
		    "^<DT><A NAME=\"[0-9]+\">\\(</A>\\)?\
<A HREF=\"\\([^>]+\\)\">\\([0-9]+\\)</A> \\([ /:0-9]+\\) \\[\\([^[]+\\)\\][ !]\\(.+\\)$"
		    nil t)
	      (setq url (concat shimbun-ruby-url (match-string 2))
		    id (format "<%s%05d%%%s>"
			       aux
			       (string-to-number (match-string 3))
			       (shimbun-current-group-internal shimbun))
		    date (shimbun-ruby-parse-time (match-string 4))
		    from (match-string 5)
		    subject (match-string 6))
	      (if (shimbun-search-id shimbun id)
		  (throw 'stop nil))
	      (push (shimbun-create-header
		     0 subject from date id "" 0 0 url)
		    headers)))
	  (setq auxs (cdr auxs)))))
    headers))

(luna-define-method shimbun-make-contents ((shimbun shimbun-ruby) header)
  (let ((headers '(("^Subject: \\(.+\\)$" . shimbun-header-set-subject)
		   ("^From:\\(.+\\)$" . shimbun-header-set-from)
		   ("^Date: \\(.+\\)$" . shimbun-header-set-date)))
	;; any other headers to be included?
	;;<A NAME=head></A><pre><A HREF="/cgi-bin/scat.rb/ruby/ruby-list/29726">...<a href="/ruby/ruby-list/29727">o</a> <a href="/cgi-bin/scat.rb/ruby/ruby-list/29727?help">HELP</a>
	;;Subject: [ruby-list:<FONT COLOR="#{CLR_NUM}">29727</FONT>] <b><FONT COLOR="#{CLR_SUB}">Re: rand(1&lt;&lt;32)</FONT></b>
	;;From: <strong>Takahiro Kambe </strong>&lt;taca@sky.yamashina.kyoto.jp&gt;
	;;Date: Mon, 21 May 2001 12:23:06 +0900
	;;In-reply-to: <a href="/cgi-bin/scat.rb/ruby/ruby-list/29724">29724</a>
	;;References: <a href="/cgi-bin/scat.rb/ruby/ruby-list/29720">29720</a> <a href="/cgi-bin/scat.rb/ruby/ruby-list/29724">29724</a>
	;;<hr>
	headerstart value)
    (if (not (re-search-forward "^Subject:" nil t nil))
	nil
      (setq headerstart (progn (beginning-of-line) (point-marker)))
      (delete-region headerstart (point-min)))
    (if (re-search-forward "^<hr><a name=tail>" nil t nil)
	(delete-region (progn (beginning-of-line) (point)) (point-max)))
    (while headers
      (goto-char (point-min))
      (if (re-search-forward (car (car headers)) nil t nil)
	  (and
	   (setq value (match-string-no-properties 1))
	   (setq value (with-temp-buffer (insert value)
					 (shimbun-remove-markup)
					 (shimbun-decode-entities)
					 (buffer-string)))
	   (funcall (cdr (car headers)) header value)))
      (setq headers (cdr headers)))
    (search-forward "<hr>" nil t nil)
    (delete-region (point) headerstart)
    (shimbun-header-insert shimbun header)
    (insert
     "Content-Type: text/html; charset=ISO-2022-JP\nMIME-Version: 1.0\n")
    (insert "\n<html><head><base href=\""
	    (shimbun-header-xref header)
	    "\"</head><body><pre>")
    (goto-char (point-max))
    (insert "</pre></body></html>")
    (encode-coding-string (buffer-string)
			  (mime-charset-to-coding-system "ISO-2022-JP"))))

(provide 'sb-ruby)

;;; sb-ruby.el ends here
