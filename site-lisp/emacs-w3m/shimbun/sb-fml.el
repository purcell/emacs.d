;;; sb-fml.el --- shimbun backend class for fml archiver.

;; Copyright (C) 2001, 2002, 2003, 2004, 2009, 2010
;; Akihiro Arisawa <ari@mbf.sphere.ne.jp>
;; Copyright (C) 2001, 2002, 2003, 2004, 2009, 2010
;; Yuuichi Teranishi <teranisi@gohome.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Akihiro Arisawa    <ari@mbf.sphere.ne.jp>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>
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

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'shimbun)

(luna-define-class shimbun-fml (shimbun) ())

(defun shimbun-fml-parse-time (str)
  (save-match-data
    (if (string-match
	 "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\) \\([0-9]+:[0-9]+:[0-9]+\\)"
	 str)
	(shimbun-make-date-string (string-to-number (match-string 1 str))
				  (string-to-number (match-string 2 str))
				  (string-to-number (match-string 3 str))
				  (match-string 4 str))
      str)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-fml)
					 &optional range)
  (let ((case-fold-search t)
	(pages (shimbun-header-index-pages range))
	(count 0)
	headers auxs aux)
    (goto-char (point-min))
    (while (and (if pages (<= (incf count) pages) t)
		(re-search-forward "<a href=\"\\([0-9]+\\(\\.week\\|\\.month\\)?\\)/index.html\">" nil t))
      (setq auxs (append auxs (list (match-string 1)))))
    (catch 'stop
      (while auxs
	(with-temp-buffer
	  (shimbun-retrieve-url
	   (concat (shimbun-index-url shimbun) (setq aux (car auxs)) "/")
	   'reload)
	  (let ((case-fold-search t)
		id url date subject from)
	    (goto-char (point-min))
	    (while (re-search-forward
		    "<LI><A HREF=\"\\([0-9]+\\.html\\)\">Article .*</A> <DIV><SPAN CLASS=article>Article <SPAN CLASS=article-value>\\([0-9]+\\)</SPAN></SPAN> at <SPAN CLASS=Date-value>\\([^<]*\\)</SPAN> <SPAN CLASS=Subject>Subject: <SPAN CLASS=Subject-value>\\([^<]*\\)</SPAN></SPAN></DIV><DIV><SPAN CLASS=From>From: <SPAN CLASS=From-value>\\([^<]*\\)</SPAN></SPAN></DIV>"
		    nil t)
	      (setq url (concat (shimbun-index-url shimbun)
				aux "/" (match-string 1))
		    id (format "<%s%05d%%%s>"
			       aux
			       (string-to-number (match-string 2))
			       (shimbun-current-group-internal shimbun))
		    date (shimbun-fml-parse-time (match-string 3))
		    subject (match-string 4)
		    from (match-string 5))
	      (if (shimbun-search-id shimbun id)
		  (throw 'stop nil))
	      (forward-line 1)
	      (push (shimbun-create-header 0 subject from date id "" 0 0 url)
		    headers)))
	  (setq auxs (cdr auxs)))))
    headers))

(luna-define-method shimbun-make-contents ((shimbun shimbun-fml) header)
  (catch 'stop
    (if (search-forward "<SPAN CLASS=mailheaders>" nil t)
	(delete-region (point-min) (point))
      (throw 'stop nil))
    (if (search-forward "</PRE>" nil t)
	(delete-region (point-at-bol) (point-max))
      (throw 'stop nil))
    (if (search-backward "</SPAN>" nil t)
	(delete-region (point-at-bol) (point-at-eol))
      (throw 'stop nil))
    (save-restriction
      (narrow-to-region (point-min) (point))
      (goto-char (point-min))
      (let (field value start value-beg end)
	(while (and (setq start (point))
		    (re-search-forward "<SPAN CLASS=\\(.*\\)>\\(.*\\)</SPAN>:"
				       nil t)
		    (setq field (match-string 2))
		    (re-search-forward
		     (concat "<SPAN CLASS=" (match-string 1) "-value>") nil t)
		    (setq value-beg (point))
		    (search-forward "</SPAN>" nil t)
		    (setq end (point)))
	  (setq value (buffer-substring value-beg
					(progn (search-backward "</SPAN>")
					       (point))))
	  (delete-region start end)
	  (cond ((string= field "Date")
		 (shimbun-header-set-date header value))
		((string= field "From")
		 (shimbun-header-set-from header value))
		((string= field "Subject")
		 (shimbun-header-set-subject header value))
		((string= field "Message-Id")
		 (shimbun-header-set-id header value))
		((string= field "References")
		 (shimbun-header-set-references header value))
		(t
		 (insert (concat field ": "
				 (shimbun-header-normalize value t) "\n")))))
	(goto-char (point-min))
	(shimbun-header-insert shimbun header)
	(insert
	 "Content-Type: text/html; charset=ISO-2022-JP\nMIME-Version: 1.0\n"))
      (goto-char (point-max))))
  (insert "<html><head><base href=\""
	  (shimbun-header-xref header)
	  "\"></head><body><pre>")
  (goto-char (point-max))
  (insert "</pre></body></html>\n")
  (encode-coding-string (buffer-string)
			(mime-charset-to-coding-system "ISO-2022-JP")))

(provide 'sb-fml)

;;; sb-fml.el ends here
