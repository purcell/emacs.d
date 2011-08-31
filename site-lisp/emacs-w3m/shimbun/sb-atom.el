;;; sb-atom.el --- shimbun backend for ATOM (Rich Site Summary).

;; Copyright (C) 2006, 2008, 2009, 2010 Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>

;; Author: Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>
;; Keywords: news
;; Created: Jun 14, 2003

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
  (require 'cl)
  (require 'static))

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-atom (shimbun-rss) ())

(luna-define-generic shimbun-atom-build-message-id (shimbun-atom url date)
  "Build unique message-id from URL and DATE and return it.
If return nil, it mean argument URL are not SHIMBUN entry.
Basically, implement illeagal URL to generate error message.
But clarify need ignored URL return nil.")

(luna-define-method shimbun-rss-build-message-id ((shimbun shimbun-atom) url date)
  (shimbun-atom-build-message-id shimbun url date))

(luna-define-method shimbun-get-headers ((shimbun shimbun-atom)
					 &optional range)
  (shimbun-atom-get-headers shimbun range t))

(defun shimbun-atom-get-headers (shimbun &optional range
					 need-summaries need-all-entries)
  "Get headers from atom feed described by SHIMBUN.
RANGE is currently ignored.  If NEED-SUMMARIES, include node text
as summary.  By default, only existing and new items from the
feed are returned, i.e., those items which are newer than the
oldest one in the shimbun.  If NEED-ALL-ENTRIES is non-nil, all
items from the feed are returned.  If the entries from the feed
have date information, the result is sorted by ascending date."
  (let* ((xml (condition-case err
		  (xml-parse-region (point-min) (point-max))
		(error
		 (message "Error while parsing %s: %s"
			  (shimbun-index-url shimbun)
			  (error-message-string err))
		 nil)))
	 headers header newheaders oldheaders oldest)
    (dolist (tmp (shimbun-atom-get-headers-1 xml shimbun need-summaries))
      (let* ((date (shimbun-header-date tmp))
	     (ftime
	      (when (and (stringp date)
			 (> (length date) 1))
		(w3m-float-time (date-to-time date)))))
	(push (list tmp ftime) headers)))
    (when headers
      (if (or need-all-entries
	      ;; If there's a header without date information, we
	      ;; return everything, just to be safe.
	      (memq nil (mapcar 'cadr headers)))
	  (mapcar 'car headers)
	;; Otherwise, sort according to date.
	(setq headers
	      (sort headers (lambda (a b)
			      (> (cadr a) (cadr b)))))
	(while headers
	  (setq header (pop headers))
	  (if (shimbun-search-id shimbun (shimbun-header-id (car header)))
	      (push header oldheaders)
	    (push header newheaders)))
	(if (null oldheaders)
	    ;; All items are new
	    (mapcar 'car newheaders)
	  ;; Delete all items which are older than the ones we already
	  ;; have
	  (setq oldest (cadr (car oldheaders)))
	  (while (and newheaders
		      (> oldest (cadr (car newheaders))))
	    (setq newheaders (cdr newheaders)))
	  (append
	   (mapcar 'car newheaders)
	   (mapcar 'car oldheaders)))))))

(defun shimbun-atom-get-headers-1 (xml shimbun need-summaries)
  "Retrieve all items found in XML for SHIMBUN and return headers.
If NEED-SUMMARIES, include node text as summary."
  (when xml
    (let* ((atom-ns (shimbun-rss-get-namespace-prefix
		     xml "http://www.w3.org/2005/Atom"))
	   (dc-ns (shimbun-rss-get-namespace-prefix
		   xml "http://purl.org/dc/elements/1.1/"))
	   (author-node (shimbun-rss-find-el
			 (intern (concat atom-ns "author")) xml))
	   (fn `(lambda (item) (shimbun-rss-node-text ,atom-ns 'name item)))
	   (author (when (consp author-node)
		     (mapconcat fn author-node ",")))
	   url headers)
      (dolist (entry (shimbun-rss-find-el
		      (intern (concat atom-ns "entry")) xml))
	(setq url
	      (catch 'url
		(dolist (link (shimbun-rss-find-el
			       (intern (concat atom-ns "link")) entry))
		  (when (string= (shimbun-atom-attribute-value
				  (intern (concat atom-ns "rel")) link)
				 "alternate")
		    (throw 'url (shimbun-atom-attribute-value
				 (intern (concat atom-ns "href")) link))))))
	(unless url
	  (setq url (shimbun-atom-attribute-value
		     (intern (concat atom-ns "href"))
		     (car (shimbun-rss-find-el
			   (intern (concat atom-ns "link")) entry)))))
	(when url
	  (let* ((date (or (shimbun-rss-get-date shimbun url)
			   (shimbun-rss-node-text atom-ns 'updated entry)
			   (shimbun-rss-node-text atom-ns 'published entry)
			   (shimbun-rss-node-text atom-ns 'modified entry)
			   (shimbun-rss-node-text atom-ns 'created entry)
			   (shimbun-rss-node-text atom-ns 'issued entry)
			   (shimbun-rss-node-text dc-ns 'date entry)))
		 (author-node (shimbun-rss-find-el
			       (intern (concat atom-ns "author")) entry))
		 (author (or (and (consp author-node)
				  (mapconcat fn author-node ","))
			     (shimbun-rss-node-text dc-ns 'creator entry)
			     (shimbun-rss-node-text dc-ns 'contributor entry)
			     author))
		 (id (shimbun-rss-build-message-id shimbun url date)))
	    (when id
	      (push (shimbun-create-header
		     0
		     (or (shimbun-rss-node-text atom-ns 'title entry)
			 (shimbun-rss-node-text dc-ns 'subject entry))
		     (or author (shimbun-from-address shimbun))
		     (shimbun-rss-process-date shimbun date)
		     id "" 0 0 url
		     (when need-summaries
		       (let ((summary (shimbun-rss-node-text
				       atom-ns 'summary entry)))
			 (when summary
			   (list (cons 'summary summary))))))
		    headers)))))
      headers)))

(defun shimbun-atom-attribute-value (attribute node)
  (let* ((attr-list (if (and node (listp node))
			(nth 1 node)
		      nil)))
    (when attr-list
      (catch 'value
	(dolist (attr attr-list)
	  (when (eq (car attr) attribute)
	    (throw 'value (cdr attr))))))))

(provide 'sb-atom)

;; end of sb-atom.el
