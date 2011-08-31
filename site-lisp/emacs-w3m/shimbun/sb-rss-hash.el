;;; sb-rss-hash.el --- shimbun backend for rss description -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2006, 2009 Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>

;; Author: Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>
;; Keywords: shimbun

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

(require 'sb-rss)
(require 'sb-hash)

(eval-and-compile
  (luna-define-class rss-content-hash (content-hash) ())
  (luna-define-class shimbun-rss-hash (shimbun-rss) (content)))

(defvar shimbun-rss-hash-group-path-alist
  '(;; name rss-url type(opt:html is t) content-start(opt) content-end(opt)
    ))

(luna-define-method content-hash-update-items ((content-hash rss-content-hash)
					       shimbun)
  (with-temp-buffer
    (let ((case-fold-search t))
      (shimbun-retrieve-url
       (content-hash-contents-url content-hash shimbun) 'no-cache 'no-decode)
      ;; In some rss feeds, LFs might be used mixed with CRLFs.
      (shimbun-strip-cr)
      (insert
       (prog1
	   (decode-coding-string (buffer-string) (shimbun-rss-get-encoding))
	 (erase-buffer)
	 (set-buffer-multibyte t)))
      (content-hash-update-items-impl content-hash shimbun))))

(luna-define-method content-hash-update-items-impl
  ((content-hash rss-content-hash) shimbun)
  (let (xml dc-ns rss-ns content-ns
	(buf-str (buffer-string)))
    (with-temp-buffer
      (erase-buffer)
      (set-buffer-multibyte t)
      (insert buf-str)
      ;; parse xml : check url and desc
      (setq xml (condition-case err
		    (xml-parse-region (point-min) (point-max))
		  (error
		   (message "Error while parsing %s: %s"
			    (content-hash-contents-url content-hash shimbun)
			    (error-message-string err))
		   nil)))
      (when xml
	(setq dc-ns (shimbun-rss-get-namespace-prefix
		     xml "http://purl.org/dc/elements/1.1/")
	      content-ns (shimbun-rss-get-namespace-prefix
		     xml "http://purl.org/rss/1.0/modules/content/")
	      rss-ns (shimbun-rss-get-namespace-prefix
		      xml "http://purl.org/rss/1.0/"))
	(dolist (item (shimbun-rss-find-el
		       (intern (concat rss-ns "item")) xml))
	  (let ((url (and (listp item)
			  (eq (intern (concat rss-ns "item")) (car item))
			  (shimbun-rss-node-text rss-ns 'link (cddr item)))))
	    (when url
	      (let* ((date (or (shimbun-rss-get-date shimbun url)
			       (shimbun-rss-node-text dc-ns 'date item)
			       (shimbun-rss-node-text rss-ns 'pubDate item)))
		     (id (shimbun-rss-build-message-id shimbun url date))
		     (content (shimbun-rss-node-text
			       content-ns 'encoded item))
		     (description (shimbun-rss-node-text
				   rss-ns 'description item)))
		(when content
		  (when (string-match (concat (regexp-quote "<![CDATA[")
					      "\\(.*\\)"
					      (regexp-quote "]]>")) content)
		    (setq content (match-string 1 content))))
		;; save contents
		(when (and id (or content description))
		  (content-hash-set-item content-hash id
					 (or content description)))))))))))

(luna-define-method initialize-instance :after ((shimbun shimbun-rss-hash)
						&rest init-args)
  (luna-set-slot-value shimbun 'content
		       (luna-make-entity 'rss-content-hash))
  shimbun)

(luna-define-method shimbun-groups ((shimbun shimbun-rss-hash))
  (mapcar 'car shimbun-rss-hash-group-path-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-rss-hash))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-rss-hash-group-path-alist)))

(luna-define-method shimbun-get-headers :before ((shimbun shimbun-rss-hash)
						 &optional range)
  (content-hash-update-items-impl (luna-slot-value shimbun 'content) shimbun))

(luna-define-method shimbun-make-contents ((shimbun shimbun-rss-hash) header)
    (if (nth 2 (assoc (shimbun-current-group-internal shimbun)
		      shimbun-rss-hash-group-path-alist))
	(shimbun-make-html-contents shimbun header)
      (shimbun-make-text-contents shimbun header)))

(luna-define-method shimbun-clear-contents ((shimbun shimbun-rss-hash) header)
  (let ((start (nth 3 (assoc (shimbun-current-group-internal shimbun)
			     shimbun-rss-hash-group-path-alist)))
	(end (nth 4 (assoc (shimbun-current-group-internal shimbun)
			     shimbun-rss-hash-group-path-alist)))
	(case-fold-search t))
    (goto-char (point-min))
    (when (and (stringp start)
	       (re-search-forward start nil t)
	       (progn
		 (setq start (point))
		 (stringp end))
	       (re-search-forward end nil t))
      (delete-region (match-beginning 0) (point-max))
      (delete-region (point-min) start)
      t)))

(luna-define-method shimbun-article ((shimbun shimbun-rss-hash) header
				     &optional outbuf)
  (content-hash-shimbun-article (luna-slot-value shimbun 'content)
				shimbun header outbuf))

(luna-define-method shimbun-rss-build-message-id ((shimbun
						   shimbun-rss-hash)
						  url &optional date)
  (let* ((group (shimbun-current-group-internal shimbun)))
    (when (string-match "#" url)
      (setq url (substring url 0 (match-beginning 0))))
    (when (stringp date)
      (setq url (concat url date)))
    (concat "<" (md5 (concat url)) "." group "@rss-hash>")))

(provide 'sb-rss-hash)

;;; sb-rss-hash.el ends here
