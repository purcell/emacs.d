;;; sb-atom-hash.el --- shimbun backend for atom content -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2006, 2007, 2008, 2009 Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>

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

(require 'sb-atom)
(require 'sb-hash)

(eval-and-compile
  (luna-define-class atom-content-hash (content-hash) ())
  (luna-define-class shimbun-atom-hash (shimbun-atom) (content)))

(defvar shimbun-atom-hash-group-path-alist
  '(;; name rss-url type(opt:html is t) content-start(opt) content-end(opt)
    ))

(luna-define-method content-hash-update-items ((content-hash atom-content-hash)
					       shimbun)
  (with-temp-buffer
    (let ((case-fold-search t))
      (shimbun-retrieve-url
       (content-hash-contents-url content-hash shimbun) 'no-cache 'no-decode)
      ;; In some atom feeds, LFs might be used mixed with CRLFs.
      (shimbun-strip-cr)
      (insert
       (prog1
	   (decode-coding-string (buffer-string) (shimbun-rss-get-encoding))
	 (erase-buffer)
	 (set-buffer-multibyte t)))
      (content-hash-update-items-impl content-hash shimbun))))

(luna-define-method content-hash-update-items-impl
  ((content-hash atom-content-hash) shimbun)
  (let (xml dc-ns atom-ns content-ns
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
	      atom-ns (shimbun-rss-get-namespace-prefix
		       xml "http://www.w3.org/2005/Atom"))
	(dolist (entry (shimbun-rss-find-el
			(intern (concat atom-ns "entry")) xml))
	  (let ((url
		 (catch 'url
		   (dolist (link (shimbun-rss-find-el
				  (intern (concat atom-ns "link")) entry))
		     (when (string= (shimbun-atom-attribute-value
				     (intern (concat atom-ns "rel")) link)
				    "alternate")
		       (throw 'url (shimbun-atom-attribute-value
				    (intern (concat atom-ns "href")) link)))))))
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
		     (id (shimbun-atom-build-message-id shimbun url date))
		     content)
		;; save contents
		(let ((contentsym 'content)
		      type mode)
		  (dolist (content-node (or (shimbun-rss-find-el
					     (intern (concat atom-ns "content"))
					     entry)
					    (progn
					      (setq contentsym 'summary)
					      (shimbun-rss-find-el
					       (intern (concat atom-ns "summary"))
					       entry))))
		    (setq type (or (shimbun-atom-attribute-value
				    (intern (concat atom-ns "type"))
				    content-node)
				   "text/plain")
			  mode (or (shimbun-atom-attribute-value
				    (intern (concat atom-ns "mode"))
				    content-node)
				   ""))
		    (cond
		     ((string-match "xhtml" type)
		      ;; xhtml (type text/xhtml,application/xhtml+xml)
		      (setq content (shimbun-atom-rebuild-node
				     atom-ns contentsym entry)))
		     (t
		      ;; text or html(without xhtml)
		      (if (string= "escaped" mode)
			  ;; escaped CDATA
			  (setq content (shimbun-rss-node-text
					 atom-ns contentsym entry))
			;; non-escaped, but  "<>& to &xxx;
			(let ((text (shimbun-rss-node-text
				     atom-ns contentsym entry)))
			  (when text
			    (setq content (shimbun-decode-entities-string
					   text)))))))))
		(when (and id content)
		  (content-hash-set-item content-hash id content))))))))))

(luna-define-method initialize-instance :after ((shimbun shimbun-atom-hash)
						&rest init-args)
  (luna-set-slot-value shimbun 'content
		       (luna-make-entity 'atom-content-hash))
  shimbun)

(luna-define-method shimbun-groups ((shimbun shimbun-atom-hash))
  (mapcar 'car shimbun-atom-hash-group-path-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-atom-hash))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-atom-hash-group-path-alist)))

(luna-define-method shimbun-atom-build-message-id ((shimbun shimbun-atom-hash)
						   url date)
  (concat "<" (md5 url) "%" (shimbun-current-group-internal shimbun) ">"))

(luna-define-method shimbun-get-headers :before ((shimbun shimbun-atom-hash)
						 &optional range)
  (content-hash-update-items-impl (luna-slot-value shimbun 'content) shimbun))

(luna-define-method shimbun-make-contents ((shimbun shimbun-atom-hash) header)
  (if (nth 2 (assoc (shimbun-current-group-internal shimbun)
		    shimbun-atom-hash-group-path-alist))
      (shimbun-make-html-contents shimbun header)
    (shimbun-make-text-contents shimbun header)))

(luna-define-method shimbun-clear-contents ((shimbun shimbun-atom-hash) header)
  (let ((start (nth 3 (assoc (shimbun-current-group-internal shimbun)
			     shimbun-atom-hash-group-path-alist)))
	(end (nth 4 (assoc (shimbun-current-group-internal shimbun)
			   shimbun-atom-hash-group-path-alist)))
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

(luna-define-method shimbun-article ((shimbun shimbun-atom-hash) header
				     &optional outbuf)
  (content-hash-shimbun-article (luna-slot-value shimbun 'content)
				shimbun header outbuf))

(defun shimbun-atom-rebuild-node (namespace local-name element)
  (let* ((node (assq (intern (concat namespace (symbol-name local-name)))
		     element))
	 (text (shimbun-atom-compose-tag node))
	 (cleaned-text (if text (shimbun-replace-in-string
				 text "^[ \000-\037\177]+\\|[ \000-\037\177]+$"
				 ""))))
    (if (string-equal "" cleaned-text)
	nil
      cleaned-text)))

(defun shimbun-atom-compose-tag (node)
  (cond
   ((null node)
    "")
   ((listp node)
    (let ((tag (car node))
	  (attributes (car (cdr node)))
	  (children (cdr (cdr node))))
      (concat "<"
	      (symbol-name tag)
	      (if attributes
		  (concat " "
			  (mapconcat (lambda (attr)
				       (concat (symbol-name (car attr))
					       "=\"" (cdr attr) "\""))
				     attributes " "))
		"")
	      (if children
		  (concat ">" (mapconcat 'shimbun-atom-compose-tag
					 children "")
			  "</" (symbol-name tag) ">")
		" />"))))
   ((stringp node)
    node)))

(provide 'sb-atom-hash)

;;; sb-atom-hash.el ends here
