;;; sb-emacswiki.el --- emacswiki shimbun backend

;; Copyright (C) 2004, 2005 David Hansen

;; Author: David Hansen <david.hansen@physik.fu-berlin.de>
;; Keywords: news

;; This file is a part of shimbun.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-emacswiki (shimbun-rss) ())

(defvar shimbun-emacswiki-url
  "http://www.emacswiki.org/cgi-bin/wiki.pl?action=rss")
(defvar shimbun-emacswiki-groups '("changes" "diff"))
(defvar shimbun-emacswiki-from-address  "invalid@emacswiki.org")
(defvar shimbun-emacswiki-content-start "<h1>")
(defvar shimbun-emacswiki-content-end "<div class=\"footer\">")

(defvar shimbun-emacswiki-x-face-alist
  '(("default" . "X-Face: 'Is?R.u_yTmkkPe(`Zyec$CF<xHX/m-bK|ROSqoD|DDW6;z&\
/T$@b=k:F#n>ri1KJ)/XVXzJ~!dA'H{,F+;f-IaJ$2~S9ZU6U@_\"%*YzLz8kAxsX3(q`>a&zos\
\\9.[2/gpE76Fim]r7o7hz&@@O#d{`BXdD)i]DQBW,Z]#$5YWYNT}@Y{cm}O}ev`l`QAeZI*NN<\
e2ibWOZWTFz8j~/m")))

(luna-define-method shimbun-index-url ((shimbun shimbun-emacswiki))
  shimbun-emacswiki-url)

(luna-define-method shimbun-get-headers :around ((shimbun shimbun-emacswiki)
						 &optional range)
  (static-when (featurep 'xemacs)
    ;; It's one of many bugs in XEmacs that the coding systems *-dos
    ;; provided by Mule-UCS don't convert CRLF to LF when decoding.
    (shimbun-strip-cr))
  (let ((xml (condition-case err
		 (xml-parse-region (point-min) (point-max))
	       (error
		(message "Error while parsing %s: %s"
			 (shimbun-index-url shimbun)
			 (error-message-string err))
		nil)))
	dc-ns rss-ns wiki-ns url headers)
    (when xml
      (setq dc-ns (shimbun-rss-get-namespace-prefix
		   xml "http://purl.org/dc/elements/1.1/")
	    rss-ns (shimbun-rss-get-namespace-prefix
		    xml "http://purl.org/rss/1.0/")
	    wiki-ns (shimbun-rss-get-namespace-prefix
		     xml "http://purl.org/rss/1.0/modules/wiki/"))
      (dolist (item (shimbun-rss-find-el (intern (concat rss-ns "item")) xml)
		    headers)
	(setq url (and (listp item)
		       (eq (intern (concat rss-ns "item")) (car item))
		       (if (string= (shimbun-current-group shimbun) "changes")
			   (shimbun-rss-node-text rss-ns 'link item)
			 (shimbun-rss-node-text wiki-ns 'diff item))))
	(when url
	  (let* ((date (or (shimbun-rss-node-text dc-ns 'date item)
			   (shimbun-rss-node-text rss-ns 'pubDate item)))
		 (id (shimbun-rss-build-message-id shimbun url date)))
	    (unless (shimbun-search-id shimbun id)
	      (push (shimbun-create-header
		     0
		     (let ((desc (shimbun-rss-node-text rss-ns 'description
							item)))
		       (concat (shimbun-rss-node-text rss-ns 'title item)
			       (if desc
				   (concat " - " desc))))
		     (or (shimbun-rss-node-text wiki-ns 'username item)
			 (shimbun-from-address shimbun))
		     (shimbun-rss-process-date shimbun date)
		     id "" 0 0 url)
		    headers))))))))

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-emacswiki) url date)
  (unless (or (string-match "id=\\(.+\\)$" url)
	      (string-match "/\\([^/]+\\)$" url))
    (error "Cannot find message-id base"))
  (concat "<" (match-string 1 url) date "@emacswiki.org>"))

(provide 'sb-emacswiki)

;;; sb-emacswiki.el ends here
