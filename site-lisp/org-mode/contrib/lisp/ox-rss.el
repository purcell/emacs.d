;;; ox-rss.el --- RSS 2.0 Back-End for Org Export Engine

;; Copyright (C) 2013, 2014  Bastien Guerry

;; Author: Bastien Guerry <bzg@gnu.org>
;; Keywords: org, wp, blog, feed, rss

;; This file is not yet part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a RSS 2.0 back-end for Org exporter, based on
;; the `html' back-end.
;;
;; It requires Emacs 24.1 at least.
;;
;; It provides two commands for export, depending on the desired output:
;; `org-rss-export-as-rss' (temporary buffer) and `org-rss-export-to-rss'
;; (as a ".xml" file).
;;
;; This backend understands two new option keywords:
;;
;; #+RSS_EXTENSION: xml
;; #+RSS_IMAGE_URL: http://myblog.org/mypicture.jpg
;;
;; It uses #+HTML_LINK_HOME: to set the base url of the feed.
;;
;; Exporting an Org file to RSS modifies each top-level entry by adding a
;; PUBDATE property.  If `org-rss-use-entry-url-as-guid', it will also add
;; an ID property, later used as the guid for the feed's item.
;;
;; You typically want to use it within a publishing project like this:
;;
;; (add-to-list
;;  'org-publish-project-alist
;;  '("homepage_rss"
;;    :base-directory "~/myhomepage/"
;;    :base-extension "org"
;;    :rss-image-url "http://lumiere.ens.fr/~guerry/images/faces/15.png"
;;    :html-link-home "http://lumiere.ens.fr/~guerry/"
;;    :html-link-use-abs-url t
;;    :rss-extension "xml"
;;    :publishing-directory "/home/guerry/public_html/"
;;    :publishing-function (org-rss-publish-to-rss)
;;    :section-numbers nil
;;    :exclude ".*"            ;; To exclude all files...
;;    :include ("index.org")   ;; ... except index.org.
;;    :table-of-contents nil))
;;
;; ... then rsync /home/guerry/public_html/ with your server.
;;
;; By default, the permalink for a blog entry points to the headline.
;; You can specify a different one by using the :RSS_PERMALINK:
;; property within an entry.

;;; Code:

(require 'ox-html)
(declare-function url-encode-url "url-util" (url))

;;; Variables and options

(defgroup org-export-rss nil
  "Options specific to RSS export back-end."
  :tag "Org RSS"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))

(defcustom org-rss-image-url "http://orgmode.org/img/org-mode-unicorn-logo.png"
  "The URL of the an image for the RSS feed."
  :group 'org-export-rss
  :type 'string)

(defcustom org-rss-extension "xml"
  "File extension for the RSS 2.0 feed."
  :group 'org-export-rss
  :type 'string)

(defcustom org-rss-categories 'from-tags
  "Where to extract items category information from.
The default is to extract categories from the tags of the
headlines.  When set to another value, extract the category
from the :CATEGORY: property of the entry."
  :group 'org-export-rss
  :type '(choice
	  (const :tag "From tags" from-tags)
	  (const :tag "From the category property" from-category)))

(defcustom org-rss-use-entry-url-as-guid t
  "Use the URL for the <guid> metatag?
When nil, Org will create ids using `org-icalendar-create-uid'."
  :group 'org-export-rss
  :type 'boolean)

;;; Define backend

(org-export-define-derived-backend 'rss 'html
  :menu-entry
  '(?r "Export to RSS"
       ((?R "As RSS buffer"
	    (lambda (a s v b) (org-rss-export-as-rss a s v)))
	(?r "As RSS file" (lambda (a s v b) (org-rss-export-to-rss a s v)))
	(?o "As RSS file and open"
	    (lambda (a s v b)
	      (if a (org-rss-export-to-rss t s v)
		(org-open-file (org-rss-export-to-rss nil s v)))))))
  :options-alist
  '((:with-toc nil nil nil) ;; Never include HTML's toc
    (:rss-extension "RSS_EXTENSION" nil org-rss-extension)
    (:rss-image-url "RSS_IMAGE_URL" nil org-rss-image-url)
    (:rss-categories nil nil org-rss-categories))
  :filters-alist '((:filter-final-output . org-rss-final-function))
  :translate-alist '((headline . org-rss-headline)
		     (comment . (lambda (&rest args) ""))
		     (comment-block . (lambda (&rest args) ""))
		     (timestamp . (lambda (&rest args) ""))
		     (plain-text . org-rss-plain-text)
		     (section . org-rss-section)
		     (template . org-rss-template)))

;;; Export functions

;;;###autoload
(defun org-rss-export-as-rss (&optional async subtreep visible-only)
  "Export current buffer to a RSS buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org RSS Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (let ((file (buffer-file-name (buffer-base-buffer))))
    (org-icalendar-create-uid file 'warn-user)
    (org-rss-add-pubdate-property))
  (org-export-to-buffer 'rss "*Org RSS Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-rss-export-to-rss (&optional async subtreep visible-only)
  "Export current buffer to a RSS file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((file (buffer-file-name (buffer-base-buffer))))
    (org-icalendar-create-uid file 'warn-user)
    (org-rss-add-pubdate-property))
  (let ((outfile (org-export-output-file-name
		  (concat "." org-rss-extension) subtreep)))
    (org-export-to-file 'rss outfile async subtreep visible-only)))

;;;###autoload
(defun org-rss-publish-to-rss (plist filename pub-dir)
  "Publish an org file to RSS.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (let ((bf (get-file-buffer filename)))
    (if bf
	(with-current-buffer bf
	  (org-rss-add-pubdate-property)
	  (write-file filename))
      (find-file filename)
      (org-rss-add-pubdate-property)
      (write-file filename) (kill-buffer)))
  (org-publish-org-to
   'rss filename (concat "." org-rss-extension) plist pub-dir))

;;; Main transcoding functions

(defun org-rss-headline (headline contents info)
  "Transcode HEADLINE element into RSS format.
CONTENTS is the headline contents.  INFO is a plist used as a
communication channel."
  (unless (or (org-element-property :footnote-section-p headline)
	      ;; Only consider first-level headlines
	      (> (org-export-get-relative-level headline info) 1))
    (let* ((htmlext (plist-get info :html-extension))
	   (hl-number (org-export-get-headline-number headline info))
	   (hl-home (file-name-as-directory (plist-get info :html-link-home)))
	   (hl-pdir (plist-get info :publishing-directory))
	   (hl-perm (org-element-property :RSS_PERMALINK headline))
	   (anchor
	    (org-export-solidify-link-text
	     (or (org-element-property :CUSTOM_ID headline)
		 (concat "sec-" (mapconcat 'number-to-string hl-number "-")))))
	   (category (org-rss-plain-text
		      (or (org-element-property :CATEGORY headline) "") info))
	   (pubdate
	    (let ((system-time-locale "C"))
	      (format-time-string
	       "%a, %d %b %Y %H:%M:%S %z"
	       (org-time-string-to-time
		(or (org-element-property :PUBDATE headline)
		    (error "Missing PUBDATE property"))))))
	   (title (replace-regexp-in-string
		   org-bracket-link-regexp
		   (lambda (m) (or (match-string 3 m)
				   (match-string 1 m)))
		   (org-element-property :raw-value headline)))
	   (publink
	    (or (and hl-perm (concat (or hl-home hl-pdir) hl-perm))
		(concat
		 (or hl-home hl-pdir)
		 (file-name-nondirectory
		  (file-name-sans-extension
		   (plist-get info :input-file))) "." htmlext "#" anchor)))
	   (guid (if org-rss-use-entry-url-as-guid
		     publink
		   (org-rss-plain-text
		    (or (org-element-property :ID headline)
			(org-element-property :CUSTOM_ID headline)
			publink)
		    info))))
      (format
       (concat
	"<item>\n"
	"<title>%s</title>\n"
	"<link>%s</link>\n"
	"<guid isPermaLink=\"false\">%s</guid>\n"
	"<pubDate>%s</pubDate>\n"
	(org-rss-build-categories headline info) "\n"
	"<description><![CDATA[%s]]></description>\n"
	"</item>\n")
       title publink guid pubdate contents))))

(defun org-rss-build-categories (headline info)
  "Build categories for the RSS item."
  (if (eq (plist-get info :rss-categories) 'from-tags)
      (mapconcat
       (lambda (c) (format "<category><![CDATA[%s]]></category>" c))
       (org-element-property :tags headline)
       "\n")
    (let ((c (org-element-property :CATEGORY headline)))
      (format "<category><![CDATA[%s]]></category>" c))))

(defun org-rss-template (contents info)
  "Return complete document string after RSS conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  (concat
   (format "<?xml version=\"1.0\" encoding=\"%s\"?>"
	   (symbol-name org-html-coding-system))
   "\n<rss version=\"2.0\"
	xmlns:content=\"http://purl.org/rss/1.0/modules/content/\"
	xmlns:wfw=\"http://wellformedweb.org/CommentAPI/\"
	xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
	xmlns:atom=\"http://www.w3.org/2005/Atom\"
	xmlns:sy=\"http://purl.org/rss/1.0/modules/syndication/\"
	xmlns:slash=\"http://purl.org/rss/1.0/modules/slash/\"
	xmlns:georss=\"http://www.georss.org/georss\"
        xmlns:geo=\"http://www.w3.org/2003/01/geo/wgs84_pos#\"
        xmlns:media=\"http://search.yahoo.com/mrss/\">"
   "<channel>"
   (org-rss-build-channel-info info) "\n"
   contents
   "</channel>\n"
   "</rss>"))

(defun org-rss-build-channel-info (info)
  "Build the RSS channel information."
  (let* ((system-time-locale "C")
	 (title (plist-get info :title))
	 (email (org-export-data (plist-get info :email) info))
	 (author (and (plist-get info :with-author)
		      (let ((auth (plist-get info :author)))
			(and auth (org-export-data auth info)))))
	 (date (format-time-string "%a, %d %b %Y %H:%M:%S %z")) ;; RFC 882
	 (description (org-export-data (plist-get info :description) info))
	 (lang (plist-get info :language))
	 (keywords (plist-get info :keywords))
	 (rssext (plist-get info :rss-extension))
	 (blogurl (or (plist-get info :html-link-home)
		      (plist-get info :publishing-directory)))
	 (image (url-encode-url (plist-get info :rss-image-url)))
	 (ifile (plist-get info :input-file))
	 (publink
	  (concat (file-name-as-directory blogurl)
		  (file-name-nondirectory
		   (file-name-sans-extension ifile))
		  "." rssext)))
    (format
     "\n<title>%s</title>
<atom:link href=\"%s\" rel=\"self\" type=\"application/rss+xml\" />
<link>%s</link>
<description><![CDATA[%s]]></description>
<language>%s</language>
<pubDate>%s</pubDate>
<lastBuildDate>%s</lastBuildDate>
<generator>%s</generator>
<webMaster>%s (%s)</webMaster>
<image>
<url>%s</url>
<title>%s</title>
<link>%s</link>
</image>
"
     title publink blogurl description lang date date
     (concat (format "Emacs %d.%d"
		     emacs-major-version
		     emacs-minor-version)
	     " Org-mode " (org-version))
     email author image title blogurl)))

(defun org-rss-section (section contents info)
  "Transcode SECTION element into RSS format.
CONTENTS is the section contents.  INFO is a plist used as
a communication channel."
  contents)

(defun org-rss-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to RSS.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-html-encode-plain-text
   (org-timestamp-translate timestamp)))

(defun org-rss-plain-text (contents info)
  "Convert plain text into RSS encoded text."
  (let (output)
    (setq output (org-html-encode-plain-text contents)
	  output (org-export-activate-smart-quotes
		  output :html info))))

;;; Filters

(defun org-rss-final-function (contents backend info)
  "Prettify the RSS output."
  (with-temp-buffer
    (xml-mode)
    (insert contents)
    (indent-region (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))

;;; Miscellaneous

(defun org-rss-add-pubdate-property ()
  "Set the PUBDATE property for top-level headlines."
  (let (msg)
    (org-map-entries
     (lambda ()
       (let* ((entry (org-element-at-point))
	      (level (org-element-property :level entry)))
	 (when (= level 1)
	   (unless (org-entry-get (point) "PUBDATE")
	     (setq msg t)
	     (org-set-property
	      "PUBDATE" (format-time-string
			 (cdr org-time-stamp-formats)))))))
     nil nil 'comment 'archive)
    (when msg
      (message "Property PUBDATE added to top-level entries in %s"
	       (buffer-file-name))
      (sit-for 2))))

(provide 'ox-rss)

;;; ox-rss.el ends here
