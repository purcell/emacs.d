;;; ox-org.el --- Org Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2017 Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou@gmail.com>
;; Keywords: org, wp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ox)
(declare-function htmlize-buffer "ext:htmlize" (&optional buffer))
(defvar htmlize-output-type)

(defgroup org-export-org nil
  "Options for exporting Org mode files to Org."
  :tag "Org Export Org"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))

(defcustom org-org-htmlized-css-url nil
  "URL pointing to the CSS defining colors for htmlized Emacs buffers.
Normally when creating an htmlized version of an Org buffer,
htmlize will create the CSS to define the font colors.  However,
this does not work when converting in batch mode, and it also can
look bad if different people with different fontification setup
work on the same website.  When this variable is non-nil,
creating an htmlized version of an Org buffer using
`org-org-export-as-org' will include a link to this URL if the
setting of `org-html-htmlize-output-type' is `css'."
  :group 'org-export-org
  :type '(choice
	  (const :tag "Don't include external stylesheet link" nil)
	  (string :tag "URL or local href")))

(org-export-define-backend 'org
  '((babel-call . org-org-identity)
    (bold . org-org-identity)
    (center-block . org-org-identity)
    (clock . org-org-identity)
    (code . org-org-identity)
    (diary-sexp . org-org-identity)
    (drawer . org-org-identity)
    (dynamic-block . org-org-identity)
    (entity . org-org-identity)
    (example-block . org-org-identity)
    (export-block . org-org-export-block)
    (fixed-width . org-org-identity)
    (footnote-definition . ignore)
    (footnote-reference . org-org-identity)
    (headline . org-org-headline)
    (horizontal-rule . org-org-identity)
    (inline-babel-call . org-org-identity)
    (inline-src-block . org-org-identity)
    (inlinetask . org-org-identity)
    (italic . org-org-identity)
    (item . org-org-identity)
    (keyword . org-org-keyword)
    (latex-environment . org-org-identity)
    (latex-fragment . org-org-identity)
    (line-break . org-org-identity)
    (link . org-org-link)
    (node-property . org-org-identity)
    (template . org-org-template)
    (paragraph . org-org-identity)
    (plain-list . org-org-identity)
    (planning . org-org-identity)
    (property-drawer . org-org-identity)
    (quote-block . org-org-identity)
    (radio-target . org-org-identity)
    (section . org-org-section)
    (special-block . org-org-identity)
    (src-block . org-org-identity)
    (statistics-cookie . org-org-identity)
    (strike-through . org-org-identity)
    (subscript . org-org-identity)
    (superscript . org-org-identity)
    (table . org-org-identity)
    (table-cell . org-org-identity)
    (table-row . org-org-identity)
    (target . org-org-identity)
    (timestamp . org-org-identity)
    (underline . org-org-identity)
    (verbatim . org-org-identity)
    (verse-block . org-org-identity))
  :menu-entry
  '(?O "Export to Org"
       ((?O "As Org buffer" org-org-export-as-org)
	(?o "As Org file" org-org-export-to-org)
	(?v "As Org file and open"
	    (lambda (a s v b)
	      (if a (org-org-export-to-org t s v b)
		(org-open-file (org-org-export-to-org nil s v b)))))))
  :filters-alist '((:filter-parse-tree . org-org--add-missing-sections)))

(defun org-org--add-missing-sections (tree _backend _info)
  "Ensure each headline has an associated section.

TREE is the parse tree being exported.

Footnotes relative to the headline are inserted in the section,
using `org-org-section'.  However, this function is not called if
the headline doesn't contain any section in the first place, so
we make sure it is always called."
  (org-element-map tree 'headline
    (lambda (h)
      (let ((first-child (car (org-element-contents h)))
	    (new-section (org-element-create 'section)))
	(pcase (org-element-type first-child)
	  (`section nil)
	  (`nil (org-element-adopt-elements h new-section))
	  (_ (org-element-insert-before new-section first-child))))))
  tree)

(defun org-org-export-block (export-block _contents _info)
  "Transcode a EXPORT-BLOCK element from Org to LaTeX.
CONTENTS and INFO are ignored."
  (and (equal (org-element-property :type export-block) "ORG")
       (org-element-property :value export-block)))

(defun org-org-identity (blob contents _info)
  "Transcode BLOB element or object back into Org syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored."
  (let ((case-fold-search t))
    (replace-regexp-in-string
     "^[ \t]*#\\+ATTR_[-_A-Za-z0-9]+:\\(?: .*\\)?\n" ""
     (org-export-expand blob contents t))))

(defun org-org-headline (headline contents info)
  "Transcode HEADLINE element back into Org syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored."
  (unless (org-element-property :footnote-section-p headline)
    (unless (plist-get info :with-todo-keywords)
      (org-element-put-property headline :todo-keyword nil))
    (unless (plist-get info :with-tags)
      (org-element-put-property headline :tags nil))
    (unless (plist-get info :with-priority)
      (org-element-put-property headline :priority nil))
    (org-element-put-property headline :level
			      (org-export-get-relative-level headline info))
    (org-element-headline-interpreter headline contents)))

(defun org-org-keyword (keyword _contents _info)
  "Transcode KEYWORD element back into Org syntax.
CONTENTS is nil.  INFO is ignored."
  (let ((key (org-element-property :key keyword)))
    (unless (member key
		    '("AUTHOR" "CREATOR" "DATE" "EMAIL" "OPTIONS" "TITLE"))
      (org-element-keyword-interpreter keyword nil))))

(defun org-org-link (link contents _info)
  "Transcode LINK object back into Org syntax.
CONTENTS is the description of the link, as a string, or nil.
INFO is a plist containing current export state."
  (or (org-export-custom-protocol-maybe link contents 'org)
      (org-element-link-interpreter link contents)))

(defun org-org-template (contents info)
  "Return Org document template with document keywords.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  (concat
   (and (plist-get info :time-stamp-file)
	(format-time-string "# Created %Y-%m-%d %a %H:%M\n"))
   (org-element-normalize-string
    (mapconcat #'identity
	       (org-element-map (plist-get info :parse-tree) 'keyword
		 (lambda (k)
		   (and (string-equal (org-element-property :key k) "OPTIONS")
			(concat "#+OPTIONS: "
				(org-element-property :value k)))))
	       "\n"))
   (and (plist-get info :with-title)
	(format "#+TITLE: %s\n" (org-export-data (plist-get info :title) info)))
   (and (plist-get info :with-date)
	(let ((date (org-export-data (org-export-get-date info) info)))
	  (and (org-string-nw-p date)
	       (format "#+DATE: %s\n" date))))
   (and (plist-get info :with-author)
	(let ((author (org-export-data (plist-get info :author) info)))
	  (and (org-string-nw-p author)
	       (format "#+AUTHOR: %s\n" author))))
   (and (plist-get info :with-email)
	(let ((email (org-export-data (plist-get info :email) info)))
	  (and (org-string-nw-p email)
	       (format "#+EMAIL: %s\n" email))))
   (and (plist-get info :with-creator)
	(org-string-nw-p (plist-get info :creator))
	(format "#+CREATOR: %s\n" (plist-get info :creator)))
   contents))

(defun org-org-section (section contents info)
  "Transcode SECTION element back into Org syntax.
CONTENTS is the contents of the section.  INFO is a plist used as
a communication channel."
  (concat
   (org-element-normalize-string contents)
   ;; Insert footnote definitions appearing for the first time in this
   ;; section, or in the relative headline title.  Indeed, some of
   ;; them may not be available to narrowing so we make sure all of
   ;; them are included in the result.
   (let ((footnotes
	  (org-element-map
	      (list (org-export-get-parent-headline section) section)
	      'footnote-reference
	    (lambda (fn)
	      (and (eq (org-element-property :type fn) 'standard)
		   (org-export-footnote-first-reference-p fn info)
		   (org-element-normalize-string
		    (format "[fn:%s] %s"
			    (org-element-property :label fn)
			    (org-export-data
			     (org-export-get-footnote-definition fn info)
			     info)))))
	    info nil 'headline t)))
     (and footnotes (concat "\n" (mapconcat #'identity footnotes "\n"))))))

;;;###autoload
(defun org-org-export-as-org
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an Org buffer.

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

When optional argument BODY-ONLY is non-nil, strip document
keywords from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org ORG Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'org "*Org ORG Export*"
    async subtreep visible-only body-only ext-plist (lambda () (org-mode))))

;;;###autoload
(defun org-org-export-to-org
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an org file.

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

When optional argument BODY-ONLY is non-nil, strip document
keywords from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".org" subtreep)))
    (org-export-to-file 'org outfile
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-org-publish-to-org (plist filename pub-dir)
  "Publish an org file to org.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'org filename ".org" plist pub-dir)
  (when (plist-get plist :htmlized-source)
    (or (require 'htmlize nil t)
	(error "Please install htmlize from https://github.com/hniksic/emacs-htmlize"))
    (require 'ox-html)
    (let* ((org-inhibit-startup t)
	   (htmlize-output-type 'css)
	   (html-ext (concat "." (or (plist-get plist :html-extension)
				     org-html-extension "html")))
	   (visitingp (find-buffer-visiting filename))
	   (work-buffer (or visitingp (find-file-noselect filename)))
	   newbuf)
      (with-current-buffer work-buffer
        (org-font-lock-ensure)
        (outline-show-all)
        (org-show-block-all)
        (setq newbuf (htmlize-buffer)))
      (with-current-buffer newbuf
	(when org-org-htmlized-css-url
	  (goto-char (point-min))
	  (and (re-search-forward
		"<style type=\"text/css\">[^\000]*?\n[ \t]*</style>.*" nil t)
	       (replace-match
		(format
		 "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\">"
		 org-org-htmlized-css-url)
                t t)))
	(write-file (concat pub-dir (file-name-nondirectory filename) html-ext)))
      (kill-buffer newbuf)
      (unless visitingp (kill-buffer work-buffer)))
    ;; FIXME: Why?  Which buffer is this supposed to apply to?
    (set-buffer-modified-p nil)))


(provide 'ox-org)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-org.el ends here
