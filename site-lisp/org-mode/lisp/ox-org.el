;;; ox-org.el --- Org Back-End for Org Export Engine

;; Copyright (C) 2013-2014 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ox)
(declare-function htmlize-buffer "htmlize" (&optional buffer))

(defgroup org-export-org nil
  "Options for exporting Org mode files to Org."
  :tag "Org Export Org"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))

(define-obsolete-variable-alias
  'org-export-htmlized-org-css-url 'org-org-htmlized-css-url "24.4")
(defcustom org-org-htmlized-css-url nil
  "URL pointing to the CSS defining colors for htmlized Emacs buffers.
Normally when creating an htmlized version of an Org buffer,
htmlize will create the CSS to define the font colors.  However,
this does not work when converting in batch mode, and it also can
look bad if different people with different fontification setup
work on the same website.  When this variable is non-nil,
creating an htmlized version of an Org buffer using
`org-org-export-as-org' will include a link to this URL if the
setting of `org-html-htmlize-output-type' is 'css."
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
    (comment . (lambda (&rest args) ""))
    (comment-block . (lambda (&rest args) ""))
    (diary-sexp . org-org-identity)
    (drawer . org-org-identity)
    (dynamic-block . org-org-identity)
    (entity . org-org-identity)
    (example-block . org-org-identity)
    (fixed-width . org-org-identity)
    (footnote-definition . org-org-identity)
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
    (link . org-org-identity)
    (node-property . org-org-identity)
    (paragraph . org-org-identity)
    (plain-list . org-org-identity)
    (planning . org-org-identity)
    (property-drawer . org-org-identity)
    (quote-block . org-org-identity)
    (quote-section . org-org-identity)
    (radio-target . org-org-identity)
    (section . org-org-identity)
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
		(org-open-file (org-org-export-to-org nil s v b))))))))

(defun org-org-identity (blob contents info)
  "Transcode BLOB element or object back into Org syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored."
  (let ((case-fold-search t))
    (replace-regexp-in-string
     "^[ \t]*#\\+ATTR_[-_A-Za-z0-9]+:\\(?: .*\\)?\n" ""
     (org-export-expand blob contents t))))

(defun org-org-headline (headline contents info)
  "Transcode HEADLINE element back into Org syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored."
  (unless (plist-get info :with-todo-keywords)
    (org-element-put-property headline :todo-keyword nil))
  (unless (plist-get info :with-tags)
    (org-element-put-property headline :tags nil))
  (unless (plist-get info :with-priority)
    (org-element-put-property headline :priority nil))
  (org-element-put-property headline :level
			    (org-export-get-relative-level headline info))
  (org-element-headline-interpreter headline contents))

(defun org-org-keyword (keyword contents info)
  "Transcode KEYWORD element back into Org syntax.
CONTENTS is nil.  INFO is ignored.  This function ignores
keywords targeted at other export back-ends."
  (unless (member (org-element-property :key keyword)
		  (mapcar
		   (lambda (block-cons)
		     (and (eq (cdr block-cons) 'org-element-export-block-parser)
			  (car block-cons)))
		   org-element-block-name-alist))
    (org-element-keyword-interpreter keyword nil)))

;;;###autoload
(defun org-org-export-as-org (&optional async subtreep visible-only ext-plist)
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

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org ORG Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'org "*Org ORG Export*"
    async subtreep visible-only nil ext-plist (lambda () (org-mode))))

;;;###autoload
(defun org-org-export-to-org (&optional async subtreep visible-only ext-plist)
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

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".org" subtreep)))
    (org-export-to-file 'org outfile
      async subtreep visible-only nil ext-plist)))

;;;###autoload
(defun org-org-publish-to-org (plist filename pub-dir)
  "Publish an org file to org.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'org filename ".org" plist pub-dir)
  (when (plist-get plist :htmlized-source)
    (require 'htmlize)
    (require 'ox-html)
    (let* ((org-inhibit-startup t)
	   (htmlize-output-type 'css)
	   (html-ext (concat "." (or (plist-get plist :html-extension)
				     org-html-extension "html")))
	   (visitingp (find-buffer-visiting filename))
	   (work-buffer (or visitingp (find-file filename)))
	   newbuf)
      (font-lock-fontify-buffer)
      (show-all)
      (org-show-block-all)
      (setq newbuf (htmlize-buffer))
      (with-current-buffer newbuf
	(when org-org-htmlized-css-url
	  (goto-char (point-min))
	  (and (re-search-forward
		"<style type=\"text/css\">[^\000]*?\n[ \t]*</style>.*" nil t)
	       (replace-match
		(format
		 "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\">"
		 org-org-htmlized-css-url) t t)))
	(write-file (concat pub-dir (file-name-nondirectory filename) html-ext)))
      (kill-buffer newbuf)
      (unless visitingp (kill-buffer work-buffer)))
    (set-buffer-modified-p nil)))


(provide 'ox-org)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-org.el ends here
