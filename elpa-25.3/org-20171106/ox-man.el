;; ox-man.el --- Man Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2017 Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou at gmail dot com>
;;      Luis R Anaya <papoanaya aroba hot mail punto com>
;; Keywords: outlines, hypermedia, calendar, wp

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
;;
;; This library implements a Man back-end for Org generic exporter.
;;
;; To test it, run
;;
;;   M-: (org-export-to-buffer 'man "*Test Man*") RET
;;
;; in an Org buffer then switch to the buffer to see the Man export.
;; See ox.el for more details on how this exporter works.
;;
;; It introduces one new buffer keywords:
;; "MAN_CLASS_OPTIONS".

;;; Code:

(require 'cl-lib)
(require 'ox)

(defvar org-export-man-default-packages-alist)
(defvar org-export-man-packages-alist)
(defvar orgtbl-exp-regexp)



;;; Define Back-End

(org-export-define-backend 'man
  '((babel-call . org-man-babel-call)
    (bold . org-man-bold)
    (center-block . org-man-center-block)
    (code . org-man-code)
    (drawer . org-man-drawer)
    (dynamic-block . org-man-dynamic-block)
    (entity . org-man-entity)
    (example-block . org-man-example-block)
    (export-block . org-man-export-block)
    (export-snippet . org-man-export-snippet)
    (fixed-width . org-man-fixed-width)
    (footnote-definition . org-man-footnote-definition)
    (footnote-reference . org-man-footnote-reference)
    (headline . org-man-headline)
    (horizontal-rule . org-man-horizontal-rule)
    (inline-babel-call . org-man-inline-babel-call)
    (inline-src-block . org-man-inline-src-block)
    (inlinetask . org-man-inlinetask)
    (italic . org-man-italic)
    (item . org-man-item)
    (keyword . org-man-keyword)
    (line-break . org-man-line-break)
    (link . org-man-link)
    (node-property . org-man-node-property)
    (paragraph . org-man-paragraph)
    (plain-list . org-man-plain-list)
    (plain-text . org-man-plain-text)
    (planning . org-man-planning)
    (property-drawer . org-man-property-drawer)
    (quote-block . org-man-quote-block)
    (radio-target . org-man-radio-target)
    (section . org-man-section)
    (special-block . org-man-special-block)
    (src-block . org-man-src-block)
    (statistics-cookie . org-man-statistics-cookie)
    (strike-through . org-man-strike-through)
    (subscript . org-man-subscript)
    (superscript . org-man-superscript)
    (table . org-man-table)
    (table-cell . org-man-table-cell)
    (table-row . org-man-table-row)
    (target . org-man-target)
    (template . org-man-template)
    (timestamp . org-man-timestamp)
    (underline . org-man-underline)
    (verbatim . org-man-verbatim)
    (verse-block . org-man-verse-block))
  :menu-entry
  '(?M "Export to MAN"
       ((?m "As MAN file" org-man-export-to-man)
	(?p "As PDF file" org-man-export-to-pdf)
	(?o "As PDF file and open"
	    (lambda (a s v b)
	      (if a (org-man-export-to-pdf t s v b)
		(org-open-file (org-man-export-to-pdf nil s v b)))))))
  :options-alist
  '((:man-class "MAN_CLASS" nil nil t)
    (:man-class-options "MAN_CLASS_OPTIONS" nil nil t)
    (:man-header-extra "MAN_HEADER" nil nil newline)
    ;; Other variables.
    (:man-tables-centered nil nil org-man-tables-centered)
    (:man-tables-verbatim nil nil org-man-tables-verbatim)
    (:man-table-scientific-notation nil nil org-man-table-scientific-notation)
    (:man-source-highlight nil nil org-man-source-highlight)
    (:man-source-highlight-langs nil nil org-man-source-highlight-langs)))



;;; User Configurable Variables

(defgroup org-export-man nil
  "Options for exporting Org mode files to Man."
  :tag "Org Export Man"
  :group 'org-export)

;;; Tables

(defcustom org-man-tables-centered t
  "When non-nil, tables are exported in a center environment."
  :group 'org-export-man
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-man-tables-verbatim nil
  "When non-nil, tables are exported verbatim."
  :group 'org-export-man
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)


(defcustom org-man-table-scientific-notation "%sE%s"
  "Format string to display numbers in scientific notation.
The format should have \"%s\" twice, for mantissa and exponent
\(i.e. \"%s\\\\times10^{%s}\").

When nil, no transformation is made."
  :group 'org-export-man
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
          (string :tag "Format string")
          (const :tag "No formatting")))


;;; Inlinetasks
;; Src blocks

(defcustom org-man-source-highlight nil
  "Use GNU source highlight to embellish source blocks "
  :group 'org-export-man
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)


(defcustom org-man-source-highlight-langs
  '((emacs-lisp "lisp") (lisp "lisp") (clojure "lisp")
    (scheme "scheme")
    (c "c") (cc "cpp") (csharp "csharp") (d "d")
    (fortran "fortran") (cobol "cobol") (pascal "pascal")
    (ada "ada") (asm "asm")
    (perl "perl") (cperl "perl")
    (python "python") (ruby "ruby") (tcl "tcl") (lua "lua")
    (java "java") (javascript "javascript")
    (tex "latex")
    (shell-script "sh") (awk "awk") (diff "diff") (m4 "m4")
    (ocaml "caml") (caml "caml")
    (sql "sql") (sqlite "sql")
    (html "html") (css "css") (xml "xml")
    (bat "bat") (bison "bison") (clipper "clipper")
    (ldap "ldap") (opa "opa")
    (php "php") (postscript "postscript") (prolog "prolog")
    (properties "properties") (makefile "makefile")
    (tml "tml") (vala "vala") (vbscript "vbscript") (xorg "xorg"))
  "Alist mapping languages to their listing language counterpart.
The key is a symbol, the major mode symbol without the \"-mode\".
The value is the string that should be inserted as the language
parameter for the listings package.  If the mode name and the
listings name are the same, the language does not need an entry
in this list - but it does not hurt if it is present."
  :group 'org-export-man
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(repeat
          (list
           (symbol :tag "Major mode       ")
           (string :tag "Listings language"))))


;;; Compilation

(defcustom org-man-pdf-process
  '("tbl %f | eqn | groff -man | ps2pdf - > %b.pdf"
    "tbl %f | eqn | groff -man | ps2pdf - > %b.pdf"
    "tbl %f | eqn | groff -man | ps2pdf - > %b.pdf")

  "Commands to process a Man file to a PDF file.

This is a list of strings, each of them will be given to the
shell as a command.  %f in the command will be replaced by the
relative file name, %F by the absolute file name, %b by the file
base name (i.e. without directory and extension parts), %o by the
base directory of the file and %O by the absolute file name of
the output file.

By default, Org uses 3 runs of to do the processing.

Alternatively, this may be a Lisp function that does the
processing.  This function should accept the file name as
its single argument."
  :group 'org-export-pdf
  :group 'org-export-man
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
          (repeat :tag "Shell command sequence"
                  (string :tag "Shell command"))
          (const :tag "2 runs of pdfgroff"
                 ("tbl %f | eqn | groff -mm | ps2pdf - > %b.pdf"
                  "tbl %f | eqn | groff -mm | ps2pdf - > %b.pdf" ))
          (const :tag "3 runs of pdfgroff"
                 ("tbl %f | eqn | groff -mm | ps2pdf - > %b.pdf"
                  "tbl %f | eqn | groff -mm | ps2pdf - > %b.pdf"
                  "tbl %f | eqn | groff -mm | ps2pdf - > %b.pdf"))
          (function)))

(defcustom org-man-logfiles-extensions
  '("log" "out" "toc")
  "The list of file extensions to consider as Man logfiles."
  :group 'org-export-man
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(repeat (string :tag "Extension")))

(defcustom org-man-remove-logfiles t
  "Non-nil means remove the logfiles produced by PDF production.
These are the .aux, .log, .out, and .toc files."
  :group 'org-export-man
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)



;;; Internal Functions

(defun org-man--caption/label-string (element info)
  "Return caption and label Man string for ELEMENT.

INFO is a plist holding contextual information.  If there's no
caption nor label, return the empty string.

For non-floats, see `org-man--wrap-label'."
  (let ((label (org-element-property :label element))
	(main (org-export-get-caption element))
	(short (org-export-get-caption element t)))
    (cond ((and (not main) (not label)) "")
	  ((not main) (format "\\fI%s\\fP" label))
	  ;; Option caption format with short name.
	  (short (format "\\fR%s\\fP - \\fI\\P - %s\n"
			 (org-export-data short info)
			 (org-export-data main info)))
	  ;; Standard caption format.
	  (t (format "\\fR%s\\fP" (org-export-data main info))))))

(defun org-man--wrap-label (element output)
  "Wrap label associated to ELEMENT around OUTPUT, if appropriate.
This function shouldn't be used for floats.  See
`org-man--caption/label-string'."
  (let ((label (org-element-property :name element)))
    (if (or (not output) (not label) (string= output "") (string= label ""))
        output
      (concat (format "%s\n.br\n" label) output))))

(defun org-man--protect-text (text)
  "Protect minus and backslash characters in string TEXT."
  (replace-regexp-in-string "-" "\\-" text nil t))



;;; Template

(defun org-man-template (contents info)
  "Return complete document string after Man conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ((title (when (plist-get info :with-title)
		  (org-export-data (plist-get info :title) info)))
        (attr (read (format "(%s)"
                            (mapconcat
                             #'identity
                             (list (plist-get info :man-class-options))
                             " "))))
        (section-item (plist-get attr :section-id)))

    (concat

     (cond
      ((and title (stringp section-item))
       (format ".TH \"%s\" \"%s\" \n" title section-item))
      ((and (string= "" title) (stringp section-item))
       (format ".TH \"%s\" \"%s\" \n" " " section-item))
      (title
       (format ".TH \"%s\" \"1\" \n" title))
      (t
       ".TH \" \" \"1\" "))
     contents)))




;;; Transcode Functions

;;; Babel Call
;;
;; Babel Calls are ignored.


;;; Bold

(defun org-man-bold (_bold contents _info)
  "Transcode BOLD from Org to Man.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format "\\fB%s\\fP" contents))


;;; Center Block

(defun org-man-center-block (center-block contents _info)
  "Transcode a CENTER-BLOCK element from Org to Man.
CONTENTS holds the contents of the center block.  INFO is a plist
holding contextual information."
  (org-man--wrap-label
   center-block
   (format ".ce %d\n.nf\n%s\n.fi"
           (- (length (split-string contents "\n")) 1 )
           contents)))


;;; Code

(defun org-man-code (code _contents _info)
  "Transcode a CODE object from Org to Man."
  (format "\\fC%s\\fP"
	  (org-man--protect-text (org-element-property :value code))))


;;; Drawer

(defun org-man-drawer (_drawer contents _info)
  "Transcode a DRAWER element from Org to Man.
   DRAWER holds the drawer information
   CONTENTS holds the contents of the block.
   INFO is a plist holding contextual information. "
  contents)


;;; Dynamic Block

(defun org-man-dynamic-block (dynamic-block contents _info)
  "Transcode a DYNAMIC-BLOCK element from Org to Man.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  (org-man--wrap-label dynamic-block contents))


;;; Entity

(defun org-man-entity (entity _contents _info)
  "Transcode an ENTITY object from Org to Man.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (org-element-property :utf-8 entity))


;;; Example Block

(defun org-man-example-block (example-block _contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-man--wrap-label
   example-block
   (format ".RS\n.nf\n%s\n.fi\n.RE"
           (org-export-format-code-default example-block info))))


;;; Export Block

(defun org-man-export-block (export-block _contents _info)
  "Transcode a EXPORT-BLOCK element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "MAN")
    (org-remove-indentation (org-element-property :value export-block))))


;;; Export Snippet

(defun org-man-export-snippet (export-snippet _contents _info)
  "Transcode a EXPORT-SNIPPET object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'man)
    (org-element-property :value export-snippet)))


;;; Fixed Width

(defun org-man-fixed-width (fixed-width _contents _info)
  "Transcode a FIXED-WIDTH element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-man--wrap-label
   fixed-width
   (format "\\fC\n%s\\fP"
           (org-remove-indentation
            (org-element-property :value fixed-width)))))


;;; Footnote Definition
;;
;; Footnote Definitions are ignored.

;;; Footnote References
;;
;; Footnote References are Ignored


;;; Headline

(defun org-man-headline (headline contents info)
  "Transcode a HEADLINE element from Org to Man.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((level (org-export-get-relative-level headline info))
	 ;; Section formatting will set two placeholders: one for the
	 ;; title and the other for the contents.
	 (section-fmt
	  (pcase level
	    (1 ".SH \"%s\"\n%s")
	    (2 ".SS \"%s\"\n%s")
	    (3 ".SS \"%s\"\n%s")
	    (_ nil)))
	 (text (org-export-data (org-element-property :title headline) info)))

    (cond
     ;; Case 1: This is a footnote section: ignore it.
     ((org-element-property :footnote-section-p headline) nil)

     ;; Case 2. This is a deep sub-tree: export it as a list item.
     ;;         Also export as items headlines for which no section
     ;;         format has been found.
     ((or (not section-fmt) (org-export-low-level-p headline info))
      ;; Build the real contents of the sub-tree.
      (let ((low-level-body
	     (concat
	      ;; If the headline is the first sibling, start a list.
	      (when (org-export-first-sibling-p headline info)
		(format "%s\n" ".RS"))
	      ;; Itemize headline
	      ".TP\n.ft I\n" text "\n.ft\n"
	      contents ".RE")))
	;; If headline is not the last sibling simply return
	;; LOW-LEVEL-BODY.  Otherwise, also close the list, before any
	;; blank line.
	(if (not (org-export-last-sibling-p headline info)) low-level-body
	  (replace-regexp-in-string
	   "[ \t\n]*\\'" ""
	   low-level-body))))

     ;; Case 3. Standard headline.  Export it as a section.
     (t (format section-fmt text contents )))))

;;; Horizontal Rule
;; Not supported

;;; Inline Babel Call
;;
;; Inline Babel Calls are ignored.

;;; Inline Src Block

(defun org-man-inline-src-block (inline-src-block _contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to Man.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((code (org-element-property :value inline-src-block)))
    (cond
     ((plist-get info :man-source-highlight)
      (let* ((tmpdir temporary-file-directory)
             (in-file  (make-temp-name
                        (expand-file-name "srchilite" tmpdir)))
             (out-file (make-temp-name
                        (expand-file-name "reshilite" tmpdir)))
             (org-lang (org-element-property :language inline-src-block))
             (lst-lang
	      (cadr (assq (intern org-lang)
			  (plist-get info :man-source-highlight-langs))))

             (cmd (concat (expand-file-name "source-highlight")
                          " -s " lst-lang
                          " -f groff_man"
                          " -i " in-file
                          " -o " out-file )))

        (if lst-lang
            (let ((code-block "" ))
              (with-temp-file in-file (insert code))
              (shell-command cmd)
              (setq code-block  (org-file-contents out-file))
              (delete-file in-file)
              (delete-file out-file)
              code-block)
          (format ".RS\n.nf\n\\fC\\m[black]%s\\m[]\\fP\n.fi\n.RE\n"
                  code))))

     ;; Do not use a special package: transcode it verbatim.
     (t
      (concat ".RS\n.nf\n" "\\fC" "\n" code "\n"
              "\\fP\n.fi\n.RE\n")))))


;;; Inlinetask
;;; Italic

(defun org-man-italic (_italic contents _info)
  "Transcode ITALIC from Org to Man.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (format "\\fI%s\\fP" contents))


;;; Item


(defun org-man-item (item contents info)
  "Transcode an ITEM element from Org to Man.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((bullet (org-element-property :bullet item))
         (type (org-element-property :type (org-element-property :parent item)))
         (checkbox (pcase (org-element-property :checkbox item)
                     (`on "\\o'\\(sq\\(mu'")
                     (`off "\\(sq ")
                     (`trans "\\o'\\(sq\\(mi'")))

         (tag (let ((tag (org-element-property :tag item)))
                ;; Check-boxes must belong to the tag.
                (and tag (format "\\fB%s\\fP"
                                 (concat checkbox
                                         (org-export-data tag info)))))))

    (if (and (null tag) (null checkbox))
	(let* ((bullet (org-trim bullet))
	       (marker (cond  ((string= "-" bullet) "\\(em")
			      ((string= "*" bullet) "\\(bu")
			      ((eq type 'ordered)
			       (format "%s " (org-trim bullet)))
			      (t "\\(dg"))))
	  (concat ".IP " marker " 4\n"
		  (org-trim (or contents " " ))))
      (concat ".TP\n" (or tag (concat " " checkbox)) "\n"
              (org-trim (or contents " " ))))))

;;; Keyword


(defun org-man-keyword (keyword _contents _info)
  "Transcode a KEYWORD element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (cond
     ((string= key "MAN") value)
     ((string= key "INDEX") nil)
     ((string= key "TOC"   ) nil))))


;;; Line Break

(defun org-man-line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "\n.br\n")


;;; Link


(defun org-man-link (link desc _info)
  "Transcode a LINK object from Org to Man.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((type (org-element-property :type link))
         (raw-path (org-element-property :path link))
         ;; Ensure DESC really exists, or set it to nil.
         (desc (and (not (string= desc "")) desc))
         (path (cond
                ((member type '("http" "https" "ftp" "mailto"))
                 (concat type ":" raw-path))
                ((string= type "file") (org-export-file-uri raw-path))
                (t raw-path))))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'man))
     ;; External link with a description part.
     ((and path desc) (format "%s \\fBat\\fP \\fI%s\\fP" path desc))
     ;; External link without a description part.
     (path (format "\\fI%s\\fP" path))
     ;; No path, only description.  Try to do something useful.
     (t (format "\\fI%s\\fP" desc)))))

;;;; Node Property

(defun org-man-node-property (node-property _contents _info)
  "Transcode a NODE-PROPERTY element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "%s:%s"
          (org-element-property :key node-property)
          (let ((value (org-element-property :value node-property)))
            (if value (concat " " value) ""))))

;;; Paragraph

(defun org-man-paragraph (paragraph contents _info)
  "Transcode a PARAGRAPH element from Org to Man.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let ((parent (plist-get (nth 1 paragraph) :parent)))
    (when parent
      (let ((parent-type (car parent))
            (fixed-paragraph ""))
        (cond ((and (eq parent-type 'item)
                    (plist-get (nth 1 parent) :bullet ))
               (setq fixed-paragraph (concat "" contents)))
              ((eq parent-type 'section)
               (setq fixed-paragraph (concat ".PP\n" contents)))
              ((eq parent-type 'footnote-definition)
               (setq fixed-paragraph contents))
              (t (setq fixed-paragraph (concat "" contents))))
        fixed-paragraph ))))


;;; Plain List

(defun org-man-plain-list (_plain-list contents _info)
  "Transcode a PLAIN-LIST element from Org to Man.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  contents)

;;; Plain Text

(defun org-man-plain-text (text info)
  "Transcode a TEXT string from Org to Man.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (let ((output text))
    ;; Protect various chars.
    (setq output (replace-regexp-in-string
		  "\\(?:[^\\]\\|^\\)\\(\\\\\\)\\(?:[^%$#&{}~^_\\]\\|$\\)"
		  "$\\" output nil t 1))
    ;; Activate smart quotes.  Be sure to provide original TEXT string
    ;; since OUTPUT may have been modified.
    (when (plist-get info :with-smart-quotes)
      (setq output (org-export-activate-smart-quotes output :utf-8 info text)))
    ;; Handle break preservation if required.
    (when (plist-get info :preserve-breaks)
      (setq output (replace-regexp-in-string "\\(\\\\\\\\\\)?[ \t]*\n" ".br\n"
					     output)))
    ;; Return value.
    output))



;;; Planning


;;; Property Drawer

(defun org-man-property-drawer (_property-drawer contents _info)
  "Transcode a PROPERTY-DRAWER element from Org to Man.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (and (org-string-nw-p contents)
       (format ".RS\n.nf\n%s\n.fi\n.RE" contents)))

;;; Quote Block

(defun org-man-quote-block (quote-block contents _info)
  "Transcode a QUOTE-BLOCK element from Org to Man.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-man--wrap-label
   quote-block
   (format ".RS\n%s\n.RE" contents)))


;;; Radio Target

(defun org-man-radio-target (_radio-target text _info)
  "Transcode a RADIO-TARGET object from Org to Man.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  text)


;;; Section

(defun org-man-section (_section contents _info)
  "Transcode a SECTION element from Org to Man.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)


;;; Special Block

(defun org-man-special-block (special-block contents _info)
  "Transcode a SPECIAL-BLOCK element from Org to Man.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-man--wrap-label special-block (format "%s\n" contents)))


;;; Src Block

(defun org-man-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to Man.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (not (plist-get info :man-source-highlight))
      (format ".RS\n.nf\n\\fC%s\\fP\n.fi\n.RE\n\n"
	      (org-export-format-code-default src-block info))
    (let* ((tmpdir temporary-file-directory)
	   (in-file  (make-temp-name (expand-file-name "srchilite" tmpdir)))
	   (out-file (make-temp-name (expand-file-name "reshilite" tmpdir)))
	   (code (org-element-property :value src-block))
	   (org-lang (org-element-property :language src-block))
	   (lst-lang
	    (cadr (assq (intern org-lang)
			(plist-get info :man-source-highlight-langs))))
	   (cmd (concat "source-highlight"
			" -s " lst-lang
			" -f groff_man "
			" -i " in-file
			" -o " out-file)))
      (if lst-lang
	  (let ((code-block ""))
	    (with-temp-file in-file (insert code))
	    (shell-command cmd)
	    (setq code-block  (org-file-contents out-file))
	    (delete-file in-file)
	    (delete-file out-file)
	    code-block)
	(format ".RS\n.nf\n\\fC\\m[black]%s\\m[]\\fP\n.fi\n.RE" code)))))


;;; Statistics Cookie

(defun org-man-statistics-cookie (statistics-cookie _contents _info)
  "Transcode a STATISTICS-COOKIE object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value statistics-cookie))


;;; Strike-Through

(defun org-man-strike-through (_strike-through contents _info)
  "Transcode STRIKE-THROUGH from Org to Man.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (format "\\fI%s\\fP" contents))

;;; Subscript

(defun org-man-subscript (_subscript contents _info)
  "Transcode a SUBSCRIPT object from Org to Man.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format  "\\d\\s-2%s\\s+2\\u" contents))

;;; Superscript "^_%s$

(defun org-man-superscript (_superscript contents _info)
  "Transcode a SUPERSCRIPT object from Org to Man.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format  "\\u\\s-2%s\\s+2\\d" contents))


;;; Table
;;
;; `org-man-table' is the entry point for table transcoding.  It
;; takes care of tables with a "verbatim" attribute.  Otherwise, it
;; delegates the job to either `org-man-table--table.el-table' or
;; `org-man-table--org-table' functions, depending of the type of
;; the table.
;;
;; `org-man-table--align-string' is a subroutine used to build
;; alignment string for Org tables.

(defun org-man-table (table contents info)
  "Transcode a TABLE element from Org to Man.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (cond
   ;; Case 1: verbatim table.
   ((or (plist-get info :man-tables-verbatim)
        (let ((attr (read (format "(%s)"
                 (mapconcat
                  #'identity
                  (org-element-property :attr_man table)
                  " ")))))

          (and attr (plist-get attr :verbatim))))

    (format ".nf\n\\fC%s\\fP\n.fi"
            ;; Re-create table, without affiliated keywords.
            (org-trim
             (org-element-interpret-data
              `(table nil ,@(org-element-contents table))))))
   ;; Case 2: Standard table.
   (t (org-man-table--org-table table contents info))))

(defun org-man-table--align-string (divider table info)
  "Return an appropriate Man alignment string.
TABLE is the considered table.  INFO is a plist used as
a communication channel."
  (let (alignment)
    ;; Extract column groups and alignment from first (non-rule) row.
    (org-element-map
	(org-element-map table 'table-row
	  (lambda (row)
	    (and (eq (org-element-property :type row) 'standard) row))
	  info 'first-match)
	'table-cell
      (lambda (cell)
	(let* ((borders (org-export-table-cell-borders cell info))
	       (raw-width (org-export-table-cell-width cell info))
	       (width-cm (when raw-width (/ raw-width 5)))
	       (width (if raw-width (format "w(%dc)"
					    (if (< width-cm 1) 1 width-cm)) "")))
	  ;; Check left border for the first cell only.
	  (when (and (memq 'left borders) (not alignment))
	    (push "|" alignment))
	  (push
	   (concat (pcase (org-export-table-cell-alignment cell info)
		     (`left "l") (`right "r") (`center "c"))
		   width
		   divider)
	   alignment)
	  (when (memq 'right borders) (push "|" alignment))))
      info)
    (apply #'concat (reverse alignment))))

(defun org-man-table--org-table (table contents info)
  "Return appropriate Man code for an Org table.

TABLE is the table type element to transcode.  CONTENTS is its
contents, as a string.  INFO is a plist used as a communication
channel.

This function assumes TABLE has `org' as its `:type' attribute."
  (let* ((attr (org-export-read-attribute :attr_man table))
         (caption (and (not (plist-get attr :disable-caption))
		       (org-man--caption/label-string table info)))
         (divider (if (plist-get attr :divider) "|" " "))

         ;; Determine alignment string.
         (alignment (org-man-table--align-string divider table info))
         ;; Extract others display options.

         (lines (org-split-string contents "\n"))

         (attr-list
	  (delq nil
		(list
		 (and (plist-get attr :expand) "expand")
		 (let ((placement (plist-get attr :placement)))
		   (cond ((string= placement 'center) "center")
			 ((string= placement 'left) nil)
			 ((plist-get info :man-tables-centered) "center")
			 (t "")))
		 (or (plist-get attr :boxtype) "box"))))

         (title-line  (plist-get attr :title-line))
         (long-cells (plist-get attr :long-cells))

         (table-format (concat
                        (format "%s" (or (car attr-list) "" ))
                        (or
                         (let ((output-list '()))
                           (when (cdr attr-list)
                             (dolist (attr-item (cdr attr-list))
			       (setq output-list (concat output-list  (format ",%s" attr-item)))))
                           output-list)
                         "")))

	 (first-line (when lines (org-split-string (car lines) "\t"))))
    ;; Prepare the final format string for the table.


    (cond
     ;; Others.
     (lines (concat ".TS\n " table-format ";\n"

                    (format "%s.\n"
                            (let ((final-line ""))
                              (when title-line
                                (dotimes (_ (length first-line))
                                  (setq final-line (concat final-line "cb" divider))))

                              (setq final-line (concat final-line "\n"))

                              (if alignment
                                  (setq final-line (concat final-line alignment))
                                (dotimes (_ (length first-line))
                                  (setq final-line (concat final-line "c" divider))))
                              final-line ))

                    (format "%s.TE\n"
                            (let ((final-line "")
                                  (long-line "")
                                  (lines (org-split-string contents "\n")))

                              (dolist (line-item lines)
                                (setq long-line "")

                                (if long-cells
                                    (progn
                                      (if (string= line-item "_")
                                          (setq long-line (format "%s\n" line-item))
                                        ;; else string =
                                        (let ((cell-item-list (org-split-string line-item "\t")))
                                          (dolist (cell-item cell-item-list)

                                            (cond  ((eq cell-item (car (last cell-item-list)))
                                                    (setq long-line (concat long-line
                                                                            (format "T{\n%s\nT}\t\n"  cell-item ))))
                                                   (t
                                                    (setq long-line (concat long-line
                                                                            (format "T{\n%s\nT}\t"  cell-item ))))))
					  long-line))
				      ;; else long cells
				      (setq final-line (concat final-line long-line )))

                                  (setq final-line (concat final-line line-item "\n"))))
                              final-line))

                    (and caption (format ".TB \"%s\"" caption)))))))

;;; Table Cell

(defun org-man-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to Man
CONTENTS is the cell contents.  INFO is a plist used as
a communication channel."
  (concat
   (let ((scientific-format (plist-get info :man-table-scientific-notation)))
     (if (and contents
	      scientific-format
	      (string-match orgtbl-exp-regexp contents))
	 ;; Use appropriate format string for scientific notation.
	 (format scientific-format
		 (match-string 1 contents)
		 (match-string 2 contents))
       contents))
   (when (org-export-get-next-element table-cell info) "\t")))


;;; Table Row

(defun org-man-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to Man.
CONTENTS is the contents of the row.  INFO is a plist used as
a communication channel."
  ;; Rules are ignored since table separators are deduced from borders
  ;; of the current row.
  (when (eq (org-element-property :type table-row) 'standard)
    (let ((borders
	   ;; TABLE-ROW's borders are extracted from its first cell.
	   (org-export-table-cell-borders
	    (car (org-element-contents table-row)) info)))
      (concat
       (cond ((and (memq 'top borders) (memq 'above borders)) "_\n"))
       contents
       (cond ((and (memq 'bottom borders) (memq 'below borders)) "\n_")
	     ((memq 'below borders) "\n_"))))))


;;; Target

(defun org-man-target (target _contents info)
  "Transcode a TARGET object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "\\fI%s\\fP" (org-export-get-reference target info)))


;;; Timestamp

(defun org-man-timestamp (_timestamp _contents _info)
  "Transcode a TIMESTAMP object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "")


;;; Underline

(defun org-man-underline (_underline contents _info)
  "Transcode UNDERLINE from Org to Man.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (format "\\fI%s\\fP" contents))


;;; Verbatim

(defun org-man-verbatim (verbatim _contents _info)
  "Transcode a VERBATIM object from Org to Man."
  (format "\\fI%s\\fP"
	  (org-man--protect-text (org-element-property :value verbatim))))


;;; Verse Block

(defun org-man-verse-block (_verse-block contents _info)
  "Transcode a VERSE-BLOCK element from Org to Man.
CONTENTS is verse block contents. INFO is a plist holding
contextual information."
  (format ".RS\n.ft I\n%s\n.ft\n.RE" contents))



;;; Interactive functions

(defun org-man-export-to-man
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Man file.

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

When optional argument BODY-ONLY is non-nil, only the body
without any markers.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".man" subtreep)))
    (org-export-to-file 'man outfile
      async subtreep visible-only body-only ext-plist)))

(defun org-man-export-to-pdf
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to Groff then process through to PDF.

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

When optional argument BODY-ONLY is non-nil, only write between
markers.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".man" subtreep)))
    (org-export-to-file 'man outfile
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))

(defun org-man-compile (file)
  "Compile a Groff file.

FILE is the name of the file being compiled.  Processing is done
through the command specified in `org-man-pdf-process'.

Return PDF file name or an error if it couldn't be produced."
  (message "Processing Groff file %s..." file)
  (let ((output (org-compile-file file org-man-pdf-process "pdf")))
    (when org-man-remove-logfiles
      (let ((base (file-name-sans-extension output)))
	(dolist (ext org-man-logfiles-extensions)
	  (let ((file (concat base "." ext)))
	    (when (file-exists-p file) (delete-file file))))))
    (message "Process completed.")
    output))

(provide 'ox-man)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-man.el ends here
