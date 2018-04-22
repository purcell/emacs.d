;;; ox-odt.el --- OpenDocument Text Exporter for Org Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2017 Free Software Foundation, Inc.

;; Author: Jambunathan K <kjambunathan at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org

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

(require 'cl-lib)
(require 'format-spec)
(require 'ox)
(require 'org-compat)
(require 'table nil 'noerror)

;;; Define Back-End

(org-export-define-backend 'odt
  '((bold . org-odt-bold)
    (center-block . org-odt-center-block)
    (clock . org-odt-clock)
    (code . org-odt-code)
    (drawer . org-odt-drawer)
    (dynamic-block . org-odt-dynamic-block)
    (entity . org-odt-entity)
    (example-block . org-odt-example-block)
    (export-block . org-odt-export-block)
    (export-snippet . org-odt-export-snippet)
    (fixed-width . org-odt-fixed-width)
    (footnote-definition . org-odt-footnote-definition)
    (footnote-reference . org-odt-footnote-reference)
    (headline . org-odt-headline)
    (horizontal-rule . org-odt-horizontal-rule)
    (inline-src-block . org-odt-inline-src-block)
    (inlinetask . org-odt-inlinetask)
    (italic . org-odt-italic)
    (item . org-odt-item)
    (keyword . org-odt-keyword)
    (latex-environment . org-odt-latex-environment)
    (latex-fragment . org-odt-latex-fragment)
    (line-break . org-odt-line-break)
    (link . org-odt-link)
    (node-property . org-odt-node-property)
    (paragraph . org-odt-paragraph)
    (plain-list . org-odt-plain-list)
    (plain-text . org-odt-plain-text)
    (planning . org-odt-planning)
    (property-drawer . org-odt-property-drawer)
    (quote-block . org-odt-quote-block)
    (radio-target . org-odt-radio-target)
    (section . org-odt-section)
    (special-block . org-odt-special-block)
    (src-block . org-odt-src-block)
    (statistics-cookie . org-odt-statistics-cookie)
    (strike-through . org-odt-strike-through)
    (subscript . org-odt-subscript)
    (superscript . org-odt-superscript)
    (table . org-odt-table)
    (table-cell . org-odt-table-cell)
    (table-row . org-odt-table-row)
    (target . org-odt-target)
    (template . org-odt-template)
    (timestamp . org-odt-timestamp)
    (underline . org-odt-underline)
    (verbatim . org-odt-verbatim)
    (verse-block . org-odt-verse-block))
  :filters-alist '((:filter-parse-tree
		    . (org-odt--translate-latex-fragments
		       org-odt--translate-description-lists
		       org-odt--translate-list-tables
		       org-odt--translate-image-links)))
  :menu-entry
  '(?o "Export to ODT"
       ((?o "As ODT file" org-odt-export-to-odt)
	(?O "As ODT file and open"
	    (lambda (a s v b)
	      (if a (org-odt-export-to-odt t s v)
		(org-open-file (org-odt-export-to-odt nil s v) 'system))))))
  :options-alist
  '((:odt-styles-file "ODT_STYLES_FILE" nil nil t)
    (:description "DESCRIPTION" nil nil newline)
    (:keywords "KEYWORDS" nil nil space)
    (:subtitle "SUBTITLE" nil nil parse)
    ;; Other variables.
    (:odt-content-template-file nil nil org-odt-content-template-file)
    (:odt-display-outline-level nil nil org-odt-display-outline-level)
    (:odt-fontify-srcblocks nil nil org-odt-fontify-srcblocks)
    (:odt-format-drawer-function nil nil org-odt-format-drawer-function)
    (:odt-format-headline-function nil nil org-odt-format-headline-function)
    (:odt-format-inlinetask-function nil nil org-odt-format-inlinetask-function)
    (:odt-inline-formula-rules nil nil org-odt-inline-formula-rules)
    (:odt-inline-image-rules nil nil org-odt-inline-image-rules)
    (:odt-pixels-per-inch nil nil org-odt-pixels-per-inch)
    (:odt-styles-file nil nil org-odt-styles-file)
    (:odt-table-styles nil nil org-odt-table-styles)
    (:odt-use-date-fields nil nil org-odt-use-date-fields)
    ;; Redefine regular option.
    (:with-latex nil "tex" org-odt-with-latex)
    ;; Retrieve LaTeX header for fragments.
    (:latex-header "LATEX_HEADER" nil nil newline)))


;;; Dependencies

;;; Hooks

;;; Function and Dynamically Scoped Variables Declarations

(declare-function hfy-face-to-style "htmlfontify" (fn))
(declare-function hfy-face-or-def-to-name "htmlfontify" (fn))
(declare-function archive-zip-extract "arc-mode" (archive name))
(declare-function org-create-math-formula "org" (latex-frag &optional mathml-file))
(declare-function browse-url-file-url "browse-url" (file))

(defvar nxml-auto-insert-xml-declaration-flag) ; nxml-mode.el
(defvar archive-zip-extract)		       ; arc-mode.el
(defvar hfy-end-span-handler)		       ; htmlfontify.el
(defvar hfy-begin-span-handler)		       ; htmlfontify.el
(defvar hfy-face-to-css)		       ; htmlfontify.el
(defvar hfy-html-quote-map)		       ; htmlfontify.el
(defvar hfy-html-quote-regex)		       ; htmlfontify.el


;;; Internal Variables

(defconst org-odt-lib-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Location of ODT exporter.
Use this to infer values of `org-odt-styles-dir' and
`org-odt-schema-dir'.")

(defvar org-odt-data-dir
  (expand-file-name "../../etc/" org-odt-lib-dir)
  "Data directory for ODT exporter.
Use this to infer values of `org-odt-styles-dir' and
`org-odt-schema-dir'.")

(defconst org-odt-special-string-regexps
  '(("\\\\-" . "&#x00ad;\\1")		; shy
    ("---\\([^-]\\)" . "&#x2014;\\1")	; mdash
    ("--\\([^-]\\)" . "&#x2013;\\1")	; ndash
    ("\\.\\.\\." . "&#x2026;"))		; hellip
  "Regular expressions for special string conversion.")

(defconst org-odt-schema-dir-list
  (list
   (and org-odt-data-dir
	(expand-file-name "./schema/" org-odt-data-dir)) ; bail out
   (eval-when-compile
     (and (boundp 'org-odt-data-dir) org-odt-data-dir ; see make install
	  (expand-file-name "./schema/" org-odt-data-dir))))
  "List of directories to search for OpenDocument schema files.
Use this list to set the default value of
`org-odt-schema-dir'.  The entries in this list are
populated heuristically based on the values of `org-odt-lib-dir'
and `org-odt-data-dir'.")

(defconst org-odt-styles-dir-list
  (list
   (and org-odt-data-dir
	(expand-file-name "./styles/" org-odt-data-dir)) ; bail out
   (eval-when-compile
     (and (boundp 'org-odt-data-dir) org-odt-data-dir ; see make install
	  (expand-file-name "./styles/" org-odt-data-dir)))
   (expand-file-name "../etc/styles/" org-odt-lib-dir) ; git
   (expand-file-name "./etc/styles/" org-odt-lib-dir)  ; elpa
   (expand-file-name "./org/" data-directory)	       ; system
   )
  "List of directories to search for OpenDocument styles files.
See `org-odt-styles-dir'.  The entries in this list are populated
heuristically based on the values of `org-odt-lib-dir' and
`org-odt-data-dir'.")

(defconst org-odt-styles-dir
  (let ((styles-dir
	 (cl-find-if
	  (lambda (dir)
	    (and dir
		 (file-readable-p
		  (expand-file-name "OrgOdtContentTemplate.xml" dir))
		 (file-readable-p (expand-file-name "OrgOdtStyles.xml" dir))))
	  org-odt-styles-dir-list)))
    (unless styles-dir
      (error "Error (ox-odt): Cannot find factory styles files, aborting"))
    styles-dir)
  "Directory that holds auxiliary XML files used by the ODT exporter.

This directory contains the following XML files -
 \"OrgOdtStyles.xml\" and \"OrgOdtContentTemplate.xml\".  These
 XML files are used as the default values of
 `org-odt-styles-file' and `org-odt-content-template-file'.

The default value of this variable varies depending on the
version of Org in use and is initialized from
`org-odt-styles-dir-list'.  Note that the user could be using Org
from one of: Org own private git repository, GNU ELPA tar or
standard Emacs.")

(defconst org-odt-bookmark-prefix "OrgXref.")

(defconst org-odt-manifest-file-entry-tag
  "\n<manifest:file-entry manifest:media-type=\"%s\" manifest:full-path=\"%s\"%s/>")

(defconst org-odt-file-extensions
  '(("odt" . "OpenDocument Text")
    ("ott" . "OpenDocument Text Template")
    ("odm" . "OpenDocument Master Document")
    ("ods" . "OpenDocument Spreadsheet")
    ("ots" . "OpenDocument Spreadsheet Template")
    ("odg" . "OpenDocument Drawing (Graphics)")
    ("otg" . "OpenDocument Drawing Template")
    ("odp" . "OpenDocument Presentation")
    ("otp" . "OpenDocument Presentation Template")
    ("odi" . "OpenDocument Image")
    ("odf" . "OpenDocument Formula")
    ("odc" . "OpenDocument Chart")))

(defconst org-odt-table-style-format
  "
<style:style style:name=\"%s\" style:family=\"table\">
  <style:table-properties style:rel-width=\"%s%%\" fo:margin-top=\"0cm\" fo:margin-bottom=\"0.20cm\" table:align=\"center\"/>
</style:style>
"
  "Template for auto-generated Table styles.")

(defvar org-odt-automatic-styles '()
  "Registry of automatic styles for various OBJECT-TYPEs.
The variable has the following form:
 ((OBJECT-TYPE-A
   ((OBJECT-NAME-A.1 OBJECT-PROPS-A.1)
    (OBJECT-NAME-A.2 OBJECT-PROPS-A.2) ...))
  (OBJECT-TYPE-B
   ((OBJECT-NAME-B.1 OBJECT-PROPS-B.1)
    (OBJECT-NAME-B.2 OBJECT-PROPS-B.2) ...))
  ...).

OBJECT-TYPEs could be \"Section\", \"Table\", \"Figure\" etc.
OBJECT-PROPS is (typically) a plist created by passing
\"#+ATTR_ODT: \" option to `org-odt-parse-block-attributes'.

Use `org-odt-add-automatic-style' to add update this variable.'")

(defvar org-odt-object-counters nil
  "Running counters for various OBJECT-TYPEs.
Use this to generate automatic names and style-names. See
`org-odt-add-automatic-style'.")

(defvar org-odt-src-block-paragraph-format
  "<style:style style:name=\"OrgSrcBlock\" style:family=\"paragraph\" style:parent-style-name=\"Preformatted_20_Text\">
   <style:paragraph-properties fo:background-color=\"%s\" fo:padding=\"0.049cm\" fo:border=\"0.51pt solid #000000\" style:shadow=\"none\">
    <style:background-image/>
   </style:paragraph-properties>
   <style:text-properties fo:color=\"%s\"/>
  </style:style>"
  "Custom paragraph style for colorized source and example blocks.
This style is much the same as that of \"OrgFixedWidthBlock\"
except that the foreground and background colors are set
according to the default face identified by the `htmlfontify'.")

(defvar hfy-optimizations)
(defvar org-odt-embedded-formulas-count 0)
(defvar org-odt-embedded-images-count 0)
(defvar org-odt-image-size-probe-method
  (append (and (executable-find "identify") '(imagemagick)) ; See Bug#10675
	  '(emacs fixed))
  "Ordered list of methods for determining image sizes.")

(defvar org-odt-default-image-sizes-alist
  '(("as-char" . (5 . 0.4))
    ("paragraph" . (5 . 5)))
  "Hardcoded image dimensions one for each of the anchor
  methods.")

;; A4 page size is 21.0 by 29.7 cms
;; The default page settings has 2cm margin on each of the sides. So
;; the effective text area is 17.0 by 25.7 cm
(defvar org-odt-max-image-size '(17.0 . 20.0)
  "Limiting dimensions for an embedded image.")

(defconst org-odt-label-styles
  '(("math-formula" "%c" "text" "(%n)")
    ("math-label" "(%n)" "text" "(%n)")
    ("category-and-value" "%e %n: %c" "category-and-value" "%e %n")
    ("value" "%e %n: %c" "value" "%n"))
  "Specify how labels are applied and referenced.

This is an alist where each element is of the form:

  (STYLE-NAME ATTACH-FMT REF-MODE REF-FMT)

ATTACH-FMT controls how labels and captions are attached to an
entity.  It may contain following specifiers - %e and %c.  %e is
replaced with the CATEGORY-NAME.  %n is replaced with
\"<text:sequence ...> SEQNO </text:sequence>\".  %c is replaced
with CAPTION.

REF-MODE and REF-FMT controls how label references are generated.
The following XML is generated for a label reference -
\"<text:sequence-ref text:reference-format=\"REF-MODE\" ...>
REF-FMT </text:sequence-ref>\".  REF-FMT may contain following
specifiers - %e and %n.  %e is replaced with the CATEGORY-NAME.
%n is replaced with SEQNO.

See also `org-odt-format-label'.")

(defvar org-odt-category-map-alist
  '(("__Table__" "Table" "value" "Table" org-odt--enumerable-p)
    ("__Figure__" "Illustration" "value" "Figure" org-odt--enumerable-image-p)
    ("__MathFormula__" "Text" "math-formula" "Equation" org-odt--enumerable-formula-p)
    ("__DvipngImage__" "Equation" "value" "Equation" org-odt--enumerable-latex-image-p)
    ("__Listing__" "Listing" "value" "Listing" org-odt--enumerable-p))
  "Map a CATEGORY-HANDLE to OD-VARIABLE and LABEL-STYLE.

This is a list where each entry is of the form:

  (CATEGORY-HANDLE OD-VARIABLE LABEL-STYLE CATEGORY-NAME ENUMERATOR-PREDICATE)

CATEGORY_HANDLE identifies the captionable entity in question.

OD-VARIABLE is the OpenDocument sequence counter associated with
the entity.  These counters are declared within
\"<text:sequence-decls>...</text:sequence-decls>\" block of
`org-odt-content-template-file'.

LABEL-STYLE is a key into `org-odt-label-styles' and specifies
how a given entity should be captioned and referenced.

CATEGORY-NAME is used for qualifying captions on export.

ENUMERATOR-PREDICATE is used for assigning a sequence number to
the entity.  See `org-odt--enumerate'.")

(defvar org-odt-manifest-file-entries nil)
(defvar hfy-user-sheet-assoc)

(defvar org-odt-zip-dir nil
  "Temporary work directory for OpenDocument exporter.")



;;; User Configuration Variables

(defgroup org-export-odt nil
  "Options for exporting Org mode files to ODT."
  :tag "Org Export ODT"
  :group 'org-export)


;;;; Debugging

(defcustom org-odt-prettify-xml nil
  "Specify whether or not the xml output should be prettified.
When this option is turned on, `indent-region' is run on all
component xml buffers before they are saved.  Turn this off for
regular use.  Turn this on if you need to examine the xml
visually."
  :group 'org-export-odt
  :version "24.1"
  :type 'boolean)


;;;; Document schema

(require 'rng-loc)
(defcustom org-odt-schema-dir
  (cl-find-if
   (lambda (dir)
     (and dir
	  (file-expand-wildcards
	   (expand-file-name "od-manifest-schema*.rnc" dir))
	  (file-expand-wildcards (expand-file-name "od-schema*.rnc" dir))
	  (file-readable-p (expand-file-name "schemas.xml" dir))))
   org-odt-schema-dir-list)
  "Directory that contains OpenDocument schema files.

This directory contains:
1. rnc files for OpenDocument schema
2. a \"schemas.xml\" file that specifies locating rules needed
   for auto validation of OpenDocument XML files.

Use the customize interface to set this variable.  This ensures
that `rng-schema-locating-files' is updated and auto-validation
of OpenDocument XML takes place based on the value
`rng-nxml-auto-validate-flag'.

The default value of this variable varies depending on the
version of org in use and is initialized from
`org-odt-schema-dir-list'.  The OASIS schema files are available
only in the org's private git repository.  It is *not* bundled
with GNU ELPA tar or standard Emacs distribution."
  :type '(choice
	  (const :tag "Not set" nil)
	  (directory :tag "Schema directory"))
  :group 'org-export-odt
  :version "24.1"
  :set
  (lambda (var value)
    "Set `org-odt-schema-dir'.
Also add it to `rng-schema-locating-files'."
    (let ((schema-dir value))
      (set var
	   (if (and
		(file-expand-wildcards
		 (expand-file-name "od-manifest-schema*.rnc" schema-dir))
		(file-expand-wildcards
		 (expand-file-name "od-schema*.rnc" schema-dir))
		(file-readable-p
		 (expand-file-name "schemas.xml" schema-dir)))
	       schema-dir
	     (when value
	       (message "Error (ox-odt): %s has no OpenDocument schema files"
			value))
	     nil)))
    (when org-odt-schema-dir
      (eval-after-load 'rng-loc
	'(add-to-list 'rng-schema-locating-files
		      (expand-file-name "schemas.xml"
					org-odt-schema-dir))))))


;;;; Document styles

(defcustom org-odt-content-template-file nil
  "Template file for \"content.xml\".
The exporter embeds the exported content just before
\"</office:text>\" element.

If unspecified, the file named \"OrgOdtContentTemplate.xml\"
under `org-odt-styles-dir' is used."
  :type '(choice (const nil)
		 (file))
  :group 'org-export-odt
  :version "24.3")

(defcustom org-odt-styles-file nil
  "Default styles file for use with ODT export.
Valid values are one of:
1. nil
2. path to a styles.xml file
3. path to a *.odt or a *.ott file
4. list of the form (ODT-OR-OTT-FILE (FILE-MEMBER-1 FILE-MEMBER-2
...))

In case of option 1, an in-built styles.xml is used. See
`org-odt-styles-dir' for more information.

In case of option 3, the specified file is unzipped and the
styles.xml embedded therein is used.

In case of option 4, the specified ODT-OR-OTT-FILE is unzipped
and FILE-MEMBER-1, FILE-MEMBER-2 etc are copied in to the
generated odt file.  Use relative path for specifying the
FILE-MEMBERS.  styles.xml must be specified as one of the
FILE-MEMBERS.

Use options 1, 2 or 3 only if styles.xml alone suffices for
achieving the desired formatting.  Use option 4, if the styles.xml
references additional files like header and footer images for
achieving the desired formatting.

Use \"#+ODT_STYLES_FILE: ...\" directive to set this variable on
a per-file basis.  For example,

#+ODT_STYLES_FILE: \"/path/to/styles.xml\" or
#+ODT_STYLES_FILE: (\"/path/to/file.ott\" (\"styles.xml\" \"image/hdr.png\"))."
  :group 'org-export-odt
  :version "24.1"
  :type
  '(choice
    (const :tag "Factory settings" nil)
    (file :must-match t :tag "styles.xml")
    (file :must-match t :tag "ODT or OTT file")
    (list :tag "ODT or OTT file + Members"
	  (file :must-match t :tag "ODF Text or Text Template file")
	  (cons :tag "Members"
		(file :tag "	Member" "styles.xml")
		(repeat (file :tag "Member"))))))

(defcustom org-odt-display-outline-level 2
  "Outline levels considered for enumerating captioned entities."
  :group 'org-export-odt
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'integer)

;;;; Document conversion

(defcustom org-odt-convert-processes
  '(("LibreOffice"
     "soffice --headless --convert-to %f%x --outdir %d %i")
    ("unoconv"
     "unoconv -f %f -o %d %i"))
  "Specify a list of document converters and their usage.
The converters in this list are offered as choices while
customizing `org-odt-convert-process'.

This variable is a list where each element is of the
form (CONVERTER-NAME CONVERTER-CMD).  CONVERTER-NAME is the name
of the converter.  CONVERTER-CMD is the shell command for the
converter and can contain format specifiers.  These format
specifiers are interpreted as below:

%i input file name in full
%I input file name as a URL
%f format of the output file
%o output file name in full
%O output file name as a URL
%d output dir in full
%D output dir as a URL.
%x extra options as set in `org-odt-convert-capabilities'."
  :group 'org-export-odt
  :version "24.1"
  :type
  '(choice
    (const :tag "None" nil)
    (alist :tag "Converters"
	   :key-type (string :tag "Converter Name")
	   :value-type (group (string :tag "Command line")))))

(defcustom org-odt-convert-process "LibreOffice"
  "Use this converter to convert from \"odt\" format to other formats.
During customization, the list of converter names are populated
from `org-odt-convert-processes'."
  :group 'org-export-odt
  :version "24.1"
  :type '(choice :convert-widget
		 (lambda (w)
		   (apply 'widget-convert (widget-type w)
			  (eval (car (widget-get w :args)))))
		 `((const :tag "None" nil)
		   ,@(mapcar (lambda (c)
			       `(const :tag ,(car c) ,(car c)))
			     org-odt-convert-processes))))

(defcustom org-odt-convert-capabilities
  '(("Text"
     ("odt" "ott" "doc" "rtf" "docx")
     (("pdf" "pdf") ("odt" "odt") ("rtf" "rtf") ("ott" "ott")
      ("doc" "doc" ":\"MS Word 97\"") ("docx" "docx") ("html" "html")))
    ("Web"
     ("html")
     (("pdf" "pdf") ("odt" "odt") ("html" "html")))
    ("Spreadsheet"
     ("ods" "ots" "xls" "csv" "xlsx")
     (("pdf" "pdf") ("ots" "ots") ("html" "html") ("csv" "csv") ("ods" "ods")
      ("xls" "xls") ("xlsx" "xlsx")))
    ("Presentation"
     ("odp" "otp" "ppt" "pptx")
     (("pdf" "pdf") ("swf" "swf") ("odp" "odp") ("otp" "otp") ("ppt" "ppt")
      ("pptx" "pptx") ("odg" "odg"))))
  "Specify input and output formats of `org-odt-convert-process'.
More correctly, specify the set of input and output formats that
the user is actually interested in.

This variable is an alist where each element is of the
form (DOCUMENT-CLASS INPUT-FMT-LIST OUTPUT-FMT-ALIST).
INPUT-FMT-LIST is a list of INPUT-FMTs.  OUTPUT-FMT-ALIST is an
alist where each element is of the form (OUTPUT-FMT
OUTPUT-FILE-EXTENSION EXTRA-OPTIONS).

The variable is interpreted as follows:
`org-odt-convert-process' can take any document that is in
INPUT-FMT-LIST and produce any document that is in the
OUTPUT-FMT-LIST.  A document converted to OUTPUT-FMT will have
OUTPUT-FILE-EXTENSION as the file name extension.  OUTPUT-FMT
serves dual purposes:
- It is used for populating completion candidates during
  `org-odt-convert' commands.
- It is used as the value of \"%f\" specifier in
  `org-odt-convert-process'.

EXTRA-OPTIONS is used as the value of \"%x\" specifier in
`org-odt-convert-process'.

DOCUMENT-CLASS is used to group a set of file formats in
INPUT-FMT-LIST in to a single class.

Note that this variable inherently captures how LibreOffice based
converters work.  LibreOffice maps documents of various formats
to classes like Text, Web, Spreadsheet, Presentation etc and
allow document of a given class (irrespective of its source
format) to be converted to any of the export formats associated
with that class.

See default setting of this variable for an typical
configuration."
  :group 'org-export-odt
  :version "24.1"
  :type
  '(choice
    (const :tag "None" nil)
    (alist :tag "Capabilities"
	   :key-type (string :tag "Document Class")
	   :value-type
	   (group (repeat :tag "Input formats" (string :tag "Input format"))
		  (alist :tag "Output formats"
			 :key-type (string :tag "Output format")
			 :value-type
			 (group (string :tag "Output file extension")
				(choice
				 (const :tag "None" nil)
				 (string :tag "Extra options"))))))))

(defcustom org-odt-preferred-output-format nil
  "Automatically post-process to this format after exporting to \"odt\".
Command `org-odt-export-to-odt' exports first to \"odt\" format
and then uses `org-odt-convert-process' to convert the
resulting document to this format.  During customization of this
variable, the list of valid values are populated based on
`org-odt-convert-capabilities'.

You can set this option on per-file basis using file local
values.  See Info node `(emacs) File Variables'."
  :group 'org-export-odt
  :version "24.1"
  :type '(choice :convert-widget
		 (lambda (w)
		   (apply 'widget-convert (widget-type w)
			  (eval (car (widget-get w :args)))))
		 `((const :tag "None" nil)
		   ,@(mapcar (lambda (c)
			       `(const :tag ,c ,c))
			     (org-odt-reachable-formats "odt")))))
;;;###autoload
(put 'org-odt-preferred-output-format 'safe-local-variable 'stringp)


;;;; Drawers

(defcustom org-odt-format-drawer-function (lambda (_name contents) contents)
  "Function called to format a drawer in ODT code.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.

The function should return the string to be exported.

The default value simply returns the value of CONTENTS."
  :group 'org-export-odt
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'function)


;;;; Headline

(defcustom org-odt-format-headline-function
  'org-odt-format-headline-default-function
  "Function to format headline text.

This function will be called with 5 arguments:
TODO      the todo keyword (string or nil).
TODO-TYPE the type of todo (symbol: `todo', `done', nil)
PRIORITY  the priority of the headline (integer or nil)
TEXT      the main headline text (string).
TAGS      the tags string, separated with colons (string or nil).

The function result will be used as headline text."
  :group 'org-export-odt
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'function)


;;;; Inlinetasks

(defcustom org-odt-format-inlinetask-function
  'org-odt-format-inlinetask-default-function
  "Function called to format an inlinetask in ODT code.

The function must accept six parameters:
  TODO      the todo keyword, as a string
  TODO-TYPE the todo type, a symbol among `todo', `done' and nil.
  PRIORITY  the inlinetask priority, as a string
  NAME      the inlinetask name, as a string.
  TAGS      the inlinetask tags, as a string.
  CONTENTS  the contents of the inlinetask, as a string.

The function should return the string to be exported."
  :group 'org-export-odt
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'function)


;;;; LaTeX

(defcustom org-odt-with-latex org-export-with-latex
  "Non-nil means process LaTeX math snippets.

When set, the exporter will process LaTeX environments and
fragments.

This option can also be set with the +OPTIONS line,
e.g. \"tex:mathjax\".  Allowed values are:

nil            Ignore math snippets.
`verbatim'     Keep everything in verbatim
`dvipng'       Process the LaTeX fragments to images.  This will also
               include processing of non-math environments.
`imagemagick'  Convert the LaTeX fragments to pdf files and use
               imagemagick to convert pdf files to png files.
`mathjax'      Do MathJax preprocessing and arrange for MathJax.js to
               be loaded.
t              Synonym for `mathjax'."
  :group 'org-export-odt
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Do not process math in any way" nil)
	  (const :tag "Use dvipng to make images" dvipng)
	  (const :tag "Use imagemagick to make images" imagemagick)
	  (const :tag "Use MathJax to display math" mathjax)
	  (const :tag "Leave math verbatim" verbatim)))


;;;; Links

(defcustom org-odt-inline-formula-rules
  '(("file" . "\\.\\(mathml\\|mml\\|odf\\)\\'"))
  "Rules characterizing formula files that can be inlined into ODT.

A rule consists in an association whose key is the type of link
to consider, and value is a regexp that will be matched against
link's path."
  :group 'org-export-odt
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(alist :key-type (string :tag "Type")
		:value-type (regexp :tag "Path")))

(defcustom org-odt-inline-image-rules
  '(("file" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'"))
  "Rules characterizing image files that can be inlined into ODT.

A rule consists in an association whose key is the type of link
to consider, and value is a regexp that will be matched against
link's path."
  :group 'org-export-odt
  :version "26.1"
  :package-version '(Org . "8.3")
  :type '(alist :key-type (string :tag "Type")
		:value-type (regexp :tag "Path")))

(defcustom org-odt-pixels-per-inch 96.0
  "Scaling factor for converting images pixels to inches.
Use this for sizing of embedded images.  See Info node `(org)
Images in ODT export' for more information."
  :type 'float
  :group 'org-export-odt
  :version "24.4"
  :package-version '(Org . "8.1"))


;;;; Src Block

(defcustom org-odt-create-custom-styles-for-srcblocks t
  "Whether custom styles for colorized source blocks be automatically created.
When this option is turned on, the exporter creates custom styles
for source blocks based on the advice of `htmlfontify'.  Creation
of custom styles happen as part of `org-odt-hfy-face-to-css'.

When this option is turned off exporter does not create such
styles.

Use the latter option if you do not want the custom styles to be
based on your current display settings.  It is necessary that the
styles.xml already contains needed styles for colorizing to work.

This variable is effective only if `org-odt-fontify-srcblocks' is
turned on."
  :group 'org-export-odt
  :version "24.1"
  :type 'boolean)

(defcustom org-odt-fontify-srcblocks t
  "Specify whether or not source blocks need to be fontified.
Turn this option on if you want to colorize the source code
blocks in the exported file.  For colorization to work, you need
to make available an enhanced version of `htmlfontify' library."
  :type 'boolean
  :group 'org-export-odt
  :version "24.1")


;;;; Table

(defcustom org-odt-table-styles
  '(("OrgEquation" "OrgEquation"
     ((use-first-column-styles . t)
      (use-last-column-styles . t)))
    ("TableWithHeaderRowAndColumn" "Custom"
     ((use-first-row-styles . t)
      (use-first-column-styles . t)))
    ("TableWithFirstRowandLastRow" "Custom"
     ((use-first-row-styles . t)
      (use-last-row-styles . t)))
    ("GriddedTable" "Custom" nil))
  "Specify how Table Styles should be derived from a Table Template.
This is a list where each element is of the
form (TABLE-STYLE-NAME TABLE-TEMPLATE-NAME TABLE-CELL-OPTIONS).

TABLE-STYLE-NAME is the style associated with the table through
\"#+ATTR_ODT: :style TABLE-STYLE-NAME\" line.

TABLE-TEMPLATE-NAME is a set of - upto 9 - automatic
TABLE-CELL-STYLE-NAMEs and PARAGRAPH-STYLE-NAMEs (as defined
below) that is included in `org-odt-content-template-file'.

TABLE-CELL-STYLE-NAME := TABLE-TEMPLATE-NAME + TABLE-CELL-TYPE +
                         \"TableCell\"
PARAGRAPH-STYLE-NAME  := TABLE-TEMPLATE-NAME + TABLE-CELL-TYPE +
                         \"TableParagraph\"
TABLE-CELL-TYPE       := \"FirstRow\"   | \"LastColumn\" |
                         \"FirstRow\"   | \"LastRow\"    |
                         \"EvenRow\"    | \"OddRow\"     |
                         \"EvenColumn\" | \"OddColumn\"  | \"\"
where \"+\" above denotes string concatenation.

TABLE-CELL-OPTIONS is an alist where each element is of the
form (TABLE-CELL-STYLE-SELECTOR . ON-OR-OFF).
TABLE-CELL-STYLE-SELECTOR := `use-first-row-styles'       |
                             `use-last-row-styles'        |
                             `use-first-column-styles'    |
                             `use-last-column-styles'     |
                             `use-banding-rows-styles'    |
                             `use-banding-columns-styles' |
                             `use-first-row-styles'
ON-OR-OFF                 := t | nil

For example, with the following configuration

\(setq org-odt-table-styles
      \\='((\"TableWithHeaderRowsAndColumns\" \"Custom\"
         ((use-first-row-styles . t)
          (use-first-column-styles . t)))
        (\"TableWithHeaderColumns\" \"Custom\"
         ((use-first-column-styles . t)))))

1. A table associated with \"TableWithHeaderRowsAndColumns\"
   style will use the following table-cell styles -
   \"CustomFirstRowTableCell\", \"CustomFirstColumnTableCell\",
   \"CustomTableCell\" and the following paragraph styles
   \"CustomFirstRowTableParagraph\",
   \"CustomFirstColumnTableParagraph\", \"CustomTableParagraph\"
   as appropriate.

2. A table associated with \"TableWithHeaderColumns\" style will
   use the following table-cell styles -
   \"CustomFirstColumnTableCell\", \"CustomTableCell\" and the
   following paragraph styles
   \"CustomFirstColumnTableParagraph\", \"CustomTableParagraph\"
   as appropriate..

Note that TABLE-TEMPLATE-NAME corresponds to the
\"<table:table-template>\" elements contained within
\"<office:styles>\".  The entries (TABLE-STYLE-NAME
TABLE-TEMPLATE-NAME TABLE-CELL-OPTIONS) correspond to
\"table:template-name\" and \"table:use-first-row-styles\" etc
attributes of \"<table:table>\" element.  Refer ODF-1.2
specification for more information.  Also consult the
implementation filed under `org-odt-get-table-cell-styles'.

The TABLE-STYLE-NAME \"OrgEquation\" is used internally for
formatting of numbered display equations.  Do not delete this
style from the list."
  :group 'org-export-odt
  :version "24.1"
  :type '(choice
          (const :tag "None" nil)
          (repeat :tag "Table Styles"
                  (list :tag "Table Style Specification"
			(string :tag "Table Style Name")
			(string  :tag "Table Template Name")
			(alist :options (use-first-row-styles
					 use-last-row-styles
					 use-first-column-styles
					 use-last-column-styles
					 use-banding-rows-styles
					 use-banding-columns-styles)
			       :key-type symbol
			       :value-type (const :tag "True" t))))))

;;;; Timestamps

(defcustom org-odt-use-date-fields nil
  "Non-nil, if timestamps should be exported as date fields.

When nil, export timestamps as plain text.

When non-nil, map `org-time-stamp-custom-formats' to a pair of
OpenDocument date-styles with names \"OrgDate1\" and \"OrgDate2\"
respectively.  A timestamp with no time component is formatted
with style \"OrgDate1\" while one with explicit hour and minutes
is formatted with style \"OrgDate2\".

This feature is experimental.  Most (but not all) of the common
%-specifiers in `format-time-string' are supported.
Specifically, locale-dependent specifiers like \"%c\", \"%x\" are
formatted as canonical Org timestamps.  For finer control, avoid
these %-specifiers.

Textual specifiers like \"%b\", \"%h\", \"%B\", \"%a\", \"%A\"
etc., are displayed by the application in the default language
and country specified in `org-odt-styles-file'.  Note that the
default styles file uses language \"en\" and country \"GB\".  You
can localize the week day and month strings in the exported
document by setting the default language and country either using
the application UI or through a custom styles file.

See `org-odt--build-date-styles' for implementation details."
  :group 'org-export-odt
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)



;;; Internal functions

;;;; Date

(defun org-odt--format-timestamp (timestamp &optional end iso-date-p)
  (let* ((format-timestamp
	  (lambda (timestamp format &optional end utc)
	    (if timestamp
		(org-timestamp-format timestamp format end utc)
	      (format-time-string format nil utc))))
	 (has-time-p (or (not timestamp)
			 (org-timestamp-has-time-p timestamp)))
	 (iso-date (let ((format (if has-time-p "%Y-%m-%dT%H:%M:%S"
				   "%Y-%m-%dT%H:%M:%S")))
		     (funcall format-timestamp timestamp format end))))
    (if iso-date-p iso-date
      (let* ((style (if has-time-p "OrgDate2" "OrgDate1"))
	     ;; LibreOffice does not care about end goes as content
	     ;; within the "<text:date>...</text:date>" field.  The
	     ;; displayed date is automagically corrected to match the
	     ;; format requested by "style:data-style-name" attribute.  So
	     ;; don't bother about formatting the date contents to be
	     ;; compatible with "OrgDate1" and "OrgDateTime" styles.  A
	     ;; simple Org-style date should suffice.
	     (date (let* ((formats
			   (if org-display-custom-times
			       (cons (substring
				      (car org-time-stamp-custom-formats) 1 -1)
				     (substring
				      (cdr org-time-stamp-custom-formats) 1 -1))
			     '("%Y-%m-%d %a" . "%Y-%m-%d %a %H:%M")))
			  (format (if has-time-p (cdr formats) (car formats))))
		     (funcall format-timestamp timestamp format end)))
	     (repeater (let ((repeater-type (org-element-property
					     :repeater-type timestamp))
			     (repeater-value (org-element-property
					      :repeater-value timestamp))
			     (repeater-unit (org-element-property
					     :repeater-unit timestamp)))
			 (concat
			  (cl-case repeater-type
			    (catchup "++") (restart ".+") (cumulate "+"))
			  (when repeater-value
			    (number-to-string repeater-value))
			  (cl-case repeater-unit
			    (hour "h") (day "d") (week "w") (month "m")
			    (year "y"))))))
	(concat
	 (format "<text:date text:date-value=\"%s\" style:data-style-name=\"%s\" text:fixed=\"true\">%s</text:date>"
		 iso-date style date)
	 (and (not (string= repeater ""))  " ")
	 repeater)))))

;;;; Frame

(defun org-odt--frame (text width height style &optional extra
			      anchor-type &rest title-and-desc)
  (let ((frame-attrs
	 (concat
	  (if width (format " svg:width=\"%0.2fcm\"" width) "")
	  (if height (format " svg:height=\"%0.2fcm\"" height) "")
	  extra
	  (format " text:anchor-type=\"%s\"" (or anchor-type "paragraph"))
	  (format " draw:name=\"%s\""
		  (car (org-odt-add-automatic-style "Frame"))))))
    (format
     "\n<draw:frame draw:style-name=\"%s\"%s>\n%s\n</draw:frame>"
     style frame-attrs
     (concat text
	     (let ((title (car title-and-desc))
		   (desc (cadr title-and-desc)))
	       (concat (when title
			 (format "<svg:title>%s</svg:title>"
				 (org-odt--encode-plain-text title t)))
		       (when desc
			 (format "<svg:desc>%s</svg:desc>"
				 (org-odt--encode-plain-text desc t)))))))))


;;;; Library wrappers

(defun org-odt--zip-extract (archive members target)
  (when (atom members) (setq members (list members)))
  (require 'arc-mode)
  (dolist (member members)
    (let* ((--quote-file-name
	    ;; This is shamelessly stolen from `archive-zip-extract'.
	    (lambda (name)
	      (if (or (not (memq system-type '(windows-nt ms-dos)))
		      (and (boundp 'w32-quote-process-args)
			   (null w32-quote-process-args)))
		  (shell-quote-argument name)
		name)))
	   (target (funcall --quote-file-name target))
	   (archive (expand-file-name archive))
	   (archive-zip-extract
	    (list "unzip" "-qq" "-o" "-d" target))
	   exit-code command-output)
      (setq command-output
	    (with-temp-buffer
	      (setq exit-code (archive-zip-extract archive member))
	      (buffer-string)))
      (unless (zerop exit-code)
	(message command-output)
	(error "Extraction failed")))))

;;;; Target

(defun org-odt--target (text id)
  (if (not id) text
    (concat
     (format "\n<text:bookmark-start text:name=\"OrgXref.%s\"/>" id)
     (format "\n<text:bookmark text:name=\"%s\"/>" id) text
     (format "\n<text:bookmark-end text:name=\"OrgXref.%s\"/>" id))))

;;;; Textbox

(defun org-odt--textbox (text width height style &optional
				extra anchor-type)
  (org-odt--frame
   (format "\n<draw:text-box %s>%s\n</draw:text-box>"
	   (concat (format " fo:min-height=\"%0.2fcm\"" (or height .2))
		   (and (not width)
			(format " fo:min-width=\"%0.2fcm\"" (or width .2))))
	   text)
   width nil style extra anchor-type))



;;;; Table of Contents

(defun org-odt--format-toc (title entries depth)
  "Return a table of contents.
TITLE is the title of the table, as a string, or nil.  ENTRIES is
the contents of the table, as a string.  DEPTH is an integer
specifying the depth of the table."
  (concat
   "
<text:table-of-content text:style-name=\"OrgIndexSection\" text:protected=\"true\" text:name=\"Table of Contents\">\n"
   (format "  <text:table-of-content-source text:outline-level=\"%d\">" depth)
   (and title
	(format "
    <text:index-title-template text:style-name=\"Contents_20_Heading\">%s</text:index-title-template>
"
		title))

   (let ((levels (number-sequence 1 10)))
     (mapconcat
      (lambda (level)
	(format
	 "
      <text:table-of-content-entry-template text:outline-level=\"%d\" text:style-name=\"Contents_20_%d\">
       <text:index-entry-link-start text:style-name=\"Internet_20_link\"/>
       <text:index-entry-chapter/>
       <text:index-entry-text/>
       <text:index-entry-link-end/>
      </text:table-of-content-entry-template>\n"
	 level level)) levels ""))
   "
  </text:table-of-content-source>
  <text:index-body>"
   (and title
	(format "
    <text:index-title text:style-name=\"Sect1\" text:name=\"Table of Contents1_Head\">
      <text:p text:style-name=\"Contents_20_Heading\">%s</text:p>
    </text:index-title>\n"
		title))
   entries
   "
  </text:index-body>
</text:table-of-content>"))

(cl-defun org-odt-format-toc-headline
    (todo _todo-type priority text tags
	  &key _level section-number headline-label &allow-other-keys)
  (format "<text:a xlink:type=\"simple\" xlink:href=\"#%s\">%s</text:a>"
	  headline-label
	  (concat
	   ;; Section number.
	   (and section-number (concat section-number ". "))
	   ;; Todo.
	   (when todo
	     (let ((style (if (member todo org-done-keywords)
			      "OrgDone" "OrgTodo")))
	       (format "<text:span text:style-name=\"%s\">%s</text:span> "
		       style todo)))
	   (when priority
	     (let* ((style (format "OrgPriority-%s" priority))
		    (priority (format "[#%c]" priority)))
	       (format "<text:span text:style-name=\"%s\">%s</text:span> "
		       style priority)))
	   ;; Title.
	   text
	   ;; Tags.
	   (when tags
	     (concat
	      (format " <text:span text:style-name=\"%s\">[%s]</text:span>"
		      "OrgTags"
		      (mapconcat
		       (lambda (tag)
			 (format
			  "<text:span text:style-name=\"%s\">%s</text:span>"
			  "OrgTag" tag)) tags " : ")))))))

(defun org-odt-toc (depth info &optional scope)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is
a plist containing current export properties.  Optional argument
SCOPE, when non-nil, defines the scope of the table.  Return the
table of contents as a string, or nil."
  (cl-assert (wholenump depth))
  ;; When a headline is marked as a radio target, as in the example below:
  ;;
  ;; ** <<<Some Heading>>>
  ;;    Some text.
  ;;
  ;; suppress generation of radio targets.  i.e., Radio targets are to
  ;; be marked as targets within /document body/ and *not* within
  ;; /TOC/, as otherwise there will be duplicated anchors one in TOC
  ;; and one in the document body.
  ;;
  ;; Likewise, links, footnote references and regular targets are also
  ;; suppressed.
  (let* ((headlines (org-export-collect-headlines info depth scope))
	 (backend (org-export-toc-entry-backend
		      (org-export-backend-name (plist-get info :back-end)))))
    (when headlines
      (org-odt--format-toc
       (and (not scope) (org-export-translate "Table of Contents" :utf-8 info))
       (mapconcat
	(lambda (headline)
	  (let* ((entry (org-odt-format-headline--wrap
			 headline backend info 'org-odt-format-toc-headline))
		 (level (org-export-get-relative-level headline info))
		 (style (format "Contents_20_%d" level)))
	    (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		    style entry)))
	headlines "\n")
       depth))))


;;;; Document styles

(defun org-odt-add-automatic-style (object-type &optional object-props)
  "Create an automatic style of type OBJECT-TYPE with param OBJECT-PROPS.
OBJECT-PROPS is (typically) a plist created by passing
\"#+ATTR_ODT: \" option of the object in question to
`org-odt-parse-block-attributes'.

Use `org-odt-object-counters' to generate an automatic
OBJECT-NAME and STYLE-NAME.  If OBJECT-PROPS is non-nil, add a
new entry in `org-odt-automatic-styles'.  Return (OBJECT-NAME
. STYLE-NAME)."
  (cl-assert (stringp object-type))
  (let* ((object (intern object-type))
	 (seqvar object)
	 (seqno (1+ (or (plist-get org-odt-object-counters seqvar) 0)))
	 (object-name (format "%s%d" object-type seqno)) style-name)
    (setq org-odt-object-counters
	  (plist-put org-odt-object-counters seqvar seqno))
    (when object-props
      (setq style-name (format "Org%s" object-name))
      (setq org-odt-automatic-styles
	    (plist-put org-odt-automatic-styles object
		       (append (list (list style-name object-props))
			       (plist-get org-odt-automatic-styles object)))))
    (cons object-name style-name)))

;;;; Checkbox

(defun org-odt--checkbox (item)
  "Return check-box string associated to ITEM."
  (let ((checkbox (org-element-property :checkbox item)))
    (if (not checkbox) ""
      (format "<text:span text:style-name=\"%s\">%s</text:span>"
	      "OrgCode" (cl-case checkbox
			  (on "[&#x2713;] ") ; CHECK MARK
			  (off "[ ] ")
			  (trans "[-] "))))))

;;; Template

(defun org-odt--build-date-styles (fmt style)
  ;; In LibreOffice 3.4.6, there doesn't seem to be a convenient way
  ;; to modify the date fields.  A date could be modified by
  ;; offsetting in days.  That's about it.  Also, date and time may
  ;; have to be emitted as two fields - a date field and a time field
  ;; - separately.

  ;; One can add Form Controls to date and time fields so that they
  ;; can be easily modified.  But then, the exported document will
  ;; become tightly coupled with LibreOffice and may not function
  ;; properly with other OpenDocument applications.

  ;; I have a strange feeling that Date styles are a bit flaky at the
  ;; moment.

  ;; The feature is experimental.
  (when (and fmt style)
    (let* ((fmt-alist
	    '(("%A" . "<number:day-of-week number:style=\"long\"/>")
	      ("%B" . "<number:month number:textual=\"true\" number:style=\"long\"/>")
	      ("%H" . "<number:hours number:style=\"long\"/>")
	      ("%M" . "<number:minutes number:style=\"long\"/>")
	      ("%S" . "<number:seconds number:style=\"long\"/>")
	      ("%V" . "<number:week-of-year/>")
	      ("%Y" . "<number:year number:style=\"long\"/>")
	      ("%a" . "<number:day-of-week number:style=\"short\"/>")
	      ("%b" . "<number:month number:textual=\"true\" number:style=\"short\"/>")
	      ("%d" . "<number:day number:style=\"long\"/>")
	      ("%e" . "<number:day number:style=\"short\"/>")
	      ("%h" . "<number:month number:textual=\"true\" number:style=\"short\"/>")
	      ("%k" . "<number:hours number:style=\"short\"/>")
	      ("%m" . "<number:month number:style=\"long\"/>")
	      ("%p" . "<number:am-pm/>")
	      ("%y" . "<number:year number:style=\"short\"/>")))
	   (case-fold-search nil)
	   (re (mapconcat 'identity (mapcar 'car fmt-alist) "\\|"))
	   match rpl (start 0) (filler-beg 0) filler-end filler output)
      (dolist (pair
	       '(("\\(?:%[[:digit:]]*N\\)" . "") ; strip ns, us and ns
		 ("%C" . "Y")		; replace century with year
		 ("%D" . "%m/%d/%y")
		 ("%G" . "Y")	      ; year corresponding to iso week
		 ("%I" . "%H")	      ; hour on a 12-hour clock
		 ("%R" . "%H:%M")
		 ("%T" . "%H:%M:%S")
		 ("%U\\|%W" . "%V")   ; week no. starting on Sun./Mon.
		 ("%Z" . "")	      ; time zone name
		 ("%c" . "%Y-%M-%d %a %H:%M" ) ; locale's date and time format
		 ("%g" . "%y")
		 ("%X" . "%x" )		; locale's pref. time format
		 ("%j" . "")		; day of the year
		 ("%l" . "%k")		; like %I blank-padded
		 ("%s" . "") ; no. of secs since 1970-01-01 00:00:00 +0000
		 ("%n" . "<text:line-break/>")
		 ("%r" . "%I:%M:%S %p")
		 ("%t" . "<text:tab/>")
		 ("%u\\|%w" . "") ; numeric day of week - Mon (1-7), Sun(0-6)
		 ("%x" . "%Y-%M-%d %a")	; locale's pref. time format
		 ("%z" . "")		; time zone in numeric form
		 ))
	(setq fmt (replace-regexp-in-string (car pair) (cdr pair) fmt t t)))
      (while (string-match re fmt start)
	(setq match (match-string 0 fmt))
	(setq rpl (assoc-default match fmt-alist))
	(setq start (match-end 0))
	(setq filler-end (match-beginning 0))
	(setq filler (substring fmt (prog1 filler-beg
				      (setq filler-beg (match-end 0)))
				filler-end))
	(setq filler (and (not (string= filler ""))
			  (format "<number:text>%s</number:text>"
				  (org-odt--encode-plain-text filler))))
	(setq output (concat output "\n" filler "\n" rpl)))
      (setq filler (substring fmt filler-beg))
      (unless (string= filler "")
	(setq output (concat output
			     (format "\n<number:text>%s</number:text>"
				     (org-odt--encode-plain-text filler)))))
      (format "\n<number:date-style style:name=\"%s\" %s>%s\n</number:date-style>"
	      style
	      (concat " number:automatic-order=\"true\""
		      " number:format-source=\"fixed\"")
	      output ))))

(defun org-odt-template (contents info)
  "Return complete document string after ODT conversion.
CONTENTS is the transcoded contents string.  RAW-DATA is the
original parsed data.  INFO is a plist holding export options."
  ;; Write meta file.
  (let ((title (org-export-data (plist-get info :title) info))
	(subtitle (org-export-data (plist-get info :subtitle) info))
	(author (let ((author (plist-get info :author)))
		  (if (not author) "" (org-export-data author info))))
	(keywords (or (plist-get info :keywords) ""))
	(description (or (plist-get info :description) "")))
    (write-region
     (concat
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
     <office:document-meta
         xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\"
         xmlns:xlink=\"http://www.w3.org/1999/xlink\"
         xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
         xmlns:meta=\"urn:oasis:names:tc:opendocument:xmlns:meta:1.0\"
         xmlns:ooo=\"http://openoffice.org/2004/office\"
         office:version=\"1.2\">
       <office:meta>\n"
      (format "<dc:creator>%s</dc:creator>\n" author)
      (format "<meta:initial-creator>%s</meta:initial-creator>\n" author)
      ;; Date, if required.
      (when (plist-get info :with-date)
	;; Check if DATE is specified as an Org-timestamp.  If yes,
	;; include it as meta information.  Otherwise, just use
	;; today's date.
	(let* ((date (let ((date (plist-get info :date)))
		       (and (not (cdr date))
			    (eq (org-element-type (car date)) 'timestamp)
			    (car date)))))
	  (let ((iso-date (org-odt--format-timestamp date nil 'iso-date)))
	    (concat
	     (format "<dc:date>%s</dc:date>\n" iso-date)
	     (format "<meta:creation-date>%s</meta:creation-date>\n"
		     iso-date)))))
      (format "<meta:generator>%s</meta:generator>\n"
	      (plist-get info :creator))
      (format "<meta:keyword>%s</meta:keyword>\n" keywords)
      (format "<dc:subject>%s</dc:subject>\n" description)
      (format "<dc:title>%s</dc:title>\n" title)
      (when (org-string-nw-p subtitle)
	(format
	 "<meta:user-defined meta:name=\"subtitle\">%s</meta:user-defined>\n"
	 subtitle))
      "\n"
      "  </office:meta>\n" "</office:document-meta>")
     nil (concat org-odt-zip-dir "meta.xml"))
    ;; Add meta.xml in to manifest.
    (org-odt-create-manifest-file-entry "text/xml" "meta.xml"))

  ;; Update styles file.
  ;; Copy styles.xml.  Also dump htmlfontify styles, if there is any.
  ;; Write styles file.
  (let* ((styles-file (plist-get info :odt-styles-file))
	 (styles-file (and (org-string-nw-p styles-file)
			   (read (org-trim styles-file))))
	 ;; Non-availability of styles.xml is not a critical
	 ;; error. For now, throw an error.
	 (styles-file (or styles-file
			  (plist-get info :odt-styles-file)
			  (expand-file-name "OrgOdtStyles.xml"
					    org-odt-styles-dir)
			  (error "org-odt: Missing styles file?"))))
    (cond
     ((listp styles-file)
      (let ((archive (nth 0 styles-file))
	    (members (nth 1 styles-file)))
	(org-odt--zip-extract archive members org-odt-zip-dir)
	(dolist (member members)
	  (when (org-file-image-p member)
	    (let* ((image-type (file-name-extension member))
		   (media-type (format "image/%s" image-type)))
	      (org-odt-create-manifest-file-entry media-type member))))))
     ((and (stringp styles-file) (file-exists-p styles-file))
      (let ((styles-file-type (file-name-extension styles-file)))
	(cond
	 ((string= styles-file-type "xml")
	  (copy-file styles-file (concat org-odt-zip-dir "styles.xml") t))
	 ((member styles-file-type '("odt" "ott"))
	  (org-odt--zip-extract styles-file "styles.xml" org-odt-zip-dir)))))
     (t
      (error "Invalid specification of styles.xml file: %S"
	     (plist-get info :odt-styles-file))))

    ;; create a manifest entry for styles.xml
    (org-odt-create-manifest-file-entry "text/xml" "styles.xml")

    ;; FIXME: Who is opening an empty styles.xml before this point?
    (with-current-buffer
	(find-file-noselect (concat org-odt-zip-dir "styles.xml") t)
      (revert-buffer t t)

      ;; Write custom styles for source blocks
      ;; Save STYLES used for colorizing of source blocks.
      ;; Update styles.xml with styles that were collected as part of
      ;; `org-odt-hfy-face-to-css' callbacks.
      (let ((styles (mapconcat (lambda (style) (format " %s\n" (cddr style)))
			       hfy-user-sheet-assoc "")))
	(when styles
	  (goto-char (point-min))
	  (when (re-search-forward "</office:styles>" nil t)
	    (goto-char (match-beginning 0))
	    (insert "\n<!-- Org Htmlfontify Styles -->\n" styles "\n"))))

      ;; Update styles.xml - take care of outline numbering

      ;; Don't make automatic backup of styles.xml file. This setting
      ;; prevents the backed-up styles.xml file from being zipped in to
      ;; odt file. This is more of a hackish fix. Better alternative
      ;; would be to fix the zip command so that the output odt file
      ;; includes only the needed files and excludes any auto-generated
      ;; extra files like backups and auto-saves etc etc. Note that
      ;; currently the zip command zips up the entire temp directory so
      ;; that any auto-generated files created under the hood ends up in
      ;; the resulting odt file.
      (setq-local backup-inhibited t)

      ;; Outline numbering is retained only upto LEVEL.
      ;; To disable outline numbering pass a LEVEL of 0.

      (goto-char (point-min))
      (let ((regex
	     "<text:outline-level-style\\([^>]*\\)text:level=\"\\([^\"]*\\)\"\\([^>]*\\)>")
	    (replacement
	     "<text:outline-level-style\\1text:level=\"\\2\" style:num-format=\"\">"))
	(while (re-search-forward regex nil t)
	  (unless (let ((sec-num (plist-get info :section-numbers))
			(level (string-to-number (match-string 2))))
		    (if (wholenump sec-num) (<= level sec-num) sec-num))
	    (replace-match replacement t nil))))
      (save-buffer 0)))
  ;; Update content.xml.

  (let* ( ;; `org-display-custom-times' should be accessed right
	 ;; within the context of the Org buffer.  So obtain its
	 ;; value before moving on to temp-buffer context down below.
	 (custom-time-fmts
	  (if org-display-custom-times
	      (cons (substring (car org-time-stamp-custom-formats) 1 -1)
		    (substring (cdr org-time-stamp-custom-formats) 1 -1))
	    '("%Y-%M-%d %a" . "%Y-%M-%d %a %H:%M"))))
    (with-temp-buffer
      (insert-file-contents
       (or (plist-get info :odt-content-template-file)
	   (expand-file-name "OrgOdtContentTemplate.xml"
			     org-odt-styles-dir)))
      ;; Write automatic styles.
      ;; - Position the cursor.
      (goto-char (point-min))
      (re-search-forward "  </office:automatic-styles>" nil t)
      (goto-char (match-beginning 0))
      ;; - Dump automatic table styles.
      (cl-loop for (style-name props) in
	       (plist-get org-odt-automatic-styles 'Table) do
	       (when (setq props (or (plist-get props :rel-width) "96"))
		 (insert (format org-odt-table-style-format style-name props))))
      ;; - Dump date-styles.
      (when (plist-get info :odt-use-date-fields)
	(insert (org-odt--build-date-styles (car custom-time-fmts)
					    "OrgDate1")
		(org-odt--build-date-styles (cdr custom-time-fmts)
					    "OrgDate2")))
      ;; Update display level.
      ;; - Remove existing sequence decls.  Also position the cursor.
      (goto-char (point-min))
      (when (re-search-forward "<text:sequence-decls" nil t)
	(delete-region (match-beginning 0)
		       (re-search-forward "</text:sequence-decls>" nil nil)))
      ;; Update sequence decls according to user preference.
      (insert
       (format
	"\n<text:sequence-decls>\n%s\n</text:sequence-decls>"
	(mapconcat
	 (lambda (x)
	   (format
	    "<text:sequence-decl text:display-outline-level=\"%d\" text:name=\"%s\"/>"
	    (plist-get info :odt-display-outline-level)
	    (nth 1 x)))
	 org-odt-category-map-alist "\n")))
      ;; Position the cursor to document body.
      (goto-char (point-min))
      (re-search-forward "</office:text>" nil nil)
      (goto-char (match-beginning 0))

      ;; Preamble - Title, Author, Date etc.
      (insert
       (let* ((title (and (plist-get info :with-title)
			  (org-export-data (plist-get info :title) info)))
	      (subtitle (when title
			  (org-export-data (plist-get info :subtitle) info)))
	      (author (and (plist-get info :with-author)
			   (let ((auth (plist-get info :author)))
			     (and auth (org-export-data auth info)))))
	      (email (plist-get info :email))
	      ;; Switch on or off above vars based on user settings
	      (author (and (plist-get info :with-author) (or author email)))
	      (email (and (plist-get info :with-email) email)))
	 (concat
	  ;; Title.
	  (when (org-string-nw-p title)
	    (concat
	     (format "\n<text:p text:style-name=\"%s\">%s</text:p>\n"
		     "OrgTitle" (format "\n<text:title>%s</text:title>" title))
	     ;; Separator.
	     "\n<text:p text:style-name=\"OrgTitle\"/>\n"
	     ;; Subtitle.
	     (when (org-string-nw-p subtitle)
	       (concat
		(format "<text:p text:style-name=\"OrgSubtitle\">\n%s\n</text:p>\n"
			(concat
			 "<text:user-defined style:data-style-name=\"N0\" text:name=\"subtitle\">\n"
			 subtitle
			 "</text:user-defined>\n"))
		;; Separator.
		"<text:p text:style-name=\"OrgSubtitle\"/>\n"))))
	  (cond
	   ((and author (not email))
	    ;; Author only.
	    (concat
	     (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		     "OrgSubtitle"
		     (format "<text:initial-creator>%s</text:initial-creator>" author))
	     ;; Separator.
	     "\n<text:p text:style-name=\"OrgSubtitle\"/>"))
	   ((and author email)
	    ;; Author and E-mail.
	    (concat
	     (format
	      "\n<text:p text:style-name=\"%s\">%s</text:p>"
	      "OrgSubtitle"
	      (format
	       "<text:a xlink:type=\"simple\" xlink:href=\"%s\">%s</text:a>"
	       (concat "mailto:" email)
	       (format "<text:initial-creator>%s</text:initial-creator>" author)))
	     ;; Separator.
	     "\n<text:p text:style-name=\"OrgSubtitle\"/>")))
	  ;; Date, if required.
	  (when (plist-get info :with-date)
	    (let* ((date (plist-get info :date))
		   ;; Check if DATE is specified as a timestamp.
		   (timestamp (and (not (cdr date))
				   (eq (org-element-type (car date)) 'timestamp)
				   (car date))))
	      (when date
		(concat
		 (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
			 "OrgSubtitle"
			 (if (and (plist-get info :odt-use-date-fields) timestamp)
			     (org-odt--format-timestamp (car date))
			   (org-export-data date info)))
		 ;; Separator
		 "<text:p text:style-name=\"OrgSubtitle\"/>")))))))
      ;; Table of Contents
      (let* ((with-toc (plist-get info :with-toc))
	     (depth (and with-toc (if (wholenump with-toc)
				      with-toc
				    (plist-get info :headline-levels)))))
	(when depth (insert (or (org-odt-toc depth info) ""))))
      ;; Contents.
      (insert contents)
      ;; Return contents.
      (buffer-substring-no-properties (point-min) (point-max)))))



;;; Transcode Functions

;;;; Bold

(defun org-odt-bold (_bold contents _info)
  "Transcode BOLD from Org to ODT.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "Bold" contents))


;;;; Center Block

(defun org-odt-center-block (_center-block contents _info)
  "Transcode a CENTER-BLOCK element from Org to ODT.
CONTENTS holds the contents of the center block.  INFO is a plist
holding contextual information."
  contents)


;;;; Clock

(defun org-odt-clock (clock contents info)
  "Transcode a CLOCK element from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((timestamp (org-element-property :value clock))
	(duration (org-element-property :duration clock)))
    (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
	    (if (eq (org-element-type (org-export-get-next-element clock info))
		    'clock) "OrgClock" "OrgClockLastLine")
	    (concat
	     (format "<text:span text:style-name=\"%s\">%s</text:span>"
		     "OrgClockKeyword" org-clock-string)
	     (org-odt-timestamp timestamp contents info)
	     (and duration (format " (%s)" duration))))))


;;;; Code

(defun org-odt-code (code _contents _info)
  "Transcode a CODE object from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "OrgCode" (org-odt--encode-plain-text
		     (org-element-property :value code))))


;;;; Drawer

(defun org-odt-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to ODT.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((name (org-element-property :drawer-name drawer))
	 (output (funcall (plist-get info :odt-format-drawer-function)
			  name contents)))
    output))


;;;; Dynamic Block

(defun org-odt-dynamic-block (_dynamic-block contents _info)
  "Transcode a DYNAMIC-BLOCK element from Org to ODT.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  contents)


;;;; Entity

(defun org-odt-entity (entity _contents _info)
  "Transcode an ENTITY object from Org to ODT.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (org-element-property :utf-8 entity))


;;;; Example Block

(defun org-odt-example-block (example-block _contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-odt-format-code example-block info))


;;;; Export Snippet

(defun org-odt-export-snippet (export-snippet _contents _info)
  "Transcode a EXPORT-SNIPPET object from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'odt)
    (org-element-property :value export-snippet)))


;;;; Export Block

(defun org-odt-export-block (export-block _contents _info)
  "Transcode a EXPORT-BLOCK element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "ODT")
    (org-remove-indentation (org-element-property :value export-block))))


;;;; Fixed Width

(defun org-odt-fixed-width (fixed-width _contents info)
  "Transcode a FIXED-WIDTH element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-odt-do-format-code (org-element-property :value fixed-width) info))


;;;; Footnote Definition

;; Footnote Definitions are ignored.


;;;; Footnote Reference

(defun org-odt-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((--format-footnote-definition
	 (lambda (n def)
	   (setq n (format "%d" n))
	   (let ((id (concat  "fn" n))
		 (note-class "footnote"))
	     (format
	      "<text:note text:id=\"%s\" text:note-class=\"%s\">%s</text:note>"
	      id note-class
	      (concat
	       (format "<text:note-citation>%s</text:note-citation>" n)
	       (format "<text:note-body>%s</text:note-body>" def))))))
	(--format-footnote-reference
	 (lambda (n)
	   (setq n (format "%d" n))
	   (let ((note-class "footnote")
		 (ref-format "text")
		 (ref-name (concat "fn" n)))
	     (format
	      "<text:span text:style-name=\"%s\">%s</text:span>"
	      "OrgSuperscript"
	      (format "<text:note-ref text:note-class=\"%s\" text:reference-format=\"%s\" text:ref-name=\"%s\">%s</text:note-ref>"
		      note-class ref-format ref-name n))))))
    (concat
     ;; Insert separator between two footnotes in a row.
     (let ((prev (org-export-get-previous-element footnote-reference info)))
       (and (eq (org-element-type prev) 'footnote-reference)
	    (format "<text:span text:style-name=\"%s\">%s</text:span>"
		    "OrgSuperscript" ",")))
     ;; Transcode footnote reference.
     (let ((n (org-export-get-footnote-number footnote-reference info nil t)))
       (cond
	((not
	  (org-export-footnote-first-reference-p footnote-reference info nil t))
	 (funcall --format-footnote-reference n))
	(t
	 (let* ((raw (org-export-get-footnote-definition
		      footnote-reference info))
		(def
		 (let ((def (org-trim
			     (org-export-data-with-backend
			      raw
			      (org-export-create-backend
			       :parent 'odt
			       :transcoders
			       '((paragraph . (lambda (p c i)
						(org-odt--format-paragraph
						 p c i
						 "Footnote"
						 "OrgFootnoteCenter"
						 "OrgFootnoteQuotations")))))
			      info))))
		   ;; Inline definitions are secondary strings.  We
		   ;; need to wrap them within a paragraph.
		   (if (eq (org-element-class (car (org-element-contents raw)))
			   'element)
		       def
		     (format
		      "\n<text:p text:style-name=\"Footnote\">%s</text:p>"
		      def)))))
	   (funcall --format-footnote-definition n def))))))))


;;;; Headline

(defun org-odt-format-headline--wrap (headline backend info
					       &optional format-function
					       &rest extra-keys)
  "Transcode a HEADLINE element using BACKEND.
INFO is a plist holding contextual information."
  (setq backend (or backend (plist-get info :back-end)))
  (let* ((level (+ (org-export-get-relative-level headline info)))
	 (headline-number (org-export-get-headline-number headline info))
	 (section-number (and (org-export-numbered-headline-p headline info)
			      (mapconcat 'number-to-string
					 headline-number ".")))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property :todo-keyword headline)))
		      (and todo
			   (org-export-data-with-backend todo backend info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (text (org-export-data-with-backend
		(org-element-property :title headline) backend info))
	 (tags (and (plist-get info :with-tags)
		    (org-export-get-tags headline info)))
	 (headline-label (org-export-get-reference headline info))
	 (format-function
	  (if (functionp format-function) format-function
	    (cl-function
	     (lambda (todo todo-type priority text tags
		      &key _level _section-number _headline-label
		      &allow-other-keys)
	       (funcall (plist-get info :odt-format-headline-function)
			todo todo-type priority text tags))))))
    (apply format-function
	   todo todo-type priority text tags
	   :headline-label headline-label
	   :level level
	   :section-number section-number extra-keys)))

(defun org-odt-headline (headline contents info)
  "Transcode a HEADLINE element from Org to ODT.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  ;; Case 1: This is a footnote section: ignore it.
  (unless (org-element-property :footnote-section-p headline)
    (let* ((full-text (org-odt-format-headline--wrap headline nil info))
	   ;; Get level relative to current parsed data.
	   (level (org-export-get-relative-level headline info))
	   (numbered (org-export-numbered-headline-p headline info))
	   ;; Get canonical label for the headline.
	   (id (org-export-get-reference headline info))
	   ;; Extra targets.
	   (extra-targets
	    (let ((id (org-element-property :ID headline)))
	      (if id (org-odt--target "" (concat "ID-" id)) "")))
	   ;; Title.
	   (anchored-title (org-odt--target full-text id)))
      (cond
       ;; Case 2. This is a deep sub-tree: export it as a list item.
       ;;         Also export as items headlines for which no section
       ;;         format has been found.
       ((org-export-low-level-p headline info)
	;; Build the real contents of the sub-tree.
	(concat
	 (and (org-export-first-sibling-p headline info)
	      (format "\n<text:list text:style-name=\"%s\" %s>"
		      ;; Choose style based on list type.
		      (if numbered "OrgNumberedList" "OrgBulletedList")
		      ;; If top-level list, re-start numbering.  Otherwise,
		      ;; continue numbering.
		      (format "text:continue-numbering=\"%s\""
			      (let* ((parent (org-export-get-parent-headline
					      headline)))
				(if (and parent
					 (org-export-low-level-p parent info))
				    "true" "false")))))
	 (let ((headline-has-table-p
		(let ((section (assq 'section (org-element-contents headline))))
		  (assq 'table (and section (org-element-contents section))))))
	   (format "\n<text:list-item>\n%s\n%s"
		   (concat
		    (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
			    "Text_20_body"
			    (concat extra-targets anchored-title))
		    contents)
		   (if headline-has-table-p
		       "</text:list-header>"
		     "</text:list-item>")))
	 (and (org-export-last-sibling-p headline info)
	      "</text:list>")))
       ;; Case 3. Standard headline.  Export it as a section.
       (t
	(concat
	 (format
	  "\n<text:h text:style-name=\"%s\" text:outline-level=\"%s\" text:is-list-header=\"%s\">%s</text:h>"
	  (format "Heading_20_%s%s"
		  level (if numbered "" "_unnumbered"))
	  level
	  (if numbered "false" "true")
	  (concat extra-targets anchored-title))
	 contents))))))

(defun org-odt-format-headline-default-function
  (todo todo-type priority text tags)
  "Default format function for a headline.
See `org-odt-format-headline-function' for details."
  (concat
   ;; Todo.
   (when todo
     (let ((style (if (eq todo-type 'done) "OrgDone" "OrgTodo")))
       (format "<text:span text:style-name=\"%s\">%s</text:span> " style todo)))
   (when priority
     (let* ((style (format "OrgPriority-%c" priority))
	    (priority (format "[#%c]" priority)))
       (format "<text:span text:style-name=\"%s\">%s</text:span> "
	       style priority)))
   ;; Title.
   text
   ;; Tags.
   (when tags
     (concat
      "<text:tab/>"
      (format "<text:span text:style-name=\"%s\">[%s]</text:span>"
	      "OrgTags" (mapconcat
			 (lambda (tag)
			   (format
			    "<text:span text:style-name=\"%s\">%s</text:span>"
			    "OrgTag" tag)) tags " : "))))))


;;;; Horizontal Rule

(defun org-odt-horizontal-rule (_horizontal-rule _contents _info)
  "Transcode an HORIZONTAL-RULE  object from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
	  "Horizontal_20_Line" ""))


;;;; Inline Babel Call

;; Inline Babel Calls are ignored.


;;;; Inline Src Block

(defun org-odt--find-verb-separator (s)
  "Return a character not used in string S.
This is used to choose a separator for constructs like \\verb."
  (let ((ll "~,./?;':\"|!@#%^&-_=+abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ<>()[]{}"))
    (cl-loop for c across ll
	     when (not (string-match (regexp-quote (char-to-string c)) s))
	     return (char-to-string c))))

(defun org-odt-inline-src-block (_inline-src-block _contents _info)
  "Transcode an INLINE-SRC-BLOCK element from Org to ODT.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (error "FIXME"))


;;;; Inlinetask

(defun org-odt-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to ODT.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((todo
	  (and (plist-get info :with-todo-keywords)
	       (let ((todo (org-element-property :todo-keyword inlinetask)))
		 (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type inlinetask)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority inlinetask)))
	 (text (org-export-data (org-element-property :title inlinetask) info))
	 (tags (and (plist-get info :with-tags)
		    (org-export-get-tags inlinetask info))))
    (funcall (plist-get info :odt-format-inlinetask-function)
	     todo todo-type priority text tags contents)))

(defun org-odt-format-inlinetask-default-function
  (todo todo-type priority name tags contents)
  "Default format function for a inlinetasks.
See `org-odt-format-inlinetask-function' for details."
  (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
	  "Text_20_body"
	  (org-odt--textbox
	   (concat
	    (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		    "OrgInlineTaskHeading"
		    (org-odt-format-headline-default-function
		     todo todo-type priority name tags))
	    contents)
	   nil nil "OrgInlineTaskFrame" " style:rel-width=\"100%\"")))

;;;; Italic

(defun org-odt-italic (_italic contents _info)
  "Transcode ITALIC from Org to ODT.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "Emphasis" contents))


;;;; Item

(defun org-odt-item (item contents info)
  "Transcode an ITEM element from Org to ODT.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((plain-list (org-export-get-parent item))
	 (type (org-element-property :type plain-list)))
    (unless (memq type '(ordered unordered descriptive-1 descriptive-2))
      (error "Unknown list type: %S" type))
    (format "\n<text:list-item>\n%s\n%s"
	    contents
	    (if (org-element-map item 'table #'identity info 'first-match)
		"</text:list-header>"
	      "</text:list-item>"))))

;;;; Keyword

(defun org-odt-keyword (keyword _contents info)
  "Transcode a KEYWORD element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    (cond
     ((string= key "ODT") value)
     ((string= key "INDEX")
      ;; FIXME
      (ignore))
     ((string= key "TOC")
      (let ((case-fold-search t))
	(cond
	 ((string-match-p "\\<headlines\\>" value)
	  (let ((depth (or (and (string-match "\\<[0-9]+\\>" value)
				(string-to-number (match-string 0 value)))
			   (plist-get info :headline-levels)))
		(localp (string-match-p "\\<local\\>" value)))
	    (org-odt-toc depth info (and localp keyword))))
	 ((string-match-p "tables\\|figures\\|listings" value)
	  ;; FIXME
	  (ignore))))))))


;;;; Latex Environment


;; (eval-after-load 'ox-odt '(ad-deactivate 'org-format-latex-as-mathml))
;; (defadvice org-format-latex-as-mathml	; FIXME
;;   (after org-odt-protect-latex-fragment activate)
;;   "Encode LaTeX fragment as XML.
;; Do this when translation to MathML fails."
;;   (unless (> (length ad-return-value) 0)
;;     (setq ad-return-value (org-odt--encode-plain-text (ad-get-arg 0)))))

(defun org-odt-latex-environment (latex-environment _contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((latex-frag (org-remove-indentation
		      (org-element-property :value latex-environment))))
    (org-odt-do-format-code latex-frag info)))


;;;; Latex Fragment

;; (when latex-frag			; FIXME
;; 	(setq href (propertize href :title "LaTeX Fragment"
;; 				   :description latex-frag)))
;; handle verbatim
;; provide descriptions

(defun org-odt-latex-fragment (latex-fragment _contents _info)
  "Transcode a LATEX-FRAGMENT object from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((latex-frag (org-element-property :value latex-fragment)))
    (format "<text:span text:style-name=\"%s\">%s</text:span>"
	    "OrgCode" (org-odt--encode-plain-text latex-frag t))))


;;;; Line Break

(defun org-odt-line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "<text:line-break/>")


;;;; Link

;;;; Links :: Label references

(defun org-odt--enumerate (element info &optional predicate n)
  (when predicate (cl-assert (funcall predicate element info)))
  (let* ((--numbered-parent-headline-at-<=-n
	  (lambda (element n info)
	    (cl-loop for x in (org-element-lineage element)
		     thereis (and (eq (org-element-type x) 'headline)
				  (<= (org-export-get-relative-level x info) n)
				  (org-export-numbered-headline-p x info)
				  x))))
	 (--enumerate
	  (lambda (element scope info &optional predicate)
	    (let ((counter 0))
	      (org-element-map (or scope (plist-get info :parse-tree))
		  (org-element-type element)
		(lambda (el)
		  (and (or (not predicate) (funcall predicate el info))
		       (cl-incf counter)
		       (eq element el)
		       counter))
		info 'first-match))))
	 (scope (funcall --numbered-parent-headline-at-<=-n
			 element
			 (or n (plist-get info :odt-display-outline-level))
			 info))
	 (ordinal (funcall --enumerate element scope info predicate))
	 (tag
	  (concat
	   ;; Section number.
	   (and scope
		(mapconcat 'number-to-string
			   (org-export-get-headline-number scope info) "."))
	   ;; Separator.
	   (and scope ".")
	   ;; Ordinal.
	   (number-to-string ordinal))))
    tag))

(defun org-odt-format-label (element info op)
  "Return a label for ELEMENT.

ELEMENT is a `link', `table', `src-block' or `paragraph' type
element.  INFO is a plist used as a communication channel.  OP is
either `definition' or `reference', depending on the purpose of
the generated string.

Return value is a string if OP is set to `reference' or a cons
cell like CAPTION . SHORT-CAPTION) where CAPTION and
SHORT-CAPTION are strings."
  (cl-assert (memq (org-element-type element) '(link table src-block paragraph)))
  (let* ((element-or-parent
	  (cl-case (org-element-type element)
	    (link (org-export-get-parent-element element))
	    (t element)))
	 ;; Get label and caption.
	 (label (and (or (org-element-property :name element)
			 (org-element-property :name element-or-parent))
		     (org-export-get-reference element-or-parent info)))
	 (caption (let ((c (org-export-get-caption element-or-parent)))
		    (and c (org-export-data c info))))
	 ;; FIXME: We don't use short-caption for now
	 (short-caption nil))
    (when (or label caption)
      (let* ((default-category
	       (cl-case (org-element-type element)
		 (table "__Table__")
		 (src-block "__Listing__")
		 ((link paragraph)
		  (cond
		   ((org-odt--enumerable-latex-image-p element info)
		    "__DvipngImage__")
		   ((org-odt--enumerable-image-p element info)
		    "__Figure__")
		   ((org-odt--enumerable-formula-p element info)
		    "__MathFormula__")
		   (t (error "Don't know how to format label for link: %S"
			     element))))
		 (t (error "Don't know how to format label for element type: %s"
			   (org-element-type element)))))
	     seqno)
	(cl-assert default-category)
	(pcase-let
	    ((`(,counter ,label-style ,category ,predicate)
	      (assoc-default default-category org-odt-category-map-alist)))
	  ;; Compute sequence number of the element.
	  (setq seqno (org-odt--enumerate element info predicate))
	  ;; Localize category string.
	  (setq category (org-export-translate category :utf-8 info))
	  (cl-case op
	    ;; Case 1: Handle Label definition.
	    (definition
	      (cons
	       (concat
		;; Sneak in a bookmark.  The bookmark is used when the
		;; labeled element is referenced with a link that
		;; provides its own description.
		(format "\n<text:bookmark text:name=\"%s\"/>" label)
		;; Label definition: Typically formatted as below:
		;;     CATEGORY SEQ-NO: LONG CAPTION
		;; with translation for correct punctuation.
		(format-spec
		 (org-export-translate
		  (cadr (assoc-string label-style org-odt-label-styles t))
		  :utf-8 info)
		 `((?e . ,category)
		   (?n . ,(format
			   "<text:sequence text:ref-name=\"%s\" text:name=\"%s\" text:formula=\"ooow:%s+1\" style:num-format=\"1\">%s</text:sequence>"
			   label counter counter seqno))
		   (?c . ,(or caption "")))))
	       short-caption))
	    ;; Case 2: Handle Label reference.
	    (reference
	     (let* ((fmt (cddr (assoc-string label-style org-odt-label-styles t)))
		    (fmt1 (car fmt))
		    (fmt2 (cadr fmt)))
	       (format "<text:sequence-ref text:reference-format=\"%s\" text:ref-name=\"%s\">%s</text:sequence-ref>"
		       fmt1
		       label
		       (format-spec fmt2 `((?e . ,category) (?n . ,seqno))))))
	    (t (error "Unknown %S on label" op))))))))


;;;; Links :: Inline Images

(defun org-odt--copy-image-file (path)
  "Returns the internal name of the file"
  (let* ((image-type (file-name-extension path))
	 (media-type (format "image/%s" image-type))
	 (target-dir "Images/")
	 (target-file
	  (format "%s%04d.%s" target-dir
		  (cl-incf org-odt-embedded-images-count) image-type)))
    (message "Embedding %s as %s..."
	     (substring-no-properties path) target-file)

    (when (= 1 org-odt-embedded-images-count)
      (make-directory (concat org-odt-zip-dir target-dir))
      (org-odt-create-manifest-file-entry "" target-dir))

    (copy-file path (concat org-odt-zip-dir target-file) 'overwrite)
    (org-odt-create-manifest-file-entry media-type target-file)
    target-file))

(defun org-odt--image-size
  (file info &optional user-width user-height scale dpi embed-as)
  (let* ((--pixels-to-cms
	  (function (lambda (pixels dpi)
		      (let ((cms-per-inch 2.54)
			    (inches (/ pixels dpi)))
			(* cms-per-inch inches)))))
	 (--size-in-cms
	  (function
	   (lambda (size-in-pixels dpi)
	     (and size-in-pixels
		  (cons (funcall --pixels-to-cms (car size-in-pixels) dpi)
			(funcall --pixels-to-cms (cdr size-in-pixels) dpi))))))
	 (dpi (or dpi (plist-get info :odt-pixels-per-inch)))
	 (anchor-type (or embed-as "paragraph"))
	 (user-width (and (not scale) user-width))
	 (user-height (and (not scale) user-height))
	 (size
	  (and
	   (not (and user-height user-width))
	   (or
	    ;; Use Imagemagick.
	    (and (executable-find "identify")
		 (let ((size-in-pixels
			(let ((dim (shell-command-to-string
				    (format "identify -format \"%%w:%%h\" \"%s\""
					    file))))
			  (when (string-match "\\([0-9]+\\):\\([0-9]+\\)" dim)
			    (cons (string-to-number (match-string 1 dim))
				  (string-to-number (match-string 2 dim)))))))
		   (funcall --size-in-cms size-in-pixels dpi)))
	    ;; Use Emacs.
	    (let ((size-in-pixels
		   (ignore-errors	; Emacs could be in batch mode
		     (clear-image-cache)
		     (image-size (create-image file) 'pixels))))
	      (funcall --size-in-cms size-in-pixels dpi))
	    ;; Use hard-coded values.
	    (cdr (assoc-string anchor-type
			       org-odt-default-image-sizes-alist))
	    ;; Error out.
	    (error "Cannot determine image size, aborting"))))
	 (width (car size)) (height (cdr size)))
    (cond
     (scale
      (setq width (* width scale) height (* height scale)))
     ((and user-height user-width)
      (setq width user-width height user-height))
     (user-height
      (setq width (* user-height (/ width height)) height user-height))
     (user-width
      (setq height (* user-width (/ height width)) width user-width))
     (t (ignore)))
    ;; ensure that an embedded image fits comfortably within a page
    (let ((max-width (car org-odt-max-image-size))
	  (max-height (cdr org-odt-max-image-size)))
      (when (or (> width max-width) (> height max-height))
	(let* ((scale1 (/ max-width width))
	       (scale2 (/ max-height height))
	       (scale (min scale1 scale2)))
	  (setq width (* scale width) height (* scale height)))))
    (cons width height)))

(defun org-odt-link--inline-image (element info)
  "Return ODT code for an inline image.
LINK is the link pointing to the inline image.  INFO is a plist
used as a communication channel."
  (cl-assert (eq (org-element-type element) 'link))
  (let* ((src (let* ((type (org-element-property :type element))
		     (raw-path (org-element-property :path element)))
		(cond ((member type '("http" "https"))
		       (concat type ":" raw-path))
		      ((file-name-absolute-p raw-path)
		       (expand-file-name raw-path))
		      (t raw-path))))
	 (src-expanded (if (file-name-absolute-p src) src
			 (expand-file-name src (file-name-directory
						(plist-get info :input-file)))))
	 (href (format
		"\n<draw:image xlink:href=\"%s\" xlink:type=\"simple\" xlink:show=\"embed\" xlink:actuate=\"onLoad\"/>"
		(org-odt--copy-image-file src-expanded)))
	 ;; Extract attributes from #+ATTR_ODT line.
	 (attr-from (cl-case (org-element-type element)
		      (link (org-export-get-parent-element element))
		      (t element)))
	 ;; Convert attributes to a plist.
	 (attr-plist (org-export-read-attribute :attr_odt attr-from))
	 ;; Handle `:anchor', `:style' and `:attributes' properties.
	 (user-frame-anchor
	  (car (assoc-string (plist-get attr-plist :anchor)
			     '(("as-char") ("paragraph") ("page")) t)))
	 (user-frame-style
	  (and user-frame-anchor (plist-get attr-plist :style)))
	 (user-frame-attrs
	  (and user-frame-anchor (plist-get attr-plist :attributes)))
	 (user-frame-params
	  (list user-frame-style user-frame-attrs user-frame-anchor))
	 ;; (embed-as (or embed-as user-frame-anchor "paragraph"))
	 ;;
	 ;; Handle `:width', `:height' and `:scale' properties.  Read
	 ;; them as numbers since we need them for computations.
	 (size (org-odt--image-size
		src-expanded info
		(let ((width (plist-get attr-plist :width)))
		  (and width (read width)))
		(let ((length (plist-get attr-plist :length)))
		  (and length (read length)))
		(let ((scale (plist-get attr-plist :scale)))
		  (and scale (read scale)))
		nil			; embed-as
		"paragraph"		; FIXME
		))
	 (width (car size)) (height (cdr size))
	 (standalone-link-p (org-odt--standalone-link-p element info))
	 (embed-as (if standalone-link-p "paragraph" "as-char"))
	 (captions (org-odt-format-label element info 'definition))
	 (caption (car captions))
	 (entity (concat (and caption "Captioned") embed-as "Image"))
	 ;; Check if this link was created by LaTeX-to-PNG converter.
	 (replaces (org-element-property
		    :replaces (if (not standalone-link-p) element
				(org-export-get-parent-element element))))
	 ;; If yes, note down the type of the element - LaTeX Fragment
	 ;; or LaTeX environment.  It will go in to frame title.
	 (title (and replaces (capitalize
			       (symbol-name (org-element-type replaces)))))

	 ;; If yes, note down its contents.  It will go in to frame
	 ;; description.  This quite useful for debugging.
	 (desc (and replaces (org-element-property :value replaces))))
    (org-odt--render-image/formula entity href width height
				   captions user-frame-params title desc)))


;;;; Links :: Math formula

(defun org-odt-link--inline-formula (element info)
  (let* ((src (let ((raw-path (org-element-property :path element)))
		(cond
		 ((file-name-absolute-p raw-path)
		  (expand-file-name raw-path))
		 (t raw-path))))
	 (src-expanded (if (file-name-absolute-p src) src
			 (expand-file-name src (file-name-directory
						(plist-get info :input-file)))))
	 (href
	  (format
	   "\n<draw:object %s xlink:href=\"%s\" xlink:type=\"simple\"/>"
	   " xlink:show=\"embed\" xlink:actuate=\"onLoad\""
	   (file-name-directory (org-odt--copy-formula-file src-expanded))))
	 (standalone-link-p (org-odt--standalone-link-p element info))
	 (embed-as (if standalone-link-p 'paragraph 'character))
	 (captions (org-odt-format-label element info 'definition))
	 ;; Check if this link was created by LaTeX-to-MathML
	 ;; converter.
	 (replaces (org-element-property
		    :replaces (if (not standalone-link-p) element
				(org-export-get-parent-element element))))
	 ;; If yes, note down the type of the element - LaTeX Fragment
	 ;; or LaTeX environment.  It will go in to frame title.
	 (title (and replaces (capitalize
			       (symbol-name (org-element-type replaces)))))

	 ;; If yes, note down its contents.  It will go in to frame
	 ;; description.  This quite useful for debugging.
	 (desc (and replaces (org-element-property :value replaces)))
	 width height)
    (cond
     ((eq embed-as 'character)
      (org-odt--render-image/formula "InlineFormula" href width height
				     nil nil title desc))
     (t
      (let* ((equation (org-odt--render-image/formula
			"CaptionedDisplayFormula" href width height
			captions nil title desc))
	     (label
	      (let* ((org-odt-category-map-alist
		      '(("__MathFormula__" "Text" "math-label" "Equation"
			 org-odt--enumerable-formula-p))))
		(car (org-odt-format-label element info 'definition)))))
	(concat equation "<text:tab/>" label))))))

(defun org-odt--copy-formula-file (src-file)
  "Returns the internal name of the file"
  (let* ((target-dir (format "Formula-%04d/"
			     (cl-incf org-odt-embedded-formulas-count)))
	 (target-file (concat target-dir "content.xml")))
    ;; Create a directory for holding formula file.  Also enter it in
    ;; to manifest.
    (make-directory (concat org-odt-zip-dir target-dir))
    (org-odt-create-manifest-file-entry
     "application/vnd.oasis.opendocument.formula" target-dir "1.2")
    ;; Copy over the formula file from user directory to zip
    ;; directory.
    (message "Embedding %s as %s..." src-file target-file)
    (let ((ext (file-name-extension src-file)))
      (cond
       ;; Case 1: Mathml.
       ((member ext '("mathml" "mml"))
	(copy-file src-file (concat org-odt-zip-dir target-file) 'overwrite))
       ;; Case 2: OpenDocument formula.
       ((string= ext "odf")
	(org-odt--zip-extract src-file "content.xml"
				(concat org-odt-zip-dir target-dir)))
       (t (error "%s is not a formula file" src-file))))
    ;; Enter the formula file in to manifest.
    (org-odt-create-manifest-file-entry "text/xml" target-file)
    target-file))

;;;; Targets

(defun org-odt--render-image/formula (cfg-key href width height &optional
					      captions user-frame-params
					      &rest title-and-desc)
  (let* ((frame-cfg-alist
	  ;; Each element of this alist is of the form (CFG-HANDLE
	  ;; INNER-FRAME-PARAMS OUTER-FRAME-PARAMS).

	  ;; CFG-HANDLE is the key to the alist.

	  ;; INNER-FRAME-PARAMS and OUTER-FRAME-PARAMS specify the
	  ;; frame params for INNER-FRAME and OUTER-FRAME
	  ;; respectively.  See below.

	  ;; Configurations that are meant to be applied to
	  ;; non-captioned image/formula specifies no
	  ;; OUTER-FRAME-PARAMS.

	  ;; TERMINOLOGY
	  ;; ===========
	  ;; INNER-FRAME :: Frame that directly surrounds an
	  ;;                image/formula.

	  ;; OUTER-FRAME :: Frame that encloses the INNER-FRAME.  This
	  ;;                frame also contains the caption, if any.

	  ;; FRAME-PARAMS :: List of the form (FRAME-STYLE-NAME
	  ;;                 FRAME-ATTRIBUTES FRAME-ANCHOR).  Note
	  ;;                 that these are the last three arguments
	  ;;                 to `org-odt--frame'.

	  ;; Note that an un-captioned image/formula requires just an
	  ;; INNER-FRAME, while a captioned image/formula requires
	  ;; both an INNER and an OUTER-FRAME.
	  '(("As-CharImage" ("OrgInlineImage" nil "as-char"))
	    ("ParagraphImage" ("OrgDisplayImage" nil "paragraph"))
	    ("PageImage" ("OrgPageImage" nil "page"))
	    ("CaptionedAs-CharImage"
	     ("OrgCaptionedImage"
	      " style:rel-width=\"100%\" style:rel-height=\"scale\"" "paragraph")
	     ("OrgInlineImage" nil "as-char"))
	    ("CaptionedParagraphImage"
	     ("OrgCaptionedImage"
	      " style:rel-width=\"100%\" style:rel-height=\"scale\"" "paragraph")
	     ("OrgImageCaptionFrame" nil "paragraph"))
	    ("CaptionedPageImage"
	     ("OrgCaptionedImage"
	      " style:rel-width=\"100%\" style:rel-height=\"scale\"" "paragraph")
	     ("OrgPageImageCaptionFrame" nil "page"))
	    ("InlineFormula" ("OrgInlineFormula" nil "as-char"))
	    ("DisplayFormula" ("OrgDisplayFormula" nil "as-char"))
	    ("CaptionedDisplayFormula"
	     ("OrgCaptionedFormula" nil "paragraph")
	     ("OrgFormulaCaptionFrame" nil "paragraph"))))
	 (caption (car captions)) (short-caption (cdr captions))
	 ;; Retrieve inner and outer frame params, from configuration.
	 (frame-cfg (assoc-string cfg-key frame-cfg-alist t))
	 (inner (nth 1 frame-cfg))
	 (outer (nth 2 frame-cfg))
	 ;; User-specified frame params (from #+ATTR_ODT spec)
	 (user user-frame-params)
	 (--merge-frame-params (function
				(lambda (default user)
				  "Merge default and user frame params."
				  (if (not user) default
				    (cl-assert (= (length default) 3))
				    (cl-assert (= (length user) 3))
				    (cl-loop for u in user
					     for d in default
					     collect (or u d)))))))
    (cond
     ;; Case 1: Image/Formula has no caption.
     ;;         There is only one frame, one that surrounds the image
     ;;         or formula.
     ((not caption)
      ;; Merge user frame params with that from configuration.
      (setq inner (funcall --merge-frame-params inner user))
      (apply 'org-odt--frame href width height
	     (append inner title-and-desc)))
     ;; Case 2: Image/Formula is captioned or labeled.
     ;;         There are two frames: The inner one surrounds the
     ;;         image or formula.  The outer one contains the
     ;;         caption/sequence number.
     (t
      ;; Merge user frame params with outer frame params.
      (setq outer (funcall --merge-frame-params outer user))
      ;; Short caption, if specified, goes as part of inner frame.
      (setq inner (let ((frame-params (copy-sequence inner)))
		    (setcar (cdr frame-params)
			    (concat
			     (cadr frame-params)
			     (when short-caption
			       (format " draw:name=\"%s\" " short-caption))))
		    frame-params))
      (apply 'org-odt--textbox
	     (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		     "Illustration"
		     (concat
		      (apply 'org-odt--frame href width height
			     (append inner title-and-desc))
		      caption))
	     width height outer)))))

(defun org-odt--enumerable-p (element _info)
  ;; Element should have a caption or label.
  (or (org-element-property :caption element)
      (org-element-property :name element)))

(defun org-odt--enumerable-image-p (element info)
  (org-odt--standalone-link-p
   element info
   ;; Paragraph should have a caption or label.  It SHOULD NOT be a
   ;; replacement element. (i.e., It SHOULD NOT be a result of LaTeX
   ;; processing.)
   (lambda (p)
     (and (not (org-element-property :replaces p))
	  (or (org-element-property :caption p)
	      (org-element-property :name p))))
   ;; Link should point to an image file.
   (lambda (l)
     (cl-assert (eq (org-element-type l) 'link))
     (org-export-inline-image-p l (plist-get info :odt-inline-image-rules)))))

(defun org-odt--enumerable-latex-image-p (element info)
  (org-odt--standalone-link-p
   element info
   ;; Paragraph should have a caption or label.  It SHOULD also be a
   ;; replacement element. (i.e., It SHOULD be a result of LaTeX
   ;; processing.)
   (lambda (p)
     (and (org-element-property :replaces p)
	  (or (org-element-property :caption p)
	      (org-element-property :name p))))
   ;; Link should point to an image file.
   (lambda (l)
     (cl-assert (eq (org-element-type l) 'link))
     (org-export-inline-image-p l (plist-get info :odt-inline-image-rules)))))

(defun org-odt--enumerable-formula-p (element info)
  (org-odt--standalone-link-p
   element info
   ;; Paragraph should have a caption or label.
   (lambda (p)
     (or (org-element-property :caption p)
	 (org-element-property :name p)))
   ;; Link should point to a MathML or ODF file.
   (lambda (l)
     (cl-assert (eq (org-element-type l) 'link))
     (org-export-inline-image-p l (plist-get info :odt-inline-formula-rules)))))

(defun org-odt--standalone-link-p (element _info &optional
					   paragraph-predicate
					   link-predicate)
  "Test if ELEMENT is a standalone link for the purpose ODT export.
INFO is a plist holding contextual information.

Return non-nil, if ELEMENT is of type paragraph satisfying
PARAGRAPH-PREDICATE and its sole content, save for whitespaces,
is a link that satisfies LINK-PREDICATE.

Return non-nil, if ELEMENT is of type link satisfying
LINK-PREDICATE and its containing paragraph satisfies
PARAGRAPH-PREDICATE in addition to having no other content save for
leading and trailing whitespaces.

Return nil, otherwise."
  (let ((p (cl-case (org-element-type element)
	     (paragraph element)
	     (link (and (or (not link-predicate)
			    (funcall link-predicate element))
			(org-export-get-parent element)))
	     (t nil))))
    (when (and p (eq (org-element-type p) 'paragraph))
      (when (or (not paragraph-predicate)
		(funcall paragraph-predicate p))
	(let ((contents (org-element-contents p)))
	  (cl-loop for x in contents
		   with inline-image-count = 0
		   always (cl-case (org-element-type x)
			    (plain-text
			     (not (org-string-nw-p x)))
			    (link
			     (and (or (not link-predicate)
				      (funcall link-predicate x))
				  (= (cl-incf inline-image-count) 1)))
			    (t nil))))))))

(defun org-odt-link--infer-description (destination info)
  ;; DESTINATION is a headline or an element (like paragraph,
  ;; verse-block etc) to which a "#+NAME: label" can be attached.

  ;; Note that labels that are attached to captioned entities - inline
  ;; images, math formulae and tables - get resolved as part of
  ;; `org-odt-format-label' and `org-odt--enumerate'.

  ;; Create a cross-reference to DESTINATION but make best-efforts to
  ;; create a *meaningful* description.  Check item numbers, section
  ;; number and section title in that order.

  ;; NOTE: Counterpart of `org-export-get-ordinal'.
  ;; FIXME: Handle footnote-definition footnote-reference?
  (let* ((genealogy (org-element-lineage destination))
	 (data (reverse genealogy))
	 (label (let ((type (org-element-type destination)))
		  (if (memq type '(headline target))
		      (org-export-get-reference destination info)
		    (error "FIXME: Unable to resolve %S" destination)))))
    (or
     (let* ( ;; Locate top-level list.
	    (top-level-list
	     (cl-loop for x on data
		      when (eq (org-element-type (car x)) 'plain-list)
		      return x))
	    ;; Get list item nos.
	    (item-numbers
	     (cl-loop for (plain-list item . rest) on top-level-list by #'cddr
		      until (not (eq (org-element-type plain-list) 'plain-list))
		      collect (when (eq (org-element-property :type
							      plain-list)
					'ordered)
				(1+ (length (org-export-get-previous-element
					     item info t))))))
	    ;; Locate top-most listified headline.
	    (listified-headlines
	     (cl-loop for x on data
		      when (and (eq (org-element-type (car x)) 'headline)
				(org-export-low-level-p (car x) info))
		      return x))
	    ;; Get listified headline numbers.
	    (listified-headline-nos
	     (cl-loop for el in listified-headlines
		      when (eq (org-element-type el) 'headline)
		      collect (when (org-export-numbered-headline-p el info)
				(1+ (length (org-export-get-previous-element
					     el info t)))))))
       ;; Combine item numbers from both the listified headlines and
       ;; regular list items.

       ;; Case 1: Check if all the parents of list item are numbered.
       ;; If yes, link to the item proper.
       (let ((item-numbers (append listified-headline-nos item-numbers)))
	 (when (and item-numbers (not (memq nil item-numbers)))
	   (format "<text:bookmark-ref text:reference-format=\"number-all-superior\" text:ref-name=\"%s\">%s</text:bookmark-ref>"
		   label
		   (mapconcat (lambda (n) (if (not n) " "
				       (concat (number-to-string n) ".")))
			      item-numbers "")))))
     ;; Case 2: Locate a regular and numbered headline in the
     ;; hierarchy.  Display its section number.
     (let ((headline
	    (and
	     ;; Test if destination is a numbered headline.
	     (org-export-numbered-headline-p destination info)
	     (cl-loop for el in (cons destination genealogy)
		      when (and (eq (org-element-type el) 'headline)
				(not (org-export-low-level-p el info))
				(org-export-numbered-headline-p el info))
		      return el))))
       ;; We found one.
       (when headline
	 (format "<text:bookmark-ref text:reference-format=\"chapter\" text:ref-name=\"OrgXref.%s\">%s</text:bookmark-ref>"
		 label
		 (mapconcat 'number-to-string (org-export-get-headline-number
					       headline info) "."))))
     ;; Case 4: Locate a regular headline in the hierarchy.  Display
     ;; its title.
     (let ((headline (cl-loop for el in (cons destination genealogy)
			      when (and (eq (org-element-type el) 'headline)
					(not (org-export-low-level-p el info)))
			      return el)))
       ;; We found one.
       (when headline
	 (format "<text:bookmark-ref text:reference-format=\"text\" text:ref-name=\"OrgXref.%s\">%s</text:bookmark-ref>"
		 label
		 (let ((title (org-element-property :title headline)))
		   (org-export-data title info)))))
     (error "FIXME?"))))

(defun org-odt-link (link desc info)
  "Transcode a LINK object from Org to ODT.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 ;; Ensure DESC really exists, or set it to nil.
	 (desc (and (not (string= desc "")) desc))
	 (imagep (org-export-inline-image-p
		  link (plist-get info :odt-inline-image-rules)))
	 (path (cond
		((member type '("http" "https" "ftp" "mailto"))
		 (concat type ":" raw-path))
		((string= type "file") (org-export-file-uri raw-path))
		(t raw-path)))
	 ;; Convert & to &amp; for correct XML representation
	 (path (replace-regexp-in-string "&" "&amp;" path)))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'odt))
     ;; Image file.
     ((and (not desc) imagep) (org-odt-link--inline-image link info))
     ;; Formula file.
     ((and (not desc)
	   (org-export-inline-image-p
	    link (plist-get info :odt-inline-formula-rules)))
      (org-odt-link--inline-formula link info))
     ;; Radio target: Transcode target's contents and use them as
     ;; link's description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
	(if (not destination) desc
	  (format
	   "<text:bookmark-ref text:reference-format=\"text\" text:ref-name=\"OrgXref.%s\">%s</text:bookmark-ref>"
	   (org-export-get-reference destination info)
	   desc))))
     ;; Links pointing to a headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(cl-case (org-element-type destination)
	  ;; Fuzzy link points to a headline.  If there's
	  ;; a description, create a hyperlink.  Otherwise, try to
	  ;; provide a meaningful description.
	  (headline
	   (if (not desc) (org-odt-link--infer-description destination info)
	     (let ((label
		    (or (and (string= type "custom-id")
			     (org-element-property :CUSTOM_ID destination))
			(org-export-get-reference destination info))))
	       (format
		"<text:a xlink:type=\"simple\" xlink:href=\"#%s\">%s</text:a>"
		label desc))))
	  ;; Fuzzy link points to a target.  If there's a description,
	  ;; create a hyperlink.  Otherwise, try to provide
	  ;; a meaningful description.
	  (target
	   (format "<text:a xlink:type=\"simple\" xlink:href=\"#%s\">%s</text:a>"
		   (org-export-get-reference destination info)
		   (or desc (org-export-get-ordinal destination info))))
	  ;; Fuzzy link points to some element (e.g., an inline image,
	  ;; a math formula or a table).
	  (otherwise
	   (let ((label-reference
		  (ignore-errors
		    (org-odt-format-label destination info 'reference))))
	     (cond
	      ((not label-reference)
	       (org-odt-link--infer-description destination info))
	      ;; LINK has no description.  Create
	      ;; a cross-reference showing entity's sequence
	      ;; number.
	      ((not desc) label-reference)
	      ;; LINK has description.  Insert a hyperlink with
	      ;; user-provided description.
	      (t
	       (format
		"<text:a xlink:type=\"simple\" xlink:href=\"#%s\">%s</text:a>"
		(org-export-get-reference destination info)
		desc))))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (let* ((line-no (format "%d" (org-export-resolve-coderef path info)))
	     (href (concat "coderef-" path)))
	(format
	 (org-export-get-coderef-format path desc)
	 (format
	  "<text:bookmark-ref text:reference-format=\"number\" text:ref-name=\"OrgXref.%s\">%s</text:bookmark-ref>"
	  href line-no))))
     ;; External link with a description part.
     ((and path desc)
      (let ((link-contents (org-element-contents link)))
	;; Check if description is a link to an inline image.
	(if (and (not (cdr link-contents))
		 (let ((desc-element (car link-contents)))
		   (and (eq (org-element-type desc-element) 'link)
			(org-export-inline-image-p
			 desc-element
			 (plist-get info :odt-inline-image-rules)))))
	    ;; Format link as a clickable image.
	    (format "\n<draw:a xlink:type=\"simple\" xlink:href=\"%s\">\n%s\n</draw:a>"
		    path desc)
	  ;; Otherwise, format it as a regular link.
	  (format "<text:a xlink:type=\"simple\" xlink:href=\"%s\">%s</text:a>"
		  path desc))))
     ;; External link without a description part.
     (path
      (format "<text:a xlink:type=\"simple\" xlink:href=\"%s\">%s</text:a>"
	      path path))
     ;; No path, only description.  Try to do something useful.
     (t (format "<text:span text:style-name=\"%s\">%s</text:span>"
		"Emphasis" desc)))))


;;;; Node Property

(defun org-odt-node-property (node-property _contents _info)
  "Transcode a NODE-PROPERTY element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-odt--encode-plain-text
   (format "%s:%s"
	   (org-element-property :key node-property)
	   (let ((value (org-element-property :value node-property)))
	     (if value (concat " " value) "")))))

;;;; Paragraph

(defun org-odt--paragraph-style (paragraph)
  "Return style of PARAGRAPH.
Style is a symbol among `quoted', `centered' and nil."
  (let ((up paragraph))
    (while (and (setq up (org-element-property :parent up))
		(not (memq (org-element-type up)
			   '(center-block quote-block section)))))
    (cl-case (org-element-type up)
      (center-block 'centered)
      (quote-block 'quoted))))

(defun org-odt--format-paragraph (paragraph contents info default center quote)
  "Format paragraph according to given styles.
PARAGRAPH is a paragraph type element.  CONTENTS is the
transcoded contents of that paragraph, as a string.  INFO is
a plist used as a communication channel.  DEFAULT, CENTER and
QUOTE are, respectively, style to use when paragraph belongs to
no special environment, a center block, or a quote block."
  (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
	  (cl-case (org-odt--paragraph-style paragraph)
	    (quoted quote)
	    (centered center)
	    (otherwise default))
	  ;; If PARAGRAPH is a leading paragraph in an item that has
	  ;; a checkbox, splice checkbox and paragraph contents
	  ;; together.
	  (concat (let ((parent (org-element-property :parent paragraph)))
		    (and (eq (org-element-type parent) 'item)
			 (not (org-export-get-previous-element paragraph info))
			 (org-odt--checkbox parent)))
		  contents)))

(defun org-odt-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to ODT.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (org-odt--format-paragraph
   paragraph contents info
   (or (org-element-property :style paragraph) "Text_20_body")
   "OrgCenter"
   "Quotations"))


;;;; Plain List

(defun org-odt-plain-list (plain-list contents _info)
  "Transcode a PLAIN-LIST element from Org to ODT.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (format "\n<text:list text:style-name=\"%s\" %s>\n%s</text:list>"
	  ;; Choose style based on list type.
	  (cl-case (org-element-property :type plain-list)
	    (ordered "OrgNumberedList")
	    (unordered "OrgBulletedList")
	    (descriptive-1 "OrgDescriptionList")
	    (descriptive-2 "OrgDescriptionList"))
	  ;; If top-level list, re-start numbering.  Otherwise,
	  ;; continue numbering.
	  (format "text:continue-numbering=\"%s\""
		  (let* ((parent (org-export-get-parent plain-list)))
		    (if (and parent (eq (org-element-type parent) 'item))
			"true" "false")))
	  contents))

;;;; Plain Text

(defun org-odt--encode-tabs-and-spaces (line)
  (replace-regexp-in-string
   "\\(\t\\| \\{2,\\}\\)"
   (lambda (s)
     (if (string= s "\t") "<text:tab/>"
       (format " <text:s text:c=\"%d\"/>" (1- (length s)))))
   line))

(defun org-odt--encode-plain-text (text &optional no-whitespace-filling)
  (dolist (pair '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;")))
    (setq text (replace-regexp-in-string (car pair) (cdr pair) text t t)))
  (if no-whitespace-filling text
    (org-odt--encode-tabs-and-spaces text)))

(defun org-odt-plain-text (text info)
  "Transcode a TEXT string from Org to ODT.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (let ((output text))
    ;; Protect &, < and >.
    (setq output (org-odt--encode-plain-text output t))
    ;; Handle smart quotes.  Be sure to provide original string since
    ;; OUTPUT may have been modified.
    (when (plist-get info :with-smart-quotes)
      (setq output (org-export-activate-smart-quotes output :utf-8 info text)))
    ;; Convert special strings.
    (when (plist-get info :with-special-strings)
      (dolist (pair org-odt-special-string-regexps)
	(setq output
	      (replace-regexp-in-string (car pair) (cdr pair) output t nil))))
    ;; Handle break preservation if required.
    (when (plist-get info :preserve-breaks)
      (setq output (replace-regexp-in-string
		    "\\(\\\\\\\\\\)?[ \t]*\n" "<text:line-break/>" output t)))
    ;; Return value.
    output))


;;;; Planning

(defun org-odt-planning (planning contents info)
  "Transcode a PLANNING element from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
	  "OrgPlanning"
	  (concat
	   (let ((closed (org-element-property :closed planning)))
	     (when closed
	       (concat
		(format "<text:span text:style-name=\"%s\">%s</text:span>"
			"OrgClosedKeyword" org-closed-string)
		(org-odt-timestamp closed contents info))))
	   (let ((deadline (org-element-property :deadline planning)))
	     (when deadline
	       (concat
		(format "<text:span text:style-name=\"%s\">%s</text:span>"
			"OrgDeadlineKeyword" org-deadline-string)
		(org-odt-timestamp deadline contents info))))
	   (let ((scheduled (org-element-property :scheduled planning)))
	     (when scheduled
	       (concat
		(format "<text:span text:style-name=\"%s\">%s</text:span>"
			"OrgScheduledKeyword" org-deadline-string)
		(org-odt-timestamp scheduled contents info)))))))


;;;; Property Drawer

(defun org-odt-property-drawer (_property-drawer contents _info)
  "Transcode a PROPERTY-DRAWER element from Org to ODT.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (and (org-string-nw-p contents)
       (format "<text:p text:style-name=\"OrgFixedWidthBlock\">%s</text:p>"
	       contents)))


;;;; Quote Block

(defun org-odt-quote-block (_quote-block contents _info)
  "Transcode a QUOTE-BLOCK element from Org to ODT.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  contents)


;;;; Section

(defun org-odt-format-section (text style &optional name)
  (let ((default-name (car (org-odt-add-automatic-style "Section"))))
    (format "\n<text:section text:style-name=\"%s\" %s>\n%s\n</text:section>"
	    style
	    (format "text:name=\"%s\"" (or name default-name))
	    text)))


(defun org-odt-section (_section contents _info) ; FIXME
  "Transcode a SECTION element from Org to ODT.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)

;;;; Radio Target

(defun org-odt-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to ODT.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (org-odt--target text (org-export-get-reference radio-target info)))


;;;; Special Block

(defun org-odt-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to ODT.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((type (org-element-property :type special-block))
	(attributes (org-export-read-attribute :attr_odt special-block)))
    (cond
     ;; Annotation.
     ((string= type "annotation")
      (let* ((author (or (plist-get attributes :author)
			 (let ((author (plist-get info :author)))
			   (and author (org-export-data author info)))))
	     (date (or (plist-get attributes :date)
		       ;; FIXME: Is `car' right thing to do below?
		       (car (plist-get info :date)))))
	(format "\n<text:p>%s</text:p>"
		(format "<office:annotation>\n%s\n</office:annotation>"
			(concat
			 (and author
			      (format "<dc:creator>%s</dc:creator>" author))
			 (and date
			      (format "<dc:date>%s</dc:date>"
				      (org-odt--format-timestamp date nil 'iso-date)))
			 contents)))))
     ;; Textbox.
     ((string= type "textbox")
      (let ((width (plist-get attributes :width))
	    (height (plist-get attributes :height))
	    (style (plist-get attributes :style))
	    (extra (plist-get attributes :extra))
	    (anchor (plist-get attributes :anchor)))
	(format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		"Text_20_body" (org-odt--textbox contents width height
						   style extra anchor))))
     (t contents))))


;;;; Src Block

(defun org-odt-hfy-face-to-css (fn)
  "Create custom style for face FN.
When FN is the default face, use its foreground and background
properties to create \"OrgSrcBlock\" paragraph style.  Otherwise
use its color attribute to create a character style whose name
is obtained from FN.  Currently all attributes of FN other than
color are ignored.

The style name for a face FN is derived using the following
operations on the face name in that order - de-dash, CamelCase
and prefix with \"OrgSrc\".  For example,
`font-lock-function-name-face' is associated with
\"OrgSrcFontLockFunctionNameFace\"."
  (let* ((css-list (hfy-face-to-style fn))
	 (style-name (concat "OrgSrc"
                             (mapconcat
                              'capitalize (split-string
                                           (hfy-face-or-def-to-name fn) "-")
                              "")))
	 (color-val (cdr (assoc "color" css-list)))
	 (background-color-val (cdr (assoc "background" css-list)))
	 (style (and org-odt-create-custom-styles-for-srcblocks
		     (cond
		      ((eq fn 'default)
		       (format org-odt-src-block-paragraph-format
			       background-color-val color-val))
		      (t
		       (format
			"
<style:style style:name=\"%s\" style:family=\"text\">
  <style:text-properties fo:color=\"%s\"/>
 </style:style>" style-name color-val))))))
    (cons style-name style)))

(defun org-odt-htmlfontify-string (line)
  (let* ((hfy-html-quote-regex "\\([<\"&> \t]\\)")
	 (hfy-html-quote-map '(("\"" "&quot;")
			       ("<" "&lt;")
			       ("&" "&amp;")
			       (">" "&gt;")
			       (" " "<text:s/>")
			       ("\t" "<text:tab/>")))
	 (hfy-face-to-css 'org-odt-hfy-face-to-css)
	 (hfy-optimizations-1 (copy-sequence hfy-optimizations))
	 (hfy-optimizations (cl-pushnew 'body-text-only hfy-optimizations-1))
	 (hfy-begin-span-handler
	  (lambda (style _text-block _text-id _text-begins-block-p)
	    (insert (format "<text:span text:style-name=\"%s\">" style))))
	 (hfy-end-span-handler (lambda () (insert "</text:span>"))))
    (with-no-warnings (htmlfontify-string line))))

(defun org-odt-do-format-code
    (code info &optional lang refs retain-labels num-start)
  (let* ((lang (or (assoc-default lang org-src-lang-modes) lang))
	 (lang-mode (and lang (intern (format "%s-mode" lang))))
	 (code-lines (org-split-string code "\n"))
	 (code-length (length code-lines))
	 (use-htmlfontify-p (and (functionp lang-mode)
				 (plist-get info :odt-fontify-srcblocks)
				 (require 'htmlfontify nil t)
				 (fboundp 'htmlfontify-string)))
	 (code (if (not use-htmlfontify-p) code
		 (with-temp-buffer
		   (insert code)
		   (funcall lang-mode)
		   (org-font-lock-ensure)
		   (buffer-string))))
	 (fontifier (if use-htmlfontify-p 'org-odt-htmlfontify-string
		      'org-odt--encode-plain-text))
	 (par-style (if use-htmlfontify-p "OrgSrcBlock"
		      "OrgFixedWidthBlock"))
	 (i 0))
    (cl-assert (= code-length (length (org-split-string code "\n"))))
    (setq code
	  (org-export-format-code
	   code
	   (lambda (loc line-num ref)
	     (setq par-style
		   (concat par-style (and (= (cl-incf i) code-length)
					  "LastLine")))

	     (setq loc (concat loc (and ref retain-labels (format " (%s)" ref))))
	     (setq loc (funcall fontifier loc))
	     (when ref
	       (setq loc (org-odt--target loc (concat "coderef-" ref))))
	     (cl-assert par-style)
	     (setq loc (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
			       par-style loc))
	     (if (not line-num) loc
	       (format "\n<text:list-item>%s\n</text:list-item>" loc)))
	   num-start refs))
    (cond
     ((not num-start) code)
     ((= num-start 0)
      (format
       "\n<text:list text:style-name=\"OrgSrcBlockNumberedLine\"%s>%s</text:list>"
       " text:continue-numbering=\"false\"" code))
     (t
      (format
       "\n<text:list text:style-name=\"OrgSrcBlockNumberedLine\"%s>%s</text:list>"
       " text:continue-numbering=\"true\"" code)))))

(defun org-odt-format-code (element info)
  (let* ((lang (org-element-property :language element))
	 ;; Extract code and references.
	 (code-info (org-export-unravel-code element))
	 (code (car code-info))
	 (refs (cdr code-info))
	 ;; Does the src block contain labels?
	 (retain-labels (org-element-property :retain-labels element))
	 ;; Does it have line numbers?
	 (num-start (org-export-get-loc element info)))
    (org-odt-do-format-code code info lang refs retain-labels num-start)))

(defun org-odt-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to ODT.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((attributes (org-export-read-attribute :attr_odt src-block))
	 (caption (car (org-odt-format-label src-block info 'definition))))
    (concat
     (and caption
	  (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		  "Listing" caption))
     (let ((--src-block (org-odt-format-code src-block info)))
       (if (not (plist-get attributes :textbox)) --src-block
	 (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		 "Text_20_body"
		 (org-odt--textbox --src-block nil nil nil)))))))


;;;; Statistics Cookie

(defun org-odt-statistics-cookie (statistics-cookie _contents _info)
  "Transcode a STATISTICS-COOKIE object from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((cookie-value (org-element-property :value statistics-cookie)))
    (format "<text:span text:style-name=\"%s\">%s</text:span>"
	    "OrgCode" cookie-value)))


;;;; Strike-Through

(defun org-odt-strike-through (_strike-through contents _info)
  "Transcode STRIKE-THROUGH from Org to ODT.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "Strikethrough" contents))


;;;; Subscript

(defun org-odt-subscript (_subscript contents _info)
  "Transcode a SUBSCRIPT object from Org to ODT.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "OrgSubscript" contents))


;;;; Superscript

(defun org-odt-superscript (_superscript contents _info)
  "Transcode a SUPERSCRIPT object from Org to ODT.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "OrgSuperscript" contents))


;;;; Table Cell

(defun org-odt-table-style-spec (element info)
  (let* ((table (org-export-get-parent-table element))
	 (table-attributes (org-export-read-attribute :attr_odt table))
	 (table-style (plist-get table-attributes :style)))
    (assoc table-style (plist-get info :odt-table-styles))))

(defun org-odt-get-table-cell-styles (table-cell info)
  "Retrieve styles applicable to a table cell.
R and C are (zero-based) row and column numbers of the table
cell.  STYLE-SPEC is an entry in `org-odt-table-styles'
applicable to the current table.  It is nil if the table is not
associated with any style attributes.

Return a cons of (TABLE-CELL-STYLE-NAME . PARAGRAPH-STYLE-NAME).

When STYLE-SPEC is nil, style the table cell the conventional way
- choose cell borders based on row and column groupings and
choose paragraph alignment based on `org-col-cookies' text
property.  See also
`org-odt-get-paragraph-style-cookie-for-table-cell'.

When STYLE-SPEC is non-nil, ignore the above cookie and return
styles congruent with the ODF-1.2 specification."
  (let* ((table-cell-address (org-export-table-cell-address table-cell info))
	 (r (car table-cell-address)) (c (cdr table-cell-address))
	 (style-spec (org-odt-table-style-spec table-cell info))
	 (table-dimensions (org-export-table-dimensions
			    (org-export-get-parent-table table-cell)
			    info)))
    (when style-spec
      ;; LibreOffice - particularly the Writer - honors neither table
      ;; templates nor custom table-cell styles.  Inorder to retain
      ;; inter-operability with LibreOffice, only automatic styles are
      ;; used for styling of table-cells.  The current implementation is
      ;; congruent with ODF-1.2 specification and hence is
      ;; future-compatible.

      ;; Additional Note: LibreOffice's AutoFormat facility for tables -
      ;; which recognizes as many as 16 different cell types - is much
      ;; richer. Unfortunately it is NOT amenable to easy configuration
      ;; by hand.
      (let* ((template-name (nth 1 style-spec))
	     (cell-style-selectors (nth 2 style-spec))
	     (cell-type
	      (cond
	       ((and (cdr (assq 'use-first-column-styles cell-style-selectors))
		     (= c 0)) "FirstColumn")
	       ((and (cdr (assq 'use-last-column-styles cell-style-selectors))
		     (= (1+ c) (cdr table-dimensions)))
		"LastColumn")
	       ((and (cdr (assq 'use-first-row-styles cell-style-selectors))
		     (= r 0)) "FirstRow")
	       ((and (cdr (assq 'use-last-row-styles cell-style-selectors))
		     (= (1+ r) (car table-dimensions)))
		"LastRow")
	       ((and (cdr (assq 'use-banding-rows-styles cell-style-selectors))
		     (= (% r 2) 1)) "EvenRow")
	       ((and (cdr (assq 'use-banding-rows-styles cell-style-selectors))
		     (= (% r 2) 0)) "OddRow")
	       ((and (cdr (assq 'use-banding-columns-styles cell-style-selectors))
		     (= (% c 2) 1)) "EvenColumn")
	       ((and (cdr (assq 'use-banding-columns-styles cell-style-selectors))
		     (= (% c 2) 0)) "OddColumn")
	       (t ""))))
	(concat template-name cell-type)))))

(defun org-odt-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((table-cell-address (org-export-table-cell-address table-cell info))
	 (r (car table-cell-address))
	 (c (cdr table-cell-address))
	 (horiz-span (or (org-export-table-cell-width table-cell info) 0))
	 (table-row (org-export-get-parent table-cell))
	 (custom-style-prefix (org-odt-get-table-cell-styles
			       table-cell info))
	 (paragraph-style
	  (or
	   (and custom-style-prefix
		(format "%sTableParagraph" custom-style-prefix))
	   (concat
	    (cond
	     ((and (= 1 (org-export-table-row-group table-row info))
		   (org-export-table-has-header-p
		    (org-export-get-parent-table table-row) info))
	      "OrgTableHeading")
	     ((let* ((table (org-export-get-parent-table table-cell))
		     (table-attrs (org-export-read-attribute :attr_odt table))
		     (table-header-columns
		      (let ((cols (plist-get table-attrs :header-columns)))
			(and cols (read cols)))))
		(<= c (cond ((wholenump table-header-columns)
			     (- table-header-columns 1))
			    (table-header-columns 0)
			    (t -1))))
	      "OrgTableHeading")
	     (t "OrgTableContents"))
	    (capitalize (symbol-name (org-export-table-cell-alignment
				      table-cell info))))))
	 (cell-style-name
	  (or
	   (and custom-style-prefix (format "%sTableCell"
					    custom-style-prefix))
	   (concat
	    "OrgTblCell"
	    (when (or (org-export-table-row-starts-rowgroup-p table-row info)
		      (zerop r)) "T")
	    (when (org-export-table-row-ends-rowgroup-p table-row info) "B")
	    (when (and (org-export-table-cell-starts-colgroup-p table-cell info)
		       (not (zerop c)) ) "L"))))
	 (cell-attributes
	  (concat
	   (format " table:style-name=\"%s\"" cell-style-name)
	   (and (> horiz-span 0)
		(format " table:number-columns-spanned=\"%d\""
			(1+ horiz-span))))))
    (unless contents (setq contents ""))
    (concat
     (cl-assert paragraph-style)
     (format "\n<table:table-cell%s>\n%s\n</table:table-cell>"
	     cell-attributes
	     (let ((table-cell-contents (org-element-contents table-cell)))
	       (if (eq (org-element-class (car table-cell-contents)) 'element)
		   contents
		 (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
			 paragraph-style contents))))
     (let (s)
       (dotimes (_ horiz-span s)
	 (setq s (concat s "\n<table:covered-table-cell/>"))))
     "\n")))


;;;; Table Row

(defun org-odt-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to ODT.
CONTENTS is the contents of the row.  INFO is a plist used as a
communication channel."
  ;; Rules are ignored since table separators are deduced from
  ;; borders of the current row.
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((rowgroup-tags
	    (if (and (= 1 (org-export-table-row-group table-row info))
		     (org-export-table-has-header-p
		      (org-export-get-parent-table table-row) info))
		;; If the row belongs to the first rowgroup and the
		;; table has more than one row groups, then this row
		;; belongs to the header row group.
		'("\n<table:table-header-rows>" . "\n</table:table-header-rows>")
	      ;; Otherwise, it belongs to non-header row group.
	      '("\n<table:table-rows>" . "\n</table:table-rows>"))))
      (concat
       ;; Does this row begin a rowgroup?
       (when (org-export-table-row-starts-rowgroup-p table-row info)
	 (car rowgroup-tags))
       ;; Actual table row
       (format "\n<table:table-row>\n%s\n</table:table-row>" contents)
       ;; Does this row end a rowgroup?
       (when (org-export-table-row-ends-rowgroup-p table-row info)
	 (cdr rowgroup-tags))))))


;;;; Table

(defun org-odt-table-first-row-data-cells (table info)
  (let ((table-row
	 (org-element-map table 'table-row
	   (lambda (row)
	     (unless (eq (org-element-property :type row) 'rule) row))
	   info 'first-match))
	(special-column-p (org-export-table-has-special-column-p table)))
    (if (not special-column-p) (org-element-contents table-row)
      (cdr (org-element-contents table-row)))))

(defun org-odt--table (table contents info)
  "Transcode a TABLE element from Org to ODT.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (cl-case (org-element-property :type table)
    ;; Case 1: table.el doesn't support export to OD format.  Strip
    ;; such tables from export.
    (table.el
     (prog1 nil
       (message
	(concat
	 "(ox-odt): Found table.el-type table in the source Org file."
	 "  table.el doesn't support export to ODT format."
	 "  Stripping the table from export."))))
    ;; Case 2: Native Org tables.
    (otherwise
     (let* ((captions (org-odt-format-label table info 'definition))
	    (caption (car captions)) (short-caption (cdr captions))
	    (attributes (org-export-read-attribute :attr_odt table))
	    (custom-table-style (nth 1 (org-odt-table-style-spec table info)))
	    (table-column-specs
	     (lambda (table info)
	       (let* ((table-style (or custom-table-style "OrgTable"))
		      (column-style (format "%sColumn" table-style)))
		 (mapconcat
		  (lambda (table-cell)
		    (let ((width (1+ (or (org-export-table-cell-width
					  table-cell info) 0)))
			  (s (format
			      "\n<table:table-column table:style-name=\"%s\"/>"
			      column-style))
			  out)
		      (dotimes (_ width out) (setq out (concat s out)))))
		  (org-odt-table-first-row-data-cells table info) "\n")))))
       (concat
	;; caption.
	(when caption
	  (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		  "Table" caption))
	;; begin table.
	(let* ((automatic-name
		(org-odt-add-automatic-style "Table" attributes)))
	  (format
	   "\n<table:table table:style-name=\"%s\"%s>"
	   (or custom-table-style (cdr automatic-name) "OrgTable")
	   (concat (when short-caption
		     (format " table:name=\"%s\"" short-caption)))))
	;; column specification.
	(funcall table-column-specs table info)
	;; actual contents.
	"\n" contents
	;; end table.
	"</table:table>")))))

(defun org-odt-table (table contents info)
  "Transcode a TABLE element from Org to ODT.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information.

Use `org-odt--table' to typeset the table.  Handle details
pertaining to indentation here."
  (let* ((--element-preceded-by-table-p
	  (lambda (element info)
	    (cl-loop for el in (org-export-get-previous-element element info t)
		     thereis (eq (org-element-type el) 'table))))
	 (--walk-list-genealogy-and-collect-tags
	  (lambda (table info)
	    (let* ((genealogy (org-element-lineage table))
		   (list-genealogy
		    (when (eq (org-element-type (car genealogy)) 'item)
		      (cl-loop for el in genealogy
			       when (memq (org-element-type el)
					  '(item plain-list))
			       collect el)))
		   (llh-genealogy
		    (apply #'nconc
			   (cl-loop
			    for el in genealogy
			    when (and (eq (org-element-type el) 'headline)
				      (org-export-low-level-p el info))
			    collect
			    (list el
				  (assq 'headline
					(org-element-contents
					 (org-export-get-parent el)))))))
		   parent-list)
	      (nconc
	       ;; Handle list genealogy.
	       (cl-loop
		for el in list-genealogy collect
		(cl-case (org-element-type el)
		  (plain-list
		   (setq parent-list el)
		   (cons "</text:list>"
			 (format "\n<text:list text:style-name=\"%s\" %s>"
				 (cl-case (org-element-property :type el)
				   (ordered "OrgNumberedList")
				   (unordered "OrgBulletedList")
				   (descriptive-1 "OrgDescriptionList")
				   (descriptive-2 "OrgDescriptionList"))
				 "text:continue-numbering=\"true\"")))
		  (item
		   (cond
		    ((not parent-list)
		     (if (funcall --element-preceded-by-table-p table info)
			 '("</text:list-header>" . "<text:list-header>")
		       '("</text:list-item>" . "<text:list-header>")))
		    ((funcall --element-preceded-by-table-p
			      parent-list info)
		     '("</text:list-header>" . "<text:list-header>"))
		    (t '("</text:list-item>" . "<text:list-item>"))))))
	       ;; Handle low-level headlines.
	       (cl-loop for el in llh-genealogy
			with step = 'item collect
			(cl-case step
			  (plain-list
			   (setq step 'item) ; Flip-flop
			   (setq parent-list el)
			   (cons "</text:list>"
				 (format "\n<text:list text:style-name=\"%s\" %s>"
					 (if (org-export-numbered-headline-p
					      el info)
					     "OrgNumberedList"
					   "OrgBulletedList")
					 "text:continue-numbering=\"true\"")))
			  (item
			   (setq step 'plain-list) ; Flip-flop
			   (cond
			    ((not parent-list)
			     (if (funcall --element-preceded-by-table-p table info)
				 '("</text:list-header>" . "<text:list-header>")
			       '("</text:list-item>" . "<text:list-header>")))
			    ((let ((section? (org-export-get-previous-element
					      parent-list info)))
			       (and section?
				    (eq (org-element-type section?) 'section)
				    (assq 'table (org-element-contents section?))))
			     '("</text:list-header>" . "<text:list-header>"))
			    (t
			     '("</text:list-item>" . "<text:list-item>"))))))))))
	 (close-open-tags (funcall --walk-list-genealogy-and-collect-tags
				   table info)))
    ;; OpenDocument schema does not permit table to occur within a
    ;; list item.

    ;; One solution - the easiest and lightweight, in terms of
    ;; implementation - is to put the table in an indented text box
    ;; and make the text box part of the list-item.  Unfortunately if
    ;; the table is big and spans multiple pages, the text box could
    ;; overflow.  In this case, the following attribute will come
    ;; handy.

    ;; ,---- From OpenDocument-v1.1.pdf
    ;; | 15.27.28 Overflow behavior
    ;; |
    ;; | For text boxes contained within text document, the
    ;; | style:overflow-behavior property specifies the behavior of text
    ;; | boxes where the containing text does not fit into the text
    ;; | box.
    ;; |
    ;; | If the attribute's value is clip, the text that does not fit
    ;; | into the text box is not displayed.
    ;; |
    ;; | If the attribute value is auto-create-new-frame, a new frame
    ;; | will be created on the next page, with the same position and
    ;; | dimensions of the original frame.
    ;; |
    ;; | If the style:overflow-behavior property's value is
    ;; | auto-create-new-frame and the text box has a minimum width or
    ;; | height specified, then the text box will grow until the page
    ;; | bounds are reached before a new frame is created.
    ;; `----

    ;; Unfortunately, LibreOffice-3.4.6 doesn't honor
    ;; auto-create-new-frame property and always resorts to clipping
    ;; the text box.  This results in table being truncated.

    ;; So we solve the problem the hard (and fun) way using list
    ;; continuations.

    ;; The problem only becomes more interesting if you take in to
    ;; account the following facts:
    ;;
    ;; - Description lists are simulated as plain lists.
    ;; - Low-level headlines can be listified.
    ;; - In Org mode, a table can occur not only as a regular list
    ;;   item, but also within description lists and low-level
    ;;   headlines.

    ;; See `org-odt-translate-description-lists' and
    ;; `org-odt-translate-low-level-headlines' for how this is
    ;; tackled.

    (concat "\n"
	    ;; Discontinue the list.
	    (mapconcat 'car close-open-tags "\n")
	    ;; Put the table in an indented section.
	    (let* ((table (org-odt--table table contents info))
		   (level (/ (length (mapcar 'car close-open-tags)) 2))
		   (style (format "OrgIndentedSection-Level-%d" level)))
	      (when table (org-odt-format-section table style)))
	    ;; Continue the list.
	    (mapconcat 'cdr (nreverse close-open-tags) "\n"))))


;;;; Target

(defun org-odt-target (target _contents info)
  "Transcode a TARGET object from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-odt--target "" (org-export-get-reference target info)))


;;;; Timestamp

(defun org-odt-timestamp (timestamp _contents info)
  "Transcode a TIMESTAMP object from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((type (org-element-property :type timestamp)))
    (if (not (plist-get info :odt-use-date-fields))
	(let ((value (org-odt-plain-text
		      (org-timestamp-translate timestamp) info)))
	  (cl-case (org-element-property :type timestamp)
	    ((active active-range)
	     (format "<text:span text:style-name=\"%s\">%s</text:span>"
		     "OrgActiveTimestamp" value))
	    ((inactive inactive-range)
	     (format "<text:span text:style-name=\"%s\">%s</text:span>"
		     "OrgInactiveTimestamp" value))
	    (otherwise value)))
      (cl-case type
	(active
	 (format "<text:span text:style-name=\"%s\">%s</text:span>"
		 "OrgActiveTimestamp"
		 (format "&lt;%s&gt;" (org-odt--format-timestamp timestamp))))
	(inactive
	 (format "<text:span text:style-name=\"%s\">%s</text:span>"
		 "OrgInactiveTimestamp"
		 (format "[%s]" (org-odt--format-timestamp timestamp))))
	(active-range
	 (format "<text:span text:style-name=\"%s\">%s</text:span>"
		 "OrgActiveTimestamp"
		 (format "&lt;%s&gt;&#x2013;&lt;%s&gt;"
			 (org-odt--format-timestamp timestamp)
			 (org-odt--format-timestamp timestamp 'end))))
	(inactive-range
	 (format "<text:span text:style-name=\"%s\">%s</text:span>"
		 "OrgInactiveTimestamp"
		 (format "[%s]&#x2013;[%s]"
			 (org-odt--format-timestamp timestamp)
			 (org-odt--format-timestamp timestamp 'end))))
	(otherwise
	 (format "<text:span text:style-name=\"%s\">%s</text:span>"
		 "OrgDiaryTimestamp"
		 (org-odt-plain-text (org-timestamp-translate timestamp)
				     info)))))))


;;;; Underline

(defun org-odt-underline (_underline contents _info)
  "Transcode UNDERLINE from Org to ODT.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "Underline" contents))


;;;; Verbatim

(defun org-odt-verbatim (verbatim _contents _info)
  "Transcode a VERBATIM object from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "OrgCode" (org-odt--encode-plain-text
		     (org-element-property :value verbatim))))


;;;; Verse Block

(defun org-odt-verse-block (_verse-block contents _info)
  "Transcode a VERSE-BLOCK element from Org to ODT.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  (format "\n<text:p text:style-name=\"OrgVerse\">%s</text:p>"
	  (replace-regexp-in-string
	   ;; Replace leading tabs and spaces.
	   "^[ \t]+" #'org-odt--encode-tabs-and-spaces
	   ;; Add line breaks to each line of verse.
	   (replace-regexp-in-string
	    "\\(<text:line-break/>\\)?[ \t]*$" "<text:line-break/>" contents))))



;;; Filters

;;; Images

(defun org-odt--translate-image-links (data _backend info)
  (org-export-insert-image-links data info org-odt-inline-image-rules))

;;;; LaTeX fragments

(defun org-odt--translate-latex-fragments (tree _backend info)
  (let ((processing-type (plist-get info :with-latex))
	(count 0))
    ;; Normalize processing-type to one of dvipng, mathml or verbatim.
    ;; If the desired converter is not available, force verbatim
    ;; processing.
    (cl-case processing-type
      ((t mathml)
       (if (and (fboundp 'org-format-latex-mathml-available-p)
		(org-format-latex-mathml-available-p))
	   (setq processing-type 'mathml)
	 (message "LaTeX to MathML converter not available.")
	 (setq processing-type 'verbatim)))
      ((dvipng imagemagick)
       (unless (and (org-check-external-command "latex" "" t)
		    (org-check-external-command
		     (if (eq processing-type 'dvipng) "dvipng" "convert") "" t))
	 (message "LaTeX to PNG converter not available.")
	 (setq processing-type 'verbatim)))
      (otherwise
       (message "Unknown LaTeX option.  Forcing verbatim.")
       (setq processing-type 'verbatim)))

    ;; Store normalized value for later use.
    (when (plist-get info :with-latex)
      (plist-put info :with-latex processing-type))
    (message "Formatting LaTeX using %s" processing-type)

    ;; Convert `latex-fragment's and `latex-environment's.
    (when (memq processing-type '(mathml dvipng imagemagick))
      (org-element-map tree '(latex-fragment latex-environment)
	(lambda (latex-*)
	  (cl-incf count)
	  (let* ((latex-frag (org-element-property :value latex-*))
		 (input-file (plist-get info :input-file))
		 (cache-dir (file-name-directory input-file))
		 (cache-subdir (concat
				(cl-case processing-type
				  ((dvipng imagemagick) "ltxpng/")
				  (mathml "ltxmathml/"))
				(file-name-sans-extension
				 (file-name-nondirectory input-file))))
		 (display-msg
		  (cl-case processing-type
		    ((dvipng imagemagick)
		     (format "Creating LaTeX Image %d..." count))
		    (mathml (format "Creating MathML snippet %d..." count))))
		 ;; Get an Org-style link to PNG image or the MathML
		 ;; file.
		 (link
		  (with-temp-buffer
		    (insert latex-frag)
		    ;; When converting to a PNG image, make sure to
		    ;; copy all LaTeX header specifications from the
		    ;; Org source.
		    (unless (eq processing-type 'mathml)
		      (let ((h (plist-get info :latex-header)))
			(when h
			  (insert "\n"
				  (replace-regexp-in-string
				   "^" "#+LATEX_HEADER: " h)))))
		    (org-format-latex cache-subdir nil nil cache-dir
				      nil display-msg nil
				      processing-type)
		    (goto-char (point-min))
		    (skip-chars-forward " \t\n")
		    (org-element-link-parser))))
	    (if (not (eq 'link (org-element-type link)))
		(message "LaTeX Conversion failed.")
	      ;; Conversion succeeded.  Parse above Org-style link to
	      ;; a `link' object.
	      (let ((replacement
		     (cl-case (org-element-type latex-*)
		       ;;LaTeX environment.  Mimic a "standalone image
		       ;; or formula" by enclosing the `link' in
		       ;; a `paragraph'.  Copy over original
		       ;; attributes, captions to the enclosing
		       ;; paragraph.
		       (latex-environment
			(org-element-adopt-elements
			 (list 'paragraph
			       (list :style "OrgFormula"
				     :name
				     (org-element-property :name latex-*)
				     :caption
				     (org-element-property :caption latex-*)))
			 link))
		       ;; LaTeX fragment.  No special action.
		       (latex-fragment link))))
		;; Note down the object that link replaces.
		(org-element-put-property replacement :replaces
					  (list (org-element-type latex-*)
						(list :value latex-frag)))
		;; Restore blank after initial element or object.
		(org-element-put-property
		 replacement :post-blank
		 (org-element-property :post-blank latex-*))
		;; Replace now.
		(org-element-set-element latex-* replacement)))))
	info nil nil t)))
  tree)


;;;; Description lists

;; This translator is necessary to handle indented tables in a uniform
;; manner.  See comment in `org-odt--table'.

(defun org-odt--translate-description-lists (tree _backend info)
  ;; OpenDocument has no notion of a description list.  So simulate it
  ;; using plain lists.  Description lists in the exported document
  ;; are typeset in the same manner as they are in a typical HTML
  ;; document.
  ;;
  ;; Specifically, a description list like this:
  ;;
  ;;     ,----
  ;;     | - term-1 :: definition-1
  ;;     | - term-2 :: definition-2
  ;;     `----
  ;;
  ;; gets translated in to the following form:
  ;;
  ;;     ,----
  ;;     | - term-1
  ;;     |   - definition-1
  ;;     | - term-2
  ;;     |   - definition-2
  ;;     `----
  ;;
  ;; Further effect is achieved by fixing the OD styles as below:
  ;;
  ;; 1. Set the :type property of the simulated lists to
  ;;    `descriptive-1' and `descriptive-2'.  Map these to list-styles
  ;;    that has *no* bullets whatsoever.
  ;;
  ;; 2. The paragraph containing the definition term is styled to be
  ;;    in bold.
  ;;
  (org-element-map tree 'plain-list
    (lambda (el)
      (when (eq (org-element-property :type el) 'descriptive)
	(org-element-set-element
	 el
	 (apply 'org-element-adopt-elements
		(list 'plain-list (list :type 'descriptive-1))
		(mapcar
		 (lambda (item)
		   (org-element-adopt-elements
		    (list 'item (list :checkbox (org-element-property
						 :checkbox item)))
		    (list 'paragraph (list :style "Text_20_body_20_bold")
			  (or (org-element-property :tag item) "(no term)"))
		    (org-element-adopt-elements
		     (list 'plain-list (list :type 'descriptive-2))
		     (apply 'org-element-adopt-elements
			    (list 'item nil)
			    (org-element-contents item)))))
		 (org-element-contents el)))))
      nil)
    info)
  tree)

;;;; List tables

;; Lists that are marked with attribute `:list-table' are called as
;; list tables.  They will be rendered as a table within the exported
;; document.

;; Consider an example.  The following list table
;;
;; #+attr_odt :list-table t
;; - Row 1
;;   - 1.1
;;   - 1.2
;;   - 1.3
;; - Row 2
;;   - 2.1
;;   - 2.2
;;   - 2.3
;;
;; will be exported as though it were an Org table like the one show
;; below.
;;
;; | Row 1 | 1.1 | 1.2 | 1.3 |
;; | Row 2 | 2.1 | 2.2 | 2.3 |
;;
;; Note that org-tables are NOT multi-line and each line is mapped to
;; a unique row in the exported document.  So if an exported table
;; needs to contain a single paragraph (with copious text) it needs to
;; be typed up in a single line.  Editing such long lines using the
;; table editor will be a cumbersome task.  Furthermore inclusion of
;; multi-paragraph text in a table cell is well-nigh impossible.
;;
;; A LIST-TABLE circumvents above problems.
;;
;; Note that in the example above the list items could be paragraphs
;; themselves and the list can be arbitrarily deep.
;;
;; Inspired by following thread:
;; https://lists.gnu.org/archive/html/emacs-orgmode/2011-03/msg01101.html

;; Translate lists to tables

(defun org-odt--translate-list-tables (tree _backend info)
  (org-element-map tree 'plain-list
    (lambda (l1-list)
      (when (org-export-read-attribute :attr_odt l1-list :list-table)
	;; Replace list with table.
	(org-element-set-element
	 l1-list
	 ;; Build replacement table.
	 (apply 'org-element-adopt-elements
		(list 'table '(:type org :attr_odt (":style \"GriddedTable\"")))
		(org-element-map l1-list 'item
		  (lambda (l1-item)
		    (let* ((l1-item-contents (org-element-contents l1-item))
			   l1-item-leading-text l2-list)
		      ;; Remove Level-2 list from the Level-item.  It
		      ;; will be subsequently attached as table-cells.
		      (let ((cur l1-item-contents) prev)
			(while (and cur (not (eq (org-element-type (car cur))
						 'plain-list)))
			  (setq prev cur)
			  (setq cur (cdr cur)))
			(when prev
			  (setcdr prev nil)
			  (setq l2-list (car cur)))
			(setq l1-item-leading-text l1-item-contents))
		      ;; Level-1 items start a table row.
		      (apply 'org-element-adopt-elements
			     (list 'table-row (list :type 'standard))
			     ;;  Leading text of level-1 item define
			     ;;  the first table-cell.
			     (apply 'org-element-adopt-elements
				    (list 'table-cell nil)
				    l1-item-leading-text)
			     ;; Level-2 items define subsequent
			     ;; table-cells of the row.
			     (org-element-map l2-list 'item
			       (lambda (l2-item)
				 (apply 'org-element-adopt-elements
					(list 'table-cell nil)
					(org-element-contents l2-item)))
			       info nil 'item))))
		  info nil 'item))))
      nil)
    info)
  tree)


;;; Interactive functions

(defun org-odt-create-manifest-file-entry (&rest args)
  (push args org-odt-manifest-file-entries))

(defun org-odt-write-manifest-file ()
  (make-directory (concat org-odt-zip-dir "META-INF"))
  (let ((manifest-file (concat org-odt-zip-dir "META-INF/manifest.xml")))
    (with-current-buffer
	(let ((nxml-auto-insert-xml-declaration-flag nil))
	  (find-file-noselect manifest-file t))
      (insert
       "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
     <manifest:manifest xmlns:manifest=\"urn:oasis:names:tc:opendocument:xmlns:manifest:1.0\" manifest:version=\"1.2\">\n")
      (dolist (file-entry org-odt-manifest-file-entries)
	(let* ((version (nth 2 file-entry))
	       (extra (if (not version) ""
			(format " manifest:version=\"%s\"" version))))
	  (insert
	   (format org-odt-manifest-file-entry-tag
		   (nth 0 file-entry) (nth 1 file-entry) extra))))
      (insert "\n</manifest:manifest>"))))

(defmacro org-odt--export-wrap (out-file &rest body)
  `(let* ((--out-file ,out-file)
	  (out-file-type (file-name-extension --out-file))
	  (org-odt-xml-files '("META-INF/manifest.xml" "content.xml"
			       "meta.xml" "styles.xml"))
	  ;; Initialize temporary workarea.  All files that end up in
	  ;; the exported document get parked/created here.
	  (org-odt-zip-dir (file-name-as-directory
			    (make-temp-file (format "%s-" out-file-type) t)))
	  (org-odt-manifest-file-entries nil)
	  (--cleanup-xml-buffers
	   (lambda ()
	     ;; Kill all XML buffers.
	     (dolist (file org-odt-xml-files)
	       (let ((buf (find-buffer-visiting
			   (concat org-odt-zip-dir file))))
		 (when buf
		   (with-current-buffer buf
		     (set-buffer-modified-p nil)
		     (kill-buffer buf)))))
	     ;; Delete temporary directory and also other embedded
	     ;; files that get copied there.
	     (delete-directory org-odt-zip-dir t))))
     (condition-case err
	 (progn
	   (unless (executable-find "zip")
	     ;; Not at all OSes ship with zip by default
	     (error "Executable \"zip\" needed for creating OpenDocument files"))
	   ;; Do export.  This creates a bunch of xml files ready to be
	   ;; saved and zipped.
	   (progn ,@body)
	   ;; Create a manifest entry for content.xml.
	   (org-odt-create-manifest-file-entry "text/xml" "content.xml")
	   ;; Write mimetype file
	   (let* ((mimetypes
		   '(("odt" . "application/vnd.oasis.opendocument.text")
		     ("odf" .  "application/vnd.oasis.opendocument.formula")))
		  (mimetype (cdr (assoc-string out-file-type mimetypes t))))
	     (unless mimetype
	       (error "Unknown OpenDocument backend %S" out-file-type))
	     (write-region mimetype nil (concat org-odt-zip-dir "mimetype"))
	     (org-odt-create-manifest-file-entry mimetype "/" "1.2"))
	   ;; Write out the manifest entries before zipping
	   (org-odt-write-manifest-file)
	   ;; Save all XML files.
	   (dolist (file org-odt-xml-files)
	     (let ((buf (find-buffer-visiting
			 (concat org-odt-zip-dir file))))
	       (when buf
		 (with-current-buffer buf
		   ;; Prettify output if needed.
		   (when org-odt-prettify-xml
		     (indent-region (point-min) (point-max)))
		   (save-buffer 0)))))
	   ;; Run zip.
	   (let* ((target --out-file)
		  (target-name (file-name-nondirectory target))
		  (cmds `(("zip" "-mX0" ,target-name "mimetype")
			  ("zip" "-rmTq" ,target-name "."))))
	     ;; If a file with same name as the desired output file
	     ;; exists, remove it.
	     (when (file-exists-p target)
	       (delete-file target))
	     ;; Zip up the xml files.
	     (let ((coding-system-for-write 'no-conversion) exitcode err-string)
	       (message "Creating ODT file...")
	       ;; Switch temporarily to content.xml.  This way Zip
	       ;; process will inherit `org-odt-zip-dir' as the current
	       ;; directory.
	       (with-current-buffer
		   (find-file-noselect (concat org-odt-zip-dir "content.xml") t)
		 (dolist (cmd cmds)
		   (message "Running %s" (mapconcat 'identity cmd " "))
		   (setq err-string
			 (with-output-to-string
			   (setq exitcode
				 (apply 'call-process (car cmd)
					nil standard-output nil (cdr cmd)))))
		   (or (zerop exitcode)
		       (error (concat "Unable to create OpenDocument file."
				      "  Zip failed with error (%s)")
			      err-string)))))
	     ;; Move the zip file from temporary work directory to
	     ;; user-mandated location.
	     (rename-file (concat org-odt-zip-dir target-name) target)
	     (message "Created %s" (expand-file-name target))
	     ;; Cleanup work directory and work files.
	     (funcall --cleanup-xml-buffers)
	     ;; Open the OpenDocument file in archive-mode for
	     ;; examination.
	     (find-file-noselect target t)
	     ;; Return exported file.
	     (cond
	      ;; Case 1: Conversion desired on exported file.  Run the
	      ;; converter on the OpenDocument file.  Return the
	      ;; converted file.
	      (org-odt-preferred-output-format
	       (or (org-odt-convert target org-odt-preferred-output-format)
		   target))
	      ;; Case 2: No further conversion.  Return exported
	      ;; OpenDocument file.
	      (t target))))
       (error
	;; Cleanup work directory and work files.
	(funcall --cleanup-xml-buffers)
	(message "OpenDocument export failed: %s"
		 (error-message-string err))))))


;;;; Export to OpenDocument formula

;;;###autoload
(defun org-odt-export-as-odf (latex-frag &optional odf-file)
  "Export LATEX-FRAG as OpenDocument formula file ODF-FILE.
Use `org-create-math-formula' to convert LATEX-FRAG first to
MathML.  When invoked as an interactive command, use
`org-latex-regexps' to infer LATEX-FRAG from currently active
region.  If no LaTeX fragments are found, prompt for it.  Push
MathML source to kill ring depending on the value of
`org-export-copy-to-kill-ring'."
  (interactive
   `(,(let (frag)
	(setq frag (and (setq frag (and (region-active-p)
					(buffer-substring (region-beginning)
							  (region-end))))
			(cl-loop for e in org-latex-regexps
				 thereis (when (string-match (nth 1 e) frag)
					   (match-string (nth 2 e) frag)))))
	(read-string "LaTeX Fragment: " frag nil frag))
     ,(let ((odf-filename (expand-file-name
			   (concat
			    (file-name-sans-extension
			     (or (file-name-nondirectory buffer-file-name)))
			    "." "odf")
			   (file-name-directory buffer-file-name))))
	(read-file-name "ODF filename: " nil odf-filename nil
			(file-name-nondirectory odf-filename)))))
  (let ((filename (or odf-file
		      (expand-file-name
		       (concat
			(file-name-sans-extension
			 (or (file-name-nondirectory buffer-file-name)))
			"." "odf")
		       (file-name-directory buffer-file-name)))))
    (org-odt--export-wrap
     filename
     (let* ((buffer (progn
		      (require 'nxml-mode)
		      (let ((nxml-auto-insert-xml-declaration-flag nil))
			(find-file-noselect (concat org-odt-zip-dir
						    "content.xml") t))))
	    (coding-system-for-write 'utf-8)
	    (save-buffer-coding-system 'utf-8))
       (set-buffer buffer)
       (set-buffer-file-coding-system coding-system-for-write)
       (let ((mathml (org-create-math-formula latex-frag)))
	 (unless mathml (error "No Math formula created"))
	 (insert mathml)
	 ;; Add MathML to kill ring, if needed.
	 (when (org-export--copy-to-kill-ring-p)
	   (org-kill-new (buffer-string))))))))

;;;###autoload
(defun org-odt-export-as-odf-and-open ()
  "Export LaTeX fragment as OpenDocument formula and immediately open it.
Use `org-odt-export-as-odf' to read LaTeX fragment and OpenDocument
formula file."
  (interactive)
  (org-open-file (call-interactively 'org-odt-export-as-odf) 'system))


;;;; Export to OpenDocument Text

;;;###autoload
(defun org-odt-export-to-odt (&optional async subtreep visible-only ext-plist)
  "Export current buffer to a ODT file.

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

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".odt" subtreep)))
    (if async
	(org-export-async-start (lambda (f) (org-export-add-to-stack f 'odt))
	  `(expand-file-name
	    (org-odt--export-wrap
	     ,outfile
	     (let* ((org-odt-embedded-images-count 0)
		    (org-odt-embedded-formulas-count 0)
		    (org-odt-automatic-styles nil)
		    (org-odt-object-counters nil)
		    ;; Let `htmlfontify' know that we are interested in
		    ;; collecting styles.
		    (hfy-user-sheet-assoc nil))
	       ;; Initialize content.xml and kick-off the export
	       ;; process.
	       (let ((out-buf
		      (progn
			(require 'nxml-mode)
			(let ((nxml-auto-insert-xml-declaration-flag nil))
			  (find-file-noselect
			   (concat org-odt-zip-dir "content.xml") t))))
		     (output (org-export-as
			      'odt ,subtreep ,visible-only nil ,ext-plist)))
		 (with-current-buffer out-buf
		   (erase-buffer)
		   (insert output)))))))
      (org-odt--export-wrap
       outfile
       (let* ((org-odt-embedded-images-count 0)
	      (org-odt-embedded-formulas-count 0)
	      (org-odt-automatic-styles nil)
	      (org-odt-object-counters nil)
	      ;; Let `htmlfontify' know that we are interested in collecting
	      ;; styles.
	      (hfy-user-sheet-assoc nil))
	 ;; Initialize content.xml and kick-off the export process.
	 (let ((output (org-export-as 'odt subtreep visible-only nil ext-plist))
	       (out-buf (progn
			  (require 'nxml-mode)
			  (let ((nxml-auto-insert-xml-declaration-flag nil))
			    (find-file-noselect
			     (concat org-odt-zip-dir "content.xml") t)))))
	   (with-current-buffer out-buf (erase-buffer) (insert output))))))))


;;;; Convert between OpenDocument and other formats

(defun org-odt-reachable-p (in-fmt out-fmt)
  "Return non-nil if IN-FMT can be converted to OUT-FMT."
  (catch 'done
    (let ((reachable-formats (org-odt-do-reachable-formats in-fmt)))
      (dolist (e reachable-formats)
	(let ((out-fmt-spec (assoc out-fmt (cdr e))))
	  (when out-fmt-spec
	    (throw 'done (cons (car e) out-fmt-spec))))))))

(defun org-odt-do-convert (in-file out-fmt &optional open)
  "Workhorse routine for `org-odt-convert'."
  (require 'browse-url)
  (let* ((in-file (let ((f (expand-file-name (or in-file buffer-file-name))))
		    (if (file-readable-p f) f
		      (error "Cannot read %s" in-file))))
	 (in-fmt (file-name-extension in-file))
	 (out-fmt (or out-fmt (error "Output format unspecified")))
	 (how (or (org-odt-reachable-p in-fmt out-fmt)
		  (error "Cannot convert from %s format to %s format?"
			 in-fmt out-fmt)))
	 (convert-process (car how))
	 (out-file (concat (file-name-sans-extension in-file) "."
			   (nth 1 (or (cdr how) out-fmt))))
	 (extra-options (or (nth 2 (cdr how)) ""))
	 (out-dir (file-name-directory in-file))
	 (cmd (format-spec convert-process
			   `((?i . ,(shell-quote-argument in-file))
			     (?I . ,(browse-url-file-url in-file))
			     (?f . ,out-fmt)
			     (?o . ,out-file)
			     (?O . ,(browse-url-file-url out-file))
			     (?d . , (shell-quote-argument out-dir))
			     (?D . ,(browse-url-file-url out-dir))
			     (?x . ,extra-options)))))
    (when (file-exists-p out-file)
      (delete-file out-file))

    (message "Executing %s" cmd)
    (let ((cmd-output (shell-command-to-string cmd)))
      (message "%s" cmd-output))

    (cond
     ((file-exists-p out-file)
      (message "Exported to %s" out-file)
      (when open
	(message "Opening %s..."  out-file)
	(org-open-file out-file 'system))
      out-file)
     (t
      (message "Export to %s failed" out-file)
      nil))))

(defun org-odt-do-reachable-formats (in-fmt)
  "Return verbose info about formats to which IN-FMT can be converted.
Return a list where each element is of the
form (CONVERTER-PROCESS . OUTPUT-FMT-ALIST).  See
`org-odt-convert-processes' for CONVERTER-PROCESS and see
`org-odt-convert-capabilities' for OUTPUT-FMT-ALIST."
  (let* ((converter
	  (and org-odt-convert-process
	       (cadr (assoc-string org-odt-convert-process
				   org-odt-convert-processes t))))
	 (capabilities
	  (and org-odt-convert-process
	       (cadr (assoc-string org-odt-convert-process
				   org-odt-convert-processes t))
	       org-odt-convert-capabilities))
	 reachable-formats)
    (when converter
      (dolist (c capabilities)
	(when (member in-fmt (nth 1 c))
	  (push (cons converter (nth 2 c)) reachable-formats))))
    reachable-formats))

(defun org-odt-reachable-formats (in-fmt)
  "Return list of formats to which IN-FMT can be converted.
The list of the form (OUTPUT-FMT-1 OUTPUT-FMT-2 ...)."
  (copy-sequence
   (apply #'append (mapcar
		    (lambda (e) (mapcar #'car (cdr e)))
		    (org-odt-do-reachable-formats in-fmt)))))

(defun org-odt-convert-read-params ()
  "Return IN-FILE and OUT-FMT params for `org-odt-do-convert'.
This is a helper routine for interactive use."
  (let* ((input (if (featurep 'ido) 'ido-completing-read 'completing-read))
	 (in-file (read-file-name "File to be converted: "
				  nil buffer-file-name t))
	 (in-fmt (file-name-extension in-file))
	 (out-fmt-choices (org-odt-reachable-formats in-fmt))
	 (out-fmt
	  (or (and out-fmt-choices
		   (funcall input "Output format: "
			    out-fmt-choices nil nil nil))
	      (error
	       "No known converter or no known output formats for %s files"
	       in-fmt))))
    (list in-file out-fmt)))

;;;###autoload
(defun org-odt-convert (&optional in-file out-fmt open)
  "Convert IN-FILE to format OUT-FMT using a command line converter.
IN-FILE is the file to be converted.  If unspecified, it defaults
to variable `buffer-file-name'.  OUT-FMT is the desired output
format.  Use `org-odt-convert-process' as the converter.  If OPEN
is non-nil then the newly converted file is opened using
`org-open-file'."
  (interactive
   (append (org-odt-convert-read-params) current-prefix-arg))
  (org-odt-do-convert in-file out-fmt open))

;;; Library Initializations

(dolist (desc org-odt-file-extensions)
  ;; Let Emacs open all OpenDocument files in archive mode.
  (add-to-list 'auto-mode-alist
	       (cons (concat  "\\." (car desc) "\\'") 'archive-mode)))

(provide 'ox-odt)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-odt.el ends here
