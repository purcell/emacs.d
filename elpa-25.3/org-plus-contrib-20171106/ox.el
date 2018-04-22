;;; ox.el --- Export Framework for Org Mode          -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2017 Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou at gmail dot com>
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
;; This library implements a generic export engine for Org, built on
;; its syntactical parser: Org Elements.
;;
;; Besides that parser, the generic exporter is made of three distinct
;; parts:
;;
;; - The communication channel consists of a property list, which is
;;   created and updated during the process.  Its use is to offer
;;   every piece of information, would it be about initial environment
;;   or contextual data, all in a single place.
;;
;; - The transcoder walks the parse tree, ignores or treat as plain
;;   text elements and objects according to export options, and
;;   eventually calls back-end specific functions to do the real
;;   transcoding, concatenating their return value along the way.
;;
;; - The filter system is activated at the very beginning and the very
;;   end of the export process, and each time an element or an object
;;   has been converted.  It is the entry point to fine-tune standard
;;   output from back-end transcoders.  See "The Filter System"
;;   section for more information.
;;
;; The core functions is `org-export-as'.  It returns the transcoded
;; buffer as a string.  Its derivatives are `org-export-to-buffer' and
;; `org-export-to-file'.
;;
;; An export back-end is defined with `org-export-define-backend'.
;; This function can also support specific buffer keywords, OPTION
;; keyword's items and filters.  Refer to function's documentation for
;; more information.
;;
;; If the new back-end shares most properties with another one,
;; `org-export-define-derived-backend' can be used to simplify the
;; process.
;;
;; Any back-end can define its own variables.  Among them, those
;; customizable should belong to the `org-export-BACKEND' group.
;;
;; Tools for common tasks across back-ends are implemented in the
;; following part of the file.
;;
;; Eventually, a dispatcher (`org-export-dispatch') is provided in the
;; last one.
;;
;; See <http://orgmode.org/worg/dev/org-export-reference.html> for
;; more information.

;;; Code:

(require 'cl-lib)
(require 'ob-exp)
(require 'org-element)
(require 'org-macro)
(require 'tabulated-list)

(declare-function org-src-coderef-format "org-src" (&optional element))
(declare-function org-src-coderef-regexp "org-src" (fmt &optional label))
(declare-function org-publish "ox-publish" (project &optional force async))
(declare-function org-publish-all "ox-publish" (&optional force async))
(declare-function org-publish-current-file "ox-publish" (&optional force async))
(declare-function org-publish-current-project "ox-publish" (&optional force async))

(defvar org-publish-project-alist)
(defvar org-table-number-fraction)
(defvar org-table-number-regexp)


;;; Internal Variables
;;
;; Among internal variables, the most important is
;; `org-export-options-alist'.  This variable define the global export
;; options, shared between every exporter, and how they are acquired.

(defconst org-export-max-depth 19
  "Maximum nesting depth for headlines, counting from 0.")

(defconst org-export-options-alist
  '((:title "TITLE" nil nil parse)
    (:date "DATE" nil nil parse)
    (:author "AUTHOR" nil user-full-name parse)
    (:email "EMAIL" nil user-mail-address t)
    (:language "LANGUAGE" nil org-export-default-language t)
    (:select-tags "SELECT_TAGS" nil org-export-select-tags split)
    (:exclude-tags "EXCLUDE_TAGS" nil org-export-exclude-tags split)
    (:creator "CREATOR" nil org-export-creator-string)
    (:headline-levels nil "H" org-export-headline-levels)
    (:preserve-breaks nil "\\n" org-export-preserve-breaks)
    (:section-numbers nil "num" org-export-with-section-numbers)
    (:time-stamp-file nil "timestamp" org-export-time-stamp-file)
    (:with-archived-trees nil "arch" org-export-with-archived-trees)
    (:with-author nil "author" org-export-with-author)
    (:with-broken-links nil "broken-links" org-export-with-broken-links)
    (:with-clocks nil "c" org-export-with-clocks)
    (:with-creator nil "creator" org-export-with-creator)
    (:with-date nil "date" org-export-with-date)
    (:with-drawers nil "d" org-export-with-drawers)
    (:with-email nil "email" org-export-with-email)
    (:with-emphasize nil "*" org-export-with-emphasize)
    (:with-entities nil "e" org-export-with-entities)
    (:with-fixed-width nil ":" org-export-with-fixed-width)
    (:with-footnotes nil "f" org-export-with-footnotes)
    (:with-inlinetasks nil "inline" org-export-with-inlinetasks)
    (:with-latex nil "tex" org-export-with-latex)
    (:with-planning nil "p" org-export-with-planning)
    (:with-priority nil "pri" org-export-with-priority)
    (:with-properties nil "prop" org-export-with-properties)
    (:with-smart-quotes nil "'" org-export-with-smart-quotes)
    (:with-special-strings nil "-" org-export-with-special-strings)
    (:with-statistics-cookies nil "stat" org-export-with-statistics-cookies)
    (:with-sub-superscript nil "^" org-export-with-sub-superscripts)
    (:with-toc nil "toc" org-export-with-toc)
    (:with-tables nil "|" org-export-with-tables)
    (:with-tags nil "tags" org-export-with-tags)
    (:with-tasks nil "tasks" org-export-with-tasks)
    (:with-timestamps nil "<" org-export-with-timestamps)
    (:with-title nil "title" org-export-with-title)
    (:with-todo-keywords nil "todo" org-export-with-todo-keywords))
  "Alist between export properties and ways to set them.

The key of the alist is the property name, and the value is a list
like (KEYWORD OPTION DEFAULT BEHAVIOR) where:

KEYWORD is a string representing a buffer keyword, or nil.  Each
  property defined this way can also be set, during subtree
  export, through a headline property named after the keyword
  with the \"EXPORT_\" prefix (i.e. DATE keyword and EXPORT_DATE
  property).
OPTION is a string that could be found in an #+OPTIONS: line.
DEFAULT is the default value for the property.
BEHAVIOR determines how Org should handle multiple keywords for
  the same property.  It is a symbol among:
  nil       Keep old value and discard the new one.
  t         Replace old value with the new one.
  `space'   Concatenate the values, separating them with a space.
  `newline' Concatenate the values, separating them with
	    a newline.
  `split'   Split values at white spaces, and cons them to the
	    previous list.
  `parse'   Parse value as a list of strings and Org objects,
            which can then be transcoded with, e.g.,
            `org-export-data'.  It implies `space' behavior.

Values set through KEYWORD and OPTION have precedence over
DEFAULT.

All these properties should be back-end agnostic.  Back-end
specific properties are set through `org-export-define-backend'.
Properties redefined there have precedence over these.")

(defconst org-export-special-keywords '("FILETAGS" "SETUPFILE" "OPTIONS")
  "List of in-buffer keywords that require special treatment.
These keywords are not directly associated to a property.  The
way they are handled must be hard-coded into
`org-export--get-inbuffer-options' function.")

(defconst org-export-filters-alist
  '((:filter-body . org-export-filter-body-functions)
    (:filter-bold . org-export-filter-bold-functions)
    (:filter-babel-call . org-export-filter-babel-call-functions)
    (:filter-center-block . org-export-filter-center-block-functions)
    (:filter-clock . org-export-filter-clock-functions)
    (:filter-code . org-export-filter-code-functions)
    (:filter-diary-sexp . org-export-filter-diary-sexp-functions)
    (:filter-drawer . org-export-filter-drawer-functions)
    (:filter-dynamic-block . org-export-filter-dynamic-block-functions)
    (:filter-entity . org-export-filter-entity-functions)
    (:filter-example-block . org-export-filter-example-block-functions)
    (:filter-export-block . org-export-filter-export-block-functions)
    (:filter-export-snippet . org-export-filter-export-snippet-functions)
    (:filter-final-output . org-export-filter-final-output-functions)
    (:filter-fixed-width . org-export-filter-fixed-width-functions)
    (:filter-footnote-definition . org-export-filter-footnote-definition-functions)
    (:filter-footnote-reference . org-export-filter-footnote-reference-functions)
    (:filter-headline . org-export-filter-headline-functions)
    (:filter-horizontal-rule . org-export-filter-horizontal-rule-functions)
    (:filter-inline-babel-call . org-export-filter-inline-babel-call-functions)
    (:filter-inline-src-block . org-export-filter-inline-src-block-functions)
    (:filter-inlinetask . org-export-filter-inlinetask-functions)
    (:filter-italic . org-export-filter-italic-functions)
    (:filter-item . org-export-filter-item-functions)
    (:filter-keyword . org-export-filter-keyword-functions)
    (:filter-latex-environment . org-export-filter-latex-environment-functions)
    (:filter-latex-fragment . org-export-filter-latex-fragment-functions)
    (:filter-line-break . org-export-filter-line-break-functions)
    (:filter-link . org-export-filter-link-functions)
    (:filter-node-property . org-export-filter-node-property-functions)
    (:filter-options . org-export-filter-options-functions)
    (:filter-paragraph . org-export-filter-paragraph-functions)
    (:filter-parse-tree . org-export-filter-parse-tree-functions)
    (:filter-plain-list . org-export-filter-plain-list-functions)
    (:filter-plain-text . org-export-filter-plain-text-functions)
    (:filter-planning . org-export-filter-planning-functions)
    (:filter-property-drawer . org-export-filter-property-drawer-functions)
    (:filter-quote-block . org-export-filter-quote-block-functions)
    (:filter-radio-target . org-export-filter-radio-target-functions)
    (:filter-section . org-export-filter-section-functions)
    (:filter-special-block . org-export-filter-special-block-functions)
    (:filter-src-block . org-export-filter-src-block-functions)
    (:filter-statistics-cookie . org-export-filter-statistics-cookie-functions)
    (:filter-strike-through . org-export-filter-strike-through-functions)
    (:filter-subscript . org-export-filter-subscript-functions)
    (:filter-superscript . org-export-filter-superscript-functions)
    (:filter-table . org-export-filter-table-functions)
    (:filter-table-cell . org-export-filter-table-cell-functions)
    (:filter-table-row . org-export-filter-table-row-functions)
    (:filter-target . org-export-filter-target-functions)
    (:filter-timestamp . org-export-filter-timestamp-functions)
    (:filter-underline . org-export-filter-underline-functions)
    (:filter-verbatim . org-export-filter-verbatim-functions)
    (:filter-verse-block . org-export-filter-verse-block-functions))
  "Alist between filters properties and initial values.

The key of each association is a property name accessible through
the communication channel.  Its value is a configurable global
variable defining initial filters.

This list is meant to install user specified filters.  Back-end
developers may install their own filters using
`org-export-define-backend'.  Filters defined there will always
be prepended to the current list, so they always get applied
first.")

(defconst org-export-default-inline-image-rule
  `(("file" .
     ,(format "\\.%s\\'"
	      (regexp-opt
	       '("png" "jpeg" "jpg" "gif" "tiff" "tif" "xbm"
		 "xpm" "pbm" "pgm" "ppm") t))))
  "Default rule for link matching an inline image.
This rule applies to links with no description.  By default, it
will be considered as an inline image if it targets a local file
whose extension is either \"png\", \"jpeg\", \"jpg\", \"gif\",
\"tiff\", \"tif\", \"xbm\", \"xpm\", \"pbm\", \"pgm\" or \"ppm\".
See `org-export-inline-image-p' for more information about
rules.")

(defconst org-export-ignored-local-variables
  '(org-font-lock-keywords
    org-element--cache org-element--cache-objects org-element--cache-sync-keys
    org-element--cache-sync-requests org-element--cache-sync-timer)
  "List of variables not copied through upon buffer duplication.
Export process takes place on a copy of the original buffer.
When this copy is created, all Org related local variables not in
this list are copied to the new buffer.  Variables with an
unreadable value are also ignored.")

(defvar org-export-async-debug nil
  "Non-nil means asynchronous export process should leave data behind.

This data is found in the appropriate \"*Org Export Process*\"
buffer, and in files prefixed with \"org-export-process\" and
located in `temporary-file-directory'.

When non-nil, it will also set `debug-on-error' to a non-nil
value in the external process.")

(defvar org-export-stack-contents nil
  "Record asynchronously generated export results and processes.
This is an alist: its CAR is the source of the
result (destination file or buffer for a finished process,
original buffer for a running one) and its CDR is a list
containing the back-end used, as a symbol, and either a process
or the time at which it finished.  It is used to build the menu
from `org-export-stack'.")

(defvar org-export-registered-backends nil
  "List of backends currently available in the exporter.
This variable is set with `org-export-define-backend' and
`org-export-define-derived-backend' functions.")

(defvar org-export-dispatch-last-action nil
  "Last command called from the dispatcher.
The value should be a list.  Its CAR is the action, as a symbol,
and its CDR is a list of export options.")

(defvar org-export-dispatch-last-position (make-marker)
  "The position where the last export command was created using the dispatcher.
This marker will be used with `C-u C-c C-e' to make sure export repetition
uses the same subtree if the previous command was restricted to a subtree.")

;; For compatibility with Org < 8
(defvar org-export-current-backend nil
  "Name, if any, of the back-end used during an export process.

Its value is a symbol such as `html', `latex', `ascii', or nil if
the back-end is anonymous (see `org-export-create-backend') or if
there is no export process in progress.

It can be used to teach Babel blocks how to act differently
according to the back-end used.")



;;; User-configurable Variables
;;
;; Configuration for the masses.
;;
;; They should never be accessed directly, as their value is to be
;; stored in a property list (cf. `org-export-options-alist').
;; Back-ends will read their value from there instead.

(defgroup org-export nil
  "Options for exporting Org mode files."
  :tag "Org Export"
  :group 'org)

(defgroup org-export-general nil
  "General options for export engine."
  :tag "Org Export General"
  :group 'org-export)

(defcustom org-export-with-archived-trees 'headline
  "Whether sub-trees with the ARCHIVE tag should be exported.

This can have three different values:
nil         Do not export, pretend this tree is not present.
t           Do export the entire tree.
`headline'  Only export the headline, but skip the tree below it.

This option can also be set with the OPTIONS keyword,
e.g. \"arch:nil\"."
  :group 'org-export-general
  :type '(choice
	  (const :tag "Not at all" nil)
	  (const :tag "Headline only" headline)
	  (const :tag "Entirely" t))
  :safe (lambda (x) (memq x '(t nil headline))))

(defcustom org-export-with-author t
  "Non-nil means insert author name into the exported file.
This option can also be set with the OPTIONS keyword,
e.g. \"author:nil\"."
  :group 'org-export-general
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-with-clocks nil
  "Non-nil means export CLOCK keywords.
This option can also be set with the OPTIONS keyword,
e.g. \"c:t\"."
  :group 'org-export-general
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-with-creator nil
  "Non-nil means the postamble should contain a creator sentence.

The sentence can be set in `org-export-creator-string', which
see.

This option can also be set with the OPTIONS keyword, e.g.,
\"creator:t\"."
  :group 'org-export-general
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-with-date t
  "Non-nil means insert date in the exported document.
This option can also be set with the OPTIONS keyword,
e.g. \"date:nil\"."
  :group 'org-export-general
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-date-timestamp-format nil
  "Time-stamp format string to use for DATE keyword.

The format string, when specified, only applies if date consists
in a single time-stamp.  Otherwise its value will be ignored.

See `format-time-string' for details on how to build this
string."
  :group 'org-export-general
  :type '(choice
	  (string :tag "Time-stamp format string")
	  (const :tag "No format string" nil))
  :safe (lambda (x) (or (null x) (stringp x))))

(defcustom org-export-creator-string
  (format "Emacs %s (Org mode %s)"
	  emacs-version
	  (if (fboundp 'org-version) (org-version) "unknown version"))
  "Information about the creator of the document.
This option can also be set on with the CREATOR keyword."
  :group 'org-export-general
  :type '(string :tag "Creator string")
  :safe #'stringp)

(defcustom org-export-with-drawers '(not "LOGBOOK")
  "Non-nil means export contents of standard drawers.

When t, all drawers are exported.  This may also be a list of
drawer names to export, as strings.  If that list starts with
`not', only drawers with such names will be ignored.

This variable doesn't apply to properties drawers.  See
`org-export-with-properties' instead.

This option can also be set with the OPTIONS keyword,
e.g. \"d:nil\"."
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "All drawers" t)
	  (const :tag "None" nil)
	  (repeat :tag "Selected drawers"
		  (string :tag "Drawer name"))
	  (list :tag "Ignored drawers"
		(const :format "" not)
		(repeat :tag "Specify names of drawers to ignore during export"
			:inline t
			(string :tag "Drawer name"))))
  :safe (lambda (x) (or (booleanp x) (consp x))))

(defcustom org-export-with-email nil
  "Non-nil means insert author email into the exported file.
This option can also be set with the OPTIONS keyword,
e.g. \"email:t\"."
  :group 'org-export-general
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-with-emphasize t
  "Non-nil means interpret *word*, /word/, _word_ and +word+.

If the export target supports emphasizing text, the word will be
typeset in bold, italic, with an underline or strike-through,
respectively.

This option can also be set with the OPTIONS keyword,
e.g. \"*:nil\"."
  :group 'org-export-general
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-exclude-tags '("noexport")
  "Tags that exclude a tree from export.

All trees carrying any of these tags will be excluded from
export.  This is without condition, so even subtrees inside that
carry one of the `org-export-select-tags' will be removed.

This option can also be set with the EXCLUDE_TAGS keyword."
  :group 'org-export-general
  :type '(repeat (string :tag "Tag"))
  :safe (lambda (x) (and (listp x) (cl-every #'stringp x))))

(defcustom org-export-with-fixed-width t
  "Non-nil means export lines starting with \":\".
This option can also be set with the OPTIONS keyword,
e.g. \"::nil\"."
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-with-footnotes t
  "Non-nil means Org footnotes should be exported.
This option can also be set with the OPTIONS keyword,
e.g. \"f:nil\"."
  :group 'org-export-general
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-with-latex t
  "Non-nil means process LaTeX environments and fragments.

This option can also be set with the OPTIONS line,
e.g. \"tex:verbatim\".  Allowed values are:

nil         Ignore math snippets.
`verbatim'  Keep everything in verbatim.
t           Allow export of math snippets."
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Do not process math in any way" nil)
	  (const :tag "Interpret math snippets" t)
	  (const :tag "Leave math verbatim" verbatim))
  :safe (lambda (x) (memq x '(t nil verbatim))))

(defcustom org-export-headline-levels 3
  "The last level which is still exported as a headline.

Inferior levels will usually produce itemize or enumerate lists
when exported, but back-end behavior may differ.

This option can also be set with the OPTIONS keyword,
e.g. \"H:2\"."
  :group 'org-export-general
  :type 'integer
  :safe #'integerp)

(defcustom org-export-default-language "en"
  "The default language for export and clocktable translations, as a string.
This may have an association in
`org-clock-clocktable-language-setup',
`org-export-smart-quotes-alist' and `org-export-dictionary'.
This option can also be set with the LANGUAGE keyword."
  :group 'org-export-general
  :type '(string :tag "Language")
  :safe #'stringp)

(defcustom org-export-preserve-breaks nil
  "Non-nil means preserve all line breaks when exporting.
This option can also be set with the OPTIONS keyword,
e.g. \"\\n:t\"."
  :group 'org-export-general
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-with-entities t
  "Non-nil means interpret entities when exporting.

For example, HTML export converts \\alpha to &alpha; and \\AA to
&Aring;.

For a list of supported names, see the constant `org-entities'
and the user option `org-entities-user'.

This option can also be set with the OPTIONS keyword,
e.g. \"e:nil\"."
  :group 'org-export-general
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-with-inlinetasks t
  "Non-nil means inlinetasks should be exported.
This option can also be set with the OPTIONS keyword,
e.g. \"inline:nil\"."
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-with-planning nil
  "Non-nil means include planning info in export.

Planning info is the line containing either SCHEDULED:,
DEADLINE:, CLOSED: time-stamps, or a combination of them.

This option can also be set with the OPTIONS keyword,
e.g. \"p:t\"."
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-with-priority nil
  "Non-nil means include priority cookies in export.
This option can also be set with the OPTIONS keyword,
e.g. \"pri:t\"."
  :group 'org-export-general
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-with-properties nil
  "Non-nil means export contents of properties drawers.

When t, all properties are exported.  This may also be a list of
properties to export, as strings.

This option can also be set with the OPTIONS keyword,
e.g. \"prop:t\"."
  :group 'org-export-general
  :version "26.1"
  :package-version '(Org . "8.3")
  :type '(choice
	  (const :tag "All properties" t)
	  (const :tag "None" nil)
	  (repeat :tag "Selected properties"
		  (string :tag "Property name")))
  :safe (lambda (x) (or (booleanp x)
			(and (listp x) (cl-every #'stringp x)))))

(defcustom org-export-with-section-numbers t
  "Non-nil means add section numbers to headlines when exporting.

When set to an integer n, numbering will only happen for
headlines whose relative level is higher or equal to n.

This option can also be set with the OPTIONS keyword,
e.g. \"num:t\"."
  :group 'org-export-general
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-select-tags '("export")
  "Tags that select a tree for export.

If any such tag is found in a buffer, all trees that do not carry
one of these tags will be ignored during export.  Inside trees
that are selected like this, you can still deselect a subtree by
tagging it with one of the `org-export-exclude-tags'.

This option can also be set with the SELECT_TAGS keyword."
  :group 'org-export-general
  :type '(repeat (string :tag "Tag"))
  :safe (lambda (x) (and (listp x) (cl-every #'stringp x))))

(defcustom org-export-with-smart-quotes nil
  "Non-nil means activate smart quotes during export.
This option can also be set with the OPTIONS keyword,
e.g., \"':t\".

When setting this to non-nil, you need to take care of
using the correct Babel package when exporting to LaTeX.
E.g., you can load Babel for french like this:

#+LATEX_HEADER: \\usepackage[french]{babel}"
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-with-special-strings t
  "Non-nil means interpret \"\\-\", \"--\" and \"---\" for export.

When this option is turned on, these strings will be exported as:

   Org     HTML     LaTeX    UTF-8
  -----+----------+--------+-------
   \\-    &shy;      \\-
   --    &ndash;    --         –
   ---   &mdash;    ---        —
   ...   &hellip;   \\ldots     …

This option can also be set with the OPTIONS keyword,
e.g. \"-:nil\"."
  :group 'org-export-general
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-with-statistics-cookies t
  "Non-nil means include statistics cookies in export.
This option can also be set with the OPTIONS keyword,
e.g. \"stat:nil\""
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-with-sub-superscripts t
  "Non-nil means interpret \"_\" and \"^\" for export.

If you want to control how Org displays those characters, see
`org-use-sub-superscripts'.  `org-export-with-sub-superscripts'
used to be an alias for `org-use-sub-superscripts' in Org <8.0,
it is not anymore.

When this option is turned on, you can use TeX-like syntax for
sub- and superscripts and see them exported correctly.

You can also set the option with #+OPTIONS: ^:t

Several characters after \"_\" or \"^\" will be considered as a
single item - so grouping with {} is normally not needed.  For
example, the following things will be parsed as single sub- or
superscripts:

 10^24   or   10^tau     several digits will be considered 1 item.
 10^-12  or   10^-tau    a leading sign with digits or a word
 x^2-y^3                 will be read as x^2 - y^3, because items are
			 terminated by almost any nonword/nondigit char.
 x_{i^2} or   x^(2-i)    braces or parenthesis do grouping.

Still, ambiguity is possible.  So when in doubt, use {} to enclose
the sub/superscript.  If you set this variable to the symbol `{}',
the braces are *required* in order to trigger interpretations as
sub/superscript.  This can be helpful in documents that need \"_\"
frequently in plain text."
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Interpret them" t)
	  (const :tag "Curly brackets only" {})
	  (const :tag "Do not interpret them" nil))
  :safe (lambda (x) (memq x '(t nil {}))))

(defcustom org-export-with-toc t
  "Non-nil means create a table of contents in exported files.

The TOC contains headlines with levels up
to`org-export-headline-levels'.  When an integer, include levels
up to N in the toc, this may then be different from
`org-export-headline-levels', but it will not be allowed to be
larger than the number of headline levels.  When nil, no table of
contents is made.

This option can also be set with the OPTIONS keyword,
e.g. \"toc:nil\" or \"toc:3\"."
  :group 'org-export-general
  :type '(choice
	  (const :tag "No Table of Contents" nil)
	  (const :tag "Full Table of Contents" t)
	  (integer :tag "TOC to level"))
  :safe (lambda (x) (or (booleanp x)
			(integerp x))))

(defcustom org-export-with-tables t
  "Non-nil means export tables.
This option can also be set with the OPTIONS keyword,
e.g. \"|:nil\"."
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-with-tags t
  "If nil, do not export tags, just remove them from headlines.

If this is the symbol `not-in-toc', tags will be removed from
table of contents entries, but still be shown in the headlines of
the document.

This option can also be set with the OPTIONS keyword,
e.g. \"tags:nil\"."
  :group 'org-export-general
  :type '(choice
	  (const :tag "Off" nil)
	  (const :tag "Not in TOC" not-in-toc)
	  (const :tag "On" t))
  :safe (lambda (x) (memq x '(t nil not-in-toc))))

(defcustom org-export-with-tasks t
  "Non-nil means include TODO items for export.

This may have the following values:
t                    include tasks independent of state.
`todo'               include only tasks that are not yet done.
`done'               include only tasks that are already done.
nil                  ignore all tasks.
list of keywords     include tasks with these keywords.

This option can also be set with the OPTIONS keyword,
e.g. \"tasks:nil\"."
  :group 'org-export-general
  :type '(choice
	  (const :tag "All tasks" t)
	  (const :tag "No tasks" nil)
	  (const :tag "Not-done tasks" todo)
	  (const :tag "Only done tasks" done)
	  (repeat :tag "Specific TODO keywords"
		  (string :tag "Keyword")))
  :safe (lambda (x) (or (memq x '(nil t todo done))
			(and (listp x)
			     (cl-every #'stringp x)))))

(defcustom org-export-with-title t
  "Non-nil means print title into the exported file.
This option can also be set with the OPTIONS keyword,
e.g. \"title:nil\"."
  :group 'org-export-general
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-time-stamp-file t
  "Non-nil means insert a time stamp into the exported file.
The time stamp shows when the file was created.  This option can
also be set with the OPTIONS keyword, e.g. \"timestamp:nil\"."
  :group 'org-export-general
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-with-timestamps t
  "Non nil means allow timestamps in export.

It can be set to any of the following values:
  t          export all timestamps.
  `active'   export active timestamps only.
  `inactive' export inactive timestamps only.
  nil        do not export timestamps

This only applies to timestamps isolated in a paragraph
containing only timestamps.  Other timestamps are always
exported.

This option can also be set with the OPTIONS keyword, e.g.
\"<:nil\"."
  :group 'org-export-general
  :type '(choice
	  (const :tag "All timestamps" t)
	  (const :tag "Only active timestamps" active)
	  (const :tag "Only inactive timestamps" inactive)
	  (const :tag "No timestamp" nil))
  :safe (lambda (x) (memq x '(t nil active inactive))))

(defcustom org-export-with-todo-keywords t
  "Non-nil means include TODO keywords in export.
When nil, remove all these keywords from the export.  This option
can also be set with the OPTIONS keyword, e.g.  \"todo:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-allow-bind-keywords nil
  "Non-nil means BIND keywords can define local variable values.
This is a potential security risk, which is why the default value
is nil.  You can also allow them through local buffer variables."
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-export-with-broken-links nil
  "Non-nil means do not raise an error on broken links.

When this variable is non-nil, broken links are ignored, without
stopping the export process.  If it is set to `mark', broken
links are marked as such in the output, with a string like

  [BROKEN LINK: path]

where PATH is the un-resolvable reference.

This option can also be set with the OPTIONS keyword, e.g.,
\"broken-links:mark\"."
  :group 'org-export-general
  :version "26.1"
  :package-version '(Org . "9.0")
  :type '(choice
	  (const :tag "Ignore broken links" t)
	  (const :tag "Mark broken links in output" mark)
	  (const :tag "Raise an error" nil)))

(defcustom org-export-snippet-translation-alist nil
  "Alist between export snippets back-ends and exporter back-ends.

This variable allows providing shortcuts for export snippets.

For example, with a value of \\='((\"h\" . \"html\")), the
HTML back-end will recognize the contents of \"@@h:<b>@@\" as
HTML code while every other back-end will ignore it."
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(repeat
	  (cons (string :tag "Shortcut")
		(string :tag "Back-end")))
  :safe (lambda (x)
	  (and (listp x)
	       (cl-every #'consp x)
	       (cl-every #'stringp (mapcar #'car x))
	       (cl-every #'stringp (mapcar #'cdr x)))))

(defcustom org-export-global-macros nil
  "Alist between macro names and expansion templates.

This variable defines macro expansion templates available
globally.  Associations follow the pattern

  (NAME . TEMPLATE)

where NAME is a string beginning with a letter and consisting of
alphanumeric characters only.

TEMPLATE is the string to which the macro is going to be
expanded.  Inside, \"$1\", \"$2\"... are place-holders for
macro's arguments.  Moreover, if the template starts with
\"(eval\", it will be parsed as an Elisp expression and evaluated
accordingly."
  :group 'org-export-general
  :version "26.1"
  :package-version '(Org . "9.1")
  :type '(repeat
	  (cons (string :tag "Name")
		(string :tag "Template"))))

(defcustom org-export-coding-system nil
  "Coding system for the exported file."
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'coding-system)

(defcustom org-export-copy-to-kill-ring nil
  "Non-nil means pushing export output to the kill ring.
This variable is ignored during asynchronous export."
  :group 'org-export-general
  :version "26.1"
  :package-version '(Org . "8.3")
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "When export is done interactively" if-interactive)
	  (const :tag "Never" nil)))

(defcustom org-export-initial-scope 'buffer
  "The initial scope when exporting with `org-export-dispatch'.
This variable can be either set to `buffer' or `subtree'."
  :group 'org-export-general
  :type '(choice
	  (const :tag "Export current buffer" buffer)
	  (const :tag "Export current subtree" subtree)))

(defcustom org-export-show-temporary-export-buffer t
  "Non-nil means show buffer after exporting to temp buffer.
When Org exports to a file, the buffer visiting that file is never
shown, but remains buried.  However, when exporting to
a temporary buffer, that buffer is popped up in a second window.
When this variable is nil, the buffer remains buried also in
these cases."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-in-background nil
  "Non-nil means export and publishing commands will run in background.
Results from an asynchronous export are never displayed
automatically.  But you can retrieve them with `\\[org-export-stack]'."
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-export-async-init-file nil
  "File used to initialize external export process.

Value must be either nil or an absolute file name.  When nil, the
external process is launched like a regular Emacs session,
loading user's initialization file and any site specific
configuration.  If a file is provided, it, and only it, is loaded
at start-up.

Therefore, using a specific configuration makes the process to
load faster and the export more portable."
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Regular startup" nil)
	  (file :tag "Specific start-up file" :must-match t)))

(defcustom org-export-dispatch-use-expert-ui nil
  "Non-nil means using a non-intrusive `org-export-dispatch'.
In that case, no help buffer is displayed.  Though, an indicator
for current export scope is added to the prompt (\"b\" when
output is restricted to body only, \"s\" when it is restricted to
the current subtree, \"v\" when only visible elements are
considered for export, \"f\" when publishing functions should be
passed the FORCE argument and \"a\" when the export should be
asynchronous).  Also, [?] allows switching back to standard
mode."
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)



;;; Defining Back-ends
;;
;; An export back-end is a structure with `org-export-backend' type
;; and `name', `parent', `transcoders', `options', `filters', `blocks'
;; and `menu' slots.
;;
;; At the lowest level, a back-end is created with
;; `org-export-create-backend' function.
;;
;; A named back-end can be registered with
;; `org-export-register-backend' function.  A registered back-end can
;; later be referred to by its name, with `org-export-get-backend'
;; function.  Also, such a back-end can become the parent of a derived
;; back-end from which slot values will be inherited by default.
;; `org-export-derived-backend-p' can check if a given back-end is
;; derived from a list of back-end names.
;;
;; `org-export-get-all-transcoders', `org-export-get-all-options' and
;; `org-export-get-all-filters' return the full alist of transcoders,
;; options and filters, including those inherited from ancestors.
;;
;; At a higher level, `org-export-define-backend' is the standard way
;; to define an export back-end.  If the new back-end is similar to
;; a registered back-end, `org-export-define-derived-backend' may be
;; used instead.
;;
;; Eventually `org-export-barf-if-invalid-backend' returns an error
;; when a given back-end hasn't been registered yet.

(cl-defstruct (org-export-backend (:constructor org-export-create-backend)
				  (:copier nil))
  name parent transcoders options filters blocks menu)

;;;###autoload
(defun org-export-get-backend (name)
  "Return export back-end named after NAME.
NAME is a symbol.  Return nil if no such back-end is found."
  (cl-find-if (lambda (b) (and (eq name (org-export-backend-name b))))
	      org-export-registered-backends))

(defun org-export-register-backend (backend)
  "Register BACKEND as a known export back-end.
BACKEND is a structure with `org-export-backend' type."
  ;; Refuse to register an unnamed back-end.
  (unless (org-export-backend-name backend)
    (error "Cannot register a unnamed export back-end"))
  ;; Refuse to register a back-end with an unknown parent.
  (let ((parent (org-export-backend-parent backend)))
    (when (and parent (not (org-export-get-backend parent)))
      (error "Cannot use unknown \"%s\" back-end as a parent" parent)))
  ;; If a back-end with the same name as BACKEND is already
  ;; registered, replace it with BACKEND.  Otherwise, simply add
  ;; BACKEND to the list of registered back-ends.
  (let ((old (org-export-get-backend (org-export-backend-name backend))))
    (if old (setcar (memq old org-export-registered-backends) backend)
      (push backend org-export-registered-backends))))

(defun org-export-barf-if-invalid-backend (backend)
  "Signal an error if BACKEND isn't defined."
  (unless (org-export-backend-p backend)
    (error "Unknown \"%s\" back-end: Aborting export" backend)))

(defun org-export-derived-backend-p (backend &rest backends)
  "Non-nil if BACKEND is derived from one of BACKENDS.
BACKEND is an export back-end, as returned by, e.g.,
`org-export-create-backend', or a symbol referring to
a registered back-end.  BACKENDS is constituted of symbols."
  (when (symbolp backend) (setq backend (org-export-get-backend backend)))
  (when backend
    (catch 'exit
      (while (org-export-backend-parent backend)
	(when (memq (org-export-backend-name backend) backends)
	  (throw 'exit t))
	(setq backend
	      (org-export-get-backend (org-export-backend-parent backend))))
      (memq (org-export-backend-name backend) backends))))

(defun org-export-get-all-transcoders (backend)
  "Return full translation table for BACKEND.

BACKEND is an export back-end, as return by, e.g,,
`org-export-create-backend'.  Return value is an alist where
keys are element or object types, as symbols, and values are
transcoders.

Unlike to `org-export-backend-transcoders', this function
also returns transcoders inherited from parent back-ends,
if any."
  (when (symbolp backend) (setq backend (org-export-get-backend backend)))
  (when backend
    (let ((transcoders (org-export-backend-transcoders backend))
	  parent)
      (while (setq parent (org-export-backend-parent backend))
	(setq backend (org-export-get-backend parent))
	(setq transcoders
	      (append transcoders (org-export-backend-transcoders backend))))
      transcoders)))

(defun org-export-get-all-options (backend)
  "Return export options for BACKEND.

BACKEND is an export back-end, as return by, e.g,,
`org-export-create-backend'.  See `org-export-options-alist'
for the shape of the return value.

Unlike to `org-export-backend-options', this function also
returns options inherited from parent back-ends, if any.

Return nil if BACKEND is unknown."
  (when (symbolp backend) (setq backend (org-export-get-backend backend)))
  (when backend
    (let ((options (org-export-backend-options backend))
	  parent)
      (while (setq parent (org-export-backend-parent backend))
	(setq backend (org-export-get-backend parent))
	(setq options (append options (org-export-backend-options backend))))
      options)))

(defun org-export-get-all-filters (backend)
  "Return complete list of filters for BACKEND.

BACKEND is an export back-end, as return by, e.g,,
`org-export-create-backend'.  Return value is an alist where
keys are symbols and values lists of functions.

Unlike to `org-export-backend-filters', this function also
returns filters inherited from parent back-ends, if any."
  (when (symbolp backend) (setq backend (org-export-get-backend backend)))
  (when backend
    (let ((filters (org-export-backend-filters backend))
	  parent)
      (while (setq parent (org-export-backend-parent backend))
	(setq backend (org-export-get-backend parent))
	(setq filters (append filters (org-export-backend-filters backend))))
      filters)))

(defun org-export-define-backend (backend transcoders &rest body)
  "Define a new back-end BACKEND.

TRANSCODERS is an alist between object or element types and
functions handling them.

These functions should return a string without any trailing
space, or nil.  They must accept three arguments: the object or
element itself, its contents or nil when it isn't recursive and
the property list used as a communication channel.

Contents, when not nil, are stripped from any global indentation
\(although the relative one is preserved).  They also always end
with a single newline character.

If, for a given type, no function is found, that element or
object type will simply be ignored, along with any blank line or
white space at its end.  The same will happen if the function
returns the nil value.  If that function returns the empty
string, the type will be ignored, but the blank lines or white
spaces will be kept.

In addition to element and object types, one function can be
associated to the `template' (or `inner-template') symbol and
another one to the `plain-text' symbol.

The former returns the final transcoded string, and can be used
to add a preamble and a postamble to document's body.  It must
accept two arguments: the transcoded string and the property list
containing export options.  A function associated to `template'
will not be applied if export has option \"body-only\".
A function associated to `inner-template' is always applied.

The latter, when defined, is to be called on every text not
recognized as an element or an object.  It must accept two
arguments: the text string and the information channel.  It is an
appropriate place to protect special chars relative to the
back-end.

BODY can start with pre-defined keyword arguments.  The following
keywords are understood:

  :filters-alist

    Alist between filters and function, or list of functions,
    specific to the back-end.  See `org-export-filters-alist' for
    a list of all allowed filters.  Filters defined here
    shouldn't make a back-end test, as it may prevent back-ends
    derived from this one to behave properly.

  :menu-entry

    Menu entry for the export dispatcher.  It should be a list
    like:

      \\='(KEY DESCRIPTION-OR-ORDINAL ACTION-OR-MENU)

    where :

      KEY is a free character selecting the back-end.

      DESCRIPTION-OR-ORDINAL is either a string or a number.

      If it is a string, is will be used to name the back-end in
      its menu entry.  If it is a number, the following menu will
      be displayed as a sub-menu of the back-end with the same
      KEY.  Also, the number will be used to determine in which
      order such sub-menus will appear (lowest first).

      ACTION-OR-MENU is either a function or an alist.

      If it is an action, it will be called with four
      arguments (booleans): ASYNC, SUBTREEP, VISIBLE-ONLY and
      BODY-ONLY.  See `org-export-as' for further explanations on
      some of them.

      If it is an alist, associations should follow the
      pattern:

        \\='(KEY DESCRIPTION ACTION)

      where KEY, DESCRIPTION and ACTION are described above.

    Valid values include:

      \\='(?m \"My Special Back-end\" my-special-export-function)

      or

      \\='(?l \"Export to LaTeX\"
           (?p \"As PDF file\" org-latex-export-to-pdf)
           (?o \"As PDF file and open\"
               (lambda (a s v b)
                 (if a (org-latex-export-to-pdf t s v b)
                   (org-open-file
                    (org-latex-export-to-pdf nil s v b)))))))

      or the following, which will be added to the previous
      sub-menu,

      \\='(?l 1
          ((?B \"As TEX buffer (Beamer)\" org-beamer-export-as-latex)
           (?P \"As PDF file (Beamer)\" org-beamer-export-to-pdf)))

  :options-alist

    Alist between back-end specific properties introduced in
    communication channel and how their value are acquired.  See
    `org-export-options-alist' for more information about
    structure of the values."
  (declare (indent 1))
  (let (filters menu-entry options)
    (while (keywordp (car body))
      (let ((keyword (pop body)))
	(pcase keyword
	  (:filters-alist (setq filters (pop body)))
	  (:menu-entry (setq menu-entry (pop body)))
	  (:options-alist (setq options (pop body)))
	  (_ (error "Unknown keyword: %s" keyword)))))
    (org-export-register-backend
     (org-export-create-backend :name backend
				:transcoders transcoders
				:options options
				:filters filters
				:menu menu-entry))))

(defun org-export-define-derived-backend (child parent &rest body)
  "Create a new back-end as a variant of an existing one.

CHILD is the name of the derived back-end.  PARENT is the name of
the parent back-end.

BODY can start with pre-defined keyword arguments.  The following
keywords are understood:

  :filters-alist

    Alist of filters that will overwrite or complete filters
    defined in PARENT back-end.  See `org-export-filters-alist'
    for a list of allowed filters.

  :menu-entry

    Menu entry for the export dispatcher.  See
    `org-export-define-backend' for more information about the
    expected value.

  :options-alist

    Alist of back-end specific properties that will overwrite or
    complete those defined in PARENT back-end.  Refer to
    `org-export-options-alist' for more information about
    structure of the values.

  :translate-alist

    Alist of element and object types and transcoders that will
    overwrite or complete transcode table from PARENT back-end.
    Refer to `org-export-define-backend' for detailed information
    about transcoders.

As an example, here is how one could define \"my-latex\" back-end
as a variant of `latex' back-end with a custom template function:

  (org-export-define-derived-backend \\='my-latex \\='latex
     :translate-alist \\='((template . my-latex-template-fun)))

The back-end could then be called with, for example:

  (org-export-to-buffer \\='my-latex \"*Test my-latex*\")"
  (declare (indent 2))
  (let (filters menu-entry options transcoders)
    (while (keywordp (car body))
      (let ((keyword (pop body)))
	(pcase keyword
	  (:filters-alist (setq filters (pop body)))
	  (:menu-entry (setq menu-entry (pop body)))
	  (:options-alist (setq options (pop body)))
	  (:translate-alist (setq transcoders (pop body)))
	  (_ (error "Unknown keyword: %s" keyword)))))
    (org-export-register-backend
     (org-export-create-backend :name child
				:parent parent
				:transcoders transcoders
				:options options
				:filters filters
				:menu menu-entry))))



;;; The Communication Channel
;;
;; During export process, every function has access to a number of
;; properties.  They are of two types:
;;
;; 1. Environment options are collected once at the very beginning of
;;    the process, out of the original buffer and configuration.
;;    Collecting them is handled by `org-export-get-environment'
;;    function.
;;
;;    Most environment options are defined through the
;;    `org-export-options-alist' variable.
;;
;; 2. Tree properties are extracted directly from the parsed tree,
;;    just before export, by `org-export--collect-tree-properties'.

;;;; Environment Options
;;
;; Environment options encompass all parameters defined outside the
;; scope of the parsed data.  They come from five sources, in
;; increasing precedence order:
;;
;; - Global variables,
;; - Buffer's attributes,
;; - Options keyword symbols,
;; - Buffer keywords,
;; - Subtree properties.
;;
;; The central internal function with regards to environment options
;; is `org-export-get-environment'.  It updates global variables with
;; "#+BIND:" keywords, then retrieve and prioritize properties from
;; the different sources.
;;
;;  The internal functions doing the retrieval are:
;;  `org-export--get-global-options',
;;  `org-export--get-buffer-attributes',
;;  `org-export--parse-option-keyword',
;;  `org-export--get-subtree-options' and
;;  `org-export--get-inbuffer-options'
;;
;; Also, `org-export--list-bound-variables' collects bound variables
;; along with their value in order to set them as buffer local
;; variables later in the process.

;;;###autoload
(defun org-export-get-environment (&optional backend subtreep ext-plist)
  "Collect export options from the current buffer.

Optional argument BACKEND is an export back-end, as returned by
`org-export-create-backend'.

When optional argument SUBTREEP is non-nil, assume the export is
done against the current sub-tree.

Third optional argument EXT-PLIST is a property list with
external parameters overriding Org default settings, but still
inferior to file-local settings."
  ;; First install #+BIND variables since these must be set before
  ;; global options are read.
  (dolist (pair (org-export--list-bound-variables))
    (set (make-local-variable (car pair)) (nth 1 pair)))
  ;; Get and prioritize export options...
  (org-combine-plists
   ;; ... from global variables...
   (org-export--get-global-options backend)
   ;; ... from an external property list...
   ext-plist
   ;; ... from in-buffer settings...
   (org-export--get-inbuffer-options backend)
   ;; ... and from subtree, when appropriate.
   (and subtreep (org-export--get-subtree-options backend))))

(defun org-export--parse-option-keyword (options &optional backend)
  "Parse an OPTIONS line and return values as a plist.
Optional argument BACKEND is an export back-end, as returned by,
e.g., `org-export-create-backend'.  It specifies which back-end
specific items to read, if any."
  (let ((line
	 (let ((s 0) alist)
	   (while (string-match "\\(.+?\\):\\((.*?)\\|\\S-*\\)[ \t]*" options s)
	     (setq s (match-end 0))
	     (push (cons (match-string 1 options)
			 (read (match-string 2 options)))
		   alist))
	   alist))
	;; Priority is given to back-end specific options.
	(all (append (org-export-get-all-options backend)
		     org-export-options-alist))
	(plist))
    (when line
      (dolist (entry all plist)
	(let ((item (nth 2 entry)))
	  (when item
	    (let ((v (assoc-string item line t)))
	      (when v (setq plist (plist-put plist (car entry) (cdr v)))))))))))

(defun org-export--get-subtree-options (&optional backend)
  "Get export options in subtree at point.
Optional argument BACKEND is an export back-end, as returned by,
e.g., `org-export-create-backend'.  It specifies back-end used
for export.  Return options as a plist."
  ;; For each buffer keyword, create a headline property setting the
  ;; same property in communication channel.  The name for the
  ;; property is the keyword with "EXPORT_" appended to it.
  (org-with-wide-buffer
   ;; Make sure point is at a heading.
   (if (org-at-heading-p) (org-up-heading-safe) (org-back-to-heading t))
   (let ((plist
	  ;; EXPORT_OPTIONS are parsed in a non-standard way.  Take
	  ;; care of them right from the start.
	  (let ((o (org-entry-get (point) "EXPORT_OPTIONS" 'selective)))
	    (and o (org-export--parse-option-keyword o backend))))
	 ;; Take care of EXPORT_TITLE.  If it isn't defined, use
	 ;; headline's title (with no todo keyword, priority cookie or
	 ;; tag) as its fallback value.
	 (cache (list
		 (cons "TITLE"
		       (or (org-entry-get (point) "EXPORT_TITLE" 'selective)
			   (let ((case-fold-search nil))
			     (looking-at org-complex-heading-regexp)
			     (match-string-no-properties 4))))))
	 ;; Look for both general keywords and back-end specific
	 ;; options, with priority given to the latter.
	 (options (append (org-export-get-all-options backend)
			  org-export-options-alist)))
     ;; Handle other keywords.  Then return PLIST.
     (dolist (option options plist)
       (let ((property (car option))
	     (keyword (nth 1 option)))
	 (when keyword
	   (let ((value
		  (or (cdr (assoc keyword cache))
		      (let ((v (org-entry-get (point)
					      (concat "EXPORT_" keyword)
					      'selective)))
			(push (cons keyword v) cache) v))))
	     (when value
	       (setq plist
		     (plist-put plist
				property
				(cl-case (nth 4 option)
				  (parse
				   (org-element-parse-secondary-string
				    value (org-element-restriction 'keyword)))
				  (split (split-string value))
				  (t value))))))))))))

(defun org-export--get-inbuffer-options (&optional backend)
  "Return current buffer export options, as a plist.

Optional argument BACKEND, when non-nil, is an export back-end,
as returned by, e.g., `org-export-create-backend'.  It specifies
which back-end specific options should also be read in the
process.

Assume buffer is in Org mode.  Narrowing, if any, is ignored."
  (let* ((case-fold-search t)
	 (options (append
		   ;; Priority is given to back-end specific options.
		   (org-export-get-all-options backend)
		   org-export-options-alist))
	 (regexp (format "^[ \t]*#\\+%s:"
			 (regexp-opt (nconc (delq nil (mapcar #'cadr options))
					    org-export-special-keywords))))
	 plist to-parse)
    (letrec ((find-properties
	      (lambda (keyword)
		;; Return all properties associated to KEYWORD.
		(let (properties)
		  (dolist (option options properties)
		    (when (equal (nth 1 option) keyword)
		      (cl-pushnew (car option) properties))))))
	     (get-options
	      (lambda (&optional files)
		;; Recursively read keywords in buffer.  FILES is
		;; a list of files read so far.  PLIST is the current
		;; property list obtained.
		(org-with-wide-buffer
		 (goto-char (point-min))
		 (while (re-search-forward regexp nil t)
		   (let ((element (org-element-at-point)))
		     (when (eq (org-element-type element) 'keyword)
		       (let ((key (org-element-property :key element))
			     (val (org-element-property :value element)))
			 (cond
			  ;; Options in `org-export-special-keywords'.
			  ((equal key "SETUPFILE")
			   (let* ((uri (org-unbracket-string "\"" "\"" (org-trim val)))
				  (uri-is-url (org-file-url-p uri))
				  (uri (if uri-is-url
					   uri
					 (expand-file-name uri))))
			     ;; Avoid circular dependencies.
			     (unless (member uri files)
			       (with-temp-buffer
				 (unless uri-is-url
				   (setq default-directory
					 (file-name-directory uri)))
				 (insert (org-file-contents uri 'noerror))
				 (let ((org-inhibit-startup t)) (org-mode))
				 (funcall get-options (cons uri files))))))
			  ((equal key "OPTIONS")
			   (setq plist
				 (org-combine-plists
				  plist
				  (org-export--parse-option-keyword
				   val backend))))
			  ((equal key "FILETAGS")
			   (setq plist
				 (org-combine-plists
				  plist
				  (list :filetags
					(org-uniquify
					 (append
					  (org-split-string val ":")
					  (plist-get plist :filetags)))))))
			  (t
			   ;; Options in `org-export-options-alist'.
			   (dolist (property (funcall find-properties key))
			     (setq
			      plist
			      (plist-put
			       plist property
			       ;; Handle value depending on specified
			       ;; BEHAVIOR.
			       (cl-case (nth 4 (assq property options))
				 (parse
				  (unless (memq property to-parse)
				    (push property to-parse))
				  ;; Even if `parse' implies `space'
				  ;; behavior, we separate line with
				  ;; "\n" so as to preserve
				  ;; line-breaks.  However, empty
				  ;; lines are forbidden since `parse'
				  ;; doesn't allow more than one
				  ;; paragraph.
				  (let ((old (plist-get plist property)))
				    (cond ((not (org-string-nw-p val)) old)
					  (old (concat old "\n" val))
					  (t val))))
				 (space
				  (if (not (plist-get plist property))
				      (org-trim val)
				    (concat (plist-get plist property)
					    " "
					    (org-trim val))))
				 (newline
				  (org-trim
				   (concat (plist-get plist property)
					   "\n"
					   (org-trim val))))
				 (split `(,@(plist-get plist property)
					  ,@(split-string val)))
				 ((t) val)
				 (otherwise
				  (if (not (plist-member plist property)) val
				    (plist-get plist property)))))))))))))))))
      ;; Read options in the current buffer and return value.
      (funcall get-options (and buffer-file-name (list buffer-file-name)))
      ;; Parse properties in TO-PARSE.  Remove newline characters not
      ;; involved in line breaks to simulate `space' behavior.
      ;; Finally return options.
      (dolist (p to-parse plist)
	(let ((value (org-element-parse-secondary-string
		      (plist-get plist p)
		      (org-element-restriction 'keyword))))
	  (org-element-map value 'plain-text
	    (lambda (s)
	      (org-element-set-element
	       s (replace-regexp-in-string "\n" " " s))))
	  (setq plist (plist-put plist p value)))))))

(defun org-export--get-export-attributes
    (&optional backend subtreep visible-only body-only)
  "Return properties related to export process, as a plist.
Optional arguments BACKEND, SUBTREEP, VISIBLE-ONLY and BODY-ONLY
are like the arguments with the same names of function
`org-export-as'."
  (list :export-options (delq nil
			      (list (and subtreep 'subtree)
				    (and visible-only 'visible-only)
				    (and body-only 'body-only)))
	:back-end backend
	:translate-alist (org-export-get-all-transcoders backend)
	:exported-data (make-hash-table :test #'eq :size 4001)))

(defun org-export--get-buffer-attributes ()
  "Return properties related to buffer attributes, as a plist."
  (list :input-buffer (buffer-name (buffer-base-buffer))
	:input-file (buffer-file-name (buffer-base-buffer))))

(defun org-export--get-global-options (&optional backend)
  "Return global export options as a plist.
Optional argument BACKEND, if non-nil, is an export back-end, as
returned by, e.g., `org-export-create-backend'.  It specifies
which back-end specific export options should also be read in the
process."
  (let (plist
	;; Priority is given to back-end specific options.
	(all (append (org-export-get-all-options backend)
		     org-export-options-alist)))
    (dolist (cell all plist)
      (let ((prop (car cell)))
	(unless (plist-member plist prop)
	  (setq plist
		(plist-put
		 plist
		 prop
		 ;; Evaluate default value provided.
		 (let ((value (eval (nth 3 cell))))
		   (if (eq (nth 4 cell) 'parse)
		       (org-element-parse-secondary-string
			value (org-element-restriction 'keyword))
		     value)))))))))

(defun org-export--list-bound-variables ()
  "Return variables bound from BIND keywords in current buffer.
Also look for BIND keywords in setup files.  The return value is
an alist where associations are (VARIABLE-NAME VALUE)."
  (when org-export-allow-bind-keywords
    (letrec ((collect-bind
	      (lambda (files alist)
		;; Return an alist between variable names and their
		;; value.  FILES is a list of setup files names read
		;; so far, used to avoid circular dependencies.  ALIST
		;; is the alist collected so far.
		(let ((case-fold-search t))
		  (org-with-wide-buffer
		   (goto-char (point-min))
		   (while (re-search-forward
			   "^[ \t]*#\\+\\(BIND\\|SETUPFILE\\):" nil t)
		     (let ((element (org-element-at-point)))
		       (when (eq (org-element-type element) 'keyword)
			 (let ((val (org-element-property :value element)))
			   (if (equal (org-element-property :key element)
				      "BIND")
			       (push (read (format "(%s)" val)) alist)
			     ;; Enter setup file.
			     (let* ((uri (org-unbracket-string "\"" "\"" val))
				    (uri-is-url (org-file-url-p uri))
				    (uri (if uri-is-url
					     uri
					   (expand-file-name uri))))
			       ;; Avoid circular dependencies.
			       (unless (member uri files)
				 (with-temp-buffer
				   (unless uri-is-url
				     (setq default-directory
					   (file-name-directory uri)))
				   (let ((org-inhibit-startup t)) (org-mode))
				   (insert (org-file-contents uri 'noerror))
				   (setq alist
					 (funcall collect-bind
						  (cons uri files)
						  alist))))))))))
		   alist)))))
      ;; Return value in appropriate order of appearance.
      (nreverse (funcall collect-bind nil nil)))))

;; defsubst org-export-get-parent must be defined before first use,
;; was originally defined in the topology section

(defsubst org-export-get-parent (blob)
  "Return BLOB parent or nil.
BLOB is the element or object considered."
  (org-element-property :parent blob))

;;;; Tree Properties
;;
;; Tree properties are information extracted from parse tree.  They
;; are initialized at the beginning of the transcoding process by
;; `org-export--collect-tree-properties'.
;;
;; Dedicated functions focus on computing the value of specific tree
;; properties during initialization.  Thus,
;; `org-export--populate-ignore-list' lists elements and objects that
;; should be skipped during export, `org-export--get-min-level' gets
;; the minimal exportable level, used as a basis to compute relative
;; level for headlines.  Eventually
;; `org-export--collect-headline-numbering' builds an alist between
;; headlines and their numbering.

(defun org-export--collect-tree-properties (data info)
  "Extract tree properties from parse tree.

DATA is the parse tree from which information is retrieved.  INFO
is a list holding export options.

Following tree properties are set or updated:

`:headline-offset' Offset between true level of headlines and
		   local level.  An offset of -1 means a headline
		   of level 2 should be considered as a level
		   1 headline in the context.

`:headline-numbering' Alist of all headlines as key and the
		      associated numbering as value.

`:id-alist' Alist of all ID references as key and associated file
            as value.

Return updated plist."
  ;; Install the parse tree in the communication channel.
  (setq info (plist-put info :parse-tree data))
  ;; Compute `:headline-offset' in order to be able to use
  ;; `org-export-get-relative-level'.
  (setq info
	(plist-put info
		   :headline-offset
		   (- 1 (org-export--get-min-level data info))))
  ;; From now on, properties order doesn't matter: get the rest of the
  ;; tree properties.
  (org-combine-plists
   info
   (list :headline-numbering (org-export--collect-headline-numbering data info)
	 :id-alist
	 (org-element-map data 'link
	   (lambda (l)
	     (and (string= (org-element-property :type l) "id")
		  (let* ((id (org-element-property :path l))
			 (file (car (org-id-find id))))
		    (and file (cons id (file-relative-name file))))))))))

(defun org-export--get-min-level (data options)
  "Return minimum exportable headline's level in DATA.
DATA is parsed tree as returned by `org-element-parse-buffer'.
OPTIONS is a plist holding export options."
  (catch 'exit
    (let ((min-level 10000))
      (dolist (datum (org-element-contents data))
	(when (and (eq (org-element-type datum) 'headline)
		   (not (org-element-property :footnote-section-p datum))
		   (not (memq datum (plist-get options :ignore-list))))
	  (setq min-level (min (org-element-property :level datum) min-level))
	  (when (= min-level 1) (throw 'exit 1))))
      ;; If no headline was found, for the sake of consistency, set
      ;; minimum level to 1 nonetheless.
      (if (= min-level 10000) 1 min-level))))

(defun org-export--collect-headline-numbering (data options)
  "Return numbering of all exportable, numbered headlines in a parse tree.

DATA is the parse tree.  OPTIONS is the plist holding export
options.

Return an alist whose key is a headline and value is its
associated numbering \(in the shape of a list of numbers) or nil
for a footnotes section."
  (let ((numbering (make-vector org-export-max-depth 0)))
    (org-element-map data 'headline
      (lambda (headline)
	(when (and (org-export-numbered-headline-p headline options)
		   (not (org-element-property :footnote-section-p headline)))
	  (let ((relative-level
		 (1- (org-export-get-relative-level headline options))))
	    (cons
	     headline
	     (cl-loop
	      for n across numbering
	      for idx from 0 to org-export-max-depth
	      when (< idx relative-level) collect n
	      when (= idx relative-level) collect (aset numbering idx (1+ n))
	      when (> idx relative-level) do (aset numbering idx 0))))))
      options)))

(defun org-export--selected-trees (data info)
  "List headlines and inlinetasks with a select tag in their tree.
DATA is parsed data as returned by `org-element-parse-buffer'.
INFO is a plist holding export options."
  (let ((select (plist-get info :select-tags)))
    (if (cl-some (lambda (tag) (member tag select)) (plist-get info :filetags))
	;; If FILETAGS contains a select tag, every headline or
	;; inlinetask is returned.
	(org-element-map data '(headline inlinetask) #'identity)
      (letrec ((selected-trees nil)
	       (walk-data
		(lambda (data genealogy)
		  (let ((type (org-element-type data)))
		    (cond
		     ((memq type '(headline inlinetask))
		      (let ((tags (org-element-property :tags data)))
			(if (cl-some (lambda (tag) (member tag select)) tags)
			    ;; When a select tag is found, mark full
			    ;; genealogy and every headline within the
			    ;; tree as acceptable.
			    (setq selected-trees
				  (append
				   genealogy
				   (org-element-map data '(headline inlinetask)
				     #'identity)
				   selected-trees))
			  ;; If at a headline, continue searching in
			  ;; tree, recursively.
			  (when (eq type 'headline)
			    (dolist (el (org-element-contents data))
			      (funcall walk-data el (cons data genealogy)))))))
		     ((or (eq type 'org-data)
			  (memq type org-element-greater-elements))
		      (dolist (el (org-element-contents data))
			(funcall walk-data el genealogy))))))))
	(funcall walk-data data nil)
	selected-trees))))

(defun org-export--skip-p (datum options selected)
  "Non-nil when element or object DATUM should be skipped during export.
OPTIONS is the plist holding export options.  SELECTED, when
non-nil, is a list of headlines or inlinetasks belonging to
a tree with a select tag."
  (cl-case (org-element-type datum)
    ((comment comment-block)
     ;; Skip all comments and comment blocks.  Make to keep maximum
     ;; number of blank lines around the comment so as to preserve
     ;; local structure of the document upon interpreting it back into
     ;; Org syntax.
     (let* ((previous (org-export-get-previous-element datum options))
	    (before (or (org-element-property :post-blank previous) 0))
	    (after (or (org-element-property :post-blank datum) 0)))
       (when previous
	 (org-element-put-property previous :post-blank (max before after 1))))
     t)
    (clock (not (plist-get options :with-clocks)))
    (drawer
     (let ((with-drawers-p (plist-get options :with-drawers)))
       (or (not with-drawers-p)
	   (and (consp with-drawers-p)
		;; If `:with-drawers' value starts with `not', ignore
		;; every drawer whose name belong to that list.
		;; Otherwise, ignore drawers whose name isn't in that
		;; list.
		(let ((name (org-element-property :drawer-name datum)))
		  (if (eq (car with-drawers-p) 'not)
		      (member-ignore-case name (cdr with-drawers-p))
		    (not (member-ignore-case name with-drawers-p))))))))
    (fixed-width (not (plist-get options :with-fixed-width)))
    ((footnote-definition footnote-reference)
     (not (plist-get options :with-footnotes)))
    ((headline inlinetask)
     (let ((with-tasks (plist-get options :with-tasks))
	   (todo (org-element-property :todo-keyword datum))
	   (todo-type (org-element-property :todo-type datum))
	   (archived (plist-get options :with-archived-trees))
	   (tags (org-export-get-tags datum options nil t)))
       (or
	(and (eq (org-element-type datum) 'inlinetask)
	     (not (plist-get options :with-inlinetasks)))
	;; Ignore subtrees with an exclude tag.
	(cl-loop for k in (plist-get options :exclude-tags)
		 thereis (member k tags))
	;; When a select tag is present in the buffer, ignore any tree
	;; without it.
	(and selected (not (memq datum selected)))
	;; Ignore commented sub-trees.
	(org-element-property :commentedp datum)
	;; Ignore archived subtrees if `:with-archived-trees' is nil.
	(and (not archived) (org-element-property :archivedp datum))
	;; Ignore tasks, if specified by `:with-tasks' property.
	(and todo
	     (or (not with-tasks)
		 (and (memq with-tasks '(todo done))
		      (not (eq todo-type with-tasks)))
		 (and (consp with-tasks) (not (member todo with-tasks))))))))
    ((latex-environment latex-fragment) (not (plist-get options :with-latex)))
    (node-property
     (let ((properties-set (plist-get options :with-properties)))
       (cond ((null properties-set) t)
	     ((consp properties-set)
	      (not (member-ignore-case (org-element-property :key datum)
				       properties-set))))))
    (planning (not (plist-get options :with-planning)))
    (property-drawer (not (plist-get options :with-properties)))
    (statistics-cookie (not (plist-get options :with-statistics-cookies)))
    (table (not (plist-get options :with-tables)))
    (table-cell
     (and (org-export-table-has-special-column-p
	   (org-export-get-parent-table datum))
	  (org-export-first-sibling-p datum options)))
    (table-row (org-export-table-row-is-special-p datum options))
    (timestamp
     ;; `:with-timestamps' only applies to isolated timestamps
     ;; objects, i.e. timestamp objects in a paragraph containing only
     ;; timestamps and whitespaces.
     (when (let ((parent (org-export-get-parent-element datum)))
	     (and (memq (org-element-type parent) '(paragraph verse-block))
		  (not (org-element-map parent
			   (cons 'plain-text
				 (remq 'timestamp org-element-all-objects))
			 (lambda (obj)
			   (or (not (stringp obj)) (org-string-nw-p obj)))
			 options t))))
       (cl-case (plist-get options :with-timestamps)
	 ((nil) t)
	 (active
	  (not (memq (org-element-property :type datum) '(active active-range))))
	 (inactive
	  (not (memq (org-element-property :type datum)
		     '(inactive inactive-range)))))))))


;;; The Transcoder
;;
;; `org-export-data' reads a parse tree (obtained with, i.e.
;; `org-element-parse-buffer') and transcodes it into a specified
;; back-end output.  It takes care of filtering out elements or
;; objects according to export options and organizing the output blank
;; lines and white space are preserved.  The function memoizes its
;; results, so it is cheap to call it within transcoders.
;;
;; It is possible to modify locally the back-end used by
;; `org-export-data' or even use a temporary back-end by using
;; `org-export-data-with-backend'.
;;
;; `org-export-transcoder' is an accessor returning appropriate
;; translator function for a given element or object.

(defun org-export-transcoder (blob info)
  "Return appropriate transcoder for BLOB.
INFO is a plist containing export directives."
  (let ((type (org-element-type blob)))
    ;; Return contents only for complete parse trees.
    (if (eq type 'org-data) (lambda (_datum contents _info) contents)
      (let ((transcoder (cdr (assq type (plist-get info :translate-alist)))))
	(and (functionp transcoder) transcoder)))))

(defun org-export-data (data info)
  "Convert DATA into current back-end format.

DATA is a parse tree, an element or an object or a secondary
string.  INFO is a plist holding export options.

Return a string."
  (or (gethash data (plist-get info :exported-data))
      ;; Handle broken links according to
      ;; `org-export-with-broken-links'.
      (cl-macrolet
	  ((broken-link-handler
	    (&rest body)
	    `(condition-case err
		 (progn ,@body)
	       (org-link-broken
		(pcase (plist-get info :with-broken-links)
		  (`nil (user-error "Unable to resolve link: %S" (nth 1 err)))
		  (`mark (org-export-data
			  (format "[BROKEN LINK: %s]" (nth 1 err)) info))
		  (_ nil))))))
	(let* ((type (org-element-type data))
	       (parent (org-export-get-parent data))
	       (results
		(cond
		 ;; Ignored element/object.
		 ((memq data (plist-get info :ignore-list)) nil)
		 ;; Plain text.
		 ((eq type 'plain-text)
		  (org-export-filter-apply-functions
		   (plist-get info :filter-plain-text)
		   (let ((transcoder (org-export-transcoder data info)))
		     (if transcoder (funcall transcoder data info) data))
		   info))
		 ;; Secondary string.
		 ((not type)
		  (mapconcat (lambda (obj) (org-export-data obj info)) data ""))
		 ;; Element/Object without contents or, as a special
		 ;; case, headline with archive tag and archived trees
		 ;; restricted to title only.
		 ((or (not (org-element-contents data))
		      (and (eq type 'headline)
			   (eq (plist-get info :with-archived-trees) 'headline)
			   (org-element-property :archivedp data)))
		  (let ((transcoder (org-export-transcoder data info)))
		    (or (and (functionp transcoder)
			     (broken-link-handler
			      (funcall transcoder data nil info)))
			;; Export snippets never return a nil value so
			;; that white spaces following them are never
			;; ignored.
			(and (eq type 'export-snippet) ""))))
		 ;; Element/Object with contents.
		 (t
		  (let ((transcoder (org-export-transcoder data info)))
		    (when transcoder
		      (let* ((greaterp (memq type org-element-greater-elements))
			     (objectp
			      (and (not greaterp)
				   (memq type org-element-recursive-objects)))
			     (contents
			      (mapconcat
			       (lambda (element) (org-export-data element info))
			       (org-element-contents
				(if (or greaterp objectp) data
				  ;; Elements directly containing
				  ;; objects must have their indentation
				  ;; normalized first.
				  (org-element-normalize-contents
				   data
				   ;; When normalizing contents of the
				   ;; first paragraph in an item or
				   ;; a footnote definition, ignore
				   ;; first line's indentation: there is
				   ;; none and it might be misleading.
				   (when (eq type 'paragraph)
				     (and
				      (eq (car (org-element-contents parent))
					  data)
				      (memq (org-element-type parent)
					    '(footnote-definition item)))))))
			       "")))
			(broken-link-handler
			 (funcall transcoder data
				  (if (not greaterp) contents
				    (org-element-normalize-string contents))
				  info)))))))))
	  ;; Final result will be memoized before being returned.
	  (puthash
	   data
	   (cond
	    ((not results) "")
	    ((memq type '(org-data plain-text nil)) results)
	    ;; Append the same white space between elements or objects
	    ;; as in the original buffer, and call appropriate filters.
	    (t
	     (org-export-filter-apply-functions
	      (plist-get info (intern (format ":filter-%s" type)))
	      (let ((blank (or (org-element-property :post-blank data) 0)))
		(if (eq (org-element-class data parent) 'object)
		    (concat results (make-string blank ?\s))
		  (concat (org-element-normalize-string results)
			  (make-string blank ?\n))))
	      info)))
	   (plist-get info :exported-data))))))

(defun org-export-data-with-backend (data backend info)
  "Convert DATA into BACKEND format.

DATA is an element, an object, a secondary string or a string.
BACKEND is a symbol.  INFO is a plist used as a communication
channel.

Unlike to `org-export-with-backend', this function will
recursively convert DATA using BACKEND translation table."
  (when (symbolp backend) (setq backend (org-export-get-backend backend)))
  ;; Set-up a new communication channel with translations defined in
  ;; BACKEND as the translate table and a new hash table for
  ;; memoization.
  (let ((new-info
	 (org-combine-plists
	  info
	  (list :back-end backend
		:translate-alist (org-export-get-all-transcoders backend)
		;; Size of the hash table is reduced since this
		;; function will probably be used on small trees.
		:exported-data (make-hash-table :test 'eq :size 401)))))
    (prog1 (org-export-data data new-info)
      ;; Preserve `:internal-references', as those do not depend on
      ;; the back-end used; we need to make sure that any new
      ;; reference when the temporary back-end was active gets through
      ;; the default one.
      (plist-put info :internal-references
		 (plist-get new-info :internal-references)))))

(defun org-export-expand (blob contents &optional with-affiliated)
  "Expand a parsed element or object to its original state.

BLOB is either an element or an object.  CONTENTS is its
contents, as a string or nil.

When optional argument WITH-AFFILIATED is non-nil, add affiliated
keywords before output."
  (let ((type (org-element-type blob)))
    (concat (and with-affiliated
		 (eq (org-element-class blob) 'element)
		 (org-element--interpret-affiliated-keywords blob))
	    (funcall (intern (format "org-element-%s-interpreter" type))
		     blob contents))))



;;; The Filter System
;;
;; Filters allow end-users to tweak easily the transcoded output.
;; They are the functional counterpart of hooks, as every filter in
;; a set is applied to the return value of the previous one.
;;
;; Every set is back-end agnostic.  Although, a filter is always
;; called, in addition to the string it applies to, with the back-end
;; used as argument, so it's easy for the end-user to add back-end
;; specific filters in the set.  The communication channel, as
;; a plist, is required as the third argument.
;;
;; From the developer side, filters sets can be installed in the
;; process with the help of `org-export-define-backend', which
;; internally stores filters as an alist.  Each association has a key
;; among the following symbols and a function or a list of functions
;; as value.
;;
;; - `:filter-options' applies to the property list containing export
;;   options.  Unlike to other filters, functions in this list accept
;;   two arguments instead of three: the property list containing
;;   export options and the back-end.  Users can set its value through
;;   `org-export-filter-options-functions' variable.
;;
;; - `:filter-parse-tree' applies directly to the complete parsed
;;   tree.  Users can set it through
;;   `org-export-filter-parse-tree-functions' variable.
;;
;; - `:filter-body' applies to the body of the output, before template
;;   translator chimes in.  Users can set it through
;;   `org-export-filter-body-functions' variable.
;;
;; - `:filter-final-output' applies to the final transcoded string.
;;   Users can set it with `org-export-filter-final-output-functions'
;;   variable.
;;
;; - `:filter-plain-text' applies to any string not recognized as Org
;;   syntax.  `org-export-filter-plain-text-functions' allows users to
;;   configure it.
;;
;; - `:filter-TYPE' applies on the string returned after an element or
;;   object of type TYPE has been transcoded.  A user can modify
;;   `org-export-filter-TYPE-functions' to install these filters.
;;
;; All filters sets are applied with
;; `org-export-filter-apply-functions' function.  Filters in a set are
;; applied in a LIFO fashion.  It allows developers to be sure that
;; their filters will be applied first.
;;
;; Filters properties are installed in communication channel with
;; `org-export-install-filters' function.
;;
;; Eventually, two hooks (`org-export-before-processing-hook' and
;; `org-export-before-parsing-hook') are run at the beginning of the
;; export process and just before parsing to allow for heavy structure
;; modifications.


;;;; Hooks

(defvar org-export-before-processing-hook nil
  "Hook run at the beginning of the export process.

This is run before include keywords and macros are expanded and
Babel code blocks executed, on a copy of the original buffer
being exported.  Visibility and narrowing are preserved.  Point
is at the beginning of the buffer.

Every function in this hook will be called with one argument: the
back-end currently used, as a symbol.")

(defvar org-export-before-parsing-hook nil
  "Hook run before parsing an export buffer.

This is run after include keywords and macros have been expanded
and Babel code blocks executed, on a copy of the original buffer
being exported.  Visibility and narrowing are preserved.  Point
is at the beginning of the buffer.

Every function in this hook will be called with one argument: the
back-end currently used, as a symbol.")


;;;; Special Filters

(defvar org-export-filter-options-functions nil
  "List of functions applied to the export options.
Each filter is called with two arguments: the export options, as
a plist, and the back-end, as a symbol.  It must return
a property list containing export options.")

(defvar org-export-filter-parse-tree-functions nil
  "List of functions applied to the parsed tree.
Each filter is called with three arguments: the parse tree, as
returned by `org-element-parse-buffer', the back-end, as
a symbol, and the communication channel, as a plist.  It must
return the modified parse tree to transcode.")

(defvar org-export-filter-plain-text-functions nil
  "List of functions applied to plain text.
Each filter is called with three arguments: a string which
contains no Org syntax, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-body-functions nil
  "List of functions applied to transcoded body.
Each filter is called with three arguments: a string which
contains no Org syntax, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-final-output-functions nil
  "List of functions applied to the transcoded string.
Each filter is called with three arguments: the full transcoded
string, the back-end, as a symbol, and the communication channel,
as a plist.  It must return a string that will be used as the
final export output.")


;;;; Elements Filters

(defvar org-export-filter-babel-call-functions nil
  "List of functions applied to a transcoded babel-call.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-center-block-functions nil
  "List of functions applied to a transcoded center block.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-clock-functions nil
  "List of functions applied to a transcoded clock.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-diary-sexp-functions nil
  "List of functions applied to a transcoded diary-sexp.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-drawer-functions nil
  "List of functions applied to a transcoded drawer.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-dynamic-block-functions nil
  "List of functions applied to a transcoded dynamic-block.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-example-block-functions nil
  "List of functions applied to a transcoded example-block.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-export-block-functions nil
  "List of functions applied to a transcoded export-block.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-fixed-width-functions nil
  "List of functions applied to a transcoded fixed-width.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-footnote-definition-functions nil
  "List of functions applied to a transcoded footnote-definition.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-headline-functions nil
  "List of functions applied to a transcoded headline.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-horizontal-rule-functions nil
  "List of functions applied to a transcoded horizontal-rule.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-inlinetask-functions nil
  "List of functions applied to a transcoded inlinetask.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-item-functions nil
  "List of functions applied to a transcoded item.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-keyword-functions nil
  "List of functions applied to a transcoded keyword.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-latex-environment-functions nil
  "List of functions applied to a transcoded latex-environment.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-node-property-functions nil
  "List of functions applied to a transcoded node-property.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-paragraph-functions nil
  "List of functions applied to a transcoded paragraph.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-plain-list-functions nil
  "List of functions applied to a transcoded plain-list.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-planning-functions nil
  "List of functions applied to a transcoded planning.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-property-drawer-functions nil
  "List of functions applied to a transcoded property-drawer.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-quote-block-functions nil
  "List of functions applied to a transcoded quote block.
Each filter is called with three arguments: the transcoded quote
data, as a string, the back-end, as a symbol, and the
communication channel, as a plist.  It must return a string or
nil.")

(defvar org-export-filter-section-functions nil
  "List of functions applied to a transcoded section.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-special-block-functions nil
  "List of functions applied to a transcoded special block.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-src-block-functions nil
  "List of functions applied to a transcoded src-block.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-table-functions nil
  "List of functions applied to a transcoded table.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-table-cell-functions nil
  "List of functions applied to a transcoded table-cell.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-table-row-functions nil
  "List of functions applied to a transcoded table-row.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-verse-block-functions nil
  "List of functions applied to a transcoded verse block.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")


;;;; Objects Filters

(defvar org-export-filter-bold-functions nil
  "List of functions applied to transcoded bold text.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-code-functions nil
  "List of functions applied to transcoded code text.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-entity-functions nil
  "List of functions applied to a transcoded entity.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-export-snippet-functions nil
  "List of functions applied to a transcoded export-snippet.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-footnote-reference-functions nil
  "List of functions applied to a transcoded footnote-reference.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-inline-babel-call-functions nil
  "List of functions applied to a transcoded inline-babel-call.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-inline-src-block-functions nil
  "List of functions applied to a transcoded inline-src-block.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-italic-functions nil
  "List of functions applied to transcoded italic text.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-latex-fragment-functions nil
  "List of functions applied to a transcoded latex-fragment.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-line-break-functions nil
  "List of functions applied to a transcoded line-break.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-link-functions nil
  "List of functions applied to a transcoded link.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-radio-target-functions nil
  "List of functions applied to a transcoded radio-target.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-statistics-cookie-functions nil
  "List of functions applied to a transcoded statistics-cookie.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-strike-through-functions nil
  "List of functions applied to transcoded strike-through text.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-subscript-functions nil
  "List of functions applied to a transcoded subscript.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-superscript-functions nil
  "List of functions applied to a transcoded superscript.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-target-functions nil
  "List of functions applied to a transcoded target.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-timestamp-functions nil
  "List of functions applied to a transcoded timestamp.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-underline-functions nil
  "List of functions applied to transcoded underline text.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-verbatim-functions nil
  "List of functions applied to transcoded verbatim text.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")


;;;; Filters Tools
;;
;; Internal function `org-export-install-filters' installs filters
;; hard-coded in back-ends (developer filters) and filters from global
;; variables (user filters) in the communication channel.
;;
;; Internal function `org-export-filter-apply-functions' takes care
;; about applying each filter in order to a given data.  It ignores
;; filters returning a nil value but stops whenever a filter returns
;; an empty string.

(defun org-export-filter-apply-functions (filters value info)
  "Call every function in FILTERS.

Functions are called with three arguments: a value, the export
back-end name and the communication channel.  First function in
FILTERS is called with VALUE as its first argument.  Second
function in FILTERS is called with the previous result as its
value, etc.

Functions returning nil are skipped.  Any function returning the
empty string ends the process, which returns the empty string.

Call is done in a LIFO fashion, to be sure that developer
specified filters, if any, are called first."
  (catch :exit
    (let* ((backend (plist-get info :back-end))
	   (backend-name (and backend (org-export-backend-name backend))))
      (dolist (filter filters value)
	(let ((result (funcall filter value backend-name info)))
	  (cond ((not result))
		((equal result "") (throw :exit ""))
		(t (setq value result))))))))

(defun org-export-install-filters (info)
  "Install filters properties in communication channel.
INFO is a plist containing the current communication channel.
Return the updated communication channel."
  (let (plist)
    ;; Install user-defined filters with `org-export-filters-alist'
    ;; and filters already in INFO (through ext-plist mechanism).
    (dolist (p org-export-filters-alist)
      (let* ((prop (car p))
	     (info-value (plist-get info prop))
	     (default-value (symbol-value (cdr p))))
	(setq plist
	      (plist-put plist prop
			 ;; Filters in INFO will be called
			 ;; before those user provided.
			 (append (if (listp info-value) info-value
				   (list info-value))
				 default-value)))))
    ;; Prepend back-end specific filters to that list.
    (dolist (p (org-export-get-all-filters (plist-get info :back-end)))
      ;; Single values get consed, lists are appended.
      (let ((key (car p)) (value (cdr p)))
	(when value
	  (setq plist
		(plist-put
		 plist key
		 (if (atom value) (cons value (plist-get plist key))
		   (append value (plist-get plist key))))))))
    ;; Return new communication channel.
    (org-combine-plists info plist)))



;;; Core functions
;;
;; This is the room for the main function, `org-export-as', along with
;; its derivative, `org-export-string-as'.
;; `org-export--copy-to-kill-ring-p' determines if output of these
;; function should be added to kill ring.
;;
;; Note that `org-export-as' doesn't really parse the current buffer,
;; but a copy of it (with the same buffer-local variables and
;; visibility), where macros and include keywords are expanded and
;; Babel blocks are executed, if appropriate.
;; `org-export-with-buffer-copy' macro prepares that copy.
;;
;; File inclusion is taken care of by
;; `org-export-expand-include-keyword' and
;; `org-export--prepare-file-contents'.  Structure wise, including
;; a whole Org file in a buffer often makes little sense.  For
;; example, if the file contains a headline and the include keyword
;; was within an item, the item should contain the headline.  That's
;; why file inclusion should be done before any structure can be
;; associated to the file, that is before parsing.
;;
;; `org-export-insert-default-template' is a command to insert
;; a default template (or a back-end specific template) at point or in
;; current subtree.

(defun org-export-copy-buffer ()
  "Return a copy of the current buffer.
The copy preserves Org buffer-local variables, visibility and
narrowing."
  (let ((copy-buffer-fun (org-export--generate-copy-script (current-buffer)))
	(new-buf (generate-new-buffer (buffer-name))))
    (with-current-buffer new-buf
      (funcall copy-buffer-fun)
      (set-buffer-modified-p nil))
    new-buf))

(defmacro org-export-with-buffer-copy (&rest body)
  "Apply BODY in a copy of the current buffer.
The copy preserves local variables, visibility and contents of
the original buffer.  Point is at the beginning of the buffer
when BODY is applied."
  (declare (debug t))
  (org-with-gensyms (buf-copy)
    `(let ((,buf-copy (org-export-copy-buffer)))
       (unwind-protect
	   (with-current-buffer ,buf-copy
	     (goto-char (point-min))
	     (progn ,@body))
	 (and (buffer-live-p ,buf-copy)
	      ;; Kill copy without confirmation.
	      (progn (with-current-buffer ,buf-copy
		       (restore-buffer-modified-p nil))
		     (kill-buffer ,buf-copy)))))))

(defun org-export--generate-copy-script (buffer)
  "Generate a function duplicating BUFFER.

The copy will preserve local variables, visibility, contents and
narrowing of the original buffer.  If a region was active in
BUFFER, contents will be narrowed to that region instead.

The resulting function can be evaluated at a later time, from
another buffer, effectively cloning the original buffer there.

The function assumes BUFFER's major mode is `org-mode'."
  (with-current-buffer buffer
    `(lambda ()
       (let ((inhibit-modification-hooks t))
	 ;; Set major mode. Ignore `org-mode-hook' as it has been run
	 ;; already in BUFFER.
	 (let ((org-mode-hook nil) (org-inhibit-startup t)) (org-mode))
	 ;; Copy specific buffer local variables and variables set
	 ;; through BIND keywords.
	 ,@(let ((bound-variables (org-export--list-bound-variables))
		 vars)
	     (dolist (entry (buffer-local-variables (buffer-base-buffer)) vars)
	       (when (consp entry)
		 (let ((var (car entry))
		       (val (cdr entry)))
		   (and (not (memq var org-export-ignored-local-variables))
			(or (memq var
				  '(default-directory
				     buffer-file-name
				     buffer-file-coding-system))
			    (assq var bound-variables)
			    (string-match "^\\(org-\\|orgtbl-\\)"
					  (symbol-name var)))
			;; Skip unreadable values, as they cannot be
			;; sent to external process.
			(or (not val) (ignore-errors (read (format "%S" val))))
			(push `(set (make-local-variable (quote ,var))
				    (quote ,val))
			      vars))))))
	 ;; Whole buffer contents.
	 (insert
	  ,(org-with-wide-buffer
	    (buffer-substring-no-properties
	     (point-min) (point-max))))
	 ;; Narrowing.
	 ,(if (org-region-active-p)
	      `(narrow-to-region ,(region-beginning) ,(region-end))
	    `(narrow-to-region ,(point-min) ,(point-max)))
	 ;; Current position of point.
	 (goto-char ,(point))
	 ;; Overlays with invisible property.
	 ,@(let (ov-set)
	     (dolist (ov (overlays-in (point-min) (point-max)) ov-set)
	       (let ((invis-prop (overlay-get ov 'invisible)))
		 (when invis-prop
		   (push `(overlay-put
			   (make-overlay ,(overlay-start ov)
					 ,(overlay-end ov))
			   'invisible (quote ,invis-prop))
			 ov-set)))))))))

(defun org-export--delete-comment-trees ()
  "Delete commented trees and commented inlinetasks in the buffer.
Narrowing, if any, is ignored."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let* ((case-fold-search t)
	  (regexp (concat org-outline-regexp-bol ".*" org-comment-string)))
     (while (re-search-forward regexp nil t)
       (let ((element (org-element-at-point)))
	 (when (org-element-property :commentedp element)
	   (delete-region (org-element-property :begin element)
			  (org-element-property :end element))))))))

(defun org-export--prune-tree (data info)
  "Prune non exportable elements from DATA.
DATA is the parse tree to traverse.  INFO is the plist holding
export info.  Also set `:ignore-list' in INFO to a list of
objects which should be ignored during export, but not removed
from tree."
  (letrec ((ignore nil)
	   ;; First find trees containing a select tag, if any.
	   (selected (org-export--selected-trees data info))
	   (walk-data
	    (lambda (data)
	      ;; Prune non-exportable elements and objects from tree.
	      ;; As a special case, special rows and cells from tables
	      ;; are stored in IGNORE, as they still need to be
	      ;; accessed during export.
	      (when data
		(let ((type (org-element-type data)))
		  (if (org-export--skip-p data info selected)
		      (if (memq type '(table-cell table-row)) (push data ignore)
			(org-element-extract-element data))
		    (if (and (eq type 'headline)
			     (eq (plist-get info :with-archived-trees)
				 'headline)
			     (org-element-property :archivedp data))
			;; If headline is archived but tree below has
			;; to be skipped, remove contents.
			(org-element-set-contents data)
		      ;; Move into recursive objects/elements.
		      (mapc walk-data (org-element-contents data)))
		    ;; Move into secondary string, if any.
		    (dolist (p (cdr (assq type
					  org-element-secondary-value-alist)))
		      (mapc walk-data (org-element-property p data))))))))
	   (definitions
	     ;; Collect definitions before possibly pruning them so as
	     ;; to avoid parsing them again if they are required.
	     (org-element-map data '(footnote-definition footnote-reference)
	       (lambda (f)
		 (cond
		  ((eq 'footnote-definition (org-element-type f)) f)
		  ((and (eq 'inline (org-element-property :type f))
			(org-element-property :label f))
		   f)
		  (t nil))))))
    ;; If a select tag is active, also ignore the section before the
    ;; first headline, if any.
    (when selected
      (let ((first-element (car (org-element-contents data))))
	(when (eq (org-element-type first-element) 'section)
	  (org-element-extract-element first-element))))
    ;; Prune tree and communication channel.
    (funcall walk-data data)
    (dolist (entry (append
		    ;; Priority is given to back-end specific options.
		    (org-export-get-all-options (plist-get info :back-end))
		    org-export-options-alist))
      (when (eq (nth 4 entry) 'parse)
	(funcall walk-data (plist-get info (car entry)))))
    (let ((missing (org-export--missing-definitions data definitions)))
      (funcall walk-data missing)
      (org-export--install-footnote-definitions missing data))
    ;; Eventually set `:ignore-list'.
    (plist-put info :ignore-list ignore)))

(defun org-export--missing-definitions (tree definitions)
  "List footnote definitions missing from TREE.
Missing definitions are searched within DEFINITIONS, which is
a list of footnote definitions or in the widened buffer."
  (let* ((list-labels
	  (lambda (data)
	    ;; List all footnote labels encountered in DATA.  Inline
	    ;; footnote references are ignored.
	    (org-element-map data 'footnote-reference
	      (lambda (reference)
		(and (eq (org-element-property :type reference) 'standard)
		     (org-element-property :label reference))))))
	 defined undefined missing-definitions)
    ;; Partition DIRECT-REFERENCES between DEFINED and UNDEFINED
    ;; references.
    (let ((known-definitions
	   (org-element-map tree '(footnote-reference footnote-definition)
	     (lambda (f)
	       (and (or (eq (org-element-type f) 'footnote-definition)
			(eq (org-element-property :type f) 'inline))
		    (org-element-property :label f)))))
	  seen)
      (dolist (l (funcall list-labels tree))
	(cond ((member l seen))
	      ((member l known-definitions) (push l defined))
	      (t (push l undefined)))))
    ;; Complete MISSING-DEFINITIONS by finding the definition of every
    ;; undefined label, first by looking into DEFINITIONS, then by
    ;; searching the widened buffer.  This is a recursive process
    ;; since definitions found can themselves contain an undefined
    ;; reference.
    (while undefined
      (let* ((label (pop undefined))
	     (definition
	       (cond
		((cl-some
		  (lambda (d) (and (equal (org-element-property :label d) label)
			      d))
		  definitions))
		((pcase (org-footnote-get-definition label)
		   (`(,_ ,beg . ,_)
		    (org-with-wide-buffer
		     (goto-char beg)
		     (let ((datum (org-element-context)))
		       (if (eq (org-element-type datum) 'footnote-reference)
			   datum
			 ;; Parse definition with contents.
			 (save-restriction
			   (narrow-to-region
			    (org-element-property :begin datum)
			    (org-element-property :end datum))
			   (org-element-map (org-element-parse-buffer)
			       'footnote-definition #'identity nil t))))))
		   (_ nil)))
		(t (user-error "Definition not found for footnote %s" label)))))
	(push label defined)
	(push definition missing-definitions)
	;; Look for footnote references within DEFINITION, since
	;; we may need to also find their definition.
	(dolist (l (funcall list-labels definition))
	  (unless (or (member l defined)    ;Known label
		      (member l undefined)) ;Processed later
	    (push l undefined)))))
    ;; MISSING-DEFINITIONS may contain footnote references with inline
    ;; definitions.  Make sure those are changed into real footnote
    ;; definitions.
    (mapcar (lambda (d)
	      (if (eq (org-element-type d) 'footnote-definition) d
		(let ((label (org-element-property :label d)))
		  (apply #'org-element-create
			 'footnote-definition `(:label ,label :post-blank 1)
			 (org-element-contents d)))))
	    missing-definitions)))

(defun org-export--install-footnote-definitions (definitions tree)
  "Install footnote definitions in tree.

DEFINITIONS is the list of footnote definitions to install.  TREE
is the parse tree.

If there is a footnote section in TREE, definitions found are
appended to it.  If `org-footnote-section' is non-nil, a new
footnote section containing all definitions is inserted in TREE.
Otherwise, definitions are appended at the end of the section
containing their first reference."
  (cond
   ((null definitions))
   ;; If there is a footnote section, insert definitions there.
   ((let ((footnote-section
	   (org-element-map tree 'headline
	     (lambda (h) (and (org-element-property :footnote-section-p h) h))
	     nil t)))
      (and footnote-section
	   (apply #'org-element-adopt-elements
		  footnote-section
		  (nreverse definitions)))))
   ;; If there should be a footnote section, create one containing all
   ;; the definitions at the end of the tree.
   (org-footnote-section
    (org-element-adopt-elements
     tree
     (org-element-create 'headline
			 (list :footnote-section-p t
			       :level 1
			       :title org-footnote-section
			       :raw-value org-footnote-section)
			 (apply #'org-element-create
				'section
				nil
				(nreverse definitions)))))
   ;; Otherwise add each definition at the end of the section where it
   ;; is first referenced.
   (t
    (letrec ((seen nil)
	     (insert-definitions
	      (lambda (data)
		;; Insert footnote definitions in the same section as
		;; their first reference in DATA.
		(org-element-map data 'footnote-reference
		  (lambda (reference)
		    (when (eq (org-element-property :type reference) 'standard)
		      (let ((label (org-element-property :label reference)))
			(unless (member label seen)
			  (push label seen)
			  (let ((definition
				  (cl-some
				   (lambda (d)
				     (and (equal (org-element-property :label d)
						 label)
					  d))
				   definitions)))
			    (org-element-adopt-elements
			     (org-element-lineage reference '(section))
			     definition)
			    ;; Also insert definitions for nested
			    ;; references, if any.
			    (funcall insert-definitions definition))))))))))
      (funcall insert-definitions tree)))))

(defun org-export--remove-uninterpreted-data (data info)
  "Change uninterpreted elements back into Org syntax.
DATA is a parse tree or a secondary string.  INFO is a plist
containing export options.  It is modified by side effect and
returned by the function."
  (org-element-map data
      '(entity bold italic latex-environment latex-fragment strike-through
	       subscript superscript underline)
    (lambda (datum)
      (let ((new
	     (cl-case (org-element-type datum)
	       ;; ... entities...
	       (entity
		(and (not (plist-get info :with-entities))
		     (list (concat
			    (org-export-expand datum nil)
			    (make-string
			     (or (org-element-property :post-blank datum) 0)
			     ?\s)))))
	       ;; ... emphasis...
	       ((bold italic strike-through underline)
		(and (not (plist-get info :with-emphasize))
		     (let ((marker (cl-case (org-element-type datum)
				     (bold "*")
				     (italic "/")
				     (strike-through "+")
				     (underline "_"))))
		       (append
			(list marker)
			(org-element-contents datum)
			(list (concat
			       marker
			       (make-string
				(or (org-element-property :post-blank datum)
				    0)
				?\s)))))))
	       ;; ... LaTeX environments and fragments...
	       ((latex-environment latex-fragment)
		(and (eq (plist-get info :with-latex) 'verbatim)
		     (list (org-export-expand datum nil))))
	       ;; ... sub/superscripts...
	       ((subscript superscript)
		(let ((sub/super-p (plist-get info :with-sub-superscript))
		      (bracketp (org-element-property :use-brackets-p datum)))
		  (and (or (not sub/super-p)
			   (and (eq sub/super-p '{}) (not bracketp)))
		       (append
			(list (concat
			       (if (eq (org-element-type datum) 'subscript)
				   "_"
				 "^")
			       (and bracketp "{")))
			(org-element-contents datum)
			(list (concat
			       (and bracketp "}")
			       (and (org-element-property :post-blank datum)
				    (make-string
				     (org-element-property :post-blank datum)
				     ?\s)))))))))))
	(when new
	  ;; Splice NEW at DATUM location in parse tree.
	  (dolist (e new (org-element-extract-element datum))
	    (unless (equal e "") (org-element-insert-before e datum))))))
    info nil nil t)
  ;; Return modified parse tree.
  data)

;;;###autoload
(defun org-export-as
    (backend &optional subtreep visible-only body-only ext-plist)
  "Transcode current Org buffer into BACKEND code.

BACKEND is either an export back-end, as returned by, e.g.,
`org-export-create-backend', or a symbol referring to
a registered back-end.

If narrowing is active in the current buffer, only transcode its
narrowed part.

If a region is active, transcode that region.

When optional argument SUBTREEP is non-nil, transcode the
sub-tree at point, extracting information from the headline
properties first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only return body
code, without surrounding template.

Optional argument EXT-PLIST, when provided, is a property list
with external parameters overriding Org default settings, but
still inferior to file-local settings.

Return code as a string."
  (when (symbolp backend) (setq backend (org-export-get-backend backend)))
  (org-export-barf-if-invalid-backend backend)
  (save-excursion
    (save-restriction
      ;; Narrow buffer to an appropriate region or subtree for
      ;; parsing.  If parsing subtree, be sure to remove main
      ;; headline, planning data and property drawer.
      (cond ((org-region-active-p)
	     (narrow-to-region (region-beginning) (region-end)))
	    (subtreep
	     (org-narrow-to-subtree)
	     (goto-char (point-min))
	     (org-end-of-meta-data)
	     (narrow-to-region (point) (point-max))))
      ;; Initialize communication channel with original buffer
      ;; attributes, unavailable in its copy.
      (let* ((org-export-current-backend (org-export-backend-name backend))
	     (info (org-combine-plists
		    (org-export--get-export-attributes
		     backend subtreep visible-only body-only)
		    (org-export--get-buffer-attributes)))
	     (parsed-keywords
	      (delq nil
		    (mapcar (lambda (o) (and (eq (nth 4 o) 'parse) (nth 1 o)))
			    (append (org-export-get-all-options backend)
				    org-export-options-alist))))
	     tree)
	;; Update communication channel and get parse tree.  Buffer
	;; isn't parsed directly.  Instead, all buffer modifications
	;; and consequent parsing are undertaken in a temporary copy.
	(org-export-with-buffer-copy
	 ;; Run first hook with current back-end's name as argument.
	 (run-hook-with-args 'org-export-before-processing-hook
			     (org-export-backend-name backend))
	 ;; Include files, delete comments and expand macros.
	 (org-export-expand-include-keyword)
	 (org-export--delete-comment-trees)
	 (org-macro-initialize-templates)
	 (org-macro-replace-all
	  (append org-macro-templates org-export-global-macros)
	  nil parsed-keywords)
	 ;; Refresh buffer properties and radio targets after
	 ;; potentially invasive previous changes.  Likewise, do it
	 ;; again after executing Babel code.
	 (org-set-regexps-and-options)
	 (org-update-radio-target-regexp)
	 (when org-export-use-babel
	   (org-babel-exp-process-buffer)
	   (org-set-regexps-and-options)
	   (org-update-radio-target-regexp))
	 ;; Run last hook with current back-end's name as argument.
	 ;; Update buffer properties and radio targets one last time
	 ;; before parsing.
	 (goto-char (point-min))
	 (save-excursion
	   (run-hook-with-args 'org-export-before-parsing-hook
			       (org-export-backend-name backend)))
	 (org-set-regexps-and-options)
	 (org-update-radio-target-regexp)
	 ;; Update communication channel with environment.
	 (setq info
	       (org-combine-plists
		info (org-export-get-environment backend subtreep ext-plist)))
	 ;; De-activate uninterpreted data from parsed keywords.
	 (dolist (entry (append (org-export-get-all-options backend)
				org-export-options-alist))
	   (pcase entry
	     (`(,p ,_ ,_ ,_ parse)
	      (let ((value (plist-get info p)))
		(plist-put info
			   p
			   (org-export--remove-uninterpreted-data value info))))
	     (_ nil)))
	 ;; Install user's and developer's filters.
	 (setq info (org-export-install-filters info))
	 ;; Call options filters and update export options.  We do not
	 ;; use `org-export-filter-apply-functions' here since the
	 ;; arity of such filters is different.
	 (let ((backend-name (org-export-backend-name backend)))
	   (dolist (filter (plist-get info :filter-options))
	     (let ((result (funcall filter info backend-name)))
	       (when result (setq info result)))))
	 ;; Expand export-specific set of macros: {{{author}}},
	 ;; {{{date(FORMAT)}}}, {{{email}}} and {{{title}}}.  It must
	 ;; be done once regular macros have been expanded, since
	 ;; parsed keywords may contain one of them.
	 (org-macro-replace-all
	  (list
	   (cons "author" (org-element-interpret-data (plist-get info :author)))
	   (cons "date"
		 (let* ((date (plist-get info :date))
			(value (or (org-element-interpret-data date) "")))
		   (if (and (consp date)
			    (not (cdr date))
			    (eq (org-element-type (car date)) 'timestamp))
		       (format "(eval (if (org-string-nw-p \"$1\") %s %S))"
			       (format "(org-timestamp-format '%S \"$1\")"
				       (org-element-copy (car date)))
			       value)
		     value)))
	   (cons "email" (org-element-interpret-data (plist-get info :email)))
	   (cons "title" (org-element-interpret-data (plist-get info :title)))
	   (cons "results" "$1"))
	  'finalize
	  parsed-keywords)
	 ;; Parse buffer.
	 (setq tree (org-element-parse-buffer nil visible-only))
	 ;; Prune tree from non-exported elements and transform
	 ;; uninterpreted elements or objects in both parse tree and
	 ;; communication channel.
	 (org-export--prune-tree tree info)
	 (org-export--remove-uninterpreted-data tree info)
	 ;; Call parse tree filters.
	 (setq tree
	       (org-export-filter-apply-functions
		(plist-get info :filter-parse-tree) tree info))
	 ;; Now tree is complete, compute its properties and add them
	 ;; to communication channel.
	 (setq info (org-export--collect-tree-properties tree info))
	 ;; Eventually transcode TREE.  Wrap the resulting string into
	 ;; a template.
	 (let* ((body (org-element-normalize-string
		       (or (org-export-data tree info) "")))
		(inner-template (cdr (assq 'inner-template
					   (plist-get info :translate-alist))))
		(full-body (org-export-filter-apply-functions
			    (plist-get info :filter-body)
			    (if (not (functionp inner-template)) body
			      (funcall inner-template body info))
			    info))
		(template (cdr (assq 'template
				     (plist-get info :translate-alist)))))
	   ;; Remove all text properties since they cannot be
	   ;; retrieved from an external process.  Finally call
	   ;; final-output filter and return result.
	   (org-no-properties
	    (org-export-filter-apply-functions
	     (plist-get info :filter-final-output)
	     (if (or (not (functionp template)) body-only) full-body
	       (funcall template full-body info))
	     info))))))))

;;;###autoload
(defun org-export-string-as (string backend &optional body-only ext-plist)
  "Transcode STRING into BACKEND code.

BACKEND is either an export back-end, as returned by, e.g.,
`org-export-create-backend', or a symbol referring to
a registered back-end.

When optional argument BODY-ONLY is non-nil, only return body
code, without preamble nor postamble.

Optional argument EXT-PLIST, when provided, is a property list
with external parameters overriding Org default settings, but
still inferior to file-local settings.

Return code as a string."
  (with-temp-buffer
    (insert string)
    (let ((org-inhibit-startup t)) (org-mode))
    (org-export-as backend nil nil body-only ext-plist)))

;;;###autoload
(defun org-export-replace-region-by (backend)
  "Replace the active region by its export to BACKEND.
BACKEND is either an export back-end, as returned by, e.g.,
`org-export-create-backend', or a symbol referring to
a registered back-end."
  (unless (org-region-active-p) (user-error "No active region to replace"))
  (insert
   (org-export-string-as
    (delete-and-extract-region (region-beginning) (region-end)) backend t)))

;;;###autoload
(defun org-export-insert-default-template (&optional backend subtreep)
  "Insert all export keywords with default values at beginning of line.

BACKEND is a symbol referring to the name of a registered export
back-end, for which specific export options should be added to
the template, or `default' for default template.  When it is nil,
the user will be prompted for a category.

If SUBTREEP is non-nil, export configuration will be set up
locally for the subtree through node properties."
  (interactive)
  (unless (derived-mode-p 'org-mode) (user-error "Not in an Org mode buffer"))
  (when (and subtreep (org-before-first-heading-p))
    (user-error "No subtree to set export options for"))
  (let ((node (and subtreep (save-excursion (org-back-to-heading t) (point))))
	(backend
	 (or backend
	     (intern
	      (org-completing-read
	       "Options category: "
	       (cons "default"
		     (mapcar (lambda (b)
			       (symbol-name (org-export-backend-name b)))
			     org-export-registered-backends))
	       nil t))))
	options keywords)
    ;; Populate OPTIONS and KEYWORDS.
    (dolist (entry (cond ((eq backend 'default) org-export-options-alist)
			 ((org-export-backend-p backend)
			  (org-export-backend-options backend))
			 (t (org-export-backend-options
			     (org-export-get-backend backend)))))
      (let ((keyword (nth 1 entry))
            (option (nth 2 entry)))
        (cond
         (keyword (unless (assoc keyword keywords)
                    (let ((value
                           (if (eq (nth 4 entry) 'split)
                               (mapconcat #'identity (eval (nth 3 entry)) " ")
                             (eval (nth 3 entry)))))
                      (push (cons keyword value) keywords))))
         (option (unless (assoc option options)
                   (push (cons option (eval (nth 3 entry))) options))))))
    ;; Move to an appropriate location in order to insert options.
    (unless subtreep (beginning-of-line))
    ;; First (multiple) OPTIONS lines.  Never go past fill-column.
    (when options
      (let ((items
	     (mapcar
	      #'(lambda (opt) (format "%s:%S" (car opt) (cdr opt)))
	      (sort options (lambda (k1 k2) (string< (car k1) (car k2)))))))
	(if subtreep
	    (org-entry-put
	     node "EXPORT_OPTIONS" (mapconcat 'identity items " "))
	  (while items
	    (insert "#+OPTIONS:")
	    (let ((width 10))
	      (while (and items
			  (< (+ width (length (car items)) 1) fill-column))
		(let ((item (pop items)))
		  (insert " " item)
		  (cl-incf width (1+ (length item))))))
	    (insert "\n")))))
    ;; Then the rest of keywords, in the order specified in either
    ;; `org-export-options-alist' or respective export back-ends.
    (dolist (key (nreverse keywords))
      (let ((val (cond ((equal (car key) "DATE")
			(or (cdr key)
			    (with-temp-buffer
			      (org-insert-time-stamp (current-time)))))
		       ((equal (car key) "TITLE")
			(or (let ((visited-file
				   (buffer-file-name (buffer-base-buffer))))
			      (and visited-file
				   (file-name-sans-extension
				    (file-name-nondirectory visited-file))))
			    (buffer-name (buffer-base-buffer))))
		       (t (cdr key)))))
	(if subtreep (org-entry-put node (concat "EXPORT_" (car key)) val)
	  (insert
	   (format "#+%s:%s\n"
		   (car key)
		   (if (org-string-nw-p val) (format " %s" val) ""))))))))

(defun org-export-expand-include-keyword (&optional included dir footnotes)
  "Expand every include keyword in buffer.
Optional argument INCLUDED is a list of included file names along
with their line restriction, when appropriate.  It is used to
avoid infinite recursion.  Optional argument DIR is the current
working directory.  It is used to properly resolve relative
paths.  Optional argument FOOTNOTES is a hash-table used for
storing and resolving footnotes.  It is created automatically."
  (let ((case-fold-search t)
	(file-prefix (make-hash-table :test #'equal))
	(current-prefix 0)
	(footnotes (or footnotes (make-hash-table :test #'equal)))
	(include-re "^[ \t]*#\\+INCLUDE:"))
    ;; If :minlevel is not set the text-property
    ;; `:org-include-induced-level' will be used to determine the
    ;; relative level when expanding INCLUDE.
    ;; Only affects included Org documents.
    (goto-char (point-min))
    (while (re-search-forward include-re nil t)
      (put-text-property (line-beginning-position) (line-end-position)
			 :org-include-induced-level
			 (1+ (org-reduced-level (or (org-current-level) 0)))))
    ;; Expand INCLUDE keywords.
    (goto-char (point-min))
    (while (re-search-forward include-re nil t)
      (unless (org-in-commented-heading-p)
	(let ((element (save-match-data (org-element-at-point))))
	  (when (eq (org-element-type element) 'keyword)
	    (beginning-of-line)
	    ;; Extract arguments from keyword's value.
	    (let* ((value (org-element-property :value element))
		   (ind (org-get-indentation))
		   location
		   (file
		    (and (string-match
			  "^\\(\".+?\"\\|\\S-+\\)\\(?:\\s-+\\|$\\)" value)
			 (prog1
			     (save-match-data
			       (let ((matched (match-string 1 value)))
				 (when (string-match "\\(::\\(.*?\\)\\)\"?\\'"
						     matched)
				   (setq location (match-string 2 matched))
				   (setq matched
					 (replace-match "" nil nil matched 1)))
				 (expand-file-name
				  (org-unbracket-string "\"" "\"" matched)
				  dir)))
			   (setq value (replace-match "" nil nil value)))))
		   (only-contents
		    (and (string-match ":only-contents *\\([^: \r\t\n]\\S-*\\)?"
				       value)
			 (prog1 (org-not-nil (match-string 1 value))
			   (setq value (replace-match "" nil nil value)))))
		   (lines
		    (and (string-match
			  ":lines +\"\\(\\(?:[0-9]+\\)?-\\(?:[0-9]+\\)?\\)\""
			  value)
			 (prog1 (match-string 1 value)
			   (setq value (replace-match "" nil nil value)))))
		   (env (cond
			 ((string-match "\\<example\\>" value) 'literal)
			 ((string-match "\\<export\\(?: +\\(.*\\)\\)?" value)
			  'literal)
			 ((string-match "\\<src\\(?: +\\(.*\\)\\)?" value)
			  'literal)))
		   ;; Minimal level of included file defaults to the
		   ;; child level of the current headline, if any, or
		   ;; one.  It only applies is the file is meant to be
		   ;; included as an Org one.
		   (minlevel
		    (and (not env)
			 (if (string-match ":minlevel +\\([0-9]+\\)" value)
			     (prog1 (string-to-number (match-string 1 value))
			       (setq value (replace-match "" nil nil value)))
			   (get-text-property (point)
					      :org-include-induced-level))))
		   (args (and (eq env 'literal) (match-string 1 value)))
		   (block (and (string-match "\\<\\(\\S-+\\)\\>" value)
			       (match-string 1 value))))
	      ;; Remove keyword.
	      (delete-region (point) (line-beginning-position 2))
	      (cond
	       ((not file) nil)
	       ((not (file-readable-p file))
		(error "Cannot include file %s" file))
	       ;; Check if files has already been parsed.  Look after
	       ;; inclusion lines too, as different parts of the same
	       ;; file can be included too.
	       ((member (list file lines) included)
		(error "Recursive file inclusion: %s" file))
	       (t
		(cond
		 ((eq env 'literal)
		  (insert
		   (let ((ind-str (make-string ind ?\s))
			 (arg-str (if (stringp args) (format " %s" args) ""))
			 (contents
			  (org-escape-code-in-string
			   (org-export--prepare-file-contents file lines))))
		     (format "%s#+BEGIN_%s%s\n%s%s#+END_%s\n"
			     ind-str block arg-str contents ind-str block))))
		 ((stringp block)
		  (insert
		   (let ((ind-str (make-string ind ?\s))
			 (contents
			  (org-export--prepare-file-contents file lines)))
		     (format "%s#+BEGIN_%s\n%s%s#+END_%s\n"
			     ind-str block contents ind-str block))))
		 (t
		  (insert
		   (with-temp-buffer
		     (let ((org-inhibit-startup t)
			   (lines
			    (if location
				(org-export--inclusion-absolute-lines
				 file location only-contents lines)
			      lines)))
		       (org-mode)
		       (insert
			(org-export--prepare-file-contents
			 file lines ind minlevel
			 (or
			  (gethash file file-prefix)
			  (puthash file (cl-incf current-prefix) file-prefix))
			 footnotes)))
		     (org-export-expand-include-keyword
		      (cons (list file lines) included)
		      (file-name-directory file)
		      footnotes)
		     (buffer-string)))))
		;; Expand footnotes after all files have been
		;; included.  Footnotes are stored at end of buffer.
		(unless included
		  (org-with-wide-buffer
		   (goto-char (point-max))
		   (maphash (lambda (k v)
			      (insert (format "\n[fn:%s] %s\n" k v)))
			    footnotes))))))))))))

(defun org-export--inclusion-absolute-lines (file location only-contents lines)
  "Resolve absolute lines for an included file with file-link.

FILE is string file-name of the file to include.  LOCATION is a
string name within FILE to be included (located via
`org-link-search').  If ONLY-CONTENTS is non-nil only the
contents of the named element will be included, as determined
Org-Element.  If LINES is non-nil only those lines are included.

Return a string of lines to be included in the format expected by
`org-export--prepare-file-contents'."
  (with-temp-buffer
    (insert-file-contents file)
    (unless (eq major-mode 'org-mode)
      (let ((org-inhibit-startup t)) (org-mode)))
    (condition-case err
	;; Enforce consistent search.
	(let ((org-link-search-must-match-exact-headline nil))
	  (org-link-search location))
      (error
       (error "%s for %s::%s" (error-message-string err) file location)))
    (let* ((element (org-element-at-point))
	   (contents-begin
	    (and only-contents (org-element-property :contents-begin element))))
      (narrow-to-region
       (or contents-begin (org-element-property :begin element))
       (org-element-property (if contents-begin :contents-end :end) element))
      (when (and only-contents
		 (memq (org-element-type element) '(headline inlinetask)))
	;; Skip planning line and property-drawer.
	(goto-char (point-min))
	(when (looking-at-p org-planning-line-re) (forward-line))
	(when (looking-at org-property-drawer-re) (goto-char (match-end 0)))
	(unless (bolp) (forward-line))
	(narrow-to-region (point) (point-max))))
    (when lines
      (org-skip-whitespace)
      (beginning-of-line)
      (let* ((lines (split-string lines "-"))
	     (lbeg (string-to-number (car lines)))
	     (lend (string-to-number (cadr lines)))
	     (beg (if (zerop lbeg) (point-min)
		    (goto-char (point-min))
		    (forward-line (1- lbeg))
		    (point)))
	     (end (if (zerop lend) (point-max)
		    (goto-char beg)
		    (forward-line (1- lend))
		    (point))))
	(narrow-to-region beg end)))
    (let ((end (point-max)))
      (goto-char (point-min))
      (widen)
      (let ((start-line (line-number-at-pos)))
	(format "%d-%d"
		start-line
		(save-excursion
		  (+ start-line
		     (let ((counter 0))
		       (while (< (point) end) (cl-incf counter) (forward-line))
		       counter))))))))

(defun org-export--prepare-file-contents
    (file &optional lines ind minlevel id footnotes)
  "Prepare contents of FILE for inclusion and return it as a string.

When optional argument LINES is a string specifying a range of
lines, include only those lines.

Optional argument IND, when non-nil, is an integer specifying the
global indentation of returned contents.  Since its purpose is to
allow an included file to stay in the same environment it was
created (e.g., a list item), it doesn't apply past the first
headline encountered.

Optional argument MINLEVEL, when non-nil, is an integer
specifying the level that any top-level headline in the included
file should have.

Optional argument ID is an integer that will be inserted before
each footnote definition and reference if FILE is an Org file.
This is useful to avoid conflicts when more than one Org file
with footnotes is included in a document.

Optional argument FOOTNOTES is a hash-table to store footnotes in
the included document."
  (with-temp-buffer
    (insert-file-contents file)
    (when lines
      (let* ((lines (split-string lines "-"))
	     (lbeg (string-to-number (car lines)))
	     (lend (string-to-number (cadr lines)))
	     (beg (if (zerop lbeg) (point-min)
		    (goto-char (point-min))
		    (forward-line (1- lbeg))
		    (point)))
	     (end (if (zerop lend) (point-max)
		    (goto-char (point-min))
		    (forward-line (1- lend))
		    (point))))
	(narrow-to-region beg end)))
    ;; Remove blank lines at beginning and end of contents.  The logic
    ;; behind that removal is that blank lines around include keyword
    ;; override blank lines in included file.
    (goto-char (point-min))
    (org-skip-whitespace)
    (beginning-of-line)
    (delete-region (point-min) (point))
    (goto-char (point-max))
    (skip-chars-backward " \r\t\n")
    (forward-line)
    (delete-region (point) (point-max))
    ;; If IND is set, preserve indentation of include keyword until
    ;; the first headline encountered.
    (when (and ind (> ind 0))
      (unless (eq major-mode 'org-mode)
	(let ((org-inhibit-startup t)) (org-mode)))
      (goto-char (point-min))
      (let ((ind-str (make-string ind ?\s)))
	(while (not (or (eobp) (looking-at org-outline-regexp-bol)))
	  ;; Do not move footnote definitions out of column 0.
	  (unless (and (looking-at org-footnote-definition-re)
		       (eq (org-element-type (org-element-at-point))
			   'footnote-definition))
	    (insert ind-str))
	  (forward-line))))
    ;; When MINLEVEL is specified, compute minimal level for headlines
    ;; in the file (CUR-MIN), and remove stars to each headline so
    ;; that headlines with minimal level have a level of MINLEVEL.
    (when minlevel
      (unless (eq major-mode 'org-mode)
	(let ((org-inhibit-startup t)) (org-mode)))
      (org-with-limited-levels
       (let ((levels (org-map-entries
		      (lambda () (org-reduced-level (org-current-level))))))
	 (when levels
	   (let ((offset (- minlevel (apply #'min levels))))
	     (unless (zerop offset)
	       (when org-odd-levels-only (setq offset (* offset 2)))
	       ;; Only change stars, don't bother moving whole
	       ;; sections.
	       (org-map-entries
		(lambda ()
		  (if (< offset 0) (delete-char (abs offset))
		    (insert (make-string offset ?*)))))))))))
    ;; Append ID to all footnote references and definitions, so they
    ;; become file specific and cannot collide with footnotes in other
    ;; included files.  Further, collect relevant footnote definitions
    ;; outside of LINES, in order to reintroduce them later.
    (when id
      (let ((marker-min (point-min-marker))
	    (marker-max (point-max-marker))
	    (get-new-label
	     (lambda (label)
	       ;; Generate new label from LABEL by prefixing it with
	       ;; "-ID-".
	       (format "-%d-%s" id label)))
	    (set-new-label
	     (lambda (f old new)
	       ;; Replace OLD label with NEW in footnote F.
	       (save-excursion
		 (goto-char (+ (org-element-property :begin f) 4))
		 (looking-at (regexp-quote old))
		 (replace-match new))))
	    (seen-alist))
	(goto-char (point-min))
	(while (re-search-forward org-footnote-re nil t)
	  (let ((footnote (save-excursion
			    (backward-char)
			    (org-element-context))))
	    (when (memq (org-element-type footnote)
			'(footnote-definition footnote-reference))
	      (let* ((label (org-element-property :label footnote)))
		;; Update the footnote-reference at point and collect
		;; the new label, which is only used for footnotes
		;; outsides LINES.
		(when label
		  (let ((seen (cdr (assoc label seen-alist))))
		    (if seen (funcall set-new-label footnote label seen)
		      (let ((new (funcall get-new-label label)))
			(push (cons label new) seen-alist)
			(org-with-wide-buffer
			 (let* ((def (org-footnote-get-definition label))
				(beg (nth 1 def)))
			   (when (and def
				      (or (< beg marker-min)
					  (>= beg marker-max)))
			     ;; Store since footnote-definition is
			     ;; outside of LINES.
			     (puthash new
				      (org-element-normalize-string (nth 3 def))
				      footnotes))))
			(funcall set-new-label footnote label new)))))))))
	(set-marker marker-min nil)
	(set-marker marker-max nil)))
    (org-element-normalize-string (buffer-string))))

(defun org-export--copy-to-kill-ring-p ()
  "Return a non-nil value when output should be added to the kill ring.
See also `org-export-copy-to-kill-ring'."
  (if (eq org-export-copy-to-kill-ring 'if-interactive)
      (not (or executing-kbd-macro noninteractive))
    (eq org-export-copy-to-kill-ring t)))



;;; Tools For Back-Ends
;;
;; A whole set of tools is available to help build new exporters.  Any
;; function general enough to have its use across many back-ends
;; should be added here.

;;;; For Affiliated Keywords
;;
;; `org-export-read-attribute' reads a property from a given element
;;  as a plist.  It can be used to normalize affiliated keywords'
;;  syntax.
;;
;; Since captions can span over multiple lines and accept dual values,
;; their internal representation is a bit tricky.  Therefore,
;; `org-export-get-caption' transparently returns a given element's
;; caption as a secondary string.

(defun org-export-read-attribute (attribute element &optional property)
  "Turn ATTRIBUTE property from ELEMENT into a plist.

When optional argument PROPERTY is non-nil, return the value of
that property within attributes.

This function assumes attributes are defined as \":keyword
value\" pairs.  It is appropriate for `:attr_html' like
properties.

All values will become strings except the empty string and
\"nil\", which will become nil.  Also, values containing only
double quotes will be read as-is, which means that \"\" value
will become the empty string."
  (let* ((prepare-value
	  (lambda (str)
	    (save-match-data
	      (cond ((member str '(nil "" "nil")) nil)
		    ((string-match "^\"\\(\"+\\)?\"$" str)
		     (or (match-string 1 str) ""))
		    (t str)))))
	 (attributes
	  (let ((value (org-element-property attribute element)))
	    (when value
	      (let ((s (mapconcat 'identity value " ")) result)
		(while (string-match
			"\\(?:^\\|[ \t]+\\)\\(:[-a-zA-Z0-9_]+\\)\\([ \t]+\\|$\\)"
			s)
		  (let ((value (substring s 0 (match-beginning 0))))
		    (push (funcall prepare-value value) result))
		  (push (intern (match-string 1 s)) result)
		  (setq s (substring s (match-end 0))))
		;; Ignore any string before first property with `cdr'.
		(cdr (nreverse (cons (funcall prepare-value s) result))))))))
    (if property (plist-get attributes property) attributes)))

(defun org-export-get-caption (element &optional shortp)
  "Return caption from ELEMENT as a secondary string.

When optional argument SHORTP is non-nil, return short caption,
as a secondary string, instead.

Caption lines are separated by a white space."
  (let ((full-caption (org-element-property :caption element)) caption)
    (dolist (line full-caption (cdr caption))
      (let ((cap (funcall (if shortp 'cdr 'car) line)))
	(when cap
	  (setq caption (nconc (list " ") (copy-sequence cap) caption)))))))


;;;; For Derived Back-ends
;;
;; `org-export-with-backend' is a function allowing to locally use
;; another back-end to transcode some object or element.  In a derived
;; back-end, it may be used as a fall-back function once all specific
;; cases have been treated.

(defun org-export-with-backend (backend data &optional contents info)
  "Call a transcoder from BACKEND on DATA.
BACKEND is an export back-end, as returned by, e.g.,
`org-export-create-backend', or a symbol referring to
a registered back-end.  DATA is an Org element, object, secondary
string or string.  CONTENTS, when non-nil, is the transcoded
contents of DATA element, as a string.  INFO, when non-nil, is
the communication channel used for export, as a plist."
  (when (symbolp backend) (setq backend (org-export-get-backend backend)))
  (org-export-barf-if-invalid-backend backend)
  (let ((type (org-element-type data)))
    (when (memq type '(nil org-data)) (error "No foreign transcoder available"))
    (let* ((all-transcoders (org-export-get-all-transcoders backend))
	   (transcoder (cdr (assq type all-transcoders))))
      (unless (functionp transcoder) (error "No foreign transcoder available"))
      (let ((new-info
	     (org-combine-plists
	      info (list
		    :back-end backend
		    :translate-alist all-transcoders
		    :exported-data (make-hash-table :test #'eq :size 401)))))
	;; `:internal-references' are shared across back-ends.
	(prog1 (funcall transcoder data contents new-info)
	  (plist-put info :internal-references
		     (plist-get new-info :internal-references)))))))


;;;; For Export Snippets
;;
;; Every export snippet is transmitted to the back-end.  Though, the
;; latter will only retain one type of export-snippet, ignoring
;; others, based on the former's target back-end.  The function
;; `org-export-snippet-backend' returns that back-end for a given
;; export-snippet.

(defun org-export-snippet-backend (export-snippet)
  "Return EXPORT-SNIPPET targeted back-end as a symbol.
Translation, with `org-export-snippet-translation-alist', is
applied."
  (let ((back-end (org-element-property :back-end export-snippet)))
    (intern
     (or (cdr (assoc back-end org-export-snippet-translation-alist))
	 back-end))))


;;;; For Footnotes
;;
;; `org-export-collect-footnote-definitions' is a tool to list
;; actually used footnotes definitions in the whole parse tree, or in
;; a headline, in order to add footnote listings throughout the
;; transcoded data.
;;
;; `org-export-footnote-first-reference-p' is a predicate used by some
;; back-ends, when they need to attach the footnote definition only to
;; the first occurrence of the corresponding label.
;;
;; `org-export-get-footnote-definition' and
;; `org-export-get-footnote-number' provide easier access to
;; additional information relative to a footnote reference.

(defun org-export-get-footnote-definition (footnote-reference info)
  "Return definition of FOOTNOTE-REFERENCE as parsed data.
INFO is the plist used as a communication channel.  If no such
definition can be found, raise an error."
  (let ((label (org-element-property :label footnote-reference)))
    (if (not label) (org-element-contents footnote-reference)
      (let ((cache (or (plist-get info :footnote-definition-cache)
		       (let ((hash (make-hash-table :test #'equal)))
			 (plist-put info :footnote-definition-cache hash)
			 hash))))
	(or
	 (gethash label cache)
	 (puthash label
		  (org-element-map (plist-get info :parse-tree)
		      '(footnote-definition footnote-reference)
		    (lambda (f)
		      (cond
		       ;; Skip any footnote with a different label.
		       ;; Also skip any standard footnote reference
		       ;; with the same label since those cannot
		       ;; contain a definition.
		       ((not (equal (org-element-property :label f) label)) nil)
		       ((eq (org-element-property :type f) 'standard) nil)
		       ((org-element-contents f))
		       ;; Even if the contents are empty, we can not
		       ;; return nil since that would eventually raise
		       ;; the error.  Instead, return the equivalent
		       ;; empty string.
		       (t "")))
		    info t)
		  cache)
	 (error "Definition not found for footnote %s" label))))))

(defun org-export--footnote-reference-map
    (function data info &optional body-first)
  "Apply FUNCTION on every footnote reference in DATA.
INFO is a plist containing export state.  By default, as soon as
a new footnote reference is encountered, FUNCTION is called onto
its definition.  However, if BODY-FIRST is non-nil, this step is
delayed until the end of the process."
  (letrec ((definitions nil)
	   (seen-refs nil)
	   (search-ref
	    (lambda (data delayp)
	      ;; Search footnote references through DATA, filling
	      ;; SEEN-REFS along the way.  When DELAYP is non-nil,
	      ;; store footnote definitions so they can be entered
	      ;; later.
	      (org-element-map data 'footnote-reference
		(lambda (f)
		  (funcall function f)
		  (let ((--label (org-element-property :label f)))
		    (unless (and --label (member --label seen-refs))
		      (when --label (push --label seen-refs))
		      ;; Search for subsequent references in footnote
		      ;; definition so numbering follows reading
		      ;; logic, unless DELAYP in non-nil.
		      (cond
		       (delayp
			(push (org-export-get-footnote-definition f info)
			      definitions))
		       ;; Do not force entering inline definitions,
		       ;; since `org-element-map' already traverses
		       ;; them at the right time.
		       ((eq (org-element-property :type f) 'inline))
		       (t (funcall search-ref
				   (org-export-get-footnote-definition f info)
				   nil))))))
		info nil
		;; Don't enter footnote definitions since it will
		;; happen when their first reference is found.
		;; Moreover, if DELAYP is non-nil, make sure we
		;; postpone entering definitions of inline references.
		(if delayp '(footnote-definition footnote-reference)
		  'footnote-definition)))))
    (funcall search-ref data body-first)
    (funcall search-ref (nreverse definitions) nil)))

(defun org-export-collect-footnote-definitions (info &optional data body-first)
  "Return an alist between footnote numbers, labels and definitions.

INFO is the current export state, as a plist.

Definitions are collected throughout the whole parse tree, or
DATA when non-nil.

Sorting is done by order of references.  As soon as a new
reference is encountered, other references are searched within
its definition.  However, if BODY-FIRST is non-nil, this step is
delayed after the whole tree is checked.  This alters results
when references are found in footnote definitions.

Definitions either appear as Org data or as a secondary string
for inlined footnotes.  Unreferenced definitions are ignored."
  (let ((n 0) labels alist)
    (org-export--footnote-reference-map
     (lambda (f)
       ;; Collect footnote number, label and definition.
       (let ((l (org-element-property :label f)))
	 (unless (and l (member l labels))
	   (cl-incf n)
	   (push (list n l (org-export-get-footnote-definition f info)) alist))
	 (when l (push l labels))))
     (or data (plist-get info :parse-tree)) info body-first)
    (nreverse alist)))

(defun org-export-footnote-first-reference-p
    (footnote-reference info &optional data body-first)
  "Non-nil when a footnote reference is the first one for its label.

FOOTNOTE-REFERENCE is the footnote reference being considered.
INFO is a plist containing current export state.

Search is done throughout the whole parse tree, or DATA when
non-nil.

By default, as soon as a new footnote reference is encountered,
other references are searched within its definition.  However, if
BODY-FIRST is non-nil, this step is delayed after the whole tree
is checked.  This alters results when references are found in
footnote definitions."
  (let ((label (org-element-property :label footnote-reference)))
    ;; Anonymous footnotes are always a first reference.
    (or (not label)
	(catch 'exit
	  (org-export--footnote-reference-map
	   (lambda (f)
	     (let ((l (org-element-property :label f)))
	       (when (and l label (string= label l))
		 (throw 'exit (eq footnote-reference f)))))
	   (or data (plist-get info :parse-tree)) info body-first)))))

(defun org-export-get-footnote-number (footnote info &optional data body-first)
  "Return number associated to a footnote.

FOOTNOTE is either a footnote reference or a footnote definition.
INFO is the plist containing export state.

Number is unique throughout the whole parse tree, or DATA, when
non-nil.

By default, as soon as a new footnote reference is encountered,
counting process moves into its definition.  However, if
BODY-FIRST is non-nil, this step is delayed until the end of the
process, leading to a different order when footnotes are nested."
  (let ((count 0)
	(seen)
	(label (org-element-property :label footnote)))
    (catch 'exit
      (org-export--footnote-reference-map
       (lambda (f)
	 (let ((l (org-element-property :label f)))
	   (cond
	    ;; Anonymous footnote match: return number.
	    ((and (not l) (not label) (eq footnote f)) (throw 'exit (1+ count)))
	    ;; Labels match: return number.
	    ((and label l (string= label l)) (throw 'exit (1+ count)))
	    ;; Otherwise store label and increase counter if label
	    ;; wasn't encountered yet.
	    ((not l) (cl-incf count))
	    ((not (member l seen)) (push l seen) (cl-incf count)))))
       (or data (plist-get info :parse-tree)) info body-first))))


;;;; For Headlines
;;
;; `org-export-get-relative-level' is a shortcut to get headline
;; level, relatively to the lower headline level in the parsed tree.
;;
;; `org-export-get-headline-number' returns the section number of an
;; headline, while `org-export-number-to-roman' allows it to be
;; converted to roman numbers.  With an optional argument,
;; `org-export-get-headline-number' returns a number to unnumbered
;; headlines (used for internal id).
;;
;; `org-export-low-level-p', `org-export-first-sibling-p' and
;; `org-export-last-sibling-p' are three useful predicates when it
;; comes to fulfill the `:headline-levels' property.
;;
;; `org-export-get-tags', `org-export-get-category' and
;; `org-export-get-node-property' extract useful information from an
;; headline or a parent headline.  They all handle inheritance.
;;
;; `org-export-get-alt-title' tries to retrieve an alternative title,
;; as a secondary string, suitable for table of contents.  It falls
;; back onto default title.

(defun org-export-get-relative-level (headline info)
  "Return HEADLINE relative level within current parsed tree.
INFO is a plist holding contextual information."
  (+ (org-element-property :level headline)
     (or (plist-get info :headline-offset) 0)))

(defun org-export-low-level-p (headline info)
  "Non-nil when HEADLINE is considered as low level.

INFO is a plist used as a communication channel.

A low level headlines has a relative level greater than
`:headline-levels' property value.

Return value is the difference between HEADLINE relative level
and the last level being considered as high enough, or nil."
  (let ((limit (plist-get info :headline-levels)))
    (when (wholenump limit)
      (let ((level (org-export-get-relative-level headline info)))
        (and (> level limit) (- level limit))))))

(defun org-export-get-headline-number (headline info)
  "Return numbered HEADLINE numbering as a list of numbers.
INFO is a plist holding contextual information."
  (and (org-export-numbered-headline-p headline info)
       (cdr (assq headline (plist-get info :headline-numbering)))))

(defun org-export-numbered-headline-p (headline info)
  "Return a non-nil value if HEADLINE element should be numbered.
INFO is a plist used as a communication channel."
  (unless (cl-some
	   (lambda (head) (org-not-nil (org-element-property :UNNUMBERED head)))
	   (org-element-lineage headline nil t))
    (let ((sec-num (plist-get info :section-numbers))
	  (level (org-export-get-relative-level headline info)))
      (if (wholenump sec-num) (<= level sec-num) sec-num))))

(defun org-export-number-to-roman (n)
  "Convert integer N into a roman numeral."
  (let ((roman '((1000 . "M") (900 . "CM") (500 . "D") (400 . "CD")
		 ( 100 . "C") ( 90 . "XC") ( 50 . "L") ( 40 . "XL")
		 (  10 . "X") (  9 . "IX") (  5 . "V") (  4 . "IV")
		 (   1 . "I")))
	(res ""))
    (if (<= n 0)
	(number-to-string n)
      (while roman
	(if (>= n (caar roman))
	    (setq n (- n (caar roman))
		  res (concat res (cdar roman)))
	  (pop roman)))
      res)))

(defun org-export-get-tags (element info &optional tags inherited)
  "Return list of tags associated to ELEMENT.

ELEMENT has either an `headline' or an `inlinetask' type.  INFO
is a plist used as a communication channel.

When non-nil, optional argument TAGS should be a list of strings.
Any tag belonging to this list will also be removed.

When optional argument INHERITED is non-nil, tags can also be
inherited from parent headlines and FILETAGS keywords."
  (cl-remove-if
   (lambda (tag) (member tag tags))
   (if (not inherited) (org-element-property :tags element)
     ;; Build complete list of inherited tags.
     (let ((current-tag-list (org-element-property :tags element)))
       (dolist (parent (org-element-lineage element))
	 (dolist (tag (org-element-property :tags parent))
	   (when (and (memq (org-element-type parent) '(headline inlinetask))
		      (not (member tag current-tag-list)))
	     (push tag current-tag-list))))
       ;; Add FILETAGS keywords and return results.
       (org-uniquify (append (plist-get info :filetags) current-tag-list))))))

(defun org-export-get-node-property (property blob &optional inherited)
  "Return node PROPERTY value for BLOB.

PROPERTY is an upcase symbol (i.e. `:COOKIE_DATA').  BLOB is an
element or object.

If optional argument INHERITED is non-nil, the value can be
inherited from a parent headline.

Return value is a string or nil."
  (let ((headline (if (eq (org-element-type blob) 'headline) blob
		    (org-export-get-parent-headline blob))))
    (if (not inherited) (org-element-property property blob)
      (let ((parent headline))
	(catch 'found
	  (while parent
	    (when (plist-member (nth 1 parent) property)
	      (throw 'found (org-element-property property parent)))
	    (setq parent (org-element-property :parent parent))))))))

(defun org-export-get-category (blob info)
  "Return category for element or object BLOB.

INFO is a plist used as a communication channel.

CATEGORY is automatically inherited from a parent headline, from
#+CATEGORY: keyword or created out of original file name.  If all
fail, the fall-back value is \"???\"."
  (or (org-export-get-node-property :CATEGORY blob t)
      (org-element-map (plist-get info :parse-tree) 'keyword
	(lambda (kwd)
	  (when (equal (org-element-property :key kwd) "CATEGORY")
	    (org-element-property :value kwd)))
	info 'first-match)
      (let ((file (plist-get info :input-file)))
	(and file (file-name-sans-extension (file-name-nondirectory file))))
      "???"))

(defun org-export-get-alt-title (headline _)
  "Return alternative title for HEADLINE, as a secondary string.
If no optional title is defined, fall-back to the regular title."
  (let ((alt (org-element-property :ALT_TITLE headline)))
    (if alt (org-element-parse-secondary-string
	     alt (org-element-restriction 'headline) headline)
      (org-element-property :title headline))))

(defun org-export-first-sibling-p (blob info)
  "Non-nil when BLOB is the first sibling in its parent.
BLOB is an element or an object.  If BLOB is a headline, non-nil
means it is the first sibling in the sub-tree.  INFO is a plist
used as a communication channel."
  (memq (org-element-type (org-export-get-previous-element blob info))
	'(nil section)))

(defun org-export-last-sibling-p (blob info)
  "Non-nil when BLOB is the last sibling in its parent.
BLOB is an element or an object.  INFO is a plist used as
a communication channel."
  (not (org-export-get-next-element blob info)))


;;;; For Keywords
;;
;; `org-export-get-date' returns a date appropriate for the document
;;  to about to be exported.  In particular, it takes care of
;;  `org-export-date-timestamp-format'.

(defun org-export-get-date (info &optional fmt)
  "Return date value for the current document.

INFO is a plist used as a communication channel.  FMT, when
non-nil, is a time format string that will be applied on the date
if it consists in a single timestamp object.  It defaults to
`org-export-date-timestamp-format' when nil.

A proper date can be a secondary string, a string or nil.  It is
meant to be translated with `org-export-data' or alike."
  (let ((date (plist-get info :date))
	(fmt (or fmt org-export-date-timestamp-format)))
    (cond ((not date) nil)
	  ((and fmt
		(not (cdr date))
		(eq (org-element-type (car date)) 'timestamp))
	   (org-timestamp-format (car date) fmt))
	  (t date))))


;;;; For Links
;;
;; `org-export-custom-protocol-maybe' handles custom protocol defined
;; in `org-link-parameters'.
;;
;; `org-export-get-coderef-format' returns an appropriate format
;; string for coderefs.
;;
;; `org-export-inline-image-p' returns a non-nil value when the link
;; provided should be considered as an inline image.
;;
;; `org-export-resolve-fuzzy-link' searches destination of fuzzy links
;; (i.e. links with "fuzzy" as type) within the parsed tree, and
;; returns an appropriate unique identifier.
;;
;; `org-export-resolve-id-link' returns the first headline with
;; specified id or custom-id in parse tree, the path to the external
;; file with the id.
;;
;; `org-export-resolve-coderef' associates a reference to a line
;; number in the element it belongs, or returns the reference itself
;; when the element isn't numbered.
;;
;; `org-export-file-uri' expands a filename as stored in :path value
;;  of a "file" link into a file URI.
;;
;; Broken links raise a `org-link-broken' error, which is caught by
;; `org-export-data' for further processing, depending on
;; `org-export-with-broken-links' value.

(org-define-error 'org-link-broken "Unable to resolve link; aborting")

(defun org-export-custom-protocol-maybe (link desc backend)
  "Try exporting LINK with a dedicated function.

DESC is its description, as a string, or nil.  BACKEND is the
back-end used for export, as a symbol.

Return output as a string, or nil if no protocol handles LINK.

A custom protocol has precedence over regular back-end export.
The function ignores links with an implicit type (e.g.,
\"custom-id\")."
  (let ((type (org-element-property :type link)))
    (unless (or (member type '("coderef" "custom-id" "fuzzy" "radio"))
		(not backend))
      (let ((protocol (org-link-get-parameter type :export)))
	(and (functionp protocol)
	     (funcall protocol
		      (org-link-unescape (org-element-property :path link))
		      desc
		      backend))))))

(defun org-export-get-coderef-format (path desc)
  "Return format string for code reference link.
PATH is the link path.  DESC is its description."
  (save-match-data
    (cond ((not desc) "%s")
	  ((string-match (regexp-quote (concat "(" path ")")) desc)
	   (replace-match "%s" t t desc))
	  (t desc))))

(defun org-export-inline-image-p (link &optional rules)
  "Non-nil if LINK object points to an inline image.

Optional argument is a set of RULES defining inline images.  It
is an alist where associations have the following shape:

  (TYPE . REGEXP)

Applying a rule means apply REGEXP against LINK's path when its
type is TYPE.  The function will return a non-nil value if any of
the provided rules is non-nil.  The default rule is
`org-export-default-inline-image-rule'.

This only applies to links without a description."
  (and (not (org-element-contents link))
       (let ((case-fold-search t))
	 (cl-some (lambda (rule)
		    (and (string= (org-element-property :type link) (car rule))
			 (string-match-p (cdr rule)
					 (org-element-property :path link))))
		  (or rules org-export-default-inline-image-rule)))))

(defun org-export-insert-image-links (data info &optional rules)
  "Insert image links in DATA.

Org syntax does not support nested links.  Nevertheless, some
export back-ends support images as descriptions of links.  Since
images are really links to image files, we need to make an
exception about links nesting.

This function recognizes links whose contents are really images
and turn them into proper nested links.  It is meant to be used
as a parse tree filter in back-ends supporting such constructs.

DATA is a parse tree.  INFO is the current state of the export
process, as a plist.

A description is a valid images if it matches any rule in RULES,
if non-nil, or `org-export-default-inline-image-rule' otherwise.
See `org-export-inline-image-p' for more information about the
structure of RULES.

Return modified DATA."
  (let ((link-re (format "\\`\\(?:%s\\|%s\\)\\'"
			 org-plain-link-re
			 org-angle-link-re))
	(case-fold-search t))
    (org-element-map data 'link
      (lambda (l)
	(let ((contents (org-element-interpret-data (org-element-contents l))))
	  (when (and (org-string-nw-p contents)
		     (string-match link-re contents))
	    (let ((type (match-string 1 contents))
		  (path (match-string 2 contents)))
	      (when (cl-some (lambda (rule)
			       (and (string= type (car rule))
				    (string-match-p (cdr rule) path)))
			     (or rules org-export-default-inline-image-rule))
		;; Replace contents with image link.
		(org-element-adopt-elements
		 (org-element-set-contents l nil)
		 (with-temp-buffer
		   (save-excursion (insert contents))
		   (org-element-link-parser))))))))
      info nil nil t))
  data)

(defun org-export-resolve-coderef (ref info)
  "Resolve a code reference REF.

INFO is a plist used as a communication channel.

Return associated line number in source code, or REF itself,
depending on src-block or example element's switches.  Throw an
error if no block contains REF."
  (or (org-element-map (plist-get info :parse-tree) '(example-block src-block)
	(lambda (el)
	  (with-temp-buffer
	    (insert (org-trim (org-element-property :value el)))
	    (let* ((label-fmt (or (org-element-property :label-fmt el)
				  org-coderef-label-format))
		   (ref-re (org-src-coderef-regexp label-fmt ref)))
	      ;; Element containing REF is found.  Resolve it to
	      ;; either a label or a line number, as needed.
	      (when (re-search-backward ref-re nil t)
		(if (org-element-property :use-labels el) ref
		  (+ (or (org-export-get-loc el info) 0)
		     (line-number-at-pos)))))))
	info 'first-match)
      (signal 'org-link-broken (list ref))))

(defun org-export-search-cells (datum)
  "List search cells for element or object DATUM.

A search cell follows the pattern (TYPE . SEARCH) where

  TYPE is a symbol among `headline', `custom-id', `target' and
  `other'.

  SEARCH is the string a link is expected to match.  More
  accurately, it is

    - headline's title, as a list of strings, if TYPE is
      `headline'.

    - CUSTOM_ID value, as a string, if TYPE is `custom-id'.

    - target's or radio-target's name as a list of strings if
      TYPE is `target'.

    - NAME affiliated keyword is TYPE is `other'.

A search cell is the internal representation of a fuzzy link.  It
ignores white spaces and statistics cookies, if applicable."
  (pcase (org-element-type datum)
    (`headline
     (let ((title (split-string
		   (replace-regexp-in-string
		    "\\[[0-9]*\\(?:%\\|/[0-9]*\\)\\]" ""
		    (org-element-property :raw-value datum)))))
       (delq nil
	     (list
	      (cons 'headline title)
	      (cons 'other title)
	      (let ((custom-id (org-element-property :custom-id datum)))
		(and custom-id (cons 'custom-id custom-id)))))))
    (`target
     (list (cons 'target (split-string (org-element-property :value datum)))))
    ((and (let name (org-element-property :name datum))
	  (guard name))
     (list (cons 'other (split-string name))))
    (_ nil)))

(defun org-export-string-to-search-cell (s)
  "Return search cells associated to string S.
S is either the path of a fuzzy link or a search option, i.e., it
tries to match either a headline (through custom ID or title),
a target or a named element."
  (pcase (string-to-char s)
    (?* (list (cons 'headline (split-string (substring s 1)))))
    (?# (list (cons 'custom-id (substring s 1))))
    ((let search (split-string s))
     (list (cons 'target search) (cons 'other search)))))

(defun org-export-match-search-cell-p (datum cells)
  "Non-nil when DATUM matches search cells CELLS.
DATUM is an element or object.  CELLS is a list of search cells,
as returned by `org-export-search-cells'."
  (let ((targets (org-export-search-cells datum)))
    (and targets (cl-some (lambda (cell) (member cell targets)) cells))))

(defun org-export-resolve-fuzzy-link (link info)
  "Return LINK destination.

INFO is a plist holding contextual information.

Return value can be an object or an element:

- If LINK path matches a target object (i.e. <<path>>) return it.

- If LINK path exactly matches the name affiliated keyword
  (i.e. #+NAME: path) of an element, return that element.

- If LINK path exactly matches any headline name, return that
  element.

- Otherwise, throw an error.

Assume LINK type is \"fuzzy\".  White spaces are not
significant."
  (let* ((search-cells (org-export-string-to-search-cell
			(org-link-unescape (org-element-property :path link))))
	 (link-cache (or (plist-get info :resolve-fuzzy-link-cache)
			 (let ((table (make-hash-table :test #'eq)))
			   (plist-put info :resolve-fuzzy-link-cache table)
			   table)))
	 (cached (gethash search-cells link-cache 'not-found)))
    (if (not (eq cached 'not-found)) cached
      (let ((matches
	     (org-element-map (plist-get info :parse-tree)
		 (cons 'target org-element-all-elements)
	       (lambda (datum)
		 (and (org-export-match-search-cell-p datum search-cells)
		      datum)))))
	(unless matches
	  (signal 'org-link-broken (list (org-element-property :path link))))
	(puthash
	 search-cells
	 ;; There can be multiple matches for un-typed searches, i.e.,
	 ;; for searches not starting with # or *.  In this case,
	 ;; prioritize targets and names over headline titles.
	 ;; Matching both a name and a target is not valid, and
	 ;; therefore undefined.
	 (or (cl-some (lambda (datum)
			(and (not (eq (org-element-type datum) 'headline))
			     datum))
		      matches)
	     (car matches))
	 link-cache)))))

(defun org-export-resolve-id-link (link info)
  "Return headline referenced as LINK destination.

INFO is a plist used as a communication channel.

Return value can be the headline element matched in current parse
tree or a file name.  Assume LINK type is either \"id\" or
\"custom-id\".  Throw an error if no match is found."
  (let ((id (org-element-property :path link)))
    ;; First check if id is within the current parse tree.
    (or (org-element-map (plist-get info :parse-tree) 'headline
	  (lambda (headline)
	    (when (or (equal (org-element-property :ID headline) id)
		      (equal (org-element-property :CUSTOM_ID headline) id))
	      headline))
	  info 'first-match)
	;; Otherwise, look for external files.
	(cdr (assoc id (plist-get info :id-alist)))
	(signal 'org-link-broken (list id)))))

(defun org-export-resolve-radio-link (link info)
  "Return radio-target object referenced as LINK destination.

INFO is a plist used as a communication channel.

Return value can be a radio-target object or nil.  Assume LINK
has type \"radio\"."
  (let ((path (replace-regexp-in-string
	       "[ \r\t\n]+" " " (org-element-property :path link))))
    (org-element-map (plist-get info :parse-tree) 'radio-target
      (lambda (radio)
	(and (eq (compare-strings
		  (replace-regexp-in-string
		   "[ \r\t\n]+" " " (org-element-property :value radio))
		  nil nil path nil nil t)
		 t)
	     radio))
      info 'first-match)))

(defun org-export-file-uri (filename)
  "Return file URI associated to FILENAME."
  (cond ((string-prefix-p "//" filename) (concat "file:" filename))
	((not (file-name-absolute-p filename)) filename)
	((org-file-remote-p filename) (concat "file:/" filename))
	(t
	 (let ((fullname (expand-file-name filename)))
	   (concat (if (string-prefix-p "/" fullname) "file://" "file:///")
		   fullname)))))

;;;; For References
;;
;; `org-export-get-reference' associate a unique reference for any
;; object or element.  It uses `org-export-new-reference' and
;; `org-export-format-reference' to, respectively, generate new
;; internal references and turn them into a string suitable for
;; output.
;;
;; `org-export-get-ordinal' associates a sequence number to any object
;; or element.

(defun org-export-new-reference (references)
  "Return a unique reference, among REFERENCES.
REFERENCES is an alist whose values are in-use references, as
numbers.  Returns a number, which is the internal representation
of a reference.  See also `org-export-format-reference'."
  ;; Generate random 7 digits hexadecimal numbers.  Collisions
  ;; increase exponentially with the numbers of references.  However,
  ;; the odds for encountering at least one collision with 1000 active
  ;; references in the same document are roughly 0.2%, so this
  ;; shouldn't be the bottleneck.
  (let ((new (random #x10000000)))
    (while (rassq new references) (setq new (random #x10000000)))
    new))

(defun org-export-format-reference (reference)
  "Format REFERENCE into a string.
REFERENCE is a number representing a reference, as returned by
`org-export-new-reference', which see."
  (format "org%07x" reference))

(defun org-export-get-reference (datum info)
  "Return a unique reference for DATUM, as a string.

DATUM is either an element or an object.  INFO is the current
export state, as a plist.

This function checks `:crossrefs' property in INFO for search
cells matching DATUM before creating a new reference.  Returned
reference consists of alphanumeric characters only."
  (let ((cache (plist-get info :internal-references)))
    (or (car (rassq datum cache))
	(let* ((crossrefs (plist-get info :crossrefs))
	       (cells (org-export-search-cells datum))
	       ;; Preserve any pre-existing association between
	       ;; a search cell and a reference, i.e., when some
	       ;; previously published document referenced a location
	       ;; within current file (see
	       ;; `org-publish-resolve-external-link').
	       ;;
	       ;; However, there is no guarantee that search cells are
	       ;; unique, e.g., there might be duplicate custom ID or
	       ;; two headings with the same title in the file.
	       ;;
	       ;; As a consequence, before re-using any reference to
	       ;; an element or object, we check that it doesn't refer
	       ;; to a previous element or object.
	       (new (or (cl-some
			 (lambda (cell)
			   (let ((stored (cdr (assoc cell crossrefs))))
			     (when stored
			       (let ((old (org-export-format-reference stored)))
				 (and (not (assoc old cache)) stored)))))
			 cells)
			(org-export-new-reference cache)))
	       (reference-string (org-export-format-reference new)))
	  ;; Cache contains both data already associated to
	  ;; a reference and in-use internal references, so as to make
	  ;; unique references.
	  (dolist (cell cells) (push (cons cell new) cache))
	  ;; Retain a direct association between reference string and
	  ;; DATUM since (1) not every object or element can be given
	  ;; a search cell (2) it permits quick lookup.
	  (push (cons reference-string datum) cache)
	  (plist-put info :internal-references cache)
	  reference-string))))

(defun org-export-get-ordinal (element info &optional types predicate)
  "Return ordinal number of an element or object.

ELEMENT is the element or object considered.  INFO is the plist
used as a communication channel.

Optional argument TYPES, when non-nil, is a list of element or
object types, as symbols, that should also be counted in.
Otherwise, only provided element's type is considered.

Optional argument PREDICATE is a function returning a non-nil
value if the current element or object should be counted in.  It
accepts two arguments: the element or object being considered and
the plist used as a communication channel.  This allows counting
only a certain type of object (i.e. inline images).

Return value is a list of numbers if ELEMENT is a headline or an
item.  It is nil for keywords.  It represents the footnote number
for footnote definitions and footnote references.  If ELEMENT is
a target, return the same value as if ELEMENT was the closest
table, item or headline containing the target.  In any other
case, return the sequence number of ELEMENT among elements or
objects of the same type."
  ;; Ordinal of a target object refer to the ordinal of the closest
  ;; table, item, or headline containing the object.
  (when (eq (org-element-type element) 'target)
    (setq element
	  (org-element-lineage
	   element
	   '(footnote-definition footnote-reference headline item table))))
  (cl-case (org-element-type element)
    ;; Special case 1: A headline returns its number as a list.
    (headline (org-export-get-headline-number element info))
    ;; Special case 2: An item returns its number as a list.
    (item (let ((struct (org-element-property :structure element)))
	    (org-list-get-item-number
	     (org-element-property :begin element)
	     struct
	     (org-list-prevs-alist struct)
	     (org-list-parents-alist struct))))
    ((footnote-definition footnote-reference)
     (org-export-get-footnote-number element info))
    (otherwise
     (let ((counter 0))
       ;; Increment counter until ELEMENT is found again.
       (org-element-map (plist-get info :parse-tree)
	   (or types (org-element-type element))
	 (lambda (el)
	   (cond
	    ((eq element el) (1+ counter))
	    ((not predicate) (cl-incf counter) nil)
	    ((funcall predicate el info) (cl-incf counter) nil)))
	 info 'first-match)))))


;;;; For Src-Blocks
;;
;; `org-export-get-loc' counts number of code lines accumulated in
;; src-block or example-block elements with a "+n" switch until
;; a given element, excluded.  Note: "-n" switches reset that count.
;;
;; `org-export-unravel-code' extracts source code (along with a code
;; references alist) from an `element-block' or `src-block' type
;; element.
;;
;; `org-export-format-code' applies a formatting function to each line
;; of code, providing relative line number and code reference when
;; appropriate.  Since it doesn't access the original element from
;; which the source code is coming, it expects from the code calling
;; it to know if lines should be numbered and if code references
;; should appear.
;;
;; Eventually, `org-export-format-code-default' is a higher-level
;; function (it makes use of the two previous functions) which handles
;; line numbering and code references inclusion, and returns source
;; code in a format suitable for plain text or verbatim output.

(defun org-export-get-loc (element info)
  "Return count of lines of code before ELEMENT.

ELEMENT is an example-block or src-block element.  INFO is the
plist used as a communication channel.

Count includes every line of code in example-block or src-block
with a \"+n\" or \"-n\" switch before block.  Return nil if
ELEMENT doesn't allow line numbering."
  (pcase (org-element-property :number-lines element)
    (`(new . ,n) n)
    (`(continued . ,n)
     (let ((loc 0))
       (org-element-map (plist-get info :parse-tree) '(src-block example-block)
	 (lambda (el)
	   ;; ELEMENT is reached: Quit loop and return locs.
	   (if (eq el element) (+ loc n)
	     ;; Only count lines from src-block and example-block
	     ;; elements with a "+n" or "-n" switch.
	     (let ((linum (org-element-property :number-lines el)))
	       (when linum
		 (let ((lines (org-count-lines
			       (org-element-property :value el))))
		   ;; Accumulate locs or reset them.
		   (pcase linum
		     (`(new . ,n) (setq loc (+ n lines)))
		     (`(continued . ,n) (cl-incf loc (+ n lines)))))))
	     nil))		      ;Return nil to stay in the loop.
	 info 'first-match)))))

(defun org-export-unravel-code (element)
  "Clean source code and extract references out of it.

ELEMENT has either a `src-block' an `example-block' type.

Return a cons cell whose CAR is the source code, cleaned from any
reference, protective commas and spurious indentation, and CDR is
an alist between relative line number (integer) and name of code
reference on that line (string)."
  (let* ((line 0) refs
	 (value (org-element-property :value element))
	 ;; Remove global indentation from code, if necessary.  Also
	 ;; remove final newline character, since it doesn't belongs
	 ;; to the code proper.
	 (code (replace-regexp-in-string
		"\n\\'" ""
		(if (or org-src-preserve-indentation
			(org-element-property :preserve-indent element))
		    value
		  (org-remove-indentation value))))
	 ;; Build a regexp matching a loc with a reference.
	 (ref-re (org-src-coderef-regexp (org-src-coderef-format element))))
    ;; Return value.
    (cons
     ;; Code with references removed.
     (mapconcat
      (lambda (loc)
	(cl-incf line)
	(if (not (string-match ref-re loc)) loc
	  ;; Ref line: remove ref, and add its position in REFS.
	  (push (cons line (match-string 3 loc)) refs)
	  (replace-match "" nil nil loc 1)))
      (split-string code "\n") "\n")
     ;; Reference alist.
     refs)))

(defun org-export-format-code (code fun &optional num-lines ref-alist)
  "Format CODE by applying FUN line-wise and return it.

CODE is a string representing the code to format.  FUN is
a function.  It must accept three arguments: a line of
code (string), the current line number (integer) or nil and the
reference associated to the current line (string) or nil.

Optional argument NUM-LINES can be an integer representing the
number of code lines accumulated until the current code.  Line
numbers passed to FUN will take it into account.  If it is nil,
FUN's second argument will always be nil.  This number can be
obtained with `org-export-get-loc' function.

Optional argument REF-ALIST can be an alist between relative line
number (i.e. ignoring NUM-LINES) and the name of the code
reference on it.  If it is nil, FUN's third argument will always
be nil.  It can be obtained through the use of
`org-export-unravel-code' function."
  (let ((--locs (split-string code "\n"))
	(--line 0))
    (concat
     (mapconcat
      (lambda (--loc)
	(cl-incf --line)
	(let ((--ref (cdr (assq --line ref-alist))))
	  (funcall fun --loc (and num-lines (+ num-lines --line)) --ref)))
      --locs "\n")
     "\n")))

(defun org-export-format-code-default (element info)
  "Return source code from ELEMENT, formatted in a standard way.

ELEMENT is either a `src-block' or `example-block' element.  INFO
is a plist used as a communication channel.

This function takes care of line numbering and code references
inclusion.  Line numbers, when applicable, appear at the
beginning of the line, separated from the code by two white
spaces.  Code references, on the other hand, appear flushed to
the right, separated by six white spaces from the widest line of
code."
  ;; Extract code and references.
  (let* ((code-info (org-export-unravel-code element))
         (code (car code-info))
	 (code-lines (split-string code "\n")))
    (if (null code-lines) ""
      (let* ((refs (and (org-element-property :retain-labels element)
			(cdr code-info)))
	     ;; Handle line numbering.
	     (num-start (org-export-get-loc element info))
	     (num-fmt
	      (and num-start
		   (format "%%%ds  "
			   (length (number-to-string
				    (+ (length code-lines) num-start))))))
	     ;; Prepare references display, if required.  Any reference
	     ;; should start six columns after the widest line of code,
	     ;; wrapped with parenthesis.
	     (max-width
	      (+ (apply 'max (mapcar 'length code-lines))
		 (if (not num-start) 0 (length (format num-fmt num-start))))))
	(org-export-format-code
	 code
	 (lambda (loc line-num ref)
	   (let ((number-str (and num-fmt (format num-fmt line-num))))
	     (concat
	      number-str
	      loc
	      (and ref
		   (concat (make-string (- (+ 6 max-width)
					   (+ (length loc) (length number-str)))
					?\s)
			   (format "(%s)" ref))))))
	 num-start refs)))))


;;;; For Tables
;;
;; `org-export-table-has-special-column-p' and and
;; `org-export-table-row-is-special-p' are predicates used to look for
;; meta-information about the table structure.
;;
;; `org-table-has-header-p' tells when the rows before the first rule
;;  should be considered as table's header.
;;
;; `org-export-table-cell-width', `org-export-table-cell-alignment'
;; and `org-export-table-cell-borders' extract information from
;; a table-cell element.
;;
;; `org-export-table-dimensions' gives the number on rows and columns
;; in the table, ignoring horizontal rules and special columns.
;; `org-export-table-cell-address', given a table-cell object, returns
;; the absolute address of a cell. On the other hand,
;; `org-export-get-table-cell-at' does the contrary.
;;
;; `org-export-table-cell-starts-colgroup-p',
;; `org-export-table-cell-ends-colgroup-p',
;; `org-export-table-row-starts-rowgroup-p',
;; `org-export-table-row-ends-rowgroup-p',
;; `org-export-table-row-starts-header-p',
;; `org-export-table-row-ends-header-p' and
;; `org-export-table-row-in-header-p' indicate position of current row
;; or cell within the table.

(defun org-export-table-has-special-column-p (table)
  "Non-nil when TABLE has a special column.
All special columns will be ignored during export."
  ;; The table has a special column when every first cell of every row
  ;; has an empty value or contains a symbol among "/", "#", "!", "$",
  ;; "*" "_" and "^".  Though, do not consider a first column
  ;; containing only empty cells as special.
  (let ((special-column? 'empty))
    (catch 'exit
      (dolist (row (org-element-contents table))
	(when (eq (org-element-property :type row) 'standard)
	  (let ((value (org-element-contents
			(car (org-element-contents row)))))
	    (cond ((member value
			   '(("/") ("#") ("!") ("$") ("*") ("_") ("^")))
		   (setq special-column? 'special))
		  ((null value))
		  (t (throw 'exit nil))))))
      (eq special-column? 'special))))

(defun org-export-table-has-header-p (table info)
  "Non-nil when TABLE has a header.

INFO is a plist used as a communication channel.

A table has a header when it contains at least two row groups."
  (let* ((cache (or (plist-get info :table-header-cache)
		    (let ((table (make-hash-table :test #'eq)))
		      (plist-put info :table-header-cache table)
		      table)))
	 (cached (gethash table cache 'no-cache)))
    (if (not (eq cached 'no-cache)) cached
      (let ((rowgroup 1) row-flag)
	(puthash table
		 (org-element-map table 'table-row
		   (lambda (row)
		     (cond
		      ((> rowgroup 1) t)
		      ((and row-flag
			    (eq (org-element-property :type row) 'rule))
		       (cl-incf rowgroup)
		       (setq row-flag nil))
		      ((and (not row-flag)
			    (eq (org-element-property :type row) 'standard))
		       (setq row-flag t)
		       nil)))
		   info 'first-match)
		 cache)))))

(defun org-export-table-row-is-special-p (table-row _)
  "Non-nil if TABLE-ROW is considered special.
All special rows will be ignored during export."
  (when (eq (org-element-property :type table-row) 'standard)
    (let ((first-cell (org-element-contents
		       (car (org-element-contents table-row)))))
      ;; A row is special either when...
      (or
       ;; ... it starts with a field only containing "/",
       (equal first-cell '("/"))
       ;; ... the table contains a special column and the row start
       ;; with a marking character among, "^", "_", "$" or "!",
       (and (org-export-table-has-special-column-p
	     (org-export-get-parent table-row))
	    (member first-cell '(("^") ("_") ("$") ("!"))))
       ;; ... it contains only alignment cookies and empty cells.
       (let ((special-row-p 'empty))
	 (catch 'exit
	   (dolist (cell (org-element-contents table-row))
	     (let ((value (org-element-contents cell)))
	       ;; Since VALUE is a secondary string, the following
	       ;; checks avoid expanding it with `org-export-data'.
	       (cond ((not value))
		     ((and (not (cdr value))
			   (stringp (car value))
			   (string-match "\\`<[lrc]?\\([0-9]+\\)?>\\'"
					 (car value)))
		      (setq special-row-p 'cookie))
		     (t (throw 'exit nil)))))
	   (eq special-row-p 'cookie)))))))

(defun org-export-table-row-group (table-row info)
  "Return TABLE-ROW's group number, as an integer.

INFO is a plist used as the communication channel.

Return value is the group number, as an integer, or nil for
special rows and rows separators.  First group is also table's
header."
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((cache (or (plist-get info :table-row-group-cache)
		      (let ((table (make-hash-table :test #'eq)))
			(plist-put info :table-row-group-cache table)
			table)))
	   (cached (gethash table-row cache 'no-cache)))
      (if (not (eq cached 'no-cache)) cached
	;; First time a row is queried, populate cache with all the
	;; rows from the table.
	(let ((group 0) row-flag)
	  (org-element-map (org-export-get-parent table-row) 'table-row
	    (lambda (row)
	      (if (eq (org-element-property :type row) 'rule)
		  (setq row-flag nil)
		(unless row-flag (cl-incf group) (setq row-flag t))
		(puthash row group cache)))
	    info))
	(gethash table-row cache)))))

(defun org-export-table-cell-width (table-cell info)
  "Return TABLE-CELL contents width.

INFO is a plist used as the communication channel.

Return value is the width given by the last width cookie in the
same column as TABLE-CELL, or nil."
  (let* ((row (org-export-get-parent table-cell))
	 (table (org-export-get-parent row))
	 (cells (org-element-contents row))
	 (columns (length cells))
	 (column (- columns (length (memq table-cell cells))))
	 (cache (or (plist-get info :table-cell-width-cache)
		    (let ((table (make-hash-table :test #'eq)))
		      (plist-put info :table-cell-width-cache table)
		      table)))
	 (width-vector (or (gethash table cache)
			   (puthash table (make-vector columns 'empty) cache)))
	 (value (aref width-vector column)))
    (if (not (eq value 'empty)) value
      (let (cookie-width)
	(dolist (row (org-element-contents table)
		     (aset width-vector column cookie-width))
	  (when (org-export-table-row-is-special-p row info)
	    ;; In a special row, try to find a width cookie at COLUMN.
	    (let* ((value (org-element-contents
			   (elt (org-element-contents row) column)))
		   (cookie (car value)))
	      ;; The following checks avoid expanding unnecessarily
	      ;; the cell with `org-export-data'.
	      (when (and value
			 (not (cdr value))
			 (stringp cookie)
			 (string-match "\\`<[lrc]?\\([0-9]+\\)?>\\'" cookie)
			 (match-string 1 cookie))
		(setq cookie-width
		      (string-to-number (match-string 1 cookie)))))))))))

(defun org-export-table-cell-alignment (table-cell info)
  "Return TABLE-CELL contents alignment.

INFO is a plist used as the communication channel.

Return alignment as specified by the last alignment cookie in the
same column as TABLE-CELL.  If no such cookie is found, a default
alignment value will be deduced from fraction of numbers in the
column (see `org-table-number-fraction' for more information).
Possible values are `left', `right' and `center'."
  ;; Load `org-table-number-fraction' and `org-table-number-regexp'.
  (require 'org-table)
  (let* ((row (org-export-get-parent table-cell))
	 (table (org-export-get-parent row))
	 (cells (org-element-contents row))
	 (columns (length cells))
	 (column (- columns (length (memq table-cell cells))))
	 (cache (or (plist-get info :table-cell-alignment-cache)
		    (let ((table (make-hash-table :test #'eq)))
		      (plist-put info :table-cell-alignment-cache table)
		      table)))
	 (align-vector (or (gethash table cache)
			   (puthash table (make-vector columns nil) cache))))
    (or (aref align-vector column)
	(let ((number-cells 0)
	      (total-cells 0)
	      cookie-align
	      previous-cell-number-p)
	  (dolist (row (org-element-contents (org-export-get-parent row)))
	    (cond
	     ;; In a special row, try to find an alignment cookie at
	     ;; COLUMN.
	     ((org-export-table-row-is-special-p row info)
	      (let ((value (org-element-contents
			    (elt (org-element-contents row) column))))
		;; Since VALUE is a secondary string, the following
		;; checks avoid useless expansion through
		;; `org-export-data'.
		(when (and value
			   (not (cdr value))
			   (stringp (car value))
			   (string-match "\\`<\\([lrc]\\)?\\([0-9]+\\)?>\\'"
					 (car value))
			   (match-string 1 (car value)))
		  (setq cookie-align (match-string 1 (car value))))))
	     ;; Ignore table rules.
	     ((eq (org-element-property :type row) 'rule))
	     ;; In a standard row, check if cell's contents are
	     ;; expressing some kind of number.  Increase NUMBER-CELLS
	     ;; accordingly.  Though, don't bother if an alignment
	     ;; cookie has already defined cell's alignment.
	     ((not cookie-align)
	      (let ((value (org-export-data
			    (org-element-contents
			     (elt (org-element-contents row) column))
			    info)))
		(cl-incf total-cells)
		;; Treat an empty cell as a number if it follows
		;; a number.
		(if (not (or (string-match org-table-number-regexp value)
			     (and (string= value "") previous-cell-number-p)))
		    (setq previous-cell-number-p nil)
		  (setq previous-cell-number-p t)
		  (cl-incf number-cells))))))
	  ;; Return value.  Alignment specified by cookies has
	  ;; precedence over alignment deduced from cell's contents.
	  (aset align-vector
		column
		(cond ((equal cookie-align "l") 'left)
		      ((equal cookie-align "r") 'right)
		      ((equal cookie-align "c") 'center)
		      ((>= (/ (float number-cells) total-cells)
			   org-table-number-fraction)
		       'right)
		      (t 'left)))))))

(defun org-export-table-cell-borders (table-cell info)
  "Return TABLE-CELL borders.

INFO is a plist used as a communication channel.

Return value is a list of symbols, or nil.  Possible values are:
`top', `bottom', `above', `below', `left' and `right'.  Note:
`top' (resp. `bottom') only happen for a cell in the first
row (resp. last row) of the table, ignoring table rules, if any.

Returned borders ignore special rows."
  (let* ((row (org-export-get-parent table-cell))
	 (table (org-export-get-parent-table table-cell))
	 borders)
    ;; Top/above border?  TABLE-CELL has a border above when a rule
    ;; used to demarcate row groups can be found above.  Hence,
    ;; finding a rule isn't sufficient to push `above' in BORDERS:
    ;; another regular row has to be found above that rule.
    (let (rule-flag)
      (catch 'exit
	;; Look at every row before the current one.
	(dolist (row (cdr (memq row (reverse (org-element-contents table)))))
	  (cond ((eq (org-element-property :type row) 'rule)
		 (setq rule-flag t))
		((not (org-export-table-row-is-special-p row info))
		 (if rule-flag (throw 'exit (push 'above borders))
		   (throw 'exit nil)))))
	;; No rule above, or rule found starts the table (ignoring any
	;; special row): TABLE-CELL is at the top of the table.
	(when rule-flag (push 'above borders))
	(push 'top borders)))
    ;; Bottom/below border? TABLE-CELL has a border below when next
    ;; non-regular row below is a rule.
    (let (rule-flag)
      (catch 'exit
	;; Look at every row after the current one.
	(dolist (row (cdr (memq row (org-element-contents table))))
	  (cond ((eq (org-element-property :type row) 'rule)
		 (setq rule-flag t))
		((not (org-export-table-row-is-special-p row info))
		 (if rule-flag (throw 'exit (push 'below borders))
		   (throw 'exit nil)))))
	;; No rule below, or rule found ends the table (modulo some
	;; special row): TABLE-CELL is at the bottom of the table.
	(when rule-flag (push 'below borders))
	(push 'bottom borders)))
    ;; Right/left borders?  They can only be specified by column
    ;; groups.  Column groups are defined in a row starting with "/".
    ;; Also a column groups row only contains "<", "<>", ">" or blank
    ;; cells.
    (catch 'exit
      (let ((column (let ((cells (org-element-contents row)))
		      (- (length cells) (length (memq table-cell cells))))))
	;; Table rows are read in reverse order so last column groups
	;; row has precedence over any previous one.
	(dolist (row (reverse (org-element-contents table)))
	  (unless (eq (org-element-property :type row) 'rule)
	    (when (equal (org-element-contents
			  (car (org-element-contents row)))
			 '("/"))
	      (let ((column-groups
		     (mapcar
		      (lambda (cell)
			(let ((value (org-element-contents cell)))
			  (when (member value '(("<") ("<>") (">") nil))
			    (car value))))
		      (org-element-contents row))))
		;; There's a left border when previous cell, if
		;; any, ends a group, or current one starts one.
		(when (or (and (not (zerop column))
			       (member (elt column-groups (1- column))
				       '(">" "<>")))
			  (member (elt column-groups column) '("<" "<>")))
		  (push 'left borders))
		;; There's a right border when next cell, if any,
		;; starts a group, or current one ends one.
		(when (or (and (/= (1+ column) (length column-groups))
			       (member (elt column-groups (1+ column))
				       '("<" "<>")))
			  (member (elt column-groups column) '(">" "<>")))
		  (push 'right borders))
		(throw 'exit nil)))))))
    ;; Return value.
    borders))

(defun org-export-table-cell-starts-colgroup-p (table-cell info)
  "Non-nil when TABLE-CELL is at the beginning of a column group.
INFO is a plist used as a communication channel."
  ;; A cell starts a column group either when it is at the beginning
  ;; of a row (or after the special column, if any) or when it has
  ;; a left border.
  (or (eq (org-element-map (org-export-get-parent table-cell) 'table-cell
	    'identity info 'first-match)
	  table-cell)
      (memq 'left (org-export-table-cell-borders table-cell info))))

(defun org-export-table-cell-ends-colgroup-p (table-cell info)
  "Non-nil when TABLE-CELL is at the end of a column group.
INFO is a plist used as a communication channel."
  ;; A cell ends a column group either when it is at the end of a row
  ;; or when it has a right border.
  (or (eq (car (last (org-element-contents
			 (org-export-get-parent table-cell))))
	     table-cell)
      (memq 'right (org-export-table-cell-borders table-cell info))))

(defun org-export-table-row-starts-rowgroup-p (table-row info)
  "Non-nil when TABLE-ROW is at the beginning of a row group.
INFO is a plist used as a communication channel."
  (unless (or (eq (org-element-property :type table-row) 'rule)
	      (org-export-table-row-is-special-p table-row info))
    (let ((borders (org-export-table-cell-borders
		    (car (org-element-contents table-row)) info)))
      (or (memq 'top borders) (memq 'above borders)))))

(defun org-export-table-row-ends-rowgroup-p (table-row info)
  "Non-nil when TABLE-ROW is at the end of a row group.
INFO is a plist used as a communication channel."
  (unless (or (eq (org-element-property :type table-row) 'rule)
	      (org-export-table-row-is-special-p table-row info))
    (let ((borders (org-export-table-cell-borders
		    (car (org-element-contents table-row)) info)))
      (or (memq 'bottom borders) (memq 'below borders)))))

(defun org-export-table-row-in-header-p (table-row info)
  "Non-nil when TABLE-ROW is located within table's header.
INFO is a plist used as a communication channel.  Always return
nil for special rows and rows separators."
  (and (org-export-table-has-header-p
	(org-export-get-parent-table table-row) info)
       (eql (org-export-table-row-group table-row info) 1)))

(defun org-export-table-row-starts-header-p (table-row info)
  "Non-nil when TABLE-ROW is the first table header's row.
INFO is a plist used as a communication channel."
  (and (org-export-table-row-in-header-p table-row info)
       (org-export-table-row-starts-rowgroup-p table-row info)))

(defun org-export-table-row-ends-header-p (table-row info)
  "Non-nil when TABLE-ROW is the last table header's row.
INFO is a plist used as a communication channel."
  (and (org-export-table-row-in-header-p table-row info)
       (org-export-table-row-ends-rowgroup-p table-row info)))

(defun org-export-table-row-number (table-row info)
  "Return TABLE-ROW number.
INFO is a plist used as a communication channel.  Return value is
zero-indexed and ignores separators.  The function returns nil
for special rows and separators."
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((cache (or (plist-get info :table-row-number-cache)
		      (let ((table (make-hash-table :test #'eq)))
			(plist-put info :table-row-number-cache table)
			table)))
	   (cached (gethash table-row cache 'no-cache)))
      (if (not (eq cached 'no-cache)) cached
	;; First time a row is queried, populate cache with all the
	;; rows from the table.
	(let ((number -1))
	  (org-element-map (org-export-get-parent-table table-row) 'table-row
	    (lambda (row)
	      (when (eq (org-element-property :type row) 'standard)
		(puthash row (cl-incf number) cache)))
	    info))
	(gethash table-row cache)))))

(defun org-export-table-dimensions (table info)
  "Return TABLE dimensions.

INFO is a plist used as a communication channel.

Return value is a CONS like (ROWS . COLUMNS) where
ROWS (resp. COLUMNS) is the number of exportable
rows (resp. columns)."
  (let (first-row (columns 0) (rows 0))
    ;; Set number of rows, and extract first one.
    (org-element-map table 'table-row
      (lambda (row)
	(when (eq (org-element-property :type row) 'standard)
	  (cl-incf rows)
	  (unless first-row (setq first-row row)))) info)
    ;; Set number of columns.
    (org-element-map first-row 'table-cell (lambda (_) (cl-incf columns)) info)
    ;; Return value.
    (cons rows columns)))

(defun org-export-table-cell-address (table-cell info)
  "Return address of a regular TABLE-CELL object.

TABLE-CELL is the cell considered.  INFO is a plist used as
a communication channel.

Address is a CONS cell (ROW . COLUMN), where ROW and COLUMN are
zero-based index.  Only exportable cells are considered.  The
function returns nil for other cells."
  (let* ((table-row (org-export-get-parent table-cell))
	 (row-number (org-export-table-row-number table-row info)))
    (when row-number
      (cons row-number
	    (let ((col-count 0))
	      (org-element-map table-row 'table-cell
		(lambda (cell)
		  (if (eq cell table-cell) col-count (cl-incf col-count) nil))
		info 'first-match))))))

(defun org-export-get-table-cell-at (address table info)
  "Return regular table-cell object at ADDRESS in TABLE.

Address is a CONS cell (ROW . COLUMN), where ROW and COLUMN are
zero-based index.  TABLE is a table type element.  INFO is
a plist used as a communication channel.

If no table-cell, among exportable cells, is found at ADDRESS,
return nil."
  (let ((column-pos (cdr address)) (column-count 0))
    (org-element-map
	;; Row at (car address) or nil.
	(let ((row-pos (car address)) (row-count 0))
	  (org-element-map table 'table-row
	    (lambda (row)
	      (cond ((eq (org-element-property :type row) 'rule) nil)
		    ((= row-count row-pos) row)
		    (t (cl-incf row-count) nil)))
	    info 'first-match))
	'table-cell
      (lambda (cell)
	(if (= column-count column-pos) cell
	  (cl-incf column-count) nil))
      info 'first-match)))


;;;; For Tables of Contents
;;
;; `org-export-collect-headlines' builds a list of all exportable
;; headline elements, maybe limited to a certain depth.  One can then
;; easily parse it and transcode it.
;;
;; Building lists of tables, figures or listings is quite similar.
;; Once the generic function `org-export-collect-elements' is defined,
;; `org-export-collect-tables', `org-export-collect-figures' and
;; `org-export-collect-listings' can be derived from it.
;;
;; `org-export-toc-entry-backend' builds a special anonymous back-end
;; useful to export table of contents' entries.

(defun org-export-collect-headlines (info &optional n scope)
  "Collect headlines in order to build a table of contents.

INFO is a plist used as a communication channel.

When optional argument N is an integer, it specifies the depth of
the table of contents.  Otherwise, it is set to the value of the
last headline level.  See `org-export-headline-levels' for more
information.

Optional argument SCOPE, when non-nil, is an element.  If it is
a headline, only children of SCOPE are collected.  Otherwise,
collect children of the headline containing provided element.  If
there is no such headline, collect all headlines.  In any case,
argument N becomes relative to the level of that headline.

Return a list of all exportable headlines as parsed elements.
Footnote sections are ignored."
  (let* ((scope (cond ((not scope) (plist-get info :parse-tree))
		      ((eq (org-element-type scope) 'headline) scope)
		      ((org-export-get-parent-headline scope))
		      (t (plist-get info :parse-tree))))
	 (limit (plist-get info :headline-levels))
	 (n (if (not (wholenump n)) limit
	      (min (if (eq (org-element-type scope) 'org-data) n
		     (+ (org-export-get-relative-level scope info) n))
		   limit))))
    (org-element-map (org-element-contents scope) 'headline
      (lambda (headline)
	(unless (org-element-property :footnote-section-p headline)
	  (let ((level (org-export-get-relative-level headline info)))
	    (and (<= level n) headline))))
      info)))

(defun org-export-collect-elements (type info &optional predicate)
  "Collect referenceable elements of a determined type.

TYPE can be a symbol or a list of symbols specifying element
types to search.  Only elements with a caption are collected.

INFO is a plist used as a communication channel.

When non-nil, optional argument PREDICATE is a function accepting
one argument, an element of type TYPE.  It returns a non-nil
value when that element should be collected.

Return a list of all elements found, in order of appearance."
  (org-element-map (plist-get info :parse-tree) type
    (lambda (element)
      (and (org-element-property :caption element)
	   (or (not predicate) (funcall predicate element))
	   element))
    info))

(defun org-export-collect-tables (info)
  "Build a list of tables.
INFO is a plist used as a communication channel.

Return a list of table elements with a caption."
  (org-export-collect-elements 'table info))

(defun org-export-collect-figures (info predicate)
  "Build a list of figures.

INFO is a plist used as a communication channel.  PREDICATE is
a function which accepts one argument: a paragraph element and
whose return value is non-nil when that element should be
collected.

A figure is a paragraph type element, with a caption, verifying
PREDICATE.  The latter has to be provided since a \"figure\" is
a vague concept that may depend on back-end.

Return a list of elements recognized as figures."
  (org-export-collect-elements 'paragraph info predicate))

(defun org-export-collect-listings (info)
  "Build a list of src blocks.

INFO is a plist used as a communication channel.

Return a list of src-block elements with a caption."
  (org-export-collect-elements 'src-block info))

(defun org-export-toc-entry-backend (parent &rest transcoders)
  "Return an export back-end appropriate for table of contents entries.

PARENT is an export back-end the returned back-end should inherit
from.

By default, the back-end removes footnote references and targets.
It also changes links and radio targets into regular text.
TRANSCODERS optional argument, when non-nil, specifies additional
transcoders.  A transcoder follows the pattern (TYPE . FUNCTION)
where type is an element or object type and FUNCTION the function
transcoding it."
  (declare (indent 1))
  (org-export-create-backend
   :parent parent
   :transcoders
   (append transcoders
	   `((footnote-reference . ,#'ignore)
	     (link . ,(lambda (l c i)
			(or c
			    (org-export-data
			     (org-element-property :raw-link l)
			     i))))
	     (radio-target . ,(lambda (_r c _) c))
	     (target . ,#'ignore)))))


;;;; Smart Quotes
;;
;; The main function for the smart quotes sub-system is
;; `org-export-activate-smart-quotes', which replaces every quote in
;; a given string from the parse tree with its "smart" counterpart.
;;
;; Dictionary for smart quotes is stored in
;; `org-export-smart-quotes-alist'.

(defconst org-export-smart-quotes-alist
  '(("ar"
     (primary-opening
      :utf-8 "«" :html "&laquo;" :latex "\\guillemotleft{}"
      :texinfo "@guillemetleft{}")
     (primary-closing
      :utf-8 "»" :html "&raquo;" :latex "\\guillemotright{}"
      :texinfo "@guillemetright{}")
     (secondary-opening :utf-8 "‹" :html "&lsaquo;" :latex "\\guilsinglleft{}"
			:texinfo "@guilsinglleft{}")
     (secondary-closing :utf-8 "›" :html "&rsaquo;" :latex "\\guilsinglright{}"
			:texinfo "@guilsinglright{}")
     (apostrophe :utf-8 "’" :html "&rsquo;"))
    ("da"
     ;; one may use: »...«, "...", ›...‹, or '...'.
     ;; http://sproget.dk/raad-og-regler/retskrivningsregler/retskrivningsregler/a7-40-60/a7-58-anforselstegn/
     ;; LaTeX quotes require Babel!
     (primary-opening
      :utf-8 "»" :html "&raquo;" :latex ">>" :texinfo "@guillemetright{}")
     (primary-closing
      :utf-8 "«" :html "&laquo;" :latex "<<" :texinfo "@guillemetleft{}")
     (secondary-opening
      :utf-8 "›" :html "&rsaquo;" :latex "\\frq{}" :texinfo "@guilsinglright{}")
     (secondary-closing
      :utf-8 "‹" :html "&lsaquo;" :latex "\\flq{}" :texinfo "@guilsingleft{}")
     (apostrophe :utf-8 "’" :html "&rsquo;"))
    ("de"
     (primary-opening
      :utf-8 "„" :html "&bdquo;" :latex "\"`" :texinfo "@quotedblbase{}")
     (primary-closing
      :utf-8 "“" :html "&ldquo;" :latex "\"'" :texinfo "@quotedblleft{}")
     (secondary-opening
      :utf-8 "‚" :html "&sbquo;" :latex "\\glq{}" :texinfo "@quotesinglbase{}")
     (secondary-closing
      :utf-8 "‘" :html "&lsquo;" :latex "\\grq{}" :texinfo "@quoteleft{}")
     (apostrophe :utf-8 "’" :html "&rsquo;"))
    ("en"
     (primary-opening :utf-8 "“" :html "&ldquo;" :latex "``" :texinfo "``")
     (primary-closing :utf-8 "”" :html "&rdquo;" :latex "''" :texinfo "''")
     (secondary-opening :utf-8 "‘" :html "&lsquo;" :latex "`" :texinfo "`")
     (secondary-closing :utf-8 "’" :html "&rsquo;" :latex "'" :texinfo "'")
     (apostrophe :utf-8 "’" :html "&rsquo;"))
    ("es"
     (primary-opening
      :utf-8 "«" :html "&laquo;" :latex "\\guillemotleft{}"
      :texinfo "@guillemetleft{}")
     (primary-closing
      :utf-8 "»" :html "&raquo;" :latex "\\guillemotright{}"
      :texinfo "@guillemetright{}")
     (secondary-opening :utf-8 "“" :html "&ldquo;" :latex "``" :texinfo "``")
     (secondary-closing :utf-8 "”" :html "&rdquo;" :latex "''" :texinfo "''")
     (apostrophe :utf-8 "’" :html "&rsquo;"))
    ("fr"
     (primary-opening
      :utf-8 "« " :html "&laquo;&nbsp;" :latex "\\og "
      :texinfo "@guillemetleft{}@tie{}")
     (primary-closing
      :utf-8 " »" :html "&nbsp;&raquo;" :latex "\\fg{}"
      :texinfo "@tie{}@guillemetright{}")
     (secondary-opening
      :utf-8 "« " :html "&laquo;&nbsp;" :latex "\\og "
      :texinfo "@guillemetleft{}@tie{}")
     (secondary-closing :utf-8 " »" :html "&nbsp;&raquo;" :latex "\\fg{}"
			:texinfo "@tie{}@guillemetright{}")
     (apostrophe :utf-8 "’" :html "&rsquo;"))
    ("is"
     (primary-opening
      :utf-8 "„" :html "&bdquo;" :latex "\"`" :texinfo "@quotedblbase{}")
     (primary-closing
      :utf-8 "“" :html "&ldquo;" :latex "\"'" :texinfo "@quotedblleft{}")
     (secondary-opening
      :utf-8 "‚" :html "&sbquo;" :latex "\\glq{}" :texinfo "@quotesinglbase{}")
     (secondary-closing
      :utf-8 "‘" :html "&lsquo;" :latex "\\grq{}" :texinfo "@quoteleft{}")
     (apostrophe :utf-8 "’" :html "&rsquo;"))
    ("no"
     ;; https://nn.wikipedia.org/wiki/Sitatteikn
     (primary-opening
      :utf-8 "«" :html "&laquo;" :latex "\\guillemotleft{}"
      :texinfo "@guillemetleft{}")
     (primary-closing
      :utf-8 "»" :html "&raquo;" :latex "\\guillemotright{}"
      :texinfo "@guillemetright{}")
     (secondary-opening :utf-8 "‘" :html "&lsquo;" :latex "`" :texinfo "`")
     (secondary-closing :utf-8 "’" :html "&rsquo;" :latex "'" :texinfo "'")
     (apostrophe :utf-8 "’" :html "&rsquo;"))
    ("nb"
     ;; https://nn.wikipedia.org/wiki/Sitatteikn
     (primary-opening
      :utf-8 "«" :html "&laquo;" :latex "\\guillemotleft{}"
      :texinfo "@guillemetleft{}")
     (primary-closing
      :utf-8 "»" :html "&raquo;" :latex "\\guillemotright{}"
      :texinfo "@guillemetright{}")
     (secondary-opening :utf-8 "‘" :html "&lsquo;" :latex "`" :texinfo "`")
     (secondary-closing :utf-8 "’" :html "&rsquo;" :latex "'" :texinfo "'")
     (apostrophe :utf-8 "’" :html "&rsquo;"))
    ("nn"
     ;; https://nn.wikipedia.org/wiki/Sitatteikn
     (primary-opening
      :utf-8 "«" :html "&laquo;" :latex "\\guillemotleft{}"
      :texinfo "@guillemetleft{}")
     (primary-closing
      :utf-8 "»" :html "&raquo;" :latex "\\guillemotright{}"
      :texinfo "@guillemetright{}")
     (secondary-opening :utf-8 "‘" :html "&lsquo;" :latex "`" :texinfo "`")
     (secondary-closing :utf-8 "’" :html "&rsquo;" :latex "'" :texinfo "'")
     (apostrophe :utf-8 "’" :html "&rsquo;"))
    ("ru"
     ;; http://ru.wikipedia.org/wiki/%D0%9A%D0%B0%D0%B2%D1%8B%D1%87%D0%BA%D0%B8#.D0.9A.D0.B0.D0.B2.D1.8B.D1.87.D0.BA.D0.B8.2C_.D0.B8.D1.81.D0.BF.D0.BE.D0.BB.D1.8C.D0.B7.D1.83.D0.B5.D0.BC.D1.8B.D0.B5_.D0.B2_.D1.80.D1.83.D1.81.D1.81.D0.BA.D0.BE.D0.BC_.D1.8F.D0.B7.D1.8B.D0.BA.D0.B5
     ;; http://www.artlebedev.ru/kovodstvo/sections/104/
     (primary-opening :utf-8 "«" :html "&laquo;" :latex "{}<<"
		      :texinfo "@guillemetleft{}")
     (primary-closing :utf-8 "»" :html "&raquo;" :latex ">>{}"
		      :texinfo "@guillemetright{}")
     (secondary-opening
      :utf-8 "„" :html "&bdquo;" :latex "\\glqq{}" :texinfo "@quotedblbase{}")
     (secondary-closing
      :utf-8 "“" :html "&ldquo;" :latex "\\grqq{}" :texinfo "@quotedblleft{}")
     (apostrophe :utf-8 "’" :html: "&#39;"))
    ("sl"
     ;; Based on https://sl.wikipedia.org/wiki/Narekovaj
     (primary-opening :utf-8 "«" :html "&laquo;" :latex "{}<<"
		      :texinfo "@guillemetleft{}")
     (primary-closing :utf-8 "»" :html "&raquo;" :latex ">>{}"
		      :texinfo "@guillemetright{}")
     (secondary-opening
      :utf-8 "„" :html "&bdquo;" :latex "\\glqq{}" :texinfo "@quotedblbase{}")
     (secondary-closing
      :utf-8 "“" :html "&ldquo;" :latex "\\grqq{}" :texinfo "@quotedblleft{}")
     (apostrophe :utf-8 "’" :html "&rsquo;"))
    ("sv"
     ;; Based on https://sv.wikipedia.org/wiki/Citattecken
     (primary-opening :utf-8 "”" :html "&rdquo;" :latex "’’" :texinfo "’’")
     (primary-closing :utf-8 "”" :html "&rdquo;" :latex "’’" :texinfo "’’")
     (secondary-opening :utf-8 "’" :html "&rsquo;" :latex "’" :texinfo "`")
     (secondary-closing :utf-8 "’" :html "&rsquo;" :latex "’" :texinfo "'")
     (apostrophe :utf-8 "’" :html "&rsquo;")))
  "Smart quotes translations.

Alist whose CAR is a language string and CDR is an alist with
quote type as key and a plist associating various encodings to
their translation as value.

A quote type can be any symbol among `primary-opening',
`primary-closing', `secondary-opening', `secondary-closing' and
`apostrophe'.

Valid encodings include `:utf-8', `:html', `:latex' and
`:texinfo'.

If no translation is found, the quote character is left as-is.")

(defun org-export--smart-quote-status (s info)
  "Return smart quote status at the beginning of string S.
INFO is the current export state, as a plist."
  (let* ((parent (org-element-property :parent s))
	 (cache (or (plist-get info :smart-quote-cache)
		    (let ((table (make-hash-table :test #'eq)))
		      (plist-put info :smart-quote-cache table)
		      table)))
	 (value (gethash parent cache 'missing-data)))
    (if (not (eq value 'missing-data)) (cdr (assq s value))
      (let (level1-open full-status)
	(org-element-map
	    (let ((secondary (org-element-secondary-p s)))
	      (if secondary (org-element-property secondary parent)
		(org-element-contents parent)))
	    'plain-text
	  (lambda (text)
	    (let ((start 0) current-status)
	      (while (setq start (string-match "['\"]" text start))
		(push
		 (cond
		  ((equal (match-string 0 text) "\"")
		   (setf level1-open (not level1-open))
		   (if level1-open 'primary-opening 'primary-closing))
		  ;; Not already in a level 1 quote: this is an
		  ;; apostrophe.
		  ((not level1-open) 'apostrophe)
		  ;; Extract previous char and next char.  As
		  ;; a special case, they can also be set to `blank',
		  ;; `no-blank' or nil.  Then determine if current
		  ;; match is allowed as an opening quote or a closing
		  ;; quote.
		  (t
		   (let* ((previous
			   (if (> start 0) (substring text (1- start) start)
			     (let ((p (org-export-get-previous-element
				       text info)))
			       (cond ((not p) nil)
				     ((stringp p) (substring p -1))
				     ((memq (org-element-property :post-blank p)
					    '(0 nil))
				      'no-blank)
				     (t 'blank)))))
			  (next
			   (if (< (1+ start) (length text))
			       (substring text (1+ start) (+ start 2))
			     (let ((n (org-export-get-next-element text info)))
			       (cond ((not n) nil)
				     ((stringp n) (substring n 0 1))
				     (t 'no-blank)))))
			  (allow-open
			   (and (if (stringp previous)
				    (string-match "\\s\"\\|\\s-\\|\\s("
						  previous)
				  (memq previous '(blank nil)))
				(if (stringp next)
				    (string-match "\\w\\|\\s.\\|\\s_" next)
				  (eq next 'no-blank))))
			  (allow-close
			   (and (if (stringp previous)
				    (string-match "\\w\\|\\s.\\|\\s_" previous)
				  (eq previous 'no-blank))
				(if (stringp next)
				    (string-match "\\s-\\|\\s)\\|\\s.\\|\\s\""
						  next)
				  (memq next '(blank nil))))))
		     (cond
		      ((and allow-open allow-close) (error "Should not happen"))
		      (allow-open 'secondary-opening)
		      (allow-close 'secondary-closing)
		      (t 'apostrophe)))))
		 current-status)
		(cl-incf start))
	      (when current-status
		(push (cons text (nreverse current-status)) full-status))))
	  info nil org-element-recursive-objects)
	(puthash parent full-status cache)
	(cdr (assq s full-status))))))

(defun org-export-activate-smart-quotes (s encoding info &optional original)
  "Replace regular quotes with \"smart\" quotes in string S.

ENCODING is a symbol among `:html', `:latex', `:texinfo' and
`:utf-8'.  INFO is a plist used as a communication channel.

The function has to retrieve information about string
surroundings in parse tree.  It can only happen with an
unmodified string.  Thus, if S has already been through another
process, a non-nil ORIGINAL optional argument will provide that
original string.

Return the new string."
  (let ((quote-status
	 (copy-sequence (org-export--smart-quote-status (or original s) info))))
    (replace-regexp-in-string
     "['\"]"
     (lambda (match)
       (or (plist-get
	    (cdr (assq (pop quote-status)
		       (cdr (assoc (plist-get info :language)
				   org-export-smart-quotes-alist))))
	    encoding)
	   match))
     s nil t)))

;;;; Topology
;;
;; Here are various functions to retrieve information about the
;; neighborhood of a given element or object.  Neighbors of interest
;; are direct parent (`org-export-get-parent'), parent headline
;; (`org-export-get-parent-headline'), first element containing an
;; object, (`org-export-get-parent-element'), parent table
;; (`org-export-get-parent-table'), previous element or object
;; (`org-export-get-previous-element') and next element or object
;; (`org-export-get-next-element').

;; defsubst org-export-get-parent must be defined before first use

(defun org-export-get-parent-headline (blob)
  "Return BLOB parent headline or nil.
BLOB is the element or object being considered."
  (org-element-lineage blob '(headline)))

(defun org-export-get-parent-element (object)
  "Return first element containing OBJECT or nil.
OBJECT is the object to consider."
  (org-element-lineage object org-element-all-elements))

(defun org-export-get-parent-table (object)
  "Return OBJECT parent table or nil.
OBJECT is either a `table-cell' or `table-element' type object."
  (org-element-lineage object '(table)))

(defun org-export-get-previous-element (blob info &optional n)
  "Return previous element or object.

BLOB is an element or object.  INFO is a plist used as
a communication channel.  Return previous exportable element or
object, a string, or nil.

When optional argument N is a positive integer, return a list
containing up to N siblings before BLOB, from farthest to
closest.  With any other non-nil value, return a list containing
all of them."
  (let* ((secondary (org-element-secondary-p blob))
	 (parent (org-export-get-parent blob))
	 (siblings
	  (if secondary (org-element-property secondary parent)
	    (org-element-contents parent)))
	 prev)
    (catch 'exit
      (dolist (obj (cdr (memq blob (reverse siblings))) prev)
	(cond ((memq obj (plist-get info :ignore-list)))
	      ((null n) (throw 'exit obj))
	      ((not (wholenump n)) (push obj prev))
	      ((zerop n) (throw 'exit prev))
	      (t (cl-decf n) (push obj prev)))))))

(defun org-export-get-next-element (blob info &optional n)
  "Return next element or object.

BLOB is an element or object.  INFO is a plist used as
a communication channel.  Return next exportable element or
object, a string, or nil.

When optional argument N is a positive integer, return a list
containing up to N siblings after BLOB, from closest to farthest.
With any other non-nil value, return a list containing all of
them."
  (let* ((secondary (org-element-secondary-p blob))
	 (parent (org-export-get-parent blob))
	 (siblings
	  (cdr (memq blob
		     (if secondary (org-element-property secondary parent)
		       (org-element-contents parent)))))
	 next)
    (catch 'exit
      (dolist (obj siblings (nreverse next))
	(cond ((memq obj (plist-get info :ignore-list)))
	      ((null n) (throw 'exit obj))
	      ((not (wholenump n)) (push obj next))
	      ((zerop n) (throw 'exit (nreverse next)))
	      (t (cl-decf n) (push obj next)))))))


;;;; Translation
;;
;; `org-export-translate' translates a string according to the language
;; specified by the LANGUAGE keyword.  `org-export-dictionary' contains
;; the dictionary used for the translation.

(defconst org-export-dictionary
  '(("%e %n: %c"
     ("fr" :default "%e %n : %c" :html "%e&nbsp;%n&nbsp;: %c"))
    ("Author"
     ("ar" :default "تأليف")
     ("ca" :default "Autor")
     ("cs" :default "Autor")
     ("da" :default "Forfatter")
     ("de" :default "Autor")
     ("eo" :html "A&#365;toro")
     ("es" :default "Autor")
     ("et" :default "Autor")
     ("fi" :html "Tekij&auml;")
     ("fr" :default "Auteur")
     ("hu" :default "Szerz&otilde;")
     ("is" :html "H&ouml;fundur")
     ("it" :default "Autore")
     ("ja" :default "著者" :html "&#33879;&#32773;")
     ("nl" :default "Auteur")
     ("no" :default "Forfatter")
     ("nb" :default "Forfatter")
     ("nn" :default "Forfattar")
     ("pl" :default "Autor")
     ("pt_BR" :default "Autor")
     ("ru" :html "&#1040;&#1074;&#1090;&#1086;&#1088;" :utf-8 "Автор")
     ("sl" :default "Avtor")
     ("sv" :html "F&ouml;rfattare")
     ("uk" :html "&#1040;&#1074;&#1090;&#1086;&#1088;" :utf-8 "Автор")
     ("zh-CN" :html "&#20316;&#32773;" :utf-8 "作者")
     ("zh-TW" :html "&#20316;&#32773;" :utf-8 "作者"))
    ("Continued from previous page"
     ("ar" :default "تتمة الصفحة السابقة")
     ("de" :default "Fortsetzung von vorheriger Seite")
     ("es" :html "Contin&uacute;a de la p&aacute;gina anterior" :ascii "Continua de la pagina anterior" :default "Continúa de la página anterior")
     ("fr" :default "Suite de la page précédente")
     ("it" :default "Continua da pagina precedente")
     ("ja" :default "前ページからの続き")
     ("nl" :default "Vervolg van vorige pagina")
     ("pt" :default "Continuação da página anterior")
     ("ru" :html "(&#1055;&#1088;&#1086;&#1076;&#1086;&#1083;&#1078;&#1077;&#1085;&#1080;&#1077;)"
      :utf-8 "(Продолжение)")
     ("sl" :default "Nadaljevanje s prejšnje strani"))
    ("Continued on next page"
     ("ar" :default "التتمة في الصفحة التالية")
     ("de" :default "Fortsetzung nächste Seite")
     ("es" :html "Contin&uacute;a en la siguiente p&aacute;gina" :ascii "Continua en la siguiente pagina" :default "Continúa en la siguiente página")
     ("fr" :default "Suite page suivante")
     ("it" :default "Continua alla pagina successiva")
     ("ja" :default "次ページに続く")
     ("nl" :default "Vervolg op volgende pagina")
     ("pt" :default "Continua na página seguinte")
     ("ru" :html "(&#1055;&#1088;&#1086;&#1076;&#1086;&#1083;&#1078;&#1077;&#1085;&#1080;&#1077; &#1089;&#1083;&#1077;&#1076;&#1091;&#1077;&#1090;)"
      :utf-8 "(Продолжение следует)")
     ("sl" :default "Nadaljevanje na naslednji strani"))
    ("Created"
     ("sl" :default "Ustvarjeno"))
    ("Date"
     ("ar" :default "بتاريخ")
     ("ca" :default "Data")
     ("cs" :default "Datum")
     ("da" :default "Dato")
     ("de" :default "Datum")
     ("eo" :default "Dato")
     ("es" :default "Fecha")
     ("et" :html "Kuup&#228;ev" :utf-8 "Kuupäev")
     ("fi" :html "P&auml;iv&auml;m&auml;&auml;r&auml;")
     ("hu" :html "D&aacute;tum")
     ("is" :default "Dagsetning")
     ("it" :default "Data")
     ("ja" :default "日付" :html "&#26085;&#20184;")
     ("nl" :default "Datum")
     ("no" :default "Dato")
     ("nb" :default "Dato")
     ("nn" :default "Dato")
     ("pl" :default "Data")
     ("pt_BR" :default "Data")
     ("ru" :html "&#1044;&#1072;&#1090;&#1072;" :utf-8 "Дата")
     ("sl" :default "Datum")
     ("sv" :default "Datum")
     ("uk" :html "&#1044;&#1072;&#1090;&#1072;" :utf-8 "Дата")
     ("zh-CN" :html "&#26085;&#26399;" :utf-8 "日期")
     ("zh-TW" :html "&#26085;&#26399;" :utf-8 "日期"))
    ("Equation"
     ("ar" :default "معادلة")
     ("da" :default "Ligning")
     ("de" :default "Gleichung")
     ("es" :ascii "Ecuacion" :html "Ecuaci&oacute;n" :default "Ecuación")
     ("et" :html "V&#245;rrand" :utf-8 "Võrrand")
     ("fr" :ascii "Equation" :default "Équation")
     ("is" :default "Jafna")
     ("ja" :default "方程式")
     ("no" :default "Ligning")
     ("nb" :default "Ligning")
     ("nn" :default "Likning")
     ("pt_BR" :html "Equa&ccedil;&atilde;o" :default "Equação" :ascii "Equacao")
     ("ru" :html "&#1059;&#1088;&#1072;&#1074;&#1085;&#1077;&#1085;&#1080;&#1077;"
      :utf-8 "Уравнение")
     ("sl" :default "Enačba")
     ("sv" :default "Ekvation")
     ("zh-CN" :html "&#26041;&#31243;" :utf-8 "方程"))
    ("Figure"
     ("ar" :default "شكل")
     ("da" :default "Figur")
     ("de" :default "Abbildung")
     ("es" :default "Figura")
     ("et" :default "Joonis")
     ("is" :default "Mynd")
     ("ja" :default "図" :html "&#22259;")
     ("no" :default "Illustrasjon")
     ("nb" :default "Illustrasjon")
     ("nn" :default "Illustrasjon")
     ("pt_BR" :default "Figura")
     ("ru" :html "&#1056;&#1080;&#1089;&#1091;&#1085;&#1086;&#1082;" :utf-8 "Рисунок")
     ("sv" :default "Illustration")
     ("zh-CN" :html "&#22270;" :utf-8 "图"))
    ("Figure %d:"
     ("ar" :default "شكل %d:")
     ("da" :default "Figur %d")
     ("de" :default "Abbildung %d:")
     ("es" :default "Figura %d:")
     ("et" :default "Joonis %d:")
     ("fr" :default "Figure %d :" :html "Figure&nbsp;%d&nbsp;:")
     ("is" :default "Mynd %d")
     ("ja" :default "図%d: " :html "&#22259;%d: ")
     ("no" :default "Illustrasjon %d")
     ("nb" :default "Illustrasjon %d")
     ("nn" :default "Illustrasjon %d")
     ("pt_BR" :default "Figura %d:")
     ("ru" :html "&#1056;&#1080;&#1089;. %d.:" :utf-8 "Рис. %d.:")
     ("sl" :default "Slika %d")
     ("sv" :default "Illustration %d")
     ("zh-CN" :html "&#22270;%d&nbsp;" :utf-8 "图%d "))
    ("Footnotes"
     ("ar" :default "الهوامش")
     ("ca" :html "Peus de p&agrave;gina")
     ("cs" :default "Pozn\xe1mky pod carou")
     ("da" :default "Fodnoter")
     ("de" :html "Fu&szlig;noten" :default "Fußnoten")
     ("eo" :default "Piednotoj")
     ("es" :ascii "Nota al pie de pagina" :html "Nota al pie de p&aacute;gina" :default "Nota al pie de página")
     ("et" :html "Allm&#228;rkused" :utf-8 "Allmärkused")
     ("fi" :default "Alaviitteet")
     ("fr" :default "Notes de bas de page")
     ("hu" :html "L&aacute;bjegyzet")
     ("is" :html "Aftanm&aacute;lsgreinar")
     ("it" :html "Note a pi&egrave; di pagina")
     ("ja" :default "脚注" :html "&#33050;&#27880;")
     ("nl" :default "Voetnoten")
     ("no" :default "Fotnoter")
     ("nb" :default "Fotnoter")
     ("nn" :default "Fotnotar")
     ("pl" :default "Przypis")
     ("pt_BR" :html "Notas de Rodap&eacute;" :default "Notas de Rodapé" :ascii "Notas de Rodape")
     ("ru" :html "&#1057;&#1085;&#1086;&#1089;&#1082;&#1080;" :utf-8 "Сноски")
     ("sl" :default "Opombe")
     ("sv" :default "Fotnoter")
     ("uk" :html "&#1055;&#1088;&#1080;&#1084;&#1110;&#1090;&#1082;&#1080;"
      :utf-8 "Примітки")
     ("zh-CN" :html "&#33050;&#27880;" :utf-8 "脚注")
     ("zh-TW" :html "&#33139;&#35387;" :utf-8 "腳註"))
    ("List of Listings"
     ("ar" :default "قائمة بالبرامج")
     ("da" :default "Programmer")
     ("de" :default "Programmauflistungsverzeichnis")
     ("es" :ascii "Indice de Listados de programas" :html "&Iacute;ndice de Listados de programas" :default "Índice de Listados de programas")
     ("et" :default "Loendite nimekiri")
     ("fr" :default "Liste des programmes")
     ("ja" :default "ソースコード目次")
     ("no" :default "Dataprogrammer")
     ("nb" :default "Dataprogrammer")
     ("ru" :html "&#1057;&#1087;&#1080;&#1089;&#1086;&#1082; &#1088;&#1072;&#1089;&#1087;&#1077;&#1095;&#1072;&#1090;&#1086;&#1082;"
      :utf-8 "Список распечаток")
     ("sl" :default "Seznam programskih izpisov")
     ("zh-CN" :html "&#20195;&#30721;&#30446;&#24405;" :utf-8 "代码目录"))
    ("List of Tables"
     ("ar" :default "قائمة بالجداول")
     ("da" :default "Tabeller")
     ("de" :default "Tabellenverzeichnis")
     ("es" :ascii "Indice de tablas" :html "&Iacute;ndice de tablas" :default "Índice de tablas")
     ("et" :default "Tabelite nimekiri")
     ("fr" :default "Liste des tableaux")
     ("is" :default "Töfluskrá" :html "T&ouml;fluskr&aacute;")
     ("ja" :default "表目次")
     ("no" :default "Tabeller")
     ("nb" :default "Tabeller")
     ("nn" :default "Tabeller")
     ("pt_BR" :default "Índice de Tabelas" :ascii "Indice de Tabelas")
     ("ru" :html "&#1057;&#1087;&#1080;&#1089;&#1086;&#1082; &#1090;&#1072;&#1073;&#1083;&#1080;&#1094;"
      :utf-8 "Список таблиц")
     ("sl" :default "Seznam tabel")
     ("sv" :default "Tabeller")
     ("zh-CN" :html "&#34920;&#26684;&#30446;&#24405;" :utf-8 "表格目录"))
    ("Listing"
     ("ar" :default "برنامج")
     ("da" :default "Program")
     ("de" :default "Programmlisting")
     ("es" :default "Listado de programa")
     ("et" :default "Loend")
     ("fr" :default "Programme" :html "Programme")
     ("ja" :default "ソースコード")
     ("no" :default "Dataprogram")
     ("nb" :default "Dataprogram")
     ("pt_BR" :default "Listagem")
     ("ru" :html "&#1056;&#1072;&#1089;&#1087;&#1077;&#1095;&#1072;&#1090;&#1082;&#1072;"
      :utf-8 "Распечатка")
     ("sl" :default "Izpis programa")
     ("zh-CN" :html "&#20195;&#30721;" :utf-8 "代码"))
    ("Listing %d:"
     ("ar" :default "برنامج %d:")
     ("da" :default "Program %d")
     ("de" :default "Programmlisting %d")
     ("es" :default "Listado de programa %d")
     ("et" :default "Loend %d")
     ("fr" :default "Programme %d :" :html "Programme&nbsp;%d&nbsp;:")
     ("ja" :default "ソースコード%d:")
     ("no" :default "Dataprogram %d")
     ("nb" :default "Dataprogram %d")
     ("pt_BR" :default "Listagem %d")
     ("ru" :html "&#1056;&#1072;&#1089;&#1087;&#1077;&#1095;&#1072;&#1090;&#1082;&#1072; %d.:"
      :utf-8 "Распечатка %d.:")
     ("sl" :default "Izpis programa %d")
     ("zh-CN" :html "&#20195;&#30721;%d&nbsp;" :utf-8 "代码%d "))
    ("References"
     ("ar" :default "المراجع")
     ("fr" :ascii "References" :default "Références")
     ("de" :default "Quellen")
     ("es" :default "Referencias")
     ("sl" :default "Reference"))
    ("See figure %s"
     ("fr" :default "cf. figure %s"
      :html "cf.&nbsp;figure&nbsp;%s" :latex "cf.~figure~%s")
     ("sl" :default "Glej sliko %s"))
    ("See listing %s"
     ("fr" :default "cf. programme %s"
      :html "cf.&nbsp;programme&nbsp;%s" :latex "cf.~programme~%s")
     ("sl" :default "Glej izpis programa %s"))
    ("See section %s"
     ("ar" :default "انظر قسم %s")
     ("da" :default "jævnfør afsnit %s")
     ("de" :default "siehe Abschnitt %s")
     ("es" :ascii "Vea seccion %s" :html "Vea secci&oacute;n %s" :default "Vea sección %s")
     ("et" :html "Vaata peat&#252;kki %s" :utf-8 "Vaata peatükki %s")
     ("fr" :default "cf. section %s")
     ("ja" :default "セクション %s を参照")
     ("pt_BR" :html "Veja a se&ccedil;&atilde;o %s" :default "Veja a seção %s"
      :ascii "Veja a secao %s")
     ("ru" :html "&#1057;&#1084;. &#1088;&#1072;&#1079;&#1076;&#1077;&#1083; %s"
      :utf-8 "См. раздел %s")
     ("sl" :default "Glej poglavje %d")
     ("zh-CN" :html "&#21442;&#35265;&#31532;%s&#33410;" :utf-8 "参见第%s节"))
    ("See table %s"
     ("fr" :default "cf. tableau %s"
      :html "cf.&nbsp;tableau&nbsp;%s" :latex "cf.~tableau~%s")
     ("sl" :default "Glej tabelo %s"))
    ("Table"
     ("ar" :default "جدول")
     ("de" :default "Tabelle")
     ("es" :default "Tabla")
     ("et" :default "Tabel")
     ("fr" :default "Tableau")
     ("is" :default "Tafla")
     ("ja" :default "表" :html "&#34920;")
     ("pt_BR" :default "Tabela")
     ("ru" :html "&#1058;&#1072;&#1073;&#1083;&#1080;&#1094;&#1072;"
      :utf-8 "Таблица")
     ("zh-CN" :html "&#34920;" :utf-8 "表"))
    ("Table %d:"
     ("ar" :default "جدول %d:")
     ("da" :default "Tabel %d")
     ("de" :default "Tabelle %d")
     ("es" :default "Tabla %d")
     ("et" :default "Tabel %d")
     ("fr" :default "Tableau %d :")
     ("is" :default "Tafla %d")
     ("ja" :default "表%d:" :html "&#34920;%d:")
     ("no" :default "Tabell %d")
     ("nb" :default "Tabell %d")
     ("nn" :default "Tabell %d")
     ("pt_BR" :default "Tabela %d")
     ("ru" :html "&#1058;&#1072;&#1073;&#1083;&#1080;&#1094;&#1072; %d.:"
      :utf-8 "Таблица %d.:")
     ("sl" :default "Tabela %d")
     ("sv" :default "Tabell %d")
     ("zh-CN" :html "&#34920;%d&nbsp;" :utf-8 "表%d "))
    ("Table of Contents"
     ("ar" :default "قائمة المحتويات")
     ("ca" :html "&Iacute;ndex")
     ("cs" :default "Obsah")
     ("da" :default "Indhold")
     ("de" :default "Inhaltsverzeichnis")
     ("eo" :default "Enhavo")
     ("es" :ascii "Indice" :html "&Iacute;ndice" :default "Índice")
     ("et" :default "Sisukord")
     ("fi" :html "Sis&auml;llysluettelo")
     ("fr" :ascii "Sommaire" :default "Table des matières")
     ("hu" :html "Tartalomjegyz&eacute;k")
     ("is" :default "Efnisyfirlit")
     ("it" :default "Indice")
     ("ja" :default "目次" :html "&#30446;&#27425;")
     ("nl" :default "Inhoudsopgave")
     ("no" :default "Innhold")
     ("nb" :default "Innhold")
     ("nn" :default "Innhald")
     ("pl" :html "Spis tre&#x015b;ci")
     ("pt_BR" :html "&Iacute;ndice" :utf8 "Índice" :ascii "Indice")
     ("ru" :html "&#1057;&#1086;&#1076;&#1077;&#1088;&#1078;&#1072;&#1085;&#1080;&#1077;"
      :utf-8 "Содержание")
     ("sl" :default "Kazalo")
     ("sv" :html "Inneh&aring;ll")
     ("uk" :html "&#1047;&#1084;&#1110;&#1089;&#1090;" :utf-8 "Зміст")
     ("zh-CN" :html "&#30446;&#24405;" :utf-8 "目录")
     ("zh-TW" :html "&#30446;&#37636;" :utf-8 "目錄"))
    ("Unknown reference"
     ("ar" :default "مرجع غير معرّف")
     ("da" :default "ukendt reference")
     ("de" :default "Unbekannter Verweis")
     ("es" :default "Referencia desconocida")
     ("et" :default "Tundmatu viide")
     ("fr" :ascii "Destination inconnue" :default "Référence inconnue")
     ("ja" :default "不明な参照先")
     ("pt_BR" :default "Referência desconhecida"
      :ascii "Referencia desconhecida")
     ("ru" :html "&#1053;&#1077;&#1080;&#1079;&#1074;&#1077;&#1089;&#1090;&#1085;&#1072;&#1103; &#1089;&#1089;&#1099;&#1083;&#1082;&#1072;"
      :utf-8 "Неизвестная ссылка")
     ("sl" :default "Neznana referenca")
     ("zh-CN" :html "&#26410;&#30693;&#24341;&#29992;" :utf-8 "未知引用")))
  "Dictionary for export engine.

Alist whose car is the string to translate and cdr is an alist
whose car is the language string and cdr is a plist whose
properties are possible charsets and values translated terms.

It is used as a database for `org-export-translate'.  Since this
function returns the string as-is if no translation was found,
the variable only needs to record values different from the
entry.")

(defun org-export-translate (s encoding info)
  "Translate string S according to language specification.

ENCODING is a symbol among `:ascii', `:html', `:latex', `:latin1'
and `:utf-8'.  INFO is a plist used as a communication channel.

Translation depends on `:language' property.  Return the
translated string.  If no translation is found, try to fall back
to `:default' encoding.  If it fails, return S."
  (let* ((lang (plist-get info :language))
	 (translations (cdr (assoc lang
				   (cdr (assoc s org-export-dictionary))))))
    (or (plist-get translations encoding)
	(plist-get translations :default)
	s)))



;;; Asynchronous Export
;;
;; `org-export-async-start' is the entry point for asynchronous
;; export.  It recreates current buffer (including visibility,
;; narrowing and visited file) in an external Emacs process, and
;; evaluates a command there.  It then applies a function on the
;; returned results in the current process.
;;
;; At a higher level, `org-export-to-buffer' and `org-export-to-file'
;; allow exporting to a buffer or a file, asynchronously or not.
;;
;; `org-export-output-file-name' is an auxiliary function meant to be
;; used with `org-export-to-file'.  With a given extension, it tries
;; to provide a canonical file name to write export output to.
;;
;; Asynchronously generated results are never displayed directly.
;; Instead, they are stored in `org-export-stack-contents'.  They can
;; then be retrieved by calling `org-export-stack'.
;;
;; Export Stack is viewed through a dedicated major mode
;;`org-export-stack-mode' and tools: `org-export-stack-refresh',
;;`org-export-stack-delete', `org-export-stack-view' and
;;`org-export-stack-clear'.
;;
;; For back-ends, `org-export-add-to-stack' add a new source to stack.
;; It should be used whenever `org-export-async-start' is called.

(defmacro org-export-async-start  (fun &rest body)
  "Call function FUN on the results returned by BODY evaluation.

FUN is an anonymous function of one argument.  BODY evaluation
happens in an asynchronous process, from a buffer which is an
exact copy of the current one.

Use `org-export-add-to-stack' in FUN in order to register results
in the stack.

This is a low level function.  See also `org-export-to-buffer'
and `org-export-to-file' for more specialized functions."
  (declare (indent 1) (debug t))
  (org-with-gensyms (process temp-file copy-fun proc-buffer coding)
    ;; Write the full sexp evaluating BODY in a copy of the current
    ;; buffer to a temporary file, as it may be too long for program
    ;; args in `start-process'.
    `(with-temp-message "Initializing asynchronous export process"
       (let ((,copy-fun (org-export--generate-copy-script (current-buffer)))
             (,temp-file (make-temp-file "org-export-process"))
             (,coding buffer-file-coding-system))
         (with-temp-file ,temp-file
           (insert
            ;; Null characters (from variable values) are inserted
            ;; within the file.  As a consequence, coding system for
            ;; buffer contents will not be recognized properly.  So,
            ;; we make sure it is the same as the one used to display
            ;; the original buffer.
            (format ";; -*- coding: %s; -*-\n%S"
                    ,coding
                    `(with-temp-buffer
                       (when org-export-async-debug '(setq debug-on-error t))
                       ;; Ignore `kill-emacs-hook' and code evaluation
                       ;; queries from Babel as we need a truly
                       ;; non-interactive process.
                       (setq kill-emacs-hook nil
                             org-babel-confirm-evaluate-answer-no t)
                       ;; Initialize export framework.
                       (require 'ox)
                       ;; Re-create current buffer there.
                       (funcall ,,copy-fun)
                       (restore-buffer-modified-p nil)
                       ;; Sexp to evaluate in the buffer.
                       (print (progn ,,@body))))))
         ;; Start external process.
         (let* ((process-connection-type nil)
                (,proc-buffer (generate-new-buffer-name "*Org Export Process*"))
                (,process
		 (apply
		  #'start-process
		  (append
		   (list "org-export-process"
			 ,proc-buffer
			 (expand-file-name invocation-name invocation-directory)
			 "--batch")
		   (if org-export-async-init-file
		       (list "-Q" "-l" org-export-async-init-file)
		     (list "-l" user-init-file))
		   (list "-l" ,temp-file)))))
           ;; Register running process in stack.
           (org-export-add-to-stack (get-buffer ,proc-buffer) nil ,process)
           ;; Set-up sentinel in order to catch results.
           (let ((handler ,fun))
             (set-process-sentinel
              ,process
              `(lambda (p status)
                 (let ((proc-buffer (process-buffer p)))
                   (when (eq (process-status p) 'exit)
                     (unwind-protect
                         (if (zerop (process-exit-status p))
                             (unwind-protect
                                 (let ((results
                                        (with-current-buffer proc-buffer
                                          (goto-char (point-max))
                                          (backward-sexp)
                                          (read (current-buffer)))))
                                   (funcall ,handler results))
                               (unless org-export-async-debug
                                 (and (get-buffer proc-buffer)
                                      (kill-buffer proc-buffer))))
                           (org-export-add-to-stack proc-buffer nil p)
                           (ding)
                           (message "Process `%s' exited abnormally" p))
                       (unless org-export-async-debug
                         (delete-file ,,temp-file)))))))))))))

;;;###autoload
(defun org-export-to-buffer
  (backend buffer
	   &optional async subtreep visible-only body-only ext-plist
	   post-process)
  "Call `org-export-as' with output to a specified buffer.

BACKEND is either an export back-end, as returned by, e.g.,
`org-export-create-backend', or a symbol referring to
a registered back-end.

BUFFER is the name of the output buffer.  If it already exists,
it will be erased first, otherwise, it will be created.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should then be accessible
through the `org-export-stack' interface.  When ASYNC is nil, the
buffer is displayed if `org-export-show-temporary-export-buffer'
is non-nil.

Optional arguments SUBTREEP, VISIBLE-ONLY, BODY-ONLY and
EXT-PLIST are similar to those used in `org-export-as', which
see.

Optional argument POST-PROCESS is a function which should accept
no argument.  It is always called within the current process,
from BUFFER, with point at its beginning.  Export back-ends can
use it to set a major mode there, e.g,

  (defun org-latex-export-as-latex
    (&optional async subtreep visible-only body-only ext-plist)
    (interactive)
    (org-export-to-buffer \\='latex \"*Org LATEX Export*\"
      async subtreep visible-only body-only ext-plist (lambda () (LaTeX-mode))))

This function returns BUFFER."
  (declare (indent 2))
  (if async
      (org-export-async-start
	  `(lambda (output)
	     (with-current-buffer (get-buffer-create ,buffer)
	       (erase-buffer)
	       (setq buffer-file-coding-system ',buffer-file-coding-system)
	       (insert output)
	       (goto-char (point-min))
	       (org-export-add-to-stack (current-buffer) ',backend)
	       (ignore-errors (funcall ,post-process))))
	`(org-export-as
	  ',backend ,subtreep ,visible-only ,body-only ',ext-plist))
    (let ((output
	   (org-export-as backend subtreep visible-only body-only ext-plist))
	  (buffer (get-buffer-create buffer))
	  (encoding buffer-file-coding-system))
      (when (and (org-string-nw-p output) (org-export--copy-to-kill-ring-p))
	(org-kill-new output))
      (with-current-buffer buffer
	(erase-buffer)
	(setq buffer-file-coding-system encoding)
	(insert output)
	(goto-char (point-min))
	(and (functionp post-process) (funcall post-process)))
      (when org-export-show-temporary-export-buffer
	(switch-to-buffer-other-window buffer))
      buffer)))

;;;###autoload
(defun org-export-to-file
  (backend file &optional async subtreep visible-only body-only ext-plist
	   post-process)
  "Call `org-export-as' with output to a specified file.

BACKEND is either an export back-end, as returned by, e.g.,
`org-export-create-backend', or a symbol referring to
a registered back-end.  FILE is the name of the output file, as
a string.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer will then be accessible
through the `org-export-stack' interface.

Optional arguments SUBTREEP, VISIBLE-ONLY, BODY-ONLY and
EXT-PLIST are similar to those used in `org-export-as', which
see.

Optional argument POST-PROCESS is called with FILE as its
argument and happens asynchronously when ASYNC is non-nil.  It
has to return a file name, or nil.  Export back-ends can use this
to send the output file through additional processing, e.g,

  (defun org-latex-export-to-latex
    (&optional async subtreep visible-only body-only ext-plist)
    (interactive)
    (let ((outfile (org-export-output-file-name \".tex\" subtreep)))
      (org-export-to-file \\='latex outfile
        async subtreep visible-only body-only ext-plist
        (lambda (file) (org-latex-compile file)))

The function returns either a file name returned by POST-PROCESS,
or FILE."
  (declare (indent 2))
  (if (not (file-writable-p file)) (error "Output file not writable")
    (let ((ext-plist (org-combine-plists `(:output-file ,file) ext-plist))
	  (encoding (or org-export-coding-system buffer-file-coding-system)))
      (if async
          (org-export-async-start
	      `(lambda (file)
		 (org-export-add-to-stack (expand-file-name file) ',backend))
	    `(let ((output
		    (org-export-as
		     ',backend ,subtreep ,visible-only ,body-only
		     ',ext-plist)))
	       (with-temp-buffer
		 (insert output)
		 (let ((coding-system-for-write ',encoding))
		   (write-file ,file)))
	       (or (ignore-errors (funcall ',post-process ,file)) ,file)))
        (let ((output (org-export-as
                       backend subtreep visible-only body-only ext-plist)))
          (with-temp-buffer
            (insert output)
            (let ((coding-system-for-write encoding))
	      (write-file file)))
          (when (and (org-export--copy-to-kill-ring-p) (org-string-nw-p output))
            (org-kill-new output))
          ;; Get proper return value.
          (or (and (functionp post-process) (funcall post-process file))
	      file))))))

(defun org-export-output-file-name (extension &optional subtreep pub-dir)
  "Return output file's name according to buffer specifications.

EXTENSION is a string representing the output file extension,
with the leading dot.

With a non-nil optional argument SUBTREEP, try to determine
output file's name by looking for \"EXPORT_FILE_NAME\" property
of subtree at point.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return file name as a string."
  (let* ((visited-file (buffer-file-name (buffer-base-buffer)))
	 (base-name
	  (concat
	   (file-name-sans-extension
	    (or
	     ;; Check EXPORT_FILE_NAME subtree property.
	     (and subtreep (org-entry-get nil "EXPORT_FILE_NAME" 'selective))
	     ;; Check #+EXPORT_FILE_NAME keyword.
	     (org-with-point-at (point-min)
	       (catch :found
		 (let ((case-fold-search t))
		   (while (re-search-forward
			   "^[ \t]*#\\+EXPORT_FILE_NAME:[ \t]+\\S-" nil t)
		     (let ((element (org-element-at-point)))
		       (when (eq 'keyword (org-element-type element))
			 (throw :found
				(org-element-property :value element))))))))
	     ;; Extract from buffer's associated file, if any.
	     (and visited-file (file-name-nondirectory visited-file))
	     ;; Can't determine file name on our own: ask user.
	     (read-file-name
	      "Output file: " pub-dir nil nil nil
	      (lambda (n) (string= extension (file-name-extension n t))))))
	   extension))
	 (output-file
	  ;; Build file name.  Enforce EXTENSION over whatever user
	  ;; may have come up with.  PUB-DIR, if defined, always has
	  ;; precedence over any provided path.
	  (cond
	   (pub-dir (concat (file-name-as-directory pub-dir)
			    (file-name-nondirectory base-name)))
	   ((file-name-absolute-p base-name) base-name)
	   (t base-name))))
    ;; If writing to OUTPUT-FILE would overwrite original file, append
    ;; EXTENSION another time to final name.
    (if (and visited-file (file-equal-p visited-file output-file))
	(concat output-file extension)
      output-file)))

(defun org-export-add-to-stack (source backend &optional process)
  "Add a new result to export stack if not present already.

SOURCE is a buffer or a file name containing export results.
BACKEND is a symbol representing export back-end used to generate
it.

Entries already pointing to SOURCE and unavailable entries are
removed beforehand.  Return the new stack."
  (setq org-export-stack-contents
	(cons (list source backend (or process (current-time)))
	      (org-export-stack-remove source))))

(defun org-export-stack ()
  "Menu for asynchronous export results and running processes."
  (interactive)
  (let ((buffer (get-buffer-create "*Org Export Stack*")))
    (with-current-buffer buffer
      (org-export-stack-mode)
      (tabulated-list-print t))
    (pop-to-buffer buffer))
  (message "Type \"q\" to quit, \"?\" for help"))

(defun org-export-stack-clear ()
  "Remove all entries from export stack."
  (interactive)
  (setq org-export-stack-contents nil))

(defun org-export-stack-refresh ()
  "Refresh the export stack."
  (interactive)
  (tabulated-list-print t))

(defun org-export-stack-remove (&optional source)
  "Remove export results at point from stack.
If optional argument SOURCE is non-nil, remove it instead."
  (interactive)
  (let ((source (or source (org-export--stack-source-at-point))))
    (setq org-export-stack-contents
	  (cl-remove-if (lambda (el) (equal (car el) source))
			 org-export-stack-contents))))

(defun org-export-stack-view (&optional in-emacs)
  "View export results at point in stack.
With an optional prefix argument IN-EMACS, force viewing files
within Emacs."
  (interactive "P")
  (let ((source (org-export--stack-source-at-point)))
    (cond ((processp source)
	   (org-switch-to-buffer-other-window (process-buffer source)))
	  ((bufferp source) (org-switch-to-buffer-other-window source))
	  (t (org-open-file source in-emacs)))))

(defvar org-export-stack-mode-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km tabulated-list-mode-map)
    (define-key km " " 'next-line)
    (define-key km "\C-n" 'next-line)
    (define-key km [down] 'next-line)
    (define-key km "\C-p" 'previous-line)
    (define-key km "\C-?" 'previous-line)
    (define-key km [up] 'previous-line)
    (define-key km "C" 'org-export-stack-clear)
    (define-key km "v" 'org-export-stack-view)
    (define-key km (kbd "RET") 'org-export-stack-view)
    (define-key km "d" 'org-export-stack-remove)
    km)
  "Keymap for Org Export Stack.")

(define-derived-mode org-export-stack-mode tabulated-list-mode "Org-Stack"
  "Mode for displaying asynchronous export stack.

Type `\\[org-export-stack]' to visualize the asynchronous export
stack.

In an Org Export Stack buffer, use \
\\<org-export-stack-mode-map>`\\[org-export-stack-view]' to view export output
on current line, `\\[org-export-stack-remove]' to remove it from the stack and \
`\\[org-export-stack-clear]' to clear
stack completely.

Removing entries in a stack buffer does not affect files
or buffers, only display.

\\{org-export-stack-mode-map}"
  (setq tabulated-list-format
	(vector (list "#" 4 #'org-export--stack-num-predicate)
		(list "Back-End" 12 t)
		(list "Age" 6 nil)
		(list "Source" 0 nil)))
  (setq tabulated-list-sort-key (cons "#" nil))
  (setq tabulated-list-entries #'org-export--stack-generate)
  (add-hook 'tabulated-list-revert-hook #'org-export--stack-generate nil t)
  (add-hook 'post-command-hook #'org-export-stack-refresh nil t)
  (tabulated-list-init-header))

(defun org-export--stack-generate ()
  "Generate the asynchronous export stack for display.
Unavailable sources are removed from the list.  Return a list
appropriate for `tabulated-list-print'."
  ;; Clear stack from exited processes, dead buffers or non-existent
  ;; files.
  (setq org-export-stack-contents
	(cl-remove-if-not
	 (lambda (el)
	   (if (processp (nth 2 el))
	       (buffer-live-p (process-buffer (nth 2 el)))
	     (let ((source (car el)))
	       (if (bufferp source) (buffer-live-p source)
		 (file-exists-p source)))))
	 org-export-stack-contents))
  ;; Update `tabulated-list-entries'.
  (let ((counter 0))
    (mapcar
     (lambda (entry)
       (let ((source (car entry)))
	 (list source
	       (vector
		;; Counter.
		(number-to-string (cl-incf counter))
		;; Back-End.
		(if (nth 1 entry) (symbol-name (nth 1 entry)) "")
		;; Age.
		(let ((info (nth 2 entry)))
		  (if (processp info) (symbol-name (process-status info))
		    (format-seconds "%h:%.2m" (float-time (time-since info)))))
		;; Source.
		(if (stringp source) source (buffer-name source))))))
     org-export-stack-contents)))

(defun org-export--stack-num-predicate (a b)
  (< (string-to-number (aref (nth 1 a) 0))
     (string-to-number (aref (nth 1 b) 0))))

(defun org-export--stack-source-at-point ()
  "Return source from export results at point in stack."
  (let ((source (car (nth (1- (org-current-line)) org-export-stack-contents))))
    (if (not source) (error "Source unavailable, please refresh buffer")
      (let ((source-name (if (stringp source) source (buffer-name source))))
	(if (save-excursion
	      (beginning-of-line)
	      (looking-at-p (concat ".* +" (regexp-quote source-name) "$")))
	    source
	  ;; SOURCE is not consistent with current line.  The stack
	  ;; view is outdated.
	  (error (substitute-command-keys
		  "Source unavailable; type `\\[org-export-stack-refresh]' \
to refresh buffer")))))))



;;; The Dispatcher
;;
;; `org-export-dispatch' is the standard interactive way to start an
;; export process.  It uses `org-export--dispatch-ui' as a subroutine
;; for its interface, which, in turn, delegates response to key
;; pressed to `org-export--dispatch-action'.

;;;###autoload
(defun org-export-dispatch (&optional arg)
  "Export dispatcher for Org mode.

It provides an access to common export related tasks in a buffer.
Its interface comes in two flavors: standard and expert.

While both share the same set of bindings, only the former
displays the valid keys associations in a dedicated buffer.
Scrolling (resp. line-wise motion) in this buffer is done with
SPC and DEL (resp. C-n and C-p) keys.

Set variable `org-export-dispatch-use-expert-ui' to switch to one
flavor or the other.

When ARG is `\\[universal-argument]', repeat the last export action, with the\
 same
set of options used back then, on the current buffer.

When ARG is `\\[universal-argument] \\[universal-argument]', display the \
asynchronous export stack."
  (interactive "P")
  (let* ((input
	  (cond ((equal arg '(16)) '(stack))
		((and arg org-export-dispatch-last-action))
		(t (save-window-excursion
		     (unwind-protect
			 (progn
			   ;; Remember where we are
			   (move-marker org-export-dispatch-last-position
					(point)
					(org-base-buffer (current-buffer)))
			   ;; Get and store an export command
			   (setq org-export-dispatch-last-action
				 (org-export--dispatch-ui
				  (list org-export-initial-scope
					(and org-export-in-background 'async))
				  nil
				  org-export-dispatch-use-expert-ui)))
		       (and (get-buffer "*Org Export Dispatcher*")
			    (kill-buffer "*Org Export Dispatcher*")))))))
	 (action (car input))
	 (optns (cdr input)))
    (unless (memq 'subtree optns)
      (move-marker org-export-dispatch-last-position nil))
    (cl-case action
      ;; First handle special hard-coded actions.
      (template (org-export-insert-default-template nil optns))
      (stack (org-export-stack))
      (publish-current-file
       (org-publish-current-file (memq 'force optns) (memq 'async optns)))
      (publish-current-project
       (org-publish-current-project (memq 'force optns) (memq 'async optns)))
      (publish-choose-project
       (org-publish (assoc (completing-read
			    "Publish project: "
			    org-publish-project-alist nil t)
			   org-publish-project-alist)
		    (memq 'force optns)
		    (memq 'async optns)))
      (publish-all (org-publish-all (memq 'force optns) (memq 'async optns)))
      (otherwise
       (save-excursion
	 (when arg
	   ;; Repeating command, maybe move cursor to restore subtree
	   ;; context.
	   (if (eq (marker-buffer org-export-dispatch-last-position)
		   (org-base-buffer (current-buffer)))
	       (goto-char org-export-dispatch-last-position)
	     ;; We are in a different buffer, forget position.
	     (move-marker org-export-dispatch-last-position nil)))
	 (funcall action
		  ;; Return a symbol instead of a list to ease
		  ;; asynchronous export macro use.
		  (and (memq 'async optns) t)
		  (and (memq 'subtree optns) t)
		  (and (memq 'visible optns) t)
		  (and (memq 'body optns) t)))))))

(defun org-export--dispatch-ui (options first-key expertp)
  "Handle interface for `org-export-dispatch'.

OPTIONS is a list containing current interactive options set for
export.  It can contain any of the following symbols:
`body'    toggles a body-only export
`subtree' restricts export to current subtree
`visible' restricts export to visible part of buffer.
`force'   force publishing files.
`async'   use asynchronous export process

FIRST-KEY is the key pressed to select the first level menu.  It
is nil when this menu hasn't been selected yet.

EXPERTP, when non-nil, triggers expert UI.  In that case, no help
buffer is provided, but indications about currently active
options are given in the prompt.  Moreover, [?] allows switching
back to standard interface."
  (let* ((fontify-key
	  (lambda (key &optional access-key)
	    ;; Fontify KEY string.  Optional argument ACCESS-KEY, when
	    ;; non-nil is the required first-level key to activate
	    ;; KEY.  When its value is t, activate KEY independently
	    ;; on the first key, if any.  A nil value means KEY will
	    ;; only be activated at first level.
	    (if (or (eq access-key t) (eq access-key first-key))
		(propertize key 'face 'org-warning)
	      key)))
	 (fontify-value
	  (lambda (value)
	    ;; Fontify VALUE string.
	    (propertize value 'face 'font-lock-variable-name-face)))
	 ;; Prepare menu entries by extracting them from registered
	 ;; back-ends and sorting them by access key and by ordinal,
	 ;; if any.
	 (entries
	  (sort (sort (delq nil
			    (mapcar #'org-export-backend-menu
				    org-export-registered-backends))
		      (lambda (a b)
			(let ((key-a (nth 1 a))
			      (key-b (nth 1 b)))
			  (cond ((and (numberp key-a) (numberp key-b))
				 (< key-a key-b))
				((numberp key-b) t)))))
		'car-less-than-car))
	 ;; Compute a list of allowed keys based on the first key
	 ;; pressed, if any.  Some keys
	 ;; (?^B, ?^V, ?^S, ?^F, ?^A, ?&, ?# and ?q) are always
	 ;; available.
	 (allowed-keys
	  (nconc (list 2 22 19 6 1)
		 (if (not first-key) (org-uniquify (mapcar 'car entries))
		   (let (sub-menu)
		     (dolist (entry entries (sort (mapcar 'car sub-menu) '<))
		       (when (eq (car entry) first-key)
			 (setq sub-menu (append (nth 2 entry) sub-menu))))))
		 (cond ((eq first-key ?P) (list ?f ?p ?x ?a))
		       ((not first-key) (list ?P)))
		 (list ?& ?#)
		 (when expertp (list ??))
		 (list ?q)))
	 ;; Build the help menu for standard UI.
	 (help
	  (unless expertp
	    (concat
	     ;; Options are hard-coded.
	     (format "[%s] Body only:    %s           [%s] Visible only:     %s
\[%s] Export scope: %s       [%s] Force publishing: %s
\[%s] Async export: %s\n\n"
		     (funcall fontify-key "C-b" t)
		     (funcall fontify-value
			      (if (memq 'body options) "On " "Off"))
		     (funcall fontify-key "C-v" t)
		     (funcall fontify-value
			      (if (memq 'visible options) "On " "Off"))
		     (funcall fontify-key "C-s" t)
		     (funcall fontify-value
			      (if (memq 'subtree options) "Subtree" "Buffer "))
		     (funcall fontify-key "C-f" t)
		     (funcall fontify-value
			      (if (memq 'force options) "On " "Off"))
		     (funcall fontify-key "C-a" t)
		     (funcall fontify-value
			      (if (memq 'async options) "On " "Off")))
	     ;; Display registered back-end entries.  When a key
	     ;; appears for the second time, do not create another
	     ;; entry, but append its sub-menu to existing menu.
	     (let (last-key)
	       (mapconcat
		(lambda (entry)
		  (let ((top-key (car entry)))
		    (concat
		     (unless (eq top-key last-key)
		       (setq last-key top-key)
		       (format "\n[%s] %s\n"
			       (funcall fontify-key (char-to-string top-key))
			       (nth 1 entry)))
		     (let ((sub-menu (nth 2 entry)))
		       (unless (functionp sub-menu)
			 ;; Split sub-menu into two columns.
			 (let ((index -1))
			   (concat
			    (mapconcat
			     (lambda (sub-entry)
			       (cl-incf index)
			       (format
				(if (zerop (mod index 2)) "    [%s] %-26s"
				  "[%s] %s\n")
				(funcall fontify-key
					 (char-to-string (car sub-entry))
					 top-key)
				(nth 1 sub-entry)))
			     sub-menu "")
			    (when (zerop (mod index 2)) "\n"))))))))
		entries ""))
	     ;; Publishing menu is hard-coded.
	     (format "\n[%s] Publish
    [%s] Current file              [%s] Current project
    [%s] Choose project            [%s] All projects\n\n\n"
		     (funcall fontify-key "P")
		     (funcall fontify-key "f" ?P)
		     (funcall fontify-key "p" ?P)
		     (funcall fontify-key "x" ?P)
		     (funcall fontify-key "a" ?P))
	     (format "[%s] Export stack                  [%s] Insert template\n"
		     (funcall fontify-key "&" t)
		     (funcall fontify-key "#" t))
	     (format "[%s] %s"
		     (funcall fontify-key "q" t)
		     (if first-key "Main menu" "Exit")))))
	 ;; Build prompts for both standard and expert UI.
	 (standard-prompt (unless expertp "Export command: "))
	 (expert-prompt
	  (when expertp
	    (format
	     "Export command (C-%s%s%s%s%s) [%s]: "
	     (if (memq 'body options) (funcall fontify-key "b" t) "b")
	     (if (memq 'visible options) (funcall fontify-key "v" t) "v")
	     (if (memq 'subtree options) (funcall fontify-key "s" t) "s")
	     (if (memq 'force options) (funcall fontify-key "f" t) "f")
	     (if (memq 'async options) (funcall fontify-key "a" t) "a")
	     (mapconcat (lambda (k)
			  ;; Strip control characters.
			  (unless (< k 27) (char-to-string k)))
			allowed-keys "")))))
    ;; With expert UI, just read key with a fancy prompt.  In standard
    ;; UI, display an intrusive help buffer.
    (if expertp
	(org-export--dispatch-action
	 expert-prompt allowed-keys entries options first-key expertp)
      ;; At first call, create frame layout in order to display menu.
      (unless (get-buffer "*Org Export Dispatcher*")
	(delete-other-windows)
	(org-switch-to-buffer-other-window
	 (get-buffer-create "*Org Export Dispatcher*"))
	(setq cursor-type nil
	      header-line-format "Use SPC, DEL, C-n or C-p to navigate.")
	;; Make sure that invisible cursor will not highlight square
	;; brackets.
	(set-syntax-table (copy-syntax-table))
	(modify-syntax-entry ?\[ "w"))
      ;; At this point, the buffer containing the menu exists and is
      ;; visible in the current window.  So, refresh it.
      (with-current-buffer "*Org Export Dispatcher*"
	;; Refresh help.  Maintain display continuity by re-visiting
	;; previous window position.
	(let ((pos (window-start)))
	  (erase-buffer)
	  (insert help)
	  (set-window-start nil pos)))
      (org-fit-window-to-buffer)
      (org-export--dispatch-action
       standard-prompt allowed-keys entries options first-key expertp))))

(defun org-export--dispatch-action
    (prompt allowed-keys entries options first-key expertp)
  "Read a character from command input and act accordingly.

PROMPT is the displayed prompt, as a string.  ALLOWED-KEYS is
a list of characters available at a given step in the process.
ENTRIES is a list of menu entries.  OPTIONS, FIRST-KEY and
EXPERTP are the same as defined in `org-export--dispatch-ui',
which see.

Toggle export options when required.  Otherwise, return value is
a list with action as CAR and a list of interactive export
options as CDR."
  (let (key)
    ;; Scrolling: when in non-expert mode, act on motion keys (C-n,
    ;; C-p, SPC, DEL).
    (while (and (setq key (read-char-exclusive prompt))
		(not expertp)
		(memq key '(14 16 ?\s ?\d)))
      (cl-case key
	(14 (if (not (pos-visible-in-window-p (point-max)))
		(ignore-errors (scroll-up 1))
	      (message "End of buffer")
	      (sit-for 1)))
	(16 (if (not (pos-visible-in-window-p (point-min)))
		(ignore-errors (scroll-down 1))
	      (message "Beginning of buffer")
	      (sit-for 1)))
	(?\s (if (not (pos-visible-in-window-p (point-max)))
		 (scroll-up nil)
	       (message "End of buffer")
	       (sit-for 1)))
	(?\d (if (not (pos-visible-in-window-p (point-min)))
		 (scroll-down nil)
	       (message "Beginning of buffer")
	       (sit-for 1)))))
    (cond
     ;; Ignore undefined associations.
     ((not (memq key allowed-keys))
      (ding)
      (unless expertp (message "Invalid key") (sit-for 1))
      (org-export--dispatch-ui options first-key expertp))
     ;; q key at first level aborts export.  At second level, cancel
     ;; first key instead.
     ((eq key ?q) (if (not first-key) (error "Export aborted")
		    (org-export--dispatch-ui options nil expertp)))
     ;; Help key: Switch back to standard interface if expert UI was
     ;; active.
     ((eq key ??) (org-export--dispatch-ui options first-key nil))
     ;; Send request for template insertion along with export scope.
     ((eq key ?#) (cons 'template (memq 'subtree options)))
     ;; Switch to asynchronous export stack.
     ((eq key ?&) '(stack))
     ;; Toggle options: C-b (2) C-v (22) C-s (19) C-f (6) C-a (1).
     ((memq key '(2 22 19 6 1))
      (org-export--dispatch-ui
       (let ((option (cl-case key (2 'body) (22 'visible) (19 'subtree)
			      (6 'force) (1 'async))))
	 (if (memq option options) (remq option options)
	   (cons option options)))
       first-key expertp))
     ;; Action selected: Send key and options back to
     ;; `org-export-dispatch'.
     ((or first-key (functionp (nth 2 (assq key entries))))
      (cons (cond
	     ((not first-key) (nth 2 (assq key entries)))
	     ;; Publishing actions are hard-coded.  Send a special
	     ;; signal to `org-export-dispatch'.
	     ((eq first-key ?P)
	      (cl-case key
		(?f 'publish-current-file)
		(?p 'publish-current-project)
		(?x 'publish-choose-project)
		(?a 'publish-all)))
	     ;; Return first action associated to FIRST-KEY + KEY
	     ;; path. Indeed, derived backends can share the same
	     ;; FIRST-KEY.
	     (t (catch 'found
		  (dolist (entry (member (assq first-key entries) entries))
		    (let ((match (assq key (nth 2 entry))))
		      (when match (throw 'found (nth 2 match))))))))
	    options))
     ;; Otherwise, enter sub-menu.
     (t (org-export--dispatch-ui options key expertp)))))



(provide 'ox)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox.el ends here
