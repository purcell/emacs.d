;;; ox.el --- Generic Export Engine for Org Mode

;; Copyright (C) 2012-2014 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This library implements a generic export engine for Org, built on
;; its syntactical parser: Org Elements.
;;
;; Besides that parser, the generic exporter is made of three distinct
;; parts:
;;
;; - The communication channel consists in a property list, which is
;;   created and updated during the process.  Its use is to offer
;;   every piece of information, would it be about initial environment
;;   or contextual data, all in a single place.  The exhaustive list
;;   of properties is given in "The Communication Channel" section of
;;   this file.
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
;; The core function is `org-export-as'.  It returns the transcoded
;; buffer as a string.
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
;; Then, a wrapper macro for asynchronous export,
;; `org-export-async-start', along with tools to display results. are
;; given in the penultimate part.
;;
;; Eventually, a dispatcher (`org-export-dispatch') for standard
;; back-ends is provided in the last one.

;;; Code:

(eval-when-compile (require 'cl))
(require 'org-element)
(require 'org-macro)
(require 'ob-exp)

(declare-function org-publish "ox-publish" (project &optional force async))
(declare-function org-publish-all "ox-publish" (&optional force async))
(declare-function
 org-publish-current-file "ox-publish" (&optional force async))
(declare-function org-publish-current-project "ox-publish"
		  (&optional force async))

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
  '((:author "AUTHOR" nil user-full-name t)
    (:creator "CREATOR" nil org-export-creator-string)
    (:date "DATE" nil nil t)
    (:description "DESCRIPTION" nil nil newline)
    (:email "EMAIL" nil user-mail-address t)
    (:exclude-tags "EXCLUDE_TAGS" nil org-export-exclude-tags split)
    (:headline-levels nil "H" org-export-headline-levels)
    (:keywords "KEYWORDS" nil nil space)
    (:language "LANGUAGE" nil org-export-default-language t)
    (:preserve-breaks nil "\\n" org-export-preserve-breaks)
    (:section-numbers nil "num" org-export-with-section-numbers)
    (:select-tags "SELECT_TAGS" nil org-export-select-tags split)
    (:time-stamp-file nil "timestamp" org-export-time-stamp-file)
    (:title "TITLE" nil nil space)
    (:with-archived-trees nil "arch" org-export-with-archived-trees)
    (:with-author nil "author" org-export-with-author)
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
    (:with-smart-quotes nil "'" org-export-with-smart-quotes)
    (:with-special-strings nil "-" org-export-with-special-strings)
    (:with-statistics-cookies nil "stat" org-export-with-statistics-cookies)
    (:with-sub-superscript nil "^" org-export-with-sub-superscripts)
    (:with-toc nil "toc" org-export-with-toc)
    (:with-tables nil "|" org-export-with-tables)
    (:with-tags nil "tags" org-export-with-tags)
    (:with-tasks nil "tasks" org-export-with-tasks)
    (:with-timestamps nil "<" org-export-with-timestamps)
    (:with-todo-keywords nil "todo" org-export-with-todo-keywords))
  "Alist between export properties and ways to set them.

The CAR of the alist is the property name, and the CDR is a list
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
  '((:filter-bold . org-export-filter-bold-functions)
    (:filter-babel-call . org-export-filter-babel-call-functions)
    (:filter-center-block . org-export-filter-center-block-functions)
    (:filter-clock . org-export-filter-clock-functions)
    (:filter-code . org-export-filter-code-functions)
    (:filter-comment . org-export-filter-comment-functions)
    (:filter-comment-block . org-export-filter-comment-block-functions)
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
    (:filter-quote-section . org-export-filter-quote-section-functions)
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

(defvar org-export--registered-backends nil
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
	  (const :tag "Entirely" t)))

(defcustom org-export-with-author t
  "Non-nil means insert author name into the exported file.
This option can also be set with the OPTIONS keyword,
e.g. \"author:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-clocks nil
  "Non-nil means export CLOCK keywords.
This option can also be set with the OPTIONS keyword,
e.g. \"c:t\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-creator 'comment
  "Non-nil means the postamble should contain a creator sentence.

The sentence can be set in `org-export-creator-string' and
defaults to \"Generated by Org mode XX in Emacs XXX.\".

If the value is `comment' insert it as a comment."
  :group 'org-export-general
  :type '(choice
	  (const :tag "No creator sentence" nil)
	  (const :tag "Sentence as a comment" comment)
	  (const :tag "Insert the sentence" t)))

(defcustom org-export-with-date t
  "Non-nil means insert date in the exported document.
This option can also be set with the OPTIONS keyword,
e.g. \"date:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-date-timestamp-format nil
  "Time-stamp format string to use for DATE keyword.

The format string, when specified, only applies if date consists
in a single time-stamp.  Otherwise its value will be ignored.

See `format-time-string' for details on how to build this
string."
  :group 'org-export-general
  :type '(choice
	  (string :tag "Time-stamp format string")
	  (const :tag "No format string" nil)))

(defcustom org-export-creator-string
  (format "Emacs %s (Org mode %s)"
	  emacs-version
	  (if (fboundp 'org-version) (org-version) "unknown version"))
  "Information about the creator of the document.
This option can also be set on with the CREATOR keyword."
  :group 'org-export-general
  :type '(string :tag "Creator string"))

(defcustom org-export-with-drawers '(not "LOGBOOK")
  "Non-nil means export contents of standard drawers.

When t, all drawers are exported.  This may also be a list of
drawer names to export.  If that list starts with `not', only
drawers with such names will be ignored.

This variable doesn't apply to properties drawers.

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
			(string :tag "Drawer name")))))

(defcustom org-export-with-email nil
  "Non-nil means insert author email into the exported file.
This option can also be set with the OPTIONS keyword,
e.g. \"email:t\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-emphasize t
  "Non-nil means interpret *word*, /word/, _word_ and +word+.

If the export target supports emphasizing text, the word will be
typeset in bold, italic, with an underline or strike-through,
respectively.

This option can also be set with the OPTIONS keyword,
e.g. \"*:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-exclude-tags '("noexport")
  "Tags that exclude a tree from export.

All trees carrying any of these tags will be excluded from
export.  This is without condition, so even subtrees inside that
carry one of the `org-export-select-tags' will be removed.

This option can also be set with the EXCLUDE_TAGS keyword."
  :group 'org-export-general
  :type '(repeat (string :tag "Tag")))

(defcustom org-export-with-fixed-width t
  "Non-nil means lines starting with \":\" will be in fixed width font.

This can be used to have pre-formatted text, fragments of code
etc.  For example:
  : ;; Some Lisp examples
  : (while (defc cnt)
  :   (ding))
will be looking just like this in also HTML.  See also the QUOTE
keyword.  Not all export backends support this.

This option can also be set with the OPTIONS keyword,
e.g. \"::nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-footnotes t
  "Non-nil means Org footnotes should be exported.
This option can also be set with the OPTIONS keyword,
e.g. \"f:nil\"."
  :group 'org-export-general
  :type 'boolean)

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
	  (const :tag "Leave math verbatim" verbatim)))

(defcustom org-export-headline-levels 3
  "The last level which is still exported as a headline.

Inferior levels will usually produce itemize or enumerate lists
when exported, but back-end behaviour may differ.

This option can also be set with the OPTIONS keyword,
e.g. \"H:2\"."
  :group 'org-export-general
  :type 'integer)

(defcustom org-export-default-language "en"
  "The default language for export and clocktable translations, as a string.
This may have an association in
`org-clock-clocktable-language-setup',
`org-export-smart-quotes-alist' and `org-export-dictionary'.
This option can also be set with the LANGUAGE keyword."
  :group 'org-export-general
  :type '(string :tag "Language"))

(defcustom org-export-preserve-breaks nil
  "Non-nil means preserve all line breaks when exporting.
This option can also be set with the OPTIONS keyword,
e.g. \"\\n:t\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-entities t
  "Non-nil means interpret entities when exporting.

For example, HTML export converts \\alpha to &alpha; and \\AA to
&Aring;.

For a list of supported names, see the constant `org-entities'
and the user option `org-entities-user'.

This option can also be set with the OPTIONS keyword,
e.g. \"e:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-inlinetasks t
  "Non-nil means inlinetasks should be exported.
This option can also be set with the OPTIONS keyword,
e.g. \"inline:nil\"."
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-export-with-planning nil
  "Non-nil means include planning info in export.

Planning info is the line containing either SCHEDULED:,
DEADLINE:, CLOSED: time-stamps, or a combination of them.

This option can also be set with the OPTIONS keyword,
e.g. \"p:t\"."
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-export-with-priority nil
  "Non-nil means include priority cookies in export.
This option can also be set with the OPTIONS keyword,
e.g. \"pri:t\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-section-numbers t
  "Non-nil means add section numbers to headlines when exporting.

When set to an integer n, numbering will only happen for
headlines whose relative level is higher or equal to n.

This option can also be set with the OPTIONS keyword,
e.g. \"num:t\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-select-tags '("export")
  "Tags that select a tree for export.

If any such tag is found in a buffer, all trees that do not carry
one of these tags will be ignored during export.  Inside trees
that are selected like this, you can still deselect a subtree by
tagging it with one of the `org-export-exclude-tags'.

This option can also be set with the SELECT_TAGS keyword."
  :group 'org-export-general
  :type '(repeat (string :tag "Tag")))

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
  :type 'boolean)

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
  :type 'boolean)

(defcustom org-export-with-statistics-cookies t
  "Non-nil means include statistics cookies in export.
This option can also be set with the OPTIONS keyword,
e.g. \"stat:nil\""
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

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
	  (const :tag "Do not interpret them" nil)))

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
	  (integer :tag "TOC to level")))

(defcustom org-export-with-tables t
  "If non-nil, lines starting with \"|\" define a table.
For example:

  | Name        | Address  | Birthday  |
  |-------------+----------+-----------|
  | Arthur Dent | England  | 29.2.2100 |

This option can also be set with the OPTIONS keyword,
e.g. \"|:nil\"."
  :group 'org-export-general
  :type 'boolean)

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
	  (const :tag "On" t)))

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
		  (string :tag "Keyword"))))

(defcustom org-export-time-stamp-file t
  "Non-nil means insert a time stamp into the exported file.
The time stamp shows when the file was created. This option can
also be set with the OPTIONS keyword, e.g. \"timestamp:nil\"."
  :group 'org-export-general
  :type 'boolean)

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
	  (const :tag "No timestamp" nil)))

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

(defcustom org-export-snippet-translation-alist nil
  "Alist between export snippets back-ends and exporter back-ends.

This variable allows to provide shortcuts for export snippets.

For example, with a value of '\(\(\"h\" . \"html\"\)\), the
HTML back-end will recognize the contents of \"@@h:<b>@@\" as
HTML code while every other back-end will ignore it."
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(repeat
	  (cons (string :tag "Shortcut")
		(string :tag "Back-end"))))

(defcustom org-export-coding-system nil
  "Coding system for the exported file."
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'coding-system)

(defcustom org-export-copy-to-kill-ring 'if-interactive
  "Non-nil means pushing export output to the kill ring.
This variable is ignored during asynchronous export."
  :group 'org-export-general
  :version "24.3"
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
When Org exports to a file, the buffer visiting that file is ever
shown, but remains buried.  However, when exporting to
a temporary buffer, that buffer is popped up in a second window.
When this variable is nil, the buffer remains buried also in
these cases."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-in-background nil
  "Non-nil means export and publishing commands will run in background.
Results from an asynchronous export are never displayed
automatically.  But you can retrieve them with \\[org-export-stack]."
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-export-async-init-file user-init-file
  "File used to initialize external export process.
Value must be an absolute file name.  It defaults to user's
initialization file.  Though, a specific configuration makes the
process faster and the export more portable."
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(file :must-match t))

(defcustom org-export-dispatch-use-expert-ui nil
  "Non-nil means using a non-intrusive `org-export-dispatch'.
In that case, no help buffer is displayed.  Though, an indicator
for current export scope is added to the prompt (\"b\" when
output is restricted to body only, \"s\" when it is restricted to
the current subtree, \"v\" when only visible elements are
considered for export, \"f\" when publishing functions should be
passed the FORCE argument and \"a\" when the export should be
asynchronous).  Also, \[?] allows to switch back to standard
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

(defstruct (org-export-backend (:constructor org-export-create-backend)
			       (:copier nil))
  name parent transcoders options filters blocks menu)

(defun org-export-get-backend (name)
  "Return export back-end named after NAME.
NAME is a symbol.  Return nil if no such back-end is found."
  (catch 'found
    (dolist (b org-export--registered-backends)
      (when (eq (org-export-backend-name b) name)
	(throw 'found b)))))

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
  ;; Register dedicated export blocks in the parser.
  (dolist (name (org-export-backend-blocks backend))
    (add-to-list 'org-element-block-name-alist
		 (cons name 'org-element-export-block-parser)))
  ;; If a back-end with the same name as BACKEND is already
  ;; registered, replace it with BACKEND.  Otherwise, simply add
  ;; BACKEND to the list of registered back-ends.
  (let ((old (org-export-get-backend (org-export-backend-name backend))))
    (if old (setcar (memq old org-export--registered-backends) backend)
      (push backend org-export--registered-backends))))

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
returns options inherited from parent back-ends, if any."
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

  :export-block

    String, or list of strings, representing block names that
    will not be parsed.  This is used to specify blocks that will
    contain raw code specific to the back-end.  These blocks
    still have to be handled by the relative `export-block' type
    translator.

  :filters-alist

    Alist between filters and function, or list of functions,
    specific to the back-end.  See `org-export-filters-alist' for
    a list of all allowed filters.  Filters defined here
    shouldn't make a back-end test, as it may prevent back-ends
    derived from this one to behave properly.

  :menu-entry

    Menu entry for the export dispatcher.  It should be a list
    like:

      '(KEY DESCRIPTION-OR-ORDINAL ACTION-OR-MENU)

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

        '(KEY DESCRIPTION ACTION)

      where KEY, DESCRIPTION and ACTION are described above.

    Valid values include:

      '(?m \"My Special Back-end\" my-special-export-function)

      or

      '(?l \"Export to LaTeX\"
           \(?p \"As PDF file\" org-latex-export-to-pdf)
           \(?o \"As PDF file and open\"
               \(lambda (a s v b)
                 \(if a (org-latex-export-to-pdf t s v b)
                   \(org-open-file
                    \(org-latex-export-to-pdf nil s v b)))))))

      or the following, which will be added to the previous
      sub-menu,

      '(?l 1
          \((?B \"As TEX buffer (Beamer)\" org-beamer-export-as-latex)
           \(?P \"As PDF file (Beamer)\" org-beamer-export-to-pdf)))

  :options-alist

    Alist between back-end specific properties introduced in
    communication channel and how their value are acquired.  See
    `org-export-options-alist' for more information about
    structure of the values."
  (declare (indent 1))
  (let (blocks filters menu-entry options contents)
    (while (keywordp (car body))
      (case (pop body)
        (:export-block (let ((names (pop body)))
			 (setq blocks (if (consp names) (mapcar 'upcase names)
					(list (upcase names))))))
	(:filters-alist (setq filters (pop body)))
	(:menu-entry (setq menu-entry (pop body)))
        (:options-alist (setq options (pop body)))
        (t (pop body))))
    (org-export-register-backend
     (org-export-create-backend :name backend
				:transcoders transcoders
				:options options
				:filters filters
				:blocks blocks
				:menu menu-entry))))

(defun org-export-define-derived-backend (child parent &rest body)
  "Create a new back-end as a variant of an existing one.

CHILD is the name of the derived back-end.  PARENT is the name of
the parent back-end.

BODY can start with pre-defined keyword arguments.  The following
keywords are understood:

  :export-block

    String, or list of strings, representing block names that
    will not be parsed.  This is used to specify blocks that will
    contain raw code specific to the back-end.  These blocks
    still have to be handled by the relative `export-block' type
    translator.

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

  \(org-export-define-derived-backend 'my-latex 'latex
     :translate-alist '((template . my-latex-template-fun)))

The back-end could then be called with, for example:

  \(org-export-to-buffer 'my-latex \"*Test my-latex*\")"
  (declare (indent 2))
  (let (blocks filters menu-entry options transcoders contents)
    (while (keywordp (car body))
      (case (pop body)
	(:export-block (let ((names (pop body)))
			 (setq blocks (if (consp names) (mapcar 'upcase names)
					(list (upcase names))))))
        (:filters-alist (setq filters (pop body)))
	(:menu-entry (setq menu-entry (pop body)))
        (:options-alist (setq options (pop body)))
        (:translate-alist (setq transcoders (pop body)))
        (t (pop body))))
    (org-export-register-backend
     (org-export-create-backend :name child
				:parent parent
				:transcoders transcoders
				:options options
				:filters filters
				:blocks blocks
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
;;    just before export, by `org-export-collect-tree-properties'.
;;
;; Here is the full list of properties available during transcode
;; process, with their category and their value type.
;;
;; + `:author' :: Author's name.
;;   - category :: option
;;   - type :: string
;;
;; + `:back-end' :: Current back-end used for transcoding.
;;   - category :: tree
;;   - type :: symbol
;;
;; + `:creator' :: String to write as creation information.
;;   - category :: option
;;   - type :: string
;;
;; + `:date' :: String to use as date.
;;   - category :: option
;;   - type :: string
;;
;; + `:description' :: Description text for the current data.
;;   - category :: option
;;   - type :: string
;;
;; + `:email' :: Author's email.
;;   - category :: option
;;   - type :: string
;;
;; + `:exclude-tags' :: Tags for exclusion of subtrees from export
;;      process.
;;   - category :: option
;;   - type :: list of strings
;;
;; + `:export-options' :: List of export options available for current
;;      process.
;;   - category :: none
;;   - type :: list of symbols, among `subtree', `body-only' and
;;      `visible-only'.
;;
;; + `:exported-data' :: Hash table used for memoizing
;;     `org-export-data'.
;;   - category :: tree
;;   - type :: hash table
;;
;; + `:filetags' :: List of global tags for buffer.  Used by
;;   `org-export-get-tags' to get tags with inheritance.
;;   - category :: option
;;   - type :: list of strings
;;
;; + `:footnote-definition-alist' :: Alist between footnote labels and
;;      their definition, as parsed data.  Only non-inlined footnotes
;;      are represented in this alist.  Also, every definition isn't
;;      guaranteed to be referenced in the parse tree.  The purpose of
;;      this property is to preserve definitions from oblivion
;;      (i.e. when the parse tree comes from a part of the original
;;      buffer), it isn't meant for direct use in a back-end.  To
;;      retrieve a definition relative to a reference, use
;;      `org-export-get-footnote-definition' instead.
;;   - category :: option
;;   - type :: alist (STRING . LIST)
;;
;; + `:headline-levels' :: Maximum level being exported as an
;;      headline.  Comparison is done with the relative level of
;;      headlines in the parse tree, not necessarily with their
;;      actual level.
;;   - category :: option
;;   - type :: integer
;;
;; + `:headline-offset' :: Difference between relative and real level
;;      of headlines in the parse tree.  For example, a value of -1
;;      means a level 2 headline should be considered as level
;;      1 (cf. `org-export-get-relative-level').
;;   - category :: tree
;;   - type :: integer
;;
;; + `:headline-numbering' :: Alist between headlines and their
;;      numbering, as a list of numbers
;;      (cf. `org-export-get-headline-number').
;;   - category :: tree
;;   - type :: alist (INTEGER . LIST)
;;
;; + `:id-alist' :: Alist between ID strings and destination file's
;;      path, relative to current directory.  It is used by
;;      `org-export-resolve-id-link' to resolve ID links targeting an
;;      external file.
;;   - category :: option
;;   - type :: alist (STRING . STRING)
;;
;; + `:ignore-list' :: List of elements and objects that should be
;;      ignored during export.
;;   - category :: tree
;;   - type :: list of elements and objects
;;
;; + `:input-file' :: Full path to input file, if any.
;;   - category :: option
;;   - type :: string or nil
;;
;; + `:keywords' :: List of keywords attached to data.
;;   - category :: option
;;   - type :: string
;;
;; + `:language' :: Default language used for translations.
;;   - category :: option
;;   - type :: string
;;
;; + `:parse-tree' :: Whole parse tree, available at any time during
;;      transcoding.
;;   - category :: option
;;   - type :: list (as returned by `org-element-parse-buffer')
;;
;; + `:preserve-breaks' :: Non-nil means transcoding should preserve
;;      all line breaks.
;;   - category :: option
;;   - type :: symbol (nil, t)
;;
;; + `:section-numbers' :: Non-nil means transcoding should add
;;      section numbers to headlines.
;;   - category :: option
;;   - type :: symbol (nil, t)
;;
;; + `:select-tags' :: List of tags enforcing inclusion of sub-trees
;;      in transcoding.  When such a tag is present, subtrees without
;;      it are de facto excluded from the process.  See
;;      `use-select-tags'.
;;   - category :: option
;;   - type :: list of strings
;;
;; + `:time-stamp-file' :: Non-nil means transcoding should insert
;;      a time stamp in the output.
;;   - category :: option
;;   - type :: symbol (nil, t)
;;
;; + `:translate-alist' :: Alist between element and object types and
;;      transcoding functions relative to the current back-end.
;;      Special keys `inner-template', `template' and `plain-text' are
;;      also possible.
;;   - category :: option
;;   - type :: alist (SYMBOL . FUNCTION)
;;
;; + `:with-archived-trees' :: Non-nil when archived subtrees should
;;      also be transcoded.  If it is set to the `headline' symbol,
;;      only the archived headline's name is retained.
;;   - category :: option
;;   - type :: symbol (nil, t, `headline')
;;
;; + `:with-author' :: Non-nil means author's name should be included
;;      in the output.
;;   - category :: option
;;   - type :: symbol (nil, t)
;;
;; + `:with-clocks' :: Non-nil means clock keywords should be exported.
;;   - category :: option
;;   - type :: symbol (nil, t)
;;
;; + `:with-creator' :: Non-nil means a creation sentence should be
;;      inserted at the end of the transcoded string.  If the value
;;      is `comment', it should be commented.
;;   - category :: option
;;   - type :: symbol (`comment', nil, t)
;;
;; + `:with-date' :: Non-nil means output should contain a date.
;;   - category :: option
;;   - type :. symbol (nil, t)
;;
;; + `:with-drawers' :: Non-nil means drawers should be exported.  If
;;      its value is a list of names, only drawers with such names
;;      will be transcoded.  If that list starts with `not', drawer
;;      with these names will be skipped.
;;   - category :: option
;;   - type :: symbol (nil, t) or list of strings
;;
;; + `:with-email' :: Non-nil means output should contain author's
;;                   email.
;;   - category :: option
;;   - type :: symbol (nil, t)
;;
;; + `:with-emphasize' :: Non-nil means emphasized text should be
;;      interpreted.
;;   - category :: option
;;   - type :: symbol (nil, t)
;;
;; + `:with-fixed-width' :: Non-nil if transcoder should interpret
;;      strings starting with a colon as a fixed-with (verbatim) area.
;;   - category :: option
;;   - type :: symbol (nil, t)
;;
;; + `:with-footnotes' :: Non-nil if transcoder should interpret
;;      footnotes.
;;   - category :: option
;;   - type :: symbol (nil, t)
;;
;; + `:with-latex' :: Non-nil means `latex-environment' elements and
;;    `latex-fragment' objects should appear in export output.  When
;;    this property is set to `verbatim', they will be left as-is.
;;   - category :: option
;;   - type :: symbol (`verbatim', nil, t)
;;
;; + `:with-planning' :: Non-nil means transcoding should include
;;      planning info.
;;   - category :: option
;;   - type :: symbol (nil, t)
;;
;; + `:with-priority' :: Non-nil means transcoding should include
;;      priority cookies.
;;   - category :: option
;;   - type :: symbol (nil, t)
;;
;; + `:with-smart-quotes' :: Non-nil means activate smart quotes in
;;      plain text.
;;   - category :: option
;;   - type :: symbol (nil, t)
;;
;; + `:with-special-strings' :: Non-nil means transcoding should
;;      interpret special strings in plain text.
;;   - category :: option
;;   - type :: symbol (nil, t)
;;
;; + `:with-sub-superscript' :: Non-nil means transcoding should
;;      interpret subscript and superscript.  With a value of "{}",
;;      only interpret those using curly brackets.
;;   - category :: option
;;   - type :: symbol (nil, {}, t)
;;
;; + `:with-tables' :: Non-nil means transcoding should interpret
;;      tables.
;;   - category :: option
;;   - type :: symbol (nil, t)
;;
;; + `:with-tags' :: Non-nil means transcoding should keep tags in
;;      headlines.  A `not-in-toc' value will remove them from the
;;      table of contents, if any, nonetheless.
;;   - category :: option
;;   - type :: symbol (nil, t, `not-in-toc')
;;
;; + `:with-tasks' :: Non-nil means transcoding should include
;;      headlines with a TODO keyword.  A `todo' value will only
;;      include headlines with a todo type keyword while a `done'
;;      value will do the contrary.  If a list of strings is provided,
;;      only tasks with keywords belonging to that list will be kept.
;;   - category :: option
;;   - type :: symbol (t, todo, done, nil) or list of strings
;;
;; + `:with-timestamps' :: Non-nil means transcoding should include
;;      time stamps.  Special value `active' (resp. `inactive') ask to
;;      export only active (resp. inactive) timestamps.  Otherwise,
;;      completely remove them.
;;   - category :: option
;;   - type :: symbol: (`active', `inactive', t, nil)
;;
;; + `:with-toc' :: Non-nil means that a table of contents has to be
;;      added to the output.  An integer value limits its depth.
;;   - category :: option
;;   - type :: symbol (nil, t or integer)
;;
;; + `:with-todo-keywords' :: Non-nil means transcoding should
;;      include TODO keywords.
;;   - category :: option
;;   - type :: symbol (nil, t)


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
    (org-set-local (car pair) (nth 1 pair)))
  ;; Get and prioritize export options...
  (org-combine-plists
   ;; ... from global variables...
   (org-export--get-global-options backend)
   ;; ... from an external property list...
   ext-plist
   ;; ... from in-buffer settings...
   (org-export--get-inbuffer-options backend)
   ;; ... and from subtree, when appropriate.
   (and subtreep (org-export--get-subtree-options backend))
   ;; Eventually add misc. properties.
   (list
    :back-end
    backend
    :translate-alist (org-export-get-all-transcoders backend)
    :footnote-definition-alist
    ;; Footnotes definitions must be collected in the original
    ;; buffer, as there's no insurance that they will still be in
    ;; the parse tree, due to possible narrowing.
    (let (alist)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward org-footnote-definition-re nil t)
	 (let ((def (save-match-data (org-element-at-point))))
	   (when (eq (org-element-type def) 'footnote-definition)
	     (push
	      (cons (org-element-property :label def)
		    (let ((cbeg (org-element-property :contents-begin def)))
		      (when cbeg
			(org-element--parse-elements
			 cbeg (org-element-property :contents-end def)
			 nil nil nil nil (list 'org-data nil)))))
	      alist))))
       alist))
    :id-alist
    ;; Collect id references.
    (let (alist)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward "\\[\\[id:\\S-+?\\]" nil t)
	 (let ((link (org-element-context)))
	   (when (eq (org-element-type link) 'link)
	     (let* ((id (org-element-property :path link))
		    (file (org-id-find-id-file id)))
	       (when file
		 (push (cons id (file-relative-name file)) alist)))))))
      alist))))

(defun org-export--parse-option-keyword (options &optional backend)
  "Parse an OPTIONS line and return values as a plist.
Optional argument BACKEND is an export back-end, as returned by,
e.g., `org-export-create-backend'.  It specifies which back-end
specific items to read, if any."
  (let* ((all
	  ;; Priority is given to back-end specific options.
	  (append (and backend (org-export-get-all-options backend))
		  org-export-options-alist))
	 plist)
    (dolist (option all)
      (let ((property (car option))
	    (item (nth 2 option)))
	(when (and item
		   (not (plist-member plist property))
		   (string-match (concat "\\(\\`\\|[ \t]\\)"
					 (regexp-quote item)
					 ":\\(([^)\n]+)\\|[^ \t\n\r;,.]*\\)")
				 options))
	  (setq plist (plist-put plist
				 property
				 (car (read-from-string
				       (match-string 2 options))))))))
    plist))

(defun org-export--get-subtree-options (&optional backend)
  "Get export options in subtree at point.
Optional argument BACKEND is an export back-end, as returned by,
e.g., `org-export-create-backend'.  It specifies back-end used
for export.  Return options as a plist."
  ;; For each buffer keyword, create a headline property setting the
  ;; same property in communication channel. The name for the property
  ;; is the keyword with "EXPORT_" appended to it.
  (org-with-wide-buffer
   (let (prop plist)
     ;; Make sure point is at a heading.
     (if (org-at-heading-p) (org-up-heading-safe) (org-back-to-heading t))
     ;; Take care of EXPORT_TITLE. If it isn't defined, use headline's
     ;; title (with no todo keyword, priority cookie or tag) as its
     ;; fallback value.
     (when (setq prop (or (org-entry-get (point) "EXPORT_TITLE")
			  (progn (looking-at org-complex-heading-regexp)
				 (org-match-string-no-properties 4))))
       (setq plist
	     (plist-put
	      plist :title
	      (org-element-parse-secondary-string
	       prop (org-element-restriction 'keyword)))))
     ;; EXPORT_OPTIONS are parsed in a non-standard way.
     (when (setq prop (org-entry-get (point) "EXPORT_OPTIONS"))
       (setq plist
	     (nconc plist (org-export--parse-option-keyword prop backend))))
     ;; Handle other keywords.  TITLE keyword is excluded as it has
     ;; been handled already.
     (let ((seen '("TITLE")))
       (mapc
	(lambda (option)
	  (let ((property (car option))
		(keyword (nth 1 option)))
	    (when (and keyword (not (member keyword seen)))
	      (let* ((subtree-prop (concat "EXPORT_" keyword))
		     ;; Export properties are not case-sensitive.
		     (value (let ((case-fold-search t))
			      (org-entry-get (point) subtree-prop))))
		(push keyword seen)
		(when (and value (not (plist-member plist property)))
		  (setq plist
			(plist-put
			 plist
			 property
			 (cond
			  ;; Parse VALUE if required.
			  ((member keyword org-element-document-properties)
			   (org-element-parse-secondary-string
			    value (org-element-restriction 'keyword)))
			  ;; If BEHAVIOR is `split' expected value is
			  ;; a list of strings, not a string.
			  ((eq (nth 4 option) 'split) (org-split-string value))
			  (t value)))))))))
	;; Look for both general keywords and back-end specific
	;; options, with priority given to the latter.
	(append (and backend (org-export-get-all-options backend))
		org-export-options-alist)))
     ;; Return value.
     plist)))

(defun org-export--get-inbuffer-options (&optional backend)
  "Return current buffer export options, as a plist.

Optional argument BACKEND, when non-nil, is an export back-end,
as returned by, e.g., `org-export-create-backend'.  It specifies
which back-end specific options should also be read in the
process.

Assume buffer is in Org mode.  Narrowing, if any, is ignored."
  (let* (plist
	 get-options			; For byte-compiler.
	 (case-fold-search t)
	 (options (append
		   ;; Priority is given to back-end specific options.
		   (and backend (org-export-get-all-options backend))
		   org-export-options-alist))
	 (regexp (format "^[ \t]*#\\+%s:"
			 (regexp-opt (nconc (delq nil (mapcar 'cadr options))
					    org-export-special-keywords))))
	 (find-properties
	  (lambda (keyword)
	    ;; Return all properties associated to KEYWORD.
	    (let (properties)
	      (dolist (option options properties)
		(when (equal (nth 1 option) keyword)
		  (pushnew (car option) properties))))))
	 (get-options
	  (lambda (&optional files plist)
	    ;; Recursively read keywords in buffer.  FILES is a list
	    ;; of files read so far.  PLIST is the current property
	    ;; list obtained.
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
		       (let ((file (expand-file-name
				    (org-remove-double-quotes (org-trim val)))))
			 ;; Avoid circular dependencies.
			 (unless (member file files)
			   (with-temp-buffer
			     (insert (org-file-contents file 'noerror))
			     (let ((org-inhibit-startup t)) (org-mode))
			     (setq plist (funcall get-options
						  (cons file files) plist))))))
		      ((equal key "OPTIONS")
		       (setq plist
			     (org-combine-plists
			      plist
			      (org-export--parse-option-keyword val backend))))
		      ((equal key "FILETAGS")
		       (setq plist
			     (org-combine-plists
			      plist
			      (list :filetags
				    (org-uniquify
				     (append (org-split-string val ":")
					     (plist-get plist :filetags)))))))
		      (t
		       ;; Options in `org-export-options-alist'.
		       (dolist (property (funcall find-properties key))
			 (let ((behaviour (nth 4 (assq property options))))
			   (setq plist
				 (plist-put
				  plist property
				  ;; Handle value depending on specified
				  ;; BEHAVIOR.
				  (case behaviour
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
					     ,@(org-split-string val)))
				    ('t val)
				    (otherwise
				     (if (not (plist-member plist property)) val
				       (plist-get plist property))))))))))))))
	     ;; Return final value.
	     plist))))
    ;; Read options in the current buffer.
    (setq plist (funcall get-options
			 (and buffer-file-name (list buffer-file-name)) nil))
    ;; Parse keywords specified in `org-element-document-properties'
    ;; and return PLIST.
    (dolist (keyword org-element-document-properties plist)
      (dolist (property (funcall find-properties keyword))
	(let ((value (plist-get plist property)))
	  (when (stringp value)
	    (setq plist
		  (plist-put plist property
			     (org-element-parse-secondary-string
			      value (org-element-restriction 'keyword))))))))))

(defun org-export--get-buffer-attributes ()
  "Return properties related to buffer attributes, as a plist."
  ;; Store full path of input file name, or nil.  For internal use.
  (let ((visited-file (buffer-file-name (buffer-base-buffer))))
    (list :input-file visited-file
	  :title (if (not visited-file) (buffer-name (buffer-base-buffer))
		   (file-name-sans-extension
		    (file-name-nondirectory visited-file))))))

(defun org-export--get-global-options (&optional backend)
  "Return global export options as a plist.
Optional argument BACKEND, if non-nil, is an export back-end, as
returned by, e.g., `org-export-create-backend'.  It specifies
which back-end specific export options should also be read in the
process."
  (let (plist
	;; Priority is given to back-end specific options.
	(all (append (and backend (org-export-get-all-options backend))
		     org-export-options-alist)))
    (dolist (cell all plist)
      (let ((prop (car cell))
	    (default-value (nth 3 cell)))
	(unless (or (not default-value) (plist-member plist prop))
	  (setq plist
		(plist-put
		 plist
		 prop
		 ;; Eval default value provided.  If keyword is
		 ;; a member of `org-element-document-properties',
		 ;; parse it as a secondary string before storing it.
		 (let ((value (eval (nth 3 cell))))
		   (if (not (stringp value)) value
		     (let ((keyword (nth 1 cell)))
		       (if (member keyword org-element-document-properties)
			   (org-element-parse-secondary-string
			    value (org-element-restriction 'keyword))
			 value)))))))))))

(defun org-export--list-bound-variables ()
  "Return variables bound from BIND keywords in current buffer.
Also look for BIND keywords in setup files.  The return value is
an alist where associations are (VARIABLE-NAME VALUE)."
  (when org-export-allow-bind-keywords
    (let* (collect-bind			; For byte-compiler.
	   (collect-bind
	    (lambda (files alist)
	      ;; Return an alist between variable names and their
	      ;; value.  FILES is a list of setup files names read so
	      ;; far, used to avoid circular dependencies.  ALIST is
	      ;; the alist collected so far.
	      (let ((case-fold-search t))
		(org-with-wide-buffer
		 (goto-char (point-min))
		 (while (re-search-forward
			 "^[ \t]*#\\+\\(BIND\\|SETUPFILE\\):" nil t)
		   (let ((element (org-element-at-point)))
		     (when (eq (org-element-type element) 'keyword)
		       (let ((val (org-element-property :value element)))
			 (if (equal (org-element-property :key element) "BIND")
			     (push (read (format "(%s)" val)) alist)
			   ;; Enter setup file.
			   (let ((file (expand-file-name
					(org-remove-double-quotes val))))
			     (unless (member file files)
			       (with-temp-buffer
				 (let ((org-inhibit-startup t)) (org-mode))
				 (insert (org-file-contents file 'noerror))
				 (setq alist
				       (funcall collect-bind
						(cons file files)
						alist))))))))))
		 alist)))))
      ;; Return value in appropriate order of appearance.
      (nreverse (funcall collect-bind nil nil)))))


;;;; Tree Properties
;;
;; Tree properties are information extracted from parse tree.  They
;; are initialized at the beginning of the transcoding process by
;; `org-export-collect-tree-properties'.
;;
;; Dedicated functions focus on computing the value of specific tree
;; properties during initialization.  Thus,
;; `org-export--populate-ignore-list' lists elements and objects that
;; should be skipped during export, `org-export--get-min-level' gets
;; the minimal exportable level, used as a basis to compute relative
;; level for headlines.  Eventually
;; `org-export--collect-headline-numbering' builds an alist between
;; headlines and their numbering.

(defun org-export-collect-tree-properties (data info)
  "Extract tree properties from parse tree.

DATA is the parse tree from which information is retrieved.  INFO
is a list holding export options.

Following tree properties are set or updated:

`:exported-data' Hash table used to memoize results from
                 `org-export-data'.

`:footnote-definition-alist' List of footnotes definitions in
                   original buffer and current parse tree.

`:headline-offset' Offset between true level of headlines and
		   local level.  An offset of -1 means a headline
		   of level 2 should be considered as a level
		   1 headline in the context.

`:headline-numbering' Alist of all headlines as key an the
		      associated numbering as value.

`:ignore-list'     List of elements that should be ignored during
                   export.

Return updated plist."
  ;; Install the parse tree in the communication channel, in order to
  ;; use `org-export-get-genealogy' and al.
  (setq info (plist-put info :parse-tree data))
  ;; Get the list of elements and objects to ignore, and put it into
  ;; `:ignore-list'.  Do not overwrite any user ignore that might have
  ;; been done during parse tree filtering.
  (setq info
	(plist-put info
		   :ignore-list
		   (append (org-export--populate-ignore-list data info)
			   (plist-get info :ignore-list))))
  ;; Compute `:headline-offset' in order to be able to use
  ;; `org-export-get-relative-level'.
  (setq info
	(plist-put info
		   :headline-offset
		   (- 1 (org-export--get-min-level data info))))
  ;; Update footnotes definitions list with definitions in parse tree.
  ;; This is required since buffer expansion might have modified
  ;; boundaries of footnote definitions contained in the parse tree.
  ;; This way, definitions in `footnote-definition-alist' are bound to
  ;; match those in the parse tree.
  (let ((defs (plist-get info :footnote-definition-alist)))
    (org-element-map data 'footnote-definition
      (lambda (fn)
	(push (cons (org-element-property :label fn)
		    `(org-data nil ,@(org-element-contents fn)))
	      defs)))
    (setq info (plist-put info :footnote-definition-alist defs)))
  ;; Properties order doesn't matter: get the rest of the tree
  ;; properties.
  (nconc
   `(:headline-numbering ,(org-export--collect-headline-numbering data info)
     :exported-data ,(make-hash-table :test 'eq :size 4001))
   info))

(defun org-export--get-min-level (data options)
  "Return minimum exportable headline's level in DATA.
DATA is parsed tree as returned by `org-element-parse-buffer'.
OPTIONS is a plist holding export options."
  (catch 'exit
    (let ((min-level 10000))
      (mapc
       (lambda (blob)
	 (when (and (eq (org-element-type blob) 'headline)
		    (not (org-element-property :footnote-section-p blob))
		    (not (memq blob (plist-get options :ignore-list))))
	   (setq min-level (min (org-element-property :level blob) min-level)))
	 (when (= min-level 1) (throw 'exit 1)))
       (org-element-contents data))
      ;; If no headline was found, for the sake of consistency, set
      ;; minimum level to 1 nonetheless.
      (if (= min-level 10000) 1 min-level))))

(defun org-export--collect-headline-numbering (data options)
  "Return numbering of all exportable headlines in a parse tree.

DATA is the parse tree.  OPTIONS is the plist holding export
options.

Return an alist whose key is a headline and value is its
associated numbering \(in the shape of a list of numbers\) or nil
for a footnotes section."
  (let ((numbering (make-vector org-export-max-depth 0)))
    (org-element-map data 'headline
      (lambda (headline)
	(unless (org-element-property :footnote-section-p headline)
	  (let ((relative-level
		 (1- (org-export-get-relative-level headline options))))
	    (cons
	     headline
	     (loop for n across numbering
		   for idx from 0 to org-export-max-depth
		   when (< idx relative-level) collect n
		   when (= idx relative-level) collect (aset numbering idx (1+ n))
		   when (> idx relative-level) do (aset numbering idx 0))))))
      options)))

(defun org-export--populate-ignore-list (data options)
  "Return list of elements and objects to ignore during export.
DATA is the parse tree to traverse.  OPTIONS is the plist holding
export options."
  (let* (ignore
	 walk-data
	 ;; First find trees containing a select tag, if any.
	 (selected (org-export--selected-trees data options))
	 (walk-data
	  (lambda (data)
	    ;; Collect ignored elements or objects into IGNORE-LIST.
	    (let ((type (org-element-type data)))
	      (if (org-export--skip-p data options selected) (push data ignore)
		(if (and (eq type 'headline)
			 (eq (plist-get options :with-archived-trees) 'headline)
			 (org-element-property :archivedp data))
		    ;; If headline is archived but tree below has
		    ;; to be skipped, add it to ignore list.
		    (mapc (lambda (e) (push e ignore))
			  (org-element-contents data))
		  ;; Move into secondary string, if any.
		  (let ((sec-prop
			 (cdr (assq type org-element-secondary-value-alist))))
		    (when sec-prop
		      (mapc walk-data (org-element-property sec-prop data))))
		  ;; Move into recursive objects/elements.
		  (mapc walk-data (org-element-contents data))))))))
    ;; Main call.
    (funcall walk-data data)
    ;; Return value.
    ignore))

(defun org-export--selected-trees (data info)
  "Return list of headlines and inlinetasks with a select tag in their tree.
DATA is parsed data as returned by `org-element-parse-buffer'.
INFO is a plist holding export options."
  (let* (selected-trees
	 walk-data			; For byte-compiler.
	 (walk-data
	  (function
	   (lambda (data genealogy)
	     (let ((type (org-element-type data)))
	       (cond
		((memq type '(headline inlinetask))
		 (let ((tags (org-element-property :tags data)))
		   (if (loop for tag in (plist-get info :select-tags)
			     thereis (member tag tags))
		       ;; When a select tag is found, mark full
		       ;; genealogy and every headline within the tree
		       ;; as acceptable.
		       (setq selected-trees
			     (append
			      genealogy
			      (org-element-map data '(headline inlinetask)
				'identity)
			      selected-trees))
		     ;; If at a headline, continue searching in tree,
		     ;; recursively.
		     (when (eq type 'headline)
		       (mapc (lambda (el)
			       (funcall walk-data el (cons data genealogy)))
			     (org-element-contents data))))))
		((or (eq type 'org-data)
		     (memq type org-element-greater-elements))
		 (mapc (lambda (el) (funcall walk-data el genealogy))
		       (org-element-contents data)))))))))
    (funcall walk-data data nil)
    selected-trees))

(defun org-export--skip-p (blob options selected)
  "Non-nil when element or object BLOB should be skipped during export.
OPTIONS is the plist holding export options.  SELECTED, when
non-nil, is a list of headlines or inlinetasks belonging to
a tree with a select tag."
  (case (org-element-type blob)
    (clock (not (plist-get options :with-clocks)))
    (drawer
     (let ((with-drawers-p (plist-get options :with-drawers)))
       (or (not with-drawers-p)
	   (and (consp with-drawers-p)
		;; If `:with-drawers' value starts with `not', ignore
		;; every drawer whose name belong to that list.
		;; Otherwise, ignore drawers whose name isn't in that
		;; list.
		(let ((name (org-element-property :drawer-name blob)))
		  (if (eq (car with-drawers-p) 'not)
		      (member-ignore-case name (cdr with-drawers-p))
		    (not (member-ignore-case name with-drawers-p))))))))
    ((footnote-definition footnote-reference)
     (not (plist-get options :with-footnotes)))
    ((headline inlinetask)
     (let ((with-tasks (plist-get options :with-tasks))
	   (todo (org-element-property :todo-keyword blob))
	   (todo-type (org-element-property :todo-type blob))
	   (archived (plist-get options :with-archived-trees))
	   (tags (org-element-property :tags blob)))
       (or
	(and (eq (org-element-type blob) 'inlinetask)
	     (not (plist-get options :with-inlinetasks)))
	;; Ignore subtrees with an exclude tag.
	(loop for k in (plist-get options :exclude-tags)
	      thereis (member k tags))
	;; When a select tag is present in the buffer, ignore any tree
	;; without it.
	(and selected (not (memq blob selected)))
	;; Ignore commented sub-trees.
	(org-element-property :commentedp blob)
	;; Ignore archived subtrees if `:with-archived-trees' is nil.
	(and (not archived) (org-element-property :archivedp blob))
	;; Ignore tasks, if specified by `:with-tasks' property.
	(and todo
	     (or (not with-tasks)
		 (and (memq with-tasks '(todo done))
		      (not (eq todo-type with-tasks)))
		 (and (consp with-tasks) (not (member todo with-tasks))))))))
    ((latex-environment latex-fragment) (not (plist-get options :with-latex)))
    (planning (not (plist-get options :with-planning)))
    (statistics-cookie (not (plist-get options :with-statistics-cookies)))
    (table-cell
     (and (org-export-table-has-special-column-p
	   (org-export-get-parent-table blob))
	  (not (org-export-get-previous-element blob options))))
    (table-row (org-export-table-row-is-special-p blob options))
    (timestamp
     ;; `:with-timestamps' only applies to isolated timestamps
     ;; objects, i.e. timestamp objects in a paragraph containing only
     ;; timestamps and whitespaces.
     (when (let ((parent (org-export-get-parent-element blob)))
	     (and (memq (org-element-type parent) '(paragraph verse-block))
		  (not (org-element-map parent
			   (cons 'plain-text
				 (remq 'timestamp org-element-all-objects))
			 (lambda (obj)
			   (or (not (stringp obj)) (org-string-nw-p obj)))
			 options t))))
       (case (plist-get options :with-timestamps)
	 ('nil t)
	 (active
	  (not (memq (org-element-property :type blob) '(active active-range))))
	 (inactive
	  (not (memq (org-element-property :type blob)
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
;; Internally, three functions handle the filtering of objects and
;; elements during the export.  In particular,
;; `org-export-ignore-element' marks an element or object so future
;; parse tree traversals skip it, `org-export--interpret-p' tells which
;; elements or objects should be seen as real Org syntax and
;; `org-export-expand' transforms the others back into their original
;; shape
;;
;; `org-export-transcoder' is an accessor returning appropriate
;; translator function for a given element or object.

(defun org-export-transcoder (blob info)
  "Return appropriate transcoder for BLOB.
INFO is a plist containing export directives."
  (let ((type (org-element-type blob)))
    ;; Return contents only for complete parse trees.
    (if (eq type 'org-data) (lambda (blob contents info) contents)
      (let ((transcoder (cdr (assq type (plist-get info :translate-alist)))))
	(and (functionp transcoder) transcoder)))))

(defun org-export-data (data info)
  "Convert DATA into current back-end format.

DATA is a parse tree, an element or an object or a secondary
string.  INFO is a plist holding export options.

Return transcoded string."
  (let ((memo (gethash data (plist-get info :exported-data) 'no-memo)))
    (if (not (eq memo 'no-memo)) memo
      (let* ((type (org-element-type data))
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
	       ;; Uninterpreted element/object: change it back to Org
	       ;; syntax and export again resulting raw string.
	       ((not (org-export--interpret-p data info))
		(org-export-data
		 (org-export-expand
		  data
		  (mapconcat (lambda (blob) (org-export-data blob info))
			     (org-element-contents data)
			     ""))
		 info))
	       ;; Secondary string.
	       ((not type)
		(mapconcat (lambda (obj) (org-export-data obj info)) data ""))
	       ;; Element/Object without contents or, as a special case,
	       ;; headline with archive tag and archived trees restricted
	       ;; to title only.
	       ((or (not (org-element-contents data))
		    (and (eq type 'headline)
			 (eq (plist-get info :with-archived-trees) 'headline)
			 (org-element-property :archivedp data)))
		(let ((transcoder (org-export-transcoder data info)))
		  (or (and (functionp transcoder)
			   (funcall transcoder data nil info))
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
				;; Elements directly containing objects
				;; must have their indentation normalized
				;; first.
				(org-element-normalize-contents
				 data
				 ;; When normalizing contents of the first
				 ;; paragraph in an item or a footnote
				 ;; definition, ignore first line's
				 ;; indentation: there is none and it
				 ;; might be misleading.
				 (when (eq type 'paragraph)
				   (let ((parent (org-export-get-parent data)))
				     (and
				      (eq (car (org-element-contents parent))
					  data)
				      (memq (org-element-type parent)
					    '(footnote-definition item))))))))
			     "")))
		      (funcall transcoder data
			       (if (not greaterp) contents
				 (org-element-normalize-string contents))
			       info))))))))
	;; Final result will be memoized before being returned.
	(puthash
	 data
	 (cond
	  ((not results) nil)
	  ((memq type '(org-data plain-text nil)) results)
	  ;; Append the same white space between elements or objects as in
	  ;; the original buffer, and call appropriate filters.
	  (t
	   (let ((results
		  (org-export-filter-apply-functions
		   (plist-get info (intern (format ":filter-%s" type)))
		   (let ((post-blank (or (org-element-property :post-blank data)
					 0)))
		     (if (memq type org-element-all-elements)
			 (concat (org-element-normalize-string results)
				 (make-string post-blank ?\n))
		       (concat results (make-string post-blank ? ))))
		   info)))
	     results)))
	 (plist-get info :exported-data))))))

(defun org-export-data-with-backend (data backend info)
  "Convert DATA into BACKEND format.

DATA is an element, an object, a secondary string or a string.
BACKEND is a symbol.  INFO is a plist used as a communication
channel.

Unlike to `org-export-with-backend', this function will
recursively convert DATA using BACKEND translation table."
  (when (symbolp backend) (setq backend (org-export-get-backend backend)))
  (org-export-data
   data
   ;; Set-up a new communication channel with translations defined in
   ;; BACKEND as the translate table and a new hash table for
   ;; memoization.
   (org-combine-plists
    info
    (list :back-end backend
	  :translate-alist (org-export-get-all-transcoders backend)
	  ;; Size of the hash table is reduced since this function
	  ;; will probably be used on small trees.
	  :exported-data (make-hash-table :test 'eq :size 401)))))

(defun org-export--interpret-p (blob info)
  "Non-nil if element or object BLOB should be interpreted during export.
If nil, BLOB will appear as raw Org syntax.  Check is done
according to export options INFO, stored as a plist."
  (case (org-element-type blob)
    ;; ... entities...
    (entity (plist-get info :with-entities))
    ;; ... emphasis...
    ((bold italic strike-through underline)
     (plist-get info :with-emphasize))
    ;; ... fixed-width areas.
    (fixed-width (plist-get info :with-fixed-width))
    ;; ... LaTeX environments and fragments...
    ((latex-environment latex-fragment)
     (let ((with-latex-p (plist-get info :with-latex)))
       (and with-latex-p (not (eq with-latex-p 'verbatim)))))
    ;; ... sub/superscripts...
    ((subscript superscript)
     (let ((sub/super-p (plist-get info :with-sub-superscript)))
       (if (eq sub/super-p '{})
	   (org-element-property :use-brackets-p blob)
	 sub/super-p)))
    ;; ... tables...
    (table (plist-get info :with-tables))
    (otherwise t)))

(defun org-export-expand (blob contents &optional with-affiliated)
  "Expand a parsed element or object to its original state.

BLOB is either an element or an object.  CONTENTS is its
contents, as a string or nil.

When optional argument WITH-AFFILIATED is non-nil, add affiliated
keywords before output."
  (let ((type (org-element-type blob)))
    (concat (and with-affiliated (memq type org-element-all-elements)
		 (org-element--interpret-affiliated-keywords blob))
	    (funcall (intern (format "org-element-%s-interpreter" type))
		     blob contents))))

(defun org-export-ignore-element (element info)
  "Add ELEMENT to `:ignore-list' in INFO.

Any element in `:ignore-list' will be skipped when using
`org-element-map'.  INFO is modified by side effects."
  (plist-put info :ignore-list (cons element (plist-get info :ignore-list))))



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
;; - `:filter-final-output' applies to the final transcoded string.
;;   Users can set it with `org-export-filter-final-output-functions'
;;   variable
;;
;; - `:filter-plain-text' applies to any string not recognized as Org
;;   syntax.  `org-export-filter-plain-text-functions' allows users to
;;   configure it.
;;
;; - `:filter-TYPE' applies on the string returned after an element or
;;   object of type TYPE has been transcoded.  A user can modify
;;   `org-export-filter-TYPE-functions'
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

(defvar org-export-filter-comment-functions nil
  "List of functions applied to a transcoded comment.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

(defvar org-export-filter-comment-block-functions nil
  "List of functions applied to a transcoded comment-block.
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

(defvar org-export-filter-quote-section-functions nil
  "List of functions applied to a transcoded quote-section.
Each filter is called with three arguments: the transcoded data,
as a string, the back-end, as a symbol, and the communication
channel, as a plist.  It must return a string or nil.")

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

Functions are called with arguments VALUE, current export
back-end's name and INFO.  A function returning a nil value will
be skipped.  If it returns the empty string, the process ends and
VALUE is ignored.

Call is done in a LIFO fashion, to be sure that developer
specified filters, if any, are called first."
  (catch 'exit
    (let* ((backend (plist-get info :back-end))
	   (backend-name (and backend (org-export-backend-name backend))))
      (dolist (filter filters value)
	(let ((result (funcall filter value backend-name info)))
	  (cond ((not result) value)
		((equal value "") (throw 'exit nil))
		(t (setq value result))))))))

(defun org-export-install-filters (info)
  "Install filters properties in communication channel.
INFO is a plist containing the current communication channel.
Return the updated communication channel."
  (let (plist)
    ;; Install user-defined filters with `org-export-filters-alist'
    ;; and filters already in INFO (through ext-plist mechanism).
    (mapc (lambda (p)
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
	  org-export-filters-alist)
    ;; Prepend back-end specific filters to that list.
    (mapc (lambda (p)
	    ;; Single values get consed, lists are appended.
	    (let ((key (car p)) (value (cdr p)))
	      (when value
		(setq plist
		      (plist-put
		       plist key
		       (if (atom value) (cons value (plist-get plist key))
			 (append value (plist-get plist key))))))))
	  (org-export-get-all-filters (plist-get info :back-end)))
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
		   (and (not (eq var 'org-font-lock-keywords))
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
	     (mapc
	      (lambda (ov)
		(let ((invis-prop (overlay-get ov 'invisible)))
		  (when invis-prop
		    (push `(overlay-put
			    (make-overlay ,(overlay-start ov)
					  ,(overlay-end ov))
			    'invisible (quote ,invis-prop))
			  ov-set))))
	      (overlays-in (point-min) (point-max)))
	     ov-set)))))

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
      ;; parsing.  If parsing subtree, be sure to remove main headline
      ;; too.
      (cond ((org-region-active-p)
	     (narrow-to-region (region-beginning) (region-end)))
	    (subtreep
	     (org-narrow-to-subtree)
	     (goto-char (point-min))
	     (forward-line)
	     (narrow-to-region (point) (point-max))))
      ;; Initialize communication channel with original buffer
      ;; attributes, unavailable in its copy.
      (let* ((org-export-current-backend (org-export-backend-name backend))
	     (info (org-combine-plists
		    (list :export-options
			  (delq nil
				(list (and subtreep 'subtree)
				      (and visible-only 'visible-only)
				      (and body-only 'body-only))))
		    (org-export--get-buffer-attributes)))
	     tree)
	;; Update communication channel and get parse tree.  Buffer
	;; isn't parsed directly.  Instead, a temporary copy is
	;; created, where include keywords, macros are expanded and
	;; code blocks are evaluated.
	(org-export-with-buffer-copy
	 ;; Run first hook with current back-end's name as argument.
	 (run-hook-with-args 'org-export-before-processing-hook
			     (org-export-backend-name backend))
	 (org-export-expand-include-keyword)
	 ;; Update macro templates since #+INCLUDE keywords might have
	 ;; added some new ones.
	 (org-macro-initialize-templates)
	 (org-macro-replace-all org-macro-templates)
	 (org-export-execute-babel-code)
	 ;; Update radio targets since keyword inclusion might have
	 ;; added some more.
	 (org-update-radio-target-regexp)
	 ;; Run last hook with current back-end's name as argument.
	 (goto-char (point-min))
	 (save-excursion
	   (run-hook-with-args 'org-export-before-parsing-hook
			       (org-export-backend-name backend)))
	 ;; Update communication channel with environment.  Also
	 ;; install user's and developer's filters.
	 (setq info
	       (org-export-install-filters
		(org-combine-plists
		 info (org-export-get-environment backend subtreep ext-plist))))
	 ;; Expand export-specific set of macros: {{{author}}},
	 ;; {{{date}}}, {{{email}}} and {{{title}}}.  It must be done
	 ;; once regular macros have been expanded, since document
	 ;; keywords may contain one of them.
	 (org-macro-replace-all
	  (list (cons "author"
		      (org-element-interpret-data (plist-get info :author)))
		(cons "date"
		      (org-element-interpret-data (plist-get info :date)))
		;; EMAIL is not a parsed keyword: store it as-is.
		(cons "email" (or (plist-get info :email) ""))
		(cons "title"
		      (org-element-interpret-data (plist-get info :title)))))
	 ;; Call options filters and update export options.  We do not
	 ;; use `org-export-filter-apply-functions' here since the
	 ;; arity of such filters is different.
	 (let ((backend-name (org-export-backend-name backend)))
	   (dolist (filter (plist-get info :filter-options))
	     (let ((result (funcall filter info backend-name)))
	       (when result (setq info result)))))
	 ;; Parse buffer and call parse-tree filter on it.
	 (setq tree
	       (org-export-filter-apply-functions
		(plist-get info :filter-parse-tree)
		(org-element-parse-buffer nil visible-only) info))
	 ;; Now tree is complete, compute its properties and add them
	 ;; to communication channel.
	 (setq info
	       (org-combine-plists
		info (org-export-collect-tree-properties tree info)))
	 ;; Eventually transcode TREE.  Wrap the resulting string into
	 ;; a template.
	 (let* ((body (org-element-normalize-string
		       (or (org-export-data tree info) "")))
		(inner-template (cdr (assq 'inner-template
					   (plist-get info :translate-alist))))
		(full-body (if (not (functionp inner-template)) body
			     (funcall inner-template body info)))
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
  (if (not (org-region-active-p))
      (user-error "No active region to replace")
    (let* ((beg (region-beginning))
	   (end (region-end))
	   (str (buffer-substring beg end)) rpl)
      (setq rpl (org-export-string-as str backend t))
      (delete-region beg end)
      (insert rpl))))

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
			     org-export--registered-backends))))))
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
                               (mapconcat 'identity (eval (nth 3 entry)) " ")
                             (eval (nth 3 entry)))))
                      (push (cons keyword value) keywords))))
         (option (unless (assoc option options)
                   (push (cons option (eval (nth 3 entry))) options))))))
    ;; Move to an appropriate location in order to insert options.
    (unless subtreep (beginning-of-line))
    ;; First get TITLE, DATE, AUTHOR and EMAIL if they belong to the
    ;; list of available keywords.
    (when (assoc "TITLE" keywords)
      (let ((title
	     (or (let ((visited-file (buffer-file-name (buffer-base-buffer))))
		   (and visited-file
			(file-name-sans-extension
			 (file-name-nondirectory visited-file))))
		 (buffer-name (buffer-base-buffer)))))
	(if (not subtreep) (insert (format "#+TITLE: %s\n" title))
	  (org-entry-put node "EXPORT_TITLE" title))))
    (when (assoc "DATE" keywords)
      (let ((date (with-temp-buffer (org-insert-time-stamp (current-time)))))
	(if (not subtreep) (insert "#+DATE: " date "\n")
	  (org-entry-put node "EXPORT_DATE" date))))
    (when (assoc "AUTHOR" keywords)
      (let ((author (cdr (assoc "AUTHOR" keywords))))
	(if subtreep (org-entry-put node "EXPORT_AUTHOR" author)
	  (insert
	   (format "#+AUTHOR:%s\n"
		   (if (not (org-string-nw-p author)) ""
		     (concat " " author)))))))
    (when (assoc "EMAIL" keywords)
      (let ((email (cdr (assoc "EMAIL" keywords))))
	(if subtreep (org-entry-put node "EXPORT_EMAIL" email)
	  (insert
	   (format "#+EMAIL:%s\n"
		   (if (not (org-string-nw-p email)) ""
		     (concat " " email)))))))
    ;; Then (multiple) OPTIONS lines.  Never go past fill-column.
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
		  (incf width (1+ (length item))))))
	    (insert "\n")))))
    ;; And the rest of keywords.
    (dolist (key (sort keywords (lambda (k1 k2) (string< (car k1) (car k2)))))
      (unless (member (car key) '("TITLE" "DATE" "AUTHOR" "EMAIL"))
        (let ((val (cdr key)))
          (if subtreep (org-entry-put node (concat "EXPORT_" (car key)) val)
            (insert
             (format "#+%s:%s\n"
                     (car key)
                     (if (org-string-nw-p val) (format " %s" val) "")))))))))

(defun org-export-expand-include-keyword (&optional included dir)
  "Expand every include keyword in buffer.
Optional argument INCLUDED is a list of included file names along
with their line restriction, when appropriate.  It is used to
avoid infinite recursion.  Optional argument DIR is the current
working directory.  It is used to properly resolve relative
paths."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*#\\+INCLUDE:" nil t)
      (let ((element (save-match-data (org-element-at-point))))
	(when (eq (org-element-type element) 'keyword)
	  (beginning-of-line)
	  ;; Extract arguments from keyword's value.
	  (let* ((value (org-element-property :value element))
		 (ind (org-get-indentation))
		 (file (and (string-match
			     "^\\(\".+?\"\\|\\S-+\\)\\(?:\\s-+\\|$\\)" value)
			    (prog1 (expand-file-name
				    (org-remove-double-quotes
				     (match-string 1 value))
				    dir)
			      (setq value (replace-match "" nil nil value)))))
		 (lines
		  (and (string-match
			":lines +\"\\(\\(?:[0-9]+\\)?-\\(?:[0-9]+\\)?\\)\""
			value)
		       (prog1 (match-string 1 value)
			 (setq value (replace-match "" nil nil value)))))
		 (env (cond ((string-match "\\<example\\>" value) 'example)
			    ((string-match "\\<src\\(?: +\\(.*\\)\\)?" value)
			     (match-string 1 value))))
		 ;; Minimal level of included file defaults to the child
		 ;; level of the current headline, if any, or one.  It
		 ;; only applies is the file is meant to be included as
		 ;; an Org one.
		 (minlevel
		  (and (not env)
		       (if (string-match ":minlevel +\\([0-9]+\\)" value)
			   (prog1 (string-to-number (match-string 1 value))
			     (setq value (replace-match "" nil nil value)))
			 (let ((cur (org-current-level)))
			   (if cur (1+ (org-reduced-level cur)) 1))))))
	    ;; Remove keyword.
	    (delete-region (point) (progn (forward-line) (point)))
	    (cond
	     ((not file) nil)
	     ((not (file-readable-p file))
	      (error "Cannot include file %s" file))
	     ;; Check if files has already been parsed.  Look after
	     ;; inclusion lines too, as different parts of the same file
	     ;; can be included too.
	     ((member (list file lines) included)
	      (error "Recursive file inclusion: %s" file))
	     (t
	      (cond
	       ((eq env 'example)
		(insert
		 (let ((ind-str (make-string ind ? ))
		       (contents
			(org-escape-code-in-string
			 (org-export--prepare-file-contents file lines))))
		   (format "%s#+BEGIN_EXAMPLE\n%s%s#+END_EXAMPLE\n"
			   ind-str contents ind-str))))
	       ((stringp env)
		(insert
		 (let ((ind-str (make-string ind ? ))
		       (contents
			(org-escape-code-in-string
			 (org-export--prepare-file-contents file lines))))
		   (format "%s#+BEGIN_SRC %s\n%s%s#+END_SRC\n"
			   ind-str env contents ind-str))))
	       (t
		(insert
		 (with-temp-buffer
		   (let ((org-inhibit-startup t)) (org-mode))
		   (insert
		    (org-export--prepare-file-contents file lines ind minlevel))
		   (org-export-expand-include-keyword
		    (cons (list file lines) included)
		    (file-name-directory file))
		   (buffer-string)))))))))))))

(defun org-export--prepare-file-contents (file &optional lines ind minlevel)
  "Prepare the contents of FILE for inclusion and return them as a string.

When optional argument LINES is a string specifying a range of
lines, include only those lines.

Optional argument IND, when non-nil, is an integer specifying the
global indentation of returned contents.  Since its purpose is to
allow an included file to stay in the same environment it was
created \(i.e. a list item), it doesn't apply past the first
headline encountered.

Optional argument MINLEVEL, when non-nil, is an integer
specifying the level that any top-level headline in the included
file should have."
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
    (when ind
      (unless (eq major-mode 'org-mode)
	(let ((org-inhibit-startup t)) (org-mode)))
      (goto-char (point-min))
      (let ((ind-str (make-string ind ? )))
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
	   (let ((offset (- minlevel (apply 'min levels))))
	     (unless (zerop offset)
	       (when org-odd-levels-only (setq offset (* offset 2)))
	       ;; Only change stars, don't bother moving whole
	       ;; sections.
	       (org-map-entries
		(lambda () (if (< offset 0) (delete-char (abs offset))
			(insert (make-string offset ?*)))))))))))
    (org-element-normalize-string (buffer-string))))

(defun org-export-execute-babel-code ()
  "Execute every Babel code in the visible part of current buffer."
  ;; Get a pristine copy of current buffer so Babel references can be
  ;; properly resolved.
  (let ((reference (org-export-copy-buffer)))
    (unwind-protect (let ((org-current-export-file reference))
		      (org-babel-exp-process-buffer))
      (kill-buffer reference))))

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
    (if (memq type '(nil org-data)) (error "No foreign transcoder available")
      (let* ((all-transcoders (org-export-get-all-transcoders backend))
	     (transcoder (cdr (assq type all-transcoders))))
	(if (not (functionp transcoder))
	    (error "No foreign transcoder available")
	  (funcall
	   transcoder data contents
	   (org-combine-plists
	    info (list :back-end backend
		       :translate-alist all-transcoders
		       :exported-data (make-hash-table :test 'eq :size 401)))))))))


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

(defun org-export-collect-footnote-definitions (data info)
  "Return an alist between footnote numbers, labels and definitions.

DATA is the parse tree from which definitions are collected.
INFO is the plist used as a communication channel.

Definitions are sorted by order of references.  They either
appear as Org data or as a secondary string for inlined
footnotes.  Unreferenced definitions are ignored."
  (let* (num-alist
	 collect-fn			; for byte-compiler.
	 (collect-fn
	  (function
	   (lambda (data)
	     ;; Collect footnote number, label and definition in DATA.
	     (org-element-map data 'footnote-reference
	       (lambda (fn)
		 (when (org-export-footnote-first-reference-p fn info)
		   (let ((def (org-export-get-footnote-definition fn info)))
		     (push
		      (list (org-export-get-footnote-number fn info)
			    (org-element-property :label fn)
			    def)
		      num-alist)
		     ;; Also search in definition for nested footnotes.
		     (when (eq (org-element-property :type fn) 'standard)
		       (funcall collect-fn def)))))
	       ;; Don't enter footnote definitions since it will happen
	       ;; when their first reference is found.
	       info nil 'footnote-definition)))))
    (funcall collect-fn (plist-get info :parse-tree))
    (reverse num-alist)))

(defun org-export-footnote-first-reference-p (footnote-reference info)
  "Non-nil when a footnote reference is the first one for its label.

FOOTNOTE-REFERENCE is the footnote reference being considered.
INFO is the plist used as a communication channel."
  (let ((label (org-element-property :label footnote-reference)))
    ;; Anonymous footnotes are always a first reference.
    (if (not label) t
      ;; Otherwise, return the first footnote with the same LABEL and
      ;; test if it is equal to FOOTNOTE-REFERENCE.
      (let* (search-refs		; for byte-compiler.
	     (search-refs
	      (function
	       (lambda (data)
		 (org-element-map data 'footnote-reference
		   (lambda (fn)
		     (cond
		      ((string= (org-element-property :label fn) label)
		       (throw 'exit fn))
		      ;; If FN isn't inlined, be sure to traverse its
		      ;; definition before resuming search.  See
		      ;; comments in `org-export-get-footnote-number'
		      ;; for more information.
		      ((eq (org-element-property :type fn) 'standard)
		       (funcall search-refs
				(org-export-get-footnote-definition fn info)))))
		   ;; Don't enter footnote definitions since it will
		   ;; happen when their first reference is found.
		   info 'first-match 'footnote-definition)))))
	(eq (catch 'exit (funcall search-refs (plist-get info :parse-tree)))
	    footnote-reference)))))

(defun org-export-get-footnote-definition (footnote-reference info)
  "Return definition of FOOTNOTE-REFERENCE as parsed data.
INFO is the plist used as a communication channel.  If no such
definition can be found, return the \"DEFINITION NOT FOUND\"
string."
  (let ((label (org-element-property :label footnote-reference)))
    (or (org-element-property :inline-definition footnote-reference)
        (cdr (assoc label (plist-get info :footnote-definition-alist)))
	"DEFINITION NOT FOUND.")))

(defun org-export-get-footnote-number (footnote info)
  "Return number associated to a footnote.

FOOTNOTE is either a footnote reference or a footnote definition.
INFO is the plist used as a communication channel."
  (let* ((label (org-element-property :label footnote))
	 seen-refs
	 search-ref			; For byte-compiler.
	 (search-ref
	  (function
	   (lambda (data)
	     ;; Search footnote references through DATA, filling
	     ;; SEEN-REFS along the way.
	     (org-element-map data 'footnote-reference
	       (lambda (fn)
		 (let ((fn-lbl (org-element-property :label fn)))
		   (cond
		    ;; Anonymous footnote match: return number.
		    ((and (not fn-lbl) (eq fn footnote))
		     (throw 'exit (1+ (length seen-refs))))
		    ;; Labels match: return number.
		    ((and label (string= label fn-lbl))
		     (throw 'exit (1+ (length seen-refs))))
		    ;; Anonymous footnote: it's always a new one.
		    ;; Also, be sure to return nil from the `cond' so
		    ;; `first-match' doesn't get us out of the loop.
		    ((not fn-lbl) (push 'inline seen-refs) nil)
		    ;; Label not seen so far: add it so SEEN-REFS.
		    ;;
		    ;; Also search for subsequent references in
		    ;; footnote definition so numbering follows
		    ;; reading logic.  Note that we don't have to care
		    ;; about inline definitions, since
		    ;; `org-element-map' already traverses them at the
		    ;; right time.
		    ;;
		    ;; Once again, return nil to stay in the loop.
		    ((not (member fn-lbl seen-refs))
		     (push fn-lbl seen-refs)
		     (funcall search-ref
			      (org-export-get-footnote-definition fn info))
		     nil))))
	       ;; Don't enter footnote definitions since it will
	       ;; happen when their first reference is found.
	       info 'first-match 'footnote-definition)))))
    (catch 'exit (funcall search-ref (plist-get info :parse-tree)))))


;;;; For Headlines
;;
;; `org-export-get-relative-level' is a shortcut to get headline
;; level, relatively to the lower headline level in the parsed tree.
;;
;; `org-export-get-headline-number' returns the section number of an
;; headline, while `org-export-number-to-roman' allows to convert it
;; to roman numbers.
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
  "Return HEADLINE numbering as a list of numbers.
INFO is a plist holding contextual information."
  (cdr (assoc headline (plist-get info :headline-numbering))))

(defun org-export-numbered-headline-p (headline info)
  "Return a non-nil value if HEADLINE element should be numbered.
INFO is a plist used as a communication channel."
  (let ((sec-num (plist-get info :section-numbers))
	(level (org-export-get-relative-level headline info)))
    (if (wholenump sec-num) (<= level sec-num) sec-num)))

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

Select tags (see `org-export-select-tags') and exclude tags (see
`org-export-exclude-tags') are removed from the list.

When non-nil, optional argument TAGS should be a list of strings.
Any tag belonging to this list will also be removed.

When optional argument INHERITED is non-nil, tags can also be
inherited from parent headlines and FILETAGS keywords."
  (org-remove-if
   (lambda (tag) (or (member tag (plist-get info :select-tags))
		(member tag (plist-get info :exclude-tags))
		(member tag tags)))
   (if (not inherited) (org-element-property :tags element)
     ;; Build complete list of inherited tags.
     (let ((current-tag-list (org-element-property :tags element)))
       (mapc
	(lambda (parent)
	  (mapc
	   (lambda (tag)
	     (when (and (memq (org-element-type parent) '(headline inlinetask))
			(not (member tag current-tag-list)))
	       (push tag current-tag-list)))
	   (org-element-property :tags parent)))
	(org-export-get-genealogy element))
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
      (let ((parent headline) value)
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
  (or (let ((headline (if (eq (org-element-type blob) 'headline) blob
			(org-export-get-parent-headline blob))))
	;; Almost like `org-export-node-property', but we cannot trust
	;; `plist-member' as every headline has a `:CATEGORY'
	;; property, would it be nil or equal to "???" (which has the
	;; same meaning).
	(let ((parent headline) value)
	  (catch 'found
	    (while parent
	      (let ((category (org-element-property :CATEGORY parent)))
		(and category (not (equal "???" category))
		     (throw 'found category)))
	      (setq parent (org-element-property :parent parent))))))
      (org-element-map (plist-get info :parse-tree) 'keyword
	(lambda (kwd)
	  (when (equal (org-element-property :key kwd) "CATEGORY")
	    (org-element-property :value kwd)))
	info 'first-match)
      (let ((file (plist-get info :input-file)))
	(and file (file-name-sans-extension (file-name-nondirectory file))))
      "???"))

(defun org-export-get-alt-title (headline info)
  "Return alternative title for HEADLINE, as a secondary string.
INFO is a plist used as a communication channel.  If no optional
title is defined, fall-back to the regular title."
  (or (org-element-property :alt-title headline)
      (org-element-property :title headline)))

(defun org-export-first-sibling-p (headline info)
  "Non-nil when HEADLINE is the first sibling in its sub-tree.
INFO is a plist used as a communication channel."
  (not (eq (org-element-type (org-export-get-previous-element headline info))
	   'headline)))

(defun org-export-last-sibling-p (headline info)
  "Non-nil when HEADLINE is the last sibling in its sub-tree.
INFO is a plist used as a communication channel."
  (not (org-export-get-next-element headline info)))


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
;; `org-export-solidify-link-text' turns a string into a safer version
;; for links, replacing most non-standard characters with hyphens.
;;
;; `org-export-get-coderef-format' returns an appropriate format
;; string for coderefs.
;;
;; `org-export-inline-image-p' returns a non-nil value when the link
;; provided should be considered as an inline image.
;;
;; `org-export-resolve-fuzzy-link' searches destination of fuzzy links
;; (i.e. links with "fuzzy" as type) within the parsed tree, and
;; returns an appropriate unique identifier when found, or nil.
;;
;; `org-export-resolve-id-link' returns the first headline with
;; specified id or custom-id in parse tree, the path to the external
;; file with the id or nil when neither was found.
;;
;; `org-export-resolve-coderef' associates a reference to a line
;; number in the element it belongs, or returns the reference itself
;; when the element isn't numbered.

(defun org-export-solidify-link-text (s)
  "Take link text S and make a safe target out of it."
  (save-match-data
    (mapconcat 'identity (org-split-string s "[^a-zA-Z0-9_.-:]+") "-")))

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

  \(TYPE . REGEXP)

Applying a rule means apply REGEXP against LINK's path when its
type is TYPE.  The function will return a non-nil value if any of
the provided rules is non-nil.  The default rule is
`org-export-default-inline-image-rule'.

This only applies to links without a description."
  (and (not (org-element-contents link))
       (let ((case-fold-search t)
	     (rules (or rules org-export-default-inline-image-rule)))
	 (catch 'exit
	   (mapc
	    (lambda (rule)
	      (and (string= (org-element-property :type link) (car rule))
		   (string-match (cdr rule)
				 (org-element-property :path link))
		   (throw 'exit t)))
	    rules)
	   ;; Return nil if no rule matched.
	   nil))))

(defun org-export-resolve-coderef (ref info)
  "Resolve a code reference REF.

INFO is a plist used as a communication channel.

Return associated line number in source code, or REF itself,
depending on src-block or example element's switches."
  (org-element-map (plist-get info :parse-tree) '(example-block src-block)
    (lambda (el)
      (with-temp-buffer
	(insert (org-trim (org-element-property :value el)))
	(let* ((label-fmt (regexp-quote
			   (or (org-element-property :label-fmt el)
			       org-coderef-label-format)))
	       (ref-re
		(format "^.*?\\S-.*?\\([ \t]*\\(%s\\)\\)[ \t]*$"
			(replace-regexp-in-string "%s" ref label-fmt nil t))))
	  ;; Element containing REF is found.  Resolve it to either
	  ;; a label or a line number, as needed.
	  (when (re-search-backward ref-re nil t)
	    (cond
	     ((org-element-property :use-labels el) ref)
	     ((eq (org-element-property :number-lines el) 'continued)
	      (+ (org-export-get-loc el info) (line-number-at-pos)))
	     (t (line-number-at-pos)))))))
    info 'first-match))

(defun org-export-resolve-fuzzy-link (link info)
  "Return LINK destination.

INFO is a plist holding contextual information.

Return value can be an object, an element, or nil:

- If LINK path matches a target object (i.e. <<path>>) return it.

- If LINK path exactly matches the name affiliated keyword
  \(i.e. #+NAME: path) of an element, return that element.

- If LINK path exactly matches any headline name, return that
  element.  If more than one headline share that name, priority
  will be given to the one with the closest common ancestor, if
  any, or the first one in the parse tree otherwise.

- Otherwise, return nil.

Assume LINK type is \"fuzzy\".  White spaces are not
significant."
  (let* ((raw-path (org-element-property :path link))
	 (match-title-p (eq (aref raw-path 0) ?*))
	 ;; Split PATH at white spaces so matches are space
	 ;; insensitive.
	 (path (org-split-string
		(if match-title-p (substring raw-path 1) raw-path)))
	 ;; Cache for destinations that are not position dependent.
	 (link-cache
	  (or (plist-get info :resolve-fuzzy-link-cache)
	      (plist-get (setq info (plist-put info :resolve-fuzzy-link-cache
					       (make-hash-table :test 'equal)))
			 :resolve-fuzzy-link-cache)))
	 (cached (gethash path link-cache 'not-found)))
    (cond
     ;; Destination is not position dependent: use cached value.
     ((and (not match-title-p) (not (eq cached 'not-found))) cached)
     ;; First try to find a matching "<<path>>" unless user specified
     ;; he was looking for a headline (path starts with a "*"
     ;; character).
     ((and (not match-title-p)
	   (let ((match (org-element-map (plist-get info :parse-tree) 'target
			  (lambda (blob)
			    (and (equal (org-split-string
					 (org-element-property :value blob))
					path)
				 blob))
			  info 'first-match)))
	     (and match (puthash path match link-cache)))))
     ;; Then try to find an element with a matching "#+NAME: path"
     ;; affiliated keyword.
     ((and (not match-title-p)
	   (let ((match (org-element-map (plist-get info :parse-tree)
			    org-element-all-elements
			  (lambda (el)
			    (let ((name (org-element-property :name el)))
			      (when (and name
					 (equal (org-split-string name) path))
				el)))
			  info 'first-match)))
	     (and match (puthash path match link-cache)))))
     ;; Last case: link either points to a headline or to nothingness.
     ;; Try to find the source, with priority given to headlines with
     ;; the closest common ancestor.  If such candidate is found,
     ;; return it, otherwise return nil.
     (t
      (let ((find-headline
	     (function
	      ;; Return first headline whose `:raw-value' property is
	      ;; NAME in parse tree DATA, or nil.  Statistics cookies
	      ;; are ignored.
	      (lambda (name data)
		(org-element-map data 'headline
		  (lambda (headline)
		    (when (equal (org-split-string
				  (replace-regexp-in-string
				   "\\[[0-9]+%\\]\\|\\[[0-9]+/[0-9]+\\]" ""
				   (org-element-property :raw-value headline)))
				 name)
		      headline))
		  info 'first-match)))))
	;; Search among headlines sharing an ancestor with link, from
	;; closest to farthest.
	(catch 'exit
	  (mapc
	   (lambda (parent)
	     (let ((foundp (funcall find-headline path parent)))
	       (when foundp (throw 'exit foundp))))
	   (let ((parent-hl (org-export-get-parent-headline link)))
	     (if (not parent-hl) (list (plist-get info :parse-tree))
	       (cons parent-hl (org-export-get-genealogy parent-hl)))))
	  ;; No destination found: return nil.
	  (and (not match-title-p) (puthash path nil link-cache))))))))

(defun org-export-resolve-id-link (link info)
  "Return headline referenced as LINK destination.

INFO is a plist used as a communication channel.

Return value can be the headline element matched in current parse
tree, a file name or nil.  Assume LINK type is either \"id\" or
\"custom-id\"."
  (let ((id (org-element-property :path link)))
    ;; First check if id is within the current parse tree.
    (or (org-element-map (plist-get info :parse-tree) 'headline
	  (lambda (headline)
	    (when (or (string= (org-element-property :ID headline) id)
		      (string= (org-element-property :CUSTOM_ID headline) id))
	      headline))
	  info 'first-match)
	;; Otherwise, look for external files.
	(cdr (assoc id (plist-get info :id-alist))))))

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


;;;; For References
;;
;; `org-export-get-ordinal' associates a sequence number to any object
;; or element.

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
the plist used as a communication channel.  This allows to count
only a certain type of objects (i.e. inline images).

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
	  (loop for parent in (org-export-get-genealogy element)
		when
		(memq
		 (org-element-type parent)
		 '(footnote-definition footnote-reference headline item
				       table))
		return parent)))
  (case (org-element-type element)
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
	    ((not predicate) (incf counter) nil)
	    ((funcall predicate el info) (incf counter) nil)))
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
  "Return accumulated lines of code up to ELEMENT.

INFO is the plist used as a communication channel.

ELEMENT is excluded from count."
  (let ((loc 0))
    (org-element-map (plist-get info :parse-tree)
	`(src-block example-block ,(org-element-type element))
      (lambda (el)
	(cond
	 ;; ELEMENT is reached: Quit the loop.
	 ((eq el element))
	 ;; Only count lines from src-block and example-block elements
	 ;; with a "+n" or "-n" switch.  A "-n" switch resets counter.
	 ((not (memq (org-element-type el) '(src-block example-block))) nil)
	 ((let ((linums (org-element-property :number-lines el)))
	    (when linums
	      ;; Accumulate locs or reset them.
	      (let ((lines (org-count-lines
			    (org-trim (org-element-property :value el)))))
		(setq loc (if (eq linums 'new) lines (+ loc lines))))))
	  ;; Return nil to stay in the loop.
	  nil)))
      info 'first-match)
    ;; Return value.
    loc))

(defun org-export-unravel-code (element)
  "Clean source code and extract references out of it.

ELEMENT has either a `src-block' an `example-block' type.

Return a cons cell whose CAR is the source code, cleaned from any
reference and protective comma and CDR is an alist between
relative line number (integer) and name of code reference on that
line (string)."
  (let* ((line 0) refs
	 ;; Get code and clean it.  Remove blank lines at its
	 ;; beginning and end.
	 (code (replace-regexp-in-string
		"\\`\\([ \t]*\n\\)+" ""
		(replace-regexp-in-string
		 "\\([ \t]*\n\\)*[ \t]*\\'" "\n"
		 (org-element-property :value element))))
	 ;; Get format used for references.
	 (label-fmt (regexp-quote
		     (or (org-element-property :label-fmt element)
			 org-coderef-label-format)))
	 ;; Build a regexp matching a loc with a reference.
	 (with-ref-re
	  (format "^.*?\\S-.*?\\([ \t]*\\(%s\\)[ \t]*\\)$"
		  (replace-regexp-in-string
		   "%s" "\\([-a-zA-Z0-9_ ]+\\)" label-fmt nil t))))
    ;; Return value.
    (cons
     ;; Code with references removed.
     (org-element-normalize-string
      (mapconcat
       (lambda (loc)
	 (incf line)
	 (if (not (string-match with-ref-re loc)) loc
	   ;; Ref line: remove ref, and signal its position in REFS.
	   (push (cons line (match-string 3 loc)) refs)
	   (replace-match "" nil nil loc 1)))
       (org-split-string code "\n") "\n"))
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
  (let ((--locs (org-split-string code "\n"))
	(--line 0))
    (org-element-normalize-string
     (mapconcat
      (lambda (--loc)
	(incf --line)
	(let ((--ref (cdr (assq --line ref-alist))))
	  (funcall fun --loc (and num-lines (+ num-lines --line)) --ref)))
      --locs "\n"))))

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
	 (code-lines (org-split-string code "\n")))
    (if (null code-lines) ""
      (let* ((refs (and (org-element-property :retain-labels element)
			(cdr code-info)))
	     ;; Handle line numbering.
	     (num-start (case (org-element-property :number-lines element)
			  (continued (org-export-get-loc element info))
			  (new 0)))
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
		   (concat (make-string
			    (- (+ 6 max-width)
			       (+ (length loc) (length number-str))) ? )
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
;; `org-export-table-row-starts-header-p' and
;; `org-export-table-row-ends-header-p' indicate position of current
;; row or cell within the table.

(defun org-export-table-has-special-column-p (table)
  "Non-nil when TABLE has a special column.
All special columns will be ignored during export."
  ;; The table has a special column when every first cell of every row
  ;; has an empty value or contains a symbol among "/", "#", "!", "$",
  ;; "*" "_" and "^".  Though, do not consider a first row containing
  ;; only empty cells as special.
  (let ((special-column-p 'empty))
    (catch 'exit
      (mapc
       (lambda (row)
	 (when (eq (org-element-property :type row) 'standard)
	   (let ((value (org-element-contents
			 (car (org-element-contents row)))))
	     (cond ((member value '(("/") ("#") ("!") ("$") ("*") ("_") ("^")))
		    (setq special-column-p 'special))
		   ((not value))
		   (t (throw 'exit nil))))))
       (org-element-contents table))
      (eq special-column-p 'special))))

(defun org-export-table-has-header-p (table info)
  "Non-nil when TABLE has a header.

INFO is a plist used as a communication channel.

A table has a header when it contains at least two row groups."
  (let ((cache (or (plist-get info :table-header-cache)
		   (plist-get (setq info
				    (plist-put info :table-header-cache
					       (make-hash-table :test 'eq)))
			      :table-header-cache))))
    (or (gethash table cache)
	(let ((rowgroup 1) row-flag)
	  (puthash
	   table
	   (org-element-map table 'table-row
	     (lambda (row)
	       (cond
		((> rowgroup 1) t)
		((and row-flag (eq (org-element-property :type row) 'rule))
		 (incf rowgroup) (setq row-flag nil))
		((and (not row-flag) (eq (org-element-property :type row)
					 'standard))
		 (setq row-flag t) nil)))
	     info 'first-match)
	   cache)))))

(defun org-export-table-row-is-special-p (table-row info)
  "Non-nil if TABLE-ROW is considered special.

INFO is a plist used as the communication channel.

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
	   (mapc
	    (lambda (cell)
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
	    (org-element-contents table-row))
	   (eq special-row-p 'cookie)))))))

(defun org-export-table-row-group (table-row info)
  "Return TABLE-ROW's group number, as an integer.

INFO is a plist used as the communication channel.

Return value is the group number, as an integer, or nil for
special rows and rows separators.  First group is also table's
header."
  (let ((cache (or (plist-get info :table-row-group-cache)
		   (plist-get (setq info
				    (plist-put info :table-row-group-cache
					       (make-hash-table :test 'eq)))
			      :table-row-group-cache))))
    (cond ((gethash table-row cache))
	  ((eq (org-element-property :type table-row) 'rule) nil)
	  (t (let ((group 0) row-flag)
	       (org-element-map (org-export-get-parent table-row) 'table-row
		 (lambda (row)
		   (if (eq (org-element-property :type row) 'rule)
		       (setq row-flag nil)
		     (unless row-flag (incf group) (setq row-flag t)))
		   (when (eq table-row row) (puthash table-row group cache)))
		 info 'first-match))))))

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
		    (plist-get (setq info
				     (plist-put info :table-cell-width-cache
						(make-hash-table :test 'eq)))
			       :table-cell-width-cache)))
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
		    (plist-get (setq info
				     (plist-put info :table-cell-alignment-cache
						(make-hash-table :test 'eq)))
			       :table-cell-alignment-cache)))
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
		(incf total-cells)
		;; Treat an empty cell as a number if it follows
		;; a number.
		(if (not (or (string-match org-table-number-regexp value)
			     (and (string= value "") previous-cell-number-p)))
		    (setq previous-cell-number-p nil)
		  (setq previous-cell-number-p t)
		  (incf number-cells))))))
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
	(mapc (lambda (row)
		(cond ((eq (org-element-property :type row) 'rule)
		       (setq rule-flag t))
		      ((not (org-export-table-row-is-special-p row info))
		       (if rule-flag (throw 'exit (push 'above borders))
			 (throw 'exit nil)))))
	      ;; Look at every row before the current one.
	      (cdr (memq row (reverse (org-element-contents table)))))
	;; No rule above, or rule found starts the table (ignoring any
	;; special row): TABLE-CELL is at the top of the table.
	(when rule-flag (push 'above borders))
	(push 'top borders)))
    ;; Bottom/below border? TABLE-CELL has a border below when next
    ;; non-regular row below is a rule.
    (let (rule-flag)
      (catch 'exit
	(mapc (lambda (row)
		(cond ((eq (org-element-property :type row) 'rule)
		       (setq rule-flag t))
		      ((not (org-export-table-row-is-special-p row info))
		       (if rule-flag (throw 'exit (push 'below borders))
			 (throw 'exit nil)))))
	      ;; Look at every row after the current one.
	      (cdr (memq row (org-element-contents table))))
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
	(mapc
	 (lambda (row)
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
		 (throw 'exit nil)))))
	 ;; Table rows are read in reverse order so last column groups
	 ;; row has precedence over any previous one.
	 (reverse (org-element-contents table)))))
    ;; Return value.
    borders))

(defun org-export-table-cell-starts-colgroup-p (table-cell info)
  "Non-nil when TABLE-CELL is at the beginning of a row group.
INFO is a plist used as a communication channel."
  ;; A cell starts a column group either when it is at the beginning
  ;; of a row (or after the special column, if any) or when it has
  ;; a left border.
  (or (eq (org-element-map (org-export-get-parent table-cell) 'table-cell
	    'identity info 'first-match)
	  table-cell)
      (memq 'left (org-export-table-cell-borders table-cell info))))

(defun org-export-table-cell-ends-colgroup-p (table-cell info)
  "Non-nil when TABLE-CELL is at the end of a row group.
INFO is a plist used as a communication channel."
  ;; A cell ends a column group either when it is at the end of a row
  ;; or when it has a right border.
  (or (eq (car (last (org-element-contents
			 (org-export-get-parent table-cell))))
	     table-cell)
      (memq 'right (org-export-table-cell-borders table-cell info))))

(defun org-export-table-row-starts-rowgroup-p (table-row info)
  "Non-nil when TABLE-ROW is at the beginning of a column group.
INFO is a plist used as a communication channel."
  (unless (or (eq (org-element-property :type table-row) 'rule)
	      (org-export-table-row-is-special-p table-row info))
    (let ((borders (org-export-table-cell-borders
		    (car (org-element-contents table-row)) info)))
      (or (memq 'top borders) (memq 'above borders)))))

(defun org-export-table-row-ends-rowgroup-p (table-row info)
  "Non-nil when TABLE-ROW is at the end of a column group.
INFO is a plist used as a communication channel."
  (unless (or (eq (org-element-property :type table-row) 'rule)
	      (org-export-table-row-is-special-p table-row info))
    (let ((borders (org-export-table-cell-borders
		    (car (org-element-contents table-row)) info)))
      (or (memq 'bottom borders) (memq 'below borders)))))

(defun org-export-table-row-starts-header-p (table-row info)
  "Non-nil when TABLE-ROW is the first table header's row.
INFO is a plist used as a communication channel."
  (and (org-export-table-has-header-p
	(org-export-get-parent-table table-row) info)
       (org-export-table-row-starts-rowgroup-p table-row info)
       (= (org-export-table-row-group table-row info) 1)))

(defun org-export-table-row-ends-header-p (table-row info)
  "Non-nil when TABLE-ROW is the last table header's row.
INFO is a plist used as a communication channel."
  (and (org-export-table-has-header-p
	(org-export-get-parent-table table-row) info)
       (org-export-table-row-ends-rowgroup-p table-row info)
       (= (org-export-table-row-group table-row info) 1)))

(defun org-export-table-row-number (table-row info)
  "Return TABLE-ROW number.
INFO is a plist used as a communication channel.  Return value is
zero-based and ignores separators.  The function returns nil for
special columns and separators."
  (when (and (eq (org-element-property :type table-row) 'standard)
	     (not (org-export-table-row-is-special-p table-row info)))
    (let ((number 0))
      (org-element-map (org-export-get-parent-table table-row) 'table-row
	(lambda (row)
	  (cond ((eq row table-row) number)
		((eq (org-element-property :type row) 'standard)
		 (incf number) nil)))
	info 'first-match))))

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
	  (incf rows)
	  (unless first-row (setq first-row row)))) info)
    ;; Set number of columns.
    (org-element-map first-row 'table-cell (lambda (cell) (incf columns)) info)
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
		  (if (eq cell table-cell) col-count (incf col-count) nil))
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
		    (t (incf row-count) nil)))
	    info 'first-match))
	'table-cell
      (lambda (cell)
	(if (= column-count column-pos) cell
	  (incf column-count) nil))
      info 'first-match)))


;;;; For Tables Of Contents
;;
;; `org-export-collect-headlines' builds a list of all exportable
;; headline elements, maybe limited to a certain depth.  One can then
;; easily parse it and transcode it.
;;
;; Building lists of tables, figures or listings is quite similar.
;; Once the generic function `org-export-collect-elements' is defined,
;; `org-export-collect-tables', `org-export-collect-figures' and
;; `org-export-collect-listings' can be derived from it.

(defun org-export-collect-headlines (info &optional n)
  "Collect headlines in order to build a table of contents.

INFO is a plist used as a communication channel.

When optional argument N is an integer, it specifies the depth of
the table of contents.  Otherwise, it is set to the value of the
last headline level.  See `org-export-headline-levels' for more
information.

Return a list of all exportable headlines as parsed elements.
Footnote sections, if any, will be ignored."
  (let ((limit (plist-get info :headline-levels)))
    (setq n (if (wholenump n) (min n limit) limit))
    (org-element-map (plist-get info :parse-tree) 'headline
      #'(lambda (headline)
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


;;;; Smart Quotes
;;
;; The main function for the smart quotes sub-system is
;; `org-export-activate-smart-quotes', which replaces every quote in
;; a given string from the parse tree with its "smart" counterpart.
;;
;; Dictionary for smart quotes is stored in
;; `org-export-smart-quotes-alist'.
;;
;; Internally, regexps matching potential smart quotes (checks at
;; string boundaries are also necessary) are defined in
;; `org-export-smart-quotes-regexps'.

(defconst org-export-smart-quotes-alist
  '(("da"
     ;; one may use: »...«, "...", ›...‹, or '...'.
     ;; http://sproget.dk/raad-og-regler/retskrivningsregler/retskrivningsregler/a7-40-60/a7-58-anforselstegn/
     ;; LaTeX quotes require Babel!
     (opening-double-quote :utf-8 "»" :html "&raquo;" :latex ">>"
			   :texinfo "@guillemetright{}")
     (closing-double-quote :utf-8 "«" :html "&laquo;" :latex "<<"
			   :texinfo "@guillemetleft{}")
     (opening-single-quote :utf-8 "›" :html "&rsaquo;" :latex "\\frq{}"
			   :texinfo "@guilsinglright{}")
     (closing-single-quote :utf-8 "‹" :html "&lsaquo;" :latex "\\flq{}"
			   :texinfo "@guilsingleft{}")
     (apostrophe :utf-8 "’" :html "&rsquo;"))
    ("de"
     (opening-double-quote :utf-8 "„" :html "&bdquo;" :latex "\"`"
			   :texinfo "@quotedblbase{}")
     (closing-double-quote :utf-8 "“" :html "&ldquo;" :latex "\"'"
			   :texinfo "@quotedblleft{}")
     (opening-single-quote :utf-8 "‚" :html "&sbquo;" :latex "\\glq{}"
			   :texinfo "@quotesinglbase{}")
     (closing-single-quote :utf-8 "‘" :html "&lsquo;" :latex "\\grq{}"
			   :texinfo "@quoteleft{}")
     (apostrophe :utf-8 "’" :html "&rsquo;"))
    ("en"
     (opening-double-quote :utf-8 "“" :html "&ldquo;" :latex "``" :texinfo "``")
     (closing-double-quote :utf-8 "”" :html "&rdquo;" :latex "''" :texinfo "''")
     (opening-single-quote :utf-8 "‘" :html "&lsquo;" :latex "`" :texinfo "`")
     (closing-single-quote :utf-8 "’" :html "&rsquo;" :latex "'" :texinfo "'")
     (apostrophe :utf-8 "’" :html "&rsquo;"))
    ("es"
     (opening-double-quote :utf-8 "«" :html "&laquo;" :latex "\\guillemotleft{}"
			   :texinfo "@guillemetleft{}")
     (closing-double-quote :utf-8 "»" :html "&raquo;" :latex "\\guillemotright{}"
			   :texinfo "@guillemetright{}")
     (opening-single-quote :utf-8 "“" :html "&ldquo;" :latex "``" :texinfo "``")
     (closing-single-quote :utf-8 "”" :html "&rdquo;" :latex "''" :texinfo "''")
     (apostrophe :utf-8 "’" :html "&rsquo;"))
    ("fr"
     (opening-double-quote :utf-8 "« " :html "&laquo;&nbsp;" :latex "\\og "
			   :texinfo "@guillemetleft{}@tie{}")
     (closing-double-quote :utf-8 " »" :html "&nbsp;&raquo;" :latex "\\fg{}"
			   :texinfo "@tie{}@guillemetright{}")
     (opening-single-quote :utf-8 "« " :html "&laquo;&nbsp;" :latex "\\og "
			   :texinfo "@guillemetleft{}@tie{}")
     (closing-single-quote :utf-8 " »" :html "&nbsp;&raquo;" :latex "\\fg{}"
			   :texinfo "@tie{}@guillemetright{}")
     (apostrophe :utf-8 "’" :html "&rsquo;"))
    ("no"
     ;; https://nn.wikipedia.org/wiki/Sitatteikn
     (opening-double-quote :utf-8 "«" :html "&laquo;" :latex "\\guillemotleft{}"
			   :texinfo "@guillemetleft{}")
     (closing-double-quote :utf-8 "»" :html "&raquo;" :latex "\\guillemotright{}"
			   :texinfo "@guillemetright{}")
     (opening-single-quote :utf-8 "‘" :html "&lsquo;" :latex "`" :texinfo "`")
     (closing-single-quote :utf-8 "’" :html "&rsquo;" :latex "'" :texinfo "'")
     (apostrophe :utf-8 "’" :html "&rsquo;"))
    ("nb"
     ;; https://nn.wikipedia.org/wiki/Sitatteikn
     (opening-double-quote :utf-8 "«" :html "&laquo;" :latex "\\guillemotleft{}"
			   :texinfo "@guillemetleft{}")
     (closing-double-quote :utf-8 "»" :html "&raquo;" :latex "\\guillemotright{}"
			   :texinfo "@guillemetright{}")
     (opening-single-quote :utf-8 "‘" :html "&lsquo;" :latex "`" :texinfo "`")
     (closing-single-quote :utf-8 "’" :html "&rsquo;" :latex "'" :texinfo "'")
     (apostrophe :utf-8 "’" :html "&rsquo;"))
    ("nn"
     ;; https://nn.wikipedia.org/wiki/Sitatteikn
     (opening-double-quote :utf-8 "«" :html "&laquo;" :latex "\\guillemotleft{}"
			   :texinfo "@guillemetleft{}")
     (closing-double-quote :utf-8 "»" :html "&raquo;" :latex "\\guillemotright{}"
			   :texinfo "@guillemetright{}")
     (opening-single-quote :utf-8 "‘" :html "&lsquo;" :latex "`" :texinfo "`")
     (closing-single-quote :utf-8 "’" :html "&rsquo;" :latex "'" :texinfo "'")
     (apostrophe :utf-8 "’" :html "&rsquo;"))
    ("sv"
     ;; based on https://sv.wikipedia.org/wiki/Citattecken
     (opening-double-quote :utf-8 "”" :html "&rdquo;" :latex "’’" :texinfo "’’")
     (closing-double-quote :utf-8 "”" :html "&rdquo;" :latex "’’" :texinfo "’’")
     (opening-single-quote :utf-8 "’" :html "&rsquo;" :latex "’" :texinfo "`")
     (closing-single-quote :utf-8 "’" :html "&rsquo;" :latex "’" :texinfo "'")
     (apostrophe :utf-8 "’" :html "&rsquo;"))
    )
  "Smart quotes translations.

Alist whose CAR is a language string and CDR is an alist with
quote type as key and a plist associating various encodings to
their translation as value.

A quote type can be any symbol among `opening-double-quote',
`closing-double-quote', `opening-single-quote',
`closing-single-quote' and `apostrophe'.

Valid encodings include `:utf-8', `:html', `:latex' and
`:texinfo'.

If no translation is found, the quote character is left as-is.")

(defconst org-export-smart-quotes-regexps
  (list
   ;; Possible opening quote at beginning of string.
   "\\`\\([\"']\\)\\(\\w\\|\\s.\\|\\s_\\|\\s(\\)"
   ;; Possible closing quote at beginning of string.
   "\\`\\([\"']\\)\\(\\s-\\|\\s)\\|\\s.\\)"
   ;; Possible apostrophe at beginning of string.
   "\\`\\('\\)\\S-"
   ;; Opening single and double quotes.
   "\\(?:\\s-\\|\\s(\\)\\([\"']\\)\\(?:\\w\\|\\s.\\|\\s_\\)"
   ;; Closing single and double quotes.
   "\\(?:\\w\\|\\s.\\|\\s_\\)\\([\"']\\)\\(?:\\s-\\|\\s)\\|\\s.\\)"
   ;; Apostrophe.
   "\\S-\\('\\)\\S-"
   ;; Possible opening quote at end of string.
   "\\(?:\\s-\\|\\s(\\)\\([\"']\\)\\'"
   ;; Possible closing quote at end of string.
   "\\(?:\\w\\|\\s.\\|\\s_\\)\\([\"']\\)\\'"
   ;; Possible apostrophe at end of string.
   "\\S-\\('\\)\\'")
  "List of regexps matching a quote or an apostrophe.
In every regexp, quote or apostrophe matched is put in group 1.")

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
  (if (equal s "") ""
    (let* ((prev (org-export-get-previous-element (or original s) info))
	   ;; Try to be flexible when computing number of blanks
	   ;; before object.  The previous object may be a string
	   ;; introduced by the back-end and not completely parsed.
	   (pre-blank (and prev
			   (or (org-element-property :post-blank prev)
			       ;; A string with missing `:post-blank'
			       ;; property.
			       (and (stringp prev)
				    (string-match " *\\'" prev)
				    (length (match-string 0 prev)))
			       ;; Fallback value.
			       0)))
	   (next (org-export-get-next-element (or original s) info))
	   (get-smart-quote
	    (lambda (q type)
	      ;; Return smart quote associated to a give quote Q, as
	      ;; a string.  TYPE is a symbol among `open', `close' and
	      ;; `apostrophe'.
	      (let ((key (case type
			   (apostrophe 'apostrophe)
			   (open (if (equal "'" q) 'opening-single-quote
				   'opening-double-quote))
			   (otherwise (if (equal "'" q) 'closing-single-quote
					'closing-double-quote)))))
		(or (plist-get
		     (cdr (assq key
				(cdr (assoc (plist-get info :language)
					    org-export-smart-quotes-alist))))
		     encoding)
		    q)))))
      (if (or (equal "\"" s) (equal "'" s))
	  ;; Only a quote: no regexp can match.  We have to check both
	  ;; sides and decide what to do.
	  (cond ((and (not prev) (not next)) s)
		((not prev) (funcall get-smart-quote s 'open))
		((and (not next) (zerop pre-blank))
		 (funcall get-smart-quote s 'close))
		((not next) s)
		((zerop pre-blank) (funcall get-smart-quote s 'apostrophe))
		(t (funcall get-smart-quote 'open)))
	;; 1. Replace quote character at the beginning of S.
	(cond
	 ;; Apostrophe?
	 ((and prev (zerop pre-blank)
	       (string-match (nth 2 org-export-smart-quotes-regexps) s))
	  (setq s (replace-match
		   (funcall get-smart-quote (match-string 1 s) 'apostrophe)
		   nil t s 1)))
	 ;; Closing quote?
	 ((and prev (zerop pre-blank)
	       (string-match (nth 1 org-export-smart-quotes-regexps) s))
	  (setq s (replace-match
		   (funcall get-smart-quote (match-string 1 s) 'close)
		   nil t s 1)))
	 ;; Opening quote?
	 ((and (or (not prev) (> pre-blank 0))
	       (string-match (nth 0 org-export-smart-quotes-regexps) s))
	  (setq s (replace-match
		   (funcall get-smart-quote (match-string 1 s) 'open)
		   nil t s 1))))
	;; 2. Replace quotes in the middle of the string.
	(setq s (replace-regexp-in-string
		 ;; Opening quotes.
		 (nth 3 org-export-smart-quotes-regexps)
		 (lambda (text)
		   (funcall get-smart-quote (match-string 1 text) 'open))
		 s nil t 1))
	(setq s (replace-regexp-in-string
		 ;; Closing quotes.
		 (nth 4 org-export-smart-quotes-regexps)
		 (lambda (text)
		   (funcall get-smart-quote (match-string 1 text) 'close))
		 s nil t 1))
	(setq s (replace-regexp-in-string
		 ;; Apostrophes.
		 (nth 5 org-export-smart-quotes-regexps)
		 (lambda (text)
		   (funcall get-smart-quote (match-string 1 text) 'apostrophe))
		 s nil t 1))
	;; 3. Replace quote character at the end of S.
	(cond
	 ;; Apostrophe?
	 ((and next (string-match (nth 8 org-export-smart-quotes-regexps) s))
	  (setq s (replace-match
		   (funcall get-smart-quote (match-string 1 s) 'apostrophe)
		   nil t s 1)))
	 ;; Closing quote?
	 ((and (not next)
	       (string-match (nth 7 org-export-smart-quotes-regexps) s))
	  (setq s (replace-match
		   (funcall get-smart-quote (match-string 1 s) 'close)
		   nil t s 1)))
	 ;; Opening quote?
	 ((and next (string-match (nth 6 org-export-smart-quotes-regexps) s))
	  (setq s (replace-match
		   (funcall get-smart-quote (match-string 1 s) 'open)
		   nil t s 1))))
	;; Return string with smart quotes.
	s))))

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
;;
;; `org-export-get-genealogy' returns the full genealogy of a given
;; element or object, from closest parent to full parse tree.

(defsubst org-export-get-parent (blob)
  "Return BLOB parent or nil.
BLOB is the element or object considered."
  (org-element-property :parent blob))

(defun org-export-get-genealogy (blob)
  "Return full genealogy relative to a given element or object.

BLOB is the element or object being considered.

Ancestors are returned from closest to farthest, the last one
being the full parse tree."
  (let (genealogy (parent blob))
    (while (setq parent (org-element-property :parent parent))
      (push parent genealogy))
    (nreverse genealogy)))

(defun org-export-get-parent-headline (blob)
  "Return BLOB parent headline or nil.
BLOB is the element or object being considered."
  (let ((parent blob))
    (while (and (setq parent (org-element-property :parent parent))
		(not (eq (org-element-type parent) 'headline))))
    parent))

(defun org-export-get-parent-element (object)
  "Return first element containing OBJECT or nil.
OBJECT is the object to consider."
  (let ((parent object))
    (while (and (setq parent (org-element-property :parent parent))
		(memq (org-element-type parent) org-element-all-objects)))
    parent))

(defun org-export-get-parent-table (object)
  "Return OBJECT parent table or nil.
OBJECT is either a `table-cell' or `table-element' type object."
  (let ((parent object))
    (while (and (setq parent (org-element-property :parent parent))
		(not (eq (org-element-type parent) 'table))))
    parent))

(defun org-export-get-previous-element (blob info &optional n)
  "Return previous element or object.

BLOB is an element or object.  INFO is a plist used as
a communication channel.  Return previous exportable element or
object, a string, or nil.

When optional argument N is a positive integer, return a list
containing up to N siblings before BLOB, from farthest to
closest.  With any other non-nil value, return a list containing
all of them."
  (let ((siblings
	 ;; An object can belong to the contents of its parent or
	 ;; to a secondary string.  We check the latter option
	 ;; first.
	 (let ((parent (org-export-get-parent blob)))
	   (or (let ((sec-value (org-element-property
				 (cdr (assq (org-element-type parent)
					    org-element-secondary-value-alist))
				 parent)))
		 (and (memq blob sec-value) sec-value))
	       (org-element-contents parent))))
	prev)
    (catch 'exit
      (mapc (lambda (obj)
	      (cond ((memq obj (plist-get info :ignore-list)))
		    ((null n) (throw 'exit obj))
		    ((not (wholenump n)) (push obj prev))
		    ((zerop n) (throw 'exit prev))
		    (t (decf n) (push obj prev))))
	    (cdr (memq blob (reverse siblings))))
      prev)))

(defun org-export-get-next-element (blob info &optional n)
  "Return next element or object.

BLOB is an element or object.  INFO is a plist used as
a communication channel.  Return next exportable element or
object, a string, or nil.

When optional argument N is a positive integer, return a list
containing up to N siblings after BLOB, from closest to farthest.
With any other non-nil value, return a list containing all of
them."
  (let ((siblings
	 ;; An object can belong to the contents of its parent or to
	 ;; a secondary string.  We check the latter option first.
	 (let ((parent (org-export-get-parent blob)))
	   (or (let ((sec-value (org-element-property
				 (cdr (assq (org-element-type parent)
					    org-element-secondary-value-alist))
				 parent)))
		 (cdr (memq blob sec-value)))
	       (cdr (memq blob (org-element-contents parent))))))
	next)
    (catch 'exit
      (mapc (lambda (obj)
	      (cond ((memq obj (plist-get info :ignore-list)))
		    ((null n) (throw 'exit obj))
		    ((not (wholenump n)) (push obj next))
		    ((zerop n) (throw 'exit (nreverse next)))
		    (t (decf n) (push obj next))))
	    siblings)
      (nreverse next))))


;;;; Translation
;;
;; `org-export-translate' translates a string according to the language
;; specified by the LANGUAGE keyword.  `org-export-dictionary' contains
;; the dictionary used for the translation.

(defconst org-export-dictionary
  '(("%e %n: %c"
     ("fr" :default "%e %n : %c" :html "%e&nbsp;%n&nbsp;: %c"))
    ("Author"
     ("ca" :default "Autor")
     ("cs" :default "Autor")
     ("da" :default "Forfatter")
     ("de" :default "Autor")
     ("eo" :html "A&#365;toro")
     ("es" :default "Autor")
     ("fi" :html "Tekij&auml;")
     ("fr" :default "Auteur")
     ("hu" :default "Szerz&otilde;")
     ("is" :html "H&ouml;fundur")
     ("it" :default "Autore")
     ("ja" :html "&#33879;&#32773;" :utf-8 "著者")
     ("nl" :default "Auteur")
     ("no" :default "Forfatter")
     ("nb" :default "Forfatter")
     ("nn" :default "Forfattar")
     ("pl" :default "Autor")
     ("ru" :html "&#1040;&#1074;&#1090;&#1086;&#1088;" :utf-8 "Автор")
     ("sv" :html "F&ouml;rfattare")
     ("uk" :html "&#1040;&#1074;&#1090;&#1086;&#1088;" :utf-8 "Автор")
     ("zh-CN" :html "&#20316;&#32773;" :utf-8 "作者")
     ("zh-TW" :html "&#20316;&#32773;" :utf-8 "作者"))
    ("Date"
     ("ca" :default "Data")
     ("cs" :default "Datum")
     ("da" :default "Dato")
     ("de" :default "Datum")
     ("eo" :default "Dato")
     ("es" :default "Fecha")
     ("fi" :html "P&auml;iv&auml;m&auml;&auml;r&auml;")
     ("hu" :html "D&aacute;tum")
     ("is" :default "Dagsetning")
     ("it" :default "Data")
     ("ja" :html "&#26085;&#20184;" :utf-8 "日付")
     ("nl" :default "Datum")
     ("no" :default "Dato")
     ("nb" :default "Dato")
     ("nn" :default "Dato")
     ("pl" :default "Data")
     ("ru" :html "&#1044;&#1072;&#1090;&#1072;" :utf-8 "Дата")
     ("sv" :default "Datum")
     ("uk" :html "&#1044;&#1072;&#1090;&#1072;" :utf-8 "Дата")
     ("zh-CN" :html "&#26085;&#26399;" :utf-8 "日期")
     ("zh-TW" :html "&#26085;&#26399;" :utf-8 "日期"))
    ("Equation"
     ("da" :default "Ligning")
     ("de" :default "Gleichung")
     ("es" :html "Ecuaci&oacute;n" :default "Ecuación")
     ("fr" :ascii "Equation" :default "Équation")
     ("no" :default "Ligning")
     ("nb" :default "Ligning")
     ("nn" :default "Likning")
     ("sv" :default "Ekvation")
     ("zh-CN" :html "&#26041;&#31243;" :utf-8 "方程"))
    ("Figure"
     ("da" :default "Figur")
     ("de" :default "Abbildung")
     ("es" :default "Figura")
     ("ja" :html "&#22259;" :utf-8 "図")
     ("no" :default "Illustrasjon")
     ("nb" :default "Illustrasjon")
     ("nn" :default "Illustrasjon")
     ("sv" :default "Illustration")
     ("zh-CN" :html "&#22270;" :utf-8 "图"))
    ("Figure %d:"
     ("da" :default "Figur %d")
     ("de" :default "Abbildung %d:")
     ("es" :default "Figura %d:")
     ("fr" :default "Figure %d :" :html "Figure&nbsp;%d&nbsp;:")
     ("ja" :html "&#22259;%d: " :utf-8 "図%d: ")
     ("no" :default "Illustrasjon %d")
     ("nb" :default "Illustrasjon %d")
     ("nn" :default "Illustrasjon %d")
     ("sv" :default "Illustration %d")
     ("zh-CN" :html "&#22270;%d&nbsp;" :utf-8 "图%d "))
    ("Footnotes"
     ("ca" :html "Peus de p&agrave;gina")
     ("cs" :default "Pozn\xe1mky pod carou")
     ("da" :default "Fodnoter")
     ("de" :html "Fu&szlig;noten" :default "Fußnoten")
     ("eo" :default "Piednotoj")
     ("es" :html "Nota al pie de p&aacute;gina" :default "Nota al pie de página")
     ("fi" :default "Alaviitteet")
     ("fr" :default "Notes de bas de page")
     ("hu" :html "L&aacute;bjegyzet")
     ("is" :html "Aftanm&aacute;lsgreinar")
     ("it" :html "Note a pi&egrave; di pagina")
     ("ja" :html "&#33050;&#27880;" :utf-8 "脚注")
     ("nl" :default "Voetnoten")
     ("no" :default "Fotnoter")
     ("nb" :default "Fotnoter")
     ("nn" :default "Fotnotar")
     ("pl" :default "Przypis")
     ("ru" :html "&#1057;&#1085;&#1086;&#1089;&#1082;&#1080;" :utf-8 "Сноски")
     ("sv" :default "Fotnoter")
     ("uk" :html "&#1055;&#1088;&#1080;&#1084;&#1110;&#1090;&#1082;&#1080;"
      :utf-8 "Примітки")
     ("zh-CN" :html "&#33050;&#27880;" :utf-8 "脚注")
     ("zh-TW" :html "&#33139;&#35387;" :utf-8 "腳註"))
    ("List of Listings"
     ("da" :default "Programmer")
     ("de" :default "Programmauflistungsverzeichnis")
     ("es" :default "Indice de Listados de programas")
     ("fr" :default "Liste des programmes")
     ("no" :default "Dataprogrammer")
     ("nb" :default "Dataprogrammer")
     ("zh-CN" :html "&#20195;&#30721;&#30446;&#24405;" :utf-8 "代码目录"))
    ("List of Tables"
     ("da" :default "Tabeller")
     ("de" :default "Tabellenverzeichnis")
     ("es" :default "Indice de tablas")
     ("fr" :default "Liste des tableaux")
     ("no" :default "Tabeller")
     ("nb" :default "Tabeller")
     ("nn" :default "Tabeller")
     ("sv" :default "Tabeller")
     ("zh-CN" :html "&#34920;&#26684;&#30446;&#24405;" :utf-8 "表格目录"))
    ("Listing %d:"
     ("da" :default "Program %d")
     ("de" :default "Programmlisting %d")
     ("es" :default "Listado de programa %d")
     ("fr" :default "Programme %d :" :html "Programme&nbsp;%d&nbsp;:")
     ("no" :default "Dataprogram")
     ("nb" :default "Dataprogram")
     ("zh-CN" :html "&#20195;&#30721;%d&nbsp;" :utf-8 "代码%d "))
    ("See section %s"
     ("da" :default "jævnfør afsnit %s")
     ("de" :default "siehe Abschnitt %s")
     ("es" :default "vea seccion %s")
     ("fr" :default "cf. section %s")
     ("zh-CN" :html "&#21442;&#35265;&#31532;%d&#33410;" :utf-8 "参见第%s节"))
    ("Table"
     ("de" :default "Tabelle")
     ("es" :default "Tabla")
     ("fr" :default "Tableau")
     ("ja" :html "&#34920;" :utf-8 "表")
     ("zh-CN" :html "&#34920;" :utf-8 "表"))
    ("Table %d:"
     ("da" :default "Tabel %d")
     ("de" :default "Tabelle %d")
     ("es" :default "Tabla %d")
     ("fr" :default "Tableau %d :")
     ("ja" :html "&#34920;%d:" :utf-8 "表%d:")
     ("no" :default "Tabell %d")
     ("nb" :default "Tabell %d")
     ("nn" :default "Tabell %d")
     ("sv" :default "Tabell %d")
     ("zh-CN" :html "&#34920;%d&nbsp;" :utf-8 "表%d "))
    ("Table of Contents"
     ("ca" :html "&Iacute;ndex")
     ("cs" :default "Obsah")
     ("da" :default "Indhold")
     ("de" :default "Inhaltsverzeichnis")
     ("eo" :default "Enhavo")
     ("es" :html "&Iacute;ndice")
     ("fi" :html "Sis&auml;llysluettelo")
     ("fr" :ascii "Sommaire" :default "Table des matières")
     ("hu" :html "Tartalomjegyz&eacute;k")
     ("is" :default "Efnisyfirlit")
     ("it" :default "Indice")
     ("ja" :html "&#30446;&#27425;" :utf-8 "目次")
     ("nl" :default "Inhoudsopgave")
     ("no" :default "Innhold")
     ("nb" :default "Innhold")
     ("nn" :default "Innhald")
     ("pl" :html "Spis tre&#x015b;ci")
     ("ru" :html "&#1057;&#1086;&#1076;&#1077;&#1088;&#1078;&#1072;&#1085;&#1080;&#1077;"
      :utf-8 "Содержание")
     ("sv" :html "Inneh&aring;ll")
     ("uk" :html "&#1047;&#1084;&#1110;&#1089;&#1090;" :utf-8 "Зміст")
     ("zh-CN" :html "&#30446;&#24405;" :utf-8 "目录")
     ("zh-TW" :html "&#30446;&#37636;" :utf-8 "目錄"))
    ("Unknown reference"
     ("da" :default "ukendt reference")
     ("de" :default "Unbekannter Verweis")
     ("es" :default "referencia desconocida")
     ("fr" :ascii "Destination inconnue" :default "Référence inconnue")
     ("zh-CN" :html "&#26410;&#30693;&#24341;&#29992;" :utf-8 "未知引用")))
  "Dictionary for export engine.

Alist whose CAR is the string to translate and CDR is an alist
whose CAR is the language string and CDR is a plist whose
properties are possible charsets and values translated terms.

It is used as a database for `org-export-translate'. Since this
function returns the string as-is if no translation was found,
the variable only needs to record values different from the
entry.")

(defun org-export-translate (s encoding info)
  "Translate string S according to language specification.

ENCODING is a symbol among `:ascii', `:html', `:latex', `:latin1'
and `:utf-8'.  INFO is a plist used as a communication channel.

Translation depends on `:language' property. Return the
translated string. If no translation is found, try to fall back
to `:default' encoding. If it fails, return S."
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
;; allow to export to a buffer or a file, asynchronously or not.
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

BODY evaluation happens in an asynchronous process, from a buffer
which is an exact copy of the current one.

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
                 (start-process
                  "org-export-process" ,proc-buffer
                  (expand-file-name invocation-name invocation-directory)
                  "-Q" "--batch"
                  "-l" org-export-async-init-file
                  "-l" ,temp-file)))
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
                           (message "Process '%s' exited abnormally" p))
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

  \(defun org-latex-export-as-latex
    \(&optional async subtreep visible-only body-only ext-plist)
    \(interactive)
    \(org-export-to-buffer 'latex \"*Org LATEX Export*\"
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

  \(defun org-latex-export-to-latex
    \(&optional async subtreep visible-only body-only ext-plist)
    \(interactive)
    \(let ((outfile (org-export-output-file-name \".tex\" subtreep)))
      \(org-export-to-file 'latex outfile
        async subtreep visible-only body-only ext-plist
        \(lambda (file) (org-latex-compile file)))

The function returns either a file name returned by POST-PROCESS,
or FILE."
  (declare (indent 2))
  (if (not (file-writable-p file)) (error "Output file not writable")
    (let ((encoding (or org-export-coding-system buffer-file-coding-system)))
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

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return file name as a string."
  (let* ((visited-file (buffer-file-name (buffer-base-buffer)))
	 (base-name
	  ;; File name may come from EXPORT_FILE_NAME subtree
	  ;; property, assuming point is at beginning of said
	  ;; sub-tree.
	  (file-name-sans-extension
	   (or (and subtreep
		    (org-entry-get
		     (save-excursion
		       (ignore-errors (org-back-to-heading) (point)))
		     "EXPORT_FILE_NAME" t))
	       ;; File name may be extracted from buffer's associated
	       ;; file, if any.
	       (and visited-file (file-name-nondirectory visited-file))
	       ;; Can't determine file name on our own: Ask user.
	       (let ((read-file-name-function
		      (and org-completion-use-ido 'ido-read-file-name)))
		 (read-file-name
		  "Output file: " pub-dir nil nil nil
		  (lambda (name)
		    (string= (file-name-extension name t) extension)))))))
	 (output-file
	  ;; Build file name.  Enforce EXTENSION over whatever user
	  ;; may have come up with.  PUB-DIR, if defined, always has
	  ;; precedence over any provided path.
	  (cond
	   (pub-dir
	    (concat (file-name-as-directory pub-dir)
		    (file-name-nondirectory base-name)
		    extension))
	   ((file-name-absolute-p base-name) (concat base-name extension))
	   (t (concat (file-name-as-directory ".") base-name extension)))))
    ;; If writing to OUTPUT-FILE would overwrite original file, append
    ;; EXTENSION another time to final name.
    (if (and visited-file (org-file-equal-p visited-file output-file))
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
    (set-buffer buffer)
    (when (zerop (buffer-size)) (org-export-stack-mode))
    (org-export-stack-refresh)
    (pop-to-buffer buffer))
  (message "Type \"q\" to quit, \"?\" for help"))

(defun org-export--stack-source-at-point ()
  "Return source from export results at point in stack."
  (let ((source (car (nth (1- (org-current-line)) org-export-stack-contents))))
    (if (not source) (error "Source unavailable, please refresh buffer")
      (let ((source-name (if (stringp source) source (buffer-name source))))
	(if (save-excursion
	      (beginning-of-line)
	      (looking-at (concat ".* +" (regexp-quote source-name) "$")))
	    source
	  ;; SOURCE is not consistent with current line.  The stack
	  ;; view is outdated.
	  (error "Source unavailable; type `g' to update buffer"))))))

(defun org-export-stack-clear ()
  "Remove all entries from export stack."
  (interactive)
  (setq org-export-stack-contents nil))

(defun org-export-stack-refresh (&rest dummy)
  "Refresh the asynchronous export stack.
DUMMY is ignored.  Unavailable sources are removed from the list.
Return the new stack."
  (let ((inhibit-read-only t))
    (org-preserve-lc
     (erase-buffer)
     (insert (concat
	      (let ((counter 0))
		(mapconcat
		 (lambda (entry)
		   (let ((proc-p (processp (nth 2 entry))))
		     (concat
		      ;; Back-end.
		      (format " %-12s  " (or (nth 1 entry) ""))
		      ;; Age.
		      (let ((data (nth 2 entry)))
			(if proc-p (format " %6s  " (process-status data))
			  ;; Compute age of the results.
			  (org-format-seconds
			   "%4h:%.2m  "
			   (float-time (time-since data)))))
		      ;; Source.
		      (format " %s"
			      (let ((source (car entry)))
				(if (stringp source) source
				  (buffer-name source)))))))
		 ;; Clear stack from exited processes, dead buffers or
		 ;; non-existent files.
		 (setq org-export-stack-contents
		       (org-remove-if-not
			(lambda (el)
			  (if (processp (nth 2 el))
			      (buffer-live-p (process-buffer (nth 2 el)))
			    (let ((source (car el)))
			      (if (bufferp source) (buffer-live-p source)
				(file-exists-p source)))))
			org-export-stack-contents)) "\n")))))))

(defun org-export-stack-remove (&optional source)
  "Remove export results at point from stack.
If optional argument SOURCE is non-nil, remove it instead."
  (interactive)
  (let ((source (or source (org-export--stack-source-at-point))))
    (setq org-export-stack-contents
	  (org-remove-if (lambda (el) (equal (car el) source))
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
    (define-key km " " 'next-line)
    (define-key km "n" 'next-line)
    (define-key km "\C-n" 'next-line)
    (define-key km [down] 'next-line)
    (define-key km "p" 'previous-line)
    (define-key km "\C-p" 'previous-line)
    (define-key km "\C-?" 'previous-line)
    (define-key km [up] 'previous-line)
    (define-key km "C" 'org-export-stack-clear)
    (define-key km "v" 'org-export-stack-view)
    (define-key km (kbd "RET") 'org-export-stack-view)
    (define-key km "d" 'org-export-stack-remove)
    km)
  "Keymap for Org Export Stack.")

(define-derived-mode org-export-stack-mode special-mode "Org-Stack"
  "Mode for displaying asynchronous export stack.

Type \\[org-export-stack] to visualize the asynchronous export
stack.

In an Org Export Stack buffer, use \\<org-export-stack-mode-map>\\[org-export-stack-view] to view export output
on current line, \\[org-export-stack-remove] to remove it from the stack and \\[org-export-stack-clear] to clear
stack completely.

Removing entries in an Org Export Stack buffer doesn't affect
files or buffers, only the display.

\\{org-export-stack-mode-map}"
  (abbrev-mode 0)
  (auto-fill-mode 0)
  (setq buffer-read-only t
	buffer-undo-list t
	truncate-lines t
	header-line-format
	'(:eval
	  (format "  %-12s | %6s | %s" "Back-End" "Age" "Source")))
  (org-add-hook 'post-command-hook 'org-export-stack-refresh nil t)
  (set (make-local-variable 'revert-buffer-function)
       'org-export-stack-refresh))



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

When ARG is \\[universal-argument], repeat the last export action, with the same set
of options used back then, on the current buffer.

When ARG is \\[universal-argument] \\[universal-argument], display the asynchronous export stack."
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
    (case action
      ;; First handle special hard-coded actions.
      (template (org-export-insert-default-template nil optns))
      (stack (org-export-stack))
      (publish-current-file
       (org-publish-current-file (memq 'force optns) (memq 'async optns)))
      (publish-current-project
       (org-publish-current-project (memq 'force optns) (memq 'async optns)))
      (publish-choose-project
       (org-publish (assoc (org-icompleting-read
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
options are given in the prompt.  Moreover, \[?] allows to switch
back to standard interface."
  (let* ((fontify-key
	  (lambda (key &optional access-key)
	    ;; Fontify KEY string.  Optional argument ACCESS-KEY, when
	    ;; non-nil is the required first-level key to activate
	    ;; KEY.  When its value is t, activate KEY independently
	    ;; on the first key, if any.  A nil value means KEY will
	    ;; only be activated at first level.
	    (if (or (eq access-key t) (eq access-key first-key))
		(org-propertize key 'face 'org-warning)
	      key)))
	 (fontify-value
	  (lambda (value)
	    ;; Fontify VALUE string.
	    (org-propertize value 'face 'font-lock-variable-name-face)))
	 ;; Prepare menu entries by extracting them from registered
	 ;; back-ends and sorting them by access key and by ordinal,
	 ;; if any.
	 (entries
	  (sort (sort (delq nil
			    (mapcar 'org-export-backend-menu
				    org-export--registered-backends))
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
			       (incf index)
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
      (case key
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
       (let ((option (case key (2 'body) (22 'visible) (19 'subtree)
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
	      (case key
		(?f 'publish-current-file)
		(?p 'publish-current-project)
		(?x 'publish-choose-project)
		(?a 'publish-all)))
	     ;; Return first action associated to FIRST-KEY + KEY
	     ;; path. Indeed, derived backends can share the same
	     ;; FIRST-KEY.
	     (t (catch 'found
		  (mapc (lambda (entry)
			  (let ((match (assq key (nth 2 entry))))
			    (when match (throw 'found (nth 2 match)))))
			(member (assq first-key entries) entries)))))
	    options))
     ;; Otherwise, enter sub-menu.
     (t (org-export--dispatch-ui options key expertp)))))



(provide 'ox)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox.el ends here
