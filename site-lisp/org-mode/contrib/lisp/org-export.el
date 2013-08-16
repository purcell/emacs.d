;;; org-export.el --- Generic Export Engine For Org

;; Copyright (C) 2011, 2012  Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a generic export engine for Org, built on
;; its syntactical parser: Org Elements.

;; Besides that parser, the generic exporter is made of three distinct
;; parts:

;; - The communication channel consists in a property list, which is
;;   created and updated during the process.  Its use is to offer
;;   every piece of information, would it be export options or
;;   contextual data, all in a single place.  The exhaustive list of
;;   properties is given in "The Communication Channel" section of
;;   this file.

;; - The transcoder walks the parse tree, ignores or treat as plain
;;   text elements and objects according to export options, and
;;   eventually calls back-end specific functions to do the real
;;   transcoding, concatenating their return value along the way.

;; - The filter system is activated at the very beginning and the very
;;   end of the export process, and each time an element or an object
;;   has been converted.  It is the entry point to fine-tune standard
;;   output from back-end transcoders.

;; The core function is `org-export-as'.  It returns the transcoded
;; buffer as a string.

;; In order to derive an exporter out of this generic implementation,
;; one can define a transcode function for each element or object.
;; Such function should return a string for the corresponding element,
;; without any trailing space, or nil.  It must accept three
;; arguments:
;; 1. the element or object itself,
;; 2. its contents, or nil when it isn't recursive,
;; 3. the property list used as a communication channel.

;; If no such function is found, that element or object type will
;; simply be ignored, along with any separating blank line.  The same
;; will happen if the function returns the nil value.  If that
;; function returns the empty string, the type will be ignored, but
;; the blank lines will be kept.

;; Contents, when not nil, are stripped from any global indentation
;; (although the relative one is preserved).  They also always end
;; with a single newline character.

;; These functions must follow a strict naming convention:
;; `org-BACKEND-TYPE' where, obviously, BACKEND is the name of the
;; export back-end and TYPE the type of the element or object handled.

;; Moreover, two additional functions can be defined.  On the one
;; hand, `org-BACKEND-template' returns the final transcoded string,
;; and can be used to add a preamble and a postamble to document's
;; body.  It must accept two arguments: the transcoded string and the
;; property list containing export options.  On the other hand,
;; `org-BACKEND-plain-text', when defined, is to be called on every
;; text not recognized as an element or an object.  It must accept two
;; arguments: the text string and the information channel.

;; Any back-end can define its own variables.  Among them, those
;; customizables should belong to the `org-export-BACKEND' group.
;; Also, a special variable, `org-BACKEND-option-alist', allows to
;; define buffer keywords and "#+options:" items specific to that
;; back-end.  See `org-export-option-alist' for supported defaults and
;; syntax.

;; Tools for common tasks across back-ends are implemented in the last
;; part of this file.

;;; Code:
(eval-when-compile (require 'cl))
(require 'org-element)


;;; Internal Variables

;; Among internal variables, the most important is
;; `org-export-option-alist'.  This variable define the global export
;; options, shared between every exporter, and how they are acquired.

(defconst org-export-max-depth 19
  "Maximum nesting depth for headlines, counting from 0.")

(defconst org-export-option-alist
  '((:author "AUTHOR" nil user-full-name t)
    (:creator "CREATOR" nil org-export-creator-string)
    (:date "DATE" nil nil t)
    (:description "DESCRIPTION" nil nil newline)
    (:email "EMAIL" nil user-mail-address t)
    (:exclude-tags "EXPORT_EXCLUDE_TAGS" nil org-export-exclude-tags split)
    (:headline-levels nil "H" org-export-headline-levels)
    (:keywords "KEYWORDS" nil nil space)
    (:language "LANGUAGE" nil org-export-default-language t)
    (:preserve-breaks nil "\\n" org-export-preserve-breaks)
    (:section-numbers nil "num" org-export-with-section-numbers)
    (:select-tags "EXPORT_SELECT_TAGS" nil org-export-select-tags split)
    (:time-stamp-file nil "timestamp" org-export-time-stamp-file)
    (:title "TITLE" nil nil space)
    (:with-archived-trees nil "arch" org-export-with-archived-trees)
    (:with-author nil "author" org-export-with-author)
    (:with-creator nil "creator" org-export-with-creator)
    (:with-drawers nil "drawer" org-export-with-drawers)
    (:with-email nil "email" org-export-with-email)
    (:with-emphasize nil "*" org-export-with-emphasize)
    (:with-entities nil "e" org-export-with-entities)
    (:with-fixed-width nil ":" org-export-with-fixed-width)
    (:with-footnotes nil "f" org-export-with-footnotes)
    (:with-priority nil "pri" org-export-with-priority)
    (:with-special-strings nil "-" org-export-with-special-strings)
    (:with-sub-superscript nil "^" org-export-with-sub-superscripts)
    (:with-toc nil "toc" org-export-with-toc)
    (:with-tables nil "|" org-export-with-tables)
    (:with-tags nil "tags" org-export-with-tags)
    (:with-tasks nil "tasks" org-export-with-tasks)
    (:with-timestamps nil "<" org-export-with-timestamps)
    (:with-todo-keywords nil "todo" org-export-with-todo-keywords))
  "Alist between export properties and ways to set them.

The car of the alist is the property name, and the cdr is a list
like \(KEYWORD OPTION DEFAULT BEHAVIOUR\) where:

KEYWORD is a string representing a buffer keyword, or nil.
OPTION is a string that could be found in an #+OPTIONS: line.
DEFAULT is the default value for the property.
BEHAVIOUR determine how Org should handle multiple keywords for
the same property.  It is a symbol among:
  nil       Keep old value and discard the new one.
  t         Replace old value with the new one.
  `space'   Concatenate the values, separating them with a space.
  `newline' Concatenate the values, separating them with
	    a newline.
  `split'   Split values at white spaces, and cons them to the
	    previous list.

KEYWORD and OPTION have precedence over DEFAULT.

All these properties should be back-end agnostic.  For back-end
specific properties, define a similar variable named
`org-BACKEND-option-alist', replacing BACKEND with the name of
the appropriate back-end.  You can also redefine properties
there, as they have precedence over these.")

(defconst org-export-special-keywords
  '("SETUP_FILE" "OPTIONS" "MACRO")
  "List of in-buffer keywords that require special treatment.
These keywords are not directly associated to a property.  The
way they are handled must be hard-coded into
`org-export-get-inbuffer-options' function.")



;;; User-configurable Variables

;; Configuration for the masses.

;; They should never be evaled directly, as their value is to be
;; stored in a property list (cf. `org-export-option-alist').

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

This option can also be set with the #+OPTIONS line,
e.g. \"arch:nil\"."
  :group 'org-export-general
  :type '(choice
	  (const :tag "Not at all" nil)
	  (const :tag "Headline only" 'headline)
	  (const :tag "Entirely" t)))

(defcustom org-export-with-author t
  "Non-nil means insert author name into the exported file.
This option can also be set with the #+OPTIONS line,
e.g. \"author:nil\"."
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
	  (const :tag "Sentence as a comment" 'comment)
	  (const :tag "Insert the sentence" t)))

(defcustom org-export-creator-string
  (format "Generated by Org mode %s in Emacs %s." org-version emacs-version)
  "String to insert at the end of the generated document."
  :group 'org-export-general
  :type '(string :tag "Creator string"))

(defcustom org-export-with-drawers nil
  "Non-nil means export with drawers like the property drawer.
When t, all drawers are exported.  This may also be a list of
drawer names to export."
  :group 'org-export-general
  :type '(choice
	  (const :tag "All drawers" t)
	  (const :tag "None" nil)
	  (repeat :tag "Selected drawers"
		  (string :tag "Drawer name"))))

(defcustom org-export-with-email nil
  "Non-nil means insert author email into the exported file.
This option can also be set with the #+OPTIONS line,
e.g. \"email:t\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-emphasize t
  "Non-nil means interpret *word*, /word/, and _word_ as emphasized text.

If the export target supports emphasizing text, the word will be
typeset in bold, italic, or underlined, respectively.  Not all
export backends support this.

This option can also be set with the #+OPTIONS line, e.g. \"*:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-exclude-tags '("noexport")
  "Tags that exclude a tree from export.
All trees carrying any of these tags will be excluded from
export.  This is without condition, so even subtrees inside that
carry one of the `org-export-select-tags' will be removed."
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

This option can also be set with the #+OPTIONS line, e.g. \"::nil\"."
  :group 'org-export-translation
  :type 'boolean)

(defcustom org-export-with-footnotes t
  "Non-nil means Org footnotes should be exported.
This option can also be set with the #+OPTIONS line,
e.g. \"f:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-headline-levels 3
  "The last level which is still exported as a headline.

Inferior levels will produce itemize lists when exported.  Note
that a numeric prefix argument to an exporter function overrides
this setting.

This option can also be set with the #+OPTIONS line, e.g. \"H:2\"."
  :group 'org-export-general
  :type 'integer)

(defcustom org-export-default-language "en"
  "The default language for export and clocktable translations, as a string.
This may have an association in
`org-clock-clocktable-language-setup'."
  :group 'org-export-general
  :type '(string :tag "Language"))

(defcustom org-export-preserve-breaks nil
  "Non-nil means preserve all line breaks when exporting.

Normally, in HTML output paragraphs will be reformatted.

This option can also be set with the #+OPTIONS line,
e.g. \"\\n:t\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-entities t
  "Non-nil means interpret entities when exporting.

For example, HTML export converts \\alpha to &alpha; and \\AA to
&Aring;.

For a list of supported names, see the constant `org-entities'
and the user option `org-entities-user'.

This option can also be set with the #+OPTIONS line,
e.g. \"e:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-priority nil
  "Non-nil means include priority cookies in export.
When nil, remove priority cookies for export."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-section-numbers t
  "Non-nil means add section numbers to headlines when exporting.

This option can also be set with the #+OPTIONS line,
e.g. \"num:t\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-select-tags '("export")
  "Tags that select a tree for export.
If any such tag is found in a buffer, all trees that do not carry
one of these tags will be deleted before export.  Inside trees
that are selected like this, you can still deselect a subtree by
tagging it with one of the `org-export-exclude-tags'."
  :group 'org-export-general
  :type '(repeat (string :tag "Tag")))

(defcustom org-export-with-special-strings t
  "Non-nil means interpret \"\-\", \"--\" and \"---\" for export.

When this option is turned on, these strings will be exported as:

  Org     HTML     LaTeX
 -----+----------+--------
  \\-    &shy;      \\-
  --    &ndash;    --
  ---   &mdash;    ---
  ...   &hellip;   \ldots

This option can also be set with the #+OPTIONS line,
e.g. \"-:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-sub-superscripts t
  "Non-nil means interpret \"_\" and \"^\" for export.

When this option is turned on, you can use TeX-like syntax for
sub- and superscripts.  Several characters after \"_\" or \"^\"
will be considered as a single item - so grouping with {} is
normally not needed.  For example, the following things will be
parsed as single sub- or superscripts.

 10^24   or   10^tau     several digits will be considered 1 item.
 10^-12  or   10^-tau    a leading sign with digits or a word
 x^2-y^3                 will be read as x^2 - y^3, because items are
			 terminated by almost any nonword/nondigit char.
 x_{i^2} or   x^(2-i)    braces or parenthesis do grouping.

Still, ambiguity is possible - so when in doubt use {} to enclose
the sub/superscript.  If you set this variable to the symbol
`{}', the braces are *required* in order to trigger
interpretations as sub/superscript.  This can be helpful in
documents that need \"_\" frequently in plain text.

This option can also be set with the #+OPTIONS line,
e.g. \"^:nil\"."
  :group 'org-export-general
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

This option can also be set with the #+OPTIONS line,
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

This option can also be set with the #+OPTIONS line, e.g. \"|:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-tags t
  "If nil, do not export tags, just remove them from headlines.

If this is the symbol `not-in-toc', tags will be removed from
table of contents entries, but still be shown in the headlines of
the document.

This option can also be set with the #+OPTIONS line,
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
todo                 include only tasks that are not yet done.
done                 include only tasks that are already done.
nil                  remove all tasks before export
list of keywords     keep only tasks with these keywords"
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
The time stamp shows when the file was created.

This option can also be set with the #+OPTIONS line,
e.g. \"timestamp:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-timestamps t
  "If nil, do not export time stamps and associated keywords."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-todo-keywords t
  "Non-nil means include TODO keywords in export.
When nil, remove all these keywords from the export.")

(defcustom org-export-allow-BIND 'confirm
  "Non-nil means allow #+BIND to define local variable values for export.
This is a potential security risk, which is why the user must
confirm the use of these lines."
  :group 'org-export-general
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Always" t)
	  (const :tag "Ask a confirmation for each file" confirm)))

(defcustom org-export-snippet-translation-alist nil
  "Alist between export snippets back-ends and exporter back-ends.

This variable allows to provide shortcuts for export snippets.

For example, with a value of '\(\(\"h\" . \"html\"\)\), the HTML
back-end will recognize the contents of \"@h{<b>}\" as HTML code
while every other back-end will ignore it."
  :group 'org-export-general
  :type '(repeat
	  (cons
	   (string :tag "Shortcut")
	   (string :tag "Back-end"))))



;;; The Communication Channel

;; During export process, every function has access to a number of
;; properties.  They are of three types:

;; 1. Export options are collected once at the very beginning of the
;;    process, out of the original buffer and environment.  The task
;;    is handled by `org-export-collect-options' function.
;;
;;    All export options are defined through the
;;    `org-export-option-alist' variable.
;;
;; 2. Persistent properties are stored in
;;    `org-export-persistent-properties' and available at every level
;;    of recursion. Their value is extracted directly from the parsed
;;    tree, and depends on export options (whole trees may be filtered
;;    out of the export process).
;;
;;    Properties belonging to that type are defined in the
;;    `org-export-persistent-properties-list' variable.
;;
;; 3. Every other property is considered local, and available at
;;    a precise level of recursion and below.

;; Managing properties during transcode process is mainly done with
;; `org-export-update-info'.  Even though they come from different
;; sources, the function transparently concatenates them in a single
;; property list passed as an argument to each transcode function.
;; Thus, during export, all necessary information is available through
;; that single property list, and the element or object itself.
;; Though, modifying a property will still require some special care,
;; and should be done with `org-export-set-property' instead of plain
;; `plist-put'.

;; Here is the full list of properties available during transcode
;; process, with their category (option, persistent or local), their
;; value type and the function updating them, when appropriate.

;; + `author' :: Author's name.
;;   - category :: option
;;   - type :: string

;; + `back-end' :: Current back-end used for transcoding.
;;   - category :: persistent
;;   - type :: symbol

;; + `code-refs' :: Association list between reference name and real
;;                  labels in source code.  It is used to properly
;;                  resolve links inside source blocks.
;;   - category :: persistent
;;   - type :: alist (INT-OR-STRING . STRING)
;;   - update :: `org-export-handle-code'

;; + `creator' :: String to write as creation information.
;;   - category :: option
;;   - type :: string

;; + `date' :: String to use as date.
;;   - category :: option
;;   - type :: string

;; + `description' :: Description text for the current data.
;;   - category :: option
;;   - type :: string

;; + `email' :: Author's email.
;;   - category :: option
;;   - type :: string

;; + `exclude-tags' :: Tags for exclusion of subtrees from export
;;      process.
;;   - category :: option
;;   - type :: list of strings

;; + `footnote-definition-alist' :: Alist between footnote labels and
;;     their definition, as parsed data.  Only non-inlined footnotes
;;     are represented in this alist.  Also, every definition isn't
;;     guaranteed to be referenced in the parse tree.  The purpose of
;;     this property is to preserve definitions from oblivion
;;     (i.e. when the parse tree comes from a part of the original
;;     buffer), it isn't meant for direct use in a back-end.  To
;;     retrieve a definition relative to a reference, use
;;     `org-export-get-footnote-definition' instead.
;;   - category :: option
;;   - type :: alist (STRING . LIST)

;; + `footnote-seen-labels' :: List of already transcoded footnote
;;      labels.  It is used to know when a reference appears for the
;;      first time. (cf. `org-export-footnote-first-reference-p').
;;   - category :: persistent
;;   - type :: list of strings
;;   - update :: `org-export-update-info'

;; + `genealogy' :: List of current element's parents types.
;;   - category :: local
;;   - type :: list of symbols
;;   - update :: `org-export-update-info'

;; + `headline-alist' :: Alist between headlines raw name and their
;;      boundaries.  It is used to resolve "fuzzy" links
;;      (cf. `org-export-resolve-fuzzy-link').
;;   - category :: persistent
;;   - type :: alist (STRING INTEGER INTEGER)

;; + `headline-levels' :: Maximum level being exported as an
;;      headline.  Comparison is done with the relative level of
;;      headlines in the parse tree, not necessarily with their
;;      actual level.
;;   - category :: option
;;   - type :: integer

;; + `headline-offset' :: Difference between relative and real level
;;      of headlines in the parse tree.  For example, a value of -1
;;      means a level 2 headline should be considered as level
;;      1 (cf. `org-export-get-relative-level').
;;   - category :: persistent
;;   - type :: integer

;; + `headline-numbering' :: Alist between headlines' beginning
;;      position and their numbering, as a list of numbers
;;      (cf. `org-export-get-headline-number').
;;   - category :: persistent
;;   - type :: alist (INTEGER . LIST)

;; + `included-files' :: List of files, with full path, included in
;;      the current buffer, through the "#+include:" keyword.  It is
;;      mainly used to verify that no infinite recursive inclusion
;;      happens.
;;   - category :: local
;;   - type :: list of strings

;; + `inherited-properties' :: Properties of the headline ancestors
;;      of the current element or object.  Those from the closest
;;      headline have precedence over the others.
;;   - category :: local
;;   - type :: plist

;; + `keywords' :: List of keywords attached to data.
;;   - category :: option
;;   - type :: string

;; + `language' :: Default language used for translations.
;;   - category :: option
;;   - type :: string

;; + `parent-properties' :: Properties of the parent element.
;;   - category :: local
;;   - type :: plist
;;   - update :: `org-export-update-info'

;; + `parse-tree' :: Whole parse tree, available at any time during
;;                   transcoding.
;;   - category :: global
;;   - type :: list (as returned by `org-element-parse-buffer')

;; + `point-max' :: Last ending position in the parse tree.
;;   - category :: global
;;   - type :: integer

;; + `preserve-breaks' :: Non-nil means transcoding should preserve
;;      all line breaks.
;;   - category :: option
;;   - type :: symbol (nil, t)

;; + `previous-element' :: Previous element's type at the same
;;      level.
;;   - category :: local
;;   - type :: symbol
;;   - update :: `org-export-update-info'

;; + `previous-object' :: Previous object type (or `plain-text') at
;;      the same level.
;;   - category :: local
;;   - type :: symbol
;;   - update :: `org-export-update-info'

;; + `section-numbers' :: Non-nil means transcoding should add
;;      section numbers to headlines.
;;   - category :: option
;;   - type :: symbol (nil, t)

;; + `select-tags' :: List of tags enforcing inclusion of sub-trees in
;;                    transcoding.  When such a tag is present,
;;                    subtrees without it are de facto excluded from
;;                    the process.  See `use-select-tags'.
;;   - category :: option
;;   - type :: list of strings

;; + `target-list' :: List of targets raw names encoutered in the
;;                    parse tree.  This is used to partly resolve
;;                    "fuzzy" links
;;                    (cf. `org-export-resolve-fuzzy-link').
;;   - category :: persistent
;;   - type :: list of strings

;; + `time-stamp-file' :: Non-nil means transcoding should insert
;;      a time stamp in the output.
;;   - category :: option
;;   - type :: symbol (nil, t)

;; + `total-loc' :: Contains total lines of code accumulated by source
;;                  blocks with the "+n" option so far.
;;   - category :: persistent
;;   - type :: integer
;;   - update :: `org-export-handle-code'

;; + `use-select-tags' :: When non-nil, a select tags has been found
;;      in the parse tree.  Thus, any headline without one will be
;;      filtered out.  See `select-tags'.
;;   - category :: persistent
;;   - type :: interger or nil

;; + `with-archived-trees' :: Non-nil when archived subtrees should
;;      also be transcoded.  If it is set to the `headline' symbol,
;;      only the archived headline's name is retained.
;;   - category :: option
;;   - type :: symbol (nil, t, `headline')

;; + `with-author' :: Non-nil means author's name should be included
;;                    in the output.
;;   - category :: option
;;   - type :: symbol (nil, t)

;; + `with-creator' :: Non-nild means a creation sentence should be
;;      inserted at the end of the transcoded string.  If the value
;;      is `comment', it should be commented.
;;   - category :: option
;;   - type :: symbol (`comment', nil, t)

;; + `with-drawers' :: Non-nil means drawers should be exported.  If
;;      its value is a list of names, only drawers with such names
;;      will be transcoded.
;;   - category :: option
;;   - type :: symbol (nil, t) or list of strings

;; + `with-email' :: Non-nil means output should contain author's
;;                   email.
;;   - category :: option
;;   - type :: symbol (nil, t)

;; + `with-emphasize' :: Non-nil means emphasized text should be
;;      interpreted.
;;   - category :: option
;;   - type :: symbol (nil, t)

;; + `with-fixed-width' :: Non-nil if transcoder should interpret
;;      strings starting with a colon as a fixed-with (verbatim)
;;      area.
;;   - category :: option
;;   - type :: symbol (nil, t)

;; + `with-footnotes' :: Non-nil if transcoder should interpret
;;      footnotes.
;;   - category :: option
;;   - type :: symbol (nil, t)

;; + `with-priority' :: Non-nil means transcoding should include
;;      priority cookies.
;;   - category :: option
;;   - type :: symbol (nil, t)

;; + `with-special-strings' :: Non-nil means transcoding should
;;      interpret special strings in plain text.
;;   - category :: option
;;   - type :: symbol (nil, t)

;; + `with-sub-superscript' :: Non-nil means transcoding should
;;      interpret subscript and superscript.  With a value of "{}",
;;      only interpret those using curly brackets.
;;   - category :: option
;;   - type :: symbol (nil, {}, t)

;; + `with-tables' :: Non-nil means transcoding should interpret
;;                    tables.
;;   - category :: option
;;   - type :: symbol (nil, t)

;; + `with-tags' :: Non-nil means transcoding should keep tags in
;;                  headlines.  A `not-in-toc' value will remove them
;;                  from the table of contents, if any, nonetheless.
;;   - category :: option
;;   - type :: symbol (nil, t, `not-in-toc')

;; + `with-tasks' :: Non-nil means transcoding should include
;;                   headlines with a TODO keyword.  A `todo' value
;;                   will only include headlines with a todo type
;;                   keyword while a `done' value will do the
;;                   contrary.  If a list of strings is provided, only
;;                   tasks with keywords belonging to that list will
;;                   be kept.
;;   - category :: option
;;   - type :: symbol (t, todo, done, nil) or list of strings

;; + `with-timestamps' :: Non-nil means transcoding should include
;;      time stamps and associated keywords.  Otherwise, completely
;;      remove them.
;;   - category :: option
;;   - type :: symbol: (t, nil)

;; + `with-toc' :: Non-nil means that a table of contents has to be
;;                 added to the output.  An integer value limits its
;;                 depth.
;;   - category :: option
;;   - type :: symbol (nil, t or integer)

;; + `with-todo-keywords' :: Non-nil means transcoding should
;;      include TODO keywords.
;;   - category :: option
;;   - type :: symbol (nil, t)

;;;; Export Options

;; Export options come from five sources, in increasing precedence
;; order:

;; - Global variables,
;; - External options provided at export time,
;; - Options keyword symbols,
;; - Buffer keywords,
;; - Subtree properties.

;; The central internal function with regards to export options is
;; `org-export-collect-options'.  It updates global variables with
;; "#+BIND:" keywords, then retrieve and prioritize properties from
;; the different sources.

;;  The internal functions doing the retrieval are:
;;  `org-export-parse-option-keyword' ,
;;  `org-export-get-subtree-options' ,
;;  `org-export-get-inbuffer-options' and
;;  `org-export-get-global-options'.
;;
;;  Some properties do not rely on the previous sources but still
;;  depend on the original buffer are taken care of in
;;  `org-export-initial-options'.

;; Also, `org-export-confirm-letbind' and `org-export-install-letbind'
;; take care of the part relative to "#+BIND:" keywords.

(defun org-export-collect-options (backend subtreep ext-plist)
  "Collect export options from the current buffer.

BACKEND is a symbol specifying the back-end to use.

When SUBTREEP is non-nil, assume the export is done against the
current sub-tree.

EXT-PLIST is a property list with external parameters overriding
org-mode's default settings, but still inferior to file-local
settings."
  ;; First install #+BIND variables.
  (org-export-install-letbind-maybe)
  ;; Get and prioritize export options...
  (let ((options (org-combine-plists
		  ;; ... from global variables...
		  (org-export-get-global-options backend)
		  ;; ... from an external property list...
		  ext-plist
		  ;; ... from in-buffer settings...
		  (org-export-get-inbuffer-options
		   (org-with-wide-buffer (buffer-string)) backend
		   (and buffer-file-name
			(org-remove-double-quotes buffer-file-name)))
		  ;; ... and from subtree, when appropriate.
		  (and subtreep
		       (org-export-get-subtree-options)))))
    ;; Add initial options.
    (setq options (append (org-export-initial-options options)
			  options))
    ;; Set a default title if none has been specified so far.
    (unless (plist-get options :title)
      (setq options (plist-put options :title
			       (or (and buffer-file-name
					(file-name-sans-extension
					 (file-name-nondirectory
					  buffer-file-name)))
				   (buffer-name)))))
    ;; Return plist.
    options))

(defun org-export-parse-option-keyword (options backend)
  "Parse an OPTIONS line and return values as a plist.
BACKEND is a symbol specifying the back-end to use."
  (let* ((all (append org-export-option-alist
			 (let ((var (intern
				     (format "org-%s-option-alist" backend))))
			   (and (boundp var) (eval var)))))
	 ;; Build an alist between #+OPTION: item and property-name.
	 (alist (delq nil
		      (mapcar (lambda (e)
				(when (nth 2 e) (cons (regexp-quote (nth 2 e))
						      (car e))))
			      all)))
	 plist)
    (mapc (lambda (e)
	    (when (string-match (concat "\\(\\`\\|[ \t]\\)"
					(car e)
					":\\(([^)\n]+)\\|[^ \t\n\r;,.]*\\)")
				options)
	      (setq plist (plist-put plist
				     (cdr e)
				     (car (read-from-string
					   (match-string 2 options)))))))
	  alist)
    plist))

(defun org-export-get-subtree-options ()
  "Get export options in subtree at point.
Return the options as a plist."
  (org-with-wide-buffer
   (when (ignore-errors (org-back-to-heading t))
     (let (prop plist)
       (when (setq prop (progn (looking-at org-todo-line-regexp)
			       (or (org-entry-get (point) "EXPORT_TITLE")
				   (org-match-string-no-properties 3))))
	 (setq plist (plist-put plist :title prop)))
       (when (setq prop (org-entry-get (point) "EXPORT_TEXT"))
	 (setq plist (plist-put plist :text prop)))
       (when (setq prop (org-entry-get (point) "EXPORT_AUTHOR"))
	 (setq plist (plist-put plist :author prop)))
       (when (setq prop (org-entry-get (point) "EXPORT_DATE"))
	 (setq plist (plist-put plist :date prop)))
       (when (setq prop (org-entry-get (point) "EXPORT_OPTIONS"))
	 (setq plist (org-export-add-options-to-plist plist prop)))
       plist))))

(defun org-export-get-inbuffer-options (buffer-string backend files)
  "Return in-buffer options as a plist.
BUFFER-STRING is the string of the buffer.  BACKEND is a symbol
specifying which back-end should be used."
  (let ((case-fold-search t) plist)
    ;; 1. Special keywords, as in `org-export-special-keywords'.
    (let ((start 0)
	  (special-re (org-make-options-regexp org-export-special-keywords)))
      (while (string-match special-re buffer-string start)
	(setq start (match-end 0))
	(let ((key (upcase (org-match-string-no-properties 1 buffer-string)))
	      ;; Special keywords do not have their value expanded.
	      (val (org-match-string-no-properties 2 buffer-string)))
	  (setq plist
		(org-combine-plists
		 (cond
		  ((string= key "SETUP_FILE")
		   (let ((file (expand-file-name
				(org-remove-double-quotes (org-trim val)))))
		     ;; Avoid circular dependencies.
		     (unless (member file files)
		       (org-export-get-inbuffer-options
			(org-file-contents file 'noerror)
			backend
			(cons file files)))))
		  ((string= key "OPTIONS")
		   (org-export-parse-option-keyword val backend))
		  ((string= key "MACRO")
		   (string-match "^\\([-a-zA-Z0-9_]+\\)[ \t]+\\(.*?[ \t]*$\\)"
				 val)
		   (plist-put nil
			      (intern (concat ":macro-"
					      (downcase (match-string 1 val))))
			      (match-string 2 val))))
		 plist)))))
    ;; 2. Standard options, as in `org-export-option-alist'.
    (let* ((all (append org-export-option-alist
			(let ((var (intern
				    (format "org-%s-option-alist" backend))))
			  (and (boundp var) (eval var)))))
	   ;; Build alist between keyword name and property name.
	   (alist (delq nil (mapcar (lambda (e)
				      (when (nth 1 e) (cons (nth 1 e) (car e))))
				    all)))
	   ;; Build regexp matching all keywords associated to export
	   ;; options.  Note: the search is case insensitive.
	   (opt-re (org-make-options-regexp
		    (delq nil (mapcar (lambda (e) (nth 1 e)) all))))
	   (start 0))
      (while (string-match opt-re buffer-string start)
	(setq start (match-end 0))
	(let* ((key (upcase (org-match-string-no-properties 1 buffer-string)))
	       ;; Expand value, applying restrictions for keywords.
	       (val (org-match-string-no-properties 2 buffer-string))
	       (prop (cdr (assoc key alist)))
	       (behaviour (nth 4 (assq prop all))))
	  (setq plist
		(plist-put
		 plist prop
		 ;; Handle value depending on specified BEHAVIOUR.
		 (case behaviour
		   (space (if (plist-get plist prop)
			      (concat (plist-get plist prop) " " (org-trim val))
			    (org-trim val)))
		   (newline (org-trim
			     (concat
			      (plist-get plist prop) "\n" (org-trim val))))
		   (split `(,@(plist-get plist prop) ,@(org-split-string val)))
		   ('t val)
		   (otherwise (plist-get plist prop)))))))
      ;; Parse keywords specified in `org-element-parsed-keywords'.
      (mapc
       (lambda (key)
	 (let* ((prop (cdr (assoc (upcase key) alist)))
		(value (and prop (plist-get plist prop))))
	   (when (stringp value)
	     (setq plist
		   (plist-put
		    plist prop
		    (org-element-parse-secondary-string
		     value
		     (cdr (assq 'keyword org-element-string-restrictions))))))))
       org-element-parsed-keywords))
    ;; Return final value.
    plist))

(defun org-export-get-global-options (backend)
  "Return global export options as a plist.
BACKEND is a symbol specifying which back-end should be used."
  (let ((all (append org-export-option-alist
		     (let ((var (intern
				 (format "org-%s-option-alist" backend))))
		       (and (boundp var) (eval var)))))
	;; Output value.
	plist)
    (mapc (lambda (cell)
	    (setq plist
		  (plist-put plist (car cell) (eval (nth 3 cell)))))
	  all)
    ;; Return value.
    plist))

(defun org-export-initial-options (options)
  "Return a plist with non-optional properties.
OPTIONS is the export options plist computed so far."
  (list
   ;; `:macro-date', `:macro-time' and `:macro-property' could as well
   ;; be initialized as persistent properties, since they don't depend
   ;; on initial environment.  Though, it may be more logical to keep
   ;; them close to other ":macro-" properties.
   :macro-date "(eval (format-time-string \"$1\"))"
   :macro-time "(eval (format-time-string \"$1\"))"
   :macro-property "(eval (org-entry-get nil \"$1\" 'selective))"
   :macro-modification-time
   (and (buffer-file-name)
	(file-exists-p (buffer-file-name))
	(concat "(eval (format-time-string \"$1\" '"
		(prin1-to-string (nth 5 (file-attributes (buffer-file-name))))
		"))"))
   :macro-input-file (and (buffer-file-name)
			  (file-name-nondirectory (buffer-file-name)))
   ;; Footnotes definitions must be collected in the original buffer,
   ;; as there's no insurance that they will still be in the parse
   ;; tree, due to some narrowing.
   :footnote-definition-alist
   (let (alist)
     (org-with-wide-buffer
      (goto-char (point-min))
      (while (re-search-forward org-footnote-definition-re nil t)
	(let ((def (org-footnote-at-definition-p)))
	  (when def
	    (org-skip-whitespace)
	    (push (cons (car def)
			(save-restriction
			  (narrow-to-region (point) (nth 2 def))
			  (org-element-parse-buffer)))
		  alist))))
      alist))))

(defvar org-export-allow-BIND-local nil)
(defun org-export-confirm-letbind ()
  "Can we use #+BIND values during export?
By default this will ask for confirmation by the user, to divert
possible security risks."
  (cond
   ((not org-export-allow-BIND) nil)
   ((eq org-export-allow-BIND t) t)
   ((local-variable-p 'org-export-allow-BIND-local) org-export-allow-BIND-local)
   (t (org-set-local 'org-export-allow-BIND-local
		     (yes-or-no-p "Allow BIND values in this buffer? ")))))

(defun org-export-install-letbind-maybe ()
  "Install the values from #+BIND lines as local variables.
Variables must be installed before in-buffer options are
retrieved."
  (let (letbind pair)
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward (org-make-options-regexp '("BIND")) nil t)
       (when (org-export-confirm-letbind)
	 (push (read (concat "(" (org-match-string-no-properties 2) ")"))
	       letbind))))
    (while (setq pair (pop letbind))
      (org-set-local (car pair) (nth 1 pair)))))


;;;; Persistent Properties

;; Persistent properties are declared in
;; `org-export-persistent-properties-list' variable.  Most of them are
;; initialized at the beginning of the transcoding process by
;; `org-export-initialize-persistent-properties'.  The others are
;; updated during that process.

;; Dedicated functions focus on computing the value of specific
;; persistent properties during initialization.  Thus,
;; `org-export-use-select-tag-p' determines if an headline makes use
;; of an export tag enforcing inclusion. `org-export-get-min-level'
;; gets the minimal exportable level, used as a basis to compute
;; relative level for headlines. `org-export-get-point-max' returns
;; the maximum exportable ending position in the parse tree.
;; Eventually `org-export-collect-headline-numbering' builds an alist
;; between headlines' beginning position and their numbering.

(defconst org-export-persistent-properties-list
  '(:back-end :code-refs :headline-alist :headline-numbering :headline-offset
	      :parse-tree :point-max :footnote-seen-labels :target-list
	      :total-loc :use-select-tags)
  "List of persistent properties.")

(defconst org-export-persistent-properties nil
  "Used internally to store properties and values during transcoding.

Only properties that should survive recursion are saved here.

This variable is reset before each transcoding.")

(defun org-export-initialize-persistent-properties (data options backend)
  "Initialize `org-export-persistent-properties'.

DATA is the parse tree from which information is retrieved.
OPTIONS is a list holding export options.  BACKEND is the
back-end called for transcoding, as a symbol.

Following initial persistent properties are set:
`:back-end'        Back-end used for transcoding.

`:headline-alist'  Alist of all headlines' name as key and a list
		   holding beginning and ending positions as
		   value.

`:headline-offset' Offset between true level of headlines and
		   local level. An offset of -1 means an headline
		   of level 2 should be considered as a level
		   1 headline in the context.

`:headline-numbering' Alist of all headlines' beginning position
		   as key an the associated numbering as value.

`:parse-tree'      Whole parse tree.

`:point-max'       Last position in the parse tree

`:target-list'     List of all targets' raw name in the parse tree.

`:use-select-tags' Non-nil when parsed tree use a special tag to
		   enforce transcoding of the headline."
  ;; First delete any residual persistent property.
  (setq org-export-persistent-properties nil)
  ;; Immediately after, set `:use-select-tags' property, as it will be
  ;; required for further computations.
  (setq options
	(org-export-set-property
	 options
	 :use-select-tags
	 (org-export-use-select-tags-p data options)))
  ;; Get the rest of the initial persistent properties, now
  ;; `:use-select-tags' is set...
  ;; 1. `:parse-tree' ...
  (setq options (org-export-set-property options :parse-tree data))
  ;; 2. `:headline-offset' ...
  (setq options
	(org-export-set-property
	 options :headline-offset
	 (- 1 (org-export-get-min-level data options))))
  ;; 3. `:point-max' ...
  (setq options (org-export-set-property
		 options :point-max
		 (org-export-get-point-max data options)))
  ;; 4. `:target-list'...
  (setq options (org-export-set-property
		 options :target-list
		 (org-element-map
		  data 'target
		  (lambda (target info)
		    (org-element-get-property :raw-value target)))))
  ;; 5. `:headline-alist'
  (setq options (org-export-set-property
		 options :headline-alist
		 (org-element-map
		  data 'headline
		  (lambda (headline info)
		    (list (org-element-get-property :raw-value headline)
			  (org-element-get-property :begin headline)
			  (org-element-get-property :end headline))))))
  ;; 6. `:headline-numbering'
  (setq options (org-export-set-property
		 options :headline-numbering
		 (org-export-collect-headline-numbering data options)))
  ;; 7. `:back-end'
  (setq options (org-export-set-property options :back-end backend)))

(defun org-export-use-select-tags-p (data options)
  "Non-nil when data use a tag enforcing transcoding.
DATA is parsed data as returned by `org-element-parse-buffer'.
OPTIONS is a plist holding export options."
  (org-element-map
   data
   'headline
   (lambda (headline info)
     (let ((tags (org-element-get-property :with-tags headline)))
       (and tags (string-match
		  (format ":%s:" (plist-get info :select-tags)) tags))))
   options
   'stop-at-first-match))

(defun org-export-get-min-level (data options)
  "Return minimum exportable headline's level in DATA.
DATA is parsed tree as returned by `org-element-parse-buffer'.
OPTIONS is a plist holding export options."
  (catch 'exit
    (let ((min-level 10000))
      (mapc (lambda (blob)
	      (when (and (eq (car blob) 'headline)
			 (not (org-export-skip-p blob options)))
		(setq min-level
		      (min (org-element-get-property :level blob) min-level)))
	      (when (= min-level 1) (throw 'exit 1)))
	    (org-element-get-contents data))
      ;; If no headline was found, for the sake of consistency, set
      ;; minimum level to 1 nonetheless.
      (if (= min-level 10000) 1 min-level))))

(defun org-export-get-point-max (data options)
  "Return last exportable ending position in DATA.
DATA is parsed tree as returned by `org-element-parse-buffer'.
OPTIONS is a plist holding export options."
  (let ((pos-max 1))
    (mapc (lambda (blob)
	    (unless (and (eq (car blob) 'headline)
			 (org-export-skip-p blob options))
	      (setq pos-max (org-element-get-property :end blob))))
	  (org-element-get-contents data))
    pos-max))

(defun org-export-collect-headline-numbering (data options)
  "Return numbering of all exportable headlines in a parse tree.

DATA is the parse tree.  OPTIONS is the plist holding export
options.

Return an alist whose key is headline's beginning position and
value is its associated numbering (in the shape of a list of
numbers)."
  (let ((numbering (make-vector org-export-max-depth 0)))
    (org-element-map
     data
     'headline
     (lambda (headline info)
       (let ((relative-level
	      (1- (org-export-get-relative-level headline info))))
	 (cons
	  (org-element-get-property :begin headline)
	  (loop for n across numbering
		for idx from 0 to org-export-max-depth
		when (< idx relative-level) collect n
		when (= idx relative-level) collect (aset numbering idx (1+ n))
		when (> idx relative-level) do (aset numbering idx 0)))))
     options)))


;;;; Properties Management

;; This is mostly done with the help of two functions.  On the one
;; hand `org-export-update-info' is used to keep up-to-date local
;; information while walking the nested list representing the parsed
;; document.  On the other end, `org-export-set-property' handles
;; properties modifications according to their type (persistent or
;; local).

;; As exceptions, `:code-refs' and `:total-loc' properties are updated
;; with `org-export-handle-code' function.

(defun org-export-update-info (blob info recursep)
  "Update export options depending on context.

BLOB is the element or object being parsed.  INFO is the plist
holding the export options.

When RECURSEP is non-nil, assume the following element or object
will be inside the current one.

The following properties are updated:
`footnote-seen-labels'    List of already parsed footnote
			  labels (string list)
`genealogy'               List of current element's parents
			  (symbol list).
`inherited-properties'    List of inherited properties from
			  parent headlines (plist).
`parent-properties'       List of last element's properties
			 (plist).
`previous-element'        Previous element's type (symbol).
`previous-object'         Previous object's type (symbol).

Return the property list."
  (let* ((type (and (not (stringp blob)) (car blob))))
    (cond
     ;; Case 1: We're moving into a recursive blob.
     (recursep
      (org-combine-plists
       info
       `(:genealogy ,(cons type (plist-get info :genealogy))
		    :previous-element nil
		    :previous-object nil
		    :parent-properties
		    ,(if (memq type org-element-all-elements)
			 (nth 1 blob)
		       (plist-get info :parent-properties))
		    :inherited-properties
		    ,(if (eq type 'headline)
			 (org-combine-plists
			  (plist-get info :inherited-properties) (nth 1 blob))
		       (plist-get info :inherited-properties)))
       ;; Add persistent properties.
       org-export-persistent-properties))
     ;; Case 2: No recursion.
     (t
      ;; At a footnote reference: mark its label as seen, if not
      ;; already the case.
      (when (eq type 'footnote-reference)
	(let ((label (org-element-get-property :label blob))
	      (seen-labels (plist-get org-export-persistent-properties
				      :footnote-seen-labels)))
	  ;; Store anonymous footnotes (nil label) without checking if
	  ;; another anonymous footnote was seen before.
	  (unless (and label (member label seen-labels))
	    (setq info (org-export-set-property
			info :footnote-seen-labels (push label seen-labels))))))
      ;; Set `:previous-element' or `:previous-object' according to
      ;; BLOB.
      (setq info (cond ((not type)
			(org-export-set-property
			 info :previous-object 'plain-text))
		       ((memq type org-element-all-elements)
			(org-export-set-property info :previous-element type))
		       (t (org-export-set-property info :previous-object type))))
      ;; Return updated value.
      info))))

(defun org-export-set-property (info prop value)
  "Set property PROP to VALUE in plist INFO.
Return the new plist."
  (when (memq prop org-export-persistent-properties-list)
    (setq org-export-persistent-properties
	  (plist-put org-export-persistent-properties prop value)))
  (plist-put info prop value))



;;; The Transcoder

;; This function reads Org data (obtained with, i.e.
;; `org-element-parse-buffer') and transcodes it into a specified
;; back-end output.  It takes care of updating local properties,
;; filtering out elements or objects according to export options and
;; organizing the output blank lines and white space are preserved.

;; Though, this function is inapropriate for secondary strings, which
;; require a fresh copy of the plist passed as INFO argument.  Thus,
;; `org-export-secondary-string' is provided for that specific task.

;; Internally, three functions handle the filtering of objects and
;; elements during the export.  More precisely, `org-export-skip-p'
;; determines if the considered object or element should be ignored
;; altogether, `org-export-interpret-p' tells which elements or
;; objects should be seen as real Org syntax and `org-export-expand'
;; transforms the others back into their original shape.

(defun org-export-data (data backend info)
  "Convert DATA to a string into BACKEND format.

DATA is a nested list as returned by `org-element-parse-buffer'.

BACKEND is a symbol among supported exporters.

INFO is a plist holding export options and also used as
a communication channel between elements when walking the nested
list.  See `org-export-update-info' function for more
details.

Return transcoded string."
  (mapconcat
   ;; BLOB can be an element, an object, a string, or nil.
   (lambda (blob)
     (cond
      ((not blob) nil) ((equal blob "") nil)
      ;; BLOB is a string.  Check if the optional transcoder for plain
      ;; text exists, and call it in that case.  Otherwise, simply
      ;; return string.  Also update INFO and call
      ;; `org-export-filter-plain-text-functions'.
      ((stringp blob)
       (setq info (org-export-update-info blob info nil))
       (let ((transcoder (intern (format "org-%s-plain-text" backend))))
	 (org-export-filter-apply-functions
	  org-export-filter-plain-text-functions
	  (if (fboundp transcoder) (funcall transcoder blob info) blob)
	  backend)))
      ;; BLOB is an element or an object.
      (t
       (let* ((type (if (stringp blob) 'plain-text (car blob)))
	      ;; 1. Determine the appropriate TRANSCODER.
	      (transcoder
	       (cond
		;; 1.0 A full Org document is inserted.
		((eq type 'org-data) 'identity)
		;; 1.1. BLOB should be ignored.
		((org-export-skip-p blob info) nil)
		;; 1.2. BLOB shouldn't be transcoded.  Interpret it
		;;      back into Org syntax.
		((not (org-export-interpret-p blob info))
		 'org-export-expand)
		;; 1.3. Else apply naming convention.
		(t (let ((trans (intern
				 (format "org-%s-%s" backend type))))
		     (and (fboundp trans) trans)))))
	      ;; 2. Compute CONTENTS of BLOB.
	      (contents
	       (cond
		;; Case 0. No transcoder defined: ignore BLOB.
		((not transcoder) nil)
		;; Case 1. Transparently export an Org document.
		((eq type 'org-data)
		 (org-export-data blob backend info))
		;; Case 2. For a recursive object.
		((memq type org-element-recursive-objects)
		 (org-export-data
		  blob backend (org-export-update-info blob info t)))
		;; Case 3. For a recursive element.
		((memq type org-element-greater-elements)
		 ;; Ignore contents of an archived tree
		 ;; when `:with-archived-trees' is `headline'.
		 (unless (and
			  (eq type 'headline)
			  (eq (plist-get info :with-archived-trees) 'headline)
			  (org-element-get-property :archivedp blob))
		   (org-element-normalize-string
		    (org-export-data
		     blob backend (org-export-update-info blob info t)))))
		;; Case 4. For a paragraph.
		((eq type 'paragraph)
		 (let ((paragraph
			(org-element-normalize-contents
			 blob
			 ;; When normalizing contents of an item or
			 ;; a footnote definition, ignore first line's
			 ;; indentation: there is none and it might be
			 ;; misleading.
			 (and (not (plist-get info :previous-element))
			      (let ((parent (car (plist-get info :genealogy))))
				(memq parent '(footnote-definition item)))))))
		   (org-export-data
		    paragraph
		    backend
		    (org-export-update-info blob info t))))))
	      ;; 3. Transcode BLOB into RESULTS string.
	      (results (cond
			((not transcoder) nil)
			((eq transcoder 'org-export-expand)
			 (org-export-data
			  `(org-data nil ,(funcall transcoder blob contents))
			  backend info))
			(t (funcall transcoder blob contents info)))))
	 ;; 4. Discard nil results.  Otherwise, update INFO, append
	 ;;    the same white space between elements or objects as in
	 ;;    the original buffer, and call appropriate filters.
	 (when results
	   (setq info (org-export-update-info blob info nil))
	   ;; No filter for a full document.
	   (if (eq type 'org-data)
	       results
	     (org-export-filter-apply-functions
	      (eval (intern (format "org-export-filter-%s-functions" type)))
	      (if (memq type org-element-all-elements)
		  (concat
		   (org-element-normalize-string results)
		   (make-string (org-element-get-property :post-blank blob) 10))
		(concat
		 results
		 (make-string
		  (org-element-get-property :post-blank blob) 32)))
	      backend)))))))
   (org-element-get-contents data) ""))

(defun org-export-secondary-string (secondary backend info)
  "Convert SECONDARY string into BACKEND format.

SECONDARY is a nested list as returned by
`org-element-parse-secondary-string'.

BACKEND is a symbol among supported exporters.

INFO is a plist holding export options and also used as
a communication channel between elements when walking the nested
list.  See `org-export-update-info' function for more
details.

Return transcoded string."
  ;; Make SECONDARY acceptable for `org-export-data'.
  (let ((s (if (listp secondary) secondary (list secondary))))
    (org-export-data `(org-data nil ,@s) backend (copy-sequence info))))

(defun org-export-skip-p (blob info)
  "Non-nil when element or object BLOB should be skipped during export.
INFO is the plist holding export options."
  ;; Check headline.
  (unless (stringp blob)
    (case (car blob)
      ('headline
       (let ((with-tasks (plist-get info :with-tasks))
	     (todo (org-element-get-property :todo-keyword blob))
	     (todo-type (org-element-get-property :todo-type blob))
	     (archived (plist-get info :with-archived-trees))
	     (tag-list (let ((tags (org-element-get-property :tags blob)))
			 (and tags (org-split-string tags ":")))))
	 (or
	  ;; Ignore subtrees with an exclude tag.
	  (loop for k in (plist-get info :exclude-tags)
		thereis (member k tag-list))
	  ;; Ignore subtrees without a select tag, when such tag is found
	  ;; in the buffer.
	  (and (plist-get info :use-select-tags)
	       (loop for k in (plist-get info :select-tags)
		     never (member k tag-list)))
	  ;; Ignore commented sub-trees.
	  (org-element-get-property :commentedp blob)
	  ;; Ignore archived subtrees if `:with-archived-trees' is nil.
	  (and (not archived) (org-element-get-property :archivedp blob))
	  ;; Ignore tasks, if specified by `:with-tasks' property.
	  (and todo (not with-tasks))
	  (and todo
	       (memq with-tasks '(todo done))
	       (not (eq todo-type with-tasks)))
	  (and todo
	       (consp with-tasks)
	       (not (member todo with-tasks))))))
      ;; Check time-stamp.
      ('time-stamp (not (plist-get info :with-timestamps)))
      ;; Check drawer.
      ('drawer
       (or (not (plist-get info :with-drawers))
	   (and (consp (plist-get info :with-drawers))
		(not (member (org-element-get-property :drawer-name blob)
			     (plist-get info :with-drawers))))))
      ;; Check export snippet.
      ('export-snippet
       (let* ((raw-back-end (org-element-get-property :back-end blob))
	      (true-back-end
	       (or (cdr (assoc raw-back-end org-export-snippet-translation-alist))
		   raw-back-end)))
	 (not (string= (symbol-name (plist-get info :back-end))
		       true-back-end)))))))

(defun org-export-interpret-p (blob info)
  "Non-nil if element or object BLOB should be interpreted as Org syntax.
Check is done according to export options INFO, stored as
a plist."
  (case (car blob)
    ;; ... entities...
    (entity (plist-get info :with-entities))
    ;; ... emphasis...
    (emphasis (plist-get info :with-emphasize))
    ;; ... fixed-width areas.
    (fixed-width (plist-get info :with-fixed-width))
    ;; ... footnotes...
    ((footnote-definition footnote-reference)
     (plist-get info :with-footnotes))
    ;; ... sub/superscripts...
    ((subscript superscript)
     (let ((sub/super-p (plist-get info :with-sub-superscript)))
       (if (eq sub/super-p '{})
	   (org-element-get-property :use-brackets-p blob)
	 sub/super-p)))
    ;; ... tables...
    (table (plist-get info :with-tables))
    (otherwise t)))

(defsubst org-export-expand (blob contents)
  "Expand a parsed element or object to its original state.
BLOB is either an element or an object.  CONTENTS is its
contents, as a string or nil."
  (funcall
   (intern (format "org-element-%s-interpreter" (car blob))) blob contents))



;;; The Filter System

;; Filters allow end-users to tweak easily the transcoded output.
;; They are the functional counterpart of hooks, as every filter in
;; a set is applied to the return value of the previous one.

;; Every set is back-end agnostic.  Although, a filter is always
;; called, in addition to the string it applies to, with the back-end
;; used as argument, so it's easy enough for the end-user to add
;; back-end specific filters in the set.

;; Filters sets are defined below. There are of four types:

;; - `org-export-filter-parse-tree-functions' applies directly on the
;;   complete parsed tree.  It's the only filters set that doesn't
;;   apply to a string.
;; - `org-export-filter-final-output-functions' applies to the final
;;   transcoded string.
;; - `org-export-filter-plain-text-functions' applies to any string
;;   not recognized as Org syntax.
;; - `org-export-filter-TYPE-functions' applies on the string returned
;;   after an element or object of type TYPE has been transcoded.

;; All filters sets are applied through
;; `org-export-filter-apply-functions' function.  Filters in a set are
;; applied in reverse order, that is in the order of consing.  It
;; allows developers to be reasonably sure that their filters will be
;; applied first.

;;;; Special Filters
(defvar org-export-filter-parse-tree-functions nil
  "Filter, or list of filters, applied to the parsed tree.
Each filter is called with two arguments: the parse tree, as
returned by `org-element-parse-buffer', and the back-end as
a symbol.  It must return the modified parse tree to transcode.")

(defvar org-export-filter-final-output-functions nil
  "Filter, or list of filters, applied to the transcoded string.
Each filter is called with two arguments: the full transcoded
string, and the back-end as a symbol.  It must return a string
that will be used as the final export output.")

(defvar org-export-filter-plain-text-functions nil
  "Filter, or list of filters, applied to plain text.
Each filter is called with two arguments: a string which contains
no Org syntax, and the back-end as a symbol.  It must return
a string or nil.")


;;;; Elements Filters

(defvar org-export-filter-center-block-functions nil
  "Filter, or list of filters, applied to a transcoded center block.
Each filter is called with two arguments: the transcoded center
block, as a string, and the back-end, as a symbol.  It must
return a string or nil.")

(defvar org-export-filter-drawer-functions nil
  "Filter, or list of filters, applied to a transcoded drawer.
Each filter is called with two arguments: the transcoded drawer,
as a string, and the back-end, as a symbol.  It must return
a string or nil.")

(defvar org-export-filter-dynamic-block-functions nil
  "Filter, or list of filters, applied to a transcoded dynamic-block.
Each filter is called with two arguments: the transcoded
dynamic-block, as a string, and the back-end, as a symbol.  It
must return a string or nil.")

(defvar org-export-filter-headline-functions nil
  "Filter, or list of filters, applied to a transcoded headline.
Each filter is called with two arguments: the transcoded
headline, as a string, and the back-end, as a symbol.  It must
return a string or nil.")

(defvar org-export-filter-inlinetask-functions nil
  "Filter, or list of filters, applied to a transcoded inlinetask.
Each filter is called with two arguments: the transcoded
inlinetask, as a string, and the back-end, as a symbol.  It must
return a string or nil.")

(defvar org-export-filter-plain-list-functions nil
  "Filter, or list of filters, applied to a transcoded plain-list.
Each filter is called with two arguments: the transcoded
plain-list, as a string, and the back-end, as a symbol.  It must
return a string or nil.")

(defvar org-export-filter-item-functions nil
  "Filter, or list of filters, applied to a transcoded item.
Each filter is called with two arguments: the transcoded item, as
a string, and the back-end, as a symbol.  It must return a string
or nil.")

(defvar org-export-filter-comment-functions nil
  "Filter, or list of filters, applied to a transcoded comment.
Each filter is called with two arguments: the transcoded comment,
as a string, and the back-end, as a symbol.  It must return
a string or nil.")

(defvar org-export-filter-comment-block-functions nil
  "Filter, or list of filters, applied to a transcoded comment-comment.
Each filter is called with two arguments: the transcoded
comment-block, as a string, and the back-end, as a symbol.  It
must return a string or nil.")

(defvar org-export-filter-example-block-functions nil
  "Filter, or list of filters, applied to a transcoded example-block.
Each filter is called with two arguments: the transcoded
example-block, as a string, and the back-end, as a symbol.  It
must return a string or nil.")

(defvar org-export-filter-export-block-functions nil
  "Filter, or list of filters, applied to a transcoded export-block.
Each filter is called with two arguments: the transcoded
export-block, as a string, and the back-end, as a symbol.  It
must return a string or nil.")

(defvar org-export-filter-fixed-width-functions nil
  "Filter, or list of filters, applied to a transcoded fixed-width.
Each filter is called with two arguments: the transcoded
fixed-width, as a string, and the back-end, as a symbol.  It must
return a string or nil.")

(defvar org-export-filter-footnote-definition-functions nil
  "Filter, or list of filters, applied to a transcoded footnote-definition.
Each filter is called with two arguments: the transcoded
footnote-definition, as a string, and the back-end, as a symbol.
It must return a string or nil.")

(defvar org-export-filter-horizontal-rule-functions nil
  "Filter, or list of filters, applied to a transcoded horizontal-rule.
Each filter is called with two arguments: the transcoded
horizontal-rule, as a string, and the back-end, as a symbol.  It
must return a string or nil.")

(defvar org-export-filter-keyword-functions nil
  "Filter, or list of filters, applied to a transcoded keyword.
Each filter is called with two arguments: the transcoded keyword,
as a string, and the back-end, as a symbol.  It must return
a string or nil.")

(defvar org-export-filter-latex-environment-functions nil
  "Filter, or list of filters, applied to a transcoded latex-environment.
Each filter is called with two arguments: the transcoded
latex-environment, as a string, and the back-end, as a symbol.
It must return a string or nil.")

(defvar org-export-filter-babel-call-functions nil
  "Filter, or list of filters, applied to a transcoded babel-call.
Each filter is called with two arguments: the transcoded
babel-call, as a string, and the back-end, as a symbol.  It must
return a string or nil.")

(defvar org-export-filter-paragraph-functions nil
  "Filter, or list of filters, applied to a transcoded paragraph.
Each filter is called with two arguments: the transcoded
paragraph, as a string, and the back-end, as a symbol.  It must
return a string or nil.")

(defvar org-export-filter-property-drawer-functions nil
  "Filter, or list of filters, applied to a transcoded property-drawer.
Each filter is called with two arguments: the transcoded
property-drawer, as a string, and the back-end, as a symbol.  It
must return a string or nil.")

(defvar org-export-filter-quote-block-functions nil
  "Filter, or list of filters, applied to a transcoded quote block.
Each filter is called with two arguments: the transcoded quote
block, as a string, and the back-end, as a symbol.  It must
return a string or nil.")

(defvar org-export-filter-quote-section-functions nil
  "Filter, or list of filters, applied to a transcoded quote-section.
Each filter is called with two arguments: the transcoded
quote-section, as a string, and the back-end, as a symbol.  It
must return a string or nil.")

(defvar org-export-filter-special-block-functions nil
  "Filter, or list of filters, applied to a transcoded special block.
Each filter is called with two arguments: the transcoded special
block, as a string, and the back-end, as a symbol.  It must
return a string or nil.")

(defvar org-export-filter-src-block-functions nil
  "Filter, or list of filters, applied to a transcoded src-block.
Each filter is called with two arguments: the transcoded
src-block, as a string, and the back-end, as a symbol.  It must
return a string or nil.")

(defvar org-export-filter-table-functions nil
  "Filter, or list of filters, applied to a transcoded table.
Each filter is called with two arguments: the transcoded table,
as a string, and the back-end, as a symbol.  It must return
a string or nil.")

(defvar org-export-filter-verse-block-functions nil
  "Filter, or list of filters, applied to a transcoded verse block.
Each filter is called with two arguments: the transcoded verse
block, as a string, and the back-end, as a symbol.  It must
return a string or nil.")


;;;; Objects Filters

(defvar org-export-filter-emphasis-functions nil
  "Filter, or list of filters, applied to a transcoded emphasis.
Each filter is called with two arguments: the transcoded
emphasis, as a string, and the back-end, as a symbol.  It must
return a string or nil.")

(defvar org-export-filter-entity-functions nil
  "Filter, or list of filters, applied to a transcoded entity.
Each filter is called with two arguments: the transcoded entity,
as a string, and the back-end, as a symbol.  It must return
a string or nil.")

(defvar org-export-filter-export-snippet-functions nil
  "Filter, or list of filters, applied to a transcoded export-snippet.
Each filter is called with two arguments: the transcoded
export-snippet, as a string, and the back-end, as a symbol.  It
must return a string or nil.")

(defvar org-export-filter-footnote-reference-functions nil
  "Filter, or list of filters, applied to a transcoded footnote-reference.
Each filter is called with two arguments: the transcoded
footnote-reference, as a string, and the back-end, as a symbol.
It must return a string or nil.")

(defvar org-export-filter-inline-babel-call-functions nil
  "Filter, or list of filters, applied to a transcoded inline-babel-call.
Each filter is called with two arguments: the transcoded
inline-babel-call, as a string, and the back-end, as a symbol.  It
must return a string or nil.")

(defvar org-export-filter-inline-src-block-functions nil
  "Filter, or list of filters, applied to a transcoded inline-src-block.
Each filter is called with two arguments: the transcoded
inline-src-block, as a string, and the back-end, as a symbol.  It
must return a string or nil.")

(defvar org-export-filter-latex-fragment-functions nil
  "Filter, or list of filters, applied to a transcoded latex-fragment.
Each filter is called with two arguments: the transcoded
latex-fragment, as a string, and the back-end, as a symbol.  It
must return a string or nil.")

(defvar org-export-filter-line-break-functions nil
  "Filter, or list of filters, applied to a transcoded line-break.
Each filter is called with two arguments: the transcoded
line-break, as a string, and the back-end, as a symbol.  It must
return a string or nil.")

(defvar org-export-filter-link-functions nil
  "Filter, or list of filters, applied to a transcoded link.
Each filter is called with two arguments: the transcoded link, as
a string, and the back-end, as a symbol.  It must return a string
or nil.")

(defvar org-export-filter-macro-functions nil
  "Filter, or list of filters, applied to a transcoded macro.
Each filter is called with two arguments: the transcoded macro,
as a string, and the back-end, as a symbol.  It must return
a string or nil.")

(defvar org-export-filter-radio-target-functions nil
  "Filter, or list of filters, applied to a transcoded radio-target.
Each filter is called with two arguments: the transcoded
radio-target, as a string, and the back-end, as a symbol.  It
must return a string or nil.")

(defvar org-export-filter-statistics-cookie-functions nil
  "Filter, or list of filters, applied to a transcoded statistics-cookie.
Each filter is called with two arguments: the transcoded
statistics-cookie, as a string, and the back-end, as a symbol.
It must return a string or nil.")

(defvar org-export-filter-subscript-functions nil
  "Filter, or list of filters, applied to a transcoded subscript.
Each filter is called with two arguments: the transcoded
subscript, as a string, and the back-end, as a symbol.  It must
return a string or nil.")

(defvar org-export-filter-superscript-functions nil
  "Filter, or list of filters, applied to a transcoded superscript.
Each filter is called with two arguments: the transcoded
superscript, as a string, and the back-end, as a symbol.  It must
return a string or nil.")

(defvar org-export-filter-target-functions nil
  "Filter, or list of filters, applied to a transcoded target.
Each filter is called with two arguments: the transcoded target,
as a string, and the back-end, as a symbol.  It must return
a string or nil.")

(defvar org-export-filter-time-stamp-functions nil
  "Filter, or list of filters, applied to a transcoded time-stamp.
Each filter is called with two arguments: the transcoded
time-stamp, as a string, and the back-end, as a symbol.  It must
return a string or nil.")

(defvar org-export-filter-verbatim-functions nil
  "Filter, or list of filters, applied to a transcoded verbatim.
Each filter is called with two arguments: the transcoded
verbatim, as a string, and the back-end, as a symbol.  It must
return a string or nil.")

(defun org-export-filter-apply-functions (filters value backend)
  "Call every function in FILTERS with arguments VALUE and BACKEND.
Functions are called in reverse order, to be reasonably sure that
developer-specified filters, if any, are called first."
  ;; Ensure FILTERS is a list.
  (let ((filters (if (listp filters) (reverse filters) (list filters))))
    (loop for filter in filters
	  if (not value) return nil else
	  do (setq value (funcall filter value backend))))
  value)



;;; Core functions

;; This is the room for the main function, `org-export-as', along with
;; its derivatives, `org-export-to-buffer' and `org-export-to-file'.
;; They differ only by the way they output the resulting code.

;; Note that `org-export-as' doesn't really parse the current buffer,
;; but a copy of it (with the same buffer-local variables and
;; visibility), where Babel blocks are executed, if appropriate.
;; `org-export-with-current-buffer-copy' macro prepares that copy.

(defun org-export-as (backend
		      &optional subtreep visible-only body-only ext-plist)
  "Transcode current Org buffer into BACKEND code.

If narrowing is active in the current buffer, only transcode its
narrowed part.

If a region is active, transcode that region.

When optional argument SUBTREEP is non-nil, transcode the
sub-tree at point, extracting information from the headline
properties first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only return body
code, without preamble nor postamble.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return code as a string."
  (save-excursion
    (save-restriction
      ;; Narrow buffer to an appropriate region for parsing.
      (when (org-region-active-p)
	(narrow-to-region (region-beginning) (region-end)))
      (goto-char (point-min))
      (when subtreep
	(unless (org-at-heading-p)
	  (org-with-limited-levels (outline-next-heading)))
	(let ((end (save-excursion (org-end-of-subtree t)))
	      (begin (progn (forward-line)
			    (org-skip-whitespace)
			    (point-at-bol))))
	  (narrow-to-region begin end)))
      ;; Retrieve export options (INFO) and parsed tree (RAW-DATA).
      ;; Buffer isn't parsed directly.  Instead, a temporary copy is
      ;; created, where all code blocks are evaluated.  RAW-DATA is
      ;; the parsed tree of the buffer resulting from that process.
      ;; Eventually call `org-export-filter-parse-tree-functions'..
      (let ((info (org-export-collect-options backend subtreep ext-plist))
	    (raw-data (org-export-filter-apply-functions
		       org-export-filter-parse-tree-functions
		       (org-export-with-current-buffer-copy
			(org-export-blocks-preprocess)
			(org-element-parse-buffer nil visible-only))
		       backend)))
	;; Initialize the communication system and combine it to INFO.
	(setq info
	      (org-combine-plists
	       info
	       (org-export-initialize-persistent-properties
		raw-data info backend)))
	;; Now transcode RAW-DATA.  Also call
	;; `org-export-filter-final-output-functions'.
	(let ((body (org-element-normalize-string
		     (org-export-data raw-data backend info)))
	      (template (intern (format "org-%s-template" backend))))
	  (if (and (not body-only) (fboundp template))
	      (org-trim
	       (org-export-filter-apply-functions
		org-export-filter-final-output-functions
		(funcall template body info)
		backend))
	    (org-export-filter-apply-functions
	     org-export-filter-final-output-functions body backend)))))))

(defun org-export-to-buffer (backend buffer &optional subtreep visible-only
				     body-only ext-plist)
  "Call `org-export-as' with output to a specified buffer.

BACKEND is the back-end used for transcoding, as a symbol.

BUFFER is the output buffer.  If it already exists, it will be
erased first, otherwise, it will be created.

Arguments SUBTREEP, VISIBLE-ONLY, BODY-ONLY and EXT-PLIST are
similar to those used in `org-export-as', which see.

Return buffer."
  (let ((out (org-export-as backend subtreep visible-only body-only ext-plist))
	(buffer (get-buffer-create buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert out)
      (goto-char (point-min)))
    buffer))

(defun org-export-to-file (backend filename &optional post-process subtreep
                                   visible-only body-only ext-plist)
  "Call `org-export-as' with output to a specified file.

BACKEND is the back-end used for transcoding, as a symbol.

FILENAME is the output file name.  If it already exists, it will
be erased first, unless it isn't writable, in which case an error
will be returned.  Otherwise, the file will be created.

Optional argument POST-PROCESS, when non-nil, is a function
applied to the output file.  It expects one argument: the file
name, as a string.  It can be used to call shell commands on that
file, display a specific buffer, etc.

Optional arguments SUBTREEP, VISIBLE-ONLY, BODY-ONLY and
EXT-PLIST are similar to those used in `org-export-as', which
see.

Return file name."
  ;; Checks for file and directory permissions.
  (cond
   ((not (file-exists-p filename))
    (let ((dir (or (file-name-directory filename) default-directory)))
      (unless (file-writable-p dir) (error "Output directory not writable"))))
   ((not (file-writable-p filename)) (error "Output file not writable")))
  ;; All checks passed: insert contents to a temporary buffer and
  ;; write it to the specified file.
  (let ((out (org-export-as backend subtreep visible-only body-only ext-plist)))
    (with-temp-buffer
      (insert out)
      (write-file filename)))
  (when post-process (funcall post-process filename))
  ;; Return value.
  filename)

(defmacro org-export-with-current-buffer-copy (&rest body)
  "Apply BODY in a copy of the current buffer.

The copy preserves local variables and visibility of the original
buffer.

Point is at buffer's beginning when BODY is applied."
  (org-with-gensyms (original-buffer offset buffer-string overlays)
    `(let ((,original-buffer ,(current-buffer))
	   (,offset ,(1- (point-min)))
	   (,buffer-string ,(buffer-string))
	   (,overlays (mapcar
		       'copy-overlay (overlays-in (point-min) (point-max)))))
       (with-temp-buffer
	 (let ((buffer-invisibility-spec nil))
	   (org-clone-local-variables
	    ,original-buffer "^\\(org-\\|orgtbl-\\|major-mode$\\)")
	   (insert ,buffer-string)
	   (mapc (lambda (ov)
		   (move-overlay
		    ov
		    (- (overlay-start ov) ,offset)
		    (- (overlay-end ov) ,offset)
		    (current-buffer)))
		 ,overlays)
	   (goto-char (point-min))
	   (progn ,@body))))))
(def-edebug-spec org-export-with-current-buffer-copy (body))



;;; Tools For Back-Ends

;; A whole set of tools is available to help build new exporters.  Any
;; function general enough to have its use across many back-ends
;; should be added here.

;; As of now, functions operating on footnotes, headlines, include
;; keywords, links, macros, references, src-blocks, tables and tables
;; of contents are implemented.

;;;; For Footnotes

;; `org-export-collect-footnote-definitions' is a tool to list
;; actually used footnotes definitions in the whole parse tree, or in
;; an headline, in order to add footnote listings throughout the
;; transcoded data.

;; `org-export-footnote-first-reference-p' is a predicate used by some
;; back-ends, when they need to attach the footnote definition only to
;; the first occurrence of the corresponding label.

;; `org-export-get-footnote-definition' and
;; `org-export-get-footnote-number' provide easier access to
;; additional information relative to a footnote reference.

(defun org-export-collect-footnote-definitions (data info)
  "Return an alist between footnote numbers, labels and definitions.

DATA is the parse tree from which definitions are collected.
INFO is the plist used as a communication channel.

Definitions are sorted by order of references.  They either
appear as Org data \(transcoded with `org-export-data'\) or as
a secondary string for inlined footnotes \(transcoded with
`org-export-secondary-string'\).  Unreferenced definitions are
ignored."
  (org-element-map
   data 'footnote-reference
   (lambda (footnote local)
     (when (org-export-footnote-first-reference-p footnote local)
       (list (org-export-get-footnote-number footnote local)
	     (org-element-get-property :label footnote)
	     (org-export-get-footnote-definition footnote local))))
   info))

(defun org-export-footnote-first-reference-p (footnote-reference info)
  "Non-nil when a footnote reference is the first one for its label.

FOOTNOTE-REFERENCE is the footnote reference being considered.
INFO is the plist used as a communication channel."
  (let ((label (org-element-get-property :label footnote-reference)))
    (not (and label (member label (plist-get info :footnote-seen-labels))))))

(defun org-export-get-footnote-definition (footnote-reference info)
  "Return definition of FOOTNOTE-REFERENCE as parsed data.
INFO is the plist used as a communication channel."
  (let ((label (org-element-get-property :label footnote-reference)))
    (or (org-element-get-property :inline-definition footnote-reference)
        (cdr (assoc label (plist-get info :footnote-definition-alist))))))

(defun org-export-get-footnote-number (footnote info)
  "Return number associated to a footnote.

FOOTNOTE is either a footnote reference or a footnote definition.
INFO is the plist used as a communication channel."
  (let ((label (org-element-get-property :label footnote)))
    (if (eq (car footnote) 'footnote-definition)
	;; If a footnote definition was provided, first search for
	;; a relative footnote reference, as only footnote references
	;; can determine the associated ordinal.
	(org-element-map
	 (plist-get info :parse-tree) 'footnote-reference
	 (lambda (foot-ref local)
	   (when (string= (org-element-get-property :label foot-ref) label)
	     (let* ((all-seen (plist-get info :footnote-seen-labels))
		    (seenp (and label (member label all-seen))))
	       (if seenp (length seenp) (1+ (length all-seen))))))
	 info 'first-match)
      (let* ((all-seen (plist-get info :footnote-seen-labels))
	     ;; Anonymous footnotes are always new footnotes.
	     (seenp (and label (member label all-seen))))
	(if seenp (length seenp) (1+ (length all-seen)))))))


;;;; For Headlines

;; `org-export-get-relative-level' is a shortcut to get headline
;; level, relatively to the lower headline level in the parsed tree.

;; `org-export-get-headline-number' returns the section number of an
;; headline, while `org-export-number-to-roman' allows to convert it
;; to roman numbers.

;; `org-export-first-sibling-p' and `org-export-last-sibling-p' are
;; two useful predicates when it comes to fulfill the
;; `:headline-levels' property.

(defun org-export-get-relative-level (headline info)
  "Return HEADLINE relative level within current parsed tree.
INFO is a plist holding contextual information."
  (+ (org-element-get-property :level headline)
     (or (plist-get info :headline-offset) 0)))

(defun org-export-get-headline-number (headline info)
  "Return HEADLINE numbering as a list of numbers.
INFO is a plist holding contextual information."
  (cdr (assq (org-element-get-property :begin headline)
	     (plist-get info :headline-numbering))))

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

(defun org-export-first-sibling-p (headline info)
  "Non-nil when HEADLINE is the first sibling in its sub-tree.
INFO is the plist used as a communication channel."
  (not (eq (plist-get info :previous-element) 'headline)))

(defun org-export-last-sibling-p (headline info)
  "Non-nil when HEADLINE is the last sibling in its sub-tree.
INFO is the plist used as a communication channel."
  (= (org-element-get-property :end headline)
     (or (plist-get (plist-get info :parent-properties) :end)
	 (plist-get info :point-max))))


;;;; For Include Keywords

;; This section provides a tool to properly handle insertion of files
;; during export: `org-export-included-files'.  It recursively
;; transcodes a file specfied by an include keyword.

;; It uses two helper functions: `org-export-get-file-contents'
;; returns contents of a file according to parameters specified in the
;; keyword while `org-export-parse-included-file' parses the file
;; specified by it.

(defun org-export-included-file (keyword backend info)
  "Transcode file specified with include KEYWORD.

KEYWORD is the include keyword element transcoded.  BACKEND is
the language back-end used for transcoding.  INFO is the plist
used as a communication channel.

This function updates `:included-files' and `:headline-offset'
properties.

Return the transcoded string."
  (let ((data (org-export-parse-included-file keyword info))
	(file (let ((value (org-element-get-property :value keyword)))
		(and (string-match "^\"\\(\\S-+\\)\"" value)
		     (match-string 1 value)))))
    (org-element-normalize-string
     (org-export-data
      data backend
      (org-combine-plists
       info
       ;; Store full path of already included files to avoid
       ;; recursive file inclusion.
       `(:included-files
	 ,(cons (expand-file-name file) (plist-get info :included-files))
	 ;; Ensure that a top-level headline in the included
	 ;; file becomes a direct child of the current headline
	 ;; in the buffer.
	 :headline-offset
	 ,(- (+ (plist-get (plist-get info :inherited-properties) :level)
		(plist-get info :headline-offset))
	     (1- (org-export-get-min-level data info)))))))))

(defun org-export-get-file-contents (file &optional lines)
  "Get the contents of FILE and return them as a string.
When optional argument LINES is a string specifying a range of
lines, include only those lines."
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
    (buffer-string)))

(defun org-export-parse-included-file (keyword info)
  "Parse file specified by include KEYWORD.

KEYWORD is the include keyword element transcoded. BACKEND is the
language back-end used for transcoding. INFO is the plist used as
a communication channel.

Return the parsed tree."
  (let* ((value (org-element-get-property :value keyword))
	 (file (and (string-match "^\"\\(\\S-+\\)\"" value)
		    (prog1 (match-string 1 value)
		      (setq value (replace-match "" nil nil value)))))
	 (lines (and (string-match
		      ":lines +\"\\(\\(?:[0-9]+\\)?-\\(?:[0-9]+\\)?\\)\"" value)
		     (prog1 (match-string 1 value)
		       (setq value (replace-match "" nil nil value)))))
	 (env (cond ((string-match "\\<example\\>" value) "example")
		    ((string-match "\\<src\\(?: +\\(.*\\)\\)?" value)
		     (match-string 1 value)))))
    (cond
     ((or (not file)
	  (not (file-exists-p file))
	  (not (file-readable-p file)))
      (format "Cannot include file %s" file))
     ((and (not env)
	   (member (expand-file-name file) (plist-get info :included-files)))
      (error "Recursive file inclusion: %S" file))
     (t (let ((raw (org-element-normalize-string
		    (org-export-get-file-contents
		     (expand-file-name file) lines))))
	  ;; If environment isn't specified, Insert file in
	  ;; a temporary buffer and parse it as Org syntax.
	  ;; Otherwise, build the element representing the file.
	  (cond
	   ((not env)
	    (with-temp-buffer
	      (insert raw) (org-mode) (org-element-parse-buffer)))
	   ((string= "example" env)
	    `(org-data nil (example-block (:value ,raw :post-blank 0))))
	   (t
	    `(org-data
	      nil
	      (src-block (:value ,raw :language ,env :post-blank 0))))))))))


;;;; For Links

;; `org-export-solidify-link-text' turns a string into a safer version
;; for links, replacing most non-standard characters with hyphens.

;; `org-export-get-coderef-format' returns an appropriate format
;; string for coderefs.

;; `org-export-inline-image-p' returns a non-nil value when the link
;; provided should be considered as an inline image.

;; `org-export-resolve-fuzzy-link' searches destination of fuzzy links
;; (i.e. links with "fuzzy" as type) within the parsed tree, and
;; returns an appropriate unique identifier when found, or nil.

(defun org-export-solidify-link-text (s)
  "Take link text and make a safe target out of it."
  (save-match-data
    (mapconcat 'identity (org-split-string s "[^a-zA-Z0-9_\\.-]+") "-")))

(defun org-export-get-coderef-format (path desc)
  "Return format string for code reference link.
PATH is the link path.  DESC is its description."
  (save-match-data
    (cond ((string-match (regexp-quote (concat "(" path ")")) desc)
	   (replace-match "%s" t t desc))
	  ((string= desc "") "%s")
	  (t desc))))

(defun org-export-inline-image-p (link &optional extensions)
  "Non-nil if LINK object points to an inline image.

When non-nil, optional argument EXTENSIONS is a list of valid
extensions for image files, as strings.  Otherwise, a default
list is provided \(cf. `org-image-file-name-regexp'\)."
  (and (not (org-element-get-contents link))
       (string= (org-element-get-property :type link) "file")
       (org-file-image-p
	(expand-file-name (org-element-get-property :path link))
	extensions)))

(defun org-export-resolve-fuzzy-link (link info)
  "Return an unique identifier for LINK destination.

INFO is a plist holding contextual information.

Return value can be a string, an buffer position, or nil:

- If LINK path exactly matches any target, return its name as the
  identifier.

- If LINK path exactly matches any headline name, return
  headline's beginning position as the identifier.  If more than
  one headline share that name, priority will be given to the one
  with the closest common ancestor, if any, or the first one in
  the parse tree otherwise.

- Otherwise, return nil.

Assume LINK type is \"fuzzy\"."
  (let ((path (org-element-get-property :path link)))
    (if (member path (plist-get info :target-list))
	;; Link points to a target: return its name as a string.
	path
      ;; Link either points to an headline or nothing.  Try to find
      ;; the source, with priority given to headlines with the closest
      ;; common ancestor.  If such candidate is found, return its
      ;; beginning position as an unique identifier, otherwise return
      ;; nil.
      (let* ((head-alist (plist-get info :headline-alist))
	     (link-begin (org-element-get-property :begin link))
	     (link-end (org-element-get-property :end link))
	     ;; Store candidates as a list of cons cells holding their
	     ;; beginning and ending position.
	     (cands (loop for head in head-alist
			  when (string= (car head) path)
			  collect (cons (nth 1 head) (nth 2 head)))))
	(cond
	 ;; No candidate: return nil.
	 ((not cands) nil)
	 ;; If one or more candidates share common ancestors with
	 ;; LINK, return beginning position of the first one matching
	 ;; the closer ancestor shared.
	 ((let ((ancestors (loop for head in head-alist
				 when (and (> link-begin (nth 1 head))
					   (<= link-end (nth 2 head)))
				 collect (cons (nth 1 head) (nth 2 head)))))
	    (loop named main for ancestor in (nreverse ancestors) do
		  (loop for candidate in cands
			when (and (>= (car candidate) (car ancestor))
				  (<= (cdr candidate) (cdr ancestor)))
			do (return-from main (car candidate))))))
	 ;; No candidate have a common ancestor with link: First match
	 ;; will do.  Return its beginning position.
	 (t (caar cands)))))))


;;;; For Macros

;; `org-export-expand-macro' simply takes care of expanding macros.

(defun org-export-expand-macro (macro info)
  "Expand MACRO and return it as a string.
INFO is a plist holding export options."
  (let* ((key (org-element-get-property :key macro))
	 (args (org-element-get-property :args macro))
	 (value (plist-get info (intern (format ":macro-%s" key)))))
    ;; Replace arguments in VALUE.
    (let ((s 0) n)
      (while (string-match "\\$\\([0-9]+\\)" value s)
	(setq s (1+ (match-beginning 0))
	      n (string-to-number (match-string 1 value)))
	(and (>= (length args) n)
	     (setq value (replace-match (nth (1- n) args) t t value)))))
    ;; VALUE starts with "(eval": it is a s-exp, `eval' it.
    (when (string-match "\\`(eval\\>" value)
      (setq value (eval (read value))))
    ;; Return expanded string.
    (format "%s" value)))


;;;; For References

;; `org-export-get-ordinal' associates a sequence number to any object
;; or element.

(defun org-export-get-ordinal (element info &optional within-section predicate)
  "Return ordinal number of an element or object.

ELEMENT is the element or object considered.  INFO is the plist
used as a communication channel.

When optional argument WITHIN-SECTION is non-nil, narrow counting
to the section containing ELEMENT.

Optional argument PREDICATE is a function returning a non-nil
value if the current element or object should be counted in.  It
accepts one argument: the element or object being considered.
This argument allows to count only a certain type of objects,
like inline images, which are a subset of links \(in that case,
`org-export-inline-image-p' might be an useful predicate\)."
  (let ((counter 0)
        (type (car element))
        ;; Determine if search should apply to current section, in
        ;; which case it should be retrieved first, or to full parse
        ;; tree.  As a special case, an element or object without
        ;; a parent headline will also trigger a full search,
        ;; notwithstanding WITHIN-SECTION value.
        (data
         (let ((parse-tree (plist-get info :parse-tree)))
           (if within-section
               (let ((parent (plist-get (plist-get info :inherited-properties)
                                        :begin)))
                 (if (not parent) parse-tree
                   (org-element-map
                    parse-tree 'headline
                    (lambda (el local)
                      (when (= (org-element-get-property :begin el) parent) el))
                    info 'first-match)))
             parse-tree))))
    ;; Increment counter until ELEMENT is found again.
    (org-element-map
     data type
     (lambda (el local)
       (cond
        ((and (functionp predicate) (funcall predicate el)))
        ((equal element el) (1+ counter))
        (t (incf counter) nil)))
     info 'first-match)))


;;;; For Src-Blocks

;; `org-export-handle-code' takes care of line numbering and reference
;; cleaning in source code, when appropriate.  It also updates global
;; LOC count (`:total-loc' property) and code references alist
;; (`:code-refs' property).

(defun org-export-handle-code (code switches info
					    &optional language num-fmt ref-fmt)
  "Handle line numbers and code references in CODE.

CODE is the string to process.  SWITCHES is the option string
determining which changes will be applied to CODE.  INFO is the
plist used as a communication channel during export.

Optional argument LANGUAGE, when non-nil, is a string specifying
code's language.

If optional argument NUM-FMT is a string, it will be used as
a format string for numbers at beginning of each line.

If optional argument REF-FMT is a string, it will be used as
a format string for each line of code containing a reference.

Update the following INFO properties by side-effect: `:total-loc'
and `:code-refs'.

Return new code as a string."
  (let* ((switches (or switches ""))
	 (numberp (string-match "[-+]n\\>" switches))
	 (continuep (string-match "\\+n\\>" switches))
	 (total-LOC (if (and numberp (not continuep))
			0
		      (or (plist-get info :total-loc) 0)))
	 (preserve-indent-p (or org-src-preserve-indentation
				(string-match "-i\\>" switches)))
	 (replace-labels (when (string-match "-r\\>" switches)
			   (if (string-match "-k\\>" switches) 'keep t)))
	 ;; Get code and clean it.  Remove blank lines at its
	 ;; beginning and end.  Also remove protective commas.
	 (code (let ((c (replace-regexp-in-string
			 "\\`\\([ \t]*\n\\)+" ""
			 (replace-regexp-in-string
			  "\\(:?[ \t]*\n\\)*[ \t]*\\'" "\n" code))))
		 ;; If appropriate, remove global indentation.
		 (unless preserve-indent-p (setq c (org-remove-indentation c)))
		 ;; Free up the protected lines.  Note: Org blocks
		 ;; have commas at the beginning or every line.
		 (if (string= language "org")
		     (replace-regexp-in-string "^," "" c)
		   (replace-regexp-in-string
		    "^\\(,\\)\\(:?\\*\\|[ \t]*#\\+\\)" "" c nil nil 1))))
	 ;; Split code to process it line by line.
	 (code-lines (org-split-string code "\n"))
	 ;; Ensure line numbers will be correctly padded before
	 ;; applying the format string.
	 (num-fmt (format (if (stringp num-fmt) num-fmt "%s:  ")
			  (format "%%%ds"
				  (length (number-to-string
					   (+ (length code-lines)
					      total-LOC))))))
	 ;; Get format used for references.
	 (label-fmt (or (and (string-match "-l +\"\\([^\"\n]+\\)\"" switches)
			     (match-string 1 switches))
			org-coderef-label-format))
	 ;; Build a regexp matching a loc with a reference.
	 (with-ref-re (format "^.*?\\S-.*?\\([ \t]*\\(%s\\)\\)[ \t]*$"
			      (replace-regexp-in-string
			       "%s" "\\([-a-zA-Z0-9_ ]+\\)" label-fmt nil t)))
	 coderefs)
    (org-element-normalize-string
     (mapconcat (lambda (loc)
		  ;; Maybe add line number to current line of code
		  ;; (LOC).
		  (when numberp
		    (setq loc (concat (format num-fmt (incf total-LOC)) loc)))
		  ;; Take action if at a ref line.
		  (when (string-match with-ref-re loc)
		    (let ((ref (match-string 3 loc)))
		      (setq loc
			    (cond
			     ;; Option "-k": don't remove labels.  Use
			     ;; numbers for references when lines are
			     ;; numbered, use labels otherwise.
			     ((eq replace-labels 'keep)
			      (let ((full-ref (format "(%s)" ref)))
				(push (cons ref (if numberp total-LOC full-ref))
				      coderefs)
				(replace-match full-ref nil nil loc 2))
			      (replace-match (format "(%s)" ref) nil nil loc 2))
			     ;; Option "-r" without "-k": remove labels.
			     ;; Use numbers for references when lines are
			     ;; numbered, use labels otherwise.
			     (replace-labels
			      (push (cons ref (if numberp total-LOC ref))
				    coderefs)
			      (replace-match "" nil nil loc 1))
			     ;; Else: don't remove labels and don't use
			     ;; numbers for references.
			     (t
			      (let ((full-ref (format "(%s)" ref)))
				(push (cons ref full-ref) coderefs)
				(replace-match full-ref nil nil loc 2)))))))
		  ;; If REF-FMT is defined, apply it to current LOC.
		  (when (stringp ref-fmt) (setq loc (format ref-fmt loc)))
		  ;; Update by side-effect communication channel.
		  ;; Return updated LOC.
		  (setq info (org-export-set-property
			      (org-export-set-property
			       info :code-refs coderefs)
			      :total-loc total-LOC))
		  loc)
		code-lines "\n"))))


;;;; For Tables

;; `org-export-table-format-info' extracts formatting information
;; (alignment, column groups and presence of a special column) from
;; a raw table and returns it as a property list.
;;
;; `org-export-clean-table' cleans the raw table from any Org
;; table-specific syntax.

(defun org-export-table-format-info (table)
  "Extract info from TABLE.
Return a plist whose properties and values are:
`:alignment'        vector of strings among \"r\", \"l\" and \"c\",
`:column-groups'    vector of symbols among `start', `end', `start-end',
`:row-groups'       list of integers representing row groups.
`:special-column-p' non-nil if table has a special column.
`:width'            vector of integers representing desired width of
		    current column, or nil."
  (with-temp-buffer
    (insert table)
    (goto-char 1)
    (org-table-align)
    (let ((align (vconcat (mapcar (lambda (c) (if c "r" "l"))
				  org-table-last-alignment)))
	  (width (make-vector (length org-table-last-alignment) nil))
	  (colgroups (make-vector (length org-table-last-alignment) nil))
	  (row-group 0)
	  (rowgroups)
	  (special-column-p 'empty))
      (mapc (lambda (row)
	      (if (string-match "^[ \t]*|[-+]+|[ \t]*$" row)
		  (incf row-group)
		(push row-group rowgroups)
		;; Determine if a special column is present by looking
		;; for special markers in the first column.  More
		;; accurately, the first column is considered special
		;; if it only contains special markers and, maybe,
		;; empty cells.
		(setq special-column-p
		      (cond
		       ((not special-column-p) nil)
		       ((string-match "^[ \t]*| *\\\\?\\([\#!$*_^]\\) *|"
				      row) 'special)
		       ((string-match "^[ \t]*| +|" row) special-column-p))))
	      (cond
	       ;; Read forced alignment and width information, if any,
	       ;; and determine final alignment for the table.
	       ((org-table-cookie-line-p row)
		(let ((col 0))
		  (mapc (lambda (field)
			  (when (string-match "<\\([lrc]\\)\\([0-9]+\\)?>" field)
			    (aset align col (match-string 1 field))
			    (aset width col (let ((w (match-string 2 field)))
					      (and w (string-to-number w)))))
			  (incf col))
			(org-split-string row "[ \t]*|[ \t]*"))))
	       ;; Read column groups information.
	       ((org-table-colgroup-line-p row)
		(let ((col 0))
		  (mapc (lambda (field)
			  (aset colgroups col
				(cond ((string= "<" field) 'start)
				      ((string= ">" field) 'end)
				      ((string= "<>" field) 'start-end)))
			  (incf col))
			(org-split-string row "[ \t]*|[ \t]*"))))))
	    (org-split-string table "\n"))
      ;; Return plist.
      (list :alignment align
	    :column-groups colgroups
	    :row-groups (reverse rowgroups)
	    :special-column-p (eq special-column-p 'special)
	    :width width))))

(defun org-export-clean-table (table specialp)
  "Clean string TABLE from its formatting elements.
Remove any row containing column groups or formatting cookies and
rows starting with a special marker.  If SPECIALP is non-nil,
assume the table contains a special formatting column and remove
it also."
  (let ((rows (org-split-string table "\n")))
    (mapconcat 'identity
	       (delq nil
		     (mapcar
		      (lambda (row)
			(cond
			 ((org-table-colgroup-line-p row) nil)
			 ((org-table-cookie-line-p row) nil)
			 ;; Ignore rows starting with a special marker.
			 ((string-match "^[ \t]*| *[!_^/] *|" row) nil)
			 ;; Remove special column.
			 ((and specialp
			       (or (string-match "^\\([ \t]*\\)|-+\\+" row)
				   (string-match "^\\([ \t]*\\)|[^|]*|" row)))
			  (replace-match "\\1|" t nil row))
			 (t row)))
		      rows))
	       "\n")))


;;;; For Tables Of Contents

;; `org-export-collect-headlines' builds a list of all exportable
;; headline elements, maybe limited to a certain depth.  One can then
;; easily parse it and transcode it.

;; Building lists of tables, figures or listings is quite similar.
;; Once the generic function `org-export-collect-elements' is defined,
;; `org-export-collect-tables', `org-export-collect-figures' and
;; `org-export-collect-listings' can be derived from it.

(defun org-export-collect-headlines (info &optional n)
  "Collect headlines in order to build a table of contents.

When non-nil, optional argument N must be an integer.  It
specifies the depth of the table of contents.

Return a list of all exportable headlines as parsed elements."
  (org-element-map
   (plist-get info :parse-tree)
   'headline
   (lambda (headline local)
     ;; Strip contents from HEADLINE.
     (let ((relative-level (org-export-get-relative-level headline local)))
       (unless (and n (> relative-level n)) headline)))
   info))

(defun org-export-collect-elements (type backend info)
  "Collect named elements of type TYPE.

Only elements with a caption or a name are collected.

BACKEND is the back-end used to transcode their caption or name.
INFO is a plist holding export options.

Return an alist where key is entry's name and value an unique
identifier that might be used for internal links."
  (org-element-map
   (plist-get info :parse-tree)
   type
   (lambda (element info)
     (let ((entry
	    (cond
	     ((org-element-get-property :caption element)
	      (org-export-secondary-string
	       (org-element-get-property :caption element) backend info))
	     ((org-element-get-property :name element)
	      (org-export-secondary-string
	       (org-element-get-property :name element) backend info)))))
       ;; Skip elements with neither a caption nor a name.
       (when entry (cons entry (org-element-get-property :begin element)))))
   info))

(defun org-export-collect-tables (backend info)
  "Build a list of tables.

BACKEND is the back-end used to transcode table's name.  INFO is
a plist holding export options.

Return an alist where key is the caption of the table and value
an unique identifier that might be used for internal links."
  (org-export-collect-elements 'table backend info))

(defun org-export-collect-figures (backend info)
  "Build a list of figures.

A figure is a paragraph type element with a caption or a name.

BACKEND is the back-end used to transcode headline's name.  INFO
is a plist holding export options.

Return an alist where key is the caption of the figure and value
an unique indentifier that might be used for internal links."
  (org-export-collect-elements 'paragraph backend info))

(defun org-export-collect-listings (backend info)
  "Build a list of src blocks.

BACKEND is the back-end used to transcode src block's name.  INFO
is a plist holding export options.

Return an alist where key is the caption of the src block and
value an unique indentifier that might be used for internal
links."
  (org-export-collect-elements 'src-block backend info))


(provide 'org-export)
;;; org-export.el ends here
