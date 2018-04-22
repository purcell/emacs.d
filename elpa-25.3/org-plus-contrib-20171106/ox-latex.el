;;; ox-latex.el --- LaTeX Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2017 Free Software Foundation, Inc.

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
;; See Org manual for details.

;;; Code:

(require 'cl-lib)
(require 'ox)
(require 'ox-publish)

(defvar org-latex-default-packages-alist)
(defvar org-latex-packages-alist)
(defvar orgtbl-exp-regexp)



;;; Define Back-End

(org-export-define-backend 'latex
  '((bold . org-latex-bold)
    (center-block . org-latex-center-block)
    (clock . org-latex-clock)
    (code . org-latex-code)
    (drawer . org-latex-drawer)
    (dynamic-block . org-latex-dynamic-block)
    (entity . org-latex-entity)
    (example-block . org-latex-example-block)
    (export-block . org-latex-export-block)
    (export-snippet . org-latex-export-snippet)
    (fixed-width . org-latex-fixed-width)
    (footnote-definition . org-latex-footnote-definition)
    (footnote-reference . org-latex-footnote-reference)
    (headline . org-latex-headline)
    (horizontal-rule . org-latex-horizontal-rule)
    (inline-src-block . org-latex-inline-src-block)
    (inlinetask . org-latex-inlinetask)
    (italic . org-latex-italic)
    (item . org-latex-item)
    (keyword . org-latex-keyword)
    (latex-environment . org-latex-latex-environment)
    (latex-fragment . org-latex-latex-fragment)
    (line-break . org-latex-line-break)
    (link . org-latex-link)
    (node-property . org-latex-node-property)
    (paragraph . org-latex-paragraph)
    (plain-list . org-latex-plain-list)
    (plain-text . org-latex-plain-text)
    (planning . org-latex-planning)
    (property-drawer . org-latex-property-drawer)
    (quote-block . org-latex-quote-block)
    (radio-target . org-latex-radio-target)
    (section . org-latex-section)
    (special-block . org-latex-special-block)
    (src-block . org-latex-src-block)
    (statistics-cookie . org-latex-statistics-cookie)
    (strike-through . org-latex-strike-through)
    (subscript . org-latex-subscript)
    (superscript . org-latex-superscript)
    (table . org-latex-table)
    (table-cell . org-latex-table-cell)
    (table-row . org-latex-table-row)
    (target . org-latex-target)
    (template . org-latex-template)
    (timestamp . org-latex-timestamp)
    (underline . org-latex-underline)
    (verbatim . org-latex-verbatim)
    (verse-block . org-latex-verse-block)
    ;; Pseudo objects and elements.
    (latex-math-block . org-latex-math-block)
    (latex-matrices . org-latex-matrices))
  :menu-entry
  '(?l "Export to LaTeX"
       ((?L "As LaTeX buffer" org-latex-export-as-latex)
	(?l "As LaTeX file" org-latex-export-to-latex)
	(?p "As PDF file" org-latex-export-to-pdf)
	(?o "As PDF file and open"
	    (lambda (a s v b)
	      (if a (org-latex-export-to-pdf t s v b)
		(org-open-file (org-latex-export-to-pdf nil s v b)))))))
  :filters-alist '((:filter-options . org-latex-math-block-options-filter)
		   (:filter-paragraph . org-latex-clean-invalid-line-breaks)
		   (:filter-parse-tree org-latex-math-block-tree-filter
				       org-latex-matrices-tree-filter
				       org-latex-image-link-filter)
		   (:filter-verse-block . org-latex-clean-invalid-line-breaks))
  :options-alist
  '((:latex-class "LATEX_CLASS" nil org-latex-default-class t)
    (:latex-class-options "LATEX_CLASS_OPTIONS" nil nil t)
    (:latex-header "LATEX_HEADER" nil nil newline)
    (:latex-header-extra "LATEX_HEADER_EXTRA" nil nil newline)
    (:description "DESCRIPTION" nil nil parse)
    (:keywords "KEYWORDS" nil nil parse)
    (:subtitle "SUBTITLE" nil nil parse)
    ;; Other variables.
    (:latex-active-timestamp-format nil nil org-latex-active-timestamp-format)
    (:latex-caption-above nil nil org-latex-caption-above)
    (:latex-classes nil nil org-latex-classes)
    (:latex-default-figure-position nil nil org-latex-default-figure-position)
    (:latex-default-table-environment nil nil org-latex-default-table-environment)
    (:latex-default-table-mode nil nil org-latex-default-table-mode)
    (:latex-diary-timestamp-format nil nil org-latex-diary-timestamp-format)
    (:latex-footnote-defined-format nil nil org-latex-footnote-defined-format)
    (:latex-footnote-separator nil nil org-latex-footnote-separator)
    (:latex-format-drawer-function nil nil org-latex-format-drawer-function)
    (:latex-format-headline-function nil nil org-latex-format-headline-function)
    (:latex-format-inlinetask-function nil nil org-latex-format-inlinetask-function)
    (:latex-hyperref-template nil nil org-latex-hyperref-template t)
    (:latex-image-default-height nil nil org-latex-image-default-height)
    (:latex-image-default-option nil nil org-latex-image-default-option)
    (:latex-image-default-width nil nil org-latex-image-default-width)
    (:latex-images-centered nil nil org-latex-images-centered)
    (:latex-inactive-timestamp-format nil nil org-latex-inactive-timestamp-format)
    (:latex-inline-image-rules nil nil org-latex-inline-image-rules)
    (:latex-link-with-unknown-path-format nil nil org-latex-link-with-unknown-path-format)
    (:latex-listings nil nil org-latex-listings)
    (:latex-listings-langs nil nil org-latex-listings-langs)
    (:latex-listings-options nil nil org-latex-listings-options)
    (:latex-minted-langs nil nil org-latex-minted-langs)
    (:latex-minted-options nil nil org-latex-minted-options)
    (:latex-prefer-user-labels nil nil org-latex-prefer-user-labels)
    (:latex-subtitle-format nil nil org-latex-subtitle-format)
    (:latex-subtitle-separate nil nil org-latex-subtitle-separate)
    (:latex-table-scientific-notation nil nil org-latex-table-scientific-notation)
    (:latex-tables-booktabs nil nil org-latex-tables-booktabs)
    (:latex-tables-centered nil nil org-latex-tables-centered)
    (:latex-text-markup-alist nil nil org-latex-text-markup-alist)
    (:latex-title-command nil nil org-latex-title-command)
    (:latex-toc-command nil nil org-latex-toc-command)
    (:latex-compiler "LATEX_COMPILER" nil org-latex-compiler)
    ;; Redefine regular options.
    (:date "DATE" nil "\\today" parse)))



;;; Internal Variables

(defconst org-latex-babel-language-alist
  '(("af" . "afrikaans")
    ("bg" . "bulgarian")
    ("bt-br" . "brazilian")
    ("ca" . "catalan")
    ("cs" . "czech")
    ("cy" . "welsh")
    ("da" . "danish")
    ("de" . "germanb")
    ("de-at" . "naustrian")
    ("de-de" . "ngerman")
    ("el" . "greek")
    ("en" . "english")
    ("en-au" . "australian")
    ("en-ca" . "canadian")
    ("en-gb" . "british")
    ("en-ie" . "irish")
    ("en-nz" . "newzealand")
    ("en-us" . "american")
    ("es" . "spanish")
    ("et" . "estonian")
    ("eu" . "basque")
    ("fi" . "finnish")
    ("fr" . "frenchb")
    ("fr-ca" . "canadien")
    ("gl" . "galician")
    ("hr" . "croatian")
    ("hu" . "hungarian")
    ("id" . "indonesian")
    ("is" . "icelandic")
    ("it" . "italian")
    ("la" . "latin")
    ("ms" . "malay")
    ("nl" . "dutch")
    ("nb" . "norsk")
    ("nn" . "nynorsk")
    ("no" . "norsk")
    ("pl" . "polish")
    ("pt" . "portuguese")
    ("ro" . "romanian")
    ("ru" . "russian")
    ("sa" . "sanskrit")
    ("sb" . "uppersorbian")
    ("sk" . "slovak")
    ("sl" . "slovene")
    ("sq" . "albanian")
    ("sr" . "serbian")
    ("sv" . "swedish")
    ("ta" . "tamil")
    ("tr" . "turkish")
    ("uk" . "ukrainian"))
  "Alist between language code and corresponding Babel option.")

(defconst org-latex-polyglossia-language-alist
  '(("am" "amharic")
    ("ast" "asturian")
    ("ar" "arabic")
    ("bo" "tibetan")
    ("bn" "bengali")
    ("bg" "bulgarian")
    ("br" "breton")
    ("bt-br" "brazilian")
    ("ca" "catalan")
    ("cop" "coptic")
    ("cs" "czech")
    ("cy" "welsh")
    ("da" "danish")
    ("de" "german" "german")
    ("de-at" "german" "austrian")
    ("de-de" "german" "german")
    ("dv" "divehi")
    ("el" "greek")
    ("en" "english" "usmax")
    ("en-au" "english" "australian")
    ("en-gb" "english" "uk")
    ("en-nz" "english" "newzealand")
    ("en-us" "english" "usmax")
    ("eo" "esperanto")
    ("es" "spanish")
    ("et" "estonian")
    ("eu" "basque")
    ("fa" "farsi")
    ("fi" "finnish")
    ("fr" "french")
    ("fu" "friulan")
    ("ga" "irish")
    ("gd" "scottish")
    ("gl" "galician")
    ("he" "hebrew")
    ("hi" "hindi")
    ("hr" "croatian")
    ("hu" "magyar")
    ("hy" "armenian")
    ("id" "bahasai")
    ("ia" "interlingua")
    ("is" "icelandic")
    ("it" "italian")
    ("kn" "kannada")
    ("la" "latin" "modern")
    ("la-modern" "latin" "modern")
    ("la-classic" "latin" "classic")
    ("la-medieval" "latin" "medieval")
    ("lo" "lao")
    ("lt" "lithuanian")
    ("lv" "latvian")
    ("mr" "maranthi")
    ("ml" "malayalam")
    ("nl" "dutch")
    ("nb" "norsk")
    ("nn" "nynorsk")
    ("nko" "nko")
    ("no" "norsk")
    ("oc" "occitan")
    ("pl" "polish")
    ("pms" "piedmontese")
    ("pt" "portuges")
    ("rm" "romansh")
    ("ro" "romanian")
    ("ru" "russian")
    ("sa" "sanskrit")
    ("hsb" "usorbian")
    ("dsb" "lsorbian")
    ("sk" "slovak")
    ("sl" "slovenian")
    ("se" "samin")
    ("sq" "albanian")
    ("sr" "serbian")
    ("sv" "swedish")
    ("syr" "syriac")
    ("ta" "tamil")
    ("te" "telugu")
    ("th" "thai")
    ("tk" "turkmen")
    ("tr" "turkish")
    ("uk" "ukrainian")
    ("ur" "urdu")
    ("vi" "vietnamese"))
  "Alist between language code and corresponding Polyglossia option")



(defconst org-latex-table-matrix-macros '(("bordermatrix" . "\\cr")
					  ("qbordermatrix" . "\\cr")
					  ("kbordermatrix" . "\\\\"))
  "Alist between matrix macros and their row ending.")

(defconst org-latex-math-environments-re
  (format
   "\\`[ \t]*\\\\begin{%s\\*?}"
   (regexp-opt
	   '("equation" "eqnarray" "math" "displaymath"
	     "align"  "gather" "multline" "flalign"  "alignat"
	     "xalignat" "xxalignat"
	     "subequations"
	     ;; breqn
	     "dmath" "dseries" "dgroup" "darray"
	     ;; empheq
	     "empheq")))
  "Regexp of LaTeX math environments.")


;;; User Configurable Variables

(defgroup org-export-latex nil
  "Options for exporting Org mode files to LaTeX."
  :tag "Org Export LaTeX"
  :group 'org-export)

;;;; Generic

(defcustom org-latex-caption-above '(table)
  "When non-nil, place caption string at the beginning of elements.
Otherwise, place it near the end.  When value is a list of
symbols, put caption above selected elements only.  Allowed
symbols are: `image', `table', `src-block' and `special-block'."
  :group 'org-export-latex
  :version "26.1"
  :package-version '(Org . "8.3")
  :type '(choice
	  (const :tag "For all elements" t)
	  (const :tag "For no element" nil)
	  (set :tag "For the following elements only" :greedy t
	       (const :tag "Images" image)
	       (const :tag "Tables" table)
	       (const :tag "Source code" src-block)
	       (const :tag "Special blocks" special-block))))

(defcustom org-latex-prefer-user-labels nil
   "Use user-provided labels instead of internal ones when non-nil.

When this variable is non-nil, Org will use the value of
CUSTOM_ID property, NAME keyword or Org target as the key for the
\\label commands generated.

By default, Org generates its own internal labels during LaTeX
export.  This process ensures that the \\label keys are unique
and valid, but it means the keys are not available in advance of
the export process.

Setting this variable gives you control over how Org generates
labels during LaTeX export, so that you may know their keys in
advance.  One reason to do this is that it allows you to refer to
various elements using a single label both in Org's link syntax
and in embedded LaTeX code.

For example, when this variable is non-nil, a headline like this:

  ** Some section
     :PROPERTIES:
     :CUSTOM_ID: sec:foo
     :END:
  This is section [[#sec:foo]].
  #+BEGIN_EXPORT latex
  And this is still section \\ref{sec:foo}.
  #+END_EXPORT

will be exported to LaTeX as:

  \\subsection{Some section}
  \\label{sec:foo}
  This is section \\ref{sec:foo}.
  And this is still section \\ref{sec:foo}.

Note, however, that setting this variable introduces a limitation
on the possible values for CUSTOM_ID and NAME.  When this
variable is non-nil, Org passes their value to \\label unchanged.
You are responsible for ensuring that the value is a valid LaTeX
\\label key, and that no other \\label commands with the same key
appear elsewhere in your document.  (Keys may contain letters,
numbers, and the following punctuation: '_' '.'  '-' ':'.)  There
are no such limitations on CUSTOM_ID and NAME when this variable
is nil.

For headlines that do not define the CUSTOM_ID property or
elements without a NAME, Org will continue to use its default
labeling scheme to generate labels and resolve links into proper
references."
  :group 'org-export-latex
  :type 'boolean
  :version "26.1"
  :package-version '(Org . "8.3"))

;;;; Preamble

(defcustom org-latex-default-class "article"
  "The default LaTeX class."
  :group 'org-export-latex
  :type '(string :tag "LaTeX class"))

(defcustom org-latex-classes
  '(("article"
     "\\documentclass[11pt]{article}"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
    ("report"
     "\\documentclass[11pt]{report}"
     ("\\part{%s}" . "\\part*{%s}")
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
    ("book"
     "\\documentclass[11pt]{book}"
     ("\\part{%s}" . "\\part*{%s}")
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  "Alist of LaTeX classes and associated header and structure.
If #+LATEX_CLASS is set in the buffer, use its value and the
associated information.  Here is the structure of each cell:

  (class-name
    header-string
    (numbered-section . unnumbered-section)
    ...)

The header string
-----------------

The HEADER-STRING is the header that will be inserted into the
LaTeX file.  It should contain the \\documentclass macro, and
anything else that is needed for this setup.  To this header, the
following commands will be added:

- Calls to \\usepackage for all packages mentioned in the
  variables `org-latex-default-packages-alist' and
  `org-latex-packages-alist'.  Thus, your header definitions
  should avoid to also request these packages.

- Lines specified via \"#+LATEX_HEADER:\" and
  \"#+LATEX_HEADER_EXTRA:\" keywords.

If you need more control about the sequence in which the header
is built up, or if you want to exclude one of these building
blocks for a particular class, you can use the following
macro-like placeholders.

 [DEFAULT-PACKAGES]      \\usepackage statements for default packages
 [NO-DEFAULT-PACKAGES]   do not include any of the default packages
 [PACKAGES]              \\usepackage statements for packages
 [NO-PACKAGES]           do not include the packages
 [EXTRA]                 the stuff from #+LATEX_HEADER(_EXTRA)
 [NO-EXTRA]              do not include #+LATEX_HEADER(_EXTRA) stuff

So a header like

  \\documentclass{article}
  [NO-DEFAULT-PACKAGES]
  [EXTRA]
  \\providecommand{\\alert}[1]{\\textbf{#1}}
  [PACKAGES]

will omit the default packages, and will include the
#+LATEX_HEADER and #+LATEX_HEADER_EXTRA lines, then have a call
to \\providecommand, and then place \\usepackage commands based
on the content of `org-latex-packages-alist'.

If your header, `org-latex-default-packages-alist' or
`org-latex-packages-alist' inserts \"\\usepackage[AUTO]{inputenc}\",
AUTO will automatically be replaced with a coding system derived
from `buffer-file-coding-system'.  See also the variable
`org-latex-inputenc-alist' for a way to influence this mechanism.

Likewise, if your header contains \"\\usepackage[AUTO]{babel}\"
or \"\\usepackage[AUTO]{polyglossia}\", AUTO will be replaced
with the language related to the language code specified by
`org-export-default-language'.  Note that constructions such as
\"\\usepackage[french,AUTO,english]{babel}\" are permitted.  For
Polyglossia the language will be set via the macros
\"\\setmainlanguage\" and \"\\setotherlanguage\".  See also
`org-latex-guess-babel-language' and
`org-latex-guess-polyglossia-language'.

The sectioning structure
------------------------

The sectioning structure of the class is given by the elements
following the header string.  For each sectioning level, a number
of strings is specified.  A %s formatter is mandatory in each
section string and will be replaced by the title of the section.

Instead of a cons cell (numbered . unnumbered), you can also
provide a list of 2 or 4 elements,

  (numbered-open numbered-close)

or

  (numbered-open numbered-close unnumbered-open unnumbered-close)

providing opening and closing strings for a LaTeX environment
that should represent the document section.  The opening clause
should have a %s to represent the section title.

Instead of a list of sectioning commands, you can also specify
a function name.  That function will be called with two
parameters, the (reduced) level of the headline, and a predicate
non-nil when the headline should be numbered.  It must return
a format string in which the section title will be added."
  :group 'org-export-latex
  :type '(repeat
	  (list (string :tag "LaTeX class")
		(string :tag "LaTeX header")
		(repeat :tag "Levels" :inline t
			(choice
			 (cons :tag "Heading"
			       (string :tag "  numbered")
			       (string :tag "unnumbered"))
			 (list :tag "Environment"
			       (string :tag "Opening   (numbered)")
			       (string :tag "Closing   (numbered)")
			       (string :tag "Opening (unnumbered)")
			       (string :tag "Closing (unnumbered)"))
			 (function :tag "Hook computing sectioning"))))))

(defcustom org-latex-inputenc-alist nil
  "Alist of inputenc coding system names, and what should really be used.
For example, adding an entry

      (\"utf8\" . \"utf8x\")

will cause \\usepackage[utf8x]{inputenc} to be used for buffers that
are written as utf8 files."
  :group 'org-export-latex
  :type '(repeat
	  (cons
	   (string :tag "Derived from buffer")
	   (string :tag "Use this instead"))))

(defcustom org-latex-title-command "\\maketitle"
  "The command used to insert the title just after \\begin{document}.

This format string may contain these elements:

  %a for AUTHOR keyword
  %t for TITLE keyword
  %s for SUBTITLE keyword
  %k for KEYWORDS line
  %d for DESCRIPTION line
  %c for CREATOR line
  %l for Language keyword
  %L for capitalized language keyword
  %D for DATE keyword

If you need to use a \"%\" character, you need to escape it
like that: \"%%\".

Setting :latex-title-command in publishing projects will take
precedence over this variable."
  :group 'org-export-latex
  :type '(string :tag "Format string"))

(defcustom org-latex-subtitle-format "\\\\\\medskip\n\\large %s"
  "Format string used for transcoded subtitle.
The format string should have at most one \"%s\"-expression,
which is replaced with the subtitle."
  :group 'org-export-latex
  :version "26.1"
  :package-version '(Org . "8.3")
  :type '(string :tag "Format string"))

(defcustom org-latex-subtitle-separate nil
  "Non-nil means the subtitle is not typeset as part of title."
  :group 'org-export-latex
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'boolean)

(defcustom org-latex-toc-command "\\tableofcontents\n\n"
  "LaTeX command to set the table of contents, list of figures, etc.
This command only applies to the table of contents generated with
the toc:nil option, not to those generated with #+TOC keyword."
  :group 'org-export-latex
  :type 'string)

(defcustom org-latex-hyperref-template
  "\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},
 pdfsubject={%d},\n pdfcreator={%c}, \n pdflang={%L}}\n"
  "Template for hyperref package options.

This format string may contain these elements:

  %a for AUTHOR keyword
  %t for TITLE keyword
  %s for SUBTITLE keyword
  %k for KEYWORDS line
  %d for DESCRIPTION line
  %c for CREATOR line
  %l for Language keyword
  %L for capitalized language keyword
  %D for DATE keyword

If you need to use a \"%\" character, you need to escape it
like that: \"%%\".

As a special case, a nil value prevents template from being
inserted.

Setting :latex-hyperref-template in publishing projects will take
precedence over this variable."
  :group 'org-export-latex
  :version "26.1"
  :package-version '(Org . "8.3")
  :type '(choice (const :tag "No template" nil)
		 (string :tag "Format string")))

;;;; Headline

(defcustom org-latex-format-headline-function
  'org-latex-format-headline-default-function
  "Function for formatting the headline's text.

This function will be called with six arguments:
TODO      the todo keyword (string or nil)
TODO-TYPE the type of todo (symbol: `todo', `done', nil)
PRIORITY  the priority of the headline (integer or nil)
TEXT      the main headline text (string)
TAGS      the tags (list of strings or nil)
INFO      the export options (plist)

The function result will be used in the section format string."
  :group 'org-export-latex
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'function)


;;;; Footnotes

(defcustom org-latex-footnote-separator "\\textsuperscript{,}\\,"
  "Text used to separate footnotes."
  :group 'org-export-latex
  :type 'string)

(defcustom org-latex-footnote-defined-format "\\textsuperscript{\\ref{%s}}"
  "Format string used to format reference to footnote already defined.
%s will be replaced by the label of the referred footnote."
  :group 'org-export-latex
  :type '(choice
	  (const :tag "Use plain superscript (default)" "\\textsuperscript{\\ref{%s}}")
	  (const :tag "Use Memoir/KOMA-Script footref" "\\footref{%s}")
	  (string :tag "Other format string"))
  :version "26.1"
  :package-version '(Org . "9.0"))

;;;; Timestamps

(defcustom org-latex-active-timestamp-format "\\textit{%s}"
  "A printf format string to be applied to active timestamps."
  :group 'org-export-latex
  :type 'string)

(defcustom org-latex-inactive-timestamp-format "\\textit{%s}"
  "A printf format string to be applied to inactive timestamps."
  :group 'org-export-latex
  :type 'string)

(defcustom org-latex-diary-timestamp-format "\\textit{%s}"
  "A printf format string to be applied to diary timestamps."
  :group 'org-export-latex
  :type 'string)


;;;; Links

(defcustom org-latex-images-centered t
  "When non-nil, images are centered."
  :group 'org-export-latex
  :version "26.1"
  :package-version '(Org . "9.0")
  :type 'boolean
  :safe #'booleanp)

(defcustom org-latex-image-default-option ""
  "Default option for images."
  :group 'org-export-latex
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-latex-image-default-width ".9\\linewidth"
  "Default width for images.
This value will not be used if a height is provided."
  :group 'org-export-latex
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-latex-image-default-height ""
  "Default height for images.
This value will not be used if a width is provided, or if the
image is wrapped within a \"figure\" or \"wrapfigure\"
environment."
  :group 'org-export-latex
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-latex-default-figure-position "htbp"
  "Default position for LaTeX figures."
  :group 'org-export-latex
  :type 'string
  :version "26.1"
  :package-version '(Org . "9.0")
  :safe #'stringp)

(defcustom org-latex-inline-image-rules
  `(("file" . ,(regexp-opt
		'("pdf" "jpeg" "jpg" "png" "ps" "eps" "tikz" "pgf" "svg"))))
  "Rules characterizing image files that can be inlined into LaTeX.

A rule consists in an association whose key is the type of link
to consider, and value is a regexp that will be matched against
link's path.

Note that, by default, the image extension *actually* allowed
depend on the way the LaTeX file is processed.  When used with
pdflatex, pdf, jpg and png images are OK.  When processing
through dvi to Postscript, only ps and eps are allowed.  The
default we use here encompasses both."
  :group 'org-export-latex
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(alist :key-type (string :tag "Type")
		:value-type (regexp :tag "Path")))

(defcustom org-latex-link-with-unknown-path-format "\\texttt{%s}"
  "Format string for links with unknown path type."
  :group 'org-export-latex
  :type 'string)


;;;; Tables

(defcustom org-latex-default-table-environment "tabular"
  "Default environment used to build tables."
  :group 'org-export-latex
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-latex-default-table-mode 'table
  "Default mode for tables.

Value can be a symbol among:

  `table' Regular LaTeX table.

  `math' In this mode, every cell is considered as being in math
     mode and the complete table will be wrapped within a math
     environment.  It is particularly useful to write matrices.

  `inline-math' This mode is almost the same as `math', but the
     math environment will be inlined.

  `verbatim' The table is exported as it appears in the Org
     buffer, within a verbatim environment.

This value can be overridden locally with, i.e. \":mode math\" in
LaTeX attributes.

When modifying this variable, it may be useful to change
`org-latex-default-table-environment' accordingly."
  :group 'org-export-latex
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice (const :tag "Table" table)
		 (const :tag "Matrix" math)
		 (const :tag "Inline matrix" inline-math)
		 (const :tag "Verbatim" verbatim))
  :safe (lambda (s) (memq s '(table math inline-math verbatim))))

(defcustom org-latex-tables-centered t
  "When non-nil, tables are exported in a center environment."
  :group 'org-export-latex
  :type 'boolean
  :safe #'booleanp)

(defcustom org-latex-tables-booktabs nil
  "When non-nil, display tables in a formal \"booktabs\" style.
This option assumes that the \"booktabs\" package is properly
loaded in the header of the document.  This value can be ignored
locally with \":booktabs t\" and \":booktabs nil\" LaTeX
attributes."
  :group 'org-export-latex
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean
  :safe #'booleanp)

(defcustom org-latex-table-scientific-notation "%s\\,(%s)"
  "Format string to display numbers in scientific notation.
The format should have \"%s\" twice, for mantissa and exponent
\(i.e., \"%s\\\\times10^{%s}\").

When nil, no transformation is made."
  :group 'org-export-latex
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (string :tag "Format string")
	  (const :tag "No formatting" nil)))

;;;; Text markup

(defcustom org-latex-text-markup-alist '((bold . "\\textbf{%s}")
					 (code . protectedtexttt)
					 (italic . "\\emph{%s}")
					 (strike-through . "\\sout{%s}")
					 (underline . "\\uline{%s}")
					 (verbatim . protectedtexttt))
  "Alist of LaTeX expressions to convert text markup.

The key must be a symbol among `bold', `code', `italic',
`strike-through', `underline' and `verbatim'.  The value is
a formatting string to wrap fontified text with.

Value can also be set to the following symbols: `verb' and
`protectedtexttt'.  For the former, Org will use \"\\verb\" to
create a format string and select a delimiter character that
isn't in the string.  For the latter, Org will use \"\\texttt\"
to typeset and try to protect special characters.

If no association can be found for a given markup, text will be
returned as-is."
  :group 'org-export-latex
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'alist
  :options '(bold code italic strike-through underline verbatim))


;;;; Drawers

(defcustom org-latex-format-drawer-function (lambda (_ contents) contents)
  "Function called to format a drawer in LaTeX code.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.

The function should return the string to be exported.

The default function simply returns the value of CONTENTS."
  :group 'org-export-latex
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'function)


;;;; Inlinetasks

(defcustom org-latex-format-inlinetask-function
  'org-latex-format-inlinetask-default-function
  "Function called to format an inlinetask in LaTeX code.

The function must accept seven parameters:
  TODO      the todo keyword (string or nil)
  TODO-TYPE the todo type (symbol: `todo', `done', nil)
  PRIORITY  the inlinetask priority (integer or nil)
  NAME      the inlinetask name (string)
  TAGS      the inlinetask tags (list of strings or nil)
  CONTENTS  the contents of the inlinetask (string or nil)
  INFO      the export options (plist)

The function should return the string to be exported."
  :group 'org-export-latex
  :type 'function
  :version "26.1"
  :package-version '(Org . "8.3"))


;; Src blocks

(defcustom org-latex-listings nil
  "Non-nil means export source code using the listings package.

This package will fontify source code, possibly even with color.
If you want to use this, you also need to make LaTeX use the
listings package, and if you want to have color, the color
package.  Just add these to `org-latex-packages-alist', for
example using customize, or with something like:

  (require \\='ox-latex)
  (add-to-list \\='org-latex-packages-alist \\='(\"\" \"listings\"))
  (add-to-list \\='org-latex-packages-alist \\='(\"\" \"color\"))

Alternatively,

  (setq org-latex-listings \\='minted)

causes source code to be exported using the minted package as
opposed to listings.  If you want to use minted, you need to add
the minted package to `org-latex-packages-alist', for example
using customize, or with

  (require \\='ox-latex)
  (add-to-list \\='org-latex-packages-alist \\='(\"newfloat\" \"minted\"))

In addition, it is necessary to install pygments
\(http://pygments.org), and to configure the variable
`org-latex-pdf-process' so that the -shell-escape option is
passed to pdflatex.

The minted choice has possible repercussions on the preview of
latex fragments (see `org-preview-latex-fragment').  If you run
into previewing problems, please consult

  http://orgmode.org/worg/org-tutorials/org-latex-preview.html"
  :group 'org-export-latex
  :type '(choice
	  (const :tag "Use listings" t)
	  (const :tag "Use minted" minted)
	  (const :tag "Export verbatim" nil))
  :safe (lambda (s) (memq s '(t nil minted))))

(defcustom org-latex-listings-langs
  '((emacs-lisp "Lisp") (lisp "Lisp") (clojure "Lisp")
    (c "C") (cc "C++")
    (fortran "fortran")
    (perl "Perl") (cperl "Perl") (python "Python") (ruby "Ruby")
    (html "HTML") (xml "XML")
    (tex "TeX") (latex "[LaTeX]TeX")
    (shell-script "bash")
    (gnuplot "Gnuplot")
    (ocaml "Caml") (caml "Caml")
    (sql "SQL") (sqlite "sql")
    (makefile "make")
    (R "r"))
  "Alist mapping languages to their listing language counterpart.
The key is a symbol, the major mode symbol without the \"-mode\".
The value is the string that should be inserted as the language
parameter for the listings package.  If the mode name and the
listings name are the same, the language does not need an entry
in this list - but it does not hurt if it is present."
  :group 'org-export-latex
  :version "26.1"
  :package-version '(Org . "8.3")
  :type '(repeat
	  (list
	   (symbol :tag "Major mode       ")
	   (string :tag "Listings language"))))

(defcustom org-latex-listings-options nil
  "Association list of options for the latex listings package.

These options are supplied as a comma-separated list to the
\\lstset command.  Each element of the association list should be
a list containing two strings: the name of the option, and the
value.  For example,

  (setq org-latex-listings-options
    \\='((\"basicstyle\" \"\\\\small\")
      (\"keywordstyle\" \"\\\\color{black}\\\\bfseries\\\\underbar\")))

will typeset the code in a small size font with underlined, bold
black keywords.

Note that the same options will be applied to blocks of all
languages.  If you need block-specific options, you may use the
following syntax:

  #+ATTR_LATEX: :options key1=value1,key2=value2
  #+BEGIN_SRC <LANG>
  ...
  #+END_SRC"
  :group 'org-export-latex
  :type '(repeat
	  (list
	   (string :tag "Listings option name ")
	   (string :tag "Listings option value"))))

(defcustom org-latex-minted-langs
  '((emacs-lisp "common-lisp")
    (cc "c++")
    (cperl "perl")
    (shell-script "bash")
    (caml "ocaml"))
  "Alist mapping languages to their minted language counterpart.
The key is a symbol, the major mode symbol without the \"-mode\".
The value is the string that should be inserted as the language
parameter for the minted package.  If the mode name and the
listings name are the same, the language does not need an entry
in this list - but it does not hurt if it is present.

Note that minted uses all lower case for language identifiers,
and that the full list of language identifiers can be obtained
with:

  pygmentize -L lexers"
  :group 'org-export-latex
  :type '(repeat
	  (list
	   (symbol :tag "Major mode     ")
	   (string :tag "Minted language"))))

(defcustom org-latex-minted-options nil
  "Association list of options for the latex minted package.

These options are supplied within square brackets in
\\begin{minted} environments.  Each element of the alist should
be a list containing two strings: the name of the option, and the
value.  For example,

  (setq org-latex-minted-options
    \\='((\"bgcolor\" \"bg\") (\"frame\" \"lines\")))

will result in src blocks being exported with

\\begin{minted}[bgcolor=bg,frame=lines]{<LANG>}

as the start of the minted environment. Note that the same
options will be applied to blocks of all languages.  If you need
block-specific options, you may use the following syntax:

  #+ATTR_LATEX: :options key1=value1,key2=value2
  #+BEGIN_SRC <LANG>
  ...
  #+END_SRC"
  :group 'org-export-latex
  :type '(repeat
	  (list
	   (string :tag "Minted option name ")
	   (string :tag "Minted option value"))))

(defcustom org-latex-custom-lang-environments nil
  "Alist mapping languages to language-specific LaTeX environments.

It is used during export of src blocks by the listings and minted
latex packages.  The environment may be a simple string, composed of
only letters and numbers.  In this case, the string is directly the
name of the latex environment to use.  The environment may also be
a format string.  In this case the format string will be directly
exported.  This format string may contain these elements:

  %s for the formatted source
  %c for the caption
  %f for the float attribute
  %l for an appropriate label
  %o for the LaTeX attributes

For example,

  (setq org-latex-custom-lang-environments
     \\='((python \"pythoncode\")
       (ocaml \"\\\\begin{listing}
\\\\begin{minted}[%o]{ocaml}
%s\\\\end{minted}
\\\\caption{%c}
\\\\label{%l}\")))

would have the effect that if Org encounters a Python source block
during LaTeX export it will produce

  \\begin{pythoncode}
  <src block body>
  \\end{pythoncode}

and if Org encounters an Ocaml source block during LaTeX export it
will produce

  \\begin{listing}
  \\begin{minted}[<attr_latex options>]{ocaml}
  <src block body>
  \\end{minted}
  \\caption{<caption>}
  \\label{<label>}
  \\end{listing}"
  :group 'org-export-latex
  :type '(repeat
	  (list
	   (symbol :tag "Language name                    ")
	   (string :tag "Environment name or format string")))
  :version "26.1"
  :package-version '(Org . "9.0"))


;;;; Compilation

(defcustom org-latex-compiler-file-string "%% Intended LaTeX compiler: %s\n"
  "LaTeX compiler format-string.
See also `org-latex-compiler'."
  :group 'org-export-latex
  :type '(choice
	  (const :tag "Comment" "%% Intended LaTeX compiler: %s\n")
	  (const :tag "latex-mode file variable" "%% -*- latex-run-command: %s -*-\n")
	  (const :tag "AUCTeX file variable" "%% -*- LaTeX-command: %s -*-\n")
	  (string :tag "custom format" "%% %s"))
  :version "26.1"
  :package-version '(Org . "9.0"))

(defcustom org-latex-compiler "pdflatex"
  "LaTeX compiler to use.

Must be an element in `org-latex-compilers' or the empty quote.
Can also be set in buffers via #+LATEX_COMPILER.  See also
`org-latex-compiler-file-string'."
  :group 'org-export-latex
  :type '(choice
	  (const :tag "pdfLaTeX" "pdflatex")
	  (const :tag "XeLaTeX"  "xelatex")
	  (const :tag "LuaLaTeX" "lualatex")
	  (const :tag "Unset" ""))
  :version "26.1"
  :package-version '(Org . "9.0"))

(defconst org-latex-compilers '("pdflatex" "xelatex" "lualatex")
  "Known LaTeX compilers.
See also `org-latex-compiler'.")

(defcustom org-latex-bib-compiler "bibtex"
  "Command to process a LaTeX file's bibliography.

The shorthand %bib in `org-latex-pdf-process' is replaced with
this value.

A better approach is to use a compiler suit such as `latexmk'."
  :group 'org-export-latex
  :type '(choice (const :tag "BibTeX" "bibtex")
		 (const :tag "Biber" "biber")
		 (string :tag "Other process"))
  :version "26.1"
  :package-version '(Org . "9.0"))

(defcustom org-latex-pdf-process
  '("%latex -interaction nonstopmode -output-directory %o %f"
    "%latex -interaction nonstopmode -output-directory %o %f"
    "%latex -interaction nonstopmode -output-directory %o %f")
  "Commands to process a LaTeX file to a PDF file.

This is a list of strings, each of them will be given to the
shell as a command.  %f in the command will be replaced by the
relative file name, %F by the absolute file name, %b by the file
base name (i.e. without directory and extension parts), %o by the
base directory of the file, %O by the absolute file name of the
output file, %latex is the LaTeX compiler (see
`org-latex-compiler'), and %bib is the BibTeX-like compiler (see
`org-latex-bib-compiler').

The reason why this is a list is that it usually takes several
runs of `pdflatex', maybe mixed with a call to `bibtex'.  Org
does not have a clever mechanism to detect which of these
commands have to be run to get to a stable result, and it also
does not do any error checking.

Consider a smart LaTeX compiler such as `texi2dvi' or `latexmk',
which calls the \"correct\" combinations of auxiliary programs.

Alternatively, this may be a Lisp function that does the
processing, so you could use this to apply the machinery of
AUCTeX or the Emacs LaTeX mode.  This function should accept the
file name as its single argument."
  :group 'org-export-pdf
  :type '(choice
	  (repeat :tag "Shell command sequence"
		  (string :tag "Shell command"))
	  (const :tag "2 runs of latex"
		 ("%latex -interaction nonstopmode -output-directory %o %f"
		  "%latex -interaction nonstopmode -output-directory %o %f"))
	  (const :tag "3 runs of latex"
		 ("%latex -interaction nonstopmode -output-directory %o %f"
		  "%latex -interaction nonstopmode -output-directory %o %f"
		  "%latex -interaction nonstopmode -output-directory %o %f"))
	  (const :tag "latex,bibtex,latex,latex"
		 ("%latex -interaction nonstopmode -output-directory %o %f"
		  "%bib %b"
		  "%latex -interaction nonstopmode -output-directory %o %f"
		  "%latex -interaction nonstopmode -output-directory %o %f"))
	  (const :tag "texi2dvi"
		 ("cd %o; LATEX=\"%latex\" texi2dvi -p -b -V %b.tex"))
	  (const :tag "latexmk"
		 ("latexmk -g -pdf -pdflatex=\"%latex\" -outdir=%o %f"))
	  (function)))

(defcustom org-latex-logfiles-extensions
  '("aux" "bcf" "blg" "fdb_latexmk" "fls" "figlist" "idx" "log" "nav" "out"
    "ptc" "run.xml" "snm" "toc" "vrb" "xdv")
  "The list of file extensions to consider as LaTeX logfiles.
The logfiles will be removed if `org-latex-remove-logfiles' is
non-nil."
  :group 'org-export-latex
  :version "26.1"
  :package-version '(Org . "8.3")
  :type '(repeat (string :tag "Extension")))

(defcustom org-latex-remove-logfiles t
  "Non-nil means remove the logfiles produced by PDF production.
By default, logfiles are files with these extensions: .aux, .idx,
.log, .out, .toc, .nav, .snm and .vrb.  To define the set of
logfiles to remove, set `org-latex-logfiles-extensions'."
  :group 'org-export-latex
  :type 'boolean)

(defcustom org-latex-known-warnings
  '(("Reference.*?undefined" . "[undefined reference]")
    ("Runaway argument" . "[runaway argument]")
    ("Underfull \\hbox" . "[underfull hbox]")
    ("Overfull \\hbox" . "[overfull hbox]")
    ("Citation.*?undefined" . "[undefined citation]")
    ("Undefined control sequence" . "[undefined control sequence]"))
  "Alist of regular expressions and associated messages for the user.
The regular expressions are used to find possible warnings in the
log of a latex-run.  These warnings will be reported after
calling `org-latex-compile'."
  :group 'org-export-latex
  :version "26.1"
  :package-version '(Org . "8.3")
  :type '(repeat
	  (cons
	   (string :tag "Regexp")
	   (string :tag "Message"))))



;;; Internal Functions

(defun org-latex--caption-above-p (element info)
  "Non nil when caption is expected to be located above ELEMENT.
INFO is a plist holding contextual information."
  (let ((above (plist-get info :latex-caption-above)))
    (if (symbolp above) above
      (let ((type (org-element-type element)))
	(memq (if (eq type 'link) 'image type) above)))))

(defun org-latex--label (datum info &optional force full)
  "Return an appropriate label for DATUM.
DATUM is an element or a `target' type object.  INFO is the
current export state, as a plist.

Return nil if element DATUM has no NAME or VALUE affiliated
keyword or no CUSTOM_ID property, unless FORCE is non-nil.  In
this case always return a unique label.

Eventually, if FULL is non-nil, wrap label within \"\\label{}\"."
  (let* ((type (org-element-type datum))
	 (user-label
	  (org-element-property
	   (cl-case type
	     ((headline inlinetask) :CUSTOM_ID)
	     (target :value)
	     (otherwise :name))
	   datum))
	 (label
	  (and (or user-label force)
	       (if (and user-label (plist-get info :latex-prefer-user-labels))
		   user-label
		 (concat (cl-case type
			   (headline "sec:")
			   (table "tab:")
			   (latex-environment
			    (and (string-match-p
				  org-latex-math-environments-re
				  (org-element-property :value datum))
				 "eq:"))
			   (paragraph
			    (and (org-element-property :caption datum)
				 "fig:")))
			 (org-export-get-reference datum info))))))
    (cond ((not full) label)
	  (label (format "\\label{%s}%s"
			 label
			 (if (eq type 'target) "" "\n")))
	  (t ""))))

(defun org-latex--caption/label-string (element info)
  "Return caption and label LaTeX string for ELEMENT.

INFO is a plist holding contextual information.  If there's no
caption nor label, return the empty string.

For non-floats, see `org-latex--wrap-label'."
  (let* ((label (org-latex--label element info nil t))
	 (main (org-export-get-caption element))
	 (attr (org-export-read-attribute :attr_latex element))
	 (type (org-element-type element))
	 (nonfloat (or (and (plist-member attr :float)
			    (not (plist-get attr :float))
			    main)
		       (and (eq type 'src-block)
			    (not (plist-get attr :float))
			    (null (plist-get info :latex-listings)))))
	 (short (org-export-get-caption element t))
	 (caption-from-attr-latex (plist-get attr :caption)))
    (cond
     ((org-string-nw-p caption-from-attr-latex)
      (concat caption-from-attr-latex "\n"))
     ((and (not main) (equal label "")) "")
     ((not main) label)
     ;; Option caption format with short name.
     (t
      (format (if nonfloat "\\captionof{%s}%s{%s%s}\n"
		"\\caption%s%s{%s%s}\n")
	      (let ((type* (if (eq type 'latex-environment)
			       (org-latex--environment-type element)
			     type)))
		(if nonfloat
		    (cl-case type*
		      (paragraph "figure")
		      (image "figure")
		      (special-block "figure")
		      (src-block (if (plist-get info :latex-listings)
				     "listing"
				   "figure"))
		      (t (symbol-name type*)))
		  ""))
	      (if short (format "[%s]" (org-export-data short info)) "")
	      label
	      (org-export-data main info))))))

(defun org-latex-guess-inputenc (header)
  "Set the coding system in inputenc to what the buffer is.

HEADER is the LaTeX header string.  This function only applies
when specified inputenc option is \"AUTO\".

Return the new header, as a string."
  (let* ((cs (or (ignore-errors
		   (latexenc-coding-system-to-inputenc
		    (or org-export-coding-system buffer-file-coding-system)))
		 "utf8")))
    (if (not cs) header
      ;; First translate if that is requested.
      (setq cs (or (cdr (assoc cs org-latex-inputenc-alist)) cs))
      ;; Then find the \usepackage statement and replace the option.
      (replace-regexp-in-string "\\\\usepackage\\[\\(AUTO\\)\\]{inputenc}"
				cs header t nil 1))))

(defun org-latex-guess-babel-language (header info)
  "Set Babel's language according to LANGUAGE keyword.

HEADER is the LaTeX header string.  INFO is the plist used as
a communication channel.

Insertion of guessed language only happens when Babel package has
explicitly been loaded.  Then it is added to the rest of
package's options.

The argument to Babel may be \"AUTO\" which is then replaced with
the language of the document or `org-export-default-language'
unless language in question is already loaded.

Return the new header."
  (let ((language-code (plist-get info :language)))
    ;; If no language is set or Babel package is not loaded, return
    ;; HEADER as-is.
    (if (or (not (stringp language-code))
	    (not (string-match "\\\\usepackage\\[\\(.*\\)\\]{babel}" header)))
	header
      (let ((options (save-match-data
		       (org-split-string (match-string 1 header) ",[ \t]*")))
	    (language (cdr (assoc-string language-code
					 org-latex-babel-language-alist t))))
	;; If LANGUAGE is already loaded, return header without AUTO.
	;; Otherwise, replace AUTO with language or append language if
	;; AUTO is not present.
	(replace-match
	 (mapconcat (lambda (option) (if (equal "AUTO" option) language option))
		    (cond ((member language options) (delete "AUTO" options))
			  ((member "AUTO" options) options)
			  (t (append options (list language))))
		    ", ")
	 t nil header 1)))))

(defun org-latex-guess-polyglossia-language (header info)
  "Set the Polyglossia language according to the LANGUAGE keyword.

HEADER is the LaTeX header string.  INFO is the plist used as
a communication channel.

Insertion of guessed language only happens when the Polyglossia
package has been explicitly loaded.

The argument to Polyglossia may be \"AUTO\" which is then
replaced with the language of the document or
`org-export-default-language'.  Note, the language is really set
using \setdefaultlanguage and not as an option to the package.

Return the new header."
  (let ((language (plist-get info :language)))
    ;; If no language is set or Polyglossia is not loaded, return
    ;; HEADER as-is.
    (if (or (not (stringp language))
	    (not (string-match
		  "\\\\usepackage\\(?:\\[\\([^]]+?\\)\\]\\){polyglossia}\n"
		  header)))
	header
      (let* ((options (org-string-nw-p (match-string 1 header)))
	     (languages (and options
			     ;; Reverse as the last loaded language is
			     ;; the main language.
			     (nreverse
			      (delete-dups
			       (save-match-data
				 (org-split-string
				  (replace-regexp-in-string
				   "AUTO" language options t)
				  ",[ \t]*"))))))
	     (main-language-set
	      (string-match-p "\\\\setmainlanguage{.*?}" header)))
	(replace-match
	 (concat "\\usepackage{polyglossia}\n"
		 (mapconcat
		  (lambda (l)
		    (let ((l (or (assoc l org-latex-polyglossia-language-alist)
				 l)))
		      (format (if main-language-set "\\setotherlanguage%s{%s}\n"
				(setq main-language-set t)
				"\\setmainlanguage%s{%s}\n")
			      (if (and (consp l) (= (length l) 3))
				  (format "[variant=%s]" (nth 2 l))
				"")
			      (nth 1 l))))
		  languages
		  ""))
	 t t header 0)))))

(defun org-latex--remove-packages (pkg-alist info)
  "Remove packages based on the current LaTeX compiler.

If the fourth argument of an element is set in pkg-alist, and it
is not a member of the LaTeX compiler of the document, the packages
is removed.  See also `org-latex-compiler'.

Return modified pkg-alist."
  (let ((compiler (or (plist-get info :latex-compiler) "")))
    (if (member-ignore-case compiler org-latex-compilers)
	(delq nil
	      (mapcar
	       (lambda (pkg)
		 (unless (and
			  (listp pkg)
			  (let ((third (nth 3 pkg)))
			    (and third
				 (not (member-ignore-case
				       compiler
				       (if (listp third) third (list third)))))))
		   pkg))
	       pkg-alist))
      pkg-alist)))

(defun org-latex--find-verb-separator (s)
  "Return a character not used in string S.
This is used to choose a separator for constructs like \\verb."
  (let ((ll "~,./?;':\"|!@#%^&-_=+abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ<>()[]{}"))
    (cl-loop for c across ll
	     when (not (string-match (regexp-quote (char-to-string c)) s))
	     return (char-to-string c))))

(defun org-latex--make-option-string (options)
  "Return a comma separated string of keywords and values.
OPTIONS is an alist where the key is the options keyword as
a string, and the value a list containing the keyword value, or
nil."
  (mapconcat (lambda (pair)
	       (pcase-let ((`(,keyword ,value) pair))
		 (concat keyword
			 (and (> (length value) 0)
			      (concat "=" value)))))
	     options
	     ","))

(defun org-latex--wrap-label (element output info)
  "Wrap label associated to ELEMENT around OUTPUT, if appropriate.
INFO is the current export state, as a plist.  This function
should not be used for floats.  See
`org-latex--caption/label-string'."
  (if (not (and (org-string-nw-p output) (org-element-property :name element)))
      output
    (concat (format "\\phantomsection\n\\label{%s}\n"
		    (org-latex--label element info))
	    output)))

(defun org-latex--protect-text (text)
  "Protect special characters in string TEXT and return it."
  (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" text))

(defun org-latex--text-markup (text markup info)
  "Format TEXT depending on MARKUP text markup.
INFO is a plist used as a communication channel.  See
`org-latex-text-markup-alist' for details."
  (let ((fmt (cdr (assq markup (plist-get info :latex-text-markup-alist)))))
    (cl-case fmt
      ;; No format string: Return raw text.
      ((nil) text)
      ;; Handle the `verb' special case: Find an appropriate separator
      ;; and use "\\verb" command.
      (verb
       (let ((separator (org-latex--find-verb-separator text)))
	 (concat "\\verb"
		 separator
		 (replace-regexp-in-string "\n" " " text)
		 separator)))
      ;; Handle the `protectedtexttt' special case: Protect some
      ;; special chars and use "\texttt{%s}" format string.
      (protectedtexttt
       (format "\\texttt{%s}"
	       (replace-regexp-in-string
		"--\\|[\\{}$%&_#~^]"
		(lambda (m)
		  (cond ((equal m "--") "-{}-")
			((equal m "\\") "\\textbackslash{}")
			((equal m "~") "\\textasciitilde{}")
			((equal m "^") "\\textasciicircum{}")
			(t (org-latex--protect-text m))))
		text nil t)))
      ;; Else use format string.
      (t (format fmt text)))))

(defun org-latex--delayed-footnotes-definitions (element info)
  "Return footnotes definitions in ELEMENT as a string.

INFO is a plist used as a communication channel.

Footnotes definitions are returned within \"\\footnotetext{}\"
commands.

This function is used within constructs that don't support
\"\\footnote{}\" command (e.g., an item tag).  In that case,
\"\\footnotemark\" is used within the construct and the function
just outside of it."
  (mapconcat
   (lambda (ref)
     (let ((def (org-export-get-footnote-definition ref info)))
       (format "\\footnotetext[%d]{%s%s}"
	       (org-export-get-footnote-number ref info)
	       (org-trim (org-latex--label def info t t))
	       (org-trim (org-export-data def info)))))
   ;; Find every footnote reference in ELEMENT.
   (letrec ((all-refs nil)
	    (search-refs
	     (lambda (data)
	       ;; Return a list of all footnote references never seen
	       ;; before in DATA.
	       (org-element-map data 'footnote-reference
		 (lambda (ref)
		   (when (org-export-footnote-first-reference-p ref info)
		     (push ref all-refs)
		     (when (eq (org-element-property :type ref) 'standard)
		       (funcall search-refs
				(org-export-get-footnote-definition ref info)))))
		 info)
	       (reverse all-refs))))
     (funcall search-refs element))
   ""))

(defun org-latex--translate (s info)
  "Translate string S according to specified language.
INFO is a plist used as a communication channel."
  (org-export-translate s :latex info))

(defun org-latex--format-spec (info)
  "Create a format-spec for document meta-data.
INFO is a plist used as a communication channel."
  (let ((language (let ((lang (plist-get info :language)))
		    (or (cdr (assoc-string lang org-latex-babel-language-alist t))
			(nth 1 (assoc-string lang org-latex-polyglossia-language-alist t))
			lang))))
    `((?a . ,(org-export-data (plist-get info :author) info))
      (?t . ,(org-export-data (plist-get info :title) info))
      (?k . ,(org-export-data (org-latex--wrap-latex-math-block
			       (plist-get info :keywords) info)
			      info))
      (?d . ,(org-export-data (org-latex--wrap-latex-math-block
			       (plist-get info :description) info)
			      info))
      (?c . ,(plist-get info :creator))
      (?l . ,language)
      (?L . ,(capitalize language))
      (?D . ,(org-export-get-date info)))))

(defun org-latex--insert-compiler (info)
  "Insert LaTeX_compiler info into the document.
INFO is a plist used as a communication channel."
  (let ((compiler (plist-get info :latex-compiler)))
       (and (org-string-nw-p org-latex-compiler-file-string)
	    (member (or compiler "") org-latex-compilers)
	    (format org-latex-compiler-file-string compiler))))


;;; Filters

(defun org-latex-matrices-tree-filter (tree _backend info)
  (org-latex--wrap-latex-matrices tree info))

(defun org-latex-math-block-tree-filter (tree _backend info)
  (org-latex--wrap-latex-math-block tree info))

(defun org-latex-math-block-options-filter (info _backend)
  (dolist (prop '(:author :date :title) info)
    (plist-put info prop
	       (org-latex--wrap-latex-math-block (plist-get info prop) info))))

(defun org-latex-clean-invalid-line-breaks (data _backend _info)
  (replace-regexp-in-string
   "\\(\\end{[A-Za-z0-9*]+}\\|^\\)[ \t]*\\\\\\\\[ \t]*$" "\\1"
   data))


;;; Template

;;;###autoload
(defun org-latex-make-preamble (info &optional template snippet?)
  "Return a formatted LaTeX preamble.
INFO is a plist used as a communication channel.  Optional
argument TEMPLATE, when non-nil, is the header template string,
as expected by `org-splice-latex-header'.  When SNIPPET? is
non-nil, only includes packages relevant to image generation, as
specified in `org-latex-default-packages-alist' or
`org-latex-packages-alist'."
  (let* ((class (plist-get info :latex-class))
	 (class-template
	  (or template
	      (let* ((class-options (plist-get info :latex-class-options))
		     (header (nth 1 (assoc class (plist-get info :latex-classes)))))
		(and (stringp header)
		     (if (not class-options) header
		       (replace-regexp-in-string
			"^[ \t]*\\\\documentclass\\(\\(\\[[^]]*\\]\\)?\\)"
			class-options header t nil 1))))
	      (user-error "Unknown LaTeX class `%s'" class))))
    (org-latex-guess-polyglossia-language
     (org-latex-guess-babel-language
      (org-latex-guess-inputenc
       (org-element-normalize-string
	(org-splice-latex-header
	 class-template
	 (org-latex--remove-packages org-latex-default-packages-alist info)
	 (org-latex--remove-packages org-latex-packages-alist info)
	 snippet?
	 (mapconcat #'org-element-normalize-string
		    (list (plist-get info :latex-header)
			  (and (not snippet?)
			       (plist-get info :latex-header-extra)))
		    ""))))
      info)
     info)))

(defun org-latex-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info))
	(spec (org-latex--format-spec info)))
    (concat
     ;; Time-stamp.
     (and (plist-get info :time-stamp-file)
	  (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     ;; LaTeX compiler.
     (org-latex--insert-compiler info)
     ;; Document class and packages.
     (org-latex-make-preamble info)
     ;; Possibly limit depth for headline numbering.
     (let ((sec-num (plist-get info :section-numbers)))
       (when (integerp sec-num)
	 (format "\\setcounter{secnumdepth}{%d}\n" sec-num)))
     ;; Author.
     (let ((author (and (plist-get info :with-author)
			(let ((auth (plist-get info :author)))
			  (and auth (org-export-data auth info)))))
	   (email (and (plist-get info :with-email)
		       (org-export-data (plist-get info :email) info))))
       (cond ((and author email (not (string= "" email)))
	      (format "\\author{%s\\thanks{%s}}\n" author email))
	     ((or author email) (format "\\author{%s}\n" (or author email)))))
     ;; Date.
     (let ((date (and (plist-get info :with-date) (org-export-get-date info))))
       (format "\\date{%s}\n" (org-export-data date info)))
     ;; Title and subtitle.
     (let* ((subtitle (plist-get info :subtitle))
	    (formatted-subtitle
	     (when subtitle
	       (format (plist-get info :latex-subtitle-format)
		       (org-export-data subtitle info))))
	    (separate (plist-get info :latex-subtitle-separate)))
       (concat
	(format "\\title{%s%s}\n" title
		(if separate "" (or formatted-subtitle "")))
	(when (and separate subtitle)
	  (concat formatted-subtitle "\n"))))
     ;; Hyperref options.
     (let ((template (plist-get info :latex-hyperref-template)))
       (and (stringp template)
            (format-spec template spec)))
     ;; Document start.
     "\\begin{document}\n\n"
     ;; Title command.
     (let* ((title-command (plist-get info :latex-title-command))
            (command (and (stringp title-command)
                          (format-spec title-command spec))))
       (org-element-normalize-string
	(cond ((not (plist-get info :with-title)) nil)
	      ((string= "" title) nil)
	      ((not (stringp command)) nil)
	      ((string-match "\\(?:[^%]\\|^\\)%s" command)
	       (format command title))
	      (t command))))
     ;; Table of contents.
     (let ((depth (plist-get info :with-toc)))
       (when depth
	 (concat (when (wholenump depth)
		   (format "\\setcounter{tocdepth}{%d}\n" depth))
		 (plist-get info :latex-toc-command))))
     ;; Document's body.
     contents
     ;; Creator.
     (and (plist-get info :with-creator)
	  (concat (plist-get info :creator) "\n"))
     ;; Document end.
     "\\end{document}")))



;;; Transcode Functions

;;;; Bold

(defun org-latex-bold (_bold contents info)
  "Transcode BOLD from Org to LaTeX.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (org-latex--text-markup contents 'bold info))


;;;; Center Block

(defun org-latex-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the center block.  INFO is a plist
holding contextual information."
  (org-latex--wrap-label
   center-block (format "\\begin{center}\n%s\\end{center}" contents) info))


;;;; Clock

(defun org-latex-clock (clock _contents info)
  "Transcode a CLOCK element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (concat
   "\\noindent"
   (format "\\textbf{%s} " org-clock-string)
   (format (plist-get info :latex-inactive-timestamp-format)
	   (concat (org-timestamp-translate (org-element-property :value clock))
		   (let ((time (org-element-property :duration clock)))
		     (and time (format " (%s)" time)))))
   "\\\\"))


;;;; Code

(defun org-latex-code (code _contents info)
  "Transcode a CODE object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-latex--text-markup (org-element-property :value code) 'code info))


;;;; Drawer

(defun org-latex-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((name (org-element-property :drawer-name drawer))
	 (output (funcall (plist-get info :latex-format-drawer-function)
			  name contents)))
    (org-latex--wrap-label drawer output info)))


;;;; Dynamic Block

(defun org-latex-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  (org-latex--wrap-label dynamic-block contents info))


;;;; Entity

(defun org-latex-entity (entity _contents _info)
  "Transcode an ENTITY object from Org to LaTeX.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (org-element-property :latex entity))


;;;; Example Block

(defun org-latex-example-block (example-block _contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (when (org-string-nw-p (org-element-property :value example-block))
    (let ((environment (or (org-export-read-attribute
			    :attr_latex example-block :environment)
			   "verbatim")))
      (org-latex--wrap-label
       example-block
       (format "\\begin{%s}\n%s\\end{%s}"
	       environment
	       (org-export-format-code-default example-block info)
	       environment)
       info))))


;;;; Export Block

(defun org-latex-export-block (export-block _contents _info)
  "Transcode a EXPORT-BLOCK element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (member (org-element-property :type export-block) '("LATEX" "TEX"))
    (org-remove-indentation (org-element-property :value export-block))))


;;;; Export Snippet

(defun org-latex-export-snippet (export-snippet _contents _info)
  "Transcode a EXPORT-SNIPPET object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'latex)
    (org-element-property :value export-snippet)))


;;;; Fixed Width

(defun org-latex-fixed-width (fixed-width _contents info)
  "Transcode a FIXED-WIDTH element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-latex--wrap-label
   fixed-width
   (format "\\begin{verbatim}\n%s\\end{verbatim}"
	   (org-remove-indentation
	    (org-element-property :value fixed-width)))
   info))


;;;; Footnote Reference

(defun org-latex-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((label (org-element-property :label footnote-reference)))
    (concat
     ;; Insert separator between two footnotes in a row.
     (let ((prev (org-export-get-previous-element footnote-reference info)))
       (when (eq (org-element-type prev) 'footnote-reference)
	 (plist-get info :latex-footnote-separator)))
     (cond
      ;; Use `:latex-footnote-defined-format' if the footnote has
      ;; already been defined.
      ((not (org-export-footnote-first-reference-p footnote-reference info))
       (format (plist-get info :latex-footnote-defined-format)
	       (org-latex--label
		(org-export-get-footnote-definition footnote-reference info)
		info t)))
      ;; Use \footnotemark if reference is within another footnote
      ;; reference, footnote definition or table cell.
      ((org-element-lineage footnote-reference
			    '(footnote-reference footnote-definition table-cell))
       "\\footnotemark")
      ;; Otherwise, define it with \footnote command.
      (t
       (let ((def (org-export-get-footnote-definition footnote-reference info)))
	 (concat
	  (format "\\footnote{%s%s}" (org-trim (org-export-data def info))
		  ;; Only insert a \label if there exist another
		  ;; reference to def.
		  (cond ((not label) "")
			((org-element-map (plist-get info :parse-tree) 'footnote-reference
			    (lambda (f)
			      (and (not (eq f footnote-reference))
				   (equal (org-element-property :label f) label)
				   (org-trim (org-latex--label def info t t))))
			    info t))
			(t "")))
	  ;; Retrieve all footnote references within the footnote and
	  ;; add their definition after it, since LaTeX doesn't support
	  ;; them inside.
	  (org-latex--delayed-footnotes-definitions def info))))))))


;;;; Headline

(defun org-latex-headline (headline contents info)
  "Transcode a HEADLINE element from Org to LaTeX.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((class (plist-get info :latex-class))
	   (level (org-export-get-relative-level headline info))
	   (numberedp (org-export-numbered-headline-p headline info))
	   (class-sectioning (assoc class (plist-get info :latex-classes)))
	   ;; Section formatting will set two placeholders: one for
	   ;; the title and the other for the contents.
	   (section-fmt
	    (let ((sec (if (functionp (nth 2 class-sectioning))
			   (funcall (nth 2 class-sectioning) level numberedp)
			 (nth (1+ level) class-sectioning))))
	      (cond
	       ;; No section available for that LEVEL.
	       ((not sec) nil)
	       ;; Section format directly returned by a function.  Add
	       ;; placeholder for contents.
	       ((stringp sec) (concat sec "\n%s"))
	       ;; (numbered-section . unnumbered-section)
	       ((not (consp (cdr sec)))
		(concat (funcall (if numberedp #'car #'cdr) sec) "\n%s"))
	       ;; (numbered-open numbered-close)
	       ((= (length sec) 2)
		(when numberedp (concat (car sec) "\n%s" (nth 1 sec))))
	       ;; (num-in num-out no-num-in no-num-out)
	       ((= (length sec) 4)
		(if numberedp (concat (car sec) "\n%s" (nth 1 sec))
		  (concat (nth 2 sec) "\n%s" (nth 3 sec)))))))
	   ;; Create a temporary export back-end that hard-codes
	   ;; "\underline" within "\section" and alike.
	   (section-back-end
	    (org-export-create-backend
	     :parent 'latex
	     :transcoders
	     '((underline . (lambda (o c i) (format "\\underline{%s}" c))))))
	   (text
	    (org-export-data-with-backend
	     (org-element-property :title headline) section-back-end info))
	   (todo
	    (and (plist-get info :with-todo-keywords)
		 (let ((todo (org-element-property :todo-keyword headline)))
		   (and todo (org-export-data todo info)))))
	   (todo-type (and todo (org-element-property :todo-type headline)))
	   (tags (and (plist-get info :with-tags)
		      (org-export-get-tags headline info)))
	   (priority (and (plist-get info :with-priority)
			  (org-element-property :priority headline)))
	   ;; Create the headline text along with a no-tag version.
	   ;; The latter is required to remove tags from toc.
	   (full-text (funcall (plist-get info :latex-format-headline-function)
			       todo todo-type priority text tags info))
	   ;; Associate \label to the headline for internal links.
	   (headline-label (org-latex--label headline info t t))
	   (pre-blanks
	    (make-string (org-element-property :pre-blank headline) ?\n)))
      (if (or (not section-fmt) (org-export-low-level-p headline info))
	  ;; This is a deep sub-tree: export it as a list item.  Also
	  ;; export as items headlines for which no section format has
	  ;; been found.
	  (let ((low-level-body
		 (concat
		  ;; If headline is the first sibling, start a list.
		  (when (org-export-first-sibling-p headline info)
		    (format "\\begin{%s}\n" (if numberedp 'enumerate 'itemize)))
		  ;; Itemize headline
		  "\\item"
		  (and full-text
		       (string-match-p "\\`[ \t]*\\[" full-text)
		       "\\relax")
		  " " full-text "\n"
		  headline-label
		  pre-blanks
		  contents)))
	    ;; If headline is not the last sibling simply return
	    ;; LOW-LEVEL-BODY.  Otherwise, also close the list, before
	    ;; any blank line.
	    (if (not (org-export-last-sibling-p headline info)) low-level-body
	      (replace-regexp-in-string
	       "[ \t\n]*\\'"
	       (format "\n\\\\end{%s}" (if numberedp 'enumerate 'itemize))
	       low-level-body)))
	;; This is a standard headline.  Export it as a section.  Add
	;; an alternative heading when possible, and when this is not
	;; identical to the usual heading.
	(let ((opt-title
	       (funcall (plist-get info :latex-format-headline-function)
			todo todo-type priority
			(org-export-data-with-backend
			 (org-export-get-alt-title headline info)
			 section-back-end info)
			(and (eq (plist-get info :with-tags) t) tags)
			info))
	      ;; Maybe end local TOC (see `org-latex-keyword').
	      (contents
	       (concat
		contents
		(let ((case-fold-search t)
		      (section
		       (let ((first (car (org-element-contents headline))))
			 (and (eq (org-element-type first) 'section) first))))
		  (org-element-map section 'keyword
		    (lambda (k)
		      (and (equal (org-element-property :key k) "TOC")
			   (let ((v (org-element-property :value k)))
			     (and (string-match-p "\\<headlines\\>" v)
				  (string-match-p "\\<local\\>" v)
				  (format "\\stopcontents[level-%d]" level)))))
		    info t)))))
	  (if (and opt-title
		   (not (equal opt-title full-text))
		   (string-match "\\`\\\\\\(.+?\\){" section-fmt))
	      (format (replace-match "\\1[%s]" nil nil section-fmt 1)
		      ;; Replace square brackets with parenthesis
		      ;; since square brackets are not supported in
		      ;; optional arguments.
		      (replace-regexp-in-string
		       "\\[" "(" (replace-regexp-in-string "\\]" ")" opt-title))
		      full-text
		      (concat headline-label pre-blanks contents))
	    ;; Impossible to add an alternative heading.  Fallback to
	    ;; regular sectioning format string.
	    (format section-fmt full-text
		    (concat headline-label pre-blanks contents))))))))

(defun org-latex-format-headline-default-function
    (todo _todo-type priority text tags _info)
  "Default format function for a headline.
See `org-latex-format-headline-function' for details."
  (concat
   (and todo (format "{\\bfseries\\sffamily %s} " todo))
   (and priority (format "\\framebox{\\#%c} " priority))
   text
   (and tags
	(format "\\hfill{}\\textsc{%s}"
		(mapconcat #'org-latex--protect-text tags ":")))))


;;;; Horizontal Rule

(defun org-latex-horizontal-rule (horizontal-rule _contents info)
  "Transcode an HORIZONTAL-RULE object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((attr (org-export-read-attribute :attr_latex horizontal-rule))
	(prev (org-export-get-previous-element horizontal-rule info)))
    (concat
     ;; Make sure the rule doesn't start at the end of the current
     ;; line by separating it with a blank line from previous element.
     (when (and prev
		(let ((prev-blank (org-element-property :post-blank prev)))
		  (or (not prev-blank) (zerop prev-blank))))
       "\n")
     (org-latex--wrap-label
      horizontal-rule
      (format "\\noindent\\rule{%s}{%s}"
	      (or (plist-get attr :width) "\\textwidth")
	      (or (plist-get attr :thickness) "0.5pt"))
      info))))


;;;; Inline Src Block

(defun org-latex-inline-src-block (inline-src-block _contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((code (org-element-property :value inline-src-block))
	 (separator (org-latex--find-verb-separator code)))
    (cl-case (plist-get info :latex-listings)
      ;; Do not use a special package: transcode it verbatim.
      ((nil) (format "\\texttt{%s}" (org-latex--text-markup code 'code info)))
      ;; Use minted package.
      (minted
       (let* ((org-lang (org-element-property :language inline-src-block))
	      (mint-lang (or (cadr (assq (intern org-lang)
					 (plist-get info :latex-minted-langs)))
			     (downcase org-lang)))
	      (options (org-latex--make-option-string
			(plist-get info :latex-minted-options))))
	 (format "\\mintinline%s{%s}{%s}"
		 (if (string= options "") "" (format "[%s]" options))
		 mint-lang
		 code)))
      ;; Use listings package.
      (otherwise
       ;; Maybe translate language's name.
       (let* ((org-lang (org-element-property :language inline-src-block))
	      (lst-lang (or (cadr (assq (intern org-lang)
					(plist-get info :latex-listings-langs)))
			    org-lang))
	      (options (org-latex--make-option-string
			(append (plist-get info :latex-listings-options)
				`(("language" ,lst-lang))))))
	 (concat (format "\\lstinline[%s]" options)
		 separator code separator))))))


;;;; Inlinetask

(defun org-latex-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((title (org-export-data (org-element-property :title inlinetask) info))
	(todo (and (plist-get info :with-todo-keywords)
		   (let ((todo (org-element-property :todo-keyword inlinetask)))
		     (and todo (org-export-data todo info)))))
	(todo-type (org-element-property :todo-type inlinetask))
	(tags (and (plist-get info :with-tags)
		   (org-export-get-tags inlinetask info)))
	(priority (and (plist-get info :with-priority)
		       (org-element-property :priority inlinetask)))
	(contents (concat (org-latex--label inlinetask info) contents)))
    (funcall (plist-get info :latex-format-inlinetask-function)
	     todo todo-type priority title tags contents info)))

(defun org-latex-format-inlinetask-default-function
    (todo _todo-type priority title tags contents _info)
  "Default format function for a inlinetasks.
See `org-latex-format-inlinetask-function' for details."
  (let ((full-title
	 (concat (when todo (format "\\textbf{\\textsf{\\textsc{%s}}} " todo))
		 (when priority (format "\\framebox{\\#%c} " priority))
		 title
		 (when tags
		   (format "\\hfill{}\\textsc{:%s:}"
			   (mapconcat #'org-latex--protect-text tags ":"))))))
    (concat "\\begin{center}\n"
	    "\\fbox{\n"
	    "\\begin{minipage}[c]{.6\\textwidth}\n"
	    full-title "\n\n"
	    (and (org-string-nw-p contents)
		 (concat "\\rule[.8em]{\\textwidth}{2pt}\n\n" contents))
	    "\\end{minipage}\n"
	    "}\n"
	    "\\end{center}")))


;;;; Italic

(defun org-latex-italic (_italic contents info)
  "Transcode ITALIC from Org to LaTeX.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (org-latex--text-markup contents 'italic info))


;;;; Item

(defun org-latex-item (item contents info)
  "Transcode an ITEM element from Org to LaTeX.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((counter
	  (let ((count (org-element-property :counter item))
		(level
		 ;; Determine level of current item to determine the
		 ;; correct LaTeX counter to use (enumi, enumii...).
		 (let ((parent item) (level 0))
		   (while (memq (org-element-type
				 (setq parent (org-export-get-parent parent)))
				'(plain-list item))
		     (when (and (eq (org-element-type parent) 'plain-list)
				(eq (org-element-property :type parent)
				    'ordered))
		       (cl-incf level)))
		   level)))
	    (and count
		 (< level 5)
		 (format "\\setcounter{enum%s}{%s}\n"
			 (nth (1- level) '("i" "ii" "iii" "iv"))
			 (1- count)))))
	 (checkbox (cl-case (org-element-property :checkbox item)
		     (on "$\\boxtimes$ ")
		     (off "$\\square$ ")
		     (trans "$\\boxminus$ ")))
	 (tag (let ((tag (org-element-property :tag item)))
		;; Check-boxes must belong to the tag.
		(and tag (format "[{%s}] "
				 (concat checkbox
					 (org-export-data tag info)))))))
    (concat counter
	    "\\item"
	    (cond
	     (tag)
	     (checkbox (concat " " checkbox))
	     ;; Without a tag or a check-box, if CONTENTS starts with
	     ;; an opening square bracket, add "\relax" to "\item",
	     ;; unless the brackets comes from an initial export
	     ;; snippet (i.e. it is inserted willingly by the user).
	     ((and contents
		   (string-match-p "\\`[ \t]*\\[" contents)
		   (not (let ((e (car (org-element-contents item))))
			  (and (eq (org-element-type e) 'paragraph)
			       (let ((o (car (org-element-contents e))))
				 (and (eq (org-element-type o) 'export-snippet)
				      (eq (org-export-snippet-backend o)
					  'latex)))))))
	      "\\relax ")
	     (t " "))
	    (and contents (org-trim contents))
	    ;; If there are footnotes references in tag, be sure to
	    ;; add their definition at the end of the item.  This
	    ;; workaround is necessary since "\footnote{}" command is
	    ;; not supported in tags.
	    (and tag
		 (org-latex--delayed-footnotes-definitions
		  (org-element-property :tag item) info)))))


;;;; Keyword

(defun org-latex-keyword (keyword _contents info)
  "Transcode a KEYWORD element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    (cond
     ((string= key "LATEX") value)
     ((string= key "INDEX") (format "\\index{%s}" value))
     ((string= key "TOC")
      (let ((case-fold-search t))
	(cond
	 ((string-match-p "\\<headlines\\>" value)
	  (let* ((localp (string-match-p "\\<local\\>" value))
		 (parent (org-element-lineage keyword '(headline)))
		 (level (if (not (and localp parent)) 0
			  (org-export-get-relative-level parent info)))
		 (depth
		  (and (string-match "\\<[0-9]+\\>" value)
		       (format
			"\\setcounter{tocdepth}{%d}"
			(+ (string-to-number (match-string 0 value)) level)))))
	    (if (and localp parent)
		;; Start local TOC, assuming package "titletoc" is
		;; required.
		(format "\\startcontents[level-%d]
\\printcontents[level-%d]{}{0}{%s}"
			level level (or depth ""))
	      (concat depth (and depth "\n") "\\tableofcontents"))))
	 ((string-match-p "\\<tables\\>" value) "\\listoftables")
	 ((string-match-p "\\<listings\\>" value)
	  (cl-case (plist-get info :latex-listings)
	    ((nil) "\\listoffigures")
	    (minted "\\listoflistings")
	    (otherwise "\\lstlistoflistings")))))))))


;;;; Latex Environment

(defun org-latex--environment-type (latex-environment)
  "Return the TYPE of LATEX-ENVIRONMENT.

The TYPE is determined from the actual latex environment, and
could be a member of `org-latex-caption-above' or `math'."
  (let* ((latex-begin-re "\\\\begin{\\([A-Za-z0-9*]+\\)}")
	 (value (org-remove-indentation
		 (org-element-property :value latex-environment)))
	 (env (or (and (string-match latex-begin-re value)
		       (match-string 1 value))
		  "")))
    (cond
     ((string-match-p org-latex-math-environments-re value) 'math)
     ((string-match-p
       (eval-when-compile
	 (regexp-opt '("table" "longtable" "tabular" "tabu" "longtabu")))
       env)
      'table)
     ((string-match-p "figure" env) 'image)
     ((string-match-p
       (eval-when-compile
	 (regexp-opt '("lstlisting" "listing" "verbatim" "minted")))
       env)
      'src-block)
     (t 'special-block))))

(defun org-latex-latex-environment (latex-environment _contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (plist-get info :with-latex)
    (let* ((value (org-remove-indentation
		   (org-element-property :value latex-environment)))
	   (type (org-latex--environment-type latex-environment))
	   (caption (if (eq type 'math)
			(org-latex--label latex-environment info nil t)
		      (org-latex--caption/label-string latex-environment info)))
	   (caption-above-p
	    (memq type (append (plist-get info :latex-caption-above) '(math)))))
      (if (not (or (org-element-property :name latex-environment)
		   (org-element-property :caption latex-environment)))
	  value
	;; Environment is labeled: label must be within the environment
	;; (otherwise, a reference pointing to that element will count
	;; the section instead).  Also insert caption if `latex-environment'
	;; is not a math environment.
	(with-temp-buffer
	  (insert value)
	  (if caption-above-p
	      (progn
		(goto-char (point-min))
		(forward-line))
	    (goto-char (point-max))
	    (forward-line -1))
	  (insert caption)
	  (buffer-string))))))

;;;; Latex Fragment

(defun org-latex-latex-fragment (latex-fragment _contents _info)
  "Transcode a LATEX-FRAGMENT object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-element-property :value latex-fragment)))
    ;; Trim math markers since the fragment is enclosed within
    ;; a latex-math-block object anyway.
    (cond ((string-match-p "\\`\\$[^$]" value) (substring value 1 -1))
	  ((string-prefix-p "\\(" value) (substring value 2 -2))
	  (t value))))


;;;; Line Break

(defun org-latex-line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "\\\\\n")


;;;; Link

(defun org-latex-image-link-filter (data _backend info)
  (org-export-insert-image-links data info org-latex-inline-image-rules))

(defun org-latex--inline-image (link info)
  "Return LaTeX code for an inline image.
LINK is the link pointing to the inline image.  INFO is a plist
used as a communication channel."
  (let* ((parent (org-export-get-parent-element link))
	 (path (let ((raw-path (org-element-property :path link)))
		 (if (not (file-name-absolute-p raw-path)) raw-path
		   (expand-file-name raw-path))))
	 (filetype (file-name-extension path))
	 (caption (org-latex--caption/label-string parent info))
	 (caption-above-p (org-latex--caption-above-p link info))
	 ;; Retrieve latex attributes from the element around.
	 (attr (org-export-read-attribute :attr_latex parent))
	 (float (let ((float (plist-get attr :float)))
		  (cond ((string= float "wrap") 'wrap)
			((string= float "sideways") 'sideways)
			((string= float "multicolumn") 'multicolumn)
			((and (plist-member attr :float) (not float)) 'nonfloat)
			((or float
			     (org-element-property :caption parent)
			     (org-string-nw-p (plist-get attr :caption)))
			 'figure)
			(t 'nonfloat))))
	 (placement
	  (let ((place (plist-get attr :placement)))
	    (cond
	     (place (format "%s" place))
	     ((eq float 'wrap) "{l}{0.5\\textwidth}")
	     ((eq float 'figure)
	      (format "[%s]" (plist-get info :latex-default-figure-position)))
	     (t ""))))
	 (center
	  (if (plist-member attr :center) (plist-get attr :center)
	    (plist-get info :latex-images-centered)))
	 (comment-include (if (plist-get attr :comment-include) "%" ""))
	 ;; It is possible to specify width and height in the
	 ;; ATTR_LATEX line, and also via default variables.
	 (width (cond ((plist-get attr :width))
		      ((plist-get attr :height) "")
		      ((eq float 'wrap) "0.48\\textwidth")
		      (t (plist-get info :latex-image-default-width))))
	 (height (cond ((plist-get attr :height))
		       ((or (plist-get attr :width)
			    (memq float '(figure wrap))) "")
		       (t (plist-get info :latex-image-default-height))))
	 (options (let ((opt (or (plist-get attr :options)
				 (plist-get info :latex-image-default-option))))
		    (if (not (string-match "\\`\\[\\(.*\\)\\]\\'" opt)) opt
		      (match-string 1 opt))))
	 image-code)
    (if (member filetype '("tikz" "pgf"))
	;; For tikz images:
	;; - use \input to read in image file.
	;; - if options are present, wrap in a tikzpicture environment.
	;; - if width or height are present, use \resizebox to change
	;;   the image size.
	(progn
	  (setq image-code (format "\\input{%s}" path))
	  (when (org-string-nw-p options)
	    (setq image-code
		  (format "\\begin{tikzpicture}[%s]\n%s\n\\end{tikzpicture}"
			  options
			  image-code)))
	  (when (or (org-string-nw-p width) (org-string-nw-p height))
	    (setq image-code (format "\\resizebox{%s}{%s}{%s}"
				     (if (org-string-nw-p width) width "!")
				     (if (org-string-nw-p height) height "!")
				     image-code))))
      ;; For other images:
      ;; - add width and height to options.
      ;; - include the image with \includegraphics.
      (when (org-string-nw-p width)
	(setq options (concat options ",width=" width)))
      (when (org-string-nw-p height)
	(setq options (concat options ",height=" height)))
      (let ((search-option (org-element-property :search-option link)))
        (when (and search-option
                   (equal filetype "pdf")
                   (string-match-p "\\`[0-9]+\\'" search-option)
                   (not (string-match-p "page=" options)))
          (setq options (concat options ",page=" search-option))))
      (setq image-code
	    (format "\\includegraphics%s{%s}"
		    (cond ((not (org-string-nw-p options)) "")
			  ((string-prefix-p "," options)
			   (format "[%s]" (substring options 1)))
			  (t (format "[%s]" options)))
		    path))
      (when (equal filetype "svg")
	(setq image-code (replace-regexp-in-string "^\\\\includegraphics"
						   "\\includesvg"
						   image-code
						   nil t))
	(setq image-code (replace-regexp-in-string "\\.svg}"
						   "}"
						   image-code
						   nil t))))
    ;; Return proper string, depending on FLOAT.
    (pcase float
      (`wrap (format "\\begin{wrapfigure}%s
%s%s
%s%s
%s\\end{wrapfigure}"
		     placement
		     (if caption-above-p caption "")
		     (if center "\\centering" "")
		     comment-include image-code
		     (if caption-above-p "" caption)))
      (`sideways (format "\\begin{sidewaysfigure}
%s%s
%s%s
%s\\end{sidewaysfigure}"
			 (if caption-above-p caption "")
			 (if center "\\centering" "")
			 comment-include image-code
			 (if caption-above-p "" caption)))
      (`multicolumn (format "\\begin{figure*}%s
%s%s
%s%s
%s\\end{figure*}"
			    placement
			    (if caption-above-p caption "")
			    (if center "\\centering" "")
			    comment-include image-code
			    (if caption-above-p "" caption)))
      (`figure (format "\\begin{figure}%s
%s%s
%s%s
%s\\end{figure}"
		       placement
		       (if caption-above-p caption "")
		       (if center "\\centering" "")
		       comment-include image-code
		       (if caption-above-p "" caption)))
      ((guard center)
       (format "\\begin{center}
%s%s
%s\\end{center}"
	       (if caption-above-p caption "")
	       image-code
	       (if caption-above-p "" caption)))
      (_
       (concat (if caption-above-p caption "")
	       image-code
	       (if caption-above-p caption ""))))))

(defun org-latex-link (link desc info)
  "Transcode a LINK object from Org to LaTeX.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 ;; Ensure DESC really exists, or set it to nil.
	 (desc (and (not (string= desc "")) desc))
	 (imagep (org-export-inline-image-p
		  link (plist-get info :latex-inline-image-rules)))
	 (path (org-latex--protect-text
		(cond ((member type '("http" "https" "ftp" "mailto" "doi"))
		       (concat type ":" raw-path))
		      ((string= type "file") (org-export-file-uri raw-path))
		      (t raw-path)))))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'latex))
     ;; Image file.
     (imagep (org-latex--inline-image link info))
     ;; Radio link: Transcode target's contents and use them as link's
     ;; description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
	(if (not destination) desc
	  (format "\\hyperref[%s]{%s}"
		  (org-export-get-reference destination info)
		  desc))))
     ;; Links pointing to a headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(cl-case (org-element-type destination)
	  ;; Id link points to an external file.
	  (plain-text
	   (if desc (format "\\href{%s}{%s}" destination desc)
	     (format "\\url{%s}" destination)))
	  ;; Fuzzy link points nowhere.
	  ((nil)
	   (format (plist-get info :latex-link-with-unknown-path-format)
		   (or desc
		       (org-export-data
			(org-element-property :raw-link link) info))))
	  ;; LINK points to a headline.  If headlines are numbered
	  ;; and the link has no description, display headline's
	  ;; number.  Otherwise, display description or headline's
	  ;; title.
	  (headline
	   (let ((label (org-latex--label destination info t)))
	     (if (and (not desc)
		      (org-export-numbered-headline-p destination info))
		 (format "\\ref{%s}" label)
	       (format "\\hyperref[%s]{%s}" label
		       (or desc
			   (org-export-data
			    (org-element-property :title destination) info))))))
          ;; Fuzzy link points to a target.  Do as above.
	  (otherwise
	   (let ((ref (org-latex--label destination info t)))
	     (if (not desc) (format "\\ref{%s}" ref)
	       (format "\\hyperref[%s]{%s}" ref desc)))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (format (org-export-get-coderef-format path desc)
	      (org-export-resolve-coderef path info)))
     ;; External link with a description part.
     ((and path desc) (format "\\href{%s}{%s}" path desc))
     ;; External link without a description part.
     (path (format "\\url{%s}" path))
     ;; No path, only description.  Try to do something useful.
     (t (format (plist-get info :latex-link-with-unknown-path-format) desc)))))


;;;; Node Property

(defun org-latex-node-property (node-property _contents _info)
  "Transcode a NODE-PROPERTY element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "%s:%s"
          (org-element-property :key node-property)
          (let ((value (org-element-property :value node-property)))
            (if value (concat " " value) ""))))


;;;; Paragraph

(defun org-latex-paragraph (_paragraph contents _info)
  "Transcode a PARAGRAPH element from Org to LaTeX.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  contents)


;;;; Plain List

(defun org-latex-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to LaTeX.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
	 (attr (org-export-read-attribute :attr_latex plain-list))
	 (latex-type (let ((env (plist-get attr :environment)))
		       (cond (env (format "%s" env))
			     ((eq type 'ordered) "enumerate")
			     ((eq type 'descriptive) "description")
			     (t "itemize")))))
    (org-latex--wrap-label
     plain-list
     (format "\\begin{%s}%s\n%s\\end{%s}"
	     latex-type
	     (or (plist-get attr :options) "")
	     contents
	     latex-type)
     info)))


;;;; Plain Text

(defun org-latex-plain-text (text info)
  "Transcode a TEXT string from Org to LaTeX.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (let* ((specialp (plist-get info :with-special-strings))
	 (output
	  ;; Turn LaTeX into \LaTeX{} and TeX into \TeX{}.
	  (let ((case-fold-search nil))
	    (replace-regexp-in-string
	     "\\<\\(?:La\\)?TeX\\>" "\\\\\\&{}"
	     ;; Protect ^, ~, %, #, &, $, _, { and }.  Also protect \.
	     ;; However, if special strings are used, be careful not
	     ;; to protect "\" in "\-" constructs.
	     (replace-regexp-in-string
	      (concat "[%$#&{}_~^]\\|\\\\" (and specialp "\\([^-]\\|$\\)"))
	      (lambda (m)
		(cl-case (string-to-char m)
		  (?\\ "$\\\\backslash$\\1")
		  (?~ "\\\\textasciitilde{}")
		  (?^ "\\\\^{}")
		  (t "\\\\\\&")))
	      text)))))
    ;; Activate smart quotes.  Be sure to provide original TEXT string
    ;; since OUTPUT may have been modified.
    (when (plist-get info :with-smart-quotes)
      (setq output (org-export-activate-smart-quotes output :latex info text)))
    ;; Convert special strings.
    (when specialp
      (setq output (replace-regexp-in-string "\\.\\.\\." "\\\\ldots{}" output)))
    ;; Handle break preservation if required.
    (when (plist-get info :preserve-breaks)
      (setq output (replace-regexp-in-string
		    "\\(?:[ \t]*\\\\\\\\\\)?[ \t]*\n" "\\\\\n" output nil t)))
    ;; Return value.
    output))


;;;; Planning

(defun org-latex-planning (planning _contents info)
  "Transcode a PLANNING element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (concat
   "\\noindent"
   (mapconcat
    'identity
    (delq nil
	  (list
	   (let ((closed (org-element-property :closed planning)))
	     (when closed
	       (concat
		(format "\\textbf{%s} " org-closed-string)
		(format (plist-get info :latex-inactive-timestamp-format)
			(org-timestamp-translate closed)))))
	   (let ((deadline (org-element-property :deadline planning)))
	     (when deadline
	       (concat
		(format "\\textbf{%s} " org-deadline-string)
		(format (plist-get info :latex-active-timestamp-format)
			(org-timestamp-translate deadline)))))
	   (let ((scheduled (org-element-property :scheduled planning)))
	     (when scheduled
	       (concat
		(format "\\textbf{%s} " org-scheduled-string)
		(format (plist-get info :latex-active-timestamp-format)
			(org-timestamp-translate scheduled)))))))
    " ")
   "\\\\"))


;;;; Property Drawer

(defun org-latex-property-drawer (_property-drawer contents _info)
  "Transcode a PROPERTY-DRAWER element from Org to LaTeX.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (and (org-string-nw-p contents)
       (format "\\begin{verbatim}\n%s\\end{verbatim}" contents)))


;;;; Pseudo Element: LaTeX Matrices

;; `latex-matrices' elements have the following properties:
;; `:caption', `:post-blank' and `:markup' (`inline', `equation' or
;; `math').

(defun org-latex--wrap-latex-matrices (data info)
  "Merge contiguous tables with the same mode within a pseudo-element.
DATA is a parse tree or a secondary string.  INFO is a plist
containing export options.  Modify DATA by side-effect and return
it."
  (org-element-map data 'table
    (lambda (table)
      (when (eq (org-element-property :type table) 'org)
	(let ((mode (or (org-export-read-attribute :attr_latex table :mode)
			(plist-get info :latex-default-table-mode))))
	  (when (and (member mode '("inline-math" "math"))
		     ;; Do not wrap twice the same table.
		     (not (eq (org-element-type
			       (org-element-property :parent table))
			      'latex-matrices)))
	    (let* ((caption (and (not (string= mode "inline-math"))
				 (org-element-property :caption table)))
		   (matrices
		    (list 'latex-matrices
			  (list :caption caption
				:markup
				(cond ((string= mode "inline-math") 'inline)
				      (caption 'equation)
				      (t 'math)))))
		   (previous table)
		   (next (org-export-get-next-element table info)))
	      (org-element-insert-before matrices table)
	      ;; Swallow all contiguous tables sharing the same mode.
	      (while (and
		      (zerop (or (org-element-property :post-blank previous) 0))
		      (setq next (org-export-get-next-element previous info))
		      (eq (org-element-type next) 'table)
		      (eq (org-element-property :type next) 'org)
		      (string= (or (org-export-read-attribute
				    :attr_latex next :mode)
				   (plist-get info :latex-default-table-mode))
			       mode))
		(org-element-extract-element previous)
		(org-element-adopt-elements matrices previous)
		(setq previous next))
	      ;; Inherit `:post-blank' from the value of the last
	      ;; swallowed table.  Set the latter's `:post-blank'
	      ;; value to 0 so as to not duplicate empty lines.
	      (org-element-put-property
	       matrices :post-blank (org-element-property :post-blank previous))
	      (org-element-put-property previous :post-blank 0)
	      (org-element-extract-element previous)
	      (org-element-adopt-elements matrices previous))))))
    info)
  data)

(defun org-latex-matrices (matrices contents _info)
  "Transcode a MATRICES element from Org to LaTeX.
CONTENTS is a string.  INFO is a plist used as a communication
channel."
  (format (cl-case (org-element-property :markup matrices)
	    (inline "\\(%s\\)")
	    (equation "\\begin{equation}\n%s\\end{equation}")
	    (t "\\[\n%s\\]"))
	  contents))


;;;; Pseudo Object: LaTeX Math Block

;; `latex-math-block' objects have the following property:
;; `:post-blank'.

(defun org-latex--wrap-latex-math-block (data info)
  "Merge contiguous math objects in a pseudo-object container.
DATA is a parse tree or a secondary string.  INFO is a plist
containing export options.  Modify DATA by side-effect and return it."
  (let ((valid-object-p
	 ;; Non-nil when OBJ can be added to the latex math block B.
	 (lambda (obj b)
	   (pcase (org-element-type obj)
	     (`entity (org-element-property :latex-math-p obj))
	     (`latex-fragment
	      (let ((value (org-element-property :value obj)))
		(or (string-prefix-p "\\(" value)
		    (string-match-p "\\`\\$[^$]" value))))
	     ((and type (or `subscript `superscript))
	      (not (memq type (mapcar #'org-element-type
				      (org-element-contents b)))))))))
    (org-element-map data '(entity latex-fragment subscript superscript)
      (lambda (object)
	;; Skip objects already wrapped.
	(when (and (not (eq (org-element-type
			     (org-element-property :parent object))
			    'latex-math-block))
		   (funcall valid-object-p object nil))
	  (let ((math-block (list 'latex-math-block nil))
		(next-elements (org-export-get-next-element object info t))
		(last object))
	    ;; Wrap MATH-BLOCK around OBJECT in DATA.
	    (org-element-insert-before math-block object)
	    (org-element-extract-element object)
	    (org-element-adopt-elements math-block object)
	    (when (zerop (or (org-element-property :post-blank object) 0))
	      ;; MATH-BLOCK swallows consecutive math objects.
	      (catch 'exit
		(dolist (next next-elements)
		  (unless (funcall valid-object-p next math-block)
		    (throw 'exit nil))
		  (org-element-extract-element next)
		  (org-element-adopt-elements math-block next)
		  ;; Eschew the case: \beta$x$ -> \(\betax\).
		  (unless (memq (org-element-type next)
				'(subscript superscript))
		    (org-element-put-property last :post-blank 1))
		  (setq last next)
		  (when (> (or (org-element-property :post-blank next) 0) 0)
		    (throw 'exit nil)))))
	    (org-element-put-property
	     math-block :post-blank (org-element-property :post-blank last)))))
      info nil '(subscript superscript latex-math-block) t)
    ;; Return updated DATA.
    data))

(defun org-latex-math-block (_math-block contents _info)
  "Transcode a MATH-BLOCK object from Org to LaTeX.
CONTENTS is a string.  INFO is a plist used as a communication
channel."
  (when (org-string-nw-p contents)
    (format "\\(%s\\)" (org-trim contents))))

;;;; Quote Block

(defun org-latex-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-latex--wrap-label
   quote-block (format "\\begin{quote}\n%s\\end{quote}" contents) info))


;;;; Radio Target

(defun org-latex-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to LaTeX.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (format "\\label{%s}%s" (org-export-get-reference radio-target info) text))


;;;; Section

(defun org-latex-section (_section contents _info)
  "Transcode a SECTION element from Org to LaTeX.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)


;;;; Special Block

(defun org-latex-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((type (org-element-property :type special-block))
	(opt (org-export-read-attribute :attr_latex special-block :options))
	(caption (org-latex--caption/label-string special-block info))
	(caption-above-p (org-latex--caption-above-p special-block info)))
    (concat (format "\\begin{%s}%s\n" type (or opt ""))
	    (and caption-above-p caption)
	    contents
	    (and (not caption-above-p) caption)
	    (format "\\end{%s}" type))))


;;;; Src Block

(defun org-latex-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (when (org-string-nw-p (org-element-property :value src-block))
    (let* ((lang (org-element-property :language src-block))
	   (caption (org-element-property :caption src-block))
	   (caption-above-p (org-latex--caption-above-p src-block info))
	   (label (org-element-property :name src-block))
	   (custom-env (and lang
			    (cadr (assq (intern lang)
					org-latex-custom-lang-environments))))
	   (num-start (org-export-get-loc src-block info))
	   (retain-labels (org-element-property :retain-labels src-block))
	   (attributes (org-export-read-attribute :attr_latex src-block))
	   (float (plist-get attributes :float))
	   (listings (plist-get info :latex-listings)))
      (cond
       ;; Case 1.  No source fontification.
       ((not listings)
	(let* ((caption-str (org-latex--caption/label-string src-block info))
	       (float-env
		(cond ((string= "multicolumn" float)
		       (format "\\begin{figure*}[%s]\n%s%%s\n%s\\end{figure*}"
			       (plist-get info :latex-default-figure-position)
			       (if caption-above-p caption-str "")
			       (if caption-above-p "" caption-str)))
		      (caption (concat
				(if caption-above-p caption-str "")
				"%s"
				(if caption-above-p "" (concat "\n" caption-str))))
		      (t "%s"))))
	  (format
	   float-env
	   (concat (format "\\begin{verbatim}\n%s\\end{verbatim}"
			   (org-export-format-code-default src-block info))))))
       ;; Case 2.  Custom environment.
       (custom-env
	(let ((caption-str (org-latex--caption/label-string src-block info))
              (formatted-src (org-export-format-code-default src-block info)))
          (if (string-match-p "\\`[a-zA-Z0-9]+\\'" custom-env)
	      (format "\\begin{%s}\n%s\\end{%s}\n"
		      custom-env
		      (concat (and caption-above-p caption-str)
			      formatted-src
			      (and (not caption-above-p) caption-str))
		      custom-env)
	    (format-spec custom-env
			 `((?s . ,formatted-src)
			   (?c . ,caption)
			   (?f . ,float)
			   (?l . ,(org-latex--label src-block info))
			   (?o . ,(or (plist-get attributes :options) "")))))))
       ;; Case 3.  Use minted package.
       ((eq listings 'minted)
	(let* ((caption-str (org-latex--caption/label-string src-block info))
	       (float-env
		(cond
		 ((string= "multicolumn" float)
		  (format "\\begin{listing*}[%s]\n%s%%s\n%s\\end{listing*}"
			  (plist-get info :latex-default-figure-position)
			  (if caption-above-p caption-str "")
			  (if caption-above-p "" caption-str)))
		 (caption
		  (format "\\begin{listing}[%s]\n%s%%s\n%s\\end{listing}"
			  (plist-get info :latex-default-figure-position)
			  (if caption-above-p caption-str "")
			  (if caption-above-p "" caption-str)))
		 ((string= "t" float)
		  (concat (format "\\begin{listing}[%s]\n"
				  (plist-get info :latex-default-figure-position))
			  "%s\n\\end{listing}"))
		 (t "%s")))
	       (options (plist-get info :latex-minted-options))
	       (body
		(format
		 "\\begin{minted}[%s]{%s}\n%s\\end{minted}"
		 ;; Options.
		 (concat
		  (org-latex--make-option-string
		   (if (or (not num-start) (assoc "linenos" options))
		       options
		     (append
		      `(("linenos")
			("firstnumber" ,(number-to-string (1+ num-start))))
		      options)))
		  (let ((local-options (plist-get attributes :options)))
		    (and local-options (concat "," local-options))))
		 ;; Language.
		 (or (cadr (assq (intern lang)
				 (plist-get info :latex-minted-langs)))
		     (downcase lang))
		 ;; Source code.
		 (let* ((code-info (org-export-unravel-code src-block))
			(max-width
			 (apply 'max
				(mapcar 'length
					(org-split-string (car code-info)
							  "\n")))))
		   (org-export-format-code
		    (car code-info)
		    (lambda (loc _num ref)
		      (concat
		       loc
		       (when ref
			 ;; Ensure references are flushed to the right,
			 ;; separated with 6 spaces from the widest line
			 ;; of code.
			 (concat (make-string (+ (- max-width (length loc)) 6)
					      ?\s)
				 (format "(%s)" ref)))))
		    nil (and retain-labels (cdr code-info)))))))
	  ;; Return value.
	  (format float-env body)))
       ;; Case 4.  Use listings package.
       (t
	(let ((lst-lang
	       (or (cadr (assq (intern lang)
			       (plist-get info :latex-listings-langs)))
		   lang))
	      (caption-str
	       (when caption
		 (let ((main (org-export-get-caption src-block))
		       (secondary (org-export-get-caption src-block t)))
		   (if (not secondary)
		       (format "{%s}" (org-export-data main info))
		     (format "{[%s]%s}"
			     (org-export-data secondary info)
			     (org-export-data main info))))))
	      (lst-opt (plist-get info :latex-listings-options)))
	  (concat
	   ;; Options.
	   (format
	    "\\lstset{%s}\n"
	    (concat
	     (org-latex--make-option-string
	      (append
	       lst-opt
	       (cond
		((and (not float) (plist-member attributes :float)) nil)
		((string= "multicolumn" float) '(("float" "*")))
		((and float (not (assoc "float" lst-opt)))
		 `(("float" ,(plist-get info :latex-default-figure-position)))))
	       `(("language" ,lst-lang))
	       (if label
		   `(("label" ,(org-latex--label src-block info)))
		 '(("label" " ")))
	       (if caption-str `(("caption" ,caption-str)) '(("caption" " ")))
	       `(("captionpos" ,(if caption-above-p "t" "b")))
	       (cond ((assoc "numbers" lst-opt) nil)
		     ((not num-start) '(("numbers" "none")))
		     (t `(("firstnumber" ,(number-to-string (1+ num-start)))
			  ("numbers" "left"))))))
	     (let ((local-options (plist-get attributes :options)))
	       (and local-options (concat "," local-options)))))
	   ;; Source code.
	   (format
	    "\\begin{lstlisting}\n%s\\end{lstlisting}"
	    (let* ((code-info (org-export-unravel-code src-block))
		   (max-width
		    (apply 'max
			   (mapcar 'length
				   (org-split-string (car code-info) "\n")))))
	      (org-export-format-code
	       (car code-info)
	       (lambda (loc _num ref)
		 (concat
		  loc
		  (when ref
		    ;; Ensure references are flushed to the right,
		    ;; separated with 6 spaces from the widest line of
		    ;; code
		    (concat (make-string (+ (- max-width (length loc)) 6) ?\s)
			    (format "(%s)" ref)))))
	       nil (and retain-labels (cdr code-info))))))))))))


;;;; Statistics Cookie

(defun org-latex-statistics-cookie (statistics-cookie _contents _info)
  "Transcode a STATISTICS-COOKIE object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (replace-regexp-in-string
   "%" "\\%" (org-element-property :value statistics-cookie) nil t))


;;;; Strike-Through

(defun org-latex-strike-through (_strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to LaTeX.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (org-latex--text-markup contents 'strike-through info))


;;;; Subscript

(defun org-latex--script-size (object info)
  "Transcode a subscript or superscript object.
OBJECT is an Org object.  INFO is a plist used as a communication
channel."
  (let ((output ""))
    (org-element-map (org-element-contents object)
	(cons 'plain-text org-element-all-objects)
      (lambda (obj)
	(cl-case (org-element-type obj)
	  ((entity latex-fragment)
	   (let ((data (org-trim (org-export-data obj info))))
	     (string-match
	      "\\`\\(?:\\\\[([]\\|\\$+\\)?\\(.*?\\)\\(?:\\\\[])]\\|\\$+\\)?\\'"
	      data)
	     (setq output
		   (concat output
			   (match-string 1 data)
			   (let ((blank (org-element-property :post-blank obj)))
			     (and blank (> blank 0) "\\ "))))))
	  (plain-text
	   (setq output
		 (format "%s\\text{%s}" output (org-export-data obj info))))
	  (otherwise
	   (setq output
		 (concat output
			 (org-export-data obj info)
			 (let ((blank (org-element-property :post-blank obj)))
			   (and blank (> blank 0) "\\ ")))))))
      info nil org-element-recursive-objects)
    ;; Result.  Do not wrap into curly brackets if OUTPUT is a single
    ;; character.
    (concat (if (eq (org-element-type object) 'subscript) "_" "^")
	    (and (> (length output) 1) "{")
	    output
	    (and (> (length output) 1) "}"))))

(defun org-latex-subscript (subscript _contents info)
  "Transcode a SUBSCRIPT object from Org to LaTeX.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (org-latex--script-size subscript info))


;;;; Superscript

(defun org-latex-superscript (superscript _contents info)
  "Transcode a SUPERSCRIPT object from Org to LaTeX.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (org-latex--script-size superscript info))


;;;; Table
;;
;; `org-latex-table' is the entry point for table transcoding.  It
;; takes care of tables with a "verbatim" mode.  Otherwise, it
;; delegates the job to either `org-latex--table.el-table',
;; `org-latex--org-table' or `org-latex--math-table' functions,
;; depending of the type of the table and the mode requested.
;;
;; `org-latex--align-string' is a subroutine used to build alignment
;; string for Org tables.

(defun org-latex-table (table contents info)
  "Transcode a TABLE element from Org to LaTeX.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (if (eq (org-element-property :type table) 'table.el)
      ;; "table.el" table.  Convert it using appropriate tools.
      (org-latex--table.el-table table info)
    (let ((type (or (org-export-read-attribute :attr_latex table :mode)
		    (plist-get info :latex-default-table-mode))))
      (cond
       ;; Case 1: Verbatim table.
       ((string= type "verbatim")
	(format "\\begin{verbatim}\n%s\n\\end{verbatim}"
		;; Re-create table, without affiliated keywords.
		(org-trim (org-element-interpret-data
			   `(table nil ,@(org-element-contents table))))))
       ;; Case 2: Matrix.
       ((or (string= type "math") (string= type "inline-math"))
	(org-latex--math-table table info))
       ;; Case 3: Standard table.
       (t (concat (org-latex--org-table table contents info)
		  ;; When there are footnote references within the
		  ;; table, insert their definition just after it.
		  (org-latex--delayed-footnotes-definitions table info)))))))

(defun org-latex--align-string (table info &optional math?)
  "Return an appropriate LaTeX alignment string.
TABLE is the considered table.  INFO is a plist used as
a communication channel.  When optional argument MATH? is
non-nil, TABLE is meant to be a matrix, where all cells are
centered."
  (or (org-export-read-attribute :attr_latex table :align)
      (let (align)
	;; Extract column groups and alignment from first (non-rule)
	;; row.
	(org-element-map
	    (org-element-map table 'table-row
	      (lambda (row)
		(and (eq (org-element-property :type row) 'standard) row))
	      info 'first-match)
	    'table-cell
	  (lambda (cell)
	    (let ((borders (org-export-table-cell-borders cell info)))
	      ;; Check left border for the first cell only.
	      (when (and (memq 'left borders) (not align))
		(push "|" align))
	      (push (if math? "c"	;center cells in matrices
		      (cl-case (org-export-table-cell-alignment cell info)
			(left "l")
			(right "r")
			(center "c")))
		    align)
	      (when (memq 'right borders) (push "|" align))))
	  info)
	(apply 'concat (nreverse align)))))

(defun org-latex--org-table (table contents info)
  "Return appropriate LaTeX code for an Org table.

TABLE is the table type element to transcode.  CONTENTS is its
contents, as a string.  INFO is a plist used as a communication
channel.

This function assumes TABLE has `org' as its `:type' property and
`table' as its `:mode' attribute."
  (let* ((caption (org-latex--caption/label-string table info))
	 (attr (org-export-read-attribute :attr_latex table))
	 ;; Determine alignment string.
	 (alignment (org-latex--align-string table info))
	 ;; Determine environment for the table: longtable, tabular...
	 (table-env (or (plist-get attr :environment)
			(plist-get info :latex-default-table-environment)))
	 ;; If table is a float, determine environment: table, table*
	 ;; or sidewaystable.
	 (float-env (unless (member table-env '("longtable" "longtabu"))
		      (let ((float (plist-get attr :float)))
			(cond
			 ((and (not float) (plist-member attr :float)) nil)
			 ((or (string= float "sidewaystable")
			      (string= float "sideways")) "sidewaystable")
			 ((string= float "multicolumn") "table*")
			 ((or float
			      (org-element-property :caption table)
			      (org-string-nw-p (plist-get attr :caption)))
			  "table")))))
	 ;; Extract others display options.
	 (fontsize (let ((font (plist-get attr :font)))
		     (and font (concat font "\n"))))
	 ;; "tabular" environment doesn't allow to define a width.
	 (width (and (not (equal table-env "tabular")) (plist-get attr :width)))
	 (spreadp (plist-get attr :spread))
	 (placement
	  (or (plist-get attr :placement)
	      (format "[%s]" (plist-get info :latex-default-figure-position))))
	 (centerp (if (plist-member attr :center) (plist-get attr :center)
		    (plist-get info :latex-tables-centered)))
	 (caption-above-p (org-latex--caption-above-p table info)))
    ;; Prepare the final format string for the table.
    (cond
     ;; Longtable.
     ((equal "longtable" table-env)
      (concat (and fontsize (concat "{" fontsize))
	      (format "\\begin{longtable}{%s}\n" alignment)
	      (and caption-above-p
		   (org-string-nw-p caption)
		   (concat caption "\\\\\n"))
	      contents
	      (and (not caption-above-p)
		   (org-string-nw-p caption)
		   (concat caption "\\\\\n"))
	      "\\end{longtable}\n"
	      (and fontsize "}")))
     ;; Longtabu
     ((equal "longtabu" table-env)
      (concat (and fontsize (concat "{" fontsize))
	      (format "\\begin{longtabu}%s{%s}\n"
		      (if width
			  (format " %s %s "
				  (if spreadp "spread" "to") width) "")
		      alignment)
	      (and caption-above-p
		   (org-string-nw-p caption)
		   (concat caption "\\\\\n"))
	      contents
	      (and (not caption-above-p)
		   (org-string-nw-p caption)
		   (concat caption "\\\\\n"))
	      "\\end{longtabu}\n"
	      (and fontsize "}")))
     ;; Others.
     (t (concat (cond
		 (float-env
		  (concat (format "\\begin{%s}%s\n" float-env placement)
			  (if caption-above-p caption "")
			  (when centerp "\\centering\n")
			  fontsize))
		 ((and (not float-env) caption)
		  (concat
		   (and centerp "\\begin{center}\n" )
		   (if caption-above-p caption "")
		   (cond ((and fontsize centerp) fontsize)
			 (fontsize (concat "{" fontsize)))))
		 (centerp (concat "\\begin{center}\n" fontsize))
		 (fontsize (concat "{" fontsize)))
		(cond ((equal "tabu" table-env)
		       (format "\\begin{tabu}%s{%s}\n%s\\end{tabu}"
			       (if width (format
					  (if spreadp " spread %s " " to %s ")
					  width) "")
			       alignment
			       contents))
		      (t (format "\\begin{%s}%s{%s}\n%s\\end{%s}"
				 table-env
				 (if width (format "{%s}" width) "")
				 alignment
				 contents
				 table-env)))
		(cond
		 (float-env
		  (concat (if caption-above-p "" (concat "\n" caption))
			  (format "\n\\end{%s}" float-env)))
		 ((and (not float-env) caption)
		  (concat
		   (if caption-above-p "" (concat "\n" caption))
		   (and centerp "\n\\end{center}")
		   (and fontsize (not centerp) "}")))
		 (centerp "\n\\end{center}")
		 (fontsize "}")))))))

(defun org-latex--table.el-table (table info)
  "Return appropriate LaTeX code for a table.el table.

TABLE is the table type element to transcode.  INFO is a plist
used as a communication channel.

This function assumes TABLE has `table.el' as its `:type'
property."
  (require 'table)
  ;; Ensure "*org-export-table*" buffer is empty.
  (with-current-buffer (get-buffer-create "*org-export-table*")
    (erase-buffer))
  (let ((output (with-temp-buffer
		  (insert (org-element-property :value table))
		  (goto-char 1)
		  (re-search-forward "^[ \t]*|[^|]" nil t)
		  (table-generate-source 'latex "*org-export-table*")
		  (with-current-buffer "*org-export-table*"
		    (org-trim (buffer-string))))))
    (kill-buffer (get-buffer "*org-export-table*"))
    ;; Remove left out comments.
    (while (string-match "^%.*\n" output)
      (setq output (replace-match "" t t output)))
    (let ((attr (org-export-read-attribute :attr_latex table)))
      (when (plist-get attr :rmlines)
	;; When the "rmlines" attribute is provided, remove all hlines
	;; but the the one separating heading from the table body.
	(let ((n 0) (pos 0))
	  (while (and (< (length output) pos)
		      (setq pos (string-match "^\\\\hline\n?" output pos)))
	    (cl-incf n)
	    (unless (= n 2) (setq output (replace-match "" nil nil output))))))
      (let ((centerp (if (plist-member attr :center) (plist-get attr :center)
		       (plist-get info :latex-tables-centered))))
	(if (not centerp) output
	  (format "\\begin{center}\n%s\n\\end{center}" output))))))

(defun org-latex--math-table (table info)
  "Return appropriate LaTeX code for a matrix.

TABLE is the table type element to transcode.  INFO is a plist
used as a communication channel.

This function assumes TABLE has `org' as its `:type' property and
`inline-math' or `math' as its `:mode' attribute."
  (let* ((attr (org-export-read-attribute :attr_latex table))
	 (env (or (plist-get attr :environment)
		  (plist-get info :latex-default-table-environment)))
	 (contents
	  (mapconcat
	   (lambda (row)
	     (if (eq (org-element-property :type row) 'rule) "\\hline"
	       ;; Return each cell unmodified.
	       (concat
		(mapconcat
		 (lambda (cell)
		   (substring (org-element-interpret-data cell) 0 -1))
		 (org-element-map row 'table-cell #'identity info) "&")
		(or (cdr (assoc env org-latex-table-matrix-macros)) "\\\\")
		"\n")))
	   (org-element-map table 'table-row #'identity info) "")))
    (concat
     ;; Prefix.
     (plist-get attr :math-prefix)
     ;; Environment.  Also treat special cases.
     (cond ((member env '("array" "tabular"))
	    (format "\\begin{%s}{%s}\n%s\\end{%s}"
		    env (org-latex--align-string table info t) contents env))
	   ((assoc env org-latex-table-matrix-macros)
	    (format "\\%s%s{\n%s}"
		    env
		    (or (plist-get attr :math-arguments) "")
		    contents))
	   (t (format "\\begin{%s}\n%s\\end{%s}" env contents env)))
     ;; Suffix.
     (plist-get attr :math-suffix))))


;;;; Table Cell

(defun org-latex-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to LaTeX.
CONTENTS is the cell contents.  INFO is a plist used as
a communication channel."
  (concat
   (let ((scientific-format (plist-get info :latex-table-scientific-notation)))
     (if (and contents
	      scientific-format
	      (string-match orgtbl-exp-regexp contents))
	 ;; Use appropriate format string for scientific
	 ;; notation.
	 (format scientific-format
		 (match-string 1 contents)
		 (match-string 2 contents))
       contents))
   (when (org-export-get-next-element table-cell info) " & ")))


;;;; Table Row

(defun org-latex-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to LaTeX.
CONTENTS is the contents of the row.  INFO is a plist used as
a communication channel."
  (let* ((attr (org-export-read-attribute :attr_latex
					  (org-export-get-parent table-row)))
	 (booktabsp (if (plist-member attr :booktabs) (plist-get attr :booktabs)
		      (plist-get info :latex-tables-booktabs)))
	 (longtablep
	  (member (or (plist-get attr :environment)
		      (plist-get info :latex-default-table-environment))
		  '("longtable" "longtabu"))))
    (if (eq (org-element-property :type table-row) 'rule)
	(cond
	 ((not booktabsp) "\\hline")
	 ((not (org-export-get-previous-element table-row info)) "\\toprule")
	 ((not (org-export-get-next-element table-row info)) "\\bottomrule")
	 ((and longtablep
	       (org-export-table-row-ends-header-p
		(org-export-get-previous-element table-row info) info))
	  "")
	 (t "\\midrule"))
      (concat
       ;; When BOOKTABS are activated enforce top-rule even when no
       ;; hline was specifically marked.
       (and booktabsp (not (org-export-get-previous-element table-row info))
	    "\\toprule\n")
       contents "\\\\\n"
       (cond
	;; Special case for long tables.  Define header and footers.
	((and longtablep (org-export-table-row-ends-header-p table-row info))
	 (let ((columns (cdr (org-export-table-dimensions
			      (org-export-get-parent-table table-row) info))))
	   (format "%s
\\endfirsthead
\\multicolumn{%d}{l}{%s} \\\\
%s
%s \\\\\n
%s
\\endhead
%s\\multicolumn{%d}{r}{%s} \\\\
\\endfoot
\\endlastfoot"
		   (if booktabsp "\\midrule" "\\hline")
		   columns
		   (org-latex--translate "Continued from previous page" info)
		   (cond
		    ((not (org-export-table-row-starts-header-p table-row info))
		     "")
		    (booktabsp "\\toprule\n")
		    (t "\\hline\n"))
		   contents
		   (if booktabsp "\\midrule" "\\hline")
		   (if booktabsp "\\midrule" "\\hline")
		   columns
		   (org-latex--translate "Continued on next page" info))))
	;; When BOOKTABS are activated enforce bottom rule even when
	;; no hline was specifically marked.
	((and booktabsp (not (org-export-get-next-element table-row info)))
	 "\\bottomrule"))))))


;;;; Target

(defun org-latex-target (target _contents info)
  "Transcode a TARGET object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "\\label{%s}" (org-latex--label target info)))


;;;; Timestamp

(defun org-latex-timestamp (timestamp _contents info)
  "Transcode a TIMESTAMP object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((value (org-latex-plain-text (org-timestamp-translate timestamp) info)))
    (format
     (plist-get info
		(cl-case (org-element-property :type timestamp)
		  ((active active-range) :latex-active-timestamp-format)
		  ((inactive inactive-range) :latex-inactive-timestamp-format)
		  (otherwise :latex-diary-timestamp-format)))
     value)))


;;;; Underline

(defun org-latex-underline (_underline contents info)
  "Transcode UNDERLINE from Org to LaTeX.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (org-latex--text-markup contents 'underline info))


;;;; Verbatim

(defun org-latex-verbatim (verbatim _contents info)
  "Transcode a VERBATIM object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-latex--text-markup
   (org-element-property :value verbatim) 'verbatim info))


;;;; Verse Block

(defun org-latex-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to LaTeX.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  (org-latex--wrap-label
   verse-block
   ;; In a verse environment, add a line break to each newline
   ;; character and change each white space at beginning of a line
   ;; into a space of 1 em.  Also change each blank line with
   ;; a vertical space of 1 em.
   (format "\\begin{verse}\n%s\\end{verse}"
	   (replace-regexp-in-string
	    "^[ \t]+" (lambda (m) (format "\\hspace*{%dem}" (length m)))
	    (replace-regexp-in-string
	     "^[ \t]*\\\\\\\\$" "\\vspace*{1em}"
	     (replace-regexp-in-string
	      "\\([ \t]*\\\\\\\\\\)?[ \t]*\n" "\\\\\n"
	      contents nil t) nil t) nil t))
   info))



;;; End-user functions

;;;###autoload
(defun org-latex-export-as-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a LaTeX buffer.

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

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org LATEX Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'latex "*Org LATEX Export*"
    async subtreep visible-only body-only ext-plist (lambda () (LaTeX-mode))))

;;;###autoload
(defun org-latex-convert-region-to-latex ()
  "Assume the current region has Org syntax, and convert it to LaTeX.
This can be used in any buffer.  For example, you can write an
itemized list in Org syntax in an LaTeX buffer and use this
command to convert it."
  (interactive)
  (org-export-replace-region-by 'latex))

;;;###autoload
(defun org-latex-export-to-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a LaTeX file.

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

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'latex outfile
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-latex-export-to-pdf
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to LaTeX then process through to PDF.

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

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'latex outfile
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))

(defun org-latex-compile (texfile &optional snippet)
  "Compile a TeX file.

TEXFILE is the name of the file being compiled.  Processing is
done through the command specified in `org-latex-pdf-process',
which see.  Output is redirected to \"*Org PDF LaTeX Output*\"
buffer.

When optional argument SNIPPET is non-nil, TEXFILE is a temporary
file used to preview a LaTeX snippet.  In this case, do not
create a log buffer and do not remove log files.

Return PDF file name or raise an error if it couldn't be
produced."
  (unless snippet (message "Processing LaTeX file %s..." texfile))
  (let* ((compiler
	  (or (with-temp-buffer
		(save-excursion (insert-file-contents texfile))
		(and (search-forward-regexp (regexp-opt org-latex-compilers)
					    (line-end-position 2)
					    t)
		     (progn (beginning-of-line) (looking-at-p "%"))
		     (match-string 0)))
	      "pdflatex"))
	 (process (if (functionp org-latex-pdf-process) org-latex-pdf-process
		    ;; Replace "%latex" and "%bibtex" with,
		    ;; respectively, "%L" and "%B" so as to adhere to
		    ;; `format-spec' specifications.
		    (mapcar (lambda (command)
			      (replace-regexp-in-string
			       "%\\(?:bib\\|la\\)tex\\>"
			       (lambda (m) (upcase (substring m 0 2)))
			       command))
			    org-latex-pdf-process)))
         (spec `((?B . ,(shell-quote-argument org-latex-bib-compiler))
                 (?L . ,(shell-quote-argument compiler))))
	 (log-buf-name "*Org PDF LaTeX Output*")
         (log-buf (and (not snippet) (get-buffer-create log-buf-name)))
         (outfile (org-compile-file texfile process "pdf"
				    (format "See %S for details" log-buf-name)
				    log-buf spec)))
    (unless snippet
      (when org-latex-remove-logfiles
	(mapc #'delete-file
	      (directory-files
	       (file-name-directory outfile)
	       t
	       (concat (regexp-quote (file-name-base outfile))
		       "\\(?:\\.[0-9]+\\)?\\."
		       (regexp-opt org-latex-logfiles-extensions))
	       t)))
      (let ((warnings (org-latex--collect-warnings log-buf)))
	(message (concat "PDF file produced"
			 (cond
			  ((eq warnings 'error) " with errors.")
			  (warnings (concat " with warnings: " warnings))
			  (t "."))))))
    ;; Return output file name.
    outfile))

(defun org-latex--collect-warnings (buffer)
  "Collect some warnings from \"pdflatex\" command output.
BUFFER is the buffer containing output.  Return collected
warnings types as a string, `error' if a LaTeX error was
encountered or nil if there was none."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward "^[ \t]*This is .*?TeX.*?Version" nil t)
	(if (re-search-forward "^!" nil t) 'error
	  (let ((case-fold-search t)
		(warnings ""))
	    (dolist (warning org-latex-known-warnings)
	      (when (save-excursion (re-search-forward (car warning) nil t))
		(setq warnings (concat warnings " " (cdr warning)))))
	    (org-string-nw-p (org-trim warnings))))))))

;;;###autoload
(defun org-latex-publish-to-latex (plist filename pub-dir)
  "Publish an Org file to LaTeX.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'latex filename ".tex" plist pub-dir))

;;;###autoload
(defun org-latex-publish-to-pdf (plist filename pub-dir)
  "Publish an Org file to PDF (via LaTeX).

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  ;; Unlike to `org-latex-publish-to-latex', PDF file is generated
  ;; in working directory and then moved to publishing directory.
  (org-publish-attachment
   plist
   ;; Default directory could be anywhere when this function is
   ;; called.  We ensure it is set to source file directory during
   ;; compilation so as to not break links to external documents.
   (let ((default-directory (file-name-directory filename)))
     (org-latex-compile
      (org-publish-org-to
       'latex filename ".tex" plist (file-name-directory filename))))
   pub-dir))


(provide 'ox-latex)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-latex.el ends here
