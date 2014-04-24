;;; ox-latex.el --- LaTeX Back-End for Org Export Engine

;; Copyright (C) 2011-2014 Free Software Foundation, Inc.

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
;; See Org manual for details.

;;; Code:

(eval-when-compile (require 'cl))
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
    (comment . (lambda (&rest args) ""))
    (comment-block . (lambda (&rest args) ""))
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
    (paragraph . org-latex-paragraph)
    (plain-list . org-latex-plain-list)
    (plain-text . org-latex-plain-text)
    (planning . org-latex-planning)
    (property-drawer . (lambda (&rest args) ""))
    (quote-block . org-latex-quote-block)
    (quote-section . org-latex-quote-section)
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
    (verse-block . org-latex-verse-block))
  :export-block '("LATEX" "TEX")
  :menu-entry
  '(?l "Export to LaTeX"
       ((?L "As LaTeX buffer" org-latex-export-as-latex)
	(?l "As LaTeX file" org-latex-export-to-latex)
	(?p "As PDF file" org-latex-export-to-pdf)
	(?o "As PDF file and open"
	    (lambda (a s v b)
	      (if a (org-latex-export-to-pdf t s v b)
		(org-open-file (org-latex-export-to-pdf nil s v b)))))))
  :options-alist '((:latex-class "LATEX_CLASS" nil org-latex-default-class t)
		   (:latex-class-options "LATEX_CLASS_OPTIONS" nil nil t)
		   (:latex-header "LATEX_HEADER" nil nil newline)
		   (:latex-header-extra "LATEX_HEADER_EXTRA" nil nil newline)
		   (:latex-hyperref-p nil "texht" org-latex-with-hyperref t)
		   ;; Redefine regular options.
		   (:date "DATE" nil "\\today" t)))



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

(defconst org-latex-table-matrix-macros '(("bordermatrix" . "\\cr")
					    ("qbordermatrix" . "\\cr")
					    ("kbordermatrix" . "\\\\"))
  "Alist between matrix macros and their row ending.")



;;; User Configurable Variables

(defgroup org-export-latex nil
  "Options for exporting Org mode files to LaTeX."
  :tag "Org Export LaTeX"
  :group 'org-export)


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

  \(class-name
    header-string
    \(numbered-section . unnumbered-section)
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

Likewise, if your header contains \"\\usepackage[AUTO]{babel}\",
AUTO will be replaced with the language related to the language
code specified by `org-export-default-language', which see.  Note
that constructions such as \"\\usepackage[french,AUTO,english]{babel}\"
are permitted.

The sectioning structure
------------------------

The sectioning structure of the class is given by the elements
following the header string.  For each sectioning level, a number
of strings is specified.  A %s formatter is mandatory in each
section string and will be replaced by the title of the section.

Instead of a cons cell (numbered . unnumbered), you can also
provide a list of 2 or 4 elements,

  \(numbered-open numbered-close)

or

  \(numbered-open numbered-close unnumbered-open unnumbered-close)

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
If this string contains the formatting specification \"%s\" then
it will be used as a formatting string, passing the title as an
argument."
  :group 'org-export-latex
  :type 'string)

(defcustom org-latex-toc-command "\\tableofcontents\n\n"
  "LaTeX command to set the table of contents, list of figures, etc.
This command only applies to the table of contents generated with
the toc:nil option, not to those generated with #+TOC keyword."
  :group 'org-export-latex
  :type 'string)

(defcustom org-latex-with-hyperref t
  "Toggle insertion of \\hypersetup{...} in the preamble."
  :group 'org-export-latex
  :type 'boolean)

;;;; Headline

(defcustom org-latex-format-headline-function
  'org-latex-format-headline-default-function
  "Function for formatting the headline's text.

This function will be called with 5 arguments:
TODO      the todo keyword (string or nil).
TODO-TYPE the type of todo (symbol: `todo', `done', nil)
PRIORITY  the priority of the headline (integer or nil)
TEXT      the main headline text (string).
TAGS      the tags as a list of strings (list of strings or nil).

The function result will be used in the section format string.

Use `org-latex-format-headline-default-function' by default,
which format headlines like for Org version prior to 8.0."
  :group 'org-export-latex
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'function)


;;;; Footnotes

(defcustom org-latex-footnote-separator "\\textsuperscript{,}\\,"
  "Text used to separate footnotes."
  :group 'org-export-latex
  :type 'string)


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

(defcustom org-latex-default-figure-position "htb"
  "Default position for latex figures."
  :group 'org-export-latex
  :type 'string)

(defcustom org-latex-inline-image-rules
  '(("file" . "\\.\\(pdf\\|jpeg\\|jpg\\|png\\|ps\\|eps\\|tikz\\|pgf\\|svg\\)\\'"))
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
		 (const :tag "Verbatim" verbatim)))

(defcustom org-latex-tables-centered t
  "When non-nil, tables are exported in a center environment."
  :group 'org-export-latex
  :type 'boolean)

(defcustom org-latex-tables-booktabs nil
  "When non-nil, display tables in a formal \"booktabs\" style.
This option assumes that the \"booktabs\" package is properly
loaded in the header of the document.  This value can be ignored
locally with \":booktabs t\" and \":booktabs nil\" LaTeX
attributes."
  :group 'org-export-latex
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-latex-table-caption-above t
  "When non-nil, place caption string at the beginning of the table.
Otherwise, place it near the end."
  :group 'org-export-latex
  :type 'boolean)

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
	  (const :tag "No formatting")))


;;;; Text markup

(defcustom org-latex-text-markup-alist '((bold . "\\textbf{%s}")
					 (code . verb)
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
  :type 'alist
  :options '(bold code italic strike-through underline verbatim))


;;;; Drawers

(defcustom org-latex-format-drawer-function
  (lambda (name contents) contents)
  "Function called to format a drawer in LaTeX code.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.

The function should return the string to be exported.

The default function simply returns the value of CONTENTS."
  :group 'org-export-latex
  :version "24.4"
  :package-version '(Org . "8.3")
  :type 'function)


;;;; Inlinetasks

(defcustom org-latex-format-inlinetask-function 'ignore
  "Function called to format an inlinetask in LaTeX code.

The function must accept six parameters:
  TODO      the todo keyword, as a string
  TODO-TYPE the todo type, a symbol among `todo', `done' and nil.
  PRIORITY  the inlinetask priority, as a string
  NAME      the inlinetask name, as a string.
  TAGS      the inlinetask tags, as a list of strings.
  CONTENTS  the contents of the inlinetask, as a string.

The function should return the string to be exported.

For example, the variable could be set to the following function
in order to mimic default behaviour:

\(defun org-latex-format-inlinetask \(todo type priority name tags contents\)
\"Format an inline task element for LaTeX export.\"
  \(let ((full-title
	 \(concat
	  \(when todo
            \(format \"\\\\textbf{\\\\textsf{\\\\textsc{%s}}} \" todo))
	  \(when priority (format \"\\\\framebox{\\\\#%c} \" priority))
	  title
	  \(when tags
            \(format \"\\\\hfill{}\\\\textsc{:%s:}\"
                    \(mapconcat 'identity tags \":\")))))
    \(format (concat \"\\\\begin{center}\\n\"
		    \"\\\\fbox{\\n\"
		    \"\\\\begin{minipage}[c]{.6\\\\textwidth}\\n\"
		    \"%s\\n\\n\"
		    \"\\\\rule[.8em]{\\\\textwidth}{2pt}\\n\\n\"
		    \"%s\"
		    \"\\\\end{minipage}}\"
		    \"\\\\end{center}\")
	    full-title contents))"
  :group 'org-export-latex
  :type 'function)


;; Src blocks

(defcustom org-latex-listings nil
  "Non-nil means export source code using the listings package.

This package will fontify source code, possibly even with color.
If you want to use this, you also need to make LaTeX use the
listings package, and if you want to have color, the color
package.  Just add these to `org-latex-packages-alist', for
example using customize, or with something like:

  \(require 'ox-latex)
  \(add-to-list 'org-latex-packages-alist '(\"\" \"listings\"))
  \(add-to-list 'org-latex-packages-alist '(\"\" \"color\"))

Alternatively,

  \(setq org-latex-listings 'minted)

causes source code to be exported using the minted package as
opposed to listings.  If you want to use minted, you need to add
the minted package to `org-latex-packages-alist', for example
using customize, or with

  \(require 'ox-latex)
  \(add-to-list 'org-latex-packages-alist '(\"\" \"minted\"))

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
	  (const :tag "Export verbatim" nil)))

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
    (sql "SQL") (sqlite "sql"))
  "Alist mapping languages to their listing language counterpart.
The key is a symbol, the major mode symbol without the \"-mode\".
The value is the string that should be inserted as the language
parameter for the listings package.  If the mode name and the
listings name are the same, the language does not need an entry
in this list - but it does not hurt if it is present."
  :group 'org-export-latex
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
    '((\"basicstyle\" \"\\\\small\")
      (\"keywordstyle\" \"\\\\color{black}\\\\bfseries\\\\underbar\")))

will typeset the code in a small size font with underlined, bold
black keywords.

Note that the same options will be applied to blocks of all
languages."
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

  \(setq org-latex-minted-options
    '\((\"bgcolor\" \"bg\") \(\"frame\" \"lines\")))

will result in src blocks being exported with

\\begin{minted}[bgcolor=bg,frame=lines]{<LANG>}

as the start of the minted environment. Note that the same
options will be applied to blocks of all languages."
  :group 'org-export-latex
  :type '(repeat
	  (list
	   (string :tag "Minted option name ")
	   (string :tag "Minted option value"))))

(defvar org-latex-custom-lang-environments nil
  "Alist mapping languages to language-specific LaTeX environments.

It is used during export of src blocks by the listings and minted
latex packages.  For example,

  \(setq org-latex-custom-lang-environments
     '\(\(python \"pythoncode\"\)\)\)

would have the effect that if org encounters begin_src python
during latex export it will output

  \\begin{pythoncode}
  <src block body>
  \\end{pythoncode}")


;;;; Compilation

(defcustom org-latex-pdf-process
  '("pdflatex -interaction nonstopmode -output-directory %o %f"
    "pdflatex -interaction nonstopmode -output-directory %o %f"
    "pdflatex -interaction nonstopmode -output-directory %o %f")
  "Commands to process a LaTeX file to a PDF file.
This is a list of strings, each of them will be given to the
shell as a command.  %f in the command will be replaced by the
full file name, %b by the file base name (i.e. without directory
and extension parts) and %o by the base directory of the file.

The reason why this is a list is that it usually takes several
runs of `pdflatex', maybe mixed with a call to `bibtex'.  Org
does not have a clever mechanism to detect which of these
commands have to be run to get to a stable result, and it also
does not do any error checking.

By default, Org uses 3 runs of `pdflatex' to do the processing.
If you have texi2dvi on your system and if that does not cause
the infamous egrep/locale bug:

     http://lists.gnu.org/archive/html/bug-texinfo/2010-03/msg00031.html

then `texi2dvi' is the superior choice as it automates the LaTeX
build process by calling the \"correct\" combinations of
auxiliary programs.  Org does offer `texi2dvi' as one of the
customize options.  Alternatively, `rubber' and `latexmk' also
provide similar functionality.  The latter supports `biber' out
of the box.

Alternatively, this may be a Lisp function that does the
processing, so you could use this to apply the machinery of
AUCTeX or the Emacs LaTeX mode.  This function should accept the
file name as its single argument."
  :group 'org-export-pdf
  :type '(choice
	  (repeat :tag "Shell command sequence"
		  (string :tag "Shell command"))
	  (const :tag "2 runs of pdflatex"
		 ("pdflatex -interaction nonstopmode -output-directory %o %f"
		   "pdflatex -interaction nonstopmode -output-directory %o %f"))
	  (const :tag "3 runs of pdflatex"
		 ("pdflatex -interaction nonstopmode -output-directory %o %f"
		   "pdflatex -interaction nonstopmode -output-directory %o %f"
		   "pdflatex -interaction nonstopmode -output-directory %o %f"))
	  (const :tag "pdflatex,bibtex,pdflatex,pdflatex"
		 ("pdflatex -interaction nonstopmode -output-directory %o %f"
		   "bibtex %b"
		   "pdflatex -interaction nonstopmode -output-directory %o %f"
		   "pdflatex -interaction nonstopmode -output-directory %o %f"))
	  (const :tag "2 runs of xelatex"
		 ("xelatex -interaction nonstopmode -output-directory %o %f"
		  "xelatex -interaction nonstopmode -output-directory %o %f"))
	  (const :tag "3 runs of xelatex"
		 ("xelatex -interaction nonstopmode -output-directory %o %f"
		  "xelatex -interaction nonstopmode -output-directory %o %f"
		  "xelatex -interaction nonstopmode -output-directory %o %f"))
	  (const :tag "xelatex,bibtex,xelatex,xelatex"
		 ("xelatex -interaction nonstopmode -output-directory %o %f"
		  "bibtex %b"
		  "xelatex -interaction nonstopmode -output-directory %o %f"
		  "xelatex -interaction nonstopmode -output-directory %o %f"))
	  (const :tag "texi2dvi"
		 ("texi2dvi -p -b -V %f"))
	  (const :tag "rubber"
		 ("rubber -d --into %o %f"))
	  (const :tag "latexmk"
		 ("latexmk -g -pdf %f"))
	  (function)))

(defcustom org-latex-logfiles-extensions
  '("aux" "idx" "log" "out" "toc" "nav" "snm" "vrb")
  "The list of file extensions to consider as LaTeX logfiles.
The logfiles will be remove if `org-latex-remove-logfiles' is
non-nil."
  :group 'org-export-latex
  :type '(repeat (string :tag "Extension")))

(defcustom org-latex-remove-logfiles t
  "Non-nil means remove the logfiles produced by PDF production.
By default, logfiles are files with these extensions: .aux, .idx,
.log, .out, .toc, .nav, .snm and .vrb.  To define the set of
logfiles to remove, set `org-latex-logfiles-extensions'."
  :group 'org-export-latex
  :type 'boolean)

(defcustom org-latex-known-errors
  '(("Reference.*?undefined" .  "[undefined reference]")
    ("Citation.*?undefined" .  "[undefined citation]")
    ("Undefined control sequence" .  "[undefined control sequence]")
    ("^! LaTeX.*?Error" .  "[LaTeX error]")
    ("^! Package.*?Error" .  "[package error]")
    ("Runaway argument" .  "Runaway argument"))
  "Alist of regular expressions and associated messages for the user.
The regular expressions are used to find possible errors in the
log of a latex-run."
  :group 'org-export-latex
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(repeat
	  (cons
	   (string :tag "Regexp")
	   (string :tag "Message"))))



;;; Internal Functions

(defun org-latex--caption/label-string (element info)
  "Return caption and label LaTeX string for ELEMENT.

INFO is a plist holding contextual information.  If there's no
caption nor label, return the empty string.

For non-floats, see `org-latex--wrap-label'."
  (let* ((label (org-element-property :name element))
	 (label-str (if (not (org-string-nw-p label)) ""
		      (format "\\label{%s}"
			      (org-export-solidify-link-text label))))
	 (main (org-export-get-caption element))
	 (short (org-export-get-caption element t))
	 (caption-from-attr-latex (org-export-read-attribute :attr_latex element :caption)))
    (cond
     ((org-string-nw-p caption-from-attr-latex)
      (concat caption-from-attr-latex "\n"))
     ((and (not main) (equal label-str "")) "")
     ((not main) (concat label-str "\n"))
     ;; Option caption format with short name.
     (short (format "\\caption[%s]{%s%s}\n"
		    (org-export-data short info)
		    label-str
		    (org-export-data main info)))
     ;; Standard caption format.
     (t (format "\\caption{%s%s}\n" label-str (org-export-data main info))))))

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
	    (language (cdr (assoc language-code
				  org-latex-babel-language-alist))))
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

(defun org-latex--find-verb-separator (s)
  "Return a character not used in string S.
This is used to choose a separator for constructs like \\verb."
  (let ((ll "~,./?;':\"|!@#%^&-_=+abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ<>()[]{}"))
    (loop for c across ll
	  when (not (string-match (regexp-quote (char-to-string c)) s))
	  return (char-to-string c))))

(defun org-latex--make-option-string (options)
  "Return a comma separated string of keywords and values.
OPTIONS is an alist where the key is the options keyword as
a string, and the value a list containing the keyword value, or
nil."
  (mapconcat (lambda (pair)
	       (concat (first pair)
		       (when (> (length (second pair)) 0)
			 (concat "=" (second pair)))))
	     options
	     ","))

(defun org-latex--wrap-label (element output)
  "Wrap label associated to ELEMENT around OUTPUT, if appropriate.
This function shouldn't be used for floats.  See
`org-latex--caption/label-string'."
  (let ((label (org-element-property :name element)))
    (if (not (and (org-string-nw-p output) (org-string-nw-p label))) output
      (concat (format "\\label{%s}\n" (org-export-solidify-link-text label))
	      output))))

(defun org-latex--text-markup (text markup)
  "Format TEXT depending on MARKUP text markup.
See `org-latex-text-markup-alist' for details."
  (let ((fmt (cdr (assq markup org-latex-text-markup-alist))))
    (cond
     ;; No format string: Return raw text.
     ((not fmt) text)
     ;; Handle the `verb' special case: Find and appropriate separator
     ;; and use "\\verb" command.
     ((eq 'verb fmt)
      (let ((separator (org-latex--find-verb-separator text)))
	(concat "\\verb" separator
		(replace-regexp-in-string "\n" " " text)
		separator)))
     ;; Handle the `protectedtexttt' special case: Protect some
     ;; special chars and use "\texttt{%s}" format string.
     ((eq 'protectedtexttt fmt)
      (let ((start 0)
	    (trans '(("\\" . "\\textbackslash{}")
		     ("~" . "\\textasciitilde{}")
		     ("^" . "\\textasciicircum{}")))
	    (rtn "")
	    char)
	(while (string-match "[\\{}$%&_#~^]" text)
	  (setq char (match-string 0 text))
	  (if (> (match-beginning 0) 0)
	      (setq rtn (concat rtn (substring text 0 (match-beginning 0)))))
	  (setq text (substring text (1+ (match-beginning 0))))
	  (setq char (or (cdr (assoc char trans)) (concat "\\" char))
		rtn (concat rtn char)))
	(setq text (concat rtn text)
	      fmt "\\texttt{%s}")
	(while (string-match "--" text)
	  (setq text (replace-match "-{}-" t t text)))
	(format fmt text)))
     ;; Else use format string.
     (t (format fmt text)))))

(defun org-latex--delayed-footnotes-definitions (element info)
  "Return footnotes definitions in ELEMENT as a string.

INFO is a plist used as a communication channel.

Footnotes definitions are returned within \"\\footnotetxt{}\"
commands.

This function is used within constructs that don't support
\"\\footnote{}\" command (i.e. an item's tag).  In that case,
\"\\footnotemark\" is used within the construct and the function
just outside of it."
  (mapconcat
   (lambda (ref)
     (format
      "\\footnotetext[%s]{%s}"
      (org-export-get-footnote-number ref info)
      (org-trim
       (org-export-data
	(org-export-get-footnote-definition ref info) info))))
   ;; Find every footnote reference in ELEMENT.
   (let* (all-refs
	  search-refs			; For byte-compiler.
	  (search-refs
	   (function
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
	      (reverse all-refs)))))
     (funcall search-refs element))
   ""))



;;; Template

(defun org-latex-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info)))
    (concat
     ;; Time-stamp.
     (and (plist-get info :time-stamp-file)
	  (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     ;; Document class and packages.
     (let* ((class (plist-get info :latex-class))
	    (class-options (plist-get info :latex-class-options))
	    (header (nth 1 (assoc class org-latex-classes)))
	    (document-class-string
	     (and (stringp header)
		  (if (not class-options) header
		    (replace-regexp-in-string
		     "^[ \t]*\\\\documentclass\\(\\(\\[[^]]*\\]\\)?\\)"
		     class-options header t nil 1)))))
       (if (not document-class-string)
	   (user-error "Unknown LaTeX class `%s'" class)
	 (org-latex-guess-babel-language
	  (org-latex-guess-inputenc
	   (org-element-normalize-string
	    (org-splice-latex-header
	     document-class-string
	     org-latex-default-packages-alist
	     org-latex-packages-alist nil
	     (concat (org-element-normalize-string
		      (plist-get info :latex-header))
		     (plist-get info :latex-header-extra)))))
	  info)))
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
     ;; Title
     (format "\\title{%s}\n" title)
     ;; Hyperref options.
     (when (plist-get info :latex-hyperref-p)
       (format "\\hypersetup{\n  pdfkeywords={%s},\n  pdfsubject={%s},\n  pdfcreator={%s}}\n"
	       (or (plist-get info :keywords) "")
	       (or (plist-get info :description) "")
	       (if (not (plist-get info :with-creator)) ""
		 (plist-get info :creator))))
     ;; Document start.
     "\\begin{document}\n\n"
     ;; Title command.
     (org-element-normalize-string
      (cond ((string= "" title) nil)
	    ((not (stringp org-latex-title-command)) nil)
	    ((string-match "\\(?:[^%]\\|^\\)%s"
			   org-latex-title-command)
	     (format org-latex-title-command title))
	    (t org-latex-title-command)))
     ;; Table of contents.
     (let ((depth (plist-get info :with-toc)))
       (when depth
	 (concat (when (wholenump depth)
		   (format "\\setcounter{tocdepth}{%d}\n" depth))
		 org-latex-toc-command)))
     ;; Document's body.
     contents
     ;; Creator.
     (let ((creator-info (plist-get info :with-creator)))
       (cond
	((not creator-info) "")
	((eq creator-info 'comment)
	 (format "%% %s\n" (plist-get info :creator)))
	(t (concat (plist-get info :creator) "\n"))))
     ;; Document end.
     "\\end{document}")))



;;; Transcode Functions

;;;; Bold

(defun org-latex-bold (bold contents info)
  "Transcode BOLD from Org to LaTeX.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (org-latex--text-markup contents 'bold))


;;;; Center Block

(defun org-latex-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the center block.  INFO is a plist
holding contextual information."
  (org-latex--wrap-label
   center-block
   (format "\\begin{center}\n%s\\end{center}" contents)))


;;;; Clock

(defun org-latex-clock (clock contents info)
  "Transcode a CLOCK element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (concat
   "\\noindent"
   (format "\\textbf{%s} " org-clock-string)
   (format org-latex-inactive-timestamp-format
	   (concat (org-translate-time
		    (org-element-property :raw-value
					  (org-element-property :value clock)))
		   (let ((time (org-element-property :duration clock)))
		     (and time (format " (%s)" time)))))
   "\\\\"))


;;;; Code

(defun org-latex-code (code contents info)
  "Transcode a CODE object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-latex--text-markup (org-element-property :value code) 'code))


;;;; Drawer

(defun org-latex-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((name (org-element-property :drawer-name drawer))
	 (output (funcall org-latex-format-drawer-function
			  name contents)))
    (org-latex--wrap-label drawer output)))


;;;; Dynamic Block

(defun org-latex-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  (org-latex--wrap-label dynamic-block contents))


;;;; Entity

(defun org-latex-entity (entity contents info)
  "Transcode an ENTITY object from Org to LaTeX.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (let ((ent (org-element-property :latex entity)))
    (if (org-element-property :latex-math-p entity) (format "$%s$" ent) ent)))


;;;; Example Block

(defun org-latex-example-block (example-block contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (when (org-string-nw-p (org-element-property :value example-block))
    (org-latex--wrap-label
     example-block
     (format "\\begin{verbatim}\n%s\\end{verbatim}"
	     (org-export-format-code-default example-block info)))))


;;;; Export Block

(defun org-latex-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (member (org-element-property :type export-block) '("LATEX" "TEX"))
    (org-remove-indentation (org-element-property :value export-block))))


;;;; Export Snippet

(defun org-latex-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'latex)
    (org-element-property :value export-snippet)))


;;;; Fixed Width

(defun org-latex-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-latex--wrap-label
   fixed-width
   (format "\\begin{verbatim}\n%s\\end{verbatim}"
	   (org-remove-indentation
	    (org-element-property :value fixed-width)))))


;;;; Footnote Reference

(defun org-latex-footnote-reference (footnote-reference contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (eq (org-element-type prev) 'footnote-reference)
       org-latex-footnote-separator))
   (cond
    ;; Use \footnotemark if the footnote has already been defined.
    ((not (org-export-footnote-first-reference-p footnote-reference info))
     (format "\\footnotemark[%s]{}"
	     (org-export-get-footnote-number footnote-reference info)))
    ;; Use \footnotemark if reference is within another footnote
    ;; reference, footnote definition or table cell.
    ((loop for parent in (org-export-get-genealogy footnote-reference)
	   thereis (memq (org-element-type parent)
			 '(footnote-reference footnote-definition table-cell)))
     "\\footnotemark")
    ;; Otherwise, define it with \footnote command.
    (t
     (let ((def (org-export-get-footnote-definition footnote-reference info)))
       (concat
	(format "\\footnote{%s}" (org-trim (org-export-data def info)))
	;; Retrieve all footnote references within the footnote and
	;; add their definition after it, since LaTeX doesn't support
	;; them inside.
	(org-latex--delayed-footnotes-definitions def info)))))))


;;;; Headline

(defun org-latex-headline (headline contents info)
  "Transcode a HEADLINE element from Org to LaTeX.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((class (plist-get info :latex-class))
	   (level (org-export-get-relative-level headline info))
	   (numberedp (org-export-numbered-headline-p headline info))
	   (class-sectioning (assoc class org-latex-classes))
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
	   (full-text (funcall org-latex-format-headline-function
			       todo todo-type priority text tags))
	   ;; Associate \label to the headline for internal links.
	   (headline-label
	    (format "\\label{sec-%s}\n"
		    (mapconcat 'number-to-string
			       (org-export-get-headline-number headline info)
			       "-")))
	   (pre-blanks
	    (make-string (org-element-property :pre-blank headline) 10)))
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
		  (and full-text (org-string-match-p "\\`[ \t]*\\[" full-text)
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
	       (funcall org-latex-format-headline-function
			todo todo-type priority
			(org-export-data-with-backend
			 (org-export-get-alt-title headline info)
			 section-back-end info)
			(and (eq (plist-get info :with-tags) t) tags))))
	  (if (and numberedp opt-title
		   (not (equal opt-title full-text))
		   (string-match "\\`\\\\\\(.*?[^*]\\){" section-fmt))
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
  (todo todo-type priority text tags)
  "Default format function for a headline.
See `org-latex-format-headline-function' for details."
  (concat
   (and todo (format "{\\bfseries\\sffamily %s} " todo))
   (and priority (format "\\framebox{\\#%c} " priority))
   text
   (and tags
	(format "\\hfill{}\\textsc{%s}" (mapconcat 'identity tags ":")))))


;;;; Horizontal Rule

(defun org-latex-horizontal-rule (horizontal-rule contents info)
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
      (format "\\rule{%s}{%s}"
	      (or (plist-get attr :width) "\\linewidth")
	      (or (plist-get attr :thickness) "0.5pt"))))))


;;;; Inline Src Block

(defun org-latex-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((code (org-element-property :value inline-src-block))
	 (separator (org-latex--find-verb-separator code)))
    (cond
     ;; Do not use a special package: transcode it verbatim.
     ((not org-latex-listings)
      (concat "\\verb" separator code separator))
     ;; Use minted package.
     ((eq org-latex-listings 'minted)
      (let* ((org-lang (org-element-property :language inline-src-block))
	     (mint-lang (or (cadr (assq (intern org-lang)
					org-latex-minted-langs))
			    (downcase org-lang)))
	     (options (org-latex--make-option-string
		       org-latex-minted-options)))
	(concat (format "\\mint%s{%s}"
			(if (string= options "") "" (format "[%s]" options))
			mint-lang)
		separator code separator)))
     ;; Use listings package.
     (t
      ;; Maybe translate language's name.
      (let* ((org-lang (org-element-property :language inline-src-block))
	     (lst-lang (or (cadr (assq (intern org-lang)
				       org-latex-listings-langs))
			   org-lang))
	     (options (org-latex--make-option-string
		       (append org-latex-listings-options
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
		       (org-element-property :priority inlinetask))))
    ;; If `org-latex-format-inlinetask-function' is provided, call it
    ;; with appropriate arguments.
    (if (not (eq org-latex-format-inlinetask-function 'ignore))
	(funcall org-latex-format-inlinetask-function
		 todo todo-type priority title tags contents)
      ;; Otherwise, use a default template.
      (org-latex--wrap-label
       inlinetask
       (let ((full-title
	      (concat
	       (when todo (format "\\textbf{\\textsf{\\textsc{%s}}} " todo))
	       (when priority (format "\\framebox{\\#%c} " priority))
	       title
	       (when tags (format "\\hfill{}\\textsc{:%s:}"
				  (mapconcat 'identity tags ":"))))))
	 (format (concat "\\begin{center}\n"
			 "\\fbox{\n"
			 "\\begin{minipage}[c]{.6\\textwidth}\n"
			 "%s\n\n"
			 "\\rule[.8em]{\\textwidth}{2pt}\n\n"
			 "%s"
			 "\\end{minipage}\n"
			 "}\n"
			 "\\end{center}")
		 full-title contents))))))


;;;; Italic

(defun org-latex-italic (italic contents info)
  "Transcode ITALIC from Org to LaTeX.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (org-latex--text-markup contents 'italic))


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
		       (incf level)))
		   level)))
	    (and count
		 (< level 5)
		 (format "\\setcounter{enum%s}{%s}\n"
			 (nth (1- level) '("i" "ii" "iii" "iv"))
			 (1- count)))))
	 (checkbox (case (org-element-property :checkbox item)
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
		   (org-string-match-p "\\`[ \t]*\\[" contents)
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

(defun org-latex-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    (cond
     ((string= key "LATEX") value)
     ((string= key "INDEX") (format "\\index{%s}" value))
     ((string= key "TOC")
      (let ((value (downcase value)))
	(cond
	 ((string-match "\\<headlines\\>" value)
	  (let ((depth (or (and (string-match "[0-9]+" value)
				(string-to-number (match-string 0 value)))
			   (plist-get info :with-toc))))
	    (concat
	     (when (wholenump depth)
	       (format "\\setcounter{tocdepth}{%s}\n" depth))
	     "\\tableofcontents")))
	 ((string= "tables" value) "\\listoftables")
	 ((string= "listings" value)
	  (cond
	   ((eq org-latex-listings 'minted) "\\listoflistings")
	   (org-latex-listings "\\lstlistoflistings")
	   ;; At the moment, src blocks with a caption are wrapped
	   ;; into a figure environment.
	   (t "\\listoffigures")))))))))


;;;; Latex Environment

(defun org-latex-latex-environment (latex-environment contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (plist-get info :with-latex)
    (let ((label (org-element-property :name latex-environment))
	  (value (org-remove-indentation
		  (org-element-property :value latex-environment))))
      (if (not (org-string-nw-p label)) value
	;; Environment is labeled: label must be within the environment
	;; (otherwise, a reference pointing to that element will count
	;; the section instead).
	(with-temp-buffer
	  (insert value)
	  (goto-char (point-min))
	  (forward-line)
	  (insert
	   (format "\\label{%s}\n" (org-export-solidify-link-text label)))
	  (buffer-string))))))


;;;; Latex Fragment

(defun org-latex-latex-fragment (latex-fragment contents info)
  "Transcode a LATEX-FRAGMENT object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (plist-get info :with-latex)
    (org-element-property :value latex-fragment)))


;;;; Line Break

(defun org-latex-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "\\\\\n")


;;;; Link

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
	 ;; Retrieve latex attributes from the element around.
	 (attr (org-export-read-attribute :attr_latex parent))
	 (float (let ((float (plist-get attr :float)))
		  (cond ((and (not float) (plist-member attr :float)) nil)
			((string= float "wrap") 'wrap)
			((string= float "multicolumn") 'multicolumn)
			((or float
			     (org-element-property :caption parent)
			     (org-string-nw-p (plist-get attr :caption)))
			 'figure))))
	 (placement
	  (let ((place (plist-get attr :placement)))
	    (cond (place (format "%s" place))
		  ((eq float 'wrap) "{l}{0.5\\textwidth}")
		  ((eq float 'figure)
		   (format "[%s]" org-latex-default-figure-position))
		  (t ""))))
	 (comment-include (if (plist-get attr :comment-include) "%" ""))
	 ;; It is possible to specify width and height in the
	 ;; ATTR_LATEX line, and also via default variables.
	 (width (cond ((plist-get attr :width))
		      ((plist-get attr :height) "")
		      ((eq float 'wrap) "0.48\\textwidth")
		      (t org-latex-image-default-width)))
	 (height (cond ((plist-get attr :height))
		       ((or (plist-get attr :width)
			    (memq float '(figure wrap))) "")
		       (t org-latex-image-default-height)))
	 (options (let ((opt (or (plist-get attr :options)
				 org-latex-image-default-option)))
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
      (setq image-code
	    (format "\\includegraphics%s{%s}"
		    (cond ((not (org-string-nw-p options)) "")
			  ((= (aref options 0) ?,)
			   (format "[%s]"(substring options 1)))
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
    (case float
      (wrap (format "\\begin{wrapfigure}%s
\\centering
%s%s
%s\\end{wrapfigure}" placement comment-include image-code caption))
      (multicolumn (format "\\begin{figure*}%s
\\centering
%s%s
%s\\end{figure*}" placement comment-include image-code caption))
      (figure (format "\\begin{figure}%s
\\centering
%s%s
%s\\end{figure}" placement comment-include image-code caption))
      (otherwise image-code))))

(defun org-latex-link (link desc info)
  "Transcode a LINK object from Org to LaTeX.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((type (org-element-property :type link))
	 (raw-path (replace-regexp-in-string
		    "%" "\\%" (org-element-property :path link) nil t))
	 ;; Ensure DESC really exists, or set it to nil.
	 (desc (and (not (string= desc "")) desc))
	 (imagep (org-export-inline-image-p
		  link org-latex-inline-image-rules))
	 (path (cond
		((member type '("http" "https" "ftp" "mailto"))
		 (concat type ":" raw-path))
		((and (string= type "file") (file-name-absolute-p raw-path))
		 (concat "file:" raw-path))
		(t raw-path)))
	 protocol)
    (cond
     ;; Image file.
     (imagep (org-latex--inline-image link info))
     ;; Radio link: Transcode target's contents and use them as link's
     ;; description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
	(when destination
	  (format "\\hyperref[%s]{%s}"
		  (org-export-solidify-link-text
		   (org-element-property :value destination))
		  desc))))
     ;; Links pointing to a headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(case (org-element-type destination)
	  ;; Id link points to an external file.
	  (plain-text
	   (if desc (format "\\href{%s}{%s}" destination desc)
	     (format "\\url{%s}" destination)))
	  ;; Fuzzy link points nowhere.
	  ('nil
	   (format org-latex-link-with-unknown-path-format
		   (or desc
		       (org-export-data
			(org-element-property :raw-link link) info))))
	  ;; LINK points to a headline.  If headlines are numbered
	  ;; and the link has no description, display headline's
	  ;; number.  Otherwise, display description or headline's
	  ;; title.
	  (headline
	   (let ((label
		  (format "sec-%s"
			  (mapconcat
			   'number-to-string
			   (org-export-get-headline-number destination info)
			   "-"))))
	     (if (and (plist-get info :section-numbers) (not desc))
		 (format "\\ref{%s}" label)
	       (format "\\hyperref[%s]{%s}" label
		       (or desc
			   (org-export-data
			    (org-element-property :title destination) info))))))
          ;; Fuzzy link points to a target.  Do as above.
	  (otherwise
	   (let ((path (org-export-solidify-link-text path)))
	     (if (not desc) (format "\\ref{%s}" path)
	       (format "\\hyperref[%s]{%s}" path desc)))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (format (org-export-get-coderef-format path desc)
	      (org-export-resolve-coderef path info)))
     ;; Link type is handled by a special function.
     ((functionp (setq protocol (nth 2 (assoc type org-link-protocols))))
      (funcall protocol (org-link-unescape path) desc 'latex))
     ;; External link with a description part.
     ((and path desc) (format "\\href{%s}{%s}" path desc))
     ;; External link without a description part.
     (path (format "\\url{%s}" path))
     ;; No path, only description.  Try to do something useful.
     (t (format org-latex-link-with-unknown-path-format desc)))))


;;;; Paragraph

(defun org-latex-paragraph (paragraph contents info)
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
	     latex-type))))


;;;; Plain Text

(defun org-latex-plain-text (text info)
  "Transcode a TEXT string from Org to LaTeX.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (let ((specialp (plist-get info :with-special-strings))
	(output text))
    ;; Protect %, #, &, $, _, { and }.
    (while (string-match "\\([^\\]\\|^\\)\\([%$#&{}_]\\)" output)
      (setq output
	    (replace-match
	     (format "\\%s" (match-string 2 output)) nil t output 2)))
    ;; Protect ^.
    (setq output
	  (replace-regexp-in-string
	   "\\([^\\]\\|^\\)\\(\\^\\)" "\\\\^{}" output nil nil 2))
    ;; Protect \.  If special strings are used, be careful not to
    ;; protect "\" in "\-" constructs.
    (let ((symbols (if specialp "-%$#&{}^_\\" "%$#&{}^_\\")))
      (setq output
	    (replace-regexp-in-string
	     (format "\\(?:[^\\]\\|^\\)\\(\\\\\\)\\(?:[^%s]\\|$\\)" symbols)
	     "$\\backslash$" output nil t 1)))
    ;; Protect ~.
    (setq output
	  (replace-regexp-in-string
	   "\\([^\\]\\|^\\)\\(~\\)" "\\textasciitilde{}" output nil t 2))
    ;; Activate smart quotes.  Be sure to provide original TEXT string
    ;; since OUTPUT may have been modified.
    (when (plist-get info :with-smart-quotes)
      (setq output (org-export-activate-smart-quotes output :latex info text)))
    ;; LaTeX into \LaTeX{} and TeX into \TeX{}.
    (let ((case-fold-search nil)
	  (start 0))
      (while (string-match "\\<\\(\\(?:La\\)?TeX\\)\\>" output start)
	(setq output (replace-match
		      (format "\\%s{}" (match-string 1 output)) nil t output)
	      start (match-end 0))))
    ;; Convert special strings.
    (when specialp
      (setq output
	    (replace-regexp-in-string "\\.\\.\\." "\\ldots{}" output nil t)))
    ;; Handle break preservation if required.
    (when (plist-get info :preserve-breaks)
      (setq output (replace-regexp-in-string
		    "\\(\\\\\\\\\\)?[ \t]*\n" " \\\\\\\\\n" output)))
    ;; Return value.
    output))


;;;; Planning

(defun org-latex-planning (planning contents info)
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
		(format org-latex-inactive-timestamp-format
			(org-translate-time
			 (org-element-property :raw-value closed))))))
	   (let ((deadline (org-element-property :deadline planning)))
	     (when deadline
	       (concat
		(format "\\textbf{%s} " org-deadline-string)
		(format org-latex-active-timestamp-format
			(org-translate-time
			 (org-element-property :raw-value deadline))))))
	   (let ((scheduled (org-element-property :scheduled planning)))
	     (when scheduled
	       (concat
		(format "\\textbf{%s} " org-scheduled-string)
		(format org-latex-active-timestamp-format
			(org-translate-time
			 (org-element-property :raw-value scheduled))))))))
    " ")
   "\\\\"))


;;;; Quote Block

(defun org-latex-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-latex--wrap-label
   quote-block
   (format "\\begin{quote}\n%s\\end{quote}" contents)))


;;;; Quote Section

(defun org-latex-quote-section (quote-section contents info)
  "Transcode a QUOTE-SECTION element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-remove-indentation
		(org-element-property :value quote-section))))
    (when value (format "\\begin{verbatim}\n%s\\end{verbatim}" value))))


;;;; Radio Target

(defun org-latex-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to LaTeX.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (format "\\label{%s}%s"
	  (org-export-solidify-link-text
	   (org-element-property :value radio-target))
	  text))


;;;; Section

(defun org-latex-section (section contents info)
  "Transcode a SECTION element from Org to LaTeX.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)


;;;; Special Block

(defun org-latex-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((type (downcase (org-element-property :type special-block)))
	(opt (org-export-read-attribute :attr_latex special-block :options)))
    (concat (format "\\begin{%s}%s\n" type (or opt ""))
	    ;; Insert any label or caption within the block
	    ;; (otherwise, a reference pointing to that element will
	    ;; count the section instead).
	    (org-latex--caption/label-string special-block info)
	    contents
	    (format "\\end{%s}" type))))


;;;; Src Block

(defun org-latex-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (when (org-string-nw-p (org-element-property :value src-block))
    (let* ((lang (org-element-property :language src-block))
	   (caption (org-element-property :caption src-block))
	   (label (org-element-property :name src-block))
	   (custom-env (and lang
			    (cadr (assq (intern lang)
					org-latex-custom-lang-environments))))
	   (num-start (case (org-element-property :number-lines src-block)
			(continued (org-export-get-loc src-block info))
			(new 0)))
	   (retain-labels (org-element-property :retain-labels src-block))
	   (attributes (org-export-read-attribute :attr_latex src-block))
	   (float (plist-get attributes :float)))
      (cond
       ;; Case 1.  No source fontification.
       ((not org-latex-listings)
	(let* ((caption-str (org-latex--caption/label-string src-block info))
	       (float-env
		(cond ((and (not float) (plist-member attributes :float)) "%s")
		      ((string= "multicolumn" float)
		       (format "\\begin{figure*}[%s]\n%%s%s\n\\end{figure*}"
			       org-latex-default-figure-position
			       caption-str))
		      ((or caption float)
		       (format "\\begin{figure}[H]\n%%s%s\n\\end{figure}"
			       caption-str))
		      (t "%s"))))
	  (format
	   float-env
	   (concat (format "\\begin{verbatim}\n%s\\end{verbatim}"
			   (org-export-format-code-default src-block info))))))
       ;; Case 2.  Custom environment.
       (custom-env (format "\\begin{%s}\n%s\\end{%s}\n"
			   custom-env
			   (org-export-format-code-default src-block info)
			   custom-env))
       ;; Case 3.  Use minted package.
       ((eq org-latex-listings 'minted)
	(let* ((caption-str (org-latex--caption/label-string src-block info))
	       (float-env
		(cond ((and (not float) (plist-member attributes :float)) "%s")
		      ((string= "multicolumn" float)
		       (format "\\begin{listing*}\n%%s\n%s\\end{listing*}"
			       caption-str))
		      ((or caption float)
		       (format "\\begin{listing}[H]\n%%s\n%s\\end{listing}"
			       caption-str))
		      (t "%s")))
	       (body
		(format
		 "\\begin{minted}[%s]{%s}\n%s\\end{minted}"
		 ;; Options.
		 (org-latex--make-option-string
		  (if (or (not num-start)
			  (assoc "linenos" org-latex-minted-options))
		      org-latex-minted-options
		    (append
		     `(("linenos")
		       ("firstnumber" ,(number-to-string (1+ num-start))))
		     org-latex-minted-options)))
		 ;; Language.
		 (or (cadr (assq (intern lang) org-latex-minted-langs))
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
		    (lambda (loc num ref)
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
	       (or (cadr (assq (intern lang) org-latex-listings-langs)) lang))
	      (caption-str
	       (when caption
		 (let ((main (org-export-get-caption src-block))
		       (secondary (org-export-get-caption src-block t)))
		   (if (not secondary)
		       (format "{%s}" (org-export-data main info))
		     (format "{[%s]%s}"
			     (org-export-data secondary info)
			     (org-export-data main info)))))))
	  (concat
	   ;; Options.
	   (format
	    "\\lstset{%s}\n"
	    (org-latex--make-option-string
	     (append
	      org-latex-listings-options
	      (cond
	       ((and (not float) (plist-member attributes :float)) nil)
	       ((string= "multicolumn" float) '(("float" "*")))
	       ((and float (not (assoc "float" org-latex-listings-options)))
		`(("float" ,org-latex-default-figure-position))))
	      `(("language" ,lst-lang))
	      (if label `(("label" ,label)) '(("label" " ")))
	      (if caption-str `(("caption" ,caption-str)) '(("caption" " ")))
	      (cond ((assoc "numbers" org-latex-listings-options) nil)
		    ((not num-start) '(("numbers" "none")))
		    ((zerop num-start) '(("numbers" "left")))
		    (t `(("numbers" "left")
			 ("firstnumber"
			  ,(number-to-string (1+ num-start)))))))))
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
	       (lambda (loc num ref)
		 (concat
		  loc
		  (when ref
		    ;; Ensure references are flushed to the right,
		    ;; separated with 6 spaces from the widest line of
		    ;; code
		    (concat (make-string (+ (- max-width (length loc)) 6) ? )
			    (format "(%s)" ref)))))
	       nil (and retain-labels (cdr code-info))))))))))))


;;;; Statistics Cookie

(defun org-latex-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (replace-regexp-in-string
   "%" "\\%" (org-element-property :value statistics-cookie) nil t))


;;;; Strike-Through

(defun org-latex-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to LaTeX.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (org-latex--text-markup contents 'strike-through))


;;;; Subscript

(defun org-latex--script-size (object info)
  "Transcode a subscript or superscript object.
OBJECT is an Org object.  INFO is a plist used as a communication
channel."
  (let ((in-script-p
	 ;; Non-nil if object is already in a sub/superscript.
	 (let ((parent object))
	   (catch 'exit
	     (while (setq parent (org-export-get-parent parent))
	       (let ((type (org-element-type parent)))
		 (cond ((memq type '(subscript superscript))
			(throw 'exit t))
		       ((memq type org-element-all-elements)
			(throw 'exit nil))))))))
	(type (org-element-type object))
	(output ""))
    (org-element-map (org-element-contents object)
	(cons 'plain-text org-element-all-objects)
      (lambda (obj)
	(case (org-element-type obj)
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
    ;; Result.  Do not wrap into math mode if already in a subscript
    ;; or superscript.  Do not wrap into curly brackets if OUTPUT is
    ;; a single character.  Also merge consecutive subscript and
    ;; superscript into the same math snippet.
    (concat (and (not in-script-p)
		 (let ((prev (org-export-get-previous-element object info)))
		   (or (not prev)
		       (not (eq (org-element-type prev)
				(if (eq type 'subscript) 'superscript
				  'subscript)))
		       (let ((blank (org-element-property :post-blank prev)))
			 (and blank (> blank 0)))))
		 "$")
	    (if (eq (org-element-type object) 'subscript) "_" "^")
	    (and (> (length output) 1) "{")
	    output
	    (and (> (length output) 1) "}")
	    (and (not in-script-p)
		 (or (let ((blank (org-element-property :post-blank object)))
		       (and blank (> blank 0)))
		     (not (eq (org-element-type
			       (org-export-get-next-element object info))
			      (if (eq type 'subscript) 'superscript
				'subscript))))
		 "$"))))

(defun org-latex-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to LaTeX.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (org-latex--script-size subscript info))


;;;; Superscript

(defun org-latex-superscript (superscript contents info)
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
		    org-latex-default-table-mode)))
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

(defun org-latex--align-string (table info)
  "Return an appropriate LaTeX alignment string.
TABLE is the considered table.  INFO is a plist used as
a communication channel."
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
	      (push (case (org-export-table-cell-alignment cell info)
		      (left "l")
		      (right "r")
		      (center "c"))
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
			org-latex-default-table-environment))
	 ;; If table is a float, determine environment: table, table*
	 ;; or sidewaystable.
	 (float-env (unless (member table-env '("longtable" "longtabu"))
		      (let ((float (plist-get attr :float)))
			(cond
			 ((and (not float) (plist-member attr :float)) nil)
			 ((string= float "sidewaystable") "sidewaystable")
			 ((string= float "multicolumn") "table*")
			 ((or float
			      (org-element-property :caption table)
			      (org-string-nw-p (plist-get attr :caption)))
			  "table")))))
	 ;; Extract others display options.
	 (fontsize (let ((font (plist-get attr :font)))
		     (and font (concat font "\n"))))
	 (width (plist-get attr :width))
	 (spreadp (plist-get attr :spread))
	 (placement (or (plist-get attr :placement)
			(format "[%s]" org-latex-default-figure-position)))
	 (centerp (if (plist-member attr :center) (plist-get attr :center)
		    org-latex-tables-centered)))
    ;; Prepare the final format string for the table.
    (cond
     ;; Longtable.
     ((equal "longtable" table-env)
      (concat (and fontsize (concat "{" fontsize))
	      (format "\\begin{longtable}{%s}\n" alignment)
	      (and org-latex-table-caption-above
		   (org-string-nw-p caption)
		   (concat caption "\\\\\n"))
	      contents
	      (and (not org-latex-table-caption-above)
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
	      (and org-latex-table-caption-above
		   (org-string-nw-p caption)
		   (concat caption "\\\\\n"))
	      contents
	      (and (not org-latex-table-caption-above)
		   (org-string-nw-p caption)
		   (concat caption "\\\\\n"))
	      "\\end{longtabu}\n"
	      (and fontsize "}")))
     ;; Others.
     (t (concat (cond
		 (float-env
		  (concat (format "\\begin{%s}%s\n" float-env placement)
			  (if org-latex-table-caption-above caption "")
			  (when centerp "\\centering\n")
			  fontsize))
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
		  (concat (if org-latex-table-caption-above "" caption)
			  (format "\n\\end{%s}" float-env)))
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
	    (incf n)
	    (unless (= n 2) (setq output (replace-match "" nil nil output))))))
      (let ((centerp (if (plist-member attr :center) (plist-get attr :center)
		       org-latex-tables-centered)))
	(if (not centerp) output
	  (format "\\begin{center}\n%s\n\\end{center}" output))))))

(defun org-latex--math-table (table info)
  "Return appropriate LaTeX code for a matrix.

TABLE is the table type element to transcode.  INFO is a plist
used as a communication channel.

This function assumes TABLE has `org' as its `:type' property and
`inline-math' or `math' as its `:mode' attribute.."
  (let* ((caption (org-latex--caption/label-string table info))
	 (attr (org-export-read-attribute :attr_latex table))
	 (inlinep (equal (plist-get attr :mode) "inline-math"))
	 (env (or (plist-get attr :environment)
		  org-latex-default-table-environment))
	 (contents
	  (mapconcat
	   (lambda (row)
	     ;; Ignore horizontal rules.
	     (when (eq (org-element-property :type row) 'standard)
	       ;; Return each cell unmodified.
	       (concat
		(mapconcat
		 (lambda (cell)
		   (substring (org-element-interpret-data cell) 0 -1))
		 (org-element-map row 'table-cell 'identity info) "&")
		(or (cdr (assoc env org-latex-table-matrix-macros)) "\\\\")
		"\n")))
	   (org-element-map table 'table-row 'identity info) ""))
	 ;; Variables related to math clusters (contiguous math tables
	 ;; of the same type).
	 (mode (org-export-read-attribute :attr_latex table :mode))
	 (prev (org-export-get-previous-element table info))
	 (next (org-export-get-next-element table info))
	 (same-mode-p
	  (lambda (table)
	    ;; Non-nil when TABLE has the same mode as current table.
	    (string= (or (org-export-read-attribute :attr_latex table :mode)
			 org-latex-default-table-mode)
		     mode))))
    (concat
     ;; Opening string.  If TABLE is in the middle of a table cluster,
     ;; do not insert any.
     (cond ((and prev
		 (eq (org-element-type prev) 'table)
		 (memq (org-element-property :post-blank prev) '(0 nil))
		 (funcall same-mode-p prev))
	    nil)
	   (inlinep "\\(")
	   ((org-string-nw-p caption) (concat "\\begin{equation}\n" caption))
	   (t "\\["))
     ;; Prefix.
     (or (plist-get attr :math-prefix) "")
     ;; Environment.  Also treat special cases.
     (cond ((equal env "array")
	    (let ((align (org-latex--align-string table info)))
	      (format "\\begin{array}{%s}\n%s\\end{array}" align contents)))
	   ((assoc env org-latex-table-matrix-macros)
	    (format "\\%s%s{\n%s}"
		    env
		    (or (plist-get attr :math-arguments) "")
		    contents))
	   (t (format "\\begin{%s}\n%s\\end{%s}" env contents env)))
     ;; Suffix.
     (or (plist-get attr :math-suffix) "")
     ;; Closing string.  If TABLE is in the middle of a table cluster,
     ;; do not insert any.  If it closes such a cluster, be sure to
     ;; close the cluster with a string matching the opening string.
     (cond ((and next
		 (eq (org-element-type next) 'table)
		 (memq (org-element-property :post-blank table) '(0 nil))
		 (funcall same-mode-p next))
	    nil)
	   (inlinep "\\)")
	   ;; Find cluster beginning to know which environment to use.
	   ((let ((cluster-beg table) prev)
	      (while (and (setq prev (org-export-get-previous-element
				      cluster-beg info))
			  (memq (org-element-property :post-blank prev)
				'(0 nil))
			  (funcall same-mode-p prev))
		(setq cluster-beg prev))
	      (and (or (org-element-property :caption cluster-beg)
		       (org-element-property :name cluster-beg))
		   "\n\\end{equation}")))
	   (t "\\]")))))


;;;; Table Cell

(defun org-latex-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to LaTeX.
CONTENTS is the cell contents.  INFO is a plist used as
a communication channel."
  (concat (if (and contents
		   org-latex-table-scientific-notation
		   (string-match orgtbl-exp-regexp contents))
	      ;; Use appropriate format string for scientific
	      ;; notation.
	      (format org-latex-table-scientific-notation
		      (match-string 1 contents)
		      (match-string 2 contents))
	    contents)
	  (when (org-export-get-next-element table-cell info) " & ")))


;;;; Table Row

(defun org-latex-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to LaTeX.
CONTENTS is the contents of the row.  INFO is a plist used as
a communication channel."
  ;; Rules are ignored since table separators are deduced from
  ;; borders of the current row.
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((attr (org-export-read-attribute :attr_latex
					    (org-export-get-parent table-row)))
	   (longtablep (member (or (plist-get attr :environment)
				    org-latex-default-table-environment)
				'("longtable" "longtabu")))
	   (booktabsp (if (plist-member attr :booktabs)
			  (plist-get attr :booktabs)
			org-latex-tables-booktabs))
	   ;; TABLE-ROW's borders are extracted from its first cell.
	   (borders (org-export-table-cell-borders
		     (car (org-element-contents table-row)) info)))
      (concat
       ;; When BOOKTABS are activated enforce top-rule even when no
       ;; hline was specifically marked.
       (cond ((and booktabsp (memq 'top borders)) "\\toprule\n")
	     ((and (memq 'top borders) (memq 'above borders)) "\\hline\n"))
       contents "\\\\\n"
       (cond
	;; Special case for long tables. Define header and footers.
	((and longtablep (org-export-table-row-ends-header-p table-row info))
	 (format "%s
\\endhead
%s\\multicolumn{%d}{r}{Continued on next page} \\\\
\\endfoot
\\endlastfoot"
		 (if booktabsp "\\midrule" "\\hline")
		 (if booktabsp "\\midrule" "\\hline")
		 ;; Number of columns.
		 (cdr (org-export-table-dimensions
		       (org-export-get-parent-table table-row) info))))
	;; When BOOKTABS are activated enforce bottom rule even when
	;; no hline was specifically marked.
	((and booktabsp (memq 'bottom borders)) "\\bottomrule")
	((and (memq 'bottom borders) (memq 'below borders)) "\\hline")
	((memq 'below borders) (if booktabsp "\\midrule" "\\hline")))))))


;;;; Target

(defun org-latex-target (target contents info)
  "Transcode a TARGET object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "\\label{%s}"
	  (org-export-solidify-link-text (org-element-property :value target))))


;;;; Timestamp

(defun org-latex-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((value (org-latex-plain-text
		(org-timestamp-translate timestamp) info)))
    (case (org-element-property :type timestamp)
      ((active active-range) (format org-latex-active-timestamp-format value))
      ((inactive inactive-range)
       (format org-latex-inactive-timestamp-format value))
      (otherwise (format org-latex-diary-timestamp-format value)))))


;;;; Underline

(defun org-latex-underline (underline contents info)
  "Transcode UNDERLINE from Org to LaTeX.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (org-latex--text-markup contents 'underline))


;;;; Verbatim

(defun org-latex-verbatim (verbatim contents info)
  "Transcode a VERBATIM object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-latex--text-markup (org-element-property :value verbatim) 'verbatim))


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
   (progn
     (setq contents (replace-regexp-in-string
		     "^ *\\\\\\\\$" "\\\\vspace*{1em}"
		     (replace-regexp-in-string
		      "\\(\\\\\\\\\\)?[ \t]*\n" " \\\\\\\\\n" contents)))
     (while (string-match "^[ \t]+" contents)
       (let ((new-str (format "\\hspace*{%dem}"
			      (length (match-string 0 contents)))))
	 (setq contents (replace-match new-str nil t contents))))
     (format "\\begin{verse}\n%s\\end{verse}" contents))))



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
  "Assume the current region has org-mode syntax, and convert it to LaTeX.
This can be used in any buffer.  For example, you can write an
itemized list in org-mode syntax in an LaTeX buffer and use this
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
done through the command specified in `org-latex-pdf-process'.

When optional argument SNIPPET is non-nil, TEXFILE is a temporary
file used to preview a LaTeX snippet.  In this case, do not
create a log buffer and do not bother removing log files.

Return PDF file name or an error if it couldn't be produced."
  (let* ((base-name (file-name-sans-extension (file-name-nondirectory texfile)))
	 (full-name (file-truename texfile))
	 (out-dir (file-name-directory texfile))
	 ;; Properly set working directory for compilation.
	 (default-directory (if (file-name-absolute-p texfile)
				(file-name-directory full-name)
			      default-directory))
	 errors)
    (unless snippet (message (format "Processing LaTeX file %s..." texfile)))
    (save-window-excursion
      (cond
       ;; A function is provided: Apply it.
       ((functionp org-latex-pdf-process)
	(funcall org-latex-pdf-process (shell-quote-argument texfile)))
       ;; A list is provided: Replace %b, %f and %o with appropriate
       ;; values in each command before applying it.  Output is
       ;; redirected to "*Org PDF LaTeX Output*" buffer.
       ((consp org-latex-pdf-process)
	(let ((outbuf (and (not snippet)
			   (get-buffer-create "*Org PDF LaTeX Output*"))))
	  (mapc
	   (lambda (command)
	     (shell-command
	      (replace-regexp-in-string
	       "%b" (shell-quote-argument base-name)
	       (replace-regexp-in-string
		"%f" (shell-quote-argument full-name)
		(replace-regexp-in-string
		 "%o" (shell-quote-argument out-dir) command t t) t t) t t)
	      outbuf))
	   org-latex-pdf-process)
	  ;; Collect standard errors from output buffer.
	  (setq errors (and (not snippet) (org-latex--collect-errors outbuf)))))
       (t (error "No valid command to process to PDF")))
      (let ((pdffile (concat out-dir base-name ".pdf")))
	;; Check for process failure.  Provide collected errors if
	;; possible.
	(if (not (file-exists-p pdffile))
	    (error (concat (format "PDF file %s wasn't produced" pdffile)
			   (when errors (concat ": " errors))))
	  ;; Else remove log files, when specified, and signal end of
	  ;; process to user, along with any error encountered.
	  (when (and (not snippet) org-latex-remove-logfiles)
	    (dolist (file (directory-files
			   out-dir t
			   (concat (regexp-quote base-name)
				   "\\(?:\\.[0-9]+\\)?"
				   "\\."
				   (regexp-opt org-latex-logfiles-extensions))))
	      (delete-file file)))
	  (message (concat "Process completed"
			   (if (not errors) "."
			     (concat " with errors: " errors)))))
	;; Return output file name.
	pdffile))))

(defun org-latex--collect-errors (buffer)
  "Collect some kind of errors from \"pdflatex\" command output.

BUFFER is the buffer containing output.

Return collected error types as a string, or nil if there was
none."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward "^[ \t]*This is .*?TeX.*?Version" nil t)
	(let ((case-fold-search t)
	      (errors ""))
	  (dolist (latex-error org-latex-known-errors)
	    (when (save-excursion (re-search-forward (car latex-error) nil t))
	      (setq errors (concat errors " " (cdr latex-error)))))
	  (and (org-string-nw-p errors) (org-trim errors)))))))

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
   (org-latex-compile
    (org-publish-org-to
     'latex filename ".tex" plist (file-name-directory filename)))
   pub-dir))


(provide 'ox-latex)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-latex.el ends here
