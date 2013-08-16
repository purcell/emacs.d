;;; org-xhtml.el --- XHTML export for Org-mode (uses org-lparse)

;; Copyright (C) 2004-2012 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;;; Code:

(require 'org-exp)
(require 'org-html) 			; FIXME; remove during merge
(require 'format-spec)
(require 'org-lparse)
(eval-when-compile (require 'cl) (require 'table) (require 'browse-url))

(declare-function org-id-find-id-file "org-id" (id))
(declare-function htmlize-region "ext:htmlize" (beg end))
(declare-function org-pop-to-buffer-same-window
		  "org-compat" (&optional buffer-or-name norecord label))

(defgroup org-export-xhtml nil
  "Options specific for HTML export of Org-mode files."
  :tag "Org Export HTML"
  :group 'org-export)

(defconst org-export-xhtml-special-string-regexps
  '(("\\\\-" . "&shy;")
    ("---\\([^-]\\)" . "&mdash;\\1")
    ("--\\([^-]\\)" . "&ndash;\\1")
    ("\\.\\.\\." . "&hellip;"))
  "Regular expressions for special string conversion.")

(defcustom org-export-xhtml-footnotes-section "<div id=\"footnotes\">
<h2 class=\"footnotes\">%s: </h2>
<div id=\"text-footnotes\">
%s
</div>
</div>"
  "Format for the footnotes section.
Should contain a two instances of %s.  The first will be replaced with the
language-specific word for \"Footnotes\", the second one will be replaced
by the footnotes themselves."
  :group 'org-export-xhtml
  :type 'string)

(defcustom org-export-xhtml-footnote-format "<sup>%s</sup>"
  "The format for the footnote reference.
%s will be replaced by the footnote reference itself."
  :group 'org-export-xhtml
  :type 'string)


(defcustom org-export-xhtml-footnote-separator "<sup>, </sup>"
  "Text used to separate footnotes."
  :group 'org-export-xhtml
  :type 'string)

(defcustom org-export-xhtml-coding-system nil
  "Coding system for HTML export, defaults to `buffer-file-coding-system'."
  :group 'org-export-xhtml
  :type 'coding-system)

(defcustom org-export-xhtml-extension "html"
  "The extension for exported HTML files."
  :group 'org-export-xhtml
  :type 'string)

(defcustom org-export-xhtml-xml-declaration
  '(("html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
    ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))
  "The extension for exported HTML files.
%s will be replaced with the charset of the exported file.
This may be a string, or an alist with export extensions
and corresponding declarations."
  :group 'org-export-xhtml
  :type '(choice
	  (string :tag "Single declaration")
	  (repeat :tag "Dependent on extension"
		  (cons (string :tag "Extension")
			(string :tag "Declaration")))))

(defcustom org-export-xhtml-style-include-scripts t
  "Non-nil means include the JavaScript snippets in exported HTML files.
The actual script is defined in `org-export-xhtml-scripts' and should
not be modified."
  :group 'org-export-xhtml
  :type 'boolean)

(defconst org-export-xhtml-scripts
"<script type=\"text/javascript\">
<!--/*--><![CDATA[/*><!--*/
 function CodeHighlightOn(elem, id)
 {
   var target = document.getElementById(id);
   if(null != target) {
     elem.cacheClassElem = elem.className;
     elem.cacheClassTarget = target.className;
     target.className = \"code-highlighted\";
     elem.className   = \"code-highlighted\";
   }
 }
 function CodeHighlightOff(elem, id)
 {
   var target = document.getElementById(id);
   if(elem.cacheClassElem)
     elem.className = elem.cacheClassElem;
   if(elem.cacheClassTarget)
     target.className = elem.cacheClassTarget;
 }
/*]]>*///-->
</script>"
"Basic JavaScript that is needed by HTML files produced by Org-mode.")

(defconst org-export-xhtml-style-default
"<style type=\"text/css\">
 <!--/*--><![CDATA[/*><!--*/
  html { font-family: Times, serif; font-size: 12pt; }
  .title  { text-align: center; }
  .todo   { color: red; }
  .done   { color: green; }
  .tag    { background-color: #add8e6; font-weight:normal }
  .target { }
  .timestamp { color: #bebebe; }
  .timestamp-kwd { color: #5f9ea0; }
  .right  {margin-left:auto; margin-right:0px;  text-align:right;}
  .left   {margin-left:0px;  margin-right:auto; text-align:left;}
  .center {margin-left:auto; margin-right:auto; text-align:center;}
  p.verse { margin-left: 3% }
  pre {
	border: 1pt solid #AEBDCC;
	background-color: #F3F5F7;
	padding: 5pt;
	font-family: courier, monospace;
        font-size: 90%;
        overflow:auto;
  }
  table { border-collapse: collapse; }
  td, th { vertical-align: top;  }
  th.right  { text-align:center;  }
  th.left   { text-align:center;   }
  th.center { text-align:center; }
  td.right  { text-align:right;  }
  td.left   { text-align:left;   }
  td.center { text-align:center; }
  dt { font-weight: bold; }
  div.figure { padding: 0.5em; }
  div.figure p { text-align: center; }
  div.inlinetask {
    padding:10px;
    border:2px solid gray;
    margin:10px;
    background: #ffffcc;
  }
  textarea { overflow-x: auto; }
  .linenr { font-size:smaller }
  .code-highlighted {background-color:#ffff00;}
  .org-info-js_info-navigation { border-style:none; }
  #org-info-js_console-label { font-size:10px; font-weight:bold;
                               white-space:nowrap; }
  .org-info-js_search-highlight {background-color:#ffff00; color:#000000;
                                 font-weight:bold; }
  /*]]>*/-->
</style>"
  "The default style specification for exported HTML files.
Please use the variables `org-export-xhtml-style' and
`org-export-xhtml-style-extra' to add to this style.  If you wish to not
have the default style included, customize the variable
`org-export-xhtml-style-include-default'.")

(defcustom org-export-xhtml-style-include-default t
  "Non-nil means include the default style in exported HTML files.
The actual style is defined in `org-export-xhtml-style-default' and should
not be modified.  Use the variables `org-export-xhtml-style' to add
your own style information."
  :group 'org-export-xhtml
  :type 'boolean)
;;;###autoload
(put 'org-export-xhtml-style-include-default 'safe-local-variable 'booleanp)

(defcustom org-export-xhtml-style ""
  "Org-wide style definitions for exported HTML files.

This variable needs to contain the full HTML structure to provide a style,
including the surrounding HTML tags.  If you set the value of this variable,
you should consider to include definitions for the following classes:
 title, todo, done, timestamp, timestamp-kwd, tag, target.

For example, a valid value would be:

   <style type=\"text/css\">
    <![CDATA[
       p { font-weight: normal; color: gray; }
       h1 { color: black; }
      .title { text-align: center; }
      .todo, .timestamp-kwd { color: red; }
      .done { color: green; }
    ]]>
   </style>

If you'd like to refer to an external style file, use something like

   <link rel=\"stylesheet\" type=\"text/css\" href=\"mystyles.css\">

As the value of this option simply gets inserted into the HTML <head> header,
you can \"misuse\" it to add arbitrary text to the header.
See also the variable `org-export-xhtml-style-extra'."
  :group 'org-export-xhtml
  :type 'string)
;;;###autoload
(put 'org-export-xhtml-style 'safe-local-variable 'stringp)

(defcustom org-export-xhtml-style-extra ""
  "Additional style information for HTML export.
The value of this variable is inserted into the HTML buffer right after
the value of `org-export-xhtml-style'.  Use this variable for per-file
settings of style information, and do not forget to surround the style
settings with <style>...</style> tags."
  :group 'org-export-xhtml
  :type 'string)
;;;###autoload
(put 'org-export-xhtml-style-extra 'safe-local-variable 'stringp)

(defcustom org-export-xhtml-mathjax-options
  '((path  "http://orgmode.org/mathjax/MathJax.js")
    (scale "100")
    (align "center")
    (indent "2em")
    (mathml nil))
  "Options for MathJax setup.

path        The path where to find MathJax
scale       Scaling for the HTML-CSS backend, usually between 100 and 133
align       How to align display math: left, center, or right
indent      If align is not center, how far from the left/right side?
mathml      Should a MathML player be used if available?
            This is faster and reduces bandwidth use, but currently
            sometimes has lower spacing quality.  Therefore, the default is
            nil.  When browsers get better, this switch can be flipped.

You can also customize this for each buffer, using something like

#+MATHJAX: scale:\"133\" align:\"right\" mathml:t path:\"/MathJax/\""
  :group 'org-export-xhtml
  :type '(list :greedy t
	      (list :tag "path   (the path from where to load MathJax.js)"
		    (const :format "       " path) (string))
	      (list :tag "scale  (scaling for the displayed math)"
		    (const :format "       " scale) (string))
	      (list :tag "align  (alignment of displayed equations)"
		    (const :format "       " align) (string))
	      (list :tag "indent (indentation with left or right alignment)"
		    (const :format "       " indent) (string))
	      (list :tag "mathml (should MathML display be used is possible)"
		    (const :format "       " mathml) (boolean))))

(defun org-export-xhtml-mathjax-config (template options in-buffer)
  "Insert the user setup into the matchjax template."
  (let (name val (yes "   ") (no "// ") x)
    (mapc
     (lambda (e)
       (setq name (car e) val (nth 1 e))
       (if (string-match (concat "\\<" (symbol-name name) ":") in-buffer)
	   (setq val (car (read-from-string
			   (substring in-buffer (match-end 0))))))
       (if (not (stringp val)) (setq val (format "%s" val)))
       (if (string-match (concat "%" (upcase (symbol-name name))) template)
	   (setq template (replace-match val t t template))))
     options)
    (setq val (nth 1 (assq 'mathml options)))
    (if (string-match (concat "\\<mathml:") in-buffer)
	(setq val (car (read-from-string
			(substring in-buffer (match-end 0))))))
    ;; Exchange prefixes depending on mathml setting
    (if (not val) (setq x yes yes no no x))
    ;; Replace cookies to turn on or off the config/jax lines
    (if (string-match ":MMLYES:" template)
	(setq template (replace-match yes t t template)))
    (if (string-match ":MMLNO:" template)
	(setq template (replace-match no t t template)))
    ;; Return the modified template
    template))

(defcustom org-export-xhtml-mathjax-template
  "<script type=\"text/javascript\" src=\"%PATH\">
<!--/*--><![CDATA[/*><!--*/
    MathJax.Hub.Config({
        // Only one of the two following lines, depending on user settings
        // First allows browser-native MathML display, second forces HTML/CSS
        :MMLYES: config: [\"MMLorHTML.js\"], jax: [\"input/TeX\"],
        :MMLNO: jax: [\"input/TeX\", \"output/HTML-CSS\"],
        extensions: [\"tex2jax.js\",\"TeX/AMSmath.js\",\"TeX/AMSsymbols.js\",
                     \"TeX/noUndefined.js\"],
        tex2jax: {
            inlineMath: [ [\"\\\\(\",\"\\\\)\"] ],
            displayMath: [ ['$$','$$'], [\"\\\\[\",\"\\\\]\"], [\"\\\\begin{displaymath}\",\"\\\\end{displaymath}\"] ],
            skipTags: [\"script\",\"noscript\",\"style\",\"textarea\",\"pre\",\"code\"],
            ignoreClass: \"tex2jax_ignore\",
            processEscapes: false,
            processEnvironments: true,
            preview: \"TeX\"
        },
        showProcessingMessages: true,
        displayAlign: \"%ALIGN\",
        displayIndent: \"%INDENT\",

        \"HTML-CSS\": {
             scale: %SCALE,
             availableFonts: [\"STIX\",\"TeX\"],
             preferredFont: \"TeX\",
             webFont: \"TeX\",
             imageFont: \"TeX\",
             showMathMenu: true,
        },
        MMLorHTML: {
             prefer: {
                 MSIE:    \"MML\",
                 Firefox: \"MML\",
                 Opera:   \"HTML\",
                 other:   \"HTML\"
             }
        }
    });
/*]]>*///-->
</script>"
  "The MathJax setup for XHTML files."
  :group 'org-export-xhtml
  :type 'string)

(defcustom org-export-xhtml-tag-class-prefix ""
  "Prefix to class names for TODO keywords.
Each tag gets a class given by the tag itself, with this prefix.
The default prefix is empty because it is nice to just use the keyword
as a class name.  But if you get into conflicts with other, existing
CSS classes, then this prefix can be very useful."
  :group 'org-export-xhtml
  :type 'string)

(defcustom org-export-xhtml-todo-kwd-class-prefix ""
  "Prefix to class names for TODO keywords.
Each TODO keyword gets a class given by the keyword itself, with this prefix.
The default prefix is empty because it is nice to just use the keyword
as a class name.  But if you get into conflicts with other, existing
CSS classes, then this prefix can be very useful."
  :group 'org-export-xhtml
  :type 'string)

(defcustom org-export-xhtml-preamble t
  "Non-nil means insert a preamble in HTML export.

When `t', insert a string as defined by one of the formatting
strings in `org-export-xhtml-preamble-format'.  When set to a
string, this string overrides `org-export-xhtml-preamble-format'.
When set to a function, apply this function and insert the
returned string.  The function takes the property list of export
options as its only argument.

Setting :html-preamble in publishing projects will take
precedence over this variable."
  :group 'org-export-xhtml
  :type '(choice (const :tag "No preamble" nil)
		 (const :tag "Default preamble" t)
		 (string :tag "Custom formatting string")
		 (function :tag "Function (must return a string)")))

(defcustom org-export-xhtml-preamble-format '(("en" ""))
  "The format for the HTML preamble.

%t stands for the title.
%a stands for the author's name.
%e stands for the author's email.
%d stands for the date.

If you need to use a \"%\" character, you need to escape it
like that: \"%%\"."
  :group 'org-export-xhtml
  :type 'string)

(defcustom org-export-xhtml-postamble 'auto
  "Non-nil means insert a postamble in HTML export.

When `t', insert a string as defined by the formatting string in
`org-export-xhtml-postamble-format'.  When set to a string, this
string overrides `org-export-xhtml-postamble-format'.  When set to
'auto, discard `org-export-xhtml-postamble-format' and honor
`org-export-author/email/creator-info' variables.  When set to a
function, apply this function and insert the returned string.
The function takes the property list of export options as its
only argument.

Setting :html-postamble in publishing projects will take
precedence over this variable."
  :group 'org-export-xhtml
  :type '(choice (const :tag "No postamble" nil)
		 (const :tag "Auto preamble" 'auto)
		 (const :tag "Default formatting string" t)
		 (string :tag "Custom formatting string")
		 (function :tag "Function (must return a string)")))

(defcustom org-export-xhtml-postamble-format
  '(("en" "<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Date: %d</p>
<p class=\"creator\">Generated by %c</p>
<p class=\"xhtml-validation\">%v</p>
"))
  "The format for the HTML postamble.

%a stands for the author's name.
%e stands for the author's email.
%d stands for the date.
%c will be replaced by information about Org/Emacs versions.
%v will be replaced by `org-export-xhtml-validation-link'.

If you need to use a \"%\" character, you need to escape it
like that: \"%%\"."
  :group 'org-export-xhtml
  :type 'string)

(defcustom org-export-xhtml-home/up-format
  "<div id=\"org-div-home-and-up\" style=\"text-align:right;font-size:70%%;white-space:nowrap;\">
 <a accesskey=\"h\" href=\"%s\"> UP </a>
 |
 <a accesskey=\"H\" href=\"%s\"> HOME </a>
</div>"
  "Snippet used to insert the HOME and UP links.
This is a format string, the first %s will receive the UP link,
the second the HOME link.  If both `org-export-xhtml-link-up' and
`org-export-xhtml-link-home' are empty, the entire snippet will be
ignored."
  :group 'org-export-xhtml
  :type 'string)

(defcustom org-export-xhtml-toplevel-hlevel 2
  "The <H> level for level 1 headings in HTML export.
This is also important for the classes that will be wrapped around headlines
and outline structure.  If this variable is 1, the top-level headlines will
be <h1>, and the corresponding classes will be outline-1, section-number-1,
and outline-text-1.  If this is 2, all of these will get a 2 instead.
The default for this variable is 2, because we use <h1> for formatting the
document title."
  :group 'org-export-xhtml
  :type 'string)

(defcustom org-export-xhtml-link-org-files-as-html t
  "Non-nil means make file links to `file.org' point to `file.html'.
When org-mode is exporting an org-mode file to HTML, links to
non-html files are directly put into a href tag in HTML.
However, links to other Org-mode files (recognized by the
extension `.org.) should become links to the corresponding html
file, assuming that the linked org-mode file will also be
converted to HTML.
When nil, the links still point to the plain `.org' file."
  :group 'org-export-xhtml
  :type 'boolean)

(defcustom org-export-xhtml-inline-images 'maybe
  "Non-nil means inline images into exported HTML pages.
This is done using an <img> tag.  When nil, an anchor with href is used to
link to the image.  If this option is `maybe', then images in links with
an empty description will be inlined, while images with a description will
be linked only."
  :group 'org-export-xhtml
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Always" t)
		 (const :tag "When there is no description" maybe)))

(defcustom org-export-xhtml-inline-image-extensions
  '("png" "jpeg" "jpg" "gif" "svg")
  "Extensions of image files that can be inlined into HTML."
  :group 'org-export-xhtml
  :type '(repeat (string :tag "Extension")))

(defcustom org-export-xhtml-table-tag
  "<table border=\"2\" cellspacing=\"0\" cellpadding=\"6\" rules=\"groups\" frame=\"hsides\">"
  "The HTML tag that is used to start a table.
This must be a <table> tag, but you may change the options like
borders and spacing."
  :group 'org-export-xhtml
  :type 'string)

(defcustom org-export-table-header-tags '("<th scope=\"%s\"%s>" . "</th>")
  "The opening tag for table header fields.
This is customizable so that alignment options can be specified.
The first %s will be filled with the scope of the field, either row or col.
The second %s will be replaced by a style entry to align the field.
See also the variable `org-export-xhtml-table-use-header-tags-for-first-column'.
See also the variable `org-export-xhtml-table-align-individual-fields'."
  :group 'org-export-tables
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom org-export-table-data-tags '("<td%s>" . "</td>")
  "The opening tag for table data fields.
This is customizable so that alignment options can be specified.
The first %s will be filled with the scope of the field, either row or col.
The second %s will be replaced by a style entry to align the field.
See also the variable `org-export-xhtml-table-align-individual-fields'."
  :group 'org-export-tables
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom org-export-table-row-tags '("<tr>" . "</tr>")
  "The opening tag for table data fields.
This is customizable so that alignment options can be specified.
Instead of strings, these can be Lisp forms that will be evaluated
for each row in order to construct the table row tags.  During evaluation,
the variable `head' will be true when this is a header line, nil when this
is a body line.  And the variable `nline' will contain the line number,
starting from 1 in the first header line.  For example

  (setq org-export-table-row-tags
        (cons '(if head
                   \"<tr>\"
                 (if (= (mod nline 2) 1)
                     \"<tr class=\\\"tr-odd\\\">\"
                   \"<tr class=\\\"tr-even\\\">\"))
              \"</tr>\"))

will give even lines the class \"tr-even\" and odd lines the class \"tr-odd\"."
  :group 'org-export-tables
  :type '(cons
	  (choice :tag "Opening tag"
		  (string :tag "Specify")
		  (sexp))
	  (choice :tag "Closing tag"
		  (string :tag "Specify")
		  (sexp))))

(defcustom org-export-xhtml-table-align-individual-fields t
  "Non-nil means attach style attributes for alignment to each table field.
When nil, alignment will only be specified in the column tags, but this
is ignored by some browsers (like Firefox, Safari).  Opera does it right
though."
  :group 'org-export-tables
  :type 'boolean)

(defcustom org-export-xhtml-table-use-header-tags-for-first-column nil
  "Non-nil means format column one in tables with header tags.
When nil, also column one will use data tags."
  :group 'org-export-tables
  :type 'boolean)

(defcustom org-export-xhtml-validation-link
  "<a href=\"http://validator.w3.org/check?uri=referer\">Validate XHTML 1.0</a>"
  "Link to HTML validation service."
  :group 'org-export-xhtml
  :type 'string)

;; FIXME Obsolete since Org 7.7
;; Use the :timestamp option or `org-export-time-stamp-file' instead
(defvar org-export-xhtml-with-timestamp nil
  "If non-nil, write container for HTML-helper-mode timestamp.")

;; FIXME Obsolete since Org 7.7
(defvar org-export-xhtml-html-helper-timestamp
  "\n<p><br/><br/>\n<!-- hhmts start --> <!-- hhmts end --></p>\n"
  "The HTML tag used as timestamp delimiter for HTML-helper-mode.")

(defcustom org-export-xhtml-protect-char-alist
  '(("&" . "&amp;")
    ("<" . "&lt;")
    (">" . "&gt;"))
  "Alist of characters to be converted by `org-html-protect'."
  :group 'org-export-xhtml
  :type '(repeat (cons (string :tag "Character")
		       (string :tag "HTML equivalent"))))

(defgroup org-export-xhtmlize nil
  "Options for processing examples with htmlize.el."
  :tag "Org Export Htmlize"
  :group 'org-export-xhtml)

(defcustom org-export-xhtmlize-output-type 'inline-css
  "Output type to be used by htmlize when formatting code snippets.
Choices are `css', to export the CSS selectors only, or `inline-css', to
export the CSS attribute values inline in the HTML.  We use as default
`inline-css', in order to make the resulting HTML self-containing.

However, this will fail when using Emacs in batch mode for export, because
then no rich font definitions are in place.  It will also not be good if
people with different Emacs setup contribute HTML files to a website,
because the fonts will represent the individual setups.  In these cases,
it is much better to let Org/Htmlize assign classes only, and to use
a style file to define the look of these classes.
To get a start for your css file, start Emacs session and make sure that
all the faces you are interested in are defined, for example by loading files
in all modes you want.  Then, use the command
\\[org-export-xhtmlize-generate-css] to extract class definitions."
  :group 'org-export-xhtmlize
  :type '(choice (const css) (const inline-css)))

(defcustom org-export-xhtmlize-css-font-prefix "org-"
  "The prefix for CSS class names for htmlize font specifications."
  :group 'org-export-xhtmlize
  :type 'string)

(defcustom org-export-xhtmlized-org-css-url nil
  "URL pointing to a CSS file defining text colors for htmlized Emacs buffers.
Normally when creating an htmlized version of an Org buffer, htmlize will
create CSS to define the font colors.  However, this does not work when
converting in batch mode, and it also can look bad if different people
with different fontification setup work on the same website.
When this variable is non-nil, creating an htmlized version of an Org buffer
using `org-export-as-org' will remove the internal CSS section and replace it
with a link to this URL."
  :group 'org-export-xhtmlize
  :type '(choice
	  (const :tag "Keep internal css" nil)
	  (string :tag "URL or local href")))

;; FIXME: The following variable is obsolete since Org 7.7 but is
;; still declared and checked within code for compatibility reasons.
;; Use the custom variables `org-export-xhtml-divs' instead.
(defvar org-export-xhtml-content-div "content"
  "The name of the container DIV that holds all the page contents.

This variable is obsolete since Org version 7.7.
Please set `org-export-xhtml-divs' instead.")

(defcustom org-export-xhtml-divs '("preamble" "content" "postamble")
  "The name of the main divs for HTML export.
This is a list of three strings, the first one for the preamble
DIV, the second one for the content DIV and the third one for the
postamble DIV."
  :group 'org-export-xhtml
  :type '(list
	  (string :tag " Div for the preamble:")
	  (string :tag "  Div for the content:")
	  (string :tag "Div for the postamble:")))

;;; Hooks

(defvar org-export-xhtml-after-blockquotes-hook nil
  "Hook run during HTML export, after blockquote, verse, center are done.")

(defvar org-export-xhtml-final-hook nil
  "Hook run at the end of HTML export, in the new buffer.")

(defun org-export-xhtml-preprocess-latex-fragments ()
  (when (equal org-lparse-backend 'xhtml)
    (org-export-xhtml-do-preprocess-latex-fragments)))

(defvar org-lparse-opt-plist)		    ; bound during org-do-lparse
(defun org-export-xhtml-do-preprocess-latex-fragments ()
  "Convert LaTeX fragments to images."
  (let* ((latex-frag-opt (plist-get org-lparse-opt-plist :LaTeX-fragments))
	 (latex-frag-opt-1		; massage the options
	  (cond
	   ((eq latex-frag-opt 'verbatim) 'verbatim)
	   ((eq latex-frag-opt 'mathjax ) 'mathjax)
	   ((eq latex-frag-opt t        ) 'mathjax)
	   ((eq latex-frag-opt 'dvipng  ) 'dvipng)
	   (t nil))))
    (when (and org-current-export-file latex-frag-opt)
      (org-format-latex
       (concat "ltxpng/" (file-name-sans-extension
			  (file-name-nondirectory
			   org-current-export-file)))
       org-current-export-dir nil "Creating LaTeX image %s"
       nil nil latex-frag-opt-1))))

(defun org-export-xhtml-preprocess-label-references ()
  (goto-char (point-min))
  (let (label l1)
    (while (re-search-forward "\\\\ref{\\([^{}\n]+\\)}" nil t)
      (org-if-unprotected-at (match-beginning 1)
	(setq label (match-string 1))
	(save-match-data
	  (if (string-match "\\`[a-z]\\{1,10\\}:\\(.+\\)" label)
	      (setq l1 (substring label (match-beginning 1)))
	    (setq l1 label)))
	(replace-match (format "[[#%s][%s]]" label l1) t t)))))

(defun org-export-xhtml-preprocess (parameters)
  (org-export-xhtml-preprocess-label-references))

;; Process latex fragments as part of
;; `org-export-preprocess-after-blockquote-hook'. Note that this hook
;; is the one that is closest and well before the call to
;; `org-export-attach-captions-and-attributes' in
;; `org-export-preprocess-stirng'.  The above arrangement permits
;; captions, labels and attributes to be attached to png images
;; generated out of latex equations.
(add-hook 'org-export-preprocess-after-blockquote-hook
	  'org-export-xhtml-preprocess-latex-fragments)

(defvar html-table-tag nil) ; dynamically scoped into this.


;; FIXME: it already exists in org-html.el
(defconst org-html-cvt-link-fn
   nil
   "Function to convert link URLs to exportable URLs.
Takes two arguments, TYPE and PATH.
Returns exportable url as (TYPE PATH), or nil to signal that it
didn't handle this case.
Intended to be locally bound around a call to `org-export-as-html'." )


;; FIXME: it already exists in org-html.el
(defun org-html-cvt-org-as-html (opt-plist type path)
   "Convert an org filename to an equivalent html filename.
If TYPE is not file, just return `nil'.
See variable `org-export-xhtml-link-org-files-as-html'"

   (save-match-data
      (and
	 org-export-xhtml-link-org-files-as-html
	 (string= type "file")
	 (string-match "\\.org$" path)
	 (progn
	    (list
	       "file"
	       (concat
		  (substring path 0 (match-beginning 0))
		  "."
		  (plist-get opt-plist :html-extension)))))))

(defun org-xhtml-format-org-link (opt-plist type-1 path fragment desc attr
					    descp)
  "Make an HTML link.
OPT-PLIST is an options list.
TYPE is the device-type of the link (THIS://foo.html).
PATH is the path of the link (http://THIS#location).
FRAGMENT is the fragment part of the link, if any (foo.html#THIS).
DESC is the link description, if any.
ATTR is a string of other attributes of the \"a\" element."
  (declare (special org-lparse-par-open))
  (save-match-data
    (when (string= type-1 "coderef")
      (let ((ref fragment))
	(setq desc (format (org-export-get-coderef-format ref (and descp desc))
			   (cdr (assoc ref org-export-code-refs)))
	      fragment (concat  "coderef-" ref)
	      attr (format "class=\"coderef\" onmouseover=\"CodeHighlightOn(this, '%s');\" onmouseout=\"CodeHighlightOff(this, '%s');\""
			   fragment fragment))))
    (let* ((may-inline-p
	    (and (member type-1 '("http" "https" "file"))
		 (org-lparse-should-inline-p path descp)
		 (not fragment)))
	   (type (if (equal type-1 "id") "file" type-1))
	   (filename path)
	   ;;First pass.  Just sanity stuff.
	   (components-1
	    (cond
	     ((string= type "file")
	      (list
	       type
	       ;;Substitute just if original path was absolute.
	       ;;(Otherwise path must remain relative)
	       (if (file-name-absolute-p path)
		   (concat "file://" (expand-file-name path))
		 path)))
	     ((string= type "")
	      (list nil path))
	     (t (list type path))))

	   ;;Second pass.  Components converted so they can refer
	   ;;to a remote site.
	   (components-2
	    (or
	     (and org-html-cvt-link-fn
		  (apply org-html-cvt-link-fn
			 opt-plist components-1))
	     (apply #'org-html-cvt-org-as-html
		    opt-plist components-1)
	     components-1))
	   (type    (first  components-2))
	   (thefile (second components-2)))


      ;;Third pass.  Build final link except for leading type
      ;;spec.
      (cond
       ((or
	 (not type)
	 (string= type "http")
	 (string= type "https")
	 (string= type "file")
	 (string= type "coderef"))
	(if fragment
	    (setq thefile (concat thefile "#" fragment))))

       (t))

      ;;Final URL-build, for all types.
      (setq thefile
	    (let
		((str (org-xml-format-href thefile)))
	      (if (and type (not (or (string= "file" type)
				     (string= "coderef" type))))
		  (concat type ":" str)
		str)))

      (if may-inline-p
	  (org-xhtml-format-image thefile)
	(org-lparse-format
	 'LINK (org-xml-format-desc desc) thefile attr)))))

(defun org-xhtml-format-inline-image (desc)
  ;; FIXME: alt text missing here?
  (org-xhtml-format-tags
   "<img src=\"%s\" alt=\"%s\"/>" "" desc (file-name-nondirectory desc)))

;; FIXME: the org-lparse defvar belongs to org-lparse.el
(defvar org-lparse-link-description-is-image)

(defun org-xhtml-format-image (src)
  "Create image tag with source and attributes."
  (save-match-data
    (let* ((caption (org-find-text-property-in-string 'org-caption src))
	   (attr (org-find-text-property-in-string 'org-attributes src))
	   (label (org-find-text-property-in-string 'org-label src))
	   (caption (and caption (org-xml-encode-org-text caption)))
	   (img-extras (if (string-match "^ltxpng/" src)
			   (format " alt=\"%s\""
				   (org-find-text-property-in-string
				    'org-latex-src src))
			 (if (string-match "\\<alt=" (or attr ""))
			     (concat " " attr )
			   (concat " " attr " alt=\"" src "\""))))
	   (img (format "<img src=\"%s\"%s />" src img-extras))
	   (extra (concat
		   (and label
			(format "id=\"%s\" " (org-solidify-link-text label)))
		   "class=\"figure\"")))
      (if caption
	  (with-temp-buffer
	    (with-org-lparse-preserve-paragraph-state
	     (insert
	      (org-lparse-format
	       '("<div %s>" . "\n</div>")
	       (concat
		(org-lparse-format '("\n<p>" . "</p>") img)
		(org-lparse-format '("\n<p>" . "</p>") caption))
	       extra)))
	    (buffer-string))
	img))))

(defun org-export-xhtml-get-bibliography ()
  "Find bibliography, cut it out and return it."
  (catch 'exit
    (let (beg end (cnt 1) bib)
      (save-excursion
	(goto-char (point-min))
	(when (re-search-forward "^[ \t]*<div \\(id\\|class\\)=\"bibliography\"" nil t)
	  (setq beg (match-beginning 0))
	  (while (re-search-forward "</?div\\>" nil t)
	    (setq cnt (+ cnt (if (string= (match-string 0) "<div") +1 -1)))
	    (when (= cnt 0)
	      (and (looking-at ">") (forward-char 1))
	      (setq bib (buffer-substring beg (point)))
	      (delete-region beg (point))
	    (throw 'exit bib))))
	nil))))

(defun org-xhtml-format-table (lines olines)
  (let ((org-xhtml-format-table-no-css nil))
    (org-lparse-format-table lines olines)))

;; Following variable is defined for native tables i.e., when
;; `org-lparse-table-is-styled' evals to t.
(defvar org-xhtml-format-table-no-css)
(defvar org-table-number-regexp) ; defined in org-table.el

;; FIXME: This function is called from other modules. Use xhtml suffix
;; to avoid conflict
(defun org-format-table-xhtml (lines olines &optional no-css)
  "Find out which HTML converter to use and return the HTML code.
NO-CSS is passed to the exporter."
  (let* ((org-lparse-backend 'xhtml)
	 (org-lparse-entity-control-callbacks-alist
	  (org-lparse-get 'ENTITY-CONTROL))
	 (org-lparse-entity-format-callbacks-alist
	  (org-lparse-get 'ENTITY-FORMAT))
	 (org-xhtml-format-table-no-css no-css))
    (org-lparse-format-table lines olines)))

;; FIXME: This function is called from other modules. Use xhtml suffix
;; to avoid conflict
(defun org-format-org-table-xhtml (lines &optional splice no-css)
  ;; This routine might get called outside of org-export-as-html. For
  ;; example, this could happen as part of org-table-export or as part
  ;; of org-export-as-docbook. Explicitly bind the parser callback to
  ;; the html ones for the duration of the call.
  (let* ((org-lparse-backend 'xhtml)
	 (org-lparse-entity-control-callbacks-alist
	  (org-lparse-get 'ENTITY-CONTROL))
	 (org-lparse-entity-format-callbacks-alist
	  (org-lparse-get 'ENTITY-FORMAT))
	 (org-xhtml-format-table-no-css no-css))
    (org-lparse-format-org-table lines splice)))


;; FIXME: it already exists in org-html.el
(defun org-export-splice-attributes (tag attributes)
  "Read attributes in string ATTRIBUTES, add and replace in HTML tag TAG."
  (if (not attributes)
      tag
    (let (oldatt newatt)
      (setq oldatt (org-extract-attributes-from-string tag)
	    tag (pop oldatt)
	    newatt (cdr (org-extract-attributes-from-string attributes)))
      (while newatt
	(setq oldatt (plist-put oldatt (pop newatt) (pop newatt))))
      (if (string-match ">" tag)
	  (setq tag
		(replace-match (concat (org-attributes-to-string oldatt) ">")
			       t t tag)))
      tag)))

;; FIXME: This function is called from other modules. Use xhtml suffix
;; to avoid conflict
(defun org-format-table-table-xhtml (lines)
  (let* ((org-lparse-get 'html)
	 (org-lparse-entity-control-callbacks-alist
	  (org-lparse-get 'ENTITY-CONTROL))
	 (org-lparse-entity-format-callbacks-alist
	  (org-lparse-get 'ENTITY-FORMAT)))
    (org-lparse-format-table-table lines)))


;; FIXME: it already exists in org-html.el
(defun org-export-splice-style (style extra)
  "Splice EXTRA into STYLE, just before \"</style>\"."
  (if (and (stringp extra)
	   (string-match "\\S-" extra)
	   (string-match "</style>" style))
      (concat (substring style 0 (match-beginning 0))
	      "\n" extra "\n"
	      (substring style (match-beginning 0)))
    style))

(defvar htmlize-buffer-places)  ; from htmlize.el
(defun org-export-xhtmlize-region-for-paste (beg end)
  "Convert the region to HTML, using htmlize.el.
This is much like `htmlize-region-for-paste', only that it uses
the settings define in the org-... variables."
  (let* ((htmlize-output-type org-export-xhtmlize-output-type)
	 (htmlize-css-name-prefix org-export-xhtmlize-css-font-prefix)
	 (htmlbuf (htmlize-region beg end)))
    (unwind-protect
	(with-current-buffer htmlbuf
	  (buffer-substring (plist-get htmlize-buffer-places 'content-start)
			    (plist-get htmlize-buffer-places 'content-end)))
      (kill-buffer htmlbuf))))

;;;###autoload
(defun org-export-xhtmlize-generate-css ()
  "Create the CSS for all font definitions in the current Emacs session.
Use this to create face definitions in your CSS style file that can then
be used by code snippets transformed by htmlize.
This command just produces a buffer that contains class definitions for all
faces used in the current Emacs session.  You can copy and paste the ones you
need into your CSS file.

If you then set `org-export-xhtmlize-output-type' to `css', calls to
the function `org-export-xhtmlize-region-for-paste' will produce code
that uses these same face definitions."
  (interactive)
  (require 'htmlize)
  (and (get-buffer "*html*") (kill-buffer "*html*"))
  (with-temp-buffer
    (let ((fl (face-list))
	  (htmlize-css-name-prefix "org-")
	  (htmlize-output-type 'css)
	  f i)
      (while (setq f (pop fl)
		   i (and f (face-attribute f :inherit)))
	(when (and (symbolp f) (or (not i) (not (listp i))))
	  (insert (org-add-props (copy-sequence "1") nil 'face f))))
      (htmlize-region (point-min) (point-max))))
  (org-pop-to-buffer-same-window "*html*")
  (goto-char (point-min))
  (if (re-search-forward "<style" nil t)
      (delete-region (point-min) (match-beginning 0)))
  (if (re-search-forward "</style>" nil t)
      (delete-region (1+ (match-end 0)) (point-max)))
  (beginning-of-line 1)
  (if (looking-at " +") (replace-match ""))
  (goto-char (point-min)))

(defvar body-only) ; dynamically scoped into this.

;; Following variable is let bound when `org-do-lparse' is in
;; progress. See org-lparse.el.

;; FIXME: the org-lparse defvar belongs to org-lparse.el
(defvar org-lparse-toc)
(defvar org-lparse-footnote-definitions)
(defvar org-lparse-dyn-first-heading-pos)

(defun org-xhtml-end-export ()
  ;; insert the table of contents
  (when (and org-export-with-toc (not body-only) org-lparse-toc)
    (org-xhtml-insert-toc org-lparse-toc))

  ;; remove empty paragraphs
  (goto-char (point-min))
  (while (re-search-forward "<p>[ \r\n\t]*</p>" nil t)
    (replace-match ""))

  ;; Convert whitespace place holders
  (goto-char (point-min))
  (let (beg end n)
    (while (setq beg (next-single-property-change (point) 'org-whitespace))
      (setq n (get-text-property beg 'org-whitespace)
	    end (next-single-property-change beg 'org-whitespace))
      (goto-char beg)
      (delete-region beg end)
      (insert (format "<span style=\"visibility:hidden;\">%s</span>"
		      (make-string n ?x)))))

  ;; Remove empty lines at the beginning of the file.
  (goto-char (point-min))
  (when (looking-at "\\s-+\n") (replace-match ""))

  ;; Remove display properties
  (remove-text-properties (point-min) (point-max) '(display t))

  ;; Run the hook
  (run-hooks 'org-export-xhtml-final-hook))

(defun org-xhtml-format-toc-entry (snumber todo headline tags href)
  (setq headline (concat
		  (and org-export-with-section-numbers
		       (concat snumber " "))
		  headline
		  (and tags
		    (concat
		     (org-lparse-format 'SPACES 3)
		     (org-lparse-format 'FONTIFY tags "tag")))))
  (when todo
    (setq headline (org-lparse-format 'FONTIFY headline "todo")))
  (org-lparse-format 'LINK headline (concat  "#" href)))

(defun org-xhtml-format-toc-item (toc-entry level org-last-level)
  (when (> level org-last-level)
    (let ((cnt (- level org-last-level)))
      (while (>= (setq cnt (1- cnt)) 0)
	(org-lparse-begin-list 'unordered)
	(org-lparse-begin-list-item 'unordered))))
  (when (< level org-last-level)
    (let ((cnt (- org-last-level level)))
      (while (>= (setq cnt (1- cnt)) 0)
	(org-lparse-end-list-item-1)
	(org-lparse-end-list 'unordered))))

  (org-lparse-end-list-item-1)
  (org-lparse-begin-list-item 'unordered)
  (insert toc-entry))

(defun org-xhtml-begin-toc (lang-specific-heading max-level)
  (org-lparse-insert-tag "<div id=\"table-of-contents\">")
  (insert
   (org-lparse-format 'HEADING lang-specific-heading
		     (or (org-lparse-get 'TOPLEVEL-HLEVEL) 1)))
  (org-lparse-insert-tag "<div id=\"text-table-of-contents\">")
  (org-lparse-begin-list 'unordered)
  (org-lparse-begin-list-item 'unordered))

(defun org-xhtml-end-toc ()
  (while (> org-last-level (1- org-min-level))
    (setq org-last-level (1- org-last-level))
    (org-lparse-end-list-item-1)
    (org-lparse-end-list 'unordered))
  (org-lparse-insert-tag "</div>")
  (org-lparse-insert-tag "</div>")

  ;; cleanup empty list items in toc
  (while (re-search-backward "<li>[ \r\n\t]*</li>\n?" (point-min) t)
    (replace-match "")))

;;;###autoload
(defun org-export-as-xhtml-and-open (arg)
  "Export the outline as HTML and immediately open it with a browser.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted lists."
  (interactive "P")
  (org-lparse-and-open "xhtml" "xhtml" arg))

;;;###autoload
(defun org-export-as-xhtml-batch ()
  "Call the function `org-lparse-batch'.
This function can be used in batch processing as:
emacs   --batch
        --load=$HOME/lib/emacs/org.el
        --eval \"(setq org-export-headline-levels 2)\"
        --visit=MyFile --funcall org-export-as-html-batch"
  (org-lparse-batch "xhtml"))

;;;###autoload
(defun org-export-as-xhtml-to-buffer (arg)
  "Call `org-lparse-to-buffer` with output to a temporary buffer.
No file is created.  The prefix ARG is passed through to `org-lparse-to-buffer'."
  (interactive "P")
  (org-lparse-to-buffer "xhtml" arg))

;;;###autoload
(defun org-replace-region-by-xhtml (beg end)
  "Assume the current region has org-mode syntax, and convert it to HTML.
This can be used in any buffer.  For example, you could write an
itemized list in org-mode syntax in an HTML buffer and then use this
command to convert it."
  (interactive "r")
  (org-replace-region-by "xhtml" beg end))

;;;###autoload
(defun org-export-region-as-xhtml (beg end &optional body-only buffer)
  "Convert region from BEG to END in `org-mode' buffer to HTML.
If prefix arg BODY-ONLY is set, omit file header, footer, and table of
contents, and only produce the region of converted text, useful for
cut-and-paste operations.
If BUFFER is a buffer or a string, use/create that buffer as a target
of the converted HTML.  If BUFFER is the symbol `string', return the
produced HTML as a string and leave not buffer behind.  For example,
a Lisp program could call this function in the following way:

  (setq html (org-export-region-as-html beg end t 'string))

When called interactively, the output buffer is selected, and shown
in a window.  A non-interactive call will only return the buffer."
  (interactive "r\nP")
  (org-lparse-region "xhtml" beg end body-only buffer))

;;; org-export-as-xhtml
;;;###autoload
(defun org-export-as-xhtml (arg &optional hidden ext-plist
			       to-buffer body-only pub-dir)
  "Export the outline as a pretty HTML file.
Use `org-lparse' internally to perform the actual export. This
routine merely binds the TARGET-BACKEND and NATIVE-BACKEND args
of `org-lparse' to \"html\"."
  (interactive "P")
  (org-lparse "xhtml" "xhtml" arg hidden ext-plist to-buffer body-only pub-dir))

(defvar org-xhtml-entity-control-callbacks-alist
  `((EXPORT
     . (org-xhtml-begin-export org-xhtml-end-export))
    (DOCUMENT-CONTENT
     . (org-xhtml-begin-document-content org-xhtml-end-document-content))
    (DOCUMENT-BODY
     . (org-xhtml-begin-document-body org-xhtml-end-document-body))
    (TOC
     . (org-xhtml-begin-toc org-xhtml-end-toc))
    (ENVIRONMENT
     . (org-xhtml-begin-environment org-xhtml-end-environment))
    (FOOTNOTE-DEFINITION
     . (org-xhtml-begin-footnote-definition org-xhtml-end-footnote-definition))
    (TABLE
     . (org-xhtml-begin-table org-xhtml-end-table))
    (TABLE-ROWGROUP
     . (org-xhtml-begin-table-rowgroup org-xhtml-end-table-rowgroup))
    (LIST
     . (org-xhtml-begin-list org-xhtml-end-list))
    (LIST-ITEM
     . (org-xhtml-begin-list-item org-xhtml-end-list-item))
    (OUTLINE
     . (org-xhtml-begin-outline org-xhtml-end-outline))
    (OUTLINE-TEXT
     . (org-xhtml-begin-outline-text org-xhtml-end-outline-text))
    (PARAGRAPH
     . (org-xhtml-begin-paragraph org-xhtml-end-paragraph)))
  "Alist of control callbacks registered with the exporter.
Each element is of the form (ENTITY . (BEGIN-ENTITY-FUNCTION
END-ENTITY-FUNCTION)).  ENTITY is one of PARAGRAPH, LIST etc as
seen above.  BEGIN-ENTITY-FUNCTION and END-ENTITY-FUNCTION are
functions that get called when the exporter needs to begin or end
an entity in the currently exported file.  The signatures of
these callbacks are specific to the ENTITY being emitted.  These
callbacks always get called with exported file as the current
buffer and need to insert the appropriate tags into the current
buffer.  For example, `org-xhtml-begin-paragraph' inserts <p> and
`org-xhtml-end-paragraph' inserts </p> in to the current buffer.
These callbacks are invoked via `org-lparse-begin' and
`org-lparse-end'.")

(defvar org-xhtml-entity-format-callbacks-alist
  `((EXTRA-TARGETS . org-lparse-format-extra-targets)
    (ORG-TAGS . org-lparse-format-org-tags)
    (SECTION-NUMBER . org-lparse-format-section-number)
    (HEADLINE . org-xhtml-format-headline)
    (TOC-ENTRY . org-xhtml-format-toc-entry)
    (TOC-ITEM . org-xhtml-format-toc-item)
    (TAGS . org-xhtml-format-tags)
    (SPACES . org-xhtml-format-spaces)
    (TABS . org-xhtml-format-tabs)
    (LINE-BREAK . org-xhtml-format-line-break)
    (FONTIFY . org-xhtml-format-fontify)
    (TODO . org-lparse-format-todo)
    (ORG-LINK . org-xhtml-format-org-link)
    (LINK . org-xhtml-format-link)
    (INLINE-IMAGE . org-xhtml-format-inline-image)
    (HEADING . org-xhtml-format-heading)
    (ANCHOR . org-xhtml-format-anchor)
    (TABLE . org-xhtml-format-table)
    (TABLE-ROW . org-xhtml-format-table-row)
    (TABLE-CELL . org-xhtml-format-table-cell)
    (FOOTNOTES-SECTION . org-xhtml-format-footnotes-section)
    (FOOTNOTE-REFERENCE . org-xhtml-format-footnote-reference)
    (HORIZONTAL-LINE . org-xhtml-format-horizontal-line)
    (LINE . org-xhtml-format-line)
    (COMMENT . org-xhtml-format-comment)
    (ORG-ENTITY . org-xhtml-format-org-entity))
  "Alist of format callbacks registered with the exporter.
Each element is of the form (ENTITY . ENTITY-FORMAT-FUNCTION).
ENTITY is one of LINE, HEADING, COMMENT, LINK, TABLE-ROW etc as
seen above.  ENTITY-FORMAT-FUNCTION is a functions that gets
called when the exporter needs to format a string in `org-mode'
buffer in a backend specific way.  The signatures of the
formatting callback is specific to the ENTITY being passed in.
These callbacks always need to encode the incoming entity in
backend specific way and return the same.  These callbacks do not
make any modifications to the exporter file.  For example,
`org-xhtml-format-table-row' encloses incoming entity in <tr>
</tr> tags and returns it.  See also `org-lparse-format'.")

;; register the xhtml exporter with org-lparse library
(org-lparse-register-backend 'xhtml)

(defun org-xhtml-unload-function ()
  (org-lparse-unregister-backend 'xhtml)
  (remove-hook 'org-export-preprocess-after-blockquote-hook
	       'org-export-xhtml-preprocess-latex-fragments)
  nil)

(defun org-xhtml-begin-document-body (opt-plist)
  (let ((link-up (and (plist-get opt-plist :link-up)
		      (string-match "\\S-" (plist-get opt-plist :link-up))
		      (plist-get opt-plist :link-up)))
	(link-home (and (plist-get opt-plist :link-home)
			(string-match "\\S-" (plist-get opt-plist :link-home))
			(plist-get opt-plist :link-home))))
    (insert "\n<body>")
    (insert  "\n"
	     (or (and (or link-up link-home)
		      (format org-export-xhtml-home/up-format
			      (or link-up link-home)
			      (or link-home link-up))) "")
	     "\n"))
  (org-xhtml-insert-preamble opt-plist)

  (org-lparse-insert-tag
   "<div id=\"%s\">" (or org-export-xhtml-content-div
			 (nth 1 org-export-xhtml-divs)))

  (org-lparse-insert-tag "\n<h1 class=\"title\"> %s </h1>\n"
			 (plist-get opt-plist :title)))

(defun org-xhtml-end-document-body (opt-plist)
  (org-xhtml-insert-postamble opt-plist)
  (unless body-only
    (insert "\n</body>")))

(defun org-xhtml-begin-document-content (opt-plist)
  (let* ((language (plist-get opt-plist :language))
	 (charset (or (and coding-system-for-write
			   (fboundp 'coding-system-get)
			   (coding-system-get coding-system-for-write
					      'mime-charset))
		      "iso-8859-1"))
	 (style (concat (if (plist-get opt-plist :style-include-default)
			    org-export-xhtml-style-default)
			(plist-get opt-plist :style)
			(plist-get opt-plist :style-extra)
			"\n"
			(if (plist-get opt-plist :style-include-scripts)
			    org-export-xhtml-scripts)))
	 (mathjax
	  (if (or (eq (plist-get opt-plist :LaTeX-fragments) 'mathjax)
		  (and org-export-have-math
		       (eq (plist-get opt-plist :LaTeX-fragments) t)))

	      (org-export-xhtml-mathjax-config
	       org-export-xhtml-mathjax-template
	       org-export-xhtml-mathjax-options
	       (or (plist-get opt-plist :mathjax) "")) "")))
    (insert (format
	     "%s
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
	       \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\">
<head>
<title>%s</title>
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=%s\"/>
<meta name=\"title\" content=\"%s\"/>
<meta name=\"generator\" content=\"Org-mode\"/>
<meta name=\"generated\" content=\"%s\"/>
<meta name=\"author\" content=\"%s\"/>
<meta name=\"description\" content=\"%s\"/>
<meta name=\"keywords\" content=\"%s\"/>
%s
%s
</head>
"
	     (format
	      (or (and (stringp org-export-xhtml-xml-declaration)
		       org-export-xhtml-xml-declaration)
		  (cdr (assoc (plist-get opt-plist :html-extension)
			      org-export-xhtml-xml-declaration))
		  (cdr (assoc "html" org-export-xhtml-xml-declaration))

		  "")
	      charset)
	     language language
	     (plist-get opt-plist :title)
	     charset
	     (plist-get opt-plist :title)
	     (plist-get opt-plist :effective-date)
	     (plist-get opt-plist :author)
	     (plist-get opt-plist :description)
	     (plist-get opt-plist :keywords)
	     style
	     mathjax))))

(defun org-xhtml-end-document-content ()
  (insert "\n</html>\n"))

(defun org-xhtml-begin-outline (level1 snumber title tags
				      target extra-targets extra-class)
  (let* ((class (format "outline-%d" level1))
	 (class (if extra-class (concat  class " " extra-class) class))
	 (id (format "outline-container-%s"
		     (org-lparse-suffix-from-snumber snumber)))
	 (extra (concat (when id (format " id=\"%s\"" id))
			(when class (format " class=\"%s\"" class)))))
    (org-lparse-insert-tag "<div%s>" extra)
    (insert
     (org-lparse-format 'HEADING
		       (org-lparse-format
			'HEADLINE title extra-targets tags snumber level1)
		       level1 target))))

(defun org-xhtml-end-outline ()
  (org-lparse-insert-tag  "</div>"))

(defun org-xhtml-begin-outline-text (level1 snumber extra-class)
  (let* ((class (format "outline-text-%d" level1))
	 (class (if extra-class (concat  class " " extra-class) class))
	 (id (format "text-%s" (org-lparse-suffix-from-snumber snumber)))
	 (extra (concat (when id (format " id=\"%s\"" id))
			(when class (format " class=\"%s\"" class)))))
    (org-lparse-insert-tag "<div%s>" extra)))

(defun org-xhtml-end-outline-text ()
  (org-lparse-insert-tag "</div>"))

(defun org-xhtml-begin-paragraph (&optional style)
  (let* ((class (cdr (assoc style '((footnote . "footnote")
				    (verse . nil)))))
	 (extra (if class (format " class=\"%s\"" class) "")))
    (org-lparse-insert-tag "<p%s>" extra)))

(defun org-xhtml-end-paragraph ()
  (insert "</p>"))

(defun org-xhtml-format-environment (style beg-end)
  (assert (memq style '(blockquote center verse fixedwidth quote native)) t)
  (case style
    (blockquote
     (case beg-end
       (BEGIN
	(org-lparse-end-paragraph)
	(insert "<blockquote>\n")
	(org-lparse-begin-paragraph))
       (END
	(org-lparse-end-paragraph)
	(insert "\n</blockquote>\n")
	(org-lparse-begin-paragraph))))
    (verse
     (case beg-end
       (BEGIN
	(org-lparse-end-paragraph)
	(insert "\n<p class=\"verse\">\n")
	(setq org-lparse-par-open t))
       (END
	(insert "</p>\n")
	(setq org-lparse-par-open nil)
	(org-lparse-begin-paragraph))))
    (center
     (case beg-end
       (BEGIN
	(org-lparse-end-paragraph)
	(insert "\n<div style=\"text-align: center\">")
	(org-lparse-begin-paragraph))
       (END
	(org-lparse-end-paragraph)
	(insert "\n</div>")
	(org-lparse-begin-paragraph))))
    (fixedwidth
     (case beg-end
       (BEGIN
	(org-lparse-end-paragraph)
	(insert "<pre class=\"example\">\n"))
       (END
	(insert "</pre>\n")
	(org-lparse-begin-paragraph))))
    (quote
     (case beg-end
       (BEGIN
	(org-lparse-end-paragraph)
	(insert "<pre>"))
       (END
	(insert "</pre>\n")
	(org-lparse-begin-paragraph))))
    (native
     (case beg-end
       (BEGIN (org-lparse-end-paragraph))
       (END (org-lparse-begin-paragraph))))
    (t (error "Unknown environment %s" style))))

(defun org-xhtml-begin-environment (style env-options-plist)
  (org-xhtml-format-environment style 'BEGIN))

(defun org-xhtml-end-environment (style env-options-plist)
  (org-xhtml-format-environment style 'END))

(defun org-xhtml-begin-list (ltype)
  (setq ltype (or (org-lparse-html-list-type-to-canonical-list-type ltype)
		  ltype))
  (case ltype
    (ordered (let* ((arg1 nil)
		    (extra (if arg1 (format " start=\"%d\"" arg1) "")))
	       (org-lparse-insert-tag "<ol%s>" extra)))
    (unordered (org-lparse-insert-tag "<ul>"))
    (description (org-lparse-insert-tag "<dl>"))
    (t (error "Unknown list type: %s"  ltype))))

(defun org-xhtml-end-list (ltype)
  (setq ltype (or (org-lparse-html-list-type-to-canonical-list-type ltype)
		  ltype))

  (org-lparse-insert-tag
     (case ltype
       (ordered "</ol>")
       (unordered "</ul>")
       (description "</dl>")
       (t (error "Unknown list type: %s" ltype)))))

(defun org-xhtml-begin-list-item (ltype &optional arg headline)
  (setq ltype (or (org-lparse-html-list-type-to-canonical-list-type ltype)
		  ltype))
  (case ltype
    (ordered
     (assert (not headline) t)
     (let* ((counter arg)
	   (extra (if counter (format " value=\"%s\"" counter) "")))
       (org-lparse-insert-tag "<li%s>" extra)))
    (unordered
     (let* ((id arg)
	   (extra (if id (format " id=\"%s\"" id) "")))
       (org-lparse-insert-tag "<li%s>" extra)
       (when headline
	 (insert headline (org-lparse-format 'LINE-BREAK) "\n"))))
    (description
     (assert (not headline) t)
     (let* ((desc-tag (or arg "(no term)")))
       (insert
	(org-xhtml-format-tags '("<dt>" . "</dt>") desc-tag))
       (org-lparse-insert-tag "<dd>")))
    (t (error "Unknown list type"))))

(defun org-xhtml-end-list-item (ltype)
  (setq ltype (or (org-lparse-html-list-type-to-canonical-list-type ltype)
		  ltype))
  (case ltype
    (ordered (org-lparse-insert-tag "</li>"))
    (unordered (org-lparse-insert-tag "</li>"))
    (description (org-lparse-insert-tag "</dd>"))
    (t (error "Unknown list type"))))

;; Following variables are let bound when table emission is in
;; progress. See org-lparse.el.

;; FIXME: the org-lparse defvar belongs to org-lparse.el
(defvar org-lparse-table-begin-marker)
(defvar org-lparse-table-ncols)
(defvar org-lparse-table-rowgrp-open)
(defvar org-lparse-table-rownum)
(defvar org-lparse-table-cur-rowgrp-is-hdr)
(defvar org-lparse-table-is-styled)
(defvar org-lparse-table-rowgrp-info)
(defvar org-lparse-table-colalign-vector)
(defvar org-lparse-table-num-numeric-items-per-column)

(defun org-xhtml-begin-table-rowgroup (&optional is-header-row)
  (when org-lparse-table-rowgrp-open
    (org-lparse-end 'TABLE-ROWGROUP))
  (org-lparse-insert-tag (if is-header-row "<thead>" "<tbody>"))
  (setq org-lparse-table-rowgrp-open t)
  (setq org-lparse-table-cur-rowgrp-is-hdr is-header-row))

(defun org-xhtml-end-table-rowgroup ()
  (when org-lparse-table-rowgrp-open
    (setq org-lparse-table-rowgrp-open nil)
    (org-lparse-insert-tag
     (if org-lparse-table-cur-rowgrp-is-hdr "</thead>" "</tbody>"))))

(defun org-xhtml-begin-table (caption label attributes)
  (let ((html-table-tag
	 (org-export-splice-attributes html-table-tag attributes)))
    (when label
      (setq html-table-tag
	    (org-export-splice-attributes
	     html-table-tag
	     (format "id=\"%s\"" (org-solidify-link-text label)))))
    (org-lparse-insert-tag html-table-tag))

  ;; Since the output of HTML table formatter can also be used in
  ;; DocBook document, we want to always include the caption to make
  ;; DocBook XML file valid.
  (insert (format "<caption>%s</caption>" (or caption "")) "\n"))

(defun org-xhtml-end-table ()
  (when org-lparse-table-is-styled
    (goto-char org-lparse-table-begin-marker)
    (setq org-lparse-table-begin-marker nil)

    (let ((c -1) gr colgropen)
      (insert
       (mapconcat
	(lambda (x)
	  (incf c)
	  (setq gr (pop org-table-colgroup-info))

	  (concat
	   (if (memq gr '(:start :startend))
	       (prog1
		   (if colgropen
		       "</colgroup>\n<colgroup>"
		     "<colgroup>")
		 (setq colgropen t))
	     "")

	   (let* ((align (aref org-lparse-table-colalign-vector c))
		  (alignspec (if (and (boundp 'org-xhtml-format-table-no-css)
				      org-xhtml-format-table-no-css)
				 " align=\"%s\"" " class=\"%s\""))
		  (extra (format alignspec  align)))
	     (format "<col%s />" extra))

	   (if (memq gr '(:end :startend))
	       (progn (setq colgropen nil) "</colgroup>")
	     "")))
	org-lparse-table-num-numeric-items-per-column ""))

      (if colgropen (insert "</colgroup>")))

    ;; fill style attributes for table cells
    (while (re-search-forward "@@class\\([0-9]+\\)@@" nil t)
      (let ((c (string-to-number (match-string 1))))
	(replace-match
	 (if org-export-xhtml-table-align-individual-fields
	     (format (if (and (boundp 'org-xhtml-format-table-no-css)
			      org-xhtml-format-table-no-css)
			 " align=\"%s\"" " class=\"%s\"")
		     (or (aref org-lparse-table-colalign-vector c) "left")) "")
	 t t)))
    (goto-char (point-max)))
  (org-lparse-insert-tag "</table>\n"))

(defun org-xhtml-format-table-row (row)
  (org-xhtml-format-tags
   (cons (eval (car org-export-table-row-tags))
	 (eval (cdr org-export-table-row-tags))) row))

(defun org-xhtml-format-table-cell (text r c horiz-span)
  (let ((cell-style-cookie (or (and org-lparse-table-is-styled
				    (format "@@class%03d@@" c)) "")))
    (cond
     (org-lparse-table-cur-rowgrp-is-hdr
      (org-xhtml-format-tags
       org-export-table-header-tags text  "col" cell-style-cookie))
     ((and (= c 0) org-export-xhtml-table-use-header-tags-for-first-column)
      (org-xhtml-format-tags
       org-export-table-header-tags text "row" cell-style-cookie))
     (t
      (org-xhtml-format-tags
       org-export-table-data-tags text cell-style-cookie)))))

(defun org-xhtml-begin-footnote-definition (n)
  (org-lparse-begin-paragraph 'footnote)
  (insert
   (format
    (format org-export-xhtml-footnote-format
	    "<a class=\"footnum\" name=\"fn.%s\" href=\"#fnr.%s\">%s</a>")
    n n n)))

(defun org-xhtml-end-footnote-definition (n)
  (org-lparse-end-paragraph))

(defun org-xhtml-format-spaces (n)
  (let ((space (or (and org-lparse-encode-pending "\\nbsp") "&nbsp;")) out)
    (while (> n 0)
      (setq out (concat out space))
      (setq n (1- n))) out))

(defun org-xhtml-format-tabs (&optional n)
  (ignore))

(defun org-xhtml-format-line-break ()
  (org-xhtml-format-tags "<br/>" ""))

(defun org-xhtml-format-horizontal-line ()
  (concat  "\n" "<hr/>" "\n"))

(defun org-xhtml-format-line (line)
  (case org-lparse-dyn-current-environment
    ((quote fixedwidth) (concat (org-xml-encode-plain-text line) "\n"))
    (t (concat line "\n"))))

(defun org-xhtml-format-comment (fmt &rest args)
  (let ((comment (apply 'format fmt args)))
    (format "\n<!-- %s  -->\n" comment)))

(defun org-xhtml-format-fontify (text style &optional id)
  (let (class extra how)
    (cond
     ((eq style 'underline)
      (setq extra " style=\"text-decoration:underline;\"" ))
     ((setq how (cdr (assoc style
			    '((bold . ("<b>" . "</b>"))
			      (emphasis . ("<i>" . "</i>"))
			      (code . ("<code>" . "</code>"))
			      (verbatim . ("<code>" . "</code>"))
			      (strike . ("<del>" . "</del>"))
			      (subscript . ("<sub>" . "</sub>"))
			      (superscript . ("<sup>" . "</sup>")))))))
     ((listp style)
      (setq class (mapconcat 'identity style " ")))
     ((stringp style)
      (setq class style))
     (t (error "Unknown style %S" style)))

    (setq extra (concat (when class (format " class=\"%s\"" class))
			(when id (format " id=\"%s\""  id))
			extra))
    (org-xhtml-format-tags
     (or how '("<span%s>" . "</span>")) text extra)))

(defun org-xhtml-format-link (text href &optional extra)
  (let ((extra (concat (format " href=\"%s\"" href)
		       (and extra (concat  " " extra)))))
    (org-xhtml-format-tags '("<a%s>" . "</a>") text extra)))

(defun org-xhtml-format-heading (text level &optional id)
  (let* ((extra (concat (when id (format " id=\"%s\"" id)))))
    (concat (format "<h%d%s>" level extra) text (format "</h%d>" level))))

(defun org-xhtml-format-headline (title extra-targets tags
					    &optional snumber level)
  (concat
   (org-lparse-format 'EXTRA-TARGETS extra-targets)
   (concat (org-lparse-format 'SECTION-NUMBER snumber level) " ")
   title
   (and tags (concat (org-lparse-format 'SPACES 3)
		     (org-lparse-format 'ORG-TAGS tags)))))

(defun org-xhtml-format-anchor (text name &optional class)
  (let* ((id name)
	 (extra (concat
		 (when name (format " name=\"%s\""  name))
		 (when id (format " id=\"%s\""  id))
		 (when class (format " class=\"%s\""  class)))))
    (org-xhtml-format-tags '("<a%s>" . "</a>") text extra)))

(defun org-xhtml-format-footnote-reference (n def refcnt)
  (let ((extra (if (= refcnt 1) "" (format ".%d"  refcnt))))
    (format org-export-xhtml-footnote-format
	    (format
	     "<a class=\"footref\" name=\"fnr.%s%s\" href=\"#fn.%s\">%s</a>"
	     n extra n n))))

(defun org-xhtml-format-footnotes-section (section-name definitions)
  (if (not definitions) ""
    (format org-export-xhtml-footnotes-section section-name definitions)))

(defun org-xhtml-format-org-entity (wd)
  (org-entity-get-representation wd 'html))

(defun org-xhtml-format-tags (tag text &rest args)
  (let ((prefix (when org-lparse-encode-pending "@"))
	(suffix (when org-lparse-encode-pending "@")))
    (apply 'org-lparse-format-tags tag text prefix suffix args)))

(defun org-xhtml-get (what &optional opt-plist)
  (case what
    (BACKEND 'html)
    (INIT-METHOD nil)
    (SAVE-METHOD nil)
    (CLEANUP-METHOD nil)
    ;; (OTHER-BACKENDS
    ;;  ;; There is a provision to register a per-backend converter and
    ;;  ;; output formats. Refer `org-lparse-get-converter' and
    ;;  ;; `org-lparse-get-other-backends'.

    ;;  ;; The default behaviour is to use `org-lparse-convert-process'
    ;;  ;; and `org-lparse-convert-capabilities'.
    ;;  )
    ;; (CONVERT-METHOD
    ;;  ;; See note above
    ;;  )
    (EXPORT-DIR (org-export-directory :html opt-plist))
    (FILE-NAME-EXTENSION (plist-get opt-plist :html-extension))
    (EXPORT-BUFFER-NAME "*Org HTML Export*")
    (ENTITY-CONTROL org-xhtml-entity-control-callbacks-alist)
    (ENTITY-FORMAT org-xhtml-entity-format-callbacks-alist)
    (TOPLEVEL-HLEVEL org-export-xhtml-toplevel-hlevel)
    (SPECIAL-STRING-REGEXPS org-export-xhtml-special-string-regexps)
    (CODING-SYSTEM-FOR-WRITE org-export-xhtml-coding-system)
    (CODING-SYSTEM-FOR-SAVE org-export-xhtml-coding-system)
    (INLINE-IMAGES org-export-xhtml-inline-images)
    (INLINE-IMAGE-EXTENSIONS org-export-xhtml-inline-image-extensions)
    (PLAIN-TEXT-MAP org-export-xhtml-protect-char-alist)
    (TABLE-FIRST-COLUMN-AS-LABELS
     org-export-xhtml-table-use-header-tags-for-first-column)
    (TODO-KWD-CLASS-PREFIX org-export-xhtml-todo-kwd-class-prefix)
    (TAG-CLASS-PREFIX org-export-xhtml-tag-class-prefix)
    (FOOTNOTE-SEPARATOR org-export-xhtml-footnote-separator)
    (t (error "Unknown property: %s"  what))))

(defun org-xhtml-get-coding-system-for-write ()
  (or org-export-xhtml-coding-system
      (and (boundp 'buffer-file-coding-system) buffer-file-coding-system)))

(defun org-xhtml-get-coding-system-for-save ()
  (or org-export-xhtml-coding-system
      (and (boundp 'buffer-file-coding-system) buffer-file-coding-system)))

(defun org-xhtml-insert-toc (toc)
  ;; locate where toc needs to be inserted
  (goto-char (point-min))
  (cond
   ((or (re-search-forward "<p>\\s-*\\[TABLE-OF-CONTENTS\\]\\s-*</p>" nil t)
	(re-search-forward "\\[TABLE-OF-CONTENTS\\]" nil t))
    (goto-char (match-beginning 0))
    (replace-match "")
    (insert toc))
   (org-lparse-dyn-first-heading-pos
    (goto-char org-lparse-dyn-first-heading-pos)
    (when (looking-at "\\s-*</p>")
      (goto-char (match-end 0))
      (insert "\n"))
    (insert toc))
   (t (ignore))))

(defun org-xhtml-insert-preamble (opt-plist)
  (when (plist-get opt-plist :html-preamble)
    (let ((html-pre (plist-get opt-plist :html-preamble))
	  (title (plist-get opt-plist :title))
	  (date (plist-get opt-plist :effective-date))
	  (author (plist-get opt-plist :author))
	  (lang-words (plist-get opt-plist :lang-words))
	  (email (plist-get opt-plist :email))
	  html-pre-real-contents)
      (cond ((stringp html-pre)
	     (setq html-pre-real-contents
		   (format-spec html-pre `((?t . ,title) (?a . ,author)
					   (?d . ,date) (?e . ,email)))))
	    ((functionp html-pre)
	     (insert "<div id=\"" (nth 0 org-export-xhtml-divs) "\">\n")
	     (funcall html-pre)
	     (insert "\n</div>\n"))
	    (t
	     (setq html-pre-real-contents
		   (format-spec
		    (or (cadr (assoc (nth 0 lang-words)
				     org-export-xhtml-preamble-format))
			(cadr (assoc "en" org-export-xhtml-preamble-format)))
		    `((?t . ,title) (?a . ,author)
		      (?d . ,date) (?e . ,email))))))

      ;; don't output an empty preamble DIV
      (unless (and (functionp html-pre)
		   (equal html-pre-real-contents ""))
	(insert "<div id=\"" (nth 0 org-export-xhtml-divs) "\">\n")
	(insert html-pre-real-contents)
	(insert "\n</div>\n")))))

(defun org-xhtml-insert-postamble (opt-plist)
  (when org-lparse-footnote-definitions
    (insert
     (org-lparse-format
      'FOOTNOTES-SECTION (nth 4 (plist-get opt-plist :lang-words))
      (mapconcat (lambda (x) (cdr x))
		 (nreverse org-lparse-footnote-definitions) "\n"))))
  (let ((bib (org-export-xhtml-get-bibliography)))
    (when bib
      (insert "\n" bib "\n")))

  (unless body-only
    (org-lparse-insert-tag "</div>"))

  ;; export html postamble
  (unless body-only
    (let* ((html-post (plist-get opt-plist :html-postamble))
	   (date (plist-get opt-plist :effective-date))
	   (author (plist-get opt-plist :author))
	   (email  (plist-get opt-plist :email))
	   (lang-words (plist-get opt-plist :lang-words))
	   (html-validation-link (or org-export-xhtml-validation-link ""))
	   (email
	    (mapconcat (lambda(e)
			 (format "<a href=\"mailto:%s\">%s</a>" e e))
		       (split-string email ",+ *")
		       ", "))
	   (creator-info
	    (concat "Org version " org-version " with Emacs version "
		    (number-to-string emacs-major-version))))
      (when (plist-get opt-plist :html-postamble)
	(insert "\n<div id=\"" (nth 2 org-export-xhtml-divs) "\">\n")
	(cond ((stringp html-post)
	       (insert (format-spec html-post
				    `((?a . ,author) (?e . ,email)
				      (?d . ,date)   (?c . ,creator-info)
				      (?v . ,html-validation-link)))))
	      ((functionp html-post)
	       (funcall html-post))
	      ((eq html-post 'auto)
	       ;; fall back on default postamble
	       (when (plist-get opt-plist :time-stamp-file)
		 (insert "<p class=\"date\">" (nth 2 lang-words) ": " date "</p>\n"))
	       (when (and (plist-get opt-plist :author-info) author)
		 (insert "<p class=\"author\">" (nth 1 lang-words) ": " author "</p>\n"))
	       (when (and (plist-get opt-plist :email-info) email)
		 (insert "<p class=\"email\">" email "</p>\n"))
	       (when (plist-get opt-plist :creator-info)
		 (insert "<p class=\"creator\">"
			 (concat "Org version " org-version " with Emacs version "
				 (number-to-string emacs-major-version) "</p>\n")))
	       (insert html-validation-link "\n"))
	      (t
	       (insert (format-spec
			(or (cadr (assoc (nth 0 lang-words)
					 org-export-xhtml-postamble-format))
			    (cadr (assoc "en" org-export-xhtml-postamble-format)))
			`((?a . ,author) (?e . ,email)
			  (?d . ,date)   (?c . ,creator-info)
			  (?v . ,html-validation-link))))))
	(insert "</div>"))))

  ;; FIXME `org-export-xhtml-with-timestamp' has been declared
  ;; obsolete since Org 7.7 -- don't forget to remove this.
  (when org-export-xhtml-with-timestamp
    (insert org-export-xhtml-html-helper-timestamp)))

;; There are references to org-html-expand, org-html-protect and
;; org-html-do-expand outside of org-html.el. For now provide a
;; migration path. Ultimately these functions have to be removed.

;; (defun org-html-expand (string)
;;   "A simple wrapper around `org-xml-encode-org-text-skip-links'."
;;   (org-xml-encode-org-text-skip-links string))

;; (defun org-html-protect (s)
;;   "A simple wrapper around `org-xml-encode-plain-text'."
;;   (org-xml-encode-plain-text s))

;; (defun org-html-do-expand (s)
;;   "A simple wrapper around `org-xml-encode-org-text'."
;;   (org-xml-encode-org-text s))

;; (defun org-export-xhtml-format-href (s)
;;   "A simple wrapper around `org-xml-format-href'."
;;   (org-xml-format-href s))

;; (defun org-export-xhtml-format-desc (s)
;;   "A simple wrapper around `org-xml-format-desc'."
;;   (org-xml-format-desc s))

(provide 'org-xhtml)

;;; org-xhtml.el ends here
