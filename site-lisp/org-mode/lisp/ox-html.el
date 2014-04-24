;;; ox-html.el --- HTML Back-End for Org Export Engine

;; Copyright (C) 2011-2014 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;;      Jambunathan K <kjambunathan at gmail dot com>
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

;; This library implements a HTML back-end for Org generic exporter.
;; See Org manual for more information.

;;; Code:

;;; Dependencies

(require 'ox)
(require 'ox-publish)
(require 'format-spec)
(eval-when-compile (require 'cl) (require 'table nil 'noerror))


;;; Function Declarations

(declare-function org-id-find-id-file "org-id" (id))
(declare-function htmlize-region "ext:htmlize" (beg end))
(declare-function org-pop-to-buffer-same-window
		  "org-compat" (&optional buffer-or-name norecord label))
(declare-function mm-url-decode-entities "mm-url" ())

;;; Define Back-End

(org-export-define-backend 'html
  '((bold . org-html-bold)
    (center-block . org-html-center-block)
    (clock . org-html-clock)
    (code . org-html-code)
    (drawer . org-html-drawer)
    (dynamic-block . org-html-dynamic-block)
    (entity . org-html-entity)
    (example-block . org-html-example-block)
    (export-block . org-html-export-block)
    (export-snippet . org-html-export-snippet)
    (fixed-width . org-html-fixed-width)
    (footnote-definition . org-html-footnote-definition)
    (footnote-reference . org-html-footnote-reference)
    (headline . org-html-headline)
    (horizontal-rule . org-html-horizontal-rule)
    (inline-src-block . org-html-inline-src-block)
    (inlinetask . org-html-inlinetask)
    (inner-template . org-html-inner-template)
    (italic . org-html-italic)
    (item . org-html-item)
    (keyword . org-html-keyword)
    (latex-environment . org-html-latex-environment)
    (latex-fragment . org-html-latex-fragment)
    (line-break . org-html-line-break)
    (link . org-html-link)
    (paragraph . org-html-paragraph)
    (plain-list . org-html-plain-list)
    (plain-text . org-html-plain-text)
    (planning . org-html-planning)
    (property-drawer . org-html-property-drawer)
    (quote-block . org-html-quote-block)
    (quote-section . org-html-quote-section)
    (radio-target . org-html-radio-target)
    (section . org-html-section)
    (special-block . org-html-special-block)
    (src-block . org-html-src-block)
    (statistics-cookie . org-html-statistics-cookie)
    (strike-through . org-html-strike-through)
    (subscript . org-html-subscript)
    (superscript . org-html-superscript)
    (table . org-html-table)
    (table-cell . org-html-table-cell)
    (table-row . org-html-table-row)
    (target . org-html-target)
    (template . org-html-template)
    (timestamp . org-html-timestamp)
    (underline . org-html-underline)
    (verbatim . org-html-verbatim)
    (verse-block . org-html-verse-block))
  :export-block "HTML"
  :filters-alist '((:filter-options . org-html-infojs-install-script)
		   (:filter-final-output . org-html-final-function))
  :menu-entry
  '(?h "Export to HTML"
       ((?H "As HTML buffer" org-html-export-as-html)
	(?h "As HTML file" org-html-export-to-html)
	(?o "As HTML file and open"
	    (lambda (a s v b)
	      (if a (org-html-export-to-html t s v b)
		(org-open-file (org-html-export-to-html nil s v b)))))))
  :options-alist
  '((:html-extension nil nil org-html-extension)
    (:html-link-org-as-html nil nil org-html-link-org-files-as-html)
    (:html-doctype "HTML_DOCTYPE" nil org-html-doctype)
    (:html-container "HTML_CONTAINER" nil org-html-container-element)
    (:html-html5-fancy nil "html5-fancy" org-html-html5-fancy)
    (:html-link-use-abs-url nil "html-link-use-abs-url" org-html-link-use-abs-url)
    (:html-link-home "HTML_LINK_HOME" nil org-html-link-home)
    (:html-link-up "HTML_LINK_UP" nil org-html-link-up)
    (:html-mathjax "HTML_MATHJAX" nil "" space)
    (:html-postamble nil "html-postamble" org-html-postamble)
    (:html-preamble nil "html-preamble" org-html-preamble)
    (:html-head "HTML_HEAD" nil org-html-head newline)
    (:html-head-extra "HTML_HEAD_EXTRA" nil org-html-head-extra newline)
    (:html-head-include-default-style nil "html-style" org-html-head-include-default-style)
    (:html-head-include-scripts nil "html-scripts" org-html-head-include-scripts)
    (:html-table-attributes nil nil org-html-table-default-attributes)
    (:html-table-row-tags nil nil org-html-table-row-tags)
    (:html-xml-declaration nil nil org-html-xml-declaration)
    (:html-inline-images nil nil org-html-inline-images)
    (:infojs-opt "INFOJS_OPT" nil nil)
    ;; Redefine regular options.
    (:creator "CREATOR" nil org-html-creator-string)
    (:with-latex nil "tex" org-html-with-latex)
    ;; Retrieve LaTeX header for fragments.
    (:latex-header "LATEX_HEADER" nil nil newline)))


;;; Internal Variables

(defvar org-html-format-table-no-css)
(defvar htmlize-buffer-places)  ; from htmlize.el

(defvar org-html--pre/postamble-class "status"
  "CSS class used for pre/postamble")

(defconst org-html-doctype-alist
  '(("html4-strict" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
\"http://www.w3.org/TR/html4/strict.dtd\">")
    ("html4-transitional" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
\"http://www.w3.org/TR/html4/loose.dtd\">")
    ("html4-frameset" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"
\"http://www.w3.org/TR/html4/frameset.dtd\">")

    ("xhtml-strict" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
    ("xhtml-transitional" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
    ("xhtml-frameset" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">")
    ("xhtml-11" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml11.dtd\">")

    ("html5" . "<!DOCTYPE html>")
    ("xhtml5" . "<!DOCTYPE html>"))
  "An alist mapping (x)html flavors to specific doctypes.")

(defconst org-html-html5-elements
  '("article" "aside" "audio" "canvas" "details" "figcaption"
    "figure" "footer" "header" "menu" "meter" "nav" "output"
    "progress" "section" "video")
  "New elements in html5.

For blocks that should contain headlines, use the HTML_CONTAINER
property on the headline itself.")

(defconst org-html-special-string-regexps
  '(("\\\\-" . "&#x00ad;")		; shy
    ("---\\([^-]\\)" . "&#x2014;\\1")	; mdash
    ("--\\([^-]\\)" . "&#x2013;\\1")	; ndash
    ("\\.\\.\\." . "&#x2026;"))		; hellip
  "Regular expressions for special string conversion.")

(defconst org-html-scripts
  "<script type=\"text/javascript\">
/*
@licstart  The following is the entire license notice for the
JavaScript code in this tag.

Copyright (C) 2012-2013 Free Software Foundation, Inc.

The JavaScript code in this tag is free software: you can
redistribute it and/or modify it under the terms of the GNU
General Public License (GNU GPL) as published by the Free Software
Foundation, either version 3 of the License, or (at your option)
any later version.  The code is distributed WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.

As additional permission under GNU GPL version 3 section 7, you
may distribute non-source (e.g., minimized or compacted) forms of
that code without the copy of the GNU GPL normally required by
section 4, provided you include this license notice and a URL
through which recipients can access the Corresponding Source.


@licend  The above is the entire license notice
for the JavaScript code in this tag.
*/
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
  "Basic JavaScript that is needed by HTML files produced by Org mode.")

(defconst org-html-style-default
  "<style type=\"text/css\">
 <!--/*--><![CDATA[/*><!--*/
  .title  { text-align: center; }
  .todo   { font-family: monospace; color: red; }
  .done   { color: green; }
  .tag    { background-color: #eee; font-family: monospace;
            padding: 2px; font-size: 80%; font-weight: normal; }
  .timestamp { color: #bebebe; }
  .timestamp-kwd { color: #5f9ea0; }
  .right  { margin-left: auto; margin-right: 0px;  text-align: right; }
  .left   { margin-left: 0px;  margin-right: auto; text-align: left; }
  .center { margin-left: auto; margin-right: auto; text-align: center; }
  .underline { text-decoration: underline; }
  #postamble p, #preamble p { font-size: 90%; margin: .2em; }
  p.verse { margin-left: 3%; }
  pre {
    border: 1px solid #ccc;
    box-shadow: 3px 3px 3px #eee;
    padding: 8pt;
    font-family: monospace;
    overflow: auto;
    margin: 1.2em;
  }
  pre.src {
    position: relative;
    overflow: visible;
    padding-top: 1.2em;
  }
  pre.src:before {
    display: none;
    position: absolute;
    background-color: white;
    top: -10px;
    right: 10px;
    padding: 3px;
    border: 1px solid black;
  }
  pre.src:hover:before { display: inline;}
  pre.src-sh:before    { content: 'sh'; }
  pre.src-bash:before  { content: 'sh'; }
  pre.src-emacs-lisp:before { content: 'Emacs Lisp'; }
  pre.src-R:before     { content: 'R'; }
  pre.src-perl:before  { content: 'Perl'; }
  pre.src-java:before  { content: 'Java'; }
  pre.src-sql:before   { content: 'SQL'; }

  table { border-collapse:collapse; }
  caption.t-above { caption-side: top; }
  caption.t-bottom { caption-side: bottom; }
  td, th { vertical-align:top;  }
  th.right  { text-align: center;  }
  th.left   { text-align: center;   }
  th.center { text-align: center; }
  td.right  { text-align: right;  }
  td.left   { text-align: left;   }
  td.center { text-align: center; }
  dt { font-weight: bold; }
  .footpara:nth-child(2) { display: inline; }
  .footpara { display: block; }
  .footdef  { margin-bottom: 1em; }
  .figure { padding: 1em; }
  .figure p { text-align: center; }
  .inlinetask {
    padding: 10px;
    border: 2px solid gray;
    margin: 10px;
    background: #ffffcc;
  }
  #org-div-home-and-up
   { text-align: right; font-size: 70%; white-space: nowrap; }
  textarea { overflow-x: auto; }
  .linenr { font-size: smaller }
  .code-highlighted { background-color: #ffff00; }
  .org-info-js_info-navigation { border-style: none; }
  #org-info-js_console-label
    { font-size: 10px; font-weight: bold; white-space: nowrap; }
  .org-info-js_search-highlight
    { background-color: #ffff00; color: #000000; font-weight: bold; }
  /*]]>*/-->
</style>"
  "The default style specification for exported HTML files.
You can use `org-html-head' and `org-html-head-extra' to add to
this style.  If you don't want to include this default style,
customize `org-html-head-include-default-style'.")


;;; User Configuration Variables

(defgroup org-export-html nil
  "Options for exporting Org mode files to HTML."
  :tag "Org Export HTML"
  :group 'org-export)

;;;; Handle infojs

(defvar org-html-infojs-opts-table
  '((path PATH "http://orgmode.org/org-info.js")
    (view VIEW "info")
    (toc TOC :with-toc)
    (ftoc FIXED_TOC "0")
    (tdepth TOC_DEPTH "max")
    (sdepth SECTION_DEPTH "max")
    (mouse MOUSE_HINT "underline")
    (buttons VIEW_BUTTONS "0")
    (ltoc LOCAL_TOC "1")
    (up LINK_UP :html-link-up)
    (home LINK_HOME :html-link-home))
  "JavaScript options, long form for script, default values.")

(defcustom org-html-use-infojs 'when-configured
  "Non-nil when Sebastian Rose's Java Script org-info.js should be active.
This option can be nil or t to never or always use the script.
It can also be the symbol `when-configured', meaning that the
script will be linked into the export file if and only if there
is a \"#+INFOJS_OPT:\" line in the buffer.  See also the variable
`org-html-infojs-options'."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "When configured in buffer" when-configured)
	  (const :tag "Always" t)))

(defcustom org-html-infojs-options
  (mapcar (lambda (x) (cons (car x) (nth 2 x))) org-html-infojs-opts-table)
  "Options settings for the INFOJS JavaScript.
Each of the options must have an entry in `org-html-infojs-opts-table'.
The value can either be a string that will be passed to the script, or
a property.  This property is then assumed to be a property that is defined
by the Export/Publishing setup of Org.
The `sdepth' and `tdepth' parameters can also be set to \"max\", which
means to use the maximum value consistent with other options."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type
  `(set :greedy t :inline t
	,@(mapcar
	   (lambda (x)
	     (list 'cons (list 'const (car x))
		   '(choice
		     (symbol :tag "Publishing/Export property")
		     (string :tag "Value"))))
	   org-html-infojs-opts-table)))

(defcustom org-html-infojs-template
  "<script type=\"text/javascript\" src=\"%SCRIPT_PATH\">
/**
 *
 * @source: %SCRIPT_PATH
 *
 * @licstart  The following is the entire license notice for the
 *  JavaScript code in %SCRIPT_PATH.
 *
 * Copyright (C) 2012-2013 Free Software Foundation, Inc.
 *
 *
 * The JavaScript code in this tag is free software: you can
 * redistribute it and/or modify it under the terms of the GNU
 * General Public License (GNU GPL) as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option)
 * any later version.  The code is distributed WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.
 *
 * As additional permission under GNU GPL version 3 section 7, you
 * may distribute non-source (e.g., minimized or compacted) forms of
 * that code without the copy of the GNU GPL normally required by
 * section 4, provided you include this license notice and a URL
 * through which recipients can access the Corresponding Source.
 *
 * @licend  The above is the entire license notice
 * for the JavaScript code in %SCRIPT_PATH.
 *
 */
</script>

<script type=\"text/javascript\">

/*
@licstart  The following is the entire license notice for the
JavaScript code in this tag.

Copyright (C) 2012-2013 Free Software Foundation, Inc.

The JavaScript code in this tag is free software: you can
redistribute it and/or modify it under the terms of the GNU
General Public License (GNU GPL) as published by the Free Software
Foundation, either version 3 of the License, or (at your option)
any later version.  The code is distributed WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.

As additional permission under GNU GPL version 3 section 7, you
may distribute non-source (e.g., minimized or compacted) forms of
that code without the copy of the GNU GPL normally required by
section 4, provided you include this license notice and a URL
through which recipients can access the Corresponding Source.


@licend  The above is the entire license notice
for the JavaScript code in this tag.
*/

<!--/*--><![CDATA[/*><!--*/
%MANAGER_OPTIONS
org_html_manager.setup();  // activate after the parameters are set
/*]]>*///-->
</script>"
  "The template for the export style additions when org-info.js is used.
Option settings will replace the %MANAGER-OPTIONS cookie."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defun org-html-infojs-install-script (exp-plist backend)
  "Install script in export options when appropriate.
EXP-PLIST is a plist containing export options.  BACKEND is the
export back-end currently used."
  (unless (or (memq 'body-only (plist-get exp-plist :export-options))
	      (not org-html-use-infojs)
	      (and (eq org-html-use-infojs 'when-configured)
		   (or (not (plist-get exp-plist :infojs-opt))
		       (string= "" (plist-get exp-plist :infojs-opt))
		       (string-match "\\<view:nil\\>"
				     (plist-get exp-plist :infojs-opt)))))
    (let* ((template org-html-infojs-template)
	   (ptoc (plist-get exp-plist :with-toc))
	   (hlevels (plist-get exp-plist :headline-levels))
	   (sdepth hlevels)
	   (tdepth (if (integerp ptoc) (min ptoc hlevels) hlevels))
	   (options (plist-get exp-plist :infojs-opt))
	   (table org-html-infojs-opts-table)
	   style)
      (dolist (entry table)
	(let* ((opt (car entry))
	       (var (nth 1 entry))
	       ;; Compute default values for script option OPT from
	       ;; `org-html-infojs-options' variable.
	       (default
		 (let ((default (cdr (assq opt org-html-infojs-options))))
		   (if (and (symbolp default) (not (memq default '(t nil))))
		       (plist-get exp-plist default)
		     default)))
	       ;; Value set through INFOJS_OPT keyword has precedence
	       ;; over the default one.
	       (val (if (and options
			     (string-match (format "\\<%s:\\(\\S-+\\)" opt)
					   options))
			(match-string 1 options)
		      default)))
	  (case opt
	    (path (setq template
			(replace-regexp-in-string
			 "%SCRIPT_PATH" val template t t)))
	    (sdepth (when (integerp (read val))
		      (setq sdepth (min (read val) sdepth))))
	    (tdepth (when (integerp (read val))
		      (setq tdepth (min (read val) tdepth))))
	    (otherwise (setq val
			     (cond
			      ((or (eq val t) (equal val "t")) "1")
			      ((or (eq val nil) (equal val "nil")) "0")
			      ((stringp val) val)
			      (t (format "%s" val))))
		       (push (cons var val) style)))))
      ;; Now we set the depth of the *generated* TOC to SDEPTH,
      ;; because the toc will actually determine the splitting.  How
      ;; much of the toc will actually be displayed is governed by the
      ;; TDEPTH option.
      (setq exp-plist (plist-put exp-plist :with-toc sdepth))
      ;; The table of contents should not show more sections than we
      ;; generate.
      (setq tdepth (min tdepth sdepth))
      (push (cons "TOC_DEPTH" tdepth) style)
      ;; Build style string.
      (setq style (mapconcat
		   (lambda (x) (format "org_html_manager.set(\"%s\", \"%s\");"
				  (car x)
				  (cdr x)))
		   style "\n"))
      (when (and style (> (length style) 0))
	(and (string-match "%MANAGER_OPTIONS" template)
	     (setq style (replace-match style t t template))
	     (setq exp-plist
		   (plist-put
		    exp-plist :html-head-extra
		    (concat (or (plist-get exp-plist :html-head-extra) "")
			    "\n"
			    style)))))
      ;; This script absolutely needs the table of contents, so we
      ;; change that setting.
      (unless (plist-get exp-plist :with-toc)
	(setq exp-plist (plist-put exp-plist :with-toc t)))
      ;; Return the modified property list.
      exp-plist)))

;;;; Bold, etc.

(defcustom org-html-text-markup-alist
  '((bold . "<b>%s</b>")
    (code . "<code>%s</code>")
    (italic . "<i>%s</i>")
    (strike-through . "<del>%s</del>")
    (underline . "<span class=\"underline\">%s</span>")
    (verbatim . "<code>%s</code>"))
  "Alist of HTML expressions to convert text markup.

The key must be a symbol among `bold', `code', `italic',
`strike-through', `underline' and `verbatim'.  The value is
a formatting string to wrap fontified text with.

If no association can be found for a given markup, text will be
returned as-is."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(alist :key-type (symbol :tag "Markup type")
		:value-type (string :tag "Format string"))
  :options '(bold code italic strike-through underline verbatim))

(defcustom org-html-indent nil
  "Non-nil means to indent the generated HTML.
Warning: non-nil may break indentation of source code blocks."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-html-use-unicode-chars nil
  "Non-nil means to use unicode characters instead of HTML entities."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

;;;; Drawers

(defcustom org-html-format-drawer-function
  (lambda (name contents) contents)
  "Function called to format a drawer in HTML code.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.

The function should return the string to be exported.

For example, the variable could be set to the following function
in order to mimic default behaviour:

The default value simply returns the value of CONTENTS."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'function)

;;;; Footnotes

(defcustom org-html-footnotes-section "<div id=\"footnotes\">
<h2 class=\"footnotes\">%s: </h2>
<div id=\"text-footnotes\">
%s
</div>
</div>"
  "Format for the footnotes section.
Should contain a two instances of %s.  The first will be replaced with the
language-specific word for \"Footnotes\", the second one will be replaced
by the footnotes themselves."
  :group 'org-export-html
  :type 'string)

(defcustom org-html-footnote-format "<sup>%s</sup>"
  "The format for the footnote reference.
%s will be replaced by the footnote reference itself."
  :group 'org-export-html
  :type 'string)

(defcustom org-html-footnote-separator "<sup>, </sup>"
  "Text used to separate footnotes."
  :group 'org-export-html
  :type 'string)

;;;; Headline

(defcustom org-html-toplevel-hlevel 2
  "The <H> level for level 1 headings in HTML export.
This is also important for the classes that will be wrapped around headlines
and outline structure.  If this variable is 1, the top-level headlines will
be <h1>, and the corresponding classes will be outline-1, section-number-1,
and outline-text-1.  If this is 2, all of these will get a 2 instead.
The default for this variable is 2, because we use <h1> for formatting the
document title."
  :group 'org-export-html
  :type 'integer)

(defcustom org-html-format-headline-function 'ignore
  "Function to format headline text.

This function will be called with 5 arguments:
TODO      the todo keyword (string or nil).
TODO-TYPE the type of todo (symbol: `todo', `done', nil)
PRIORITY  the priority of the headline (integer or nil)
TEXT      the main headline text (string).
TAGS      the tags (string or nil).

The function result will be used in the section format string."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'function)

;;;; HTML-specific

(defcustom org-html-allow-name-attribute-in-anchors t
  "When nil, do not set \"name\" attribute in anchors.
By default, anchors are formatted with both \"id\" and \"name\"
attributes, when appropriate."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

;;;; Inlinetasks

(defcustom org-html-format-inlinetask-function 'ignore
  "Function called to format an inlinetask in HTML code.

The function must accept six parameters:
  TODO      the todo keyword, as a string
  TODO-TYPE the todo type, a symbol among `todo', `done' and nil.
  PRIORITY  the inlinetask priority, as a string
  NAME      the inlinetask name, as a string.
  TAGS      the inlinetask tags, as a list of strings.
  CONTENTS  the contents of the inlinetask, as a string.

The function should return the string to be exported."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'function)

;;;; LaTeX

(defcustom org-html-with-latex org-export-with-latex
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
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Do not process math in any way" nil)
	  (const :tag "Use dvipng to make images" dvipng)
	  (const :tag "Use imagemagick to make images" imagemagick)
	  (const :tag "Use MathJax to display math" mathjax)
	  (const :tag "Leave math verbatim" verbatim)))

;;;; Links :: Generic

(defcustom org-html-link-org-files-as-html t
  "Non-nil means make file links to `file.org' point to `file.html'.
When `org-mode' is exporting an `org-mode' file to HTML, links to
non-html files are directly put into a href tag in HTML.
However, links to other Org-mode files (recognized by the
extension `.org.) should become links to the corresponding html
file, assuming that the linked `org-mode' file will also be
converted to HTML.
When nil, the links still point to the plain `.org' file."
  :group 'org-export-html
  :type 'boolean)

;;;; Links :: Inline images

(defcustom org-html-inline-images t
  "Non-nil means inline images into exported HTML pages.
This is done using an <img> tag.  When nil, an anchor with href is used to
link to the image."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.1")
  :type 'boolean)

(defcustom org-html-inline-image-rules
  '(("file" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'")
    ("http" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'")
    ("https" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'"))
  "Rules characterizing image files that can be inlined into HTML.
A rule consists in an association whose key is the type of link
to consider, and value is a regexp that will be matched against
link's path."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(alist :key-type (string :tag "Type")
		:value-type (regexp :tag "Path")))

;;;; Plain Text

(defcustom org-html-protect-char-alist
  '(("&" . "&amp;")
    ("<" . "&lt;")
    (">" . "&gt;"))
  "Alist of characters to be converted by `org-html-protect'."
  :group 'org-export-html
  :type '(repeat (cons (string :tag "Character")
		       (string :tag "HTML equivalent"))))

;;;; Src Block

(defcustom org-html-htmlize-output-type 'inline-css
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
\\[org-html-htmlize-generate-css] to extract class definitions."
  :group 'org-export-html
  :type '(choice (const css) (const inline-css)))

(defcustom org-html-htmlize-font-prefix "org-"
  "The prefix for CSS class names for htmlize font specifications."
  :group 'org-export-html
  :type 'string)

;;;; Table

(defcustom org-html-table-default-attributes
  '(:border "2" :cellspacing "0" :cellpadding "6" :rules "groups" :frame "hsides")
  "Default attributes and values which will be used in table tags.
This is a plist where attributes are symbols, starting with
colons, and values are strings.

When exporting to HTML5, these values will be disregarded."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(plist :key-type (symbol :tag "Property")
		:value-type (string :tag "Value")))

(defcustom org-html-table-header-tags '("<th scope=\"%s\"%s>" . "</th>")
  "The opening tag for table header fields.
This is customizable so that alignment options can be specified.
The first %s will be filled with the scope of the field, either row or col.
The second %s will be replaced by a style entry to align the field.
See also the variable `org-html-table-use-header-tags-for-first-column'.
See also the variable `org-html-table-align-individual-fields'."
  :group 'org-export-html
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom org-html-table-data-tags '("<td%s>" . "</td>")
  "The opening tag for table data fields.
This is customizable so that alignment options can be specified.
The first %s will be filled with the scope of the field, either row or col.
The second %s will be replaced by a style entry to align the field.
See also the variable `org-html-table-align-individual-fields'."
  :group 'org-export-html
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom org-html-table-row-tags '("<tr>" . "</tr>")
  "The opening and ending tags for table rows.
This is customizable so that alignment options can be specified.
Instead of strings, these can be Lisp forms that will be
evaluated for each row in order to construct the table row tags.

During evaluation, these variables will be dynamically bound so that
you can reuse them:

       `row-number': row number (0 is the first row)
  `rowgroup-number': group number of current row
 `start-rowgroup-p': non-nil means the row starts a group
   `end-rowgroup-p': non-nil means the row ends a group
        `top-row-p': non-nil means this is the top row
     `bottom-row-p': non-nil means this is the bottom row

For example:

\(setq org-html-table-row-tags
      (cons '(cond (top-row-p \"<tr class=\\\"tr-top\\\">\")
                   (bottom-row-p \"<tr class=\\\"tr-bottom\\\">\")
                   (t (if (= (mod row-number 2) 1)
			  \"<tr class=\\\"tr-odd\\\">\"
			\"<tr class=\\\"tr-even\\\">\")))
	    \"</tr>\"))

will use the \"tr-top\" and \"tr-bottom\" classes for the top row
and the bottom row, and otherwise alternate between \"tr-odd\" and
\"tr-even\" for odd and even rows."
  :group 'org-export-html
  :type '(cons
	  (choice :tag "Opening tag"
		  (string :tag "Specify")
		  (sexp))
	  (choice :tag "Closing tag"
		  (string :tag "Specify")
		  (sexp))))

(defcustom org-html-table-align-individual-fields t
  "Non-nil means attach style attributes for alignment to each table field.
When nil, alignment will only be specified in the column tags, but this
is ignored by some browsers (like Firefox, Safari).  Opera does it right
though."
  :group 'org-export-html
  :type 'boolean)

(defcustom org-html-table-use-header-tags-for-first-column nil
  "Non-nil means format column one in tables with header tags.
When nil, also column one will use data tags."
  :group 'org-export-html
  :type 'boolean)

(defcustom org-html-table-caption-above t
  "When non-nil, place caption string at the beginning of the table.
Otherwise, place it near the end."
  :group 'org-export-html
  :type 'boolean)

;;;; Tags

(defcustom org-html-tag-class-prefix ""
  "Prefix to class names for TODO keywords.
Each tag gets a class given by the tag itself, with this prefix.
The default prefix is empty because it is nice to just use the keyword
as a class name.  But if you get into conflicts with other, existing
CSS classes, then this prefix can be very useful."
  :group 'org-export-html
  :type 'string)

;;;; Template :: Generic

(defcustom org-html-extension "html"
  "The extension for exported HTML files."
  :group 'org-export-html
  :type 'string)

(defcustom org-html-xml-declaration
  '(("html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
    ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))
  "The extension for exported HTML files.
%s will be replaced with the charset of the exported file.
This may be a string, or an alist with export extensions
and corresponding declarations.

This declaration only applies when exporting to XHTML."
  :group 'org-export-html
  :type '(choice
	  (string :tag "Single declaration")
	  (repeat :tag "Dependent on extension"
		  (cons (string :tag "Extension")
			(string :tag "Declaration")))))

(defcustom org-html-coding-system 'utf-8
  "Coding system for HTML export.
Use utf-8 as the default value."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'coding-system)

(defcustom org-html-doctype "xhtml-strict"
  "Document type definition to use for exported HTML files.
Can be set with the in-buffer HTML_DOCTYPE property or for
publishing, with :html-doctype."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-html-html5-fancy nil
  "Non-nil means using new HTML5 elements.
This variable is ignored for anything other than HTML5 export.

For compatibility with Internet Explorer, it's probably a good
idea to download some form of the html5shiv (for instance
https://code.google.com/p/html5shiv/) and add it to your
HTML_HEAD_EXTRA, so that your pages don't break for users of IE
versions 8 and below."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-html-container-element "div"
  "HTML element to use for wrapping top level sections.
Can be set with the in-buffer HTML_CONTAINER property or for
publishing, with :html-container.

Note that changing the default will prevent you from using
org-info.js for your website."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-html-divs
  '((preamble  "div" "preamble")
    (content   "div" "content")
    (postamble "div" "postamble"))
  "Alist of the three section elements for HTML export.
The car of each entry is one of 'preamble, 'content or 'postamble.
The cdrs of each entry are the ELEMENT_TYPE and ID for each
section of the exported document.

Note that changing the default will prevent you from using
org-info.js for your website."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(list :greedy t
	       (list :tag "Preamble"
		     (const :format "" preamble)
		     (string :tag "element") (string :tag "     id"))
	       (list :tag "Content"
		     (const :format "" content)
		     (string :tag "element") (string :tag "     id"))
	       (list :tag "Postamble" (const :format "" postamble)
		     (string :tag "     id") (string :tag "element"))))

(defcustom org-html-metadata-timestamp-format "%Y-%m-%d %a %H:%M"
  "Format used for timestamps in preamble, postamble and metadata.
See `format-time-string' for more information on its components."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

;;;; Template :: Mathjax

(defcustom org-html-mathjax-options
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
  :group 'org-export-html
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

(defcustom org-html-mathjax-template
  "<script type=\"text/javascript\" src=\"%PATH\"></script>
<script type=\"text/javascript\">
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
  :group 'org-export-html
  :type 'string)

;;;; Template :: Postamble

(defcustom org-html-postamble 'auto
  "Non-nil means insert a postamble in HTML export.

When set to 'auto, check against the
`org-export-with-author/email/creator/date' variables to set the
content of the postamble.  When set to a string, use this string
as the postamble.  When t, insert a string as defined by the
formatting string in `org-html-postamble-format'.

When set to a function, apply this function and insert the
returned string.  The function takes the property list of export
options as its only argument.

Setting :html-postamble in publishing projects will take
precedence over this variable."
  :group 'org-export-html
  :type '(choice (const :tag "No postamble" nil)
		 (const :tag "Auto postamble" auto)
		 (const :tag "Default formatting string" t)
		 (string :tag "Custom formatting string")
		 (function :tag "Function (must return a string)")))

(defcustom org-html-postamble-format
  '(("en" "<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Date: %d</p>
<p class=\"creator\">%c</p>
<p class=\"validation\">%v</p>"))
  "Alist of languages and format strings for the HTML postamble.

The first element of each list is the language code, as used for
the LANGUAGE keyword.  See `org-export-default-language'.

The second element of each list is a format string to format the
postamble itself.  This format string can contain these elements:

  %t stands for the title.
  %a stands for the author's name.
  %e stands for the author's email.
  %d stands for the date.
  %c will be replaced by `org-html-creator-string'.
  %v will be replaced by `org-html-validation-link'.
  %T will be replaced by the export time.
  %C will be replaced by the last modification time.

If you need to use a \"%\" character, you need to escape it
like that: \"%%\"."
  :group 'org-export-html
  :type '(repeat
	  (list (string :tag "Language")
		(string :tag "Format string"))))

(defcustom org-html-validation-link
  "<a href=\"http://validator.w3.org/check?uri=referer\">Validate</a>"
  "Link to HTML validation service."
  :group 'org-export-html
  :type 'string)

(defcustom org-html-creator-string
  (format "<a href=\"http://www.gnu.org/software/emacs/\">Emacs</a> %s (<a href=\"http://orgmode.org\">Org</a> mode %s)"
	  emacs-version
	  (if (fboundp 'org-version) (org-version) "unknown version"))
  "Information about the creator of the HTML document.
This option can also be set on with the CREATOR keyword."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(string :tag "Creator string"))

;;;; Template :: Preamble

(defcustom org-html-preamble t
  "Non-nil means insert a preamble in HTML export.

When t, insert a string as defined by the formatting string in
`org-html-preamble-format'.  When set to a string, use this
formatting string instead (see `org-html-postamble-format' for an
example of such a formatting string).

When set to a function, apply this function and insert the
returned string.  The function takes the property list of export
options as its only argument.

Setting :html-preamble in publishing projects will take
precedence over this variable."
  :group 'org-export-html
  :type '(choice (const :tag "No preamble" nil)
		 (const :tag "Default preamble" t)
		 (string :tag "Custom formatting string")
		 (function :tag "Function (must return a string)")))

(defcustom org-html-preamble-format '(("en" ""))
  "Alist of languages and format strings for the HTML preamble.

The first element of each list is the language code, as used for
the LANGUAGE keyword.  See `org-export-default-language'.

The second element of each list is a format string to format the
preamble itself.  This format string can contain these elements:

  %t stands for the title.
  %a stands for the author's name.
  %e stands for the author's email.
  %d stands for the date.
  %c will be replaced by `org-html-creator-string'.
  %v will be replaced by `org-html-validation-link'.
  %T will be replaced by the export time.
  %C will be replaced by the last modification time.

If you need to use a \"%\" character, you need to escape it
like that: \"%%\".

See the default value of `org-html-postamble-format' for an
example."
  :group 'org-export-html
  :type '(repeat
	  (list (string :tag "Language")
		(string :tag "Format string"))))

(defcustom org-html-link-up ""
  "Where should the \"UP\" link of exported HTML pages lead?"
  :group 'org-export-html
  :type '(string :tag "File or URL"))

(defcustom org-html-link-home ""
  "Where should the \"HOME\" link of exported HTML pages lead?"
  :group 'org-export-html
  :type '(string :tag "File or URL"))

(defcustom org-html-link-use-abs-url nil
  "Should we prepend relative links with HTML_LINK_HOME?"
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.1")
  :type 'boolean)

(defcustom org-html-home/up-format
  "<div id=\"org-div-home-and-up\">
 <a accesskey=\"h\" href=\"%s\"> UP </a>
 |
 <a accesskey=\"H\" href=\"%s\"> HOME </a>
</div>"
  "Snippet used to insert the HOME and UP links.
This is a format string, the first %s will receive the UP link,
the second the HOME link.  If both `org-html-link-up' and
`org-html-link-home' are empty, the entire snippet will be
ignored."
  :group 'org-export-html
  :type 'string)

;;;; Template :: Scripts

(define-obsolete-variable-alias
  'org-html-style-include-scripts 'org-html-head-include-scripts "24.4")
(defcustom org-html-head-include-scripts t
  "Non-nil means include the JavaScript snippets in exported HTML files.
The actual script is defined in `org-html-scripts' and should
not be modified."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

;;;; Template :: Styles

(define-obsolete-variable-alias
  'org-html-style-include-default 'org-html-head-include-default-style "24.4")
(defcustom org-html-head-include-default-style t
  "Non-nil means include the default style in exported HTML files.
The actual style is defined in `org-html-style-default' and
should not be modified.  Use `org-html-head' to use your own
style information."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)
;;;###autoload
(put 'org-html-head-include-default-style 'safe-local-variable 'booleanp)

(define-obsolete-variable-alias 'org-html-style 'org-html-head "24.4")
(defcustom org-html-head ""
  "Org-wide head definitions for exported HTML files.

This variable can contain the full HTML structure to provide a
style, including the surrounding HTML tags.  You can consider
including definitions for the following classes: title, todo,
done, timestamp, timestamp-kwd, tag, target.

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

If you want to refer to an external style, use something like

   <link rel=\"stylesheet\" type=\"text/css\" href=\"mystyles.css\" />

As the value of this option simply gets inserted into the HTML
<head> header, you can use it to add any arbitrary text to the
header.

You can set this on a per-file basis using #+HTML_HEAD:,
or for publication projects using the :html-head property."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)
;;;###autoload
(put 'org-html-head 'safe-local-variable 'stringp)

(defcustom org-html-head-extra ""
  "More head information to add in the HTML output.

You can set this on a per-file basis using #+HTML_HEAD_EXTRA:,
or for publication projects using the :html-head-extra property."
  :group 'org-export-html
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)
;;;###autoload
(put 'org-html-head-extra 'safe-local-variable 'stringp)

;;;; Todos

(defcustom org-html-todo-kwd-class-prefix ""
  "Prefix to class names for TODO keywords.
Each TODO keyword gets a class given by the keyword itself, with this prefix.
The default prefix is empty because it is nice to just use the keyword
as a class name.  But if you get into conflicts with other, existing
CSS classes, then this prefix can be very useful."
  :group 'org-export-html
  :type 'string)


;;; Internal Functions

(defun org-html-xhtml-p (info)
  (let ((dt (downcase (plist-get info :html-doctype))))
    (string-match-p "xhtml" dt)))

(defun org-html-html5-p (info)
  (let ((dt (downcase (plist-get info :html-doctype))))
	(member dt '("html5" "xhtml5" "<!doctype html>"))))

(defun org-html-close-tag (tag attr info)
  (concat "<" tag " " attr
	  (if (org-html-xhtml-p info) " />" ">")))

(defun org-html-doctype (info)
  "Return correct html doctype tag from `org-html-doctype-alist',
or the literal value of :html-doctype from INFO if :html-doctype
is not found in the alist.
INFO is a plist used as a communication channel."
  (let ((dt (plist-get info :html-doctype)))
    (or (cdr (assoc dt org-html-doctype-alist)) dt)))

(defun org-html--make-attribute-string (attributes)
  "Return a list of attributes, as a string.
ATTRIBUTES is a plist where values are either strings or nil. An
attributes with a nil value will be omitted from the result."
  (let (output)
    (dolist (item attributes (mapconcat 'identity (nreverse output) " "))
      (cond ((null item) (pop output))
            ((symbolp item) (push (substring (symbol-name item) 1) output))
            (t (let ((key (car output))
                     (value (replace-regexp-in-string
                             "\"" "&quot;" (org-html-encode-plain-text item))))
                 (setcar output (format "%s=\"%s\"" key value))))))))

(defun org-html--wrap-image (contents info &optional caption label)
  "Wrap CONTENTS string within an appropriate environment for images.
INFO is a plist used as a communication channel.  When optional
arguments CAPTION and LABEL are given, use them for caption and
\"id\" attribute."
  (let ((html5-fancy (and (org-html-html5-p info)
			  (plist-get info :html-html5-fancy))))
    (format (if html5-fancy "\n<figure%s>%s%s\n</figure>"
	      "\n<div%s class=\"figure\">%s%s\n</div>")
	    ;; ID.
	    (if (not (org-string-nw-p label)) ""
	      (format " id=\"%s\"" (org-export-solidify-link-text label)))
	    ;; Contents.
	    (format "\n<p>%s</p>" contents)
	    ;; Caption.
	    (if (not (org-string-nw-p caption)) ""
	      (format (if html5-fancy "\n<figcaption>%s</figcaption>"
			"\n<p>%s</p>")
		      caption)))))

(defun org-html--format-image (source attributes info)
  "Return \"img\" tag with given SOURCE and ATTRIBUTES.
SOURCE is a string specifying the location of the image.
ATTRIBUTES is a plist, as returned by
`org-export-read-attribute'.  INFO is a plist used as
a communication channel."
  (org-html-close-tag
   "img"
   (org-html--make-attribute-string
    (org-combine-plists
     (list :src source
	   :alt (if (string-match-p "^ltxpng/" source)
		    (org-html-encode-plain-text
		     (org-find-text-property-in-string 'org-latex-src source))
		  (file-name-nondirectory source)))
     attributes))
   info))

(defun org-html--textarea-block (element)
  "Transcode ELEMENT into a textarea block.
ELEMENT is either a src block or an example block."
  (let* ((code (car (org-export-unravel-code element)))
	 (attr (org-export-read-attribute :attr_html element)))
    (format "<p>\n<textarea cols=\"%s\" rows=\"%s\">\n%s</textarea>\n</p>"
	    (or (plist-get attr :width) 80)
	    (or (plist-get attr :height) (org-count-lines code))
	    code)))

(defun org-html--has-caption-p (element &optional info)
  "Non-nil when ELEMENT has a caption affiliated keyword.
INFO is a plist used as a communication channel.  This function
is meant to be used as a predicate for `org-export-get-ordinal' or
a value to `org-html-standalone-image-predicate'."
  (org-element-property :caption element))

;;;; Table

(defun org-html-htmlize-region-for-paste (beg end)
  "Convert the region between BEG and END to HTML, using htmlize.el.
This is much like `htmlize-region-for-paste', only that it uses
the settings define in the org-... variables."
  (let* ((htmlize-output-type org-html-htmlize-output-type)
	 (htmlize-css-name-prefix org-html-htmlize-font-prefix)
	 (htmlbuf (htmlize-region beg end)))
    (unwind-protect
	(with-current-buffer htmlbuf
	  (buffer-substring (plist-get htmlize-buffer-places 'content-start)
			    (plist-get htmlize-buffer-places 'content-end)))
      (kill-buffer htmlbuf))))

;;;###autoload
(defun org-html-htmlize-generate-css ()
  "Create the CSS for all font definitions in the current Emacs session.
Use this to create face definitions in your CSS style file that can then
be used by code snippets transformed by htmlize.
This command just produces a buffer that contains class definitions for all
faces used in the current Emacs session.  You can copy and paste the ones you
need into your CSS file.

If you then set `org-html-htmlize-output-type' to `css', calls
to the function `org-html-htmlize-region-for-paste' will
produce code that uses these same face definitions."
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

(defun org-html--make-string (n string)
  "Build a string by concatenating N times STRING."
  (let (out) (dotimes (i n out) (setq out (concat string out)))))

(defun org-html-fix-class-name (kwd)	; audit callers of this function
  "Turn todo keyword KWD into a valid class name.
Replaces invalid characters with \"_\"."
  (save-match-data
    (while (string-match "[^a-zA-Z0-9_]" kwd)
      (setq kwd (replace-match "_" t t kwd))))
  kwd)

(defun org-html-format-footnote-reference (n def refcnt)
  "Format footnote reference N with definition DEF into HTML."
  (let ((extra (if (= refcnt 1) "" (format ".%d"  refcnt))))
    (format org-html-footnote-format
	    (let* ((id (format "fnr.%s%s" n extra))
		   (href (format " href=\"#fn.%s\"" n))
		   (attributes (concat " class=\"footref\"" href)))
	      (org-html--anchor id n attributes)))))

(defun org-html-format-footnotes-section (section-name definitions)
  "Format footnotes section SECTION-NAME."
  (if (not definitions) ""
    (format org-html-footnotes-section section-name definitions)))

(defun org-html-format-footnote-definition (fn)
  "Format the footnote definition FN."
  (let ((n (car fn)) (def (cdr fn)))
    (format
     "<div class=\"footdef\">%s %s</div>\n"
     (format org-html-footnote-format
	     (let* ((id (format "fn.%s" n))
		    (href (format " href=\"#fnr.%s\"" n))
		    (attributes (concat " class=\"footnum\"" href)))
	       (org-html--anchor id n attributes)))
     def)))

(defun org-html-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (let* ((fn-alist (org-export-collect-footnote-definitions
		    (plist-get info :parse-tree) info))
	 (fn-alist
	  (loop for (n type raw) in fn-alist collect
		(cons n (if (eq (org-element-type raw) 'org-data)
			    (org-trim (org-export-data raw info))
			  (format "<p>%s</p>"
				  (org-trim (org-export-data raw info))))))))
    (when fn-alist
      (org-html-format-footnotes-section
       (org-html--translate "Footnotes" info)
       (format
	"\n%s\n"
	(mapconcat 'org-html-format-footnote-definition fn-alist "\n"))))))


;;; Template

(defun org-html--build-meta-info (info)
  "Return meta tags for exported document.
INFO is a plist used as a communication channel."
  (let ((protect-string
	 (lambda (str)
	   (replace-regexp-in-string
	    "\"" "&quot;" (org-html-encode-plain-text str))))
	(title (org-export-data (plist-get info :title) info))
	(author (and (plist-get info :with-author)
		     (let ((auth (plist-get info :author)))
		       (and auth
			    ;; Return raw Org syntax, skipping non
			    ;; exportable objects.
			    (org-element-interpret-data
			     (org-element-map auth
				 (cons 'plain-text org-element-all-objects)
			       'identity info))))))
	(description (plist-get info :description))
	(keywords (plist-get info :keywords))
	(charset (or (and org-html-coding-system
			  (fboundp 'coding-system-get)
			  (coding-system-get org-html-coding-system
					     'mime-charset))
		     "iso-8859-1")))
    (concat
     (format "<title>%s</title>\n" title)
     (when (plist-get info :time-stamp-file)
       (format-time-string
	 (concat "<!-- " org-html-metadata-timestamp-format " -->\n")))
     (format
      (if (org-html-html5-p info)
	  (org-html-close-tag "meta" " charset=\"%s\"" info)
	(org-html-close-tag
	 "meta" " http-equiv=\"Content-Type\" content=\"text/html;charset=%s\""
	 info))
      charset) "\n"
     (org-html-close-tag "meta" " name=\"generator\" content=\"Org-mode\"" info)
     "\n"
     (and (org-string-nw-p author)
	  (concat
	   (org-html-close-tag "meta"
			       (format " name=\"author\" content=\"%s\""
				       (funcall protect-string author))
			       info)
	   "\n"))
     (and (org-string-nw-p description)
	  (concat
	   (org-html-close-tag "meta"
			       (format " name=\"description\" content=\"%s\"\n"
				       (funcall protect-string description))
			       info)
	   "\n"))
     (and (org-string-nw-p keywords)
	  (concat
	   (org-html-close-tag "meta"
			       (format " name=\"keywords\" content=\"%s\""
				       (funcall protect-string keywords))
			       info)
	   "\n")))))

(defun org-html--build-head (info)
  "Return information for the <head>..</head> of the HTML output.
INFO is a plist used as a communication channel."
  (org-element-normalize-string
   (concat
    (when (plist-get info :html-head-include-default-style)
      (org-element-normalize-string org-html-style-default))
    (org-element-normalize-string (plist-get info :html-head))
    (org-element-normalize-string (plist-get info :html-head-extra))
    (when (and (plist-get info :html-htmlized-css-url)
	       (eq org-html-htmlize-output-type 'css))
      (org-html-close-tag "link"
			  (format " rel=\"stylesheet\" href=\"%s\" type=\"text/css\""
				  (plist-get info :html-htmlized-css-url))
			  info))
    (when (plist-get info :html-head-include-scripts) org-html-scripts))))

(defun org-html--build-mathjax-config (info)
  "Insert the user setup into the mathjax template.
INFO is a plist used as a communication channel."
  (when (and (memq (plist-get info :with-latex) '(mathjax t))
	     (org-element-map (plist-get info :parse-tree)
		 '(latex-fragment latex-environment) 'identity info t))
    (let ((template org-html-mathjax-template)
	  (options org-html-mathjax-options)
	  (in-buffer (or (plist-get info :html-mathjax) ""))
	  name val (yes "   ") (no "// ") x)
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
      ;; Exchange prefixes depending on mathml setting.
      (if (not val) (setq x yes yes no no x))
      ;; Replace cookies to turn on or off the config/jax lines.
      (if (string-match ":MMLYES:" template)
	  (setq template (replace-match yes t t template)))
      (if (string-match ":MMLNO:" template)
	  (setq template (replace-match no t t template)))
      ;; Return the modified template.
      (org-element-normalize-string template))))

(defun org-html-format-spec (info)
  "Return format specification for elements that can be
used in the preamble or postamble."
  `((?t . ,(org-export-data (plist-get info :title) info))
    (?d . ,(org-export-data (org-export-get-date info) info))
    (?T . ,(format-time-string org-html-metadata-timestamp-format))
    (?a . ,(org-export-data (plist-get info :author) info))
    (?e . ,(mapconcat
	    (lambda (e)
	      (format "<a href=\"mailto:%s\">%s</a>" e e))
	    (split-string (plist-get info :email)  ",+ *")
	    ", "))
    (?c . ,(plist-get info :creator))
    (?C . ,(let ((file (plist-get info :input-file)))
	     (format-time-string org-html-metadata-timestamp-format
				 (if file (nth 5 (file-attributes file))
				   (current-time)))))
    (?v . ,(or org-html-validation-link ""))))

(defun org-html--build-pre/postamble (type info)
  "Return document preamble or postamble as a string, or nil.
TYPE is either 'preamble or 'postamble, INFO is a plist used as a
communication channel."
  (let ((section (plist-get info (intern (format ":html-%s" type))))
	(spec (org-html-format-spec info)))
    (when section
      (let ((section-contents
	     (if (functionp section) (funcall section info)
	       (cond
		((stringp section) (format-spec section spec))
		((eq section 'auto)
		 (let ((date (cdr (assq ?d spec)))
		       (author (cdr (assq ?a spec)))
		       (email (cdr (assq ?e spec)))
		       (creator (cdr (assq ?c spec)))
		       (timestamp (cdr (assq ?T spec)))
		       (validation-link (cdr (assq ?v spec))))
		   (concat
		    (when (and (plist-get info :with-date)
			       (org-string-nw-p date))
		      (format "<p class=\"date\">%s: %s</p>\n"
			      (org-html--translate "Date" info)
			      date))
		    (when (and (plist-get info :with-author)
			       (org-string-nw-p author))
		      (format "<p class=\"author\">%s: %s</p>\n"
			      (org-html--translate "Author" info)
			      author))
		    (when (and (plist-get info :with-email)
			       (org-string-nw-p email))
		      (format "<p class=\"email\">%s: %s</p>\n"
			      (org-html--translate "Email" info)
			      email))
		    (when (plist-get info :time-stamp-file)
		      (format
		       "<p class=\"date\">%s: %s</p>\n"
		       (org-html--translate "Created" info)
		       (format-time-string org-html-metadata-timestamp-format)))
		    (when (plist-get info :with-creator)
		      (format "<p class=\"creator\">%s</p>\n" creator))
		    (format "<p class=\"validation\">%s</p>\n"
			    validation-link))))
		(t (format-spec
		    (or (cadr (assoc
			       (plist-get info :language)
			       (eval (intern
				      (format "org-html-%s-format" type)))))
			(cadr
			 (assoc
			  "en"
			  (eval
			   (intern (format "org-html-%s-format" type))))))
		    spec))))))
	(when (org-string-nw-p section-contents)
	  (concat
	   (format "<%s id=\"%s\" class=\"%s\">\n"
		   (nth 1 (assq type org-html-divs))
		   (nth 2 (assq type org-html-divs))
		   org-html--pre/postamble-class)
	   (org-element-normalize-string section-contents)
	   (format "</%s>\n" (nth 1 (assq type org-html-divs)))))))))

(defun org-html-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-html-toc depth info)))
   ;; Document contents.
   contents
   ;; Footnotes section.
   (org-html-footnote-section info)))

(defun org-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
     (let ((decl (or (and (stringp org-html-xml-declaration)
			      org-html-xml-declaration)
			 (cdr (assoc (plist-get info :html-extension)
				     org-html-xml-declaration))
			 (cdr (assoc "html" org-html-xml-declaration))

			 "")))
       (when (not (or (eq nil decl) (string= "" decl)))
	 (format "%s\n"
		 (format decl
		  (or (and org-html-coding-system
			   (fboundp 'coding-system-get)
			   (coding-system-get org-html-coding-system 'mime-charset))
		      "iso-8859-1"))))))
   (org-html-doctype info)
   "\n"
   (concat "<html"
	   (when (org-html-xhtml-p info)
	     (format
	      " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
	      (plist-get info :language) (plist-get info :language)))
	   ">\n")
   "<head>\n"
   (org-html--build-meta-info info)
   (org-html--build-head info)
   (org-html--build-mathjax-config info)
   "</head>\n"
   "<body>\n"
   (let ((link-up (org-trim (plist-get info :html-link-up)))
	 (link-home (org-trim (plist-get info :html-link-home))))
     (unless (and (string= link-up "") (string= link-home ""))
       (format org-html-home/up-format
	       (or link-up link-home)
	       (or link-home link-up))))
   ;; Preamble.
   (org-html--build-pre/postamble 'preamble info)
   ;; Document contents.
   (format "<%s id=\"%s\">\n"
	   (nth 1 (assq 'content org-html-divs))
	   (nth 2 (assq 'content org-html-divs)))
   ;; Document title.
   (let ((title (plist-get info :title)))
     (format "<h1 class=\"title\">%s</h1>\n" (org-export-data (or title "") info)))
   contents
   (format "</%s>\n"
	   (nth 1 (assq 'content org-html-divs)))
   ;; Postamble.
   (org-html--build-pre/postamble 'postamble info)
   ;; Closing document.
   "</body>\n</html>"))

(defun org-html--translate (s info)
  "Translate string S according to specified language.
INFO is a plist used as a communication channel."
  (org-export-translate s :html info))

;;;; Anchor

(defun org-html--anchor (&optional id desc attributes)
  "Format a HTML anchor."
  (let* ((name (and org-html-allow-name-attribute-in-anchors id))
	 (attributes (concat (and id (format " id=\"%s\"" id))
			     (and name (format " name=\"%s\"" name))
			     attributes)))
    (format "<a%s>%s</a>" attributes (or desc ""))))

;;;; Todo

(defun org-html--todo (todo)
  "Format TODO keywords into HTML."
  (when todo
    (format "<span class=\"%s %s%s\">%s</span>"
	    (if (member todo org-done-keywords) "done" "todo")
	    org-html-todo-kwd-class-prefix (org-html-fix-class-name todo)
	    todo)))

;;;; Tags

(defun org-html--tags (tags)
  "Format TAGS into HTML."
  (when tags
    (format "<span class=\"tag\">%s</span>"
	    (mapconcat
	     (lambda (tag)
	       (format "<span class=\"%s\">%s</span>"
		       (concat org-html-tag-class-prefix
			       (org-html-fix-class-name tag))
		       tag))
	     tags "&#xa0;"))))

;;;; Headline

(defun* org-html-format-headline
  (todo todo-type priority text tags
	&key level section-number headline-label &allow-other-keys)
  "Format a headline in HTML."
  (let ((section-number
	 (when section-number
	   (format "<span class=\"section-number-%d\">%s</span> "
		   level section-number)))
	(todo (org-html--todo todo))
	(tags (org-html--tags tags)))
    (concat section-number todo (and todo " ") text
	    (and tags "&#xa0;&#xa0;&#xa0;") tags)))

;;;; Src Code

(defun org-html-fontify-code (code lang)
  "Color CODE with htmlize library.
CODE is a string representing the source code to colorize.  LANG
is the language used for CODE, as a string, or nil."
  (when code
    (cond
     ;; Case 1: No lang.  Possibly an example block.
     ((not lang)
      ;; Simple transcoding.
      (org-html-encode-plain-text code))
     ;; Case 2: No htmlize or an inferior version of htmlize
     ((not (and (require 'htmlize nil t) (fboundp 'htmlize-region-for-paste)))
      ;; Emit a warning.
      (message "Cannot fontify src block (htmlize.el >= 1.34 required)")
      ;; Simple transcoding.
      (org-html-encode-plain-text code))
     (t
      ;; Map language
      (setq lang (or (assoc-default lang org-src-lang-modes) lang))
      (let* ((lang-mode (and lang (intern (format "%s-mode" lang)))))
	(cond
	 ;; Case 1: Language is not associated with any Emacs mode
	 ((not (functionp lang-mode))
	  ;; Simple transcoding.
	  (org-html-encode-plain-text code))
	 ;; Case 2: Default.  Fontify code.
	 (t
	  ;; htmlize
	  (setq code (with-temp-buffer
		       ;; Switch to language-specific mode.
		       (funcall lang-mode)
		       (insert code)
		       ;; Fontify buffer.
		       (font-lock-fontify-buffer)
		       ;; Remove formatting on newline characters.
		       (save-excursion
			 (let ((beg (point-min))
			       (end (point-max)))
			   (goto-char beg)
			   (while (progn (end-of-line) (< (point) end))
			     (put-text-property (point) (1+ (point)) 'face nil)
			     (forward-char 1))))
		       (org-src-mode)
		       (set-buffer-modified-p nil)
		       ;; Htmlize region.
		       (org-html-htmlize-region-for-paste
			(point-min) (point-max))))
	  ;; Strip any enclosing <pre></pre> tags.
	  (let* ((beg (and (string-match "\\`<pre[^>]*>\n*" code) (match-end 0)))
		 (end (and beg (string-match "</pre>\\'" code))))
	    (if (and beg end) (substring code beg end) code)))))))))

(defun org-html-do-format-code
  (code &optional lang refs retain-labels num-start)
  "Format CODE string as source code.
Optional arguments LANG, REFS, RETAIN-LABELS and NUM-START are,
respectively, the language of the source code, as a string, an
alist between line numbers and references (as returned by
`org-export-unravel-code'), a boolean specifying if labels should
appear in the source code, and the number associated to the first
line of code."
  (let* ((code-lines (org-split-string code "\n"))
	 (code-length (length code-lines))
	 (num-fmt
	  (and num-start
	       (format "%%%ds: "
		       (length (number-to-string (+ code-length num-start))))))
	 (code (org-html-fontify-code code lang)))
    (org-export-format-code
     code
     (lambda (loc line-num ref)
       (setq loc
	     (concat
	      ;; Add line number, if needed.
	      (when num-start
		(format "<span class=\"linenr\">%s</span>"
			(format num-fmt line-num)))
	      ;; Transcoded src line.
	      loc
	      ;; Add label, if needed.
	      (when (and ref retain-labels) (format " (%s)" ref))))
       ;; Mark transcoded line as an anchor, if needed.
       (if (not ref) loc
	 (format "<span id=\"coderef-%s\" class=\"coderef-off\">%s</span>"
		 ref loc)))
     num-start refs)))

(defun org-html-format-code (element info)
  "Format contents of ELEMENT as source code.
ELEMENT is either an example block or a src block.  INFO is
a plist used as a communication channel."
  (let* ((lang (org-element-property :language element))
	 ;; Extract code and references.
	 (code-info (org-export-unravel-code element))
	 (code (car code-info))
	 (refs (cdr code-info))
	 ;; Does the src block contain labels?
	 (retain-labels (org-element-property :retain-labels element))
	 ;; Does it have line numbers?
	 (num-start (case (org-element-property :number-lines element)
		      (continued (org-export-get-loc element info))
		      (new 0))))
    (org-html-do-format-code code lang refs retain-labels num-start)))


;;; Tables of Contents

(defun org-html-toc (depth info)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is a
plist used as a communication channel.  Return the table of
contents as a string, or nil if it is empty."
  (let ((toc-entries
	 (mapcar (lambda (headline)
		   (cons (org-html--format-toc-headline headline info)
			 (org-export-get-relative-level headline info)))
		 (org-export-collect-headlines info depth)))
	(outer-tag (if (and (org-html-html5-p info)
			    (plist-get info :html-html5-fancy))
		       "nav"
		     "div")))
    (when toc-entries
      (concat (format "<%s id=\"table-of-contents\">\n" outer-tag)
	      (format "<h%d>%s</h%d>\n"
		      org-html-toplevel-hlevel
		      (org-html--translate "Table of Contents" info)
		      org-html-toplevel-hlevel)
	      "<div id=\"text-table-of-contents\">"
	      (org-html--toc-text toc-entries)
	      "</div>\n"
	      (format "</%s>\n" outer-tag)))))

(defun org-html--toc-text (toc-entries)
  "Return innards of a table of contents, as a string.
TOC-ENTRIES is an alist where key is an entry title, as a string,
and value is its relative level, as an integer."
  (let* ((prev-level (1- (cdar toc-entries)))
	 (start-level prev-level))
    (concat
     (mapconcat
      (lambda (entry)
	(let ((headline (car entry))
	      (level (cdr entry)))
	  (concat
	   (let* ((cnt (- level prev-level))
		  (times (if (> cnt 0) (1- cnt) (- cnt)))
		  rtn)
	     (setq prev-level level)
	     (concat
	      (org-html--make-string
	       times (cond ((> cnt 0) "\n<ul>\n<li>")
			   ((< cnt 0) "</li>\n</ul>\n")))
	      (if (> cnt 0) "\n<ul>\n<li>" "</li>\n<li>")))
	   headline)))
      toc-entries "")
     (org-html--make-string (- prev-level start-level) "</li>\n</ul>\n"))))

(defun org-html--format-toc-headline (headline info)
  "Return an appropriate table of contents entry for HEADLINE.
INFO is a plist used as a communication channel."
  (let* ((headline-number (org-export-get-headline-number headline info))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property :todo-keyword headline)))
		      (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (text (org-export-data-with-backend
		(org-export-get-alt-title headline info)
		;; Create an anonymous back-end that will ignore any
		;; footnote-reference, link, radio-target and target
		;; in table of contents.
		(org-export-create-backend
		 :parent 'html
		 :transcoders '((footnote-reference . ignore)
				(link . (lambda (object c i) c))
				(radio-target . (lambda (object c i) c))
				(target . ignore)))
		info))
	 (tags (and (eq (plist-get info :with-tags) t)
		    (org-export-get-tags headline info))))
    (format "<a href=\"#%s\">%s</a>"
	    ;; Label.
	    (org-export-solidify-link-text
	     (or (org-element-property :CUSTOM_ID headline)
		 (concat "sec-"
			 (mapconcat #'number-to-string headline-number "-"))))
	    ;; Body.
	    (concat
	     (and (not (org-export-low-level-p headline info))
		  (org-export-numbered-headline-p headline info)
		  (concat (mapconcat #'number-to-string headline-number ".")
			  ". "))
	     (apply (if (not (eq org-html-format-headline-function 'ignore))
			(lambda (todo todo-type priority text tags &rest ignore)
			  (funcall org-html-format-headline-function
				   todo todo-type priority text tags))
		      #'org-html-format-headline)
		    todo todo-type priority text tags :section-number nil)))))

(defun org-html-list-of-listings (info)
  "Build a list of listings.
INFO is a plist used as a communication channel.  Return the list
of listings as a string, or nil if it is empty."
  (let ((lol-entries (org-export-collect-listings info)))
    (when lol-entries
      (concat "<div id=\"list-of-listings\">\n"
	      (format "<h%d>%s</h%d>\n"
		      org-html-toplevel-hlevel
		      (org-html--translate "List of Listings" info)
		      org-html-toplevel-hlevel)
	      "<div id=\"text-list-of-listings\">\n<ul>\n"
	      (let ((count 0)
		    (initial-fmt (format "<span class=\"listing-number\">%s</span>"
					 (org-html--translate "Listing %d:" info))))
		(mapconcat
		 (lambda (entry)
		   (let ((label (org-element-property :name entry))
			 (title (org-trim
				 (org-export-data
				  (or (org-export-get-caption entry t)
				      (org-export-get-caption entry))
				  info))))
		     (concat
		      "<li>"
		      (if (not label)
			  (concat (format initial-fmt (incf count)) " " title)
			(format "<a href=\"#%s\">%s %s</a>"
				(org-export-solidify-link-text label)
				(format initial-fmt (incf count))
				title))
		      "</li>")))
		 lol-entries "\n"))
	      "\n</ul>\n</div>\n</div>"))))

(defun org-html-list-of-tables (info)
  "Build a list of tables.
INFO is a plist used as a communication channel.  Return the list
of tables as a string, or nil if it is empty."
  (let ((lol-entries (org-export-collect-tables info)))
    (when lol-entries
      (concat "<div id=\"list-of-tables\">\n"
	      (format "<h%d>%s</h%d>\n"
		      org-html-toplevel-hlevel
		      (org-html--translate "List of Tables" info)
		      org-html-toplevel-hlevel)
	      "<div id=\"text-list-of-tables\">\n<ul>\n"
	      (let ((count 0)
		    (initial-fmt (format "<span class=\"table-number\">%s</span>"
					 (org-html--translate "Table %d:" info))))
		(mapconcat
		 (lambda (entry)
		   (let ((label (org-element-property :name entry))
			 (title (org-trim
				 (org-export-data
				  (or (org-export-get-caption entry t)
				      (org-export-get-caption entry))
				  info))))
		     (concat
		      "<li>"
		      (if (not label)
			  (concat (format initial-fmt (incf count)) " " title)
			(format "<a href=\"#%s\">%s %s</a>"
				(org-export-solidify-link-text label)
				(format initial-fmt (incf count))
				title))
		      "</li>")))
		 lol-entries "\n"))
	      "\n</ul>\n</div>\n</div>"))))


;;; Transcode Functions

;;;; Bold

(defun org-html-bold (bold contents info)
  "Transcode BOLD from Org to HTML.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format (or (cdr (assq 'bold org-html-text-markup-alist)) "%s")
	  contents))

;;;; Center Block

(defun org-html-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "<div class=\"center\">\n%s</div>" contents))

;;;; Clock

(defun org-html-clock (clock contents info)
  "Transcode a CLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "<p>
<span class=\"timestamp-wrapper\">
<span class=\"timestamp-kwd\">%s</span> <span class=\"timestamp\">%s</span>%s
</span>
</p>"
	  org-clock-string
	  (org-translate-time
	   (org-element-property :raw-value
				 (org-element-property :value clock)))
	  (let ((time (org-element-property :duration clock)))
	    (and time (format " <span class=\"timestamp\">(%s)</span>" time)))))

;;;; Code

(defun org-html-code (code contents info)
  "Transcode CODE from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format (or (cdr (assq 'code org-html-text-markup-alist)) "%s")
	  (org-html-encode-plain-text (org-element-property :value code))))

;;;; Drawer

(defun org-html-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (if (functionp org-html-format-drawer-function)
      (funcall org-html-format-drawer-function
	       (org-element-property :drawer-name drawer)
	       contents)
    ;; If there's no user defined function: simply
    ;; display contents of the drawer.
    contents))

;;;; Dynamic Block

(defun org-html-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  contents)

;;;; Entity

(defun org-html-entity (entity contents info)
  "Transcode an ENTITY object from Org to HTML.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (org-element-property :html entity))

;;;; Example Block

(defun org-html-example-block (example-block contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (if (org-export-read-attribute :attr_html example-block :textarea)
      (org-html--textarea-block example-block)
    (format "<pre class=\"example\">\n%s</pre>"
	    (org-html-format-code example-block info))))

;;;; Export Snippet

(defun org-html-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (when (eq (org-export-snippet-backend export-snippet) 'html)
    (org-element-property :value export-snippet)))

;;;; Export Block

(defun org-html-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "HTML")
    (org-remove-indentation (org-element-property :value export-block))))

;;;; Fixed Width

(defun org-html-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format "<pre class=\"example\">\n%s</pre>"
	  (org-html-do-format-code
	   (org-remove-indentation
	    (org-element-property :value fixed-width)))))

;;;; Footnote Reference

(defun org-html-footnote-reference (footnote-reference contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (eq (org-element-type prev) 'footnote-reference)
       org-html-footnote-separator))
   (cond
    ((not (org-export-footnote-first-reference-p footnote-reference info))
     (org-html-format-footnote-reference
      (org-export-get-footnote-number footnote-reference info)
      "IGNORED" 100))
    ;; Inline definitions are secondary strings.
    ((eq (org-element-property :type footnote-reference) 'inline)
     (org-html-format-footnote-reference
      (org-export-get-footnote-number footnote-reference info)
      "IGNORED" 1))
    ;; Non-inline footnotes definitions are full Org data.
    (t (org-html-format-footnote-reference
	(org-export-get-footnote-number footnote-reference info)
	"IGNORED" 1)))))

;;;; Headline

(defun org-html-format-headline--wrap
  (headline info &optional format-function &rest extra-keys)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((level (+ (org-export-get-relative-level headline info)
		   (1- org-html-toplevel-hlevel)))
	 (headline-number (org-export-get-headline-number headline info))
	 (section-number (and (not (org-export-low-level-p headline info))
			      (org-export-numbered-headline-p headline info)
			      (mapconcat 'number-to-string
					 headline-number ".")))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property :todo-keyword headline)))
		      (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (text (org-export-data (org-element-property :title headline) info))
	 (tags (and (plist-get info :with-tags)
		    (org-export-get-tags headline info)))
	 (headline-label (or (org-element-property :CUSTOM_ID headline)
			     (concat "sec-" (mapconcat 'number-to-string
						       headline-number "-"))))
	 (format-function
	  (cond ((functionp format-function) format-function)
		((not (eq org-html-format-headline-function 'ignore))
		 (lambda (todo todo-type priority text tags &rest ignore)
		   (funcall org-html-format-headline-function
			    todo todo-type priority text tags)))
		(t 'org-html-format-headline))))
    (apply format-function
	   todo todo-type  priority text tags
	   :headline-label headline-label :level level
	   :section-number section-number extra-keys)))

(defun org-html-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  ;; Empty contents?
  (setq contents (or contents ""))
  (let* ((numberedp (org-export-numbered-headline-p headline info))
	 (level (org-export-get-relative-level headline info))
	 (text (org-export-data (org-element-property :title headline) info))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property :todo-keyword headline)))
		      (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (tags (and (plist-get info :with-tags)
		    (org-export-get-tags headline info)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (section-number (and (org-export-numbered-headline-p headline info)
			      (mapconcat 'number-to-string
					 (org-export-get-headline-number
					  headline info) ".")))
	 ;; Create the headline text.
	 (full-text (org-html-format-headline--wrap headline info)))
    (cond
     ;; Case 1: This is a footnote section: ignore it.
     ((org-element-property :footnote-section-p headline) nil)
     ;; Case 2. This is a deep sub-tree: export it as a list item.
     ;;         Also export as items headlines for which no section
     ;;         format has been found.
     ((org-export-low-level-p headline info)
      ;; Build the real contents of the sub-tree.
      (let* ((type (if numberedp 'ordered 'unordered))
	     (itemized-body (org-html-format-list-item
			     contents type nil info nil full-text)))
	(concat
	 (and (org-export-first-sibling-p headline info)
	      (org-html-begin-plain-list type))
	 itemized-body
	 (and (org-export-last-sibling-p headline info)
	      (org-html-end-plain-list type)))))
     ;; Case 3. Standard headline.  Export it as a section.
     (t
      (let* ((section-number (mapconcat 'number-to-string
					(org-export-get-headline-number
					 headline info) "-"))
	     (ids (remove 'nil
			  (list (org-element-property :CUSTOM_ID headline)
				(concat "sec-" section-number)
				(org-element-property :ID headline))))
	     (preferred-id (car ids))
	     (extra-ids (cdr ids))
	     (extra-class (org-element-property :HTML_CONTAINER_CLASS headline))
	     (level1 (+ level (1- org-html-toplevel-hlevel)))
	     (first-content (car (org-element-contents headline))))
	(format "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n"
		(org-html--container headline info)
		(format "outline-container-%s"
			(or (org-element-property :CUSTOM_ID headline)
			    (concat "sec-" section-number)))
		(concat (format "outline-%d" level1) (and extra-class " ")
			extra-class)
		(format "\n<h%d id=\"%s\">%s%s</h%d>\n"
			level1
			preferred-id
			(mapconcat
			 (lambda (x)
			   (let ((id (org-export-solidify-link-text
				      (if (org-uuidgen-p x) (concat "ID-" x)
					x))))
			     (org-html--anchor id)))
			 extra-ids "")
			full-text
			level1)
		;; When there is no section, pretend there is an empty
		;; one to get the correct <div class="outline- ...>
		;; which is needed by `org-info.js'.
		(if (not (eq (org-element-type first-content) 'section))
		    (concat (org-html-section first-content "" info)
			    contents)
		  contents)
		(org-html--container headline info)))))))

(defun org-html--container (headline info)
  (or (org-element-property :HTML_CONTAINER headline)
      (if (= 1 (org-export-get-relative-level headline info))
	  (plist-get info :html-container)
	"div")))

;;;; Horizontal Rule

(defun org-html-horizontal-rule (horizontal-rule contents info)
  "Transcode an HORIZONTAL-RULE  object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-html-close-tag "hr" nil info))

;;;; Inline Src Block

(defun org-html-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((org-lang (org-element-property :language inline-src-block))
	 (code (org-element-property :value inline-src-block)))
    (error "Cannot export inline src block")))

;;;; Inlinetask

(defun org-html-format-section (text class &optional id)
  "Format a section with TEXT into a HTML div with CLASS and ID."
  (let ((extra (concat (when id (format " id=\"%s\"" id)))))
    (concat (format "<div class=\"%s\"%s>\n" class extra) text "</div>\n")))

(defun org-html-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (cond
   ;; If `org-html-format-inlinetask-function' is not 'ignore, call it
   ;; with appropriate arguments.
   ((not (eq org-html-format-inlinetask-function 'ignore))
    (let ((format-function
	   (function*
	    (lambda (todo todo-type priority text tags
		     &key contents &allow-other-keys)
	      (funcall org-html-format-inlinetask-function
		       todo todo-type priority text tags contents)))))
      (org-html-format-headline--wrap
       inlinetask info format-function :contents contents)))
   ;; Otherwise, use a default template.
   (t (format "<div class=\"inlinetask\">\n<b>%s</b>%s\n%s</div>"
	      (org-html-format-headline--wrap inlinetask info)
	      (org-html-close-tag "br" nil info)
	      contents))))

;;;; Italic

(defun org-html-italic (italic contents info)
  "Transcode ITALIC from Org to HTML.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (format (or (cdr (assq 'italic org-html-text-markup-alist)) "%s") contents))

;;;; Item

(defun org-html-checkbox (checkbox)
  "Format CHECKBOX into HTML."
  (case checkbox (on "<code>[X]</code>")
	(off "<code>[&#xa0;]</code>")
	(trans "<code>[-]</code>")
	(t "")))

(defun org-html-format-list-item (contents type checkbox info
					     &optional term-counter-id
					     headline)
  "Format a list item into HTML."
  (let ((checkbox (concat (org-html-checkbox checkbox) (and checkbox " ")))
	(br (org-html-close-tag "br" nil info)))
    (concat
     (case type
       (ordered
	(let* ((counter term-counter-id)
	       (extra (if counter (format " value=\"%s\"" counter) "")))
	  (concat
	   (format "<li%s>" extra)
	   (when headline (concat headline br)))))
       (unordered
	(let* ((id term-counter-id)
	       (extra (if id (format " id=\"%s\"" id) "")))
	  (concat
	   (format "<li%s>" extra)
	   (when headline (concat headline br)))))
       (descriptive
	(let* ((term term-counter-id))
	  (setq term (or term "(no term)"))
	  ;; Check-boxes in descriptive lists are associated to tag.
	  (concat (format "<dt> %s </dt>"
			  (concat checkbox term))
		  "<dd>"))))
     (unless (eq type 'descriptive) checkbox)
     contents
     (case type
       (ordered "</li>")
       (unordered "</li>")
       (descriptive "</dd>")))))

(defun org-html-item (item contents info)
  "Transcode an ITEM element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((plain-list (org-export-get-parent item))
	 (type (org-element-property :type plain-list))
	 (counter (org-element-property :counter item))
	 (checkbox (org-element-property :checkbox item))
	 (tag (let ((tag (org-element-property :tag item)))
		(and tag (org-export-data tag info)))))
    (org-html-format-list-item
     contents type checkbox info (or tag counter))))

;;;; Keyword

(defun org-html-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    (cond
     ((string= key "HTML") value)
     ((string= key "TOC")
      (let ((value (downcase value)))
	(cond
	 ((string-match "\\<headlines\\>" value)
	  (let ((depth (or (and (string-match "[0-9]+" value)
				(string-to-number (match-string 0 value)))
			   (plist-get info :with-toc))))
	    (org-html-toc depth info)))
	 ((string= "listings" value) (org-html-list-of-listings info))
	 ((string= "tables" value) (org-html-list-of-tables info))))))))

;;;; Latex Environment

(defun org-html-format-latex (latex-frag processing-type info)
  "Format a LaTeX fragment LATEX-FRAG into HTML.
PROCESSING-TYPE designates the tool used for conversion.  It is
a symbol among `mathjax', `dvipng', `imagemagick', `verbatim' nil
and t.  See `org-html-with-latex' for more information.  INFO is
a plist containing export properties."
  (let ((cache-relpath "") (cache-dir ""))
    (unless (eq processing-type 'mathjax)
      (let ((bfn (or (buffer-file-name)
		     (make-temp-name
		      (expand-file-name "latex" temporary-file-directory))))
	    (latex-header
	     (let ((header (plist-get info :latex-header)))
	       (and header
		    (concat (mapconcat
			     (lambda (line) (concat "#+LATEX_HEADER: " line))
			     (org-split-string header "\n")
			     "\n")
			    "\n")))))
	(setq cache-relpath
	      (concat "ltxpng/"
		      (file-name-sans-extension
		       (file-name-nondirectory bfn)))
	      cache-dir (file-name-directory bfn))
	;; Re-create LaTeX environment from original buffer in
	;; temporary buffer so that dvipng/imagemagick can properly
	;; turn the fragment into an image.
	(setq latex-frag (concat latex-header latex-frag))))
    (with-temp-buffer
      (insert latex-frag)
      (org-format-latex cache-relpath cache-dir nil "Creating LaTeX Image..."
			nil nil processing-type)
      (buffer-string))))

(defun org-html-latex-environment (latex-environment contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((processing-type (plist-get info :with-latex))
	(latex-frag (org-remove-indentation
		     (org-element-property :value latex-environment)))
	(attributes (org-export-read-attribute :attr_html latex-environment)))
    (case processing-type
      ((t mathjax)
       (org-html-format-latex latex-frag 'mathjax info))
      ((dvipng imagemagick)
       (let ((formula-link
	      (org-html-format-latex latex-frag processing-type info)))
	 (when (and formula-link (string-match "file:\\([^]]*\\)" formula-link))
	   ;; Do not provide a caption or a name to be consistent with
	   ;; `mathjax' handling.
	   (org-html--wrap-image
	    (org-html--format-image
	     (match-string 1 formula-link) attributes info) info))))
      (t latex-frag))))

;;;; Latex Fragment

(defun org-html-latex-fragment (latex-fragment contents info)
  "Transcode a LATEX-FRAGMENT object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((latex-frag (org-element-property :value latex-fragment))
	(processing-type (plist-get info :with-latex)))
    (case processing-type
      ((t mathjax)
       (org-html-format-latex latex-frag 'mathjax info))
      ((dvipng imagemagick)
       (let ((formula-link
	      (org-html-format-latex latex-frag processing-type info)))
	 (when (and formula-link (string-match "file:\\([^]]*\\)" formula-link))
	   (org-html--format-image (match-string 1 formula-link) nil info))))
      (t latex-frag))))

;;;; Line Break

(defun org-html-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat (org-html-close-tag "br" nil info) "\n"))

;;;; Link

(defun org-html-inline-image-p (link info)
  "Non-nil when LINK is meant to appear as an image.
INFO is a plist used as a communication channel.  LINK is an
inline image when it has no description and targets an image
file (see `org-html-inline-image-rules' for more information), or
if its description is a single link targeting an image file."
  (if (not (org-element-contents link))
      (org-export-inline-image-p link org-html-inline-image-rules)
    (not
     (let ((link-count 0))
       (org-element-map (org-element-contents link)
	   (cons 'plain-text org-element-all-objects)
	 (lambda (obj)
	   (case (org-element-type obj)
	     (plain-text (org-string-nw-p obj))
	     (link (if (= link-count 1) t
		     (incf link-count)
		     (not (org-export-inline-image-p
			   obj org-html-inline-image-rules))))
	     (otherwise t)))
         info t)))))

(defvar org-html-standalone-image-predicate)
(defun org-html-standalone-image-p (element info)
  "Test if ELEMENT is a standalone image.

INFO is a plist holding contextual information.

Return non-nil, if ELEMENT is of type paragraph and its sole
content, save for white spaces, is a link that qualifies as an
inline image.

Return non-nil, if ELEMENT is of type link and its containing
paragraph has no other content save white spaces.

Return nil, otherwise.

Bind `org-html-standalone-image-predicate' to constrain paragraph
further.  For example, to check for only captioned standalone
images, set it to:

  \(lambda (paragraph) (org-element-property :caption paragraph))"
  (let ((paragraph (case (org-element-type element)
		     (paragraph element)
		     (link (org-export-get-parent element)))))
    (and (eq (org-element-type paragraph) 'paragraph)
	 (or (not (and (boundp 'org-html-standalone-image-predicate)
		       (functionp org-html-standalone-image-predicate)))
	     (funcall org-html-standalone-image-predicate paragraph))
	 (not (let ((link-count 0))
		(org-element-map (org-element-contents paragraph)
		    (cons 'plain-text org-element-all-objects)
		  (lambda (obj) (case (org-element-type obj)
			     (plain-text (org-string-nw-p obj))
			     (link
			      (or (> (incf link-count) 1)
				  (not (org-html-inline-image-p obj info))))
			     (otherwise t)))
		  info 'first-match 'link))))))

(defun org-html-link (link desc info)
  "Transcode a LINK object from Org to HTML.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((home (when (plist-get info :html-link-home)
		 (org-trim (plist-get info :html-link-home))))
	 (use-abs-url (plist-get info :html-link-use-abs-url))
	 (link-org-files-as-html-maybe
	  (function
	   (lambda (raw-path info)
	     "Treat links to `file.org' as links to `file.html', if needed.
           See `org-html-link-org-files-as-html'."
	     (cond
	      ((and org-html-link-org-files-as-html
		    (string= ".org"
			     (downcase (file-name-extension raw-path "."))))
	       (concat (file-name-sans-extension raw-path) "."
		       (plist-get info :html-extension)))
	      (t raw-path)))))
	 (type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 ;; Ensure DESC really exists, or set it to nil.
	 (desc (org-string-nw-p desc))
	 (path
	  (cond
	   ((member type '("http" "https" "ftp" "mailto"))
	    (org-link-escape
	     (org-link-unescape
	      (concat type ":" raw-path)) org-link-escape-chars-browser))
	   ((string= type "file")
	    ;; Treat links to ".org" files as ".html", if needed.
	    (setq raw-path
		  (funcall link-org-files-as-html-maybe raw-path info))
	    ;; If file path is absolute, prepend it with protocol
	    ;; component - "file:".
	    (cond
	     ((file-name-absolute-p raw-path)
	      (setq raw-path (concat "file:" raw-path)))
	     ((and home use-abs-url)
	      (setq raw-path (concat (file-name-as-directory home) raw-path))))
	    ;; Add search option, if any.  A search option can be
	    ;; relative to a custom-id or a headline title.  Any other
	    ;; option is ignored.
	    (let ((option (org-element-property :search-option link)))
	      (cond ((not option) raw-path)
		    ((eq (aref option 0) ?#) (concat raw-path option))
		    ;; External fuzzy link: try to resolve it if path
		    ;; belongs to current project, if any.
		    ((eq (aref option 0) ?*)
		     (concat
		      raw-path
		      (let ((numbers
			     (org-publish-resolve-external-fuzzy-link
			      (org-element-property :path link) option)))
			(and numbers (concat "#sec-"
					     (mapconcat 'number-to-string
							numbers "-"))))))
		    (t raw-path))))
	   (t raw-path)))
	 ;; Extract attributes from parent's paragraph.  HACK: Only do
	 ;; this for the first link in parent (inner image link for
	 ;; inline images).  This is needed as long as attributes
	 ;; cannot be set on a per link basis.
	 (attributes-plist
	  (let* ((parent (org-export-get-parent-element link))
		 (link (let ((container (org-export-get-parent link)))
			 (if (and (eq (org-element-type container) 'link)
				  (org-html-inline-image-p link info))
			     container
			   link))))
	    (and (eq (org-element-map parent 'link 'identity info t) link)
		 (org-export-read-attribute :attr_html parent))))
	 (attributes
	  (let ((attr (org-html--make-attribute-string attributes-plist)))
	    (if (org-string-nw-p attr) (concat " " attr) "")))
	 protocol)
    (cond
     ;; Image file.
     ((and org-html-inline-images
	   (org-export-inline-image-p link org-html-inline-image-rules))
      (org-html--format-image path attributes-plist info))
     ;; Radio target: Transcode target's contents and use them as
     ;; link's description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
	(when destination
	  (format "<a href=\"#%s\"%s>%s</a>"
		  (org-export-solidify-link-text
		   (org-element-property :value destination))
		  attributes desc))))
     ;; Links pointing to a headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(case (org-element-type destination)
	  ;; ID link points to an external file.
	  (plain-text
	   (let ((fragment (concat "ID-" path))
		 ;; Treat links to ".org" files as ".html", if needed.
		 (path (funcall link-org-files-as-html-maybe
				destination info)))
	     (format "<a href=\"%s#%s\"%s>%s</a>"
		     path fragment attributes (or desc destination))))
	  ;; Fuzzy link points nowhere.
	  ((nil)
	   (format "<i>%s</i>"
		   (or desc
		       (org-export-data
			(org-element-property :raw-link link) info))))
	  ;; Link points to a headline.
	  (headline
	   (let ((href
		  ;; What href to use?
		  (cond
		   ;; Case 1: Headline is linked via it's CUSTOM_ID
		   ;; property.  Use CUSTOM_ID.
		   ((string= type "custom-id")
		    (org-element-property :CUSTOM_ID destination))
		   ;; Case 2: Headline is linked via it's ID property
		   ;; or through other means.  Use the default href.
		   ((member type '("id" "fuzzy"))
		    (format "sec-%s"
			    (mapconcat 'number-to-string
				       (org-export-get-headline-number
					destination info) "-")))
		   (t (error "Shouldn't reach here"))))
		 ;; What description to use?
		 (desc
		  ;; Case 1: Headline is numbered and LINK has no
		  ;; description.  Display section number.
		  (if (and (org-export-numbered-headline-p destination info)
			   (not desc))
		      (mapconcat 'number-to-string
				 (org-export-get-headline-number
				  destination info) ".")
		    ;; Case 2: Either the headline is un-numbered or
		    ;; LINK has a custom description.  Display LINK's
		    ;; description or headline's title.
		    (or desc (org-export-data (org-element-property
					       :title destination) info)))))
	     (format "<a href=\"#%s\"%s>%s</a>"
		     (org-export-solidify-link-text href) attributes desc)))
	  ;; Fuzzy link points to a target or an element.
	  (t
	   (let* ((path (org-export-solidify-link-text path))
		  (org-html-standalone-image-predicate 'org-html--has-caption-p)
		  (number (cond
			   (desc nil)
			   ((org-html-standalone-image-p destination info)
			    (org-export-get-ordinal
			     (org-element-map destination 'link
			       'identity info t)
			     info 'link 'org-html-standalone-image-p))
			   (t (org-export-get-ordinal
			       destination info nil 'org-html--has-caption-p))))
		  (desc (cond (desc)
			      ((not number) "No description for this link")
			      ((numberp number) (number-to-string number))
			      (t (mapconcat 'number-to-string number ".")))))
	     (format "<a href=\"#%s\"%s>%s</a>" path attributes desc))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (let ((fragment (concat "coderef-" path)))
	(format "<a href=\"#%s\"%s%s>%s</a>"
		fragment
		(org-trim
		 (format (concat "class=\"coderef\""
				 " onmouseover=\"CodeHighlightOn(this, '%s');\""
				 " onmouseout=\"CodeHighlightOff(this, '%s');\"")
			 fragment fragment))
		attributes
		(format (org-export-get-coderef-format path desc)
			(org-export-resolve-coderef path info)))))
     ;; Link type is handled by a special function.
     ((functionp (setq protocol (nth 2 (assoc type org-link-protocols))))
      (funcall protocol (org-link-unescape path) desc 'html))
     ;; External link with a description part.
     ((and path desc) (format "<a href=\"%s\"%s>%s</a>" path attributes desc))
     ;; External link without a description part.
     (path (format "<a href=\"%s\"%s>%s</a>" path attributes path))
     ;; No path, only description.  Try to do something useful.
     (t (format "<i>%s</i>" desc)))))

;;;; Paragraph

(defun org-html-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let* ((parent (org-export-get-parent paragraph))
	 (parent-type (org-element-type parent))
	 (style '((footnote-definition " class=\"footpara\"")))
	 (extra (or (cadr (assoc parent-type style)) "")))
    (cond
     ((and (eq (org-element-type parent) 'item)
	   (= (org-element-property :begin paragraph)
	      (org-element-property :contents-begin parent)))
      ;; Leading paragraph in a list item have no tags.
      contents)
     ((org-html-standalone-image-p paragraph info)
      ;; Standalone image.
      (let ((caption
	     (let ((raw (org-export-data
			 (org-export-get-caption paragraph) info))
		   (org-html-standalone-image-predicate
		    'org-html--has-caption-p))
	       (if (not (org-string-nw-p raw)) raw
		 (concat
                  "<span class=\"figure-number\">"
		  (format (org-html--translate "Figure %d:" info)
			  (org-export-get-ordinal
			   (org-element-map paragraph 'link
			     'identity info t)
			   info nil 'org-html-standalone-image-p))
		  "</span> " raw))))
	    (label (org-element-property :name paragraph)))
	(org-html--wrap-image contents info caption label)))
     ;; Regular paragraph.
     (t (format "<p%s>\n%s</p>" extra contents)))))

;;;; Plain List

;; FIXME Maybe arg1 is not needed because <li value="20"> already sets
;; the correct value for the item counter
(defun org-html-begin-plain-list (type &optional arg1)
  "Insert the beginning of the HTML list depending on TYPE.
When ARG1 is a string, use it as the start parameter for ordered
lists."
  (case type
    (ordered
     (format "<ol class=\"org-ol\"%s>"
	     (if arg1 (format " start=\"%d\"" arg1) "")))
    (unordered "<ul class=\"org-ul\">")
    (descriptive "<dl class=\"org-dl\">")))

(defun org-html-end-plain-list (type)
  "Insert the end of the HTML list depending on TYPE."
  (case type
    (ordered "</ol>")
    (unordered "</ul>")
    (descriptive "</dl>")))

(defun org-html-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to HTML.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* (arg1 ;; (assoc :counter (org-element-map plain-list 'item
	 (type (org-element-property :type plain-list)))
    (format "%s\n%s%s"
	    (org-html-begin-plain-list type)
	    contents (org-html-end-plain-list type))))

;;;; Plain Text

(defun org-html-convert-special-strings (string)
  "Convert special characters in STRING to HTML."
  (let ((all org-html-special-string-regexps)
	e a re rpl start)
    (while (setq a (pop all))
      (setq re (car a) rpl (cdr a) start 0)
      (while (string-match re string start)
	(setq string (replace-match rpl t nil string))))
    string))

(defun org-html-encode-plain-text (text)
  "Convert plain text characters from TEXT to HTML equivalent.
Possible conversions are set in `org-html-protect-char-alist'."
  (mapc
   (lambda (pair)
     (setq text (replace-regexp-in-string (car pair) (cdr pair) text t t)))
   org-html-protect-char-alist)
  text)

(defun org-html-plain-text (text info)
  "Transcode a TEXT string from Org to HTML.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (let ((output text))
    ;; Protect following characters: <, >, &.
    (setq output (org-html-encode-plain-text output))
    ;; Handle smart quotes.  Be sure to provide original string since
    ;; OUTPUT may have been modified.
    (when (plist-get info :with-smart-quotes)
      (setq output (org-export-activate-smart-quotes output :html info text)))
    ;; Handle special strings.
    (when (plist-get info :with-special-strings)
      (setq output (org-html-convert-special-strings output)))
    ;; Handle break preservation if required.
    (when (plist-get info :preserve-breaks)
      (setq output
	    (replace-regexp-in-string
	     "\\(\\\\\\\\\\)?[ \t]*\n"
	     (concat (org-html-close-tag "br" nil info) "\n") output)))
    ;; Return value.
    output))


;; Planning

(defun org-html-planning (planning contents info)
  "Transcode a PLANNING element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((span-fmt "<span class=\"timestamp-kwd\">%s</span> <span class=\"timestamp\">%s</span>"))
    (format
     "<p><span class=\"timestamp-wrapper\">%s</span></p>"
     (mapconcat
      'identity
      (delq nil
	    (list
	     (let ((closed (org-element-property :closed planning)))
	       (when closed
		 (format span-fmt org-closed-string
			 (org-translate-time
			  (org-element-property :raw-value closed)))))
	     (let ((deadline (org-element-property :deadline planning)))
	       (when deadline
		 (format span-fmt org-deadline-string
			 (org-translate-time
			  (org-element-property :raw-value deadline)))))
	     (let ((scheduled (org-element-property :scheduled planning)))
	       (when scheduled
		 (format span-fmt org-scheduled-string
			 (org-translate-time
			  (org-element-property :raw-value scheduled)))))))
      " "))))

;;;; Property Drawer

(defun org-html-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  ;; The property drawer isn't exported but we want separating blank
  ;; lines nonetheless.
  "")

;;;; Quote Block

(defun org-html-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "<blockquote>\n%s</blockquote>" contents))

;;;; Quote Section

(defun org-html-quote-section (quote-section contents info)
  "Transcode a QUOTE-SECTION element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-remove-indentation
		(org-element-property :value quote-section))))
    (when value (format "<pre>\n%s</pre>" value))))

;;;; Section

(defun org-html-section (section contents info)
  "Transcode a SECTION element from Org to HTML.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (let ((parent (org-export-get-parent-headline section)))
    ;; Before first headline: no container, just return CONTENTS.
    (if (not parent) contents
      ;; Get div's class and id references.
      (let* ((class-num (+ (org-export-get-relative-level parent info)
			   (1- org-html-toplevel-hlevel)))
	     (section-number
	      (mapconcat
	       'number-to-string
	       (org-export-get-headline-number parent info) "-")))
        ;; Build return value.
	(format "<div class=\"outline-text-%d\" id=\"text-%s\">\n%s</div>"
		class-num
		(or (org-element-property :CUSTOM_ID parent) section-number)
		contents)))))

;;;; Radio Target

(defun org-html-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to HTML.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (let ((id (org-export-solidify-link-text
	     (org-element-property :value radio-target))))
    (org-html--anchor id text)))

;;;; Special Block

(defun org-html-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((block-type (downcase
		      (org-element-property :type special-block)))
	 (contents (or contents ""))
	 (html5-fancy (and (org-html-html5-p info)
			   (plist-get info :html-html5-fancy)
			   (member block-type org-html-html5-elements)))
	 (attributes (org-export-read-attribute :attr_html special-block)))
    (unless html5-fancy
      (let ((class (plist-get attributes :class)))
	(setq attributes (plist-put attributes :class
				    (if class (concat class " " block-type)
				      block-type)))))
    (setq attributes (org-html--make-attribute-string attributes))
    (when (not (equal attributes ""))
      (setq attributes (concat " " attributes)))
    (if html5-fancy
	(format "<%s%s>\n%s</%s>" block-type attributes
		contents block-type)
      (format "<div%s>\n%s\n</div>" attributes contents))))

;;;; Src Block

(defun org-html-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let ((lang (org-element-property :language src-block))
	  (caption (org-export-get-caption src-block))
	  (code (org-html-format-code src-block info))
	  (label (let ((lbl (org-element-property :name src-block)))
		   (if (not lbl) ""
		     (format " id=\"%s\""
			     (org-export-solidify-link-text lbl))))))
      (if (not lang) (format "<pre class=\"example\"%s>\n%s</pre>" label code)
	(format
	 "<div class=\"org-src-container\">\n%s%s\n</div>"
	 (if (not caption) ""
	   (format "<label class=\"org-src-name\">%s</label>"
		   (org-export-data caption info)))
	 (format "\n<pre class=\"src src-%s\"%s>%s</pre>" lang label code))))))

;;;; Statistics Cookie

(defun org-html-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((cookie-value (org-element-property :value statistics-cookie)))
    (format "<code>%s</code>" cookie-value)))

;;;; Strike-Through

(defun org-html-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to HTML.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (format (or (cdr (assq 'strike-through org-html-text-markup-alist)) "%s")
	  contents))

;;;; Subscript

(defun org-html-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to HTML.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "<sub>%s</sub>" contents))

;;;; Superscript

(defun org-html-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to HTML.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "<sup>%s</sup>" contents))

;;;; Table Cell

(defun org-html-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((table-row (org-export-get-parent table-cell))
	 (table (org-export-get-parent-table table-cell))
	 (cell-attrs
	  (if (not org-html-table-align-individual-fields) ""
	    (format (if (and (boundp 'org-html-format-table-no-css)
			     org-html-format-table-no-css)
			" align=\"%s\"" " class=\"%s\"")
		    (org-export-table-cell-alignment table-cell info)))))
    (when (or (not contents) (string= "" (org-trim contents)))
      (setq contents "&#xa0;"))
    (cond
     ((and (org-export-table-has-header-p table info)
	   (= 1 (org-export-table-row-group table-row info)))
      (concat "\n" (format (car org-html-table-header-tags) "col" cell-attrs)
	      contents (cdr org-html-table-header-tags)))
     ((and org-html-table-use-header-tags-for-first-column
	   (zerop (cdr (org-export-table-cell-address table-cell info))))
      (concat "\n" (format (car org-html-table-header-tags) "row" cell-attrs)
	      contents (cdr org-html-table-header-tags)))
     (t (concat "\n" (format (car org-html-table-data-tags) cell-attrs)
		contents (cdr org-html-table-data-tags))))))

;;;; Table Row

(defun org-html-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to HTML.
CONTENTS is the contents of the row.  INFO is a plist used as a
communication channel."
  ;; Rules are ignored since table separators are deduced from
  ;; borders of the current row.
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((rowgroup-number (org-export-table-row-group table-row info))
	   (row-number (org-export-table-row-number table-row info))
	   (start-rowgroup-p
	    (org-export-table-row-starts-rowgroup-p table-row info))
	   (end-rowgroup-p
	    (org-export-table-row-ends-rowgroup-p table-row info))
	   ;; `top-row-p' and `end-rowgroup-p' are not used directly
	   ;; but should be set so that `org-html-table-row-tags' can
	   ;; use them (see the docstring of this variable.)
	   (top-row-p (and (equal start-rowgroup-p '(top))
			   (equal end-rowgroup-p '(below top))))
	   (bottom-row-p (and (equal start-rowgroup-p '(above))
			      (equal end-rowgroup-p '(bottom above))))
	   (rowgroup-tags
	    (cond
	     ;; Case 1: Row belongs to second or subsequent rowgroups.
	     ((not (= 1 rowgroup-number))
	      '("<tbody>" . "\n</tbody>"))
	     ;; Case 2: Row is from first rowgroup.  Table has >=1 rowgroups.
	     ((org-export-table-has-header-p
	       (org-export-get-parent-table table-row) info)
	      '("<thead>" . "\n</thead>"))
	     ;; Case 2: Row is from first and only row group.
	     (t '("<tbody>" . "\n</tbody>")))))
      (concat
       ;; Begin a rowgroup?
       (when start-rowgroup-p (car rowgroup-tags))
       ;; Actual table row
       (concat "\n" (eval (car org-html-table-row-tags))
	       contents
	       "\n"
	       (eval (cdr org-html-table-row-tags)))
       ;; End a rowgroup?
       (when end-rowgroup-p (cdr rowgroup-tags))))))

;;;; Table

(defun org-html-table-first-row-data-cells (table info)
  "Transcode the first row of TABLE.
INFO is a plist used as a communication channel."
  (let ((table-row
	 (org-element-map table 'table-row
	   (lambda (row)
	     (unless (eq (org-element-property :type row) 'rule) row))
	   info 'first-match))
	(special-column-p (org-export-table-has-special-column-p table)))
    (if (not special-column-p) (org-element-contents table-row)
      (cdr (org-element-contents table-row)))))

(defun org-html-table--table.el-table (table info)
  "Format table.el tables into HTML.
INFO is a plist used as a communication channel."
  (when (eq (org-element-property :type table) 'table.el)
    (require 'table)
    (let ((outbuf (with-current-buffer
		      (get-buffer-create "*org-export-table*")
		    (erase-buffer) (current-buffer))))
      (with-temp-buffer
	(insert (org-element-property :value table))
	(goto-char 1)
	(re-search-forward "^[ \t]*|[^|]" nil t)
	(table-generate-source 'html outbuf))
      (with-current-buffer outbuf
	(prog1 (org-trim (buffer-string))
	  (kill-buffer) )))))

(defun org-html-table (table contents info)
  "Transcode a TABLE element from Org to HTML.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (case (org-element-property :type table)
    ;; Case 1: table.el table.  Convert it using appropriate tools.
    (table.el (org-html-table--table.el-table table info))
    ;; Case 2: Standard table.
    (t
     (let* ((label (org-element-property :name table))
	    (caption (org-export-get-caption table))
	    (number (org-export-get-ordinal
		     table info nil 'org-html--has-caption-p))
	    (attributes
	     (org-html--make-attribute-string
	      (org-combine-plists
	       (and label (list :id (org-export-solidify-link-text label)))
	       (and (not (org-html-html5-p info))
		    (plist-get info :html-table-attributes))
	       (org-export-read-attribute :attr_html table))))
	    (alignspec
	     (if (and (boundp 'org-html-format-table-no-css)
		      org-html-format-table-no-css)
		 "align=\"%s\"" "class=\"%s\""))
	    (table-column-specs
	     (function
	      (lambda (table info)
		(mapconcat
		 (lambda (table-cell)
		   (let ((alignment (org-export-table-cell-alignment
				     table-cell info)))
		     (concat
		      ;; Begin a colgroup?
		      (when (org-export-table-cell-starts-colgroup-p
			     table-cell info)
			"\n<colgroup>")
		      ;; Add a column.  Also specify it's alignment.
		      (format "\n%s"
			      (org-html-close-tag
			       "col" (concat " " (format alignspec alignment)) info))
		      ;; End a colgroup?
		      (when (org-export-table-cell-ends-colgroup-p
			     table-cell info)
			"\n</colgroup>"))))
		 (org-html-table-first-row-data-cells table info) "\n")))))
       (format "<table%s>\n%s\n%s\n%s</table>"
	       (if (equal attributes "") "" (concat " " attributes))
	       (if (not caption) ""
		 (format (if org-html-table-caption-above
			     "<caption class=\"t-above\">%s</caption>"
			   "<caption class=\"t-bottom\">%s</caption>")
			 (concat
			  "<span class=\"table-number\">"
                          (format (org-html--translate "Table %d:" info) number)
			  "</span> " (org-export-data caption info))))
	       (funcall table-column-specs table info)
	       contents)))))

;;;; Target

(defun org-html-target (target contents info)
  "Transcode a TARGET object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((id (org-export-solidify-link-text
	     (org-element-property :value target))))
    (org-html--anchor id)))

;;;; Timestamp

(defun org-html-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((value (org-html-plain-text
		(org-timestamp-translate timestamp) info)))
    (format "<span class=\"timestamp-wrapper\"><span class=\"timestamp\">%s</span></span>"
	    (replace-regexp-in-string "--" "&#x2013;" value))))

;;;; Underline

(defun org-html-underline (underline contents info)
  "Transcode UNDERLINE from Org to HTML.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (format (or (cdr (assq 'underline org-html-text-markup-alist)) "%s")
	  contents))

;;;; Verbatim

(defun org-html-verbatim (verbatim contents info)
  "Transcode VERBATIM from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format (or (cdr (assq 'verbatim org-html-text-markup-alist)) "%s")
	  (org-html-encode-plain-text (org-element-property :value verbatim))))

;;;; Verse Block

(defun org-html-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to HTML.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  ;; Replace each newline character with line break.  Also replace
  ;; each blank line with a line break.
  (setq contents (replace-regexp-in-string
		  "^ *\\\\\\\\$" (format "%s\n" (org-html-close-tag "br" nil info))
		  (replace-regexp-in-string
		   "\\(\\\\\\\\\\)?[ \t]*\n"
		   (format "%s\n" (org-html-close-tag "br" nil info)) contents)))
  ;; Replace each white space at beginning of a line with a
  ;; non-breaking space.
  (while (string-match "^[ \t]+" contents)
    (let* ((num-ws (length (match-string 0 contents)))
	   (ws (let (out) (dotimes (i num-ws out)
			    (setq out (concat out "&#xa0;"))))))
      (setq contents (replace-match ws nil t contents))))
  (format "<p class=\"verse\">\n%s</p>" contents))


;;; Filter Functions

(defun org-html-final-function (contents backend info)
  "Filter to indent the HTML and convert HTML entities."
  (with-temp-buffer
    (insert contents)
    (set-auto-mode t)
    (if org-html-indent
	(indent-region (point-min) (point-max)))
    (when org-html-use-unicode-chars
      (require 'mm-url)
      (mm-url-decode-entities))
    (buffer-substring-no-properties (point-min) (point-max))))


;;; End-user functions

;;;###autoload
(defun org-html-export-as-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer.

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
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'html "*Org HTML Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun org-html-convert-region-to-html ()
  "Assume the current region has org-mode syntax, and convert it to HTML.
This can be used in any buffer.  For example, you can write an
itemized list in org-mode syntax in an HTML buffer and use this
command to convert it."
  (interactive)
  (org-export-replace-region-by 'html))

;;;###autoload
(defun org-html-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file.

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
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension (concat "." org-html-extension))
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'html file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'html filename
		      (concat "." (or (plist-get plist :html-extension)
				      org-html-extension "html"))
		      plist pub-dir))


(provide 'ox-html)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-html.el ends here
