;;; org.el --- Outline-based notes management and organizer -*- lexical-binding: t; -*-

;; Carstens outline-mode for keeping track of everything.
;; Copyright (C) 2004-2017 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Maintainer: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Org is a mode for keeping notes, maintaining ToDo lists, and doing
;; project planning with a fast and effective plain-text system.
;;
;; Org mode develops organizational tasks around NOTES files that
;; contain information about projects as plain text.  Org mode is
;; implemented on top of outline-mode, which makes it possible to keep
;; the content of large files well structured.  Visibility cycling and
;; structure editing help to work with the tree.  Tables are easily
;; created with a built-in table editor.  Org mode supports ToDo
;; items, deadlines, time stamps, and scheduling.  It dynamically
;; compiles entries into an agenda that utilizes and smoothly
;; integrates much of the Emacs calendar and diary.  Plain text
;; URL-like links connect to websites, emails, Usenet messages, BBDB
;; entries, and any files related to the projects.  For printing and
;; sharing of notes, an Org file can be exported as a structured ASCII
;; file, as HTML, or (todo and agenda items only) as an iCalendar
;; file.  It can also serve as a publishing tool for a set of linked
;; webpages.
;;
;; Installation and Activation
;; ---------------------------
;; See the corresponding sections in the manual at
;;
;;   http://orgmode.org/org.html#Installation
;;
;; Documentation
;; -------------
;; The documentation of Org mode can be found in the TeXInfo file.  The
;; distribution also contains a PDF version of it.  At the homepage of
;; Org mode, you can read the same text online as HTML.  There is also an
;; excellent reference card made by Philip Rooke.  This card can be found
;; in the doc/ directory.
;;
;; A list of recent changes can be found at
;; http://orgmode.org/Changes.html
;;
;;; Code:

(defvar org-inhibit-highlight-removal nil) ; dynamically scoped param
(defvar-local org-table-formula-constants-local nil
  "Local version of `org-table-formula-constants'.")

;;;; Require other packages

(require 'cl-lib)

(eval-when-compile (require 'gnus-sum))

(require 'calendar)
(require 'find-func)
(require 'format-spec)

(or (eq this-command 'eval-buffer)
    (condition-case nil
	(load (concat (file-name-directory load-file-name)
		      "org-loaddefs.el")
	      nil t t t)
      (error
       (message "WARNING: No org-loaddefs.el file could be found from where org.el is loaded.")
       (sit-for 3)
       (message "You need to run \"make\" or \"make autoloads\" from Org lisp directory")
       (sit-for 3))))

(require 'org-macs)
(require 'org-compat)

;; `org-outline-regexp' ought to be a defconst but is let-bound in
;; some places -- e.g. see the macro `org-with-limited-levels'.
;;
;; In Org buffers, the value of `outline-regexp' is that of
;; `org-outline-regexp'.  The only function still directly relying on
;; `outline-regexp' is `org-overview' so that `org-cycle' can do its
;; job when `orgstruct-mode' is active.
(defvar org-outline-regexp "\\*+ "
  "Regexp to match Org headlines.")

(defvar org-outline-regexp-bol "^\\*+ "
  "Regexp to match Org headlines.
This is similar to `org-outline-regexp' but additionally makes
sure that we are at the beginning of the line.")

(defvar org-heading-regexp "^\\(\\*+\\)\\(?: +\\(.*?\\)\\)?[ \t]*$"
  "Matches a headline, putting stars and text into groups.
Stars are put in group 1 and the trimmed body in group 2.")

(declare-function calendar-check-holidays "holidays" (date))
(declare-function cdlatex-environment "ext:cdlatex" (environment item))
(declare-function isearch-no-upper-case-p "isearch" (string regexp-flag))
(declare-function org-add-archive-files "org-archive" (files))
(declare-function org-agenda-entry-get-agenda-timestamp "org-agenda" (pom))
(declare-function org-agenda-list "org-agenda" (&optional arg start-day span with-hour))
(declare-function org-agenda-redo "org-agenda" (&optional all))
(declare-function org-babel-do-in-edit-buffer "ob-core" (&rest body) t)
(declare-function org-babel-tangle-file "ob-tangle" (file &optional target-file lang))
(declare-function org-beamer-mode "ox-beamer" (&optional prefix) t)
(declare-function org-clock-get-last-clock-out-time "org-clock" ())
(declare-function org-clock-out "org-clock" (&optional switch-to-state fail-quietly at-time))
(declare-function org-clock-remove-overlays "org-clock" (&optional beg end noremove))
(declare-function org-clock-sum "org-clock" (&optional tstart tend headline-filter propname))
(declare-function org-clock-sum-current-item "org-clock" (&optional tstart))
(declare-function org-clock-timestamps-down "org-clock" (&optional n))
(declare-function org-clock-timestamps-up "org-clock" (&optional n))
(declare-function org-clock-update-time-maybe "org-clock" ())
(declare-function org-clocking-buffer "org-clock" ())
(declare-function org-clocktable-shift "org-clock" (dir n))
(declare-function
 org-duration-from-minutes "org-duration" (minutes &optional fmt canonical))
(declare-function org-element-at-point "org-element" ())
(declare-function org-element-cache-refresh "org-element" (pos))
(declare-function org-element-cache-reset "org-element" (&optional all))
(declare-function org-element-contents "org-element" (element))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-copy "org-element" (datum))
(declare-function org-element-interpret-data "org-element" (data))
(declare-function org-element-lineage "org-element" (blob &optional types with-self))
(declare-function org-element-link-parser "org-element" ())
(declare-function org-element-nested-p "org-element" (elem-a elem-b))
(declare-function org-element-parse-buffer "org-element" (&optional granularity visible-only))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-put-property "org-element" (element property value))
(declare-function org-element-swap-A-B "org-element" (elem-a elem-b))
(declare-function org-element-type "org-element" (element))
(declare-function org-element-update-syntax "org-element" ())
(declare-function org-id-find-id-file "org-id" (id))
(declare-function org-id-get-create "org-id" (&optional force))
(declare-function org-inlinetask-at-task-p "org-inlinetask" ())
(declare-function org-inlinetask-outline-regexp "org-inlinetask" ())
(declare-function org-inlinetask-toggle-visibility "org-inlinetask" ())
(declare-function org-plot/gnuplot "org-plot" (&optional params))
(declare-function org-table-align "org-table" ())
(declare-function org-table-begin "org-table" (&optional table-type))
(declare-function org-table-beginning-of-field "org-table" (&optional n))
(declare-function org-table-blank-field "org-table" ())
(declare-function org-table-calc-current-TBLFM "org-table" (&optional arg))
(declare-function org-table-copy-region "org-table" (beg end &optional cut))
(declare-function org-table-cut-region "org-table" (beg end))
(declare-function org-table-edit-field "org-table" (arg))
(declare-function org-table-end "org-table" (&optional table-type))
(declare-function org-table-end-of-field "org-table" (&optional n))
(declare-function org-table-insert-row "org-table" (&optional arg))
(declare-function org-table-justify-field-maybe "org-table" (&optional new))
(declare-function org-table-maybe-eval-formula "org-table" ())
(declare-function org-table-maybe-recalculate-line "org-table" ())
(declare-function org-table-next-row "org-table" ())
(declare-function org-table-paste-rectangle "org-table" ())
(declare-function org-table-recalculate "org-table" (&optional all noalign))
(declare-function
 org-table-sort-lines "org-table"
 (&optional with-case sorting-type getkey-func compare-func interactive?))
(declare-function org-table-wrap-region "org-table" (arg))
(declare-function org-tags-view "org-agenda" (&optional todo-only match))
(declare-function orgtbl-ascii-plot "org-table" (&optional ask))
(declare-function orgtbl-mode "org-table" (&optional arg))
(declare-function org-export-get-backend "ox" (name))
(declare-function org-export-get-environment "ox" (&optional backend subtreep ext-plist))
(declare-function org-latex-make-preamble "ox-latex" (info &optional template snippet?))

(defvar ffap-url-regexp)		;Silence byte-compiler

(defsubst org-uniquify (list)
  "Non-destructively remove duplicate elements from LIST."
  (let ((res (copy-sequence list))) (delete-dups res)))

(defsubst org-get-at-bol (property)
  "Get text property PROPERTY at the beginning of line."
  (get-text-property (point-at-bol) property))

(defsubst org-trim (s &optional keep-lead)
  "Remove whitespace at the beginning and the end of string S.
When optional argument KEEP-LEAD is non-nil, removing blank lines
at the beginning of the string does not affect leading indentation."
  (replace-regexp-in-string
   (if keep-lead "\\`\\([ \t]*\n\\)+" "\\`[ \t\n\r]+") ""
   (replace-regexp-in-string "[ \t\n\r]+\\'" "" s)))

;; load languages based on value of `org-babel-load-languages'
(defvar org-babel-load-languages)

;;;###autoload
(defun org-babel-do-load-languages (sym value)
  "Load the languages defined in `org-babel-load-languages'."
  (set-default sym value)
  (dolist (pair org-babel-load-languages)
    (let ((active (cdr pair)) (lang (symbol-name (car pair))))
      (if active
	  (require (intern (concat "ob-" lang)))
	(funcall 'fmakunbound
		 (intern (concat "org-babel-execute:" lang)))
	(funcall 'fmakunbound
		 (intern (concat "org-babel-expand-body:" lang)))))))

(declare-function org-babel-tangle-file "ob-tangle" (file &optional target-file lang))
;;;###autoload
(defun org-babel-load-file (file &optional compile)
  "Load Emacs Lisp source code blocks in the Org FILE.
This function exports the source code using `org-babel-tangle'
and then loads the resulting file using `load-file'.  With prefix
arg (noninteractively: 2nd arg) COMPILE the tangled Emacs Lisp
file to byte-code before it is loaded."
  (interactive "fFile to load: \nP")
  (let* ((age (lambda (file)
		(float-time
		 (time-subtract (current-time)
				(nth 5 (or (file-attributes (file-truename file))
					   (file-attributes file)))))))
	 (base-name (file-name-sans-extension file))
	 (exported-file (concat base-name ".el")))
    ;; tangle if the Org file is newer than the elisp file
    (unless (and (file-exists-p exported-file)
		 (> (funcall age file) (funcall age exported-file)))
      ;; Tangle-file traversal returns reversed list of tangled files
      ;; and we want to evaluate the first target.
      (setq exported-file
	    (car (last (org-babel-tangle-file file exported-file "emacs-lisp")))))
    (message "%s %s"
	     (if compile
		 (progn (byte-compile-file exported-file 'load)
			"Compiled and loaded")
	       (progn (load-file exported-file) "Loaded"))
	     exported-file)))

(defcustom org-babel-load-languages '((emacs-lisp . t))
  "Languages which can be evaluated in Org buffers.
This list can be used to load support for any of the languages
below, note that each language will depend on a different set of
system executables and/or Emacs modes.  When a language is
\"loaded\", then code blocks in that language can be evaluated
with `org-babel-execute-src-block' bound by default to C-c
C-c (note the `org-babel-no-eval-on-ctrl-c-ctrl-c' variable can
be set to remove code block evaluation from the C-c C-c
keybinding.  By default only Emacs Lisp (which has no
requirements) is loaded."
  :group 'org-babel
  :set 'org-babel-do-load-languages
  :version "24.1"
  :type '(alist :tag "Babel Languages"
		:key-type
		(choice
		 (const :tag "Awk" awk)
		 (const :tag "C" C)
		 (const :tag "R" R)
		 (const :tag "Asymptote" asymptote)
		 (const :tag "Calc" calc)
		 (const :tag "Clojure" clojure)
		 (const :tag "CSS" css)
		 (const :tag "Ditaa" ditaa)
		 (const :tag "Dot" dot)
		 (const :tag "Ebnf2ps" ebnf2ps)
		 (const :tag "Emacs Lisp" emacs-lisp)
		 (const :tag "Forth" forth)
		 (const :tag "Fortran" fortran)
		 (const :tag "Gnuplot" gnuplot)
		 (const :tag "Haskell" haskell)
		 (const :tag "hledger" hledger)
		 (const :tag "IO" io)
		 (const :tag "J" J)
		 (const :tag "Java" java)
		 (const :tag "Javascript" js)
		 (const :tag "LaTeX" latex)
		 (const :tag "Ledger" ledger)
		 (const :tag "Lilypond" lilypond)
		 (const :tag "Lisp" lisp)
		 (const :tag "Makefile" makefile)
		 (const :tag "Maxima" maxima)
		 (const :tag "Matlab" matlab)
		 (const :tag "Mscgen" mscgen)
		 (const :tag "Ocaml" ocaml)
		 (const :tag "Octave" octave)
		 (const :tag "Org" org)
		 (const :tag "Perl" perl)
		 (const :tag "Pico Lisp" picolisp)
		 (const :tag "PlantUML" plantuml)
		 (const :tag "Python" python)
		 (const :tag "Ruby" ruby)
		 (const :tag "Sass" sass)
		 (const :tag "Scala" scala)
		 (const :tag "Scheme" scheme)
		 (const :tag "Screen" screen)
		 (const :tag "Shell Script" shell)
		 (const :tag "Shen" shen)
		 (const :tag "Sql" sql)
		 (const :tag "Sqlite" sqlite)
		 (const :tag "Stan" stan)
		 (const :tag "Vala" vala))
		:value-type (boolean :tag "Activate" :value t)))

;;;; Customization variables
(defcustom org-clone-delete-id nil
  "Remove ID property of clones of a subtree.
When non-nil, clones of a subtree don't inherit the ID property.
Otherwise they inherit the ID property with a new unique
identifier."
  :type 'boolean
  :version "24.1"
  :group 'org-id)

;;; Version
(org-check-version)

;;;###autoload
(defun org-version (&optional here full message)
  "Show the Org version.
Interactively, or when MESSAGE is non-nil, show it in echo area.
With prefix argument, or when HERE is non-nil, insert it at point.
In non-interactive uses, a reduced version string is output unless
FULL is given."
  (interactive (list current-prefix-arg t (not current-prefix-arg)))
  (let ((org-dir (ignore-errors (org-find-library-dir "org")))
	(save-load-suffixes (when (boundp 'load-suffixes) load-suffixes))
	(load-suffixes (list ".el"))
	(org-install-dir
	 (ignore-errors (org-find-library-dir "org-loaddefs"))))
    (unless (and (fboundp 'org-release) (fboundp 'org-git-version))
      (org-load-noerror-mustsuffix (concat org-dir "org-version")))
    (let* ((load-suffixes save-load-suffixes)
	   (release (org-release))
	   (git-version (org-git-version))
	   (version (format "Org mode version %s (%s @ %s)"
			    release
			    git-version
			    (if org-install-dir
				(if (string= org-dir org-install-dir)
				    org-install-dir
				  (concat "mixed installation! "
					  org-install-dir
					  " and "
					  org-dir))
			      "org-loaddefs.el can not be found!")))
	   (version1 (if full version release)))
      (when here (insert version1))
      (when message (message "%s" version1))
      version1)))

(defconst org-version (org-version))


;;; Syntax Constants

;;;; Block

(defconst org-block-regexp
  "^[ \t]*#\\+begin_?\\([^ \n]+\\)\\(\\([^\n]+\\)\\)?\n\\([^\000]+?\\)#\\+end_?\\1[ \t]*$"
  "Regular expression for hiding blocks.")

(defconst org-dblock-start-re
  "^[ \t]*#\\+\\(?:BEGIN\\|begin\\):[ \t]+\\(\\S-+\\)\\([ \t]+\\(.*\\)\\)?"
  "Matches the start line of a dynamic block, with parameters.")

(defconst org-dblock-end-re "^[ \t]*#\\+\\(?:END\\|end\\)\\([: \t\r\n]\\|$\\)"
  "Matches the end of a dynamic block.")

;;;; Clock and Planning

(defconst org-clock-string "CLOCK:"
  "String used as prefix for timestamps clocking work hours on an item.")

(defvar org-closed-string "CLOSED:"
  "String used as the prefix for timestamps logging closing a TODO entry.")

(defvar org-deadline-string "DEADLINE:"
  "String to mark deadline entries.
\\<org-mode-map>
A deadline is this string, followed by a time stamp.  It must be
a word, terminated by a colon.  You can insert a schedule keyword
and a timestamp with `\\[org-deadline]'.")

(defvar org-scheduled-string "SCHEDULED:"
  "String to mark scheduled TODO entries.
\\<org-mode-map>
A schedule is this string, followed by a time stamp.  It must be
a word, terminated by a colon.  You can insert a schedule keyword
and a timestamp with `\\[org-schedule]'.")

(defconst org-ds-keyword-length
  (+ 2
     (apply #'max
	    (mapcar #'length
		    (list org-deadline-string org-scheduled-string
			  org-clock-string org-closed-string))))
  "Maximum length of the DEADLINE and SCHEDULED keywords.")

(defconst org-planning-line-re
  (concat "^[ \t]*"
	  (regexp-opt
	   (list org-closed-string org-deadline-string org-scheduled-string)
	   t))
  "Matches a line with planning info.
Matched keyword is in group 1.")

(defconst org-clock-line-re
  (concat "^[ \t]*" org-clock-string)
  "Matches a line with clock info.")

(defconst org-deadline-regexp (concat "\\<" org-deadline-string)
  "Matches the DEADLINE keyword.")

(defconst org-deadline-time-regexp
  (concat "\\<" org-deadline-string " *<\\([^>]+\\)>")
  "Matches the DEADLINE keyword together with a time stamp.")

(defconst org-deadline-time-hour-regexp
  (concat "\\<" org-deadline-string
	  " *<\\([^>]+[0-9]\\{1,2\\}:[0-9]\\{2\\}[0-9-+:hdwmy \t.]*\\)>")
  "Matches the DEADLINE keyword together with a time-and-hour stamp.")

(defconst org-deadline-line-regexp
  (concat "\\<\\(" org-deadline-string "\\).*")
  "Matches the DEADLINE keyword and the rest of the line.")

(defconst org-scheduled-regexp (concat "\\<" org-scheduled-string)
  "Matches the SCHEDULED keyword.")

(defconst org-scheduled-time-regexp
  (concat "\\<" org-scheduled-string " *<\\([^>]+\\)>")
  "Matches the SCHEDULED keyword together with a time stamp.")

(defconst org-scheduled-time-hour-regexp
  (concat "\\<" org-scheduled-string
	  " *<\\([^>]+[0-9]\\{1,2\\}:[0-9]\\{2\\}[0-9-+:hdwmy \t.]*\\)>")
  "Matches the SCHEDULED keyword together with a time-and-hour stamp.")

(defconst org-closed-time-regexp
  (concat "\\<" org-closed-string " *\\[\\([^]]+\\)\\]")
  "Matches the CLOSED keyword together with a time stamp.")

(defconst org-keyword-time-regexp
  (concat "\\<"
	  (regexp-opt
	   (list org-scheduled-string org-deadline-string org-closed-string
		 org-clock-string)
	   t)
	  " *[[<]\\([^]>]+\\)[]>]")
  "Matches any of the 4 keywords, together with the time stamp.")

(defconst org-keyword-time-not-clock-regexp
  (concat
   "\\<"
   (regexp-opt
    (list org-scheduled-string org-deadline-string org-closed-string) t)
   " *[[<]\\([^]>]+\\)[]>]")
  "Matches any of the 3 keywords, together with the time stamp.")

(defconst org-maybe-keyword-time-regexp
  (concat "\\(\\<"
	  (regexp-opt
	   (list org-scheduled-string org-deadline-string org-closed-string
		 org-clock-string)
	   t)
	  "\\)?"
	  " *\\([[<][0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ?[^]\r\n>]*?[]>]"
	  "\\|"
	  "<%%([^\r\n>]*>\\)")
  "Matches a timestamp, possibly preceded by a keyword.")

(defconst org-all-time-keywords
  (mapcar (lambda (w) (substring w 0 -1))
	  (list org-scheduled-string org-deadline-string
		org-clock-string org-closed-string))
  "List of time keywords.")

;;;; Drawer

(defconst org-drawer-regexp "^[ \t]*:\\(\\(?:\\w\\|[-_]\\)+\\):[ \t]*$"
  "Matches first or last line of a hidden block.
Group 1 contains drawer's name or \"END\".")

(defconst org-property-start-re "^[ \t]*:PROPERTIES:[ \t]*$"
  "Regular expression matching the first line of a property drawer.")

(defconst org-property-end-re "^[ \t]*:END:[ \t]*$"
  "Regular expression matching the last line of a property drawer.")

(defconst org-clock-drawer-start-re "^[ \t]*:CLOCK:[ \t]*$"
  "Regular expression matching the first line of a clock drawer.")

(defconst org-clock-drawer-end-re "^[ \t]*:END:[ \t]*$"
  "Regular expression matching the last line of a clock drawer.")

(defconst org-property-drawer-re
  (concat "^[ \t]*:PROPERTIES:[ \t]*\n"
	  "\\(?:[ \t]*:\\S-+:\\(?: .*\\)?[ \t]*\n\\)*?"
	  "[ \t]*:END:[ \t]*$")
  "Matches an entire property drawer.")

(defconst org-clock-drawer-re
  (concat "\\(" org-clock-drawer-start-re "\\)[^\000]*?\\("
	  org-clock-drawer-end-re "\\)\n?")
  "Matches an entire clock drawer.")

;;;; Headline

(defconst org-heading-keyword-regexp-format
  "^\\(\\*+\\)\\(?: +%s\\)\\(?: +\\(.*?\\)\\)?[ \t]*$"
  "Printf format for a regexp matching a headline with some keyword.
This regexp will match the headline of any node which has the
exact keyword that is put into the format.  The keyword isn't in
any group by default, but the stars and the body are.")

(defconst org-heading-keyword-maybe-regexp-format
  "^\\(\\*+\\)\\(?: +%s\\)?\\(?: +\\(.*?\\)\\)?[ \t]*$"
  "Printf format for a regexp matching a headline, possibly with some keyword.
This regexp can match any headline with the specified keyword, or
without a keyword.  The keyword isn't in any group by default,
but the stars and the body are.")

(defconst org-archive-tag "ARCHIVE"
  "The tag that marks a subtree as archived.
An archived subtree does not open during visibility cycling, and does
not contribute to the agenda listings.")

(eval-and-compile
  (defconst org-comment-string "COMMENT"
    "Entries starting with this keyword will never be exported.
\\<org-mode-map>
An entry can be toggled between COMMENT and normal with
`\\[org-toggle-comment]'."))


;;;; LaTeX Environments and Fragments

(defconst org-latex-regexps
  '(("begin" "^[ \t]*\\(\\\\begin{\\([a-zA-Z0-9\\*]+\\)[^\000]+?\\\\end{\\2}\\)" 1 t)
    ;; ("$" "\\([ \t(]\\|^\\)\\(\\(\\([$]\\)\\([^ \t\n,.$].*?\\(\n.*?\\)\\{0,5\\}[^ \t\n,.$]\\)\\4\\)\\)\\([ \t.,?;:'\")]\\|$\\)" 2 nil)
    ;; \000 in the following regex is needed for org-inside-LaTeX-fragment-p
    ("$1" "\\([^$]\\|^\\)\\(\\$[^ \t\r\n,;.$]\\$\\)\\(\\s.\\|\\s-\\|\\s(\\|\\s)\\|\\s\"\\|\000\\|'\\|$\\)" 2 nil)
    ("$"  "\\([^$]\\|^\\)\\(\\(\\$\\([^ \t\n,;.$][^$\n\r]*?\\(\n[^$\n\r]*?\\)\\{0,2\\}[^ \t\n,.$]\\)\\$\\)\\)\\(\\s.\\|\\s-\\|\\s(\\|\\s)\\|\\s\"\\|\000\\|'\\|$\\)" 2 nil)
    ("\\(" "\\\\([^\000]*?\\\\)" 0 nil)
    ("\\[" "\\\\\\[[^\000]*?\\\\\\]" 0 nil)
    ("$$" "\\$\\$[^\000]*?\\$\\$" 0 nil))
  "Regular expressions for matching embedded LaTeX.")

;;;; Node Property

(defconst org-effort-property "Effort"
  "The property that is being used to keep track of effort estimates.
Effort estimates given in this property need to have the format H:MM.")

;;;; Table

(defconst org-table-any-line-regexp "^[ \t]*\\(|\\|\\+-[-+]\\)"
  "Detect an org-type or table-type table.")

(defconst org-table-line-regexp "^[ \t]*|"
  "Detect an org-type table line.")

(defconst org-table-dataline-regexp "^[ \t]*|[^-]"
  "Detect an org-type table line.")

(defconst org-table-hline-regexp "^[ \t]*|-"
  "Detect an org-type table hline.")

(defconst org-table1-hline-regexp "^[ \t]*\\+-[-+]"
  "Detect a table-type table hline.")

(defconst org-table-any-border-regexp "^[ \t]*[^|+ \t]"
  "Detect the first line outside a table when searching from within it.
This works for both table types.")

(defconst org-TBLFM-regexp "^[ \t]*#\\+TBLFM: "
  "Detect a #+TBLFM line.")

;;;; Timestamp

(defconst org-ts-regexp "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ?[^\r\n>]*?\\)>"
  "Regular expression for fast time stamp matching.")

(defconst org-ts-regexp-inactive
  "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ?[^\r\n>]*?\\)\\]"
  "Regular expression for fast inactive time stamp matching.")

(defconst org-ts-regexp-both "[[<]\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ?[^]\r\n>]*?\\)[]>]"
  "Regular expression for fast time stamp matching.")

(defconst org-ts-regexp0
  "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\( +[^]+0-9>\r\n -]+\\)?\\( +\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\)"
  "Regular expression matching time strings for analysis.
This one does not require the space after the date, so it can be used
on a string that terminates immediately after the date.")

(defconst org-ts-regexp1 "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) *\\([^]+0-9>\r\n -]*\\)\\( \\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\)"
  "Regular expression matching time strings for analysis.")

(defconst org-ts-regexp2 (concat "<" org-ts-regexp1 "[^>\n]\\{0,16\\}>")
  "Regular expression matching time stamps, with groups.")

(defconst org-ts-regexp3 (concat "[[<]" org-ts-regexp1 "[^]>\n]\\{0,16\\}[]>]")
  "Regular expression matching time stamps (also [..]), with groups.")

(defconst org-tr-regexp (concat org-ts-regexp "--?-?" org-ts-regexp)
  "Regular expression matching a time stamp range.")

(defconst org-tr-regexp-both
  (concat org-ts-regexp-both "--?-?" org-ts-regexp-both)
  "Regular expression matching a time stamp range.")

(defconst org-tsr-regexp (concat org-ts-regexp "\\(--?-?"
				 org-ts-regexp "\\)?")
  "Regular expression matching a time stamp or time stamp range.")

(defconst org-tsr-regexp-both
  (concat org-ts-regexp-both "\\(--?-?"
	  org-ts-regexp-both "\\)?")
  "Regular expression matching a time stamp or time stamp range.
The time stamps may be either active or inactive.")

(defconst org-repeat-re
  "<[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [^>\n]*?\\([.+]?\\+[0-9]+[hdwmy]\\(/[0-9]+[hdwmy]\\)?\\)"
  "Regular expression for specifying repeated events.
After a match, group 1 contains the repeat expression.")

(defconst org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>")
  "Formats for `format-time-string' which are used for time stamps.")


;;; The custom variables

(defgroup org nil
  "Outline-based notes management and organizer."
  :tag "Org"
  :group 'outlines
  :group 'calendar)

(defcustom org-mode-hook nil
  "Mode hook for Org mode, run after the mode was turned on."
  :group 'org
  :type 'hook)

(defcustom org-load-hook nil
  "Hook that is run after org.el has been loaded."
  :group 'org
  :type 'hook)

(defcustom org-log-buffer-setup-hook nil
  "Hook that is run after an Org log buffer is created."
  :group 'org
  :version "24.1"
  :type 'hook)

(defvar org-modules)  ; defined below
(defvar org-modules-loaded nil
  "Have the modules been loaded already?")

(defun org-load-modules-maybe (&optional force)
  "Load all extensions listed in `org-modules'."
  (when (or force (not org-modules-loaded))
    (dolist (ext org-modules)
      (condition-case nil (require ext)
	(error (message "Problems while trying to load feature `%s'" ext))))
    (setq org-modules-loaded t)))

(defun org-set-modules (var value)
  "Set VAR to VALUE and call `org-load-modules-maybe' with the force flag."
  (set var value)
  (when (featurep 'org)
    (org-load-modules-maybe 'force)
    (org-element-cache-reset 'all)))

(defcustom org-modules '(org-w3m org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail)
  "Modules that should always be loaded together with org.el.

If a description starts with <C>, the file is not part of Emacs
and loading it will require that you have downloaded and properly
installed the Org mode distribution.

You can also use this system to load external packages (i.e. neither Org
core modules, nor modules from the CONTRIB directory).  Just add symbols
to the end of the list.  If the package is called org-xyz.el, then you need
to add the symbol `xyz', and the package must have a call to:

   (provide \\='org-xyz)

For export specific modules, see also `org-export-backends'."
  :group 'org
  :set 'org-set-modules
  :version "24.4"
  :package-version '(Org . "8.0")
  :type
  '(set :greedy t
	(const :tag "   bbdb:              Links to BBDB entries" org-bbdb)
	(const :tag "   bibtex:            Links to BibTeX entries" org-bibtex)
	(const :tag "   crypt:             Encryption of subtrees" org-crypt)
	(const :tag "   ctags:             Access to Emacs tags with links" org-ctags)
	(const :tag "   docview:           Links to doc-view buffers" org-docview)
	(const :tag "   eww:               Store link to url of eww" org-eww)
	(const :tag "   gnus:              Links to GNUS folders/messages" org-gnus)
	(const :tag "   habit:             Track your consistency with habits" org-habit)
	(const :tag "   id:                Global IDs for identifying entries" org-id)
	(const :tag "   info:              Links to Info nodes" org-info)
	(const :tag "   inlinetask:        Tasks independent of outline hierarchy" org-inlinetask)
	(const :tag "   irc:               Links to IRC/ERC chat sessions" org-irc)
	(const :tag "   mhe:               Links to MHE folders/messages" org-mhe)
	(const :tag "   mouse:             Additional mouse support" org-mouse)
	(const :tag "   protocol:          Intercept calls from emacsclient" org-protocol)
	(const :tag "   rmail:             Links to RMAIL folders/messages" org-rmail)
	(const :tag "   w3m:               Special cut/paste from w3m to Org mode." org-w3m)

	(const :tag "C  annotate-file:     Annotate a file with org syntax" org-annotate-file)
	(const :tag "C  bookmark:          Org links to bookmarks" org-bookmark)
	(const :tag "C  checklist:         Extra functions for checklists in repeated tasks" org-checklist)
	(const :tag "C  choose:            Use TODO keywords to mark decisions states" org-choose)
	(const :tag "C  collector:         Collect properties into tables" org-collector)
	(const :tag "C  depend:            TODO dependencies for Org mode\n\t\t\t(PARTIALLY OBSOLETE, see built-in dependency support))" org-depend)
	(const :tag "C  drill:             Flashcards and spaced repetition for Org mode" org-drill)
	(const :tag "C  elisp-symbol:      Org links to emacs-lisp symbols" org-elisp-symbol)
	(const :tag "C  eshell             Support for links to working directories in eshell" org-eshell)
	(const :tag "C  eval-light:        Evaluate inbuffer-code on demand" org-eval-light)
	(const :tag "C  eval:              Include command output as text" org-eval)
	(const :tag "C  expiry:            Expiry mechanism for Org entries" org-expiry)
	(const :tag "C  favtable:          Lookup table of favorite references and links" org-favtable)
	(const :tag "C  git-link:          Provide org links to specific file version" org-git-link)
	(const :tag "C  interactive-query: Interactive modification of tags query\n\t\t\t(PARTIALLY OBSOLETE, see secondary filtering)" org-interactive-query)
        (const :tag "C  invoice:           Help manage client invoices in Org mode" org-invoice)
	(const :tag "C  learn:             SuperMemo's incremental learning algorithm" org-learn)
	(const :tag "C  mac-iCal           Imports events from iCal.app to the Emacs diary" org-mac-iCal)
	(const :tag "C  mac-link:          Grab links and url from various mac Applications" org-mac-link)
	(const :tag "C  mairix:            Hook mairix search into Org for different MUAs" org-mairix)
	(const :tag "C  man:               Support for links to manpages in Org mode" org-man)
	(const :tag "C  mew:               Links to Mew folders/messages" org-mew)
	(const :tag "C  mtags:             Support for muse-like tags" org-mtags)
	(const :tag "C  notmuch:           Provide org links to notmuch searches or messages" org-notmuch)
	(const :tag "C  panel:             Simple routines for us with bad memory" org-panel)
	(const :tag "C  registry:          A registry for Org links" org-registry)
	(const :tag "C  screen:            Visit screen sessions through Org links" org-screen)
	(const :tag "C  secretary:         Team management with org-mode" org-secretary)
	(const :tag "C  sqlinsert:         Convert Org tables to SQL insertions" orgtbl-sqlinsert)
	(const :tag "C  toc:               Table of contents for Org buffer" org-toc)
	(const :tag "C  track:             Keep up with Org mode development" org-track)
	(const :tag "C  velocity           Something like Notational Velocity for Org" org-velocity)
	(const :tag "C  vm:                Links to VM folders/messages" org-vm)
	(const :tag "C  wikinodes:         CamelCase wiki-like links" org-wikinodes)
	(const :tag "C  wl:                Links to Wanderlust folders/messages" org-wl)
	(repeat :tag "External packages" :inline t (symbol :tag "Package"))))

(defvar org-export-registered-backends) ; From ox.el.
(declare-function org-export-derived-backend-p "ox" (backend &rest backends))
(declare-function org-export-backend-name "ox" (backend) t)
(defcustom org-export-backends '(ascii html icalendar latex odt)
  "List of export back-ends that should be always available.

If a description starts with <C>, the file is not part of Emacs
and loading it will require that you have downloaded and properly
installed the Org mode distribution.

Unlike to `org-modules', libraries in this list will not be
loaded along with Org, but only once the export framework is
needed.

This variable needs to be set before org.el is loaded.  If you
need to make a change while Emacs is running, use the customize
interface or run the following code, where VAL stands for the new
value of the variable, after updating it:

  (progn
    (setq org-export-registered-backends
          (cl-remove-if-not
           (lambda (backend)
             (let ((name (org-export-backend-name backend)))
               (or (memq name val)
                   (catch \\='parentp
                     (dolist (b val)
                       (and (org-export-derived-backend-p b name)
                            (throw \\='parentp t)))))))
           org-export-registered-backends))
    (let ((new-list (mapcar #\\='org-export-backend-name
                            org-export-registered-backends)))
      (dolist (backend val)
        (cond
         ((not (load (format \"ox-%s\" backend) t t))
          (message \"Problems while trying to load export back-end \\=`%s\\='\"
                   backend))
         ((not (memq backend new-list)) (push backend new-list))))
      (set-default \\='org-export-backends new-list)))

Adding a back-end to this list will also pull the back-end it
depends on, if any."
  :group 'org
  :group 'org-export
  :version "26.1"
  :package-version '(Org . "9.0")
  :initialize 'custom-initialize-set
  :set (lambda (var val)
	 (if (not (featurep 'ox)) (set-default var val)
	   ;; Any back-end not required anymore (not present in VAL and not
	   ;; a parent of any back-end in the new value) is removed from the
	   ;; list of registered back-ends.
	   (setq org-export-registered-backends
		 (cl-remove-if-not
		  (lambda (backend)
		    (let ((name (org-export-backend-name backend)))
		      (or (memq name val)
			  (catch 'parentp
			    (dolist (b val)
			      (and (org-export-derived-backend-p b name)
				   (throw 'parentp t)))))))
		  org-export-registered-backends))
	   ;; Now build NEW-LIST of both new back-ends and required
	   ;; parents.
	   (let ((new-list (mapcar #'org-export-backend-name
				   org-export-registered-backends)))
	     (dolist (backend val)
	       (cond
		((not (load (format "ox-%s" backend) t t))
		 (message "Problems while trying to load export back-end `%s'"
			  backend))
		((not (memq backend new-list)) (push backend new-list))))
	     ;; Set VAR to that list with fixed dependencies.
	     (set-default var new-list))))
  :type '(set :greedy t
	      (const :tag "   ascii       Export buffer to ASCII format" ascii)
	      (const :tag "   beamer      Export buffer to Beamer presentation" beamer)
	      (const :tag "   html        Export buffer to HTML format" html)
	      (const :tag "   icalendar   Export buffer to iCalendar format" icalendar)
	      (const :tag "   latex       Export buffer to LaTeX format" latex)
	      (const :tag "   man         Export buffer to MAN format" man)
	      (const :tag "   md          Export buffer to Markdown format" md)
	      (const :tag "   odt         Export buffer to ODT format" odt)
	      (const :tag "   org         Export buffer to Org format" org)
	      (const :tag "   texinfo     Export buffer to Texinfo format" texinfo)
	      (const :tag "C  confluence  Export buffer to Confluence Wiki format" confluence)
	      (const :tag "C  deck        Export buffer to deck.js presentations" deck)
	      (const :tag "C  freemind    Export buffer to Freemind mindmap format" freemind)
	      (const :tag "C  groff       Export buffer to Groff format" groff)
	      (const :tag "C  koma-letter Export buffer to KOMA Scrlttrl2 format" koma-letter)
	      (const :tag "C  RSS 2.0     Export buffer to RSS 2.0 format" rss)
	      (const :tag "C  s5          Export buffer to s5 presentations" s5)
	      (const :tag "C  taskjuggler Export buffer to TaskJuggler format" taskjuggler)))

(eval-after-load 'ox
  '(dolist (backend org-export-backends)
     (condition-case nil (require (intern (format "ox-%s" backend)))
       (error (message "Problems while trying to load export back-end `%s'"
		       backend)))))

(defcustom org-support-shift-select nil
  "Non-nil means make shift-cursor commands select text when possible.
\\<org-mode-map>\

In Emacs 23, when `shift-select-mode' is on, shifted cursor keys
start selecting a region, or enlarge regions started in this way.
In Org mode, in special contexts, these same keys are used for
other purposes, important enough to compete with shift selection.
Org tries to balance these needs by supporting `shift-select-mode'
outside these special contexts, under control of this variable.

The default of this variable is nil, to avoid confusing behavior.  Shifted
cursor keys will then execute Org commands in the following contexts:
- on a headline, changing TODO state (left/right) and priority (up/down)
- on a time stamp, changing the time
- in a plain list item, changing the bullet type
- in a property definition line, switching between allowed values
- in the BEGIN line of a clock table (changing the time block).
Outside these contexts, the commands will throw an error.

When this variable is t and the cursor is not in a special
context, Org mode will support shift-selection for making and
enlarging regions.  To make this more effective, the bullet
cycling will no longer happen anywhere in an item line, but only
if the cursor is exactly on the bullet.

If you set this variable to the symbol `always', then the keys
will not be special in headlines, property lines, and item lines,
to make shift selection work there as well.  If this is what you
want, you can use the following alternative commands:
`\\[org-todo]' and `\\[org-priority]' \
to change TODO state and priority,
`\\[universal-argument] \\[universal-argument] \\[org-todo]' \
can be used to switch TODO sets,
`\\[org-ctrl-c-minus]' to cycle item bullet types,
and properties can be edited by hand or in column view.

However, when the cursor is on a timestamp, shift-cursor commands
will still edit the time stamp - this is just too good to give up."
  :group 'org
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "When outside special context" t)
	  (const :tag "Everywhere except timestamps" always)))

(defcustom org-loop-over-headlines-in-active-region nil
  "Shall some commands act upon headlines in the active region?

When set to t, some commands will be performed in all headlines
within the active region.

When set to `start-level', some commands will be performed in all
headlines within the active region, provided that these headlines
are of the same level than the first one.

When set to a string, those commands will be performed on the
matching headlines within the active region.  Such string must be
a tags/property/todo match as it is used in the agenda tags view.

The list of commands is: `org-schedule', `org-deadline',
`org-todo', `org-archive-subtree', `org-archive-set-tag' and
`org-archive-to-archive-sibling'.  The archiving commands skip
already archived entries."
  :type '(choice (const :tag "Don't loop" nil)
		 (const :tag "All headlines in active region" t)
		 (const :tag "In active region, headlines at the same level than the first one" start-level)
		 (string :tag "Tags/Property/Todo matcher"))
  :version "24.1"
  :group 'org-todo
  :group 'org-archive)

(defgroup org-startup nil
  "Options concerning startup of Org mode."
  :tag "Org Startup"
  :group 'org)

(defcustom org-startup-folded t
  "Non-nil means entering Org mode will switch to OVERVIEW.

This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: fold              (or `overview', this is equivalent)
   #+STARTUP: nofold            (or `showall', this is equivalent)
   #+STARTUP: content
   #+STARTUP: showeverything

Set `org-agenda-inhibit-startup' to a non-nil value if you want
to ignore this option when Org opens agenda files for the first
time."
  :group 'org-startup
  :type '(choice
	  (const :tag "nofold: show all" nil)
	  (const :tag "fold: overview" t)
	  (const :tag "content: all headlines" content)
	  (const :tag "show everything, even drawers" showeverything)))

(defcustom org-startup-truncated t
  "Non-nil means entering Org mode will set `truncate-lines'.
This is useful since some lines containing links can be very long and
uninteresting.  Also tables look terrible when wrapped.

The variable `org-startup-truncated' allows to configure
truncation for Org mode different to the other modes that use the
variable `truncate-lines' and as a shortcut instead of putting
the variable `truncate-lines' into the `org-mode-hook'.  If one
wants to configure truncation for Org mode not statically but
dynamically e. g. in a hook like `ediff-prepare-buffer-hook' then
the variable `truncate-lines' has to be used because in such a
case it is too late to set the variable `org-startup-truncated'."
  :group 'org-startup
  :type 'boolean)

(defcustom org-startup-indented nil
  "Non-nil means turn on `org-indent-mode' on startup.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: indent
   #+STARTUP: noindent"
  :group 'org-structure
  :type '(choice
	  (const :tag "Not" nil)
	  (const :tag "Globally (slow on startup in large files)" t)))

(defcustom org-use-sub-superscripts t
  "Non-nil means interpret \"_\" and \"^\" for display.

If you want to control how Org exports those characters, see
`org-export-with-sub-superscripts'.  `org-use-sub-superscripts'
used to be an alias for `org-export-with-sub-superscripts' in
Org <8.0, it is not anymore.

When this option is turned on, you can use TeX-like syntax for
sub- and superscripts within the buffer.  Several characters after
\"_\" or \"^\" will be considered as a single item - so grouping
with {} is normally not needed.  For example, the following things
will be parsed as single sub- or superscripts:

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
  :group 'org-startup
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Always interpret" t)
	  (const :tag "Only with braces" {})
	  (const :tag "Never interpret" nil)))

(defcustom org-startup-with-beamer-mode nil
  "Non-nil means turn on `org-beamer-mode' on startup.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: beamer"
  :group 'org-startup
  :version "24.1"
  :type 'boolean)

(defcustom org-startup-align-all-tables nil
  "Non-nil means align all tables when visiting a file.
This is useful when the column width in tables is forced with <N> cookies
in table fields.  Such tables will look correct only after the first re-align.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:
   #+STARTUP: align
   #+STARTUP: noalign"
  :group 'org-startup
  :type 'boolean)

(defcustom org-startup-with-inline-images nil
  "Non-nil means show inline images when loading a new Org file.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:
   #+STARTUP: inlineimages
   #+STARTUP: noinlineimages"
  :group 'org-startup
  :version "24.1"
  :type 'boolean)

(defcustom org-startup-with-latex-preview nil
  "Non-nil means preview LaTeX fragments when loading a new Org file.

This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:
   #+STARTUP: latexpreview
   #+STARTUP: nolatexpreview"
  :group 'org-startup
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-insert-mode-line-in-empty-file nil
  "Non-nil means insert the first line setting Org mode in empty files.
When the function `org-mode' is called interactively in an empty file, this
normally means that the file name does not automatically trigger Org mode.
To ensure that the file will always be in Org mode in the future, a
line enforcing Org mode will be inserted into the buffer, if this option
has been set."
  :group 'org-startup
  :type 'boolean)

(defcustom org-replace-disputed-keys nil
  "Non-nil means use alternative key bindings for some keys.
Org mode uses S-<cursor> keys for changing timestamps and priorities.
These keys are also used by other packages like shift-selection-mode'
\(built into Emacs 23), `CUA-mode' or `windmove.el'.
If you want to use Org mode together with one of these other modes,
or more generally if you would like to move some Org mode commands to
other keys, set this variable and configure the keys with the variable
`org-disputed-keys'.

This option is only relevant at load-time of Org mode, and must be set
*before* org.el is loaded.  Changing it requires a restart of Emacs to
become effective."
  :group 'org-startup
  :type 'boolean)

(defcustom org-use-extra-keys nil
  "Non-nil means use extra key sequence definitions for certain commands.
This happens automatically if `window-system' is nil.  This
variable lets you do the same manually.  You must set it before
loading Org."
  :group 'org-startup
  :type 'boolean)

(defvaralias 'org-CUA-compatible 'org-replace-disputed-keys)

(defcustom org-disputed-keys
  '(([(shift up)]		. [(meta p)])
    ([(shift down)]		. [(meta n)])
    ([(shift left)]		. [(meta -)])
    ([(shift right)]		. [(meta +)])
    ([(control shift right)] 	. [(meta shift +)])
    ([(control shift left)]	. [(meta shift -)]))
  "Keys for which Org mode and other modes compete.
This is an alist, cars are the default keys, second element specifies
the alternative to use when `org-replace-disputed-keys' is t.

Keys can be specified in any syntax supported by `define-key'.
The value of this option takes effect only at Org mode startup,
therefore you'll have to restart Emacs to apply it after changing."
  :group 'org-startup
  :type 'alist)

(defun org-key (key)
  "Select key according to `org-replace-disputed-keys' and `org-disputed-keys'.
Or return the original if not disputed."
  (when org-replace-disputed-keys
    (let* ((nkey (key-description key))
	   (x (cl-find-if (lambda (x) (equal (key-description (car x)) nkey))
			  org-disputed-keys)))
      (setq key (if x (cdr x) key))))
  key)

(defun org-defkey (keymap key def)
  "Define a key, possibly translated, as returned by `org-key'."
  (define-key keymap (org-key key) def))

(defcustom org-ellipsis nil
  "The ellipsis to use in the Org mode outline.

When nil, just use the standard three dots.  When a non-empty string,
use that string instead.

The change affects only Org mode (which will then use its own display table).
Changing this requires executing `\\[org-mode]' in a buffer to become
effective."
  :group 'org-startup
  :type '(choice (const :tag "Default" nil)
		 (string :tag "String" :value "...#"))
  :safe (lambda (v) (and (string-or-null-p v) (not (equal "" v)))))

(defvar org-display-table nil
  "The display table for Org mode, in case `org-ellipsis' is non-nil.")

(defgroup org-keywords nil
  "Keywords in Org mode."
  :tag "Org Keywords"
  :group 'org)

(defcustom org-closed-keep-when-no-todo nil
  "Remove CLOSED: time-stamp when switching back to a non-todo state?"
  :group 'org-todo
  :group 'org-keywords
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defgroup org-structure nil
  "Options concerning the general structure of Org files."
  :tag "Org Structure"
  :group 'org)

(defgroup org-reveal-location nil
  "Options about how to make context of a location visible."
  :tag "Org Reveal Location"
  :group 'org-structure)

(defcustom org-show-context-detail '((agenda . local)
				     (bookmark-jump . lineage)
				     (isearch . lineage)
				     (default . ancestors))
  "Alist between context and visibility span when revealing a location.

\\<org-mode-map>Some actions may move point into invisible
locations.  As a consequence, Org always expose a neighborhood
around point.  How much is shown depends on the initial action,
or context.  Valid contexts are

  agenda         when exposing an entry from the agenda
  org-goto       when using the command `org-goto' (`\\[org-goto]')
  occur-tree     when using the command `org-occur' (`\\[org-sparse-tree] /')
  tags-tree      when constructing a sparse tree based on tags matches
  link-search    when exposing search matches associated with a link
  mark-goto      when exposing the jump goal of a mark
  bookmark-jump  when exposing a bookmark location
  isearch        when exiting from an incremental search
  default        default for all contexts not set explicitly

Allowed visibility spans are

  minimal        show current headline; if point is not on headline,
                 also show entry

  local          show current headline, entry and next headline

  ancestors      show current headline and its direct ancestors; if
                 point is not on headline, also show entry

  lineage        show current headline, its direct ancestors and all
                 their children; if point is not on headline, also show
                 entry and first child

  tree           show current headline, its direct ancestors and all
                 their children; if point is not on headline, also show
                 entry and all children

  canonical      show current headline, its direct ancestors along with
                 their entries and children; if point is not located on
                 the headline, also show current entry and all children

As special cases, a nil or t value means show all contexts in
`minimal' or `canonical' view, respectively.

Some views can make displayed information very compact, but also
make it harder to edit the location of the match.  In such
a case, use the command `org-reveal' (`\\[org-reveal]') to show
more context."
  :group 'org-reveal-location
  :version "26.1"
  :package-version '(Org . "9.0")
  :type '(choice
	  (const :tag "Canonical" t)
	  (const :tag "Minimal" nil)
	  (repeat :greedy t :tag "Individual contexts"
		  (cons
		   (choice :tag "Context"
			   (const agenda)
			   (const org-goto)
			   (const occur-tree)
			   (const tags-tree)
			   (const link-search)
			   (const mark-goto)
			   (const bookmark-jump)
			   (const isearch)
			   (const default))
		   (choice :tag "Detail level"
			   (const minimal)
			   (const local)
			   (const ancestors)
			   (const lineage)
			   (const tree)
			   (const canonical))))))

(defcustom org-indirect-buffer-display 'other-window
  "How should indirect tree buffers be displayed?

This applies to indirect buffers created with the commands
`org-tree-to-indirect-buffer' and `org-agenda-tree-to-indirect-buffer'.

Valid values are:
current-window   Display in the current window
other-window     Just display in another window.
dedicated-frame  Create one new frame, and re-use it each time.
new-frame        Make a new frame each time.  Note that in this case
                 previously-made indirect buffers are kept, and you need to
                 kill these buffers yourself."
  :group 'org-structure
  :group 'org-agenda-windows
  :type '(choice
	  (const :tag "In current window" current-window)
	  (const :tag "In current frame, other window" other-window)
	  (const :tag "Each time a new frame" new-frame)
	  (const :tag "One dedicated frame" dedicated-frame)))

(defcustom org-use-speed-commands nil
  "Non-nil means activate single letter commands at beginning of a headline.
This may also be a function to test for appropriate locations where speed
commands should be active.

For example, to activate speed commands when the point is on any
star at the beginning of the headline, you can do this:

  (setq org-use-speed-commands
      (lambda () (and (looking-at org-outline-regexp) (looking-back \"^\\**\"))))"
  :group 'org-structure
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "At beginning of headline stars" t)
	  (function)))

(defcustom org-speed-commands-user nil
  "Alist of additional speed commands.
This list will be checked before `org-speed-commands-default'
when the variable `org-use-speed-commands' is non-nil
and when the cursor is at the beginning of a headline.
The car of each entry is a string with a single letter, which must
be assigned to `self-insert-command' in the global map.
The cdr is either a command to be called interactively, a function
to be called, or a form to be evaluated.
An entry that is just a list with a single string will be interpreted
as a descriptive headline that will be added when listing the speed
commands in the Help buffer using the `?' speed command."
  :group 'org-structure
  :type '(repeat :value ("k" . ignore)
		 (choice :value ("k" . ignore)
			 (list :tag "Descriptive Headline" (string :tag "Headline"))
			 (cons :tag "Letter and Command"
			       (string :tag "Command letter")
			       (choice
				(function)
				(sexp))))))

(defcustom org-bookmark-names-plist
  '(:last-capture "org-capture-last-stored"
		  :last-refile "org-refile-last-stored"
		  :last-capture-marker "org-capture-last-stored-marker")
  "Names for bookmarks automatically set by some Org commands.
This can provide strings as names for a number of bookmarks Org sets
automatically.  The following keys are currently implemented:
  :last-capture
  :last-capture-marker
  :last-refile
When a key does not show up in the property list, the corresponding bookmark
is not set."
  :group 'org-structure
  :type 'plist)

(defgroup org-cycle nil
  "Options concerning visibility cycling in Org mode."
  :tag "Org Cycle"
  :group 'org-structure)

(defcustom org-cycle-skip-children-state-if-no-children t
  "Non-nil means skip CHILDREN state in entries that don't have any."
  :group 'org-cycle
  :type 'boolean)

(defcustom org-cycle-max-level nil
  "Maximum level which should still be subject to visibility cycling.
Levels higher than this will, for cycling, be treated as text, not a headline.
When `org-odd-levels-only' is set, a value of N in this variable actually
means 2N-1 stars as the limiting headline.
When nil, cycle all levels.
Note that the limiting level of cycling is also influenced by
`org-inlinetask-min-level'.  When `org-cycle-max-level' is not set but
`org-inlinetask-min-level' is, cycling will be limited to levels one less
than its value."
  :group 'org-cycle
  :type '(choice
	  (const :tag "No limit" nil)
	  (integer :tag "Maximum level")))

(defcustom org-hide-block-startup nil
  "Non-nil means entering Org mode will fold all blocks.
This can also be set in on a per-file basis with

#+STARTUP: hideblocks
#+STARTUP: showblocks"
  :group 'org-startup
  :group 'org-cycle
  :type 'boolean)

(defcustom org-cycle-global-at-bob nil
  "Cycle globally if cursor is at beginning of buffer and not at a headline.

This makes it possible to do global cycling without having to use `S-TAB'
or `\\[universal-argument] TAB'.  For this special case to work, the first \
line of the buffer
must not be a headline -- it may be empty or some other text.

When used in this way, `org-cycle-hook' is disabled temporarily to make
sure the cursor stays at the beginning of the buffer.

When this option is nil, don't do anything special at the beginning of
the buffer."
  :group 'org-cycle
  :type 'boolean)

(defcustom org-cycle-level-after-item/entry-creation t
  "Non-nil means cycle entry level or item indentation in new empty entries.

When the cursor is at the end of an empty headline, i.e., with only stars
and maybe a TODO keyword, TAB will then switch the entry to become a child,
and then all possible ancestor states, before returning to the original state.
This makes data entry extremely fast:  M-RET to create a new headline,
on TAB to make it a child, two or more tabs to make it a (grand-)uncle.

When the cursor is at the end of an empty plain list item, one TAB will
make it a subitem, two or more tabs will back up to make this an item
higher up in the item hierarchy."
  :group 'org-cycle
  :type 'boolean)

(defcustom org-cycle-emulate-tab t
  "Where should `org-cycle' emulate TAB.
nil         Never
white       Only in completely white lines
whitestart  Only at the beginning of lines, before the first non-white char
t           Everywhere except in headlines
exc-hl-bol  Everywhere except at the start of a headline
If TAB is used in a place where it does not emulate TAB, the current subtree
visibility is cycled."
  :group 'org-cycle
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Only in completely white lines" white)
		 (const :tag "Before first char in a line" whitestart)
		 (const :tag "Everywhere except in headlines" t)
		 (const :tag "Everywhere except at bol in headlines" exc-hl-bol)))

(defcustom org-cycle-separator-lines 2
  "Number of empty lines needed to keep an empty line between collapsed trees.
If you leave an empty line between the end of a subtree and the following
headline, this empty line is hidden when the subtree is folded.
Org mode will leave (exactly) one empty line visible if the number of
empty lines is equal or larger to the number given in this variable.
So the default 2 means at least 2 empty lines after the end of a subtree
are needed to produce free space between a collapsed subtree and the
following headline.

If the number is negative, and the number of empty lines is at least -N,
all empty lines are shown.

Special case: when 0, never leave empty lines in collapsed view."
  :group 'org-cycle
  :type 'integer)
(put 'org-cycle-separator-lines 'safe-local-variable 'integerp)

(defcustom org-pre-cycle-hook nil
  "Hook that is run before visibility cycling is happening.
The function(s) in this hook must accept a single argument which indicates
the new state that will be set right after running this hook.  The
argument is a symbol.  Before a global state change, it can have the values
`overview', `content', or `all'.  Before a local state change, it can have
the values `folded', `children', or `subtree'."
  :group 'org-cycle
  :type 'hook)

(defcustom org-cycle-hook '(org-cycle-hide-archived-subtrees
			    org-cycle-hide-drawers
			    org-cycle-show-empty-lines
			    org-optimize-window-after-visibility-change)
  "Hook that is run after `org-cycle' has changed the buffer visibility.
The function(s) in this hook must accept a single argument which indicates
the new state that was set by the most recent `org-cycle' command.  The
argument is a symbol.  After a global state change, it can have the values
`overview', `contents', or `all'.  After a local state change, it can have
the values `folded', `children', or `subtree'."
  :group 'org-cycle
  :type 'hook
  :version "26.1"
  :package-version '(Org . "8.3"))

(defgroup org-edit-structure nil
  "Options concerning structure editing in Org mode."
  :tag "Org Edit Structure"
  :group 'org-structure)

(defcustom org-odd-levels-only nil
  "Non-nil means skip even levels and only use odd levels for the outline.
This has the effect that two stars are being added/taken away in
promotion/demotion commands.  It also influences how levels are
handled by the exporters.
Changing it requires restart of `font-lock-mode' to become effective
for fontification also in regions already fontified.
You may also set this on a per-file basis by adding one of the following
lines to the buffer:

   #+STARTUP: odd
   #+STARTUP: oddeven"
  :group 'org-edit-structure
  :group 'org-appearance
  :type 'boolean)

(defcustom org-adapt-indentation t
  "Non-nil means adapt indentation to outline node level.

When this variable is set, Org assumes that you write outlines by
indenting text in each node to align with the headline (after the
stars).  The following issues are influenced by this variable:

- The indentation is increased by one space in a demotion
  command, and decreased by one in a promotion command.  However,
  in the latter case, if shifting some line in the entry body
  would alter document structure (e.g., insert a new headline),
  indentation is not changed at all.

- Property drawers and planning information is inserted indented
  when this variable is set.  When nil, they will not be indented.

- TAB indents a line relative to current level.  The lines below
  a headline will be indented when this variable is set.

Note that this is all about true indentation, by adding and
removing space characters.  See also `org-indent.el' which does
level-dependent indentation in a virtual way, i.e. at display
time in Emacs."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-special-ctrl-a/e nil
  "Non-nil means `C-a' and `C-e' behave specially in headlines and items.

When t, `C-a' will bring back the cursor to the beginning of the
headline text, i.e. after the stars and after a possible TODO
keyword.  In an item, this will be the position after bullet and
check-box, if any.  When the cursor is already at that position,
another `C-a' will bring it to the beginning of the line.

`C-e' will jump to the end of the headline, ignoring the presence
of tags in the headline.  A second `C-e' will then jump to the
true end of the line, after any tags.  This also means that, when
this variable is non-nil, `C-e' also will never jump beyond the
end of the heading of a folded section, i.e. not after the
ellipses.

When set to the symbol `reversed', the first `C-a' or `C-e' works
normally, going to the true line boundary first.  Only a directly
following, identical keypress will bring the cursor to the
special positions.

This may also be a cons cell where the behavior for `C-a' and
`C-e' is set separately."
  :group 'org-edit-structure
  :type '(choice
	  (const :tag "off" nil)
	  (const :tag "on: after stars/bullet and before tags first" t)
	  (const :tag "reversed: true line boundary first" reversed)
	  (cons :tag "Set C-a and C-e separately"
		(choice :tag "Special C-a"
			(const :tag "off" nil)
			(const :tag "on: after  stars/bullet first" t)
			(const :tag "reversed: before stars/bullet first" reversed))
		(choice :tag "Special C-e"
			(const :tag "off" nil)
			(const :tag "on: before tags first" t)
			(const :tag "reversed: after tags first" reversed)))))
(defvaralias 'org-special-ctrl-a 'org-special-ctrl-a/e)

(defcustom org-special-ctrl-k nil
  "Non-nil means `C-k' will behave specially in headlines.
When nil, `C-k' will call the default `kill-line' command.
When t, the following will happen while the cursor is in the headline:

- When the cursor is at the beginning of a headline, kill the entire
  line and possible the folded subtree below the line.
- When in the middle of the headline text, kill the headline up to the tags.
- When after the headline text, kill the tags."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-ctrl-k-protect-subtree nil
  "Non-nil means, do not delete a hidden subtree with C-k.
When set to the symbol `error', simply throw an error when C-k is
used to kill (part-of) a headline that has hidden text behind it.
Any other non-nil value will result in a query to the user, if it is
OK to kill that hidden subtree.  When nil, kill without remorse."
  :group 'org-edit-structure
  :version "24.1"
  :type '(choice
	  (const :tag "Do not protect hidden subtrees" nil)
	  (const :tag "Protect hidden subtrees with a security query" t)
	  (const :tag "Never kill a hidden subtree with C-k" error)))

(defcustom org-special-ctrl-o t
  "Non-nil means, make `C-o' insert a row in tables."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-catch-invisible-edits nil
  "Check if in invisible region before inserting or deleting a character.
Valid values are:

nil              Do not check, so just do invisible edits.
error            Throw an error and do nothing.
show             Make point visible, and do the requested edit.
show-and-error   Make point visible, then throw an error and abort the edit.
smart            Make point visible, and do insertion/deletion if it is
                 adjacent to visible text and the change feels predictable.
                 Never delete a previously invisible character or add in the
                 middle or right after an invisible region.  Basically, this
                 allows insertion and backward-delete right before ellipses.
                 FIXME: maybe in this case we should not even show?"
  :group 'org-edit-structure
  :version "24.1"
  :type '(choice
	  (const :tag "Do not check" nil)
	  (const :tag "Throw error when trying to edit" error)
	  (const :tag "Unhide, but do not do the edit" show-and-error)
	  (const :tag "Show invisible part and do the edit" show)
	  (const :tag "Be smart and do the right thing" smart)))

(defcustom org-yank-folded-subtrees t
  "Non-nil means when yanking subtrees, fold them.
If the kill is a single subtree, or a sequence of subtrees, i.e. if
it starts with a heading and all other headings in it are either children
or siblings, then fold all the subtrees.  However, do this only if no
text after the yank would be swallowed into a folded tree by this action."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-yank-adjusted-subtrees nil
  "Non-nil means when yanking subtrees, adjust the level.
With this setting, `org-paste-subtree' is used to insert the subtree, see
this function for details."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-M-RET-may-split-line '((default . t))
  "Non-nil means M-RET will split the line at the cursor position.
When nil, it will go to the end of the line before making a
new line.
You may also set this option in a different way for different
contexts.  Valid contexts are:

headline  when creating a new headline
item      when creating a new item
table     in a table field
default   the value to be used for all contexts not explicitly
          customized"
  :group 'org-structure
  :group 'org-table
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (repeat :greedy t :tag "Individual contexts"
		  (cons
		   (choice :tag "Context"
			   (const headline)
			   (const item)
			   (const table)
			   (const default))
		   (boolean)))))


(defcustom org-insert-heading-respect-content nil
  "Non-nil means insert new headings after the current subtree.
\\<org-mode-map>
When nil, the new heading is created directly after the current line.
The commands `\\[org-insert-heading-respect-content]' and \
`\\[org-insert-todo-heading-respect-content]' turn this variable on
for the duration of the command."
  :group 'org-structure
  :type 'boolean)

(defcustom org-blank-before-new-entry '((heading . auto)
					(plain-list-item . auto))
  "Should `org-insert-heading' leave a blank line before new heading/item?
The value is an alist, with `heading' and `plain-list-item' as CAR,
and a boolean flag as CDR.  The cdr may also be the symbol `auto', in
which case Org will look at the surrounding headings/items and try to
make an intelligent decision whether to insert a blank line or not."
  :group 'org-edit-structure
  :type '(list
	  (cons (const heading)
		(choice (const :tag "Never" nil)
			(const :tag "Always" t)
			(const :tag "Auto" auto)))
	  (cons (const plain-list-item)
		(choice (const :tag "Never" nil)
			(const :tag "Always" t)
			(const :tag "Auto" auto)))))

(defcustom org-insert-heading-hook nil
  "Hook being run after inserting a new heading."
  :group 'org-edit-structure
  :type 'hook)

(defcustom org-enable-fixed-width-editor t
  "Non-nil means lines starting with \":\" are treated as fixed-width.
This currently only means they are never auto-wrapped.
When nil, such lines will be treated like ordinary lines."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-goto-auto-isearch t
  "Non-nil means typing characters in `org-goto' starts incremental search.
When nil, you can use these keybindings to navigate the buffer:

  q    Quit the org-goto interface
  n    Go to the next visible heading
  p    Go to the previous visible heading
  f    Go one heading forward on same level
  b    Go one heading backward on same level
  u    Go one heading up"
  :group 'org-edit-structure
  :type 'boolean)

(defgroup org-sparse-trees nil
  "Options concerning sparse trees in Org mode."
  :tag "Org Sparse Trees"
  :group 'org-structure)

(defcustom org-highlight-sparse-tree-matches t
  "Non-nil means highlight all matches that define a sparse tree.
The highlights will automatically disappear the next time the buffer is
changed by an edit command."
  :group 'org-sparse-trees
  :type 'boolean)

(defcustom org-remove-highlights-with-change t
  "Non-nil means any change to the buffer will remove temporary highlights.
\\<org-mode-map>\
Such highlights are created by `org-occur' and `org-clock-display'.
When nil, `\\[org-ctrl-c-ctrl-c]' needs to be used \
to get rid of the highlights.
The highlights created by `org-toggle-latex-fragment' always need
`\\[org-toggle-latex-fragment]' to be removed."
  :group 'org-sparse-trees
  :group 'org-time
  :type 'boolean)

(defcustom org-occur-case-fold-search t
  "Non-nil means `org-occur' should be case-insensitive.
If set to `smart' the search will be case-insensitive only if it
doesn't specify any upper case character."
  :group 'org-sparse-trees
  :version "26.1"
  :type '(choice
	  (const :tag "Case-sensitive" nil)
	  (const :tag "Case-insensitive" t)
	  (const :tag "Case-insensitive for lower case searches only" 'smart)))

(defcustom org-occur-hook '(org-first-headline-recenter)
  "Hook that is run after `org-occur' has constructed a sparse tree.
This can be used to recenter the window to show as much of the structure
as possible."
  :group 'org-sparse-trees
  :type 'hook)

(defgroup org-imenu-and-speedbar nil
  "Options concerning imenu and speedbar in Org mode."
  :tag "Org Imenu and Speedbar"
  :group 'org-structure)

(defcustom org-imenu-depth 2
  "The maximum level for Imenu access to Org headlines.
This also applied for speedbar access."
  :group 'org-imenu-and-speedbar
  :type 'integer)

(defgroup org-table nil
  "Options concerning tables in Org mode."
  :tag "Org Table"
  :group 'org)

(defcustom org-self-insert-cluster-for-undo nil
  "Non-nil means cluster self-insert commands for undo when possible.
If this is set, then, like in the Emacs command loop, 20 consecutive
characters will be undone together.
This is configurable, because there is some impact on typing performance."
  :group 'org-table
  :type 'boolean)

(defcustom org-table-tab-recognizes-table.el t
  "Non-nil means TAB will automatically notice a table.el table.
When it sees such a table, it moves point into it and - if necessary -
calls `table-recognize-table'."
  :group 'org-table-editing
  :type 'boolean)

(defgroup org-link nil
  "Options concerning links in Org mode."
  :tag "Org Link"
  :group 'org)

(defvar-local org-link-abbrev-alist-local nil
  "Buffer-local version of `org-link-abbrev-alist', which see.
The value of this is taken from the #+LINK lines.")

(defcustom org-link-parameters
  '(("doi" :follow org--open-doi-link)
    ("elisp" :follow org--open-elisp-link)
    ("file" :complete org-file-complete-link)
    ("ftp" :follow (lambda (path) (browse-url (concat "ftp:" path))))
    ("help" :follow org--open-help-link)
    ("http" :follow (lambda (path) (browse-url (concat "http:" path))))
    ("https" :follow (lambda (path) (browse-url (concat "https:" path))))
    ("mailto" :follow (lambda (path) (browse-url (concat "mailto:" path))))
    ("news" :follow (lambda (path) (browse-url (concat "news:" path))))
    ("shell" :follow org--open-shell-link))
  "An alist of properties that defines all the links in Org mode.
The key in each association is a string of the link type.
Subsequent optional elements make up a p-list of link properties.

:follow - A function that takes the link path as an argument.

:export - A function that takes the link path, description and
export-backend as arguments.

:store - A function responsible for storing the link.  See the
function `org-store-link-functions'.

:complete - A function that inserts a link with completion.  The
function takes one optional prefix arg.

:face - A face for the link, or a function that returns a face.
The function takes one argument which is the link path.  The
default face is `org-link'.

:mouse-face - The mouse-face. The default is `highlight'.

:display - `full' will not fold the link in descriptive
display.  Default is `org-link'.

:help-echo - A string or function that takes (window object position)
as arguments and returns a string.

:keymap - A keymap that is active on the link.  The default is
`org-mouse-map'.

:htmlize-link - A function for the htmlize-link.  Defaults
to (list :uri \"type:path\")

:activate-func - A function to run at the end of font-lock
activation.  The function must accept (link-start link-end path bracketp)
as arguments."
  :group 'org-link
  :type '(alist :tag "Link display parameters"
		:value-type plist)
  :version "26.1"
  :package-version '(Org . "9.1"))

(defun org-link-get-parameter (type key)
  "Get TYPE link property for KEY.
TYPE is a string and KEY is a plist keyword."
  (plist-get
   (cdr (assoc type org-link-parameters))
   key))

(defun org-link-set-parameters (type &rest parameters)
  "Set link TYPE properties to PARAMETERS.
  PARAMETERS should be :key val pairs."
  (let ((data (assoc type org-link-parameters)))
    (if data (setcdr data (org-combine-plists (cdr data) parameters))
      (push (cons type parameters) org-link-parameters)
      (org-make-link-regexps)
      (org-element-update-syntax))))

(defun org-link-types ()
  "Return a list of known link types."
  (mapcar #'car org-link-parameters))

(defcustom org-link-abbrev-alist nil
  "Alist of link abbreviations.
The car of each element is a string, to be replaced at the start of a link.
The cdrs are replacement values, like (\"linkkey\" . REPLACE).  Abbreviated
links in Org buffers can have an optional tag after a double colon, e.g.,

     [[linkkey:tag][description]]

The `linkkey' must be a single word, starting with a letter, followed
by letters, numbers, `-' or `_'.

If REPLACE is a string, the tag will simply be appended to create the link.
If the string contains \"%s\", the tag will be inserted there.  If the string
contains \"%h\", it will cause a url-encoded version of the tag to be inserted
at that point (see the function `url-hexify-string').  If the string contains
the specifier \"%(my-function)\", then the custom function `my-function' will
be invoked: this function takes the tag as its only argument and must return
a string.

REPLACE may also be a function that will be called with the tag as the
only argument to create the link, which should be returned as a string.

See the manual for examples."
  :group 'org-link
  :type '(repeat
	  (cons
	   (string :tag "Protocol")
	   (choice
	    (string :tag "Format")
	    (function)))))

(defcustom org-descriptive-links t
  "Non-nil means Org will display descriptive links.
E.g. [[http://orgmode.org][Org website]] will be displayed as
\"Org Website\", hiding the link itself and just displaying its
description.  When set to nil, Org will display the full links
literally.

You can interactively set the value of this variable by calling
`org-toggle-link-display' or from the menu Org>Hyperlinks menu."
  :group 'org-link
  :type 'boolean)

(defcustom org-link-file-path-type 'adaptive
  "How the path name in file links should be stored.
Valid values are:

relative  Relative to the current directory, i.e. the directory of the file
          into which the link is being inserted.
absolute  Absolute path, if possible with ~ for home directory.
noabbrev  Absolute path, no abbreviation of home directory.
adaptive  Use relative path for files in the current directory and sub-
          directories of it.  For other files, use an absolute path."
  :group 'org-link
  :type '(choice
	  (const relative)
	  (const absolute)
	  (const noabbrev)
	  (const adaptive)))

(defvaralias 'org-activate-links 'org-highlight-links)
(defcustom org-highlight-links '(bracket angle plain radio tag date footnote)
  "Types of links that should be highlighted in Org files.

This is a list of symbols, each one of them leading to the
highlighting of a certain link type.

You can still open links that are not highlighted.

In principle, it does not hurt to turn on highlighting for all
link types.  There may be a small gain when turning off unused
link types.  The types are:

bracket   The recommended [[link][description]] or [[link]] links with hiding.
angle     Links in angular brackets that may contain whitespace like
          <bbdb:Carsten Dominik>.
plain     Plain links in normal text, no whitespace, like http://google.com.
radio     Text that is matched by a radio target, see manual for details.
tag       Tag settings in a headline (link to tag search).
date      Time stamps (link to calendar).
footnote  Footnote labels.

If you set this variable during an Emacs session, use `org-mode-restart'
in the Org buffer so that the change takes effect."
  :group 'org-link
  :group 'org-appearance
  :type '(set :greedy t
	      (const :tag "Double bracket links" bracket)
	      (const :tag "Angular bracket links" angle)
	      (const :tag "Plain text links" plain)
	      (const :tag "Radio target matches" radio)
	      (const :tag "Tags" tag)
	      (const :tag "Timestamps" date)
	      (const :tag "Footnotes" footnote)))

(defcustom org-make-link-description-function nil
  "Function to use for generating link descriptions from links.
This function must take two parameters: the first one is the
link, the second one is the description generated by
`org-insert-link'.  The function should return the description to
use."
  :group 'org-link
  :type '(choice (const nil) (function)))

(defgroup org-link-store nil
  "Options concerning storing links in Org mode."
  :tag "Org Store Link"
  :group 'org-link)

(defcustom org-url-hexify-p t
  "When non-nil, hexify URL when creating a link."
  :type 'boolean
  :version "24.3"
  :group 'org-link-store)

(defcustom org-email-link-description-format "Email %c: %.30s"
  "Format of the description part of a link to an email or usenet message.
The following %-escapes will be replaced by corresponding information:

%F   full \"From\" field
%f   name, taken from \"From\" field, address if no name
%T   full \"To\" field
%t   first name in \"To\" field, address if no name
%c   correspondent.  Usually \"from NAME\", but if you sent it yourself, it
     will be \"to NAME\".  See also the variable `org-from-is-user-regexp'.
%s   subject
%d   date
%m   message-id.

You may use normal field width specification between the % and the letter.
This is for example useful to limit the length of the subject.

Examples: \"%f on: %.30s\", \"Email from %f\", \"Email %c\""
  :group 'org-link-store
  :type 'string)

(defcustom org-from-is-user-regexp
  (let (r1 r2)
    (when (and user-mail-address (not (string= user-mail-address "")))
      (setq r1 (concat "\\<" (regexp-quote user-mail-address) "\\>")))
    (when (and user-full-name (not (string= user-full-name "")))
      (setq r2 (concat "\\<" (regexp-quote user-full-name) "\\>")))
    (if (and r1 r2) (concat r1 "\\|" r2) (or r1 r2)))
  "Regexp matched against the \"From:\" header of an email or usenet message.
It should match if the message is from the user him/herself."
  :group 'org-link-store
  :type 'regexp)

(defcustom org-context-in-file-links t
  "Non-nil means file links from `org-store-link' contain context.
\\<org-mode-map>
A search string will be added to the file name with :: as separator
and used to find the context when the link is activated by the command
`org-open-at-point'.  When this option is t, the entire active region
will be placed in the search string of the file link.  If set to a
positive integer, only the first n lines of context will be stored.

Using a prefix arg to the command `org-store-link' (`\\[universal-argument] \
\\[org-store-link]')
negates this setting for the duration of the command."
  :group 'org-link-store
  :type '(choice boolean integer))

(defcustom org-keep-stored-link-after-insertion nil
  "Non-nil means keep link in list for entire session.
\\<org-mode-map>
The command `org-store-link' adds a link pointing to the current
location to an internal list.  These links accumulate during a session.
The command `org-insert-link' can be used to insert links into any
Org file (offering completion for all stored links).

When this option is nil, every link which has been inserted once using
`\\[org-insert-link]' will be removed from the list, to make completing the \
unused
links more efficient."
  :group 'org-link-store
  :type 'boolean)

(defgroup org-link-follow nil
  "Options concerning following links in Org mode."
  :tag "Org Follow Link"
  :group 'org-link)

(defcustom org-link-translation-function nil
  "Function to translate links with different syntax to Org syntax.
This can be used to translate links created for example by the Planner
or emacs-wiki packages to Org syntax.
The function must accept two parameters, a TYPE containing the link
protocol name like \"rmail\" or \"gnus\" as a string, and the linked path,
which is everything after the link protocol.  It should return a cons
with possibly modified values of type and path.
Org contains a function for this, so if you set this variable to
`org-translate-link-from-planner', you should be able follow many
links created by planner."
  :group 'org-link-follow
  :type '(choice (const nil) (function)))

(defcustom org-follow-link-hook nil
  "Hook that is run after a link has been followed."
  :group 'org-link-follow
  :type 'hook)

(defcustom org-tab-follows-link nil
  "Non-nil means on links TAB will follow the link.
Needs to be set before org.el is loaded.
This really should not be used, it does not make sense, and the
implementation is bad."
  :group 'org-link-follow
  :type 'boolean)

(defcustom org-return-follows-link nil
  "Non-nil means on links RET will follow the link.
In tables, the special behavior of RET has precedence."
  :group 'org-link-follow
  :type 'boolean)

(defcustom org-mouse-1-follows-link
  (if (boundp 'mouse-1-click-follows-link) mouse-1-click-follows-link t)
  "Non-nil means mouse-1 on a link will follow the link.
A longer mouse click will still set point.  Needs to be set
before org.el is loaded."
  :group 'org-link-follow
  :version "26.1"
  :package-version '(Org . "8.3")
  :type '(choice
	  (const :tag "A double click follows the link" double)
	  (const :tag "Unconditionally follow the link with mouse-1" t)
	  (integer :tag "mouse-1 click does not follow the link if longer than N ms" 450)))

(defcustom org-mark-ring-length 4
  "Number of different positions to be recorded in the ring.
Changing this requires a restart of Emacs to work correctly."
  :group 'org-link-follow
  :type 'integer)

(defcustom org-link-search-must-match-exact-headline 'query-to-create
  "Non-nil means internal fuzzy links can only match headlines.

When nil, the a fuzzy link may point to a target or a named
construct in the document.  When set to the special value
`query-to-create', offer to create a new headline when none
matched.

Spaces and statistics cookies are ignored during heading searches."
  :group 'org-link-follow
  :version "24.1"
  :type '(choice
	  (const :tag "Use fuzzy text search" nil)
	  (const :tag "Match only exact headline" t)
	  (const :tag "Match exact headline or query to create it"
		 query-to-create))
  :safe #'symbolp)

(defcustom org-link-frame-setup
  '((vm . vm-visit-folder-other-frame)
    (vm-imap . vm-visit-imap-folder-other-frame)
    (gnus . org-gnus-no-new-news)
    (file . find-file-other-window)
    (wl . wl-other-frame))
  "Setup the frame configuration for following links.
When following a link with Emacs, it may often be useful to display
this link in another window or frame.  This variable can be used to
set this up for the different types of links.
For VM, use any of
    `vm-visit-folder'
    `vm-visit-folder-other-window'
    `vm-visit-folder-other-frame'
For Gnus, use any of
    `gnus'
    `gnus-other-frame'
    `org-gnus-no-new-news'
For FILE, use any of
    `find-file'
    `find-file-other-window'
    `find-file-other-frame'
For Wanderlust use any of
    `wl'
    `wl-other-frame'
For the calendar, use the variable `calendar-setup'.
For BBDB, it is currently only possible to display the matches in
another window."
  :group 'org-link-follow
  :type '(list
	  (cons (const vm)
		(choice
		 (const vm-visit-folder)
		 (const vm-visit-folder-other-window)
		 (const vm-visit-folder-other-frame)))
	  (cons (const vm-imap)
		(choice
		 (const vm-visit-imap-folder)
		 (const vm-visit-imap-folder-other-window)
		 (const vm-visit-imap-folder-other-frame)))
	  (cons (const gnus)
		(choice
		 (const gnus)
		 (const gnus-other-frame)
		 (const org-gnus-no-new-news)))
	  (cons (const file)
		(choice
		 (const find-file)
		 (const find-file-other-window)
		 (const find-file-other-frame)))
	  (cons (const wl)
		(choice
		 (const wl)
		 (const wl-other-frame)))))

(defcustom org-display-internal-link-with-indirect-buffer nil
  "Non-nil means use indirect buffer to display infile links.
Activating internal links (from one location in a file to another location
in the same file) normally just jumps to the location.  When the link is
activated with a `\\[universal-argument]' prefix (or with mouse-3), the link \
is displayed in
another window.  When this option is set, the other window actually displays
an indirect buffer clone of the current buffer, to avoid any visibility
changes to the current buffer."
  :group 'org-link-follow
  :type 'boolean)

(defcustom org-open-non-existing-files nil
  "Non-nil means `org-open-file' will open non-existing files.
When nil, an error will be generated.
This variable applies only to external applications because they
might choke on non-existing files.  If the link is to a file that
will be opened in Emacs, the variable is ignored."
  :group 'org-link-follow
  :type 'boolean)

(defcustom org-open-directory-means-index-dot-org nil
  "Non-nil means a link to a directory really means to index.org.
When nil, following a directory link will run dired or open a finder/explorer
window on that directory."
  :group 'org-link-follow
  :type 'boolean)

(defcustom org-confirm-shell-link-function 'yes-or-no-p
  "Non-nil means ask for confirmation before executing shell links.
Shell links can be dangerous: just think about a link

     [[shell:rm -rf ~/*][Google Search]]

This link would show up in your Org document as \"Google Search\",
but really it would remove your entire home directory.
Therefore we advise against setting this variable to nil.
Just change it to `y-or-n-p' if you want to confirm with a
single keystroke rather than having to type \"yes\"."
  :group 'org-link-follow
  :type '(choice
	  (const :tag "with yes-or-no (safer)" yes-or-no-p)
	  (const :tag "with y-or-n (faster)" y-or-n-p)
	  (const :tag "no confirmation (dangerous)" nil)))
(put 'org-confirm-shell-link-function
     'safe-local-variable
     (lambda (x) (member x '(yes-or-no-p y-or-n-p))))

(defcustom org-confirm-shell-link-not-regexp ""
  "A regexp to skip confirmation for shell links."
  :group 'org-link-follow
  :version "24.1"
  :type 'regexp)

(defcustom org-confirm-elisp-link-function 'yes-or-no-p
  "Non-nil means ask for confirmation before executing Emacs Lisp links.
Elisp links can be dangerous: just think about a link

     [[elisp:(shell-command \"rm -rf ~/*\")][Google Search]]

This link would show up in your Org document as \"Google Search\",
but really it would remove your entire home directory.
Therefore we advise against setting this variable to nil.
Just change it to `y-or-n-p' if you want to confirm with a
single keystroke rather than having to type \"yes\"."
  :group 'org-link-follow
  :type '(choice
	  (const :tag "with yes-or-no (safer)" yes-or-no-p)
	  (const :tag "with y-or-n (faster)" y-or-n-p)
	  (const :tag "no confirmation (dangerous)" nil)))
(put 'org-confirm-shell-link-function
     'safe-local-variable
     (lambda (x) (member x '(yes-or-no-p y-or-n-p))))

(defcustom org-confirm-elisp-link-not-regexp ""
  "A regexp to skip confirmation for Elisp links."
  :group 'org-link-follow
  :version "24.1"
  :type 'regexp)

(defconst org-file-apps-defaults-gnu
  '((remote . emacs)
    (system . mailcap)
    (t . mailcap))
  "Default file applications on a UNIX or GNU/Linux system.
See `org-file-apps'.")

(defconst org-file-apps-defaults-macosx
  '((remote . emacs)
    (system . "open %s")
    ("ps.gz"  . "gv %s")
    ("eps.gz" . "gv %s")
    ("dvi"    . "xdvi %s")
    ("fig"    . "xfig %s")
    (t . "open %s"))
  "Default file applications on a macOS system.
The system \"open\" is known as a default, but we use X11 applications
for some files for which the OS does not have a good default.
See `org-file-apps'.")

(defconst org-file-apps-defaults-windowsnt
  (list '(remote . emacs)
	(cons 'system (lambda (file _path)
			(with-no-warnings (w32-shell-execute "open" file))))
	(cons t (lambda (file _path)
		  (with-no-warnings (w32-shell-execute "open" file)))))
  "Default file applications on a Windows NT system.
The system \"open\" is used for most files.
See `org-file-apps'.")

(defcustom org-file-apps
  '((auto-mode . emacs)
    ("\\.mm\\'" . default)
    ("\\.x?html?\\'" . default)
    ("\\.pdf\\'" . default))
  "External applications for opening `file:path' items in a document.
\\<org-mode-map>\

Org mode uses system defaults for different file types, but
you can use this variable to set the application for a given file
extension.  The entries in this list are cons cells where the car identifies
files and the cdr the corresponding command.

Possible values for the file identifier are:

 \"string\"    A string as a file identifier can be interpreted in different
               ways, depending on its contents:

               - Alphanumeric characters only:
                 Match links with this file extension.
                 Example: (\"pdf\" . \"evince %s\")
                          to open PDFs with evince.

               - Regular expression: Match links where the
                 filename matches the regexp.  If you want to
                 use groups here, use shy groups.

                 Example: (\"\\\\.x?html\\\\\\='\" . \"firefox %s\")
                          (\"\\\\(?:xhtml\\\\|html\\\\)\\\\\\='\" . \"firefox %s\")
                          to open *.html and *.xhtml with firefox.

               - Regular expression which contains (non-shy) groups:
                 Match links where the whole link, including \"::\", and
                 anything after that, matches the regexp.
                 In a custom command string, %1, %2, etc. are replaced with
                 the parts of the link that were matched by the groups.
                 For backwards compatibility, if a command string is given
                 that does not use any of the group matches, this case is
                 handled identically to the second one (i.e. match against
                 file name only).
                 In a custom function, you can access the group matches with
                 (match-string n link).

                 Example: (\"\\\\.pdf::\\\\(\\\\d+\\\\)\\\\\\='\" . \
\"evince -p %1 %s\")
                     to open [[file:document.pdf::5]] with evince at page 5.

 `directory'   Matches a directory
 `remote'      Matches a remote file, accessible through tramp or efs.
               Remote files most likely should be visited through Emacs
               because external applications cannot handle such paths.
`auto-mode'    Matches files that are matched by any entry in `auto-mode-alist',
               so all files Emacs knows how to handle.  Using this with
               command `emacs' will open most files in Emacs.  Beware that this
               will also open html files inside Emacs, unless you add
               (\"html\" . default) to the list as well.
 `system'      The system command to open files, like `open' on Windows
               and macOS, and mailcap under GNU/Linux.  This is the command
               that will be selected if you call `org-open-at-point' with a
               double prefix argument (`\\[universal-argument] \
\\[universal-argument] \\[org-open-at-point]').
 t             Default for files not matched by any of the other options.

Possible values for the command are:

 `emacs'       The file will be visited by the current Emacs process.
 `default'     Use the default application for this file type, which is the
               association for t in the list, most likely in the system-specific
               part.  This can be used to overrule an unwanted setting in the
               system-specific variable.
 `system'      Use the system command for opening files, like \"open\".
               This command is specified by the entry whose car is `system'.
               Most likely, the system-specific version of this variable
               does define this command, but you can overrule/replace it
               here.
`mailcap'      Use command specified in the mailcaps.
 string        A command to be executed by a shell; %s will be replaced
               by the path to the file.
 function      A Lisp function, which will be called with two arguments:
               the file path and the original link string, without the
               \"file:\" prefix.

For more examples, see the system specific constants
`org-file-apps-defaults-macosx'
`org-file-apps-defaults-windowsnt'
`org-file-apps-defaults-gnu'."
  :group 'org-link-follow
  :type '(repeat
	  (cons (choice :value ""
			(string :tag "Extension")
			(const :tag "System command to open files" system)
			(const :tag "Default for unrecognized files" t)
			(const :tag "Remote file" remote)
			(const :tag "Links to a directory" directory)
			(const :tag "Any files that have Emacs modes"
			       auto-mode))
		(choice :value ""
			(const :tag "Visit with Emacs" emacs)
			(const :tag "Use default" default)
			(const :tag "Use the system command" system)
			(string :tag "Command")
			(function :tag "Function")))))

(defcustom org-doi-server-url "http://dx.doi.org/"
  "The URL of the DOI server."
  :type 'string
  :version "24.3"
  :group 'org-link-follow)

(defgroup org-refile nil
  "Options concerning refiling entries in Org mode."
  :tag "Org Refile"
  :group 'org)

(defcustom org-directory "~/org"
  "Directory with Org files.
This is just a default location to look for Org files.  There is no need
at all to put your files into this directory.  It is used in the
following situations:

1. When a capture template specifies a target file that is not an
   absolute path.  The path will then be interpreted relative to
   `org-directory'
2. When the value of variable `org-agenda-files' is a single file, any
   relative paths in this file will be taken as relative to
   `org-directory'."
  :group 'org-refile
  :group 'org-capture
  :type 'directory)

(defcustom org-default-notes-file (convert-standard-filename "~/.notes")
  "Default target for storing notes.
Used as a fall back file for org-capture.el, for templates that
do not specify a target file."
  :group 'org-refile
  :group 'org-capture
  :type 'file)

(defcustom org-goto-interface 'outline
  "The default interface to be used for `org-goto'.
Allowed values are:
outline                  The interface shows an outline of the relevant file
                         and the correct heading is found by moving through
                         the outline or by searching with incremental search.
outline-path-completion  Headlines in the current buffer are offered via
                         completion.  This is the interface also used by
                         the refile command."
  :group 'org-refile
  :type '(choice
	  (const :tag "Outline" outline)
	  (const :tag "Outline-path-completion" outline-path-completion)))

(defcustom org-goto-max-level 5
  "Maximum target level when running `org-goto' with refile interface."
  :group 'org-refile
  :type 'integer)

(defcustom org-reverse-note-order nil
  "Non-nil means store new notes at the beginning of a file or entry.
When nil, new notes will be filed to the end of a file or entry.
This can also be a list with cons cells of regular expressions that
are matched against file names, and values."
  :group 'org-capture
  :group 'org-refile
  :type '(choice
	  (const :tag "Reverse always" t)
	  (const :tag "Reverse never" nil)
	  (repeat :tag "By file name regexp"
		  (cons regexp boolean))))

(defcustom org-log-refile nil
  "Information to record when a task is refiled.

Possible values are:

nil     Don't add anything
time    Add a time stamp to the task
note    Prompt for a note and add it with template `org-log-note-headings'

This option can also be set with on a per-file-basis with

   #+STARTUP: nologrefile
   #+STARTUP: logrefile
   #+STARTUP: lognoterefile

You can have local logging settings for a subtree by setting the LOGGING
property to one or more of these keywords.

When bulk-refiling from the agenda, the value `note' is forbidden and
will temporarily be changed to `time'."
  :group 'org-refile
  :group 'org-progress
  :version "24.1"
  :type '(choice
	  (const :tag "No logging" nil)
	  (const :tag "Record timestamp" time)
	  (const :tag "Record timestamp with note." note)))

(defcustom org-refile-targets nil
  "Targets for refiling entries with `\\[org-refile]'.
This is a list of cons cells.  Each cell contains:
- a specification of the files to be considered, either a list of files,
  or a symbol whose function or variable value will be used to retrieve
  a file name or a list of file names.  If you use `org-agenda-files' for
  that, all agenda files will be scanned for targets.  Nil means consider
  headings in the current buffer.
- A specification of how to find candidate refile targets.  This may be
  any of:
  - a cons cell (:tag . \"TAG\") to identify refile targets by a tag.
    This tag has to be present in all target headlines, inheritance will
    not be considered.
  - a cons cell (:todo . \"KEYWORD\") to identify refile targets by
    todo keyword.
  - a cons cell (:regexp . \"REGEXP\") with a regular expression matching
    headlines that are refiling targets.
  - a cons cell (:level . N).  Any headline of level N is considered a target.
    Note that, when `org-odd-levels-only' is set, level corresponds to
    order in hierarchy, not to the number of stars.
  - a cons cell (:maxlevel . N).  Any headline with level <= N is a target.
    Note that, when `org-odd-levels-only' is set, level corresponds to
    order in hierarchy, not to the number of stars.

Each element of this list generates a set of possible targets.
The union of these sets is presented (with completion) to
the user by `org-refile'.

You can set the variable `org-refile-target-verify-function' to a function
to verify each headline found by the simple criteria above.

When this variable is nil, all top-level headlines in the current buffer
are used, equivalent to the value `((nil . (:level . 1))'."
  :group 'org-refile
  :type '(repeat
	  (cons
	   (choice :value org-agenda-files
		   (const :tag "All agenda files" org-agenda-files)
		   (const :tag "Current buffer" nil)
		   (function) (variable) (file))
	   (choice :tag "Identify target headline by"
		   (cons :tag "Specific tag" (const :value :tag) (string))
		   (cons :tag "TODO keyword" (const :value :todo) (string))
		   (cons :tag "Regular expression" (const :value :regexp) (regexp))
		   (cons :tag "Level number" (const :value :level) (integer))
		   (cons :tag "Max Level number" (const :value :maxlevel) (integer))))))

(defcustom org-refile-target-verify-function nil
  "Function to verify if the headline at point should be a refile target.
The function will be called without arguments, with point at the
beginning of the headline.  It should return t and leave point
where it is if the headline is a valid target for refiling.

If the target should not be selected, the function must return nil.
In addition to this, it may move point to a place from where the search
should be continued.  For example, the function may decide that the entire
subtree of the current entry should be excluded and move point to the end
of the subtree."
  :group 'org-refile
  :type '(choice
	  (const nil)
	  (function)))

(defcustom org-refile-use-cache nil
  "Non-nil means cache refile targets to speed up the process.
\\<org-mode-map>\
The cache for a particular file will be updated automatically when
the buffer has been killed, or when any of the marker used for flagging
refile targets no longer points at a live buffer.
If you have added new entries to a buffer that might themselves be targets,
you need to clear the cache manually by pressing `C-0 \\[org-refile]' or,
if you find that easier, \
`\\[universal-argument] \\[universal-argument] \\[universal-argument] \
\\[org-refile]'."
  :group 'org-refile
  :version "24.1"
  :type 'boolean)

(defcustom org-refile-use-outline-path nil
  "Non-nil means provide refile targets as paths.
So a level 3 headline will be available as level1/level2/level3.

When the value is `file', also include the file name (without directory)
into the path.  In this case, you can also stop the completion after
the file name, to get entries inserted as top level in the file.

When `full-file-path', include the full file path.

When `buffer-name', use the buffer name."
  :group 'org-refile
  :type '(choice
	  (const :tag "Not" nil)
	  (const :tag "Yes" t)
	  (const :tag "Start with file name" file)
	  (const :tag "Start with full file path" full-file-path)
	  (const :tag "Start with buffer name" buffer-name)))

(defcustom org-outline-path-complete-in-steps t
  "Non-nil means complete the outline path in hierarchical steps.
When Org uses the refile interface to select an outline path (see
`org-refile-use-outline-path'), the completion of the path can be
done in a single go, or it can be done in steps down the headline
hierarchy.  Going in steps is probably the best if you do not use
a special completion package like `ido' or `icicles'.  However,
when using these packages, going in one step can be very fast,
while still showing the whole path to the entry."
  :group 'org-refile
  :type 'boolean)

(defcustom org-refile-allow-creating-parent-nodes nil
  "Non-nil means allow the creation of new nodes as refile targets.
New nodes are then created by adding \"/new node name\" to the completion
of an existing node.  When the value of this variable is `confirm',
new node creation must be confirmed by the user (recommended).
When nil, the completion must match an existing entry.

Note that, if the new heading is not seen by the criteria
listed in `org-refile-targets', multiple instances of the same
heading would be created by trying again to file under the new
heading."
  :group 'org-refile
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Always" t)
	  (const :tag "Prompt for confirmation" confirm)))

(defcustom org-refile-active-region-within-subtree nil
  "Non-nil means also refile active region within a subtree.

By default `org-refile' doesn't allow refiling regions if they
don't contain a set of subtrees, but it might be convenient to
do so sometimes: in that case, the first line of the region is
converted to a headline before refiling."
  :group 'org-refile
  :version "24.1"
  :type 'boolean)

(defgroup org-todo nil
  "Options concerning TODO items in Org mode."
  :tag "Org TODO"
  :group 'org)

(defgroup org-progress nil
  "Options concerning Progress logging in Org mode."
  :tag "Org Progress"
  :group 'org-time)

(defvar org-todo-interpretation-widgets
  '((:tag "Sequence (cycling hits every state)" sequence)
    (:tag "Type     (cycling directly to DONE)" type))
  "The available interpretation symbols for customizing `org-todo-keywords'.
Interested libraries should add to this list.")

(defcustom org-todo-keywords '((sequence "TODO" "DONE"))
  "List of TODO entry keyword sequences and their interpretation.
\\<org-mode-map>This is a list of sequences.

Each sequence starts with a symbol, either `sequence' or `type',
indicating if the keywords should be interpreted as a sequence of
action steps, or as different types of TODO items.  The first
keywords are states requiring action - these states will select a headline
for inclusion into the global TODO list Org produces.  If one of the
\"keywords\" is the vertical bar, \"|\", the remaining keywords
signify that no further action is necessary.  If \"|\" is not found,
the last keyword is treated as the only DONE state of the sequence.

The command `\\[org-todo]' cycles an entry through these states, and one
additional state where no keyword is present.  For details about this
cycling, see the manual.

TODO keywords and interpretation can also be set on a per-file basis with
the special #+SEQ_TODO and #+TYP_TODO lines.

Each keyword can optionally specify a character for fast state selection
\(in combination with the variable `org-use-fast-todo-selection')
and specifiers for state change logging, using the same syntax that
is used in the \"#+TODO:\" lines.  For example, \"WAIT(w)\" says that
the WAIT state can be selected with the \"w\" key.  \"WAIT(w!)\"
indicates to record a time stamp each time this state is selected.

Each keyword may also specify if a timestamp or a note should be
recorded when entering or leaving the state, by adding additional
characters in the parenthesis after the keyword.  This looks like this:
\"WAIT(w@/!)\".  \"@\" means to add a note (with time), \"!\" means to
record only the time of the state change.  With X and Y being either
\"@\" or \"!\", \"X/Y\" means use X when entering the state, and use
Y when leaving the state if and only if the *target* state does not
define X.  You may omit any of the fast-selection key or X or /Y,
so WAIT(w@), WAIT(w/@) and WAIT(@/@) are all valid.

For backward compatibility, this variable may also be just a list
of keywords.  In this case the interpretation (sequence or type) will be
taken from the (otherwise obsolete) variable `org-todo-interpretation'."
  :group 'org-todo
  :group 'org-keywords
  :type '(choice
	  (repeat :tag "Old syntax, just keywords"
		  (string :tag "Keyword"))
	  (repeat :tag "New syntax"
		  (cons
		   (choice
		    :tag "Interpretation"
		    ;;Quick and dirty way to see
		    ;;`org-todo-interpretations'.  This takes the
		    ;;place of item arguments
		    :convert-widget
		    (lambda (widget)
		      (widget-put widget
				  :args (mapcar
					 (lambda (x)
					   (widget-convert
					    (cons 'const x)))
					 org-todo-interpretation-widgets))
		      widget))
		   (repeat
		    (string :tag "Keyword"))))))

(defvar-local org-todo-keywords-1 nil
  "All TODO and DONE keywords active in a buffer.")
(defvar org-todo-keywords-for-agenda nil)
(defvar org-done-keywords-for-agenda nil)
(defvar org-todo-keyword-alist-for-agenda nil)
(defvar org-tag-alist-for-agenda nil
  "Alist of all tags from all agenda files.")
(defvar org-tag-groups-alist-for-agenda nil
  "Alist of all groups tags from all current agenda files.")
(defvar-local org-tag-groups-alist nil)
(defvar org-agenda-contributing-files nil)
(defvar-local org-current-tag-alist nil
  "Alist of all tag groups in current buffer.
This variable takes into consideration `org-tag-alist',
`org-tag-persistent-alist' and TAGS keywords in the buffer.")
(defvar-local org-not-done-keywords nil)
(defvar-local org-done-keywords nil)
(defvar-local org-todo-heads nil)
(defvar-local org-todo-sets nil)
(defvar-local org-todo-log-states nil)
(defvar-local org-todo-kwd-alist nil)
(defvar-local org-todo-key-alist nil)
(defvar-local org-todo-key-trigger nil)

(defcustom org-todo-interpretation 'sequence
  "Controls how TODO keywords are interpreted.
This variable is in principle obsolete and is only used for
backward compatibility, if the interpretation of todo keywords is
not given already in `org-todo-keywords'.  See that variable for
more information."
  :group 'org-todo
  :group 'org-keywords
  :type '(choice (const sequence)
		 (const type)))

(defcustom org-use-fast-todo-selection t
  "\\<org-mode-map>\
Non-nil means use the fast todo selection scheme with `\\[org-todo]'.
This variable describes if and under what circumstances the cycling
mechanism for TODO keywords will be replaced by a single-key, direct
selection scheme.

When nil, fast selection is never used.

When the symbol `prefix', it will be used when `org-todo' is called
with a prefix argument,  i.e. `\\[universal-argument] \\[org-todo]' \
in an Org buffer, and
`\\[universal-argument] t' in an agenda buffer.

When t, fast selection is used by default.  In this case, the prefix
argument forces cycling instead.

In all cases, the special interface is only used if access keys have
actually been assigned by the user, i.e. if keywords in the configuration
are followed by a letter in parenthesis, like TODO(t)."
  :group 'org-todo
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "By default" t)
	  (const :tag "Only with C-u C-c C-t" prefix)))

(defcustom org-provide-todo-statistics t
  "Non-nil means update todo statistics after insert and toggle.
ALL-HEADLINES means update todo statistics by including headlines
with no TODO keyword as well, counting them as not done.
A list of TODO keywords means the same, but skip keywords that are
not in this list.
When set to a list of two lists, the first list contains keywords
to consider as TODO keywords, the second list contains keywords
to consider as DONE keywords.

When this is set, todo statistics is updated in the parent of the
current entry each time a todo state is changed."
  :group 'org-todo
  :type '(choice
	  (const :tag "Yes, only for TODO entries" t)
	  (const :tag "Yes, including all entries" all-headlines)
	  (repeat :tag "Yes, for TODOs in this list"
		  (string :tag "TODO keyword"))
	  (list :tag "Yes, for TODOs and DONEs in these lists"
		(repeat (string :tag "TODO keyword"))
		(repeat (string :tag "DONE keyword")))
	  (other :tag "No TODO statistics" nil)))

(defcustom org-hierarchical-todo-statistics t
  "Non-nil means TODO statistics covers just direct children.
When nil, all entries in the subtree are considered.
This has only an effect if `org-provide-todo-statistics' is set.
To set this to nil for only a single subtree, use a COOKIE_DATA
property and include the word \"recursive\" into the value."
  :group 'org-todo
  :type 'boolean)

(defcustom org-after-todo-state-change-hook nil
  "Hook which is run after the state of a TODO item was changed.
The new state (a string with a TODO keyword, or nil) is available in the
Lisp variable `org-state'."
  :group 'org-todo
  :type 'hook)

(defvar org-blocker-hook nil
  "Hook for functions that are allowed to block a state change.

Functions in this hook should not modify the buffer.
Each function gets as its single argument a property list,
see `org-trigger-hook' for more information about this list.

If any of the functions in this hook returns nil, the state change
is blocked.")

(defvar org-trigger-hook nil
  "Hook for functions that are triggered by a state change.

Each function gets as its single argument a property list with at
least the following elements:

 (:type type-of-change :position pos-at-entry-start
  :from old-state :to new-state)

Depending on the type, more properties may be present.

This mechanism is currently implemented for:

TODO state changes
------------------
:type  todo-state-change
:from  previous state (keyword as a string), or nil, or a symbol
       `todo' or `done', to indicate the general type of state.
:to    new state, like in :from")

(defcustom org-enforce-todo-dependencies nil
  "Non-nil means undone TODO entries will block switching the parent to DONE.
Also, if a parent has an :ORDERED: property, switching an entry to DONE will
be blocked if any prior sibling is not yet done.
Finally, if the parent is blocked because of ordered siblings of its own,
the child will also be blocked."
  :set (lambda (var val)
	 (set var val)
	 (if val
	     (add-hook 'org-blocker-hook
		       'org-block-todo-from-children-or-siblings-or-parent)
	   (remove-hook 'org-blocker-hook
			'org-block-todo-from-children-or-siblings-or-parent)))
  :group 'org-todo
  :type 'boolean)

(defcustom org-enforce-todo-checkbox-dependencies nil
  "Non-nil means unchecked boxes will block switching the parent to DONE.
When this is nil, checkboxes have no influence on switching TODO states.
When non-nil, you first need to check off all check boxes before the TODO
entry can be switched to DONE.
This variable needs to be set before org.el is loaded, and you need to
restart Emacs after a change to make the change effective.  The only way
to change is while Emacs is running is through the customize interface."
  :set (lambda (var val)
	 (set var val)
	 (if val
	     (add-hook 'org-blocker-hook
		       'org-block-todo-from-checkboxes)
	   (remove-hook 'org-blocker-hook
			'org-block-todo-from-checkboxes)))
  :group 'org-todo
  :type 'boolean)

(defcustom org-treat-insert-todo-heading-as-state-change nil
  "Non-nil means inserting a TODO heading is treated as state change.
So when the command `\\[org-insert-todo-heading]' is used, state change
logging will apply if appropriate.  When nil, the new TODO item will
be inserted directly, and no logging will take place."
  :group 'org-todo
  :type 'boolean)

(defcustom org-treat-S-cursor-todo-selection-as-state-change t
  "Non-nil means switching TODO states with S-cursor counts as state change.
This is the default behavior.  However, setting this to nil allows a
convenient way to select a TODO state and bypass any logging associated
with that."
  :group 'org-todo
  :type 'boolean)

(defcustom org-todo-state-tags-triggers nil
  "Tag changes that should be triggered by TODO state changes.
This is a list.  Each entry is

  (state-change (tag . flag) .......)

State-change can be a string with a state, and empty string to indicate the
state that has no TODO keyword, or it can be one of the symbols `todo'
or `done', meaning any not-done or done state, respectively."
  :group 'org-todo
  :group 'org-tags
  :type '(repeat
	  (cons (choice :tag "When changing to"
			(const :tag "Not-done state" todo)
			(const :tag "Done state" done)
			(string :tag "State"))
		(repeat
		 (cons :tag "Tag action"
		       (string :tag "Tag")
		       (choice (const :tag "Add" t) (const :tag "Remove" nil)))))))

(defcustom org-log-done nil
  "Information to record when a task moves to the DONE state.

Possible values are:

nil     Don't add anything, just change the keyword
time    Add a time stamp to the task
note    Prompt for a note and add it with template `org-log-note-headings'

This option can also be set with on a per-file-basis with

   #+STARTUP: nologdone
   #+STARTUP: logdone
   #+STARTUP: lognotedone

You can have local logging settings for a subtree by setting the LOGGING
property to one or more of these keywords."
  :group 'org-todo
  :group 'org-progress
  :type '(choice
	  (const :tag "No logging" nil)
	  (const :tag "Record CLOSED timestamp" time)
	  (const :tag "Record CLOSED timestamp with note." note)))

;; Normalize old uses of org-log-done.
(cond
 ((eq org-log-done t) (setq org-log-done 'time))
 ((and (listp org-log-done) (memq 'done org-log-done))
  (setq org-log-done 'note)))

(defcustom org-log-reschedule nil
  "Information to record when the scheduling date of a tasks is modified.

Possible values are:

nil     Don't add anything, just change the date
time    Add a time stamp to the task
note    Prompt for a note and add it with template `org-log-note-headings'

This option can also be set with on a per-file-basis with

   #+STARTUP: nologreschedule
   #+STARTUP: logreschedule
   #+STARTUP: lognotereschedule"
  :group 'org-todo
  :group 'org-progress
  :type '(choice
	  (const :tag "No logging" nil)
	  (const :tag "Record timestamp" time)
	  (const :tag "Record timestamp with note." note)))

(defcustom org-log-redeadline nil
  "Information to record when the deadline date of a tasks is modified.

Possible values are:

nil     Don't add anything, just change the date
time    Add a time stamp to the task
note    Prompt for a note and add it with template `org-log-note-headings'

This option can also be set with on a per-file-basis with

   #+STARTUP: nologredeadline
   #+STARTUP: logredeadline
   #+STARTUP: lognoteredeadline

You can have local logging settings for a subtree by setting the LOGGING
property to one or more of these keywords."
  :group 'org-todo
  :group 'org-progress
  :type '(choice
	  (const :tag "No logging" nil)
	  (const :tag "Record timestamp" time)
	  (const :tag "Record timestamp with note." note)))

(defcustom org-log-note-clock-out nil
  "Non-nil means record a note when clocking out of an item.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: lognoteclock-out
   #+STARTUP: nolognoteclock-out"
  :group 'org-todo
  :group 'org-progress
  :type 'boolean)

(defcustom org-log-done-with-time t
  "Non-nil means the CLOSED time stamp will contain date and time.
When nil, only the date will be recorded."
  :group 'org-progress
  :type 'boolean)

(defcustom org-log-note-headings
  '((done .  "CLOSING NOTE %t")
    (state . "State %-12s from %-12S %t")
    (note .  "Note taken on %t")
    (reschedule .  "Rescheduled from %S on %t")
    (delschedule .  "Not scheduled, was %S on %t")
    (redeadline .  "New deadline from %S on %t")
    (deldeadline .  "Removed deadline, was %S on %t")
    (refile . "Refiled on %t")
    (clock-out . ""))
  "Headings for notes added to entries.

The value is an alist, with the car being a symbol indicating the
note context, and the cdr is the heading to be used.  The heading
may also be the empty string.  The following placeholders can be
used:

  %t  a time stamp.
  %T  an active time stamp instead the default inactive one
  %d  a short-format time stamp.
  %D  an active short-format time stamp.
  %s  the new TODO state or time stamp (inactive), in double quotes.
  %S  the old TODO state or time stamp (inactive), in double quotes.
  %u  the user name.
  %U  full user name.

In fact, it is not a good idea to change the `state' entry,
because Agenda Log mode depends on the format of these entries."
  :group  'org-todo
  :group  'org-progress
  :type '(list :greedy t
	       (cons (const :tag "Heading when closing an item" done) string)
	       (cons (const :tag
			    "Heading when changing todo state (todo sequence only)"
			    state) string)
	       (cons (const :tag "Heading when just taking a note" note) string)
	       (cons (const :tag "Heading when rescheduling" reschedule) string)
	       (cons (const :tag "Heading when an item is no longer scheduled" delschedule) string)
	       (cons (const :tag "Heading when changing deadline"  redeadline) string)
	       (cons (const :tag "Heading when deleting a deadline" deldeadline) string)
	       (cons (const :tag "Heading when refiling" refile) string)
	       (cons (const :tag "Heading when clocking out" clock-out) string)))

(unless (assq 'note org-log-note-headings)
  (push '(note . "%t") org-log-note-headings))

(defcustom org-log-into-drawer nil
  "Non-nil means insert state change notes and time stamps into a drawer.
When nil, state changes notes will be inserted after the headline and
any scheduling and clock lines, but not inside a drawer.

The value of this variable should be the name of the drawer to use.
LOGBOOK is proposed as the default drawer for this purpose, you can
also set this to a string to define the drawer of your choice.

A value of t is also allowed, representing \"LOGBOOK\".

A value of t or nil can also be set with on a per-file-basis with

   #+STARTUP: logdrawer
   #+STARTUP: nologdrawer

If this variable is set, `org-log-state-notes-insert-after-drawers'
will be ignored.

You can set the property LOG_INTO_DRAWER to overrule this setting for
a subtree.

Do not check directly this variable in a Lisp program.  Call
function `org-log-into-drawer' instead."
  :group 'org-todo
  :group 'org-progress
  :type '(choice
	  (const :tag "Not into a drawer" nil)
	  (const :tag "LOGBOOK" t)
	  (string :tag "Other")))

(defvaralias 'org-log-state-notes-into-drawer 'org-log-into-drawer)

(defun org-log-into-drawer ()
  "Name of the log drawer, as a string, or nil.
This is the value of `org-log-into-drawer'.  However, if the
current entry has or inherits a LOG_INTO_DRAWER property, it will
be used instead of the default value."
  (let ((p (org-entry-get nil "LOG_INTO_DRAWER" 'inherit t)))
    (cond ((equal p "nil") nil)
	  ((equal p "t") "LOGBOOK")
	  ((stringp p) p)
	  (p "LOGBOOK")
	  ((stringp org-log-into-drawer) org-log-into-drawer)
	  (org-log-into-drawer "LOGBOOK"))))

(defcustom org-log-state-notes-insert-after-drawers nil
  "Non-nil means insert state change notes after any drawers in entry.
Only the drawers that *immediately* follow the headline and the
deadline/scheduled line are skipped.
When nil, insert notes right after the heading and perhaps the line
with deadline/scheduling if present.

This variable will have no effect if `org-log-into-drawer' is
set."
  :group 'org-todo
  :group 'org-progress
  :type 'boolean)

(defcustom org-log-states-order-reversed t
  "Non-nil means the latest state note will be directly after heading.
When nil, the state change notes will be ordered according to time.

This option can also be set with on a per-file-basis with

   #+STARTUP: logstatesreversed
   #+STARTUP: nologstatesreversed"
  :group 'org-todo
  :group 'org-progress
  :type 'boolean)

(defcustom org-todo-repeat-to-state nil
  "The TODO state to which a repeater should return the repeating task.
By default this is the first task in a TODO sequence, or the previous state
in a TODO_TYP set.  But you can specify another task here.
alternatively, set the :REPEAT_TO_STATE: property of the entry."
  :group 'org-todo
  :version "24.1"
  :type '(choice (const :tag "Head of sequence" nil)
		 (string :tag "Specific state")))

(defcustom org-log-repeat 'time
  "Non-nil means record moving through the DONE state when triggering repeat.
An auto-repeating task is immediately switched back to TODO when
marked DONE.  If you are not logging state changes (by adding \"@\"
or \"!\" to the TODO keyword definition), or set `org-log-done' to
record a closing note, there will be no record of the task moving
through DONE.  This variable forces taking a note anyway.

nil     Don't force a record
time    Record a time stamp
note    Prompt for a note and add it with template `org-log-note-headings'

This option can also be set with on a per-file-basis with

   #+STARTUP: nologrepeat
   #+STARTUP: logrepeat
   #+STARTUP: lognoterepeat

You can have local logging settings for a subtree by setting the LOGGING
property to one or more of these keywords."
  :group 'org-todo
  :group 'org-progress
  :type '(choice
	  (const :tag "Don't force a record" nil)
	  (const :tag "Force recording the DONE state" time)
	  (const :tag "Force recording a note with the DONE state" note)))


(defgroup org-priorities nil
  "Priorities in Org mode."
  :tag "Org Priorities"
  :group 'org-todo)

(defcustom org-enable-priority-commands t
  "Non-nil means priority commands are active.
When nil, these commands will be disabled, so that you never accidentally
set a priority."
  :group 'org-priorities
  :type 'boolean)

(defcustom org-highest-priority ?A
  "The highest priority of TODO items.  A character like ?A, ?B etc.
Must have a smaller ASCII number than `org-lowest-priority'."
  :group 'org-priorities
  :type 'character)

(defcustom org-lowest-priority ?C
  "The lowest priority of TODO items.  A character like ?A, ?B etc.
Must have a larger ASCII number than `org-highest-priority'."
  :group 'org-priorities
  :type 'character)

(defcustom org-default-priority ?B
  "The default priority of TODO items.
This is the priority an item gets if no explicit priority is given.
When starting to cycle on an empty priority the first step in the cycle
depends on `org-priority-start-cycle-with-default'.  The resulting first
step priority must not exceed the range from `org-highest-priority' to
`org-lowest-priority' which means that `org-default-priority' has to be
in this range exclusive or inclusive the range boundaries.  Else the
first step refuses to set the default and the second will fall back
to (depending on the command used) the highest or lowest priority."
  :group 'org-priorities
  :type 'character)

(defcustom org-priority-start-cycle-with-default t
  "Non-nil means start with default priority when starting to cycle.
When this is nil, the first step in the cycle will be (depending on the
command used) one higher or lower than the default priority.
See also `org-default-priority'."
  :group 'org-priorities
  :type 'boolean)

(defcustom org-get-priority-function nil
  "Function to extract the priority from a string.
The string is normally the headline.  If this is nil Org computes the
priority from the priority cookie like [#A] in the headline.  It returns
an integer, increasing by 1000 for each priority level.
The user can set a different function here, which should take a string
as an argument and return the numeric priority."
  :group 'org-priorities
  :version "24.1"
  :type '(choice
	  (const nil)
	  (function)))

(defgroup org-time nil
  "Options concerning time stamps and deadlines in Org mode."
  :tag "Org Time"
  :group 'org)

(defcustom org-time-stamp-rounding-minutes '(0 5)
  "Number of minutes to round time stamps to.
\\<org-mode-map>\
These are two values, the first applies when first creating a time stamp.
The second applies when changing it with the commands `S-up' and `S-down'.
When changing the time stamp, this means that it will change in steps
of N minutes, as given by the second value.

When a setting is 0 or 1, insert the time unmodified.  Useful rounding
numbers should be factors of 60, so for example 5, 10, 15.

When this is larger than 1, you can still force an exact time stamp by using
a double prefix argument to a time stamp command like \
`\\[org-time-stamp]' or `\\[org-time-stamp-inactive],
and by using a prefix arg to `S-up/down' to specify the exact number
of minutes to shift."
  :group 'org-time
  :get (lambda (var) ; Make sure both elements are there
	 (if (integerp (default-value var))
	     (list (default-value var) 5)
	   (default-value var)))
  :type '(list
	  (integer :tag "when inserting times")
	  (integer :tag "when modifying times")))

;; Normalize old customizations of this variable.
(when (integerp org-time-stamp-rounding-minutes)
  (setq org-time-stamp-rounding-minutes
	(list org-time-stamp-rounding-minutes
	      org-time-stamp-rounding-minutes)))

(defcustom org-display-custom-times nil
  "Non-nil means overlay custom formats over all time stamps.
The formats are defined through the variable `org-time-stamp-custom-formats'.
To turn this on on a per-file basis, insert anywhere in the file:
   #+STARTUP: customtime"
  :group 'org-time
  :set 'set-default
  :type 'sexp)
(make-variable-buffer-local 'org-display-custom-times)

(defcustom org-time-stamp-custom-formats
  '("<%m/%d/%y %a>" . "<%m/%d/%y %a %H:%M>") ; american
  "Custom formats for time stamps.  See `format-time-string' for the syntax.
These are overlaid over the default ISO format if the variable
`org-display-custom-times' is set.  Time like %H:%M should be at the
end of the second format.  The custom formats are also honored by export
commands, if custom time display is turned on at the time of export."
  :group 'org-time
  :type 'sexp)

(defun org-time-stamp-format (&optional long inactive)
  "Get the right format for a time string."
  (let ((f (if long (cdr org-time-stamp-formats)
	     (car org-time-stamp-formats))))
    (if inactive
	(concat "[" (substring f 1 -1) "]")
      f)))

(defcustom org-deadline-warning-days 14
  "Number of days before expiration during which a deadline becomes active.
This variable governs the display in sparse trees and in the agenda.
When 0 or negative, it means use this number (the absolute value of it)
even if a deadline has a different individual lead time specified.

Custom commands can set this variable in the options section."
  :group 'org-time
  :group 'org-agenda-daily/weekly
  :type 'integer)

(defcustom org-scheduled-delay-days 0
  "Number of days before a scheduled item becomes active.
This variable governs the display in sparse trees and in the agenda.
The default value (i.e. 0) means: don't delay scheduled item.
When negative, it means use this number (the absolute value of it)
even if a scheduled item has a different individual delay time
specified.

Custom commands can set this variable in the options section."
  :group 'org-time
  :group 'org-agenda-daily/weekly
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'integer)

(defcustom org-read-date-prefer-future t
  "Non-nil means assume future for incomplete date input from user.
This affects the following situations:
1. The user gives a month but not a year.
   For example, if it is April and you enter \"feb 2\", this will be read
   as Feb 2, *next* year.  \"May 5\", however, will be this year.
2. The user gives a day, but no month.
   For example, if today is the 15th, and you enter \"3\", Org will read
   this as the third of *next* month.  However, if you enter \"17\",
   it will be considered as *this* month.

If you set this variable to the symbol `time', then also the following
will work:

3. If the user gives a time.
   If the time is before now, it will be interpreted as tomorrow.

Currently none of this works for ISO week specifications.

When this option is nil, the current day, month and year will always be
used as defaults.

See also `org-agenda-jump-prefer-future'."
  :group 'org-time
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Check month and day" t)
	  (const :tag "Check month, day, and time" time)))

(defcustom org-agenda-jump-prefer-future 'org-read-date-prefer-future
  "Should the agenda jump command prefer the future for incomplete dates?
The default is to do the same as configured in `org-read-date-prefer-future'.
But you can also set a deviating value here.
This may t or nil, or the symbol `org-read-date-prefer-future'."
  :group 'org-agenda
  :group 'org-time
  :version "24.1"
  :type '(choice
	  (const :tag "Use org-read-date-prefer-future"
		 org-read-date-prefer-future)
	  (const :tag "Never" nil)
	  (const :tag "Always" t)))

(defcustom org-read-date-force-compatible-dates t
  "Should date/time prompt force dates that are guaranteed to work in Emacs?

Depending on the system Emacs is running on, certain dates cannot
be represented with the type used internally to represent time.
Dates between 1970-1-1 and 2038-1-1 can always be represented
correctly.  Some systems allow for earlier dates, some for later,
some for both.  One way to find out it to insert any date into an
Org buffer, putting the cursor on the year and hitting S-up and
S-down to test the range.

When this variable is set to t, the date/time prompt will not let
you specify dates outside the 1970-2037 range, so it is certain that
these dates will work in whatever version of Emacs you are
running, and also that you can move a file from one Emacs implementation
to another.  WHenever Org is forcing the year for you, it will display
a message and beep.

When this variable is nil, Org will check if the date is
representable in the specific Emacs implementation you are using.
If not, it will force a year, usually the current year, and beep
to remind you.  Currently this setting is not recommended because
the likelihood that you will open your Org files in an Emacs that
has limited date range is not negligible.

A workaround for this problem is to use diary sexp dates for time
stamps outside of this range."
  :group 'org-time
  :version "24.1"
  :type 'boolean)

(defcustom org-read-date-display-live t
  "Non-nil means display current interpretation of date prompt live.
This display will be in an overlay, in the minibuffer."
  :group 'org-time
  :type 'boolean)

(defcustom org-read-date-popup-calendar t
  "Non-nil means pop up a calendar when prompting for a date.
In the calendar, the date can be selected with mouse-1.  However, the
minibuffer will also be active, and you can simply enter the date as well.
When nil, only the minibuffer will be available."
  :group 'org-time
  :type 'boolean)
(defvaralias 'org-popup-calendar-for-date-prompt
  'org-read-date-popup-calendar)

(defcustom org-extend-today-until 0
  "The hour when your day really ends.  Must be an integer.
This has influence for the following applications:
- When switching the agenda to \"today\".  It it is still earlier than
  the time given here, the day recognized as TODAY is actually yesterday.
- When a date is read from the user and it is still before the time given
  here, the current date and time will be assumed to be yesterday, 23:59.
  Also, timestamps inserted in capture templates follow this rule.

IMPORTANT:  This is a feature whose implementation is and likely will
remain incomplete.  Really, it is only here because past midnight seems to
be the favorite working time of John Wiegley :-)"
  :group 'org-time
  :type 'integer)

(defcustom org-use-effective-time nil
  "If non-nil, consider `org-extend-today-until' when creating timestamps.
For example, if `org-extend-today-until' is 8, and it's 4am, then the
\"effective time\" of any timestamps between midnight and 8am will be
23:59 of the previous day."
  :group 'org-time
  :version "24.1"
  :type 'boolean)

(defcustom org-use-last-clock-out-time-as-effective-time nil
  "When non-nil, use the last clock out time for `org-todo'.
Note that this option has precedence over the combined use of
`org-use-effective-time' and `org-extend-today-until'."
  :group 'org-time
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-edit-timestamp-down-means-later nil
  "Non-nil means S-down will increase the time in a time stamp.
When nil, S-up will increase."
  :group 'org-time
  :type 'boolean)

(defcustom org-calendar-follow-timestamp-change t
  "Non-nil means make the calendar window follow timestamp changes.
When a timestamp is modified and the calendar window is visible, it will be
moved to the new date."
  :group 'org-time
  :type 'boolean)

(defgroup org-tags nil
  "Options concerning tags in Org mode."
  :tag "Org Tags"
  :group 'org)

(defcustom org-tag-alist nil
  "Default tags available in Org files.

The value of this variable is an alist.  Associations either:

  (TAG)
  (TAG . SELECT)
  (SPECIAL)

where TAG is a tag as a string, SELECT is character, used to
select that tag through the fast tag selection interface, and
SPECIAL is one of the following keywords: `:startgroup',
`:startgrouptag', `:grouptags', `:engroup', `:endgrouptag' or
`:newline'.  These keywords are used to define a hierarchy of
tags.  See manual for details.

When this variable is nil, Org mode bases tag input on what is
already in the buffer.  The value can be overridden locally by
using a TAGS keyword, e.g.,

  #+TAGS: tag1 tag2

See also `org-tag-persistent-alist' to sidestep this behavior."
  :group 'org-tags
  :type '(repeat
	  (choice
	   (cons :tag "Tag with key"
		 (string    :tag "Tag name")
		 (character :tag "Access char"))
	   (list :tag "Tag" (string :tag "Tag name"))
	   (const :tag "Start radio group" (:startgroup))
	   (const :tag "Start tag group, non distinct" (:startgrouptag))
	   (const :tag "Group tags delimiter" (:grouptags))
	   (const :tag "End radio group" (:endgroup))
	   (const :tag "End tag group, non distinct" (:endgrouptag))
	   (const :tag "New line" (:newline)))))

(defcustom org-tag-persistent-alist nil
  "Tags always available in Org files.

The value of this variable is an alist.  Associations either:

  (TAG)
  (TAG . SELECT)
  (SPECIAL)

where TAG is a tag as a string, SELECT is a character, used to
select that tag through the fast tag selection interface, and
SPECIAL is one of the following keywords: `:startgroup',
`:startgrouptag', `:grouptags', `:engroup', `:endgrouptag' or
`:newline'.  These keywords are used to define a hierarchy of
tags.  See manual for details.

Unlike to `org-tag-alist', tags defined in this variable do not
depend on a local TAGS keyword.  Instead, to disable these tags
on a per-file basis, insert anywhere in the file:

  #+STARTUP: noptag"
  :group 'org-tags
  :type '(repeat
	  (choice
	   (cons :tag "Tag with key"
		 (string    :tag "Tag name")
		 (character :tag "Access char"))
	   (list :tag "Tag" (string :tag "Tag name"))
	   (const :tag "Start radio group" (:startgroup))
	   (const :tag "Start tag group, non distinct" (:startgrouptag))
	   (const :tag "Group tags delimiter" (:grouptags))
	   (const :tag "End radio group" (:endgroup))
	   (const :tag "End tag group, non distinct" (:endgrouptag))
	   (const :tag "New line" (:newline)))))

(defcustom org-complete-tags-always-offer-all-agenda-tags nil
  "If non-nil, always offer completion for all tags of all agenda files.
Instead of customizing this variable directly, you might want to
set it locally for capture buffers, because there no list of
tags in that file can be created dynamically (there are none).

  (add-hook \\='org-capture-mode-hook
            (lambda ()
              (setq-local org-complete-tags-always-offer-all-agenda-tags t)))"
  :group 'org-tags
  :version "24.1"
  :type 'boolean)

(defvar org-file-tags nil
  "List of tags that can be inherited by all entries in the file.
The tags will be inherited if the variable `org-use-tag-inheritance'
says they should be.
This variable is populated from #+FILETAGS lines.")

(defcustom org-use-fast-tag-selection 'auto
  "Non-nil means use fast tag selection scheme.
This is a special interface to select and deselect tags with single keys.
When nil, fast selection is never used.
When the symbol `auto', fast selection is used if and only if selection
characters for tags have been configured, either through the variable
`org-tag-alist' or through a #+TAGS line in the buffer.
When t, fast selection is always used and selection keys are assigned
automatically if necessary."
  :group 'org-tags
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (const :tag "When selection characters are configured" auto)))

(defcustom org-fast-tag-selection-single-key nil
  "Non-nil means fast tag selection exits after first change.
When nil, you have to press RET to exit it.
During fast tag selection, you can toggle this flag with `C-c'.
This variable can also have the value `expert'.  In this case, the window
displaying the tags menu is not even shown, until you press C-c again."
  :group 'org-tags
  :type '(choice
	  (const :tag "No" nil)
	  (const :tag "Yes" t)
	  (const :tag "Expert" expert)))

(defvar org-fast-tag-selection-include-todo nil
  "Non-nil means fast tags selection interface will also offer TODO states.
This is an undocumented feature, you should not rely on it.")

(defcustom org-tags-column -77
  "The column to which tags should be indented in a headline.
If this number is positive, it specifies the column.  If it is negative,
it means that the tags should be flushright to that column.  For example,
-80 works well for a normal 80 character screen.
When 0, place tags directly after headline text, with only one space in
between."
  :group 'org-tags
  :type 'integer)

(defcustom org-auto-align-tags t
  "Non-nil keeps tags aligned when modifying headlines.
Some operations (i.e. demoting) change the length of a headline and
therefore shift the tags around.  With this option turned on, after
each such operation the tags are again aligned to `org-tags-column'."
  :group 'org-tags
  :type 'boolean)

(defcustom org-use-tag-inheritance t
  "Non-nil means tags in levels apply also for sublevels.
When nil, only the tags directly given in a specific line apply there.
This may also be a list of tags that should be inherited, or a regexp that
matches tags that should be inherited.  Additional control is possible
with the variable  `org-tags-exclude-from-inheritance' which gives an
explicit list of tags to be excluded from inheritance, even if the value of
`org-use-tag-inheritance' would select it for inheritance.

If this option is t, a match early-on in a tree can lead to a large
number of matches in the subtree when constructing the agenda or creating
a sparse tree.  If you only want to see the first match in a tree during
a search, check out the variable `org-tags-match-list-sublevels'."
  :group 'org-tags
  :type '(choice
	  (const :tag "Not" nil)
	  (const :tag "Always" t)
	  (repeat :tag "Specific tags" (string :tag "Tag"))
	  (regexp :tag "Tags matched by regexp")))

(defcustom org-tags-exclude-from-inheritance nil
  "List of tags that should never be inherited.
This is a way to exclude a few tags from inheritance.  For way to do
the opposite, to actively allow inheritance for selected tags,
see the variable `org-use-tag-inheritance'."
  :group 'org-tags
  :type '(repeat (string :tag "Tag")))

(defun org-tag-inherit-p (tag)
  "Check if TAG is one that should be inherited."
  (cond
   ((member tag org-tags-exclude-from-inheritance) nil)
   ((eq org-use-tag-inheritance t) t)
   ((not org-use-tag-inheritance) nil)
   ((stringp org-use-tag-inheritance)
    (string-match org-use-tag-inheritance tag))
   ((listp org-use-tag-inheritance)
    (member tag org-use-tag-inheritance))
   (t (error "Invalid setting of `org-use-tag-inheritance'"))))

(defcustom org-tags-match-list-sublevels t
  "Non-nil means list also sublevels of headlines matching a search.
This variable applies to tags/property searches, and also to stuck
projects because this search is based on a tags match as well.

When set to the symbol `indented', sublevels are indented with
leading dots.

Because of tag inheritance (see variable `org-use-tag-inheritance'),
the sublevels of a headline matching a tag search often also match
the same search.  Listing all of them can create very long lists.
Setting this variable to nil causes subtrees of a match to be skipped.

This variable is semi-obsolete and probably should always be true.  It
is better to limit inheritance to certain tags using the variables
`org-use-tag-inheritance' and `org-tags-exclude-from-inheritance'."
  :group 'org-tags
  :type '(choice
	  (const :tag "No, don't list them" nil)
	  (const :tag "Yes, do list them" t)
	  (const :tag "List them, indented with leading dots" indented)))

(defcustom org-tags-sort-function nil
  "When set, tags are sorted using this function as a comparator."
  :group 'org-tags
  :type '(choice
	  (const :tag "No sorting" nil)
	  (const :tag "Alphabetical" string<)
	  (const :tag "Reverse alphabetical" string>)
	  (function :tag "Custom function" nil)))

(defvar org-tags-history nil
  "History of minibuffer reads for tags.")
(defvar org-last-tags-completion-table nil
  "The last used completion table for tags.")
(defvar org-after-tags-change-hook nil
  "Hook that is run after the tags in a line have changed.")

(defgroup org-properties nil
  "Options concerning properties in Org mode."
  :tag "Org Properties"
  :group 'org)

(defcustom org-property-format "%-10s %s"
  "How property key/value pairs should be formatted by `indent-line'.
When `indent-line' hits a property definition, it will format the line
according to this format, mainly to make sure that the values are
lined-up with respect to each other."
  :group 'org-properties
  :type 'string)

(defcustom org-properties-postprocess-alist nil
  "Alist of properties and functions to adjust inserted values.
Elements of this alist must be of the form

  ([string] [function])

where [string] must be a property name and [function] must be a
lambda expression: this lambda expression must take one argument,
the value to adjust, and return the new value as a string.

For example, this element will allow the property \"Remaining\"
to be updated wrt the relation between the \"Effort\" property
and the clock summary:

 ((\"Remaining\" (lambda(value)
                   (let ((clocksum (org-clock-sum-current-item))
                         (effort (org-duration-to-minutes
                                   (org-entry-get (point) \"Effort\"))))
                     (org-minutes-to-clocksum-string (- effort clocksum))))))"
  :group 'org-properties
  :version "24.1"
  :type '(alist :key-type (string     :tag "Property")
		:value-type (function :tag "Function")))

(defcustom org-use-property-inheritance nil
  "Non-nil means properties apply also for sublevels.

This setting is chiefly used during property searches.  Turning it on can
cause significant overhead when doing a search, which is why it is not
on by default.

When nil, only the properties directly given in the current entry count.
When t, every property is inherited.  The value may also be a list of
properties that should have inheritance, or a regular expression matching
properties that should be inherited.

However, note that some special properties use inheritance under special
circumstances (not in searches).  Examples are CATEGORY, ARCHIVE, COLUMNS,
and the properties ending in \"_ALL\" when they are used as descriptor
for valid values of a property.

Note for programmers:
When querying an entry with `org-entry-get',  you can control if inheritance
should be used.  By default, `org-entry-get' looks only at the local
properties.  You can request inheritance by setting the inherit argument
to t (to force inheritance) or to `selective' (to respect the setting
in this variable)."
  :group 'org-properties
  :type '(choice
	  (const :tag "Not" nil)
	  (const :tag "Always" t)
	  (repeat :tag "Specific properties" (string :tag "Property"))
	  (regexp :tag "Properties matched by regexp")))

(defun org-property-inherit-p (property)
  "Return a non-nil value if PROPERTY should be inherited."
  (cond
   ((eq org-use-property-inheritance t) t)
   ((not org-use-property-inheritance) nil)
   ((stringp org-use-property-inheritance)
    (string-match org-use-property-inheritance property))
   ((listp org-use-property-inheritance)
    (member-ignore-case property org-use-property-inheritance))
   (t (error "Invalid setting of `org-use-property-inheritance'"))))

(defcustom org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS"
  "The default column format, if no other format has been defined.
This variable can be set on the per-file basis by inserting a line

#+COLUMNS: %25ITEM ....."
  :group 'org-properties
  :type 'string)

(defcustom org-columns-ellipses ".."
  "The ellipses to be used when a field in column view is truncated.
When this is the empty string, as many characters as possible are shown,
but then there will be no visual indication that the field has been truncated.
When this is a string of length N, the last N characters of a truncated
field are replaced by this string.  If the column is narrower than the
ellipses string, only part of the ellipses string will be shown."
  :group 'org-properties
  :type 'string)

(defconst org-global-properties-fixed
  '(("VISIBILITY_ALL" . "folded children content all")
    ("CLOCK_MODELINE_TOTAL_ALL" . "current today repeat all auto"))
  "List of property/value pairs that can be inherited by any entry.

These are fixed values, for the preset properties.  The user variable
that can be used to add to this list is `org-global-properties'.

The entries in this list are cons cells where the car is a property
name and cdr is a string with the value.  If the value represents
multiple items like an \"_ALL\" property, separate the items by
spaces.")

(defcustom org-global-properties nil
  "List of property/value pairs that can be inherited by any entry.

This list will be combined with the constant `org-global-properties-fixed'.

The entries in this list are cons cells where the car is a property
name and cdr is a string with the value.

You can set buffer-local values for the same purpose in the variable
`org-file-properties' this by adding lines like

#+PROPERTY: NAME VALUE"
  :group 'org-properties
  :type '(repeat
	  (cons (string :tag "Property")
		(string :tag "Value"))))

(defvar-local org-file-properties nil
  "List of property/value pairs that can be inherited by any entry.
Valid for the current buffer.
This variable is populated from #+PROPERTY lines.")

(defgroup org-agenda nil
  "Options concerning agenda views in Org mode."
  :tag "Org Agenda"
  :group 'org)

(defvar-local org-category nil
  "Variable used by org files to set a category for agenda display.
Such files should use a file variable to set it, for example

#   -*- mode: org; org-category: \"ELisp\"

or contain a special line

#+CATEGORY: ELisp

If the file does not specify a category, then file's base name
is used instead.")
(put 'org-category 'safe-local-variable (lambda (x) (or (symbolp x) (stringp x))))

(defcustom org-agenda-files nil
  "The files to be used for agenda display.

If an entry is a directory, all files in that directory that are matched
by `org-agenda-file-regexp' will be part of the file list.

If the value of the variable is not a list but a single file name, then
the list of agenda files is actually stored and maintained in that file,
one agenda file per line.  In this file paths can be given relative to
`org-directory'.  Tilde expansion and environment variable substitution
are also made.

Entries may be added to this list with `\\[org-agenda-file-to-front]'
and removed with `\\[org-remove-file]'."
  :group 'org-agenda
  :type '(choice
	  (repeat :tag "List of files and directories" file)
	  (file :tag "Store list in a file\n" :value "~/.agenda_files")))

(defcustom org-agenda-file-regexp "\\`[^.].*\\.org\\'"
  "Regular expression to match files for `org-agenda-files'.
If any element in the list in that variable contains a directory instead
of a normal file, all files in that directory that are matched by this
regular expression will be included."
  :group 'org-agenda
  :type 'regexp)

(defcustom org-agenda-text-search-extra-files nil
  "List of extra files to be searched by text search commands.
These files will be searched in addition to the agenda files by the
commands `org-search-view' (`\\[org-agenda] s') \
and `org-occur-in-agenda-files'.
Note that these files will only be searched for text search commands,
not for the other agenda views like todo lists, tag searches or the weekly
agenda.  This variable is intended to list notes and possibly archive files
that should also be searched by these two commands.
In fact, if the first element in the list is the symbol `agenda-archives',
then all archive files of all agenda files will be added to the search
scope."
  :group 'org-agenda
  :type '(set :greedy t
	      (const :tag "Agenda Archives" agenda-archives)
	      (repeat :inline t (file))))

(defvaralias 'org-agenda-multi-occur-extra-files
  'org-agenda-text-search-extra-files)

(defcustom org-agenda-skip-unavailable-files nil
  "Non-nil means to just skip non-reachable files in `org-agenda-files'.
A nil value means to remove them, after a query, from the list."
  :group 'org-agenda
  :type 'boolean)

(defcustom org-calendar-to-agenda-key [?c]
  "The key to be installed in `calendar-mode-map' for switching to the agenda.
The command `org-calendar-goto-agenda' will be bound to this key.  The
default is the character `c' because then `c' can be used to switch back and
forth between agenda and calendar."
  :group 'org-agenda
  :type 'sexp)

(defcustom org-calendar-insert-diary-entry-key [?i]
  "The key to be installed in `calendar-mode-map' for adding diary entries.
This option is irrelevant until `org-agenda-diary-file' has been configured
to point to an Org file.  When that is the case, the command
`org-agenda-diary-entry' will be bound to the key given here, by default
`i'.  In the calendar, `i' normally adds entries to `diary-file'.  So
if you want to continue doing this, you need to change this to a different
key."
  :group 'org-agenda
  :type 'sexp)

(defcustom org-agenda-diary-file 'diary-file
  "File to which to add new entries with the `i' key in agenda and calendar.
When this is the symbol `diary-file', the functionality in the Emacs
calendar will be used to add entries to the `diary-file'.  But when this
points to a file, `org-agenda-diary-entry' will be used instead."
  :group 'org-agenda
  :type '(choice
	  (const :tag "The standard Emacs diary file" diary-file)
	  (file :tag "Special Org file diary entries")))

(eval-after-load "calendar"
  '(progn
     (org-defkey calendar-mode-map org-calendar-to-agenda-key
		 'org-calendar-goto-agenda)
     (add-hook 'calendar-mode-hook
	       (lambda ()
		 (unless (eq org-agenda-diary-file 'diary-file)
		   (define-key calendar-mode-map
		     org-calendar-insert-diary-entry-key
		     'org-agenda-diary-entry))))))

(defgroup org-latex nil
  "Options for embedding LaTeX code into Org mode."
  :tag "Org LaTeX"
  :group 'org)

(defcustom org-format-latex-options
  '(:foreground default :background default :scale 1.0
		:html-foreground "Black" :html-background "Transparent"
		:html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
  "Options for creating images from LaTeX fragments.
This is a property list with the following properties:
:foreground  the foreground color for images embedded in Emacs, e.g. \"Black\".
             `default' means use the foreground of the default face.
             `auto' means use the foreground from the text face.
:background  the background color, or \"Transparent\".
             `default' means use the background of the default face.
             `auto' means use the background from the text face.
:scale       a scaling factor for the size of the images, to get more pixels
:html-foreground, :html-background, :html-scale
             the same numbers for HTML export.
:matchers    a list indicating which matchers should be used to
             find LaTeX fragments.  Valid members of this list are:
             \"begin\" find environments
             \"$1\"    find single characters surrounded by $.$
             \"$\"     find math expressions surrounded by $...$
             \"$$\"    find math expressions surrounded by $$....$$
             \"\\(\"    find math expressions surrounded by \\(...\\)
             \"\\=\\[\"    find math expressions surrounded by \\=\\[...\\]"
  :group 'org-latex
  :type 'plist)

(defcustom org-format-latex-signal-error t
  "Non-nil means signal an error when image creation of LaTeX snippets fails.
When nil, just push out a message."
  :group 'org-latex
  :version "24.1"
  :type 'boolean)

(defcustom org-latex-to-mathml-jar-file nil
  "Value of\"%j\" in `org-latex-to-mathml-convert-command'.
Use this to specify additional executable file say a jar file.

When using MathToWeb as the converter, specify the full-path to
your mathtoweb.jar file."
  :group 'org-latex
  :version "24.1"
  :type '(choice
	  (const :tag "None" nil)
	  (file :tag "JAR file" :must-match t)))

(defcustom org-latex-to-mathml-convert-command nil
  "Command to convert LaTeX fragments to MathML.
Replace format-specifiers in the command as noted below and use
`shell-command' to convert LaTeX to MathML.
%j:     Executable file in fully expanded form as specified by
        `org-latex-to-mathml-jar-file'.
%I:     Input LaTeX file in fully expanded form.
%i:     The latex fragment to be converted.
%o:     Output MathML file.

This command is used by `org-create-math-formula'.

When using MathToWeb as the converter, set this option to
\"java -jar %j -unicode -force -df %o %I\".

When using LaTeXML set this option to
\"latexmlmath \"%i\" --presentationmathml=%o\"."
  :group 'org-latex
  :version "24.1"
  :type '(choice
	  (const :tag "None" nil)
	  (string :tag "\nShell command")))

(defcustom org-preview-latex-default-process 'dvipng
  "The default process to convert LaTeX fragments to image files.
All available processes and theirs documents can be found in
`org-preview-latex-process-alist', which see."
  :group 'org-latex
  :version "26.1"
  :package-version '(Org . "9.0")
  :type 'symbol)

(defcustom org-preview-latex-process-alist
  '((dvipng
     :programs ("latex" "dvipng")
     :description "dvi > png"
     :message "you need to install the programs: latex and dvipng."
     :image-input-type "dvi"
     :image-output-type "png"
     :image-size-adjust (1.0 . 1.0)
     :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
     :image-converter ("dvipng -fg %F -bg %B -D %D -T tight -o %O %f"))
    (dvisvgm
     :programs ("latex" "dvisvgm")
     :description "dvi > svg"
     :message "you need to install the programs: latex and dvisvgm."
     :use-xcolor t
     :image-input-type "dvi"
     :image-output-type "svg"
     :image-size-adjust (1.7 . 1.5)
     :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
     :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))
    (imagemagick
     :programs ("latex" "convert")
     :description "pdf > png"
     :message "you need to install the programs: latex and imagemagick."
     :use-xcolor t
     :image-input-type "pdf"
     :image-output-type "png"
     :image-size-adjust (1.0 . 1.0)
     :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f")
     :image-converter
     ("convert -density %D -trim -antialias %f -quality 100 %O")))
  "Definitions of external processes for LaTeX previewing.
Org mode can use some external commands to generate TeX snippet's images for
previewing or inserting into HTML files, e.g., \"dvipng\".  This variable tells
`org-create-formula-image' how to call them.

The value is an alist with the pattern (NAME . PROPERTIES).  NAME is a symbol.
PROPERTIES accepts the following attributes:

  :programs           list of strings, required programs.
  :description        string, describe the process.
  :message            string, message it when required programs cannot be found.
  :image-input-type   string, input file type of image converter (e.g., \"dvi\").
  :image-output-type  string, output file type of image converter (e.g., \"png\").
  :use-xcolor         boolean, when non-nil, LaTeX \"xcolor\" macro is used to
                      deal with background and foreground color of image.
                      Otherwise, dvipng style background and foreground color
                      format are generated.  You may then refer to them in
                      command options with \"%F\" and \"%B\".
  :image-size-adjust  cons of numbers, the car element is used to adjust LaTeX
                      image size showed in buffer and the cdr element is for
                      HTML file.  This option is only useful for process
                      developers, users should use variable
                      `org-format-latex-options' instead.
  :post-clean         list of strings, files matched are to be cleaned up once
                      the image is generated.  When nil, the files with \".dvi\",
                      \".xdv\", \".pdf\", \".tex\", \".aux\", \".log\", \".svg\",
                      \".png\", \".jpg\", \".jpeg\" or \".out\" extension will
                      be cleaned up.
  :latex-header       list of strings, the LaTeX header of the snippet file.
                      When nil, the fallback value is used instead, which is
                      controlled by `org-format-latex-header',
                      `org-latex-default-packages-alist' and
                      `org-latex-packages-alist', which see.
  :latex-compiler     list of LaTeX commands, as strings.  Each of them is given
                      to the shell.  Place-holders \"%t\", \"%b\" and \"%o\" are
                      replaced with values defined below.
  :image-converter    list of image converter commands strings.  Each of them is
                      given to the shell and supports any of the following
                      place-holders defined below.

Place-holders used by `:image-converter' and `:latex-compiler':

  %f    input file name
  %b    base name of input file
  %o    base directory of input file
  %O    absolute output file name

Place-holders only used by `:image-converter':

  %F    foreground of image
  %B    background of image
  %D    dpi, which is used to adjust image size by some processing commands.
  %S    the image size scale ratio, which is used to adjust image size by some
        processing commands."
  :group 'org-latex
  :version "26.1"
  :package-version '(Org . "9.0")
  :type '(alist :tag "LaTeX to image backends"
		:value-type (plist)))

(defcustom org-preview-latex-image-directory "ltximg/"
  "Path to store latex preview images.
A relative path here creates many directories relative to the
processed org files paths.  An absolute path puts all preview
images at the same place."
  :group 'org-latex
  :version "26.1"
  :package-version '(Org . "9.0")
  :type 'string)

(defun org-format-latex-mathml-available-p ()
  "Return t if `org-latex-to-mathml-convert-command' is usable."
  (save-match-data
    (when (and (boundp 'org-latex-to-mathml-convert-command)
	       org-latex-to-mathml-convert-command)
      (let ((executable (car (split-string
			      org-latex-to-mathml-convert-command))))
	(when (executable-find executable)
	  (if (string-match
	       "%j" org-latex-to-mathml-convert-command)
	      (file-readable-p org-latex-to-mathml-jar-file)
	    t))))))

(defcustom org-format-latex-header "\\documentclass{article}
\\usepackage[usenames]{color}
\[PACKAGES]
\[DEFAULT-PACKAGES]
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}"
  "The document header used for processing LaTeX fragments.
It is imperative that this header make sure that no page number
appears on the page.  The package defined in the variables
`org-latex-default-packages-alist' and `org-latex-packages-alist'
will either replace the placeholder \"[PACKAGES]\" in this
header, or they will be appended."
  :group 'org-latex
  :type 'string)

(defun org-set-packages-alist (var val)
  "Set the packages alist and make sure it has 3 elements per entry."
  (set var (mapcar (lambda (x)
		     (if (and (consp x) (= (length x) 2))
			 (list (car x) (nth 1 x) t)
		       x))
		   val)))

(defun org-get-packages-alist (var)
  "Get the packages alist and make sure it has 3 elements per entry."
  (mapcar (lambda (x)
	    (if (and (consp x) (= (length x) 2))
		(list (car x) (nth 1 x) t)
	      x))
	  (default-value var)))

(defcustom org-latex-default-packages-alist
  '(("AUTO" "inputenc"  t ("pdflatex"))
    ("T1"   "fontenc"   t ("pdflatex"))
    (""     "graphicx"  t)
    (""     "grffile"   t)
    (""     "longtable" nil)
    (""     "wrapfig"   nil)
    (""     "rotating"  nil)
    ("normalem" "ulem"  t)
    (""     "amsmath"   t)
    (""     "textcomp"  t)
    (""     "amssymb"   t)
    (""     "capt-of"   nil)
    (""     "hyperref"  nil))
  "Alist of default packages to be inserted in the header.

Change this only if one of the packages here causes an
incompatibility with another package you are using.

The packages in this list are needed by one part or another of
Org mode to function properly:

- inputenc, fontenc:  for basic font and character selection
- graphicx: for including images
- grffile: allow periods and spaces in graphics file names
- longtable: For multipage tables
- wrapfig: for figure placement
- rotating: for sideways figures and tables
- ulem: for underline and strike-through
- amsmath: for subscript and superscript and math environments
- textcomp, amssymb: for various symbols used
  for interpreting the entities in `org-entities'.  You can skip
  some of these packages if you don't use any of their symbols.
- capt-of: for captions outside of floats
- hyperref: for cross references

Therefore you should not modify this variable unless you know
what you are doing.  The one reason to change it anyway is that
you might be loading some other package that conflicts with one
of the default packages.  Each element is either a cell or
a string.

A cell is of the format

  (\"options\" \"package\" SNIPPET-FLAG COMPILERS)

If SNIPPET-FLAG is non-nil, the package also needs to be included
when compiling LaTeX snippets into images for inclusion into
non-LaTeX output.  COMPILERS is a list of compilers that should
include the package, see `org-latex-compiler'.  If the document
compiler is not in the list, and the list is non-nil, the package
will not be inserted in the final document.

A string will be inserted as-is in the header of the document."
  :group 'org-latex
  :group 'org-export-latex
  :set 'org-set-packages-alist
  :get 'org-get-packages-alist
  :version "26.1"
  :package-version '(Org . "8.3")
  :type '(repeat
	  (choice
	   (list :tag "options/package pair"
		 (string :tag "options")
		 (string :tag "package")
		 (boolean :tag "Snippet"))
	   (string :tag "A line of LaTeX"))))

(defcustom org-latex-packages-alist nil
  "Alist of packages to be inserted in every LaTeX header.

These will be inserted after `org-latex-default-packages-alist'.
Each element is either a cell or a string.

A cell is of the format:

    (\"options\" \"package\" SNIPPET-FLAG)

SNIPPET-FLAG, when non-nil, indicates that this package is also
needed when turning LaTeX snippets into images for inclusion into
non-LaTeX output.

A string will be inserted as-is in the header of the document.

Make sure that you only list packages here which:

  - you want in every file;
  - do not conflict with the setup in `org-format-latex-header';
  - do not conflict with the default packages in
    `org-latex-default-packages-alist'."
  :group 'org-latex
  :group 'org-export-latex
  :set 'org-set-packages-alist
  :get 'org-get-packages-alist
  :type '(repeat
	  (choice
	   (list :tag "options/package pair"
		 (string :tag "options")
		 (string :tag "package")
		 (boolean :tag "Snippet"))
	   (string :tag "A line of LaTeX"))))

(defgroup org-appearance nil
  "Settings for Org mode appearance."
  :tag "Org Appearance"
  :group 'org)

(defcustom org-level-color-stars-only nil
  "Non-nil means fontify only the stars in each headline.
When nil, the entire headline is fontified.
Changing it requires restart of `font-lock-mode' to become effective
also in regions already fontified."
  :group 'org-appearance
  :type 'boolean)

(defcustom org-hide-leading-stars nil
  "Non-nil means hide the first N-1 stars in a headline.
This works by using the face `org-hide' for these stars.  This
face is white for a light background, and black for a dark
background.  You may have to customize the face `org-hide' to
make this work.
Changing it requires restart of `font-lock-mode' to become effective
also in regions already fontified.
You may also set this on a per-file basis by adding one of the following
lines to the buffer:

   #+STARTUP: hidestars
   #+STARTUP: showstars"
  :group 'org-appearance
  :type 'boolean)

(defcustom org-hidden-keywords nil
  "List of symbols corresponding to keywords to be hidden the org buffer.
For example, a value \\='(title) for this list will make the document's title
appear in the buffer without the initial #+TITLE: keyword."
  :group 'org-appearance
  :version "24.1"
  :type '(set (const :tag "#+AUTHOR" author)
	      (const :tag "#+DATE" date)
	      (const :tag "#+EMAIL" email)
	      (const :tag "#+TITLE" title)))

(defcustom org-custom-properties nil
  "List of properties (as strings) with a special meaning.
The default use of these custom properties is to let the user
hide them with `org-toggle-custom-properties-visibility'."
  :group 'org-properties
  :group 'org-appearance
  :version "24.3"
  :type '(repeat (string :tag "Property Name")))

(defcustom org-fontify-done-headline nil
  "Non-nil means change the face of a headline if it is marked DONE.
Normally, only the TODO/DONE keyword indicates the state of a headline.
When this is non-nil, the headline after the keyword is set to the
`org-headline-done' as an additional indication."
  :group 'org-appearance
  :type 'boolean)

(defcustom org-fontify-emphasized-text t
  "Non-nil means fontify *bold*, /italic/ and _underlined_ text.
Changing this variable requires a restart of Emacs to take effect."
  :group 'org-appearance
  :type 'boolean)

(defcustom org-fontify-whole-heading-line nil
  "Non-nil means fontify the whole line for headings.
This is useful when setting a background color for the
org-level-* faces."
  :group 'org-appearance
  :type 'boolean)

(defcustom org-highlight-latex-and-related nil
  "Non-nil means highlight LaTeX related syntax in the buffer.
When non nil, the value should be a list containing any of the
following symbols:
  `latex'    Highlight LaTeX snippets and environments.
  `script'   Highlight subscript and superscript.
  `entities' Highlight entities."
  :group 'org-appearance
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "No highlighting" nil)
	  (set :greedy t :tag "Highlight"
	       (const :tag "LaTeX snippets and environments" latex)
	       (const :tag "Subscript and superscript" script)
	       (const :tag "Entities" entities))))

(defcustom org-hide-emphasis-markers nil
  "Non-nil mean font-lock should hide the emphasis marker characters."
  :group 'org-appearance
  :type 'boolean)

(defcustom org-hide-macro-markers nil
  "Non-nil mean font-lock should hide the brackets marking macro calls."
  :group 'org-appearance
  :type 'boolean)

(defcustom org-pretty-entities nil
  "Non-nil means show entities as UTF8 characters.
When nil, the \\name form remains in the buffer."
  :group 'org-appearance
  :version "24.1"
  :type 'boolean)

(defcustom org-pretty-entities-include-sub-superscripts t
  "Non-nil means, pretty entity display includes formatting sub/superscripts."
  :group 'org-appearance
  :version "24.1"
  :type 'boolean)

(defvar org-emph-re nil
  "Regular expression for matching emphasis.
After a match, the match groups contain these elements:
0  The match of the full regular expression, including the characters
   before and after the proper match
1  The character before the proper match, or empty at beginning of line
2  The proper match, including the leading and trailing markers
3  The leading marker like * or /, indicating the type of highlighting
4  The text between the emphasis markers, not including the markers
5  The character after the match, empty at the end of a line")

(defvar org-verbatim-re nil
  "Regular expression for matching verbatim text.")

(defvar org-emphasis-regexp-components) ; defined just below
(defvar org-emphasis-alist) ; defined just below
(defun org-set-emph-re (var val)
  "Set variable and compute the emphasis regular expression."
  (set var val)
  (when (and (boundp 'org-emphasis-alist)
	     (boundp 'org-emphasis-regexp-components)
	     org-emphasis-alist org-emphasis-regexp-components)
    (pcase-let*
	((`(,pre ,post ,border ,body ,nl) org-emphasis-regexp-components)
	 (body (if (<= nl 0) body
		 (format "%s*?\\(?:\n%s*?\\)\\{0,%d\\}" body body nl)))
	 (template
	  (format (concat "\\([%s]\\|^\\)" ;before markers
			  "\\(\\([%%s]\\)\\([^%s]\\|[^%s]%s[^%s]\\)\\3\\)"
			  "\\([%s]\\|$\\)") ;after markers
		  pre border border body border post)))
      (setq org-emph-re (format template "*/_+"))
      (setq org-verbatim-re (format template "=~")))))

;; This used to be a defcustom (Org <8.0) but allowing the users to
;; set this option proved cumbersome.  See this message/thread:
;; http://article.gmane.org/gmane.emacs.orgmode/68681
(defvar org-emphasis-regexp-components
  '("- \t('\"{" "- \t.,:!?;'\")}\\[" " \t\r\n" "." 1)
  "Components used to build the regular expression for emphasis.
This is a list with five entries.  Terminology:  In an emphasis string
like \" *strong word* \", we call the initial space PREMATCH, the final
space POSTMATCH, the stars MARKERS, \"s\" and \"d\" are BORDER characters
and \"trong wor\" is the body.  The different components in this variable
specify what is allowed/forbidden in each part:

pre          Chars allowed as prematch.  Beginning of line will be allowed too.
post         Chars allowed as postmatch.  End of line will be allowed too.
border       The chars *forbidden* as border characters.
body-regexp  A regexp like \".\" to match a body character.  Don't use
             non-shy groups here, and don't allow newline here.
newline      The maximum number of newlines allowed in an emphasis exp.

You need to reload Org or to restart Emacs after customizing this.")

(defcustom org-emphasis-alist
  '(("*" bold)
    ("/" italic)
    ("_" underline)
    ("=" org-verbatim verbatim)
    ("~" org-code verbatim)
    ("+" (:strike-through t)))
  "Alist of characters and faces to emphasize text.
Text starting and ending with a special character will be emphasized,
for example *bold*, _underlined_ and /italic/.  This variable sets the
marker characters and the face to be used by font-lock for highlighting
in Org buffers.

You need to reload Org or to restart Emacs after customizing this."
  :group 'org-appearance
  :set 'org-set-emph-re
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(repeat
	  (list
	   (string :tag "Marker character")
	   (choice
	    (face :tag "Font-lock-face")
	    (plist :tag "Face property list"))
	   (option (const verbatim)))))

(defvar org-protecting-blocks '("src" "example" "export")
  "Blocks that contain text that is quoted, i.e. not processed as Org syntax.
This is needed for font-lock setup.")

;;; Functions and variables from their packages
;;  Declared here to avoid compiler warnings
(defvar mark-active)

;; Various packages
(declare-function calc-eval "calc" (str &optional separator &rest args))
(declare-function calendar-forward-day "cal-move" (arg))
(declare-function calendar-goto-date "cal-move" (date))
(declare-function calendar-goto-today "cal-move" ())
(declare-function calendar-iso-from-absolute "cal-iso" (date))
(declare-function calendar-iso-to-absolute "cal-iso" (date))
(declare-function cdlatex-compute-tables "ext:cdlatex" ())
(declare-function cdlatex-tab "ext:cdlatex" ())
(declare-function dired-get-filename
		  "dired"
		  (&optional localp no-error-if-not-filep))
(declare-function iswitchb-read-buffer
		  "iswitchb"
		  (prompt &optional
			  default require-match _predicate start matches-set))
(declare-function org-agenda-change-all-lines
		  "org-agenda"
		  (newhead hdmarker &optional fixface just-this))
(declare-function org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item
		  "org-agenda"
		  (&optional end))
(declare-function org-agenda-copy-local-variable "org-agenda" (var))
(declare-function org-agenda-format-item
		  "org-agenda"
		  (extra txt &optional level category tags dotime
			 remove-re habitp))
(declare-function org-agenda-maybe-redo "org-agenda" ())
(declare-function org-agenda-new-marker "org-agenda" (&optional pos))
(declare-function org-agenda-save-markers-for-cut-and-paste
		  "org-agenda"
		  (beg end))
(declare-function org-agenda-set-restriction-lock "org-agenda" (&optional type))
(declare-function org-agenda-skip "org-agenda" ())
(declare-function org-attach-reveal "org-attach" (&optional if-exists))
(declare-function org-gnus-follow-link "org-gnus" (&optional group article))
(declare-function org-indent-mode "org-indent" (&optional arg))
(declare-function org-inlinetask-goto-beginning "org-inlinetask" ())
(declare-function org-inlinetask-goto-end "org-inlinetask" ())
(declare-function org-inlinetask-in-task-p "org-inlinetask" ())
(declare-function org-inlinetask-remove-END-maybe "org-inlinetask" ())
(declare-function orgtbl-send-table "org-table" (&optional maybe))
(declare-function parse-time-string "parse-time" (string))
(declare-function speedbar-line-directory "speedbar" (&optional depth))

(defvar align-mode-rules-list)
(defvar calc-embedded-close-formula)
(defvar calc-embedded-open-formula)
(defvar calc-embedded-open-mode)
(defvar font-lock-unfontify-region-function)
(defvar iswitchb-temp-buflist)
(defvar org-agenda-tags-todo-honor-ignore-options)
(defvar remember-data-file)
(defvar texmathp-why)

;;;###autoload
(defun turn-on-orgtbl ()
  "Unconditionally turn on `orgtbl-mode'."
  (require 'org-table)
  (orgtbl-mode 1))

(defun org-at-table-p (&optional table-type)
  "Non-nil if the cursor is inside an Org table.
If TABLE-TYPE is non-nil, also check for table.el-type tables."
  (and (org-match-line (if table-type "[ \t]*[|+]" "[ \t]*|"))
       (or (not (derived-mode-p 'org-mode))
	   (let ((e (org-element-lineage (org-element-at-point) '(table) t)))
	     (and e (or table-type
			(eq 'org (org-element-property :type e))))))))

(defun org-at-table.el-p ()
  "Non-nil when point is at a table.el table."
  (and (org-match-line "[ \t]*[|+]")
       (let ((element (org-element-at-point)))
	 (and (eq (org-element-type element) 'table)
	      (eq (org-element-property :type element) 'table.el)))))

(defun org-at-table-hline-p ()
  "Non-nil when point is inside a hline in a table.
Assume point is already in a table."
  (org-match-line org-table-hline-regexp))

(defun org-table-map-tables (function &optional quietly)
  "Apply FUNCTION to the start of all tables in the buffer."
  (org-with-wide-buffer
   (goto-char (point-min))
   (while (re-search-forward org-table-any-line-regexp nil t)
     (unless quietly
       (message "Mapping tables: %d%%"
		(floor (* 100.0 (point)) (buffer-size))))
     (beginning-of-line 1)
     (when (and (looking-at org-table-line-regexp)
		;; Exclude tables in src/example/verbatim/clocktable blocks
		(not (org-in-block-p '("src" "example" "verbatim" "clocktable"))))
       (save-excursion (funcall function))
       (or (looking-at org-table-line-regexp)
	   (forward-char 1)))
     (re-search-forward org-table-any-border-regexp nil 1)))
  (unless quietly (message "Mapping tables: done")))

(declare-function org-clock-save-markers-for-cut-and-paste "org-clock" (beg end))
(declare-function org-clock-update-mode-line "org-clock" ())
(declare-function org-resolve-clocks "org-clock"
		  (&optional also-non-dangling-p prompt last-valid))

(defun org-at-TBLFM-p (&optional pos)
  "Non-nil when point (or POS) is in #+TBLFM line."
  (save-excursion
    (goto-char (or pos (point)))
    (beginning-of-line)
    (and (let ((case-fold-search t)) (looking-at org-TBLFM-regexp))
	 (eq (org-element-type (org-element-at-point)) 'table))))

(defvar org-clock-start-time)
(defvar org-clock-marker (make-marker)
  "Marker recording the last clock-in.")
(defvar org-clock-hd-marker (make-marker)
  "Marker recording the last clock-in, but the headline position.")
(defvar org-clock-heading ""
  "The heading of the current clock entry.")
(defun org-clock-is-active ()
  "Return the buffer where the clock is currently running.
Return nil if no clock is running."
  (marker-buffer org-clock-marker))

(defun org-check-running-clock ()
  "Check if the current buffer contains the running clock.
If yes, offer to stop it and to save the buffer with the changes."
  (when (and (equal (marker-buffer org-clock-marker) (current-buffer))
	     (y-or-n-p (format "Clock-out in buffer %s before killing it? "
			       (buffer-name))))
    (org-clock-out)
    (when (y-or-n-p "Save changed buffer?")
      (save-buffer))))

(defun org-clocktable-try-shift (dir n)
  "Check if this line starts a clock table, if yes, shift the time block."
  (when (org-match-line "^[ \t]*#\\+BEGIN:[ \t]+clocktable\\>")
    (org-clocktable-shift dir n)))

;;;###autoload
(defun org-clock-persistence-insinuate ()
  "Set up hooks for clock persistence."
  (require 'org-clock)
  (add-hook 'org-mode-hook 'org-clock-load)
  (add-hook 'kill-emacs-hook 'org-clock-save))

(defgroup org-archive nil
  "Options concerning archiving in Org mode."
  :tag "Org Archive"
  :group 'org-structure)

(defcustom org-archive-location "%s_archive::"
  "The location where subtrees should be archived.

The value of this variable is a string, consisting of two parts,
separated by a double-colon.  The first part is a filename and
the second part is a headline.

When the filename is omitted, archiving happens in the same file.
%s in the filename will be replaced by the current file
name (without the directory part).  Archiving to a different file
is useful to keep archived entries from contributing to the
Org Agenda.

The archived entries will be filed as subtrees of the specified
headline.  When the headline is omitted, the subtrees are simply
filed away at the end of the file, as top-level entries.  Also in
the heading you can use %s to represent the file name, this can be
useful when using the same archive for a number of different files.

Here are a few examples:
\"%s_archive::\"
	If the current file is Projects.org, archive in file
	Projects.org_archive, as top-level trees.  This is the default.

\"::* Archived Tasks\"
	Archive in the current file, under the top-level headline
	\"* Archived Tasks\".

\"~/org/archive.org::\"
	Archive in file ~/org/archive.org (absolute path), as top-level trees.

\"~/org/archive.org::* From %s\"
	Archive in file ~/org/archive.org (absolute path), under headlines
        \"From FILENAME\" where file name is the current file name.

\"~/org/datetree.org::datetree/* Finished Tasks\"
        The \"datetree/\" string is special, signifying to archive
        items to the datetree.  Items are placed in either the CLOSED
        date of the item, or the current date if there is no CLOSED date.
        The heading will be a subentry to the current date.  There doesn't
        need to be a heading, but there always needs to be a slash after
        datetree.  For example, to store archived items directly in the
        datetree, use \"~/org/datetree.org::datetree/\".

\"basement::** Finished Tasks\"
	Archive in file ./basement (relative path), as level 3 trees
	below the level 2 heading \"** Finished Tasks\".

You may set this option on a per-file basis by adding to the buffer a
line like

#+ARCHIVE: basement::** Finished Tasks

You may also define it locally for a subtree by setting an ARCHIVE property
in the entry.  If such a property is found in an entry, or anywhere up
the hierarchy, it will be used."
  :group 'org-archive
  :type 'string)

(defcustom org-agenda-skip-archived-trees t
  "Non-nil means the agenda will skip any items located in archived trees.
An archived tree is a tree marked with the tag ARCHIVE.  The use of this
variable is no longer recommended, you should leave it at the value t.
Instead, use the key `v' to cycle the archives-mode in the agenda."
  :group 'org-archive
  :group 'org-agenda-skip
  :type 'boolean)

(defcustom org-columns-skip-archived-trees t
  "Non-nil means ignore archived trees when creating column view."
  :group 'org-archive
  :group 'org-properties
  :type 'boolean)

(defcustom org-cycle-open-archived-trees nil
  "Non-nil means `org-cycle' will open archived trees.
An archived tree is a tree marked with the tag ARCHIVE.
When nil, archived trees will stay folded.  You can still open them with
normal outline commands like `show-all', but not with the cycling commands."
  :group 'org-archive
  :group 'org-cycle
  :type 'boolean)

(defcustom org-sparse-tree-open-archived-trees nil
  "Non-nil means sparse tree construction shows matches in archived trees.
When nil, matches in these trees are highlighted, but the trees are kept in
collapsed state."
  :group 'org-archive
  :group 'org-sparse-trees
  :type 'boolean)

(defcustom org-sparse-tree-default-date-type nil
  "The default date type when building a sparse tree.
When this is nil, a date is a scheduled or a deadline timestamp.
Otherwise, these types are allowed:

        all: all timestamps
     active: only active timestamps (<...>)
   inactive: only inactive timestamps ([...])
  scheduled: only scheduled timestamps
   deadline: only deadline timestamps"
  :type '(choice (const :tag "Scheduled or deadline" nil)
		 (const :tag "All timestamps" all)
		 (const :tag "Only active timestamps" active)
		 (const :tag "Only inactive timestamps" inactive)
		 (const :tag "Only scheduled timestamps" scheduled)
		 (const :tag "Only deadline timestamps" deadline)
		 (const :tag "Only closed timestamps" closed))
  :version "26.1"
  :package-version '(Org . "8.3")
  :group 'org-sparse-trees)

(defun org-cycle-hide-archived-subtrees (state)
  "Re-hide all archived subtrees after a visibility state change.
STATE should be one of the symbols listed in the docstring of
`org-cycle-hook'."
  (when (and (not org-cycle-open-archived-trees)
             (not (memq state '(overview folded))))
    (save-excursion
      (let* ((globalp (memq state '(contents all)))
             (beg (if globalp (point-min) (point)))
             (end (if globalp (point-max) (org-end-of-subtree t))))
	(org-hide-archived-subtrees beg end)
	(goto-char beg)
	(when (looking-at-p (concat ".*:" org-archive-tag ":"))
	  (message "%s" (substitute-command-keys
			 "Subtree is archived and stays closed.  Use \
`\\[org-force-cycle-archived]' to cycle it anyway.")))))))

(defun org-force-cycle-archived ()
  "Cycle subtree even if it is archived."
  (interactive)
  (setq this-command 'org-cycle)
  (let ((org-cycle-open-archived-trees t))
    (call-interactively 'org-cycle)))

(defun org-hide-archived-subtrees (beg end)
  "Re-hide all archived subtrees after a visibility state change."
  (org-with-wide-buffer
   (let ((case-fold-search nil)
	 (re (concat org-outline-regexp-bol ".*:" org-archive-tag ":")))
     (goto-char beg)
     ;; Include headline point is currently on.
     (beginning-of-line)
     (while (and (< (point) end) (re-search-forward re end t))
       (when (member org-archive-tag (org-get-tags))
	 (org-flag-subtree t)
	 (org-end-of-subtree t))))))

(declare-function outline-end-of-heading "outline" ())
(declare-function outline-flag-region "outline" (from to flag))
(defun org-flag-subtree (flag)
  (save-excursion
    (org-back-to-heading t)
    (outline-end-of-heading)
    (outline-flag-region (point)
			 (progn (org-end-of-subtree t) (point))
			 flag)))

(defalias 'org-advertized-archive-subtree 'org-archive-subtree)

;; Declare Column View Code

(declare-function org-columns-get-format-and-top-level "org-colview" ())
(declare-function org-columns-compute "org-colview" (property))

;; Declare ID code

(declare-function org-id-store-link "org-id")
(declare-function org-id-locations-load "org-id")
(declare-function org-id-locations-save "org-id")
(defvar org-id-track-globally)

;;; Variables for pre-computed regular expressions, all buffer local

(defvar-local org-todo-regexp nil
  "Matches any of the TODO state keywords.
Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")

(defvar-local org-not-done-regexp nil
  "Matches any of the TODO state keywords except the last one.
Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")

(defvar-local org-not-done-heading-regexp nil
  "Matches a TODO headline that is not done.
Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")

(defvar-local org-todo-line-regexp nil
  "Matches a headline and puts TODO state into group 2 if present.
Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")

(defvar-local org-complex-heading-regexp nil
  "Matches a headline and puts everything into groups:

group 1: Stars
group 2: The TODO keyword, maybe
group 3: Priority cookie
group 4: True headline
group 5: Tags

Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")

(defvar-local org-complex-heading-regexp-format nil
  "Printf format to make regexp to match an exact headline.
This regexp will match the headline of any node which has the
exact headline text that is put into the format, but may have any
TODO state, priority and tags.")

(defvar-local org-todo-line-tags-regexp nil
  "Matches a headline and puts TODO state into group 2 if present.
Also put tags into group 4 if tags are present.")

(defconst org-plain-time-of-day-regexp
  (concat
   "\\(\\<[012]?[0-9]"
   "\\(\\(:\\([0-5][0-9]\\([AaPp][Mm]\\)?\\)\\)\\|\\([AaPp][Mm]\\)\\)\\>\\)"
   "\\(--?"
   "\\(\\<[012]?[0-9]"
   "\\(\\(:\\([0-5][0-9]\\([AaPp][Mm]\\)?\\)\\)\\|\\([AaPp][Mm]\\)\\)\\>\\)"
   "\\)?")
  "Regular expression to match a plain time or time range.
Examples:  11:45 or 8am-13:15 or 2:45-2:45pm.  After a match, the following
groups carry important information:
0  the full match
1  the first time, range or not
8  the second time, if it is a range.")

(defconst org-plain-time-extension-regexp
  (concat
   "\\(\\<[012]?[0-9]"
   "\\(\\(:\\([0-5][0-9]\\([AaPp][Mm]\\)?\\)\\)\\|\\([AaPp][Mm]\\)\\)\\>\\)"
   "\\+\\([0-9]+\\)\\(:\\([0-5][0-9]\\)\\)?")
  "Regular expression to match a time range like 13:30+2:10 = 13:30-15:40.
Examples:  11:45 or 8am-13:15 or 2:45-2:45pm.  After a match, the following
groups carry important information:
0  the full match
7  hours of duration
9  minutes of duration")

(defconst org-stamp-time-of-day-regexp
  (concat
   "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} +\\sw+ +\\)"
   "\\([012][0-9]:[0-5][0-9]\\(-\\([012][0-9]:[0-5][0-9]\\)\\)?[^\n\r>]*?\\)>"
   "\\(--?"
   "<\\1\\([012][0-9]:[0-5][0-9]\\)>\\)?")
  "Regular expression to match a timestamp time or time range.
After a match, the following groups carry important information:
0  the full match
1  date plus weekday, for back referencing to make sure both times are on the same day
2  the first time, range or not
4  the second time, if it is a range.")

(defconst org-startup-options
  '(("fold" org-startup-folded t)
    ("overview" org-startup-folded t)
    ("nofold" org-startup-folded nil)
    ("showall" org-startup-folded nil)
    ("showeverything" org-startup-folded showeverything)
    ("content" org-startup-folded content)
    ("indent" org-startup-indented t)
    ("noindent" org-startup-indented nil)
    ("hidestars" org-hide-leading-stars t)
    ("showstars" org-hide-leading-stars nil)
    ("odd" org-odd-levels-only t)
    ("oddeven" org-odd-levels-only nil)
    ("align" org-startup-align-all-tables t)
    ("noalign" org-startup-align-all-tables nil)
    ("inlineimages" org-startup-with-inline-images t)
    ("noinlineimages" org-startup-with-inline-images nil)
    ("latexpreview" org-startup-with-latex-preview t)
    ("nolatexpreview" org-startup-with-latex-preview nil)
    ("customtime" org-display-custom-times t)
    ("logdone" org-log-done time)
    ("lognotedone" org-log-done note)
    ("nologdone" org-log-done nil)
    ("lognoteclock-out" org-log-note-clock-out t)
    ("nolognoteclock-out" org-log-note-clock-out nil)
    ("logrepeat" org-log-repeat state)
    ("lognoterepeat" org-log-repeat note)
    ("logdrawer" org-log-into-drawer t)
    ("nologdrawer" org-log-into-drawer nil)
    ("logstatesreversed" org-log-states-order-reversed t)
    ("nologstatesreversed" org-log-states-order-reversed nil)
    ("nologrepeat" org-log-repeat nil)
    ("logreschedule" org-log-reschedule time)
    ("lognotereschedule" org-log-reschedule note)
    ("nologreschedule" org-log-reschedule nil)
    ("logredeadline" org-log-redeadline time)
    ("lognoteredeadline" org-log-redeadline note)
    ("nologredeadline" org-log-redeadline nil)
    ("logrefile" org-log-refile time)
    ("lognoterefile" org-log-refile note)
    ("nologrefile" org-log-refile nil)
    ("fninline" org-footnote-define-inline t)
    ("nofninline" org-footnote-define-inline nil)
    ("fnlocal" org-footnote-section nil)
    ("fnauto" org-footnote-auto-label t)
    ("fnprompt" org-footnote-auto-label nil)
    ("fnconfirm" org-footnote-auto-label confirm)
    ("fnplain" org-footnote-auto-label plain)
    ("fnadjust" org-footnote-auto-adjust t)
    ("nofnadjust" org-footnote-auto-adjust nil)
    ("constcgs" constants-unit-system cgs)
    ("constSI" constants-unit-system SI)
    ("noptag" org-tag-persistent-alist nil)
    ("hideblocks" org-hide-block-startup t)
    ("nohideblocks" org-hide-block-startup nil)
    ("beamer" org-startup-with-beamer-mode t)
    ("entitiespretty" org-pretty-entities t)
    ("entitiesplain" org-pretty-entities nil))
  "Variable associated with STARTUP options for org-mode.
Each element is a list of three items: the startup options (as written
in the #+STARTUP line), the corresponding variable, and the value to set
this variable to if the option is found.  An optional forth element PUSH
means to push this value onto the list in the variable.")

(defcustom org-group-tags t
  "When non-nil (the default), use group tags.
This can be turned on/off through `org-toggle-tags-groups'."
  :group 'org-tags
  :group 'org-startup
  :type 'boolean)

(defvar org-inhibit-startup nil)        ; Dynamically-scoped param.

(defun org-toggle-tags-groups ()
  "Toggle support for group tags.
Support for group tags is controlled by the option
`org-group-tags', which is non-nil by default."
  (interactive)
  (setq org-group-tags (not org-group-tags))
  (cond ((and (derived-mode-p 'org-agenda-mode)
	      org-group-tags)
	 (org-agenda-redo))
	((derived-mode-p 'org-mode)
	 (let ((org-inhibit-startup t)) (org-mode))))
  (message "Groups tags support has been turned %s"
	   (if org-group-tags "on" "off")))

(defun org-set-regexps-and-options (&optional tags-only)
  "Precompute regular expressions used in the current buffer.
When optional argument TAGS-ONLY is non-nil, only compute tags
related expressions."
  (when (derived-mode-p 'org-mode)
    (let ((alist (org--setup-collect-keywords
		  (org-make-options-regexp
		   (append '("FILETAGS" "TAGS" "SETUPFILE")
			   (and (not tags-only)
				'("ARCHIVE" "CATEGORY" "COLUMNS" "CONSTANTS"
				  "LINK" "OPTIONS" "PRIORITIES" "PROPERTY"
				  "SEQ_TODO" "STARTUP" "TODO" "TYP_TODO")))))))
      ;; Startup options.  Get this early since it does change
      ;; behavior for other options (e.g., tags).
      (let ((startup (cdr (assq 'startup alist))))
	(dolist (option startup)
	  (let ((entry (assoc-string option org-startup-options t)))
	    (when entry
	      (let ((var (nth 1 entry))
		    (val (nth 2 entry)))
		(if (not (nth 3 entry)) (set (make-local-variable var) val)
		  (unless (listp (symbol-value var))
		    (set (make-local-variable var) nil))
		  (add-to-list var val)))))))
      (setq-local org-file-tags
		  (mapcar #'org-add-prop-inherited
			  (cdr (assq 'filetags alist))))
      (setq org-current-tag-alist
	    (append org-tag-persistent-alist
		    (let ((tags (cdr (assq 'tags alist))))
		      (if tags (org-tag-string-to-alist tags)
			org-tag-alist))))
      (setq org-tag-groups-alist
	    (org-tag-alist-to-groups org-current-tag-alist))
      (unless tags-only
	;; File properties.
	(setq-local org-file-properties (cdr (assq 'property alist)))
	;; Archive location.
	(let ((archive (cdr (assq 'archive alist))))
	  (when archive (setq-local org-archive-location archive)))
	;; Category.
	(let ((cat (org-string-nw-p (cdr (assq 'category alist)))))
	  (when cat
	    (setq-local org-category (intern cat))
	    (setq-local org-file-properties
			(org--update-property-plist
			 "CATEGORY" cat org-file-properties))))
	;; Columns.
	(let ((column (cdr (assq 'columns alist))))
	  (when column (setq-local org-columns-default-format column)))
	;; Constants.
	(setq org-table-formula-constants-local (cdr (assq 'constants alist)))
	;; Link abbreviations.
	(let ((links (cdr (assq 'link alist))))
	  (when links (setq org-link-abbrev-alist-local (nreverse links))))
	;; Priorities.
	(let ((priorities (cdr (assq 'priorities alist))))
	  (when priorities
	    (setq-local org-highest-priority (nth 0 priorities))
	    (setq-local org-lowest-priority (nth 1 priorities))
	    (setq-local org-default-priority (nth 2 priorities))))
	;; Scripts.
	(let ((scripts (assq 'scripts alist)))
	  (when scripts
	    (setq-local org-use-sub-superscripts (cdr scripts))))
	;; TODO keywords.
	(setq-local org-todo-kwd-alist nil)
	(setq-local org-todo-key-alist nil)
	(setq-local org-todo-key-trigger nil)
	(setq-local org-todo-keywords-1 nil)
	(setq-local org-done-keywords nil)
	(setq-local org-todo-heads nil)
	(setq-local org-todo-sets nil)
	(setq-local org-todo-log-states nil)
	(let ((todo-sequences
	       (or (nreverse (cdr (assq 'todo alist)))
		   (let ((d (default-value 'org-todo-keywords)))
		     (if (not (stringp (car d))) d
		       ;; XXX: Backward compatibility code.
		       (list (cons org-todo-interpretation d)))))))
	  (dolist (sequence todo-sequences)
	    (let* ((sequence (or (run-hook-with-args-until-success
				  'org-todo-setup-filter-hook sequence)
				 sequence))
		   (sequence-type (car sequence))
		   (keywords (cdr sequence))
		   (sep (member "|" keywords))
		   names alist)
	      (dolist (k (remove "|" keywords))
		(unless (string-match "^\\(.*?\\)\\(?:(\\([^!@/]\\)?.*?)\\)?$"
				      k)
		  (error "Invalid TODO keyword %s" k))
		(let ((name (match-string 1 k))
		      (key (match-string 2 k))
		      (log (org-extract-log-state-settings k)))
		  (push name names)
		  (push (cons name (and key (string-to-char key))) alist)
		  (when log (push log org-todo-log-states))))
	      (let* ((names (nreverse names))
		     (done (if sep (org-remove-keyword-keys (cdr sep))
			     (last names)))
		     (head (car names))
		     (tail (list sequence-type head (car done) (org-last done))))
		(add-to-list 'org-todo-heads head 'append)
		(push names org-todo-sets)
		(setq org-done-keywords (append org-done-keywords done nil))
		(setq org-todo-keywords-1 (append org-todo-keywords-1 names nil))
		(setq org-todo-key-alist
		      (append org-todo-key-alist
			      (and alist
				   (append '((:startgroup))
					   (nreverse alist)
					   '((:endgroup))))))
		(dolist (k names) (push (cons k tail) org-todo-kwd-alist))))))
	(setq org-todo-sets (nreverse org-todo-sets)
	      org-todo-kwd-alist (nreverse org-todo-kwd-alist)
	      org-todo-key-trigger (delq nil (mapcar #'cdr org-todo-key-alist))
	      org-todo-key-alist (org-assign-fast-keys org-todo-key-alist))
	;; Compute the regular expressions and other local variables.
	;; Using `org-outline-regexp-bol' would complicate them much,
	;; because of the fixed white space at the end of that string.
	(unless org-done-keywords
	  (setq org-done-keywords
		(and org-todo-keywords-1 (last org-todo-keywords-1))))
	(setq org-not-done-keywords
	      (org-delete-all org-done-keywords
			      (copy-sequence org-todo-keywords-1))
	      org-todo-regexp (regexp-opt org-todo-keywords-1 t)
	      org-not-done-regexp (regexp-opt org-not-done-keywords t)
	      org-not-done-heading-regexp
	      (format org-heading-keyword-regexp-format org-not-done-regexp)
	      org-todo-line-regexp
	      (format org-heading-keyword-maybe-regexp-format org-todo-regexp)
	      org-complex-heading-regexp
	      (concat "^\\(\\*+\\)"
		      "\\(?: +" org-todo-regexp "\\)?"
		      "\\(?: +\\(\\[#.\\]\\)\\)?"
		      "\\(?: +\\(.*?\\)\\)??"
		      "\\(?:[ \t]+\\(:[[:alnum:]_@#%:]+:\\)\\)?"
		      "[ \t]*$")
	      org-complex-heading-regexp-format
	      (concat "^\\(\\*+\\)"
		      "\\(?: +" org-todo-regexp "\\)?"
		      "\\(?: +\\(\\[#.\\]\\)\\)?"
		      "\\(?: +"
		      ;; Stats cookies can be stuck to body.
		      "\\(?:\\[[0-9%%/]+\\] *\\)*"
		      "\\(%s\\)"
		      "\\(?: *\\[[0-9%%/]+\\]\\)*"
		      "\\)"
		      "\\(?:[ \t]+\\(:[[:alnum:]_@#%%:]+:\\)\\)?"
		      "[ \t]*$")
	      org-todo-line-tags-regexp
	      (concat "^\\(\\*+\\)"
		      "\\(?: +" org-todo-regexp "\\)?"
		      "\\(?: +\\(.*?\\)\\)??"
		      "\\(?:[ \t]+\\(:[[:alnum:]:_@#%]+:\\)\\)?"
		      "[ \t]*$"))
	(org-compute-latex-and-related-regexp)))))

(defun org--setup-collect-keywords (regexp &optional files alist)
  "Return setup keywords values as an alist.

REGEXP matches a subset of setup keywords.  FILES is a list of
file names already visited.  It is used to avoid circular setup
files.  ALIST, when non-nil, is the alist computed so far.

Return value contains the following keys: `archive', `category',
`columns', `constants', `filetags', `link', `priorities',
`property', `scripts', `startup', `tags' and `todo'."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((case-fold-search t))
     (while (re-search-forward regexp nil t)
       (let ((element (org-element-at-point)))
	 (when (eq (org-element-type element) 'keyword)
	   (let ((key (org-element-property :key element))
		 (value (org-element-property :value element)))
	     (cond
	      ((equal key "ARCHIVE")
	       (when (org-string-nw-p value)
		 (push (cons 'archive value) alist)))
	      ((equal key "CATEGORY") (push (cons 'category value) alist))
	      ((equal key "COLUMNS") (push (cons 'columns value) alist))
	      ((equal key "CONSTANTS")
	       (let* ((constants (assq 'constants alist))
		      (store (cdr constants)))
		 (dolist (pair (split-string value))
		   (when (string-match "^\\([a-zA-Z0][_a-zA-Z0-9]*\\)=\\(.*\\)"
				       pair)
		     (let* ((name (match-string 1 pair))
			    (value (match-string 2 pair))
			    (old (assoc name store)))
		       (if old (setcdr old value)
			 (push (cons name value) store)))))
		 (if constants (setcdr constants store)
		   (push (cons 'constants store) alist))))
	      ((equal key "FILETAGS")
	       (when (org-string-nw-p value)
		 (let ((old (assq 'filetags alist))
		       (new (apply #'nconc
				   (mapcar (lambda (x) (org-split-string x ":"))
					   (split-string value)))))
		   (if old (setcdr old (append new (cdr old)))
		     (push (cons 'filetags new) alist)))))
	      ((equal key "LINK")
	       (when (string-match "\\`\\(\\S-+\\)[ \t]+\\(.+\\)" value)
		 (let ((links (assq 'link alist))
		       (pair (cons (match-string-no-properties 1 value)
				   (match-string-no-properties 2 value))))
		   (if links (push pair (cdr links))
		     (push (list 'link pair) alist)))))
	      ((equal key "OPTIONS")
	       (when (and (org-string-nw-p value)
			  (string-match "\\^:\\(t\\|nil\\|{}\\)" value))
		 (push (cons 'scripts (read (match-string 1 value))) alist)))
	      ((equal key "PRIORITIES")
	       (push (cons 'priorities
			   (let ((prio (split-string value)))
			     (if (< (length prio) 3) '(?A ?C ?B)
			       (mapcar #'string-to-char prio))))
		     alist))
	      ((equal key "PROPERTY")
	       (when (string-match "\\(\\S-+\\)[ \t]+\\(.*\\)" value)
		 (let* ((property (assq 'property alist))
			(value (org--update-property-plist
				(match-string-no-properties 1 value)
				(match-string-no-properties 2 value)
				(cdr property))))
		   (if property (setcdr property value)
		     (push (cons 'property value) alist)))))
	      ((equal key "STARTUP")
	       (let ((startup (assq 'startup alist)))
		 (if startup
		     (setcdr startup
			     (append (cdr startup) (split-string value)))
		   (push (cons 'startup (split-string value)) alist))))
	      ((equal key "TAGS")
	       (let ((tag-cell (assq 'tags alist)))
		 (if tag-cell
		     (setcdr tag-cell (concat (cdr tag-cell) "\n" value))
		   (push (cons 'tags value) alist))))
	      ((member key '("TODO" "SEQ_TODO" "TYP_TODO"))
	       (let ((todo (assq 'todo alist))
		     (value (cons (if (equal key "TYP_TODO") 'type 'sequence)
				  (split-string value))))
		 (if todo (push value (cdr todo))
		   (push (list 'todo value) alist))))
	      ((equal key "SETUPFILE")
	       (unless buffer-read-only ; Do not check in Gnus messages.
		 (let ((f (and (org-string-nw-p value)
			       (expand-file-name
				(org-unbracket-string "\"" "\"" value)))))
		   (when (and f (file-readable-p f) (not (member f files)))
		     (with-temp-buffer
		       (setq default-directory (file-name-directory f))
		       (insert-file-contents f)
		       (setq alist
			     ;; Fake Org mode to benefit from cache
			     ;; without recurring needlessly.
			     (let ((major-mode 'org-mode))
			       (org--setup-collect-keywords
				regexp (cons f files) alist)))))))))))))))
  alist)

(defun org-tag-string-to-alist (s)
  "Return tag alist associated to string S.
S is a value for TAGS keyword or produced with
`org-tag-alist-to-string'.  Return value is an alist suitable for
`org-tag-alist' or `org-tag-persistent-alist'."
  (let ((lines (mapcar #'split-string (split-string s "\n" t)))
	(tag-re (concat "\\`\\([[:alnum:]_@#%]+"
			"\\|{.+?}\\)"	; regular expression
			"\\(?:(\\(.\\))\\)?\\'"))
	alist group-flag)
    (dolist (tokens lines (cdr (nreverse alist)))
      (push '(:newline) alist)
      (while tokens
	(let ((token (pop tokens)))
	  (pcase token
	    ("{"
	     (push '(:startgroup) alist)
	     (when (equal (nth 1 tokens) ":") (setq group-flag t)))
	    ("}"
	     (push '(:endgroup) alist)
	     (setq group-flag nil))
	    ("["
	     (push '(:startgrouptag) alist)
	     (when (equal (nth 1 tokens) ":") (setq group-flag t)))
	    ("]"
	     (push '(:endgrouptag) alist)
	     (setq group-flag nil))
	    (":"
	     (push '(:grouptags) alist))
	    ((guard (string-match tag-re token))
	     (let ((tag (match-string 1 token))
		   (key (and (match-beginning 2)
			     (string-to-char (match-string 2 token)))))
	       ;; Push all tags in groups, no matter if they already
	       ;; appear somewhere else in the list.
	       (when (or group-flag (not (assoc tag alist)))
		 (push (cons tag key) alist))))))))))

(defun org-tag-alist-to-string (alist &optional skip-key)
  "Return tag string associated to ALIST.

ALIST is an alist, as defined in `org-tag-alist' or
`org-tag-persistent-alist', or produced with
`org-tag-string-to-alist'.

Return value is a string suitable as a value for \"TAGS\"
keyword.

When optional argument SKIP-KEY is non-nil, skip selection keys
next to tags."
  (mapconcat (lambda (token)
	       (pcase token
		 (`(:startgroup) "{")
		 (`(:endgroup) "}")
		 (`(:startgrouptag) "[")
		 (`(:endgrouptag) "]")
		 (`(:grouptags) ":")
		 (`(:newline) "\\n")
		 ((and
		   (guard (not skip-key))
		   `(,(and tag (pred stringp)) . ,(and key (pred characterp))))
		  (format "%s(%c)" tag key))
		 (`(,(and tag (pred stringp)) . ,_) tag)
		 (_ (user-error "Invalid tag token: %S" token))))
	     alist
	     " "))

(defun org-tag-alist-to-groups (alist)
  "Return group alist from tag ALIST.
ALIST is an alist, as defined in `org-tag-alist' or
`org-tag-persistent-alist', or produced with
`org-tag-string-to-alist'.  Return value is an alist following
the pattern (GROUP-TAG TAGS) where GROUP-TAG is the tag, as
a string, summarizing TAGS, as a list of strings."
  (let (groups group-status current-group)
    (dolist (token alist (nreverse groups))
      (pcase token
	(`(,(or :startgroup :startgrouptag)) (setq group-status t))
	(`(,(or :endgroup :endgrouptag))
	 (when (eq group-status 'append)
	   (push (nreverse current-group) groups))
	 (setq group-status nil))
	(`(:grouptags) (setq group-status 'append))
	((and `(,tag . ,_) (guard group-status))
	 (if (eq group-status 'append) (push tag current-group)
	   (setq current-group (list tag))))
	(_ nil)))))

(defvar org--file-cache (make-hash-table :test #'equal)
  "Hash table to store contents of files referenced via a URL.
This is the cache of file URLs read using `org-file-contents'.")

(defun org-reset-file-cache ()
  "Reset the cache of files downloaded by `org-file-contents'."
  (clrhash org--file-cache))

(defun org-file-url-p (file)
  "Non-nil if FILE is a URL."
  (require 'ffap)
  (string-match-p ffap-url-regexp file))

(defun org-file-contents (file &optional noerror nocache)
  "Return the contents of FILE, as a string.

FILE can be a file name or URL.

If FILE is a URL, download the contents.  If the URL contents are
already cached in the `org--file-cache' hash table, the download step
is skipped.

If NOERROR is non-nil, ignore the error when unable to read the FILE
from file or URL.

If NOCACHE is non-nil, do a fresh fetch of FILE even if cached version
is available.  This option applies only if FILE is a URL."
  (let* ((is-url (org-file-url-p file))
         (cache (and is-url
                     (not nocache)
                     (gethash file org--file-cache))))
    (cond
     (cache)
     (is-url
      (with-current-buffer (url-retrieve-synchronously file)
	(goto-char (point-min))
	;; Move point to after the url-retrieve header.
	(search-forward "\n\n" nil :move)
	;; Search for the success code only in the url-retrieve header.
	(if (save-excursion
	      (re-search-backward "HTTP.*\\s-+200\\s-OK" nil :noerror))
	    ;; Update the cache `org--file-cache' and return contents.
	    (puthash file
		     (buffer-substring-no-properties (point) (point-max))
		     org--file-cache)
	  (funcall (if noerror #'message #'user-error)
		   "Unable to fetch file from %S"
		   file))))
     (t
      (with-temp-buffer
        (condition-case nil
	    (progn
	      (insert-file-contents file)
	      (buffer-string))
	  (file-error
           (funcall (if noerror #'message #'user-error)
		    "Unable to read file %S"
		    file))))))))

(defun org-extract-log-state-settings (x)
  "Extract the log state setting from a TODO keyword string.
This will extract info from a string like \"WAIT(w@/!)\"."
  (when (string-match "^\\(.*?\\)\\(?:(\\([^!@/]\\)?\\([!@]\\)?\\(?:/\\([!@]\\)\\)?)\\)?$" x)
    (let ((kw (match-string 1 x))
	  (log1 (and (match-end 3) (match-string 3 x)))
	  (log2 (and (match-end 4) (match-string 4 x))))
      (and (or log1 log2)
	   (list kw
		 (and log1 (if (equal log1 "!") 'time 'note))
		 (and log2 (if (equal log2 "!") 'time 'note)))))))

(defun org-remove-keyword-keys (list)
  "Remove a pair of parenthesis at the end of each string in LIST."
  (mapcar (lambda (x)
	    (if (string-match "(.*)$" x)
		(substring x 0 (match-beginning 0))
	      x))
	  list))

(defun org-assign-fast-keys (alist)
  "Assign fast keys to a keyword-key alist.
Respect keys that are already there."
  (let (new e (alt ?0))
    (while (setq e (pop alist))
      (if (or (memq (car e) '(:newline :grouptags :endgroup :startgroup))
	      (cdr e)) ;; Key already assigned.
	  (push e new)
	(let ((clist (string-to-list (downcase (car e))))
	      (used (append new alist)))
	  (when (= (car clist) ?@)
	    (pop clist))
	  (while (and clist (rassoc (car clist) used))
	    (pop clist))
	  (unless clist
	    (while (rassoc alt used)
	      (cl-incf alt)))
	  (push (cons (car e) (or (car clist) alt)) new))))
    (nreverse new)))

;;; Some variables used in various places

(defvar org-window-configuration nil
  "Used in various places to store a window configuration.")
(defvar org-selected-window nil
  "Used in various places to store a window configuration.")
(defvar org-finish-function nil
  "Function to be called when `C-c C-c' is used.
This is for getting out of special buffers like capture.")
(defvar org-last-state)

;; Defined somewhere in this file, but used before definition.
(defvar org-entities)     ;; defined in org-entities.el
(defvar org-struct-menu)
(defvar org-org-menu)
(defvar org-tbl-menu)

;;;; Define the Org mode

;; We use a before-change function to check if a table might need
;; an update.
(defvar org-table-may-need-update t
  "Indicates that a table might need an update.
This variable is set by `org-before-change-function'.
`org-table-align' sets it back to nil.")
(defun org-before-change-function (_beg _end)
  "Every change indicates that a table might need an update."
  (setq org-table-may-need-update t))
(defvar org-mode-map)
(defvar org-inhibit-startup-visibility-stuff nil) ; Dynamically-scoped param.
(defvar org-agenda-keep-modes nil)      ; Dynamically-scoped param.
(defvar org-inhibit-logging nil)        ; Dynamically-scoped param.
(defvar org-inhibit-blocking nil)       ; Dynamically-scoped param.
(defvar org-table-buffer-is-an nil)

(defvar bidi-paragraph-direction)
(defvar buffer-face-mode-face)

(require 'outline)

;; Other stuff we need.
(require 'time-date)
(unless (fboundp 'time-subtract) (defalias 'time-subtract 'subtract-time))
(require 'easymenu)
(autoload 'easy-menu-add "easymenu")
(require 'overlay)

;; (require 'org-macs) moved higher up in the file before it is first used
(require 'org-entities)
;; (require 'org-compat) moved higher up in the file before it is first used
(require 'org-faces)
(require 'org-list)
(require 'org-pcomplete)
(require 'org-src)
(require 'org-footnote)
(require 'org-macro)

;; babel
(require 'ob)

;;;###autoload
(define-derived-mode org-mode outline-mode "Org"
  "Outline-based notes management and organizer, alias
\"Carsten's outline-mode for keeping track of everything.\"

Org mode develops organizational tasks around a NOTES file which
contains information about projects as plain text.  Org mode is
implemented on top of Outline mode, which is ideal to keep the content
of large files well structured.  It supports ToDo items, deadlines and
time stamps, which magically appear in the diary listing of the Emacs
calendar.  Tables are easily created with a built-in table editor.
Plain text URL-like links connect to websites, emails (VM), Usenet
messages (Gnus), BBDB entries, and any files related to the project.
For printing and sharing of notes, an Org file (or a part of it)
can be exported as a structured ASCII or HTML file.

The following commands are available:

\\{org-mode-map}"

  ;; Get rid of Outline menus, they are not needed
  ;; Need to do this here because define-derived-mode sets up
  ;; the keymap so late.  Still, it is a waste to call this each time
  ;; we switch another buffer into Org mode.
  (define-key org-mode-map [menu-bar headings] 'undefined)
  (define-key org-mode-map [menu-bar hide] 'undefined)
  (define-key org-mode-map [menu-bar show] 'undefined)

  (org-load-modules-maybe)
  (org-install-agenda-files-menu)
  (when org-descriptive-links (add-to-invisibility-spec '(org-link)))
  (add-to-invisibility-spec '(org-cwidth))
  (add-to-invisibility-spec '(org-hide-block . t))
  (setq-local outline-regexp org-outline-regexp)
  (setq-local outline-level 'org-outline-level)
  (setq bidi-paragraph-direction 'left-to-right)
  (when (and (stringp org-ellipsis) (not (equal "" org-ellipsis)))
    (unless org-display-table
      (setq org-display-table (make-display-table)))
    (set-display-table-slot
     org-display-table 4
     (vconcat (mapcar (lambda (c) (make-glyph-code c 'org-ellipsis))
		      org-ellipsis)))
    (setq buffer-display-table org-display-table))
  (org-set-regexps-and-options)
  (org-set-font-lock-defaults)
  (when (and org-tag-faces (not org-tags-special-faces-re))
    ;; tag faces set outside customize.... force initialization.
    (org-set-tag-faces 'org-tag-faces org-tag-faces))
  ;; Calc embedded
  (setq-local calc-embedded-open-mode "# ")
  ;; Modify a few syntax entries
  (modify-syntax-entry ?@ "w")
  (modify-syntax-entry ?\" "\"")
  (modify-syntax-entry ?\\ "_")
  (modify-syntax-entry ?~ "_")
  (setq-local font-lock-unfontify-region-function 'org-unfontify-region)
  ;; Activate before-change-function
  (setq-local org-table-may-need-update t)
  (add-hook 'before-change-functions 'org-before-change-function nil 'local)
  ;; Check for running clock before killing a buffer
  (add-hook 'kill-buffer-hook 'org-check-running-clock nil 'local)
  ;; Initialize macros templates.
  (org-macro-initialize-templates)
  ;; Initialize radio targets.
  (org-update-radio-target-regexp)
  ;; Indentation.
  (setq-local indent-line-function 'org-indent-line)
  (setq-local indent-region-function 'org-indent-region)
  ;; Filling and auto-filling.
  (org-setup-filling)
  ;; Comments.
  (org-setup-comments-handling)
  ;; Initialize cache.
  (org-element-cache-reset)
  ;; Beginning/end of defun
  (setq-local beginning-of-defun-function 'org-backward-element)
  (setq-local end-of-defun-function
	      (lambda ()
		(if (not (org-at-heading-p))
		    (org-forward-element)
		  (org-forward-element)
		  (forward-char -1))))
  ;; Next error for sparse trees
  (setq-local next-error-function 'org-occur-next-match)
  ;; Make sure dependence stuff works reliably, even for users who set it
  ;; too late :-(
  (if org-enforce-todo-dependencies
      (add-hook 'org-blocker-hook
		'org-block-todo-from-children-or-siblings-or-parent)
    (remove-hook 'org-blocker-hook
		 'org-block-todo-from-children-or-siblings-or-parent))
  (if org-enforce-todo-checkbox-dependencies
      (add-hook 'org-blocker-hook
		'org-block-todo-from-checkboxes)
    (remove-hook 'org-blocker-hook
		 'org-block-todo-from-checkboxes))

  ;; Align options lines
  (setq-local
   align-mode-rules-list
   '((org-in-buffer-settings
      (regexp . "^[ \t]*#\\+[A-Z_]+:\\(\\s-*\\)\\S-+")
      (modes . '(org-mode)))))

  ;; Imenu
  (setq-local imenu-create-index-function 'org-imenu-get-tree)

  ;; Make isearch reveal context
  (setq-local outline-isearch-open-invisible-function
	      (lambda (&rest _) (org-show-context 'isearch)))

  ;; Setup the pcomplete hooks
  (setq-local pcomplete-command-completion-function 'org-pcomplete-initial)
  (setq-local pcomplete-command-name-function 'org-command-at-point)
  (setq-local pcomplete-default-completion-function 'ignore)
  (setq-local pcomplete-parse-arguments-function 'org-parse-arguments)
  (setq-local pcomplete-termination-string "")
  (setq-local buffer-face-mode-face 'org-default)

  ;; If empty file that did not turn on Org mode automatically, make
  ;; it to.
  (when (and org-insert-mode-line-in-empty-file
	     (called-interactively-p 'any)
	     (= (point-min) (point-max)))
    (insert "#    -*- mode: org -*-\n\n"))
  (unless org-inhibit-startup
    (org-unmodified
     (when org-startup-with-beamer-mode (org-beamer-mode))
     (when org-startup-align-all-tables
       (org-table-map-tables #'org-table-align t))
     (when org-startup-with-inline-images (org-display-inline-images))
     (when org-startup-with-latex-preview (org-toggle-latex-fragment '(16)))
     (unless org-inhibit-startup-visibility-stuff (org-set-startup-visibility))
     (when org-startup-truncated (setq truncate-lines t))
     (when org-startup-indented (require 'org-indent) (org-indent-mode 1))
     (org-refresh-effort-properties)))
  ;; Try to set `org-hide' face correctly.
  (let ((foreground (org-find-invisible-foreground)))
    (when foreground
      (set-face-foreground 'org-hide foreground))))

;; Update `customize-package-emacs-version-alist'
(add-to-list 'customize-package-emacs-version-alist
	     '(Org ("8.0" . "24.4")
		   ("8.1" . "24.4")
		   ("8.2" . "24.4")
		   ("8.2.7" . "24.4")
		   ("8.3" . "26.1")
		   ("9.0" . "26.1")
		   ("9.1" . "26.1")))

(defvar org-mode-transpose-word-syntax-table
  (let ((st (make-syntax-table text-mode-syntax-table)))
    (dolist (c org-emphasis-alist st)
      (modify-syntax-entry (string-to-char (car c)) "w p" st))))

(when (fboundp 'abbrev-table-put)
  (abbrev-table-put org-mode-abbrev-table
		    :parents (list text-mode-abbrev-table)))

(defun org-find-invisible-foreground ()
  (let ((candidates (remove
		     "unspecified-bg"
		     (nconc
		      (list (face-background 'default)
			    (face-background 'org-default))
		      (mapcar
		       (lambda (alist)
			 (when (boundp alist)
			   (cdr (assq 'background-color (symbol-value alist)))))
		       '(default-frame-alist initial-frame-alist window-system-default-frame-alist))
		      (list (face-foreground 'org-hide))))))
    (car (remove nil candidates))))

(defun org-current-time (&optional rounding-minutes past)
  "Current time, possibly rounded to ROUNDING-MINUTES.
When ROUNDING-MINUTES is not an integer, fall back on the car of
`org-time-stamp-rounding-minutes'.  When PAST is non-nil, ensure
the rounding returns a past time."
  (let ((r (or (and (integerp rounding-minutes) rounding-minutes)
	       (car org-time-stamp-rounding-minutes)))
	(time (decode-time)) res)
    (if (< r 1)
	(current-time)
      (setq res
	    (apply 'encode-time
		   (append (list 0 (* r (floor (+ .5 (/ (float (nth 1 time)) r)))))
			   (nthcdr 2 time))))
      (if (and past (< (float-time (time-subtract (current-time) res)) 0))
	  (seconds-to-time (- (float-time res) (* r 60)))
	res))))

(defun org-today ()
  "Return today date, considering `org-extend-today-until'."
  (time-to-days
   (time-subtract (current-time)
		  (list 0 (* 3600 org-extend-today-until) 0))))

;;;; Font-Lock stuff, including the activators

(defvar org-mouse-map (make-sparse-keymap))
(org-defkey org-mouse-map [mouse-2] 'org-open-at-mouse)
(org-defkey org-mouse-map [mouse-3] 'org-find-file-at-mouse)
(when org-mouse-1-follows-link
  (org-defkey org-mouse-map [follow-link] 'mouse-face))
(when org-tab-follows-link
  (org-defkey org-mouse-map [(tab)] 'org-open-at-point)
  (org-defkey org-mouse-map "\C-i" 'org-open-at-point))

(require 'font-lock)

(defconst org-non-link-chars "]\t\n\r<>")
(defvar org-link-types-re nil
  "Matches a link that has a url-like prefix like \"http:\"")
(defvar org-link-re-with-space nil
  "Matches a link with spaces, optional angular brackets around it.")
(defvar org-link-re-with-space2 nil
  "Matches a link with spaces, optional angular brackets around it.")
(defvar org-link-re-with-space3 nil
  "Matches a link with spaces, only for internal part in bracket links.")
(defvar org-angle-link-re nil
  "Matches link with angular brackets, spaces are allowed.")
(defvar org-plain-link-re nil
  "Matches plain link, without spaces.")
(defvar org-bracket-link-regexp nil
  "Matches a link in double brackets.")
(defvar org-bracket-link-analytic-regexp nil
  "Regular expression used to analyze links.
Here is what the match groups contain after a match:
1: http:
2: http
3: path
4: [desc]
5: desc")
(defvar org-bracket-link-analytic-regexp++ nil
  "Like `org-bracket-link-analytic-regexp', but include coderef internal type.")
(defvar org-any-link-re nil
  "Regular expression matching any link.")

(defconst org-match-sexp-depth 3
  "Number of stacked braces for sub/superscript matching.")

(defun org-create-multibrace-regexp (left right n)
  "Create a regular expression which will match a balanced sexp.
Opening delimiter is LEFT, and closing delimiter is RIGHT, both given
as single character strings.
The regexp returned will match the entire expression including the
delimiters.  It will also define a single group which contains the
match except for the outermost delimiters.  The maximum depth of
stacked delimiters is N.  Escaping delimiters is not possible."
  (let* ((nothing (concat "[^" left right "]*?"))
	 (or "\\|")
	 (re nothing)
	 (next (concat "\\(?:" nothing left nothing right "\\)+" nothing)))
    (while (> n 1)
      (setq n (1- n)
	    re (concat re or next)
	    next (concat "\\(?:" nothing left next right "\\)+" nothing)))
    (concat left "\\(" re "\\)" right)))

(defconst org-match-substring-regexp
  (concat
   "\\(\\S-\\)\\([_^]\\)\\("
   "\\(?:" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)"
   "\\|"
   "\\(?:" (org-create-multibrace-regexp "(" ")" org-match-sexp-depth) "\\)"
   "\\|"
   "\\(?:\\*\\|[+-]?[[:alnum:].,\\]*[[:alnum:]]\\)\\)")
  "The regular expression matching a sub- or superscript.")

(defconst org-match-substring-with-braces-regexp
  (concat
   "\\(\\S-\\)\\([_^]\\)"
   "\\(" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)")
  "The regular expression matching a sub- or superscript, forcing braces.")

(defun org-make-link-regexps ()
  "Update the link regular expressions.
This should be called after the variable `org-link-parameters' has changed."
  (let ((types-re (regexp-opt (org-link-types) t)))
    (setq org-link-types-re
	  (concat "\\`" types-re ":")
	  org-link-re-with-space
	  (concat "<?" types-re ":"
		  "\\([^" org-non-link-chars " ]"
		  "[^" org-non-link-chars "]*"
		  "[^" org-non-link-chars " ]\\)>?")
	  org-link-re-with-space2
	  (concat "<?" types-re ":"
		  "\\([^" org-non-link-chars " ]"
		  "[^\t\n\r]*"
		  "[^" org-non-link-chars " ]\\)>?")
	  org-link-re-with-space3
	  (concat "<?" types-re ":"
		  "\\([^" org-non-link-chars " ]"
		  "[^\t\n\r]*\\)")
	  org-angle-link-re
	  (format "<%s:\\([^>\n]*\\(?:\n[ \t]*[^> \t\n][^>\n]*\\)*\\)>"
		  types-re)
	  org-plain-link-re
	  (concat
	   "\\<" types-re ":"
	   "\\([^][ \t\n()<>]+\\(?:([[:word:]0-9_]+)\\|\\([^[:punct:] \t\n]\\|/\\)\\)\\)")
	  ;;	 "\\([^]\t\n\r<>() ]+[^]\t\n\r<>,.;() ]\\)")
	  org-bracket-link-regexp
	  "\\[\\[\\([^][]+\\)\\]\\(\\[\\([^][]+\\)\\]\\)?\\]"
	  org-bracket-link-analytic-regexp
	  (concat
	   "\\[\\["
	   "\\(" types-re ":\\)?"
	   "\\([^]]+\\)"
	   "\\]"
	   "\\(\\[" "\\([^]]+\\)" "\\]\\)?"
	   "\\]")
	  org-bracket-link-analytic-regexp++
	  (concat
	   "\\[\\["
	   "\\(" (regexp-opt (cons "coderef" (org-link-types)) t) ":\\)?"
	   "\\([^]]+\\)"
	   "\\]"
	   "\\(\\[" "\\([^]]+\\)" "\\]\\)?"
	   "\\]")
	  org-any-link-re
	  (concat "\\(" org-bracket-link-regexp "\\)\\|\\("
		  org-angle-link-re "\\)\\|\\("
		  org-plain-link-re "\\)"))))

(org-make-link-regexps)

(defvar org-emph-face nil)

(defun org-do-emphasis-faces (limit)
  "Run through the buffer and emphasize strings."
  (let ((quick-re (format "\\([%s]\\|^\\)\\([~=*/_+]\\)"
			  (car org-emphasis-regexp-components))))
    (catch :exit
      (while (re-search-forward quick-re limit t)
	(let* ((marker (match-string 2))
	       (verbatim? (member marker '("~" "="))))
	  (when (save-excursion
		  (goto-char (match-beginning 0))
		  (and
		   ;; Do not match headline stars.  Do not consider
		   ;; stars of a headline as closing marker for bold
		   ;; markup either.
		   (not (and (equal marker "*")
			     (save-excursion
			       (forward-char)
			       (skip-chars-backward "*")
			       (looking-at-p org-outline-regexp-bol))))
		   ;; Do not match table hlines.
		   (not (and (equal marker "+")
			     (org-match-line
			      "^[ \t]*\\(|[-+]+|?\\|\\+[-+]+\\+\\)[ \t]*$")))
		   (looking-at (if verbatim? org-verbatim-re org-emph-re))
		   ;; At a table row, do not cross cell boundaries.
		   (not (and (save-match-data (org-match-line "[ \t]*|"))
			     (string-match-p "|" (match-string 4))))))
	    (pcase-let ((`(,_ ,face ,_) (assoc marker org-emphasis-alist)))
	      (font-lock-prepend-text-property
	       (match-beginning 2) (match-end 2) 'face face)
	      (when verbatim?
		(org-remove-flyspell-overlays-in
		 (match-beginning 0) (match-end 0)))
	      (add-text-properties (match-beginning 2) (match-end 2)
				   '(font-lock-multiline t org-emphasis t))
	      (when org-hide-emphasis-markers
		(add-text-properties (match-end 4) (match-beginning 5)
				     '(invisible org-link))
		(add-text-properties (match-beginning 3) (match-end 3)
				     '(invisible org-link)))
	      (throw :exit t))))))))

(defun org-emphasize (&optional char)
  "Insert or change an emphasis, i.e. a font like bold or italic.
If there is an active region, change that region to a new emphasis.
If there is no region, just insert the marker characters and position
the cursor between them.
CHAR should be the marker character.  If it is a space, it means to
remove the emphasis of the selected region.
If CHAR is not given (for example in an interactive call) it will be
prompted for."
  (interactive)
  (let ((erc org-emphasis-regexp-components)
	(string "") beg end move s)
    (if (org-region-active-p)
	(setq beg (region-beginning)
	      end (region-end)
	      string (buffer-substring beg end))
      (setq move t))

    (unless char
      (message "Emphasis marker or tag: [%s]"
	       (mapconcat #'car org-emphasis-alist ""))
      (setq char (read-char-exclusive)))
    (if (equal char ?\s)
	(setq s ""
	      move nil)
      (unless (assoc (char-to-string char) org-emphasis-alist)
	(user-error "No such emphasis marker: \"%c\"" char))
      (setq s (char-to-string char)))
    (while (and (> (length string) 1)
		(equal (substring string 0 1) (substring string -1))
		(assoc (substring string 0 1) org-emphasis-alist))
      (setq string (substring string 1 -1)))
    (setq string (concat s string s))
    (when beg (delete-region beg end))
    (unless (or (bolp)
		(string-match (concat "[" (nth 0 erc) "\n]")
			      (char-to-string (char-before (point)))))
      (insert " "))
    (unless (or (eobp)
		(string-match (concat "[" (nth 1 erc) "\n]")
			      (char-to-string (char-after (point)))))
      (insert " ") (backward-char 1))
    (insert string)
    (and move (backward-char 1))))

(defconst org-nonsticky-props
  '(mouse-face highlight keymap invisible intangible help-echo org-linked-text htmlize-link))

(defsubst org-rear-nonsticky-at (pos)
  (add-text-properties (1- pos) pos (list 'rear-nonsticky org-nonsticky-props)))

(defun org-activate-links (limit)
  "Add link properties to links.
This includes angle, plain, and bracket links."
  (catch :exit
    (while (re-search-forward org-any-link-re limit t)
      (let* ((start (match-beginning 0))
	     (end (match-end 0))
	     (style (cond ((eq ?< (char-after start)) 'angle)
			  ((eq ?\[ (char-after (1+ start))) 'bracket)
			  (t 'plain))))
	(when (and (memq style org-highlight-links)
		   ;; Do not confuse plain links with tags.
		   (not (and (eq style 'plain)
			     (let ((face (get-text-property
					  (max (1- start) (point-min)) 'face)))
			       (if (consp face) (memq 'org-tag face)
				 (eq 'org-tag face))))))
	  (let* ((link-object (save-excursion
				(goto-char start)
				(save-match-data (org-element-link-parser))))
		 (link (org-element-property :raw-link link-object))
		 (type (org-element-property :type link-object))
		 (path (org-element-property :path link-object))
		 (properties		;for link's visible part
		  (list
		   'face (pcase (org-link-get-parameter type :face)
			   ((and (pred functionp) face) (funcall face path))
			   ((and (pred facep) face) face)
			   ((and (pred consp) face) face) ;anonymous
			   (_ 'org-link))
		   'mouse-face (or (org-link-get-parameter type :mouse-face)
				   'highlight)
		   'keymap (or (org-link-get-parameter type :keymap)
			       org-mouse-map)
		   'help-echo (pcase (org-link-get-parameter type :help-echo)
				((and (pred stringp) echo) echo)
				((and (pred functionp) echo) echo)
				(_ (concat "LINK: " link)))
		   'htmlize-link (pcase (org-link-get-parameter type
								:htmlize-link)
				   ((and (pred functionp) f) (funcall f))
				   (_ `(:uri ,link)))
		   'font-lock-multiline t)))
	    (org-remove-flyspell-overlays-in start end)
	    (org-rear-nonsticky-at end)
	    (if (not (eq 'bracket style))
		(add-text-properties start end properties)
	      ;; Handle invisible parts in bracket links.
	      (remove-text-properties start end '(invisible nil))
	      (let ((hidden
		     (append `(invisible
			       ,(or (org-link-get-parameter type :display)
				    'org-link))
			     properties))
		    (visible-start (or (match-beginning 4) (match-beginning 2)))
		    (visible-end (or (match-end 4) (match-end 2))))
		(add-text-properties start visible-start hidden)
		(add-text-properties visible-start visible-end properties)
		(add-text-properties visible-end end hidden)
		(org-rear-nonsticky-at visible-start)
		(org-rear-nonsticky-at visible-end)))
	    (let ((f (org-link-get-parameter type :activate-func)))
	      (when (functionp f)
		(funcall f start end path (eq style 'bracket))))
	    (throw :exit t)))))		;signal success
    nil))

(defun org-activate-code (limit)
  (when (re-search-forward "^[ \t]*\\(:\\(?: .*\\|$\\)\n?\\)" limit t)
    (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
    (remove-text-properties (match-beginning 0) (match-end 0)
			    '(display t invisible t intangible t))
    t))

(defcustom org-src-fontify-natively t
  "When non-nil, fontify code in code blocks.
See also the `org-block' face."
  :type 'boolean
  :version "26.1"
  :package-version '(Org . "8.3")
  :group 'org-appearance
  :group 'org-babel)

(defcustom org-allow-promoting-top-level-subtree nil
  "When non-nil, allow promoting a top level subtree.
The leading star of the top level headline will be replaced
by a #."
  :type 'boolean
  :version "24.1"
  :group 'org-appearance)

(defun org-fontify-meta-lines-and-blocks (limit)
  (condition-case nil
      (org-fontify-meta-lines-and-blocks-1 limit)
    (error (message "org-mode fontification error in %S at %d"
		    (current-buffer)
		    (line-number-at-pos)))))

(defun org-fontify-meta-lines-and-blocks-1 (limit)
  "Fontify #+ lines and blocks."
  (let ((case-fold-search t))
    (when (re-search-forward
	   "^\\([ \t]*#\\(\\(\\+[a-zA-Z]+:?\\| \\|$\\)\\(_\\([a-zA-Z]+\\)\\)?\\)[ \t]*\\(\\([^ \t\n]*\\)[ \t]*\\(.*\\)\\)\\)"
	   limit t)
      (let ((beg (match-beginning 0))
	    (block-start (match-end 0))
	    (block-end nil)
	    (lang (match-string 7))
	    (beg1 (line-beginning-position 2))
	    (dc1 (downcase (match-string 2)))
	    (dc3 (downcase (match-string 3)))
	    end end1 quoting block-type)
	(cond
	 ((and (match-end 4) (equal dc3 "+begin"))
	  ;; Truly a block
	  (setq block-type (downcase (match-string 5))
		quoting (member block-type org-protecting-blocks))
	  (when (re-search-forward
		 (concat "^[ \t]*#\\+end" (match-string 4) "\\>.*")
		 nil t)  ;; on purpose, we look further than LIMIT
	    (setq end (min (point-max) (match-end 0))
		  end1 (min (point-max) (1- (match-beginning 0))))
	    (setq block-end (match-beginning 0))
	    (when quoting
	      (org-remove-flyspell-overlays-in beg1 end1)
	      (remove-text-properties beg end
				      '(display t invisible t intangible t)))
	    (add-text-properties
	     beg end '(font-lock-fontified t font-lock-multiline t))
	    (add-text-properties beg beg1 '(face org-meta-line))
	    (org-remove-flyspell-overlays-in beg beg1)
	    (add-text-properties	; For end_src
	     end1 (min (point-max) (1+ end)) '(face org-meta-line))
	    (org-remove-flyspell-overlays-in end1 end)
	    (cond
	     ((and lang (not (string= lang "")) org-src-fontify-natively)
	      (org-src-font-lock-fontify-block lang block-start block-end)
	      (add-text-properties beg1 block-end '(src-block t)))
	     (quoting
	      (add-text-properties beg1 (min (point-max) (1+ end1))
				   (list 'face
					 (list :inherit
					       (let ((face-name
						      (intern (format "org-block-%s" lang))))
						 (append (and (facep face-name) (list face-name))
							 '(org-block))))))) ; end of source block
	     ((not org-fontify-quote-and-verse-blocks))
	     ((string= block-type "quote")
	      (add-face-text-property
	       beg1 (min (point-max) (1+ end1)) 'org-quote t))
	     ((string= block-type "verse")
	      (add-face-text-property
	       beg1 (min (point-max) (1+ end1)) 'org-verse t)))
	    (add-text-properties beg beg1 '(face org-block-begin-line))
	    (add-text-properties (min (point-max) (1+ end)) (min (point-max) (1+ end1))
				 '(face org-block-end-line))
	    t))
	 ((member dc1 '("+title:" "+author:" "+email:" "+date:"))
	  (org-remove-flyspell-overlays-in
	   (match-beginning 0)
	   (if (equal "+title:" dc1) (match-end 2) (match-end 0)))
	  (add-text-properties
	   beg (match-end 3)
	   (if (member (intern (substring dc1 1 -1)) org-hidden-keywords)
	       '(font-lock-fontified t invisible t)
	     '(font-lock-fontified t face org-document-info-keyword)))
	  (add-text-properties
	   (match-beginning 6) (min (point-max) (1+ (match-end 6)))
	   (if (string-equal dc1 "+title:")
	       '(font-lock-fontified t face org-document-title)
	     '(font-lock-fontified t face org-document-info))))
	 ((string-prefix-p "+caption" dc1)
	  (org-remove-flyspell-overlays-in (match-end 2) (match-end 0))
	  (remove-text-properties (match-beginning 0) (match-end 0)
				  '(display t invisible t intangible t))
	  ;; Handle short captions.
	  (save-excursion
	    (beginning-of-line)
	    (looking-at "\\([ \t]*#\\+caption\\(?:\\[.*\\]\\)?:\\)[ \t]*"))
	  (add-text-properties (line-beginning-position) (match-end 1)
			       '(font-lock-fontified t face org-meta-line))
	  (add-text-properties (match-end 0) (line-end-position)
			       '(font-lock-fontified t face org-block))
	  t)
	 ((member dc3 '(" " ""))
	  (org-remove-flyspell-overlays-in beg (match-end 0))
	  (add-text-properties
	   beg (match-end 0)
	   '(font-lock-fontified t face font-lock-comment-face)))
	 (t ;; just any other in-buffer setting, but not indented
	  (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
	  (remove-text-properties (match-beginning 0) (match-end 0)
				  '(display t invisible t intangible t))
	  (add-text-properties beg (match-end 0)
			       '(font-lock-fontified t face org-meta-line))
	  t))))))

(defun org-fontify-drawers (limit)
  "Fontify drawers."
  (when (re-search-forward org-drawer-regexp limit t)
    (add-text-properties
     (match-beginning 0) (match-end 0)
     '(font-lock-fontified t face org-special-keyword))
    (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
    t))

(defun org-fontify-macros (limit)
  "Fontify macros."
  (when (re-search-forward "\\({{{\\).+?\\(}}}\\)" limit t)
    (add-text-properties
     (match-beginning 0) (match-end 0)
     '(font-lock-fontified t face org-macro))
    (when org-hide-macro-markers
      (add-text-properties (match-end 2) (match-beginning 2)
			   '(invisible t))
      (add-text-properties (match-beginning 1) (match-end 1)
			   '(invisible t)))
    (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
    t))

(defun org-activate-footnote-links (limit)
  "Add text properties for footnotes."
  (let ((fn (org-footnote-next-reference-or-definition limit)))
    (when fn
      (let* ((beg (nth 1 fn))
	     (end (nth 2 fn))
	     (label (car fn))
	     (referencep (/= (line-beginning-position) beg)))
	(when (and referencep (nth 3 fn))
	  (save-excursion
	    (goto-char beg)
	    (search-forward (or label "fn:"))
	    (org-remove-flyspell-overlays-in beg (match-end 0))))
	(add-text-properties beg end
			     (list 'mouse-face 'highlight
				   'keymap org-mouse-map
				   'help-echo
				   (if referencep "Footnote reference"
				     "Footnote definition")
				   'font-lock-fontified t
				   'font-lock-multiline t
				   'face 'org-footnote))))))

(defun org-activate-dates (limit)
  "Add text properties for dates."
  (when (and (re-search-forward org-tsr-regexp-both limit t)
	     (not (equal (char-before (match-beginning 0)) 91)))
    (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
    (add-text-properties (match-beginning 0) (match-end 0)
			 (list 'mouse-face 'highlight
			       'keymap org-mouse-map))
    (org-rear-nonsticky-at (match-end 0))
    (when org-display-custom-times
      ;; If it's a date range, activate custom time for second date.
      (when (match-end 3)
	(org-display-custom-time (match-beginning 3) (match-end 3)))
      (org-display-custom-time (match-beginning 1) (match-end 1)))
    t))

(defvar-local org-target-link-regexp nil
  "Regular expression matching radio targets in plain text.")

(defconst org-target-regexp (let ((border "[^<>\n\r \t]"))
			      (format "<<\\(%s\\|%s[^<>\n\r]*%s\\)>>"
				      border border border))
  "Regular expression matching a link target.")

(defconst org-radio-target-regexp (format "<%s>" org-target-regexp)
  "Regular expression matching a radio target.")

(defconst org-any-target-regexp
  (format "%s\\|%s" org-radio-target-regexp org-target-regexp)
  "Regular expression matching any target.")

(defun org-activate-target-links (limit)
  "Add text properties for target matches."
  (when org-target-link-regexp
    (let ((case-fold-search t))
      (when (re-search-forward org-target-link-regexp limit t)
	(org-remove-flyspell-overlays-in (match-beginning 1) (match-end 1))
	(add-text-properties (match-beginning 1) (match-end 1)
			     (list 'mouse-face 'highlight
				   'keymap org-mouse-map
				   'help-echo "Radio target link"
				   'org-linked-text t))
	(org-rear-nonsticky-at (match-end 1))
	t))))

(defun org-update-radio-target-regexp ()
  "Find all radio targets in this file and update the regular expression.
Also refresh fontification if needed."
  (interactive)
  (let ((old-regexp org-target-link-regexp)
	(before-re "\\(?:^\\|[^[:alnum:]]\\)\\(")
	(after-re "\\)\\(?:$\\|[^[:alnum:]]\\)")
	(targets
	 (org-with-wide-buffer
	  (goto-char (point-min))
	  (let (rtn)
	    (while (re-search-forward org-radio-target-regexp nil t)
	      ;; Make sure point is really within the object.
	      (backward-char)
	      (let ((obj (org-element-context)))
		(when (eq (org-element-type obj) 'radio-target)
		  (cl-pushnew (org-element-property :value obj) rtn
			      :test #'equal))))
	    rtn))))
    (setq org-target-link-regexp
	  (and targets
	       (concat before-re
		       (mapconcat
			(lambda (x)
			  (replace-regexp-in-string
			   " +" "\\s-+" (regexp-quote x) t t))
			targets
			"\\|")
		       after-re)))
    (unless (equal old-regexp org-target-link-regexp)
      ;; Clean-up cache.
      (let ((regexp (cond ((not old-regexp) org-target-link-regexp)
			  ((not org-target-link-regexp) old-regexp)
			  (t
			   (concat before-re
				   (mapconcat
				    (lambda (re)
				      (substring re (length before-re)
						 (- (length after-re))))
				    (list old-regexp org-target-link-regexp)
				    "\\|")
				   after-re)))))
	(org-with-wide-buffer
	 (goto-char (point-min))
	 (while (re-search-forward regexp nil t)
	   (org-element-cache-refresh (match-beginning 1)))))
      ;; Re fontify buffer.
      (when (memq 'radio org-highlight-links)
	(org-restart-font-lock)))))

(defun org-hide-wide-columns (limit)
  (let (s e)
    (setq s (text-property-any (point) (or limit (point-max))
			       'org-cwidth t))
    (when s
      (setq e (next-single-property-change s 'org-cwidth))
      (add-text-properties s e '(invisible org-cwidth))
      (goto-char e)
      t)))

(defvar org-latex-and-related-regexp nil
  "Regular expression for highlighting LaTeX, entities and sub/superscript.")

(defun org-compute-latex-and-related-regexp ()
  "Compute regular expression for LaTeX, entities and sub/superscript.
Result depends on variable `org-highlight-latex-and-related'."
  (setq-local
   org-latex-and-related-regexp
   (let* ((re-sub
	   (cond ((not (memq 'script org-highlight-latex-and-related)) nil)
		 ((eq org-use-sub-superscripts '{})
		  (list org-match-substring-with-braces-regexp))
		 (org-use-sub-superscripts (list org-match-substring-regexp))))
	  (re-latex
	   (when (memq 'latex org-highlight-latex-and-related)
	     (let ((matchers (plist-get org-format-latex-options :matchers)))
	       (delq nil
		     (mapcar (lambda (x)
			       (and (member (car x) matchers) (nth 1 x)))
			     org-latex-regexps)))))
	  (re-entities
	   (when (memq 'entities org-highlight-latex-and-related)
	     (list "\\\\\\(there4\\|sup[123]\\|frac[13][24]\\|[a-zA-Z]+\\)\\($\\|{}\\|[^[:alpha:]]\\)"))))
     (mapconcat 'identity (append re-latex re-entities re-sub) "\\|"))))

(defun org-do-latex-and-related (limit)
  "Highlight LaTeX snippets and environments, entities and sub/superscript.
LIMIT bounds the search for syntax to highlight.  Stop at first
highlighted object, if any.  Return t if some highlighting was
done, nil otherwise."
  (when (org-string-nw-p org-latex-and-related-regexp)
    (catch 'found
      (while (re-search-forward org-latex-and-related-regexp limit t)
	(unless
	    (cl-some
	     (lambda (f)
	       (memq f '(org-code org-verbatim underline org-special-keyword)))
	     (save-excursion
	       (goto-char (1+ (match-beginning 0)))
	       (face-at-point nil t)))
	  (let ((offset (if (memq (char-after (1+ (match-beginning 0)))
				  '(?_ ?^))
			    1
			  0)))
	    (font-lock-prepend-text-property
	     (+ offset (match-beginning 0)) (match-end 0)
	     'face 'org-latex-and-related)
	    (add-text-properties (+ offset (match-beginning 0)) (match-end 0)
				 '(font-lock-multiline t)))
	  (throw 'found t)))
      nil)))

(defun org-restart-font-lock ()
  "Restart `font-lock-mode', to force refontification."
  (when (and (boundp 'font-lock-mode) font-lock-mode)
    (font-lock-mode -1)
    (font-lock-mode 1)))

(defun org-activate-tags (limit)
  (when (re-search-forward
	 "^\\*+.*[ \t]\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$" limit t)
    (org-remove-flyspell-overlays-in (match-beginning 1) (match-end 1))
    (add-text-properties (match-beginning 1) (match-end 1)
			 (list 'mouse-face 'highlight
			       'keymap org-mouse-map))
    (org-rear-nonsticky-at (match-end 1))
    t))

(defun org-outline-level ()
  "Compute the outline level of the heading at point.

If this is called at a normal headline, the level is the number
of stars.  Use `org-reduced-level' to remove the effect of
`org-odd-levels'.  Unlike to `org-current-level', this function
takes into consideration inlinetasks."
  (org-with-wide-buffer
   (end-of-line)
   (if (re-search-backward org-outline-regexp-bol nil t)
       (1- (- (match-end 0) (match-beginning 0)))
     0)))

(defvar org-font-lock-keywords nil)

(defsubst org-re-property (property &optional literal allow-null value)
  "Return a regexp matching a PROPERTY line.

When optional argument LITERAL is non-nil, do not quote PROPERTY.
This is useful when PROPERTY is a regexp.  When ALLOW-NULL is
non-nil, match properties even without a value.

Match group 3 is set to the value when it exists.  If there is no
value and ALLOW-NULL is non-nil, it is set to the empty string.

With optional argument VALUE, match only property lines with
that value; in this case, ALLOW-NULL is ignored.  VALUE is quoted
unless LITERAL is non-nil."
  (concat
   "^\\(?4:[ \t]*\\)"
   (format "\\(?1::\\(?2:%s\\):\\)"
	   (if literal property (regexp-quote property)))
   (cond (value
	  (format "[ \t]+\\(?3:%s\\)\\(?5:[ \t]*\\)$"
		  (if literal value (regexp-quote value))))
	 (allow-null
	  "\\(?:\\(?3:$\\)\\|[ \t]+\\(?3:.*?\\)\\)\\(?5:[ \t]*\\)$")
	 (t
	  "[ \t]+\\(?3:[^ \r\t\n]+.*?\\)\\(?5:[ \t]*\\)$"))))

(defconst org-property-re
  (org-re-property "\\S-+" 'literal t)
  "Regular expression matching a property line.
There are four matching groups:
1: :PROPKEY: including the leading and trailing colon,
2: PROPKEY without the leading and trailing colon,
3: PROPVAL without leading or trailing spaces,
4: the indentation of the current line,
5: trailing whitespace.")

(defvar org-font-lock-hook nil
  "Functions to be called for special font lock stuff.")

(defvar org-font-lock-extra-keywords nil) ;Dynamically scoped.

(defvar org-font-lock-set-keywords-hook nil
  "Functions that can manipulate `org-font-lock-extra-keywords'.
This is called after `org-font-lock-extra-keywords' is defined, but before
it is installed to be used by font lock.  This can be useful if something
needs to be inserted at a specific position in the font-lock sequence.")

(defun org-font-lock-hook (limit)
  "Run `org-font-lock-hook' within LIMIT."
  (run-hook-with-args 'org-font-lock-hook limit))

(defun org-set-font-lock-defaults ()
  "Set font lock defaults for the current buffer."
  (let* ((em org-fontify-emphasized-text)
	 (lk org-highlight-links)
	 (org-font-lock-extra-keywords
	  (list
	   ;; Call the hook
	   '(org-font-lock-hook)
	   ;; Headlines
	   `(,(if org-fontify-whole-heading-line
		  "^\\(\\**\\)\\(\\* \\)\\(.*\n?\\)"
		"^\\(\\**\\)\\(\\* \\)\\(.*\\)")
	     (1 (org-get-level-face 1))
	     (2 (org-get-level-face 2))
	     (3 (org-get-level-face 3)))
	   ;; Table lines
	   '("^[ \t]*\\(\\(|\\|\\+-[-+]\\).*\\S-\\)"
	     (1 'org-table t))
	   ;; Table internals
	   '("^[ \t]*|\\(?:.*?|\\)? *\\(:?=[^|\n]*\\)" (1 'org-formula t))
	   '("^[ \t]*| *\\([#*]\\) *|" (1 'org-formula t))
	   '("^[ \t]*|\\( *\\([$!_^/]\\) *|.*\\)|" (1 'org-formula t))
	   '("| *\\(<[lrc]?[0-9]*>\\)" (1 'org-formula t))
	   ;; Drawers
	   '(org-fontify-drawers)
	   ;; Properties
	   (list org-property-re
		 '(1 'org-special-keyword t)
		 '(3 'org-property-value t))
	   ;; Link related fontification.
	   '(org-activate-links)
	   (when (memq 'tag lk) '(org-activate-tags (1 'org-tag prepend)))
	   (when (memq 'radio lk) '(org-activate-target-links (1 'org-link t)))
	   (when (memq 'date lk) '(org-activate-dates (0 'org-date t)))
	   (when (memq 'footnote lk) '(org-activate-footnote-links))
           ;; Targets.
           (list org-any-target-regexp '(0 'org-target t))
	   ;; Diary sexps.
	   '("^&?%%(.*\\|<%%([^>\n]*?>" (0 'org-sexp-date t))
	   ;; Macro
	   '(org-fontify-macros)
	   '(org-hide-wide-columns (0 nil append))
	   ;; TODO keyword
	   (list (format org-heading-keyword-regexp-format
			 org-todo-regexp)
		 '(2 (org-get-todo-face 2) t))
	   ;; DONE
	   (if org-fontify-done-headline
	       (list (format org-heading-keyword-regexp-format
			     (concat
			      "\\(?:"
			      (mapconcat 'regexp-quote org-done-keywords "\\|")
			      "\\)"))
		     '(2 'org-headline-done t))
	     nil)
	   ;; Priorities
	   '(org-font-lock-add-priority-faces)
	   ;; Tags
	   '(org-font-lock-add-tag-faces)
	   ;; Tags groups
	   (when (and org-group-tags org-tag-groups-alist)
	     (list (concat org-outline-regexp-bol ".+\\(:"
			   (regexp-opt (mapcar 'car org-tag-groups-alist))
			   ":\\).*$")
		   '(1 'org-tag-group prepend)))
	   ;; Special keywords
	   (list (concat "\\<" org-deadline-string) '(0 'org-special-keyword t))
	   (list (concat "\\<" org-scheduled-string) '(0 'org-special-keyword t))
	   (list (concat "\\<" org-closed-string) '(0 'org-special-keyword t))
	   (list (concat "\\<" org-clock-string) '(0 'org-special-keyword t))
	   ;; Emphasis
	   (when em '(org-do-emphasis-faces))
	   ;; Checkboxes
	   '("^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\(\\[[- X]\\]\\)"
	     1 'org-checkbox prepend)
	   (when (cdr (assq 'checkbox org-list-automatic-rules))
	     '("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
	       (0 (org-get-checkbox-statistics-face) t)))
	   ;; Description list items
	   '("^[ \t]*[-+*][ \t]+\\(.*?[ \t]+::\\)\\([ \t]+\\|$\\)"
	     1 'org-list-dt prepend)
	   ;; ARCHIVEd headings
	   (list (concat
		  org-outline-regexp-bol
		  "\\(.*:" org-archive-tag ":.*\\)")
		 '(1 'org-archived prepend))
	   ;; Specials
	   '(org-do-latex-and-related)
	   '(org-fontify-entities)
	   '(org-raise-scripts)
	   ;; Code
	   '(org-activate-code (1 'org-code t))
	   ;; COMMENT
	   (list (format
		  "^\\*+\\(?: +%s\\)?\\(?: +\\[#[A-Z0-9]\\]\\)? +\\(?9:%s\\)\\(?: \\|$\\)"
		  org-todo-regexp
		  org-comment-string)
		 '(9 'org-special-keyword t))
	   ;; Blocks and meta lines
	   '(org-fontify-meta-lines-and-blocks))))
    (setq org-font-lock-extra-keywords (delq nil org-font-lock-extra-keywords))
    (run-hooks 'org-font-lock-set-keywords-hook)
    ;; Now set the full font-lock-keywords
    (setq-local org-font-lock-keywords org-font-lock-extra-keywords)
    (setq-local font-lock-defaults
		'(org-font-lock-keywords t nil nil backward-paragraph))
    (kill-local-variable 'font-lock-keywords)
    nil))

(defun org-toggle-pretty-entities ()
  "Toggle the composition display of entities as UTF8 characters."
  (interactive)
  (setq-local org-pretty-entities (not org-pretty-entities))
  (org-restart-font-lock)
  (if org-pretty-entities
      (message "Entities are now displayed as UTF8 characters")
    (save-restriction
      (widen)
      (decompose-region (point-min) (point-max))
      (message "Entities are now displayed as plain text"))))

(defvar-local org-custom-properties-overlays nil
  "List of overlays used for custom properties.")

(defun org-toggle-custom-properties-visibility ()
  "Display or hide properties in `org-custom-properties'."
  (interactive)
  (if org-custom-properties-overlays
      (progn (mapc #'delete-overlay org-custom-properties-overlays)
	     (setq org-custom-properties-overlays nil))
    (when org-custom-properties
      (org-with-wide-buffer
       (goto-char (point-min))
       (let ((regexp (org-re-property (regexp-opt org-custom-properties) t t)))
	 (while (re-search-forward regexp nil t)
	   (let ((end (cdr (save-match-data (org-get-property-block)))))
	     (when (and end (< (point) end))
	       ;; Hide first custom property in current drawer.
	       (let ((o (make-overlay (match-beginning 0) (1+ (match-end 0)))))
		 (overlay-put o 'invisible t)
		 (overlay-put o 'org-custom-property t)
		 (push o org-custom-properties-overlays))
	       ;; Hide additional custom properties in the same drawer.
	       (while (re-search-forward regexp end t)
		 (let ((o (make-overlay (match-beginning 0) (1+ (match-end 0)))))
		   (overlay-put o 'invisible t)
		   (overlay-put o 'org-custom-property t)
		   (push o org-custom-properties-overlays)))))
	   ;; Each entry is limited to a single property drawer.
	   (outline-next-heading)))))))

(defun org-fontify-entities (limit)
  "Find an entity to fontify."
  (let (ee)
    (when org-pretty-entities
      (catch 'match
	;; "\_ "-family is left out on purpose.  Only the first one,
	;; i.e., "\_ ", could be fontified anyway, and it would be
	;; confusing when adding a second white space character.
	(while (re-search-forward
		"\\\\\\(there4\\|sup[123]\\|frac[13][24]\\|[a-zA-Z]+\\)\\($\\|{}\\|[^[:alpha:]\n]\\)"
		limit t)
	  (when (and (not (org-at-comment-p))
		     (setq ee (org-entity-get (match-string 1)))
		     (= (length (nth 6 ee)) 1))
	    (let* ((end (if (equal (match-string 2) "{}")
			    (match-end 2)
			  (match-end 1))))
	      (add-text-properties
	       (match-beginning 0) end
	       (list 'font-lock-fontified t))
	      (compose-region (match-beginning 0) end
			      (nth 6 ee) nil)
	      (backward-char 1)
	      (throw 'match t))))
	nil))))

(defun org-fontify-like-in-org-mode (s &optional odd-levels)
  "Fontify string S like in Org mode."
  (with-temp-buffer
    (insert s)
    (let ((org-odd-levels-only odd-levels))
      (org-mode)
      (org-font-lock-ensure)
      (buffer-string))))

(defvar org-m nil)
(defvar org-l nil)
(defvar org-f nil)
(defun org-get-level-face (n)
  "Get the right face for match N in font-lock matching of headlines."
  (setq org-l (- (match-end 2) (match-beginning 1) 1))
  (when org-odd-levels-only (setq org-l (1+ (/ org-l 2))))
  (if org-cycle-level-faces
      (setq org-f (nth (% (1- org-l) org-n-level-faces) org-level-faces))
    (setq org-f (nth (1- (min org-l org-n-level-faces)) org-level-faces)))
  (cond
   ((eq n 1) (if org-hide-leading-stars 'org-hide org-f))
   ((eq n 2) org-f)
   (t (unless org-level-color-stars-only org-f))))

(defun org-face-from-face-or-color (context inherit face-or-color)
  "Create a face list that inherits INHERIT, but sets the foreground color.
When FACE-OR-COLOR is not a string, just return it."
  (if (stringp face-or-color)
      (list :inherit inherit
	    (cdr (assoc context org-faces-easy-properties))
	    face-or-color)
    face-or-color))

(defun org-get-todo-face (kwd)
  "Get the right face for a TODO keyword KWD.
If KWD is a number, get the corresponding match group."
  (when (numberp kwd) (setq kwd (match-string kwd)))
  (or (org-face-from-face-or-color
       'todo 'org-todo (cdr (assoc kwd org-todo-keyword-faces)))
      (and (member kwd org-done-keywords) 'org-done)
      'org-todo))

(defun org-get-priority-face (priority)
  "Get the right face for PRIORITY.
PRIORITY is a character."
  (or (org-face-from-face-or-color
       'priority 'org-priority (cdr (assq priority org-priority-faces)))
      'org-priority))

(defun org-get-tag-face (tag)
  "Get the right face for TAG.
If TAG is a number, get the corresponding match group."
  (let ((tag (if (wholenump tag) (match-string tag) tag)))
    (or (org-face-from-face-or-color
	 'tag 'org-tag (cdr (assoc tag org-tag-faces)))
	'org-tag)))

(defun org-font-lock-add-priority-faces (limit)
  "Add the special priority faces."
  (while (re-search-forward "^\\*+ .*?\\(\\[#\\(.\\)\\]\\)" limit t)
    (add-text-properties
     (match-beginning 1) (match-end 1)
     (list 'face (org-get-priority-face (string-to-char (match-string 2)))
	   'font-lock-fontified t))))

(defun org-font-lock-add-tag-faces (limit)
  "Add the special tag faces."
  (when (and org-tag-faces org-tags-special-faces-re)
    (while (re-search-forward org-tags-special-faces-re limit t)
      (add-text-properties (match-beginning 1) (match-end 1)
			   (list 'face (org-get-tag-face 1)
				 'font-lock-fontified t))
      (backward-char 1))))

(defun org-unfontify-region (beg end &optional _maybe_loudly)
  "Remove fontification and activation overlays from links."
  (font-lock-default-unfontify-region beg end)
  (let* ((buffer-undo-list t)
	 (inhibit-read-only t) (inhibit-point-motion-hooks t)
	 (inhibit-modification-hooks t)
	 deactivate-mark buffer-file-name buffer-file-truename)
    (decompose-region beg end)
    (remove-text-properties beg end
			    '(mouse-face t keymap t org-linked-text t
					 invisible t intangible t
					 org-emphasis t))
    (org-remove-font-lock-display-properties beg end)))

(defconst org-script-display  '(((raise -0.3) (height 0.7))
				((raise 0.3)  (height 0.7))
				((raise -0.5))
				((raise 0.5)))
  "Display properties for showing superscripts and subscripts.")

(defun org-remove-font-lock-display-properties (beg end)
  "Remove specific display properties that have been added by font lock.
The will remove the raise properties that are used to show superscripts
and subscripts."
  (let (next prop)
    (while (< beg end)
      (setq next (next-single-property-change beg 'display nil end)
	    prop (get-text-property beg 'display))
      (when (member prop org-script-display)
	(put-text-property beg next 'display nil))
      (setq beg next))))

(defun org-raise-scripts (limit)
  "Add raise properties to sub/superscripts."
  (when (and org-pretty-entities org-pretty-entities-include-sub-superscripts
	     (re-search-forward
	      (if (eq org-use-sub-superscripts t)
		  org-match-substring-regexp
		org-match-substring-with-braces-regexp)
	      limit t))
    (let* ((pos (point)) table-p comment-p
	   (mpos (match-beginning 3))
	   (emph-p (get-text-property mpos 'org-emphasis))
	   (link-p (get-text-property mpos 'mouse-face))
	   (keyw-p (eq 'org-special-keyword (get-text-property mpos 'face))))
      (goto-char (point-at-bol))
      (setq table-p (looking-at-p org-table-dataline-regexp)
	    comment-p (looking-at-p "^[ \t]*#[ +]"))
      (goto-char pos)
      ;; Handle a_b^c
      (when (member (char-after) '(?_ ?^)) (goto-char (1- pos)))
      (unless (or comment-p emph-p link-p keyw-p)
	(put-text-property (match-beginning 3) (match-end 0)
			   'display
			   (if (equal (char-after (match-beginning 2)) ?^)
			       (nth (if table-p 3 1) org-script-display)
			     (nth (if table-p 2 0) org-script-display)))
	(add-text-properties (match-beginning 2) (match-end 2)
			     (list 'invisible t))
	(when (and (eq (char-after (match-beginning 3)) ?{)
		   (eq (char-before (match-end 3)) ?}))
	  (add-text-properties (match-beginning 3) (1+ (match-beginning 3))
			       (list 'invisible t))
	  (add-text-properties (1- (match-end 3)) (match-end 3)
			       (list 'invisible t))))
      t)))

;;;; Visibility cycling, including org-goto and indirect buffer

;;; Cycling

(defvar-local org-cycle-global-status nil)
(put 'org-cycle-global-status 'org-state t)
(defvar-local org-cycle-subtree-status nil)
(put 'org-cycle-subtree-status 'org-state t)

(defvar org-inlinetask-min-level)

(defun org-unlogged-message (&rest args)
  "Display a message, but avoid logging it in the *Messages* buffer."
  (let ((message-log-max nil))
    (apply 'message args)))

;;;###autoload
(defun org-cycle (&optional arg)
  "TAB-action and visibility cycling for Org mode.

This is the command invoked in Org mode by the `TAB' key.  Its main
purpose is outline visibility cycling, but it also invokes other actions
in special contexts.

When this function is called with a `\\[universal-argument]' prefix, rotate \
the entire
buffer through 3 states (global cycling)
  1. OVERVIEW: Show only top-level headlines.
  2. CONTENTS: Show all headlines of all levels, but no body text.
  3. SHOW ALL: Show everything.

With a `\\[universal-argument] \\[universal-argument]' prefix argument, \
switch to the startup visibility,
determined by the variable `org-startup-folded', and by any VISIBILITY
properties in the buffer.

With a `\\[universal-argument] \\[universal-argument] \
\\[universal-argument]' prefix argument, show the entire buffer, including
any drawers.

When inside a table, re-align the table and move to the next field.

When point is at the beginning of a headline, rotate the subtree started
by this line through 3 different states (local cycling)
  1. FOLDED:   Only the main headline is shown.
  2. CHILDREN: The main headline and the direct children are shown.
               From this state, you can move to one of the children
               and zoom in further.
  3. SUBTREE:  Show the entire subtree, including body text.
If there is no subtree, switch directly from CHILDREN to FOLDED.

When point is at the beginning of an empty headline and the variable
`org-cycle-level-after-item/entry-creation' is set, cycle the level
of the headline by demoting and promoting it to likely levels.  This
speeds up creation document structure by pressing `TAB' once or several
times right after creating a new headline.

When there is a numeric prefix, go up to a heading with level ARG, do
a `show-subtree' and return to the previous cursor position.  If ARG
is negative, go up that many levels.

When point is not at the beginning of a headline, execute the global
binding for `TAB', which is re-indenting the line.  See the option
`org-cycle-emulate-tab' for details.

As a special case, if point is at the beginning of the buffer and there is
no headline in line 1, this function will act as if called with prefix arg
\(`\\[universal-argument] TAB', same as `S-TAB') also when called without \
prefix arg, but only
if the variable `org-cycle-global-at-bob' is t."
  (interactive "P")
  (org-load-modules-maybe)
  (unless (or (run-hook-with-args-until-success 'org-tab-first-hook)
	      (and org-cycle-level-after-item/entry-creation
		   (or (org-cycle-level)
		       (org-cycle-item-indentation))))
    (let* ((limit-level
	    (or org-cycle-max-level
		(and (boundp 'org-inlinetask-min-level)
		     org-inlinetask-min-level
		     (1- org-inlinetask-min-level))))
	   (nstars (and limit-level
			(if org-odd-levels-only
			    (and limit-level (1- (* limit-level 2)))
			  limit-level)))
	   (org-outline-regexp
	    (if (not (derived-mode-p 'org-mode))
		outline-regexp
	      (concat "\\*" (if nstars (format "\\{1,%d\\} " nstars) "+ "))))
	   (bob-special (and org-cycle-global-at-bob (not arg) (bobp)
			     (not (looking-at org-outline-regexp))))
	   (org-cycle-hook
	    (if bob-special
		(delq 'org-optimize-window-after-visibility-change
		      (copy-sequence org-cycle-hook))
	      org-cycle-hook))
	   (pos (point)))

      (cond

       ((equal arg '(16))
	(setq last-command 'dummy)
	(org-set-startup-visibility)
	(org-unlogged-message "Startup visibility, plus VISIBILITY properties"))

       ((equal arg '(64))
	(outline-show-all)
	(org-unlogged-message "Entire buffer visible, including drawers"))

       ((equal arg '(4)) (org-cycle-internal-global))

       ;; Try hiding block at point.
       ((org-hide-block-toggle-maybe))

       ;; Try cdlatex TAB completion
       ((org-try-cdlatex-tab))

       ;; Table: enter it or move to the next field.
       ((org-at-table-p 'any)
	(if (org-at-table.el-p)
	    (message "%s" (substitute-command-keys "\\<org-mode-map>\
Use `\\[org-edit-special]' to edit table.el tables"))
	  (if arg (org-table-edit-field t)
	    (org-table-justify-field-maybe)
	    (call-interactively 'org-table-next-field))))

       ((run-hook-with-args-until-success 'org-tab-after-check-for-table-hook))

       ;; Global cycling: delegate to `org-cycle-internal-global'.
       (bob-special (org-cycle-internal-global))

       ;; Drawers: delegate to `org-flag-drawer'.
       ((save-excursion
	  (beginning-of-line 1)
	  (looking-at org-drawer-regexp))
	(org-flag-drawer		; toggle block visibility
	 (not (get-char-property (match-end 0) 'invisible))))

       ;; Show-subtree, ARG levels up from here.
       ((integerp arg)
	(save-excursion
	  (org-back-to-heading)
	  (outline-up-heading (if (< arg 0) (- arg)
				(- (funcall outline-level) arg)))
	  (org-show-subtree)))

       ;; Inline task: delegate to `org-inlinetask-toggle-visibility'.
       ((and (featurep 'org-inlinetask)
	     (org-inlinetask-at-task-p)
	     (or (bolp) (not (eq org-cycle-emulate-tab 'exc-hl-bol))))
	(org-inlinetask-toggle-visibility))

       ;; At an item/headline: delegate to `org-cycle-internal-local'.
       ((and (or (and org-cycle-include-plain-lists (org-at-item-p))
		 (save-excursion (move-beginning-of-line 1)
				 (looking-at org-outline-regexp)))
	     (or (bolp) (not (eq org-cycle-emulate-tab 'exc-hl-bol))))
	(org-cycle-internal-local))

       ;; From there: TAB emulation and template completion.
       (buffer-read-only (org-back-to-heading))

       ((run-hook-with-args-until-success
	 'org-tab-after-check-for-cycling-hook))

       ((org-try-structure-completion))

       ((run-hook-with-args-until-success
	 'org-tab-before-tab-emulation-hook))

       ((and (eq org-cycle-emulate-tab 'exc-hl-bol)
	     (or (not (bolp))
		 (not (looking-at org-outline-regexp))))
	(call-interactively (global-key-binding "\t")))

       ((if (and (memq org-cycle-emulate-tab '(white whitestart))
		 (save-excursion (beginning-of-line 1) (looking-at "[ \t]*"))
		 (or (and (eq org-cycle-emulate-tab 'white)
			  (= (match-end 0) (point-at-eol)))
		     (and (eq org-cycle-emulate-tab 'whitestart)
			  (>= (match-end 0) pos))))
	    t
	  (eq org-cycle-emulate-tab t))
	(call-interactively (global-key-binding "\t")))

       (t (save-excursion
	    (org-back-to-heading)
	    (org-cycle)))))))

(defun org-cycle-internal-global ()
  "Do the global cycling action."
  ;; Hack to avoid display of messages for .org  attachments in Gnus
  (let ((ga (string-match "\\*fontification" (buffer-name))))
    (cond
     ((and (eq last-command this-command)
	   (eq org-cycle-global-status 'overview))
      ;; We just created the overview - now do table of contents
      ;; This can be slow in very large buffers, so indicate action
      (run-hook-with-args 'org-pre-cycle-hook 'contents)
      (unless ga (org-unlogged-message "CONTENTS..."))
      (org-content)
      (unless ga (org-unlogged-message "CONTENTS...done"))
      (setq org-cycle-global-status 'contents)
      (run-hook-with-args 'org-cycle-hook 'contents))

     ((and (eq last-command this-command)
	   (eq org-cycle-global-status 'contents))
      ;; We just showed the table of contents - now show everything
      (run-hook-with-args 'org-pre-cycle-hook 'all)
      (outline-show-all)
      (unless ga (org-unlogged-message "SHOW ALL"))
      (setq org-cycle-global-status 'all)
      (run-hook-with-args 'org-cycle-hook 'all))

     (t
      ;; Default action: go to overview
      (run-hook-with-args 'org-pre-cycle-hook 'overview)
      (org-overview)
      (unless ga (org-unlogged-message "OVERVIEW"))
      (setq org-cycle-global-status 'overview)
      (run-hook-with-args 'org-cycle-hook 'overview)))))

(defvar org-called-with-limited-levels nil
  "Non-nil when `org-with-limited-levels' is currently active.")

(defun org-invisible-p (&optional pos)
  "Non-nil if the character after POS is invisible.
If POS is nil, use `point' instead."
  (get-char-property (or pos (point)) 'invisible))

(defun org-cycle-internal-local ()
  "Do the local cycling action."
  (let ((goal-column 0) eoh eol eos has-children children-skipped struct)
    ;; First, determine end of headline (EOH), end of subtree or item
    ;; (EOS), and if item or heading has children (HAS-CHILDREN).
    (save-excursion
      (if (org-at-item-p)
	  (progn
	    (beginning-of-line)
	    (setq struct (org-list-struct))
	    (setq eoh (point-at-eol))
	    (setq eos (org-list-get-item-end-before-blank (point) struct))
	    (setq has-children (org-list-has-child-p (point) struct)))
	(org-back-to-heading)
	(setq eoh (save-excursion (outline-end-of-heading) (point)))
	(setq eos (save-excursion (org-end-of-subtree t t)
				  (when (bolp) (backward-char)) (point)))
	(setq has-children
	      (or (save-excursion
		    (let ((level (funcall outline-level)))
		      (outline-next-heading)
		      (and (org-at-heading-p t)
			   (> (funcall outline-level) level))))
		  (save-excursion
		    (org-list-search-forward (org-item-beginning-re) eos t)))))
      ;; Determine end invisible part of buffer (EOL)
      (beginning-of-line 2)
      (while (and (not (eobp)) ;This is like `next-line'.
		  (get-char-property (1- (point)) 'invisible))
	(goto-char (next-single-char-property-change (point) 'invisible))
	(and (eolp) (beginning-of-line 2)))
      (setq eol (point)))
    ;; Find out what to do next and set `this-command'
    (cond
     ((= eos eoh)
      ;; Nothing is hidden behind this heading
      (unless (org-before-first-heading-p)
	(run-hook-with-args 'org-pre-cycle-hook 'empty))
      (org-unlogged-message "EMPTY ENTRY")
      (setq org-cycle-subtree-status nil)
      (save-excursion
	(goto-char eos)
	(outline-next-heading)
	(when (org-invisible-p) (org-flag-heading nil))))
     ((and (or (>= eol eos)
	       (not (string-match "\\S-" (buffer-substring eol eos))))
	   (or has-children
	       (not (setq children-skipped
			  org-cycle-skip-children-state-if-no-children))))
      ;; Entire subtree is hidden in one line: children view
      (unless (org-before-first-heading-p)
	(run-hook-with-args 'org-pre-cycle-hook 'children))
      (if (org-at-item-p)
	  (org-list-set-item-visibility (point-at-bol) struct 'children)
	(org-show-entry)
	(org-with-limited-levels (org-show-children))
	;; FIXME: This slows down the func way too much.
	;; How keep drawers hidden in subtree anyway?
	;; (when (memq 'org-cycle-hide-drawers org-cycle-hook)
	;;   (org-cycle-hide-drawers 'subtree))

	;; Fold every list in subtree to top-level items.
	(when (eq org-cycle-include-plain-lists 'integrate)
	  (save-excursion
	    (org-back-to-heading)
	    (while (org-list-search-forward (org-item-beginning-re) eos t)
	      (beginning-of-line 1)
	      (let* ((struct (org-list-struct))
		     (prevs (org-list-prevs-alist struct))
		     (end (org-list-get-bottom-point struct)))
		(dolist (e (org-list-get-all-items (point) struct prevs))
		  (org-list-set-item-visibility e struct 'folded))
		(goto-char (if (< end eos) end eos)))))))
      (org-unlogged-message "CHILDREN")
      (save-excursion
	(goto-char eos)
	(outline-next-heading)
	(when (org-invisible-p) (org-flag-heading nil)))
      (setq org-cycle-subtree-status 'children)
      (unless (org-before-first-heading-p)
	(run-hook-with-args 'org-cycle-hook 'children)))
     ((or children-skipped
	  (and (eq last-command this-command)
	       (eq org-cycle-subtree-status 'children)))
      ;; We just showed the children, or no children are there,
      ;; now show everything.
      (unless (org-before-first-heading-p)
	(run-hook-with-args 'org-pre-cycle-hook 'subtree))
      (outline-flag-region eoh eos nil)
      (org-unlogged-message
       (if children-skipped "SUBTREE (NO CHILDREN)" "SUBTREE"))
      (setq org-cycle-subtree-status 'subtree)
      (unless (org-before-first-heading-p)
	(run-hook-with-args 'org-cycle-hook 'subtree)))
     (t
      ;; Default action: hide the subtree.
      (run-hook-with-args 'org-pre-cycle-hook 'folded)
      (outline-flag-region eoh eos t)
      (org-unlogged-message "FOLDED")
      (setq org-cycle-subtree-status 'folded)
      (unless (org-before-first-heading-p)
	(run-hook-with-args 'org-cycle-hook 'folded))))))

;;;###autoload
(defun org-global-cycle (&optional arg)
  "Cycle the global visibility.  For details see `org-cycle'.
With `\\[universal-argument]' prefix ARG, switch to startup visibility.
With a numeric prefix, show all headlines up to that level."
  (interactive "P")
  (let ((org-cycle-include-plain-lists
	 (if (derived-mode-p 'org-mode) org-cycle-include-plain-lists nil)))
    (cond
     ((integerp arg)
      (outline-show-all)
      (outline-hide-sublevels arg)
      (setq org-cycle-global-status 'contents))
     ((equal arg '(4))
      (org-set-startup-visibility)
      (org-unlogged-message "Startup visibility, plus VISIBILITY properties."))
     (t
      (org-cycle '(4))))))

(defun org-set-startup-visibility ()
  "Set the visibility required by startup options and properties."
  (cond
   ((eq org-startup-folded t)
    (org-overview))
   ((eq org-startup-folded 'content)
    (org-content))
   ((or (eq org-startup-folded 'showeverything)
	(eq org-startup-folded nil))
    (outline-show-all)))
  (unless (eq org-startup-folded 'showeverything)
    (when org-hide-block-startup (org-hide-block-all))
    (org-set-visibility-according-to-property 'no-cleanup)
    (org-cycle-hide-archived-subtrees 'all)
    (org-cycle-hide-drawers 'all)
    (org-cycle-show-empty-lines t)))

(defun org-set-visibility-according-to-property (&optional no-cleanup)
  "Switch subtree visibilities according to :VISIBILITY: property."
  (interactive)
  (org-with-wide-buffer
   (goto-char (point-min))
   (while (re-search-forward "^[ \t]*:VISIBILITY:" nil t)
     (if (not (org-at-property-p)) (outline-next-heading)
       (let ((state (match-string 3)))
	 (save-excursion
	   (org-back-to-heading t)
	   (outline-hide-subtree)
	   (org-reveal)
	   (cond
	    ((equal state "folded")
	     (outline-hide-subtree))
	    ((equal state "children")
	     (org-show-hidden-entry)
	     (org-show-children))
	    ((equal state "content")
	     (save-excursion
	       (save-restriction
		 (org-narrow-to-subtree)
		 (org-content))))
	    ((member state '("all" "showall"))
	     (outline-show-subtree)))))))
   (unless no-cleanup
     (org-cycle-hide-archived-subtrees 'all)
     (org-cycle-hide-drawers 'all)
     (org-cycle-show-empty-lines 'all))))

;; This function uses outline-regexp instead of the more fundamental
;; org-outline-regexp so that org-cycle-global works outside of Org
;; buffers, where outline-regexp is needed.
(defun org-overview ()
  "Switch to overview mode, showing only top-level headlines.
This shows all headlines with a level equal or greater than the level
of the first headline in the buffer.  This is important, because if the
first headline is not level one, then (hide-sublevels 1) gives confusing
results."
  (interactive)
  (save-excursion
    (let ((level
	   (save-excursion
	     (goto-char (point-min))
	     (when (re-search-forward (concat "^" outline-regexp) nil t)
	       (goto-char (match-beginning 0))
	       (funcall outline-level)))))
      (and level (outline-hide-sublevels level)))))

(defun org-content (&optional arg)
  "Show all headlines in the buffer, like a table of contents.
With numerical argument N, show content up to level N."
  (interactive "P")
  (org-overview)
  (save-excursion
    ;; Visit all headings and show their offspring
    (and (integerp arg) (org-overview))
    (goto-char (point-max))
    (catch 'exit
      (while (and (progn (condition-case nil
			     (outline-previous-visible-heading 1)
			   (error (goto-char (point-min))))
			 t)
		  (looking-at org-outline-regexp))
	(if (integerp arg)
	    (org-show-children (1- arg))
	  (outline-show-branches))
	(when (bobp) (throw 'exit nil))))))

(defun org-optimize-window-after-visibility-change (state)
  "Adjust the window after a change in outline visibility.
This function is the default value of the hook `org-cycle-hook'."
  (when (get-buffer-window (current-buffer))
    (cond
     ((eq state 'content)  nil)
     ((eq state 'all)      nil)
     ((eq state 'folded)   nil)
     ((eq state 'children) (or (org-subtree-end-visible-p) (recenter 1)))
     ((eq state 'subtree)  (or (org-subtree-end-visible-p) (recenter 1))))))

(defun org-remove-empty-overlays-at (pos)
  "Remove outline overlays that do not contain non-white stuff."
  (dolist (o (overlays-at pos))
    (and (eq 'outline (overlay-get o 'invisible))
	 (not (string-match "\\S-" (buffer-substring (overlay-start o)
						     (overlay-end o))))
	 (delete-overlay o))))

(defun org-clean-visibility-after-subtree-move ()
  "Fix visibility issues after moving a subtree."
  ;; First, find a reasonable region to look at:
  ;; Start two siblings above, end three below
  (let* ((beg (save-excursion
		(and (org-get-last-sibling)
		     (org-get-last-sibling))
		(point)))
	 (end (save-excursion
		(and (org-get-next-sibling)
		     (org-get-next-sibling)
		     (org-get-next-sibling))
		(if (org-at-heading-p)
		    (point-at-eol)
		  (point))))
	 (level (looking-at "\\*+"))
	 (re (when level (concat "^" (regexp-quote (match-string 0)) " "))))
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(when re
	  ;; Properly fold already folded siblings
	  (goto-char (point-min))
	  (while (re-search-forward re nil t)
	    (when (and (not (org-invisible-p))
		       (save-excursion
			 (goto-char (point-at-eol)) (org-invisible-p)))
	      (outline-hide-entry))))
	(org-cycle-show-empty-lines 'overview)
	(org-cycle-hide-drawers 'overview)))))

(defun org-cycle-show-empty-lines (state)
  "Show empty lines above all visible headlines.
The region to be covered depends on STATE when called through
`org-cycle-hook'.  Lisp program can use t for STATE to get the
entire buffer covered.  Note that an empty line is only shown if there
are at least `org-cycle-separator-lines' empty lines before the headline."
  (when (/= org-cycle-separator-lines 0)
    (save-excursion
      (let* ((n (abs org-cycle-separator-lines))
	     (re (cond
		  ((= n 1) "\\(\n[ \t]*\n\\*+\\) ")
		  ((= n 2) "^[ \t]*\\(\n[ \t]*\n\\*+\\) ")
		  (t (let ((ns (number-to-string (- n 2))))
		       (concat "^\\(?:[ \t]*\n\\)\\{" ns "," ns "\\}"
			       "[ \t]*\\(\n[ \t]*\n\\*+\\) ")))))
	     beg end)
	(cond
	 ((memq state '(overview contents t))
	  (setq beg (point-min) end (point-max)))
	 ((memq state '(children folded))
	  (setq beg (point)
		end (progn (org-end-of-subtree t t)
			   (line-beginning-position 2)))))
	(when beg
	  (goto-char beg)
	  (while (re-search-forward re end t)
	    (unless (get-char-property (match-end 1) 'invisible)
	      (let ((e (match-end 1))
		    (b (if (>= org-cycle-separator-lines 0)
			   (match-beginning 1)
			 (save-excursion
			   (goto-char (match-beginning 0))
			   (skip-chars-backward " \t\n")
			   (line-end-position)))))
		(outline-flag-region b e nil))))))))
  ;; Never hide empty lines at the end of the file.
  (save-excursion
    (goto-char (point-max))
    (outline-previous-heading)
    (outline-end-of-heading)
    (when (and (looking-at "[ \t\n]+")
	       (= (match-end 0) (point-max)))
      (outline-flag-region (point) (match-end 0) nil))))

(defun org-show-empty-lines-in-parent ()
  "Move to the parent and re-show empty lines before visible headlines."
  (save-excursion
    (let ((context (if (org-up-heading-safe) 'children 'overview)))
      (org-cycle-show-empty-lines context))))

(defun org-files-list ()
  "Return `org-agenda-files' list, plus all open Org files.
This is useful for operations that need to scan all of a user's
open and agenda-wise Org files."
  (let ((files (mapcar 'expand-file-name (org-agenda-files))))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
	(when (and (derived-mode-p 'org-mode) (buffer-file-name))
	  (cl-pushnew (expand-file-name (buffer-file-name)) files))))
    files))

(defsubst org-entry-beginning-position ()
  "Return the beginning position of the current entry."
  (save-excursion (org-back-to-heading t) (point)))

(defsubst org-entry-end-position ()
  "Return the end position of the current entry."
  (save-excursion (outline-next-heading) (point)))

(defun org-cycle-hide-drawers (state &optional exceptions)
  "Re-hide all drawers after a visibility state change.
STATE should be one of the symbols listed in the docstring of
`org-cycle-hook'.  When non-nil, optional argument EXCEPTIONS is
a list of strings specifying which drawers should not be hidden."
  (when (and (derived-mode-p 'org-mode)
	     (not (memq state '(overview folded contents))))
    (save-excursion
      (let* ((globalp (eq state 'all))
             (beg (if globalp (point-min) (point)))
             (end (if globalp (point-max)
		    (if (eq state 'children)
			(save-excursion (outline-next-heading) (point))
		      (org-end-of-subtree t)))))
	(goto-char beg)
	(while (re-search-forward org-drawer-regexp (max end (point)) t)
	  (unless (member-ignore-case (match-string 1) exceptions)
	    (let ((drawer (org-element-at-point)))
	      (when (memq (org-element-type drawer) '(drawer property-drawer))
		(org-flag-drawer t drawer)
		;; Make sure to skip drawer entirely or we might flag
		;; it another time when matching its ending line with
		;; `org-drawer-regexp'.
		(goto-char (org-element-property :end drawer))))))))))

(defun org-flag-drawer (flag &optional element)
  "When FLAG is non-nil, hide the drawer we are at.
Otherwise make it visible.  When optional argument ELEMENT is
a parsed drawer, as returned by `org-element-at-point', hide or
show that drawer instead."
  (let ((drawer (or element
		    (and (save-excursion
			   (beginning-of-line)
			   (looking-at-p org-drawer-regexp))
			 (org-element-at-point)))))
    (when (memq (org-element-type drawer) '(drawer property-drawer))
      (let ((post (org-element-property :post-affiliated drawer)))
	(save-excursion
	  (outline-flag-region
	   (progn (goto-char post) (line-end-position))
	   (progn (goto-char (org-element-property :end drawer))
		  (skip-chars-backward " \r\t\n")
		  (line-end-position))
	   flag))
	;; When the drawer is hidden away, make sure point lies in
	;; a visible part of the buffer.
	(when (and flag (> (line-beginning-position) post))
	  (goto-char post))))))

(defun org-subtree-end-visible-p ()
  "Is the end of the current subtree visible?"
  (pos-visible-in-window-p
   (save-excursion (org-end-of-subtree t) (point))))

(defun org-first-headline-recenter ()
  "Move cursor to the first headline and recenter the headline."
  (let ((window (get-buffer-window)))
    (when window
      (goto-char (point-min))
      (when (re-search-forward (concat "^\\(" org-outline-regexp "\\)") nil t)
	(set-window-start window (line-beginning-position))))))

;;; Saving and restoring visibility

(defun org-outline-overlay-data (&optional use-markers)
  "Return a list of the locations of all outline overlays.
These are overlays with the `invisible' property value `outline'.
The return value is a list of cons cells, with start and stop
positions for each overlay.
If USE-MARKERS is set, return the positions as markers."
  (let (beg end)
    (org-with-wide-buffer
     (delq nil
	   (mapcar (lambda (o)
		     (when (eq (overlay-get o 'invisible) 'outline)
		       (setq beg (overlay-start o)
			     end (overlay-end o))
		       (and beg end (> end beg)
			    (if use-markers
				(cons (copy-marker beg)
				      (copy-marker end t))
			      (cons beg end)))))
		   (overlays-in (point-min) (point-max)))))))

(defun org-set-outline-overlay-data (data)
  "Create visibility overlays for all positions in DATA.
DATA should have been made by `org-outline-overlay-data'."
  (org-with-wide-buffer
   (outline-show-all)
   (dolist (c data) (outline-flag-region (car c) (cdr c) t))))

;;; Folding of blocks

(defvar-local org-hide-block-overlays nil
  "Overlays hiding blocks.")

(defun org-block-map (function &optional start end)
  "Call FUNCTION at the head of all source blocks in the current buffer.
Optional arguments START and END can be used to limit the range."
  (let ((start (or start (point-min)))
        (end (or end (point-max))))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end) (re-search-forward org-block-regexp end t))
	(save-excursion
	  (save-match-data
            (goto-char (match-beginning 0))
            (funcall function)))))))

(defun org-hide-block-toggle-all ()
  "Toggle the visibility of all blocks in the current buffer."
  (org-block-map 'org-hide-block-toggle))

(defun org-hide-block-all ()
  "Fold all blocks in the current buffer."
  (interactive)
  (org-show-block-all)
  (org-block-map 'org-hide-block-toggle-maybe))

(defun org-show-block-all ()
  "Unfold all blocks in the current buffer."
  (interactive)
  (mapc #'delete-overlay org-hide-block-overlays)
  (setq org-hide-block-overlays nil))

(defun org-hide-block-toggle-maybe ()
  "Toggle visibility of block at point.
Unlike to `org-hide-block-toggle', this function does not throw
an error.  Return a non-nil value when toggling is successful."
  (interactive)
  (ignore-errors (org-hide-block-toggle)))

(defun org-hide-block-toggle (&optional force)
  "Toggle the visibility of the current block.
When optional argument FORCE is `off', make block visible.  If it
is non-nil, hide it unconditionally.  Throw an error when not at
a block.  Return a non-nil value when toggling is successful."
  (interactive)
  (let ((element (org-element-at-point)))
    (unless (memq (org-element-type element)
		  '(center-block comment-block dynamic-block example-block
				 export-block quote-block special-block
				 src-block verse-block))
      (user-error "Not at a block"))
    (let* ((start (save-excursion
		    (goto-char (org-element-property :post-affiliated element))
		    (line-end-position)))
	   (end (save-excursion
		  (goto-char (org-element-property :end element))
		  (skip-chars-backward " \r\t\n")
		  (line-end-position)))
	   (overlays (overlays-at start)))
      (cond
       ;; Do nothing when not before or at the block opening line or
       ;; at the block closing line.
       ((let ((eol (line-end-position))) (and (> eol start) (/= eol end))) nil)
       ((and (not (eq force 'off))
	     (not (memq t (mapcar
			   (lambda (o)
			     (eq (overlay-get o 'invisible) 'org-hide-block))
			   overlays))))
	(let ((ov (make-overlay start end)))
	  (overlay-put ov 'invisible 'org-hide-block)
	  ;; Make the block accessible to `isearch'.
	  (overlay-put
	   ov 'isearch-open-invisible
	   (lambda (ov)
	     (when (memq ov org-hide-block-overlays)
	       (setq org-hide-block-overlays (delq ov org-hide-block-overlays)))
	     (when (eq (overlay-get ov 'invisible) 'org-hide-block)
	       (delete-overlay ov))))
	  (push ov org-hide-block-overlays)
	  ;; When the block is hidden away, make sure point is left in
	  ;; a visible part of the buffer.
	  (when (> (line-beginning-position) start)
	    (goto-char start)
	    (beginning-of-line))
	  ;; Signal successful toggling.
	  t))
       ((or (not force) (eq force 'off))
	(dolist (ov overlays t)
	  (when (memq ov org-hide-block-overlays)
	    (setq org-hide-block-overlays (delq ov org-hide-block-overlays)))
	  (when (eq (overlay-get ov 'invisible) 'org-hide-block)
	    (delete-overlay ov))))))))

;; Remove overlays when changing major mode
(add-hook 'org-mode-hook
	  (lambda () (add-hook 'change-major-mode-hook
			       'org-show-block-all 'append 'local)))

;;; Org-goto

(defvar org-goto-window-configuration nil)
(defvar org-goto-marker nil)
(defvar org-goto-map)
(defun org-goto-map ()
  "Set the keymap `org-goto'."
  (setq org-goto-map
	(let ((map (make-sparse-keymap)))
	  (let ((cmds '(isearch-forward isearch-backward kill-ring-save set-mark-command
					mouse-drag-region universal-argument org-occur)))
	    (dolist (cmd cmds)
	      (substitute-key-definition cmd cmd map global-map)))
	  (suppress-keymap map)
	  (org-defkey map "\C-m"     'org-goto-ret)
	  (org-defkey map [(return)] 'org-goto-ret)
	  (org-defkey map [(left)]   'org-goto-left)
	  (org-defkey map [(right)]  'org-goto-right)
	  (org-defkey map [(control ?g)] 'org-goto-quit)
	  (org-defkey map "\C-i" 'org-cycle)
	  (org-defkey map [(tab)] 'org-cycle)
	  (org-defkey map [(down)] 'outline-next-visible-heading)
	  (org-defkey map [(up)] 'outline-previous-visible-heading)
	  (if org-goto-auto-isearch
	      (if (fboundp 'define-key-after)
		  (define-key-after map [t] 'org-goto-local-auto-isearch)
		nil)
	    (org-defkey map "q" 'org-goto-quit)
	    (org-defkey map "n" 'outline-next-visible-heading)
	    (org-defkey map "p" 'outline-previous-visible-heading)
	    (org-defkey map "f" 'outline-forward-same-level)
	    (org-defkey map "b" 'outline-backward-same-level)
	    (org-defkey map "u" 'outline-up-heading))
	  (org-defkey map "/" 'org-occur)
	  (org-defkey map "\C-c\C-n" 'outline-next-visible-heading)
	  (org-defkey map "\C-c\C-p" 'outline-previous-visible-heading)
	  (org-defkey map "\C-c\C-f" 'outline-forward-same-level)
	  (org-defkey map "\C-c\C-b" 'outline-backward-same-level)
	  (org-defkey map "\C-c\C-u" 'outline-up-heading)
	  map)))

(defconst org-goto-help
  "Browse buffer copy, to find location or copy text.%s
RET=jump to location             C-g=quit and return to previous location
\[Up]/[Down]=next/prev headline   TAB=cycle visibility   [/] org-occur")

(defvar org-goto-start-pos) ; dynamically scoped parameter

(defun org-goto (&optional alternative-interface)
  "Look up a different location in the current file, keeping current visibility.

When you want look-up or go to a different location in a
document, the fastest way is often to fold the entire buffer and
then dive into the tree.  This method has the disadvantage, that
the previous location will be folded, which may not be what you
want.

This command works around this by showing a copy of the current
buffer in an indirect buffer, in overview mode.  You can dive
into the tree in that copy, use org-occur and incremental search
to find a location.  When pressing RET or `Q', the command
returns to the original buffer in which the visibility is still
unchanged.  After RET it will also jump to the location selected
in the indirect buffer and expose the headline hierarchy above.

With a prefix argument, use the alternative interface: e.g., if
`org-goto-interface' is `outline' use `outline-path-completion'."
  (interactive "P")
  (org-goto-map)
  (let* ((org-refile-targets `((nil . (:maxlevel . ,org-goto-max-level))))
	 (org-refile-use-outline-path t)
	 (org-refile-target-verify-function nil)
	 (interface
	  (if (not alternative-interface)
	      org-goto-interface
	    (if (eq org-goto-interface 'outline)
		'outline-path-completion
	      'outline)))
	 (org-goto-start-pos (point))
	 (selected-point
	  (if (eq interface 'outline)
	      (car (org-get-location (current-buffer) org-goto-help))
	    (let ((pa (org-refile-get-location "Goto")))
	      (org-refile-check-position pa)
	      (nth 3 pa)))))
    (if selected-point
	(progn
	  (org-mark-ring-push org-goto-start-pos)
	  (goto-char selected-point)
	  (when (or (org-invisible-p) (org-invisible-p2))
	    (org-show-context 'org-goto)))
      (message "Quit"))))

(defvar org-goto-selected-point nil) ; dynamically scoped parameter
(defvar org-goto-exit-command nil) ; dynamically scoped parameter
(defvar org-goto-local-auto-isearch-map) ; defined below

(defun org-get-location (_buf help)
  "Let the user select a location in current buffer.
This function uses a recursive edit.  It returns the selected position
or nil."
  (org-no-popups
   (let ((isearch-mode-map org-goto-local-auto-isearch-map)
	 (isearch-hide-immediately nil)
	 (isearch-search-fun-function
	  (lambda () 'org-goto-local-search-headings))
	 (org-goto-selected-point org-goto-exit-command))
     (save-excursion
       (save-window-excursion
	 (delete-other-windows)
	 (and (get-buffer "*org-goto*") (kill-buffer "*org-goto*"))
	 (pop-to-buffer-same-window
	  (condition-case nil
	      (make-indirect-buffer (current-buffer) "*org-goto*")
	    (error (make-indirect-buffer (current-buffer) "*org-goto*"))))
	 (with-output-to-temp-buffer "*Org Help*"
	   (princ (format help (if org-goto-auto-isearch
				   "  Just type for auto-isearch."
				 "  n/p/f/b/u to navigate, q to quit."))))
	 (org-fit-window-to-buffer (get-buffer-window "*Org Help*"))
	 (setq buffer-read-only nil)
	 (let ((org-startup-truncated t)
	       (org-startup-folded nil)
	       (org-startup-align-all-tables nil))
	   (org-mode)
	   (org-overview))
	 (setq buffer-read-only t)
	 (if (and (boundp 'org-goto-start-pos)
		  (integer-or-marker-p org-goto-start-pos))
	     (progn (goto-char org-goto-start-pos)
		    (when (org-invisible-p)
		      (org-show-set-visibility 'lineage)))
	   (goto-char (point-min)))
	 (let (org-special-ctrl-a/e) (org-beginning-of-line))
	 (message "Select location and press RET")
	 (use-local-map org-goto-map)
	 (recursive-edit)))
     (kill-buffer "*org-goto*")
     (cons org-goto-selected-point org-goto-exit-command))))

(defvar org-goto-local-auto-isearch-map (make-sparse-keymap))
(set-keymap-parent org-goto-local-auto-isearch-map isearch-mode-map)
;; `isearch-other-control-char' was removed in Emacs 24.4.
(if (fboundp 'isearch-other-control-char)
    (progn
      (define-key org-goto-local-auto-isearch-map "\C-i" 'isearch-other-control-char)
      (define-key org-goto-local-auto-isearch-map "\C-m" 'isearch-other-control-char))
  (define-key org-goto-local-auto-isearch-map "\C-i" nil)
  (define-key org-goto-local-auto-isearch-map "\C-m" nil)
  (define-key org-goto-local-auto-isearch-map [return] nil))

(defun org-goto-local-search-headings (string bound noerror)
  "Search and make sure that any matches are in headlines."
  (catch 'return
    (while (if isearch-forward
               (search-forward string bound noerror)
             (search-backward string bound noerror))
      (when (save-match-data
	      (and (save-excursion
		     (beginning-of-line)
		     (looking-at org-complex-heading-regexp))
		   (or (not (match-beginning 5))
		       (< (point) (match-beginning 5)))))
	(throw 'return (point))))))

(defun org-goto-local-auto-isearch ()
  "Start isearch."
  (interactive)
  (goto-char (point-min))
  (let ((keys (this-command-keys)))
    (when (eq (lookup-key isearch-mode-map keys) 'isearch-printing-char)
      (isearch-mode t)
      (isearch-process-search-char (string-to-char keys)))))

(defun org-goto-ret (&optional _arg)
  "Finish `org-goto' by going to the new location."
  (interactive "P")
  (setq org-goto-selected-point (point))
  (setq org-goto-exit-command 'return)
  (throw 'exit nil))

(defun org-goto-left ()
  "Finish `org-goto' by going to the new location."
  (interactive)
  (if (org-at-heading-p)
      (progn
	(beginning-of-line 1)
	(setq org-goto-selected-point (point)
	      org-goto-exit-command 'left)
	(throw 'exit nil))
    (user-error "Not on a heading")))

(defun org-goto-right ()
  "Finish `org-goto' by going to the new location."
  (interactive)
  (if (org-at-heading-p)
      (progn
	(setq org-goto-selected-point (point)
	      org-goto-exit-command 'right)
	(throw 'exit nil))
    (user-error "Not on a heading")))

(defun org-goto-quit ()
  "Finish `org-goto' without cursor motion."
  (interactive)
  (setq org-goto-selected-point nil)
  (setq org-goto-exit-command 'quit)
  (throw 'exit nil))

;;; Indirect buffer display of subtrees

(defvar org-indirect-dedicated-frame nil
  "This is the frame being used for indirect tree display.")
(defvar org-last-indirect-buffer nil)

(defun org-tree-to-indirect-buffer (&optional arg)
  "Create indirect buffer and narrow it to current subtree.

With a numerical prefix ARG, go up to this level and then take that tree.
If ARG is negative, go up that many levels.

If `org-indirect-buffer-display' is not `new-frame', the command removes the
indirect buffer previously made with this command, to avoid proliferation of
indirect buffers.  However, when you call the command with a \
`\\[universal-argument]' prefix, or
when `org-indirect-buffer-display' is `new-frame', the last buffer is kept
so that you can work with several indirect buffers at the same time.  If
`org-indirect-buffer-display' is `dedicated-frame', the \
`\\[universal-argument]' prefix also
requests that a new frame be made for the new buffer, so that the dedicated
frame is not changed."
  (interactive "P")
  (let ((cbuf (current-buffer))
	(cwin (selected-window))
	(pos (point))
	beg end level heading ibuf)
    (save-excursion
      (org-back-to-heading t)
      (when (numberp arg)
	(setq level (org-outline-level))
	(when (< arg 0) (setq arg (+ level arg)))
	(while (> (setq level (org-outline-level)) arg)
	  (org-up-heading-safe)))
      (setq beg (point)
	    heading (org-get-heading 'no-tags))
      (org-end-of-subtree t t)
      (when (org-at-heading-p) (backward-char 1))
      (setq end (point)))
    (when (and (buffer-live-p org-last-indirect-buffer)
	       (not (eq org-indirect-buffer-display 'new-frame))
	       (not arg))
      (kill-buffer org-last-indirect-buffer))
    (setq ibuf (org-get-indirect-buffer cbuf heading)
	  org-last-indirect-buffer ibuf)
    (cond
     ((or (eq org-indirect-buffer-display 'new-frame)
	  (and arg (eq org-indirect-buffer-display 'dedicated-frame)))
      (select-frame (make-frame))
      (delete-other-windows)
      (pop-to-buffer-same-window ibuf)
      (org-set-frame-title heading))
     ((eq org-indirect-buffer-display 'dedicated-frame)
      (raise-frame
       (select-frame (or (and org-indirect-dedicated-frame
			      (frame-live-p org-indirect-dedicated-frame)
			      org-indirect-dedicated-frame)
			 (setq org-indirect-dedicated-frame (make-frame)))))
      (delete-other-windows)
      (pop-to-buffer-same-window ibuf)
      (org-set-frame-title (concat "Indirect: " heading)))
     ((eq org-indirect-buffer-display 'current-window)
      (pop-to-buffer-same-window ibuf))
     ((eq org-indirect-buffer-display 'other-window)
      (pop-to-buffer ibuf))
     (t (error "Invalid value")))
    (narrow-to-region beg end)
    (outline-show-all)
    (goto-char pos)
    (run-hook-with-args 'org-cycle-hook 'all)
    (and (window-live-p cwin) (select-window cwin))))

(defun org-get-indirect-buffer (&optional buffer heading)
  (setq buffer (or buffer (current-buffer)))
  (let ((n 1) (base (buffer-name buffer)) bname)
    (while (buffer-live-p
	    (get-buffer
	     (setq bname
		   (concat base "-"
			   (if heading (concat heading "-" (number-to-string n))
			     (number-to-string n))))))
      (setq n (1+ n)))
    (condition-case nil
        (make-indirect-buffer buffer bname 'clone)
      (error (make-indirect-buffer buffer bname)))))

(defun org-set-frame-title (title)
  "Set the title of the current frame to the string TITLE."
  (modify-frame-parameters (selected-frame) (list (cons 'name title))))

;;;; Structure editing

;;; Inserting headlines

(defun org--line-empty-p (n)
  "Is the Nth next line empty?

Counts the current line as N = 1 and the previous line as N = 0;
see `beginning-of-line'."
  (save-excursion
    (and (not (bobp))
	 (or (beginning-of-line n) t)
	 (save-match-data
	   (looking-at "[ \t]*$")))))

(defun org-previous-line-empty-p ()
  "Is the previous line a blank line?
When NEXT is non-nil, check the next line instead."
  (org--line-empty-p 0))

(defun org-next-line-empty-p ()
  "Is the previous line a blank line?
When NEXT is non-nil, check the next line instead."
  (org--line-empty-p 2))

(defun org--blank-before-heading-p (&optional parent)
  "Non-nil when an empty line should precede a new heading here.
When optional argument PARENT is non-nil, consider parent
headline instead of current one."
  (pcase (assq 'heading org-blank-before-new-entry)
    (`(heading . auto)
     (save-excursion
       (org-with-limited-levels
        (unless (and (org-before-first-heading-p)
                     (not (outline-next-heading)))
          (org-back-to-heading t)
          (when parent (org-up-heading-safe))
          (cond ((not (bobp))
                 (org-previous-line-empty-p))
		((outline-next-heading)
		 (org-previous-line-empty-p))
		;; Ignore trailing spaces on last buffer line.
		((progn (skip-chars-backward " \t") (bolp))
		 (org-previous-line-empty-p))
		(t nil))))))
    (`(heading . ,value) value)
    (_ nil)))

(defun org-insert-heading (&optional arg invisible-ok top)
  "Insert a new heading or an item with the same depth at point.

If point is at the beginning of a heading, insert a new heading
or a new headline above the current one.  When at the beginning
of a regular line of text, turn it into a heading.

If point is in the middle of a line, split it and create a new
headline with the text in the current line after point (see
`org-M-RET-may-split-line' on how to modify this behavior).  As
a special case, on a headline, splitting can only happen on the
title itself.  E.g., this excludes breaking stars or tags.

With a `\\[universal-argument]' prefix, set \
`org-insert-heading-respect-content' to
a non-nil value for the duration of the command.  This forces the
insertion of a heading after the current subtree, independently
on the location of point.

With a `\\[universal-argument] \\[universal-argument]' prefix, \
insert the heading at the end of the tree
above the current heading.  For example, if point is within a
2nd-level heading, then it will insert a 2nd-level heading at
the end of the 1st-level parent subtree.

When INVISIBLE-OK is set, stop at invisible headlines when going
back.  This is important for non-interactive uses of the
command.

When optional argument TOP is non-nil, insert a level 1 heading,
unconditionally."
  (interactive "P")
  (let* ((blank? (org--blank-before-heading-p (equal arg '(16))))
	 (level (org-current-level))
	 (stars (make-string (if (and level (not top)) level 1) ?*)))
    (cond
     ((or org-insert-heading-respect-content
	  (member arg '((4) (16)))
	  (and (not invisible-ok)
	       (invisible-p (max (1- (point)) (point-min)))))
      ;; Position point at the location of insertion.
      (if (not level)			;before first headline
	  (org-with-limited-levels (outline-next-heading))
	;; Make sure we end up on a visible headline if INVISIBLE-OK
	;; is nil.
	(org-with-limited-levels (org-back-to-heading invisible-ok))
	(cond ((equal arg '(16))
	       (org-up-heading-safe)
	       (org-end-of-subtree t t))
	      (t
	       (org-end-of-subtree t t))))
      (unless (bolp) (insert "\n"))   ;ensure final newline
      (unless (and blank? (org-previous-line-empty-p))
	(org-N-empty-lines-before-current (if blank? 1 0)))
      (insert stars " \n")
      (forward-char -1))
     ;; At a headline...
     ((org-at-heading-p)
      (cond ((bolp)
	     (when blank? (save-excursion (insert "\n")))
	     (save-excursion (insert stars " \n"))
	     (unless (and blank? (org-previous-line-empty-p))
	       (org-N-empty-lines-before-current (if blank? 1 0)))
	     (end-of-line))
	    ((and (org-get-alist-option org-M-RET-may-split-line 'headline)
		  (org-match-line org-complex-heading-regexp)
		  (org-pos-in-match-range (point) 4))
	     ;; Grab the text that should moved to the new headline.
	     ;; Preserve tags.
	     (let ((split (delete-and-extract-region (point) (match-end 4))))
	       (if (looking-at "[ \t]*$") (replace-match "")
		 (org-set-tags nil t))
	       (end-of-line)
	       (when blank? (insert "\n"))
	       (insert "\n" stars " ")
	       (when (org-string-nw-p split) (insert split))
	       (when (eobp) (save-excursion (insert "\n")))))
	    (t
	     (end-of-line)
	     (when blank? (insert "\n"))
	     (insert "\n" stars " ")
	     (when (eobp) (save-excursion (insert "\n"))))))
     ;; On regular text, turn line into a headline or split, if
     ;; appropriate.
     ((bolp)
      (insert stars " ")
      (unless (and blank? (org-previous-line-empty-p))
        (org-N-empty-lines-before-current (if blank? 1 0))))
     (t
      (unless (org-get-alist-option org-M-RET-may-split-line 'headline)
        (end-of-line))
      (insert "\n" stars " ")
      (unless (and blank? (org-previous-line-empty-p))
        (org-N-empty-lines-before-current (if blank? 1 0))))))
  (run-hooks 'org-insert-heading-hook))

(defun org-N-empty-lines-before-current (n)
  "Make the number of empty lines before current exactly N.
So this will delete or add empty lines."
  (let ((column (current-column)))
    (beginning-of-line)
    (unless (bobp)
      (let ((start (save-excursion
		     (skip-chars-backward " \r\t\n")
		     (line-end-position))))
	(delete-region start (line-end-position 0))))
    (insert (make-string n ?\n))
    (move-to-column column)))

(defun org-get-heading (&optional no-tags no-todo no-priority no-comment)
  "Return the heading of the current entry, without the stars.
When NO-TAGS is non-nil, don't include tags.
When NO-TODO is non-nil, don't include TODO keywords.
When NO-PRIORITY is non-nil, don't include priority cookie.
When NO-COMMENT is non-nil, don't include COMMENT string."
  (save-excursion
    (org-back-to-heading t)
    (let ((case-fold-search nil))
      (looking-at org-complex-heading-regexp)
      (let ((todo (and (not no-todo) (match-string 2)))
	    (priority (and (not no-priority) (match-string 3)))
	    (headline (pcase (match-string 4)
			(`nil "")
			((and (guard no-comment) h)
			 (replace-regexp-in-string
			  (eval-when-compile
			    (format "\\`%s[ \t]+" org-comment-string))
			  "" h))
			(h h)))
	    (tags (and (not no-tags) (match-string 5))))
	(mapconcat #'identity
		   (delq nil (list todo priority headline tags))
		   " ")))))

(defvar orgstruct-mode)   ; defined below

(defun org-heading-components ()
  "Return the components of the current heading.
This is a list with the following elements:
- the level as an integer
- the reduced level, different if `org-odd-levels-only' is set.
- the TODO keyword, or nil
- the priority character, like ?A, or nil if no priority is given
- the headline text itself, or the tags string if no headline text
- the tags string, or nil."
  (save-excursion
    (org-back-to-heading t)
    (when (let (case-fold-search)
	    (looking-at
	     (if orgstruct-mode
		 org-heading-regexp
	       org-complex-heading-regexp)))
      (if orgstruct-mode
	  (list (length (match-string 1))
		(org-reduced-level (length (match-string 1)))
		nil
		nil
		(match-string 2)
		nil)
	(list (length (match-string 1))
	      (org-reduced-level (length (match-string 1)))
	      (match-string-no-properties 2)
	      (and (match-end 3) (aref (match-string 3) 2))
	      (match-string-no-properties 4)
	      (match-string-no-properties 5))))))

(defun org-get-entry ()
  "Get the entry text, after heading, entire subtree."
  (save-excursion
    (org-back-to-heading t)
    (buffer-substring (point-at-bol 2) (org-end-of-subtree t))))

(defun org-edit-headline (&optional heading)
  "Edit the current headline.
Set it to HEADING when provided."
  (interactive)
  (org-with-wide-buffer
   (org-back-to-heading t)
   (let ((case-fold-search nil))
     (when (looking-at org-complex-heading-regexp)
       (let* ((old (match-string-no-properties 4))
	      (new (save-match-data
		     (org-trim (or heading (read-string "Edit: " old))))))
	 (unless (equal old new)
	   (if old (replace-match new t t nil 4)
	     (goto-char (or (match-end 3) (match-end 2) (match-end 1)))
	     (insert " " new))
	   (org-set-tags nil t)
	   (when (looking-at "[ \t]*$") (replace-match ""))))))))

(defun org-insert-heading-after-current ()
  "Insert a new heading with same level as current, after current subtree."
  (interactive)
  (org-back-to-heading)
  (org-insert-heading)
  (org-move-subtree-down)
  (end-of-line 1))

(defun org-insert-heading-respect-content (&optional invisible-ok)
  "Insert heading with `org-insert-heading-respect-content' set to t."
  (interactive)
  (org-insert-heading '(4) invisible-ok))

(defun org-insert-todo-heading-respect-content (&optional force-state)
  "Insert TODO heading with `org-insert-heading-respect-content' set to t."
  (interactive)
  (org-insert-todo-heading force-state '(4)))

(defun org-insert-todo-heading (arg &optional force-heading)
  "Insert a new heading with the same level and TODO state as current heading.

If the heading has no TODO state, or if the state is DONE, use
the first state (TODO by default).  Also with one prefix arg,
force first state.  With two prefix args, force inserting at the
end of the parent subtree.

When called at a plain list item, insert a new item with an
unchecked check box."
  (interactive "P")
  (when (or force-heading (not (org-insert-item 'checkbox)))
    (org-insert-heading (or (and (equal arg '(16)) '(16))
			    force-heading))
    (save-excursion
      (org-forward-heading-same-level -1)
      (let ((case-fold-search nil)) (looking-at org-todo-line-regexp)))
    (let* ((new-mark-x
	    (if (or (equal arg '(4))
		    (not (match-beginning 2))
		    (member (match-string 2) org-done-keywords))
		(car org-todo-keywords-1)
	      (match-string 2)))
	   (new-mark
	    (or
	     (run-hook-with-args-until-success
	      'org-todo-get-default-hook new-mark-x nil)
	     new-mark-x)))
      (beginning-of-line 1)
      (and (looking-at org-outline-regexp) (goto-char (match-end 0))
	   (if org-treat-insert-todo-heading-as-state-change
	       (org-todo new-mark)
	     (insert new-mark " "))))
    (when org-provide-todo-statistics
      (org-update-parent-todo-statistics))))

(defun org-insert-subheading (arg)
  "Insert a new subheading and demote it.
Works for outline headings and for plain lists alike."
  (interactive "P")
  (org-insert-heading arg)
  (cond
   ((org-at-heading-p) (org-do-demote))
   ((org-at-item-p) (org-indent-item))))

(defun org-insert-todo-subheading (arg)
  "Insert a new subheading with TODO keyword or checkbox and demote it.
Works for outline headings and for plain lists alike."
  (interactive "P")
  (org-insert-todo-heading arg)
  (cond
   ((org-at-heading-p) (org-do-demote))
   ((org-at-item-p) (org-indent-item))))

;;; Promotion and Demotion

(defvar org-after-demote-entry-hook nil
  "Hook run after an entry has been demoted.
The cursor will be at the beginning of the entry.
When a subtree is being demoted, the hook will be called for each node.")

(defvar org-after-promote-entry-hook nil
  "Hook run after an entry has been promoted.
The cursor will be at the beginning of the entry.
When a subtree is being promoted, the hook will be called for each node.")

(defun org-promote-subtree ()
  "Promote the entire subtree.
See also `org-promote'."
  (interactive)
  (save-excursion
    (org-with-limited-levels (org-map-tree 'org-promote)))
  (org-fix-position-after-promote))

(defun org-demote-subtree ()
  "Demote the entire subtree.
See `org-demote' and `org-promote'."
  (interactive)
  (save-excursion
    (org-with-limited-levels (org-map-tree 'org-demote)))
  (org-fix-position-after-promote))

(defun org-do-promote ()
  "Promote the current heading higher up the tree.
If the region is active in `transient-mark-mode', promote all
headings in the region."
  (interactive)
  (save-excursion
    (if (org-region-active-p)
	(org-map-region 'org-promote (region-beginning) (region-end))
      (org-promote)))
  (org-fix-position-after-promote))

(defun org-do-demote ()
  "Demote the current heading lower down the tree.
If the region is active in `transient-mark-mode', demote all
headings in the region."
  (interactive)
  (save-excursion
    (if (org-region-active-p)
	(org-map-region 'org-demote (region-beginning) (region-end))
      (org-demote)))
  (org-fix-position-after-promote))

(defun org-fix-position-after-promote ()
  "Fix cursor position and indentation after demoting/promoting."
  (let ((pos (point)))
    (when (save-excursion
	    (beginning-of-line)
	    (let ((case-fold-search nil)) (looking-at org-todo-line-regexp))
	    (or (eq pos (match-end 1)) (eq pos (match-end 2))))
      (cond ((eobp) (insert " "))
	    ((eolp) (insert " "))
	    ((equal (char-after) ?\s) (forward-char 1))))))

(defun org-current-level ()
  "Return the level of the current entry, or nil if before the first headline.
The level is the number of stars at the beginning of the
headline.  Use `org-reduced-level' to remove the effect of
`org-odd-levels'.  Unlike to `org-outline-level', this function
ignores inlinetasks."
  (let ((level (org-with-limited-levels (org-outline-level))))
    (and (> level 0) level)))

(defun org-get-previous-line-level ()
  "Return the outline depth of the last headline before the current line.
Returns 0 for the first headline in the buffer, and nil if before the
first headline."
  (and (org-current-level)
       (or (and (/= (line-beginning-position) (point-min))
		(save-excursion (beginning-of-line 0) (org-current-level)))
	   0)))

(defun org-reduced-level (l)
  "Compute the effective level of a heading.
This takes into account the setting of `org-odd-levels-only'."
  (cond
   ((zerop l) 0)
   (org-odd-levels-only (1+ (floor (/ l 2))))
   (t l)))

(defun org-level-increment ()
  "Return the number of stars that will be added or removed at a
time to headlines when structure editing, based on the value of
`org-odd-levels-only'."
  (if org-odd-levels-only 2 1))

(defun org-get-valid-level (level &optional change)
  "Rectify a level change under the influence of `org-odd-levels-only'.
LEVEL is a current level, CHANGE is by how much the level should
be modified.  Even if CHANGE is nil, LEVEL may be returned
modified because even level numbers will become the next higher
odd number.  Returns values greater than 0."
  (if org-odd-levels-only
      (cond ((or (not change) (= 0 change)) (1+ (* 2 (/ level 2))))
	    ((> change 0) (1+ (* 2 (/ (+ (1- level) (* 2 change)) 2))))
	    ((< change 0) (max 1 (1+ (* 2 (/ (+ level (* 2 change)) 2))))))
    (max 1 (+ level (or change 0)))))

(defun org-promote ()
  "Promote the current heading higher up the tree."
  (org-with-wide-buffer
   (org-back-to-heading t)
   (let* ((after-change-functions (remq 'flyspell-after-change-function
					after-change-functions))
	  (level (save-match-data (funcall outline-level)))
	  (up-head (concat (make-string (org-get-valid-level level -1) ?*) " "))
	  (diff (abs (- level (length up-head) -1))))
     (cond
      ((and (= level 1) org-allow-promoting-top-level-subtree)
       (replace-match "# " nil t))
      ((= level 1)
       (user-error "Cannot promote to level 0.  UNDO to recover if necessary"))
      (t (replace-match up-head nil t)))
     (unless (= level 1)
       (when org-auto-align-tags (org-set-tags nil 'ignore-column))
       (when org-adapt-indentation (org-fixup-indentation (- diff))))
     (run-hooks 'org-after-promote-entry-hook))))

(defun org-demote ()
  "Demote the current heading lower down the tree."
  (org-with-wide-buffer
   (org-back-to-heading t)
   (let* ((after-change-functions (remq 'flyspell-after-change-function
					after-change-functions))
	  (level (save-match-data (funcall outline-level)))
	  (down-head (concat (make-string (org-get-valid-level level 1) ?*) " "))
	  (diff (abs (- level (length down-head) -1))))
     (replace-match down-head nil t)
     (when org-auto-align-tags (org-set-tags nil 'ignore-column))
     (when org-adapt-indentation (org-fixup-indentation diff))
     (run-hooks 'org-after-demote-entry-hook))))

(defun org-cycle-level ()
  "Cycle the level of an empty headline through possible states.
This goes first to child, then to parent, level, then up the hierarchy.
After top level, it switches back to sibling level."
  (interactive)
  (let ((org-adapt-indentation nil))
    (when (org-point-at-end-of-empty-headline)
      (setq this-command 'org-cycle-level) ; Only needed for caching
      (let ((cur-level (org-current-level))
            (prev-level (org-get-previous-line-level)))
        (cond
         ;; If first headline in file, promote to top-level.
         ((= prev-level 0)
          (cl-loop repeat (/ (- cur-level 1) (org-level-increment))
		   do (org-do-promote)))
         ;; If same level as prev, demote one.
         ((= prev-level cur-level)
          (org-do-demote))
         ;; If parent is top-level, promote to top level if not already.
         ((= prev-level 1)
          (cl-loop repeat (/ (- cur-level 1) (org-level-increment))
		   do (org-do-promote)))
         ;; If top-level, return to prev-level.
         ((= cur-level 1)
          (cl-loop repeat (/ (- prev-level 1) (org-level-increment))
		   do (org-do-demote)))
         ;; If less than prev-level, promote one.
         ((< cur-level prev-level)
          (org-do-promote))
         ;; If deeper than prev-level, promote until higher than
         ;; prev-level.
         ((> cur-level prev-level)
          (cl-loop repeat (+ 1 (/ (- cur-level prev-level) (org-level-increment)))
		   do (org-do-promote))))
        t))))

(defun org-map-tree (fun)
  "Call FUN for every heading underneath the current one."
  (org-back-to-heading t)
  (let ((level (funcall outline-level)))
    (save-excursion
      (funcall fun)
      (while (and (progn
		    (outline-next-heading)
		    (> (funcall outline-level) level))
		  (not (eobp)))
	(funcall fun)))))

(defun org-map-region (fun beg end)
  "Call FUN for every heading between BEG and END."
  (let ((org-ignore-region t))
    (save-excursion
      (setq end (copy-marker end))
      (goto-char beg)
      (when (and (re-search-forward org-outline-regexp-bol nil t)
		 (< (point) end))
	(funcall fun))
      (while (and (progn
		    (outline-next-heading)
		    (< (point) end))
		  (not (eobp)))
	(funcall fun)))))

(defun org-fixup-indentation (diff)
  "Change the indentation in the current entry by DIFF.

DIFF is an integer.  Indentation is done according to the
following rules:

  - Planning information and property drawers are always indented
    according to the new level of the headline;

  - Footnote definitions and their contents are ignored;

  - Inlinetasks' boundaries are not shifted;

  - Empty lines are ignored;

  - Other lines' indentation are shifted by DIFF columns, unless
    it would introduce a structural change in the document, in
    which case no shifting is done at all.

Assume point is at a heading or an inlinetask beginning."
  (org-with-wide-buffer
   (narrow-to-region (line-beginning-position)
		     (save-excursion
		       (if (org-with-limited-levels (org-at-heading-p))
			   (org-with-limited-levels (outline-next-heading))
			 (org-inlinetask-goto-end))
		       (point)))
   (forward-line)
   ;; Indent properly planning info and property drawer.
   (when (looking-at-p org-planning-line-re)
     (org-indent-line)
     (forward-line))
   (when (looking-at org-property-drawer-re)
     (goto-char (match-end 0))
     (forward-line)
     (save-excursion (org-indent-region (match-beginning 0) (match-end 0))))
   (catch 'no-shift
     (when (zerop diff) (throw 'no-shift nil))
     ;; If DIFF is negative, first check if a shift is possible at all
     ;; (e.g., it doesn't break structure).  This can only happen if
     ;; some contents are not properly indented.
     (let ((case-fold-search t))
       (when (< diff 0)
	 (let ((diff (- diff))
	       (forbidden-re (concat org-outline-regexp
				     "\\|"
				     (substring org-footnote-definition-re 1))))
	   (save-excursion
	     (while (not (eobp))
	       (cond
		((looking-at-p "[ \t]*$") (forward-line))
		((and (looking-at-p org-footnote-definition-re)
		      (let ((e (org-element-at-point)))
			(and (eq (org-element-type e) 'footnote-definition)
			     (goto-char (org-element-property :end e))))))
		((looking-at-p org-outline-regexp) (forward-line))
		;; Give up if shifting would move before column 0 or
		;; if it would introduce a headline or a footnote
		;; definition.
		(t
		 (skip-chars-forward " \t")
		 (let ((ind (current-column)))
		   (when (or (< ind diff)
			     (and (= ind diff) (looking-at-p forbidden-re)))
		     (throw 'no-shift nil)))
		 ;; Ignore contents of example blocks and source
		 ;; blocks if their indentation is meant to be
		 ;; preserved.  Jump to block's closing line.
		 (beginning-of-line)
		 (or (and (looking-at-p "[ \t]*#\\+BEGIN_\\(EXAMPLE\\|SRC\\)")
			  (let ((e (org-element-at-point)))
			    (and (memq (org-element-type e)
				       '(example-block src-block))
				 (or org-src-preserve-indentation
				     (org-element-property :preserve-indent e))
				 (goto-char (org-element-property :end e))
				 (progn (skip-chars-backward " \r\t\n")
					(beginning-of-line)
					t))))
		     (forward-line))))))))
       ;; Shift lines but footnote definitions, inlinetasks boundaries
       ;; by DIFF.  Also skip contents of source or example blocks
       ;; when indentation is meant to be preserved.
       (while (not (eobp))
	 (cond
	  ((and (looking-at-p org-footnote-definition-re)
		(let ((e (org-element-at-point)))
		  (and (eq (org-element-type e) 'footnote-definition)
		       (goto-char (org-element-property :end e))))))
	  ((looking-at-p org-outline-regexp) (forward-line))
	  ((looking-at-p "[ \t]*$") (forward-line))
	  (t
	   (indent-line-to (+ (org-get-indentation) diff))
	   (beginning-of-line)
	   (or (and (looking-at-p "[ \t]*#\\+BEGIN_\\(EXAMPLE\\|SRC\\)")
		    (let ((e (org-element-at-point)))
		      (and (memq (org-element-type e)
				 '(example-block src-block))
			   (or org-src-preserve-indentation
			       (org-element-property :preserve-indent e))
			   (goto-char (org-element-property :end e))
			   (progn (skip-chars-backward " \r\t\n")
				  (beginning-of-line)
				  t))))
	       (forward-line)))))))))

(defun org-convert-to-odd-levels ()
  "Convert an Org file with all levels allowed to one with odd levels.
This will leave level 1 alone, convert level 2 to level 3, level 3 to
level 5 etc."
  (interactive)
  (when (yes-or-no-p "Are you sure you want to globally change levels to odd? ")
    (let ((outline-level 'org-outline-level)
	  (org-odd-levels-only nil) n)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^\\*\\*+ " nil t)
	  (setq n (- (length (match-string 0)) 2))
	  (while (>= (setq n (1- n)) 0)
	    (org-demote))
	  (end-of-line 1))))))

(defun org-convert-to-oddeven-levels ()
  "Convert an Org file with only odd levels to one with odd/even levels.
This promotes level 3 to level 2, level 5 to level 3 etc.  If the
file contains a section with an even level, conversion would
destroy the structure of the file.  An error is signaled in this
case."
  (interactive)
  (goto-char (point-min))
  ;; First check if there are no even levels
  (when (re-search-forward "^\\(\\*\\*\\)+ " nil t)
    (org-show-set-visibility 'canonical)
    (error "Not all levels are odd in this file.  Conversion not possible"))
  (when (yes-or-no-p "Are you sure you want to globally change levels to odd-even? ")
    (let ((outline-regexp org-outline-regexp)
	  (outline-level 'org-outline-level)
	  (org-odd-levels-only nil) n)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^\\*\\*+ " nil t)
	  (setq n (/ (1- (length (match-string 0))) 2))
	  (while (>= (setq n (1- n)) 0)
	    (org-promote))
	  (end-of-line 1))))))

(defun org-tr-level (n)
  "Make N odd if required."
  (if org-odd-levels-only (1+ (/ n 2)) n))

;;; Vertical tree motion, cutting and pasting of subtrees

(defun org-move-subtree-up (&optional arg)
  "Move the current subtree up past ARG headlines of the same level."
  (interactive "p")
  (org-move-subtree-down (- (prefix-numeric-value arg))))

(defun org-move-subtree-down (&optional arg)
  "Move the current subtree down past ARG headlines of the same level."
  (interactive "p")
  (setq arg (prefix-numeric-value arg))
  (let ((movfunc (if (> arg 0) 'org-get-next-sibling
		   'org-get-last-sibling))
	(ins-point (make-marker))
	(cnt (abs arg))
	(col (current-column))
	beg beg0 end txt folded ne-beg ne-end ne-ins ins-end)
    ;; Select the tree
    (org-back-to-heading)
    (setq beg0 (point))
    (save-excursion
      (setq ne-beg (org-back-over-empty-lines))
      (setq beg (point)))
    (save-match-data
      (save-excursion (outline-end-of-heading)
		      (setq folded (org-invisible-p)))
      (progn (org-end-of-subtree nil t)
	     (unless (eobp) (backward-char))))
    (outline-next-heading)
    (setq ne-end (org-back-over-empty-lines))
    (setq end (point))
    (goto-char beg0)
    (when (and (> arg 0) (org-first-sibling-p) (< ne-end ne-beg))
      ;; include less whitespace
      (save-excursion
	(goto-char beg)
	(forward-line (- ne-beg ne-end))
	(setq beg (point))))
    ;; Find insertion point, with error handling
    (while (> cnt 0)
      (or (and (funcall movfunc) (looking-at org-outline-regexp))
	  (progn (goto-char beg0)
		 (user-error "Cannot move past superior level or buffer limit")))
      (setq cnt (1- cnt)))
    (when (> arg 0)
      ;; Moving forward - still need to move over subtree
      (org-end-of-subtree t t)
      (save-excursion
	(org-back-over-empty-lines)
	(or (bolp) (newline))))
    (setq ne-ins (org-back-over-empty-lines))
    (move-marker ins-point (point))
    (setq txt (buffer-substring beg end))
    (org-save-markers-in-region beg end)
    (delete-region beg end)
    (org-remove-empty-overlays-at beg)
    (or (= beg (point-min)) (outline-flag-region (1- beg) beg nil))
    (or (bobp) (outline-flag-region (1- (point)) (point) nil))
    (and (not (bolp)) (looking-at "\n") (forward-char 1))
    (let ((bbb (point)))
      (insert-before-markers txt)
      (org-reinstall-markers-in-region bbb)
      (move-marker ins-point bbb))
    (or (bolp) (insert "\n"))
    (setq ins-end (point))
    (goto-char ins-point)
    (org-skip-whitespace)
    (when (and (< arg 0)
	       (org-first-sibling-p)
	       (> ne-ins ne-beg))
      ;; Move whitespace back to beginning
      (save-excursion
	(goto-char ins-end)
	(let ((kill-whole-line t))
	  (kill-line (- ne-ins ne-beg)) (point)))
      (insert (make-string (- ne-ins ne-beg) ?\n)))
    (move-marker ins-point nil)
    (if folded
	(outline-hide-subtree)
      (org-show-entry)
      (org-show-children)
      (org-cycle-hide-drawers 'children))
    (org-clean-visibility-after-subtree-move)
    ;; move back to the initial column we were at
    (move-to-column col)))

(defvar org-subtree-clip ""
  "Clipboard for cut and paste of subtrees.
This is actually only a copy of the kill, because we use the normal kill
ring.  We need it to check if the kill was created by `org-copy-subtree'.")

(defvar org-subtree-clip-folded nil
  "Was the last copied subtree folded?
This is used to fold the tree back after pasting.")

(defun org-cut-subtree (&optional n)
  "Cut the current subtree into the clipboard.
With prefix arg N, cut this many sequential subtrees.
This is a short-hand for marking the subtree and then cutting it."
  (interactive "p")
  (org-copy-subtree n 'cut))

(defun org-copy-subtree (&optional n cut force-store-markers nosubtrees)
  "Copy the current subtree it in the clipboard.
With prefix arg N, copy this many sequential subtrees.
This is a short-hand for marking the subtree and then copying it.
If CUT is non-nil, actually cut the subtree.
If FORCE-STORE-MARKERS is non-nil, store the relative locations
of some markers in the region, even if CUT is non-nil.  This is
useful if the caller implements cut-and-paste as copy-then-paste-then-cut."
  (interactive "p")
  (let (beg end folded (beg0 (point)))
    (if (called-interactively-p 'any)
	(org-back-to-heading nil) ; take what looks like a subtree
      (org-back-to-heading t)) ; take what is really there
    (setq beg (point))
    (skip-chars-forward " \t\r\n")
    (save-match-data
      (if nosubtrees
	  (outline-next-heading)
	(save-excursion (outline-end-of-heading)
			(setq folded (org-invisible-p)))
	(ignore-errors (org-forward-heading-same-level (1- n) t))
	(org-end-of-subtree t t)))
    ;; Include the end of an inlinetask
    (when (and (featurep 'org-inlinetask)
	       (looking-at-p (concat (org-inlinetask-outline-regexp)
				     "END[ \t]*$")))
      (end-of-line))
    (setq end (point))
    (goto-char beg0)
    (when (> end beg)
      (setq org-subtree-clip-folded folded)
      (when (or cut force-store-markers)
	(org-save-markers-in-region beg end))
      (if cut (kill-region beg end) (copy-region-as-kill beg end))
      (setq org-subtree-clip (current-kill 0))
      (message "%s: Subtree(s) with %d characters"
	       (if cut "Cut" "Copied")
	       (length org-subtree-clip)))))

(defun org-paste-subtree (&optional level tree for-yank remove)
  "Paste the clipboard as a subtree, with modification of headline level.
The entire subtree is promoted or demoted in order to match a new headline
level.

If the cursor is at the beginning of a headline, the same level as
that headline is used to paste the tree.

If not, the new level is derived from the *visible* headings
before and after the insertion point, and taken to be the inferior headline
level of the two.  So if the previous visible heading is level 3 and the
next is level 4 (or vice versa), level 4 will be used for insertion.
This makes sure that the subtree remains an independent subtree and does
not swallow low level entries.

You can also force a different level, either by using a numeric prefix
argument, or by inserting the heading marker by hand.  For example, if the
cursor is after \"*****\", then the tree will be shifted to level 5.

If optional TREE is given, use this text instead of the kill ring.

When FOR-YANK is set, this is called by `org-yank'.  In this case, do not
move back over whitespace before inserting, and move point to the end of
the inserted text when done.

When REMOVE is non-nil, remove the subtree from the clipboard."
  (interactive "P")
  (setq tree (or tree (and kill-ring (current-kill 0))))
  (unless (org-kill-is-subtree-p tree)
    (user-error "%s"
		(substitute-command-keys
		 "The kill is not a (set of) tree(s) - please use \\[yank] to yank anyway")))
  (org-with-limited-levels
   (let* ((visp (not (org-invisible-p)))
	  (txt tree)
	  (^re_ "\\(\\*+\\)[  \t]*")
	  (old-level (if (string-match org-outline-regexp-bol txt)
			 (- (match-end 0) (match-beginning 0) 1)
		       -1))
	  (force-level (cond (level (prefix-numeric-value level))
			     ((and (looking-at "[ \t]*$")
				   (string-match
				    "^\\*+$" (buffer-substring
					      (point-at-bol) (point))))
			      (- (match-end 0) (match-beginning 0)))
			     ((and (bolp)
				   (looking-at org-outline-regexp))
			      (- (match-end 0) (point) 1))))
	  (previous-level (save-excursion
			    (condition-case nil
				(progn
				  (outline-previous-visible-heading 1)
				  (if (looking-at ^re_)
				      (- (match-end 0) (match-beginning 0) 1)
				    1))
			      (error 1))))
	  (next-level (save-excursion
			(condition-case nil
			    (progn
			      (or (looking-at org-outline-regexp)
				  (outline-next-visible-heading 1))
			      (if (looking-at ^re_)
				  (- (match-end 0) (match-beginning 0) 1)
				1))
			  (error 1))))
	  (new-level (or force-level (max previous-level next-level)))
	  (shift (if (or (= old-level -1)
			 (= new-level -1)
			 (= old-level new-level))
		     0
		   (- new-level old-level)))
	  (delta (if (> shift 0) -1 1))
	  (func (if (> shift 0) 'org-demote 'org-promote))
	  (org-odd-levels-only nil)
	  beg end newend)
     ;; Remove the forced level indicator
     (when force-level
       (delete-region (point-at-bol) (point)))
     ;; Paste
     (beginning-of-line (if (bolp) 1 2))
     (setq beg (point))
     (and (fboundp 'org-id-paste-tracker) (org-id-paste-tracker txt))
     (insert-before-markers txt)
     (unless (string-suffix-p "\n" txt) (insert "\n"))
     (setq newend (point))
     (org-reinstall-markers-in-region beg)
     (setq end (point))
     (goto-char beg)
     (skip-chars-forward " \t\n\r")
     (setq beg (point))
     (when (and (org-invisible-p) visp)
       (save-excursion (outline-show-heading)))
     ;; Shift if necessary
     (unless (= shift 0)
       (save-restriction
	 (narrow-to-region beg end)
	 (while (not (= shift 0))
	   (org-map-region func (point-min) (point-max))
	   (setq shift (+ delta shift)))
	 (goto-char (point-min))
	 (setq newend (point-max))))
     (when (or (called-interactively-p 'interactive) for-yank)
       (message "Clipboard pasted as level %d subtree" new-level))
     (when (and (not for-yank) ; in this case, org-yank will decide about folding
		kill-ring
		(eq org-subtree-clip (current-kill 0))
		org-subtree-clip-folded)
       ;; The tree was folded before it was killed/copied
       (outline-hide-subtree))
     (and for-yank (goto-char newend))
     (and remove (setq kill-ring (cdr kill-ring))))))

(defun org-kill-is-subtree-p (&optional txt)
  "Check if the current kill is an outline subtree, or a set of trees.
Returns nil if kill does not start with a headline, or if the first
headline level is not the largest headline level in the tree.
So this will actually accept several entries of equal levels as well,
which is OK for `org-paste-subtree'.
If optional TXT is given, check this string instead of the current kill."
  (let* ((kill (or txt (and kill-ring (current-kill 0)) ""))
	 (re (org-get-limited-outline-regexp))
	 (^re (concat "^" re))
	 (start-level (and kill
			   (string-match
			    (concat "\\`\\([ \t\n\r]*?\n\\)?\\(" re "\\)")
			    kill)
			   (- (match-end 2) (match-beginning 2) 1)))
	 (start (1+ (or (match-beginning 2) -1))))
    (if (not start-level)
	(progn
	  nil)  ;; does not even start with a heading
      (catch 'exit
	(while (setq start (string-match ^re kill (1+ start)))
	  (when (< (- (match-end 0) (match-beginning 0) 1) start-level)
	    (throw 'exit nil)))
	t))))

(defvar org-markers-to-move nil
  "Markers that should be moved with a cut-and-paste operation.
Those markers are stored together with their positions relative to
the start of the region.")

(defun org-save-markers-in-region (beg end)
  "Check markers in region.
If these markers are between BEG and END, record their position relative
to BEG, so that after moving the block of text, we can put the markers back
into place.
This function gets called just before an entry or tree gets cut from the
buffer.  After re-insertion, `org-reinstall-markers-in-region' must be
called immediately, to move the markers with the entries."
  (setq org-markers-to-move nil)
  (when (featurep 'org-clock)
    (org-clock-save-markers-for-cut-and-paste beg end))
  (when (featurep 'org-agenda)
    (org-agenda-save-markers-for-cut-and-paste beg end)))

(defun org-check-and-save-marker (marker beg end)
  "Check if MARKER is between BEG and END.
If yes, remember the marker and the distance to BEG."
  (when (and (marker-buffer marker)
	     (equal (marker-buffer marker) (current-buffer))
	     (>= marker beg) (< marker end))
    (push (cons marker (- marker beg)) org-markers-to-move)))

(defun org-reinstall-markers-in-region (beg)
  "Move all remembered markers to their position relative to BEG."
  (dolist (x org-markers-to-move)
    (move-marker (car x) (+ beg (cdr x))))
  (setq org-markers-to-move nil))

(defun org-narrow-to-subtree ()
  "Narrow buffer to the current subtree."
  (interactive)
  (save-excursion
    (save-match-data
      (org-with-limited-levels
       (narrow-to-region
	(progn (org-back-to-heading t) (point))
	(progn (org-end-of-subtree t t)
	       (when (and (org-at-heading-p) (not (eobp))) (backward-char 1))
	       (point)))))))

(defun org-narrow-to-block ()
  "Narrow buffer to the current block."
  (interactive)
  (let* ((case-fold-search t)
	 (blockp (org-between-regexps-p "^[ \t]*#\\+begin_.*"
					"^[ \t]*#\\+end_.*")))
    (if blockp
	(narrow-to-region (car blockp) (cdr blockp))
      (user-error "Not in a block"))))

(defun org-clone-subtree-with-time-shift (n &optional shift)
  "Clone the task (subtree) at point N times.
The clones will be inserted as siblings.

In interactive use, the user will be prompted for the number of
clones to be produced.  If the entry has a timestamp, the user
will also be prompted for a time shift, which may be a repeater
as used in time stamps, for example `+3d'.  To disable this,
you can call the function with a universal prefix argument.

When a valid repeater is given and the entry contains any time
stamps, the clones will become a sequence in time, with time
stamps in the subtree shifted for each clone produced.  If SHIFT
is nil or the empty string, time stamps will be left alone.  The
ID property of the original subtree is removed.

In each clone, all the CLOCK entries will be removed.  This
prevents Org from considering that the clocked times overlap.

If the original subtree did contain time stamps with a repeater,
the following will happen:
- the repeater will be removed in each clone
- an additional clone will be produced, with the current, unshifted
  date(s) in the entry.
- the original entry will be placed *after* all the clones, with
  repeater intact.
- the start days in the repeater in the original entry will be shifted
  to past the last clone.
In this way you can spell out a number of instances of a repeating task,
and still retain the repeater to cover future instances of the task.

As described above, N+1 clones are produced when the original
subtree has a repeater.  Setting N to 0, then, can be used to
remove the repeater from a subtree and create a shifted clone
with the original repeater."
  (interactive "nNumber of clones to produce: ")
  (unless (wholenump n) (user-error "Invalid number of replications %s" n))
  (when (org-before-first-heading-p) (user-error "No subtree to clone"))
  (let* ((beg (save-excursion (org-back-to-heading t) (point)))
	 (end-of-tree (save-excursion (org-end-of-subtree t t) (point)))
	 (shift
	  (or shift
	      (if (and (not (equal current-prefix-arg '(4)))
		       (save-excursion
			 (goto-char beg)
			 (re-search-forward org-ts-regexp-both end-of-tree t)))
		  (read-from-minibuffer
		   "Date shift per clone (e.g. +1w, empty to copy unchanged): ")
		"")))			;No time shift
	 (doshift
	  (and (org-string-nw-p shift)
	       (or (string-match "\\`[ \t]*\\+?\\([0-9]+\\)\\([dwmy]\\)[ \t]*\\'"
				 shift)
		   (user-error "Invalid shift specification %s" shift)))))
    (goto-char end-of-tree)
    (unless (bolp) (insert "\n"))
    (let* ((end (point))
	   (template (buffer-substring beg end))
	   (shift-n (and doshift (string-to-number (match-string 1 shift))))
	   (shift-what (pcase (and doshift (match-string 2 shift))
			 (`nil nil)
			 ("d" 'day)
			 ("w" (setq shift-n (* 7 shift-n)) 'day)
			 ("m" 'month)
			 ("y" 'year)
			 (_ (error "Unsupported time unit"))))
	   (nmin 1)
	   (nmax n)
	   (n-no-remove -1)
	   (idprop (org-entry-get nil "ID")))
      (when (and doshift
		 (string-match-p "<[^<>\n]+ [.+]?\\+[0-9]+[hdwmy][^<>\n]*>"
				 template))
	(delete-region beg end)
	(setq end beg)
	(setq nmin 0)
	(setq nmax (1+ nmax))
	(setq n-no-remove nmax))
      (goto-char end)
      (cl-loop for n from nmin to nmax do
	       (insert
		;; Prepare clone.
		(with-temp-buffer
		  (insert template)
		  (org-mode)
		  (goto-char (point-min))
		  (org-show-subtree)
		  (and idprop (if org-clone-delete-id
				  (org-entry-delete nil "ID")
				(org-id-get-create t)))
		  (unless (= n 0)
		    (while (re-search-forward org-clock-line-re nil t)
		      (delete-region (line-beginning-position)
				     (line-beginning-position 2)))
		    (goto-char (point-min))
		    (while (re-search-forward org-drawer-regexp nil t)
		      (org-remove-empty-drawer-at (point))))
		  (goto-char (point-min))
		  (when doshift
		    (while (re-search-forward org-ts-regexp-both nil t)
		      (org-timestamp-change (* n shift-n) shift-what))
		    (unless (= n n-no-remove)
		      (goto-char (point-min))
		      (while (re-search-forward org-ts-regexp nil t)
			(save-excursion
			  (goto-char (match-beginning 0))
			  (when (looking-at "<[^<>\n]+\\( +[.+]?\\+[0-9]+[hdwmy]\\)")
			    (delete-region (match-beginning 1) (match-end 1)))))))
		  (buffer-string)))))
    (goto-char beg)))

;;; Outline Sorting

(defun org-sort (&optional with-case)
  "Call `org-sort-entries', `org-table-sort-lines' or `org-sort-list'.
Optional argument WITH-CASE means sort case-sensitively."
  (interactive "P")
  (org-call-with-arg
   (cond ((org-at-table-p) #'org-table-sort-lines)
	 ((org-at-item-p) #'org-sort-list)
	 (t #'org-sort-entries))
   with-case))

(defun org-sort-remove-invisible (s)
  "Remove invisible part of links and emphasis markers from string S."
  (remove-text-properties 0 (length s) org-rm-props s)
  (replace-regexp-in-string
   org-verbatim-re (lambda (m) (format "%s " (match-string 4 m)))
   (replace-regexp-in-string
    org-emph-re (lambda (m) (format " %s " (match-string 4 m)))
    (org-link-display-format s)
    t t) t t))

(defvar org-priority-regexp) ; defined later in the file

(defvar org-after-sorting-entries-or-items-hook nil
  "Hook that is run after a bunch of entries or items have been sorted.
When children are sorted, the cursor is in the parent line when this
hook gets called.  When a region or a plain list is sorted, the cursor
will be in the first entry of the sorted region/list.")

(defun org-sort-entries
    (&optional with-case sorting-type getkey-func compare-func property
	       interactive?)
  "Sort entries on a certain level of an outline tree.
If there is an active region, the entries in the region are sorted.
Else, if the cursor is before the first entry, sort the top-level items.
Else, the children of the entry at point are sorted.

Sorting can be alphabetically, numerically, by date/time as given by
a time stamp, by a property, by priority order, or by a custom function.

The command prompts for the sorting type unless it has been given to the
function through the SORTING-TYPE argument, which needs to be a character,
\(?n ?N ?a ?A ?t ?T ?s ?S ?d ?D ?p ?P ?o ?O ?r ?R ?f ?F ?k ?K).  Here is
the precise meaning of each character:

a   Alphabetically, ignoring the TODO keyword and the priority, if any.
c   By creation time, which is assumed to be the first inactive time stamp
    at the beginning of a line.
d   By deadline date/time.
k   By clocking time.
n   Numerically, by converting the beginning of the entry/item to a number.
o   By order of TODO keywords.
p   By priority according to the cookie.
r   By the value of a property.
s   By scheduled date/time.
t   By date/time, either the first active time stamp in the entry, or, if
    none exist, by the first inactive one.

Capital letters will reverse the sort order.

If the SORTING-TYPE is ?f or ?F, then GETKEY-FUNC specifies a function to be
called with point at the beginning of the record.  It must return a
value that is compatible with COMPARE-FUNC, the function used to
compare entries.

Comparing entries ignores case by default.  However, with an optional argument
WITH-CASE, the sorting considers case as well.

Sorting is done against the visible part of the headlines, it ignores hidden
links.

When sorting is done, call `org-after-sorting-entries-or-items-hook'.

A non-nil value for INTERACTIVE? is used to signal that this
function is being called interactively."
  (interactive (list current-prefix-arg nil nil nil nil t))
  (let ((case-func (if with-case 'identity 'downcase))
        start beg end stars re re2
        txt what tmp)
    ;; Find beginning and end of region to sort
    (cond
     ((org-region-active-p)
      ;; we will sort the region
      (setq end (region-end)
            what "region")
      (goto-char (region-beginning))
      (unless (org-at-heading-p) (outline-next-heading))
      (setq start (point)))
     ((or (org-at-heading-p)
          (ignore-errors (progn (org-back-to-heading) t)))
      ;; we will sort the children of the current headline
      (org-back-to-heading)
      (setq start (point)
	    end (progn (org-end-of-subtree t t)
		       (or (bolp) (insert "\n"))
		       (when (>= (org-back-over-empty-lines) 1)
			 (forward-line 1))
		       (point))
	    what "children")
      (goto-char start)
      (outline-show-subtree)
      (outline-next-heading))
     (t
      ;; we will sort the top-level entries in this file
      (goto-char (point-min))
      (or (org-at-heading-p) (outline-next-heading))
      (setq start (point))
      (goto-char (point-max))
      (beginning-of-line 1)
      (when (looking-at ".*?\\S-")
	;; File ends in a non-white line
	(end-of-line 1)
	(insert "\n"))
      (setq end (point-max))
      (setq what "top-level")
      (goto-char start)
      (outline-show-all)))

    (setq beg (point))
    (when (>= beg end) (goto-char start) (user-error "Nothing to sort"))

    (looking-at "\\(\\*+\\)")
    (setq stars (match-string 1)
	  re (concat "^" (regexp-quote stars) " +")
	  re2 (concat "^" (regexp-quote (substring stars 0 -1)) "[ \t\n]")
	  txt (buffer-substring beg end))
    (unless (equal (substring txt -1) "\n") (setq txt (concat txt "\n")))
    (when (and (not (equal stars "*")) (string-match re2 txt))
      (user-error "Region to sort contains a level above the first entry"))

    (unless sorting-type
      (message
       "Sort %s: [a]lpha  [n]umeric  [p]riority  p[r]operty  todo[o]rder  [f]unc
               [t]ime [s]cheduled  [d]eadline  [c]reated  cloc[k]ing
               A/N/P/R/O/F/T/S/D/C/K means reversed:"
       what)
      (setq sorting-type (read-char-exclusive)))

    (unless getkey-func
      (and (= (downcase sorting-type) ?f)
	   (setq getkey-func
		 (or (and interactive?
			  (org-read-function
			   "Function for extracting keys: "))
		     (error "Missing key extractor")))))

    (and (= (downcase sorting-type) ?r)
	 (not property)
	 (setq property
	       (completing-read "Property: "
				(mapcar #'list (org-buffer-property-keys t))
				nil t)))

    (when (member sorting-type '(?k ?K)) (org-clock-sum))
    (message "Sorting entries...")

    (save-restriction
      (narrow-to-region start end)
      (let ((restore-clock?
	     ;; The clock marker is lost when using `sort-subr'; mark
	     ;; the clock with temporary `:org-clock-marker-backup'
	     ;; text property.
	     (when (and (eq (org-clock-is-active) (current-buffer))
			(<= start (marker-position org-clock-marker))
			(>= end (marker-position org-clock-marker)))
	       (org-with-silent-modifications
		(put-text-property (1- org-clock-marker) org-clock-marker
				   :org-clock-marker-backup t))
	       t))
	    (dcst (downcase sorting-type))
	    (case-fold-search nil)
	    (now (current-time)))
        (sort-subr
         (/= dcst sorting-type)
         ;; This function moves to the beginning character of the "record" to
         ;; be sorted.
	 (lambda nil
	   (if (re-search-forward re nil t)
	       (goto-char (match-beginning 0))
	     (goto-char (point-max))))
         ;; This function moves to the last character of the "record" being
         ;; sorted.
	 (lambda nil
	   (save-match-data
	     (condition-case nil
		 (outline-forward-same-level 1)
	       (error
		(goto-char (point-max))))))
         ;; This function returns the value that gets sorted against.
	 (lambda nil
	   (cond
	    ((= dcst ?n)
	     (if (looking-at org-complex-heading-regexp)
		 (string-to-number (org-sort-remove-invisible (match-string 4)))
	       nil))
	    ((= dcst ?a)
	     (if (looking-at org-complex-heading-regexp)
		 (funcall case-func (org-sort-remove-invisible (match-string 4)))
	       nil))
	    ((= dcst ?k)
	     (or (get-text-property (point) :org-clock-minutes) 0))
	    ((= dcst ?t)
	     (let ((end (save-excursion (outline-next-heading) (point))))
	       (if (or (re-search-forward org-ts-regexp end t)
		       (re-search-forward org-ts-regexp-both end t))
		   (org-time-string-to-seconds (match-string 0))
		 (float-time now))))
	    ((= dcst ?c)
	     (let ((end (save-excursion (outline-next-heading) (point))))
	       (if (re-search-forward
		    (concat "^[ \t]*\\[" org-ts-regexp1 "\\]")
		    end t)
		   (org-time-string-to-seconds (match-string 0))
		 (float-time now))))
	    ((= dcst ?s)
	     (let ((end (save-excursion (outline-next-heading) (point))))
	       (if (re-search-forward org-scheduled-time-regexp end t)
		   (org-time-string-to-seconds (match-string 1))
		 (float-time now))))
	    ((= dcst ?d)
	     (let ((end (save-excursion (outline-next-heading) (point))))
	       (if (re-search-forward org-deadline-time-regexp end t)
		   (org-time-string-to-seconds (match-string 1))
		 (float-time now))))
	    ((= dcst ?p)
	     (if (re-search-forward org-priority-regexp (point-at-eol) t)
		 (string-to-char (match-string 2))
	       org-default-priority))
	    ((= dcst ?r)
	     (or (org-entry-get nil property) ""))
	    ((= dcst ?o)
	     (when (looking-at org-complex-heading-regexp)
	       (let* ((m (match-string 2))
		      (s (if (member m org-done-keywords) '- '+)))
		 (- 99 (funcall s (length (member m org-todo-keywords-1)))))))
	    ((= dcst ?f)
	     (if getkey-func
		 (progn
		   (setq tmp (funcall getkey-func))
		   (when (stringp tmp) (setq tmp (funcall case-func tmp)))
		   tmp)
	       (error "Invalid key function `%s'" getkey-func)))
	    (t (error "Invalid sorting type `%c'" sorting-type))))
         nil
         (cond
          ((= dcst ?a) 'string<)
          ((= dcst ?f)
	   (or compare-func
	       (and interactive?
		    (org-read-function
		     (concat "Function for comparing keys "
			     "(empty for default `sort-subr' predicate): ")
		     'allow-empty))))
          ((member dcst '(?p ?t ?s ?d ?c ?k)) '<)))
	(when restore-clock?
	  (move-marker org-clock-marker
		       (1+ (next-single-property-change
			    start :org-clock-marker-backup)))
	  (remove-text-properties (1- org-clock-marker) org-clock-marker
				  '(:org-clock-marker-backup t)))))
    (run-hooks 'org-after-sorting-entries-or-items-hook)
    (message "Sorting entries...done")))

;;; The orgstruct minor mode

;; Define a minor mode which can be used in other modes in order to
;; integrate the Org mode structure editing commands.

;; This is really a hack, because the Org mode structure commands use
;; keys which normally belong to the major mode.  Here is how it
;; works: The minor mode defines all the keys necessary to operate the
;; structure commands, but wraps the commands into a function which
;; tests if the cursor is currently at a headline or a plain list
;; item.  If that is the case, the structure command is used,
;; temporarily setting many Org mode variables like regular
;; expressions for filling etc.  However, when any of those keys is
;; used at a different location, function uses `key-binding' to look
;; up if the key has an associated command in another currently active
;; keymap (minor modes, major mode, global), and executes that
;; command.  There might be problems if any of the keys is otherwise
;; used as a prefix key.

(defcustom orgstruct-heading-prefix-regexp ""
  "Regexp that matches the custom prefix of Org headlines in
orgstruct(++)-mode."
  :group 'org
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'regexp)
;;;###autoload(put 'orgstruct-heading-prefix-regexp 'safe-local-variable 'stringp)

(defcustom orgstruct-setup-hook nil
  "Hook run after orgstruct-mode-map is filled."
  :group 'org
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'hook)

(defvar orgstruct-initialized nil)

(defvar org-local-vars nil
  "List of local variables, for use by `orgstruct-mode'.")

;;;###autoload
(define-minor-mode orgstruct-mode
  "Toggle the minor mode `orgstruct-mode'.
This mode is for using Org mode structure commands in other
modes.  The following keys behave as if Org mode were active, if
the cursor is on a headline, or on a plain list item (both as
defined by Org mode)."
  nil " OrgStruct" (make-sparse-keymap)
  (funcall (if orgstruct-mode
	       'add-to-invisibility-spec
	     'remove-from-invisibility-spec)
	   '(outline . t))
  (when orgstruct-mode
    (org-load-modules-maybe)
    (unless orgstruct-initialized
      (orgstruct-setup)
      (setq orgstruct-initialized t))))

;;;###autoload
(defun turn-on-orgstruct ()
  "Unconditionally turn on `orgstruct-mode'."
  (orgstruct-mode 1))

(defvar-local orgstruct-is-++ nil
  "Is `orgstruct-mode' in ++ version in the current-buffer?")
(defvar-local org-fb-vars nil)
(defun orgstruct++-mode (&optional arg)
  "Toggle `orgstruct-mode', the enhanced version of it.
In addition to setting orgstruct-mode, this also exports all
indentation and autofilling variables from Org mode into the
buffer.  It will also recognize item context in multiline items."
  (interactive "P")
  (setq arg (prefix-numeric-value (or arg (if orgstruct-mode -1 1))))
  (if (< arg 1)
      (progn (orgstruct-mode -1)
	     (dolist (v org-fb-vars)
	       (set (make-local-variable (car v))
		    (if (eq (car-safe (cadr v)) 'quote)
			(cl-cadadr v)
		      (nth 1 v)))))
    (orgstruct-mode 1)
    (setq org-fb-vars nil)
    (unless org-local-vars
      (setq org-local-vars (org-get-local-variables)))
    (let (var val)
      (dolist (x org-local-vars)
	(when (string-match
	       "^\\(paragraph-\\|auto-fill\\|normal-auto-fill\\|fill-paragraph\
\\|fill-prefix\\|indent-\\)"
	       (symbol-name (car x)))
	  (setq var (car x) val (nth 1 x))
	  (push (list var `(quote ,(eval var))) org-fb-vars)
	  (set (make-local-variable var)
	       (if (eq (car-safe val) 'quote) (nth 1 val) val))))
      (setq-local orgstruct-is-++ t))))

;;;###autoload
(defun turn-on-orgstruct++ ()
  "Unconditionally turn on `orgstruct++-mode'."
  (orgstruct++-mode 1))

(defun orgstruct-error ()
  "Error when there is no default binding for a structure key."
  (interactive)
  (funcall (if (fboundp 'user-error)
	       'user-error
	     'error)
	   "This key has no function outside structure elements"))

(defun orgstruct-setup ()
  "Setup orgstruct keymap."
  (dolist (cell '((org-demote . t)
		  (org-metaleft . t)
		  (org-metaright . t)
		  (org-promote . t)
		  (org-shiftmetaleft . t)
		  (org-shiftmetaright . t)
		  org-backward-element
		  org-backward-heading-same-level
		  org-ctrl-c-ret
		  org-ctrl-c-minus
		  org-ctrl-c-star
		  org-cycle
		  org-force-cycle-archived
		  org-forward-heading-same-level
		  org-insert-heading
		  org-insert-heading-respect-content
		  org-kill-note-or-show-branches
		  org-mark-subtree
		  org-meta-return
		  org-metadown
		  org-metaup
		  org-narrow-to-subtree
		  org-promote-subtree
		  org-reveal
		  org-shiftdown
		  org-shiftleft
		  org-shiftmetadown
		  org-shiftmetaup
		  org-shiftright
		  org-shifttab
		  org-shifttab
		  org-shiftup
		  org-show-children
		  org-show-subtree
		  org-sort
		  org-up-element
		  outline-demote
		  outline-next-visible-heading
		  outline-previous-visible-heading
		  outline-promote
		  outline-up-heading))
    (let ((f (or (car-safe cell) cell))
	  (disable-when-heading-prefix (cdr-safe cell)))
      (when (fboundp f)
	(let ((new-bindings))
	  (dolist (binding (nconc (where-is-internal f org-mode-map)
				  (where-is-internal f outline-mode-map)))
	    (push binding new-bindings)
	    ;; TODO use local-function-key-map
	    (dolist (rep '(("<tab>" . "TAB")
			   ("<return>" . "RET")
			   ("<escape>" . "ESC")
			   ("<delete>" . "DEL")))
	      (setq binding (read-kbd-macro
			     (let ((case-fold-search))
			       (replace-regexp-in-string
				(regexp-quote (cdr rep))
				(car rep)
				(key-description binding)))))
	      (cl-pushnew binding new-bindings :test 'equal)))
	  (dolist (binding new-bindings)
	    (let ((key (lookup-key orgstruct-mode-map binding)))
	      (when (or (not key) (numberp key))
		(ignore-errors
		  (org-defkey orgstruct-mode-map
			      binding
			      (orgstruct-make-binding
			       f binding disable-when-heading-prefix))))))))))
  (run-hooks 'orgstruct-setup-hook))

(defun orgstruct-make-binding (fun key disable-when-heading-prefix)
  "Create a function for binding in the structure minor mode.
FUN is the command to call inside a table.  KEY is the key that
should be checked in for a command to execute outside of tables.
Non-nil `disable-when-heading-prefix' means to disable the command
if `orgstruct-heading-prefix-regexp' is not empty."
  (let ((name (concat "orgstruct-hijacker-" (symbol-name fun))))
    (let ((nname name)
	  (i 0))
      (while (fboundp (intern nname))
	(setq nname (format "%s-%d" name (setq i (1+ i)))))
      (setq name (intern nname)))
    (eval
     (let ((bindings '((org-heading-regexp
			(concat "^"
				orgstruct-heading-prefix-regexp
				"\\(\\*+\\)\\(?: +\\(.*?\\)\\)?[		]*$"))
		       (org-outline-regexp
			(concat orgstruct-heading-prefix-regexp "\\*+ "))
		       (org-outline-regexp-bol
			(concat "^" org-outline-regexp))
		       (outline-regexp org-outline-regexp)
		       (outline-heading-end-regexp "\n")
		       (outline-level 'org-outline-level)
		       (outline-heading-alist))))
       `(defun ,name (arg)
	  ,(concat "In Structure, run `" (symbol-name fun) "'.\n"
		   "Outside of structure, run the binding of `"
		   (key-description key) "'."
		   (when disable-when-heading-prefix
		     (concat
		      "\nIf `orgstruct-heading-prefix-regexp' is not empty, this command will always fall\n"
		      "back to the default binding due to limitations of Org's implementation of\n"
		      "`" (symbol-name fun) "'.")))
	  (interactive "p")
	  (let* ((disable
		  ,(and disable-when-heading-prefix
			'(not (string= orgstruct-heading-prefix-regexp ""))))
		 (fallback
		  (or disable
		      (not
		       (let* ,bindings
			 (org-context-p 'headline 'item
					,(when (memq fun
						     '(org-insert-heading
						       org-insert-heading-respect-content
						       org-meta-return))
					   '(when orgstruct-is-++
					      'item-body))))))))
	    (if fallback
		(let* ((orgstruct-mode)
		       (binding
			(let ((key ,key))
			  (catch 'exit
			    (dolist
				(rep
				 '(nil
				   ("<\\([^>]*\\)tab>" . "\\1TAB")
				   ("<\\([^>]*\\)return>" . "\\1RET")
				   ("<\\([^>]*\\)escape>" . "\\1ESC")
				   ("<\\([^>]*\\)delete>" . "\\1DEL"))
				 nil)
			      (when rep
				(setq key (read-kbd-macro
					   (let ((case-fold-search))
					     (replace-regexp-in-string
					      (car rep)
					      (cdr rep)
					      (key-description key))))))
			      (when (key-binding key)
				(throw 'exit (key-binding key))))))))
		  (if (keymapp binding)
		      (org-set-transient-map binding)
		    (let ((func (or binding
				    (unless disable
				      'orgstruct-error))))
		      (when func
			(call-interactively func)))))
	      (org-run-like-in-org-mode
	       (lambda ()
		 (interactive)
		 (let* ,bindings
		   (call-interactively ',fun)))))))))
    name))

(defun org-contextualize-keys (alist contexts)
  "Return valid elements in ALIST depending on CONTEXTS.

`org-agenda-custom-commands' or `org-capture-templates' are the
values used for ALIST, and `org-agenda-custom-commands-contexts'
or `org-capture-templates-contexts' are the associated contexts
definitions."
  (let ((contexts
	 ;; normalize contexts
	 (mapcar
	  (lambda(c) (cond ((listp (cadr c))
			    (list (car c) (car c) (nth 1 c)))
			   ((string= "" (cadr c))
			    (list (car c) (car c) (nth 2 c)))
			   (t c)))
          contexts))
	(a alist) r s)
    ;; loop over all commands or templates
    (dolist (c a)
      (let (vrules repl)
	(cond
	 ((not (assoc (car c) contexts))
	  (push c r))
	 ((and (assoc (car c) contexts)
	       (setq vrules (org-contextualize-validate-key
			     (car c) contexts)))
	  (mapc (lambda (vr)
		  (unless (equal (car vr) (cadr vr))
		    (setq repl vr)))
                vrules)
	  (if (not repl) (push c r)
	    (push (cadr repl) s)
	    (push
	     (cons (car c)
		   (cdr (or (assoc (cadr repl) alist)
			    (error "Undefined key `%s' as contextual replacement for `%s'"
				   (cadr repl) (car c)))))
	     r))))))
    ;; Return limited ALIST, possibly with keys modified, and deduplicated
    (delq
     nil
     (delete-dups
      (mapcar (lambda (x)
		(let ((tpl (car x)))
		  (unless (delq
			   nil
			   (mapcar (lambda (y)
				     (equal y tpl))
				   s))
                    x)))
	      (reverse r))))))

(defun org-contextualize-validate-key (key contexts)
  "Check CONTEXTS for agenda or capture KEY."
  (let (res)
    (dolist (r contexts)
      (dolist (rr (car (last r)))
	(when
	    (and (equal key (car r))
		 (if (functionp rr) (funcall rr)
		   (or (and (eq (car rr) 'in-file)
			    (buffer-file-name)
			    (string-match (cdr rr) (buffer-file-name)))
		       (and (eq (car rr) 'in-mode)
			    (string-match (cdr rr) (symbol-name major-mode)))
		       (and (eq (car rr) 'in-buffer)
			    (string-match (cdr rr) (buffer-name)))
		       (when (and (eq (car rr) 'not-in-file)
				  (buffer-file-name))
			 (not (string-match (cdr rr) (buffer-file-name))))
		       (when (eq (car rr) 'not-in-mode)
			 (not (string-match (cdr rr) (symbol-name major-mode))))
		       (when (eq (car rr) 'not-in-buffer)
			 (not (string-match (cdr rr) (buffer-name)))))))
	  (push r res))))
    (delete-dups (delq nil res))))

(defun org-context-p (&rest contexts)
  "Check if local context is any of CONTEXTS.
Possible values in the list of contexts are `table', `headline', and `item'."
  (let ((pos (point)))
    (goto-char (point-at-bol))
    (prog1 (or (and (memq 'table contexts)
		    (looking-at "[ \t]*|"))
	       (and (memq 'headline contexts)
		    (looking-at org-outline-regexp))
	       (and (memq 'item contexts)
		    (looking-at "[ \t]*\\([-+*] \\|[0-9]+[.)] \\)"))
	       (and (memq 'item-body contexts)
		    (org-in-item-p)))
      (goto-char pos))))

;;;###autoload
(defun org-run-like-in-org-mode (cmd)
  "Run a command, pretending that the current buffer is in Org mode.
This will temporarily bind local variables that are typically bound in
Org mode to the values they have in Org mode, and then interactively
call CMD."
  (org-load-modules-maybe)
  (unless org-local-vars
    (setq org-local-vars (org-get-local-variables)))
  (let (binds)
    (dolist (var org-local-vars)
      (when (or (not (boundp (car var)))
		(eq (symbol-value (car var))
		    (default-value (car var))))
	(push (list (car var) `(quote ,(cadr var))) binds)))
    (eval `(let ,binds
	     (call-interactively (quote ,cmd))))))

(defun org-get-category (&optional pos force-refresh)
  "Get the category applying to position POS."
  (save-match-data
    (when force-refresh (org-refresh-category-properties))
    (let ((pos (or pos (point))))
      (or (get-text-property pos 'org-category)
	  (progn (org-refresh-category-properties)
		 (get-text-property pos 'org-category))))))

;;; Refresh properties

(defun org-refresh-properties (dprop tprop)
  "Refresh buffer text properties.
DPROP is the drawer property and TPROP is either the
corresponding text property to set, or an alist with each element
being a text property (as a symbol) and a function to apply to
the value of the drawer property."
  (let* ((case-fold-search t)
	 (inhibit-read-only t)
	 (inherit? (org-property-inherit-p dprop))
	 (property-re (org-re-property (concat (regexp-quote dprop) "\\+?") t))
	 (global (and inherit? (org--property-global-value dprop nil))))
    (org-with-silent-modifications
     (org-with-point-at 1
       ;; Set global values (e.g., values defined through
       ;; "#+PROPERTY:" keywords) to the whole buffer.
       (when global (put-text-property (point-min) (point-max) tprop global))
       ;; Set local values.
       (while (re-search-forward property-re nil t)
	 (when (org-at-property-p)
	   (org-refresh-property tprop (org-entry-get (point) dprop) inherit?))
	 (outline-next-heading))))))

(defun org-refresh-property (tprop p &optional inherit)
  "Refresh the buffer text property TPROP from the drawer property P.
The refresh happens only for the current headline, or the whole
sub-tree if optional argument INHERIT is non-nil."
  (unless (org-before-first-heading-p)
    (save-excursion
      (org-back-to-heading t)
      (let ((start (point))
	    (end (save-excursion
		   (if inherit (org-end-of-subtree t t)
		     (or (outline-next-heading) (point-max))))))
	(if (symbolp tprop)
	    ;; TPROP is a text property symbol.
	    (put-text-property start end tprop p)
	  ;; TPROP is an alist with (property . function) elements.
	  (pcase-dolist (`(,prop . ,f) tprop)
	    (put-text-property start end prop (funcall f p))))))))

(defun org-refresh-category-properties ()
  "Refresh category text properties in the buffer."
  (let ((case-fold-search t)
	(inhibit-read-only t)
	(default-category
	  (cond ((null org-category)
		 (if buffer-file-name
		     (file-name-sans-extension
		      (file-name-nondirectory buffer-file-name))
		   "???"))
		((symbolp org-category) (symbol-name org-category))
		(t org-category))))
    (org-with-silent-modifications
     (org-with-wide-buffer
      ;; Set buffer-wide category.  Search last #+CATEGORY keyword.
      ;; This is the default category for the buffer.  If none is
      ;; found, fall-back to `org-category' or buffer file name.
      (put-text-property
       (point-min) (point-max)
       'org-category
       (catch 'buffer-category
	 (goto-char (point-max))
	 (while (re-search-backward "^[ \t]*#\\+CATEGORY:" (point-min) t)
	   (let ((element (org-element-at-point)))
	     (when (eq (org-element-type element) 'keyword)
	       (throw 'buffer-category
		      (org-element-property :value element)))))
	 default-category))
      ;; Set sub-tree specific categories.
      (goto-char (point-min))
      (let ((regexp (org-re-property "CATEGORY")))
	(while (re-search-forward regexp nil t)
	  (let ((value (match-string-no-properties 3)))
	    (when (org-at-property-p)
	      (put-text-property
	       (save-excursion (org-back-to-heading t) (point))
	       (save-excursion (org-end-of-subtree t t) (point))
	       'org-category
	       value)))))))))

(defun org-refresh-stats-properties ()
  "Refresh stats text properties in the buffer."
  (org-with-silent-modifications
   (org-with-point-at 1
     (let ((regexp (concat org-outline-regexp-bol
			   ".*\\[\\([0-9]*\\)\\(?:%\\|/\\([0-9]*\\)\\)\\]")))
       (while (re-search-forward regexp nil t)
	 (let* ((numerator (string-to-number (match-string 1)))
		(denominator (and (match-end 2)
				  (string-to-number (match-string 2))))
		(stats (cond ((not denominator) numerator) ;percent
			     ((= denominator 0) 0)
			     (t (/ (* numerator 100) denominator)))))
	   (put-text-property (point) (progn (org-end-of-subtree t t) (point))
			      'org-stats stats)))))))

(defun org-refresh-effort-properties ()
  "Refresh effort properties"
  (org-refresh-properties
   org-effort-property
   '((effort . identity)
     (effort-minutes . org-duration-to-minutes))))

;;;; Link Stuff

;;; Link abbreviations

(defun org-link-expand-abbrev (link)
  "Apply replacements as defined in `org-link-abbrev-alist'."
  (if (string-match "^\\([^:]*\\)\\(::?\\(.*\\)\\)?$" link)
      (let* ((key (match-string 1 link))
	     (as (or (assoc key org-link-abbrev-alist-local)
		     (assoc key org-link-abbrev-alist)))
	     (tag (and (match-end 2) (match-string 3 link)))
	     rpl)
	(if (not as)
	    link
	  (setq rpl (cdr as))
	  (cond
	   ((symbolp rpl) (funcall rpl tag))
	   ((string-match "%(\\([^)]+\\))" rpl)
	    (replace-match
	     (save-match-data
	       (funcall (intern-soft (match-string 1 rpl)) tag)) t t rpl))
	   ((string-match "%s" rpl) (replace-match (or tag "") t t rpl))
	   ((string-match "%h" rpl)
	    (replace-match (url-hexify-string (or tag "")) t t rpl))
	   (t (concat rpl tag)))))
    link))

;;; Storing and inserting links

(defvar org-insert-link-history nil
  "Minibuffer history for links inserted with `org-insert-link'.")

(defvar org-stored-links nil
  "Contains the links stored with `org-store-link'.")

(defvar org-store-link-plist nil
  "Plist with info about the most recently link created with `org-store-link'.")

(defun org-store-link-functions ()
  "Return a list of functions that are called to create and store a link.
The functions defined in the :store property of
`org-link-parameters'.

Each function will be called in turn until one returns a non-nil
value.  Each function should check if it is responsible for
creating this link (for example by looking at the major mode).
If not, it must exit and return nil.  If yes, it should return
a non-nil value after calling `org-store-link-props' with a list
of properties and values.  Special properties are:

:type         The link prefix, like \"http\".  This must be given.
:link         The link, like \"http://www.astro.uva.nl/~dominik\".
              This is obligatory as well.
:description  Optional default description for the second pair
              of brackets in an Org mode link.  The user can still change
              this when inserting this link into an Org mode buffer.

In addition to these, any additional properties can be specified
and then used in capture templates."
  (cl-loop for link in org-link-parameters
	   with store-func
	   do (setq store-func (org-link-get-parameter (car link) :store))
	   if store-func
	   collect store-func))

(defvar org-agenda-buffer-name) ; Defined in org-agenda.el
(defvar org-id-link-to-org-use-id) ; Defined in org-id.el

;;;###autoload
(defun org-store-link (arg)
  "Store an org-link to the current location.
\\<org-mode-map>
This link is added to `org-stored-links' and can later be inserted
into an Org buffer with `org-insert-link' (`\\[org-insert-link]').

For some link types, a `\\[universal-argument]' prefix ARG is interpreted.  \
A single
`\\[universal-argument]' negates `org-context-in-file-links' for file links or
`org-gnus-prefer-web-links' for links to Usenet articles.

A `\\[universal-argument] \\[universal-argument]' prefix ARG forces \
skipping storing functions that are not
part of Org core.

A `\\[universal-argument] \\[universal-argument] \\[universal-argument]' \
prefix ARG forces storing a link for each line in the
active region."
  (interactive "P")
  (org-load-modules-maybe)
  (if (and (equal arg '(64)) (org-region-active-p))
      (save-excursion
	(let ((end (region-end)))
	  (goto-char (region-beginning))
	  (set-mark (point))
	  (while (< (point-at-eol) end)
	    (move-end-of-line 1) (activate-mark)
	    (let (current-prefix-arg)
	      (call-interactively 'org-store-link))
	    (move-beginning-of-line 2)
	    (set-mark (point)))))
    (setq org-store-link-plist nil)
    (let (link cpltxt desc description search
	       txt custom-id agenda-link sfuns sfunsn)
      (cond

       ;; Store a link using an external link type
       ((and (not (equal arg '(16)))
	     (setq sfuns
		   (delq
		    nil (mapcar (lambda (f)
				  (let (fs) (if (funcall f) (push f fs))))
				(org-store-link-functions)))
		   sfunsn (mapcar (lambda (fu) (symbol-name (car fu))) sfuns))
	     (or (and (cdr sfuns)
		      (funcall (intern
				(completing-read
				 "Which function for creating the link? "
				 sfunsn nil t (car sfunsn)))))
		 (funcall (caar sfuns)))
	     (setq link (plist-get org-store-link-plist :link)
		   desc (or (plist-get org-store-link-plist
				       :description)
			    link))))

       ;; Store a link from a source code buffer.
       ((org-src-edit-buffer-p)
	(let ((coderef-format (org-src-coderef-format)))
	  (cond ((save-excursion
		   (beginning-of-line)
		   (looking-at (org-src-coderef-regexp coderef-format)))
		 (setq link (format "(%s)" (match-string-no-properties 3))))
		((called-interactively-p 'any)
		 (let ((label (read-string "Code line label: ")))
		   (end-of-line)
		   (setq link (format coderef-format label))
		   (let ((gc (- 79 (length link))))
		     (if (< (current-column) gc)
			 (org-move-to-column gc t)
		       (insert " ")))
		   (insert link)
		   (setq link (concat "(" label ")"))
		   (setq desc nil)))
		(t (setq link nil)))))

       ;; We are in the agenda, link to referenced location
       ((equal (bound-and-true-p org-agenda-buffer-name) (buffer-name))
	(let ((m (or (get-text-property (point) 'org-hd-marker)
		     (get-text-property (point) 'org-marker))))
	  (when m
	    (org-with-point-at m
	      (setq agenda-link
		    (if (called-interactively-p 'any)
			(call-interactively 'org-store-link)
		      (org-store-link nil)))))))

       ((eq major-mode 'calendar-mode)
	(let ((cd (calendar-cursor-to-date)))
	  (setq link
		(format-time-string
		 (car org-time-stamp-formats)
		 (apply 'encode-time
			(list 0 0 0 (nth 1 cd) (nth 0 cd) (nth 2 cd)
			      nil nil nil))))
	  (org-store-link-props :type "calendar" :date cd)))

       ((eq major-mode 'help-mode)
	(setq link (concat "help:" (save-excursion
				     (goto-char (point-min))
				     (looking-at "^[^ ]+")
				     (match-string 0))))
	(org-store-link-props :type "help"))

       ((eq major-mode 'w3-mode)
	(setq cpltxt (if (and (buffer-name)
			      (not (string-match "Untitled" (buffer-name))))
			 (buffer-name)
		       (url-view-url t))
	      link (url-view-url t))
	(org-store-link-props :type "w3" :url (url-view-url t)))

       ((eq major-mode 'image-mode)
	(setq cpltxt (concat "file:"
			     (abbreviate-file-name buffer-file-name))
	      link cpltxt)
	(org-store-link-props :type "image" :file buffer-file-name))

       ;; In dired, store a link to the file of the current line
       ((derived-mode-p 'dired-mode)
	(let ((file (dired-get-filename nil t)))
	  (setq file (if file
			 (abbreviate-file-name
			  (expand-file-name (dired-get-filename nil t)))
		       ;; otherwise, no file so use current directory.
		       default-directory))
	  (setq cpltxt (concat "file:" file)
		link cpltxt)))

       ((setq search (run-hook-with-args-until-success
		      'org-create-file-search-functions))
	(setq link (concat "file:" (abbreviate-file-name buffer-file-name)
			   "::" search))
	(setq cpltxt (or description link)))

       ((and (buffer-file-name (buffer-base-buffer)) (derived-mode-p 'org-mode))
	(org-with-limited-levels
	 (setq custom-id (org-entry-get nil "CUSTOM_ID"))
	 (cond
	  ;; Store a link using the target at point
	  ((org-in-regexp "[^<]<<\\([^<>]+\\)>>[^>]" 1)
	   (setq cpltxt
		 (concat "file:"
			 (abbreviate-file-name
			  (buffer-file-name (buffer-base-buffer)))
			 "::" (match-string 1))
		 link cpltxt))
	  ((and (featurep 'org-id)
		(or (eq org-id-link-to-org-use-id t)
		    (and (called-interactively-p 'any)
			 (or (eq org-id-link-to-org-use-id 'create-if-interactive)
			     (and (eq org-id-link-to-org-use-id
				      'create-if-interactive-and-no-custom-id)
				  (not custom-id))))
		    (and org-id-link-to-org-use-id (org-entry-get nil "ID"))))
	   ;; Store a link using the ID at point
	   (setq link (condition-case nil
			  (prog1 (org-id-store-link)
			    (setq desc (or (plist-get org-store-link-plist
						      :description)
					   "")))
			(error
			 ;; Probably before first headline, link only to file
			 (concat "file:"
				 (abbreviate-file-name
				  (buffer-file-name (buffer-base-buffer))))))))
	  (t
	   ;; Just link to current headline
	   (setq cpltxt (concat "file:"
				(abbreviate-file-name
				 (buffer-file-name (buffer-base-buffer)))))
	   ;; Add a context search string
	   (when (org-xor org-context-in-file-links
			  (equal arg '(4)))
	     (let* ((element (org-element-at-point))
		    (name (org-element-property :name element)))
	       (setq txt (cond
			  ((org-at-heading-p) nil)
			  (name)
			  ((org-region-active-p)
			   (buffer-substring (region-beginning) (region-end)))))
	       (when (or (null txt) (string-match "\\S-" txt))
		 (setq cpltxt
		       (concat cpltxt "::"
			       (condition-case nil
				   (org-make-org-heading-search-string txt)
				 (error "")))
		       desc (or name
				(nth 4 (ignore-errors (org-heading-components)))
				"NONE")))))
	   (when (string-match "::\\'" cpltxt)
	     (setq cpltxt (substring cpltxt 0 -2)))
	   (setq link cpltxt)))))

       ((buffer-file-name (buffer-base-buffer))
	;; Just link to this file here.
	(setq cpltxt (concat "file:"
			     (abbreviate-file-name
			      (buffer-file-name (buffer-base-buffer)))))
	;; Add a context string.
	(when (org-xor org-context-in-file-links
		       (equal arg '(4)))
	  (setq txt (if (org-region-active-p)
			(buffer-substring (region-beginning) (region-end))
		      (buffer-substring (point-at-bol) (point-at-eol))))
	  ;; Only use search option if there is some text.
	  (when (string-match "\\S-" txt)
	    (setq cpltxt
		  (concat cpltxt "::" (org-make-org-heading-search-string txt))
		  desc "NONE")))
	(setq link cpltxt))

       ((called-interactively-p 'interactive)
	(user-error "No method for storing a link from this buffer"))

       (t (setq link nil)))

      ;; We're done setting link and desc, clean up
      (when (consp link) (setq cpltxt (car link) link (cdr link)))
      (setq link (or link cpltxt)
	    desc (or desc cpltxt))
      (cond ((not desc))
	    ((equal desc "NONE") (setq desc nil))
	    (t (setq desc
		     (replace-regexp-in-string
		      org-bracket-link-analytic-regexp
		      (lambda (m) (or (match-string 5 m) (match-string 3 m)))
		      desc))))
      ;; Return the link
      (if (not (and (or (called-interactively-p 'any)
			executing-kbd-macro)
		    link))
	  (or agenda-link (and link (org-make-link-string link desc)))
	(push (list link desc) org-stored-links)
	(message "Stored: %s" (or desc link))
	(when custom-id
	  (setq link (concat "file:" (abbreviate-file-name
				      (buffer-file-name)) "::#" custom-id))
	  (push (list link desc) org-stored-links))
	(car org-stored-links)))))

(defun org-store-link-props (&rest plist)
  "Store link properties.
The properties are pre-processed by extracting names, addresses
and dates."
  (let ((x (plist-get plist :from)))
    (when x
      (let ((adr (mail-extract-address-components x)))
	(setq plist (plist-put plist :fromname (car adr)))
	(setq plist (plist-put plist :fromaddress (nth 1 adr))))))
  (let ((x (plist-get plist :to)))
    (when x
      (let ((adr (mail-extract-address-components x)))
	(setq plist (plist-put plist :toname (car adr)))
	(setq plist (plist-put plist :toaddress (nth 1 adr))))))
  (let ((x (ignore-errors (date-to-time (plist-get plist :date)))))
    (when x
      (setq plist (plist-put plist :date-timestamp
			     (format-time-string
			      (org-time-stamp-format t) x)))
      (setq plist (plist-put plist :date-timestamp-inactive
			     (format-time-string
			      (org-time-stamp-format t t) x)))))
  (let ((from (plist-get plist :from))
	(to (plist-get plist :to)))
    (when (and from to org-from-is-user-regexp)
      (setq plist
	    (plist-put plist :fromto
		       (if (string-match org-from-is-user-regexp from)
			   (concat "to %t")
			 (concat "from %f"))))))
  (setq org-store-link-plist plist))

(defun org-add-link-props (&rest plist)
  "Add these properties to the link property list."
  (let (key value)
    (while plist
      (setq key (pop plist) value (pop plist))
      (setq org-store-link-plist
	    (plist-put org-store-link-plist key value)))))

(defun org-email-link-description (&optional fmt)
  "Return the description part of an email link.
This takes information from `org-store-link-plist' and formats it
according to FMT (default from `org-email-link-description-format')."
  (setq fmt (or fmt org-email-link-description-format))
  (let* ((p org-store-link-plist)
	 (to (plist-get p :toaddress))
	 (from (plist-get p :fromaddress))
	 (table
	  (list
	   (cons "%c" (plist-get p :fromto))
	   (cons "%F" (plist-get p :from))
	   (cons "%f" (or (plist-get p :fromname) (plist-get p :fromaddress) "?"))
	   (cons "%T" (plist-get p :to))
	   (cons "%t" (or (plist-get p :toname) (plist-get p :toaddress) "?"))
	   (cons "%s" (plist-get p :subject))
	   (cons "%d" (plist-get p :date))
	   (cons "%m" (plist-get p :message-id)))))
    (when (string-match "%c" fmt)
      ;; Check if the user wrote this message
      (if (and org-from-is-user-regexp from to
	       (save-match-data (string-match org-from-is-user-regexp from)))
	  (setq fmt (replace-match "to %t" t t fmt))
	(setq fmt (replace-match "from %f" t t fmt))))
    (org-replace-escapes fmt table)))

(defun org-make-org-heading-search-string (&optional string)
  "Make search string for the current headline or STRING."
  (let ((s (or string
	       (and (derived-mode-p 'org-mode)
		    (save-excursion
		      (org-back-to-heading t)
		      (org-element-property :raw-value (org-element-at-point))))))
	(lines org-context-in-file-links))
    (unless string (setq s (concat "*" s))) ;Add * for headlines
    (setq s (replace-regexp-in-string "\\[[0-9]+%\\]\\|\\[[0-9]+/[0-9]+\\]" "" s))
    (when (and string (integerp lines) (> lines 0))
      (let ((slines (org-split-string s "\n")))
	(when (< lines (length slines))
	  (setq s (mapconcat
		   'identity
		   (reverse (nthcdr (- (length slines) lines)
				    (reverse slines))) "\n")))))
    (mapconcat #'identity (split-string s) " ")))

(defun org-make-link-string (link &optional description)
  "Make a link with brackets, consisting of LINK and DESCRIPTION."
  (unless (org-string-nw-p link) (error "Empty link"))
  (let ((uri (cond ((string-match org-link-types-re link)
		    (concat (match-string 1 link)
			    (org-link-escape (substring link (match-end 1)))))
		   ;; For readability, url-encode internal links only
		   ;; when absolutely needed (i.e, when they contain
		   ;; square brackets).  File links however, are
		   ;; encoded since, e.g., spaces are significant.
		   ((or (file-name-absolute-p link)
			(string-match-p "\\`\\.\\.?/\\|[][]" link))
		    (org-link-escape link))
		   (t link)))
	(description
	 (and (org-string-nw-p description)
	      ;; Remove brackets from description, as they are fatal.
	      (replace-regexp-in-string
	       "[][]" (lambda (m) (if (equal "[" m) "{" "}"))
	       (org-trim description)))))
    (format "[[%s]%s]"
	    uri
	    (if description (format "[%s]" description) ""))))

(defconst org-link-escape-chars
  ;;%20 %5B %5D %25
  '(?\s ?\[ ?\] ?%)
  "List of characters that should be escaped in a link when stored to Org.
This is the list that is used for internal purposes.")

(defun org-link-escape (text &optional table merge)
  "Return percent escaped representation of TEXT.
TEXT is a string with the text to escape.
Optional argument TABLE is a list with characters that should be
escaped.  When nil, `org-link-escape-chars' is used.
If optional argument MERGE is set, merge TABLE into
`org-link-escape-chars'."
  (let ((characters-to-encode
	 (cond ((null table) org-link-escape-chars)
	       (merge (append org-link-escape-chars table))
	       (t table))))
    (mapconcat
     (lambda (c)
       (if (or (memq c characters-to-encode)
	       (and org-url-hexify-p (or (< c 32) (> c 126))))
	   (mapconcat (lambda (e) (format "%%%.2X" e))
		      (or (encode-coding-char c 'utf-8)
			  (error "Unable to percent escape character: %c" c))
		      "")
	 (char-to-string c)))
     text "")))

(defun org-link-unescape (str)
  "Unhex hexified Unicode parts in string STR.
E.g. `%C3%B6' becomes the german o-Umlaut.  This is the
reciprocal of `org-link-escape', which see."
  (if (org-string-nw-p str)
      (replace-regexp-in-string
       "\\(%[0-9A-Za-z]\\{2\\}\\)+" #'org-link-unescape-compound str t t)
    str))

(defun org-link-unescape-compound (hex)
  "Unhexify Unicode hex-chars.  E.g. `%C3%B6' is the German o-Umlaut.
Note: this function also decodes single byte encodings like
`%E1' (a-acute) if not followed by another `%[A-F0-9]{2}' group."
  (save-match-data
    (let* ((bytes (cdr (split-string hex "%")))
	   (ret "")
	   (eat 0)
	   (sum 0))
      (while bytes
	(let* ((val (string-to-number (pop bytes) 16))
	       (shift-xor
		(if (= 0 eat)
		    (cond
		     ((>= val 252) (cons 6 252))
		     ((>= val 248) (cons 5 248))
		     ((>= val 240) (cons 4 240))
		     ((>= val 224) (cons 3 224))
		     ((>= val 192) (cons 2 192))
		     (t (cons 0 0)))
		  (cons 6 128))))
	  (when (>= val 192) (setq eat (car shift-xor)))
	  (setq val (logxor val (cdr shift-xor)))
	  (setq sum (+ (lsh sum (car shift-xor)) val))
	  (when (> eat 0) (setq eat (- eat 1)))
	  (cond
	   ((= 0 eat)			;multi byte
	    (setq ret (concat ret (char-to-string sum)))
	    (setq sum 0))
	   ((not bytes)			; single byte(s)
	    (setq ret (org-link-unescape-single-byte-sequence hex))))))
      ret)))

(defun org-link-unescape-single-byte-sequence (hex)
  "Unhexify hex-encoded single byte character sequences."
  (mapconcat (lambda (byte)
	       (char-to-string (string-to-number byte 16)))
	     (cdr (split-string hex "%")) ""))

(defun org-xor (a b)
  "Exclusive or."
  (if a (not b) b))

(defun org-fixup-message-id-for-http (s)
  "Replace special characters in a message id, so it can be used in an http query."
  (when (string-match "%" s)
    (setq s (mapconcat (lambda (c)
			 (if (eq c ?%)
			     "%25"
			   (char-to-string c)))
		       s "")))
  (while (string-match "<" s)
    (setq s (replace-match "%3C" t t s)))
  (while (string-match ">" s)
    (setq s (replace-match "%3E" t t s)))
  (while (string-match "@" s)
    (setq s (replace-match "%40" t t s)))
  s)

(defun org-link-prettify (link)
  "Return a human-readable representation of LINK.
The car of LINK must be a raw link.
The cdr of LINK must be either a link description or nil."
  (let ((desc (or (cadr link) "<no description>")))
    (concat (format "%-45s" (substring desc 0 (min (length desc) 40)))
	    "<" (car link) ">")))

;;;###autoload
(defun org-insert-link-global ()
  "Insert a link like Org mode does.
This command can be called in any mode to insert a link in Org syntax."
  (interactive)
  (org-load-modules-maybe)
  (org-run-like-in-org-mode 'org-insert-link))

(defun org-insert-all-links (arg &optional pre post)
  "Insert all links in `org-stored-links'.
When a universal prefix, do not delete the links from `org-stored-links'.
When `ARG' is a number, insert the last N link(s).
`PRE' and `POST' are optional arguments to define a string to
prepend or to append."
  (interactive "P")
  (let ((org-keep-stored-link-after-insertion (equal arg '(4)))
	(links (copy-sequence org-stored-links))
	(pr (or pre "- "))
	(po (or post "\n"))
	(cnt 1) l)
    (if (null org-stored-links)
	(message "No link to insert")
      (while (and (or (listp arg) (>= arg cnt))
		  (setq l (if (listp arg)
			      (pop links)
			    (pop org-stored-links))))
	(setq cnt (1+ cnt))
	(insert pr)
	(org-insert-link nil (car l) (or (cadr l) "<no description>"))
	(insert po)))))

(defun org-insert-last-stored-link (arg)
  "Insert the last link stored in `org-stored-links'."
  (interactive "p")
  (org-insert-all-links arg "" "\n"))

(defun org-link-fontify-links-to-this-file ()
  "Fontify links to the current file in `org-stored-links'."
  (let ((f (buffer-file-name)) a b)
    (setq a (mapcar (lambda(l)
		      (let ((ll (car l)))
			(when (and (string-match "^file:\\(.+\\)::" ll)
				   (equal f (expand-file-name (match-string 1 ll))))
			  ll)))
		    org-stored-links))
    (when (featurep 'org-id)
      (setq b (mapcar (lambda(l)
			(let ((ll (car l)))
			  (when (and (string-match "^id:\\(.+\\)$" ll)
				     (equal f (expand-file-name
					       (or (org-id-find-id-file
						    (match-string 1 ll)) ""))))
			    ll)))
		      org-stored-links)))
    (mapcar (lambda(l)
	      (put-text-property 0 (length l) 'face 'font-lock-comment-face l))
	    (delq nil (append a b)))))

(defvar org--links-history nil)
(defun org-insert-link (&optional complete-file link-location default-description)
  "Insert a link.  At the prompt, enter the link.

Completion can be used to insert any of the link protocol prefixes in use.

The history can be used to select a link previously stored with
`org-store-link'.  When the empty string is entered (i.e. if you just
press `RET' at the prompt), the link defaults to the most recently
stored link.  As `SPC' triggers completion in the minibuffer, you need to
use `M-SPC' or `C-q SPC' to force the insertion of a space character.

You will also be prompted for a description, and if one is given, it will
be displayed in the buffer instead of the link.

If there is already a link at point, this command will allow you to edit
link and description parts.

With a `\\[universal-argument]' prefix, prompts for a file to link to.  The \
file name can be
selected using completion.  The path to the file will be relative to the
current directory if the file is in the current directory or a subdirectory.
Otherwise, the link will be the absolute path as completed in the minibuffer
\(i.e. normally ~/path/to/file).  You can configure this behavior using the
option `org-link-file-path-type'.

With a `\\[universal-argument] \\[universal-argument]' prefix, enforce an \
absolute path even if the file is in
the current directory or below.

A `\\[universal-argument] \\[universal-argument] \\[universal-argument]' \
prefix negates `org-keep-stored-link-after-insertion'.

If the LINK-LOCATION parameter is non-nil, this value will be used as
the link location instead of reading one interactively.

If the DEFAULT-DESCRIPTION parameter is non-nil, this value will
be used as the default description.  Otherwise, if
`org-make-link-description-function' is non-nil, this function
will be called with the link target, and the result will be the
default link description."
  (interactive "P")
  (let* ((wcf (current-window-configuration))
	 (origbuf (current-buffer))
	 (region (when (org-region-active-p)
		   (buffer-substring (region-beginning) (region-end))))
	 (remove (and region (list (region-beginning) (region-end))))
	 (desc region)
	 (link link-location)
	 (abbrevs org-link-abbrev-alist-local)
	 entry all-prefixes auto-desc)
    (cond
     (link-location)		      ; specified by arg, just use it.
     ((org-in-regexp org-bracket-link-regexp 1)
      ;; We do have a link at point, and we are going to edit it.
      (setq remove (list (match-beginning 0) (match-end 0)))
      (setq desc (when (match-end 3) (match-string-no-properties 3)))
      (setq link (read-string "Link: "
			      (org-link-unescape
			       (match-string-no-properties 1)))))
     ((or (org-in-regexp org-angle-link-re)
	  (org-in-regexp org-plain-link-re))
      ;; Convert to bracket link
      (setq remove (list (match-beginning 0) (match-end 0))
	    link (read-string "Link: "
			      (org-unbracket-string "<" ">" (match-string 0)))))
     ((member complete-file '((4) (16)))
      ;; Completing read for file names.
      (setq link (org-file-complete-link complete-file)))
     (t
      ;; Read link, with completion for stored links.
      (org-link-fontify-links-to-this-file)
      (org-switch-to-buffer-other-window "*Org Links*")
      (with-current-buffer "*Org Links*"
	(erase-buffer)
	(insert "Insert a link.
Use TAB to complete link prefixes, then RET for type-specific completion support\n")
	(when org-stored-links
	  (insert "\nStored links are available with <up>/<down> or M-p/n (most recent with RET):\n\n")
	  (insert (mapconcat 'org-link-prettify
			     (reverse org-stored-links) "\n")))
	(goto-char (point-min)))
      (let ((cw (selected-window)))
	(select-window (get-buffer-window "*Org Links*" 'visible))
	(with-current-buffer "*Org Links*" (setq truncate-lines t))
	(unless (pos-visible-in-window-p (point-max))
	  (org-fit-window-to-buffer))
	(and (window-live-p cw) (select-window cw)))
      (setq all-prefixes (append (mapcar 'car abbrevs)
				 (mapcar 'car org-link-abbrev-alist)
				 (org-link-types)))
      (unwind-protect
	  ;; Fake a link history, containing the stored links.
	  (let ((org--links-history
		 (append (mapcar #'car org-stored-links)
			 org-insert-link-history)))
	    (setq link
		  (org-completing-read
		   "Link: "
		   (append
		    (mapcar (lambda (x) (concat x ":")) all-prefixes)
		    (mapcar #'car org-stored-links))
		   nil nil nil
		   'org--links-history
		   (caar org-stored-links)))
	    (unless (org-string-nw-p link) (user-error "No link selected"))
	    (dolist (l org-stored-links)
	      (when (equal link (cadr l))
		(setq link (car l))
		(setq auto-desc t)))
	    (when (or (member link all-prefixes)
		      (and (equal ":" (substring link -1))
			   (member (substring link 0 -1) all-prefixes)
			   (setq link (substring link 0 -1))))
	      (setq link (with-current-buffer origbuf
			   (org-link-try-special-completion link)))))
	(set-window-configuration wcf)
	(kill-buffer "*Org Links*"))
      (setq entry (assoc link org-stored-links))
      (or entry (push link org-insert-link-history))
      (setq desc (or desc (nth 1 entry)))))

    (when (funcall (if (equal complete-file '(64)) 'not 'identity)
		   (not org-keep-stored-link-after-insertion))
      (setq org-stored-links (delq (assoc link org-stored-links)
				   org-stored-links)))

    (when (and (string-match org-plain-link-re link)
	       (not (string-match org-ts-regexp link)))
      ;; URL-like link, normalize the use of angular brackets.
      (setq link (org-unbracket-string "<" ">" link)))

    ;; Check if we are linking to the current file with a search
    ;; option If yes, simplify the link by using only the search
    ;; option.
    (when (and buffer-file-name
	       (let ((case-fold-search nil))
		 (string-match "\\`file:\\(.+?\\)::" link)))
      (let ((path (match-string-no-properties 1 link))
	    (search (substring-no-properties link (match-end 0))))
	(save-match-data
	  (when (equal (file-truename buffer-file-name) (file-truename path))
	    ;; We are linking to this same file, with a search option
	    (setq link search)))))

    ;; Check if we can/should use a relative path.  If yes, simplify the link
    (let ((case-fold-search nil))
      (when (string-match "\\`\\(file\\|docview\\):" link)
	(let* ((type (match-string-no-properties 0 link))
	       (path (substring-no-properties link (match-end 0)))
	       (origpath path))
	  (cond
	   ((or (eq org-link-file-path-type 'absolute)
		(equal complete-file '(16)))
	    (setq path (abbreviate-file-name (expand-file-name path))))
	   ((eq org-link-file-path-type 'noabbrev)
	    (setq path (expand-file-name path)))
	   ((eq org-link-file-path-type 'relative)
	    (setq path (file-relative-name path)))
	   (t
	    (save-match-data
	      (if (string-match (concat "^" (regexp-quote
					     (expand-file-name
					      (file-name-as-directory
					       default-directory))))
				(expand-file-name path))
		  ;; We are linking a file with relative path name.
		  (setq path (substring (expand-file-name path)
					(match-end 0)))
		(setq path (abbreviate-file-name (expand-file-name path)))))))
	  (setq link (concat type path))
	  (when (equal desc origpath)
	    (setq desc path)))))

    (unless auto-desc
      (let ((initial-input
	     (cond
	      (default-description)
	      ((not org-make-link-description-function) desc)
	      (t (condition-case nil
		     (funcall org-make-link-description-function link desc)
		   (error
		    (message "Can't get link description from `%s'"
			     (symbol-name org-make-link-description-function))
		    (sit-for 2)
		    nil))))))
	(setq desc (read-string "Description: " initial-input))))

    (unless (string-match "\\S-" desc) (setq desc nil))
    (when remove (apply 'delete-region remove))
    (insert (org-make-link-string link desc))
    ;; Redisplay so as the new link has proper invisible characters.
    (sit-for 0)))

(defun org-link-try-special-completion (type)
  "If there is completion support for link type TYPE, offer it."
  (let ((fun (org-link-get-parameter type :complete)))
    (if (functionp fun)
	(funcall fun)
      (read-string "Link (no completion support): " (concat type ":")))))

(defun org-file-complete-link (&optional arg)
  "Create a file link using completion."
  (let ((file (read-file-name "File: "))
	(pwd (file-name-as-directory (expand-file-name ".")))
	(pwd1 (file-name-as-directory (abbreviate-file-name
				       (expand-file-name ".")))))
    (cond ((equal arg '(16))
	   (concat "file:"
		   (abbreviate-file-name (expand-file-name file))))
	  ((string-match
	    (concat "^" (regexp-quote pwd1) "\\(.+\\)") file)
	   (concat "file:" (match-string 1 file)))
	  ((string-match
	    (concat "^" (regexp-quote pwd) "\\(.+\\)")
	    (expand-file-name file))
	   (concat "file:"
		   (match-string 1 (expand-file-name file))))
	  (t (concat "file:" file)))))

(defun org-completing-read (&rest args)
  "Completing-read with SPACE being a normal character."
  (let ((enable-recursive-minibuffers t)
	(minibuffer-local-completion-map
	 (copy-keymap minibuffer-local-completion-map)))
    (org-defkey minibuffer-local-completion-map " " 'self-insert-command)
    (org-defkey minibuffer-local-completion-map "?" 'self-insert-command)
    (org-defkey minibuffer-local-completion-map (kbd "C-c !")
		'org-time-stamp-inactive)
    (apply #'completing-read args)))

;;; Opening/following a link

(defvar org-link-search-failed nil)

(defvar org-open-link-functions nil
  "Hook for functions finding a plain text link.
These functions must take a single argument, the link content.
They will be called for links that look like [[link text][description]]
when LINK TEXT does not have a protocol like \"http:\" and does not look
like a filename (e.g. \"./blue.png\").

These functions will be called *before* Org attempts to resolve the
link by doing text searches in the current buffer - so if you want a
link \"[[target]]\" to still find \"<<target>>\", your function should
handle this as a special case.

When the function does handle the link, it must return a non-nil value.
If it decides that it is not responsible for this link, it must return
nil to indicate that that Org can continue with other options like
exact and fuzzy text search.")

(defun org-next-link (&optional search-backward)
  "Move forward to the next link.
If the link is in hidden text, expose it."
  (interactive "P")
  (when (and org-link-search-failed (eq this-command last-command))
    (goto-char (point-min))
    (message "Link search wrapped back to beginning of buffer"))
  (setq org-link-search-failed nil)
  (let* ((pos (point))
	 (ct (org-context))
	 (a (assq :link ct))
	 (srch-fun (if search-backward 're-search-backward 're-search-forward)))
    (cond (a (goto-char (nth (if search-backward 1 2) a)))
	  ((looking-at org-any-link-re)
	   ;; Don't stay stuck at link without an org-link face
	   (forward-char (if search-backward -1 1))))
    (if (funcall srch-fun org-any-link-re nil t)
	(progn
	  (goto-char (match-beginning 0))
	  (when (org-invisible-p) (org-show-context)))
      (goto-char pos)
      (setq org-link-search-failed t)
      (message "No further link found"))))

(defun org-previous-link ()
  "Move backward to the previous link.
If the link is in hidden text, expose it."
  (interactive)
  (funcall 'org-next-link t))

(defun org-translate-link (s)
  "Translate a link string if a translation function has been defined."
  (with-temp-buffer
    (insert (org-trim s))
    (org-trim (org-element-interpret-data (org-element-context)))))

(defun org-translate-link-from-planner (type path)
  "Translate a link from Emacs Planner syntax so that Org can follow it.
This is still an experimental function, your mileage may vary."
  (cond
   ((member type '("http" "https" "news" "ftp"))
    ;; standard Internet links are the same.
    nil)
   ((and (equal type "irc") (string-match "^//" path))
    ;; Planner has two / at the beginning of an irc link, we have 1.
    ;; We should have zero, actually....
    (setq path (substring path 1)))
   ((and (equal type "lisp") (string-match "^/" path))
    ;; Planner has a slash, we do not.
    (setq type "elisp" path (substring path 1)))
   ((string-match "^//\\(.?*\\)/\\(<.*>\\)$" path)
    ;; A typical message link.  Planner has the id after the final slash,
    ;; we separate it with a hash mark
    (setq path (concat (match-string 1 path) "#"
		       (org-unbracket-string "<" ">" (match-string 2 path))))))
  (cons type path))

(defun org-find-file-at-mouse (ev)
  "Open file link or URL at mouse."
  (interactive "e")
  (mouse-set-point ev)
  (org-open-at-point 'in-emacs))

(defun org-open-at-mouse (ev)
  "Open file link or URL at mouse.
See the docstring of `org-open-file' for details."
  (interactive "e")
  (mouse-set-point ev)
  (when (eq major-mode 'org-agenda-mode)
    (org-agenda-copy-local-variable 'org-link-abbrev-alist-local))
  (org-open-at-point))

(defvar org-window-config-before-follow-link nil
  "The window configuration before following a link.
This is saved in case the need arises to restore it.")

;;;###autoload
(defun org-open-at-point-global ()
  "Follow a link or time-stamp like Org mode does.
This command can be called in any mode to follow an external link
or a time-stamp that has Org mode syntax.  Its behavior is
undefined when called on internal links (e.g., fuzzy links).
Raise an error when there is nothing to follow.  "
  (interactive)
  (cond ((org-in-regexp org-any-link-re)
	 (org-open-link-from-string (match-string-no-properties 0)))
	((or (org-in-regexp org-ts-regexp-both nil t)
	     (org-in-regexp org-tsr-regexp-both nil t))
	 (org-follow-timestamp-link))
	(t (user-error "No link found"))))

;;;###autoload
(defun org-open-link-from-string (s &optional arg reference-buffer)
  "Open a link in the string S, as if it was in Org mode."
  (interactive "sLink: \nP")
  (let ((reference-buffer (or reference-buffer (current-buffer))))
    (with-temp-buffer
      (let ((org-inhibit-startup (not reference-buffer)))
	(org-mode)
	(insert s)
	(goto-char (point-min))
	(when reference-buffer
	  (setq org-link-abbrev-alist-local
		(with-current-buffer reference-buffer
		  org-link-abbrev-alist-local)))
	(org-open-at-point arg reference-buffer)))))

(defvar org-open-at-point-functions nil
  "Hook that is run when following a link at point.

Functions in this hook must return t if they identify and follow
a link at point.  If they don't find anything interesting at point,
they must return nil.")

(defvar org-link-search-inhibit-query nil)
(defvar clean-buffer-list-kill-buffer-names) ;Defined in midnight.el
(defun org--open-doi-link (path)
  "Open a \"doi\" type link.
PATH is a the path to search for, as a string."
  (browse-url (url-encode-url (concat org-doi-server-url path))))

(defun org--open-elisp-link (path)
  "Open a \"elisp\" type link.
PATH is the sexp to evaluate, as a string."
  (let ((cmd path))
    (if (or (and (org-string-nw-p
		  org-confirm-elisp-link-not-regexp)
		 (string-match-p org-confirm-elisp-link-not-regexp cmd))
	    (not org-confirm-elisp-link-function)
	    (funcall org-confirm-elisp-link-function
		     (format "Execute \"%s\" as elisp? "
			     (org-add-props cmd nil 'face 'org-warning))))
	(message "%s => %s" cmd
		 (if (eq (string-to-char cmd) ?\()
		     (eval (read cmd))
		   (call-interactively (read cmd))))
      (user-error "Abort"))))

(defun org--open-help-link (path)
  "Open a \"help\" type link.
PATH is a symbol name, as a string."
  (pcase (intern path)
    ((and (pred fboundp) variable) (describe-function variable))
    ((and (pred boundp) function) (describe-variable function))
    (name (user-error "Unknown function or variable: %s" name))))

(defun org--open-shell-link (path)
  "Open a \"shell\" type link.
PATH is the command to execute, as a string."
  (let ((buf (generate-new-buffer "*Org Shell Output*"))
	(cmd path))
    (if (or (and (org-string-nw-p
		  org-confirm-shell-link-not-regexp)
		 (string-match
		  org-confirm-shell-link-not-regexp cmd))
	    (not org-confirm-shell-link-function)
	    (funcall org-confirm-shell-link-function
		     (format "Execute \"%s\" in shell? "
			     (org-add-props cmd nil
			       'face 'org-warning))))
	(progn
	  (message "Executing %s" cmd)
	  (shell-command cmd buf)
	  (when (featurep 'midnight)
	    (setq clean-buffer-list-kill-buffer-names
		  (cons (buffer-name buf)
			clean-buffer-list-kill-buffer-names))))
      (user-error "Abort"))))

(defun org-open-at-point (&optional arg reference-buffer)
  "Open link, timestamp, footnote or tags at point.

When point is on a link, follow it.  Normally, files will be
opened by an appropriate application.  If the optional prefix
argument ARG is non-nil, Emacs will visit the file.  With
a double prefix argument, try to open outside of Emacs, in the
application the system uses for this file type.

When point is on a timestamp, open the agenda at the day
specified.

When point is a footnote definition, move to the first reference
found.  If it is on a reference, move to the associated
definition.

When point is on a headline, display a list of every link in the
entry, so it is possible to pick one, or all, of them.  If point
is on a tag, call `org-tags-view' instead.

When optional argument REFERENCE-BUFFER is non-nil, it should
specify a buffer from where the link search should happen.  This
is used internally by `org-open-link-from-string'.

On top of syntactically correct links, this function will also
try to open links and time-stamps in comments, example
blocks... i.e., whenever point is on something looking like
a timestamp or a link."
  (interactive "P")
  ;; On a code block, open block's results.
  (unless (call-interactively 'org-babel-open-src-block-result)
    (org-load-modules-maybe)
    (setq org-window-config-before-follow-link (current-window-configuration))
    (org-remove-occur-highlights nil nil t)
    (unless (run-hook-with-args-until-success 'org-open-at-point-functions)
      (let* ((context
	      ;; Only consider supported types, even if they are not
	      ;; the closest one.
	      (org-element-lineage
	       (org-element-context)
	       '(clock footnote-definition footnote-reference headline
		       inlinetask link timestamp)
	       t))
	     (type (org-element-type context))
	     (value (org-element-property :value context)))
	(cond
	 ;; On a headline or an inlinetask, but not on a timestamp,
	 ;; a link, a footnote reference.
	 ((memq type '(headline inlinetask))
	  (org-match-line org-complex-heading-regexp)
	  (if (and (match-beginning 5)
		   (>= (point) (match-beginning 5))
		   (< (point) (match-end 5)))
	      ;; On tags.
	      (org-tags-view arg (substring (match-string 5) 0 -1))
	    ;; Not on tags.
	    (pcase (org-offer-links-in-entry (current-buffer) (point) arg)
	      (`(nil . ,_)
	       (require 'org-attach)
	       (org-attach-reveal 'if-exists))
	      (`(,links . ,links-end)
	       (dolist (link (if (stringp links) (list links) links))
		 (search-forward link nil links-end)
		 (goto-char (match-beginning 0))
		 (org-open-at-point))))))
	 ;; On a footnote reference or at definition's label.
	 ((or (eq type 'footnote-reference)
	      (and (eq type 'footnote-definition)
		   (save-excursion
		     ;; Do not validate action when point is on the
		     ;; spaces right after the footnote label, in
		     ;; order to be on par with behaviour on links.
		     (skip-chars-forward " \t")
		     (let ((begin
			    (org-element-property :contents-begin context)))
		       (if begin (< (point) begin)
			 (= (org-element-property :post-affiliated context)
			    (line-beginning-position)))))))
	  (org-footnote-action))
	 ;; No valid context.  Ignore catch-all types like `headline'.
	 ;; If point is on something looking like a link or
	 ;; a time-stamp, try opening it.  It may be useful in
	 ;; comments, example blocks...
	 ((memq type '(footnote-definition headline inlinetask nil))
	  (call-interactively #'org-open-at-point-global))
	 ;; On a clock line, make sure point is on the timestamp
	 ;; before opening it.
	 ((and (eq type 'clock)
	       value
	       (>= (point) (org-element-property :begin value))
	       (<= (point) (org-element-property :end value)))
	  (org-follow-timestamp-link))
	 ;; Do nothing on white spaces after an object.
	 ((>= (point)
	      (save-excursion
		(goto-char (org-element-property :end context))
		(skip-chars-backward " \t")
		(point)))
	  (user-error "No link found"))
	 ((eq type 'timestamp) (org-follow-timestamp-link))
	 ((eq type 'link)
	  (let ((type (org-element-property :type context))
		(path (org-link-unescape (org-element-property :path context))))
	    ;; Switch back to REFERENCE-BUFFER needed when called in
	    ;; a temporary buffer through `org-open-link-from-string'.
	    (with-current-buffer (or reference-buffer (current-buffer))
	      (cond
	       ((equal type "file")
		(if (string-match "[*?{]" (file-name-nondirectory path))
		    (dired path)
		  ;; Look into `org-link-parameters' in order to find
		  ;; a DEDICATED-FUNCTION to open file.  The function
		  ;; will be applied on raw link instead of parsed
		  ;; link due to the limitation in `org-add-link-type'
		  ;; ("open" function called with a single argument).
		  ;; If no such function is found, fallback to
		  ;; `org-open-file'.
		  (let* ((option (org-element-property :search-option context))
			 (app (org-element-property :application context))
			 (dedicated-function
			  (org-link-get-parameter
			   (if app (concat type "+" app) type)
			   :follow)))
		    (if dedicated-function
			(funcall dedicated-function
				 (concat path
					 (and option (concat "::" option))))
		      (apply #'org-open-file
			     path
			     (cond (arg)
				   ((equal app "emacs") 'emacs)
				   ((equal app "sys") 'system))
			     (cond ((not option) nil)
				   ((string-match-p "\\`[0-9]+\\'" option)
				    (list (string-to-number option)))
				   (t (list nil
					    (org-link-unescape option)))))))))
	       ((functionp (org-link-get-parameter type :follow))
		(funcall (org-link-get-parameter type :follow) path))
	       ((member type '("coderef" "custom-id" "fuzzy" "radio"))
		(unless (run-hook-with-args-until-success
			 'org-open-link-functions path)
		  (if (not arg) (org-mark-ring-push)
		    (switch-to-buffer-other-window
		     (org-get-buffer-for-internal-link (current-buffer))))
		  (let ((destination
			 (org-with-wide-buffer
			  (if (equal type "radio")
			      (org-search-radio-target
			       (org-element-property :path context))
			    (org-link-search
			     (if (member type '("custom-id" "coderef"))
				 (org-element-property :raw-link context)
			       path)
			     ;; Prevent fuzzy links from matching
			     ;; themselves.
			     (and (equal type "fuzzy")
				  (+ 2 (org-element-property :begin context)))))
			  (point))))
		    (unless (and (<= (point-min) destination)
				 (>= (point-max) destination))
		      (widen))
		    (goto-char destination))))
	       (t (browse-url-at-point))))))
	 (t (user-error "No link found")))))
    (run-hook-with-args 'org-follow-link-hook)))

(defun org-offer-links-in-entry (buffer marker &optional nth zero)
  "Offer links in the current entry and return the selected link.
If there is only one link, return it.
If NTH is an integer, return the NTH link found.
If ZERO is a string, check also this string for a link, and if
there is one, return it."
  (with-current-buffer buffer
    (org-with-wide-buffer
     (goto-char marker)
     (let ((cnt ?0)
	   have-zero end links link c)
       (when (and (stringp zero) (string-match org-bracket-link-regexp zero))
	 (push (match-string 0 zero) links)
	 (setq cnt (1- cnt) have-zero t))
       (save-excursion
	 (org-back-to-heading t)
	 (setq end (save-excursion (outline-next-heading) (point)))
	 (while (re-search-forward org-any-link-re end t)
	   (push (match-string 0) links))
	 (setq links (org-uniquify (reverse links))))
       (cond
	((null links)
	 (message "No links"))
	((equal (length links) 1)
	 (setq link (car links)))
	((and (integerp nth) (>= (length links) (if have-zero (1+ nth) nth)))
	 (setq link (nth (if have-zero nth (1- nth)) links)))
	(t				; we have to select a link
	 (save-excursion
	   (save-window-excursion
	     (delete-other-windows)
	     (with-output-to-temp-buffer "*Select Link*"
	       (dolist (l links)
		 (cond
		  ((not (string-match org-bracket-link-regexp l))
		   (princ (format "[%c]  %s\n" (cl-incf cnt)
				  (org-unbracket-string "<" ">" l))))
		  ((match-end 3)
		   (princ (format "[%c]  %s (%s)\n" (cl-incf cnt)
				  (match-string 3 l) (match-string 1 l))))
		  (t (princ (format "[%c]  %s\n" (cl-incf cnt)
				    (match-string 1 l)))))))
	     (org-fit-window-to-buffer (get-buffer-window "*Select Link*"))
	     (message "Select link to open, RET to open all:")
	     (setq c (read-char-exclusive))
	     (and (get-buffer "*Select Link*") (kill-buffer "*Select Link*"))))
	 (when (equal c ?q) (user-error "Abort"))
	 (if (equal c ?\C-m)
	     (setq link links)
	   (setq nth (- c ?0))
	   (when have-zero (setq nth (1+ nth)))
	   (unless (and (integerp nth) (>= (length links) nth))
	     (user-error "Invalid link selection"))
	   (setq link (nth (1- nth) links)))))
       (cons link end)))))

;; TODO: These functions are deprecated since `org-open-at-point'
;; hard-codes behaviour for "file+emacs" and "file+sys" types.
(defun org-open-file-with-system (path)
  "Open file at PATH using the system way of opening it."
  (org-open-file path 'system))
(defun org-open-file-with-emacs (path)
  "Open file at PATH in Emacs."
  (org-open-file path 'emacs))


;;; File search

(defvar org-create-file-search-functions nil
  "List of functions to construct the right search string for a file link.
These functions are called in turn with point at the location to
which the link should point.

A function in the hook should first test if it would like to
handle this file type, for example by checking the `major-mode'
or the file extension.  If it decides not to handle this file, it
should just return nil to give other functions a chance.  If it
does handle the file, it must return the search string to be used
when following the link.  The search string will be part of the
file link, given after a double colon, and `org-open-at-point'
will automatically search for it.  If special measures must be
taken to make the search successful, another function should be
added to the companion hook `org-execute-file-search-functions',
which see.

A function in this hook may also use `setq' to set the variable
`description' to provide a suggestion for the descriptive text to
be used for this link when it gets inserted into an Org buffer
with \\[org-insert-link].")

(defvar org-execute-file-search-functions nil
  "List of functions to execute a file search triggered by a link.

Functions added to this hook must accept a single argument, the
search string that was part of the file link, the part after the
double colon.  The function must first check if it would like to
handle this search, for example by checking the `major-mode' or
the file extension.  If it decides not to handle this search, it
should just return nil to give other functions a chance.  If it
does handle the search, it must return a non-nil value to keep
other functions from trying.

Each function can access the current prefix argument through the
variable `current-prefix-arg'.  Note that a single prefix is used
to force opening a link in Emacs, so it may be good to only use a
numeric or double prefix to guide the search function.

In case this is needed, a function in this hook can also restore
the window configuration before `org-open-at-point' was called using:

    (set-window-configuration org-window-config-before-follow-link)")

(defun org-search-radio-target (target)
  "Search a radio target matching TARGET in current buffer.
White spaces are not significant."
  (let ((re (format "<<<%s>>>"
		    (mapconcat #'regexp-quote
			       (split-string target)
			       "[ \t]+\\(?:\n[ \t]*\\)?")))
	(origin (point)))
    (goto-char (point-min))
    (catch :radio-match
      (while (re-search-forward re nil t)
	(backward-char)
	(let ((object (org-element-context)))
	  (when (eq (org-element-type object) 'radio-target)
	    (goto-char (org-element-property :begin object))
	    (org-show-context 'link-search)
	    (throw :radio-match nil))))
      (goto-char origin)
      (user-error "No match for radio target: %s" target))))

(defun org-link-search (s &optional avoid-pos stealth)
  "Search for a search string S.

If S starts with \"#\", it triggers a custom ID search.

If S is enclosed within parenthesis, it initiates a coderef
search.

If S is surrounded by forward slashes, it is interpreted as
a regular expression.  In Org mode files, this will create an
`org-occur' sparse tree.  In ordinary files, `occur' will be used
to list matches.  If the current buffer is in `dired-mode', grep
will be used to search in all files.

When AVOID-POS is given, ignore matches near that position.

When optional argument STEALTH is non-nil, do not modify
visibility around point, thus ignoring `org-show-context-detail'
variable.

Search is case-insensitive and ignores white spaces.  Return type
of matched result, which is either `dedicated' or `fuzzy'."
  (unless (org-string-nw-p s) (error "Invalid search string \"%s\"" s))
  (let* ((case-fold-search t)
	 (origin (point))
	 (normalized (replace-regexp-in-string "\n[ \t]*" " " s))
	 (starred (eq (string-to-char normalized) ?*))
	 (words (split-string (if starred (substring s 1) s)))
	 (s-multi-re (mapconcat #'regexp-quote words "\\(?:[ \t\n]+\\)"))
	 (s-single-re (mapconcat #'regexp-quote words "[ \t]+"))
	 type)
    (cond
     ;; Check if there are any special search functions.
     ((run-hook-with-args-until-success 'org-execute-file-search-functions s))
     ((eq (string-to-char s) ?#)
      ;; Look for a custom ID S if S starts with "#".
      (let* ((id (substring normalized 1))
	     (match (org-find-property "CUSTOM_ID" id)))
	(if match (progn (goto-char match) (setf type 'dedicated))
	  (error "No match for custom ID: %s" id))))
     ((string-match "\\`(\\(.*\\))\\'" normalized)
      ;; Look for coderef targets if S is enclosed within parenthesis.
      (let ((coderef (match-string-no-properties 1 normalized))
	    (re (substring s-single-re 1 -1)))
	(goto-char (point-min))
	(catch :coderef-match
	  (while (re-search-forward re nil t)
	    (let ((element (org-element-at-point)))
	      (when (and (memq (org-element-type element)
			       '(example-block src-block))
			 ;; Build proper regexp according to current
			 ;; block's label format.
			 (let ((label-fmt
				(regexp-quote
				 (or (org-element-property :label-fmt element)
				     org-coderef-label-format))))
			   (save-excursion
			     (beginning-of-line)
			     (looking-at (format ".*?\\(%s\\)[ \t]*$"
						 (format label-fmt coderef))))))
		(setq type 'dedicated)
		(goto-char (match-beginning 1))
		(throw :coderef-match nil))))
	  (goto-char origin)
	  (error "No match for coderef: %s" coderef))))
     ((string-match "\\`/\\(.*\\)/\\'" normalized)
      ;; Look for a regular expression.
      (funcall (if (derived-mode-p 'org-mode) #'org-occur #'org-do-occur)
	       (match-string 1 s)))
     ;; From here, we handle fuzzy links.
     ;;
     ;; Look for targets, only if not in a headline search.
     ((and (not starred)
	   (let ((target (format "<<%s>>" s-multi-re)))
	     (catch :target-match
	       (goto-char (point-min))
	       (while (re-search-forward target nil t)
		 (backward-char)
		 (let ((context (org-element-context)))
		   (when (eq (org-element-type context) 'target)
		     (setq type 'dedicated)
		     (goto-char (org-element-property :begin context))
		     (throw :target-match t))))
	       nil))))
     ;; Look for elements named after S, only if not in a headline
     ;; search.
     ((and (not starred)
	   (let ((name (format "^[ \t]*#\\+NAME: +%s[ \t]*$" s-single-re)))
	     (catch :name-match
	       (goto-char (point-min))
	       (while (re-search-forward name nil t)
		 (let ((element (org-element-at-point)))
		   (when (equal words
				(split-string
				 (org-element-property :name element)))
		     (setq type 'dedicated)
		     (beginning-of-line)
		     (throw :name-match t))))
	       nil))))
     ;; Regular text search.  Prefer headlines in Org mode buffers.
     ;; Ignore COMMENT keyword, TODO keywords, priority cookies,
     ;; statistics cookies and tags.
     ((and (derived-mode-p 'org-mode)
	   (let ((title-re
		  (format "%s.*\\(?:%s[ \t]\\)?.*%s"
			  org-outline-regexp-bol
			  org-comment-string
			  (mapconcat #'regexp-quote words ".+")))
		 (cookie-re "\\[[0-9]*\\(?:%\\|/[0-9]*\\)\\]")
		 (comment-re (eval-when-compile
			       (format "\\`%s[ \t]+" org-comment-string))))
	     (goto-char (point-min))
	     (catch :found
	       (while (re-search-forward title-re nil t)
		 (when (equal words
			      (split-string
			       (replace-regexp-in-string
				cookie-re ""
				(replace-regexp-in-string
				 comment-re "" (org-get-heading t t t)))))
		   (throw :found t)))
	       nil)))
      (beginning-of-line)
      (setq type 'dedicated))
     ;; Offer to create non-existent headline depending on
     ;; `org-link-search-must-match-exact-headline'.
     ((and (derived-mode-p 'org-mode)
	   (not org-link-search-inhibit-query)
	   (eq org-link-search-must-match-exact-headline 'query-to-create)
	   (yes-or-no-p "No match - create this as a new heading? "))
      (goto-char (point-max))
      (unless (bolp) (newline))
      (org-insert-heading nil t t)
      (insert s "\n")
      (beginning-of-line 0))
     ;; Only headlines are looked after.  No need to process
     ;; further: throw an error.
     ((and (derived-mode-p 'org-mode)
	   (or starred org-link-search-must-match-exact-headline))
      (goto-char origin)
      (error "No match for fuzzy expression: %s" normalized))
     ;; Regular text search.
     ((catch :fuzzy-match
	(goto-char (point-min))
	(while (re-search-forward s-multi-re nil t)
	  ;; Skip match if it contains AVOID-POS or it is included in
	  ;; a link with a description but outside the description.
	  (unless (or (and avoid-pos
			   (<= (match-beginning 0) avoid-pos)
			   (> (match-end 0) avoid-pos))
		      (and (save-match-data
			     (org-in-regexp org-bracket-link-regexp))
			   (match-beginning 3)
			   (or (> (match-beginning 3) (point))
			       (<= (match-end 3) (point)))
			   (org-element-lineage
			    (save-match-data (org-element-context))
			    '(link) t)))
	    (goto-char (match-beginning 0))
	    (setq type 'fuzzy)
	    (throw :fuzzy-match t)))
	nil))
     ;; All failed.  Throw an error.
     (t (goto-char origin)
	(error "No match for fuzzy expression: %s" normalized)))
    ;; Disclose surroundings of match, if appropriate.
    (when (and (derived-mode-p 'org-mode) (not stealth))
      (org-show-context 'link-search))
    type))

(defun org-get-buffer-for-internal-link (buffer)
  "Return a buffer to be used for displaying the link target of internal links."
  (cond
   ((not org-display-internal-link-with-indirect-buffer)
    buffer)
   ((string-suffix-p "(Clone)" (buffer-name buffer))
    (message "Buffer is already a clone, not making another one")
    ;; we also do not modify visibility in this case
    buffer)
   (t ; make a new indirect buffer for displaying the link
    (let* ((bn (buffer-name buffer))
	   (ibn (concat bn "(Clone)"))
	   (ib (or (get-buffer ibn) (make-indirect-buffer buffer ibn 'clone))))
      (with-current-buffer ib (org-overview))
      ib))))

(defun org-do-occur (regexp &optional cleanup)
  "Call the Emacs command `occur'.
If CLEANUP is non-nil, remove the printout of the regular expression
in the *Occur* buffer.  This is useful if the regex is long and not useful
to read."
  (occur regexp)
  (when cleanup
    (let ((cwin (selected-window)) win beg end)
      (when (setq win (get-buffer-window "*Occur*"))
	(select-window win))
      (goto-char (point-min))
      (when (re-search-forward "match[a-z]+" nil t)
	(setq beg (match-end 0))
	(when (re-search-forward "^[ \t]*[0-9]+" nil t)
	  (setq end (1- (match-beginning 0)))))
      (and beg end (let ((inhibit-read-only t)) (delete-region beg end)))
      (goto-char (point-min))
      (select-window cwin))))

;;; The mark ring for links jumps

(defvar org-mark-ring nil
  "Mark ring for positions before jumps in Org mode.")
(defvar org-mark-ring-last-goto nil
  "Last position in the mark ring used to go back.")
;; Fill and close the ring
(setq org-mark-ring nil org-mark-ring-last-goto nil) ;; in case file is reloaded
(dotimes (_ org-mark-ring-length)
  (push (make-marker) org-mark-ring))
(setcdr (nthcdr (1- org-mark-ring-length) org-mark-ring)
	org-mark-ring)

(defun org-mark-ring-push (&optional pos buffer)
  "Put the current position or POS into the mark ring and rotate it."
  (interactive)
  (setq pos (or pos (point)))
  (setq org-mark-ring (nthcdr (1- org-mark-ring-length) org-mark-ring))
  (move-marker (car org-mark-ring)
	       (or pos (point))
	       (or buffer (current-buffer)))
  (message "%s"
	   (substitute-command-keys
	    "Position saved to mark ring, go back with \
`\\[org-mark-ring-goto]'.")))

(defun org-mark-ring-goto (&optional n)
  "Jump to the previous position in the mark ring.
With prefix arg N, jump back that many stored positions.  When
called several times in succession, walk through the entire ring.
Org mode commands jumping to a different position in the current file,
or to another Org file, automatically push the old position onto the ring."
  (interactive "p")
  (let (p m)
    (if (eq last-command this-command)
	(setq p (nthcdr n (or org-mark-ring-last-goto org-mark-ring)))
      (setq p org-mark-ring))
    (setq org-mark-ring-last-goto p)
    (setq m (car p))
    (pop-to-buffer-same-window (marker-buffer m))
    (goto-char m)
    (when (or (org-invisible-p) (org-invisible-p2)) (org-show-context 'mark-goto))))

(defun org-add-angle-brackets (s)
  (unless (equal (substring s 0 1) "<") (setq s (concat "<" s)))
  (unless (equal (substring s -1) ">") (setq s (concat s ">")))
  s)

;;; Following specific links

(defvar org-agenda-buffer-tmp-name)
(defvar org-agenda-start-on-weekday)
(defun org-follow-timestamp-link ()
  "Open an agenda view for the time-stamp date/range at point."
  (cond
   ((org-at-date-range-p t)
    (let ((org-agenda-start-on-weekday)
	  (t1 (match-string 1))
	  (t2 (match-string 2)) tt1 tt2)
      (setq tt1 (time-to-days (org-time-string-to-time t1))
	    tt2 (time-to-days (org-time-string-to-time t2)))
      (let ((org-agenda-buffer-tmp-name
	     (format "*Org Agenda(a:%s)"
		     (concat (substring t1 0 10) "--" (substring t2 0 10)))))
	(org-agenda-list nil tt1 (1+ (- tt2 tt1))))))
   ((org-at-timestamp-p 'lax)
    (let ((org-agenda-buffer-tmp-name
	   (format "*Org Agenda(a:%s)" (substring (match-string 1) 0 10))))
      (org-agenda-list nil (time-to-days (org-time-string-to-time
					  (substring (match-string 1) 0 10)))
		       1)))
   (t (error "This should not happen"))))


;;; Following file links
(declare-function mailcap-parse-mailcaps "mailcap" (&optional path force))
(declare-function mailcap-extension-to-mime "mailcap" (extn))
(declare-function mailcap-mime-info
		  "mailcap" (string &optional request no-decode))
(defvar org-wait nil)
(defun org-open-file (path &optional in-emacs line search)
  "Open the file at PATH.
First, this expands any special file name abbreviations.  Then the
configuration variable `org-file-apps' is checked if it contains an
entry for this file type, and if yes, the corresponding command is launched.

If no application is found, Emacs simply visits the file.

With optional prefix argument IN-EMACS, Emacs will visit the file.
With a double \\[universal-argument] \\[universal-argument] \
prefix arg, Org tries to avoid opening in Emacs
and to use an external application to visit the file.

Optional LINE specifies a line to go to, optional SEARCH a string
to search for.  If LINE or SEARCH is given, the file will be
opened in Emacs, unless an entry from org-file-apps that makes
use of groups in a regexp matches.

If you want to change the way frames are used when following a
link, please customize `org-link-frame-setup'.

If the file does not exist, an error is thrown."
  (let* ((file (if (equal path "")
		   buffer-file-name
		 (substitute-in-file-name (expand-file-name path))))
	 (file-apps (append org-file-apps (org-default-apps)))
	 (apps (cl-remove-if
		'org-file-apps-entry-match-against-dlink-p file-apps))
	 (apps-dlink (cl-remove-if-not
		      'org-file-apps-entry-match-against-dlink-p file-apps))
	 (remp (and (assq 'remote apps) (org-file-remote-p file)))
	 (dirp (unless remp (file-directory-p file)))
	 (file (if (and dirp org-open-directory-means-index-dot-org)
		   (concat (file-name-as-directory file) "index.org")
		 file))
	 (a-m-a-p (assq 'auto-mode apps))
	 (dfile (downcase file))
	 ;; Reconstruct the original link from the PATH, LINE and
	 ;; SEARCH args.
	 (link (cond (line (concat file "::" (number-to-string line)))
		     (search (concat file "::" search))
		     (t file)))
	 (dlink (downcase link))
	 (ext
	  (and (string-match "\\`.*?\\.\\([a-zA-Z0-9]+\\(\\.gz\\)?\\)\\'" dfile)
	       (match-string 1 dfile)))
	 (save-position-maybe
	  (let ((old-buffer (current-buffer))
		(old-pos (point))
		(old-mode major-mode))
	    (lambda ()
	      (and (derived-mode-p 'org-mode)
		   (eq old-mode 'org-mode)
		   (or (not (eq old-buffer (current-buffer)))
		       (not (eq old-pos (point))))
		   (org-mark-ring-push old-pos old-buffer)))))
	 cmd link-match-data)
    (cond
     ((member in-emacs '((16) system))
      (setq cmd (cdr (assq 'system apps))))
     (in-emacs (setq cmd 'emacs))
     (t
      (setq cmd (or (and remp (cdr (assq 'remote apps)))
		    (and dirp (cdr (assq 'directory apps)))
		    ;; First, try matching against apps-dlink if we
		    ;; get a match here, store the match data for
		    ;; later.
		    (let ((match (assoc-default dlink apps-dlink
						'string-match)))
		      (if match
			  (progn (setq link-match-data (match-data))
				 match)
			(progn (setq in-emacs (or in-emacs line search))
			       nil))) ; if we have no match in apps-dlink,
					; always open the file in emacs if line or search
					; is given (for backwards compatibility)
		    (assoc-default dfile (org-apps-regexp-alist apps a-m-a-p)
				   'string-match)
		    (cdr (assoc ext apps))
		    (cdr (assq t apps))))))
    (when (eq cmd 'system)
      (setq cmd (cdr (assq 'system apps))))
    (when (eq cmd 'default)
      (setq cmd (cdr (assoc t apps))))
    (when (eq cmd 'mailcap)
      (require 'mailcap)
      (mailcap-parse-mailcaps)
      (let* ((mime-type (mailcap-extension-to-mime (or ext "")))
	     (command (mailcap-mime-info mime-type)))
	(if (stringp command)
	    (setq cmd command)
	  (setq cmd 'emacs))))
    (when (and (not (eq cmd 'emacs)) ; Emacs has no problems with non-ex files
	       (not (file-exists-p file))
	       (not org-open-non-existing-files))
      (user-error "No such file: %s" file))
    (cond
     ((and (stringp cmd) (not (string-match "^\\s-*$" cmd)))
      ;; Remove quotes around the file name - we'll use shell-quote-argument.
      (while (string-match "['\"]%s['\"]" cmd)
	(setq cmd (replace-match "%s" t t cmd)))
      (setq cmd (replace-regexp-in-string
		 "%s"
		 (shell-quote-argument (convert-standard-filename file))
		 cmd
		 nil t))

      ;; Replace "%1", "%2" etc. in command with group matches from regex
      (save-match-data
	(let ((match-index 1)
	      (number-of-groups (- (/ (length link-match-data) 2) 1)))
	  (set-match-data link-match-data)
	  (while (<= match-index number-of-groups)
	    (let ((regex (concat "%" (number-to-string match-index)))
		  (replace-with (match-string match-index dlink)))
	      (while (string-match regex cmd)
		(setq cmd (replace-match replace-with t t cmd))))
	    (setq match-index (+ match-index 1)))))

      (save-window-excursion
	(message "Running %s...done" cmd)
	(start-process-shell-command cmd nil cmd)
	(and (boundp 'org-wait) (numberp org-wait) (sit-for org-wait))))
     ((or (stringp cmd)
	  (eq cmd 'emacs))
      (funcall (cdr (assq 'file org-link-frame-setup)) file)
      (widen)
      (cond (line (org-goto-line line)
		  (when (derived-mode-p 'org-mode) (org-reveal)))
	    (search (condition-case err
			(org-link-search search)
		      ;; Save position before error-ing out so user
		      ;; can easily move back to the original buffer.
		      (error (funcall save-position-maybe)
			     (error (nth 1 err)))))))
     ((functionp cmd)
      (save-match-data
	(set-match-data link-match-data)
	(condition-case nil
	    (funcall cmd file link)
	  ;; FIXME: Remove this check when most default installations
	  ;; of Emacs have at least Org 9.0.
	  ((debug wrong-number-of-arguments wrong-type-argument
		  invalid-function)
	   (user-error "Please see Org News for version 9.0 about \
`org-file-apps'--Lisp error: %S" cmd)))))
     ((consp cmd)
      ;; FIXME: Remove this check when most default installations of
      ;; Emacs have at least Org 9.0.  Heads-up instead of silently
      ;; fall back to `org-link-frame-setup' for an old usage of
      ;; `org-file-apps' with sexp instead of a function for `cmd'.
      (user-error "Please see Org News for version 9.0 about \
`org-file-apps'--Error: Deprecated usage of %S" cmd))
     (t (funcall (cdr (assq 'file org-link-frame-setup)) file)))
    (funcall save-position-maybe)))

(defun org-file-apps-entry-match-against-dlink-p (entry)
  "This function returns non-nil if `entry' uses a regular
expression which should be matched against the whole link by
org-open-file.

It assumes that is the case when the entry uses a regular
expression which has at least one grouping construct and the
action is either a lisp form or a command string containing
`%1', i.e. using at least one subexpression match as a
parameter."
  (let ((selector (car entry))
	(action (cdr entry)))
    (if (stringp selector)
	(and (> (regexp-opt-depth selector) 0)
	     (or (and (stringp action)
		      (string-match "%[0-9]" action))
		 (consp action)))
      nil)))

(defun org-default-apps ()
  "Return the default applications for this operating system."
  (cond
   ((eq system-type 'darwin)
    org-file-apps-defaults-macosx)
   ((eq system-type 'windows-nt)
    org-file-apps-defaults-windowsnt)
   (t org-file-apps-defaults-gnu)))

(defun org-apps-regexp-alist (list &optional add-auto-mode)
  "Convert extensions to regular expressions in the cars of LIST.
Also, weed out any non-string entries, because the return value is used
only for regexp matching.
When ADD-AUTO-MODE is set, make all matches in `auto-mode-alist'
point to the symbol `emacs', indicating that the file should
be opened in Emacs."
  (append
   (delq nil
	 (mapcar (lambda (x)
		   (unless (not (stringp (car x)))
		     (if (string-match "\\W" (car x))
			 x
		       (cons (concat "\\." (car x) "\\'") (cdr x)))))
		 list))
   (when add-auto-mode
     (mapcar (lambda (x) (cons (car x) 'emacs)) auto-mode-alist))))

(defvar ange-ftp-name-format)
(defun org-file-remote-p (file)
  "Test whether FILE specifies a location on a remote system.
Return non-nil if the location is indeed remote.

For example, the filename \"/user@host:/foo\" specifies a location
on the system \"/user@host:\"."
  (cond ((fboundp 'file-remote-p)
         (file-remote-p file))
        ((fboundp 'tramp-handle-file-remote-p)
         (tramp-handle-file-remote-p file))
        ((and (boundp 'ange-ftp-name-format)
              (string-match (car ange-ftp-name-format) file))
         t)))


;;;; Refiling

(defun org-get-org-file ()
  "Read a filename, with default directory `org-directory'."
  (let ((default (or org-default-notes-file remember-data-file)))
    (read-file-name (format "File name [%s]: " default)
		    (file-name-as-directory org-directory)
		    default)))

(defun org-notes-order-reversed-p ()
  "Check if the current file should receive notes in reversed order."
  (cond
   ((not org-reverse-note-order) nil)
   ((eq t org-reverse-note-order) t)
   ((not (listp org-reverse-note-order)) nil)
   (t (catch 'exit
        (dolist (entry org-reverse-note-order)
          (when (string-match (car entry) buffer-file-name)
	    (throw 'exit (cdr entry))))))))

(defvar org-refile-target-table nil
  "The list of refile targets, created by `org-refile'.")

(defvar org-agenda-new-buffers nil
  "Buffers created to visit agenda files.")

(defvar org-refile-cache nil
  "Cache for refile targets.")

(defvar org-refile-markers nil
  "All the markers used for caching refile locations.")

(defun org-refile-marker (pos)
  "Get a new refile marker, but only if caching is in use."
  (if (not org-refile-use-cache)
      pos
    (let ((m (make-marker)))
      (move-marker m pos)
      (push m org-refile-markers)
      m)))

(defun org-refile-cache-clear ()
  "Clear the refile cache and disable all the markers."
  (dolist (m org-refile-markers) (move-marker m nil))
  (setq org-refile-markers nil)
  (setq org-refile-cache nil)
  (message "Refile cache has been cleared"))

(defun org-refile-cache-check-set (set)
  "Check if all the markers in the cache still have live buffers."
  (let (marker)
    (catch 'exit
      (while (and set (setq marker (nth 3 (pop set))))
	;; If `org-refile-use-outline-path' is 'file, marker may be nil
	(when (and marker (null (marker-buffer marker)))
	  (message "Please regenerate the refile cache with `C-0 C-c C-w'")
	  (sit-for 3)
	  (throw 'exit nil)))
      t)))

(defun org-refile-cache-put (set &rest identifiers)
  "Push the refile targets SET into the cache, under IDENTIFIERS."
  (let* ((key (sha1 (prin1-to-string identifiers)))
	 (entry (assoc key org-refile-cache)))
    (if entry
	(setcdr entry set)
      (push (cons key set) org-refile-cache))))

(defun org-refile-cache-get (&rest identifiers)
  "Retrieve the cached value for refile targets given by IDENTIFIERS."
  (cond
   ((not org-refile-cache) nil)
   ((not org-refile-use-cache) (org-refile-cache-clear) nil)
   (t
    (let ((set (cdr (assoc (sha1 (prin1-to-string identifiers))
			   org-refile-cache))))
      (and set (org-refile-cache-check-set set) set)))))

(defvar org-outline-path-cache nil
  "Alist between buffer positions and outline paths.
It value is an alist (POSITION . PATH) where POSITION is the
buffer position at the beginning of an entry and PATH is a list
of strings describing the outline path for that entry, in reverse
order.")

(defun org-refile-get-targets (&optional default-buffer)
  "Produce a table with refile targets."
  (let ((case-fold-search nil)
	;; otherwise org confuses "TODO" as a kw and "Todo" as a word
	(entries (or org-refile-targets '((nil . (:level . 1)))))
	targets tgs files desc descre)
    (message "Getting targets...")
    (with-current-buffer (or default-buffer (current-buffer))
      (dolist (entry entries)
	(setq files (car entry) desc (cdr entry))
	(cond
	 ((null files) (setq files (list (current-buffer))))
	 ((eq files 'org-agenda-files)
	  (setq files (org-agenda-files 'unrestricted)))
	 ((and (symbolp files) (fboundp files))
	  (setq files (funcall files)))
	 ((and (symbolp files) (boundp files))
	  (setq files (symbol-value files))))
	(when (stringp files) (setq files (list files)))
	(cond
	 ((eq (car desc) :tag)
	  (setq descre (concat "^\\*+[ \t]+.*?:" (regexp-quote (cdr desc)) ":")))
	 ((eq (car desc) :todo)
	  (setq descre (concat "^\\*+[ \t]+" (regexp-quote (cdr desc)) "[ \t]")))
	 ((eq (car desc) :regexp)
	  (setq descre (cdr desc)))
	 ((eq (car desc) :level)
	  (setq descre (concat "^\\*\\{" (number-to-string
					  (if org-odd-levels-only
					      (1- (* 2 (cdr desc)))
					    (cdr desc)))
			       "\\}[ \t]")))
	 ((eq (car desc) :maxlevel)
	  (setq descre (concat "^\\*\\{1," (number-to-string
					    (if org-odd-levels-only
						(1- (* 2 (cdr desc)))
					      (cdr desc)))
			       "\\}[ \t]")))
	 (t (error "Bad refiling target description %s" desc)))
	(dolist (f files)
	  (with-current-buffer (if (bufferp f) f (org-get-agenda-file-buffer f))
	    (or
	     (setq tgs (org-refile-cache-get (buffer-file-name) descre))
	     (progn
	       (when (bufferp f)
		 (setq f (buffer-file-name (buffer-base-buffer f))))
	       (setq f (and f (expand-file-name f)))
	       (when (eq org-refile-use-outline-path 'file)
		 (push (list (file-name-nondirectory f) f nil nil) tgs))
	       (when (eq org-refile-use-outline-path 'buffer-name)
		 (push (list (buffer-name (buffer-base-buffer)) f nil nil) tgs))
	       (when (eq org-refile-use-outline-path 'full-file-path)
		 (push (list (file-truename (buffer-file-name (buffer-base-buffer))) f nil nil) tgs))
	       (org-with-wide-buffer
		(goto-char (point-min))
		(setq org-outline-path-cache nil)
		(while (re-search-forward descre nil t)
		  (beginning-of-line)
		  (let ((case-fold-search nil))
		    (looking-at org-complex-heading-regexp))
		  (let ((begin (point))
			(heading (match-string-no-properties 4)))
		    (unless (or (and
				 org-refile-target-verify-function
				 (not
				  (funcall org-refile-target-verify-function)))
				(not heading))
		      (let ((re (format org-complex-heading-regexp-format
					(regexp-quote heading)))
			    (target
			     (if (not org-refile-use-outline-path) heading
			       (mapconcat
				#'identity
				(append
				 (pcase org-refile-use-outline-path
				   (`file (list (file-name-nondirectory
						 (buffer-file-name
						  (buffer-base-buffer)))))
				   (`full-file-path
				    (list (buffer-file-name
					   (buffer-base-buffer))))
				   (`buffer-name
				    (list (buffer-name
					   (buffer-base-buffer))))
				   (_ nil))
				 (mapcar (lambda (s) (replace-regexp-in-string
						 "/" "\\/" s nil t))
					 (org-get-outline-path t t)))
				"/"))))
			(push (list target f re (org-refile-marker (point)))
			      tgs)))
		    (when (= (point) begin)
		      ;; Verification function has not moved point.
		      (end-of-line)))))))
	    (when org-refile-use-cache
	      (org-refile-cache-put tgs (buffer-file-name) descre))
	    (setq targets (append tgs targets))))))
    (message "Getting targets...done")
    (delete-dups (nreverse targets))))

(defun org--get-outline-path-1 (&optional use-cache)
  "Return outline path to current headline.

Outline path is a list of strings, in reverse order.  When
optional argument USE-CACHE is non-nil, make use of a cache.  See
`org-get-outline-path' for details.

Assume buffer is widened and point is on a headline."
  (or (and use-cache (cdr (assq (point) org-outline-path-cache)))
      (let ((p (point))
	    (heading (let ((case-fold-search nil))
		       (looking-at org-complex-heading-regexp)
		       (if (not (match-end 4)) ""
			 ;; Remove statistics cookies.
			 (org-trim
			  (org-link-display-format
			   (replace-regexp-in-string
			    "\\[[0-9]+%\\]\\|\\[[0-9]+/[0-9]+\\]" ""
			    (match-string-no-properties 4))))))))
	(if (org-up-heading-safe)
	    (let ((path (cons heading (org--get-outline-path-1 use-cache))))
	      (when use-cache
		(push (cons p path) org-outline-path-cache))
	      path)
	  ;; This is a new root node.  Since we assume we are moving
	  ;; forward, we can drop previous cache so as to limit number
	  ;; of associations there.
	  (let ((path (list heading)))
	    (when use-cache (setq org-outline-path-cache (list (cons p path))))
	    path)))))

(defun org-get-outline-path (&optional with-self use-cache)
  "Return the outline path to the current entry.

An outline path is a list of ancestors for current headline, as
a list of strings.  Statistics cookies are removed and links are
replaced with their description, if any, or their path otherwise.

When optional argument WITH-SELF is non-nil, the path also
includes the current headline.

When optional argument USE-CACHE is non-nil, cache outline paths
between calls to this function so as to avoid backtracking.  This
argument is useful when planning to find more than one outline
path in the same document.  In that case, there are two
conditions to satisfy:
  - `org-outline-path-cache' is set to nil before starting the
    process;
  - outline paths are computed by increasing buffer positions."
  (org-with-wide-buffer
   (and (or (and with-self (org-back-to-heading t))
	    (org-up-heading-safe))
	(reverse (org--get-outline-path-1 use-cache)))))

(defun org-format-outline-path (path &optional width prefix separator)
  "Format the outline path PATH for display.
WIDTH is the maximum number of characters that is available.
PREFIX is a prefix to be included in the returned string,
such as the file name.
SEPARATOR is inserted between the different parts of the path,
the default is \"/\"."
  (setq width (or width 79))
  (setq path (delq nil path))
  (unless (> width 0)
    (user-error "Argument `width' must be positive"))
  (setq separator (or separator "/"))
  (let* ((org-odd-levels-only nil)
	 (fpath (concat
		 prefix (and prefix path separator)
		 (mapconcat
		  (lambda (s) (replace-regexp-in-string "[ \t]+\\'" "" s))
		  (cl-loop for head in path
			   for n from 0
			   collect (org-add-props
				       head nil 'face
				       (nth (% n org-n-level-faces) org-level-faces)))
		  separator))))
    (when (> (length fpath) width)
      (if (< width 7)
	  ;; It's unlikely that `width' will be this small, but don't
	  ;; waste characters by adding ".." if it is.
	  (setq fpath (substring fpath 0 width))
	(setf (substring fpath (- width 2)) "..")))
    fpath))

(defun org-display-outline-path (&optional file current separator just-return-string)
  "Display the current outline path in the echo area.

If FILE is non-nil, prepend the output with the file name.
If CURRENT is non-nil, append the current heading to the output.
SEPARATOR is passed through to `org-format-outline-path'.  It separates
the different parts of the path and defaults to \"/\".
If JUST-RETURN-STRING is non-nil, return a string, don't display a message."
  (interactive "P")
  (let* (case-fold-search
	 (bfn (buffer-file-name (buffer-base-buffer)))
	 (path (and (derived-mode-p 'org-mode) (org-get-outline-path)))
	 res)
    (when current (setq path (append path
				     (save-excursion
				       (org-back-to-heading t)
				       (when (looking-at org-complex-heading-regexp)
					 (list (match-string 4)))))))
    (setq res
	  (org-format-outline-path
	   path
	   (1- (frame-width))
	   (and file bfn (concat (file-name-nondirectory bfn) separator))
	   separator))
    (if just-return-string
	(org-no-properties res)
      (org-unlogged-message "%s" res))))

(defvar org-refile-history nil
  "History for refiling operations.")

(defvar org-after-refile-insert-hook nil
  "Hook run after `org-refile' has inserted its stuff at the new location.
Note that this is still *before* the stuff will be removed from
the *old* location.")

(defvar org-capture-last-stored-marker)
(defvar org-refile-keep nil
  "Non-nil means `org-refile' will copy instead of refile.")

(defun org-copy ()
  "Like `org-refile', but copy."
  (interactive)
  (let ((org-refile-keep t))
    (funcall 'org-refile nil nil nil "Copy")))

(defun org-refile (&optional arg default-buffer rfloc msg)
  "Move the entry or entries at point to another heading.

The list of target headings is compiled using the information in
`org-refile-targets', which see.

At the target location, the entry is filed as a subitem of the
target heading.  Depending on `org-reverse-note-order', the new
subitem will either be the first or the last subitem.

If there is an active region, all entries in that region will be
refiled.  However, the region must fulfill the requirement that
the first heading sets the top-level of the moved text.

With a `\\[universal-argument]' ARG, the command will only visit the target \
location
and not actually move anything.

With a prefix `\\[universal-argument] \\[universal-argument]', go to the \
location where the last
refiling operation has put the subtree.

With a numeric prefix argument of `2', refile to the running clock.

With a numeric prefix argument of `3', emulate `org-refile-keep'
being set to t and copy to the target location, don't move it.
Beware that keeping refiled entries may result in duplicated ID
properties.

RFLOC can be a refile location obtained in a different way.

MSG is a string to replace \"Refile\" in the default prompt with
another verb.  E.g. `org-copy' sets this parameter to \"Copy\".

See also `org-refile-use-outline-path'.

If you are using target caching (see `org-refile-use-cache'), you
have to clear the target cache in order to find new targets.
This can be done with a `0' prefix (`C-0 C-c C-w') or a triple
prefix argument (`C-u C-u C-u C-c C-w')."
  (interactive "P")
  (if (member arg '(0 (64)))
      (org-refile-cache-clear)
    (let* ((actionmsg (cond (msg msg)
			    ((equal arg 3) "Refile (and keep)")
			    (t "Refile")))
	   (regionp (org-region-active-p))
	   (region-start (and regionp (region-beginning)))
	   (region-end (and regionp (region-end)))
	   (org-refile-keep (if (equal arg 3) t org-refile-keep))
	   pos it nbuf file level reversed)
      (setq last-command nil)
      (when regionp
	(goto-char region-start)
	(or (bolp) (goto-char (point-at-bol)))
	(setq region-start (point))
	(unless (or (org-kill-is-subtree-p
		     (buffer-substring region-start region-end))
		    (prog1 org-refile-active-region-within-subtree
		      (let ((s (point-at-eol)))
			(org-toggle-heading)
			(setq region-end (+ (- (point-at-eol) s) region-end)))))
	  (user-error "The region is not a (sequence of) subtree(s)")))
      (if (equal arg '(16))
	  (org-refile-goto-last-stored)
	(when (or
	       (and (equal arg 2)
		    org-clock-hd-marker (marker-buffer org-clock-hd-marker)
		    (prog1
			(setq it (list (or org-clock-heading "running clock")
				       (buffer-file-name
					(marker-buffer org-clock-hd-marker))
				       ""
				       (marker-position org-clock-hd-marker)))
		      (setq arg nil)))
	       (setq it
		     (or rfloc
			 (let (heading-text)
			   (save-excursion
			     (unless (and arg (listp arg))
			       (org-back-to-heading t)
			       (setq heading-text
				     (replace-regexp-in-string
				      org-bracket-link-regexp
				      "\\3"
				      (or (nth 4 (org-heading-components))
					  ""))))
			     (org-refile-get-location
			      (cond ((and arg (listp arg)) "Goto")
				    (regionp (concat actionmsg " region to"))
				    (t (concat actionmsg " subtree \""
					       heading-text "\" to")))
			      default-buffer
			      (and (not (equal '(4) arg))
				   org-refile-allow-creating-parent-nodes)))))))
	  (setq file (nth 1 it)
		pos (nth 3 it))
	  (when (and (not arg)
		     pos
		     (equal (buffer-file-name) file)
		     (if regionp
			 (and (>= pos region-start)
			      (<= pos region-end))
		       (and (>= pos (point))
			    (< pos (save-excursion
				     (org-end-of-subtree t t))))))
	    (error "Cannot refile to position inside the tree or region"))
	  (setq nbuf (or (find-buffer-visiting file)
			 (find-file-noselect file)))
	  (if (and arg (not (equal arg 3)))
	      (progn
		(pop-to-buffer-same-window nbuf)
		(goto-char (cond (pos)
				 ((org-notes-order-reversed-p) (point-min))
				 (t (point-max))))
		(org-show-context 'org-goto))
	    (if regionp
		(progn
		  (org-kill-new (buffer-substring region-start region-end))
		  (org-save-markers-in-region region-start region-end))
	      (org-copy-subtree 1 nil t))
	    (with-current-buffer (setq nbuf (or (find-buffer-visiting file)
						(find-file-noselect file)))
	      (setq reversed (org-notes-order-reversed-p))
	      (org-with-wide-buffer
	       (if pos
		   (progn
		     (goto-char pos)
		     (setq level (org-get-valid-level (funcall outline-level) 1))
		     (goto-char
		      (if reversed
			  (or (outline-next-heading) (point-max))
			(or (save-excursion (org-get-next-sibling))
			    (org-end-of-subtree t t)
			    (point-max)))))
		 (setq level 1)
		 (if (not reversed)
		     (goto-char (point-max))
		   (goto-char (point-min))
		   (or (outline-next-heading) (goto-char (point-max)))))
	       (unless (bolp) (newline))
	       (org-paste-subtree level nil nil t)
	       (when org-log-refile
		 (org-add-log-setup 'refile nil nil org-log-refile)
		 (unless (eq org-log-refile 'note)
		   (save-excursion (org-add-log-note))))
	       (and org-auto-align-tags
		    (let ((org-loop-over-headlines-in-active-region nil))
		      (org-set-tags nil t)))
	       (let ((bookmark-name (plist-get org-bookmark-names-plist
					       :last-refile)))
		 (when bookmark-name
		   (with-demoted-errors
		       (bookmark-set bookmark-name))))
	       ;; If we are refiling for capture, make sure that the
	       ;; last-capture pointers point here
	       (when (bound-and-true-p org-capture-is-refiling)
		 (let ((bookmark-name (plist-get org-bookmark-names-plist
						 :last-capture-marker)))
		   (when bookmark-name
		     (with-demoted-errors
			 (bookmark-set bookmark-name))))
		 (move-marker org-capture-last-stored-marker (point)))
	       (when (fboundp 'deactivate-mark) (deactivate-mark))
	       (run-hooks 'org-after-refile-insert-hook)))
	    (unless org-refile-keep
	      (if regionp
		  (delete-region (point) (+ (point) (- region-end region-start)))
		(delete-region
		 (and (org-back-to-heading t) (point))
		 (min (1+ (buffer-size)) (org-end-of-subtree t t) (point)))))
	    (when (featurep 'org-inlinetask)
	      (org-inlinetask-remove-END-maybe))
	    (setq org-markers-to-move nil)
	    (message (concat actionmsg " to \"%s\" in file %s: done") (car it) file)))))))

(defun org-refile-goto-last-stored ()
  "Go to the location where the last refile was stored."
  (interactive)
  (bookmark-jump (plist-get org-bookmark-names-plist :last-refile))
  (message "This is the location of the last refile"))

(defun org-refile--get-location (refloc tbl)
  "When user refile to REFLOC, find the associated target in TBL.
Also check `org-refile-target-table'."
  (car (delq
	nil
	(mapcar
	 (lambda (r) (or (assoc r tbl)
			 (assoc r org-refile-target-table)))
	 (list (replace-regexp-in-string "/$" "" refloc)
	       (replace-regexp-in-string "\\([^/]\\)$" "\\1/" refloc))))))

(defun org-refile-get-location (&optional prompt default-buffer new-nodes)
  "Prompt the user for a refile location, using PROMPT.
PROMPT should not be suffixed with a colon and a space, because
this function appends the default value from
`org-refile-history' automatically, if that is not empty."
  (let ((org-refile-targets org-refile-targets)
	(org-refile-use-outline-path org-refile-use-outline-path))
    (setq org-refile-target-table (org-refile-get-targets default-buffer)))
  (unless org-refile-target-table
    (user-error "No refile targets"))
  (let* ((cbuf (current-buffer))
	 (cfn (buffer-file-name (buffer-base-buffer cbuf)))
	 (cfunc (if (and org-refile-use-outline-path
			 org-outline-path-complete-in-steps)
		    #'org-olpath-completing-read
		  #'completing-read))
	 (extra (if org-refile-use-outline-path "/" ""))
	 (cbnex (concat (buffer-name) extra))
	 (filename (and cfn (expand-file-name cfn)))
	 (tbl (mapcar
	       (lambda (x)
		 (if (and (not (member org-refile-use-outline-path
				       '(file full-file-path)))
			  (not (equal filename (nth 1 x))))
		     (cons (concat (car x) extra " ("
				   (file-name-nondirectory (nth 1 x)) ")")
			   (cdr x))
		   (cons (concat (car x) extra) (cdr x))))
	       org-refile-target-table))
	 (completion-ignore-case t)
	 cdef
	 (prompt (concat prompt
			 (or (and (car org-refile-history)
				  (concat " (default " (car org-refile-history) ")"))
			     (and (assoc cbnex tbl) (setq cdef cbnex)
				  (concat " (default " cbnex ")"))) ": "))
	 pa answ parent-target child parent old-hist)
    (setq old-hist org-refile-history)
    (setq answ (funcall cfunc prompt tbl nil (not new-nodes)
			nil 'org-refile-history (or cdef (car org-refile-history))))
    (if (setq pa (org-refile--get-location answ tbl))
	(progn
	  (org-refile-check-position pa)
	  (when (or (not org-refile-history)
		    (not (eq old-hist org-refile-history))
		    (not (equal (car pa) (car org-refile-history))))
	    (setq org-refile-history
		  (cons (car pa) (if (assoc (car org-refile-history) tbl)
				     org-refile-history
				   (cdr org-refile-history))))
	    (when (equal (car org-refile-history) (nth 1 org-refile-history))
	      (pop org-refile-history)))
	  pa)
      (if (string-match "\\`\\(.*\\)/\\([^/]+\\)\\'" answ)
	  (progn
	    (setq parent (match-string 1 answ)
		  child (match-string 2 answ))
	    (setq parent-target (org-refile--get-location parent tbl))
	    (when (and parent-target
		       (or (eq new-nodes t)
			   (and (eq new-nodes 'confirm)
				(y-or-n-p (format "Create new node \"%s\"? "
						  child)))))
	      (org-refile-new-child parent-target child)))
	(user-error "Invalid target location")))))

(declare-function org-string-nw-p "org-macs" (s))
(defun org-refile-check-position (refile-pointer)
  "Check if the refile pointer matches the headline to which it points."
  (let* ((file (nth 1 refile-pointer))
	 (re (nth 2 refile-pointer))
	 (pos (nth 3 refile-pointer))
	 buffer)
    (if (and (not (markerp pos)) (not file))
	(user-error "Please indicate a target file in the refile path")
      (when (org-string-nw-p re)
	(setq buffer (if (markerp pos)
			 (marker-buffer pos)
		       (or (find-buffer-visiting file)
			   (find-file-noselect file))))
	(with-current-buffer buffer
	  (org-with-wide-buffer
	   (goto-char pos)
	   (beginning-of-line 1)
	   (unless (looking-at-p re)
	     (user-error "Invalid refile position, please clear the cache with `C-0 C-c C-w' before refiling"))))))))

(defun org-refile-new-child (parent-target child)
  "Use refile target PARENT-TARGET to add new CHILD below it."
  (unless parent-target
    (error "Cannot find parent for new node"))
  (let ((file (nth 1 parent-target))
	(pos (nth 3 parent-target))
	level)
    (with-current-buffer (or (find-buffer-visiting file)
			     (find-file-noselect file))
      (org-with-wide-buffer
       (if pos
	   (goto-char pos)
	 (goto-char (point-max))
	 (unless (bolp) (newline)))
       (when (looking-at org-outline-regexp)
	 (setq level (funcall outline-level))
	 (org-end-of-subtree t t))
       (org-back-over-empty-lines)
       (insert "\n" (make-string
		     (if pos (org-get-valid-level level 1) 1) ?*)
	       " " child "\n")
       (beginning-of-line 0)
       (list (concat (car parent-target) "/" child) file "" (point))))))

(defun org-olpath-completing-read (prompt collection &rest args)
  "Read an outline path like a file name."
  (let ((thetable collection))
    (apply #'completing-read
	   prompt
	   (lambda (string predicate &optional flag)
	     (cond
	      ((eq flag nil) (try-completion string thetable))
	      ((eq flag t)
	       (let ((l (length string)))
		 (mapcar (lambda (x)
			   (let ((r (substring x l))
				 (f (if (string-match " ([^)]*)$" x)
					(match-string 0 x)
				      "")))
			     (if (string-match "/" r)
				 (concat string (substring r 0 (match-end 0)) f)
			       x)))
			 (all-completions string thetable predicate))))
	      ;; Exact match?
	      ((eq flag 'lambda) (assoc string thetable))))
	   args)))

;;;; Dynamic blocks

(defun org-find-dblock (name)
  "Find the first dynamic block with name NAME in the buffer.
If not found, stay at current position and return nil."
  (let ((case-fold-search t) pos)
    (save-excursion
      (goto-char (point-min))
      (setq pos (and (re-search-forward
		      (concat "^[ \t]*#\\+\\(?:BEGIN\\|begin\\):[ \t]+" name "\\>") nil t)
		     (match-beginning 0))))
    (when pos (goto-char pos))
    pos))

(defun org-create-dblock (plist)
  "Create a dynamic block section, with parameters taken from PLIST.
PLIST must contain a :name entry which is used as the name of the block."
  (when (string-match "\\S-" (buffer-substring (point-at-bol) (point-at-eol)))
    (end-of-line 1)
    (newline))
  (let ((col (current-column))
	(name (plist-get plist :name)))
    (insert "#+BEGIN: " name)
    (while plist
      (if (eq (car plist) :name)
	  (setq plist (cddr plist))
	(insert " " (prin1-to-string (pop plist)))))
    (insert "\n\n" (make-string col ?\ ) "#+END:\n")
    (beginning-of-line -2)))

(defun org-prepare-dblock ()
  "Prepare dynamic block for refresh.
This empties the block, puts the cursor at the insert position and returns
the property list including an extra property :name with the block name."
  (unless (looking-at org-dblock-start-re)
    (user-error "Not at a dynamic block"))
  (let* ((begdel (1+ (match-end 0)))
	 (name (org-no-properties (match-string 1)))
	 (params (append (list :name name)
			 (read (concat "(" (match-string 3) ")")))))
    (save-excursion
      (beginning-of-line 1)
      (skip-chars-forward " \t")
      (setq params (plist-put params :indentation-column (current-column))))
    (unless (re-search-forward org-dblock-end-re nil t)
      (error "Dynamic block not terminated"))
    (setq params
	  (append params
		  (list :content (buffer-substring
				  begdel (match-beginning 0)))))
    (delete-region begdel (match-beginning 0))
    (goto-char begdel)
    (open-line 1)
    params))

(defun org-map-dblocks (&optional command)
  "Apply COMMAND to all dynamic blocks in the current buffer.
If COMMAND is not given, use `org-update-dblock'."
  (let ((cmd (or command 'org-update-dblock)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-dblock-start-re nil t)
	(goto-char (match-beginning 0))
        (save-excursion
          (condition-case nil
              (funcall cmd)
            (error (message "Error during update of dynamic block"))))
	(unless (re-search-forward org-dblock-end-re nil t)
	  (error "Dynamic block not terminated"))))))

(defun org-dblock-update (&optional arg)
  "User command for updating dynamic blocks.
Update the dynamic block at point.  With prefix ARG, update all dynamic
blocks in the buffer."
  (interactive "P")
  (if arg
      (org-update-all-dblocks)
    (or (looking-at org-dblock-start-re)
	(org-beginning-of-dblock))
    (org-update-dblock)))

(defun org-update-dblock ()
  "Update the dynamic block at point.
This means to empty the block, parse for parameters and then call
the correct writing function."
  (interactive)
  (save-excursion
    (let* ((win (selected-window))
	   (pos (point))
	   (line (org-current-line))
	   (params (org-prepare-dblock))
	   (name (plist-get params :name))
	   (indent (plist-get params :indentation-column))
	   (cmd (intern (concat "org-dblock-write:" name))))
      (message "Updating dynamic block `%s' at line %d..." name line)
      (funcall cmd params)
      (message "Updating dynamic block `%s' at line %d...done" name line)
      (goto-char pos)
      (when (and indent (> indent 0))
	(setq indent (make-string indent ?\ ))
	(save-excursion
	  (select-window win)
	  (org-beginning-of-dblock)
	  (forward-line 1)
	  (while (not (looking-at org-dblock-end-re))
	    (insert indent)
	    (beginning-of-line 2))
	  (when (looking-at org-dblock-end-re)
	    (and (looking-at "[ \t]+")
		 (replace-match ""))
	    (insert indent)))))))

(defun org-beginning-of-dblock ()
  "Find the beginning of the dynamic block at point.
Error if there is no such block at point."
  (let ((pos (point))
	beg)
    (end-of-line 1)
    (if (and (re-search-backward org-dblock-start-re nil t)
	     (setq beg (match-beginning 0))
	     (re-search-forward org-dblock-end-re nil t)
	     (> (match-end 0) pos))
	(goto-char beg)
      (goto-char pos)
      (error "Not in a dynamic block"))))

(defun org-update-all-dblocks ()
  "Update all dynamic blocks in the buffer.
This function can be used in a hook."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (org-map-dblocks 'org-update-dblock)))


;;;; Completion

(declare-function org-export-backend-options "ox" (cl-x) t)
(defun org-get-export-keywords ()
  "Return a list of all currently understood export keywords.
Export keywords include options, block names, attributes and
keywords relative to each registered export back-end."
  (let (keywords)
    (dolist (backend
	     (bound-and-true-p org-export-registered-backends)
	     (delq nil keywords))
      ;; Back-end name (for keywords, like #+LATEX:)
      (push (upcase (symbol-name (org-export-backend-name backend))) keywords)
      (dolist (option-entry (org-export-backend-options backend))
	;; Back-end options.
	(push (nth 1 option-entry) keywords)))))

(defconst org-options-keywords
  '("ARCHIVE:" "AUTHOR:" "BIND:" "CATEGORY:" "COLUMNS:" "CREATOR:" "DATE:"
    "DESCRIPTION:" "DRAWERS:" "EMAIL:" "EXCLUDE_TAGS:" "FILETAGS:" "INCLUDE:"
    "INDEX:" "KEYWORDS:" "LANGUAGE:" "MACRO:" "OPTIONS:" "PROPERTY:"
    "PRIORITIES:" "SELECT_TAGS:" "SEQ_TODO:" "SETUPFILE:" "STARTUP:" "TAGS:"
    "TITLE:" "TODO:" "TYP_TODO:" "SELECT_TAGS:" "EXCLUDE_TAGS:"))

(defcustom org-structure-template-alist
  '(("s" "#+BEGIN_SRC ?\n\n#+END_SRC")
    ("e" "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE")
    ("q" "#+BEGIN_QUOTE\n?\n#+END_QUOTE")
    ("v" "#+BEGIN_VERSE\n?\n#+END_VERSE")
    ("V" "#+BEGIN_VERBATIM\n?\n#+END_VERBATIM")
    ("c" "#+BEGIN_CENTER\n?\n#+END_CENTER")
    ("C" "#+BEGIN_COMMENT\n?\n#+END_COMMENT")
    ("l" "#+BEGIN_EXPORT latex\n?\n#+END_EXPORT")
    ("L" "#+LaTeX: ")
    ("h" "#+BEGIN_EXPORT html\n?\n#+END_EXPORT")
    ("H" "#+HTML: ")
    ("a" "#+BEGIN_EXPORT ascii\n?\n#+END_EXPORT")
    ("A" "#+ASCII: ")
    ("i" "#+INDEX: ?")
    ("I" "#+INCLUDE: %file ?"))
  "Structure completion elements.
This is a list of abbreviation keys and values.  The value gets inserted
if you type `<' followed by the key and then press the completion key,
usually `TAB'.  %file will be replaced by a file name after prompting
for the file using completion.  The cursor will be placed at the position
of the `?' in the template.
There are two templates for each key, the first uses the original Org syntax,
the second uses Emacs Muse-like syntax tags.  These Muse-like tags become
the default when the /org-mtags.el/ module has been loaded.  See also the
variable `org-mtags-prefer-muse-templates'."
  :group 'org-completion
  :type '(repeat
	  (list
	   (string :tag "Key")
	   (string :tag "Template")))
  :version "26.1"
  :package-version '(Org . "8.3"))

(defun org-try-structure-completion ()
  "Try to complete a structure template before point.
This looks for strings like \"<e\" on an otherwise empty line and
expands them."
  (let ((l (buffer-substring (point-at-bol) (point)))
	a)
    (when (and (looking-at "[ \t]*$")
	       (string-match "^[ \t]*<\\([a-zA-Z]+\\)$" l)
	       (setq a (assoc (match-string 1 l) org-structure-template-alist)))
      (org-complete-expand-structure-template (+ -1 (point-at-bol)
						 (match-beginning 1)) a)
      t)))

(defun org-complete-expand-structure-template (start cell)
  "Expand a structure template."
  (let ((rpl (nth 1 cell))
	(ind ""))
    (delete-region start (point))
    (when (string-match "\\`[ \t]*#\\+" rpl)
      (cond
       ((bolp))
       ((not (string-match "\\S-" (buffer-substring (point-at-bol) (point))))
	(setq ind (buffer-substring (point-at-bol) (point))))
       (t (newline))))
    (setq start (point))
    (when (string-match "%file" rpl)
      (setq rpl (replace-match
		 (concat
		  "\""
		  (save-match-data
		    (abbreviate-file-name (read-file-name "Include file: ")))
		  "\"")
		 t t rpl)))
    (setq rpl (mapconcat 'identity (split-string rpl "\n")
			 (concat "\n" ind)))
    (insert rpl)
    (when (re-search-backward "\\?" start t) (delete-char 1))))

;;;; TODO, DEADLINE, Comments

(defun org-toggle-comment ()
  "Change the COMMENT state of an entry."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let ((case-fold-search nil))
      (looking-at org-complex-heading-regexp))
    (goto-char (or (match-end 3) (match-end 2) (match-end 1)))
    (skip-chars-forward " \t")
    (unless (memq (char-before) '(?\s ?\t)) (insert " "))
    (if (org-in-commented-heading-p t)
	(delete-region (point)
		       (progn (search-forward " " (line-end-position) 'move)
			      (skip-chars-forward " \t")
			      (point)))
      (insert org-comment-string)
      (unless (eolp) (insert " ")))))

(defvar org-last-todo-state-is-todo nil
  "This is non-nil when the last TODO state change led to a TODO state.
If the last change removed the TODO tag or switched to DONE, then
this is nil.")

(defvar org-setting-tags nil) ; dynamically skipped

(defvar org-todo-setup-filter-hook nil
  "Hook for functions that pre-filter todo specs.
Each function takes a todo spec and returns either nil or the spec
transformed into canonical form." )

(defvar org-todo-get-default-hook nil
  "Hook for functions that get a default item for todo.
Each function takes arguments (NEW-MARK OLD-MARK) and returns either
nil or a string to be used for the todo mark." )

(defvar org-agenda-headline-snapshot-before-repeat)

(defun org-current-effective-time ()
  "Return current time adjusted for `org-extend-today-until' variable."
  (let* ((ct (org-current-time))
	 (dct (decode-time ct))
	 (ct1
	  (cond
	   (org-use-last-clock-out-time-as-effective-time
	    (or (org-clock-get-last-clock-out-time) ct))
	   ((and org-use-effective-time (< (nth 2 dct) org-extend-today-until))
	    (encode-time 0 59 23 (1- (nth 3 dct)) (nth 4 dct) (nth 5 dct)))
	   (t ct))))
    ct1))

(defun org-todo-yesterday (&optional arg)
  "Like `org-todo' but the time of change will be 23:59 of yesterday."
  (interactive "P")
  (if (eq major-mode 'org-agenda-mode)
      (apply 'org-agenda-todo-yesterday arg)
    (let* ((org-use-effective-time t)
	   (hour (nth 2 (decode-time (org-current-time))))
	   (org-extend-today-until (1+ hour)))
      (org-todo arg))))

(defvar org-block-entry-blocking ""
  "First entry preventing the TODO state change.")

(defun org-cancel-repeater ()
  "Cancel a repeater by setting its numeric value to zero."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((bound1 (point))
	  (bound0 (save-excursion (outline-next-heading) (point))))
      (when (and (re-search-forward
		  (concat "\\(" org-scheduled-time-regexp "\\)\\|\\("
			  org-deadline-time-regexp "\\)\\|\\("
			  org-ts-regexp "\\)")
		  bound0 t)
		 (re-search-backward "[ \t]+\\(?:[.+]\\)?\\+\\([0-9]+\\)[hdwmy]"
				     bound1 t))
	(replace-match "0" t nil nil 1)))))

(defvar org-state)
(defvar org-blocked-by-checkboxes)
(defun org-todo (&optional arg)
  "Change the TODO state of an item.

The state of an item is given by a keyword at the start of the heading,
like
     *** TODO Write paper
     *** DONE Call mom

The different keywords are specified in the variable `org-todo-keywords'.
By default the available states are \"TODO\" and \"DONE\".  So, for this
example: when the item starts with TODO, it is changed to DONE.
When it starts with DONE, the DONE is removed.  And when neither TODO nor
DONE are present, add TODO at the beginning of the heading.

With `\\[universal-argument]' prefix ARG, use completion to determine the new \
state.
With numeric prefix ARG, switch to that state.
With a `\\[universal-argument] \\[universal-argument]' prefix, switch to the \
next set of TODO \
keywords (nextset).
With a `\\[universal-argument] \\[universal-argument] \\[universal-argument]' \
prefix, circumvent any state blocking.
With a numeric prefix arg of 0, inhibit note taking for the change.
With a numeric prefix arg of -1, cancel repeater to allow marking as DONE.

When called through ELisp, arg is also interpreted in the following way:
`none'        -> empty state
\"\"            -> switch to empty state
`done'        -> switch to DONE
`nextset'     -> switch to the next set of keywords
`previousset' -> switch to the previous set of keywords
\"WAITING\"     -> switch to the specified keyword, but only if it
                 really is a member of `org-todo-keywords'."
  (interactive "P")
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
		    'region-start-level 'region))
	    org-loop-over-headlines-in-active-region)
	(org-map-entries
	 `(org-todo ,arg)
	 org-loop-over-headlines-in-active-region
	 cl (when (org-invisible-p) (org-end-of-subtree nil t))))
    (when (equal arg '(16)) (setq arg 'nextset))
    (when (equal arg -1) (org-cancel-repeater) (setq arg nil))
    (let ((org-blocker-hook org-blocker-hook)
	  commentp
	  case-fold-search)
      (when (equal arg '(64))
	(setq arg nil org-blocker-hook nil))
      (when (and org-blocker-hook
		 (or org-inhibit-blocking
		     (org-entry-get nil "NOBLOCKING")))
	(setq org-blocker-hook nil))
      (save-excursion
	(catch 'exit
	  (org-back-to-heading t)
	  (when (org-in-commented-heading-p t)
	    (org-toggle-comment)
	    (setq commentp t))
	  (when (looking-at org-outline-regexp) (goto-char (1- (match-end 0))))
	  (or (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
	      (looking-at "\\(?: *\\|[ \t]*$\\)"))
	  (let* ((match-data (match-data))
		 (startpos (point-at-bol))
		 (logging (save-match-data (org-entry-get nil "LOGGING" t t)))
		 (org-log-done org-log-done)
		 (org-log-repeat org-log-repeat)
		 (org-todo-log-states org-todo-log-states)
		 (org-inhibit-logging
		  (if (equal arg 0)
		      (progn (setq arg nil) 'note) org-inhibit-logging))
		 (this (match-string 1))
		 (hl-pos (match-beginning 0))
		 (head (org-get-todo-sequence-head this))
		 (ass (assoc head org-todo-kwd-alist))
		 (interpret (nth 1 ass))
		 (done-word (nth 3 ass))
		 (final-done-word (nth 4 ass))
		 (org-last-state (or this ""))
		 (completion-ignore-case t)
		 (member (member this org-todo-keywords-1))
		 (tail (cdr member))
		 (org-state (cond
			     ((and org-todo-key-trigger
				   (or (and (equal arg '(4))
					    (eq org-use-fast-todo-selection 'prefix))
				       (and (not arg) org-use-fast-todo-selection
					    (not (eq org-use-fast-todo-selection
						     'prefix)))))
			      ;; Use fast selection.
			      (org-fast-todo-selection))
			     ((and (equal arg '(4))
				   (or (not org-use-fast-todo-selection)
				       (not org-todo-key-trigger)))
			      ;; Read a state with completion.
			      (completing-read
			       "State: " (mapcar #'list org-todo-keywords-1)
			       nil t))
			     ((eq arg 'right)
			      (if this
				  (if tail (car tail) nil)
				(car org-todo-keywords-1)))
			     ((eq arg 'left)
			      (unless (equal member org-todo-keywords-1)
				(if this
				    (nth (- (length org-todo-keywords-1)
					    (length tail) 2)
					 org-todo-keywords-1)
				  (org-last org-todo-keywords-1))))
			     ((and (eq org-use-fast-todo-selection t) (equal arg '(4))
				   (setq arg nil))) ;hack to fall back to cycling
			     (arg
			      ;; User or caller requests a specific state.
			      (cond
			       ((equal arg "") nil)
			       ((eq arg 'none) nil)
			       ((eq arg 'done) (or done-word (car org-done-keywords)))
			       ((eq arg 'nextset)
				(or (car (cdr (member head org-todo-heads)))
				    (car org-todo-heads)))
			       ((eq arg 'previousset)
				(let ((org-todo-heads (reverse org-todo-heads)))
				  (or (car (cdr (member head org-todo-heads)))
				      (car org-todo-heads))))
			       ((car (member arg org-todo-keywords-1)))
			       ((stringp arg)
				(user-error "State `%s' not valid in this file" arg))
			       ((nth (1- (prefix-numeric-value arg))
				     org-todo-keywords-1))))
			     ((null member) (or head (car org-todo-keywords-1)))
			     ((equal this final-done-word) nil) ;-> make empty
			     ((null tail) nil) ;-> first entry
			     ((memq interpret '(type priority))
			      (if (eq this-command last-command)
				  (car tail)
				(if (> (length tail) 0)
				    (or done-word (car org-done-keywords))
				  nil)))
			     (t
			      (car tail))))
		 (org-state (or
			     (run-hook-with-args-until-success
			      'org-todo-get-default-hook org-state org-last-state)
			     org-state))
		 (next (if org-state (concat " " org-state " ") " "))
		 (change-plist (list :type 'todo-state-change :from this :to org-state
				     :position startpos))
		 dolog now-done-p)
	    (when org-blocker-hook
	      (let (org-blocked-by-checkboxes block-reason)
		(setq org-last-todo-state-is-todo
		      (not (member this org-done-keywords)))
		(unless (save-excursion
			  (save-match-data
			    (org-with-wide-buffer
			     (run-hook-with-args-until-failure
			      'org-blocker-hook change-plist))))
		  (setq block-reason (if org-blocked-by-checkboxes
					 "contained checkboxes"
				       (format "\"%s\"" org-block-entry-blocking)))
		  (if (called-interactively-p 'interactive)
		      (user-error "TODO state change from %s to %s blocked (by %s)"
				  this org-state block-reason)
		    ;; Fail silently.
		    (message "TODO state change from %s to %s blocked (by %s)"
			     this org-state block-reason)
		    (throw 'exit nil)))))
	    (store-match-data match-data)
	    (replace-match next t t)
	    (cond ((equal this org-state)
		   (message "TODO state was already %s" (org-trim next)))
		  ((not (pos-visible-in-window-p hl-pos))
		   (message "TODO state changed to %s" (org-trim next))))
	    (unless head
	      (setq head (org-get-todo-sequence-head org-state)
		    ass (assoc head org-todo-kwd-alist)
		    interpret (nth 1 ass)
		    done-word (nth 3 ass)
		    final-done-word (nth 4 ass)))
	    (when (memq arg '(nextset previousset))
	      (message "Keyword-Set %d/%d: %s"
		       (- (length org-todo-sets) -1
			  (length (memq (assoc org-state org-todo-sets) org-todo-sets)))
		       (length org-todo-sets)
		       (mapconcat 'identity (assoc org-state org-todo-sets) " ")))
	    (setq org-last-todo-state-is-todo
		  (not (member org-state org-done-keywords)))
	    (setq now-done-p (and (member org-state org-done-keywords)
				  (not (member this org-done-keywords))))
	    (and logging (org-local-logging logging))
	    (when (and (or org-todo-log-states org-log-done)
		       (not (eq org-inhibit-logging t))
		       (not (memq arg '(nextset previousset))))
	      ;; We need to look at recording a time and note.
	      (setq dolog (or (nth 1 (assoc org-state org-todo-log-states))
			      (nth 2 (assoc this org-todo-log-states))))
	      (when (and (eq dolog 'note) (eq org-inhibit-logging 'note))
		(setq dolog 'time))
	      (when (or (and (not org-state) (not org-closed-keep-when-no-todo))
			(and org-state
			     (member org-state org-not-done-keywords)
			     (not (member this org-not-done-keywords))))
		;; This is now a todo state and was not one before
		;; If there was a CLOSED time stamp, get rid of it.
		(org-add-planning-info nil nil 'closed))
	      (when (and now-done-p org-log-done)
		;; It is now done, and it was not done before.
		(org-add-planning-info 'closed (org-current-effective-time))
		(when (and (not dolog) (eq 'note org-log-done))
		  (org-add-log-setup 'done org-state this 'note)))
	      (when (and org-state dolog)
		;; This is a non-nil state, and we need to log it.
		(org-add-log-setup 'state org-state this dolog)))
	    ;; Fixup tag positioning.
	    (org-todo-trigger-tag-changes org-state)
	    (and org-auto-align-tags (not org-setting-tags) (org-set-tags nil t))
	    (when org-provide-todo-statistics
	      (org-update-parent-todo-statistics))
	    (run-hooks 'org-after-todo-state-change-hook)
	    (when (and arg (not (member org-state org-done-keywords)))
	      (setq head (org-get-todo-sequence-head org-state)))
	    (put-text-property (point-at-bol) (point-at-eol) 'org-todo-head head)
	    ;; Do we need to trigger a repeat?
	    (when now-done-p
	      (when (boundp 'org-agenda-headline-snapshot-before-repeat)
		;; This is for the agenda, take a snapshot of the headline.
		(save-match-data
		  (setq org-agenda-headline-snapshot-before-repeat
			(org-get-heading))))
	      (org-auto-repeat-maybe org-state))
	    ;; Fixup cursor location if close to the keyword.
	    (when (and (outline-on-heading-p)
		       (not (bolp))
		       (save-excursion (beginning-of-line 1)
				       (looking-at org-todo-line-regexp))
		       (< (point) (+ 2 (or (match-end 2) (match-end 1)))))
	      (goto-char (or (match-end 2) (match-end 1)))
	      (and (looking-at " ") (just-one-space)))
	    (when org-trigger-hook
	      (save-excursion
		(run-hook-with-args 'org-trigger-hook change-plist)))
	    (when commentp (org-toggle-comment))))))))

(defun org-block-todo-from-children-or-siblings-or-parent (change-plist)
  "Block turning an entry into a TODO, using the hierarchy.
This checks whether the current task should be blocked from state
changes.  Such blocking occurs when:

  1. The task has children which are not all in a completed state.

  2. A task has a parent with the property :ORDERED:, and there
     are siblings prior to the current task with incomplete
     status.

  3. The parent of the task is blocked because it has siblings that should
     be done first, or is child of a block grandparent TODO entry."

  (if (not org-enforce-todo-dependencies)
      t ; if locally turned off don't block
    (catch 'dont-block
      ;; If this is not a todo state change, or if this entry is already DONE,
      ;; do not block
      (when (or (not (eq (plist-get change-plist :type) 'todo-state-change))
		(member (plist-get change-plist :from)
			(cons 'done org-done-keywords))
		(member (plist-get change-plist :to)
			(cons 'todo org-not-done-keywords))
		(not (plist-get change-plist :to)))
	(throw 'dont-block t))
      ;; If this task has children, and any are undone, it's blocked
      (save-excursion
	(org-back-to-heading t)
	(let ((this-level (funcall outline-level)))
	  (outline-next-heading)
	  (let ((child-level (funcall outline-level)))
	    (while (and (not (eobp))
			(> child-level this-level))
	      ;; this todo has children, check whether they are all
	      ;; completed
	      (when (and (not (org-entry-is-done-p))
			 (org-entry-is-todo-p))
		(setq org-block-entry-blocking (org-get-heading))
		(throw 'dont-block nil))
	      (outline-next-heading)
	      (setq child-level (funcall outline-level))))))
      ;; Otherwise, if the task's parent has the :ORDERED: property, and
      ;; any previous siblings are undone, it's blocked
      (save-excursion
	(org-back-to-heading t)
	(let* ((pos (point))
	       (parent-pos (and (org-up-heading-safe) (point)))
	       (case-fold-search nil))
	  (unless parent-pos (throw 'dont-block t)) ; no parent
	  (when (and (org-not-nil (org-entry-get (point) "ORDERED"))
		     (forward-line 1)
		     (re-search-forward org-not-done-heading-regexp pos t))
	    (setq org-block-entry-blocking (match-string 0))
	    (throw 'dont-block nil))  ; block, there is an older sibling not done.
	  ;; Search further up the hierarchy, to see if an ancestor is blocked
	  (while t
	    (goto-char parent-pos)
	    (unless (looking-at org-not-done-heading-regexp)
	      (throw 'dont-block t))	; do not block, parent is not a TODO
	    (setq pos (point))
	    (setq parent-pos (and (org-up-heading-safe) (point)))
	    (unless parent-pos (throw 'dont-block t)) ; no parent
	    (when (and (org-not-nil (org-entry-get (point) "ORDERED"))
		       (forward-line 1)
		       (re-search-forward org-not-done-heading-regexp pos t)
		       (setq org-block-entry-blocking (org-get-heading)))
	      (throw 'dont-block nil)))))))) ; block, older sibling not done.

(defcustom org-track-ordered-property-with-tag nil
  "Should the ORDERED property also be shown as a tag?
The ORDERED property decides if an entry should require subtasks to be
completed in sequence.  Since a property is not very visible, setting
this option means that toggling the ORDERED property with the command
`org-toggle-ordered-property' will also toggle a tag ORDERED.  That tag is
not relevant for the behavior, but it makes things more visible.

Note that toggling the tag with tags commands will not change the property
and therefore not influence behavior!

This can be t, meaning the tag ORDERED should be used,  It can also be a
string to select a different tag for this task."
  :group 'org-todo
  :type '(choice
	  (const :tag "No tracking" nil)
	  (const :tag "Track with ORDERED tag" t)
	  (string :tag "Use other tag")))

(defun org-toggle-ordered-property ()
  "Toggle the ORDERED property of the current entry.
For better visibility, you can track the value of this property with a tag.
See variable `org-track-ordered-property-with-tag'."
  (interactive)
  (let* ((t1 org-track-ordered-property-with-tag)
	 (tag (and t1 (if (stringp t1) t1 "ORDERED"))))
    (save-excursion
      (org-back-to-heading)
      (if (org-entry-get nil "ORDERED")
	  (progn
	    (org-delete-property "ORDERED")
	    (and tag (org-toggle-tag tag 'off))
	    (message "Subtasks can be completed in arbitrary order"))
	(org-entry-put nil "ORDERED" "t")
	(and tag (org-toggle-tag tag 'on))
	(message "Subtasks must be completed in sequence")))))

(defun org-block-todo-from-checkboxes (change-plist)
  "Block turning an entry into a TODO, using checkboxes.
This checks whether the current task should be blocked from state
changes because there are unchecked boxes in this entry."
  (if (not org-enforce-todo-checkbox-dependencies)
      t ; if locally turned off don't block
    (catch 'dont-block
      ;; If this is not a todo state change, or if this entry is already DONE,
      ;; do not block
      (when (or (not (eq (plist-get change-plist :type) 'todo-state-change))
		(member (plist-get change-plist :from)
			(cons 'done org-done-keywords))
		(member (plist-get change-plist :to)
			(cons 'todo org-not-done-keywords))
		(not (plist-get change-plist :to)))
	(throw 'dont-block t))
      ;; If this task has checkboxes that are not checked, it's blocked
      (save-excursion
	(org-back-to-heading t)
	(let ((beg (point)) end)
	  (outline-next-heading)
	  (setq end (point))
	  (goto-char beg)
	  (when (org-list-search-forward
		 (concat (org-item-beginning-re)
			 "\\(?:\\[@\\(?:start:\\)?\\([0-9]+\\|[A-Za-z]\\)\\][ \t]*\\)?"
			 "\\[[- ]\\]")
		 end t)
	    (when (boundp 'org-blocked-by-checkboxes)
	      (setq org-blocked-by-checkboxes t))
	    (throw 'dont-block nil))))
      t))) ; do not block

(defun org-entry-blocked-p ()
  "Non-nil if entry at point is blocked."
  (and (not (org-entry-get nil "NOBLOCKING"))
       (member (org-entry-get nil "TODO") org-not-done-keywords)
       (not (run-hook-with-args-until-failure
	     'org-blocker-hook
	     (list :type 'todo-state-change
		   :position (point)
		   :from 'todo
		   :to 'done)))))

(defun org-update-statistics-cookies (all)
  "Update the statistics cookie, either from TODO or from checkboxes.
This should be called with the cursor in a line with a statistics
cookie.  When called with a \\[universal-argument] prefix, update
all statistics cookies in the buffer."
  (interactive "P")
  (if all
      (progn
	(org-update-checkbox-count 'all)
	(org-map-entries 'org-update-parent-todo-statistics))
    (if (not (org-at-heading-p))
	(org-update-checkbox-count)
      (let ((pos (point-marker))
	    end l1 l2)
	(ignore-errors (org-back-to-heading t))
	(if (not (org-at-heading-p))
	    (org-update-checkbox-count)
	  (setq l1 (org-outline-level))
	  (setq end (save-excursion
		      (outline-next-heading)
		      (when (org-at-heading-p) (setq l2 (org-outline-level)))
		      (point)))
	  (if (and (save-excursion
		     (re-search-forward
		      "^[ \t]*\\([-+*]\\|[0-9]+[.)]\\) \\[[- X]\\]" end t))
		   (not (save-excursion (re-search-forward
					 ":COOKIE_DATA:.*\\<todo\\>" end t))))
	      (org-update-checkbox-count)
	    (if (and l2 (> l2 l1))
		(progn
		  (goto-char end)
		  (org-update-parent-todo-statistics))
	      (goto-char pos)
	      (beginning-of-line 1)
	      (while (re-search-forward
		      "\\(\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)\\)"
		      (point-at-eol) t)
		(replace-match (if (match-end 2) "[100%]" "[0/0]") t t)))))
	(goto-char pos)
	(move-marker pos nil)))))

(defvar org-entry-property-inherited-from) ;; defined below
(defun org-update-parent-todo-statistics ()
  "Update any statistics cookie in the parent of the current headline.
When `org-hierarchical-todo-statistics' is nil, statistics will cover
the entire subtree and this will travel up the hierarchy and update
statistics everywhere."
  (let* ((prop (save-excursion (org-up-heading-safe)
			       (org-entry-get nil "COOKIE_DATA" 'inherit)))
	 (recursive (or (not org-hierarchical-todo-statistics)
			(and prop (string-match "\\<recursive\\>" prop))))
	 (lim (or (and prop (marker-position org-entry-property-inherited-from))
		  0))
	 (first t)
	 (box-re "\\(\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)\\)")
	 level ltoggle l1 new ndel
	 (cnt-all 0) (cnt-done 0) is-percent kwd
	 checkbox-beg cookie-present)
    (catch 'exit
      (save-excursion
	(beginning-of-line 1)
	(setq ltoggle (funcall outline-level))
	;; Three situations are to consider:

	;; 1. if `org-hierarchical-todo-statistics' is nil, repeat up
	;;    to the top-level ancestor on the headline;

	;; 2. If parent has "recursive" property, repeat up to the
	;;    headline setting that property, taking inheritance into
	;;    account;

	;; 3. Else, move up to direct parent and proceed only once.
	(while (and (setq level (org-up-heading-safe))
		    (or recursive first)
		    (>= (point) lim))
	  (setq first nil cookie-present nil)
	  (unless (and level
		       (not (string-match
			     "\\<checkbox\\>"
			     (downcase (or (org-entry-get nil "COOKIE_DATA")
					   "")))))
	    (throw 'exit nil))
	  (while (re-search-forward box-re (point-at-eol) t)
	    (setq cnt-all 0 cnt-done 0 cookie-present t)
	    (setq is-percent (match-end 2) checkbox-beg (match-beginning 0))
	    (save-match-data
	      (unless (outline-next-heading) (throw 'exit nil))
	      (while (and (looking-at org-complex-heading-regexp)
	    		  (> (setq l1 (length (match-string 1))) level))
	    	(setq kwd (and (or recursive (= l1 ltoggle))
	    		       (match-string 2)))
	    	(if (or (eq org-provide-todo-statistics 'all-headlines)
			(and (eq org-provide-todo-statistics t)
			     (or (member kwd org-done-keywords)))
	    		(and (listp org-provide-todo-statistics)
			     (stringp (car org-provide-todo-statistics))
	    		     (or (member kwd org-provide-todo-statistics)
				 (member kwd org-done-keywords)))
			(and (listp org-provide-todo-statistics)
			     (listp (car org-provide-todo-statistics))
			     (or (member kwd (car org-provide-todo-statistics))
				 (and (member kwd org-done-keywords)
				      (member kwd (cadr org-provide-todo-statistics))))))
	    	    (setq cnt-all (1+ cnt-all))
		  (and (eq org-provide-todo-statistics t)
		       kwd
		       (setq cnt-all (1+ cnt-all))))
		(when (or (and (member org-provide-todo-statistics '(t all-headlines))
			       (member kwd org-done-keywords))
			  (and (listp org-provide-todo-statistics)
			       (listp (car org-provide-todo-statistics))
			       (member kwd org-done-keywords)
			       (member kwd (cadr org-provide-todo-statistics)))
			  (and (listp org-provide-todo-statistics)
			       (stringp (car org-provide-todo-statistics))
			       (member kwd org-done-keywords)))
		  (setq cnt-done (1+ cnt-done)))
	    	(outline-next-heading)))
	    (setq new
	    	  (if is-percent
		      (format "[%d%%]" (floor (* 100.0 cnt-done)
					      (max 1 cnt-all)))
	    	    (format "[%d/%d]" cnt-done cnt-all))
	    	  ndel (- (match-end 0) checkbox-beg))
	    (goto-char checkbox-beg)
	    (insert new)
	    (delete-region (point) (+ (point) ndel))
	    (when org-auto-align-tags (org-fix-tags-on-the-fly)))
	  (when cookie-present
	    (run-hook-with-args 'org-after-todo-statistics-hook
				cnt-done (- cnt-all cnt-done))))))
    (run-hooks 'org-todo-statistics-hook)))

(defvar org-after-todo-statistics-hook nil
  "Hook that is called after a TODO statistics cookie has been updated.
Each function is called with two arguments: the number of not-done entries
and the number of done entries.

For example, the following function, when added to this hook, will switch
an entry to DONE when all children are done, and back to TODO when new
entries are set to a TODO status.  Note that this hook is only called
when there is a statistics cookie in the headline!

 (defun org-summary-todo (n-done n-not-done)
   \"Switch entry to DONE when all subentries are done, to TODO otherwise.\"
   (let (org-log-done org-log-states)   ; turn off logging
     (org-todo (if (= n-not-done 0) \"DONE\" \"TODO\"))))
")

(defvar org-todo-statistics-hook nil
  "Hook that is run whenever Org thinks TODO statistics should be updated.
This hook runs even if there is no statistics cookie present, in which case
`org-after-todo-statistics-hook' would not run.")

(defun org-todo-trigger-tag-changes (state)
  "Apply the changes defined in `org-todo-state-tags-triggers'."
  (let ((l org-todo-state-tags-triggers)
	changes)
    (when (or (not state) (equal state ""))
      (setq changes (append changes (cdr (assoc "" l)))))
    (when (and (stringp state) (> (length state) 0))
      (setq changes (append changes (cdr (assoc state l)))))
    (when (member state org-not-done-keywords)
      (setq changes (append changes (cdr (assq 'todo l)))))
    (when (member state org-done-keywords)
      (setq changes (append changes (cdr (assq 'done l)))))
    (dolist (c changes)
      (org-toggle-tag (car c) (if (cdr c) 'on 'off)))))

(defun org-local-logging (value)
  "Get logging settings from a property VALUE."
  ;; Directly set the variables, they are already local.
  (setq org-log-done nil
        org-log-repeat nil
        org-todo-log-states nil)
  (dolist (w (split-string value))
    (let (a)
      (cond
       ((setq a (assoc w org-startup-options))
        (and (member (nth 1 a) '(org-log-done org-log-repeat))
             (set (nth 1 a) (nth 2 a))))
       ((setq a (org-extract-log-state-settings w))
        (and (member (car a) org-todo-keywords-1)
             (push a org-todo-log-states)))))))

(defun org-get-todo-sequence-head (kwd)
  "Return the head of the TODO sequence to which KWD belongs.
If KWD is not set, check if there is a text property remembering the
right sequence."
  (let (p)
    (cond
     ((not kwd)
      (or (get-text-property (point-at-bol) 'org-todo-head)
	  (progn
	    (setq p (next-single-property-change (point-at-bol) 'org-todo-head
						 nil (point-at-eol)))
	    (get-text-property p 'org-todo-head))))
     ((not (member kwd org-todo-keywords-1))
      (car org-todo-keywords-1))
     (t (nth 2 (assoc kwd org-todo-kwd-alist))))))

(defun org-fast-todo-selection ()
  "Fast TODO keyword selection with single keys.
Returns the new TODO keyword, or nil if no state change should occur."
  (let* ((fulltable org-todo-key-alist)
	 (done-keywords org-done-keywords) ;; needed for the faces.
	 (maxlen (apply 'max (mapcar
			      (lambda (x)
				(if (stringp (car x)) (string-width (car x)) 0))
			      fulltable)))
	 (expert nil)
	 (fwidth (+ maxlen 3 1 3))
	 (ncol (/ (- (window-width) 4) fwidth))
	 tg cnt e c tbl
	 groups ingroup)
    (save-excursion
      (save-window-excursion
	(if expert
	    (set-buffer (get-buffer-create " *Org todo*"))
	  (org-switch-to-buffer-other-window (get-buffer-create " *Org todo*")))
	(erase-buffer)
	(setq-local org-done-keywords done-keywords)
	(setq tbl fulltable cnt 0)
	(while (setq e (pop tbl))
	  (cond
	   ((equal e '(:startgroup))
	    (push '() groups) (setq ingroup t)
	    (unless (= cnt 0)
	      (setq cnt 0)
	      (insert "\n"))
	    (insert "{ "))
	   ((equal e '(:endgroup))
	    (setq ingroup nil cnt 0)
	    (insert "}\n"))
	   ((equal e '(:newline))
	    (unless (= cnt 0)
	      (setq cnt 0)
	      (insert "\n")
	      (setq e (car tbl))
	      (while (equal (car tbl) '(:newline))
		(insert "\n")
		(setq tbl (cdr tbl)))))
	   (t
	    (setq tg (car e) c (cdr e))
	    (when ingroup (push tg (car groups)))
	    (setq tg (org-add-props tg nil 'face
				    (org-get-todo-face tg)))
	    (when (and (= cnt 0) (not ingroup)) (insert "  "))
	    (insert "[" c "] " tg (make-string
				   (- fwidth 4 (length tg)) ?\ ))
	    (when (= (setq cnt (1+ cnt)) ncol)
	      (insert "\n")
	      (when ingroup (insert "  "))
	      (setq cnt 0)))))
	(insert "\n")
	(goto-char (point-min))
	(unless expert (org-fit-window-to-buffer))
	(message "[a-z..]:Set [SPC]:clear")
	(setq c (let ((inhibit-quit t)) (read-char-exclusive)))
	(cond
	 ((or (= c ?\C-g)
	      (and (= c ?q) (not (rassoc c fulltable))))
	  (setq quit-flag t))
	 ((= c ?\ ) nil)
	 ((setq e (rassoc c fulltable) tg (car e))
	  tg)
	 (t (setq quit-flag t)))))))

(defun org-entry-is-todo-p ()
  (member (org-get-todo-state) org-not-done-keywords))

(defun org-entry-is-done-p ()
  (member (org-get-todo-state) org-done-keywords))

(defun org-get-todo-state ()
  "Return the TODO keyword of the current subtree."
  (save-excursion
    (org-back-to-heading t)
    (and (let ((case-fold-search nil)) (looking-at org-todo-line-regexp))
	 (match-end 2)
	 (match-string 2))))

(defun org-at-date-range-p (&optional inactive-ok)
  "Non-nil if point is inside a date range.

When optional argument INACTIVE-OK is non-nil, also consider
inactive time ranges.

When this function returns a non-nil value, match data is set
according to `org-tr-regexp-both' or `org-tr-regexp', depending
on INACTIVE-OK."
  (interactive)
  (save-excursion
    (catch 'exit
      (let ((pos (point)))
	(skip-chars-backward "^[<\r\n")
	(skip-chars-backward "<[")
	(and (looking-at (if inactive-ok org-tr-regexp-both org-tr-regexp))
	     (>= (match-end 0) pos)
	     (throw 'exit t))
	(skip-chars-backward "^<[\r\n")
	(skip-chars-backward "<[")
	(and (looking-at (if inactive-ok org-tr-regexp-both org-tr-regexp))
	     (>= (match-end 0) pos)
	     (throw 'exit t)))
      nil)))

(defun org-get-repeat (&optional timestamp)
  "Check if there is a time-stamp with repeater in this entry.

Return the repeater, as a string, or nil.  Also return nil when
this function is called before first heading.

When optional argument TIMESTAMP is a string, extract the
repeater from there instead."
  (save-match-data
    (cond (timestamp
	   (and (string-match org-repeat-re timestamp)
		(match-string-no-properties 1 timestamp)))
	  ((org-before-first-heading-p) nil)
	  (t
	   (save-excursion
	     (org-back-to-heading t)
	     (let ((end (org-entry-end-position)))
	       (catch :repeat
		 (while (re-search-forward org-repeat-re end t)
		   (when (save-match-data (org-at-timestamp-p 'agenda))
		     (throw :repeat (match-string-no-properties 1)))))))))))

(defvar org-last-changed-timestamp)
(defvar org-last-inserted-timestamp)
(defvar org-log-post-message)
(defvar org-log-note-purpose)
(defvar org-log-note-how nil)
(defvar org-log-note-extra)
(defun org-auto-repeat-maybe (done-word)
  "Check if the current headline contains a repeated time-stamp.

If yes, set TODO state back to what it was and change the base date
of repeating deadline/scheduled time stamps to new date.

This function is run automatically after each state change to a DONE state."
  (let* ((repeat (org-get-repeat))
	 (aa (assoc org-last-state org-todo-kwd-alist))
	 (interpret (nth 1 aa))
	 (head (nth 2 aa))
	 (whata '(("h" . hour) ("d" . day) ("m" . month) ("y" . year)))
	 (msg "Entry repeats: ")
	 (org-log-done nil)
	 (org-todo-log-states nil)
	 (end (copy-marker (org-entry-end-position))))
    (unwind-protect
	(when (and repeat (not (zerop (string-to-number (substring repeat 1)))))
	  (when (eq org-log-repeat t) (setq org-log-repeat 'state))
	  (let ((to-state (or (org-entry-get nil "REPEAT_TO_STATE" 'selective)
			      org-todo-repeat-to-state)))
	    (org-todo (cond
		       ((and to-state (member to-state org-todo-keywords-1))
			to-state)
		       ((eq interpret 'type) org-last-state)
		       (head)
		       (t 'none))))
	  (org-back-to-heading t)
	  (org-add-planning-info nil nil 'closed)
	  ;; When `org-log-repeat' is non-nil or entry contains
	  ;; a clock, set LAST_REPEAT property.
	  (when (or org-log-repeat
		    (catch :clock
		      (save-excursion
			(while (re-search-forward org-clock-line-re end t)
			  (when (org-at-clock-log-p) (throw :clock t))))))
	    (org-entry-put nil "LAST_REPEAT" (format-time-string
					      (org-time-stamp-format t t)
					      (current-time))))
	  (when org-log-repeat
	    (if (or (memq 'org-add-log-note (default-value 'post-command-hook))
		    (memq 'org-add-log-note post-command-hook))
		;; We are already setup for some record.
		(when (eq org-log-repeat 'note)
		  ;; Make sure we take a note, not only a time stamp.
		  (setq org-log-note-how 'note))
	      ;; Set up for taking a record.
	      (org-add-log-setup 'state
				 (or done-word (car org-done-keywords))
				 org-last-state
				 org-log-repeat)))
	  (let ((planning-re (regexp-opt
			      (list org-scheduled-string org-deadline-string))))
	    (while (re-search-forward org-ts-regexp end t)
	      (let* ((ts (match-string 0))
		     (planning? (org-at-planning-p))
		     (type (if (not planning?) "Plain:"
			     (save-excursion
			       (re-search-backward
				planning-re (line-beginning-position) t)
			       (match-string 0)))))
		(cond
		 ;; Ignore fake time-stamps (e.g., within comments).
		 ((not (org-at-timestamp-p 'agenda)))
		 ;; Time-stamps without a repeater are usually
		 ;; skipped.  However, a SCHEDULED time-stamp without
		 ;; one is removed, as they are no longer relevant.
		 ((not (string-match "\\([.+]\\)?\\(\\+[0-9]+\\)\\([hdwmy]\\)"
				     ts))
		  (when (equal type org-scheduled-string)
		    (org-remove-timestamp-with-keyword type)))
		 (t
		  (let ((n (string-to-number (match-string 2 ts)))
			(what (match-string 3 ts)))
		    (when (equal what "w") (setq n (* n 7) what "d"))
		    (when (and (equal what "h")
			       (not (string-match-p "[0-9]\\{1,2\\}:[0-9]\\{2\\}"
						    ts)))
		      (user-error
		       "Cannot repeat in Repeat in %d hour(s) because no hour \
has been set"
		       n))
		    ;; Preparation, see if we need to modify the start
		    ;; date for the change.
		    (when (match-end 1)
		      (let ((time (save-match-data
				    (org-time-string-to-time ts))))
			(cond
			 ((equal (match-string 1 ts) ".")
			  ;; Shift starting date to today
			  (org-timestamp-change
			   (- (org-today) (time-to-days time))
			   'day))
			 ((equal (match-string 1 ts) "+")
			  (let ((nshiftmax 10)
				(nshift 0))
			    (while (or (= nshift 0)
				       (not (time-less-p (current-time) time)))
			      (when (= (cl-incf nshift) nshiftmax)
				(or (y-or-n-p
				     (format "%d repeater intervals were not \
enough to shift date past today.  Continue? "
					     nshift))
				    (user-error "Abort")))
			      (org-timestamp-change n (cdr (assoc what whata)))
			      (org-in-regexp org-ts-regexp3)
			      (setq ts (match-string 1))
			      (setq time
				    (save-match-data
				      (org-time-string-to-time ts)))))
			  (org-timestamp-change (- n) (cdr (assoc what whata)))
			  ;; Rematch, so that we have everything in place
			  ;; for the real shift.
			  (org-in-regexp org-ts-regexp3)
			  (setq ts (match-string 1))
			  (string-match "\\([.+]\\)?\\(\\+[0-9]+\\)\\([hdwmy]\\)"
					ts)))))
		    (save-excursion
		      (org-timestamp-change n (cdr (assoc what whata)) nil t))
		    (setq msg
			  (concat
			   msg type " " org-last-changed-timestamp " "))))))))
	  (setq org-log-post-message msg)
	  (message "%s" msg))
      (set-marker end nil))))

(defun org-show-todo-tree (arg)
  "Make a compact tree which shows all headlines marked with TODO.
The tree will show the lines where the regexp matches, and all higher
headlines above the match.
With a `\\[universal-argument]' prefix, prompt for a regexp to match.
With a numeric prefix N, construct a sparse tree for the Nth element
of `org-todo-keywords-1'."
  (interactive "P")
  (let ((case-fold-search nil)
	(kwd-re
	 (cond ((null arg) org-not-done-regexp)
	       ((equal arg '(4))
		(let ((kwd
		       (completing-read "Keyword (or KWD1|KWD2|...): "
					(mapcar #'list org-todo-keywords-1))))
		  (concat "\\("
			  (mapconcat 'identity (org-split-string kwd "|") "\\|")
			  "\\)\\>")))
	       ((<= (prefix-numeric-value arg) (length org-todo-keywords-1))
		(regexp-quote (nth (1- (prefix-numeric-value arg))
				   org-todo-keywords-1)))
	       (t (user-error "Invalid prefix argument: %s" arg)))))
    (message "%d TODO entries found"
	     (org-occur (concat "^" org-outline-regexp " *" kwd-re )))))

(defun org--deadline-or-schedule (arg type time)
  "Insert DEADLINE or SCHEDULE information in current entry.
TYPE is either `deadline' or `scheduled'.  See `org-deadline' or
`org-schedule' for information about ARG and TIME arguments."
  (let* ((deadline? (eq type 'deadline))
	 (keyword (if deadline? org-deadline-string org-scheduled-string))
	 (log (if deadline? org-log-redeadline org-log-reschedule))
	 (old-date (org-entry-get nil (if deadline? "DEADLINE" "SCHEDULED")))
	 (old-date-time (and old-date (org-time-string-to-time old-date)))
	 ;; Save repeater cookie from either TIME or current scheduled
	 ;; time stamp.  We are going to insert it back at the end of
	 ;; the process.
	 (repeater (or (and (org-string-nw-p time)
			    ;; We use `org-repeat-re' because we need
			    ;; to tell the difference between a real
			    ;; repeater and a time delta, e.g. "+2d".
			    (string-match org-repeat-re time)
			    (match-string 1 time))
		       (and (org-string-nw-p old-date)
			    (string-match "\\([.+-]+[0-9]+[hdwmy]\
\\(?:[/ ][-+]?[0-9]+[hdwmy]\\)?\\)"
					  old-date)
			    (match-string 1 old-date)))))
    (pcase arg
      (`(4)
       (when (and old-date log)
	 (org-add-log-setup (if deadline? 'deldeadline 'delschedule)
			    nil old-date log))
       (org-remove-timestamp-with-keyword keyword)
       (message (if deadline? "Item no longer has a deadline."
		  "Item is no longer scheduled.")))
      (`(16)
       (save-excursion
	 (org-back-to-heading t)
	 (let ((regexp (if deadline? org-deadline-time-regexp
			 org-scheduled-time-regexp)))
	   (if (not (re-search-forward regexp (line-end-position 2) t))
	       (user-error (if deadline? "No deadline information to update"
			     "No scheduled information to update"))
	     (let* ((rpl0 (match-string 1))
		    (rpl (replace-regexp-in-string " -[0-9]+[hdwmy]" "" rpl0))
		    (msg (if deadline? "Warn starting from" "Delay until")))
	       (replace-match
		(concat keyword
			" <" rpl
			(format " -%dd"
				(abs (- (time-to-days
					 (save-match-data
					   (org-read-date
					    nil t nil msg old-date-time)))
					(time-to-days old-date-time))))
			">") t t))))))
      (_
       (org-add-planning-info type time 'closed)
       (when (and old-date
		  log
		  (not (equal old-date org-last-inserted-timestamp)))
	 (org-add-log-setup (if deadline? 'redeadline 'reschedule)
			    org-last-inserted-timestamp
			    old-date
			    log))
       (when repeater
	 (save-excursion
	   (org-back-to-heading t)
	   (when (re-search-forward
		  (concat keyword " " org-last-inserted-timestamp)
		  (line-end-position 2)
		  t)
	     (goto-char (1- (match-end 0)))
	     (insert " " repeater)
	     (setq org-last-inserted-timestamp
		   (concat (substring org-last-inserted-timestamp 0 -1)
			   " " repeater
			   (substring org-last-inserted-timestamp -1))))))
       (message (if deadline? "Deadline on %s" "Scheduled to %s")
		org-last-inserted-timestamp)))))

(defun org-deadline (arg &optional time)
  "Insert the \"DEADLINE:\" string with a timestamp to make a deadline.
With one universal prefix argument, remove any deadline from the item.
With two universal prefix arguments, prompt for a warning delay.
With argument TIME, set the deadline at the corresponding date.  TIME
can either be an Org date like \"2011-07-24\" or a delta like \"+2d\"."
  (interactive "P")
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (org-map-entries
       (lambda () (org--deadline-or-schedule arg 'deadline time))
       nil
       (if (eq org-loop-over-headlines-in-active-region 'start-level)
	   'region-start-level
	 'region)
       (lambda () (when (org-invisible-p) (org-end-of-subtree nil t))))
    (org--deadline-or-schedule arg 'deadline time)))

(defun org-schedule (arg &optional time)
  "Insert the SCHEDULED: string with a timestamp to schedule a TODO item.
With one universal prefix argument, remove any scheduling date from the item.
With two universal prefix arguments, prompt for a delay cookie.
With argument TIME, scheduled at the corresponding date.  TIME can
either be an Org date like \"2011-07-24\" or a delta like \"+2d\"."
  (interactive "P")
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (org-map-entries
       (lambda () (org--deadline-or-schedule arg 'scheduled time))
       nil
       (if (eq org-loop-over-headlines-in-active-region 'start-level)
	   'region-start-level
	 'region)
       (lambda () (when (org-invisible-p) (org-end-of-subtree nil t))))
    (org--deadline-or-schedule arg 'scheduled time)))

(defun org-get-scheduled-time (pom &optional inherit)
  "Get the scheduled time as a time tuple, of a format suitable
for calling org-schedule with, or if there is no scheduling,
returns nil."
  (let ((time (org-entry-get pom "SCHEDULED" inherit)))
    (when time
      (apply 'encode-time (org-parse-time-string time)))))

(defun org-get-deadline-time (pom &optional inherit)
  "Get the deadline as a time tuple, of a format suitable for
calling org-deadline with, or if there is no scheduling, returns
nil."
  (let ((time (org-entry-get pom "DEADLINE" inherit)))
    (when time
      (apply 'encode-time (org-parse-time-string time)))))

(defun org-remove-timestamp-with-keyword (keyword)
  "Remove all time stamps with KEYWORD in the current entry."
  (let ((re (concat "\\<" (regexp-quote keyword) " +<[^>\n]+>[ \t]*"))
	beg)
    (save-excursion
      (org-back-to-heading t)
      (setq beg (point))
      (outline-next-heading)
      (while (re-search-backward re beg t)
	(replace-match "")
	(if (and (string-match "\\S-" (buffer-substring (point-at-bol) (point)))
		 (equal (char-before) ?\ ))
	    (backward-delete-char 1)
	  (when (string-match "^[ \t]*$" (buffer-substring
					  (point-at-bol) (point-at-eol)))
	    (delete-region (point-at-bol)
			   (min (point-max) (1+ (point-at-eol))))))))))

(defvar org-time-was-given) ; dynamically scoped parameter
(defvar org-end-time-was-given) ; dynamically scoped parameter

(defun org-at-planning-p ()
  "Non-nil when point is on a planning info line."
  ;; This is as accurate and faster than `org-element-at-point' since
  ;; planning info location is fixed in the section.
  (org-with-wide-buffer
   (beginning-of-line)
   (and (looking-at-p org-planning-line-re)
	(eq (point)
	    (ignore-errors
	      (if (and (featurep 'org-inlinetask) (org-inlinetask-in-task-p))
		  (org-back-to-heading t)
		(org-with-limited-levels (org-back-to-heading t)))
	      (line-beginning-position 2))))))

(defun org-add-planning-info (what &optional time &rest remove)
  "Insert new timestamp with keyword in the planning line.
WHAT indicates what kind of time stamp to add.  It is a symbol
among `closed', `deadline', `scheduled' and nil.  TIME indicates
the time to use.  If none is given, the user is prompted for
a date.  REMOVE indicates what kind of entries to remove.  An old
WHAT entry will also be removed."
  (let (org-time-was-given org-end-time-was-given default-time default-input)
    (catch 'exit
      (when (and (memq what '(scheduled deadline))
		 (or (not time)
		     (and (stringp time)
			  (string-match "^[-+]+[0-9]" time))))
	;; Try to get a default date/time from existing timestamp
	(save-excursion
	  (org-back-to-heading t)
	  (let ((end (save-excursion (outline-next-heading) (point))) ts)
	    (when (re-search-forward (if (eq what 'scheduled)
					 org-scheduled-time-regexp
				       org-deadline-time-regexp)
				     end t)
	      (setq ts (match-string 1)
		    default-time (apply 'encode-time (org-parse-time-string ts))
		    default-input (and ts (org-get-compact-tod ts)))))))
      (when what
	(setq time
	      (if (stringp time)
		  ;; This is a string (relative or absolute), set
		  ;; proper date.
		  (apply #'encode-time
			 (org-read-date-analyze
			  time default-time (decode-time default-time)))
		;; If necessary, get the time from the user
		(or time (org-read-date nil 'to-time nil nil
					default-time default-input)))))

      (org-with-wide-buffer
       (org-back-to-heading t)
       (forward-line)
       (unless (bolp) (insert "\n"))
       (cond ((looking-at-p org-planning-line-re)
	      ;; Move to current indentation.
	      (skip-chars-forward " \t")
	      ;; Check if we have to remove something.
	      (dolist (type (if what (cons what remove) remove))
		(save-excursion
		  (when (re-search-forward
			 (cl-case type
			   (closed org-closed-time-regexp)
			   (deadline org-deadline-time-regexp)
			   (scheduled org-scheduled-time-regexp)
			   (otherwise
			    (error "Invalid planning type: %s" type)))
			 (line-end-position) t)
		    ;; Delete until next keyword or end of line.
		    (delete-region
		     (match-beginning 0)
		     (if (re-search-forward org-keyword-time-not-clock-regexp
					    (line-end-position)
					    t)
			 (match-beginning 0)
		       (line-end-position))))))
	      ;; If there is nothing more to add and no more keyword
	      ;; is left, remove the line completely.
	      (if (and (looking-at-p "[ \t]*$") (not what))
		  (delete-region (line-beginning-position)
				 (line-beginning-position 2))
		;; If we removed last keyword, do not leave trailing
		;; white space at the end of line.
		(let ((p (point)))
		  (save-excursion
		    (end-of-line)
		    (unless (= (skip-chars-backward " \t" p) 0)
		      (delete-region (point) (line-end-position)))))))
	     ((not what) (throw 'exit nil)) ; Nothing to do.
	     (t (insert-before-markers "\n")
		(backward-char 1)
		(when org-adapt-indentation
		  (indent-to-column (1+ (org-outline-level))))))
       (when what
	 ;; Insert planning keyword.
	 (insert (cl-case what
		   (closed org-closed-string)
		   (deadline org-deadline-string)
		   (scheduled org-scheduled-string)
		   (otherwise (error "Invalid planning type: %s" what)))
		 " ")
	 ;; Insert associated timestamp.
	 (let ((ts (org-insert-time-stamp
		    time
		    (or org-time-was-given
			(and (eq what 'closed) org-log-done-with-time))
		    (eq what 'closed)
		    nil nil (list org-end-time-was-given))))
	   (unless (eolp) (insert " "))
	   ts))))))

(defvar org-log-note-marker (make-marker)
  "Marker pointing at the entry where the note is to be inserted.")
(defvar org-log-note-purpose nil)
(defvar org-log-note-state nil)
(defvar org-log-note-previous-state nil)
(defvar org-log-note-extra nil)
(defvar org-log-note-window-configuration nil)
(defvar org-log-note-return-to (make-marker))
(defvar org-log-note-effective-time nil
  "Remembered current time so that dynamically scoped
`org-extend-today-until' affects timestamps in state change log")

(defvar org-log-post-message nil
  "Message to be displayed after a log note has been stored.
The auto-repeater uses this.")

(defun org-add-note ()
  "Add a note to the current entry.
This is done in the same way as adding a state change note."
  (interactive)
  (org-add-log-setup 'note))

(defun org-log-beginning (&optional create)
  "Return expected start of log notes in current entry.
When optional argument CREATE is non-nil, the function creates
a drawer to store notes, if necessary.  Returned position ignores
narrowing."
  (org-with-wide-buffer
   (let ((drawer (org-log-into-drawer)))
     (cond
      (drawer
       (org-end-of-meta-data)
       (let ((regexp (concat "^[ \t]*:" (regexp-quote drawer) ":[ \t]*$"))
	     (end (if (org-at-heading-p) (point)
		    (save-excursion (outline-next-heading) (point))))
	     (case-fold-search t))
	 (catch 'exit
	   ;; Try to find existing drawer.
	   (while (re-search-forward regexp end t)
	     (let ((element (org-element-at-point)))
	       (when (eq (org-element-type element) 'drawer)
		 (let ((cend  (org-element-property :contents-end element)))
		   (when (and (not org-log-states-order-reversed) cend)
		     (goto-char cend)))
		 (throw 'exit nil))))
	   ;; No drawer found.  Create one, if permitted.
	   (when create
	     (unless (bolp) (insert "\n"))
	     (let ((beg (point)))
	       (insert ":" drawer ":\n:END:\n")
	       (org-indent-region beg (point)))
	     (end-of-line -1)))))
      (t
       (org-end-of-meta-data org-log-state-notes-insert-after-drawers)
       (skip-chars-forward " \t\n")
       (beginning-of-line)
       (unless org-log-states-order-reversed
	 (org-skip-over-state-notes)
	 (skip-chars-backward " \t\n")
	 (forward-line)))))
   (if (bolp) (point) (line-beginning-position 2))))

(defun org-add-log-setup (&optional purpose state prev-state how extra)
  "Set up the post command hook to take a note.
If this is about to TODO state change, the new state is expected in STATE.
HOW is an indicator what kind of note should be created.
EXTRA is additional text that will be inserted into the notes buffer."
  (move-marker org-log-note-marker (point))
  (setq org-log-note-purpose purpose
	org-log-note-state state
	org-log-note-previous-state prev-state
	org-log-note-how how
	org-log-note-extra extra
	org-log-note-effective-time (org-current-effective-time))
  (add-hook 'post-command-hook 'org-add-log-note 'append))

(defun org-skip-over-state-notes ()
  "Skip past the list of State notes in an entry."
  (when (ignore-errors (goto-char (org-in-item-p)))
    (let* ((struct (org-list-struct))
	   (prevs (org-list-prevs-alist struct))
	   (regexp
	    (concat "[ \t]*- +"
		    (replace-regexp-in-string
		     " +" " +"
		     (org-replace-escapes
		      (regexp-quote (cdr (assq 'state org-log-note-headings)))
		      `(("%d" . ,org-ts-regexp-inactive)
			("%D" . ,org-ts-regexp)
			("%s" . "\"\\S-+\"")
			("%S" . "\"\\S-+\"")
			("%t" . ,org-ts-regexp-inactive)
			("%T" . ,org-ts-regexp)
			("%u" . ".*?")
			("%U" . ".*?")))))))
      (while (looking-at-p regexp)
	(goto-char (or (org-list-get-next-item (point) struct prevs)
		       (org-list-get-item-end (point) struct)))))))

(defun org-add-log-note (&optional _purpose)
  "Pop up a window for taking a note, and add this note later."
  (remove-hook 'post-command-hook 'org-add-log-note)
  (setq org-log-note-window-configuration (current-window-configuration))
  (delete-other-windows)
  (move-marker org-log-note-return-to (point))
  (pop-to-buffer-same-window (marker-buffer org-log-note-marker))
  (goto-char org-log-note-marker)
  (org-switch-to-buffer-other-window "*Org Note*")
  (erase-buffer)
  (if (memq org-log-note-how '(time state))
      (org-store-log-note)
    (let ((org-inhibit-startup t)) (org-mode))
    (insert (format "# Insert note for %s.
# Finish with C-c C-c, or cancel with C-c C-k.\n\n"
		    (cond
		     ((eq org-log-note-purpose 'clock-out) "stopped clock")
		     ((eq org-log-note-purpose 'done)  "closed todo item")
		     ((eq org-log-note-purpose 'state)
		      (format "state change from \"%s\" to \"%s\""
			      (or org-log-note-previous-state "")
			      (or org-log-note-state "")))
		     ((eq org-log-note-purpose 'reschedule)
		      "rescheduling")
		     ((eq org-log-note-purpose 'delschedule)
		      "no longer scheduled")
		     ((eq org-log-note-purpose 'redeadline)
		      "changing deadline")
		     ((eq org-log-note-purpose 'deldeadline)
		      "removing deadline")
		     ((eq org-log-note-purpose 'refile)
		      "refiling")
		     ((eq org-log-note-purpose 'note)
		      "this entry")
		     (t (error "This should not happen")))))
    (when org-log-note-extra (insert org-log-note-extra))
    (setq-local org-finish-function 'org-store-log-note)
    (run-hooks 'org-log-buffer-setup-hook)))

(defvar org-note-abort nil) ; dynamically scoped
(defun org-store-log-note ()
  "Finish taking a log note, and insert it to where it belongs."
  (let ((txt (prog1 (buffer-string)
	       (kill-buffer)))
	(note (cdr (assq org-log-note-purpose org-log-note-headings)))
	lines)
    (while (string-match "\\`# .*\n[ \t\n]*" txt)
      (setq txt (replace-match "" t t txt)))
    (when (string-match "\\s-+\\'" txt)
      (setq txt (replace-match "" t t txt)))
    (setq lines (and (not (equal "" txt)) (org-split-string txt "\n")))
    (when (org-string-nw-p note)
      (setq note
	    (org-replace-escapes
	     note
	     (list (cons "%u" (user-login-name))
		   (cons "%U" user-full-name)
		   (cons "%t" (format-time-string
			       (org-time-stamp-format 'long 'inactive)
			       org-log-note-effective-time))
		   (cons "%T" (format-time-string
			       (org-time-stamp-format 'long nil)
			       org-log-note-effective-time))
		   (cons "%d" (format-time-string
			       (org-time-stamp-format nil 'inactive)
			       org-log-note-effective-time))
		   (cons "%D" (format-time-string
			       (org-time-stamp-format nil nil)
			       org-log-note-effective-time))
		   (cons "%s" (cond
			       ((not org-log-note-state) "")
			       ((string-match-p org-ts-regexp
						org-log-note-state)
				(format "\"[%s]\""
					(substring org-log-note-state 1 -1)))
			       (t (format "\"%s\"" org-log-note-state))))
		   (cons "%S"
			 (cond
			  ((not org-log-note-previous-state) "")
			  ((string-match-p org-ts-regexp
					   org-log-note-previous-state)
			   (format "\"[%s]\""
				   (substring
				    org-log-note-previous-state 1 -1)))
			  (t (format "\"%s\""
				     org-log-note-previous-state)))))))
      (when lines (setq note (concat note " \\\\")))
      (push note lines))
    (when (and lines (not org-note-abort))
      (with-current-buffer (marker-buffer org-log-note-marker)
	(org-with-wide-buffer
	 ;; Find location for the new note.
	 (goto-char org-log-note-marker)
	 (set-marker org-log-note-marker nil)
	 ;; Note associated to a clock is to be located right after
	 ;; the clock.  Do not move point.
	 (unless (eq org-log-note-purpose 'clock-out)
	   (goto-char (org-log-beginning t)))
	 ;; Make sure point is at the beginning of an empty line.
	 (cond ((not (bolp)) (let ((inhibit-read-only t)) (insert "\n")))
	       ((looking-at "[ \t]*\\S-") (save-excursion (insert "\n"))))
	 ;; In an existing list, add a new item at the top level.
	 ;; Otherwise, indent line like a regular one.
	 (let ((itemp (org-in-item-p)))
	   (if itemp
	       (indent-line-to
		(let ((struct (save-excursion
				(goto-char itemp) (org-list-struct))))
		  (org-list-get-ind (org-list-get-top-point struct) struct)))
	     (org-indent-line)))
	 (insert (org-list-bullet-string "-") (pop lines))
	 (let ((ind (org-list-item-body-column (line-beginning-position))))
	   (dolist (line lines)
	     (insert "\n")
	     (indent-line-to ind)
	     (insert line)))
	 (message "Note stored")
	 (org-back-to-heading t)
	 (org-cycle-hide-drawers 'children))
	;; Fix `buffer-undo-list' when `org-store-log-note' is called
	;; from within `org-add-log-note' because `buffer-undo-list'
	;; is then modified outside of `org-with-remote-undo'.
	(when (eq this-command 'org-agenda-todo)
	  (setcdr buffer-undo-list (cddr buffer-undo-list))))))
  ;; Don't add undo information when called from `org-agenda-todo'.
  (let ((buffer-undo-list (eq this-command 'org-agenda-todo)))
    (set-window-configuration org-log-note-window-configuration)
    (with-current-buffer (marker-buffer org-log-note-return-to)
      (goto-char org-log-note-return-to))
    (move-marker org-log-note-return-to nil)
    (when org-log-post-message (message "%s" org-log-post-message))))

(defun org-remove-empty-drawer-at (pos)
  "Remove an empty drawer at position POS.
POS may also be a marker."
  (with-current-buffer (if (markerp pos) (marker-buffer pos) (current-buffer))
    (org-with-wide-buffer
     (goto-char pos)
     (let ((drawer (org-element-at-point)))
       (when (and (memq (org-element-type drawer) '(drawer property-drawer))
		  (not (org-element-property :contents-begin drawer)))
	 (delete-region (org-element-property :begin drawer)
			(progn (goto-char (org-element-property :end drawer))
			       (skip-chars-backward " \r\t\n")
			       (forward-line)
			       (point))))))))

(defvar org-ts-type nil)
(defun org-sparse-tree (&optional arg type)
  "Create a sparse tree, prompt for the details.
This command can create sparse trees.  You first need to select the type
of match used to create the tree:

t      Show all TODO entries.
T      Show entries with a specific TODO keyword.
m      Show entries selected by a tags/property match.
p      Enter a property name and its value (both with completion on existing
       names/values) and show entries with that property.
r      Show entries matching a regular expression (`/' can be used as well).
b      Show deadlines and scheduled items before a date.
a      Show deadlines and scheduled items after a date.
d      Show deadlines due within `org-deadline-warning-days'.
D      Show deadlines and scheduled items between a date range."
  (interactive "P")
  (setq type (or type org-sparse-tree-default-date-type))
  (setq org-ts-type type)
  (message "Sparse tree: [r]egexp [t]odo [T]odo-kwd [m]atch [p]roperty
             \[d]eadlines [b]efore-date [a]fter-date [D]ates range
             \[c]ycle through date types: %s"
	   (cl-case type
	     (all "all timestamps")
	     (scheduled "only scheduled")
	     (deadline "only deadline")
	     (active "only active timestamps")
	     (inactive "only inactive timestamps")
	     (closed "with a closed time-stamp")
	     (otherwise "scheduled/deadline")))
  (let ((answer (read-char-exclusive)))
    (cl-case answer
      (?c
       (org-sparse-tree
	arg
	(cadr
	 (memq type '(nil all scheduled deadline active inactive closed)))))
      (?d (call-interactively 'org-check-deadlines))
      (?b (call-interactively 'org-check-before-date))
      (?a (call-interactively 'org-check-after-date))
      (?D (call-interactively 'org-check-dates-range))
      (?t (call-interactively 'org-show-todo-tree))
      (?T (org-show-todo-tree '(4)))
      (?m (call-interactively 'org-match-sparse-tree))
      ((?p ?P)
       (let* ((kwd (completing-read
		    "Property: " (mapcar #'list (org-buffer-property-keys))))
	      (value (completing-read
		      "Value: " (mapcar #'list (org-property-values kwd)))))
	 (unless (string-match "\\`{.*}\\'" value)
	   (setq value (concat "\"" value "\"")))
	 (org-match-sparse-tree arg (concat kwd "=" value))))
      ((?r ?R ?/) (call-interactively 'org-occur))
      (otherwise (user-error "No such sparse tree command \"%c\"" answer)))))

(defvar-local org-occur-highlights nil
  "List of overlays used for occur matches.")
(defvar-local org-occur-parameters nil
  "Parameters of the active org-occur calls.
This is a list, each call to org-occur pushes as cons cell,
containing the regular expression and the callback, onto the list.
The list can contain several entries if `org-occur' has been called
several time with the KEEP-PREVIOUS argument.  Otherwise, this list
will only contain one set of parameters.  When the highlights are
removed (for example with `C-c C-c', or with the next edit (depending
on `org-remove-highlights-with-change'), this variable is emptied
as well.")

(defun org-occur (regexp &optional keep-previous callback)
  "Make a compact tree which shows all matches of REGEXP.

The tree will show the lines where the regexp matches, and any other context
defined in `org-show-context-detail', which see.

When optional argument KEEP-PREVIOUS is non-nil, highlighting and exposing
done by a previous call to `org-occur' will be kept, to allow stacking of
calls to this command.

Optional argument CALLBACK can be a function of no argument.  In this case,
it is called with point at the end of the match, match data being set
accordingly.  Current match is shown only if the return value is non-nil.
The function must neither move point nor alter narrowing."
  (interactive "sRegexp: \nP")
  (when (equal regexp "")
    (user-error "Regexp cannot be empty"))
  (unless keep-previous
    (org-remove-occur-highlights nil nil t))
  (push (cons regexp callback) org-occur-parameters)
  (let ((cnt 0))
    (save-excursion
      (goto-char (point-min))
      (when (or (not keep-previous)	    ; do not want to keep
		(not org-occur-highlights)) ; no previous matches
	;; hide everything
	(org-overview))
      (let ((case-fold-search (if (eq org-occur-case-fold-search 'smart)
				  (isearch-no-upper-case-p regexp t)
				org-occur-case-fold-search)))
	(while (re-search-forward regexp nil t)
	  (when (or (not callback)
		    (save-match-data (funcall callback)))
	    (setq cnt (1+ cnt))
	    (when org-highlight-sparse-tree-matches
	      (org-highlight-new-match (match-beginning 0) (match-end 0)))
	    (org-show-context 'occur-tree)))))
    (when org-remove-highlights-with-change
      (add-hook 'before-change-functions 'org-remove-occur-highlights
		nil 'local))
    (unless org-sparse-tree-open-archived-trees
      (org-hide-archived-subtrees (point-min) (point-max)))
    (run-hooks 'org-occur-hook)
    (when (called-interactively-p 'interactive)
      (message "%d match(es) for regexp %s" cnt regexp))
    cnt))

(defun org-occur-next-match (&optional n _reset)
  "Function for `next-error-function' to find sparse tree matches.
N is the number of matches to move, when negative move backwards.
This function always goes back to the starting point when no
match is found."
  (let* ((limit (if (< n 0) (point-min) (point-max)))
	 (search-func (if (< n 0)
			  'previous-single-char-property-change
			'next-single-char-property-change))
	 (n (abs n))
	 (pos (point))
	 p1)
    (catch 'exit
      (while (setq p1 (funcall search-func (point) 'org-type))
	(when (equal p1 limit)
	  (goto-char pos)
	  (user-error "No more matches"))
	(when (equal (get-char-property p1 'org-type) 'org-occur)
	  (setq n (1- n))
	  (when (= n 0)
	    (goto-char p1)
	    (throw 'exit (point))))
	(goto-char p1))
      (goto-char p1)
      (user-error "No more matches"))))

(defun org-show-context (&optional key)
  "Make sure point and context are visible.
Optional argument KEY, when non-nil, is a symbol.  See
`org-show-context-detail' for allowed values and how much is to
be shown."
  (org-show-set-visibility
   (cond ((symbolp org-show-context-detail) org-show-context-detail)
	 ((cdr (assq key org-show-context-detail)))
	 (t (cdr (assq 'default org-show-context-detail))))))

(defun org-show-set-visibility (detail)
  "Set visibility around point according to DETAIL.
DETAIL is either nil, `minimal', `local', `ancestors', `lineage',
`tree', `canonical' or t.  See `org-show-context-detail' for more
information."
  ;; Show current heading and possibly its entry, following headline
  ;; or all children.
  (if (and (org-at-heading-p) (not (eq detail 'local)))
      (org-flag-heading nil)
    (org-show-entry)
    ;; If point is hidden within a drawer or a block, make sure to
    ;; expose it.
    (dolist (o (overlays-at (point)))
      (when (memq (overlay-get o 'invisible) '(org-hide-block outline))
	(delete-overlay o)))
    (unless (org-before-first-heading-p)
      (org-with-limited-levels
       (cl-case detail
	 ((tree canonical t) (org-show-children))
	 ((nil minimal ancestors))
	 (t (save-excursion
	      (outline-next-heading)
	      (org-flag-heading nil)))))))
  ;; Show all siblings.
  (when (eq detail 'lineage) (org-show-siblings))
  ;; Show ancestors, possibly with their children.
  (when (memq detail '(ancestors lineage tree canonical t))
    (save-excursion
      (while (org-up-heading-safe)
	(org-flag-heading nil)
	(when (memq detail '(canonical t)) (org-show-entry))
	(when (memq detail '(tree canonical t)) (org-show-children))))))

(defvar org-reveal-start-hook nil
  "Hook run before revealing a location.")

(defun org-reveal (&optional siblings)
  "Show current entry, hierarchy above it, and the following headline.

This can be used to show a consistent set of context around
locations exposed with `org-show-context'.

With optional argument SIBLINGS, on each level of the hierarchy all
siblings are shown.  This repairs the tree structure to what it would
look like when opened with hierarchical calls to `org-cycle'.

With a \\[universal-argument] \\[universal-argument] prefix, \
go to the parent and show the entire tree."
  (interactive "P")
  (run-hooks 'org-reveal-start-hook)
  (cond ((equal siblings '(4)) (org-show-set-visibility 'canonical))
	((equal siblings '(16))
	 (save-excursion
	   (when (org-up-heading-safe)
	     (org-show-subtree)
	     (run-hook-with-args 'org-cycle-hook 'subtree))))
	(t (org-show-set-visibility 'lineage))))

(defun org-highlight-new-match (beg end)
  "Highlight from BEG to END and mark the highlight is an occur headline."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face 'secondary-selection)
    (overlay-put ov 'org-type 'org-occur)
    (push ov org-occur-highlights)))

(defun org-remove-occur-highlights (&optional _beg _end noremove)
  "Remove the occur highlights from the buffer.
BEG and END are ignored.  If NOREMOVE is nil, remove this function
from the `before-change-functions' in the current buffer."
  (interactive)
  (unless org-inhibit-highlight-removal
    (mapc #'delete-overlay org-occur-highlights)
    (setq org-occur-highlights nil)
    (setq org-occur-parameters nil)
    (unless noremove
      (remove-hook 'before-change-functions
		   'org-remove-occur-highlights 'local))))

;;;; Priorities

(defvar org-priority-regexp ".*?\\(\\[#\\([A-Z0-9]\\)\\] ?\\)"
  "Regular expression matching the priority indicator.")

(defvar org-remove-priority-next-time nil)

(defun org-priority-up ()
  "Increase the priority of the current item."
  (interactive)
  (org-priority 'up))

(defun org-priority-down ()
  "Decrease the priority of the current item."
  (interactive)
  (org-priority 'down))

(defun org-priority (&optional action _show)
  "Change the priority of an item.
ACTION can be `set', `up', `down', or a character."
  (interactive "P")
  (if (equal action '(4))
      (org-show-priority)
    (unless org-enable-priority-commands
      (user-error "Priority commands are disabled"))
    (setq action (or action 'set))
    (let (current new news have remove)
      (save-excursion
	(org-back-to-heading t)
	(when (looking-at org-priority-regexp)
	  (setq current (string-to-char (match-string 2))
		have t))
	(cond
	 ((eq action 'remove)
	  (setq remove t new ?\ ))
	 ((or (eq action 'set)
	      (integerp action))
	  (if (not (eq action 'set))
	      (setq new action)
	    (message "Priority %c-%c, SPC to remove: "
		     org-highest-priority org-lowest-priority)
	    (save-match-data
	      (setq new (read-char-exclusive))))
	  (when (and (= (upcase org-highest-priority) org-highest-priority)
		     (= (upcase org-lowest-priority) org-lowest-priority))
	    (setq new (upcase new)))
	  (cond ((equal new ?\ ) (setq remove t))
		((or (< (upcase new) org-highest-priority) (> (upcase new) org-lowest-priority))
		 (user-error "Priority must be between `%c' and `%c'"
			     org-highest-priority org-lowest-priority))))
	 ((eq action 'up)
	  (setq new (if have
			(1- current)  ; normal cycling
		      ;; last priority was empty
		      (if (eq last-command this-command)
			  org-lowest-priority  ; wrap around empty to lowest
			;; default
			(if org-priority-start-cycle-with-default
			    org-default-priority
			  (1- org-default-priority))))))
	 ((eq action 'down)
	  (setq new (if have
			(1+ current)  ; normal cycling
		      ;; last priority was empty
		      (if (eq last-command this-command)
			  org-highest-priority  ; wrap around empty to highest
			;; default
			(if org-priority-start-cycle-with-default
			    org-default-priority
			  (1+ org-default-priority))))))
	 (t (user-error "Invalid action")))
	(when (or (< (upcase new) org-highest-priority)
		  (> (upcase new) org-lowest-priority))
	  (if (and (memq action '(up down))
		   (not have) (not (eq last-command this-command)))
	      ;; `new' is from default priority
	      (error
	       "The default can not be set, see `org-default-priority' why")
	    ;; normal cycling: `new' is beyond highest/lowest priority
	    ;; and is wrapped around to the empty priority
	    (setq remove t)))
	(setq news (format "%c" new))
	(if have
	    (if remove
		(replace-match "" t t nil 1)
	      (replace-match news t t nil 2))
	  (if remove
	      (user-error "No priority cookie found in line")
	    (let ((case-fold-search nil)) (looking-at org-todo-line-regexp))
	    (if (match-end 2)
		(progn
		  (goto-char (match-end 2))
		  (insert " [#" news "]"))
	      (goto-char (match-beginning 3))
	      (insert "[#" news "] "))))
	(org-set-tags nil 'align))
      (if remove
	  (message "Priority removed")
	(message "Priority of current item set to %s" news)))))

(defun org-show-priority ()
  "Show the priority of the current item.
This priority is composed of the main priority given with the [#A] cookies,
and by additional input from the age of a schedules or deadline entry."
  (interactive)
  (let ((pri (if (eq major-mode 'org-agenda-mode)
		 (org-get-at-bol 'priority)
	       (save-excursion
		 (save-match-data
		   (beginning-of-line)
		   (and (looking-at org-heading-regexp)
			(org-get-priority (match-string 0))))))))
    (message "Priority is %d" (if pri pri -1000))))

(defun org-get-priority (s)
  "Find priority cookie and return priority."
  (save-match-data
    (if (functionp org-get-priority-function)
	(funcall org-get-priority-function)
      (if (not (string-match org-priority-regexp s))
	  (* 1000 (- org-lowest-priority org-default-priority))
	(* 1000 (- org-lowest-priority
		   (string-to-char (match-string 2 s))))))))

;;;; Tags

(defvar org-agenda-archives-mode)
(defvar org-map-continue-from nil
  "Position from where mapping should continue.
Can be set by the action argument to `org-scan-tags' and `org-map-entries'.")

(defvar org-scanner-tags nil
  "The current tag list while the tags scanner is running.")

(defvar org-trust-scanner-tags nil
  "Should `org-get-tags-at' use the tags for the scanner.
This is for internal dynamical scoping only.
When this is non-nil, the function `org-get-tags-at' will return the value
of `org-scanner-tags' instead of building the list by itself.  This
can lead to large speed-ups when the tags scanner is used in a file with
many entries, and when the list of tags is retrieved, for example to
obtain a list of properties.  Building the tags list for each entry in such
a file becomes an N^2 operation - but with this variable set, it scales
as N.")

(defvar org--matcher-tags-todo-only nil)

(defun org-scan-tags (action matcher todo-only &optional start-level)
  "Scan headline tags with inheritance and produce output ACTION.

ACTION can be `sparse-tree' to produce a sparse tree in the current buffer,
or `agenda' to produce an entry list for an agenda view.  It can also be
a Lisp form or a function that should be called at each matched headline, in
this case the return value is a list of all return values from these calls.

MATCHER is a function accepting three arguments, returning
a non-nil value whenever a given set of tags qualifies a headline
for inclusion.  See `org-make-tags-matcher' for more information.
As a special case, it can also be set to t (respectively nil) in
order to match all (respectively none) headline.

When TODO-ONLY is non-nil, only lines with a TODO keyword are
included in the output.

START-LEVEL can be a string with asterisks, reducing the scope to
headlines matching this string."
  (require 'org-agenda)
  (let* ((re (concat "^"
		     (if start-level
			 ;; Get the correct level to match
			 (concat "\\*\\{" (number-to-string start-level) "\\} ")
		       org-outline-regexp)
		     " *\\(\\<\\("
		     (mapconcat #'regexp-quote org-todo-keywords-1 "\\|")
		     "\\)\\>\\)? *\\(.*?\\)\\(:[[:alnum:]_@#%:]+:\\)?[ \t]*$"))
	 (props (list 'face 'default
		      'done-face 'org-agenda-done
		      'undone-face 'default
		      'mouse-face 'highlight
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name
			       (or (buffer-file-name (buffer-base-buffer))
				   (buffer-name (buffer-base-buffer)))))))
	 (org-map-continue-from nil)
         lspos tags tags-list
	 (tags-alist (list (cons 0 org-file-tags)))
	 (llast 0) rtn rtn1 level category i txt
	 todo marker entry priority
	 ts-date ts-date-type ts-date-pair)
    (unless (or (member action '(agenda sparse-tree)) (functionp action))
      (setq action (list 'lambda nil action)))
    (save-excursion
      (goto-char (point-min))
      (when (eq action 'sparse-tree)
	(org-overview)
	(org-remove-occur-highlights))
      (while (let (case-fold-search)
	       (re-search-forward re nil t))
	(setq org-map-continue-from nil)
	(catch :skip
	  (setq todo
		;; TODO: is the 1-2 difference a bug?
		(when (match-end 1) (match-string-no-properties 2))
		tags (when (match-end 4) (match-string-no-properties 4)))
	  (goto-char (setq lspos (match-beginning 0)))
	  (setq level (org-reduced-level (org-outline-level))
		category (org-get-category))
          (when (eq action 'agenda)
            (setq ts-date-pair (org-agenda-entry-get-agenda-timestamp (point))
		  ts-date (car ts-date-pair)
		  ts-date-type (cdr ts-date-pair)))
	  (setq i llast llast level)
	  ;; remove tag lists from same and sublevels
	  (while (>= i level)
	    (when (setq entry (assoc i tags-alist))
	      (setq tags-alist (delete entry tags-alist)))
	    (setq i (1- i)))
	  ;; add the next tags
	  (when tags
	    (setq tags (org-split-string tags ":")
		  tags-alist
		  (cons (cons level tags) tags-alist)))
	  ;; compile tags for current headline
	  (setq tags-list
		(if org-use-tag-inheritance
		    (apply 'append (mapcar 'cdr (reverse tags-alist)))
		  tags)
		org-scanner-tags tags-list)
	  (when org-use-tag-inheritance
	    (setcdr (car tags-alist)
		    (mapcar (lambda (x)
			      (setq x (copy-sequence x))
			      (org-add-prop-inherited x))
			    (cdar tags-alist))))
	  (when (and tags org-use-tag-inheritance
		     (or (not (eq t org-use-tag-inheritance))
			 org-tags-exclude-from-inheritance))
	    ;; Selective inheritance, remove uninherited ones.
	    (setcdr (car tags-alist)
		    (org-remove-uninherited-tags (cdar tags-alist))))
	  (when (and

		 ;; eval matcher only when the todo condition is OK
		 (and (or (not todo-only) (member todo org-todo-keywords-1))
		      (if (functionp matcher)
			  (let ((case-fold-search t) (org-trust-scanner-tags t))
			    (funcall matcher todo tags-list level))
			matcher))

		 ;; Call the skipper, but return t if it does not
		 ;; skip, so that the `and' form continues evaluating.
		 (progn
		   (unless (eq action 'sparse-tree) (org-agenda-skip))
		   t)

		 ;; Check if timestamps are deselecting this entry
		 (or (not todo-only)
		     (and (member todo org-todo-keywords-1)
			  (or (not org-agenda-tags-todo-honor-ignore-options)
			      (not (org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item))))))

	    ;; select this headline
	    (cond
	     ((eq action 'sparse-tree)
	      (and org-highlight-sparse-tree-matches
		   (org-get-heading) (match-end 0)
		   (org-highlight-new-match
		    (match-beginning 1) (match-end 1)))
	      (org-show-context 'tags-tree))
	     ((eq action 'agenda)
	      (setq txt (org-agenda-format-item
			 ""
			 (concat
			  (if (eq org-tags-match-list-sublevels 'indented)
			      (make-string (1- level) ?.) "")
			  (org-get-heading))
			 (make-string level ?\s)
			 category
			 tags-list)
		    priority (org-get-priority txt))
	      (goto-char lspos)
	      (setq marker (org-agenda-new-marker))
	      (org-add-props txt props
		'org-marker marker 'org-hd-marker marker 'org-category category
		'todo-state todo
                'ts-date ts-date
		'priority priority
                'type (concat "tagsmatch" ts-date-type))
	      (push txt rtn))
	     ((functionp action)
	      (setq org-map-continue-from nil)
	      (save-excursion
		(setq rtn1 (funcall action))
		(push rtn1 rtn)))
	     (t (user-error "Invalid action")))

	    ;; if we are to skip sublevels, jump to end of subtree
	    (unless org-tags-match-list-sublevels
	      (org-end-of-subtree t)
	      (backward-char 1))))
	;; Get the correct position from where to continue
	(if org-map-continue-from
	    (goto-char org-map-continue-from)
	  (and (= (point) lspos) (end-of-line 1)))))
    (when (and (eq action 'sparse-tree)
	       (not org-sparse-tree-open-archived-trees))
      (org-hide-archived-subtrees (point-min) (point-max)))
    (nreverse rtn)))

(defun org-remove-uninherited-tags (tags)
  "Remove all tags that are not inherited from the list TAGS."
  (cond
   ((eq org-use-tag-inheritance t)
    (if org-tags-exclude-from-inheritance
	(org-delete-all org-tags-exclude-from-inheritance tags)
      tags))
   ((not org-use-tag-inheritance) nil)
   ((stringp org-use-tag-inheritance)
    (delq nil (mapcar
	       (lambda (x)
		 (if (and (string-match org-use-tag-inheritance x)
			  (not (member x org-tags-exclude-from-inheritance)))
		     x nil))
	       tags)))
   ((listp org-use-tag-inheritance)
    (delq nil (mapcar
	       (lambda (x)
		 (if (member x org-use-tag-inheritance) x nil))
	       tags)))))

(defun org-match-sparse-tree (&optional todo-only match)
  "Create a sparse tree according to tags string MATCH.

MATCH is a string with match syntax.  It can contain a selection
of tags (\"+work+urgent-boss\"), properties (\"LEVEL>3\"), and
TODO keywords (\"TODO=\\\"WAITING\\\"\") or a combination of
those.  See the manual for details.

If optional argument TODO-ONLY is non-nil, only select lines that
are also TODO tasks."
  (interactive "P")
  (org-agenda-prepare-buffers (list (current-buffer)))
  (let ((org--matcher-tags-todo-only todo-only))
    (org-scan-tags 'sparse-tree (cdr (org-make-tags-matcher match))
		   org--matcher-tags-todo-only)))

(defalias 'org-tags-sparse-tree 'org-match-sparse-tree)

(defvar org-cached-props nil)
(defun org-cached-entry-get (pom property)
  (if (or (eq t org-use-property-inheritance)
	  (and (stringp org-use-property-inheritance)
	       (let ((case-fold-search t))
		 (string-match-p org-use-property-inheritance property)))
	  (and (listp org-use-property-inheritance)
	       (member-ignore-case property org-use-property-inheritance)))
      ;; Caching is not possible, check it directly.
      (org-entry-get pom property 'inherit)
    ;; Get all properties, so we can do complicated checks easily.
    (cdr (assoc-string property
		       (or org-cached-props
			   (setq org-cached-props (org-entry-properties pom)))
		       t))))

(defun org-global-tags-completion-table (&optional files)
  "Return the list of all tags in all agenda buffer/files.
Optional FILES argument is a list of files which can be used
instead of the agenda files."
  (save-excursion
    (org-uniquify
     (delq nil
	   (apply #'append
		  (mapcar
		   (lambda (file)
		     (set-buffer (find-file-noselect file))
		     (mapcar (lambda (x)
			       (and (stringp (car-safe x))
				    (list (car-safe x))))
			     (or org-current-tag-alist (org-get-buffer-tags))))
		   (if (car-safe files) files
		     (org-agenda-files))))))))

(defun org-make-tags-matcher (match)
  "Create the TAGS/TODO matcher form for the selection string MATCH.

Returns a cons of the selection string MATCH and a function
implementing the matcher.

The matcher is to be called at an Org entry, with point on the
headline, and returns non-nil if the entry matches the selection
string MATCH.  It must be called with three arguments: the TODO
keyword at the entry (or nil if none), the list of all tags at
the entry including inherited ones and the reduced level of the
headline.  Additionally, the category of the entry, if any, must
be specified as the text property `org-category' on the headline.

This function sets the variable `org--matcher-tags-todo-only' to
a non-nil value if the matcher restricts matching to TODO
entries, otherwise it is not touched.

See also `org-scan-tags'."
  (unless match
    ;; Get a new match request, with completion against the global
    ;; tags table and the local tags in current buffer.
    (let ((org-last-tags-completion-table
	   (org-uniquify
	    (delq nil (append (org-get-buffer-tags)
			      (org-global-tags-completion-table))))))
      (setq match
	    (completing-read
	     "Match: "
	     'org-tags-completion-function nil nil nil 'org-tags-history))))

  (let ((match0 match)
	(re "^&?\\([-+:]\\)?\\({[^}]+}\\|LEVEL\\([<=>]\\{1,2\\}\\)\\([0-9]+\\)\\|\\(\\(?:[[:alnum:]_]+\\(?:\\\\-\\)*\\)+\\)\\([<>=]\\{1,2\\}\\)\\({[^}]+}\\|\"[^\"]*\"\\|-?[.0-9]+\\(?:[eE][-+]?[0-9]+\\)?\\)\\|[[:alnum:]_@#%]+\\)")
	(start 0)
	tagsmatch todomatch tagsmatcher todomatcher)

    ;; Expand group tags.
    (setq match (org-tags-expand match))

    ;; Check if there is a TODO part of this match, which would be the
    ;; part after a "/".  To make sure that this slash is not part of
    ;; a property value to be matched against, we also check that
    ;; there is no / after that slash.  First, find the last slash.
    (let ((s 0))
      (while (string-match "/+" match s)
	(setq start (match-beginning 0))
	(setq s (match-end 0))))
    (if (and (string-match "/+" match start)
	     (not (string-match-p "\"" match start)))
	;; Match contains also a TODO-matching request.
	(progn
	  (setq tagsmatch (substring match 0 (match-beginning 0)))
	  (setq todomatch (substring match (match-end 0)))
	  (when (string-prefix-p "!" todomatch)
	    (setq org--matcher-tags-todo-only t)
	    (setq todomatch (substring todomatch 1)))
	  (when (string-match "\\`\\s-*\\'" todomatch)
	    (setq todomatch nil)))
      ;; Only matching tags.
      (setq tagsmatch match)
      (setq todomatch nil))

    ;; Make the tags matcher.
    (when (org-string-nw-p tagsmatch)
      (let ((orlist nil)
	    (orterms (org-split-string tagsmatch "|"))
	    term)
	(while (setq term (pop orterms))
	  (while (and (equal (substring term -1) "\\") orterms)
	    (setq term (concat term "|" (pop orterms)))) ;repair bad split.
	  (while (string-match re term)
	    (let* ((rest (substring term (match-end 0)))
		   (minus (and (match-end 1)
			       (equal (match-string 1 term) "-")))
		   (tag (save-match-data
			  (replace-regexp-in-string
			   "\\\\-" "-" (match-string 2 term))))
		   (regexp (eq (string-to-char tag) ?{))
		   (levelp (match-end 4))
		   (propp (match-end 5))
		   (mm
		    (cond
		     (regexp `(org-match-any-p ,(substring tag 1 -1) tags-list))
		     (levelp
		      `(,(org-op-to-function (match-string 3 term))
			level
			,(string-to-number (match-string 4 term))))
		     (propp
		      (let* ((gv (pcase (upcase (match-string 5 term))
				   ("CATEGORY"
				    '(get-text-property (point) 'org-category))
				   ("TODO" 'todo)
				   (p `(org-cached-entry-get nil ,p))))
			     (pv (match-string 7 term))
			     (regexp (eq (string-to-char pv) ?{))
			     (strp (eq (string-to-char pv) ?\"))
			     (timep (string-match-p "^\"[[<].*[]>]\"$" pv))
			     (po (org-op-to-function (match-string 6 term)
						     (if timep 'time strp))))
			(setq pv (if (or regexp strp) (substring pv 1 -1) pv))
			(when timep (setq pv (org-matcher-time pv)))
			(cond ((and regexp (eq po 'org<>))
			       `(not (string-match ,pv (or ,gv ""))))
			      (regexp `(string-match ,pv (or ,gv "")))
			      (strp `(,po (or ,gv "") ,pv))
			      (t
			       `(,po
				 (string-to-number (or ,gv ""))
				 ,(string-to-number pv))))))
		     (t `(member ,tag tags-list)))))
	      (push (if minus `(not ,mm) mm) tagsmatcher)
	      (setq term rest)))
	  (push `(and ,@tagsmatcher) orlist)
	  (setq tagsmatcher nil))
	(setq tagsmatcher `(progn (setq org-cached-props nil) (or ,@orlist)))))

    ;; Make the TODO matcher.
    (when (org-string-nw-p todomatch)
      (let ((orlist nil))
	(dolist (term (org-split-string todomatch "|"))
	  (while (string-match re term)
	    (let* ((minus (and (match-end 1)
			       (equal (match-string 1 term) "-")))
		   (kwd (match-string 2 term))
		   (regexp (eq (string-to-char kwd) ?{))
		   (mm (if regexp `(string-match ,(substring kwd 1 -1) todo)
			 `(equal todo ,kwd))))
	      (push (if minus `(not ,mm) mm) todomatcher))
	    (setq term (substring term (match-end 0))))
	  (push (if (> (length todomatcher) 1)
		    (cons 'and todomatcher)
		  (car todomatcher))
		orlist)
	  (setq todomatcher nil))
	(setq todomatcher (cons 'or orlist))))

    ;; Return the string and function of the matcher.  If no
    ;; tags-specific or todo-specific matcher exists, match
    ;; everything.
    (let ((matcher (if (and tagsmatcher todomatcher)
		       `(and ,tagsmatcher ,todomatcher)
		     (or tagsmatcher todomatcher t))))
      (when org--matcher-tags-todo-only
	(setq matcher `(and (member todo org-not-done-keywords) ,matcher)))
      (cons match0 `(lambda (todo tags-list level) ,matcher)))))

(defun org-tags-expand (match &optional single-as-list downcased tags-already-expanded)
  "Expand group tags in MATCH.

This replaces every group tag in MATCH with a regexp tag search.
For example, a group tag \"Work\" defined as { Work : Lab Conf }
will be replaced like this:

   Work =>  {\\<\\(?:Work\\|Lab\\|Conf\\)\\>}
  +Work => +{\\<\\(?:Work\\|Lab\\|Conf\\)\\>}
  -Work => -{\\<\\(?:Work\\|Lab\\|Conf\\)\\>}

Replacing by a regexp preserves the structure of the match.
E.g., this expansion

  Work|Home => {\\(?:Work\\|Lab\\|Conf\\}|Home

will match anything tagged with \"Lab\" and \"Home\", or tagged
with \"Conf\" and \"Home\" or tagged with \"Work\" and \"home\".

A group tag in MATCH can contain regular expressions of its own.
For example, a group tag \"Proj\" defined as { Proj : {P@.+} }
will be replaced like this:

   Proj => {\\<\\(?:Proj\\)\\>\\|P@.+}

When the optional argument SINGLE-AS-LIST is non-nil, MATCH is
assumed to be a single group tag, and the function will return
the list of tags in this group.

When DOWNCASE is non-nil, expand downcased TAGS."
  (if org-group-tags
      (let* ((case-fold-search t)
	     (stable org-mode-syntax-table)
	     (taggroups (or org-tag-groups-alist-for-agenda org-tag-groups-alist))
	     (taggroups (if downcased
			    (mapcar (lambda (tg) (mapcar #'downcase tg))
				    taggroups)
			  taggroups))
	     (taggroups-keys (mapcar #'car taggroups))
	     (return-match (if downcased (downcase match) match))
	     (count 0)
	     (work-already-expanded tags-already-expanded)
	     regexps-in-match tags-in-group regexp-in-group regexp-in-group-escaped)
	;; @ and _ are allowed as word-components in tags.
	(modify-syntax-entry ?@ "w" stable)
	(modify-syntax-entry ?_ "w" stable)
	;; Temporarily replace regexp-expressions in the match-expression.
	(while (string-match "{.+?}" return-match)
	  (cl-incf count)
	  (push (match-string 0 return-match) regexps-in-match)
	  (setq return-match (replace-match (format "<%d>" count) t nil return-match)))
	(while (and taggroups-keys
		    (with-syntax-table stable
		      (string-match
		       (concat "\\(?1:[+-]?\\)\\(?2:\\<"
			       (regexp-opt taggroups-keys) "\\>\\)")
		       return-match)))
	  (let* ((dir (match-string 1 return-match))
		 (tag (match-string 2 return-match))
		 (tag (if downcased (downcase tag) tag)))
	    (unless (or (get-text-property 0 'grouptag (match-string 2 return-match))
		        (member tag work-already-expanded))
	      (setq tags-in-group (assoc tag taggroups))
	      (push tag work-already-expanded)
	      ;; Recursively expand each tag in the group, if the tag hasn't
	      ;; already been expanded.  Restore the match-data after all recursive calls.
	      (save-match-data
		(let (tags-expanded)
		  (dolist (x (cdr tags-in-group))
		    (if (and (member x taggroups-keys)
			     (not (member x work-already-expanded)))
			(setq tags-expanded
			      (delete-dups
			       (append
				(org-tags-expand x t downcased
						 work-already-expanded)
				tags-expanded)))
		      (setq tags-expanded
			    (append (list x) tags-expanded)))
		    (setq work-already-expanded
			  (delete-dups
			   (append tags-expanded
				   work-already-expanded))))
		  (setq tags-in-group
			(delete-dups (cons (car tags-in-group)
					   tags-expanded)))))
	      ;; Filter tag-regexps from tags.
	      (setq regexp-in-group-escaped
		    (delq nil (mapcar (lambda (x)
					(if (stringp x)
					    (and (equal "{" (substring x 0 1))
						 (equal "}" (substring x -1))
						 x)
					  x))
				      tags-in-group))
		    regexp-in-group
		    (mapcar (lambda (x)
			      (substring x 1 -1))
			    regexp-in-group-escaped)
		    tags-in-group
		    (delq nil (mapcar (lambda (x)
					(if (stringp x)
					    (and (not (equal "{" (substring x 0 1)))
						 (not (equal "}" (substring x -1)))
						 x)
					  x))
				      tags-in-group)))
	      ;; If single-as-list, do no more in the while-loop.
	      (if (not single-as-list)
		  (progn
		    (when regexp-in-group
		      (setq regexp-in-group
			    (concat "\\|"
				    (mapconcat 'identity regexp-in-group
					       "\\|"))))
		    (setq tags-in-group
			  (concat dir
				  "{\\<"
				  (regexp-opt tags-in-group)
				  "\\>"
				  regexp-in-group
				  "}"))
		    (when (stringp tags-in-group)
		      (org-add-props tags-in-group '(grouptag t)))
		    (setq return-match
			  (replace-match tags-in-group t t return-match)))
		(setq tags-in-group
		      (append regexp-in-group-escaped tags-in-group))))
	    (setq taggroups-keys (delete tag taggroups-keys))))
	;; Add the regular expressions back into the match-expression again.
	(while regexps-in-match
	  (setq return-match (replace-regexp-in-string (format "<%d>" count)
						       (pop regexps-in-match)
						       return-match t t))
	  (cl-decf count))
	(if single-as-list
	    (if tags-in-group tags-in-group (list return-match))
	  return-match))
    (if single-as-list
	(list (if downcased (downcase match) match))
      match)))

(defun org-op-to-function (op &optional stringp)
  "Turn an operator into the appropriate function."
  (setq op
	(cond
	 ((equal  op   "<"       ) '(<     string<      org-time<))
	 ((equal  op   ">"       ) '(>     org-string>  org-time>))
	 ((member op '("<=" "=<")) '(<=    org-string<= org-time<=))
	 ((member op '(">=" "=>")) '(>=    org-string>= org-time>=))
	 ((member op '("="  "==")) '(=     string=      org-time=))
	 ((member op '("<>" "!=")) '(org<> org-string<> org-time<>))))
  (nth (if (eq stringp 'time) 2 (if stringp 1 0)) op))

(defun org<> (a b) (not (= a b)))
(defun org-string<= (a b) (or (string= a b) (string< a b)))
(defun org-string>= (a b) (not (string< a b)))
(defun org-string>  (a b) (and (not (string= a b)) (not (string< a b))))
(defun org-string<> (a b) (not (string= a b)))
(defun org-time=  (a b) (setq a (org-2ft a) b (org-2ft b)) (and (> a 0) (> b 0) (=     a b)))
(defun org-time<  (a b) (setq a (org-2ft a) b (org-2ft b)) (and (> a 0) (> b 0) (<     a b)))
(defun org-time<= (a b) (setq a (org-2ft a) b (org-2ft b)) (and (> a 0) (> b 0) (<=    a b)))
(defun org-time>  (a b) (setq a (org-2ft a) b (org-2ft b)) (and (> a 0) (> b 0) (>     a b)))
(defun org-time>= (a b) (setq a (org-2ft a) b (org-2ft b)) (and (> a 0) (> b 0) (>=    a b)))
(defun org-time<> (a b) (setq a (org-2ft a) b (org-2ft b)) (and (> a 0) (> b 0) (org<> a b)))
(defun org-2ft (s)
  "Convert S to a floating point time.
If S is already a number, just return it.  If it is a string, parse
it as a time string and apply `float-time' to it.  If S is nil, just return 0."
  (cond
   ((numberp s) s)
   ((stringp s)
    (condition-case nil
	(float-time (apply #'encode-time (org-parse-time-string s nil t)))
      (error 0.)))
   (t 0.)))

(defun org-time-today ()
  "Time in seconds today at 0:00.
Returns the float number of seconds since the beginning of the
epoch to the beginning of today (00:00)."
  (float-time (apply 'encode-time
		     (append '(0 0 0) (nthcdr 3 (decode-time))))))

(defun org-matcher-time (s)
  "Interpret a time comparison value."
  (save-match-data
    (cond
     ((string= s "<now>") (float-time))
     ((string= s "<today>") (org-time-today))
     ((string= s "<tomorrow>")   (+ 86400.0 (org-time-today)))
     ((string= s "<yesterday>")  (- (org-time-today) 86400.0))
     ((string-match "^<\\([-+][0-9]+\\)\\([hdwmy]\\)>$" s)
      (+ (org-time-today)
	 (* (string-to-number (match-string 1 s))
	    (cdr (assoc (match-string 2 s)
			'(("d" . 86400.0)   ("w" . 604800.0)
			  ("m" . 2678400.0) ("y" . 31557600.0)))))))
     (t (org-2ft s)))))

(defun org-match-any-p (re list)
  "Does re match any element of list?"
  (setq list (mapcar (lambda (x) (string-match re x)) list))
  (delq nil list))

(defvar org-add-colon-after-tag-completion nil)  ;; dynamically scoped param
(defvar org-tags-overlay (make-overlay 1 1))
(delete-overlay org-tags-overlay)

(defun org-get-local-tags-at (&optional pos)
  "Get a list of tags defined in the current headline."
  (org-get-tags-at pos 'local))

(defun org-get-local-tags ()
  "Get a list of tags defined in the current headline."
  (org-get-tags-at nil 'local))

(defun org-get-tags-at (&optional pos local)
  "Get a list of all headline tags applicable at POS.
POS defaults to point.  If tags are inherited, the list contains
the targets in the same sequence as the headlines appear, i.e.
the tags of the current headline come last.
When LOCAL is non-nil, only return tags from the current headline,
ignore inherited ones."
  (interactive)
  (if (and org-trust-scanner-tags
	   (or (not pos) (equal pos (point)))
	   (not local))
      org-scanner-tags
    (let (tags ltags lastpos parent)
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (or pos (point)))
	  (save-match-data
	    (catch 'done
	      (condition-case nil
		  (progn
		    (org-back-to-heading t)
		    (while (not (equal lastpos (point)))
		      (setq lastpos (point))
		      (when (looking-at ".+?:\\([[:alnum:]_@#%:]+\\):[ \t]*$")
			(setq ltags (org-split-string
				     (match-string-no-properties 1) ":"))
			(when parent
			  (setq ltags (mapcar 'org-add-prop-inherited ltags)))
			(setq tags (append
				    (if parent
					(org-remove-uninherited-tags ltags)
				      ltags)
				    tags)))
		      (or org-use-tag-inheritance (throw 'done t))
		      (when local (throw 'done t))
		      (or (org-up-heading-safe) (error nil))
		      (setq parent t)))
		(error nil)))))
	(if local
	    tags
	  (reverse (delete-dups
		    (reverse (append
			      (org-remove-uninherited-tags
			       org-file-tags)
                              tags)))))))))

(defun org-add-prop-inherited (s)
  (add-text-properties 0 (length s) '(inherited t) s)
  s)

(defun org-toggle-tag (tag &optional onoff)
  "Toggle the tag TAG for the current line.
If ONOFF is `on' or `off', don't toggle but set to this state."
  (save-excursion
    (org-back-to-heading t)
    (let ((current
	   (when (re-search-forward "[ \t]:\\([[:alnum:]_@#%:]+\\):[ \t]*$"
				    (line-end-position) t)
	     (let ((tags (match-string 1)))
	       ;; Clear current tags.
	       (replace-match "")
	       ;; Reverse the tags list so any new tag is appended to
	       ;; the current list of tags.
	       (nreverse (org-split-string tags ":")))))
	  res)
      (pcase onoff
	(`off (setq current (delete tag current)))
	((or `on (guard (not (member tag current))))
	 (setq res t)
	 (cl-pushnew tag current :test #'equal))
	(_ (setq current (delete tag current))))
      (end-of-line)
      (if current
	  (progn
	    (insert " :" (mapconcat #'identity (nreverse current) ":") ":")
	    (org-set-tags nil t))
	(delete-horizontal-space))
      (run-hooks 'org-after-tags-change-hook)
      res)))

(defun org--align-tags-here (to-col)
  "Align tags on the current headline to TO-COL.
Assume point is on a headline."
  (let ((pos (point)))
    (beginning-of-line)
    (if	(or (not (looking-at ".*?\\([ \t]+\\)\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$"))
	    (>= pos (match-beginning 2)))
	;; No tags or point within tags: do not align.
	(goto-char pos)
      (goto-char (match-beginning 1))
      (let ((shift (max (- (if (>= to-col 0) to-col
			     (- (abs to-col) (string-width (match-string 2))))
			   (current-column))
			1)))
	(replace-match (make-string shift ?\s) nil nil nil 1)
	;; Preserve initial position, if possible.  In any case, stop
	;; before tags.
	(when (< pos (point)) (goto-char pos))))))

(defun org-set-tags-command (&optional arg just-align)
  "Call the set-tags command for the current entry."
  (interactive "P")
  (if (or (org-at-heading-p) (and arg (org-before-first-heading-p)))
      (org-set-tags arg just-align)
    (save-excursion
      (unless (and (org-region-active-p)
		   org-loop-over-headlines-in-active-region)
	(org-back-to-heading t))
      (org-set-tags arg just-align))))

(defun org-set-tags-to (data)
  "Set the tags of the current entry to DATA, replacing the current tags.
DATA may be a tags string like :aa:bb:cc:, or a list of tags.
If DATA is nil or the empty string, any tags will be removed."
  (interactive "sTags: ")
  (setq data
	(cond
	 ((eq data nil) "")
	 ((equal data "") "")
	 ((stringp data)
	  (concat ":" (mapconcat 'identity (org-split-string data ":+") ":")
		  ":"))
	 ((listp data)
	  (concat ":" (mapconcat 'identity data ":") ":"))))
  (when data
    (save-excursion
      (org-back-to-heading t)
      (when (let ((case-fold-search nil))
	      (looking-at org-complex-heading-regexp))
	(if (match-end 5)
	    (progn
	      (goto-char (match-beginning 5))
	      (insert data)
	      (delete-region (point) (point-at-eol))
	      (org-set-tags nil 'align))
	  (goto-char (point-at-eol))
	  (insert " " data)
	  (org-set-tags nil 'align)))
      (beginning-of-line 1)
      (when (looking-at ".*?\\([ \t]+\\)$")
	(delete-region (match-beginning 1) (match-end 1))))))

(defun org-align-all-tags ()
  "Align the tags in all headings."
  (interactive)
  (save-excursion
    (or (ignore-errors (org-back-to-heading t))
	(outline-next-heading))
    (if (org-at-heading-p)
	(org-set-tags t)
      (message "No headings"))))

(defvar org-indent-indentation-per-level)
(defun org-set-tags (&optional arg just-align)
  "Set the tags for the current headline.
With prefix ARG, realign all tags in headings in the current buffer.
When JUST-ALIGN is non-nil, only align tags."
  (interactive "P")
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
                    'region-start-level
		  'region))
            org-loop-over-headlines-in-active-region)
        (org-map-entries
         ;; We don't use ARG and JUST-ALIGN here because these args
         ;; are not useful when looping over headlines.
         #'org-set-tags
         org-loop-over-headlines-in-active-region
         cl
	 '(when (org-invisible-p) (org-end-of-subtree nil t))))
    (let ((org-setting-tags t))
      (if arg
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward org-outline-regexp-bol nil t)
	      (org-set-tags nil t)
	      (end-of-line))
            (message "All tags realigned to column %d" org-tags-column))
	(let* ((current (org-get-tags-string))
	       (tags
		(if just-align current
		  ;; Get a new set of tags from the user.
		  (save-excursion
		    (let* ((seen)
			   (table
			    (setq
			     org-last-tags-completion-table
			     ;; Uniquify tags in alists, yet preserve
			     ;; structure (i.e., keywords).
			     (delq nil
				   (mapcar
				    (lambda (pair)
				      (let ((head (car pair)))
					(cond ((symbolp head) pair)
					      ((member head seen) nil)
					      (t (push head seen)
						 pair))))
				    (append
				     (or org-current-tag-alist
					 (org-get-buffer-tags))
				     (and
				      org-complete-tags-always-offer-all-agenda-tags
				      (org-global-tags-completion-table
				       (org-agenda-files))))))))
			   (current-tags (org-split-string current ":"))
			   (inherited-tags
			    (nreverse (nthcdr (length current-tags)
					      (nreverse (org-get-tags-at))))))
		      (replace-regexp-in-string
		       "\\([-+&]+\\|,\\)"
		       ":"
		       (if (or (eq t org-use-fast-tag-selection)
			       (and org-use-fast-tag-selection
				    (delq nil (mapcar #'cdr table))))
			   (org-fast-tag-selection
			    current-tags inherited-tags table
			    (and org-fast-tag-selection-include-todo
				 org-todo-key-alist))
			 (let ((org-add-colon-after-tag-completion
				(< 1 (length table))))
			   (org-trim
			    (completing-read
			     "Tags: "
			     #'org-tags-completion-function
			     nil nil current 'org-tags-history))))))))))

	  (when org-tags-sort-function
	    (setq tags
		  (mapconcat
		   #'identity
		   (sort (org-split-string tags "[^[:alnum:]_@#%]+")
			 org-tags-sort-function)
		   ":")))

	  (if (or (string= ":" tags)
		  (string= "::" tags))
	      (setq tags ""))
	  (if (not (org-string-nw-p tags)) (setq tags "")
	    (unless (string-suffix-p ":" tags) (setq tags (concat tags ":")))
	    (unless (string-prefix-p ":" tags) (setq tags (concat ":" tags))))

	  ;; Insert new tags at the correct column.
	  (unless (equal current tags)
	    (save-excursion
	      (beginning-of-line)
	      (let ((case-fold-search nil))
		(looking-at org-complex-heading-regexp))
	      ;; Remove current tags, if any.
	      (when (match-end 5) (replace-match "" nil nil nil 5))
	      ;; Insert new tags, if any.  Otherwise, remove trailing
	      ;; white spaces.
	      (end-of-line)
	      (if (not (equal tags ""))
		  ;; When text is being inserted on an invisible
		  ;; region boundary, it can be inadvertently sucked
		  ;; into invisibility.
		  (outline-flag-region (point) (progn (insert " " tags) (point)) nil)
		(skip-chars-backward " \t")
		(delete-region (point) (line-end-position)))))
	  ;; Align tags, if any.  Fix tags column if `org-indent-mode'
	  ;; is on.
	  (unless (equal tags "")
	    (let* ((level (save-excursion
			    (beginning-of-line)
			    (skip-chars-forward "\\*")))
		   (offset (if (bound-and-true-p org-indent-mode)
			       (* (1- org-indent-indentation-per-level)
				  (1- level))
			     0))
		   (tags-column
		    (+ org-tags-column
		       (if (> org-tags-column 0) (- offset) offset))))
	      (org--align-tags-here tags-column))))
        (unless just-align (run-hooks 'org-after-tags-change-hook))))))

(defun org-change-tag-in-region (beg end tag off)
  "Add or remove TAG for each entry in the region.
This works in the agenda, and also in an Org buffer."
  (interactive
   (list (region-beginning) (region-end)
	 (let ((org-last-tags-completion-table
		(if (derived-mode-p 'org-mode)
		    (org-uniquify
		     (delq nil (append (org-get-buffer-tags)
				       (org-global-tags-completion-table))))
		  (org-global-tags-completion-table))))
	   (completing-read
	    "Tag: " 'org-tags-completion-function nil nil nil
	    'org-tags-history))
	 (progn
	   (message "[s]et or [r]emove? ")
	   (equal (read-char-exclusive) ?r))))
  (when (fboundp 'deactivate-mark) (deactivate-mark))
  (let ((agendap (equal major-mode 'org-agenda-mode))
	l1 l2 m buf pos newhead (cnt 0))
    (goto-char end)
    (setq l2 (1- (org-current-line)))
    (goto-char beg)
    (setq l1 (org-current-line))
    (cl-loop for l from l1 to l2 do
	     (org-goto-line l)
	     (setq m (get-text-property (point) 'org-hd-marker))
	     (when (or (and (derived-mode-p 'org-mode) (org-at-heading-p))
		       (and agendap m))
	       (setq buf (if agendap (marker-buffer m) (current-buffer))
		     pos (if agendap m (point)))
	       (with-current-buffer buf
		 (save-excursion
		   (save-restriction
		     (goto-char pos)
		     (setq cnt (1+ cnt))
		     (org-toggle-tag tag (if off 'off 'on))
		     (setq newhead (org-get-heading)))))
	       (and agendap (org-agenda-change-all-lines newhead m))))
    (message "Tag :%s: %s in %d headings" tag (if off "removed" "set") cnt)))

(defun org-tags-completion-function (string _predicate &optional flag)
  (let (s1 s2 rtn (ctable org-last-tags-completion-table)
	   (confirm (lambda (x) (stringp (car x)))))
    (if (string-match "^\\(.*[-+:&,|]\\)\\([^-+:&,|]*\\)$" string)
        (setq s1 (match-string 1 string)
              s2 (match-string 2 string))
      (setq s1 "" s2 string))
    (cond
     ((eq flag nil)
      ;; try completion
      (setq rtn (try-completion s2 ctable confirm))
      (when (stringp rtn)
	(setq rtn
	      (concat s1 s2 (substring rtn (length s2))
		      (if (and org-add-colon-after-tag-completion
			       (assoc rtn ctable))
			  ":" ""))))
      rtn)
     ((eq flag t)
      ;; all-completions
      (all-completions s2 ctable confirm))
     ((eq flag 'lambda)
      ;; exact match?
      (assoc s2 ctable)))))

(defun org-fast-tag-insert (kwd tags face &optional end)
  "Insert KDW, and the TAGS, the latter with face FACE.
Also insert END."
  (insert (format "%-12s" (concat kwd ":"))
	  (org-add-props (mapconcat 'identity tags " ") nil 'face face)
	  (or end "")))

(defun org-fast-tag-show-exit (flag)
  (save-excursion
    (org-goto-line 3)
    (when (re-search-forward "[ \t]+Next change exits" (point-at-eol) t)
      (replace-match ""))
    (when flag
      (end-of-line 1)
      (org-move-to-column (- (window-width) 19) t)
      (insert (org-add-props " Next change exits" nil 'face 'org-warning)))))

(defun org-set-current-tags-overlay (current prefix)
  "Add an overlay to CURRENT tag with PREFIX."
  (let ((s (concat ":" (mapconcat 'identity current ":") ":")))
    (put-text-property 0 (length s) 'face '(secondary-selection org-tag) s)
    (org-overlay-display org-tags-overlay (concat prefix s))))

(defvar org-last-tag-selection-key nil)
(defun org-fast-tag-selection (current inherited table &optional todo-table)
  "Fast tag selection with single keys.
CURRENT is the current list of tags in the headline, INHERITED is the
list of inherited tags, and TABLE is an alist of tags and corresponding keys,
possibly with grouping information.  TODO-TABLE is a similar table with
TODO keywords, should these have keys assigned to them.
If the keys are nil, a-z are automatically assigned.
Returns the new tags string, or nil to not change the current settings."
  (let* ((fulltable (append table todo-table))
	 (maxlen (apply 'max (mapcar
			      (lambda (x)
				(if (stringp (car x)) (string-width (car x)) 0))
			      fulltable)))
	 (buf (current-buffer))
	 (expert (eq org-fast-tag-selection-single-key 'expert))
	 (buffer-tags nil)
	 (fwidth (+ maxlen 3 1 3))
	 (ncol (/ (- (window-width) 4) fwidth))
	 (i-face 'org-done)
	 (c-face 'org-todo)
	 tg cnt e c char c1 c2 ntable tbl rtn
	 ov-start ov-end ov-prefix
	 (exit-after-next org-fast-tag-selection-single-key)
	 (done-keywords org-done-keywords)
	 groups ingroup intaggroup)
    (save-excursion
      (beginning-of-line 1)
      (if (looking-at ".*[ \t]\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$")
	  (setq ov-start (match-beginning 1)
		ov-end (match-end 1)
		ov-prefix "")
	(setq ov-start (1- (point-at-eol))
	      ov-end (1+ ov-start))
	(skip-chars-forward "^\n\r")
	(setq ov-prefix
	      (concat
	       (buffer-substring (1- (point)) (point))
	       (if (> (current-column) org-tags-column)
		   " "
		 (make-string (- org-tags-column (current-column)) ?\ ))))))
    (move-overlay org-tags-overlay ov-start ov-end)
    (save-window-excursion
      (if expert
	  (set-buffer (get-buffer-create " *Org tags*"))
	(delete-other-windows)
	(set-window-buffer (split-window-vertically) (get-buffer-create " *Org tags*"))
	(org-switch-to-buffer-other-window " *Org tags*"))
      (erase-buffer)
      (setq-local org-done-keywords done-keywords)
      (org-fast-tag-insert "Inherited" inherited i-face "\n")
      (org-fast-tag-insert "Current" current c-face "\n\n")
      (org-fast-tag-show-exit exit-after-next)
      (org-set-current-tags-overlay current ov-prefix)
      (setq tbl fulltable char ?a cnt 0)
      (while (setq e (pop tbl))
	(cond
	 ((eq (car e) :startgroup)
	  (push '() groups) (setq ingroup t)
	  (unless (zerop cnt)
	    (setq cnt 0)
	    (insert "\n"))
	  (insert (if (cdr e) (format "%s: " (cdr e)) "") "{ "))
	 ((eq (car e) :endgroup)
	  (setq ingroup nil cnt 0)
	  (insert "}" (if (cdr e) (format " (%s) " (cdr e)) "") "\n"))
	 ((eq (car e) :startgrouptag)
	  (setq intaggroup t)
	  (unless (zerop cnt)
	    (setq cnt 0)
	    (insert "\n"))
	  (insert "[ "))
	 ((eq (car e) :endgrouptag)
	  (setq intaggroup nil cnt 0)
	  (insert "]\n"))
	 ((equal e '(:newline))
	  (unless (zerop cnt)
	    (setq cnt 0)
	    (insert "\n")
	    (setq e (car tbl))
	    (while (equal (car tbl) '(:newline))
	      (insert "\n")
	      (setq tbl (cdr tbl)))))
	 ((equal e '(:grouptags)) (insert " : "))
	 (t
	  (setq tg (copy-sequence (car e)) c2 nil)
	  (if (cdr e)
	      (setq c (cdr e))
	    ;; automatically assign a character.
	    (setq c1 (string-to-char
		      (downcase (substring
				 tg (if (= (string-to-char tg) ?@) 1 0)))))
	    (if (or (rassoc c1 ntable) (rassoc c1 table))
		(while (or (rassoc char ntable) (rassoc char table))
		  (setq char (1+ char)))
	      (setq c2 c1))
	    (setq c (or c2 char)))
	  (when ingroup (push tg (car groups)))
	  (setq tg (org-add-props tg nil 'face
	  			  (cond
	  			   ((not (assoc tg table))
	  			    (org-get-todo-face tg))
	  			   ((member tg current) c-face)
	  			   ((member tg inherited) i-face))))
	  (when (equal (caar tbl) :grouptags)
	    (org-add-props tg nil 'face 'org-tag-group))
	  (when (and (zerop cnt) (not ingroup) (not intaggroup)) (insert " "))
	  (insert "[" c "] " tg (make-string
				 (- fwidth 4 (length tg)) ?\ ))
	  (push (cons tg c) ntable)
	  (when (= (cl-incf cnt) ncol)
	    (insert "\n")
	    (when (or ingroup intaggroup) (insert " "))
	    (setq cnt 0)))))
      (setq ntable (nreverse ntable))
      (insert "\n")
      (goto-char (point-min))
      (unless expert (org-fit-window-to-buffer))
      (setq rtn
	    (catch 'exit
	      (while t
		(message "[a-z..]:toggle [SPC]:clear [RET]:accept [TAB]:edit [!] %sgroups%s"
			 (if (not groups) "no " "")
			 (if expert " [C-c]:window" (if exit-after-next " [C-c]:single" " [C-c]:multi")))
		(setq c (let ((inhibit-quit t)) (read-char-exclusive)))
		(setq org-last-tag-selection-key c)
		(cond
		 ((= c ?\r) (throw 'exit t))
		 ((= c ?!)
		  (setq groups (not groups))
		  (goto-char (point-min))
		  (while (re-search-forward "[{}]" nil t) (replace-match " ")))
		 ((= c ?\C-c)
		  (if (not expert)
		      (org-fast-tag-show-exit
		       (setq exit-after-next (not exit-after-next)))
		    (setq expert nil)
		    (delete-other-windows)
		    (set-window-buffer (split-window-vertically) " *Org tags*")
		    (org-switch-to-buffer-other-window " *Org tags*")
		    (org-fit-window-to-buffer)))
		 ((or (= c ?\C-g)
		      (and (= c ?q) (not (rassoc c ntable))))
		  (delete-overlay org-tags-overlay)
		  (setq quit-flag t))
		 ((= c ?\ )
		  (setq current nil)
		  (when exit-after-next (setq exit-after-next 'now)))
		 ((= c ?\t)
		  (condition-case nil
		      (setq tg (completing-read
				"Tag: "
				(or buffer-tags
				    (with-current-buffer buf
				      (setq buffer-tags
					    (org-get-buffer-tags))))))
		    (quit (setq tg "")))
		  (when (string-match "\\S-" tg)
		    (cl-pushnew (list tg) buffer-tags :test #'equal)
		    (if (member tg current)
			(setq current (delete tg current))
		      (push tg current)))
		  (when exit-after-next (setq exit-after-next 'now)))
		 ((setq e (rassoc c todo-table) tg (car e))
		  (with-current-buffer buf
		    (save-excursion (org-todo tg)))
		  (when exit-after-next (setq exit-after-next 'now)))
		 ((setq e (rassoc c ntable) tg (car e))
		  (if (member tg current)
		      (setq current (delete tg current))
		    (cl-loop for g in groups do
			     (when (member tg g)
			       (dolist (x g) (setq current (delete x current)))))
		    (push tg current))
		  (when exit-after-next (setq exit-after-next 'now))))

		;; Create a sorted list
		(setq current
		      (sort current
			    (lambda (a b)
			      (assoc b (cdr (memq (assoc a ntable) ntable))))))
		(when (eq exit-after-next 'now) (throw 'exit t))
		(goto-char (point-min))
		(beginning-of-line 2)
		(delete-region (point) (point-at-eol))
		(org-fast-tag-insert "Current" current c-face)
		(org-set-current-tags-overlay current ov-prefix)
		(while (re-search-forward "\\[.\\] \\([[:alnum:]_@#%]+\\)" nil t)
		  (setq tg (match-string 1))
		  (add-text-properties
		   (match-beginning 1) (match-end 1)
		   (list 'face
			 (cond
			  ((member tg current) c-face)
			  ((member tg inherited) i-face)
			  (t (get-text-property (match-beginning 1) 'face))))))
		(goto-char (point-min)))))
      (delete-overlay org-tags-overlay)
      (if rtn
	  (mapconcat 'identity current ":")
	nil))))

(defun org-get-tags-string ()
  "Get the TAGS string in the current headline."
  (unless (org-at-heading-p t)
    (user-error "Not on a heading"))
  (save-excursion
    (beginning-of-line 1)
    (if (looking-at ".*[ \t]\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$")
	(match-string-no-properties 1)
      "")))

(defun org-get-tags ()
  "Get the list of tags specified in the current headline."
  (org-split-string (org-get-tags-string) ":"))

(defun org-get-buffer-tags ()
  "Get a table of all tags used in the buffer, for completion."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((tag-re (concat org-outline-regexp-bol
			 "\\(?:.*?[ \t]\\)?:\\([[:alnum:]_@#%:]+\\):[ \t]*$"))
	 tags)
     (while (re-search-forward tag-re nil t)
       (dolist (tag (org-split-string (match-string-no-properties 1) ":"))
	 (push tag tags)))
     (mapcar #'list (append org-file-tags (org-uniquify tags))))))

;;;; The mapping API

(defvar org-agenda-skip-comment-trees)
(defvar org-agenda-skip-function)
(defun org-map-entries (func &optional match scope &rest skip)
  "Call FUNC at each headline selected by MATCH in SCOPE.

FUNC is a function or a lisp form.  The function will be called without
arguments, with the cursor positioned at the beginning of the headline.
The return values of all calls to the function will be collected and
returned as a list.

The call to FUNC will be wrapped into a save-excursion form, so FUNC
does not need to preserve point.  After evaluation, the cursor will be
moved to the end of the line (presumably of the headline of the
processed entry) and search continues from there.  Under some
circumstances, this may not produce the wanted results.  For example,
if you have removed (e.g. archived) the current (sub)tree it could
mean that the next entry will be skipped entirely.  In such cases, you
can specify the position from where search should continue by making
FUNC set the variable `org-map-continue-from' to the desired buffer
position.

MATCH is a tags/property/todo match as it is used in the agenda tags view.
Only headlines that are matched by this query will be considered during
the iteration.  When MATCH is nil or t, all headlines will be
visited by the iteration.

SCOPE determines the scope of this command.  It can be any of:

nil     The current buffer, respecting the restriction if any
tree    The subtree started with the entry at point
region  The entries within the active region, if any
region-start-level
        The entries within the active region, but only those at
        the same level than the first one.
file    The current buffer, without restriction
file-with-archives
        The current buffer, and any archives associated with it
agenda  All agenda files
agenda-with-archives
        All agenda files with any archive files associated with them
\(file1 file2 ...)
        If this is a list, all files in the list will be scanned

The remaining args are treated as settings for the skipping facilities of
the scanner.  The following items can be given here:

  archive    skip trees with the archive tag
  comment    skip trees with the COMMENT keyword
  function or Emacs Lisp form:
             will be used as value for `org-agenda-skip-function', so
             whenever the function returns a position, FUNC will not be
             called for that entry and search will continue from the
             position returned

If your function needs to retrieve the tags including inherited tags
at the *current* entry, you can use the value of the variable
`org-scanner-tags' which will be much faster than getting the value
with `org-get-tags-at'.  If your function gets properties with
`org-entry-properties' at the *current* entry, bind `org-trust-scanner-tags'
to t around the call to `org-entry-properties' to get the same speedup.
Note that if your function moves around to retrieve tags and properties at
a *different* entry, you cannot use these techniques."
  (unless (and (or (eq scope 'region) (eq scope 'region-start-level))
	       (not (org-region-active-p)))
    (let* ((org-agenda-archives-mode nil) ; just to make sure
	   (org-agenda-skip-archived-trees (memq 'archive skip))
	   (org-agenda-skip-comment-trees (memq 'comment skip))
	   (org-agenda-skip-function
	    (car (org-delete-all '(comment archive) skip)))
	   (org-tags-match-list-sublevels t)
	   (start-level (eq scope 'region-start-level))
	   matcher res
	   org-todo-keywords-for-agenda
	   org-done-keywords-for-agenda
	   org-todo-keyword-alist-for-agenda
	   org-tag-alist-for-agenda
	   org--matcher-tags-todo-only)

      (cond
       ((eq match t)   (setq matcher t))
       ((eq match nil) (setq matcher t))
       (t (setq matcher (if match (cdr (org-make-tags-matcher match)) t))))

      (save-excursion
	(save-restriction
	  (cond ((eq scope 'tree)
		 (org-back-to-heading t)
		 (org-narrow-to-subtree)
		 (setq scope nil))
		((and (or (eq scope 'region) (eq scope 'region-start-level))
		      (org-region-active-p))
		 ;; If needed, set start-level to a string like "2"
		 (when start-level
		   (save-excursion
		     (goto-char (region-beginning))
		     (unless (org-at-heading-p) (outline-next-heading))
		     (setq start-level (org-current-level))))
		 (narrow-to-region (region-beginning)
				   (save-excursion
				     (goto-char (region-end))
				     (unless (and (bolp) (org-at-heading-p))
				       (outline-next-heading))
				     (point)))
		 (setq scope nil)))

	  (if (not scope)
	      (progn
		(org-agenda-prepare-buffers
		 (and buffer-file-name (list buffer-file-name)))
		(setq res
		      (org-scan-tags
		       func matcher org--matcher-tags-todo-only start-level)))
	    ;; Get the right scope
	    (cond
	     ((and scope (listp scope) (symbolp (car scope)))
	      (setq scope (eval scope)))
	     ((eq scope 'agenda)
	      (setq scope (org-agenda-files t)))
	     ((eq scope 'agenda-with-archives)
	      (setq scope (org-agenda-files t))
	      (setq scope (org-add-archive-files scope)))
	     ((eq scope 'file)
	      (setq scope (and buffer-file-name (list buffer-file-name))))
	     ((eq scope 'file-with-archives)
	      (setq scope (org-add-archive-files (list (buffer-file-name))))))
	    (org-agenda-prepare-buffers scope)
	    (dolist (file scope)
	      (with-current-buffer (org-find-base-buffer-visiting file)
		(org-with-wide-buffer
		 (goto-char (point-min))
		 (setq res
		       (append
			res
			(org-scan-tags
			 func matcher org--matcher-tags-todo-only)))))))))
      res)))

;;; Properties API

(defconst org-special-properties
  '("ALLTAGS" "BLOCKED" "CLOCKSUM" "CLOCKSUM_T" "CLOSED" "DEADLINE" "FILE"
    "ITEM" "PRIORITY" "SCHEDULED" "TAGS" "TIMESTAMP" "TIMESTAMP_IA" "TODO")
  "The special properties valid in Org mode.
These are properties that are not defined in the property drawer,
but in some other way.")

(defconst org-default-properties
  '("ARCHIVE" "CATEGORY" "SUMMARY" "DESCRIPTION" "CUSTOM_ID"
    "LOCATION" "LOGGING" "COLUMNS" "VISIBILITY"
    "TABLE_EXPORT_FORMAT" "TABLE_EXPORT_FILE"
    "EXPORT_OPTIONS" "EXPORT_TEXT" "EXPORT_FILE_NAME"
    "EXPORT_TITLE" "EXPORT_AUTHOR" "EXPORT_DATE" "UNNUMBERED"
    "ORDERED" "NOBLOCKING" "COOKIE_DATA" "LOG_INTO_DRAWER" "REPEAT_TO_STATE"
    "CLOCK_MODELINE_TOTAL" "STYLE" "HTML_CONTAINER_CLASS")
  "Some properties that are used by Org mode for various purposes.
Being in this list makes sure that they are offered for completion.")

(defun org--valid-property-p (property)
  "Non nil when string PROPERTY is a valid property name."
  (not
   (or (equal property "")
       (string-match-p "\\s-" property))))

(defun org--update-property-plist (key val props)
  "Associate KEY to VAL in alist PROPS.
Modifications are made by side-effect.  Return new alist."
  (let* ((appending (string= (substring key -1) "+"))
	 (key (if appending (substring key 0 -1) key))
	 (old (assoc-string key props t)))
    (if (not old) (cons (cons key val) props)
      (setcdr old (if appending (concat (cdr old) " " val) val))
      props)))

(defun org-get-property-block (&optional beg force)
  "Return the (beg . end) range of the body of the property drawer.
BEG is the beginning of the current subtree, or of the part
before the first headline.  If it is not given, it will be found.
If the drawer does not exist, create it if FORCE is non-nil, or
return nil."
  (org-with-wide-buffer
   (when beg (goto-char beg))
   (unless (org-before-first-heading-p)
     (let ((beg (cond (beg)
		      ((or (not (featurep 'org-inlinetask))
			   (org-inlinetask-in-task-p))
		       (org-back-to-heading t))
		      (t (org-with-limited-levels (org-back-to-heading t))))))
       (forward-line)
       (when (looking-at-p org-planning-line-re) (forward-line))
       (cond ((looking-at org-property-drawer-re)
	      (forward-line)
	      (cons (point) (progn (goto-char (match-end 0))
				   (line-beginning-position))))
	     (force
	      (goto-char beg)
	      (org-insert-property-drawer)
	      (let ((pos (save-excursion (search-forward ":END:")
					 (line-beginning-position))))
		(cons pos pos))))))))

(defun org-at-property-p ()
  "Non-nil when point is inside a property drawer.
See `org-property-re' for match data, if applicable."
  (save-excursion
    (beginning-of-line)
    (and (looking-at org-property-re)
	 (let ((property-drawer (save-match-data (org-get-property-block))))
	   (and property-drawer
		(>= (point) (car property-drawer))
		(< (point) (cdr property-drawer)))))))

(defun org-property-action ()
  "Do an action on properties."
  (interactive)
  (message "Property Action:  [s]et  [d]elete  [D]elete globally  [c]ompute")
  (let ((c (read-char-exclusive)))
    (cl-case c
      (?s (call-interactively #'org-set-property))
      (?d (call-interactively #'org-delete-property))
      (?D (call-interactively #'org-delete-property-globally))
      (?c (call-interactively #'org-compute-property-at-point))
      (otherwise (user-error "No such property action %c" c)))))

(defun org-inc-effort ()
  "Increment the value of the effort property in the current entry."
  (interactive)
  (org-set-effort nil t))

(defvar org-clock-effort)       ; Defined in org-clock.el.
(defvar org-clock-current-task) ; Defined in org-clock.el.
(defun org-set-effort (&optional value increment)
  "Set the effort property of the current entry.
With numerical prefix arg, use the nth allowed value, 0 stands for the
10th allowed value.

When INCREMENT is non-nil, set the property to the next allowed value."
  (interactive "P")
  (when (equal value 0) (setq value 10))
  (let* ((completion-ignore-case t)
	 (prop org-effort-property)
	 (cur (org-entry-get nil prop))
	 (allowed (org-property-get-allowed-values nil prop 'table))
	 (existing (mapcar 'list (org-property-values prop)))
	 (heading (nth 4 (org-heading-components)))
	 rpl
	 (val (cond
	       ((stringp value) value)
	       ((and allowed (integerp value))
		(or (car (nth (1- value) allowed))
		    (car (org-last allowed))))
	       ((and allowed increment)
		(or (cl-caadr (member (list cur) allowed))
		    (user-error "Allowed effort values are not set")))
	       (allowed
		(message "Select 1-9,0, [RET%s]: %s"
			 (if cur (concat "=" cur) "")
			 (mapconcat 'car allowed " "))
		(setq rpl (read-char-exclusive))
		(if (equal rpl ?\r)
		    cur
		  (setq rpl (- rpl ?0))
		  (when (equal rpl 0) (setq rpl 10))
		  (if (and (> rpl 0) (<= rpl (length allowed)))
		      (car (nth (1- rpl) allowed))
		    (org-completing-read "Effort: " allowed nil))))
	       (t
		(org-completing-read
		 (concat "Effort" (and cur (string-match "\\S-" cur)
				       (concat " [" cur "]"))
			 ": ")
		 existing nil nil "" nil cur)))))
    (unless (equal (org-entry-get nil prop) val)
      (org-entry-put nil prop val))
    (org-refresh-property
     '((effort . identity)
       (effort-minutes . org-duration-to-minutes))
     val)
    (when (equal heading (bound-and-true-p org-clock-current-task))
      (setq org-clock-effort (get-text-property (point-at-bol) 'effort))
      (org-clock-update-mode-line))
    (message "%s is now %s" prop val)))

(defun org-entry-properties (&optional pom which)
  "Get all properties of the current entry.

When POM is a buffer position, get all properties from the entry
there instead.

This includes the TODO keyword, the tags, time strings for
deadline, scheduled, and clocking, and any additional properties
defined in the entry.

If WHICH is nil or `all', get all properties.  If WHICH is
`special' or `standard', only get that subclass.  If WHICH is
a string, only get that property.

Return value is an alist.  Keys are properties, as upcased
strings."
  (org-with-point-at pom
    (when (and (derived-mode-p 'org-mode)
	       (ignore-errors (org-back-to-heading t)))
      (catch 'exit
	(let* ((beg (point))
	       (specific (and (stringp which) (upcase which)))
	       (which (cond ((not specific) which)
			    ((member specific org-special-properties) 'special)
			    (t 'standard)))
	       props)
	  ;; Get the special properties, like TODO and TAGS.
	  (when (memq which '(nil all special))
	    (when (or (not specific) (string= specific "CLOCKSUM"))
	      (let ((clocksum (get-text-property (point) :org-clock-minutes)))
		(when clocksum
		  (push (cons "CLOCKSUM" (org-duration-from-minutes clocksum))
			props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "CLOCKSUM_T"))
	      (let ((clocksumt (get-text-property (point)
						  :org-clock-minutes-today)))
		(when clocksumt
		  (push (cons "CLOCKSUM_T"
			      (org-duration-from-minutes clocksumt))
			props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "ITEM"))
	      (let ((case-fold-search nil))
		(when (looking-at org-complex-heading-regexp)
		  (push (cons "ITEM"
			      (let ((title (match-string-no-properties 4)))
				(if (org-string-nw-p title)
				    (org-remove-tabs title)
				  "")))
			props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "TODO"))
	      (let ((case-fold-search nil))
		(when (and (looking-at org-todo-line-regexp) (match-end 2))
		  (push (cons "TODO" (match-string-no-properties 2)) props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "PRIORITY"))
	      (push (cons "PRIORITY"
			  (if (looking-at org-priority-regexp)
			      (match-string-no-properties 2)
			    (char-to-string org-default-priority)))
		    props)
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "FILE"))
	      (push (cons "FILE" (buffer-file-name (buffer-base-buffer)))
		    props)
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "TAGS"))
	      (let ((value (org-string-nw-p (org-get-tags-string))))
		(when value (push (cons "TAGS" value) props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "ALLTAGS"))
	      (let ((value (org-get-tags-at)))
		(when value
		  (push (cons "ALLTAGS"
			      (format ":%s:" (mapconcat #'identity value ":")))
			props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "BLOCKED"))
	      (push (cons "BLOCKED" (if (org-entry-blocked-p) "t" "")) props)
	      (when specific (throw 'exit props)))
	    (when (or (not specific)
		      (member specific '("CLOSED" "DEADLINE" "SCHEDULED")))
	      (forward-line)
	      (when (looking-at-p org-planning-line-re)
		(end-of-line)
		(let ((bol (line-beginning-position))
		      ;; Backward compatibility: time keywords used to
		      ;; be configurable (before 8.3).  Make sure we
		      ;; get the correct keyword.
		      (key-assoc `(("CLOSED" . ,org-closed-string)
				   ("DEADLINE" . ,org-deadline-string)
				   ("SCHEDULED" . ,org-scheduled-string))))
		  (dolist (pair (if specific (list (assoc specific key-assoc))
				  key-assoc))
		    (save-excursion
		      (when (search-backward (cdr pair) bol t)
			(goto-char (match-end 0))
			(skip-chars-forward " \t")
			(and (looking-at org-ts-regexp-both)
			     (push (cons (car pair)
					 (match-string-no-properties 0))
				   props)))))))
	      (when specific (throw 'exit props)))
	    (when (or (not specific)
		      (member specific '("TIMESTAMP" "TIMESTAMP_IA")))
	      (let ((find-ts
		     (lambda (end ts)
		       ;; Fix next time-stamp before END.  TS is the
		       ;; list of time-stamps found so far.
		       (let ((ts ts)
			     (regexp (cond
				      ((string= specific "TIMESTAMP")
				       org-ts-regexp)
				      ((string= specific "TIMESTAMP_IA")
				       org-ts-regexp-inactive)
				      ((assoc "TIMESTAMP_IA" ts)
				       org-ts-regexp)
				      ((assoc "TIMESTAMP" ts)
				       org-ts-regexp-inactive)
				      (t org-ts-regexp-both))))
			 (catch 'next
			   (while (re-search-forward regexp end t)
			     (backward-char)
			     (let ((object (org-element-context)))
			       ;; Accept to match timestamps in node
			       ;; properties, too.
			       (when (memq (org-element-type object)
					   '(node-property timestamp))
				 (let ((type
					(org-element-property :type object)))
				   (cond
				    ((and (memq type '(active active-range))
					  (not (equal specific "TIMESTAMP_IA")))
				     (unless (assoc "TIMESTAMP" ts)
				       (push (cons "TIMESTAMP"
						   (org-element-property
						    :raw-value object))
					     ts)
				       (when specific (throw 'exit ts))))
				    ((and (memq type '(inactive inactive-range))
					  (not (string= specific "TIMESTAMP")))
				     (unless (assoc "TIMESTAMP_IA" ts)
				       (push (cons "TIMESTAMP_IA"
						   (org-element-property
						    :raw-value object))
					     ts)
				       (when specific (throw 'exit ts))))))
				 ;; Both timestamp types are found,
				 ;; move to next part.
				 (when (= (length ts) 2) (throw 'next ts)))))
			   ts)))))
		(goto-char beg)
		;; First look for timestamps within headline.
		(let ((ts (funcall find-ts (line-end-position) nil)))
		  (if (= (length ts) 2) (setq props (nconc ts props))
		    ;; Then find timestamps in the section, skipping
		    ;; planning line.
		    (let ((end (save-excursion (outline-next-heading))))
		      (forward-line)
		      (when (looking-at-p org-planning-line-re) (forward-line))
		      (setq props (nconc (funcall find-ts end ts) props))))))))
	  ;; Get the standard properties, like :PROP:.
	  (when (memq which '(nil all standard))
	    ;; If we are looking after a specific property, delegate
	    ;; to `org-entry-get', which is faster.  However, make an
	    ;; exception for "CATEGORY", since it can be also set
	    ;; through keywords (i.e. #+CATEGORY).
	    (if (and specific (not (equal specific "CATEGORY")))
		(let ((value (org-entry-get beg specific nil t)))
		  (throw 'exit (and value (list (cons specific value)))))
	      (let ((range (org-get-property-block beg)))
		(when range
		  (let ((end (cdr range)) seen-base)
		    (goto-char (car range))
		    ;; Unlike to `org--update-property-plist', we
		    ;; handle the case where base values is found
		    ;; after its extension.  We also forbid standard
		    ;; properties to be named as special properties.
		    (while (re-search-forward org-property-re end t)
		      (let* ((key (upcase (match-string-no-properties 2)))
			     (extendp (string-match-p "\\+\\'" key))
			     (key-base (if extendp (substring key 0 -1) key))
			     (value (match-string-no-properties 3)))
			(cond
			 ((member-ignore-case key-base org-special-properties))
			 (extendp
			  (setq props
				(org--update-property-plist key value props)))
			 ((member key seen-base))
			 (t (push key seen-base)
			    (let ((p (assoc-string key props t)))
			      (if p (setcdr p (concat value " " (cdr p)))
				(push (cons key value) props))))))))))))
	  (unless (assoc "CATEGORY" props)
	    (push (cons "CATEGORY" (org-get-category beg)) props)
	    (when (string= specific "CATEGORY") (throw 'exit props)))
	  ;; Return value.
	  props)))))

(defun org--property-local-values (property literal-nil)
  "Return value for PROPERTY in current entry.
Value is a list whose car is the base value for PROPERTY and cdr
a list of accumulated values.  Return nil if neither is found in
the entry.  Also return nil when PROPERTY is set to \"nil\",
unless LITERAL-NIL is non-nil."
  (let ((range (org-get-property-block)))
    (when range
      (goto-char (car range))
      (let* ((case-fold-search t)
	     (end (cdr range))
	     (value
	      ;; Base value.
	      (save-excursion
		(let ((v (and (re-search-forward
			       (org-re-property property nil t) end t)
			      (match-string-no-properties 3))))
		  (list (if literal-nil v (org-not-nil v)))))))
	;; Find additional values.
	(let* ((property+ (org-re-property (concat property "+") nil t)))
	  (while (re-search-forward property+ end t)
	    (push (match-string-no-properties 3) value)))
	;; Return final values.
	(and (not (equal value '(nil))) (nreverse value))))))

(defun org--property-global-value (property literal-nil)
  "Return value for PROPERTY in current buffer.
Return value is a string.  Return nil if property is not set
globally.  Also return nil when PROPERTY is set to \"nil\",
unless LITERAL-NIL is non-nil."
  (let ((global
	 (cdr (or (assoc-string property org-file-properties t)
		  (assoc-string property org-global-properties t)
		  (assoc-string property org-global-properties-fixed t)))))
    (if literal-nil global (org-not-nil global))))

(defun org-entry-get (pom property &optional inherit literal-nil)
  "Get value of PROPERTY for entry or content at point-or-marker POM.

If INHERIT is non-nil and the entry does not have the property,
then also check higher levels of the hierarchy.  If INHERIT is
the symbol `selective', use inheritance only if the setting in
`org-use-property-inheritance' selects PROPERTY for inheritance.

If the property is present but empty, the return value is the
empty string.  If the property is not present at all, nil is
returned.  In any other case, return the value as a string.
Search is case-insensitive.

If LITERAL-NIL is set, return the string value \"nil\" as
a string, do not interpret it as the list atom nil.  This is used
for inheritance when a \"nil\" value can supersede a non-nil
value higher up the hierarchy."
  (org-with-point-at pom
    (cond
     ((member-ignore-case property (cons "CATEGORY" org-special-properties))
      ;; We need a special property.  Use `org-entry-properties' to
      ;; retrieve it, but specify the wanted property.
      (cdr (assoc-string property (org-entry-properties nil property))))
     ((and inherit
	   (or (not (eq inherit 'selective)) (org-property-inherit-p property)))
      (org-entry-get-with-inheritance property literal-nil))
     (t
      (let* ((local (org--property-local-values property literal-nil))
	     (value (and local (mapconcat #'identity (delq nil local) " "))))
	(if literal-nil value (org-not-nil value)))))))

(defun org-property-or-variable-value (var &optional inherit)
  "Check if there is a property fixing the value of VAR.
If yes, return this value.  If not, return the current value of the variable."
  (let ((prop (org-entry-get nil (symbol-name var) inherit)))
    (if (and prop (stringp prop) (string-match "\\S-" prop))
	(read prop)
      (symbol-value var))))

(defun org-entry-delete (pom property)
  "Delete PROPERTY from entry at point-or-marker POM.
Accumulated properties, i.e. PROPERTY+, are also removed.  Return
non-nil when a property was removed."
  (org-with-point-at pom
    (pcase (org-get-property-block)
      (`(,begin . ,origin)
       (let* ((end (copy-marker origin))
	      (re (org-re-property
		   (concat (regexp-quote property) "\\+?") t t)))
	 (goto-char begin)
	 (while (re-search-forward re end t)
	   (delete-region (match-beginning 0) (line-beginning-position 2)))
	 ;; If drawer is empty, remove it altogether.
	 (when (= begin end)
	   (delete-region (line-beginning-position 0)
			  (line-beginning-position 2)))
	 ;; Return non-nil if some property was removed.
	 (prog1 (/= end origin) (set-marker end nil))))
      (_ nil))))

;; Multi-values properties are properties that contain multiple values
;; These values are assumed to be single words, separated by whitespace.
(defun org-entry-add-to-multivalued-property (pom property value)
  "Add VALUE to the words in the PROPERTY in entry at point-or-marker POM."
  (let* ((old (org-entry-get pom property))
	 (values (and old (split-string old))))
    (setq value (org-entry-protect-space value))
    (unless (member value values)
      (setq values (append values (list value)))
      (org-entry-put pom property (mapconcat #'identity values " ")))))

(defun org-entry-remove-from-multivalued-property (pom property value)
  "Remove VALUE from words in the PROPERTY in entry at point-or-marker POM."
  (let* ((old (org-entry-get pom property))
	 (values (and old (split-string old))))
    (setq value (org-entry-protect-space value))
    (when (member value values)
      (setq values (delete value values))
      (org-entry-put pom property (mapconcat #'identity values " ")))))

(defun org-entry-member-in-multivalued-property (pom property value)
  "Is VALUE one of the words in the PROPERTY in entry at point-or-marker POM?"
  (let* ((old (org-entry-get pom property))
	 (values (and old (split-string old))))
    (setq value (org-entry-protect-space value))
    (member value values)))

(defun org-entry-get-multivalued-property (pom property)
  "Return a list of values in a multivalued property."
  (let* ((value (org-entry-get pom property))
	 (values (and value (split-string value))))
    (mapcar #'org-entry-restore-space values)))

(defun org-entry-put-multivalued-property (pom property &rest values)
  "Set multivalued PROPERTY at point-or-marker POM to VALUES.
VALUES should be a list of strings.  Spaces will be protected."
  (org-entry-put pom property (mapconcat #'org-entry-protect-space values " "))
  (let* ((value (org-entry-get pom property))
	 (values (and value (split-string value))))
    (mapcar #'org-entry-restore-space values)))

(defun org-entry-protect-space (s)
  "Protect spaces and newline in string S."
  (while (string-match " " s)
    (setq s (replace-match "%20" t t s)))
  (while (string-match "\n" s)
    (setq s (replace-match "%0A" t t s)))
  s)

(defun org-entry-restore-space (s)
  "Restore spaces and newline in string S."
  (while (string-match "%20" s)
    (setq s (replace-match " " t t s)))
  (while (string-match "%0A" s)
    (setq s (replace-match "\n" t t s)))
  s)

(defvar org-entry-property-inherited-from (make-marker)
  "Marker pointing to the entry from where a property was inherited.
Each call to `org-entry-get-with-inheritance' will set this marker to the
location of the entry where the inheritance search matched.  If there was
no match, the marker will point nowhere.
Note that also `org-entry-get' calls this function, if the INHERIT flag
is set.")

(defun org-entry-get-with-inheritance (property &optional literal-nil)
  "Get PROPERTY of entry or content at point, search higher levels if needed.
The search will stop at the first ancestor which has the property defined.
If the value found is \"nil\", return nil to show that the property
should be considered as undefined (this is the meaning of nil here).
However, if LITERAL-NIL is set, return the string value \"nil\" instead."
  (move-marker org-entry-property-inherited-from nil)
  (org-with-wide-buffer
   (let (value)
     (catch 'exit
       (while t
	 (let ((v (org--property-local-values property literal-nil)))
	   (when v
	     (setq value
		   (concat (mapconcat #'identity (delq nil v) " ")
			   (and value " ")
			   value)))
	   (cond
	    ((car v)
	     (org-back-to-heading t)
	     (move-marker org-entry-property-inherited-from (point))
	     (throw 'exit nil))
	    ((org-up-heading-safe))
	    (t
	     (let ((global (org--property-global-value property literal-nil)))
	       (cond ((not global))
		     (value (setq value (concat global " " value)))
		     (t (setq value global))))
	     (throw 'exit nil))))))
     (if literal-nil value (org-not-nil value)))))

(defvar org-property-changed-functions nil
  "Hook called when the value of a property has changed.
Each hook function should accept two arguments, the name of the property
and the new value.")

(defun org-entry-put (pom property value)
  "Set PROPERTY to VALUE for entry at point-or-marker POM.

If the value is nil, it is converted to the empty string.  If it
is not a string, an error is raised.  Also raise an error on
invalid property names.

PROPERTY can be any regular property (see
`org-special-properties').  It can also be \"TODO\",
\"PRIORITY\", \"SCHEDULED\" and \"DEADLINE\".

For the last two properties, VALUE may have any of the special
values \"earlier\" and \"later\".  The function then increases or
decreases scheduled or deadline date by one day."
  (cond ((null value) (setq value ""))
	((not (stringp value)) (error "Properties values should be strings"))
	((not (org--valid-property-p property))
	 (user-error "Invalid property name: \"%s\"" property)))
  (org-with-point-at pom
    (if (or (not (featurep 'org-inlinetask)) (org-inlinetask-in-task-p))
	(org-back-to-heading t)
      (org-with-limited-levels (org-back-to-heading t)))
    (let ((beg (point)))
      (cond
       ((equal property "TODO")
	(cond ((not (org-string-nw-p value)) (setq value 'none))
	      ((not (member value org-todo-keywords-1))
	       (user-error "\"%s\" is not a valid TODO state" value)))
	(org-todo value)
	(org-set-tags nil 'align))
       ((equal property "PRIORITY")
	(org-priority (if (org-string-nw-p value) (string-to-char value) ?\s))
	(org-set-tags nil 'align))
       ((equal property "SCHEDULED")
	(forward-line)
	(if (and (looking-at-p org-planning-line-re)
		 (re-search-forward
		  org-scheduled-time-regexp (line-end-position) t))
	    (cond ((string= value "earlier") (org-timestamp-change -1 'day))
		  ((string= value "later") (org-timestamp-change 1 'day))
		  ((string= value "") (org-schedule '(4)))
		  (t (org-schedule nil value)))
	  (if (member value '("earlier" "later" ""))
	      (call-interactively #'org-schedule)
	    (org-schedule nil value))))
       ((equal property "DEADLINE")
	(forward-line)
	(if (and (looking-at-p org-planning-line-re)
		 (re-search-forward
		  org-deadline-time-regexp (line-end-position) t))
	    (cond ((string= value "earlier") (org-timestamp-change -1 'day))
		  ((string= value "later") (org-timestamp-change 1 'day))
		  ((string= value "") (org-deadline '(4)))
		  (t (org-deadline nil value)))
	  (if (member value '("earlier" "later" ""))
	      (call-interactively #'org-deadline)
	    (org-deadline nil value))))
       ((member property org-special-properties)
	(error "The %s property cannot be set with `org-entry-put'" property))
       (t
	(let* ((range (org-get-property-block beg 'force))
	       (end (cdr range))
	       (case-fold-search t))
	  (goto-char (car range))
	  (if (re-search-forward (org-re-property property nil t) end t)
	      (progn (delete-region (match-beginning 0) (match-end 0))
		     (goto-char (match-beginning 0)))
	    (goto-char end)
	    (insert "\n")
	    (backward-char))
	  (insert ":" property ":")
	  (when value (insert " " value))
	  (org-indent-line)))))
    (run-hook-with-args 'org-property-changed-functions property value)))

(defun org-buffer-property-keys
    (&optional specials defaults columns ignore-malformed)
  "Get all property keys in the current buffer.

When SPECIALS is non-nil, also list the special properties that
reflect things like tags and TODO state.

When DEFAULTS is non-nil, also include properties that has
special meaning internally: ARCHIVE, CATEGORY, SUMMARY,
DESCRIPTION, LOCATION, and LOGGING and others.

When COLUMNS in non-nil, also include property names given in
COLUMN formats in the current buffer.

When IGNORE-MALFORMED is non-nil, malformed drawer repair will not be
automatically performed, such drawers will be silently ignored."
  (let ((case-fold-search t)
	(props (append
		(and specials org-special-properties)
		(and defaults (cons org-effort-property org-default-properties))
		nil)))
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward org-property-start-re nil t)
       (let ((range (org-get-property-block)))
	 (catch 'skip
	   (unless range
	     (when (and (not ignore-malformed)
			(not (org-before-first-heading-p))
			(y-or-n-p (format "Malformed drawer at %d, repair?"
					  (line-beginning-position))))
	       (org-get-property-block nil t))
	     (throw 'skip nil))
	   (goto-char (car range))
	   (let ((begin (car range))
		 (end (cdr range)))
	     ;; Make sure that found property block is not located
	     ;; before current point, as it would generate an infloop.
	     ;; It can happen, for example, in the following
	     ;; situation:
	     ;;
	     ;; * Headline
	     ;;   :PROPERTIES:
	     ;;   ...
	     ;;   :END:
	     ;; *************** Inlinetask
	     ;; #+BEGIN_EXAMPLE
	     ;; :PROPERTIES:
	     ;; #+END_EXAMPLE
	     ;;
	     (if (< begin (point)) (throw 'skip nil) (goto-char begin))
	     (while (< (point) end)
	       (let ((p (progn (looking-at org-property-re)
			       (match-string-no-properties 2))))
		 ;; Only add true property name, not extension symbol.
		 (push (if (not (string-match-p "\\+\\'" p)) p
			 (substring p 0 -1))
		       props))
	       (forward-line))))
	 (outline-next-heading)))
     (when columns
       (goto-char (point-min))
       (while (re-search-forward "^[ \t]*\\(?:#\\+\\|:\\)COLUMNS:" nil t)
	 (let ((element (org-element-at-point)))
	   (when (memq (org-element-type element) '(keyword node-property))
	     (let ((value (org-element-property :value element))
		   (start 0))
	       (while (string-match "%[0-9]*\\([[:alnum:]_-]+\\)\\(([^)]+)\\)?\
\\(?:{[^}]+}\\)?"
				    value start)
		 (setq start (match-end 0))
		 (let ((p (match-string-no-properties 1 value)))
		   (unless (member-ignore-case p org-special-properties)
		     (push p props))))))))))
    (sort (delete-dups props) (lambda (a b) (string< (upcase a) (upcase b))))))

(defun org-property-values (key)
  "List all non-nil values of property KEY in current buffer."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((case-fold-search t)
	 (re (org-re-property key))
	 values)
     (while (re-search-forward re nil t)
       (push (org-entry-get (point) key) values))
     (delete-dups values))))

(defun org-insert-property-drawer ()
  "Insert a property drawer into the current entry."
  (org-with-wide-buffer
   (if (or (not (featurep 'org-inlinetask)) (org-inlinetask-in-task-p))
       (org-back-to-heading t)
     (org-with-limited-levels (org-back-to-heading t)))
   (forward-line)
   (when (looking-at-p org-planning-line-re) (forward-line))
   (unless (looking-at-p org-property-drawer-re)
     ;; Make sure we start editing a line from current entry, not from
     ;; next one.  It prevents extending text properties or overlays
     ;; belonging to the latter.
     (when (bolp) (backward-char))
     (let ((begin (1+ (point)))
	   (inhibit-read-only t))
       (insert "\n:PROPERTIES:\n:END:")
       (when (eobp) (insert "\n"))
       (org-indent-region begin (point))))))

(defun org-insert-drawer (&optional arg drawer)
  "Insert a drawer at point.

When optional argument ARG is non-nil, insert a property drawer.

Optional argument DRAWER, when non-nil, is a string representing
drawer's name.  Otherwise, the user is prompted for a name.

If a region is active, insert the drawer around that region
instead.

Point is left between drawer's boundaries."
  (interactive "P")
  (let* ((drawer (if arg "PROPERTIES"
		   (or drawer (read-from-minibuffer "Drawer: ")))))
    (cond
     ;; With C-u, fall back on `org-insert-property-drawer'
     (arg (org-insert-property-drawer))
     ;; Check validity of suggested drawer's name.
     ((not (string-match-p org-drawer-regexp (format ":%s:" drawer)))
      (user-error "Invalid drawer name"))
     ;; With an active region, insert a drawer at point.
     ((not (org-region-active-p))
      (progn
	(unless (bolp) (insert "\n"))
	(insert (format ":%s:\n\n:END:\n" drawer))
	(forward-line -2)))
     ;; Otherwise, insert the drawer at point
     (t
      (let ((rbeg (region-beginning))
	    (rend (copy-marker (region-end))))
	(unwind-protect
	    (progn
	      (goto-char rbeg)
	      (beginning-of-line)
	      (when (save-excursion
		      (re-search-forward org-outline-regexp-bol rend t))
		(user-error "Drawers cannot contain headlines"))
	      ;; Position point at the beginning of the first
	      ;; non-blank line in region.  Insert drawer's opening
	      ;; there, then indent it.
	      (org-skip-whitespace)
	      (beginning-of-line)
	      (insert ":" drawer ":\n")
	      (forward-line -1)
	      (indent-for-tab-command)
	      ;; Move point to the beginning of the first blank line
	      ;; after the last non-blank line in region.  Insert
	      ;; drawer's closing, then indent it.
	      (goto-char rend)
	      (skip-chars-backward " \r\t\n")
	      (insert "\n:END:")
	      (deactivate-mark t)
	      (indent-for-tab-command)
	      (unless (eolp) (insert "\n")))
	  ;; Clear marker, whatever the outcome of insertion is.
	  (set-marker rend nil)))))))

(defvar org-property-set-functions-alist nil
  "Property set function alist.
Each entry should have the following format:

 (PROPERTY . READ-FUNCTION)

The read function will be called with the same argument as
`org-completing-read'.")

(defun org-set-property-function (property)
  "Get the function that should be used to set PROPERTY.
This is computed according to `org-property-set-functions-alist'."
  (or (cdr (assoc property org-property-set-functions-alist))
      'org-completing-read))

(defun org-read-property-value (property)
  "Read PROPERTY value from user."
  (let* ((completion-ignore-case t)
	 (allowed (org-property-get-allowed-values nil property 'table))
	 (cur (org-entry-get nil property))
	 (prompt (concat property " value"
			 (if (and cur (string-match "\\S-" cur))
			     (concat " [" cur "]") "") ": "))
	 (set-function (org-set-property-function property))
	 (val (if allowed
		  (funcall set-function prompt allowed nil
			   (not (get-text-property 0 'org-unrestricted
						   (caar allowed))))
		(funcall set-function prompt
			 (mapcar 'list (org-property-values property))
			 nil nil "" nil cur))))
    (org-trim val)))

(defvar org-last-set-property nil)
(defvar org-last-set-property-value nil)
(defun org-read-property-name ()
  "Read a property name."
  (let ((completion-ignore-case t)
	(default-prop (or (and (org-at-property-p)
			       (match-string-no-properties 2))
			  org-last-set-property)))
    (org-completing-read
     (concat "Property"
	     (if default-prop (concat " [" default-prop "]") "")
	     ": ")
     (mapcar #'list (org-buffer-property-keys nil t t))
     nil nil nil nil default-prop)))

(defun org-set-property-and-value (use-last)
  "Allow to set [PROPERTY]: [value] direction from prompt.
When use-default, don't even ask, just use the last
\"[PROPERTY]: [value]\" string from the history."
  (interactive "P")
  (let* ((completion-ignore-case t)
	 (pv (or (and use-last org-last-set-property-value)
		 (org-completing-read
		  "Enter a \"[Property]: [value]\" pair: "
		  nil nil nil nil nil
		  org-last-set-property-value)))
	 prop val)
    (when (string-match "^[ \t]*\\([^:]+\\):[ \t]*\\(.*\\)[ \t]*$" pv)
      (setq prop (match-string 1 pv)
	    val (match-string 2 pv))
      (org-set-property prop val))))

(defun org-set-property (property value)
  "In the current entry, set PROPERTY to VALUE.

When called interactively, this will prompt for a property name, offering
completion on existing and default properties.  And then it will prompt
for a value, offering completion either on allowed values (via an inherited
xxx_ALL property) or on existing values in other instances of this property
in the current file.

Throw an error when trying to set a property with an invalid name."
  (interactive (list nil nil))
  (let ((property (or property (org-read-property-name))))
    ;; `org-entry-put' also makes the following check, but this one
    ;; avoids polluting `org-last-set-property' and
    ;; `org-last-set-property-value' needlessly.
    (unless (org--valid-property-p property)
      (user-error "Invalid property name: \"%s\"" property))
    (let ((value (or value (org-read-property-value property)))
	  (fn (cdr (assoc-string property org-properties-postprocess-alist t))))
      (setq org-last-set-property property)
      (setq org-last-set-property-value (concat property ": " value))
      ;; Possibly postprocess the inserted value:
      (when fn (setq value (funcall fn value)))
      (unless (equal (org-entry-get nil property) value)
	(org-entry-put nil property value)))))

(defun org-find-property (property &optional value)
  "Find first entry in buffer that sets PROPERTY.

When optional argument VALUE is non-nil, only consider an entry
if it contains PROPERTY set to this value.  If PROPERTY should be
explicitly set to nil, use string \"nil\" for VALUE.

Return position where the entry begins, or nil if there is no
such entry.  If narrowing is in effect, only search the visible
part of the buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (re (org-re-property property nil (not value) value)))
      (catch 'exit
	(while (re-search-forward re nil t)
	  (when (if value (org-at-property-p)
		  (org-entry-get (point) property nil t))
	    (throw 'exit (progn (org-back-to-heading t) (point)))))))))

(defun org-delete-property (property)
  "In the current entry, delete PROPERTY."
  (interactive
   (let* ((completion-ignore-case t)
	  (cat (org-entry-get (point) "CATEGORY"))
	  (props0 (org-entry-properties nil 'standard))
	  (props (if cat props0
		   (delete `("CATEGORY" . ,(org-get-category)) props0)))
	  (prop (if (< 1 (length props))
		    (completing-read "Property: " props nil t)
		  (caar props))))
     (list prop)))
  (if (not property)
      (message "No property to delete in this entry")
    (org-entry-delete nil property)
    (message "Property \"%s\" deleted" property)))

(defun org-delete-property-globally (property)
  "Remove PROPERTY globally, from all entries.
This function ignores narrowing, if any."
  (interactive
   (let* ((completion-ignore-case t)
	  (prop (completing-read
		 "Globally remove property: "
		 (mapcar #'list (org-buffer-property-keys)))))
     (list prop)))
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((count 0)
	 (re (org-re-property (concat (regexp-quote property) "\\+?") t t)))
     (while (re-search-forward re nil t)
       (when (org-entry-delete (point) property) (cl-incf count)))
     (message "Property \"%s\" removed from %d entries" property count))))

(defvar org-columns-current-fmt-compiled) ; defined in org-colview.el

(defun org-compute-property-at-point ()
  "Compute the property at point.
This looks for an enclosing column format, extracts the operator and
then applies it to the property in the column format's scope."
  (interactive)
  (unless (org-at-property-p)
    (user-error "Not at a property"))
  (let ((prop (match-string-no-properties 2)))
    (org-columns-get-format-and-top-level)
    (unless (nth 3 (assoc-string prop org-columns-current-fmt-compiled t))
      (user-error "No operator defined for property %s" prop))
    (org-columns-compute prop)))

(defvar org-property-allowed-value-functions nil
  "Hook for functions supplying allowed values for a specific property.
The functions must take a single argument, the name of the property, and
return a flat list of allowed values.  If \":ETC\" is one of
the values, this means that these values are intended as defaults for
completion, but that other values should be allowed too.
The functions must return nil if they are not responsible for this
property.")

(defun org-property-get-allowed-values (pom property &optional table)
  "Get allowed values for the property PROPERTY.
When TABLE is non-nil, return an alist that can directly be used for
completion."
  (let (vals)
    (cond
     ((equal property "TODO")
      (setq vals (org-with-point-at pom
		   (append org-todo-keywords-1 '("")))))
     ((equal property "PRIORITY")
      (let ((n org-lowest-priority))
	(while (>= n org-highest-priority)
	  (push (char-to-string n) vals)
	  (setq n (1- n)))))
     ((equal property "CATEGORY"))
     ((member property org-special-properties))
     ((setq vals (run-hook-with-args-until-success
		  'org-property-allowed-value-functions property)))
     (t
      (setq vals (org-entry-get pom (concat property "_ALL") 'inherit))
      (when (and vals (string-match "\\S-" vals))
	(setq vals (car (read-from-string (concat "(" vals ")"))))
	(setq vals (mapcar (lambda (x)
			     (cond ((stringp x) x)
				   ((numberp x) (number-to-string x))
				   ((symbolp x) (symbol-name x))
				   (t "???")))
			   vals)))))
    (when (member ":ETC" vals)
      (setq vals (remove ":ETC" vals))
      (org-add-props (car vals) '(org-unrestricted t)))
    (if table (mapcar 'list vals) vals)))

(defun org-property-previous-allowed-value (&optional _previous)
  "Switch to the next allowed value for this property."
  (interactive)
  (org-property-next-allowed-value t))

(defun org-property-next-allowed-value (&optional previous)
  "Switch to the next allowed value for this property."
  (interactive)
  (unless (org-at-property-p)
    (user-error "Not at a property"))
  (let* ((prop (car (save-match-data (org-split-string (match-string 1) ":"))))
	 (key (match-string 2))
	 (value (match-string 3))
	 (allowed (or (org-property-get-allowed-values (point) key)
		      (and (member value  '("[ ]" "[-]" "[X]"))
			   '("[ ]" "[X]"))))
	 (heading (save-match-data (nth 4 (org-heading-components))))
	 nval)
    (unless allowed
      (user-error "Allowed values for this property have not been defined"))
    (when previous (setq allowed (reverse allowed)))
    (when (member value allowed)
      (setq nval (car (cdr (member value allowed)))))
    (setq nval (or nval (car allowed)))
    (when (equal nval value)
      (user-error "Only one allowed value for this property"))
    (org-at-property-p)
    (replace-match (concat " :" key ": " nval) t t)
    (org-indent-line)
    (beginning-of-line 1)
    (skip-chars-forward " \t")
    (when (equal prop org-effort-property)
      (org-refresh-property
       '((effort . identity)
	 (effort-minutes . org-duration-to-minutes))
       nval)
      (when (string= org-clock-current-task heading)
	(setq org-clock-effort nval)
	(org-clock-update-mode-line)))
    (run-hook-with-args 'org-property-changed-functions key nval)))

(defun org-find-olp (path &optional this-buffer)
  "Return a marker pointing to the entry at outline path OLP.
If anything goes wrong, throw an error.
You can wrap this call to catch the error like this:

  (condition-case msg
      (org-mobile-locate-entry (match-string 4))
    (error (nth 1 msg)))

The return value will then be either a string with the error message,
or a marker if everything is OK.

If THIS-BUFFER is set, the outline path does not contain a file,
only headings."
  (let* ((file (if this-buffer buffer-file-name (pop path)))
	 (buffer (if this-buffer (current-buffer) (find-file-noselect file)))
	 (level 1)
	 (lmin 1)
	 (lmax 1)
	 end found flevel)
    (unless buffer (error "File not found :%s" file))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode)
	(error "Buffer %s needs to be in Org mode" buffer))
      (org-with-wide-buffer
       (goto-char (point-min))
       (dolist (heading path)
	 (let ((re (format org-complex-heading-regexp-format
			   (regexp-quote heading)))
	       (cnt 0))
	   (while (re-search-forward re end t)
	     (setq level (- (match-end 1) (match-beginning 1)))
	     (when (and (>= level lmin) (<= level lmax))
	       (setq found (match-beginning 0) flevel level cnt (1+ cnt))))
	   (when (= cnt 0)
	     (error "Heading not found on level %d: %s" lmax heading))
	   (when (> cnt 1)
	     (error "Heading not unique on level %d: %s" lmax heading))
	   (goto-char found)
	   (setq lmin (1+ flevel) lmax (+ lmin (if org-odd-levels-only 1 0)))
	   (setq end (save-excursion (org-end-of-subtree t t)))))
       (when (org-at-heading-p)
	 (point-marker))))))

(defun org-find-exact-headline-in-buffer (heading &optional buffer pos-only)
  "Find node HEADING in BUFFER.
Return a marker to the heading if it was found, or nil if not.
If POS-ONLY is set, return just the position instead of a marker.

The heading text must match exact, but it may have a TODO keyword,
a priority cookie and tags in the standard locations."
  (with-current-buffer (or buffer (current-buffer))
    (org-with-wide-buffer
     (goto-char (point-min))
     (let (case-fold-search)
       (when (re-search-forward
	      (format org-complex-heading-regexp-format
		      (regexp-quote heading)) nil t)
	 (if pos-only
	     (match-beginning 0)
	   (move-marker (make-marker) (match-beginning 0))))))))

(defun org-find-exact-heading-in-directory (heading &optional dir)
  "Find Org node headline HEADING in all .org files in directory DIR.
When the target headline is found, return a marker to this location."
  (let ((files (directory-files (or dir default-directory)
				t "\\`[^.#].*\\.org\\'"))
	visiting m buffer)
    (catch 'found
      (dolist (file files)
        (message "trying %s" file)
        (setq visiting (org-find-base-buffer-visiting file))
        (setq buffer (or visiting (find-file-noselect file)))
        (setq m (org-find-exact-headline-in-buffer
                 heading buffer))
        (when (and (not m) (not visiting)) (kill-buffer buffer))
        (and m (throw 'found m))))))

(defun org-find-entry-with-id (ident)
  "Locate the entry that contains the ID property with exact value IDENT.
IDENT can be a string, a symbol or a number, this function will search for
the string representation of it.
Return the position where this entry starts, or nil if there is no such entry."
  (interactive "sID: ")
  (let ((id (cond
	     ((stringp ident) ident)
	     ((symbolp ident) (symbol-name ident))
	     ((numberp ident) (number-to-string ident))
	     (t (error "IDENT %s must be a string, symbol or number" ident)))))
    (org-with-wide-buffer (org-find-property "ID" id))))

;;;; Timestamps

(defvar org-last-changed-timestamp nil)
(defvar org-last-inserted-timestamp nil
  "The last time stamp inserted with `org-insert-time-stamp'.")

(defun org-time-stamp (arg &optional inactive)
  "Prompt for a date/time and insert a time stamp.

If the user specifies a time like HH:MM or if this command is
called with at least one prefix argument, the time stamp contains
the date and the time.  Otherwise, only the date is included.

All parts of a date not specified by the user are filled in from
the timestamp at point, if any, or the current date/time
otherwise.

If there is already a timestamp at the cursor, it is replaced.

With two universal prefix arguments, insert an active timestamp
with the current time without prompting the user.

When called from lisp, the timestamp is inactive if INACTIVE is
non-nil."
  (interactive "P")
  (let* ((ts (cond
	      ((org-at-date-range-p t)
	       (match-string (if (< (point) (- (match-beginning 2) 2)) 1 2)))
	      ((org-at-timestamp-p 'lax) (match-string 0))))
	 ;; Default time is either the timestamp at point or today.
	 ;; When entering a range, only the range start is considered.
         (default-time (if (not ts) (current-time)
			 (apply #'encode-time (org-parse-time-string ts))))
         (default-input (and ts (org-get-compact-tod ts)))
         (repeater (and ts
			(string-match "\\([.+-]+[0-9]+[hdwmy] ?\\)+" ts)
			(match-string 0 ts)))
	 org-time-was-given
	 org-end-time-was-given
	 (time
	  (and (if (equal arg '(16)) (current-time)
		 ;; Preserve `this-command' and `last-command'.
		 (let ((this-command this-command)
		       (last-command last-command))
		   (org-read-date
		    arg 'totime nil nil default-time default-input
		    inactive))))))
    (cond
     ((and ts
           (memq last-command '(org-time-stamp org-time-stamp-inactive))
           (memq this-command '(org-time-stamp org-time-stamp-inactive)))
      (insert "--")
      (org-insert-time-stamp time (or org-time-was-given arg) inactive))
     (ts
      ;; Make sure we're on a timestamp.  When in the middle of a date
      ;; range, move arbitrarily to range end.
      (unless (org-at-timestamp-p 'lax)
	(skip-chars-forward "-")
	(org-at-timestamp-p 'lax))
      (replace-match "")
      (setq org-last-changed-timestamp
	    (org-insert-time-stamp
	     time (or org-time-was-given arg)
	     inactive nil nil (list org-end-time-was-given)))
      (when repeater
	(backward-char)
	(insert " " repeater)
	(setq org-last-changed-timestamp
	      (concat (substring org-last-inserted-timestamp 0 -1)
		      " " repeater ">")))
      (message "Timestamp updated"))
     ((equal arg '(16)) (org-insert-time-stamp time t inactive))
     (t (org-insert-time-stamp
	 time (or org-time-was-given arg) inactive nil nil
	 (list org-end-time-was-given))))))

;; FIXME: can we use this for something else, like computing time differences?
(defun org-get-compact-tod (s)
  (when (string-match "\\(\\([012]?[0-9]\\):\\([0-5][0-9]\\)\\)\\(-\\(\\([012]?[0-9]\\):\\([0-5][0-9]\\)\\)\\)?" s)
    (let* ((t1 (match-string 1 s))
	   (h1 (string-to-number (match-string 2 s)))
	   (m1 (string-to-number (match-string 3 s)))
	   (t2 (and (match-end 4) (match-string 5 s)))
	   (h2 (and t2 (string-to-number (match-string 6 s))))
	   (m2 (and t2 (string-to-number (match-string 7 s))))
	   dh dm)
      (if (not t2)
	  t1
	(setq dh (- h2 h1) dm (- m2 m1))
	(when (< dm 0) (setq dm (+ dm 60) dh (1- dh)))
	(concat t1 "+" (number-to-string dh)
		(and (/= 0 dm) (format ":%02d" dm)))))))

(defun org-time-stamp-inactive (&optional arg)
  "Insert an inactive time stamp.
An inactive time stamp is enclosed in square brackets instead of angle
brackets.  It is inactive in the sense that it does not trigger agenda entries,
does not link to the calendar and cannot be changed with the S-cursor keys.
So these are more for recording a certain time/date."
  (interactive "P")
  (org-time-stamp arg 'inactive))

(defvar org-date-ovl (make-overlay 1 1))
(overlay-put org-date-ovl 'face 'org-date-selected)
(delete-overlay org-date-ovl)

(defvar org-ans1) ; dynamically scoped parameter
(defvar org-ans2) ; dynamically scoped parameter

(defvar org-plain-time-of-day-regexp) ; defined below

(defvar org-overriding-default-time nil) ; dynamically scoped
(defvar org-read-date-overlay nil)
(defvar org-dcst nil) ; dynamically scoped
(defvar org-read-date-history nil)
(defvar org-read-date-final-answer nil)
(defvar org-read-date-analyze-futurep nil)
(defvar org-read-date-analyze-forced-year nil)
(defvar org-read-date-inactive)

(defvar org-read-date-minibuffer-local-map
  (let* ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (org-defkey map (kbd ".")
                (lambda () (interactive)
		  ;; Are we at the beginning of the prompt?
		  (if (looking-back "^[^:]+: "
				    (let ((inhibit-field-text-motion t))
				      (line-beginning-position)))
		      (org-eval-in-calendar '(calendar-goto-today))
		    (insert "."))))
    (org-defkey map (kbd "C-.")
                (lambda () (interactive)
		  (org-eval-in-calendar '(calendar-goto-today))))
    (org-defkey map [(meta shift left)]
                (lambda () (interactive)
                  (org-eval-in-calendar '(calendar-backward-month 1))))
    (org-defkey map [(meta shift right)]
                (lambda () (interactive)
                  (org-eval-in-calendar '(calendar-forward-month 1))))
    (org-defkey map [(meta shift up)]
                (lambda () (interactive)
                  (org-eval-in-calendar '(calendar-backward-year 1))))
    (org-defkey map [(meta shift down)]
                (lambda () (interactive)
                  (org-eval-in-calendar '(calendar-forward-year 1))))
    (org-defkey map [?\e (shift left)]
                (lambda () (interactive)
                  (org-eval-in-calendar '(calendar-backward-month 1))))
    (org-defkey map [?\e (shift right)]
                (lambda () (interactive)
                  (org-eval-in-calendar '(calendar-forward-month 1))))
    (org-defkey map [?\e (shift up)]
                (lambda () (interactive)
                  (org-eval-in-calendar '(calendar-backward-year 1))))
    (org-defkey map [?\e (shift down)]
                (lambda () (interactive)
                  (org-eval-in-calendar '(calendar-forward-year 1))))
    (org-defkey map [(shift up)]
                (lambda () (interactive)
                  (org-eval-in-calendar '(calendar-backward-week 1))))
    (org-defkey map [(shift down)]
                (lambda () (interactive)
                  (org-eval-in-calendar '(calendar-forward-week 1))))
    (org-defkey map [(shift left)]
                (lambda () (interactive)
                  (org-eval-in-calendar '(calendar-backward-day 1))))
    (org-defkey map [(shift right)]
                (lambda () (interactive)
                  (org-eval-in-calendar '(calendar-forward-day 1))))
    (org-defkey map "!"
                (lambda () (interactive)
                  (org-eval-in-calendar '(diary-view-entries))
                  (message "")))
    (org-defkey map ">"
                (lambda () (interactive)
                  (org-eval-in-calendar '(calendar-scroll-left 1))))
    (org-defkey map "<"
                (lambda () (interactive)
                  (org-eval-in-calendar '(calendar-scroll-right 1))))
    (org-defkey map "\C-v"
                (lambda () (interactive)
                  (org-eval-in-calendar
                   '(calendar-scroll-left-three-months 1))))
    (org-defkey map "\M-v"
                (lambda () (interactive)
                  (org-eval-in-calendar
                   '(calendar-scroll-right-three-months 1))))
    map)
  "Keymap for minibuffer commands when using `org-read-date'.")

(defvar org-def)
(defvar org-defdecode)
(defvar org-with-time)

(defvar calendar-setup)			; Dynamically scoped.
(defun org-read-date (&optional with-time to-time from-string prompt
				default-time default-input inactive)
  "Read a date, possibly a time, and make things smooth for the user.
The prompt will suggest to enter an ISO date, but you can also enter anything
which will at least partially be understood by `parse-time-string'.
Unrecognized parts of the date will default to the current day, month, year,
hour and minute.  If this command is called to replace a timestamp at point,
or to enter the second timestamp of a range, the default time is taken
from the existing stamp.  Furthermore, the command prefers the future,
so if you are giving a date where the year is not given, and the day-month
combination is already past in the current year, it will assume you
mean next year.  For details, see the manual.  A few examples:

  3-2-5         --> 2003-02-05
  feb 15        --> currentyear-02-15
  2/15          --> currentyear-02-15
  sep 12 9      --> 2009-09-12
  12:45         --> today 12:45
  22 sept 0:34  --> currentyear-09-22 0:34
  12            --> currentyear-currentmonth-12
  Fri           --> nearest Friday after today
  -Tue          --> last Tuesday
  etc.

Furthermore you can specify a relative date by giving, as the *first* thing
in the input:  a plus/minus sign, a number and a letter [hdwmy] to indicate
change in days weeks, months, years.
With a single plus or minus, the date is relative to today.  With a double
plus or minus, it is relative to the date in DEFAULT-TIME.  E.g.
  +4d           --> four days from today
  +4            --> same as above
  +2w           --> two weeks from today
  ++5           --> five days from default date

The function understands only English month and weekday abbreviations.

While prompting, a calendar is popped up - you can also select the
date with the mouse (button 1).  The calendar shows a period of three
months.  To scroll it to other months, use the keys `>' and `<'.
If you don't like the calendar, turn it off with
       (setq org-read-date-popup-calendar nil)

With optional argument TO-TIME, the date will immediately be converted
to an internal time.
With an optional argument WITH-TIME, the prompt will suggest to
also insert a time.  Note that when WITH-TIME is not set, you can
still enter a time, and this function will inform the calling routine
about this change.  The calling routine may then choose to change the
format used to insert the time stamp into the buffer to include the time.
With optional argument FROM-STRING, read from this string instead from
the user.  PROMPT can overwrite the default prompt.  DEFAULT-TIME is
the time/date that is used for everything that is not specified by the
user."
  (require 'parse-time)
  (let* ((org-with-time with-time)
	 (org-time-stamp-rounding-minutes
	  (if (equal org-with-time '(16))
	      '(0 0)
	    org-time-stamp-rounding-minutes))
	 (org-dcst org-display-custom-times)
	 (ct (org-current-time))
	 (org-def (or org-overriding-default-time default-time ct))
	 (org-defdecode (decode-time org-def))
         (cur-frame (selected-frame))
	 (mouse-autoselect-window nil)	; Don't let the mouse jump
	 (calendar-setup
	  (and (eq calendar-setup 'calendar-only) 'calendar-only))
	 (calendar-move-hook nil)
	 (calendar-view-diary-initially-flag nil)
	 (calendar-view-holidays-initially-flag nil)
	 ans (org-ans0 "") org-ans1 org-ans2 final cal-frame)
    ;; Rationalize `org-def' and `org-defdecode', if required.
    (when (< (nth 2 org-defdecode) org-extend-today-until)
      (setf (nth 2 org-defdecode) -1)
      (setf (nth 1 org-defdecode) 59)
      (setq org-def (apply #'encode-time org-defdecode))
      (setq org-defdecode (decode-time org-def)))
    (let* ((timestr (format-time-string
		     (if org-with-time "%Y-%m-%d %H:%M" "%Y-%m-%d")
		     org-def))
	   (prompt (concat (if prompt (concat prompt " ") "")
			   (format "Date+time [%s]: " timestr))))
      (cond
       (from-string (setq ans from-string))
       (org-read-date-popup-calendar
	(save-excursion
	  (save-window-excursion
	    (calendar)
	    (when (eq calendar-setup 'calendar-only)
	      (setq cal-frame
		    (window-frame (get-buffer-window "*Calendar*" 'visible)))
	      (select-frame cal-frame))
	    (org-eval-in-calendar '(setq cursor-type nil) t)
	    (unwind-protect
		(progn
		  (calendar-forward-day (- (time-to-days org-def)
					   (calendar-absolute-from-gregorian
					    (calendar-current-date))))
		  (org-eval-in-calendar nil t)
		  (let* ((old-map (current-local-map))
			 (map (copy-keymap calendar-mode-map))
			 (minibuffer-local-map
			  (copy-keymap org-read-date-minibuffer-local-map)))
		    (org-defkey map (kbd "RET") 'org-calendar-select)
		    (org-defkey map [mouse-1] 'org-calendar-select-mouse)
		    (org-defkey map [mouse-2] 'org-calendar-select-mouse)
		    (unwind-protect
			(progn
			  (use-local-map map)
			  (setq org-read-date-inactive inactive)
			  (add-hook 'post-command-hook 'org-read-date-display)
			  (setq org-ans0
				(read-string prompt
					     default-input
					     'org-read-date-history
					     nil))
			  ;; org-ans0: from prompt
			  ;; org-ans1: from mouse click
			  ;; org-ans2: from calendar motion
			  (setq ans
				(concat org-ans0 " " (or org-ans1 org-ans2))))
		      (remove-hook 'post-command-hook 'org-read-date-display)
		      (use-local-map old-map)
		      (when org-read-date-overlay
			(delete-overlay org-read-date-overlay)
			(setq org-read-date-overlay nil)))))
	      (bury-buffer "*Calendar*")
	      (when cal-frame
		(delete-frame cal-frame)
		(select-frame-set-input-focus cur-frame))))))

       (t				; Naked prompt only
	(unwind-protect
	    (setq ans (read-string prompt default-input
				   'org-read-date-history timestr))
	  (when org-read-date-overlay
	    (delete-overlay org-read-date-overlay)
	    (setq org-read-date-overlay nil))))))

    (setq final (org-read-date-analyze ans org-def org-defdecode))

    (when org-read-date-analyze-forced-year
      (message "Year was forced into %s"
	       (if org-read-date-force-compatible-dates
		   "compatible range (1970-2037)"
		 "range representable on this machine"))
      (ding))

    ;; One round trip to get rid of 34th of August and stuff like that....
    (setq final (decode-time (apply 'encode-time final)))

    (setq org-read-date-final-answer ans)

    (if to-time
	(apply 'encode-time final)
      (if (and (boundp 'org-time-was-given) org-time-was-given)
	  (format "%04d-%02d-%02d %02d:%02d"
		  (nth 5 final) (nth 4 final) (nth 3 final)
		  (nth 2 final) (nth 1 final))
	(format "%04d-%02d-%02d" (nth 5 final) (nth 4 final) (nth 3 final))))))

(defun org-read-date-display ()
  "Display the current date prompt interpretation in the minibuffer."
  (when org-read-date-display-live
    (when org-read-date-overlay
      (delete-overlay org-read-date-overlay))
    (when (minibufferp (current-buffer))
      (save-excursion
	(end-of-line 1)
	(while (not (equal (buffer-substring
			    (max (point-min) (- (point) 4)) (point))
			   "    "))
	  (insert " ")))
      (let* ((ans (concat (buffer-substring (point-at-bol) (point-max))
			  " " (or org-ans1 org-ans2)))
	     (org-end-time-was-given nil)
	     (f (org-read-date-analyze ans org-def org-defdecode))
	     (fmts (if org-dcst
		       org-time-stamp-custom-formats
		     org-time-stamp-formats))
	     (fmt (if (or org-with-time
			  (and (boundp 'org-time-was-given) org-time-was-given))
		      (cdr fmts)
		    (car fmts)))
	     (txt (format-time-string fmt (apply 'encode-time f)))
	     (txt (if org-read-date-inactive (concat "[" (substring txt 1 -1) "]") txt))
	     (txt (concat "=> " txt)))
	(when (and org-end-time-was-given
		   (string-match org-plain-time-of-day-regexp txt))
	  (setq txt (concat (substring txt 0 (match-end 0)) "-"
			    org-end-time-was-given
			    (substring txt (match-end 0)))))
	(when org-read-date-analyze-futurep
	  (setq txt (concat txt " (=>F)")))
	(setq org-read-date-overlay
	      (make-overlay (1- (point-at-eol)) (point-at-eol)))
	(org-overlay-display org-read-date-overlay txt 'secondary-selection)))))

(defun org-read-date-analyze (ans def defdecode)
  "Analyze the combined answer of the date prompt."
  ;; FIXME: cleanup and comment
  ;; Pass `current-time' result to `decode-time' (instead of calling
  ;; without arguments) so that only `current-time' has to be
  ;; overridden in tests.
  (let ((org-def def)
	(org-defdecode defdecode)
	(nowdecode (decode-time (current-time)))
	delta deltan deltaw deltadef year month day
	hour minute second wday pm h2 m2 tl wday1
	iso-year iso-weekday iso-week iso-date futurep kill-year)
    (setq org-read-date-analyze-futurep nil
	  org-read-date-analyze-forced-year nil)
    (when (string-match "\\`[ \t]*\\.[ \t]*\\'" ans)
      (setq ans "+0"))

    (when (setq delta (org-read-date-get-relative ans (current-time) org-def))
      (setq ans (replace-match "" t t ans)
	    deltan (car delta)
	    deltaw (nth 1 delta)
	    deltadef (nth 2 delta)))

    ;; Check if there is an iso week date in there.  If yes, store the
    ;; info and postpone interpreting it until the rest of the parsing
    ;; is done.
    (when (string-match "\\<\\(?:\\([0-9]+\\)-\\)?[wW]\\([0-9]\\{1,2\\}\\)\\(?:-\\([0-6]\\)\\)?\\([ \t]\\|$\\)" ans)
      (setq iso-year (when (match-end 1)
		       (org-small-year-to-year
			(string-to-number (match-string 1 ans))))
	    iso-weekday (when (match-end 3)
			  (string-to-number (match-string 3 ans)))
	    iso-week (string-to-number (match-string 2 ans)))
      (setq ans (replace-match "" t t ans)))

    ;; Help matching ISO dates with single digit month or day, like 2006-8-11.
    (when (string-match
	   "^ *\\(\\([0-9]+\\)-\\)?\\([0-1]?[0-9]\\)-\\([0-3]?[0-9]\\)\\([^-0-9]\\|$\\)" ans)
      (setq year (if (match-end 2)
		     (string-to-number (match-string 2 ans))
		   (progn (setq kill-year t)
			  (string-to-number (format-time-string "%Y"))))
	    month (string-to-number (match-string 3 ans))
	    day (string-to-number (match-string 4 ans)))
      (setq year (org-small-year-to-year year))
      (setq ans (replace-match (format "%04d-%02d-%02d\\5" year month day)
			       t nil ans)))

    ;; Help matching dotted european dates
    (when (string-match
	   "^ *\\(3[01]\\|0?[1-9]\\|[12][0-9]\\)\\. ?\\(0?[1-9]\\|1[012]\\)\\.\\( ?[1-9][0-9]\\{3\\}\\)?" ans)
      (setq year (if (match-end 3) (string-to-number (match-string 3 ans))
		   (setq kill-year t)
		   (string-to-number (format-time-string "%Y")))
	    day (string-to-number (match-string 1 ans))
	    month (string-to-number (match-string 2 ans))
	    ans (replace-match (format "%04d-%02d-%02d" year month day)
			       t nil ans)))

    ;; Help matching american dates, like 5/30 or 5/30/7
    (when (string-match
	   "^ *\\(0?[1-9]\\|1[012]\\)/\\(0?[1-9]\\|[12][0-9]\\|3[01]\\)\\(/\\([0-9]+\\)\\)?\\([^/0-9]\\|$\\)" ans)
      (setq year (if (match-end 4)
		     (string-to-number (match-string 4 ans))
		   (progn (setq kill-year t)
			  (string-to-number (format-time-string "%Y"))))
	    month (string-to-number (match-string 1 ans))
	    day (string-to-number (match-string 2 ans)))
      (setq year (org-small-year-to-year year))
      (setq ans (replace-match (format "%04d-%02d-%02d\\5" year month day)
			       t nil ans)))
    ;; Help matching am/pm times, because `parse-time-string' does not do that.
    ;; If there is a time with am/pm, and *no* time without it, we convert
    ;; so that matching will be successful.
    (cl-loop for i from 1 to 2 do	; twice, for end time as well
	     (when (and (not (string-match "\\(\\`\\|[^+]\\)[012]?[0-9]:[0-9][0-9]\\([ \t\n]\\|$\\)" ans))
			(string-match "\\([012]?[0-9]\\)\\(:\\([0-5][0-9]\\)\\)?\\(am\\|AM\\|pm\\|PM\\)\\>" ans))
	       (setq hour (string-to-number (match-string 1 ans))
		     minute (if (match-end 3)
				(string-to-number (match-string 3 ans))
			      0)
		     pm (equal ?p
			       (string-to-char (downcase (match-string 4 ans)))))
	       (if (and (= hour 12) (not pm))
		   (setq hour 0)
		 (when (and pm (< hour 12)) (setq hour (+ 12 hour))))
	       (setq ans (replace-match (format "%02d:%02d" hour minute)
					t t ans))))

    ;; Check if a time range is given as a duration
    (when (string-match "\\([012]?[0-9]\\):\\([0-6][0-9]\\)\\+\\([012]?[0-9]\\)\\(:\\([0-5][0-9]\\)\\)?" ans)
      (setq hour (string-to-number (match-string 1 ans))
	    h2 (+ hour (string-to-number (match-string 3 ans)))
	    minute (string-to-number (match-string 2 ans))
	    m2 (+ minute (if (match-end 5) (string-to-number
					    (match-string 5 ans))0)))
      (when (>= m2 60) (setq h2 (1+ h2) m2 (- m2 60)))
      (setq ans (replace-match (format "%02d:%02d-%02d:%02d" hour minute h2 m2)
			       t t ans)))

    ;; Check if there is a time range
    (when (boundp 'org-end-time-was-given)
      (setq org-time-was-given nil)
      (when (and (string-match org-plain-time-of-day-regexp ans)
		 (match-end 8))
	(setq org-end-time-was-given (match-string 8 ans))
	(setq ans (concat (substring ans 0 (match-beginning 7))
			  (substring ans (match-end 7))))))

    (setq tl (parse-time-string ans)
	  day (or (nth 3 tl) (nth 3 org-defdecode))
	  month
	  (cond ((nth 4 tl))
		((not org-read-date-prefer-future) (nth 4 org-defdecode))
		;; Day was specified.  Make sure DAY+MONTH
		;; combination happens in the future.
		((nth 3 tl)
		 (setq futurep t)
		 (if (< day (nth 3 nowdecode)) (1+ (nth 4 nowdecode))
		   (nth 4 nowdecode)))
		(t (nth 4 org-defdecode)))
	  year
	  (cond ((and (not kill-year) (nth 5 tl)))
		((not org-read-date-prefer-future) (nth 5 org-defdecode))
		;; Month was guessed in the future and is at least
		;; equal to NOWDECODE's.  Fix year accordingly.
		(futurep
		 (if (or (> month (nth 4 nowdecode))
			 (>= day (nth 3 nowdecode)))
		     (nth 5 nowdecode)
		   (1+ (nth 5 nowdecode))))
		;; Month was specified.  Make sure MONTH+YEAR
		;; combination happens in the future.
		((nth 4 tl)
		 (setq futurep t)
		 (cond ((> month (nth 4 nowdecode)) (nth 5 nowdecode))
		       ((< month (nth 4 nowdecode)) (1+ (nth 5 nowdecode)))
		       ((< day (nth 3 nowdecode)) (1+ (nth 5 nowdecode)))
		       (t (nth 5 nowdecode))))
		(t (nth 5 org-defdecode)))
	  hour (or (nth 2 tl) (nth 2 org-defdecode))
	  minute (or (nth 1 tl) (nth 1 org-defdecode))
	  second (or (nth 0 tl) 0)
	  wday (nth 6 tl))

    (when (and (eq org-read-date-prefer-future 'time)
	       (not (nth 3 tl)) (not (nth 4 tl)) (not (nth 5 tl))
	       (equal day (nth 3 nowdecode))
	       (equal month (nth 4 nowdecode))
	       (equal year (nth 5 nowdecode))
	       (nth 2 tl)
	       (or (< (nth 2 tl) (nth 2 nowdecode))
		   (and (= (nth 2 tl) (nth 2 nowdecode))
			(nth 1 tl)
			(< (nth 1 tl) (nth 1 nowdecode)))))
      (setq day (1+ day)
	    futurep t))

    ;; Special date definitions below
    (cond
     (iso-week
      ;; There was an iso week
      (require 'cal-iso)
      (setq futurep nil)
      (setq year (or iso-year year)
	    day (or iso-weekday wday 1)
	    wday nil ; to make sure that the trigger below does not match
	    iso-date (calendar-gregorian-from-absolute
		      (calendar-iso-to-absolute
		       (list iso-week day year))))
					; FIXME:  Should we also push ISO weeks into the future?
					;      (when (and org-read-date-prefer-future
					;		 (not iso-year)
					;		 (< (calendar-absolute-from-gregorian iso-date)
					;		    (time-to-days (current-time))))
					;	(setq year (1+ year)
					;	      iso-date (calendar-gregorian-from-absolute
					;			(calendar-iso-to-absolute
					;			 (list iso-week day year)))))
      (setq month (car iso-date)
	    year (nth 2 iso-date)
	    day (nth 1 iso-date)))
     (deltan
      (setq futurep nil)
      (unless deltadef
	;; Pass `current-time' result to `decode-time' (instead of
	;; calling without arguments) so that only `current-time' has
	;; to be overridden in tests.
	(let ((now (decode-time (current-time))))
	  (setq day (nth 3 now) month (nth 4 now) year (nth 5 now))))
      (cond ((member deltaw '("d" "")) (setq day (+ day deltan)))
	    ((equal deltaw "w") (setq day (+ day (* 7 deltan))))
	    ((equal deltaw "m") (setq month (+ month deltan)))
	    ((equal deltaw "y") (setq year (+ year deltan)))))
     ((and wday (not (nth 3 tl)))
      ;; Weekday was given, but no day, so pick that day in the week
      ;; on or after the derived date.
      (setq wday1 (nth 6 (decode-time (encode-time 0 0 0 day month year))))
      (unless (equal wday wday1)
	(setq day (+ day (% (- wday wday1 -7) 7))))))
    (when (and (boundp 'org-time-was-given)
	       (nth 2 tl))
      (setq org-time-was-given t))
    (when (< year 100) (setq year (+ 2000 year)))
    ;; Check of the date is representable
    (if org-read-date-force-compatible-dates
	(progn
	  (when (< year 1970)
	    (setq year 1970 org-read-date-analyze-forced-year t))
	  (when (> year 2037)
	    (setq year 2037 org-read-date-analyze-forced-year t)))
      (condition-case nil
	  (ignore (encode-time second minute hour day month year))
	(error
	 (setq year (nth 5 org-defdecode))
	 (setq org-read-date-analyze-forced-year t))))
    (setq org-read-date-analyze-futurep futurep)
    (list second minute hour day month year)))

(defvar parse-time-weekdays)
(defun org-read-date-get-relative (s today default)
  "Check string S for special relative date string.
TODAY and DEFAULT are internal times, for today and for a default.
Return shift list (N what def-flag)
WHAT       is \"d\", \"w\", \"m\", or \"y\" for day, week, month, year.
N          is the number of WHATs to shift.
DEF-FLAG   is t when a double ++ or -- indicates shift relative to
           the DEFAULT date rather than TODAY."
  (require 'parse-time)
  (when (and
	 (string-match
	  (concat
	   "\\`[ \t]*\\([-+]\\{0,2\\}\\)"
	   "\\([0-9]+\\)?"
	   "\\([hdwmy]\\|\\(" (mapconcat 'car parse-time-weekdays "\\|") "\\)\\)?"
	   "\\([ \t]\\|$\\)") s)
	 (or (> (match-end 1) (match-beginning 1)) (match-end 4)))
    (let* ((dir (if (> (match-end 1) (match-beginning 1))
		    (string-to-char (substring (match-string 1 s) -1))
		  ?+))
	   (rel (and (match-end 1) (= 2 (- (match-end 1) (match-beginning 1)))))
	   (n (if (match-end 2) (string-to-number (match-string 2 s)) 1))
	   (what (if (match-end 3) (match-string 3 s) "d"))
	   (wday1 (cdr (assoc (downcase what) parse-time-weekdays)))
	   (date (if rel default today))
	   (wday (nth 6 (decode-time date)))
	   delta)
      (if wday1
	  (progn
	    (setq delta (mod (+ 7 (- wday1 wday)) 7))
	    (when (= delta 0) (setq delta 7))
	    (when (= dir ?-)
	      (setq delta (- delta 7))
	      (when (= delta 0) (setq delta -7)))
	    (when (> n 1) (setq delta (+ delta (* (1- n) (if (= dir ?-) -7 7)))))
	    (list delta "d" rel))
	(list (* n (if (= dir ?-) -1 1)) what rel)))))

(defun org-order-calendar-date-args (arg1 arg2 arg3)
  "Turn a user-specified date into the internal representation.
The internal representation needed by the calendar is (month day year).
This is a wrapper to handle the brain-dead convention in calendar that
user function argument order change dependent on argument order."
  (pcase calendar-date-style
    (`american (list arg1 arg2 arg3))
    (`european (list arg2 arg1 arg3))
    (`iso (list arg2 arg3 arg1))))

(defun org-eval-in-calendar (form &optional keepdate)
  "Eval FORM in the calendar window and return to current window.
Unless KEEPDATE is non-nil, update `org-ans2' to the cursor date."
  (let ((sf (selected-frame))
	(sw (selected-window)))
    (select-window (get-buffer-window "*Calendar*" t))
    (eval form)
    (when (and (not keepdate) (calendar-cursor-to-date))
      (let* ((date (calendar-cursor-to-date))
	     (time (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
	(setq org-ans2 (format-time-string "%Y-%m-%d" time))))
    (move-overlay org-date-ovl (1- (point)) (1+ (point)) (current-buffer))
    (select-window sw)
    (select-frame-set-input-focus sf)))

(defun org-calendar-select ()
  "Return to `org-read-date' with the date currently selected.
This is used by `org-read-date' in a temporary keymap for the calendar buffer."
  (interactive)
  (when (calendar-cursor-to-date)
    (let* ((date (calendar-cursor-to-date))
	   (time (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
      (setq org-ans1 (format-time-string "%Y-%m-%d" time)))
    (when (active-minibuffer-window) (exit-minibuffer))))

(defun org-insert-time-stamp (time &optional with-hm inactive pre post extra)
  "Insert a date stamp for the date given by the internal TIME.
See `format-time-string' for the format of TIME.
WITH-HM means use the stamp format that includes the time of the day.
INACTIVE means use square brackets instead of angular ones, so that the
stamp will not contribute to the agenda.
PRE and POST are optional strings to be inserted before and after the
stamp.
The command returns the inserted time stamp."
  (let ((fmt (funcall (if with-hm 'cdr 'car) org-time-stamp-formats))
	stamp)
    (when inactive (setq fmt (concat "[" (substring fmt 1 -1) "]")))
    (insert-before-markers (or pre ""))
    (when (listp extra)
      (setq extra (car extra))
      (if (and (stringp extra)
	       (string-match "\\([0-9]+\\):\\([0-9]+\\)" extra))
	  (setq extra (format "-%02d:%02d"
			      (string-to-number (match-string 1 extra))
			      (string-to-number (match-string 2 extra))))
	(setq extra nil)))
    (when extra
      (setq fmt (concat (substring fmt 0 -1) extra (substring fmt -1))))
    (insert-before-markers (setq stamp (format-time-string fmt time)))
    (insert-before-markers (or post ""))
    (setq org-last-inserted-timestamp stamp)))

(defun org-toggle-time-stamp-overlays ()
  "Toggle the use of custom time stamp formats."
  (interactive)
  (setq org-display-custom-times (not org-display-custom-times))
  (unless org-display-custom-times
    (let ((p (point-min)) (bmp (buffer-modified-p)))
      (while (setq p (next-single-property-change p 'display))
	(when (and (get-text-property p 'display)
		   (eq (get-text-property p 'face) 'org-date))
	  (remove-text-properties
	   p (setq p (next-single-property-change p 'display))
	   '(display t))))
      (set-buffer-modified-p bmp)))
  (org-restart-font-lock)
  (setq org-table-may-need-update t)
  (if org-display-custom-times
      (message "Time stamps are overlaid with custom format")
    (message "Time stamp overlays removed")))

(defun org-display-custom-time (beg end)
  "Overlay modified time stamp format over timestamp between BEG and END."
  (let* ((ts (buffer-substring beg end))
	 t1 with-hm tf time str (off 0))
    (save-match-data
      (setq t1 (org-parse-time-string ts t))
      (when (string-match "\\(-[0-9]+:[0-9]+\\)?\\( [.+]?\\+[0-9]+[hdwmy]\\(/[0-9]+[hdwmy]\\)?\\)?\\'" ts)
	(setq off (- (match-end 0) (match-beginning 0)))))
    (setq end (- end off))
    (setq with-hm (and (nth 1 t1) (nth 2 t1))
	  tf (funcall (if with-hm 'cdr 'car) org-time-stamp-custom-formats)
	  time (org-fix-decoded-time t1)
	  str (org-add-props
		  (format-time-string
		   (substring tf 1 -1) (apply 'encode-time time))
		  nil 'mouse-face 'highlight))
    (put-text-property beg end 'display str)))

(defun org-fix-decoded-time (time)
  "Set 0 instead of nil for the first 6 elements of time.
Don't touch the rest."
  (let ((n 0))
    (mapcar (lambda (x) (if (< (setq n (1+ n)) 7) (or x 0) x)) time)))

(defun org-time-stamp-to-now (timestamp-string &optional seconds)
  "Difference between TIMESTAMP-STRING and now in days.
If SECONDS is non-nil, return the difference in seconds."
  (let ((fdiff (if seconds #'float-time #'time-to-days)))
    (- (funcall fdiff (org-time-string-to-time timestamp-string))
       (funcall fdiff (current-time)))))

(defun org-deadline-close-p (timestamp-string &optional ndays)
  "Is the time in TIMESTAMP-STRING close to the current date?"
  (setq ndays (or ndays (org-get-wdays timestamp-string)))
  (and (<= (org-time-stamp-to-now timestamp-string) ndays)
       (not (org-entry-is-done-p))))

(defun org-get-wdays (ts &optional delay zero-delay)
  "Get the deadline lead time appropriate for timestring TS.
When DELAY is non-nil, get the delay time for scheduled items
instead of the deadline lead time.  When ZERO-DELAY is non-nil
and `org-scheduled-delay-days' is 0, enforce 0 as the delay,
don't try to find the delay cookie in the scheduled timestamp."
  (let ((tv (if delay org-scheduled-delay-days
	      org-deadline-warning-days)))
    (cond
     ((or (and delay (< tv 0))
	  (and delay zero-delay (<= tv 0))
	  (and (not delay) (<= tv 0)))
      ;; Enforce this value no matter what
      (- tv))
     ((string-match "-\\([0-9]+\\)\\([hdwmy]\\)\\(\\'\\|>\\| \\)" ts)
      ;; lead time is specified.
      (floor (* (string-to-number (match-string 1 ts))
		(cdr (assoc (match-string 2 ts)
			    '(("d" . 1)    ("w" . 7)
			      ("m" . 30.4) ("y" . 365.25)
			      ("h" . 0.041667)))))))
     ;; go for the default.
     (t tv))))

(defun org-calendar-select-mouse (ev)
  "Return to `org-read-date' with the date currently selected.
This is used by `org-read-date' in a temporary keymap for the calendar buffer."
  (interactive "e")
  (mouse-set-point ev)
  (when (calendar-cursor-to-date)
    (let* ((date (calendar-cursor-to-date))
	   (time (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
      (setq org-ans1 (format-time-string "%Y-%m-%d" time)))
    (when (active-minibuffer-window) (exit-minibuffer))))

(defun org-check-deadlines (ndays)
  "Check if there are any deadlines due or past due.
A deadline is considered due if it happens within `org-deadline-warning-days'
days from today's date.  If the deadline appears in an entry marked DONE,
it is not shown.  A numeric prefix argument NDAYS can be used to test that
many days.  If the prefix is a raw `\\[universal-argument]', all deadlines \
are shown."
  (interactive "P")
  (let* ((org-warn-days
	  (cond
	   ((equal ndays '(4)) 100000)
	   (ndays (prefix-numeric-value ndays))
	   (t (abs org-deadline-warning-days))))
	 (case-fold-search nil)
	 (regexp (concat "\\<" org-deadline-string " *<\\([^>]+\\)>"))
	 (callback
	  (lambda () (org-deadline-close-p (match-string 1) org-warn-days))))
    (message "%d deadlines past-due or due within %d days"
	     (org-occur regexp nil callback)
	     org-warn-days)))

(defsubst org-re-timestamp (type)
  "Return a regexp for timestamp TYPE.
Allowed values for TYPE are:

        all: all timestamps
     active: only active timestamps (<...>)
   inactive: only inactive timestamps ([...])
  scheduled: only scheduled timestamps
   deadline: only deadline timestamps
     closed: only closed time-stamps

When TYPE is nil, fall back on returning a regexp that matches
both scheduled and deadline timestamps."
  (cl-case type
    (all org-ts-regexp-both)
    (active org-ts-regexp)
    (inactive org-ts-regexp-inactive)
    (scheduled org-scheduled-time-regexp)
    (deadline org-deadline-time-regexp)
    (closed org-closed-time-regexp)
    (otherwise
     (concat "\\<"
	     (regexp-opt (list org-deadline-string org-scheduled-string))
	     " *<\\([^>]+\\)>"))))

(defun org-check-before-date (d)
  "Check if there are deadlines or scheduled entries before date D."
  (interactive (list (org-read-date)))
  (let* ((case-fold-search nil)
	 (regexp (org-re-timestamp org-ts-type))
	 (ts-type org-ts-type)
	 (callback
	  (lambda ()
	    (let ((match (match-string 1)))
	      (and (if (memq ts-type '(active inactive all))
		       (eq (org-element-type (save-excursion
					       (backward-char)
					       (org-element-context)))
			   'timestamp)
		     (org-at-planning-p))
		   (time-less-p
		    (org-time-string-to-time match t)
		    (org-time-string-to-time d t)))))))
    (message "%d entries before %s"
	     (org-occur regexp nil callback)
	     d)))

(defun org-check-after-date (d)
  "Check if there are deadlines or scheduled entries after date D."
  (interactive (list (org-read-date)))
  (let* ((case-fold-search nil)
	 (regexp (org-re-timestamp org-ts-type))
	 (ts-type org-ts-type)
	 (callback
	  (lambda ()
	    (let ((match (match-string 1)))
	      (and (if (memq ts-type '(active inactive all))
		       (eq (org-element-type (save-excursion
					       (backward-char)
					       (org-element-context)))
			   'timestamp)
		     (org-at-planning-p))
		   (not (time-less-p
			 (org-time-string-to-time match t)
			 (org-time-string-to-time d t))))))))
    (message "%d entries after %s"
	     (org-occur regexp nil callback)
	     d)))

(defun org-check-dates-range (start-date end-date)
  "Check for deadlines/scheduled entries between START-DATE and END-DATE."
  (interactive (list (org-read-date nil nil nil "Range starts")
		     (org-read-date nil nil nil "Range end")))
  (let ((case-fold-search nil)
	(regexp (org-re-timestamp org-ts-type))
	(callback
	 (let ((type org-ts-type))
	   (lambda ()
	     (let ((match (match-string 1)))
	       (and
		(if (memq type '(active inactive all))
		    (eq (org-element-type (save-excursion
					    (backward-char)
					    (org-element-context)))
			'timestamp)
		  (org-at-planning-p))
		(not (time-less-p
		      (org-time-string-to-time match t)
		      (org-time-string-to-time start-date t)))
		(time-less-p
		 (org-time-string-to-time match t)
		 (org-time-string-to-time end-date t))))))))
    (message "%d entries between %s and %s"
	     (org-occur regexp nil callback) start-date end-date)))

(defun org-evaluate-time-range (&optional to-buffer)
  "Evaluate a time range by computing the difference between start and end.
Normally the result is just printed in the echo area, but with prefix arg
TO-BUFFER, the result is inserted just after the date stamp into the buffer.
If the time range is actually in a table, the result is inserted into the
next column.
For time difference computation, a year is assumed to be exactly 365
days in order to avoid rounding problems."
  (interactive "P")
  (or
   (org-clock-update-time-maybe)
   (save-excursion
     (unless (org-at-date-range-p t)
       (goto-char (point-at-bol))
       (re-search-forward org-tr-regexp-both (point-at-eol) t))
     (unless (org-at-date-range-p t)
       (user-error "Not at a time-stamp range, and none found in current line")))
   (let* ((ts1 (match-string 1))
	  (ts2 (match-string 2))
	  (havetime (or (> (length ts1) 15) (> (length ts2) 15)))
	  (match-end (match-end 0))
	  (time1 (org-time-string-to-time ts1))
	  (time2 (org-time-string-to-time ts2))
	  (t1 (float-time time1))
	  (t2 (float-time time2))
	  (diff (abs (- t2 t1)))
	  (negative (< (- t2 t1) 0))
	  ;; (ys (floor (* 365 24 60 60)))
	  (ds (* 24 60 60))
	  (hs (* 60 60))
	  (fy "%dy %dd %02d:%02d")
	  (fy1 "%dy %dd")
	  (fd "%dd %02d:%02d")
	  (fd1 "%dd")
	  (fh "%02d:%02d")
	  y d h m align)
     (if havetime
	 (setq ; y (floor (/ diff ys))  diff (mod diff ys)
	  y 0
	  d (floor (/ diff ds))  diff (mod diff ds)
	  h (floor (/ diff hs))  diff (mod diff hs)
	  m (floor (/ diff 60)))
       (setq ; y (floor (/ diff ys))  diff (mod diff ys)
	y 0
	d (floor (+ (/ diff ds) 0.5))
	h 0 m 0))
     (if (not to-buffer)
	 (message "%s" (org-make-tdiff-string y d h m))
       (if (org-at-table-p)
	   (progn
	     (goto-char match-end)
	     (setq align t)
	     (and (looking-at " *|") (goto-char (match-end 0))))
	 (goto-char match-end))
       (when (looking-at
	      "\\( *-? *[0-9]+y\\)?\\( *[0-9]+d\\)? *[0-9][0-9]:[0-9][0-9]")
	 (replace-match ""))
       (when negative (insert " -"))
       (if (> y 0) (insert " " (format (if havetime fy fy1) y d h m))
	 (if (> d 0) (insert " " (format (if havetime fd fd1) d h m))
	   (insert " " (format fh h m))))
       (when align (org-table-align))
       (message "Time difference inserted")))))

(defun org-make-tdiff-string (y d h m)
  (let ((fmt "")
	(l nil))
    (when (> y 0)
      (setq fmt (concat fmt "%d year" (if (> y 1) "s" "") " "))
      (push y l))
    (when (> d 0)
      (setq fmt (concat fmt "%d day"  (if (> d 1) "s" "") " "))
      (push d l))
    (when (> h 0)
      (setq fmt (concat fmt "%d hour" (if (> h 1) "s" "") " "))
      (push h l))
    (when (> m 0)
      (setq fmt (concat fmt "%d minute" (if (> m 1) "s" "") " "))
      (push m l))
    (apply 'format fmt (nreverse l))))

(defun org-time-string-to-time (s &optional zone)
  "Convert timestamp string S into internal time.
The optional ZONE is omitted or nil for Emacs local time, t for
Universal Time, ‘wall’ for system wall clock time, or a string as
in the TZ environment variable."
  (apply #'encode-time (org-parse-time-string s nil zone)))

(defun org-time-string-to-seconds (s &optional zone)
  "Convert a timestamp string S into a number of seconds.
The optional ZONE is omitted or nil for Emacs local time, t for
Universal Time, ‘wall’ for system wall clock time, or a string as
in the TZ environment variable."
  (float-time (org-time-string-to-time s zone)))

(org-define-error 'org-diary-sexp-no-match "Unable to match diary sexp")

(defun org-time-string-to-absolute (s &optional daynr prefer buffer pos)
  "Convert time stamp S to an absolute day number.

If DAYNR in non-nil, and there is a specifier for a cyclic time
stamp, get the closest date to DAYNR.  If PREFER is
`past' (respectively `future') return a date past (respectively
after) or equal to DAYNR.

POS is the location of time stamp S, as a buffer position in
BUFFER.

Diary sexp timestamps are matched against DAYNR, when non-nil.
If matching fails or DAYNR is nil, `org-diary-sexp-no-match' is
signaled."
  (cond
   ((string-match "\\`%%\\((.*)\\)" s)
    ;; Sexp timestamp: try to match DAYNR, if available, since we're
    ;; only able to match individual dates.  If it fails, raise an
    ;; error.
    (if (and daynr
	     (org-diary-sexp-entry
	      (match-string 1 s) "" (calendar-gregorian-from-absolute daynr)))
	daynr
      (signal 'org-diary-sexp-no-match (list s))))
   (daynr (org-closest-date s daynr prefer))
   (t (time-to-days
       (condition-case errdata
	   (apply #'encode-time (org-parse-time-string s))
	 (error (error "Bad timestamp `%s'%s\nError was: %s"
		       s
		       (if (not (and buffer pos)) ""
			 (format-message " at %d in buffer `%s'" pos buffer))
		       (cdr errdata))))))))

(defun org-days-to-iso-week (days)
  "Return the iso week number."
  (require 'cal-iso)
  (car (calendar-iso-from-absolute days)))

(defun org-small-year-to-year (year)
  "Convert 2-digit years into 4-digit years.
YEAR is expanded into one of the 30 next years, if possible, or
into a past one.  Any year larger than 99 is returned unchanged."
  (if (>= year 100) year
    (let* ((current (string-to-number (format-time-string "%Y" (current-time))))
	   (century (/ current 100))
	   (offset (- year (% current 100))))
      (cond ((> offset 30) (+ (* (1- century) 100) year))
	    ((> offset -70) (+ (* century 100) year))
	    (t (+ (* (1+ century) 100) year))))))

(defun org-time-from-absolute (d)
  "Return the time corresponding to date D.
D may be an absolute day number, or a calendar-type list (month day year)."
  (when (numberp d) (setq d (calendar-gregorian-from-absolute d)))
  (encode-time 0 0 0 (nth 1 d) (car d) (nth 2 d)))

(defvar org-agenda-current-date)
(defun org-calendar-holiday ()
  "List of holidays, for Diary display in Org mode."
  (require 'holidays)
  (let ((hl (calendar-check-holidays org-agenda-current-date)))
    (and hl (mapconcat #'identity hl "; "))))

(defun org-diary-sexp-entry (sexp entry d)
  "Process a SEXP diary ENTRY for date D."
  (require 'diary-lib)
  ;; `org-anniversary' and alike expect ENTRY and DATE to be bound
  ;; dynamically.
  (let* ((sexp `(let ((entry ,entry)
		      (date ',d))
		  ,(car (read-from-string sexp))))
	 (result (if calendar-debug-sexp (eval sexp)
		   (condition-case nil
		       (eval sexp)
		     (error
		      (beep)
		      (message "Bad sexp at line %d in %s: %s"
			       (org-current-line)
			       (buffer-file-name) sexp)
		      (sleep-for 2))))))
    (cond ((stringp result) (split-string result "; "))
	  ((and (consp result)
		(not (consp (cdr result)))
		(stringp (cdr result))) (cdr result))
	  ((and (consp result)
		(stringp (car result))) result)
	  (result entry))))

(defun org-diary-to-ical-string (frombuf)
  "Get iCalendar entries from diary entries in buffer FROMBUF.
This uses the icalendar.el library."
  (let* ((tmpdir temporary-file-directory)
	 (tmpfile (make-temp-name
		   (expand-file-name "orgics" tmpdir)))
	 buf rtn b e)
    (with-current-buffer frombuf
      (icalendar-export-region (point-min) (point-max) tmpfile)
      (setq buf (find-buffer-visiting tmpfile))
      (set-buffer buf)
      (goto-char (point-min))
      (when (re-search-forward "^BEGIN:VEVENT" nil t)
	(setq b (match-beginning 0)))
      (goto-char (point-max))
      (when (re-search-backward "^END:VEVENT" nil t)
	(setq e (match-end 0)))
      (setq rtn (if (and b e) (concat (buffer-substring b e) "\n") "")))
    (kill-buffer buf)
    (delete-file tmpfile)
    rtn))

(defun org-closest-date (start current prefer)
  "Return closest date to CURRENT starting from START.

CURRENT and START are both time stamps.

When PREFER is `past', return a date that is either CURRENT or
past.  When PREFER is `future', return a date that is either
CURRENT or future.

Only time stamps with a repeater are modified.  Any other time
stamp stay unchanged.  In any case, return value is an absolute
day number."
  (if (not (string-match "\\+\\([0-9]+\\)\\([hdwmy]\\)" start))
      ;; No repeater.  Do not shift time stamp.
      (time-to-days (apply #'encode-time (org-parse-time-string start)))
    (let ((value (string-to-number (match-string 1 start)))
	  (type (match-string 2 start)))
      (if (= 0 value)
	  ;; Repeater with a 0-value is considered as void.
	  (time-to-days (apply #'encode-time (org-parse-time-string start)))
	(let* ((base (org-date-to-gregorian start))
	       (target (org-date-to-gregorian current))
	       (sday (calendar-absolute-from-gregorian base))
	       (cday (calendar-absolute-from-gregorian target))
	       n1 n2)
	  ;; If START is already past CURRENT, just return START.
	  (if (<= cday sday) sday
	    ;; Compute closest date before (N1) and closest date past
	    ;; (N2) CURRENT.
	    (pcase type
	      ("h"
	       (let ((missing-hours
		      (mod (+ (- (* 24 (- cday sday))
				 (nth 2 (org-parse-time-string start)))
			      org-extend-today-until)
			   value)))
		 (setf n1 (if (= missing-hours 0) cday
			    (- cday (1+ (/ missing-hours 24)))))
		 (setf n2 (+ cday (/ (- value missing-hours) 24)))))
	      ((or "d" "w")
	       (let ((value (if (equal type "w") (* 7 value) value)))
		 (setf n1 (+ sday (* value (/ (- cday sday) value))))
		 (setf n2 (+ n1 value))))
	      ("m"
	       (let* ((add-months
		       (lambda (d n)
			 ;; Add N months to gregorian date D, i.e.,
			 ;; a list (MONTH DAY YEAR).  Return a valid
			 ;; gregorian date.
			 (let ((m (+ (nth 0 d) n)))
			   (list (mod m 12)
				 (nth 1 d)
				 (+ (/ m 12) (nth 2 d))))))
		      (months		; Complete months to TARGET.
		       (* (/ (+ (* 12 (- (nth 2 target) (nth 2 base)))
				(- (nth 0 target) (nth 0 base))
				;; If START's day is greater than
				;; TARGET's, remove incomplete month.
				(if (> (nth 1 target) (nth 1 base)) 0 -1))
			     value)
			  value))
		      (before (funcall add-months base months)))
		 (setf n1 (calendar-absolute-from-gregorian before))
		 (setf n2
		       (calendar-absolute-from-gregorian
			(funcall add-months before value)))))
	      (_
	       (let* ((d (nth 1 base))
		      (m (nth 0 base))
		      (y (nth 2 base))
		      (years		; Complete years to TARGET.
		       (* (/ (- (nth 2 target)
				y
				;; If START's month and day are
				;; greater than TARGET's, remove
				;; incomplete year.
				(if (or (> (nth 0 target) m)
					(and (= (nth 0 target) m)
					     (> (nth 1 target) d)))
				    0
				  1))
			     value)
			  value))
		      (before (list m d (+ y years))))
		 (setf n1 (calendar-absolute-from-gregorian before))
		 (setf n2 (calendar-absolute-from-gregorian
			   (list m d (+ (nth 2 before) value)))))))
	    ;; Handle PREFER parameter, if any.
	    (cond
	     ((eq prefer 'past)   (if (= cday n2) n2 n1))
	     ((eq prefer 'future) (if (= cday n1) n1 n2))
	     (t (if (> (abs (- cday n1)) (abs (- cday n2))) n2 n1)))))))))

(defun org-date-to-gregorian (d)
  "Turn any specification of date D into a Gregorian date for the calendar."
  (cond ((integerp d) (calendar-gregorian-from-absolute d))
	((and (listp d) (= (length d) 3)) d)
	((stringp d)
	 (let ((d (org-parse-time-string d)))
	   (list (nth 4 d) (nth 3 d) (nth 5 d))))
	((listp d) (list (nth 4 d) (nth 3 d) (nth 5 d)))))

(defun org-parse-time-string (s &optional nodefault zone)
  "Parse the standard Org time string.

This should be a lot faster than the normal `parse-time-string'.

If time is not given, defaults to 0:00.  However, with optional
NODEFAULT, hour and minute fields will be nil if not given.

The optional ZONE is omitted or nil for Emacs local time, t for
Universal Time, ‘wall’ for system wall clock time, or a string as
in the TZ environment variable."
  (cond ((string-match org-ts-regexp0 s)
	 (list 0
	       (when (or (match-beginning 8) (not nodefault))
		 (string-to-number (or (match-string 8 s) "0")))
	       (when (or (match-beginning 7) (not nodefault))
		 (string-to-number (or (match-string 7 s) "0")))
	       (string-to-number (match-string 4 s))
	       (string-to-number (match-string 3 s))
	       (string-to-number (match-string 2 s))
	       nil nil zone))
	((string-match "^<[^>]+>$" s)
	 ;; FIXME: `decode-time' needs to be called with ZONE as its
	 ;; second argument.  However, this requires at least Emacs
	 ;; 25.1.  We can do it when we switch to this version as our
	 ;; minimal requirement.
	 (decode-time (seconds-to-time (org-matcher-time s))))
	(t (error "Not a standard Org time string: %s" s))))

(defun org-timestamp-up (&optional arg)
  "Increase the date item at the cursor by one.
If the cursor is on the year, change the year.  If it is on the month,
the day or the time, change that.
With prefix ARG, change by that many units."
  (interactive "p")
  (org-timestamp-change (prefix-numeric-value arg) nil 'updown))

(defun org-timestamp-down (&optional arg)
  "Decrease the date item at the cursor by one.
If the cursor is on the year, change the year.  If it is on the month,
the day or the time, change that.
With prefix ARG, change by that many units."
  (interactive "p")
  (org-timestamp-change (- (prefix-numeric-value arg)) nil 'updown))

(defun org-timestamp-up-day (&optional arg)
  "Increase the date in the time stamp by one day.
With prefix ARG, change that many days."
  (interactive "p")
  (if (and (not (org-at-timestamp-p 'lax))
	   (org-at-heading-p))
      (org-todo 'up)
    (org-timestamp-change (prefix-numeric-value arg) 'day 'updown)))

(defun org-timestamp-down-day (&optional arg)
  "Decrease the date in the time stamp by one day.
With prefix ARG, change that many days."
  (interactive "p")
  (if (and (not (org-at-timestamp-p 'lax))
	   (org-at-heading-p))
      (org-todo 'down)
    (org-timestamp-change (- (prefix-numeric-value arg)) 'day) 'updown))

(defun org-at-timestamp-p (&optional extended)
  "Non-nil if point is inside a timestamp.

By default, the function only consider syntactically valid active
timestamps.  However, the caller may have a broader definition
for timestamps.  As a consequence, optional argument EXTENDED can
be set to the following values

  `inactive'

    Include also syntactically valid inactive timestamps.

  `agenda'

    Include timestamps allowed in Agenda, i.e., those in
    properties drawers, planning lines and clock lines.

  `lax'

    Ignore context.  The function matches any part of the
    document looking like a timestamp.  This includes comments,
    example blocks...

For backward-compatibility with Org 9.0, every other non-nil
value is equivalent to `inactive'.

When at a timestamp, return the position of the point as a symbol
among `bracket', `after', `year', `month', `hour', `minute',
`day' or a number of character from the last know part of the
time stamp.

When matching, the match groups are the following:
  group 1: year
  group 2: month
  group 3: day number
  group 4: day name
  group 5: hours, if any
  group 6: minutes, if any"
  (let* ((regexp (if extended org-ts-regexp3 org-ts-regexp2))
	 (pos (point))
	 (match?
	  (let ((boundaries (org-in-regexp regexp)))
	    (save-match-data
	      (cond ((null boundaries) nil)
		    ((eq extended 'lax) t)
		    (t
		     (or (and (eq extended 'agenda)
			      (or (org-at-planning-p)
				  (org-at-property-p)
				  (and (bound-and-true-p
					org-agenda-include-inactive-timestamps)
				       (org-at-clock-log-p))))
			 (eq 'timestamp
			     (save-excursion
			       (when (= pos (cdr boundaries)) (forward-char -1))
			       (org-element-type (org-element-context)))))))))))
    (cond
     ((not match?)                        nil)
     ((= pos (match-beginning 0))         'bracket)
     ;; Distinguish location right before the closing bracket from
     ;; right after it.
     ((= pos (1- (match-end 0)))          'bracket)
     ((= pos (match-end 0))               'after)
     ((org-pos-in-match-range pos 2)      'year)
     ((org-pos-in-match-range pos 3)      'month)
     ((org-pos-in-match-range pos 7)      'hour)
     ((org-pos-in-match-range pos 8)      'minute)
     ((or (org-pos-in-match-range pos 4)
	  (org-pos-in-match-range pos 5)) 'day)
     ((and (> pos (or (match-end 8) (match-end 5)))
	   (< pos (match-end 0)))
      (- pos (or (match-end 8) (match-end 5))))
     (t                                   'day))))

(defun org-toggle-timestamp-type ()
  "Toggle the type (<active> or [inactive]) of a time stamp."
  (interactive)
  (when (org-at-timestamp-p 'lax)
    (let ((beg (match-beginning 0)) (end (match-end 0))
	  (map '((?\[ . "<") (?\] . ">") (?< . "[") (?> . "]"))))
      (save-excursion
	(goto-char beg)
	(while (re-search-forward "[][<>]" end t)
	  (replace-match (cdr (assoc (char-after (match-beginning 0)) map))
			 t t)))
      (message "Timestamp is now %sactive"
	       (if (equal (char-after beg) ?<) "" "in")))))

(defun org-at-clock-log-p ()
  "Non-nil if point is on a clock log line."
  (and (org-match-line org-clock-line-re)
       (eq (org-element-type (save-match-data (org-element-at-point))) 'clock)))

(defvar org-clock-history)                     ; defined in org-clock.el
(defvar org-clock-adjust-closest nil)          ; defined in org-clock.el
(defun org-timestamp-change (n &optional what updown suppress-tmp-delay)
  "Change the date in the time stamp at point.
The date will be changed by N times WHAT.  WHAT can be `day', `month',
`year', `minute', `second'.  If WHAT is not given, the cursor position
in the timestamp determines what will be changed.
When SUPPRESS-TMP-DELAY is non-nil, suppress delays like \"--2d\"."
  (let ((origin (point))
	(timestamp? (org-at-timestamp-p 'lax))
	origin-cat
	with-hm inactive
	(dm (max (nth 1 org-time-stamp-rounding-minutes) 1))
	extra rem
	ts time time0 fixnext clrgx)
    (unless timestamp? (user-error "Not at a timestamp"))
    (if (and (not what) (eq timestamp? 'bracket))
	(org-toggle-timestamp-type)
      ;; Point isn't on brackets.  Remember the part of the time-stamp
      ;; the point was in.  Indeed, size of time-stamps may change,
      ;; but point must be kept in the same category nonetheless.
      (setq origin-cat timestamp?)
      (when (and (not what) (not (eq timestamp? 'day))
		 org-display-custom-times
		 (get-text-property (point) 'display)
		 (not (get-text-property (1- (point)) 'display)))
	(setq timestamp? 'day))
      (setq timestamp? (or what timestamp?)
	    inactive (= (char-after (match-beginning 0)) ?\[)
	    ts (match-string 0))
      (replace-match "")
      (when (string-match
	     "\\(\\(-[012][0-9]:[0-5][0-9]\\)?\\( +[.+]?-?[-+][0-9]+[hdwmy]\\(/[0-9]+[hdwmy]\\)?\\)*\\)[]>]"
	     ts)
	(setq extra (match-string 1 ts))
	(when suppress-tmp-delay
	  (setq extra (replace-regexp-in-string " --[0-9]+[hdwmy]" "" extra))))
      (when (string-match "^.\\{10\\}.*?[0-9]+:[0-9][0-9]" ts)
	(setq with-hm t))
      (setq time0 (org-parse-time-string ts))
      (when (and updown
		 (eq timestamp? 'minute)
		 (not current-prefix-arg))
	;; This looks like s-up and s-down.  Change by one rounding step.
	(setq n (* dm (cond ((> n 0) 1) ((< n 0) -1) (t 0))))
	(unless (= 0 (setq rem (% (nth 1 time0) dm)))
	  (setcar (cdr time0) (+ (nth 1 time0)
				 (if (> n 0) (- rem) (- dm rem))))))
      (setq time
	    (apply #'encode-time
		   (or (car time0) 0)
		   (+ (if (eq timestamp? 'minute) n 0) (nth 1 time0))
		   (+ (if (eq timestamp? 'hour) n 0)   (nth 2 time0))
		   (+ (if (eq timestamp? 'day) n 0)    (nth 3 time0))
		   (+ (if (eq timestamp? 'month) n 0)  (nth 4 time0))
		   (+ (if (eq timestamp? 'year) n 0)   (nth 5 time0))
		   (nthcdr 6 time0)))
      (when (and (memq timestamp? '(hour minute))
		 extra
		 (string-match "-\\([012][0-9]\\):\\([0-5][0-9]\\)" extra))
	(setq extra (org-modify-ts-extra
		     extra
		     (if (eq timestamp? 'hour) 2 5)
		     n dm)))
      (when (integerp timestamp?)
	(setq extra (org-modify-ts-extra extra timestamp? n dm)))
      (when (eq what 'calendar)
	(let ((cal-date (org-get-date-from-calendar)))
	  (setcar (nthcdr 4 time0) (nth 0 cal-date)) ; month
	  (setcar (nthcdr 3 time0) (nth 1 cal-date)) ; day
	  (setcar (nthcdr 5 time0) (nth 2 cal-date)) ; year
	  (setcar time0 (or (car time0) 0))
	  (setcar (nthcdr 1 time0) (or (nth 1 time0) 0))
	  (setcar (nthcdr 2 time0) (or (nth 2 time0) 0))
	  (setq time (apply 'encode-time time0))))
      ;; Insert the new time-stamp, and ensure point stays in the same
      ;; category as before (i.e. not after the last position in that
      ;; category).
      (let ((pos (point)))
	;; Stay before inserted string. `save-excursion' is of no use.
	(setq org-last-changed-timestamp
	      (org-insert-time-stamp time with-hm inactive nil nil extra))
	(goto-char pos))
      (save-match-data
	(looking-at org-ts-regexp3)
	(goto-char
	 (pcase origin-cat
	   ;; `day' category ends before `hour' if any, or at the end
	   ;; of the day name.
	   (`day (min (or (match-beginning 7) (1- (match-end 5))) origin))
	   (`hour (min (match-end 7) origin))
	   (`minute (min (1- (match-end 8)) origin))
	   ((pred integerp) (min (1- (match-end 0)) origin))
	   ;; Point was right after the time-stamp.  However, the
	   ;; time-stamp length might have changed, so refer to
	   ;; (match-end 0) instead.
	   (`after (match-end 0))
	   ;; `year' and `month' have both fixed size: point couldn't
	   ;; have moved into another part.
	   (_ origin))))
      ;; Update clock if on a CLOCK line.
      (org-clock-update-time-maybe)
      ;; Maybe adjust the closest clock in `org-clock-history'
      (when org-clock-adjust-closest
	(if (not (and (org-at-clock-log-p)
		      (< 1 (length (delq nil (mapcar 'marker-position
						     org-clock-history))))))
	    (message "No clock to adjust")
	  (cond ((save-excursion	; fix previous clock?
		   (re-search-backward org-ts-regexp0 nil t)
		   (looking-back (concat org-clock-string " \\[")
				 (line-beginning-position)))
		 (setq fixnext 1 clrgx (concat org-ts-regexp0 "\\] =>.*$")))
		((save-excursion	; fix next clock?
		   (re-search-backward org-ts-regexp0 nil t)
		   (looking-at (concat org-ts-regexp0 "\\] =>")))
		 (setq fixnext -1 clrgx (concat org-clock-string " \\[" org-ts-regexp0))))
	  (save-window-excursion
	    ;; Find closest clock to point, adjust the previous/next one in history
	    (let* ((p (save-excursion (org-back-to-heading t)))
		   (cl (mapcar (lambda(c) (abs (- (marker-position c) p))) org-clock-history))
		   (clfixnth
		    (+ fixnext (- (length cl) (or (length (member (apply 'min cl) cl)) 100))))
		   (clfixpos (unless (> 0 clfixnth) (nth clfixnth org-clock-history))))
	      (if (not clfixpos)
		  (message "No clock to adjust")
		(save-excursion
		  (org-goto-marker-or-bmk clfixpos)
		  (org-show-subtree)
		  (when (re-search-forward clrgx nil t)
		    (goto-char (match-beginning 1))
		    (let (org-clock-adjust-closest)
		      (org-timestamp-change n timestamp? updown))
		    (message "Clock adjusted in %s for heading: %s"
			     (file-name-nondirectory (buffer-file-name))
			     (org-get-heading t t)))))))))
      ;; Try to recenter the calendar window, if any.
      (when (and org-calendar-follow-timestamp-change
		 (get-buffer-window "*Calendar*" t)
		 (memq timestamp? '(day month year)))
	(org-recenter-calendar (time-to-days time))))))

(defun org-modify-ts-extra (s pos n dm)
  "Change the different parts of the lead-time and repeat fields in timestamp."
  (let ((idx '(("d" . 0) ("w" . 1) ("m" . 2) ("y" . 3) ("d" . -1) ("y" . 4)))
	ng h m new rem)
    (when (string-match "\\(-\\([012][0-9]\\):\\([0-5][0-9]\\)\\)?\\( +\\+\\([0-9]+\\)\\([dmwy]\\)\\)?\\( +-\\([0-9]+\\)\\([dmwy]\\)\\)?" s)
      (cond
       ((or (org-pos-in-match-range pos 2)
	    (org-pos-in-match-range pos 3))
	(setq m (string-to-number (match-string 3 s))
	      h (string-to-number (match-string 2 s)))
	(if (org-pos-in-match-range pos 2)
	    (setq h (+ h n))
	  (setq n (* dm (with-no-warnings (signum n))))
	  (unless (= 0 (setq rem (% m dm)))
	    (setq m (+ m (if (> n 0) (- rem) (- dm rem)))))
	  (setq m (+ m n)))
	(when (< m 0) (setq m (+ m 60) h (1- h)))
	(when (> m 59) (setq m (- m 60) h (1+ h)))
	(setq h (mod h 24))
	(setq ng 1 new (format "-%02d:%02d" h m)))
       ((org-pos-in-match-range pos 6)
	(setq ng 6 new (car (rassoc (+ n (cdr (assoc (match-string 6 s) idx))) idx))))
       ((org-pos-in-match-range pos 5)
	(setq ng 5 new (format "%d" (max 1 (+ n (string-to-number (match-string 5 s)))))))

       ((org-pos-in-match-range pos 9)
	(setq ng 9 new (car (rassoc (+ n (cdr (assoc (match-string 9 s) idx))) idx))))
       ((org-pos-in-match-range pos 8)
	(setq ng 8 new (format "%d" (max 0 (+ n (string-to-number (match-string 8 s))))))))

      (when ng
	(setq s (concat
		 (substring s 0 (match-beginning ng))
		 new
		 (substring s (match-end ng))))))
    s))

(defun org-recenter-calendar (d)
  "If the calendar is visible, recenter it to date D."
  (let ((cwin (get-buffer-window "*Calendar*" t)))
    (when cwin
      (let ((calendar-move-hook nil))
	(with-selected-window cwin
	  (calendar-goto-date
	   (if (listp d) d (calendar-gregorian-from-absolute d))))))))

(defun org-goto-calendar (&optional arg)
  "Go to the Emacs calendar at the current date.
If there is a time stamp in the current line, go to that date.
A prefix ARG can be used to force the current date."
  (interactive "P")
  (let ((calendar-move-hook nil)
	(calendar-view-holidays-initially-flag nil)
	(calendar-view-diary-initially-flag nil)
	diff)
    (when (or (org-at-timestamp-p 'lax)
	      (org-match-line (concat ".*" org-ts-regexp)))
      (let ((d1 (time-to-days (current-time)))
	    (d2 (time-to-days (org-time-string-to-time (match-string 1)))))
	(setq diff (- d2 d1))))
    (calendar)
    (calendar-goto-today)
    (when (and diff (not arg)) (calendar-forward-day diff))))

(defun org-get-date-from-calendar ()
  "Return a list (month day year) of date at point in calendar."
  (with-current-buffer "*Calendar*"
    (save-match-data
      (calendar-cursor-to-date))))

(defun org-date-from-calendar ()
  "Insert time stamp corresponding to cursor date in *Calendar* buffer.
If there is already a time stamp at the cursor position, update it."
  (interactive)
  (if (org-at-timestamp-p 'lax)
      (org-timestamp-change 0 'calendar)
    (let ((cal-date (org-get-date-from-calendar)))
      (org-insert-time-stamp
       (encode-time 0 0 0 (nth 1 cal-date) (car cal-date) (nth 2 cal-date))))))

(defcustom org-effort-durations
  `(("min" . 1)
    ("h" . 60)
    ("d" . ,(* 60 8))
    ("w" . ,(* 60 8 5))
    ("m" . ,(* 60 8 5 4))
    ("y" . ,(* 60 8 5 40)))
  "Conversion factor to minutes for an effort modifier.

Each entry has the form (MODIFIER . MINUTES).

In an effort string, a number followed by MODIFIER is multiplied
by the specified number of MINUTES to obtain an effort in
minutes.

For example, if the value of this variable is ((\"hours\" . 60)), then an
effort string \"2hours\" is equivalent to 120 minutes."
  :group 'org-agenda
  :version "26.1"
  :package-version '(Org . "8.3")
  :type '(alist :key-type (string :tag "Modifier")
		:value-type (number :tag "Minutes")))

(defcustom org-image-actual-width t
  "Should we use the actual width of images when inlining them?

When set to t, always use the image width.

When set to a number, use imagemagick (when available) to set
the image's width to this value.

When set to a number in a list, try to get the width from any
#+ATTR.* keyword if it matches a width specification like

  #+ATTR_HTML: :width 300px

and fall back on that number if none is found.

When set to nil, try to get the width from an #+ATTR.* keyword
and fall back on the original width if none is found.

This requires Emacs >= 24.1, build with imagemagick support."
  :group 'org-appearance
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Use the image width" t)
	  (integer :tag "Use a number of pixels")
	  (list :tag "Use #+ATTR* or a number of pixels" (integer))
	  (const :tag "Use #+ATTR* or don't resize" nil)))

(defcustom org-agenda-inhibit-startup nil
  "Inhibit startup when preparing agenda buffers.
When this variable is t, the initialization of the Org agenda
buffers is inhibited: e.g. the visibility state is not set, the
tables are not re-aligned, etc."
  :type 'boolean
  :version "24.3"
  :group 'org-agenda)

(defcustom org-agenda-ignore-properties nil
  "Avoid updating text properties when building the agenda.
Properties are used to prepare buffers for effort estimates,
appointments, statistics and subtree-local categories.
If you don't use these in the agenda, you can add them to this
list and agenda building will be a bit faster.
The value is a list, with zero or more of the symbols `effort', `appt',
`stats' or `category'."
  :type '(set :greedy t
	      (const effort)
	      (const appt)
	      (const stats)
	      (const category))
  :version "26.1"
  :package-version '(Org . "8.3")
  :group 'org-agenda)

;;;; Files

(defun org-save-all-org-buffers ()
  "Save all Org buffers without user confirmation."
  (interactive)
  (message "Saving all Org buffers...")
  (save-some-buffers t (lambda () (derived-mode-p 'org-mode)))
  (when (featurep 'org-id) (org-id-locations-save))
  (message "Saving all Org buffers... done"))

(defun org-revert-all-org-buffers ()
  "Revert all Org buffers.
Prompt for confirmation when there are unsaved changes.
Be sure you know what you are doing before letting this function
overwrite your changes.

This function is useful in a setup where one tracks org files
with a version control system, to revert on one machine after pulling
changes from another.  I believe the procedure must be like this:

1. M-x org-save-all-org-buffers
2. Pull changes from the other machine, resolve conflicts
3. M-x org-revert-all-org-buffers"
  (interactive)
  (unless (yes-or-no-p "Revert all Org buffers from their files? ")
    (user-error "Abort"))
  (save-excursion
    (save-window-excursion
      (dolist (b (buffer-list))
	(when (and (with-current-buffer b (derived-mode-p 'org-mode))
		   (with-current-buffer b buffer-file-name))
	  (pop-to-buffer-same-window b)
	  (revert-buffer t 'no-confirm)))
      (when (and (featurep 'org-id) org-id-track-globally)
	(org-id-locations-load)))))

;;;; Agenda files

;;;###autoload
(defun org-switchb (&optional arg)
  "Switch between Org buffers.

With `\\[universal-argument]' prefix, restrict available buffers to files.

With `\\[universal-argument] \\[universal-argument]' \
prefix, restrict available buffers to agenda files."
  (interactive "P")
  (let ((blist (org-buffer-list
		(cond ((equal arg '(4))  'files)
		      ((equal arg '(16)) 'agenda)))))
    (pop-to-buffer-same-window
     (completing-read "Org buffer: "
		      (mapcar #'list (mapcar #'buffer-name blist))
		      nil t))))

(defun org-buffer-list (&optional predicate exclude-tmp)
  "Return a list of Org buffers.
PREDICATE can be `export', `files' or `agenda'.

export   restrict the list to Export buffers.
files    restrict the list to buffers visiting Org files.
agenda   restrict the list to buffers visiting agenda files.

If EXCLUDE-TMP is non-nil, ignore temporary buffers."
  (let* ((bfn nil)
	 (agenda-files (and (eq predicate 'agenda)
			    (mapcar 'file-truename (org-agenda-files t))))
	 (filter
	  (cond
	   ((eq predicate 'files)
	    (lambda (b) (with-current-buffer b (derived-mode-p 'org-mode))))
	   ((eq predicate 'export)
	    (lambda (b) (string-match "\\*Org .*Export" (buffer-name b))))
	   ((eq predicate 'agenda)
	    (lambda (b)
	      (with-current-buffer b
		(and (derived-mode-p 'org-mode)
		     (setq bfn (buffer-file-name b))
		     (member (file-truename bfn) agenda-files)))))
	   (t (lambda (b) (with-current-buffer b
			    (or (derived-mode-p 'org-mode)
				(string-match "\\*Org .*Export"
					      (buffer-name b)))))))))
    (delq nil
	  (mapcar
	   (lambda(b)
	     (if (and (funcall filter b)
		      (or (not exclude-tmp)
			  (not (string-match "tmp" (buffer-name b)))))
		 b
	       nil))
	   (buffer-list)))))

(defun org-agenda-files (&optional unrestricted archives)
  "Get the list of agenda files.
Optional UNRESTRICTED means return the full list even if a restriction
is currently in place.
When ARCHIVES is t, include all archive files that are really being
used by the agenda files.  If ARCHIVE is `ifmode', do this only if
`org-agenda-archives-mode' is t."
  (let ((files
	 (cond
	  ((and (not unrestricted) (get 'org-agenda-files 'org-restrict)))
	  ((stringp org-agenda-files) (org-read-agenda-file-list))
	  ((listp org-agenda-files) org-agenda-files)
	  (t (error "Invalid value of `org-agenda-files'")))))
    (setq files (apply 'append
		       (mapcar (lambda (f)
				 (if (file-directory-p f)
				     (directory-files
				      f t org-agenda-file-regexp)
				   (list f)))
			       files)))
    (when org-agenda-skip-unavailable-files
      (setq files (delq nil
			(mapcar (function
				 (lambda (file)
				   (and (file-readable-p file) file)))
				files))))
    (when (or (eq archives t)
	      (and (eq archives 'ifmode) (eq org-agenda-archives-mode t)))
      (setq files (org-add-archive-files files)))
    files))

(defun org-agenda-file-p (&optional file)
  "Return non-nil, if FILE is an agenda file.
If FILE is omitted, use the file associated with the current
buffer."
  (let ((fname (or file (buffer-file-name))))
    (and fname
         (member (file-truename fname)
                 (mapcar #'file-truename (org-agenda-files t))))))

(defun org-edit-agenda-file-list ()
  "Edit the list of agenda files.
Depending on setup, this either uses customize to edit the variable
`org-agenda-files', or it visits the file that is holding the list.  In the
latter case, the buffer is set up in a way that saving it automatically kills
the buffer and restores the previous window configuration."
  (interactive)
  (if (stringp org-agenda-files)
      (let ((cw (current-window-configuration)))
	(find-file org-agenda-files)
	(setq-local org-window-configuration cw)
	(add-hook 'after-save-hook
		  (lambda ()
		    (set-window-configuration
		     (prog1 org-window-configuration
		       (kill-buffer (current-buffer))))
		    (org-install-agenda-files-menu)
		    (message "New agenda file list installed"))
		  nil 'local)
	(message "%s" (substitute-command-keys
		       "Edit list and finish with \\[save-buffer]")))
    (customize-variable 'org-agenda-files)))

(defun org-store-new-agenda-file-list (list)
  "Set new value for the agenda file list and save it correctly."
  (if (stringp org-agenda-files)
      (let ((fe (org-read-agenda-file-list t)) b u)
	(while (setq b (find-buffer-visiting org-agenda-files))
	  (kill-buffer b))
	(with-temp-file org-agenda-files
	  (insert
	   (mapconcat
	    (lambda (f) ;; Keep un-expanded entries.
	      (if (setq u (assoc f fe))
		  (cdr u)
		f))
	    list "\n")
	   "\n")))
    (let ((org-mode-hook nil) (org-inhibit-startup t)
	  (org-insert-mode-line-in-empty-file nil))
      (setq org-agenda-files list)
      (customize-save-variable 'org-agenda-files org-agenda-files))))

(defun org-read-agenda-file-list (&optional pair-with-expansion)
  "Read the list of agenda files from a file.
If PAIR-WITH-EXPANSION is t return pairs with un-expanded
filenames, used by `org-store-new-agenda-file-list' to write back
un-expanded file names."
  (when (file-directory-p org-agenda-files)
    (error "`org-agenda-files' cannot be a single directory"))
  (when (stringp org-agenda-files)
    (with-temp-buffer
      (insert-file-contents org-agenda-files)
      (mapcar
       (lambda (f)
	 (let ((e (expand-file-name (substitute-in-file-name f)
				    org-directory)))
	   (if pair-with-expansion
	       (cons e f)
	     e)))
       (org-split-string (buffer-string) "[ \t\r\n]*?[\r\n][ \t\r\n]*")))))

;;;###autoload
(defun org-cycle-agenda-files ()
  "Cycle through the files in `org-agenda-files'.
If the current buffer visits an agenda file, find the next one in the list.
If the current buffer does not, find the first agenda file."
  (interactive)
  (let* ((fs (or (org-agenda-files t)
		 (user-error "No agenda files")))
	 (files (copy-sequence fs))
	 (tcf (and buffer-file-name (file-truename buffer-file-name)))
	 file)
    (when tcf
      (while (and (setq file (pop files))
		  (not (equal (file-truename file) tcf)))))
    (find-file (car (or files fs)))
    (when (buffer-base-buffer) (pop-to-buffer-same-window (buffer-base-buffer)))))

(defun org-agenda-file-to-front (&optional to-end)
  "Move/add the current file to the top of the agenda file list.
If the file is not present in the list, it is added to the front.  If it is
present, it is moved there.  With optional argument TO-END, add/move to the
end of the list."
  (interactive "P")
  (let ((org-agenda-skip-unavailable-files nil)
	(file-alist (mapcar (lambda (x)
			      (cons (file-truename x) x))
			    (org-agenda-files t)))
	(ctf (file-truename
	      (or buffer-file-name
		  (user-error "Please save the current buffer to a file"))))
	x had)
    (setq x (assoc ctf file-alist) had x)

    (unless x (setq x (cons ctf (abbreviate-file-name buffer-file-name))))
    (if to-end
	(setq file-alist (append (delq x file-alist) (list x)))
      (setq file-alist (cons x (delq x file-alist))))
    (org-store-new-agenda-file-list (mapcar 'cdr file-alist))
    (org-install-agenda-files-menu)
    (message "File %s to %s of agenda file list"
	     (if had "moved" "added") (if to-end "end" "front"))))

(defun org-remove-file (&optional file)
  "Remove current file from the list of files in variable `org-agenda-files'.
These are the files which are being checked for agenda entries.
Optional argument FILE means use this file instead of the current."
  (interactive)
  (let* ((org-agenda-skip-unavailable-files nil)
	 (file (or file buffer-file-name
		   (user-error "Current buffer does not visit a file")))
	 (true-file (file-truename file))
	 (afile (abbreviate-file-name file))
	 (files (delq nil (mapcar
			   (lambda (x)
			     (unless (equal true-file
					    (file-truename x))
			       x))
			   (org-agenda-files t)))))
    (if (not (= (length files) (length (org-agenda-files t))))
	(progn
	  (org-store-new-agenda-file-list files)
	  (org-install-agenda-files-menu)
	  (message "Removed from Org Agenda list: %s" afile))
      (message "File was not in list: %s (not removed)" afile))))

(defun org-file-menu-entry (file)
  (vector file (list 'find-file file) t))

(defun org-check-agenda-file (file)
  "Make sure FILE exists.  If not, ask user what to do."
  (unless (file-exists-p file)
    (message "Non-existent agenda file %s.  [R]emove from list or [A]bort?"
	     (abbreviate-file-name file))
    (let ((r (downcase (read-char-exclusive))))
      (cond
       ((equal r ?r)
	(org-remove-file file)
	(throw 'nextfile t))
       (t (user-error "Abort"))))))

(defun org-get-agenda-file-buffer (file)
  "Get an agenda buffer visiting FILE.
If the buffer needs to be created, add it to the list of buffers
which might be released later."
  (let ((buf (org-find-base-buffer-visiting file)))
    (if buf
	buf ; just return it
      ;; Make a new buffer and remember it
      (setq buf (find-file-noselect file))
      (when buf (push buf org-agenda-new-buffers))
      buf)))

(defun org-release-buffers (blist)
  "Release all buffers in list, asking the user for confirmation when needed.
When a buffer is unmodified, it is just killed.  When modified, it is saved
\(if the user agrees) and then killed."
  (let (file)
    (dolist (buf blist)
      (setq file (buffer-file-name buf))
      (when (and (buffer-modified-p buf)
		 file
		 (y-or-n-p (format "Save file %s? " file)))
	(with-current-buffer buf (save-buffer)))
      (kill-buffer buf))))

(defun org-agenda-prepare-buffers (files)
  "Create buffers for all agenda files, protect archived trees and comments."
  (interactive)
  (let ((pa '(:org-archived t))
	(pc '(:org-comment t))
	(pall '(:org-archived t :org-comment t))
	(inhibit-read-only t)
	(org-inhibit-startup org-agenda-inhibit-startup)
	(rea (concat ":" org-archive-tag ":"))
	re pos)
    (setq org-tag-alist-for-agenda nil
	  org-tag-groups-alist-for-agenda nil)
    (save-excursion
      (save-restriction
	(dolist (file files)
	  (catch 'nextfile
	    (if (bufferp file)
		(set-buffer file)
	      (org-check-agenda-file file)
	      (set-buffer (org-get-agenda-file-buffer file)))
	    (widen)
	    (org-set-regexps-and-options 'tags-only)
	    (setq pos (point))
	    (or (memq 'category org-agenda-ignore-properties)
		(org-refresh-category-properties))
	    (or (memq 'stats org-agenda-ignore-properties)
		(org-refresh-stats-properties))
	    (or (memq 'effort org-agenda-ignore-properties)
		(org-refresh-effort-properties))
	    (or (memq 'appt org-agenda-ignore-properties)
		(org-refresh-properties "APPT_WARNTIME" 'org-appt-warntime))
	    (setq org-todo-keywords-for-agenda
		  (append org-todo-keywords-for-agenda org-todo-keywords-1))
	    (setq org-done-keywords-for-agenda
		  (append org-done-keywords-for-agenda org-done-keywords))
	    (setq org-todo-keyword-alist-for-agenda
		  (append org-todo-keyword-alist-for-agenda org-todo-key-alist))
	    (setq org-tag-alist-for-agenda
		  (org-uniquify
		   (append org-tag-alist-for-agenda
			   org-current-tag-alist)))
	    ;; Merge current file's tag groups into global
	    ;; `org-tag-groups-alist-for-agenda'.
	    (when org-group-tags
	      (dolist (alist org-tag-groups-alist)
		(let ((old (assoc (car alist) org-tag-groups-alist-for-agenda)))
		  (if old
		      (setcdr old (org-uniquify (append (cdr old) (cdr alist))))
		    (push alist org-tag-groups-alist-for-agenda)))))
	    (org-with-silent-modifications
	     (save-excursion
	       (remove-text-properties (point-min) (point-max) pall)
	       (when org-agenda-skip-archived-trees
		 (goto-char (point-min))
		 (while (re-search-forward rea nil t)
		   (when (org-at-heading-p t)
		     (add-text-properties (point-at-bol) (org-end-of-subtree t) pa))))
	       (goto-char (point-min))
	       (setq re (format "^\\*+ .*\\<%s\\>" org-comment-string))
	       (while (re-search-forward re nil t)
		 (when (save-match-data (org-in-commented-heading-p t))
		   (add-text-properties
		    (match-beginning 0) (org-end-of-subtree t) pc)))))
	    (goto-char pos)))))
    (setq org-todo-keywords-for-agenda
          (org-uniquify org-todo-keywords-for-agenda))
    (setq org-todo-keyword-alist-for-agenda
	  (org-uniquify org-todo-keyword-alist-for-agenda))))


;;;; CDLaTeX minor mode

(defvar org-cdlatex-mode-map (make-sparse-keymap)
  "Keymap for the minor `org-cdlatex-mode'.")

(org-defkey org-cdlatex-mode-map "_" 'org-cdlatex-underscore-caret)
(org-defkey org-cdlatex-mode-map "^" 'org-cdlatex-underscore-caret)
(org-defkey org-cdlatex-mode-map "`" 'cdlatex-math-symbol)
(org-defkey org-cdlatex-mode-map "'" 'org-cdlatex-math-modify)
(org-defkey org-cdlatex-mode-map "\C-c{" 'org-cdlatex-environment-indent)

(defvar org-cdlatex-texmathp-advice-is-done nil
  "Flag remembering if we have applied the advice to texmathp already.")

(define-minor-mode org-cdlatex-mode
  "Toggle the minor `org-cdlatex-mode'.
This mode supports entering LaTeX environment and math in LaTeX fragments
in Org mode.
\\{org-cdlatex-mode-map}"
  nil " OCDL" nil
  (when org-cdlatex-mode
    (require 'cdlatex)
    (run-hooks 'cdlatex-mode-hook)
    (cdlatex-compute-tables))
  (unless org-cdlatex-texmathp-advice-is-done
    (setq org-cdlatex-texmathp-advice-is-done t)
    (defadvice texmathp (around org-math-always-on activate)
      "Always return t in Org buffers.
This is because we want to insert math symbols without dollars even outside
the LaTeX math segments.  If Org mode thinks that point is actually inside
an embedded LaTeX fragment, let `texmathp' do its job.
`\\[org-cdlatex-mode-map]'"
      (interactive)
      (let (p)
	(cond
	 ((not (derived-mode-p 'org-mode)) ad-do-it)
	 ((eq this-command 'cdlatex-math-symbol)
	  (setq ad-return-value t
		texmathp-why '("cdlatex-math-symbol in org-mode" . 0)))
	 (t
	  (let ((p (org-inside-LaTeX-fragment-p)))
	    (if (and p (member (car p) (plist-get org-format-latex-options :matchers)))
		(setq ad-return-value t
		      texmathp-why '("Org mode embedded math" . 0))
	      (when p ad-do-it)))))))))

(defun turn-on-org-cdlatex ()
  "Unconditionally turn on `org-cdlatex-mode'."
  (org-cdlatex-mode 1))

(defun org-try-cdlatex-tab ()
  "Check if it makes sense to execute `cdlatex-tab', and do it if yes.
It makes sense to do so if `org-cdlatex-mode' is active and if the cursor is
  - inside a LaTeX fragment, or
  - after the first word in a line, where an abbreviation expansion could
    insert a LaTeX environment."
  (when org-cdlatex-mode
    (cond
     ;; Before any word on the line: No expansion possible.
     ((save-excursion (skip-chars-backward " \t") (bolp)) nil)
     ;; Just after first word on the line: Expand it.  Make sure it
     ;; cannot happen on headlines, though.
     ((save-excursion
	(skip-chars-backward "a-zA-Z0-9*")
	(skip-chars-backward " \t")
	(and (bolp) (not (org-at-heading-p))))
      (cdlatex-tab) t)
     ((org-inside-LaTeX-fragment-p) (cdlatex-tab) t))))

(defun org-cdlatex-underscore-caret (&optional _arg)
  "Execute `cdlatex-sub-superscript' in LaTeX fragments.
Revert to the normal definition outside of these fragments."
  (interactive "P")
  (if (org-inside-LaTeX-fragment-p)
      (call-interactively 'cdlatex-sub-superscript)
    (let (org-cdlatex-mode)
      (call-interactively (key-binding (vector last-input-event))))))

(defun org-cdlatex-math-modify (&optional _arg)
  "Execute `cdlatex-math-modify' in LaTeX fragments.
Revert to the normal definition outside of these fragments."
  (interactive "P")
  (if (org-inside-LaTeX-fragment-p)
      (call-interactively 'cdlatex-math-modify)
    (let (org-cdlatex-mode)
      (call-interactively (key-binding (vector last-input-event))))))

(defun org-cdlatex-environment-indent (&optional environment item)
  "Execute `cdlatex-environment' and indent the inserted environment.

ENVIRONMENT and ITEM are passed to `cdlatex-environment'.

The inserted environment is indented to current indentation
unless point is at the beginning of the line, in which the
environment remains unintended."
  (interactive)
  ;; cdlatex-environment always return nil.  Therefore, capture output
  ;; first and determine if an environment was selected.
  (let* ((beg (point-marker))
	 (end (copy-marker (point) t))
	 (inserted (progn
		     (ignore-errors (cdlatex-environment environment item))
		     (< beg end)))
	 ;; Figure out how many lines to move forward after the
	 ;; environment has been inserted.
	 (lines (when inserted
		  (save-excursion
		    (- (cl-loop while (< beg (point))
				with x = 0
				do (forward-line -1)
				(cl-incf x)
				finally return x)
		       (if (progn (goto-char beg)
				  (and (progn (skip-chars-forward " \t") (eolp))
				       (progn (skip-chars-backward " \t") (bolp))))
			   1 0)))))
	 (env (org-trim (delete-and-extract-region beg end))))
    (when inserted
      ;; Get indentation of next line unless at column 0.
      (let ((ind (if (bolp) 0
		   (save-excursion
		     (org-return-indent)
		     (prog1 (org-get-indentation)
		       (when (progn (skip-chars-forward " \t") (eolp))
			 (delete-region beg (point)))))))
	    (bol (progn (skip-chars-backward " \t") (bolp))))
	;; Insert a newline before environment unless at column zero
	;; to "escape" the current line.  Insert a newline if
	;; something is one the same line as \end{ENVIRONMENT}.
	(insert
	 (concat (unless bol "\n") env
		 (when (and (skip-chars-forward " \t") (not (eolp))) "\n")))
	(unless (zerop ind)
	  (save-excursion
	    (goto-char beg)
	    (while (< (point) end)
	      (unless (eolp) (indent-line-to ind))
	      (forward-line))))
	(goto-char beg)
	(forward-line lines)
	(indent-line-to ind)))
    (set-marker beg nil)
    (set-marker end nil)))


;;;; LaTeX fragments

(defun org-inside-LaTeX-fragment-p ()
  "Test if point is inside a LaTeX fragment.
I.e. after a \\begin, \\(, \\[, $, or $$, without the corresponding closing
sequence appearing also before point.
Even though the matchers for math are configurable, this function assumes
that \\begin, \\(, \\[, and $$ are always used.  Only the single dollar
delimiters are skipped when they have been removed by customization.
The return value is nil, or a cons cell with the delimiter and the
position of this delimiter.

This function does a reasonably good job, but can locally be fooled by
for example currency specifications.  For example it will assume being in
inline math after \"$22.34\".  The LaTeX fragment formatter will only format
fragments that are properly closed, but during editing, we have to live
with the uncertainty caused by missing closing delimiters.  This function
looks only before point, not after."
  (catch 'exit
    (let ((pos (point))
	  (dodollar (member "$" (plist-get org-format-latex-options :matchers)))
	  (lim (save-excursion (org-backward-paragraph) (point)))
	  dd-on str (start 0) m re)
      (goto-char pos)
      (when dodollar
	(setq str (concat (buffer-substring lim (point)) "\000 X$.")
	      re (nth 1 (assoc "$" org-latex-regexps)))
	(while (string-match re str start)
	  (cond
	   ((= (match-end 0) (length str))
	    (throw 'exit (cons "$" (+ lim (match-beginning 0) 1))))
	   ((= (match-end 0) (- (length str) 5))
	    (throw 'exit nil))
	   (t (setq start (match-end 0))))))
      (when (setq m (re-search-backward "\\(\\\\begin{[^}]*}\\|\\\\(\\|\\\\\\[\\)\\|\\(\\\\end{[^}]*}\\|\\\\)\\|\\\\\\]\\)\\|\\(\\$\\$\\)" lim t))
	(goto-char pos)
	(and (match-beginning 1) (throw 'exit (cons (match-string 1) m)))
	(and (match-beginning 2) (throw 'exit nil))
	;; count $$
	(while (re-search-backward "\\$\\$" lim t)
	  (setq dd-on (not dd-on)))
	(goto-char pos)
	(when dd-on (cons "$$" m))))))

(defun org-inside-latex-macro-p ()
  "Is point inside a LaTeX macro or its arguments?"
  (save-match-data
    (org-in-regexp
     "\\\\[a-zA-Z]+\\*?\\(\\(\\[[^][\n{}]*\\]\\)\\|\\({[^{}\n]*}\\)\\)*")))

(defun org--format-latex-make-overlay (beg end image &optional imagetype)
  "Build an overlay between BEG and END using IMAGE file.
Argument IMAGETYPE is the extension of the displayed image,
as a string.  It defaults to \"png\"."
  (let ((ov (make-overlay beg end))
	(imagetype (or (intern imagetype) 'png)))
    (overlay-put ov 'org-overlay-type 'org-latex-overlay)
    (overlay-put ov 'evaporate t)
    (overlay-put ov
		 'modification-hooks
		 (list (lambda (o _flag _beg _end &optional _l)
			 (delete-overlay o))))
    (overlay-put ov
		 'display
		 (list 'image :type imagetype :file image :ascent 'center))))

(defun org--list-latex-overlays (&optional beg end)
  "List all Org LaTeX overlays in current buffer.
Limit to overlays between BEG and END when those are provided."
  (cl-remove-if-not
   (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
   (overlays-in (or beg (point-min)) (or end (point-max)))))

(defun org-remove-latex-fragment-image-overlays (&optional beg end)
  "Remove all overlays with LaTeX fragment images in current buffer.
When optional arguments BEG and END are non-nil, remove all
overlays between them instead.  Return a non-nil value when some
overlays were removed, nil otherwise."
  (let ((overlays (org--list-latex-overlays beg end)))
    (mapc #'delete-overlay overlays)
    overlays))

(defun org-toggle-latex-fragment (&optional arg)
  "Preview the LaTeX fragment at point, or all locally or globally.

If the cursor is on a LaTeX fragment, create the image and overlay
it over the source code, if there is none.  Remove it otherwise.
If there is no fragment at point, display all fragments in the
current section.

With prefix ARG, preview or clear image for all fragments in the
current subtree or in the whole buffer when used before the first
headline.  With a prefix ARG `\\[universal-argument] \
\\[universal-argument]' preview or clear images
for all fragments in the buffer."
  (interactive "P")
  (when (display-graphic-p)
    (catch 'exit
      (save-excursion
	(let (beg end msg)
	  (cond
	   ((or (equal arg '(16))
		(and (equal arg '(4))
		     (org-with-limited-levels (org-before-first-heading-p))))
	    (if (org-remove-latex-fragment-image-overlays)
		(progn (message "LaTeX fragments images removed from buffer")
		       (throw 'exit nil))
	      (setq msg "Creating images for buffer...")))
	   ((equal arg '(4))
	    (org-with-limited-levels (org-back-to-heading t))
	    (setq beg (point))
	    (setq end (progn (org-end-of-subtree t) (point)))
	    (if (org-remove-latex-fragment-image-overlays beg end)
		(progn
		  (message "LaTeX fragment images removed from subtree")
		  (throw 'exit nil))
	      (setq msg "Creating images for subtree...")))
	   ((let ((datum (org-element-context)))
	      (when (memq (org-element-type datum)
			  '(latex-environment latex-fragment))
		(setq beg (org-element-property :begin datum))
		(setq end (org-element-property :end datum))
		(if (org-remove-latex-fragment-image-overlays beg end)
		    (progn (message "LaTeX fragment image removed")
			   (throw 'exit nil))
		  (setq msg "Creating image...")))))
	   (t
	    (org-with-limited-levels
	     (setq beg (if (org-at-heading-p) (line-beginning-position)
			 (outline-previous-heading)
			 (point)))
	     (setq end (progn (outline-next-heading) (point)))
	     (if (org-remove-latex-fragment-image-overlays beg end)
		 (progn
		   (message "LaTeX fragment images removed from section")
		   (throw 'exit nil))
	       (setq msg "Creating images for section...")))))
	  (let ((file (buffer-file-name (buffer-base-buffer))))
	    (org-format-latex
	     (concat org-preview-latex-image-directory "org-ltximg")
	     beg end
	     ;; Emacs cannot overlay images from remote hosts.  Create
	     ;; it in `temporary-file-directory' instead.
	     (if (or (not file) (file-remote-p file))
		 temporary-file-directory
	       default-directory)
	     'overlays msg 'forbuffer org-preview-latex-default-process))
	  (message (concat msg "done")))))))

(defun org-format-latex
    (prefix &optional beg end dir overlays msg forbuffer processing-type)
  "Replace LaTeX fragments with links to an image.

The function takes care of creating the replacement image.

Only consider fragments between BEG and END when those are
provided.

When optional argument OVERLAYS is non-nil, display the image on
top of the fragment instead of replacing it.

PROCESSING-TYPE is the conversion method to use, as a symbol.

Some of the options can be changed using the variable
`org-format-latex-options', which see."
  (when (and overlays (fboundp 'clear-image-cache)) (clear-image-cache))
  (unless (eq processing-type 'verbatim)
    (let* ((math-regexp "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}")
	   (cnt 0)
	   checkdir-flag)
      (goto-char (or beg (point-min)))
      ;; Optimize overlay creation: (info "(elisp) Managing Overlays").
      (when (and overlays (memq processing-type '(dvipng imagemagick)))
	(overlay-recenter (or end (point-max))))
      (while (re-search-forward math-regexp end t)
	(unless (and overlays
		     (eq (get-char-property (point) 'org-overlay-type)
			 'org-latex-overlay))
	  (let* ((context (org-element-context))
		 (type (org-element-type context)))
	    (when (memq type '(latex-environment latex-fragment))
	      (let ((block-type (eq type 'latex-environment))
		    (value (org-element-property :value context))
		    (beg (org-element-property :begin context))
		    (end (save-excursion
			   (goto-char (org-element-property :end context))
			   (skip-chars-backward " \r\t\n")
			   (point))))
		(cond
		 ((eq processing-type 'mathjax)
		  ;; Prepare for MathJax processing.
		  (if (not (string-match "\\`\\$\\$?" value))
		      (goto-char end)
		    (delete-region beg end)
		    (if (string= (match-string 0 value) "$$")
			(insert "\\[" (substring value 2 -2) "\\]")
		      (insert "\\(" (substring value 1 -1) "\\)"))))
		 ((assq processing-type org-preview-latex-process-alist)
		  ;; Process to an image.
		  (cl-incf cnt)
		  (goto-char beg)
		  (let* ((processing-info
			  (cdr (assq processing-type org-preview-latex-process-alist)))
			 (face (face-at-point))
			 ;; Get the colors from the face at point.
			 (fg
			  (let ((color (plist-get org-format-latex-options
						  :foreground)))
			    (if (and forbuffer (eq color 'auto))
				(face-attribute face :foreground nil 'default)
			      color)))
			 (bg
			  (let ((color (plist-get org-format-latex-options
						  :background)))
			    (if (and forbuffer (eq color 'auto))
				(face-attribute face :background nil 'default)
			      color)))
			 (hash (sha1 (prin1-to-string
				      (list org-format-latex-header
					    org-latex-default-packages-alist
					    org-latex-packages-alist
					    org-format-latex-options
					    forbuffer value fg bg))))
			 (imagetype (or (plist-get processing-info :image-output-type) "png"))
			 (absprefix (expand-file-name prefix dir))
			 (linkfile (format "%s_%s.%s" prefix hash imagetype))
			 (movefile (format "%s_%s.%s" absprefix hash imagetype))
			 (sep (and block-type "\n\n"))
			 (link (concat sep "[[file:" linkfile "]]" sep))
			 (options
			  (org-combine-plists
			   org-format-latex-options
			   `(:foreground ,fg :background ,bg))))
		    (when msg (message msg cnt))
		    (unless checkdir-flag ; Ensure the directory exists.
		      (setq checkdir-flag t)
		      (let ((todir (file-name-directory absprefix)))
			(unless (file-directory-p todir)
			  (make-directory todir t))))
		    (unless (file-exists-p movefile)
		      (org-create-formula-image
		       value movefile options forbuffer processing-type))
		    (if overlays
			(progn
			  (dolist (o (overlays-in beg end))
			    (when (eq (overlay-get o 'org-overlay-type)
				      'org-latex-overlay)
			      (delete-overlay o)))
			  (org--format-latex-make-overlay beg end movefile imagetype)
			  (goto-char end))
		      (delete-region beg end)
		      (insert
		       (org-add-props link
			   (list 'org-latex-src
				 (replace-regexp-in-string "\"" "" value)
				 'org-latex-src-embed-type
				 (if block-type 'paragraph 'character)))))))
		 ((eq processing-type 'mathml)
		  ;; Process to MathML.
		  (unless (org-format-latex-mathml-available-p)
		    (user-error "LaTeX to MathML converter not configured"))
		  (cl-incf cnt)
		  (when msg (message msg cnt))
		  (goto-char beg)
		  (delete-region beg end)
		  (insert (org-format-latex-as-mathml
			   value block-type prefix dir)))
		 (t
		  (error "Unknown conversion process %s for LaTeX fragments"
			 processing-type)))))))))))

(defun org-create-math-formula (latex-frag &optional mathml-file)
  "Convert LATEX-FRAG to MathML and store it in MATHML-FILE.
Use `org-latex-to-mathml-convert-command'.  If the conversion is
sucessful, return the portion between \"<math...> </math>\"
elements otherwise return nil.  When MATHML-FILE is specified,
write the results in to that file.  When invoked as an
interactive command, prompt for LATEX-FRAG, with initial value
set to the current active region and echo the results for user
inspection."
  (interactive (list (let ((frag (when (org-region-active-p)
				   (buffer-substring-no-properties
				    (region-beginning) (region-end)))))
		       (read-string "LaTeX Fragment: " frag nil frag))))
  (unless latex-frag (user-error "Invalid LaTeX fragment"))
  (let* ((tmp-in-file
	  (let ((file (file-relative-name
		       (make-temp-name (expand-file-name "ltxmathml-in")))))
	    (write-region latex-frag nil file)
	    file))
	 (tmp-out-file (file-relative-name
			(make-temp-name (expand-file-name  "ltxmathml-out"))))
	 (cmd (format-spec
	       org-latex-to-mathml-convert-command
	       `((?j . ,(and org-latex-to-mathml-jar-file
			     (shell-quote-argument
			      (expand-file-name
			       org-latex-to-mathml-jar-file))))
		 (?I . ,(shell-quote-argument tmp-in-file))
		 (?i . ,latex-frag)
		 (?o . ,(shell-quote-argument tmp-out-file)))))
	 mathml shell-command-output)
    (when (called-interactively-p 'any)
      (unless (org-format-latex-mathml-available-p)
	(user-error "LaTeX to MathML converter not configured")))
    (message "Running %s" cmd)
    (setq shell-command-output (shell-command-to-string cmd))
    (setq mathml
	  (when (file-readable-p tmp-out-file)
	    (with-current-buffer (find-file-noselect tmp-out-file t)
	      (goto-char (point-min))
	      (when (re-search-forward
		     (format "<math[^>]*?%s[^>]*?>\\(.\\|\n\\)*</math>"
			     (regexp-quote
			      "xmlns=\"http://www.w3.org/1998/Math/MathML\""))
		     nil t)
		(prog1 (match-string 0) (kill-buffer))))))
    (cond
     (mathml
      (setq mathml
	    (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" mathml))
      (when mathml-file
	(write-region mathml nil mathml-file))
      (when (called-interactively-p 'any)
	(message mathml)))
     ((message "LaTeX to MathML conversion failed")
      (message shell-command-output)))
    (delete-file tmp-in-file)
    (when (file-exists-p tmp-out-file)
      (delete-file tmp-out-file))
    mathml))

(defun org-format-latex-as-mathml (latex-frag latex-frag-type
					      prefix &optional dir)
  "Use `org-create-math-formula' but check local cache first."
  (let* ((absprefix (expand-file-name prefix dir))
	 (print-length nil) (print-level nil)
	 (formula-id (concat
		      "formula-"
		      (sha1
		       (prin1-to-string
			(list latex-frag
			      org-latex-to-mathml-convert-command)))))
	 (formula-cache (format "%s-%s.mathml" absprefix formula-id))
	 (formula-cache-dir (file-name-directory formula-cache)))

    (unless (file-directory-p formula-cache-dir)
      (make-directory formula-cache-dir t))

    (unless (file-exists-p formula-cache)
      (org-create-math-formula latex-frag formula-cache))

    (if (file-exists-p formula-cache)
	;; Successful conversion.  Return the link to MathML file.
	(org-add-props
	    (format  "[[file:%s]]" (file-relative-name formula-cache dir))
	    (list 'org-latex-src (replace-regexp-in-string "\"" "" latex-frag)
		  'org-latex-src-embed-type (if latex-frag-type
						'paragraph 'character)))
      ;; Failed conversion.  Return the LaTeX fragment verbatim
      latex-frag)))

(defun org--get-display-dpi ()
  "Get the DPI of the display.
The function assumes that the display has the same pixel width in
the horizontal and vertical directions."
  (if (display-graphic-p)
      (round (/ (display-pixel-height)
		(/ (display-mm-height) 25.4)))
    (error "Attempt to calculate the dpi of a non-graphic display")))

(defun org-create-formula-image
    (string tofile options buffer &optional processing-type)
  "Create an image from LaTeX source using external processes.

The LaTeX STRING is saved to a temporary LaTeX file, then
converted to an image file by process PROCESSING-TYPE defined in
`org-preview-latex-process-alist'.  A nil value defaults to
`org-preview-latex-default-process'.

The generated image file is eventually moved to TOFILE.

The OPTIONS argument controls the size, foreground color and
background color of the generated image.

When BUFFER non-nil, this function is used for LaTeX previewing.
Otherwise, it is used to deal with LaTeX snippets showed in
a HTML file."
  (let* ((processing-type (or processing-type
			      org-preview-latex-default-process))
	 (processing-info
	  (cdr (assq processing-type org-preview-latex-process-alist)))
	 (programs (plist-get processing-info :programs))
	 (error-message (or (plist-get processing-info :message) ""))
	 (use-xcolor (plist-get processing-info :use-xcolor))
	 (image-input-type (plist-get processing-info :image-input-type))
	 (image-output-type (plist-get processing-info :image-output-type))
	 (post-clean (or (plist-get processing-info :post-clean)
			 '(".dvi" ".xdv" ".pdf" ".tex" ".aux" ".log"
			   ".svg" ".png" ".jpg" ".jpeg" ".out")))
	 (latex-header
	  (or (plist-get processing-info :latex-header)
	      (org-latex-make-preamble
	       (org-export-get-environment (org-export-get-backend 'latex))
	       org-format-latex-header
	       'snippet)))
	 (latex-compiler (plist-get processing-info :latex-compiler))
	 (image-converter (plist-get processing-info :image-converter))
	 (tmpdir temporary-file-directory)
	 (texfilebase (make-temp-name
		       (expand-file-name "orgtex" tmpdir)))
	 (texfile (concat texfilebase ".tex"))
	 (image-size-adjust (or (plist-get processing-info :image-size-adjust)
				'(1.0 . 1.0)))
	 (scale (* (if buffer (car image-size-adjust) (cdr image-size-adjust))
		   (or (plist-get options (if buffer :scale :html-scale)) 1.0)))
	 (dpi (* scale (if buffer (org--get-display-dpi) 140.0)))
	 (fg (or (plist-get options (if buffer :foreground :html-foreground))
		 "Black"))
	 (bg (or (plist-get options (if buffer :background :html-background))
		 "Transparent"))
	 (log-buf (get-buffer-create "*Org Preview LaTeX Output*"))
	 (resize-mini-windows nil)) ;Fix Emacs flicker when creating image.
    (dolist (program programs)
      (org-check-external-command program error-message))
    (if use-xcolor
	(progn (if (eq fg 'default)
		   (setq fg (org-latex-color :foreground))
		 (setq fg (org-latex-color-format fg)))
	       (if (eq bg 'default)
		   (setq bg (org-latex-color :background))
		 (setq bg (org-latex-color-format
			   (if (string= bg "Transparent") "white" bg))))
	       (with-temp-file texfile
		 (insert latex-header)
		 (insert "\n\\begin{document}\n"
			 "\\definecolor{fg}{rgb}{" fg "}\n"
			 "\\definecolor{bg}{rgb}{" bg "}\n"
			 "\n\\pagecolor{bg}\n"
			 "\n{\\color{fg}\n"
			 string
			 "\n}\n"
			 "\n\\end{document}\n")))
      (if (eq fg 'default)
	  (setq fg (org-dvipng-color :foreground))
	(unless (string= fg "Transparent")
	  (setq fg (org-dvipng-color-format fg))))
      (if (eq bg 'default)
	  (setq bg (org-dvipng-color :background))
	(unless (string= bg "Transparent")
	  (setq bg (org-dvipng-color-format bg))))
      (with-temp-file texfile
	(insert latex-header)
	(insert "\n\\begin{document}\n" string "\n\\end{document}\n")))

    (let* ((err-msg (format "Please adjust `%s' part of \
`org-preview-latex-process-alist'."
			    processing-type))
	   (image-input-file
	    (org-compile-file
	     texfile latex-compiler image-input-type err-msg log-buf))
	   (image-output-file
	    (org-compile-file
	     image-input-file image-converter image-output-type err-msg log-buf
	     `((?F . ,(shell-quote-argument fg))
	       (?B . ,(shell-quote-argument bg))
	       (?D . ,(shell-quote-argument (format "%s" dpi)))
	       (?S . ,(shell-quote-argument (format "%s" (/ dpi 140.0))))))))
      (copy-file image-output-file tofile 'replace)
      (dolist (e post-clean)
	(when (file-exists-p (concat texfilebase e))
	  (delete-file (concat texfilebase e))))
      image-output-file)))

(defun org-splice-latex-header (tpl def-pkg pkg snippets-p &optional extra)
  "Fill a LaTeX header template TPL.
In the template, the following place holders will be recognized:

 [DEFAULT-PACKAGES]      \\usepackage statements for DEF-PKG
 [NO-DEFAULT-PACKAGES]   do not include DEF-PKG
 [PACKAGES]              \\usepackage statements for PKG
 [NO-PACKAGES]           do not include PKG
 [EXTRA]                 the string EXTRA
 [NO-EXTRA]              do not include EXTRA

For backward compatibility, if both the positive and the negative place
holder is missing, the positive one (without the \"NO-\") will be
assumed to be present at the end of the template.
DEF-PKG and PKG are assumed to be alists of options/packagename lists.
EXTRA is a string.
SNIPPETS-P indicates if this is run to create snippet images for HTML."
  (let (rpl (end ""))
    (if (string-match "^[ \t]*\\[\\(NO-\\)?DEFAULT-PACKAGES\\][ \t]*\n?" tpl)
	(setq rpl (if (or (match-end 1) (not def-pkg))
		      "" (org-latex-packages-to-string def-pkg snippets-p t))
	      tpl (replace-match rpl t t tpl))
      (when def-pkg (setq end (org-latex-packages-to-string def-pkg snippets-p))))

    (if (string-match "\\[\\(NO-\\)?PACKAGES\\][ \t]*\n?" tpl)
	(setq rpl (if (or (match-end 1) (not pkg))
		      "" (org-latex-packages-to-string pkg snippets-p t))
	      tpl (replace-match rpl t t tpl))
      (when pkg (setq end
		      (concat end "\n"
			      (org-latex-packages-to-string pkg snippets-p)))))

    (if (string-match "\\[\\(NO-\\)?EXTRA\\][ \t]*\n?" tpl)
	(setq rpl (if (or (match-end 1) (not extra))
		      "" (concat extra "\n"))
	      tpl (replace-match rpl t t tpl))
      (when (and extra (string-match "\\S-" extra))
	(setq end (concat end "\n" extra))))

    (if (string-match "\\S-" end)
	(concat tpl "\n" end)
      tpl)))

(defun org-latex-packages-to-string (pkg &optional snippets-p newline)
  "Turn an alist of packages into a string with the \\usepackage macros."
  (setq pkg (mapconcat (lambda(p)
			 (cond
			  ((stringp p) p)
			  ((and snippets-p (>= (length p) 3) (not (nth 2 p)))
			   (format "%% Package %s omitted" (cadr p)))
			  ((equal "" (car p))
			   (format "\\usepackage{%s}" (cadr p)))
			  (t
			   (format "\\usepackage[%s]{%s}"
				   (car p) (cadr p)))))
		       pkg
		       "\n"))
  (if newline (concat pkg "\n") pkg))

(defun org-dvipng-color (attr)
  "Return a RGB color specification for dvipng."
  (org-dvipng-color-format (face-attribute 'default attr nil)))

(defun org-dvipng-color-format (color-name)
  "Convert COLOR-NAME to a RGB color value for dvipng."
  (apply #'format "rgb %s %s %s"
	 (mapcar 'org-normalize-color
		 (color-values color-name))))

(defun org-latex-color (attr)
  "Return a RGB color for the LaTeX color package."
  (org-latex-color-format (face-attribute 'default attr nil)))

(defun org-latex-color-format (color-name)
  "Convert COLOR-NAME to a RGB color value."
  (apply #'format "%s,%s,%s"
	 (mapcar 'org-normalize-color
		 (color-values color-name))))

(defun org-normalize-color (value)
  "Return string to be used as color value for an RGB component."
  (format "%g" (/ value 65535.0)))



;; Image display

(defvar-local org-inline-image-overlays nil)

(defun org-toggle-inline-images (&optional include-linked)
  "Toggle the display of inline images.
INCLUDE-LINKED is passed to `org-display-inline-images'."
  (interactive "P")
  (if org-inline-image-overlays
      (progn
	(org-remove-inline-images)
	(when (called-interactively-p 'interactive)
	  (message "Inline image display turned off")))
    (org-display-inline-images include-linked)
    (when (called-interactively-p 'interactive)
      (message (if org-inline-image-overlays
		   (format "%d images displayed inline"
			   (length org-inline-image-overlays))
		 "No images to display inline")))))

(defun org-redisplay-inline-images ()
  "Refresh the display of inline images."
  (interactive)
  (if (not org-inline-image-overlays)
      (org-toggle-inline-images)
    (org-toggle-inline-images)
    (org-toggle-inline-images)))

(defun org-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.

An inline image is a link which follows either of these
conventions:

  1. Its path is a file with an extension matching return value
     from `image-file-name-regexp' and it has no contents.

  2. Its description consists in a single link of the previous
     type.

When optional argument INCLUDE-LINKED is non-nil, also links with
a text description part will be inlined.  This can be nice for
a quick look at those images, but it does not reflect what
exported files will look like.

When optional argument REFRESH is non-nil, refresh existing
images between BEG and END.  This will create new image displays
only if necessary.  BEG and END default to the buffer
boundaries."
  (interactive "P")
  (when (display-graphic-p)
    (unless refresh
      (org-remove-inline-images)
      (when (fboundp 'clear-image-cache) (clear-image-cache)))
    (org-with-wide-buffer
     (goto-char (or beg (point-min)))
     (let* ((case-fold-search t)
	    (file-extension-re (image-file-name-regexp))
	    (link-abbrevs (mapcar #'car
				  (append org-link-abbrev-alist-local
					  org-link-abbrev-alist)))
	    ;; Check absolute, relative file names and explicit
	    ;; "file:" links.  Also check link abbreviations since
	    ;; some might expand to "file" links.
	    (file-types-re (format "[][]\\[\\(?:file\\|[./~]%s\\)"
				   (if (not link-abbrevs) ""
				     (format "\\|\\(?:%s:\\)"
					     (regexp-opt link-abbrevs))))))
       (while (re-search-forward file-types-re end t)
	 (let ((link (save-match-data (org-element-context))))
	   ;; Check if we're at an inline image, i.e., an image file
	   ;; link without a description (unless INCLUDE-LINKED is
	   ;; non-nil).
	   (when (and (equal "file" (org-element-property :type link))
		      (or include-linked
			  (null (org-element-contents link)))
		      (string-match-p file-extension-re
				      (org-element-property :path link)))
	     (let ((file (expand-file-name
			  (org-link-unescape
			   (org-element-property :path link)))))
	       (when (file-exists-p file)
		 (let ((width
			;; Apply `org-image-actual-width' specifications.
			(cond
			 ((not (image-type-available-p 'imagemagick)) nil)
			 ((eq org-image-actual-width t) nil)
			 ((listp org-image-actual-width)
			  (or
			   ;; First try to find a width among
			   ;; attributes associated to the paragraph
			   ;; containing link.
			   (let ((paragraph
				  (let ((e link))
				    (while (and (setq e (org-element-property
							 :parent e))
						(not (eq (org-element-type e)
							 'paragraph))))
				    e)))
			     (when paragraph
			       (save-excursion
				 (goto-char (org-element-property :begin paragraph))
				 (when
				     (re-search-forward
				      "^[ \t]*#\\+attr_.*?: +.*?:width +\\(\\S-+\\)"
				      (org-element-property
				       :post-affiliated paragraph)
				      t)
				   (string-to-number (match-string 1))))))
			   ;; Otherwise, fall-back to provided number.
			   (car org-image-actual-width)))
			 ((numberp org-image-actual-width)
			  org-image-actual-width)))
		       (old (get-char-property-and-overlay
			     (org-element-property :begin link)
			     'org-image-overlay)))
		   (if (and (car-safe old) refresh)
		       (image-refresh (overlay-get (cdr old) 'display))
		     (let ((image (create-image file
						(and width 'imagemagick)
						nil
						:width width)))
		       (when image
			 (let ((ov (make-overlay
				    (org-element-property :begin link)
				    (progn
				      (goto-char
				       (org-element-property :end link))
				      (skip-chars-backward " \t")
				      (point)))))
			   (overlay-put ov 'display image)
			   (overlay-put ov 'face 'default)
			   (overlay-put ov 'org-image-overlay t)
			   (overlay-put
			    ov 'modification-hooks
			    (list 'org-display-inline-remove-overlay))
			   (push ov org-inline-image-overlays)))))))))))))))

(defun org-display-inline-remove-overlay (ov after _beg _end &optional _len)
  "Remove inline-display overlay if a corresponding region is modified."
  (let ((inhibit-modification-hooks t))
    (when (and ov after)
      (delete ov org-inline-image-overlays)
      (delete-overlay ov))))

(defun org-remove-inline-images ()
  "Remove inline display of images."
  (interactive)
  (mapc #'delete-overlay org-inline-image-overlays)
  (setq org-inline-image-overlays nil))

;;;; Key bindings

(defun org-remap (map &rest commands)
  "In MAP, remap the functions given in COMMANDS.
COMMANDS is a list of alternating OLDDEF NEWDEF command names."
  (let (new old)
    (while commands
      (setq old (pop commands) new (pop commands))
      (org-defkey map (vector 'remap old) new))))

;; Outline functions from `outline-mode-prefix-map'
;; that can be remapped in Org:
(define-key org-mode-map [remap outline-mark-subtree] 'org-mark-subtree)
(define-key org-mode-map [remap outline-show-subtree] 'org-show-subtree)
(define-key org-mode-map [remap outline-forward-same-level]
  'org-forward-heading-same-level)
(define-key org-mode-map [remap outline-backward-same-level]
  'org-backward-heading-same-level)
(define-key org-mode-map [remap outline-show-branches]
  'org-kill-note-or-show-branches)
(define-key org-mode-map [remap outline-promote] 'org-promote-subtree)
(define-key org-mode-map [remap outline-demote] 'org-demote-subtree)
(define-key org-mode-map [remap outline-insert-heading] 'org-ctrl-c-ret)
(define-key org-mode-map [remap outline-next-visible-heading]
  'org-next-visible-heading)
(define-key org-mode-map [remap outline-previous-visible-heading]
  'org-previous-visible-heading)
(define-key org-mode-map [remap show-children] 'org-show-children)

;; Outline functions from `outline-mode-prefix-map' that can not
;; be remapped in Org:

;; - the column "key binding" shows whether the Outline function is still
;;   available in Org mode on the same key that it has been bound to in
;;   Outline mode:
;;   - "overridden": key used for a different functionality in Org mode
;;   - else: key still bound to the same Outline function in Org mode

;; | Outline function                   | key binding | Org replacement          |
;; |------------------------------------+-------------+--------------------------|
;; | `outline-up-heading'               | `C-c C-u'   | still same function      |
;; | `outline-move-subtree-up'          | overridden  | better: org-shiftup      |
;; | `outline-move-subtree-down'        | overridden  | better: org-shiftdown    |
;; | `show-entry'                       | overridden  | no replacement           |
;; | `show-branches'                    | `C-c C-k'   | still same function      |
;; | `show-subtree'                     | overridden  | visibility cycling       |
;; | `show-all'                         | overridden  | no replacement           |
;; | `hide-subtree'                     | overridden  | visibility cycling       |
;; | `hide-body'                        | overridden  | no replacement           |
;; | `hide-entry'                       | overridden  | visibility cycling       |
;; | `hide-leaves'                      | overridden  | no replacement           |
;; | `hide-sublevels'                   | overridden  | no replacement           |
;; | `hide-other'                       | overridden  | no replacement           |

;; Make `C-c C-x' a prefix key
(org-defkey org-mode-map "\C-c\C-x" (make-sparse-keymap))

;; TAB key with modifiers
(org-defkey org-mode-map "\C-i"       'org-cycle)
(org-defkey org-mode-map [(tab)]      'org-cycle)
(org-defkey org-mode-map [(control tab)] 'org-force-cycle-archived)
(org-defkey org-mode-map "\M-\t" #'pcomplete)

;; The following line is necessary under Suse GNU/Linux
(org-defkey org-mode-map [S-iso-lefttab]  'org-shifttab)
(org-defkey org-mode-map [(shift tab)]    'org-shifttab)
(define-key org-mode-map [backtab] 'org-shifttab)

(org-defkey org-mode-map [(shift return)]   'org-table-copy-down)
(org-defkey org-mode-map [(meta shift return)] 'org-insert-todo-heading)
(org-defkey org-mode-map (kbd "M-RET") #'org-meta-return)

;; Cursor keys with modifiers
(org-defkey org-mode-map [(meta left)]  'org-metaleft)
(org-defkey org-mode-map [(meta right)] 'org-metaright)
(org-defkey org-mode-map [(meta up)]    'org-metaup)
(org-defkey org-mode-map [(meta down)]  'org-metadown)

(org-defkey org-mode-map [(control meta shift right)] 'org-increase-number-at-point)
(org-defkey org-mode-map [(control meta shift left)] 'org-decrease-number-at-point)
(org-defkey org-mode-map [(meta shift left)]   'org-shiftmetaleft)
(org-defkey org-mode-map [(meta shift right)]  'org-shiftmetaright)
(org-defkey org-mode-map [(meta shift up)]     'org-shiftmetaup)
(org-defkey org-mode-map [(meta shift down)]   'org-shiftmetadown)

(org-defkey org-mode-map [(shift up)]          'org-shiftup)
(org-defkey org-mode-map [(shift down)]        'org-shiftdown)
(org-defkey org-mode-map [(shift left)]        'org-shiftleft)
(org-defkey org-mode-map [(shift right)]       'org-shiftright)

(org-defkey org-mode-map [(control shift right)] 'org-shiftcontrolright)
(org-defkey org-mode-map [(control shift left)]  'org-shiftcontrolleft)
(org-defkey org-mode-map [(control shift up)] 'org-shiftcontrolup)
(org-defkey org-mode-map [(control shift down)]  'org-shiftcontroldown)

;; Babel keys
(define-key org-mode-map org-babel-key-prefix org-babel-map)
(dolist (pair org-babel-key-bindings)
  (define-key org-babel-map (car pair) (cdr pair)))

;;; Extra keys for tty access.
;;  We only set them when really needed because otherwise the
;;  menus don't show the simple keys

(when (or org-use-extra-keys (not window-system))
  (org-defkey org-mode-map "\C-c\C-xc"    'org-table-copy-down)
  (org-defkey org-mode-map "\C-c\C-xM"    'org-insert-todo-heading)
  (org-defkey org-mode-map "\C-c\C-xm"    'org-meta-return)
  (org-defkey org-mode-map [?\e (return)] 'org-meta-return)
  (org-defkey org-mode-map [?\e (left)]   'org-metaleft)
  (org-defkey org-mode-map "\C-c\C-xl"    'org-metaleft)
  (org-defkey org-mode-map [?\e (right)]  'org-metaright)
  (org-defkey org-mode-map "\C-c\C-xr"    'org-metaright)
  (org-defkey org-mode-map [?\e (up)]     'org-metaup)
  (org-defkey org-mode-map "\C-c\C-xu"    'org-metaup)
  (org-defkey org-mode-map [?\e (down)]   'org-metadown)
  (org-defkey org-mode-map "\C-c\C-xd"    'org-metadown)
  (org-defkey org-mode-map "\C-c\C-xL"    'org-shiftmetaleft)
  (org-defkey org-mode-map "\C-c\C-xR"    'org-shiftmetaright)
  (org-defkey org-mode-map "\C-c\C-xU"    'org-shiftmetaup)
  (org-defkey org-mode-map "\C-c\C-xD"    'org-shiftmetadown)
  (org-defkey org-mode-map [?\C-c (up)]    'org-shiftup)
  (org-defkey org-mode-map [?\C-c (down)]  'org-shiftdown)
  (org-defkey org-mode-map [?\C-c (left)]  'org-shiftleft)
  (org-defkey org-mode-map [?\C-c (right)] 'org-shiftright)
  (org-defkey org-mode-map [?\C-c ?\C-x (right)] 'org-shiftcontrolright)
  (org-defkey org-mode-map [?\C-c ?\C-x (left)] 'org-shiftcontrolleft)
  (org-defkey org-mode-map [?\e (tab)] #'pcomplete)
  (org-defkey org-mode-map [?\e (shift return)] 'org-insert-todo-heading)
  (org-defkey org-mode-map [?\e (shift left)]   'org-shiftmetaleft)
  (org-defkey org-mode-map [?\e (shift right)]  'org-shiftmetaright)
  (org-defkey org-mode-map [?\e (shift up)]     'org-shiftmetaup)
  (org-defkey org-mode-map [?\e (shift down)]   'org-shiftmetadown))

;; All the other keys
(org-remap org-mode-map
	   'self-insert-command 'org-self-insert-command
	   'delete-char 'org-delete-char
	   'delete-backward-char 'org-delete-backward-char)
(org-defkey org-mode-map "|" 'org-force-self-insert)

(org-defkey org-mode-map "\C-c\C-a" 'outline-show-all) ; in case allout messed up.
(org-defkey org-mode-map "\C-c\C-r" 'org-reveal)
(if (boundp 'narrow-map)
    (org-defkey narrow-map "s" 'org-narrow-to-subtree)
  (org-defkey org-mode-map "\C-xns" 'org-narrow-to-subtree))
(if (boundp 'narrow-map)
    (org-defkey narrow-map "b" 'org-narrow-to-block)
  (org-defkey org-mode-map "\C-xnb" 'org-narrow-to-block))
(if (boundp 'narrow-map)
    (org-defkey narrow-map "e" 'org-narrow-to-element)
  (org-defkey org-mode-map "\C-xne" 'org-narrow-to-element))
(org-defkey org-mode-map "\C-\M-t"  'org-transpose-element)
(org-defkey org-mode-map "\M-}"     'org-forward-element)
(org-defkey org-mode-map "\M-{"     'org-backward-element)
(org-defkey org-mode-map "\C-c\C-^" 'org-up-element)
(org-defkey org-mode-map "\C-c\C-_" 'org-down-element)
(org-defkey org-mode-map "\C-c\C-f" 'org-forward-heading-same-level)
(org-defkey org-mode-map "\C-c\C-b" 'org-backward-heading-same-level)
(org-defkey org-mode-map "\C-c\M-f" 'org-next-block)
(org-defkey org-mode-map "\C-c\M-b" 'org-previous-block)
(org-defkey org-mode-map "\C-c$"    'org-archive-subtree)
(org-defkey org-mode-map "\C-c\C-x\C-s" 'org-archive-subtree)
(org-defkey org-mode-map "\C-c\C-x\C-a" 'org-archive-subtree-default)
(org-defkey org-mode-map "\C-c\C-xd" 'org-insert-drawer)
(org-defkey org-mode-map "\C-c\C-xa" 'org-toggle-archive-tag)
(org-defkey org-mode-map "\C-c\C-xA" 'org-archive-to-archive-sibling)
(org-defkey org-mode-map "\C-c\C-xb" 'org-tree-to-indirect-buffer)
(org-defkey org-mode-map "\C-c\C-xq" 'org-toggle-tags-groups)
(org-defkey org-mode-map "\C-c\C-j" 'org-goto)
(org-defkey org-mode-map "\C-c\C-t" 'org-todo)
(org-defkey org-mode-map "\C-c\C-q" 'org-set-tags-command)
(org-defkey org-mode-map "\C-c\C-s" 'org-schedule)
(org-defkey org-mode-map "\C-c\C-d" 'org-deadline)
(org-defkey org-mode-map "\C-c;"    'org-toggle-comment)
(org-defkey org-mode-map "\C-c\C-w" 'org-refile)
(org-defkey org-mode-map "\C-c\M-w" 'org-copy)
(org-defkey org-mode-map "\C-c/"    'org-sparse-tree)   ; Minor-mode reserved
(org-defkey org-mode-map "\C-c\\"   'org-match-sparse-tree) ; Minor-mode res.
(org-defkey org-mode-map "\C-c\C-m" 'org-ctrl-c-ret)
(org-defkey org-mode-map "\C-c\C-xc" 'org-clone-subtree-with-time-shift)
(org-defkey org-mode-map "\C-c\C-xv" 'org-copy-visible)
(org-defkey org-mode-map [(control return)] 'org-insert-heading-respect-content)
(org-defkey org-mode-map [(shift control return)] 'org-insert-todo-heading-respect-content)
(org-defkey org-mode-map "\C-c\C-x\C-n" 'org-next-link)
(org-defkey org-mode-map "\C-c\C-x\C-p" 'org-previous-link)
(org-defkey org-mode-map "\C-c\C-l" 'org-insert-link)
(org-defkey org-mode-map "\C-c\M-l" 'org-insert-last-stored-link)
(org-defkey org-mode-map "\C-c\C-\M-l" 'org-insert-all-links)
(org-defkey org-mode-map "\C-c\C-o" 'org-open-at-point)
(org-defkey org-mode-map "\C-c%"    'org-mark-ring-push)
(org-defkey org-mode-map "\C-c&"    'org-mark-ring-goto)
(org-defkey org-mode-map "\C-c\C-z" 'org-add-note)  ; Alternative binding
(org-defkey org-mode-map "\C-c."    'org-time-stamp)  ; Minor-mode reserved
(org-defkey org-mode-map "\C-c!"    'org-time-stamp-inactive) ; Minor-mode r.
(org-defkey org-mode-map "\C-c,"    'org-priority)    ; Minor-mode reserved
(org-defkey org-mode-map "\C-c\C-y" 'org-evaluate-time-range)
(org-defkey org-mode-map "\C-c>"    'org-goto-calendar)
(org-defkey org-mode-map "\C-c<"    'org-date-from-calendar)
(org-defkey org-mode-map [(control ?,)]     'org-cycle-agenda-files)
(org-defkey org-mode-map [(control ?\')]     'org-cycle-agenda-files)
(org-defkey org-mode-map "\C-c["    'org-agenda-file-to-front)
(org-defkey org-mode-map "\C-c]"    'org-remove-file)
(org-defkey org-mode-map "\C-c\C-x<" 'org-agenda-set-restriction-lock)
(org-defkey org-mode-map "\C-c\C-x>" 'org-agenda-remove-restriction-lock)
(org-defkey org-mode-map "\C-c-"    'org-ctrl-c-minus)
(org-defkey org-mode-map "\C-c*"    'org-ctrl-c-star)
(org-defkey org-mode-map "\C-c^"    'org-sort)
(org-defkey org-mode-map "\C-c\C-c" 'org-ctrl-c-ctrl-c)
(org-defkey org-mode-map "\C-c\C-k" 'org-kill-note-or-show-branches)
(org-defkey org-mode-map "\C-c#"    'org-update-statistics-cookies)
(org-defkey org-mode-map [remap open-line] 'org-open-line)
(org-defkey org-mode-map [remap comment-dwim] 'org-comment-dwim)
(org-defkey org-mode-map [remap forward-paragraph] 'org-forward-paragraph)
(org-defkey org-mode-map [remap backward-paragraph] 'org-backward-paragraph)
(org-defkey org-mode-map "\M-^"     'org-delete-indentation)
(org-defkey org-mode-map "\C-m"     'org-return)
(org-defkey org-mode-map "\C-j"     'org-return-indent)
(org-defkey org-mode-map "\C-c?"    'org-table-field-info)
(org-defkey org-mode-map "\C-c "    'org-table-blank-field)
(org-defkey org-mode-map "\C-c+"    'org-table-sum)
(org-defkey org-mode-map "\C-c="    'org-table-eval-formula)
(org-defkey org-mode-map "\C-c'"    'org-edit-special)
(org-defkey org-mode-map "\C-c`"    'org-table-edit-field)
(org-defkey org-mode-map "\C-c\"a"  'orgtbl-ascii-plot)
(org-defkey org-mode-map "\C-c\"g"  'org-plot/gnuplot)
(org-defkey org-mode-map "\C-c|"    'org-table-create-or-convert-from-region)
(org-defkey org-mode-map [(control ?#)] 'org-table-rotate-recalc-marks)
(org-defkey org-mode-map "\C-c~"    'org-table-create-with-table.el)
(org-defkey org-mode-map "\C-c\C-a" 'org-attach)
(org-defkey org-mode-map "\C-c}"    'org-table-toggle-coordinate-overlays)
(org-defkey org-mode-map "\C-c{"    'org-table-toggle-formula-debugger)
(org-defkey org-mode-map "\C-c\C-e" 'org-export-dispatch)
(org-defkey org-mode-map "\C-c:"    'org-toggle-fixed-width)
(org-defkey org-mode-map "\C-c\C-x\C-f" 'org-emphasize)
(org-defkey org-mode-map "\C-c\C-xf"    'org-footnote-action)
(org-defkey org-mode-map "\C-c\C-x\C-mg"    'org-mobile-pull)
(org-defkey org-mode-map "\C-c\C-x\C-mp"    'org-mobile-push)
(org-defkey org-mode-map "\C-c@" 'org-mark-subtree)
(org-defkey org-mode-map "\M-h" 'org-mark-element)
(org-defkey org-mode-map [?\C-c (control ?*)] 'org-list-make-subtree)
;;(org-defkey org-mode-map [?\C-c (control ?-)] 'org-list-make-list-from-subtree)

(org-defkey org-mode-map "\C-c\C-x\C-w" 'org-cut-special)
(org-defkey org-mode-map "\C-c\C-x\M-w" 'org-copy-special)
(org-defkey org-mode-map "\C-c\C-x\C-y" 'org-paste-special)

(org-defkey org-mode-map "\C-c\C-x\C-t" 'org-toggle-time-stamp-overlays)
(org-defkey org-mode-map "\C-c\C-x\C-i" 'org-clock-in)
(org-defkey org-mode-map "\C-c\C-x\C-x" 'org-clock-in-last)
(org-defkey org-mode-map "\C-c\C-x\C-z" 'org-resolve-clocks)
(org-defkey org-mode-map "\C-c\C-x\C-o" 'org-clock-out)
(org-defkey org-mode-map "\C-c\C-x\C-j" 'org-clock-goto)
(org-defkey org-mode-map "\C-c\C-x\C-q" 'org-clock-cancel)
(org-defkey org-mode-map "\C-c\C-x\C-d" 'org-clock-display)
(org-defkey org-mode-map "\C-c\C-x\C-r" 'org-clock-report)
(org-defkey org-mode-map "\C-c\C-x\C-u" 'org-dblock-update)
(org-defkey org-mode-map "\C-c\C-x\C-l" 'org-toggle-latex-fragment)
(org-defkey org-mode-map "\C-c\C-x\C-v" 'org-toggle-inline-images)
(org-defkey org-mode-map "\C-c\C-x\C-\M-v" 'org-redisplay-inline-images)
(org-defkey org-mode-map "\C-c\C-x\\"   'org-toggle-pretty-entities)
(org-defkey org-mode-map "\C-c\C-x\C-b" 'org-toggle-checkbox)
(org-defkey org-mode-map "\C-c\C-xp"    'org-set-property)
(org-defkey org-mode-map "\C-c\C-xP"    'org-set-property-and-value)
(org-defkey org-mode-map "\C-c\C-xe"    'org-set-effort)
(org-defkey org-mode-map "\C-c\C-xE"    'org-inc-effort)
(org-defkey org-mode-map "\C-c\C-xo"    'org-toggle-ordered-property)
(org-defkey org-mode-map "\C-c\C-xi"    'org-columns-insert-dblock)
(org-defkey org-mode-map [(control ?c) (control ?x) ?\;] 'org-timer-set-timer)

(org-defkey org-mode-map "\C-c\C-x."    'org-timer)
(org-defkey org-mode-map "\C-c\C-x-"    'org-timer-item)
(org-defkey org-mode-map "\C-c\C-x0"    'org-timer-start)
(org-defkey org-mode-map "\C-c\C-x_"    'org-timer-stop)
(org-defkey org-mode-map "\C-c\C-x,"    'org-timer-pause-or-continue)

(define-key org-mode-map "\C-c\C-x\C-c" 'org-columns)

(define-key org-mode-map "\C-c\C-x!" 'org-reload)

(define-key org-mode-map "\C-c\C-xg" 'org-feed-update-all)
(define-key org-mode-map "\C-c\C-xG" 'org-feed-goto-inbox)

(define-key org-mode-map "\C-c\C-x[" 'org-reftex-citation)


(defconst org-speed-commands-default
  '(
    ("Outline Navigation")
    ("n" . (org-speed-move-safe 'org-next-visible-heading))
    ("p" . (org-speed-move-safe 'org-previous-visible-heading))
    ("f" . (org-speed-move-safe 'org-forward-heading-same-level))
    ("b" . (org-speed-move-safe 'org-backward-heading-same-level))
    ("F" . org-next-block)
    ("B" . org-previous-block)
    ("u" . (org-speed-move-safe 'outline-up-heading))
    ("j" . org-goto)
    ("g" . (org-refile t))
    ("Outline Visibility")
    ("c" . org-cycle)
    ("C" . org-shifttab)
    (" " . org-display-outline-path)
    ("s" . org-narrow-to-subtree)
    ("=" . org-columns)
    ("Outline Structure Editing")
    ("U" . org-metaup)
    ("D" . org-metadown)
    ("r" . org-metaright)
    ("l" . org-metaleft)
    ("R" . org-shiftmetaright)
    ("L" . org-shiftmetaleft)
    ("i" . (progn (forward-char 1) (call-interactively
				    'org-insert-heading-respect-content)))
    ("^" . org-sort)
    ("w" . org-refile)
    ("a" . org-archive-subtree-default-with-confirmation)
    ("@" . org-mark-subtree)
    ("#" . org-toggle-comment)
    ("Clock Commands")
    ("I" . org-clock-in)
    ("O" . org-clock-out)
    ("Meta Data Editing")
    ("t" . org-todo)
    ("," . (org-priority))
    ("0" . (org-priority ?\ ))
    ("1" . (org-priority ?A))
    ("2" . (org-priority ?B))
    ("3" . (org-priority ?C))
    (":" . org-set-tags-command)
    ("e" . org-set-effort)
    ("E" . org-inc-effort)
    ("W" . (lambda(m) (interactive "sMinutes before warning: ")
	     (org-entry-put (point) "APPT_WARNTIME" m)))
    ("Agenda Views etc")
    ("v" . org-agenda)
    ("/" . org-sparse-tree)
    ("Misc")
    ("o" . org-open-at-point)
    ("?" . org-speed-command-help)
    ("<" . (org-agenda-set-restriction-lock 'subtree))
    (">" . (org-agenda-remove-restriction-lock))
    )
  "The default speed commands.")

(defun org-print-speed-command (e)
  (if (> (length (car e)) 1)
      (progn
	(princ "\n")
	(princ (car e))
	(princ "\n")
	(princ (make-string (length (car e)) ?-))
	(princ "\n"))
    (princ (car e))
    (princ "   ")
    (if (symbolp (cdr e))
	(princ (symbol-name (cdr e)))
      (prin1 (cdr e)))
    (princ "\n")))

(defun org-speed-command-help ()
  "Show the available speed commands."
  (interactive)
  (if (not org-use-speed-commands)
      (user-error "Speed commands are not activated, customize `org-use-speed-commands'")
    (with-output-to-temp-buffer "*Help*"
      (princ "User-defined Speed commands\n===========================\n")
      (mapc #'org-print-speed-command org-speed-commands-user)
      (princ "\n")
      (princ "Built-in Speed commands\n=======================\n")
      (mapc #'org-print-speed-command org-speed-commands-default))
    (with-current-buffer "*Help*"
      (setq truncate-lines t))))

(defun org-speed-move-safe (cmd)
  "Execute CMD, but make sure that the cursor always ends up in a headline.
If not, return to the original position and throw an error."
  (interactive)
  (let ((pos (point)))
    (call-interactively cmd)
    (unless (and (bolp) (org-at-heading-p))
      (goto-char pos)
      (error "Boundary reached while executing %s" cmd))))

(defvar org-self-insert-command-undo-counter 0)

(defvar org-table-auto-blank-field) ; defined in org-table.el
(defvar org-speed-command nil)

(defun org-speed-command-activate (keys)
  "Hook for activating single-letter speed commands.
`org-speed-commands-default' specifies a minimal command set.
Use `org-speed-commands-user' for further customization."
  (when (or (and (bolp) (looking-at org-outline-regexp))
	    (and (functionp org-use-speed-commands)
		 (funcall org-use-speed-commands)))
    (cdr (assoc keys (append org-speed-commands-user
			     org-speed-commands-default)))))

(defun org-babel-speed-command-activate (keys)
  "Hook for activating single-letter code block commands."
  (when (and (bolp) (looking-at org-babel-src-block-regexp))
    (cdr (assoc keys org-babel-key-bindings))))

(defcustom org-speed-command-hook
  '(org-speed-command-activate org-babel-speed-command-activate)
  "Hook for activating speed commands at strategic locations.
Hook functions are called in sequence until a valid handler is
found.

Each hook takes a single argument, a user-pressed command key
which is also a `self-insert-command' from the global map.

Within the hook, examine the cursor position and the command key
and return nil or a valid handler as appropriate.  Handler could
be one of an interactive command, a function, or a form.

Set `org-use-speed-commands' to non-nil value to enable this
hook.  The default setting is `org-speed-command-activate'."
  :group 'org-structure
  :version "24.1"
  :type 'hook)

(defun org-self-insert-command (N)
  "Like `self-insert-command', use overwrite-mode for whitespace in tables.
If the cursor is in a table looking at whitespace, the whitespace is
overwritten, and the table is not marked as requiring realignment."
  (interactive "p")
  (org-check-before-invisible-edit 'insert)
  (cond
   ((and org-use-speed-commands
	 (let ((kv (this-command-keys-vector)))
	   (setq org-speed-command
		 (run-hook-with-args-until-success
		  'org-speed-command-hook
		  (make-string 1 (aref kv (1- (length kv))))))))
    (cond
     ((commandp org-speed-command)
      (setq this-command org-speed-command)
      (call-interactively org-speed-command))
     ((functionp org-speed-command)
      (funcall org-speed-command))
     ((and org-speed-command (listp org-speed-command))
      (eval org-speed-command))
     (t (let (org-use-speed-commands)
	  (call-interactively 'org-self-insert-command)))))
   ((and
     (org-at-table-p)
     (eq N 1)
     (not (org-region-active-p))
     (progn
       ;; Check if we blank the field, and if that triggers align.
       (and (featurep 'org-table) org-table-auto-blank-field
	    (memq last-command
		  '(org-cycle org-return org-shifttab org-ctrl-c-ctrl-c))
	    (if (or (eq (char-after) ?\s) (looking-at "[^|\n]*  |"))
		;; Got extra space, this field does not determine
		;; column width.
		(let (org-table-may-need-update) (org-table-blank-field))
	      ;; No extra space, this field may determine column
	      ;; width.
	      (org-table-blank-field)))
       t)
     (looking-at "[^|\n]* \\( \\)|"))
    ;; There is room for insertion without re-aligning the table.
    (delete-region (match-beginning 1) (match-end 1))
    (self-insert-command N))
   (t
    (setq org-table-may-need-update t)
    (self-insert-command N)
    (org-fix-tags-on-the-fly)
    (when org-self-insert-cluster-for-undo
      (if (not (eq last-command 'org-self-insert-command))
	  (setq org-self-insert-command-undo-counter 1)
	(if (>= org-self-insert-command-undo-counter 20)
	    (setq org-self-insert-command-undo-counter 1)
	  (and (> org-self-insert-command-undo-counter 0)
	       buffer-undo-list (listp buffer-undo-list)
	       (not (cadr buffer-undo-list)) ; remove nil entry
	       (setcdr buffer-undo-list (cddr buffer-undo-list)))
	  (setq org-self-insert-command-undo-counter
		(1+ org-self-insert-command-undo-counter))))))))

(defun org-check-before-invisible-edit (kind)
  "Check is editing if kind KIND would be dangerous with invisible text around.
The detailed reaction depends on the user option `org-catch-invisible-edits'."
  ;; First, try to get out of here as quickly as possible, to reduce overhead
  (when (and org-catch-invisible-edits
	     (or (not (boundp 'visible-mode)) (not visible-mode))
	     (or (get-char-property (point) 'invisible)
		 (get-char-property (max (point-min) (1- (point))) 'invisible)))
    ;; OK, we need to take a closer look.  Do not consider
    ;; invisibility obtained through text properties (e.g., link
    ;; fontification), as it cannot be toggled.
    (let* ((invisible-at-point
	    (pcase (get-char-property-and-overlay (point) 'invisible)
	      (`(,_ . ,(and (pred overlayp) o)) o)))
	   ;; Assume that point cannot land in the middle of an
	   ;; overlay, or between two overlays.
	   (invisible-before-point
	    (and (not invisible-at-point)
		 (not (bobp))
		 (pcase (get-char-property-and-overlay (1- (point)) 'invisible)
		   (`(,_ . ,(and (pred overlayp) o)) o))))
	   (border-and-ok-direction
	    (or
	     ;; Check if we are acting predictably before invisible
	     ;; text.
	     (and invisible-at-point
		  (memq kind '(insert delete-backward)))
	     ;; Check if we are acting predictably after invisible text
	     ;; This works not well, and I have turned it off.  It seems
	     ;; better to always show and stop after invisible text.
	     ;; (and (not invisible-at-point) invisible-before-point
	     ;;  (memq kind '(insert delete)))
	     )))
      (when (or invisible-at-point invisible-before-point)
	(when (eq org-catch-invisible-edits 'error)
	  (user-error "Editing in invisible areas is prohibited, make them visible first"))
	(if (and org-custom-properties-overlays
		 (y-or-n-p "Display invisible properties in this buffer? "))
	    (org-toggle-custom-properties-visibility)
	  ;; Make the area visible
	  (save-excursion
	    (when invisible-before-point
	      (goto-char
	       (previous-single-char-property-change (point) 'invisible)))
	    ;; Remove whatever overlay is currently making yet-to-be
	    ;; edited text invisible.  Also remove nested invisibility
	    ;; related overlays.
	    (delete-overlay (or invisible-at-point invisible-before-point))
	    (let ((origin (if invisible-at-point (point) (1- (point)))))
	      (while (pcase (get-char-property-and-overlay origin 'invisible)
		       (`(,_ . ,(and (pred overlayp) o))
			(delete-overlay o)
			t)))))
	  (cond
	   ((eq org-catch-invisible-edits 'show)
	    ;; That's it, we do the edit after showing
	    (message
	     "Unfolding invisible region around point before editing")
	    (sit-for 1))
	   ((and (eq org-catch-invisible-edits 'smart)
		 border-and-ok-direction)
	    (message "Unfolding invisible region around point before editing"))
	   (t
	    ;; Don't do the edit, make the user repeat it in full visibility
	    (user-error "Edit in invisible region aborted, repeat to confirm with text visible"))))))))

(defun org-fix-tags-on-the-fly ()
  "Align tags in headline at point.
Unlike to `org-set-tags', it ignores region and sorting."
  (when (and (eq (char-after (line-beginning-position)) ?*) ;short-circuit
	     (org-at-heading-p))
    (let ((org-ignore-region t)
	  (org-tags-sort-function nil))
      (org-set-tags nil t))))

(defun org-delete-backward-char (N)
  "Like `delete-backward-char', insert whitespace at field end in tables.
When deleting backwards, in tables this function will insert whitespace in
front of the next \"|\" separator, to keep the table aligned.  The table will
still be marked for re-alignment if the field did fill the entire column,
because, in this case the deletion might narrow the column."
  (interactive "p")
  (save-match-data
    (org-check-before-invisible-edit 'delete-backward)
    (if (and (org-at-table-p)
	     (eq N 1)
	     (not (org-region-active-p))
	     (string-match "|" (buffer-substring (point-at-bol) (point)))
	     (looking-at ".*?|"))
	(let ((pos (point))
	      (noalign (looking-at "[^|\n\r]*  |"))
	      (c org-table-may-need-update))
	  (backward-delete-char N)
	  (unless overwrite-mode
	    (skip-chars-forward "^|")
	    (insert " ")
	    (goto-char (1- pos)))
	  ;; noalign: if there were two spaces at the end, this field
	  ;; does not determine the width of the column.
	  (when noalign (setq org-table-may-need-update c)))
      (backward-delete-char N)
      (org-fix-tags-on-the-fly))))

(defun org-delete-char (N)
  "Like `delete-char', but insert whitespace at field end in tables.
When deleting characters, in tables this function will insert whitespace in
front of the next \"|\" separator, to keep the table aligned.  The table will
still be marked for re-alignment if the field did fill the entire column,
because, in this case the deletion might narrow the column."
  (interactive "p")
  (save-match-data
    (org-check-before-invisible-edit 'delete)
    (if (and (org-at-table-p)
	     (not (bolp))
	     (not (= (char-after) ?|))
	     (eq N 1))
	(if (looking-at ".*?|")
	    (let ((pos (point))
		  (noalign (looking-at "[^|\n\r]*  |"))
		  (c org-table-may-need-update))
	      (replace-match
	       (concat (substring (match-string 0) 1 -1) " |") nil t)
	      (goto-char pos)
	      ;; noalign: if there were two spaces at the end, this field
	      ;; does not determine the width of the column.
	      (when noalign (setq org-table-may-need-update c)))
	  (delete-char N))
      (delete-char N)
      (org-fix-tags-on-the-fly))))

;; Make `delete-selection-mode' work with Org mode and Orgtbl mode
(put 'org-self-insert-command 'delete-selection
     (lambda ()
       (not (run-hook-with-args-until-success
             'self-insert-uses-region-functions))))
(put 'orgtbl-self-insert-command 'delete-selection
     (lambda ()
       (not (run-hook-with-args-until-success
             'self-insert-uses-region-functions))))
(put 'org-delete-char 'delete-selection 'supersede)
(put 'org-delete-backward-char 'delete-selection 'supersede)
(put 'org-yank 'delete-selection 'yank)

;; Make `flyspell-mode' delay after some commands
(put 'org-self-insert-command 'flyspell-delayed t)
(put 'orgtbl-self-insert-command 'flyspell-delayed t)
(put 'org-delete-char 'flyspell-delayed t)
(put 'org-delete-backward-char 'flyspell-delayed t)

;; Make pabbrev-mode expand after Org mode commands
(put 'org-self-insert-command 'pabbrev-expand-after-command t)
(put 'orgtbl-self-insert-command 'pabbrev-expand-after-command t)

(defun org-transpose-words ()
  "Transpose words for Org.
This uses the `org-mode-transpose-word-syntax-table' syntax
table, which interprets characters in `org-emphasis-alist' as
word constituents."
  (interactive)
  (with-syntax-table org-mode-transpose-word-syntax-table
    (call-interactively 'transpose-words)))
(org-remap org-mode-map 'transpose-words 'org-transpose-words)

(defvar org-ctrl-c-ctrl-c-hook nil
  "Hook for functions attaching themselves to `C-c C-c'.

This can be used to add additional functionality to the C-c C-c
key which executes context-dependent commands.  This hook is run
before any other test, while `org-ctrl-c-ctrl-c-final-hook' is
run after the last test.

Each function will be called with no arguments.  The function
must check if the context is appropriate for it to act.  If yes,
it should do its thing and then return a non-nil value.  If the
context is wrong, just do nothing and return nil.")

(defvar org-ctrl-c-ctrl-c-final-hook nil
  "Hook for functions attaching themselves to `C-c C-c'.

This can be used to add additional functionality to the C-c C-c
key which executes context-dependent commands.  This hook is run
after any other test, while `org-ctrl-c-ctrl-c-hook' is run
before the first test.

Each function will be called with no arguments.  The function
must check if the context is appropriate for it to act.  If yes,
it should do its thing and then return a non-nil value.  If the
context is wrong, just do nothing and return nil.")

(defvar org-tab-first-hook nil
  "Hook for functions to attach themselves to TAB.
See `org-ctrl-c-ctrl-c-hook' for more information.
This hook runs as the first action when TAB is pressed, even before
`org-cycle' messes around with the `outline-regexp' to cater for
inline tasks and plain list item folding.
If any function in this hook returns t, any other actions that
would have been caused by TAB (such as table field motion or visibility
cycling) will not occur.")

(defvar org-tab-after-check-for-table-hook nil
  "Hook for functions to attach themselves to TAB.
See `org-ctrl-c-ctrl-c-hook' for more information.
This hook runs after it has been established that the cursor is not in a
table, but before checking if the cursor is in a headline or if global cycling
should be done.
If any function in this hook returns t, not other actions like visibility
cycling will be done.")

(defvar org-tab-after-check-for-cycling-hook nil
  "Hook for functions to attach themselves to TAB.
See `org-ctrl-c-ctrl-c-hook' for more information.
This hook runs after it has been established that not table field motion and
not visibility should be done because of current context.  This is probably
the place where a package like yasnippets can hook in.")

(defvar org-tab-before-tab-emulation-hook nil
  "Hook for functions to attach themselves to TAB.
See `org-ctrl-c-ctrl-c-hook' for more information.
This hook runs after every other options for TAB have been exhausted, but
before indentation and \t insertion takes place.")

(defvar org-metaleft-hook nil
  "Hook for functions attaching themselves to `M-left'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metaright-hook nil
  "Hook for functions attaching themselves to `M-right'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metaup-hook nil
  "Hook for functions attaching themselves to `M-up'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metadown-hook nil
  "Hook for functions attaching themselves to `M-down'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetaleft-hook nil
  "Hook for functions attaching themselves to `M-S-left'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetaright-hook nil
  "Hook for functions attaching themselves to `M-S-right'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetaup-hook nil
  "Hook for functions attaching themselves to `M-S-up'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetadown-hook nil
  "Hook for functions attaching themselves to `M-S-down'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metareturn-hook nil
  "Hook for functions attaching themselves to `M-RET'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftup-hook nil
  "Hook for functions attaching themselves to `S-up'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftup-final-hook nil
  "Hook for functions attaching themselves to `S-up'.
This one runs after all other options except shift-select have been excluded.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftdown-hook nil
  "Hook for functions attaching themselves to `S-down'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftdown-final-hook nil
  "Hook for functions attaching themselves to `S-down'.
This one runs after all other options except shift-select have been excluded.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftleft-hook nil
  "Hook for functions attaching themselves to `S-left'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftleft-final-hook nil
  "Hook for functions attaching themselves to `S-left'.
This one runs after all other options except shift-select have been excluded.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftright-hook nil
  "Hook for functions attaching themselves to `S-right'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftright-final-hook nil
  "Hook for functions attaching themselves to `S-right'.
This one runs after all other options except shift-select have been excluded.
See `org-ctrl-c-ctrl-c-hook' for more information.")

(defun org-modifier-cursor-error ()
  "Throw an error, a modified cursor command was applied in wrong context."
  (user-error "This command is active in special context like tables, headlines or items"))

(defun org-shiftselect-error ()
  "Throw an error because Shift-Cursor command was applied in wrong context."
  (if (and (boundp 'shift-select-mode) shift-select-mode)
      (user-error "To use shift-selection with Org mode, customize `org-support-shift-select'")
    (user-error "This command works only in special context like headlines or timestamps")))

(defun org-call-for-shift-select (cmd)
  (let ((this-command-keys-shift-translated t))
    (call-interactively cmd)))

(defun org-shifttab (&optional arg)
  "Global visibility cycling or move to previous table field.
Call `org-table-previous-field' within a table.
When ARG is nil, cycle globally through visibility states.
When ARG is a numeric prefix, show contents of this level."
  (interactive "P")
  (cond
   ((org-at-table-p) (call-interactively 'org-table-previous-field))
   ((integerp arg)
    (let ((arg2 (if org-odd-levels-only (1- (* 2 arg)) arg)))
      (message "Content view to level: %d" arg)
      (org-content (prefix-numeric-value arg2))
      (org-cycle-show-empty-lines t)
      (setq org-cycle-global-status 'overview)))
   (t (call-interactively 'org-global-cycle))))

(defun org-shiftmetaleft ()
  "Promote subtree or delete table column.
Calls `org-promote-subtree', `org-outdent-item-tree', or
`org-table-delete-column', depending on context.  See the
individual commands for more information."
  (interactive)
  (cond
   ((run-hook-with-args-until-success 'org-shiftmetaleft-hook))
   ((org-at-table-p) (call-interactively 'org-table-delete-column))
   ((org-at-heading-p) (call-interactively 'org-promote-subtree))
   ((if (not (org-region-active-p)) (org-at-item-p)
      (save-excursion (goto-char (region-beginning))
		      (org-at-item-p)))
    (call-interactively 'org-outdent-item-tree))
   (t (org-modifier-cursor-error))))

(defun org-shiftmetaright ()
  "Demote subtree or insert table column.
Calls `org-demote-subtree', `org-indent-item-tree', or
`org-table-insert-column', depending on context.  See the
individual commands for more information."
  (interactive)
  (cond
   ((run-hook-with-args-until-success 'org-shiftmetaright-hook))
   ((org-at-table-p) (call-interactively 'org-table-insert-column))
   ((org-at-heading-p) (call-interactively 'org-demote-subtree))
   ((if (not (org-region-active-p)) (org-at-item-p)
      (save-excursion (goto-char (region-beginning))
		      (org-at-item-p)))
    (call-interactively 'org-indent-item-tree))
   (t (org-modifier-cursor-error))))

(defun org-shiftmetaup (&optional _arg)
  "Drag the line at point up.
In a table, kill the current row.
On a clock timestamp, update the value of the timestamp like `S-<up>'
but also adjust the previous clocked item in the clock history.
Everywhere else, drag the line at point up."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftmetaup-hook))
   ((org-at-table-p) (call-interactively 'org-table-kill-row))
   ((org-at-clock-log-p) (let ((org-clock-adjust-closest t))
			   (call-interactively 'org-timestamp-up)))
   (t (call-interactively 'org-drag-line-backward))))

(defun org-shiftmetadown (&optional _arg)
  "Drag the line at point down.
In a table, insert an empty row at the current line.
On a clock timestamp, update the value of the timestamp like `S-<down>'
but also adjust the previous clocked item in the clock history.
Everywhere else, drag the line at point down."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftmetadown-hook))
   ((org-at-table-p) (call-interactively 'org-table-insert-row))
   ((org-at-clock-log-p) (let ((org-clock-adjust-closest t))
			   (call-interactively 'org-timestamp-down)))
   (t (call-interactively 'org-drag-line-forward))))

(defsubst org-hidden-tree-error ()
  (user-error
   "Hidden subtree, open with TAB or use subtree command M-S-<left>/<right>"))

(defun org-metaleft (&optional _arg)
  "Promote heading, list item at point or move table column left.

Calls `org-do-promote', `org-outdent-item' or `org-table-move-column',
depending on context.  With no specific context, calls the Emacs
default `backward-word'.  See the individual commands for more
information.

This function runs the hook `org-metaleft-hook' as a first step,
and returns at first non-nil value."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-metaleft-hook))
   ((org-at-table-p) (org-call-with-arg 'org-table-move-column 'left))
   ((org-with-limited-levels
     (or (org-at-heading-p)
	 (and (org-region-active-p)
	      (save-excursion
		(goto-char (region-beginning))
		(org-at-heading-p)))))
    (when (org-check-for-hidden 'headlines) (org-hidden-tree-error))
    (call-interactively 'org-do-promote))
   ;; At an inline task.
   ((org-at-heading-p)
    (call-interactively 'org-inlinetask-promote))
   ((or (org-at-item-p)
	(and (org-region-active-p)
	     (save-excursion
	       (goto-char (region-beginning))
	       (org-at-item-p))))
    (when (org-check-for-hidden 'items) (org-hidden-tree-error))
    (call-interactively 'org-outdent-item))
   (t (call-interactively 'backward-word))))

(defun org-metaright (&optional _arg)
  "Demote heading, list item at point or move table column right.

In front of a drawer or a block keyword, indent it correctly.

Calls `org-do-demote', `org-indent-item', `org-table-move-column',
`org-indent-drawer' or `org-indent-block' depending on context.
With no specific context, calls the Emacs default `forward-word'.
See the individual commands for more information.

This function runs the hook `org-metaright-hook' as a first step,
and returns at first non-nil value."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-metaright-hook))
   ((org-at-table-p) (call-interactively 'org-table-move-column))
   ((org-at-drawer-p) (call-interactively 'org-indent-drawer))
   ((org-at-block-p) (call-interactively 'org-indent-block))
   ((org-with-limited-levels
     (or (org-at-heading-p)
	 (and (org-region-active-p)
	      (save-excursion
		(goto-char (region-beginning))
		(org-at-heading-p)))))
    (when (org-check-for-hidden 'headlines) (org-hidden-tree-error))
    (call-interactively 'org-do-demote))
   ;; At an inline task.
   ((org-at-heading-p)
    (call-interactively 'org-inlinetask-demote))
   ((or (org-at-item-p)
	(and (org-region-active-p)
	     (save-excursion
	       (goto-char (region-beginning))
	       (org-at-item-p))))
    (when (org-check-for-hidden 'items) (org-hidden-tree-error))
    (call-interactively 'org-indent-item))
   (t (call-interactively 'forward-word))))

(defun org-check-for-hidden (what)
  "Check if there are hidden headlines/items in the current visual line.
WHAT can be either `headlines' or `items'.  If the current line is
an outline or item heading and it has a folded subtree below it,
this function returns t, nil otherwise."
  (let ((re (cond
	     ((eq what 'headlines) org-outline-regexp-bol)
	     ((eq what 'items) (org-item-beginning-re))
	     (t (error "This should not happen"))))
	beg end)
    (save-excursion
      (catch 'exit
	(unless (org-region-active-p)
	  (setq beg (point-at-bol))
	  (beginning-of-line 2)
	  (while (and (not (eobp)) ;; this is like `next-line'
		      (get-char-property (1- (point)) 'invisible))
	    (beginning-of-line 2))
	  (setq end (point))
	  (goto-char beg)
	  (goto-char (point-at-eol))
	  (setq end (max end (point)))
	  (while (re-search-forward re end t)
	    (when (get-char-property (match-beginning 0) 'invisible)
	      (throw 'exit t))))
	nil))))

(defun org-metaup (&optional _arg)
  "Move subtree up or move table row up.
Calls `org-move-subtree-up' or `org-table-move-row' or
`org-move-item-up', depending on context.  See the individual commands
for more information."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-metaup-hook))
   ((org-region-active-p)
    (let* ((a (min (region-beginning) (region-end)))
	   (b (1- (max (region-beginning) (region-end))))
	   (c (save-excursion (goto-char a)
			      (move-beginning-of-line 0)))
	   (d (save-excursion (goto-char a)
			      (move-end-of-line 0) (point))))
      (transpose-regions a b c d)
      (goto-char c)))
   ((org-at-table-p) (org-call-with-arg 'org-table-move-row 'up))
   ((org-at-heading-p) (call-interactively 'org-move-subtree-up))
   ((org-at-item-p) (call-interactively 'org-move-item-up))
   (t (org-drag-element-backward))))

(defun org-metadown (&optional _arg)
  "Move subtree down or move table row down.
Calls `org-move-subtree-down' or `org-table-move-row' or
`org-move-item-down', depending on context.  See the individual
commands for more information."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-metadown-hook))
   ((org-region-active-p)
    (let* ((a (min (region-beginning) (region-end)))
	   (b (max (region-beginning) (region-end)))
	   (c (save-excursion (goto-char b)
			      (move-beginning-of-line 1)))
	   (d (save-excursion (goto-char b)
			      (move-end-of-line 1) (1+ (point)))))
      (transpose-regions a b c d)
      (goto-char d)))
   ((org-at-table-p) (call-interactively 'org-table-move-row))
   ((org-at-heading-p) (call-interactively 'org-move-subtree-down))
   ((org-at-item-p) (call-interactively 'org-move-item-down))
   (t (org-drag-element-forward))))

(defun org-shiftup (&optional arg)
  "Increase item in timestamp or increase priority of current headline.
Calls `org-timestamp-up' or `org-priority-up', or `org-previous-item',
depending on context.  See the individual commands for more information."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftup-hook))
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'previous-line))
   ((org-at-timestamp-p 'lax)
    (call-interactively (if org-edit-timestamp-down-means-later
			    'org-timestamp-down 'org-timestamp-up)))
   ((and (not (eq org-support-shift-select 'always))
	 org-enable-priority-commands
	 (org-at-heading-p))
    (call-interactively 'org-priority-up))
   ((and (not org-support-shift-select) (org-at-item-p))
    (call-interactively 'org-previous-item))
   ((org-clocktable-try-shift 'up arg))
   ((run-hook-with-args-until-success 'org-shiftup-final-hook))
   (org-support-shift-select
    (org-call-for-shift-select 'previous-line))
   (t (org-shiftselect-error))))

(defun org-shiftdown (&optional arg)
  "Decrease item in timestamp or decrease priority of current headline.
Calls `org-timestamp-down' or `org-priority-down', or `org-next-item'
depending on context.  See the individual commands for more information."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftdown-hook))
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'next-line))
   ((org-at-timestamp-p 'lax)
    (call-interactively (if org-edit-timestamp-down-means-later
			    'org-timestamp-up 'org-timestamp-down)))
   ((and (not (eq org-support-shift-select 'always))
	 org-enable-priority-commands
	 (org-at-heading-p))
    (call-interactively 'org-priority-down))
   ((and (not org-support-shift-select) (org-at-item-p))
    (call-interactively 'org-next-item))
   ((org-clocktable-try-shift 'down arg))
   ((run-hook-with-args-until-success 'org-shiftdown-final-hook))
   (org-support-shift-select
    (org-call-for-shift-select 'next-line))
   (t (org-shiftselect-error))))

(defun org-shiftright (&optional arg)
  "Cycle the thing at point or in the current line, depending on context.
Depending on context, this does one of the following:

- switch a timestamp at point one day into the future
- on a headline, switch to the next TODO keyword.
- on an item, switch entire list to the next bullet type
- on a property line, switch to the next allowed value
- on a clocktable definition line, move time block into the future"
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftright-hook))
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'forward-char))
   ((org-at-timestamp-p 'lax) (call-interactively 'org-timestamp-up-day))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-heading-p))
    (let ((org-inhibit-logging
	   (not org-treat-S-cursor-todo-selection-as-state-change))
	  (org-inhibit-blocking
	   (not org-treat-S-cursor-todo-selection-as-state-change)))
      (org-call-with-arg 'org-todo 'right)))
   ((or (and org-support-shift-select
	     (not (eq org-support-shift-select 'always))
	     (org-at-item-bullet-p))
	(and (not org-support-shift-select) (org-at-item-p)))
    (org-call-with-arg 'org-cycle-list-bullet nil))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-property-p))
    (call-interactively 'org-property-next-allowed-value))
   ((org-clocktable-try-shift 'right arg))
   ((run-hook-with-args-until-success 'org-shiftright-final-hook))
   (org-support-shift-select
    (org-call-for-shift-select 'forward-char))
   (t (org-shiftselect-error))))

(defun org-shiftleft (&optional arg)
  "Cycle the thing at point or in the current line, depending on context.
Depending on context, this does one of the following:

- switch a timestamp at point one day into the past
- on a headline, switch to the previous TODO keyword.
- on an item, switch entire list to the previous bullet type
- on a property line, switch to the previous allowed value
- on a clocktable definition line, move time block into the past"
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftleft-hook))
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'backward-char))
   ((org-at-timestamp-p 'lax) (call-interactively 'org-timestamp-down-day))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-heading-p))
    (let ((org-inhibit-logging
	   (not org-treat-S-cursor-todo-selection-as-state-change))
	  (org-inhibit-blocking
	   (not org-treat-S-cursor-todo-selection-as-state-change)))
      (org-call-with-arg 'org-todo 'left)))
   ((or (and org-support-shift-select
	     (not (eq org-support-shift-select 'always))
	     (org-at-item-bullet-p))
	(and (not org-support-shift-select) (org-at-item-p)))
    (org-call-with-arg 'org-cycle-list-bullet 'previous))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-property-p))
    (call-interactively 'org-property-previous-allowed-value))
   ((org-clocktable-try-shift 'left arg))
   ((run-hook-with-args-until-success 'org-shiftleft-final-hook))
   (org-support-shift-select
    (org-call-for-shift-select 'backward-char))
   (t (org-shiftselect-error))))

(defun org-shiftcontrolright ()
  "Switch to next TODO set."
  (interactive)
  (cond
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'forward-word))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-heading-p))
    (org-call-with-arg 'org-todo 'nextset))
   (org-support-shift-select
    (org-call-for-shift-select 'forward-word))
   (t (org-shiftselect-error))))

(defun org-shiftcontrolleft ()
  "Switch to previous TODO set."
  (interactive)
  (cond
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'backward-word))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-heading-p))
    (org-call-with-arg 'org-todo 'previousset))
   (org-support-shift-select
    (org-call-for-shift-select 'backward-word))
   (t (org-shiftselect-error))))

(defun org-shiftcontrolup (&optional n)
  "Change timestamps synchronously up in CLOCK log lines.
Optional argument N tells to change by that many units."
  (interactive "P")
  (if (and (org-at-clock-log-p) (org-at-timestamp-p 'lax))
      (let (org-support-shift-select)
	(org-clock-timestamps-up n))
    (user-error "Not at a clock log")))

(defun org-shiftcontroldown (&optional n)
  "Change timestamps synchronously down in CLOCK log lines.
Optional argument N tells to change by that many units."
  (interactive "P")
  (if (and (org-at-clock-log-p) (org-at-timestamp-p 'lax))
      (let (org-support-shift-select)
	(org-clock-timestamps-down n))
    (user-error "Not at a clock log")))

(defun org-increase-number-at-point (&optional inc)
  "Increment the number at point.
With an optional prefix numeric argument INC, increment using
this numeric value."
  (interactive "p")
  (if (not (number-at-point))
      (user-error "Not on a number")
    (unless inc (setq inc 1))
    (let ((pos (point))
	  (beg (skip-chars-backward "-+^/*0-9eE."))
	  (end (skip-chars-forward "-+^/*0-9eE^.")) nap)
      (setq nap (buffer-substring-no-properties
		 (+ pos beg) (+ pos beg end)))
      (delete-region (+ pos beg) (+ pos beg end))
      (insert (calc-eval (concat (number-to-string inc) "+" nap))))
    (when (org-at-table-p)
      (org-table-align)
      (org-table-end-of-field 1))))

(defun org-decrease-number-at-point (&optional inc)
  "Decrement the number at point.
With an optional prefix numeric argument INC, decrement using
this numeric value."
  (interactive "p")
  (org-increase-number-at-point (- (or inc 1))))

(defun org-ctrl-c-ret ()
  "Call `org-table-hline-and-move' or `org-insert-heading' dep. on context."
  (interactive)
  (cond
   ((org-at-table-p) (call-interactively 'org-table-hline-and-move))
   (t (call-interactively 'org-insert-heading))))

(defun org-find-visible ()
  (let ((s (point)))
    (while (and (not (= (point-max) (setq s (next-overlay-change s))))
		(get-char-property s 'invisible)))
    s))
(defun org-find-invisible ()
  (let ((s (point)))
    (while (and (not (= (point-max) (setq s (next-overlay-change s))))
		(not (get-char-property s 'invisible))))
    s))

(defun org-copy-visible (beg end)
  "Copy the visible parts of the region."
  (interactive "r")
  (let ((result ""))
    (while (/= beg end)
      (when (get-char-property beg 'invisible)
	(setq beg (next-single-char-property-change beg 'invisible nil end)))
      (let ((next (next-single-char-property-change beg 'invisible nil end)))
	(setq result (concat result (buffer-substring beg next)))
	(setq beg next)))
    (kill-new result)))

(defun org-copy-special ()
  "Copy region in table or copy current subtree.
Calls `org-table-copy-region' or `org-copy-subtree', depending on
context.  See the individual commands for more information."
  (interactive)
  (call-interactively
   (if (org-at-table-p) #'org-table-copy-region #'org-copy-subtree)))

(defun org-cut-special ()
  "Cut region in table or cut current subtree.
Calls `org-table-cut-region' or `org-cut-subtree', depending on
context.  See the individual commands for more information."
  (interactive)
  (call-interactively
   (if (org-at-table-p) #'org-table-cut-region #'org-cut-subtree)))

(defun org-paste-special (arg)
  "Paste rectangular region into table, or past subtree relative to level.
Calls `org-table-paste-rectangle' or `org-paste-subtree', depending on context.
See the individual commands for more information."
  (interactive "P")
  (if (org-at-table-p)
      (org-table-paste-rectangle)
    (org-paste-subtree arg)))

(defun org-edit-special (&optional arg)
  "Call a special editor for the element at point.
When at a table, call the formula editor with `org-table-edit-formulas'.
When in a source code block, call `org-edit-src-code'.
When in a fixed-width region, call `org-edit-fixed-width-region'.
When in an export block, call `org-edit-export-block'.
When in a LaTeX environment, call `org-edit-latex-environment'.
When at an #+INCLUDE keyword, visit the included file.
When at a footnote reference, call `org-edit-footnote-reference'
On a link, call `ffap' to visit the link at point.
Otherwise, return a user error."
  (interactive "P")
  (let ((element (org-element-at-point)))
    (barf-if-buffer-read-only)
    (pcase (org-element-type element)
      (`src-block
       (if (not arg) (org-edit-src-code)
	 (let* ((info (org-babel-get-src-block-info))
		(lang (nth 0 info))
		(params (nth 2 info))
		(session (cdr (assq :session params))))
	   (if (not session) (org-edit-src-code)
	     ;; At a src-block with a session and function called with
	     ;; an ARG: switch to the buffer related to the inferior
	     ;; process.
	     (switch-to-buffer
	      (funcall (intern (concat "org-babel-prep-session:" lang))
		       session params))))))
      (`keyword
       (if (member (org-element-property :key element) '("INCLUDE" "SETUPFILE"))
           (org-open-link-from-string
	    (format "[[%s]]"
		    (expand-file-name
		     (let ((value (org-element-property :value element)))
		       (cond ((org-file-url-p value)
			      (user-error "The file is specified as a URL, cannot be edited"))
			     ((not (org-string-nw-p value))
			      (user-error "No file to edit"))
			     ((string-match "\\`\"\\(.*?\\)\"" value)
			      (match-string 1 value))
			     ((string-match "\\`[^ \t\"]\\S-*" value)
			      (match-string 0 value))
			     (t (user-error "No valid file specified")))))))
         (user-error "No special environment to edit here")))
      (`table
       (if (eq (org-element-property :type element) 'table.el)
           (org-edit-table.el)
         (call-interactively 'org-table-edit-formulas)))
      ;; Only Org tables contain `table-row' type elements.
      (`table-row (call-interactively 'org-table-edit-formulas))
      (`example-block (org-edit-src-code))
      (`export-block (org-edit-export-block))
      (`fixed-width (org-edit-fixed-width-region))
      (`latex-environment (org-edit-latex-environment))
      (_
       ;; No notable element at point.  Though, we may be at a link or
       ;; a footnote reference, which are objects.  Thus, scan deeper.
       (let ((context (org-element-context element)))
	 (pcase (org-element-type context)
	   (`footnote-reference (org-edit-footnote-reference))
	   (`inline-src-block (org-edit-inline-src-code))
	   (`link (call-interactively #'ffap))
	   (_ (user-error "No special environment to edit here"))))))))

(defvar org-table-coordinate-overlays) ; defined in org-table.el
(defun org-ctrl-c-ctrl-c (&optional arg)
  "Set tags in headline, or update according to changed information at point.

This command does many different things, depending on context:

- If a function in `org-ctrl-c-ctrl-c-hook' recognizes this location,
  this is what we do.

- If the cursor is on a statistics cookie, update it.

- If the cursor is in a headline, prompt for tags and insert them
  into the current line, aligned to `org-tags-column'.  When called
  with prefix arg, realign all tags in the current buffer.

- If the cursor is in one of the special #+KEYWORD lines, this
  triggers scanning the buffer for these lines and updating the
  information.

- If the cursor is inside a table, realign the table.  This command
  works even if the automatic table editor has been turned off.

- If the cursor is on a #+TBLFM line, re-apply the formulas to
  the entire table.

- If the cursor is at a footnote reference or definition, jump to
  the corresponding definition or references, respectively.

- If the cursor is a the beginning of a dynamic block, update it.

- If the current buffer is a capture buffer, close note and file it.

- If the cursor is on a <<<target>>>, update radio targets and
  corresponding links in this buffer.

- If the cursor is on a numbered item in a plain list, renumber the
  ordered list.

- If the cursor is on a checkbox, toggle it.

- If the cursor is on a code block, evaluate it.  The variable
  `org-confirm-babel-evaluate' can be used to control prompting
  before code block evaluation, by default every code block
  evaluation requires confirmation.  Code block evaluation can be
  inhibited by setting `org-babel-no-eval-on-ctrl-c-ctrl-c'."
  (interactive "P")
  (cond
   ((or (bound-and-true-p org-clock-overlays) org-occur-highlights)
    (when (boundp 'org-clock-overlays) (org-clock-remove-overlays))
    (org-remove-occur-highlights)
    (message "Temporary highlights/overlays removed from current buffer"))
   ((and (local-variable-p 'org-finish-function)
	 (fboundp org-finish-function))
    (funcall org-finish-function))
   ((org-babel-hash-at-point))
   ((run-hook-with-args-until-success 'org-ctrl-c-ctrl-c-hook))
   (t
    (let* ((context
	    (org-element-lineage
	     (org-element-context)
	     ;; Limit to supported contexts.
	     '(babel-call clock dynamic-block footnote-definition
			  footnote-reference inline-babel-call inline-src-block
			  inlinetask item keyword node-property paragraph
			  plain-list planning property-drawer radio-target
			  src-block statistics-cookie table table-cell table-row
			  timestamp)
	     t))
	   (type (org-element-type context)))
      ;; For convenience: at the first line of a paragraph on the same
      ;; line as an item, apply function on that item instead.
      (when (eq type 'paragraph)
	(let ((parent (org-element-property :parent context)))
	  (when (and (eq (org-element-type parent) 'item)
		     (= (line-beginning-position)
			(org-element-property :begin parent)))
	    (setq context parent)
	    (setq type 'item))))
      ;; Act according to type of element or object at point.
      ;;
      ;; Do nothing on a blank line, except if it is contained in
      ;; a src block.  Hence, we first check if point is in such
      ;; a block and then if it is at a blank line.
      (pcase type
	((or `inline-src-block `src-block)
	 (unless org-babel-no-eval-on-ctrl-c-ctrl-c
	   (org-babel-eval-wipe-error-buffer)
	   (org-babel-execute-src-block
	    current-prefix-arg (org-babel-get-src-block-info nil context))))
	((guard (org-match-line "[ \t]*$"))
	 (or (run-hook-with-args-until-success 'org-ctrl-c-ctrl-c-final-hook)
	     (user-error
	      (substitute-command-keys
	       "`\\[org-ctrl-c-ctrl-c]' can do nothing useful here"))))
	((or `babel-call `inline-babel-call)
	 (let ((info (org-babel-lob-get-info context)))
	   (when info (org-babel-execute-src-block nil info))))
	(`clock (org-clock-update-time-maybe))
	(`dynamic-block
	 (save-excursion
	   (goto-char (org-element-property :post-affiliated context))
	   (org-update-dblock)))
	(`footnote-definition
	 (goto-char (org-element-property :post-affiliated context))
	 (call-interactively 'org-footnote-action))
	(`footnote-reference (call-interactively #'org-footnote-action))
	((or `headline `inlinetask)
	 (save-excursion (goto-char (org-element-property :begin context))
			 (call-interactively #'org-set-tags)))
	(`item
	 ;; At an item: `C-u C-u' sets checkbox to "[-]"
	 ;; unconditionally, whereas `C-u' will toggle its presence.
	 ;; Without a universal argument, if the item has a checkbox,
	 ;; toggle it.  Otherwise repair the list.
	 (let* ((box (org-element-property :checkbox context))
		(struct (org-element-property :structure context))
		(old-struct (copy-tree struct))
		(parents (org-list-parents-alist struct))
		(prevs (org-list-prevs-alist struct))
		(orderedp (org-not-nil (org-entry-get nil "ORDERED"))))
	   (org-list-set-checkbox
	    (org-element-property :begin context) struct
	    (cond ((equal arg '(16)) "[-]")
		  ((and (not box) (equal arg '(4))) "[ ]")
		  ((or (not box) (equal arg '(4))) nil)
		  ((eq box 'on) "[ ]")
		  (t "[X]")))
	   ;; Mimic `org-list-write-struct' but with grabbing a return
	   ;; value from `org-list-struct-fix-box'.
	   (org-list-struct-fix-ind struct parents 2)
	   (org-list-struct-fix-item-end struct)
	   (org-list-struct-fix-bul struct prevs)
	   (org-list-struct-fix-ind struct parents)
	   (let ((block-item
		  (org-list-struct-fix-box struct parents prevs orderedp)))
	     (if (and box (equal struct old-struct))
		 (if (equal arg '(16))
		     (message "Checkboxes already reset")
		   (user-error "Cannot toggle this checkbox: %s"
			       (if (eq box 'on)
				   "all subitems checked"
				 "unchecked subitems")))
	       (org-list-struct-apply-struct struct old-struct)
	       (org-update-checkbox-count-maybe))
	     (when block-item
	       (message "Checkboxes were removed due to empty box at line %d"
			(org-current-line block-item))))))
	(`keyword
	 (let ((org-inhibit-startup-visibility-stuff t)
	       (org-startup-align-all-tables nil))
	   (when (boundp 'org-table-coordinate-overlays)
	     (mapc #'delete-overlay org-table-coordinate-overlays)
	     (setq org-table-coordinate-overlays nil))
	   (org-save-outline-visibility 'use-markers (org-mode-restart)))
	 (message "Local setup has been refreshed"))
	(`plain-list
	 ;; At a plain list, with a double C-u argument, set
	 ;; checkboxes of each item to "[-]", whereas a single one
	 ;; will toggle their presence according to the state of the
	 ;; first item in the list.  Without an argument, repair the
	 ;; list.
	 (let* ((begin (org-element-property :contents-begin context))
		(beginm (move-marker (make-marker) begin))
		(struct (org-element-property :structure context))
		(old-struct (copy-tree struct))
		(first-box (save-excursion
			     (goto-char begin)
			     (looking-at org-list-full-item-re)
			     (match-string-no-properties 3)))
		(new-box (cond ((equal arg '(16)) "[-]")
			       ((equal arg '(4)) (unless first-box "[ ]"))
			       ((equal first-box "[X]") "[ ]")
			       (t "[X]"))))
	   (cond
	    (arg
	     (dolist (pos
		      (org-list-get-all-items
		       begin struct (org-list-prevs-alist struct)))
	       (org-list-set-checkbox pos struct new-box)))
	    ((and first-box (eq (point) begin))
	     ;; For convenience, when point is at bol on the first
	     ;; item of the list and no argument is provided, simply
	     ;; toggle checkbox of that item, if any.
	     (org-list-set-checkbox begin struct new-box)))
	   (org-list-write-struct
	    struct (org-list-parents-alist struct) old-struct)
	   (org-update-checkbox-count-maybe)
	   (save-excursion (goto-char beginm) (org-list-send-list 'maybe))))
	((or `property-drawer `node-property)
	 (call-interactively #'org-property-action))
	(`radio-target
	 (call-interactively #'org-update-radio-target-regexp))
	(`statistics-cookie
	 (call-interactively #'org-update-statistics-cookies))
	((or `table `table-cell `table-row)
	 ;; At a table, recalculate every field and align it.  Also
	 ;; send the table if necessary.  If the table has
	 ;; a `table.el' type, just give up.  At a table row or cell,
	 ;; maybe recalculate line but always align table.
	 (if (eq (org-element-property :type context) 'table.el)
	     (message "%s" (substitute-command-keys "\\<org-mode-map>\
Use `\\[org-edit-special]' to edit table.el tables"))
	   (if (or (eq type 'table)
		   ;; Check if point is at a TBLFM line.
		   (and (eq type 'table-row)
			(= (point) (org-element-property :end context))))
	       (save-excursion
		 (if (org-at-TBLFM-p)
		     (progn (require 'org-table)
			    (org-table-calc-current-TBLFM))
		   (goto-char (org-element-property :contents-begin context))
		   (org-call-with-arg 'org-table-recalculate (or arg t))
		   (orgtbl-send-table 'maybe)))
	     (org-table-maybe-eval-formula)
	     (cond (arg (call-interactively #'org-table-recalculate))
		   ((org-table-maybe-recalculate-line))
		   (t (org-table-align))))))
	((or `timestamp (and `planning (guard (org-at-timestamp-p 'lax))))
	 (org-timestamp-change 0 'day))
	((and `nil (guard (org-at-heading-p)))
	 ;; When point is on an unsupported object type, we can miss
	 ;; the fact that it also is at a heading.  Handle it here.
	 (call-interactively #'org-set-tags))
	((guard
	  (run-hook-with-args-until-success 'org-ctrl-c-ctrl-c-final-hook)))
	(_
	 (user-error
	  (substitute-command-keys
	   "`\\[org-ctrl-c-ctrl-c]' can do nothing useful here"))))))))

(defun org-mode-restart ()
  (interactive)
  (let ((indent-status (bound-and-true-p org-indent-mode)))
    (funcall major-mode)
    (hack-local-variables)
    (when (and indent-status (not (bound-and-true-p org-indent-mode)))
      (org-indent-mode -1))
    (org-reset-file-cache))
  (message "%s restarted" major-mode))

(defun org-kill-note-or-show-branches ()
  "Abort storing current note, or call `outline-show-branches'."
  (interactive)
  (if (not org-finish-function)
      (progn
	(outline-hide-subtree)
	(call-interactively 'outline-show-branches))
    (let ((org-note-abort t))
      (funcall org-finish-function))))

(defun org-delete-indentation (&optional arg)
  "Join current line to previous and fix whitespace at join.

If previous line is a headline add to headline title.  Otherwise
the function calls `delete-indentation'.

With a non-nil optional argument, join it to the following one."
  (interactive "*P")
  (if (save-excursion
	(beginning-of-line (if arg 1 0))
	(let ((case-fold-search nil))
	  (looking-at org-complex-heading-regexp)))
      ;; At headline.
      (let ((tags-column (when (match-beginning 5)
			   (save-excursion (goto-char (match-beginning 5))
					   (current-column))))
	    (string (concat " " (progn (when arg (forward-line 1))
				       (org-trim (delete-and-extract-region
						  (line-beginning-position)
						  (line-end-position)))))))
	(unless (bobp) (delete-region (point) (1- (point))))
	(goto-char (or (match-end 4)
		       (match-beginning 5)
		       (match-end 0)))
	(skip-chars-backward " \t")
	(save-excursion (insert string))
	;; Adjust alignment of tags.
	(cond
	 ((not tags-column))		;no tags
	 (org-auto-align-tags (org-set-tags nil t))
	 (t (org--align-tags-here tags-column)))) ;preserve tags column
    (delete-indentation arg)))

(defun org-open-line (n)
  "Insert a new row in tables, call `open-line' elsewhere.
If `org-special-ctrl-o' is nil, just call `open-line' everywhere.
As a special case, when a document starts with a table, allow to
call `open-line' on the very first character."
  (interactive "*p")
  (if (and org-special-ctrl-o (/= (point) 1) (org-at-table-p))
      (org-table-insert-row)
    (open-line n)))

(defun org-return (&optional indent)
  "Goto next table row or insert a newline.

Calls `org-table-next-row' or `newline', depending on context.

When optional INDENT argument is non-nil, call
`newline-and-indent' instead of `newline'.

When `org-return-follows-link' is non-nil and point is on
a timestamp or a link, call `org-open-at-point'.  However, it
will not happen if point is in a table or on a \"dead\"
object (e.g., within a comment).  In these case, you need to use
`org-open-at-point' directly."
  (interactive)
  (let ((context (if org-return-follows-link (org-element-context)
		   (org-element-at-point))))
    (cond
     ;; In a table, call `org-table-next-row'.
     ((or (and (eq (org-element-type context) 'table)
	       (>= (point) (org-element-property :contents-begin context))
	       (< (point) (org-element-property :contents-end context)))
	  (org-element-lineage context '(table-row table-cell) t))
      (org-table-justify-field-maybe)
      (call-interactively #'org-table-next-row))
     ;; On a link or a timestamp, call `org-open-at-point' if
     ;; `org-return-follows-link' allows it.  Tolerate fuzzy
     ;; locations, e.g., in a comment, as `org-open-at-point'.
     ((and org-return-follows-link
	   (or (org-in-regexp org-ts-regexp-both nil t)
	       (org-in-regexp org-tsr-regexp-both nil  t)
	       (org-in-regexp org-any-link-re nil t)))
      (call-interactively #'org-open-at-point))
     ;; Insert newline in heading, but preserve tags.
     ((and (not (bolp))
	   (save-excursion (beginning-of-line)
			   (let ((case-fold-search nil))
			     (looking-at org-complex-heading-regexp))))
      ;; At headline.  Split line.  However, if point is on keyword,
      ;; priority cookie or tags, do not break any of them: add
      ;; a newline after the headline instead.
      (let ((tags-column (and (match-beginning 5)
			      (save-excursion (goto-char (match-beginning 5))
					      (current-column))))
	    (string
	     (when (and (match-end 4) (org-point-in-group (point) 4))
	       (delete-and-extract-region (point) (match-end 4)))))
	;; Adjust tag alignment.
	(cond
	 ((not (and tags-column string)))
	 (org-auto-align-tags (org-set-tags nil t))
	 (t (org--align-tags-here tags-column))) ;preserve tags column
	(end-of-line)
	(org-show-entry)
	(if indent (newline-and-indent) (newline))
	(when string (save-excursion (insert (org-trim string))))))
     ;; In a list, make sure indenting keeps trailing text within.
     ((and indent
	   (not (eolp))
	   (org-element-lineage context '(item)))
      (let ((trailing-data
	     (delete-and-extract-region (point) (line-end-position))))
	(newline-and-indent)
	(save-excursion (insert trailing-data))))
     (t (if indent (newline-and-indent) (newline))))))

(defun org-return-indent ()
  "Goto next table row or insert a newline and indent.
Calls `org-table-next-row' or `newline-and-indent', depending on
context.  See the individual commands for more information."
  (interactive)
  (org-return t))

(defun org-ctrl-c-star ()
  "Compute table, or change heading status of lines.
Calls `org-table-recalculate' or `org-toggle-heading',
depending on context."
  (interactive)
  (cond
   ((org-at-table-p)
    (call-interactively 'org-table-recalculate))
   (t
    ;; Convert all lines in region to list items
    (call-interactively 'org-toggle-heading))))

(defun org-ctrl-c-minus ()
  "Insert separator line in table or modify bullet status of line.
Also turns a plain line or a region of lines into list items.
Calls `org-table-insert-hline', `org-toggle-item', or
`org-cycle-list-bullet', depending on context."
  (interactive)
  (cond
   ((org-at-table-p)
    (call-interactively 'org-table-insert-hline))
   ((org-region-active-p)
    (call-interactively 'org-toggle-item))
   ((org-in-item-p)
    (call-interactively 'org-cycle-list-bullet))
   (t
    (call-interactively 'org-toggle-item))))

(defun org-toggle-heading (&optional nstars)
  "Convert headings to normal text, or items or text to headings.
If there is no active region, only convert the current line.

With a `\\[universal-argument]' prefix, convert the whole list at
point into heading.

In a region:

- If the first non blank line is a headline, remove the stars
  from all headlines in the region.

- If it is a normal line, turn each and every normal line (i.e.,
  not an heading or an item) in the region into headings.  If you
  want to convert only the first line of this region, use one
  universal prefix argument.

- If it is a plain list item, turn all plain list items into headings.

When converting a line into a heading, the number of stars is chosen
such that the lines become children of the current entry.  However,
when a numeric prefix argument is given, its value determines the
number of stars to add."
  (interactive "P")
  (let ((skip-blanks
	 (function
	  ;; Return beginning of first non-blank line, starting from
	  ;; line at POS.
	  (lambda (pos)
	    (save-excursion
	      (goto-char pos)
	      (while (org-at-comment-p) (forward-line))
	      (skip-chars-forward " \r\t\n")
	      (point-at-bol)))))
	beg end toggled)
    ;; Determine boundaries of changes.  If a universal prefix has
    ;; been given, put the list in a region.  If region ends at a bol,
    ;; do not consider the last line to be in the region.

    (when (and current-prefix-arg (org-at-item-p))
      (when (listp current-prefix-arg) (setq current-prefix-arg 1))
      (org-mark-element))

    (if (org-region-active-p)
	(setq beg (funcall skip-blanks (region-beginning))
	      end (copy-marker (save-excursion
				 (goto-char (region-end))
				 (if (bolp) (point) (point-at-eol)))))
      (setq beg (funcall skip-blanks (point-at-bol))
	    end (copy-marker (point-at-eol))))
    ;; Ensure inline tasks don't count as headings.
    (org-with-limited-levels
     (save-excursion
       (goto-char beg)
       (cond
	;; Case 1. Started at an heading: de-star headings.
	((org-at-heading-p)
	 (while (< (point) end)
	   (when (org-at-heading-p t)
	     (looking-at org-outline-regexp) (replace-match "")
	     (setq toggled t))
	   (forward-line)))
	;; Case 2. Started at an item: change items into headlines.
	;;         One star will be added by `org-list-to-subtree'.
	((org-at-item-p)
	 (while (< (point) end)
	   (when (org-at-item-p)
	     ;; Pay attention to cases when region ends before list.
	     (let* ((struct (org-list-struct))
		    (list-end
		     (min (org-list-get-bottom-point struct) (1+ end))))
	       (save-restriction
		 (narrow-to-region (point) list-end)
		 (insert (org-list-to-subtree (org-list-to-lisp t)) "\n")))
	     (setq toggled t))
	   (forward-line)))
	;; Case 3. Started at normal text: make every line an heading,
	;;         skipping headlines and items.
	(t (let* ((stars
		   (make-string
		    (if (numberp nstars) nstars (or (org-current-level) 0)) ?*))
		  (add-stars
		   (cond (nstars "")                ; stars from prefix only
			 ((equal stars "") "*")     ; before first heading
			 (org-odd-levels-only "**") ; inside heading, odd
			 (t "*")))                  ; inside heading, oddeven
		  (rpl (concat stars add-stars " "))
		  (lend (when (listp nstars) (save-excursion (end-of-line) (point)))))
	     (while (< (point) (if (equal nstars '(4)) lend end))
	       (when (and (not (or (org-at-heading-p) (org-at-item-p) (org-at-comment-p)))
			  (looking-at "\\([ \t]*\\)\\(\\S-\\)"))
		 (replace-match (concat rpl (match-string 2))) (setq toggled t))
	       (forward-line)))))))
    (unless toggled (message "Cannot toggle heading from here"))))

(defun org-meta-return (&optional arg)
  "Insert a new heading or wrap a region in a table.
Calls `org-insert-heading', `org-insert-item' or
`org-table-wrap-region', depending on context.  When called with
an argument, unconditionally call `org-insert-heading'."
  (interactive "P")
  (org-check-before-invisible-edit 'insert)
  (or (run-hook-with-args-until-success 'org-metareturn-hook)
      (call-interactively (cond (arg #'org-insert-heading)
				((org-at-table-p) #'org-table-wrap-region)
				((org-in-item-p) #'org-insert-item)
				(t #'org-insert-heading)))))

;;; Menu entries

(defsubst org-in-subtree-not-table-p ()
  "Are we in a subtree and not in a table?"
  (and (not (org-before-first-heading-p))
       (not (org-at-table-p))))

;; Define the Org mode menus
(easy-menu-define org-tbl-menu org-mode-map "Tbl menu"
  '("Tbl"
    ["Align" org-ctrl-c-ctrl-c :active (org-at-table-p)]
    ["Next Field" org-cycle (org-at-table-p)]
    ["Previous Field" org-shifttab (org-at-table-p)]
    ["Next Row" org-return (org-at-table-p)]
    "--"
    ["Blank Field" org-table-blank-field (org-at-table-p)]
    ["Edit Field" org-table-edit-field (org-at-table-p)]
    ["Copy Field from Above" org-table-copy-down (org-at-table-p)]
    "--"
    ("Column"
     ["Move Column Left" org-metaleft (org-at-table-p)]
     ["Move Column Right" org-metaright (org-at-table-p)]
     ["Delete Column" org-shiftmetaleft (org-at-table-p)]
     ["Insert Column" org-shiftmetaright (org-at-table-p)])
    ("Row"
     ["Move Row Up" org-metaup (org-at-table-p)]
     ["Move Row Down" org-metadown (org-at-table-p)]
     ["Delete Row" org-shiftmetaup (org-at-table-p)]
     ["Insert Row" org-shiftmetadown (org-at-table-p)]
     ["Sort lines in region" org-table-sort-lines (org-at-table-p)]
     "--"
     ["Insert Hline" org-ctrl-c-minus (org-at-table-p)])
    ("Rectangle"
     ["Copy Rectangle" org-copy-special (org-at-table-p)]
     ["Cut Rectangle" org-cut-special (org-at-table-p)]
     ["Paste Rectangle" org-paste-special (org-at-table-p)]
     ["Fill Rectangle" org-table-wrap-region (org-at-table-p)])
    "--"
    ("Calculate"
     ["Set Column Formula" org-table-eval-formula (org-at-table-p)]
     ["Set Field Formula" (org-table-eval-formula '(4)) :active (org-at-table-p) :keys "C-u C-c ="]
     ["Edit Formulas" org-edit-special (org-at-table-p)]
     "--"
     ["Recalculate line" org-table-recalculate (org-at-table-p)]
     ["Recalculate all" (lambda () (interactive) (org-table-recalculate '(4))) :active (org-at-table-p) :keys "C-u C-c *"]
     ["Iterate all" (lambda () (interactive) (org-table-recalculate '(16))) :active (org-at-table-p) :keys "C-u C-u C-c *"]
     "--"
     ["Toggle Recalculate Mark" org-table-rotate-recalc-marks (org-at-table-p)]
     "--"
     ["Sum Column/Rectangle" org-table-sum
      (or (org-at-table-p) (org-region-active-p))]
     ["Which Column?" org-table-current-column (org-at-table-p)])
    ["Debug Formulas"
     org-table-toggle-formula-debugger
     :style toggle :selected (bound-and-true-p org-table-formula-debug)]
    ["Show Col/Row Numbers"
     org-table-toggle-coordinate-overlays
     :style toggle
     :selected (bound-and-true-p org-table-overlay-coordinates)]
    "--"
    ["Create" org-table-create (not (org-at-table-p))]
    ["Convert Region" org-table-convert-region (not (org-at-table-p 'any))]
    ["Import from File" org-table-import (not (org-at-table-p))]
    ["Export to File" org-table-export (org-at-table-p)]
    "--"
    ["Create/Convert from/to table.el" org-table-create-with-table.el t]
    "--"
    ("Plot"
     ["Ascii plot" orgtbl-ascii-plot :active (org-at-table-p) :keys "C-c \" a"]
     ["Gnuplot" org-plot/gnuplot :active (org-at-table-p) :keys "C-c \" g"])))

(easy-menu-define org-org-menu org-mode-map "Org menu"
  '("Org"
    ("Show/Hide"
     ["Cycle Visibility" org-cycle :active (or (bobp) (outline-on-heading-p))]
     ["Cycle Global Visibility" org-shifttab :active (not (org-at-table-p))]
     ["Sparse Tree..." org-sparse-tree t]
     ["Reveal Context" org-reveal t]
     ["Show All" outline-show-all t]
     "--"
     ["Subtree to indirect buffer" org-tree-to-indirect-buffer t])
    "--"
    ["New Heading" org-insert-heading t]
    ("Navigate Headings"
     ["Up" outline-up-heading t]
     ["Next" outline-next-visible-heading t]
     ["Previous" outline-previous-visible-heading t]
     ["Next Same Level" outline-forward-same-level t]
     ["Previous Same Level" outline-backward-same-level t]
     "--"
     ["Jump" org-goto t])
    ("Edit Structure"
     ["Refile Subtree" org-refile (org-in-subtree-not-table-p)]
     "--"
     ["Move Subtree Up" org-metaup (org-at-heading-p)]
     ["Move Subtree Down" org-metadown (org-at-heading-p)]
     "--"
     ["Copy Subtree"  org-copy-special (org-in-subtree-not-table-p)]
     ["Cut Subtree"  org-cut-special (org-in-subtree-not-table-p)]
     ["Paste Subtree"  org-paste-special (not (org-at-table-p))]
     "--"
     ["Clone subtree, shift time" org-clone-subtree-with-time-shift t]
     "--"
     ["Copy visible text"  org-copy-visible t]
     "--"
     ["Promote Heading" org-metaleft (org-in-subtree-not-table-p)]
     ["Promote Subtree" org-shiftmetaleft (org-in-subtree-not-table-p)]
     ["Demote Heading"  org-metaright (org-in-subtree-not-table-p)]
     ["Demote Subtree"  org-shiftmetaright (org-in-subtree-not-table-p)]
     "--"
     ["Sort Region/Children" org-sort t]
     "--"
     ["Convert to odd levels" org-convert-to-odd-levels t]
     ["Convert to odd/even levels" org-convert-to-oddeven-levels t])
    ("Editing"
     ["Emphasis..." org-emphasize t]
     ["Edit Source Example" org-edit-special t]
     "--"
     ["Footnote new/jump" org-footnote-action t]
     ["Footnote extra" (org-footnote-action t) :active t :keys "C-u C-c C-x f"])
    ("Archive"
     ["Archive (default method)" org-archive-subtree-default (org-in-subtree-not-table-p)]
     "--"
     ["Move Subtree to Archive file" org-archive-subtree (org-in-subtree-not-table-p)]
     ["Toggle ARCHIVE tag" org-toggle-archive-tag (org-in-subtree-not-table-p)]
     ["Move subtree to Archive sibling" org-archive-to-archive-sibling (org-in-subtree-not-table-p)]
     )
    "--"
    ("Hyperlinks"
     ["Store Link (Global)" org-store-link t]
     ["Find existing link to here" org-occur-link-in-agenda-files t]
     ["Insert Link" org-insert-link t]
     ["Follow Link" org-open-at-point t]
     "--"
     ["Next link" org-next-link t]
     ["Previous link" org-previous-link t]
     "--"
     ["Descriptive Links"
      org-toggle-link-display
      :style radio
      :selected org-descriptive-links
      ]
     ["Literal Links"
      org-toggle-link-display
      :style radio
      :selected (not org-descriptive-links)])
    "--"
    ("TODO Lists"
     ["TODO/DONE/-" org-todo t]
     ("Select keyword"
      ["Next keyword" org-shiftright (org-at-heading-p)]
      ["Previous keyword" org-shiftleft (org-at-heading-p)]
      ["Complete Keyword" pcomplete (assq :todo-keyword (org-context))]
      ["Next keyword set" org-shiftcontrolright (and (> (length org-todo-sets) 1) (org-at-heading-p))]
      ["Previous keyword set" org-shiftcontrolright (and (> (length org-todo-sets) 1) (org-at-heading-p))])
     ["Show TODO Tree" org-show-todo-tree :active t :keys "C-c / t"]
     ["Global TODO list" org-todo-list :active t :keys "C-c a t"]
     "--"
     ["Enforce dependencies" (customize-variable 'org-enforce-todo-dependencies)
      :selected org-enforce-todo-dependencies :style toggle :active t]
     "Settings for tree at point"
     ["Do Children sequentially" org-toggle-ordered-property :style radio
      :selected (org-entry-get nil "ORDERED")
      :active org-enforce-todo-dependencies :keys "C-c C-x o"]
     ["Do Children parallel" org-toggle-ordered-property :style radio
      :selected (not (org-entry-get nil "ORDERED"))
      :active org-enforce-todo-dependencies :keys "C-c C-x o"]
     "--"
     ["Set Priority" org-priority t]
     ["Priority Up" org-shiftup t]
     ["Priority Down" org-shiftdown t]
     "--"
     ["Get news from all feeds" org-feed-update-all t]
     ["Go to the inbox of a feed..." org-feed-goto-inbox t]
     ["Customize feeds" (customize-variable 'org-feed-alist) t])
    ("TAGS and Properties"
     ["Set Tags" org-set-tags-command (not (org-before-first-heading-p))]
     ["Change tag in region" org-change-tag-in-region (org-region-active-p)]
     "--"
     ["Set property" org-set-property (not (org-before-first-heading-p))]
     ["Column view of properties" org-columns t]
     ["Insert Column View DBlock" org-columns-insert-dblock t])
    ("Dates and Scheduling"
     ["Timestamp" org-time-stamp (not (org-before-first-heading-p))]
     ["Timestamp (inactive)" org-time-stamp-inactive (not (org-before-first-heading-p))]
     ("Change Date"
      ["1 Day Later" org-shiftright (org-at-timestamp-p 'lax)]
      ["1 Day Earlier" org-shiftleft (org-at-timestamp-p 'lax)]
      ["1 ... Later" org-shiftup (org-at-timestamp-p 'lax)]
      ["1 ... Earlier" org-shiftdown (org-at-timestamp-p 'lax)])
     ["Compute Time Range" org-evaluate-time-range t]
     ["Schedule Item" org-schedule (not (org-before-first-heading-p))]
     ["Deadline" org-deadline (not (org-before-first-heading-p))]
     "--"
     ["Custom time format" org-toggle-time-stamp-overlays
      :style radio :selected org-display-custom-times]
     "--"
     ["Goto Calendar" org-goto-calendar t]
     ["Date from Calendar" org-date-from-calendar t]
     "--"
     ["Start/Restart Timer" org-timer-start t]
     ["Pause/Continue Timer" org-timer-pause-or-continue t]
     ["Stop Timer" org-timer-pause-or-continue :active t :keys "C-u C-c C-x ,"]
     ["Insert Timer String" org-timer t]
     ["Insert Timer Item" org-timer-item t])
    ("Logging work"
     ["Clock in" org-clock-in :active t :keys "C-c C-x C-i"]
     ["Switch task" (lambda () (interactive) (org-clock-in '(4))) :active t :keys "C-u C-c C-x C-i"]
     ["Clock out" org-clock-out t]
     ["Clock cancel" org-clock-cancel t]
     "--"
     ["Mark as default task" org-clock-mark-default-task t]
     ["Clock in, mark as default" (lambda () (interactive) (org-clock-in '(16))) :active t :keys "C-u C-u C-c C-x C-i"]
     ["Goto running clock" org-clock-goto t]
     "--"
     ["Display times" org-clock-display t]
     ["Create clock table" org-clock-report t]
     "--"
     ["Record DONE time"
      (progn (setq org-log-done (not org-log-done))
	     (message "Switching to %s will %s record a timestamp"
		      (car org-done-keywords)
		      (if org-log-done "automatically" "not")))
      :style toggle :selected org-log-done])
    "--"
    ["Agenda Command..." org-agenda t]
    ["Set Restriction Lock" org-agenda-set-restriction-lock t]
    ("File List for Agenda")
    ("Special views current file"
     ["TODO Tree"  org-show-todo-tree t]
     ["Check Deadlines" org-check-deadlines t]
     ["Tags/Property tree" org-match-sparse-tree t])
    "--"
    ["Export/Publish..." org-export-dispatch t]
    ("LaTeX"
     ["Org CDLaTeX mode" org-cdlatex-mode :style toggle
      :selected org-cdlatex-mode]
     ["Insert Environment" cdlatex-environment (fboundp 'cdlatex-environment)]
     ["Insert math symbol" cdlatex-math-symbol (fboundp 'cdlatex-math-symbol)]
     ["Modify math symbol" org-cdlatex-math-modify
      (org-inside-LaTeX-fragment-p)]
     ["Insert citation" org-reftex-citation t])
    "--"
    ("MobileOrg"
     ["Push Files and Views" org-mobile-push t]
     ["Get Captured and Flagged" org-mobile-pull t]
     ["Find FLAGGED Tasks" (org-agenda nil "?") :active t :keys "C-c a ?"]
     "--"
     ["Setup" (progn (require 'org-mobile) (customize-group 'org-mobile)) t])
    "--"
    ("Documentation"
     ["Show Version" org-version t]
     ["Info Documentation" org-info t])
    ("Customize"
     ["Browse Org Group" org-customize t]
     "--"
     ["Expand This Menu" org-create-customize-menu
      (fboundp 'customize-menu-create)])
    ["Send bug report" org-submit-bug-report t]
    "--"
    ("Refresh/Reload"
     ["Refresh setup current buffer" org-mode-restart t]
     ["Reload Org (after update)" org-reload t]
     ["Reload Org uncompiled" (org-reload t) :active t :keys "C-u C-c C-x !"])
    ))

(defun org-info (&optional node)
  "Read documentation for Org in the info system.
With optional NODE, go directly to that node."
  (interactive)
  (info (format "(org)%s" (or node ""))))

;;;###autoload
(defun org-submit-bug-report ()
  "Submit a bug report on Org via mail.

Don't hesitate to report any problems or inaccurate documentation.

If you don't have setup sending mail from (X)Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your Org version and configuration."
  (interactive)
  (require 'reporter)
  (defvar reporter-prompt-for-summary-p)
  (org-load-modules-maybe)
  (org-require-autoloaded-modules)
  (let ((reporter-prompt-for-summary-p "Bug report subject: "))
    (reporter-submit-bug-report
     "emacs-orgmode@gnu.org"
     (org-version nil 'full)
     (let (list)
       (save-window-excursion
	 (pop-to-buffer-same-window (get-buffer-create "*Warn about privacy*"))
	 (delete-other-windows)
	 (erase-buffer)
	 (insert "You are about to submit a bug report to the Org mailing list.

We would like to add your full Org and Outline configuration to the
bug report.  This greatly simplifies the work of the maintainer and
other experts on the mailing list.

HOWEVER, some variables you have customized may contain private
information.  The names of customers, colleagues, or friends, might
appear in the form of file names, tags, todo states, or search strings.
If you answer yes to the prompt, you might want to check and remove
such private information before sending the email.")
	 (add-text-properties (point-min) (point-max) '(face org-warning))
	 (when (yes-or-no-p "Include your Org configuration ")
	   (mapatoms
	    (lambda (v)
	      (and (boundp v)
		   (string-match "\\`\\(org-\\|outline-\\)" (symbol-name v))
		   (or (and (symbol-value v)
			    (string-match "\\(-hook\\|-function\\)\\'" (symbol-name v)))
		       (and
			(get v 'custom-type) (get v 'standard-value)
			(not (equal (symbol-value v) (eval (car (get v 'standard-value)))))))
		   (push v list)))))
	 (kill-buffer (get-buffer "*Warn about privacy*"))
	 list))
     nil nil
     "Remember to cover the basics, that is, what you expected to happen and
what in fact did happen.  You don't know how to make a good report?  See

     http://orgmode.org/manual/Feedback.html#Feedback

Your bug report will be posted to the Org mailing list.
------------------------------------------------------------------------")
    (save-excursion
      (when (re-search-backward "^\\(Subject: \\)Org mode version \\(.*?\\);[ \t]*\\(.*\\)" nil t)
	(replace-match "\\1Bug: \\3 [\\2]")))))


(defun org-install-agenda-files-menu ()
  (let ((bl (buffer-list)))
    (save-excursion
      (while bl
	(set-buffer (pop bl))
	(when (derived-mode-p 'org-mode) (setq bl nil)))
      (when (derived-mode-p 'org-mode)
	(easy-menu-change
	 '("Org") "File List for Agenda"
	 (append
	  (list
	   ["Edit File List" (org-edit-agenda-file-list) t]
	   ["Add/Move Current File to Front of List" org-agenda-file-to-front t]
	   ["Remove Current File from List" org-remove-file t]
	   ["Cycle through agenda files" org-cycle-agenda-files t]
	   ["Occur in all agenda files" org-occur-in-agenda-files t]
	   "--")
	  (mapcar 'org-file-menu-entry
		  ;; Prevent initialization from failing.
		  (ignore-errors (org-agenda-files t)))))))))

;;;; Documentation

(defun org-require-autoloaded-modules ()
  (interactive)
  (mapc #'require
	'(org-agenda org-archive org-attach org-clock org-colview org-id
		     org-table org-timer)))

;;;###autoload
(defun org-reload (&optional uncompiled)
  "Reload all org lisp files.
With prefix arg UNCOMPILED, load the uncompiled versions."
  (interactive "P")
  (require 'loadhist)
  (let* ((org-dir     (org-find-library-dir "org"))
	 (contrib-dir (or (org-find-library-dir "org-contribdir") org-dir))
	 (feature-re "^\\(org\\|ob\\|ox\\)\\(-.*\\)?")
	 (remove-re (format "\\`%s\\'"
			    (regexp-opt '("org" "org-loaddefs" "org-version"))))
	 (feats (delete-dups
		 (mapcar 'file-name-sans-extension
			 (mapcar 'file-name-nondirectory
				 (delq nil
				       (mapcar 'feature-file
					       features))))))
	 (lfeat (append
		 (sort
		  (setq feats
			(delq nil (mapcar
				   (lambda (f)
				     (if (and (string-match feature-re f)
					      (not (string-match remove-re f)))
					 f nil))
				   feats)))
		  'string-lessp)
		 (list "org-version" "org")))
	 (load-suffixes (when (boundp 'load-suffixes) load-suffixes))
	 (load-suffixes (if uncompiled (reverse load-suffixes) load-suffixes))
	 load-uncore load-misses)
    (setq load-misses
	  (delq 't
		(mapcar (lambda (f)
			  (or (org-load-noerror-mustsuffix (concat org-dir f))
			      (and (string= org-dir contrib-dir)
				   (org-load-noerror-mustsuffix (concat contrib-dir f)))
			      (and (org-load-noerror-mustsuffix (concat (org-find-library-dir f) f))
				   (add-to-list 'load-uncore f 'append)
				   't)
			      f))
			lfeat)))
    (when load-uncore
      (message "The following feature%s found in load-path, please check if that's correct:\n%s"
	       (if (> (length load-uncore) 1) "s were" " was") load-uncore))
    (if load-misses
	(message "Some error occurred while reloading Org feature%s\n%s\nPlease check *Messages*!\n%s"
		 (if (> (length load-misses) 1) "s" "") load-misses (org-version nil 'full))
      (message "Successfully reloaded Org\n%s" (org-version nil 'full)))))

;;;###autoload
(defun org-customize ()
  "Call the customize function with org as argument."
  (interactive)
  (org-load-modules-maybe)
  (org-require-autoloaded-modules)
  (customize-browse 'org))

(defun org-create-customize-menu ()
  "Create a full customization menu for Org mode, insert it into the menu."
  (interactive)
  (org-load-modules-maybe)
  (org-require-autoloaded-modules)
  (if (fboundp 'customize-menu-create)
      (progn
	(easy-menu-change
	 '("Org") "Customize"
	 `(["Browse Org group" org-customize t]
	   "--"
	   ,(customize-menu-create 'org)
	   ["Set" Custom-set t]
	   ["Save" Custom-save t]
	   ["Reset to Current" Custom-reset-current t]
	   ["Reset to Saved" Custom-reset-saved t]
	   ["Reset to Standard Settings" Custom-reset-standard t]))
	(message "\"Org\"-menu now contains full customization menu"))
    (error "Cannot expand menu (outdated version of cus-edit.el)")))

;;;; Miscellaneous stuff

;;; Generally useful functions

(defun org-get-at-eol (property n)
  "Get text property PROPERTY at the end of line less N characters."
  (get-text-property (- (point-at-eol) n) property))

(defun org-find-text-property-in-string (prop s)
  "Return the first non-nil value of property PROP in string S."
  (or (get-text-property 0 prop s)
      (get-text-property (or (next-single-property-change 0 prop s) 0)
			 prop s)))

(defun org-display-warning (message)
  "Display the given MESSAGE as a warning."
  (display-warning 'org message :warning))

(defun org-eval (form)
  "Eval FORM and return result."
  (condition-case error
      (eval form)
    (error (format "%%![Error: %s]" error))))

(defun org-in-clocktable-p ()
  "Check if the cursor is in a clocktable."
  (let ((pos (point)) start)
    (save-excursion
      (end-of-line 1)
      (and (re-search-backward "^[ \t]*#\\+BEGIN:[ \t]+clocktable" nil t)
	   (setq start (match-beginning 0))
	   (re-search-forward "^[ \t]*#\\+END:.*" nil t)
	   (>= (match-end 0) pos)
	   start))))

(defun org-in-verbatim-emphasis ()
  (save-match-data
    (and (org-in-regexp org-verbatim-re 2)
	 (>= (point) (match-beginning 3))
	 (<= (point) (match-end 4)))))

(defun org-overlay-display (ovl text &optional face evap)
  "Make overlay OVL display TEXT with face FACE."
  (overlay-put ovl 'display text)
  (if face (overlay-put ovl 'face face))
  (if evap (overlay-put ovl 'evaporate t)))

(defun org-overlay-before-string (ovl text &optional face evap)
  "Make overlay OVL display TEXT with face FACE."
  (if face (org-add-props text nil 'face face))
  (overlay-put ovl 'before-string text)
  (if evap (overlay-put ovl 'evaporate t)))

(defun org-find-overlays (prop &optional pos delete)
  "Find all overlays specifying PROP at POS or point.
If DELETE is non-nil, delete all those overlays."
  (let (found)
    (dolist (ov (overlays-at (or pos (point))) found)
      (cond ((not (overlay-get ov prop)))
	    (delete (delete-overlay ov))
	    (t (push ov found))))))

(defun org-goto-marker-or-bmk (marker &optional bookmark)
  "Go to MARKER, widen if necessary.  When marker is not live, try BOOKMARK."
  (if (and marker (marker-buffer marker)
	   (buffer-live-p (marker-buffer marker)))
      (progn
	(pop-to-buffer-same-window (marker-buffer marker))
	(when (or (> marker (point-max)) (< marker (point-min)))
	  (widen))
	(goto-char marker)
	(org-show-context 'org-goto))
    (if bookmark
	(bookmark-jump bookmark)
      (error "Cannot find location"))))

(defun org-quote-csv-field (s)
  "Quote field for inclusion in CSV material."
  (if (string-match "[\",]" s)
      (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\"")
    s))

(defun org-force-self-insert (N)
  "Needed to enforce self-insert under remapping."
  (interactive "p")
  (self-insert-command N))

(defun org-shorten-string (s maxlength)
  "Shorten string S so that it is no longer than MAXLENGTH characters.
If the string is shorter or has length MAXLENGTH, just return the
original string.  If it is longer, the functions finds a space in the
string, breaks this string off at that locations and adds three dots
as ellipsis.  Including the ellipsis, the string will not be longer
than MAXLENGTH.  If finding a good breaking point in the string does
not work, the string is just chopped off in the middle of a word
if necessary."
  (if (<= (length s) maxlength)
      s
    (let* ((n (max (- maxlength 4) 1))
	   (re (concat "\\`\\(.\\{1," (int-to-string n) "\\}[^ ]\\)\\([ ]\\|\\'\\)")))
      (if (string-match re s)
	  (concat (match-string 1 s) "...")
	(concat (substring s 0 (max (- maxlength 3) 0)) "...")))))

(defun org-get-indentation (&optional line)
  "Get the indentation of the current line, interpreting tabs.
When LINE is given, assume it represents a line and compute its indentation."
  (if line
      (when (string-match "^ *" (org-remove-tabs line))
	(match-end 0))
    (save-excursion
      (beginning-of-line 1)
      (skip-chars-forward " \t")
      (current-column))))

(defun org-get-string-indentation (s)
  "What indentation has S due to SPACE and TAB at the beginning of the string?"
  (let ((n -1) (i 0) (w tab-width) c)
    (catch 'exit
      (while (< (setq n (1+ n)) (length s))
	(setq c (aref s n))
	(cond ((= c ?\ ) (setq i (1+ i)))
	      ((= c ?\t) (setq i (* (/ (+ w i) w) w)))
	      (t (throw 'exit t)))))
    i))

(defun org-remove-tabs (s &optional width)
  "Replace tabulators in S with spaces.
Assumes that s is a single line, starting in column 0."
  (setq width (or width tab-width))
  (while (string-match "\t" s)
    (setq s (replace-match
	     (make-string
	      (- (* width (/ (+ (match-beginning 0) width) width))
		 (match-beginning 0)) ?\ )
	     t t s)))
  s)

(defun org-fix-indentation (line ind)
  "Fix indentation in LINE.
IND is a cons cell with target and minimum indentation.
If the current indentation in LINE is smaller than the minimum,
leave it alone.  If it is larger than ind, set it to the target."
  (let* ((l (org-remove-tabs line))
	 (i (org-get-indentation l))
	 (i1 (car ind)) (i2 (cdr ind)))
    (when (>= i i2) (setq l (substring line i2)))
    (if (> i1 0)
	(concat (make-string i1 ?\ ) l)
      l)))

(defun org-remove-indentation (code &optional n)
  "Remove maximum common indentation in string CODE and return it.
N may optionally be the number of columns to remove.  Return CODE
as-is if removal failed."
  (with-temp-buffer
    (insert code)
    (if (org-do-remove-indentation n) (buffer-string) code)))

(defun org-do-remove-indentation (&optional n)
  "Remove the maximum common indentation from the buffer.
When optional argument N is a positive integer, remove exactly
that much characters from indentation, if possible.  Return nil
if it fails."
  (catch :exit
    (goto-char (point-min))
    ;; Find maximum common indentation, if not specified.
    (let ((n (or n
		 (let ((min-ind (point-max)))
		   (save-excursion
		     (while (re-search-forward "^[ \t]*\\S-" nil t)
		       (let ((ind (1- (current-column))))
			 (if (zerop ind) (throw :exit nil)
			   (setq min-ind (min min-ind ind))))))
		   min-ind))))
      (if (zerop n) (throw :exit nil)
	;; Remove exactly N indentation, but give up if not possible.
	(while (not (eobp))
	  (let ((ind (progn (skip-chars-forward " \t") (current-column))))
	    (cond ((eolp) (delete-region (line-beginning-position) (point)))
		  ((< ind n) (throw :exit nil))
		  (t (indent-line-to (- ind n))))
	    (forward-line)))
	;; Signal success.
	t))))

(defun org-fill-template (template alist)
  "Find each %key of ALIST in TEMPLATE and replace it."
  (let ((case-fold-search nil))
    (dolist (entry (sort (copy-sequence alist)
                         (lambda (a b) (< (length (car a)) (length (car b))))))
      (setq template
	    (replace-regexp-in-string
	     (concat "%" (regexp-quote (car entry)))
	     (or (cdr entry) "") template t t)))
    template))

(defun org-base-buffer (buffer)
  "Return the base buffer of BUFFER, if it has one.  Else return the buffer."
  (if (not buffer)
      buffer
    (or (buffer-base-buffer buffer)
	buffer)))

(defun org-wrap (string &optional width lines)
  "Wrap string to either a number of lines, or a width in characters.
If WIDTH is non-nil, the string is wrapped to that width, however many lines
that costs.  If there is a word longer than WIDTH, the text is actually
wrapped to the length of that word.
IF WIDTH is nil and LINES is non-nil, the string is forced into at most that
many lines, whatever width that takes.
The return value is a list of lines, without newlines at the end."
  (let* ((words (split-string string))
	 (maxword (apply 'max (mapcar 'org-string-width words)))
	 w ll)
    (cond (width
	   (org-do-wrap words (max maxword width)))
	  (lines
	   (setq w maxword)
	   (setq ll (org-do-wrap words maxword))
	   (if (<= (length ll) lines)
	       ll
	     (setq ll words)
	     (while (> (length ll) lines)
	       (setq w (1+ w))
	       (setq ll (org-do-wrap words w)))
	     ll))
	  (t (error "Cannot wrap this")))))

(defun org-do-wrap (words width)
  "Create lines of maximum width WIDTH (in characters) from word list WORDS."
  (let (lines line)
    (while words
      (setq line (pop words))
      (while (and words (< (+ (length line) (length (car words))) width))
	(setq line (concat line " " (pop words))))
      (setq lines (push line lines)))
    (nreverse lines)))

(defun org-quote-vert (s)
  "Replace \"|\" with \"\\vert\"."
  (while (string-match "|" s)
    (setq s (replace-match "\\vert" t t s)))
  s)

(defun org-uuidgen-p (s)
  "Is S an ID created by UUIDGEN?"
  (string-match "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\'" (downcase s)))

(defun org-in-src-block-p (&optional inside)
  "Whether point is in a code source block.
When INSIDE is non-nil, don't consider we are within a src block
when point is at #+BEGIN_SRC or #+END_SRC."
  (let ((case-fold-search t))
    (or (and (eq (get-char-property (point) 'src-block) t))
	(and (not inside)
	     (save-match-data
	       (save-excursion
		 (beginning-of-line)
		 (looking-at ".*#\\+\\(begin\\|end\\)_src")))))))

(defun org-context ()
  "Return a list of contexts of the current cursor position.
If several contexts apply, all are returned.
Each context entry is a list with a symbol naming the context, and
two positions indicating start and end of the context.  Possible
contexts are:

:headline         anywhere in a headline
:headline-stars   on the leading stars in a headline
:todo-keyword     on a TODO keyword (including DONE) in a headline
:tags             on the TAGS in a headline
:priority         on the priority cookie in a headline
:item             on the first line of a plain list item
:item-bullet      on the bullet/number of a plain list item
:checkbox         on the checkbox in a plain list item
:table            in an Org table
:table-special    on a special filed in a table
:table-table      in a table.el table
:clocktable       in a clocktable
:src-block        in a source block
:link             on a hyperlink
:keyword          on a keyword: SCHEDULED, DEADLINE, CLOSE, COMMENT.
:target           on a <<target>>
:radio-target     on a <<<radio-target>>>
:latex-fragment   on a LaTeX fragment
:latex-preview    on a LaTeX fragment with overlaid preview image

This function expects the position to be visible because it uses font-lock
faces as a help to recognize the following contexts: :table-special, :link,
and :keyword."
  (let* ((f (get-text-property (point) 'face))
	 (faces (if (listp f) f (list f)))
	 (case-fold-search t)
	 (p (point)) clist o)
    ;; First the large context
    (cond
     ((org-at-heading-p t)
      (push (list :headline (point-at-bol) (point-at-eol)) clist)
      (when (progn
	      (beginning-of-line 1)
	      (looking-at org-todo-line-tags-regexp))
	(push (org-point-in-group p 1 :headline-stars) clist)
	(push (org-point-in-group p 2 :todo-keyword) clist)
	(push (org-point-in-group p 4 :tags) clist))
      (goto-char p)
      (skip-chars-backward "^[\n\r \t") (or (bobp) (backward-char 1))
      (when (looking-at "\\[#[A-Z0-9]\\]")
	(push (org-point-in-group p 0 :priority) clist)))

     ((org-at-item-p)
      (push (org-point-in-group p 2 :item-bullet) clist)
      (push (list :item (point-at-bol)
		  (save-excursion (org-end-of-item) (point)))
	    clist)
      (and (org-at-item-checkbox-p)
	   (push (org-point-in-group p 0 :checkbox) clist)))

     ((org-at-table-p)
      (push (list :table (org-table-begin) (org-table-end)) clist)
      (when (memq 'org-formula faces)
	(push (list :table-special
		    (previous-single-property-change p 'face)
		    (next-single-property-change p 'face)) clist)))
     ((org-at-table-p 'any)
      (push (list :table-table) clist)))
    (goto-char p)

    (let ((case-fold-search t))
      ;; New the "medium" contexts: clocktables, source blocks
      (cond ((org-in-clocktable-p)
	     (push (list :clocktable
			 (and (or (looking-at "[ \t]*\\(#\\+BEGIN: clocktable\\)")
				  (re-search-backward "[ \t]*\\(#+BEGIN: clocktable\\)" nil t))
			      (match-beginning 1))
			 (and (re-search-forward "[ \t]*#\\+END:?" nil t)
			      (match-end 0))) clist))
	    ((org-in-src-block-p)
	     (push (list :src-block
			 (and (or (looking-at "[ \t]*\\(#\\+BEGIN_SRC\\)")
				  (re-search-backward "[ \t]*\\(#+BEGIN_SRC\\)" nil t))
			      (match-beginning 1))
			 (and (search-forward "#+END_SRC" nil t)
			      (match-beginning 0))) clist))))
    (goto-char p)

    ;; Now the small context
    (cond
     ((org-at-timestamp-p)
      (push (org-point-in-group p 0 :timestamp) clist))
     ((memq 'org-link faces)
      (push (list :link
		  (previous-single-property-change p 'face)
		  (next-single-property-change p 'face)) clist))
     ((memq 'org-special-keyword faces)
      (push (list :keyword
		  (previous-single-property-change p 'face)
		  (next-single-property-change p 'face)) clist))
     ((org-at-target-p)
      (push (org-point-in-group p 0 :target) clist)
      (goto-char (1- (match-beginning 0)))
      (when (looking-at org-radio-target-regexp)
	(push (org-point-in-group p 0 :radio-target) clist))
      (goto-char p))
     ((setq o (cl-some
	       (lambda (o)
		 (and (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
		      o))
	       (overlays-at (point))))
      (push (list :latex-fragment
		  (overlay-start o) (overlay-end o)) clist)
      (push (list :latex-preview
		  (overlay-start o) (overlay-end o)) clist))
     ((org-inside-LaTeX-fragment-p)
      ;; FIXME: positions wrong.
      (push (list :latex-fragment (point) (point)) clist)))

    (setq clist (nreverse (delq nil clist)))
    clist))

(defun org-in-regexp (regexp &optional nlines visually)
  "Check if point is inside a match of REGEXP.

Normally only the current line is checked, but you can include
NLINES extra lines around point into the search.  If VISUALLY is
set, require that the cursor is not after the match but really
on, so that the block visually is on the match.

Return nil or a cons cell (BEG . END) where BEG and END are,
respectively, the positions at the beginning and the end of the
match."
  (catch :exit
    (let ((pos (point))
          (eol (line-end-position (if nlines (1+ nlines) 1))))
      (save-excursion
	(beginning-of-line (- 1 (or nlines 0)))
	(while (and (re-search-forward regexp eol t)
		    (<= (match-beginning 0) pos))
	  (let ((end (match-end 0)))
	    (when (or (> end pos) (and (= end pos) (not visually)))
	      (throw :exit (cons (match-beginning 0) (match-end 0))))))))))

(defun org-between-regexps-p (start-re end-re &optional lim-up lim-down)
  "Non-nil when point is between matches of START-RE and END-RE.

Also return a non-nil value when point is on one of the matches.

Optional arguments LIM-UP and LIM-DOWN bound the search; they are
buffer positions.  Default values are the positions of headlines
surrounding the point.

The functions returns a cons cell whose car (resp. cdr) is the
position before START-RE (resp. after END-RE)."
  (save-match-data
    (let ((pos (point))
	  (limit-up (or lim-up (save-excursion (outline-previous-heading))))
	  (limit-down (or lim-down (save-excursion (outline-next-heading))))
	  beg end)
      (save-excursion
	;; Point is on a block when on START-RE or if START-RE can be
	;; found before it...
	(and (or (org-in-regexp start-re)
		 (re-search-backward start-re limit-up t))
	     (setq beg (match-beginning 0))
	     ;; ... and END-RE after it...
	     (goto-char (match-end 0))
	     (re-search-forward end-re limit-down t)
	     (> (setq end (match-end 0)) pos)
	     ;; ... without another START-RE in-between.
	     (goto-char (match-beginning 0))
	     (not (re-search-backward start-re (1+ beg) t))
	     ;; Return value.
	     (cons beg end))))))

(defun org-in-block-p (names)
  "Non-nil when point belongs to a block whose name belongs to NAMES.

NAMES is a list of strings containing names of blocks.

Return first block name matched, or nil.  Beware that in case of
nested blocks, the returned name may not belong to the closest
block from point."
  (save-match-data
    (catch 'exit
      (let ((case-fold-search t)
	    (lim-up (save-excursion (outline-previous-heading)))
	    (lim-down (save-excursion (outline-next-heading))))
	(dolist (name names)
	  (let ((n (regexp-quote name)))
	    (when (org-between-regexps-p
		   (concat "^[ \t]*#\\+begin_" n)
		   (concat "^[ \t]*#\\+end_" n)
		   lim-up lim-down)
	      (throw 'exit n)))))
      nil)))

(defun org-occur-in-agenda-files (regexp &optional _nlines)
  "Call `multi-occur' with buffers for all agenda files."
  (interactive "sOrg-files matching: ")
  (let* ((files (org-agenda-files))
	 (tnames (mapcar #'file-truename files))
	 (extra org-agenda-text-search-extra-files))
    (when (eq (car extra) 'agenda-archives)
      (setq extra (cdr extra))
      (setq files (org-add-archive-files files)))
    (dolist (f extra)
      (unless (member (file-truename f) tnames)
	(unless (member f files) (setq files (append files (list f))))
	(setq tnames (append tnames (list (file-truename f))))))
    (multi-occur
     (mapcar (lambda (x)
	       (with-current-buffer
		   ;; FIXME: Why not just (find-file-noselect x)?
		   ;; Is it to avoid the "revert buffer" prompt?
		   (or (get-file-buffer x) (find-file-noselect x))
		 (widen)
		 (current-buffer)))
	     files)
     regexp)))

(add-hook 'occur-mode-find-occurrence-hook
	  (lambda () (when (derived-mode-p 'org-mode) (org-reveal))))

(defun org-occur-link-in-agenda-files ()
  "Create a link and search for it in the agendas.
The link is not stored in `org-stored-links', it is just created
for the search purpose."
  (interactive)
  (let ((link (condition-case nil
		  (org-store-link nil)
		(error "Unable to create a link to here"))))
    (org-occur-in-agenda-files (regexp-quote link))))

(defun org-reverse-string (string)
  "Return the reverse of STRING."
  (apply 'string (reverse (string-to-list string))))

;; defsubst org-uniquify must be defined before first use

(defun org-uniquify-alist (alist)
  "Merge elements of ALIST with the same key.

For example, in this alist:

\(org-uniquify-alist \\='((a 1) (b 2) (a 3)))
  => \\='((a 1 3) (b 2))

merge (a 1) and (a 3) into (a 1 3).

The function returns the new ALIST."
  (let (rtn)
    (dolist (e alist rtn)
      (let (n)
	(if (not (assoc (car e) rtn))
	    (push e rtn)
	  (setq n (cons (car e) (append (cdr (assoc (car e) rtn)) (cdr e))))
	  (setq rtn (assq-delete-all (car e) rtn))
	  (push n rtn))))))

(defun org-delete-all (elts list)
  "Remove all elements in ELTS from LIST.
Comparison is done with `equal'.  It is a destructive operation
that may remove elements by altering the list structure."
  (while elts
    (setq list (delete (pop elts) list)))
  list)

(defun org-back-over-empty-lines ()
  "Move backwards over whitespace, to the beginning of the first empty line.
Returns the number of empty lines passed."
  (let ((pos (point)))
    (if (cdr (assq 'heading org-blank-before-new-entry))
	(skip-chars-backward " \t\n\r")
      (unless (eobp)
	(forward-line -1)))
    (beginning-of-line 2)
    (goto-char (min (point) pos))
    (count-lines (point) pos)))

(defun org-skip-whitespace ()
  (skip-chars-forward " \t\n\r"))

(defun org-point-in-group (point group &optional context)
  "Check if POINT is in match-group GROUP.
If CONTEXT is non-nil, return a list with CONTEXT and the boundaries of the
match.  If the match group does not exist or point is not inside it,
return nil."
  (and (match-beginning group)
       (>= point (match-beginning group))
       (<= point (match-end group))
       (if context
	   (list context (match-beginning group) (match-end group))
	 t)))

(defun org-switch-to-buffer-other-window (&rest args)
  "Switch to buffer in a second window on the current frame.
In particular, do not allow pop-up frames.
Returns the newly created buffer."
  (org-no-popups
   (apply 'switch-to-buffer-other-window args)))

(defun org-combine-plists (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the first list, and then setting properties
from the other lists.  Settings in the last list are the most significant
ones and overrule settings in the other lists."
  (let ((rtn (copy-sequence (pop plists)))
	p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
	(setq p (pop ls) v (pop ls))
	(setq rtn (plist-put rtn p v))))
    rtn))

(defun org-replace-escapes (string table)
  "Replace %-escapes in STRING with values in TABLE.
TABLE is an association list with keys like \"%a\" and string values.
The sequences in STRING may contain normal field width and padding information,
for example \"%-5s\".  Replacements happen in the sequence given by TABLE,
so values can contain further %-escapes if they are define later in TABLE."
  (let ((tbl (copy-alist table))
	(case-fold-search nil)
        (pchg 0)
        re rpl)
    (dolist (e tbl)
      (setq re (concat "%-?[0-9.]*" (substring (car e) 1)))
      (when (and (cdr e) (string-match re (cdr e)))
        (let ((sref (substring (cdr e) (match-beginning 0) (match-end 0)))
              (safe "SREF"))
          (add-text-properties 0 3 (list 'sref sref) safe)
          (setcdr e (replace-match safe t t (cdr e)))))
      (while (string-match re string)
        (setq rpl (format (concat (substring (match-string 0 string) 0 -1) "s")
                          (cdr e)))
        (setq string (replace-match rpl t t string))))
    (while (setq pchg (next-property-change pchg string))
      (let ((sref (get-text-property pchg 'sref string)))
	(when (and sref (string-match "SREF" string pchg))
	  (setq string (replace-match sref t t string)))))
    string))

(defun org-find-base-buffer-visiting (file)
  "Like `find-buffer-visiting' but always return the base buffer and
not an indirect buffer."
  (let ((buf (or (get-file-buffer file)
		 (find-buffer-visiting file))))
    (if buf
	(or (buffer-base-buffer buf) buf)
      nil)))

;;; TODO: Only called once, from ox-odt which should probably use
;;; org-export-inline-image-p or something.
(defun org-file-image-p (file)
  "Return non-nil if FILE is an image."
  (save-match-data
    (string-match (image-file-name-regexp) file)))

(defun org-get-cursor-date (&optional with-time)
  "Return the date at cursor in as a time.
This works in the calendar and in the agenda, anywhere else it just
returns the current time.
If WITH-TIME is non-nil, returns the time of the event at point (in
the agenda) or the current time of the day."
  (let (date day defd tp hod mod)
    (when with-time
      (setq tp (get-text-property (point) 'time))
      (when (and tp (string-match "\\([0-9][0-9]\\):\\([0-9][0-9]\\)" tp))
	(setq hod (string-to-number (match-string 1 tp))
	      mod (string-to-number (match-string 2 tp))))
      (or tp (let ((now (decode-time)))
	       (setq hod (nth 2 now)
		     mod (nth 1 now)))))
    (cond
     ((eq major-mode 'calendar-mode)
      (setq date (calendar-cursor-to-date)
	    defd (encode-time 0 (or mod 0) (or hod 0)
			      (nth 1 date) (nth 0 date) (nth 2 date))))
     ((eq major-mode 'org-agenda-mode)
      (setq day (get-text-property (point) 'day))
      (when day
	(setq date (calendar-gregorian-from-absolute day)
	      defd (encode-time 0 (or mod 0) (or hod 0)
				(nth 1 date) (nth 0 date) (nth 2 date))))))
    (or defd (current-time))))

(defun org-mark-subtree (&optional up)
  "Mark the current subtree.
This puts point at the start of the current subtree, and mark at
the end.  If a numeric prefix UP is given, move up into the
hierarchy of headlines by UP levels before marking the subtree."
  (interactive "P")
  (org-with-limited-levels
   (cond ((org-at-heading-p) (beginning-of-line))
	 ((org-before-first-heading-p) (user-error "Not in a subtree"))
	 (t (outline-previous-visible-heading 1))))
  (when up (while (and (> up 0) (org-up-heading-safe)) (cl-decf up)))
  (if (called-interactively-p 'any)
      (call-interactively 'org-mark-element)
    (org-mark-element)))

(defun org-file-newer-than-p (file time)
  "Non-nil if FILE is newer than TIME.
FILE is a filename, as a string, TIME is a list of integers, as
returned by, e.g., `current-time'."
  (and (file-exists-p file)
       ;; Only compare times up to whole seconds as some file-systems
       ;; (e.g. HFS+) do not retain any finer granularity.  As
       ;; a consequence, make sure we return non-nil when the two
       ;; times are equal.
       (not (time-less-p (cl-subseq (nth 5 (file-attributes file)) 0 2)
			 (cl-subseq time 0 2)))))

(defun org-compile-file (source process ext &optional err-msg log-buf spec)
  "Compile a SOURCE file using PROCESS.

PROCESS is either a function or a list of shell commands, as
strings.  EXT is a file extension, without the leading dot, as
a string.  It is used to check if the process actually succeeded.

PROCESS must create a file with the same base name and directory
as SOURCE, but ending with EXT.  The function then returns its
filename.  Otherwise, it raises an error.  The error message can
then be refined by providing string ERR-MSG, which is appended to
the standard message.

If PROCESS is a function, it is called with a single argument:
the SOURCE file.

If it is a list of commands, each of them is called using
`shell-command'.  By default, in each command, %b, %f, %F, %o and
%O are replaced with, respectively, SOURCE base name, name, full
name, directory and absolute output file name.  It is possible,
however, to use more place-holders by specifying them in optional
argument SPEC, as an alist following the pattern

  (CHARACTER . REPLACEMENT-STRING).

When PROCESS is a list of commands, optional argument LOG-BUF can
be set to a buffer or a buffer name.  `shell-command' then uses
it for output."
  (let* ((base-name (file-name-base source))
	 (full-name (file-truename source))
	 (out-dir (or (file-name-directory source) "./"))
	 (output (expand-file-name (concat base-name "." ext) out-dir))
	 (time (current-time))
	 (err-msg (if (stringp err-msg) (concat ".  " err-msg) "")))
    (save-window-excursion
      (pcase process
	((pred functionp) (funcall process (shell-quote-argument source)))
	((pred consp)
	 (let ((log-buf (and log-buf (get-buffer-create log-buf)))
	       (spec (append spec
			     `((?b . ,(shell-quote-argument base-name))
			       (?f . ,(shell-quote-argument source))
			       (?F . ,(shell-quote-argument full-name))
			       (?o . ,(shell-quote-argument out-dir))
			       (?O . ,(shell-quote-argument output))))))
	   (dolist (command process)
	     (shell-command (format-spec command spec) log-buf))
	   (when log-buf (with-current-buffer log-buf (compilation-mode)))))
	(_ (error "No valid command to process %S%s" source err-msg))))
    ;; Check for process failure.  Output file is expected to be
    ;; located in the same directory as SOURCE.
    (unless (org-file-newer-than-p output time)
      (error (format "File %S wasn't produced%s" output err-msg)))
    output))

;;; Indentation

(defvar org-element-greater-elements)
(defun org--get-expected-indentation (element contentsp)
  "Expected indentation column for current line, according to ELEMENT.
ELEMENT is an element containing point.  CONTENTSP is non-nil
when indentation is to be computed according to contents of
ELEMENT."
  (let ((type (org-element-type element))
	(start (org-element-property :begin element))
	(post-affiliated (org-element-property :post-affiliated element)))
    (org-with-wide-buffer
     (cond
      (contentsp
       (cl-case type
	 ((diary-sexp footnote-definition) 0)
	 ((headline inlinetask nil)
	  (if (not org-adapt-indentation) 0
	    (let ((level (org-current-level)))
	      (if level (1+ level) 0))))
	 ((item plain-list) (org-list-item-body-column post-affiliated))
	 (t
	  (goto-char start)
	  (org-get-indentation))))
      ((memq type '(headline inlinetask nil))
       (if (org-match-line "[ \t]*$")
	   (org--get-expected-indentation element t)
	 0))
      ((memq type '(diary-sexp footnote-definition)) 0)
      ;; First paragraph of a footnote definition or an item.
      ;; Indent like parent.
      ((< (line-beginning-position) start)
       (org--get-expected-indentation
	(org-element-property :parent element) t))
      ;; At first line: indent according to previous sibling, if any,
      ;; ignoring footnote definitions and inline tasks, or parent's
      ;; contents.
      ((= (line-beginning-position) start)
       (catch 'exit
	 (while t
	   (if (= (point-min) start) (throw 'exit 0)
	     (goto-char (1- start))
	     (let* ((previous (org-element-at-point))
		    (parent previous))
	       (while (and parent (<= (org-element-property :end parent) start))
		 (setq previous parent
		       parent (org-element-property :parent parent)))
	       (cond
		((not previous) (throw 'exit 0))
		((> (org-element-property :end previous) start)
		 (throw 'exit (org--get-expected-indentation previous t)))
		((memq (org-element-type previous)
		       '(footnote-definition inlinetask))
		 (setq start (org-element-property :begin previous)))
		(t (goto-char (org-element-property :begin previous))
		   (throw 'exit
			  (if (bolp) (org-get-indentation)
			    ;; At first paragraph in an item or
			    ;; a footnote definition.
			    (org--get-expected-indentation
			     (org-element-property :parent previous) t))))))))))
      ;; Otherwise, move to the first non-blank line above.
      (t
       (beginning-of-line)
       (let ((pos (point)))
	 (skip-chars-backward " \r\t\n")
	 (cond
	  ;; Two blank lines end a footnote definition or a plain
	  ;; list.  When we indent an empty line after them, the
	  ;; containing list or footnote definition is over, so it
	  ;; qualifies as a previous sibling.  Therefore, we indent
	  ;; like its first line.
	  ((and (memq type '(footnote-definition plain-list))
		(> (count-lines (point) pos) 2))
	   (goto-char start)
	   (org-get-indentation))
	  ;; Line above is the first one of a paragraph at the
	  ;; beginning of an item or a footnote definition.  Indent
	  ;; like parent.
	  ((< (line-beginning-position) start)
	   (org--get-expected-indentation
	    (org-element-property :parent element) t))
	  ;; Line above is the beginning of an element, i.e., point
	  ;; was originally on the blank lines between element's start
	  ;; and contents.
	  ((= (line-beginning-position) post-affiliated)
	   (org--get-expected-indentation element t))
	  ;; POS is after contents in a greater element.  Indent like
	  ;; the beginning of the element.
	  ((and (memq type org-element-greater-elements)
		(let ((cend (org-element-property :contents-end element)))
		  (and cend (<= cend pos))))
	   ;; As a special case, if point is at the end of a footnote
	   ;; definition or an item, indent like the very last element
	   ;; within.  If that last element is an item, indent like
	   ;; its contents.
	   (if (memq type '(footnote-definition item plain-list))
	       (let ((last (org-element-at-point)))
		 (goto-char pos)
		 (org--get-expected-indentation
		  last (eq (org-element-type last) 'item)))
	     (goto-char start)
	     (org-get-indentation)))
	  ;; In any other case, indent like the current line.
	  (t (org-get-indentation)))))))))

(defun org--align-node-property ()
  "Align node property at point.
Alignment is done according to `org-property-format', which see."
  (when (save-excursion
	  (beginning-of-line)
	  (looking-at org-property-re))
    (replace-match
     (concat (match-string 4)
	     (org-trim
	      (format org-property-format (match-string 1) (match-string 3))))
     t t)))

(defun org-indent-line ()
  "Indent line depending on context.

Indentation is done according to the following rules:

  - Footnote definitions, diary sexps, headlines and inline tasks
    have to start at column 0.

  - On the very first line of an element, consider, in order, the
    next rules until one matches:

    1. If there's a sibling element before, ignoring footnote
       definitions and inline tasks, indent like its first line.

    2. If element has a parent, indent like its contents.  More
       precisely, if parent is an item, indent after the
       description part, if any, or the bullet (see
       `org-list-description-max-indent').  Else, indent like
       parent's first line.

    3. Otherwise, indent relatively to current level, if
       `org-adapt-indentation' is non-nil, or to left margin.

  - On a blank line at the end of an element, indent according to
    the type of the element.  More precisely

    1. If element is a plain list, an item, or a footnote
       definition, indent like the very last element within.

    2. If element is a paragraph, indent like its last non blank
       line.

    3. Otherwise, indent like its very first line.

  - In the code part of a source block, use language major mode
    to indent current line if `org-src-tab-acts-natively' is
    non-nil.  If it is nil, do nothing.

  - Otherwise, indent like the first non-blank line above.

The function doesn't indent an item as it could break the whole
list structure.  Instead, use \\<org-mode-map>`\\[org-shiftmetaleft]' or \
`\\[org-shiftmetaright]'.

Also align node properties according to `org-property-format'."
  (interactive)
  (cond
   (orgstruct-is-++
    (let ((indent-line-function
	   (cl-cadadr (assq 'indent-line-function org-fb-vars))))
      (indent-according-to-mode)))
   ((org-at-heading-p) 'noindent)
   (t
    (let* ((element (save-excursion (beginning-of-line) (org-element-at-point)))
	   (type (org-element-type element)))
      (cond ((and (memq type '(plain-list item))
		  (= (line-beginning-position)
		     (org-element-property :post-affiliated element)))
	     'noindent)
	    ((and (eq type 'latex-environment)
		  (>= (point) (org-element-property :post-affiliated element))
		  (< (point) (org-with-wide-buffer
			      (goto-char (org-element-property :end element))
			      (skip-chars-backward " \r\t\n")
			      (line-beginning-position 2))))
	     'noindent)
	    ((and (eq type 'src-block)
		  org-src-tab-acts-natively
		  (> (line-beginning-position)
		     (org-element-property :post-affiliated element))
		  (< (line-beginning-position)
		     (org-with-wide-buffer
		      (goto-char (org-element-property :end element))
		      (skip-chars-backward " \r\t\n")
		      (line-beginning-position))))
	     (org-babel-do-key-sequence-in-edit-buffer (kbd "TAB")))
	    (t
	     (let ((column (org--get-expected-indentation element nil)))
	       ;; Preserve current column.
	       (if (<= (current-column) (current-indentation))
		   (indent-line-to column)
		 (save-excursion (indent-line-to column))))
	     ;; Align node property.  Also preserve current column.
	     (when (eq type 'node-property)
	       (let ((column (current-column)))
		 (org--align-node-property)
		 (org-move-to-column column)))))))))

(defun org-indent-region (start end)
  "Indent each non-blank line in the region.
Called from a program, START and END specify the region to
indent.  The function will not indent contents of example blocks,
verse blocks and export blocks as leading white spaces are
assumed to be significant there."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (skip-chars-forward " \r\t\n")
    (unless (eobp) (beginning-of-line))
    (let ((indent-to
	   (lambda (ind pos)
	     ;; Set IND as indentation for all lines between point and
	     ;; POS.  Blank lines are ignored.  Leave point after POS
	     ;; once done.
	     (let ((limit (copy-marker pos)))
	       (while (< (point) limit)
		 (unless (looking-at-p "[ \t]*$") (indent-line-to ind))
		 (forward-line))
	       (set-marker limit nil))))
	  (end (copy-marker end)))
      (while (< (point) end)
	(if (or (looking-at-p " \r\t\n") (org-at-heading-p)) (forward-line)
	  (let* ((element (org-element-at-point))
		 (type (org-element-type element))
		 (element-end (copy-marker (org-element-property :end element)))
		 (ind (org--get-expected-indentation element nil)))
	    (cond
	     ;; Element indented as a single block.  Example blocks
	     ;; preserving indentation are a special case since the
	     ;; "contents" must not be indented whereas the block
	     ;; boundaries can.
	     ((or (memq type '(export-block latex-environment))
		  (and (eq type 'example-block)
		       (not
			(or org-src-preserve-indentation
			    (org-element-property :preserve-indent element)))))
	      (let ((offset (- ind (org-get-indentation))))
		(unless (zerop offset)
		  (indent-rigidly (org-element-property :begin element)
				  (org-element-property :end element)
				  offset)))
	      (goto-char element-end))
	     ;; Elements indented line wise.  Be sure to exclude
	     ;; example blocks (preserving indentation) and source
	     ;; blocks from this category as they are treated
	     ;; specially later.
	     ((or (memq type '(paragraph table table-row))
		  (not (or (org-element-property :contents-begin element)
			   (memq type '(example-block src-block)))))
	      (when (eq type 'node-property)
		(org--align-node-property)
		(beginning-of-line))
	      (funcall indent-to ind (min element-end end)))
	     ;; Elements consisting of three parts: before the
	     ;; contents, the contents, and after the contents.  The
	     ;; contents are treated specially, according to the
	     ;; element type, or not indented at all.  Other parts are
	     ;; indented as a single block.
	     (t
	      (let* ((post (copy-marker
			    (org-element-property :post-affiliated element)))
		     (cbeg
		      (copy-marker
		       (cond
			((not (org-element-property :contents-begin element))
			 ;; Fake contents for source blocks.
			 (org-with-wide-buffer
			  (goto-char post)
			  (line-beginning-position 2)))
			((memq type '(footnote-definition item plain-list))
			 ;; Contents in these elements could start on
			 ;; the same line as the beginning of the
			 ;; element.  Make sure we start indenting
			 ;; from the second line.
			 (org-with-wide-buffer
			  (goto-char post)
			  (end-of-line)
			  (skip-chars-forward " \r\t\n")
			  (if (eobp) (point) (line-beginning-position))))
			(t (org-element-property :contents-begin element)))))
		     (cend (copy-marker
			    (or (org-element-property :contents-end element)
				;; Fake contents for source blocks.
				(org-with-wide-buffer
				 (goto-char element-end)
				 (skip-chars-backward " \r\t\n")
				 (line-beginning-position)))
			    t)))
		;; Do not change items indentation individually as it
		;; might break the list as a whole.  On the other
		;; hand, when at a plain list, indent it as a whole.
		(cond ((eq type 'plain-list)
		       (let ((offset (- ind (org-get-indentation))))
			 (unless (zerop offset)
			   (indent-rigidly (org-element-property :begin element)
					   (org-element-property :end element)
					   offset))
			 (goto-char cbeg)))
		      ((eq type 'item) (goto-char cbeg))
		      (t (funcall indent-to ind (min cbeg end))))
		(when (< (point) end)
		  (cl-case type
		    ((example-block verse-block))
		    (src-block
		     ;; In a source block, indent source code
		     ;; according to language major mode, but only if
		     ;; `org-src-tab-acts-natively' is non-nil.
		     (when (and (< (point) end) org-src-tab-acts-natively)
		       (ignore-errors
			 (org-babel-do-in-edit-buffer
			  (indent-region (point-min) (point-max))))))
		    (t (org-indent-region (point) (min cend end))))
		  (goto-char (min cend end))
		  (when (< (point) end)
		    (funcall indent-to ind (min element-end end))))
		(set-marker post nil)
		(set-marker cbeg nil)
		(set-marker cend nil))))
	    (set-marker element-end nil))))
      (set-marker end nil))))

(defun org-indent-drawer ()
  "Indent the drawer at point."
  (interactive)
  (unless (save-excursion
	    (beginning-of-line)
	    (looking-at-p org-drawer-regexp))
    (user-error "Not at a drawer"))
  (let ((element (org-element-at-point)))
    (unless (memq (org-element-type element) '(drawer property-drawer))
      (user-error "Not at a drawer"))
    (org-with-wide-buffer
     (org-indent-region (org-element-property :begin element)
			(org-element-property :end element))))
  (message "Drawer at point indented"))

(defun org-indent-block ()
  "Indent the block at point."
  (interactive)
  (unless (save-excursion
	    (beginning-of-line)
	    (let ((case-fold-search t))
	      (looking-at-p "[ \t]*#\\+\\(begin\\|end\\)_")))
    (user-error "Not at a block"))
  (let ((element (org-element-at-point)))
    (unless (memq (org-element-type element)
		  '(comment-block center-block dynamic-block example-block
				  export-block quote-block special-block
				  src-block verse-block))
      (user-error "Not at a block"))
    (org-with-wide-buffer
     (org-indent-region (org-element-property :begin element)
			(org-element-property :end element))))
  (message "Block at point indented"))


;;; Filling

;; We use our own fill-paragraph and auto-fill functions.

;; `org-fill-paragraph' relies on adaptive filling and context
;; checking.  Appropriate `fill-prefix' is computed with
;; `org-adaptive-fill-function'.

;; `org-auto-fill-function' takes care of auto-filling.  It calls
;; `do-auto-fill' only on valid areas with `fill-prefix' shadowed with
;; `org-adaptive-fill-function' value.  Internally,
;; `org-comment-line-break-function' breaks the line.

;; `org-setup-filling' installs filling and auto-filling related
;; variables during `org-mode' initialization.

(defvar org-element-paragraph-separate) ; org-element.el
(defun org-setup-filling ()
  (require 'org-element)
  ;; Prevent auto-fill from inserting unwanted new items.
  (when (boundp 'fill-nobreak-predicate)
    (setq-local
     fill-nobreak-predicate
     (org-uniquify
      (append fill-nobreak-predicate
	      '(org-fill-line-break-nobreak-p
		org-fill-n-macro-as-item-nobreak-p
		org-fill-paragraph-with-timestamp-nobreak-p)))))
  (let ((paragraph-ending (substring org-element-paragraph-separate 1)))
    (setq-local paragraph-start paragraph-ending)
    (setq-local paragraph-separate paragraph-ending))
  (setq-local fill-paragraph-function 'org-fill-paragraph)
  (setq-local auto-fill-inhibit-regexp nil)
  (setq-local adaptive-fill-function 'org-adaptive-fill-function)
  (setq-local normal-auto-fill-function 'org-auto-fill-function)
  (setq-local comment-line-break-function 'org-comment-line-break-function))

(defun org-fill-line-break-nobreak-p ()
  "Non-nil when a new line at point would create an Org line break."
  (save-excursion
    (skip-chars-backward "[ \t]")
    (skip-chars-backward "\\\\")
    (looking-at "\\\\\\\\\\($\\|[^\\\\]\\)")))

(defun org-fill-paragraph-with-timestamp-nobreak-p ()
  "Non-nil when a new line at point would split a timestamp."
  (and (org-at-timestamp-p 'lax)
       (not (looking-at org-ts-regexp-both))))

(defun org-fill-n-macro-as-item-nobreak-p ()
  "Non-nil when a new line at point would create a new list."
  ;; During export, a "n" macro followed by a dot or a closing
  ;; parenthesis can end up being parsed as a new list item.
  (looking-at-p "[ \t]*{{{n\\(?:([^\n)]*)\\)?}}}[.)]\\(?:$\\| \\)"))

(declare-function message-in-body-p "message" ())
(defvar orgtbl-line-start-regexp) ; From org-table.el
(defun org-adaptive-fill-function ()
  "Compute a fill prefix for the current line.
Return fill prefix, as a string, or nil if current line isn't
meant to be filled.  For convenience, if `adaptive-fill-regexp'
matches in paragraphs or comments, use it."
  (catch 'exit
    (when (derived-mode-p 'message-mode)
      (save-excursion
	(beginning-of-line)
	(cond ((not (message-in-body-p)) (throw 'exit nil))
	      ((looking-at-p org-table-line-regexp) (throw 'exit nil))
	      ((looking-at message-cite-prefix-regexp)
	       (throw 'exit (match-string-no-properties 0)))
	      ((looking-at org-outline-regexp)
	       (throw 'exit (make-string (length (match-string 0)) ?\s))))))
    (org-with-wide-buffer
     (unless (org-at-heading-p)
       (let* ((p (line-beginning-position))
	      (element (save-excursion
			 (beginning-of-line)
			 (org-element-at-point)))
	      (type (org-element-type element))
	      (post-affiliated (org-element-property :post-affiliated element)))
	 (unless (< p post-affiliated)
	   (cl-case type
	     (comment
	      (save-excursion
		(beginning-of-line)
		(looking-at "[ \t]*")
		(concat (match-string 0) "# ")))
	     (footnote-definition "")
	     ((item plain-list)
	      (make-string (org-list-item-body-column post-affiliated) ?\s))
	     (paragraph
	      ;; Fill prefix is usually the same as the current line,
	      ;; unless the paragraph is at the beginning of an item.
	      (let ((parent (org-element-property :parent element)))
		(save-excursion
		  (beginning-of-line)
		  (cond ((eq (org-element-type parent) 'item)
			 (make-string (org-list-item-body-column
				       (org-element-property :begin parent))
				      ?\s))
			((and adaptive-fill-regexp
			      ;; Locally disable
			      ;; `adaptive-fill-function' to let
			      ;; `fill-context-prefix' handle
			      ;; `adaptive-fill-regexp' variable.
			      (let (adaptive-fill-function)
				(fill-context-prefix
				 post-affiliated
				 (org-element-property :end element)))))
			((looking-at "[ \t]+") (match-string 0))
			(t  "")))))
	     (comment-block
	      ;; Only fill contents if P is within block boundaries.
	      (let* ((cbeg (save-excursion (goto-char post-affiliated)
					   (forward-line)
					   (point)))
		     (cend (save-excursion
			     (goto-char (org-element-property :end element))
			     (skip-chars-backward " \r\t\n")
			     (line-beginning-position))))
		(when (and (>= p cbeg) (< p cend))
		  (if (save-excursion (beginning-of-line) (looking-at "[ \t]+"))
		      (match-string 0)
		    "")))))))))))

(declare-function message-goto-body "message" ())
(defvar message-cite-prefix-regexp)	; From message.el

(defun org-fill-element (&optional justify)
  "Fill element at point, when applicable.

This function only applies to comment blocks, comments, example
blocks and paragraphs.  Also, as a special case, re-align table
when point is at one.

If JUSTIFY is non-nil (interactively, with prefix argument),
justify as well.  If `sentence-end-double-space' is non-nil, then
period followed by one space does not end a sentence, so don't
break a line there.  The variable `fill-column' controls the
width for filling.

For convenience, when point is at a plain list, an item or
a footnote definition, try to fill the first paragraph within."
  (with-syntax-table org-mode-transpose-word-syntax-table
    ;; Move to end of line in order to get the first paragraph within
    ;; a plain list or a footnote definition.
    (let ((element (save-excursion (end-of-line) (org-element-at-point))))
      ;; First check if point is in a blank line at the beginning of
      ;; the buffer.  In that case, ignore filling.
      (cl-case (org-element-type element)
	;; Use major mode filling function is src blocks.
	(src-block (org-babel-do-key-sequence-in-edit-buffer (kbd "M-q")))
	;; Align Org tables, leave table.el tables as-is.
	(table-row (org-table-align) t)
	(table
	 (when (eq (org-element-property :type element) 'org)
	   (save-excursion
	     (goto-char (org-element-property :post-affiliated element))
	     (org-table-align)))
	 t)
	(paragraph
	 ;; Paragraphs may contain `line-break' type objects.
	 (let ((beg (max (point-min)
			 (org-element-property :contents-begin element)))
	       (end (min (point-max)
			 (org-element-property :contents-end element))))
	   ;; Do nothing if point is at an affiliated keyword.
	   (if (< (line-end-position) beg) t
	     (when (derived-mode-p 'message-mode)
	       ;; In `message-mode', do not fill following citation
	       ;; in current paragraph nor text before message body.
	       (let ((body-start (save-excursion (message-goto-body))))
		 (when body-start (setq beg (max body-start beg))))
	       (when (save-excursion
		       (re-search-forward
			(concat "^" message-cite-prefix-regexp) end t))
		 (setq end (match-beginning 0))))
	     ;; Fill paragraph, taking line breaks into account.
	     (save-excursion
	       (goto-char beg)
	       (let ((cuts (list beg)))
		 (while (re-search-forward "\\\\\\\\[ \t]*\n" end t)
		   (when (eq 'line-break
			     (org-element-type
			      (save-excursion (backward-char)
					      (org-element-context))))
		     (push (point) cuts)))
		 (dolist (c (delq end cuts))
		   (fill-region-as-paragraph c end justify)
		   (setq end c))))
	     t)))
	;; Contents of `comment-block' type elements should be
	;; filled as plain text, but only if point is within block
	;; markers.
	(comment-block
	 (let* ((case-fold-search t)
		(beg (save-excursion
		       (goto-char (org-element-property :begin element))
		       (re-search-forward "^[ \t]*#\\+begin_comment" nil t)
		       (forward-line)
		       (point)))
		(end (save-excursion
		       (goto-char (org-element-property :end element))
		       (re-search-backward "^[ \t]*#\\+end_comment" nil t)
		       (line-beginning-position))))
	   (if (or (< (point) beg) (> (point) end)) t
	     (fill-region-as-paragraph
	      (save-excursion (end-of-line)
			      (re-search-backward "^[ \t]*$" beg 'move)
			      (line-beginning-position))
	      (save-excursion (beginning-of-line)
			      (re-search-forward "^[ \t]*$" end 'move)
			      (line-beginning-position))
	      justify))))
	;; Fill comments.
	(comment
	 (let ((begin (org-element-property :post-affiliated element))
	       (end (org-element-property :end element)))
	   (when (and (>= (point) begin) (<= (point) end))
	     (let ((begin (save-excursion
			    (end-of-line)
			    (if (re-search-backward "^[ \t]*#[ \t]*$" begin t)
				(progn (forward-line) (point))
			      begin)))
		   (end (save-excursion
			  (end-of-line)
			  (if (re-search-forward "^[ \t]*#[ \t]*$" end 'move)
			      (1- (line-beginning-position))
			    (skip-chars-backward " \r\t\n")
			    (line-end-position)))))
	       ;; Do not fill comments when at a blank line.
	       (when (> end begin)
		 (let ((fill-prefix
			(save-excursion
			  (beginning-of-line)
			  (looking-at "[ \t]*#")
			  (let ((comment-prefix (match-string 0)))
			    (goto-char (match-end 0))
			    (if (looking-at adaptive-fill-regexp)
				(concat comment-prefix (match-string 0))
			      (concat comment-prefix " "))))))
		   (save-excursion
		     (fill-region-as-paragraph begin end justify))))))
	   t))
	;; Ignore every other element.
	(otherwise t)))))

(defun org-fill-paragraph (&optional justify region)
  "Fill element at point, when applicable.

This function only applies to comment blocks, comments, example
blocks and paragraphs.  Also, as a special case, re-align table
when point is at one.

For convenience, when point is at a plain list, an item or
a footnote definition, try to fill the first paragraph within.

If JUSTIFY is non-nil (interactively, with prefix argument),
justify as well.  If `sentence-end-double-space' is non-nil, then
period followed by one space does not end a sentence, so don't
break a line there.  The variable `fill-column' controls the
width for filling.

The REGION argument is non-nil if called interactively; in that
case, if Transient Mark mode is enabled and the mark is active,
fill each of the elements in the active region, instead of just
filling the current element."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (if current-prefix-arg 'full) t)))
  (cond
   ((and (derived-mode-p 'message-mode)
	 (or (not (message-in-body-p))
	     (save-excursion (move-beginning-of-line 1)
			     (looking-at message-cite-prefix-regexp))))
    ;; First ensure filling is correct in message-mode.
    (let ((fill-paragraph-function
	   (cl-cadadr (assq 'fill-paragraph-function org-fb-vars)))
	  (fill-prefix (cl-cadadr (assq 'fill-prefix org-fb-vars)))
	  (paragraph-start (cl-cadadr (assq 'paragraph-start org-fb-vars)))
	  (paragraph-separate
	   (cl-cadadr (assq 'paragraph-separate org-fb-vars))))
      (fill-paragraph nil)))
   ((and region transient-mark-mode mark-active
	 (not (eq (region-beginning) (region-end))))
    (let ((origin (point-marker))
	  (start (region-beginning)))
      (unwind-protect
	  (progn
	    (goto-char (region-end))
	    (while (> (point) start)
	      (org-backward-paragraph)
	      (org-fill-element justify)))
	(goto-char origin)
	(set-marker origin nil))))
   (t (org-fill-element justify))))
(org-remap org-mode-map 'fill-paragraph 'org-fill-paragraph)

(defun org-auto-fill-function ()
  "Auto-fill function."
  ;; Check if auto-filling is meaningful.
  (let ((fc (current-fill-column)))
    (when (and fc (> (current-column) fc))
      (let* ((fill-prefix (org-adaptive-fill-function))
	     ;; Enforce empty fill prefix, if required.  Otherwise, it
	     ;; will be computed again.
	     (adaptive-fill-mode (not (equal fill-prefix ""))))
	(when fill-prefix (do-auto-fill))))))

(defun org-comment-line-break-function (&optional soft)
  "Break line at point and indent, continuing comment if within one.
The inserted newline is marked hard if variable
`use-hard-newlines' is true, unless optional argument SOFT is
non-nil."
  (if soft (insert-and-inherit ?\n) (newline 1))
  (save-excursion (forward-char -1) (delete-horizontal-space))
  (delete-horizontal-space)
  (indent-to-left-margin)
  (insert-before-markers-and-inherit fill-prefix))


;;; Fixed Width Areas

(defun org-toggle-fixed-width ()
  "Toggle fixed-width markup.

Add or remove fixed-width markup on current line, whenever it
makes sense.  Return an error otherwise.

If a region is active and if it contains only fixed-width areas
or blank lines, remove all fixed-width markup in it.  If the
region contains anything else, convert all non-fixed-width lines
to fixed-width ones.

Blank lines at the end of the region are ignored unless the
region only contains such lines."
  (interactive)
  (if (not (org-region-active-p))
      ;; No region:
      ;;
      ;; Remove fixed width marker only in a fixed-with element.
      ;;
      ;; Add fixed width maker in paragraphs, in blank lines after
      ;; elements or at the beginning of a headline or an inlinetask,
      ;; and before any one-line elements (e.g., a clock).
      (progn
        (beginning-of-line)
        (let* ((element (org-element-at-point))
               (type (org-element-type element)))
          (cond
           ((and (eq type 'fixed-width)
                 (looking-at "[ \t]*\\(:\\(?: \\|$\\)\\)"))
            (replace-match
	     "" nil nil nil (if (= (line-end-position) (match-end 0)) 0 1)))
           ((and (memq type '(babel-call clock comment diary-sexp headline
					 horizontal-rule keyword paragraph
					 planning))
		 (<= (org-element-property :post-affiliated element) (point)))
            (skip-chars-forward " \t")
            (insert ": "))
           ((and (looking-at-p "[ \t]*$")
                 (or (eq type 'inlinetask)
                     (save-excursion
                       (skip-chars-forward " \r\t\n")
                       (<= (org-element-property :end element) (point)))))
            (delete-region (point) (line-end-position))
            (org-indent-line)
            (insert ": "))
           (t (user-error "Cannot insert a fixed-width line here")))))
    ;; Region active.
    (let* ((begin (save-excursion
                    (goto-char (region-beginning))
                    (line-beginning-position)))
           (end (copy-marker
                 (save-excursion
                   (goto-char (region-end))
                   (unless (eolp) (beginning-of-line))
                   (if (save-excursion (re-search-backward "\\S-" begin t))
                       (progn (skip-chars-backward " \r\t\n") (point))
                     (point)))))
           (all-fixed-width-p
            (catch 'not-all-p
              (save-excursion
                (goto-char begin)
                (skip-chars-forward " \r\t\n")
                (when (eobp) (throw 'not-all-p nil))
                (while (< (point) end)
                  (let ((element (org-element-at-point)))
                    (if (eq (org-element-type element) 'fixed-width)
                        (goto-char (org-element-property :end element))
                      (throw 'not-all-p nil))))
                t))))
      (if all-fixed-width-p
          (save-excursion
            (goto-char begin)
            (while (< (point) end)
              (when (looking-at "[ \t]*\\(:\\(?: \\|$\\)\\)")
                (replace-match
                 "" nil nil nil
                 (if (= (line-end-position) (match-end 0)) 0 1)))
              (forward-line)))
        (let ((min-ind (point-max)))
          ;; Find minimum indentation across all lines.
          (save-excursion
            (goto-char begin)
            (if (not (save-excursion (re-search-forward "\\S-" end t)))
                (setq min-ind 0)
              (catch 'zerop
                (while (< (point) end)
                  (unless (looking-at-p "[ \t]*$")
                    (let ((ind (org-get-indentation)))
                      (setq min-ind (min min-ind ind))
                      (when (zerop ind) (throw 'zerop t))))
                  (forward-line)))))
          ;; Loop over all lines and add fixed-width markup everywhere
          ;; but in fixed-width lines.
          (save-excursion
            (goto-char begin)
            (while (< (point) end)
              (cond
               ((org-at-heading-p)
                (insert ": ")
                (forward-line)
                (while (and (< (point) end) (looking-at-p "[ \t]*$"))
                  (insert ":")
                  (forward-line)))
               ((looking-at-p "[ \t]*:\\( \\|$\\)")
                (let* ((element (org-element-at-point))
                       (element-end (org-element-property :end element)))
                  (if (eq (org-element-type element) 'fixed-width)
                      (progn (goto-char element-end)
                             (skip-chars-backward " \r\t\n")
                             (forward-line))
                    (let ((limit (min end element-end)))
                      (while (< (point) limit)
                        (org-move-to-column min-ind t)
                        (insert ": ")
                        (forward-line))))))
               (t
                (org-move-to-column min-ind t)
                (insert ": ")
                (forward-line)))))))
      (set-marker end nil))))


;;; Comments

;; Org comments syntax is quite complex.  It requires the entire line
;; to be just a comment.  Also, even with the right syntax at the
;; beginning of line, some elements (e.g., verse-block or
;; example-block) don't accept comments.  Usual Emacs comment commands
;; cannot cope with those requirements.  Therefore, Org replaces them.

;; Org still relies on `comment-dwim', but cannot trust
;; `comment-only-p'.  So, `comment-region-function' and
;; `uncomment-region-function' both point
;; to`org-comment-or-uncomment-region'.  Eventually,
;; `org-insert-comment' takes care of insertion of comments at the
;; beginning of line.

;; `org-setup-comments-handling' install comments related variables
;; during `org-mode' initialization.

(defun org-setup-comments-handling ()
  (interactive)
  (setq-local comment-use-syntax nil)
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "^\\s-*#\\(?: \\|$\\)")
  (setq-local comment-insert-comment-function 'org-insert-comment)
  (setq-local comment-region-function 'org-comment-or-uncomment-region)
  (setq-local uncomment-region-function 'org-comment-or-uncomment-region))

(defun org-insert-comment ()
  "Insert an empty comment above current line.
If the line is empty, insert comment at its beginning.  When
point is within a source block, comment according to the related
major mode."
  (if (let ((element (org-element-at-point)))
	(and (eq (org-element-type element) 'src-block)
	     (< (save-excursion
		  (goto-char (org-element-property :post-affiliated element))
		  (line-end-position))
		(point))
	     (> (save-excursion
		  (goto-char (org-element-property :end element))
		  (skip-chars-backward " \r\t\n")
		  (line-beginning-position))
		(point))))
      (org-babel-do-in-edit-buffer (call-interactively 'comment-dwim))
    (beginning-of-line)
    (if (looking-at "\\s-*$") (delete-region (point) (point-at-eol))
      (open-line 1))
    (org-indent-line)
    (insert "# ")))

(defvar comment-empty-lines)		; From newcomment.el.
(defun org-comment-or-uncomment-region (beg end &rest _)
  "Comment or uncomment each non-blank line in the region.
Uncomment each non-blank line between BEG and END if it only
contains commented lines.  Otherwise, comment them.  If region is
strictly within a source block, use appropriate comment syntax."
  (if (let ((element (org-element-at-point)))
	(and (eq (org-element-type element) 'src-block)
	     (< (save-excursion
		  (goto-char (org-element-property :post-affiliated element))
		  (line-end-position))
		beg)
	     (>= (save-excursion
		   (goto-char (org-element-property :end element))
		   (skip-chars-backward " \r\t\n")
		   (line-beginning-position))
		 end)))
      ;; Translate region boundaries for the Org buffer to the source
      ;; buffer.
      (let ((offset (- end beg)))
	(save-excursion
	  (goto-char beg)
	  (org-babel-do-in-edit-buffer
	   (comment-or-uncomment-region (point) (+ offset (point))))))
    (save-restriction
      ;; Restrict region
      (narrow-to-region (save-excursion (goto-char beg)
					(skip-chars-forward " \r\t\n" end)
					(line-beginning-position))
			(save-excursion (goto-char end)
					(skip-chars-backward " \r\t\n" beg)
					(line-end-position)))
      (let ((uncommentp
	     ;; UNCOMMENTP is non-nil when every non blank line between
	     ;; BEG and END is a comment.
	     (save-excursion
	       (goto-char (point-min))
	       (while (and (not (eobp))
			   (let ((element (org-element-at-point)))
			     (and (eq (org-element-type element) 'comment)
				  (goto-char (min (point-max)
						  (org-element-property
						   :end element)))))))
	       (eobp))))
	(if uncommentp
	    ;; Only blank lines and comments in region: uncomment it.
	    (save-excursion
	      (goto-char (point-min))
	      (while (not (eobp))
		(when (looking-at "[ \t]*\\(#\\(?: \\|$\\)\\)")
		  (replace-match "" nil nil nil 1))
		(forward-line)))
	  ;; Comment each line in region.
	  (let ((min-indent (point-max)))
	    ;; First find the minimum indentation across all lines.
	    (save-excursion
	      (goto-char (point-min))
	      (while (and (not (eobp)) (not (zerop min-indent)))
		(unless (looking-at "[ \t]*$")
		  (setq min-indent (min min-indent (current-indentation))))
		(forward-line)))
	    ;; Then loop over all lines.
	    (save-excursion
	      (goto-char (point-min))
	      (while (not (eobp))
		(unless (and (not comment-empty-lines) (looking-at "[ \t]*$"))
		  ;; Don't get fooled by invisible text (e.g. link path)
		  ;; when moving to column MIN-INDENT.
		  (let ((buffer-invisibility-spec nil))
		    (org-move-to-column min-indent t))
		  (insert comment-start))
		(forward-line)))))))))

(defun org-comment-dwim (_arg)
  "Call `comment-dwim' within a source edit buffer if needed."
  (interactive "*P")
  (if (org-in-src-block-p)
      (org-babel-do-in-edit-buffer (call-interactively 'comment-dwim))
    (call-interactively 'comment-dwim)))


;;; Timestamps API

;; This section contains tools to operate on timestamp objects, as
;; returned by, e.g. `org-element-context'.

(defun org-timestamp--to-internal-time (timestamp &optional end)
  "Encode TIMESTAMP object into Emacs internal time.
Use end of date range or time range when END is non-nil."
  (apply #'encode-time
	 (cons 0
	       (mapcar
		(lambda (prop) (or (org-element-property prop timestamp) 0))
		(if end '(:minute-end :hour-end :day-end :month-end :year-end)
		  '(:minute-start :hour-start :day-start :month-start
				  :year-start))))))

(defun org-timestamp-has-time-p (timestamp)
  "Non-nil when TIMESTAMP has a time specified."
  (org-element-property :hour-start timestamp))

(defun org-timestamp-format (timestamp format &optional end utc)
  "Format a TIMESTAMP object into a string.

FORMAT is a format specifier to be passed to
`format-time-string'.

When optional argument END is non-nil, use end of date-range or
time-range, if possible.

When optional argument UTC is non-nil, time will be expressed as
Universal Time."
  (format-time-string
   format (org-timestamp--to-internal-time timestamp end)
   (and utc t)))

(defun org-timestamp-split-range (timestamp &optional end)
  "Extract a TIMESTAMP object from a date or time range.

END, when non-nil, means extract the end of the range.
Otherwise, extract its start.

Return a new timestamp object."
  (let ((type (org-element-property :type timestamp)))
    (if (memq type '(active inactive diary)) timestamp
      (let ((split-ts (org-element-copy timestamp)))
	;; Set new type.
	(org-element-put-property
	 split-ts :type (if (eq type 'active-range) 'active 'inactive))
	;; Copy start properties over end properties if END is
	;; non-nil.  Otherwise, copy end properties over `start' ones.
	(let ((p-alist '((:minute-start . :minute-end)
			 (:hour-start . :hour-end)
			 (:day-start . :day-end)
			 (:month-start . :month-end)
			 (:year-start . :year-end))))
	  (dolist (p-cell p-alist)
	    (org-element-put-property
	     split-ts
	     (funcall (if end #'car #'cdr) p-cell)
	     (org-element-property
	      (funcall (if end #'cdr #'car) p-cell) split-ts)))
	  ;; Eventually refresh `:raw-value'.
	  (org-element-put-property split-ts :raw-value nil)
	  (org-element-put-property
	   split-ts :raw-value (org-element-interpret-data split-ts)))))))

(defun org-timestamp-translate (timestamp &optional boundary)
  "Translate TIMESTAMP object to custom format.

Format string is defined in `org-time-stamp-custom-formats',
which see.

When optional argument BOUNDARY is non-nil, it is either the
symbol `start' or `end'.  In this case, only translate the
starting or ending part of TIMESTAMP if it is a date or time
range.  Otherwise, translate both parts.

Return timestamp as-is if `org-display-custom-times' is nil or if
it has a `diary' type."
  (let ((type (org-element-property :type timestamp)))
    (if (or (not org-display-custom-times) (eq type 'diary))
	(org-element-interpret-data timestamp)
      (let ((fmt (funcall (if (org-timestamp-has-time-p timestamp) #'cdr #'car)
			  org-time-stamp-custom-formats)))
	(if (and (not boundary) (memq type '(active-range inactive-range)))
	    (concat (org-timestamp-format timestamp fmt)
		    "--"
		    (org-timestamp-format timestamp fmt t))
	  (org-timestamp-format timestamp fmt (eq boundary 'end)))))))



;;; Other stuff.

(defvar reftex-docstruct-symbol)
(defvar org--rds)

(defun org-reftex-citation ()
  "Use reftex-citation to insert a citation into the buffer.
This looks for a line like

#+BIBLIOGRAPHY: foo plain option:-d

and derives from it that foo.bib is the bibliography file relevant
for this document.  It then installs the necessary environment for RefTeX
to work in this buffer and calls `reftex-citation'  to insert a citation
into the buffer.

Export of such citations to both LaTeX and HTML is handled by the contributed
package ox-bibtex by Taru Karttunen."
  (interactive)
  (let ((reftex-docstruct-symbol 'org--rds)
	org--rds bib)
    (org-with-wide-buffer
     (let ((case-fold-search t)
	   (re "^[ \t]*#\\+BIBLIOGRAPHY:[ \t]+\\([^ \t\n]+\\)"))
       (if (not (save-excursion
		  (or (re-search-forward re nil t)
		      (re-search-backward re nil t))))
	   (user-error "No bibliography defined in file")
	 (setq bib (concat (match-string 1) ".bib")
	       org--rds (list (list 'bib bib))))))
    (call-interactively 'reftex-citation)))

;;;; Functions extending outline functionality

(defun org-beginning-of-line (&optional n)
  "Go to the beginning of the current visible line.

If this is a headline, and `org-special-ctrl-a/e' is set, ignore
tags on the first attempt, and only move to after the tags when
the cursor is already beyond the end of the headline.

With argument N not nil or 1, move forward N - 1 lines first."
  (interactive "^p")
  (let ((origin (point))
	(special (pcase org-special-ctrl-a/e
		   (`(,C-a . ,_) C-a) (_ org-special-ctrl-a/e)))
	deactivate-mark)
    ;; First move to a visible line.
    (if (bound-and-true-p visual-line-mode)
	(beginning-of-visual-line n)
      (move-beginning-of-line n)
      ;; `move-beginning-of-line' may leave point after invisible
      ;; characters if line starts with such of these (e.g., with
      ;; a link at column 0).  Really move to the beginning of the
      ;; current visible line.
      (beginning-of-line))
    (cond
     ;; No special behavior.  Point is already at the beginning of
     ;; a line, logical or visual.
     ((not special))
     ;; `beginning-of-visual-line' left point before logical beginning
     ;; of line: point is at the beginning of a visual line.  Bail
     ;; out.
     ((and (bound-and-true-p visual-line-mode) (not (bolp))))
     ((let ((case-fold-search nil)) (looking-at org-complex-heading-regexp))
      ;; At a headline, special position is before the title, but
      ;; after any TODO keyword or priority cookie.
      (let ((refpos (min (1+ (or (match-end 3) (match-end 2) (match-end 1)))
			 (line-end-position)))
	    (bol (point)))
	(if (eq special 'reversed)
	    (when (and (= origin bol) (eq last-command this-command))
	      (goto-char refpos))
	  (when (or (> origin refpos) (= origin bol))
	    (goto-char refpos)))))
     ((and (looking-at org-list-full-item-re)
	   (memq (org-element-type (save-match-data (org-element-at-point)))
		 '(item plain-list)))
      ;; Set special position at first white space character after
      ;; bullet, and check-box, if any.
      (let ((after-bullet
	     (let ((box (match-end 3)))
	       (cond ((not box) (match-end 1))
		     ((eq (char-after box) ?\s) (1+ box))
		     (t box)))))
	(if (eq special 'reversed)
	    (when (and (= (point) origin) (eq last-command this-command))
	      (goto-char after-bullet))
	  (when (or (> origin after-bullet) (= (point) origin))
	    (goto-char after-bullet)))))
     ;; No special context.  Point is already at beginning of line.
     (t nil))))

(defun org-end-of-line (&optional n)
  "Go to the end of the line, but before ellipsis, if any.

If this is a headline, and `org-special-ctrl-a/e' is set, ignore
tags on the first attempt, and only move to after the tags when
the cursor is already beyond the end of the headline.

With argument N not nil or 1, move forward N - 1 lines first."
  (interactive "^p")
  (let ((origin (point))
	(special (pcase org-special-ctrl-a/e
		   (`(,_ . ,C-e) C-e) (_ org-special-ctrl-a/e)))
	deactivate-mark)
    ;; First move to a visible line.
    (if (bound-and-true-p visual-line-mode)
	(beginning-of-visual-line n)
      (move-beginning-of-line n))
    (cond
     ;; At a headline, with tags.
     ((and special
	   (save-excursion
	     (beginning-of-line)
	     (let ((case-fold-search nil))
	       (looking-at org-complex-heading-regexp)))
	   (match-end 5))
      (let ((tags (save-excursion
		    (goto-char (match-beginning 5))
		    (skip-chars-backward " \t")
		    (point)))
	    (visual-end (and (bound-and-true-p visual-line-mode)
			     (save-excursion
			       (end-of-visual-line)
			       (point)))))
	;; If `end-of-visual-line' brings us before end of line or
	;; even tags, i.e., the headline spans over multiple visual
	;; lines, move there.
	(cond ((and visual-end
		    (< visual-end tags)
		    (<= origin visual-end))
	       (goto-char visual-end))
	      ((eq special 'reversed)
	       (if (and (= origin (line-end-position))
			(eq this-command last-command))
		   (goto-char tags)
		 (end-of-line)))
	      (t
	       (if (or (< origin tags) (= origin (line-end-position)))
		   (goto-char tags)
		 (end-of-line))))))
     ((bound-and-true-p visual-line-mode)
      (let ((bol (line-beginning-position)))
	(end-of-visual-line)
	;; If `end-of-visual-line' gets us past the ellipsis at the
	;; end of a line, backtrack and use `end-of-line' instead.
	(when (/= bol (line-beginning-position))
	  (goto-char bol)
	  (end-of-line))))
     (t (end-of-line)))))

(define-key org-mode-map "\C-a" 'org-beginning-of-line)
(define-key org-mode-map "\C-e" 'org-end-of-line)

(defun org-backward-sentence (&optional _arg)
  "Go to beginning of sentence, or beginning of table field.
This will call `backward-sentence' or `org-table-beginning-of-field',
depending on context."
  (interactive)
  (let* ((element (org-element-at-point))
	 (contents-begin (org-element-property :contents-begin element))
	 (table (org-element-lineage element '(table) t)))
    (if (and table
	     (> (point) contents-begin)
	     (<= (point) (org-element-property :contents-end table)))
	(call-interactively #'org-table-beginning-of-field)
      (save-restriction
	(when (and contents-begin
		   (< (point-min) contents-begin)
		   (> (point) contents-begin))
	  (narrow-to-region contents-begin
			    (org-element-property :contents-end element)))
	(call-interactively #'backward-sentence)))))

(defun org-forward-sentence (&optional _arg)
  "Go to end of sentence, or end of table field.
This will call `forward-sentence' or `org-table-end-of-field',
depending on context."
  (interactive)
  (if (and (org-at-heading-p)
	   (save-restriction (skip-chars-forward " \t") (not (eolp))))
      (save-restriction
	(narrow-to-region (line-beginning-position) (line-end-position))
	(call-interactively #'forward-sentence))
    (let* ((element (org-element-at-point))
	   (contents-end (org-element-property :contents-end element))
	   (table (org-element-lineage element '(table) t)))
      (if (and table
	       (>= (point) (org-element-property :contents-begin table))
	       (< (point) contents-end))
	  (call-interactively #'org-table-end-of-field)
	(save-restriction
	  (when (and contents-end
		     (> (point-max) contents-end)
		     ;; Skip blank lines between elements.
		     (< (org-element-property :end element)
			(save-excursion (goto-char contents-end)
					(skip-chars-forward " \r\t\n"))))
	    (narrow-to-region (org-element-property :contents-begin element)
			      contents-end))
	  ;; End of heading is considered as the end of a sentence.
	  (let ((sentence-end (concat (sentence-end) "\\|^\\*+ .*$")))
	    (call-interactively #'forward-sentence)))))))

(define-key org-mode-map "\M-a" 'org-backward-sentence)
(define-key org-mode-map "\M-e" 'org-forward-sentence)

(defun org-kill-line (&optional _arg)
  "Kill line, to tags or end of line."
  (interactive)
  (cond
   ((or (not org-special-ctrl-k)
	(bolp)
	(not (org-at-heading-p)))
    (when (and (get-char-property (min (point-max) (point-at-eol)) 'invisible)
	       org-ctrl-k-protect-subtree
	       (or (eq org-ctrl-k-protect-subtree 'error)
		   (not (y-or-n-p "Kill hidden subtree along with headline? "))))
      (user-error "C-k aborted as it would kill a hidden subtree"))
    (call-interactively
     (if (bound-and-true-p visual-line-mode) 'kill-visual-line 'kill-line)))
   ((looking-at ".*?\\S-\\([ \t]+\\(:[[:alnum:]_@#%:]+:\\)\\)[ \t]*$")
    (kill-region (point) (match-beginning 1))
    (org-set-tags nil t))
   (t (kill-region (point) (point-at-eol)))))

(define-key org-mode-map "\C-k" 'org-kill-line)

(defun org-yank (&optional arg)
  "Yank.  If the kill is a subtree, treat it specially.
This command will look at the current kill and check if is a single
subtree, or a series of subtrees[1].  If it passes the test, and if the
cursor is at the beginning of a line or after the stars of a currently
empty headline, then the yank is handled specially.  How exactly depends
on the value of the following variables.

`org-yank-folded-subtrees'
    By default, this variable is non-nil, which results in
    subtree(s) being folded after insertion, except if doing so
    would swallow text after the yanked text.

`org-yank-adjusted-subtrees'
    When non-nil (the default value is nil), the subtree will be
    promoted or demoted in order to fit into the local outline tree
    structure, which means that the level will be adjusted so that it
    becomes the smaller one of the two *visible* surrounding headings.

Any prefix to this command will cause `yank' to be called directly with
no special treatment.  In particular, a simple `\\[universal-argument]' prefix \
will just
plainly yank the text as it is.

\[1] The test checks if the first non-white line is a heading
    and if there are no other headings with fewer stars."
  (interactive "P")
  (org-yank-generic 'yank arg))

(defun org-yank-generic (command arg)
  "Perform some yank-like command.

This function implements the behavior described in the `org-yank'
documentation.  However, it has been generalized to work for any
interactive command with similar behavior."

  ;; pretend to be command COMMAND
  (setq this-command command)

  (if arg
      (call-interactively command)

    (let ((subtreep ; is kill a subtree, and the yank position appropriate?
	   (and (org-kill-is-subtree-p)
		(or (bolp)
		    (and (looking-at "[ \t]*$")
			 (string-match
			  "\\`\\*+\\'"
			  (buffer-substring (point-at-bol) (point)))))))
	  swallowp)
      (cond
       ((and subtreep org-yank-folded-subtrees)
	(let ((beg (point))
	      end)
	  (if (and subtreep org-yank-adjusted-subtrees)
	      (org-paste-subtree nil nil 'for-yank)
	    (call-interactively command))

	  (setq end (point))
	  (goto-char beg)
	  (when (and (bolp) subtreep
		     (not (setq swallowp
				(org-yank-folding-would-swallow-text beg end))))
	    (org-with-limited-levels
	     (or (looking-at org-outline-regexp)
		 (re-search-forward org-outline-regexp-bol end t))
	     (while (and (< (point) end) (looking-at org-outline-regexp))
	       (outline-hide-subtree)
	       (org-cycle-show-empty-lines 'folded)
	       (condition-case nil
		   (outline-forward-same-level 1)
		 (error (goto-char end))))))
	  (when swallowp
	    (message
	     "Inserted text not folded because that would swallow text"))

	  (goto-char end)
	  (skip-chars-forward " \t\n\r")
	  (beginning-of-line 1)
	  (push-mark beg 'nomsg)))
       ((and subtreep org-yank-adjusted-subtrees)
	(let ((beg (point-at-bol)))
	  (org-paste-subtree nil nil 'for-yank)
	  (push-mark beg 'nomsg)))
       (t
	(call-interactively command))))))

(defun org-yank-folding-would-swallow-text (beg end)
  "Would hide-subtree at BEG swallow any text after END?"
  (let (level)
    (org-with-limited-levels
     (save-excursion
       (goto-char beg)
       (when (or (looking-at org-outline-regexp)
		 (re-search-forward org-outline-regexp-bol end t))
	 (setq level (org-outline-level)))
       (goto-char end)
       (skip-chars-forward " \t\r\n\v\f")
       (not (or (eobp)
		(and (bolp) (looking-at-p org-outline-regexp)
		     (<= (org-outline-level) level))))))))

(define-key org-mode-map "\C-y" 'org-yank)

(defun org-truely-invisible-p ()
  "Check if point is at a character currently not visible.
This version does not only check the character property, but also
`visible-mode'."
  (unless (bound-and-true-p visible-mode)
    (org-invisible-p)))

(defun org-invisible-p2 ()
  "Check if point is at a character currently not visible.

If the point is at EOL (and not at the beginning of a buffer too),
move it back by one char before doing this check."
  (save-excursion
    (when (and (eolp) (not (bobp)))
      (backward-char 1))
    (org-invisible-p)))

(defun org-back-to-heading (&optional invisible-ok)
  "Call `outline-back-to-heading', but provide a better error message."
  (condition-case nil
      (outline-back-to-heading invisible-ok)
    (error (error "Before first headline at position %d in buffer %s"
		  (point) (current-buffer)))))

(defun org-before-first-heading-p ()
  "Before first heading?"
  (save-excursion
    (end-of-line)
    (null (re-search-backward org-outline-regexp-bol nil t))))

(defun org-at-heading-p (&optional ignored)
  (outline-on-heading-p t))

(defun org-in-commented-heading-p (&optional no-inheritance)
  "Non-nil if point is under a commented heading.
This function also checks ancestors of the current headline,
unless optional argument NO-INHERITANCE is non-nil."
  (cond
   ((org-before-first-heading-p) nil)
   ((let ((headline (nth 4 (org-heading-components))))
      (and headline
	   (let ((case-fold-search nil))
	     (string-match-p (concat "^" org-comment-string "\\(?: \\|$\\)")
			     headline)))))
   (no-inheritance nil)
   (t
    (save-excursion (and (org-up-heading-safe) (org-in-commented-heading-p))))))

(defun org-at-comment-p nil
  "Is cursor in a commented line?"
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (looking-at "^[ \t]*# "))))

(defun org-at-drawer-p nil
  "Is cursor at a drawer keyword?"
  (save-excursion
    (move-beginning-of-line 1)
    (looking-at org-drawer-regexp)))

(defun org-at-block-p nil
  "Is cursor at a block keyword?"
  (save-excursion
    (move-beginning-of-line 1)
    (looking-at org-block-regexp)))

(defun org-point-at-end-of-empty-headline ()
  "If point is at the end of an empty headline, return t, else nil.
If the heading only contains a TODO keyword, it is still still considered
empty."
  (let ((case-fold-search nil))
    (and (looking-at "[ \t]*$")
	 org-todo-line-regexp
	 (save-excursion
	   (beginning-of-line)
	   (looking-at org-todo-line-regexp)
	   (string= (match-string 3) "")))))

(defun org-at-heading-or-item-p ()
  (or (org-at-heading-p) (org-at-item-p)))

(defun org-at-target-p ()
  (or (org-in-regexp org-radio-target-regexp)
      (org-in-regexp org-target-regexp)))
;; Compatibility alias with Org versions < 7.8.03
(defalias 'org-on-target-p 'org-at-target-p)

(defun org-up-heading-all (arg)
  "Move to the heading line of which the present line is a subheading.
This function considers both visible and invisible heading lines.
With argument, move up ARG levels."
  (outline-up-heading arg t))

(defun org-up-heading-safe ()
  "Move to the heading line of which the present line is a subheading.
This version will not throw an error.  It will return the level of the
headline found, or nil if no higher level is found.

Also, this function will be a lot faster than `outline-up-heading',
because it relies on stars being the outline starters.  This can really
make a significant difference in outlines with very many siblings."
  (when (ignore-errors (org-back-to-heading t))
    (let ((level-up (1- (funcall outline-level))))
      (and (> level-up 0)
	   (re-search-backward (format "^\\*\\{1,%d\\} " level-up) nil t)
	   (funcall outline-level)))))

(defun org-first-sibling-p ()
  "Is this heading the first child of its parents?"
  (interactive)
  (let ((re org-outline-regexp-bol)
	level l)
    (unless (org-at-heading-p t)
      (user-error "Not at a heading"))
    (setq level (funcall outline-level))
    (save-excursion
      (if (not (re-search-backward re nil t))
	  t
	(setq l (funcall outline-level))
	(< l level)))))

(defun org-goto-sibling (&optional previous)
  "Goto the next sibling, even if it is invisible.
When PREVIOUS is set, go to the previous sibling instead.  Returns t
when a sibling was found.  When none is found, return nil and don't
move point."
  (let ((fun (if previous 're-search-backward 're-search-forward))
	(pos (point))
	(re org-outline-regexp-bol)
	level l)
    (when (ignore-errors (org-back-to-heading t))
      (setq level (funcall outline-level))
      (catch 'exit
	(or previous (forward-char 1))
	(while (funcall fun re nil t)
	  (setq l (funcall outline-level))
	  (when (< l level) (goto-char pos) (throw 'exit nil))
	  (when (= l level) (goto-char (match-beginning 0)) (throw 'exit t)))
	(goto-char pos)
	nil))))

(defun org-show-siblings ()
  "Show all siblings of the current headline."
  (save-excursion
    (while (org-goto-sibling) (org-flag-heading nil)))
  (save-excursion
    (while (org-goto-sibling 'previous)
      (org-flag-heading nil))))

(defun org-goto-first-child ()
  "Goto the first child, even if it is invisible.
Return t when a child was found.  Otherwise don't move point and
return nil."
  (let (level (pos (point)) (re org-outline-regexp-bol))
    (when (ignore-errors (org-back-to-heading t))
      (setq level (outline-level))
      (forward-char 1)
      (if (and (re-search-forward re nil t) (> (outline-level) level))
	  (progn (goto-char (match-beginning 0)) t)
	(goto-char pos) nil))))

(defun org-show-hidden-entry ()
  "Show an entry where even the heading is hidden."
  (save-excursion
    (org-show-entry)))

(defun org-flag-heading (flag &optional entry)
  "Flag the current heading.  FLAG non-nil means make invisible.
When ENTRY is non-nil, show the entire entry."
  (save-excursion
    (org-back-to-heading t)
    ;; Check if we should show the entire entry
    (if entry
	(progn
	  (org-show-entry)
	  (save-excursion
	    (and (outline-next-heading)
		 (org-flag-heading nil))))
      (outline-flag-region (max (point-min) (1- (point)))
			   (save-excursion (outline-end-of-heading) (point))
			   flag))))

(defun org-get-next-sibling ()
  "Move to next heading of the same level, and return point.
If there is no such heading, return nil.
This is like outline-next-sibling, but invisible headings are ok."
  (let ((level (funcall outline-level)))
    (outline-next-heading)
    (while (and (not (eobp)) (> (funcall outline-level) level))
      (outline-next-heading))
    (unless (or (eobp) (< (funcall outline-level) level))
      (point))))

(defun org-get-last-sibling ()
  "Move to previous heading of the same level, and return point.
If there is no such heading, return nil."
  (let ((opoint (point))
	(level (funcall outline-level)))
    (outline-previous-heading)
    (when (and (/= (point) opoint) (outline-on-heading-p t))
      (while (and (> (funcall outline-level) level)
		  (not (bobp)))
	(outline-previous-heading))
      (unless (< (funcall outline-level) level)
        (point)))))

(defun org-end-of-subtree (&optional invisible-ok to-heading)
  "Goto to the end of a subtree."
  ;; This contains an exact copy of the original function, but it uses
  ;; `org-back-to-heading', to make it work also in invisible
  ;; trees.  And is uses an invisible-ok argument.
  ;; Under Emacs this is not needed, but the old outline.el needs this fix.
  ;; Furthermore, when used inside Org, finding the end of a large subtree
  ;; with many children and grandchildren etc, this can be much faster
  ;; than the outline version.
  (org-back-to-heading invisible-ok)
  (let ((first t)
	(level (funcall outline-level)))
    (if (and (derived-mode-p 'org-mode) (< level 1000))
	;; A true heading (not a plain list item), in Org
	;; This means we can easily find the end by looking
	;; only for the right number of stars.  Using a regexp to do
	;; this is so much faster than using a Lisp loop.
	(let ((re (concat "^\\*\\{1," (int-to-string level) "\\} ")))
	  (forward-char 1)
	  (and (re-search-forward re nil 'move) (beginning-of-line 1)))
      ;; something else, do it the slow way
      (while (and (not (eobp))
		  (or first (> (funcall outline-level) level)))
	(setq first nil)
	(outline-next-heading)))
    (unless to-heading
      (when (memq (preceding-char) '(?\n ?\^M))
	;; Go to end of line before heading
	(forward-char -1)
	(when (memq (preceding-char) '(?\n ?\^M))
	  ;; leave blank line before heading
	  (forward-char -1)))))
  (point))

(defun org-end-of-meta-data (&optional full)
  "Skip planning line and properties drawer in current entry.
When optional argument FULL is non-nil, also skip empty lines,
clocking lines and regular drawers at the beginning of the
entry."
  (org-back-to-heading t)
  (forward-line)
  (when (looking-at-p org-planning-line-re) (forward-line))
  (when (looking-at org-property-drawer-re)
    (goto-char (match-end 0))
    (forward-line))
  (when (and full (not (org-at-heading-p)))
    (catch 'exit
      (let ((end (save-excursion (outline-next-heading) (point)))
	    (re (concat "[ \t]*$" "\\|" org-clock-line-re)))
	(while (not (eobp))
	  (cond ((looking-at-p org-drawer-regexp)
		 (if (re-search-forward "^[ \t]*:END:[ \t]*$" end t)
		     (forward-line)
		   (throw 'exit t)))
		((looking-at-p re) (forward-line))
		(t (throw 'exit t))))))))

(defun org-forward-heading-same-level (arg &optional invisible-ok)
  "Move forward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading.
Normally this only looks at visible headings, but when INVISIBLE-OK is
non-nil it will also look at invisible ones."
  (interactive "p")
  (let ((backward? (and arg (< arg 0))))
    (if (org-before-first-heading-p)
	(if backward? (goto-char (point-min)) (outline-next-heading))
      (org-back-to-heading invisible-ok)
      (unless backward? (end-of-line))	;do not match current headline
      (let ((level (- (match-end 0) (match-beginning 0) 1))
	    (f (if backward? #'re-search-backward #'re-search-forward))
	    (count (if arg (abs arg) 1))
	    (result (point)))
	(while (and (> count 0)
		    (funcall f org-outline-regexp-bol nil 'move))
	  (let ((l (- (match-end 0) (match-beginning 0) 1)))
	    (cond ((< l level) (setq count 0))
		  ((and (= l level)
			(or invisible-ok
			    (not (org-invisible-p
				  (line-beginning-position)))))
		   (cl-decf count)
		   (when (= l level) (setq result (point)))))))
	(goto-char result))
      (beginning-of-line))))

(defun org-backward-heading-same-level (arg &optional invisible-ok)
  "Move backward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (org-forward-heading-same-level (if arg (- arg) -1) invisible-ok))

(defun org-next-visible-heading (arg)
  "Move to the next visible heading.

This function wraps `outline-next-visible-heading' with
`org-with-limited-levels' in order to skip over inline tasks and
respect customization of `org-odd-levels-only'."
  (interactive "p")
  (org-with-limited-levels
   (outline-next-visible-heading arg)))

(defun org-previous-visible-heading (arg)
  "Move to the previous visible heading.

This function wraps `outline-previous-visible-heading' with
`org-with-limited-levels' in order to skip over inline tasks and
respect customization of `org-odd-levels-only'."
  (interactive "p")
  (org-with-limited-levels
   (outline-previous-visible-heading arg)))

(defun org-next-block (arg &optional backward block-regexp)
  "Jump to the next block.

With a prefix argument ARG, jump forward ARG many blocks.

When BACKWARD is non-nil, jump to the previous block.

When BLOCK-REGEXP is non-nil, use this regexp to find blocks.
Match data is set according to this regexp when the function
returns.

Return point at beginning of the opening line of found block.
Throw an error if no block is found."
  (interactive "p")
  (let ((re (or block-regexp "^[ \t]*#\\+BEGIN"))
	(case-fold-search t)
	(search-fn (if backward #'re-search-backward #'re-search-forward))
	(count (or arg 1))
	(origin (point))
	last-element)
    (if backward (beginning-of-line) (end-of-line))
    (while (and (> count 0) (funcall search-fn re nil t))
      (let ((element (save-excursion
		       (goto-char (match-beginning 0))
		       (save-match-data (org-element-at-point)))))
	(when (and (memq (org-element-type element)
			 '(center-block comment-block dynamic-block
					example-block export-block quote-block
					special-block src-block verse-block))
		   (<= (match-beginning 0)
		       (org-element-property :post-affiliated element)))
	  (setq last-element element)
	  (cl-decf count))))
    (if (= count 0)
	(prog1 (goto-char (org-element-property :post-affiliated last-element))
	  (save-match-data (org-show-context)))
      (goto-char origin)
      (user-error "No %s code blocks" (if backward "previous" "further")))))

(defun org-previous-block (arg &optional block-regexp)
  "Jump to the previous block.
With a prefix argument ARG, jump backward ARG many source blocks.
When BLOCK-REGEXP is non-nil, use this regexp to find blocks."
  (interactive "p")
  (org-next-block arg t block-regexp))

(defun org-forward-paragraph ()
  "Move forward to beginning of next paragraph or equivalent.

The function moves point to the beginning of the next visible
structural element, which can be a paragraph, a table, a list
item, etc.  It also provides some special moves for convenience:

  - On an affiliated keyword, jump to the beginning of the
    relative element.
  - On an item or a footnote definition, move to the second
    element inside, if any.
  - On a table or a property drawer, jump after it.
  - On a verse or source block, stop after blank lines."
  (interactive)
  (unless (eobp)
    (let* ((deactivate-mark nil)
	   (element (org-element-at-point))
	   (type (org-element-type element))
	   (post-affiliated (org-element-property :post-affiliated element))
	   (contents-begin (org-element-property :contents-begin element))
	   (contents-end (org-element-property :contents-end element))
	   (end (let ((end (org-element-property :end element)) (parent element))
		  (while (and (setq parent (org-element-property :parent parent))
			      (= (org-element-property :contents-end parent) end))
		    (setq end (org-element-property :end parent)))
		  end)))
      (cond ((not element)
	     (skip-chars-forward " \r\t\n")
	     (or (eobp) (beginning-of-line)))
	    ;; On affiliated keywords, move to element's beginning.
	    ((< (point) post-affiliated)
	     (goto-char post-affiliated))
	    ;; At a table row, move to the end of the table.  Similarly,
	    ;; at a node property, move to the end of the property
	    ;; drawer.
	    ((memq type '(node-property table-row))
	     (goto-char (org-element-property
			 :end (org-element-property :parent element))))
	    ((memq type '(property-drawer table)) (goto-char end))
	    ;; Consider blank lines as separators in verse and source
	    ;; blocks to ease editing.
	    ((memq type '(src-block verse-block))
	     (when (eq type 'src-block)
	       (setq contents-end
		     (save-excursion (goto-char end)
				     (skip-chars-backward " \r\t\n")
				     (line-beginning-position))))
	     (beginning-of-line)
	     (when (looking-at "[ \t]*$") (skip-chars-forward " \r\t\n"))
	     (if (not (re-search-forward "^[ \t]*$" contents-end t))
		 (goto-char end)
	       (skip-chars-forward " \r\t\n")
	       (if (= (point) contents-end) (goto-char end)
		 (beginning-of-line))))
	    ;; With no contents, just skip element.
	    ((not contents-begin) (goto-char end))
	    ;; If contents are invisible, skip the element altogether.
	    ((org-invisible-p (line-end-position))
	     (cl-case type
	       (headline
		(org-with-limited-levels (outline-next-visible-heading 1)))
	       ;; At a plain list, make sure we move to the next item
	       ;; instead of skipping the whole list.
	       (plain-list (forward-char)
			   (org-forward-paragraph))
	       (otherwise (goto-char end))))
	    ((>= (point) contents-end) (goto-char end))
	    ((>= (point) contents-begin)
	     ;; This can only happen on paragraphs and plain lists.
	     (cl-case type
	       (paragraph (goto-char end))
	       ;; At a plain list, try to move to second element in
	       ;; first item, if possible.
	       (plain-list (end-of-line)
			   (org-forward-paragraph))))
	    ;; When contents start on the middle of a line (e.g. in
	    ;; items and footnote definitions), try to reach first
	    ;; element starting after current line.
	    ((> (line-end-position) contents-begin)
	     (end-of-line)
	     (org-forward-paragraph))
	    (t (goto-char contents-begin))))))

(defun org-backward-paragraph ()
  "Move backward to start of previous paragraph or equivalent.

The function moves point to the beginning of the current
structural element, which can be a paragraph, a table, a list
item, etc., or to the beginning of the previous visible one if
point is already there.  It also provides some special moves for
convenience:

  - On an affiliated keyword, jump to the first one.
  - On a table or a property drawer, move to its beginning.
  - On comment, example, export, src and verse blocks, stop
    before blank lines."
  (interactive)
  (unless (bobp)
    (let* ((deactivate-mark nil)
	   (element (org-element-at-point))
	   (type (org-element-type element))
	   (contents-end (org-element-property :contents-end element))
	   (post-affiliated (org-element-property :post-affiliated element))
	   (begin (org-element-property :begin element))
	   (special?			;blocks handled specially
	    (memq type '(comment-block example-block export-block src-block
				       verse-block)))
	   (contents-begin
	    (if special?
		;; These types have no proper contents.  Fake line
		;; below the block opening line as contents beginning.
		(save-excursion (goto-char begin) (line-beginning-position 2))
	      (org-element-property :contents-begin element))))
      (cond
       ((not element) (goto-char (point-min)))
       ((= (point) begin)
	(backward-char)
	(org-backward-paragraph))
       ((<= (point) post-affiliated) (goto-char begin))
       ;; Special behavior: on a table or a property drawer, move to
       ;; its beginning.
       ((memq type '(node-property table-row))
	(goto-char (org-element-property
		    :post-affiliated (org-element-property :parent element))))
       (special?
	(if (<= (point) contents-begin) (goto-char post-affiliated)
	  ;; Inside a verse block, see blank lines as paragraph
	  ;; separators.
	  (let ((origin (point)))
	    (skip-chars-backward " \r\t\n" contents-begin)
	    (when (re-search-backward "^[ \t]*$" contents-begin 'move)
	      (skip-chars-forward " \r\t\n" origin)
	      (if (= (point) origin) (goto-char contents-begin)
		(beginning-of-line))))))
       ((eq type 'paragraph) (goto-char contents-begin)
	;; When at first paragraph in an item or a footnote definition,
	;; move directly to beginning of line.
	(let ((parent-contents
	       (org-element-property
		:contents-begin (org-element-property :parent element))))
	  (when (and parent-contents (= parent-contents contents-begin))
	    (beginning-of-line))))
       ;; At the end of a greater element, move to the beginning of
       ;; the last element within.
       ((and contents-end (>= (point) contents-end))
	(goto-char (1- contents-end))
	(org-backward-paragraph))
       (t (goto-char (or post-affiliated begin))))
      ;; Ensure we never leave point invisible.
      (when (org-invisible-p (point)) (beginning-of-visual-line)))))

(defun org-forward-element ()
  "Move forward by one element.
Move to the next element at the same level, when possible."
  (interactive)
  (cond ((eobp) (user-error "Cannot move further down"))
	((org-with-limited-levels (org-at-heading-p))
	 (let ((origin (point)))
	   (goto-char (org-end-of-subtree nil t))
	   (unless (org-with-limited-levels (org-at-heading-p))
	     (goto-char origin)
	     (user-error "Cannot move further down"))))
	(t
	 (let* ((elem (org-element-at-point))
		(end (org-element-property :end elem))
		(parent (org-element-property :parent elem)))
	   (cond ((and parent (= (org-element-property :contents-end parent) end))
		  (goto-char (org-element-property :end parent)))
		 ((integer-or-marker-p end) (goto-char end))
		 (t (message "No element at point")))))))

(defun org-backward-element ()
  "Move backward by one element.
Move to the previous element at the same level, when possible."
  (interactive)
  (cond ((bobp) (user-error "Cannot move further up"))
	((org-with-limited-levels (org-at-heading-p))
	 ;; At a headline, move to the previous one, if any, or stay
	 ;; here.
	 (let ((origin (point)))
	   (org-with-limited-levels (org-backward-heading-same-level 1))
	   ;; When current headline has no sibling above, move to its
	   ;; parent.
	   (when (= (point) origin)
	     (or (org-with-limited-levels (org-up-heading-safe))
		 (progn (goto-char origin)
			(user-error "Cannot move further up"))))))
	(t
	 (let* ((elem (org-element-at-point))
		(beg (org-element-property :begin elem)))
	   (cond
	    ;; Move to beginning of current element if point isn't
	    ;; there already.
	    ((null beg) (message "No element at point"))
	    ((/= (point) beg) (goto-char beg))
	    (t (goto-char beg)
	       (skip-chars-backward " \r\t\n")
	       (unless (bobp)
		 (let ((prev (org-element-at-point)))
		   (goto-char (org-element-property :begin prev))
		   (while (and (setq prev (org-element-property :parent prev))
			       (<= (org-element-property :end prev) beg))
		     (goto-char (org-element-property :begin prev)))))))))))

(defun org-up-element ()
  "Move to upper element."
  (interactive)
  (if (org-with-limited-levels (org-at-heading-p))
      (unless (org-up-heading-safe) (user-error "No surrounding element"))
    (let* ((elem (org-element-at-point))
	   (parent (org-element-property :parent elem)))
      (if parent (goto-char (org-element-property :begin parent))
	(if (org-with-limited-levels (org-before-first-heading-p))
	    (user-error "No surrounding element")
	  (org-with-limited-levels (org-back-to-heading)))))))

(defun org-down-element ()
  "Move to inner element."
  (interactive)
  (let ((element (org-element-at-point)))
    (cond
     ((memq (org-element-type element) '(plain-list table))
      (goto-char (org-element-property :contents-begin element))
      (forward-char))
     ((memq (org-element-type element) org-element-greater-elements)
      ;; If contents are hidden, first disclose them.
      (when (org-invisible-p (line-end-position)) (org-cycle))
      (goto-char (or (org-element-property :contents-begin element)
		     (user-error "No content for this element"))))
     (t (user-error "No inner element")))))

(defun org-drag-element-backward ()
  "Move backward element at point."
  (interactive)
  (let ((elem (or (org-element-at-point)
		  (user-error "No element at point"))))
    (if (eq (org-element-type elem) 'headline)
	;; Preserve point when moving a whole tree, even if point was
	;; on blank lines below the headline.
	(let ((offset (skip-chars-backward " \t\n")))
	  (unwind-protect (org-move-subtree-up)
	    (forward-char (- offset))))
      (let ((prev-elem
	     (save-excursion
	       (goto-char (org-element-property :begin elem))
	       (skip-chars-backward " \r\t\n")
	       (unless (bobp)
		 (let* ((beg (org-element-property :begin elem))
			(prev (org-element-at-point))
			(up prev))
		   (while (and (setq up (org-element-property :parent up))
			       (<= (org-element-property :end up) beg))
		     (setq prev up))
		   prev)))))
	;; Error out if no previous element or previous element is
	;; a parent of the current one.
	(if (or (not prev-elem) (org-element-nested-p elem prev-elem))
	    (user-error "Cannot drag element backward")
	  (let ((pos (point)))
	    (org-element-swap-A-B prev-elem elem)
	    (goto-char (+ (org-element-property :begin prev-elem)
			  (- pos (org-element-property :begin elem))))))))))

(defun org-drag-element-forward ()
  "Move forward element at point."
  (interactive)
  (let* ((pos (point))
	 (elem (or (org-element-at-point)
		   (user-error "No element at point"))))
    (when (= (point-max) (org-element-property :end elem))
      (user-error "Cannot drag element forward"))
    (goto-char (org-element-property :end elem))
    (let ((next-elem (org-element-at-point)))
      (when (or (org-element-nested-p elem next-elem)
		(and (eq (org-element-type next-elem) 'headline)
		     (not (eq (org-element-type elem) 'headline))))
	(goto-char pos)
	(user-error "Cannot drag element forward"))
      ;; Compute new position of point: it's shifted by NEXT-ELEM
      ;; body's length (without final blanks) and by the length of
      ;; blanks between ELEM and NEXT-ELEM.
      (let ((size-next (- (save-excursion
			    (goto-char (org-element-property :end next-elem))
			    (skip-chars-backward " \r\t\n")
			    (forward-line)
			    ;; Small correction if buffer doesn't end
			    ;; with a newline character.
			    (if (and (eolp) (not (bolp))) (1+ (point)) (point)))
			  (org-element-property :begin next-elem)))
	    (size-blank (- (org-element-property :end elem)
			   (save-excursion
			     (goto-char (org-element-property :end elem))
			     (skip-chars-backward " \r\t\n")
			     (forward-line)
			     (point)))))
	(org-element-swap-A-B elem next-elem)
	(goto-char (+ pos size-next size-blank))))))

(defun org-drag-line-forward (arg)
  "Drag the line at point ARG lines forward."
  (interactive "p")
  (dotimes (_ (abs arg))
    (let ((c (current-column)))
      (if (< 0 arg)
	  (progn
	    (beginning-of-line 2)
	    (transpose-lines 1)
	    (beginning-of-line 0))
	(transpose-lines 1)
	(beginning-of-line -1))
      (org-move-to-column c))))

(defun org-drag-line-backward (arg)
  "Drag the line at point ARG lines backward."
  (interactive "p")
  (org-drag-line-forward (- arg)))

(defun org-mark-element ()
  "Put point at beginning of this element, mark at end.

Interactively, if this command is repeated or (in Transient Mark
mode) if the mark is active, it marks the next element after the
ones already marked."
  (interactive)
  (let (deactivate-mark)
    (if (and (called-interactively-p 'any)
	     (or (and (eq last-command this-command) (mark t))
		 (and transient-mark-mode mark-active)))
	(set-mark
	 (save-excursion
	   (goto-char (mark))
	   (goto-char (org-element-property :end (org-element-at-point)))))
      (let ((element (org-element-at-point)))
	(end-of-line)
	(push-mark (org-element-property :end element) t t)
	(goto-char (org-element-property :begin element))))))

(defun org-narrow-to-element ()
  "Narrow buffer to current element."
  (interactive)
  (let ((elem (org-element-at-point)))
    (cond
     ((eq (car elem) 'headline)
      (narrow-to-region
       (org-element-property :begin elem)
       (org-element-property :end elem)))
     ((memq (car elem) org-element-greater-elements)
      (narrow-to-region
       (org-element-property :contents-begin elem)
       (org-element-property :contents-end elem)))
     (t
      (narrow-to-region
       (org-element-property :begin elem)
       (org-element-property :end elem))))))

(defun org-transpose-element ()
  "Transpose current and previous elements, keeping blank lines between.
Point is moved after both elements."
  (interactive)
  (org-skip-whitespace)
  (let ((end (org-element-property :end (org-element-at-point))))
    (org-drag-element-backward)
    (goto-char end)))

(defun org-unindent-buffer ()
  "Un-indent the visible part of the buffer.
Relative indentation (between items, inside blocks, etc.) isn't
modified."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Cannot un-indent a buffer not in Org mode"))
  (letrec ((parse-tree (org-element-parse-buffer 'greater-element))
	   (unindent-tree
	    (lambda (contents)
	      (dolist (element (reverse contents))
		(if (memq (org-element-type element) '(headline section))
		    (funcall unindent-tree (org-element-contents element))
		  (save-excursion
		    (save-restriction
		      (narrow-to-region
		       (org-element-property :begin element)
		       (org-element-property :end element))
		      (org-do-remove-indentation))))))))
    (funcall unindent-tree (org-element-contents parse-tree))))

(defun org-show-children (&optional level)
  "Show all direct subheadings of this heading.
Prefix arg LEVEL is how many levels below the current level
should be shown.  Default is enough to cause the following
heading to appear."
  (interactive "p")
  ;; If `orgstruct-mode' is active, use the slower version.
  (if orgstruct-mode (call-interactively #'outline-show-children)
    (save-excursion
      (org-back-to-heading t)
      (let* ((current-level (funcall outline-level))
	     (max-level (org-get-valid-level
			 current-level
			 (if level (prefix-numeric-value level) 1)))
	     (end (save-excursion (org-end-of-subtree t t)))
	     (regexp-fmt "^\\*\\{%d,%s\\}\\(?: \\|$\\)")
	     (past-first-child nil)
	     ;; Make sure to skip inlinetasks.
	     (re (format regexp-fmt
			 current-level
			 (cond
			  ((not (featurep 'org-inlinetask)) "")
			  (org-odd-levels-only (- (* 2 org-inlinetask-min-level)
						  3))
			  (t (1- org-inlinetask-min-level))))))
	;; Display parent heading.
	(outline-flag-region (line-end-position 0) (line-end-position) nil)
	(forward-line)
	;; Display children.  First child may be deeper than expected
	;; MAX-LEVEL.  Since we want to display it anyway, adjust
	;; MAX-LEVEL accordingly.
	(while (re-search-forward re end t)
	  (unless past-first-child
	    (setq re (format regexp-fmt
			     current-level
			     (max (funcall outline-level) max-level)))
	    (setq past-first-child t))
	  (outline-flag-region
	   (line-end-position 0) (line-end-position) nil))))))

(defun org-show-subtree ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (outline-flag-region
   (point)
   (save-excursion
     (org-end-of-subtree t t))
   nil))

(defun org-show-entry ()
  "Show the body directly following this heading.
Show the heading too, if it is currently invisible."
  (interactive)
  (save-excursion
    (ignore-errors
      (org-back-to-heading t)
      (outline-flag-region
       (max (point-min) (1- (point)))
       (save-excursion
	 (if (re-search-forward
	      (concat "[\r\n]\\(" org-outline-regexp "\\)") nil t)
	     (match-beginning 1)
	   (point-max)))
       nil)
      (org-cycle-hide-drawers 'children))))

(defun org-make-options-regexp (kwds &optional extra)
  "Make a regular expression for keyword lines.
KWDS is a list of keywords, as strings.  Optional argument EXTRA,
when non-nil, is a regexp matching keywords names."
  (concat "^[ \t]*#\\+\\("
	  (regexp-opt kwds)
	  (and extra (concat (and kwds "\\|") extra))
	  "\\):[ \t]*\\(.*\\)"))

;;;; Integration with and fixes for other packages

;;; Imenu support

(defvar-local org-imenu-markers nil
  "All markers currently used by Imenu.")

(defun org-imenu-new-marker (&optional pos)
  "Return a new marker for use by Imenu, and remember the marker."
  (let ((m (make-marker)))
    (move-marker m (or pos (point)))
    (push m org-imenu-markers)
    m))

(defun org-imenu-get-tree ()
  "Produce the index for Imenu."
  (dolist (x org-imenu-markers) (move-marker x nil))
  (setq org-imenu-markers nil)
  (let* ((case-fold-search nil)
	 (n org-imenu-depth)
	 (re (concat "^" (org-get-limited-outline-regexp)))
	 (subs (make-vector (1+ n) nil))
	 (last-level 0)
	 m level head0 head)
    (org-with-wide-buffer
     (goto-char (point-max))
     (while (re-search-backward re nil t)
       (setq level (org-reduced-level (funcall outline-level)))
       (when (and (<= level n)
		  (looking-at org-complex-heading-regexp)
		  (setq head0 (match-string-no-properties 4)))
	 (setq head (org-link-display-format head0)
	       m (org-imenu-new-marker))
	 (org-add-props head nil 'org-imenu-marker m 'org-imenu t)
	 (if (>= level last-level)
	     (push (cons head m) (aref subs level))
	   (push (cons head (aref subs (1+ level))) (aref subs level))
	   (cl-loop for i from (1+ level) to n do (aset subs i nil)))
	 (setq last-level level))))
    (aref subs 1)))

(eval-after-load "imenu"
  '(progn
     (add-hook 'imenu-after-jump-hook
	       (lambda ()
		 (when (derived-mode-p 'org-mode)
		   (org-show-context 'org-goto))))))

(defun org-link-display-format (s)
  "Replace links in string S with their description.
If there is no description, use the link target."
  (save-match-data
    (replace-regexp-in-string
     org-bracket-link-analytic-regexp
     (lambda (m)
       (if (match-end 5) (match-string 5 m)
	 (concat (match-string 1 m) (match-string 3 m))))
     s nil t)))

(defun org-toggle-link-display ()
  "Toggle the literal or descriptive display of links."
  (interactive)
  (if org-descriptive-links
      (progn (org-remove-from-invisibility-spec '(org-link))
	     (org-restart-font-lock)
	     (setq org-descriptive-links nil))
    (progn (add-to-invisibility-spec '(org-link))
	   (org-restart-font-lock)
	   (setq org-descriptive-links t))))

;; Speedbar support

(defvar org-speedbar-restriction-lock-overlay (make-overlay 1 1)
  "Overlay marking the agenda restriction line in speedbar.")
(overlay-put org-speedbar-restriction-lock-overlay
	     'face 'org-agenda-restriction-lock)
(overlay-put org-speedbar-restriction-lock-overlay
	     'help-echo "Agendas are currently limited to this item.")
(delete-overlay org-speedbar-restriction-lock-overlay)

(defun org-speedbar-set-agenda-restriction ()
  "Restrict future agenda commands to the location at point in speedbar.
To get rid of the restriction, use `\\[org-agenda-remove-restriction-lock]'."
  (interactive)
  (require 'org-agenda)
  (let (p m tp np dir txt)
    (cond
     ((setq p (text-property-any (point-at-bol) (point-at-eol)
				 'org-imenu t))
      (setq m (get-text-property p 'org-imenu-marker))
      (with-current-buffer (marker-buffer m)
	(goto-char m)
	(org-agenda-set-restriction-lock 'subtree)))
     ((setq p (text-property-any (point-at-bol) (point-at-eol)
				 'speedbar-function 'speedbar-find-file))
      (setq tp (previous-single-property-change
		(1+ p) 'speedbar-function)
	    np (next-single-property-change
		tp 'speedbar-function)
	    dir (speedbar-line-directory)
	    txt (buffer-substring-no-properties (or tp (point-min))
						(or np (point-max))))
      (with-current-buffer (find-file-noselect
			    (let ((default-directory dir))
			      (expand-file-name txt)))
	(unless (derived-mode-p 'org-mode)
	  (user-error "Cannot restrict to non-Org mode file"))
	(org-agenda-set-restriction-lock 'file)))
     (t (user-error "Don't know how to restrict Org mode agenda")))
    (move-overlay org-speedbar-restriction-lock-overlay
		  (point-at-bol) (point-at-eol))
    (setq current-prefix-arg nil)
    (org-agenda-maybe-redo)))

(defvar speedbar-file-key-map)
(declare-function speedbar-add-supported-extension "speedbar" (extension))
(eval-after-load "speedbar"
  '(progn
     (speedbar-add-supported-extension ".org")
     (define-key speedbar-file-key-map "<" 'org-speedbar-set-agenda-restriction)
     (define-key speedbar-file-key-map "\C-c\C-x<" 'org-speedbar-set-agenda-restriction)
     (define-key speedbar-file-key-map ">" 'org-agenda-remove-restriction-lock)
     (define-key speedbar-file-key-map "\C-c\C-x>" 'org-agenda-remove-restriction-lock)
     (add-hook 'speedbar-visiting-tag-hook
	       (lambda () (and (derived-mode-p 'org-mode) (org-show-context 'org-goto))))))

;;; Fixes and Hacks for problems with other packages

(defun org--flyspell-object-check-p (element)
  "Non-nil when Flyspell can check object at point.
ELEMENT is the element at point."
  (let ((object (save-excursion
		  (when (looking-at-p "\\>") (backward-char))
		  (org-element-context element))))
    (cl-case (org-element-type object)
      ;; Prevent checks in links due to keybinding conflict with
      ;; Flyspell.
      ((code entity export-snippet inline-babel-call
	     inline-src-block line-break latex-fragment link macro
	     statistics-cookie target timestamp verbatim)
       nil)
      (footnote-reference
       ;; Only in inline footnotes, within the definition.
       (and (eq (org-element-property :type object) 'inline)
	    (< (save-excursion
		 (goto-char (org-element-property :begin object))
		 (search-forward ":" nil t 2))
	       (point))))
      (otherwise t))))

(defun org-mode-flyspell-verify ()
  "Function used for `flyspell-generic-check-word-predicate'."
  (if (org-at-heading-p)
      ;; At a headline or an inlinetask, check title only.  This is
      ;; faster than relying on `org-element-at-point'.
      (and (save-excursion (beginning-of-line)
			   (and (let ((case-fold-search t))
				  (not (looking-at-p "\\*+ END[ \t]*$")))
				(let ((case-fold-search nil))
				  (looking-at org-complex-heading-regexp))))
	   (match-beginning 4)
	   (>= (point) (match-beginning 4))
	   (or (not (match-beginning 5))
	       (< (point) (match-beginning 5))))
    (let* ((element (org-element-at-point))
	   (post-affiliated (org-element-property :post-affiliated element)))
      (cond
       ;; Ignore checks in all affiliated keywords but captions.
       ((< (point) post-affiliated)
	(and (save-excursion
	       (beginning-of-line)
	       (let ((case-fold-search t)) (looking-at "[ \t]*#\\+CAPTION:")))
	     (> (point) (match-end 0))
	     (org--flyspell-object-check-p element)))
       ;; Ignore checks in LOGBOOK (or equivalent) drawer.
       ((let ((log (org-log-into-drawer)))
	  (and log
	       (let ((drawer (org-element-lineage element '(drawer))))
		 (and drawer
		      (eq (compare-strings
			   log nil nil
			   (org-element-property :drawer-name drawer) nil nil t)
			  t)))))
	nil)
       (t
	(cl-case (org-element-type element)
	  ((comment quote-section) t)
	  (comment-block
	   ;; Allow checks between block markers, not on them.
	   (and (> (line-beginning-position) post-affiliated)
		(save-excursion
		  (end-of-line)
		  (skip-chars-forward " \r\t\n")
		  (< (point) (org-element-property :end element)))))
	  ;; Arbitrary list of keywords where checks are meaningful.
	  ;; Make sure point is on the value part of the element.
	  (keyword
	   (and (member (org-element-property :key element)
			'("DESCRIPTION" "TITLE"))
		(save-excursion
		  (search-backward ":" (line-beginning-position) t))))
	  ;; Check is globally allowed in paragraphs verse blocks and
	  ;; table rows (after affiliated keywords) but some objects
	  ;; must not be affected.
	  ((paragraph table-row verse-block)
	   (let ((cbeg (org-element-property :contents-begin element))
		 (cend (org-element-property :contents-end element)))
	     (and cbeg (>= (point) cbeg) (< (point) cend)
		  (org--flyspell-object-check-p element))))))))))
(put 'org-mode 'flyspell-mode-predicate 'org-mode-flyspell-verify)

(defun org-remove-flyspell-overlays-in (beg end)
  "Remove flyspell overlays in region."
  (and (bound-and-true-p flyspell-mode)
       (fboundp 'flyspell-delete-region-overlays)
       (flyspell-delete-region-overlays beg end)))

(defvar flyspell-delayed-commands)
(eval-after-load "flyspell"
  '(add-to-list 'flyspell-delayed-commands 'org-self-insert-command))

;; Make `bookmark-jump' shows the jump location if it was hidden.
(eval-after-load "bookmark"
  '(if (boundp 'bookmark-after-jump-hook)
       ;; We can use the hook
       (add-hook 'bookmark-after-jump-hook 'org-bookmark-jump-unhide)
     ;; Hook not available, use advice
     (defadvice bookmark-jump (after org-make-visible activate)
       "Make the position visible."
       (org-bookmark-jump-unhide))))

;; Make sure saveplace shows the location if it was hidden
(eval-after-load "saveplace"
  '(defadvice save-place-find-file-hook (after org-make-visible activate)
     "Make the position visible."
     (org-bookmark-jump-unhide)))

;; Make sure ecb shows the location if it was hidden
(eval-after-load "ecb"
  '(defadvice ecb-method-clicked (after esf/org-show-context activate)
     "Make hierarchy visible when jumping into location from ECB tree buffer."
     (when (derived-mode-p 'org-mode)
       (org-show-context))))

(defun org-bookmark-jump-unhide ()
  "Unhide the current position, to show the bookmark location."
  (and (derived-mode-p 'org-mode)
       (or (org-invisible-p)
	   (save-excursion (goto-char (max (point-min) (1- (point))))
			   (org-invisible-p)))
       (org-show-context 'bookmark-jump)))

(defun org-mark-jump-unhide ()
  "Make the point visible with `org-show-context' after jumping to the mark."
  (when (and (derived-mode-p 'org-mode)
	     (org-invisible-p))
    (org-show-context 'mark-goto)))

(eval-after-load "simple"
  '(defadvice pop-to-mark-command (after org-make-visible activate)
     "Make the point visible with `org-show-context'."
     (org-mark-jump-unhide)))

(eval-after-load "simple"
  '(defadvice exchange-point-and-mark (after org-make-visible activate)
     "Make the point visible with `org-show-context'."
     (org-mark-jump-unhide)))

(eval-after-load "simple"
  '(defadvice pop-global-mark (after org-make-visible activate)
     "Make the point visible with `org-show-context'."
     (org-mark-jump-unhide)))

;; Make session.el ignore our circular variable
(defvar session-globals-exclude)
(eval-after-load "session"
  '(add-to-list 'session-globals-exclude 'org-mark-ring))

;;;; Finish up

(provide 'org)

(run-hooks 'org-load-hook)

;;; org.el ends here
