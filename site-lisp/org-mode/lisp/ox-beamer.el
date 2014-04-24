;;; ox-beamer.el --- Beamer Back-End for Org Export Engine

;; Copyright (C) 2007-2014 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik AT gmail DOT com>
;;         Nicolas Goaziou <n.goaziou AT gmail DOT com>
;; Keywords: org, wp, tex

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
;; This library implements both a Beamer back-end, derived from the
;; LaTeX one and a minor mode easing structure edition of the
;; document.  See Org manual for more information.

;;; Code:

(eval-when-compile (require 'cl))
(require 'ox-latex)

;; Install a default set-up for Beamer export.
(unless (assoc "beamer" org-latex-classes)
  (add-to-list 'org-latex-classes
	       '("beamer"
		 "\\documentclass[presentation]{beamer}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))



;;; User-Configurable Variables

(defgroup org-export-beamer nil
  "Options specific for using the beamer class in LaTeX export."
  :tag "Org Beamer"
  :group 'org-export
  :version "24.2")

(defcustom org-beamer-frame-level 1
  "The level at which headlines become frames.

Headlines at a lower level will be translated into a sectioning
structure.  At a higher level, they will be translated into
blocks.

If a headline with a \"BEAMER_env\" property set to \"frame\" is
found within a tree, its level locally overrides this number.

This variable has no effect on headlines with the \"BEAMER_env\"
property set to either \"ignoreheading\", \"appendix\", or
\"note\", which will respectively, be invisible, become an
appendix or a note.

This integer is relative to the minimal level of a headline
within the parse tree, defined as 1."
  :group 'org-export-beamer
  :type 'integer)

(defcustom org-beamer-frame-default-options ""
  "Default options string to use for frames.
For example, it could be set to \"allowframebreaks\"."
  :group 'org-export-beamer
  :type '(string :tag "[options]"))

(defcustom org-beamer-column-view-format
  "%45ITEM %10BEAMER_env(Env) %10BEAMER_act(Act) %4BEAMER_col(Col) %8BEAMER_opt(Opt)"
  "Column view format that should be used to fill the template."
  :group 'org-export-beamer
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const  :tag "Do not insert Beamer column view format" nil)
	  (string :tag "Beamer column view format")))

(defcustom org-beamer-theme "default"
  "Default theme used in Beamer presentations."
  :group 'org-export-beamer
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Do not insert a Beamer theme" nil)
	  (string :tag "Beamer theme")))

(defcustom org-beamer-environments-extra nil
  "Environments triggered by tags in Beamer export.
Each entry has 4 elements:

name    Name of the environment
key     Selection key for `org-beamer-select-environment'
open    The opening template for the environment, with the following escapes
        %a   the action/overlay specification
        %A   the default action/overlay specification
        %o   the options argument of the template
        %h   the headline text
        %r   the raw headline text (i.e. without any processing)
        %H   if there is headline text, that raw text in {} braces
        %U   if there is headline text, that raw text in [] brackets
close   The closing string of the environment."
  :group 'org-export-beamer
  :version "24.4"
  :package-version '(Org . "8.1")
  :type '(repeat
	  (list
	   (string :tag "Environment")
	   (string :tag "Selection key")
	   (string :tag "Begin")
	   (string :tag "End"))))

(defcustom org-beamer-outline-frame-title "Outline"
  "Default title of a frame containing an outline."
  :group 'org-export-beamer
  :type '(string :tag "Outline frame title"))

(defcustom org-beamer-outline-frame-options ""
  "Outline frame options appended after \\begin{frame}.
You might want to put e.g. \"allowframebreaks=0.9\" here."
  :group 'org-export-beamer
  :type '(string :tag "Outline frame options"))



;;; Internal Variables

(defconst org-beamer-column-widths
  "0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 0.0 :ETC"
"The column widths that should be installed as allowed property values.")

(defconst org-beamer-environments-special
  '(("againframe"     "A")
    ("appendix"       "x")
    ("column"         "c")
    ("columns"        "C")
    ("frame"          "f")
    ("fullframe"      "F")
    ("ignoreheading"  "i")
    ("note"           "n")
    ("noteNH"         "N"))
  "Alist of environments treated in a special way by the back-end.
Keys are environment names, as strings, values are bindings used
in `org-beamer-select-environment'.  Environments listed here,
along with their binding, are hard coded and cannot be modified
through `org-beamer-environments-extra' variable.")

(defconst org-beamer-environments-default
  '(("block"          "b" "\\begin{block}%a{%h}"          "\\end{block}")
    ("alertblock"     "a" "\\begin{alertblock}%a{%h}"     "\\end{alertblock}")
    ("verse"          "v" "\\begin{verse}%a %% %h"        "\\end{verse}")
    ("quotation"      "q" "\\begin{quotation}%a %% %h"    "\\end{quotation}")
    ("quote"          "Q" "\\begin{quote}%a %% %h"        "\\end{quote}")
    ("structureenv"   "s" "\\begin{structureenv}%a %% %h" "\\end{structureenv}")
    ("theorem"        "t" "\\begin{theorem}%a%U"          "\\end{theorem}")
    ("definition"     "d" "\\begin{definition}%a%U"       "\\end{definition}")
    ("example"        "e" "\\begin{example}%a%U"          "\\end{example}")
    ("exampleblock"   "E" "\\begin{exampleblock}%a{%h}"   "\\end{exampleblock}")
    ("proof"          "p" "\\begin{proof}%a%U"            "\\end{proof}")
    ("beamercolorbox" "o" "\\begin{beamercolorbox}%o{%h}" "\\end{beamercolorbox}"))
  "Environments triggered by properties in Beamer export.
These are the defaults - for user definitions, see
`org-beamer-environments-extra'.")

(defconst org-beamer-verbatim-elements
  '(code example-block fixed-width inline-src-block src-block verbatim)
  "List of element or object types producing verbatim text.
This is used internally to determine when a frame should have the
\"fragile\" option.")



;;; Internal functions

(defun org-beamer--normalize-argument (argument type)
  "Return ARGUMENT string with proper boundaries.

TYPE is a symbol among the following:
`action'    Return ARGUMENT within angular brackets.
`defaction' Return ARGUMENT within both square and angular brackets.
`option'    Return ARGUMENT within square brackets."
  (if (not (string-match "\\S-" argument)) ""
    (case type
      (action (if (string-match "\\`<.*>\\'" argument) argument
		(format "<%s>" argument)))
      (defaction (cond
		  ((string-match "\\`\\[<.*>\\]\\'" argument) argument)
		  ((string-match "\\`<.*>\\'" argument)
		   (format "[%s]" argument))
		  ((string-match "\\`\\[\\(.*\\)\\]\\'" argument)
		   (format "[<%s>]" (match-string 1 argument)))
		  (t (format "[<%s>]" argument))))
      (option (if (string-match "\\`\\[.*\\]\\'" argument) argument
		(format "[%s]" argument)))
      (otherwise argument))))

(defun org-beamer--element-has-overlay-p (element)
  "Non-nil when ELEMENT has an overlay specified.
An element has an overlay specification when it starts with an
`beamer' export-snippet whose value is between angular brackets.
Return overlay specification, as a string, or nil."
  (let ((first-object (car (org-element-contents element))))
    (when (eq (org-element-type first-object) 'export-snippet)
      (let ((value (org-element-property :value first-object)))
	(and (string-match "\\`<.*>\\'" value) value)))))



;;; Define Back-End

(org-export-define-derived-backend 'beamer 'latex
  :export-block "BEAMER"
  :menu-entry
  '(?l 1
       ((?B "As LaTeX buffer (Beamer)" org-beamer-export-as-latex)
	(?b "As LaTeX file (Beamer)" org-beamer-export-to-latex)
	(?P "As PDF file (Beamer)" org-beamer-export-to-pdf)
	(?O "As PDF file and open (Beamer)"
	    (lambda (a s v b)
	      (if a (org-beamer-export-to-pdf t s v b)
		(org-open-file (org-beamer-export-to-pdf nil s v b)))))))
  :options-alist
  '((:beamer-theme "BEAMER_THEME" nil org-beamer-theme)
    (:beamer-color-theme "BEAMER_COLOR_THEME" nil nil t)
    (:beamer-font-theme "BEAMER_FONT_THEME" nil nil t)
    (:beamer-inner-theme "BEAMER_INNER_THEME" nil nil t)
    (:beamer-outer-theme "BEAMER_OUTER_THEME" nil nil t)
    (:beamer-header-extra "BEAMER_HEADER" nil nil newline)
    ;; Modify existing properties.
    (:headline-levels nil "H" org-beamer-frame-level)
    (:latex-class "LATEX_CLASS" nil "beamer" t))
  :translate-alist '((bold . org-beamer-bold)
		     (export-block . org-beamer-export-block)
		     (export-snippet . org-beamer-export-snippet)
		     (headline . org-beamer-headline)
		     (item . org-beamer-item)
		     (keyword . org-beamer-keyword)
		     (link . org-beamer-link)
		     (plain-list . org-beamer-plain-list)
		     (radio-target . org-beamer-radio-target)
		     (target . org-beamer-target)
		     (template . org-beamer-template)))



;;; Transcode Functions

;;;; Bold

(defun org-beamer-bold (bold contents info)
  "Transcode BLOCK object into Beamer code.
CONTENTS is the text being bold.  INFO is a plist used as
a communication channel."
  (format "\\alert%s{%s}"
	  (or (org-beamer--element-has-overlay-p bold) "")
	  contents))


;;;; Export Block

(defun org-beamer-export-block (export-block contents info)
  "Transcode an EXPORT-BLOCK element into Beamer code.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (when (member (org-element-property :type export-block) '("BEAMER" "LATEX"))
    (org-remove-indentation (org-element-property :value export-block))))


;;;; Export Snippet

(defun org-beamer-export-snippet (export-snippet contents info)
  "Transcode an EXPORT-SNIPPET object into Beamer code.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((backend (org-export-snippet-backend export-snippet))
	(value (org-element-property :value export-snippet)))
    ;; Only "latex" and "beamer" snippets are retained.
    (cond ((eq backend 'latex) value)
	  ;; Ignore "beamer" snippets specifying overlays.
	  ((and (eq backend 'beamer)
		(or (org-export-get-previous-element export-snippet info)
		    (not (string-match "\\`<.*>\\'" value))))
	   value))))


;;;; Headline
;;
;; The main function to translate a headline is
;; `org-beamer-headline'.
;;
;; Depending on the level at which a headline is considered as
;; a frame (given by `org-beamer--frame-level'), the headline is
;; either a section (`org-beamer--format-section'), a frame
;; (`org-beamer--format-frame') or a block
;; (`org-beamer--format-block').
;;
;; `org-beamer-headline' also takes care of special environments
;; like "ignoreheading", "note", "noteNH", "appendix" and
;; "againframe".

(defun org-beamer--get-label (headline info)
  "Return label for HEADLINE, as a string.

INFO is a plist used as a communication channel.

The value is either the label specified in \"BEAMER_opt\"
property, or a fallback value built from headline's number.  This
function assumes HEADLINE will be treated as a frame."
  (let ((opt (org-element-property :BEAMER_OPT headline)))
    (if (and (org-string-nw-p opt)
	     (string-match "\\(?:^\\|,\\)label=\\(.*?\\)\\(?:$\\|,\\)" opt))
	(match-string 1 opt)
      (format "sec-%s"
	      (mapconcat 'number-to-string
			 (org-export-get-headline-number headline info)
			 "-")))))

(defun org-beamer--frame-level (headline info)
  "Return frame level in subtree containing HEADLINE.
INFO is a plist used as a communication channel."
  (or
   ;; 1. Look for "frame" environment in parents, starting from the
   ;;    farthest.
   (catch 'exit
     (mapc (lambda (parent)
	     (let ((env (org-element-property :BEAMER_ENV parent)))
	       (when (and env (member-ignore-case env '("frame" "fullframe")))
		 (throw 'exit (org-export-get-relative-level parent info)))))
	   (nreverse (org-export-get-genealogy headline)))
     nil)
   ;; 2. Look for "frame" environment in HEADLINE.
   (let ((env (org-element-property :BEAMER_ENV headline)))
     (and env (member-ignore-case env '("frame" "fullframe"))
	  (org-export-get-relative-level headline info)))
   ;; 3. Look for "frame" environment in sub-tree.
   (org-element-map headline 'headline
     (lambda (hl)
       (let ((env (org-element-property :BEAMER_ENV hl)))
	 (when (and env (member-ignore-case env '("frame" "fullframe")))
	   (org-export-get-relative-level hl info))))
     info 'first-match)
   ;; 4. No "frame" environment in tree: use default value.
   (plist-get info :headline-levels)))

(defun org-beamer--format-section (headline contents info)
  "Format HEADLINE as a sectioning part.
CONTENTS holds the contents of the headline.  INFO is a plist
used as a communication channel."
  (let ((latex-headline
	 (org-export-with-backend
	  ;; We create a temporary export back-end which behaves the
	  ;; same as current one, but adds "\protect" in front of the
	  ;; output of some objects.
	  (org-export-create-backend
	   :parent 'latex
	   :transcoders
	   (let ((protected-output
		  (function
		   (lambda (object contents info)
		     (let ((code (org-export-with-backend
				  'beamer object contents info)))
		       (if (org-string-nw-p code) (concat "\\protect" code)
			 code))))))
	     (mapcar #'(lambda (type) (cons type protected-output))
		     '(bold footnote-reference italic strike-through timestamp
			    underline))))
	  headline
	  contents
	  info))
	(mode-specs (org-element-property :BEAMER_ACT headline)))
    (if (and mode-specs
	     (string-match "\\`\\\\\\(.*?\\)\\(?:\\*\\|\\[.*\\]\\)?{"
			   latex-headline))
	;; Insert overlay specifications.
	(replace-match (concat (match-string 1 latex-headline)
			       (format "<%s>" mode-specs))
		       nil nil latex-headline 1)
      latex-headline)))

(defun org-beamer--format-frame (headline contents info)
  "Format HEADLINE as a frame.
CONTENTS holds the contents of the headline.  INFO is a plist
used as a communication channel."
  (let ((fragilep
	 ;; FRAGILEP is non-nil when HEADLINE contains an element
	 ;; among `org-beamer-verbatim-elements'.
	 (org-element-map headline org-beamer-verbatim-elements 'identity
			  info 'first-match)))
    (concat "\\begin{frame}"
	    ;; Overlay specification, if any. When surrounded by
	    ;; square brackets, consider it as a default
	    ;; specification.
	    (let ((action (org-element-property :BEAMER_ACT headline)))
	      (cond
	       ((not action) "")
	       ((string-match "\\`\\[.*\\]\\'" action )
		(org-beamer--normalize-argument action 'defaction))
	       (t (org-beamer--normalize-argument action 'action))))
	    ;; Options, if any.
	    (let* ((beamer-opt (org-element-property :BEAMER_OPT headline))
		   (options
		    ;; Collect options from default value and headline's
		    ;; properties.  Also add a label for links.
		    (append
		     (org-split-string org-beamer-frame-default-options ",")
		     (and beamer-opt
			  (org-split-string
			   ;; Remove square brackets if user provided
			   ;; them.
			   (and (string-match "^\\[?\\(.*\\)\\]?$" beamer-opt)
				(match-string 1 beamer-opt))
			   ","))
		     ;; Provide an automatic label for the frame
		     ;; unless the user specified one.
		     (unless (and beamer-opt
				  (string-match "\\(^\\|,\\)label=" beamer-opt))
		       (list
			(format "label=%s"
				(org-beamer--get-label headline info)))))))
	      ;; Change options list into a string.
	      (org-beamer--normalize-argument
	       (mapconcat
		'identity
		(if (or (not fragilep) (member "fragile" options)) options
		  (cons "fragile" options))
		",")
	       'option))
	    ;; Title.
	    (let ((env (org-element-property :BEAMER_ENV headline)))
	      (format "{%s}"
		      (if (and env (equal (downcase env) "fullframe")) ""
			(org-export-data
			 (org-element-property :title headline) info))))
	    "\n"
	    ;; The following workaround is required in fragile frames
	    ;; as Beamer will append "\par" to the beginning of the
	    ;; contents.  So we need to make sure the command is
	    ;; separated from the contents by at least one space.  If
	    ;; it isn't, it will create "\parfirst-word" command and
	    ;; remove the first word from the contents in the PDF
	    ;; output.
	    (if (not fragilep) contents
	      (replace-regexp-in-string "\\`\n*" "\\& " (or contents "")))
	    "\\end{frame}")))

(defun org-beamer--format-block (headline contents info)
  "Format HEADLINE as a block.
CONTENTS holds the contents of the headline.  INFO is a plist
used as a communication channel."
  (let* ((column-width (org-element-property :BEAMER_COL headline))
	 ;; ENVIRONMENT defaults to "block" if none is specified and
	 ;; there is no column specification.  If there is a column
	 ;; specified but still no explicit environment, ENVIRONMENT
	 ;; is "column".
	 (environment (let ((env (org-element-property :BEAMER_ENV headline)))
			(cond
			 ;; "block" is the fallback environment.
			 ((and (not env) (not column-width)) "block")
			 ;; "column" only.
			 ((not env) "column")
			 ;; Use specified environment.
			 (t env))))
	 (raw-title (org-element-property :raw-value headline))
	 (env-format
	  (cond ((member environment '("column" "columns")) nil)
		((assoc environment
			(append org-beamer-environments-extra
				org-beamer-environments-default)))
		(t (user-error "Wrong block type at a headline named \"%s\""
			       raw-title))))
	 (title (org-export-data (org-element-property :title headline) info))
	 (options (let ((options (org-element-property :BEAMER_OPT headline)))
		    (if (not options) ""
		      (org-beamer--normalize-argument options 'option))))
	 ;; Start a "columns" environment when explicitly requested or
	 ;; when there is no previous headline or the previous
	 ;; headline do not have a BEAMER_column property.
	 (parent-env (org-element-property
		      :BEAMER_ENV (org-export-get-parent-headline headline)))
	 (start-columns-p
	  (or (equal environment "columns")
	      (and column-width
		   (not (and parent-env
			     (equal (downcase parent-env) "columns")))
		   (or (org-export-first-sibling-p headline info)
		       (not (org-element-property
			     :BEAMER_COL
			     (org-export-get-previous-element
			      headline info)))))))
	 ;; End the "columns" environment when explicitly requested or
	 ;; when there is no next headline or the next headline do not
	 ;; have a BEAMER_column property.
	 (end-columns-p
	  (or (equal environment "columns")
	      (and column-width
		   (not (and parent-env
			     (equal (downcase parent-env) "columns")))
		   (or (org-export-last-sibling-p headline info)
		       (not (org-element-property
			     :BEAMER_COL
			     (org-export-get-next-element headline info))))))))
    (concat
     (when start-columns-p
       ;; Column can accept options only when the environment is
       ;; explicitly defined.
       (if (not (equal environment "columns")) "\\begin{columns}\n"
	 (format "\\begin{columns}%s\n" options)))
     (when column-width
       (format "\\begin{column}%s{%s}\n"
	       ;; One can specify placement for column only when
	       ;; HEADLINE stands for a column on its own.
	       (if (equal environment "column") options "")
	       (format "%s\\textwidth" column-width)))
     ;; Block's opening string.
     (when (nth 2 env-format)
       (concat
	(org-fill-template
	 (nth 2 env-format)
	 (nconc
	  ;; If BEAMER_act property has its value enclosed in square
	  ;; brackets, it is a default overlay specification and
	  ;; overlay specification is empty.  Otherwise, it is an
	  ;; overlay specification and the default one is nil.
	  (let ((action (org-element-property :BEAMER_ACT headline)))
	    (cond
	     ((not action) (list (cons "a" "") (cons "A" "")))
	     ((string-match "\\`\\[.*\\]\\'" action)
	      (list
	       (cons "A" (org-beamer--normalize-argument action 'defaction))
	       (cons "a" "")))
	     (t
	      (list (cons "a" (org-beamer--normalize-argument action 'action))
		    (cons "A" "")))))
	  (list (cons "o" options)
		(cons "h" title)
		(cons "r" raw-title)
		(cons "H" (if (equal raw-title "") ""
			    (format "{%s}" raw-title)))
		(cons "U" (if (equal raw-title "") ""
			    (format "[%s]" raw-title))))))
	"\n"))
     contents
     ;; Block's closing string, if any.
     (and (nth 3 env-format) (concat (nth 3 env-format) "\n"))
     (when column-width "\\end{column}\n")
     (when end-columns-p "\\end{columns}"))))

(defun org-beamer-headline (headline contents info)
  "Transcode HEADLINE element into Beamer code.
CONTENTS is the contents of the headline.  INFO is a plist used
as a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let ((level (org-export-get-relative-level headline info))
	  (frame-level (org-beamer--frame-level headline info))
	  (environment (let ((env (org-element-property :BEAMER_ENV headline)))
			 (or (org-string-nw-p env) "block"))))
      (cond
       ;; Case 1: Resume frame specified by "BEAMER_ref" property.
       ((equal environment "againframe")
	(let ((ref (org-element-property :BEAMER_REF headline)))
	  ;; Reference to frame being resumed is mandatory.  Ignore
	  ;; the whole headline if it isn't provided.
	  (when (org-string-nw-p ref)
	    (concat "\\againframe"
		    ;; Overlay specification.
		    (let ((overlay (org-element-property :BEAMER_ACT headline)))
		      (when overlay
			(org-beamer--normalize-argument
			 overlay
			 (if (string-match "^\\[.*\\]$" overlay) 'defaction
			   'action))))
		    ;; Options.
		    (let ((options (org-element-property :BEAMER_OPT headline)))
		      (when options
			(org-beamer--normalize-argument options 'option)))
		    ;; Resolve reference provided by "BEAMER_ref"
		    ;; property.  This is done by building a minimal fake
		    ;; link and calling the appropriate resolve function,
		    ;; depending on the reference syntax.
		    (let* ((type
			    (progn
			      (string-match "^\\(id:\\|#\\|\\*\\)?\\(.*\\)" ref)
			      (cond
			       ((or (not (match-string 1 ref))
				    (equal (match-string 1 ref) "*")) 'fuzzy)
			       ((equal (match-string 1 ref) "id:") 'id)
			       (t 'custom-id))))
			   (link (list 'link (list :path (match-string 2 ref))))
			   (target (if (eq type 'fuzzy)
				       (org-export-resolve-fuzzy-link link info)
				     (org-export-resolve-id-link link info))))
		      ;; Now use user-defined label provided in TARGET
		      ;; headline, or fallback to standard one.
		      (format "{%s}" (org-beamer--get-label target info)))))))
       ;; Case 2: Creation of an appendix is requested.
       ((equal environment "appendix")
	(concat "\\appendix"
		(org-element-property :BEAMER_ACT headline)
		"\n"
		(make-string (org-element-property :pre-blank headline) ?\n)
		contents))
       ;; Case 3: Ignore heading.
       ((equal environment "ignoreheading")
	(concat (make-string (org-element-property :pre-blank headline) ?\n)
		contents))
       ;; Case 4: HEADLINE is a note.
       ((member environment '("note" "noteNH"))
	(format "\\note{%s}"
		(concat (and (equal environment "note")
			     (concat
			      (org-export-data
			       (org-element-property :title headline) info)
			      "\n"))
			(org-trim contents))))
       ;; Case 5: HEADLINE is a frame.
       ((= level frame-level)
	(org-beamer--format-frame headline contents info))
       ;; Case 6: Regular section, extracted from
       ;; `org-latex-classes'.
       ((< level frame-level)
	(org-beamer--format-section headline contents info))
       ;; Case 7: Otherwise, HEADLINE is a block.
       (t (org-beamer--format-block headline contents info))))))


;;;; Item

(defun org-beamer-item (item contents info)
  "Transcode an ITEM element into Beamer code.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let ((action (let ((first-element (car (org-element-contents item))))
		  (and (eq (org-element-type first-element) 'paragraph)
		       (org-beamer--element-has-overlay-p first-element))))
	(output (org-export-with-backend 'latex item contents info)))
    (if (or (not action) (not (string-match "\\\\item" output))) output
      ;; If the item starts with a paragraph and that paragraph starts
      ;; with an export snippet specifying an overlay, insert it after
      ;; \item command.
      (replace-match (concat "\\\\item" action) nil nil output))))


;;;; Keyword

(defun org-beamer-keyword (keyword contents info)
  "Transcode a KEYWORD element into Beamer code.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    ;; Handle specifically BEAMER and TOC (headlines only) keywords.
    ;; Otherwise, fallback to `latex' back-end.
    (cond
     ((equal key "BEAMER") value)
     ((and (equal key "TOC") (string-match "\\<headlines\\>" value))
      (let ((depth (or (and (string-match "[0-9]+" value)
			    (string-to-number (match-string 0 value)))
		       (plist-get info :with-toc)))
	    (options (and (string-match "\\[.*?\\]" value)
			  (match-string 0 value))))
	(concat
	 (when (wholenump depth) (format "\\setcounter{tocdepth}{%s}\n" depth))
	 "\\tableofcontents" options)))
     (t (org-export-with-backend 'latex keyword contents info)))))


;;;; Link

(defun org-beamer-link (link contents info)
  "Transcode a LINK object into Beamer code.
CONTENTS is the description part of the link.  INFO is a plist
used as a communication channel."
  (let ((type (org-element-property :type link))
	(path (org-element-property :path link)))
    ;; Use \hyperlink command for all internal links.
    (cond
     ((equal type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
	(when destination
	  (format "\\hyperlink%s{%s}{%s}"
		  (or (org-beamer--element-has-overlay-p link) "")
		  (org-export-solidify-link-text
		   (org-element-property :value destination))
		  contents))))
     ((and (member type '("custom-id" "fuzzy" "id"))
	   (let ((destination (if (string= type "fuzzy")
				  (org-export-resolve-fuzzy-link link info)
				(org-export-resolve-id-link link info))))
	     (case (org-element-type destination)
	       (headline
		(let ((label
		       (format "sec-%s"
			       (mapconcat
				'number-to-string
				(org-export-get-headline-number
				 destination info)
				"-"))))
		  (if (and (plist-get info :section-numbers) (not contents))
		      (format "\\ref{%s}" label)
		    (format "\\hyperlink%s{%s}{%s}"
			    (or (org-beamer--element-has-overlay-p link) "")
			    label
			    contents))))
	       (target
		(let ((path (org-export-solidify-link-text path)))
		  (if (not contents) (format "\\ref{%s}" path)
		    (format "\\hyperlink%s{%s}{%s}"
			    (or (org-beamer--element-has-overlay-p link) "")
			    path
			    contents))))))))
     ;; Otherwise, use `latex' back-end.
     (t (org-export-with-backend 'latex link contents info)))))


;;;; Plain List
;;
;; Plain lists support `:environment', `:overlay' and `:options'
;; attributes.

(defun org-beamer-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element into Beamer code.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
	 (attributes (org-combine-plists
		      (org-export-read-attribute :attr_latex plain-list)
		      (org-export-read-attribute :attr_beamer plain-list)))
	 (latex-type (let ((env (plist-get attributes :environment)))
		       (cond (env)
			     ((eq type 'ordered) "enumerate")
			     ((eq type 'descriptive) "description")
			     (t "itemize")))))
    (org-latex--wrap-label
     plain-list
     (format "\\begin{%s}%s%s\n%s\\end{%s}"
	     latex-type
	     ;; Default overlay specification, if any.
	     (org-beamer--normalize-argument
	      (or (plist-get attributes :overlay) "")
	      'defaction)
	     ;; Second optional argument depends on the list type.
	     (org-beamer--normalize-argument
	      (or (plist-get attributes :options) "")
	      'option)
	     ;; Eventually insert contents and close environment.
	     contents
	     latex-type))))


;;;; Radio Target

(defun org-beamer-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object into Beamer code.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (format "\\hypertarget%s{%s}{%s}"
	  (or (org-beamer--element-has-overlay-p radio-target) "")
	  (org-export-solidify-link-text
	   (org-element-property :value radio-target))
	  text))


;;;; Target

(defun org-beamer-target (target contents info)
  "Transcode a TARGET object into Beamer code.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "\\hypertarget{%s}{}"
	  (org-export-solidify-link-text (org-element-property :value target))))


;;;; Template
;;
;; Template used is similar to the one used in `latex' back-end,
;; excepted for the table of contents and Beamer themes.

(defun org-beamer-template (contents info)
  "Return complete document string after Beamer conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info)))
    (concat
     ;; 1. Time-stamp.
     (and (plist-get info :time-stamp-file)
	  (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     ;; 2. Document class and packages.
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
		     (org-element-normalize-string
		      (plist-get info :latex-header-extra))
		     (plist-get info :beamer-header-extra)))))
	  info)))
     ;; 3. Insert themes.
     (let ((format-theme
	    (function
	     (lambda (prop command)
	       (let ((theme (plist-get info prop)))
		 (when theme
		   (concat command
			   (if (not (string-match "\\[.*\\]" theme))
			       (format "{%s}\n" theme)
			     (format "%s{%s}\n"
				     (match-string 0 theme)
				     (org-trim
				      (replace-match "" nil nil theme)))))))))))
       (mapconcat (lambda (args) (apply format-theme args))
		  '((:beamer-theme "\\usetheme")
		    (:beamer-color-theme "\\usecolortheme")
		    (:beamer-font-theme "\\usefonttheme")
		    (:beamer-inner-theme "\\useinnertheme")
		    (:beamer-outer-theme "\\useoutertheme"))
		  ""))
     ;; 4. Possibly limit depth for headline numbering.
     (let ((sec-num (plist-get info :section-numbers)))
       (when (integerp sec-num)
	 (format "\\setcounter{secnumdepth}{%d}\n" sec-num)))
     ;; 5. Author.
     (let ((author (and (plist-get info :with-author)
			(let ((auth (plist-get info :author)))
			  (and auth (org-export-data auth info)))))
	   (email (and (plist-get info :with-email)
		       (org-export-data (plist-get info :email) info))))
       (cond ((and author email (not (string= "" email)))
	      (format "\\author{%s\\thanks{%s}}\n" author email))
	     (author (format "\\author{%s}\n" author))
	     (t "\\author{}\n")))
     ;; 6. Date.
     (let ((date (and (plist-get info :with-date) (org-export-get-date info))))
       (format "\\date{%s}\n" (org-export-data date info)))
     ;; 7. Title
     (format "\\title{%s}\n" title)
     ;; 8. Hyperref options.
     (when (plist-get info :latex-hyperref-p)
       (format "\\hypersetup{\n  pdfkeywords={%s},\n  pdfsubject={%s},\n  pdfcreator={%s}}\n"
	       (or (plist-get info :keywords) "")
	       (or (plist-get info :description) "")
	       (if (not (plist-get info :with-creator)) ""
		 (plist-get info :creator))))
     ;; 9. Document start.
     "\\begin{document}\n\n"
     ;; 10. Title command.
     (org-element-normalize-string
      (cond ((string= "" title) nil)
	    ((not (stringp org-latex-title-command)) nil)
	    ((string-match "\\(?:[^%]\\|^\\)%s"
			   org-latex-title-command)
	     (format org-latex-title-command title))
	    (t org-latex-title-command)))
     ;; 11. Table of contents.
     (let ((depth (plist-get info :with-toc)))
       (when depth
	 (concat
	  (format "\\begin{frame}%s{%s}\n"
		  (org-beamer--normalize-argument
		   org-beamer-outline-frame-options 'option)
		  org-beamer-outline-frame-title)
	  (when (wholenump depth)
	    (format "\\setcounter{tocdepth}{%d}\n" depth))
	  "\\tableofcontents\n"
	  "\\end{frame}\n\n")))
     ;; 12. Document's body.
     contents
     ;; 13. Creator.
     (let ((creator-info (plist-get info :with-creator)))
       (cond
	((not creator-info) "")
	((eq creator-info 'comment)
	 (format "%% %s\n" (plist-get info :creator)))
	(t (concat (plist-get info :creator) "\n"))))
     ;; 14. Document end.
     "\\end{document}")))



;;; Minor Mode


(defvar org-beamer-mode-map (make-sparse-keymap)
  "The keymap for `org-beamer-mode'.")
(define-key org-beamer-mode-map "\C-c\C-b" 'org-beamer-select-environment)

;;;###autoload
(define-minor-mode org-beamer-mode
  "Support for editing Beamer oriented Org mode files."
  nil " Bm" 'org-beamer-mode-map)

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'org-mode
   '((":\\(B_[a-z]+\\|BMCOL\\):" 1 'org-beamer-tag prepend))
   'prepend))

(defface org-beamer-tag '((t (:box (:line-width 1 :color grey40))))
  "The special face for beamer tags."
  :group 'org-export-beamer)

(defun org-beamer-property-changed (property value)
  "Track the BEAMER_env property with tags.
PROPERTY is the name of the modified property.  VALUE is its new
value."
  (cond
   ((equal property "BEAMER_env")
    (save-excursion
      (org-back-to-heading t)
      ;; Filter out Beamer-related tags and install environment tag.
      (let ((tags (org-remove-if (lambda (x) (string-match "^B_" x))
				 (org-get-tags)))
	    (env-tag (and (org-string-nw-p value) (concat "B_" value))))
	(org-set-tags-to (if env-tag (cons env-tag tags) tags))
	(when env-tag (org-toggle-tag env-tag 'on)))))
   ((equal property "BEAMER_col")
    (org-toggle-tag "BMCOL" (if (org-string-nw-p value) 'on 'off)))))

(add-hook 'org-property-changed-functions 'org-beamer-property-changed)

(defun org-beamer-allowed-property-values (property)
  "Supply allowed values for PROPERTY."
  (cond
   ((and (equal property "BEAMER_env")
	 (not (org-entry-get nil (concat property "_ALL") 'inherit)))
    ;; If no allowed values for BEAMER_env have been defined,
    ;; supply all defined environments
    (mapcar 'car (append org-beamer-environments-special
			 org-beamer-environments-extra
			 org-beamer-environments-default)))
   ((and (equal property "BEAMER_col")
	 (not (org-entry-get nil (concat property "_ALL") 'inherit)))
    ;; If no allowed values for BEAMER_col have been defined,
    ;; supply some
    (org-split-string org-beamer-column-widths " "))))

(add-hook 'org-property-allowed-value-functions
	  'org-beamer-allowed-property-values)



;;; Commands

;;;###autoload
(defun org-beamer-export-as-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a Beamer buffer.

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

Export is done in a buffer named \"*Org BEAMER Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'beamer "*Org BEAMER Export*"
    async subtreep visible-only body-only ext-plist (lambda () (LaTeX-mode))))

;;;###autoload
(defun org-beamer-export-to-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a Beamer presentation (tex).

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

Return output file's name."
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'beamer file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-beamer-export-to-pdf
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a Beamer presentation (PDF).

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
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'beamer file
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))

;;;###autoload
(defun org-beamer-select-environment ()
  "Select the environment to be used by beamer for this entry.
While this uses (for convenience) a tag selection interface, the
result of this command will be that the BEAMER_env *property* of
the entry is set.

In addition to this, the command will also set a tag as a visual
aid, but the tag does not have any semantic meaning."
  (interactive)
  ;; Make sure `org-beamer-environments-special' has a higher
  ;; priority than `org-beamer-environments-extra'.
  (let* ((envs (append org-beamer-environments-special
		       org-beamer-environments-extra
		       org-beamer-environments-default))
	 (org-tag-alist
	  (append '((:startgroup))
		  (mapcar (lambda (e) (cons (concat "B_" (car e))
				       (string-to-char (nth 1 e))))
			  envs)
		  '((:endgroup))
		  '(("BMCOL" . ?|))))
	 (org-use-fast-tag-selection t)
	 (org-fast-tag-selection-single-key t))
    (org-set-tags)
    (let ((tags (or (ignore-errors (org-get-tags-string)) "")))
      (cond
       ;; For a column, automatically ask for its width.
       ((eq org-last-tag-selection-key ?|)
	(if (string-match ":BMCOL:" tags)
	    (org-set-property "BEAMER_col" (read-string "Column width: "))
	  (org-delete-property "BEAMER_col")))
       ;; For an "againframe" section, automatically ask for reference
       ;; to resumed frame and overlay specifications.
       ((eq org-last-tag-selection-key ?A)
	(if (equal (org-entry-get nil "BEAMER_env") "againframe")
	    (progn (org-entry-delete nil "BEAMER_env")
		   (org-entry-delete nil "BEAMER_ref")
		   (org-entry-delete nil "BEAMER_act"))
	  (org-entry-put nil "BEAMER_env" "againframe")
	  (org-set-property
	   "BEAMER_ref"
	   (read-string "Frame reference (*Title, #custom-id, id:...): "))
	  (org-set-property "BEAMER_act"
			    (read-string "Overlay specification: "))))
       ((string-match (concat ":B_\\(" (mapconcat 'car envs "\\|") "\\):") tags)
	(org-entry-put nil "BEAMER_env" (match-string 1 tags)))
       (t (org-entry-delete nil "BEAMER_env"))))))

;;;###autoload
(defun org-beamer-insert-options-template (&optional kind)
  "Insert a settings template, to make sure users do this right."
  (interactive (progn
		 (message "Current [s]ubtree or [g]lobal?")
		 (if (eq (read-char-exclusive) ?g) (list 'global)
		   (list 'subtree))))
  (if (eq kind 'subtree)
      (progn
	(org-back-to-heading t)
	(org-reveal)
	(org-entry-put nil "EXPORT_LaTeX_CLASS" "beamer")
	(org-entry-put nil "EXPORT_LaTeX_CLASS_OPTIONS" "[presentation]")
	(org-entry-put nil "EXPORT_FILE_NAME" "presentation.pdf")
	(when org-beamer-column-view-format
	  (org-entry-put nil "COLUMNS" org-beamer-column-view-format))
	(org-entry-put nil "BEAMER_col_ALL" org-beamer-column-widths))
    (insert "#+LaTeX_CLASS: beamer\n")
    (insert "#+LaTeX_CLASS_OPTIONS: [presentation]\n")
    (when org-beamer-theme (insert "#+BEAMER_THEME: " org-beamer-theme "\n"))
    (when org-beamer-column-view-format
      (insert "#+COLUMNS: " org-beamer-column-view-format "\n"))
    (insert "#+PROPERTY: BEAMER_col_ALL " org-beamer-column-widths "\n")))

;;;###autoload
(defun org-beamer-publish-to-latex (plist filename pub-dir)
  "Publish an Org file to a Beamer presentation (LaTeX).

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'beamer filename ".tex" plist pub-dir))

;;;###autoload
(defun org-beamer-publish-to-pdf (plist filename pub-dir)
  "Publish an Org file to a Beamer presentation (PDF, via LaTeX).

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  ;; Unlike to `org-beamer-publish-to-latex', PDF file is generated in
  ;; working directory and then moved to publishing directory.
  (org-publish-attachment
   plist
   (org-latex-compile
    (org-publish-org-to
     'beamer filename ".tex" plist (file-name-directory filename)))
   pub-dir))


(provide 'ox-beamer)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-beamer.el ends here
