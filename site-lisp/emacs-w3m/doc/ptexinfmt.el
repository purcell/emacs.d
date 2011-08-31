;;; ptexinfmt.el -- portable Texinfo formatter.

;; Copyright (C) 1985, 1986, 1988, 1990, 1991, 1992, 1993,
;;               1994, 1995, 1996, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1999 Yoshiki Hayashi <yoshiki@xemacs.org>
;; Copyright (C) 2000, 2001, 2002 TAKAHASHI Kaoru <kaoru@kaisei.org>

;; Author: TAKAHASHI Kaoru <kaoru@kaisei.org>
;;	Yoshiki Hayashi <yoshiki@xemacs.org>
;;	Katsumi Yamaoka <yamaoka@jpl.org>
;; Maintainer: TAKAHASHI Kaoru <kaoru@kaisei.org>
;; Created: 7 Jul 2000
;; Keywords: maint, tex, docs, emulation, compatibility

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Original code: Yoshiki Hayashi <yoshiki@xemacs.org>
;;	makeinfo.el (gnujdoc project)

;; Support texinfmt.el 2.32 or later.

;; Modified by Yamaoka not to use APEL functions.

;; Unimplemented command:
;;  @abbr{ABBREVIATION}
;;  @float ... @end float, @caption{TEXT}, @shortcaption{TEXT}, @listoffloats
;;  @deftypecv[x]
;;  @headitem
;;  @comma{}
;;  @quotation (optional arguments)
;;  @acronym{ACRONYM[, MEANING]} (optional argument)
;;  @dofirstparagraphindent
;;  @indent
;;  @verbatiminclude FILENAME
;;  @\
;;  @definfoenclose phoo,//,\\
;;  @deftypeivar CLASS DATA-TYPE VARIABLE-NAME
;;  @deftypeop CATEGORY CLASS DATA-TYPE NAME ARGUMENTS...
;;  @allowcodebreaks false
;;  @thischapternum
;;  @quotedblleft @quotedblright
;;  @quoteleft @quoteright  @quotedblbase @quotesinglbase
;;  @guillemetleft @guillemetright @guilsinglleft @guilsinglright.
;;  @clicksequence, @click, @clickstyle, @arrow

;;; Code:

(require 'texinfmt)

;;; Broken
(defvar ptexinfmt-disable-broken-notice-flag t
  "If non-nil disable notice, when call `ptexinfmt-broken-facility'.
This is last argument in `ptexinfmt-broken-facility'.")

(put 'ptexinfmt-broken-facility 'lisp-indent-function 'defun)
(defmacro ptexinfmt-broken-facility (facility docstring assertion
					      &optional dummy)
  "Declare a symbol FACILITY is broken if ASSERTION is nil.
DOCSTRING will be printed if ASSERTION is nil and
`ptexinfmt-disable-broken-notice-flag' is nil."
  `(let ((facility ',facility)
	 (docstring ,docstring)
	 (assertion (eval ',assertion)))
     (put facility 'broken (not assertion))
     (if assertion
	 nil
       (put facility 'broken-docstring docstring)
       (if ptexinfmt-disable-broken-notice-flag
	   nil
	 (message "BROKEN FACILITY DETECTED: %s" docstring)))))

(put 'ptexinfmt-defun-if-broken 'lisp-indent-function 'defun)
(defmacro ptexinfmt-defun-if-broken (&rest args)
  "Redefine a function just like `defun' if it is considered broken."
  (let ((name (list 'quote (car args))))
    (setq args (cdr args))
    `(prog1
	 ,name
       (if (get ,name 'broken)
	   (defalias ,name
	     (function (lambda ,@args)))))))

(put 'ptexinfmt-defun-if-void 'lisp-indent-function 'defun)
(defmacro ptexinfmt-defun-if-void (&rest args)
  "Define a function just like `defun' unless it is already defined."
  (let ((name (list 'quote (car args))))
    (setq args (cdr args))
    `(prog1
	 ,name
       (if (fboundp ,name)
	   nil
	 (defalias ,name
	   (function (lambda ,@args)))))))

(put 'ptexinfmt-defvar-if-void 'lisp-indent-function 'defun)
(defmacro ptexinfmt-defvar-if-void (&rest args)
  "Define a variable just like `defvar' unless it is already defined."
  (let ((name (car args)))
    (setq args (cdr args))
    `(prog1
	 (defvar ,name)
       (if (boundp ',name)
	   nil
	 (defvar ,name ,@args)))))

;; sort -fd
(ptexinfmt-broken-facility texinfo-format-printindex
  "Can't sort on Mule for Windows."
  (if (and (memq system-type '(windows-nt ms-dos))
;;; I don't know version threshold.
;;;	   (string< texinfmt-version "2.37 of 24 May 1997")
	   (boundp 'MULE) (not (featurep 'meadow))) ; Mule for Windows
      nil
    t))

;; @var{METASYNTACTIC-VARIABLE}
(ptexinfmt-broken-facility texinfo-format-var
  "Don't perse @var argument."
  (condition-case nil
      (with-temp-buffer
	(let (texinfo-enclosure-list texinfo-alias-list)
	  (texinfo-mode)
	  (insert "@var{@asis{foo}}\n")
	  (texinfo-format-expand-region (point-min) (point-max))
	  t))
    (error nil)))

;; @xref{NODE-NAME[, CROSS-REFERENCE-NAME, TITLE-OR-TOPIC,
;;     INFO-FILE-NAME, PRINTED-MANUAL-TITLE]}.
(ptexinfmt-broken-facility texinfo-format-xref
  "Can't format @xref, 1st argument is empty."
  (condition-case nil
      (with-temp-buffer
	(let (texinfo-enclosure-list texinfo-alias-list)
	  (texinfo-mode)
	  (insert "@xref{, xref, , file}\n")
	  (texinfo-format-expand-region (point-min) (point-max))
	  t))
    (error nil)))

;; @uref{URL[, TEXT][, REPLACEMENT]}
(ptexinfmt-broken-facility texinfo-format-uref
  "Parse twice @uref argument."
  (condition-case nil
      (with-temp-buffer
	(let (texinfo-enclosure-list texinfo-alias-list)
	  (texinfo-mode)
	  (insert "@uref{mailto:foo@@noncommand.example.com}\n")
	  (texinfo-format-expand-region (point-min) (point-max))
	  t))
    (error nil)))

;; @multitable
(ptexinfmt-broken-facility texinfo-multitable-widths
  "`texinfo-multitable-widths' unsupport wide-char."
  (if (fboundp 'texinfo-multitable-widths)
      (with-temp-buffer
	(let ((str "幅広文字"))
	  (texinfo-mode)
	  (insert (format " {%s}\n" str))
	  (goto-char (point-min))
	  (if (= (car (texinfo-multitable-widths)) (length str))
	      t
	    nil)))
    ;; function definition is void
    nil))

(ptexinfmt-broken-facility texinfo-multitable-item
  "`texinfo-multitable-item' unsupport wide-char."
  (not (get 'texinfo-multitable-widths 'broken)))


;;; Hardcopy and HTML (discard)
;; html
(put 'documentlanguage 'texinfo-format 'texinfo-discard-line-with-args)
(put 'documentencoding 'texinfo-format 'texinfo-discard-line-with-args)
(put 'documentdescription 'texinfo-format 'texinfo-discard-line-with-args)

;; size
(put 'smallbook 'texinfo-format 'texinfo-discard-line)
(put 'letterpaper 'texinfo-format 'texinfo-discard-line)
(put 'afourpaper 'texinfo-format 'texinfo-discard-line)
(put 'afourlatex 'texinfo-format 'texinfo-discard-line)
(put 'afourwide 'texinfo-format 'texinfo-discard-line)
(put 'afivepaper 'texinfo-format 'texinfo-discard-line)
(put 'pagesizes 'texinfo-format 'texinfo-discard-line-with-args)
(put 'fonttextsize 'texinfo-format 'texinfo-discard-line-with-args)

;; style
(put 'setchapternewpage 'texinfo-format 'texinfo-discard-line-with-args)
(put 'kbdinputstyle 'texinfo-format 'texinfo-discard-line-with-args)

;; flags
(put 'setcontentsaftertitlepage 'texinfo-format 'texinfo-discard-line)
(put 'setshortcontentsaftertitlepage 'texinfo-format 'texinfo-discard-line)
(put 'novalidate 'texinfo-format 'texinfo-discard-line-with-args)
(put 'frenchspacing 'texinfo-format 'texinfo-discard-line-with-args)

;; head & foot
(put 'headings 'texinfo-format 'texinfo-discard-line-with-args)
(put 'evenfooting 'texinfo-format 'texinfo-discard-line-with-args)
(put 'evenheading 'texinfo-format 'texinfo-discard-line-with-args)
(put 'oddfooting 'texinfo-format 'texinfo-discard-line-with-args)
(put 'oddheading 'texinfo-format 'texinfo-discard-line-with-args)
(put 'everyfooting 'texinfo-format 'texinfo-discard-line-with-args)
(put 'everyheading 'texinfo-format 'texinfo-discard-line-with-args)

;; misc
(put 'page 'texinfo-format 'texinfo-discard-line)
(put 'hyphenation 'texinfo-format 'texinfo-discard-command-and-arg)

;; @slanted{TEXT} (makeinfo 4.8 or later)
(put 'slanted 'texinfo-format 'texinfo-format-noop)

;; @sansserif{TEXT} (makeinfo 4.8 or later)
(put 'sansserif 'texinfo-format 'texinfo-format-noop)

;; @tie{} (makeinfo 4.3 or later)
(put 'tie 'texinfo-format 'texinfo-format-tie)
(ptexinfmt-defun-if-void texinfo-format-tie ()
  (texinfo-parse-arg-discard)
  (insert " "))


;;; Directory File
;; @direcategory DIRPART
(put 'dircategory 'texinfo-format 'texinfo-format-dircategory)
(ptexinfmt-defun-if-void texinfo-format-dircategory ()
  (let ((str (texinfo-parse-arg-discard)))
    (delete-region (point)
		   (progn
		     (skip-chars-forward " ")
		     (point)))
    (insert "INFO-DIR-SECTION " str "\n")))

;; @direntry ... @end direntry
(put 'direntry 'texinfo-format 'texinfo-format-direntry)
(ptexinfmt-defun-if-void texinfo-format-direntry ()
  (texinfo-push-stack 'direntry nil)
  (texinfo-discard-line)
  (insert "START-INFO-DIR-ENTRY\n"))

(put 'direntry 'texinfo-end 'texinfo-end-direntry)
(ptexinfmt-defun-if-void texinfo-end-direntry ()
  (texinfo-discard-command)
  (insert "END-INFO-DIR-ENTRY\n\n")
  (texinfo-pop-stack 'direntry))


;;; Block Enclosing
;; @detailmenu ... @end detailmenu
(put 'detailmenu 'texinfo-format 'texinfo-discard-line)
(put 'detailmenu 'texinfo-end 'texinfo-discard-command)

;; @smalldisplay ... @end smalldisplay
(put 'smalldisplay 'texinfo-format 'texinfo-format-example)
(put 'smalldisplay 'texinfo-end 'texinfo-end-example)

;; @smallformat ... @end smallformat
(put 'smallformat 'texinfo-format 'texinfo-format-flushleft)
(put 'smallformat 'texinfo-end 'texinfo-end-flushleft)

;; @cartouche  ... @end cartouche
(put 'cartouche 'texinfo-format 'texinfo-discard-line)
(put 'cartouche 'texinfo-end 'texinfo-discard-command)


;;; Conditional
;; @ifnottex ... @end ifnottex (makeinfo 3.11 or later)
(put 'ifnottex 'texinfo-format 'texinfo-discard-line)
(put 'ifnottex 'texinfo-end 'texinfo-discard-command)

;; @ifnothtml ... @end ifnothtml (makeinfo 3.11 or later)
(put 'ifnothtml 'texinfo-format 'texinfo-discard-line)
(put 'ifnothtml 'texinfo-end 'texinfo-discard-command)

;; @ifnotplaintext ... @end ifnotplaintext (makeinfo 4.2 or later)
(put 'ifnotplaintext 'texinfo-format 'texinfo-discard-line)
(put 'ifnotplaintext 'texinfo-end 'texinfo-discard-command)

;; @ifnotdocbook ... @end ifnotdocbook (makeinfo 4.7 or later)
(put 'ifnotdocbook 'texinfo-format 'texinfo-discard-line)
(put 'ifnotdocbook 'texinfo-end 'texinfo-discard-command)

;; @ifnotinfo ... @end ifnotinfo (makeinfo 3.11 or later)
(put 'ifnotinfo 'texinfo-format 'texinfo-format-ifnotinfo)
(ptexinfmt-defun-if-void texinfo-format-ifnotinfo ()
  (delete-region texinfo-command-start
		 (progn (re-search-forward "@end ifnotinfo[ \t]*\n")
			(point))))

;; @html ... @end html (makeinfo 3.11 or later)
(put 'html 'texinfo-format 'texinfo-format-html)
(ptexinfmt-defun-if-void texinfo-format-html ()
  (delete-region texinfo-command-start
		 (progn (re-search-forward "@end html[ \t]*\n")
			(point))))

;; @docbook ... @end docbook (makeinfo 4.7 or later)
(put 'docbook 'texinfo-format 'texinfo-format-docbook)
(ptexinfmt-defun-if-void texinfo-format-docbook ()
  (delete-region texinfo-command-start
		 (progn (re-search-forward "@end docbook[ \t]*\n")
			(point))))

;; @ifhtml ... @end ifhtml (makeinfo 3.8 or later)
(put 'ifhtml 'texinfo-format 'texinfo-format-ifhtml)
(defun texinfo-format-ifhtml ()
  (delete-region texinfo-command-start
		 (progn (re-search-forward "@end ifhtml[ \t]*\n")
			(point))))

;; @ifplaintext ... @end ifplaintext (makeinfo 4.2 or later)
(put 'ifplaintext 'texinfo-format 'texinfo-format-ifplaintext)
(ptexinfmt-defun-if-void texinfo-format-ifplaintext ()
  (delete-region texinfo-command-start
		 (progn (re-search-forward "@end ifplaintext[ \t]*\n")
			(point))))

;; @ifdocbook ... @end ifdocbook (makeinfo 4.7 or later)
(put 'ifdocbook 'texinfo-format 'texinfo-format-ifdocbook)
(ptexinfmt-defun-if-void texinfo-format-ifdocbook ()
  (delete-region texinfo-command-start
		 (progn (re-search-forward "@end ifdocbook[ \t]*\n")
			(point))))


;;; Marking
;; @env{ENVIRONMENT-VARIABLE}
(put 'env 'texinfo-format 'texinfo-format-code)

;; @command{COMMAND-NAME}
(put 'command 'texinfo-format 'texinfo-format-code)

;; @indicateurl{INDICATEURL}
(put 'indicateurl 'texinfo-format 'texinfo-format-code)

;; @url{URL[, DISPLAYED-TEXT][, REPLACEMENT}
(put 'url 'texinfo-format 'texinfo-format-uref)	; Texinfo 4.7

;; @acronym{ACRONYM}
(put 'acronym 'texinfo-format 'texinfo-format-var)

;; @var{METASYNTACTIC-VARIABLE}
(ptexinfmt-defun-if-broken texinfo-format-var ()
  (let ((arg (texinfo-parse-expanded-arg)))
    (texinfo-discard-command)
    (insert (upcase arg))))

;; @key{KEY-NAME}
(put 'key 'texinfo-format 'texinfo-format-key)
(ptexinfmt-defun-if-void texinfo-format-key ()
  (insert (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @email{EMAIL-ADDRESS[, DISPLAYED-TEXT]}
(put 'email 'texinfo-format 'texinfo-format-email)
(ptexinfmt-defun-if-void texinfo-format-email ()
  "Format EMAIL-ADDRESS and optional DISPLAYED-TXT.
Insert < ... > around EMAIL-ADDRESS."
  (let ((args (texinfo-format-parse-args)))
  (texinfo-discard-command)
    ;; if displayed-text
    (if (nth 1 args)
	(insert (nth 1 args) " <" (nth 0 args) ">")
      (insert "<" (nth 0 args) ">"))))

;; @option{OPTION-NAME}
(put 'option 'texinfo-format 'texinfo-format-option)
(ptexinfmt-defun-if-void texinfo-format-option ()
  "Insert ` ... ' around arg unless inside a table; in that case, no quotes."
  ;; `looking-at-backward' not available in v. 18.57, 20.2
  ;; searched-for character is a control-H
  (if (not (search-backward "\010"
			    (save-excursion (beginning-of-line) (point))
			    t))
      (insert "`" (texinfo-parse-arg-discard) "'")
    (insert (texinfo-parse-arg-discard)))
  (goto-char texinfo-command-start))

;; @verb{<char>TEXT<char>}  (makeinfo 4.1 or later)
(put 'verb 'texinfo-format 'texinfo-format-verb)
(ptexinfmt-defun-if-void texinfo-format-verb ()
  "Format text between non-quoted unique delimiter characters verbatim.
Enclose the verbatim text, including the delimiters, in braces.  Print
text exactly as written (but not the delimiters) in a fixed-width.

For example, @verb\{|@|\} results in @ and
@verb\{+@'e?`!`+} results in @'e?`!`."

  (let ((delimiter (buffer-substring-no-properties
		    (1+ texinfo-command-end) (+ 2 texinfo-command-end))))
    (unless (looking-at "{")
      (error "Not found: @verb start brace"))
    (delete-region texinfo-command-start (+ 2 texinfo-command-end))
    (search-forward  delimiter))
  (delete-backward-char 1)
  (unless (looking-at "}")
    (error "Not found: @verb end brace"))
  (delete-char 1))


;; @LaTeX{}
(put 'LaTeX 'texinfo-format 'texinfo-format-LaTeX)
(ptexinfmt-defun-if-void texinfo-format-LaTeX ()
  (texinfo-parse-arg-discard)
  (insert "LaTeX"))

;; @registeredsymbol{}
(put 'registeredsymbol 'texinfo-format 'texinfo-format-registeredsymbol)
(ptexinfmt-defun-if-void texinfo-format-registeredsymbol ()
  (texinfo-parse-arg-discard)
  (insert "(R)"))

;;; Accents and Special characters
;; @euro{}	==>	Euro
(put 'euro 'texinfo-format 'texinfo-format-euro)
(ptexinfmt-defun-if-void texinfo-format-euro ()
  (texinfo-parse-arg-discard)
  (insert "Euro "))

;; @pounds{}	==>	#	Pounds Sterling
(put 'pounds 'texinfo-format 'texinfo-format-pounds)
(ptexinfmt-defun-if-void texinfo-format-pounds ()
  (texinfo-parse-arg-discard)
  (insert "#"))

;; @ordf{}	==>	a	Spanish feminine
(put 'ordf 'texinfo-format 'texinfo-format-ordf)
(ptexinfmt-defun-if-void texinfo-format-ordf ()
  (texinfo-parse-arg-discard)
  (insert "a"))

;; @ordm{}	==>	o	Spanish masculine
(put 'ordm 'texinfo-format 'texinfo-format-ordm)
(ptexinfmt-defun-if-void texinfo-format-ordm ()
  (texinfo-parse-arg-discard)
  (insert "o"))

;; @OE{}	==>	OE	French-OE-ligature
(put 'OE 'texinfo-format 'texinfo-format-French-OE-ligature)
(ptexinfmt-defun-if-void texinfo-format-French-OE-ligature ()
  (insert "OE" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @oe{}	==>	oe
(put 'oe 'texinfo-format 'texinfo-format-French-oe-ligature)
(ptexinfmt-defun-if-void texinfo-format-French-oe-ligature () ; lower case
  (insert "oe" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @AA{}	==>	AA	Scandinavian-A-with-circle
(put 'AA 'texinfo-format 'texinfo-format-Scandinavian-A-with-circle)
(ptexinfmt-defun-if-void texinfo-format-Scandinavian-A-with-circle ()
  (insert "AA" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @aa{}	==>	aa
(put 'aa 'texinfo-format 'texinfo-format-Scandinavian-a-with-circle)
(ptexinfmt-defun-if-void texinfo-format-Scandinavian-a-with-circle () ; lower case
  (insert "aa" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @AE{}	==>	AE	Latin-Scandinavian-AE
(put 'AE 'texinfo-format 'texinfo-format-Latin-Scandinavian-AE)
(ptexinfmt-defun-if-void texinfo-format-Latin-Scandinavian-AE ()
  (insert "AE" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @ae{}	==>	ae
(put 'ae 'texinfo-format 'texinfo-format-Latin-Scandinavian-ae)
(ptexinfmt-defun-if-void texinfo-format-Latin-Scandinavian-ae () ; lower case
  (insert "ae" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @ss{}	==>	ss	German-sharp-S
(put 'ss 'texinfo-format 'texinfo-format-German-sharp-S)
(ptexinfmt-defun-if-void texinfo-format-German-sharp-S ()
  (insert "ss" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @questiondown{}	==>	?	upside-down-question-mark
(put 'questiondown 'texinfo-format 'texinfo-format-upside-down-question-mark)
(ptexinfmt-defun-if-void texinfo-format-upside-down-question-mark ()
  (insert "?" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @exclamdown{}	==>	!	upside-down-exclamation-mark
(put 'exclamdown 'texinfo-format 'texinfo-format-upside-down-exclamation-mark)
(ptexinfmt-defun-if-void texinfo-format-upside-down-exclamation-mark ()
  (insert "!" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @L{}		==>	L/	Polish suppressed-L (Lslash)
(put 'L 'texinfo-format 'texinfo-format-Polish-suppressed-L)
(ptexinfmt-defun-if-void texinfo-format-Polish-suppressed-L ()
  (insert (texinfo-parse-arg-discard) "/L")
  (goto-char texinfo-command-start))

;; @l{}		==>	l/	Polish suppressed-L (Lslash) (lower case)
(put 'l 'texinfo-format 'texinfo-format-Polish-suppressed-l-lower-case)
(ptexinfmt-defun-if-void texinfo-format-Polish-suppressed-l-lower-case ()
  (insert (texinfo-parse-arg-discard) "/l")
  (goto-char texinfo-command-start))

;; @O{}		==>	O/	Scandinavian O-with-slash
(put 'O 'texinfo-format 'texinfo-format-Scandinavian-O-with-slash)
(ptexinfmt-defun-if-void texinfo-format-Scandinavian-O-with-slash ()
  (insert (texinfo-parse-arg-discard) "O/")
  (goto-char texinfo-command-start))

;; @o{}		==>	o/	Scandinavian O-with-slash (lower case)
(put 'o 'texinfo-format 'texinfo-format-Scandinavian-o-with-slash-lower-case)
(ptexinfmt-defun-if-void texinfo-format-Scandinavian-o-with-slash-lower-case ()
  (insert (texinfo-parse-arg-discard) "o/")
  (goto-char texinfo-command-start))

;; @,{c}	==>	c,	cedilla accent
(put '\, 'texinfo-format 'texinfo-format-cedilla-accent)
(ptexinfmt-defun-if-void texinfo-format-cedilla-accent ()
  (insert (texinfo-parse-arg-discard) ",")
  (goto-char texinfo-command-start))


;; @dotaccent{o}	==>	.o	overdot-accent
(put 'dotaccent 'texinfo-format 'texinfo-format-overdot-accent)
(ptexinfmt-defun-if-void texinfo-format-overdot-accent ()
  (insert "." (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @ubaraccent{o}	==>	_o	underbar-accent
(put 'ubaraccent 'texinfo-format 'texinfo-format-underbar-accent)
(ptexinfmt-defun-if-void texinfo-format-underbar-accent ()
  (insert "_" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @udotaccent{o}	==>	o-.	underdot-accent
(put 'udotaccent 'texinfo-format 'texinfo-format-underdot-accent)
(ptexinfmt-defun-if-void texinfo-format-underdot-accent ()
  (insert (texinfo-parse-arg-discard) "-.")
  (goto-char texinfo-command-start))

;; @H{o}	==>	""o	long Hungarian umlaut
(put 'H 'texinfo-format 'texinfo-format-long-Hungarian-umlaut)
(ptexinfmt-defun-if-void texinfo-format-long-Hungarian-umlaut ()
  (insert "\"\"" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @ringaccent{o}	==>	*o	ring accent
(put 'ringaccent 'texinfo-format 'texinfo-format-ring-accent)
(ptexinfmt-defun-if-void texinfo-format-ring-accent ()
  (insert "*" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @tieaccent{oo}	==>	[oo	tie after accent
(put 'tieaccent 'texinfo-format 'texinfo-format-tie-after-accent)
(ptexinfmt-defun-if-void texinfo-format-tie-after-accent ()
  (insert "[" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @u{o}	==>	(o	breve accent
(put 'u 'texinfo-format 'texinfo-format-breve-accent)
(ptexinfmt-defun-if-void texinfo-format-breve-accent ()
  (insert "(" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @v{o}	==>	<o	hacek accent
(put 'v 'texinfo-format 'texinfo-format-hacek-accent)
(ptexinfmt-defun-if-void texinfo-format-hacek-accent ()
  (insert "<" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @dotless{i}	==>	i	dotless i and dotless j
(put 'dotless 'texinfo-format 'texinfo-format-dotless)
(ptexinfmt-defun-if-void texinfo-format-dotless ()
  (insert (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @.
(put '\. 'texinfo-format 'texinfo-format-\.)
(ptexinfmt-defun-if-void texinfo-format-\. ()
  (texinfo-discard-command)
  (insert "."))

;; @:
(put '\: 'texinfo-format 'texinfo-format-\:)
(ptexinfmt-defun-if-void texinfo-format-\: ()
  (texinfo-discard-command))

;; @-
(put '\- 'texinfo-format 'texinfo-format-soft-hyphen)
(ptexinfmt-defun-if-void texinfo-format-soft-hyphen ()
  (texinfo-discard-command))

;; @/
(put '\/ 'texinfo-format 'texinfo-format-\/)
(ptexinfmt-defun-if-void texinfo-format-\/ ()
  (texinfo-discard-command))

;; @textdegree{}
(put 'textdegree 'texinfo-format 'texinfo-format-textdegree)
(ptexinfmt-defun-if-void texinfo-format-textdegree ()
  (insert "o" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @geq{}
(put 'geq 'texinfo-format 'texinfo-format-geq)
(ptexinfmt-defun-if-void texinfo-format-geq ()
  (insert ">=" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @leq{}
(put 'leq 'texinfo-format 'texinfo-format-leq)
(ptexinfmt-defun-if-void texinfo-format-leq ()
  (insert "<=" (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))


;;; Cross References
;; @ref{NODE-NAME, ...}
;; @xref{NODE-NAME, ...}
(put 'ref 'texinfo-format 'texinfo-format-xref)

(ptexinfmt-defun-if-broken texinfo-format-xref ()
  (let ((args (texinfo-format-parse-args)))
    (texinfo-discard-command)
    (insert "*Note ")
    (let ((fname (or (nth 1 args) (nth 2 args))))
      (if (null (or fname (nth 3 args)))
	  (insert (nth 0 args) "::")
	(insert (or fname (nth 0 args)) ": ")
	(if (nth 3 args)
	    (insert "(" (nth 3 args) ")"))
	(unless (null (nth 0 args))
	  (insert (nth 0 args)))))))

;; @uref{URL [,TEXT] [,REPLACEMENT]}
(put 'uref 'texinfo-format 'texinfo-format-uref)
(ptexinfmt-defun-if-broken texinfo-format-uref ()
  "Format URL and optional URL-TITLE.
Insert ` ... ' around URL if no URL-TITLE argument;
otherwise, insert URL-TITLE followed by URL in parentheses."
  (let ((args (texinfo-format-parse-args)))
    (texinfo-discard-command)
    ;; if url-title
    (if (nth 1 args)
	(insert  (nth 1 args) " (" (nth 0 args) ")")
      (insert "`" (nth 0 args) "'"))))

;; @inforef{NODE-NAME, CROSS-REFERENCE-NAME, INFO-FILE-NAME}
(put 'inforef 'texinfo-format 'texinfo-format-inforef)
(ptexinfmt-defun-if-void texinfo-format-inforef ()
  (let ((args (texinfo-format-parse-args)))
    (texinfo-discard-command)
    (if (nth 1 args)
	(insert "*Note " (nth 1 args) ": (" (nth 2 args) ")" (car args))
      (insert "*Note " "(" (nth 2 args) ")" (car args) "::"))))


;; @anchor{NAME}
;; don't emulation
;; If support @anchor for Mule 2.3, We must fix informat.el and info.el:
;;  - Info-tagify suport @anthor-*-refill.
;;  - info.el support Ref in Tag table.
(unless (get 'anchor 'texinfo-format)
  (put 'anchor 'texinfo-format 'texinfo-discard-command-and-arg))



;;; New command definition
;; @alias NEW=EXISTING
(put 'alias 'texinfo-format 'texinfo-alias)
(ptexinfmt-defun-if-void texinfo-alias ()
  (let ((start (1- (point)))
	args)
    (skip-chars-forward " ")
    (save-excursion (end-of-line) (setq texinfo-command-end (point)))
    (if (not (looking-at "\\([^=]+\\)=\\(.*\\)"))
	(error "Invalid alias command")
      (setq texinfo-alias-list
	    (cons
	     (cons
	      (buffer-substring (match-beginning 1) (match-end 1))
	      (buffer-substring (match-beginning 2) (match-end 2)))
	     texinfo-alias-list))
      (texinfo-discard-command))))


;;; Indent
;; @exampleindent INDENT  (makeinfo 4.0 or later)

;; @paragraphindent INDENT  (makeinfo 4.0 or later)
;; INDENT: asis, 0, n

;; @firstparagraphindent WORD   (makeinfo 4.6 or later)
;; WORD: none, insert



;;; Special
;; @image{FILENAME [, WIDTH] [, HEIGHT]}
(put 'image 'texinfo-format 'texinfo-format-image)
(ptexinfmt-defun-if-void texinfo-format-image ()
  ;; I don't know makeinfo parse FILENAME.
  (let ((args (texinfo-format-parse-args))
	filename)
    (when (null (nth 0 args))
      (error "Invalid image command"))
    (texinfo-discard-command)
    ;; makeinfo uses FILENAME.txt
    (setq filename (format "%s.txt" (nth 0 args)))
    (message "Reading included file: %s" filename)
    ;; verbatim for Info output
    (goto-char (+ (point) (cadr (insert-file-contents filename))))
    (message "Reading included file: %s...done" filename)))

;; @hyphenation command discards an argument within braces
(put 'hyphenation 'texinfo-format 'texinfo-discard-command-and-arg)
(ptexinfmt-defun-if-void texinfo-discard-command-and-arg ()
  "Discard both @-command and its argument in braces."
  (goto-char texinfo-command-end)
  (forward-list 1)
  (setq texinfo-command-end (point))
  (delete-region texinfo-command-start texinfo-command-end))


;;; @multitable ... @end multitable
(ptexinfmt-defvar-if-void texinfo-extra-inter-column-width 0
  "*Number of extra spaces between entries (columns) in @multitable.")

(ptexinfmt-defvar-if-void texinfo-multitable-buffer-name
  "*multitable-temporary-buffer*")
(ptexinfmt-defvar-if-void texinfo-multitable-rectangle-name
  "texinfo-multitable-temp-")

;; These commands are defined in texinfo.tex for printed output.
(put 'multitableparskip 'texinfo-format 'texinfo-discard-line-with-args)
(put 'multitableparindent 'texinfo-format 'texinfo-discard-line-with-args)
(put 'multitablecolmargin 'texinfo-format 'texinfo-discard-line-with-args)
(put 'multitablelinespace 'texinfo-format 'texinfo-discard-line-with-args)

(put 'multitable 'texinfo-format 'texinfo-multitable)

(ptexinfmt-defun-if-void texinfo-multitable ()
  "Produce multi-column tables."

;; This function pushes information onto the `texinfo-stack'.
;; A stack element consists of:
;;   - type-of-command, i.e., multitable
;;   - the information about column widths, and
;;   - the position of texinfo-command-start.
;; e.g., ('multitable (1 2 3 4) 123)
;; The command line is then deleted.
  (texinfo-push-stack
   'multitable
   ;; push width information on stack
   (texinfo-multitable-widths))
  (texinfo-discard-line-with-args))

(put 'multitable 'texinfo-end 'texinfo-end-multitable)
(ptexinfmt-defun-if-void texinfo-end-multitable ()
  "Discard the @end multitable line and pop the stack of multitable."
  (texinfo-discard-command)
  (texinfo-pop-stack 'multitable))

(ptexinfmt-defun-if-broken texinfo-multitable-widths ()
  "Return list of widths of each column in a multi-column table."
  (let (texinfo-multitable-width-list)
    ;; Fractions format:
    ;;  @multitable @columnfractions .25 .3 .45
    ;;
    ;; Template format:
    ;;  @multitable {Column 1 template} {Column 2} {Column 3 example}
    ;; Place point before first argument
    (skip-chars-forward " \t")
    (cond
     ;; Check for common misspelling
     ((looking-at "@columnfraction ")
      (error "In @multitable, @columnfractions misspelled"))
     ;; Case 1: @columnfractions .25 .3 .45
     ((looking-at "@columnfractions")
      (forward-word 1)
      (while (not (eolp))
	(setq texinfo-multitable-width-list
	      (cons
	       (truncate
		(1-
		 (* fill-column (read (get-buffer (current-buffer))))))
	       texinfo-multitable-width-list))))
     ;;
     ;; Case 2: {Column 1 template} {Column 2} {Column 3 example}
     ((looking-at "{")
      (let ((start-of-templates (point)))
	(while (not (eolp))
	  (skip-chars-forward " \t")
	  (let* ((start-of-template (1+ (point)))
		 (end-of-template
		  ;; forward-sexp works with braces in Texinfo mode
		  (progn (forward-sexp 1) (1- (point)))))
	    (setq texinfo-multitable-width-list
		  (cons (- (progn
			     (goto-char end-of-template)
			     (current-column))
			   (progn
			     (goto-char start-of-template)
			     (current-column)))
			texinfo-multitable-width-list))
	    ;; Remove carriage return from within a template, if any.
	    ;; This helps those those who want to use more than
	    ;; one line's worth of words in @multitable line.
	    (narrow-to-region start-of-template end-of-template)
	    (goto-char (point-min))
	    (while (search-forward "\n" nil t)
	      (delete-char -1))
	    (goto-char (point-max))
	    (widen)
	    (forward-char 1)))))
     ;;
     ;; Case 3: Trouble
     (t
      (error "\
You probably need to specify column widths for @multitable correctly")))
    ;; Check whether columns fit on page.
    (let ((desired-columns
	   (+
	    ;; between column spaces
	    (length texinfo-multitable-width-list)
	    ;; additional between column spaces, if any
	    texinfo-extra-inter-column-width
	    ;; sum of spaces for each entry
	    (apply '+ texinfo-multitable-width-list))))
      (if (> desired-columns fill-column)
	  (error (format "\
Multi-column table width, %d chars, is greater than page width, %d chars."
			 desired-columns fill-column))))
    texinfo-multitable-width-list))

;; @item  A1  @tab  A2  @tab  A3
(ptexinfmt-defun-if-void texinfo-multitable-extract-row ()
  "Return multitable row, as a string.
End of row is beginning of next @item or beginning of @end.
Cells within rows are separated by @tab."
  (skip-chars-forward " \t")
  (let* ((start (point))
	 (end (progn
		(re-search-forward "@item\\|@end")
		(match-beginning 0)))
	 (row (progn (goto-char end)
		     (skip-chars-backward " ")
		     ;; remove whitespace at end of argument
		     (delete-region (point) end)
		     (buffer-substring start (point)))))
    (delete-region texinfo-command-start end)
    row))

(put 'multitable 'texinfo-item 'texinfo-multitable-item)
(ptexinfmt-defun-if-void texinfo-multitable-item ()
  "Format a row within a multicolumn table.
Cells in row are separated by @tab.
Widths of cells are specified by the arguments in the @multitable line.
All cells are made to be the same height.
This command is executed when texinfmt sees @item inside @multitable."
  (let ((original-buffer (current-buffer))
	(table-widths (reverse (car (cdr (car texinfo-stack)))))
	(existing-fill-column fill-column)
	start
	end
	(table-column       0)
	(table-entry-height 0)
	;; unformatted row looks like:  A1  @tab  A2  @tab  A3
	;; extract-row command deletes the source line in the table.
	(unformated-row (texinfo-multitable-extract-row)))
    ;; Use a temporary buffer
    (set-buffer (get-buffer-create texinfo-multitable-buffer-name))
    (delete-region (point-min) (point-max))
    (insert unformated-row)
    (goto-char (point-min))
;; 1. Check for correct number of @tab in line.
    (let ((tab-number 1)) ;; one @tab between two columns
      (while (search-forward "@tab" nil t)
	(setq tab-number (1+ tab-number)))
      (if (/= tab-number (length table-widths))
	  (error "Wrong number of @tab's in a @multitable row")))
    (goto-char (point-min))
;; 2. Format each cell, and copy to a rectangle
    ;; buffer looks like this:    A1  @tab  A2  @tab  A3
    ;; Cell #1: format up to @tab
    ;; Cell #2: format up to @tab
    ;; Cell #3: format up to eob
    (while (not (eobp))
      (setq start (point))
      (setq end (save-excursion
		  (if (search-forward "@tab" nil 'move)
		      ;; Delete the @tab command, including the @-sign
		      (delete-region
		       (point)
		       (progn (forward-word -1) (1- (point)))))
		  (point)))
      ;; Set fill-column *wider* than needed to produce inter-column space
      (setq fill-column (+ 1
			   texinfo-extra-inter-column-width
			   (nth table-column table-widths)))
      (narrow-to-region start end)
      ;; Remove whitespace before and after entry.
      (skip-chars-forward " ")
      (delete-region (point) (save-excursion (beginning-of-line) (point)))
      (goto-char (point-max))
      (skip-chars-backward " ")
      (delete-region (point) (save-excursion (end-of-line) (point)))
      ;; Temorarily set texinfo-stack to nil so texinfo-format-scan
      ;; does not see an unterminated @multitable.
      (let (texinfo-stack) ;; nil
	(texinfo-format-scan))
      (let (fill-prefix) ;; no fill prefix
	(fill-region (point-min) (point-max)))
      (setq table-entry-height
	    (max table-entry-height (count-lines (point-min) (point-max))))
;; 3. Move point to end of bottom line, and pad that line to fill column.
      (goto-char (point-min))
      (forward-line (1- table-entry-height))
      (let* ((beg (point)) ;; beginning of line
	     ;; add one more space for inter-column spacing
	     (needed-whitespace
	      (1+
	       (- fill-column
		  (progn
		    (end-of-line)
		    (current-column)))))) ;; end of existing line
	(insert (make-string
		 (if (> needed-whitespace 0) needed-whitespace 1)
		 ? )))
      ;; now, put formatted cell into a rectangle
      (set (intern (concat texinfo-multitable-rectangle-name
			   (int-to-string table-column)))
	   (extract-rectangle (point-min) (point)))
      (delete-region (point-min) (point))
      (goto-char (point-max))
      (setq table-column (1+ table-column))
      (widen))
;; 4. Add extra lines to rectangles so all are of same height
    (let ((total-number-of-columns table-column)
	  (column-number 0)
	  here)
      (while (> table-column 0)
	(let ((this-rectangle (int-to-string table-column)))
	  (while (< (length this-rectangle) table-entry-height)
	    (setq this-rectangle (append this-rectangle '("")))))
	(setq table-column (1- table-column)))
;; 5. Insert formatted rectangles in original buffer
      (switch-to-buffer original-buffer)
      (open-line table-entry-height)
      (while (< column-number total-number-of-columns)
	(setq here (point))
	(insert-rectangle
	 (eval (intern
		(concat texinfo-multitable-rectangle-name
			(int-to-string column-number)))))
	(goto-char here)
	(end-of-line)
	(setq column-number (1+ column-number))))
    (kill-buffer texinfo-multitable-buffer-name)
    (setq fill-column existing-fill-column)))


(ptexinfmt-defun-if-broken texinfo-format-printindex ()
  (let ((indexelts (symbol-value
		    (cdr (assoc (texinfo-parse-arg-discard)
				texinfo-indexvar-alist))))
	opoint)
    (insert "\n* Menu:\n\n")
    (setq opoint (point))
    (texinfo-print-index nil indexelts)

    (if (memq system-type '(vax-vms windows-nt ms-dos))
	(texinfo-sort-region opoint (point))
      (shell-command-on-region opoint (point) "sort -fd" 1))))


;; @copying ... @end copying
;; that Emacs 21.4 and lesser and XEmacs don't support.
(if (fboundp 'texinfo-copying)
    nil
  (defvar texinfo-copying-text ""
    "Text of the copyright notice and copying permissions.")

  (defun texinfo-copying ()
    "Copy the copyright notice and copying permissions from the Texinfo file,
as indicated by the @copying ... @end copying command;
insert the text with the @insertcopying command."
    (let ((beg (progn (beginning-of-line) (point)))
	  (end  (progn (re-search-forward "^@end copying[ \t]*\n") (point))))
      (setq texinfo-copying-text
	    (buffer-substring-no-properties
	     (save-excursion (goto-char beg) (forward-line 1) (point))
	     (save-excursion (goto-char end) (forward-line -1) (point))))
      (delete-region beg end)))

  (defun texinfo-insertcopying ()
    "Insert the copyright notice and copying permissions from the Texinfo file,
which are indicated by the @copying ... @end copying command."
    (insert (concat "\n" texinfo-copying-text)))

  (defadvice texinfo-format-scan (before expand-@copying-section activate)
    "Extract @copying and replace @insertcopying with it."
    (goto-char (point-min))
    (when (search-forward "@copying" nil t)
      (texinfo-copying))
    (while (search-forward "@insertcopying" nil t)
      (delete-region (match-beginning 0) (match-end 0))
      (texinfo-insertcopying))))

(provide 'ptexinfmt)

;;; ptexinfmt.el ends here
