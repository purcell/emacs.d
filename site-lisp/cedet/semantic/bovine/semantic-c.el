;;; semantic-c.el --- Semantic details for C

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-c.el,v 1.105 2009/02/26 03:11:48 zappo Exp $

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Support for the C/C++ bovine parser for Semantic.
;;
;; @todo - can I support c++-font-lock-extra-types ?

(require 'semantic)
(require 'semantic-lex-spp)
(require 'semantic-c-by)
(require 'backquote)

(eval-when-compile
  (require 'semantic-ctxt)
  (require 'semantic-imenu)
  (require 'semantic-tag-ls)
  (require 'senator)
  (require 'cc-mode))


;;; Compatibility
;;
(if (fboundp 'c-end-of-macro)
    (eval-and-compile
      (defalias 'semantic-c-end-of-macro 'c-end-of-macro))
  ;; From cc-mode 5.30
  (defun semantic-c-end-of-macro ()
    "Go to the end of a preprocessor directive.
More accurately, move point to the end of the closest following line
that doesn't end with a line continuation backslash.

This function does not do any hidden buffer changes."
    (while (progn
             (end-of-line)
             (when (and (eq (char-before) ?\\)
                        (not (eobp)))
               (forward-char)
               t))))
  )

;;; Code:
(when (member system-type '(gnu gnu/linux darwin cygwin))
  (semantic-gcc-setup))

;;; Pre-processor maps
;;
;;; Lexical analysis
(defvar semantic-lex-c-preprocessor-symbol-map-builtin
  '( ("__THROW" . "")
     ("__const" . "const")
     ("__restrict" . "")
     )
  "List of symbols to include by default.")

(defun semantic-c-reset-preprocessor-symbol-map ()
  "Reset the C preprocessor symbol map based on all input variables."
  (let ((filemap nil))
    (dolist (sf semantic-lex-c-preprocessor-symbol-file)
      ;; Global map entries
      (let* ((table (semanticdb-file-table-object sf)))
	(when table
	  (when (semanticdb-needs-refresh-p table)
	    (semanticdb-refresh-table table))
	  (setq filemap (append filemap (oref table lexical-table)))
	  )
	))

    (setq-mode-local c-mode
		     semantic-lex-spp-macro-symbol-obarray
		     (semantic-lex-make-spp-table
		      (append semantic-lex-c-preprocessor-symbol-map-builtin
			      semantic-lex-c-preprocessor-symbol-map
			      filemap))
		     )
    ))

;;;###autoload
(defcustom semantic-lex-c-preprocessor-symbol-map nil
  "Table of C Preprocessor keywords used by the Semantic C lexer.
Each entry is a cons cell like this:
  ( \"KEYWORD\" . \"REPLACEMENT\" )
Where KEYWORD is the macro that gets replaced in the lexical phase,
and REPLACEMENT is a string that is inserted in it's place.  Empty string
implies that the lexical analyzer will discard KEYWORD when it is encountered.

Alternately, it can be of the form:
  ( \"KEYWORD\" ( LEXSYM1 \"str\" 1 1 ) ... ( LEXSYMN \"str\" 1 1 ) )
where LEXSYM is a symbol that would normally be produced by the
lexical analyzer, such as `symbol' or `string'.  The string in the
second position is the text that makes up the replacement.  This is
the way to have multiple lexical symbols in a replacement.  Using the
first way to specify text like \"foo::bar\" would not work, because :
is a sepearate lexical symbol.

A quick way to see what you would need to insert is to place a
definition such as:

#define MYSYM foo::bar

into a C file, and do this:
  \\[semantic-lex-spp-describe]

The output table will describe the symbols needed."
  :group 'c
  :type '(repeat (cons (string :tag "Keyword")
		       (sexp :tag "Replacement")))
  :set (lambda (sym value)
	 (set-default sym value)
	 (condition-case nil
	     (semantic-c-reset-preprocessor-symbol-map)
	   (error nil))
	 )
  )

;;;###autoload
(defcustom semantic-lex-c-preprocessor-symbol-file nil
  "List of C/C++ files that contain preprocessor macros for the C lexer.
Each entry is a filename and each file is parsed, and those macros
are included in every C/C++ file parsed by semantic.
You can use this variable instead of `semantic-lex-c-preprocessor-symbol-map'
to store your global macros in a more natural way."
  :group 'c
  :type '(repeat (file :tag "File"))
  :set (lambda (sym value)
	 (set-default sym value)
	 (condition-case nil
	     (semantic-c-reset-preprocessor-symbol-map)
	   (error nil))
	 )
  )

;;;###autoload
(defcustom semantic-c-member-of-autocast 't
  "Non-nil means classes with a '->' operator will cast to it's return type.

For Examples:

  class Foo {
    Bar *operator->();
  }

  Foo foo;

if `semantic-c-member-of-autocast' is non-nil :
  foo->[here completion will list method of Bar]

if `semantic-c-member-of-autocast' is nil :
  foo->[here completion will list method of Foo]"
  :group 'c
  :type 'boolean)

;; XEmacs' autoload can't seem to byte compile the above because it
;; directly includes the entire defcustom.  To safely execute those,
;; it needs this next line to bootstrap in user settings w/out
;; loading in semantic-c.el at bootstrap time.
(semantic-c-reset-preprocessor-symbol-map)

(define-lex-spp-macro-declaration-analyzer semantic-lex-cpp-define
  "A #define of a symbol with some value.
Record the symbol in the semantic preprocessor.
Return the the defined symbol as a special spp lex token."
  "^\\s-*#\\s-*define\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 1
  (goto-char (match-end 0))
  (skip-chars-forward " \t")
  (if (eolp)
      nil
    (let* ((name (buffer-substring-no-properties 
		  (match-beginning 1) (match-end 1)))
	   (with-args (save-excursion
			(goto-char (match-end 0))
			(looking-at "(")))
	   (semantic-lex-spp-replacements-enabled nil)
	   ;; Temporarilly override the lexer to include
	   ;; special items needed inside a macro
	   (semantic-lex-analyzer #'semantic-cpp-lexer)
	   (raw-stream
	    (semantic-lex-spp-stream-for-macro (save-excursion
						 (semantic-c-end-of-macro)
						 (point))))
	   )

      ;; Only do argument checking if the paren was immediatly after
      ;; the macro name.
      (if with-args
	  (semantic-lex-spp-first-token-arg-list (car raw-stream)))

      ;; Magical spp variable for end point.
      (setq semantic-lex-end-point (point))

      ;; Handled nexted macro streams.
      (semantic-lex-spp-merge-streams raw-stream)
      )))

(define-lex-spp-macro-undeclaration-analyzer semantic-lex-cpp-undef
  "A #undef of a symbol.
Remove the symbol from the semantic preprocessor.
Return the the defined symbol as a special spp lex token."
  "^\\s-*#\\s-*undef\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 1)


;;; Conditional Skipping
;;
(defcustom semantic-c-obey-conditional-section-parsing-flag t
  "*Non-nil means to interpret preprocessor #if sections.
This implies that some blocks of code will not be parsed based on the
values of the conditions in the #if blocks."
  :group 'c
  :type 'boolean)

(defun semantic-c-skip-conditional-section ()
  "Skip one section of a conditional.
Moves forward to a matching #elif, #else, or #endif.
Movers completely over balanced #if blocks."
  (let ((done nil))
    ;; (if (looking-at "^\\s-*#if")
    ;; (semantic-lex-spp-push-if (point))
    (end-of-line)
    (while (and semantic-c-obey-conditional-section-parsing-flag
		(and (not done)
		     (re-search-forward
		      "^\\s-*#\\s-*\\(if\\(n?def\\)?\\|el\\(if\\|se\\)\\|endif\\)\\>"
		      nil t)))
      (goto-char (match-beginning 0))
      (cond
       ((looking-at "^\\s-*#\\s-*if")
	;; We found a nested if.  Skip it.
	(c-forward-conditional 1))
       ((looking-at "^\\s-*#\\s-*elif")
	;; We need to let the preprocessor analize this one.
	(beginning-of-line)
	(setq done t)
	)
       ((looking-at "^\\s-*#\\s-*\\(endif\\|else\\)\\>")
	;; We are at the end.  Pop our state.
	;; (semantic-lex-spp-pop-if)
	;; Note: We include ELSE and ENDIF the same. If skip some previous
	;; section, then we should do the else by default, making it much
	;; like the endif.
	(end-of-line)
	(forward-char 1)
	(setq done t))
       (t
	;; We found an elif.  Stop here.
	(setq done t))))))

(define-lex-regex-analyzer semantic-lex-c-if
  "Code blocks wrapped up in #if, or #ifdef.
Uses known macro tables in SPP to determine what block to skip."
  "^\\s-*#\\s-*\\(if\\|ifndef\\|ifdef\\|elif\\)\\s-+\\(!?defined(\\|\\)\\s-*\\(\\(\\sw\\|\\s_\\)+\\)\\(\\s-*)\\)?\\s-*$"
  (semantic-c-do-lex-if))

(defun semantic-c-do-lex-if ()
  "Handle lexical CPP if statements."
  (let* ((sym (buffer-substring-no-properties
	       (match-beginning 3) (match-end 3)))
	 (defstr (buffer-substring-no-properties
		  (match-beginning 2) (match-end 2)))
	 (defined (string= defstr "defined("))
	 (notdefined (string= defstr "!defined("))
	 (ift (buffer-substring-no-properties
	       (match-beginning 1) (match-end 1)))
	 (ifdef (or (string= ift "ifdef")
		    (and (string= ift "if") defined)
		    (and (string= ift "elif") defined)
		    ))
	 (ifndef (or (string= ift "ifndef")
		     (and (string= ift "if") notdefined)
		     (and (string= ift "elif") notdefined)
		     ))
	 )
    (if (or (and (or (string= ift "if") (string= ift "elif"))
		 (string= sym "0"))
	    (and ifdef (not (semantic-lex-spp-symbol-p sym)))
	    (and ifndef (semantic-lex-spp-symbol-p sym)))
	;; The if indecates to skip this preprocessor section
	(let ((pt nil))
	  ;; (message "%s %s yes" ift sym)
	  (beginning-of-line)
	  (setq pt (point))
	  ;;(c-forward-conditional 1)
	  ;; This skips only a section of a conditional.  Once that section
	  ;; is opened, encountering any new #else or related conditional
	  ;; should be skipped.
	  (semantic-c-skip-conditional-section)
	  (setq semantic-lex-end-point (point))
	  (semantic-push-parser-warning (format "Skip #%s %s" ift sym)
					pt (point))
;;	  (semantic-lex-push-token
;;	   (semantic-lex-token 'c-preprocessor-skip pt (point)))
	  nil)
      ;; Else, don't ignore it, but do handle the internals.
      ;;(message "%s %s no" ift sym)
      (end-of-line)
      (setq semantic-lex-end-point (point))
      nil)))

(define-lex-regex-analyzer semantic-lex-c-macro-else
  "Ignore an #else block.
We won't see the #else due to the macro skip section block
unless we are actively parsing an open #if statement.  In that
case, we must skip it since it is the ELSE part."
  "^\\s-*#\\s-*\\(else\\)"
  (let ((pt (point)))
    (semantic-c-skip-conditional-section)
    (setq semantic-lex-end-point (point))
    (semantic-push-parser-warning "Skip #else" pt (point))
;;    (semantic-lex-push-token
;;     (semantic-lex-token 'c-preprocessor-skip pt (point)))
    nil))

(define-lex-regex-analyzer semantic-lex-c-macrobits
  "Ignore various forms of #if/#else/#endif conditionals."
  "^\\s-*#\\s-*\\(if\\(n?def\\)?\\|endif\\|elif\\|else\\)"
  (semantic-c-end-of-macro)
  (setq semantic-lex-end-point (point))
  nil)

(define-lex-spp-include-analyzer semantic-lex-c-include-system
  "Identify include strings, and return special tokens."
    "^\\s-*#\\s-*include\\s-*<\\([^ \t\n>]+\\)>" 0
    ;; Hit 1 is the name of the include.
    (goto-char (match-end 0))
    (setq semantic-lex-end-point (point))
    (cons (buffer-substring-no-properties (match-beginning 1)
					  (match-end 1))
	  'system))

(define-lex-spp-include-analyzer semantic-lex-c-include
  "Identify include strings, and return special tokens."
    "^\\s-*#\\s-*include\\s-*\"\\([^ \t\n>]+\\)\"" 0
    ;; Hit 1 is the name of the include.
    (goto-char (match-end 0))
    (setq semantic-lex-end-point (point))
    (cons (buffer-substring-no-properties (match-beginning 1)
					  (match-end 1))
	  nil))
  

(define-lex-regex-analyzer semantic-lex-c-ignore-ending-backslash
  "Skip backslash ending a line.
Go to the next line."
  "\\\\\\s-*\n"
  (setq semantic-lex-end-point (match-end 0)))

(define-lex-regex-analyzer semantic-lex-c-string
  "Detect and create a C string token."
  "L?\\(\\s\"\\)"
  ;; Zing to the end of this string.
  (semantic-lex-push-token
   (semantic-lex-token
    'string (point)
    (save-excursion
      ;; Skip L prefix if present.
      (goto-char (match-beginning 1))
      (semantic-lex-unterminated-syntax-protection 'string
	(forward-sexp 1)
	(point))
      ))))

(define-lex-regex-analyzer semantic-c-lex-ignore-newline
  "Detect and ignore newline tokens.
Use this ONLY if newlines are not whitespace characters (such as when
they are comment end characters)."
  ;; Just like semantic-lex-ignore-newline, but also ignores
  ;; trailing \.
  "\\s-*\\\\?\\s-*\\(\n\\|\\s>\\)"
  (setq semantic-lex-end-point (match-end 0)))


(define-lex semantic-c-lexer
  "Lexical Analyzer for C code.
Use semantic-cpp-lexer for parsing text inside a CPP macro."
  ;; C preprocessor features
  semantic-lex-cpp-define
  semantic-lex-cpp-undef
  semantic-lex-c-if
  semantic-lex-c-macro-else
  semantic-lex-c-macrobits
  semantic-lex-c-include
  semantic-lex-c-include-system
  semantic-lex-c-ignore-ending-backslash
  ;; Whitespace handling
  semantic-lex-ignore-whitespace
  semantic-c-lex-ignore-newline
  ;; Non-preprocessor features
  semantic-lex-number
  ;; Must detect C strings before symbols because of possible L prefix!
  semantic-lex-c-string
  semantic-lex-spp-replace-or-symbol-or-keyword
  semantic-lex-charquote
  semantic-lex-paren-or-list
  semantic-lex-close-paren
  semantic-lex-ignore-comments
  semantic-lex-punctuation
  semantic-lex-default-action)

(define-lex-simple-regex-analyzer semantic-lex-cpp-hashhash
  "Match ## inside a CPP macro as special."
  "##" 'spp-concat)

(define-lex semantic-cpp-lexer
  "Lexical Analyzer for CPP macros in C code."
  ;; CPP special
  semantic-lex-cpp-hashhash
  ;; C preprocessor features
  semantic-lex-cpp-define
  semantic-lex-cpp-undef
  semantic-lex-c-if
  semantic-lex-c-macro-else
  semantic-lex-c-macrobits
  semantic-lex-c-include
  semantic-lex-c-include-system
  semantic-lex-c-ignore-ending-backslash
  ;; Whitespace handling
  semantic-lex-ignore-whitespace
  semantic-c-lex-ignore-newline
  ;; Non-preprocessor features
  semantic-lex-number
  ;; Must detect C strings before symbols because of possible L prefix!
  semantic-lex-c-string
  ;; Parsing inside a macro means that we don't do macro replacement.
  ;; semantic-lex-spp-replace-or-symbol-or-keyword
  semantic-lex-symbol-or-keyword
  semantic-lex-charquote
  semantic-lex-paren-or-list
  semantic-lex-close-paren
  semantic-lex-ignore-comments
  semantic-lex-punctuation
  semantic-lex-default-action)

(define-mode-local-override semantic-parse-region c-mode 
  (start end &optional nonterminal depth returnonerror)
  "Calls 'semantic-parse-region-default', except in a macro expansion.
MACRO expansion mode is handled through the nature of Emacs's non-lexical
binding of variables.
START, END, NONTERMINAL, DEPTH, and RETURNONERRORS are the same
as for the parent."
  (if (and (boundp 'lse) (or (/= start 1) (/= end (point-max))))
      (let* ((last-lexical-token lse)
	     (macroexpand (stringp (car (cdr last-lexical-token)))))
	(if macroexpand
  	    (progn
	      ;; It is a macro expansion.  Do something special.
	      ;;(message "MOOSE %S %S, %S : %S" start end nonterminal lse)
	      (semantic-c-parse-lexical-token
	       lse nonterminal depth returnonerror)
	      )
	  ;; Not a macro expansion.  the old thing.
	  (semantic-parse-region-default start end 
					 nonterminal depth
					 returnonerror)
	  ))
    ;; Else, do the old thing.
    (semantic-parse-region-default start end nonterminal
				   depth returnonerror)
    ))

(defun semantic-c-parse-lexical-token (lexicaltoken nonterminal depth
						    returnonerror)
  "Do a region parse on the contents of LEXICALTOKEN.
Presumably, this token has a string in it from a macro.
The text of the token is inserted into a different buffer, and
parsed there.
Argument NONTERMINAL, DEPTH, and RETURNONERROR are passed into
the regular parser."
  (let* ((buf (get-buffer-create " *C parse hack*"))
	 (mode major-mode)
	 (spp-syms semantic-lex-spp-dynamic-macro-symbol-obarray)
	 (stream nil)
	 (start (semantic-lex-token-start lexicaltoken))
	 (end (semantic-lex-token-end lexicaltoken))
	 (symtext (semantic-lex-token-text lexicaltoken))
	 (macros (get-text-property 0 'macros symtext))
	 )
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (when (not (eq major-mode mode))
	(funcall mode)
	;; Hack in mode-local
	(activate-mode-local-bindings)
	;; CHEATER!  The following 3 lines are from
	;; `semantic-new-buffer-fcn', but we don't want to turn
	;; on all the other annoying modes for this little task.
	(setq semantic-new-buffer-fcn-was-run t)
	(semantic-lex-init)
	(semantic-clear-toplevel-cache)
	(remove-hook 'semantic-lex-reset-hooks 'semantic-lex-spp-reset-hook
		     t)
	)
      ;; Get the macro symbol table right.
      (setq semantic-lex-spp-dynamic-macro-symbol-obarray spp-syms)
      ;; (message "%S" macros)
      (dolist (sym macros)
	(semantic-lex-spp-symbol-set (car sym) (cdr sym)))

      (insert symtext)

      (setq stream
	    (semantic-parse-region-default
	     (point-min) (point-max) nonterminal depth returnonerror))

      ;; Clean up macro symbols
      (dolist (sym macros)
	(semantic-lex-spp-symbol-remove (car sym)))

      ;; Convert the text of the stream.
      (dolist (tag stream)
	;; Only do two levels here 'cause I'm lazy.
	(semantic--tag-set-overlay tag (list start end))
	(dolist (stag (semantic-tag-components-with-overlays tag))
	  (semantic--tag-set-overlay stag (list start end))
	  ))
      )
    stream))

(defun semantic-expand-c-tag (tag)
  "Expand TAG into a list of equivalent tags, or nil."
  (cond ((eq (semantic-tag-class tag) 'extern)
	 ;; We have hit an exter "C" command with a list after it.
	 (let* ((mb (semantic-tag-get-attribute tag :members))
		(ret mb))
	   (while mb
	     (let ((mods (semantic-tag-get-attribute (car mb) :typemodifiers)))
	       (setq mods (cons "extern" (cons "\"C\"" mods)))
	       (semantic-tag-put-attribute (car mb) :typemodifiers mods))
	     (setq mb (cdr mb)))
	   ret))
	((listp (car tag))
	 (cond ((eq (semantic-tag-class tag) 'variable)
		;; The name part comes back in the form of:
		;; ( NAME NUMSTARS BITS ARRAY ASSIGN )
		(let ((vl nil)
		      (basety (semantic-tag-type tag))
		      (ty "")
		      (mods (semantic-tag-get-attribute tag :typemodifiers))
		      (suffix "")
		      (lst (semantic-tag-name tag))
		      (default nil)
		      (cur nil))
		  (while lst
		    (setq suffix "" ty "")
		    (setq cur (car lst))
		    (if (nth 2 cur)
			(setq suffix (concat ":" (nth 2 cur))))
		    (if (= (length basety) 1)
			(setq ty (car basety))
		      (setq ty basety))
		    (setq default (nth 4 cur))
		    (setq vl (cons
			      (semantic-tag-new-variable
			       (car cur) ;name
			       ty	;type
			       (if default
				   (buffer-substring-no-properties
				    (car default) (car (cdr default))))
			       :constant-flag (semantic-tag-variable-constant-p tag)
			       :suffix suffix
			       :typemodifiers mods
			       :dereference (length (nth 3 cur))
			       :pointer (nth 1 cur)
			       :documentation (semantic-tag-docstring tag) ;doc
			       )
			      vl))
		    (semantic--tag-copy-properties tag (car vl))
		    (semantic--tag-set-overlay (car vl)
					       (semantic-tag-overlay tag))
		    (setq lst (cdr lst)))
		  vl))
	       ((eq (semantic-tag-class tag) 'type)
		;; We may someday want to add an extra check for a type
		;; of type "typedef".
		;; Each elt of NAME is ( STARS NAME )
		(let ((vl nil)
		      (names (semantic-tag-name tag)))
		  (while names
		    (setq vl (cons (semantic-tag-new-type
				    (nth 1 (car names)) ; name
				    "typedef"
				    (semantic-tag-type-members tag)
				    ;; parent is just tbe name of what
				    ;; is passed down as a tag.
				    (list
				     (semantic-tag-name
				      (semantic-tag-type-superclasses tag)))
				    :pointer
				    (let ((stars (car (car (car names)))))
				      (if (= stars 0) nil stars))
				    ;; This specifies what the typedef
				    ;; is expanded out as.  Just the
				    ;; name shows up as a parent of this
				    ;; typedef.
				    :typedef
				    (semantic-tag-get-attribute tag :superclasses)
				    ;;(semantic-tag-type-superclasses tag)
				    :documentation
				    (semantic-tag-docstring tag))
				   vl))
		    (semantic--tag-copy-properties tag (car vl))
		    (semantic--tag-set-overlay (car vl)
					       (semantic-tag-overlay tag))
		    (setq names (cdr names)))
		  vl))
	       ((and (listp (car tag))
		     (eq (semantic-tag-class (car tag)) 'variable))
		;; Argument lists come in this way.  Append all the expansions!
		(let ((vl nil))
		  (while tag
		    (setq vl (append (semantic-tag-components (car vl))
				     vl)
			  tag (cdr tag)))
		  vl))
	       (t nil)))
	(t nil)))

(defvar-mode-local c-mode semantic-tag-expand-function 'semantic-expand-c-tag
  "Function used to expand tags generated in the C bovine parser.")

(defvar semantic-c-classname nil
  "At parse time, assign a class or struct name text here.
It is picked up by `semantic-c-reconstitute-token' to determine
if something is a constructor.  Value should be:
  ( TYPENAME .  TYPEOFTYPE)
where typename is the name of the type, and typeoftype is \"class\"
or \"struct\".")

(defun semantic-c-reconstitute-token (tokenpart declmods typedecl)
  "Reconstitute a token TOKENPART with DECLMODS and TYPEDECL.
This is so we don't have to match the same starting text several times.
Optional argument STAR and REF indicate the number of * and & in the typedef."
  (when (and (listp typedecl)
	     (= 1 (length typedecl))
	     (stringp (car typedecl)))
    (setq typedecl (car typedecl)))
  (cond ((eq (nth 1 tokenpart) 'variable)
	 (semantic-tag-new-variable
	  (car tokenpart)
	  (or typedecl "int")	;type
	  nil			;default value (filled with expand)
	  :constant-flag (if (member "const" declmods) t nil)
	  :typemodifiers (delete "const" declmods)
	  )
	 )
	((eq (nth 1 tokenpart) 'function)
	 ;; We should look at part 4 (the arglist) here, and throw an
	 ;; error of some sort if it contains parser errors so that we
	 ;; don't parser function calls, but that is a little beyond what
	 ;; is available for data here.
	 (let* ((constructor
		 (and (or (and semantic-c-classname
			       (string= (car semantic-c-classname)
					(car tokenpart)))
			  (and (stringp (car (nth 2 tokenpart)))
			       (string= (car (nth 2 tokenpart)) (car tokenpart)))
			  )
		      (not (car (nth 3 tokenpart)))))
		(fcnpointer (and (string-match "^\\*" (car tokenpart))
				 (string-match "[a-z][A-Z]" (car tokenpart))))
		(fnname (if fcnpointer
			    (substring (car tokenpart) 1)
			  (car tokenpart)))
		(operator (if (string-match "[a-zA-Z]" fnname)
			      nil
			    t))
		)
	   (if fcnpointer
	       ;; Function pointers are really variables.
	       (semantic-tag-new-variable
		fnname
		typedecl
		nil
		;; It is a function pointer
		:functionpointer-flag t
		)
	     ;; The function
	     (semantic-tag-new-function
	      fnname
	      (or typedecl		;type
		  (cond ((car (nth 3 tokenpart) )
			 "void")	; Destructors have no return?
			(constructor
			 ;; Constructors return an object.
			 (semantic-tag-new-type
			  ;; name
			  (or (car semantic-c-classname)
			      (car (nth 2 tokenpart)))
			  ;; type
			  (or (cdr semantic-c-classname)
			      "class")
			  ;; members
			  nil
			  ;; parents
			  nil
			  ))
			(t "int")))
	      (nth 4 tokenpart)		;arglist
	      :constant-flag (if (member "const" declmods) t nil)
	      :typemodifiers (delete "const" declmods)
	      :parent (car (nth 2 tokenpart))
	      :destructor-flag (if (car (nth 3 tokenpart) ) t)
	      :constructor-flag (if constructor t)
	      :pointer (nth 7 tokenpart)
	      :operator-flag operator
	      ;; Even though it is "throw" in C++, we use
	      ;; `throws' as a common name for things that toss
	      ;; exceptions about.
	      :throws (nth 5 tokenpart)
	      ;; Reemtrant is a C++ thingy.  Add it here
	      :reentrant-flag (if (member "reentrant" (nth 6 tokenpart)) t)
	      ;; A function post-const is funky.  Try stuff
	      :methodconst-flag (if (member "const" (nth 6 tokenpart)) t)
	      ;; prototypes are functions w/ no body
	      :prototype-flag (if (nth 8 tokenpart) t)
	      ;; Pure virtual
	      :pure-virtual-flag (if (eq (nth 8 tokenpart) :pure-virtual-flag) t)
	      ;; Template specifier.
	      :template-specifier (nth 9 tokenpart)
	      )))
	 )
	))

(defun semantic-c-reconstitute-template (tag specifier)
  "Reconstitute the token TAG with the template SPECIFIER."
  (semantic-tag-put-attribute tag :template (or specifier ""))
  tag)

;;; Override methods & Variables
;;
(defcustom-mode-local-semantic-dependency-system-include-path
  c-mode semantic-c-dependency-system-include-path
  '("/usr/include")
  "The system include path used by the C langauge.")

(defcustom semantic-default-c-path nil
  "Default set of include paths for C code.
Used by `semantic-dep' to define an include path.
NOTE: In process of obsoleting this."
  :group 'c
  :group 'semantic
  :type '(repeat (string :tag "Path")))

(defvar-mode-local c-mode semantic-dependency-include-path
  semantic-default-c-path
  "System path to search for include files.")

(define-mode-local-override semantic-format-tag-name
  c-mode (tag &optional parent color)
  "Convert TAG to a string that is the print name for TAG.
Optional PARENT and COLOR are ignored."
  (let ((name (semantic-format-tag-name-default tag parent color))
	(fnptr (semantic-tag-get-attribute tag :functionpointer-flag))
	)
    (if (not fnptr)
	name
      (concat "(*" name ")"))
    ))

(define-mode-local-override semantic-format-tag-canonical-name
  c-mode (tag &optional parent color)
  "Create a cannonical name for TAG.
PARENT specifies a parent class.
COLOR indicates that the text should be type colorized.
Enhances the base class to search for the entire parent
tree to make the name accurate."
  (semantic-format-tag-canonical-name-default tag parent color)
  )

(define-mode-local-override semantic-format-tag-type c-mode (tag color)
  "Convert the data type of TAG to a string usable in tag formatting.
Adds pointer and reference symbols to the default.
Argument COLOR adds color to the text."
  (let* ((type (semantic-tag-type tag))
	 (defaulttype nil)
	 (point (semantic-tag-get-attribute tag :pointer))
	 (ref (semantic-tag-get-attribute tag :reference))
	 )
    (if (semantic-tag-p type)
	(let ((typetype (semantic-tag-type type))
	      (typename (semantic-tag-name type)))
	  ;; Create the string that expresses the type
	  (if (string= typetype "class")
	      (setq defaulttype typename)
	    (setq defaulttype (concat typetype " " typename))))
      (setq defaulttype (semantic-format-tag-type-default tag color)))
      
    ;; Colorize
    (when color 
      (setq defaulttype (semantic--format-colorize-text defaulttype 'type)))

    ;; Add refs, ptrs, etc
    (if ref (setq ref "&"))
    (if point (setq point (make-string point ?*)) "")
    (when type
      (concat defaulttype ref point))
    ))

(define-mode-local-override semantic-find-tags-by-scope-protection
  c-mode (scopeprotection parent &optional table)
  "Override the usual search for protection.
We can be more effective than the default by scanning through once,
and collecting tags based on the labels we see along the way."
  (if (not table) (setq table (semantic-tag-type-members parent)))
  (if (null scopeprotection)
      table
    (let ((ans nil)
	  (curprot 1)
	  (targetprot (cond ((eq scopeprotection 'public)
			     1)
			    ((eq scopeprotection 'protected)
			     2)
			    (t 3)
			    ))
	  (alist '(("public" . 1)
		   ("protected" . 2)
		   ("private" . 3)))
	  )
      (dolist (tag table)
	(cond 
	 ((semantic-tag-of-class-p tag 'label)
	  (setq curprot (cdr (assoc (semantic-tag-name tag) alist)))
	  )
	 ((>= targetprot curprot)
	  (setq ans (cons tag ans)))
	 ))
      ans)))

(define-mode-local-override semantic-tag-protection
  c-mode (tag &optional parent)
  "Return the protection of TAG in PARENT.
Override function for `semantic-tag-protection'."
  (let ((mods (semantic-tag-modifiers tag))
	(prot nil))
    ;; Check the modifiers for protection if we are not a child
    ;; of some class type.
    (when (or (not parent) (not (eq (semantic-tag-class parent) 'type)))
      (while (and (not prot) mods)
	(if (stringp (car mods))
	    (let ((s (car mods)))
	      ;; A few silly defaults to get things started.
	      (cond ((or (string= s "extern")
			 (string= s "export"))
		     'public)
		    ((string= s "static")
		     'private))))
	(setq mods (cdr mods))))
    ;; If we have a typed parent, look for :public style labels.
    (when (and parent (eq (semantic-tag-class parent) 'type))
      (let ((pp (semantic-tag-type-members parent)))
	(while (and pp (not (semantic-equivalent-tag-p (car pp) tag)))
	  (when (eq (semantic-tag-class (car pp)) 'label)
	    (setq prot
		  (cond ((string= (semantic-tag-name (car pp)) "public")
			 'public)
			((string= (semantic-tag-name (car pp)) "private")
			 'private)
			((string= (semantic-tag-name (car pp)) "protected")
			 'protected)))
	    )
	  (setq pp (cdr pp)))))
    (when (and (not prot) (eq (semantic-tag-class parent) 'type))
      (setq prot
	    (cond ((string= (semantic-tag-type parent) "class") 'private)
		  ((string= (semantic-tag-type parent) "struct") 'public)
		  (t 'unknown))))
    (or prot
	(if (and parent (semantic-tag-of-class-p parent 'type))
	    'public
	  nil))))

(define-mode-local-override semantic-tag-components c-mode (tag)
  "Return components for TAG."
  (if (and (eq (semantic-tag-class tag) 'type)
	   (string= (semantic-tag-type tag) "typedef"))
      ;; A typedef can contain a parent who has positional children,
      ;; but that parent will not have a position.  Do this funny hack
      ;; to make sure we can apply overlays properly.
      (let ((sc (semantic-tag-get-attribute tag :typedef)))
	(when (semantic-tag-p sc) (semantic-tag-components sc)))
    (semantic-tag-components-default tag)))

(defun semantic-c-tag-template (tag)
  "Return the template specification for TAG, or nil."
  (semantic-tag-get-attribute tag :template))

(defun semantic-c-tag-template-specifier (tag)
  "Return the template specifier specification for TAG, or nil."
  (semantic-tag-get-attribute tag :template-specifier))

(defun semantic-c-template-string-body (templatespec)
  "Convert TEMPLATESPEC into a string.
This might be a string, or a list of tokens."
  (cond ((stringp templatespec)
	 templatespec)
	((semantic-tag-p templatespec)
	 (semantic-format-tag-abbreviate templatespec))
	((listp templatespec)
	 (mapconcat 'semantic-format-tag-abbreviate templatespec ", "))))

(defun semantic-c-template-string (token &optional parent color)
  "Return a string representing the TEMPLATE attribute of TOKEN.
This string is prefixed with a space, or is the empty string.
Argument PARENT specifies a parent type.
Argument COLOR specifies that the string should be colorized."
  (let ((t2 (semantic-c-tag-template-specifier token))
	(t1 (semantic-c-tag-template token))
	;; @todo - Need to account for a parent that is a template
	(pt1 (if parent (semantic-c-tag-template parent)))
	(pt2 (if parent (semantic-c-tag-template-specifier parent)))
	)
    (cond (t2 ;; we have a template with specifier
	   (concat " <"
		   ;; Fill in the parts here
		   (semantic-c-template-string-body t2)
		   ">"))
	  (t1 ;; we have a template without specifier
	   " <>")
	  (t
	   ""))))

(define-mode-local-override semantic-format-tag-concise-prototype
  c-mode (token &optional parent color)
  "Return an abbreviated string describing TOKEN for C and C++.
Optional PARENT and COLOR as specified with
`semantic-format-tag-abbreviate-default'."
  ;; If we have special template things, append.
  (concat  (semantic-format-tag-concise-prototype-default token parent color)
	   (semantic-c-template-string token parent color)))

(define-mode-local-override semantic-format-tag-uml-prototype
  c-mode (token &optional parent color)
  "Return an uml string describing TOKEN for C and C++.
Optional PARENT and COLOR as specified with
`semantic-abbreviate-tag-default'."
  ;; If we have special template things, append.
  (concat  (semantic-format-tag-uml-prototype-default token parent color)
	   (semantic-c-template-string token parent color)))

(define-mode-local-override semantic-tag-abstract-p
  c-mode (tag &optional parent)
  "Return non-nil if TAG is considered abstract.
PARENT is tag's parent.
In C, a method is abstract if it is `virtual', which is already
handled.  A class is abstract iff it's destructor is virtual."
  (cond
   ((eq (semantic-tag-class tag) 'type)
    (or (semantic-brute-find-tag-by-attribute :pure-virtual-flag
					      (semantic-tag-components tag)
					      )
	(let* ((ds (semantic-brute-find-tag-by-attribute
		    :destructor-flag
		    (semantic-tag-components tag)
		    ))
	       (cs (semantic-brute-find-tag-by-attribute
		    :constructor-flag
		    (semantic-tag-components tag)
		    )))
	  (and ds (member "virtual" (semantic-tag-modifiers (car ds)))
	       cs (eq 'protected (semantic-tag-protection (car cs) tag))
	       )
	  )))
   ((eq (semantic-tag-class tag) 'function)
    (or (semantic-tag-get-attribute tag :pure-virtual-flag)
        (member "virtual" (semantic-tag-modifiers tag))))
   (t (semantic-tag-abstract-p-default tag parent))))

(defun semantic-c-dereference-typedef (type scope &optional type-declaration)
  "If TYPE is a typedef, get TYPE's type by name or tag, and return."         
  (if (and (eq (semantic-tag-class type) 'type)
           (string= (semantic-tag-type type) "typedef"))
      (let ((dt (semantic-tag-get-attribute type :typedef)))
        (cond ((and (semantic-tag-p dt)
                    (not (semantic-analyze-tag-prototype-p dt)))
               (list dt dt))
              ((stringp dt) (list dt (semantic-tag dt 'type)))
              ((consp dt) (list (car dt) dt))))

    (list type type-declaration)))

(defun semantic-c--instantiate-template (tag def-list spec-list)
  "Replace TAG name according to template specification.
DEF-LIST is the template information.
SPEC-LIST is the template specifier of the datatype instantiated."
  (when (and (car def-list) (car spec-list))

    (when (and (string= (semantic-tag-type (car def-list)) "class") 
               (string= (semantic-tag-name tag) (semantic-tag-name (car def-list))))
      (semantic-tag-set-name tag (semantic-tag-name (car spec-list))))

    (semantic-c--instantiate-template tag (cdr def-list) (cdr spec-list))))

(defun semantic-c--template-name-1 (spec-list)
  "return a string used to compute template class name based on SPEC-LIST
for ref<Foo,Bar> it will return 'Foo,Bar'."
  (when (car spec-list)
    (let* ((endpart (semantic-c--template-name-1 (cdr spec-list)))
	   (separator (and endpart ",")))
      (concat (semantic-tag-name (car spec-list)) separator endpart))))

(defun semantic-c--template-name (type spec-list)
  "Return a template class name for TYPE based on SPEC-LIST.
For a type `ref' with a template specifier of (Foo Bar) it will
return 'ref<Foo,Bar>'."
  (concat (semantic-tag-name type)
	  "<" (semantic-c--template-name-1 (cdr spec-list)) ">"))

(defun semantic-c-dereference-template (type scope &optional type-declaration)
  "Dereference any template specifieres in TYPE within SCOPE.
If TYPE is a template, return a TYPE copy with the templates types
instantiated as specified in TYPE-DECLARATION."
  (when (semantic-tag-p type-declaration)
    (let ((def-list  (semantic-tag-get-attribute type :template))
          (spec-list (semantic-tag-get-attribute type-declaration :template-specifier)))
      (when (and def-list spec-list)
        (setq type (semantic-tag-deep-copy-one-tag
		    type
		    (lambda (tag)
		      (when (semantic-tag-of-class-p tag 'type)
			(semantic-c--instantiate-template
			 tag def-list spec-list))
		      tag)
		    ))
        (semantic-tag-set-name type (semantic-c--template-name type spec-list))
        (semantic-tag-put-attribute type :template nil)
        (semantic-tag-set-faux type))))
  (list type type-declaration))

(defun semantic-c-dereference-member-of (type scope &optional type-declaration)
  "Dereference through the `->' operator of TYPE.
Uses the return type of the '->' operator if it is contained in TYPE.
SCOPE is the current local scope to perform searches in.
TYPE-DECLARATION is passed through."
  (if semantic-c-member-of-autocast
      (let ((operator (car (semantic-find-tags-by-name "->" (semantic-analyze-scoped-type-parts type)))))
        (if operator 
            (list (semantic-tag-get-attribute operator :type) (semantic-tag-get-attribute operator :type))
          (list type type-declaration)))
    (list type type-declaration)))


(define-mode-local-override semantic-analyze-dereference-metatype
  c-mode (type scope &optional type-declaration)
  "Dereference TYPE as described in `semantic-analyze-dereference-metatype'.
Handle typedef, template instantiation, and '->' operator."
  (let* ((dereferencer-list '(semantic-c-dereference-typedef 
                              semantic-c-dereference-template
                              semantic-c-dereference-member-of))
         (dereferencer (pop dereferencer-list))
         (type-tuple)
         (original-type type))
    (while dereferencer
      (setq type-tuple (funcall dereferencer type scope type-declaration)
            type (car type-tuple) 
            type-declaration (cadr type-tuple))      
      (if (not (eq type original-type))
          ;; we found a new type so break the dereferencer loop now ! 
          ;; (we will be recalled with the new type expanded by 
          ;; semantic-analyze-dereference-metatype-stack).
          (setq dereferencer nil)        
        ;; no new type found try the next dereferencer :
        (setq dereferencer (pop dereferencer-list)))))
    (list type type-declaration))

(define-mode-local-override semantic-analyze-type-constants c-mode (type)
  "When TYPE is a tag for an enum, return it's parts.
These are constants which are of type TYPE."
  (if (and (eq (semantic-tag-class type) 'type)
	   (string= (semantic-tag-type type) "enum"))
      (semantic-tag-type-members type)))

(define-mode-local-override semantic-analyze-split-name c-mode (name)
  "Split up tag names on colon (:) boundaries."
  (let ((ans (split-string name ":")))
    (if (= (length ans) 1)
	name
      (delete "" ans))))

(define-mode-local-override semantic-analyze-unsplit-name c-mode (namelist)
  "Assemble the list of names NAMELIST into a namespace name."
  (mapconcat 'identity namelist "::"))

(define-mode-local-override semantic-ctxt-scoped-types c++-mode (&optional point)
  "Return a list of tags of CLASS type based on POINT.
DO NOT return the list of tags encompassing point."
  (when point (goto-char (point)))
  (let ((tagsaroundpoint (semantic-find-tag-by-overlay))
	(tagreturn nil)
	(tmp nil))
    ;; In C++, we want to find all the namespaces declared
    ;; locally and add them to the list.
    (setq tmp (semantic-find-tags-by-class 'type (current-buffer)))
    (setq tmp (semantic-find-tags-by-type "namespace" tmp))
    (setq tmp (semantic-find-tags-by-name "unnamed" tmp))
    (setq tagreturn tmp)
    ;; We should also find all "using" type statements and
    ;; accept those entities in as well.
    (setq tmp (semanticdb-find-tags-by-class 'using))
    (let ((idx 0)
	  (len (semanticdb-find-result-length tmp)))
      (while (< idx len)
	(setq tagreturn (cons (semantic-tag-type (car (semanticdb-find-result-nth tmp idx))) tagreturn))
	(setq idx (1+ idx)))
      )
    ;; Use the encompased types around point to also look for using statements.
    ;;(setq tagreturn (cons "bread_name" tagreturn))
    (while (cdr tagsaroundpoint)  ; don't search the last one
      (setq tmp (semantic-find-tags-by-class 'using (semantic-tag-components (car tagsaroundpoint))))
      (dolist (T tmp)
	(setq tagreturn (cons (semantic-tag-type T) tagreturn))
	)
      (setq tagsaroundpoint (cdr tagsaroundpoint))
      )
    ;; Return the stuff
    tagreturn
    ))

(define-mode-local-override semantic-get-local-variables c++-mode ()
  "Do what `semantic-get-local-variables' does, plus add `this' if needed."
  (let* ((origvar (semantic-get-local-variables-default))
	 (ct (semantic-current-tag))
	 (p (semantic-tag-function-parent ct)))
    ;; If we have a function parent, then that implies we can
    (if (and p (semantic-tag-of-class-p ct 'function))
	;; Append a new tag THIS into our space.
	(cons (semantic-tag-new-variable "this" p nil)
	      origvar)
      ;; No parent, just return the usual
      origvar)
    ))

(define-mode-local-override semantic-idle-summary-current-symbol-info
  c-mode ()
  "Handle the SPP keywords, then use the default mechanism."
  (let* ((sym (car (semantic-ctxt-current-thing)))
	 (spp-sym (semantic-lex-spp-symbol sym)))
    (if spp-sym
	(let* ((txt (concat "Macro: " sym))
	       (sv  (symbol-value spp-sym))
	       (arg (semantic-lex-spp-macro-with-args sv))
	       )
	  (when arg
	    (setq txt (concat txt (format "%S" arg)))
	    (setq sv (cdr sv)))

	  ;; This is optional, and potentially fraught w/ errors.
	  (condition-case nil
	      (dolist (lt sv)
		(setq txt (concat txt " " (semantic-lex-token-text lt))))
	    (error (setq txt (concat txt "  #error in summary fcn"))))

	  txt)
      (semantic-idle-summary-current-symbol-info-default))))

(defvar-mode-local c-mode semantic-orphaned-member-metaparent-type "struct"
  "When lost memberes are found in the class hierarchy generator, use a struct.")

(defvar-mode-local c-mode semantic-symbol->name-assoc-list
  '((type     . "Types")
    (variable . "Variables")
    (function . "Functions")
    (include  . "Includes")
    )
  "List of tag classes, and strings to describe them.")

(defvar-mode-local c-mode semantic-symbol->name-assoc-list-for-type-parts
  '((type     . "Types")
    (variable . "Attributes")
    (function . "Methods")
    (label    . "Labels")
    )
  "List of tag classes in a datatype decl, and strings to describe them.")

(defvar-mode-local c-mode imenu-create-index-function 'semantic-create-imenu-index
  "Imenu index function for C.")

(defvar-mode-local c-mode semantic-type-relation-separator-character 
  '("." "->" "::")
  "Separator characters between something of a given type, and a field.")

(defvar-mode-local c-mode semantic-command-separation-character ";"
  "Commen separation character for C")

(defvar-mode-local c-mode senator-step-at-tag-classes '(function variable)
  "Tag classes where senator will stop at the end.")

;;;###autoload
(defun semantic-default-c-setup ()
  "Set up a buffer for semantic parsing of the C language."
  (semantic-c-by--install-parser)
  (setq semantic-lex-syntax-modifications '((?> ".")
                                            (?< ".")
                                            )
        )
  
  (setq semantic-lex-analyzer #'semantic-c-lexer)
  (add-hook 'semantic-lex-reset-hooks 'semantic-lex-spp-reset-hook nil t)
  )

;;;###autoload
(defun semantic-c-add-preprocessor-symbol (sym replacement)
  "Add a preprocessor symbol SYM with a REPLACEMENT value."
  (interactive "sSymbol: \nsReplacement: ")
  (let ((SA (assoc sym semantic-lex-c-preprocessor-symbol-map)))
    (if SA
	;; Replace if there is one.
	(setcdr SA replacement)
      ;; Otherwise, append
      (setq semantic-lex-c-preprocessor-symbol-map
	    (cons  (cons sym replacement)
		   semantic-lex-c-preprocessor-symbol-map))))

  (semantic-c-reset-preprocessor-symbol-map)
  )

;;;###autoload
(add-hook 'c-mode-hook 'semantic-default-c-setup)
;;;###autoload
(add-hook 'c++-mode-hook 'semantic-default-c-setup)

(define-child-mode c++-mode c-mode
  "`c++-mode' uses the same parser as `c-mode'.")

;;; SETUP QUERY
;;
(defun semantic-c-describe-environment ()
  "Describe the Semantic features of the current C environment."
  (interactive)
  (if (not (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode)))
      (error "Not useful to query C mode in %s mode" major-mode))
  (let ((gcc (when (boundp 'semantic-gcc-setup-data)
	       semantic-gcc-setup-data))
	)
    (semantic-fetch-tags)

    (with-output-to-temp-buffer "*Semantic C Environment*"
      (when gcc
	(princ "Calculated GCC Parameters:")
	(dolist (P gcc)
	  (princ "\n  ")
	  (princ (car P))
	  (princ " = ")
	  (princ (cdr P))
	  )
	)

      (princ "\n\nInclude Path Summary:\n")
      (when ede-object
	(princ "\n  This file's project include is handled by:\n")
	(princ "   ")
	(princ (object-print ede-object))
	(princ "\n  with the system path:\n")
	(dolist (dir (ede-system-include-path ede-object))
	  (princ "    ")
	  (princ dir)
	  (princ "\n"))
	)

      (when semantic-dependency-include-path
	(princ "\n  This file's generic include path is:\n")
	(dolist (dir semantic-dependency-include-path)
	  (princ "    ")
	  (princ dir)
	  (princ "\n")))

      (when semantic-dependency-system-include-path
	(princ "\n  This file's system include path is:\n")
	(dolist (dir semantic-dependency-system-include-path)
	  (princ "    ")
	  (princ dir)
	  (princ "\n")))

      (princ "\n\nMacro Summary:\n")
      (when semantic-lex-c-preprocessor-symbol-file
	(princ "\n  Your CPP table is primed from these files:\n")
	(dolist (file semantic-lex-c-preprocessor-symbol-file)
	  (princ "    ")
	  (princ file)
	  (princ "\n")
	  (princ "    in table: ")
	  (princ (object-print (semanticdb-file-table-object file)))
	  (princ "\n")
	  ))

      (when semantic-lex-c-preprocessor-symbol-map-builtin
	(princ "\n  Built-in symbol map:\n")
	(dolist (S semantic-lex-c-preprocessor-symbol-map-builtin)
	  (princ "    ")
	  (princ (car S))
	  (princ " = ")
	  (princ (cdr S))
	  (princ "\n")
	  ))

      (when semantic-lex-c-preprocessor-symbol-map
	(princ "\n  User symbol map:\n")
	(dolist (S semantic-lex-c-preprocessor-symbol-map)
	  (princ "    ")
	  (princ (car S))
	  (princ " = ")
	  (princ (cdr S))
	  (princ "\n")
	  ))

      (princ "\n\n  Use: M-x semantic-lex-spp-describe RET\n")
      (princ "\n  to see the complete macro table.\n")

      )))

(provide 'semantic-c)

;;; semantic-c.el ends here

