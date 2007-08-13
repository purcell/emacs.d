;;; qp-c.el --- C scanning for `quickpeek'

;;; Copyright (C) 1999, 2000 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tools
;; X-RCS: $Id: qp-c.el,v 1.5 2005/09/30 20:42:05 zappo Exp $

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; 
;; This program handles C quick peeking.  It depends on etag files
;; for finding remote functions.

;;; Code:
(defun c-quickpeek-collect-data ()
  "Collect data about the current position in a C file."
  (let ((completable-word nil)
	(case-fold-search nil))
    (list 'quickpeek-functional-form
	  (save-excursion
	    (quickpeek-beginning-of-defun)
	    (if (save-excursion
		  (forward-sexp -1)
		  (looking-at "else"))
		  ;; In this case, we are in an #ifed body
		  (c-up-conditional 1))
	    (if (re-search-backward
		 "\\(\\s-\\|^\\)\\**\\(\\w+\\)\\s-*("
		 (save-excursion (re-search-backward "(" nil t)
				 (beginning-of-line)
				 (point))
		 t)
		(let ((name (match-string 2)))
		  (list (cons "Function: " 'bold) (cons name 'leave-faces)))
	      (cons "None" 'bold)))
	  (save-excursion
	    (condition-case nil
		(end-of-thing 'sexp)
	      (error (forward-word 1)))
	    (let ((current-sym (current-word)))
	      ;; Start with the variable under the point (if applicable.)
	      (cond
	       ((not current-sym) "")
	       ((looking-at "\\s-*(")
		(c-quickpeek-context-for-function current-sym))
	       (t
		;; Well, it's not a function, so try some other things.
		(condition-case nil
		    (progn
		      (forward-char -1)
		      (quickpeek-thing-beginning)
		      (cond
		       ;; Start looking for macros which sometimes look
		       ;; like regular symbols.
		       ((re-search-backward "#\\s-*"
					    (save-excursion (beginning-of-line)
							    (point))
					    t)
			(c-quickpeek-macro))
		       (;; Is it a number, string, or in a comment?
			(or (string-match "^[0-9]+$" current-sym)
			    (quickpeek-in-non-code))
			(and (skip-chars-backward "->.")
			     (looking-at "\\(->\\|\\.\\)\\(\\w+\\)\\>"))
			;; Open a structure or union?
			(c-quickpeek-context-for-structure
			 (quickpeek-thing)))
		       (t
			;; Not a structure?  How about a variable?
			(setq completable-word current-sym)
			(c-quickpeek-context-for-variable
			 current-sym))))
		  (error ""))))))
	  (if (and completable-word (> (length completable-word) 2))
	      (quickpeek-tags-completion completable-word)))))

(defvar c-quickpeek-symbol-constants
  '(("return" . (("return " . font-lock-keyword-face)
		 "<return value of function>;"))
    ("continue" . (("continue" . font-lock-keyword-face)
		   "; - continue the current loop."))
    ("break" . (("break " . font-lock-keyword-face)
		"- exit the current loop."))
    ("case" . (("case " . font-lock-keyword-face)
	       ("<CONSTANT>" . font-lock-string-face)
		": - element of a switch statement."))
    ("default" . (("default" . font-lock-keyword-face)
		  ": - The default tag in a switch statement."))
    ("const" . (("const <type> " . font-lock-type-face)
		("<variable>" . font-lock-variable-name-face)
		"; - Declare a variable as a constant."))
    ("static" . (("static <type> " . font-lock-type-face)
		 "- Declare an object as having local persistant meaning."))
    ("struct" . (("struct <name> " . font-lock-type-face)
		 ("{ <vardef>; }; - Define a structure of data.")))
    ("union" . (("union <name> " . font-lock-type-face)
		 ("{ <vardef>; }; - Define a union of definitions")))
    ;; some structural things
    ("else" . (("if" . font-lock-keyword-face)
	       "(condition) { <then clause> } "
	       ("else" . font-lock-keyword-face)
	       " { <else clause> } "))
    ("do" . (("do " . font-lock-keyword-face)
	     "{ <repeat> } "
	     ("while" . font-lock-keyword-face)
	     "(condition);"))
    ;; A couple silly things
    ("False" . (("False " . font-lock-constant-face) "- Not true."))
    ("True" . (("True " . font-lock-constant-face) "- Not false"))
    ;; How about a bunch of types?  Just a few for now
    ("void" . (("void " . font-lock-type-face)
	       "- Unknown (opaque) data type."))
    ("char" . (("char " . font-lock-type-face)
	       "- Character (8 bit) data type."))
    ("int" . (("int " . font-lock-type-face) "- Integer data type."))
    ("float" . (("float " . font-lock-type-face) "- Floating point data type."))
    ("double" . (("double " . font-lock-type-face) "- Double precision data type."))
    )
  "List of words that might appear to be variables that have special meaning.")

(defun c-quickpeek-variable-type (variable)
  "Fetch the type of VARIABLE."
  ;; Use condition case to make this a little more robust.
  (let (sa)
    (condition-case nil
	(cond ((re-search-backward
		(concat "\\(^\\s-*\\|^\\s-*static\\s-+\\|^\\s-*const\\s-+\
\\|[(,]\\s-*\\)\\(struct\\s-+\\|union\\s-+\\|enum\\s-+\\)"
			"\\(\\w+\\)"
			"\\(\\s-*\\*+\\s-*\\|\\s-+\\)"
			(regexp-quote variable) "\\s-*[[,;=]")
		nil t)
	       (list (cons (concat (match-string 2) " " (match-string 3)
				   (match-string 4))
			   font-lock-type-face)))
	      ((and (save-excursion
		      (re-search-backward
		       (concat
			"\\(^\\s-*\\|^\\s-*static\\s-+\\|^\\s-*const\\s-+"
			"\\|[(,]\\s-*\\)"
			"\\(\\s-*\\|unsigned\\s-+\\|short\\s-+\\|signed\\s-+\\)"
			"\\(\\w+\\)"
			"\\(\\s-*\\*+\\s-*\\|\\s-+\\)"
			(regexp-quote variable) "\\s-*[[,;=)]")
		       nil t))
		    (not (save-match-data
			   (string-match "\\(struct\\|union\\|enum\\)"
					 (match-string 3)))))
		 (list (cons (concat (match-string 3) (match-string 4))
			     'font-lock-type-face)))
	      (;; Well, it might be in the middle of a list
	       ;; of variables after a type.  Unfortunatly, this
	       ;; looks just like a parameter list as well.
	       (and
		(re-search-backward
		 (concat ",\\s-*\\**" (regexp-quote variable) "\\s-*[[,;=]")
		 nil t)
		(progn
		  (while (and (progn
				(beginning-of-line)
				(looking-at "\\s-*for\\s-*("))
			      (re-search-backward
			       (concat ",\\s-*\\**" (regexp-quote variable)
				       "\\s-*[[,;=]") nil t)))
		  (or (looking-at
		       "\\s-*\\(\\(struct\\|union\\|enum\\)\\s-+\\w+\\)\
\\s-*\\w+\\s-*[[,=]")
		      (looking-at
		       "\\s-*\\(\\w+\\)\\s-*\\w+\\s-*[[,=]"))))
	       (list (cons (concat (match-string 1) " ")
			   'font-lock-type-face)))
	      ;; How about a macro?
	      ((re-search-backward (concat "#\\s-*define\\s-+"
					   (regexp-quote variable)))
	       "#define ")
	      (t
	       nil))
      (error nil))))

(defun c-quickpeek-context-for-variable (symbol)
  "Convert SYMBOL into a description based on a variable."
  (let ((sa (assoc symbol c-quickpeek-symbol-constants)))
    (if sa (cdr sa)
      (let ((type (c-quickpeek-variable-type symbol)))
	(if type
	    (list (cons "Variable: " 'bold) type " "
		  (cons symbol font-lock-variable-name-face))
	  ;; Try to lookup this symbol
	  (let ((taginfo (condition-case nil
			     (quickpeek-find-tag-stealthy symbol)
			   (error nil))))
	    (if taginfo
		(save-excursion
		  (set-buffer (car taginfo))
		  (goto-char (cdr taginfo))
		  (beginning-of-line)
		  (cond ((looking-at (concat "#\\s-*define\\s-+"
					     (regexp-quote symbol)))
			 (list '("Macro: " . bold)
			       (buffer-substring (point)
						 (save-excursion (end-of-line)
								 (point))))
			 )
			((looking-at (concat "\\s-*\\(struct\\|union\\|enum\\)\\s-*"
					     (regexp-quote symbol)
					     "\\s-*{"))
			 (list '("Type: " . bold)
			       (cons (concat (match-string 1) " ")
				     font-lock-type-face)
			       (cons (concat symbol " ")
				     font-lock-variable-name-face)
			       "in "
			       (cons (buffer-name) font-lock-comment-face)))
			((looking-at
			  (concat "\\s-*typedef\\s-*\\(\\(\\(struct\\|union\\|enum\\)\
\\s-+\\)\\w+\\s-\\)\\s-*"
				  (regexp-quote symbol) "\\s-*;"))
			 (list '("Type: " . bold)
			       '("typedef " . font-lock-type-face)
			       (cons (match-string 1) 'leave-faces)
			       (cons symbol font-lock-type-face)))
			((or
			  (looking-at
			   "\\s-*\\(enum\\s-\\)\\s-*\\(\\w+\\s-\\)\\s-*{")
			  (save-excursion
			    (up-list -1)
			    (beginning-of-line)
			    (looking-at
			     "\\s-*\\(enum\\s-\\)\\s-*\\(\\w+\\s-\\)\\s-*{")))
			 (list '("Enum Element: " . bold)
			       (cons "enum " font-lock-type-face)
			       (cons (match-string 2)
				     font-lock-variable-name-face)
			       "{ ... "
			       (cons (concat symbol " ")
				     font-lock-constant-face)
			       " ... }"))
			 ;; An element of an enum is here.
			(t
			 (list (cons "Unknown Symbol: " 'bold) symbol))))
	      (list (cons "Unknown Symbol: " 'bold) symbol))))))))

(defun c-quickpeek-context-for-structure (symbol)
  "Convert SYMBOL into a description based on it's structure."
  ;; for now, nothing special
  (c-quickpeek-context-for-variable symbol))

(defvar c-quickpeek-function-alist
  '(("if" . (("if" . font-lock-keyword-face)
	      "(condition) { <then clause> } [ "
	      ("else" . font-lock-keyword-face)
	      " { <else clause> } ]"))
    ("switch" . (("switch" . font-lock-keyword-face)
		 "(condition) { "
		 ("case " . font-lock-keyword-face)
		 ("<constant>" . font-lock-string-face)
		 ": <case clause> "
		 ("break" . font-lock-keyword-face)
		 "; "
		 ("default" . font-lock-keyword-face)
		 ": }"))
    ("while" . (("do " . font-lock-keyword-face)
		"{ <repeat> } "
		("while" . font-lock-keyword-face)
		"(condition); OR "
		("while" . font-lock-keyword-face)
		"(condition) { <repeat> }"))
    ("for" . (("for" . font-lock-function-name-face)
	      "(<init>; <condition>; <repeat>) { <for body> }"))
    ("sizeof" . (("int " . font-lock-type-face)
		 ("sizeof" . font-lock-function-name-face)
		 "("
		 ("type" . font-lock-type-face)
		 ") - Size of TYPE in bytes"))
   )
"Alist of common functions and their context display.")

(defun c-quickpeek-context-for-function (symbol)
  "Convert SYMBOL into some sort of context relavant string.  (80 char limit)."
  (cond ((eq symbol nil) nil)
	((let ((a (assoc symbol c-quickpeek-function-alist)))
	   (cdr a)))
	((re-search-backward
	  (concat "\\<\\(\\w+\\(\\s-*\\*[ \t\n]*\\|[ \t\n]+\\)\\)" ; type
		  (regexp-quote symbol) ; our function
		  "\\s-*(") nil t)
	 (let ((ret (match-string 1))
	       (params (save-excursion
			 (goto-char (match-end 0))
			 (forward-char -1)
			 (buffer-substring (point)
					   (save-excursion
					     (forward-sexp 1)
					     (point))))))
	   (while (string-match "\n" params)
	     (aset params (match-beginning 0) ? ))
	   (list (if (string= ret "define ")
		     '("Macro: " . bold)
		   (cons ret font-lock-type-face))
		 (cons symbol font-lock-function-name-face)
		 (cons params 'leave-faces))))
	(t
	 ;; Try to lookup this symbol
	 (let ((taginfo (condition-case nil
			    (quickpeek-find-tag-stealthy symbol)
			  (error nil))))
	   (or
	    (if taginfo
		(save-excursion
		  (set-buffer (car taginfo))
		  (goto-char (cdr taginfo))
		  (beginning-of-line)
		  (quickpeek-with-alternate-syntax-table
		    ;; This makes sure it isn't a split definition.
		    (if (looking-at "^\\w+\\s-*(")
			(forward-line -1))
		    ;; Find the symbol
		    (if (re-search-forward
			 (concat
			  "\\<\\(\\w+\\(\\s-*\\*[ \t\n]*\\|[ \t\n]+\\)\\)"
			  (regexp-quote symbol) ; our function
			  "\\s-*(")
			 (save-excursion (forward-line 2)
					 (end-of-line) (point)) t)
			(let ((ret (match-string 1))
			      (params (save-excursion
					(goto-char (match-end 0))
					(forward-char -1)
					(buffer-substring
					 (point)
					 (save-excursion
					   (forward-sexp 1)
					   (point))))))
			  (while (string-match "\n" params)
			    (aset params (match-beginning 0) ? ))
			  (list
			   (if (string= ret "define ")
			       '("Macro: " . bold)
			     (cons ret font-lock-type-face))
			   (cons symbol font-lock-function-name-face)
			   (cons params 'leave-faces)))
		      ;; Backup plans???
		      nil))))
	    (list (cons "Unknown function: " 'bold)
		  symbol))))))

(defconst c-quickpeek-macro-matches
  '(("define" .
     (("#define " . bold) ("<var> " . italic) "[value]"))
    ("if" .
     (("#if " . bold) ("<cond> " . italic) " code " ("#endif". bold)))
    ("else" .
     (("#if " . bold) ("<cond> " . italic) "code "
      ("#else ". bold) "code " ("#endif". bold)))
    ("elseif" .
     (("#if " . bold) ("<cond> " . italic) "code "
      ("#elseif". bold) ("<cond> " . italic) "code "
      ("#endif". bold)))
    ("endif" .
     (("#if " . bold) ("<cond> " . italic) "code " ("#endif". bold)))
    ("pragma" .
     (("#pragma " . bold) "<compiler key word>"))
    ("include" .
     (("#include " . bold) "\"file.h\" -OR- <file.h>"))
    )
  "List of pre-processor macros we like to match against.")

(defun c-quickpeek-macro ()
  "Return details about the macro under point."
  (looking-at "#\\s-*\\(\\w+\\)")
  (let* ((str (match-string 1))
	 (match (assoc str c-quickpeek-macro-matches)))
    (if match
	(append '(("Macro: " . bold)) (cdr match))
      (list '("Unknown Macro: " . bold) str))))

(add-hook 'c-mode-hook
	  (lambda ()
	    (setq quickpeek-info-function 'c-quickpeek-collect-data)))

(provide 'qp-c)

;;; qp-c.el ends here
