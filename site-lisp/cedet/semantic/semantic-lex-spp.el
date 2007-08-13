;;; semantic-lex-spp.el --- Semantic Lexical Pre-processor

;;; Copyright (C) 2006, 2007 Eric M. Ludlam

;; X-CVS: $Id: semantic-lex-spp.el,v 1.8 2007/06/06 01:05:05 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
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
;; The Semantic Preprocessor works with semantic-lex to provide a phase
;; during lexical analysis to do the work of a pre-processor.
;;
;; A pre-processor identifies lexical syntax mixed in with another language
;; and replaces some keyword tokens with streams of alternate tokens.
;; 
;; If you use SPP in your language, be sure to specify this in your
;; semantic language setup function:
;;
;; (add-hook 'semantic-lex-reset-hooks 'semantic-lex-spp-reset-hook nil t)
;;
;;; TODO:
;;
;; Use `semantic-push-parser-warning' for situations where there are likely
;; macros that are undefined unexpectedly, or other problem.

(require 'semantic-lex)

;;; Code:
(defvar semantic-lex-spp-macro-symbol-obarray nil
  "Table of macro keywords used by the Semantic Macro.")
(make-variable-buffer-local 'semantic-lex-spp-macro-symbol-obarray)

(defvar semantic-lex-spp-dynamic-macro-symbol-obarray nil
  "Table of macro keywords found during lexical analysis.
This table is then used by the macro during the lexical analysis
step.")
(make-variable-buffer-local 'semantic-lex-spp-dynamic-macro-symbol-obarray)

;;; MACRO TABLE UTILS
;;
(defun semantic-lex-spp-symbol-replacement (name)
  "Return an SPP replacement stream for NAME.
nil is a valid return.  Use `semantic-lex-spp-symbol-l\p' to determine
if the symbol is in the table."
  )

(defsubst semantic-lex-spp-symbol (name)
  "Return spp symbol with NAME or nil if not found."
  (and
   (stringp name)
   (or (and (arrayp semantic-lex-spp-dynamic-macro-symbol-obarray)
	    (intern-soft name semantic-lex-spp-dynamic-macro-symbol-obarray))
       (and (arrayp semantic-lex-spp-macro-symbol-obarray)
	    (intern-soft name semantic-lex-spp-macro-symbol-obarray)))))

(defsubst semantic-lex-spp-symbol-p (name)
  "Return non-nil if a keyword with NAME exists in any keyword table."
  (if (semantic-lex-spp-symbol name)
      t))

(defsubst semantic-lex-spp-dynamic-map ()
  "Return the dynamic macro map for the current buffer."
  (or semantic-lex-spp-dynamic-macro-symbol-obarray
      (setq semantic-lex-spp-dynamic-macro-symbol-obarray
	    (make-vector 13 0))))

(defsubst semantic-lex-spp-symbol-set (name value &optional obarray)
  "Set value of spp symbol with NAME to VALUE and return VALUE.
If optional OBARRAY is non-nil, then use that obarray instead of
the dynamic map."
  (if (string= value "") (setq value nil))
  (set (intern name (or obarray
			(semantic-lex-spp-dynamic-map)))
       value))

(defsubst semantic-lex-spp-symbol-remove (name &optional obarray)
  "Remove the spp symbol with NAME.
If optional OBARRAY is non-nil, then use that obarray instead of
the dynamic map."
  (unintern name (or obarray
		     (semantic-lex-spp-dynamic-map))))

(defsubst semantic-lex-spp-symbol-stream (name)
  "Return replacement stream of macro with NAME."
  (let ((spp (semantic-lex-spp-symbol name)))
    (if spp
        (symbol-value spp))))

(defun semantic-lex-make-spp-table (specs)
  "Convert spp macro list SPECS into an obarray and return it.
SPECS must be a list of (NAME . REPLACEMENT) elements, where:

NAME is the name of the spp macro symbol to define.
REPLACEMENT a string that would be substituted in for NAME."

  ;; Create the symbol hash table
  (let ((semantic-lex-spp-macro-symbol-obarray (make-vector 13 0))
        spec)
    ;; fill it with stuff
    (while specs
      (setq spec  (car specs)
            specs (cdr specs))
      (semantic-lex-spp-symbol-set
       (car spec) 
       (cdr spec)
       semantic-lex-spp-macro-symbol-obarray))
    semantic-lex-spp-macro-symbol-obarray))

(defun semantic-lex-spp-macros ()
  "Return a list of spp macros as Lisp symbols.
The value of each symbol is the replacement stream."
  (let (macros)
    (when (arrayp semantic-lex-spp-macro-symbol-obarray)
      (mapatoms
       #'(lambda (symbol)
	   (setq macros (cons symbol macros)))
       semantic-lex-spp-macro-symbol-obarray))
    (when (arrayp semantic-lex-spp-dynamic-macro-symbol-obarray)
      (mapatoms
       #'(lambda (symbol)
	   (setq macros (cons symbol macros)))
       semantic-lex-spp-dynamic-macro-symbol-obarray))
    macros))

(defun semantic-lex-spp-reset-dynamic-table ()
  "Reset the dynamic spp symbol table.
This should be done before any new parsing step."
  (setq semantic-lex-spp-dynamic-macro-symbol-obarray nil))

(defun semantic-lex-spp-reset-hook (start end)
  "Reset anything needed by SPP for parsing.
In this case, reset the dynamic macro symbol table if
START recons the entire buffer.
END is not used."
  (if (= start (point-min))
      (setq semantic-lex-spp-dynamic-macro-symbol-obarray nil)))

;;; MACRO EXPANSION PARSING
;;
(defun semantic-lex-spp-string-to-macro-stream (val beg end)
  "Convert string VAL into a macro expansion stream.
Argument VAL is the value of some macro to be converted into a stream.
BEG and END are the token bounds of the macro to be expanded
that will somehow gain a much longer token stream."
  ;; NOTE: Must write this function!!!!!

  ;; We perform a replacement.  Technically, this should
  ;; be a full lexical step over the "val" string, but take
  ;; a guess that its just a keyword or existing symbol.
  ;;
  ;; Probably a really bad idea.  See how it goes.
  (semantic-lex-push-token
   (semantic-lex-token (or (semantic-lex-keyword-p val) 'symbol)
		       beg end
		       val))
  )


;;; MACRO TABLE DEBUG
;;
(defun semantic-lex-spp-describe (&optional buffer)
  "Describe the current list of spp macros for BUFFER.
If BUFFER is not provided, use the current buffer."
  (interactive)
  (let ((syms (save-excursion
		(if buffer (set-buffer buffer))
		(semantic-lex-spp-macros)))
	(sym nil))
    (with-output-to-temp-buffer "*SPP MACROS*"
      (princ "Macro\t\tValue\n")
      (while syms
	(setq sym (car syms)
	      syms (cdr syms))
	(princ (symbol-name sym))
	(princ "\t")
	(if (< (length (symbol-name sym)) 8)
	    (princ "\t"))
	(prin1 (symbol-value sym))
	(princ "\n")
	))))


;;; Analyzers
;;
(define-lex-regex-analyzer semantic-lex-spp-replace-or-symbol-or-keyword
  "Like 'semantic-lex-symbol-or-keyword' plus preprocessor macro replacement."
  "\\(\\sw\\|\\s_\\)+"
  (let ((str (match-string 0))
	(beg (match-beginning 0))
	(end (match-end 0)))
    (if (semantic-lex-spp-symbol-p str)
	;; It is a macro.  Prepare for a replacement.
	(let* ((sym (semantic-lex-spp-symbol str))
	       (val (symbol-value sym)))
	  (if (not val)
	      (setq semantic-lex-end-point end)
	    (semantic-lex-spp-string-to-macro-stream val beg end)
	    ))
      ;; A regular keyword.
      (semantic-lex-push-token
       (semantic-lex-token (or (semantic-lex-keyword-p str) 'symbol)
			   beg end)))))

(defmacro define-lex-spp-macro-declaration-analyzer (name doc regexp tokidx
							  &rest valform)
  "Define a lexical analyzer for defining new MACROS.
NAME is the name of the analyzer.
DOC is the documentation for the analyzer.
REGEXP is a regular expression for the analyzer to match.
See `define-lex-regex-analyzer' for more on regexp.
TOKIDX is an index into REGEXP for which a new lexical token
of type `spp-macro-def' is to be created.
Optional VALFORM are forms that return the value to be saved for
this macro, or nil."
  (let ((start (make-symbol "start"))
	(end (make-symbol "end"))
	(val (make-symbol "val"))
	(startpnt (make-symbol "startpnt"))
	(endpnt (make-symbol "endpnt")))
    `(define-lex-regex-analyzer ,name
       ,doc
       ,regexp
       (let ((,start (match-beginning ,tokidx))
	     (,end (match-end ,tokidx))
	     (,startpnt semantic-lex-end-point)
	     (,val (save-match-data ,@valform))
	     (,endpnt semantic-lex-end-point))
	 (semantic-lex-spp-symbol-set
	  (buffer-substring-no-properties ,start ,end)
	  ,val)
	 (semantic-lex-push-token
	  (semantic-lex-token 'spp-macro-def
			      ,start ,end))
	 ;; Preserve setting of the end point from the calling macro.
	 (when (and (/= ,startpnt ,endpnt)
		    (/= ,endpnt semantic-lex-end-point))
	   (setq semantic-lex-end-point ,endpnt))
	 ))))

(defmacro define-lex-spp-macro-undeclaration-analyzer (name doc regexp tokidx)
  "Undefine a lexical analyzer for defining new MACROS.
NAME is the name of the analyzer.
DOC is the documentation for the analyzer.
REGEXP is a regular expression for the analyzer to match.
See `define-lex-regex-analyzer' for more on regexp.
TOKIDX is an index into REGEXP for which a new lexical token
of type `spp-macro-undef' is to be created."
  (let ((start (make-symbol "start"))
	(end (make-symbol "end")))
    `(define-lex-regex-analyzer ,name
       ,doc
       ,regexp
       (let ((,start (match-beginning ,tokidx))
	     (,end (match-end ,tokidx)))
	 (semantic-lex-spp-symbol-remove
	  (buffer-substring-no-properties ,start ,end))
	 (semantic-lex-push-token
	  (semantic-lex-token 'spp-macro-undef
			      ,start ,end))
	 ))))

(add-hook
 'edebug-setup-hook
 #'(lambda ()
     
     (def-edebug-spec define-lex-spp-macro-declaration-analyzer
       (&define name stringp stringp form def-body)
       )

     (def-edebug-spec define-lex-spp-macro-undeclaration-analyzer
       (&define name stringp stringp form def-body)
       )
     ))

  
(provide 'semantic-lex-spp)

;;; semantic-lex-spp.el ends here
