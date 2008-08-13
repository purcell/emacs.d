;;; wisent-ruby.el: Lexer and parser support for wisent
;;; ruby grammar. 

;;; Copyright (C) 2007 Daniel Debertin (debertin@gmail.com)

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
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

;; Depends on ruby-mode.el (not part of Emacs). You can find ruby-mode
;; in the misc/ directory of the Ruby source code (www.ruby-lang.org).

;; This is support in semantic for a minimal Ruby grammar. It allows
;; extraction of constant, variable, and class and method extraction,
;; but is as yet unable to do more subtle things like context
;; derivation and "intellisense mode". Work on these items is
;; ongoing, but feel free to contact the author if you would like
;; to contribute to wisent-ruby's development.
;; 

(require 'semantic)
(condition-case nil
    (require 'ruby-mode)
  (error (message "Can't find ruby-mode.el")))

(require 'wisent-ruby-wy)

;; Lexers for handling values expressed as literals
(define-lex-regex-analyzer semantic-lex-ruby-delimited-input
  "Handle literals expressed as delimited productions"
  "%\\([qQwWrx]?\\)\\(?:{[^\}]*}\\|\\[[^\]]*\\]\\|\([^\)]*\)\\|<[^>]*>\\|\\(.\\)[^\\2]*\\2\\)\\([imox]*\\)"
  (let ((marker (match-string 1))
	(options (match-string 3)))
    (cond ((or (string-match "[qQ]" marker)
	       (not marker))
	   (lex-token-at-pt 'STRING_LITERAL 'ruby-forward-sexp 1))
	  ((string-match "[wW]" marker)
	   (lex-token-at-pt 'ARRAY_LITERAL 'ruby-forward-sexp 1))
	  ((string-equal marker "r")
	   (lex-token-at-pt 'REGEXP_LITERAL 'ruby-forward-sexp (if options 2 1)))
	  ((string-equal marker "x")
	   (lex-token-at-pt 'SHELL_COMMAND_LITERAL 'ruby-forward-sexp 1)))
    (setq semantic-lex-end-point (point))))

;; TODO -- floats!
(define-lex-regex-analyzer semantic-lex-ruby-numbers
  "Handle number literals in various bases and formats"
  "-?\\(0b[01]+\\|0[0-7]+\\|0x[[:digit:]A-Fa-f]+\\|\\(?:0d\\)?[1-9][[:digit:]_]*\\)"
  (lex-token-at-pt 'INTEGER_LITERAL 'goto-char (match-end 0)))

(define-lex-regex-analyzer semantic-lex-ruby-strings
  "Handle string literals"
  "\\(\"[^\"]*\"\\|\'[^\']*\'\\|\`[^\`]*\`\\)"
  (if (string-equal (substring (match-string 1) 0 1) "`")
      (lex-token-at-pt 'SHELL_COMMAND_LITERAL 'ruby-forward-sexp 1)
    (lex-token-at-pt 'STRING_LITERAL 'ruby-forward-sexp 1)))

(define-lex-regex-analyzer semantic-lex-ruby-ranges
  "Handle range literals"
  "\\.\\.\\.?"
  (lex-token-at-pt 'RANGE_LITERAL 'goto-char (match-end 0)))

(define-lex-regex-analyzer semantic-lex-ruby-arrays
  "Handle literal arrays"
  "\\[\\([^,]*, *\\)*[^,]*\\]"
  (lex-token-at-pt 'ARRAY_LITERAL 'ruby-forward-sexp 1))

(define-lex-regex-analyzer semantic-lex-ruby-hashes
  "Handle literal hashes"
  "{\\(\\s*[^[:space:]]+\\s*=>\\s*[^[:space:]]+,\\)*\\s*[^[:space:]]+\\s*=>\\s*[^[:space:]]+\\s*}"
  (lex-token-at-pt 'HASH_LITERAL 'ruby-forward-sexp 1))

(define-lex-regex-analyzer semantic-lex-ruby-symbols
  "Handle literal symbols"
  ":[^[:space:]]+"
  (lex-token-at-pt 'SYMBOL_LITERAL 'ruby-forward-sexp 1))

(define-lex-regex-analyzer semantic-lex-ruby-regexps
  "Handle literal REs"
  "/[^/]+/\\([imox]*\\)"
  (lex-token-at-pt 'REGEXP_LITERAL 'ruby-forward-sexp (if (match-string 1) 2 1)))


;; Other non-literal lexers
(define-lex-regex-analyzer semantic-lex-ruby-variables
  "Lex variables of various types"
  "\\(@@\\|@\\|\\$\\|[A-Z]\\|[a-z_]\\)[A-Za-z0-9_]*"
  (let ((marker (match-string 1)))
    (cond ((string-equal marker "@")
	   (lex-token-at-pt 'VAR_INSTANCE 'ruby-forward-sexp 1))
	  ((string-equal marker "@@")
	   (lex-token-at-pt 'VAR_CLASS 'ruby-forward-sexp 1))
	  ((string-equal marker "$")
	   (lex-token-at-pt 'VAR_GLOBAL 'ruby-forward-sexp 1))
	  ((string-match "[A-Z]" (or marker ""))
	   (lex-token-at-pt 'CONSTANT 'ruby-forward-sexp 1))
	  (t
	   (lex-token-at-pt 'VAR_LOCAL 'ruby-forward-sexp 1)))))

(define-lex-regex-analyzer semantic-lex-ruby-keywords
  "ID or keyword tokens"
  "[a-z0-9_][A-Za-z0-9_]*"
  (lex-token-at-pt (or (semantic-lex-keyword-p (match-string 0))
		       'IDENTIFIER) 'ruby-forward-sexp 1))

;; Put this AFTER semantic-lex-ruby-hashes
(define-lex-block-analyzer semantic-lex-ruby-punctuated-blocks
  "Lex delimited blocks"
  (PAREN_BLOCK ("(" LPAREN) (")" RPAREN))
  (BRACE_BLOCK ("{" LBRACE) ("}" RBRACE)))

;; This gargantuan function is used to parse stmts that encompass a
;; RUBY_BLOCK -- a block of code delimited by (something) ... END.  A
;; lot of constructions do this, but never in a way that could be
;; generalized. So we have to "hand-lex" each one individually. It's
;; messy, but necessary.
(define-lex-regex-analyzer semantic-lex-ruby-blocks
  "Lex blocks"
  ruby-block-beg-re
  (incf semantic-lex-current-depth)
  (if (>= semantic-lex-current-depth semantic-lex-maximum-depth)
      (let ((matched-token (match-string 0))
	    (block-start (point))
	    (block-end (save-excursion
			 (ruby-end-of-block)
			 (forward-word 1)
			 (point))))
	(save-excursion
	  (cond ((string-match "\\(class\\|module\\)" matched-token)
		 (progn
		   (lex-token-at-pt (word-at-point) 'forward-word 1)
		   (incf semantic-lex-current-depth)
		   (forward-char 1)
		   (if (string-match "[A-Z][A-Za-z0-9_]*" (word-at-point))
		       (lex-token-at-pt 'CONSTANT 'ruby-forward-sexp 1))
		   (if (and (string-equal matched-token "class")
			    (looking-at " *<"))
		       (progn
			 (lex-token-at-pt 'LT '(lambda () (forward-sexp 1) (backward-sexp 1)))
			 (if (looking-at "[A-Z][A-Za-z0-9_]*")
			     (lex-token-at-pt 'CONSTANT 'ruby-forward-sexp 1))))
		   (lex-token-at-pt 'RUBY_BLOCK '(lambda () (goto-char block-start) (ruby-end-of-block)))))
		((string-equal "def" matched-token)
		 (progn
		   (lex-token-at-pt (word-at-point) 'forward-word 1)
		   (forward-char 1)
		   (lex-token-at-pt 'IDENTIFIER 'ruby-forward-sexp 1)
		   (if (looking-at " *(")
		       (lex-token-at-pt 'PAREN_BLOCK 'forward-sexp 1))
		   (lex-token-at-pt 'RUBY_BLOCK 'ruby-end-of-block)))
		((string-equal "begin" matched-token)
		 (progn
		   (incf semantic-lex-current-depth)
		   (lex-token-at-pt 'BEGIN 'forward-sexp 1)
		   (let ((start (point)))
		     (save-restriction
		       (narrow-to-region (point) block-end)
		       (while (re-search-forward "\\(rescue\\|ensure\\|end\\)" nil t)
			 (backward-word 1)
			 (semantic-lex-push-token
			  (semantic-lex-token 'RUBY_BLOCK start (point)))
			 (lex-token-at-pt (word-at-point) 'forward-word 1)
			 (if (looking-at " *[A-Z][A-Za-z0-9_]*")
			     (lex-token-at-pt 'CONSTANT 'forward-word 1))
			 (if (looking-at " *=> *")
			     (progn
			       (lex-token-at-pt 'HASH_REF '(lambda () (forward-sexp 1) (backward-sexp 1)))
			       (lex-token-at-pt 'IDENTIFIER 'forward-sexp 1)))
			 (setq start (point)))
		       (ruby-end-of-block)))))
		 ((string-match "\\(if\\|unless\\|while\\|until\\)" matched-token)
		 (progn
		   (semantic-lex-unterminated-syntax-protection 'RUBY_BLOCK
		   (lex-token-at-pt (word-at-point) 'forward-sexp 1)
		   (if (looking-at " *(")
		       (lex-token-at-pt 'PAREN_BLOCK 'forward-sexp 1))
		     (lex-token-at-pt 'RUBY_BLOCK 'ruby-end-of-block))))
		((string-equal "for" matched-token)  ; who uses 'for', anyway?
		 (progn
		   (lex-token-at-pt (word-at-point) 'forward-sexp 1)
		   (lex-token-at-pt 'RUBY_BLOCK 'ruby-end-of-block)))
		((string-equal "case" matched-token)
		 (let ((initial t)
		       (start (point)))
		   (lex-token-at-pt (word-at-point) 'forward-word 1) ;; case
		   (forward-char 1)
		   (lex-token-at-pt 'IDENTIFIER 'forward-word 1) ;; thingy after case
		   (while (re-search-forward "\\(when\\|else\\|end\\)" nil t)
		     (backward-word 1)
		     (if (not initial)
			 (semantic-lex-push-token
			  (semantic-lex-token 'RUBY_BLOCK start (1- (point)))) ;; preceding block
		       (setq initial nil))
		     (lex-token-at-pt (word-at-point) 'forward-word 1) ;; when
		     (if (not (string-equal (word-at-point) "end"))
			 (progn
			   (forward-char 1)
			   (if (string-equal (match-string-no-properties 0) "when")
			       (lex-token-at-pt 'IDENTIFIER 'forward-word 1)))) ;; thingy after when
		     (setq start (point)))))
		((string-equal "do" matched-token)
		 (progn
		   (lex-token-at-pt (word-at-point) 'forward-sexp 1)
		   (lex-token-at-pt 'RUBY_BLOCK 'ruby-end-of-block))))
	  (setq semantic-lex-end-point (point))))
    (lex-token-at-pt (word-at-point) 'forward-sexp 1)
    (setq semantic-lex-end-point (point))))
    
(define-lex-regex-analyzer semantic-lex-ruby-newline
  "Detect newlines"
  "\\s-*\n+\\s-*"
  (goto-char (match-end 0))
  (semantic-lex-push-token
   (semantic-lex-token 'NEWLINE (1- (point)) (point))))
		 
;; Lexer support functions
(defun lex-token-at-pt (type forwardfn &rest fw-args)
  "Eat and lex the token at point, leave point at the end of the token"
  (let ((start (point))
	(kw (or (and (symbolp type) type) (semantic-lex-keyword-p type))))
    (apply forwardfn fw-args)
    (semantic-lex-push-token
     (semantic-lex-token kw start (point)))))

(define-lex wisent-ruby-lexer
  "Lexical analyzer for Ruby"
  semantic-lex-ignore-comments
  semantic-lex-ignore-newline
  semantic-lex-ignore-whitespace
  semantic-lex-ruby-delimited-input
  semantic-lex-ruby-numbers
  semantic-lex-ruby-blocks
  semantic-lex-ruby-keywords
  semantic-lex-ruby-variables
  semantic-lex-ruby-strings
  semantic-lex-ruby-ranges
  semantic-lex-ruby-arrays
  semantic-lex-ruby-hashes
  semantic-lex-ruby-punctuated-blocks
  semantic-lex-ruby-symbols
  semantic-lex-ruby-regexps
  semantic-lex-punctuation-type
  semantic-lex-default-action)

(defvar semantic-lex-ruby-assigned-vars nil
  "List of variables assigned by ruby.")
(make-variable-buffer-local 'semantic-lex-ruby-assigned-vars)

(defun wisent-ruby-default-setup ()
  (wisent-ruby-wy--install-parser)
  (setq case-fold-search nil
	semantic-lex-analyzer 'wisent-ruby-lexer
	semantic-lex-ruby-assigned-vars ()
	semantic-tag-expand-function 'wisent-ruby-expand-tags))
;;	semantic-dependency-include-path wisent-ruby-include-path
;;	wisent-ruby-current-protection 'public))

(defun process-assignment-variables (vars)
  (dolist (v vars)
    (if (string-match "^[a-z_]" v)
	(pushnew v semantic-lex-ruby-assigned-vars :test 'string-equal))))

(defun wisent-ruby-expand-tags (tag)
  "Expand or otherwise post-process incoming tags from the parser"
  (cond ((semantic-tag-of-class-p tag 'include)
	 (let* ((tag-name (semantic-tag-name tag))
		(dep-file (substring tag-name 1 (1- (length tag-name))))
		(dep-file-with-ext (if (not (string-match ".*\\.rb$" dep-file))
				       (concat dep-file ".rb")
				     dep-file)))
	   (list (semantic-tag-new-include dep-file-with-ext nil))))
	((semantic-tag-of-class-p tag 'attribute)
	 (let ((read-symbols (mapcar '(lambda (s) (substring s 1))
				     (semantic-tag-get-attribute tag :symbols)))
	       (write-symbols (mapcar '(lambda (s) (concat (substring s 1) "="))
				      (semantic-tag-get-attribute tag :symbols)))
	       (functions))
	   (if (or (string-equal (semantic-tag-name tag) "reader")
		   (string-equal (semantic-tag-name tag) "accessor"))
	       (dolist (fun read-symbols)
		 (push (semantic-tag-new-function fun "method" (list nil)) functions)))
	   (if (or (string-equal (semantic-tag-name tag) "writer")
		   (string-equal (semantic-tag-name tag) "accessor"))
	       (dolist (fun write-symbols)
		 (push (semantic-tag-new-function fun "method" (list "newvalue")) functions)))
	   (dolist (fun functions)
	     (semantic-tag-set-bounds fun (semantic-tag-start tag) (semantic-tag-end tag)))
	   functions))))

;; These two funcs are boilerplate until the type-tag system is nailed down.
(defun set-current-protection-context (level)
  nil)

(defun set-tag-protection (sym level)
  nil)
	   
(defun variable-type (var)
  "Do the same thing as semantic-lex-ruby-variables, but as a function, not a lexer"
  (cond ((string-match "^@@" var)
	 'class)
	((string-match "^@" var)
	 'instance)
	((string-match "^\\$" var)
	 'global)
	((string-match "^[A-Z]" var)
	 'constant)
	(t
	 'local)))

;;; wisent-ruby.el ends here