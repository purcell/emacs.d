;;; espresso.el --- Major mode for editing JavaScript source text
;; Copyright (C) 2008 Free Software Foundation, Inc.
;; Copyright (C) 2009 Daniel Colascione <dan.colascione@gmail.com>
;; Author: Karl Landstrom <karl.landstrom@brgeight.se>
;; Author: Daniel Colascione <dan.colascione@gmail.com>
;; Maintainer: Daniel Colascione <dan.colascione@gmail.com>
;; Version: 4
;; Date: 2009-01-06
;; Keywords: languages, oop, javascript

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary

;; This is based on Karl Landstrom's barebones javascript-mode. This
;; is much more robust and works with cc-mode's comment filling
;; (mostly).
;;
;; The main features of this JavaScript mode are syntactic
;; highlighting (enabled with `font-lock-mode' or
;; `global-font-lock-mode'), automatic indentation and filling of
;; comments, and C preprocessor fontification.
;;
;; This package has (only) been tested with GNU Emacs 22 (the latest
;; stable release).
;;
;; Installation:
;;
;; Put this file in a directory where Emacs can find it (`C-h v
;; load-path' for more info). Then add the following lines to your
;; Emacs initialization file:
;;
;;    (add-to-list 'auto-mode-alist '("\\.js\\'" . espresso-mode))
;;    (autoload 'espresso-mode "espresso" nil t)
;;
;; General Remarks:
;;
;; XXX: This mode assumes that block comments are not nested inside block
;; XXX: comments and that strings do not contain line breaks.
;;
;; Exported names start with "espresso-" whereas private names start
;; with "espresso--".
;;
;; Code:

;;; Code

(require 'cc-mode)
(require 'font-lock)
(require 'newcomment)

(eval-when-compile
  (require 'cl))

;;; User customization

(defgroup espresso nil
  "Customization variables for `espresso-mode'."
  :tag "JavaScript - Espresso-Mode"
  :group 'languages)

(defcustom espresso-indent-level 4
  "Number of spaces for each indentation step."
  :type 'integer
  :group 'espresso)

(defcustom espresso-expr-indent-offset 0
  "Number of additional spaces used for indentation of continued
expressions. The value must be no less than minus
`espresso-indent-level'."
  :type 'integer
  :group 'espresso)

(defcustom espresso-auto-indent-flag t
  "Automatic indentation with punctuation characters. If non-nil, the
current line is indented when certain punctuations are inserted."
  :type 'boolean
  :group 'espresso)

;;; KeyMap

(defvar espresso-mode-map nil
  "Keymap used in Espresso mode.")

(unless espresso-mode-map
  (setq espresso-mode-map (make-sparse-keymap)))

(when espresso-auto-indent-flag
  (mapc (lambda (key)
	  (define-key espresso-mode-map key 'espresso-insert-and-indent))
	'("{" "}" "(" ")" ":" ";" ",")))

(defun espresso-insert-and-indent (key)
  "Runs the command bound to KEY in the global keymap, and if
we're not in a string or comment, indents the current line."
  (interactive (list (this-command-keys)))
  (call-interactively (lookup-key (current-global-map) key))
  (let ((syntax (save-restriction (widen) (syntax-ppss))))
    (unless (nth 8 syntax)
      (indent-according-to-mode))))

;;; Syntax table and parsing

(defvar espresso-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?$ "_" table)
    table)
  "Syntax table used in Espresso mode.")

(defconst espresso--name-start-re "[a-zA-Z_$]"
  "Matches the first character of a Espresso identifier. No grouping")

(defconst espresso--stmt-delim-chars "^;{}?:")

(defconst espresso--name-re (concat espresso--name-start-re
                                    "\\(?:\\s_\\|\\sw\\)*")
  "Matches a Javascript name. No grouping.")

(defconst espresso--dotted-name-re
  (concat espresso--name-re "\\(?:\\." espresso--name-re "\\)*")
  "Matches a dot-separated sequence of Javascript names")

(defconst espresso--cpp-name-re espresso--name-re
  "Matches a C preprocessor name")

(defconst espresso--opt-cpp-start "^\\s-*#\\s-*\\([[:alnum:]]+\\)"
  " Regexp matching the prefix of a cpp directive including the directive
name, or nil in languages without preprocessor support.  The first
submatch surrounds the directive name.")


(defconst espresso--class-decls
  `(; var NewClass = BaseClass.extend(
    ,(concat "^\\s-*\\_<var\\_>\\s-+"
             "\\(" espresso--dotted-name-re "\\)"
             "\\s-*=" "\\s-*"
             "\\(" espresso--dotted-name-re
             "\\)\\.extend\\(?:Final\\)?\\s-*(")

    ; NewClass: BaseClass.extend( ; for nested classes
    ,(concat "^\\s-*"
             "\\(" espresso--dotted-name-re "\\):"
             "\\s-*\\(" espresso--dotted-name-re
             "\\)\\.extend\\(?:Finak\\)?\\s-*("))
  "List of regular expressions that can match class definitions.
Each one must set match group 1 to the name of the class being
defined, and optionally, group 2 to the name of the base class.")

(defun espresso--regexp-opt-symbol (list)
  "Like regexp-opt, but surround the optimized regular expression
with `\\\\_<' and `\\\\_>'."
  (concat "\\_<" (regexp-opt list t) "\\_>"))

(defun espresso--re-search-forward-inner (regexp &optional bound count)
  "Auxiliary function for `espresso--re-search-forward'."
  (let ((parse)
        (orig-macro-end (save-excursion
                          (when (espresso--beginning-of-macro)
                            (c-end-of-macro)
                            (point))))
        (saved-point (point-min)))
    (while (> count 0)
      (re-search-forward regexp bound)
      (setq parse (parse-partial-sexp saved-point (point)))
      (cond ((nth 3 parse)
             (re-search-forward
              (concat "\\([^\\]\\|^\\)" (string (nth 3 parse)))
              (save-excursion (end-of-line) (point)) t))
            ((nth 7 parse)
             (forward-line))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?\/) (eq (char-after) ?\*)))
             (re-search-forward "\\*/"))
            ((and (not (and orig-macro-end
                            (<= (point) orig-macro-end)))
                  (espresso--beginning-of-macro))
             (c-end-of-macro))
            (t
             (setq count (1- count))))
      (setq saved-point (point))))
  (point))


(defun espresso--re-search-forward (regexp &optional bound noerror count)
  "Search forward but ignore strings, cpp macros, and comments.
Invokes `re-search-forward' but treats the buffer as if strings,
cpp macros, and comments have been removed.

If invoked while inside a macro, treat the contents of the macro
as normal text.

"
  (let ((saved-point (point))
        (search-expr
         (cond ((null count)
                '(espresso--re-search-forward-inner regexp bound 1))
               ((< count 0)
                '(espresso--re-search-backward-inner regexp bound (- count)))
               ((> count 0)
                '(espresso--re-search-forward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))


(defun espresso--re-search-backward-inner (regexp &optional bound count)
  "Auxiliary function for `espresso--re-search-backward'."
  (let ((parse)
        (orig-macro-start
         (save-excursion
           (and (espresso--beginning-of-macro)
                (point))))
        (saved-point (point-min)))
    (while (> count 0)
      (re-search-backward regexp bound)
      (when (and (> (point) (point-min))
                 (save-excursion (backward-char) (looking-at "/[/*]")))
        (forward-char))
      (setq parse (parse-partial-sexp saved-point (point)))
      (cond ((nth 3 parse)
             (re-search-backward
              (concat "\\([^\\]\\|^\\)" (string (nth 3 parse)))
              (save-excursion (beginning-of-line) (point)) t))
            ((nth 7 parse)
             (goto-char (nth 8 parse)))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?/) (eq (char-after) ?*)))
             (re-search-backward "/\\*"))
            ((and (not (and orig-macro-start
                            (>= (point) orig-macro-start)))
                  (espresso--beginning-of-macro)))
            (t
             (setq count (1- count))))))
  (point))


(defun espresso--re-search-backward (regexp &optional bound noerror count)
  "Search backward but ignore strings, preprocessor macros, and
comments. Invokes `re-search-backward' but treats the buffer as
if strings, preprocessor macros, and comments have been removed.

If inside a macro when called, treat the macro as normal text.
"
  (let ((saved-point (point))
        (search-expr
         (cond ((null count)
                '(espresso--re-search-backward-inner regexp bound 1))
               ((< count 0)
                '(espresso--re-search-forward-inner regexp bound (- count)))
               ((> count 0)
                '(espresso--re-search-backward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))


(defun espresso--forward-function-decl ()
  (assert (looking-at "\\_<function\\_>"))
  (forward-word)
  (forward-comment most-positive-fixnum)
  (skip-chars-forward "^(")
  (unless (eobp)
    (forward-list)
    (forward-comment most-positive-fixnum)
    (skip-chars-forward "^{"))
  t)

(defun espresso--beginning-of-defun ()
  (cond ((espresso--re-search-backward "\\_<function\\_>" (point-min) t)
         (let ((pos (point)))
           (save-excursion
             (forward-line 0)
             (when (looking-at espresso--function-heading-2-re)
               (setq pos (match-beginning 1))))
           (goto-char pos)))

        (t
         (goto-char (point-min)))))

(defun espresso--end-of-defun ()
  ;; look for function backward. if we're inside it, go to that
  ;; function's end. otherwise, search for the next function's end and
  ;; go there
  (unless (looking-at "\\_<")
    (skip-syntax-backward "w_"))

  (let ((orig-point (point)) pos)
    (when (or (looking-at "\\_<function\\_>")
              (espresso--re-search-backward "\\_<function\\_>" (point-min) t))
      (goto-char (match-beginning 0))
      (let* ((func-loc (point))
             (opening-brace-loc (progn (espresso--forward-function-decl)
                                       (point))))

        (cond ((and (<= func-loc orig-point)
                    (<= orig-point opening-brace-loc))
               (setq pos opening-brace-loc))

              ((/= 0 (nth 0 (parse-partial-sexp
                             opening-brace-loc orig-point 0)))
               (setq pos opening-brace-loc)))))

    (cond
     (pos (goto-char pos)
          (forward-list))

     ((espresso--re-search-forward "\\_<function\\_>" (point-max) t)
      (espresso--end-of-defun))

     (t (goto-char (point-max))))))

(defun espresso--beginning-of-macro (&optional lim)
  (let ((here (point)))
    (save-restriction
      (if lim (narrow-to-region lim (point-max)))
      (beginning-of-line)
      (while (eq (char-before (1- (point))) ?\\)
        (forward-line -1))
      (back-to-indentation)
      (if (and (<= (point) here)
               (looking-at espresso--opt-cpp-start))
          t
        (goto-char here)
        nil))))

(defun espresso--backward-syntactic-ws (&optional lim)
  "Simple implementation of c-backward-syntactic-ws"
  (save-restriction
    (when lim (narrow-to-region lim (point-max)))

    (let ((in-macro (save-excursion (espresso--beginning-of-macro)))
          (pos (point)))

      (while (progn (unless in-macro (espresso--beginning-of-macro))
                    (forward-comment most-negative-fixnum)
                    (/= (point)
                        (prog1
                            pos
                          (setq pos (point)))))))))

(defun espresso--forward-syntactic-ws (&optional lim)
  "Simple implementation of c-forward-syntactic-ws"
  (save-restriction
    (when lim (narrow-to-region (point-min) min))
    (let ((pos (point)))
      (while (progn
               (forward-comment most-positive-fixnum)
               (when (eq (char-after) ?#)
                 (c-end-of-macro))
               (/= (point)
                   (prog1
                       pos
                     (setq pos (point)))))))))

;;; Font Lock

(defun espresso--inside-param-list-p ()
  "Return non-nil iff point is inside a function parameter list."
  (condition-case err
      (save-excursion
	(up-list -1)
	(and (looking-at "(")
	     (progn (forward-symbol -1)
		    (or (looking-at "function")
			(progn (forward-symbol -1) (looking-at "function"))))))
    (error nil)))

(defconst espresso--function-heading-1-re
  (concat
   "^\\s-*function\\s-+\\(" espresso--name-re "\\)")
  "Regular expression matching the start of a function header. Match group 1
is the name of the function.")

(defconst espresso--function-heading-2-re
  (concat
   "^\\s-*\\(" espresso--name-re "\\)\\s-*:\\s-*function\\_>")
  "Regular expression matching the start of a function entry in
  an associative array. Match group 1 is the name of the function.")

(defconst espresso--macro-decl-re
  (concat "^\\s-*#\\s-*define\\s-+\\(" espresso--cpp-name-re "\\)\\s-*(")
  "Regular expression matching a CPP macro definition up to the opening
parenthesis. Match group 1 is the name of the function.")

(defconst espresso--keyword-re
  (espresso--regexp-opt-symbol
   '("abstract" "break" "case" "catch" "class" "const"
     "continue" "debugger" "default" "delete" "do" "else"
     "enum" "export" "extends" "final" "finally" "for"
     "function" "goto" "if" "implements" "import" "in"
     "instanceof" "interface" "native" "new" "package"
     "private" "protected" "public" "return" "static"
     "super" "switch" "synchronized" "throw"
     "throws" "transient" "try" "typeof" "var" "void"
     "volatile" "while" "with"))
  "Regular expression matching any JavaScript keyword.")

(defconst espresso--basic-type-re
  (espresso--regexp-opt-symbol
   '("boolean" "byte" "char" "double" "float" "int" "long"
     "short" "void"))
  "Regular expression matching any predefined type in JavaScript.")

(defconst espresso--constant-re
  (espresso--regexp-opt-symbol '("false" "null" "undefined"
                                 "true" "arguments" "this"))
  "Regular expression matching any future reserved words in JavaScript.")


(defconst espresso--font-lock-keywords-1
  (list
   "\\_<import\\_>"
   (list espresso--function-heading-1-re 1 font-lock-function-name-face)
   (list espresso--function-heading-2-re 1 font-lock-function-name-face))
  "Level one font lock.")

(defconst espresso--font-lock-keywords-2
  (append espresso--font-lock-keywords-1
          (list (list espresso--keyword-re 1 font-lock-keyword-face)
                (cons espresso--basic-type-re font-lock-type-face)
                (cons espresso--constant-re font-lock-constant-face)))
  "Level two font lock.")


;; Limitations with variable declarations: There seems to be no
;; sensible way to highlight variables occuring after an initialized
;; variable in a variable list. For instance, in
;;
;;    var x, y = f(a, b), z
;;
;; z will not be highlighted. Also, in variable declaration lists
;; spanning several lines only variables on the first line will be
;; highlighted. To get correct fontification, every line with variable
;; declarations must contain a `var' keyword.

(defconst espresso--font-lock-keywords-3
  `(
    ;; This goes before keywords-2 so it gets used preferentially
    ;; instead of the keywords in keywords-2. Don't use override
    ;; because that will override syntactic fontification too, which
    ;; will fontify commented-out directives as if they weren't
    ;; commented out.
    ,@cpp-font-lock-keywords ; from font-lock.el

    ,@espresso--font-lock-keywords-2

    ;; variable declarations
    ,(list
      (concat "\\_<\\(const\\|var\\)\\_>\\|" espresso--basic-type-re)
      (list (concat "\\(" espresso--name-re "\\)"
                    "\\s-*\\([=;].*\\|\\_<in\\_>.*\\|,\\|/[/*]\\|$\\)")
            nil
            nil
            '(1 font-lock-variable-name-face)))

    ;; class instantiation
    ,(list
      (concat "\\_<new\\_>\\s-+\\(" espresso--dotted-name-re "\\)")
      (list 1 'font-lock-type-face))

    ;; instanceof
    ,(list
      (concat "\\_<instanceof\\_>\\s-+\\(" espresso--dotted-name-re "\\)")
      (list 1 'font-lock-type-face))

    ;; formal parameters
    ,(list
      (concat
       "\\_<function\\_>\\(\\s-+" espresso--name-re "\\)?\\s-*(\\s-*"
       espresso--name-start-re)
      (list (concat "\\(" espresso--name-re "\\)\\(\\s-*).*\\)?")
            '(backward-char)
            '(end-of-line)
            '(1 font-lock-variable-name-face)))

    ;; continued formal parameter list
    ,(list
      (concat
       "^\\s-*" espresso--name-re "\\s-*[,)]")
      (list espresso--name-re
            '(if (save-excursion (backward-char)
                                 (espresso--inside-param-list-p))
                 (forward-symbol -1)
               (end-of-line))
            '(end-of-line)
            '(0 font-lock-variable-name-face)))

    ;; class declarations
    ,@(mapcar #'(lambda (x)
                  `(,x
                    (1 font-lock-type-face t t)
                    (2 font-lock-type-face t t)))

              espresso--class-decls))
  "Level three font lock.")


(defconst espresso--font-lock-keywords
  '(espresso--font-lock-keywords-3 espresso--font-lock-keywords-1
                                   espresso--font-lock-keywords-2
                            espresso--font-lock-keywords-3)
  "See `font-lock-keywords'.")

;; Note: Javascript cannot continue a regular expression literal
;; across lines
(defconst espresso--regexp-literal
  "[=(,]\\(?:\\s-\\|\n\\)*\\(/\\)[^/*]\\(?:.*?[^\\]\\)?\\(/\\)"
  "Match a regular expression literal. Match groups 1 and 2 are
the characters forming the beginning and end of the literal")

;; we want to match regular expressions only at the beginning of
;; expressions
(defconst espresso--font-lock-syntactic-keywords
  `((,espresso--regexp-literal (1 "|") (2 "|")))
  "Highlighting of regular expressions. See also the variable
  `font-lock-keywords'.")

;;; Indentation

(defconst espresso--possibly-braceless-keyword-re
  (espresso--regexp-opt-symbol
   '("catch" "do" "else" "finally" "for" "if" "try" "while" "with"))
  "Regular expression matching keywords that are optionally
  followed by an opening brace.")

(defconst espresso--indent-operator-re
  (concat "[-+*/%<>=&^|?:.]\\([^-+*/]\\|$\\)\\|"
          (espresso--regexp-opt-symbol '("in" "instanceof")))
  "Regular expression matching operators that affect indentation
  of continued expressions.")


(defun espresso--looking-at-operator-p ()
  "Return non-nil if text after point is an operator (that is not
a comma)."
  (save-match-data
    (and (looking-at espresso--indent-operator-re)
         (or (not (looking-at ":"))
             (save-excursion
               (and (espresso--re-search-backward "[?:{]\\|\\_<case\\_>" nil t)
                    (looking-at "?")))))))


(defun espresso--continued-expression-p ()
  "Returns non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (or (espresso--looking-at-operator-p)
        (and (espresso--re-search-backward "\n" nil t)
	     (progn
	       (skip-chars-backward " \t")
	       (or (bobp) (backward-char))
	       (and (> (point) (point-min))
                    (save-excursion (backward-char) (not (looking-at "[/*]/")))
                    (espresso--looking-at-operator-p)
		    (and (progn (backward-char)
				(not (looking-at "++\\|--\\|/[/*]"))))))))))


(defun espresso--end-of-do-while-loop-p ()
  "Returns non-nil if word after point is `while' of a do-while
statement, else returns nil. A braceless do-while statement
spanning several lines requires that the start of the loop is
indented to the same column as the current line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (looking-at "\\s-*\\_<while\\_>")
	(if (save-excursion
	      (skip-chars-backward "[ \t\n]*}")
	      (looking-at "[ \t\n]*}"))
	    (save-excursion
	      (backward-list) (forward-symbol -1) (looking-at "\\_<do\\_>"))
	  (espresso--re-search-backward "\\_<do\\_>" (point-at-bol) t)
	  (or (looking-at "\\_<do\\_>")
	      (let ((saved-indent (current-indentation)))
		(while (and (espresso--re-search-backward "^\\s-*\\_<" nil t)
			    (/= (current-indentation) saved-indent)))
		(and (looking-at "\\s-*\\_<do\\_>")
		     (not (espresso--re-search-forward
			   "\\_<while\\_>" (point-at-eol) t))
		     (= (current-indentation) saved-indent)))))))))


(defun espresso--ctrl-statement-indentation ()
  "Returns the proper indentation of the current line if it
starts the body of a control statement without braces, else
returns nil."
  (save-excursion
    (back-to-indentation)
    (when (save-excursion
            (and (not (looking-at "[{]"))
                 (progn
                   (espresso--re-search-backward "[[:graph:]]" nil t)
                   (or (eobp) (forward-char))
                   (when (= (char-before) ?\)) (backward-list))
                   (skip-syntax-backward " ")
                   (skip-syntax-backward "w_")
                   (looking-at espresso--possibly-braceless-keyword-re))
                 (not (espresso--end-of-do-while-loop-p))))
      (save-excursion
        (goto-char (match-beginning 0))
        (+ (current-indentation) espresso-indent-level)))))


(defun espresso--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (let ((ctrl-stmt-indent (espresso--ctrl-statement-indentation))
          (same-indent-p (looking-at "[]})]\\|\\_<case\\_>\\|\\_<default\\_>"))
          (continued-expr-p (espresso--continued-expression-p)))
      (cond (ctrl-stmt-indent)
            ((eq (char-after) ?#) 0)
            ((save-excursion (espresso--beginning-of-macro))
             4)
            ((nth 1 parse-status)
             (goto-char (nth 1 parse-status))
             (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                 (progn
                   (skip-syntax-backward " ")
                   (when (= (char-before) ?\)) (backward-list))
                   (back-to-indentation)
                   (cond (same-indent-p
                          (current-column))
                         (continued-expr-p
                          (+ (current-column) (* 2 espresso-indent-level)
                             espresso-expr-indent-offset))
                         (t
                          (+ (current-column) espresso-indent-level))))
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (current-column)))
	    (continued-expr-p (+ espresso-indent-level
                                 espresso-expr-indent-offset))
            (t 0)))))


(defun espresso-indent-line ()
  "Indent the current line as JavaScript source text."
  (interactive)
  (save-restriction
    (widen)
    (let* ((parse-status
            (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation))))

      (if (nth 8 parse-status)
          (indent-relative-maybe)
        (indent-line-to (espresso--proper-indentation parse-status))
        (when (> offset 0) (forward-char offset))))))

;;; Filling

(defun espresso-c-fill-paragraph (&optional justify)
  "Fill the paragraph with c-fill-paragraph"
  (interactive "*P")

  ;; FIXME: filling a single-line C-style comment into multiple lines
  ;; does something horrible to the undo list

  (flet ((c-forward-sws
          (&optional limit)
          (espresso--forward-syntactic-ws limit))

         (c-backward-sws
          (&optional limit)
          (espresso--backward-syntactic-ws limit))

         (c-beginning-of-macro
          (&optional limit)
          (espresso--beginning-of-macro limit)))

    (let ((fill-paragraph-function 'c-fill-paragraph))
      (c-fill-paragraph justify))))

;;; Imenu

(defun espresso--imenu-create-index ()
  (let ((search-re (mapconcat (lambda (x)
                                (concat "\\(" x "\\)"))
                              (list espresso--function-heading-1-re
                                    espresso--function-heading-2-re
                                    (concat "\\(?:"
                                            (mapconcat
                                             #'identity
                                             espresso--class-decls "\\|")
                                            "\\)")
                                    espresso--macro-decl-re)
                              "\\|"))
        entries parent-entries ends tmp syntax)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))

        (while (re-search-forward search-re (point-max) t)
          (goto-char (match-beginning 0))
          (setq syntax (syntax-ppss))
          (unless (or (nth 3 syntax) (nth 4 syntax))
            (while (and ends (>= (point) (car ends)))
              (setq tmp     (nreverse entries)
                    entries (pop parent-entries))

              (unless tmp
                (setq tmp (list
                           (cons "[empty]" (set-marker (make-marker)
                                                       (car ends))))))

              (pop ends)

              (setcdr (car entries) tmp))

            (cond ((and (not parent-entries) ; regular function or macro
                        (or (looking-at espresso--function-heading-1-re)
                            (looking-at espresso--macro-decl-re)))

                   (push (cons (match-string-no-properties 1)
                               (set-marker (make-marker) (match-beginning 1)))
                         entries))

                  ;; does one of the espresso--class-decls regexps match?
                  ((let ((r espresso--class-decls))
                     (while (and r (not (looking-at (car r) )))
                       (setq r (cdr r)))
                     r)

                   (push (cons
                          (match-string-no-properties 1)
                          nil)
                         entries)
                   (push entries parent-entries)
                   (setq entries nil)
                   (goto-char (match-end 1))
                   (condition-case err
                       (forward-list)
                     (error nil))
                   (push (point) ends))


                  ((and parent-entries
                        (looking-at espresso--function-heading-2-re))
                   (push (cons (match-string-no-properties 1)
                               (set-marker (make-marker) (match-beginning 1)))
                         entries))))

          (goto-char (match-end 0)))

        (while parent-entries
          (setq tmp     (nreverse entries)
                entries (pop parent-entries))
            (setcdr (car entries) tmp))))

    (nreverse entries)))

(defun espresso--which-func-joiner (parts)
  (mapconcat #'identity parts "."))

;;; Main Function

;;;###autoload
(defun espresso-mode ()
  "Major mode for editing JavaScript source text.

Key bindings:

\\{espresso-mode-map}"
  (interactive)
  (kill-all-local-variables)

  (use-local-map espresso-mode-map)
  (set-syntax-table espresso-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'espresso-indent-line)
  (set (make-local-variable 'beginning-of-defun-function)
       'espresso--beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'espresso--end-of-defun)

  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)

  (set (make-local-variable 'font-lock-defaults)
       (list espresso--font-lock-keywords
	     nil nil nil nil
	     '(font-lock-syntactic-keywords
               . espresso--font-lock-syntactic-keywords)))

  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'which-func-imenu-joiner-function)
       #'espresso--which-func-joiner)

  ;; Comments
  (setq comment-start "// ")
  (setq comment-end "")
  (set (make-local-variable 'fill-paragraph-function)
       'espresso-c-fill-paragraph)

  ;; Imenu
  (setq imenu-case-fold-search nil)
  (set (make-local-variable 'imenu-create-index-function)
       #'espresso--imenu-create-index)

  (setq major-mode 'espresso-mode)
  (setq mode-name "Espresso")

  ;; for filling, pretend we're cc-mode
  (setq c-comment-prefix-regexp "//+\\|\\**"
        c-paragraph-start "$"
        c-paragraph-separate "$"
        c-block-comment-prefix "* "
        c-line-comment-starter "//"
        c-comment-start-regexp "/[*/]\\|\\s!"
        comment-start-skip "\\(//+\\|/\\*+\\)\\s *")

  (let ((c-buffer-is-cc-mode t))
    (c-setup-paragraph-variables))

  ;; Important to fontify the whole buffer syntactically! If we don't,
  ;; then we might have regular expression literals that aren't marked
  ;; as strings, which will screw up parse-partial-sexp, scan-lists, etc.
  ;; and and produce maddening "unbalanced parenthesis" errors. When we attempt
  ;; to find the error and scroll to the portion of the buffer containing the problem,
  ;; JIT-lock will apply the correct syntax to the regular expresion literal and
  ;; the problem will mysteriously disappear.
  (font-lock-set-defaults)

  (let (font-lock-keywords) ; leaves syntactic keywords intact
    (font-lock-fontify-buffer))

  (run-mode-hooks 'espresso-mode-hook))


(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
                '(espresso-mode "{" "}" "/[*/]"
                                nil hs-c-like-adjust-block-beginning)))

(eval-after-load "folding"
  (when (fboundp 'folding-add-to-marks-list)
    (folding-add-to-marks-list 'espresso-mode "// {{{" "// }}}" )))


;;; Emacs
(provide 'espresso-mode)
;; Local Variables:
;; outline-regexp: ";;; "
;; End:
;; espresso.el ends here
