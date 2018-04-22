;;; js2-old-indent.el --- Indentation code kept for compatibility

;; Copyright (C) 2015  Free Software Foundation, Inc.

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

;; All features of this indentation code have been ported to Emacs's
;; built-in `js-mode' by now, so we derive from it.  An older
;; commentary follows.

;; This code is kept for Emacs 24.5 and ealier.

;; This indenter is based on Karl Landström's "javascript.el" indenter.
;; Karl cleverly deduces that the desired indentation level is often a
;; function of paren/bracket/brace nesting depth, which can be determined
;; quickly via the built-in `parse-partial-sexp' function.  His indenter
;; then does some equally clever checks to see if we're in the context of a
;; substatement of a possibly braceless statement keyword such as if, while,
;; or finally.  This approach yields pretty good results.

;; The indenter is often "wrong", however, and needs to be overridden.
;; The right long-term solution is probably to emulate (or integrate
;; with) cc-engine, but it's a nontrivial amount of coding.  Even when a
;; parse tree from `js2-parse' is present, which is not true at the
;; moment the user is typing, computing indentation is still thousands
;; of lines of code to handle every possible syntactic edge case.

;; In the meantime, the compromise solution is that we offer a "bounce
;; indenter", configured with `js2-bounce-indent-p', which cycles the
;; current line indent among various likely guess points.  This approach
;; is far from perfect, but should at least make it slightly easier to
;; move the line towards its desired indentation when manually
;; overriding Karl's heuristic nesting guesser.

;; I've made miscellaneous tweaks to Karl's code to handle some Ecma
;; extensions such as `let' and Array comprehensions.  Major kudos to
;; Karl for coming up with the initial approach, which packs a lot of
;; punch for so little code. -- Steve

;;; Code:

(require 'sgml-mode)

(defvar js2-language-version)

(declare-function js2-backward-sws "js2-mode")
(declare-function js2-forward-sws "js2-mode")
(declare-function js2-same-line "js2-mode")

(defcustom js2-basic-offset (if (and (boundp 'c-basic-offset)
                                     (numberp c-basic-offset))
                                c-basic-offset
                              4)
  "Number of spaces to indent nested statements.
Similar to `c-basic-offset'."
  :group 'js2-mode
  :safe 'integerp
  :type 'integer)

(defcustom js2-pretty-multiline-declarations t
  "Non-nil to line up multiline declarations vertically:

  var a = 10,
      b = 20,
      c = 30;

If the value is t, and the first assigned value in the
declaration is a function/array/object literal spanning several
lines, it won't be indented additionally:

  var o = {                   var bar = 2,
    foo: 3          vs.           o = {
  },                                foo: 3
      bar = 2;                    };

If the value is `all', it will always be indented additionally:

  var o = {
        foo: 3
      };

  var o = {
        foo: 3
      },
      bar = 2;

If the value is `dynamic', it will be indented additionally only
if the declaration contains more than one variable:

  var o = {
    foo: 3
  };

  var o = {
        foo: 3
      },
      bar = 2;"
  :group 'js2-mode
  :safe  'symbolp
  :type 'symbol)

(defcustom js2-indent-switch-body nil
  "When nil, case labels are indented on the same level as the
containing switch statement.  Otherwise, all lines inside
switch statement body are indented one additional level."
  :type 'boolean
  :safe 'booleanp
  :group 'js2-mode)

(defconst js2-possibly-braceless-keywords-re
  (concat "else[ \t]+if\\|for[ \t]+each\\|"
          (regexp-opt '("catch" "do" "else" "finally" "for" "if"
                        "try" "while" "with" "let")))
  "Regular expression matching keywords that are optionally
followed by an opening brace.")

(defconst js2-indent-operator-re
  (concat "[-+*/%<>&^|?:.]\\([^-+*/.]\\|$\\)\\|!?=\\|"
          (regexp-opt '("in" "instanceof") 'symbols))
  "Regular expression matching operators that affect indentation
of continued expressions.")

(defconst js2-declaration-keyword-re
  (regexp-opt '("var" "let" "const") 'symbols)
  "Regular expression matching variable declaration keywords.")

(defun js2-re-search-forward-inner (regexp &optional bound count)
  "Auxiliary function for `js2-re-search-forward'."
  (let (parse saved-point)
    (while (> count 0)
      (re-search-forward regexp bound)
      (setq parse (if saved-point
                      (parse-partial-sexp saved-point (point))
                    (syntax-ppss (point))))
      (cond ((nth 3 parse)
             (re-search-forward
              (concat "\\(\\=\\|[^\\]\\|^\\)" (string (nth 3 parse)))
              (save-excursion (end-of-line) (point)) t))
            ((nth 7 parse)
             (forward-line))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?\/) (eq (char-after) ?\*)))
             (re-search-forward "\\*/"))
            (t
             (setq count (1- count))))
      (setq saved-point (point))))
  (point))

(defun js2-re-search-forward (regexp &optional bound noerror count)
  "Search forward but ignore strings and comments.
Invokes `re-search-forward' but treats the buffer as if strings
and comments have been removed."
  (let ((saved-point (point)))
    (condition-case err
        (cond ((null count)
               (js2-re-search-forward-inner regexp bound 1))
              ((< count 0)
               (js2-re-search-backward-inner regexp bound (- count)))
              ((> count 0)
               (js2-re-search-forward-inner regexp bound count)))
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))

(defun js2-re-search-backward-inner (regexp &optional bound count)
  "Auxiliary function for `js2-re-search-backward'."
  (let (parse)
    (while (> count 0)
      (re-search-backward regexp bound)
      (setq parse (syntax-ppss (point)))
      (cond ((nth 3 parse)
             (re-search-backward
              (concat "\\([^\\]\\|^\\)" (string (nth 3 parse)))
              (line-beginning-position) t))
            ((nth 7 parse)
             (goto-char (nth 8 parse)))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?/) (eq (char-after) ?*)))
             (re-search-backward "/\\*"))
            (t
             (setq count (1- count))))))
  (point))

(defun js2-re-search-backward (regexp &optional bound noerror count)
  "Search backward but ignore strings and comments.
Invokes `re-search-backward' but treats the buffer as if strings
and comments have been removed."
  (let ((saved-point (point)))
    (condition-case err
        (cond ((null count)
               (js2-re-search-backward-inner regexp bound 1))
              ((< count 0)
               (js2-re-search-forward-inner regexp bound (- count)))
              ((> count 0)
               (js2-re-search-backward-inner regexp bound count)))
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))

(defun js2-looking-at-operator-p ()
  "Return non-nil if text after point is a non-comma operator."
  (defvar js2-mode-identifier-re)
  (and (looking-at js2-indent-operator-re)
       (or (not (eq (char-after) ?:))
           (save-excursion
             (and (js2-re-search-backward "[?:{]\\|\\_<case\\_>" nil t)
                  (eq (char-after) ??))))
       (not (and
             (eq (char-after) ?/)
             (save-excursion
               (eq (nth 3 (syntax-ppss)) ?/))))
       (not (and
             (eq (char-after) ?*)
             ;; Generator method (possibly using computed property).
             (looking-at (concat "\\* *\\(?:\\[\\|"
                                 js2-mode-identifier-re
                                 " *(\\)"))
             (save-excursion
               (js2-backward-sws)
               ;; We might misindent some expressions that would
               ;; return NaN anyway.  Shouldn't be a problem.
               (memq (char-before) '(?, ?} ?{)))))))

(defun js2-continued-expression-p ()
  "Return non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (if (js2-looking-at-operator-p)
        (or (not (memq (char-after) '(?- ?+)))
            (progn
              (forward-comment (- (point)))
              (not (memq (char-before) '(?, ?\[ ?\()))))
      (forward-comment (- (point)))
      (or (bobp) (backward-char))
      (when (js2-looking-at-operator-p)
        (backward-char)
        (not (looking-at "\\*\\|\\+\\+\\|--\\|/[/*]"))))))

(defun js2-end-of-do-while-loop-p ()
  "Return non-nil if word after point is `while' of a do-while
statement, else returns nil. A braceless do-while statement
spanning several lines requires that the start of the loop is
indented to the same column as the current line."
  (interactive)
  (save-excursion
    (when (looking-at "\\s-*\\_<while\\_>")
      (if (save-excursion
            (skip-chars-backward "[ \t\n]*}")
            (looking-at "[ \t\n]*}"))
          (save-excursion
            (backward-list) (backward-word 1) (looking-at "\\_<do\\_>"))
        (js2-re-search-backward "\\_<do\\_>" (point-at-bol) t)
        (or (looking-at "\\_<do\\_>")
            (let ((saved-indent (current-indentation)))
              (while (and (js2-re-search-backward "^[ \t]*\\_<" nil t)
                          (/= (current-indentation) saved-indent)))
              (and (looking-at "[ \t]*\\_<do\\_>")
                   (not (js2-re-search-forward
                         "\\_<while\\_>" (point-at-eol) t))
                   (= (current-indentation) saved-indent))))))))

(defun js2-multiline-decl-indentation ()
  "Return the declaration indentation column if the current line belongs
to a multiline declaration statement.  See `js2-pretty-multiline-declarations'."
  (let (forward-sexp-function ; use Lisp version
        at-opening-bracket)
    (save-excursion
      (back-to-indentation)
      (when (not (looking-at js2-declaration-keyword-re))
        (when (looking-at js2-indent-operator-re)
          (goto-char (match-end 0))) ; continued expressions are ok
        (while (and (not at-opening-bracket)
                    (not (bobp))
                    (let ((pos (point)))
                      (save-excursion
                        (js2-backward-sws)
                        (or (eq (char-before) ?,)
                            (and (not (eq (char-before) ?\;))
                                 (prog2 (skip-syntax-backward ".")
                                     (looking-at js2-indent-operator-re)
                                   (js2-backward-sws))
                                 (not (eq (char-before) ?\;)))
                            (js2-same-line pos)))))
          (condition-case _
              (backward-sexp)
            (scan-error (setq at-opening-bracket t))))
        (when (looking-at js2-declaration-keyword-re)
          (goto-char (match-end 0))
          (1+ (current-column)))))))

(defun js2-ctrl-statement-indentation ()
  "Return the proper indentation of current line if it is a control statement.
Returns an indentation if this line starts the body of a control
statement without braces, else returns nil."
  (let (forward-sexp-function)
    (save-excursion
      (back-to-indentation)
      (when (and (not (js2-same-line (point-min)))
                 (not (looking-at "{"))
                 (js2-re-search-backward "[[:graph:]]" nil t)
                 (not (looking-at "[{([]"))
                 (progn
                   (forward-char)
                   (when (= (char-before) ?\))
                     ;; scan-sexps sometimes throws an error
                     (ignore-errors (backward-sexp))
                     (skip-chars-backward " \t" (point-at-bol)))
                   (let ((pt (point)))
                     (back-to-indentation)
                     (when (looking-at "}[ \t]*")
                       (goto-char (match-end 0)))
                     (and (looking-at js2-possibly-braceless-keywords-re)
                          (= (match-end 0) pt)
                          (not (js2-end-of-do-while-loop-p))))))
        (+ (current-indentation) js2-basic-offset)))))

(defun js2-indent-in-array-comp (parse-status)
  "Return non-nil if we think we're in an array comprehension.
In particular, return the buffer position of the first `for' kwd."
  (let ((bracket (nth 1 parse-status))
        (end (point)))
    (when bracket
      (save-excursion
        (goto-char bracket)
        (when (looking-at "\\[")
          (forward-char 1)
          (js2-forward-sws)
          (if (looking-at "[[{]")
              (let (forward-sexp-function) ; use Lisp version
                (forward-sexp)             ; skip destructuring form
                (js2-forward-sws)
                (if (and (/= (char-after) ?,) ; regular array
                         (looking-at "for"))
                    (match-beginning 0)))
            ;; to skip arbitrary expressions we need the parser,
            ;; so we'll just guess at it.
            (if (and (> end (point)) ; not empty literal
                     (re-search-forward "[^,]]* \\(for\\) " end t)
                     ;; not inside comment or string literal
                     (let ((state (parse-partial-sexp bracket (point))))
                       (not (or (nth 3 state) (nth 4 state)))))
                (match-beginning 1))))))))

(defun js2-array-comp-indentation (parse-status for-kwd)
  (if (js2-same-line for-kwd)
      ;; first continuation line
      (save-excursion
        (goto-char (nth 1 parse-status))
        (forward-char 1)
        (skip-chars-forward " \t")
        (current-column))
    (save-excursion
      (goto-char for-kwd)
      (current-column))))

(defun js2-maybe-goto-declaration-keyword-end (bracket)
  "Helper function for `js2-proper-indentation'.
Depending on the value of `js2-pretty-multiline-declarations',
move point to the end of a variable declaration keyword so that
indentation is aligned to that column."
  (cond
   ((eq js2-pretty-multiline-declarations 'all)
    (when (looking-at js2-declaration-keyword-re)
      (goto-char (1+ (match-end 0)))))
   ((eq js2-pretty-multiline-declarations 'dynamic)
    (let (declaration-keyword-end
          at-closing-bracket-p
          comma-p)
      (when (looking-at js2-declaration-keyword-re)
        ;; Preserve the match data lest it somehow be overridden.
        (setq declaration-keyword-end (match-end 0))
        (save-excursion
          (goto-char bracket)
          (setq at-closing-bracket-p
                ;; Handle scan errors gracefully.
                (condition-case nil
                    (progn
                      ;; Use the regular `forward-sexp-function' because the
                      ;; normal one for this mode uses the AST.
                      (let (forward-sexp-function)
                        (forward-sexp))
                      t)
                  (error nil)))
          (when at-closing-bracket-p
            (js2-forward-sws)
            (setq comma-p (looking-at-p ","))))
        (when comma-p
          (goto-char (1+ declaration-keyword-end))))))))

(cl-defun js2-proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (when (nth 4 parse-status)
      (cl-return-from js2-proper-indentation (js2--comment-indent parse-status)))
    (let* ((at-closing-bracket (looking-at "[]})]"))
           (same-indent-p (or at-closing-bracket
                              (looking-at "\\_<case\\_>[^:]")
                              (and (looking-at "\\_<default:")
                                   (save-excursion
                                     (js2-backward-sws)
                                     (not (memq (char-before) '(?, ?{)))))))
           (continued-expr-p (js2-continued-expression-p))
           (declaration-indent (and js2-pretty-multiline-declarations
                                    (js2-multiline-decl-indentation)))
           (bracket (nth 1 parse-status))
           beg indent)
      (cond
       ;; indent array comprehension continuation lines specially
       ((and bracket
             (>= js2-language-version 170)
             (not (js2-same-line bracket))
             (setq beg (js2-indent-in-array-comp parse-status))
             (>= (point) (save-excursion
                           (goto-char beg)
                           (point-at-bol)))) ; at or after first loop?
        (js2-array-comp-indentation parse-status beg))

       ((js2-ctrl-statement-indentation))

       ((and declaration-indent continued-expr-p)
        (+ declaration-indent js2-basic-offset))

       (declaration-indent)

       (bracket
        (goto-char bracket)
        (cond
         ((looking-at "[({[][ \t]*\\(/[/*]\\|$\\)")
          (when (save-excursion (skip-chars-backward " \t\n)")
                                (looking-at ")"))
            (backward-list))
          (back-to-indentation)
          (js2-maybe-goto-declaration-keyword-end bracket)
          (setq indent
                (cond (same-indent-p
                       (current-column))
                      (continued-expr-p
                       (+ (current-column) (* 2 js2-basic-offset)))
                      (t
                       (+ (current-column) js2-basic-offset))))
          (if (and js2-indent-switch-body
                   (not at-closing-bracket)
                   (looking-at "\\_<switch\\_>"))
              (+ indent js2-basic-offset)
            indent))
         (t
          (unless same-indent-p
            (forward-char)
            (skip-chars-forward " \t"))
          (current-column))))

       (continued-expr-p js2-basic-offset)

       (t 0)))))

(defun js2--comment-indent (parse-status)
  "Indentation inside a multi-line block comment continuation line."
  (save-excursion
    (goto-char (nth 8 parse-status))
    (if (looking-at "/\\*")
        (+ 1 (current-column))
      0)))

(defun js2-indent-line (&optional bounce-backwards)
  "Indent the current line as JavaScript source text."
  (interactive)
  (let (parse-status offset
        ;; Don't whine about errors/warnings when we're indenting.
        ;; This has to be set before calling parse-partial-sexp below.
        (inhibit-point-motion-hooks t))
    (setq parse-status (save-excursion
                         (syntax-ppss (point-at-bol)))
          offset (- (point) (save-excursion
                              (back-to-indentation)
                              (point))))
    ;; Don't touch multiline strings.
    (unless (nth 3 parse-status)
      (indent-line-to (js2-proper-indentation parse-status))
      (when (cl-plusp offset)
        (forward-char offset)))))

;;; JSX Indentation

;; The following JSX indentation code is copied basically verbatim from js.el at
;; 958da7f, except that the prefixes on the functions/variables are changed.

(defsubst js2--jsx-find-before-tag ()
  "Find where JSX starts.

Assume JSX appears in the following instances:
- Inside parentheses, when returned or as the first argument
  to a function, and after a newline
- When assigned to variables or object properties, but only
  on a single line
- As the N+1th argument to a function

This is an optimized version of (re-search-backward \"[(,]\n\"
nil t), except set point to the end of the match.  This logic
executes up to the number of lines in the file, so it should be
really fast to reduce that impact."
  (let (pos)
    (while (and (> (point) (point-min))
                (not (progn
                       (end-of-line 0)
                       (when (or (eq (char-before) 40)   ; (
                                 (eq (char-before) 44))  ; ,
                         (setq pos (1- (point))))))))
    pos))

(defconst js2--jsx-end-tag-re
  (concat "</" sgml-name-re ">\\|/>")
  "Find the end of a JSX element.")

(defconst js2--jsx-after-tag-re "[),]"
  "Find where JSX ends.
This complements the assumption of where JSX appears from
`js--jsx-before-tag-re', which see.")

(defun js2--jsx-indented-element-p ()
  "Determine if/how the current line should be indented as JSX.

Return `first' for the first JSXElement on its own line.
Return `nth' for subsequent lines of the first JSXElement.
Return `expression' for an embedded JS expression.
Return `after' for anything after the last JSXElement.
Return nil for non-JSX lines.

Currently, JSX indentation supports the following styles:

- Single-line elements (indented like normal JS):

  var element = <div></div>;

- Multi-line elements (enclosed in parentheses):

  function () {
    return (
      <div>
        <div></div>
      </div>
    );
 }

- Function arguments:

  React.render(
    <div></div>,
    document.querySelector('.root')
  );"
  (let ((current-pos (point))
        (current-line (line-number-at-pos))
        last-pos
        before-tag-pos before-tag-line
        tag-start-pos tag-start-line
        tag-end-pos tag-end-line
        after-tag-line
        parens paren type)
    (save-excursion
      (and
       ;; Determine if we're inside a jsx element
       (progn
         (end-of-line)
         (while (and (not tag-start-pos)
                     (setq last-pos (js2--jsx-find-before-tag)))
           (while (forward-comment 1))
           (when (= (char-after) 60) ; <
             (setq before-tag-pos last-pos
                   tag-start-pos (point)))
           (goto-char last-pos))
         tag-start-pos)
       (progn
         (setq before-tag-line (line-number-at-pos before-tag-pos)
               tag-start-line (line-number-at-pos tag-start-pos))
         (and
          ;; A "before" line which also starts an element begins with js, so
          ;; indent it like js
          (> current-line before-tag-line)
          ;; Only indent the jsx lines like jsx
          (>= current-line tag-start-line)))
       (cond
        ;; Analyze bounds if there are any
        ((progn
           (while (and (not tag-end-pos)
                       (setq last-pos (re-search-forward js2--jsx-end-tag-re nil t)))
             (while (forward-comment 1))
             (when (looking-at js2--jsx-after-tag-re)
               (setq tag-end-pos last-pos)))
           tag-end-pos)
         (setq tag-end-line (line-number-at-pos tag-end-pos)
               after-tag-line (line-number-at-pos after-tag-line))
         (or (and
              ;; Ensure we're actually within the bounds of the jsx
              (<= current-line tag-end-line)
              ;; An "after" line which does not end an element begins with
              ;; js, so indent it like js
              (<= current-line after-tag-line))
             (and
              ;; Handle another case where there could be e.g. comments after
              ;; the element
              (> current-line tag-end-line)
              (< current-line after-tag-line)
              (setq type 'after))))
        ;; They may not be any bounds (yet)
        (t))
       ;; Check if we're inside an embedded multi-line js expression
       (cond
        ((not type)
         (goto-char current-pos)
         (end-of-line)
         (setq parens (nth 9 (syntax-ppss)))
         (while (and parens (not type))
           (setq paren (car parens))
           (cond
            ((and (>= paren tag-start-pos)
                  ;; Curly bracket indicates the start of an embedded expression
                  (= (char-after paren) 123) ; {
                  ;; The first line of the expression is indented like sgml
                  (> current-line (line-number-at-pos paren))
                  ;; Check if within a closing curly bracket (if any)
                  ;; (exclusive, as the closing bracket is indented like sgml)
                  (cond
                   ((progn
                      (goto-char paren)
                      (ignore-errors (let (forward-sexp-function)
                                       (forward-sexp))))
                    (< current-line (line-number-at-pos)))
                   (t)))
             ;; Indicate this guy will be indented specially
             (setq type 'expression))
            (t (setq parens (cdr parens)))))
         t)
        (t))
       (cond
        (type)
        ;; Indent the first jsx thing like js so we can indent future jsx things
        ;; like sgml relative to the first thing
        ((= current-line tag-start-line) 'first)
        ('nth))))))

(defmacro js2--as-sgml (&rest body)
  "Execute BODY as if in sgml-mode."
  `(with-syntax-table sgml-mode-syntax-table
     (let (forward-sexp-function
           parse-sexp-lookup-properties)
       ,@body)))

(defun js2--expression-in-sgml-indent-line ()
  "Indent the current line as JavaScript or SGML (whichever is farther)."
  (let* (indent-col
         (savep (point))
         ;; Don't whine about errors/warnings when we're indenting.
         ;; This has to be set before calling parse-partial-sexp below.
         (inhibit-point-motion-hooks t)
         (parse-status (save-excursion
                         (syntax-ppss (point-at-bol)))))
    ;; Don't touch multiline strings.
    (unless (nth 3 parse-status)
      (setq indent-col (save-excursion
                         (back-to-indentation)
                         (if (>= (point) savep) (setq savep nil))
                         (js2--as-sgml (sgml-calculate-indent))))
      (if (null indent-col)
          'noindent
        ;; Use whichever indentation column is greater, such that the sgml
        ;; column is effectively a minimum
        (setq indent-col (max (js2-proper-indentation parse-status)
                              (+ indent-col js2-basic-offset)))
        (if savep
            (save-excursion (indent-line-to indent-col))
          (indent-line-to indent-col))))))

(defun js2-jsx-indent-line ()
  "Indent the current line as JSX (with SGML offsets).
i.e., customize JSX element indentation with `sgml-basic-offset'
et al."
  (interactive)
  (let ((indentation-type (js2--jsx-indented-element-p)))
    (cond
     ((eq indentation-type 'expression)
      (js2--expression-in-sgml-indent-line))
     ((or (eq indentation-type 'first)
          (eq indentation-type 'after))
      ;; Don't treat this first thing as a continued expression (often a "<" or
      ;; ">" causes this misinterpretation)
      (cl-letf (((symbol-function #'js2-continued-expression-p) 'ignore))
        (js2-indent-line)))
     ((eq indentation-type 'nth)
      (js2--as-sgml (sgml-indent-line)))
     (t (js2-indent-line)))))

(provide 'js2-old-indent)

;;; js2-old-indent.el ends here
