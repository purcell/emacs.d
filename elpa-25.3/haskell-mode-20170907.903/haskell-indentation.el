;;; haskell-indentation.el --- indentation module for Haskell Mode -*- lexical-binding: t -*-

;; Copyright (C) 2013  Kristof Bastiaensen, Gergely Risko

;; Author: Kristof Bastiaensen <kristof.bastiaensen@vleeuwen.org>
;; Author: Gergely Risko <errge@nilcons.com>
;; Keywords: indentation haskell
;; URL: https://github.com/haskell/haskell-mode

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Installation:
;;
;; To turn indentation on for all Haskell buffers under Haskell mode add
;; this to your configuration file:
;;
;;     (add-hook haskell-mode-hook 'haskell-indentation-mode)
;;
;; Otherwise, call `haskell-indentation-mode'.

;;; Code:

;; TODO eliminate magic number 2 where possible, use a variable

;; TODO `haskell-indentation-find-indentation' — fix it, get rid of "safe"
;; version

(require 'cl-lib)
(require 'haskell-lexeme)

;;;###autoload
(defgroup haskell-indentation nil
  "Haskell indentation."
  :link '(custom-manual "(haskell-mode)Indentation")
  :group 'haskell
  :prefix "haskell-indentation-")

(defcustom haskell-indentation-layout-offset 2
  "Extra indentation to add before expressions in a Haskell layout list."
  :type 'integer
  :group 'haskell-indentation)

(defcustom haskell-indentation-starter-offset 2
  "Extra indentation after an opening keyword (e.g. \"let\")."
  :type 'integer
  :group 'haskell-indentation)

(defcustom haskell-indentation-left-offset 2
  "Extra indentation after an indentation to the left (e.g. after \"do\")."
  :type 'integer
  :group 'haskell-indentation)

(defcustom haskell-indentation-where-pre-offset 2
  "Extra indentation before the keyword \"where\"."
  :type 'integer
  :group 'haskell-indentation)

(defcustom haskell-indentation-where-post-offset 2
  "Extra indentation after the keyword \"where\"."
  :type 'integer
  :group 'haskell-indentation)

(defcustom haskell-indentation-electric-flag nil
  "Non-nil means insertion of some characters may auto reindent the line.
If the variable `electric-indent-mode' is non-nil then this variable is
overridden."
  :type 'symbol
  :group 'haskell-indentation)
(make-variable-buffer-local 'haskell-indentation-electric-flag)

(defvar haskell-indentation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'haskell-indentation-newline-and-indent)
    (define-key map (kbd "<backtab>") #'haskell-indentation-indent-backwards)
    (define-key map (kbd ",") #'haskell-indentation-common-electric-command)
    (define-key map (kbd ";") #'haskell-indentation-common-electric-command)
    (define-key map (kbd ")") #'haskell-indentation-common-electric-command)
    (define-key map (kbd "}") #'haskell-indentation-common-electric-command)
    (define-key map (kbd "]") #'haskell-indentation-common-electric-command)
    map)
  "Keymap for `haskell-indentation-mode'.")

;;;###autoload
(define-minor-mode haskell-indentation-mode
  "Haskell indentation mode that deals with the layout rule.
It rebinds RET, DEL and BACKSPACE, so that indentations can be
set and deleted as if they were real tabs."
  :keymap haskell-indentation-mode-map
  (kill-local-variable 'indent-line-function)
  (kill-local-variable 'indent-region-function)

  (when haskell-indentation-mode
    (when (and (bound-and-true-p haskell-indent-mode)
               (fboundp 'turn-off-haskell-indent))
      (turn-off-haskell-indent))
    (setq-local indent-line-function #'haskell-indentation-indent-line)
    (setq-local indent-region-function #'haskell-indentation-indent-region)))

;;;###autoload
(defun turn-on-haskell-indentation ()
  "Turn on the haskell-indentation minor mode."
  (interactive)
  (haskell-indentation-mode t))

(make-obsolete 'turn-on-haskell-indentation
               'haskell-indentation-mode
               "2015-05-25")

(defvar haskell-literate) ; defined in haskell-mode.el

(defun haskell-indentation-bird-p ()
  "Return t if this is a literate Haskell buffer in bird style, NIL otherwise."
  (eq haskell-literate 'bird))

;;----------------------------------------------------------------------------
;; UI starts here

(defun haskell-indentation-reindent-to (col &optional move)
  "Reindent current line to COL, move the point there if MOVE is non-NIL."
  (let* ((ci (haskell-indentation-current-indentation)))
    (save-excursion
      (move-to-column ci)
      (if (<= ci col)
          (insert-before-markers (make-string (- col ci) ? ))
        (delete-char (- col ci))))
    (when move
      (move-to-column col))))

(defun haskell-indentation-indent-rigidly (start end arg)
  "Indent all lines starting in the region sideways by ARG columns.
Called from a program, takes three arguments, START, END and ARG.
You can remove all indentation from a region by giving a large
negative ARG.  Handles bird style literate Haskell too."
  (interactive "*r\np")
  (save-excursion
    (goto-char end)
    (let ((end-marker (point-marker)))
      (goto-char start)
      (or (bolp) (forward-line 0))
      (while (< (point) end-marker)
        (let ((ci (haskell-indentation-current-indentation)))
          (when (and t
                     (eq (char-after) ?>))
            (forward-char 1))
          (skip-syntax-forward "-")
          (unless (eolp)
            (haskell-indentation-reindent-to (max 0 (+ ci arg))))
          (forward-line 1)))
      (move-marker end-marker nil))))

(defun haskell-indentation-current-indentation ()
  "Column position of first non-whitespace character in current line."
  (save-excursion
    (beginning-of-line)
    (when (haskell-indentation-bird-p)
      (forward-char))
    (skip-syntax-forward "-")
    (current-column)))

(defun haskell-indentation-bird-outside-code-p ()
  "Non-NIL if we are in bird literate mode, but outside of code."
  (and (haskell-indentation-bird-p)
       (or (< (current-column) 2)
           (save-excursion
             (beginning-of-line)
             (not (eq (char-after) ?>))))))

(defun haskell-indentation-newline-and-indent ()
  "Insert newline and indent."
  (interactive "*")
  ;; On RET (or C-j), we:
  ;;   - just jump to the next line if literate haskell, but outside code
  (if (haskell-indentation-bird-outside-code-p)
      (progn
        (delete-horizontal-space)
        (newline))
    ;;  - save the current column
    (let ((ci (haskell-indentation-current-indentation)))
      ;; - jump to the next line and reindent to at the least same level
      (delete-horizontal-space)
      (newline)
      ;; calculate indentation after newline is inserted because if we
      ;; break an identifier we might create a keyword, for example
      ;; "dowhere" => "do where"
      (let ((indentations (or (haskell-indentation-find-indentations)
                              '(0))))
        (when (haskell-indentation-bird-p)
          (insert "> "))
        (haskell-indentation-reindent-to
         (haskell-indentation-next-indentation (- ci 1) indentations 'nofail)
         'move)))))

(defun haskell-indentation-next-indentation (col indentations &optional nofail)
  "Find the leftmost indentation which is greater than COL.
Indentations are taken from INDENTATIONS, which should be a
list.  Return the last indentation if there are no bigger ones and
NOFAIL is non-NIL."
  (when (null indentations)
    (error "haskell-indentation-next-indentation called with empty list"))
  (or (cl-find-if (lambda (i) (> i col)) indentations)
      (when nofail
        (car (last indentations)))))

(defun haskell-indentation-previous-indentation (col indentations &optional nofail)
  "Find the rightmost indentation less than COL from INDENTATIONS.
When no indentations are less than COL, return the rightmost indentation
if NOFAIL is non-nil, or nil otherwise."
  (when (null indentations)
    (error "haskell-indentation-previous-indentation called with empty list"))
  (let ((rev (reverse indentations)))
    (or (cl-find-if (lambda (i) (< i col)) rev)
        (when nofail
          (car rev)))))

(defvar haskell-indentation-dyn-last-direction nil
  "") ; FIXME
(defvar haskell-indentation-dyn-last-indentations nil
  "") ; FIXME

(defun haskell-indentation-indent-line ()
  "Indent current line, cycle though indentation positions.
Do nothing inside multiline comments and multiline strings.
Start enumerating the indentation points to the right.  The user
can continue by repeatedly pressing TAB.  When there is no more
indentation points to the right, we switch going to the left."
  (interactive "*")
  ;; try to repeat
  (when (not (haskell-indentation-indent-line-repeat))
    (setq haskell-indentation-dyn-last-direction nil)
    ;; parse error is intentionally not cought here, it may come from
    ;; `haskell-indentation-find-indentations', but escapes the scope
    ;; and aborts the opertaion before any moving happens
    (let* ((cc (current-column))
           (ci (haskell-indentation-current-indentation))
           (inds (save-excursion
                   (move-to-column ci)
                   (or (haskell-indentation-find-indentations)
                       '(0))))
           (valid (memq ci inds))
           (cursor-in-whitespace (< cc ci)))

      (if (and valid cursor-in-whitespace)
          (move-to-column ci)
        (haskell-indentation-reindent-to
         (haskell-indentation-next-indentation ci inds 'nofail)
         cursor-in-whitespace))
      (setq haskell-indentation-dyn-last-direction 'right
            haskell-indentation-dyn-last-indentations inds))))

(defun haskell-indentation-indent-line-repeat ()
  "Cycle though indentation positions."
  (cond
   ((and (memq last-command
               '(indent-for-tab-command
                 haskell-indentation-indent-backwards))
         (eq haskell-indentation-dyn-last-direction 'region))
    (let ((mark-even-if-inactive t))
      (haskell-indentation-indent-rigidly
       (region-beginning)
       (region-end)
       1))
    t)
   ((and (eq last-command 'indent-for-tab-command)
         (memq haskell-indentation-dyn-last-direction '(left right))
         haskell-indentation-dyn-last-indentations)
    (let ((ci (haskell-indentation-current-indentation)))
      (if (eq haskell-indentation-dyn-last-direction 'left)
          (haskell-indentation-reindent-to
           (haskell-indentation-previous-indentation
            ci haskell-indentation-dyn-last-indentations 'nofail))
        ;; right
        (if (haskell-indentation-next-indentation
             ci haskell-indentation-dyn-last-indentations)
            (haskell-indentation-reindent-to
             (haskell-indentation-next-indentation
              ci haskell-indentation-dyn-last-indentations 'nofail))
          ;; but failed, switch to left
          (setq haskell-indentation-dyn-last-direction 'left)
          (haskell-indentation-indent-line-repeat)))
      t))
   (t nil)))

(defun haskell-indentation-indent-region (_start _end)
  "This function does nothing.

It is better to do nothing to indent region in Haskell than to
break the semantics of indentation.  This function is used for
`indent-region-function' because the default is to call
`indent-line-function' on every line from START to END and that
also produces catastrophic results.

Someday we will have indent region that preserves semantics and
fixes up only indentation."
  nil)

(defun haskell-indentation-indent-backwards ()
  "Indent the current line to the previous indentation point."
  (interactive "*")
  (cond
   ((and (memq last-command
               '(indent-for-tab-command haskell-indentation-indent-backwards))
         (eq haskell-indentation-dyn-last-direction 'region))
    (let ((mark-even-if-inactive t))
      (haskell-indentation-indent-rigidly (region-beginning) (region-end) -1)))
   ((use-region-p)
    (setq haskell-indentation-dyn-last-direction 'region)
    (haskell-indentation-indent-rigidly (region-beginning) (region-end) -1)
    (message "Press TAB or S-TAB again to indent the region more"))
   (t
    (setq haskell-indentation-dyn-last-direction nil)
    (let* ((cc (current-column))
           (ci (haskell-indentation-current-indentation))
           (inds (save-excursion
                   (move-to-column ci)
                   (or (haskell-indentation-find-indentations)
                       '(0))))
           (cursor-in-whitespace (< cc ci))
           (pi (haskell-indentation-previous-indentation ci inds)))
      (if (null pi)
          ;; if there are no more indentations to the left, just go to column 0
          (haskell-indentation-reindent-to
           (car (haskell-indentation-first-indentation)) cursor-in-whitespace)
        (haskell-indentation-reindent-to pi cursor-in-whitespace))))))

(defun haskell-indentation-common-electric-command (arg)
  "Call `self-insert-command' to insert the character typed ARG times
and indent when all of the following are true:
1) The character is the first non-whitespace character on the line.
2) There is only one possible indentation position.
3) The variable `electric-indent-mode' or `haskell-indentation-electric-flag'
   is non-nil.
4) The point is not in a comment, string, or quasiquote."
  (interactive "*p")
  (let ((col (current-column))
        ind)
    (self-insert-command arg)
    (when (and (or haskell-indentation-electric-flag
                   electric-indent-mode)
               (= (haskell-indentation-current-indentation)
                  col)
               (> arg 0)
               (not (nth 8 (syntax-ppss)))
               (= 1 (save-excursion
                      (move-to-column col)
                      (length (setq ind (haskell-indentation-find-indentations))))))
      (haskell-indentation-reindent-to (car ind)))))


;;----------------------------------------------------------------------------
;; Parser Starts Here

;; The parser is implemented as a recursive descent parser.  Each parser
;; advances the point to after the expression it parses, and sets the
;; dynamic scoped variables containing the information about the
;; indentations.  The dynamic scoping allows transparent backtracking to
;; previous states of these variables.  A new state can be set using `let'.
;; When the scope of this function ends, the variable is automatically
;; reverted to its old value.

;; This is basicly a performance hack.  It would have been possible to
;; thread this state using a association-list through the parsers, but it
;; would be probably more complicated and slower due to the lack of real
;; closures in Emacs Lisp.
;;
;; When finished parsing, the tokenizer returns 'end-token, and
;; following-token is set to the token after point.  The parser adds its
;; indentations to possible-indentations and returns to it's parent, or
;; exits non-locally by throwing parse-end, so that the parent will not add
;; new indentations to it.

;; the parse state:
(defvar following-token)        ;; the next token after parsing finished
;; the token at the current parser point or a pseudo-token (see
;; `haskell-indentation-read-next-token')
(defvar current-token)
(defvar previous-token)
(defvar left-indent)            ;; most left possible indentation
(defvar starter-indent)         ;; column at a keyword
(defvar current-indent)         ;; the most right indentation
(defvar layout-indent)          ;; the column of the layout list
(defvar possible-indentations)  ;; the return value of the indentations
(defvar indentation-point)      ;; where to stop parsing
(defvar implicit-layout-active) ;; is "off-side" rule active?

(defun haskell-indentation-goto-least-indentation ()
  "" ; FIXME
  (beginning-of-line)
  (if (haskell-indentation-bird-p)
      (catch 'return
        (while t
          (when (not (eq (char-after) ?>))
            (forward-line)
            (forward-char 2)
            (throw 'return nil))
          (let ((ps (nth 8 (syntax-ppss))))
            (when ps ;; inside comment or string
              (goto-char ps)
              (beginning-of-line)))
          (when (and (>= 2 (haskell-indentation-current-indentation))
                     (not (looking-at ">\\s-*$")))
            (forward-char 2)
            (throw 'return nil))
          (when (bobp)
            (forward-char 2)
            (throw 'return nil))
          (forward-line -1)))
    ;; not bird style
    (catch 'return
      (while (not (bobp))
        (let ((point (point)))
          ;; (forward-comment -1) gets lost if there are unterminated
          ;; string constants and does not move point anywhere. We fix
          ;; that case with (forward-line -1)
          (forward-comment (- (buffer-size)))
          (if (equal (point) point)
              (forward-line -1)
            (beginning-of-line)))
        (let* ((ps (syntax-ppss))
              (start-of-comment-or-string (nth 8 ps))
              (start-of-list-expression (nth 1 ps)))
          (cond
           (start-of-comment-or-string
            ;; inside comment or string
            (goto-char start-of-comment-or-string))
           (start-of-list-expression
            ;; inside a parenthesized expression
            (goto-char start-of-list-expression))
           ((= 0 (haskell-indentation-current-indentation))
             (throw 'return nil))))))
    (beginning-of-line)
    (when (bobp)
      (forward-comment (buffer-size)))))

(defun haskell-indentation-parse-to-indentations ()
  "" ; FIXME
  (save-excursion
    (skip-syntax-forward "-")
    (let ((indentation-point (point))
          (layout-indent 0)
          (current-indent haskell-indentation-layout-offset)
          (starter-indent haskell-indentation-layout-offset)
          (left-indent haskell-indentation-layout-offset)
          (case-fold-search nil)
          current-token
          previous-token
          following-token
          possible-indentations
          implicit-layout-active)
      (haskell-indentation-goto-least-indentation)
      (if (<= indentation-point (point))
          (haskell-indentation-first-indentation)
        (setq current-token (haskell-indentation-peek-token))
        (catch 'parse-end
          (haskell-indentation-toplevel))
        possible-indentations))))

(defun haskell-indentation-first-indentation ()
  "Return column of first indentation."
  (list (if (haskell-indentation-bird-p) 2 0)))

(defun haskell-indentation-find-indentations ()
  "Return list of indentation positions corresponding to actual cursor position."
  (let ((ppss (syntax-ppss)))
    (cond
     ((nth 3 ppss)
      (if (save-excursion
            (and (forward-line -1)
                 (< (nth 8 ppss) (point))))
          ;; if this string goes over more than one line we want to
          ;; sync with the last line, not the first one
          (list (save-excursion
                  (forward-line -1)
                  (current-indentation)))

        (append
         (haskell-indentation-first-indentation)
         (list (save-excursion
                 (goto-char (nth 8 ppss))
                 (current-column))))))
     ((nth 4 ppss)
      (if (save-excursion
            (and (skip-syntax-forward "-")
                 (eolp)
                 (not (> (forward-line 1) 0))
                 (not (nth 4 (syntax-ppss)))))
          (haskell-indentation-parse-to-indentations)
        (haskell-indentation-first-indentation)))
     (t
      (haskell-indentation-parse-to-indentations)))))

(defconst haskell-indentation-unicode-tokens
  '(("→" . "->")     ;; #x2192 RIGHTWARDS ARROW
    ("∷" . "::")     ;; #x2237 PROPORTION
    ("←" . "<-")     ;; #x2190 LEFTWARDS ARROW
    ("⇒" . "=>")     ;; #x21D2 RIGHTWARDS DOUBLE ARROW
    ("∀" . "forall") ;; #x2200 FOR ALL
    ("⤙" . "-<")     ;; #x2919 LEFTWARDS ARROW-TAIL
    ("⤚" . ">-")     ;; #x291A RIGHTWARDS ARROW-TAIL
    ("⤛" . "-<<")    ;; #x291B LEFTWARDS DOUBLE ARROW-TAIL
    ("⤜" . ">>-")    ;; #x291C RIGHTWARDS DOUBLE ARROW-TAIL
    ("★" . "*"))     ;; #x2605 BLACK STAR
  "Translation from UnicodeSyntax tokens to their ASCII representation.")

(defconst haskell-indentation-toplevel-list
  `(("module"   . haskell-indentation-module)
    ("data"     . haskell-indentation-data)
    ("type"     . haskell-indentation-data)
    ("newtype"  . haskell-indentation-data)
    ("import"   . haskell-indentation-import)
    ("foreign"  . haskell-indentation-foreign)
    ("where"    . haskell-indentation-toplevel-where)
    ("class"    . haskell-indentation-class-declaration)
    ("instance" . haskell-indentation-class-declaration)
    ("deriving" . haskell-indentation-deriving))
  "Alist of toplevel keywords with associated parsers.")

(defconst haskell-indentation-type-list
  `(("::" .
     ,(apply-partially 'haskell-indentation-with-starter
                       (apply-partially 'haskell-indentation-separated
                                        'haskell-indentation-type '("->" "=>"))))
    ("("  .
     ,(apply-partially 'haskell-indentation-list
                       'haskell-indentation-type ")" ","))
    ("["  .
     ,(apply-partially 'haskell-indentation-list
                       'haskell-indentation-type "]" ","))
    ("{"  .
     ,(apply-partially 'haskell-indentation-list
                       'haskell-indentation-type "}" ",")))
  "Alist of tokens in type declarations with associated parsers.")

(defconst haskell-indentation-expression-list
  `(("data"    . haskell-indentation-data)
    ("type"    . haskell-indentation-data)
    ("newtype" . haskell-indentation-data)
    ("if"      . haskell-indentation-if)
    ("let"     .
     ,(apply-partially 'haskell-indentation-phrase
                       '(haskell-indentation-declaration-layout
                         "in" haskell-indentation-expression)))
    ("do"      .
     ,(apply-partially 'haskell-indentation-with-starter
                       'haskell-indentation-expression-layout))
    ("mdo"     .
     ,(apply-partially 'haskell-indentation-with-starter
                       'haskell-indentation-expression-layout))
    ("rec"     .
     ,(apply-partially 'haskell-indentation-with-starter
                       'haskell-indentation-expression-layout))
    ("case"    .
     ,(apply-partially 'haskell-indentation-phrase
                       '(haskell-indentation-expression
                         "of" haskell-indentation-case-layout)))
    ("\\"      .
     ,(apply-partially 'haskell-indentation-with-starter
                       'haskell-indentation-lambda-maybe-lambdacase))
    ("proc"    .
     ,(apply-partially 'haskell-indentation-phrase
                       '(haskell-indentation-expression
                         "->" haskell-indentation-expression)))
    ("where"   .
     ,(apply-partially 'haskell-indentation-with-starter
                       'haskell-indentation-declaration-layout nil t))
    ("::"      .        haskell-indentation-scoped-type)
    ("="       .
     ,(apply-partially 'haskell-indentation-statement-right
                       'haskell-indentation-expression))
    ("<-"      .
     ,(apply-partially 'haskell-indentation-statement-right
                       'haskell-indentation-expression))
    ("("       .
     ,(apply-partially 'haskell-indentation-list
                       'haskell-indentation-expression
                       ")"
                       '(list "," "->")))
    ("["       .
     ,(apply-partially 'haskell-indentation-list
                       'haskell-indentation-expression "]" "," "|"))
    ("{"       .
     ,(apply-partially 'haskell-indentation-list
                       'haskell-indentation-expression "}" ",")))
  "Alist of keywords in expressions with associated parsers.")

(defun haskell-indentation-expression-layout ()
  "Parse layout list with expressions, such as after \"do\"."
  (haskell-indentation-layout #'haskell-indentation-expression))

(defun haskell-indentation-declaration-layout ()
  "Parse layout list with declarations, such as after \"where\"."
  (haskell-indentation-layout #'haskell-indentation-declaration))

(defun haskell-indentation-case-layout ()
  "Parse layout list with case expressions."
  (haskell-indentation-layout #'haskell-indentation-case))

(defun haskell-indentation-lambda-maybe-lambdacase ()
  "Parse lambda or lambda-case expression.
After a lambda (backslash) there are two possible cases:

- the new lambdacase expression, that can be recognized by the
  next token being \"case\";

- or simply an anonymous function definition in the form of
  \"expression -> expression\"."
  (if (string= current-token "case")
      (haskell-indentation-with-starter
       #'haskell-indentation-case-layout)
    (haskell-indentation-phrase-rest
     '(haskell-indentation-expression "->" haskell-indentation-expression))))

(defun haskell-indentation-fundep ()
  "Parse functional dependency."
  (haskell-indentation-with-starter
   (apply-partially #'haskell-indentation-separated
                    #'haskell-indentation-fundep1 ",")))

(defun haskell-indentation-fundep1 ()
  "Parse an item in functional dependency declaration."
  (let ((current-indent (current-column)))
    (while (member current-token '(value "->"))
      (haskell-indentation-read-next-token))
    (when (and (eq current-token 'end-tokens)
               (member following-token '(value "->")))
      (haskell-indentation-add-indentation current-indent))))

(defun haskell-indentation-toplevel ()
  "Parse toplevel statements."
  (haskell-indentation-layout
   (lambda ()
     (let ((parser (assoc current-token haskell-indentation-toplevel-list)))
       (if parser
           (funcall (cdr parser))
         (haskell-indentation-declaration))))))

(defun haskell-indentation-type ()
  "Parse type declaration."
  (let ((current-indent (current-column)))
    (catch 'return
      (while t
        (cond
         ((member current-token '(value operator "->" "=>"))
          (haskell-indentation-read-next-token))

         ((eq current-token 'end-tokens)
          (when (member following-token
                        '(value operator no-following-token
                                "(" "[" "{" "::"))
            (if (equal following-token "=>")
                (haskell-indentation-add-indentation starter-indent)
              (haskell-indentation-add-indentation current-indent))
            (haskell-indentation-add-indentation left-indent))
          (throw 'return nil))
         (t (let ((parser (assoc current-token haskell-indentation-type-list)))
              (if (not parser)
                  (throw 'return nil)
                (funcall (cdr parser))))))))))

(defun haskell-indentation-type-1 ()
  "Parse a single type declaration."
  (let ((current-indent (current-column)))
    (catch 'return
      (cond
       ((member current-token '(value operator "->" "=>"))
        (haskell-indentation-read-next-token))

       ((eq current-token 'end-tokens)
        (when (member following-token
                      '(value operator no-following-token
                              "->" "=>" "(" "[" "{" "::"))
          (haskell-indentation-add-indentation current-indent))
        (throw 'return nil))
       (t (let ((parser (assoc current-token haskell-indentation-type-list)))
            (if (not parser)
                (throw 'return nil)
              (funcall (cdr parser)))))))))

(defun haskell-indentation-scoped-type ()
  "Parse scoped type declaration.

For example
   let x :: Int = 12
   do x :: Int <- return 12"
  (haskell-indentation-with-starter
   (apply-partially #'haskell-indentation-separated #'haskell-indentation-type '("->" "=>")))
  (when (member current-token '("<-" "="))
    (haskell-indentation-statement-right #'haskell-indentation-expression)))

(defun haskell-indentation-data ()
  "Parse data or type declaration."
  (haskell-indentation-read-next-token)
  (when (string= current-token "instance")
    (haskell-indentation-read-next-token))
  (haskell-indentation-type)
  (cond ((eq current-token 'end-tokens)
         (when (member following-token '("=" "where"))
           (haskell-indentation-add-indentation current-indent)
           (throw 'parse-end nil)))
        ((string= current-token "=")
         (let ((starter-indent-inside (current-column)))
           (haskell-indentation-with-starter
            (lambda ()
              (haskell-indentation-separated
               #'haskell-indentation-expression "|")))
           (cond
            ((equal current-token 'end-tokens)
             (when (string= following-token "deriving")
               (haskell-indentation-push-indentation starter-indent-inside)
               (haskell-indentation-add-left-indent)))
            ((equal current-token "deriving")
             (haskell-indentation-with-starter
              #'haskell-indentation-type-1)))))
        ((string= current-token "where")
         (haskell-indentation-with-starter
          #'haskell-indentation-expression-layout nil)
         (cond
          ((equal current-token 'end-tokens)
           (when (string= following-token "deriving")
             (haskell-indentation-add-left-indent)))
          ((equal current-token "deriving")
           (haskell-indentation-with-starter
            #'haskell-indentation-type-1))))))

(defun haskell-indentation-import ()
  "Parse import declaration."
  (haskell-indentation-with-starter #'haskell-indentation-expression))

(defun haskell-indentation-foreign ()
  "Parse foreign import declaration."
  (haskell-indentation-with-starter (apply-partially #'haskell-indentation-expression '(value operator "import"))))

(defun haskell-indentation-class-declaration ()
  "Parse class declaration."
  (haskell-indentation-with-starter
   (lambda ()
     (haskell-indentation-type)
     (when (string= current-token "|")
       (haskell-indentation-fundep))
     (when (string= current-token "where")
       (haskell-indentation-with-starter
        #'haskell-indentation-declaration-layout nil)))))

(defun haskell-indentation-deriving ()
  "Parse standalone declaration."
  (haskell-indentation-with-starter
   (lambda ()
     (when (string= "instance" current-token)
       (haskell-indentation-read-next-token))
     (when (equal current-token 'end-tokens)
       (haskell-indentation-add-left-indent)
       (throw 'parse-end nil))
     (haskell-indentation-type)
     (when (string= current-token "|")
       (haskell-indentation-fundep)))))

(defun haskell-indentation-module ()
  "Parse module declaration."
  (haskell-indentation-with-starter
   (lambda ()
     (haskell-indentation-read-next-token)
     (when (equal current-token 'layout-item)
       (haskell-indentation-read-next-token))
     (when (string= current-token "(")
       (haskell-indentation-list
        #'haskell-indentation-module-export
        ")" ","))
     (if (string= current-token "where")
         (haskell-indentation-read-next-token)

       (when (eq current-token 'end-tokens)
         (when (member following-token '(value no-following-token "("))
           (haskell-indentation-add-indentation
            (+ starter-indent haskell-indentation-starter-offset))
           (haskell-indentation-add-indentation
            (+ left-indent haskell-indentation-starter-offset))
           (throw 'parse-end nil))
         (haskell-indentation-add-layout-indent)
         (throw 'parse-end nil))))))

(defun haskell-indentation-toplevel-where ()
  "Parse 'where' that we may hit as a standalone in module declaration."
  (haskell-indentation-read-next-token)

  (when (eq current-token 'end-tokens)
    (haskell-indentation-add-layout-indent)
    (throw 'parse-end nil)))

(defun haskell-indentation-module-export ()
  "Parse export list."
  (cond ((string= current-token "module")
         (let ((current-indent (current-column)))
           (haskell-indentation-read-next-token)
           (cond ((eq current-token 'end-tokens)
                  (haskell-indentation-add-indentation current-indent))
                 ((eq current-token 'value)
                  (haskell-indentation-read-next-token)))))
        (t (haskell-indentation-type))))

(defun haskell-indentation-list (parser end sep &optional stmt-sep)
  "Parse a list, pair or other expression containing multiple
items parsed by PARSER, separated by SEP or STMT-SEP, and ending
with END."
  ;; note that we use macro expansion here to preserver Emacs 23
  ;; compatibility and its lack of lexical binding
  (haskell-indentation-with-starter
   `(lambda ()
      (let ((implicit-layout-active nil))
        (haskell-indentation-separated
         #',parser ,sep ,stmt-sep)))
   end))

(defun haskell-indentation-with-starter (parser &optional end where-expr?)
  "Parse an expression starting with a keyword or parenthesis.
Skip the keyword or parenthesis." ; FIXME: better description needed
  (let ((starter-column (current-column))
        (current-indent current-indent)
        (left-indent
         (if (= (current-column) (haskell-indentation-current-indentation))
             (current-column)
           left-indent)))
    (haskell-indentation-read-next-token)
    (when (eq current-token 'end-tokens)
      (cond ((equal following-token end)
             ;; indent before keyword or parenthesis
             (haskell-indentation-add-indentation starter-column))
            (where-expr?
             ;; left indent + where post indent
             (haskell-indentation-add-where-post-indent left-indent))
            (t
             (haskell-indentation-add-left-indent)))
      (throw 'parse-end nil))
    (let* ((current-indent (current-column))
           (starter-indent (min starter-column current-indent))
           (left-indent
            (if end
                (+ starter-indent haskell-indentation-starter-offset)
              left-indent)))
      (funcall parser)
      (cond ((eq current-token 'end-tokens)
             (when (equal following-token end)
               ;; indent before keyword or parenthesis
               (haskell-indentation-add-indentation starter-indent))
             ;; add no more indentations if we expect a closing keyword
             (when end
               (throw 'parse-end nil)))
            ((equal current-token end)
             (haskell-indentation-read-next-token))))))

(defun haskell-indentation-case-alternative ()
  "" ; FIXME
  (setq left-indent (current-column))
  (haskell-indentation-separated #'haskell-indentation-expression "," nil)
  (cond ((eq current-token 'end-tokens)
         (haskell-indentation-add-indentation current-indent))
        ((string= current-token "->")
         (haskell-indentation-statement-right #'haskell-indentation-expression))
        ;; otherwise fallthrough
        ))

(defun haskell-indentation-case ()
  "" ; FIXME
  (haskell-indentation-expression)
  (cond ((eq current-token 'end-tokens)
         (haskell-indentation-add-indentation current-indent))
        ((string= current-token "|")
         (haskell-indentation-with-starter
          (apply-partially #'haskell-indentation-separated
                           #'haskell-indentation-case-alternative "|" nil)
          nil))
        ((string= current-token "->")
         (haskell-indentation-statement-right #'haskell-indentation-expression))
        ;; otherwise fallthrough
        ))

(defun haskell-indentation-statement-right (parser)
  "Process right side of a statement.
Set `current-indent' to the current column and calls the given
parser.  If parsing ends here, set indentation to left-indent."
  (haskell-indentation-read-next-token)
  (when (eq current-token 'end-tokens)
    (haskell-indentation-add-left-indent)
    (haskell-indentation-add-indentation current-indent)
    (throw 'parse-end nil))
  (funcall parser)
  (when (equal current-token "where")
    (haskell-indentation-with-starter
     #'haskell-indentation-expression-layout nil)))

(defun haskell-indentation-guard ()
  "Parse \"guard\" statement."
  (setq left-indent (current-column))
  (haskell-indentation-separated
   #'haskell-indentation-expression "," nil))

(defun haskell-indentation-declaration ()
  "Parse function or type declaration."
  (haskell-indentation-separated #'haskell-indentation-expression "," nil)
  (when (string= current-token "|")
    (haskell-indentation-with-starter
     (apply-partially #'haskell-indentation-separated
                      #'haskell-indentation-guard "|" nil)
     nil))
  (when (eq current-token 'end-tokens)
   (when (member following-token '("|" "=" "::" ","))
     (haskell-indentation-add-indentation current-indent)
     (throw 'parse-end nil))))

(defun haskell-indentation-layout (parser)
  "Parse layout list, where each layout item is parsed by parser."
  (if (string= current-token "{")
      (haskell-indentation-list parser "}" ";") ; explicit layout
    (haskell-indentation-implicit-layout-list parser)))

(defun haskell-indentation-expression-token-p (token)
  "Return non-NIL value if TOKEN is an expression token."
  (member token
          '("if" "let" "do" "case" "\\" "(" "{" "[" "::"
            value operator no-following-token)))

(defun haskell-indentation-expression (&optional accepted-tokens)
  "Parse an expression until an unknown token is encountered."
  (catch 'return
    (let ((current-indent (current-column)))
      (unless accepted-tokens
        (setq accepted-tokens '(value operator)))
      (while t
        (cond
         ((memq current-token accepted-tokens)
          (haskell-indentation-read-next-token))
         ((eq current-token 'end-tokens)
          (cond ((string= following-token "where")
                 (haskell-indentation-add-where-pre-indent)) ; before a where
                ((haskell-indentation-expression-token-p following-token)
                 ;; a normal expression can be either continued or have
                 ;; left indent
                 (haskell-indentation-add-indentation
                  current-indent)
                 (haskell-indentation-add-indentation
                  left-indent)))
          (throw 'return nil))
         (t (let ((parser (assoc current-token
                                 haskell-indentation-expression-list)))
              (when (null parser)
                (throw 'return nil)) ; not expression token, so exit
              (funcall (cdr parser)) ; run parser

              ;; after an 'open' expression such as 'if', exit
              (unless (member (car parser) '("(" "[" "{" "case"))
                (throw 'return nil)))))))))

(defun haskell-indentation-separated (parser separator &optional stmt-separator)
  "Evaluate PARSER separated by SEPARATOR and STMT-SEPARATOR.
If STMT-SEPARATOR is not NIL, it will be used to set a new starter-indent.

For example:

   [ i | i <- [1..10]
    ,"
  (catch 'return
    (unless (listp separator)
      (setq separator (list separator)))
    (unless (listp stmt-separator)
      (setq stmt-separator (list stmt-separator)))
    (while t
      (funcall parser)
      (cond ((member current-token separator)
             (haskell-indentation-at-separator))

            ((member current-token stmt-separator)
             (setq starter-indent (current-column))
             (haskell-indentation-at-separator))

            ((eq current-token 'end-tokens)
             (when (or (member following-token separator)
                       (member following-token stmt-separator))
               ;; Set an indentation before a separator, for example:
               ;;  [ 1   or   [ 1 | a
               ;;  , 2            , 20
               (haskell-indentation-add-indentation starter-indent)
               (when (< left-indent starter-indent)
                 (haskell-indentation-add-indentation left-indent))
               (throw 'parse-end nil))
             (when (equal following-token 'no-following-token)
               ;; Set an indentation before a separator, for example:
               ;;  [ 1   or   [ 1 | a
               ;;  , 2            , 20
               (haskell-indentation-add-indentation starter-indent)
               (haskell-indentation-add-indentation left-indent))
             (throw 'return nil))
            (t (throw 'return nil))))))

(defun haskell-indentation-at-separator ()
  "At a separator.

If at a new line, set starter-indent at the separator
and current-indent after the separator, for example:

l = [  1
     , 2
     ,    -- start now here."
  (let ((separator-column
         (and (= (current-column) (haskell-indentation-current-indentation))
              (current-column))))
    (haskell-indentation-read-next-token)
    (cond ((eq current-token 'end-tokens)
           (haskell-indentation-add-indentation current-indent)
           (haskell-indentation-add-indentation left-indent)
           (throw 'return nil))
          (separator-column ; on the beginning of the line
           (setq current-indent (current-column))
           (setq starter-indent separator-column)
           (setq left-indent
            (+ starter-indent haskell-indentation-starter-offset))))))

(defun haskell-indentation-implicit-layout-list (parser)
  "An implicit layout list, elements are parsed with PARSER.
This sets the `layout-indent' variable to the column where the
layout starts."
  (let* ((layout-indent (current-column))
         (current-indent (current-column))
         (left-indent (current-column))
         (implicit-layout-active t))
    (catch 'return
      (while t
        (let ((left-indent left-indent))
          (funcall parser))
        (cond ((member current-token '(layout-item ";"))
               (haskell-indentation-read-next-token))
              ((eq current-token 'end-tokens)
               (when (or (and
                          (not (member following-token '("{" operator)))
                          (not (member previous-token '(operator)))
                          (haskell-indentation-expression-token-p following-token))
                         (string= following-token ";")
                         (and (equal layout-indent 0)
                              (member following-token (mapcar #'car haskell-indentation-toplevel-list))
                              (not (string= following-token "where"))))
                 (haskell-indentation-add-layout-indent))
               (throw 'return nil))
              (t (throw 'return nil))))))
  ;; put `haskell-indentation-read-next-token' outside the current-indent
  ;; definition so it will not return 'layout-end again
  (when (eq current-token 'layout-end)
    (let ((implicit-layout-active t))
      ;; leave layout at 'layout-end or illegal token
      (haskell-indentation-read-next-token))))

(defun haskell-indentation-if ()
  "" ; FIXME
  (haskell-indentation-with-starter
   (lambda ()
     (if (string= current-token "|")
         (haskell-indentation-with-starter
          (lambda ()
            (haskell-indentation-separated
             #'haskell-indentation-case-alternative "|" nil))
          nil)
       (haskell-indentation-phrase-rest
        '(haskell-indentation-expression
          "then" haskell-indentation-expression
          "else" haskell-indentation-expression))))
   nil))

(defun haskell-indentation-phrase (phrase)
  "" ; FIXME
  (haskell-indentation-with-starter
   (apply-partially #'haskell-indentation-phrase-rest phrase)
   nil))

(defun haskell-indentation-phrase-rest (phrase1)
  "" ; FIXME
  (while phrase1
    (let ((phrase phrase1))
      (setq phrase1 nil)
      (let ((current-indent (current-column))
            (left-indent left-indent)
            (layout-indent layout-indent))
        (funcall (car phrase)))
      (cond
       ((eq current-token 'end-tokens)
        (cond ((null (cdr phrase))) ;; fallthrough
              ((equal following-token (cadr phrase))
               (haskell-indentation-add-indentation starter-indent)
               (unless (member following-token '("," ";"))
                 ;; we want to keep comma and semicolon aligned always
                 (haskell-indentation-add-indentation left-indent))
               (throw 'parse-end nil))
              ((string= (cadr phrase) "in")
               (when (= left-indent layout-indent)
                 (haskell-indentation-add-layout-indent)
                 (throw 'parse-end nil)))
              (t (throw 'parse-end nil))))
       ((null (cdr phrase)))
       ((equal (cadr phrase) current-token)
        (haskell-indentation-read-next-token)
        (when (eq current-token 'end-tokens)
          (haskell-indentation-add-indentation
           (+ starter-indent haskell-indentation-starter-offset))
          (haskell-indentation-add-indentation
           (+ left-indent haskell-indentation-starter-offset))
          (throw 'parse-end nil))
        (setq phrase1 (cddr phrase)))
       ((string= (cadr phrase) "in"))))))

(defun haskell-indentation-add-indentation (indent)
  "" ; FIXME
  (haskell-indentation-push-indentation
   (if (<= indent layout-indent)
       (+ layout-indent haskell-indentation-layout-offset)
     indent)))

(defun haskell-indentation-add-layout-indent ()
  "" ; FIXME
  (haskell-indentation-push-indentation layout-indent))

(defun haskell-indentation-add-where-pre-indent ()
  "" ; FIXME
  (haskell-indentation-push-indentation
   (+ layout-indent haskell-indentation-where-pre-offset))
  (if (= layout-indent haskell-indentation-layout-offset)
      (haskell-indentation-push-indentation
       haskell-indentation-where-pre-offset)))

(defun haskell-indentation-add-where-post-indent (indent)
  "" ; FIXME
  (haskell-indentation-push-indentation
   (+ indent haskell-indentation-where-post-offset)))

(defun haskell-indentation-add-left-indent ()
  "" ; FIXME
  (haskell-indentation-add-indentation
   (+ left-indent haskell-indentation-left-offset)))

(defun haskell-indentation-push-indentation (indent)
  "Add INDENT to list of possible indentations.

Add INDENT to `possible-indentations' if it is not there
yet. Keep the list in ascending order."
  (unless (member indent possible-indentations)
    (setq possible-indentations
          (sort (cons indent possible-indentations) #'<))))

(defun haskell-indentation-read-next-token ()
  "Go to the next token and set current-token to the next token.

The following symbols are used as pseudo tokens:

'layout-item: A new item in a layout list.  The next token
              will be the first token from the item.

'layout-end:  the end of a layout list.  Next token will be
              the first token after the layout list.

'end-tokens:  back at point where we started, following-token
              will be set to the next token.

Pseudo tokens are used only when implicit-layout-active is
t. That is the case only after keywords \"do\", \"where\",
\"let\" and \"of\".

If we are at a new line, parse-line is increased, and
current-indent and left-indent are set to the indentation of the
line."
  (cond ((and implicit-layout-active
              (eq current-token 'end-tokens))
         'end-tokens)
        ((and implicit-layout-active
              (eq current-token 'layout-end))
         (cond ((> layout-indent (current-column))
                'layout-end)
               ((= layout-indent (current-column))
                (setq current-token 'layout-item))
               ((< layout-indent (current-column))
                (setq current-token (haskell-indentation-peek-token)))))
        ((and implicit-layout-active
              (eq current-token 'layout-item))
         (setq current-token (haskell-indentation-peek-token)))
        ((and implicit-layout-active
              (> layout-indent (current-column)))
         (setq current-token 'layout-end))
        (t
         (setq previous-token (haskell-indentation-peek-token))
         (haskell-indentation-skip-token)
         (if (>= (point) indentation-point)
             (progn
               (setq following-token
                     (if (and (not (eobp))
                              (= (point) indentation-point))
                         (haskell-indentation-peek-token)
                       'no-following-token))
               (setq current-token 'end-tokens))
           (when (= (current-column) (haskell-indentation-current-indentation))
             ;; on a new line
             (setq current-indent (current-column)))
           (cond ((and implicit-layout-active
                       (> layout-indent (current-column)))
                  (setq current-token 'layout-end))
                 ((and implicit-layout-active
                       (= layout-indent (current-column)))
                  (setq current-token 'layout-item))
                 (t (setq current-token (haskell-indentation-peek-token))))))))

(defun haskell-indentation-peek-token ()
  "Return token starting at point."
  (cond ((looking-at "\\(if\\|then\\|else\\|let\\|in\\|mdo\\|rec\\|do\\|proc\\|case\\|of\\|where\\|module\\|deriving\\|import\\|data\\|type\\|newtype\\|class\\|instance\\)\\([^[:alnum:]'_]\\|$\\)")
         (match-string-no-properties 1))
        ((looking-at "[][(){}[,;]")
         (match-string-no-properties 0))
        ((looking-at "\\(\\\\\\|->\\|<-\\|::\\|=\\||\\|=>\\)\\([^-:!#$%&*+./<=>?@\\\\^|~]\\|$\\)")
         (match-string-no-properties 1))
        ((looking-at "\\(→\\|←\\|∷\\|⇒\\)\\([^-:!#$%&*+./<=>?@\\\\^|~]\\|$\\)")
         (let ((tok (match-string-no-properties 1)))
           (or (cdr (assoc tok haskell-indentation-unicode-tokens)) tok)))
        ((looking-at"[-:!#$%&*+./<=>?@\\\\^|~`]" )
         'operator)
        (t 'value)))

(defun haskell-indentation-skip-token ()
  "Skip to the next token."
  (if (haskell-lexeme-looking-at-token)
      (goto-char (match-end 0))
    ;; otherwise skip until space found
    (skip-syntax-forward "^-"))
  ;; we have to skip unterminated string fence at the end of line
  (skip-chars-forward "\n")
  (forward-comment (buffer-size))
  (while (and (haskell-indentation-bird-p)
              (bolp)
              (eq (char-after) ?>))
    (forward-char)
    (forward-comment (buffer-size))))

(provide 'haskell-indentation)

;; Local Variables:
;; tab-width: 8
;; End:

;;; haskell-indentation.el ends here
