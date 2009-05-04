;;; espresso.el --- Major mode for editing JavaScript source text
;; Copyright (C) 2008 Free Software Foundation, Inc.
;; Copyright (C) 2009 Daniel Colascione <dan.colascione@gmail.com>
;; Author: Karl Landstrom <karl.landstrom@brgeight.se>
;; Author: Daniel Colascione <dan.colascione@gmail.com>
;; Maintainer: Daniel Colascione <dan.colascione@gmail.com>
;; Version: 5
;; Date: 2009-04-30
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
;; After that, type M-x byte-compile-file and have Emacs byte-compile
;; this file. Performance is vastly better when this file is
;; byte-compiled.
;;
;; General Remarks:
;;
;; XXX: This mode assumes that block comments are not nested inside block
;; XXX: comments
;;
;; Exported names start with "espresso-"; private names start with
;; "espresso--".
;;
;; Code:

;;; Code

(require 'cc-mode)
(require 'font-lock)
(require 'newcomment)
(require 'thingatpt)

(eval-when-compile
  (require 'cl))

(declaim (optimize (speed 0) (safety 3))) ; XXX: change for release

;;; Constants

(defconst espresso--name-start-re "[a-zA-Z_$]"
  "Matches the first character of a Espresso identifier. No grouping")

(defconst espresso--stmt-delim-chars "^;{}?:")

(defconst espresso--name-re (concat espresso--name-start-re
                                    "\\(?:\\s_\\|\\sw\\)*")
  "Matches a Javascript identifier. No grouping.")

(defconst espresso--dotted-name-re
  (concat espresso--name-re "\\(?:\\." espresso--name-re "\\)*")
  "Matches a dot-separated sequence of Javascript names")

(defconst espresso--cpp-name-re espresso--name-re
  "Matches a C preprocessor name")

(defconst espresso--opt-cpp-start "^\\s-*#\\s-*\\([[:alnum:]]+\\)"
  "Regexp matching the prefix of a cpp directive including the directive
name, or nil in languages without preprocessor support.  The first
submatch surrounds the directive name.")

(defconst espresso--plain-method-re
  (concat "^\\s-*?\\(" espresso--dotted-name-re "\\)\\.prototype"
          "\\.\\(" espresso--name-re "\\)\\s-*?=\\s-*?\\(function\\)\\_>")
  "Regexp matching an old-fashioned explicit prototype \"method\"
  declaration. Group 1 is a (possibly-dotted) class name, group 2
  is a method name, and group 3 is the 'function' keyword." )

(defconst espresso--plain-class-re
  (concat "^\\s-*\\(" espresso--dotted-name-re "\\)\\.prototype"
          "\\s-*=\\s-*{")
  "Regexp matching an old-fashioned explicit prototype \"class\"
  declaration, as in Class.prototype = { method1: ...} ")

(defconst espresso--mp-class-decl-re
  (concat "^\\s-*var\\s-+"
          "\\(" espresso--name-re "\\)"
          "\\s-*=\\s-*"
          "\\(" espresso--dotted-name-re
          "\\)\\.extend\\(?:Final\\)?\\s-*(\\s-*{?\\s-*$")
  "var NewClass = BaseClass.extend(")

(defconst espresso--prototype-obsolete-class-decl-re
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" espresso--dotted-name-re "\\)"
          "\\s-*=\\s-*Class\\.create()")
  "var NewClass = Class.create()")

(defconst espresso--prototype-objextend-class-decl-re-1
  (concat "^\\s-*Object\\.extend\\s-*("
          "\\(" espresso--dotted-name-re "\\)"
          "\\s-*,\\s-*{"))

(defconst espresso--prototype-objextend-class-decl-re-2
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" espresso--dotted-name-re "\\)"
          "\\s-*=\\s-*Object\\.extend\\s-*\("))

(defconst espresso--prototype-class-decl-re
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" espresso--name-re "\\)"
          "\\s-*=\\s-*Class\\.create\\s-*(\\s-*"
          "\\(?:\\(" espresso--dotted-name-re "\\)\\s-*,\\s-*\\)?{?")
  "var NewClass = Class.create({")

;; Parent class name(s) (yes, multiple inheritance in Javascript) are
;; matched with dedicated font-lock matchers
(defconst espresso--dojo-class-decl-re
  (concat "^\\s-*dojo\\.declare\\s-*(\"\\(" espresso--dotted-name-re "\\)"))

(defconst espresso--extjs-class-decl-re-1
  (concat "^\\s-*Ext\\.extend\\s-*("
          "\\s-*\\(" espresso--dotted-name-re "\\)"
          "\\s-*,\\s-*\\(" espresso--dotted-name-re "\\)")
  "ExtJS class declaration (style 1)")

(defconst espresso--extjs-class-decl-re-2
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" espresso--name-re "\\)"
          "\\s-*=\\s-*Ext\\.extend\\s-*(\\s-*"
          "\\(" espresso--dotted-name-re "\\)")
  "ExtJS class declaration (style 2)")

(defconst espresso--mochikit-class-re
  (concat "^\\s-*MochiKit\\.Base\\.update\\s-*(\\s-*"
          "\\(" espresso--dotted-name-re "\\)")
  "MochiKit class declaration?")

(defconst espresso--dummy-class-style
  '(:name "[Automatically Generated Class]"))

(defconst espresso--class-styles
  `((:name            "Plain"
     :class-decl      ,espresso--plain-class-re
     :prototype       t
     :contexts        (toplevel)
     :framework       javascript)

    (:name            "MochiKit"
     :class-decl      ,espresso--mochikit-class-re
     :prototype       t
     :contexts        (toplevel)
     :framework       mochikit)

    (:name            "Prototype (Obsolete)"
     :class-decl      ,espresso--prototype-obsolete-class-decl-re
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Prototype (Modern)"
     :class-decl      ,espresso--prototype-class-decl-re
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Prototype (Object.extend)"
     :class-decl      ,espresso--prototype-objextend-class-decl-re-1
     :prototype       t
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Prototype (Object.extend) 2"
     :class-decl      ,espresso--prototype-objextend-class-decl-re-2
     :prototype       t
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Dojo"
     :class-decl      ,espresso--dojo-class-decl-re
     :contexts        (toplevel)
     :framework       dojo)

    (:name            "ExtJS (style 1)"
     :class-decl      ,espresso--extjs-class-decl-re-1
     :prototype       t
     :contexts        (toplevel)
     :framework       extjs)

    (:name            "ExtJS (style 2)"
     :class-decl      ,espresso--extjs-class-decl-re-2
     :contexts        (toplevel)
     :framework       extjs)

    (:name            "Merrill Press"
     :class-decl      ,espresso--mp-class-decl-re
     :contexts        (toplevel)
     :framework       merrillpress))

  "A list of class definition styles.

A class definition style is a plist with the following keys:

:name is a human-readable name of the class type

:class-decl is a regular expression giving the start of the
class. Its first group must match the name of its class. If there
is a parent class, the second group should match, and it should
be the name of the class.

If :prototype is present and non-nil, the parser will merge
declarations for this constructs with others at the same lexical
level that have the same name. Otherwise, multiple definitions
will create multiple top-level entries. Don't use :prototype
unnecessarily: it has an associated cost in performance.

If :strip-prototype is present and non-nil, then if the class
name as matched contains
")

(defconst espresso--available-frameworks
  (loop with available-frameworks
        for style in espresso--class-styles
        for framework = (plist-get style :framework)
        unless (memq framework available-frameworks)
        collect framework into available-frameworks
        finally return available-frameworks)

  "List of available frameworks symbols")

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

(defconst espresso--function-heading-3-re
  (concat
   "^\\s-*\\(?:var\\s-+\\)?\\(" espresso--dotted-name-re "\\)"
   "\\s-*=\\s-*function\\_>")
  "Matches a line in the form var MUMBLE = function. Match group
  1 is MUMBLE.")

(defconst espresso--macro-decl-re
  (concat "^\\s-*#\\s-*define\\s-+\\(" espresso--cpp-name-re "\\)\\s-*(")
  "Regular expression matching a CPP macro definition up to the opening
parenthesis. Match group 1 is the name of the function.")

(defun espresso--regexp-opt-symbol (list)
  "Like regexp-opt, but surround the optimized regular expression
with `\\\\_<' and `\\\\_>'."
  (concat "\\_<" (regexp-opt list t) "\\_>"))

(defconst espresso--keyword-re
  (espresso--regexp-opt-symbol
   '("abstract" "break" "case" "catch" "class" "const"
     "continue" "debugger" "default" "delete" "do" "else"
     "enum" "export" "extends" "final" "finally" "for"
     "function" "goto" "if" "implements" "import" "in"
     "instanceof" "interface" "native" "new" "package"
     "private" "protected" "public" "return" "static"
     "super" "switch" "synchronized" "throw"
     "throws" "transient" "try" "typeof" "var" "void" "let"
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

;; espresso--pitem is the basic building block of the lexical
;; database. When one refesr to a real part of the buffer, the region
;; of text to which it refers is split into a conceptual header and
;; body. Consider the (very short) block described by a hypothetical
;; espresso--pitem:
;;
;;   function foo(a,b,c) { return 42; }
;;   ^                    ^            ^
;;   |                    |            |
;;   +- h-begin           +- h-end     +- b-end
;;
;; (Remember that these are buffer positions, and therefore point
;; between characters, not at them. An arrow drawn to a character
;; indicates the corresponding position is between that character and
;; the one immediately preceding it.)
;;
;; The header is the region of text [h-begin, h-end], and is
;; the text needed to unambiguously recognize the start of the
;; construct. If the entire header is not present, the construct is
;; not recognized at all. No other pitems may be nested inside the
;; header.
;;
;; The body is the region [h-end, b-end]. It may contain nested
;; espresso--pitem instances. The body of a pitem may be empty: in
;; that case, b-end is equal to header-end.
;;
;; The three points obey the following relationship:
;;
;;   h-begin < h-end <= b-end
;;
;; We put a text property in the buffer on the character *before*
;; h-end, and if we see it, on the character *before* b-end.
;;
;; The text property for h-end, espresso--pstate, is actually a list
;; of all espresso--pitem instances open after the marked character.
;;
;; The text property for b-end, espresso--pend, is simply the
;; espresso--pitem that ends after the marked character. (Because
;; pitems always end when the paren-depth drops below a critical
;; value, and because we can only drop one level per character, only
;; one pitem may end at a given character.)
;;
;; In the structure below, we only store h-begin and (sometimes)
;; b-end. We can trivially and quickly find h-end by going to h-begin
;; and searching for an espresso--pstate text property. Since no other
;; espresso--pitem instances can be nested inside the header of a
;; pitem, the location after the character with this text property
;; must be h-end.
;;
;; espresso--pitem instances, are never modified (with the exception
;; of the b-end field), and instead copied and changed in the process.
;; (The exception and its caveats for b-end is described below.)
;;

(defstruct (espresso--pitem (:type list))
  ;; IMPORTANT: Do not alter the offsets of fields within the list.
  ;; Various bits of code depend on their positions, particularly
  ;; anything that manipulates the children list.

  ;; List of children inside this pitem's body
  (children nil :read-only t)

  ;; When we reach this paren depth after h-end, the pitem ends
  (paren-depth nil :read-only t)

  ;; Symbol or class-style plist if this is a class
  (type nil :read-only t)

  ;; See above
  (h-begin nil :read-only t)

  ;; List of strings giving the parts of the name of this pitem (e.g.,
  ;; '("MyClass" "myMethod"), or t if this pitem is anonymous
  (name nil :read-only t)

  ;; THIS FIELD IS MUTATED, and its value is shared by all copies of
  ;; this pitem: when we copy-and-modify pitem instances, we share
  ;; their tail structures, so all the copies actually have the same
  ;; terminating cons cell, which we modify directly.
  ;;
  ;; The field value is either a number (buffer location) or nil if
  ;; unknown.
  ;;
  ;; If the field's value is greater than `espresso--cache-end', the
  ;; value is stale and must be treated as if it were nil. Conversely,
  ;; if this field is nil, it is guaranteed that this pitem is open up
  ;; to at least `espresso--cache-end'. (This property is handy when
  ;; computing whether we're inside a given pitem.)
  ;;
  (b-end nil))

(defconst espresso--initial-pitem
  (make-espresso--pitem
   :paren-depth most-negative-fixnum
   :type 'toplevel)

  "The pitem we start parsing with")

;;; User Customization

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

(defcustom espresso-flat-functions nil
  "Treat nested functions as if they were top-level functions for
function movement, marking, and so on."
  :type 'boolean
  :group 'espresso)

(defcustom espresso-enabled-frameworks espresso--available-frameworks
  "Select which frameworks espresso-mode will recognize.

Turn off some frameworks you seldom use to improve performance.
The set of recognized frameworks can also be overriden on a
per-buffer basis."
  :type (cons 'set (loop for framework in espresso--available-frameworks
                         collect (list 'const framework)))
  :group 'espresso)

;;; KeyMap

(defvar espresso-mode-map
  (let ((keymap (make-sparse-keymap)))
    (mapc (lambda (key)
            (define-key keymap key #'espresso-insert-and-indent))
          '("{" "}" "(" ")" ":" ";" ","))
    (define-key keymap [(control ?c) (meta ?:)] #'espresso-js-eval)
    keymap)
  "Keymap for espresso-mode")

(defun espresso-insert-and-indent (key)
  "Runs the command bound to KEY in the global keymap, and if
we're not in a string or comment, indents the current line."
  (interactive (list (this-command-keys)))
  (call-interactively (lookup-key (current-global-map) key))
  (let ((syntax (save-restriction (widen) (syntax-ppss))))
    (and (not (nth 8 syntax))
         espresso-auto-indent-flag
         (indent-according-to-mode))))

;;; Syntax table and parsing

(defvar espresso-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?$ "_" table)
    table)
  "Syntax table used in Espresso mode.")

(defvar espresso--quick-match-re nil
  "Autogenerated regular expression to match buffer constructs")

(defvar espresso--quick-match-re-func nil
  "Autogenerated regular expression to match buffer constructs
and functions")

(make-variable-buffer-local 'espresso--quick-match-re)
(make-variable-buffer-local 'espresso--quick-match-re-func)

(defvar espresso--cache-end 1
  "Last place in the buffer the function cache is valid")
(make-variable-buffer-local 'espresso--cache-end)

(defvar espresso--last-parse-pos nil
  "Last place we parsed up to in espresso--ensure-cache")
(make-variable-buffer-local 'espresso--last-parse-pos)

(defvar espresso--state-at-last-parse-pos nil
  "pstate at espresso--last-parse-pos")
(make-variable-buffer-local 'espresso--state-at-last-parse-pos)

(defun espresso--flatten-list (list)
  (loop for item in list
        nconc (cond ((consp item)
                     (espresso--flatten-list item))

                    (item (list item)))))

(defun espresso--maybe-join (prefix separator suffix &rest list)
  "If LIST contains any element that is not nil, return its
non-nil elements, separated by SEPARATOR, prefixed by PREFIX, and
ended with SUFFIX as with `concat'. Otherwise, if LIST is empty,
return nil. If any element in LIST is itself a list, flatten that
element."
  (setq list (espresso--flatten-list list))
  (when list
    (concat prefix (mapconcat #'identity list separator) suffix)))

(defun espresso--update-quick-match-re ()
  "Update espresso--quick-match-re based on the current set of
enabled frameworks"
  (setq espresso--quick-match-re
        (espresso--maybe-join
         "^[ \t]*\\(?:" "\\|" "\\)"

         ;; #define mumble
         "#define[ \t]+[a-zA-Z_]"

         (when (memq 'extjs espresso-enabled-frameworks)
           "Ext\\.extend")

         (when (memq 'prototype espresso-enabled-frameworks)
           "Object\\.extend")

          ;; var mumble = THING (
         (espresso--maybe-join
          "\\(?:var[ \t]+\\)?[a-zA-Z_$0-9.]+[ \t]*=[ \t]*\\(?:"
          "\\|"
          "\\)[ \t]*\("

          (when (memq 'prototype espresso-enabled-frameworks)
                    "Class\\.create")

          (when (memq 'extjs espresso-enabled-frameworks)
            "Ext\\.extend")

          (when (memq 'merrillpress espresso-enabled-frameworks)
            "[a-zA-Z_$0-9]+\\.extend\\(?:Final\\)?"))

         (when (memq 'dojo espresso-enabled-frameworks)
           "dojo\\.declare[ \t]*\(")

         (when (memq 'mochikit espresso-enabled-frameworks)
           "MochiKit\\.Base\\.update[ \t]*\(")

         ;; mumble.prototypeTHING
         (espresso--maybe-join
          "[a-zA-Z_$0-9.]+\\.prototype\\(?:" "\\|" "\\)"

          (when (memq 'javascript espresso-enabled-frameworks)
            '( ;; foo.prototype.bar = function(
              "\\.[a-zA-Z_$0-9]+[ \t]*=[ \t]*function[ \t]*\("

              ;; mumble.prototype = {
              "[ \t]*=[ \t]*{")))))

  (setq espresso--quick-match-re-func
        (concat "function\\|" espresso--quick-match-re)))

(defun espresso--forward-text-property (propname)
  "Move over the next value of PROPNAME in the buffer. If found,
return that value and leave point after the character having that
value, otherwise return nil and leave point at EOB."
  (let ((next-value (get-text-property (point) propname)))
    (if next-value
        (forward-char)

      (goto-char (next-single-property-change
                  (point) propname nil (point-max)))
      (unless (eobp)
        (setq next-value (get-text-property (point) propname))
        (forward-char)))

    next-value))

(defun espresso--backward-text-property (propname)
    "Move over the previous value of PROPNAME in the buffer. If found,
return that value and leave point just before the character that
has that value, otherwise return nil and leave point at BOB."
    (unless (bobp)
      (let ((prev-value (get-text-property (1- (point)) propname)))
        (if prev-value
            (backward-char)

          (goto-char (previous-single-property-change
                      (point) propname nil (point-min)))

          (unless (bobp)
            (backward-char)
            (setq prev-value (get-text-property (point) propname))))

        prev-value)))

(defsubst espresso--forward-pstate ()
  (espresso--forward-text-property 'espresso--pstate))

(defsubst espresso--backward-pstate ()
  (espresso--backward-text-property 'espresso--pstate))

(defun espresso--re-search-forward-inner (regexp &optional bound count)
  "Auxiliary function for `espresso--re-search-forward'."
  (let ((parse)
        str-terminator
        (orig-macro-end (save-excursion
                          (when (espresso--beginning-of-macro)
                            (c-end-of-macro)
                            (point)))))
    (while (> count 0)
      (re-search-forward regexp bound)
      (setq parse (syntax-ppss))
      (cond ((setq str-terminator (nth 3 parse))
             (when (eq str-terminator t)
               (setq str-terminator ?/))
             (re-search-forward
              (concat "\\([^\\]\\|^\\)" (string str-terminator))
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
             (setq count (1- count))))))
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
        str-terminator
        (orig-macro-start
         (save-excursion
           (and (espresso--beginning-of-macro)
                (point)))))
    (while (> count 0)
      (re-search-backward regexp bound)
      (when (and (> (point) (point-min))
                 (save-excursion (backward-char) (looking-at "/[/*]")))
        (forward-char))
      (setq parse (syntax-ppss))
      (cond ((setq str-terminator (nth 3 parse))
             (when (eq str-terminator t)
               (setq str-terminator ?/))
             (re-search-backward
              (concat "\\([^\\]\\|^\\)" (string str-terminator))
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
  "Move forward over a function declaration with point at the
'function' keyword. Return no-nil if this is a
syntactically-correct non-expression function, nil otherwise.
Specifically, return the name of the function, or t if the name
could not be determined."
  (assert (looking-at "\\_<function\\_>"))
  (let ((name t))
    (forward-word)
    (forward-comment most-positive-fixnum)
    (when (looking-at espresso--name-re)
      (setq name (match-string-no-properties 0))
      (goto-char (match-end 0)))
    (forward-comment most-positive-fixnum)
    (and (eq (char-after) ?\( )
         (ignore-errors (forward-list) t)
         (progn (forward-comment most-positive-fixnum)
                (and (eq (char-after) ?{)
                     name)))))

(defun espresso--function-prologue-beginning (&optional pos)
  "Return the start of the function prologue that contains POS,
or nil if we're not in a function prologue. A function prologue
is everything from the start of the 'function' keyword up to and
including the opening brace. POS defaults to point."

  (save-excursion
    (let ((pos (or pos (point))))
      (skip-syntax-backward "w_")
      (and (or (looking-at "\\_<function\\_>")
               (espresso--re-search-backward "\\_<function\\_>" nil t))

           (save-match-data (goto-char (match-beginning 0))
                            (espresso--forward-function-decl))

           (<= pos (point))
           (match-beginning 0)))))

(defun espresso--beginning-of-defun-raw ()
  "Internal helper for espresso--beginning-of-defun. Go to
previous defun-beginning and return the parse state for it, or
nil if we went all the way back to bob and don't find anything."
  (espresso--ensure-cache)
  (let (pstate)
    (while (and (setq pstate (espresso--backward-pstate))
                (not (eq 'function (espresso--pitem-type (car pstate))))))
    (and (not (bobp)) pstate)))

(defun espresso--pstate-is-toplevel-defun (pstate)
  "If PSTATE represents a non-empty top-level defun, return the
top-most pitem. Otherwise, return nil."
  (loop for pitem in pstate
        with func-depth = 0
        with func-pitem
        if (eq 'function (espresso--pitem-type pitem))
        do (incf func-depth)
        and do (setq func-pitem pitem)
        finally return (if (eq func-depth 1) func-pitem)))

(defun espresso--beginning-of-defun-nested ()
  "Internal helper for espresso--beginning-of-defun"

  (or
   ;; Look for the smallest function that encloses point...
   (loop for pitem in (espresso--parse-state-at-point)
         if (and (eq 'function (espresso--pitem-type pitem))
                 (espresso--inside-pitem-p pitem))
         do (goto-char (espresso--pitem-h-begin pitem))
         and return t)

   ;; ...and if that isn't found, look for the previous top-level
   ;; defun
   (loop for pstate = (espresso--backward-pstate)
         while pstate
         if (espresso--pstate-is-toplevel-defun pstate)
         do (goto-char (espresso--pitem-h-begin it))
         and return t)))

(defun espresso--beginning-of-defun-flat ()
  "Internal helper for espresso--beginning-of-defun"

  (let ((pstate (espresso--beginning-of-defun-raw)))
    (when pstate
      (goto-char (espresso--pitem-h-begin (car pstate))))))

(defun espresso--beginning-of-defun ()
  "Used as beginning-of-defun-function"

  ;; If we're just past the end of a function, the user probably wants
  ;; to go to the beginning of *that* function
  (when (eq (char-before) ?})
    (backward-char))

  (let ((prologue-begin (espresso--function-prologue-beginning)))
    (cond ((and prologue-begin (< prologue-begin (point)))
           (goto-char prologue-begin))

          (espresso-flat-functions
           (espresso--beginning-of-defun-flat))
          (t
           (espresso--beginning-of-defun-nested)))))

(defun espresso--flush-caches (&optional beg ignored)
  "Flush syntax cache info after position BEG. BEG defaults to
point-min, flushing the entire cache."
  (interactive)
  (setq beg (or beg (save-restriction (widen) (point-min))))
  (setq espresso--cache-end (min espresso--cache-end beg)))

(defmacro espresso--debug (&rest arguments)
  ;; `(message ,@arguments)
  )

(defun espresso--ensure-cache--pop-if-ended (open-items paren-depth)
  (let ((top-item (car open-items)))
    (when (<= paren-depth (espresso--pitem-paren-depth top-item))
      (assert (not (get-text-property (1- (point)) 'espresso-pend)))
      (put-text-property (1- (point)) (point) 'espresso--pend top-item)
      (setf (espresso--pitem-b-end top-item) (point))
      (setq open-items
            ;; open-items must contain at least two items for this to
            ;; work, but because we push a dummy item to start with,
            ;; that assumption holds.
            (cons (espresso--pitem-add-child (second open-items) top-item)
                  (cddr open-items)))))

  open-items)

(defmacro espresso--ensure-cache--update-parse ()
  "Helper for use inside espresso--ensure-cache. Updates parsing
information up to point. Refers to parse, prev-parse-point,
goal-point, and open-items bound lexically in the body of
`espresso--ensure-cache'."
  `(progn
     (setq goal-point (point))
     (goto-char prev-parse-point)
     (while (progn
              (setq open-items (espresso--ensure-cache--pop-if-ended
                                open-items (car parse)))
              ;; Make sure parse-partial-sexp doesn't stop because we *entered*
              ;; the given depth -- i.e., make sure we're deeper than the target
              ;; depth.
              (assert (> (nth 0 parse)
                         (espresso--pitem-paren-depth (car open-items))))
              (setq parse (parse-partial-sexp
                           prev-parse-point goal-point
                           (espresso--pitem-paren-depth (car open-items))
                           nil parse))

              (let ((overlay (make-overlay prev-parse-point (point))))
                (overlay-put overlay 'face '(:background "red"))
                (unwind-protect
;;                     (progn
;;                       (espresso--debug "parsed: %S" parse)
;;                       (sit-for 1))
                  (delete-overlay overlay)))

              (setq prev-parse-point (point))
              (< (point) goal-point)))

     (setq open-items (espresso--ensure-cache--pop-if-ended
                       open-items (car parse)))))

(defun espresso--show-cache-at-point ()
  (interactive)
  (require 'pp)
  (let ((prop (get-text-property (point) 'espresso--pstate)))
    (with-output-to-temp-buffer "*Help*"
      (pp prop))))

(defun espresso--split-name (string)
  "Splits a name into its dot-separated parts. Also removes any
prototype parts from the split name (unless the name is just
\"prototype\" to start with, that is)."
  (let ((name (save-match-data
                (split-string string "\\." t))))
    (unless (and (= (length name) 1)
                 (equal (car name) "prototype"))

      (setq name (remove "prototype" name)))))

(defun espresso--guess-function-name (position)
  "Guess the name of the function at POSITION, which should be
the just after the end of the word 'function'. Return the name of
the function or nil if the name could not be guessed. Clobbers
match data."

  (save-excursion
    (goto-char position)
    (forward-line 0)
    (cond
     ((looking-at espresso--function-heading-3-re)
      (and (eq (match-end 0) position)
           (match-string-no-properties 1)))

     ((looking-at espresso--function-heading-2-re)
      (and (eq (match-end 0) position)
           (match-string-no-properties 1))))))

(defun espresso--clear-stale-cache ()
  ;; Clear any endings that occur after point
  (let (end-prop)
    (save-excursion
      (while (setq end-prop (espresso--forward-text-property
                             'espreso--pend))
        (setf (espresso--pitem-b-end end-prop) nil))))

  ;; Remove any cache properties after this point
  (remove-text-properties (point) (point-max)
                          '(espresso--pstate t espresso--pend t)))

(defun espresso--ensure-cache (&optional limit stop-on-end)
  "Ensures brace cache is valid up to the character before LIMIT.
LIMIT defaults to point. If STOP-ON-END is non-nil, it must be a
pitem instance, and espresso--ensure-cache will return when the
given item ends instead of parsing all the way to LIMIT."
  (setq limit (or limit (point)))
  (when (< espresso--cache-end limit)

    (c-save-buffer-state
        (open-items
         orig-match-start
         orig-match-end
         orig-depth
         parse
         prev-parse-point
         name
         case-fold-search
         filtered-class-styles
         new-item
         goal-point
         end-prop)

      ;; Figure out which class styles we need to look for
      (setq filtered-class-styles
            (loop for style in espresso--class-styles
                  if (memq (plist-get style :framework)
                           espresso-enabled-frameworks)
                  collect style))

      (save-excursion
        (save-restriction
          (widen)

          ;; Find last known good position
          (goto-char espresso--cache-end)
          (unless (bobp)
            (setq open-items (get-text-property
                              (1- (point)) 'espresso--pstate))

            (unless open-items
              (goto-char (previous-single-property-change
                          (point) 'espresso--pstate nil (point-min)))

              (unless (bobp)
                (setq open-items (get-text-property (1- (point))
                                                    'espresso--pstate))
                (assert open-items))))

          (unless open-items
            ;; Make a placeholder for the top-level definition
            (setq open-items (list espresso--initial-pitem)))

          (setq parse (syntax-ppss))
          (setq prev-parse-point (point))

          (espresso--clear-stale-cache)

          (narrow-to-region (point-min) limit)

          (loop while (re-search-forward espresso--quick-match-re-func nil t)
                for orig-match-start = (goto-char (match-beginning 0))
                for orig-match-end = (match-end 0)
                do (espresso--ensure-cache--update-parse)
                for orig-depth = (nth 0 parse)

                ;; Each of these conditions should return non-nil if
                ;; we should add a new item and leave point at the end
                ;; of the new item's header (h-end in the
                ;; espresso--pitem diagram). This point is the one
                ;; after the last character nweed to unambiguously
                ;; detect this construct. If one of these evaluates to
                ;; nil, the location of the point is ignored.
                if (cond
                    ;; In comment or string
                    ((nth 8 parse) nil)

                    ;; Regular function declaration
                    ((and (looking-at "\\_<function\\_>")
                          (setq name (espresso--forward-function-decl)))

                     (when (eq name t)
                       (setq name (or (espresso--guess-function-name
                                       orig-match-end)
                                      t)))

                     ;; Guess function name if we don't have one from
                     ;; the function preamble
                     (when (and (eq name t)
                                (save-excursion
                                  (goto-char orig-match-end)
                                  (forward-line 0)
                                  (looking-at
                                   espresso--function-heading-2-re)))

                       (setq name (match-string-no-properties 1)))

                     (assert (eq (char-after) ?{))
                     (forward-char)
                     (make-espresso--pitem
                      :paren-depth orig-depth
                      :h-begin orig-match-start
                      :type 'function
                      :name (if (eq name t)
                                name
                              (espresso--split-name name))))

                    ;; Macro
                    ((looking-at espresso--macro-decl-re)

                     ;; Macros often contain unbalanced parentheses.
                     ;; Make sure that h-end is at the textual end of
                     ;; the macro no matter what the parenthesis say.
                     (c-end-of-macro)
                     (espresso--ensure-cache--update-parse)

                     (make-espresso--pitem
                      :paren-depth (nth 0 parse)
                      :h-begin orig-match-start
                      :type 'macro
                      :name (list (match-string-no-properties 1))))

                    ;; "Prototype function" declaration
                    ((looking-at espresso--plain-method-re)
                     (goto-char (match-beginning 3))
                     (when (save-match-data
                             (espresso--forward-function-decl))
                       (forward-char)
                       (make-espresso--pitem
                        :paren-depth orig-depth
                        :h-begin orig-match-start
                        :type 'function
                        :name (nconc (espresso--split-name
                                      (match-string-no-properties 1))
                                     (list (match-string-no-properties 2))))))

                    ;; Class definition
                    ((loop with syntactic-context =
                           (espresso--syntactic-context-from-pstate open-items)
                           for class-style in filtered-class-styles
                           if (and (memq syntactic-context
                                         (plist-get class-style :contexts))
                                   (looking-at (plist-get class-style
                                                          :class-decl)))
                           do (goto-char (match-end 0))
                           and return
                           (make-espresso--pitem
                            :paren-depth orig-depth
                            :h-begin orig-match-start
                            :type class-style
                            :name (espresso--split-name
                                   (match-string-no-properties 1))))))

                do (espresso--ensure-cache--update-parse)
                and do (push it open-items)
                and do (put-text-property
                        (1- (point)) (point) 'espresso--pstate open-items)
                else do (goto-char orig-match-end))

          (goto-char limit)
          (espresso--ensure-cache--update-parse)
          (setq espresso--cache-end limit)
          (setq espresso--last-parse-pos limit)
          (setq espresso--state-at-last-parse-pos open-items)
          )))))

(defun espresso--end-of-defun-flat ()
  "Internal helper for espresso--end-of-defun"
  (loop while (espresso--re-search-forward "}" nil t)
        do (espresso--ensure-cache)
        if (get-text-property (1- (point)) 'espresso--pend)
        if (eq 'function (espresso--pitem-type it))
        return t
        finally do (goto-char (point-max))))

(defun espresso--end-of-defun-nested ()
  "Internal helper for espresso--end-of-defun"
  (let ((this-end (save-excursion
                    (and (espresso--beginning-of-defun-nested)
                         (progn (espresso--forward-function-decl)
                                (forward-list)))))
        found)

    (if (and this-end (< (point) this-end))
        ;; We're already inside a function; just go to its end.
        (goto-char this-end)

      ;; Otherwise, go to the end of the next function...
      (while (and (espresso--re-search-forward "\\_<function\\_>" nil t)
                  (not (setq found (progn
                                     (goto-char (match-beginning 0))
                                     (espresso--forward-function-decl))))))

      (if found (forward-list)
        ;; ... or eob.
        (goto-char (point-max))))))

(defun espresso--end-of-defun ()
  "Used as end-of-defun-function"
  ;; look for function backward. if we're inside it, go to that
  ;; function's end. otherwise, search for the next function's end and
  ;; go there
  (if espresso-flat-functions
      (espresso--end-of-defun-flat)

  (let ((prologue-begin (espresso--function-prologue-beginning)))
    (cond ((and prologue-begin (<= prologue-begin (point)))
           (goto-char prologue-begin)
           (espresso--forward-function-decl)
           (forward-list))

          (t (espresso--end-of-defun-nested))))))

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
    (when lim (narrow-to-region (point-min) lim))
    (let ((pos (point)))
      (while (progn
               (forward-comment most-positive-fixnum)
               (when (eq (char-after) ?#)
                 (c-end-of-macro))
               (/= (point)
                   (prog1
                       pos
                     (setq pos (point)))))))))

(defun espresso--up-nearby-list ()
  "Like (up-list -1), but only considers lists that end nearby"
  (save-restriction
    ;; Look at a very small region so our compuation time doesn't
    ;; explode in pathological cases.
    (narrow-to-region (max (point-min) (- (point) 500)) (point))
    (up-list -1)))

(defun espresso--inside-param-list-p ()
  "Return non-nil iff point is inside a function parameter list."
  (ignore-errors
    (save-excursion
      (espresso--up-nearby-list)
      (and (looking-at "(")
           (progn (forward-symbol -1)
                  (or (looking-at "function")
                      (progn (forward-symbol -1)
                             (looking-at "function"))))))))

(defun espresso--inside-dojo-class-list-p ()
  "Return non-nil iff point is inside a Dojo multiple-inheritance
class block."
  (ignore-errors
    (save-excursion
      (espresso--up-nearby-list)
      (let ((list-begin (point)))
        (forward-line 0)
        (and (looking-at espresso--dojo-class-decl-re)
             (goto-char (match-end 0))
             (looking-at "\"\\s-*,\\s-*\\[")
             (eq (match-end 0) (1+ list-begin)))))))

(defun espresso--syntax-begin-function ()
  (when (< espresso--cache-end (point))
    (goto-char (max (point-min) espresso--cache-end)))

  (let ((pitem))
    (while (and (setq pitem (car (espresso--backward-pstate)))
                (not (eq 0 (espresso--pitem-paren-depth pitem)))))

    (when pitem
      (goto-char (espresso--pitem-h-begin pitem )))))

;;; Font Lock
(defun espresso--make-framework-matcher (framework &rest regexps)
  "Create a byte-compiled function that only matches the given
regular expressions (that concatenation of REGEXPS) if FRAMEWORK
is in espresso-enabled-frameworks"

  (setq regexps (apply #'concat regexps))
  (byte-compile
   `(lambda (limit)
      (when (memq (quote ,framework) espresso-enabled-frameworks)
        (re-search-forward ,regexps limit t)))))

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

    ("\\.\\(prototype\\)\\_>"
     (1 font-lock-constant-face))

    ;; Highlights class being declared, in parts
    (espresso--class-decl-matcher
     ,(concat "\\(" espresso--name-re "\\)\\(?:\\.\\|.*$\\)")
     (goto-char (match-beginning 1))
     nil
     (1 font-lock-type-face))

    ;; Highlights parent class, in parts, if available
    (espresso--class-decl-matcher
     ,(concat "\\(" espresso--name-re "\\)\\(?:\\.\\|.*$\\)")
     (goto-char (or (match-beginning 2) (point-at-eol)))
     nil
     (1 font-lock-type-face))

    ;; Highlights parent class
    (espresso--class-decl-matcher
     (2 font-lock-type-face nil t))

    ;; Dojo needs its own matcher to override the string highlighting
    (,(espresso--make-framework-matcher
       'dojo
       "^\\s-*dojo\\.declare\\s-*(\""
       "\\(" espresso--dotted-name-re "\\)"
       "\\(?:\"\\s-*,\\s-*\\(" espresso--dotted-name-re "\\)\\)?")
     (1 font-lock-type-face t)
     (2 font-lock-type-face nil t))

    ;; Match Dojo base classes. Of course Mojo has to be different
    ;; from everything else under the sun...
    (,(espresso--make-framework-matcher
       'dojo
       "^\\s-*dojo\\.declare\\s-*(\""
       "\\(" espresso--dotted-name-re "\\)\"\\s-*,\\s-*\\[")
     ,(concat "[[,]\\s-*\\(" espresso--dotted-name-re "\\)\\s-*"
              "\\(?:\\].*$\\)?")
     (backward-char)
     (end-of-line)
     (1 font-lock-type-face))

    ;; continued Dojo base-class list
    (,(espresso--make-framework-matcher
       'dojo
       "^\\s-*" espresso--dotted-name-re "\\s-*[],]")
     ,(concat "\\(" espresso--dotted-name-re "\\)"
              "\\s-*\\(?:\\].*$\\)?")
     (if (save-excursion (backward-char)
                         (espresso--inside-dojo-class-list-p))
         (forward-symbol -1)
       (end-of-line))
     (end-of-line)
     (1 font-lock-type-face))

    ;; variable declarations
    ,(list
      (concat "\\_<\\(const\\|var\\|let\\)\\_>\\|" espresso--basic-type-re)
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
            '(0 font-lock-variable-name-face))))

  "Level three font lock.")

(defun espresso--inside-pitem-p (pitem)
  "Return whether point is inside the given pitem's header or body"
  (espresso--ensure-cache)
  (assert (espresso--pitem-h-begin pitem))
  (assert (espresso--pitem-paren-depth pitem))

  (and (> (point) (espresso--pitem-h-begin pitem))
       (or (null (espresso--pitem-b-end pitem))
           (> (espresso--pitem-b-end pitem) (point)))))

(defun espresso--parse-state-at-point ()
  "Get a list of espresso--pitem instances that apply to point,
most specific first. In the worst case, the current toplevel
instance will be returned."

  (save-excursion
    (save-restriction
      (widen)
      (espresso--ensure-cache)
      (let* ((bound (if (eobp) (point) (1+ (point))))
             (pstate (or (save-excursion
                           (espresso--backward-pstate))
                         (list espresso--initial-pitem))))

        ;; Loop until we either hit a pitem at BOB or pitem ends after
        ;; point (or at point if we're at eob)
        (loop for pitem = (car pstate)
              until (or (eq (espresso--pitem-type pitem)
                            'toplevel)
                        (espresso--inside-pitem-p pitem))
              do (pop pstate))

        pstate))))

(defun espresso--syntactic-context-from-pstate (pstate)
  "Return the syntactic context corresponding to PSTATE"
  (let ((type (espresso--pitem-type (car pstate))))
    (cond ((memq type '(function macro))
           type)

          ((consp type)
           'class)

          (t 'toplevel))))

(defun espresso-syntactic-context ()
  "Get the current syntactic context of point. When called
interatively, also display a message with that context."
  (interactive)
  (let* ((syntactic-context (espresso--syntactic-context-from-pstate
                             (espresso--parse-state-at-point))))

    (when (interactive-p)
      (message "Syntactic context: %s" syntactic-context))

    syntactic-context))

(defun espresso--class-decl-matcher (limit)
  "Fontifies according to espresso--class-styles"
  (loop initially (espresso--ensure-cache limit)
        while (re-search-forward espresso--quick-match-re limit t)
        for orig-end = (match-end 0)
        do (goto-char (match-beginning 0))
        if (loop for style in espresso--class-styles
                 for decl-re = (plist-get style :class-decl)
                 if (and (memq (plist-get style :framework)
                               espresso-enabled-frameworks)
                         (memq (espresso-syntactic-context)
                               (plist-get style :contexts))
                         decl-re
                         (looking-at decl-re))
                 do (goto-char (match-end 0))
                 and return t)
        return t
        else do (goto-char orig-end)))

(defconst espresso--font-lock-keywords
  '(espresso--font-lock-keywords-3 espresso--font-lock-keywords-1
                                   espresso--font-lock-keywords-2
                                   espresso--font-lock-keywords-3)
  "See `font-lock-keywords'.")

;; XXX: Javascript can continue a regexp literal across lines so long
;; as the newline is escaped with \. Account for that in the regexp
;; below.
(defconst espresso--regexp-literal
  "[=(,:]\\(?:\\s-\\|\n\\)*\\(/\\)[^/*]\\(?:.*?[^\\]\\)?\\(/\\)"
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

;;; Type database and Imenu

;; We maintain a cache of semantic information, i.e., the classes and
;; functions we've encountered so far. In order to avoid having to
;; re-parse the buffer on every change, we cache the parse state at
;; each interesting point in the buffer. Each parse state is a
;; modified copy of the previous one, or in the case of the first
;; parse state, the empty state.
;;
;; The parse state itself is just a stack of espresso--pitem
;; instances. It starts off containing one element that is never
;; closed, that is initially espresso--initial-pitem.
;;


(defun espresso--pitem-format (pitem)
  (let ((name (espresso--pitem-name pitem))
        (type (espresso--pitem-type pitem)))

    (format "name:%S type:%S"
            name
            (if (atom type)
                type
              (plist-get type :name)))))

(defun espresso--make-merged-item (item child name-parts)
  "Internal helper for espresso--splice-into-items. Return a new
item that is the result of merging CHILD into ITEM. NAME-PARTS is
a list of parts of the name of CHILD that we haven't consumed
yet."

  (espresso--debug "espresso--make-merged-item: {%s} into {%s}"
                   (espresso--pitem-format child)
                   (espresso--pitem-format item))

  ;; If the item we're merging into isn't a class, make it into one
  (unless (consp (espresso--pitem-type item))
    (espresso--debug "espresso--make-merged-item: changing dest into class")
    (setq item (make-espresso--pitem
                :children (list item)

                ;; Use the child's class-style if it's available
                :type (if (atom (espresso--pitem-type child))
                          espresso--dummy-class-style
                  (espresso--pitem-type child))

                :name (espresso--pitem-strname item))))

  ;; Now we can merge either a function or a class into a class
  (cons (cond
         ((cdr name-parts)
          (espresso--debug "espresso--make-merged-item: recursing")
          ;; if we have more name-parts to go before we get to the
          ;; bottom of the class hierarchy, call the merger
          ;; recursively
          (espresso--splice-into-items (car item) child
                                       (cdr name-parts)))

         ((atom (espresso--pitem-type child))
          (espresso--debug "espresso--make-merged-item: straight merge")
          ;; Not merging a class, but something else, so just prepend
          ;; it
          (cons child (car item)))

         (t
          ;; Otherwise, merge the new child's items into those
          ;; of the new class
          (espresso--debug "espresso--make-merged-item: merging class contents")
          (append (car child) (car item))))
        (cdr item)))

(defun espresso--pitem-strname (pitem)
  "Last part of the name of PITEM as a string or symbol"
  (let ((name (espresso--pitem-name pitem)))
    (if (consp name)
        (car (last name))
      name)))

(defun espresso--splice-into-items (items child name-parts)
  "Non-destructively inserts CHILD into the item list ITEMS in
the proper place as given by NAME-PARTS. If a class doesn't
exist in the tree, create it. Return the new items list.
NAME-PARTS is a list of strings given the broken-down class
name of the item to insert."

  (let ((top-name (car name-parts))
        (item-ptr items)
        new-items last-new-item new-cons item)

    (espresso--debug "espresso--splice-into-items: name-parts: %S items:%S"
             name-parts
             (mapcar #'espresso--pitem-name items))

    (assert (stringp top-name))
    (assert (> (length top-name) 0))

    ;; If top-name isn't found in items, then we build a copy of items
    ;; and throw it away. But that's okay, since most of the time, we
    ;; *will* find an instance.

    (while (and item-ptr
                (cond ((equal (espresso--pitem-strname (car item-ptr)) top-name)
                       ;; Okay, we found an entry with the right name. Splice
                       ;; the merged item into the list...
                       (setq new-cons (cons (espresso--make-merged-item
                                             (car item-ptr) child
                                             name-parts)
                                            (cdr item-ptr)))

                       (if last-new-item
                           (setcdr last-new-item new-cons)
                         (setq new-items new-cons))

                       ;; ...and terminate the loop
                       nil)

                      (t
                       ;; Otherwise, copy the current cons and move onto the
                       ;; text. This is tricky; we keep track of the tail of
                       ;; the list that begins with new-items in
                       ;; last-new-item.
                       (setq new-cons (cons (car item-ptr) nil))
                       (if last-new-item
                           (setcdr last-new-item new-cons)
                         (setq new-items new-cons))
                       (setq last-new-item new-cons)

                       ;; Go to the next cell in items
                       (setq item-ptr (cdr item-ptr))))))

    (if item-ptr
        ;; Yay! We stopped because we found something, not because
        ;; we ran out of items to search. Just return the new
        ;; list.
        (progn
          (espresso--debug "search succeeded: %S" name-parts)
          new-items)

      ;; We didn't find anything. If the child is a class and we don't
      ;; have any classes to drill down into, just push that class;
      ;; otherwise, make a fake class and carry on.
      (espresso--debug "search failed: %S" name-parts)
      (cons (if (cdr name-parts)
                ;; We have name-parts left to process. Make a fake
                ;; class for this particular part...
                (make-espresso--pitem
                 ;; ...and recursively digest the rest of the name
                 :children (espresso--splice-into-items
                            nil child (cdr name-parts))
                 :type espresso--dummy-class-style
                 :name top-name)

              ;; Otherwise, this is the only name we have, so stick
              ;; the item on the front of the list
              child)
            items))))

(defun espresso--pitem-add-child (pitem child)
  "Copy espresso--pitem PITEM and while copying, push CHILD onto
its list of children."

  (assert (integerp (espresso--pitem-h-begin child)))
  (assert (if (consp (espresso--pitem-name child))
              (loop for part in (espresso--pitem-name child)
                    always (stringp part))
            t))

  ;; This trick works because we know (based on our defstructs) that
  ;; the child list is always the first element, and so the second
  ;; element and beyond can be shared when we make our "copy".
  (cons

   (let ((name (espresso--pitem-name child))
         (type (espresso--pitem-type child)))

     (cond ((cdr-safe name) ; true if a list of at least two elements
            ;; Use slow path because we need class lookup
            (espresso--splice-into-items (car pitem) child name))

           ((and (consp type)
                 (plist-get type :prototype))

            ;; Use slow path because we need class merging. We know
            ;; name is a list here because down in
            ;; `espresso--ensure-cache', we made sure to only add
            ;; class entries with lists for :name
            (assert (consp name))
            (espresso--splice-into-items (car pitem) child name))

           (t
            ;; Fast path
            (cons child (car pitem)))))

   (cdr pitem)))


(defun espresso--pitems-to-imenu (pitems unknown-ctr)
  "Convert list of pitems PITEMS to imenu format"

  (let (imenu-items pitem pitem-type pitem-name subitems)

    (while (setq pitem (pop pitems))
      (setq pitem-type (espresso--pitem-type pitem))
      (setq pitem-name (espresso--pitem-strname pitem))
      (when (eq pitem-name t)
        (setq pitem-name (format "[unknown %s]"
                                 (incf (car unknown-ctr)))))

      (cond
       ((memq pitem-type '(function macro))
        (assert (integerp (espresso--pitem-h-begin pitem)))
        (push (cons pitem-name
                    (espresso--pitem-h-begin pitem))
              imenu-items))

       ((consp pitem-type)
        (setq subitems (espresso--pitems-to-imenu
                        (espresso--pitem-children pitem)
                        unknown-ctr))
        (cond (subitems
               (push (cons pitem-name subitems)
                     imenu-items))

              ((espresso--pitem-h-begin pitem)
               (assert (integerp (espresso--pitem-h-begin pitem)))
               (setq subitems (list
                               (cons "[empty]"
                                     (set-marker
                                      (make-marker)
                                      (espresso--pitem-h-begin pitem)))))
               (push (cons pitem-name subitems)
                     imenu-items))))

       (t (error "Unknown item type: %S" pitem-type))))

    imenu-items))

(defun espresso--imenu-create-index ()
  "Creates an imenu index for the current buffer"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (espresso--ensure-cache)
      (assert (or (= (point-min) (point-max))
                  (eq espresso--last-parse-pos (point))))
      (when espresso--last-parse-pos
        (let ((state espresso--state-at-last-parse-pos)
              (unknown-ctr (cons -1 nil)))

          ;; Make sure everything is closed
          (while (cdr state)
            (setq state
                  (cons (espresso--pitem-add-child (second state) (car state))
                        (cddr state))))

          (assert (= (length state) 1))

          ;; Convert the new-finalized state into what imenu expects
          (espresso--pitems-to-imenu
           (car (espresso--pitem-children state))
           unknown-ctr))))))

(defun espresso--which-func-joiner (parts)
  (mapconcat #'identity parts "."))

;;; MozRepl integration

(require 'moz nil t)
(require 'json nil t)

(put 'espresso-moz-bad-rpc 'error-conditions '(error timeout))
(put 'espresso-moz-bad-rpc 'error-message "Mozilla RPC Error")

(put 'espresso-js-error 'error-conditions '(error js-error))
(put 'espresso-js-error 'error-message "Javascript Error")

(defun espresso--wait-for-matching-output (process regexp timeout)
  "Wait TIMEOUT seconds for PROCESS to output something that
matches REGEXP. On timeout, return nil. On success, return t with
match data set."
  (with-current-buffer (process-buffer process)
    (loop with start-pos = (marker-position (process-mark process))
          with end-time = (+ (float-time) timeout)
          for time-left = (- end-time (float-time))
          while (> time-left 0)
          do (accept-process-output process time-left nil t)
          do (goto-char (process-mark process))
          if (looking-back regexp start-pos)
          return t
          finally do (signal 'espresso-moz-bad-rpc
                             (list (format "No output matching %S" regexp))))))

(defstruct espresso--js-handle
  ;; Integer, mirrors the value we see in JS
  (id nil :read-only t)

  ;; Process to which this thing belongs, or nil if this is a special,
  ;; immortal handle
  (process nil))

(defvar espresso--js-references nil
  "Maps Elisp Javascript proxy objects to their Javascript IDs.")

(defvar espresso--js-process nil
  "The last MozRepl process object we saw")

(defvar espresso--js-gc-idle-timer nil
  "Idle timer for cleaning up JS object references")

(defvar espresso--js-last-gcs-done nil
  "Last number of gcs we GCed with")

(defconst espresso--js-global
  (make-espresso--js-handle :id -1)
  "Handle of the current Javascript global object. Never appears
as the value of an expression: i.e., you can send this to
Javascript, but you will never receive it in reply.")

(defconst espresso--js-null
  (make-espresso--js-handle :id 0)
  "Handle of Javascript null")

(defconst espresso--js-true
  (make-espresso--js-handle :id 1)
  "Handle of Javascript true")

(defconst espresso--js-false
  (make-espresso--js-handle :id 2)
  "Handle of Javascript false")

(defconst espresso--js-undefined
  (make-espresso--js-handle :id 3)
  "Handle of Javascript undefined")

(defconst espresso--js-getprop
  (make-espresso--js-handle :id 4)
  "Handle of the getprop pseudo-function")

(defconst espresso--js-putprop
  (make-espresso--js-handle :id 5)
  "Handle of the putprop pseudo-function")

(defconst espresso--js-delprop
  (make-espresso--js-handle :id 6)
  "Handle of the elprop pseudo-function")

(defconst espresso--js-typeof
  (make-espresso--js-handle :id 7)
  "Handle of the typeof pseudo-function")

(defconst espresso--js-repl
  (make-espresso--js-handle :id 8)
  "Handle of the REPL itself")

(defconst espresso--js-new
  (make-espresso--js-handle :id 9)
  "Handle to the new pseudo-func")

(defun espresso--make-handle-hash ()
  "Make a Javascript handle table"
  (loop with table = (make-hash-table :test 'eq :weakness t)
        for special-value in (list espresso--js-null
                                   espresso--js-true
                                   espresso--js-false
                                   espresso--js-undefined
                                   espresso--js-getprop
                                   espresso--js-putprop
                                   espresso--js-delprop
                                   espresso--js-typeof
                                   espresso--js-repl
                                   espresso--js-new)
        do (puthash (espresso--js-handle-id special-value)
                    special-value table)
        finally return table))

(defconst espresso--moz-interactor
  (replace-regexp-in-string
   "[ \n]+" " "
"(function(repl) {
  repl.defineInteractor('espresso', {
    onStart: function onStart(repl) {
      if(!repl._espressoObjects) {
        repl._espressoObjects = ({
          /*  -1: current global object, */
          0: null,
          1: true,
          2: false,
          3: undefined,
          4: this._getProp,
          5: this._putProp,
          6: this._delProp,
          7: this._typeOf,
          8: repl,
          9: this._callNew});
        repl._espressoLastID = 9;
        repl._espressoGC = this._espressoGC;
      }
    },

    _espressoGC: function _espressoGC(ids_in_use) {
      var objects = this._espressoObjects;
      var keys = [];
      var num_freed = 0;

      for(var pn in objects) {
        keys.push(Number(pn));
      }

      keys.sort(function(x, y) x - y);
      ids_in_use.sort(function(x, y) x - y);
      var i = 0;
      var j = 0;

      while(i < ids_in_use.length && j < keys.length) {
        var id = ids_in_use[i++];
        while(j < keys.length && keys[j] !== id) {
          var k_id = keys[j++];
          delete objects[k_id];
          ++num_freed;
        }
        ++j;
      }

      while(j < keys.length) {
        var k_id = keys[j++];
        delete objects[k_id];
        ++num_freed;
      }

      return num_freed;
    },

    _getProp: function _getProp(propname) {
      return this[propname];
    },

    _putProp: function _putProp(propname, value) {
      this[propname] = value;
    },

    _delProp: function _delProp(propname) {
      delete this[propname];
    },

    _typeOf: function _typeOf(thing) {
      return typeof thing;
    },

    _callNew: function(constructor) {
      var s = 'new constructor(';
      for(var i = 1; i < arguments.length; ++i) {
        if(i != 1) {
          s += ',';
        }

        s += 'arguments[' + i + ']';
      }

      s += ')';
      return eval(s);
    },

    getPrompt: function getPrompt(repl) {
      return 'EVAL>'
    },

    _lookupObject: function _lookupObject(repl, id) {
      if(id === -1) { return window; }
      var ret = repl._espressoObjects[id];
      if(ret === undefined && id !== 3) {
        throw new Error('No object with id:' + id + '(' + typeof id + ')');
      }
      return ret;
    },

    _findOrAllocateObject: function _findOrAllocateObject(repl, value) {
      for(var id in repl._espressoObjects) {
        id = Number(id);
        var obj = repl._espressoObjects[id];
        if(obj === value) {
          return id;
        }
      }

      var id = ++repl._espressoLastID;
      repl._espressoObjects[id] = value;
      return id;
    },

    _fixupList: function fixupList(repl, list) {
      for(var i = 0; i < list.length; ++i) {
        if(list[i] instanceof Array) {
          this._fixupList(repl, list[i]);
        } else if(typeof list[i] === 'object') {
          list[i] = this._lookupObject(repl, list[i].objid);
        }
      }
    },

    handleInput: function handleInput(repl, input) {
      try {
        var parts = eval(input);
        this._fixupList(repl, parts);
        var value = parts[0].apply(parts[1], parts.slice(2));
        if(value === null || value === true || value === false ||
          value === undefined || typeof value === 'object' |
          typeof value === 'function')
        {
          var ret = ['objid', this._findOrAllocateObject(repl, value) ];
        } else {
          var ret = ['atom', value ];
        }
      } catch(x) {
        var ret = ['error', x.toString() ];
      }

      var JSON = Components.classes['@mozilla.org/dom/json;1'].createInstance(Components.interfaces.nsIJSON);
      repl.print(JSON.encode(ret));
      repl.popInteractor();
      repl._prompt();
    }
  });
})
")

  "String to set MozRepl up into a simple-minded evaluation mode")

(defun espresso--js-encode-value (x)
  "Marshall the given value for JS. Strings and numbers get
JSON-encoded. Lists (including nil) are made into Javascript
array literals and their contents encoded with
espresso--js-encode-value."

  (cond ((stringp x) (json-encode-string x))
        ((numberp x) (json-encode-number x))
        ((espresso--js-handle-p x)

         ;; Check that the handle hasn't expired
         (and (espresso--js-handle-process x)
              (not (eq (espresso--js-handle-process x)
                       (inferior-moz-process)))
              (error "Stale JS handle"))

         (format "{objid:%s}" (espresso--js-handle-id x)))

        ((listp x)
         (concat
          "[" (mapconcat #'espresso--js-encode-value x ",") "]"))

        (t
         (error "Unrecognized item: %S" x))))

(defconst espresso--js-prompt-regexp
  "repl[0-9]*> $")

(defun espresso--js-funcall-raw (function thisp arguments)
  "Call the Mozilla function FUNCTION with this set to THISP and
the rest of the given arguments."

  (inferior-moz-process) ; Called for side-effect
  (let ((argstr (espresso--js-encode-value
                 (append (list function thisp)
                         arguments))))

    (with-current-buffer inferior-moz-buffer
      (goto-char (point-max))

      (unless (eq (inferior-moz-process) espresso--js-process)
        ;; Do some initialization the first time we see a process
        (setq espresso--js-process (inferior-moz-process))
        (setq espresso--js-references (espresso--make-handle-hash))

        ;; Send interactor
        (comint-send-string espresso--js-process
                            espresso--moz-interactor)
        (comint-send-string espresso--js-process
                            (concat "(" moz-repl-name ");\n"))
        (espresso--wait-for-matching-output
         (inferior-moz-process) espresso--js-prompt-regexp 10))

      ;; Mark next line to be sent as special
      (insert "repl.pushInteractor('espresso');")
      (comint-send-input nil t)
      (espresso--wait-for-matching-output
       (inferior-moz-process) "EVAL>$" 10)

      ;; Actual funcall
      (insert argstr)
      (comint-send-input nil t)
      (espresso--wait-for-matching-output
       (inferior-moz-process) espresso--js-prompt-regexp 30)
      (goto-char comint-last-input-end)

      ;; Read the result
      (let* ((json-array-type 'list)
             (result (prog1 (json-read)
                       (goto-char (point-max)))))
        (ecase (intern (first result))
          (atom (second result))
          (objid
           (or (gethash (second result)
                        espresso--js-references)
               (puthash (second result)
                        (make-espresso--js-handle
                         :id (second result)
                         :process (inferior-moz-process))
                        espresso--js-references)))

          (error (signal 'espresso-js-error (list (second result))))))
      )))

(defun espresso--js-new (constructor &rest arguments)
  "Call CONSTRUCTOR as a constructor (as with new), with the
given arguments ARGUMENTS. CONSTRUCTOR is a JS handle, a string,
or a list of these things."

  (espresso--js-funcall-raw espresso--js-new espresso--js-null
                            (cons (espresso--js-get constructor)
                                  arguments)))

(defun espresso--js-funcall (function &rest arguments)
  "Call the function identified by FUNCTION with the given
ARGUMENTS. FUNCTION is either a Javascript handle, in which case
it is called directly, a string that is looked up on the global
object, or a list of properties as for `espresso--js-get'. Except
in the third case, the `this' value will be null. In the third
case, the `this' value will be the second-to-list value looked
up."

  (let ((thisp espresso--js-null))
    (cond ((espresso--js-handle-p function))
          ((stringp function)
           (setq function (espresso--js-get function)))
          ((null (cdr function))
           (setq function (espresso--js-get (car function))))
          (t
           (setq thisp (apply #'espresso--js-get
                              (butlast function 1)))
           (setq function (espresso--js-get thisp
                                            (car (last function))))))

    (espresso--js-funcall-raw function thisp arguments)))


(defun espresso--js-get (object &rest props)
  "Get the given property of the Javascript object OBJECT. If
more than one property is listed, the property lookups are
chained. If OBJECT is a string or number, treat OBJECT and all
subsequent PROPS as a lookup against the global object. "

  (when (or (stringp object) (numberp object))
    (setq props (cons object props))
    (setq object espresso--js-global))

  (assert (loop for prop in props
                always (or (stringp prop) (numberp prop))))
  (assert (espresso--js-handle-p object))

  (if props
      (let ((result (espresso--js-funcall-raw
                     espresso--js-getprop object
                     (list (car props)))))

        (if (cdr props)
            (apply #'espresso--js-get result (cdr props))
          result))
    object))

(defun espresso-js-gc (&optional force)
  "Tell the repl about any objects we don't reference anymore.
With argument, run even if no intervening GC has happened."
  (interactive)

  (when force
    (setq espresso--js-last-gcs-done nil))

  (let ((this-gcs-done gcs-done) keys num)
    (when (and espresso--js-references
               (boundp 'inferior-moz-buffer)
               (buffer-live-p inferior-moz-buffer)
               ;; Don't bother running unless we've had an intervening
               ;; garbage collection; without a gc, nothing is deleted
               ;; from the weak hash table, so it's pointless telling
               ;; MozRepl about that references we still hold
               (not (eq espresso--js-last-gcs-done this-gcs-done))

               ;; Are we looking at a normal prompt? Make sure not to
               ;; interrupt the user if he's doing something
               (with-current-buffer inferior-moz-buffer
                 (save-excursion
                   (goto-char (point-max))
                   (looking-back espresso--js-prompt-regexp
                                 (save-excursion (forward-line 0) (point))))))

      (setq keys (loop for x being the hash-keys
                       of espresso--js-references
                       collect x))
      (setq num (espresso--js-funcall
                 (list espresso--js-repl "_espressoGC")
                 keys))

      (setq espresso--js-last-gcs-done this-gcs-done)
      (when (interactive-p)
        (message "Cleaned %s entries" num))

      num)))

(run-with-idle-timer 30 t #'espresso-js-gc)

(defun espresso-js-eval (js)
  "Evaluate the Javascript contained in JS and return the
JSON-decoded result"
  (interactive "MJavascript to evaluate: ")
  (let ((result (espresso--js-funcall "eval" js)))
    (when (interactive-p)
      (message "%s" (espresso--js-funcall "String" result)))
    result))

(defmacro espresso--with-js-context (&rest forms)
  "Execute FORMS in a new MozRepl context"
  `(progn
     (espresso--js-funcall (list espresso--js-repl "enter")
                           (espresso--js-new "Object"))
     (unwind-protect
         (progn ,@forms)
       (espresso--js-funcall (list espresso--js-repl "back")))))

(defun espresso--get-tabs ()
  "Enumerate all the contexts available"

  (loop with window-mediator = (espresso--js-funcall
                                '("Components" "classes"
                                  "@mozilla.org/appshell/window-mediator;1"
                                  "getService")
                                (espresso--js-get "Components"
                                                  "interfaces"
                                                  "nsIWindowMediator"))

        with enumerator = (espresso--js-funcall
                           `(,window-mediator "getEnumerator")
                           espresso--js-null)

        while (eq (espresso--js-funcall `(,enumerator "hasMoreElements"))
                  espresso--js-true)
        for window = (espresso--js-funcall `(,enumerator "getNext"))
        collect window

        for gbrowser = (espresso--js-get window "gBrowser")
        if (espresso--js-handle-p gbrowser)
        nconc (loop for x below (espresso--js-get
                                 gbrowser "browsers" "length")
                    collect (espresso--js-get
                             gbrowser "browsers" x "currentURI" "spec"))
        )

  )

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

  ;; Parse cache
  (add-hook 'before-change-functions #'espresso--flush-caches t t)

  ;; Frameworks
  (espresso--update-quick-match-re)

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

  (set (make-local-variable 'syntax-begin-function)
       #'espresso--syntax-begin-function)

  ;; Important to fontify the whole buffer syntactically! If we don't,
  ;; then we might have regular expression literals that aren't marked
  ;; as strings, which will screw up parse-partial-sexp, scan-lists,
  ;; etc. and and produce maddening "unbalanced parenthesis" errors.
  ;; When we attempt to find the error and scroll to the portion of
  ;; the buffer containing the problem, JIT-lock will apply the
  ;; correct syntax to the regular expresion literal and the problem
  ;; will mysteriously disappear.
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
