;;; espresso.el --- Major mode for editing JavaScript source text
;; Copyright (C) 2008 Free Software Foundation, Inc.
;; Copyright (C) 2009 Daniel Colascione <dan.colascione@gmail.com>
;; Author: Karl Landstrom <karl.landstrom@brgeight.se>
;; Author: Daniel Colascione <dan.colascione@gmail.com>
;; Maintainer: Daniel Colascione <dan.colascione@gmail.com>
;; Version: 9
;; Date: 2009-07-25
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
;; comments, C preprocessor fontification, and MozRepl integration.
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

(eval-and-compile
  (require 'cc-mode)
  (require 'font-lock)
  (require 'newcomment)
  (require 'imenu)
  (require 'etags)
  (require 'thingatpt)
  (require 'easymenu)
  (require 'moz nil t)
  (require 'json nil t))

(eval-when-compile
  (require 'cl)
  (require 'comint)
  (require 'ido)
  (proclaim '(optimize (speed 0) (safety 3))))

;;; Constants

(defconst espresso--name-start-re "[a-zA-Z_$]"
  "Matches the first character of a Espresso identifier. No grouping")

(defconst espresso--stmt-delim-chars "^;{}?:")

(defconst espresso--name-re (concat espresso--name-start-re
                                    "\\(?:\\s_\\|\\sw\\)*")
  "Matches a Javascript identifier. No grouping.")

(defconst espresso--objfield-re (concat espresso--name-re ":")
  "Matches a Javascript object field start")

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
     "yield" "volatile" "while" "with"))
  "Regular expression matching any JavaScript keyword.")

(defconst espresso--basic-type-re
  (espresso--regexp-opt-symbol
   '("boolean" "byte" "char" "double" "float" "int" "long"
     "short" "void"))
  "Regular expression matching any predefined type in JavaScript.")

(defconst espresso--constant-re
  (espresso--regexp-opt-symbol '("false" "null" "undefined"
                                 "Infinity" "NaN"
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
                (list "\\_<for\\_>"
                      "\\s-+\\(each\\)\\_>" nil nil
                      (list 1 'font-lock-keyword-face))
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

(defcustom espresso-comment-lineup-func #'c-lineup-C-comments
  "cc-mode-style lineup function for C comments"
  :type 'function
  :group 'espresso)

(defcustom espresso-enabled-frameworks espresso--available-frameworks
  "Select which frameworks espresso-mode will recognize.

Turn off some frameworks you seldom use to improve performance.
The set of recognized frameworks can also be overriden on a
per-buffer basis."
  :type (cons 'set (mapcar (lambda (x)
                             (list 'const x))
                           espresso--available-frameworks))
  :group 'espresso)

(defcustom espresso-js-switch-tabs
  (and (memq system-type '(darwin)) t)
  "Non-nil if Emacs should display tabs while selecting them.
  Useful only if the windowing system has a good mechanism for
  preventing Firefox from stealing the keyboard focus."
  :type 'boolean
  :group 'espresso)

(defcustom espresso-js-tmpdir
  "~/.emacs.d/espresso/js"
  "Temporary directory used for communicating with Mozilla. It
  must be readable and writable by both Mozilla and Emacs."
  :type 'directory
  :group 'espresso)

(defcustom espresso-js-timeout 5
  "Wait this many seconds for a reply from Mozilla when executing
commands. Increase this value if you are getting timeout
messages."
  :type 'integer
  :group 'espresso)

;;; KeyMap

(defvar espresso-mode-map
  (let ((keymap (make-sparse-keymap)))
    (mapc (lambda (key)
            (define-key keymap key #'espresso-insert-and-indent))
          '("+" "-" "*" "{" "}" "(" ")" ":" ";" ","))
    (define-key keymap [(control ?c) (meta ?:)] #'espresso-js-eval)
    (define-key keymap [(control ?c) (control ?j)] #'espresso-set-js-context)
    (define-key keymap [(control meta ?x)] #'espresso-eval-defun)
    (define-key keymap [(meta ?.)] #'espresso-find-symbol)

    (easy-menu-define nil keymap "Espresso Menu"
      '("Javascript"
        ["Select new Mozilla context…" espresso-set-js-context
         (fboundp #'inferior-moz-process)]
        ["Evaluate expression in Mozilla context…" espresso-js-eval
         (fboundp #'inferior-moz-process)]
        ["Send current function to Mozilla…" espresso-eval-defun
         (fboundp #'inferior-moz-process)]
        )
      )

    keymap)
  "Keymap for espresso-mode")

(defun espresso-insert-and-indent (key)
  "Runs the command bound to KEY in the global keymap, and if
we're not in a string or comment, indents the current line."
  (interactive (list (this-command-keys)))
  (call-interactively (lookup-key (current-global-map) key))
  (let ((syntax (save-restriction (widen) (syntax-ppss))))
    (when (or (and (not (nth 8 syntax))
                   espresso-auto-indent-flag)
              (and (nth 4 syntax)
                   (eq (current-column)
                       (1+ (current-indentation)))))

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

(defun espresso--pitem-goto-h-end (pitem)
  (goto-char (espresso--pitem-h-begin pitem))
  (espresso--forward-pstate))

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

(defun espresso--forward-expression ()
  "Move forward over a whole expression. Doesn't move over
expressions continued across lines, but we don't actually care"
  (loop
   ;; non-continued case; simplistic, but good enough?
   do (loop until (or (eolp)
                      (progn
                        (forward-comment most-positive-fixnum)
                        (memq (char-after) '(?\, ?\; ?\] ?\) ?\}))))
            do (forward-sexp))

   while (and (eq (char-after) ?\n)
              (save-excursion
                (forward-char)
                (espresso--continued-expression-p)))))

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
is everything from start of the definition up to and including
the opening brace. POS defaults to point."

  (let (prologue-begin)
    (save-excursion
      (if pos
          (goto-char pos)
        (setq pos (point)))

      (when (save-excursion
              (forward-line 0)
              (or (looking-at espresso--function-heading-2-re)
                  (looking-at espresso--function-heading-3-re)))

        (setq prologue-begin (match-beginning 1))
        (when (<= prologue-begin pos)
          (goto-char (match-end 0))))

      (skip-syntax-backward "w_")
      (and (or (looking-at "\\_<function\\_>")
               (espresso--re-search-backward "\\_<function\\_>" nil t))

           (save-match-data (goto-char (match-beginning 0))
                            (espresso--forward-function-decl))

           (<= pos (point))
           (or prologue-begin (match-beginning 0))))))

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
  "Internal helper for espresso--beginning-of-defun. Returns the
pitem of the function we went to the beginning of."

  (or
   ;; Look for the smallest function that encloses point...
   (loop for pitem in (espresso--parse-state-at-point)
         if (and (eq 'function (espresso--pitem-type pitem))
                 (espresso--inside-pitem-p pitem))
         do (goto-char (espresso--pitem-h-begin pitem))
         and return pitem)

   ;; ...and if that isn't found, look for the previous top-level
   ;; defun
   (loop for pstate = (espresso--backward-pstate)
         while pstate
         if (espresso--pstate-is-toplevel-defun pstate)
         do (goto-char (espresso--pitem-h-begin it))
         and return it)))

(defun espresso--beginning-of-defun-flat ()
  "Internal helper for espresso--beginning-of-defun"

  (let ((pstate (espresso--beginning-of-defun-raw)))
    (when pstate
      (goto-char (espresso--pitem-h-begin (car pstate))))))

(defun espresso--beginning-of-defun (&optional arg)
  "Used as beginning-of-defun-function"

  (setq arg (or arg 1))
  (while (and (not (eobp)) (< arg 0))
    (incf arg)
    (when (and (not espresso-flat-functions)
               (or (eq (espresso-syntactic-context) 'function)
                   (espresso--function-prologue-beginning)))
      (espresso--end-of-defun))

    (if (espresso--re-search-forward
         "\\_<function\\_>" nil t)
        (goto-char (espresso--function-prologue-beginning))
      (goto-char (point-max))))

  (while (> arg 0)
    (decf arg)
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
             (espresso--beginning-of-defun-nested))))))

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

;;              (let ((overlay (make-overlay prev-parse-point (point))))
;;                (overlay-put overlay 'face '(:background "red"))
;;                (unwind-protect
;;                     (progn
;;                       (espresso--debug "parsed: %S" parse)
;;                       (sit-for 1))
;;                  (delete-overlay overlay)))

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

(defvar espresso--guess-function-name-start nil
  "Secondary out-variable for espresso--guess-function-name")

(defun espresso--guess-function-name (position)
  "Guess the name of the function at POSITION, which should be
the just after the end of the word 'function'. Return the name of
the function or nil if the name could not be guessed. Clobbers
match data. If in guessing the function name we find the preamble
begins earlier than expected, set `espresso--guess-function-name-start'
to that position, otherwise set that variable to nil."

  (setq espresso--guess-function-name-start nil)
  (save-excursion
    (goto-char position)
    (forward-line 0)
    (cond
     ((looking-at espresso--function-heading-3-re)
      (and (eq (match-end 0) position)
           (setq espresso--guess-function-name-start (match-beginning 1))
           (match-string-no-properties 1)))

     ((looking-at espresso--function-heading-2-re)
      (and (eq (match-end 0) position)
           (setq espresso--guess-function-name-start (match-beginning 1))
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
                       (setq name (espresso--guess-function-name orig-match-end))
                       (if name
                           (when espresso--guess-function-name-start
                             (setq orig-match-start
                                   espresso--guess-function-name-start))

                         (setq name t)))

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
  (message "test")
  (let* (pitem
         (this-end (save-excursion
                     (and (setq pitem (espresso--beginning-of-defun-nested))
                          (espresso--pitem-goto-h-end pitem)
                          (progn (backward-char)
                                 (forward-list)
                                 (point)))))
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

(defun espresso--end-of-defun (&optional arg)
  "Used as end-of-defun-function"
  (setq arg (or arg 1))
  (while (and (not (bobp)) (< arg 0))
    (let (orig-pos (point))
      (incf arg)
      (espresso--beginning-of-defun)
      (espresso--beginning-of-defun)
      (unless (bobp)
        (espresso--end-of-defun))))

  (while (> arg 0)
    (decf arg)
    ;; look for function backward. if we're inside it, go to that
    ;; function's end. otherwise, search for the next function's end and
    ;; go there
    (if espresso-flat-functions
        (espresso--end-of-defun-flat)

      ;; if we're doing nested functions, see whether we're in the
      ;; prologue. If we are, go to the end of the function; otherwise,
      ;; call espresso--end-of-defun-nested to do the real work
      (let ((prologue-begin (espresso--function-prologue-beginning)))
        (cond ((and prologue-begin (<= prologue-begin (point)))
               (goto-char prologue-begin)
               (re-search-forward "\\_<function")
               (goto-char (match-beginning 0))
               (espresso--forward-function-decl)
               (forward-list))

              (t (espresso--end-of-defun-nested)))))))

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

(defvar espresso--tmp-location nil)
(make-variable-buffer-local 'espresso--tmp-location)

(defun espresso--forward-destructuring-spec (&optional func)
  "Move forward over a destructuring spec. If FUNC is supplied,
call it with no arguments before every variable name in the spec.
Return true iff this was actually a spec. FUNC must preserve the
match data."

  (case (char-after)
    (?\[
     (forward-char)
     (while
         (progn
           (forward-comment most-positive-fixnum)
           (cond ((memq (char-after) '(?\[ ?\{))
                  (espresso--forward-destructuring-spec func))

                 ((eq (char-after) ?,)
                  (forward-char)
                  t)

                 ((looking-at espresso--name-re)
                  (and func (funcall func))
                  (goto-char (match-end 0))
                  t))))
     (when (eq (char-after) ?\])
       (forward-char)
       t))

    (?\{
     (forward-char)
     (forward-comment most-positive-fixnum)
     (while
         (when (looking-at espresso--objfield-re)
           (goto-char (match-end 0))
           (forward-comment most-positive-fixnum)
           (and (cond ((memq (char-after) '(?\[ ?\{))
                       (espresso--forward-destructuring-spec func))
                      ((looking-at espresso--name-re)
                       (and func (funcall func))
                       (goto-char (match-end 0))
                       t))
                (progn (forward-comment most-positive-fixnum)
                       (when (eq (char-after) ?\,)
                         (forward-char)
                         (forward-comment most-positive-fixnum)
                         t)))))
     (when (eq (char-after) ?\})
       (forward-char)
       t))))

(defun espresso--variable-decl-matcher (limit)
  "Font-lock matcher for variable names in a variable
declaration. This is a cc-mode-style matcher that *always* fails,
from the point of view of font-lock. It applies highlighting
directly with `font-lock-apply-higlight'."

  (condition-case nil
      (save-restriction
        (narrow-to-region (point-min) limit)

        (let ((first t))
          (forward-comment most-positive-fixnum)
          (while
              (and (or first
                       (when (eq (char-after) ?,)
                         (forward-char)
                         (forward-comment most-positive-fixnum)
                         t))
                   (cond ((looking-at espresso--name-re)
                          (font-lock-apply-highlight
                           '(0 font-lock-variable-name-face))
                          (goto-char (match-end 0)))

                         ((save-excursion
                            (espresso--forward-destructuring-spec))

                          (espresso--forward-destructuring-spec
                           (lambda ()
                             (font-lock-apply-highlight
                              '(0 font-lock-variable-name-face)))))))

            (forward-comment most-positive-fixnum)
            (when (eq (char-after) ?=)
              (forward-char)
              (espresso--forward-expression)
              (forward-comment most-positive-fixnum))

            (setq first nil))))

    ;; Conditions to handle
    (scan-error nil)
    (end-of-buffer nil))

  ;; Matcher always "fails"
  nil)

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
     (if (match-beginning 2)
         (progn
           (setq espresso--tmp-location (match-end 2))
           (goto-char espresso--tmp-location)
           (insert "=")
           (goto-char (match-beginning 2)))
       (setq espresso--tmp-location nil)
       (goto-char (point-at-eol)))
     (when espresso--tmp-location
       (save-excursion
         (goto-char espresso--tmp-location)
         (delete-char 1)))
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
      (list #'espresso--variable-decl-matcher nil nil nil))

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
  "[=(,:]\\(?:\\s-\\|\n\\)*\\(/\\)\\(?:\\\\/\\|[^/*]\\)\\(?:\\\\/\\|[^/]\\)*\\(/\\)"
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
   '("catch" "do" "else" "finally" "for" "if" "try" "while" "with"
     "each"))
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
            (and (not (eq (point-at-bol) (point-min)))
                 (not (looking-at "[{]"))
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

(defun espresso--get-c-offset (symbol anchor)
  (let ((c-offsets-alist
         (list (cons 'c espresso-comment-lineup-func))))
    (c-get-syntactic-indentation (list (cons symbol anchor)))))

(defun espresso--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)
           (espresso--get-c-offset 'c (nth 8 parse-status)))
          ((nth 8 parse-status) 0) ; inside string
          ((espresso--ctrl-statement-indentation))
          ((eq (char-after) ?#) 0)
          ((save-excursion (espresso--beginning-of-macro)) 4)
          ((nth 1 parse-status)
           (let ((same-indent-p (looking-at
                                 "[]})]\\|\\_<case\\_>\\|\\_<default\\_>"))
                 (continued-expr-p (espresso--continued-expression-p)))
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
               (current-column))))

          ((espresso--continued-expression-p)
           (+ espresso-indent-level espresso-expr-indent-offset))
          (t 0))))

(defun espresso-indent-line ()
  "Indent the current line as JavaScript source text."
  (interactive)
  (save-restriction
    (widen)
    (let* ((parse-status
            (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation))))

      (indent-line-to (espresso--proper-indentation parse-status))
      (when (> offset 0) (forward-char offset)))))

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

(defun espresso--maybe-make-marker (location)
  "Make LOCATION into a marker if imenu-use-markers"
  (if imenu-use-markers
      (set-marker (make-marker) location)
    location))

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
                    (espresso--maybe-make-marker
                     (espresso--pitem-h-begin pitem)))
              imenu-items))

       ((consp pitem-type) ; class definition
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
                                     (espresso--maybe-make-marker
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

(defun espresso--imenu-to-flat (items prefix symbols)
  (loop for item in items
        if (imenu--subalist-p item)
        do (espresso--imenu-to-flat
            (cdr item) (concat prefix (car item) ".")
            symbols)
        else
        do (let* ((name (concat prefix (car item)))
                  (name2 name)
                  (ctr 0))

             (while (gethash name2 symbols)
               (setq name2 (format "%s<%d>" name (incf ctr))))

             (puthash name2 (cdr item) symbols))))

(defun espresso--get-all-known-symbols ()
  "Get a hash table of all relevant Javascript symbols across all
espresso-mode buffers. Each key is the name of a symbol (possibly
disambiguated with <N>, where N > 1), and each value is a marker
giving the location of that symbol."
  (loop with symbols = (make-hash-table :test 'equal)
        with imenu-use-markers = t
        for buffer being the buffers
        for imenu-index = (with-current-buffer buffer
                            (when (eq major-mode 'espresso-mode)
                              (espresso--imenu-create-index)))
        do (espresso--imenu-to-flat imenu-index "" symbols)
        finally return symbols))

(defvar espresso--symbol-history nil
  "History of entered Javascript symbols")

(defun espresso--read-symbol (symbols-table prompt &optional initial-input)
  "Read a symbol from SYMBOLS-TABLE, which is a hash table like
the one from `espresso--get-all-known-symbols'. Return value is a
cons of (SYMBOL-NAME . LOCATION), where SYMBOL-NAME is a string
and LOCATION is a marker.

Prompt is PROMPT.

If INITIAL-INPUT is not nil, use it as the initial input"

  (unless ido-mode
    (ido-mode t)
    (ido-mode nil))

  (let ((choice (ido-completing-read
                 prompt
                 (loop for key being the hash-keys of symbols-table
                       collect key)
                 nil t initial-input 'espresso--symbol-history)))
    (cons choice (gethash choice symbols-table))))

(defun espresso--guess-symbol-at-point ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (save-excursion
        (goto-char (car bounds))
        (when (eq (char-before) ?.)
          (backward-char)
          (setf (car bounds) (point))))
      (buffer-substring (car bounds) (cdr bounds)))))

(defun espresso-find-symbol (&optional arg)
  "Jump to a Javascript symbol we read in from the user. With
prefix argument, restrict symbols to those from the current
buffer. Pushes a mark onto the tag ring just like `find-tag'."

  (interactive "P")
  (let (symbols marker)
    (if (not arg)
        (setq symbols (espresso--get-all-known-symbols))
      (setq symbols (make-hash-table :test 'equal))
      (espresso--imenu-to-flat (espresso--imenu-create-index)
                               "" symbols))

    (setq marker (cdr (espresso--read-symbol
                       symbols "Jump to: "
                       (espresso--guess-symbol-at-point))))

    (ring-insert find-tag-marker-ring (point-marker))
    (switch-to-buffer (marker-buffer marker))
    (push-mark)
    (goto-char marker)))

;;; MozRepl integration

(put 'espresso-moz-bad-rpc 'error-conditions '(error timeout))
(put 'espresso-moz-bad-rpc 'error-message "Mozilla RPC Error")

(put 'espresso-js-error 'error-conditions '(error js-error))
(put 'espresso-js-error 'error-message "Javascript Error")

(defun espresso--wait-for-matching-output
  (process regexp timeout &optional start)
  "Wait TIMEOUT seconds for PROCESS to output something that
matches REGEXP. On timeout, return nil. On success, return t with
match data set. If START is non-nil, look for output starting
from START. Otherwise, use the current value of `process-mark'."
  (with-current-buffer (process-buffer process)
    (loop with start-pos = (or start
                               (marker-position (process-mark process)))
          with end-time = (+ (float-time) timeout)
          for time-left = (- end-time (float-time))
          do (goto-char (point-max))
          if (looking-back regexp start-pos) return t
          while (> time-left 0)
          do (accept-process-output process time-left nil t)
          do (goto-char (process-mark process))
          finally do (signal
                      'espresso-moz-bad-rpc
                      (list (format "Timed out waiting for output matching %S" regexp))))))

(defstruct espresso--js-handle
  ;; Integer, mirrors the value we see in JS
  (id nil :read-only t)

  ;; Process to which this thing belongs
  (process nil :read-only t))

(defun espresso--js-handle-expired-p (x)
  (not (eq (espresso--js-handle-process x)
           (inferior-moz-process))))

(defvar espresso--js-references nil
  "Maps Elisp Javascript proxy objects to their Javascript IDs.")

(defvar espresso--js-process nil
  "The last MozRepl process object we saw")

(defvar espresso--js-gc-idle-timer nil
  "Idle timer for cleaning up JS object references")

(defvar espresso--js-last-gcs-done nil
  "Last number of gcs we GCed with")

(defconst espresso--moz-interactor
  (replace-regexp-in-string
   "[ \n]+" " "
   ; */" Make Emacs happy
"(function(repl) {
  repl.defineInteractor('espresso', {
    onStart: function onStart(repl) {
      if(!repl._espressoObjects) {
        repl._espressoObjects = {};
        repl._espressoLastID = 0;
        repl._espressoGC = this._espressoGC;
      }
      this._input = '';
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

    _mkArray: function _mkArray() {
      var result = [];
      for(var i = 0; i < arguments.length; ++i) {
        result.push(arguments[i]);
      }
      return result;
    },

    _parsePropDescriptor: function _parsePropDescriptor(parts) {
      if(typeof parts === 'string') {
        parts = [ parts ];
      }

      var obj = parts[0];
      var start = 1;

      if(typeof obj === 'string') {
        obj = window;
        start = 0;
      } else if(parts.length < 2) {
        throw new Error('expected at least 2 arguments');
      }

      for(var i = start; i < parts.length - 1; ++i) {
        obj = obj[parts[i]];
      }

      return [obj, parts[parts.length - 1]];
    },

    _getProp: function _getProp(/*...*/) {
      if(arguments.length === 0) {
        throw new Error('no arguments supplied to getprop');
      }

      if(arguments.length === 1 &&
         (typeof arguments[0]) !== 'string')
      {
        return arguments[0];
      }

      var [obj, propname] = this._parsePropDescriptor(arguments);
      return obj[propname];
    },

    _putProp: function _putProp(properties, value) {
      var [obj, propname] = this._parsePropDescriptor(properties);
      obj[propname] = value;
    },

    _delProp: function _delProp(propname) {
      var [obj, propname] = this._parsePropDescriptor(arguments);
      delete obj[propname];
    },

    _typeOf: function _typeOf(thing) {
      return typeof thing;
    },

    _callNew: function(constructor) {
      if(typeof constructor === 'string')
      {
        constructor = window[constructor];
      } else if(constructor.length === 1 &&
                typeof constructor[0] !== 'string')
      {
        constructor = constructor[0];
      } else {
        var [obj,propname] = this._parsePropDescriptor(constructor);
        constructor = obj[propname];
      }

      /* Hacky, but should be robust */
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

    _callEval: function(thisobj, js) {
      return eval.call(thisobj, js);
    },

    getPrompt: function getPrompt(repl) {
      return 'EVAL>'
    },

    _lookupObject: function _lookupObject(repl, id) {
      if(typeof id === 'string') {
        switch(id) {
        case 'global':
          return window;
        case 'nil':
          return null;
        case 't':
          return true;
        case 'false':
          return false;
        case 'undefined':
          return undefined;
        case 'repl':
          return repl;
        case 'interactor':
          return this;
        case 'NaN':
          return NaN;
        case 'Infinity':
          return Infinity;
        case '-Infinity':
          return -Infinity;
        default:
          throw new Error('No object with special id:' + id);
        }
      }

      var ret = repl._espressoObjects[id];
      if(ret === undefined) {
        throw new Error('No object with id:' + id + '(' + typeof id + ')');
      }
      return ret;
    },

    _findOrAllocateObject: function _findOrAllocateObject(repl, value) {
      if(typeof value !== 'object'  && typeof value !== 'function') {
        throw new Error('_findOrAllocateObject called on non-object('
                        + typeof(value) + '): '
                        + value)
      }

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

    _fixupList: function _fixupList(repl, list) {
      for(var i = 0; i < list.length; ++i) {
        if(list[i] instanceof Array) {
          this._fixupList(repl, list[i]);
        } else if(typeof list[i] === 'object') {
          var obj = list[i];
          if(obj.funcall) {
            var parts = obj.funcall;
            this._fixupList(repl, parts);
            var [thisobj, func] = this._parseFunc(parts[0]);
            list[i] = func.apply(thisobj, parts.slice(1));
          } else if(obj.objid) {
            list[i] = this._lookupObject(repl, obj.objid);
          } else {
            throw new Error('Unknown object type: ' + obj.toSource());
          }
        }
      }
    },

    _parseFunc: function(func) {
      var thisobj = null;

      if(typeof func === 'string') {
        func = window[func];
      } else if(func instanceof Array) {
        if(func.length === 1 && typeof func[0] !== 'string') {
          func = func[0];
        } else {
          [thisobj, func] = this._parsePropDescriptor(func);
          func = thisobj[func];
        }
      }

      return [thisobj,func];
    },

    _encodeReturn: function(value, array_as_mv) {
      var ret;

      if(value === null) {
        ret = ['special', 'null'];
      } else if(value === true) {
        ret = ['special', 'true'];
      } else if(value === false) {
        ret = ['special', 'false'];
      } else if(value === undefined) {
        ret = ['special', 'undefined'];
      } else if(typeof value === 'number') {
        if(isNaN(value)) {
          ret = ['special', 'NaN'];
        } else if(value === Infinity) {
          ret = ['special', 'Infinity'];
        } else if(value === -Infinity) {
          ret = ['special', '-Infinity'];
        } else {
          ret = ['atom', value];
        }
      } else if(typeof value === 'string') {
        ret = ['atom', value];
      } else if(array_as_mv && value instanceof Array) {
        ret = ['array', value.map(this._encodeReturn, this)];
      } else {
        ret = ['objid', this._findOrAllocateObject(repl, value)];
      }

      return ret;
    },

    _handleInputLine: function _handleInputLine(repl, line) {
      var ret;
      var array_as_mv = false;

      try {
        if(line[0] === '*') {
          array_as_mv = true;
          line = line.substring(1);
        }
        var parts = eval(line);
        this._fixupList(repl, parts);
        var [thisobj, func] = this._parseFunc(parts[0]);
        ret = this._encodeReturn(
          func.apply(thisobj, parts.slice(1)),
          array_as_mv);
      } catch(x) {
        ret = ['error', x.toString() ];
      }

      var JSON = Components.classes['@mozilla.org/dom/json;1'].createInstance(Components.interfaces.nsIJSON);
      repl.print(JSON.encode(ret));
      repl._prompt();
    },

    handleInput: function handleInput(repl, chunk) {
      this._input += chunk;
      var match, line;
      while(match = this._input.match(/.*\\n/)) {
        line = match[0];

        if(line === 'EXIT\\n') {
          repl.popInteractor();
          repl._prompt();
          return;
        }

        this._input = this._input.substring(line.length);
        this._handleInputLine(repl, line);
      }
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
        ((symbolp x) (format "{objid:%S}" (symbol-name x)))
        ((espresso--js-handle-p x)

         (when (espresso--js-handle-expired-p x)
           (error "Stale JS handle"))

         (format "{objid:%s}" (espresso--js-handle-id x)))

        ((sequencep x)
         (if (eq (car-safe x) 'espresso--funcall)
             (format "{funcall:[%s]}"
                     (mapconcat #'espresso--js-encode-value (cdr x) ","))
           (concat
            "[" (mapconcat #'espresso--js-encode-value x ",") "]")))
        (t
         (error "Unrecognized item: %S" x))))

(defconst espresso--js-prompt-regexp "\\(repl[0-9]*\\)> $")
(defconst espresso--js-repl-prompt-regexp "^EVAL>$")
(defvar espresso--js-repl-depth 0)

(defun espresso--js-wait-for-eval-prompt ()
  (espresso--wait-for-matching-output
   (inferior-moz-process)
   espresso--js-repl-prompt-regexp espresso-js-timeout

   ;; start matching against the beginning of the line in
   ;; order to catch a prompt that's only partially arrived
   (save-excursion (forward-line 0) (point))))

(defun espresso--js-enter-repl ()
  (inferior-moz-process) ; called for side-effect
  (with-current-buffer inferior-moz-buffer
    (goto-char (point-max))

    ;; Do some initialization the first time we see a process
    (unless (eq (inferior-moz-process) espresso--js-process)
      (setq espresso--js-process (inferior-moz-process))
      (setq espresso--js-references (make-hash-table :test 'eq :weakness t))
      (setq espresso--js-repl-depth 0)

      ;; Send interactor definition
      (comint-send-string espresso--js-process espresso--moz-interactor)
      (comint-send-string espresso--js-process
                          (concat "(" moz-repl-name ")\n"))
      (espresso--wait-for-matching-output
       (inferior-moz-process) espresso--js-prompt-regexp
       espresso-js-timeout))

    ;; Sanity check
    (when (looking-back espresso--js-prompt-regexp
                        (save-excursion (forward-line 0) (point)))
      (setq espresso--js-repl-depth 0))

    (if (> espresso--js-repl-depth 0)
        ;; If espresso--js-repl-depth > 0, we *should* be seeing an
        ;; EVAL> prompt. If we don't, give Mozilla a chance to catch
        ;; up with us.
        (espresso--js-wait-for-eval-prompt)

      ;; Otherwise, tell Mozilla to enter the interactor mode
      (insert (match-string-no-properties 1)
              ".pushInteractor('espresso')")
      (comint-send-input nil t)
      (espresso--wait-for-matching-output
       (inferior-moz-process) espresso--js-repl-prompt-regexp
       espresso-js-timeout))

    (incf espresso--js-repl-depth)))

(defun espresso--js-leave-repl ()
  (assert (> espresso--js-repl-depth 0))
  (when (= 0 (decf espresso--js-repl-depth))
    (with-current-buffer inferior-moz-buffer
      (goto-char (point-max))
      (espresso--js-wait-for-eval-prompt)
      (insert "EXIT")
      (comint-send-input nil t)
      (espresso--wait-for-matching-output
       (inferior-moz-process) espresso--js-prompt-regexp
       espresso-js-timeout))))

(defsubst espresso--js-not (value)
  (memq value '(nil false undefined)))

(defsubst espresso--js-true (value)
  (not (espresso--js-not value)))

(eval-and-compile
  (defun espresso--optimize-arglist (arglist)
    "Convert immediate js< and js! references to deferred ones"
    (loop for item in arglist
          if (eq (car-safe item) 'js<)
          collect (append (list 'list ''espresso--funcall
                                '(list 'interactor "_getProp"))
                          (espresso--optimize-arglist (cdr item)))
          else if (eq (car-safe item) 'js>)
          collect (append (list 'list ''espresso--funcall
                                '(list 'interactor "_putProp"))

                          (if (atom (cadr item))
                              (list (cadr item))
                            (list
                             (append
                              (list 'list ''espresso--funcall
                                    '(list 'interactor "_mkArray"))
                              (espresso--optimize-arglist (cadr item)))))
                          (espresso--optimize-arglist (cddr item)))
          else if (eq (car-safe item) 'js!)
          collect (destructuring-bind (ignored function &rest body) item
                    (append (list 'list ''espresso--funcall
                                  (if (consp function)
                                      (cons 'list
                                            (espresso--optimize-arglist function))
                                    function))
                            (espresso--optimize-arglist body)))
          else
          collect item)))

(defmacro espresso--js-get-service (class-name interface-name)
    `(js! ("Components" "classes" ,class-name "getService")
        (js< "Components" "interfaces" ,interface-name)))

(defmacro espresso--js-create-instance (class-name interface-name)
  `(js! ("Components" "classes" ,class-name "createInstance")
        (js< "Components" "interfaces" ,interface-name)))

(defmacro espresso--js-qi (object interface-name)
  `(js! (,object "QueryInterface")
        (js< "Components" "interfaces" ,interface-name)))

(defmacro with-espresso-js (&rest forms)
  "Runs FORMS with the Mozilla repl set up for espresso commands.
Inside the lexical scope of `with-espresso-js', `js?', `js!',
`js-new', `js-eval', `js-list', `js<', `js>', `js-get-service',
`js-create-instance', and `js-qi' are defined."

  `(progn
     (espresso--js-enter-repl)
     (unwind-protect
         (macrolet ((js? (&rest body) `(espresso--js-true ,@body))
                    (js! (function &rest body)
                         `(espresso--js-funcall
                           ,(if (consp function)
                                (cons 'list
                                      (espresso--optimize-arglist function))
                              function)
                           ,@(espresso--optimize-arglist body)))

                    (js-new (function &rest body)
                            `(espresso--js-new
                              ,(if (consp function)
                                   (cons 'list
                                         (espresso--optimize-arglist function))
                                 function)
                              ,@body))

                    (js-eval (thisobj js)
                            `(espresso--js-eval
                              ,@(espresso--optimize-arglist
                                 (list thisobj js))))

                    (js-list (&rest args)
                             `(espresso--js-list
                               ,@(espresso--optimize-arglist args)))

                    (js-get-service (&rest args)
                                    `(espresso--js-get-service
                                      ,@(espresso--optimize-arglist args)))

                    (js-create-instance (&rest args)
                                        `(espresso--js-create-instance
                                          ,@(espresso--optimize-arglist args)))

                    (js-qi (&rest args)
                           `(espresso--js-qi
                             ,@(espresso--optimize-arglist args)))

                    (js< (&rest body) `(espresso--js-get
                                        ,@(espresso--optimize-arglist body)))
                    (js> (props value)
                         `(espresso--js-funcall
                           '(interactor "_putProp")
                           ,(if (consp props)
                                (cons 'list
                                      (espresso--optimize-arglist props))
                              props)
                           ,@(espresso--optimize-arglist (list value))
                           ))
                    (js-handle? (arg) `(espresso--js-handle-p ,arg)))
           ,@forms)
       (espresso--js-leave-repl))))

(defvar espresso--js-array-as-list nil
"If bound to t and a function returns an Array, return the
elements of that Array as a list instead of returning the whole
Array as a JS symbol" )

(defun espresso--js-decode-retval (result)
  (ecase (intern (first result))
         (atom (second result))
         (special (intern (second result)))
         (array
          (mapcar #'espresso--js-decode-retval (second result)))
         (objid
          (or (gethash (second result)
                       espresso--js-references)
              (puthash (second result)
                       (make-espresso--js-handle
                        :id (second result)
                        :process (inferior-moz-process))
                       espresso--js-references)))

         (error (signal 'espresso-js-error (list (second result))))))

(defun espresso--js-funcall (function &rest arguments)
  "Call the Mozilla function FUNCTION with arguments ARGUMENTS.
If function is a string, look it up as a property on the global
object and use the global object for `this'; if FUNCTION is a
list with one element, use that element as the function with the
global object for `this', except that if that single element is a
string, look it up on the global object. If FUNCTION is a list
with more than one argument, use the list up to the last value as
a property descriptor and the last argument as a function."

  (with-espresso-js
   (let ((argstr (espresso--js-encode-value
                  (cons function arguments))))

     (with-current-buffer inferior-moz-buffer
       ;; Actual funcall
       (when espresso--js-array-as-list
         (insert "*"))
       (insert argstr)
       (comint-send-input nil t)
       (espresso--wait-for-matching-output
        (inferior-moz-process) "EVAL>"
        espresso-js-timeout)
       (goto-char comint-last-input-end)

       ;; Read the result
       (let* ((json-array-type 'list)
              (result (prog1 (json-read)
                        (goto-char (point-max)))))
         (espresso--js-decode-retval result))))))

(defun espresso--js-new (constructor &rest arguments)
  "Call CONSTRUCTOR as a constructor (as with new), with the
given arguments ARGUMENTS. CONSTRUCTOR is a JS handle, a string,
or a list of these things."

  (apply #'espresso--js-funcall
         '(interactor "_callNew")
         constructor arguments))

(defun espresso--js-eval (thisobj js)
  (espresso--js-funcall '(interactor "_callEval") thisobj js))

(defun espresso--js-list (&rest arguments)
  "Return a Lisp array that is the result of evaluating each of
  the elements of ARGUMENTS"

  (let ((espresso--js-array-as-list t))
    (apply #'espresso--js-funcall '(interactor "_mkArray")
           arguments)))

(defun espresso--js-get (&rest props)
  (apply #'espresso--js-funcall '(interactor "_getProp") props))

(defun espresso--js-put (props value)
  (espresso--js-funcall '(interactor "_putProp") props value))

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
      (setq num (espresso--js-funcall '(repl "_espressoGC") (or keys [])))

      (setq espresso--js-last-gcs-done this-gcs-done)
      (when (interactive-p)
        (message "Cleaned %s entries" num))

      num)))

(run-with-idle-timer 30 t #'espresso-js-gc)

(defun espresso-js-eval (js)
  "Evaluate the Javascript contained in JS and return the
JSON-decoded result. eval must be called using this function
because it is special to the Javascript interpreter."
  (interactive "MJavascript to evaluate: ")
  (with-espresso-js
   (let* ((content-window (espresso--js-content-window
                           (espresso--get-js-context)))
          (result (js-eval content-window js)))

     (when (interactive-p)
       (message "%s" (js! "String" result)))
     result)))

(defun espresso--get-tabs ()
  "Enumerate all the contexts available. Each one is a list:

   The list is (TITLE URL BROWSER TAB TABBROWSER) for content documents
   The list is (TITLE URL WINDOW) for windows

   All tabs of a given window are grouped together. The most
   recent window is first. Within each window, the tabs are
   returned left-to-right.
"
  (with-espresso-js
   (let (windows)

     (loop with window-mediator = (js! ("Components" "classes"
                                        "@mozilla.org/appshell/window-mediator;1"
                                        "getService")
                                       (js< "Components" "interfaces"
                                            "nsIWindowMediator"))
           with enumerator = (js! (window-mediator "getEnumerator") nil)

           while (js? (js! (enumerator "hasMoreElements")))
           for window = (js! (enumerator "getNext"))
           for window-info = (js-list window
                                      (js< window "document" "title")
                                      (js! (window "location" "toString"))
                                      (js< window "closed")
                                      (js< window "windowState"))

           unless (or (js? (fourth window-info))
                      (eq (fifth window-info) 2))
           do (push window-info windows))

     (loop for window-info in windows
           for window = (first window-info)
           collect (list (second window-info)
                         (third window-info)
                         window)

           for gbrowser = (js< window "gBrowser")
           if (js-handle? gbrowser)
           nconc (loop
                  for x below (js< gbrowser "browsers" "length")
                  collect (js-list (js< gbrowser
                                        "browsers"
                                        x
                                        "contentDocument"
                                        "title")

                                   (js! (gbrowser
                                         "browsers"
                                         x
                                         "contentWindow"
                                         "location"
                                         "toString"))
                                   (js< gbrowser
                                        "browsers"
                                        x)

                                   (js! (gbrowser
                                         "tabContainer"
                                         "childNodes"
                                         "item")
                                        x)

                                   gbrowser))))))

(defvar espresso-read-tab-history nil)

(defun espresso--read-tab (prompt)
  "Read a Mozilla tab with prompt PROMPT. Return a cons of (TYPE . OBJECT).
TYPE is either 'window or 'tab, and OBJECT is a Javascript handle
to a ChromeWindow or a browser, respectively."

  ;; Prime IDO
  (unless ido-mode
    (ido-mode t)
    (ido-mode nil))

  (with-espresso-js
   (lexical-let ((tabs (espresso--get-tabs)) selected-tab-cname
                 selected-tab prev-hitab)

     ;; Disambiguate names
     (setq tabs (loop with tab-names = (make-hash-table :test 'equal)
                      for tab in tabs
                      for cname = (format "%s (%s)" (second tab) (first tab))
                      for num = (incf (gethash cname tab-names -1))
                      if (> num 0)
                      do (setq cname (format "%s <%d>" cname num))
                      collect (cons cname tab)))

     (labels ((find-tab-by-cname
               (cname)
               (loop for tab in tabs
                     if (equal (car tab) cname)
                     return (cdr tab)))

              (mogrify-highlighting
               (hitab unhitab)

               ;; Hack to reduce the number of
               ;; round-trips to mozilla
               (let (cmds)
                 (cond
                  ;; Highlighting tab
                  ((fourth hitab)
                   (push '(js! ((fourth hitab) "setAttribute")
                               "style"
                               "color: red; font-weight: bold")
                         cmds)

                   ;; Highlight window proper
                   (push '(js! ((third hitab)
                                "setAttribute")
                               "style"
                               "border: 8px solid red")
                         cmds)

                   ;; Select tab, when appropriate
                   (when espresso-js-switch-tabs
                     (push
                      '(js> ((fifth hitab) "selectedTab") (fourth hitab))
                      cmds)))

                  ;; Hilighting whole window
                  ((third hitab)
                   (push '(js! ((third hitab) "document"
                                "documentElement" "setAttribute")
                               "style"
                               (concat "-moz-appearance: none;"
                                       "border: 8px solid red;"))
                         cmds)))

                 (cond
                  ;; Unhighlighting tab
                  ((fourth unhitab)
                   (push '(js! ((fourth unhitab) "setAttribute") "style" "")
                         cmds)
                   (push '(js! ((third unhitab) "setAttribute") "style" "")
                         cmds))

                  ;; Unhighlighting window
                  ((third unhitab)
                   (push '(js! ((third unhitab) "document"
                                "documentElement" "setAttribute")
                               "style" "")
                         cmds)))

                 (eval (list 'with-espresso-js
                             (cons 'js-list (nreverse cmds))))))

              (command-hook
               ()
               (let* ((tab (find-tab-by-cname (car ido-matches))))
                 (mogrify-highlighting tab prev-hitab)
                 (setq prev-hitab tab)))

              (setup-hook
               ()
               ;; Fiddle with the match list a bit: if our first match
               ;; is a tabbrowser window, rotate the match list until
               ;; the active tab comes up
               (let ((matched-tab (find-tab-by-cname (car ido-matches))))
                 (when (and matched-tab
                            (null (fourth matched-tab))
                            (equal "navigator:browser"
                                   (js! ((third matched-tab)
                                         "document"
                                         "documentElement"
                                         "getAttribute")
                                        "windowtype")))

                   (loop with tab-to-match = (js< (third matched-tab)
                                                  "gBrowser"
                                                  "selectedTab")

                         with index = 0
                         for match in ido-matches
                         for candidate-tab = (find-tab-by-cname match)
                         if (eq (fourth candidate-tab) tab-to-match)
                         do (setq ido-cur-list (ido-chop ido-cur-list match))
                         and return t)))

               (add-hook 'post-command-hook #'command-hook t t)))


       (unwind-protect
           (setq selected-tab-cname
                 (let ((ido-minibuffer-setup-hook
                        (cons #'setup-hook ido-minibuffer-setup-hook)))
                   (ido-completing-read
                    prompt
                    (mapcar #'car tabs)
                    nil t nil
                    'espresso-read-tab-history)))

         (when prev-hitab
           (mogrify-highlighting nil prev-hitab)
           (setq prev-hitab nil)))

       (add-to-history 'espresso-read-tab-history selected-tab-cname)

       (setq selected-tab (loop for tab in tabs
                                if (equal (car tab) selected-tab-cname)
                                return (cdr tab)))

       (if (fourth selected-tab)
           (cons 'browser (third selected-tab))
         (cons 'window (third selected-tab)))))))

(defun espresso--guess-eval-defun-info (pstate)
  "Internal helper for `espresso-eval-defun'. Returns a list (NAME . CLASSPARTS),
where CLASSPARTS is a list of strings making up the class name
and NAME is the name of the function part."
  (cond ((and (= (length pstate) 3)
              (eq (espresso--pitem-type (first pstate)) 'function)
              (= (length (espresso--pitem-name (first pstate))) 1)
              (consp (espresso--pitem-type (second pstate))))

         (append (espresso--pitem-name (second pstate))
                 (list (first (espresso--pitem-name (first pstate))))))

        ((and (= (length pstate) 2)
              (eq (espresso--pitem-type (first pstate)) 'function))

         (append
          (butlast (espresso--pitem-name (first pstate)))
          (list (car (last (espresso--pitem-name (first pstate)))))))

        (t (error "Function not a toplevel defun or class member"))))

(defvar espresso--js-context nil
  "The current JS context. This is a cons like the one returned
from `espresso--read-tab'. Change with
`espresso-set-js-context'.")

(defconst espresso--js-inserter
  "(function(func_info,func) {
    func_info.unshift('window');
    var obj = window;
    for(var i = 1; i < func_info.length - 1; ++i) {
      var next = obj[func_info[i]];
      if(typeof next !== 'object' && typeof next !== 'function') {
        next = obj.prototype && obj.prototype[func_info[i]];
        if(typeof next !== 'object' && typeof next !== 'function') {
          alert('Could not find ' + func_info.slice(0, i+1).join('.') +
                ' or ' + func_info.slice(0, i+1).join('.') + '.prototype');
          return;
        }

        func_info.splice(i+1, 0, 'prototype');
        ++i;
      }
    }

    obj[func_info[i]] = func;
    alert('Successfully updated '+func_info.join('.'));
  })")

(defun espresso-set-js-context (context)
  "Set the Javascript context to CONTEXT, reading from the user
if interactive"
  (interactive (list (espresso--read-tab "Javascript Context: ")))
  (setq espresso--js-context context))

(defun espresso--get-js-context ()
  "Return a valid JS context. If one hasn't been set, or if it's
stale, ask the user for a new one."

  (with-espresso-js
   (when (or (null espresso--js-context)
             (espresso--js-handle-expired-p (cdr espresso--js-context))
             (ecase (car espresso--js-context)
               (window (js? (js< (cdr espresso--js-context) "closed")))
               (browser (not (js? (js< (cdr espresso--js-context)
                                       "contentDocument"))))))
     (setq espresso--js-context (espresso--read-tab "Javascript Context: ")))

   espresso--js-context))

(defun espresso--js-content-window (context)
  (with-espresso-js
   (ecase (car context)
     (window (cdr context))
     (browser (js< (cdr context)
                   "contentWindow" "wrappedJSObject")))))

(defun espresso--make-nsilocalfile (path)
  (with-espresso-js
   (let ((file (js-create-instance "@mozilla.org/file/local;1"
                                   "nsILocalFile")))
     (js! (file "initWithPath") path)
     file)))

(defun espresso--js-add-resource-alias (alias path)
  (with-espresso-js
   (let* ((io-service (js-get-service "@mozilla.org/network/io-service;1"
                                                "nsIIOService"))
          (res-prot (js! (io-service "getProtocolHandler") "resource"))
          (res-prot (js-qi res-prot "nsIResProtocolHandler"))
          (path-file (espresso--make-nsilocalfile path))
          (path-uri (js! (io-service "newFileURI") path-file)))
     (js! (res-prot "setSubstitution") alias path-uri))))

(defun* espresso-eval-defun ()
  "Update some Mozilla tab with defun containing point or after
point"
  (interactive)

  ;; This function works by generating a temporary file that contains
  ;; the function we'd like to insert. We then use the elisp-js bridge
  ;; to command mozilla to load this file by inserting a script tag
  ;; into the document we set. This way, debuggers and such will have
  ;; a way to find the source of the just-inserted function.
  ;;
  ;; We delete the temporary file if there's an error, but otherwise
  ;; we add an unload event listener on the Mozilla side to delete the
  ;; file.

  (save-excursion
    (let (begin end pstate defun-info temp-name defun-body)
      (espresso--end-of-defun)
      (setq end (point))
      (espresso--ensure-cache)
      (espresso--beginning-of-defun)
      (re-search-forward "\\_<function\\_>")
      (setq begin (match-beginning 0))
      (setq pstate (espresso--forward-pstate))

      (when (or (null pstate)
                (> (point) end))
        (error "Could not locate function definition"))

      (setq defun-info (espresso--guess-eval-defun-info pstate))

      (let ((overlay (make-overlay begin end)))
        (overlay-put overlay 'face 'highlight)
        (unwind-protect
            (unless (y-or-n-p (format "Send %s to Mozilla? "
                                      (mapconcat #'identity defun-info ".")))
              (message "") ; question message lingers until next command
              (return-from espresso-eval-defun))
          (delete-overlay overlay)))

      (setq defun-body (buffer-substring-no-properties begin end))

      (make-directory espresso-js-tmpdir t)

      ;; (Re)register a Mozilla resource URL to point to the
      ;; temporary directory
      (espresso--js-add-resource-alias "espresso" espresso-js-tmpdir)

      (setq temp-name (make-temp-file (concat espresso-js-tmpdir
                                             "/espresso-")
                                      nil ".js"))
      (unwind-protect
          (with-espresso-js
            (with-temp-buffer
              (insert espresso--js-inserter)
              (insert "(")
              (insert (json-encode-list defun-info))
              (insert ",\n")
              (insert defun-body)
              (insert "\n)")
              (write-region (point-min) (point-max) temp-name
                            nil 1))

            ;; Give Mozilla responsibility for deleting this file
            (let* ((content-window (espresso--js-content-window
                                    (espresso--get-js-context)))
                   (content-document (js< content-window "document"))
                   (head (if (js? (js< content-document "body"))
                             ;; Regular content
                             (js< (js! (content-document "getElementsByTagName")
                                       "head")
                                  0)
                           ;; Chrome
                           (js< content-document "documentElement")))
                   (elem (js! (content-document "createElementNS")
                              "http://www.w3.org/1999/xhtml" "script")))

              (js! (elem "setAttribute") "type" "text/javascript")
              (js! (elem "setAttribute") "src"
                   (format "resource://espresso/%s"
                           (file-name-nondirectory temp-name)))

              (js! (head "appendChild") elem)

              (js! (content-window "addEventListener") "unload"
                   (js! ((js-new
                          "Function" "file"
                          "return function() { file.remove(false) }"))
                        (espresso--make-nsilocalfile temp-name))
                   'false)
              (setq temp-name nil)



              ))

        ;; temp-name is set to nil on success
        (when temp-name
          (delete-file temp-name))))))

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


(eval-after-load 'hideshow
  '(add-to-list 'hs-special-modes-alist
                '(espresso-mode "{" "}" "/[*/]"
                                nil hs-c-like-adjust-block-beginning)))

(eval-after-load 'folding
  '(when (fboundp 'folding-add-to-marks-list)
     (folding-add-to-marks-list 'espresso-mode "// {{{" "// }}}" )))

(eval-after-load 'speedbar
  '(speedbar-add-supported-extension ".js"))


;;; Emacs
(provide 'espresso)
;; Local Variables:
;; outline-regexp: ";;; "
;; End:
;; espresso.el ends here
