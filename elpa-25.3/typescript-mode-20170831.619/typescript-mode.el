;;; typescript-mode.el --- Major mode for editing typescript

;; -----------------------------------------------------------------------------------
;;     TypeScript support for Emacs
;;     Unmodified original sourve available at http://www.karllandstrom.se/downloads/emacs/javascript.el
;;     Copyright (c) 2008 Free Software Foundation
;;     Portions Copyright (C) Microsoft Open Technologies, Inc. All rights reserved.
;;
;;     This program is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 3 of the License, or
;;     (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public License
;;     along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; -------------------------------------------------------------------------------------------

;; URL: http://github.com/ananthakumaran/typescript.el
;; Package-Version: 20170831.619
;; Version: 0.1
;; Keywords: typescript languages
;; Package-Requires: ()

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is based on Karl Landstrom's barebones typescript-mode. This
;; is much more robust and works with cc-mode's comment filling
;; (mostly).
;; The modifications to the original javascript.el mode mainly consisted in
;; replacing "javascript" with "typescript"
;;
;; The main features of this typescript mode are syntactic
;; highlighting (enabled with `font-lock-mode' or
;; `global-font-lock-mode'), automatic indentation and filling of
;; comments and C preprocessor fontification
;;
;;
;; General Remarks:
;;
;; XXX: This mode assumes that block comments are not nested inside block
;; XXX: comments
;;
;; Exported names start with "typescript-"; private names start with
;; "typescript--".

;;; Code:

(eval-and-compile
  (require 'compile)
  (require 'cc-mode)
  (require 'font-lock)
  (require 'newcomment)
  (require 'etags)
  (require 'thingatpt)
  (require 'ido)
  (require 'json nil t))

(eval-when-compile
  (require 'cl))

(declare-function ido-mode "ido")

;;; Constants

(defconst typescript--name-start-re "[a-zA-Z_$]"
  "Regexp matching the start of a typescript identifier, without grouping.")

(defconst typescript--name-re (concat typescript--name-start-re
                              "\\(?:\\s_\\|\\sw\\)*")
  "Regexp matching a typescript identifier, without grouping.")

(defconst typescript--objfield-re (concat typescript--name-re ":")
  "Regexp matching the start of a typescript object field.")

(defconst typescript--dotted-name-re
  (concat typescript--name-re "\\(?:\\." typescript--name-re "\\)*")
  "Regexp matching a dot-separated sequence of typescript names.")

(defconst typescript--cpp-name-re typescript--name-re
  "Regexp matching a C preprocessor name.")

(defconst typescript--opt-cpp-start "^\\s-*#\\s-*\\([[:alnum:]]+\\)"
  "Regexp matching the prefix of a cpp directive.
This includes the directive name, or nil in languages without
preprocessor support.  The first submatch surrounds the directive
name.")

(defconst typescript--plain-method-re
  (concat "^\\s-*?\\(" typescript--dotted-name-re "\\)\\.prototype"
          "\\.\\(" typescript--name-re "\\)\\s-*?=\\s-*?\\(function\\)\\_>")
  "Regexp matching an explicit typescript prototype \"method\" declaration.
Group 1 is a (possibly-dotted) class name, group 2 is a method name,
and group 3 is the 'function' keyword.")

(defconst typescript--plain-class-re
  (concat "^\\s-*\\(" typescript--dotted-name-re "\\)\\.prototype"
          "\\s-*=\\s-*{")
  "Regexp matching a typescript explicit prototype \"class\" declaration.
An example of this is \"Class.prototype = { method1: ...}\".")

;; var NewClass = BaseClass.extend(
(defconst typescript--mp-class-decl-re
  (concat "^\\s-*var\\s-+"
          "\\(" typescript--name-re "\\)"
          "\\s-*=\\s-*"
          "\\(" typescript--dotted-name-re
          "\\)\\.extend\\(?:Final\\)?\\s-*(\\s-*{?\\s-*$"))

;; var NewClass = Class.create()
(defconst typescript--prototype-obsolete-class-decl-re
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" typescript--dotted-name-re "\\)"
          "\\s-*=\\s-*Class\\.create()"))

(defconst typescript--prototype-objextend-class-decl-re-1
  (concat "^\\s-*Object\\.extend\\s-*("
          "\\(" typescript--dotted-name-re "\\)"
          "\\s-*,\\s-*{"))

(defconst typescript--prototype-objextend-class-decl-re-2
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" typescript--dotted-name-re "\\)"
          "\\s-*=\\s-*Object\\.extend\\s-*\("))

;; var NewClass = Class.create({
(defconst typescript--prototype-class-decl-re
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" typescript--name-re "\\)"
          "\\s-*=\\s-*Class\\.create\\s-*(\\s-*"
          "\\(?:\\(" typescript--dotted-name-re "\\)\\s-*,\\s-*\\)?{?"))

;; Parent class name(s) (yes, multiple inheritance in typescript) are
;; matched with dedicated font-lock matchers
(defconst typescript--dojo-class-decl-re
  (concat "^\\s-*dojo\\.declare\\s-*(\"\\(" typescript--dotted-name-re "\\)"))

(defconst typescript--exttypescript-class-decl-re-1
  (concat "^\\s-*Ext\\.extend\\s-*("
          "\\s-*\\(" typescript--dotted-name-re "\\)"
          "\\s-*,\\s-*\\(" typescript--dotted-name-re "\\)")
  "Regexp matching an ExtTYPESCRIPT class declaration (style 1).")

(defconst typescript--exttypescript-class-decl-re-2
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" typescript--name-re "\\)"
          "\\s-*=\\s-*Ext\\.extend\\s-*(\\s-*"
          "\\(" typescript--dotted-name-re "\\)")
  "Regexp matching an ExtTYPESCRIPT class declaration (style 2).")

(defconst typescript--mochikit-class-re
  (concat "^\\s-*MochiKit\\.Base\\.update\\s-*(\\s-*"
          "\\(" typescript--dotted-name-re "\\)")
  "Regexp matching a MochiKit class declaration.")

(defconst typescript--dummy-class-style
  '(:name "[Automatically Generated Class]"))

(defconst typescript--class-styles
  `((:name            "Plain"
     :class-decl      ,typescript--plain-class-re
     :prototype       t
     :contexts        (toplevel)
     :framework       typescript)

    (:name            "MochiKit"
     :class-decl      ,typescript--mochikit-class-re
     :prototype       t
     :contexts        (toplevel)
     :framework       mochikit)

    (:name            "Prototype (Obsolete)"
     :class-decl      ,typescript--prototype-obsolete-class-decl-re
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Prototype (Modern)"
     :class-decl      ,typescript--prototype-class-decl-re
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Prototype (Object.extend)"
     :class-decl      ,typescript--prototype-objextend-class-decl-re-1
     :prototype       t
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Prototype (Object.extend) 2"
     :class-decl      ,typescript--prototype-objextend-class-decl-re-2
     :prototype       t
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Dojo"
     :class-decl      ,typescript--dojo-class-decl-re
     :contexts        (toplevel)
     :framework       dojo)

    (:name            "ExtTYPESCRIPT (style 1)"
     :class-decl      ,typescript--exttypescript-class-decl-re-1
     :prototype       t
     :contexts        (toplevel)
     :framework       exttypescript)

    (:name            "ExtTYPESCRIPT (style 2)"
     :class-decl      ,typescript--exttypescript-class-decl-re-2
     :contexts        (toplevel)
     :framework       exttypescript)

    (:name            "Merrill Press"
     :class-decl      ,typescript--mp-class-decl-re
     :contexts        (toplevel)
     :framework       merrillpress))

  "List of typescript class definition styles.

A class definition style is a plist with the following keys:

:name is a human-readable name of the class type

:class-decl is a regular expression giving the start of the
class.  Its first group must match the name of its class.  If there
is a parent class, the second group should match, and it should be
the name of the class.

If :prototype is present and non-nil, the parser will merge
declarations for this constructs with others at the same lexical
level that have the same name.  Otherwise, multiple definitions
will create multiple top-level entries.  Don't use :prototype
unnecessarily: it has an associated cost in performance.

If :strip-prototype is present and non-nil, then if the class
name as matched contains
")

(defconst typescript--available-frameworks
  (loop with available-frameworks
        for style in typescript--class-styles
        for framework = (plist-get style :framework)
        unless (memq framework available-frameworks)
        collect framework into available-frameworks
        finally return available-frameworks)
  "List of available typescript frameworks symbols.")

(defconst typescript--function-heading-1-re
  (concat
   "^\\s-*function\\s-+\\(" typescript--name-re "\\)")
  "Regexp matching the start of a typescript function header.
Match group 1 is the name of the function.")

(defconst typescript--function-heading-2-re
  (concat
   "^\\s-*\\(" typescript--name-re "\\)\\s-*:\\s-*function\\_>")
  "Regexp matching the start of a function entry in an associative array.
Match group 1 is the name of the function.")

(defconst typescript--function-heading-3-re
  (concat
   "^\\s-*\\(?:var\\s-+\\)?\\(" typescript--dotted-name-re "\\)"
   "\\s-*=\\s-*function\\_>")
  "Regexp matching a line in the typescript form \"var MUMBLE = function\".
Match group 1 is MUMBLE.")

(defconst typescript--macro-decl-re
  (concat "^\\s-*#\\s-*define\\s-+\\(" typescript--cpp-name-re "\\)\\s-*(")
  "Regexp matching a CPP macro definition, up to the opening parenthesis.
Match group 1 is the name of the macro.")

(defun typescript--regexp-opt-symbol (list)
  "Like `regexp-opt', but surround the result with `\\\\_<' and `\\\\_>'."
  (concat "\\_<" (regexp-opt list t) "\\_>"))

(defconst typescript--keyword-re
  (typescript--regexp-opt-symbol
   '("abstract" "any" "as" "async" "await" "boolean" "break" "case" "catch" "class" "const"
     "constructor" "continue" "declare" "default" "delete" "do" "else"
     "enum" "export" "extends" "extern" "false" "finally" "for"
     "function" "from" "get" "goto" "if" "implements" "import" "in" "instanceof"
     "interface" "keyof" "let" "module" "namespace" "new" "null" "number" "object" "of"
     "private" "protected" "public" "readonly" "return" "set" "static" "string"
     "super" "switch"  "this" "throw" "true"
     "try" "type" "typeof" "var" "void"
     "while" ))
  "Regexp matching any typescript keyword.")

(defconst typescript--basic-type-re
  (typescript--regexp-opt-symbol
   '("bool" "boolean" "string" "number" "any" "void"))
  "Regular expression matching any predefined type in typescript.")

(defconst typescript--constant-re
  (typescript--regexp-opt-symbol '("false" "null" "undefined"
                                 "Infinity" "NaN"
                                 "true" "arguments" "this"))
  "Regular expression matching any future reserved words in typescript.")


(defconst typescript--font-lock-keywords-1
  (list
   "\\_<import\\_>"
   (list typescript--function-heading-1-re 1 font-lock-function-name-face)
   (list typescript--function-heading-2-re 1 font-lock-function-name-face))
  "Level one font lock keywords for `typescript-mode'.")

(defconst typescript--font-lock-keywords-2
  (append typescript--font-lock-keywords-1
          (list (list typescript--keyword-re 1 font-lock-keyword-face)
                (list "\\_<for\\_>"
                      "\\s-+\\(each\\)\\_>" nil nil
                      (list 1 'font-lock-keyword-face))
                (cons typescript--basic-type-re font-lock-type-face)
                (cons typescript--constant-re font-lock-constant-face)))
  "Level two font lock keywords for `typescript-mode'.")

;; typescript--pitem is the basic building block of the lexical
;; database. When one refers to a real part of the buffer, the region
;; of text to which it refers is split into a conceptual header and
;; body. Consider the (very short) block described by a hypothetical
;; typescript--pitem:
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
;; typescript--pitem instances. The body of a pitem may be empty: in
;; that case, b-end is equal to header-end.
;;
;; The three points obey the following relationship:
;;
;;   h-begin < h-end <= b-end
;;
;; We put a text property in the buffer on the character *before*
;; h-end, and if we see it, on the character *before* b-end.
;;
;; The text property for h-end, typescript--pstate, is actually a list
;; of all typescript--pitem instances open after the marked character.
;;
;; The text property for b-end, typescript--pend, is simply the
;; typescript--pitem that ends after the marked character. (Because
;; pitems always end when the paren-depth drops below a critical
;; value, and because we can only drop one level per character, only
;; one pitem may end at a given character.)
;;
;; In the structure below, we only store h-begin and (sometimes)
;; b-end. We can trivially and quickly find h-end by going to h-begin
;; and searching for an typescript--pstate text property. Since no other
;; typescript--pitem instances can be nested inside the header of a
;; pitem, the location after the character with this text property
;; must be h-end.
;;
;; typescript--pitem instances are never modified (with the exception
;; of the b-end field). Instead, modified copies are added at subseqnce parse points.
;; (The exception for b-end and its caveats is described below.)
;;

(defstruct (typescript--pitem (:type list))
  ;; IMPORTANT: Do not alter the position of fields within the list.
  ;; Various bits of code depend on their positions, particularly
  ;; anything that manipulates the list of children.

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
  ;; terminating cons cell. We modify that shared cons cell directly.
  ;;
  ;; The field value is either a number (buffer location) or nil if
  ;; unknown.
  ;;
  ;; If the field's value is greater than `typescript--cache-end', the
  ;; value is stale and must be treated as if it were nil. Conversely,
  ;; if this field is nil, it is guaranteed that this pitem is open up
  ;; to at least `typescript--cache-end'. (This property is handy when
  ;; computing whether we're inside a given pitem.)
  ;;
  (b-end nil))

;; The pitem we start parsing with.
(defconst typescript--initial-pitem
  (make-typescript--pitem
   :paren-depth most-negative-fixnum
   :type 'toplevel))

;; When we say "jsdoc" here, we mean "jsdoc 3". There exist multiple dialects of
;; "jsdoc documentation".

;; Note that all typedoc/jsdoc regexp by themselves would match occurrences that appear outside
;; documentation comments. The logic that uses these regexps must guard against it.
(defconst typescript-typedoc-link-tag-regexp
  "\\[\\[.*?\\]\\]"
  "Matches a typedoc link.")

(defconst typescript-typedoc-literal-markup-regexp
  "\\(`+\\).*?\\1"
  "Matches a typedoc keyword markup.")

(defconst typescript-jsdoc-before-tag-regexp
  "\\(?:^\\s-*\\*+\\|/\\*\\*\\)\\s-*"
  "Matches everything we allow before the @ of a jsdoc tag.")

;; This was taken from js2-mode.
(defconst typescript-jsdoc-param-tag-regexp
  (concat typescript-jsdoc-before-tag-regexp
          "\\(@"
          "\\(?:param\\|arg\\(?:ument\\)?\\|prop\\(?:erty\\)?\\)"
          "\\)"
          "\\s-*\\({[^}]+}\\)?"         ; optional type
          "\\s-*\\[?\\([[:alnum:]_$\.]+\\)?\\]?"  ; name
          "\\_>")
  "Matches jsdoc tags with optional type and optional param name.")

;; This was taken from js2-mode.
(defconst typescript-jsdoc-typed-tag-regexp
  (concat typescript-jsdoc-before-tag-regexp
          "\\(@\\(?:"
          (regexp-opt
           '("enum"
             "extends"
             "field"
             "id"
             "implements"
             "lends"
             "mods"
             "requires"
             "return"
             "returns"
             "throw"
             "throws"
             "type"))
          "\\)\\)\\s-*\\({[^}]+}\\)?")
  "Matches jsdoc tags with optional type.")

;; This was taken from js2-mode.
(defconst typescript-jsdoc-arg-tag-regexp
  (concat typescript-jsdoc-before-tag-regexp
          "\\(@\\(?:"
          (regexp-opt
           '("alias"
             "augments"
             "borrows"
             "callback"
             "bug"
             "base"
             "config"
             "default"
             "define"
             "emits"
             "exception"
             "fires"
             "func"
             "function"
             "member"
             "memberOf"
             "method"
             "name"
             "namespace"
             "since"
             "suppress"
             "this"
             "throws"
             "version"))
          "\\)\\)\\s-+\\([^ \t]+\\)")
  "Matches jsdoc tags with a single argument.")

;; This was taken from js2-mode.
(defconst typescript-jsdoc-empty-tag-regexp
  (concat typescript-jsdoc-before-tag-regexp
          "\\(@\\(?:"
          (regexp-opt
           '("addon"
             "author"
             "class"
             "const"
             "constant"
             "constructor"
             "constructs"
             "copyright"
             "deprecated"
             "desc"
             "description"
             "event"
             "example"
             "exec"
             "export"
             "fileoverview"
             "final"
             "func"
             "function"
             "hidden"
             "ignore"
             "implicitCast"
             "inheritDoc"
             "inner"
             "interface"
             "license"
             "method"
             "noalias"
             "noshadow"
             "notypecheck"
             "override"
             "owner"
             "preserve"
             "preserveTry"
             "private"
             "protected"
             "public"
             "static"
             "supported"
             ))
          "\\)\\)\\s-*")
  "Matches empty jsdoc tags.")

;; Note that this regexp by itself would match tslint flags that appear inside
;; strings. The logic using this regexp must guard against it.
(defconst typescript-tslint-flag-regexp
  "\\(?://\\|/\\*\\)\\s-*\\(tslint:.*?\\)\\(?:\\*/\\|$\\)"
  "Matches tslint flags.")

;;; Faces

(defface typescript-jsdoc-tag
  '((t :foreground "SlateGray"))
  "Face used to highlight @whatever tags in jsdoc comments."
  :group 'typescript)

(defface typescript-jsdoc-type
  '((t :foreground "SteelBlue"))
  "Face used to highlight {FooBar} types in jsdoc comments."
  :group 'typescript)

(defface typescript-jsdoc-value
  '((t :foreground "gold4"))
  "Face used to highlight tag values in jsdoc comments."
  :group 'typescript)

;;; User Customization

(defgroup typescript nil
  "Customization variables for typescript mode."
  :tag "typescript"
  :group 'languages)

(defcustom typescript-indent-level 4
  "Number of spaces for each indentation step in `typescript-mode'."
  :type 'integer
  :group 'typescript)

(defcustom typescript-expr-indent-offset 0
  "Number of additional spaces used for indentation of continued expressions.
The value must be no less than minus `typescript-indent-level'."
  :type 'integer
  :group 'typescript)

(defcustom typescript-auto-indent-flag t
  "Whether to automatically indent when typing punctuation characters.
If non-nil, the characters {}();,: also indent the current line
in typescript mode."
  :type 'boolean
  :group 'typescript)

(defcustom typescript-flat-functions nil
  "Treat nested functions as top-level functions in `typescript-mode'.
This applies to function movement, marking, and so on."
  :type 'boolean
  :group 'typescript)

(defcustom typescript-comment-lineup-func #'c-lineup-C-comments
  "Lineup function for `cc-mode-style', for C comments in `typescript-mode'."
  :type 'function
  :group 'typescript)

(defcustom typescript-enabled-frameworks typescript--available-frameworks
  "Frameworks recognized by `typescript-mode'.
To improve performance, you may turn off some frameworks you
seldom use, either globally or on a per-buffer basis."
  :type (cons 'set (mapcar (lambda (x)
                             (list 'const x))
                           typescript--available-frameworks))
  :group 'typescript)

(defcustom typescript-mode-hook nil
  "*Hook called by `typescript-mode'."
  :type 'hook
  :group 'typescript)

;;; KeyMap

(defvar typescript-mode-map
  (let ((keymap (make-sparse-keymap)))
    (mapc (lambda (key)
	    (define-key keymap key #'typescript-insert-and-indent))
	  '("{" "}" "(" ")" ":" ";" ","))
    keymap)
  "Keymap for `typescript-mode'.")

(defun typescript-insert-and-indent (key)
  "Run the command bound to KEY, and indent if necessary.
Indentation does not take place if point is in a string or
comment."
  (interactive (list (this-command-keys)))
  (call-interactively (lookup-key (current-global-map) key))
  (let ((syntax (save-restriction (widen) (syntax-ppss))))
    (when (or (and (not (nth 8 syntax))
                   typescript-auto-indent-flag)
              (and (nth 4 syntax)
                   (eq (current-column)
                       (1+ (current-indentation)))))
      (indent-according-to-mode))))


;;; Syntax table and parsing

(defvar typescript-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?` "\"" table)
    table)
  "Syntax table for `typescript-mode'.")

(defvar typescript--quick-match-re nil
  "Autogenerated regexp used by `typescript-mode' to match buffer constructs.")

(defvar typescript--quick-match-re-func nil
  "Autogenerated regexp used by `typescript-mode' to match constructs and functions.")

(make-variable-buffer-local 'typescript--quick-match-re)
(make-variable-buffer-local 'typescript--quick-match-re-func)

(defvar typescript--cache-end 1
  "Last valid buffer position for the `typescript-mode' function cache.")
(make-variable-buffer-local 'typescript--cache-end)

(defvar typescript--last-parse-pos nil
  "Latest parse position reached by `typescript--ensure-cache'.")
(make-variable-buffer-local 'typescript--last-parse-pos)

(defvar typescript--state-at-last-parse-pos nil
  "Parse state at `typescript--last-parse-pos'.")
(make-variable-buffer-local 'typescript--state-at-last-parse-pos)

(defun typescript--flatten-list (list)
  (loop for item in list
        nconc (cond ((consp item)
                     (typescript--flatten-list item))
                    (item (list item)))))

(defun typescript--maybe-join (prefix separator suffix &rest list)
  "Helper function for `typescript--update-quick-match-re'.
If LIST contains any element that is not nil, return its non-nil
elements, separated by SEPARATOR, prefixed by PREFIX, and ended
with SUFFIX as with `concat'.  Otherwise, if LIST is empty, return
nil.  If any element in LIST is itself a list, flatten that
element."
  (setq list (typescript--flatten-list list))
  (when list
    (concat prefix (mapconcat #'identity list separator) suffix)))

(defun typescript--update-quick-match-re ()
  "Internal function used by `typescript-mode' for caching buffer constructs.
This updates `typescript--quick-match-re', based on the current set of
enabled frameworks."
  (setq typescript--quick-match-re
        (typescript--maybe-join
         "^[ \t]*\\(?:" "\\|" "\\)"

         ;; #define mumble
         "#define[ \t]+[a-zA-Z_]"

         (when (memq 'exttypescript typescript-enabled-frameworks)
           "Ext\\.extend")

         (when (memq 'prototype typescript-enabled-frameworks)
           "Object\\.extend")

          ;; var mumble = THING (
         (typescript--maybe-join
          "\\(?:var[ \t]+\\)?[a-zA-Z_$0-9.]+[ \t]*=[ \t]*\\(?:"
          "\\|"
          "\\)[ \t]*\("

          (when (memq 'prototype typescript-enabled-frameworks)
                    "Class\\.create")

          (when (memq 'exttypescript typescript-enabled-frameworks)
            "Ext\\.extend")

          (when (memq 'merrillpress typescript-enabled-frameworks)
            "[a-zA-Z_$0-9]+\\.extend\\(?:Final\\)?"))

         (when (memq 'dojo typescript-enabled-frameworks)
           "dojo\\.declare[ \t]*\(")

         (when (memq 'mochikit typescript-enabled-frameworks)
           "MochiKit\\.Base\\.update[ \t]*\(")

         ;; mumble.prototypeTHING
         (typescript--maybe-join
          "[a-zA-Z_$0-9.]+\\.prototype\\(?:" "\\|" "\\)"

          (when (memq 'typescript typescript-enabled-frameworks)
            '( ;; foo.prototype.bar = function(
              "\\.[a-zA-Z_$0-9]+[ \t]*=[ \t]*function[ \t]*\("

              ;; mumble.prototype = {
              "[ \t]*=[ \t]*{")))))

  (setq typescript--quick-match-re-func
        (concat "function\\|" typescript--quick-match-re)))

(defun typescript--forward-text-property (propname)
  "Move over the next value of PROPNAME in the buffer.
If found, return that value and leave point after the character
having that value; otherwise, return nil and leave point at EOB."
  (let ((next-value (get-text-property (point) propname)))
    (if next-value
        (forward-char)

      (goto-char (next-single-property-change
                  (point) propname nil (point-max)))
      (unless (eobp)
        (setq next-value (get-text-property (point) propname))
        (forward-char)))

    next-value))

(defun typescript--backward-text-property (propname)
  "Move over the previous value of PROPNAME in the buffer.
If found, return that value and leave point just before the
character that has that value, otherwise return nil and leave
point at BOB."
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

(defsubst typescript--forward-pstate ()
  (typescript--forward-text-property 'typescript--pstate))

(defsubst typescript--backward-pstate ()
  (typescript--backward-text-property 'typescript--pstate))

(defun typescript--pitem-goto-h-end (pitem)
  (goto-char (typescript--pitem-h-begin pitem))
  (typescript--forward-pstate))

(defun typescript--re-search-forward-inner (regexp &optional bound count)
  "Helper function for `typescript--re-search-forward'."
  (let ((parse)
        str-terminator
        (orig-macro-end (save-excursion
                          (when (typescript--beginning-of-macro)
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
                  (typescript--beginning-of-macro))
             (c-end-of-macro))
            (t
             (setq count (1- count))))))
  (point))


(defun typescript--re-search-forward (regexp &optional bound noerror count)
  "Search forward, ignoring strings, cpp macros, and comments.
This function invokes `re-search-forward', but treats the buffer
as if strings, cpp macros, and comments have been removed.

If invoked while inside a macro, it treats the contents of the
macro as normal text."
  (let ((saved-point (point))
        (search-expr
         (cond ((null count)
                '(typescript--re-search-forward-inner regexp bound 1))
               ((< count 0)
                '(typescript--re-search-backward-inner regexp bound (- count)))
               ((> count 0)
                '(typescript--re-search-forward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))


(defun typescript--re-search-backward-inner (regexp &optional bound count)
  "Auxiliary function for `typescript--re-search-backward'."
  (let ((parse)
        (orig-macro-start
         (save-excursion
           (and (typescript--beginning-of-macro)
                (point)))))
    (while (> count 0)
      (re-search-backward regexp bound)
      (when (and (> (point) (point-min))
                 (save-excursion (backward-char) (looking-at "/[/*]")))
        (forward-char))
      (setq parse (syntax-ppss))
      (cond
       ;; If we are in a comment or a string, jump back to the start
       ;; of the comment or string.
       ((nth 8 parse)
        (goto-char (nth 8 parse)))
       ((and (eq (char-before) ?/) (eq (char-after) ?*))
        (re-search-backward "/\\*"))
       ((and (not (and orig-macro-start
                       (>= (point) orig-macro-start)))
             (typescript--beginning-of-macro)))
       (t
        (setq count (1- count))))))
  (point))


(defun typescript--re-search-backward (regexp &optional bound noerror count)
  "Search backward, ignoring strings, preprocessor macros, and comments.

This function invokes `re-search-backward' but treats the buffer
as if strings, preprocessor macros, and comments have been
removed.

If invoked while inside a macro, treat the macro as normal text."
  (let ((saved-point (point))
        (search-expr
         (cond ((null count)
                '(typescript--re-search-backward-inner regexp bound 1))
               ((< count 0)
                '(typescript--re-search-forward-inner regexp bound (- count)))
               ((> count 0)
                '(typescript--re-search-backward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))

(defun typescript--forward-expression ()
  "Move forward over a whole typescript expression.
This function doesn't move over expressions continued across
lines."
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
                (typescript--continued-expression-p)))))

(defun typescript--forward-function-decl ()
  "Move forward over a typescript function declaration.
This puts point at the 'function' keyword.

If this is a syntactically-correct non-expression function,
return the name of the function, or t if the name could not be
determined.  Otherwise, return nil."
  (assert (looking-at "\\_<function\\_>"))
  (let ((name t))
    (forward-word)
    (forward-comment most-positive-fixnum)
    (when (looking-at typescript--name-re)
      (setq name (match-string-no-properties 0))
      (goto-char (match-end 0)))
    (forward-comment most-positive-fixnum)
    (and (eq (char-after) ?\( )
         (ignore-errors (forward-list) t)
         (progn (forward-comment most-positive-fixnum)
                (and (eq (char-after) ?{)
                     name)))))

(defun typescript--function-prologue-beginning (&optional pos)
  "Return the start of the typescript function prologue containing POS.
A function prologue is everything from start of the definition up
to and including the opening brace.  POS defaults to point.
If POS is not in a function prologue, return nil."
  (let (prologue-begin)
    (save-excursion
      (if pos
          (goto-char pos)
        (setq pos (point)))

      (when (save-excursion
              (forward-line 0)
              (or (looking-at typescript--function-heading-2-re)
                  (looking-at typescript--function-heading-3-re)))

        (setq prologue-begin (match-beginning 1))
        (when (<= prologue-begin pos)
          (goto-char (match-end 0))))

      (skip-syntax-backward "w_")
      (and (or (looking-at "\\_<function\\_>")
               (typescript--re-search-backward "\\_<function\\_>" nil t))

           (save-match-data (goto-char (match-beginning 0))
                            (typescript--forward-function-decl))

           (<= pos (point))
           (or prologue-begin (match-beginning 0))))))

(defun typescript--beginning-of-defun-raw ()
  "Helper function for `typescript-beginning-of-defun'.
Go to previous defun-beginning and return the parse state for it,
or nil if we went all the way back to bob and don't find
anything."
  (typescript--ensure-cache)
  (let (pstate)
    (while (and (setq pstate (typescript--backward-pstate))
                (not (eq 'function (typescript--pitem-type (car pstate))))))
    (and (not (bobp)) pstate)))

(defun typescript--pstate-is-toplevel-defun (pstate)
  "Helper function for `typescript--beginning-of-defun-nested'.
If PSTATE represents a non-empty top-level defun, return the
top-most pitem.  Otherwise, return nil."
  (loop for pitem in pstate
        with func-depth = 0
        with func-pitem
        if (eq 'function (typescript--pitem-type pitem))
        do (incf func-depth)
        and do (setq func-pitem pitem)
        finally return (if (eq func-depth 1) func-pitem)))

(defun typescript--beginning-of-defun-nested ()
  "Helper function for `typescript--beginning-of-defun'.
Return the pitem of the function we went to the beginning of."
  (or
   ;; Look for the smallest function that encloses point...
   (loop for pitem in (typescript--parse-state-at-point)
         if (and (eq 'function (typescript--pitem-type pitem))
                 (typescript--inside-pitem-p pitem))
         do (goto-char (typescript--pitem-h-begin pitem))
         and return pitem)

   ;; ...and if that isn't found, look for the previous top-level
   ;; defun
   (loop for pstate = (typescript--backward-pstate)
         while pstate
         if (typescript--pstate-is-toplevel-defun pstate)
         do (goto-char (typescript--pitem-h-begin it))
         and return it)))

(defun typescript--beginning-of-defun-flat ()
  "Helper function for `typescript-beginning-of-defun'."
  (let ((pstate (typescript--beginning-of-defun-raw)))
    (when pstate
      (goto-char (typescript--pitem-h-begin (car pstate))))))

(defun typescript-beginning-of-defun (&optional arg)
  "Value of `beginning-of-defun-function' for `typescript-mode'."
  (setq arg (or arg 1))
  (while (and (not (eobp)) (< arg 0))
    (incf arg)
    (when (and (not typescript-flat-functions)
               (or (eq (typescript-syntactic-context) 'function)
                   (typescript--function-prologue-beginning)))
      (typescript-end-of-defun))

    (if (typescript--re-search-forward
         "\\_<function\\_>" nil t)
        (goto-char (typescript--function-prologue-beginning))
      (goto-char (point-max))))

  (while (> arg 0)
    (decf arg)
    ;; If we're just past the end of a function, the user probably wants
    ;; to go to the beginning of *that* function
    (when (eq (char-before) ?})
      (backward-char))

    (let ((prologue-begin (typescript--function-prologue-beginning)))
      (cond ((and prologue-begin (< prologue-begin (point)))
             (goto-char prologue-begin))

            (typescript-flat-functions
             (typescript--beginning-of-defun-flat))
            (t
             (typescript--beginning-of-defun-nested))))))

(defun typescript--flush-caches (&optional beg ignored)
  "Flush the `typescript-mode' syntax cache after position BEG.
BEG defaults to `point-min', meaning to flush the entire cache."
  (interactive)
  (setq beg (or beg (save-restriction (widen) (point-min))))
  (setq typescript--cache-end (min typescript--cache-end beg)))

(defmacro typescript--debug (&rest arguments)
  ;; `(message ,@arguments)
  )

(defun typescript--ensure-cache--pop-if-ended (open-items paren-depth)
  (let ((top-item (car open-items)))
    (when (<= paren-depth (typescript--pitem-paren-depth top-item))
      (assert (not (get-text-property (1- (point)) 'typescript-pend)))
      (put-text-property (1- (point)) (point) 'typescript--pend top-item)
      (setf (typescript--pitem-b-end top-item) (point))
      (setq open-items
            ;; open-items must contain at least two items for this to
            ;; work, but because we push a dummy item to start with,
            ;; that assumption holds.
            (cons (typescript--pitem-add-child (second open-items) top-item)
                  (cddr open-items)))))
  open-items)

(defmacro typescript--ensure-cache--update-parse ()
  "Helper function for `typescript--ensure-cache'.
Update parsing information up to point, referring to parse,
prev-parse-point, goal-point, and open-items bound lexically in
the body of `typescript--ensure-cache'."
  `(progn
     (setq goal-point (point))
     (goto-char prev-parse-point)
     (while (progn
              (setq open-items (typescript--ensure-cache--pop-if-ended
                                open-items (car parse)))
              ;; Make sure parse-partial-sexp doesn't stop because we *entered*
              ;; the given depth -- i.e., make sure we're deeper than the target
              ;; depth.
              (assert (> (nth 0 parse)
                         (typescript--pitem-paren-depth (car open-items))))
              (setq parse (parse-partial-sexp
                           prev-parse-point goal-point
                           (typescript--pitem-paren-depth (car open-items))
                           nil parse))

;;              (let ((overlay (make-overlay prev-parse-point (point))))
;;                (overlay-put overlay 'face '(:background "red"))
;;                (unwind-protect
;;                     (progn
;;                       (typescript--debug "parsed: %S" parse)
;;                       (sit-for 1))
;;                  (delete-overlay overlay)))

              (setq prev-parse-point (point))
              (< (point) goal-point)))

     (setq open-items (typescript--ensure-cache--pop-if-ended
                       open-items (car parse)))))

(defun typescript--show-cache-at-point ()
  (interactive)
  (require 'pp)
  (let ((prop (get-text-property (point) 'typescript--pstate)))
    (with-output-to-temp-buffer "*Help*"
      (pp prop))))

(defun typescript--split-name (string)
  "Split a typescript name into its dot-separated parts.
This also removes any prototype parts from the split name
\(unless the name is just \"prototype\" to start with)."
  (let ((name (save-match-data
                (split-string string "\\." t))))
    (unless (and (= (length name) 1)
                 (equal (car name) "prototype"))

      (setq name (remove "prototype" name)))))

(defvar typescript--guess-function-name-start nil)

(defun typescript--guess-function-name (position)
  "Guess the name of the typescript function at POSITION.
POSITION should be just after the end of the word \"function\".
Return the name of the function, or nil if the name could not be
guessed.

This function clobbers match data.  If we find the preamble
begins earlier than expected while guessing the function name,
set `typescript--guess-function-name-start' to that position; otherwise,
set that variable to nil."
  (setq typescript--guess-function-name-start nil)
  (save-excursion
    (goto-char position)
    (forward-line 0)
    (cond
     ((looking-at typescript--function-heading-3-re)
      (and (eq (match-end 0) position)
           (setq typescript--guess-function-name-start (match-beginning 1))
           (match-string-no-properties 1)))

     ((looking-at typescript--function-heading-2-re)
      (and (eq (match-end 0) position)
           (setq typescript--guess-function-name-start (match-beginning 1))
           (match-string-no-properties 1))))))

(defun typescript--clear-stale-cache ()
  ;; Clear any endings that occur after point
  (let (end-prop)
    (save-excursion
      (while (setq end-prop (typescript--forward-text-property
                             'typescript--pend))
        (setf (typescript--pitem-b-end end-prop) nil))))

  ;; Remove any cache properties after this point
  (remove-text-properties (point) (point-max)
                          '(typescript--pstate t typescript--pend t)))

(defun typescript--ensure-cache (&optional limit)
  "Ensures brace cache is valid up to the character before LIMIT.
LIMIT defaults to point."
  (setq limit (or limit (point)))
  (when (< typescript--cache-end limit)

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
            (loop for style in typescript--class-styles
                  if (memq (plist-get style :framework)
                           typescript-enabled-frameworks)
                  collect style))

      (save-excursion
        (save-restriction
          (widen)

          ;; Find last known good position
          (goto-char typescript--cache-end)
          (unless (bobp)
            (setq open-items (get-text-property
                              (1- (point)) 'typescript--pstate))

            (unless open-items
              (goto-char (previous-single-property-change
                          (point) 'typescript--pstate nil (point-min)))

              (unless (bobp)
                (setq open-items (get-text-property (1- (point))
                                                    'typescript--pstate))
                (assert open-items))))

          (unless open-items
            ;; Make a placeholder for the top-level definition
            (setq open-items (list typescript--initial-pitem)))

          (setq parse (syntax-ppss))
          (setq prev-parse-point (point))

          (typescript--clear-stale-cache)

          (narrow-to-region (point-min) limit)

          (loop while (re-search-forward typescript--quick-match-re-func nil t)
                for orig-match-start = (goto-char (match-beginning 0))
                for orig-match-end = (match-end 0)
                do (typescript--ensure-cache--update-parse)
                for orig-depth = (nth 0 parse)

                ;; Each of these conditions should return non-nil if
                ;; we should add a new item and leave point at the end
                ;; of the new item's header (h-end in the
                ;; typescript--pitem diagram). This point is the one
                ;; after the last character we need to unambiguously
                ;; detect this construct. If one of these evaluates to
                ;; nil, the location of the point is ignored.
                if (cond
                    ;; In comment or string
                    ((nth 8 parse) nil)

                    ;; Regular function declaration
                    ((and (looking-at "\\_<function\\_>")
                          (setq name (typescript--forward-function-decl)))

                     (when (eq name t)
                       (setq name (typescript--guess-function-name orig-match-end))
                       (if name
                           (when typescript--guess-function-name-start
                             (setq orig-match-start
                                   typescript--guess-function-name-start))

                         (setq name t)))

                     (assert (eq (char-after) ?{))
                     (forward-char)
                     (make-typescript--pitem
                      :paren-depth orig-depth
                      :h-begin orig-match-start
                      :type 'function
                      :name (if (eq name t)
                                name
                              (typescript--split-name name))))

                    ;; Macro
                    ((looking-at typescript--macro-decl-re)

                     ;; Macros often contain unbalanced parentheses.
                     ;; Make sure that h-end is at the textual end of
                     ;; the macro no matter what the parenthesis say.
                     (c-end-of-macro)
                     (typescript--ensure-cache--update-parse)

                     (make-typescript--pitem
                      :paren-depth (nth 0 parse)
                      :h-begin orig-match-start
                      :type 'macro
                      :name (list (match-string-no-properties 1))))

                    ;; "Prototype function" declaration
                    ((looking-at typescript--plain-method-re)
                     (goto-char (match-beginning 3))
                     (when (save-match-data
                             (typescript--forward-function-decl))
                       (forward-char)
                       (make-typescript--pitem
                        :paren-depth orig-depth
                        :h-begin orig-match-start
                        :type 'function
                        :name (nconc (typescript--split-name
                                      (match-string-no-properties 1))
                                     (list (match-string-no-properties 2))))))

                    ;; Class definition
                    ((loop with syntactic-context =
                           (typescript--syntactic-context-from-pstate open-items)
                           for class-style in filtered-class-styles
                           if (and (memq syntactic-context
                                         (plist-get class-style :contexts))
                                   (looking-at (plist-get class-style
                                                          :class-decl)))
                           do (goto-char (match-end 0))
                           and return
                           (make-typescript--pitem
                            :paren-depth orig-depth
                            :h-begin orig-match-start
                            :type class-style
                            :name (typescript--split-name
                                   (match-string-no-properties 1))))))

                do (typescript--ensure-cache--update-parse)
                and do (push it open-items)
                and do (put-text-property
                        (1- (point)) (point) 'typescript--pstate open-items)
                else do (goto-char orig-match-end))

          (goto-char limit)
          (typescript--ensure-cache--update-parse)
          (setq typescript--cache-end limit)
          (setq typescript--last-parse-pos limit)
          (setq typescript--state-at-last-parse-pos open-items)
          )))))

(defun typescript--end-of-defun-flat ()
  "Helper function for `typescript-end-of-defun'."
  (loop while (typescript--re-search-forward "}" nil t)
        do (typescript--ensure-cache)
        if (get-text-property (1- (point)) 'typescript--pend)
        if (eq 'function (typescript--pitem-type it))
        return t
        finally do (goto-char (point-max))))

(defun typescript--end-of-defun-nested ()
  "Helper function for `typescript-end-of-defun'."
  (message "test")
  (let* (pitem
         (this-end (save-excursion
                     (and (setq pitem (typescript--beginning-of-defun-nested))
                          (typescript--pitem-goto-h-end pitem)
                          (progn (backward-char)
                                 (forward-list)
                                 (point)))))
         found)

    (if (and this-end (< (point) this-end))
        ;; We're already inside a function; just go to its end.
        (goto-char this-end)

      ;; Otherwise, go to the end of the next function...
      (while (and (typescript--re-search-forward "\\_<function\\_>" nil t)
                  (not (setq found (progn
                                     (goto-char (match-beginning 0))
                                     (typescript--forward-function-decl))))))

      (if found (forward-list)
        ;; ... or eob.
        (goto-char (point-max))))))

(defun typescript-end-of-defun (&optional arg)
  "Value of `end-of-defun-function' for `typescript-mode'."
  (setq arg (or arg 1))
  (while (and (not (bobp)) (< arg 0))
    (incf arg)
    (typescript-beginning-of-defun)
    (typescript-beginning-of-defun)
    (unless (bobp)
      (typescript-end-of-defun)))

  (while (> arg 0)
    (decf arg)
    ;; look for function backward. if we're inside it, go to that
    ;; function's end. otherwise, search for the next function's end and
    ;; go there
    (if typescript-flat-functions
        (typescript--end-of-defun-flat)

      ;; if we're doing nested functions, see whether we're in the
      ;; prologue. If we are, go to the end of the function; otherwise,
      ;; call typescript--end-of-defun-nested to do the real work
      (let ((prologue-begin (typescript--function-prologue-beginning)))
        (cond ((and prologue-begin (<= prologue-begin (point)))
               (goto-char prologue-begin)
               (re-search-forward "\\_<function")
               (goto-char (match-beginning 0))
               (typescript--forward-function-decl)
               (forward-list))

              (t (typescript--end-of-defun-nested)))))))

(defun typescript--beginning-of-macro (&optional lim)
  (let ((here (point)))
    (save-restriction
      (if lim (narrow-to-region lim (point-max)))
      (beginning-of-line)
      (while (eq (char-before (1- (point))) ?\\)
        (forward-line -1))
      (back-to-indentation)
      (if (and (<= (point) here)
               (looking-at typescript--opt-cpp-start))
          t
        (goto-char here)
        nil))))

(defun typescript--backward-syntactic-ws (&optional lim)
  "Simple implementation of `c-backward-syntactic-ws' for `typescript-mode'."
  (save-restriction
    (when lim (narrow-to-region lim (point-max)))

    (let ((in-macro (save-excursion (typescript--beginning-of-macro)))
          (pos (point)))

      (while (progn (unless in-macro (typescript--beginning-of-macro))
                    (forward-comment most-negative-fixnum)
                    (/= (point)
                        (prog1
                            pos
                          (setq pos (point)))))))))

(defun typescript--forward-syntactic-ws (&optional lim)
  "Simple implementation of `c-forward-syntactic-ws' for `typescript-mode'."
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

;; Like (up-list -1), but only considers lists that end nearby"
(defun typescript--up-nearby-list ()
  (save-restriction
    ;; Look at a very small region so our compuation time doesn't
    ;; explode in pathological cases.
    (narrow-to-region (max (point-min) (- (point) 500)) (point))
    (up-list -1)))

(defun typescript--inside-param-list-p ()
  "Return non-nil iff point is in a function parameter list."
  (ignore-errors
    (save-excursion
      (typescript--up-nearby-list)
      (and (looking-at "(")
           (progn (forward-symbol -1)
                  (or (looking-at "function")
                      (progn (forward-symbol -1)
                             (looking-at "function"))))))))

(defun typescript--inside-dojo-class-list-p ()
  "Return non-nil iff point is in a Dojo multiple-inheritance class block."
  (ignore-errors
    (save-excursion
      (typescript--up-nearby-list)
      (let ((list-begin (point)))
        (forward-line 0)
        (and (looking-at typescript--dojo-class-decl-re)
             (goto-char (match-end 0))
             (looking-at "\"\\s-*,\\s-*\\[")
             (eq (match-end 0) (1+ list-begin)))))))

(defun typescript--syntax-begin-function ()
  (when (< typescript--cache-end (point))
    (goto-char (max (point-min) typescript--cache-end)))

  (let ((pitem))
    (while (and (setq pitem (car (typescript--backward-pstate)))
                (not (eq 0 (typescript--pitem-paren-depth pitem)))))

    (when pitem
      (goto-char (typescript--pitem-h-begin pitem )))))

;;; Font Lock
(defun typescript--make-framework-matcher (framework &rest regexps)
  "Helper function for building `typescript--font-lock-keywords'.
Create a byte-compiled function for matching a concatenation of
REGEXPS, but only if FRAMEWORK is in `typescript-enabled-frameworks'."
  (setq regexps (apply #'concat regexps))
  (byte-compile
   `(lambda (limit)
      (when (memq (quote ,framework) typescript-enabled-frameworks)
        (re-search-forward ,regexps limit t)))))

(defvar typescript--tmp-location nil)
(make-variable-buffer-local 'typescript--tmp-location)

(defun typescript--forward-destructuring-spec (&optional func)
  "Move forward over a typescript destructuring spec.
If FUNC is supplied, call it with no arguments before every
variable name in the spec.  Return true iff this was actually a
spec.  FUNC must preserve the match data."
  (case (char-after)
    (?\[
     (forward-char)
     (while
         (progn
           (forward-comment most-positive-fixnum)
           (cond ((memq (char-after) '(?\[ ?\{))
                  (typescript--forward-destructuring-spec func))

                 ((eq (char-after) ?,)
                  (forward-char)
                  t)

                 ((looking-at typescript--name-re)
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
         (when (looking-at typescript--objfield-re)
           (goto-char (match-end 0))
           (forward-comment most-positive-fixnum)
           (and (cond ((memq (char-after) '(?\[ ?\{))
                       (typescript--forward-destructuring-spec func))
                      ((looking-at typescript--name-re)
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

(defun typescript--variable-decl-matcher (limit)
  "Font-lock matcher for variable names in a variable declaration.
This is a cc-mode-style matcher that *always* fails, from the
point of view of font-lock.  It applies highlighting directly with
`font-lock-apply-highlight'."
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
                   (cond ((looking-at typescript--name-re)
                          (font-lock-apply-highlight
                           '(0 font-lock-variable-name-face))
                          (goto-char (match-end 0)))

                         ((save-excursion
                            (typescript--forward-destructuring-spec))

                          (typescript--forward-destructuring-spec
                           (lambda ()
                             (font-lock-apply-highlight
                              '(0 font-lock-variable-name-face)))))))

            (forward-comment most-positive-fixnum)
            (when (eq (char-after) ?=)
              (forward-char)
              (typescript--forward-expression)
              (forward-comment most-positive-fixnum))

            (setq first nil))))

    ;; Conditions to handle
    (scan-error nil)
    (end-of-buffer nil))

  ;; Matcher always "fails"
  nil)

(defun typescript--in-documentation-comment-p ()
  "Reports whether point is inside a documentation comment."
  (let ((parse (syntax-ppss)))
    (and
     (nth 4 parse) ;; Inside a comment ...
     (save-match-data
       (save-excursion
         (goto-char (nth 8 parse))
         (looking-at "/\\*\\*")))))) ;; ... which starts with /**

(defun typescript--documentation-font-lock-helper (re limit)
  "This is a helper macro that determines whether jsdoc highlighting is to be applied,
and searches for the next token to be highlighted."
  (loop while (re-search-forward re limit t)
        if (typescript--in-documentation-comment-p)
        return (point)))

(defun typescript--jsdoc-param-matcher (limit)
  "Font-lock mode matcher that finds jsdoc parameter tags in documentation."
  (typescript--documentation-font-lock-helper typescript-jsdoc-param-tag-regexp limit))

(defun typescript--jsdoc-typed-tag-matcher (limit)
  "Font-lock mode matcher that finds jsdoc typed tags in documentation."
  (typescript--documentation-font-lock-helper typescript-jsdoc-typed-tag-regexp limit))

(defun typescript--jsdoc-arg-tag-matcher (limit)
  "Font-lock mode matcher that finds jsdoc tags that take one argument in documentation."
  (typescript--documentation-font-lock-helper typescript-jsdoc-arg-tag-regexp limit))

(defun typescript--jsdoc-empty-tag-matcher (limit)
  "Font-lock mode matcher that finds jsdoc tags without argument in documentation."
  (typescript--documentation-font-lock-helper typescript-jsdoc-empty-tag-regexp limit))

(defun typescript--typedoc-link-matcher (limit)
  "Font-lock mode matcher that finds typedoc links in documentation."
  (typescript--documentation-font-lock-helper typescript-typedoc-link-tag-regexp limit))

(defun typescript--typedoc-literal-markup-matcher (limit)
  "Font-lock mode matcher that finds typedoc literal markup in documentation."
  (typescript--documentation-font-lock-helper typescript-typedoc-literal-markup-regexp limit))

(defun typescript--tslint-flag-matcher (limit)
  "Font-lock mode matcher that finds tslint flags in comments."
  (loop while (re-search-forward typescript-tslint-flag-regexp limit t)
        if (nth 4 (syntax-ppss (match-beginning 1)))
        return (point)))

(defconst typescript--font-lock-keywords-3
  `(
    ;; This goes before keywords-2 so it gets used preferentially
    ;; instead of the keywords in keywords-2. Don't use override
    ;; because that will override syntactic fontification too, which
    ;; will fontify commented-out directives as if they weren't
    ;; commented out.
    ,@cpp-font-lock-keywords ; from font-lock.el

    ,@typescript--font-lock-keywords-2

    (typescript--jsdoc-param-matcher (1 'typescript-jsdoc-tag t t)
                                     (2 'typescript-jsdoc-type t t)
                                     (3 'typescript-jsdoc-value t t))

    (typescript--jsdoc-typed-tag-matcher (1 'typescript-jsdoc-tag t t)
                                         (2 'typescript-jsdoc-type t t))

    (typescript--jsdoc-arg-tag-matcher (1 'typescript-jsdoc-tag t t)
                                       (2 'typescript-jsdoc-value t t))

    (typescript--jsdoc-empty-tag-matcher (1 'typescript-jsdoc-tag t t))

    (typescript--typedoc-link-matcher (0 'typescript-jsdoc-value t))

    (typescript--typedoc-literal-markup-matcher
     (0 'typescript-jsdoc-value t))

    (typescript--tslint-flag-matcher
     (1 font-lock-preprocessor-face t))

    ("\\.\\(prototype\\)\\_>"
     (1 font-lock-constant-face))

    ;; Highlights class being declared, in parts
    (typescript--class-decl-matcher
     ,(concat "\\(" typescript--name-re "\\)\\(?:\\.\\|.*$\\)")
     (goto-char (match-beginning 1))
     nil
     (1 font-lock-type-face))

    ;; Highlights parent class, in parts, if available
    (typescript--class-decl-matcher
     ,(concat "\\(" typescript--name-re "\\)\\(?:\\.\\|.*$\\)")
     (if (match-beginning 2)
         (progn
           (setq typescript--tmp-location (match-end 2))
           (goto-char typescript--tmp-location)
           (insert "=")
           (goto-char (match-beginning 2)))
       (setq typescript--tmp-location nil)
       (goto-char (point-at-eol)))
     (when typescript--tmp-location
       (save-excursion
         (goto-char typescript--tmp-location)
         (delete-char 1)))
     (1 font-lock-type-face))

    ;; Highlights parent class
    (typescript--class-decl-matcher
     (2 font-lock-type-face nil t))

    ;; Dojo needs its own matcher to override the string highlighting
    (,(typescript--make-framework-matcher
       'dojo
       "^\\s-*dojo\\.declare\\s-*(\""
       "\\(" typescript--dotted-name-re "\\)"
       "\\(?:\"\\s-*,\\s-*\\(" typescript--dotted-name-re "\\)\\)?")
     (1 font-lock-type-face t)
     (2 font-lock-type-face nil t))

    ;; Match Dojo base classes. Of course Mojo has to be different
    ;; from everything else under the sun...
    (,(typescript--make-framework-matcher
       'dojo
       "^\\s-*dojo\\.declare\\s-*(\""
       "\\(" typescript--dotted-name-re "\\)\"\\s-*,\\s-*\\[")
     ,(concat "[[,]\\s-*\\(" typescript--dotted-name-re "\\)\\s-*"
              "\\(?:\\].*$\\)?")
     (backward-char)
     (end-of-line)
     (1 font-lock-type-face))

    ;; continued Dojo base-class list
    (,(typescript--make-framework-matcher
       'dojo
       "^\\s-*" typescript--dotted-name-re "\\s-*[],]")
     ,(concat "\\(" typescript--dotted-name-re "\\)"
              "\\s-*\\(?:\\].*$\\)?")
     (if (save-excursion (backward-char)
                         (typescript--inside-dojo-class-list-p))
         (forward-symbol -1)
       (end-of-line))
     (end-of-line)
     (1 font-lock-type-face))

    ;; variable declarations
    ,(list
      (concat "\\_<\\(const\\|var\\|let\\)\\_>\\|" typescript--basic-type-re)
      (list #'typescript--variable-decl-matcher nil nil nil))

    ;; class instantiation
    ,(list
      (concat "\\_<new\\_>\\s-+\\(" typescript--dotted-name-re "\\)")
      (list 1 'font-lock-type-face))

    ;; instanceof
    ,(list
      (concat "\\_<instanceof\\_>\\s-+\\(" typescript--dotted-name-re "\\)")
      (list 1 'font-lock-type-face))

    ;; formal parameters
    ,(list
      (concat
       "\\_<function\\_>\\(\\s-+" typescript--name-re "\\)?\\s-*(\\s-*"
       typescript--name-start-re)
      (list (concat "\\(" typescript--name-re "\\)\\(\\s-*).*\\)?")
            '(backward-char)
            '(end-of-line)
            '(1 font-lock-variable-name-face)))

    ;; continued formal parameter list
    ,(list
      (concat
       "^\\s-*" typescript--name-re "\\s-*[,)]")
      (list typescript--name-re
            '(if (save-excursion (backward-char)
                                 (typescript--inside-param-list-p))
                 (forward-symbol -1)
               (end-of-line))
            '(end-of-line)
            '(0 font-lock-variable-name-face))))
  "Level three font lock for `typescript-mode'.")

(defun typescript--flyspell-mode-predicate ()
  "A custom predicate to help `flyspell-prog-mode' determine whether a word should be checked."
  ;; We depend on fontification for our results. font-lock-ensure is defined on
  ;; Emacs 25 and over. Earlier versions use font-lock-fontify-buffer.
  (if (fboundp 'font-lock-ensure)
      (font-lock-ensure)
    (font-lock-fontify-buffer))
  (and
   ;; Check with the default method that flyspell provides.
   (flyspell-generic-progmode-verify)

   ;;
   ;; And eliminate cases specific to our mode we don't want to have
   ;; spell-checked.
   ;;

   ;; Don't check the module names in import statements.
   (save-excursion
     (not (let* ((parse (syntax-ppss (1- (point))))
                 (string-start-pos (and (nth 3 parse)
                                        (nth 8 parse))))
            (and string-start-pos
                 (save-match-data
                   ;; Move to back to the start of the string, then past any ws
                   ;; and then past any non-ws to see if we have "from" or "import".
                   (goto-char string-start-pos)
                   (typescript--backward-syntactic-ws)
                   (skip-syntax-backward "^-" (point-at-bol))
                   (looking-at "from\\|import\\s-"))))))))

(defun typescript--inside-pitem-p (pitem)
  "Return whether point is inside the given pitem's header or body."
  (typescript--ensure-cache)
  (assert (typescript--pitem-h-begin pitem))
  (assert (typescript--pitem-paren-depth pitem))

  (and (> (point) (typescript--pitem-h-begin pitem))
       (or (null (typescript--pitem-b-end pitem))
           (> (typescript--pitem-b-end pitem) (point)))))

(defun typescript--parse-state-at-point ()
  "Parse the typescript program state at point.
Return a list of `typescript--pitem' instances that apply to point, most
specific first.  In the worst case, the current toplevel instance
will be returned."
  (save-excursion
    (save-restriction
      (widen)
      (typescript--ensure-cache)
      (let* ((bound (if (eobp) (point) (1+ (point))))
             (pstate (or (save-excursion
                           (typescript--backward-pstate))
                         (list typescript--initial-pitem))))

        ;; Loop until we either hit a pitem at BOB or pitem ends after
        ;; point (or at point if we're at eob)
        (loop for pitem = (car pstate)
              until (or (eq (typescript--pitem-type pitem)
                            'toplevel)
                        (typescript--inside-pitem-p pitem))
              do (pop pstate))

        pstate))))

(defun typescript--syntactic-context-from-pstate (pstate)
  "Return the typescript syntactic context corresponding to PSTATE."
  (let ((type (typescript--pitem-type (car pstate))))
    (cond ((memq type '(function macro))
           type)
          ((consp type)
           'class)
          (t 'toplevel))))

(defun typescript-syntactic-context ()
  "Return the typescript syntactic context at point.
When called interatively, also display a message with that
context."
  (interactive)
  (let* ((syntactic-context (typescript--syntactic-context-from-pstate
                             (typescript--parse-state-at-point))))

    (when (called-interactively-p 'interactive)
      (message "Syntactic context: %s" syntactic-context))

    syntactic-context))

(defun typescript--class-decl-matcher (limit)
  "Font lock function used by `typescript-mode'.
This performs fontification according to `typescript--class-styles'."
  (loop initially (typescript--ensure-cache limit)
        while (re-search-forward typescript--quick-match-re limit t)
        for orig-end = (match-end 0)
        do (goto-char (match-beginning 0))
        if (loop for style in typescript--class-styles
                 for decl-re = (plist-get style :class-decl)
                 if (and (memq (plist-get style :framework)
                               typescript-enabled-frameworks)
                         (memq (typescript-syntactic-context)
                               (plist-get style :contexts))
                         decl-re
                         (looking-at decl-re))
                 do (goto-char (match-end 0))
                 and return t)
        return t
        else do (goto-char orig-end)))

(defconst typescript--font-lock-keywords
  '(typescript--font-lock-keywords-3 typescript--font-lock-keywords-1
                                   typescript--font-lock-keywords-2
                                   typescript--font-lock-keywords-3)
  "Font lock keywords for `typescript-mode'.  See `font-lock-keywords'.")

;; XXX: typescript can continue a regexp literal across lines so long
;; as the newline is escaped with \. Account for that in the regexp
;; below.
(defconst typescript--regexp-literal
  "[=(,:]\\(?:\\s-\\|\n\\)*\\(/\\)\\(?:\\\\/\\|[^/*]\\)\\(?:\\\\/\\|[^/]\\)*\\(/\\)"
  "Regexp matching a typescript regular expression literal.
Match groups 1 and 2 are the characters forming the beginning and
end of the literal.")

;; we want to match regular expressions only at the beginning of
;; expressions
(defconst typescript-font-lock-syntactic-keywords
  `((,typescript--regexp-literal (1 "|") (2 "|")))
  "Syntactic font lock keywords matching regexps in typescript.
See `font-lock-keywords'.")

;;; Indentation

(defconst typescript--possibly-braceless-keyword-re
  (typescript--regexp-opt-symbol
   '("catch" "do" "else" "finally" "for" "if" "try" "while" "with"
     "each"))
  "Regexp matching keywords optionally followed by an opening brace.")

(defconst typescript--indent-keyword-re
  (typescript--regexp-opt-symbol '("in" "instanceof"))
  "Regexp matching keywords that affect indentation of continued expressions.")

(defconst typescript--indent-operator-re
  (concat "[-+*/%<>=&^|?:.]\\([^-+*/]\\|$\\)\\|" typescript--indent-keyword-re)
  "Regexp matching operators that affect indentation of continued expressions.")


(defun typescript--looking-at-operator-p ()
  "Return non-nil if point is on a typescript operator, other than a comma."
  (save-match-data
    (and (looking-at typescript--indent-operator-re)
         (or (not (looking-at ":"))
             (save-excursion
               (and (typescript--re-search-backward "[?:{]\\|\\_<case\\_>" nil t)
                    (looking-at "?"))))
         ;; Do not identify forward slashes appearing in a "list" as
         ;; an operator. The lists are: arrays, or lists of
         ;; arguments. In this context, they must be part of regular
         ;; expressions, and not math operators.
         (not (and (looking-at "/")
                   (save-excursion
                     (typescript--backward-syntactic-ws)
                     (memq (char-before) '(?, ?\[ ?\()))))
         ;; Do not identify methods, or fields, that are named "in" or
         ;; "instanceof" as being operator keywords.
         (not (and
               (looking-at typescript--indent-keyword-re)
               (save-excursion
                 (typescript--backward-syntactic-ws)
                 (memq (char-before) '(?, ?{ ?} ?\;)))))
         (not (and
               (looking-at "*")
               ;; Generator method (possibly using computed property).
               (looking-at (concat "\\* *\\(?:\\[\\|" typescript--name-re
                                   " *(\\)"))
               (save-excursion
                 (typescript--backward-syntactic-ws)
                 ;; We might misindent some expressions that would
                 ;; return NaN anyway.  Shouldn't be a problem.
                 (memq (char-before) '(?, ?} ?{ ?\;)))))))
)


(defun typescript--continued-expression-p ()
  "Return non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (and
     ;; Don't identify the spread syntax or rest operator as a
     ;; "continuation".
     (not (looking-at "\\.\\.\\."))
     (or (typescript--looking-at-operator-p)
         (and (typescript--re-search-backward "\n" nil t)
              (progn
                (skip-chars-backward " \t")
                (or (bobp) (backward-char))
                (and (> (point) (point-min))
                     (save-excursion (backward-char) (not (looking-at "[/*]/")))
                     (typescript--looking-at-operator-p)
                     (and (progn (backward-char)
                                 (not (looking-at "++\\|--\\|/[/*]")))))))))))


(defun typescript--end-of-do-while-loop-p ()
  "Return non-nil if point is on the \"while\" of a do-while statement.
Otherwise, return nil.  A braceless do-while statement spanning
several lines requires that the start of the loop is indented to
the same column as the current line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (looking-at "\\s-*\\_<while\\_>")
	(if (save-excursion
	      (skip-chars-backward "[ \t\n]*}")
	      (looking-at "[ \t\n]*}"))
	    (save-excursion
	      (backward-list) (forward-symbol -1) (looking-at "\\_<do\\_>"))
	  (typescript--re-search-backward "\\_<do\\_>" (point-at-bol) t)
	  (or (looking-at "\\_<do\\_>")
	      (let ((saved-indent (current-indentation)))
		(while (and (typescript--re-search-backward "^\\s-*\\_<" nil t)
			    (/= (current-indentation) saved-indent)))
		(and (looking-at "\\s-*\\_<do\\_>")
		     (not (typescript--re-search-forward
			   "\\_<while\\_>" (point-at-eol) t))
		     (= (current-indentation) saved-indent)))))))))


(defun typescript--ctrl-statement-indentation ()
  "Helper function for `typescript--proper-indentation'.
Return the proper indentation of the current line if it starts
the body of a control statement without braces; otherwise, return
nil."
  (save-excursion
    (back-to-indentation)
    (when (save-excursion
            (and (not (eq (point-at-bol) (point-min)))
                 (not (looking-at "[{]"))
                 (progn
                   (typescript--re-search-backward "[[:graph:]]" nil t)
                   (or (eobp) (forward-char))
                   (when (= (char-before) ?\)) (backward-list))
                   (skip-syntax-backward " ")
                   (skip-syntax-backward "w_")
                   (looking-at typescript--possibly-braceless-keyword-re))
                 (not (typescript--end-of-do-while-loop-p))))
      (save-excursion
        (goto-char (match-beginning 0))
        (+ (current-indentation) typescript-indent-level)))))

(defun typescript--get-c-offset (symbol anchor)
  (let ((c-offsets-alist
         (list (cons 'c typescript-comment-lineup-func))))
    (c-get-syntactic-indentation (list (cons symbol anchor)))))

(defun typescript--backward-over-generic-parameter-list ()
  "Search backward for the start of a generic's parameter list and move to it.

This is a utility function for
`typescript--backward-to-parameter-list'.

This function must be called with the point placed on the final >
of the generic's parameter list.  It will scan backwards to find
the start.  If successful, it will move the point to the start of
the list.  If not, it does not move the point.

Returns nil on failure, or the position to which the point was
moved on success."
  (when (eq (char-after) ?>)
    (let ((depth 1))
      (loop named search-loop
            while (> depth 0)
            do (progn
                 (unless (re-search-backward "[<>]" nil t)
                   (cl-return-from search-loop nil))
                 (cond
                  ((looking-at ">")
                   (unless (eq (char-before) ?=)
                     (setq depth (1+ depth))))
                  ((looking-at "<") (setq depth (1- depth)))))
            finally return (point)))))

(defun typescript--backward-to-parameter-list ()
  "Search backward for the end of a parameter list and move to it.

This is a utility function for `typescript--proper-indentation'.

This function must be called with the point placed before an
opening curly brace.  It will try to skip over the type
annotation that would mark the return value of a function and
move to the end of the parameter list.  If it is unsuccessful, it
does not move the point. \"Unsuccessful\" here also means that
the position at which we started did not in fact mark the
beginning of a function. The curly brace belonged to some other
syntactic construct than a function.

Returns nil on failure, or the position to which the point was
moved on success."
  (let ((location
         (or
          ;; This handles the case of a function with return type annotation.
          (save-excursion
            (loop named search-loop
                  do (progn
                       (cond
                        ;; Looking at the arrow of an arrow function:
                        ;; move back over the arrow.
                        ((looking-back "=>" (- (point) 2))
                         (backward-char 2))
                        ;; Looking at the end of the parameters list
                        ;; of a generic: move back over the list.
                        ((eq (char-before) ?>)
                         (backward-char)
                         (typescript--backward-over-generic-parameter-list))
                        ;; Looking at a union: skip over the character.
                        ((eq (char-before) ?|)
                         (backward-char))
                        ;; General case: we just move back over the current sexp.
                        (t
                         (condition-case nil
                             (backward-sexp)
                           (scan-error nil))))
                       (typescript--backward-syntactic-ws)
                       (let ((before (char-before)))
                         ;; Check whether we are at "):".
                         (when (and (eq before ?\:)
                                    (progn
                                      (backward-char)
                                      (skip-syntax-backward " ")
                                      (eq (char-before) ?\))))
                           ;; Success! This the end of the parameter list.
                           (cl-return-from search-loop (point)))
                         ;; All the following cases are constructs that are allowed to
                         ;; appear between the opening brace of a function and the
                         ;; end of a parameter list.
                         (unless
                             (or
                              ;; End of a generic.
                              (eq before ?>)
                              ;; Union of types
                              (eq before ?|)
                              ;; Dotted names
                              (eq before ?.)
                              ;; Typeguard (eg. foo is SomeClass)
                              (looking-back "is" (- (point) 2))
                              ;; This is also dealing with dotted names. This may come
                              ;; into play if a jump back moves over an entire dotted
                              ;; name at once.
                              ;;
                              ;; The earlier test for dotted names comes into play if the
                              ;; logic moves over one part of a dotted name at a time (which
                              ;; is what `backward-sexp` normally does).
                              (looking-back typescript--dotted-name-re nil)
                             )
                           ;; We did not encounter a valid construct, so
                           ;; the search is unsuccessful.
                           (cl-return-from search-loop nil))))))
          ;; This handles the case of a function without return type annotation.
          (progn
            (typescript--backward-syntactic-ws)
            (when (eq (char-before) ?\))
              (point))))))
    (when location
      (goto-char location))))

(defun typescript--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)
           (typescript--get-c-offset 'c (nth 8 parse-status)))
          ((nth 8 parse-status) 0) ; inside string
          ((typescript--ctrl-statement-indentation))
          ((eq (char-after) ?#) 0)
          ((save-excursion (typescript--beginning-of-macro)) 4)
          ((nth 1 parse-status)
           (let ((same-indent-p (looking-at
                                 "[]})]\\|\\_<case\\_>\\|\\_<default\\_>"))
                 (continued-expr-p (typescript--continued-expression-p)))
             (goto-char (nth 1 parse-status))
             (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                 (progn
                   (skip-syntax-backward " ")
                   (when (or (typescript--backward-to-parameter-list)
                             (eq (char-before) ?\)))
                     (backward-list))
                   (back-to-indentation)
                   (cond (same-indent-p
                          (current-column))
                         (continued-expr-p
                          (+ (current-column) (* 2 typescript-indent-level)
                             typescript-expr-indent-offset))
                         (t
                          (+ (current-column) typescript-indent-level))))
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (current-column))))

          ((typescript--continued-expression-p)
           (+ typescript-indent-level typescript-expr-indent-offset))
          (t 0))))

(defun typescript-indent-line ()
  "Indent the current line as typescript."
  (interactive)
  (save-restriction
    (widen)
    (let* ((parse-status
            (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation))))
      (indent-line-to (typescript--proper-indentation parse-status))
      (when (> offset 0) (move-to-column (+ offset (current-indentation)))))))

;;; Filling

(defun typescript-c-fill-paragraph (&optional justify)
  "Fill the paragraph with `c-fill-paragraph'."
  (interactive "*P")
  ;; Dynamically replace functions using the lexically scoped cl-letf.
  ;; See below for more details:
  ;; http://endlessparentheses.com/understanding-letf-and-how-it-replaces-flet.html
  (cl-letf (((symbol-function 'c-forward-sws)
             (lambda  (&optional limit)
               (typescript--forward-syntactic-ws limit)))
            ((symbol-function 'c-backward-sws)
             (lambda  (&optional limit)
               (typescript--backward-syntactic-ws limit)))
            ((symbol-function 'c-beginning-of-macro)
             (lambda  (&optional limit)
               (typescript--beginning-of-macro limit))))
    (let ((fill-paragraph-function 'c-fill-paragraph))
      (c-fill-paragraph justify))))

;; We maintain a cache of semantic information, i.e., the classes and
;; functions we've encountered so far. In order to avoid having to
;; re-parse the buffer on every change, we cache the parse state at
;; each interesting point in the buffer. Each parse state is a
;; modified copy of the previous one, or in the case of the first
;; parse state, the empty state.
;;
;; The parse state itself is just a stack of typescript--pitem
;; instances. It starts off containing one element that is never
;; closed, that is initially typescript--initial-pitem.
;;


(defun typescript--pitem-format (pitem)
  (let ((name (typescript--pitem-name pitem))
        (type (typescript--pitem-type pitem)))

    (format "name:%S type:%S"
            name
            (if (atom type)
                type
              (plist-get type :name)))))

(defun typescript--make-merged-item (item child name-parts)
  "Helper function for `typescript--splice-into-items'.
Return a new item that is the result of merging CHILD into
ITEM.  NAME-PARTS is a list of parts of the name of CHILD
that we haven't consumed yet."
  (typescript--debug "typescript--make-merged-item: {%s} into {%s}"
                   (typescript--pitem-format child)
                   (typescript--pitem-format item))

  ;; If the item we're merging into isn't a class, make it into one
  (unless (consp (typescript--pitem-type item))
    (typescript--debug "typescript--make-merged-item: changing dest into class")
    (setq item (make-typescript--pitem
                :children (list item)

                ;; Use the child's class-style if it's available
                :type (if (atom (typescript--pitem-type child))
                          typescript--dummy-class-style
                  (typescript--pitem-type child))

                :name (typescript--pitem-strname item))))

  ;; Now we can merge either a function or a class into a class
  (cons (cond
         ((cdr name-parts)
          (typescript--debug "typescript--make-merged-item: recursing")
          ;; if we have more name-parts to go before we get to the
          ;; bottom of the class hierarchy, call the merger
          ;; recursively
          (typescript--splice-into-items (car item) child
                                       (cdr name-parts)))

         ((atom (typescript--pitem-type child))
          (typescript--debug "typescript--make-merged-item: straight merge")
          ;; Not merging a class, but something else, so just prepend
          ;; it
          (cons child (car item)))

         (t
          ;; Otherwise, merge the new child's items into those
          ;; of the new class
          (typescript--debug "typescript--make-merged-item: merging class contents")
          (append (car child) (car item))))
        (cdr item)))

(defun typescript--pitem-strname (pitem)
  "Last part of the name of PITEM, as a string or symbol."
  (let ((name (typescript--pitem-name pitem)))
    (if (consp name)
        (car (last name))
      name)))

(defun typescript--splice-into-items (items child name-parts)
  "Splice CHILD into the `typescript--pitem' ITEMS at NAME-PARTS.
If a class doesn't exist in the tree, create it.  Return
the new items list.  NAME-PARTS is a list of strings given
the broken-down class name of the item to insert."

  (let ((top-name (car name-parts))
        (item-ptr items)
        new-items last-new-item new-cons item)

    (typescript--debug "typescript--splice-into-items: name-parts: %S items:%S"
             name-parts
             (mapcar #'typescript--pitem-name items))

    (assert (stringp top-name))
    (assert (> (length top-name) 0))

    ;; If top-name isn't found in items, then we build a copy of items
    ;; and throw it away. But that's okay, since most of the time, we
    ;; *will* find an instance.

    (while (and item-ptr
                (cond ((equal (typescript--pitem-strname (car item-ptr)) top-name)
                       ;; Okay, we found an entry with the right name. Splice
                       ;; the merged item into the list...
                       (setq new-cons (cons (typescript--make-merged-item
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
          (typescript--debug "search succeeded: %S" name-parts)
          new-items)

      ;; We didn't find anything. If the child is a class and we don't
      ;; have any classes to drill down into, just push that class;
      ;; otherwise, make a fake class and carry on.
      (typescript--debug "search failed: %S" name-parts)
      (cons (if (cdr name-parts)
                ;; We have name-parts left to process. Make a fake
                ;; class for this particular part...
                (make-typescript--pitem
                 ;; ...and recursively digest the rest of the name
                 :children (typescript--splice-into-items
                            nil child (cdr name-parts))
                 :type typescript--dummy-class-style
                 :name top-name)

              ;; Otherwise, this is the only name we have, so stick
              ;; the item on the front of the list
              child)
            items))))

(defun typescript--pitem-add-child (pitem child)
  "Copy `typescript--pitem' PITEM, and push CHILD onto its list of children."
  (assert (integerp (typescript--pitem-h-begin child)))
  (assert (if (consp (typescript--pitem-name child))
              (loop for part in (typescript--pitem-name child)
                    always (stringp part))
            t))

  ;; This trick works because we know (based on our defstructs) that
  ;; the child list is always the first element, and so the second
  ;; element and beyond can be shared when we make our "copy".
  (cons

   (let ((name (typescript--pitem-name child))
         (type (typescript--pitem-type child)))

     (cond ((cdr-safe name) ; true if a list of at least two elements
            ;; Use slow path because we need class lookup
            (typescript--splice-into-items (car pitem) child name))

           ((and (consp type)
                 (plist-get type :prototype))

            ;; Use slow path because we need class merging. We know
            ;; name is a list here because down in
            ;; `typescript--ensure-cache', we made sure to only add
            ;; class entries with lists for :name
            (assert (consp name))
            (typescript--splice-into-items (car pitem) child name))

           (t
            ;; Fast path
            (cons child (car pitem)))))

   (cdr pitem)))

;;; compilation-mode support

;; handle compiler-errors like the following when doing M-x compile<ret>tsc<ret>
;; greeter.ts(24,9): error TS2362: The left-hand side of an arithmetic operation must be of type 'any', 'number' or an enum type.
;; greeter.ts(30,12): error TS2339: Property 'indexOf' does not exist on type 'number'.
(defconst typescript-tsc-error-regexp
  (concat
   "^[[:blank:]]*"
   "\\([^(\r\n)]+\\)(\\([0-9]+\\),\\([0-9]+\\)):[[:blank:]]+"
   "error [[:alnum:]]+: [^\r\n]+$")
  "Regexp to match errors generated by tsc.")

;;
;; Should handle output like:
;; src/modules/authenticator.ts[1, 83]: ' should be "
;; (quotemarks) src/modules/authenticator.ts[2, 26]: ' should be "
;; ERROR: (quotemarks) src/modules/authenticator.ts[2, 26]: ' should be "
;; WARNING: src/modules/authenticator.ts[2, 26]: ' should be "
;;
;; "(quotemarks)" it the rule name. It is produced when using the
;; "verbose" formatter. The "verbose" formatter is identical to the
;; default ("prose") formatter, except for the additional rule name.
;;
;; "ERROR:" and "WARNING:" are the severity. This was added in tslint
;; 5.0. Prior versions have no notion of severity and simply omit this
;; part.
;;
(defconst typescript-tslint-report-regexp
  (concat
   "^[[:blank:]]*"
   ;; severity ("type" in Emacs' parlance)
   "\\(?:\\(?:ERROR\\|\\(WARNING\\)\\):[[:blank:]]+\\)?"
   ;; rule name
   "\\((.*)[[:blank:]]+\\)?"
   ;; filename
   "\\([^(\r\n)]+\\)"
   "\\["
   ;; line
   "\\([[:digit:]]+\\)"
   ", "
   ;; column
   "\\([[:digit:]]+\\)"
   "\\]: "
   ;; message
   ".*$"
   )
  "Regexp to match reports generated by tslint.")

(dolist
    (regexp
     `((typescript-tsc
        ,typescript-tsc-error-regexp
        1 2 3 2)

       (typescript-tslint
        ,typescript-tslint-report-regexp
        3 4 5 (1))))
  (add-to-list 'compilation-error-regexp-alist-alist regexp)
  (add-to-list 'compilation-error-regexp-alist (car regexp)))

;;; Main Function

(defalias 'typescript-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode typescript-mode typescript-parent-mode "typescript"
  "Major mode for editing typescript.

Key bindings:

\\{typescript-mode-map}"

  :group 'typescript
  :syntax-table typescript-mode-syntax-table

  (set (make-local-variable 'indent-line-function) 'typescript-indent-line)
  (set (make-local-variable 'beginning-of-defun-function)
       'typescript-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'typescript-end-of-defun)

  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)
  (set (make-local-variable 'font-lock-defaults)
       (list typescript--font-lock-keywords
	     nil nil nil nil
	     '(font-lock-syntactic-keywords
               . typescript-font-lock-syntactic-keywords)))

  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)

  ;; Comments
  (setq comment-start "// ")
  (setq comment-end "")
  (set (make-local-variable 'fill-paragraph-function)
       'typescript-c-fill-paragraph)

  ;; Parse cache
  (add-hook 'before-change-functions #'typescript--flush-caches t t)

  ;; Frameworks
  (typescript--update-quick-match-re)

  (setq major-mode 'typescript-mode)
  (setq mode-name "typescript")

  ;; for filling, pretend we're cc-mode
  (setq c-comment-prefix-regexp "//+\\|\\**"
        c-paragraph-start "$"
        c-paragraph-separate "$"
        c-block-comment-prefix "* "
        c-line-comment-starter "//"
        c-comment-start-regexp "/[*/]\\|\\s!"
        comment-start-skip "\\(//+\\|/\\*+\\)\\s *")

  (let ((c-buffer-is-cc-mode t))
    ;; FIXME: These are normally set by `c-basic-common-init'.  Should
    ;; we call it instead?  (Bug#6071)
    (make-local-variable 'paragraph-start)
    (make-local-variable 'paragraph-separate)
    (make-local-variable 'paragraph-ignore-fill-prefix)
    (make-local-variable 'adaptive-fill-mode)
    (make-local-variable 'adaptive-fill-regexp)
    (c-setup-paragraph-variables))

  (set (make-local-variable 'syntax-begin-function)
       #'typescript--syntax-begin-function))

;; Set our custom predicate for flyspell prog mode
(put 'typescript-mode 'flyspell-mode-predicate
     'typescript--flyspell-mode-predicate)

;;;###autoload
(eval-after-load 'folding
  '(when (fboundp 'folding-add-to-marks-list)
     (folding-add-to-marks-list 'typescript-mode "// {{{" "// }}}" )))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))

(provide 'typescript-mode)

;;; typescript-mode.el ends here
