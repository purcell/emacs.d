;;; js2-mode.el --- Improved JavaScript editing mode

;; Copyright (C) 2009, 2011-2013  Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;;         mooz <stillpedant@gmail.com>
;;         Dmitry Gutov <dgutov@yandex.ru>
;; URL:  https://github.com/mooz/js2-mode/
;;       http://code.google.com/p/js2-mode/
;; Version: 20130619
;; Keywords: languages, javascript
;; Package-Requires: ((emacs "24.1"))

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

;; This JavaScript editing mode supports:

;;  - strict recognition of the Ecma-262 language standard
;;  - support for most Rhino and SpiderMonkey extensions from 1.5 and up
;;  - parsing support for ECMAScript for XML (E4X, ECMA-357)
;;  - accurate syntax highlighting using a recursive-descent parser
;;  - on-the-fly reporting of syntax errors and strict-mode warnings
;;  - undeclared-variable warnings using a configurable externs framework
;;  - "bouncing" line indentation to choose among alternate indentation points
;;  - smart line-wrapping within comments and strings
;;  - code folding:
;;    - show some or all function bodies as {...}
;;    - show some or all block comments as /*...*/
;;  - context-sensitive menu bar and popup menus
;;  - code browsing using the `imenu' package
;;  - many customization options

;; Installation:
;;
;; To install it as your major mode for JavaScript editing:

;;   (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Alternatively, to install it as a minor mode just for JavaScript linting,
;; you must add it to the appropriate major-mode hook.  Normally this would be:

;;   (add-hook 'js-mode-hook 'js2-minor-mode)

;; You may also want to hook it in for shell scripts running via node.js:

;;   (add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; To customize how it works:
;;   M-x customize-group RET js2-mode RET

;; Notes:

;; This mode includes a port of Mozilla Rhino's scanner, parser and
;; symbol table.  Ideally it should stay in sync with Rhino, keeping
;; `js2-mode' current as the EcmaScript language standard evolves.

;; Unlike cc-engine based language modes, js2-mode's line-indentation is not
;; customizable.  It is a surprising amount of work to support customizable
;; indentation.  The current compromise is that the tab key lets you cycle among
;; various likely indentation points, similar to the behavior of python-mode.

;; This mode does not yet work with "multi-mode" modes such as `mmm-mode'
;; and `mumamo', although it could be made to do so with some effort.
;; This means that `js2-mode' is currently only useful for editing JavaScript
;; files, and not for editing JavaScript within <script> tags or templates.

;; The project page on GitHub is used for development and issue tracking.
;; The original homepage at Google Code has outdated information and is mostly
;; unmaintained.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'imenu)
(require 'cc-cmds)  ; for `c-fill-paragraph'

(eval-and-compile
  (require 'cc-mode)     ; (only) for `c-populate-syntax-table'
  (require 'cc-engine))  ; for `c-paragraph-start' et. al.

(defvar electric-layout-rules)

;;; Externs (variables presumed to be defined by the host system)

(defvar js2-ecma-262-externs
  (mapcar 'symbol-name
          '(Array Boolean Date Error EvalError Function Infinity JSON
          Math NaN Number Object RangeError ReferenceError RegExp
          String SyntaxError TypeError URIError arguments
          decodeURI decodeURIComponent encodeURI
          encodeURIComponent escape eval isFinite isNaN
          parseFloat parseInt undefined unescape))
"Ecma-262 externs.  Included in `js2-externs' by default.")

(defvar js2-browser-externs
  (mapcar 'symbol-name
          '(;; DOM level 1
            Attr CDATASection CharacterData Comment DOMException
            DOMImplementation Document DocumentFragment
            DocumentType Element Entity EntityReference
            ExceptionCode NamedNodeMap Node NodeList Notation
            ProcessingInstruction Text

            ;; DOM level 2
            HTMLAnchorElement HTMLAppletElement HTMLAreaElement
            HTMLBRElement HTMLBaseElement HTMLBaseFontElement
            HTMLBodyElement HTMLButtonElement HTMLCollection
            HTMLDListElement HTMLDirectoryElement HTMLDivElement
            HTMLDocument HTMLElement HTMLFieldSetElement
            HTMLFontElement HTMLFormElement HTMLFrameElement
            HTMLFrameSetElement HTMLHRElement HTMLHeadElement
            HTMLHeadingElement HTMLHtmlElement HTMLIFrameElement
            HTMLImageElement HTMLInputElement HTMLIsIndexElement
            HTMLLIElement HTMLLabelElement HTMLLegendElement
            HTMLLinkElement HTMLMapElement HTMLMenuElement
            HTMLMetaElement HTMLModElement HTMLOListElement
            HTMLObjectElement HTMLOptGroupElement
            HTMLOptionElement HTMLOptionsCollection
            HTMLParagraphElement HTMLParamElement HTMLPreElement
            HTMLQuoteElement HTMLScriptElement HTMLSelectElement
            HTMLStyleElement HTMLTableCaptionElement
            HTMLTableCellElement HTMLTableColElement
            HTMLTableElement HTMLTableRowElement
            HTMLTableSectionElement HTMLTextAreaElement
            HTMLTitleElement HTMLUListElement

            ;; DOM level 3
            DOMConfiguration DOMError DOMException
            DOMImplementationList DOMImplementationSource
            DOMLocator DOMStringList NameList TypeInfo
            UserDataHandler

            ;; Window
            window alert confirm document java navigator prompt screen
            self top

            ;; W3C CSS
            CSSCharsetRule CSSFontFace CSSFontFaceRule
            CSSImportRule CSSMediaRule CSSPageRule
            CSSPrimitiveValue CSSProperties CSSRule CSSRuleList
            CSSStyleDeclaration CSSStyleRule CSSStyleSheet
            CSSValue CSSValueList Counter DOMImplementationCSS
            DocumentCSS DocumentStyle ElementCSSInlineStyle
            LinkStyle MediaList RGBColor Rect StyleSheet
            StyleSheetList ViewCSS

            ;; W3C Event
            EventListener EventTarget Event DocumentEvent UIEvent
            MouseEvent MutationEvent KeyboardEvent

            ;; W3C Range
            DocumentRange Range RangeException

            ;; W3C XML
            XPathResult XMLHttpRequest

            ;; console object.  Provided by at least Chrome and Firefox.
            console))
  "Browser externs.
You can cause these to be included or excluded with the custom
variable `js2-include-browser-externs'.")

(defvar js2-rhino-externs
  (mapcar 'symbol-name
          '(Packages importClass importPackage com org java
            ;; Global object (shell) externs.
            defineClass deserialize doctest gc help load
            loadClass print quit readFile readUrl runCommand seal
            serialize spawn sync toint32 version))
  "Mozilla Rhino externs.
Set `js2-include-rhino-externs' to t to include them.")

(defvar js2-node-externs
  (mapcar 'symbol-name
          '(__dirname __filename Buffer clearInterval clearTimeout require
            console exports global module process setInterval setTimeout))
  "Node.js externs.
Set `js2-include-node-externs' to t to include them.")

(defvar js2-typed-array-externs
  (mapcar 'symbol-name
          '(ArrayBuffer Uint8ClampedArray DataView
            Int8Array Uint8Array Int16Array Uint16Array Int32Array Uint32Array
            Float32Array Float64Array))
  "Khronos typed array externs. Available in most modern browsers and
in node.js >= 0.6. If `js2-include-node-externs' or `js2-include-browser-externs'
are enabled, these will also be included.")

;;; Variables

(defun js2-mark-safe-local (name pred)
  "Make the variable NAME buffer-local and mark it as safe file-local
variable with predicate PRED."
  (make-variable-buffer-local name)
  (put name 'safe-local-variable pred))

(defcustom js2-highlight-level 2
  "Amount of syntax highlighting to perform.
0 or a negative value means none.
1 adds basic syntax highlighting.
2 adds highlighting of some Ecma built-in properties.
3 adds highlighting of many Ecma built-in functions."
  :group 'js2-mode
  :type '(choice (const :tag "None" 0)
                 (const :tag "Basic" 1)
                 (const :tag "Include Properties" 2)
                 (const :tag "Include Functions" 3)))

(defvar js2-mode-dev-mode-p nil
  "Non-nil if running in development mode.  Normally nil.")

(defgroup js2-mode nil
  "An improved JavaScript mode."
  :group 'languages)

(defcustom js2-basic-offset (if (and (boundp 'c-basic-offset)
                                     (numberp c-basic-offset))
                                c-basic-offset
                              4)
  "Number of spaces to indent nested statements.
Similar to `c-basic-offset'."
  :group 'js2-mode
  :type 'integer)
(js2-mark-safe-local 'js2-basic-offset 'integerp)

(defcustom js2-bounce-indent-p nil
  "Non-nil to have indent-line function choose among alternatives.
If nil, the indent-line function will indent to a predetermined column
based on heuristic guessing.  If non-nil, then if the current line is
already indented to that predetermined column, indenting will choose
another likely column and indent to that spot.  Repeated invocation of
the indent-line function will cycle among the computed alternatives.
See the function `js2-bounce-indent' for details.  When it is non-nil,
js2-mode also binds `js2-bounce-indent-backwards' to Shift-Tab."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-pretty-multiline-declarations t
  "Non-nil to line up multiline declarations vertically:

  var a = 10,
      b = 20,
      c = 30;

If the value is not `all', and the first assigned value in
declaration is a function/array/object literal spanning several
lines, it won't be indented additionally:

  var o = {                   var bar = 2,
    foo: 3          vs.           o = {
  },                                foo: 3
      bar = 2;                    };"
  :group 'js2-mode
  :type 'symbol)
(js2-mark-safe-local 'js2-pretty-multiline-declarations 'symbolp)

(defcustom js2-indent-switch-body nil
  "When nil, case labels are indented on the same level as the
containing switch statement.  Otherwise, all lines inside
switch statement body are indented one additional level."
  :type 'boolean
  :group 'js2-mode)
(js2-mark-safe-local 'js2-indent-case-same-as-switch 'booleanp)

(defcustom js2-idle-timer-delay 0.2
  "Delay in secs before re-parsing after user makes changes.
Multiplied by `js2-dynamic-idle-timer-adjust', which see."
  :type 'number
  :group 'js2-mode)
(make-variable-buffer-local 'js2-idle-timer-delay)

(defcustom js2-dynamic-idle-timer-adjust 0
  "Positive to adjust `js2-idle-timer-delay' based on file size.
The idea is that for short files, parsing is faster so we can be
more responsive to user edits without interfering with editing.
The buffer length in characters (typically bytes) is divided by
this value and used to multiply `js2-idle-timer-delay' for the
buffer.  For example, a 21k file and 10k adjust yields 21k/10k
== 2, so js2-idle-timer-delay is multiplied by 2.
If `js2-dynamic-idle-timer-adjust' is 0 or negative,
`js2-idle-timer-delay' is not dependent on the file size."
  :type 'number
  :group 'js2-mode)

(defcustom js2-concat-multiline-strings t
  "When non-nil, `js2-line-break' in mid-string will make it a
string concatenation. When `eol', the '+' will be inserted at the
end of the line, otherwise, at the beginning of the next line."
  :type '(choice (const t) (const eol) (const nil))
  :group 'js2-mode)

(defcustom js2-mode-show-parse-errors t
  "True to highlight parse errors."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-mode-show-strict-warnings t
  "Non-nil to emit Ecma strict-mode warnings.
Some of the warnings can be individually disabled by other flags,
even if this flag is non-nil."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-trailing-comma-warning t
  "Non-nil to warn about trailing commas in array literals.
Ecma-262-5.1 allows them, but older versions of IE raise an error."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-missing-semi-warning t
  "Non-nil to warn about semicolon auto-insertion after statement.
Technically this is legal per Ecma-262, but some style guides disallow
depending on it."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-missing-semi-one-line-override nil
  "Non-nil to permit missing semicolons in one-line functions.
In one-liner functions such as `function identity(x) {return x}'
people often omit the semicolon for a cleaner look.  If you are
such a person, you can suppress the missing-semicolon warning
by setting this variable to t."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-inconsistent-return-warning t
  "Non-nil to warn about mixing returns with value-returns.
It's perfectly legal to have a `return' and a `return foo' in the
same function, but it's often an indicator of a bug, and it also
interferes with type inference (in systems that support it.)"
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-cond-assign-warning t
  "Non-nil to warn about expressions like if (a = b).
This often should have been '==' instead of '='.  If the warning
is enabled, you can suppress it on a per-expression basis by
parenthesizing the expression, e.g. if ((a = b)) ..."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-var-redeclaration-warning t
  "Non-nil to warn about redeclaring variables in a script or function."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-var-hides-function-arg-warning t
  "Non-nil to warn about a var decl hiding a function argument."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-skip-preprocessor-directives nil
  "Non-nil to treat lines beginning with # as comments.
Useful for viewing Mozilla JavaScript source code."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-language-version 200
  "Configures what JavaScript language version to recognize.
Currently versions 150, 160, 170, 180 and 200 are supported,
corresponding to JavaScript 1.5, 1.6, 1.7, 1.8 and 2.0 (Harmony),
respectively.  In a nutshell, 1.6 adds E4X support, 1.7 adds let,
yield, and Array comprehensions, and 1.8 adds function closures."
  :type 'integer
  :group 'js2-mode)

(defcustom js2-allow-keywords-as-property-names t
  "If non-nil, you can use JavaScript keywords as object property names.
Examples:

  var foo = {int: 5, while: 6, continue: 7};
  foo.return = 8;

Ecma-262 5.1 allows this syntax, but some engines still don't."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-instanceof-has-side-effects nil
  "If non-nil, treats the instanceof operator as having side effects.
This is useful for xulrunner apps."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-move-point-on-right-click t
  "Non-nil to move insertion point when you right-click.
This makes right-click context menu behavior a bit more intuitive,
since menu operations generally apply to the point.  The exception
is if there is a region selection, in which case the point does -not-
move, so cut/copy/paste can work properly.

Note that IntelliJ moves the point, and Eclipse leaves it alone,
so this behavior is customizable."
  :group 'js2-mode
  :type 'boolean)

(defcustom js2-allow-rhino-new-expr-initializer t
  "Non-nil to support a Rhino's experimental syntactic construct.

Rhino supports the ability to follow a `new' expression with an object
literal, which is used to set additional properties on the new object
after calling its constructor.  Syntax:

  new <expr> [ ( arglist ) ] [initializer]

Hence, this expression:

  new Object {a: 1, b: 2}

results in an Object with properties a=1 and b=2.  This syntax is
apparently not configurable in Rhino - it's currently always enabled,
as of Rhino version 1.7R2."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-allow-member-expr-as-function-name nil
  "Non-nil to support experimental Rhino syntax for function names.

Rhino supports an experimental syntax configured via the Rhino Context
setting `allowMemberExprAsFunctionName'.  The experimental syntax is:

  function <member-expr> ( [ arg-list ] ) { <body> }

Where member-expr is a non-parenthesized 'member expression', which
is anything at the grammar level of a new-expression or lower, meaning
any expression that does not involve infix or unary operators.

When <member-expr> is not a simple identifier, then it is syntactic
sugar for assigning the anonymous function to the <member-expr>.  Hence,
this code:

  function a.b().c[2] (x, y) { ... }

is rewritten as:

  a.b().c[2] = function(x, y) {...}

which doesn't seem particularly useful, but Rhino permits it."
  :type 'boolean
  :group 'js2-mode)

;; scanner variables

(defmacro js2-deflocal (name value &optional comment)
  "Define a buffer-local variable NAME with VALUE and COMMENT."
  `(progn
     (defvar ,name ,value ,comment)
     (make-variable-buffer-local ',name)))

(defvar js2-EOF_CHAR -1
  "Represents end of stream.  Distinct from js2-EOF token type.")

;; I originally used symbols to represent tokens, but Rhino uses
;; ints and then sets various flag bits in them, so ints it is.
;; The upshot is that we need a `js2-' prefix in front of each name.
(defvar js2-ERROR -1)
(defvar js2-EOF 0)
(defvar js2-EOL 1)
(defvar js2-ENTERWITH 2)       ; begin interpreter bytecodes
(defvar js2-LEAVEWITH 3)
(defvar js2-RETURN 4)
(defvar js2-GOTO 5)
(defvar js2-IFEQ 6)
(defvar js2-IFNE 7)
(defvar js2-SETNAME 8)
(defvar js2-BITOR 9)
(defvar js2-BITXOR 10)
(defvar js2-BITAND 11)
(defvar js2-EQ 12)
(defvar js2-NE 13)
(defvar js2-LT 14)
(defvar js2-LE 15)
(defvar js2-GT 16)
(defvar js2-GE 17)
(defvar js2-LSH 18)
(defvar js2-RSH 19)
(defvar js2-URSH 20)
(defvar js2-ADD 21)            ; infix plus
(defvar js2-SUB 22)            ; infix minus
(defvar js2-MUL 23)
(defvar js2-DIV 24)
(defvar js2-MOD 25)
(defvar js2-NOT 26)
(defvar js2-BITNOT 27)
(defvar js2-POS 28)            ; unary plus
(defvar js2-NEG 29)            ; unary minus
(defvar js2-NEW 30)
(defvar js2-DELPROP 31)
(defvar js2-TYPEOF 32)
(defvar js2-GETPROP 33)
(defvar js2-GETPROPNOWARN 34)
(defvar js2-SETPROP 35)
(defvar js2-GETELEM 36)
(defvar js2-SETELEM 37)
(defvar js2-CALL 38)
(defvar js2-NAME 39)           ; an identifier
(defvar js2-NUMBER 40)
(defvar js2-STRING 41)
(defvar js2-NULL 42)
(defvar js2-THIS 43)
(defvar js2-FALSE 44)
(defvar js2-TRUE 45)
(defvar js2-SHEQ 46)           ; shallow equality (===)
(defvar js2-SHNE 47)           ; shallow inequality (!==)
(defvar js2-REGEXP 48)
(defvar js2-BINDNAME 49)
(defvar js2-THROW 50)
(defvar js2-RETHROW 51)        ; rethrow caught exception: catch (e if ) uses it
(defvar js2-IN 52)
(defvar js2-INSTANCEOF 53)
(defvar js2-LOCAL_LOAD 54)
(defvar js2-GETVAR 55)
(defvar js2-SETVAR 56)
(defvar js2-CATCH_SCOPE 57)
(defvar js2-ENUM_INIT_KEYS 58)
(defvar js2-ENUM_INIT_VALUES 59)
(defvar js2-ENUM_INIT_ARRAY 60)
(defvar js2-ENUM_NEXT 61)
(defvar js2-ENUM_ID 62)
(defvar js2-THISFN 63)
(defvar js2-RETURN_RESULT 64)  ; to return previously stored return result
(defvar js2-ARRAYLIT 65)       ; array literal
(defvar js2-OBJECTLIT 66)      ; object literal
(defvar js2-GET_REF 67)        ; *reference
(defvar js2-SET_REF 68)        ; *reference = something
(defvar js2-DEL_REF 69)        ; delete reference
(defvar js2-REF_CALL 70)       ; f(args) = something or f(args)++
(defvar js2-REF_SPECIAL 71)    ; reference for special properties like __proto
(defvar js2-YIELD 72)          ; JS 1.7 yield pseudo keyword

;; XML support
(defvar js2-DEFAULTNAMESPACE 73)
(defvar js2-ESCXMLATTR 74)
(defvar js2-ESCXMLTEXT 75)
(defvar js2-REF_MEMBER 76)     ; Reference for x.@y, x..y etc.
(defvar js2-REF_NS_MEMBER 77)  ; Reference for x.ns::y, x..ns::y etc.
(defvar js2-REF_NAME 78)       ; Reference for @y, @[y] etc.
(defvar js2-REF_NS_NAME 79)    ; Reference for ns::y, @ns::y@[y] etc.

(defvar js2-first-bytecode js2-ENTERWITH)
(defvar js2-last-bytecode js2-REF_NS_NAME)

(defvar js2-TRY 80)
(defvar js2-SEMI 81)           ; semicolon
(defvar js2-LB 82)             ; left and right brackets
(defvar js2-RB 83)
(defvar js2-LC 84)             ; left and right curly-braces
(defvar js2-RC 85)
(defvar js2-LP 86)             ; left and right parens
(defvar js2-RP 87)
(defvar js2-COMMA 88)          ; comma operator

(defvar js2-ASSIGN 89)         ; simple assignment (=)
(defvar js2-ASSIGN_BITOR 90)   ; |=
(defvar js2-ASSIGN_BITXOR 91)  ; ^=
(defvar js2-ASSIGN_BITAND 92)  ; &=
(defvar js2-ASSIGN_LSH 93)     ; <<=
(defvar js2-ASSIGN_RSH 94)     ; >>=
(defvar js2-ASSIGN_URSH 95)    ; >>>=
(defvar js2-ASSIGN_ADD 96)     ; +=
(defvar js2-ASSIGN_SUB 97)     ; -=
(defvar js2-ASSIGN_MUL 98)     ; *=
(defvar js2-ASSIGN_DIV 99)     ; /=
(defvar js2-ASSIGN_MOD 100)    ; %=

(defvar js2-first-assign js2-ASSIGN)
(defvar js2-last-assign js2-ASSIGN_MOD)

(defvar js2-HOOK 101)          ; conditional (?:)
(defvar js2-COLON 102)
(defvar js2-OR 103)            ; logical or (||)
(defvar js2-AND 104)           ; logical and (&&)
(defvar js2-INC 105)           ; increment/decrement (++ --)
(defvar js2-DEC 106)
(defvar js2-DOT 107)           ; member operator (.)
(defvar js2-FUNCTION 108)      ; function keyword
(defvar js2-EXPORT 109)        ; export keyword
(defvar js2-IMPORT 110)        ; import keyword
(defvar js2-IF 111)            ; if keyword
(defvar js2-ELSE 112)          ; else keyword
(defvar js2-SWITCH 113)        ; switch keyword
(defvar js2-CASE 114)          ; case keyword
(defvar js2-DEFAULT 115)       ; default keyword
(defvar js2-WHILE 116)         ; while keyword
(defvar js2-DO 117)            ; do keyword
(defvar js2-FOR 118)           ; for keyword
(defvar js2-BREAK 119)         ; break keyword
(defvar js2-CONTINUE 120)      ; continue keyword
(defvar js2-VAR 121)           ; var keyword
(defvar js2-WITH 122)          ; with keyword
(defvar js2-CATCH 123)         ; catch keyword
(defvar js2-FINALLY 124)       ; finally keyword
(defvar js2-VOID 125)          ; void keyword
(defvar js2-RESERVED 126)      ; reserved keywords

(defvar js2-EMPTY 127)

;; Types used for the parse tree - never returned by scanner.

(defvar js2-BLOCK 128)         ; statement block
(defvar js2-LABEL 129)         ; label
(defvar js2-TARGET 130)
(defvar js2-LOOP 131)
(defvar js2-EXPR_VOID 132)     ; expression statement in functions
(defvar js2-EXPR_RESULT 133)   ; expression statement in scripts
(defvar js2-JSR 134)
(defvar js2-SCRIPT 135)        ; top-level node for entire script
(defvar js2-TYPEOFNAME 136)    ; for typeof(simple-name)
(defvar js2-USE_STACK 137)
(defvar js2-SETPROP_OP 138)    ; x.y op= something
(defvar js2-SETELEM_OP 139)    ; x[y] op= something
(defvar js2-LOCAL_BLOCK 140)
(defvar js2-SET_REF_OP 141)    ; *reference op= something

;; For XML support:
(defvar js2-DOTDOT 142)        ; member operator (..)
(defvar js2-COLONCOLON 143)    ; namespace::name
(defvar js2-XML 144)           ; XML type
(defvar js2-DOTQUERY 145)      ; .() -- e.g., x.emps.emp.(name == "terry")
(defvar js2-XMLATTR 146)       ; @
(defvar js2-XMLEND 147)

;; Optimizer-only tokens
(defvar js2-TO_OBJECT 148)
(defvar js2-TO_DOUBLE 149)

(defvar js2-GET 150)           ; JS 1.5 get pseudo keyword
(defvar js2-SET 151)           ; JS 1.5 set pseudo keyword
(defvar js2-LET 152)           ; JS 1.7 let pseudo keyword
(defvar js2-CONST 153)
(defvar js2-SETCONST 154)
(defvar js2-SETCONSTVAR 155)
(defvar js2-ARRAYCOMP 156)
(defvar js2-LETEXPR 157)
(defvar js2-WITHEXPR 158)
(defvar js2-DEBUGGER 159)

(defvar js2-COMMENT 160)
(defvar js2-ENUM 161)          ; for "enum" reserved word
(defvar js2-TRIPLEDOT 162)     ; for rest parameter
(defvar js2-ARROW 163)         ; function arrow (=>)

(defconst js2-num-tokens (1+ js2-ARROW))

(defconst js2-debug-print-trees nil)

;; Rhino accepts any string or stream as input.  Emacs character
;; processing works best in buffers, so we'll assume the input is a
;; buffer.  JavaScript strings can be copied into temp buffers before
;; scanning them.

;; Buffer-local variables yield much cleaner code than using `defstruct'.
;; They're the Emacs equivalent of instance variables, more or less.

(js2-deflocal js2-ts-dirty-line nil
  "Token stream buffer-local variable.
Indicates stuff other than whitespace since start of line.")

(js2-deflocal js2-ts-hit-eof nil
  "Token stream buffer-local variable.")

;; FIXME: Unused.
(js2-deflocal js2-ts-line-start 0
  "Token stream buffer-local variable.")

(js2-deflocal js2-ts-lineno 1
  "Token stream buffer-local variable.")

;; FIXME: Unused.
(js2-deflocal js2-ts-line-end-char -1
  "Token stream buffer-local variable.")

(js2-deflocal js2-ts-cursor 1  ; emacs buffers are 1-indexed
  "Token stream buffer-local variable.
Current scan position.")

;; FIXME: Unused.
(js2-deflocal js2-ts-is-xml-attribute nil
  "Token stream buffer-local variable.")

(js2-deflocal js2-ts-xml-is-tag-content nil
  "Token stream buffer-local variable.")

(js2-deflocal js2-ts-xml-open-tags-count 0
  "Token stream buffer-local variable.")

(js2-deflocal js2-ts-string-buffer nil
  "Token stream buffer-local variable.
List of chars built up while scanning various tokens.")

(defstruct (js2-token
            (:constructor nil)
            (:constructor make-js2-token (beg)))
  "Value returned from the token stream."
  (type js2-EOF)
  (beg 1)
  (end -1)
  (string "")
  number
  regexp-flags
  comment-type
  follows-eol-p)

(defstruct (js2-ts-state
            (:constructor make-js2-ts-state (&key (lineno js2-ts-lineno)
                                                  (cursor js2-ts-cursor)
                                                  (tokens (copy-sequence js2-ti-tokens))
                                                  (tokens-cursor js2-ti-tokens-cursor)
                                                  (lookahead js2-ti-lookahead))))
  lineno
  cursor
  tokens
  tokens-cursor
  lookahead)

;;; Parser variables

(js2-deflocal js2-parsed-errors nil
  "List of errors produced during scanning/parsing.")

(js2-deflocal js2-parsed-warnings nil
  "List of warnings produced during scanning/parsing.")

(js2-deflocal js2-recover-from-parse-errors t
  "Non-nil to continue parsing after a syntax error.

In recovery mode, the AST will be built in full, and any error
nodes will be flagged with appropriate error information.  If
this flag is nil, a syntax error will result in an error being
signaled.

The variable is automatically buffer-local, because different
modes that use the parser will need different settings.")

(js2-deflocal js2-parse-hook nil
  "List of callbacks for receiving parsing progress.")

(defvar js2-parse-finished-hook nil
  "List of callbacks to notify when parsing finishes.
Not called if parsing was interrupted.")

(js2-deflocal js2-is-eval-code nil
  "True if we're evaluating code in a string.
If non-nil, the tokenizer will record the token text, and the AST nodes
will record their source text.  Off by default for IDE modes, since the
text is available in the buffer.")

(defvar js2-parse-ide-mode t
  "Non-nil if the parser is being used for `js2-mode'.
If non-nil, the parser will set text properties for fontification
and the syntax table.  The value should be nil when using the
parser as a frontend to an interpreter or byte compiler.")

;;; Parser instance variables (buffer-local vars for js2-parse)

(defconst js2-ti-after-eol (lsh 1 16)
  "Flag:  first token of the source line.")

;; Inline Rhino's CompilerEnvirons vars as buffer-locals.

(js2-deflocal js2-compiler-generate-debug-info t)
(js2-deflocal js2-compiler-use-dynamic-scope nil)
(js2-deflocal js2-compiler-reserved-keywords-as-identifier nil)
(js2-deflocal js2-compiler-xml-available t)
(js2-deflocal js2-compiler-optimization-level 0)
(js2-deflocal js2-compiler-generating-source t)
(js2-deflocal js2-compiler-strict-mode nil)
(js2-deflocal js2-compiler-report-warning-as-error nil)
(js2-deflocal js2-compiler-generate-observer-count nil)
(js2-deflocal js2-compiler-activation-names nil)

;; SKIP:  sourceURI

;; There's a compileFunction method in Context.java - may need it.
(js2-deflocal js2-called-by-compile-function nil
  "True if `js2-parse' was called by `js2-compile-function'.
Will only be used when we finish implementing the interpreter.")

;; SKIP:  ts  (we just call `js2-init-scanner' and use its vars)

;; SKIP:  node factory - we're going to just call functions directly,
;; and eventually go to a unified AST format.

(js2-deflocal js2-nesting-of-function 0)

(js2-deflocal js2-recorded-identifiers nil
  "Tracks identifiers found during parsing.")

(js2-deflocal js2-is-in-destructuring nil
  "True while parsing destructuring expression.")

(defcustom js2-global-externs nil
  "A list of any extern names you'd like to consider always declared.
This list is global and is used by all `js2-mode' files.
You can create buffer-local externs list using `js2-additional-externs'.

There is also a buffer-local variable `js2-default-externs',
which is initialized by default to include the Ecma-262 externs
and the standard browser externs.  The three lists are all
checked during highlighting."
  :type 'list
  :group 'js2-mode)

(js2-deflocal js2-default-externs nil
  "Default external declarations.

These are currently only used for highlighting undeclared variables,
which only worries about top-level (unqualified) references.
As js2-mode's processing improves, we will flesh out this list.

The initial value is set to `js2-ecma-262-externs', unless some
of the `js2-include-?-externs' variables are set to t, in which
case the browser, Rhino and/or Node.js externs are also included.

See `js2-additional-externs' for more information.")

(defcustom js2-include-browser-externs t
  "Non-nil to include browser externs in the master externs list.
If you work on JavaScript files that are not intended for browsers,
such as Mozilla Rhino server-side JavaScript, set this to nil.
See `js2-additional-externs' for more information about externs."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-include-rhino-externs nil
  "Non-nil to include Mozilla Rhino externs in the master externs list.
See `js2-additional-externs' for more information about externs."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-include-node-externs nil
  "Non-nil to include Node.js externs in the master externs list.
See `js2-additional-externs' for more information about externs."
  :type 'boolean
  :group 'js2-mode)

(js2-deflocal js2-additional-externs nil
  "A buffer-local list of additional external declarations.
It is used to decide whether variables are considered undeclared
for purposes of highlighting.

Each entry is a Lisp string.  The string should be the fully qualified
name of an external entity.  All externs should be added to this list,
so that as js2-mode's processing improves it can take advantage of them.

You may want to declare your externs in three ways.
First, you can add externs that are valid for all your JavaScript files.
You should probably do this by adding them to `js2-global-externs', which
is a global list used for all js2-mode files.

Next, you can add a function to `js2-init-hook' that adds additional
externs appropriate for the specific file, perhaps based on its path.
These should go in `js2-additional-externs', which is buffer-local.

Third, you can use JSLint's global declaration, as long as
`js2-include-jslint-globals' is non-nil, which see.

Finally, you can add a function to `js2-post-parse-callbacks',
which is called after parsing completes, and `js2-mode-ast' is bound to
the root of the parse tree.  At this stage you can set up an AST
node visitor using `js2-visit-ast' and examine the parse tree
for specific import patterns that may imply the existence of
other externs, possibly tied to your build system.  These should also
be added to `js2-additional-externs'.

Your post-parse callback may of course also use the simpler and
faster (but perhaps less robust) approach of simply scanning the
buffer text for your imports, using regular expressions.")

;; SKIP:  decompiler
;; SKIP:  encoded-source

;;; The following variables are per-function and should be saved/restored
;;; during function parsing...

(js2-deflocal js2-current-script-or-fn nil)
(js2-deflocal js2-current-scope nil)
(js2-deflocal js2-nesting-of-with 0)
(js2-deflocal js2-label-set nil
  "An alist mapping label names to nodes.")

(js2-deflocal js2-loop-set nil)
(js2-deflocal js2-loop-and-switch-set nil)
(js2-deflocal js2-has-return-value nil)
(js2-deflocal js2-end-flags 0)

;;; ...end of per function variables

;; These flags enumerate the possible ways a statement/function can
;; terminate. These flags are used by endCheck() and by the Parser to
;; detect inconsistent return usage.
;;
;; END_UNREACHED is reserved for code paths that are assumed to always be
;; able to execute (example: throw, continue)
;;
;; END_DROPS_OFF indicates if the statement can transfer control to the
;; next one. Statement such as return dont. A compound statement may have
;; some branch that drops off control to the next statement.
;;
;; END_RETURNS indicates that the statement can return (without arguments)
;; END_RETURNS_VALUE indicates that the statement can return a value.
;;
;; A compound statement such as
;; if (condition) {
;;   return value;
;; }
;; Will be detected as (END_DROPS_OFF | END_RETURN_VALUE) by endCheck()

(defconst js2-end-unreached     #x0)
(defconst js2-end-drops-off     #x1)
(defconst js2-end-returns       #x2)
(defconst js2-end-returns-value #x4)
(defconst js2-end-yields        #x8)

;; Rhino awkwardly passes a statementLabel parameter to the
;; statementHelper() function, the main statement parser, which
;; is then used by quite a few of the sub-parsers.  We just make
;; it a buffer-local variable and make sure it's cleaned up properly.
(js2-deflocal js2-labeled-stmt nil)  ; type `js2-labeled-stmt-node'

;; Similarly, Rhino passes an inForInit boolean through about half
;; the expression parsers.  We use a dynamically-scoped variable,
;; which makes it easier to funcall the parsers individually without
;; worrying about whether they take the parameter or not.
(js2-deflocal js2-in-for-init nil)
(js2-deflocal js2-temp-name-counter 0)
(js2-deflocal js2-parse-stmt-count 0)

(defsubst js2-get-next-temp-name ()
  (format "$%d" (incf js2-temp-name-counter)))

(defvar js2-parse-interruptable-p t
  "Set this to nil to force parse to continue until finished.
This will mostly be useful for interpreters.")

(defvar js2-statements-per-pause 50
  "Pause after this many statements to check for user input.
If user input is pending, stop the parse and discard the tree.
This makes for a smoother user experience for large files.
You may have to wait a second or two before the highlighting
and error-reporting appear, but you can always type ahead if
you wish.  This appears to be more or less how Eclipse, IntelliJ
and other editors work.")

(js2-deflocal js2-record-comments t
  "Instructs the scanner to record comments in `js2-scanned-comments'.")

(js2-deflocal js2-scanned-comments nil
  "List of all comments from the current parse.")

(defcustom js2-mode-indent-inhibit-undo nil
  "Non-nil to disable collection of Undo information when indenting lines.
Some users have requested this behavior.  It's nil by default because
other Emacs modes don't work this way."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-mode-indent-ignore-first-tab nil
  "If non-nil, ignore first TAB keypress if we look indented properly.
It's fairly common for users to navigate to an already-indented line
and press TAB for reassurance that it's been indented.  For this class
of users, we want the first TAB press on a line to be ignored if the
line is already indented to one of the precomputed alternatives.

This behavior is only partly implemented.  If you TAB-indent a line,
navigate to another line, and then navigate back, it fails to clear
the last-indented variable, so it thinks you've already hit TAB once,
and performs the indent.  A full solution would involve getting on the
point-motion hooks for the entire buffer.  If we come across another
use cases that requires watching point motion, I'll consider doing it.

If you set this variable to nil, then the TAB key will always change
the indentation of the current line, if more than one alternative
indentation spot exists."
  :type 'boolean
  :group 'js2-mode)

(defvar js2-indent-hook nil
  "A hook for user-defined indentation rules.

Functions on this hook should expect two arguments:    (LIST INDEX)
The LIST argument is the list of computed indentation points for
the current line.  INDEX is the list index of the indentation point
that `js2-bounce-indent' plans to use.  If INDEX is nil, then the
indent function is not going to change the current line indentation.

If a hook function on this list returns a non-nil value, then
`js2-bounce-indent' assumes the hook function has performed its own
indentation, and will do nothing.  If all hook functions on the list
return nil, then `js2-bounce-indent' will use its computed indentation
and reindent the line.

When hook functions on this hook list are called, the variable
`js2-mode-ast' may or may not be set, depending on whether the
parse tree is available.  If the variable is nil, you can pass a
callback to `js2-mode-wait-for-parse', and your callback will be
called after the new parse tree is built.  This can take some time
in large files.")

(defface js2-warning
  `((((class color) (background light))
     (:underline  "orange"))
    (((class color) (background dark))
     (:underline "orange"))
    (t (:underline t)))
  "Face for JavaScript warnings."
  :group 'js2-mode)

(defface js2-error
  `((((class color) (background light))
     (:foreground "red"))
    (((class color) (background dark))
     (:foreground "red"))
    (t (:foreground "red")))
  "Face for JavaScript errors."
  :group 'js2-mode)

(defface js2-jsdoc-tag
  '((t :foreground "SlateGray"))
  "Face used to highlight @whatever tags in jsdoc comments."
  :group 'js2-mode)

(defface js2-jsdoc-type
  '((t :foreground "SteelBlue"))
  "Face used to highlight {FooBar} types in jsdoc comments."
  :group 'js2-mode)

(defface js2-jsdoc-value
  '((t :foreground "PeachPuff3"))
  "Face used to highlight tag values in jsdoc comments."
  :group 'js2-mode)

(defface js2-function-param
  '((t :foreground "SeaGreen"))
  "Face used to highlight function parameters in javascript."
  :group 'js2-mode)

(defface js2-instance-member
  '((t :foreground "DarkOrchid"))
  "Face used to highlight instance variables in javascript.
Not currently used."
  :group 'js2-mode)

(defface js2-private-member
  '((t :foreground "PeachPuff3"))
  "Face used to highlight calls to private methods in javascript.
Not currently used."
  :group 'js2-mode)

(defface js2-private-function-call
  '((t :foreground "goldenrod"))
  "Face used to highlight calls to private functions in javascript.
Not currently used."
  :group 'js2-mode)

(defface js2-jsdoc-html-tag-name
  '((((class color) (min-colors 88) (background light))
     (:foreground "rosybrown"))
    (((class color) (min-colors 8) (background dark))
     (:foreground "yellow"))
    (((class color) (min-colors 8) (background light))
     (:foreground "magenta")))
    "Face used to highlight jsdoc html tag names"
  :group 'js2-mode)

(defface js2-jsdoc-html-tag-delimiter
  '((((class color) (min-colors 88) (background light))
     (:foreground "dark khaki"))
    (((class color) (min-colors 8) (background dark))
     (:foreground "green"))
    (((class color) (min-colors 8) (background light))
     (:foreground "green")))
  "Face used to highlight brackets in jsdoc html tags."
  :group 'js2-mode)

(defface js2-external-variable
  '((t :foreground "orange"))
  "Face used to highlight undeclared variable identifiers.")

(defcustom js2-init-hook nil
  "List of functions to be called after `js2-mode' or
`js2-minor-mode' has initialized all variables, before parsing
the buffer for the first time."
  :type 'hook
  :group 'js2-mode
  :version "20130608")

(defcustom js2-post-parse-callbacks nil
  "List of callback functions invoked after parsing finishes.
Currently, the main use for this function is to add synthetic
declarations to `js2-recorded-identifiers', which see."
  :type 'hook
  :group 'js2-mode)

(defcustom js2-highlight-external-variables t
  "Non-nil to highlight undeclared variable identifiers.
An undeclared variable is any variable not declared with var or let
in the current scope or any lexically enclosing scope.  If you use
such a variable, then you are either expecting it to originate from
another file, or you've got a potential bug."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-include-jslint-globals t
  "Non-nil to include the identifiers from JSLint global
declaration (see http://www.jslint.com/lint.html#global) in the
buffer-local externs list.  See `js2-additional-externs' for more
information."
  :type 'boolean
  :group 'js2-mode)

(defvar js2-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'js2-mode-show-node)
    (define-key map (kbd "M-j") #'js2-line-break)
    (define-key map (kbd "C-c C-e") #'js2-mode-hide-element)
    (define-key map (kbd "C-c C-s") #'js2-mode-show-element)
    (define-key map (kbd "C-c C-a") #'js2-mode-show-all)
    (define-key map (kbd "C-c C-f") #'js2-mode-toggle-hide-functions)
    (define-key map (kbd "C-c C-t") #'js2-mode-toggle-hide-comments)
    (define-key map (kbd "C-c C-o") #'js2-mode-toggle-element)
    (define-key map (kbd "C-c C-w") #'js2-mode-toggle-warnings-and-errors)
    (define-key map [down-mouse-3] #'js2-down-mouse-3)
    (when js2-bounce-indent-p
      (define-key map (kbd "<backtab>") #'js2-indent-bounce-backwards))

    (define-key map [menu-bar javascript]
      (cons "JavaScript" (make-sparse-keymap "JavaScript")))

    (define-key map [menu-bar javascript customize-js2-mode]
      '(menu-item "Customize js2-mode" js2-mode-customize
                  :help "Customize the behavior of this mode"))

    (define-key map [menu-bar javascript js2-force-refresh]
      '(menu-item "Force buffer refresh" js2-mode-reset
                  :help "Re-parse the buffer from scratch"))

    (define-key map [menu-bar javascript separator-2]
      '("--"))

    (define-key map [menu-bar javascript next-error]
      '(menu-item "Next warning or error" next-error
                  :enabled (and js2-mode-ast
                                (or (js2-ast-root-errors js2-mode-ast)
                                    (js2-ast-root-warnings js2-mode-ast)))
                  :help "Move to next warning or error"))

    (define-key map [menu-bar javascript display-errors]
      '(menu-item "Show errors and warnings" js2-mode-display-warnings-and-errors
                  :visible (not js2-mode-show-parse-errors)
                  :help "Turn on display of warnings and errors"))

    (define-key map [menu-bar javascript hide-errors]
      '(menu-item "Hide errors and warnings" js2-mode-hide-warnings-and-errors
                  :visible js2-mode-show-parse-errors
                  :help "Turn off display of warnings and errors"))

    (define-key map [menu-bar javascript separator-1]
      '("--"))

    (define-key map [menu-bar javascript js2-toggle-function]
      '(menu-item "Show/collapse element" js2-mode-toggle-element
                  :help "Hide or show function body or comment"))

    (define-key map [menu-bar javascript show-comments]
      '(menu-item "Show block comments" js2-mode-toggle-hide-comments
                  :visible js2-mode-comments-hidden
                  :help "Expand all hidden block comments"))

    (define-key map [menu-bar javascript hide-comments]
      '(menu-item "Hide block comments" js2-mode-toggle-hide-comments
                  :visible (not js2-mode-comments-hidden)
                  :help "Show block comments as /*...*/"))

    (define-key map [menu-bar javascript show-all-functions]
      '(menu-item "Show function bodies" js2-mode-toggle-hide-functions
                  :visible js2-mode-functions-hidden
                  :help "Expand all hidden function bodies"))

    (define-key map [menu-bar javascript hide-all-functions]
      '(menu-item "Hide function bodies" js2-mode-toggle-hide-functions
                  :visible (not js2-mode-functions-hidden)
                  :help "Show {...} for all top-level function bodies"))

    map)
  "Keymap used in `js2-mode' buffers.")

(defconst js2-mode-identifier-re "[[:alpha:]_$][[:alnum:]_$]*")

(defvar js2-mode-//-comment-re "^\\(\\s-*\\)//.+"
  "Matches a //-comment line.  Must be first non-whitespace on line.
First match-group is the leading whitespace.")

(defvar js2-mode-hook nil)

(js2-deflocal js2-mode-ast nil "Private variable.")
(js2-deflocal js2-mode-parse-timer nil "Private variable.")
(js2-deflocal js2-mode-buffer-dirty-p nil "Private variable.")
(js2-deflocal js2-mode-parsing nil "Private variable.")
(js2-deflocal js2-mode-node-overlay nil)

(defvar js2-mode-show-overlay js2-mode-dev-mode-p
  "Debug:  Non-nil to highlight AST nodes on mouse-down.")

(js2-deflocal js2-mode-fontifications nil "Private variable")
(js2-deflocal js2-mode-deferred-properties nil "Private variable")
(js2-deflocal js2-imenu-recorder nil "Private variable")
(js2-deflocal js2-imenu-function-map nil "Private variable")

(defvar js2-paragraph-start
  "\\(@[[:alpha:]]+\\>\\|$\\)")

;; Note that we also set a 'c-in-sws text property in html comments,
;; so that `c-forward-sws' and `c-backward-sws' work properly.
(defvar js2-syntactic-ws-start
  "\\s \\|/[*/]\\|[\n\r]\\|\\\\[\n\r]\\|\\s!\\|<!--\\|^\\s-*-->")

(defvar js2-syntactic-ws-end
  "\\s \\|[\n\r/]\\|\\s!")

(defvar js2-syntactic-eol
  (concat "\\s *\\(/\\*[^*\n\r]*"
          "\\(\\*+[^*\n\r/][^*\n\r]*\\)*"
          "\\*+/\\s *\\)*"
          "\\(//\\|/\\*[^*\n\r]*"
          "\\(\\*+[^*\n\r/][^*\n\r]*\\)*$"
          "\\|\\\\$\\|$\\)")
  "Copied from `java-mode'.  Needed for some cc-engine functions.")

(defvar js2-comment-prefix-regexp
  "//+\\|\\**")

(defvar js2-comment-start-skip
  "\\(//+\\|/\\*+\\)\\s *")

(defvar js2-mode-verbose-parse-p js2-mode-dev-mode-p
  "Non-nil to emit status messages during parsing.")

(defvar js2-mode-functions-hidden nil "Private variable.")
(defvar js2-mode-comments-hidden nil "Private variable.")

(defvar js2-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    table)
  "Syntax table used in `js2-mode' buffers.")

(defvar js2-mode-abbrev-table nil
  "Abbrev table in use in `js2-mode' buffers.")
(define-abbrev-table 'js2-mode-abbrev-table ())

(defvar js2-mode-pending-parse-callbacks nil
  "List of functions waiting to be notified that parse is finished.")

(defvar js2-mode-last-indented-line -1)

;;; Localizable error and warning messages

;; Messages are copied from Rhino's Messages.properties.
;; Many of the Java-specific messages have been elided.
;; Add any js2-specific ones at the end, so we can keep
;; this file synced with changes to Rhino's.

(defvar js2-message-table
  (make-hash-table :test 'equal :size 250)
  "Contains localized messages for `js2-mode'.")

;; TODO(stevey):  construct this table at compile-time.
(defmacro js2-msg (key &rest strings)
  `(puthash ,key (concat ,@strings)
            js2-message-table))

(defun js2-get-msg (msg-key)
  "Look up a localized message.
MSG-KEY is a list of (MSG ARGS).  If the message takes parameters,
the correct number of ARGS must be provided."
  (let* ((key (if (listp msg-key) (car msg-key) msg-key))
         (args (if (listp msg-key) (cdr msg-key)))
         (msg (gethash key js2-message-table)))
    (if msg
        (apply #'format msg args)
      key)))  ; default to showing the key

(js2-msg "msg.dup.parms"
         "Duplicate parameter name '%s'.")

(js2-msg "msg.too.big.jump"
         "Program too complex: jump offset too big.")

(js2-msg "msg.too.big.index"
         "Program too complex: internal index exceeds 64K limit.")

(js2-msg "msg.while.compiling.fn"
         "Encountered code generation error while compiling function '%s': %s")

(js2-msg "msg.while.compiling.script"
         "Encountered code generation error while compiling script: %s")

;; Context
(js2-msg "msg.ctor.not.found"
         "Constructor for '%s' not found.")

(js2-msg "msg.not.ctor"
         "'%s' is not a constructor.")

;; FunctionObject
(js2-msg "msg.varargs.ctor"
         "Method or constructor '%s' must be static "
         "with the signature (Context cx, Object[] args, "
         "Function ctorObj, boolean inNewExpr) "
         "to define a variable arguments constructor.")

(js2-msg "msg.varargs.fun"
         "Method '%s' must be static with the signature "
         "(Context cx, Scriptable thisObj, Object[] args, Function funObj) "
         "to define a variable arguments function.")

(js2-msg "msg.incompat.call"
         "Method '%s' called on incompatible object.")

(js2-msg "msg.bad.parms"
         "Unsupported parameter type '%s' in method '%s'.")

(js2-msg "msg.bad.method.return"
         "Unsupported return type '%s' in method '%s'.")

(js2-msg "msg.bad.ctor.return"
         "Construction of objects of type '%s' is not supported.")

(js2-msg "msg.no.overload"
         "Method '%s' occurs multiple times in class '%s'.")

(js2-msg "msg.method.not.found"
         "Method '%s' not found in '%s'.")

;; IRFactory

(js2-msg "msg.bad.for.in.lhs"
         "Invalid left-hand side of for..in loop.")

(js2-msg "msg.mult.index"
         "Only one variable allowed in for..in loop.")

(js2-msg "msg.bad.for.in.destruct"
         "Left hand side of for..in loop must be an array of "
         "length 2 to accept key/value pair.")

(js2-msg "msg.cant.convert"
         "Can't convert to type '%s'.")

(js2-msg "msg.bad.assign.left"
         "Invalid assignment left-hand side.")

(js2-msg "msg.bad.decr"
         "Invalid decerement operand.")

(js2-msg "msg.bad.incr"
         "Invalid increment operand.")

(js2-msg "msg.bad.yield"
         "yield must be in a function.")

(js2-msg "msg.yield.parenthesized"
         "yield expression must be parenthesized.")

;; NativeGlobal
(js2-msg "msg.cant.call.indirect"
          "Function '%s' must be called directly, and not by way of a "
          "function of another name.")

(js2-msg "msg.eval.nonstring"
          "Calling eval() with anything other than a primitive "
          "string value will simply return the value. "
          "Is this what you intended?")

(js2-msg "msg.eval.nonstring.strict"
         "Calling eval() with anything other than a primitive "
         "string value is not allowed in strict mode.")

(js2-msg "msg.bad.destruct.op"
         "Invalid destructuring assignment operator")

;; NativeCall
(js2-msg "msg.only.from.new"
         "'%s' may only be invoked from a `new' expression.")

(js2-msg "msg.deprec.ctor"
         "The '%s' constructor is deprecated.")

;; NativeFunction
(js2-msg "msg.no.function.ref.found"
         "no source found to decompile function reference %s")

(js2-msg "msg.arg.isnt.array"
         "second argument to Function.prototype.apply must be an array")

;; NativeGlobal
(js2-msg "msg.bad.esc.mask"
         "invalid string escape mask")

;; NativeRegExp
(js2-msg "msg.bad.quant"
  "Invalid quantifier %s")

(js2-msg "msg.overlarge.backref"
  "Overly large back reference %s")

(js2-msg "msg.overlarge.min"
  "Overly large minimum %s")

(js2-msg "msg.overlarge.max"
  "Overly large maximum %s")

(js2-msg "msg.zero.quant"
  "Zero quantifier %s")

(js2-msg "msg.max.lt.min"
  "Maximum %s less than minimum")

(js2-msg "msg.unterm.quant"
  "Unterminated quantifier %s")

(js2-msg "msg.unterm.paren"
  "Unterminated parenthetical %s")

(js2-msg "msg.unterm.class"
  "Unterminated character class %s")

(js2-msg "msg.bad.range"
  "Invalid range in character class.")

(js2-msg "msg.trail.backslash"
  "Trailing \\ in regular expression.")

(js2-msg "msg.re.unmatched.right.paren"
  "unmatched ) in regular expression.")

(js2-msg "msg.no.regexp"
  "Regular expressions are not available.")

(js2-msg "msg.bad.backref"
  "back-reference exceeds number of capturing parentheses.")

(js2-msg "msg.bad.regexp.compile"
         "Only one argument may be specified if the first "
         "argument to RegExp.prototype.compile is a RegExp object.")

;; Parser
(js2-msg "msg.got.syntax.errors"
         "Compilation produced %s syntax errors.")

(js2-msg "msg.var.redecl"
         "TypeError: redeclaration of var %s.")

(js2-msg "msg.const.redecl"
         "TypeError: redeclaration of const %s.")

(js2-msg "msg.let.redecl"
         "TypeError: redeclaration of variable %s.")

(js2-msg "msg.parm.redecl"
         "TypeError: redeclaration of formal parameter %s.")

(js2-msg "msg.fn.redecl"
         "TypeError: redeclaration of function %s.")

(js2-msg "msg.let.decl.not.in.block"
         "SyntaxError: let declaration not directly within block")

;; NodeTransformer
(js2-msg "msg.dup.label"
         "duplicated label")

(js2-msg "msg.undef.label"
         "undefined label")

(js2-msg "msg.bad.break"
         "unlabelled break must be inside loop or switch")

(js2-msg "msg.continue.outside"
         "continue must be inside loop")

(js2-msg "msg.continue.nonloop"
         "continue can only use labels of iteration statements")

(js2-msg "msg.bad.throw.eol"
         "Line terminator is not allowed between the throw "
         "keyword and throw expression.")

(js2-msg "msg.unnamed.function.stmt" ; added by js2-mode
         "function statement requires a name")

(js2-msg "msg.no.paren.parms"
         "missing ( before function parameters.")

(js2-msg "msg.no.parm"
         "missing formal parameter")

(js2-msg "msg.no.paren.after.parms"
         "missing ) after formal parameters")

(js2-msg "msg.no.default.after.default.param" ; added by js2-mode
         "parameter without default follows parameter with default")

(js2-msg "msg.param.after.rest" ; added by js2-mode
         "parameter after rest parameter")

(js2-msg "msg.bad.arrow.args" ; added by js2-mode
         "invalid arrow-function arguments (parentheses around the arrow-function may help)")

(js2-msg "msg.no.brace.body"
         "missing '{' before function body")

(js2-msg "msg.no.brace.after.body"
         "missing } after function body")

(js2-msg "msg.no.paren.cond"
         "missing ( before condition")

(js2-msg "msg.no.paren.after.cond"
         "missing ) after condition")

(js2-msg "msg.no.semi.stmt"
         "missing ; before statement")

(js2-msg "msg.missing.semi"
         "missing ; after statement")

(js2-msg "msg.no.name.after.dot"
         "missing name after . operator")

(js2-msg "msg.no.name.after.coloncolon"
         "missing name after :: operator")

(js2-msg "msg.no.name.after.dotdot"
         "missing name after .. operator")

(js2-msg "msg.no.name.after.xmlAttr"
         "missing name after .@")

(js2-msg "msg.no.bracket.index"
         "missing ] in index expression")

(js2-msg "msg.no.paren.switch"
         "missing ( before switch expression")

(js2-msg "msg.no.paren.after.switch"
         "missing ) after switch expression")

(js2-msg "msg.no.brace.switch"
         "missing '{' before switch body")

(js2-msg "msg.bad.switch"
         "invalid switch statement")

(js2-msg "msg.no.colon.case"
         "missing : after case expression")

(js2-msg "msg.double.switch.default"
         "double default label in the switch statement")

(js2-msg "msg.no.while.do"
         "missing while after do-loop body")

(js2-msg "msg.no.paren.for"
         "missing ( after for")

(js2-msg "msg.no.semi.for"
         "missing ; after for-loop initializer")

(js2-msg "msg.no.semi.for.cond"
         "missing ; after for-loop condition")

(js2-msg "msg.in.after.for.name"
         "missing in or of after for")

(js2-msg "msg.no.paren.for.ctrl"
         "missing ) after for-loop control")

(js2-msg "msg.no.paren.with"
         "missing ( before with-statement object")

(js2-msg "msg.no.paren.after.with"
         "missing ) after with-statement object")

(js2-msg "msg.no.paren.after.let"
         "missing ( after let")

(js2-msg "msg.no.paren.let"
         "missing ) after variable list")

(js2-msg "msg.no.curly.let"
         "missing } after let statement")

(js2-msg "msg.bad.return"
         "invalid return")

(js2-msg "msg.no.brace.block"
         "missing } in compound statement")

(js2-msg "msg.bad.label"
         "invalid label")

(js2-msg "msg.bad.var"
         "missing variable name")

(js2-msg "msg.bad.var.init"
         "invalid variable initialization")

(js2-msg "msg.no.colon.cond"
         "missing : in conditional expression")

(js2-msg "msg.no.paren.arg"
         "missing ) after argument list")

(js2-msg "msg.no.bracket.arg"
         "missing ] after element list")

(js2-msg "msg.bad.prop"
         "invalid property id")

(js2-msg "msg.no.colon.prop"
         "missing : after property id")

(js2-msg "msg.no.brace.prop"
         "missing } after property list")

(js2-msg "msg.no.paren"
         "missing ) in parenthetical")

(js2-msg "msg.reserved.id"
         "identifier is a reserved word")

(js2-msg "msg.no.paren.catch"
         "missing ( before catch-block condition")

(js2-msg "msg.bad.catchcond"
         "invalid catch block condition")

(js2-msg "msg.catch.unreachable"
         "any catch clauses following an unqualified catch are unreachable")

(js2-msg "msg.no.brace.try"
         "missing '{' before try block")

(js2-msg "msg.no.brace.catchblock"
         "missing '{' before catch-block body")

(js2-msg "msg.try.no.catchfinally"
         "'try' without 'catch' or 'finally'")

(js2-msg "msg.no.return.value"
         "function %s does not always return a value")

(js2-msg "msg.anon.no.return.value"
         "anonymous function does not always return a value")

(js2-msg "msg.return.inconsistent"
         "return statement is inconsistent with previous usage")

(js2-msg "msg.generator.returns"
         "TypeError: generator function '%s' returns a value")

(js2-msg "msg.anon.generator.returns"
         "TypeError: anonymous generator function returns a value")

(js2-msg "msg.syntax"
         "syntax error")

(js2-msg "msg.unexpected.eof"
         "Unexpected end of file")

(js2-msg "msg.XML.bad.form"
         "illegally formed XML syntax")

(js2-msg "msg.XML.not.available"
         "XML runtime not available")

(js2-msg "msg.too.deep.parser.recursion"
         "Too deep recursion while parsing")

(js2-msg "msg.no.side.effects"
         "Code has no side effects")

(js2-msg "msg.extra.trailing.comma"
         "Trailing comma is not supported in some browsers")

(js2-msg "msg.array.trailing.comma"
         "Trailing comma yields different behavior across browsers")

(js2-msg "msg.equal.as.assign"
         (concat "Test for equality (==) mistyped as assignment (=)?"
                 " (parenthesize to suppress warning)"))

(js2-msg "msg.var.hides.arg"
         "Variable %s hides argument")

(js2-msg "msg.destruct.assign.no.init"
         "Missing = in destructuring declaration")

;; ScriptRuntime
(js2-msg "msg.no.properties"
         "%s has no properties.")

(js2-msg "msg.invalid.iterator"
         "Invalid iterator value")

(js2-msg "msg.iterator.primitive"
         "__iterator__ returned a primitive value")

(js2-msg "msg.assn.create.strict"
         "Assignment to undeclared variable %s")

(js2-msg "msg.undeclared.variable"  ; added by js2-mode
         "Undeclared variable or function '%s'")

(js2-msg "msg.ref.undefined.prop"
         "Reference to undefined property '%s'")

(js2-msg "msg.prop.not.found"
         "Property %s not found.")

(js2-msg "msg.invalid.type"
         "Invalid JavaScript value of type %s")

(js2-msg "msg.primitive.expected"
         "Primitive type expected (had %s instead)")

(js2-msg "msg.namespace.expected"
         "Namespace object expected to left of :: (found %s instead)")

(js2-msg "msg.null.to.object"
         "Cannot convert null to an object.")

(js2-msg "msg.undef.to.object"
         "Cannot convert undefined to an object.")

(js2-msg "msg.cyclic.value"
         "Cyclic %s value not allowed.")

(js2-msg "msg.is.not.defined"
         "'%s' is not defined.")

(js2-msg "msg.undef.prop.read"
         "Cannot read property '%s' from %s")

(js2-msg "msg.undef.prop.write"
         "Cannot set property '%s' of %s to '%s'")

(js2-msg "msg.undef.prop.delete"
         "Cannot delete property '%s' of %s")

(js2-msg "msg.undef.method.call"
         "Cannot call method '%s' of %s")

(js2-msg "msg.undef.with"
         "Cannot apply 'with' to %s")

(js2-msg "msg.isnt.function"
         "%s is not a function, it is %s.")

(js2-msg "msg.isnt.function.in"
         "Cannot call property %s in object %s. "
         "It is not a function, it is '%s'.")

(js2-msg "msg.function.not.found"
         "Cannot find function %s.")

(js2-msg "msg.function.not.found.in"
         "Cannot find function %s in object %s.")

(js2-msg "msg.isnt.xml.object"
         "%s is not an xml object.")

(js2-msg "msg.no.ref.to.get"
         "%s is not a reference to read reference value.")

(js2-msg "msg.no.ref.to.set"
         "%s is not a reference to set reference value to %s.")

(js2-msg "msg.no.ref.from.function"
         "Function %s can not be used as the left-hand "
         "side of assignment or as an operand of ++ or -- operator.")

(js2-msg "msg.bad.default.value"
         "Object's getDefaultValue() method returned an object.")

(js2-msg "msg.instanceof.not.object"
         "Can't use instanceof on a non-object.")

(js2-msg "msg.instanceof.bad.prototype"
         "'prototype' property of %s is not an object.")

(js2-msg "msg.bad.radix"
         "illegal radix %s.")

;; ScriptableObject
(js2-msg "msg.default.value"
         "Cannot find default value for object.")

(js2-msg "msg.zero.arg.ctor"
         "Cannot load class '%s' which has no zero-parameter constructor.")

(js2-msg "msg.ctor.multiple.parms"
         "Can't define constructor or class %s since more than "
         "one constructor has multiple parameters.")

(js2-msg "msg.extend.scriptable"
         "%s must extend ScriptableObject in order to define property %s.")

(js2-msg "msg.bad.getter.parms"
         "In order to define a property, getter %s must have zero "
         "parameters or a single ScriptableObject parameter.")

(js2-msg "msg.obj.getter.parms"
         "Expected static or delegated getter %s to take "
         "a ScriptableObject parameter.")

(js2-msg "msg.getter.static"
         "Getter and setter must both be static or neither be static.")

(js2-msg "msg.setter.return"
         "Setter must have void return type: %s")

(js2-msg "msg.setter2.parms"
         "Two-parameter setter must take a ScriptableObject as "
         "its first parameter.")

(js2-msg "msg.setter1.parms"
         "Expected single parameter setter for %s")

(js2-msg "msg.setter2.expected"
         "Expected static or delegated setter %s to take two parameters.")

(js2-msg "msg.setter.parms"
         "Expected either one or two parameters for setter.")

(js2-msg "msg.setter.bad.type"
         "Unsupported parameter type '%s' in setter '%s'.")

(js2-msg "msg.add.sealed"
         "Cannot add a property to a sealed object: %s.")

(js2-msg "msg.remove.sealed"
         "Cannot remove a property from a sealed object: %s.")

(js2-msg "msg.modify.sealed"
         "Cannot modify a property of a sealed object: %s.")

(js2-msg "msg.modify.readonly"
         "Cannot modify readonly property: %s.")

;; TokenStream
(js2-msg "msg.missing.exponent"
         "missing exponent")

(js2-msg "msg.caught.nfe"
         "number format error")

(js2-msg "msg.unterminated.string.lit"
         "unterminated string literal")

(js2-msg "msg.unterminated.comment"
         "unterminated comment")

(js2-msg "msg.unterminated.re.lit"
         "unterminated regular expression literal")

(js2-msg "msg.invalid.re.flag"
         "invalid flag after regular expression")

(js2-msg "msg.no.re.input.for"
         "no input for %s")

(js2-msg "msg.illegal.character"
         "illegal character")

(js2-msg "msg.invalid.escape"
         "invalid Unicode escape sequence")

(js2-msg "msg.bad.namespace"
         "not a valid default namespace statement. "
         "Syntax is: default xml namespace = EXPRESSION;")

;; TokensStream warnings
(js2-msg "msg.bad.octal.literal"
         "illegal octal literal digit %s; "
         "interpreting it as a decimal digit")

(js2-msg "msg.reserved.keyword"
         "illegal usage of future reserved keyword %s; "
         "interpreting it as ordinary identifier")

(js2-msg "msg.script.is.not.constructor"
         "Script objects are not constructors.")

;; Arrays
(js2-msg "msg.arraylength.bad"
         "Inappropriate array length.")

;; Arrays
(js2-msg "msg.arraylength.too.big"
         "Array length %s exceeds supported capacity limit.")

;; URI
(js2-msg "msg.bad.uri"
         "Malformed URI sequence.")

;; Number
(js2-msg "msg.bad.precision"
         "Precision %s out of range.")

;; NativeGenerator
(js2-msg "msg.send.newborn"
         "Attempt to send value to newborn generator")

(js2-msg "msg.already.exec.gen"
         "Already executing generator")

(js2-msg "msg.StopIteration.invalid"
         "StopIteration may not be changed to an arbitrary object.")

;; Interpreter
(js2-msg "msg.yield.closing"
         "Yield from closing generator")

;;; Tokens Buffer

(defconst js2-ti-max-lookahead 2)
(defconst js2-ti-ntokens (1+ js2-ti-max-lookahead))

;; Have to call `js2-init-scanner' to initialize the values.
(js2-deflocal js2-ti-tokens nil)
(js2-deflocal js2-ti-tokens-cursor nil)
(js2-deflocal js2-ti-lookahead nil)

(defun js2-new-token (offset)
  (let ((token (make-js2-token (+ offset js2-ts-cursor))))
    (setq js2-ti-tokens-cursor (mod (1+ js2-ti-tokens-cursor) js2-ti-ntokens))
    (aset js2-ti-tokens js2-ti-tokens-cursor token)
    token))

(defsubst js2-current-token ()
  (aref js2-ti-tokens js2-ti-tokens-cursor))

(defsubst js2-current-token-string ()
  (js2-token-string (js2-current-token)))

(defsubst js2-current-token-type ()
  (js2-token-type (js2-current-token)))

(defsubst js2-current-token-beg ()
  (js2-token-beg (js2-current-token)))

(defsubst js2-current-token-end ()
  (js2-token-end (js2-current-token)))

(defun js2-current-token-len ()
  (let ((token (js2-current-token)))
    (- (js2-token-end token)
       (js2-token-beg token))))

(defun js2-ts-seek (state)
  (setq js2-ts-lineno (js2-ts-state-lineno state)
        js2-ts-cursor (js2-ts-state-cursor state)
        js2-ti-tokens (js2-ts-state-tokens state)
        js2-ti-tokens-cursor (js2-ts-state-tokens-cursor state)
        js2-ti-lookahead (js2-ts-state-lookahead state)))

;;; Utilities

(defun js2-delete-if (predicate list)
  "Remove all items satisfying PREDICATE in LIST."
  (loop for item in list
        if (not (funcall predicate item))
        collect item))

(defun js2-position (element list)
  "Find 0-indexed position of ELEMENT in LIST comparing with `eq'.
Returns nil if element is not found in the list."
  (let ((count 0)
        found)
    (while (and list (not found))
      (if (eq element (car list))
          (setq found t)
        (setq count (1+ count)
              list (cdr list))))
    (if found count)))

(defun js2-find-if (predicate list)
  "Find first item satisfying PREDICATE in LIST."
  (let (result)
    (while (and list (not result))
      (if (funcall predicate (car list))
          (setq result (car list)))
      (setq list (cdr list)))
    result))

(defmacro js2-time (form)
  "Evaluate FORM, discard result, and return elapsed time in sec."
  (declare (debug t))
  (let ((beg (make-symbol "--js2-time-beg--"))
        (delta (make-symbol "--js2-time-end--")))
    `(let ((,beg (current-time))
           ,delta)
       ,form
       (/ (truncate (* (- (float-time (current-time))
                          (float-time ,beg))
                       10000))
          10000.0))))

(defsubst js2-same-line (pos)
  "Return t if POS is on the same line as current point."
  (and (>= pos (point-at-bol))
       (<= pos (point-at-eol))))

(defun js2-code-bug ()
  "Signal an error when we encounter an unexpected code path."
  (error "failed assertion"))

(defsubst js2-record-text-property (beg end prop value)
  "Record a text property to set when parsing finishes."
  (push (list beg end prop value) js2-mode-deferred-properties))

;; I'd like to associate errors with nodes, but for now the
;; easiest thing to do is get the context info from the last token.
(defun js2-record-parse-error (msg &optional arg pos len)
  (push (list (list msg arg)
              (or pos (js2-current-token-beg))
              (or len (js2-current-token-len)))
        js2-parsed-errors))

(defun js2-report-error (msg &optional msg-arg pos len)
  "Signal a syntax error or record a parse error."
  (if js2-recover-from-parse-errors
      (js2-record-parse-error msg msg-arg pos len)
  (signal 'js2-syntax-error
          (list msg
                js2-ts-lineno
                (save-excursion
                  (goto-char js2-ts-cursor)
                  (current-column))
                js2-ts-hit-eof))))

(defun js2-report-warning (msg &optional msg-arg pos len face)
  (if js2-compiler-report-warning-as-error
      (js2-report-error msg msg-arg pos len)
    (push (list (list msg msg-arg)
                (or pos (js2-current-token-beg))
                (or len (js2-current-token-len))
                face)
          js2-parsed-warnings)))

(defun js2-add-strict-warning (msg-id &optional msg-arg beg end)
  (if js2-compiler-strict-mode
      (js2-report-warning msg-id msg-arg beg
                          (and beg end (- end beg)))))

(put 'js2-syntax-error 'error-conditions
     '(error syntax-error js2-syntax-error))
(put 'js2-syntax-error 'error-message "Syntax error")

(put 'js2-parse-error 'error-conditions
     '(error parse-error js2-parse-error))
(put 'js2-parse-error 'error-message "Parse error")

(defmacro js2-clear-flag (flags flag)
  `(setq ,flags (logand ,flags (lognot ,flag))))

(defmacro js2-set-flag (flags flag)
  "Logical-or FLAG into FLAGS."
  `(setq ,flags (logior ,flags ,flag)))

(defsubst js2-flag-set-p (flags flag)
  (/= 0 (logand flags flag)))

(defsubst js2-flag-not-set-p (flags flag)
  (zerop (logand flags flag)))

(defmacro js2-with-underscore-as-word-syntax (&rest body)
  "Evaluate BODY with the _ character set to be word-syntax."
  (declare (indent 0) (debug t))
  (let ((old-syntax (make-symbol "old-syntax")))
  `(let ((,old-syntax (string (char-syntax ?_))))
     (unwind-protect
         (progn
           (modify-syntax-entry ?_ "w" js2-mode-syntax-table)
           ,@body)
       (modify-syntax-entry ?_ ,old-syntax js2-mode-syntax-table)))))

(defsubst js2-char-uppercase-p (c)
  "Return t if C is an uppercase character.
Handles unicode and latin chars properly."
  (/= c (downcase c)))

(defsubst js2-char-lowercase-p (c)
  "Return t if C is an uppercase character.
Handles unicode and latin chars properly."
  (/= c (upcase c)))

;;; AST struct and function definitions

;; flags for ast node property 'member-type (used for e4x operators)
(defvar js2-property-flag    #x1 "Property access: element is valid name.")
(defvar js2-attribute-flag   #x2 "x.@y or x..@y.")
(defvar js2-descendants-flag #x4 "x..y or x..@i.")

(defsubst js2-relpos (pos anchor)
  "Convert POS to be relative to ANCHOR.
If POS is nil, returns nil."
  (and pos (- pos anchor)))

(defun js2-make-pad (indent)
  (if (zerop indent)
      ""
    (make-string (* indent js2-basic-offset) ? )))

(defun js2-visit-ast (node callback)
  "Visit every node in ast NODE with visitor CALLBACK.

CALLBACK is a function that takes two arguments:  (NODE END-P).  It is
called twice:  once to visit the node, and again after all the node's
children have been processed.  The END-P argument is nil on the first
call and non-nil on the second call.  The return value of the callback
affects the traversal:  if non-nil, the children of NODE are processed.
If the callback returns nil, or if the node has no children, then the
callback is called immediately with a non-nil END-P argument.

The node traversal is approximately lexical-order, although there
are currently no guarantees around this."
  (when node
    (let ((vfunc (get (aref node 0) 'js2-visitor)))
      ;; visit the node
      (when  (funcall callback node nil)
        ;; visit the kids
        (cond
         ((eq vfunc 'js2-visit-none)
          nil)                            ; don't even bother calling it
         ;; Each AST node type has to define a `js2-visitor' function
         ;; that takes a node and a callback, and calls `js2-visit-ast'
         ;; on each child of the node.
         (vfunc
          (funcall vfunc node callback))
         (t
          (error "%s does not define a visitor-traversal function"
                 (aref node 0)))))
      ;; call the end-visit
      (funcall callback node t))))

(defstruct (js2-node
            (:constructor nil))  ; abstract
  "Base AST node type."
  (type -1)  ; token type
  (pos -1)   ; start position of this AST node in parsed input
  (len 1)    ; num characters spanned by the node
  props      ; optional node property list (an alist)
  parent)    ; link to parent node; null for root

(defsubst js2-node-get-prop (node prop &optional default)
  (or (cadr (assoc prop (js2-node-props node))) default))

(defsubst js2-node-set-prop (node prop value)
  (setf (js2-node-props node)
        (cons (list prop value) (js2-node-props node))))

(defun js2-fixup-starts (n nodes)
  "Adjust the start positions of NODES to be relative to N.
Any node in the list may be nil, for convenience."
  (dolist (node nodes)
    (when node
      (setf (js2-node-pos node) (- (js2-node-pos node)
                                   (js2-node-pos n))))))

(defun js2-node-add-children (parent &rest nodes)
  "Set parent node of NODES to PARENT, and return PARENT.
Does nothing if we're not recording parent links.
If any given node in NODES is nil, doesn't record that link."
  (js2-fixup-starts parent nodes)
  (dolist (node nodes)
    (and node
         (setf (js2-node-parent node) parent))))

;; Non-recursive since it's called a frightening number of times.
(defun js2-node-abs-pos (n)
  (let ((pos (js2-node-pos n)))
    (while (setq n (js2-node-parent n))
      (setq pos (+ pos (js2-node-pos n))))
    pos))

(defsubst js2-node-abs-end (n)
  "Return absolute buffer position of end of N."
  (+ (js2-node-abs-pos n) (js2-node-len n)))

;; It's important to make sure block nodes have a Lisp list for the
;; child nodes, to limit printing recursion depth in an AST that
;; otherwise consists of defstruct vectors.  Emacs will crash printing
;; a sufficiently large vector tree.

(defstruct (js2-block-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-block-node (&key (type js2-BLOCK)
                                                    (pos (js2-current-token-beg))
                                                    len
                                                    props
                                                    kids)))
  "A block of statements."
  kids)  ; a Lisp list of the child statement nodes

(put 'cl-struct-js2-block-node 'js2-visitor 'js2-visit-block)
(put 'cl-struct-js2-block-node 'js2-printer 'js2-print-block)

(defun js2-visit-block (ast callback)
  "Visit the `js2-block-node' children of AST."
  (dolist (kid (js2-block-node-kids ast))
    (js2-visit-ast kid callback)))

(defun js2-print-block (n i)
  (let ((pad (js2-make-pad i)))
    (insert pad "{\n")
    (dolist (kid (js2-block-node-kids n))
      (js2-print-ast kid (1+ i)))
    (insert pad "}")))

(defstruct (js2-scope
            (:include js2-block-node)
            (:constructor nil)
            (:constructor make-js2-scope (&key (type js2-BLOCK)
                                               (pos (js2-current-token-beg))
                                               len
                                               kids)))
  ;; The symbol-table is a LinkedHashMap<String,Symbol> in Rhino.
  ;; I don't have one of those handy, so I'll use an alist for now.
  ;; It's as fast as an emacs hashtable for up to about 50 elements,
  ;; and is much lighter-weight to construct (both CPU and mem).
  ;; The keys are interned strings (symbols) for faster lookup.
  ;; Should switch to hybrid alist/hashtable eventually.
  symbol-table  ; an alist of (symbol . js2-symbol)
  parent-scope  ; a `js2-scope'
  top)          ; top-level `js2-scope' (script/function)

(put 'cl-struct-js2-scope 'js2-visitor 'js2-visit-block)
(put 'cl-struct-js2-scope 'js2-printer 'js2-print-none)

(defun js2-node-get-enclosing-scope (node)
  "Return the innermost `js2-scope' node surrounding NODE.
Returns nil if there is no enclosing scope node."
  (let ((parent (js2-node-parent node)))
    (while (not (js2-scope-p parent))
      (setq parent (js2-node-parent parent)))
    parent))

(defun js2-get-defining-scope (scope name)
  "Search up scope chain from SCOPE looking for NAME, a string or symbol.
Returns `js2-scope' in which NAME is defined, or nil if not found."
  (let ((sym (if (symbolp name)
                 name
               (intern name)))
        table
        result
        (continue t))
    (while (and scope continue)
      (if (and (setq table (js2-scope-symbol-table scope))
               (assq sym table))
          (setq continue nil
                result scope)
        (setq scope (js2-scope-parent-scope scope))))
    result))

(defun js2-scope-get-symbol (scope name)
  "Return symbol table entry for NAME in SCOPE.
NAME can be a string or symbol.   Returns a `js2-symbol' or nil if not found."
  (and (js2-scope-symbol-table scope)
       (cdr (assq (if (symbolp name)
                      name
                    (intern name))
                  (js2-scope-symbol-table scope)))))

(defun js2-scope-put-symbol (scope name symbol)
  "Enter SYMBOL into symbol-table for SCOPE under NAME.
NAME can be a Lisp symbol or string.  SYMBOL is a `js2-symbol'."
  (let* ((table (js2-scope-symbol-table scope))
         (sym (if (symbolp name) name (intern name)))
         (entry (assq sym table)))
    (if entry
        (setcdr entry symbol)
      (push (cons sym symbol)
            (js2-scope-symbol-table scope)))))

(defstruct (js2-symbol
            (:constructor nil)
            (:constructor make-js2-symbol (decl-type name &optional ast-node)))
  "A symbol table entry."
  ;; One of js2-FUNCTION, js2-LP (for parameters), js2-VAR,
  ;; js2-LET, or js2-CONST
  decl-type
  name  ; string
  ast-node) ; a `js2-node'

(defstruct (js2-error-node
            (:include js2-node)
            (:constructor nil) ; silence emacs21 byte-compiler
            (:constructor make-js2-error-node (&key (type js2-ERROR)
                                                    (pos (js2-current-token-beg))
                                                    len)))
  "AST node representing a parse error.")

(put 'cl-struct-js2-error-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-error-node 'js2-printer 'js2-print-none)

(defstruct (js2-script-node
            (:include js2-scope)
            (:constructor nil)
            (:constructor make-js2-script-node (&key (type js2-SCRIPT)
                                                     (pos (js2-current-token-beg))
                                                     len
                                                     ;; FIXME: What are those?
                                                     var-decls
                                                     fun-decls)))
  functions   ; Lisp list of nested functions
  regexps     ; Lisp list of (string . flags)
  symbols     ; alist (every symbol gets unique index)
  (param-count 0)
  var-names   ; vector of string names
  consts      ; bool-vector matching var-decls
  (temp-number 0))  ; for generating temp variables

(put 'cl-struct-js2-script-node 'js2-visitor 'js2-visit-block)
(put 'cl-struct-js2-script-node 'js2-printer 'js2-print-script)

(defun js2-print-script (node indent)
  (dolist (kid (js2-block-node-kids node))
    (js2-print-ast kid indent)))

(defstruct (js2-ast-root
            (:include js2-script-node)
            (:constructor nil)
            (:constructor make-js2-ast-root (&key (type js2-SCRIPT)
                                                  (pos (js2-current-token-beg))
                                                  len
                                                  buffer)))
  "The root node of a js2 AST."
  buffer         ; the source buffer from which the code was parsed
  comments       ; a Lisp list of comments, ordered by start position
  errors         ; a Lisp list of errors found during parsing
  warnings       ; a Lisp list of warnings found during parsing
  node-count)    ; number of nodes in the tree, including the root

(put 'cl-struct-js2-ast-root 'js2-visitor 'js2-visit-ast-root)
(put 'cl-struct-js2-ast-root 'js2-printer 'js2-print-script)

(defun js2-visit-ast-root (ast callback)
  (dolist (kid (js2-ast-root-kids ast))
    (js2-visit-ast kid callback))
  (dolist (comment (js2-ast-root-comments ast))
    (js2-visit-ast comment callback)))

(defstruct (js2-comment-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-comment-node (&key (type js2-COMMENT)
                                                      (pos (js2-current-token-beg))
                                                      len
                                                      format)))
  format)  ; 'line, 'block, 'jsdoc or 'html

(put 'cl-struct-js2-comment-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-comment-node 'js2-printer 'js2-print-comment)

(defun js2-print-comment (n i)
  ;; We really ought to link end-of-line comments to their nodes.
  ;; Or maybe we could add a new comment type, 'endline.
  (insert (js2-make-pad i)
          (js2-node-string n)))

(defstruct (js2-expr-stmt-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-expr-stmt-node (&key (type js2-EXPR_VOID)
                                                        (pos js2-ts-cursor)
                                                        len
                                                        expr)))
  "An expression statement."
  expr)

(defsubst js2-expr-stmt-node-set-has-result (node)
  "Change NODE type to `js2-EXPR_RESULT'.  Used for code generation."
  (setf (js2-node-type node) js2-EXPR_RESULT))

(put 'cl-struct-js2-expr-stmt-node 'js2-visitor 'js2-visit-expr-stmt-node)
(put 'cl-struct-js2-expr-stmt-node 'js2-printer 'js2-print-expr-stmt-node)

(defun js2-visit-expr-stmt-node (n v)
  (js2-visit-ast (js2-expr-stmt-node-expr n) v))

(defun js2-print-expr-stmt-node (n indent)
  (js2-print-ast (js2-expr-stmt-node-expr n) indent)
  (insert ";\n"))

(defstruct (js2-loop-node
            (:include js2-scope)
            (:constructor nil))
  "Abstract supertype of loop nodes."
  body      ; a `js2-block-node'
  lp        ; position of left-paren, nil if omitted
  rp)       ; position of right-paren, nil if omitted

(defstruct (js2-do-node
            (:include js2-loop-node)
            (:constructor nil)
            (:constructor make-js2-do-node (&key (type js2-DO)
                                                 (pos (js2-current-token-beg))
                                                 len
                                                 body
                                                 condition
                                                 while-pos
                                                 lp
                                                 rp)))
  "AST node for do-loop."
  condition  ; while (expression)
  while-pos) ; buffer position of 'while' keyword

(put 'cl-struct-js2-do-node 'js2-visitor 'js2-visit-do-node)
(put 'cl-struct-js2-do-node 'js2-printer 'js2-print-do-node)

(defun js2-visit-do-node (n v)
  (js2-visit-ast (js2-do-node-body n) v)
  (js2-visit-ast (js2-do-node-condition n) v))

(defun js2-print-do-node (n i)
  (let ((pad (js2-make-pad i)))
    (insert pad "do {\n")
    (dolist (kid (js2-block-node-kids (js2-do-node-body n)))
      (js2-print-ast kid (1+ i)))
    (insert pad "} while (")
    (js2-print-ast (js2-do-node-condition n) 0)
    (insert ");\n")))

(defstruct (js2-while-node
            (:include js2-loop-node)
            (:constructor nil)
            (:constructor make-js2-while-node (&key (type js2-WHILE)
                                                    (pos (js2-current-token-beg))
                                                    len body
                                                    condition lp
                                                    rp)))
  "AST node for while-loop."
  condition)    ; while-condition

(put 'cl-struct-js2-while-node 'js2-visitor 'js2-visit-while-node)
(put 'cl-struct-js2-while-node 'js2-printer 'js2-print-while-node)

(defun js2-visit-while-node (n v)
  (js2-visit-ast (js2-while-node-condition n) v)
  (js2-visit-ast (js2-while-node-body n) v))

(defun js2-print-while-node (n i)
  (let ((pad (js2-make-pad i)))
    (insert pad "while (")
    (js2-print-ast (js2-while-node-condition n) 0)
    (insert ") {\n")
    (js2-print-body (js2-while-node-body n) (1+ i))
    (insert pad "}\n")))

(defstruct (js2-for-node
            (:include js2-loop-node)
            (:constructor nil)
            (:constructor make-js2-for-node (&key (type js2-FOR)
                                                  (pos js2-ts-cursor)
                                                  len body init
                                                  condition
                                                  update lp rp)))
  "AST node for a C-style for-loop."
  init       ; initialization expression
  condition  ; loop condition
  update)    ; update clause

(put 'cl-struct-js2-for-node 'js2-visitor 'js2-visit-for-node)
(put 'cl-struct-js2-for-node 'js2-printer 'js2-print-for-node)

(defun js2-visit-for-node (n v)
  (js2-visit-ast (js2-for-node-init n) v)
  (js2-visit-ast (js2-for-node-condition n) v)
  (js2-visit-ast (js2-for-node-update n) v)
  (js2-visit-ast (js2-for-node-body n) v))

(defun js2-print-for-node (n i)
  (let ((pad (js2-make-pad i)))
    (insert pad "for (")
    (js2-print-ast (js2-for-node-init n) 0)
    (insert "; ")
    (js2-print-ast (js2-for-node-condition n) 0)
    (insert "; ")
    (js2-print-ast (js2-for-node-update n) 0)
    (insert ") {\n")
    (js2-print-body (js2-for-node-body n) (1+ i))
    (insert pad "}\n")))

(defstruct (js2-for-in-node
            (:include js2-loop-node)
            (:constructor nil)
            (:constructor make-js2-for-in-node (&key (type js2-FOR)
                                                     (pos js2-ts-cursor)
                                                     len body
                                                     iterator
                                                     object
                                                     in-pos
                                                     each-pos
                                                     foreach-p forof-p
                                                     lp rp)))
  "AST node for a for..in loop."
  iterator  ; [var] foo in ...
  object    ; object over which we're iterating
  in-pos    ; buffer position of 'in' keyword
  each-pos  ; buffer position of 'each' keyword, if foreach-p
  foreach-p ; t if it's a for-each loop
  forof-p)  ; t if it's a for-of loop

(put 'cl-struct-js2-for-in-node 'js2-visitor 'js2-visit-for-in-node)
(put 'cl-struct-js2-for-in-node 'js2-printer 'js2-print-for-in-node)

(defun js2-visit-for-in-node (n v)
  (js2-visit-ast (js2-for-in-node-iterator n) v)
  (js2-visit-ast (js2-for-in-node-object n) v)
  (js2-visit-ast (js2-for-in-node-body n) v))

(defun js2-print-for-in-node (n i)
  (let ((pad (js2-make-pad i))
        (foreach (js2-for-in-node-foreach-p n))
        (forof (js2-for-in-node-forof-p n)))
    (insert pad "for ")
    (if foreach
        (insert "each "))
    (insert "(")
    (js2-print-ast (js2-for-in-node-iterator n) 0)
    (insert (if forof " of " " in "))
    (js2-print-ast (js2-for-in-node-object n) 0)
    (insert ") {\n")
    (js2-print-body (js2-for-in-node-body n) (1+ i))
    (insert pad "}\n")))

(defstruct (js2-return-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-return-node (&key (type js2-RETURN)
                                                     (pos js2-ts-cursor)
                                                     len
                                                     retval)))
  "AST node for a return statement."
  retval)  ; expression to return, or 'undefined

(put 'cl-struct-js2-return-node 'js2-visitor 'js2-visit-return-node)
(put 'cl-struct-js2-return-node 'js2-printer 'js2-print-return-node)

(defun js2-visit-return-node (n v)
  (js2-visit-ast (js2-return-node-retval n) v))

(defun js2-print-return-node (n i)
  (insert (js2-make-pad i) "return")
  (when (js2-return-node-retval n)
    (insert " ")
    (js2-print-ast (js2-return-node-retval n) 0))
  (insert ";\n"))

(defstruct (js2-if-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-if-node (&key (type js2-IF)
                                                 (pos js2-ts-cursor)
                                                 len condition
                                                 then-part
                                                 else-pos
                                                 else-part lp
                                                 rp)))
  "AST node for an if-statement."
  condition   ; expression
  then-part   ; statement or block
  else-pos    ; optional buffer position of 'else' keyword
  else-part   ; optional statement or block
  lp          ; position of left-paren, nil if omitted
  rp)         ; position of right-paren, nil if omitted

(put 'cl-struct-js2-if-node 'js2-visitor 'js2-visit-if-node)
(put 'cl-struct-js2-if-node 'js2-printer 'js2-print-if-node)

(defun js2-visit-if-node (n v)
  (js2-visit-ast (js2-if-node-condition n) v)
  (js2-visit-ast (js2-if-node-then-part n) v)
  (js2-visit-ast (js2-if-node-else-part n) v))

(defun js2-print-if-node (n i)
  (let ((pad (js2-make-pad i))
        (then-part (js2-if-node-then-part n))
        (else-part (js2-if-node-else-part n)))
    (insert pad "if (")
    (js2-print-ast (js2-if-node-condition n) 0)
    (insert ") {\n")
    (js2-print-body then-part (1+ i))
    (insert pad "}")
    (cond
     ((not else-part)
      (insert "\n"))
     ((js2-if-node-p else-part)
      (insert " else ")
      (js2-print-body else-part i))
     (t
      (insert " else {\n")
      (js2-print-body else-part (1+ i))
      (insert pad "}\n")))))

(defstruct (js2-try-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-try-node (&key (type js2-TRY)
                                                  (pos js2-ts-cursor)
                                                  len
                                                  try-block
                                                  catch-clauses
                                                  finally-block)))
  "AST node for a try-statement."
  try-block
  catch-clauses  ; a Lisp list of `js2-catch-node'
  finally-block) ; a `js2-finally-node'

(put 'cl-struct-js2-try-node 'js2-visitor 'js2-visit-try-node)
(put 'cl-struct-js2-try-node 'js2-printer 'js2-print-try-node)

(defun js2-visit-try-node (n v)
  (js2-visit-ast (js2-try-node-try-block n) v)
  (dolist (clause (js2-try-node-catch-clauses n))
    (js2-visit-ast clause v))
  (js2-visit-ast (js2-try-node-finally-block n) v))

(defun js2-print-try-node (n i)
  (let ((pad (js2-make-pad i))
        (catches (js2-try-node-catch-clauses n))
        (finally (js2-try-node-finally-block n)))
    (insert pad "try {\n")
    (js2-print-body (js2-try-node-try-block n) (1+ i))
    (insert pad "}")
    (when catches
      (dolist (catch catches)
        (js2-print-ast catch i)))
    (if finally
        (js2-print-ast finally i)
      (insert "\n"))))

(defstruct (js2-catch-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-catch-node (&key (type js2-CATCH)
                                                    (pos js2-ts-cursor)
                                                    len
                                                    param
                                                    guard-kwd
                                                    guard-expr
                                                    block lp
                                                    rp)))
  "AST node for a catch clause."
  param       ; destructuring form or simple name node
  guard-kwd   ; relative buffer position of "if" in "catch (x if ...)"
  guard-expr  ; catch condition, a `js2-node'
  block       ; statements, a `js2-block-node'
  lp          ; buffer position of left-paren, nil if omitted
  rp)         ; buffer position of right-paren, nil if omitted

(put 'cl-struct-js2-catch-node 'js2-visitor 'js2-visit-catch-node)
(put 'cl-struct-js2-catch-node 'js2-printer 'js2-print-catch-node)

(defun js2-visit-catch-node (n v)
  (js2-visit-ast (js2-catch-node-param n) v)
  (when (js2-catch-node-guard-kwd n)
    (js2-visit-ast (js2-catch-node-guard-expr n) v))
  (js2-visit-ast (js2-catch-node-block n) v))

(defun js2-print-catch-node (n i)
  (let ((pad (js2-make-pad i))
        (guard-kwd (js2-catch-node-guard-kwd n))
        (guard-expr (js2-catch-node-guard-expr n)))
    (insert " catch (")
    (js2-print-ast (js2-catch-node-param n) 0)
    (when guard-kwd
      (insert " if ")
      (js2-print-ast guard-expr 0))
    (insert ") {\n")
    (js2-print-body (js2-catch-node-block n) (1+ i))
    (insert pad "}")))

(defstruct (js2-finally-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-finally-node (&key (type js2-FINALLY)
                                                      (pos js2-ts-cursor)
                                                      len body)))
  "AST node for a finally clause."
  body)  ; a `js2-node', often but not always a block node

(put 'cl-struct-js2-finally-node 'js2-visitor 'js2-visit-finally-node)
(put 'cl-struct-js2-finally-node 'js2-printer 'js2-print-finally-node)

(defun js2-visit-finally-node (n v)
  (js2-visit-ast (js2-finally-node-body n) v))

(defun js2-print-finally-node (n i)
  (let ((pad (js2-make-pad i)))
    (insert " finally {\n")
    (js2-print-body (js2-finally-node-body n) (1+ i))
    (insert pad "}\n")))

(defstruct (js2-switch-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-switch-node (&key (type js2-SWITCH)
                                                     (pos js2-ts-cursor)
                                                     len
                                                     discriminant
                                                     cases lp
                                                     rp)))
  "AST node for a switch statement."
  discriminant  ; a `js2-node' (switch expression)
  cases  ; a Lisp list of `js2-case-node'
  lp     ; position of open-paren for discriminant, nil if omitted
  rp)    ; position of close-paren for discriminant, nil if omitted

(put 'cl-struct-js2-switch-node 'js2-visitor 'js2-visit-switch-node)
(put 'cl-struct-js2-switch-node 'js2-printer 'js2-print-switch-node)

(defun js2-visit-switch-node (n v)
  (js2-visit-ast (js2-switch-node-discriminant n) v)
  (dolist (c (js2-switch-node-cases n))
    (js2-visit-ast c v)))

(defun js2-print-switch-node (n i)
  (let ((pad (js2-make-pad i))
        (cases (js2-switch-node-cases n)))
    (insert pad "switch (")
    (js2-print-ast (js2-switch-node-discriminant n) 0)
    (insert ") {\n")
    (dolist (case cases)
      (js2-print-ast case i))
    (insert pad "}\n")))

(defstruct (js2-case-node
            (:include js2-block-node)
            (:constructor nil)
            (:constructor make-js2-case-node (&key (type js2-CASE)
                                                   (pos js2-ts-cursor)
                                                   len kids expr)))
  "AST node for a case clause of a switch statement."
  expr)   ; the case expression (nil for default)

(put 'cl-struct-js2-case-node 'js2-visitor 'js2-visit-case-node)
(put 'cl-struct-js2-case-node 'js2-printer 'js2-print-case-node)

(defun js2-visit-case-node (n v)
  (js2-visit-ast (js2-case-node-expr n) v)
  (js2-visit-block n v))

(defun js2-print-case-node (n i)
  (let ((pad (js2-make-pad i))
        (expr (js2-case-node-expr n)))
    (insert pad)
    (if (null expr)
        (insert "default:\n")
      (insert "case ")
      (js2-print-ast expr 0)
      (insert ":\n"))
    (dolist (kid (js2-case-node-kids n))
      (js2-print-ast kid (1+ i)))))

(defstruct (js2-throw-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-throw-node (&key (type js2-THROW)
                                                    (pos js2-ts-cursor)
                                                    len expr)))
  "AST node for a throw statement."
  expr)   ; the expression to throw

(put 'cl-struct-js2-throw-node 'js2-visitor 'js2-visit-throw-node)
(put 'cl-struct-js2-throw-node 'js2-printer 'js2-print-throw-node)

(defun js2-visit-throw-node (n v)
  (js2-visit-ast (js2-throw-node-expr n) v))

(defun js2-print-throw-node (n i)
  (insert (js2-make-pad i) "throw ")
  (js2-print-ast (js2-throw-node-expr n) 0)
  (insert ";\n"))

(defstruct (js2-with-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-with-node (&key (type js2-WITH)
                                                   (pos js2-ts-cursor)
                                                   len object
                                                   body lp rp)))
  "AST node for a with-statement."
  object
  body
  lp    ; buffer position of left-paren around object, nil if omitted
  rp)   ; buffer position of right-paren around object, nil if omitted

(put 'cl-struct-js2-with-node 'js2-visitor 'js2-visit-with-node)
(put 'cl-struct-js2-with-node 'js2-printer 'js2-print-with-node)

(defun js2-visit-with-node (n v)
  (js2-visit-ast (js2-with-node-object n) v)
  (js2-visit-ast (js2-with-node-body n) v))

(defun js2-print-with-node (n i)
  (let ((pad (js2-make-pad i)))
    (insert pad "with (")
    (js2-print-ast (js2-with-node-object n) 0)
    (insert ") {\n")
    (js2-print-body (js2-with-node-body n) (1+ i))
    (insert pad "}\n")))

(defstruct (js2-label-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-label-node (&key (type js2-LABEL)
                                                    (pos js2-ts-cursor)
                                                    len name)))
  "AST node for a statement label or case label."
  name   ; a string
  loop)  ; for validating and code-generating continue-to-label

(put 'cl-struct-js2-label-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-label-node 'js2-printer 'js2-print-label)

(defun js2-print-label (n i)
  (insert (js2-make-pad i)
          (js2-label-node-name n)
          ":\n"))

(defstruct (js2-labeled-stmt-node
            (:include js2-node)
            (:constructor nil)
            ;; type needs to be in `js2-side-effecting-tokens' to avoid spurious
            ;; no-side-effects warnings, hence js2-EXPR_RESULT.
            (:constructor make-js2-labeled-stmt-node (&key (type js2-EXPR_RESULT)
                                                           (pos js2-ts-cursor)
                                                           len labels stmt)))
  "AST node for a statement with one or more labels.
Multiple labels for a statement are collapsed into the labels field."
  labels  ; Lisp list of `js2-label-node'
  stmt)   ; the statement these labels are for

(put 'cl-struct-js2-labeled-stmt-node 'js2-visitor 'js2-visit-labeled-stmt)
(put 'cl-struct-js2-labeled-stmt-node 'js2-printer 'js2-print-labeled-stmt)

(defun js2-get-label-by-name (lbl-stmt name)
  "Return a `js2-label-node' by NAME from LBL-STMT's labels list.
Returns nil if no such label is in the list."
  (let ((label-list (js2-labeled-stmt-node-labels lbl-stmt))
        result)
    (while (and label-list (not result))
      (if (string= (js2-label-node-name (car label-list)) name)
          (setq result (car label-list))
        (setq label-list (cdr label-list))))
    result))

(defun js2-visit-labeled-stmt (n v)
  (dolist (label (js2-labeled-stmt-node-labels n))
    (js2-visit-ast label v))
  (js2-visit-ast (js2-labeled-stmt-node-stmt n) v))

(defun js2-print-labeled-stmt (n i)
  (dolist (label (js2-labeled-stmt-node-labels n))
    (js2-print-ast label i))
  (js2-print-ast (js2-labeled-stmt-node-stmt n) i))

(defun js2-labeled-stmt-node-contains (node label)
  "Return t if NODE contains LABEL in its label set.
NODE is a `js2-labels-node'.  LABEL is an identifier."
  (loop for nl in (js2-labeled-stmt-node-labels node)
        if (string= label (js2-label-node-name nl))
          return t
        finally return nil))

(defsubst js2-labeled-stmt-node-add-label (node label)
  "Add a `js2-label-node' to the label set for this statement."
  (setf (js2-labeled-stmt-node-labels node)
        (nconc (js2-labeled-stmt-node-labels node) (list label))))

(defstruct (js2-jump-node
            (:include js2-node)
            (:constructor nil))
  "Abstract supertype of break and continue nodes."
  label   ; `js2-name-node' for location of label identifier, if present
  target) ; target js2-labels-node or loop/switch statement

(defun js2-visit-jump-node (n v)
  ;; We don't visit the target, since it's a back-link.
  (js2-visit-ast (js2-jump-node-label n) v))

(defstruct (js2-break-node
            (:include js2-jump-node)
            (:constructor nil)
            (:constructor make-js2-break-node (&key (type js2-BREAK)
                                                    (pos js2-ts-cursor)
                                                    len label target)))
  "AST node for a break statement.
The label field is a `js2-name-node', possibly nil, for the named label
if provided.  E.g. in 'break foo', it represents 'foo'.  The target field
is the target of the break - a label node or enclosing loop/switch statement.")

(put 'cl-struct-js2-break-node 'js2-visitor 'js2-visit-jump-node)
(put 'cl-struct-js2-break-node 'js2-printer 'js2-print-break-node)

(defun js2-print-break-node (n i)
  (insert (js2-make-pad i) "break")
  (when (js2-break-node-label n)
    (insert " ")
    (js2-print-ast (js2-break-node-label n) 0))
  (insert ";\n"))

(defstruct (js2-continue-node
            (:include js2-jump-node)
            (:constructor nil)
            (:constructor make-js2-continue-node (&key (type js2-CONTINUE)
                                                       (pos js2-ts-cursor)
                                                       len label target)))
  "AST node for a continue statement.
The label field is the user-supplied enclosing label name, a `js2-name-node'.
It is nil if continue specifies no label.  The target field is the jump target:
a `js2-label-node' or the innermost enclosing loop.")

(put 'cl-struct-js2-continue-node 'js2-visitor 'js2-visit-jump-node)
(put 'cl-struct-js2-continue-node 'js2-printer 'js2-print-continue-node)

(defun js2-print-continue-node (n i)
  (insert (js2-make-pad i) "continue")
  (when (js2-continue-node-label n)
    (insert " ")
    (js2-print-ast (js2-continue-node-label n) 0))
  (insert ";\n"))

(defstruct (js2-function-node
            (:include js2-script-node)
            (:constructor nil)
            (:constructor make-js2-function-node (&key (type js2-FUNCTION)
                                                       (pos js2-ts-cursor)
                                                       len
                                                       (ftype 'FUNCTION)
                                                       (form 'FUNCTION_STATEMENT)
                                                       (name "")
                                                       params rest-p
                                                       body
                                                       lp rp)))
  "AST node for a function declaration.
The `params' field is a Lisp list of nodes.  Each node is either a simple
`js2-name-node', or if it's a destructuring-assignment parameter, a
`js2-array-node' or `js2-object-node'."
  ftype            ; FUNCTION, GETTER or SETTER
  form             ; FUNCTION_{STATEMENT|EXPRESSION|ARROW}
  name             ; function name (a `js2-name-node', or nil if anonymous)
  params           ; a Lisp list of destructuring forms or simple name nodes
  rest-p           ; if t, the last parameter is rest parameter
  body             ; a `js2-block-node' or expression node (1.8 only)
  lp               ; position of arg-list open-paren, or nil if omitted
  rp               ; position of arg-list close-paren, or nil if omitted
  ignore-dynamic   ; ignore value of the dynamic-scope flag (interpreter only)
  needs-activation ; t if we need an activation object for this frame
  is-generator     ; t if this function contains a yield
  member-expr)     ; nonstandard Ecma extension from Rhino

(put 'cl-struct-js2-function-node 'js2-visitor 'js2-visit-function-node)
(put 'cl-struct-js2-function-node 'js2-printer 'js2-print-function-node)

(defun js2-visit-function-node (n v)
  (js2-visit-ast (js2-function-node-name n) v)
  (dolist (p (js2-function-node-params n))
    (js2-visit-ast p v))
  (js2-visit-ast (js2-function-node-body n) v))

(defun js2-print-function-node (n i)
  (let* ((pad (js2-make-pad i))
         (getter (js2-node-get-prop n 'GETTER_SETTER))
         (name (or (js2-function-node-name n)
                   (js2-function-node-member-expr n)))
         (params (js2-function-node-params n))
         (arrow (eq (js2-function-node-form n) 'FUNCTION_ARROW))
         (rest-p (js2-function-node-rest-p n))
         (body (js2-function-node-body n))
         (expr (not (eq (js2-function-node-form n) 'FUNCTION_STATEMENT))))
    (unless (or getter arrow)
      (insert pad "function"))
    (when name
      (insert " ")
      (js2-print-ast name 0))
    (insert "(")
    (loop with len = (length params)
          for param in params
          for count from 1
          do
          (when (and rest-p (= count len))
            (insert "..."))
          (js2-print-ast param 0)
          (when (< count len)
            (insert ", ")))
    (insert ") ")
    (when arrow
      (insert "=> "))
    (insert "{")
    ;; TODO:  fix this to be smarter about indenting, etc.
    (unless expr
      (insert "\n"))
    (if (js2-block-node-p body)
        (js2-print-body body (1+ i))
      (js2-print-ast body 0))
    (insert pad "}")
    (unless expr
      (insert "\n"))))

(defun js2-function-name (node)
  "Return function name for NODE, a `js2-function-node', or nil if anonymous."
  (and (js2-function-node-name node)
       (js2-name-node-name (js2-function-node-name node))))

;; Having this be an expression node makes it more flexible.
;; There are IDE contexts, such as indentation in a for-loop initializer,
;; that work better if you assume it's an expression.  Whenever we have
;; a standalone var/const declaration, we just wrap with an expr stmt.
;; Eclipse apparently screwed this up and now has two versions, expr and stmt.
(defstruct (js2-var-decl-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-var-decl-node (&key (type js2-VAR)
                                                       (pos (js2-current-token-beg))
                                                       len kids
                                                       decl-type)))
  "AST node for a variable declaration list (VAR, CONST or LET).
The node bounds differ depending on the declaration type.  For VAR or
CONST declarations, the bounds include the var/const keyword.  For LET
declarations, the node begins at the position of the first child."
  kids        ; a Lisp list of `js2-var-init-node' structs.
  decl-type)  ; js2-VAR, js2-CONST or js2-LET

(put 'cl-struct-js2-var-decl-node 'js2-visitor 'js2-visit-var-decl)
(put 'cl-struct-js2-var-decl-node 'js2-printer 'js2-print-var-decl)

(defun js2-visit-var-decl (n v)
  (dolist (kid (js2-var-decl-node-kids n))
    (js2-visit-ast kid v)))

(defun js2-print-var-decl (n i)
  (let ((pad (js2-make-pad i))
        (tt (js2-var-decl-node-decl-type n)))
    (insert pad)
    (insert (cond
             ((= tt js2-VAR) "var ")
             ((= tt js2-LET) "let ")
             ((= tt js2-CONST) "const ")
             (t
              (error "malformed var-decl node"))))
    (loop with kids = (js2-var-decl-node-kids n)
          with len = (length kids)
          for kid in kids
          for count from 1
          do
          (js2-print-ast kid 0)
          (if (< count len)
              (insert ", ")))))

(defstruct (js2-var-init-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-var-init-node (&key (type js2-VAR)
                                                       (pos js2-ts-cursor)
                                                       len target
                                                       initializer)))
  "AST node for a variable declaration.
The type field will be js2-CONST for a const decl."
  target        ; `js2-name-node', `js2-object-node', or `js2-array-node'
  initializer)  ; initializer expression, a `js2-node'

(put 'cl-struct-js2-var-init-node 'js2-visitor 'js2-visit-var-init-node)
(put 'cl-struct-js2-var-init-node 'js2-printer 'js2-print-var-init-node)

(defun js2-visit-var-init-node (n v)
  (js2-visit-ast (js2-var-init-node-target n) v)
  (js2-visit-ast (js2-var-init-node-initializer n) v))

(defun js2-print-var-init-node (n i)
  (let ((pad (js2-make-pad i))
        (name (js2-var-init-node-target n))
        (init (js2-var-init-node-initializer n)))
    (insert pad)
    (js2-print-ast name 0)
    (when init
      (insert " = ")
      (js2-print-ast init 0))))

(defstruct (js2-cond-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-cond-node (&key (type js2-HOOK)
                                                   (pos js2-ts-cursor)
                                                   len
                                                   test-expr
                                                   true-expr
                                                   false-expr
                                                   q-pos c-pos)))
  "AST node for the ternary operator"
  test-expr
  true-expr
  false-expr
  q-pos   ; buffer position of ?
  c-pos)  ; buffer position of :

(put 'cl-struct-js2-cond-node 'js2-visitor 'js2-visit-cond-node)
(put 'cl-struct-js2-cond-node 'js2-printer 'js2-print-cond-node)

(defun js2-visit-cond-node (n v)
  (js2-visit-ast (js2-cond-node-test-expr n) v)
  (js2-visit-ast (js2-cond-node-true-expr n) v)
  (js2-visit-ast (js2-cond-node-false-expr n) v))

(defun js2-print-cond-node (n i)
  (let ((pad (js2-make-pad i)))
    (insert pad)
    (js2-print-ast (js2-cond-node-test-expr n) 0)
    (insert " ? ")
    (js2-print-ast (js2-cond-node-true-expr n) 0)
    (insert " : ")
    (js2-print-ast (js2-cond-node-false-expr n) 0)))

(defstruct (js2-infix-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-infix-node (&key type
                                                    (pos js2-ts-cursor)
                                                    len op-pos
                                                    left right)))
  "Represents infix expressions.
Includes assignment ops like `|=', and the comma operator.
The type field inherited from `js2-node' holds the operator."
  op-pos    ; buffer position where operator begins
  left      ; any `js2-node'
  right)    ; any `js2-node'

(put 'cl-struct-js2-infix-node 'js2-visitor 'js2-visit-infix-node)
(put 'cl-struct-js2-infix-node 'js2-printer 'js2-print-infix-node)

(defun js2-visit-infix-node (n v)
  (js2-visit-ast (js2-infix-node-left n) v)
  (js2-visit-ast (js2-infix-node-right n) v))

(defconst js2-operator-tokens
  (let ((table (make-hash-table :test 'eq))
        (tokens
         (list (cons js2-IN "in")
               (cons js2-TYPEOF "typeof")
               (cons js2-INSTANCEOF "instanceof")
               (cons js2-DELPROP "delete")
               (cons js2-COMMA ",")
               (cons js2-COLON ":")
               (cons js2-OR "||")
               (cons js2-AND "&&")
               (cons js2-INC "++")
               (cons js2-DEC "--")
               (cons js2-BITOR "|")
               (cons js2-BITXOR "^")
               (cons js2-BITAND "&")
               (cons js2-EQ "==")
               (cons js2-NE "!=")
               (cons js2-LT "<")
               (cons js2-LE "<=")
               (cons js2-GT ">")
               (cons js2-GE ">=")
               (cons js2-LSH "<<")
               (cons js2-RSH ">>")
               (cons js2-URSH ">>>")
               (cons js2-ADD "+")       ; infix plus
               (cons js2-SUB "-")       ; infix minus
               (cons js2-MUL "*")
               (cons js2-DIV "/")
               (cons js2-MOD "%")
               (cons js2-NOT "!")
               (cons js2-BITNOT "~")
               (cons js2-POS "+")       ; unary plus
               (cons js2-NEG "-")       ; unary minus
               (cons js2-SHEQ "===")    ; shallow equality
               (cons js2-SHNE "!==")    ; shallow inequality
               (cons js2-ASSIGN "=")
               (cons js2-ASSIGN_BITOR "|=")
               (cons js2-ASSIGN_BITXOR "^=")
               (cons js2-ASSIGN_BITAND "&=")
               (cons js2-ASSIGN_LSH "<<=")
               (cons js2-ASSIGN_RSH ">>=")
               (cons js2-ASSIGN_URSH ">>>=")
               (cons js2-ASSIGN_ADD "+=")
               (cons js2-ASSIGN_SUB "-=")
               (cons js2-ASSIGN_MUL "*=")
               (cons js2-ASSIGN_DIV "/=")
               (cons js2-ASSIGN_MOD "%="))))
    (loop for (k . v) in tokens do
          (puthash k v table))
    table))

(defun js2-print-infix-node (n i)
  (let* ((tt (js2-node-type n))
         (op (gethash tt js2-operator-tokens)))
    (unless op
      (error "unrecognized infix operator %s" (js2-node-type n)))
    (insert (js2-make-pad i))
    (js2-print-ast (js2-infix-node-left n) 0)
    (unless (= tt js2-COMMA)
      (insert " "))
    (insert op)
    (insert " ")
    (js2-print-ast (js2-infix-node-right n) 0)))

(defstruct (js2-assign-node
            (:include js2-infix-node)
            (:constructor nil)
            (:constructor make-js2-assign-node (&key type
                                                     (pos js2-ts-cursor)
                                                     len op-pos
                                                     left right)))
  "Represents any assignment.
The type field holds the actual assignment operator.")

(put 'cl-struct-js2-assign-node 'js2-visitor 'js2-visit-infix-node)
(put 'cl-struct-js2-assign-node 'js2-printer 'js2-print-infix-node)

(defstruct (js2-unary-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-unary-node (&key type ; required
                                                    (pos js2-ts-cursor)
                                                    len operand)))
  "AST node type for unary operator nodes.
The type field can be NOT, BITNOT, POS, NEG, INC, DEC,
TYPEOF, or DELPROP.  For INC or DEC, a 'postfix node
property is added if the operator follows the operand."
  operand)  ; a `js2-node' expression

(put 'cl-struct-js2-unary-node 'js2-visitor 'js2-visit-unary-node)
(put 'cl-struct-js2-unary-node 'js2-printer 'js2-print-unary-node)

(defun js2-visit-unary-node (n v)
  (js2-visit-ast (js2-unary-node-operand n) v))

(defun js2-print-unary-node (n i)
  (let* ((tt (js2-node-type n))
         (op (gethash tt js2-operator-tokens))
         (postfix (js2-node-get-prop n 'postfix)))
    (unless op
      (error "unrecognized unary operator %s" tt))
    (insert (js2-make-pad i))
    (unless postfix
      (insert op))
    (if (or (= tt js2-TYPEOF)
            (= tt js2-DELPROP))
        (insert " "))
    (js2-print-ast (js2-unary-node-operand n) 0)
    (when postfix
      (insert op))))

(defstruct (js2-let-node
            (:include js2-scope)
            (:constructor nil)
            (:constructor make-js2-let-node (&key (type js2-LETEXPR)
                                                  (pos (js2-current-token-beg))
                                                  len vars body
                                                  lp rp)))
  "AST node for a let expression or a let statement.
Note that a let declaration such as let x=6, y=7 is a `js2-var-decl-node'."
  vars   ; a `js2-var-decl-node'
  body   ; a `js2-node' representing the expression or body block
  lp
  rp)

(put 'cl-struct-js2-let-node 'js2-visitor 'js2-visit-let-node)
(put 'cl-struct-js2-let-node 'js2-printer 'js2-print-let-node)

(defun js2-visit-let-node (n v)
  (js2-visit-ast (js2-let-node-vars n) v)
  (js2-visit-ast (js2-let-node-body n) v))

(defun js2-print-let-node (n i)
  (insert (js2-make-pad i) "let (")
  (js2-print-ast (js2-let-node-vars n) 0)
  (insert ") ")
  (js2-print-ast (js2-let-node-body n) i))

(defstruct (js2-keyword-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-keyword-node (&key type
                                                      (pos (js2-current-token-beg))
                                                      (len (- js2-ts-cursor pos)))))
  "AST node representing a literal keyword such as `null'.
Used for `null', `this', `true', `false' and `debugger'.
The node type is set to js2-NULL, js2-THIS, etc.")

(put 'cl-struct-js2-keyword-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-keyword-node 'js2-printer 'js2-print-keyword-node)

(defun js2-print-keyword-node (n i)
  (insert (js2-make-pad i)
          (let ((tt (js2-node-type n)))
            (cond
             ((= tt js2-THIS) "this")
             ((= tt js2-NULL) "null")
             ((= tt js2-TRUE) "true")
             ((= tt js2-FALSE) "false")
             ((= tt js2-DEBUGGER) "debugger")
             (t (error "Invalid keyword literal type: %d" tt))))))

(defsubst js2-this-node-p (node)
  "Return t if NODE is a `js2-literal-node' of type js2-THIS."
  (eq (js2-node-type node) js2-THIS))

(defstruct (js2-new-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-new-node (&key (type js2-NEW)
                                                  (pos (js2-current-token-beg))
                                                  len target
                                                  args initializer
                                                  lp rp)))
  "AST node for new-expression such as new Foo()."
  target  ; an identifier or reference
  args    ; a Lisp list of argument nodes
  lp      ; position of left-paren, nil if omitted
  rp      ; position of right-paren, nil if omitted
  initializer) ; experimental Rhino syntax:  optional `js2-object-node'

(put 'cl-struct-js2-new-node 'js2-visitor 'js2-visit-new-node)
(put 'cl-struct-js2-new-node 'js2-printer 'js2-print-new-node)

(defun js2-visit-new-node (n v)
  (js2-visit-ast (js2-new-node-target n) v)
  (dolist (arg (js2-new-node-args n))
    (js2-visit-ast arg v))
  (js2-visit-ast (js2-new-node-initializer n) v))

(defun js2-print-new-node (n i)
  (insert (js2-make-pad i) "new ")
  (js2-print-ast (js2-new-node-target n))
  (insert "(")
  (js2-print-list (js2-new-node-args n))
  (insert ")")
  (when (js2-new-node-initializer n)
    (insert " ")
    (js2-print-ast (js2-new-node-initializer n))))

(defstruct (js2-name-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-name-node (&key (type js2-NAME)
                                                   (pos (js2-current-token-beg))
                                                   (len (- js2-ts-cursor
                                                           (js2-current-token-beg)))
                                                   (name (js2-current-token-string)))))
  "AST node for a JavaScript identifier"
  name   ; a string
  scope) ; a `js2-scope' (optional, used for codegen)

(put 'cl-struct-js2-name-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-name-node 'js2-printer 'js2-print-name-node)

(defun js2-print-name-node (n i)
  (insert (js2-make-pad i)
          (js2-name-node-name n)))

(defsubst js2-name-node-length (node)
  "Return identifier length of NODE, a `js2-name-node'.
Returns 0 if NODE is nil or its identifier field is nil."
  (if node
      (length (js2-name-node-name node))
    0))

(defstruct (js2-number-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-number-node (&key (type js2-NUMBER)
                                                     (pos (js2-current-token-beg))
                                                     (len (- js2-ts-cursor
                                                             (js2-current-token-beg)))
                                                     (value (js2-current-token-string))
                                                     (num-value (js2-token-number
                                                                 (js2-current-token))))))
  "AST node for a number literal."
  value      ; the original string, e.g. "6.02e23"
  num-value) ; the parsed number value

(put 'cl-struct-js2-number-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-number-node 'js2-printer 'js2-print-number-node)

(defun js2-print-number-node (n i)
  (insert (js2-make-pad i)
          (number-to-string (js2-number-node-num-value n))))

(defstruct (js2-regexp-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-regexp-node (&key (type js2-REGEXP)
                                                     (pos (js2-current-token-beg))
                                                     (len (- js2-ts-cursor
                                                             (js2-current-token-beg)))
                                                     value flags)))
  "AST node for a regular expression literal."
  value  ; the regexp string, without // delimiters
  flags) ; a string of flags, e.g. `mi'.

(put 'cl-struct-js2-regexp-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-regexp-node 'js2-printer 'js2-print-regexp)

(defun js2-print-regexp (n i)
  (insert (js2-make-pad i)
          "/"
          (js2-regexp-node-value n)
          "/")
  (if (js2-regexp-node-flags n)
      (insert (js2-regexp-node-flags n))))

(defstruct (js2-string-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-string-node (&key (type js2-STRING)
                                                     (pos (js2-current-token-beg))
                                                     (len (- js2-ts-cursor
                                                             (js2-current-token-beg)))
                                                     (value (js2-current-token-string)))))
  "String literal.
Escape characters are not evaluated; e.g. \n is 2 chars in value field.
You can tell the quote type by looking at the first character."
  value) ; the characters of the string, including the quotes

(put 'cl-struct-js2-string-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-string-node 'js2-printer 'js2-print-string-node)

(defun js2-print-string-node (n i)
  (insert (js2-make-pad i)
          (js2-node-string n)))

(defstruct (js2-array-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-array-node (&key (type js2-ARRAYLIT)
                                                    (pos js2-ts-cursor)
                                                    len elems)))
  "AST node for an array literal."
  elems)  ; list of expressions.  [foo,,bar] yields a nil middle element.

(put 'cl-struct-js2-array-node 'js2-visitor 'js2-visit-array-node)
(put 'cl-struct-js2-array-node 'js2-printer 'js2-print-array-node)

(defun js2-visit-array-node (n v)
  (dolist (e (js2-array-node-elems n))
    (js2-visit-ast e v)))  ; Can be nil; e.g. [a, ,b].

(defun js2-print-array-node (n i)
  (insert (js2-make-pad i) "[")
  (js2-print-list (js2-array-node-elems n))
  (insert "]"))

(defstruct (js2-object-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-object-node (&key (type js2-OBJECTLIT)
                                                     (pos js2-ts-cursor)
                                                     len
                                                     elems)))
  "AST node for an object literal expression.
`elems' is a list of either `js2-object-prop-node' or `js2-name-node'.
The latter represents abbreviation in destructuring expressions."
  elems)

(put 'cl-struct-js2-object-node 'js2-visitor 'js2-visit-object-node)
(put 'cl-struct-js2-object-node 'js2-printer 'js2-print-object-node)

(defun js2-visit-object-node (n v)
  (dolist (e (js2-object-node-elems n))
    (js2-visit-ast e v)))

(defun js2-print-object-node (n i)
  (insert (js2-make-pad i) "{")
  (js2-print-list (js2-object-node-elems n))
  (insert "}"))

(defstruct (js2-object-prop-node
            (:include js2-infix-node)
            (:constructor nil)
            (:constructor make-js2-object-prop-node (&key (type js2-COLON)
                                                          (pos js2-ts-cursor)
                                                          len left
                                                          right op-pos)))
  "AST node for an object literal prop:value entry.
The `left' field is the property:  a name node, string node or number node.
The `right' field is a `js2-node' representing the initializer value.")

(put 'cl-struct-js2-object-prop-node 'js2-visitor 'js2-visit-infix-node)
(put 'cl-struct-js2-object-prop-node 'js2-printer 'js2-print-object-prop-node)

(defun js2-print-object-prop-node (n i)
  (insert (js2-make-pad i))
  (js2-print-ast (js2-object-prop-node-left n) 0)
  (insert ": ")
  (js2-print-ast (js2-object-prop-node-right n) 0))

(defstruct (js2-getter-setter-node
            (:include js2-infix-node)
            (:constructor nil)
            (:constructor make-js2-getter-setter-node (&key type ; GET or SET
                                                            (pos js2-ts-cursor)
                                                            len left right)))
  "AST node for a getter/setter property in an object literal.
The `left' field is the `js2-name-node' naming the getter/setter prop.
The `right' field is always an anonymous `js2-function-node' with a node
property `GETTER_SETTER' set to js2-GET or js2-SET. ")

(put 'cl-struct-js2-getter-setter-node 'js2-visitor 'js2-visit-infix-node)
(put 'cl-struct-js2-getter-setter-node 'js2-printer 'js2-print-getter-setter)

(defun js2-print-getter-setter (n i)
  (let ((pad (js2-make-pad i))
        (left (js2-getter-setter-node-left n))
        (right (js2-getter-setter-node-right n)))
    (insert pad)
    (insert (if (= (js2-node-type n) js2-GET) "get " "set "))
    (js2-print-ast left 0)
    (js2-print-ast right 0)))

(defstruct (js2-prop-get-node
            (:include js2-infix-node)
            (:constructor nil)
            (:constructor make-js2-prop-get-node (&key (type js2-GETPROP)
                                                       (pos js2-ts-cursor)
                                                       len left right)))
  "AST node for a dotted property reference, e.g. foo.bar or foo().bar")

(put 'cl-struct-js2-prop-get-node 'js2-visitor 'js2-visit-prop-get-node)
(put 'cl-struct-js2-prop-get-node 'js2-printer 'js2-print-prop-get-node)

(defun js2-visit-prop-get-node (n v)
  (js2-visit-ast (js2-prop-get-node-left n) v)
  (js2-visit-ast (js2-prop-get-node-right n) v))

(defun js2-print-prop-get-node (n i)
  (insert (js2-make-pad i))
  (js2-print-ast (js2-prop-get-node-left n) 0)
  (insert ".")
  (js2-print-ast (js2-prop-get-node-right n) 0))

(defstruct (js2-elem-get-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-elem-get-node (&key (type js2-GETELEM)
                                                       (pos js2-ts-cursor)
                                                       len target element
                                                       lb rb)))
  "AST node for an array index expression such as foo[bar]."
  target  ; a `js2-node' - the expression preceding the "."
  element ; a `js2-node' - the expression in brackets
  lb      ; position of left-bracket, nil if omitted
  rb)     ; position of right-bracket, nil if omitted

(put 'cl-struct-js2-elem-get-node 'js2-visitor 'js2-visit-elem-get-node)
(put 'cl-struct-js2-elem-get-node 'js2-printer 'js2-print-elem-get-node)

(defun js2-visit-elem-get-node (n v)
  (js2-visit-ast (js2-elem-get-node-target n) v)
  (js2-visit-ast (js2-elem-get-node-element n) v))

(defun js2-print-elem-get-node (n i)
  (insert (js2-make-pad i))
  (js2-print-ast (js2-elem-get-node-target n) 0)
  (insert "[")
  (js2-print-ast (js2-elem-get-node-element n) 0)
  (insert "]"))

(defstruct (js2-call-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-call-node (&key (type js2-CALL)
                                                   (pos js2-ts-cursor)
                                                   len target args
                                                   lp rp)))
  "AST node for a JavaScript function call."
  target  ; a `js2-node' evaluating to the function to call
  args  ; a Lisp list of `js2-node' arguments
  lp    ; position of open-paren, or nil if missing
  rp)   ; position of close-paren, or nil if missing

(put 'cl-struct-js2-call-node 'js2-visitor 'js2-visit-call-node)
(put 'cl-struct-js2-call-node 'js2-printer 'js2-print-call-node)

(defun js2-visit-call-node (n v)
  (js2-visit-ast (js2-call-node-target n) v)
  (dolist (arg (js2-call-node-args n))
    (js2-visit-ast arg v)))

(defun js2-print-call-node (n i)
  (insert (js2-make-pad i))
  (js2-print-ast (js2-call-node-target n) 0)
  (insert "(")
  (js2-print-list (js2-call-node-args n))
  (insert ")"))

(defstruct (js2-yield-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-yield-node (&key (type js2-YIELD)
                                                    (pos js2-ts-cursor)
                                                    len value)))
  "AST node for yield statement or expression."
  value) ; optional:  value to be yielded

(put 'cl-struct-js2-yield-node 'js2-visitor 'js2-visit-yield-node)
(put 'cl-struct-js2-yield-node 'js2-printer 'js2-print-yield-node)

(defun js2-visit-yield-node (n v)
  (js2-visit-ast (js2-yield-node-value n) v))

(defun js2-print-yield-node (n i)
  (insert (js2-make-pad i))
  (insert "yield")
  (when (js2-yield-node-value n)
    (insert " ")
    (js2-print-ast (js2-yield-node-value n) 0)))

(defstruct (js2-paren-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-paren-node (&key (type js2-LP)
                                                    (pos js2-ts-cursor)
                                                    len expr)))
  "AST node for a parenthesized expression.
In particular, used when the parens are syntactically optional,
as opposed to required parens such as those enclosing an if-conditional."
  expr)   ; `js2-node'

(put 'cl-struct-js2-paren-node 'js2-visitor 'js2-visit-paren-node)
(put 'cl-struct-js2-paren-node 'js2-printer 'js2-print-paren-node)

(defun js2-visit-paren-node (n v)
  (js2-visit-ast (js2-paren-node-expr n) v))

(defun js2-print-paren-node (n i)
  (insert (js2-make-pad i))
  (insert "(")
  (js2-print-ast (js2-paren-node-expr n) 0)
  (insert ")"))

(defstruct (js2-array-comp-node
            (:include js2-scope)
            (:constructor nil)
            (:constructor make-js2-array-comp-node (&key (type js2-ARRAYCOMP)
                                                         (pos js2-ts-cursor)
                                                         len result
                                                         loops filter
                                                         if-pos lp rp)))
  "AST node for an Array comprehension such as [[x,y] for (x in foo) for (y in bar)]."
  result  ; result expression (just after left-bracket)
  loops   ; a Lisp list of `js2-array-comp-loop-node'
  filter  ; guard/filter expression
  if-pos  ; buffer pos of 'if' keyword, if present, else nil
  lp      ; buffer position of if-guard left-paren, or nil if not present
  rp)     ; buffer position of if-guard right-paren, or nil if not present

(put 'cl-struct-js2-array-comp-node 'js2-visitor 'js2-visit-array-comp-node)
(put 'cl-struct-js2-array-comp-node 'js2-printer 'js2-print-array-comp-node)

(defun js2-visit-array-comp-node (n v)
  (js2-visit-ast (js2-array-comp-node-result n) v)
  (dolist (l (js2-array-comp-node-loops n))
    (js2-visit-ast l v))
  (js2-visit-ast (js2-array-comp-node-filter n) v))

(defun js2-print-array-comp-node (n i)
  (let ((pad (js2-make-pad i))
        (result (js2-array-comp-node-result n))
        (loops (js2-array-comp-node-loops n))
        (filter (js2-array-comp-node-filter n)))
    (insert pad "[")
    (js2-print-ast result 0)
    (dolist (l loops)
      (insert " ")
      (js2-print-ast l 0))
    (when filter
      (insert " if (")
      (js2-print-ast filter 0)
      (insert ")"))
    (insert "]")))

(defstruct (js2-array-comp-loop-node
            (:include js2-for-in-node)
            (:constructor nil)
            (:constructor make-js2-array-comp-loop-node (&key (type js2-FOR)
                                                              (pos js2-ts-cursor)
                                                              len iterator
                                                              object in-pos
                                                              foreach-p
                                                              each-pos
                                                              forof-p
                                                              lp rp)))
  "AST subtree for each 'for (foo in bar)' loop in an array comprehension.")

(put 'cl-struct-js2-array-comp-loop-node 'js2-visitor 'js2-visit-array-comp-loop)
(put 'cl-struct-js2-array-comp-loop-node 'js2-printer 'js2-print-array-comp-loop)

(defun js2-visit-array-comp-loop (n v)
  (js2-visit-ast (js2-array-comp-loop-node-iterator n) v)
  (js2-visit-ast (js2-array-comp-loop-node-object n) v))

(defun js2-print-array-comp-loop (n _i)
  (insert "for ")
  (when (js2-array-comp-loop-node-foreach-p n) (insert "each "))
  (insert "(")
  (js2-print-ast (js2-array-comp-loop-node-iterator n) 0)
  (insert (if (js2-array-comp-loop-node-forof-p n)
              " of " " in "))
  (js2-print-ast (js2-array-comp-loop-node-object n) 0)
  (insert ")"))

(defstruct (js2-empty-expr-node
            (:include js2-node)
            (:constructor nil)
            (:constructor make-js2-empty-expr-node (&key (type js2-EMPTY)
                                                         (pos (js2-current-token-beg))
                                                         len)))
  "AST node for an empty expression.")

(put 'cl-struct-js2-empty-expr-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-empty-expr-node 'js2-printer 'js2-print-none)

(defstruct (js2-xml-node
            (:include js2-block-node)
            (:constructor nil)
            (:constructor make-js2-xml-node (&key (type js2-XML)
                                                  (pos (js2-current-token-beg))
                                                  len kids)))
  "AST node for initial parse of E4X literals.
The kids field is a list of XML fragments, each a `js2-string-node' or
a `js2-xml-js-expr-node'.  Equivalent to Rhino's XmlLiteral node.")

(put 'cl-struct-js2-xml-node 'js2-visitor 'js2-visit-block)
(put 'cl-struct-js2-xml-node 'js2-printer 'js2-print-xml-node)

(defun js2-print-xml-node (n i)
  (dolist (kid (js2-xml-node-kids n))
    (js2-print-ast kid i)))

(defstruct (js2-xml-js-expr-node
            (:include js2-xml-node)
            (:constructor nil)
            (:constructor make-js2-xml-js-expr-node (&key (type js2-XML)
                                                          (pos js2-ts-cursor)
                                                          len expr)))
  "AST node for an embedded JavaScript {expression} in an E4X literal.
The start and end fields correspond to the curly-braces."
  expr)  ; a `js2-expr-node' of some sort

(put 'cl-struct-js2-xml-js-expr-node 'js2-visitor 'js2-visit-xml-js-expr)
(put 'cl-struct-js2-xml-js-expr-node 'js2-printer 'js2-print-xml-js-expr)

(defun js2-visit-xml-js-expr (n v)
  (js2-visit-ast (js2-xml-js-expr-node-expr n) v))

(defun js2-print-xml-js-expr (n i)
  (insert (js2-make-pad i))
  (insert "{")
  (js2-print-ast (js2-xml-js-expr-node-expr n) 0)
  (insert "}"))

(defstruct (js2-xml-dot-query-node
            (:include js2-infix-node)
            (:constructor nil)
            (:constructor make-js2-xml-dot-query-node (&key (type js2-DOTQUERY)
                                                            (pos js2-ts-cursor)
                                                            op-pos len left
                                                            right rp)))
  "AST node for an E4X foo.(bar) filter expression.
Note that the left-paren is automatically the character immediately
following the dot (.) in the operator.  No whitespace is permitted
between the dot and the lp by the scanner."
  rp)

(put 'cl-struct-js2-xml-dot-query-node 'js2-visitor 'js2-visit-infix-node)
(put 'cl-struct-js2-xml-dot-query-node 'js2-printer 'js2-print-xml-dot-query)

(defun js2-print-xml-dot-query (n i)
  (insert (js2-make-pad i))
  (js2-print-ast (js2-xml-dot-query-node-left n) 0)
  (insert ".(")
  (js2-print-ast (js2-xml-dot-query-node-right n) 0)
  (insert ")"))

(defstruct (js2-xml-ref-node
            (:include js2-node)
            (:constructor nil))  ; abstract
  "Base type for E4X XML attribute-access or property-get expressions.
Such expressions can take a variety of forms.  The general syntax has
three parts:

  - (optional) an @ (specifying an attribute access)
  - (optional) a namespace (a `js2-name-node') and double-colon
  - (required) either a `js2-name-node' or a bracketed [expression]

The property-name expressions (examples:  ns::name, @name) are
represented as `js2-xml-prop-ref' nodes.  The bracketed-expression
versions (examples:  ns::[name], @[name]) become `js2-xml-elem-ref' nodes.

This node type (or more specifically, its subclasses) will sometimes
be the right-hand child of a `js2-prop-get-node' or a
`js2-infix-node' of type `js2-DOTDOT', the .. xml-descendants operator.
The `js2-xml-ref-node' may also be a standalone primary expression with
no explicit target, which is valid in certain expression contexts such as

  company..employee.(@id < 100)

in this case, the @id is a `js2-xml-ref' that is part of an infix '<'
expression whose parent is a `js2-xml-dot-query-node'."
  namespace
  at-pos
  colon-pos)

(defsubst js2-xml-ref-node-attr-access-p (node)
  "Return non-nil if this expression began with an @-token."
  (and (numberp (js2-xml-ref-node-at-pos node))
       (plusp (js2-xml-ref-node-at-pos node))))

(defstruct (js2-xml-prop-ref-node
            (:include js2-xml-ref-node)
            (:constructor nil)
            (:constructor make-js2-xml-prop-ref-node (&key (type js2-REF_NAME)
                                                           (pos (js2-current-token-beg))
                                                           len propname
                                                           namespace at-pos
                                                           colon-pos)))
  "AST node for an E4X XML [expr] property-ref expression.
The JavaScript syntax is an optional @, an optional ns::, and a name.

  [ '@' ] [ name '::' ] name

Examples include name, ns::name, ns::*, *::name, *::*, @attr, @ns::attr,
@ns::*, @*::attr, @*::*, and @*.

The node starts at the @ token, if present.  Otherwise it starts at the
namespace name.  The node bounds extend through the closing right-bracket,
or if it is missing due to a syntax error, through the end of the index
expression."
  propname)

(put 'cl-struct-js2-xml-prop-ref-node 'js2-visitor 'js2-visit-xml-prop-ref-node)
(put 'cl-struct-js2-xml-prop-ref-node 'js2-printer 'js2-print-xml-prop-ref-node)

(defun js2-visit-xml-prop-ref-node (n v)
  (js2-visit-ast (js2-xml-prop-ref-node-namespace n) v)
  (js2-visit-ast (js2-xml-prop-ref-node-propname n) v))

(defun js2-print-xml-prop-ref-node (n i)
  (insert (js2-make-pad i))
  (if (js2-xml-ref-node-attr-access-p n)
      (insert "@"))
  (when (js2-xml-prop-ref-node-namespace n)
    (js2-print-ast (js2-xml-prop-ref-node-namespace n) 0)
    (insert "::"))
  (if (js2-xml-prop-ref-node-propname n)
      (js2-print-ast (js2-xml-prop-ref-node-propname n) 0)))

(defstruct (js2-xml-elem-ref-node
            (:include js2-xml-ref-node)
            (:constructor nil)
            (:constructor make-js2-xml-elem-ref-node (&key (type js2-REF_MEMBER)
                                                           (pos (js2-current-token-beg))
                                                           len expr lb rb
                                                           namespace at-pos
                                                           colon-pos)))
  "AST node for an E4X XML [expr] member-ref expression.
Syntax:

 [ '@' ] [ name '::' ] '[' expr ']'

Examples include ns::[expr], @ns::[expr], @[expr], *::[expr] and @*::[expr].

Note that the form [expr] (i.e. no namespace or attribute-qualifier)
is not a legal E4X XML element-ref expression, since it's already used
for standard JavaScript element-get array indexing.  Hence, a
`js2-xml-elem-ref-node' always has either the attribute-qualifier, a
non-nil namespace node, or both.

The node starts at the @ token, if present.  Otherwise it starts
at the namespace name.  The node bounds extend through the closing
right-bracket, or if it is missing due to a syntax error, through the
end of the index expression."
  expr  ; the bracketed index expression
  lb
  rb)

(put 'cl-struct-js2-xml-elem-ref-node 'js2-visitor 'js2-visit-xml-elem-ref-node)
(put 'cl-struct-js2-xml-elem-ref-node 'js2-printer 'js2-print-xml-elem-ref-node)

(defun js2-visit-xml-elem-ref-node (n v)
  (js2-visit-ast (js2-xml-elem-ref-node-namespace n) v)
  (js2-visit-ast (js2-xml-elem-ref-node-expr n) v))

(defun js2-print-xml-elem-ref-node (n i)
  (insert (js2-make-pad i))
  (if (js2-xml-ref-node-attr-access-p n)
      (insert "@"))
  (when (js2-xml-elem-ref-node-namespace n)
    (js2-print-ast (js2-xml-elem-ref-node-namespace n) 0)
    (insert "::"))
  (insert "[")
  (if (js2-xml-elem-ref-node-expr n)
      (js2-print-ast (js2-xml-elem-ref-node-expr n) 0))
  (insert "]"))

;;; Placeholder nodes for when we try parsing the XML literals structurally.

(defstruct (js2-xml-start-tag-node
            (:include js2-xml-node)
            (:constructor nil)
            (:constructor make-js2-xml-start-tag-node (&key (type js2-XML)
                                                            (pos js2-ts-cursor)
                                                            len name attrs kids
                                                            empty-p)))
  "AST node for an XML start-tag.  Not currently used.
The `kids' field is a Lisp list of child content nodes."
  name      ; a `js2-xml-name-node'
  attrs     ; a Lisp list of `js2-xml-attr-node'
  empty-p)  ; t if this is an empty element such as <foo bar="baz"/>

(put 'cl-struct-js2-xml-start-tag-node 'js2-visitor 'js2-visit-xml-start-tag)
(put 'cl-struct-js2-xml-start-tag-node 'js2-printer 'js2-print-xml-start-tag)

(defun js2-visit-xml-start-tag (n v)
  (js2-visit-ast (js2-xml-start-tag-node-name n) v)
  (dolist (attr (js2-xml-start-tag-node-attrs n))
    (js2-visit-ast attr v))
  (js2-visit-block n v))

(defun js2-print-xml-start-tag (n i)
  (insert (js2-make-pad i) "<")
  (js2-print-ast (js2-xml-start-tag-node-name n) 0)
  (when (js2-xml-start-tag-node-attrs n)
    (insert " ")
    (js2-print-list (js2-xml-start-tag-node-attrs n) " "))
  (insert ">"))

;; I -think- I'm going to make the parent node the corresponding start-tag,
;; and add the end-tag to the kids list of the parent as well.
(defstruct (js2-xml-end-tag-node
            (:include js2-xml-node)
            (:constructor nil)
            (:constructor make-js2-xml-end-tag-node (&key (type js2-XML)
                                                          (pos js2-ts-cursor)
                                                          len name)))
  "AST node for an XML end-tag.  Not currently used."
  name)  ; a `js2-xml-name-node'

(put 'cl-struct-js2-xml-end-tag-node 'js2-visitor 'js2-visit-xml-end-tag)
(put 'cl-struct-js2-xml-end-tag-node 'js2-printer 'js2-print-xml-end-tag)

(defun js2-visit-xml-end-tag (n v)
  (js2-visit-ast (js2-xml-end-tag-node-name n) v))

(defun js2-print-xml-end-tag (n i)
  (insert (js2-make-pad i))
  (insert "</")
  (js2-print-ast (js2-xml-end-tag-node-name n) 0)
  (insert ">"))

(defstruct (js2-xml-name-node
            (:include js2-xml-node)
            (:constructor nil)
            (:constructor make-js2-xml-name-node (&key (type js2-XML)
                                                       (pos js2-ts-cursor)
                                                       len namespace kids)))
  "AST node for an E4X XML name.  Not currently used.
Any XML name can be qualified with a namespace, hence the namespace field.
Further, any E4X name can be comprised of arbitrary JavaScript {} expressions.
The kids field is a list of `js2-name-node' and `js2-xml-js-expr-node'.
For a simple name, the kids list has exactly one node, a `js2-name-node'."
  namespace)  ; a `js2-string-node'

(put 'cl-struct-js2-xml-name-node 'js2-visitor 'js2-visit-xml-name-node)
(put 'cl-struct-js2-xml-name-node 'js2-printer 'js2-print-xml-name-node)

(defun js2-visit-xml-name-node (n v)
  (js2-visit-ast (js2-xml-name-node-namespace n) v))

(defun js2-print-xml-name-node (n i)
  (insert (js2-make-pad i))
  (when (js2-xml-name-node-namespace n)
    (js2-print-ast (js2-xml-name-node-namespace n) 0)
    (insert "::"))
  (dolist (kid (js2-xml-name-node-kids n))
    (js2-print-ast kid 0)))

(defstruct (js2-xml-pi-node
            (:include js2-xml-node)
            (:constructor nil)
            (:constructor make-js2-xml-pi-node (&key (type js2-XML)
                                                     (pos js2-ts-cursor)
                                                     len name attrs)))
  "AST node for an E4X XML processing instruction.  Not currently used."
  name   ; a `js2-xml-name-node'
  attrs) ; a list of `js2-xml-attr-node'

(put 'cl-struct-js2-xml-pi-node 'js2-visitor 'js2-visit-xml-pi-node)
(put 'cl-struct-js2-xml-pi-node 'js2-printer 'js2-print-xml-pi-node)

(defun js2-visit-xml-pi-node (n v)
  (js2-visit-ast (js2-xml-pi-node-name n) v)
  (dolist (attr (js2-xml-pi-node-attrs n))
    (js2-visit-ast attr v)))

(defun js2-print-xml-pi-node (n i)
  (insert (js2-make-pad i) "<?")
  (js2-print-ast (js2-xml-pi-node-name n))
  (when (js2-xml-pi-node-attrs n)
    (insert " ")
    (js2-print-list (js2-xml-pi-node-attrs n)))
  (insert "?>"))

(defstruct (js2-xml-cdata-node
            (:include js2-xml-node)
            (:constructor nil)
            (:constructor make-js2-xml-cdata-node (&key (type js2-XML)
                                                        (pos js2-ts-cursor)
                                                        len content)))
  "AST node for a CDATA escape section.  Not currently used."
  content)  ; a `js2-string-node' with node-property 'quote-type 'cdata

(put 'cl-struct-js2-xml-cdata-node 'js2-visitor 'js2-visit-xml-cdata-node)
(put 'cl-struct-js2-xml-cdata-node 'js2-printer 'js2-print-xml-cdata-node)

(defun js2-visit-xml-cdata-node (n v)
  (js2-visit-ast (js2-xml-cdata-node-content n) v))

(defun js2-print-xml-cdata-node (n i)
  (insert (js2-make-pad i))
  (js2-print-ast (js2-xml-cdata-node-content n)))

(defstruct (js2-xml-attr-node
            (:include js2-xml-node)
            (:constructor nil)
            (:constructor make-js2-attr-node (&key (type js2-XML)
                                                   (pos js2-ts-cursor)
                                                   len name value
                                                   eq-pos quote-type)))
  "AST node representing a foo='bar' XML attribute value.  Not yet used."
  name   ; a `js2-xml-name-node'
  value  ; a `js2-xml-name-node'
  eq-pos ; buffer position of "=" sign
  quote-type) ; 'single or 'double

(put 'cl-struct-js2-xml-attr-node 'js2-visitor 'js2-visit-xml-attr-node)
(put 'cl-struct-js2-xml-attr-node 'js2-printer 'js2-print-xml-attr-node)

(defun js2-visit-xml-attr-node (n v)
  (js2-visit-ast (js2-xml-attr-node-name n) v)
  (js2-visit-ast (js2-xml-attr-node-value n) v))

(defun js2-print-xml-attr-node (n i)
  (let ((quote (if (eq (js2-xml-attr-node-quote-type n) 'single)
                   "'"
                 "\"")))
    (insert (js2-make-pad i))
    (js2-print-ast (js2-xml-attr-node-name n) 0)
    (insert "=" quote)
    (js2-print-ast (js2-xml-attr-node-value n) 0)
    (insert quote)))

(defstruct (js2-xml-text-node
            (:include js2-xml-node)
            (:constructor nil)
            (:constructor make-js2-text-node (&key (type js2-XML)
                                                   (pos js2-ts-cursor)
                                                   len content)))
  "AST node for an E4X XML text node.  Not currently used."
  content)  ; a Lisp list of `js2-string-node' and `js2-xml-js-expr-node'

(put 'cl-struct-js2-xml-text-node 'js2-visitor 'js2-visit-xml-text-node)
(put 'cl-struct-js2-xml-text-node 'js2-printer 'js2-print-xml-text-node)

(defun js2-visit-xml-text-node (n v)
  (js2-visit-ast (js2-xml-text-node-content n) v))

(defun js2-print-xml-text-node (n i)
  (insert (js2-make-pad i))
  (dolist (kid (js2-xml-text-node-content n))
    (js2-print-ast kid)))

(defstruct (js2-xml-comment-node
            (:include js2-xml-node)
            (:constructor nil)
            (:constructor make-js2-xml-comment-node (&key (type js2-XML)
                                                          (pos js2-ts-cursor)
                                                          len)))
  "AST node for E4X XML comment.  Not currently used.")

(put 'cl-struct-js2-xml-comment-node 'js2-visitor 'js2-visit-none)
(put 'cl-struct-js2-xml-comment-node 'js2-printer 'js2-print-xml-comment)

(defun js2-print-xml-comment (n i)
  (insert (js2-make-pad i)
          (js2-node-string n)))

;;; Node utilities

(defsubst js2-node-line (n)
  "Fetch the source line number at the start of node N.
This is O(n) in the length of the source buffer; use prudently."
  (1+ (count-lines (point-min) (js2-node-abs-pos n))))

(defsubst js2-block-node-kid (n i)
  "Return child I of node N, or nil if there aren't that many."
  (nth i (js2-block-node-kids n)))

(defsubst js2-block-node-first (n)
  "Return first child of block node N, or nil if there is none."
  (first (js2-block-node-kids n)))

(defun js2-node-root (n)
  "Return the root of the AST containing N.
If N has no parent pointer, returns N."
  (let ((parent (js2-node-parent n)))
    (if parent
        (js2-node-root parent)
      n)))

(defsubst js2-node-short-name (n)
  "Return the short name of node N as a string, e.g. `js2-if-node'."
  (substring (symbol-name (aref n 0))
             (length "cl-struct-")))

(defun js2-node-child-list (node)
  "Return the child list for NODE, a Lisp list of nodes.
Works for block nodes, array nodes, obj literals, funarg lists,
var decls and try nodes (for catch clauses).  Note that you should call
`js2-block-node-kids' on the function body for the body statements.
Returns nil for zero-length child lists or unsupported nodes."
  (cond
   ((js2-function-node-p node)
    (js2-function-node-params node))
   ((js2-block-node-p node)
    (js2-block-node-kids node))
   ((js2-try-node-p node)
    (js2-try-node-catch-clauses node))
   ((js2-array-node-p node)
    (js2-array-node-elems node))
   ((js2-object-node-p node)
    (js2-object-node-elems node))
   ((js2-call-node-p node)
    (js2-call-node-args node))
   ((js2-new-node-p node)
    (js2-new-node-args node))
   ((js2-var-decl-node-p node)
    (js2-var-decl-node-kids node))
   (t
    nil)))

(defun js2-node-set-child-list (node kids)
  "Set the child list for NODE to KIDS."
   (cond
    ((js2-function-node-p node)
     (setf (js2-function-node-params node) kids))
    ((js2-block-node-p node)
     (setf (js2-block-node-kids node) kids))
    ((js2-try-node-p node)
     (setf (js2-try-node-catch-clauses node) kids))
    ((js2-array-node-p node)
     (setf (js2-array-node-elems node) kids))
    ((js2-object-node-p node)
     (setf (js2-object-node-elems node) kids))
    ((js2-call-node-p node)
     (setf (js2-call-node-args node) kids))
    ((js2-new-node-p node)
     (setf (js2-new-node-args node) kids))
    ((js2-var-decl-node-p node)
     (setf (js2-var-decl-node-kids node) kids))
    (t
     (error "Unsupported node type: %s" (js2-node-short-name node))))
   kids)

;; All because Common Lisp doesn't support multiple inheritance for defstructs.
(defconst js2-paren-expr-nodes
  '(cl-struct-js2-array-comp-loop-node
    cl-struct-js2-array-comp-node
    cl-struct-js2-call-node
    cl-struct-js2-catch-node
    cl-struct-js2-do-node
    cl-struct-js2-elem-get-node
    cl-struct-js2-for-in-node
    cl-struct-js2-for-node
    cl-struct-js2-function-node
    cl-struct-js2-if-node
    cl-struct-js2-let-node
    cl-struct-js2-new-node
    cl-struct-js2-paren-node
    cl-struct-js2-switch-node
    cl-struct-js2-while-node
    cl-struct-js2-with-node
    cl-struct-js2-xml-dot-query-node)
  "Node types that can have a parenthesized child expression.
In particular, nodes that respond to `js2-node-lp' and `js2-node-rp'.")

(defsubst js2-paren-expr-node-p (node)
  "Return t for nodes that typically have a parenthesized child expression.
Useful for computing the indentation anchors for arg-lists and conditions.
Note that it may return a false positive, for instance when NODE is
a `js2-new-node' and there are no arguments or parentheses."
  (memq (aref node 0) js2-paren-expr-nodes))

;; Fake polymorphism... yech.
(defun js2-node-lp (node)
  "Return relative left-paren position for NODE, if applicable.
For `js2-elem-get-node' structs, returns left-bracket position.
Note that the position may be nil in the case of a parse error."
  (cond
   ((js2-elem-get-node-p node)
    (js2-elem-get-node-lb node))
   ((js2-loop-node-p node)
    (js2-loop-node-lp node))
   ((js2-function-node-p node)
    (js2-function-node-lp node))
   ((js2-if-node-p node)
    (js2-if-node-lp node))
   ((js2-new-node-p node)
    (js2-new-node-lp node))
   ((js2-call-node-p node)
    (js2-call-node-lp node))
   ((js2-paren-node-p node)
    0)
   ((js2-switch-node-p node)
    (js2-switch-node-lp node))
   ((js2-catch-node-p node)
    (js2-catch-node-lp node))
   ((js2-let-node-p node)
    (js2-let-node-lp node))
   ((js2-array-comp-node-p node)
    (js2-array-comp-node-lp node))
   ((js2-with-node-p node)
    (js2-with-node-lp node))
   ((js2-xml-dot-query-node-p node)
    (1+ (js2-infix-node-op-pos node)))
   (t
    (error "Unsupported node type: %s" (js2-node-short-name node)))))

;; Fake polymorphism... blech.
(defun js2-node-rp (node)
  "Return relative right-paren position for NODE, if applicable.
For `js2-elem-get-node' structs, returns right-bracket position.
Note that the position may be nil in the case of a parse error."
  (cond
   ((js2-elem-get-node-p node)
    (js2-elem-get-node-rb node))
   ((js2-loop-node-p node)
    (js2-loop-node-rp node))
   ((js2-function-node-p node)
    (js2-function-node-rp node))
   ((js2-if-node-p node)
    (js2-if-node-rp node))
   ((js2-new-node-p node)
    (js2-new-node-rp node))
   ((js2-call-node-p node)
    (js2-call-node-rp node))
   ((js2-paren-node-p node)
    (1- (js2-node-len node)))
   ((js2-switch-node-p node)
    (js2-switch-node-rp node))
   ((js2-catch-node-p node)
    (js2-catch-node-rp node))
   ((js2-let-node-p node)
    (js2-let-node-rp node))
   ((js2-array-comp-node-p node)
    (js2-array-comp-node-rp node))
   ((js2-with-node-p node)
    (js2-with-node-rp node))
   ((js2-xml-dot-query-node-p node)
    (1+ (js2-xml-dot-query-node-rp node)))
   (t
    (error "Unsupported node type: %s" (js2-node-short-name node)))))

(defsubst js2-node-first-child (node)
  "Return the first element of `js2-node-child-list' for NODE."
  (car (js2-node-child-list node)))

(defsubst js2-node-last-child (node)
  "Return the last element of `js2-node-last-child' for NODE."
  (car (last (js2-node-child-list node))))

(defun js2-node-prev-sibling (node)
  "Return the previous statement in parent.
Works for parents supported by `js2-node-child-list'.
Returns nil if NODE is not in the parent, or PARENT is
not a supported node, or if NODE is the first child."
  (let* ((p (js2-node-parent node))
         (kids (js2-node-child-list p))
         (sib (car kids)))
    (while (and kids
                (not (eq node (cadr kids))))
      (setq kids (cdr kids)
            sib (car kids)))
    sib))

(defun js2-node-next-sibling (node)
  "Return the next statement in parent block.
Returns nil if NODE is not in the block, or PARENT is not
a block node, or if NODE is the last statement."
  (let* ((p (js2-node-parent node))
         (kids (js2-node-child-list p)))
    (while (and kids
                (not (eq node (car kids))))
      (setq kids (cdr kids)))
    (cadr kids)))

(defun js2-node-find-child-before (pos parent &optional after)
  "Find the last child that starts before POS in parent.
If AFTER is non-nil, returns first child starting after POS.
POS is an absolute buffer position.  PARENT is any node
supported by `js2-node-child-list'.
Returns nil if no applicable child is found."
  (let ((kids (if (js2-function-node-p parent)
                  (js2-block-node-kids (js2-function-node-body parent))
                (js2-node-child-list parent)))
        (beg (js2-node-abs-pos (if (js2-function-node-p parent)
                                   (js2-function-node-body parent)
                                 parent)))
        kid result fn
        (continue t))
    (setq fn (if after '>= '<))
    (while (and kids continue)
      (setq kid (car kids))
      (if (funcall fn (+ beg (js2-node-pos kid)) pos)
          (setq result kid
                continue (not after))
        (setq continue after))
      (setq kids (cdr kids)))
    result))

(defun js2-node-find-child-after (pos parent)
  "Find first child that starts after POS in parent.
POS is an absolute buffer position.  PARENT is any node
supported by `js2-node-child-list'.
Returns nil if no applicable child is found."
  (js2-node-find-child-before pos parent 'after))

(defun js2-node-replace-child (pos parent new-node)
  "Replace node at index POS in PARENT with NEW-NODE.
Only works for parents supported by `js2-node-child-list'."
  (let ((kids (js2-node-child-list parent))
        (i 0))
    (while (< i pos)
      (setq kids (cdr kids)
            i (1+ i)))
    (setcar kids new-node)
    (js2-node-add-children parent new-node)))

(defun js2-node-buffer (n)
  "Return the buffer associated with AST N.
Returns nil if the buffer is not set as a property on the root
node, or if parent links were not recorded during parsing."
  (let ((root (js2-node-root n)))
    (and root
         (js2-ast-root-p root)
         (js2-ast-root-buffer root))))

(defun js2-block-node-push (n kid)
  "Push js2-node KID onto the end of js2-block-node N's child list.
KID is always added to the -end- of the kids list.
Function also calls `js2-node-add-children' to add the parent link."
  (let ((kids (js2-node-child-list n)))
    (if kids
        (setcdr kids (nconc (cdr kids) (list kid)))
      (js2-node-set-child-list n (list kid)))
    (js2-node-add-children n kid)))

(defun js2-node-string (node)
  (with-current-buffer (or (js2-node-buffer node)
                           (error "No buffer available for node %s" node))
    (let ((pos (js2-node-abs-pos node)))
      (buffer-substring-no-properties pos (+ pos (js2-node-len node))))))

;; Container for storing the node we're looking for in a traversal.
(js2-deflocal js2-discovered-node nil)

;; Keep track of absolute node position during traversals.
(js2-deflocal js2-visitor-offset nil)

(js2-deflocal js2-node-search-point nil)

(when js2-mode-dev-mode-p
  (defun js2-find-node-at-point ()
    (interactive)
    (let ((node (js2-node-at-point)))
      (message "%s" (or node "No node found at point"))))
  (defun js2-node-name-at-point ()
    (interactive)
    (let ((node (js2-node-at-point)))
      (message "%s" (if node
                        (js2-node-short-name node)
                      "No node found at point.")))))

(defun js2-node-at-point (&optional pos skip-comments)
  "Return AST node at POS, a buffer position, defaulting to current point.
The `js2-mode-ast' variable must be set to the current parse tree.
Signals an error if the AST (`js2-mode-ast') is nil.
Always returns a node - if it can't find one, it returns the root.
If SKIP-COMMENTS is non-nil, comment nodes are ignored."
  (let ((ast js2-mode-ast)
        result)
    (unless ast
      (error "No JavaScript AST available"))
    ;; Look through comments first, since they may be inside nodes that
    ;; would otherwise report a match.
    (setq pos (or pos (point))
          result (if (> pos (js2-node-abs-end ast))
                     ast
                   (if (not skip-comments)
                       (js2-comment-at-point pos))))
    (unless result
      (setq js2-discovered-node nil
            js2-visitor-offset 0
            js2-node-search-point pos)
      (unwind-protect
          (catch 'js2-visit-done
            (js2-visit-ast ast #'js2-node-at-point-visitor))
        (setq js2-visitor-offset nil
              js2-node-search-point nil))
      (setq result js2-discovered-node))
    ;; may have found a comment beyond end of last child node,
    ;; since visiting the ast-root looks at the comment-list last.
    (if (and skip-comments
             (js2-comment-node-p result))
        (setq result nil))
    (or result js2-mode-ast)))

(defun js2-node-at-point-visitor (node end-p)
  (let ((rel-pos (js2-node-pos node))
        abs-pos
        abs-end
        (point js2-node-search-point))
    (cond
     (end-p
      ;; this evaluates to a non-nil return value, even if it's zero
      (decf js2-visitor-offset rel-pos))
     ;; we already looked for comments before visiting, and don't want them now
     ((js2-comment-node-p node)
      nil)
     (t
      (setq abs-pos (incf js2-visitor-offset rel-pos)
            ;; we only want to use the node if the point is before
            ;; the last character position in the node, so we decrement
            ;; the absolute end by 1.
            abs-end (+ abs-pos (js2-node-len node) -1))
      (cond
       ;; If this node starts after search-point, stop the search.
       ((> abs-pos point)
        (throw 'js2-visit-done nil))
       ;; If this node ends before the search-point, don't check kids.
       ((> point abs-end)
        nil)
       (t
        ;; Otherwise point is within this node, possibly in a child.
        (setq js2-discovered-node node)
        t))))))  ; keep processing kids to look for more specific match

(defsubst js2-block-comment-p (node)
  "Return non-nil if NODE is a comment node of format `jsdoc' or `block'."
  (and (js2-comment-node-p node)
       (memq (js2-comment-node-format node) '(jsdoc block))))

;; TODO:  put the comments in a vector and binary-search them instead
(defun js2-comment-at-point (&optional pos)
  "Look through scanned comment nodes for one containing POS.
POS is a buffer position that defaults to current point.
Function returns nil if POS was not in any comment node."
  (let ((ast js2-mode-ast)
        (x (or pos (point)))
        beg end)
    (unless ast
      (error "No JavaScript AST available"))
    (catch 'done
      ;; Comments are stored in lexical order.
      (dolist (comment (js2-ast-root-comments ast) nil)
        (setq beg (js2-node-abs-pos comment)
              end (+ beg (js2-node-len comment)))
        (if (and (>= x beg)
                 (<= x end))
            (throw 'done comment))))))

(defun js2-mode-find-parent-fn (node)
  "Find function enclosing NODE.
Returns nil if NODE is not inside a function."
  (setq node (js2-node-parent node))
  (while (and node (not (js2-function-node-p node)))
    (setq node (js2-node-parent node)))
  (and (js2-function-node-p node) node))

(defun js2-mode-find-enclosing-fn (node)
  "Find function or root enclosing NODE."
  (if (js2-ast-root-p node)
      node
    (setq node (js2-node-parent node))
    (while (not (or (js2-ast-root-p node)
                    (js2-function-node-p node)))
      (setq node (js2-node-parent node)))
    node))

(defun js2-mode-find-enclosing-node (beg end)
  "Find script or function fully enclosing BEG and END."
  (let ((node (js2-node-at-point beg))
        pos
        (continue t))
    (while continue
      (if (or (js2-ast-root-p node)
              (and (js2-function-node-p node)
                   (<= (setq pos (js2-node-abs-pos node)) beg)
                   (>= (+ pos (js2-node-len node)) end)))
          (setq continue nil)
        (setq node (js2-node-parent node))))
    node))

(defun js2-node-parent-script-or-fn (node)
  "Find script or function immediately enclosing NODE.
If NODE is the ast-root, returns nil."
  (if (js2-ast-root-p node)
      nil
    (setq node (js2-node-parent node))
    (while (and node (not (or (js2-function-node-p node)
                              (js2-script-node-p node))))
      (setq node (js2-node-parent node)))
    node))

(defun js2-node-is-descendant (node ancestor)
  "Return t if NODE is a descendant of ANCESTOR."
  (while (and node
              (not (eq node ancestor)))
    (setq node (js2-node-parent node)))
  node)

;;; visitor infrastructure

(defun js2-visit-none (_node _callback)
  "Visitor for AST node that have no node children."
  nil)

(defun js2-print-none (_node _indent)
  "Visitor for AST node with no printed representation.")

(defun js2-print-body (node indent)
  "Print a statement, or a block without braces."
  (if (js2-block-node-p node)
      (dolist (kid (js2-block-node-kids node))
        (js2-print-ast kid indent))
    (js2-print-ast node indent)))

(defun js2-print-list (args &optional delimiter)
  (loop with len = (length args)
        for arg in args
        for count from 1
        do
        (when arg (js2-print-ast arg 0))
        (if (< count len)
            (insert (or delimiter ", ")))))

(defun js2-print-tree (ast)
  "Prints an AST to the current buffer.
Makes `js2-ast-parent-nodes' available to the printer functions."
  (let ((max-lisp-eval-depth (max max-lisp-eval-depth 1500)))
    (js2-print-ast ast)))

(defun js2-print-ast (node &optional indent)
  "Helper function for printing AST nodes.
Requires `js2-ast-parent-nodes' to be non-nil.
You should use `js2-print-tree' instead of this function."
  (let ((printer (get (aref node 0) 'js2-printer))
        (i (or indent 0)))
    ;; TODO:  wedge comments in here somewhere
    (if printer
        (funcall printer node i))))

(defconst js2-side-effecting-tokens
  (let ((tokens (make-bool-vector js2-num-tokens nil)))
    (dolist (tt (list js2-ASSIGN
                      js2-ASSIGN_ADD
                      js2-ASSIGN_BITAND
                      js2-ASSIGN_BITOR
                      js2-ASSIGN_BITXOR
                      js2-ASSIGN_DIV
                      js2-ASSIGN_LSH
                      js2-ASSIGN_MOD
                      js2-ASSIGN_MUL
                      js2-ASSIGN_RSH
                      js2-ASSIGN_SUB
                      js2-ASSIGN_URSH
                      js2-BLOCK
                      js2-BREAK
                      js2-CALL
                      js2-CATCH
                      js2-CATCH_SCOPE
                      js2-CONST
                      js2-CONTINUE
                      js2-DEBUGGER
                      js2-DEC
                      js2-DELPROP
                      js2-DEL_REF
                      js2-DO
                      js2-ELSE
                      js2-EMPTY
                      js2-ENTERWITH
                      js2-EXPORT
                      js2-EXPR_RESULT
                      js2-FINALLY
                      js2-FOR
                      js2-FUNCTION
                      js2-GOTO
                      js2-IF
                      js2-IFEQ
                      js2-IFNE
                      js2-IMPORT
                      js2-INC
                      js2-JSR
                      js2-LABEL
                      js2-LEAVEWITH
                      js2-LET
                      js2-LETEXPR
                      js2-LOCAL_BLOCK
                      js2-LOOP
                      js2-NEW
                      js2-REF_CALL
                      js2-RETHROW
                      js2-RETURN
                      js2-RETURN_RESULT
                      js2-SEMI
                      js2-SETELEM
                      js2-SETELEM_OP
                      js2-SETNAME
                      js2-SETPROP
                      js2-SETPROP_OP
                      js2-SETVAR
                      js2-SET_REF
                      js2-SET_REF_OP
                      js2-SWITCH
                      js2-TARGET
                      js2-THROW
                      js2-TRY
                      js2-VAR
                      js2-WHILE
                      js2-WITH
                      js2-WITHEXPR
                      js2-YIELD))
      (aset tokens tt t))
    (if js2-instanceof-has-side-effects
        (aset tokens js2-INSTANCEOF t))
    tokens))

(defun js2-node-has-side-effects (node)
  "Return t if NODE has side effects."
  (when node  ; makes it easier to handle malformed expressions
    (let ((tt (js2-node-type node)))
      (cond
       ;; This doubtless needs some work, since EXPR_VOID is used
       ;; in several ways in Rhino and I may not have caught them all.
       ;; I'll wait for people to notice incorrect warnings.
       ((and (= tt js2-EXPR_VOID)
             (js2-expr-stmt-node-p node)) ; but not if EXPR_RESULT
        (let ((expr (js2-expr-stmt-node-expr node)))
          (or (js2-node-has-side-effects expr)
              (when (js2-string-node-p expr)
                (member (js2-string-node-value expr) '("use strict" "use asm"))))))
       ((= tt js2-COMMA)
        (js2-node-has-side-effects (js2-infix-node-right node)))
       ((or (= tt js2-AND)
            (= tt js2-OR))
        (or (js2-node-has-side-effects (js2-infix-node-right node))
            (js2-node-has-side-effects (js2-infix-node-left node))))
       ((= tt js2-HOOK)
        (and (js2-node-has-side-effects (js2-cond-node-true-expr node))
             (js2-node-has-side-effects (js2-cond-node-false-expr node))))
       ((js2-paren-node-p node)
        (js2-node-has-side-effects (js2-paren-node-expr node)))
       ((= tt js2-ERROR) ; avoid cascaded error messages
        nil)
       (t
        (aref js2-side-effecting-tokens tt))))))

(defconst js2-stmt-node-types
  (list js2-BLOCK
        js2-BREAK
        js2-CONTINUE
        js2-DEFAULT  ; e4x "default xml namespace" statement
        js2-DO
        js2-EXPR_RESULT
        js2-EXPR_VOID
        js2-FOR
        js2-IF
        js2-RETURN
        js2-SWITCH
        js2-THROW
        js2-TRY
        js2-WHILE
        js2-WITH)
  "Node types that only appear in statement contexts.
The list does not include nodes that always appear as the child
of another specific statement type, such as switch-cases,
catch and finally blocks, and else-clauses.  The list also excludes
nodes like yield, let and var, which may appear in either expression
or statement context, and in the latter context always have a
`js2-expr-stmt-node' parent.  Finally, the list does not include
functions or scripts, which are treated separately from statements
by the JavaScript parser and runtime.")

(defun js2-stmt-node-p (node)
  "Heuristic for figuring out if NODE is a statement.
Some node types can appear in either an expression context or a
statement context, e.g. let-nodes, yield-nodes, and var-decl nodes.
For these node types in a statement context, the parent will be a
`js2-expr-stmt-node'.
Functions aren't included in the check."
  (memq (js2-node-type node) js2-stmt-node-types))

(defun js2-mode-find-first-stmt (node)
  "Search upward starting from NODE looking for a statement.
For purposes of this function, a `js2-function-node' counts."
  (while (not (or (js2-stmt-node-p node)
                  (js2-function-node-p node)))
    (setq node (js2-node-parent node)))
  node)

(defun js2-node-parent-stmt (node)
  "Return the node's first ancestor that is a statement.
Returns nil if NODE is a `js2-ast-root'.  Note that any expression
appearing in a statement context will have a parent that is a
`js2-expr-stmt-node' that will be returned by this function."
  (let ((parent (js2-node-parent node)))
    (if (or (null parent)
            (js2-stmt-node-p parent)
            (and (js2-function-node-p parent)
                 (not (eq (js2-function-node-form parent)
                          'FUNCTION_EXPRESSION))))
        parent
      (js2-node-parent-stmt parent))))

;; In the Mozilla Rhino sources, Roshan James writes:
;;  Does consistent-return analysis on the function body when strict mode is
;;  enabled.
;;
;;    function (x) { return (x+1) }
;;
;;  is ok, but
;;
;;    function (x) { if (x < 0) return (x+1); }
;;
;;  is not because the function can potentially return a value when the
;;  condition is satisfied and if not, the function does not explicitly
;;  return a value.
;;
;;  This extends to checking mismatches such as "return" and "return <value>"
;;  used in the same function. Warnings are not emitted if inconsistent
;;  returns exist in code that can be statically shown to be unreachable.
;;  Ex.
;;    function (x) { while (true) { ... if (..) { return value } ... } }
;;
;;  emits no warning. However if the loop had a break statement, then a
;;  warning would be emitted.
;;
;;  The consistency analysis looks at control structures such as loops, ifs,
;;  switch, try-catch-finally blocks, examines the reachable code paths and
;;  warns the user about an inconsistent set of termination possibilities.
;;
;;  These flags enumerate the possible ways a statement/function can
;;  terminate. These flags are used by endCheck() and by the Parser to
;;  detect inconsistent return usage.
;;
;;  END_UNREACHED is reserved for code paths that are assumed to always be
;;  able to execute (example: throw, continue)
;;
;;  END_DROPS_OFF indicates if the statement can transfer control to the
;;  next one. Statement such as return dont. A compound statement may have
;;  some branch that drops off control to the next statement.
;;
;;  END_RETURNS indicates that the statement can return with no value.
;;  END_RETURNS_VALUE indicates that the statement can return a value.
;;
;;  A compound statement such as
;;  if (condition) {
;;    return value;
;;  }
;;  Will be detected as (END_DROPS_OFF | END_RETURN_VALUE) by endCheck()

(defconst js2-END_UNREACHED 0)
(defconst js2-END_DROPS_OFF 1)
(defconst js2-END_RETURNS 2)
(defconst js2-END_RETURNS_VALUE 4)
(defconst js2-END_YIELDS 8)

(defun js2-has-consistent-return-usage (node)
  "Check that every return usage in a function body is consistent.
Returns t if the function satisfies strict mode requirement."
  (let ((n (js2-end-check node)))
    ;; either it doesn't return a value in any branch...
    (or (js2-flag-not-set-p n js2-END_RETURNS_VALUE)
        ;; or it returns a value (or is unreached) at every branch
        (js2-flag-not-set-p n (logior js2-END_DROPS_OFF
                                      js2-END_RETURNS
                                      js2-END_YIELDS)))))

(defun js2-end-check-if (node)
  "Ensure that return usage in then/else blocks is consistent.
If there is no else block, then the return statement can fall through.
Returns logical OR of END_* flags"
  (let ((th (js2-if-node-then-part node))
        (el (js2-if-node-else-part node)))
    (if (null th)
        js2-END_UNREACHED
      (logior (js2-end-check th) (if el
                                     (js2-end-check el)
                                   js2-END_DROPS_OFF)))))

(defun js2-end-check-switch (node)
  "Consistency of return statements is checked between the case statements.
If there is no default, then the switch can fall through. If there is a
default, we check to see if all code paths in the default return or if
there is a code path that can fall through.
Returns logical OR of END_* flags."
  (let ((rv js2-END_UNREACHED)
        default-case)
    ;; examine the cases
    (catch 'break
      (dolist (c (js2-switch-node-cases node))
        (if (js2-case-node-expr c)
            (js2-set-flag rv (js2-end-check-block c))
          (setq default-case c)
          (throw 'break nil))))
    ;; we don't care how the cases drop into each other
    (js2-clear-flag rv js2-END_DROPS_OFF)
    ;; examine the default
    (js2-set-flag rv (if default-case
                         (js2-end-check default-case)
                       js2-END_DROPS_OFF))
    rv))

(defun js2-end-check-try (node)
 "If the block has a finally, return consistency is checked in the
finally block. If all code paths in the finally return, then the
returns in the try-catch blocks don't matter. If there is a code path
that does not return or if there is no finally block, the returns
of the try and catch blocks are checked for mismatch.
Returns logical OR of END_* flags."
 (let ((finally (js2-try-node-finally-block node))
       rv)
   ;; check the finally if it exists
   (setq rv (if finally
                (js2-end-check (js2-finally-node-body finally))
              js2-END_DROPS_OFF))
   ;; If the finally block always returns, then none of the returns
   ;; in the try or catch blocks matter.
   (when (js2-flag-set-p rv js2-END_DROPS_OFF)
     (js2-clear-flag rv js2-END_DROPS_OFF)
     ;; examine the try block
     (js2-set-flag rv (js2-end-check (js2-try-node-try-block node)))
     ;; check each catch block
     (dolist (cb (js2-try-node-catch-clauses node))
       (js2-set-flag rv (js2-end-check (js2-catch-node-block cb)))))
   rv))

(defun js2-end-check-loop (node)
  "Return statement in the loop body must be consistent.
The default assumption for any kind of a loop is that it will eventually
terminate.  The only exception is a loop with a constant true condition.
Code that follows such a loop is examined only if one can determine
statically that there is a break out of the loop.

    for(... ; ... ; ...) {}
    for(... in ... ) {}
    while(...) { }
    do { } while(...)

Returns logical OR of END_* flags."
  (let ((rv (js2-end-check (js2-loop-node-body node)))
        (condition (cond
                    ((js2-while-node-p node)
                     (js2-while-node-condition node))
                     ((js2-do-node-p node)
                      (js2-do-node-condition node))
                     ((js2-for-node-p node)
                      (js2-for-node-condition node)))))

    ;; check to see if the loop condition is always true
    (if (and condition
             (eq (js2-always-defined-boolean-p condition) 'ALWAYS_TRUE))
        (js2-clear-flag rv js2-END_DROPS_OFF))

    ;; look for effect of breaks
    (js2-set-flag rv (js2-node-get-prop node
                                        'CONTROL_BLOCK_PROP
                                        js2-END_UNREACHED))
    rv))

(defun js2-end-check-block (node)
  "A general block of code is examined statement by statement.
If any statement (even a compound one) returns in all branches, then
subsequent statements are not examined.
Returns logical OR of END_* flags."
  (let* ((rv js2-END_DROPS_OFF)
         (kids (js2-block-node-kids node))
         (n (car kids)))
    ;; Check each statment.  If the statement can continue onto the next
    ;; one (i.e. END_DROPS_OFF is set), then check the next statement.
    (while (and n (js2-flag-set-p rv js2-END_DROPS_OFF))
      (js2-clear-flag rv js2-END_DROPS_OFF)
      (js2-set-flag rv (js2-end-check n))
      (setq kids (cdr kids)
            n (car kids)))
    rv))

(defun js2-end-check-label (node)
  "A labeled statement implies that there may be a break to the label.
The function processes the labeled statement and then checks the
CONTROL_BLOCK_PROP property to see if there is ever a break to the
particular label.
Returns logical OR of END_* flags."
  (let ((rv (js2-end-check (js2-labeled-stmt-node-stmt node))))
    (logior rv (js2-node-get-prop node
                                  'CONTROL_BLOCK_PROP
                                  js2-END_UNREACHED))))

(defun js2-end-check-break (node)
  "When a break is encountered annotate the statement being broken
out of by setting its CONTROL_BLOCK_PROP property.
Returns logical OR of END_* flags."
  (and (js2-break-node-target node)
       (js2-node-set-prop (js2-break-node-target node)
                          'CONTROL_BLOCK_PROP
                          js2-END_DROPS_OFF))
  js2-END_UNREACHED)

(defun js2-end-check (node)
  "Examine the body of a function, doing a basic reachability analysis.
Returns a combination of flags END_* flags that indicate
how the function execution can terminate. These constitute only the
pessimistic set of termination conditions. It is possible that at
runtime certain code paths will never be actually taken. Hence this
analysis will flag errors in cases where there may not be errors.
Returns logical OR of END_* flags"
  (let (kid)
    (cond
     ((js2-break-node-p node)
      (js2-end-check-break node))
     ((js2-expr-stmt-node-p node)
      (if (setq kid (js2-expr-stmt-node-expr node))
          (js2-end-check kid)
        js2-END_DROPS_OFF))
     ((or (js2-continue-node-p node)
          (js2-throw-node-p node))
      js2-END_UNREACHED)
     ((js2-return-node-p node)
      (if (setq kid (js2-return-node-retval node))
          js2-END_RETURNS_VALUE
        js2-END_RETURNS))
     ((js2-loop-node-p node)
      (js2-end-check-loop node))
     ((js2-switch-node-p node)
      (js2-end-check-switch node))
     ((js2-labeled-stmt-node-p node)
      (js2-end-check-label node))
     ((js2-if-node-p node)
      (js2-end-check-if node))
     ((js2-try-node-p node)
      (js2-end-check-try node))
     ((js2-block-node-p node)
      (if (null (js2-block-node-kids node))
          js2-END_DROPS_OFF
        (js2-end-check-block node)))
     ((js2-yield-node-p node)
      js2-END_YIELDS)
     (t
      js2-END_DROPS_OFF))))

(defun js2-always-defined-boolean-p (node)
  "Check if NODE always evaluates to true or false in boolean context.
Returns 'ALWAYS_TRUE, 'ALWAYS_FALSE, or nil if it's neither always true
nor always false."
  (let ((tt (js2-node-type node))
        num)
    (cond
     ((or (= tt js2-FALSE) (= tt js2-NULL))
      'ALWAYS_FALSE)
     ((= tt js2-TRUE)
      'ALWAYS_TRUE)
     ((= tt js2-NUMBER)
      (setq num (js2-number-node-num-value node))
      (if (and (not (eq num 0.0e+NaN))
               (not (zerop num)))
          'ALWAYS_TRUE
        'ALWAYS_FALSE))
     (t
      nil))))

;;; Scanner -- a port of Mozilla Rhino's lexer.
;; Corresponds to Rhino files Token.java and TokenStream.java.

(defvar js2-tokens nil
  "List of all defined token names.")  ; initialized in `js2-token-names'

(defconst js2-token-names
  (let* ((names (make-vector js2-num-tokens -1))
         (case-fold-search nil)  ; only match js2-UPPER_CASE
         (syms (apropos-internal "^js2-\\(?:[[:upper:]_]+\\)")))
    (loop for sym in syms
          for i from 0
          do
          (unless (or (memq sym '(js2-EOF_CHAR js2-ERROR))
                      (not (boundp sym)))
            (aset names (symbol-value sym)           ; code, e.g. 152
                  (downcase
                   (substring (symbol-name sym) 4))) ; name, e.g. "let"
            (push sym js2-tokens)))
    names)
  "Vector mapping int values to token string names, sans `js2-' prefix.")

(defun js2-tt-name (tok)
  "Return a string name for TOK, a token symbol or code.
Signals an error if it's not a recognized token."
  (let ((code tok))
    (if (symbolp tok)
        (setq code (symbol-value tok)))
    (if (eq code -1)
        "ERROR"
      (if (and (numberp code)
               (not (minusp code))
               (< code js2-num-tokens))
          (aref js2-token-names code)
        (error "Invalid token: %s" code)))))

(defsubst js2-tt-sym (tok)
  "Return symbol for TOK given its code, e.g. 'js2-LP for code 86."
  (intern (js2-tt-name tok)))

(defconst js2-token-codes
  (let ((table (make-hash-table :test 'eq :size 256)))
    (loop for name across js2-token-names
          for sym = (intern (concat "js2-" (upcase name)))
          do
          (puthash sym (symbol-value sym) table))
    ;; clean up a few that are "wrong" in Rhino's token codes
    (puthash 'js2-DELETE js2-DELPROP table)
    table)
  "Hashtable mapping token type symbols to their bytecodes.")

(defsubst js2-tt-code (sym)
  "Return code for token symbol SYM, e.g. 86 for 'js2-LP."
  (or (gethash sym js2-token-codes)
      (error "Invalid token symbol: %s " sym)))  ; signal code bug

(defun js2-report-scan-error (msg &optional no-throw beg len)
  (setf (js2-token-end (js2-current-token)) js2-ts-cursor)
  (js2-report-error msg nil
                    (or beg (js2-current-token-beg))
                    (or len (js2-current-token-len)))
  (unless no-throw
    (throw 'return js2-ERROR)))

(defun js2-set-string-from-buffer (token)
  "Set `string' and `end' slots for TOKEN, return the string."
  (setf (js2-token-end token) js2-ts-cursor
        (js2-token-string token) (js2-collect-string js2-ts-string-buffer)))

;; TODO:  could potentially avoid a lot of consing by allocating a
;; char buffer the way Rhino does.
(defsubst js2-add-to-string (c)
  (push c js2-ts-string-buffer))

;; Note that when we "read" the end-of-file, we advance js2-ts-cursor
;; to (1+ (point-max)), which lets the scanner treat end-of-file like
;; any other character:  when it's not part of the current token, we
;; unget it, allowing it to be read again by the following call.
(defsubst js2-unget-char ()
  (decf js2-ts-cursor))

;; Rhino distinguishes \r and \n line endings.  We don't need to
;; because we only scan from Emacs buffers, which always use \n.
(defun js2-get-char ()
  "Read and return the next character from the input buffer.
Increments `js2-ts-lineno' if the return value is a newline char.
Updates `js2-ts-cursor' to the point after the returned char.
Returns `js2-EOF_CHAR' if we hit the end of the buffer.
Also updates `js2-ts-hit-eof' and `js2-ts-line-start' as needed."
  (let (c)
    ;; check for end of buffer
    (if (>= js2-ts-cursor (point-max))
        (setq js2-ts-hit-eof t
              js2-ts-cursor (1+ js2-ts-cursor)
              c js2-EOF_CHAR)  ; return value
      ;; otherwise read next char
      (setq c (char-before (incf js2-ts-cursor)))
      ;; if we read a newline, update counters
      (if (= c ?\n)
          (setq js2-ts-line-start js2-ts-cursor
                js2-ts-lineno (1+ js2-ts-lineno)))
      ;; TODO:  skip over format characters
      c)))

(defun js2-read-unicode-escape ()
  "Read a \\uNNNN sequence from the input.
Assumes the ?\ and ?u have already been read.
Returns the unicode character, or nil if it wasn't a valid character.
Doesn't change the values of any scanner variables."
  ;; I really wish I knew a better way to do this, but I can't
  ;; find the Emacs function that takes a 16-bit int and converts
  ;; it to a Unicode/utf-8 character.  So I basically eval it with (read).
  ;; Have to first check that it's 4 hex characters or it may stop
  ;; the read early.
  (ignore-errors
    (let ((s (buffer-substring-no-properties js2-ts-cursor
                                             (+ 4 js2-ts-cursor))))
      (if (string-match "[[:alnum:]]\\{4\\}" s)
          (read (concat "?\\u" s))))))

(defun js2-match-char (test)
  "Consume and return next character if it matches TEST, a character.
Returns nil and consumes nothing if TEST is not the next character."
  (let ((c (js2-get-char)))
    (if (eq c test)
        t
      (js2-unget-char)
      nil)))

(defun js2-peek-char ()
  (prog1
      (js2-get-char)
    (js2-unget-char)))

(defun js2-java-identifier-start-p (c)
  (or
   (memq c '(?$ ?_))
   (js2-char-uppercase-p c)
   (js2-char-lowercase-p c)))

(defun js2-java-identifier-part-p (c)
  "Implementation of java.lang.Character.isJavaIdentifierPart()."
  ;; TODO:  make me Unicode-friendly.  See comments above.
  (or
   (memq c '(?$ ?_))
   (js2-char-uppercase-p c)
   (js2-char-lowercase-p c)
   (and (>= c ?0) (<= c ?9))))

(defun js2-alpha-p (c)
  (cond ((and (<= ?A c) (<= c ?Z)) t)
        ((and (<= ?a c) (<= c ?z)) t)
        (t nil)))

(defsubst js2-digit-p (c)
  (and (<= ?0 c) (<= c ?9)))

(defun js2-js-space-p (c)
  (if (<= c 127)
      (memq c '(#x20 #x9 #xB #xC #xD))
    (or
     (eq c #xA0)
     ;; TODO:  change this nil to check for Unicode space character
     nil)))

(defconst js2-eol-chars (list js2-EOF_CHAR ?\n ?\r))

(defun js2-skip-line ()
  "Skip to end of line."
  (while (not (memq (js2-get-char) js2-eol-chars)))
  (js2-unget-char)
  (setf (js2-token-end (js2-current-token)) js2-ts-cursor)
  (setq js2-token-end js2-ts-cursor))

(defun js2-init-scanner (&optional buf line)
  "Create token stream for BUF starting on LINE.
BUF defaults to `current-buffer' and LINE defaults to 1.

A buffer can only have one scanner active at a time, which yields
dramatically simpler code than using a defstruct.  If you need to
have simultaneous scanners in a buffer, copy the regions to scan
into temp buffers."
  (with-current-buffer (or buf (current-buffer))
    (setq js2-ts-dirty-line nil
          js2-ts-hit-eof nil
          js2-ts-line-start 0
          js2-ts-lineno (or line 1)
          js2-ts-line-end-char -1
          js2-ts-cursor (point-min)
          js2-ti-tokens (make-vector js2-ti-ntokens nil)
          js2-ti-tokens-cursor 0
          js2-ti-lookahead 0
          js2-ts-is-xml-attribute nil
          js2-ts-xml-is-tag-content nil
          js2-ts-xml-open-tags-count 0
          js2-ts-string-buffer nil)))

;; This function uses the cached op, string and number fields in
;; TokenStream; if getToken has been called since the passed token
;; was scanned, the op or string printed may be incorrect.
(defun js2-token-to-string (token)
  ;; Not sure where this function is used in Rhino.  Not tested.
  (if (not js2-debug-print-trees)
      ""
    (let ((name (js2-tt-name token)))
      (cond
       ((memq token (list js2-STRING js2-REGEXP js2-NAME))
        (concat name " `" (js2-current-token-string) "'"))
       ((eq token js2-NUMBER)
        (format "NUMBER %g" (js2-token-number (js2-current-token))))
       (t
        name)))))

(defconst js2-keywords
  '(break
    case catch const continue
    debugger default delete do
    else enum
    false finally for function
    if in instanceof import
    let
    new null
    return
    switch
    this throw true try typeof
    var void
    while with
    yield))

;; Token names aren't exactly the same as the keywords, unfortunately.
;; E.g. enum isn't in the tokens, and delete is js2-DELPROP.
(defconst js2-kwd-tokens
  (let ((table (make-vector js2-num-tokens nil))
        (tokens
         (list js2-BREAK
               js2-CASE js2-CATCH js2-CONST js2-CONTINUE
               js2-DEBUGGER js2-DEFAULT js2-DELPROP js2-DO
               js2-ELSE
               js2-FALSE js2-FINALLY js2-FOR js2-FUNCTION
               js2-IF js2-IN js2-INSTANCEOF js2-IMPORT
               js2-LET
               js2-NEW js2-NULL
               js2-RETURN
               js2-SWITCH
               js2-THIS js2-THROW js2-TRUE js2-TRY js2-TYPEOF
               js2-VAR
               js2-WHILE js2-WITH
               js2-YIELD)))
    (dolist (i tokens)
      (aset table i 'font-lock-keyword-face))
    (aset table js2-STRING 'font-lock-string-face)
    (aset table js2-REGEXP 'font-lock-string-face)
    (aset table js2-COMMENT 'font-lock-comment-face)
    (aset table js2-THIS 'font-lock-builtin-face)
    (aset table js2-VOID 'font-lock-constant-face)
    (aset table js2-NULL 'font-lock-constant-face)
    (aset table js2-TRUE 'font-lock-constant-face)
    (aset table js2-FALSE 'font-lock-constant-face)
    table)
  "Vector whose values are non-nil for tokens that are keywords.
The values are default faces to use for highlighting the keywords.")

(defconst js2-reserved-words
  '(abstract
    boolean byte
    char class
    double
    enum export extends
    final float
    goto
    implements import int interface
    long
    native
    package private protected public
    short static super synchronized
    throws transient
    volatile))

(defconst js2-keyword-names
  (let ((table (make-hash-table :test 'equal)))
    (loop for k in js2-keywords
          do (puthash
              (symbol-name k)                            ; instanceof
              (intern (concat "js2-"
                              (upcase (symbol-name k)))) ; js2-INSTANCEOF
              table))
    table)
  "JavaScript keywords by name, mapped to their symbols.")

(defconst js2-reserved-word-names
  (let ((table (make-hash-table :test 'equal)))
    (loop for k in js2-reserved-words
          do
          (puthash (symbol-name k) 'js2-RESERVED table))
    table)
  "JavaScript reserved words by name, mapped to 'js2-RESERVED.")

(defun js2-collect-string (buf)
  "Convert BUF, a list of chars, to a string.
Reverses BUF before converting."
  (if buf
      (apply #'string (nreverse buf))
    ""))

(defun js2-string-to-keyword (s)
  "Return token for S, a string, if S is a keyword or reserved word.
Returns a symbol such as 'js2-BREAK, or nil if not keyword/reserved."
  (or (gethash s js2-keyword-names)
      (gethash s js2-reserved-word-names)))

(defsubst js2-ts-set-char-token-bounds (token)
  "Used when next token is one character."
  (setf (js2-token-beg token) (1- js2-ts-cursor)
        (js2-token-end token) js2-ts-cursor))

(defsubst js2-ts-return (token type)
  "Update the `end' and `type' slots of TOKEN,
then throw `return' with value TYPE."
  (setf (js2-token-end token) js2-ts-cursor
        (js2-token-type token) type)
  (throw 'return type))

(defun js2-x-digit-to-int (c accumulator)
  "Build up a hex number.
If C is a hexadecimal digit, return ACCUMULATOR * 16 plus
corresponding number.  Otherwise return -1."
  (catch 'return
    (catch 'check
      ;; Use 0..9 < A..Z < a..z
      (cond
       ((<= c ?9)
        (decf c ?0)
        (if (<= 0 c)
            (throw 'check nil)))
       ((<= c ?F)
        (when (<= ?A c)
          (decf c (- ?A 10))
          (throw 'check nil)))
       ((<= c ?f)
        (when (<= ?a c)
          (decf c (- ?a 10))
          (throw 'check nil))))
      (throw 'return -1))
    (logior c (lsh accumulator 4))))

(defun js2-get-token ()
  "If `js2-ti-lookahead' is zero, call scanner to get new token.
Otherwise, move `js2-ti-tokens-cursor' and return the type of
next saved token.

This function will not return a newline (js2-EOL) - instead, it
gobbles newlines until it finds a non-newline token.  Call
`js2-peek-token-or-eol' when you care about newlines.

This function will also not return a js2-COMMENT.  Instead, it
records comments found in `js2-scanned-comments'.  If the token
returned by this function immediately follows a jsdoc comment,
the token is flagged as such."
  (if (zerop js2-ti-lookahead)
      (js2-get-token-internal)
    (decf js2-ti-lookahead)
    (setq js2-ti-tokens-cursor (mod (1+ js2-ti-tokens-cursor) js2-ti-ntokens))
    (let ((tt (js2-current-token-type)))
      (assert (not (= tt js2-EOL)))
      tt)))

(defun js2-unget-token ()
  (assert (< js2-ti-lookahead js2-ti-max-lookahead))
  (incf js2-ti-lookahead)
  (setq js2-ti-tokens-cursor (mod (1- js2-ti-tokens-cursor) js2-ti-ntokens)))

(defun js2-get-token-internal ()
  (let* ((token (js2-get-token-internal-1)) ; call scanner
         (tt (js2-token-type token))
         saw-eol
         face)
    ;; process comments
    (while (or (= tt js2-EOL) (= tt js2-COMMENT))
      (if (= tt js2-EOL)
          (setq saw-eol t)
        (setq saw-eol nil)
        (when js2-record-comments
          (js2-record-comment token)))
      (setq js2-ti-tokens-cursor (mod (1- js2-ti-tokens-cursor) js2-ti-ntokens))
      (setq token (js2-get-token-internal-1) ; call scanner again
            tt (js2-token-type token)))

    (when saw-eol
      (setf (js2-token-follows-eol-p token) t))

    ;; perform lexical fontification as soon as token is scanned
    (when js2-parse-ide-mode
      (cond
       ((minusp tt)
        (js2-record-face 'js2-error token))
       ((setq face (aref js2-kwd-tokens tt))
        (js2-record-face face token))
       ((and (= tt js2-NAME)
             (equal (js2-token-string token) "undefined"))
        (js2-record-face 'font-lock-constant-face token))))
    tt))

(defun js2-get-token-internal-1 ()
  "Return next JavaScript token type, an int such as js2-RETURN.
During operation, creates an instance of `js2-token' struct, sets
its relevant fields and puts it into `js2-ti-tokens'."
  (let (c c1 identifier-start is-unicode-escape-start
        contains-escape escape-val str result base
        is-integer quote-char val look-for-slash continue tt
        (token (js2-new-token 0)))
   (setq
    tt
    (catch 'return
      (while t
        ;; Eat whitespace, possibly sensitive to newlines.
        (setq continue t)
        (while continue
          (setq c (js2-get-char))
          (cond
           ((eq c js2-EOF_CHAR)
            (js2-unget-char)
            (js2-ts-set-char-token-bounds token)
            (throw 'return js2-EOF))
           ((eq c ?\n)
            (js2-ts-set-char-token-bounds token)
            (setq js2-ts-dirty-line nil)
            (throw 'return js2-EOL))
           ((not (js2-js-space-p c))
            (if (/= c ?-)               ; in case end of HTML comment
                (setq js2-ts-dirty-line t))
            (setq continue nil))))
        ;; Assume the token will be 1 char - fixed up below.
        (js2-ts-set-char-token-bounds token)
        (when (eq c ?@)
          (throw 'return js2-XMLATTR))
        ;; identifier/keyword/instanceof?
        ;; watch out for starting with a <backslash>
        (cond
         ((eq c ?\\)
          (setq c (js2-get-char))
          (if (eq c ?u)
              (setq identifier-start t
                    is-unicode-escape-start t
                    js2-ts-string-buffer nil)
            (setq identifier-start nil)
            (js2-unget-char)
            (setq c ?\\)))
         (t
          (when (setq identifier-start (js2-java-identifier-start-p c))
            (setq js2-ts-string-buffer nil)
            (js2-add-to-string c))))
        (when identifier-start
          (setq contains-escape is-unicode-escape-start)
          (catch 'break
            (while t
              (if is-unicode-escape-start
                  ;; strictly speaking we should probably push-back
                  ;; all the bad characters if the <backslash>uXXXX
                  ;; sequence is malformed. But since there isn't a
                  ;; correct context(is there?) for a bad Unicode
                  ;; escape sequence in an identifier, we can report
                  ;; an error here.
                  (progn
                    (setq escape-val 0)
                    (dotimes (_ 4)
                      (setq c (js2-get-char)
                            escape-val (js2-x-digit-to-int c escape-val))
                      ;; Next check takes care of c < 0 and bad escape
                      (if (minusp escape-val)
                          (throw 'break nil)))
                    (if (minusp escape-val)
                        (js2-report-scan-error "msg.invalid.escape" t))
                    (js2-add-to-string escape-val)
                    (setq is-unicode-escape-start nil))
                (setq c (js2-get-char))
                (cond
                 ((eq c ?\\)
                  (setq c (js2-get-char))
                  (if (eq c ?u)
                      (setq is-unicode-escape-start t
                            contains-escape t)
                    (js2-report-scan-error "msg.illegal.character" t)))
                 (t
                  (if (or (eq c js2-EOF_CHAR)
                          (not (js2-java-identifier-part-p c)))
                      (throw 'break nil))
                  (js2-add-to-string c))))))
          (js2-unget-char)
          (setf str (js2-collect-string js2-ts-string-buffer)
                (js2-token-end token) js2-ts-cursor)
          (unless contains-escape
            ;; OPT we shouldn't have to make a string (object!) to
            ;; check if it's a keyword.
            ;; Return the corresponding token if it's a keyword
            (when (setq result (js2-string-to-keyword str))
              (if (and (< js2-language-version 170)
                       (memq result '(js2-LET js2-YIELD)))
                  ;; LET and YIELD are tokens only in 1.7 and later
                  (setq result 'js2-NAME))
              (if (not (eq result 'js2-RESERVED))
                  (throw 'return (js2-tt-code result)))
              (js2-report-warning "msg.reserved.keyword" str)))
          ;; If we want to intern these as Rhino does, just use (intern str)
          (setf (js2-token-string token) str)
          (throw 'return js2-NAME))     ; end identifier/kwd check
        ;; is it a number?
        (when (or (js2-digit-p c)
                  (and (eq c ?.) (js2-digit-p (js2-peek-char))))
          (setq js2-ts-string-buffer nil
                base 10)
          (when (eq c ?0)
            (setq c (js2-get-char))
            (cond
             ((or (eq c ?x) (eq c ?X))
              (setq base 16)
              (setq c (js2-get-char)))
             ((js2-digit-p c)
              (setq base 8))
             (t
              (js2-add-to-string ?0))))
          (if (eq base 16)
              (while (<= 0 (js2-x-digit-to-int c 0))
                (js2-add-to-string c)
                (setq c (js2-get-char)))
            (while (and (<= ?0 c) (<= c ?9))
              ;; We permit 08 and 09 as decimal numbers, which
              ;; makes our behavior a superset of the ECMA
              ;; numeric grammar.  We might not always be so
              ;; permissive, so we warn about it.
              (when (and (eq base 8) (>= c ?8))
                (js2-report-warning "msg.bad.octal.literal"
                                    (if (eq c ?8) "8" "9"))
                (setq base 10))
              (js2-add-to-string c)
              (setq c (js2-get-char))))
          (setq is-integer t)
          (when (and (eq base 10) (memq c '(?. ?e ?E)))
            (setq is-integer nil)
            (when (eq c ?.)
              (loop do
                    (js2-add-to-string c)
                    (setq c (js2-get-char))
                    while (js2-digit-p c)))
            (when (memq c '(?e ?E))
              (js2-add-to-string c)
              (setq c (js2-get-char))
              (when (memq c '(?+ ?-))
                (js2-add-to-string c)
                (setq c (js2-get-char)))
              (unless (js2-digit-p c)
                (js2-report-scan-error "msg.missing.exponent" t))
              (loop do
                    (js2-add-to-string c)
                    (setq c (js2-get-char))
                    while (js2-digit-p c))))
          (js2-unget-char)
          (let ((str (js2-set-string-from-buffer token)))
            (setf (js2-token-number token)
                  (if (and (eq base 10) (not is-integer))
                      (string-to-number str)
                    ;; TODO:  call runtime number-parser.  Some of it is in
                    ;; js2-util.el, but I need to port ScriptRuntime.stringToNumber.
                    (string-to-number str))))
          (throw 'return js2-NUMBER))
        ;; is it a string?
        (when (memq c '(?\" ?\'))
          ;; We attempt to accumulate a string the fast way, by
          ;; building it directly out of the reader.  But if there
          ;; are any escaped characters in the string, we revert to
          ;; building it out of a string buffer.
          (setq quote-char c
                js2-ts-string-buffer nil
                c (js2-get-char))
          (catch 'break
            (while (/= c quote-char)
              (catch 'continue
                (when (or (eq c ?\n) (eq c js2-EOF_CHAR))
                  (js2-unget-char)
                  (setf (js2-token-end token) js2-ts-cursor)
                  (js2-report-error "msg.unterminated.string.lit")
                  (throw 'return js2-STRING))
                (when (eq c ?\\)
                  ;; We've hit an escaped character
                  (setq c (js2-get-char))
                  (case c
                    (?b (setq c ?\b))
                    (?f (setq c ?\f))
                    (?n (setq c ?\n))
                    (?r (setq c ?\r))
                    (?t (setq c ?\t))
                    (?v (setq c ?\v))
                    (?u
                     (setq c1 (js2-read-unicode-escape))
                     (if js2-parse-ide-mode
                         (if c1
                             (progn
                               ;; just copy the string in IDE-mode
                               (js2-add-to-string ?\\)
                               (js2-add-to-string ?u)
                               (dotimes (_ 3)
                                 (js2-add-to-string (js2-get-char)))
                               (setq c (js2-get-char))) ; added at end of loop
                           ;; flag it as an invalid escape
                           (js2-report-warning "msg.invalid.escape"
                                               nil (- js2-ts-cursor 2) 6))
                       ;; Get 4 hex digits; if the u escape is not
                       ;; followed by 4 hex digits, use 'u' + the
                       ;; literal character sequence that follows.
                       (js2-add-to-string ?u)
                       (setq escape-val 0)
                       (dotimes (_ 4)
                         (setq c (js2-get-char)
                               escape-val (js2-x-digit-to-int c escape-val))
                         (if (minusp escape-val)
                             (throw 'continue nil))
                         (js2-add-to-string c))
                       ;; prepare for replace of stored 'u' sequence by escape value
                       (setq js2-ts-string-buffer (nthcdr 5 js2-ts-string-buffer)
                             c escape-val)))
                    (?x
                     ;; Get 2 hex digits, defaulting to 'x'+literal
                     ;; sequence, as above.
                     (setq c (js2-get-char)
                           escape-val (js2-x-digit-to-int c 0))
                     (if (minusp escape-val)
                         (progn
                           (js2-add-to-string ?x)
                           (throw 'continue nil))
                       (setq c1 c
                             c (js2-get-char)
                             escape-val (js2-x-digit-to-int c escape-val))
                       (if (minusp escape-val)
                           (progn
                             (js2-add-to-string ?x)
                             (js2-add-to-string c1)
                             (throw 'continue nil))
                         ;; got 2 hex digits
                         (setq c escape-val))))
                    (?\n
                     ;; Remove line terminator after escape to follow
                     ;; SpiderMonkey and C/C++
                     (setq c (js2-get-char))
                     (throw 'continue nil))
                    (t
                     (when (and (<= ?0 c) (< c ?8))
                       (setq val (- c ?0)
                             c (js2-get-char))
                       (when (and (<= ?0 c) (< c ?8))
                         (setq val (- (+ (* 8 val) c) ?0)
                               c (js2-get-char))
                         (when (and (<= ?0 c)
                                    (< c ?8)
                                    (< val #o37))
                           ;; c is 3rd char of octal sequence only
                           ;; if the resulting val <= 0377
                           (setq val (- (+ (* 8 val) c) ?0)
                                 c (js2-get-char))))
                       (js2-unget-char)
                       (setq c val)))))
                (js2-add-to-string c)
                (setq c (js2-get-char)))))
          (js2-set-string-from-buffer token)
          (throw 'return js2-STRING))
        (js2-ts-return token
         (case c
          (?\;
           (throw 'return js2-SEMI))
          (?\[
           (throw 'return js2-LB))
          (?\]
           (throw 'return js2-RB))
          (?{
           (throw 'return js2-LC))
          (?}
           (throw 'return js2-RC))
          (?\(
           (throw 'return js2-LP))
          (?\)
           (throw 'return js2-RP))
          (?,
           (throw 'return js2-COMMA))
          (??
           (throw 'return js2-HOOK))
          (?:
           (if (js2-match-char ?:)
               js2-COLONCOLON
             (throw 'return js2-COLON)))
          (?.
           (if (js2-match-char ?.)
               (if (js2-match-char ?.)
                   js2-TRIPLEDOT js2-DOTDOT)
             (if (js2-match-char ?\()
                 js2-DOTQUERY
               (throw 'return js2-DOT))))
          (?|
           (if (js2-match-char ?|)
               (throw 'return js2-OR)
             (if (js2-match-char ?=)
                 js2-ASSIGN_BITOR
               (throw 'return js2-BITOR))))
          (?^
           (if (js2-match-char ?=)
               js2-ASSIGN_BITOR
             (throw 'return js2-BITXOR)))
          (?&
           (if (js2-match-char ?&)
               (throw 'return js2-AND)
             (if (js2-match-char ?=)
                 js2-ASSIGN_BITAND
               (throw 'return js2-BITAND))))
          (?=
           (if (js2-match-char ?=)
               (if (js2-match-char ?=)
                   js2-SHEQ
                 (throw 'return js2-EQ))
             (if (js2-match-char ?>)
                 (js2-ts-return token js2-ARROW)
               (throw 'return js2-ASSIGN))))
          (?!
           (if (js2-match-char ?=)
               (if (js2-match-char ?=)
                   js2-SHNE
                 js2-NE)
             (throw 'return js2-NOT)))
          (?<
           ;; NB:treat HTML begin-comment as comment-till-eol
           (when (js2-match-char ?!)
             (when (js2-match-char ?-)
               (when (js2-match-char ?-)
                 (js2-skip-line)
                 (setf (js2-token-comment-type (js2-current-token)) 'html)
                 (throw 'return js2-COMMENT)))
             (js2-unget-char))
           (if (js2-match-char ?<)
               (if (js2-match-char ?=)
                   js2-ASSIGN_LSH
                 js2-LSH)
             (if (js2-match-char ?=)
                 js2-LE
               (throw 'return js2-LT))))
          (?>
           (if (js2-match-char ?>)
               (if (js2-match-char ?>)
                   (if (js2-match-char ?=)
                       js2-ASSIGN_URSH
                     js2-URSH)
                 (if (js2-match-char ?=)
                     js2-ASSIGN_RSH
                   js2-RSH))
             (if (js2-match-char ?=)
                 js2-GE
               (throw 'return js2-GT))))
          (?*
           (if (js2-match-char ?=)
               js2-ASSIGN_MUL
             (throw 'return js2-MUL)))
          (?/
           ;; is it a // comment?
           (when (js2-match-char ?/)
             (setf (js2-token-beg token) (- js2-ts-cursor 2))
             (js2-skip-line)
             (setf (js2-token-comment-type token) 'line)
             ;; include newline so highlighting goes to end of window
             (incf (js2-token-end token))
             (throw 'return js2-COMMENT))
           ;; is it a /* comment?
           (when (js2-match-char ?*)
             (setf look-for-slash nil
                   (js2-token-beg token) (- js2-ts-cursor 2)
                   (js2-token-comment-type token)
                   (if (js2-match-char ?*)
                       (progn
                         (setq look-for-slash t)
                         'jsdoc)
                     'block))
             (while t
               (setq c (js2-get-char))
               (cond
                ((eq c js2-EOF_CHAR)
                 (setf (js2-token-end token) (1- js2-ts-cursor))
                 (js2-report-error "msg.unterminated.comment")
                 (throw 'return js2-COMMENT))
                ((eq c ?*)
                 (setq look-for-slash t))
                ((eq c ?/)
                 (if look-for-slash
                     (js2-ts-return token js2-COMMENT)))
                (t
                 (setf look-for-slash nil
                       (js2-token-end token) js2-ts-cursor)))))
           (if (js2-match-char ?=)
               js2-ASSIGN_DIV
             (throw 'return js2-DIV)))
          (?#
           (when js2-skip-preprocessor-directives
             (js2-skip-line)
             (setf (js2-token-comment-type token) 'preprocessor
                   (js2-token-end token) js2-ts-cursor)
             (throw 'return js2-COMMENT))
           (throw 'return js2-ERROR))
          (?%
           (if (js2-match-char ?=)
               js2-ASSIGN_MOD
             (throw 'return js2-MOD)))
          (?~
           (throw 'return js2-BITNOT))
          (?+
           (if (js2-match-char ?=)
               js2-ASSIGN_ADD
             (if (js2-match-char ?+)
                 js2-INC
               (throw 'return js2-ADD))))
          (?-
           (cond
            ((js2-match-char ?=)
             (setq c js2-ASSIGN_SUB))
            ((js2-match-char ?-)
             (unless js2-ts-dirty-line
               ;; treat HTML end-comment after possible whitespace
               ;; after line start as comment-until-eol
               (when (js2-match-char ?>)
                 (js2-skip-line)
                 (setf (js2-token-comment-type (js2-current-token)) 'html)
                 (throw 'return js2-COMMENT)))
             (setq c js2-DEC))
            (t
             (setq c js2-SUB)))
           (setq js2-ts-dirty-line t)
           c)
          (otherwise
           (js2-report-scan-error "msg.illegal.character")))))))
   (setf (js2-token-type token) tt)
   token))

(defun js2-read-regexp (start-tt)
  "Called by parser when it gets / or /= in literal context."
  (let (c err
        in-class  ; inside a '[' .. ']' character-class
        flags
        (continue t)
        (token (js2-new-token 0)))
    (setq js2-ts-string-buffer nil)
    (if (eq start-tt js2-ASSIGN_DIV)
        ;; mis-scanned /=
        (js2-add-to-string ?=)
      (if (not (eq start-tt js2-DIV))
          (error "failed assertion")))
    (while (and (not err)
                (or (/= (setq c (js2-get-char)) ?/)
                    in-class))
      (cond
       ((or (= c ?\n)
            (= c js2-EOF_CHAR))
        (setf (js2-token-end token) (1- js2-ts-cursor)
              err t
              (js2-token-string token) (js2-collect-string js2-ts-string-buffer))
        (js2-report-error "msg.unterminated.re.lit"))
       (t (cond
           ((= c ?\\)
            (js2-add-to-string c)
            (setq c (js2-get-char)))
           ((= c ?\[)
            (setq in-class t))
           ((= c ?\])
            (setq in-class nil)))
          (js2-add-to-string c))))
    (unless err
      (while continue
        (cond
         ((js2-match-char ?g)
          (push ?g flags))
         ((js2-match-char ?i)
          (push ?i flags))
         ((js2-match-char ?m)
          (push ?m flags))
         (t
          (setq continue nil))))
      (if (js2-alpha-p (js2-peek-char))
          (js2-report-scan-error "msg.invalid.re.flag" t
                                 js2-ts-cursor 1))
      (js2-set-string-from-buffer token)
      ;; tell `parse-partial-sexp' to ignore this range of chars
      (js2-record-text-property (js2-current-token-beg)
                                (js2-current-token-end) 'syntax-class '(2)))
    (js2-collect-string flags)))

(defun js2-get-first-xml-token ()
  (setq js2-ts-xml-open-tags-count 0
        js2-ts-is-xml-attribute nil
        js2-ts-xml-is-tag-content nil)
  (js2-unget-char)
  (js2-get-next-xml-token))

(defun js2-xml-discard-string (token)
  "Throw away the string in progress and flag an XML parse error."
  (setf js2-ts-string-buffer nil
        (js2-token-string token) nil)
  (js2-report-scan-error "msg.XML.bad.form" t))

(defun js2-get-next-xml-token ()
  (setq js2-ts-string-buffer nil)  ; for recording the XML
  (let ((token (js2-new-token 0))
        c result)
    (setq result
          (catch 'return
            (while t
              (setq c (js2-get-char))
              (cond
               ((= c js2-EOF_CHAR)
                (throw 'return js2-ERROR))
               (js2-ts-xml-is-tag-content
                (case c
                  (?>
                   (js2-add-to-string c)
                   (setq js2-ts-xml-is-tag-content nil
                         js2-ts-is-xml-attribute nil))
                  (?/
                   (js2-add-to-string c)
                   (when (eq ?> (js2-peek-char))
                     (setq c (js2-get-char))
                     (js2-add-to-string c)
                     (setq js2-ts-xml-is-tag-content nil)
                     (decf js2-ts-xml-open-tags-count)))
                  (?{
                   (js2-unget-char)
                   (js2-set-string-from-buffer token)
                   (throw 'return js2-XML))
                  ((?\' ?\")
                   (js2-add-to-string c)
                   (unless (js2-read-quoted-string c token)
                     (throw 'return js2-ERROR)))
                  (?=
                   (js2-add-to-string c)
                   (setq js2-ts-is-xml-attribute t))
                  ((? ?\t ?\r ?\n)
                   (js2-add-to-string c))
                  (t
                   (js2-add-to-string c)
                   (setq js2-ts-is-xml-attribute nil)))
                (when (and (not js2-ts-xml-is-tag-content)
                           (zerop js2-ts-xml-open-tags-count))
                  (js2-set-string-from-buffer token)
                  (throw 'return js2-XMLEND)))
               (t
                ;; else not tag content
                (case c
                  (?<
                   (js2-add-to-string c)
                   (setq c (js2-peek-char))
                   (case c
                     (?!
                      (setq c (js2-get-char)) ;; skip !
                      (js2-add-to-string c)
                      (setq c (js2-peek-char))
                      (case c
                        (?-
                         (setq c (js2-get-char)) ;; skip -
                         (js2-add-to-string c)
                         (if (eq c ?-)
                             (progn
                               (js2-add-to-string c)
                               (unless (js2-read-xml-comment token)
                                 (throw 'return js2-ERROR)))
                           (js2-xml-discard-string token)
                           (throw 'return js2-ERROR)))
                        (?\[
                         (setq c (js2-get-char)) ;; skip [
                         (js2-add-to-string c)
                         (if (and (= (js2-get-char) ?C)
                                  (= (js2-get-char) ?D)
                                  (= (js2-get-char) ?A)
                                  (= (js2-get-char) ?T)
                                  (= (js2-get-char) ?A)
                                  (= (js2-get-char) ?\[))
                             (progn
                               (js2-add-to-string ?C)
                               (js2-add-to-string ?D)
                               (js2-add-to-string ?A)
                               (js2-add-to-string ?T)
                               (js2-add-to-string ?A)
                               (js2-add-to-string ?\[)
                               (unless (js2-read-cdata token)
                                 (throw 'return js2-ERROR)))
                           (js2-xml-discard-string token)
                           (throw 'return js2-ERROR)))
                        (t
                         (unless (js2-read-entity token)
                           (throw 'return js2-ERROR))))
                      ;; Allow bare CDATA section, e.g.:
                      ;;   let xml = <![CDATA[ foo bar baz ]]>;
                      (when (zerop js2-ts-xml-open-tags-count)
                        (throw 'return js2-XMLEND)))
                     (??
                      (setq c (js2-get-char)) ;; skip ?
                      (js2-add-to-string c)
                      (unless (js2-read-PI token)
                        (throw 'return js2-ERROR)))
                     (?/
                      ;; end tag
                      (setq c (js2-get-char)) ;; skip /
                      (js2-add-to-string c)
                      (when (zerop js2-ts-xml-open-tags-count)
                        (js2-xml-discard-string token)
                        (throw 'return js2-ERROR))
                      (setq js2-ts-xml-is-tag-content t)
                      (decf js2-ts-xml-open-tags-count))
                     (t
                      ;; start tag
                      (setq js2-ts-xml-is-tag-content t)
                      (incf js2-ts-xml-open-tags-count))))
                  (?{
                   (js2-unget-char)
                   (js2-set-string-from-buffer token)
                   (throw 'return js2-XML))
                  (t
                   (js2-add-to-string c))))))))
    (setf (js2-token-end token) js2-ts-cursor)
    (setf (js2-token-type token) result)
    result))

(defun js2-read-quoted-string (quote token)
  (let (c)
    (catch 'return
      (while (/= (setq c (js2-get-char)) js2-EOF_CHAR)
        (js2-add-to-string c)
        (if (eq c quote)
            (throw 'return t)))
      (js2-xml-discard-string token)  ;; throw away string in progress
      nil)))

(defun js2-read-xml-comment (token)
  (let ((c (js2-get-char)))
    (catch 'return
      (while (/= c js2-EOF_CHAR)
        (catch 'continue
          (js2-add-to-string c)
          (when (and (eq c ?-) (eq ?- (js2-peek-char)))
            (setq c (js2-get-char))
            (js2-add-to-string c)
            (if (eq (js2-peek-char) ?>)
                (progn
                  (setq c (js2-get-char)) ;; skip >
                  (js2-add-to-string c)
                  (throw 'return t))
              (throw 'continue nil)))
          (setq c (js2-get-char))))
      (js2-xml-discard-string token)
      nil)))

(defun js2-read-cdata (token)
  (let ((c (js2-get-char)))
    (catch 'return
      (while (/= c js2-EOF_CHAR)
        (catch 'continue
          (js2-add-to-string c)
          (when (and (eq c ?\]) (eq (js2-peek-char) ?\]))
            (setq c (js2-get-char))
            (js2-add-to-string c)
            (if (eq (js2-peek-char) ?>)
                (progn
                  (setq c (js2-get-char)) ;; Skip >
                  (js2-add-to-string c)
                  (throw 'return t))
              (throw 'continue nil)))
          (setq c (js2-get-char))))
      (js2-xml-discard-string token)
      nil)))

(defun js2-read-entity (token)
  (let ((decl-tags 1)
        c)
    (catch 'return
      (while (/= js2-EOF_CHAR (setq c (js2-get-char)))
        (js2-add-to-string c)
        (case c
          (?<
           (incf decl-tags))
          (?>
           (decf decl-tags)
           (if (zerop decl-tags)
               (throw 'return t)))))
      (js2-xml-discard-string token)
      nil)))

(defun js2-read-PI (token)
  "Scan an XML processing instruction."
  (let (c)
    (catch 'return
      (while (/= js2-EOF_CHAR (setq c (js2-get-char)))
        (js2-add-to-string c)
        (when (and (eq c ??) (eq (js2-peek-char) ?>))
          (setq c (js2-get-char))  ;; Skip >
          (js2-add-to-string c)
          (throw 'return t)))
      (js2-xml-discard-string token)
      nil)))

;;; Highlighting

(defun js2-set-face (beg end face &optional record)
  "Fontify a region.  If RECORD is non-nil, record for later."
  (when (plusp js2-highlight-level)
    (setq beg (min (point-max) beg)
          beg (max (point-min) beg)
          end (min (point-max) end)
          end (max (point-min) end))
    (if record
        (push (list beg end face) js2-mode-fontifications)
      (put-text-property beg end 'font-lock-face face))))

(defsubst js2-clear-face (beg end)
  (remove-text-properties beg end '(font-lock-face nil
                                    help-echo nil
                                    point-entered nil
                                    c-in-sws nil)))

(defconst js2-ecma-global-props
  (concat "^"
          (regexp-opt
           '("Infinity" "NaN" "undefined" "arguments") t)
          "$")
  "Value properties of the Ecma-262 Global Object.
Shown at or above `js2-highlight-level' 2.")

;; might want to add the name "arguments" to this list?
(defconst js2-ecma-object-props
  (concat "^"
          (regexp-opt
           '("prototype" "__proto__" "__parent__") t)
          "$")
  "Value properties of the Ecma-262 Object constructor.
Shown at or above `js2-highlight-level' 2.")

(defconst js2-ecma-global-funcs
  (concat
   "^"
   (regexp-opt
    '("decodeURI" "decodeURIComponent" "encodeURI" "encodeURIComponent"
      "eval" "isFinite" "isNaN" "parseFloat" "parseInt") t)
   "$")
  "Function properties of the Ecma-262 Global object.
Shown at or above `js2-highlight-level' 2.")

(defconst js2-ecma-number-props
  (concat "^"
          (regexp-opt '("MAX_VALUE" "MIN_VALUE" "NaN"
                        "NEGATIVE_INFINITY"
                        "POSITIVE_INFINITY") t)
          "$")
  "Properties of the Ecma-262 Number constructor.
Shown at or above `js2-highlight-level' 2.")

(defconst js2-ecma-date-props "^\\(parse\\|UTC\\)$"
  "Properties of the Ecma-262 Date constructor.
Shown at or above `js2-highlight-level' 2.")

(defconst js2-ecma-math-props
  (concat "^"
          (regexp-opt
           '("E" "LN10" "LN2" "LOG2E" "LOG10E" "PI" "SQRT1_2" "SQRT2")
           t)
          "$")
  "Properties of the Ecma-262 Math object.
Shown at or above `js2-highlight-level' 2.")

(defconst js2-ecma-math-funcs
  (concat "^"
          (regexp-opt
           '("abs" "acos" "asin" "atan" "atan2" "ceil" "cos" "exp" "floor"
             "log" "max" "min" "pow" "random" "round" "sin" "sqrt" "tan") t)
          "$")
  "Function properties of the Ecma-262 Math object.
Shown at or above `js2-highlight-level' 2.")

(defconst js2-ecma-function-props
  (concat
   "^"
   (regexp-opt
    '(;; properties of the Object prototype object
      "hasOwnProperty" "isPrototypeOf" "propertyIsEnumerable"
      "toLocaleString" "toString" "valueOf"
      ;; properties of the Function prototype object
      "apply" "call"
      ;; properties of the Array prototype object
      "concat" "join" "pop" "push" "reverse" "shift" "slice" "sort"
      "splice" "unshift"
      ;; properties of the String prototype object
      "charAt" "charCodeAt" "fromCharCode" "indexOf" "lastIndexOf"
      "localeCompare" "match" "replace" "search" "split" "substring"
      "toLocaleLowerCase" "toLocaleUpperCase" "toLowerCase"
      "toUpperCase"
      ;; properties of the Number prototype object
      "toExponential" "toFixed" "toPrecision"
      ;; properties of the Date prototype object
      "getDate" "getDay" "getFullYear" "getHours" "getMilliseconds"
      "getMinutes" "getMonth" "getSeconds" "getTime"
      "getTimezoneOffset" "getUTCDate" "getUTCDay" "getUTCFullYear"
      "getUTCHours" "getUTCMilliseconds" "getUTCMinutes" "getUTCMonth"
      "getUTCSeconds" "setDate" "setFullYear" "setHours"
      "setMilliseconds" "setMinutes" "setMonth" "setSeconds" "setTime"
      "setUTCDate" "setUTCFullYear" "setUTCHours" "setUTCMilliseconds"
      "setUTCMinutes" "setUTCMonth" "setUTCSeconds" "toDateString"
      "toLocaleDateString" "toLocaleString" "toLocaleTimeString"
      "toTimeString" "toUTCString"
      ;; properties of the RegExp prototype object
      "exec" "test"
      ;; properties of the JSON prototype object
      "parse" "stringify"
      ;; SpiderMonkey/Rhino extensions, versions 1.5+
      "toSource" "__defineGetter__" "__defineSetter__"
      "__lookupGetter__" "__lookupSetter__" "__noSuchMethod__"
      "every" "filter" "forEach" "lastIndexOf" "map" "some")
    t)
   "$")
  "Built-in functions defined by Ecma-262 and SpiderMonkey extensions.
Shown at or above `js2-highlight-level' 3.")

(defun js2-parse-highlight-prop-get (parent target prop call-p)
  (let ((target-name (and target
                          (js2-name-node-p target)
                          (js2-name-node-name target)))
        (prop-name (if prop (js2-name-node-name prop)))
        (level2 (>= js2-highlight-level 2))
        (level3 (>= js2-highlight-level 3)))
    (when level2
      (let ((face
             (if call-p
                 (cond
                  ((and target prop)
                   (cond
                    ((and level3 (string-match js2-ecma-function-props prop-name))
                     'font-lock-builtin-face)
                    ((and target-name prop)
                     (cond
                      ((string= target-name "Date")
                       (if (string-match js2-ecma-date-props prop-name)
                           'font-lock-builtin-face))
                      ((string= target-name "Math")
                       (if (string-match js2-ecma-math-funcs prop-name)
                           'font-lock-builtin-face))))))
                  (prop
                   (if (string-match js2-ecma-global-funcs prop-name)
                       'font-lock-builtin-face)))
               (cond
                ((and target prop)
                 (cond
                  ((string= target-name "Number")
                   (if (string-match js2-ecma-number-props prop-name)
                       'font-lock-constant-face))
                  ((string= target-name "Math")
                   (if (string-match js2-ecma-math-props prop-name)
                       'font-lock-constant-face))))
                (prop
                 (if (string-match js2-ecma-object-props prop-name)
                     'font-lock-constant-face))))))
        (when face
          (let ((pos (+ (js2-node-pos parent)  ; absolute
                        (js2-node-pos prop)))) ; relative
            (js2-set-face pos
                          (+ pos (js2-node-len prop))
                          face 'record)))))))

(defun js2-parse-highlight-member-expr-node (node)
  "Perform syntax highlighting of EcmaScript built-in properties.
The variable `js2-highlight-level' governs this highighting."
  (let (face target prop name pos end parent call-p callee)
    (cond
     ;; case 1:  simple name, e.g. foo
     ((js2-name-node-p node)
      (setq name (js2-name-node-name node))
      ;; possible for name to be nil in rare cases - saw it when
      ;; running js2-mode on an elisp buffer.  Might as well try to
      ;; make it so js2-mode never barfs.
      (when name
        (setq face (if (string-match js2-ecma-global-props name)
                       'font-lock-constant-face))
        (when face
          (setq pos (js2-node-pos node)
                end (+ pos (js2-node-len node)))
          (js2-set-face pos end face 'record))))
     ;; case 2:  property access or function call
     ((or (js2-prop-get-node-p node)
          ;; highlight function call if expr is a prop-get node
          ;; or a plain name (i.e. unqualified function call)
          (and (setq call-p (js2-call-node-p node))
               (setq callee (js2-call-node-target node)) ; separate setq!
               (or (js2-prop-get-node-p callee)
                   (js2-name-node-p callee))))
      (setq parent node
            node (if call-p callee node))
      (if (and call-p (js2-name-node-p callee))
          (setq prop callee)
        (setq target (js2-prop-get-node-left node)
              prop (js2-prop-get-node-right node)))
      (cond
       ((js2-name-node-p prop)
        ;; case 2(a&c):  simple or complex target, simple name, e.g. x[y].bar
        (js2-parse-highlight-prop-get parent target prop call-p))
       ((js2-name-node-p target)
        ;; case 2b:  simple target, complex name, e.g. foo.x[y]
        (js2-parse-highlight-prop-get parent target nil call-p)))))))

(defun js2-parse-highlight-member-expr-fn-name (expr)
  "Highlight the `baz' in function foo.bar.baz(args) {...}.
This is experimental Rhino syntax.  EXPR is the foo.bar.baz member expr.
We currently only handle the case where the last component is a prop-get
of a simple name.  Called before EXPR has a parent node."
  (let (pos
        (name (and (js2-prop-get-node-p expr)
                   (js2-prop-get-node-right expr))))
    (when (js2-name-node-p name)
      (js2-set-face (setq pos (+ (js2-node-pos expr)  ; parent is absolute
                                 (js2-node-pos name)))
                    (+ pos (js2-node-len name))
                    'font-lock-function-name-face
                    'record))))

;; source:  http://jsdoc.sourceforge.net/
;; Note - this syntax is for Google's enhanced jsdoc parser that
;; allows type specifications, and needs work before entering the wild.

(defconst js2-jsdoc-param-tag-regexp
  (concat "^\\s-*\\*+\\s-*\\(@"
          "\\(?:param\\|argument\\)"
          "\\)"
          "\\s-*\\({[^}]+}\\)?"         ; optional type
          "\\s-*\\[?\\([[:alnum:]_$\.]+\\)?\\]?"  ; name
          "\\>")
  "Matches jsdoc tags with optional type and optional param name.")

(defconst js2-jsdoc-typed-tag-regexp
  (concat "^\\s-*\\*+\\s-*\\(@\\(?:"
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
             "throws"))
          "\\)\\)\\s-*\\({[^}]+}\\)?")
  "Matches jsdoc tags with optional type.")

(defconst js2-jsdoc-arg-tag-regexp
  (concat "^\\s-*\\*+\\s-*\\(@\\(?:"
          (regexp-opt
           '("alias"
             "augments"
             "borrows"
             "bug"
             "base"
             "config"
             "default"
             "define"
             "exception"
             "function"
             "member"
             "memberOf"
             "name"
             "namespace"
             "property"
             "since"
             "suppress"
             "this"
             "throws"
             "type"
             "version"))
          "\\)\\)\\s-+\\([^ \t]+\\)")
  "Matches jsdoc tags with a single argument.")

(defconst js2-jsdoc-empty-tag-regexp
  (concat "^\\s-*\\*+\\s-*\\(@\\(?:"
          (regexp-opt
           '("addon"
             "author"
             "class"
             "const"
             "constant"
             "constructor"
             "constructs"
             "deprecated"
             "desc"
             "description"
             "event"
             "example"
             "exec"
             "export"
             "fileoverview"
             "final"
             "function"
             "hidden"
             "ignore"
             "implicitCast"
             "inheritDoc"
             "inner"
             "interface"
             "license"
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

(defconst js2-jsdoc-link-tag-regexp
  "{\\(@\\(?:link\\|code\\)\\)\\s-+\\([^#}\n]+\\)\\(#.+\\)?}"
  "Matches a jsdoc link or code tag.")

(defconst js2-jsdoc-see-tag-regexp
  "^\\s-*\\*+\\s-*\\(@see\\)\\s-+\\([^#}\n]+\\)\\(#.+\\)?"
  "Matches a jsdoc @see tag.")

(defconst js2-jsdoc-html-tag-regexp
  "\\(</?\\)\\([[:alpha:]]+\\)\\s-*\\(/?>\\)"
  "Matches a simple (no attributes) html start- or end-tag.")

(defun js2-jsdoc-highlight-helper ()
  (js2-set-face (match-beginning 1)
                (match-end 1)
                'js2-jsdoc-tag)
  (if (match-beginning 2)
      (if (save-excursion
            (goto-char (match-beginning 2))
            (= (char-after) ?{))
          (js2-set-face (1+ (match-beginning 2))
                        (1- (match-end 2))
                        'js2-jsdoc-type)
        (js2-set-face (match-beginning 2)
                      (match-end 2)
                      'js2-jsdoc-value)))
  (if (match-beginning 3)
      (js2-set-face (match-beginning 3)
                    (match-end 3)
                    'js2-jsdoc-value)))

(defun js2-highlight-jsdoc (ast)
  "Highlight doc comment tags."
  (let ((comments (js2-ast-root-comments ast))
        beg end)
    (save-excursion
      (dolist (node comments)
        (when (eq (js2-comment-node-format node) 'jsdoc)
          (setq beg (js2-node-abs-pos node)
                end (+ beg (js2-node-len node)))
          (save-restriction
            (narrow-to-region beg end)
            (dolist (re (list js2-jsdoc-param-tag-regexp
                              js2-jsdoc-typed-tag-regexp
                              js2-jsdoc-arg-tag-regexp
                              js2-jsdoc-link-tag-regexp
                              js2-jsdoc-see-tag-regexp
                              js2-jsdoc-empty-tag-regexp))
              (goto-char beg)
              (while (re-search-forward re nil t)
                (js2-jsdoc-highlight-helper)))
            ;; simple highlighting for html tags
            (goto-char beg)
            (while (re-search-forward js2-jsdoc-html-tag-regexp nil t)
              (js2-set-face (match-beginning 1)
                            (match-end 1)
                            'js2-jsdoc-html-tag-delimiter)
              (js2-set-face (match-beginning 2)
                            (match-end 2)
                            'js2-jsdoc-html-tag-name)
              (js2-set-face (match-beginning 3)
                            (match-end 3)
                            'js2-jsdoc-html-tag-delimiter))))))))

(defun js2-highlight-assign-targets (_node left right)
  "Highlight function properties and external variables."
  (let (leftpos name)
    ;; highlight vars and props assigned function values
    (when (js2-function-node-p right)
      (cond
       ;; var foo = function() {...}
       ((js2-name-node-p left)
        (setq name left))
       ;; foo.bar.baz = function() {...}
       ((and (js2-prop-get-node-p left)
             (js2-name-node-p (js2-prop-get-node-right left)))
        (setq name (js2-prop-get-node-right left))))
      (when name
        (js2-set-face (setq leftpos (js2-node-abs-pos name))
                      (+ leftpos (js2-node-len name))
                      'font-lock-function-name-face
                      'record)))))

(defun js2-record-name-node (node)
  "Saves NODE to `js2-recorded-identifiers' to check for undeclared variables
later. NODE must be a name node."
  (let ((leftpos (js2-node-abs-pos node)))
    (push (list node js2-current-scope
                leftpos
                (+ leftpos (js2-node-len node)))
          js2-recorded-identifiers)))

(defun js2-highlight-undeclared-vars ()
  "After entire parse is finished, look for undeclared variable references.
We have to wait until entire buffer is parsed, since JavaScript permits var
decls to occur after they're used.

If any undeclared var name is in `js2-externs' or `js2-additional-externs',
it is considered declared."
  (let (name)
    (dolist (entry js2-recorded-identifiers)
      (destructuring-bind (name-node scope pos end) entry
        (setq name (js2-name-node-name name-node))
        (unless (or (member name js2-global-externs)
                    (member name js2-default-externs)
                    (member name js2-additional-externs)
                    (js2-get-defining-scope scope name))
          (js2-report-warning "msg.undeclared.variable" name pos (- end pos)
                              'js2-external-variable))))
    (setq js2-recorded-identifiers nil)))

(defun js2-set-default-externs ()
  "Set the value of `js2-default-externs' based on the various
`js2-include-?-externs' variables."
  (setq js2-default-externs
        (append js2-ecma-262-externs
                (if js2-include-browser-externs js2-browser-externs)
                (if js2-include-rhino-externs js2-rhino-externs)
                (if js2-include-node-externs js2-node-externs)
                (if (or js2-include-browser-externs js2-include-node-externs)
                    js2-typed-array-externs))))

(defun js2-apply-jslint-globals ()
  (setq js2-additional-externs
        (nconc (js2-get-jslint-globals)
               js2-additional-externs)))

(defun js2-get-jslint-globals ()
  (loop for node in (js2-ast-root-comments js2-mode-ast)
        when (and (eq 'block (js2-comment-node-format node))
                  (save-excursion
                    (goto-char (js2-node-abs-pos node))
                    (looking-at "/\\*global ")))
        append (js2-get-jslint-globals-in
                (match-end 0)
                (js2-node-abs-end node))))

(defun js2-get-jslint-globals-in (beg end)
  (let (res)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward js2-mode-identifier-re end t)
        (let ((match (match-string 0)))
          (unless (member match '("true" "false"))
            (push match res)))))
    (nreverse res)))

;;; IMenu support

;; We currently only support imenu, but eventually should support speedbar and
;; possibly other browsing mechanisms.

;; The basic strategy is to identify function assignment targets of the form
;; `foo.bar.baz', convert them to (list fn foo bar baz <position>), and push the
;; list into `js2-imenu-recorder'.  The lists are merged into a trie-like tree
;; for imenu after parsing is finished.

;; A `foo.bar.baz' assignment target may be expressed in many ways in
;; JavaScript, and the general problem is undecidable.  However, several forms
;; are readily recognizable at parse-time; the forms we attempt to recognize
;; include:

;;  function foo()  -- function declaration
;;  foo = function()  -- function expression assigned to variable
;;  foo.bar.baz = function()  -- function expr assigned to nested property-get
;;  foo = {bar: function()}  -- fun prop in object literal assigned to var
;;  foo = {bar: {baz: function()}} -- inside nested object literal
;;  foo.bar = {baz: function()}} -- obj lit assigned to nested prop get
;;  a.b = {c: {d: function()}} -- nested obj lit assigned to nested prop get
;;  foo = {get bar() {...}}  -- getter/setter in obj literal
;;  function foo() {function bar() {...}}  -- nested function
;;  foo['a'] = function()  -- fun expr assigned to deterministic element-get

;; This list boils down to a few forms that can be combined recursively.
;; Top-level named function declarations include both the left-hand (name)
;; and the right-hand (function value) expressions needed to produce an imenu
;; entry.  The other "right-hand" forms we need to look for are:
;;  - functions declared as props/getters/setters in object literals
;;  - nested named function declarations
;; The "left-hand" expressions that functions can be assigned to include:
;;  - local/global variables
;;  - nested property-get expressions like a.b.c.d
;;  - element gets like foo[10] or foo['bar'] where the index
;;    expression can be trivially converted to a property name.  They
;;    effectively then become property gets.

;; All the different definition types are canonicalized into the form
;; foo.bar.baz = position-of-function-keyword

;; We need to build a trie-like structure for imenu.  As an example,
;; consider the following JavaScript code:

;; a = function() {...}  // function at position 5
;; b = function() {...}  // function at position 25
;; foo = function() {...} // function at position 100
;; foo.bar = function() {...} // function at position 200
;; foo.bar.baz = function() {...} // function at position 300
;; foo.bar.zab = function() {...} // function at position 400

;; During parsing we accumulate an entry for each definition in
;; the variable `js2-imenu-recorder', like so:

;; '((fn a 5)
;;   (fn b 25)
;;   (fn foo 100)
;;   (fn foo bar 200)
;;   (fn foo bar baz 300)
;;   (fn foo bar zab 400))

;; Where 'fn' is the respective function node.
;; After parsing these entries are merged into this alist-trie:

;; '((a . 1)
;;   (b . 2)
;;   (foo (<definition> . 3)
;;        (bar (<definition> . 6)
;;             (baz . 100)
;;             (zab . 200))))

;; Note the wacky need for a <definition> name.  The token can be anything
;; that isn't a valid JavaScript identifier, because you might make foo
;; a function and then start setting properties on it that are also functions.

(defun js2-prop-node-name (node)
  "Return the name of a node that may be a property-get/property-name.
If NODE is not a valid name-node, string-node or integral number-node,
returns nil.  Otherwise returns the string name/value of the node."
  (cond
   ((js2-name-node-p node)
    (js2-name-node-name node))
   ((js2-string-node-p node)
    (js2-string-node-value node))
   ((and (js2-number-node-p node)
         (string-match "^[0-9]+$" (js2-number-node-value node)))
    (js2-number-node-value node))
   ((js2-this-node-p node)
    "this")))

(defun js2-node-qname-component (node)
  "Return the name of this node, if it contributes to a qname.
Returns nil if the node doesn't contribute."
  (copy-sequence
   (or (js2-prop-node-name node)
       (if (and (js2-function-node-p node)
                (js2-function-node-name node))
           (js2-name-node-name (js2-function-node-name node))))))

(defun js2-record-imenu-entry (fn-node qname pos)
  "Add an entry to `js2-imenu-recorder'.
FN-NODE should be the current item's function node.

Associate FN-NODE with its QNAME for later lookup.
This is used in postprocessing the chain list.  For each chain, we find
the parent function, look up its qname, then prepend a copy of it to the chain."
  (push (cons fn-node (append qname (list pos))) js2-imenu-recorder)
  (unless js2-imenu-function-map
    (setq js2-imenu-function-map (make-hash-table :test 'eq)))
  (puthash fn-node qname js2-imenu-function-map))

(defun js2-record-imenu-functions (node &optional var)
  "Record function definitions for imenu.
NODE is a function node or an object literal.
VAR, if non-nil, is the expression that NODE is being assigned to.
When passed arguments of wrong type, does nothing."
  (when js2-parse-ide-mode
    (let ((fun-p (js2-function-node-p node))
          qname fname-node)
      (cond
       ;; non-anonymous function declaration?
       ((and fun-p
             (not var)
             (setq fname-node (js2-function-node-name node)))
        (js2-record-imenu-entry node (list fname-node) (js2-node-pos node)))
       ;; for remaining forms, compute left-side tree branch first
       ((and var (setq qname (js2-compute-nested-prop-get var)))
        (cond
         ;; foo.bar.baz = function
         (fun-p
          (js2-record-imenu-entry node qname (js2-node-pos node)))
         ;; foo.bar.baz = object-literal
         ;; look for nested functions:  {a: {b: function() {...} }}
         ((js2-object-node-p node)
          ;; Node position here is still absolute, since the parser
          ;; passes the assignment target and value expressions
          ;; to us before they are added as children of the assignment node.
          (js2-record-object-literal node qname (js2-node-pos node)))))))))

(defun js2-compute-nested-prop-get (node)
  "If NODE is of form foo.bar, foo['bar'], or any nested combination, return
component nodes as a list.  Otherwise return nil.  Element-gets are treated
as property-gets if the index expression is a string, or a positive integer."
  (let (left right head)
    (cond
     ((or (js2-name-node-p node)
          (js2-this-node-p node))
      (list node))
     ;; foo.bar.baz is parenthesized as (foo.bar).baz => right operand is a leaf
     ((js2-prop-get-node-p node)        ; foo.bar
      (setq left (js2-prop-get-node-left node)
            right (js2-prop-get-node-right node))
      (if (setq head (js2-compute-nested-prop-get left))
          (nconc head (list right))))
     ((js2-elem-get-node-p node)        ; foo['bar'] or foo[101]
      (setq left (js2-elem-get-node-target node)
            right (js2-elem-get-node-element node))
      (if (or (js2-string-node-p right)      ; ['bar']
              (and (js2-number-node-p right) ; [10]
                   (string-match "^[0-9]+$"
                                 (js2-number-node-value right))))
          (if (setq head (js2-compute-nested-prop-get left))
              (nconc head (list right))))))))

(defun js2-record-object-literal (node qname pos)
  "Recursively process an object literal looking for functions.
NODE is an object literal that is the right-hand child of an assignment
expression.  QNAME is a list of nodes representing the assignment target,
e.g. for foo.bar.baz = {...}, QNAME is (foo-node bar-node baz-node).
POS is the absolute position of the node.
We do a depth-first traversal of NODE.  For any functions we find,
we append the property name to QNAME, then call `js2-record-imenu-entry'."
  (let (right)
    (dolist (e (js2-object-node-elems node))  ; e is a `js2-object-prop-node'
      (let ((left (js2-infix-node-left e))
            ;; Element positions are relative to the parent position.
            (pos (+ pos (js2-node-pos e))))
        (cond
         ;; foo: function() {...}
         ((js2-function-node-p (setq right (js2-infix-node-right e)))
          (when (js2-prop-node-name left)
            ;; As a policy decision, we record the position of the property,
            ;; not the position of the `function' keyword, since the property
            ;; is effectively the name of the function.
            (js2-record-imenu-entry right (append qname (list left)) pos)))
         ;; foo: {object-literal} -- add foo to qname, offset position, and recurse
         ((js2-object-node-p right)
          (js2-record-object-literal right
                                     (append qname (list (js2-infix-node-left e)))
                                     (+ pos (js2-node-pos right)))))))))

(defun js2-node-top-level-decl-p (node)
  "Return t if NODE's name is defined in the top-level scope.
Also returns t if NODE's name is not defined in any scope, since it implies
that it's an external variable, which must also be in the top-level scope."
  (let* ((name (js2-prop-node-name node))
         (this-scope (js2-node-get-enclosing-scope node))
         defining-scope)
    (cond
     ((js2-this-node-p node)
      nil)
     ((null this-scope)
      t)
     ((setq defining-scope (js2-get-defining-scope this-scope name))
      (js2-ast-root-p defining-scope))
     (t t))))

(defun js2-wrapper-function-p (node)
  "Return t if NODE is a function expression that's immediately invoked.
NODE must be `js2-function-node'."
  (let ((parent (js2-node-parent node)))
    (or
     ;; function(){...}();
     (js2-call-node-p parent)
     (and (js2-paren-node-p parent)
          ;; (function(){...})();
          (or (js2-call-node-p (setq parent (js2-node-parent parent)))
              ;; (function(){...}).call(this);
              (and (js2-prop-get-node-p parent)
                   (member (js2-name-node-name (js2-prop-get-node-right parent))
                           '("call" "apply"))
                   (js2-call-node-p (js2-node-parent parent))))))))

(defun js2-browse-postprocess-chains (entries)
  "Modify function-declaration name chains after parsing finishes.
Some of the information is only available after the parse tree is complete.
For instance, processing a nested scope requires a parent function node."
  (let (result fn parent-qname p elem)
    (dolist (entry entries)
      ;; function node goes first
      (destructuring-bind (current-fn &rest (&whole chain head &rest)) entry
        ;; Examine head's defining scope:
        ;; Pre-processed chain, or top-level/external, keep as-is.
        (if (or (stringp head) (js2-node-top-level-decl-p head))
            (push chain result)
          (when (js2-this-node-p head)
            (setq chain (cdr chain))) ; discard this-node
          (when (setq fn (js2-node-parent-script-or-fn current-fn))
            (setq parent-qname (gethash fn js2-imenu-function-map 'not-found))
            (when (eq parent-qname 'not-found)
              ;; anonymous function expressions are not recorded
              ;; during the parse, so we need to handle this case here
              (setq parent-qname
                    (if (js2-wrapper-function-p fn)
                        (let ((grandparent (js2-node-parent-script-or-fn fn)))
                          (if (js2-ast-root-p grandparent)
                              nil
                            (gethash grandparent js2-imenu-function-map 'skip)))
                      'skip))
              (puthash fn parent-qname js2-imenu-function-map))
            (unless (eq parent-qname 'skip)
              ;; prefix parent fn qname to this chain.
              (let ((qname (append parent-qname chain)))
                (puthash current-fn (butlast qname) js2-imenu-function-map)
                (push qname result)))))))
    ;; finally replace each node in each chain with its name.
    (dolist (chain result)
      (setq p chain)
      (while p
        (if (js2-node-p (setq elem (car p)))
            (setcar p (js2-node-qname-component elem)))
        (setq p (cdr p))))
    result))

;; Merge name chains into a trie-like tree structure of nested lists.
;; To simplify construction of the trie, we first build it out using the rule
;; that the trie consists of lists of pairs.  Each pair is a 2-element array:
;; [key, num-or-list].  The second element can be a number; if so, this key
;; is a leaf-node with only one value.  (I.e. there is only one declaration
;; associated with the key at this level.)  Otherwise the second element is
;; a list of pairs, with the rule applied recursively.  This symmetry permits
;; a simple recursive formulation.
;;
;; js2-mode is building the data structure for imenu.  The imenu documentation
;; claims that it's the structure above, but in practice it wants the children
;; at the same list level as the key for that level, which is how I've drawn
;; the "Expected final result" above.  We'll postprocess the trie to remove the
;; list wrapper around the children at each level.
;;
;; A completed nested imenu-alist entry looks like this:
;;       '(("foo"
;;          ("<definition>" . 7)
;;          ("bar"
;;           ("a" . 40)
;;           ("b" . 60))))
;;
;; In particular, the documentation for `imenu--index-alist' says that
;; a nested sub-alist element looks like (INDEX-NAME SUB-ALIST).
;; The sub-alist entries immediately follow INDEX-NAME, the head of the list.

(defun js2-treeify (lst)
  "Convert (a b c d) to (a ((b ((c d)))))."
  (if (null (cddr lst))  ; list length <= 2
      lst
    (list (car lst) (list (js2-treeify (cdr lst))))))

(defun js2-build-alist-trie (chains trie)
  "Merge declaration name chains into a trie-like alist structure for imenu.
CHAINS is the qname chain list produced during parsing. TRIE is a
list of elements built up so far."
  (let (head tail pos branch kids)
    (dolist (chain chains)
      (setq head (car chain)
            tail (cdr chain)
            pos (if (numberp (car tail)) (car tail))
            branch (js2-find-if (lambda (n)
                                  (string= (car n) head))
                                trie)
            kids (second branch))
      (cond
       ;; case 1:  this key isn't in the trie yet
       ((null branch)
        (if trie
            (setcdr (last trie) (list (js2-treeify chain)))
          (setq trie (list (js2-treeify chain)))))
       ;; case 2:  key is present with a single number entry:  replace w/ list
       ;;  ("a1" 10)  +  ("a1" 20) => ("a1" (("<definition>" 10)
       ;;                                    ("<definition>" 20)))
       ((numberp kids)
        (setcar (cdr branch)
                (list (list "<definition-1>" kids)
                      (if pos
                          (list "<definition-2>" pos)
                        (js2-treeify tail)))))
       ;; case 3:  key is there (with kids), and we're a number entry
       (pos
        (setcdr (last kids)
                (list
                 (list (format "<definition-%d>"
                               (1+ (loop for kid in kids
                                         count (eq ?< (aref (car kid) 0)))))
                       pos))))
       ;; case 4:  key is there with kids, need to merge in our chain
       (t
        (js2-build-alist-trie (list tail) kids))))
    trie))

(defun js2-flatten-trie (trie)
  "Convert TRIE to imenu-format.
Recurses through nodes, and for each one whose second element is a list,
appends the list's flattened elements to the current element.  Also
changes the tails into conses.  For instance, this pre-flattened trie

'(a ((b 20)
     (c ((d 30)
         (e 40)))))

becomes

'(a (b . 20)
    (c (d . 30)
       (e . 40)))

Note that the root of the trie has no key, just a list of chains.
This is also true for the value of any key with multiple children,
e.g. key 'c' in the example above."
  (cond
   ((listp (car trie))
    (mapcar #'js2-flatten-trie trie))
   (t
    (if (numberp (second trie))
        (cons (car trie) (second trie))
      ;; else pop list and append its kids
      (apply #'append (list (car trie)) (js2-flatten-trie (cdr trie)))))))

(defun js2-build-imenu-index ()
  "Turn `js2-imenu-recorder' into an imenu data structure."
  (unless (eq js2-imenu-recorder 'empty)
    (let* ((chains (js2-browse-postprocess-chains js2-imenu-recorder))
           (result (js2-build-alist-trie chains nil)))
      (js2-flatten-trie result))))

(defun js2-test-print-chains (chains)
  "Print a list of qname chains.
Each element of CHAINS is a list of the form (NODE [NODE *] pos);
i.e. one or more nodes, and an integer position as the list tail."
  (mapconcat (lambda (chain)
               (concat "("
                       (mapconcat (lambda (elem)
                                    (if (js2-node-p elem)
                                        (or (js2-node-qname-component elem)
                                            "nil")
                                      (number-to-string elem)))
                                  chain
                                  " ")
                       ")"))
             chains
             "\n"))

;;; Parser

(defconst js2-version "1.8.5"
  "Version of JavaScript supported.")

(defun js2-record-face (face &optional token)
  "Record a style run of FACE for TOKEN or the current token."
  (unless token (setq token (js2-current-token)))
  (js2-set-face (js2-token-beg token) (js2-token-end token) face 'record))

(defsubst js2-node-end (n)
  "Computes the absolute end of node N.
Use with caution!  Assumes `js2-node-pos' is -absolute-, which
is only true until the node is added to its parent; i.e., while parsing."
  (+ (js2-node-pos n)
     (js2-node-len n)))

(defun js2-record-comment (token)
  "Record a comment in `js2-scanned-comments'."
  (let ((ct (js2-token-comment-type token))
        (beg (js2-token-beg token))
        (end (js2-token-end token)))
    (push (make-js2-comment-node :len (- end beg)
                                 :format ct)
          js2-scanned-comments)
    (when js2-parse-ide-mode
      (js2-record-face (if (eq ct 'jsdoc)
                           'font-lock-doc-face
                         'font-lock-comment-face)
                       token)
      (when (memq ct '(html preprocessor))
        ;; Tell cc-engine the bounds of the comment.
        (js2-record-text-property beg (1- end) 'c-in-sws t)))))

(defun js2-peek-token ()
  "Return the next token type without consuming it.
If `js2-ti-lookahead' is positive, return the type of next token
from `js2-ti-tokens'.  Otherwise, call `js2-get-token'."
  (if (not (zerop js2-ti-lookahead))
      (js2-token-type
       (aref js2-ti-tokens (mod (1+ js2-ti-tokens-cursor) js2-ti-ntokens)))
    (let ((tt (js2-get-token-internal)))
      (js2-unget-token)
      tt)))

(defalias 'js2-next-token 'js2-get-token)

(defun js2-match-token (match)
  "Get next token and return t if it matches MATCH, a bytecode.
Returns nil and consumes nothing if MATCH is not the next token."
  (if (/= (js2-get-token) match)
      (ignore (js2-unget-token))
    t))

(defun js2-match-contextual-kwd (name)
  "Consume and return t if next token is `js2-NAME', and its
string is NAME.  Returns nil and keeps current token otherwise."
  (if (or (/= (js2-get-token) js2-NAME)
          (not (string= (js2-current-token-string) name)))
      (progn
        (js2-unget-token)
        nil)
    (js2-record-face 'font-lock-keyword-face)
    t))

(defun js2-valid-prop-name-token (tt)
  (or (= tt js2-NAME)
      (and js2-allow-keywords-as-property-names
           (plusp tt)
           (aref js2-kwd-tokens tt))))

(defun js2-match-prop-name ()
  "Consume token and return t if next token is a valid property name.
It's valid if it's a js2-NAME, or `js2-allow-keywords-as-property-names'
is non-nil and it's a keyword token."
  (if (js2-valid-prop-name-token (js2-get-token))
      t
    (js2-unget-token)
    nil))

(defun js2-must-match-prop-name (msg-id &optional pos len)
  (if (js2-match-prop-name)
      t
    (js2-report-error msg-id nil pos len)
    nil))

(defun js2-peek-token-or-eol ()
  "Return js2-EOL if the next token immediately follows a newline.
Else returns the next token.  Used in situations where we don't
consider certain token types valid if they are preceded by a newline.
One example is the postfix ++ or -- operator, which has to be on the
same line as its operand."
  (let ((tt (js2-get-token))
        (follows-eol (js2-token-follows-eol-p (js2-current-token))))
    (js2-unget-token)
    (if follows-eol
        js2-EOL
      tt)))

(defun js2-must-match (token msg-id &optional pos len)
  "Match next token to token code TOKEN, or record a syntax error.
MSG-ID is the error message to report if the match fails.
Returns t on match, nil if no match."
  (if (js2-match-token token)
      t
    (js2-report-error msg-id nil pos len)
    nil))

(defsubst js2-inside-function ()
  (plusp js2-nesting-of-function))

(defun js2-set-requires-activation ()
  (if (js2-function-node-p js2-current-script-or-fn)
      (setf (js2-function-node-needs-activation js2-current-script-or-fn) t)))

(defun js2-check-activation-name (name _token)
  (when (js2-inside-function)
    ;; skip language-version 1.2 check from Rhino
    (if (or (string= "arguments" name)
            (and js2-compiler-activation-names  ; only used in codegen
                 (gethash name js2-compiler-activation-names)))
        (js2-set-requires-activation))))

(defun js2-set-is-generator ()
  (if (js2-function-node-p js2-current-script-or-fn)
      (setf (js2-function-node-is-generator js2-current-script-or-fn) t)))

(defun js2-must-have-xml ()
  (unless js2-compiler-xml-available
    (js2-report-error "msg.XML.not.available")))

(defun js2-push-scope (scope)
  "Push SCOPE, a `js2-scope', onto the lexical scope chain."
  (assert (js2-scope-p scope))
  (assert (null (js2-scope-parent-scope scope)))
  (assert (not (eq js2-current-scope scope)))
  (setf (js2-scope-parent-scope scope) js2-current-scope
        js2-current-scope scope))

(defsubst js2-pop-scope ()
  (setq js2-current-scope
        (js2-scope-parent-scope js2-current-scope)))

(defun js2-enter-loop (loop-node)
  (push loop-node js2-loop-set)
  (push loop-node js2-loop-and-switch-set)
  (js2-push-scope loop-node)
  ;; Tell the current labeled statement (if any) its statement,
  ;; and set the jump target of the first label to the loop.
  ;; These are used in `js2-parse-continue' to verify that the
  ;; continue target is an actual labeled loop.  (And for codegen.)
  (when js2-labeled-stmt
    (setf (js2-labeled-stmt-node-stmt js2-labeled-stmt) loop-node
          (js2-label-node-loop (car (js2-labeled-stmt-node-labels
                                     js2-labeled-stmt))) loop-node)))

(defun js2-exit-loop ()
  (pop js2-loop-set)
  (pop js2-loop-and-switch-set)
  (js2-pop-scope))

(defsubst js2-enter-switch (switch-node)
  (push switch-node js2-loop-and-switch-set))

(defsubst js2-exit-switch ()
  (pop js2-loop-and-switch-set))

(defun js2-parse (&optional buf cb)
  "Tell the js2 parser to parse a region of JavaScript.

BUF is a buffer or buffer name containing the code to parse.
Call `narrow-to-region' first to parse only part of the buffer.

The returned AST root node is given some additional properties:
  `node-count' - total number of nodes in the AST
  `buffer' - BUF.  The buffer it refers to may change or be killed,
             so the value is not necessarily reliable.

An optional callback CB can be specified to report parsing
progress.  If `(functionp CB)' returns t, it will be called with
the current line number once before parsing begins, then again
each time the lexer reaches a new line number.

CB can also be a list of the form `(symbol cb ...)' to specify
multiple callbacks with different criteria.  Each symbol is a
criterion keyword, and the following element is the callback to
call

  :line  - called whenever the line number changes
  :token - called for each new token consumed

The list of criteria could be extended to include entering or
leaving a statement, an expression, or a function definition."
  (if (and cb (not (functionp cb)))
      (error "criteria callbacks not yet implemented"))
  (let ((inhibit-point-motion-hooks t)
        (js2-compiler-xml-available (>= js2-language-version 160))
        ;; This is a recursive-descent parser, so give it a big stack.
        (max-lisp-eval-depth (max max-lisp-eval-depth 3000))
        (max-specpdl-size (max max-specpdl-size 3000))
        (case-fold-search nil)
        ast)
    (message nil)  ; clear any error message from previous parse
    (with-current-buffer (or buf (current-buffer))
      (setq js2-scanned-comments nil
            js2-parsed-errors nil
            js2-parsed-warnings nil
            js2-imenu-recorder nil
            js2-imenu-function-map nil
            js2-label-set nil)
      (js2-init-scanner)
      (setq ast (with-silent-modifications
                  (js2-do-parse)))
      (unless js2-ts-hit-eof
        (js2-report-error "msg.got.syntax.errors" (length js2-parsed-errors)))
      (setf (js2-ast-root-errors ast) js2-parsed-errors
            (js2-ast-root-warnings ast) js2-parsed-warnings)
      ;; if we didn't find any declarations, put a dummy in this list so we
      ;; don't end up re-parsing the buffer in `js2-mode-create-imenu-index'
      (unless js2-imenu-recorder
        (setq js2-imenu-recorder 'empty))
      (run-hooks 'js2-parse-finished-hook)
      ast)))

;; Corresponds to Rhino's Parser.parse() method.
(defun js2-do-parse ()
  "Parse current buffer starting from current point.
Scanner should be initialized."
  (let ((pos js2-ts-cursor)
        (end js2-ts-cursor)  ; in case file is empty
        root n tt)
    ;; initialize buffer-local parsing vars
    (setf root (make-js2-ast-root :buffer (buffer-name) :pos pos)
          js2-current-script-or-fn root
          js2-current-scope root
          js2-nesting-of-function 0
          js2-labeled-stmt nil
          js2-recorded-identifiers nil)  ; for js2-highlight
    (while (/= (setq tt (js2-get-token)) js2-EOF)
      (if (= tt js2-FUNCTION)
          (progn
            (setq n (if js2-called-by-compile-function
                        (js2-parse-function-expr)
                      (js2-parse-function-stmt))))
        ;; not a function - parse a statement
        (js2-unget-token)
        (setq n (js2-parse-statement)))
      ;; add function or statement to script
      (setq end (js2-node-end n))
      (js2-block-node-push root n))
    ;; add comments to root in lexical order
    (when js2-scanned-comments
      ;; if we find a comment beyond end of normal kids, use its end
      (setq end (max end (js2-node-end (first js2-scanned-comments))))
      (dolist (comment js2-scanned-comments)
        (push comment (js2-ast-root-comments root))
        (js2-node-add-children root comment)))
    (setf (js2-node-len root) (- end pos))
    (setq js2-mode-ast root)  ; Make sure this is available for callbacks.
    ;; Give extensions a chance to muck with things before highlighting starts.
    (let ((js2-additional-externs js2-additional-externs))
      (save-excursion
        (run-hooks 'js2-post-parse-callbacks))
      (js2-highlight-undeclared-vars))
    root))

(defun js2-function-parser ()
  (js2-get-token)
  (js2-parse-function-stmt))

(defun js2-parse-function-closure-body (fn-node)
  "Parse a JavaScript 1.8 function closure body."
  (let ((js2-nesting-of-function (1+ js2-nesting-of-function)))
    (if js2-ts-hit-eof
        (js2-report-error "msg.no.brace.body" nil
                          (js2-node-pos fn-node)
                          (- js2-ts-cursor (js2-node-pos fn-node)))
      (js2-node-add-children fn-node
                             (setf (js2-function-node-body fn-node)
                                   (js2-parse-expr t))))))

(defun js2-parse-function-body (fn-node)
  (js2-must-match js2-LC "msg.no.brace.body"
                  (js2-node-pos fn-node)
                  (- js2-ts-cursor (js2-node-pos fn-node)))
  (let ((pos (js2-current-token-beg))         ; LC position
        (pn (make-js2-block-node))  ; starts at LC position
        tt
        end)
    (incf js2-nesting-of-function)
    (unwind-protect
        (while (not (or (= (setq tt (js2-peek-token)) js2-ERROR)
                        (= tt js2-EOF)
                        (= tt js2-RC)))
          (js2-block-node-push pn (if (/= tt js2-FUNCTION)
                                      (js2-parse-statement)
                                    (js2-get-token)
                                    (js2-parse-function-stmt))))
      (decf js2-nesting-of-function))
    (setq end (js2-current-token-end))  ; assume no curly and leave at current token
    (if (js2-must-match js2-RC "msg.no.brace.after.body" pos)
        (setq end (js2-current-token-end)))
    (setf (js2-node-pos pn) pos
          (js2-node-len pn) (- end pos))
    (setf (js2-function-node-body fn-node) pn)
    (js2-node-add-children fn-node pn)
    pn))

(defun js2-define-destruct-symbols (node decl-type face &optional ignore-not-in-block)
  "Declare and fontify destructuring parameters inside NODE.
NODE is either `js2-array-node', `js2-object-node', or `js2-name-node'."
  (cond
   ((js2-name-node-p node)
    (let (leftpos)
      (js2-define-symbol decl-type (js2-name-node-name node)
                         node ignore-not-in-block)
      (when face
        (js2-set-face (setq leftpos (js2-node-abs-pos node))
                      (+ leftpos (js2-node-len node))
                      face 'record))))
   ((js2-object-node-p node)
    (dolist (elem (js2-object-node-elems node))
      (js2-define-destruct-symbols
       (if (js2-object-prop-node-p elem)
           (js2-object-prop-node-right elem)
         ;; abbreviated destructuring {a, b}
         elem)
       decl-type face ignore-not-in-block)))
   ((js2-array-node-p node)
    (dolist (elem (js2-array-node-elems node))
      (when elem
        (js2-define-destruct-symbols elem decl-type face ignore-not-in-block))))
   (t (js2-report-error "msg.no.parm" nil (js2-node-abs-pos node)
                        (js2-node-len node)))))

(defun js2-parse-function-params (function-type fn-node pos)
  (if (js2-match-token js2-RP)
      (setf (js2-function-node-rp fn-node) (- (js2-current-token-beg) pos))
    (let ((paren-free-arrow (and (eq function-type 'FUNCTION_ARROW)
                                 (eq (js2-current-token-type) js2-NAME)))
          params param default-found rest-param-at)
      (when paren-free-arrow
        (js2-unget-token))
      (loop for tt = (js2-peek-token)
            do
            (cond
             ;; destructuring param
             ((and (not paren-free-arrow)
                   (or (= tt js2-LB) (= tt js2-LC)))
              (js2-get-token)
              (when default-found
                (js2-report-error "msg.no.default.after.default.param"))
              (setq param (js2-parse-destruct-primary-expr))
              (js2-define-destruct-symbols param
                                           js2-LP
                                           'js2-function-param)
              (push param params))
             ;; variable name
             (t
              (when (and (>= js2-language-version 200)
                         (not paren-free-arrow)
                         (js2-match-token js2-TRIPLEDOT)
                         (not rest-param-at))
                ;; to report errors if there are more parameters
                (setq rest-param-at (length params)))
              (js2-must-match js2-NAME "msg.no.parm")
              (js2-record-face 'js2-function-param)
              (setq param (js2-create-name-node))
              (js2-define-symbol js2-LP (js2-current-token-string) param)
              ;; default parameter value
              (when (or (and default-found
                             (not rest-param-at)
                             (js2-must-match js2-ASSIGN
                                             "msg.no.default.after.default.param"
                                             (js2-node-pos param)
                                             (js2-node-len param)))
                        (and (>= js2-language-version 200)
                             (js2-match-token js2-ASSIGN)))
                (assert (not paren-free-arrow))
                (let* ((pos (js2-node-pos param))
                       (tt (js2-current-token-type))
                       (op-pos (- (js2-current-token-beg) pos))
                       (left param)
                       (right (js2-parse-assign-expr))
                       (len (- (js2-node-end right) pos)))
                  (setq param (make-js2-assign-node
                               :type tt :pos pos :len len :op-pos op-pos
                               :left left :right right)
                        default-found t)
                  (js2-node-add-children param left right)))
              (push param params)))
            (when (and rest-param-at (> (length params) (1+ rest-param-at)))
              (js2-report-error "msg.param.after.rest" nil
                                (js2-node-pos param) (js2-node-len param)))
            while
            (js2-match-token js2-COMMA))
      (when (and (not paren-free-arrow)
                 (js2-must-match js2-RP "msg.no.paren.after.parms"))
        (setf (js2-function-node-rp fn-node) (- (js2-current-token-beg) pos)))
      (when rest-param-at
        (setf (js2-function-node-rest-p fn-node) t))
      (dolist (p params)
        (js2-node-add-children fn-node p)
        (push p (js2-function-node-params fn-node))))))

(defun js2-check-inconsistent-return-warning (fn-node name)
  "Possibly show inconsistent-return warning.
Last token scanned is the close-curly for the function body."
  (when (and js2-mode-show-strict-warnings
             js2-strict-inconsistent-return-warning
             (not (js2-has-consistent-return-usage
                   (js2-function-node-body fn-node))))
    ;; Have it extend from close-curly to bol or beginning of block.
    (let ((pos (save-excursion
                 (goto-char (js2-current-token-end))
                 (max (js2-node-abs-pos (js2-function-node-body fn-node))
                      (point-at-bol))))
          (end (js2-current-token-end)))
      (if (plusp (js2-name-node-length name))
          (js2-add-strict-warning "msg.no.return.value"
                                  (js2-name-node-name name) pos end)
        (js2-add-strict-warning "msg.anon.no.return.value" nil pos end)))))

(defun js2-parse-function-stmt ()
  (let ((pos (js2-current-token-beg)))
    (js2-must-match js2-NAME "msg.unnamed.function.stmt")
    (let ((name (js2-create-name-node t))
          pn member-expr)
      (cond
       ((js2-match-token js2-LP)
        (js2-parse-function 'FUNCTION_STATEMENT pos name))
       (js2-allow-member-expr-as-function-name
        (setq member-expr (js2-parse-member-expr-tail nil name))
        (js2-parse-highlight-member-expr-fn-name member-expr)
        (js2-must-match js2-LP "msg.no.paren.parms")
        (setf pn (js2-parse-function 'FUNCTION_STATEMENT pos)
              (js2-function-node-member-expr pn) member-expr)
        pn)
       (t
        (js2-report-error "msg.no.paren.parms")
        (make-js2-error-node))))))

(defun js2-parse-function-expr ()
  (let ((pos (js2-current-token-beg))
        name)
    (when (js2-match-token js2-NAME)
      (setq name (js2-create-name-node t)))
    (js2-must-match js2-LP "msg.no.paren.parms")
    (js2-parse-function 'FUNCTION_EXPRESSION pos name)))

(defun js2-parse-function (function-type pos &optional name)
  "Function parser.  FUNCTION-TYPE is a symbol, POS is the
beginning of the first token (function keyword, unless it's an
arrow function), NAME is js2-name-node."
  (let (fn-node lp)
    (if (= (js2-current-token-type) js2-LP) ; eventually matched LP?
        (setq lp (js2-current-token-beg)))
    (setf fn-node (make-js2-function-node :pos pos
                                          :name name
                                          :form function-type
                                          :lp (if lp (- lp pos))))
    (when name
      (js2-set-face (js2-node-pos name) (js2-node-end name)
                    'font-lock-function-name-face 'record)
      (when (plusp (js2-name-node-length name))
        ;; Function statements define a symbol in the enclosing scope
        (js2-define-symbol js2-FUNCTION (js2-name-node-name name) fn-node)))
    (if (or (js2-inside-function) (plusp js2-nesting-of-with))
        ;; 1. Nested functions are not affected by the dynamic scope flag
        ;;    as dynamic scope is already a parent of their scope.
        ;; 2. Functions defined under the with statement also immune to
        ;;    this setup, in which case dynamic scope is ignored in favor
        ;;    of the with object.
        (setf (js2-function-node-ignore-dynamic fn-node) t))
    ;; dynamically bind all the per-function variables
    (let ((js2-current-script-or-fn fn-node)
          (js2-current-scope fn-node)
          (js2-nesting-of-with 0)
          (js2-end-flags 0)
          js2-label-set
          js2-loop-set
          js2-loop-and-switch-set)
      (js2-parse-function-params function-type fn-node pos)
      (when (eq function-type 'FUNCTION_ARROW)
        (js2-must-match js2-ARROW "msg.bad.arrow.args"))
      (if (and (>= js2-language-version 180)
               (/= (js2-peek-token) js2-LC))
          (js2-parse-function-closure-body fn-node)
        (js2-parse-function-body fn-node))
      (js2-check-inconsistent-return-warning fn-node name)

      (when name
        (js2-node-add-children fn-node name)
        ;; Function expressions define a name only in the body of the
        ;; function, and only if not hidden by a parameter name
        (when (and (eq function-type 'FUNCTION_EXPRESSION)
                   (null (js2-scope-get-symbol js2-current-scope
                                               (js2-name-node-name name))))
          (js2-define-symbol js2-FUNCTION
                             (js2-name-node-name name)
                             fn-node))
        (when (eq function-type 'FUNCTION_STATEMENT)
          (js2-record-imenu-functions fn-node))))

    (setf (js2-node-len fn-node) (- js2-ts-cursor pos))
    ;; Rhino doesn't do this, but we need it for finding undeclared vars.
    ;; We wait until after parsing the function to set its parent scope,
    ;; since `js2-define-symbol' needs the defining-scope check to stop
    ;; at the function boundary when checking for redeclarations.
    (setf (js2-scope-parent-scope fn-node) js2-current-scope)
    fn-node))

(defun js2-parse-statements (&optional parent)
  "Parse a statement list.  Last token consumed must be js2-LC.

PARENT can be a `js2-block-node', in which case the statements are
appended to PARENT.  Otherwise a new `js2-block-node' is created
and returned.

This function does not match the closing js2-RC: the caller
matches the RC so it can provide a suitable error message if not
matched.  This means it's up to the caller to set the length of
the node to include the closing RC.  The node start pos is set to
the absolute buffer start position, and the caller should fix it
up to be relative to the parent node.  All children of this block
node are given relative start positions and correct lengths."
  (let ((pn (or parent (make-js2-block-node)))
        tt)
    (setf (js2-node-pos pn) (js2-current-token-beg))
    (while (and (> (setq tt (js2-peek-token)) js2-EOF)
                (/= tt js2-RC))
      (js2-block-node-push pn (js2-parse-statement)))
    pn))

(defun js2-parse-statement ()
  (let (pn beg end)
    ;; coarse-grained user-interrupt check - needs work
    (and js2-parse-interruptable-p
         (zerop (% (incf js2-parse-stmt-count)
                   js2-statements-per-pause))
         (input-pending-p)
         (throw 'interrupted t))
    (setq pn (js2-statement-helper))
    ;; no-side-effects warning check
    (unless (js2-node-has-side-effects pn)
      (setq end (js2-node-end pn))
      (save-excursion
        (goto-char end)
        (setq beg (max (js2-node-pos pn) (point-at-bol))))
      (js2-add-strict-warning "msg.no.side.effects" nil beg end))
    pn))

;; These correspond to the switch cases in Parser.statementHelper
(defconst js2-parsers
  (let ((parsers (make-vector js2-num-tokens
                                #'js2-parse-expr-stmt)))
    (aset parsers js2-BREAK     #'js2-parse-break)
    (aset parsers js2-CONST     #'js2-parse-const-var)
    (aset parsers js2-CONTINUE  #'js2-parse-continue)
    (aset parsers js2-DEBUGGER  #'js2-parse-debugger)
    (aset parsers js2-DEFAULT   #'js2-parse-default-xml-namespace)
    (aset parsers js2-DO        #'js2-parse-do)
    (aset parsers js2-FOR       #'js2-parse-for)
    (aset parsers js2-FUNCTION  #'js2-function-parser)
    (aset parsers js2-IF        #'js2-parse-if)
    (aset parsers js2-LC        #'js2-parse-block)
    (aset parsers js2-LET       #'js2-parse-let-stmt)
    (aset parsers js2-NAME      #'js2-parse-name-or-label)
    (aset parsers js2-RETURN    #'js2-parse-ret-yield)
    (aset parsers js2-SEMI      #'js2-parse-semi)
    (aset parsers js2-SWITCH    #'js2-parse-switch)
    (aset parsers js2-THROW     #'js2-parse-throw)
    (aset parsers js2-TRY       #'js2-parse-try)
    (aset parsers js2-VAR       #'js2-parse-const-var)
    (aset parsers js2-WHILE     #'js2-parse-while)
    (aset parsers js2-WITH      #'js2-parse-with)
    (aset parsers js2-YIELD     #'js2-parse-ret-yield)
    parsers)
  "A vector mapping token types to parser functions.")

(defun js2-parse-warn-missing-semi (beg end)
  (and js2-mode-show-strict-warnings
       js2-strict-missing-semi-warning
       (js2-add-strict-warning
        "msg.missing.semi" nil
        ;; back up to beginning of statement or line
        (max beg (save-excursion
                   (goto-char end)
                   (point-at-bol)))
        end)))

(defconst js2-no-semi-insertion
  (list js2-IF
        js2-SWITCH
        js2-WHILE
        js2-DO
        js2-FOR
        js2-TRY
        js2-WITH
        js2-LC
        js2-ERROR
        js2-SEMI
        js2-FUNCTION)
  "List of tokens that don't do automatic semicolon insertion.")

(defconst js2-autoinsert-semi-and-warn
  (list js2-ERROR js2-EOF js2-RC))

(defun js2-statement-helper ()
  (let* ((tt (js2-get-token))
         (first-tt tt)
         (parser (if (= tt js2-ERROR)
                     #'js2-parse-semi
                   (aref js2-parsers tt)))
         pn)
    ;; If the statement is set, then it's been told its label by now.
    (and js2-labeled-stmt
         (js2-labeled-stmt-node-stmt js2-labeled-stmt)
         (setq js2-labeled-stmt nil))
    (setq pn (funcall parser))
    ;; Don't do auto semi insertion for certain statement types.
    (unless (or (memq first-tt js2-no-semi-insertion)
                (js2-labeled-stmt-node-p pn))
      (js2-auto-insert-semicolon pn))
    pn))

(defun js2-auto-insert-semicolon (pn)
  (let* ((tt (js2-get-token))
         (pos (js2-node-pos pn)))
      (cond
       ((= tt js2-SEMI)
        ;; extend the node bounds to include the semicolon.
        (setf (js2-node-len pn) (- (js2-current-token-end) pos)))
       ((memq tt js2-autoinsert-semi-and-warn)
        (js2-unget-token) ; Not ';', do not consume.
        ;; Autoinsert ;
        (js2-parse-warn-missing-semi pos (js2-node-end pn)))
       (t
        (if (not (js2-token-follows-eol-p (js2-current-token)))
            ;; Report error if no EOL or autoinsert ';' otherwise
            (js2-report-error "msg.no.semi.stmt")
          (js2-parse-warn-missing-semi pos (js2-node-end pn)))
        (js2-unget-token) ; Not ';', do not consume.
        ))))

(defun js2-parse-condition ()
  "Parse a parenthesized boolean expression, e.g. in an if- or while-stmt.
The parens are discarded and the expression node is returned.
The `pos' field of the return value is set to an absolute position
that must be fixed up by the caller.
Return value is a list (EXPR LP RP), with absolute paren positions."
  (let (pn lp rp)
    (if (js2-must-match js2-LP "msg.no.paren.cond")
        (setq lp (js2-current-token-beg)))
    (setq pn (js2-parse-expr))
    (if (js2-must-match js2-RP "msg.no.paren.after.cond")
        (setq rp (js2-current-token-beg)))
    ;; Report strict warning on code like "if (a = 7) ..."
    (if (and js2-strict-cond-assign-warning
             (js2-assign-node-p pn))
        (js2-add-strict-warning "msg.equal.as.assign" nil
                                (js2-node-pos pn)
                                (+ (js2-node-pos pn)
                                   (js2-node-len pn))))
    (list pn lp rp)))

(defun js2-parse-if ()
  "Parser for if-statement.  Last matched token must be js2-IF."
  (let ((pos (js2-current-token-beg))
        cond if-true if-false else-pos end pn)
    (setq cond (js2-parse-condition)
          if-true (js2-parse-statement)
          if-false (if (js2-match-token js2-ELSE)
                       (progn
                         (setq else-pos (- (js2-current-token-beg) pos))
                         (js2-parse-statement)))
          end (js2-node-end (or if-false if-true))
          pn (make-js2-if-node :pos pos
                               :len (- end pos)
                               :condition (car cond)
                               :then-part if-true
                               :else-part if-false
                               :else-pos else-pos
                               :lp (js2-relpos (second cond) pos)
                               :rp (js2-relpos (third cond) pos)))
    (js2-node-add-children pn (car cond) if-true if-false)
    pn))

(defun js2-parse-switch ()
  "Parser for switch-statement.  Last matched token must be js2-SWITCH."
  (let ((pos (js2-current-token-beg))
        tt pn discriminant has-default case-expr case-node
        case-pos cases stmt lp)
    (if (js2-must-match js2-LP "msg.no.paren.switch")
        (setq lp (js2-current-token-beg)))
    (setq discriminant (js2-parse-expr)
          pn (make-js2-switch-node :discriminant discriminant
                                   :pos pos
                                   :lp (js2-relpos lp pos)))
    (js2-node-add-children pn discriminant)
    (js2-enter-switch pn)
    (unwind-protect
        (progn
          (if (js2-must-match js2-RP "msg.no.paren.after.switch")
              (setf (js2-switch-node-rp pn) (- (js2-current-token-beg) pos)))
          (js2-must-match js2-LC "msg.no.brace.switch")
          (catch 'break
            (while t
              (setq tt (js2-next-token)
                    case-pos (js2-current-token-beg))
              (cond
               ((= tt js2-RC)
                (setf (js2-node-len pn) (- (js2-current-token-end) pos))
                (throw 'break nil))  ; done
               ((= tt js2-CASE)
                (setq case-expr (js2-parse-expr))
                (js2-must-match js2-COLON "msg.no.colon.case"))
               ((= tt js2-DEFAULT)
                (if has-default
                    (js2-report-error "msg.double.switch.default"))
                (setq has-default t
                      case-expr nil)
                (js2-must-match js2-COLON "msg.no.colon.case"))
               (t
                (js2-report-error "msg.bad.switch")
                (throw 'break nil)))
              (setq case-node (make-js2-case-node :pos case-pos
                                                  :len (- (js2-current-token-end) case-pos)
                                                  :expr case-expr))
              (js2-node-add-children case-node case-expr)
              (while (and (/= (setq tt (js2-peek-token)) js2-RC)
                          (/= tt js2-CASE)
                          (/= tt js2-DEFAULT)
                          (/= tt js2-EOF))
                (setf stmt (js2-parse-statement)
                      (js2-node-len case-node) (- (js2-node-end stmt) case-pos))
                (js2-block-node-push case-node stmt))
              (push case-node cases)))
          ;; add cases last, as pushing reverses the order to be correct
          (dolist (kid cases)
            (js2-node-add-children pn kid)
            (push kid (js2-switch-node-cases pn)))
          pn)  ; return value
      (js2-exit-switch))))

(defun js2-parse-while ()
  "Parser for while-statement.  Last matched token must be js2-WHILE."
  (let ((pos (js2-current-token-beg))
        (pn (make-js2-while-node))
        cond body)
    (js2-enter-loop pn)
    (unwind-protect
        (progn
          (setf cond (js2-parse-condition)
                (js2-while-node-condition pn) (car cond)
                body (js2-parse-statement)
                (js2-while-node-body pn) body
                (js2-node-len pn) (- (js2-node-end body) pos)
                (js2-while-node-lp pn) (js2-relpos (second cond) pos)
                (js2-while-node-rp pn) (js2-relpos (third cond) pos))
          (js2-node-add-children pn body (car cond)))
      (js2-exit-loop))
    pn))

(defun js2-parse-do ()
  "Parser for do-statement.  Last matched token must be js2-DO."
  (let ((pos (js2-current-token-beg))
        (pn (make-js2-do-node))
        cond body end)
    (js2-enter-loop pn)
    (unwind-protect
        (progn
          (setq body (js2-parse-statement))
          (js2-must-match js2-WHILE "msg.no.while.do")
          (setf (js2-do-node-while-pos pn) (- (js2-current-token-beg) pos)
                cond (js2-parse-condition)
                (js2-do-node-condition pn) (car cond)
                (js2-do-node-body pn) body
                end js2-ts-cursor
                (js2-do-node-lp pn) (js2-relpos (second cond) pos)
                (js2-do-node-rp pn) (js2-relpos (third cond) pos))
          (js2-node-add-children pn (car cond) body))
      (js2-exit-loop))
    ;; Always auto-insert semicolon to follow SpiderMonkey:
    ;; It is required by ECMAScript but is ignored by the rest of
    ;; world; see bug 238945
    (if (js2-match-token js2-SEMI)
        (setq end js2-ts-cursor))
    (setf (js2-node-len pn) (- end pos))
    pn))

(defun js2-parse-for ()
  "Parser for for-statement.  Last matched token must be js2-FOR.
Parses for, for-in, and for each-in statements."
  (let ((for-pos (js2-current-token-beg))
        pn is-for-each is-for-in-or-of is-for-of
        in-pos each-pos tmp-pos
        init  ; Node init is also foo in 'foo in object'
        cond  ; Node cond is also object in 'foo in object'
        incr  ; 3rd section of for-loop initializer
        body tt lp rp)
    ;; See if this is a for each () instead of just a for ()
    (when (js2-match-token js2-NAME)
      (if (string= "each" (js2-current-token-string))
          (progn
            (setq is-for-each t
                  each-pos (- (js2-current-token-beg) for-pos)) ; relative
            (js2-record-face 'font-lock-keyword-face))
        (js2-report-error "msg.no.paren.for")))
    (if (js2-must-match js2-LP "msg.no.paren.for")
        (setq lp (- (js2-current-token-beg) for-pos)))
    (setq tt (js2-get-token))
    ;; 'for' makes local scope
    (js2-push-scope (make-js2-scope))
    (unwind-protect
        ;; parse init clause
        (let ((js2-in-for-init t))  ; set as dynamic variable
          (cond
           ((= tt js2-SEMI)
            (js2-unget-token)
            (setq init (make-js2-empty-expr-node)))
           ((or (= tt js2-VAR) (= tt js2-LET))
            (setq init (js2-parse-variables tt (js2-current-token-beg))))
           (t
            (js2-unget-token)
            (setq init (js2-parse-expr)))))
      (if (or (js2-match-token js2-IN)
              (and (>= js2-language-version 200)
                   (js2-match-contextual-kwd "of")
                   (setq is-for-of t)))
          (setq is-for-in-or-of t
                in-pos (- (js2-current-token-beg) for-pos)
                ;; scope of iteration target object is not the scope we've created above.
                ;; stash current scope temporary.
                cond (let ((js2-current-scope (js2-scope-parent-scope js2-current-scope)))
                       (js2-parse-expr)))  ; object over which we're iterating
        ;; else ordinary for loop - parse cond and incr
        (js2-must-match js2-SEMI "msg.no.semi.for")
        (setq cond (if (= (js2-peek-token) js2-SEMI)
                       (make-js2-empty-expr-node) ; no loop condition
                     (js2-parse-expr)))
        (js2-must-match js2-SEMI "msg.no.semi.for.cond")
        (setq tmp-pos (js2-current-token-end)
              incr (if (= (js2-peek-token) js2-RP)
                       (make-js2-empty-expr-node :pos tmp-pos)
                     (js2-parse-expr))))
      (if (js2-must-match js2-RP "msg.no.paren.for.ctrl")
          (setq rp (- (js2-current-token-beg) for-pos)))
      (if (not is-for-in-or-of)
          (setq pn (make-js2-for-node :init init
                                      :condition cond
                                      :update incr
                                      :lp lp
                                      :rp rp))
        ;; cond could be null if 'in obj' got eaten by the init node.
        (if (js2-infix-node-p init)
            ;; it was (foo in bar) instead of (var foo in bar)
            (setq cond (js2-infix-node-right init)
                  init (js2-infix-node-left init))
          (if (and (js2-var-decl-node-p init)
                   (> (length (js2-var-decl-node-kids init)) 1))
              (js2-report-error "msg.mult.index")))
        (setq pn (make-js2-for-in-node :iterator init
                                       :object cond
                                       :in-pos in-pos
                                       :foreach-p is-for-each
                                       :each-pos each-pos
                                       :forof-p is-for-of
                                       :lp lp
                                       :rp rp)))
      (unwind-protect
          (progn
            (js2-enter-loop pn)
            ;; We have to parse the body -after- creating the loop node,
            ;; so that the loop node appears in the js2-loop-set, allowing
            ;; break/continue statements to find the enclosing loop.
            (setf body (js2-parse-statement)
                  (js2-loop-node-body pn) body
                  (js2-node-pos pn) for-pos
                  (js2-node-len pn) (- (js2-node-end body) for-pos))
            (js2-node-add-children pn init cond incr body))
        ;; finally
        (js2-exit-loop))
      (js2-pop-scope))
    pn))

(defun js2-parse-try ()
  "Parser for try-statement.  Last matched token must be js2-TRY."
  (let ((try-pos (js2-current-token-beg))
        try-end
        try-block
        catch-blocks
        finally-block
        saw-default-catch
        peek
        param
        catch-cond
        catch-node
        guard-kwd
        catch-pos
        finally-pos
        pn
        block
        lp
        rp)
    (if (/= (js2-peek-token) js2-LC)
        (js2-report-error "msg.no.brace.try"))
    (setq try-block (js2-parse-statement)
          try-end (js2-node-end try-block)
          peek (js2-peek-token))
    (cond
     ((= peek js2-CATCH)
      (while (js2-match-token js2-CATCH)
        (setq catch-pos (js2-current-token-beg)
              guard-kwd nil
              catch-cond nil
              lp nil
              rp nil)
        (if saw-default-catch
            (js2-report-error "msg.catch.unreachable"))
        (if (js2-must-match js2-LP "msg.no.paren.catch")
            (setq lp (- (js2-current-token-beg) catch-pos)))
        (js2-push-scope (make-js2-scope))
        (let ((tt (js2-peek-token)))
          (cond
           ;; destructuring pattern
           ;;     catch ({ message, file }) { ... }
           ((or (= tt js2-LB) (= tt js2-LC))
            (js2-get-token)
            (setq param (js2-parse-destruct-primary-expr))
            (js2-define-destruct-symbols param js2-LET nil))
           ;; simple name
           (t
            (js2-must-match js2-NAME "msg.bad.catchcond")
            (setq param (js2-create-name-node))
            (js2-define-symbol js2-LET (js2-current-token-string) param))))
        ;; pattern guard
        (if (js2-match-token js2-IF)
            (setq guard-kwd (- (js2-current-token-beg) catch-pos)
                  catch-cond (js2-parse-expr))
          (setq saw-default-catch t))
        (if (js2-must-match js2-RP "msg.bad.catchcond")
            (setq rp (- (js2-current-token-beg) catch-pos)))
        (js2-must-match js2-LC "msg.no.brace.catchblock")
        (setq block (js2-parse-statements)
              try-end (js2-node-end block)
              catch-node (make-js2-catch-node :pos catch-pos
                                              :param param
                                              :guard-expr catch-cond
                                              :guard-kwd guard-kwd
                                              :block block
                                              :lp lp
                                              :rp rp))
        (js2-pop-scope)
        (if (js2-must-match js2-RC "msg.no.brace.after.body")
            (setq try-end (js2-current-token-beg)))
        (setf (js2-node-len block) (- try-end (js2-node-pos block))
              (js2-node-len catch-node) (- try-end catch-pos))
        (js2-node-add-children catch-node param catch-cond block)
        (push catch-node catch-blocks)))
     ((/= peek js2-FINALLY)
      (js2-must-match js2-FINALLY "msg.try.no.catchfinally"
                      (js2-node-pos try-block)
                      (- (setq try-end (js2-node-end try-block))
                         (js2-node-pos try-block)))))
    (when (js2-match-token js2-FINALLY)
      (setq finally-pos (js2-current-token-beg)
            block (js2-parse-statement)
            try-end (js2-node-end block)
            finally-block (make-js2-finally-node :pos finally-pos
                                                 :len (- try-end finally-pos)
                                                 :body block))
      (js2-node-add-children finally-block block))
    (setq pn (make-js2-try-node :pos try-pos
                                :len (- try-end try-pos)
                                :try-block try-block
                                :finally-block finally-block))
    (js2-node-add-children pn try-block finally-block)
    ;; push them onto the try-node, which reverses and corrects their order
    (dolist (cb catch-blocks)
      (js2-node-add-children pn cb)
      (push cb (js2-try-node-catch-clauses pn)))
    pn))

(defun js2-parse-throw ()
  "Parser for throw-statement.  Last matched token must be js2-THROW."
  (let ((pos (js2-current-token-beg))
        expr pn)
    (if (= (js2-peek-token-or-eol) js2-EOL)
        ;; ECMAScript does not allow new lines before throw expression,
        ;; see bug 256617
        (js2-report-error "msg.bad.throw.eol"))
    (setq expr (js2-parse-expr)
          pn (make-js2-throw-node :pos pos
                                  :len (- (js2-node-end expr) pos)
                                  :expr expr))
    (js2-node-add-children pn expr)
    pn))

(defun js2-match-jump-label-name (label-name)
  "If break/continue specified a label, return that label's labeled stmt.
Returns the corresponding `js2-labeled-stmt-node', or if LABEL-NAME
does not match an existing label, reports an error and returns nil."
  (let ((bundle (cdr (assoc label-name js2-label-set))))
    (if (null bundle)
        (js2-report-error "msg.undef.label"))
    bundle))

(defun js2-parse-break ()
  "Parser for break-statement.  Last matched token must be js2-BREAK."
  (let ((pos (js2-current-token-beg))
        (end (js2-current-token-end))
        break-target ; statement to break from
        break-label  ; in "break foo", name-node representing the foo
        labels       ; matching labeled statement to break to
        pn)
    (when (eq (js2-peek-token-or-eol) js2-NAME)
      (js2-get-token)
      (setq break-label (js2-create-name-node)
            end (js2-node-end break-label)
            ;; matchJumpLabelName only matches if there is one
            labels (js2-match-jump-label-name (js2-current-token-string))
            break-target (if labels (car (js2-labeled-stmt-node-labels labels)))))
    (unless (or break-target break-label)
      ;; no break target specified - try for innermost enclosing loop/switch
      (if (null js2-loop-and-switch-set)
          (unless break-label
            (js2-report-error "msg.bad.break" nil pos (length "break")))
        (setq break-target (car js2-loop-and-switch-set))))
    (setq pn (make-js2-break-node :pos pos
                                  :len (- end pos)
                                  :label break-label
                                  :target break-target))
    (js2-node-add-children pn break-label)  ; but not break-target
    pn))

(defun js2-parse-continue ()
  "Parser for continue-statement.  Last matched token must be js2-CONTINUE."
  (let ((pos (js2-current-token-beg))
        (end (js2-current-token-end))
        label   ; optional user-specified label, a `js2-name-node'
        labels  ; current matching labeled stmt, if any
        target  ; the `js2-loop-node' target of this continue stmt
        pn)
    (when (= (js2-peek-token-or-eol) js2-NAME)
      (js2-get-token)
      (setq label (js2-create-name-node)
            end (js2-node-end label)
            ;; matchJumpLabelName only matches if there is one
            labels (js2-match-jump-label-name (js2-current-token-string))))
    (cond
     ((null labels)  ; no current label to go to
      (if (null js2-loop-set)  ; no loop to continue to
          (js2-report-error "msg.continue.outside" nil pos
                            (length "continue"))
        (setq target (car js2-loop-set))))  ; innermost enclosing loop
     (t
      (if (js2-loop-node-p (js2-labeled-stmt-node-stmt labels))
          (setq target (js2-labeled-stmt-node-stmt labels))
        (js2-report-error "msg.continue.nonloop" nil pos (- end pos)))))
    (setq pn (make-js2-continue-node :pos pos
                                     :len (- end pos)
                                     :label label
                                     :target target))
    (js2-node-add-children pn label)  ; but not target - it's not our child
    pn))

(defun js2-parse-with ()
  "Parser for with-statement.  Last matched token must be js2-WITH."
  (let ((pos (js2-current-token-beg))
        obj body pn lp rp)
    (if (js2-must-match js2-LP "msg.no.paren.with")
        (setq lp (js2-current-token-beg)))
    (setq obj (js2-parse-expr))
    (if (js2-must-match js2-RP "msg.no.paren.after.with")
        (setq rp (js2-current-token-beg)))
    (let ((js2-nesting-of-with (1+ js2-nesting-of-with)))
        (setq body (js2-parse-statement)))
    (setq pn (make-js2-with-node :pos pos
                                 :len (- (js2-node-end body) pos)
                                 :object obj
                                 :body body
                                 :lp (js2-relpos lp pos)
                                 :rp (js2-relpos rp pos)))
    (js2-node-add-children pn obj body)
    pn))

(defun js2-parse-const-var ()
  "Parser for var- or const-statement.
Last matched token must be js2-CONST or js2-VAR."
  (let ((tt (js2-current-token-type))
        (pos (js2-current-token-beg))
        expr pn)
    (setq expr (js2-parse-variables tt (js2-current-token-beg))
          pn (make-js2-expr-stmt-node :pos pos
                                      :len (- (js2-node-end expr) pos)
                                      :expr expr))
    (js2-node-add-children pn expr)
    pn))

(defun js2-wrap-with-expr-stmt (pos expr &optional add-child)
  (let ((pn (make-js2-expr-stmt-node :pos pos
                                     :len (js2-node-len expr)
                                     :type (if (js2-inside-function)
                                               js2-EXPR_VOID
                                             js2-EXPR_RESULT)
                                     :expr expr)))
    (if add-child
        (js2-node-add-children pn expr))
    pn))

(defun js2-parse-let-stmt ()
  "Parser for let-statement.  Last matched token must be js2-LET."
  (let ((pos (js2-current-token-beg))
        expr pn)
    (if (= (js2-peek-token) js2-LP)
        ;; let expression in statement context
        (setq expr (js2-parse-let pos 'statement)
              pn (js2-wrap-with-expr-stmt pos expr t))
      ;; else we're looking at a statement like let x=6, y=7;
      (setf expr (js2-parse-variables js2-LET pos)
            pn (js2-wrap-with-expr-stmt pos expr t)
            (js2-node-type pn) js2-EXPR_RESULT))
    pn))

(defun js2-parse-ret-yield ()
  (js2-parse-return-or-yield (js2-current-token-type) nil))

(defconst js2-parse-return-stmt-enders
  (list js2-SEMI js2-RC js2-EOF js2-EOL js2-ERROR js2-RB js2-RP js2-YIELD))

(defsubst js2-now-all-set (before after mask)
  "Return whether or not the bits in the mask have changed to all set.
BEFORE is bits before change, AFTER is bits after change, and MASK is
the mask for bits.  Returns t if all the bits in the mask are set in AFTER
but not BEFORE."
  (and (/= (logand before mask) mask)
       (= (logand after mask) mask)))

(defun js2-parse-return-or-yield (tt expr-context)
  (let ((pos (js2-current-token-beg))
        (end (js2-current-token-end))
        (before js2-end-flags)
        (inside-function (js2-inside-function))
        e ret name)
    (unless inside-function
      (js2-report-error (if (eq tt js2-RETURN)
                            "msg.bad.return"
                          "msg.bad.yield")))
    ;; This is ugly, but we don't want to require a semicolon.
    (unless (memq (js2-peek-token-or-eol) js2-parse-return-stmt-enders)
      (setq e (js2-parse-expr)
            end (js2-node-end e)))
    (cond
     ((eq tt js2-RETURN)
      (js2-set-flag js2-end-flags (if (null e)
                                      js2-end-returns
                                    js2-end-returns-value))
      (setq ret (make-js2-return-node :pos pos
                                      :len (- end pos)
                                      :retval e))
      (js2-node-add-children ret e)
      ;; See if we need a strict mode warning.
      ;; TODO:  The analysis done by `js2-has-consistent-return-usage' is
      ;; more thorough and accurate than this before/after flag check.
      ;; E.g. if there's a finally-block that always returns, we shouldn't
      ;; show a warning generated by inconsistent returns in the catch blocks.
      ;; Basically `js2-has-consistent-return-usage' needs to keep more state,
      ;; so we know which returns/yields to highlight, and we should get rid of
      ;; all the checking in `js2-parse-return-or-yield'.
      (if (and js2-strict-inconsistent-return-warning
               (js2-now-all-set before js2-end-flags
                                (logior js2-end-returns js2-end-returns-value)))
          (js2-add-strict-warning "msg.return.inconsistent" nil pos end)))
     (t
      (unless (js2-inside-function)
        (js2-report-error "msg.bad.yield"))
      (js2-set-flag js2-end-flags js2-end-yields)
      (setq ret (make-js2-yield-node :pos pos
                                     :len (- end pos)
                                     :value e))
      (js2-node-add-children ret e)
      (unless expr-context
        (setq e ret
              ret (js2-wrap-with-expr-stmt pos e t))
      (js2-set-requires-activation)
      (js2-set-is-generator))))
    ;; see if we are mixing yields and value returns.
    (when (and inside-function
               (js2-now-all-set before js2-end-flags
                                (logior js2-end-yields js2-end-returns-value)))
      (setq name (js2-function-name js2-current-script-or-fn))
      (if (zerop (length name))
          (js2-report-error "msg.anon.generator.returns" nil pos (- end pos))
        (js2-report-error "msg.generator.returns" name pos (- end pos))))
    ret))

(defun js2-parse-debugger ()
  (make-js2-keyword-node :type js2-DEBUGGER))

(defun js2-parse-block ()
  "Parser for a curly-delimited statement block.
Last token matched must be `js2-LC'."
  (let ((pos (js2-current-token-beg))
        (pn (make-js2-scope)))
    (js2-push-scope pn)
    (unwind-protect
        (progn
          (js2-parse-statements pn)
          (js2-must-match js2-RC "msg.no.brace.block")
          (setf (js2-node-len pn) (- (js2-current-token-end) pos)))
      (js2-pop-scope))
    pn))

;; For `js2-ERROR' too, to have a node for error recovery to work on.
(defun js2-parse-semi ()
  "Parse a statement or handle an error.
Current token type is `js2-SEMI' or `js2-ERROR'."
  (let ((tt (js2-current-token-type)) pos len)
    (if (eq tt js2-SEMI)
        (make-js2-empty-expr-node :len 1)
      (setq pos (js2-current-token-beg)
            len (- (js2-current-token-end) pos))
      (js2-report-error "msg.syntax" nil pos len)
      (make-js2-error-node :pos pos :len len))))

(defun js2-parse-default-xml-namespace ()
  "Parse a `default xml namespace = <expr>' e4x statement."
  (let ((pos (js2-current-token-beg))
        end len expr unary)
    (js2-must-have-xml)
    (js2-set-requires-activation)
    (setq len (- js2-ts-cursor pos))
    (unless (and (js2-match-token js2-NAME)
                 (string= (js2-current-token-string) "xml"))
      (js2-report-error "msg.bad.namespace" nil pos len))
    (unless (and (js2-match-token js2-NAME)
                 (string= (js2-current-token-string) "namespace"))
      (js2-report-error "msg.bad.namespace" nil pos len))
    (unless (js2-match-token js2-ASSIGN)
      (js2-report-error "msg.bad.namespace" nil pos len))
    (setq expr (js2-parse-expr)
          end (js2-node-end expr)
          unary (make-js2-unary-node :type js2-DEFAULTNAMESPACE
                                     :pos pos
                                     :len (- end pos)
                                     :operand expr))
    (js2-node-add-children unary expr)
    (make-js2-expr-stmt-node :pos pos
                             :len (- end pos)
                             :expr unary)))

(defun js2-record-label (label bundle)
  ;; current token should be colon that `js2-parse-primary-expr' left untouched
  (js2-get-token)
  (let ((name (js2-label-node-name label))
        labeled-stmt
        dup)
    (when (setq labeled-stmt (cdr (assoc name js2-label-set)))
      ;; flag both labels if possible when used in editing mode
      (if (and js2-parse-ide-mode
               (setq dup (js2-get-label-by-name labeled-stmt name)))
          (js2-report-error "msg.dup.label" nil
                            (js2-node-abs-pos dup) (js2-node-len dup)))
      (js2-report-error "msg.dup.label" nil
                        (js2-node-pos label) (js2-node-len label)))
    (js2-labeled-stmt-node-add-label bundle label)
    (js2-node-add-children bundle label)
    ;; Add one reference to the bundle per label in `js2-label-set'
    (push (cons name bundle) js2-label-set)))

(defun js2-parse-name-or-label ()
  "Parser for identifier or label.  Last token matched must be js2-NAME.
Called when we found a name in a statement context.  If it's a label, we gather
up any following labels and the next non-label statement into a
`js2-labeled-stmt-node' bundle and return that.  Otherwise we parse an
expression and return it wrapped in a `js2-expr-stmt-node'."
  (let ((pos (js2-current-token-beg))
        expr stmt bundle
        (continue t))
    ;; set check for label and call down to `js2-parse-primary-expr'
    (setq expr (js2-maybe-parse-label))
    (if (null expr)
        ;; Parse the non-label expression and wrap with expression stmt.
        (js2-wrap-with-expr-stmt pos (js2-parse-expr) t)
      ;; else parsed a label
      (setq bundle (make-js2-labeled-stmt-node :pos pos))
      (js2-record-label expr bundle)
      ;; look for more labels
      (while (and continue (= (js2-get-token) js2-NAME))
        (if (setq expr (js2-maybe-parse-label))
            (js2-record-label expr bundle)
          (setq expr (js2-parse-expr)
                stmt (js2-wrap-with-expr-stmt (js2-node-pos expr) expr t)
                continue nil)
          (js2-auto-insert-semicolon stmt)))
      ;; no more labels; now parse the labeled statement
      (unwind-protect
            (unless stmt
              (let ((js2-labeled-stmt bundle))  ; bind dynamically
                (js2-unget-token)
                (setq stmt (js2-statement-helper))))
        ;; remove the labels for this statement from the global set
        (dolist (label (js2-labeled-stmt-node-labels bundle))
          (setq js2-label-set (remove label js2-label-set))))
      (setf (js2-labeled-stmt-node-stmt bundle) stmt
            (js2-node-len bundle) (- (js2-node-end stmt) pos))
      (js2-node-add-children bundle stmt)
      bundle)))

(defun js2-maybe-parse-label ()
  (assert (= (js2-current-token-type) js2-NAME))
  (let (label-pos
        (next-tt (js2-get-token))
        (label-end (js2-current-token-end)))
    ;; Do not consume colon, it is used as unwind indicator
    ;; to return to statementHelper.
    (js2-unget-token)
    (if (= next-tt js2-COLON)
        (prog2
            (setq label-pos (js2-current-token-beg))
            (make-js2-label-node :pos label-pos
                                 :len (- label-end label-pos)
                                 :name (js2-current-token-string))
          (js2-set-face label-pos
                        label-end
                        'font-lock-variable-name-face 'record))
      ;; Backtrack from the name token, too.
      (js2-unget-token)
      nil)))

(defun js2-parse-expr-stmt ()
  "Default parser in statement context, if no recognized statement found."
  (js2-wrap-with-expr-stmt (js2-current-token-beg)
                           (progn
                             (js2-unget-token)
                             (js2-parse-expr)) t))

(defun js2-parse-variables (decl-type pos)
  "Parse a comma-separated list of variable declarations.
Could be a 'var', 'const' or 'let' expression, possibly in a for-loop initializer.

DECL-TYPE is a token value: either VAR, CONST, or LET depending on context.
For 'var' or 'const', the keyword should be the token last scanned.

POS is the position where the node should start. It's sometimes the
var/const/let keyword, and other times the beginning of the first token
in the first variable declaration.

Returns the parsed `js2-var-decl-node' expression node."
  (let* ((result (make-js2-var-decl-node :decl-type decl-type
                                         :pos pos))
         destructuring kid-pos tt init name end nbeg nend vi
         (continue t))
    ;; Example:
    ;; var foo = {a: 1, b: 2}, bar = [3, 4];
    ;; var {b: s2, a: s1} = foo, x = 6, y, [s3, s4] = bar;
    ;; var {a, b} = baz;
    (while continue
      (setq destructuring nil
            name nil
            tt (js2-get-token)
            kid-pos (js2-current-token-beg)
            end (js2-current-token-end)
            init nil)
      (if (or (= tt js2-LB) (= tt js2-LC))
          ;; Destructuring assignment, e.g., var [a, b] = ...
          (setq destructuring (js2-parse-destruct-primary-expr)
                end (js2-node-end destructuring))
        ;; Simple variable name
        (js2-unget-token)
        (when (js2-must-match js2-NAME "msg.bad.var")
          (setq name (js2-create-name-node)
                nbeg (js2-current-token-beg)
                nend (js2-current-token-end)
                end nend)
          (js2-define-symbol decl-type (js2-current-token-string) name js2-in-for-init)))
      (when (js2-match-token js2-ASSIGN)
        (setq init (js2-parse-assign-expr)
              end (js2-node-end init))
        (js2-record-imenu-functions init name))
      (when name
        (js2-set-face nbeg nend (if (js2-function-node-p init)
                                    'font-lock-function-name-face
                                  'font-lock-variable-name-face)
                      'record))
      (setq vi (make-js2-var-init-node :pos kid-pos
                                       :len (- end kid-pos)
                                       :type decl-type))
      (if destructuring
          (progn
            (if (and (null init) (not js2-in-for-init))
                (js2-report-error "msg.destruct.assign.no.init"))
            (js2-define-destruct-symbols destructuring
                                         decl-type
                                         'font-lock-variable-name-face)
            (setf (js2-var-init-node-target vi) destructuring))
        (setf (js2-var-init-node-target vi) name))
      (setf (js2-var-init-node-initializer vi) init)
      (js2-node-add-children vi name destructuring init)
      (js2-block-node-push result vi)
      (unless (js2-match-token js2-COMMA)
        (setq continue nil)))
    (setf (js2-node-len result) (- end pos))
    result))

(defun js2-parse-let (pos &optional stmt-p)
  "Parse a let expression or statement.
A let-expression is of the form `let (vars) expr'.
A let-statment is of the form `let (vars) {statements}'.
The third form of let is a variable declaration list, handled
by `js2-parse-variables'."
  (let ((pn (make-js2-let-node :pos pos))
        beg vars body)
    (if (js2-must-match js2-LP "msg.no.paren.after.let")
        (setf (js2-let-node-lp pn) (- (js2-current-token-beg) pos)))
    (js2-push-scope pn)
    (unwind-protect
        (progn
          (setq vars (js2-parse-variables js2-LET (js2-current-token-beg)))
          (if (js2-must-match js2-RP "msg.no.paren.let")
              (setf (js2-let-node-rp pn) (- (js2-current-token-beg) pos)))
          (if (and stmt-p (eq (js2-get-token) js2-LC))
              ;; let statement
              (progn
                (setf beg (js2-current-token-beg)  ; position stmt at LC
                      body (js2-parse-statements))
                (js2-must-match js2-RC "msg.no.curly.let")
                (setf (js2-node-len body) (- (js2-current-token-end) beg)
                      (js2-node-len pn) (- (js2-current-token-end) pos)
                      (js2-let-node-body pn) body
                      (js2-node-type pn) js2-LET))
            ;; let expression
            (js2-unget-token)
            (setf body (js2-parse-expr)
                  (js2-node-len pn) (- (js2-node-end body) pos)
                  (js2-let-node-body pn) body))
          (js2-node-add-children pn vars body))
      (js2-pop-scope))
    pn))

(defun js2-define-new-symbol (decl-type name node &optional scope)
  (js2-scope-put-symbol (or scope js2-current-scope)
                        name
                        (make-js2-symbol decl-type name node)))

(defun js2-define-symbol (decl-type name &optional node ignore-not-in-block)
  "Define a symbol in the current scope.
If NODE is non-nil, it is the AST node associated with the symbol."
  (let* ((defining-scope (js2-get-defining-scope js2-current-scope name))
         (symbol (if defining-scope
                     (js2-scope-get-symbol defining-scope name)))
         (sdt (if symbol (js2-symbol-decl-type symbol) -1)))
    (cond
     ((and symbol ; already defined
           (or (= sdt js2-CONST) ; old version is const
               (= decl-type js2-CONST) ; new version is const
               ;; two let-bound vars in this block have same name
               (and (= sdt js2-LET)
                    (eq defining-scope js2-current-scope))))
      (js2-report-error
       (cond
        ((= sdt js2-CONST) "msg.const.redecl")
        ((= sdt js2-LET) "msg.let.redecl")
        ((= sdt js2-VAR) "msg.var.redecl")
        ((= sdt js2-FUNCTION) "msg.function.redecl")
        (t "msg.parm.redecl"))
       name))
     ((= decl-type js2-LET)
      (if (and (not ignore-not-in-block)
               (or (= (js2-node-type js2-current-scope) js2-IF)
                   (js2-loop-node-p js2-current-scope)))
          (js2-report-error "msg.let.decl.not.in.block")
        (js2-define-new-symbol decl-type name node)))
     ((or (= decl-type js2-VAR)
          (= decl-type js2-CONST)
          (= decl-type js2-FUNCTION))
      (if symbol
          (if (and js2-strict-var-redeclaration-warning (= sdt js2-VAR))
              (js2-add-strict-warning "msg.var.redecl" name)
            (if (and js2-strict-var-hides-function-arg-warning (= sdt js2-LP))
                (js2-add-strict-warning "msg.var.hides.arg" name)))
        (js2-define-new-symbol decl-type name node
                               js2-current-script-or-fn)))
     ((= decl-type js2-LP)
      (if symbol
          ;; must be duplicate parameter. Second parameter hides the
          ;; first, so go ahead and add the second pararameter
          (js2-report-warning "msg.dup.parms" name))
      (js2-define-new-symbol decl-type name node))
     (t (js2-code-bug)))))

(defun js2-parse-expr (&optional oneshot)
  (let* ((pn (js2-parse-assign-expr))
         (pos (js2-node-pos pn))
         left
         right
         op-pos)
    (while (and (not oneshot)
                (js2-match-token js2-COMMA))
      (setq op-pos (- (js2-current-token-beg) pos))  ; relative
      (if (= (js2-peek-token) js2-YIELD)
          (js2-report-error "msg.yield.parenthesized"))
      (setq right (js2-parse-assign-expr)
            left pn
            pn (make-js2-infix-node :type js2-COMMA
                                    :pos pos
                                    :len (- js2-ts-cursor pos)
                                    :op-pos op-pos
                                    :left left
                                    :right right))
      (js2-node-add-children pn left right))
    pn))

(defun js2-parse-assign-expr ()
  (let ((tt (js2-get-token))
        (pos (js2-current-token-beg))
        pn left right op-pos
        ts-state recorded-identifiers)
    (if (= tt js2-YIELD)
        (js2-parse-return-or-yield tt t)
      ;; Save the tokenizer state in case we find an arrow function
      ;; and have to rewind.
      (setq ts-state (make-js2-ts-state)
            recorded-identifiers js2-recorded-identifiers)
      ;; not yield - parse assignment expression
      (setq pn (js2-parse-cond-expr)
            tt (js2-get-token))
      (cond
       ((and (<= js2-first-assign tt)
             (<= tt js2-last-assign))
        ;; tt express assignment (=, |=, ^=, ..., %=)
        (setq op-pos (- (js2-current-token-beg) pos)  ; relative
              left pn)
        (setq right (js2-parse-assign-expr)
              pn (make-js2-assign-node :type tt
                                       :pos pos
                                       :len (- (js2-node-end right) pos)
                                       :op-pos op-pos
                                       :left left
                                       :right right))
        (when js2-parse-ide-mode
          (js2-highlight-assign-targets pn left right)
          (js2-record-imenu-functions right left))
        ;; do this last so ide checks above can use absolute positions
        (js2-node-add-children pn left right))
       ((and (= tt js2-ARROW)
             (>= js2-language-version 200))
        (js2-ts-seek ts-state)
        (setq js2-recorded-identifiers recorded-identifiers)
        (setq pn (js2-parse-function 'FUNCTION_ARROW (js2-current-token-beg))))
       (t
        (js2-unget-token)))
      pn)))

(defun js2-parse-cond-expr ()
  (let ((pos (js2-current-token-beg))
        (pn (js2-parse-or-expr))
        test-expr
        if-true
        if-false
        q-pos
        c-pos)
    (when (js2-match-token js2-HOOK)
      (setq q-pos (- (js2-current-token-beg) pos)
            if-true (js2-parse-assign-expr))
      (js2-must-match js2-COLON "msg.no.colon.cond")
      (setq c-pos (- (js2-current-token-beg) pos)
            if-false (js2-parse-assign-expr)
            test-expr pn
            pn (make-js2-cond-node :pos pos
                                   :len (- (js2-node-end if-false) pos)
                                   :test-expr test-expr
                                   :true-expr if-true
                                   :false-expr if-false
                                   :q-pos q-pos
                                   :c-pos c-pos))
      (js2-node-add-children pn test-expr if-true if-false))
    pn))

(defun js2-make-binary (type left parser)
  "Helper for constructing a binary-operator AST node.
LEFT is the left-side-expression, already parsed, and the
binary operator should have just been matched.
PARSER is a function to call to parse the right operand,
or a `js2-node' struct if it has already been parsed.
FIXME: The latter option is unused?"
  (let* ((pos (js2-node-pos left))
         (op-pos (- (js2-current-token-beg) pos))
         (right (if (js2-node-p parser)
                    parser
                  (js2-get-token)
                  (funcall parser)))
         (pn (make-js2-infix-node :type type
                                  :pos pos
                                  :len (- (js2-node-end right) pos)
                                  :op-pos op-pos
                                  :left left
                                  :right right)))
    (js2-node-add-children pn left right)
    pn))

(defun js2-parse-or-expr ()
  (let ((pn (js2-parse-and-expr)))
    (when (js2-match-token js2-OR)
      (setq pn (js2-make-binary js2-OR
                                pn
                                'js2-parse-or-expr)))
    pn))

(defun js2-parse-and-expr ()
  (let ((pn (js2-parse-bit-or-expr)))
    (when (js2-match-token js2-AND)
      (setq pn (js2-make-binary js2-AND
                                pn
                                'js2-parse-and-expr)))
    pn))

(defun js2-parse-bit-or-expr ()
  (let ((pn (js2-parse-bit-xor-expr)))
    (while (js2-match-token js2-BITOR)
      (setq pn (js2-make-binary js2-BITOR
                                pn
                                'js2-parse-bit-xor-expr)))
    pn))

(defun js2-parse-bit-xor-expr ()
  (let ((pn (js2-parse-bit-and-expr)))
    (while (js2-match-token js2-BITXOR)
      (setq pn (js2-make-binary js2-BITXOR
                                pn
                                'js2-parse-bit-and-expr)))
    pn))

(defun js2-parse-bit-and-expr ()
  (let ((pn (js2-parse-eq-expr)))
    (while (js2-match-token js2-BITAND)
      (setq pn (js2-make-binary js2-BITAND
                                pn
                                'js2-parse-eq-expr)))
    pn))

(defconst js2-parse-eq-ops
  (list js2-EQ js2-NE js2-SHEQ js2-SHNE))

(defun js2-parse-eq-expr ()
  (let ((pn (js2-parse-rel-expr))
        tt)
    (while (memq (setq tt (js2-get-token)) js2-parse-eq-ops)
      (setq pn (js2-make-binary tt
                                pn
                                'js2-parse-rel-expr)))
    (js2-unget-token)
    pn))

(defconst js2-parse-rel-ops
  (list js2-IN js2-INSTANCEOF js2-LE js2-LT js2-GE js2-GT))

(defun js2-parse-rel-expr ()
  (let ((pn (js2-parse-shift-expr))
        (continue t)
        tt)
    (while continue
      (setq tt (js2-get-token))
      (cond
       ((and js2-in-for-init (= tt js2-IN))
        (js2-unget-token)
        (setq continue nil))
       ((memq tt js2-parse-rel-ops)
        (setq pn (js2-make-binary tt pn 'js2-parse-shift-expr)))
       (t
        (js2-unget-token)
        (setq continue nil))))
    pn))

(defconst js2-parse-shift-ops
  (list js2-LSH js2-URSH js2-RSH))

(defun js2-parse-shift-expr ()
  (let ((pn (js2-parse-add-expr))
        tt
        (continue t))
    (while continue
      (setq tt (js2-get-token))
      (if (memq tt js2-parse-shift-ops)
          (setq pn (js2-make-binary tt pn 'js2-parse-add-expr))
        (js2-unget-token)
        (setq continue nil)))
    pn))

(defun js2-parse-add-expr ()
  (let ((pn (js2-parse-mul-expr))
        tt
        (continue t))
    (while continue
      (setq tt (js2-get-token))
      (if (or (= tt js2-ADD) (= tt js2-SUB))
          (setq pn (js2-make-binary tt pn 'js2-parse-mul-expr))
        (js2-unget-token)
        (setq continue nil)))
    pn))

(defconst js2-parse-mul-ops
  (list js2-MUL js2-DIV js2-MOD))

(defun js2-parse-mul-expr ()
  (let ((pn (js2-parse-unary-expr))
        tt
        (continue t))
    (while continue
      (setq tt (js2-get-token))
      (if (memq tt js2-parse-mul-ops)
          (setq pn (js2-make-binary tt pn 'js2-parse-unary-expr))
        (js2-unget-token)
        (setq continue nil)))
    pn))

(defun js2-make-unary (type parser &rest args)
  "Make a unary node of type TYPE.
PARSER is either a node (for postfix operators) or a function to call
to parse the operand (for prefix operators)."
  (let* ((pos (js2-current-token-beg))
         (postfix (js2-node-p parser))
         (expr (if postfix
                   parser
                 (apply parser args)))
         end
         pn)
    (if postfix  ; e.g. i++
        (setq pos (js2-node-pos expr)
              end (js2-current-token-end))
      (setq end (js2-node-end expr)))
    (setq pn (make-js2-unary-node :type type
                                  :pos pos
                                  :len (- end pos)
                                  :operand expr))
    (js2-node-add-children pn expr)
    pn))

(defconst js2-incrementable-node-types
  (list js2-NAME js2-GETPROP js2-GETELEM js2-GET_REF js2-CALL)
  "Node types that can be the operand of a ++ or -- operator.")

(defun js2-check-bad-inc-dec (tt beg end unary)
  (unless (memq (js2-node-type (js2-unary-node-operand unary))
                js2-incrementable-node-types)
    (js2-report-error (if (= tt js2-INC)
                          "msg.bad.incr"
                        "msg.bad.decr")
                      nil beg (- end beg))))

(defun js2-parse-unary-expr ()
  (let ((tt (js2-current-token-type))
        pn expr beg end)
    (cond
     ((or (= tt js2-VOID)
          (= tt js2-NOT)
          (= tt js2-BITNOT)
          (= tt js2-TYPEOF))
      (js2-get-token)
      (js2-make-unary tt 'js2-parse-unary-expr))
     ((= tt js2-ADD)
      (js2-get-token)
      ;; Convert to special POS token in decompiler and parse tree
      (js2-make-unary js2-POS 'js2-parse-unary-expr))
     ((= tt js2-SUB)
      (js2-get-token)
      ;; Convert to special NEG token in decompiler and parse tree
      (js2-make-unary js2-NEG 'js2-parse-unary-expr))
     ((or (= tt js2-INC)
          (= tt js2-DEC))
      (js2-get-token)
      (prog1
          (setq beg (js2-current-token-beg)
                end (js2-current-token-end)
                expr (js2-make-unary tt 'js2-parse-member-expr t))
        (js2-check-bad-inc-dec tt beg end expr)))
     ((= tt js2-DELPROP)
      (js2-get-token)
      (js2-make-unary js2-DELPROP 'js2-parse-unary-expr))
     ((= tt js2-ERROR)
      (js2-get-token)
      (make-js2-error-node))  ; try to continue
     ((and (= tt js2-LT)
           js2-compiler-xml-available)
      ;; XML stream encountered in expression.
      (js2-parse-member-expr-tail t (js2-parse-xml-initializer)))
     (t
      (setq pn (js2-parse-member-expr t)
            ;; Don't look across a newline boundary for a postfix incop.
            tt (js2-peek-token-or-eol))
      (when (or (= tt js2-INC) (= tt js2-DEC))
        (js2-get-token)
        (setf expr pn
              pn (js2-make-unary tt expr))
        (js2-node-set-prop pn 'postfix t)
        (js2-check-bad-inc-dec tt (js2-current-token-beg) (js2-current-token-end) pn))
      pn))))

(defun js2-parse-xml-initializer ()
  "Parse an E4X XML initializer.
I'm parsing it the way Rhino parses it, but without the tree-rewriting.
Then I'll postprocess the result, depending on whether we're in IDE
mode or codegen mode, and generate the appropriate rewritten AST.
IDE mode uses a rich AST that models the XML structure.  Codegen mode
just concatenates everything and makes a new XML or XMLList out of it."
  (let ((tt (js2-get-first-xml-token))
        pn-xml pn expr kids expr-pos
        (continue t)
        (first-token t))
    (when (not (or (= tt js2-XML) (= tt js2-XMLEND)))
      (js2-report-error "msg.syntax"))
    (setq pn-xml (make-js2-xml-node))
    (while continue
      (if first-token
          (setq first-token nil)
        (setq tt (js2-get-next-xml-token)))
      (cond
       ;; js2-XML means we found a {expr} in the XML stream.
       ;; The token string is the XML up to the left-curly.
       ((= tt js2-XML)
        (push (make-js2-string-node :pos (js2-current-token-beg)
                                    :len (- js2-ts-cursor (js2-current-token-beg)))
              kids)
        (js2-must-match js2-LC "msg.syntax")
        (setq expr-pos js2-ts-cursor
              expr (if (eq (js2-peek-token) js2-RC)
                       (make-js2-empty-expr-node :pos expr-pos)
                     (js2-parse-expr)))
        (js2-must-match js2-RC "msg.syntax")
        (setq pn (make-js2-xml-js-expr-node :pos (js2-node-pos expr)
                                            :len (js2-node-len expr)
                                            :expr expr))
        (js2-node-add-children pn expr)
        (push pn kids))
       ;; a js2-XMLEND token means we hit the final close-tag.
       ((= tt js2-XMLEND)
        (push (make-js2-string-node :pos (js2-current-token-beg)
                                    :len (- js2-ts-cursor (js2-current-token-beg)))
              kids)
        (dolist (kid (nreverse kids))
          (js2-block-node-push pn-xml kid))
        (setf (js2-node-len pn-xml) (- js2-ts-cursor
                                       (js2-node-pos pn-xml))
              continue nil))
       (t
        (js2-report-error "msg.syntax")
        (setq continue nil))))
    pn-xml))


(defun js2-parse-argument-list ()
  "Parse an argument list and return it as a Lisp list of nodes.
Returns the list in reverse order.  Consumes the right-paren token."
  (let (result)
    (unless (js2-match-token js2-RP)
      (loop do
            (if (= (js2-peek-token) js2-YIELD)
                (js2-report-error "msg.yield.parenthesized"))
            (push (js2-parse-assign-expr) result)
            while
            (js2-match-token js2-COMMA))
      (js2-must-match js2-RP "msg.no.paren.arg")
      result)))

(defun js2-parse-member-expr (&optional allow-call-syntax)
  (let ((tt (js2-current-token-type))
        pn pos target args beg end init)
    (if (/= tt js2-NEW)
        (setq pn (js2-parse-primary-expr))
      ;; parse a 'new' expression
      (js2-get-token)
      (setq pos (js2-current-token-beg)
            beg pos
            target (js2-parse-member-expr)
            end (js2-node-end target)
            pn (make-js2-new-node :pos pos
                                  :target target
                                  :len (- end pos)))
      (js2-node-add-children pn target)
      (when (js2-match-token js2-LP)
        ;; Add the arguments to pn, if any are supplied.
        (setf beg pos  ; start of "new" keyword
              pos (js2-current-token-beg)
              args (nreverse (js2-parse-argument-list))
              (js2-new-node-args pn) args
              end (js2-current-token-end)
              (js2-new-node-lp pn) (- pos beg)
              (js2-new-node-rp pn) (- end 1 beg))
        (apply #'js2-node-add-children pn args))
      (when (and js2-allow-rhino-new-expr-initializer
                 (js2-match-token js2-LC))
        (setf init (js2-parse-object-literal)
              end (js2-node-end init)
              (js2-new-node-initializer pn) init)
        (js2-node-add-children pn init))
        (setf (js2-node-len pn) (- end beg)))  ; end outer if
    (js2-parse-member-expr-tail allow-call-syntax pn)))

(defun js2-parse-member-expr-tail (allow-call-syntax pn)
  "Parse a chain of property/array accesses or function calls.
Includes parsing for E4X operators like `..' and `.@'.
If ALLOW-CALL-SYNTAX is nil, stops when we encounter a left-paren.
Returns an expression tree that includes PN, the parent node."
  (let (tt
        (continue t))
    (while continue
      (setq tt (js2-get-token))
      (cond
       ((or (= tt js2-DOT) (= tt js2-DOTDOT))
        (setq pn (js2-parse-property-access tt pn)))
       ((= tt js2-DOTQUERY)
        (setq pn (js2-parse-dot-query pn)))
       ((= tt js2-LB)
        (setq pn (js2-parse-element-get pn)))
       ((= tt js2-LP)
        (if allow-call-syntax
            (setq pn (js2-parse-function-call pn))
          (js2-unget-token)
          (setq continue nil)))
       (t
        (js2-unget-token)
        (setq continue nil))))
    (if (>= js2-highlight-level 2)
        (js2-parse-highlight-member-expr-node pn))
    pn))

(defun js2-parse-dot-query (pn)
  "Parse a dot-query expression, e.g. foo.bar.(@name == 2)
Last token parsed must be `js2-DOTQUERY'."
  (let ((pos (js2-node-pos pn))
        op-pos expr end)
    (js2-must-have-xml)
    (js2-set-requires-activation)
    (setq op-pos (js2-current-token-beg)
          expr (js2-parse-expr)
          end (js2-node-end expr)
          pn (make-js2-xml-dot-query-node :left pn
                                          :pos pos
                                          :op-pos op-pos
                                          :right expr))
    (js2-node-add-children pn
                           (js2-xml-dot-query-node-left pn)
                           (js2-xml-dot-query-node-right pn))
    (if (js2-must-match js2-RP "msg.no.paren")
        (setf (js2-xml-dot-query-node-rp pn) (js2-current-token-beg)
              end (js2-current-token-end)))
    (setf (js2-node-len pn) (- end pos))
    pn))

(defun js2-parse-element-get (pn)
  "Parse an element-get expression, e.g. foo[bar].
Last token parsed must be `js2-RB'."
  (let ((lb (js2-current-token-beg))
        (pos (js2-node-pos pn))
        rb expr)
    (setq expr (js2-parse-expr))
    (if (js2-must-match js2-RB "msg.no.bracket.index")
        (setq rb (js2-current-token-beg)))
    (setq pn (make-js2-elem-get-node :target pn
                                     :pos pos
                                     :element expr
                                     :lb (js2-relpos lb pos)
                                     :rb (js2-relpos rb pos)
                                     :len (- (js2-current-token-end) pos)))
    (js2-node-add-children pn
                           (js2-elem-get-node-target pn)
                           (js2-elem-get-node-element pn))
    pn))

(defun js2-parse-function-call (pn)
  (let (args
        (pos (js2-node-pos pn)))
    (setq pn (make-js2-call-node :pos pos
                                 :target pn
                                 :lp (- (js2-current-token-beg) pos)))
    (js2-node-add-children pn (js2-call-node-target pn))
    ;; Add the arguments to pn, if any are supplied.
    (setf args (nreverse (js2-parse-argument-list))
          (js2-call-node-rp pn) (- (js2-current-token-beg) pos)
          (js2-call-node-args pn) args)
    (apply #'js2-node-add-children pn args)
    (setf (js2-node-len pn) (- js2-ts-cursor pos))
    pn))

(defun js2-parse-property-access (tt pn)
  "Parse a property access, XML descendants access, or XML attr access."
  (let ((member-type-flags 0)
        (dot-pos (js2-current-token-beg))
        (dot-len (if (= tt js2-DOTDOT) 2 1))
        name
        ref  ; right side of . or .. operator
        result)
    (when (= tt js2-DOTDOT)
      (js2-must-have-xml)
      (setq member-type-flags js2-descendants-flag))
    (if (not js2-compiler-xml-available)
        (progn
          (js2-must-match-prop-name "msg.no.name.after.dot")
          (setq name (js2-create-name-node t js2-GETPROP)
                result (make-js2-prop-get-node :left pn
                                               :pos (js2-current-token-beg)
                                               :right name
                                               :len (js2-current-token-len)))
          (js2-node-add-children result pn name)
          result)
      ;; otherwise look for XML operators
      (setf result (if (= tt js2-DOT)
                       (make-js2-prop-get-node)
                     (make-js2-infix-node :type js2-DOTDOT))
            (js2-node-pos result) (js2-node-pos pn)
            (js2-infix-node-op-pos result) dot-pos
            (js2-infix-node-left result) pn  ; do this after setting position
            tt (js2-next-token))
      (cond
       ;; needed for generator.throw()
       ((= tt js2-THROW)
        (setq ref (js2-parse-property-name nil nil member-type-flags)))
       ;; handles: name, ns::name, ns::*, ns::[expr]
       ((js2-valid-prop-name-token tt)
        (setq ref (js2-parse-property-name -1 nil member-type-flags)))
       ;; handles: *, *::name, *::*, *::[expr]
       ((= tt js2-MUL)
        (setq ref (js2-parse-property-name nil "*" member-type-flags)))
       ;; handles: '@attr', '@ns::attr', '@ns::*', '@ns::[expr]', etc.
       ((= tt js2-XMLATTR)
        (setq result (js2-parse-attribute-access)))
       (t
        (js2-report-error "msg.no.name.after.dot" nil dot-pos dot-len)))
      (if ref
          (setf (js2-node-len result) (- (js2-node-end ref)
                                         (js2-node-pos result))
                (js2-infix-node-right result) ref))
      (if (js2-infix-node-p result)
          (js2-node-add-children result
                                 (js2-infix-node-left result)
                                 (js2-infix-node-right result)))
      result)))

(defun js2-parse-attribute-access ()
  "Parse an E4X XML attribute expression.
This includes expressions of the forms:

  @attr      @ns::attr     @ns::*
  @*         @*::attr      @*::*
  @[expr]    @*::[expr]    @ns::[expr]

Called if we peeked an '@' token."
  (let ((tt (js2-next-token))
        (at-pos (js2-current-token-beg)))
    (cond
     ;; handles: @name, @ns::name, @ns::*, @ns::[expr]
     ((js2-valid-prop-name-token tt)
      (js2-parse-property-name at-pos nil 0))
     ;; handles: @*, @*::name, @*::*, @*::[expr]
     ((= tt js2-MUL)
      (js2-parse-property-name (js2-current-token-beg) "*" 0))
     ;; handles @[expr]
     ((= tt js2-LB)
      (js2-parse-xml-elem-ref at-pos))
     (t
      (js2-report-error "msg.no.name.after.xmlAttr")
      ;; Avoid cascaded errors that happen if we make an error node here.
      (js2-parse-property-name (js2-current-token-beg) "" 0)))))

(defun js2-parse-property-name (at-pos s member-type-flags)
  "Check if :: follows name in which case it becomes qualified name.

AT-POS is a natural number if we just read an '@' token, else nil.
S is the name or string that was matched:  an identifier, 'throw' or '*'.
MEMBER-TYPE-FLAGS is a bit set tracking whether we're a '.' or '..' child.

Returns a `js2-xml-ref-node' if it's an attribute access, a child of a '..'
operator, or the name is followed by ::.  For a plain name, returns a
`js2-name-node'.  Returns a `js2-error-node' for malformed XML expressions."
  (let ((pos (or at-pos (js2-current-token-beg)))
        colon-pos
        (name (js2-create-name-node t (js2-current-token-type) s))
        ns tt pn)
    (catch 'return
      (when (js2-match-token js2-COLONCOLON)
        (setq ns name
              colon-pos (js2-current-token-beg)
              tt (js2-next-token))
        (cond
         ;; handles name::name
         ((js2-valid-prop-name-token tt)
          (setq name (js2-create-name-node)))
         ;; handles name::*
         ((= tt js2-MUL)
          (setq name (js2-create-name-node nil nil "*")))
         ;; handles name::[expr]
         ((= tt js2-LB)
          (throw 'return (js2-parse-xml-elem-ref at-pos ns colon-pos)))
         (t
          (js2-report-error "msg.no.name.after.coloncolon"))))
      (if (and (null ns) (zerop member-type-flags))
          name
        (prog1
            (setq pn
                  (make-js2-xml-prop-ref-node :pos pos
                                              :len (- (js2-node-end name) pos)
                                              :at-pos at-pos
                                              :colon-pos colon-pos
                                              :propname name))
          (js2-node-add-children pn name))))))

(defun js2-parse-xml-elem-ref (at-pos &optional namespace colon-pos)
  "Parse the [expr] portion of an xml element reference.
For instance, @[expr], @*::[expr], or ns::[expr]."
  (let* ((lb (js2-current-token-beg))
         (pos (or at-pos lb))
         rb
         (expr (js2-parse-expr))
         (end (js2-node-end expr))
         pn)
    (if (js2-must-match js2-RB "msg.no.bracket.index")
        (setq rb (js2-current-token-beg)
              end (js2-current-token-end)))
    (prog1
        (setq pn
              (make-js2-xml-elem-ref-node :pos pos
                                          :len (- end pos)
                                          :namespace namespace
                                          :colon-pos colon-pos
                                          :at-pos at-pos
                                          :expr expr
                                          :lb (js2-relpos lb pos)
                                          :rb (js2-relpos rb pos)))
      (js2-node-add-children pn namespace expr))))

(defun js2-parse-destruct-primary-expr ()
  (let ((js2-is-in-destructuring t))
    (js2-parse-primary-expr)))

(defun js2-parse-primary-expr ()
  "Parse a literal (leaf) expression of some sort.
Includes complex literals such as functions, object-literals,
array-literals, array comprehensions and regular expressions."
  (let (pn      ; parent node  (usually return value)
        tt
        px-pos  ; paren-expr pos
        len
        flags   ; regexp flags
        expr)
    (setq tt (js2-current-token-type))
    (cond
     ((= tt js2-FUNCTION)
      (js2-parse-function-expr))
     ((= tt js2-LB)
      (js2-parse-array-literal))
     ((= tt js2-LC)
      (js2-parse-object-literal))
     ((= tt js2-LET)
      (js2-parse-let (js2-current-token-beg)))
     ((= tt js2-LP)
      (setq px-pos (js2-current-token-beg)
            expr (js2-parse-expr))
      (js2-must-match js2-RP "msg.no.paren")
      (setq pn (make-js2-paren-node :pos px-pos
                                    :expr expr
                                    :len (- (js2-current-token-end) px-pos)))
      (js2-node-add-children pn (js2-paren-node-expr pn))
      pn)
     ((= tt js2-XMLATTR)
      (js2-must-have-xml)
      (js2-parse-attribute-access))
     ((= tt js2-NAME)
      (js2-parse-name tt))
     ((= tt js2-NUMBER)
      (make-js2-number-node))
     ((= tt js2-STRING)
      (prog1
          (make-js2-string-node)
        (js2-record-face 'font-lock-string-face)))
     ((or (= tt js2-DIV) (= tt js2-ASSIGN_DIV))
      ;; Got / or /= which in this context means a regexp literal
      (setq px-pos (js2-current-token-beg))
      (setq flags (js2-read-regexp tt))
      (prog1
          (make-js2-regexp-node :pos px-pos
                                :len (- js2-ts-cursor px-pos)
                                :value (js2-current-token-string)
                                :flags flags)
        (js2-set-face px-pos js2-ts-cursor 'font-lock-string-face 'record)
        (js2-record-text-property px-pos js2-ts-cursor 'syntax-table '(2))))
     ((or (= tt js2-NULL)
          (= tt js2-THIS)
          (= tt js2-FALSE)
          (= tt js2-TRUE))
      (make-js2-keyword-node :type tt))
     ((= tt js2-RP)
      ;; Not valid expression syntax, but this is valid in an arrow
      ;; function with no params: () => body.
      (if (eq (js2-peek-token) js2-ARROW)
          (progn
            (js2-unget-token)  ; Put back the right paren.
            ;; Return whatever, it will hopefully be rewinded and
            ;; reparsed when we reach the =>.
            (make-js2-keyword-node :type js2-NULL))
        (js2-report-error "msg.syntax")
        (make-js2-error-node)))
     ((= tt js2-TRIPLEDOT)
      ;; Likewise, only valid in an arrow function with a rest param.
      (if (and (js2-match-token js2-NAME)
               (js2-match-token js2-RP)
               (eq (js2-peek-token) js2-ARROW))
          (progn
            (js2-unget-token)  ; Put back the right paren.
            ;; See the previous case.
            (make-js2-keyword-node :type js2-NULL))
        (js2-report-error "msg.syntax")
        (make-js2-error-node)))
     ((= tt js2-RESERVED)
      (js2-report-error "msg.reserved.id")
      (make-js2-name-node))
     ((= tt js2-ERROR)
      ;; the scanner or one of its subroutines reported the error.
      (make-js2-error-node))
     ((= tt js2-EOF)
      (setq px-pos (point-at-bol)
            len (- js2-ts-cursor px-pos))
      (js2-report-error "msg.unexpected.eof" nil px-pos len)
      (make-js2-error-node :pos (1- js2-ts-cursor)))
     (t
      (js2-report-error "msg.syntax")
      (make-js2-error-node)))))

(defun js2-parse-name (_tt)
  (let ((name (js2-current-token-string))
        node)
    (setq node (if js2-compiler-xml-available
                   (js2-parse-property-name nil name 0)
                 (js2-create-name-node 'check-activation nil name)))
    (if js2-highlight-external-variables
        (js2-record-name-node node))
    node))

(defun js2-parse-warn-trailing-comma (msg pos elems comma-pos)
  (js2-add-strict-warning
   msg nil
   ;; back up from comma to beginning of line or array/objlit
   (max (if elems
            (js2-node-pos (car elems))
          pos)
        (save-excursion
          (goto-char comma-pos)
          (back-to-indentation)
          (point)))
   comma-pos))

(defun js2-parse-array-literal ()
  (let ((pos (js2-current-token-beg))
        (after-lb-or-comma t)
        after-comma tt elems pn
        (continue t))
    (unless js2-is-in-destructuring
        (js2-push-scope (make-js2-scope))) ; for array comp
    (while continue
      (setq tt (js2-get-token))
      (cond
       ;; comma
       ((= tt js2-COMMA)
        (setq after-comma (js2-current-token-end))
        (if (not after-lb-or-comma)
            (setq after-lb-or-comma t)
          (push nil elems)))
       ;; end of array
       ((or (= tt js2-RB)
            (= tt js2-EOF))  ; prevent infinite loop
        (if (= tt js2-EOF)
            (js2-report-error "msg.no.bracket.arg" nil pos))
        (setq continue nil
              pn (make-js2-array-node :pos pos
                                      :len (- js2-ts-cursor pos)
                                      :elems (nreverse elems)))
        (apply #'js2-node-add-children pn (js2-array-node-elems pn))
        (when (and after-comma (not js2-is-in-destructuring))
          (js2-parse-warn-trailing-comma "msg.array.trailing.comma"
                                         pos elems after-comma)))
       ;; destructuring binding
       (js2-is-in-destructuring
        (push (if (or (= tt js2-LC)
                      (= tt js2-LB)
                      (= tt js2-NAME))
                  ;; [a, b, c] | {a, b, c} | {a:x, b:y, c:z} | a
                  (js2-parse-destruct-primary-expr)
                ;; invalid pattern
                (js2-report-error "msg.bad.var")
                (make-js2-error-node))
              elems)
        (setq after-lb-or-comma nil
              after-comma nil))
       ;; array comp
       ((and (>= js2-language-version 170)
             (= tt js2-FOR)          ; check for array comprehension
             (not after-lb-or-comma) ; "for" can't follow a comma
             elems                   ; must have at least 1 element
             (not (cdr elems)))      ; but no 2nd element
        (js2-unget-token)
        (setf continue nil
              pn (js2-parse-array-comprehension (car elems) pos)))

       ;; another element
       (t
        (unless after-lb-or-comma
          (js2-report-error "msg.no.bracket.arg"))
        (js2-unget-token)
        (push (js2-parse-assign-expr) elems)
        (setq after-lb-or-comma nil
              after-comma nil))))
    (unless js2-is-in-destructuring
      (js2-pop-scope))
    pn))

(defun js2-parse-array-comprehension (expr pos)
  "Parse a JavaScript 1.7 Array Comprehension.
EXPR is the first expression after the opening left-bracket.
POS is the beginning of the LB token preceding EXPR.
We should have just parsed the 'for' keyword before calling this function."
  (let (loops loop first filter if-pos result)
    (while (= (js2-get-token) js2-FOR)
      (let ((prev (car loops))) ; rearrange scope chain
        (push (setq loop (js2-parse-array-comp-loop)) loops)
        (if prev ; each loop is parent scope to the next one
            (setf (js2-scope-parent-scope loop) prev)
          ; first loop takes expr scope's parent
          (setf (js2-scope-parent-scope (setq first loop))
                (js2-scope-parent-scope js2-current-scope)))))
    (js2-unget-token)
    ;; set expr scope's parent to the last loop
    (setf (js2-scope-parent-scope js2-current-scope) (car loops))
    (if (/= (js2-get-token) js2-IF)
        (js2-unget-token)
      (setq if-pos (- (js2-current-token-beg) pos)  ; relative
            filter (js2-parse-condition)))
    (js2-must-match js2-RB "msg.no.bracket.arg" pos)
    (setq result (make-js2-array-comp-node :pos pos
                                           :len (- js2-ts-cursor pos)
                                           :result expr
                                           :loops (nreverse loops)
                                           :filter (car filter)
                                           :lp (js2-relpos (second filter) pos)
                                           :rp (js2-relpos (third filter) pos)
                                           :if-pos if-pos))
    (apply #'js2-node-add-children result expr (car filter)
           (js2-array-comp-node-loops result))
    (setq js2-current-scope first) ; pop to the first loop
    result))

(defun js2-parse-array-comp-loop ()
  "Parse a 'for [each] (foo [in|of] bar)' expression in an Array comprehension.
Last token peeked should be the initial FOR."
  (let ((pos (js2-current-token-beg))
        (pn (make-js2-array-comp-loop-node))
        tt iter obj foreach-p forof-p in-pos each-pos lp rp)
    (assert (= (js2-current-token-type) js2-FOR))
    (js2-push-scope pn)
    (unwind-protect
        (progn
          (when (js2-match-token js2-NAME)
            (if (string= (js2-current-token-string) "each")
                (progn
                  (setq foreach-p t
                        each-pos (- (js2-current-token-beg) pos)) ; relative
                  (js2-record-face 'font-lock-keyword-face))
              (js2-report-error "msg.no.paren.for")))
          (if (js2-must-match js2-LP "msg.no.paren.for")
              (setq lp (- (js2-current-token-beg) pos)))
          (setq tt (js2-peek-token))
          (cond
           ((or (= tt js2-LB)
                (= tt js2-LC))
            (js2-get-token)
            (setq iter (js2-parse-destruct-primary-expr))
            (js2-define-destruct-symbols iter js2-LET
                                         'font-lock-variable-name-face t))
           ((js2-match-token js2-NAME)
            (setq iter (js2-create-name-node)))
           (t
            (js2-report-error "msg.bad.var")))
          ;; Define as a let since we want the scope of the variable to
          ;; be restricted to the array comprehension
          (if (js2-name-node-p iter)
              (js2-define-symbol js2-LET (js2-name-node-name iter) pn t))
          (if (or (js2-match-token js2-IN)
                  (and (>= js2-language-version 200)
                       (js2-match-contextual-kwd "of")
                       (setq forof-p t)))
              (setq in-pos (- (js2-current-token-beg) pos))
            (js2-report-error "msg.in.after.for.name"))
          (setq obj (js2-parse-expr))
          (if (js2-must-match js2-RP "msg.no.paren.for.ctrl")
              (setq rp (- (js2-current-token-beg) pos)))
          (setf (js2-node-pos pn) pos
                (js2-node-len pn) (- js2-ts-cursor pos)
                (js2-array-comp-loop-node-iterator pn) iter
                (js2-array-comp-loop-node-object pn) obj
                (js2-array-comp-loop-node-in-pos pn) in-pos
                (js2-array-comp-loop-node-each-pos pn) each-pos
                (js2-array-comp-loop-node-foreach-p pn) foreach-p
                (js2-array-comp-loop-node-forof-p pn) forof-p
                (js2-array-comp-loop-node-lp pn) lp
                (js2-array-comp-loop-node-rp pn) rp)
          (js2-node-add-children pn iter obj))
      (js2-pop-scope))
    pn))

(defun js2-parse-object-literal ()
  (let ((pos (js2-current-token-beg))
        tt elems result after-comma
        (continue t))
    (while continue
      (setq tt (js2-get-token))
      (cond
       ;; {foo: ...}, {'foo': ...}, {foo, bar, ...},
       ;; {get foo() {...}}, or {set foo(x) {...}}
       ((or (js2-valid-prop-name-token tt)
            (= tt js2-STRING))
        (setq after-comma nil
              result (js2-parse-named-prop tt))
        (if (and (null result)
                 (not js2-recover-from-parse-errors))
            (setq continue nil)
          (push result elems)))
       ;; {12: x} or {10.7: x}
       ((= tt js2-NUMBER)
        (setq after-comma nil)
        (push (js2-parse-plain-property (make-js2-number-node)) elems))
       ;; trailing comma
       ((= tt js2-RC)
        (js2-unget-token)
        (setq continue nil)
        (if after-comma
            (js2-parse-warn-trailing-comma "msg.extra.trailing.comma"
                                           pos elems after-comma)))
       (t
        (js2-report-error "msg.bad.prop")
        (unless js2-recover-from-parse-errors
          (setq continue nil))))         ; end switch
      (if (js2-match-token js2-COMMA)
          (setq after-comma (js2-current-token-end))
        (setq continue nil)))           ; end loop
    (js2-must-match js2-RC "msg.no.brace.prop")
    (setq result (make-js2-object-node :pos pos
                                       :len (- js2-ts-cursor pos)
                                       :elems (nreverse elems)))
    (apply #'js2-node-add-children result (js2-object-node-elems result))
    result))

(defun js2-parse-named-prop (tt)
  "Parse a name, string, or getter/setter object property.
When `js2-is-in-destructuring' is t, forms like {a, b, c} will be permitted."
  (let ((string-prop (and (= tt js2-STRING)
                          (make-js2-string-node)))
        expr
        (ppos (js2-current-token-beg))
        (pend (js2-current-token-end))
        (name (js2-create-name-node))
        (prop (js2-current-token-string)))
    (cond
     ;; getter/setter prop
     ((and (= tt js2-NAME)
           (= (js2-peek-token) js2-NAME)
           (or (string= prop "get")
               (string= prop "set")))
      (js2-get-token)
      (js2-set-face ppos pend 'font-lock-keyword-face 'record)  ; get/set
      (js2-record-face 'font-lock-function-name-face)      ; for peeked name
      (setq name (js2-create-name-node)) ; discard get/set & use peeked name
      (js2-parse-getter-setter-prop ppos name (string= prop "get")))
     ;; Abbreviated destructuring binding, e.g. {a, b} = c;
     ;; XXX: To be honest, the value of `js2-is-in-destructuring' becomes t only
     ;; when patterns are used in variable declarations, function parameters,
     ;; catch-clause, and iterators.
     ;; We have to set `js2-is-in-destructuring' to t when the current
     ;; expressions are on the left side of any assignment, but it's difficult
     ;; because it requires looking ahead of expression.
     ((and js2-is-in-destructuring
           (= tt js2-NAME)
           (let ((ctk (js2-peek-token)))
             (or (= ctk js2-COMMA)
                 (= ctk js2-RC)
                 (js2-valid-prop-name-token ctk))))
      name)
     ;; regular prop
     (t
      (prog1
          (setq expr (js2-parse-plain-property (or string-prop name)))
        (js2-set-face ppos pend
                      (if (js2-function-node-p
                           (js2-object-prop-node-right expr))
                          'font-lock-function-name-face
                        'font-lock-variable-name-face)
                      'record))))))

(defun js2-parse-plain-property (prop)
  "Parse a non-getter/setter property in an object literal.
PROP is the node representing the property:  a number, name or string."
  (js2-must-match js2-COLON "msg.no.colon.prop")
  (let* ((pos (js2-node-pos prop))
        (colon (- (js2-current-token-beg) pos))
        (expr (js2-parse-assign-expr))
        (result (make-js2-object-prop-node
                 :pos pos
                 ;; don't include last consumed token in length
                 :len (- (+ (js2-node-pos expr)
                            (js2-node-len expr))
                         pos)
                 :left prop
                 :right expr
                 :op-pos colon)))
    (js2-node-add-children result prop expr)
    result))

(defun js2-parse-getter-setter-prop (pos prop get-p)
  "Parse getter or setter property in an object literal.
JavaScript syntax is:

  { get foo() {...}, set foo(x) {...} }

and expression closure style is also supported

  { get foo() x, set foo(x) _x = x }

POS is the start position of the `get' or `set' keyword.
PROP is the `js2-name-node' representing the property name.
GET-P is non-nil if the keyword was `get'."
  (let ((type (if get-p js2-GET js2-SET))
        result end
        (fn (js2-parse-function-expr)))
    ;; it has to be an anonymous function, as we already parsed the name
    (if (/= (js2-node-type fn) js2-FUNCTION)
        (js2-report-error "msg.bad.prop")
      (if (plusp (length (js2-function-name fn)))
          (js2-report-error "msg.bad.prop")))
    (js2-node-set-prop fn 'GETTER_SETTER type)  ; for codegen
    (setq end (js2-node-end fn)
          result (make-js2-getter-setter-node :type type
                                              :pos pos
                                              :len (- end pos)
                                              :left prop
                                              :right fn))
    (js2-node-add-children result prop fn)
    result))

(defun js2-create-name-node (&optional check-activation-p token string)
  "Create a name node using the current token and, optionally, STRING.
And, if CHECK-ACTIVATION-P is non-nil, use the value of TOKEN."
  (let* ((beg (js2-current-token-beg))
         (tt (js2-current-token-type))
         (s (or string
                (if (= js2-NAME tt)
                    (js2-current-token-string)
                  (js2-tt-name tt))))
         name)
    (setq name (make-js2-name-node :pos beg
                                   :name s
                                   :len (length s)))
    (if check-activation-p
        (js2-check-activation-name s (or token js2-NAME)))
    name))

;;; Indentation support

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
;; punch for so little code.

(defconst js2-possibly-braceless-keywords-re
  (concat "else[ \t]+if\\|for[ \t]+each\\|"
          (regexp-opt '("catch" "do" "else" "finally" "for" "if"
                        "try" "while" "with" "let")))
  "Regular expression matching keywords that are optionally
followed by an opening brace.")

(defconst js2-indent-operator-re
  (concat "[-+*/%<>=&^|?:.]\\([^-+*/]\\|$\\)\\|"
          (regexp-opt '("in" "instanceof") 'words))
  "Regular expression matching operators that affect indentation
of continued expressions.")

(defconst js2-declaration-keyword-re
  (regexp-opt '("var" "let" "const") 'words)
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
              (concat "\\([^\\]\\|^\\)" (string (nth 3 parse)))
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
  (and (looking-at js2-indent-operator-re)
       (or (not (looking-at ":"))
           (save-excursion
             (and (js2-re-search-backward "[?:{]\\|\\<case\\>" nil t)
                  (looking-at "?"))))))

(defun js2-continued-expression-p ()
  "Return non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (or (js2-looking-at-operator-p)
        (when (catch 'found
                (while (and (re-search-backward "\n" nil t)
                            (let ((state (syntax-ppss)))
                              (when (nth 4 state)
                                (goto-char (nth 8 state))) ;; skip comments
                              (skip-chars-backward " \t")
                              (if (bolp)
                                  t
                                (throw 'found t))))))
          (backward-char)
          (when (js2-looking-at-operator-p)
            (backward-char)
            (not (looking-at "\\*\\|++\\|--\\|/[/*]")))))))

(defun js2-end-of-do-while-loop-p ()
  "Return non-nil if word after point is `while' of a do-while
statement, else returns nil. A braceless do-while statement
spanning several lines requires that the start of the loop is
indented to the same column as the current line."
  (interactive)
  (save-excursion
    (when (looking-at "\\s-*\\<while\\>")
      (if (save-excursion
            (skip-chars-backward "[ \t\n]*}")
            (looking-at "[ \t\n]*}"))
          (save-excursion
            (backward-list) (backward-word 1) (looking-at "\\<do\\>"))
        (js2-re-search-backward "\\<do\\>" (point-at-bol) t)
        (or (looking-at "\\<do\\>")
            (let ((saved-indent (current-indentation)))
              (while (and (js2-re-search-backward "^[ \t]*\\<" nil t)
                          (/= (current-indentation) saved-indent)))
              (and (looking-at "[ \t]*\\<do\\>")
                   (not (js2-re-search-forward
                         "\\<while\\>" (point-at-eol) t))
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

(defun js2-proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (let* ((ctrl-stmt-indent (js2-ctrl-statement-indentation))
           (at-closing-bracket (looking-at "[]})]"))
           (same-indent-p (or at-closing-bracket
                              (looking-at "\\<case\\>[^:]")
                              (and (looking-at "\\<default:")
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

       (ctrl-stmt-indent)

       ((and declaration-indent continued-expr-p)
        (+ declaration-indent js2-basic-offset))

       (declaration-indent)

       (bracket
        (goto-char bracket)
        (cond
         ((looking-at "[({[][ \t]*\\(/[/*]\\|$\\)")
          (when (save-excursion (skip-chars-backward " \t)")
                                (looking-at ")"))
            (backward-list))
          (back-to-indentation)
          (and (eq js2-pretty-multiline-declarations 'all)
               (looking-at js2-declaration-keyword-re)
               (goto-char (1+ (match-end 0))))
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

(defun js2-lineup-comment (parse-status)
  "Indent a multi-line block comment continuation line."
  (let* ((beg (nth 8 parse-status))
         (first-line (js2-same-line beg))
         (offset (save-excursion
                   (goto-char beg)
                   (if (looking-at "/\\*")
                       (+ 1 (current-column))
                     0))))
    (unless first-line
      (indent-line-to offset))))

(defun js2-backward-sws ()
  "Move backward through whitespace and comments."
  (interactive)
  (while (forward-comment -1)))

(defun js2-forward-sws ()
  "Move forward through whitespace and comments."
  (interactive)
  (while (forward-comment 1)))

(defun js2-current-indent (&optional pos)
  "Return column of indentation on current line.
If POS is non-nil, go to that point and return indentation for that line."
  (save-excursion
    (if pos
        (goto-char pos))
    (back-to-indentation)
    (current-column)))

(defun js2-arglist-close ()
  "Return non-nil if we're on a line beginning with a close-paren/brace."
  (save-excursion
    (goto-char (point-at-bol))
    (js2-forward-sws)
    (looking-at "[])}]")))

(defun js2-indent-looks-like-label-p ()
  (goto-char (point-at-bol))
  (js2-forward-sws)
  (looking-at (concat js2-mode-identifier-re ":")))

(defun js2-indent-in-objlit-p (parse-status)
  "Return non-nil if this looks like an object-literal entry."
  (let ((start (nth 1 parse-status)))
    (and
     start
     (save-excursion
       (and (zerop (forward-line -1))
            (not (< (point) start))     ; crossed a {} boundary
            (js2-indent-looks-like-label-p)))
     (save-excursion
       (js2-indent-looks-like-label-p)))))

;; If prev line looks like foobar({ then we're passing an object
;; literal to a function call, and people pretty much always want to
;; de-dent back to the previous line, so move the 'basic-offset'
;; position to the front.
(defun js2-indent-objlit-arg-p (parse-status)
  (save-excursion
    (back-to-indentation)
    (js2-backward-sws)
    (and (eq (1- (point)) (nth 1 parse-status))
         (eq (char-before) ?{)
         (progn
           (forward-char -1)
           (skip-chars-backward " \t")
           (eq (char-before) ?\()))))

(defun js2-indent-case-block-p ()
  (save-excursion
    (back-to-indentation)
    (js2-backward-sws)
    (goto-char (point-at-bol))
    (skip-chars-forward " \t")
    (looking-at "case\\s-.+:")))

(defun js2-bounce-indent (normal-col parse-status &optional backwards)
  "Cycle among alternate computed indentation positions.
PARSE-STATUS is the result of `parse-partial-sexp' from the beginning
of the buffer to the current point.  NORMAL-COL is the indentation
column computed by the heuristic guesser based on current paren,
bracket, brace and statement nesting.  If BACKWARDS, cycle positions
in reverse."
  (let ((cur-indent (js2-current-indent))
        (old-buffer-undo-list buffer-undo-list)
        ;; Emacs 21 only has `count-lines', not `line-number-at-pos'
        (current-line (save-excursion
                        (forward-line 0)  ; move to bol
                        (1+ (count-lines (point-min) (point)))))
        positions pos main-pos anchor arglist-cont same-indent
        basic-offset computed-pos)
    ;; temporarily don't record undo info, if user requested this
    (when js2-mode-indent-inhibit-undo
      (setq buffer-undo-list t))
    (unwind-protect
        (progn
          ;; First likely point:  indent from beginning of previous code line
          (push (setq basic-offset
                      (+ (save-excursion
                           (back-to-indentation)
                           (js2-backward-sws)
                           (back-to-indentation)
                           (current-column))
                         js2-basic-offset))
                positions)

          ;; (First + epsilon) likely point:  indent 2x from beginning of
          ;; previous code line.  Google does it this way.
          (push (setq basic-offset
                      (+ (save-excursion
                           (back-to-indentation)
                           (js2-backward-sws)
                           (back-to-indentation)
                           (current-column))
                         (* 2 js2-basic-offset)))
                positions)

          ;; Second likely point:  indent from assign-expr RHS.  This
          ;; is just a crude guess based on finding " = " on the previous
          ;; line containing actual code.
          (setq pos (save-excursion
                      (forward-line -1)
                      (goto-char (point-at-bol))
                      (when (re-search-forward "\\s-+\\(=\\)\\s-+"
                                               (point-at-eol) t)
                        (goto-char (match-end 1))
                        (skip-chars-forward " \t\r\n")
                        (current-column))))
          (when pos
            (incf pos js2-basic-offset)
            (push pos positions))

          ;; Third likely point:  same indent as previous line of code.
          ;; Make it the first likely point if we're not on an
          ;; arglist-close line and previous line ends in a comma, or
          ;; both this line and prev line look like object-literal
          ;; elements.
          (setq pos (save-excursion
                      (goto-char (point-at-bol))
                      (js2-backward-sws)
                      (back-to-indentation)
                      (prog1
                          (current-column)
                        ;; while we're here, look for trailing comma
                        (if (save-excursion
                              (goto-char (point-at-eol))
                              (js2-backward-sws)
                              (eq (char-before) ?,))
                            (setq arglist-cont (1- (point)))))))
          (when pos
            (if (and (or arglist-cont
                         (js2-indent-in-objlit-p parse-status))
                     (not (js2-arglist-close)))
                (setq same-indent pos))
            (push pos positions))

          ;; Fourth likely point:  first preceding code with less indentation.
          ;; than the immediately preceding code line.
          (setq pos (save-excursion
                      (back-to-indentation)
                      (js2-backward-sws)
                      (back-to-indentation)
                      (setq anchor (current-column))
                      (while (and (zerop (forward-line -1))
                                  (>= (progn
                                        (back-to-indentation)
                                        (current-column))
                                      anchor)))
                      (setq pos (current-column))))
          (push pos positions)

          ;; nesting-heuristic position, main by default
          (push (setq main-pos normal-col) positions)

          ;; delete duplicates and sort positions list
          (setq positions (sort (delete-dups positions) '<))

          ;; comma-list continuation lines:  prev line indent takes precedence
          (if same-indent
              (setq main-pos same-indent))

          ;; common special cases where we want to indent in from previous line
          (if (or (js2-indent-case-block-p)
                  (js2-indent-objlit-arg-p parse-status))
              (setq main-pos basic-offset))

          ;; if bouncing backwards, reverse positions list
          (if backwards
              (setq positions (reverse positions)))

          ;; record whether we're already sitting on one of the alternatives
          (setq pos (member cur-indent positions))

          (cond
           ;; case 0:  we're one one of the alternatives and this is the
           ;; first time they've pressed TAB on this line (best-guess).
           ((and js2-mode-indent-ignore-first-tab
                 pos
                 ;; first time pressing TAB on this line?
                 (not (eq js2-mode-last-indented-line current-line)))
            ;; do nothing
            (setq computed-pos nil))
           ;; case 1:  only one computed position => use it
           ((null (cdr positions))
            (setq computed-pos 0))
           ;; case 2:  not on any of the computed spots => use main spot
           ((not pos)
            (setq computed-pos (js2-position main-pos positions)))
           ;; case 3:  on last position:  cycle to first position
           ((null (cdr pos))
            (setq computed-pos 0))
           ;; case 4:  on intermediate position:  cycle to next position
           (t
            (setq computed-pos (js2-position (second pos) positions))))

          ;; see if any hooks want to indent; otherwise we do it
          (loop with result = nil
                for hook in js2-indent-hook
                while (null result)
                do
                (setq result (funcall hook positions computed-pos))
                finally do
                (unless (or result (null computed-pos))
                  (indent-line-to (nth computed-pos positions)))))

      ;; finally
      (if js2-mode-indent-inhibit-undo
          (setq buffer-undo-list old-buffer-undo-list))
      ;; see commentary for `js2-mode-last-indented-line'
      (setq js2-mode-last-indented-line current-line))))

(defun js2-indent-bounce-backwards ()
  "Calls `js2-indent-line'.  When `js2-bounce-indent-p',
cycles between the computed indentation positions in reverse order."
  (interactive)
  (js2-indent-line t))

(defun js2-1-line-comment-continuation-p ()
  "Return t if we're in a 1-line comment continuation.
If so, we don't ever want to use bounce-indent."
  (save-excursion
    (and (progn
           (forward-line 0)
           (looking-at "\\s-*//"))
         (progn
           (forward-line -1)
           (forward-line 0)
           (when (looking-at "\\s-*$")
             (js2-backward-sws)
             (forward-line 0))
           (looking-at "\\s-*//")))))

(defun js2-indent-line (&optional bounce-backwards)
  "Indent the current line as JavaScript source text."
  (interactive)
  (let (parse-status offset indent-col
        ;; Don't whine about errors/warnings when we're indenting.
        ;; This has to be set before calling parse-partial-sexp below.
        (inhibit-point-motion-hooks t))
    (setq parse-status (save-excursion
                         (syntax-ppss (point-at-bol)))
          offset (- (point) (save-excursion
                              (back-to-indentation)
                              (point))))
    (js2-with-underscore-as-word-syntax
     (if (nth 4 parse-status)
         (js2-lineup-comment parse-status)
       (setq indent-col (js2-proper-indentation parse-status))
       ;; See comments below about `js2-mode-last-indented-line'.
       (cond
        ;; bounce-indenting is disabled during electric-key indent.
        ;; It doesn't work well on first line of buffer.
        ((and js2-bounce-indent-p
              (not (js2-same-line (point-min)))
              (not (js2-1-line-comment-continuation-p)))
         (js2-bounce-indent indent-col parse-status bounce-backwards))
        ;; just indent to the guesser's likely spot
        (t (indent-line-to indent-col))))
     (when (plusp offset)
       (forward-char offset)))))

(defun js2-indent-region (start end)
  "Indent the region, but don't use bounce indenting."
  (let ((js2-bounce-indent-p nil)
        (indent-region-function nil)
        (after-change-functions (remq 'js2-mode-edit
                                      after-change-functions)))
    (indent-region start end nil) ; nil for byte-compiler
    (js2-mode-edit start end (- end start))))

(defvar js2-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-`") #'js2-next-error)
    (define-key map [mouse-1] #'js2-mode-show-node)
    map)
  "Keymap used when `js2-minor-mode' is active.")

;;;###autoload
(define-minor-mode js2-minor-mode
  "Minor mode for running js2 as a background linter.
This allows you to use a different major mode for JavaScript editing,
such as `espresso-mode', while retaining the asynchronous error/warning
highlighting features of `js2-mode'."
  :group 'js2-mode
  :lighter " js-lint"
  (if js2-minor-mode
      (js2-minor-mode-enter)
    (js2-minor-mode-exit)))

(defun js2-minor-mode-enter ()
  "Initialization for `js2-minor-mode'."
  (set (make-local-variable 'max-lisp-eval-depth)
       (max max-lisp-eval-depth 3000))
  (setq next-error-function #'js2-next-error)
  (js2-set-default-externs)
  ;; Experiment:  make reparse-delay longer for longer files.
  (if (plusp js2-dynamic-idle-timer-adjust)
      (setq js2-idle-timer-delay
            (* js2-idle-timer-delay
               (/ (point-max) js2-dynamic-idle-timer-adjust))))
  (setq js2-mode-buffer-dirty-p t
        js2-mode-parsing nil)
  (set (make-local-variable 'js2-highlight-level) 0) ; no syntax highlighting
  (add-hook 'after-change-functions #'js2-minor-mode-edit nil t)
  (add-hook 'change-major-mode-hook #'js2-minor-mode-exit nil t)
  (when js2-include-jslint-globals
    (add-hook 'js2-post-parse-callbacks 'js2-apply-jslint-globals nil t))
  (run-hooks 'js2-init-hook)
  (js2-reparse))

(defun js2-minor-mode-exit ()
  "Turn off `js2-minor-mode'."
  (setq next-error-function nil)
  (remove-hook 'after-change-functions #'js2-mode-edit t)
  (remove-hook 'change-major-mode-hook #'js2-minor-mode-exit t)
  (when js2-mode-node-overlay
    (delete-overlay js2-mode-node-overlay)
    (setq js2-mode-node-overlay nil))
  (js2-remove-overlays)
  (remove-hook 'js2-post-parse-callbacks 'js2-apply-jslint-globals t)
  (setq js2-mode-ast nil))

(defvar js2-source-buffer nil "Linked source buffer for diagnostics view")
(make-variable-buffer-local 'js2-source-buffer)

(defun* js2-display-error-list ()
  "Display a navigable buffer listing parse errors/warnings."
  (interactive)
  (unless (js2-have-errors-p)
    (message "No errors")
    (return-from js2-display-error-list))
  (labels ((annotate-list
            (lst type)
            "Add diagnostic TYPE and line number to errs list"
            (mapcar (lambda (err)
                      (list err type (line-number-at-pos (nth 1 err))))
                    lst)))
    (let* ((srcbuf (current-buffer))
           (errbuf (get-buffer-create "*js-lint*"))
           (errors (annotate-list
                    (when js2-mode-ast (js2-ast-root-errors js2-mode-ast))
                    'js2-error))  ; must be a valid face name
           (warnings (annotate-list
                      (when js2-mode-ast (js2-ast-root-warnings js2-mode-ast))
                      'js2-warning))  ; must be a valid face name
           (all-errs (sort (append errors warnings)
                           (lambda (e1 e2) (< (cadar e1) (cadar e2))))))
      (with-current-buffer errbuf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (err all-errs)
            (destructuring-bind ((msg-key beg _end &rest) type line) err
              (insert-text-button
               (format "line %d: %s" line (js2-get-msg msg-key))
               'face type
               'follow-link "\C-m"
               'action 'js2-error-buffer-jump
               'js2-msg (js2-get-msg msg-key)
               'js2-pos beg)
              (insert "\n"))))
        (js2-error-buffer-mode)
        (setq js2-source-buffer srcbuf)
        (pop-to-buffer errbuf)
        (goto-char (point-min))
        (unless (eobp)
          (js2-error-buffer-view))))))

(defvar js2-error-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'js2-error-buffer-next)
    (define-key map "p" #'js2-error-buffer-prev)
    (define-key map (kbd "RET") #'js2-error-buffer-jump)
    (define-key map "o" #'js2-error-buffer-view)
    (define-key map "q" #'js2-error-buffer-quit)
    map)
  "Keymap used for js2 diagnostics buffers.")

(defun js2-error-buffer-mode ()
  "Major mode for js2 diagnostics buffers.
Selecting an error will jump it to the corresponding source-buffer error.
\\{js2-error-buffer-mode-map}"
  (interactive)
  (setq major-mode 'js2-error-buffer-mode
        mode-name "JS Lint Diagnostics")
  (use-local-map js2-error-buffer-mode-map)
  (setq truncate-lines t)
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (run-hooks 'js2-error-buffer-mode-hook))

(defun js2-error-buffer-next ()
  "Move to next error and view it."
  (interactive)
  (when (zerop (forward-line 1))
    (js2-error-buffer-view)))

(defun js2-error-buffer-prev ()
  "Move to previous error and view it."
  (interactive)
  (when (zerop (forward-line -1))
    (js2-error-buffer-view)))

(defun js2-error-buffer-quit ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer))

(defun js2-error-buffer-jump (&rest ignored)
  "Jump cursor to current error in source buffer."
  (interactive)
  (when (js2-error-buffer-view)
    (pop-to-buffer js2-source-buffer)))

(defun js2-error-buffer-view ()
  "Scroll source buffer to show error at current line."
  (interactive)
  (cond
   ((not (eq major-mode 'js2-error-buffer-mode))
    (message "Not in a js2 errors buffer"))
   ((not (buffer-live-p js2-source-buffer))
    (message "Source buffer has been killed"))
   ((not (wholenump (get-text-property (point) 'js2-pos)))
    (message "There does not seem to be an error here"))
   (t
    (let ((pos (get-text-property (point) 'js2-pos))
          (msg (get-text-property (point) 'js2-msg)))
      (save-selected-window
        (pop-to-buffer js2-source-buffer)
        (goto-char pos)
        (message msg))))))

;;;###autoload
(define-derived-mode js2-mode prog-mode "Javascript-IDE"
  ;; FIXME: Should derive from js-mode.
  "Major mode for editing JavaScript code."
  ;; Used by comment-region; don't change it.
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) js2-comment-start-skip)
  (set (make-local-variable 'max-lisp-eval-depth)
       (max max-lisp-eval-depth 3000))
  (set (make-local-variable 'indent-line-function) #'js2-indent-line)
  (set (make-local-variable 'indent-region-function) #'js2-indent-region)
  (set (make-local-variable 'fill-paragraph-function) #'c-fill-paragraph)
  (set (make-local-variable 'comment-line-break-function) #'js2-line-break)
  (set (make-local-variable 'beginning-of-defun-function) #'js2-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) #'js2-end-of-defun)
  ;; We un-confuse `parse-partial-sexp' by setting syntax-table properties
  ;; for characters inside regexp literals.
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  ;; this is necessary to make `show-paren-function' work properly
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; needed for M-x rgrep, among other things
  (put 'js2-mode 'find-tag-default-function #'js2-mode-find-tag)

  (set (make-local-variable 'electric-indent-chars)
       (append '("{" "}" "(" ")" "[" "]" ":" ";" "," "*")
               electric-indent-chars))
  (set (make-local-variable 'electric-layout-rules)
       '((?\; . after) (?\{ . after) (?\} . before)))

  ;; some variables needed by cc-engine for paragraph-fill, etc.
  (setq c-comment-prefix-regexp js2-comment-prefix-regexp
        c-comment-start-regexp "/[*/]\\|\\s|"
        c-line-comment-starter "//"
        c-paragraph-start js2-paragraph-start
        c-paragraph-separate "$"
        c-syntactic-ws-start js2-syntactic-ws-start
        c-syntactic-ws-end js2-syntactic-ws-end
        c-syntactic-eol js2-syntactic-eol)

  (let ((c-buffer-is-cc-mode t))
    ;; Copied from `js-mode'.  Also see Bug#6071.
    (make-local-variable 'paragraph-start)
    (make-local-variable 'paragraph-separate)
    (make-local-variable 'paragraph-ignore-fill-prefix)
    (make-local-variable 'adaptive-fill-mode)
    (make-local-variable 'adaptive-fill-regexp)
    (c-setup-paragraph-variables))

  (setq font-lock-defaults '(nil t))

  ;; Experiment:  make reparse-delay longer for longer files.
  (when (plusp js2-dynamic-idle-timer-adjust)
    (setq js2-idle-timer-delay
          (* js2-idle-timer-delay
             (/ (point-max) js2-dynamic-idle-timer-adjust))))

  (add-hook 'change-major-mode-hook #'js2-mode-exit nil t)
  (add-hook 'after-change-functions #'js2-mode-edit nil t)
  (setq imenu-create-index-function #'js2-mode-create-imenu-index)
  (setq next-error-function #'js2-next-error)
  (imenu-add-to-menubar (concat "IM-" mode-name))
  (add-to-invisibility-spec '(js2-outline . t))
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (set (make-local-variable 'forward-sexp-function) #'js2-mode-forward-sexp)

  (setq js2-mode-functions-hidden nil
        js2-mode-comments-hidden nil
        js2-mode-buffer-dirty-p t
        js2-mode-parsing nil)

  (js2-set-default-externs)

  (when js2-include-jslint-globals
    (add-hook 'js2-post-parse-callbacks 'js2-apply-jslint-globals nil t))

  (run-hooks 'js2-init-hook)

  (js2-reparse))

(defun js2-mode-exit ()
  "Exit `js2-mode' and clean up."
  (interactive)
  (when js2-mode-node-overlay
    (delete-overlay js2-mode-node-overlay)
    (setq js2-mode-node-overlay nil))
  (js2-remove-overlays)
  (setq js2-mode-ast nil)
  (remove-hook 'change-major-mode-hook #'js2-mode-exit t)
  (remove-from-invisibility-spec '(js2-outline . t))
  (js2-mode-show-all)
  (with-silent-modifications
    (js2-clear-face (point-min) (point-max))))

(defun js2-mode-reset-timer ()
  "Cancel any existing parse timer and schedule a new one."
  (if js2-mode-parse-timer
      (cancel-timer js2-mode-parse-timer))
  (setq js2-mode-parsing nil)
  (let ((timer (timer-create)))
    (setq js2-mode-parse-timer timer)
    (timer-set-function timer 'js2-mode-idle-reparse (list (current-buffer)))
    (timer-set-idle-time timer js2-idle-timer-delay)
    ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=12326
    (timer-activate-when-idle timer nil)))

(defun js2-mode-idle-reparse (buffer)
  "Run `js2-reparse' if BUFFER is the current buffer, or schedule
it to be reparsed when the buffer is selected."
  (if (eq buffer (current-buffer))
      (js2-reparse)
    ;; reparse when the buffer is selected again
    (with-current-buffer buffer
      (add-hook 'window-configuration-change-hook
                #'js2-mode-idle-reparse-inner
                nil t))))

(defun js2-mode-idle-reparse-inner ()
  (remove-hook 'window-configuration-change-hook
               #'js2-mode-idle-reparse-inner
               t)
  (js2-reparse))

(defun js2-mode-edit (_beg _end _len)
  "Schedule a new parse after buffer is edited.
Buffer edit spans from BEG to END and is of length LEN."
  (setq js2-mode-buffer-dirty-p t)
  (js2-mode-hide-overlay)
  (js2-mode-reset-timer))

(defun js2-minor-mode-edit (_beg _end _len)
  "Callback for buffer edits in `js2-mode'.
Schedules a new parse after buffer is edited.
Buffer edit spans from BEG to END and is of length LEN."
  (setq js2-mode-buffer-dirty-p t)
  (js2-mode-hide-overlay)
  (js2-mode-reset-timer))

(defun js2-reparse (&optional force)
  "Re-parse current buffer after user finishes some data entry.
If we get any user input while parsing, including cursor motion,
we discard the parse and reschedule it.  If FORCE is nil, then the
buffer will only rebuild its `js2-mode-ast' if the buffer is dirty."
  (let (time
        interrupted-p
        (js2-compiler-strict-mode js2-mode-show-strict-warnings))
    (unless js2-mode-parsing
      (setq js2-mode-parsing t)
      (unwind-protect
          (when (or js2-mode-buffer-dirty-p force)
            (js2-remove-overlays)
            (with-silent-modifications
              (setq js2-mode-buffer-dirty-p nil
                    js2-mode-fontifications nil
                    js2-mode-deferred-properties nil)
              (if js2-mode-verbose-parse-p
                  (message "parsing..."))
              (setq time
                    (js2-time
                     (setq interrupted-p
                           (catch 'interrupted
                             (js2-parse)
                             ;; if parsing is interrupted, comments and regex
                             ;; literals stay ignored by `parse-partial-sexp'
                             (remove-text-properties (point-min) (point-max)
                                                     '(syntax-table))
                             (js2-mode-apply-deferred-properties)
                             (js2-mode-remove-suppressed-warnings)
                             (js2-mode-show-warnings)
                             (js2-mode-show-errors)
                             (if (>= js2-highlight-level 1)
                                 (js2-highlight-jsdoc js2-mode-ast))
                             nil))))
              (if interrupted-p
                  (progn
                    ;; unfinished parse => try again
                    (setq js2-mode-buffer-dirty-p t)
                    (js2-mode-reset-timer))
                (if js2-mode-verbose-parse-p
                    (message "Parse time: %s" time)))))
        (setq js2-mode-parsing nil)
        (unless interrupted-p
          (setq js2-mode-parse-timer nil))))))

(defun js2-mode-show-node (event)
  "Debugging aid:  highlight selected AST node on mouse click."
  (interactive "e")
  (mouse-set-point event)
  (setq deactivate-mark t)
  (when js2-mode-show-overlay
    (let ((node (js2-node-at-point))
          beg end)
      (if (null node)
          (message "No node found at location %s" (point))
        (setq beg (js2-node-abs-pos node)
              end (+ beg (js2-node-len node)))
        (if js2-mode-node-overlay
            (move-overlay js2-mode-node-overlay beg end)
          (setq js2-mode-node-overlay (make-overlay beg end))
          (overlay-put js2-mode-node-overlay 'font-lock-face 'highlight))
        (with-silent-modifications
          (put-text-property beg end 'point-left #'js2-mode-hide-overlay))
        (message "%s, parent: %s"
                 (js2-node-short-name node)
                 (if (js2-node-parent node)
                     (js2-node-short-name (js2-node-parent node))
                   "nil"))))))

(defun js2-mode-hide-overlay (&optional _p1 p2)
  "Remove the debugging overlay when the point moves.
P1 and P2 are the old and new values of point, respectively."
  (when js2-mode-node-overlay
    (let ((beg (overlay-start js2-mode-node-overlay))
          (end (overlay-end js2-mode-node-overlay)))
      ;; Sometimes we're called spuriously.
      (unless (and p2
                   (>= p2 beg)
                   (<= p2 end))
        (with-silent-modifications
          (remove-text-properties beg end '(point-left nil)))
        (delete-overlay js2-mode-node-overlay)
        (setq js2-mode-node-overlay nil)))))

(defun js2-mode-reset ()
  "Debugging helper:  reset everything."
  (interactive)
  (js2-mode-exit)
  (js2-mode))

(defun js2-mode-show-warn-or-err (e face)
  "Highlight a warning or error E with FACE.
E is a list of ((MSG-KEY MSG-ARG) BEG LEN OVERRIDE-FACE).
The last element is optional.  When present, use instead of FACE."
  (let* ((key (first e))
         (beg (second e))
         (end (+ beg (third e)))
         ;; Don't inadvertently go out of bounds.
         (beg (max (point-min) (min beg (point-max))))
         (end (max (point-min) (min end (point-max))))
         (js2-highlight-level 3)    ; so js2-set-face is sure to fire
         (ovl (make-overlay beg end)))
    (overlay-put ovl 'font-lock-face (or (fourth e) face))
    (overlay-put ovl 'js2-error t)
    (put-text-property beg end 'help-echo (js2-get-msg key))
    (put-text-property beg end 'point-entered #'js2-echo-error)))

(defun js2-remove-overlays ()
  "Remove overlays from buffer that have a `js2-error' property."
  (let ((beg (point-min))
        (end (point-max)))
    (save-excursion
      (dolist (o (overlays-in beg end))
        (when (overlay-get o 'js2-error)
          (delete-overlay o))))))

(defun js2-error-at-point (&optional pos)
  "Return non-nil if there's an error overlay at POS.
Defaults to point."
  (loop with pos = (or pos (point))
        for o in (overlays-at pos)
        thereis (overlay-get o 'js2-error)))

(defun js2-mode-apply-deferred-properties ()
  "Apply fontifications and other text properties recorded during parsing."
  (when (plusp js2-highlight-level)
    ;; We defer clearing faces as long as possible to eliminate flashing.
    (js2-clear-face (point-min) (point-max))
    ;; Have to reverse the recorded fontifications list so that errors
    ;; and warnings overwrite the normal fontifications.
    (dolist (f (nreverse js2-mode-fontifications))
      (put-text-property (first f) (second f) 'font-lock-face (third f)))
    (setq js2-mode-fontifications nil))
  (dolist (p js2-mode-deferred-properties)
    (apply #'put-text-property p))
  (setq js2-mode-deferred-properties nil))

(defun js2-mode-show-errors ()
  "Highlight syntax errors."
  (when js2-mode-show-parse-errors
    (dolist (e (js2-ast-root-errors js2-mode-ast))
      (js2-mode-show-warn-or-err e 'js2-error))))

(defun js2-mode-remove-suppressed-warnings ()
  "Take suppressed warnings out of the AST warnings list.
This ensures that the counts and `next-error' are correct."
  (setf (js2-ast-root-warnings js2-mode-ast)
        (js2-delete-if
         (lambda (e)
           (let ((key (caar e)))
             (or
              (and (not js2-strict-trailing-comma-warning)
                   (string-match "trailing\\.comma" key))
              (and (not js2-strict-cond-assign-warning)
                   (string= key "msg.equal.as.assign"))
              (and js2-missing-semi-one-line-override
                   (string= key "msg.missing.semi")
                   (let* ((beg (second e))
                          (node (js2-node-at-point beg))
                          (fn (js2-mode-find-parent-fn node))
                          (body (and fn (js2-function-node-body fn)))
                          (lc (and body (js2-node-abs-pos body)))
                          (rc (and lc (+ lc (js2-node-len body)))))
                     (and fn
                          (or (null body)
                              (save-excursion
                                (goto-char beg)
                                (and (js2-same-line lc)
                                     (js2-same-line rc))))))))))
         (js2-ast-root-warnings js2-mode-ast))))

(defun js2-mode-show-warnings ()
  "Highlight strict-mode warnings."
  (when js2-mode-show-strict-warnings
    (dolist (e (js2-ast-root-warnings js2-mode-ast))
      (js2-mode-show-warn-or-err e 'js2-warning))))

(defun js2-echo-error (_old-point new-point)
  "Called by point-motion hooks."
  (let ((msg (get-text-property new-point 'help-echo)))
    (when (and (stringp msg)
               (not (active-minibuffer-window))
               (not (current-message)))
      (message msg))))

(defalias 'js2-echo-help #'js2-echo-error)

(defun js2-line-break (&optional _soft)
  "Break line at point and indent, continuing comment if within one.
If inside a string, and `js2-concat-multiline-strings' is not
nil, turn it into concatenation."
  (interactive)
  (let ((parse-status (syntax-ppss)))
    (cond
     ;; Check if we're inside a string.
     ((nth 3 parse-status)
      (if js2-concat-multiline-strings
          (js2-mode-split-string parse-status)
        (insert "\n")))
     ;; Check if inside a block comment.
     ((nth 4 parse-status)
      (js2-mode-extend-comment (nth 8 parse-status)))
     (t
      (newline-and-indent)))))

(defun js2-mode-split-string (parse-status)
  "Turn a newline in mid-string into a string concatenation.
PARSE-STATUS is as documented in `parse-partial-sexp'."
  (let* ((quote-char (nth 3 parse-status))
         (at-eol (eq js2-concat-multiline-strings 'eol)))
    (insert quote-char)
    (insert (if at-eol " +\n" "\n"))
    (unless at-eol
      (insert "+ "))
    (js2-indent-line)
    (insert quote-char)
    (when (eolp)
      (insert quote-char)
      (backward-char 1))))

(defun js2-mode-extend-comment (start-pos)
  "Indent the line and, when inside a comment block, add comment prefix."
  (let (star single col first-line needs-close)
    (save-excursion
      (back-to-indentation)
      (when (< (point) start-pos)
        (goto-char start-pos))
      (cond
       ((looking-at "\\*[^/]")
        (setq star t
              col (current-column)))
       ((looking-at "/\\*")
        (setq star t
              first-line t
              col (1+ (current-column))))
       ((looking-at "//")
        (setq single t
              col (current-column)))))
    ;; Heuristic for whether we need to close the comment:
    ;; if we've got a parse error here, assume it's an unterminated
    ;; comment.
    (setq needs-close
          (or
           (eq (get-text-property (1- (point)) 'point-entered)
               'js2-echo-error)
           ;; The heuristic above doesn't work well when we're
           ;; creating a comment and there's another one downstream,
           ;; as our parser thinks this one ends at the end of the
           ;; next one.  (You can have a /* inside a js block comment.)
           ;; So just close it if the next non-ws char isn't a *.
           (and first-line
                (eolp)
                (save-excursion
                  (skip-chars-forward " \t\r\n")
                  (not (eq (char-after) ?*))))))
    (delete-horizontal-space)
    (insert "\n")
    (cond
     (star
      (indent-to col)
      (insert "* ")
      (if (and first-line needs-close)
          (save-excursion
            (insert "\n")
            (indent-to col)
            (insert "*/"))))
     ((and single
           (save-excursion
              (and (zerop (forward-line 1))
                   (looking-at "\\s-*//"))))
      (indent-to col)
      (insert "// ")))
    ;; Don't need to extend the comment after all.
    (js2-indent-line)))

(defun js2-beginning-of-line ()
  "Toggle point between bol and first non-whitespace char in line.
Also moves past comment delimiters when inside comments."
  (interactive)
  (let (node)
    (cond
     ((bolp)
      (back-to-indentation))
     ((looking-at "//")
      (skip-chars-forward "/ \t"))
     ((and (eq (char-after) ?*)
           (setq node (js2-comment-at-point))
           (memq (js2-comment-node-format node) '(jsdoc block))
           (save-excursion
             (skip-chars-backward " \t")
             (bolp)))
      (skip-chars-forward "\* \t"))
     (t
      (goto-char (point-at-bol))))))

(defun js2-end-of-line ()
  "Toggle point between eol and last non-whitespace char in line."
  (interactive)
  (if (eolp)
      (skip-chars-backward " \t")
    (goto-char (point-at-eol))))

(defun js2-mode-wait-for-parse (callback)
  "Invoke CALLBACK when parsing is finished.
If parsing is already finished, calls CALLBACK immediately."
  (if (not js2-mode-buffer-dirty-p)
      (funcall callback)
    (push callback js2-mode-pending-parse-callbacks)
    (add-hook 'js2-parse-finished-hook #'js2-mode-parse-finished)))

(defun js2-mode-parse-finished ()
  "Invoke callbacks in `js2-mode-pending-parse-callbacks'."
  ;; We can't let errors propagate up, since it prevents the
  ;; `js2-parse' method from completing normally and returning
  ;; the ast, which makes things mysteriously not work right.
  (unwind-protect
      (dolist (cb js2-mode-pending-parse-callbacks)
        (condition-case err
            (funcall cb)
          (error (message "%s" err))))
    (setq js2-mode-pending-parse-callbacks nil)))

(defun js2-mode-flag-region (from to flag)
  "Hide or show text from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden.
Returns the created overlay if FLAG is non-nil."
  (remove-overlays from to 'invisible 'js2-outline)
  (when flag
    (let ((o (make-overlay from to)))
      (overlay-put o 'invisible 'js2-outline)
      (overlay-put o 'isearch-open-invisible
                   'js2-isearch-open-invisible)
      o)))

;; Function to be set as an outline-isearch-open-invisible' property
;; to the overlay that makes the outline invisible (see
;; `js2-mode-flag-region').
(defun js2-isearch-open-invisible (_overlay)
  ;; We rely on the fact that isearch places point on the matched text.
  (js2-mode-show-element))

(defun js2-mode-invisible-overlay-bounds (&optional pos)
  "Return cons cell of bounds of folding overlay at POS.
Returns nil if not found."
  (let ((overlays (overlays-at (or pos (point))))
        o)
    (while (and overlays
                (not o))
      (if (overlay-get (car overlays) 'invisible)
          (setq o (car overlays))
        (setq overlays (cdr overlays))))
    (if o
        (cons (overlay-start o) (overlay-end o)))))

(defun js2-mode-function-at-point (&optional pos)
  "Return the innermost function node enclosing current point.
Returns nil if point is not in a function."
  (let ((node (js2-node-at-point pos)))
    (while (and node (not (js2-function-node-p node)))
      (setq node (js2-node-parent node)))
    (if (js2-function-node-p node)
        node)))

(defun js2-mode-toggle-element ()
  "Hide or show the foldable element at the point."
  (interactive)
  (let (comment fn pos)
    (save-excursion
      (cond
       ;; /* ... */ comment?
       ((js2-block-comment-p (setq comment (js2-comment-at-point)))
        (if (js2-mode-invisible-overlay-bounds
             (setq pos (+ 3 (js2-node-abs-pos comment))))
            (progn
              (goto-char pos)
              (js2-mode-show-element))
          (js2-mode-hide-element)))
       ;; //-comment?
       ((save-excursion
          (back-to-indentation)
          (looking-at js2-mode-//-comment-re))
        (js2-mode-toggle-//-comment))
       ;; function?
       ((setq fn (js2-mode-function-at-point))
        (setq pos (and (js2-function-node-body fn)
                       (js2-node-abs-pos (js2-function-node-body fn))))
        (goto-char (1+ pos))
        (if (js2-mode-invisible-overlay-bounds)
            (js2-mode-show-element)
          (js2-mode-hide-element)))
       (t
        (message "Nothing at point to hide or show"))))))

(defun js2-mode-hide-element ()
  "Fold/hide contents of a block, showing ellipses.
Show the hidden text with \\[js2-mode-show-element]."
  (interactive)
  (if js2-mode-buffer-dirty-p
      (js2-mode-wait-for-parse #'js2-mode-hide-element))
  (let (node body beg end)
    (cond
     ((js2-mode-invisible-overlay-bounds)
      (message "already hidden"))
     (t
      (setq node (js2-node-at-point))
      (cond
       ((js2-block-comment-p node)
        (js2-mode-hide-comment node))
       (t
        (while (and node (not (js2-function-node-p node)))
          (setq node (js2-node-parent node)))
        (if (and node
                 (setq body (js2-function-node-body node)))
            (progn
              (setq beg (js2-node-abs-pos body)
                    end (+ beg (js2-node-len body)))
              (js2-mode-flag-region (1+ beg) (1- end) 'hide))
          (message "No collapsable element found at point"))))))))

(defun js2-mode-show-element ()
  "Show the hidden element at current point."
  (interactive)
  (let ((bounds (js2-mode-invisible-overlay-bounds)))
    (if bounds
        (js2-mode-flag-region (car bounds) (cdr bounds) nil)
      (message "Nothing to un-hide"))))

(defun js2-mode-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (js2-mode-flag-region (point-min) (point-max) nil))

(defun js2-mode-toggle-hide-functions ()
  (interactive)
  (if js2-mode-functions-hidden
      (js2-mode-show-functions)
    (js2-mode-hide-functions)))

(defun js2-mode-hide-functions ()
  "Hides all non-nested function bodies in the buffer.
Use \\[js2-mode-show-all] to reveal them, or \\[js2-mode-show-element]
to open an individual entry."
  (interactive)
  (if js2-mode-buffer-dirty-p
      (js2-mode-wait-for-parse #'js2-mode-hide-functions))
  (if (null js2-mode-ast)
      (message "Oops - parsing failed")
    (setq js2-mode-functions-hidden t)
    (js2-visit-ast js2-mode-ast #'js2-mode-function-hider)))

(defun js2-mode-function-hider (n endp)
  (when (not endp)
    (let ((tt (js2-node-type n))
          body beg end)
      (cond
       ((and (= tt js2-FUNCTION)
             (setq body (js2-function-node-body n)))
        (setq beg (js2-node-abs-pos body)
              end (+ beg (js2-node-len body)))
        (js2-mode-flag-region (1+ beg) (1- end) 'hide)
        nil)   ; don't process children of function
       (t
        t))))) ; keep processing other AST nodes

(defun js2-mode-show-functions ()
  "Un-hide any folded function bodies in the buffer."
  (interactive)
  (setq js2-mode-functions-hidden nil)
  (save-excursion
    (goto-char (point-min))
    (while (/= (goto-char (next-overlay-change (point)))
               (point-max))
      (dolist (o (overlays-at (point)))
        (when (and (overlay-get o 'invisible)
                   (not (overlay-get o 'comment)))
          (js2-mode-flag-region (overlay-start o) (overlay-end o) nil))))))

(defun js2-mode-hide-comment (n)
  (let* ((head (if (eq (js2-comment-node-format n) 'jsdoc)
                   3  ; /**
                 2))  ; /*
         (beg (+ (js2-node-abs-pos n) head))
         (end (- (+ beg (js2-node-len n)) head 2))
         (o (js2-mode-flag-region beg end 'hide)))
    (overlay-put o 'comment t)))

(defun js2-mode-toggle-hide-comments ()
  "Folds all block comments in the buffer.
Use \\[js2-mode-show-all] to reveal them, or \\[js2-mode-show-element]
to open an individual entry."
  (interactive)
  (if js2-mode-comments-hidden
      (js2-mode-show-comments)
    (js2-mode-hide-comments)))

(defun js2-mode-hide-comments ()
  (interactive)
  (if js2-mode-buffer-dirty-p
      (js2-mode-wait-for-parse #'js2-mode-hide-comments))
  (if (null js2-mode-ast)
      (message "Oops - parsing failed")
    (setq js2-mode-comments-hidden t)
    (dolist (n (js2-ast-root-comments js2-mode-ast))
      (when (js2-block-comment-p n)
        (js2-mode-hide-comment n)))
    (js2-mode-hide-//-comments)))

(defun js2-mode-extend-//-comment (direction)
  "Find start or end of a block of similar //-comment lines.
DIRECTION is -1 to look back, 1 to look forward.
INDENT is the indentation level to match.
Returns the end-of-line position of the furthest adjacent
//-comment line with the same indentation as the current line.
If there is no such matching line, returns current end of line."
  (let ((pos (point-at-eol))
        (indent (current-indentation)))
    (save-excursion
      (while (and (zerop (forward-line direction))
                  (looking-at js2-mode-//-comment-re)
                  (eq indent (length (match-string 1))))
        (setq pos (point-at-eol))
      pos))))

(defun js2-mode-hide-//-comments ()
  "Fold adjacent 1-line comments, showing only snippet of first one."
  (let (beg end)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward js2-mode-//-comment-re nil t)
        (setq beg (point)
              end (js2-mode-extend-//-comment 1))
        (unless (eq beg end)
          (overlay-put (js2-mode-flag-region beg end 'hide)
                       'comment t))
        (goto-char end)
        (forward-char 1)))))

(defun js2-mode-toggle-//-comment ()
  "Fold or un-fold any multi-line //-comment at point.
Caller should have determined that this line starts with a //-comment."
  (let* ((beg (point-at-eol))
         (end beg))
    (save-excursion
      (goto-char end)
      (if (js2-mode-invisible-overlay-bounds)
          (js2-mode-show-element)
        ;; else hide the comment
        (setq beg (js2-mode-extend-//-comment -1)
              end (js2-mode-extend-//-comment 1))
        (unless (eq beg end)
          (overlay-put (js2-mode-flag-region beg end 'hide)
                       'comment t))))))

(defun js2-mode-show-comments ()
  "Un-hide any hidden comments, leaving other hidden elements alone."
  (interactive)
  (setq js2-mode-comments-hidden nil)
  (save-excursion
    (goto-char (point-min))
    (while (/= (goto-char (next-overlay-change (point)))
               (point-max))
      (dolist (o (overlays-at (point)))
        (when (overlay-get o 'comment)
          (js2-mode-flag-region (overlay-start o) (overlay-end o) nil))))))

(defun js2-mode-display-warnings-and-errors ()
  "Turn on display of warnings and errors."
  (interactive)
  (setq js2-mode-show-parse-errors t
        js2-mode-show-strict-warnings t)
  (js2-reparse 'force))

(defun js2-mode-hide-warnings-and-errors ()
  "Turn off display of warnings and errors."
  (interactive)
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)
  (js2-reparse 'force))

(defun js2-mode-toggle-warnings-and-errors ()
  "Toggle the display of warnings and errors.
Some users don't like having warnings/errors reported while they type."
  (interactive)
  (setq js2-mode-show-parse-errors (not js2-mode-show-parse-errors)
        js2-mode-show-strict-warnings (not js2-mode-show-strict-warnings))
  (if (called-interactively-p 'any)
      (message "warnings and errors %s"
               (if js2-mode-show-parse-errors
                   "enabled"
                 "disabled")))
  (js2-reparse 'force))

(defun js2-mode-customize ()
  (interactive)
  (customize-group 'js2-mode))

(defun js2-mode-forward-sexp (&optional arg)
  "Move forward across one statement or balanced expression.
With ARG, do it that many times.  Negative arg -N means
move backward across N balanced expressions."
  (interactive "p")
  (setq arg (or arg 1))
  (save-restriction
    (widen) ;; `blink-matching-open' calls `narrow-to-region'
    (js2-reparse)
    (let (forward-sexp-function
          node (start (point)) pos lp rp child)
      (cond
       ;; backward-sexp
       ;; could probably make this better for some cases:
       ;;  - if in statement block (e.g. function body), go to parent
       ;;  - infix exprs like (foo in bar) - maybe go to beginning
       ;;    of infix expr if in the right-side expression?
       ((and arg (minusp arg))
        (dotimes (_ (- arg))
          (js2-backward-sws)
          (forward-char -1)   ; Enter the node we backed up to.
          (when (setq node (js2-node-at-point (point) t))
            (setq pos (js2-node-abs-pos node))
            (let ((parens (js2-mode-forward-sexp-parens node pos)))
              (setq lp (car parens)
                    rp (cdr parens)))
            (when (and lp (> start lp))
              (if (and rp (<= start rp))
                  ;; Between parens, check if there's a child node we can jump.
                  (when (setq child (js2-node-closest-child node (point) lp t))
                    (setq pos (js2-node-abs-pos child)))
                ;; Before both parens.
                (setq pos lp)))
            (let ((state (parse-partial-sexp start pos)))
              (goto-char (if (not (zerop (car state)))
                             ;; Stumble at the unbalanced paren if < 0, or
                             ;; jump a bit further if > 0.
                             (scan-sexps start -1)
                           pos))))
          (unless pos (goto-char (point-min)))))
       (t
        ;; forward-sexp
        (dotimes (_ arg)
          (js2-forward-sws)
          (when (setq node (js2-node-at-point (point) t))
            (setq pos (js2-node-abs-pos node))
            (let ((parens (js2-mode-forward-sexp-parens node pos)))
              (setq lp (car parens)
                    rp (cdr parens)))
            (or
             (when (and rp (<= start rp))
               (if (> start lp)
                   (when (setq child (js2-node-closest-child node (point) rp))
                     (setq pos (js2-node-abs-end child)))
                 (setq pos (1+ rp))))
             ;; No parens or child nodes, looks for the end of the curren node.
             (incf pos (js2-node-len
                        (if (js2-expr-stmt-node-p (js2-node-parent node))
                            ;; Stop after the semicolon.
                            (js2-node-parent node)
                          node))))
            (let ((state (save-excursion (parse-partial-sexp start pos))))
              (goto-char (if (not (zerop (car state)))
                             (scan-sexps start 1)
                           pos))))
          (unless pos (goto-char (point-max)))))))))

(defun js2-mode-forward-sexp-parens (node abs-pos)
  "Return a cons cell with positions of main parens in NODE."
  (cond
   ((or (js2-array-node-p node)
        (js2-object-node-p node)
        (js2-array-comp-node-p node)
        (memq (aref node 0) '(cl-struct-js2-block-node cl-struct-js2-scope)))
    (cons abs-pos (+ abs-pos (js2-node-len node) -1)))
   ((js2-paren-expr-node-p node)
    (let ((lp (js2-node-lp node))
          (rp (js2-node-rp node)))
      (cons (when lp (+ abs-pos lp))
            (when rp (+ abs-pos rp)))))))

(defun js2-node-closest-child (parent point limit &optional before)
  (let* ((parent-pos (js2-node-abs-pos parent))
         (rpoint (- point parent-pos))
         (rlimit (- limit parent-pos))
         (min (min rpoint rlimit))
         (max (max rpoint rlimit))
         found)
    (catch 'done
      (js2-visit-ast
       parent
       (lambda (node _end-p)
         (if (eq node parent)
             t
           (let ((pos (js2-node-pos node)) ;; Both relative values.
                 (end (+ (js2-node-pos node) (js2-node-len node))))
             (when (and (>= pos min) (<= end max)
                        (if before (< pos rpoint) (> end rpoint)))
               (setq found node))
             (when (> end rpoint)
               (throw 'done nil)))
           nil))))
    found))

(defun js2-errors ()
  "Return a list of errors found."
  (and js2-mode-ast
       (js2-ast-root-errors js2-mode-ast)))

(defun js2-warnings ()
  "Return a list of warnings found."
  (and js2-mode-ast
       (js2-ast-root-warnings js2-mode-ast)))

(defun js2-have-errors-p ()
  "Return non-nil if any parse errors or warnings were found."
  (or (js2-errors) (js2-warnings)))

(defun js2-errors-and-warnings ()
  "Return a copy of the concatenated errors and warnings lists.
They are appended:  first the errors, then the warnings.
Entries are of the form (MSG BEG END)."
  (when js2-mode-ast
    (append (js2-ast-root-errors js2-mode-ast)
            (copy-sequence (js2-ast-root-warnings js2-mode-ast)))))

(defun js2-next-error (&optional arg reset)
  "Move to next parse error.
Typically invoked via \\[next-error].
ARG is the number of errors, forward or backward, to move.
RESET means start over from the beginning."
  (interactive "p")
  (if (not (or (js2-errors) (js2-warnings)))
      (message "No errors")
    (when reset
      (goto-char (point-min)))
    (let* ((errs (js2-errors-and-warnings))
           (continue t)
           (start (point))
           (count (or arg 1))
           (backward (minusp count))
           (sorter (if backward '> '<))
           (stopper (if backward '< '>))
           (count (abs count))
           all-errs err)
      ;; Sort by start position.
      (setq errs (sort errs (lambda (e1 e2)
                              (funcall sorter (second e1) (second e2))))
            all-errs errs)
      ;; Find nth error with pos > start.
      (while (and errs continue)
        (when (funcall stopper (cadar errs) start)
          (setq err (car errs))
          (if (zerop (decf count))
              (setq continue nil)))
        (setq errs (cdr errs)))
      ;; Clear for `js2-echo-error'.
      (message nil)
      (if err
          (goto-char (second err))
        ;; Wrap around to first error.
        (goto-char (second (car all-errs)))
        ;; If we were already on it, echo msg again.
        (if (= (point) start)
            (js2-echo-error (point) (point)))))))

(defun js2-down-mouse-3 ()
  "Make right-click move the point to the click location.
This makes right-click context menu operations a bit more intuitive.
The point will not move if the region is active, however, to avoid
destroying the region selection."
  (interactive)
  (when (and js2-move-point-on-right-click
             (not mark-active))
    (let ((e last-input-event))
      (ignore-errors
        (goto-char (cadadr e))))))

(defun js2-mode-create-imenu-index ()
  "Return an alist for `imenu--index-alist'."
  ;; This is built up in `js2-parse-record-imenu' during parsing.
  (when js2-mode-ast
    ;; if we have an ast but no recorder, they're requesting a rescan
    (unless js2-imenu-recorder
      (js2-reparse 'force))
    (prog1
        (js2-build-imenu-index)
      (setq js2-imenu-recorder nil
            js2-imenu-function-map nil))))

(defun js2-mode-find-tag ()
  "Replacement for `find-tag-default'.
`find-tag-default' returns a ridiculous answer inside comments."
  (let (beg end)
    (js2-with-underscore-as-word-syntax
      (save-excursion
        (if (and (not (looking-at "[[:alnum:]_$]"))
                 (looking-back "[[:alnum:]_$]"))
            (setq beg (progn (forward-word -1) (point))
                  end (progn (forward-word 1) (point)))
          (setq beg (progn (forward-word 1) (point))
                end (progn (forward-word -1) (point))))
        (replace-regexp-in-string
         "[\"']" ""
         (buffer-substring-no-properties beg end))))))

(defun js2-mode-forward-sibling ()
  "Move to the end of the sibling following point in parent.
Returns non-nil if successful, or nil if there was no following sibling."
  (let* ((node (js2-node-at-point))
         (parent (js2-mode-find-enclosing-fn node))
         sib)
    (when (setq sib (js2-node-find-child-after (point) parent))
      (goto-char (+ (js2-node-abs-pos sib)
                    (js2-node-len sib))))))

(defun js2-mode-backward-sibling ()
  "Move to the beginning of the sibling node preceding point in parent.
Parent is defined as the enclosing script or function."
  (let* ((node (js2-node-at-point))
         (parent (js2-mode-find-enclosing-fn node))
         sib)
    (when (setq sib (js2-node-find-child-before (point) parent))
      (goto-char (js2-node-abs-pos sib)))))

(defun js2-beginning-of-defun (&optional arg)
  "Go to line on which current function starts, and return t on success.
If we're not in a function or already at the beginning of one, go
to beginning of previous script-level element.
With ARG N, do that N times. If N is negative, move forward."
  (setq arg (or arg 1))
  (if (plusp arg)
      (let ((parent (js2-node-parent-script-or-fn (js2-node-at-point))))
        (when (cond
               ((js2-function-node-p parent)
                (goto-char (js2-node-abs-pos parent)))
               (t
                (js2-mode-backward-sibling)))
          (if (> arg 1)
              (js2-beginning-of-defun (1- arg))
            t)))
    (when (js2-end-of-defun)
      (js2-beginning-of-defun (if (>= arg -1) 1 (1+ arg))))))

(defun js2-end-of-defun ()
  "Go to the char after the last position of the current function
or script-level element."
  (let* ((node (js2-node-at-point))
         (parent (or (and (js2-function-node-p node) node)
                     (js2-node-parent-script-or-fn node)))
         script)
    (unless (js2-function-node-p parent)
      ;; Use current script-level node, or, if none, the next one.
      (setq script (or parent node)
            parent (js2-node-find-child-before (point) script))
      (when (or (null parent)
                (>= (point) (+ (js2-node-abs-pos parent)
                               (js2-node-len parent))))
        (setq parent (js2-node-find-child-after (point) script))))
    (when parent
      (goto-char (+ (js2-node-abs-pos parent)
                    (js2-node-len parent))))))

(defun js2-mark-defun (&optional allow-extend)
  "Put mark at end of this function, point at beginning.
The function marked is the one that contains point.

Interactively, if this command is repeated,
or (in Transient Mark mode) if the mark is active,
it marks the next defun after the ones already marked."
  (interactive "p")
  (let (extended)
    (when (and allow-extend
               (or (and (eq last-command this-command) (mark t))
                   (and transient-mark-mode mark-active)))
      (let ((sib (save-excursion
                   (goto-char (mark))
                   (if (js2-mode-forward-sibling)
                       (point)))))
        (if sib
            (progn
              (set-mark sib)
              (setq extended t))
          ;; no more siblings - try extending to enclosing node
          (goto-char (mark t)))))
   (when (not extended)
     (let ((node (js2-node-at-point (point) t)) ; skip comments
           ast fn stmt parent beg end)
       (when (js2-ast-root-p node)
         (setq ast node
               node (or (js2-node-find-child-after (point) node)
                        (js2-node-find-child-before (point) node))))
       ;; only mark whole buffer if we can't find any children
       (if (null node)
           (setq node ast))
       (if (js2-function-node-p node)
           (setq parent node)
         (setq fn (js2-mode-find-enclosing-fn node)
               stmt (if (or (null fn)
                            (js2-ast-root-p fn))
                        (js2-mode-find-first-stmt node))
               parent (or stmt fn)))
       (setq beg (js2-node-abs-pos parent)
             end (+ beg (js2-node-len parent)))
       (push-mark beg)
       (goto-char end)
       (exchange-point-and-mark)))))

(defun js2-narrow-to-defun ()
  "Narrow to the function enclosing point."
  (interactive)
  (let* ((node (js2-node-at-point (point) t))  ; skip comments
         (fn (if (js2-script-node-p node)
                 node
               (js2-mode-find-enclosing-fn node)))
         (beg (js2-node-abs-pos fn)))
    (unless (js2-ast-root-p fn)
      (narrow-to-region beg (+ beg (js2-node-len fn))))))

(provide 'js2-mode)

;;; js2-mode.el ends here
