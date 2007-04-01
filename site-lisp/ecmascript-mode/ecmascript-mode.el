;;; ecmascript-mode.el --- major mode for editing ECMAScript code

;; Copyright (c) 2004-2005 David Lindquist <david.lindquist@gmail.com>

;; Author: David Lindquist <david.lindquist@gmail.com>
;; Keywords: languages ecmascript javascript

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; ecmascript-mode is a basic major mode for editing code conforming
;; to the ECMA-262 standard (3rd edition). The most notable languages
;; employing all or most of the standard are JavaScript (Netscape),
;; JScript (Microsoft), and ActionScript (Macromedia).

;; See also:
;; http://www.ecma-international.org/publications/files/ecma-st/ECMA-262.pdf

;;; History:

;; 2005-11-24 david Fixed void function error and corrected font-lock
;;                  problems that appeared in emacs version 21.3.50.1
;;                  (probably due to changes in java-mode).
;; 2004-??-?? david Initial release.

;;; Code:

(require 'font-lock)
(require 'cc-mode)
(eval-when-compile
  (require 'regexp-opt))

(defconst ecmascript-mode-version "1.1"
  "ECMAScript Mode version number.")

(defgroup ecmascript nil
  "Major mode for editing ECMAScript code."
  :group 'languages
  :prefix "ecmascript-")

(defcustom ecmascript-mode-hook nil
  "Hook for customizing `ecmascript-mode'."
  :group 'ecmascript
  :type 'hook)

(defvar ecmascript-mode-map (c-make-inherited-keymap)
  "Keymap used in `ecmascript-mode' buffers.")

;;;###autoload
(define-derived-mode ecmascript-mode java-mode "ECMAScript"
  "Major mode for editing ECMAScript code.

This mode is derived from `java-mode'; see its documentation for further
information.

\\{ecmascript-mode-map}"
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((;; comment out the lines below to adjust
           ;; syntax highlighting gaudiness
           ecmascript-font-lock-keywords-1
           ecmascript-font-lock-keywords-2
           ecmascript-font-lock-keywords-3
           )
          nil nil ((?_ . "w") (?$ . "w")) nil))

  (easy-menu-define c-ecmascript-menu ecmascript-mode-map
    "ECMAScript Mode Commands" (c-mode-menu "ECMAScript"))
  )

(defvar ecmascript-font-lock-default-face 'ecmascript-font-lock-default-face)

(defconst ecmascript-font-lock-keywords-1
  (append
   java-font-lock-keywords-1
   (list

    '("\\<\\(function\\)\\>\\(?:\\s-+\\(\\sw+\\)\\)?"
      (1 font-lock-keyword-face t)
      (2 font-lock-function-name-face nil t))

    ;; need to fix this to handle: var a, b;
    '("\\<\\(var\\)\\>\\(?:\\s-+\\(\\sw+\\)\\)?"
      (1 font-lock-keyword-face t)
      (2 font-lock-variable-name-face nil t))
    ))
  "Subdued level highlighting for ECMAScript mode.")

(defconst ecmascript-font-lock-keywords-2
  (append
   java-font-lock-keywords-2
   ecmascript-font-lock-keywords-1
   (list

    '("\\<\\(debugger\\|delete\\|export\\|in\\|typeof\\|with\\)\\>"
      (1 font-lock-keyword-face t))

    (list (concat
           "\\<\\("
           (mapconcat 'identity java-font-lock-extra-types nil)
           "\\)\\>\\.")
          '(1 font-lock-type-face nil t))

    ;; In Java, `void' is a type. In ECMAScript, it is an operator.
    ;; This overrides the inherited notion of keyword `void'.
    '("\\<\\(void\\)\\>\\(?:\\s-+\\(\\sw+\\)\\)?"
      (1 font-lock-keyword-face t)
      (2 ecmascript-font-lock-default-face t t))

    ;; Value properties of the global object
    '("\\<\\(Infinity\\|NaN\\|undefined\\)\\>" 0 font-lock-constant-face t)

    ;; Properties of the Number constructor
    (list (concat
           "\\<Number\\."
           (regexp-opt
            '("MAX_VALUE" "MIN_VALUE" "NaN" "NEGATIVE_INFINITY"
              "POSITIVE_INFINITY") t)
           "\\>")
          '(1 font-lock-constant-face))

    ;; Value properties of the Math object
    (list (concat
           "\\<Math\\."
           (regexp-opt
            '("E" "LN10" "LN2" "LOG2E" "LOG10E" "PI" "SQRT1_2" "SQRT2") t)
           "\\>")
          '(1 font-lock-constant-face))
    ))
  "Medium level highlighting for ECMAScript mode.")

(defconst ecmascript-font-lock-keywords-3
  (append
   java-font-lock-keywords-3
   ecmascript-font-lock-keywords-2
   (list

    ;; Properties of the Date constructor
    '("\\<Date\\.\\(parse\\|UTC\\)\\>" 1 font-lock-builtin-face)

    ;; Function properties of the Math object
    (list (concat
           "\\<Math\\."
           (regexp-opt
            '("abs" "acos" "asin" "atan" "atan2" "ceil" "cos" "exp" "floor"
              "log" "max" "min" "pow" "random" "round" "sin" "sqrt" "tan") t)
           "\\>")
          '(1 font-lock-builtin-face))

    (list (regexp-opt
           '(;; URI handling function properties
             "decodeURI" "decodeURIComponent" "encodeURI" "encodeURIComponent"
             ;; Function properties of the global object
             "eval" "isFinite" "isNaN" "parseFloat" "parseInt") 'words)
          '(0 font-lock-builtin-face))

    (list (concat
           "\\."
           (regexp-opt
            '(;; Properties of the Object prototype object
              "hasOwnProperty" "isPrototypeOf" "propertyIsEnumerable"
              "toLocaleString" "toString" "valueOf"
              ;; Properties of the Function prototype object
              "apply" "call"
              ;; Properties of the Array prototype object
              "concat" "join" "pop" "push" "reverse" "shift" "slice" "sort"
              "splice" "unshift"
              ;; Properties of the String prototype object
              "charAt" "charCodeAt" "fromCharCode" "indexOf" "lastIndexOf"
              "localeCompare" "match" "replace" "search" "split" "substring"
              "toLocaleLowerCase" "toLocaleUpperCase" "toLowerCase"
              "toUpperCase"
              ;; Properties of the Number prototype object
              "toExponential" "toFixed" "toPrecision"
              ;; Properties of the Date prototype object
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
              ;; Properties of the RegExp prototype object
              "exec" "test"
              ) t)
           "\\>")
          '(1 font-lock-builtin-face))
    ))
  "Gaudy level highlighting for ECMAScript mode.")

(provide 'ecmascript-mode)

;;; ecmascript-mode.el ends here
