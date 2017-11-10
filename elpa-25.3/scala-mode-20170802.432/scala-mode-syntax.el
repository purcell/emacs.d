;;;; scala-mode-syntax.el - Major mode for editing scala, syntax
;;; Copyright (c) 2012 Heikki Vesalainen
;;; For information on the License, see the LICENSE file

;;; Based on Scala Language Specification (SLS) Version 2.9

;;;;
;;;; Scala syntax regular expressions
;;;;

;;; Based on the Scala language specification 2.9.  Note: order is not
;;; the same as in the document, as here things are declared before
;;; used.

;;; A note on naming. Things that end with '-re' are regular
;;; expressions.  Things that end with '-group' are regular expression
;;; character groups without the enclosing [], i.e. they are not
;;; regular expressions, but can be used in declaring one.

;; single letter matching groups (Chapter 1)
(defconst scala-syntax:hexDigit-group "0-9A-Fa-f")
(defconst scala-syntax:UnicodeEscape-re (concat "\\\\u[" scala-syntax:hexDigit-group "]\\{4\\}"))

(defconst scala-syntax:upper-group "[:upper:]\\$") ;; missing _ to make ids work
(defconst scala-syntax:upperAndUnderscore-group (concat "_" scala-syntax:upper-group ))
(defconst scala-syntax:lower-group "[:lower:]")
(defconst scala-syntax:letter-group (concat scala-syntax:lower-group scala-syntax:upper-group)) ;; TODO: add Lt, Lo, Nl
(defconst scala-syntax:digit-group "0-9")
(defconst scala-syntax:letterOrDigit-group (concat
                                            scala-syntax:upperAndUnderscore-group
                                            scala-syntax:lower-group
                                            scala-syntax:digit-group))
(defconst scala-syntax:opchar-safe-group "!%&*+/?\\\\^|~-") ;; TODO: Sm, So
(defconst scala-syntax:opchar-unsafe-group "#:<=>@")
(defconst scala-syntax:opchar-group (concat scala-syntax:opchar-unsafe-group
                                            scala-syntax:opchar-safe-group))

;; Scala delimiters (Chapter 1), but no quotes
(defconst scala-syntax:delimiter-group ".,;")

;; Integer Literal (Chapter 1.3.1)
(defconst scala-syntax:nonZeroDigit-group "1-9")
(defconst scala-syntax:octalDigit-group "0-7")
(defconst scala-syntax:decimalNumeral-re
  (concat "0"
          "\\|[" scala-syntax:nonZeroDigit-group "][" scala-syntax:digit-group "]*"))
(defconst scala-syntax:hexNumeral-re (concat "0x[" scala-syntax:hexDigit-group "]+"))
(defconst scala-syntax:octalNumeral-re (concat "0[" scala-syntax:octalDigit-group "]+"))
(defconst scala-syntax:integerLiteral-re (concat "-?" ;; added from definition of literal
                                                 "\\(" scala-syntax:hexNumeral-re
                                                 "\\|" scala-syntax:octalNumeral-re
                                                 "\\|" scala-syntax:decimalNumeral-re
                                                 "\\)[Ll]?"))


;; Floating Point Literal (Chapter 1.3.2)
(defconst scala-syntax:exponentPart-re (concat "\\([eE][+-]?[" scala-syntax:digit-group "]+\\)"))
(defconst scala-syntax:floatType-re "[fFdD]")
(defconst scala-syntax:floatingPointLiteral-re
  (concat "-?" ;; added from definition of literal
          "\\([" scala-syntax:digit-group "]+\\.[" scala-syntax:digit-group "]*"
          scala-syntax:exponentPart-re "?" scala-syntax:floatType-re "?"
          "\\|" "\\.[" scala-syntax:digit-group "]+"
          scala-syntax:exponentPart-re "?" scala-syntax:floatType-re "?"
          "\\|" "[" scala-syntax:digit-group "]+" scala-syntax:exponentPart-re
          "\\|" "[" scala-syntax:digit-group "]+" scala-syntax:floatType-re "\\)"))

(defconst scala-syntax:number-safe-start-re
  (concat "[^_" scala-syntax:letter-group "]"))

;; Boolean Literals (Chapter 1.3.3)
(defconst scala-syntax:booleanLiteral-re "true|false")

;; Escape Sequences (Chapter 1.3.6)
(defconst scala-syntax:escapeSequence-re "\\\\['btnfr\"\\\\]")

;; Octal Escape Sequences (Chapter 1.3.6)
(defconst scala-syntax:octalEscape-re (concat "\\\\[" scala-syntax:octalDigit-group "\\]\\{1,3\\}"))

;; Character Literals (Chapter 1.3.4)
(defconst scala-syntax:characterLiteral-re
  (concat "\\('\\)\\(" "[^\\\\]" ;; should be just printable char, but this is faster
          "\\|" scala-syntax:escapeSequence-re
          "\\|" scala-syntax:octalEscape-re
          "\\|" scala-syntax:UnicodeEscape-re "\\)\\('\\)"))

(defconst scala-syntax:string-escape-re
  (concat scala-syntax:escapeSequence-re
          "\\|" scala-syntax:octalEscape-re
          "\\|" scala-syntax:UnicodeEscape-re))

;; String Literals (Chapter 1.3.5)
(defconst scala-syntax:stringElement-re
  (concat "\\(" "[^\n\"\\\\]"
          "\\|" scala-syntax:string-escape-re  "\\)"))
(defconst scala-syntax:oneLineStringLiteral-re (concat "\\(\"\\)" scala-syntax:stringElement-re "*\\(\"\\)"))
(defconst scala-syntax:multiLineStringLiteral-start-re
  "\\(\"\\)\"\"\\(\"?\"?[^\"]\\)*")
(defconst scala-syntax:multiLineStringLiteral-end-re
  "\"\"+\\(\"\\)")
(defconst scala-syntax:multiLineStringLiteral-re
  (concat scala-syntax:multiLineStringLiteral-start-re
          scala-syntax:multiLineStringLiteral-end-re))
(defconst scala-syntax:stringLiteral-re
  (concat "\\(" scala-syntax:multiLineStringLiteral-re
          "\\|" scala-syntax:oneLineStringLiteral-re "\\)" ))

;; If you change this or any of the used regex, be sure to
;; maintain this or update propertize function accordingly:
;; group 1 = char start, 3 = char end
;; group 4 = multi-line string start, 6 = end
;; group 7 = string start, 9 = end
(defconst scala-syntax:relaxed-char-and-string-literal-re
  (concat scala-syntax:characterLiteral-re
          "\\|" scala-syntax:multiLineStringLiteral-start-re
          "\\(?:" scala-syntax:multiLineStringLiteral-end-re "\\)?"
          "\\|\\(\"\\)" "\\(\\\\.\\|[^\"\n\\]\\)*" "\\(\"\\)"))

;; Identifiers (Chapter 1.1)
(defconst scala-syntax:op-re
  (concat "[" scala-syntax:opchar-group "]+" ))
(defconst scala-syntax:idrest-re
  ;; Eagerness of regexp causes problems with _. The following is a workaround,
  ;; but the resulting regexp matches only what SLS demands.
  (concat "\\(" "[_]??" "[" scala-syntax:letter-group scala-syntax:digit-group "]+" "\\)*"
          "\\(" "_+" scala-syntax:op-re "\\|" "_" "\\)?"))
(defconst scala-syntax:varid-re (concat "[" scala-syntax:lower-group "]" scala-syntax:idrest-re))
(defconst scala-syntax:capitalid-re (concat "[" scala-syntax:upperAndUnderscore-group "]" scala-syntax:idrest-re))
;; alphaid introduce by SIP11
(defconst scala-syntax:alphaid-re (concat "\\(" "[" scala-syntax:lower-group scala-syntax:upperAndUnderscore-group "]" scala-syntax:idrest-re "\\)"))
(defconst scala-syntax:plainid-re (concat "\\(" scala-syntax:alphaid-re "\\|" scala-syntax:op-re "\\)"))
;; stringlit is referred to, but not defined Scala Language Specification 2.9
;; we define it as consisting of anything but '`' and newline
(defconst scala-syntax:stringlit-re "[^`\n\r]")
(defconst scala-syntax:quotedid-re (concat "`" scala-syntax:stringlit-re "+`"))
(defconst scala-syntax:id-re (concat "\\(" scala-syntax:plainid-re
                              "\\|" scala-syntax:quotedid-re "\\)"))
(defconst scala-syntax:id-first-char-group
  (concat scala-syntax:lower-group
          scala-syntax:upperAndUnderscore-group
          scala-syntax:opchar-group))

;; Symbol literals (Chapter 1.3.7)
(defconst scala-syntax:symbolLiteral-re
  ;; must end with non-' to not conflict with scala-syntax:characterLiteral-re
  (concat "\\('" scala-syntax:plainid-re "\\)\\([^']\\|$\\)"))

;; Literals (Chapter 1.3)
(defconst scala-syntax:literal-re
  (concat "\\(" scala-syntax:integerLiteral-re
          "\\|" scala-syntax:floatingPointLiteral-re
          "\\|" scala-syntax:booleanLiteral-re
          "\\|" scala-syntax:characterLiteral-re
          "\\|" scala-syntax:stringLiteral-re
          "\\|" scala-syntax:symbolLiteral-re
          "\\|" "null" "\\)"))

(defconst scala-syntax:interpolation-re
  (concat "\\(" "\\$"  scala-syntax:id-re "\\|" "\\${[^}\n\\\\]*}" "\\)"))

(defun scala-syntax:interpolation-matcher (end)
  (let* ((pos nil)
         (syntax nil)
         (str-start nil)
         (char-before-str nil))
    (while (and
            (setq pos (re-search-forward scala-syntax:interpolation-re end t))
            (setq syntax (syntax-ppss pos))
            (if (nth 3 syntax) ;; "is string"
                (progn
                  (setq str-start (nth 8 syntax))
                  ;; s"foo"
                  ;; ^-- `char-before-str', must be identifier
                  (setq char-before-str (char-after (1- str-start)))
                  ;; break if match
                  (null (string-match-p
                         scala-syntax:id-re (string char-before-str))))
              t))) ;; keep going
    pos))

;; Paths (Chapter 3.1)
;; emacs has a problem with these regex, don't use them
;; (defconst scala-syntax:classQualifier-re (concat "[[]" scala-syntax:id-re "[]]"))
;; (defconst scala-syntax:stableId-re
;;   (concat "\\(\\(" "this"
;;           "\\|" "super" scala-syntax:classQualifier-re
;;           "\\|" scala-syntax:id-re
;;           "\\)\\.\\)*"
;;           scala-syntax:id-re))
;; (defconst scala-syntax:path-re
;;   (concat "\\(" scala-syntax:stableId-re
;;           "\\|" "\\(" scala-syntax:id-re "\\." "\\)?" "this" "\\)"))

(defun scala-syntax:looking-at-super ()
  (save-excursion
    (when (looking-at "\\<super\\>")
      (let ((beg (match-beginning 0)))
        (when (and (goto-char (match-end 0))
                   (or (when (= (char-after) ?.)
                         (forward-char)
                         t)
                       (and (when (and (not (eobp)) (= (char-after) ?\[))
                              (forward-char)
                              t)
                            (progn (scala-syntax:skip-forward-ignorable)
                                   (looking-at scala-syntax:id-re))
                            (progn (goto-char (match-end 0))
                                   (scala-syntax:skip-forward-ignorable)
                                   (when (and (not (eobp)) (= (char-after) ?\]))
                                     (forward-char)
                                     t))
                            (when (and (not (eobp)) (= (char-after) ?.))
                              (forward-char)
                              t)))
                   (looking-at scala-syntax:id-re))
          (set-match-data `(,beg ,(match-end 0)))
          t)))))

(defun scala-syntax:looking-at-stableIdOrPath (&optional path-p beg)
  (unless beg (setq beg (point)))
  (save-excursion
    (cond ((looking-at "\\<this\\>")
           (goto-char (match-end 0))
           (if (and (not (eobp)) (= (char-after) ?.))
               (progn (forward-char)
                      (scala-syntax:looking-at-stableIdOrPath path-p beg))
             path-p))
          ((or (scala-syntax:looking-at-super)
               (and (not (or (looking-at scala-syntax:keywords-unsafe-re)
                             (scala-syntax:looking-at-reserved-symbol nil)))
                    (looking-at scala-syntax:id-re)))
           (goto-char (match-end 0))
           (if (and (not (eobp)) (= (char-after) ?.))
               (progn (forward-char)
                      (scala-syntax:looking-at-stableIdOrPath path-p beg))
             (set-match-data `(,beg ,(match-end 0)))
             (point))))))

(defun scala-syntax:looking-at-simplePattern-beginning ()
  (or (looking-at "[_(]")
      (looking-at scala-syntax:literal-re)
      (scala-syntax:looking-at-stableIdOrPath)))


(defun scala-syntax:regexp-for-id (id)
  (let ((prefix-regex
         (if (string-match scala-syntax:alphaid-re id)
             "\\b" (concat "\\(^\\|[^" scala-syntax:opchar-group "]\\)")))
        (suffix-regex
         (if (string-match scala-syntax:op-re (substring id -1 nil))
             (concat "\\([^" scala-syntax:opchar-group "]\\|$\\)") "\\b")))
    (concat prefix-regex id suffix-regex)))

;;;
;;; Other regular expressions
;;;

(defconst scala-syntax:preamble-start-re
  "\#\!")

(defconst scala-syntax:empty-line-re
  "^\\s *$")

(defconst scala-syntax:comment-start-re
  "/[/*]")

(defconst scala-syntax:end-of-code-line-re
  (concat "\\([ ]\\|$\\|" scala-syntax:comment-start-re "\\)")
  "A special regexp that can be concatenated to an other regular
  expression when used with scala-syntax:looking-back-token. Not
  meaningfull in other contexts.")

(defconst scala-syntax:path-keywords-unsafe-re
  (regexp-opt '("super" "this") 'words))

(defconst scala-syntax:path-keywords-re
  (concat "\\(^\\|[^`'_]\\)\\(" scala-syntax:path-keywords-unsafe-re "\\)"))

(defconst scala-syntax:value-keywords-unsafe-re
  (regexp-opt '("false" "null" "true") 'words))

(defconst scala-syntax:value-keywords-re
  (concat "\\(^\\|[^`'_]\\)\\(" scala-syntax:value-keywords-unsafe-re "\\)"))

(defconst scala-syntax:other-keywords-unsafe-re
  (regexp-opt '("abstract" "case" "catch" "class" "def" "do" "else" "extends"
                "final" "finally" "for" "forSome" "if" "implicit" "import"
                "lazy" "match" "new" "object" "override" "package" "private"
                "protected" "return" "sealed" "throw" "trait" "try" "type"
                "val" "var" "while" "with" "yield" "inline") 'words))

(defconst scala-syntax:other-keywords-re
  (concat "\\(^\\|[^`'_]\\)\\(" scala-syntax:other-keywords-unsafe-re "\\)"))

(defconst scala-syntax:keywords-unsafe-re
  (concat "\\(" scala-syntax:path-keywords-unsafe-re
          "\\|" scala-syntax:value-keywords-unsafe-re
          "\\|" scala-syntax:other-keywords-unsafe-re
          "\\)"))

;; TODO: remove
;; (defconst scala-syntax:keywords-re
;;   (concat "\\(^\\|[^`'_]\\)\\(" scala-syntax:value-keywords-unsafe-re
;;           "\\|" scala-syntax:path-keywords-unsafe-re
;;           "\\|" scala-syntax:other-keywords-unsafe-re "\\)"))


(defconst scala-syntax:after-reserved-symbol-underscore-re
  (concat "$\\|" scala-syntax:comment-start-re
          "\\|[^" scala-syntax:letterOrDigit-group "]"))

(defconst scala-syntax:reserved-symbol-underscore-re
  ;; reserved symbol _
  (concat "\\(^\\|[^" scala-syntax:letterOrDigit-group "]\\)"
          "\\(_\\)"
          "\\(" scala-syntax:after-reserved-symbol-underscore-re "\\)"))

(defconst scala-syntax:reserved-symbols-unsafe-re
  ;; reserved symbols. The regexp is unsafe as it does not
  ;; check the context.
  "\\([:#@\u21D2\u2190]\\|=>?\\|<[:%!?\\-]\\|>:\\)" )

(defconst scala-syntax:double-arrow-unsafe-re
  "\\(=>\\|\u21D2\\)")

(defconst scala-syntax:after-reserved-symbol-re
  (concat "\\($\\|" scala-syntax:comment-start-re
          "\\|[^" scala-syntax:opchar-group "]\\)"))

(defconst scala-syntax:reserved-symbols-re
  ;; reserved symbols and XML starts ('<!' and '<?')
  (concat "\\(^\\|[^" scala-syntax:opchar-group "]\\)"
          scala-syntax:reserved-symbols-unsafe-re
          "\\(" scala-syntax:after-reserved-symbol-re "\\)"))

(defconst scala-syntax:colon-re
  (concat "\\(^\\|[^" scala-syntax:opchar-group "]\\)"
          "\\(:\\)"
          "\\(" scala-syntax:after-reserved-symbol-re "\\)"))


(defconst scala-syntax:override-unsafe-re
  (regexp-opt '("override") 'words))

(defconst scala-syntax:override-re
  (concat "\\(^\\|[^`'_]\\)\\(" scala-syntax:override-unsafe-re "\\)"))

(defconst scala-syntax:abstract-unsafe-re
  (regexp-opt '("abstract") 'words))

(defconst scala-syntax:abstract-re
  (concat "\\(^\\|[^`'_]\\)\\(" scala-syntax:abstract-unsafe-re "\\)"))

(defconst scala-syntax:final-unsafe-re
  (regexp-opt '("final") 'words))

(defconst scala-syntax:final-re
  (concat "\\(^\\|[^`'_]\\)\\(" scala-syntax:final-unsafe-re "\\)"))

(defconst scala-syntax:sealed-unsafe-re
  (regexp-opt '("sealed") 'words))

(defconst scala-syntax:sealed-re
  (concat "\\(^\\|[^`'_]\\)\\(" scala-syntax:sealed-unsafe-re "\\)"))

(defconst scala-syntax:implicit-unsafe-re
  (regexp-opt '("implicit") 'words))

(defconst scala-syntax:implicit-re
  (concat "\\(^\\|[^`'_]\\)\\(" scala-syntax:implicit-unsafe-re "\\)"))

(defconst scala-syntax:lazy-unsafe-re
  (regexp-opt '("lazy") 'words))

(defconst scala-syntax:lazy-re
  (concat "\\(^\\|[^`'_]\\)\\(" scala-syntax:lazy-unsafe-re "\\)"))

(defconst scala-syntax:private-unsafe-re
  (regexp-opt '("private") 'words))

(defconst scala-syntax:private-re
  (concat "\\(^\\|[^`'_]\\)\\(" scala-syntax:private-unsafe-re "\\)"))

(defconst scala-syntax:protected-unsafe-re
  (regexp-opt '("protected") 'words))

(defconst scala-syntax:protected-re
  (concat "\\(^\\|[^`'_]\\)\\(" scala-syntax:protected-unsafe-re "\\)"))

(defconst scala-syntax:modifiers-unsafe-re
  (regexp-opt '("override" "abstract" "final" "sealed" "implicit" "lazy"
                "private" "protected") 'words))

(defconst scala-syntax:modifiers-re
  (concat "\\(^\\|[^`'_]\\)\\(" scala-syntax:modifiers-unsafe-re "\\)"))

(defconst scala-syntax:body-start-re
  (concat "=" scala-syntax:end-of-code-line-re)
  "A regexp for detecting if a line ends with '='")

(defconst scala-syntax:list-keywords-re
  (regexp-opt '("var" "val" "import") 'words)
  ("Keywords that can start a list"))

(defconst scala-syntax:case-re
  "\\<case\\>")

(defconst scala-syntax:for-re
  "\\<for\\>")

(defconst scala-syntax:class-or-object-re
  (regexp-opt '("class" "object") 'words))


;;;;
;;;; Character syntax table and related syntax-propertize functions
;;;;

;;; The syntax table relies havily on the syntax-propertize-functions being
;;; run. Hence this syntax requires at least emacs 24, which introduced
;;; this new facility.

(defvar scala-syntax:syntax-table nil
  "Syntax table used in `scala-mode' buffers.")
(when (not scala-syntax:syntax-table)
  (let ((syntab (make-syntax-table)))
    ;; 1. start by reseting the syntax table: only (){}[] are
    ;; parentheses, so all others marked as parentheses in the parent
    ;; table must be marked as symbols, nothing is a punctuation
    ;; unless otherwise stated
    (map-char-table
     #'(lambda (key value)
         (when (or (= (syntax-class value) 4) ; open
                   (= (syntax-class value) 5) ; close
                   (= (syntax-class value) 1)) ; punctuation
           (modify-syntax-entry key "_" syntab)))
     (char-table-parent syntab))

    ;; Below 'space', everything is either illegal or whitespace.
    ;; Consider as whitespace, unless otherwise stated below.
    (modify-syntax-entry '(0 . 32) " " syntab)

    ;; The scala parentheses
    (modify-syntax-entry ?\( "()" syntab)
    (modify-syntax-entry ?\[ "(]" syntab)
    (modify-syntax-entry ?\{ "(}" syntab)
    (modify-syntax-entry ?\) ")(" syntab)
    (modify-syntax-entry ?\] ")[" syntab)
    (modify-syntax-entry ?\} "){" syntab)

    ;; _ is upper-case letter, but will be modified to be symbol
    ;; constituent when in reserved symbol position by
    ;; syntax-propertize-function
    (modify-syntax-entry ?\_ "w" syntab)

    ;; by default all opchars are punctuation, but they will be
    ;; modified by syntax-propertize-function to be symbol
    ;; constituents when a part of varid or capitalid
    (dolist (char (mapcar 'identity "!#%&*+/:<=>?@^|~-\u21D2\u2190")) ;; TODO: Sm, So
      (modify-syntax-entry char "." syntab))

    ;; for clarity, the \ is alone here and not in the string above
    (modify-syntax-entry ?\\ "." syntab)

    ;; scala strings cannot span lines, so we mark
    ;; " as punctuation, but do the real stuff
    ;; in syntax-propertize-function for properly
    ;; formatted strings.
    (modify-syntax-entry ?\" "." syntab)

    ;; backquote is given paired delimiter syntax so that
    ;; quoted ids are parsed as one sexp. Fontification
    ;; is done separately.
    (modify-syntax-entry ?\` "$" syntab)

    ;; ' is considered an expression prefix, since it can
    ;; both start a Symbol and is a char quote. It
    ;; will be given string syntax by syntax-propertize-function
    ;; for properly formatted char literals.
    (modify-syntax-entry ?\' "'" syntab)

    ;; punctuation as specified by SLS
    (modify-syntax-entry ?\. "." syntab)
    (modify-syntax-entry ?\; "." syntab)
    (modify-syntax-entry ?\, "." syntab)

    ;; comments
    ;; the `n' means that comments can be nested
    (modify-syntax-entry ?\/  ". 124b" syntab)
    (modify-syntax-entry ?\*  ". 23n"   syntab)
    (modify-syntax-entry ?\n  "> b" syntab)
    (modify-syntax-entry ?\r  "> b" syntab)

    (setq scala-syntax:syntax-table syntab)))

(defun scala-syntax:propertize-extend-region (start end)
  "See syntax-propertize-extend-region-functions"
  ;; nothing yet
  nil)

(defmacro scala-syntax:put-syntax-table-property (match-group value)
  "Add 'syntax-table entry 'value' to the region marked by the
match-group 'match-group'"
  `(put-text-property (match-beginning ,match-group)
                      (match-end ,match-group)
                      'syntax-table
                      ,value))

(defun scala-syntax:propertize-char-and-string-literals (start end)
  "Mark start and end of character literals as well as one-line
and multi-line string literals. One-line strings and characters
use syntax class 7 (string quotes), while multi-line strings are
marked with 15 (generic string delimiter). Multi-line string
literals are marked even if they are unbalanced. One-line string
literals have to be balanced to get marked. This means invalid
characters and one-line strings will not be fontified."

  (let* ((string-state (nth 3 (syntax-ppss start)))
         (unbalanced-p (eq string-state t)))

    (if (and string-state (not unbalanced-p))
        ;; a normal string is open, let's de-propertize
        (remove-text-properties start end '(syntax-table nil))
      (save-excursion
        (goto-char start)
        ;; close the closing for the unbalanced multi-line literal
        (when (and unbalanced-p
                   (re-search-forward scala-syntax:multiLineStringLiteral-end-re end t))
          (scala-syntax:put-syntax-table-property 1 '(15 . nil)))
        ;; match any balanced one-line or multi-line literals
        (catch 'break
          (while (re-search-forward
                  scala-syntax:relaxed-char-and-string-literal-re end t)
            ;; Expects the following groups:
            ;; group 1 = char start, 3 = char end
            ;; group 4 = multi-line string start, 6 = end
            ;; group 7 = string start, 9 = end
            (cond
             ((match-beginning 1)
              (scala-syntax:put-syntax-table-property 1 '(7 . nil))
              (scala-syntax:put-syntax-table-property 3 '(7 . nil)))
             ((match-beginning 4) ;; start of multi-line literal
              (scala-syntax:put-syntax-table-property 4 '(15 . nil))
              (if (match-beginning 6)
                  ;; balanced multi-line
                  (scala-syntax:put-syntax-table-property 6 '(15 . nil))
                ;; un-balanced multi-line
                (throw 'break nil)))
             ((or
               ;; normal string, content is not empty
               (match-beginning 8)
               ;; empty string at line end
               (= (match-end 9) (line-end-position))
               ;; no " after empty string
               (not (= (char-after (match-end 10)) ?\")))
              (when (save-excursion
                      (goto-char (match-beginning 7))
                      ;; really valid?
                      (looking-at-p scala-syntax:oneLineStringLiteral-re))
                (scala-syntax:put-syntax-table-property 7 '(7 . nil))
                (scala-syntax:put-syntax-table-property 9 '(7 . nil))))
             (t (throw 'break nil)))))))))

(defun scala-syntax:propertize-shell-preamble (start end)
  "Mark a shell preamble (#!) at the beginning of a script as a line comment."
  (save-excursion
    (goto-char start)
    (when (and (= start 1)
               (looking-at scala-syntax:preamble-start-re))
      (scala-syntax:put-syntax-table-property 0 '(11 . nil))
      (end-of-line)
      (when (re-search-forward "\n" end t)
        (scala-syntax:put-syntax-table-property 0 '(12 . nil))))))

(defun scala-syntax:propertize-underscore-and-idrest (start end)
  "Mark all underscores (_) as symbol constituents (syntax 3) or
upper case letter (syntax 2). Also mark opchars in idrest as
symbol constituents (syntax 3)."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "_" end t)
      (let ((match-beg (match-beginning 0))
            (match-end (match-end 0)))
        (put-text-property
         match-beg match-end 'syntax-table
         (if (= match-beg (line-beginning-position))
             (if (looking-at scala-syntax:after-reserved-symbol-underscore-re)
                 '(3 . nil) ; symbol constituent
               '(2 . nil)) ; word syntax
           (save-excursion
             (goto-char (1- match-beg))
             (if (looking-at scala-syntax:reserved-symbol-underscore-re)
                 '(3 . nil) ; symbol constituent
               ;; check for opchars that should be marked as symbol constituents (3)
               (goto-char match-end)
               (when (looking-at scala-syntax:op-re)
                 (scala-syntax:put-syntax-table-property 0 '(3 . nil)))
               '(3 . nil))))))))) ;; symbol constituent syntax (3) also for the '_'

(defun scala-syntax:propertize-special-symbols (start end)
  (save-excursion
    (goto-char start)
    (while (re-search-forward (concat "[" scala-syntax:opchar-group "]" scala-syntax:op-re) end t)
      (let ((match-beg (match-beginning 0))
            (match-end (match-end 0))
            (match (match-string 0)))
        (unless (or
                 (string-suffix-p "*/" match)
                 (member match '("</"))
                 (member 0 (mapcar (lambda (regexp) (string-match regexp match)) '("^*+/$" "^//.*$" "^/\\*+$")))
                 (equal 2 (syntax-class (syntax-after match-end)))
                 (equal 2 (syntax-class (syntax-after (1- match-beg)))))
          (put-text-property match-beg match-end 'syntax-table '(3 . nil)))))))

(defun scala-syntax:propertize-quotedid (start end)
  "Mark all `scala-syntax:quotedid-re' as symbol constituents (syntax 3)"
  (save-excursion
    (goto-char start)
    (while (re-search-forward scala-syntax:quotedid-re end t)
      (scala-syntax:put-syntax-table-property 0 '(3 . nil)))))

(defun scala-syntax:propertize-dollar (start end)
  "Mark all $ occurences as punctuation (syntax 1)"
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\$" end t)
      (scala-syntax:put-syntax-table-property 0 '(1 . nil)))))

(defun scala-syntax:propertize (start end)
  "See syntax-propertize-function"
  (scala-syntax:propertize-char-and-string-literals start end)
  (scala-syntax:propertize-shell-preamble start end)
  (scala-syntax:propertize-underscore-and-idrest start end)
  (scala-syntax:propertize-special-symbols start end)
  (scala-syntax:propertize-quotedid start end)
  (scala-syntax:propertize-dollar start end))

;;;;
;;;; Syntax navigation functions
;;;;

(defun scala-syntax:beginning-of-code-line ()
  (interactive)
  "Move to the beginning of code on the line, or to the end of
the line, if the line is empty. Return the new point.  Not to be
called on a line whose start is inside a comment, i.e. a comment
begins on the previous line and continues past the start of this
line."
  ;; TODO: make it work even if the start IS inside a comment
  (beginning-of-line)
  (let ((eol (line-end-position))
        (pos (point)))

    (while (and (forward-comment 1)
                (< (point) eol))
      (setq pos (point)))
    ;; Now we are either on a different line or at eol.
    ;; Pos is the last point one the starting line.
    (if (> (point) eol)
        (goto-char pos)
      (skip-syntax-forward " " eol)
      (point))))

(defun scala-syntax:looking-at-varid-p (&optional point)
  "Return true if looking-at varid, and it is not the start of a
stableId"
  (save-excursion
    (when point (goto-char point))
    (scala-syntax:skip-forward-ignorable)
    (let ((case-fold-search nil))
      (when (looking-at scala-syntax:varid-re)
        (save-match-data
          (if (or (= (char-after (match-end 0)) ?.)
                  (looking-at "\\<\\(this\\|super\\)\\>"))
              nil
            t))))))

(defun scala-syntax:looking-at-empty-line-p ()
  (save-excursion
    (or (bolp)
        (skip-syntax-forward " >" (1+ (line-end-position))))
    (looking-at scala-syntax:empty-line-re)))

(defun scala-syntax:looking-at-reserved-symbol (re &optional point)
  (interactive)
  (unless re (setq re scala-syntax:reserved-symbols-unsafe-re))
  (save-excursion
    (when point (goto-char point))
    (scala-syntax:skip-forward-ignorable)
    (and (looking-at re)
         (goto-char (match-end 0))
         (looking-at-p scala-syntax:after-reserved-symbol-re))))

(defun scala-syntax:looking-at-case-p (&optional point)
  (save-excursion
    (when point (goto-char point))
    (scala-syntax:skip-forward-ignorable)
    (and (looking-at scala-syntax:case-re)
         (goto-char (match-end 0))
         (scala-syntax:skip-forward-ignorable)
         (not (looking-at-p scala-syntax:class-or-object-re)))))

(defun scala-syntax:looking-back-empty-line-p ()
  "Return t if the previous line is empty"
  (save-excursion
    (skip-syntax-backward " " (line-beginning-position))
    (and (bolp)
         (forward-line -1)
         (looking-at-p scala-syntax:empty-line-re))))

(defun scala-syntax:skip-forward-ignorable ()
  "Moves forward over ignorable whitespace and comments. A
completely empty line is not ignorable and will not be mobed over."
  (interactive)
  (save-match-data
    (while (and (not (scala-syntax:looking-at-empty-line-p))
                (forward-comment 1)))
    (skip-syntax-forward " " (line-end-position))))

(defun scala-syntax:skip-backward-ignorable ()
  "Move backwards over ignorable whitespace and comments. A
completely empty line is not ignorable and will not be moved
over. Returns the number of points moved (will be negative)."
  (save-match-data
    (while (and (not (scala-syntax:looking-back-empty-line-p))
                (forward-comment -1)))
    (skip-syntax-backward " " (line-beginning-position))))

(defun scala-syntax:looking-at (re)
  "Return the end position of the matched re, if the current
position is followed by it, or nil if not. All ignorable comments
and whitespace are skipped before matching."
  (save-excursion
    (scala-syntax:skip-forward-ignorable)
    (looking-at re)))

(defun scala-syntax:looking-back-token (re &optional max-chars)
  "Return the start position of the token matched by re, if the
current position is preceeded by it, or nil if not. All ignorable
comments and whitespace are ignored, i.e. does not search past an
empty line. Expects to be outside of comment. A limit for the
search is calculated based on max-chars. The function won't look
further than max-chars starting after skipping any ignorable."
  (save-excursion
    ;; skip back all comments
    (scala-syntax:skip-backward-ignorable)
    (let ((end (point))
          (limit (when max-chars (- (point) max-chars))))
      ;; skip back punctuation or ids (words and related symbols and delimiters)
      (if (or (/= 0 (skip-chars-backward scala-syntax:delimiter-group limit))
              (/= 0 (skip-syntax-backward "." limit))
              (/= 0 (skip-syntax-backward "(" limit))
              (/= 0 (skip-syntax-backward ")" limit))
              (/= 0 (skip-syntax-backward "w_'$" limit)))
          (if (looking-at re) (point) nil)
        nil))))

(defun scala-syntax:backward-parameter-groups ()
  "Move back over all parameter groups to the start of the first
one."
  (save-match-data
    (while (scala-syntax:looking-back-token "[])]" 1)
      (backward-list))))

(defun scala-syntax:forward-parameter-groups ()
  "Move back over all parameter groups to the end of the last
one."
  (save-match-data
    (while (scala-syntax:looking-at "[[(]")
      (forward-list))))

(defun scala-syntax:forward-modifiers ()
  "Move forward over any modifiers."
  (save-match-data
    (while (scala-syntax:looking-at scala-syntax:modifiers-re)
      (scala-syntax:forward-sexp)
      (when (scala-syntax:looking-at "[[]")
        (forward-list)))))

(defun scala-syntax:looking-back-else-if-p ()
  ;; TODO: rewrite using (scala-syntax:if-skipped (scala:syntax:skip-backward-else-if))
  (save-excursion
    (if (and (scala-syntax:looking-back-token "\\s)" 1)
             (backward-list)
             (prog1 (scala-syntax:looking-back-token "if")
               (goto-char (match-beginning 0)))
             (prog1 (scala-syntax:looking-back-token "else")
               (goto-char (match-beginning 0))))
        (point) nil)))

(defun scala-syntax:newlines-disabled-p (&optional point)
  "Return true if newlines are disabled at the current point (or
point 'point') as specified by SLS chapter 1.2"
  ;; newlines are disabled if
  ;; - in '()' or '[]'
  ;; - between 'case' and '=>'
  ;; - XML mode (not implemented here)
  (unless point (setq point (point)))
  (save-excursion
    (let* ((state (syntax-ppss point))
           (parenthesisPos (nth 1 state)))
      (when parenthesisPos ;; if no parenthesis, then this cannot be a case block either
        (goto-char parenthesisPos)
        (or
         ;; the trivial cases of being inside ( or [
         (= (char-after) ?\()
         (= (char-after) ?\[)
         ;; else we have to see about case
         (progn
           (forward-char)
           (forward-comment (buffer-size))
           (skip-syntax-forward " >")
           (when (looking-at scala-syntax:case-re)
             (let ((limit (match-beginning 0)))
               (goto-char (or (nth 8 state) point))
               ;; go to the start of => or 'case'
               (while (> (point) limit)
                 (scala-syntax:backward-sexp)
                 (when (or (looking-at scala-syntax:case-re)
                           (scala-syntax:looking-at-reserved-symbol
                            scala-syntax:double-arrow-unsafe-re))
                   (setq limit (point))))
               ;; unless we found '=>', check if we found 'case' (but
               ;; 'case class' or 'case object')
               (unless (scala-syntax:looking-at-reserved-symbol
                        scala-syntax:double-arrow-unsafe-re)
                 (scala-syntax:forward-sexp)

                 (and (<= (point) point) ;; check that we were inside in the first place
                      (progn (scala-syntax:skip-forward-ignorable)
                             (not (looking-at scala-syntax:class-or-object-re)))))))))))))

(defun scala-syntax:forward-sexp ()
  "Move forward one scala expression. It can be: parameter list (value or type),
id, reserved symbol, keyword, block, or literal. Punctuation (.,;)
and comments are skipped silently. Position is placed at the
end of the skipped expression."
  (interactive)
  (syntax-propertize (point-max))
  ;; emacs knows how to properly skip: lists, varid, capitalid,
  ;; strings, symbols, chars, quotedid. What we have to handle here is
  ;; most of all ids made of op chars

  ;; skip comments, whitespace and scala delimiter chars .,; so we
  ;; will be at the start of something interesting
  (forward-comment (buffer-size))
  (while (< 0 (+ (skip-syntax-forward " ")
                 (skip-chars-forward scala-syntax:delimiter-group))))

  ;; emacs can handle everything but opchars
  (when (= (skip-syntax-forward ".") 0)
    (goto-char (or (scan-sexps (point) 1) (buffer-end 1)))))

(defun scala-syntax:forward-token ()
  "Move forward one scala token, comment word or string word. It
can be: start or end of list (value or type), id, reserved
symbol, keyword, block, or literal. Punctuation (.,;), comment
delimiters and string delimiters are skipped silently. Position
is placed at the end of the skipped token."
  (interactive)
  (syntax-propertize (point-max))
  (skip-syntax-forward " >" (point-max))
  (when (looking-at
         (concat "\\([#@:]\\|" scala-syntax:double-arrow-unsafe-re
                 "\\|:>\\|<:\\)" scala-syntax:after-reserved-symbol-re))
    (goto-char (match-end 1)))
  (let ((syntax (char-syntax (char-after)))
        (state (syntax-ppss)))
    (cond
     ((or (nth 4 state) (nth 3 state))
      ;; inside a string or comment, skip words as normal unless that
      ;; would end up outside the string. Then leave point at end of
      ;; string delimiter.
      (let ((start (nth 8 state))
            (end (save-excursion (forward-word) (point))))
        (if (eq (nth 8 (save-excursion (syntax-ppss end))) start)
            (goto-char end)
          (while (eq (nth 8 (syntax-ppss)) start)
            (forward-char)))))
     ;; list start or end
     ((or (= syntax ?\)) (= syntax ?\()) (forward-char))
     ;; comment or string start is skipped
     ((looking-at "\\(//\\|/\\*+\\|\"\\(\"\"\\)?\\)")
      (goto-char (match-end 1)))
     ;; otherwise forward-sexp
     (t (forward-sexp)))))

(defun scala-syntax:backward-sexp ()
  "Move backward one scala expression. It can be: parameter
  list (value or type), id, reserved symbol, keyword, block, or
  literal. Delimiters (.,;) and comments are skipped
  silently. Position is placed at the beginning of the skipped
  expression."
  (interactive)
  (syntax-propertize (point))
  ;; for implementation comments, see scala-syntax:forward-sexp
  (forward-comment (- (buffer-size)))
  (while (> 0 (+ (skip-syntax-backward " ")
                 (skip-chars-backward scala-syntax:delimiter-group))))

  (when (= (skip-syntax-backward ".") 0)
    (goto-char (or (scan-sexps (point) -1) (buffer-end -1)))
    (backward-prefix-chars)))

(defun scala-syntax:has-char-before (char end)
  (save-excursion
    (while (and (< (point) end)
                (or (bobp)
                    (/= (char-before) char)))
      (scala-syntax:forward-sexp))
    (when (= (char-before) char)
      (scala-syntax:skip-forward-ignorable)
      (> end (point)))))

(defun scala-syntax:search-backward-sexp (re)
  "Searches backward sexps until it reaches re, empty line or ;.
If re is found, point is set to beginning of re and the position
is returned, otherwise nil is returned"
  (let ((found (save-excursion
                 (while (not (or (bobp)
                                 (scala-syntax:looking-back-empty-line-p)
                                 (scala-syntax:looking-back-token "[;,]")
                                 (looking-at re)))
                   (scala-syntax:backward-sexp))
                 (if (looking-at re)
                     (point)
                   nil))))
    (when found (goto-char found))))

(defun scala-syntax:list-p (&optional point)
  "Returns the start of the list, if the current point (or point
'point') is on the first line of a list element > 1, or nil if
not. A list must be either enclosed in parentheses or start with
'val', 'var' or 'import'."
  (save-excursion
    ;; first check that the previous line ended with ','
    (when point (goto-char point))
    (scala-syntax:beginning-of-code-line)
    (when (scala-syntax:looking-back-token "," 1)
      (goto-char (match-beginning 0))
      (let ((parenpoint (nth 1 (syntax-ppss))))
        (if (and parenpoint (or (= (char-after parenpoint) ?\()
                                (= (char-after parenpoint) ?\[)))
            (1+ parenpoint)
          (ignore-errors ; catches when we get at parentheses
            (while (not (or (bobp)
                            (looking-at scala-syntax:list-keywords-re)
                            (scala-syntax:looking-back-empty-line-p)
                            (scala-syntax:looking-back-token ";")))
              (scala-syntax:backward-sexp)))
          (when (looking-at scala-syntax:list-keywords-re)
            (goto-char (match-end 0))))))))

;; Functions to help with finding the beginning and end of scala definitions.

(defconst scala-syntax:modifiers-re
  (regexp-opt '("override" "abstract" "final" "sealed" "implicit" "lazy"
                "private" "protected" "case") 'words))

(defconst scala-syntax:whitespace-delimeted-modifiers-re
  (concat "\\(?:" scala-syntax:modifiers-re "\\(?: *\\)" "\\)*"))

(defconst scala-syntax:definition-words-re
  (mapconcat 'regexp-quote '("class" "object" "trait" "val" "var" "def" "type") "\\|"))

(defun scala-syntax:build-definition-re (words-re)
  (concat " *"
	  scala-syntax:whitespace-delimeted-modifiers-re
	  words-re
	  "\\(?: *\\)"
	  "\\(?2:"
	  scala-syntax:id-re
	  "\\)"))

(defconst scala-syntax:all-definition-re
  (scala-syntax:build-definition-re
   (concat "\\(?1:" scala-syntax:definition-words-re "\\)\\b")))

;; Functions to help with beginning and end of definitions.

(defun scala-syntax:backward-sexp-forcing ()
  (condition-case ex (backward-sexp) ('error (backward-char))))

(defun scala-syntax:forward-sexp-or-next-line ()
  (interactive)
  (cond ((looking-at "\n") (next-line) (beginning-of-line))
	(t (forward-sexp))))

(defun scala-syntax:beginning-of-definition ()
  "This function may not work properly with certain types of scala definitions.
For example, no care has been taken to support multiple assignments to vals such as

val a, b = (1, 2)
"
  (interactive)
  (let ((found-position
	 (save-excursion
	   (scala-syntax:backward-sexp-forcing)
	   (scala-syntax:movement-function-until-re scala-syntax:all-definition-re
						    'scala-syntax:backward-sexp-forcing))))
    (when found-position (progn (goto-char found-position) (back-to-indentation)))))

(defun scala-syntax:end-of-definition ()
  "This function may not work properly with certain types of scala definitions.
For example, no care has been taken to support multiple assignments to vals such as

val a, b = (1, 2)
"
  (interactive)
  (re-search-forward scala-syntax:all-definition-re)
  (scala-syntax:find-brace-equals-or-next)
  (scala-syntax:handle-brace-equals-or-next))

(defun scala-syntax:find-brace-equals-or-next ()
  (scala-syntax:go-to-pos
   (save-excursion
     (scala-syntax:movement-function-until-cond-function
      (lambda () (or (looking-at "[[:space:]]*[{=]")
		     (looking-at scala-syntax:all-definition-re)))
      (lambda () (condition-case ex (scala-syntax:forward-sexp-or-next-line) ('error nil)))))))

(defun scala-syntax:handle-brace-equals-or-next ()
  (cond ((eobp) nil)
        ((looking-at "[[:space:]]*{") (forward-sexp))
        ((looking-at "[[:space:]]*=") (scala-syntax:forward-sexp-or-next-line)
         (scala-syntax:handle-brace-equals-or-next))
        ((looking-at scala-syntax:all-definition-re) nil)
        ((looking-at "[[:space:]]*\n[[:space:]]*}") (skip-syntax-forward "[[:space:]]*\n[[:space:]]*}"))
        (t (scala-syntax:forward-sexp-or-next-line)
           (scala-syntax:handle-brace-equals-or-next))))

(defun scala-syntax:movement-function-until-re (re movement-function)
  (save-excursion
    (scala-syntax:movement-function-until-cond-function
     (lambda () (looking-at re)) movement-function)))

(defun scala-syntax:movement-function-until-cond-function (cond-function movement-function)
  (let ((last-point (point)))
    (if (not (funcall cond-function))
	(progn (funcall movement-function)
	       (if (equal last-point (point)) nil
		 (scala-syntax:movement-function-until-cond-function
		  cond-function movement-function))) last-point)))

(defun scala-syntax:go-to-pos (pos) (when pos (goto-char pos)))

(provide 'scala-mode-syntax)
