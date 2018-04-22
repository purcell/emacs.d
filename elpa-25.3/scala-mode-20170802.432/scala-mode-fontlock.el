;;; scala-mode-fontlock.el - Major mode for editing scala, font-lock
;;; Copyright (c) 2012 Heikki Vesalainen
;;; For information on the License, see the LICENSE file

(require 'scala-mode-syntax)

(defcustom scala-font-lock:constant-list '()
  "A list of strigs that should be fontified in constant
face. This customization property takes effect only after the
scala-mode has been reloaded."
  :type '(repeat string)
  :group 'scala)

(defun scala-font-lock:create-user-constant-re ()
  (regexp-opt scala-font-lock:constant-list 'words))

(defun scala-font-lock:mark-reserved-symbols (limit)
  (when (re-search-forward scala-syntax:reserved-symbols-re limit t)
      (goto-char (match-end 2)))) ;; step back to the match (re matches futher)

(defun scala-font-lock:mark-underscore (limit)
  (when (re-search-forward scala-syntax:reserved-symbol-underscore-re limit t)
      (goto-char (match-end 2)))) ;; step back to the match (re matches futher)

;(defun scala-font-lock:extend-region-function ()

(defun scala-font-lock:limit-pattern2 (&optional start)
  (save-excursion
    (when start (goto-char start))
    (scala-syntax:skip-forward-ignorable)
    (ignore-errors
      (while (and (not (or (eobp)
                           (looking-at scala-syntax:other-keywords-unsafe-re)
                           (scala-syntax:looking-at-reserved-symbol nil)))
                  (scala-syntax:looking-at-simplePattern-beginning))
;        (message "- now at %d" (point))
        (if (= (char-after) ?\()
            (forward-list)
          ;; else
          (goto-char (match-end 0))
          (scala-syntax:skip-forward-ignorable)
;          (message "+ now at %d" (point))
          (cond ((looking-at "(")
                 (forward-list))
                ((looking-at "@")
                 (goto-char (match-end 0)))
                ((or (scala-syntax:looking-at-reserved-symbol nil)
                     (looking-at scala-syntax:other-keywords-unsafe-re))
;                 (messssage "saw reserved symbol or keyword")
                 nil)
                ((looking-at scala-syntax:id-re)
;                 (message "saw id-re %d" (match-beginning 0))
                 (goto-char (match-end 0)))
;                (t
;                 (message "nothing special here %s" (point)))
                ))
        (scala-syntax:skip-forward-ignorable)))
;    (message "limit at %s" (point))
    (point)))

(defun scala-font-lock:limit-pattern2-list (&optional start)
  (let ((limit (scala-font-lock:limit-pattern2 start)))
    (while (= (char-after limit) ?,)
      (setq limit (scala-font-lock:limit-pattern2 (1+ limit))))
;    (message "list limit at %s" limit)
    limit))

(defun scala-font-lock:mark-pattern1-part (&optional limit pattern-p)
  "Parses a part of val, var and case pattern (or id). Always
parses a variable or constant name first and then type, leaving
the pointer at the next variablename, constant name, list or
Pattern3, if any, and setting up match data 1 (variable),
2 (constant) and 3 (type) accordingly. If there is no variable
name before the first type, then the match data for the variable
name is nil. Returns t if something was matched or nil if nothing
was found.

If pattern-p is defined, then only varid is matched as variable
and everything else is constant.

Does not continue past limit.
"
;  (message "will stop at %d" limit)
  (cond
   ;; quit if we are past limit
   ((or (and limit (>= (point) limit))
        (eobp))
;    (message "at limit %s" (point))
    nil)
   ;; Type pattern, just skip the whole thing. It will end at ',' or ')'.
   ;; Note: forms starting with ':' are handled by a completely separete
   ;; font-lock matcher.
   ((scala-syntax:looking-at-reserved-symbol ":")
;    (message ":")
    (while (not (or (eobp)
                    (scala-syntax:looking-at "[,);]")
                    (scala-syntax:looking-at-reserved-symbol "|")
                    (scala-syntax:looking-at-reserved-symbol
                     scala-syntax:double-arrow-unsafe-re)
                    (scala-syntax:looking-at-empty-line-p)))
      (scala-syntax:forward-sexp)
      (scala-syntax:skip-forward-ignorable))
    (set-match-data nil)
    t)
   ;; Binding part cannot start with reserved symbols. If they
   ;; are seen, we must quit.
   ((scala-syntax:looking-at-reserved-symbol nil)
;    (message "symbol")
    nil)
   ((scala-syntax:looking-at-stableIdOrPath)
;    (message "stableId")
    (let ((beg (match-beginning 0))
          (end (match-end 0))
          (varid (scala-syntax:looking-at-varid-p)))
      (goto-char end)
      (let ((new-match-data
             (cond
              ((= (char-after end) ?\()
               ;; matched type
;               (message "it's a type")
               `(,beg ,end nil nil nil nil ,beg ,end))
              ((progn (scala-syntax:backward-sexp)
                      (= (char-before) ?.))
               ;; matched constant
               `(,beg ,end nil nil ,(point) ,end nil nil))
              ((or varid (not pattern-p))
               ;; matched variable name or we can't be sure
               `(,beg ,end ,beg ,end nil nil nil nil))
              (t
               ;; matched constant
               `(,beg ,end nil nil ,beg ,end nil nil)))))
        (goto-char end)
        (scala-syntax:skip-forward-ignorable)
        (cond
         ((and (not (or (scala-syntax:looking-at-reserved-symbol nil)
                        (scala-syntax:looking-at-reserved-symbol "|")))
               (scala-syntax:looking-at-stableIdOrPath))
          (setq new-match-data
                (append (butlast new-match-data 2)
                        `(,(match-beginning 0)
                          ,(match-end 0))))
          (goto-char (match-end 0))
          (scala-syntax:skip-forward-ignorable))
         ((= (char-after) ?@)
          (forward-char)
          (scala-syntax:skip-forward-ignorable)))
        (set-match-data new-match-data)))
    t)
   ;; Pattern3 can be a literal. Just skip them.
   ((looking-at scala-syntax:literal-re)
;    (message "literal")
    (goto-char (match-end 0))
    (scala-syntax:skip-forward-ignorable)
    (set-match-data nil)
    t)
   ;; Start of a patterns list or alternatives. Skip if alternatives or
   ;; else leave point at start of first element.
   ((= (char-after) ?\()
;    (message "(")
    (let ((alternatives-p
           (save-excursion
             (forward-char)
             (ignore-errors
               ;; forward-sexp will terminate the loop with error
               ;; if '|' is not found before end of list ')'
               (while (not (or (eobp)
                               (= (char-before) ?|)
                               (scala-syntax:looking-at-empty-line-p)))
                 (scala-syntax:forward-sexp))
               t))))
      (if alternatives-p
          (forward-list)
        (forward-char)))
    (scala-syntax:skip-forward-ignorable)
    (set-match-data nil)
    t)
   ;; continuation or end of list, just skip and position at the
   ;; next element
   ((or (= (char-after) ?,)
        (= (char-after) ?\)))
;    (message ", or )")
    (forward-char)
    (scala-syntax:skip-forward-ignorable)
    (set-match-data nil)
    t)
   ;; none of the above, just stop
   (t
;    (message "Cannot continue Pattern1 at %d" (point))
    nil)
))

(defun scala-font-lock:limit-pattern (&optional start)
  (save-excursion
    (goto-char (scala-font-lock:limit-pattern2 start))
;    (message "now at %d" (point))
    (when (scala-syntax:looking-at-reserved-symbol ":")
      (while (not (or (eobp)
                      (scala-syntax:looking-at-reserved-symbol "|")
                      (scala-syntax:looking-at-reserved-symbol
                       scala-syntax:double-arrow-unsafe-re)
                      (scala-syntax:looking-at-empty-line-p)))
        (scala-syntax:forward-sexp)
        (scala-syntax:skip-forward-ignorable)))
    (if (or (/= (char-after) ?|)
            (scala-syntax:looking-at-reserved-symbol
             scala-syntax:double-arrow-unsafe-re))
        (point)
      (forward-char)
      (scala-font-lock:limit-pattern))))

(defun scala-font-lock:mark-pattern-part (&optional limit)
  (when (scala-syntax:looking-at-reserved-symbol "|")
;    (message "skipping |")
    (forward-char)
    (scala-syntax:skip-forward-ignorable))
  (scala-font-lock:mark-pattern1-part limit t))

(defun scala-font-lock:limit-type (&optional start)
  start)


(defun scala-font-lock:limit-simpleType (&optional start)
  (when start (goto-char start))
  (scala-syntax:skip-forward-ignorable)
  (setq start (point))

  (if (= (char-after) ?\()
      (ignore-errors (forward-list))
    (scala-font-lock:mark-simpleType))
  (when (and (not (eobp)) (= (char-after) ?#))
    (scala-font-lock:mark-simpleType))
  (when (and (not (eobp)) (= (char-after) ?\[))
    (ignore-errors (forward-list))
    (scala-syntax:skip-forward-ignorable))
  (let ((limit (point)))
    (goto-char start)
;    (message "simpeType limit at %d" limit)
    limit))

(defun scala-font-lock:mark-simpleType (&optional limit)
;  (message "looking for simpleType at %d" (point))
  (cond
   ;; stop at limit
   ((and limit (>= (point) limit))
    nil)
   ;; just dive into lists
   ((> (skip-chars-forward "[(,)]") 0)
;    (message "skipping list-marks")
    (scala-syntax:skip-forward-ignorable)
    (set-match-data nil)
    t)
   ;; jump over blocks
   ((= (char-after) ?\{)
    (ignore-errors
      (forward-list)
      (set-match-data nil)
      t))
   ;; ignore arrows and reserved words and symbols
   ((or (scala-syntax:looking-at-reserved-symbol
         scala-syntax:double-arrow-unsafe-re)
        (scala-syntax:looking-at-reserved-symbol
         "<[:%]\\|>?:")
        (looking-at "\\<forSome\\>"))
;    (message "skipping reserved")
    (goto-char (match-end 0))
    (scala-syntax:skip-forward-ignorable)
    (set-match-data nil)
    t)
   ;; color id after '#'
   ((= (char-after) ?#)
;    (message "at #")
    (forward-char)
    (if (and (not (or (looking-at scala-syntax:keywords-unsafe-re)
                      (scala-syntax:looking-at-reserved-symbol nil)))
             (looking-at scala-syntax:id-re))
        (goto-char (match-end 0)) nil))
   ;; color paths (including stableid)
   ((scala-syntax:looking-at-stableIdOrPath t)
;    (message "at path")
    (let ((end (match-end 0)))
      (goto-char end)
      (while (scala-syntax:looking-back-token "this\\|type")
        (goto-char (match-beginning 0))
        (skip-chars-backward "."))
      (unless (scala-syntax:looking-back-token scala-syntax:id-re)
        (set-match-data nil))
      (goto-char end))
    (scala-syntax:skip-forward-ignorable)
    t)
   (t
;    (message "Cannot continue simpleType at %d" (point))
    nil)))

(defun scala-font-lock:mark-string-escapes (limit)
  (when (re-search-forward scala-syntax:string-escape-re limit t)
    (goto-char (match-end 0))
    (or (eq (nth 3 (save-excursion (syntax-ppss (match-beginning 0)))) ?\")
        (scala-font-lock:mark-string-escapes limit))))

(defun scala-font-lock:mark-numberLiteral (re limit)
  (when (re-search-forward re limit t)
    (if (string-match-p scala-syntax:number-safe-start-re
                        ;; get char-before match or a magic ',', which is safe
                        (string (or (char-before (match-beginning 0)) ?,)))
        t
      (scala-font-lock:mark-numberLiteral re limit))))

(defun scala-font-lock:mark-floatingPointLiteral (limit)
  (scala-font-lock:mark-numberLiteral
   scala-syntax:floatingPointLiteral-re
   limit))

(defun scala-font-lock:mark-integerLiteral (limit)
  (scala-font-lock:mark-numberLiteral
   scala-syntax:integerLiteral-re
   limit))

(defun scala-font-lock:keywords ()
  ;; chars, string, comments are handled acording to syntax and
  ;; syntax propertize

  `(;; keywords
    (,scala-syntax:override-re 2 scala-font-lock:override-face)
    (,scala-syntax:abstract-re 2 scala-font-lock:abstract-face)
    (,scala-syntax:final-re 2 scala-font-lock:final-face)
    (,scala-syntax:sealed-re 2 scala-font-lock:sealed-face)
    (,scala-syntax:implicit-re 2 scala-font-lock:implicit-face)
    (,scala-syntax:lazy-re 2 scala-font-lock:lazy-face)
    (,scala-syntax:private-re 2 scala-font-lock:private-face)
    (,scala-syntax:protected-re 2 scala-font-lock:protected-face)
    (,scala-syntax:other-keywords-re 2 font-lock-keyword-face)
    (,scala-syntax:value-keywords-re 2 font-lock-constant-face)
    (,scala-syntax:path-keywords-re 2 font-lock-keyword-face)

    ;; User defined constants
    (,(scala-font-lock:create-user-constant-re) 0 font-lock-constant-face)

    ;; Annotations
    (, (rx (and "@" (in "a-zA-Z_.") (0+ (in "a-zA-Z0-9_."))))
       . font-lock-preprocessor-face)

    ;; reserved symbols
    (scala-font-lock:mark-reserved-symbols 2 font-lock-keyword-face)

    ;; 'Symbols
    (,scala-syntax:symbolLiteral-re 1 font-lock-string-face)

    ;; underscore
    (scala-font-lock:mark-underscore 2 font-lock-keyword-face)

    ;; escapes inside strings
    (scala-font-lock:mark-string-escapes (0 font-lock-constant-face prepend nil))

    ;; object
    (,(concat "\\<object[ \t]+\\("
              scala-syntax:id-re
              "\\)")
     1 font-lock-constant-face)

    ;; class, trait, object
    (,(concat "\\<\\(class\\|trait\\)[ \t]+\\("
              scala-syntax:id-re
              "\\)")
     2 font-lock-type-face)

    ;; ;; extends, with, new
    ;; (,(concat "\\<\\(extends\\|with\\|new\\)[ \t]+\\([("
    ;;           scala-syntax:id-first-char-group "]\\)")
    ;;  (scala-font-lock:mark-simpleType (scala-font-lock:limit-simpleType
    ;;                                    (goto-char (match-beginning 2)))
    ;;                                   nil
    ;;                                   (0 font-lock-type-face nil t)))

    ;; ;; ':'
    ;; (,scala-syntax:colon-re
    ;;  (scala-font-lock:mark-simpleType (scala-font-lock:limit-simpleType
    ;;                                    (goto-char (match-end 2)))
    ;;                                   nil
    ;;                                   (0 font-lock-type-face nil t)))

    ;; def
    (,(concat "\\<def[ \t]+\\(" scala-syntax:id-re "\\)") 1 font-lock-function-name-face)

    ;; VarDcl
    ("\\<val[ \t]+\\([^:]\\)"
     (scala-font-lock:mark-pattern1-part (scala-font-lock:limit-pattern2-list
                                          (goto-char (match-beginning 1)))
                                         nil
                                         (1 font-lock-variable-name-face nil t)
                                         (2 font-lock-constant-face nil t)
                                         (3 font-lock-type-face nil t)))

    ("\\<var[ \t]+\\([^:]\\)"
     (scala-font-lock:mark-pattern1-part (scala-font-lock:limit-pattern2-list
                                          (goto-char (match-beginning 1)))
                                         nil
                                         (1 scala-font-lock:var-face nil t)
                                         (2 font-lock-constant-face nil t)
                                         (3 font-lock-type-face nil t)
                                         ))

    ;; case (but not case class|object)
    ("\\<case[ \t]+\\([^:]\\)"
     (scala-font-lock:mark-pattern-part (scala-font-lock:limit-pattern
                                         (goto-char (match-beginning 1)))
                                        nil
                                        (1 font-lock-variable-name-face nil t)
                                        (2 font-lock-constant-face nil t)
                                        (3 font-lock-type-face nil t)))

    ;; type ascription (: followed by alpha type name)
    (,(rx
       (or (not (in "!#%&*+-/:<=>?@\\^|~")) line-start)
       (group ":")
       (0+ space)
       (group (in "a-zA-Z_")
              (0+ (in "a-zA-Z0-9_"))
              (\? (and "_" (1+ (in "!#%&*+-/:<=>?@\\^|~"))))))
     (1 font-lock-keyword-face) (2 font-lock-type-face))

    ;; type ascription (: followed by punctuation type name)
    (,(rx
       (or (not (in "!#%&*+-/:<=>?@\\^|~")) line-start)
       (group ":")
       (1+ space)
       (group (1+ (in "-!#%&*+/:<=>?@\\^|~"))))
     (1 font-lock-keyword-face) (2 font-lock-type-face))

    ;; extends followed by type
    (,(rx symbol-start
          (group "extends")
          (1+ space)
          (group (or
                  (and (in "a-zA-Z_")
                     (0+ (in "a-zA-Z0-9_"))
                     (\? (and "_" (1+ (in "!#%&*+-/:<=>?@\\^|~")))))
                  (1+ (in "!#%&*+-/:<=>?@\\^|~")))))
     (1 font-lock-keyword-face) (2 font-lock-type-face))

    ;; with followed by type
    (,(rx symbol-start
          (group "with")
          (1+ space)
          (group (or
                  (and (in "a-zA-Z_")
                     (0+ (in "a-zA-Z0-9_"))
                     (\? (and "_" (1+ (in "!#%&*+-/:<=>?@\\^|~")))))
                  (1+ (in "!#%&*+-/:<=>?@\\^|~")))))
     (1 font-lock-keyword-face) (2 font-lock-type-face))

    ;; new followed by type
    (,(rx symbol-start
          (group "new")
          (1+ space)
          (group (or
                  (and (in "a-zA-Z_")
                     (0+ (in "a-zA-Z0-9_"))
                     (\? (and "_" (1+ (in "!#%&*+-/:<=>?@\\^|~")))))
                  (1+ (in "!#%&*+-/:<=>?@\\^|~")))))
     (1 font-lock-keyword-face) (2 font-lock-type-face))

    ;; uppercase means a type or object
    (,(rx symbol-start
          (and (in "A-Z")
             (0+ (in "a-zA-Z0-9_"))
             (\? (and "_" (1+ (in "!#%&*+-/:<=>?@\\^|~"))))))
     . font-lock-constant-face)
    ;; . font-lock-type-face)
                                        ; uncomment this to go back to highlighting objects as types

    ;; uppercase
    (,(rx symbol-start
          (group
           (and (in "A-Z")
              (0+ (in "a-zA-Z0-9_"))
              (\? (and "_" (1+ (in "!#%&*+-/:<=>?@\\^|~")))))))
     . font-lock-constant-face)

    ;; package name
    (,(rx symbol-start
          (group "package")
          (1+ space)
          (group (and (in "a-zA-Z_.") (0+ (in "a-zA-Z0-9_.")))))
     (1 font-lock-keyword-face) (2 font-lock-string-face))

    ;; number literals (have to be here so that other rules take precedence)
    (scala-font-lock:mark-floatingPointLiteral . font-lock-constant-face)
    (scala-font-lock:mark-integerLiteral . font-lock-constant-face)

    (scala-syntax:interpolation-matcher 0 font-lock-variable-name-face t)

    ))

(defun scala-font-lock:syntactic-face-function (state)
  "Return correct face for string or comment"
  (if (and (integerp (nth 4 state))
         (save-excursion
           (goto-char (nth 8 state))
           (looking-at "/\\*\\*\\($\\|[^*]\\)")))
      ;; scaladoc (starts with /** only)
      font-lock-doc-face
    (if (nth 3 state) font-lock-string-face font-lock-comment-face)))

(defface scala-font-lock:var-face
  '((t (:inherit font-lock-warning-face)))
  "Font Lock mode face used to highlight scala variable names."
  :group 'scala)

(defvar scala-font-lock:var-face 'scala-font-lock:var-face
  "Face for scala variable names.")

(defface scala-font-lock:private-face
  '((t (:inherit font-lock-builtin-face)))
  "Font Lock mode face used for the private keyword."
  :group 'scala)

(defvar scala-font-lock:private-face 'scala-font-lock:private-face
  "Face for the scala private keyword.")

(defface scala-font-lock:protected-face
  '((t (:inherit font-lock-builtin-face)))
  "Font Lock mode face used for the protected keyword."
  :group 'scala)

(defvar scala-font-lock:protected-face 'scala-font-lock:protected-face
  "Face for the scala protected keyword.")

(defface scala-font-lock:override-face
  '((t (:inherit font-lock-builtin-face)))
  "Font Lock mode face used for the override keyword."
  :group 'scala)

(defvar scala-font-lock:override-face 'scala-font-lock:override-face
  "Face for the scala override keyword.")

(defface scala-font-lock:sealed-face
  '((t (:inherit font-lock-builtin-face)))
  "Font Lock mode face used for the sealed keyword."
  :group 'scala)

(defvar scala-font-lock:sealed-face 'scala-font-lock:sealed-face
  "Face for the scala sealed keyword.")

(defface scala-font-lock:abstract-face
  '((t (:inherit font-lock-builtin-face)))
  "Font Lock mode face used for the abstract keyword."
  :group 'scala)

(defvar scala-font-lock:abstract-face 'scala-font-lock:abstract-face
  "Face for the scala abstract keyword.")

(defface scala-font-lock:final-face
  '((t (:inherit font-lock-builtin-face)))
  "Font Lock mode face used for the final keyword."
  :group 'scala)

(defvar scala-font-lock:final-face 'scala-font-lock:final-face
  "Face for the scala final keyword.")

(defface scala-font-lock:implicit-face
  '((t (:inherit font-lock-builtin-face)))
  "Font Lock mode face used for the implicit keyword."
  :group 'scala)

(defvar scala-font-lock:implicit-face 'scala-font-lock:implicit-face
  "Face for the scala implicit keyword.")

(defface scala-font-lock:lazy-face
  '((t (:inherit font-lock-builtin-face)))
  "Font Lock mode face used for the lazy keyword."
  :group 'scala)

(defvar scala-font-lock:lazy-face 'scala-font-lock:lazy-face
  "Face for the scala lazy keyword.")

(defface scala-font-lock:var-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Font Lock mode face used for the var keyword."
  :group 'scala)

(defvar scala-font-lock:var-keyword-face 'scala-font-lock:var-keyword-face
  "Face for the scala var keyword.")

(provide 'scala-mode-fontlock)
