;;; elm-font-lock.el --- Font locking module for Elm mode.

;; Copyright (C) 2013, 2014 Joseph Collard
;; Copyright (C) 2015 Bogdan Popa

;; Authors: Joseph Collard
;; URL: https://github.com/jcollard/elm-mode

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
;;; Code:
(require 'font-lock)
(require 'rx)

(defgroup elm-font-lock nil
  "Font locking for Elm code."
  :group 'faces)

(defface elm-font-lock-operators
  '((t :inherit font-lock-builtin-face))
  "The default face used to highlight operators inside expressions."
  :group 'elm-font-lock)

(defcustom elm-font-lock-operators-face 'elm-font-lock-operators
  "The face used to highlight operators inside expressions.
To disable this highlighting, set this to nil."
  :type '(choice (const nil)
                 face)
  :group 'elm-font-lock)

(defface elm-font-lock-multiline-list-delimiters
  '((t :inherit font-lock-keyword-face))
  "The default face used to highlight brackets and commas in multiline lists."
  :group 'elm-font-lock)

(defcustom elm-font-lock-multiline-list-delimiters-face 'elm-font-lock-multiline-list-delimiters
  "The face used to highlight brackets and commas in multilist lists.
To disable this highlighting, set this to nil."
  :type '(choice (const nil)
                 face)
  :group 'elm-font-lock)

(defconst elm--keywords
  '("let" "case" "in" "if" "of" "then" "else" "effect"
    "module" "import" "as" "exposing" "type" "where"
    "alias" "port" "infix" "infixr" "infixl")
  "Reserved keywords.")

(defconst elm--regexp-keywords
  (regexp-opt elm--keywords 'symbols)
  "A regular expression representing the reserved keywords.")

(defconst elm--font-lock-keywords
  (cons elm--regexp-keywords font-lock-keyword-face)
  "Highlighting for keywords.")

(defun elm--syntax-stringify ()
  "Syntax propertize triple quoted strings."
  (let* ((ppss (save-excursion
                 (backward-char 3)
                 (syntax-ppss)))
         (string-started (and (not (nth 4 ppss)) (nth 8 ppss)))
         (quote-starting-pos (- (point) 3))
         (quote-ending-pos (point)))
    (if (not string-started)
        (put-text-property quote-starting-pos (1+ quote-starting-pos)
                           'syntax-table (string-to-syntax "|"))
      (put-text-property (1- quote-ending-pos) quote-ending-pos
                           'syntax-table (string-to-syntax "|")))))

(defconst elm--syntax-propertize
  (syntax-propertize-rules
   ;;; Syntax rule for char literals
   ((rx (and (1+ " ")
             (group "'")
             (optional "\\") any
             (group "'")))
    (1 "\"")
    (2 "\""))

   ((rx (and (or point
                 (not (any ?\\ ?\"))
                 (and (or (not (any ?\\)) point) ?\\ (* ?\\ ?\\) (any ?\")))
             (* ?\\ ?\\)
             "\"\"\""))
    (0 (ignore (elm--syntax-stringify))))))

(defun elm--syntax-propertize-function (begin end)
  "Mark special lexemes between BEGIN and END."
  (funcall elm--syntax-propertize begin end)
  (save-excursion
    (goto-char begin)
    (while (re-search-forward "\\\\[({]" end t)
      (let ((open (match-beginning 0)))
        (add-text-properties open (1+ open) '(syntax-table (1 . nil)))))))

(defvar elm--syntax-table
  (let ((st (make-syntax-table)))
    ;;; Syntax entry for {- -} type comments.
    (modify-syntax-entry ?\{  "(}1nb" st)
    (modify-syntax-entry ?\}  "){4nb" st)
    (modify-syntax-entry ?-  ". 123" st)
    (modify-syntax-entry ?\n ">" st)

    (modify-syntax-entry ?\" "\"\"" st)
    (modify-syntax-entry ?\\ "\\" st)
    st))

;;; Name regexp is according to https://github.com/elm-lang/elm-compiler/blob/353930a474fee4d833f962100edde70417691bca/src/Parse/Helpers.hs#L65
(defconst elm--regexp-function
  "^\\([a-z_][0-9A-Za-z_']*\\|([^)]+)\\)"
  "A regular expression representing function names.")

(defconst elm--font-lock-functions
  (cons elm--regexp-function font-lock-function-name-face)
  "Highlighting for function names.")

(defconst elm--regexp-type
  "\\<[A-Z][0-9A-Za-z_']*"
  "A regular expression representing modules and types.")

(defconst elm--font-lock-types
  (cons elm--regexp-type font-lock-type-face)
  "Highlighting for module names and types.")

(defconst elm--regexp-operators
  (concat "\\(" "`[^`]+`"
          "\\|" "\\B\\\\"
          "\\|" "[-+*/\\\\|<>=:!@#$%^&,.]+"
          "\\)")
  "A regular expression representing operators inside expressions.")

(defconst elm--font-lock-operators
  (cons elm--regexp-operators '(1 elm-font-lock-operators-face))
  "Highlighting for operators inside expressions.")

(defconst elm--regexp-multiline-list-comma-closing-brackets
  (concat "^[[:space:]]*" (regexp-opt '("," "]" "}") t))
  "A regular expression representing commas and closing brackets in multiline lists and records.")

(defconst elm--font-lock-multiline-list-comma-closing-brackets
  (cons elm--regexp-multiline-list-comma-closing-brackets
        '(1 elm-font-lock-multiline-list-delimiters-face))
  "Highlighting for commas and closing brackets in multiline lists and records.")

(defun elm--match-multiline-list-opening-bracket (limit)
  "Highlighting search function for opening brackets in multiline lists and records.
Also highlights opening brackets without a matching bracket."
  (when (elm--search-forward-opening-bracket limit)
    (let ((opening (point))
          (eol (line-end-position))
          (closing (elm--search-forward-closing-bracket)))
      (if (or (= closing opening) (> closing eol))
          (progn
            (set-match-data (match-data))
            (goto-char (+ 1 opening))
            t)
        (elm--match-multiline-list-opening-bracket limit)))))

(defun elm--search-forward-opening-bracket (limit)
  "Go to the next opening bracket up to LIMIT."
  (if (search-forward-regexp (regexp-opt '("[" "{")) limit t)
      (progn
        (backward-char)
        t)))

(defun elm--search-forward-closing-bracket ()
  "Go to the next matching bracket, assuming that the cursor is on an opening bracket."
  (ignore-errors
    (save-match-data
      (forward-sexp)))
  (point))

(defconst elm--font-lock-multiline-list-opening-brackets
  '(elm--match-multiline-list-opening-bracket (0 elm-font-lock-multiline-list-delimiters-face))
  "Highlighting for opening brackets in multiline lists and records.")

(defconst elm--font-lock-highlighting
  (list (list elm--font-lock-keywords
              elm--font-lock-functions
              elm--font-lock-types
              elm--font-lock-multiline-list-comma-closing-brackets
              elm--font-lock-multiline-list-opening-brackets
              elm--font-lock-operators)
        nil nil))

(defun turn-on-elm-font-lock ()
  "Turn on Elm font lock."
  (setq font-lock-multiline t)
  (set-syntax-table elm--syntax-table)
  (set (make-local-variable 'syntax-propertize-function) #'elm--syntax-propertize-function)
  (set (make-local-variable 'font-lock-defaults) elm--font-lock-highlighting))

(provide 'elm-font-lock)
;;; elm-font-lock.el ends here
