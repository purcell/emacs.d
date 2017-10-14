;;; haskell-decl-scan.el --- Declaration scanning module for Haskell Mode -*- lexical-binding: t -*-

;; Copyright (C) 2004, 2005, 2007, 2009  Free Software Foundation, Inc.
;; Copyright (C) 1997-1998  Graeme E Moss
;; Copyright (C) 2016  Chris Gregory

;; Author: 1997-1998 Graeme E Moss <gem@cs.york.ac.uk>
;; Maintainer: Stefan Monnier <monnier@gnu.org>
;; Keywords: declarations menu files Haskell
;; URL: http://cvs.haskell.org/cgi-bin/cvsweb.cgi/fptools/CONTRIB/haskell-modes/emacs/haskell-decl-scan.el?rev=HEAD

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

;; Purpose:
;;
;; Top-level declarations are scanned and placed in a menu.  Supports
;; full Latin1 Haskell 1.4 as well as literate scripts.
;;
;;
;; Installation:
;;
;; To turn declaration scanning on for all Haskell buffers under the
;; Haskell mode of Moss&Thorn, add this to .emacs:
;;
;;    (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
;;
;; Otherwise, call `haskell-decl-scan-mode'.
;;
;;
;; Customisation:
;;
;; M-x customize-group haskell-decl-scan
;;
;;
;; History:
;;
;; If you have any problems or suggestions, after consulting the list
;; below, email gem@cs.york.ac.uk quoting the version of the library
;; you are using, the version of Emacs you are using, and a small
;; example of the problem or suggestion.  Note that this library
;; requires a reasonably recent version of Emacs.
;;
;; Uses `imenu' under Emacs.
;;
;; Version 1.2:
;;   Added support for LaTeX-style literate scripts.
;;
;; Version 1.1:
;;   Use own syntax table.  Fixed bug for very small buffers.  Use
;;   markers instead of pointers (markers move with the text).
;;
;; Version 1.0:
;;   Brought over from Haskell mode v1.1.
;;
;;
;; Present Limitations/Future Work (contributions are most welcome!):
;;
;; . Declarations requiring information extending beyond starting line
;;   don't get scanned properly, eg.
;;   > class Eq a =>
;;   >       Test a
;;
;; . Comments placed in the midst of the first few lexemes of a
;;   declaration will cause havoc, eg.
;;   > infixWithComments :: Int -> Int -> Int
;;   > x {-nastyComment-} `infixWithComments` y = x + y
;;   but are not worth worrying about.
;;
;; . Would be nice to scan other top-level declarations such as
;;   methods of a class, datatype field labels...  any more?
;;
;; . Support for GreenCard?
;;
;; . Re-running (literate-)haskell-imenu should not cause the problems
;;   that it does.  The ability to turn off scanning would also be
;;   useful.  (Note that re-running (literate-)haskell-mode seems to
;;   cause no problems.)

;; All functions/variables start with
;; `(turn-(on/off)-)haskell-decl-scan' or `haskell-ds-'.

;; The imenu support is based on code taken from `hugs-mode',
;; thanks go to Chris Van Humbeeck.

;; Version.

;;; Code:

(require 'cl-lib)
(require 'haskell-mode)
(require 'syntax)
(require 'imenu)

;;;###autoload
(defgroup haskell-decl-scan nil
  "Haskell declaration scanning (`imenu' support)."
  :link '(custom-manual "(haskell-mode)haskell-decl-scan-mode")
  :group 'haskell
  :prefix "haskell-decl-scan-")

(defcustom haskell-decl-scan-bindings-as-variables nil
  "Whether to put top-level value bindings into a \"Variables\" category."
  :group 'haskell-decl-scan
  :type 'boolean)

(defcustom haskell-decl-scan-add-to-menubar t
  "Whether to add a \"Declarations\" menu entry to menu bar."
  :group 'haskell-decl-scan
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General declaration scanning functions.

(defvar haskell-ds-start-keywords-re
  (concat "\\(\\<"
          "class\\|data\\|i\\(mport\\|n\\(fix\\(\\|[lr]\\)\\|stance\\)\\)\\|"
          "module\\|primitive\\|type\\|newtype"
          "\\)\\>")
  "Keywords that may start a declaration.")

(defvar haskell-ds-syntax-table
  (let ((table (copy-syntax-table haskell-mode-syntax-table)))
    (modify-syntax-entry ?\' "w" table)
    (modify-syntax-entry ?_  "w" table)
    (modify-syntax-entry ?\\ "_" table)
    table)
  "Syntax table used for Haskell declaration scanning.")


(defun haskell-ds-get-variable (prefix)
  "Return variable involved in value binding or type signature.
Assumes point is looking at the regexp PREFIX followed by the
start of a declaration (perhaps in the middle of a series of
declarations concerning a single variable).  Otherwise return nil.
Point is not changed."
  ;; I think I can now handle all declarations bar those with comments
  ;; nested before the second lexeme.
  (save-excursion
    (with-syntax-table haskell-ds-syntax-table
      (if (looking-at prefix) (goto-char (match-end 0)))
      ;; Keyword.
      (if (looking-at haskell-ds-start-keywords-re)
          nil
        (or ;; Parenthesized symbolic variable.
         (and (looking-at "(\\(\\s_+\\))") (match-string-no-properties 1))
         ;; General case.
         (if (looking-at
              (if (eq ?\( (char-after))
                  ;; Skip paranthesised expression.
                  (progn
                    (forward-sexp)
                    ;; Repeating this code and avoiding moving point if
                    ;; possible speeds things up.
                    "\\(\\'\\)?\\s-*\\(\\s_+\\|`\\(\\sw+\\)`\\)")
                "\\(\\sw+\\)?\\s-*\\(\\s_+\\|`\\(\\sw+\\)`\\)"))
             (let ((match2 (match-string-no-properties 2)))
               ;; Weed out `::', `∷',`=' and `|' from potential infix
               ;; symbolic variable.
               (if (member match2 '("::" "∷" "=" "|"))
                   ;; Variable identifier.
                   (match-string-no-properties 1)
                 (if (eq (aref match2 0) ?\`)
                     ;; Infix variable identifier.
                     (match-string-no-properties 3)
                   ;; Infix symbolic variable.
                   match2))))
         ;; Variable identifier.
         (and (looking-at "\\sw+") (match-string-no-properties 0)))))))

(defun haskell-ds-move-to-start-regexp (inc regexp)
  "Move to beginning of line that succeeds/precedes (INC = 1/-1)
current line that starts with REGEXP and is not in `font-lock-comment-face'."
  ;; Making this defsubst instead of defun appears to have little or
  ;; no effect on efficiency.  It is probably not called enough to do
  ;; so.
  (while (and (= (forward-line inc) 0)
              (or (not (looking-at regexp))
                  (eq (get-text-property (point) 'face)
                      'font-lock-comment-face)))))

(defun haskell-ds-move-to-start-regexp-skipping-comments (inc regexp)
  "Like haskell-ds-move-to-start-regexp, but uses syntax-ppss to
  skip comments"
  (let (p)
    (cl-loop
     do (setq p (point))
     (haskell-ds-move-to-start-regexp inc regexp)
     while (and (nth 4 (syntax-ppss))
                (/= p (point))))))

(defvar literate-haskell-ds-line-prefix "> ?"
  "Regexp matching start of a line of Bird-style literate code.
Current value is \"> \" as we assume top-level declarations start
at column 3.  Must not contain the special \"^\" regexp as we may
not use the regexp at the start of a regexp string.  Note this is
only for `imenu' support.")

(defvar haskell-ds-start-decl-re "\\(\\sw\\|(\\)"
  "The regexp that starts a Haskell declaration.")

(defvar literate-haskell-ds-start-decl-re
  (concat literate-haskell-ds-line-prefix haskell-ds-start-decl-re)
  "The regexp that starts a Bird-style literate Haskell declaration.")

(defun haskell-ds-whitespace-p (char)
  "Test if CHAR is a whitespace character."
  ;; the nil is a bob/eob test
  (member char '(nil ?\t ?\n ?\ )))

(defun haskell-ds-move-to-decl (direction bird-literate fix)
  "General function for moving to the start of a declaration,
either forwards or backwards from point, with normal or with Bird-style
literate scripts.  If DIRECTION is t, then forward, else backward.  If
BIRD-LITERATE is t, then treat as Bird-style literate scripts, else
normal scripts.  Returns point if point is left at the start of a
declaration, and nil otherwise, ie. because point is at the beginning
or end of the buffer and no declaration starts there.  If FIX is t,
then point does not move if already at the start of a declaration."
  ;; As `haskell-ds-get-variable' cannot separate an infix variable
  ;; identifier out of a value binding with non-alphanumeric first
  ;; argument, this function will treat such value bindings as
  ;; separate from the declarations surrounding it.
  (let ( ;; The variable typed or bound in the current series of
        ;; declarations.
        name
        ;; The variable typed or bound in the new declaration.
        newname
        ;; Hack to solve hard problem for Bird-style literate scripts
        ;; that start with a declaration.  We are in the abyss if
        ;; point is before start of this declaration.
        abyss
        (line-prefix (if bird-literate literate-haskell-ds-line-prefix ""))
        ;; The regexp to match for the start of a declaration.
        (start-decl-re (if bird-literate
                           literate-haskell-ds-start-decl-re
                         haskell-ds-start-decl-re))
        (increment (if direction 1 -1))
        (bound (if direction (point-max) (point-min))))
    ;; Change syntax table.
    (with-syntax-table haskell-ds-syntax-table
      ;; move to beginning of line that starts the "current
      ;; declaration" (dependent on DIRECTION and FIX), and then get
      ;; the variable typed or bound by this declaration, if any.
      (let ( ;; Where point was at call of function.
            (here (point))
            ;; Where the declaration on this line (if any) starts.
            (start (progn
                     (beginning-of-line)
                     ;; Checking the face to ensure a declaration starts
                     ;; here seems to be the only addition to make this
                     ;; module support LaTeX-style literate scripts.
                     (if (and (looking-at start-decl-re)
                              (not (elt (syntax-ppss) 4)))
                         (match-beginning 1)))))
        (if (and start
                 ;; This complicated boolean determines whether we
                 ;; should include the declaration that starts on the
                 ;; current line as the "current declaration" or not.
                 (or (and (or (and direction (not fix))
                              (and (not direction) fix))
                          (>= here start))
                     (and (or (and direction fix)
                              (and (not direction) (not fix)))
                          (> here start))))
            ;; If so, we are already at start of the current line, so
            ;; do nothing.
            ()
          ;; If point was before start of a declaration on the first
          ;; line of the buffer (possible for Bird-style literate
          ;; scripts) then we are in the abyss.
          (if (and start (bobp))
              (setq abyss t)
            ;; Otherwise we move to the start of the first declaration
            ;; on a line preceding the current one, skipping comments
            (haskell-ds-move-to-start-regexp-skipping-comments -1 start-decl-re))))
      ;; If we are in the abyss, position and return as appropriate.
      (if abyss
          (if (not direction)
              nil
            (re-search-forward (concat "\\=" line-prefix) nil t)
            (point))
        ;; Get the variable typed or bound by this declaration, if any.
        (setq name (haskell-ds-get-variable line-prefix))
        (if (not name)
            ;; If no such variable, stop at the start of this
            ;; declaration if moving backward, or move to the next
            ;; declaration if moving forward.
            (if direction
                (haskell-ds-move-to-start-regexp-skipping-comments 1 start-decl-re))
          ;; If there is a variable, find the first
          ;; succeeding/preceding declaration that does not type or
          ;; bind it.  Check for reaching start/end of buffer and
          ;; comments.
          (haskell-ds-move-to-start-regexp-skipping-comments increment start-decl-re)
          (while (and (/= (point) bound)
                      (and (setq newname (haskell-ds-get-variable line-prefix))
                           (string= name newname)))
            (setq name newname)
            (haskell-ds-move-to-start-regexp-skipping-comments increment start-decl-re))
          ;; If we are going backward, and have either reached a new
          ;; declaration or the beginning of a buffer that does not
          ;; start with a declaration, move forward to start of next
          ;; declaration (which must exist).  Otherwise, we are done.
          (if (and (not direction)
                   (or (and (looking-at start-decl-re)
                            (not (string= name
                                          ;; Note we must not use
                                          ;; newname here as this may
                                          ;; not have been set if we
                                          ;; have reached the beginning
                                          ;; of the buffer.
                                          (haskell-ds-get-variable
                                           line-prefix))))
                       (and (not (looking-at start-decl-re))
                            (bobp))))
              (haskell-ds-move-to-start-regexp-skipping-comments 1 start-decl-re)))
        ;; Store whether we are at the start of a declaration or not.
        ;; Used to calculate final result.
        (let ((at-start-decl (looking-at start-decl-re)))
          ;; If we are at the beginning of a line, move over
          ;; line-prefix, if present at point.
          (if (bolp)
              (re-search-forward (concat "\\=" line-prefix) (point-max) t))
          ;; Return point if at the start of a declaration and nil
          ;; otherwise.
          (if at-start-decl (point) nil))))))

(defun haskell-ds-bird-p ()
  (and (boundp 'haskell-literate) (eq haskell-literate 'bird)))

(defun haskell-ds-backward-decl ()
  "Move backward to the first character that starts a top-level declaration.
A series of declarations concerning one variable is treated as one
declaration by this function.  So, if point is within a top-level
declaration then move it to the start of that declaration.  If point
is already at the start of a top-level declaration, then move it to
the start of the preceding declaration.  Returns point if point is
left at the start of a declaration, and nil otherwise, because
point is at the beginning of the buffer and no declaration starts
there."
  (interactive)
  (haskell-ds-move-to-decl nil (haskell-ds-bird-p) nil))

(defun haskell-ds-comment-p
    (&optional
     pt)
  "Test if the cursor is on whitespace or a comment.

`PT' defaults to `(point)'"
  ;; ensure result is `t' or `nil' instead of just truthy
  (if (or
       ;; is cursor on whitespace
       (haskell-ds-whitespace-p (following-char))
       ;; http://emacs.stackexchange.com/questions/14269/how-to-detect-if-the-point-is-within-a-comment-area
       ;; is cursor at begging, inside, or end of comment
       (let ((fontfaces (get-text-property (or pt
                                               (point)) 'face)))
         (when (not (listp fontfaces))
           (setf fontfaces (list fontfaces)))
         (delq nil (mapcar
                    #'(lambda (f)
                        (member f '(font-lock-comment-face
                                    font-lock-doc-face
                                    font-lock-comment-delimiter-face)))
                    fontfaces))))
      t
    nil))

(defun haskell-ds-line-commented-p ()
  "Tests if all characters from `point' to `end-of-line' pass
`haskell-ds-comment-p'"
  (let ((r t))
    (while (and r (not (eolp)))
      (if (not (haskell-ds-comment-p))
          (setq r nil))
      (forward-char))
    r))

(defun haskell-ds-forward-decl ()
  "Move forward to the first character that starts a top-level
declaration.  As `haskell-ds-backward-decl' but forward."
  (interactive)
  (let ((p (point)) b e empty was-at-bob)
    ;; Go back to beginning of defun, then go to beginning of next
    (haskell-ds-move-to-decl nil (haskell-ds-bird-p) nil)
    (setq b (point))
    (haskell-ds-move-to-decl t (haskell-ds-bird-p) nil)
    (setq e (point))
    ;; tests if line is empty
    (setq empty (and (<= (point) p)
                     (not (eolp))))
    (setq was-at-bob (and (= (point-min) b)
                          (= b p)
                          (< p e)))
    ;; this conditional allows for when empty lines at end, first
    ;; `C-M-e' will go to end of defun, next will go to end of file.
    (when (or was-at-bob
              empty)
      (if (or (and was-at-bob
                   (= ?\n
                      (save-excursion
                        (goto-char (point-min))
                        (following-char))))
              empty)
          (haskell-ds-move-to-decl t (haskell-ds-bird-p) nil))
      ;; Then go back to end of current
      (forward-line -1)
      (while (and (haskell-ds-line-commented-p)
                  ;; prevent infinite loop
                  (not (bobp)))
        (forward-line -1))
      (forward-line 1)))
  (point))

(defun haskell-ds-generic-find-next-decl (bird-literate)
  "Find the name, position and type of the declaration at or after point.
Return ((NAME . (START-POSITION . NAME-POSITION)) . TYPE)
if one exists and nil otherwise.  The start-position is at the start
of the declaration, and the name-position is at the start of the name
of the declaration.  The name is a string, the positions are buffer
positions and the type is one of the symbols \"variable\", \"datatype\",
\"class\", \"import\" and \"instance\"."
  (let ( ;; The name, type and name-position of the declaration to
        ;; return.
        name
        type
        name-pos
        ;; Buffer positions marking the start and end of the space
        ;; containing a declaration.
        start
        end)
    ;; Change to declaration scanning syntax.
    (with-syntax-table haskell-ds-syntax-table
      ;; Stop when we are at the end of the buffer or when a valid
      ;; declaration is grabbed.
      (while (not (or (eobp) name))
        ;; Move forward to next declaration at or after point.
        (haskell-ds-move-to-decl t bird-literate t)
        ;; Start and end of search space is currently just the starting
        ;; line of the declaration.
        (setq start (point)
              end   (line-end-position))
        (cond
         ;; If the start of the top-level declaration does not begin
         ;; with a starting keyword, then (if legal) must be a type
         ;; signature or value binding, and the variable concerned is
         ;; grabbed.
         ((not (looking-at haskell-ds-start-keywords-re))
          (setq name (haskell-ds-get-variable ""))
          (if name
              (progn
                (setq type 'variable)
                (re-search-forward (regexp-quote name) end t)
                (setq name-pos (match-beginning 0)))))
         ;; User-defined datatype declaration.
         ((re-search-forward "\\=\\(data\\|newtype\\|type\\)\\>" end t)
          (re-search-forward "=>" end t)
          (if (looking-at "[ \t]*\\(\\sw+\\)")
              (progn
                (setq name (match-string-no-properties 1))
                (setq name-pos (match-beginning 1))
                (setq type 'datatype))))
         ;; Class declaration.
         ((re-search-forward "\\=class\\>" end t)
          (re-search-forward "=>" end t)
          (if (looking-at "[ \t]*\\(\\sw+\\)")
              (progn
                (setq name (match-string-no-properties 1))
                (setq name-pos (match-beginning 1))
                (setq type 'class))))
         ;; Import declaration.
         ((looking-at "import[ \t]+\\(?:safe[\t ]+\\)?\\(?:qualified[ \t]+\\)?\\(?:\"[^\"]*\"[\t ]+\\)?\\(\\(?:\\sw\\|.\\)+\\)")
          (setq name (match-string-no-properties 1))
          (setq name-pos (match-beginning 1))
          (setq type 'import))
         ;; Instance declaration.
         ((re-search-forward "\\=instance[ \t]+" end t)
          (re-search-forward "=>[ \t]+" end t)
          ;; The instance "title" starts just after the `instance' (and
          ;; any context) and finishes just before the _first_ `where'
          ;; if one exists.  This solution is ugly, but I can't find a
          ;; nicer one---a simple regexp will pick up the last `where',
          ;; which may be rare but nevertheless...
          (setq name-pos (point))
          (setq name (buffer-substring-no-properties
                      (point)
                      (progn
                        ;; Look for a `where'.
                        (if (re-search-forward "\\<where\\>" end t)
                            ;; Move back to just before the `where'.
                            (progn
                              (re-search-backward "\\s-where")
                              (point))
                          ;; No `where' so move to last non-whitespace
                          ;; before `end'.
                          (progn
                            (goto-char end)
                            (skip-chars-backward " \t")
                            (point))))))
          ;; If we did not manage to extract a name, cancel this
          ;; declaration (eg. when line ends in "=> ").
          (if (string-match "^[ \t]*$" name) (setq name nil))
          (setq type 'instance)))
        ;; Move past start of current declaration.
        (goto-char end))
      ;; If we have a valid declaration then return it, otherwise return
      ;; nil.
      (if name
          (cons (cons name (cons (copy-marker start t) (copy-marker name-pos t)))
                type)
        nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declaration scanning via `imenu'.

;;;###autoload
(defun haskell-ds-create-imenu-index ()
  "Function for finding `imenu' declarations in Haskell mode.
Finds all declarations (classes, variables, imports, instances and
datatypes) in a Haskell file for the `imenu' package."
  ;; Each list has elements of the form `(INDEX-NAME . INDEX-POSITION)'.
  ;; These lists are nested using `(INDEX-TITLE . INDEX-ALIST)'.
  (let* ((bird-literate (haskell-ds-bird-p))
         (index-alist '())
         (index-class-alist '()) ;; Classes
         (index-var-alist '())   ;; Variables
         (index-imp-alist '())   ;; Imports
         (index-inst-alist '())  ;; Instances
         (index-type-alist '())  ;; Datatypes
         ;; Variables for showing progress.
         (bufname (buffer-name))
         (divisor-of-progress (max 1 (/ (buffer-size) 100)))
         ;; The result we wish to return.
         result)
    (goto-char (point-min))
    ;; Loop forwards from the beginning of the buffer through the
    ;; starts of the top-level declarations.
    (while (< (point) (point-max))
      (message "Scanning declarations in %s... (%3d%%)" bufname
               (/ (- (point) (point-min)) divisor-of-progress))
      ;; Grab the next declaration.
      (setq result (haskell-ds-generic-find-next-decl bird-literate))
      (if result
          ;; If valid, extract the components of the result.
          (let* ((name-posns (car result))
                 (name (car name-posns))
                 (posns (cdr name-posns))
                 (start-pos (car posns))
                 (type (cdr result)))
                 ;; Place `(name . start-pos)' in the correct alist.
                 (cl-case type
                   (variable
                    (setq index-var-alist
                          (cl-acons name start-pos index-var-alist)))
                   (datatype
                    (setq index-type-alist
                          (cl-acons name start-pos index-type-alist)))
                   (class
                    (setq index-class-alist
                          (cl-acons name start-pos index-class-alist)))
                   (import
                    (setq index-imp-alist
                          (cl-acons name start-pos index-imp-alist)))
                   (instance
                    (setq index-inst-alist
                          (cl-acons name start-pos index-inst-alist)))))))
    ;; Now sort all the lists, label them, and place them in one list.
    (message "Sorting declarations in %s..." bufname)
    (when index-type-alist
      (push (cons "Datatypes"
                  (sort index-type-alist 'haskell-ds-imenu-label-cmp))
            index-alist))
    (when index-inst-alist
      (push (cons "Instances"
                  (sort index-inst-alist 'haskell-ds-imenu-label-cmp))
            index-alist))
    (when index-imp-alist
      (push (cons "Imports"
                  (sort index-imp-alist 'haskell-ds-imenu-label-cmp))
            index-alist))
    (when index-class-alist
      (push (cons "Classes"
                  (sort index-class-alist 'haskell-ds-imenu-label-cmp))
            index-alist))
    (when index-var-alist
      (if haskell-decl-scan-bindings-as-variables
          (push (cons "Variables"
                      (sort index-var-alist 'haskell-ds-imenu-label-cmp))
                index-alist)
        (setq index-alist (append index-alist
                                  (sort index-var-alist 'haskell-ds-imenu-label-cmp)))))
    (message "Sorting declarations in %s...done" bufname)
    ;; Return the alist.
    index-alist))

(defun haskell-ds-imenu-label-cmp (el1 el2)
  "Predicate to compare labels in lists from `haskell-ds-create-imenu-index'."
  (string< (car el1) (car el2)))

(defun haskell-ds-imenu ()
  "Install `imenu' for Haskell scripts."
  (setq imenu-create-index-function 'haskell-ds-create-imenu-index)
  (when haskell-decl-scan-add-to-menubar
    (imenu-add-to-menubar "Declarations")))

;; The main functions to turn on declaration scanning.
;;;###autoload
(defun turn-on-haskell-decl-scan ()
  "Unconditionally activate `haskell-decl-scan-mode'."
  (interactive)
  (haskell-decl-scan-mode))
(make-obsolete 'turn-on-haskell-decl-scan
               'haskell-decl-scan-mode
               "2015-07-23")

;;;###autoload
(define-minor-mode haskell-decl-scan-mode
  "Toggle Haskell declaration scanning minor mode on or off.
With a prefix argument ARG, enable minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.

See also info node `(haskell-mode)haskell-decl-scan-mode' for
more details about this minor mode.

Top-level declarations are scanned and listed in the menu item
\"Declarations\" (if enabled via option
`haskell-decl-scan-add-to-menubar').  Selecting an item from this
menu will take point to the start of the declaration.

\\[beginning-of-defun] and \\[end-of-defun] move forward and backward to the start of a declaration.

This may link with `haskell-doc-mode'.

For non-literate and LaTeX-style literate scripts, we assume the
common convention that top-level declarations start at the first
column.  For Bird-style literate scripts, we assume the common
convention that top-level declarations start at the third column,
ie. after \"> \".

Anything in `font-lock-comment-face' is not considered for a
declaration.  Therefore, using Haskell font locking with comments
coloured in `font-lock-comment-face' improves declaration scanning.

Literate Haskell scripts are supported: If the value of
`haskell-literate' (set automatically by `literate-haskell-mode')
is `bird', a Bird-style literate script is assumed.  If it is nil
or `tex', a non-literate or LaTeX-style literate script is
assumed, respectively.

Invokes `haskell-decl-scan-mode-hook' on activation."
  :group 'haskell-decl-scan

  (kill-local-variable 'beginning-of-defun-function)
  (kill-local-variable 'end-of-defun-function)
  (kill-local-variable 'imenu-create-index-function)
  (unless haskell-decl-scan-mode
    ;; How can we cleanly remove the "Declarations" menu?
    (when haskell-decl-scan-add-to-menubar
      (local-set-key [menu-bar index] nil)))

  (when haskell-decl-scan-mode
    (setq-local beginning-of-defun-function 'haskell-ds-backward-decl)
    (setq-local end-of-defun-function 'haskell-ds-forward-decl)
    (haskell-ds-imenu)))


;; Provide ourselves:

(provide 'haskell-decl-scan)

;;; haskell-decl-scan.el ends here
