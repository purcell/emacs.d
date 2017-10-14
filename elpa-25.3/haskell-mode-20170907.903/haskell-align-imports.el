;;; haskell-align-imports.el --- Align the import lines in a Haskell file -*- lexical-binding: t -*-

;; Copyright (C) 2010  Chris Done

;; Author: Chris Done <chrisdone@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Consider the following imports list:
;;
;; import One
;; import Two as A
;; import qualified Three
;; import qualified Four as PRELUDE
;; import Five (A)
;; import Six (A,B)
;; import qualified Seven (A,B)
;; import "abc" Eight
;; import "abc" Nine as TWO
;; import qualified "abc" Ten
;; import qualified "defg" Eleven as PRELUDE
;; import "barmu" Twelve (A)
;; import "zotconpop" Thirteen (A,B)
;; import qualified "z" Fourteen (A,B)
;; import Fifteen hiding (A)
;; import Sixteen as TWO hiding (A)
;; import qualified Seventeen hiding (A)
;; import qualified Eighteen as PRELUDE hiding (A)
;; import "abc" Nineteen hiding (A)
;; import "abc" Twenty as TWO hiding (A)
;;
;; When haskell-align-imports is run within the same buffer, the
;; import list is transformed to:
;;
;; import "abc"            Eight
;; import qualified        Eighteen as PRELUDE hiding (A)
;; import qualified "defg" Eleven as PRELUDE
;; import                  Fifteen hiding (A)
;; import                  Five (A)
;; import qualified        Four as PRELUDE
;; import qualified "z"    Fourteen  (A,B)
;; import "abc"            Nine as TWO
;; import "abc"            Nineteen hiding (A)
;; import                  One
;; import qualified        Seven (A,B)
;; import qualified        Seventeen hiding (A)
;; import                  Six (A,B)
;; import                  Sixteen as TWO hiding (A)
;; import qualified "abc"  Ten
;; import "zotconpop"      Thirteen (A,B)
;; import qualified        Three
;; import "barmu"          Twelve (A)
;; import "abc"            Twenty as TWO hiding (A)
;; import                  Two as A
;;
;; If you want everything after module names to be padded out, too,
;; customize `haskell-align-imports-pad-after-name', and you'll get:
;;
;; import                  One
;; import                  Two       as A
;; import qualified        Three
;; import qualified        Four      as PRELUDE
;; import                  Five      (A)
;; import                  Six       (A,B)
;; import qualified        Seven     (A,B)
;; import "abc"            Eight
;; import "abc"            Nine      as TWO
;; import qualified "abc"  Ten
;; import qualified "defg" Eleven    as PRELUDE
;; import "barmu"          Twelve    (A)
;; import "zotconpop"      Thirteen  (A,B)
;; import qualified "z"    Fourteen  (A,B)
;; import                  Fifteen   hiding (A)
;; import                  Sixteen   as TWO hiding (A)
;; import qualified        Seventeen hiding (A)
;; import qualified        Eighteen  as PRELUDE hiding (A)
;; import "abc"            Nineteen  hiding (A)
;; import "abc"            Twenty    as TWO hiding (A)

;;; Code:

(require 'cl-lib)

(defvar haskell-align-imports-regexp
  (concat "^\\(import[ ]+\\)"
          "\\(qualified \\)?"
          "[ ]*\\(\"[^\"]*\" \\)?"
          "[ ]*\\([A-Za-z0-9_.']+\\)"
          "[ ]*\\([ ]*as [A-Z][^ ]*\\)?"
          "[ ]*\\((.*)\\)?"
          "\\([ ]*hiding (.*)\\)?"
          "\\( -- .*\\)?[ ]*$")
  "Regex used for matching components of an import.")

(defcustom haskell-align-imports-pad-after-name
  nil
  "Pad layout after the module name also."
  :type 'boolean
  :group 'haskell-interactive)

;;;###autoload
(defun haskell-align-imports ()
  "Align all the imports in the buffer."
  (interactive)
  (when (haskell-align-imports-line-match)
    (save-excursion
      (goto-char (point-min))
      (let* ((imports (haskell-align-imports-collect))
             (padding (haskell-align-imports-padding imports)))
        (mapc (lambda (x)
                (goto-char (cdr x))
                (delete-region (point) (line-end-position))
                (insert (haskell-align-imports-chomp
                         (haskell-align-imports-fill padding (car x)))))
              imports))))
  nil)

(defun haskell-align-imports-line-match ()
  "Try to match the current line as a regexp."
  (let ((line (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position))))
    (if (string-match "^import " line)
        line
      nil)))

(defun haskell-align-imports-collect ()
  "Collect a list of mark / import statement pairs."
  (let ((imports '()))
    (while (not (or (equal (point) (point-max)) (haskell-align-imports-after-imports-p)))
      (let ((line (haskell-align-imports-line-match-it)))
        (when line
          (let ((match
                 (haskell-align-imports-merge-parts
                  (cl-loop for i from 1 to 8
                           collect (haskell-align-imports-chomp (match-string i line))))))
            (setq imports (cons (cons match (line-beginning-position))
                                imports)))))
      (forward-line))
    imports))

(defun haskell-align-imports-merge-parts (l)
  "Merge together parts of an import statement that shouldn't be separated."
  (let ((parts (apply #'vector l))
        (join (lambda (ls)
                (cl-reduce (lambda (a b)
                             (concat a
                                     (if (and (> (length a) 0)
                                              (> (length b) 0))
                                         " "
                                       "")
                                     b))
                           ls))))
    (if haskell-align-imports-pad-after-name
        (list (funcall join (list (aref parts 0)
                                  (aref parts 1)
                                  (aref parts 2)))
              (aref parts 3)
              (funcall join (list (aref parts 4)
                                  (aref parts 5)
                                  (aref parts 6)))
              (aref parts 7))
      (list (funcall join (list (aref parts 0)
                                (aref parts 1)
                                (aref parts 2)))
            (funcall join (list (aref parts 3)
                                (aref parts 4)
                                (aref parts 5)
                                (aref parts 6)
                                (aref parts 7)))))))

(defun haskell-align-imports-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (if str
      (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" ""
                                str)
    ""))

(defun haskell-align-imports-padding (imports)
  "Find the padding for each part of the import statements."
  (if (null imports)
      imports
    (cl-reduce (lambda (a b) (cl-mapcar #'max a b))
               (mapcar (lambda (x) (mapcar #'length (car x)))
                       imports))))

(defun haskell-align-imports-fill (padding line)
  "Fill an import line using the padding worked out from all statements."
  (mapconcat #'identity
             (cl-mapcar (lambda (pad part)
                          (if (> (length part) 0)
                              (concat part (make-string (- pad (length part)) ? ))
                            (make-string pad ? )))
                        padding
                        line)
             " "))

(defun haskell-align-imports-line-match-it ()
  "Try to match the current line as a regexp."
  (let ((line (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position))))
    (if (string-match haskell-align-imports-regexp line)
        line
      nil)))

(defun haskell-align-imports-after-imports-p ()
  "Are we after the imports list?"
  (save-excursion
    (goto-char (line-beginning-position))
    (let ((case-fold-search nil))
      (not (not (search-forward-regexp "\\( = \\|\\<instance\\>\\| :: \\| ∷ \\)"
                                       (line-end-position) t 1))))))

(provide 'haskell-align-imports)

;;; haskell-align-imports.el ends here
