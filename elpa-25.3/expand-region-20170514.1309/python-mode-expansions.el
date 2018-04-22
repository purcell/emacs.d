;;; python-mode-expansions.el --- python-mode-specific expansions for expand-region

;; Copyright (C) 2012 Felix Geller

;; Author: Felix Geller
;; Based on python-mode-expansions by: Ivan Andrus
;; Keywords: marking region python

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Commentary:
;; cf. https://github.com/magnars/expand-region.el/pull/18

;; For python-mode: https://launchpad.net/python-mode
;;  - Mark functionality taken from python-mode:
;;    - `py-mark-expression'
;;    - `py-mark-statement'
;;    - `py-mark-block'
;;    - `py-mark-class'
;;  - Additions implemented here:
;;    - `er/mark-inside-python-string'
;;    - `er/mark-outside-python-string'
;;    - `er/mark-outer-python-block'
;;  - Supports multi-line strings
;;  - Supports incremental expansion of nested blocks

;;; Code:

(require 'expand-region-core)

(defvar er--python-string-delimiter "'\"")

(defalias 'py-goto-beyond-clause 'py-end-of-clause-bol)

(declare-function py-in-string-p "python-mode")
(declare-function py-beginning-of-block "python-mode")
(declare-function py-end-of-block "python-mode")
(declare-function py-mark-block-or-clause "python-mode")
(declare-function py-end-of-clause-bol "python-mode")
(defvar py-indent-offset)

(defun er/mark-outside-python-string ()
  "Marks region outside a (possibly multi-line) Python string"
  (interactive)
  (let ((string-beginning (py-in-string-p)))
    (when string-beginning
      (goto-char string-beginning)
      (set-mark (point))
      (forward-sexp)
      (exchange-point-and-mark))))

(defun er/mark-inside-python-string ()
  "Marks region inside a (possibly multi-line) Python string"
  (interactive)
  (let ((string-beginning (py-in-string-p)))
    (when string-beginning
      (goto-char string-beginning)
      (forward-sexp)
      (skip-chars-backward er--python-string-delimiter)
      (set-mark (point))
      (goto-char string-beginning)
      (skip-chars-forward er--python-string-delimiter))))

(defun er--move-to-beginning-of-outer-python-block (start-column)
  "Assumes that point is in a python block that is surrounded by
another that is not the entire module. Uses `py-indent-offset' to
find the beginning of the surrounding block because
`py-beginning-of-block-position' just looks for the previous
block-starting key word syntactically."
  (while (> (current-column) (- start-column py-indent-offset))
    (forward-line -1)
    (py-beginning-of-block)))

(defun er/mark-outer-python-block ()
  "Attempts to mark a surrounding block by moving to the previous
line and selecting the surrounding block."
  (interactive)
  (let ((start-column (current-column)))
    (when (> start-column 0) ; outer block is the whole buffer
      (er--move-to-beginning-of-outer-python-block start-column)
      (let ((block-beginning (point)))
        (py-end-of-block)
        (set-mark (point))
        (goto-char block-beginning)))))

(defun er/mark-x-python-compound-statement ()
  "Mark the current compound statement (if, while, for, try) and all clauses."
  (interactive)
  (let ((secondary-re
         (save-excursion
           (py-mark-block-or-clause)
           (cond ((looking-at "if\\|for\\|while\\|else\\|elif") "else\\|elif")
                 ((looking-at "try\\|except\\|finally") "except\\|finally"))))
        start-col)
    (when secondary-re
      (py-mark-block-or-clause)
      (setq start-col (current-column))
      (while (looking-at secondary-re)
        (forward-line -1) (back-to-indentation)
        (while (> (current-column) start-col)
          (forward-line -1) (back-to-indentation)))
      (set-mark (point))
      (py-end-of-clause-bol) (forward-line) (back-to-indentation)
      (while (and (looking-at secondary-re)
                  (>= (current-column) start-col))
        (py-end-of-clause-bol) (forward-line) (back-to-indentation))
      (forward-line -1) (end-of-line)
      (exchange-point-and-mark))))

(defun er/add-python-mode-expansions ()
  "Adds python-mode-specific expansions for buffers in python-mode"
  (let ((try-expand-list-additions '(
                                     er/mark-inside-python-string
                                     er/mark-outside-python-string
                                     py-mark-expression
                                     py-mark-statement
                                     py-mark-block
                                     py-mark-def
                                     py-mark-clause
                                     er/mark-x-python-compound-statement
                                     er/mark-outer-python-block
                                     py-mark-class
                                     )))
    (set (make-local-variable 'expand-region-skip-whitespace) nil)
    (set (make-local-variable 'er/try-expand-list)
         (remove 'er/mark-inside-quotes
                 (remove 'er/mark-outside-quotes
                         (append er/try-expand-list try-expand-list-additions))))))

(er/enable-mode-expansions 'python-mode 'er/add-python-mode-expansions)

(provide 'python-mode-expansions)

;; python-mode-expansions.el ends here
