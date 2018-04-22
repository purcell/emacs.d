;;; python-el-expansions.el --- Python-specific expansions for expand-region

;; Copyright (C) 2012 Ivan Andrus

;; Authors: Ivan Andrus, Felix Geller, @edmccard
;; Based on js-mode-expansions by: Magnar Sveen <magnars@gmail.com>
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

;; For python.el included with GNU Emacs
;;  - Mark functionality taken from python.el:
;;    - `python-mark-block'
;;  - Additions implemented here:
;;    - `er/mark-python-statement'
;;    - `er/mark-inside-python-string'
;;    - `er/mark-outside-python-string'
;;  - Supports multi-line strings

;; There is no need for a er/mark-python-defun since
;; er/mark-python-block will mark it

;; Feel free to contribute any other expansions for Python at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(require 'expand-region-core)
(require 'python)

(declare-function python-beginning-of-string "python-mode")

(defvar er--python-string-delimiter "'\"")

(defun er/mark-python-statement ()
  "Marks one Python statement, eg. x = 3"
  (interactive)
  (python-nav-end-of-statement)
  (set-mark (point))
  (python-nav-beginning-of-statement))

(defun er/mark-outside-python-string ()
  "Marks region outside a (possibly multi-line) Python string"
  (interactive)
  (python-beginning-of-string)
  (set-mark (point))
  (forward-sexp)
  (exchange-point-and-mark))

(defun er/mark-inside-python-string ()
  "Marks region inside a (possibly multi-line) Python string"
  (interactive)
  (when (eq 'string (syntax-ppss-context (syntax-ppss)))
    (python-beginning-of-string)
    (let ((string-beginning (point)))
      (forward-sexp)
      (skip-chars-backward er--python-string-delimiter)
      (set-mark (point))
      (goto-char string-beginning)
      (skip-chars-forward er--python-string-delimiter))))

(defun er/add-python-mode-expansions ()
  "Adds Python-specific expansions for buffers in python-mode"
  (let ((try-expand-list-additions '(er/mark-python-statement
                                     er/mark-inside-python-string
                                     er/mark-outside-python-string
                                     python-mark-block)))
    (set (make-local-variable 'expand-region-skip-whitespace) nil)
    (set (make-local-variable 'er/try-expand-list)
         (remove 'er/mark-inside-quotes
                 (remove 'er/mark-outside-quotes
                         (append er/try-expand-list try-expand-list-additions))))))

(er/enable-mode-expansions 'python-mode 'er/add-python-mode-expansions)

(provide 'python-el-expansions)

;; python-el-expansions.el ends here
