;;; mc-separate-operations.el - functions that work differently on each cursor

;; Copyright (C) 2012-2016 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: editing cursors

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

;; This file contains functions that work differently on each cursor,
;; instead of treating all of them the same.

;; Please see multiple-cursors.el for more commentary.

;;; Code:

(require 'multiple-cursors-core)

;;;###autoload
(defun mc/insert-numbers (arg)
  "Insert increasing numbers for each cursor, starting at
`mc/insert-numbers-default' or ARG."
  (interactive "P")
  (setq mc--insert-numbers-number (or (and arg (prefix-numeric-value arg))
                                      mc/insert-numbers-default))
  (mc/for-each-cursor-ordered
   (mc/execute-command-for-fake-cursor 'mc--insert-number-and-increase cursor)))

(defcustom mc/insert-numbers-default 0
  "The default number at which to start counting for
`mc/insert-numbers'"
  :type 'integer
  :group 'multiple-cursors)

(defvar mc--insert-numbers-number 0)

(defun mc--insert-number-and-increase ()
  (interactive)
  (insert (number-to-string mc--insert-numbers-number))
  (setq mc--insert-numbers-number (1+ mc--insert-numbers-number)))

(defun mc--ordered-region-strings ()
  (let (strings)
    (save-excursion
      (mc/for-each-cursor-ordered
       (setq strings (cons (buffer-substring-no-properties
                            (mc/cursor-beg cursor)
                            (mc/cursor-end cursor)) strings))))
    (nreverse strings)))

;;;###autoload
(defun mc/insert-letters (arg)
  "Insert increasing letters for each cursor, starting at 0 or ARG.
     Where letter[0]=a letter[2]=c letter[26]=aa"
  (interactive "P")
  (setq mc--insert-letters-number (or (and arg (prefix-numeric-value arg))
                                      0))
  (mc/for-each-cursor-ordered
   (mc/execute-command-for-fake-cursor 'mc--insert-letter-and-increase cursor)))

(defun mc--number-to-letters (number)
  (let ((letter
	 (char-to-string
	  (+ (mod number 26) ?a)))
	(number2 (/ number 26)))
    (if (> number2 0)
	(concat (mc--number-to-letters (- number2 1)) letter)
      letter)))

(defvar mc--insert-letters-number 0)

(defun mc--insert-letter-and-increase ()
  (interactive)
  (insert (mc--number-to-letters mc--insert-letters-number))
  (setq mc--insert-letters-number (1+ mc--insert-letters-number)))

(defvar mc--strings-to-replace nil)

(defun mc--replace-region-strings-1 ()
  (interactive)
  (delete-region (region-beginning) (region-end))
  (save-excursion (insert (car mc--strings-to-replace)))
  (setq mc--strings-to-replace (cdr mc--strings-to-replace)))

(defun mc--replace-region-strings ()
  (mc/for-each-cursor-ordered
   (mc/execute-command-for-fake-cursor 'mc--replace-region-strings-1 cursor)))

;;;###autoload
(defun mc/reverse-regions ()
  (interactive)
  (if (not multiple-cursors-mode)
      (progn
        (mc/mark-next-lines 1)
        (mc/reverse-regions)
        (multiple-cursors-mode 0))
    (unless (use-region-p)
      (mc/execute-command-for-all-cursors 'mark-sexp))
    (setq mc--strings-to-replace (nreverse (mc--ordered-region-strings)))
    (mc--replace-region-strings)))

;;;###autoload
(defun mc/sort-regions ()
  (interactive)
  (unless (use-region-p)
    (mc/execute-command-for-all-cursors 'mark-sexp))
  (setq mc--strings-to-replace (sort (mc--ordered-region-strings) 'string<))
  (mc--replace-region-strings))


;;;###autoload
(defun mc/vertical-align (character)
  "Aligns all cursors vertically with a given CHARACTER to the one with the
highest colum number (the rightest).
Might not behave as intended if more than one cursors are on the same line."
  (interactive "c")
  (let ((rightest-column (current-column)))
    (mc/execute-command-for-all-cursors
     (lambda () "get the rightest cursor"
       (interactive)
       (setq rightest-column (max (current-column) rightest-column))
       ))
    (mc/execute-command-for-all-cursors
     (lambda ()
       (interactive)
       (let ((missing-spaces (- rightest-column (current-column))))
	 (save-excursion (insert (make-string missing-spaces character)))
	 (forward-char missing-spaces)
	 )
       ))
      )
    )

;;;###autoload
(defun mc/vertical-align-with-space ()
  "Aligns all cursors with whitespace like `mc/vertical-align' does"
  (interactive)
  (mc/vertical-align 32)
  )

(provide 'mc-separate-operations)
;;; mc-separate-operations.el ends here
