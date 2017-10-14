;;; mc-edit-lines.el

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

;; This file contains functions to add multiple cursors to consecutive lines
;; given an active region.

;; Please see multiple-cursors.el for more commentary.

;;; Code:

(require 'multiple-cursors-core)

(defcustom mc/edit-lines-empty-lines nil
  "What should be done by `mc/edit-lines' when a line is not long enough."
  :type '(radio (const :tag "Pad the line with spaces." pad)
                (const :tag "Ignore the line." ignore)
                (const :tag "Signal an error." error)
                (const :tag "Nothing.  Cursor is at end of line." nil))
  :group 'multiple-cursors)

;;;###autoload
(defun mc/edit-lines (&optional arg)
  "Add one cursor to each line of the active region.
Starts from mark and moves in straight down or up towards the
line point is on.

What is done with lines which are not long enough is governed by
`mc/edit-lines-empty-lines'.  The prefix argument ARG can be used
to override this.  If ARG is a symbol (when called from Lisp),
that symbol is used instead of `mc/edit-lines-empty-lines'.
Otherwise, if ARG negative, short lines will be ignored.  Any
other non-nil value will cause short lines to be padded."
  (interactive "P")
  (when (not (and mark-active (/= (point) (mark))))
    (error "Mark a set of lines first"))
  (mc/remove-fake-cursors)
  (let* ((col (current-column))
         (point-line (line-number-at-pos))
         (mark-line (progn (exchange-point-and-mark) (line-number-at-pos)))
         (direction (if (< point-line mark-line) :up :down))
         (style (cond
                 ;; called from lisp
                 ((and arg (symbolp arg))
                  arg)
                 ;; negative argument
                 ((< (prefix-numeric-value arg) 0)
                  'ignore)
                 (arg 'pad)
                 (t mc/edit-lines-empty-lines))))
    (deactivate-mark)
    (when (and (eq direction :up) (bolp))
      (previous-logical-line 1 nil)
      (move-to-column col))
    ;; Add the cursors
    (while (not (eq (line-number-at-pos) point-line))
      ;; Pad the line
      (when (eq style 'pad)
        (while (< (current-column) col)
          (insert " ")))
      ;; Error
      (when (and (eq style 'error)
                 (not (equal col (current-column))))
        (error "Short line encountered in `mc/edit-lines'"))
      ;; create the cursor
      (unless (and (eq style 'ignore)
                   (not (equal col (current-column))))
        (mc/create-fake-cursor-at-point))
      ;; proceed to next
      (if (eq direction :up)
          (previous-logical-line 1 nil)
        (next-logical-line 1 nil))
      (move-to-column col))
    (multiple-cursors-mode)))

;;;###autoload
(defun mc/edit-ends-of-lines ()
  "Add one cursor to the end of each line in the active region."
  (interactive)
  (mc/edit-lines)
  (mc/execute-command-for-all-cursors 'end-of-line))

;;;###autoload
(defun mc/edit-beginnings-of-lines ()
  "Add one cursor to the beginning of each line in the active region."
  (interactive)
  (mc/edit-lines)
  (mc/execute-command-for-all-cursors 'beginning-of-line))

(provide 'mc-edit-lines)

;;; mc-edit-lines.el ends here
