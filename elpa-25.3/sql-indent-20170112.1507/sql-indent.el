;;; sql-indent.el --- indentation of SQL statements

;; Copyright (C) 2000  Alex Schroeder

;; Authors: Alex Schroeder <alex@gnu.org>
;;          Matt Henry <mcthenry+gnu@gmail.com>
;; Maintainer: Boerge Svingen <bsvingen@borkdal.com>
;; Version: $Id: sql-indent.el,v 1.10 2009/03/25 22:52:25 mhenry Exp $
;; Package-Version: 20170112.1507

;; Keywords: languages
;; URL: https://github.com/bsvingen/sql-indent

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Indent SQL statements.

;; As the indentation of SQL statements depends not only on the previous
;; line but also on the current line, empty lines cannot always be
;; indented correctly.

;; Usage note: Loading this file will make all SQL mode buffers created
;; from then on use `sql-indent-line' for indentation.  A possible way
;; to install sql-indent.el would be to add the following to your
;; .emacs:

;; (eval-after-load "sql"
;;   '(load-library "sql-indent"))

;; Thanks:
;; Arcady Genkin <antipode@thpoon.com>


;;; History:
;; 2009-03-22*
;;     * mhenry
;;             Added `sql-indent-buffer' for efficient full buffer processing.
;;             Modified `sql-indent' to be savvy to comments and strings.
;;             Removed "and", "or" and "exists" from `sql-indent-first-column-regexp'
;;             Added "create", "drop" and "truncate" to `sql-indent-first-column-regexp'

;;; Code:

(require 'sql)

;; Need the following to allow GNU Emacs 19 to compile the file.
(require 'regexp-opt)

(defcustom sql-indent-first-column-regexp
  (concat "\\(^\\s-*" (regexp-opt '(
				    "select" "update" "insert" "delete"
				    "union" "intersect"
				    "from" "where" "into" "group" "having" "order"
				    "set"
				    "create" "drop" "truncate"
				    "--") t) "\\(\\b\\|\\s-\\)\\)\\|\\(^```$\\)")
  "Regexp matching keywords relevant for indentation.
The regexp matches lines which start SQL statements and it matches lines
that should be indented at the same column as the start of the SQL
statement.  The regexp is created at compile-time.  Take a look at the
source before changing it.  All lines not matching this regexp will be
indented by `sql-indent-offset'."
  :type 'regexp
  :group 'SQL)

(defcustom sql-indent-offset 4
  "*Offset for SQL indentation."
  :type 'number
  :group 'SQL)


(defvar sql-indent-debug nil
  "If non-nil, `sql-indent-line' will output debugging messages.")

(defun sql-indent-is-string-or-comment ()
  "Return nil if point is not in a comment or string; non-nil otherwise."
  (let ((parse-state (syntax-ppss)))
    (or (nth 3 parse-state)             ; String
	(nth 4 parse-state)))           ; Comment
  )

(defun sql-indent-get-last-line-start ()
  "Find the last non-blank line.  Return the beginning position of that line and its indentation."

  (save-excursion
    (forward-line -1)

    (while (and (not (bobp))
		(or
		 (looking-at "^\\s-*$")
		 (sql-indent-is-string-or-comment)) ; Skip comments or strings
		)

      (forward-line -1))
    (list (point) (current-indentation))
    )
  )

(defun sql-indent-level-delta (&optional prev-start prev-indent)
  "Calculate the change in level from the previous non-blank line.
Given the optional parameter `PREV-START' and `PREV-INDENT', assume that to be
the previous non-blank line.
Return a list containing the level change and the previous indentation."

  (save-excursion
    ;; Go back to the previous non-blank line
    (let* ((p-line (cond ((and prev-start prev-indent)
			  (list prev-start prev-indent))
			 ((sql-indent-get-last-line-start))))
	   (curr-start (point-at-bol))
	   (paren (nth 0 (parse-partial-sexp (nth 0 p-line) curr-start))))

      ;; Add opening or closing parens.
      ;; If the current line starts with a keyword statement (e.g. SELECT, FROM, ...) back up one level
      ;; If the previous line starts with a keyword statement then add one level

      (list
       (+ paren
	  (if (progn (goto-char (nth 0 p-line))
		     (looking-at sql-indent-first-column-regexp))
	      1
	    0)
	  (if (progn (goto-char curr-start)
		     (looking-at sql-indent-first-column-regexp))
	      -1
	    0)
	  )
       (nth 1 p-line))
      )
    )
  )

(defun sql-indent-buffer ()
  "Indent the buffer's SQL statements."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (/= (point) (point-max))
	(forward-line)
	(sql-indent-line)
	(end-of-line))))

(defun sql-indent-line ()
  "Indent current line in an SQL statement."
  (interactive)
  (let* ((pos (point))
	 (indent-info (sql-indent-level-delta))
	 (level-delta (nth 0 indent-info))
	 (prev-indent (nth 1 indent-info))
	 (this-indent (max 0            ; Make sure the indentation is at least 0
			   (+ prev-indent
			      (* sql-indent-offset
				 (nth 0 indent-info)))))
	 )

    (if sql-indent-debug
	(message "SQL Indent: line: %3d, level delta: %3d; prev: %3d; this: %3d"
		 (line-number-at-pos) level-delta prev-indent this-indent))

    (save-excursion

      (beginning-of-line)

      (if (and (not (looking-at "^\\s-*$")) ; Leave blank lines alone
	       (not (sql-indent-is-string-or-comment))  ; Don't mess with comments or strings
	       (/= this-indent (current-indentation))) ; Don't change the line if already ok.
	  (indent-line-to this-indent))
      )
    )
  )

(add-hook 'sql-mode-hook
	  (function (lambda ()
		      (make-local-variable 'indent-line-function)
		      (setq indent-line-function 'sql-indent-line))))

(provide 'sql-indent)

;;; sql-indent.el ends here
