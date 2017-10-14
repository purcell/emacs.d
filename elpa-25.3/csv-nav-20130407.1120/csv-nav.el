;;; csv-nav.el --- navigate and edit CSV files
;; Package-Version: 20130407.1120

;; Copyright (C) 2006  Alex Schroeder <alex@gnu.org>

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; Use `csv-nav-mode' to edit CSV files such as contacts exported from
;; other applications.

;;; Code:

;;;###autoload
(define-generic-mode csv-nav-mode
  nil '(",") nil '(".csv\\'")
  '((lambda ()
      (local-set-key (kbd "RET") 'csv-nav-edit)))
  "Major mode for viewing and editing CSV files.")

(defvar csv-nav-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\"  "\"" table)
    (modify-syntax-entry ?,  "." table)
    table))

(defun csv-nav-parse-field (start)
  "Return field starting at START and ending at point."
  (let ((field (buffer-substring start (point))))
    ;; remove double quotes, fix newlines
    (when (and (> (point) start); no quotes in zero length fields 
	       (= (aref field 0) ?\")
	       (= (char-before) ?\"))
      (setq field
	    (replace-regexp-in-string
	     "\r" "" (replace-regexp-in-string
		      "\"\"" "\"" (substring field 1 -1)))))
    field))

(defun csv-nav-parse-line ()
  "Parse the current line and return the list of values."
  (let ((start (point))
	result)
    (with-syntax-table csv-nav-syntax-table
      (while start
	(skip-syntax-forward "^.\" ")
	(cond ((eq (char-after) ?,)
	       (setq result (cons (csv-nav-parse-field start) result)
		     start (1+ (point)))
	       (forward-char 1))
	      ((eq (char-after) ?\n)
	       (setq result (cons (csv-nav-parse-field start)
				  result)
		     start nil)
	       (forward-char 1))
	      ((eq (char-after) ?\")
	       (forward-sexp 1))
	      (t
	       (forward-char 1))))	; break
      (nreverse result))))

(defun csv-nav-parse-buffer ()
  "Parse the current line and return the list of values."
  (goto-char (point-min))
  (let (data (max (point-max)))
    (while (< (point) max)
      (setq data (cons (csv-nav-parse-line) data)))
    (nreverse data)))

(defun csv-nav-get-columns ()
  "Get the field names of the buffer."
  (save-excursion
    (goto-char (point-min))
    (csv-nav-parse-line)))

(defun csv-nav-edit ()
  "Edit the current row."
  (interactive)
  (let ((columns (csv-nav-get-columns))
	start end cells source)
    (save-excursion
      (beginning-of-line)
      (setq start (point-marker)
	    cells (csv-nav-parse-line)
	    end (point-marker)
	    source (list (current-buffer) start end)))
    (when (< (length columns)
	     (length cells))
      (error "Not enough columns for all the cells"))
    (pop-to-buffer (get-buffer-create
		    (car (delete "" (copy-sequence cells)))))
    (erase-buffer)
    (text-mode)
    (local-set-key (kbd "C-c C-c") 'csv-nav-edit-save)
    (local-set-key (kbd "C-c C-n") 'csv-nav-insert)
    (set (make-local-variable 'csv-nav-source) source)
    (while columns
      (when (> (length (car cells)) 0)
	(insert (propertize (concat (car columns) ": ")
			    'field 'column
			    'face 'bold
			    'rear-nonsticky t)
		(car cells) "\n\n"))
      (setq columns (cdr columns)
	    cells (cdr cells)))
    (goto-char (point-min))))

(defun csv-nav-quote-field (str)
  "Strip leading and trailing whitespace, quote double quotes."
  (let ((rules '(("\"" . "\"\"")
		 ("\\s-+$" . "")
		 ("^\\s-+" . "")
		 ("\n" . "\r\n"))))
    (dolist (rule rules)
      (setq str (replace-regexp-in-string (car rule) (cdr rule) str)))
    (when (string-match "\n\\|," str)
      (setq str (concat "\"" str "\"")))
    str))

(defun csv-nav-parse-edit-buffer (columns)
  "Parse the edit buffer back into data for CSV files.
The source buffer knows COLUMNS.  Parses buffers that use the following
format:

column: text

column: text
text text text

The COLUMN must be a known value from the COLUMNS list.  Empty
lines separate fields.  The COLUMN must be inserted using
`csv-nav-edit-add-column'."
  (let (data-alist column start)
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((next-change
	      (or (next-single-property-change (point) 'field (current-buffer))
		  (point-max)))
	     (str (buffer-substring (point) next-change)))
	(cond ((get-text-property (point) 'field)
	       (when (equal ": " (substring str -2))
		 (setq column (substring str 0 -2))); strip ": "
	       (unless (member column columns)
		 (error "Column %s is not known in the source buffer" column)))
	      (column
	       (setq data-alist (cons (cons column (csv-nav-quote-field str))
				      data-alist)))
	      (t
	       (error "Text before the first field is ignored")))
	(goto-char next-change)))
    (let ((result (nreverse (mapcar (lambda (key)
				      (cdr (assoc key data-alist)))
				    columns))))
      ;; strip empty fields at the back
      (while (not (car result))
	(setq result (cdr result)))
      (nreverse result))))

(defun csv-nav-edit-save ()
  "Save the current buffer back to the file."
  (interactive)
  (when (not (boundp 'csv-nav-source))
      (error "This buffer doesn't know where to save the edit"))
  (let* ((buf (car csv-nav-source))
	 (start (nth 1 csv-nav-source))
	 (end (nth 2 csv-nav-source))
	 (data (csv-nav-parse-edit-buffer
		(with-current-buffer buf
		  (csv-nav-get-columns)))))
    (bury-buffer)
    (switch-to-buffer buf)
    (goto-char start)
    (delete-region start end)
    (insert (mapconcat 'identity data ",") "\n")))

(defun csv-nav-insert ()
  "Insert a new field."
  (interactive)
  (when (not (boundp 'csv-nav-source))
      (error "This buffer doesn't know where to save the edit"))
  (let* ((buf (car csv-nav-source))
	 (columns (with-current-buffer buf
		    (csv-nav-get-columns)))
	 (data (csv-nav-parse-edit-buffer columns))
	 table)
    (while columns
      (unless (car data); only the ones without data!
	(setq table (cons (list (car columns)) table)))
      (setq data (cdr data)
	    columns (cdr columns)))
    (insert (propertize (concat (completing-read "Field: " table) ": ")
			'field 'column
			'face 'bold
			'rear-nonsticky t)
	    "\n\n")
    (forward-char -2)))

(provide 'csv-nav)
;;; csv-nav.el ends here
