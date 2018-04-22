;;; org-plot.el --- Support for Plotting from Org -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2017 Free Software Foundation, Inc.
;;
;; Author: Eric Schulte <schulte dot eric at gmail dot com>
;; Keywords: tables, plotting
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Borrows ideas and a couple of lines of code from org-exp.el.

;; Thanks to the Org mailing list for testing and implementation and
;; feature suggestions

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-table)

(declare-function gnuplot-delchar-or-maybe-eof "ext:gnuplot" (arg))
(declare-function gnuplot-mode "ext:gnuplot" ())
(declare-function gnuplot-send-buffer-to-gnuplot "ext:gnuplot" ())

(defvar org-plot/gnuplot-default-options
  '((:plot-type . 2d)
    (:with . lines)
    (:ind . 0))
  "Default options to gnuplot used by `org-plot/gnuplot'.")

(defvar org-plot-timestamp-fmt nil)

(defun org-plot/add-options-to-plist (p options)
  "Parse an OPTIONS line and set values in the property list P.
Returns the resulting property list."
  (when options
    (let ((op '(("type"    . :plot-type)
		("script"  . :script)
		("line"    . :line)
		("set"     . :set)
		("title"   . :title)
		("ind"     . :ind)
		("deps"    . :deps)
		("with"    . :with)
		("file"    . :file)
		("labels"  . :labels)
		("map"     . :map)
		("timeind" . :timeind)
		("timefmt" . :timefmt)))
	  (multiples '("set" "line"))
	  (regexp ":\\([\"][^\"]+?[\"]\\|[(][^)]+?[)]\\|[^ \t\n\r;,.]*\\)")
	  (start 0))
      (dolist (o op)
	(if (member (car o) multiples) ;; keys with multiple values
	    (while (string-match
		    (concat (regexp-quote (car o)) regexp)
		    options start)
	      (setq start (match-end 0))
	      (setq p (plist-put p (cdr o)
				 (cons (car (read-from-string
					     (match-string 1 options)))
				       (plist-get p (cdr o)))))
	      p)
	  (if (string-match (concat (regexp-quote (car o)) regexp)
			    options)
	      (setq p (plist-put p (cdr o)
				 (car (read-from-string
				       (match-string 1 options))))))))))
  p)

(defun org-plot/goto-nearest-table ()
  "Move the point forward to the beginning of nearest table.
Return value is the point at the beginning of the table."
  (interactive) (move-beginning-of-line 1)
  (while (not (or (org-at-table-p) (< 0 (forward-line 1)))))
  (goto-char (org-table-begin)))

(defun org-plot/collect-options (&optional params)
  "Collect options from an org-plot `#+Plot:' line.
Accepts an optional property list PARAMS, to which the options
will be added.  Returns the resulting property list."
  (interactive)
  (let ((line (thing-at-point 'line)))
    (if (string-match "#\\+PLOT: +\\(.*\\)$" line)
	(org-plot/add-options-to-plist params (match-string 1 line))
      params)))

(defun org-plot-quote-timestamp-field (s)
  "Convert field S from timestamp to Unix time and export to gnuplot."
  (format-time-string org-plot-timestamp-fmt (org-time-string-to-time s)))

(defun org-plot-quote-tsv-field (s)
  "Quote field S for export to gnuplot."
  (if (string-match org-table-number-regexp s) s
    (if (string-match org-ts-regexp3 s)
	(org-plot-quote-timestamp-field s)
      (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\""))))

(defun org-plot/gnuplot-to-data (table data-file params)
  "Export TABLE to DATA-FILE in a format readable by gnuplot.
Pass PARAMS through to `orgtbl-to-generic' when exporting TABLE."
  (with-temp-file
      data-file
    (setq-local org-plot-timestamp-fmt (or
					(plist-get params :timefmt)
					"%Y-%m-%d-%H:%M:%S"))
    (insert (orgtbl-to-generic
	     table
	     (org-combine-plists
	      '(:sep "\t" :fmt org-plot-quote-tsv-field)
	      params))))
  nil)

(defun org-plot/gnuplot-to-grid-data (table data-file params)
  "Export the data in TABLE to DATA-FILE for gnuplot.
This means in a format appropriate for grid plotting by gnuplot.
PARAMS specifies which columns of TABLE should be plotted as independent
and dependant variables."
  (interactive)
  (let* ((ind (- (plist-get params :ind) 1))
	 (deps (if (plist-member params :deps)
		   (mapcar (lambda (val) (- val 1)) (plist-get params :deps))
		 (let (collector)
		   (dotimes (col (length (nth 0 table)))
		     (setf collector (cons col collector)))
		   collector)))
	 (counter 0)
	 row-vals)
    (when (>= ind 0) ;; collect values of ind col
      (setf row-vals (mapcar (lambda (row) (setf counter (+ 1 counter))
			       (cons counter (nth ind row))) table)))
    (when (or deps (>= ind 0)) ;; remove non-plotting columns
      (setf deps (delq ind deps))
      (setf table (mapcar (lambda (row)
			    (dotimes (col (length row))
			      (unless (memq col deps)
				(setf (nth col row) nil)))
			    (delq nil row))
			  table)))
    ;; write table to gnuplot grid datafile format
    (with-temp-file data-file
      (let ((num-rows (length table)) (num-cols (length (nth 0 table)))
	    (gnuplot-row (lambda (col row value)
			   (setf col (+ 1 col)) (setf row (+ 1 row))
			   (format "%f  %f  %f\n%f  %f  %f\n"
				   col (- row 0.5) value ;; lower edge
				   col (+ row 0.5) value))) ;; upper edge
	    front-edge back-edge)
	(dotimes (col num-cols)
	  (dotimes (row num-rows)
	    (setf back-edge
		  (concat back-edge
			  (funcall gnuplot-row (- col 1) row
				   (string-to-number (nth col (nth row table))))))
	    (setf front-edge
		  (concat front-edge
			  (funcall gnuplot-row col row
				   (string-to-number (nth col (nth row table)))))))
	  ;; only insert once per row
	  (insert back-edge) (insert "\n") ;; back edge
	  (insert front-edge) (insert "\n") ;; front edge
	  (setf back-edge "") (setf front-edge ""))))
    row-vals))

(defun org-plot/gnuplot-script (data-file num-cols params &optional preface)
  "Write a gnuplot script to DATA-FILE respecting the options set in PARAMS.
NUM-COLS controls the number of columns plotted in a 2-d plot.
Optional argument PREFACE returns only option parameters in a
manner suitable for prepending to a user-specified script."
  (let* ((type (plist-get params :plot-type))
	 (with (if (eq type 'grid) 'pm3d (plist-get params :with)))
	 (sets (plist-get params :set))
	 (lines (plist-get params :line))
	 (map (plist-get params :map))
	 (title (plist-get params :title))
	 (file (plist-get params :file))
	 (ind (plist-get params :ind))
	 (time-ind (plist-get params :timeind))
	 (timefmt (plist-get params :timefmt))
	 (text-ind (plist-get params :textind))
	 (deps (if (plist-member params :deps) (plist-get params :deps)))
	 (col-labels (plist-get params :labels))
	 (x-labels (plist-get params :xlabels))
	 (y-labels (plist-get params :ylabels))
	 (plot-str "'%s' using %s%d%s with %s title '%s'")
	 (plot-cmd (pcase type
		     (`2d "plot")
		     (`3d "splot")
		     (`grid "splot")))
	 (script "reset")
	 ;; ats = add-to-script
	 (ats (lambda (line) (setf script (concat script "\n" line))))
	 plot-lines)
    (when file				; output file
      (funcall ats (format "set term %s" (file-name-extension file)))
      (funcall ats (format "set output '%s'" file)))
    (pcase type				; type
      (`2d ())
      (`3d (when map (funcall ats "set map")))
      (`grid (funcall ats (if map "set pm3d map" "set pm3d"))))
    (when title (funcall ats (format "set title '%s'" title))) ; title
    (mapc ats lines)					       ; line
    (dolist (el sets) (funcall ats (format "set %s" el)))      ; set
    ;; Unless specified otherwise, values are TAB separated.
    (unless (string-match-p "^set datafile separator" script)
      (funcall ats "set datafile separator \"\\t\""))
    (when x-labels			; x labels (xtics)
      (funcall ats
	       (format "set xtics (%s)"
		       (mapconcat (lambda (pair)
				    (format "\"%s\" %d" (cdr pair) (car pair)))
				  x-labels ", "))))
    (when y-labels			; y labels (ytics)
      (funcall ats
	       (format "set ytics (%s)"
		       (mapconcat (lambda (pair)
				    (format "\"%s\" %d" (cdr pair) (car pair)))
				  y-labels ", "))))
    (when time-ind			; timestamp index
      (funcall ats "set xdata time")
      (funcall ats (concat "set timefmt \""
			   (or timefmt	; timefmt passed to gnuplot
			       "%Y-%m-%d-%H:%M:%S") "\"")))
    (unless preface
      (pcase type			; plot command
	(`2d (dotimes (col num-cols)
	       (unless (and (eq type '2d)
			    (or (and ind (equal (1+ col) ind))
				(and deps (not (member (1+ col) deps)))))
		 (setf plot-lines
		       (cons
			(format plot-str data-file
				(or (and ind (> ind 0)
					 (not text-ind)
					 (format "%d:" ind)) "")
				(1+ col)
				(if text-ind (format ":xticlabel(%d)" ind) "")
				with
				(or (nth col col-labels)
				    (format "%d" (1+ col))))
			plot-lines)))))
	(`3d
	 (setq plot-lines (list (format "'%s' matrix with %s title ''"
					data-file with))))
	(`grid
	 (setq plot-lines (list (format "'%s' with %s title ''"
					data-file with)))))
      (funcall ats
	       (concat plot-cmd " " (mapconcat #'identity
					       (reverse plot-lines)
					       ",\\\n    "))))
    script))

;;-----------------------------------------------------------------------------
;; facade functions
;;;###autoload
(defun org-plot/gnuplot (&optional params)
  "Plot table using gnuplot.  Gnuplot options can be specified with PARAMS.
If not given options will be taken from the +PLOT
line directly before or after the table."
  (interactive)
  (require 'gnuplot)
  (save-window-excursion
    (delete-other-windows)
    (when (get-buffer "*gnuplot*") ; reset *gnuplot* if it already running
      (with-current-buffer "*gnuplot*"
	(goto-char (point-max))))
    (org-plot/goto-nearest-table)
    ;; Set default options.
    (dolist (pair org-plot/gnuplot-default-options)
      (unless (plist-member params (car pair))
	(setf params (plist-put params (car pair) (cdr pair)))))
    ;; collect table and table information
    (let* ((data-file (make-temp-file "org-plot"))
	   (table (org-table-to-lisp))
	   (num-cols (length (if (eq (nth 0 table) 'hline) (nth 1 table)
			       (nth 0 table)))))
      (run-with-idle-timer 0.1 nil #'delete-file data-file)
      (while (eq 'hline (car table)) (setf table (cdr table)))
      (when (eq (cadr table) 'hline)
	(setf params
	      (plist-put params :labels (nth 0 table))) ; headers to labels
	(setf table (delq 'hline (cdr table)))) ; clean non-data from table
      ;; Collect options.
      (save-excursion (while (and (equal 0 (forward-line -1))
				  (looking-at "[[:space:]]*#\\+"))
			(setf params (org-plot/collect-options params))))
      ;; Dump table to datafile (very different for grid).
      (pcase (plist-get params :plot-type)
	(`2d   (org-plot/gnuplot-to-data table data-file params))
	(`3d   (org-plot/gnuplot-to-data table data-file params))
	(`grid (let ((y-labels (org-plot/gnuplot-to-grid-data
				table data-file params)))
		 (when y-labels (plist-put params :ylabels y-labels)))))
      ;; Check for timestamp ind column.
      (let ((ind (1- (plist-get params :ind))))
	(when (and (>= ind 0) (eq '2d (plist-get params :plot-type)))
	  (if (= (length
		  (delq 0 (mapcar
			   (lambda (el)
			     (if (string-match org-ts-regexp3 el) 0 1))
			   (mapcar (lambda (row) (nth ind row)) table))))
		 0)
	      (plist-put params :timeind t)
	    ;; Check for text ind column.
	    (if (or (string= (plist-get params :with) "hist")
		    (> (length
			(delq 0 (mapcar
				 (lambda (el)
				   (if (string-match org-table-number-regexp el)
				       0 1))
				 (mapcar (lambda (row) (nth ind row)) table))))
		       0))
		(plist-put params :textind t)))))
      ;; Write script.
      (with-temp-buffer
	(if (plist-get params :script)	; user script
	    (progn (insert
                    (org-plot/gnuplot-script data-file num-cols params t))
                   (insert "\n")
                   (insert-file-contents (plist-get params :script))
                   (goto-char (point-min))
                   (while (re-search-forward "$datafile" nil t)
                     (replace-match data-file nil nil)))
	  (insert (org-plot/gnuplot-script data-file num-cols params)))
	;; Graph table.
	(gnuplot-mode)
	(gnuplot-send-buffer-to-gnuplot))
      ;; Cleanup.
      (bury-buffer (get-buffer "*gnuplot*")))))

(provide 'org-plot)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-plot.el ends here
