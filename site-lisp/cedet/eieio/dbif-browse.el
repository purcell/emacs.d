;;; dbif-browse.el --- generic browser for any dbif child class type
;;
;; Copyright (C) 1996, 1998, 1999 Eric M. Ludlam
;;
;; Author: <zappo@gnu.ai.mit.edu>
;; Version: 0.1
;; RCS: $Id: dbif-browse.el,v 1.5 1999/02/18 19:15:08 zappo Exp $
;; Keywords: OO database
;;                                                                          
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.org.
;;
;; Updates can be found at:
;;    ftp://ftp.ultranet.com/pub/zappo

;;; Commentary:
;;   By using the dbif database superclass, it's possible to use
;; the basic accessors to maintain a browser system (table-list,
;; table-schema, and sample data) for any database with a supporting
;; connection type.  (providing all such requests return a tuple
;; type.)
;;   This mode is not very sophisticated yet, as it serves to provide
;; a basic testing framework to my silly ideals.
;;
;; PREFERRED: Emacs 19 for faces and colors.

(require 'dbif)

;;; Code:
(defvar dbif-browse-mode-map nil
  "Keymap used in various browse modes.")
(if dbif-browse-mode-map
    nil
  (setq dbif-browse-mode-map (make-sparse-keymap))
  (define-key dbif-browse-mode-map "\C-n" 'dbif-browse-next-line)
  (define-key dbif-browse-mode-map "\C-p" 'dbif-browse-prev-line)
  (define-key dbif-browse-mode-map "n"    'dbif-browse-next-line)
  (define-key dbif-browse-mode-map "p"    'dbif-browse-prev-line)
  (define-key dbif-browse-mode-map [down] 'dbif-browse-next-line)
  (define-key dbif-browse-mode-map [up]   'dbif-browse-prev-line)
  )

(defvar dbif-data-object nil
  "Stores a pointer to the dbif object which allows us data access.")

(defvar dbif-local-tuple nil
  "Stores the tuple currently displayed in the current buffer.")

(defvar dbif-local-selected 0
  "Stores the index to the currently selected tuple.")

(defvar dbif-local-link-buffer nil
  "Stores the buffer in which we have linked info.")

(defvar dbif-local-dependants nil
  "List of buffers dependant on out selected field...")

(defmethod dbif-browse ((conn dbif-connection) name)
  "Handle of windows and reading from generic database connection type.
Argument CONN is the current database connection.
Argument NAME is the name of the database to browse."
  ;;(if (or (not database) (string= database ""))
  ;;    (setq database (user-login-name)))
  (let ((table-buffer (get-buffer-create (format "TABLES: %s" name)))
	(schema-buffer (get-buffer-create (format "SCHEMA: %s" name)))
	(sample-buffer (get-buffer-create (format "SAMPLE: %s" name))))
    ;; Now setup the windows nicely
    (delete-other-windows)
    (switch-to-buffer table-buffer)
    (dbif-browse-table-mode conn schema-buffer sample-buffer)
    (split-window (selected-window) 10)
    (split-window (selected-window) 40 t)
    (other-window 1)
    (switch-to-buffer schema-buffer)
    (dbif-browse-schema-mode conn table-buffer)
    (other-window 1)
    (switch-to-buffer sample-buffer)
    (dbif-browse-sample-mode conn schema-buffer)
    (other-window 1)
    (sit-for 0)
    ))

(defun psql-browse (database host port)
  "Browse a Postgres95 database.
Opens a couple windows in which you
can click on items to expand them, or view their values.
Argument DATABASE, HOST, and PORT specifies the data to connect to."
  (interactive "sDatabase: \nsHost: \nsPort: ")
  (require 'psql)
  (let ((dbbuff (psql-set-db database host port)))
    (dbif-browse dbbuff database)))

(defun ingsql-browse (host username database)
  "Browse an Ingres database.
Opens a couple windows in which you
can click on items to expand them, or view their values.
Argument HOST, USERNAME, and DATABASE specifies the data to connect to."
  (interactive "sDatabase: \nsHost: \nsUsername:")
  (require 'ingsql)
  (let ((dbbuff (ingsql-set-db host username database)))
    (dbif-browse dbbuff database)))

(defun dbif-browse-table-mode (dbbuff schem samp)
  "Takes selected buffer and set's it up as a table selection buffer.
Argument DBBUFF , SCHEM, and SAMP specify buffers we depend on."
  (dbif-browse-mode-common (current-buffer) dbbuff)
  (setq major-mode 'dbif-browse-table-mode)
  (setq mode-name "DBIF-table")
  ;; Set our local variables
  (setq dbif-local-link-buffer nil)
  (setq dbif-local-dependants (list schem samp))
  ;; Last thing.. get my tuple and run hooks
  (setq dbif-local-tuple (dbif-get-my-tuple))
  (dbif-update-contents t)
  (run-hooks 'dbif-browse-table-mode-hooks))

(defun dbif-browse-schema-mode (dbbuff tabbuff)
  "Takes selected buffer and set's it up as a table selection buffer.
Argument DBBUFF and TABBUFF specify buffers we modify."
  (dbif-browse-mode-common (current-buffer) dbbuff)
  (setq major-mode 'dbif-browse-sample-mode)
  (setq mode-name "DBIF-schema")
  (setq dbif-local-link-buffer tabbuff)
  ;; Last thing.. get my tuple and run hooks
  (setq dbif-local-tuple (dbif-get-my-tuple))
  (dbif-update-contents t)
  (run-hooks 'dbif-browse-schema-mode-hooks))

(defun dbif-browse-sample-mode (dbbuff schembuff)
  "Takes selected buffer and set's it up as a table selection buffer.
Argument DBBUFF and SCHEMBUFF specify buffers we modify."
  (dbif-browse-mode-common (current-buffer) dbbuff)
  (setq major-mode 'dbif-browse-sample-mode)
  (setq mode-name "DBIF-sample")
  (setq dbif-local-link-buffer schembuff)
  ;; Last thing.. get my tuple and run hooks
  (setq dbif-local-tuple (dbif-get-my-tuple 10))
  (dbif-update-contents t)
  (run-hooks 'dbif-browse-sample-mode-hooks))

(defun dbif-browse-mode-common (buffer dbbuff)
  "Takes BUFFER and turn it into a DBIF mode buffer.
Using DBBUFF as the database link to use for each of these buffers."
  (set-buffer buffer)
  (setq mode-line-buffer-identification (list "DBIF" ": %15b"))
  (make-local-variable 'dbif-data-object)
  (setq dbif-data-object dbbuff)
  (make-local-variable 'dbif-local-tuple)
  (make-local-variable 'dbif-local-selected)
  (setq dbif-local-selected 0)
  (make-local-variable 'dbif-local-link-buffer)
  (make-local-variable 'dbif-local-dependants)
  (setq dbif-local-dependants nil)
  (use-local-map dbif-browse-mode-map)
  )

(defun dbif-browse-next-line (&optional n)
  "Move forward in the DBIF buffer N lines."
  (interactive)
  (if (not n) (setq n 1))
  (setq dbif-local-selected (+ dbif-local-selected n))
  (if (< dbif-local-selected 0) (setq dbif-local-selected 0))
  (if (> dbif-local-selected (- (length (oref dbif-local-tuple values)) 1))
      (setq dbif-local-selected (- (length (oref dbif-local-tuple values)) 1)))
  (dbif-update-contents nil)
  (let ((dependants dbif-local-dependants))
    (while dependants
      (save-excursion
	(set-buffer (car dependants))
	(setq dbif-local-tuple (dbif-get-my-tuple 10))
	(dbif-update-contents t))
      (setq dependants (cdr dependants))))
  )

(defun dbif-browse-prev-line (&optional n)
  "Move backward one line in the DBIF buffer N lines.
Calls `dbif-browse-next-line'"
  (interactive)
  (if (not n) (setq n 1))
  (dbif-browse-next-line (- n)))

(defun dbif-get-my-tuple (&optional dbif-max-parse)
  "Return a tuple which represents the contents of this buffer.
Optional argument DBIF-MAX-PARSE largest number of rows to parse out.r."
  (if (not dbif-local-link-buffer)
      ;; The list of all tables
      (dbif-get-table-list dbif-data-object)
    ;; The schema for a given table
    (if (save-excursion (set-buffer dbif-local-link-buffer)
			(not dbif-local-link-buffer))
	(dbif-get-table-info dbif-data-object
			     (save-excursion
			       (set-buffer dbif-local-link-buffer)
			       (dbif-tuple-value dbif-local-tuple "Relation"
						 dbif-local-selected)))
      ;; The sample data in a window
      (let* ((query (save-excursion
		      (set-buffer dbif-local-link-buffer)
		      (if dbif-local-link-buffer
			  "select * from %s"
			(error "Something goofy in dbif-get-my-tuple"))))
	     (fillin (save-excursion
		       (set-buffer dbif-local-link-buffer)
		       (if dbif-local-link-buffer
			   (set-buffer dbif-local-link-buffer))
		       (dbif-tuple-value dbif-local-tuple "Relation"
					 dbif-local-selected))))
	(dbif-exec dbif-data-object (format query fillin))))))

(defun dbif-update-contents (redraw)
  "Starting in the TABLE buffer, update all buffers based on user's selections.
If REDRAW, the erase what is in the buffer, and re-create it,
otherwise, only update the highlight line"
  (unwind-protect
      (let ((cnt 0)
	    (numfield (dbif-tuple-num-fields dbif-local-tuple))
	    (datalist (oref dbif-local-tuple headers))
	    (sizelist (oref dbif-local-tuple maxwidths))
	    (dlist2 nil)
	    (cl 0)
	    (leaveme (point-min)))
	(toggle-read-only -1)
	;; handle redraw-methods
	(if redraw
	    (progn
	      (erase-buffer)
	      ;; draw the headers
	      (while (< cnt numfield)
		(insert (format (format "%%-%ds " (nth cnt sizelist))
				(nth cnt datalist)))
		(setq cnt (1+ cnt)))
	      (insert "\n"))
	  (goto-char (point-min))
	  (forward-line 1)
	  (remove-text-properties (point-min) (point-max) '(face)))
	(setq cnt 0 datalist (oref dbif-local-tuple values))
	(while datalist
	  (if redraw
	      (progn
		(setq dlist2 (car datalist)
		      cnt 0)
		(while (< cnt numfield)
		  (let* ((ss (nth cnt dlist2)))
		    (while (string-match "\n" ss)
		      (setq ss (replace-match "\\n" nil nil ss)))
		    (setq ss (if (< (length ss) (nth cnt sizelist))
				 ss
			       (substring ss 0 (nth cnt sizelist))))
		    (insert (format (format "%%-%ds " (nth cnt sizelist)) ss))
		    (setq cnt (1+ cnt))))
		(insert "\n"))
	    (forward-line 1))
	  (if (= dbif-local-selected cl)
	      (put-text-property (save-excursion (beginning-of-line)
						 (forward-line -1)
						 (setq leaveme (point)))
				 (point) 'face 'highlight))
	  (setq cl (1+ cl))
	  (setq datalist (cdr datalist)))
	(goto-char (point-min))
	(put-text-property (point) (save-excursion (end-of-line) (point))
			   'face 'underline)
	(goto-char leaveme))
    (set-buffer-modified-p nil)
    (toggle-read-only 1)))

;;; end of lisp
(provide 'dbif-browse)


(provide 'dbif-browse)

;;; dbif-browse.el ends here
