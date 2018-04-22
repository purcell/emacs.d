;;; ob-sql.el --- Babel Functions for SQL            -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

;; This file is part of GNU Emacs.

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

;; Org-Babel support for evaluating sql source code.
;; (see also ob-sqlite.el)
;;
;; SQL is somewhat unique in that there are many different engines for
;; the evaluation of sql (Mysql, PostgreSQL, etc...), so much of this
;; file will have to be implemented engine by engine.
;;
;; Also SQL evaluation generally takes place inside of a database.
;;
;; Header args used:
;; - engine
;; - cmdline
;; - dbhost
;; - dbport
;; - dbuser
;; - dbpassword
;; - database
;; - colnames (default, nil, means "yes")
;; - result-params
;; - out-file
;;
;; The following are used but not really implemented for SQL:
;; - colname-names
;; - rownames
;; - rowname-names
;;
;; Engines supported:
;; - mysql
;; - dbi
;; - mssql
;; - sqsh
;; - postgresql
;; - oracle
;; - vertica
;;
;; TODO:
;;
;; - support for sessions
;; - support for more engines
;; - what's a reasonable way to drop table data into SQL?
;;

;;; Code:
(require 'ob)

(declare-function org-table-import "org-table" (file arg))
(declare-function orgtbl-to-csv "org-table" (table params))
(declare-function org-table-to-lisp "org-table" (&optional txt))
(declare-function cygwin-convert-file-name-to-windows "cygw32.c" (file &optional absolute-p))

(defvar org-babel-default-header-args:sql '())

(defconst org-babel-header-args:sql
  '((engine	       . :any)
    (out-file	       . :any)
    (dbhost	       . :any)
    (dbport	       . :any)
    (dbuser	       . :any)
    (dbpassword	       . :any)
    (database	       . :any))
  "SQL-specific header arguments.")

(defun org-babel-expand-body:sql (body params)
  "Expand BODY according to the values of PARAMS."
  (org-babel-sql-expand-vars
   body (org-babel--get-vars params)))

(defun org-babel-sql-dbstring-mysql (host port user password database)
  "Make MySQL cmd line args for database connection.  Pass nil to omit that arg."
  (combine-and-quote-strings
   (delq nil
	 (list (when host     (concat "-h" host))
	       (when port     (format "-P%d" port))
	       (when user     (concat "-u" user))
	       (when password (concat "-p" password))
	       (when database (concat "-D" database))))))

(defun org-babel-sql-dbstring-postgresql (host port user database)
  "Make PostgreSQL command line args for database connection.
Pass nil to omit that arg."
  (combine-and-quote-strings
   (delq nil
	 (list (when host (concat "-h" host))
	       (when port (format "-p%d" port))
	       (when user (concat "-U" user))
	       (when database (concat "-d" database))))))

(defun org-babel-sql-dbstring-oracle (host port user password database)
  "Make Oracle command line args for database connection."
  (format "%s/%s@%s:%s/%s" user password host port database))

(defun org-babel-sql-dbstring-mssql (host user password database)
  "Make sqlcmd command line args for database connection.
`sqlcmd' is the preferred command line tool to access Microsoft
SQL Server on Windows and Linux platform."
  (mapconcat #'identity
	     (delq nil
		   (list (when host (format "-S \"%s\"" host))
			 (when user (format "-U \"%s\"" user))
			 (when password (format "-P \"%s\"" password))
			 (when database (format "-d \"%s\"" database))))
	     " "))

(defun org-babel-sql-dbstring-sqsh (host user password database)
  "Make sqsh command line args for database connection.
\"sqsh\" is one method to access Sybase or MS SQL via Linux platform"
  (mapconcat #'identity
             (delq nil
                   (list  (when host     (format "-S \"%s\"" host))
                          (when user     (format "-U \"%s\"" user))
                          (when password (format "-P \"%s\"" password))
                          (when database (format "-D \"%s\"" database))))
             " "))

(defun org-babel-sql-dbstring-vertica (host port user password database)
  "Make Vertica command line args for database connection. Pass nil to omit that arg."
  (mapconcat #'identity
	      (delq nil
		    (list (when host     (format "-h %s" host))
			  (when port     (format "-p %d" port))
			  (when user     (format "-U %s" user))
			  (when password (format "-w %s" (shell-quote-argument password) ))
			  (when database (format "-d %s" database))))
	      " "))

(defun org-babel-sql-convert-standard-filename (file)
  "Convert FILE to OS standard file name.
If in Cygwin environment, uses Cygwin specific function to
convert the file name.  In a Windows-NT environment, do nothing.
Otherwise, use Emacs' standard conversion function."
  (cond ((fboundp 'cygwin-convert-file-name-to-windows)
	 (format "%S" (cygwin-convert-file-name-to-windows file)))
	((string= "windows-nt" system-type) file)
	(t (format "%S" (convert-standard-filename file)))))

(defun org-babel-execute:sql (body params)
  "Execute a block of Sql code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((result-params (cdr (assq :result-params params)))
         (cmdline (cdr (assq :cmdline params)))
         (dbhost (cdr (assq :dbhost params)))
         (dbport (cdr (assq :dbport params)))
         (dbuser (cdr (assq :dbuser params)))
         (dbpassword (cdr (assq :dbpassword params)))
         (database (cdr (assq :database params)))
         (engine (cdr (assq :engine params)))
         (colnames-p (not (equal "no" (cdr (assq :colnames params)))))
         (in-file (org-babel-temp-file "sql-in-"))
         (out-file (or (cdr (assq :out-file params))
                       (org-babel-temp-file "sql-out-")))
	 (header-delim "")
         (command (pcase (intern engine)
                    (`dbi (format "dbish --batch %s < %s | sed '%s' > %s"
				  (or cmdline "")
				  (org-babel-process-file-name in-file)
				  "/^+/d;s/^|//;s/(NULL)/ /g;$d"
				  (org-babel-process-file-name out-file)))
                    (`monetdb (format "mclient -f tab %s < %s > %s"
				      (or cmdline "")
				      (org-babel-process-file-name in-file)
				      (org-babel-process-file-name out-file)))
		    (`mssql (format "sqlcmd %s -s \"\t\" %s -i %s -o %s"
				    (or cmdline "")
				    (org-babel-sql-dbstring-mssql
				     dbhost dbuser dbpassword database)
				    (org-babel-sql-convert-standard-filename
				     (org-babel-process-file-name in-file))
				    (org-babel-sql-convert-standard-filename
				     (org-babel-process-file-name out-file))))
                    (`mysql (format "mysql %s %s %s < %s > %s"
				    (org-babel-sql-dbstring-mysql
				     dbhost dbport dbuser dbpassword database)
				    (if colnames-p "" "-N")
				    (or cmdline "")
				    (org-babel-process-file-name in-file)
				    (org-babel-process-file-name out-file)))
		    (`postgresql (format
				  "%spsql --set=\"ON_ERROR_STOP=1\" %s -A -P \
footer=off -F \"\t\"  %s -f %s -o %s %s"
				  (if dbpassword
				      (format "PGPASSWORD=%s " dbpassword)
				    "")
				  (if colnames-p "" "-t")
				  (org-babel-sql-dbstring-postgresql
				   dbhost dbport dbuser database)
				  (org-babel-process-file-name in-file)
				  (org-babel-process-file-name out-file)
				  (or cmdline "")))
		    (`sqsh (format "sqsh %s %s -i %s -o %s -m csv"
				   (or cmdline "")
				   (org-babel-sql-dbstring-sqsh
				    dbhost dbuser dbpassword database)
				   (org-babel-sql-convert-standard-filename
				    (org-babel-process-file-name in-file))
				   (org-babel-sql-convert-standard-filename
				    (org-babel-process-file-name out-file))))
		    (`vertica (format "vsql %s -f %s -o %s %s"
				    (org-babel-sql-dbstring-vertica
				     dbhost dbport dbuser dbpassword database)
				    (org-babel-process-file-name in-file)
				    (org-babel-process-file-name out-file)
				    (or cmdline "")))
                    (`oracle (format
			      "sqlplus -s %s < %s > %s"
			      (org-babel-sql-dbstring-oracle
			       dbhost dbport dbuser dbpassword database)
			      (org-babel-process-file-name in-file)
			      (org-babel-process-file-name out-file)))
                    (_ (error "No support for the %s SQL engine" engine)))))
    (with-temp-file in-file
      (insert
       (pcase (intern engine)
	 (`dbi "/format partbox\n")
         (`oracle "SET PAGESIZE 50000
SET NEWPAGE 0
SET TAB OFF
SET SPACE 0
SET LINESIZE 9999
SET ECHO OFF
SET FEEDBACK OFF
SET VERIFY OFF
SET HEADING ON
SET MARKUP HTML OFF SPOOL OFF
SET COLSEP '|'

")
	 ((or `mssql `sqsh) "SET NOCOUNT ON

")
	 (`vertica "\\a\n")
	 (_ ""))
       (org-babel-expand-body:sql body params)
       ;; "sqsh" requires "go" inserted at EOF.
       (if (string= engine "sqsh") "\ngo" "")))
    (org-babel-eval command "")
    (org-babel-result-cond result-params
      (with-temp-buffer
	(progn (insert-file-contents-literally out-file) (buffer-string)))
      (with-temp-buffer
	(cond
	 ((memq (intern engine) '(dbi mysql postgresql sqsh vertica))
	  ;; Add header row delimiter after column-names header in first line
	  (cond
	   (colnames-p
	    (with-temp-buffer
	      (insert-file-contents out-file)
	      (goto-char (point-min))
	      (forward-line 1)
	      (insert "-\n")
	      (setq header-delim "-")
	      (write-file out-file)))))
	 (t
	  ;; Need to figure out the delimiter for the header row
	  (with-temp-buffer
	    (insert-file-contents out-file)
	    (goto-char (point-min))
	    (when (re-search-forward "^\\(-+\\)[^-]" nil t)
	      (setq header-delim (match-string-no-properties 1)))
	    (goto-char (point-max))
	    (forward-char -1)
	    (while (looking-at "\n")
	      (delete-char 1)
	      (goto-char (point-max))
	      (forward-char -1))
	    (write-file out-file))))
	(org-table-import out-file (if (string= engine "sqsh") '(4) '(16)))
	(org-babel-reassemble-table
	 (mapcar (lambda (x)
		   (if (string= (car x) header-delim)
		       'hline
		     x))
		 (org-table-to-lisp))
	 (org-babel-pick-name (cdr (assq :colname-names params))
			      (cdr (assq :colnames params)))
	 (org-babel-pick-name (cdr (assq :rowname-names params))
			      (cdr (assq :rownames params))))))))

(defun org-babel-sql-expand-vars (body vars)
  "Expand the variables held in VARS in BODY."
  (mapc
   (lambda (pair)
     (setq body
	   (replace-regexp-in-string
	    (format "$%s" (car pair))
	    (let ((val (cdr pair)))
              (if (listp val)
                  (let ((data-file (org-babel-temp-file "sql-data-")))
                    (with-temp-file data-file
                      (insert (orgtbl-to-csv
                               val '(:fmt (lambda (el) (if (stringp el)
                                                      el
                                                    (format "%S" el)))))))
                    data-file)
                (if (stringp val) val (format "%S" val))))
	    body)))
   vars)
  body)

(defun org-babel-prep-session:sql (_session _params)
  "Raise an error because Sql sessions aren't implemented."
  (error "SQL sessions not yet implemented"))

(provide 'ob-sql)



;;; ob-sql.el ends here
