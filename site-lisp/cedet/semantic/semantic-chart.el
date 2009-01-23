;;; semantic-util.el --- Utilities for use with semantic tag tables

;;; Copyright (C) 1999, 2000, 2001, 2003, 2005, 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: chart
;; X-RCS: $Id: semantic-chart.el,v 1.14 2009/01/20 02:32:10 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; A set of simple functions for charting details about a file based on
;; the output of the semantic parser.
;;

(require 'semantic)
(require 'chart)

;;; Code:

;;;###autoload
(defun semantic-chart-tags-by-class (&optional tagtable)
  "Create a bar chart representing the number of tags for a given tag class.
Each bar represents how many toplevel tags in TAGTABLE
exist with a given class.  See `semantic-symbol->name-assoc-list'
for tokens which will be charted.
TAGTABLE is passedto `semantic-something-to-tag-table'."
  (interactive)
  (let* ((stream (semantic-something-to-tag-table
		  (or tagtable (current-buffer))))
	 (names (mapcar 'cdr semantic-symbol->name-assoc-list))
	 (nums (mapcar
		(lambda (symname)
		  (length
		   (semantic-brute-find-tag-by-class (car symname)
						     stream)
		   ))
		semantic-symbol->name-assoc-list)))
    (chart-bar-quickie 'vertical
		       "Semantic Toplevel Tag Volume"
		       names "Tag Class"
		       nums "Volume")
    ))

;;;###autoload
(defun semantic-chart-database-size (&optional tagtable)
  "Create a bar chart representing the size of each file in semanticdb.
Each bar represents how many toplevel tags in TAGTABLE
exist in each database entry.
TAGTABLE is passed to `semantic-something-to-tag-table'."
  (interactive)
  (if (or (not (fboundp 'semanticdb-minor-mode-p))
	  (not (semanticdb-minor-mode-p)))
      (error "Semanticdb is not enabled"))
  (let* ((db semanticdb-current-database)
	 (dbt (semanticdb-get-database-tables db))
	 (names (mapcar 'car
			(object-assoc-list
			 'file
			 dbt)))
	 (numnuts (mapcar (lambda (dba)
			    (prog1
				(cons
				 (if (slot-boundp dba 'tags)
				     (length (oref dba tags))
				   1)
				 (car names))
			      (setq names (cdr names))))
			  dbt))
	 (nums nil)
	 (fh (/ (- (frame-height) 7) 4)))
    (setq numnuts (sort numnuts (lambda (a b) (> (car a) (car b)))))
    (setq names (mapcar 'cdr numnuts)
	  nums (mapcar 'car numnuts))
    (if (> (length names) fh)
	(progn
	  (setcdr (nthcdr fh names) nil)
	  (setcdr (nthcdr fh nums) nil)))
    (chart-bar-quickie 'horizontal
		       "Semantic DB Toplevel Tag Volume"
		       names "File"
		       nums "Volume")
    ))

(defun semantic-chart-token-complexity (tok)
  "Calculate the `complexity' of token TOK."
  (count-lines
   (semantic-tag-end tok)
   (semantic-tag-start tok)))

;;;###autoload
(defun semantic-chart-tag-complexity
  (&optional class tagtable)
  "Create a bar chart representing the complexity of some tags.
Complexity is calculated for tags of CLASS.  Each bar represents
the complexity of some tag in TAGTABLE.  Only the most complex
items are charted.  TAGTABLE is passedto
`semantic-something-to-tag-table'."
  (interactive)
  (let* ((sym (if (not class) 'function))
	 (stream
	  (semantic-find-tags-by-class
	   sym (semantic-something-to-tag-table (or tagtable
						    (current-buffer)))
	   ))
	 (name (cond ((semantic-tag-with-position-p (car stream))
		      (buffer-name (semantic-tag-buffer (car stream))))
		     (t "")))
	 (cplx (mapcar (lambda (tok)
			 (cons tok (semantic-chart-token-complexity tok)))
		       stream))
	 (namelabel (cdr (assoc 'function semantic-symbol->name-assoc-list)))
	 (names nil)
	 (nums nil))
    (setq cplx (sort cplx (lambda (a b) (> (cdr a) (cdr b)))))
    (while (and cplx (<= (length names) (/ (- (frame-height) 7) 4)))
      (setq names (cons (semantic-tag-name (car (car cplx)))
			names)
	    nums (cons (cdr (car cplx)) nums)
	    cplx (cdr cplx)))
;; ;;     (setq names (mapcar (lambda (str)
;; ;; 			  (substring str (- (length str) 10)))
;; ;; 			names))
    (chart-bar-quickie 'horizontal
		       (format "%s Complexity in %s"
			       (capitalize (symbol-name sym))
			       name)
		       names namelabel
		       nums "Complexity (Lines of code)")
    ))

;;;###autoload
(defun semantic-chart-analyzer ()
  "Chart the extent of the context analysis."
  (interactive)
  (let* ((p (semanticdb-find-translate-path nil nil))
	 (plen (length p))
	 (tab semanticdb-current-table)
	 (tc (semanticdb-get-typecache tab))
	 (tclen (+ (length (oref tc filestream))
		   (length (oref tc includestream))))
	 (scope (semantic-calculate-scope))
	 (fslen (length (oref scope fullscope)))
	 (lvarlen (length (oref scope localvar)))
	 )
    (chart-bar-quickie 'vertical
		       (format "Analyzer Overhead in %s" (buffer-name))
		       '("includes" "typecache" "scopelen" "localvar")
		       "Overhead Entries"
		       (list plen tclen fslen lvarlen)
		       "Number of tags")
    ))
	 


(provide 'semantic-chart)

;;; semantic-chart.el ends here
