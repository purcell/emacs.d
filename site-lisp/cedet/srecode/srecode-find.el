;;;; srecode-find.el --- Tools for finding templates in the database.

;; Copyright (C) 2007 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Various routines that search through various template tables
;; in search of the right template.

(require 'srecode-ctxt)

;;; Code:

;;; Basics
;;

;;;###autoload
(defun srecode-table ()
  "Return the currently active Semantic Recoder table for this buffer."
  ;; @todo - Do a better tree for this.
  (or (srecode-get-mode-table major-mode)
      (srecode-get-mode-table 'default)))

;;; TRACKER
;;
;; Template file tracker for between sessions.
;;
(defcustom srecode-template-file-alist
  '( ( default . "default.srt" )
     ( srecode-template-mode . "srecode-template.srt" )
     ( c++-mode . "srecode-cpp.srt" )
     ( emacs-lisp-mode . "srecode-el.srt" )
     ( wisent-grammar-mode . "srecode-wisent.srt" )
    )
  ;; @todo - Make this variable auto-generated from the Makefile.
  "List of template files and location associated with a given major mode."
  :group 'srecode
  :type '(repeat (cons (sexp :tag "Mode")
		       (sexp :tag "Filename"))
		 ))

(defcustom srecode-user-template-directory "~/.srecode/"
  "Directory where user templates are stored."
  :group 'srecode
  :type 'file)

;;;###autoload
(defun srecode-load-tables-for-mode (mmode &optional alist)
  "Load all the template files for MMODE.
Templates are found on the Emacs Lisp path as follows:
  <load-path>/templates/*.srt
  ~/.srecode/*.srt
If ALIST is provided, then use ALIST instead of
`srecode-template-file-alist'."
  (let ((search-list (or alist srecode-template-file-alist)))
    ;; Don't recurse if we are already the 'default state.
    (when (not (eq mmode 'default))
      ;; Are we a derived mode?  If so, get the parent mode's
      ;; templates loaded too.
      (if (get-mode-local-parent mmode)
	  (srecode-load-tables-for-mode (get-mode-local-parent mmode)
					search-list)
	;; No parent mode, all templates depend on the defaults being
	;; loaded in, so get that in instead.
	(srecode-load-tables-for-mode 'default search-list)))

    ;; Load in templates for our major mode.
    ;; @todo More than one template file??  User files??
    (let* ((mma (assoc mmode search-list))
	   (fname (cdr-safe mma))
	   (mt (srecode-get-mode-table mmode))
	   )
      (when fname
	;; Don't reload tables.
	(let ((actualfname
	       (or (locate-library fname t)
		   (locate-library (concat "templates/" fname))
		   )))
	  (when (and actualfname
		     (or (not mt)
			 (not (srecode-mode-table-find mt actualfname))))
	    (srecode-compile-file (locate-library actualfname t)))))
      )
    
    ;; @todo Once we've loaded in the shipped files, load in user files.
    ))

;;; SEARCH
;;
;; Find a given template based on name, and features of the current
;; buffer.
(defmethod srecode-template-get-table ((tab srecode-template-table)
				       template-name &optional context)
  "Find in the template in table TAB, the template with TEMPLATE-NAME.
Optional argument CONTEXT specifies that the template should part
of a particular context."
  (if context
      ;; If a context is specified, then look it up there.
      (let ((ctxth (gethash context (oref tab contexthash))))
	(when ctxth
	  (gethash template-name ctxth)))
    ;; No context, perhaps a merged name?
    (gethash template-name (oref tab namehash))))

;;;###autoload
(defmethod srecode-template-get-table ((tab srecode-mode-table)
				       template-name &optional
				       context application)
  "Find in the template in mode table TAB, the template with TEMPLATE-NAME.
Optional argument CONTEXT specifies a context a particular template
would belong to.
Optional argument APPLICATION restricts searches to only template tables
belonging to a specific application.  If APPLICATION is nil, then only
tables that do not belong to an application will be searched."
  (let* ((mt tab)
	 (tabs (oref mt :tables))
	 (ans nil))
    (while (and (not ans) tabs)
      (let ((app (oref (car tabs) :application)))
	(when (or (and (not application) (null app))
		  (and application (eq app application)))
	  (setq ans (srecode-template-get-table (car tabs) template-name
						context)))
	(setq tabs (cdr tabs))))
    (or ans
	;; Recurse to the default.
	(when (not (equal (oref tab :major-mode) 'default))
	  (srecode-template-get-table (srecode-get-mode-table 'default)
				      template-name context)))))

;;; Interactive
;;
;; Interactive queries into the template data.
;;
(defvar srecode-read-template-name-history nil
  "History for completing reads for template names.")

(defun srecode-all-template-hash (&optional mode hash)
  "Create a hash table of all the currently available templates.
Optional argument MODE is the major mode to look for.
Optional argument HASH is the hash table to fill in."
  (let* ((mhash (or hash (make-hash-table :test 'equal)))
	 (mmode (or mode major-mode))
	 (mp (get-mode-local-parent mmode))
	 )
    ;; Get the parent hash table filled into our current hash.
    (when (not (eq mode 'default))
      (if mp
	  (srecode-all-template-hash mp mhash)
	(srecode-all-template-hash 'default mhash)))
    ;; Load up the hash table for our current mode.
    (let* ((mt (srecode-get-mode-table mmode))
	   (tabs (when mt (oref mt :tables)))
	   )
      (while tabs
	;; Exclude templates for a perticular application.
	(when (not (oref (car tabs) :application))
	  (maphash (lambda (key temp)
		     (puthash key temp mhash)
		     )
		   (oref (car tabs) namehash)))
	(setq tabs (cdr tabs)))
      mhash)))

(defun srecode-calculate-default-template (hash)
  "Calculate the name of the template to use as a DEFAULT.
Templates are read from HASH.
Context into which the template is inserted is calculated
with `srecode-calculate-context'."
  (let* ((ctxt (srecode-calculate-context))
	 (ans (concat (nth 0 ctxt) ":" (nth 1 ctxt))))
    (when (gethash ans hash)
      ans)))

;;;###autoload
(defun srecode-read-template-name (prompt)
  "Completing read for Semantic Recoder template names.
PROMPT is used to query for the name of the template desired."
  (srecode-load-tables-for-mode major-mode)
  (let* ((hash (srecode-all-template-hash))
	 (def (srecode-calculate-default-template hash)))
    (completing-read prompt hash
		     nil t def
		     'srecode-read-template-name-history)))



(provide 'srecode-find)

;;; srecode-find.el ends here
