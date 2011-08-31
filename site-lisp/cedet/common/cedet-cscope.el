;;; cedet-cscope.el --- CScope support for CEDET
;;
;; Copyright (C) 2009 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cedet-cscope.el,v 1.1 2009/02/27 04:53:12 zappo Exp $
;;
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
;; Support using CScope for symbol lookups.

(require 'inversion)

(defvar cedet-cscope-min-version "16.0"
  "Minimum version of GNU global required.")

;;;###autoload
(defcustom cedet-cscope-command "cscope"
  "Command name for the CScope executable."
  :type 'string
  :group 'cedet)

;;; Code:
;;;###autoload
(defun cedet-cscope-search (searchtext texttype type scope)
  "Perform a search with CScope, return the created buffer.
SEARCHTEXT is text to find.
TEXTTYPE is the type of text, such as 'regexp, 'string, 'tagname,
'tagregexp, or 'tagcompletions.
TYPE is the type of search, meaning that SEARCHTEXT is compared to
filename, tagname (tags table), references (uses of a tag) , or
symbol (uses of something not in the tag table.)
SCOPE is the scope of the search, such as 'project or 'subdirs."
  ;; CScope is an interactive program.  It uses number flags
  ;; in order to perform command line searches.  Useful for this
  ;; tool are:
  ;;
  ;; -0 = Find C symbol
  ;; -1 = Find global definition
  ;; -3 = Find references
  ;; -6 = Find egrep pattern
  ;; -7 = Find file
  (let ((idx (cond ((eq type 'file)
		    "-7")
		   ;; Non files are symbols and such
		   ((eq texttype 'tagname)
		    "-1")
		   ((eq texttype 'tagregexp)
		    "-0")
		   ((eq texttype 'tagcompletions)
		    (setq searchtext (concat "^" searchtext ".*"))
		    "-1")
		   ((eq texttype 'regexp)
		    "-5")
		   (t
		    "-3")
		   )
	     )
	)
    (cedet-cscope-call (list "-d" "-L" idx searchtext))))

(defun cedet-cscope-call (flags)
  "Call CScope with the list of FLAGS."
  (let ((b (get-buffer-create "*CEDET CScope*"))
	(cd default-directory)
	)
    (save-excursion
      (set-buffer b)
      (setq default-directory cd)
      (erase-buffer))
    (apply 'call-process cedet-cscope-command
	   nil b nil
	   flags)
    b))


;;;###autoload
(defun cedet-cscope-expand-filename (filename)
  "Expand the FILENAME with CScope.
Return a fully qualified filename."
  (interactive "sFile: ")
  (let* ((ans1 (save-excursion
		 (set-buffer (cedet-cscope-call (list "-d" "-L" "-7" filename)))
		 (goto-char (point-min))
		 (if (looking-at "[^ \n]*cscope: ")
		     (error "CScope not available")
		   (split-string (buffer-string) "\n" t))))
	 (ans2 (mapcar (lambda (hit)
			 (expand-file-name (car (split-string hit " "))))
		       ans1)))
    (when (interactive-p)
      (if ans2
	  (if (= (length ans2) 1)
	      (message "%s" (car ans2))
	    (message "%s + %d others" (car ans2)
		     (length (cdr ans2))))
	(error "No file found")))
    ans2))

(defun cedet-cscope-support-for-directory (&optional dir)
  "Return non-nil if CScope has a support file for DIR.
If DIR is not supplied, use the current default directory.
This works by running cscope on a bogus symbol, and looking for
the error code."
  (save-excursion
    (let ((default-directory (or dir default-directory)))
      (set-buffer (cedet-cscope-call (list "-d" "-L" "-7" "moose")))
      (goto-char (point-min))
      (if (looking-at "[^ \n]*cscope: ")
	  nil
	t))))

;;;###autoload
(defun cedet-cscope-version-check (&optional noerror)
  "Check the version of the installed CScope command.
If optional programatic argument NOERROR is non-nil, then
instead of throwing an error if Global isn't available, then
return nil."
  (interactive)
  (let ((b (cedet-cscope-call (list "-V")))
	(rev nil))
    (save-excursion
      (set-buffer b)
      (goto-char (point-min))
      (re-search-forward "cscope: version \\([0-9.]+\\)" nil t)
      (setq rev (match-string 1))
      (if (inversion-check-version rev nil cedet-cscope-min-version)
	  (if noerror
	      nil
	    (error "Version of CScope is %s.  Need at least %s"
		   rev cedet-cscope-min-version))
	;; Else, return TRUE, as in good enough.
	(when (interactive-p)
	  (message "CScope %s  - Good enough for CEDET." rev))
	t))))

(provide 'cedet-cscope)
;;; cedet-cscope.el ends here
