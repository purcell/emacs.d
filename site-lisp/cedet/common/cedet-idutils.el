;;; cedet-idutils.el --- ID Utils support for CEDET.
;;
;; Copyright (C) 2009 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cedet-idutils.el,v 1.1 2009/02/23 22:12:31 zappo Exp $
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
;; Basic support calling ID Utils functions, and checking version
;; numbers.

(require 'inversion)

(defvar cedet-idutils-min-version "4.0"
  "Minimum version of ID Utils required.")

;;;###autoload
(defcustom cedet-idutils-file-command "fnid"
  "Command name for the ID Utils executable for searching file names."
  :type 'string
  :group 'cedet)

;;;###autoload
(defcustom cedet-idutils-token-command "lid"
  "Command name for the ID Utils executable for searching for tokens."
  :type 'string
  :group 'cedet)

;;; Code:
(defun cedet-idutils-search (searchtext texttype type scope)
  "Perform a search with IDUtils, return the created buffer.
SEARCHTEXT is text to find.
TEXTTYPE is the type of text, such as 'regexp, 'string, 'tagname,
'tagregexp, or 'tagcompletions.
TYPE is the type of search, meaning that SEARCHTEXT is compared to
filename, tagname (tags table), references (uses of a tag) , or
symbol (uses of something not in the tag table.)
SCOPE is the scope of the search, such as 'project or 'subdirs.
Note: Scope is not yet supported."
  (if (eq type 'file)
      ;; Calls for file stuff is very simple.
      (cedet-idutils-fnid-call (list searchtext))
    ;; Calls for text searches is more complex.
    (let* ((resultflg (if (eq texttype 'tagcompletions)
			  (list "--key=token")
			(list "--result=grep")))
	   (scopeflgs nil) ; (cond ((eq scope 'project) "" ) ((eq scope 'target) "l")))
	   (stflag (cond ((or (eq texttype 'tagname)
			      (eq texttype 'tagregexp))
			  (list "-r" "-w"))
			 ((eq texttype 'tagcompletions)
			  ;; Add regex to search text for beginning of char.
			  (setq searchtext (concat "^" searchtext))
			  (list "-r" "-s" ))
			 ((eq texttype 'regexp)
			  (list "-r"))
			 ;; t means 'symbol
			 (t (list "-l" "-w"))))
	   )
      (cedet-idutils-lid-call (append resultflg scopeflgs stflag (list searchtext))))
    ))

(defun cedet-idutils-fnid-call (flags)
  "Call ID Utils fnid with the list of FLAGS.
Return the created buffer with with program output."
  (let ((b (get-buffer-create "*CEDET fnid*"))
	(cd default-directory)
	)
    (save-excursion
      (set-buffer b)
      (setq default-directory cd)
      (erase-buffer))
    (apply 'call-process cedet-idutils-file-command
	   nil b nil
	   flags)
    b))

(defun cedet-idutils-lid-call (flags)
  "Call ID Utils lid with the list of FLAGS.
Return the created buffer with with program output."
  (let ((b (get-buffer-create "*CEDET lid*"))
	(cd default-directory)
	)
    (save-excursion
      (set-buffer b)
      (setq default-directory cd)
      (erase-buffer))
    (apply 'call-process cedet-idutils-token-command
	   nil b nil
	   flags)
    b))

;;; UTIL CALLS
;;
;;;###autoload
(defun cedet-idutils-expand-filename (filename)
  "Expand the FILENAME with IDUtils.
Return a filename relative to the default directory."
  (interactive "sFile: ")
  (let ((ans (save-excursion
	       (set-buffer (cedet-idutils-fnid-call (list filename)))
	       (goto-char (point-min))
	       (if (looking-at "[^ \n]*fnid: ")
		   (error "ID Utils not available")
		 (split-string (buffer-string) "\n" t)))))
    (setq ans (mapcar 'expand-file-name ans))
    (when (interactive-p)
      (if ans
	  (if (= (length ans) 1)
	      (message "%s" (car ans))
	    (message "%s + %d others" (car ans)
		     (length (cdr ans))))
	(error "No file found")))
    ans))

(defun cedet-idutils-support-for-directory (&optional dir)
  "Return non-nil if IDUtils has a support file for DIR.
If DIR is not supplied, use the current default directory.
This works by running lid on a bogus symbol, and looking for
the error code."
  (save-excursion
    (let ((default-directory (or dir default-directory)))
      (set-buffer (cedet-idutils-fnid-call '("moose")))
      (goto-char (point-min))
      (if (looking-at "[^ \n]*fnid: ")
	  nil
	t))))

;;;###autoload
(defun cedet-idutils-version-check (&optional noerror)
  "Check the version of the installed ID Utils command.
If optional programatic argument NOERROR is non-nil, then
instead of throwing an error if Global isn't available, then
return nil."
  (interactive)
  (let ((b (cedet-idutils-fnid-call (list "--version")))
	(rev nil))
    (save-excursion
      (set-buffer b)
      (goto-char (point-min))
      (re-search-forward "fnid - \\([0-9.]+\\)" nil t)
      (setq rev (match-string 1))
      (if (inversion-check-version rev nil cedet-idutils-min-version)
	  (if noerror
	      nil
	    (error "Version of ID Utis is %s.  Need at least %s"
		   rev cedet-idutils-min-version))
	;; Else, return TRUE, as in good enough.
	(when (interactive-p)
	  (message "ID Utils %s  - Good enough for CEDET." rev))
	t))))


(provide 'cedet-idutils)
;;; cedet-idutils.el ends here

