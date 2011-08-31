;;; cedet-global.el --- GNU Global support for CEDET.

;; Copyright (C) 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cedet-global.el,v 1.6 2009/01/14 00:24:32 zappo Exp $

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
;; Basic support for calling GNU Global, and testing version numbers.

(require 'inversion)

(defvar cedet-global-min-version "5.0"
  "Minimum version of GNU global required.")

;;;###autoload
(defcustom cedet-global-command "global"
  "Command name for the GNU Global executable."
  :type 'string
  :group 'cedet)

;;; Code:
;;;###autoload
(defun cedet-gnu-global-search (searchtext texttype type scope)
  "Perform a search with GNU Global, return the created buffer.
SEARCHTEXT is text to find.
TEXTTYPE is the type of text, such as 'regexp, 'string, 'tagname,
'tagregexp, or 'tagcompletions.
TYPE is the type of search, meaning that SEARCHTEXT is compared to
filename, tagname (tags table), references (uses of a tag) , or
symbol (uses of something not in the tag table.)
SCOPE is the scope of the search, such as 'project or 'subdirs."
  (let ((flgs (cond ((eq type 'file)
		     "-a")
		    (t "-xa")))
	(scopeflgs (cond
		    ((eq scope 'project)
		     ""
		     )
		    ((eq scope 'target)
		     "l")))
	(stflag (cond ((or (eq texttype 'tagname)
			   (eq texttype 'tagregexp))
		       "")
		      ((eq texttype 'tagcompletions)
		       "c")
		      ((eq texttype 'regexp)
		       "g")
		      (t "r")))
	)
    (cedet-gnu-global-call (list (concat flgs scopeflgs stflag)
				 searchtext))))

(defun cedet-gnu-global-call (flags)
  "Call GNU Global with the list of FLAGS."
  (let ((b (get-buffer-create "*CEDET Global*"))
	(cd default-directory)
	)
    (save-excursion
      (set-buffer b)
      (setq default-directory cd)
      (erase-buffer))
    (apply 'call-process cedet-global-command
	   nil b nil
	   flags)
    b))

;;;###autoload
(defun cedet-gnu-global-expand-filename (filename)
  "Expand the FILENAME with GNU Global.
Return a fully qualified filename."
  (interactive "sFile: ")
  (let ((ans (save-excursion
	       (set-buffer (cedet-gnu-global-call (list "-Pa" filename)))
	       (goto-char (point-min))
	       (if (looking-at "global: ")
		   (error "GNU Global not available")
		 (split-string (buffer-string) "\n" t)))))
    (when (interactive-p)
      (if ans
	  (if (= (length ans) 1)
	      (message "%s" (car ans))
	    (message "%s + %d others" (car ans)
		     (length (cdr ans))))
	(error "No file found")))
    ans))

;;;###autoload
(defun cedet-gnu-global-show-root ()
  "Show the root of a GNU Global area under the current buffer."
  (interactive)
  (message "%s" (cedet-gnu-global-root)))

;;;###autoload
(defun cedet-gnu-global-root (&optional dir)
  "Return the root of any GNU Global scanned project.
If a default starting DIR is not specified, the current buffer's
`default-directory' is used."
  (let ((default-directory (or dir default-directory))
	)
    (save-excursion
      (set-buffer (cedet-gnu-global-call (list "-pq")))
      (goto-char (point-min))
      (when (not (eobp))
	(file-name-as-directory
	 (buffer-substring (point) (point-at-eol)))))))

;;;###autoload
(defun cedet-gnu-global-version-check (&optional noerror)
  "Check the version of the installed GNU Global command.
If optional programatic argument NOERROR is non-nil, then
instead of throwing an error if Global isn't available, then
return nil."
  (interactive)
  (let ((b (cedet-gnu-global-call (list "--version")))
	(rev nil))
    (save-excursion
      (set-buffer b)
      (goto-char (point-min))
      (re-search-forward "GNU GLOBAL \\([0-9.]+\\)" nil t)
      (setq rev (match-string 1))
      (if (inversion-check-version rev nil cedet-global-min-version)
	  (if noerror
	      nil
	    (error "Version of GNU Global is %s.  Need at least %s"
		   rev cedet-global-min-version))
	;; Else, return TRUE, as in good enough.
	(when (interactive-p)
	  (message "GNU Global %s  - Good enough for CEDET." rev))
	t))))

(defun cedet-gnu-global-scan-hits (buffer)
  "Scan all the hits from the GNU Global output BUFFER."
  (let ((hits nil)
	(r1 "^\\([^ ]+\\) +\\([0-9]+\\) \\([^ ]+\\) "))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (while (re-search-forward r1 nil t)
	(setq hits (cons (cons (string-to-number (match-string 2))
			       (match-string 3))
			 hits)))
      ;; Return the results
      (nreverse hits))))

(provide 'cedet-global)
;;; cedet-global.el ends here
