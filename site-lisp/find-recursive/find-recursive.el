;; find-recursive.el -- Find files recursively into a directory
;;
;; Copyright (C) 2001 Ovidiu Predescu 
;; 
;; Author: Ovidiu Predescu <ovidiu@cup.hp.com>
;; Date: March 26, 2001
;;
;; This program is free software; you can redistribute it and/or 
;; modify it under the terms of the GNU General Public License 
;; as published by the Free Software Foundation; either version 2 
;; of the License, or (at your option) any later version. 
;;  
;; This program is distributed in the hope that it will be useful, 
;; but WITHOUT ANY WARRANTY; without even the implied warranty of 
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
;; GNU General Public License for more details. 
;;  
;; You should have received a copy of the GNU General Public License 
;; along with this program; if not, write to the Free Software 
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. 
 
;; 
;; Setup: put this file in your Lisp path and add the following line in 
;; your .emacs: 
;; 
;; (require 'find-recursive) 
;; 

(require 'cl)

(defcustom find-recursive-exclude-files '(".*.class$" ".*~$" ".*.elc$")
  "List of regular expressions of files to be excluded when recursively searching for files."
  :type '(repeat (string :tag "File regexp")))

(defun find-file-recursively (file-regexp directory)
  (interactive "sFile name to search for recursively: \nDIn directory: ")
  (let ((directory (if (equal (substring directory -1) "/")
		       directory
		     (concat directory "/")))
	(matches
	 (find-recursive-filter-out
	  find-recursive-exclude-files
	  (find-recursive-directory-relative-files directory "" file-regexp))))
    (cond ((eq (length matches) 0) (message "No file(s) found!"))
	   ((eq (length matches) 1)
	    (find-file (concat directory (car matches))))
	   (t
	    (run-with-timer 0.001 nil
			    (lambda ()
			      (dispatch-event
			       (make-event 'key-press '(key tab)))))
	    (let ((file (completing-read "Choose file: "
					   (mapcar 'list matches)
					   nil t)))
		(if (or (eq file nil) (equal file ""))
		    (message "No file selected.")
		  (find-file (concat directory file))))))))

(defun find-recursive-directory-relative-files (directory
					  relative-directory
					  file-regexp)
  (let* ((full-dir (concat directory "/" relative-directory))
	 (matches
	  (mapcar
	   (function (lambda (x)
		       (concat relative-directory x)))
	   (find-recursive-filter-out '(nil)
				(directory-files full-dir nil
						 file-regexp nil t))))
	 (inner
	  (mapcar
	   (function
	    (lambda (dir)
	      (find-recursive-directory-relative-files directory
						 (concat relative-directory
							 dir "/")
						 file-regexp)))
	   (find-recursive-filter-out '(nil "\\." "\\.\\.")
				(directory-files full-dir nil ".*"
						 nil 'directories)))))
    (mapcar (function (lambda (dir) (setq matches (append matches dir))))
	    inner)
    matches))

(defun find-recursive-filter-out (remove-list list)
  "Remove all the elements in *remove-list* from *list*"
  (if (eq list nil)
      nil
    (let ((elem (car list))
	  (rest (cdr list)))
      (if (some
	   (lambda (regexp)
	     (if (or (eq elem nil) (eq regexp nil))
		 nil
	       (not (eq (string-match regexp elem) nil))))
	   remove-list)
	  (find-recursive-filter-out remove-list rest)
	(cons elem (find-recursive-filter-out remove-list rest))))))

(defvar find-recursive-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

(if find-recursive-running-xemacs
    nil
  (defadvice directory-files (after
			      directory-files-xemacs
			      (dirname &optional full match nosort files-only)
			      activate)
    "Add an additional argument, FILES-ONLY to the list of arguments
for GNU Emacs. If the symbol is t, then only the files in the
directory will be returned. If FILES-ONLY is nil, then both files and
directories are selected. If FILES-ONLY is not nil and not t, then
only sundirectories are returned."
    (setq ad-return-value
	  (cond ((null files-only) ad-return-value)
		((eq files-only t)
		 (find-recursive-remove-if (lambda (f)
					     (file-directory-p
					      (concat dirname "/" f)))
					   ad-return-value))
		(t
		 (find-recursive-remove-if (lambda (f)
					     (not (file-directory-p
						   (concat dirname "/" f))))
					   ad-return-value)))))

  (defun find-recursive-remove-if (func list)
    "Removes all elements satisfying FUNC from LIST."
    (let ((result nil))
      (while list
	(if (not (funcall func (car list)))
	    (setq result (cons (car list) result)))
	(setq list (cdr list)))
      (nreverse result))))

(global-set-key [(control x) (meta f)] 'find-file-recursively)

(provide 'find-recursive)
