;;; w3m-dtree.el --- The add-on program to display local directory tree.

;; Copyright (C) 2001, 2002, 2003, 2005, 2006, 2007, 2009
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: Hideyuki SHIRAI    <shirai@meadowy.org>,
;;         TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: w3m, WWW, hypermedia, directory, tree

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;; w3m-dtree.el is the add-on program of emacs-w3m to display local
;; directory tree.  For more detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/


;;; Code:
(require 'w3m)

(defcustom w3m-dtree-default-allfiles nil
  "*If non-nil, set 'allfiles' to default."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-dtree-directory-depth 8
  "*Interger of a depth of the viewing directory."
  :group 'w3m
  :type '(choice
	  (const :tag "No limit" nil)
	  (integer :format "%t: %v\n" :size 0 :tag "depth" 10)))

(defcustom w3m-dtree-indent-strings ["|-" "+-" "|  " "   "]
  "*Vector of strings to be used for indentation with w3m-dtree.

If use default value or choice 'ASCII', display like this,
/home/shirai/work/emacs-w3m/
 |-CVS/
 |-icons/
 |  +-CVS/
 +-shimbun/
    +-CVS/

If you care for another style, set manually and try it :-).
"
  :group 'w3m
  :type '(radio
	  (const :format "ASCII: " ["|-" "+-" "|  " "   "])
	  (vector
	   :convert-widget w3m-widget-type-convert-widget
	   (let ((defaults (if (equal w3m-language "Japanese")
			       (vconcat
				(mapcar
				 (lambda (s)
				   (decode-coding-string s 'iso-2022-7bit))
				 '("\e$B('\e(B" "\e$B(&\e(B"
				   "\e$B(\"\e(B" "\e$B!!\e(B")))
			     ["|-" "+-" "|  " "   "])))
	     `(:format "Others:\n%v" :indent 4
		       (string :format "%{|-%}          %v\n"
			       :sample-face widget-field-face :size 0
			       :value ,(aref defaults 0))
		       (string :format "%{+-%}          %v\n"
			       :sample-face widget-field-face :size 0
			       :value ,(aref defaults 1))
		       (string :format "%{|  %}         %v\n"
			       :sample-face widget-field-face :size 0
			       :value ,(aref defaults 2))
		       (string :format "%{   %}         %v"
			       :sample-face widget-field-face :size 0
			       :value ,(aref defaults 3)))))))

(defcustom w3m-dtree-stop-strings ["|=" "+="]
  "*Vector of strings to be used for indentation when a depth of directory
over the 'w3m-dtree-directory-depth'."
  :group 'w3m
  :type '(radio
	  (const :format "ASCII: " ["|=" "+="])
	  (const :format "ASCII Bold: " ["<b>|-</b>" "<b>+-</b>"])
	  (vector
	   :convert-widget w3m-widget-type-convert-widget
	   (let ((defaults (if (equal w3m-language "Japanese")
			       (vconcat
				(mapcar
				 (lambda (s)
				   (decode-coding-string s 'iso-2022-7bit))
				 '("\e$B(<\e(B" "\e$B(1\e(B")))
			     ["|=" "+="])))
	     `(:format "Others:\n%v" :indent 4
		       (string :format "|=          %{|=%}              %v\n"
			       :sample-face bold :size 0
			       :value ,(aref defaults 0))
		       (string :format "+=          %{+=%}              %v\n"
			       :sample-face bold :size 0
			       :value ,(aref defaults 1)))))))

(defun w3m-dtree-expand-file-name (path)
  (if (string-match "^\\(.\\):\\(.*\\)" path)
      (if w3m-use-cygdrive
	  (concat "/cygdrive/"
		  (match-string 1 path) (match-string 2 path))
	(concat "/" (match-string 1 path) "|" (match-string 2 path)))
    path))

(defun w3m-dtree-directory-name (path)
  (when (and w3m-treat-drive-letter
	     (string-match
	      "^/\\(?:\\([A-Za-z]\\)[|:]?\\|cygdrive/\\([A-Za-z]\\)\\)/"
	      path))
    (setq path (concat
		(or (match-string 1 path)
		    (match-string 2 path))
		":/"
		(substring path (match-end 0)))))
  path)

(defmacro w3m-dtree-has-child (path)
  `(let ((w32-get-true-file-link-count t)) ;; true link count for Meadow
     (and (nth 1 (file-attributes ,path))
	  (/= (nth 1 (file-attributes ,path)) 2))))

(defun w3m-dtree-create-sub (path allfiles dirprefix fileprefix indent depth)
  (let* ((files (directory-files path t))
	 (limit (and (integerp w3m-dtree-directory-depth)
		     (>= depth w3m-dtree-directory-depth)))
	 (indent-sub1 (if limit
			  (aref w3m-dtree-stop-strings 0)
			(aref w3m-dtree-indent-strings 0)))
	 (indent-sub2 (aref w3m-dtree-indent-strings 2))
	 file fullpath tmp)
    (setq files (delete (concat (file-name-as-directory path) ".")
			(delete (concat (file-name-as-directory path) "..")
				files)))
    (unless allfiles
      (setq tmp files)
      (while (setq file (car tmp))
	(unless (file-directory-p file)
	  (setq files (delete file files)))
	(setq tmp (cdr tmp))))
    (while (setq fullpath (car files))
      (when (= (length files) 1)
	(if limit
	    (setq indent-sub1 (aref w3m-dtree-stop-strings 1))
	  (setq indent-sub1 (aref w3m-dtree-indent-strings 1)))
	(setq indent-sub2 (aref w3m-dtree-indent-strings 3)))
      (setq file (file-name-nondirectory fullpath))
      (cond
       ((or (not allfiles) (file-directory-p fullpath))
	(insert (format "%s%s%s<A HREF=\"%s%s\">%s</A>\n"
			indent indent-sub1
			(if allfiles "<B>[d]</B>" "")
			dirprefix
			(w3m-dtree-expand-file-name (file-name-as-directory fullpath))
			(concat file "/")))
	(when (and (null limit)
		   (or allfiles (w3m-dtree-has-child fullpath)))
	  (w3m-dtree-create-sub fullpath allfiles dirprefix fileprefix
				(concat indent indent-sub2) (1+ depth))))
       ((and allfiles (file-exists-p fullpath))
	(insert (format "%s%s%s<A HREF=\"%s%s\">%s</A>\n"
			indent indent-sub1
			(if allfiles "(f)" "")
			fileprefix (w3m-dtree-expand-file-name fullpath)
			file))))
      (setq files (cdr files)))))

(defun w3m-dtree-create (path allfiles dirprefix fileprefix)
  (let ((charset (or (car (rassq w3m-file-name-coding-system
				 w3m-charset-coding-system-alist))
		     w3m-file-name-coding-system)))
    (insert "<!doctype html public \"-//W3C//DTD HTML 3.2//EN\">\n"
	    "<html>\n<head>\n"
	    "<meta http-equiv=\"CONTENT-TYPE\" "
	    "content=\"text/html; charset="
	    (symbol-name charset)
	    "\">\n"
	    "<title>"
	    path
	    "</title>\n</head>\n<body>\n<pre>\n")
    (insert (format "<A HREF=\"%s%s\">%s</A>%s\n"
		    dirprefix (w3m-dtree-expand-file-name path) path
		    (if allfiles " (allfiles)" "")))
    (if (file-directory-p path)
	(w3m-dtree-create-sub path allfiles dirprefix fileprefix " " 0)
      (insert (format "\n<h3>Warning: Directory not found.</h3>\n")))
    (insert "</pre>\n</body>\n</html>\n")))

;;;###autoload
(defun w3m-about-dtree (url &optional nodecode allfiles &rest args)
  (let ((prelen (length "about://dtree"))
	(dirprefix "about://dtree")
	(fileprefix "file://")
	path)
    (if (string-match "\\?allfiles=\\(?:\\(true\\)\\|false\\)$" url)
	(progn
	  (setq path (substring url prelen (match-beginning 0)))
	  (if (match-beginning 1) (setq allfiles t)))
      (if w3m-dtree-default-allfiles
	  (setq allfiles (not allfiles)))
      (setq path (substring url prelen)))
    ;; counter drive letter
    (setq path (file-name-as-directory (w3m-dtree-directory-name path)))
    (setq default-directory path)
    (w3m-message "Dtree (%s)..." path)
    (w3m-dtree-create path allfiles dirprefix fileprefix)
    (w3m-message "Dtree...done")
    "text/html"))

;;;###autoload
(defun w3m-dtree (allfiles path)
  "Display directory tree on local file system.
If called with 'prefix argument', display all directorys and files."
  (interactive "P\nDDtree directory: ")
  (if w3m-dtree-default-allfiles
      (setq allfiles (not allfiles)))
  (w3m-goto-url (format "about://dtree%s%s"
			(w3m-dtree-expand-file-name
			 (file-name-as-directory
			  (expand-file-name path)))
			(if allfiles "?allfiles=true" ""))))

(provide 'w3m-dtree)
;;; w3m-dtree.el ends here
