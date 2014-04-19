;;; ob-ditaa.el --- org-babel functions for ditaa evaluation

;; Copyright (C) 2009-2014 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating ditaa source code.
;;
;; This differs from most standard languages in that
;;
;; 1) there is no such thing as a "session" in ditaa
;;
;; 2) we are generally only going to return results of type "file"
;;
;; 3) we are adding the "file" and "cmdline" header arguments
;;
;; 4) there are no variables (at least for now)

;;; Code:
(require 'ob)
(require 'org-compat)

(defvar org-babel-default-header-args:ditaa
  '((:results . "file")
    (:exports . "results")
    (:java . "-Dfile.encoding=UTF-8"))
  "Default arguments for evaluating a ditaa source block.")

(defcustom org-ditaa-jar-path (expand-file-name
			       "ditaa.jar"
			       (file-name-as-directory
				(expand-file-name
				 "scripts"
				 (file-name-as-directory
				  (expand-file-name
				   "../contrib"
				   (file-name-directory (org-find-library-dir "org")))))))
  "Path to the ditaa jar executable."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-ditaa-java-cmd "java"
  "Java executable to use when evaluating ditaa blocks."
  :group 'org-babel
  :type 'string)

(defcustom org-ditaa-eps-jar-path
  (expand-file-name "DitaaEps.jar" (file-name-directory org-ditaa-jar-path))
  "Path to the DitaaEps.jar executable."
  :group 'org-babel
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-ditaa-jar-option "-jar"
  "Option for the ditaa jar file.
Do not leave leading or trailing spaces in this string."
  :group 'org-babel
  :version "24.1"
  :type 'string)

(defun org-babel-execute:ditaa (body params)
  "Execute a block of Ditaa code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((result-params (split-string (or (cdr (assoc :results params)) "")))
	 (out-file (let ((el (cdr (assoc :file params))))
                     (or el
                         (error
                          "ditaa code block requires :file header argument"))))
	 (cmdline (cdr (assoc :cmdline params)))
	 (java (cdr (assoc :java params)))
	 (in-file (org-babel-temp-file "ditaa-"))
	 (eps (cdr (assoc :eps params)))
	 (cmd (concat org-babel-ditaa-java-cmd
		      " " java " " org-ditaa-jar-option " "
		      (shell-quote-argument
		       (expand-file-name
			(if eps org-ditaa-eps-jar-path org-ditaa-jar-path)))
		      " " cmdline
		      " " (org-babel-process-file-name in-file)
		      " " (org-babel-process-file-name out-file)))
	 (pdf-cmd (when (and (or (string= (file-name-extension out-file) "pdf")
				 (cdr (assoc :pdf params))))
		    (concat
		     "epstopdf"
		     " " (org-babel-process-file-name (concat in-file ".eps"))
		     " -o=" (org-babel-process-file-name out-file)))))
    (unless (file-exists-p org-ditaa-jar-path)
      (error "Could not find ditaa.jar at %s" org-ditaa-jar-path))
    (with-temp-file in-file (insert body))
    (message cmd) (shell-command cmd)
    (when pdf-cmd (message pdf-cmd) (shell-command pdf-cmd))
    nil)) ;; signal that output has already been written to file

(defun org-babel-prep-session:ditaa (session params)
  "Return an error because ditaa does not support sessions."
  (error "Ditaa does not support sessions"))

(provide 'ob-ditaa)



;;; ob-ditaa.el ends here
