;;; ob-plantuml.el --- Babel Functions for Plantuml  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2017 Free Software Foundation, Inc.

;; Author: Zhang Weize
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

;; Org-Babel support for evaluating plantuml script.
;;
;; Inspired by Ian Yang's org-export-blocks-format-plantuml
;; http://www.emacswiki.org/emacs/org-export-blocks-format-plantuml.el

;;; Requirements:

;; plantuml     | http://plantuml.sourceforge.net/
;; plantuml.jar | `org-plantuml-jar-path' should point to the jar file

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:plantuml
  '((:results . "file") (:exports . "results"))
  "Default arguments for evaluating a plantuml source block.")

(defcustom org-plantuml-jar-path ""
  "Path to the plantuml.jar file."
  :group 'org-babel
  :version "24.1"
  :type 'string)

(defun org-babel-variable-assignments:plantuml (params)
  "Return a list of PlantUML statements assigning the block's variables.
PARAMS is a property list of source block parameters, which may
contain multiple entries for the key `:var'.  `:var' entries in PARAMS
are expected to be scalar variables."
  (mapcar
   (lambda (pair)
       (format "!define %s %s"
	       (car pair)
	       (replace-regexp-in-string "\"" "" (cdr pair))))
   (org-babel--get-vars params)))

(defun org-babel-plantuml-make-body (body params)
  "Return PlantUML input string.
BODY is the content of the source block and PARAMS is a property list
of source block parameters.  This function relies on the
`org-babel-expand-body:generic' function to extract `:var' entries
from PARAMS and on the `org-babel-variable-assignments:plantuml'
function to convert variables to PlantUML assignments."
  (concat
   "@startuml\n"
   (org-babel-expand-body:generic
    body params (org-babel-variable-assignments:plantuml params))
   "\n@enduml"))

(defun org-babel-execute:plantuml (body params)
  "Execute a block of plantuml code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((out-file (or (cdr (assq :file params))
		       (error "PlantUML requires a \":file\" header argument")))
	 (cmdline (cdr (assq :cmdline params)))
	 (in-file (org-babel-temp-file "plantuml-"))
	 (java (or (cdr (assq :java params)) ""))
	 (full-body (org-babel-plantuml-make-body body params))
	 (cmd (if (string= "" org-plantuml-jar-path)
		  (error "`org-plantuml-jar-path' is not set")
		(concat "java " java " -jar "
			(shell-quote-argument
			 (expand-file-name org-plantuml-jar-path))
			(if (string= (file-name-extension out-file) "png")
			    " -tpng" "")
			(if (string= (file-name-extension out-file) "svg")
			    " -tsvg" "")
			(if (string= (file-name-extension out-file) "eps")
			    " -teps" "")
			(if (string= (file-name-extension out-file) "pdf")
			    " -tpdf" "")
			(if (string= (file-name-extension out-file) "vdx")
			    " -tvdx" "")
			(if (string= (file-name-extension out-file) "xmi")
			    " -txmi" "")
			(if (string= (file-name-extension out-file) "scxml")
			    " -tscxml" "")
			(if (string= (file-name-extension out-file) "html")
			    " -thtml" "")
			(if (string= (file-name-extension out-file) "txt")
			    " -ttxt" "")
			(if (string= (file-name-extension out-file) "utxt")
			    " -utxt" "")
			" -p " cmdline " < "
			(org-babel-process-file-name in-file)
			" > "
			(org-babel-process-file-name out-file)))))
    (unless (file-exists-p org-plantuml-jar-path)
      (error "Could not find plantuml.jar at %s" org-plantuml-jar-path))
    (with-temp-file in-file (insert full-body))
    (message "%s" cmd) (org-babel-eval cmd "")
    nil)) ;; signal that output has already been written to file

(defun org-babel-prep-session:plantuml (_session _params)
  "Return an error because plantuml does not support sessions."
  (error "Plantuml does not support sessions"))

(provide 'ob-plantuml)



;;; ob-plantuml.el ends here
