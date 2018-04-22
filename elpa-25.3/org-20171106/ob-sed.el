;;; ob-sed.el --- Babel Functions for Sed Scripts    -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2017 Free Software Foundation, Inc.

;; Author: Bjarte Johansen
;; Keywords: literate programming, reproducible research
;; Version: 0.1.0

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

;; Provides a way to evaluate sed scripts in Org mode.

;;; Usage:

;; Add to your Emacs config:

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((sed . t)))

;; In addition to the normal header arguments, ob-sed also provides
;; :cmd-line and :in-file. :cmd-line allows one to pass other flags to
;; the sed command like the "--in-place" flag which makes sed edit the
;; file pass to it instead of outputting to standard out or to a
;; different file. :in-file is a header arguments that allows one to
;; tell Org Babel which file the sed script to act on.

;;; Code:
(require 'ob)

(defvar org-babel-sed-command "sed"
  "Name of the sed executable command.")

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("sed" . "sed"))

(defconst org-babel-header-args:sed
  '((:cmd-line . :any)
    (:in-file  . :any))
  "Sed specific header arguments.")

(defvar org-babel-default-header-args:sed '()
  "Default arguments for evaluating a sed source block.")

(defun org-babel-execute:sed (body params)
  "Execute a block of sed code with Org Babel.
BODY is the source inside a sed source block and PARAMS is an
association list over the source block configurations.  This
function is called by `org-babel-execute-src-block'."
  (message "executing sed source code block")
  (let* ((result-params (cdr (assq :result-params params)))
         (cmd-line (cdr (assq :cmd-line params)))
         (in-file (cdr (assq :in-file params)))
	 (code-file (let ((file (org-babel-temp-file "sed-")))
                      (with-temp-file file
			(insert body)) file))
	 (stdin (let ((stdin (cdr (assq :stdin params))))
		   (when stdin
		     (let ((tmp (org-babel-temp-file "sed-stdin-"))
			   (res (org-babel-ref-resolve stdin)))
		       (with-temp-file tmp
			 (insert res))
		       tmp))))
         (cmd (mapconcat #'identity
			 (remq nil
			       (list org-babel-sed-command
				     (format "--file=\"%s\"" code-file)
				     cmd-line
				     in-file))
			 " ")))
    (org-babel-reassemble-table
     (let ((results
            (cond
             (stdin (with-temp-buffer
                      (call-process-shell-command cmd stdin (current-buffer))
                      (buffer-string)))
             (t (org-babel-eval cmd "")))))
       (when results
         (org-babel-result-cond result-params
	   results
	   (let ((tmp (org-babel-temp-file "sed-results-")))
	     (with-temp-file tmp (insert results))
	     (org-babel-import-elisp-from-file tmp)))))
     (org-babel-pick-name
      (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
     (org-babel-pick-name
      (cdr (assq :rowname-names params)) (cdr (assq :rownames params))))))

(provide 'ob-sed)
;;; ob-sed.el ends here
