;;; ob-groovy.el --- Babel Functions for Groovy      -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2017 Free Software Foundation, Inc.

;; Author: Miro Bezjak
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
;; Currently only supports the external execution.  No session support yet.

;;; Requirements:
;; - Groovy language :: http://groovy.codehaus.org
;; - Groovy major mode :: Can be installed from MELPA or
;;   https://github.com/russel/Emacs-Groovy-Mode

;;; Code:
(require 'ob)

(defvar org-babel-tangle-lang-exts) ;; Autoloaded
(add-to-list 'org-babel-tangle-lang-exts '("groovy" . "groovy"))
(defvar org-babel-default-header-args:groovy '())
(defcustom org-babel-groovy-command "groovy"
  "Name of the command to use for executing Groovy code.
May be either a command in the path, like groovy
or an absolute path name, like /usr/local/bin/groovy
parameters may be used, like groovy -v"
  :group 'org-babel
  :version "24.3"
  :type 'string)

(defun org-babel-execute:groovy (body params)
  "Execute a block of Groovy code with org-babel.  This function is
called by `org-babel-execute-src-block'"
  (message "executing Groovy source code block")
  (let* ((processed-params (org-babel-process-params params))
         (session (org-babel-groovy-initiate-session (nth 0 processed-params)))
         (result-params (nth 2 processed-params))
         (result-type (cdr (assq :result-type params)))
         (full-body (org-babel-expand-body:generic
                     body params))
         (result (org-babel-groovy-evaluate
                  session full-body result-type result-params)))

    (org-babel-reassemble-table
     result
     (org-babel-pick-name
      (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
     (org-babel-pick-name
      (cdr (assq :rowname-names params)) (cdr (assq :rownames params))))))

(defvar org-babel-groovy-wrapper-method

  "class Runner extends Script {
    def out = new PrintWriter(new ByteArrayOutputStream())
    def run() { %s }
}

println(new Runner().run())
")


(defun org-babel-groovy-evaluate
    (session body &optional result-type result-params)
  "Evaluate BODY in external Groovy process.
If RESULT-TYPE equals `output' then return standard output as a string.
If RESULT-TYPE equals `value' then return the value of the last statement
in BODY as elisp."
  (when session (error "Sessions are not (yet) supported for Groovy"))
  (pcase result-type
    (`output
     (let ((src-file (org-babel-temp-file "groovy-")))
       (progn (with-temp-file src-file (insert body))
              (org-babel-eval
               (concat org-babel-groovy-command " " src-file) ""))))
    (`value
     (let* ((src-file (org-babel-temp-file "groovy-"))
            (wrapper (format org-babel-groovy-wrapper-method body)))
       (with-temp-file src-file (insert wrapper))
       (let ((raw (org-babel-eval
                   (concat org-babel-groovy-command " " src-file) "")))
         (org-babel-result-cond result-params
	   raw
           (org-babel-script-escape raw)))))))


(defun org-babel-prep-session:groovy (_session _params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (error "Sessions are not (yet) supported for Groovy"))

(defun org-babel-groovy-initiate-session (&optional _session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session.  Sessions are not
supported in Groovy."
  nil)

(provide 'ob-groovy)



;;; ob-groovy.el ends here
