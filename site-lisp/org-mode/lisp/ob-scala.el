;;; ob-scala.el --- org-babel functions for Scala evaluation

;; Copyright (C) 2012-2014 Free Software Foundation, Inc.

;; Author: Andrzej Lichnerowicz
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
;; Currently only supports the external execution.  No session support yet.

;;; Requirements:
;; - Scala language :: http://www.scala-lang.org/
;; - Scala major mode :: Can be installed from Scala sources
;;  https://github.com/scala/scala-dist/blob/master/tool-support/src/emacs/scala-mode.el

;;; Code:
(require 'ob)
(eval-when-compile (require 'cl))

(defvar org-babel-tangle-lang-exts) ;; Autoloaded
(add-to-list 'org-babel-tangle-lang-exts '("scala" . "scala"))
(defvar org-babel-default-header-args:scala '())
(defvar org-babel-scala-command "scala"
  "Name of the command to use for executing Scala code.")

(defun org-babel-execute:scala (body params)
  "Execute a block of Scala code with org-babel.  This function is
called by `org-babel-execute-src-block'"
  (message "executing Scala source code block")
  (let* ((processed-params (org-babel-process-params params))
         (session (org-babel-scala-initiate-session (nth 0 processed-params)))
         (vars (nth 1 processed-params))
         (result-params (nth 2 processed-params))
         (result-type (cdr (assoc :result-type params)))
         (full-body (org-babel-expand-body:generic
                     body params))
         (result (org-babel-scala-evaluate
                  session full-body result-type result-params)))

    (org-babel-reassemble-table
     result
     (org-babel-pick-name
      (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
     (org-babel-pick-name
      (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))


(defun org-babel-scala-table-or-string (results)
  "Convert RESULTS into an appropriate elisp value.
If RESULTS look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-script-escape results))


(defvar org-babel-scala-wrapper-method

"var str_result :String = null;

Console.withOut(new java.io.OutputStream() {def write(b: Int){
}}) {
  str_result = {
%s
  }.toString
}

print(str_result)
")


(defun org-babel-scala-evaluate
  (session body &optional result-type result-params)
  "Evaluate BODY in external Scala process.
If RESULT-TYPE equals 'output then return standard output as a string.
If RESULT-TYPE equals 'value then return the value of the last statement
in BODY as elisp."
  (when session (error "Sessions are not (yet) supported for Scala"))
  (case result-type
    (output
     (let ((src-file (org-babel-temp-file "scala-")))
       (progn (with-temp-file src-file (insert body))
              (org-babel-eval
               (concat org-babel-scala-command " " src-file) ""))))
    (value
     (let* ((src-file (org-babel-temp-file "scala-"))
            (wrapper (format org-babel-scala-wrapper-method body)))
       (with-temp-file src-file (insert wrapper))
       (let ((raw (org-babel-eval
                   (concat org-babel-scala-command " " src-file) "")))
         (org-babel-result-cond result-params
	   raw
           (org-babel-scala-table-or-string raw)))))))


(defun org-babel-prep-session:scala (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (error "Sessions are not (yet) supported for Scala"))

(defun org-babel-scala-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session.  Sessions are not
supported in Scala."
  nil)

(provide 'ob-scala)



;;; ob-scala.el ends here
