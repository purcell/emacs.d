;;; ob-java.el --- Babel Functions for Java          -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2017 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Currently this only supports the external compilation and execution
;; of java code blocks (i.e., no session support).

;;; Code:
(require 'ob)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("java" . "java"))

(defcustom org-babel-java-command "java"
  "Name of the java command.
May be either a command in the path, like java
or an absolute path name, like /usr/local/bin/java
parameters may be used, like java -verbose"
  :group 'org-babel
  :version "24.3"
  :type 'string)

(defcustom org-babel-java-compiler "javac"
  "Name of the java compiler.
May be either a command in the path, like javac
or an absolute path name, like /usr/local/bin/javac
parameters may be used, like javac -verbose"
  :group 'org-babel
  :version "24.3"
  :type 'string)

(defun org-babel-execute:java (body params)
  (let* ((classname (or (cdr (assq :classname params))
			(error
			 "Can't compile a java block without a classname")))
	 (packagename (file-name-directory classname))
	 (src-file (concat classname ".java"))
	 (cmpflag (or (cdr (assq :cmpflag params)) ""))
	 (cmdline (or (cdr (assq :cmdline params)) ""))
	 (full-body (org-babel-expand-body:generic body params)))
    (with-temp-file src-file (insert full-body))
    (org-babel-eval
     (concat org-babel-java-compiler " " cmpflag " " src-file) "")
    ;; created package-name directories if missing
    (unless (or (not packagename) (file-exists-p packagename))
      (make-directory packagename 'parents))
    (let ((results (org-babel-eval (concat org-babel-java-command
                                           " " cmdline " " classname) "")))
      (org-babel-reassemble-table
       (org-babel-result-cond (cdr (assq :result-params params))
	 (org-babel-read results)
         (let ((tmp-file (org-babel-temp-file "c-")))
           (with-temp-file tmp-file (insert results))
           (org-babel-import-elisp-from-file tmp-file)))
       (org-babel-pick-name
        (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
       (org-babel-pick-name
        (cdr (assq :rowname-names params)) (cdr (assq :rownames params)))))))

(provide 'ob-java)



;;; ob-java.el ends here
