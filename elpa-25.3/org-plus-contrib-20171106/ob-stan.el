;;; ob-stan.el --- Babel Functions for Stan          -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2017 Free Software Foundation, Inc.

;; Author: Kyle Meyer
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

;; Org-Babel support for evaluating Stan [1] source code.
;;
;; Evaluating a Stan block can produce two different results.
;;
;; 1) Dump the source code contents to a file.
;;
;;    This file can then be used as a variable in other blocks, which
;;    allows interfaces like RStan to use the model.
;;
;; 2) Compile the contents to a model file.
;;
;;    This provides access to the CmdStan interface.  To use this, set
;;    `org-babel-stan-cmdstan-directory' and provide a :file argument
;;    that does not end in ".stan".
;;
;; For more information and usage examples, visit
;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-stan.html
;;
;; [1] http://mc-stan.org/

;;; Code:
(require 'ob)
(require 'org-compat)

(defcustom org-babel-stan-cmdstan-directory nil
  "CmdStan source directory.
'make' will be called from this directory to compile the Stan
block.  When nil, executing Stan blocks dumps the content to a
plain text file."
  :group 'org-babel
  :type 'string)

(defvar org-babel-default-header-args:stan
  '((:results . "file")))

(defun org-babel-execute:stan (body params)
  "Generate Stan file from BODY according to PARAMS.
A :file header argument must be given.  If
`org-babel-stan-cmdstan-directory' is non-nil and the file name
does not have a \".stan\" extension, save an intermediate
\".stan\" file and compile the block to the named file.
Otherwise, write the Stan code directly to the named file."
  (let ((file (expand-file-name
	       (or (cdr (assq :file params))
		   (user-error "Set :file argument to execute Stan blocks")))))
    (if (or (not org-babel-stan-cmdstan-directory)
	    (string-match-p "\\.stan\\'" file))
	(with-temp-file file (insert body))
      (with-temp-file (concat file ".stan") (insert body))
      (let ((default-directory org-babel-stan-cmdstan-directory))
	(call-process-shell-command (concat "make " file))))
    nil))		; Signal that output has been written to file.

(defun org-babel-prep-session:stan (_session _params)
  "Return an error because Stan does not support sessions."
  (user-error "Stan does not support sessions"))

(provide 'ob-stan)
;;; ob-stan.el ends here
