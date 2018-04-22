;;; ob-makefile.el --- Babel Functions for Makefile  -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; 	   Thomas S. Dye
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

;; This file exists solely for tangling a Makefile from Org files.

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:makefile '())

(defun org-babel-execute:makefile (body _params)
  "Execute a block of makefile code.
This function is called by `org-babel-execute-src-block'."
  body)

(defun org-babel-prep-session:makefile (_session _params)
  "Return an error if the :session header argument is set.  Make
does not support sessions."
  (error "Makefile sessions are nonsensical"))

(provide 'ob-makefile)



;;; ob-makefile.el ends here
