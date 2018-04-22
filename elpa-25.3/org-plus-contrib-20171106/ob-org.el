;;; ob-org.el --- Babel Functions for Org Code Blocks -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2017 Free Software Foundation, Inc.

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

;; This is the simplest of code blocks, where upon evaluation the
;; contents of the code block are returned in a raw result.

;;; Code:
(require 'ob)

(declare-function org-export-string-as "ox"
		  (string backend &optional body-only ext-plist))

(defvar org-babel-default-header-args:org
  '((:results . "raw silent") (:exports . "code"))
  "Default arguments for evaluating a org source block.")

(defvar org-babel-org-default-header
  "#+TITLE: default empty header\n"
  "Default header inserted during export of org blocks.")

(defun org-babel-expand-body:org (body params)
  (dolist (var (org-babel--get-vars params))
    (setq body (replace-regexp-in-string
		(regexp-quote (format "$%s" (car var)))
		(format "%s" (cdr var))
		body nil 'literal)))
  body)

(defun org-babel-execute:org (body params)
  "Execute a block of Org code with.
This function is called by `org-babel-execute-src-block'."
  (let ((result-params (split-string (or (cdr (assq :results params)) "")))
	(body (org-babel-expand-body:org
	       (replace-regexp-in-string "^," "" body) params)))
    (cond
     ((member "latex" result-params)
      (org-export-string-as (concat "#+Title: \n" body) 'latex t))
     ((member "html" result-params) (org-export-string-as  body 'html t))
     ((member "ascii" result-params) (org-export-string-as body 'ascii t))
     (t body))))

(defun org-babel-prep-session:org (_session _params)
  "Return an error because org does not support sessions."
  (error "Org does not support sessions"))

(provide 'ob-org)



;;; ob-org.el ends here
