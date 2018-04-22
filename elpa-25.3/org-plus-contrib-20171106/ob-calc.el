;;; ob-calc.el --- Babel Functions for Calc          -*- lexical-binding: t; -*-

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

;; Org-Babel support for evaluating calc code

;;; Code:
(require 'ob)
(require 'calc)
(require 'calc-trail)
(require 'calc-store)

(declare-function calc-store-into    "calc-store" (&optional var))
(declare-function calc-recall        "calc-store" (&optional var))
(declare-function math-evaluate-expr "calc-ext"   (x))
(declare-function org-trim "org" (s &optional keep-lead))

(defvar org-babel-default-header-args:calc nil
  "Default arguments for evaluating an calc source block.")

(defun org-babel-expand-body:calc (body _params)
  "Expand BODY according to PARAMS, return the expanded body." body)

(defvar org--var-syms) ; Dynamically scoped from org-babel-execute:calc

(defun org-babel-execute:calc (body params)
  "Execute a block of calc code with Babel."
  (unless (get-buffer "*Calculator*")
    (save-window-excursion (calc) (calc-quit)))
  (let* ((vars (org-babel--get-vars params))
	 (org--var-syms (mapcar #'car vars))
	 (var-names (mapcar #'symbol-name org--var-syms)))
    (mapc
     (lambda (pair)
       (calc-push-list (list (cdr pair)))
       (calc-store-into (car pair)))
     vars)
    (mapc
     (lambda (line)
       (when (> (length line) 0)
	 (cond
	  ;; simple variable name
	  ((member line var-names) (calc-recall (intern line)))
	  ;; stack operation
	  ((string= "'" (substring line 0 1))
	   (funcall (lookup-key calc-mode-map (substring line 1)) nil))
	  ;; complex expression
	  (t
	   (calc-push-list
	    (list (let ((res (calc-eval line)))
                    (cond
                     ((numberp res) res)
                     ((math-read-number res) (math-read-number res))
                     ((listp res) (error "Calc error \"%s\" on input \"%s\""
                                         (cadr res) line))
                     (t (replace-regexp-in-string
                         "'" ""
                         (calc-eval
                          (math-evaluate-expr
                           ;; resolve user variables, calc built in
                           ;; variables are handled automatically
                           ;; upstream by calc
                           (mapcar #'org-babel-calc-maybe-resolve-var
                                   ;; parse line into calc objects
                                   (car (math-read-exprs line)))))))))
                  ))))))
     (mapcar #'org-trim
	     (split-string (org-babel-expand-body:calc body params) "[\n\r]"))))
  (save-excursion
    (with-current-buffer (get-buffer "*Calculator*")
      (prog1
        (calc-eval (calc-top 1))
        (calc-pop 1)))))

(defun org-babel-calc-maybe-resolve-var (el)
  (if (consp el)
      (if (and (eq 'var (car el)) (member (cadr el) org--var-syms))
	  (progn
	    (calc-recall (cadr el))
	    (prog1 (calc-top 1)
	      (calc-pop 1)))
	(mapcar #'org-babel-calc-maybe-resolve-var el))
    el))

(provide 'ob-calc)



;;; ob-calc.el ends here
