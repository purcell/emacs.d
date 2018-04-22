;;; ob-forth.el --- Babel Functions for Forth        -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2017 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research, forth
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

;; Requires the gforth forth compiler and `forth-mode' (see below).
;; https://www.gnu.org/software/gforth/

;;; Requirements:

;; Session evaluation requires the gforth forth compiler as well as
;; `forth-mode' which is distributed with gforth (in gforth.el).

;;; Code:
(require 'ob)

(declare-function forth-proc "ext:gforth" ())
(declare-function org-trim "org" (s &optional keep-lead))

(defvar org-babel-default-header-args:forth '((:session . "yes"))
  "Default header arguments for forth code blocks.")

(defun org-babel-execute:forth (body params)
  "Execute a block of Forth code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (if (string= "none" (cdr (assq :session params)))
      (error "Non-session evaluation not supported for Forth code blocks")
    (let ((all-results (org-babel-forth-session-execute body params)))
      (if (member "output" (cdr (assq :result-params params)))
	  (mapconcat #'identity all-results "\n")
	(car (last all-results))))))

(defun org-babel-forth-session-execute (body params)
  (require 'forth-mode)
  (let ((proc (forth-proc))
	(rx " \\(\n:\\|compiled\n\\\|ok\n\\)")
	(result-start))
    (with-current-buffer (process-buffer (forth-proc))
      (mapcar (lambda (line)
		(setq result-start (progn (goto-char (process-mark proc))
					  (point)))
		(comint-send-string proc (concat line "\n"))
		;; wait for forth to say "ok"
		(while (not (progn (goto-char result-start)
				   (re-search-forward rx nil t)))
		  (accept-process-output proc 0.01))
		(let ((case (match-string 1)))
		  (cond
		   ((string= "ok\n" case)
		    ;; Collect intermediate output.
		    (buffer-substring (+ result-start 1 (length line))
				      (match-beginning 0)))
		   ((string= "compiled\n" case))
		   ;; Ignore partial compilation.
		   ((string= "\n:" case)
		    ;; Report errors.
		    (org-babel-eval-error-notify 1
		     (buffer-substring
		      (+ (match-beginning 0) 1) (point-max))) nil))))
	      (split-string (org-trim
			     (org-babel-expand-body:generic body params))
			    "\n"
			    'omit-nulls)))))

(provide 'ob-forth)

;;; ob-forth.el ends here
