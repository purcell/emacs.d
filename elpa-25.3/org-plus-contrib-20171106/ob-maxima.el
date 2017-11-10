;;; ob-maxima.el --- Babel Functions for Maxima      -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017 Free Software Foundation, Inc.

;; Author: Eric S Fraga
;;	Eric Schulte
;; Keywords: literate programming, reproducible research, maxima
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

;; Org-Babel support for evaluating maxima entries.
;;
;; This differs from most standard languages in that
;;
;; 1) there is no such thing as a "session" in maxima
;;
;; 2) we are adding the "cmdline" header argument

;;; Code:
(require 'ob)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("maxima" . "max"))

(defvar org-babel-default-header-args:maxima '())

(defcustom org-babel-maxima-command
  (if (boundp 'maxima-command) maxima-command "maxima")
  "Command used to call maxima on the shell."
  :group 'org-babel
  :type 'string)

(defun org-babel-maxima-expand (body params)
  "Expand a block of Maxima code according to its header arguments."
  (let ((vars (org-babel--get-vars params))
	(epilogue (cdr (assq :epilogue params)))
	(prologue (cdr (assq :prologue params))))
    (mapconcat 'identity
	       (list
		;; Any code from the specified prologue at the start.
		prologue
		;; graphic output
		(let ((graphic-file (ignore-errors (org-babel-graphical-output-file params))))
		  (if graphic-file
		      (format
		       "set_plot_option ([gnuplot_term, png]); set_plot_option ([gnuplot_out_file, %S]);"
		       graphic-file)
		    ""))
		;; variables
		(mapconcat 'org-babel-maxima-var-to-maxima vars "\n")
		;; body
		body
		;; Any code from the specified epilogue at the end.
		epilogue
		"gnuplot_close ()$")
	       "\n")))

(defun org-babel-execute:maxima (body params)
  "Execute a block of Maxima entries with org-babel.
This function is called by `org-babel-execute-src-block'."
  (message "executing Maxima source code block")
  (let ((result-params (split-string (or (cdr (assq :results params)) "")))
	(result
	 (let* ((cmdline (or (cdr (assq :cmdline params)) ""))
		(in-file (org-babel-temp-file "maxima-" ".max"))
		(cmd (format "%s --very-quiet -r 'batchload(%S)$' %s"
			     org-babel-maxima-command in-file cmdline)))
	   (with-temp-file in-file (insert (org-babel-maxima-expand body params)))
	   (message cmd)
           ;; " | grep -v batch | grep -v 'replaced' | sed '/^$/d' "
	   (let ((raw (org-babel-eval cmd "")))
             (mapconcat
              #'identity
              (delq nil
                    (mapcar (lambda (line)
                              (unless (or (string-match "batch" line)
                                          (string-match "^rat: replaced .*$" line)
                                          (string-match "^;;; Loading #P" line)
                                          (= 0 (length line)))
                                line))
                            (split-string raw "[\r\n]"))) "\n")))))
    (if (ignore-errors (org-babel-graphical-output-file params))
	nil
      (org-babel-result-cond result-params
	result
	(let ((tmp-file (org-babel-temp-file "maxima-res-")))
	  (with-temp-file tmp-file (insert result))
	  (org-babel-import-elisp-from-file tmp-file))))))


(defun org-babel-prep-session:maxima (_session _params)
  (error "Maxima does not support sessions"))

(defun org-babel-maxima-var-to-maxima (pair)
  "Convert an elisp val into a string of maxima code specifying a var
of the same value."
  (let ((var (car pair))
        (val (cdr pair)))
    (when (symbolp val)
      (setq val (symbol-name val))
      (when (= (length val) 1)
        (setq val (string-to-char val))))
    (format "%S: %s$" var
	    (org-babel-maxima-elisp-to-maxima val))))

(defun org-babel-maxima-elisp-to-maxima (val)
  "Return a string of maxima code which evaluates to VAL."
  (if (listp val)
      (concat "[" (mapconcat #'org-babel-maxima-elisp-to-maxima val ", ") "]")
    (format "%s" val)))


(provide 'ob-maxima)



;;; ob-maxima.el ends here
