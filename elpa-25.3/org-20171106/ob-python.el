;;; ob-python.el --- Babel Functions for Python      -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017 Free Software Foundation, Inc.

;; Authors: Eric Schulte
;;	 Dan Davison
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

;; Org-Babel support for evaluating python source code.

;;; Code:
(require 'ob)

(declare-function org-remove-indentation "org" )
(declare-function org-trim "org" (s &optional keep-lead))
(declare-function py-shell "ext:python-mode" (&optional argprompt))
(declare-function py-toggle-shells "ext:python-mode" (arg))
(declare-function run-python "ext:python" (&optional cmd dedicated show))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("python" . "py"))

(defvar org-babel-default-header-args:python '())

(defcustom org-babel-python-command "python"
  "Name of the command for executing Python code."
  :version "24.4"
  :package-version '(Org . "8.0")
  :group 'org-babel
  :type 'string)

(defcustom org-babel-python-mode
  (if (featurep 'python-mode) 'python-mode 'python)
  "Preferred python mode for use in running python interactively.
This will typically be either `python' or `python-mode'."
  :group 'org-babel
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'symbol)

(defcustom org-babel-python-hline-to "None"
  "Replace hlines in incoming tables with this when translating to python."
  :group 'org-babel
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-babel-python-None-to 'hline
  "Replace `None' in python tables with this before returning."
  :group 'org-babel
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'symbol)

(defun org-babel-execute:python (body params)
  "Execute a block of Python code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((org-babel-python-command
	  (or (cdr (assq :python params))
	      org-babel-python-command))
	 (session (org-babel-python-initiate-session
		   (cdr (assq :session params))))
         (result-params (cdr (assq :result-params params)))
         (result-type (cdr (assq :result-type params)))
	 (return-val (when (and (eq result-type 'value) (not session))
		       (cdr (assq :return params))))
	 (preamble (cdr (assq :preamble params)))
         (full-body
	  (org-babel-expand-body:generic
	   (concat body (if return-val (format "\nreturn %s" return-val) ""))
	   params (org-babel-variable-assignments:python params)))
         (result (org-babel-python-evaluate
		  session full-body result-type result-params preamble)))
    (org-babel-reassemble-table
     result
     (org-babel-pick-name (cdr (assq :colname-names params))
			  (cdr (assq :colnames params)))
     (org-babel-pick-name (cdr (assq :rowname-names params))
			  (cdr (assq :rownames params))))))

(defun org-babel-prep-session:python (session params)
  "Prepare SESSION according to the header arguments in PARAMS.
VARS contains resolved variable references"
  (let* ((session (org-babel-python-initiate-session session))
	 (var-lines
	  (org-babel-variable-assignments:python params)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (end-of-line 1) (insert var) (comint-send-input)
              (org-babel-comint-wait-for-output session)) var-lines))
    session))

(defun org-babel-load-session:python (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:python session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))

;; helper functions

(defun org-babel-variable-assignments:python (params)
  "Return a list of Python statements assigning the block's variables."
  (mapcar
   (lambda (pair)
     (format "%s=%s"
	     (car pair)
	     (org-babel-python-var-to-python (cdr pair))))
   (org-babel--get-vars params)))

(defun org-babel-python-var-to-python (var)
  "Convert an elisp value to a python variable.
Convert an elisp value, VAR, into a string of python source code
specifying a variable of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-python-var-to-python var ", ") "]")
    (if (eq var 'hline)
	org-babel-python-hline-to
      (format
       (if (and (stringp var) (string-match "[\n\r]" var)) "\"\"%S\"\"" "%S")
       (if (stringp var) (substring-no-properties var) var)))))

(defun org-babel-python-table-or-string (results)
  "Convert RESULTS into an appropriate elisp value.
If the results look like a list or tuple, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (let ((res (org-babel-script-escape results)))
    (if (listp res)
        (mapcar (lambda (el) (if (eq el 'None)
                            org-babel-python-None-to el))
                res)
      res)))

(defvar org-babel-python-buffers '((:default . "*Python*")))

(defun org-babel-python-session-buffer (session)
  "Return the buffer associated with SESSION."
  (cdr (assoc session org-babel-python-buffers)))

(defun org-babel-python-with-earmuffs (session)
  (let ((name (if (stringp session) session (format "%s" session))))
    (if (and (string= "*" (substring name 0 1))
	     (string= "*" (substring name (- (length name) 1))))
	name
      (format "*%s*" name))))

(defun org-babel-python-without-earmuffs (session)
  (let ((name (if (stringp session) session (format "%s" session))))
    (if (and (string= "*" (substring name 0 1))
	     (string= "*" (substring name (- (length name) 1))))
	(substring name 1 (- (length name) 1))
      name)))

(defvar py-default-interpreter)
(defvar py-which-bufname)
(defvar python-shell-buffer-name)
(defun org-babel-python-initiate-session-by-key (&optional session)
  "Initiate a python session.
If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (require org-babel-python-mode)
  (save-window-excursion
    (let* ((session (if session (intern session) :default))
           (python-buffer (org-babel-python-session-buffer session))
	   (cmd (if (member system-type '(cygwin windows-nt ms-dos))
		    (concat org-babel-python-command " -i")
		  org-babel-python-command)))
      (cond
       ((and (eq 'python org-babel-python-mode)
	     (fboundp 'run-python)) ; python.el
	(if (not (version< "24.1" emacs-version))
	    (run-python cmd)
	  (unless python-buffer
	    (setq python-buffer (org-babel-python-with-earmuffs session)))
	  (let ((python-shell-buffer-name
		 (org-babel-python-without-earmuffs python-buffer)))
	    (run-python cmd))))
       ((and (eq 'python-mode org-babel-python-mode)
	     (fboundp 'py-shell)) ; python-mode.el
	;; Make sure that py-which-bufname is initialized, as otherwise
	;; it will be overwritten the first time a Python buffer is
	;; created.
	(py-toggle-shells py-default-interpreter)
	;; `py-shell' creates a buffer whose name is the value of
	;; `py-which-bufname' with '*'s at the beginning and end
	(let* ((bufname (if (and python-buffer (buffer-live-p python-buffer))
			    (replace-regexp-in-string ;; zap surrounding *
			     "^\\*\\([^*]+\\)\\*$" "\\1" python-buffer)
			  (concat "Python-" (symbol-name session))))
	       (py-which-bufname bufname))
	  (py-shell)
	  (setq python-buffer (org-babel-python-with-earmuffs bufname))))
       (t
	(error "No function available for running an inferior Python")))
      (setq org-babel-python-buffers
	    (cons (cons session python-buffer)
		  (assq-delete-all session org-babel-python-buffers)))
      session)))

(defun org-babel-python-initiate-session (&optional session _params)
  "Create a session named SESSION according to PARAMS."
  (unless (string= session "none")
    (org-babel-python-session-buffer
     (org-babel-python-initiate-session-by-key session))))

(defvar org-babel-python-eoe-indicator "'org_babel_python_eoe'"
  "A string to indicate that evaluation has completed.")
(defconst org-babel-python-wrapper-method
  "
def main():
%s

open('%s', 'w').write( str(main()) )")
(defconst org-babel-python-pp-wrapper-method
  "
import pprint
def main():
%s

open('%s', 'w').write( pprint.pformat(main()) )")

(defun org-babel-python-evaluate
  (session body &optional result-type result-params preamble)
  "Evaluate BODY as Python code."
  (if session
      (org-babel-python-evaluate-session
       session body result-type result-params)
    (org-babel-python-evaluate-external-process
     body result-type result-params preamble)))

(defun org-babel-python-evaluate-external-process
    (body &optional result-type result-params preamble)
  "Evaluate BODY in external python process.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as elisp."
  (let ((raw
         (pcase result-type
           (`output (org-babel-eval org-babel-python-command
				    (concat (if preamble (concat preamble "\n"))
					    body)))
           (`value (let ((tmp-file (org-babel-temp-file "python-")))
		     (org-babel-eval
		      org-babel-python-command
		      (concat
		       (if preamble (concat preamble "\n") "")
		       (format
			(if (member "pp" result-params)
			    org-babel-python-pp-wrapper-method
			  org-babel-python-wrapper-method)
			(mapconcat
			 (lambda (line) (format "\t%s" line))
			 (split-string (org-remove-indentation (org-trim body))
				       "[\r\n]")
			 "\n")
			(org-babel-process-file-name tmp-file 'noquote))))
		     (org-babel-eval-read-file tmp-file))))))
    (org-babel-result-cond result-params
      raw
      (org-babel-python-table-or-string (org-trim raw)))))

(defun org-babel-python-evaluate-session
    (session body &optional result-type result-params)
  "Pass BODY to the Python process in SESSION.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as elisp."
  (let* ((send-wait (lambda () (comint-send-input nil t) (sleep-for 0 5)))
	 (dump-last-value
	  (lambda
	    (tmp-file pp)
	    (mapc
	     (lambda (statement) (insert statement) (funcall send-wait))
	     (if pp
		 (list
		  "import pprint"
		  (format "open('%s', 'w').write(pprint.pformat(_))"
			  (org-babel-process-file-name tmp-file 'noquote)))
	       (list (format "open('%s', 'w').write(str(_))"
			     (org-babel-process-file-name tmp-file
                                                          'noquote)))))))
	 (input-body (lambda (body)
		       (mapc (lambda (line) (insert line) (funcall send-wait))
			     (split-string body "[\r\n]"))
		       (funcall send-wait)))
         (results
          (pcase result-type
            (`output
             (mapconcat
              #'org-trim
              (butlast
               (org-babel-comint-with-output
                   (session org-babel-python-eoe-indicator t body)
                 (funcall input-body body)
                 (funcall send-wait) (funcall send-wait)
                 (insert org-babel-python-eoe-indicator)
                 (funcall send-wait))
               2) "\n"))
            (`value
             (let ((tmp-file (org-babel-temp-file "python-")))
               (org-babel-comint-with-output
                   (session org-babel-python-eoe-indicator nil body)
                 (let ((comint-process-echoes nil))
                   (funcall input-body body)
                   (funcall dump-last-value tmp-file
                            (member "pp" result-params))
                   (funcall send-wait) (funcall send-wait)
                   (insert org-babel-python-eoe-indicator)
                   (funcall send-wait)))
               (org-babel-eval-read-file tmp-file))))))
    (unless (string= (substring org-babel-python-eoe-indicator 1 -1) results)
      (org-babel-result-cond result-params
	results
        (org-babel-python-table-or-string results)))))

(defun org-babel-python-read-string (string)
  "Strip \\='s from around Python string."
  (if (and (string-prefix-p "'" string)
	   (string-suffix-p "'" string))
      (substring string 1 -1)
    string))

(provide 'ob-python)



;;; ob-python.el ends here
