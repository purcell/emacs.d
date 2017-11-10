;;; ob-shell.el --- Babel Functions for Shell Evaluation -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017 Free Software Foundation, Inc.

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

;; Org-Babel support for evaluating shell source code.

;;; Code:
(require 'ob)
(require 'shell)
(require 'cl-lib)

(declare-function org-babel-comint-in-buffer "ob-comint" (buffer &rest body)
		  t)
(declare-function org-babel-comint-wait-for-output "ob-comint" (buffer))
(declare-function org-babel-comint-buffer-livep "ob-comint" (buffer))
(declare-function org-babel-comint-with-output "ob-comint" (meta &rest body)
		  t)
(declare-function org-trim "org" (s &optional keep-lead))
(declare-function orgtbl-to-generic "org-table" (table params))

(defvar org-babel-default-header-args:shell '())
(defvar org-babel-shell-names)

(defun org-babel-shell-initialize ()
  "Define execution functions associated to shell names.
This function has to be called whenever `org-babel-shell-names'
is modified outside the Customize interface."
  (interactive)
  (dolist (name org-babel-shell-names)
    (eval `(defun ,(intern (concat "org-babel-execute:" name))
	       (body params)
	     ,(format "Execute a block of %s commands with Babel." name)
	     (let ((shell-file-name ,name))
	       (org-babel-execute:shell body params))))
    (eval `(defalias ',(intern (concat "org-babel-variable-assignments:" name))
	     'org-babel-variable-assignments:shell
	     ,(format "Return list of %s statements assigning to the block's \
variables."
		      name)))))

(defcustom org-babel-shell-names
  '("sh" "bash" "csh" "ash" "dash" "ksh" "mksh" "posh")
  "List of names of shell supported by babel shell code blocks.
Call `org-babel-shell-initialize' when modifying this variable
outside the Customize interface."
  :group 'org-babel
  :type '(repeat (string :tag "Shell name: "))
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (org-babel-shell-initialize)))

(defun org-babel-execute:shell (body params)
  "Execute a block of Shell commands with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((session (org-babel-sh-initiate-session
		   (cdr (assq :session params))))
	 (stdin (let ((stdin (cdr (assq :stdin params))))
                  (when stdin (org-babel-sh-var-to-string
                               (org-babel-ref-resolve stdin)))))
	 (cmdline (cdr (assq :cmdline params)))
         (full-body (org-babel-expand-body:generic
		     body params (org-babel-variable-assignments:shell params))))
    (org-babel-reassemble-table
     (org-babel-sh-evaluate session full-body params stdin cmdline)
     (org-babel-pick-name
      (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
     (org-babel-pick-name
      (cdr (assq :rowname-names params)) (cdr (assq :rownames params))))))

(defun org-babel-prep-session:shell (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-sh-initiate-session session))
	 (var-lines (org-babel-variable-assignments:shell params)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session)) var-lines))
    session))

(defun org-babel-load-session:shell (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:shell session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))


;;; Helper functions
(defun org-babel--variable-assignments:sh-generic
    (varname values &optional sep hline)
  "Returns a list of statements declaring the values as a generic variable."
  (format "%s=%s" varname (org-babel-sh-var-to-sh values sep hline)))

(defun org-babel--variable-assignments:bash_array
    (varname values &optional sep hline)
  "Returns a list of statements declaring the values as a bash array."
  (format "unset %s\ndeclare -a %s=( %s )"
	  varname varname
	  (mapconcat
	   (lambda (value) (org-babel-sh-var-to-sh value sep hline))
	   values
	   " ")))

(defun org-babel--variable-assignments:bash_assoc
    (varname values &optional sep hline)
  "Returns a list of statements declaring the values as bash associative array."
  (format "unset %s\ndeclare -A %s\n%s"
    varname varname
    (mapconcat
     (lambda (items)
       (format "%s[%s]=%s"
	       varname
	       (org-babel-sh-var-to-sh (car items) sep hline)
	       (org-babel-sh-var-to-sh (cdr items) sep hline)))
     values
     "\n")))

(defun org-babel--variable-assignments:bash (varname values &optional sep hline)
  "Represents the parameters as useful Bash shell variables."
  (pcase values
    (`((,_ ,_ . ,_) . ,_)		;two-dimensional array
     (org-babel--variable-assignments:bash_assoc varname values sep hline))
    (`(,_ . ,_)				;simple list
     (org-babel--variable-assignments:bash_array varname values sep hline))
    (_					;scalar value
     (org-babel--variable-assignments:sh-generic varname values sep hline))))

(defun org-babel-variable-assignments:shell (params)
  "Return list of shell statements assigning the block's variables."
  (let ((sep (cdr (assq :separator params)))
	(hline (when (string= "yes" (cdr (assq :hlines params)))
		 (or (cdr (assq :hline-string params))
		     "hline"))))
    (mapcar
     (lambda (pair)
       (if (string-suffix-p "bash" shell-file-name)
	   (org-babel--variable-assignments:bash
            (car pair) (cdr pair) sep hline)
         (org-babel--variable-assignments:sh-generic
	  (car pair) (cdr pair) sep hline)))
     (org-babel--get-vars params))))

(defun org-babel-sh-var-to-sh (var &optional sep hline)
  "Convert an elisp value to a shell variable.
Convert an elisp var into a string of shell commands specifying a
var of the same value."
  (concat "'" (replace-regexp-in-string
	       "'" "'\"'\"'"
	       (org-babel-sh-var-to-string var sep hline))
	  "'"))

(defun org-babel-sh-var-to-string (var &optional sep hline)
  "Convert an elisp value to a string."
  (let ((echo-var (lambda (v) (if (stringp v) v (format "%S" v)))))
    (cond
     ((and (listp var) (or (listp (car var)) (eq (car var) 'hline)))
      (orgtbl-to-generic var  (list :sep (or sep "\t") :fmt echo-var
				    :hline hline)))
     ((listp var)
      (mapconcat echo-var var "\n"))
     (t (funcall echo-var var)))))

(defun org-babel-sh-initiate-session (&optional session _params)
  "Initiate a session named SESSION according to PARAMS."
  (when (and session (not (string= session "none")))
    (save-window-excursion
      (or (org-babel-comint-buffer-livep session)
          (progn
	    (shell session)
	    ;; Needed for Emacs 23 since the marker is initially
	    ;; undefined and the filter functions try to use it without
	    ;; checking.
	    (set-marker comint-last-output-start (point))
	    (get-buffer (current-buffer)))))))

(defvar org-babel-sh-eoe-indicator "echo 'org_babel_sh_eoe'"
  "String to indicate that evaluation has completed.")
(defvar org-babel-sh-eoe-output "org_babel_sh_eoe"
  "String to indicate that evaluation has completed.")

(defun org-babel-sh-evaluate (session body &optional params stdin cmdline)
  "Pass BODY to the Shell process in BUFFER.
If RESULT-TYPE equals `output' then return a list of the outputs
of the statements in BODY, if RESULT-TYPE equals `value' then
return the value of the last statement in BODY."
  (let ((results
         (cond
          ((or stdin cmdline)	       ; external shell script w/STDIN
           (let ((script-file (org-babel-temp-file "sh-script-"))
                 (stdin-file (org-babel-temp-file "sh-stdin-"))
                 (shebang (cdr (assq :shebang params)))
                 (padline (not (string= "no" (cdr (assq :padline params))))))
             (with-temp-file script-file
               (when shebang (insert (concat shebang "\n")))
               (when padline (insert "\n"))
               (insert body))
             (set-file-modes script-file #o755)
             (with-temp-file stdin-file (insert (or stdin "")))
             (with-temp-buffer
               (call-process-shell-command
                (concat (if shebang script-file
			  (format "%s %s" shell-file-name script-file))
			(and cmdline (concat " " cmdline)))
                stdin-file
		(current-buffer))
               (buffer-string))))
          (session                      ; session evaluation
           (mapconcat
            #'org-babel-sh-strip-weird-long-prompt
            (mapcar
             #'org-trim
             (butlast
              (org-babel-comint-with-output
                  (session org-babel-sh-eoe-output t body)
                (mapc
                 (lambda (line)
                   (insert line)
                   (comint-send-input nil t)
                   (while (save-excursion
                            (goto-char comint-last-input-end)
                            (not (re-search-forward
                                  comint-prompt-regexp nil t)))
                     (accept-process-output
                      (get-buffer-process (current-buffer)))))
                 (append
                  (split-string (org-trim body) "\n")
                  (list org-babel-sh-eoe-indicator))))
              2)) "\n"))
          ('otherwise                   ; external shell script
           (if (and (cdr (assq :shebang params))
                    (> (length (cdr (assq :shebang params))) 0))
               (let ((script-file (org-babel-temp-file "sh-script-"))
                     (shebang (cdr (assq :shebang params)))
                     (padline (not (equal "no" (cdr (assq :padline params))))))
                 (with-temp-file script-file
                   (when shebang (insert (concat shebang "\n")))
                   (when padline (insert "\n"))
                   (insert body))
                 (set-file-modes script-file #o755)
                 (org-babel-eval script-file ""))
             (org-babel-eval shell-file-name (org-trim body)))))))
    (when results
      (let ((result-params (cdr (assq :result-params params))))
        (org-babel-result-cond result-params
          results
          (let ((tmp-file (org-babel-temp-file "sh-")))
            (with-temp-file tmp-file (insert results))
            (org-babel-import-elisp-from-file tmp-file)))))))

(defun org-babel-sh-strip-weird-long-prompt (string)
  "Remove prompt cruft from a string of shell output."
  (while (string-match "^% +[\r\n$]+ *" string)
    (setq string (substring string (match-end 0))))
  string)

(provide 'ob-shell)



;;; ob-shell.el ends here
