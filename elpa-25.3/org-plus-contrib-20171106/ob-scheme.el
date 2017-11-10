;;; ob-scheme.el --- Babel Functions for Scheme      -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2017 Free Software Foundation, Inc.

;; Authors: Eric Schulte
;;	    Michael Gauland
;; Keywords: literate programming, reproducible research, scheme
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

;; Now working with SBCL for both session and external evaluation.
;;
;; This certainly isn't optimally robust, but it seems to be working
;; for the basic use cases.

;;; Requirements:

;; - a working scheme implementation
;;   (e.g. guile https://www.gnu.org/software/guile/guile.html)
;;
;; - for session based evaluation geiser is required, which is available from
;;   ELPA.

;;; Code:
(require 'ob)
(require 'geiser nil t)
(defvar geiser-repl--repl)             ; Defined in geiser-repl.el
(defvar geiser-impl--implementation)   ; Defined in geiser-impl.el
(defvar geiser-default-implementation) ; Defined in geiser-impl.el
(defvar geiser-active-implementations) ; Defined in geiser-impl.el
(defvar geiser-debug-show-debug-p)     ; Defined in geiser-debug.el
(defvar geiser-debug-jump-to-debug-p)  ; Defined in geiser-debug.el
(defvar geiser-repl-use-other-window)  ; Defined in geiser-repl.el
(defvar geiser-repl-window-allow-split)	; Defined in geiser-repl.el

(declare-function run-geiser "ext:geiser-repl" (impl))
(declare-function geiser-mode "ext:geiser-mode" ())
(declare-function geiser-eval-region "ext:geiser-mode"
                  (start end &optional and-go raw nomsg))
(declare-function geiser-repl-exit "ext:geiser-repl" (&optional arg))
(declare-function geiser-eval--retort-output "ext:geiser-eval" (ret))
(declare-function geiser-eval--retort-result-str "ext:geiser-eval" (ret prefix))

(defcustom org-babel-scheme-null-to 'hline
  "Replace `null' and empty lists in scheme tables with this before returning."
  :group 'org-babel
  :version "26.1"
  :package-version '(Org . "9.1")
  :type 'symbol)

(defvar org-babel-default-header-args:scheme '()
  "Default header arguments for scheme code blocks.")

(defun org-babel-expand-body:scheme (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params))
	(prepends (cdr (assq :prologue params))))
    (concat (and prepends (concat prepends "\n"))
	    (if (null vars) body
	      (format "(let (%s)\n%s\n)"
		      (mapconcat
		       (lambda (var)
			 (format "%S" (print `(,(car var) ',(cdr var)))))
		       vars
		       "\n      ")
		      body)))))


(defvar org-babel-scheme-repl-map (make-hash-table :test #'equal)
  "Map of scheme sessions to session names.")

(defun org-babel-scheme-cleanse-repl-map ()
  "Remove dead buffers from the REPL map."
  (maphash
   (lambda (x y) (unless (buffer-name y) (remhash x org-babel-scheme-repl-map)))
   org-babel-scheme-repl-map))

(defun org-babel-scheme-get-session-buffer (session-name)
  "Look up the scheme buffer for a session; return nil if it doesn't exist."
  (org-babel-scheme-cleanse-repl-map) ; Prune dead sessions
  (gethash session-name org-babel-scheme-repl-map))

(defun org-babel-scheme-set-session-buffer (session-name buffer)
  "Record the scheme buffer used for a given session."
  (puthash session-name buffer org-babel-scheme-repl-map))

(defun org-babel-scheme-get-buffer-impl (buffer)
  "Returns the scheme implementation geiser associates with the buffer."
  (with-current-buffer (set-buffer buffer)
    geiser-impl--implementation))

(defun org-babel-scheme-get-repl (impl name)
  "Switch to a scheme REPL, creating it if it doesn't exist:"
  (let ((buffer (org-babel-scheme-get-session-buffer name)))
    (or buffer
	(progn
	  (run-geiser impl)
	  (if name
	      (progn
		(rename-buffer name t)
		(org-babel-scheme-set-session-buffer name (current-buffer))))
	  (current-buffer)))))

(defun org-babel-scheme-make-session-name (buffer name impl)
  "Generate a name for the session buffer.

For a named session, the buffer name will be the session name.

If the session is unnamed (nil), generate a name.

If the session is `none', use nil for the session name, and
org-babel-scheme-execute-with-geiser will use a temporary session."
  (cond ((not name) (concat buffer " " (symbol-name impl) " REPL"))
	((string= name "none") nil)
	(name)))

(defmacro org-babel-scheme-capture-current-message (&rest body)
  "Capture current message in both interactive and noninteractive mode"
  `(if noninteractive
       (let ((original-message (symbol-function 'message))
             (current-message nil))
         (unwind-protect
             (progn
               (defun message (&rest args)
                 (setq current-message (apply original-message args)))
               ,@body
               current-message)
           (fset 'message original-message)))
     (progn
       ,@body
       (current-message))))

(defun org-babel-scheme-execute-with-geiser (code output impl repl)
  "Execute code in specified REPL. If the REPL doesn't exist, create it
using the given scheme implementation.

Returns the output of executing the code if the output parameter
is true; otherwise returns the last value."
  (let ((result nil))
    (with-temp-buffer
      (insert (format ";; -*- geiser-scheme-implementation: %s -*-" impl))
      (newline)
      (insert code)
      (geiser-mode)
      (let ((geiser-repl-window-allow-split nil)
	    (geiser-repl-use-other-window nil))
	(let ((repl-buffer (save-current-buffer
			     (org-babel-scheme-get-repl impl repl))))
	  (when (not (eq impl (org-babel-scheme-get-buffer-impl
			       (current-buffer))))
	    (message "Implementation mismatch: %s (%s) %s (%s)" impl (symbolp impl)
		     (org-babel-scheme-get-buffer-impl (current-buffer))
		     (symbolp (org-babel-scheme-get-buffer-impl
			       (current-buffer)))))
	  (setq geiser-repl--repl repl-buffer)
	  (setq geiser-impl--implementation nil)
	  (let ((geiser-debug-jump-to-debug-p nil)
		(geiser-debug-show-debug-p nil))
	    (let ((ret (geiser-eval-region (point-min) (point-max))))
	      (setq result (if output
			       (geiser-eval--retort-output ret)
			     (geiser-eval--retort-result-str ret "")))))
	  (when (not repl)
	    (save-current-buffer (set-buffer repl-buffer)
				 (geiser-repl-exit))
	    (set-process-query-on-exit-flag (get-buffer-process repl-buffer) nil)
	    (kill-buffer repl-buffer)))))
    result))

(defun org-babel-scheme--table-or-string (results)
  "Convert RESULTS into an appropriate elisp value.
If the results look like a list or tuple, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (let ((res (org-babel-script-escape results)))
    (cond ((listp res)
           (mapcar (lambda (el)
		     (if (or (null el) (eq el 'null))
			 org-babel-scheme-null-to
		       el))
                   res))
	  (t res))))

(defun org-babel-execute:scheme (body params)
  "Execute a block of Scheme code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((source-buffer (current-buffer))
	 (source-buffer-name (replace-regexp-in-string ;; zap surrounding *
			      "^ ?\\*\\([^*]+\\)\\*" "\\1"
			      (buffer-name source-buffer))))
    (save-excursion
      (let* ((result-type (cdr (assq :result-type params)))
	     (impl (or (when (cdr (assq :scheme params))
			 (intern (cdr (assq :scheme params))))
		       geiser-default-implementation
		       (car geiser-active-implementations)))
	     (session (org-babel-scheme-make-session-name
		       source-buffer-name (cdr (assq :session params)) impl))
	     (full-body (org-babel-expand-body:scheme body params))
	     (result
	      (org-babel-scheme-execute-with-geiser
	       full-body		       ; code
	       (string= result-type "output")  ; output?
	       impl			       ; implementation
	       (and (not (string= session "none")) session)))) ; session
	(let ((table
	       (org-babel-reassemble-table
		result
		(org-babel-pick-name (cdr (assq :colname-names params))
				     (cdr (assq :colnames params)))
		(org-babel-pick-name (cdr (assq :rowname-names params))
				     (cdr (assq :rownames params))))))
	  (org-babel-scheme--table-or-string table))))))

(provide 'ob-scheme)

;;; ob-scheme.el ends here
