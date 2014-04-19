;;; ob-scheme.el --- org-babel functions for Scheme

;; Copyright (C) 2010-2014 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Now working with SBCL for both session and external evaluation.
;;
;; This certainly isn't optimally robust, but it seems to be working
;; for the basic use cases.

;;; Requirements:

;; - a working scheme implementation
;;   (e.g. guile http://www.gnu.org/software/guile/guile.html)
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

(declare-function run-geiser "geiser-repl" (impl))
(declare-function geiser-mode "geiser-mode" ())
(declare-function geiser-eval-region "geiser-mode" (start end &optional and-go raw nomsg))
(declare-function geiser-repl-exit "geiser-repl" (&optional arg))

(defvar org-babel-default-header-args:scheme '()
  "Default header arguments for scheme code blocks.")

(defun org-babel-expand-body:scheme (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (mapcar #'cdr (org-babel-get-header params :var))))
    (if (> (length vars) 0)
        (concat "(let ("
                (mapconcat
                 (lambda (var) (format "%S" (print `(,(car var) ',(cdr var)))))
                 vars "\n      ")
                ")\n" body ")")
      body)))


(defvar org-babel-scheme-repl-map (make-hash-table :test 'equal)
  "Map of scheme sessions to session names.")

(defun org-babel-scheme-cleanse-repl-map ()
  "Remove dead buffers from the REPL map."
  (maphash
   (lambda (x y)
     (when (not (buffer-name y))
       (remhash x org-babel-scheme-repl-map)))
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

If the session is 'none', use nil for the session name, and
org-babel-scheme-execute-with-geiser will use a temporary session."
  (let ((result
	 (cond ((not name)
		(concat buffer " " (symbol-name impl) " REPL"))
	       ((string= name "none") nil)
	       (name))))
    result))

(defun org-babel-scheme-execute-with-geiser (code output impl repl)
  "Execute code in specified REPL. If the REPL doesn't exist, create it
using the given scheme implementation.

Returns the output of executing the code if the output parameter
is true; otherwise returns the last value."
  (let ((result nil))
    (with-temp-buffer
      (insert (format ";; -*- geiser-scheme-implementation: %s -*-" impl))
      (newline)
      (insert (if output
		  (format "(with-output-to-string (lambda () %s))" code)
		code))
      (geiser-mode)
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
	(geiser-eval-region (point-min) (point-max))
	(setq result
	      (if (equal (substring (current-message) 0 3) "=> ")
		  (replace-regexp-in-string "^=> " "" (current-message))
		"\"An error occurred.\""))
	(when (not repl)
	  (save-current-buffer (set-buffer repl-buffer)
			       (geiser-repl-exit))
	  (set-process-query-on-exit-flag (get-buffer-process repl-buffer) nil)
	  (kill-buffer repl-buffer))
	(setq result (if (or (string= result "#<void>")
			     (string= result "#<unspecified>"))
			 nil
		       (read result)))))
    result))

(defun org-babel-execute:scheme (body params)
  "Execute a block of Scheme code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((source-buffer (current-buffer))
	 (source-buffer-name (replace-regexp-in-string ;; zap surrounding *
			      "^ ?\\*\\([^*]+\\)\\*" "\\1"
			      (buffer-name source-buffer))))
    (save-excursion
      (org-babel-reassemble-table
       (let* ((result-type (cdr (assoc :result-type params)))
	      (impl (or (when (cdr (assoc :scheme params))
			  (intern (cdr (assoc :scheme params))))
			geiser-default-implementation
			(car geiser-active-implementations)))
	      (session (org-babel-scheme-make-session-name
			source-buffer-name (cdr (assoc :session params)) impl))
	      (full-body (org-babel-expand-body:scheme body params)))
	 (org-babel-scheme-execute-with-geiser
	  full-body			 ; code
	  (string= result-type "output") ; output?
	  impl				 ; implementation
	  (and (not (string= session "none")) session))) ; session
       (org-babel-pick-name (cdr (assoc :colname-names params))
			    (cdr (assoc :colnames params)))
       (org-babel-pick-name (cdr (assoc :rowname-names params))
			    (cdr (assoc :rownames params)))))))

(provide 'ob-scheme)

;;; ob-scheme.el ends here
