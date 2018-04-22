;;; ob-clojure.el --- Babel Functions for Clojure    -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017 Free Software Foundation, Inc.

;; Author: Joel Boehland, Eric Schulte, Oleh Krehel, Frederick Giasson
;;
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

;; Support for evaluating clojure code

;; Requirements:

;; - clojure (at least 1.2.0)
;; - clojure-mode
;; - either cider or SLIME

;; For Cider, see https://github.com/clojure-emacs/cider

;; For SLIME, the best way to install these components is by following
;; the directions as set out by Phil Hagelberg (Technomancy) on the
;; web page: http://technomancy.us/126

;;; Code:
(require 'cl-lib)
(require 'ob)

(declare-function cider-current-connection "ext:cider-client" (&optional type))
(declare-function cider-current-ns "ext:cider-client" ())
(declare-function nrepl--merge "ext:nrepl-client" (dict1 dict2))
(declare-function nrepl-dict-get "ext:nrepl-client" (dict key))
(declare-function nrepl-dict-put "ext:nrepl-client" (dict key value))
(declare-function nrepl-request:eval "ext:nrepl-client"
		  (input callback connection &optional session ns line column additional-params))
(declare-function nrepl-sync-request:eval "ext:nrepl-client"
		  (input connection session &optional ns))
(declare-function org-trim "org" (s &optional keep-lead))
(declare-function slime-eval "ext:slime" (sexp &optional package))

(defvar nrepl-sync-request-timeout)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

(defvar org-babel-default-header-args:clojure '())
(defvar org-babel-header-args:clojure '((package . :any)))

(defcustom org-babel-clojure-sync-nrepl-timeout 10
  "Timeout value, in seconds, of a Clojure sync call.
If the value is nil, timeout is disabled."
  :group 'org-babel
  :type 'integer
  :version "26.1"
  :package-version '(Org . "9.1")
  :safe #'wholenump)

(defcustom org-babel-clojure-backend
  (cond ((featurep 'cider) 'cider)
	(t 'slime))
  "Backend used to evaluate Clojure code blocks."
  :group 'org-babel
  :type '(choice
	  (const :tag "cider" cider)
	  (const :tag "SLIME" slime)))

(defun org-babel-expand-body:clojure (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let* ((vars (org-babel--get-vars params))
	 (result-params (cdr (assq :result-params params)))
	 (print-level nil) (print-length nil)
	 (body (org-trim
		(if (null vars) (org-trim body)
		  (concat "(let ["
			  (mapconcat
			   (lambda (var)
			     (format "%S (quote %S)" (car var) (cdr var)))
			   vars "\n      ")
			  "]\n" body ")")))))
    (if (or (member "code" result-params)
	    (member "pp" result-params))
	(format "(clojure.pprint/pprint (do %s))" body)
      body)))

(defun org-babel-execute:clojure (body params)
  "Execute a block of Clojure code with Babel.
The underlying process performed by the code block can be output
using the :show-process parameter."
  (let ((expanded (org-babel-expand-body:clojure body params))
	(response (list 'dict))
        result)
    (cl-case org-babel-clojure-backend
      (cider
       (require 'cider)
       (let ((result-params (cdr (assq :result-params params)))
	     (show (cdr (assq :show-process params))))
         (if (member show '(nil "no"))
	     ;; Run code without showing the process.
	     (progn
	       (setq response
		     (let ((nrepl-sync-request-timeout
			    org-babel-clojure-sync-nrepl-timeout))
		       (nrepl-sync-request:eval expanded
						(cider-current-connection)
						(cider-current-ns))))
	       (setq result
		     (concat
		      (nrepl-dict-get response
				      (if (or (member "output" result-params)
					      (member "pp" result-params))
					  "out"
					"value"))
		      (nrepl-dict-get response "ex")
		      (nrepl-dict-get response "root-ex")
		      (nrepl-dict-get response "err"))))
	   ;; Show the process in an output buffer/window.
           (let ((process-buffer (switch-to-buffer-other-window
				  "*Clojure Show Process Sub Buffer*"))
		 status)
	     ;; Run the Clojure code in nREPL.
	     (nrepl-request:eval
	      expanded
	      (lambda (resp)
		(when (member "out" resp)
		  ;; Print the output of the nREPL in the output buffer.
		  (princ (nrepl-dict-get resp "out") process-buffer))
		(when (member "ex" resp)
		  ;; In case there is an exception, then add it to the
		  ;; output buffer as well.
		  (princ (nrepl-dict-get resp "ex") process-buffer)
		  (princ (nrepl-dict-get resp "root-ex") process-buffer))
		(when (member "err" resp)
		  ;; In case there is an error, then add it to the
		  ;; output buffer as well.
		  (princ (nrepl-dict-get resp "err") process-buffer))
		(nrepl--merge response resp)
		;; Update the status of the nREPL output session.
		(setq status (nrepl-dict-get response "status")))
	      (cider-current-connection)
	      (cider-current-ns))

	     ;; Wait until the nREPL code finished to be processed.
	     (while (not (member "done" status))
	       (nrepl-dict-put response "status" (remove "need-input" status))
	       (accept-process-output nil 0.01)
	       (redisplay))

	     ;; Delete the show buffer & window when the processing is
	     ;; finalized.
	     (mapc #'delete-window
		   (get-buffer-window-list process-buffer nil t))
	     (kill-buffer process-buffer)

	     ;; Put the output or the value in the result section of
	     ;; the code block.
	     (setq result
		   (concat
		    (nrepl-dict-get response
				    (if (or (member "output" result-params)
					    (member "pp" result-params))
					"out"
				      "value"))
		    (nrepl-dict-get response "ex")
		    (nrepl-dict-get response "root-ex")
		    (nrepl-dict-get response "err")))))))
      (slime
       (require 'slime)
       (with-temp-buffer
	 (insert expanded)
	 (setq result
	       (slime-eval
		`(swank:eval-and-grab-output
		  ,(buffer-substring-no-properties (point-min) (point-max)))
		(cdr (assq :package params)))))))
    (org-babel-result-cond (cdr (assq :result-params params))
      result
      (condition-case nil (org-babel-script-escape result)
	(error result)))))

(provide 'ob-clojure)

;;; ob-clojure.el ends here
