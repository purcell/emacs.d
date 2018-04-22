;;; ob-comint.el --- Babel Functions for Interaction with Comint Buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research, comint
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

;; These functions build on comint to ease the sending and receiving
;; of commands and results from comint buffers.

;; Note that the buffers in this file are analogous to sessions in
;; org-babel at large.

;;; Code:
(require 'ob-core)
(require 'org-compat)
(require 'comint)

(defun org-babel-comint-buffer-livep (buffer)
  "Check if BUFFER is a comint buffer with a live process."
  (let ((buffer (if buffer (get-buffer buffer))))
    (and buffer (buffer-live-p buffer) (get-buffer-process buffer) buffer)))

(defmacro org-babel-comint-in-buffer (buffer &rest body)
  "Check BUFFER and execute BODY.
BUFFER is checked with `org-babel-comint-buffer-livep'.  BODY is
executed inside the protection of `save-excursion' and
`save-match-data'."
  (declare (indent 1))
  `(progn
     (unless (org-babel-comint-buffer-livep ,buffer)
       (error "Buffer %s does not exist or has no process" ,buffer))
     (save-match-data
       (with-current-buffer ,buffer
	 (save-excursion
	   (let ((comint-input-filter (lambda (_input) nil)))
	     ,@body))))))
(def-edebug-spec org-babel-comint-in-buffer (form body))

(defmacro org-babel-comint-with-output (meta &rest body)
  "Evaluate BODY in BUFFER and return process output.
Will wait until EOE-INDICATOR appears in the output, then return
all process output.  If REMOVE-ECHO and FULL-BODY are present and
non-nil, then strip echo'd body from the returned output.  META
should be a list containing the following where the last two
elements are optional.

 (BUFFER EOE-INDICATOR REMOVE-ECHO FULL-BODY)

This macro ensures that the filter is removed in case of an error
or user `keyboard-quit' during execution of body."
  (declare (indent 1))
  (let ((buffer (nth 0 meta))
	(eoe-indicator (nth 1 meta))
	(remove-echo (nth 2 meta))
	(full-body (nth 3 meta)))
    `(org-babel-comint-in-buffer ,buffer
       (let* ((string-buffer "")
	      (comint-output-filter-functions
	       (cons (lambda (text) (setq string-buffer (concat string-buffer text)))
		     comint-output-filter-functions))
	      dangling-text)
	 ;; got located, and save dangling text
	 (goto-char (process-mark (get-buffer-process (current-buffer))))
	 (let ((start (point))
	       (end (point-max)))
	   (setq dangling-text (buffer-substring start end))
	   (delete-region start end))
	 ;; pass FULL-BODY to process
	 ,@body
	 ;; wait for end-of-evaluation indicator
	 (while (progn
		  (goto-char comint-last-input-end)
		  (not (save-excursion
			 (and (re-search-forward
			       (regexp-quote ,eoe-indicator) nil t)
			      (re-search-forward
			       comint-prompt-regexp nil t)))))
	   (accept-process-output (get-buffer-process (current-buffer)))
	   ;; thought the following this would allow async
	   ;; background running, but I was wrong...
	   ;; (run-with-timer .5 .5 'accept-process-output
	   ;; 		 (get-buffer-process (current-buffer)))
	   )
	 ;; replace cut dangling text
	 (goto-char (process-mark (get-buffer-process (current-buffer))))
	 (insert dangling-text)

	 ;; remove echo'd FULL-BODY from input
	 (when (and ,remove-echo ,full-body
		    (string-match
		     (replace-regexp-in-string
		      "\n" "[\r\n]+" (regexp-quote (or ,full-body "")))
		     string-buffer))
	   (setq string-buffer (substring string-buffer (match-end 0))))
	 (split-string string-buffer comint-prompt-regexp)))))
(def-edebug-spec org-babel-comint-with-output (sexp body))

(defun org-babel-comint-input-command (buffer cmd)
  "Pass CMD to BUFFER.
The input will not be echoed."
  (org-babel-comint-in-buffer buffer
    (goto-char (process-mark (get-buffer-process buffer)))
    (insert cmd)
    (comint-send-input)
    (org-babel-comint-wait-for-output buffer)))

(defun org-babel-comint-wait-for-output (buffer)
  "Wait until output arrives from BUFFER.
Note: this is only safe when waiting for the result of a single
statement (not large blocks of code)."
  (org-babel-comint-in-buffer buffer
    (while (progn
             (goto-char comint-last-input-end)
             (not (and (re-search-forward comint-prompt-regexp nil t)
                       (goto-char (match-beginning 0))
                       (string= (face-name (face-at-point))
                                "comint-highlight-prompt"))))
      (accept-process-output (get-buffer-process buffer)))))

(defun org-babel-comint-eval-invisibly-and-wait-for-file
  (buffer file string &optional period)
  "Evaluate STRING in BUFFER invisibly.
Don't return until FILE exists.  Code in STRING must ensure that
FILE exists at end of evaluation."
  (unless (org-babel-comint-buffer-livep buffer)
    (error "Buffer %s does not exist or has no process" buffer))
  (when (file-exists-p file) (delete-file file))
  (process-send-string
   (get-buffer-process buffer)
   (if (= (aref string (1- (length string))) ?\n) string (concat string "\n")))
  (while (not (file-exists-p file)) (sit-for (or period 0.25))))

(provide 'ob-comint)



;;; ob-comint.el ends here
