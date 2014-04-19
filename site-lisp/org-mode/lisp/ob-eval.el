;;; ob-eval.el --- org-babel functions for external code evaluation

;; Copyright (C) 2009-2014 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; These functions build existing Emacs support for executing external
;; shell commands.

;;; Code:
(require 'org-macs)
(eval-when-compile (require 'cl))

(defvar org-babel-error-buffer-name "*Org-Babel Error Output*")
(declare-function org-babel-temp-file "ob-core" (prefix &optional suffix))

(defun org-babel-eval-error-notify (exit-code stderr)
  "Open a buffer to display STDERR and a message with the value of EXIT-CODE."
  (let ((buf (get-buffer-create org-babel-error-buffer-name)))
    (with-current-buffer buf
      (goto-char (point-max))
      (save-excursion (insert stderr)))
    (display-buffer buf))
  (message "Babel evaluation exited with code %S" exit-code))

(defun org-babel-eval (cmd body)
  "Run CMD on BODY.
If CMD succeeds then return its results, otherwise display
STDERR with `org-babel-eval-error-notify'."
  (let ((err-buff (get-buffer-create " *Org-Babel Error*")) exit-code)
    (with-current-buffer err-buff (erase-buffer))
    (with-temp-buffer
      (insert body)
      (setq exit-code
	    (org-babel--shell-command-on-region
	     (point-min) (point-max) cmd err-buff))
      (if (or (not (numberp exit-code)) (> exit-code 0))
	  (progn
	    (with-current-buffer err-buff
	      (org-babel-eval-error-notify exit-code (buffer-string)))
	    nil)
	(buffer-string)))))

(defun org-babel-eval-read-file (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer (insert-file-contents file)
		    (buffer-string)))

(defun org-babel--shell-command-on-region (start end command error-buffer)
  "Execute COMMAND in an inferior shell with region as input.

Stripped down version of shell-command-on-region for internal use
in Babel only.  This lets us work around errors in the original
function in various versions of Emacs.
"
  (let ((input-file (org-babel-temp-file "ob-input-"))
	(error-file (if error-buffer (org-babel-temp-file "ob-error-") nil))
	;; Unfortunately, `executable-find' does not support file name
	;; handlers.  Therefore, we could use it in the local case
	;; only.
	(shell-file-name
	 (cond ((and (not (file-remote-p default-directory))
		     (executable-find shell-file-name))
		shell-file-name)
	       ((file-executable-p
		 (concat (file-remote-p default-directory) shell-file-name))
		shell-file-name)
	       ("/bin/sh")))
	exit-status)
    ;; There is an error in `process-file' when `error-file' exists.
    ;; This is fixed in Emacs trunk as of 2012-12-21; let's use this
    ;; workaround for now.
    (unless (file-remote-p default-directory)
      (delete-file error-file))
    ;; we always call this with 'replace, remove conditional
    ;; Replace specified region with output from command.
    (let ((swap (< start end)))
      (goto-char start)
      (push-mark (point) 'nomsg)
      (write-region start end input-file)
      (delete-region start end)
      (setq exit-status
	    (process-file shell-file-name input-file
			  (if error-file
			      (list t error-file)
			    t)
			  nil shell-command-switch command))
      (when swap (exchange-point-and-mark)))

    (when (and input-file (file-exists-p input-file)
	       ;; bind org-babel--debug-input around the call to keep
	       ;; the temporary input files available for inspection
	       (not (when (boundp 'org-babel--debug-input)
		      org-babel--debug-input)))
      (delete-file input-file))

    (when (and error-file (file-exists-p error-file))
      (if (< 0 (nth 7 (file-attributes error-file)))
	  (with-current-buffer (get-buffer-create error-buffer)
	    (let ((pos-from-end (- (point-max) (point))))
	      (or (bobp)
		  (insert "\f\n"))
	      ;; Do no formatting while reading error file,
	      ;; because that can run a shell command, and we
	      ;; don't want that to cause an infinite recursion.
	      (format-insert-file error-file nil)
	      ;; Put point after the inserted errors.
	      (goto-char (- (point-max) pos-from-end)))
	    (current-buffer)))
      (delete-file error-file))
    exit-status))

(defun org-babel-eval-wipe-error-buffer ()
  "Delete the contents of the Org code block error buffer.
This buffer is named by `org-babel-error-buffer-name'."
  (when (get-buffer org-babel-error-buffer-name)
    (with-current-buffer org-babel-error-buffer-name
      (delete-region (point-min) (point-max)))))

(provide 'ob-eval)



;;; ob-eval.el ends here
