;;; sb-gud --- Speedbar support for the Grand Unified Debugger

;; Copyright (C) 1997, 1998, 2001, 2002 Free Software Foundation
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Version: 0.1
;; Keywords: tools, gud
;; X-RCS: $Id: sb-gud.el,v 1.11 2003/09/17 16:58:28 ponced Exp $
;;
;; This file is part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;

;;; Commentary:
;;
;;   Speedbar provides a frame in which files, and locations in
;; files are displayed.  These functions provide gud specific support,
;; showing stacks, files, and the like in the side-bar.
;;
;;   To enable in emacs 20.2 or earlier, add this to your .emacs file.
;;   (autoload 'gud-speedbar-buttons "sb-gud"
;;             "GUD specific speedbar button generator.")
;;
;;   This file requires speedbar.

;;; Change log:
;; 0.1   - First revision
;; 0.1.1 - Removed dependency on speedbspec.
;; 0.1.2 - Changed to handle keymap feature.

(require 'speedbar)

;;; Code:
(defvar gud-last-speedbar-buffer nil
  "The last GUD buffer used.")

(defvar gud-last-speedbar-stackframe nil
  "Description of the currently displayed GUD stack.
t means that there is no stack, and we are in display-file mode.")

(defvar gud-speedbar-key-map nil
  "Keymap used when in the buffers display mode.")

(defun gud-install-speedbar-variables ()
  "Install those variables used by speedbar to enhance gud/gdb."
  (if gud-speedbar-key-map
      nil
    (setq gud-speedbar-key-map (speedbar-make-specialized-keymap))

    (define-key gud-speedbar-key-map "j" 'speedbar-edit-line)
    (define-key gud-speedbar-key-map "e" 'speedbar-edit-line)
    (define-key gud-speedbar-key-map "\C-m" 'speedbar-edit-line)))

(defvar gud-speedbar-menu-items
  ;; Note to self.  Add expand, and turn off items when not available.
  '(["Jump to stack frame" speedbar-edit-line t])
  "Additional menu items to add the the speedbar frame.")

;; Make sure our special speedbar mode is loaded
(if (featurep 'speedbar)
    (gud-install-speedbar-variables)
  (add-hook 'speedbar-load-hook 'gud-install-speedbar-variables))

;;;###autoload
(defun gud-speedbar-buttons (buffer)
  "Create a speedbar display based on the current state of GUD.
If the GUD BUFFER is not running a supported debugger, then turn
off the specialized speedbar mode."
  (if (and (save-excursion (goto-char (point-min))
			   (looking-at "\\(//\\)?Current Stack"))
	   (equal gud-last-last-frame gud-last-speedbar-stackframe))
      nil
    (setq gud-last-speedbar-buffer buffer)
    (let* ((ff (save-excursion (set-buffer buffer) gud-find-file))
	   ;;(lf (save-excursion (set-buffer buffer) gud-last-last-frame))
	   (frames
	    (cond ((eq ff 'gud-gdb-find-file)
		   (gud-gdb-get-stackframe buffer)
		   )
		  ;; Add more debuggers here!
		  (t
		   (speedbar-remove-localized-speedbar-support buffer)
		   nil))))
      (erase-buffer)
      (if (not frames)
	  (speedbar-insert-label "No Stack frames")
	(speedbar-insert-label "Current Stack:"))
      (while frames
	(insert (nth 1 (car frames)) ":\n")
	(if (= (length (car frames)) 2)
	    (progn
;	      (speedbar-insert-button "[?]"
;				      'speedbar-button-face
;				      nil nil nil t)
	      (speedbar-insert-button (car (car frames))
				      'speedbar-directory-face
				      nil nil nil t))
;	  (speedbar-insert-button "[+]"
;				  'speedbar-button-face
;				  'speedbar-highlight-face
;				  'gud-gdb-get-scope-data
;				  (car frames) t)
	  (speedbar-insert-button (car (car frames))
				  'speedbar-file-face
				  'speedbar-highlight-face
				  (cond ((eq ff 'gud-gdb-find-file)
					 'gud-gdb-goto-stackframe)
					(t (error "Should never be here")))
				  (car frames) t))
	(setq frames (cdr frames)))
      (let ((selected-frame
	     (cond ((eq ff 'gud-gdb-find-file)
		    (gud-gdb-selected-frame-info buffer))
		   (t (error "Should never be here")))))))
    (setq gud-last-speedbar-stackframe gud-last-last-frame)))

(defun gud-gdb-goto-stackframe (text token indent)
  "Goto the stackframe described by TEXT, TOKEN, and INDENT."
  (speedbar-with-attached-buffer
   (gud-basic-call (concat "frame " (nth 1 token)))
   (sit-for 1)))

(defvar gud-gdb-fetched-stack-frame nil
  "Stack frames we are fetching from GDB.")

(defvar gud-gdb-fetched-stack-frame-list nil
  "List of stack frames we are fetching from GDB.")

;(defun gud-gdb-get-scope-data (text token indent)
;  ;; checkdoc-params: (indent)
;  "Fetch data associated with a stack frame, and expand/contract it.
;Data to do this is retrieved from TEXT and TOKEN."
;  (let ((args nil) (scope nil))
;    (gud-gdb-run-command-fetch-lines "info args")
;
;    (gud-gdb-run-command-fetch-lines "info local")
;
;    ))

(defun gud-gdb-get-stackframe (buffer)
  "Extract the current stack frame out of the GUD GDB BUFFER."
  (let ((newlst nil)
	(gud-gdb-fetched-stack-frame-list nil))
    (gud-gdb-run-command-fetch-lines "backtrace" buffer)
    (if (and (car gud-gdb-fetched-stack-frame-list)
	     (string-match "No stack" (car gud-gdb-fetched-stack-frame-list)))
	;; Go into some other mode???
	nil
      (while gud-gdb-fetched-stack-frame-list
	(let ((e (car gud-gdb-fetched-stack-frame-list))
	      (name nil) (num nil))
	  (if (not (or
		    (string-match "^#\\([0-9]+\\) +[0-9a-fx]+ in \\([:0-9a-zA-Z_]+\\) (" e)
		    (string-match "^#\\([0-9]+\\) +\\([:0-9a-zA-Z_]+\\) (" e)))
	      (if (not (string-match
			"at \\([-0-9a-zA-Z_.]+\\):\\([0-9]+\\)$" e))
		  nil
		(setcar newlst
			(list (nth 0 (car newlst))
			      (nth 1 (car newlst))
			      (match-string 1 e)
			      (match-string 2 e))))
	    (setq num (match-string 1 e)
		  name (match-string 2 e))
	    (setq newlst
		  (cons
		   (if (string-match
			"at \\([-0-9a-zA-Z_.]+\\):\\([0-9]+\\)$" e)
		       (list name num (match-string 1 e)
			     (match-string 2 e))
		     (list name num))
		   newlst))))
	(setq gud-gdb-fetched-stack-frame-list
	      (cdr gud-gdb-fetched-stack-frame-list)))
      (nreverse newlst))))

;(defun gud-gdb-selected-frame-info (buffer)
;  "Learn GDB information for the currently selected stack frame in BUFFER."
;  )

(defun gud-gdb-run-command-fetch-lines (command buffer)
  "Run COMMAND, and return when `gud-gdb-fetched-stack-frame-list' is full.
BUFFER is the GUD buffer in which to run the command."
  (save-excursion
    (set-buffer buffer)
    (if (save-excursion
	  (goto-char (point-max))
	  (beginning-of-line)
	  (not (looking-at comint-prompt-regexp)))
	nil
      ;; Much of this copied from GDB complete, but I'm grabbing the stack
      ;; frame instead.
      (let ((gud-marker-filter 'gud-gdb-speedbar-stack-filter))
	;; Issue the command to GDB.
	(gud-basic-call command)
	(setq gud-gdb-complete-in-progress t ;; use this flag for our purposes.
	      gud-gdb-complete-string nil
	      gud-gdb-complete-list nil)
	;; Slurp the output.
	(while gud-gdb-complete-in-progress
	  (accept-process-output (get-buffer-process gud-comint-buffer)))
	(setq gud-gdb-fetched-stack-frame nil
	      gud-gdb-fetched-stack-frame-list
	      (nreverse gud-gdb-fetched-stack-frame-list))))))
  
(defun gud-gdb-speedbar-stack-filter (string)
  ;; checkdoc-params: (string)
  "Filter used to read in the current GDB stack."
  (setq string (concat gud-gdb-fetched-stack-frame string))
  (while (string-match "\n" string)
    (setq gud-gdb-fetched-stack-frame-list
	  (cons (substring string 0 (match-beginning 0))
		gud-gdb-fetched-stack-frame-list))
    (setq string (substring string (match-end 0))))
  (if (string-match comint-prompt-regexp string)
      (progn
	(setq gud-gdb-complete-in-progress nil)
	string)
    (progn
      (setq gud-gdb-complete-string string)
      "")))

(provide 'sb-gud)
;;; sb-gud.el ends here
