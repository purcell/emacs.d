;;; org-indent.el --- Dynamic indentation for Org    -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This is an implementation of dynamic virtual indentation.  It works
;; by adding text properties to a buffer to make sure lines are
;; indented according to outline structure.
;;
;; The process is synchronous, toggled at every buffer modification.
;; Though, the initialization (indentation of text already in the
;; buffer), which can take a few seconds in large buffers, happens on
;; idle time.
;;
;;; Code:

(require 'org-macs)
(require 'org-compat)
(require 'org)

(require 'cl-lib)

(declare-function org-inlinetask-get-task-level "org-inlinetask" ())
(declare-function org-inlinetask-in-task-p "org-inlinetask" ())
(declare-function org-list-item-body-column "org-list" (item))
(defvar org-inlinetask-show-first-star)

(defgroup org-indent nil
  "Options concerning dynamic virtual outline indentation."
  :tag "Org Indent"
  :group 'org)

(defvar org-indent-inlinetask-first-star (org-add-props "*" '(face org-warning))
  "First star of inline tasks, with correct face.")
(defvar org-indent-agent-timer nil
  "Timer running the initialize agent.")
(defvar org-indent-agentized-buffers nil
  "List of buffers watched by the initialize agent.")
(defvar org-indent-agent-resume-timer nil
  "Timer to reschedule agent after switching to other idle processes.")
(defvar org-indent-agent-active-delay '(0 2 0)
  "Time to run agent before switching to other idle processes.
Delay used when the buffer to initialize is current.")
(defvar org-indent-agent-passive-delay '(0 0 400000)
  "Time to run agent before switching to other idle processes.
Delay used when the buffer to initialize isn't current.")
(defvar org-indent-agent-resume-delay '(0 0 100000)
  "Minimal time for other idle processes before switching back to agent.")
(defvar org-indent--initial-marker nil
  "Position of initialization before interrupt.
This is used locally in each buffer being initialized.")
(defvar org-hide-leading-stars-before-indent-mode nil
  "Used locally.")
(defvar org-indent-modified-headline-flag nil
  "Non-nil means the last deletion operated on a headline.
It is modified by `org-indent-notify-modified-headline'.")


(defcustom org-indent-boundary-char ?\s
  "The end of the virtual indentation strings, a single-character string.
The default is just a space, but if you wish, you can use \"|\" or so.
This can be useful on a terminal window - under a windowing system,
it may be prettier to customize the `org-indent' face."
  :group 'org-indent
  :type 'character)

(defcustom org-indent-mode-turns-off-org-adapt-indentation t
  "Non-nil means setting the variable `org-indent-mode' will \
turn off indentation adaptation.
For details see the variable `org-adapt-indentation'."
  :group 'org-indent
  :type 'boolean)

(defcustom org-indent-mode-turns-on-hiding-stars t
  "Non-nil means setting the variable `org-indent-mode' will \
turn on `org-hide-leading-stars'."
  :group 'org-indent
  :type 'boolean)

(defcustom org-indent-indentation-per-level 2
  "Indentation per level in number of characters."
  :group 'org-indent
  :type 'integer)

(defface org-indent '((t (:inherit org-hide)))
  "Face for outline indentation.
The default is to make it look like whitespace.  But you may find it
useful to make it ever so slightly different."
  :group 'org-faces)

(defvar org-indent--text-line-prefixes nil
  "Vector containing line prefixes strings for regular text.")

(defvar org-indent--heading-line-prefixes nil
  "Vector containing line prefix strings for headlines.")

(defvar org-indent--inlinetask-line-prefixes nil
  "Vector containing line prefix strings for inline tasks.")

(defconst org-indent--deepest-level 50
  "Maximum theoretical headline depth.")

(defun org-indent--compute-prefixes ()
  "Compute prefix strings for regular text and headlines."
  (setq org-indent--heading-line-prefixes
	(make-vector org-indent--deepest-level nil))
  (setq org-indent--inlinetask-line-prefixes
	(make-vector org-indent--deepest-level nil))
  (setq org-indent--text-line-prefixes
	(make-vector org-indent--deepest-level nil))
  (dotimes (n org-indent--deepest-level)
    (let ((indentation (if (<= n 1) 0
			 (* (1- org-indent-indentation-per-level)
			    (1- n)))))
      ;; Headlines line prefixes.
      (let ((heading-prefix (make-string indentation ?*)))
	(aset org-indent--heading-line-prefixes
	      n
	      (org-add-props heading-prefix nil 'face 'org-indent))
	;; Inline tasks line prefixes
	(aset org-indent--inlinetask-line-prefixes
	      n
	      (cond ((<= n 1) "")
		    ((bound-and-true-p org-inlinetask-show-first-star)
		     (concat org-indent-inlinetask-first-star
			     (substring heading-prefix 1)))
		    (t (org-add-props heading-prefix nil 'face 'org-indent)))))
      ;; Text line prefixes.
      (aset org-indent--text-line-prefixes
	    n
	    (concat (org-add-props (make-string (+ n indentation) ?\s)
			nil 'face 'org-indent)
		    (and (> n 0)
			 (char-to-string org-indent-boundary-char)))))))

(defsubst org-indent-remove-properties (beg end)
  "Remove indentations between BEG and END."
  (org-with-silent-modifications
   (remove-text-properties beg end '(line-prefix nil wrap-prefix nil))))

;;;###autoload
(define-minor-mode org-indent-mode
  "When active, indent text according to outline structure.

Internally this works by adding `line-prefix' and `wrap-prefix'
properties, after each buffer modification, on the modified zone.

The process is synchronous.  Though, initial indentation of
buffer, which can take a few seconds on large buffers, is done
during idle time."
  nil " Ind" nil
  (cond
   (org-indent-mode
    ;; mode was turned on.
    (setq-local indent-tabs-mode nil)
    (setq-local org-indent--initial-marker (copy-marker 1))
    (when org-indent-mode-turns-off-org-adapt-indentation
      (setq-local org-adapt-indentation nil))
    (when org-indent-mode-turns-on-hiding-stars
      (setq-local org-hide-leading-stars-before-indent-mode
		  org-hide-leading-stars)
      (setq-local org-hide-leading-stars t))
    (org-indent--compute-prefixes)
    (add-hook 'filter-buffer-substring-functions
	      (lambda (fun start end delete)
		(org-indent-remove-properties-from-string
		 (funcall fun start end delete)))
	      nil t)
    (add-hook 'after-change-functions 'org-indent-refresh-maybe nil 'local)
    (add-hook 'before-change-functions
	      'org-indent-notify-modified-headline nil 'local)
    (and font-lock-mode (org-restart-font-lock))
    (org-indent-remove-properties (point-min) (point-max))
    ;; Submit current buffer to initialize agent.  If it's the first
    ;; buffer submitted, also start the agent.  Current buffer is
    ;; pushed in both cases to avoid a race condition.
    (if org-indent-agentized-buffers
	(push (current-buffer) org-indent-agentized-buffers)
      (push (current-buffer) org-indent-agentized-buffers)
      (setq org-indent-agent-timer
	    (run-with-idle-timer 0.2 t #'org-indent-initialize-agent))))
   (t
    ;; mode was turned off (or we refused to turn it on)
    (kill-local-variable 'org-adapt-indentation)
    (setq org-indent-agentized-buffers
	  (delq (current-buffer) org-indent-agentized-buffers))
    (when (markerp org-indent--initial-marker)
      (set-marker org-indent--initial-marker nil))
    (when (boundp 'org-hide-leading-stars-before-indent-mode)
      (setq-local org-hide-leading-stars
		  org-hide-leading-stars-before-indent-mode))
    (remove-hook 'filter-buffer-substring-functions
		 (lambda (fun start end delete)
		   (org-indent-remove-properties-from-string
		    (funcall fun start end delete))))
    (remove-hook 'after-change-functions 'org-indent-refresh-maybe 'local)
    (remove-hook 'before-change-functions
		 'org-indent-notify-modified-headline 'local)
    (org-with-wide-buffer
     (org-indent-remove-properties (point-min) (point-max)))
    (and font-lock-mode (org-restart-font-lock))
    (redraw-display))))

(defun org-indent-indent-buffer ()
  "Add indentation properties to the accessible part of the buffer."
  (interactive)
  (if (not (derived-mode-p 'org-mode))
      (error "Not in Org mode")
    (message "Setting buffer indentation.  It may take a few seconds...")
    (org-indent-remove-properties (point-min) (point-max))
    (org-indent-add-properties (point-min) (point-max))
    (message "Indentation of buffer set.")))

(defun org-indent-remove-properties-from-string (string)
  "Remove indentation properties from STRING."
  (remove-text-properties 0 (length string)
			  '(line-prefix nil wrap-prefix nil) string)
  string)

(defun org-indent-initialize-agent ()
  "Start or resume current buffer initialization.
Only buffers in `org-indent-agentized-buffers' trigger an action.
When no more buffer is being watched, the agent suppress itself."
  (when org-indent-agent-resume-timer
    (cancel-timer org-indent-agent-resume-timer))
  (setq org-indent-agentized-buffers
	(cl-remove-if-not #'buffer-live-p org-indent-agentized-buffers))
  (cond
   ;; Job done:  kill agent.
   ((not org-indent-agentized-buffers) (cancel-timer org-indent-agent-timer))
   ;; Current buffer is agentized: start/resume initialization
   ;; somewhat aggressively.
   ((memq (current-buffer) org-indent-agentized-buffers)
    (org-indent-initialize-buffer (current-buffer)
				  org-indent-agent-active-delay))
   ;; Else, start/resume initialization of the last agentized buffer,
   ;; softly.
   (t (org-indent-initialize-buffer (car org-indent-agentized-buffers)
				    org-indent-agent-passive-delay))))

(defun org-indent-initialize-buffer (buffer delay)
  "Set virtual indentation for the buffer BUFFER, asynchronously.
Give hand to other idle processes if it takes longer than DELAY,
a time value."
  (with-current-buffer buffer
    (when org-indent-mode
      (org-with-wide-buffer
       (let ((interruptp
	      ;; Always nil unless interrupted.
	      (catch 'interrupt
		(and org-indent--initial-marker
		     (marker-position org-indent--initial-marker)
		     (equal (marker-buffer org-indent--initial-marker)
			    buffer)
		     (org-indent-add-properties org-indent--initial-marker
						(point-max)
						delay)
		     nil))))
	 (move-marker org-indent--initial-marker interruptp)
	 ;; Job is complete: un-agentize buffer.
	 (unless interruptp
	   (setq org-indent-agentized-buffers
		 (delq buffer org-indent-agentized-buffers))))))))

(defun org-indent-set-line-properties (level indentation &optional heading)
  "Set prefix properties on current line an move to next one.

LEVEL is the current level of heading.  INDENTATION is the
expected indentation when wrapping line.

When optional argument HEADING is non-nil, assume line is at
a heading.  Moreover, if is is `inlinetask', the first star will
have `org-warning' face."
  (let* ((line (aref (pcase heading
		       (`nil org-indent--text-line-prefixes)
		       (`inlinetask org-indent--inlinetask-line-prefixes)
		       (_ org-indent--heading-line-prefixes))
		     level))
	 (wrap
	  (org-add-props
	      (concat line
		      (if heading (concat (make-string level ?*) " ")
			(make-string indentation ?\s)))
	      nil 'face 'org-indent)))
    ;; Add properties down to the next line to indent empty lines.
    (add-text-properties (line-beginning-position) (line-beginning-position 2)
			 `(line-prefix ,line wrap-prefix ,wrap)))
  (forward-line))

(defun org-indent-add-properties (beg end &optional delay)
  "Add indentation properties between BEG and END.

When DELAY is non-nil, it must be a time value.  In that case,
the process is asynchronous and can be interrupted, either by
user request, or after DELAY.  This is done by throwing the
`interrupt' tag along with the buffer position where the process
stopped."
  (save-match-data
    (org-with-wide-buffer
     (goto-char beg)
     (beginning-of-line)
     ;; Initialize prefix at BEG, according to current entry's level.
     (let* ((case-fold-search t)
	    (limited-re (org-get-limited-outline-regexp))
	    (level (or (org-current-level) 0))
	    (time-limit (and delay (time-add (current-time) delay))))
       ;; For each line, set `line-prefix' and `wrap-prefix'
       ;; properties depending on the type of line (headline, inline
       ;; task, item or other).
       (org-with-silent-modifications
	(while (and (<= (point) end) (not (eobp)))
	  (cond
	   ;; When in asynchronous mode, check if interrupt is
	   ;; required.
	   ((and delay (input-pending-p)) (throw 'interrupt (point)))
	   ;; In asynchronous mode, take a break of
	   ;; `org-indent-agent-resume-delay' every DELAY to avoid
	   ;; blocking any other idle timer or process output.
	   ((and delay (time-less-p time-limit (current-time)))
	    (setq org-indent-agent-resume-timer
		  (run-with-idle-timer
		   (time-add (current-idle-time) org-indent-agent-resume-delay)
		   nil #'org-indent-initialize-agent))
	    (throw 'interrupt (point)))
	   ;; Headline or inline task.
	   ((looking-at org-outline-regexp)
	    (let* ((nstars (- (match-end 0) (match-beginning 0) 1))
		   (type (or (looking-at-p limited-re) 'inlinetask)))
	      (org-indent-set-line-properties nstars 0 type)
	      ;; At an headline, define new value for LEVEL.
	      (unless (eq type 'inlinetask) (setq level nstars))))
	   ;; List item: `wrap-prefix' is set where body starts.
	   ((org-at-item-p)
	    (org-indent-set-line-properties
	     level (org-list-item-body-column (point))))
	   ;; Regular line.
	   (t
	    (org-indent-set-line-properties level (org-get-indentation))))))))))

(defun org-indent-notify-modified-headline (beg end)
  "Set `org-indent-modified-headline-flag' depending on context.

BEG and END are the positions of the beginning and end of the
range of deleted text.

This function is meant to be called by `before-change-functions'.
Flag will be non-nil if command is going to modify or delete an
headline."
  (when org-indent-mode
    (setq org-indent-modified-headline-flag
	  (org-with-wide-buffer
	   (goto-char beg)
	   (save-match-data
	     (or (and (org-at-heading-p) (< beg (match-end 0)))
		 (re-search-forward
		  (org-with-limited-levels org-outline-regexp-bol) end t)))))))

(defun org-indent-refresh-maybe (beg end _)
  "Refresh indentation properties in an adequate portion of buffer.
BEG and END are the positions of the beginning and end of the
range of inserted text.  DUMMY is an unused argument.

This function is meant to be called by `after-change-functions'."
  (when org-indent-mode
    (save-match-data
      ;; If a headline was modified or inserted, set properties until
      ;; next headline.
      (org-with-wide-buffer
       (if (or org-indent-modified-headline-flag
	       (save-excursion
		 (goto-char beg)
		 (beginning-of-line)
		 (re-search-forward
		  (org-with-limited-levels org-outline-regexp-bol) end t)))
	   (let ((end (save-excursion
			(goto-char end)
			(org-with-limited-levels (outline-next-heading))
			(point))))
	     (setq org-indent-modified-headline-flag nil)
	     (org-indent-add-properties beg end))
	 ;; Otherwise, only set properties on modified area.
	 (org-indent-add-properties beg end))))))

(provide 'org-indent)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-indent.el ends here
