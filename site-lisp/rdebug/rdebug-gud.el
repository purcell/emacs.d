;;; rdebug-gud.el --- rdebug interface to gud.

;; Copyright (C) 2007, 2008 Rocky Bernstein (rocky@gnu.org)
;; Copyright (C) 2007, 2008 Anders Lindgren

;; $Id: rdebug-gud.el 821 2008-04-25 02:54:44Z rockyb $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; See the manual and the file `rdebug.el' for more information.

;; This file contains rdebug Emacs routines which interface with gud.

;;; Code:

;; -------------------------------------------------------------------
;; Dependencies.
;;

(require 'gud)
(require 'rdebug-error)
(require 'rdebug-fns)
(require 'rdebug-regexp)
(require 'rdebug-vars)


(defun gud-rdebug-massage-args (file args)
  "Change FILE and list ARGS before running the debugger.

gud requires this routine to be defined."
  args)


(defun gud-rdebug-find-file (file)
  "`rdebug' and `gud' call this with FILE when they encounter a Ruby program file."

  (find-file-noselect file 'nowarn))

(defun rdebug-display-line (file line &optional move-arrow)
  "Arrange for marker to appear in at FILE and LINE.
The line marker might appear in the Emacs fringe or as an overlay arroe. 
Optional argument MOVE-ARROW indicates whether to move any previous indicator."
  (if file 
      (let ((oldpos (and gud-overlay-arrow-position
			 (marker-position gud-overlay-arrow-position)))
	    (oldbuf (and gud-overlay-arrow-position
			 (marker-buffer gud-overlay-arrow-position))))
	(gud-display-line file line)
	(unless move-arrow
	  (when gud-overlay-arrow-position
	    (set-marker gud-overlay-arrow-position oldpos oldbuf))))))

(defun rdebug-stepping (step-or-next &optional arg)
  (or arg (setq arg 1))
  ;;(if (not (member '('rdebug-next 'rdebug-step 'digit-argument) last-command))
  ;; (setq rdebug-stepping-prefix ""))
  (unless (member rdebug-stepping-prefix '("" "+" "-"))
    (setq rdebug-stepping-prefix ""))
  (rdebug-call (format "%s%s %d" step-or-next rdebug-stepping-prefix arg)))



;; -------------------------------------------------------------------
;; Rdebug commands.
;;

(defun rdebug-call (cmd)
  "Run a debugger command with some error checking."
  (if (rdebug-dead-process-p)
      (rdebug-errmsg 
       "Can't find a live debugger process buffer to feed the command to.")
    (gud-call cmd)))

(defun rdebug-call-return (cmd &rest options)
  "Register a debugger command whose output should be handled specially.

OPTIONS is zero or more indicators what should happen with the
output. The following are supported:
 * :tooltip -- Show the result in a tool-tip.
 * :info    -- Show the result in the info secondary buffer."
  (if (rdebug-dead-process-p)
      (rdebug-errmsg 
       "Can't find a live debugger process buffer to feed the command to.")
    (with-current-buffer gud-comint-buffer
      (setq rdebug-call-queue
	    (append rdebug-call-queue (list (cons cmd options))))
      (gud-call cmd))))

(defun rdebug-continue (&optional arg)
  "Run a debugger \"continue\" command.

With a numeric ARG, continue to that line number of the current file."
  (interactive "p")
  (if arg
      (rdebug-call (format "continue %d" arg))
    (rdebug-call (format "continue"))))


(defun rdebug-next (&optional arg)
  "Run a debugger \"next\" command, respecting `rdebug-stepping-prefix'.

With a numeric ARG, continue to that line number of the current file."
  (interactive "p")
  (rdebug-stepping "next" arg))

(defvar rdebug-stepping-prefix ""
  "The kind of stepping modifier that is desired.

This variable will have a string value which is either \"\",
\"+\", or \"-\"; this string is be appended to the debugger
stepping commands (\"next\", or \"step\").")

(defun rdebug-print-cmd (expr &optional cmd)
  "Run a debugger print (pl, ps, pp, p) command on `EXPR'; `CMD' is the command to run."
  (interactive "s")
  (unless cmd (setq cmd "pp"))
  (rdebug-call-return (format "%s %s " cmd expr) :tooltip))

(defun rdebug-print-list-region (from to)
  "Run a debugger \"pl\" command on the marked region."
  (interactive "r")
  (if (> from to)
      (let ((tem to))
	(setq to from from tem)))
  (rdebug-print-cmd (buffer-substring from to) "pl"))

(defun rdebug-print-region (from to)
  "Run a debugger \"p\" command on the marked region."
  (interactive "r")
  (if (> from to)
      (let ((tem to))
	(setq to from from tem)))
  (rdebug-print-cmd (buffer-substring from to) "p"))

(defun rdebug-print-sorted-region (from to)
  "Run a debugger \"ps\" command on the marked region."
  (interactive "r")
  (if (> from to)
      (let ((tem to))
	(setq to from from tem)))
  (rdebug-print-cmd (buffer-substring from to) "ps"))

(defun rdebug-pretty-print-region (from to)
  "Run a debugger \"pp\" command on the marked region."
  (interactive "r")
  (if (> from to)
      (let ((tem to))
	(setq to from from tem)))
  (rdebug-print-cmd (buffer-substring from to) "pp"))


;;
;; The following two commands should be seen as proof-of-concept
;; functions for the info buffer.
;;

(defun rdebug-pretty-print-to-buffer (s)
  "Pretty print expression to the info buffer."
  (interactive "sPretty print: ")
  (rdebug-call-return (format "pp %s" s) :info))

(defun rdebug-pretty-print-region-to-buffer (from to)
  "Pretty print expression in region to the info buffer."
  (interactive "r")
  (rdebug-call-return (format "pp %s" (buffer-substring from to)) :info))



(defun rdebug-quit ()
  "Kill the debugger process associated with the current buffer.

When `rdebug-many-windows' is active, the original window layout
is restored."
  (interactive)
  (if (yes-or-no-p "Really quit? ")
      (rdebug-call "quit unconditionally")))

(defun rdebug-restart ()
  "Restart the debugged Ruby script.

An exec restart is used."
  (interactive)
  (if (yes-or-no-p "Restart? ")
      (rdebug-call "restart")))

(defun rdebug-set-stepping-prefix ()
  "Set the granularity of stepping on the subsequent 'next' or 'step' command.
As long as repeated next or step commands are given, they inherit this setting."
  (interactive)
  (setq rdebug-stepping-prefix (this-command-keys)))

(defun rdebug-step (&optional arg)
  "Run a debugger \"next\" command, respecting `rdebug-stepping-prefix'.

With a numeric ARG, continue to that line number of the current file."
  (interactive "p")
  (rdebug-stepping "step" arg))

(defun rdebug-newer-frame ()
  "Run a debugger \"down\" command to an newer frame. 

If we try to go down from frame 0, wrap to the end of the file"
  (interactive)
  (let* ((buf-name (rdebug-get-secondary-buffer-name "frame"))
         (buf (or (get-buffer buf-name) (current-buffer))))
    (with-current-buffer buf
      ;; Should we add a mode to disable wrapping?
      (if (equal rdebug-frames-current-frame-number 0)
	  (rdebug-call "frame -1")
	(rdebug-call "down 1")))))

(defun rdebug-older-frame ()
  "Run a debugger \"up\" command to an older frame."
  (interactive)
  (let* ((buf-name (rdebug-get-secondary-buffer-name "frame"))
         (buf (or (get-buffer buf-name) (current-buffer))))
    (with-current-buffer buf
      ;; Should we add a mode to disable wrapping? 
      (rdebug-call "up 1"))))

(provide 'rdebug-gud)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-gud.el ends here
