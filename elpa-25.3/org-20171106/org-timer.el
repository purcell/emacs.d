;;; org-timer.el --- Timer code for Org mode         -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2017 Free Software Foundation, Inc.

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

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file implements two types of timers for Org buffers:
;;
;; - A relative timer that counts up (from 0 or a specified offset)
;; - A countdown timer that counts down from a specified time
;;
;; The relative and countdown timers differ in their entry points.
;; Use `org-timer' or `org-timer-start' to start the relative timer,
;; and `org-timer-set-timer' to start the countdown timer.

;;; Code:

(require 'cl-lib)
(require 'org-clock)

(declare-function org-agenda-error "org-agenda" ())

(defvar org-timer-start-time nil
  "t=0 for the running timer.")

(defvar org-timer-pause-time nil
  "Time when the timer was paused.")

(defvar org-timer-countdown-timer nil
  "Current countdown timer.
This is a timer object if there is an active countdown timer,
`paused' if there is a paused countdown timer, and nil
otherwise.")

(defvar org-timer-countdown-timer-title nil
  "Title for notification displayed when a countdown finishes.")

(defconst org-timer-re "\\([-+]?[0-9]+\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)"
  "Regular expression used to match timer stamps.")

(defcustom org-timer-format "%s "
  "The format to insert the time of the timer.
This format must contain one instance of \"%s\" which will be replaced by
the value of the timer."
  :group 'org-time
  :type 'string)

(defcustom org-timer-default-timer "0"
  "The default timer when a timer is set, in minutes or hh:mm:ss format.
When 0, the user is prompted for a value."
  :group 'org-time
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'string)

(defcustom org-timer-display 'mode-line
  "Define where running timer is displayed, if at all.
When a timer is running, Org can display it in the mode line
and/or frame title.  Allowed values are:

both         displays in both mode line and frame title
mode-line    displays only in mode line (default)
frame-title  displays only in frame title
nil          current timer is not displayed"
  :group 'org-time
  :type '(choice
	  (const :tag "Mode line" mode-line)
	  (const :tag "Frame title" frame-title)
	  (const :tag "Both" both)
	  (const :tag "None" nil)))

(defvar org-timer-start-hook nil
  "Hook run after relative timer is started.")

(defvar org-timer-stop-hook nil
  "Hook run before relative or countdown timer is stopped.")

(defvar org-timer-pause-hook nil
  "Hook run before relative or countdown timer is paused.")

(defvar org-timer-continue-hook nil
  "Hook run after relative or countdown timer is continued.")

(defvar org-timer-set-hook nil
  "Hook run after countdown timer is set.")

(defvar org-timer-done-hook nil
  "Hook run after countdown timer reaches zero.")

;;;###autoload
(defun org-timer-start (&optional offset)
  "Set the starting time for the relative timer to now.
When called with prefix argument OFFSET, prompt the user for an offset time,
with the default taken from a timer stamp at point, if any.
If OFFSET is a string or an integer, it is directly taken to be the offset
without user interaction.
When called with a double prefix arg, all timer strings in the active
region will be shifted by a specific amount.  You will be prompted for
the amount, with the default to make the first timer string in
the region 0:00:00."
  (interactive "P")
  (cond
   ((equal offset '(16))
    (call-interactively 'org-timer-change-times-in-region))
   (org-timer-countdown-timer
    (user-error "Countdown timer is running.  Cancel first"))
   (t
    (let (delta def s)
      (if (not offset)
	  (setq org-timer-start-time (current-time))
	(cond
	 ((integerp offset) (setq delta offset))
	 ((stringp offset) (setq delta (org-timer-hms-to-secs offset)))
	 (t
	  (setq def (if (org-in-regexp org-timer-re)
			(match-string 0)
		      "0:00:00")
		s (read-string
		   (format "Restart timer with offset [%s]: " def)))
	  (unless (string-match "\\S-" s) (setq s def))
	  (setq delta (org-timer-hms-to-secs (org-timer-fix-incomplete s)))))
	(setq org-timer-start-time
	      (seconds-to-time
	       ;; Pass `current-time' result to `float-time' (instead
	       ;; of calling without arguments) so that only
	       ;; `current-time' has to be overridden in tests.
	       (- (float-time (current-time)) delta))))
      (setq org-timer-pause-time nil)
      (org-timer-set-mode-line 'on)
      (message "Timer start time set to %s, current value is %s"
	       (format-time-string "%T" org-timer-start-time)
	       (org-timer-secs-to-hms (or delta 0)))
      (run-hooks 'org-timer-start-hook)))))

(defun org-timer-pause-or-continue (&optional stop)
  "Pause or continue the relative or countdown timer.
With prefix arg STOP, stop it entirely."
  (interactive "P")
  (cond
   (stop (org-timer-stop))
   ((not org-timer-start-time) (error "No timer is running"))
   (org-timer-pause-time
    (let ((start-secs (float-time org-timer-start-time))
	  (pause-secs (float-time org-timer-pause-time)))
      (if org-timer-countdown-timer
	  (let ((new-secs (- start-secs pause-secs)))
	    (setq org-timer-countdown-timer
		  (org-timer--run-countdown-timer
		   new-secs org-timer-countdown-timer-title))
	    (setq org-timer-start-time
		  (time-add (current-time) (seconds-to-time new-secs))))
	(setq org-timer-start-time
	      ;; Pass `current-time' result to `float-time' (instead
	      ;; of calling without arguments) so that only
	      ;; `current-time' has to be overridden in tests.
	      (seconds-to-time (- (float-time (current-time))
				  (- pause-secs start-secs)))))
      (setq org-timer-pause-time nil)
      (org-timer-set-mode-line 'on)
      (run-hooks 'org-timer-continue-hook)
      (message "Timer continues at %s" (org-timer-value-string))))
   (t
    ;; pause timer
    (when org-timer-countdown-timer
      (cancel-timer org-timer-countdown-timer)
      (setq org-timer-countdown-timer 'paused))
    (run-hooks 'org-timer-pause-hook)
    (setq org-timer-pause-time (current-time))
    (org-timer-set-mode-line 'paused)
    (message "Timer paused at %s" (org-timer-value-string)))))

(defun org-timer-stop ()
  "Stop the relative or countdown timer."
  (interactive)
  (unless org-timer-start-time
    (user-error "No timer running"))
  (when (timerp org-timer-countdown-timer)
    (cancel-timer org-timer-countdown-timer))
  (run-hooks 'org-timer-stop-hook)
  (setq org-timer-start-time nil
	org-timer-pause-time nil
	org-timer-countdown-timer nil)
  (org-timer-set-mode-line 'off)
  (message "Timer stopped"))

;;;###autoload
(defun org-timer (&optional restart no-insert)
  "Insert a H:MM:SS string from the timer into the buffer.
The first time this command is used, the timer is started.

When used with a `\\[universal-argument]' prefix, force restarting the timer.

When used with a `\\[universal-argument] \\[universal-argument]' \
prefix, change all the timer strings
in the region by a fixed amount.  This can be used to re-calibrate
a timer that was not started at the correct moment.

If NO-INSERT is non-nil, return the string instead of inserting
it in the buffer."
  (interactive "P")
  (if (equal restart '(16))
      (org-timer-start restart)
    (when (or (equal restart '(4)) (not org-timer-start-time))
      (org-timer-start))
    (if no-insert
	(org-timer-value-string)
      (insert (org-timer-value-string)))))

(defun org-timer-value-string ()
  "Set the timer string."
  (format org-timer-format
	  (org-timer-secs-to-hms
	   (abs (floor (org-timer-seconds))))))

(defun org-timer-seconds ()
  ;; Pass `current-time' result to `float-time' (instead of calling
  ;; without arguments) so that only `current-time' has to be
  ;; overridden in tests.
  (if org-timer-countdown-timer
      (- (float-time org-timer-start-time)
	 (float-time (or org-timer-pause-time (current-time))))
    (- (float-time (or org-timer-pause-time (current-time)))
       (float-time org-timer-start-time))))

;;;###autoload
(defun org-timer-change-times-in-region (beg end delta)
  "Change all h:mm:ss time in region by a DELTA."
  (interactive
   "r\nsEnter time difference like \"-1:08:26\".  Default is first time to zero: ")
  (let ((re "[-+]?[0-9]+:[0-9]\\{2\\}:[0-9]\\{2\\}") p)
    (unless (string-match "\\S-" delta)
      (save-excursion
	(goto-char beg)
	(when (re-search-forward re end t)
	  (setq delta (match-string 0))
	  (if (equal (string-to-char delta) ?-)
	      (setq delta (substring delta 1))
	    (setq delta (concat "-" delta))))))
    (setq delta (org-timer-hms-to-secs (org-timer-fix-incomplete delta)))
    (when (= delta 0) (error "No change"))
    (save-excursion
      (goto-char end)
      (while (re-search-backward re beg t)
	(setq p (point))
	(replace-match
	 (save-match-data
	   (org-timer-secs-to-hms (+ (org-timer-hms-to-secs (match-string 0)) delta)))
	 t t)
	(goto-char p)))))

;;;###autoload
(defun org-timer-item (&optional arg)
  "Insert a description-type item with the current timer value."
  (interactive "P")
  (let ((itemp (org-in-item-p)) (pos (point)))
    (cond
     ;; In a timer list, insert with `org-list-insert-item',
     ;; then fix the list.
     ((and itemp (goto-char itemp) (org-at-item-timer-p))
      (let* ((struct (org-list-struct))
	     (prevs (org-list-prevs-alist struct))
	     (s (concat (org-timer (when arg '(4)) t) ":: ")))
	(setq struct (org-list-insert-item pos struct prevs nil s))
	(org-list-write-struct struct (org-list-parents-alist struct))
	(looking-at org-list-full-item-re)
	(goto-char (match-end 0))))
     ;; In a list of another type, don't break anything: throw an error.
     (itemp (goto-char pos) (error "This is not a timer list"))
     ;; Else, start a new list.
     (t
      (beginning-of-line)
      (org-indent-line)
      (insert  "- ")
      (org-timer (when arg '(4)))
      (insert ":: ")))))

(defun org-timer-fix-incomplete (hms)
  "If hms is a H:MM:SS string with missing hour or hour and minute, fix it."
  (if (string-match "\\(?:\\([0-9]+:\\)?\\([0-9]+:\\)\\)?\\([0-9]+\\)" hms)
      (replace-match
       (format "%d:%02d:%02d"
	       (if (match-end 1) (string-to-number (match-string 1 hms)) 0)
	       (if (match-end 2) (string-to-number (match-string 2 hms)) 0)
	       (string-to-number (match-string 3 hms)))
       t t hms)
    (error "Cannot parse HMS string \"%s\"" hms)))

(defun org-timer-hms-to-secs (hms)
  "Convert h:mm:ss string to an integer time.
If the string starts with a minus sign, the integer will be negative."
  (if (not (string-match
	    "\\([-+]?[0-9]+\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)"
	    hms))
      0
    (let* ((h (string-to-number (match-string 1 hms)))
	   (m (string-to-number (match-string 2 hms)))
	   (s (string-to-number (match-string 3 hms)))
	   (sign (equal (substring (match-string 1 hms) 0 1) "-")))
      (setq h (abs h))
      (* (if sign -1 1) (+ s (* 60 (+ m (* 60 h))))))))

(defun org-timer-secs-to-hms (s)
  "Convert integer S into h:mm:ss.
If the integer is negative, the string will start with \"-\"."
  (let (sign m h)
    (setq sign (if (< s 0) "-" "")
	  s (abs s)
	  m (/ s 60) s (- s (* 60 m))
	  h (/ m 60) m (- m (* 60 h)))
    (format "%s%d:%02d:%02d" sign h m s)))

(defvar org-timer-mode-line-timer nil)
(defvar org-timer-mode-line-string nil)

(defun org-timer-set-mode-line (value)
  "Set the mode-line display for relative or countdown timer.
VALUE can be `on', `off', or `paused'."
  (when (or (eq org-timer-display 'mode-line)
	    (eq org-timer-display 'both))
    (or global-mode-string (setq global-mode-string '("")))
    (or (memq 'org-timer-mode-line-string global-mode-string)
	(setq global-mode-string
	      (append global-mode-string '(org-timer-mode-line-string)))))
  (when (or (eq org-timer-display 'frame-title)
	    (eq org-timer-display 'both))
    (or (memq 'org-timer-mode-line-string frame-title-format)
	(setq frame-title-format
	      (append frame-title-format '(org-timer-mode-line-string)))))
  (cl-case value
    (off
     (when org-timer-mode-line-timer
       (cancel-timer org-timer-mode-line-timer)
       (setq org-timer-mode-line-timer nil))
     (when (or (eq org-timer-display 'mode-line)
	       (eq org-timer-display 'both))
       (setq global-mode-string
	     (delq 'org-timer-mode-line-string global-mode-string)))
     (when (or (eq org-timer-display 'frame-title)
	       (eq org-timer-display 'both))
       (setq frame-title-format
	     (delq 'org-timer-mode-line-string frame-title-format)))
     (force-mode-line-update))
    (paused
     (when org-timer-mode-line-timer
       (cancel-timer org-timer-mode-line-timer)
       (setq org-timer-mode-line-timer nil)))
    (on
     (when (or (eq org-timer-display 'mode-line)
	       (eq org-timer-display 'both))
       (or global-mode-string (setq global-mode-string '("")))
       (or (memq 'org-timer-mode-line-string global-mode-string)
	   (setq global-mode-string
		 (append global-mode-string '(org-timer-mode-line-string)))))
     (when (or (eq org-timer-display 'frame-title)
	       (eq org-timer-display 'both))
       (or (memq 'org-timer-mode-line-string frame-title-format)
	   (setq frame-title-format
		 (append frame-title-format '(org-timer-mode-line-string)))))
     (org-timer-update-mode-line)
     (when org-timer-mode-line-timer
       (cancel-timer org-timer-mode-line-timer)
       (setq org-timer-mode-line-timer nil))
     (when org-timer-display
       (setq org-timer-mode-line-timer
	     (run-with-timer 1 1 'org-timer-update-mode-line))))))

(defun org-timer-update-mode-line ()
  "Update the timer time in the mode line."
  (if org-timer-pause-time
      nil
    (setq org-timer-mode-line-string
	  (concat " <" (substring (org-timer-value-string) 0 -1) ">"))
    (force-mode-line-update)))

(defun org-timer-show-remaining-time ()
  "Display the remaining time before the timer ends."
  (interactive)
  (require 'time)
  (if (not org-timer-countdown-timer)
      (message "No timer set")
    (let* ((rtime (decode-time
		   (time-subtract (timer--time org-timer-countdown-timer)
				  (current-time))))
	   (rsecs (nth 0 rtime))
	   (rmins (nth 1 rtime)))
      (message "%d minute(s) %d seconds left before next time out"
	       rmins rsecs))))

;;;###autoload
(defun org-timer-set-timer (&optional opt)
  "Prompt for a duration in minutes or hh:mm:ss and set a timer.

If `org-timer-default-timer' is not \"0\", suggest this value as
the default duration for the timer.  If a timer is already set,
prompt the user if she wants to replace it.

Called with a numeric prefix argument, use this numeric value as
the duration of the timer in minutes.

Called with a `C-u' prefix arguments, use `org-timer-default-timer'
without prompting the user for a duration.

With two `C-u' prefix arguments, use `org-timer-default-timer'
without prompting the user for a duration and automatically
replace any running timer.

By default, the timer duration will be set to the number of
minutes in the Effort property, if any.  You can ignore this by
using three `C-u' prefix arguments."
  (interactive "P")
  (when (and org-timer-start-time
	     (not org-timer-countdown-timer))
    (user-error "Relative timer is running.  Stop first"))
  (let* ((default-timer
	   ;; `org-timer-default-timer' used to be a number, don't choke:
	   (if (numberp org-timer-default-timer)
	       (number-to-string org-timer-default-timer)
	     org-timer-default-timer))
	 (effort-minutes (ignore-errors (floor (org-get-at-eol 'effort-minutes 1))))
	 (minutes (or (and (numberp opt) (number-to-string opt))
		      (and (not (equal opt '(64)))
			   effort-minutes
			   (number-to-string effort-minutes))
		      (and (consp opt) default-timer)
		      (and (stringp opt) opt)
		      (read-from-minibuffer
		       "How much time left? (minutes or h:mm:ss) "
		       (and (not (string-equal default-timer "0")) default-timer)))))
    (when (string-match "\\`[0-9]+\\'" minutes)
      (setq minutes (concat minutes ":00")))
    (if (not (string-match "[0-9]+" minutes))
	(org-timer-show-remaining-time)
      (let ((secs (org-timer-hms-to-secs (org-timer-fix-incomplete minutes))))
	(if (and org-timer-countdown-timer
		 (not (or (equal opt '(16))
			  (y-or-n-p "Replace current timer? "))))
	    (message "No timer set")
	  (when (timerp org-timer-countdown-timer)
	    (cancel-timer org-timer-countdown-timer))
	  (setq org-timer-countdown-timer-title
		(org-timer--get-timer-title))
	  (setq org-timer-countdown-timer
		(org-timer--run-countdown-timer
		 secs org-timer-countdown-timer-title))
	  (run-hooks 'org-timer-set-hook)
	  (setq org-timer-start-time
		(time-add (current-time) (seconds-to-time secs)))
	  (setq org-timer-pause-time nil)
	  (org-timer-set-mode-line 'on))))))

(defun org-timer--run-countdown-timer (secs title)
  "Start countdown timer that will last SECS.
TITLE will be appended to the notification message displayed when
time is up."
  (let ((msg (format "%s: time out" title)))
    (run-with-timer
     secs nil `(lambda ()
		 (setq org-timer-countdown-timer nil
		       org-timer-start-time nil)
		 (org-notify ,msg ,org-clock-sound)
		 (org-timer-set-mode-line 'off)
		 (run-hooks 'org-timer-done-hook)))))

(defun org-timer--get-timer-title ()
  "Construct timer title from heading or file name of Org buffer."
  (cond
   ((derived-mode-p 'org-agenda-mode)
    (let* ((marker (or (get-text-property (point) 'org-marker)
		       (org-agenda-error)))
	   (hdmarker (or (get-text-property (point) 'org-hd-marker)
			 marker)))
      (with-current-buffer (marker-buffer marker)
	(org-with-wide-buffer
	 (goto-char hdmarker)
	 (org-show-entry)
	 (or (ignore-errors (org-get-heading))
	     (buffer-name (buffer-base-buffer)))))))
   ((derived-mode-p 'org-mode)
    (or (ignore-errors (org-get-heading))
	(buffer-name (buffer-base-buffer))))
   (t (error "Not in an Org buffer"))))

(provide 'org-timer)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-timer.el ends here
