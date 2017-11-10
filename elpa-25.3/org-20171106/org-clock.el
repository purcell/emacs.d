;;; org-clock.el --- The time clocking code for Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2017 Free Software Foundation, Inc.

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

;; This file contains the time clocking code for Org mode

;;; Code:

(require 'cl-lib)
(require 'org)

(declare-function calendar-iso-to-absolute "cal-iso" (date))
(declare-function notifications-notify "notifications" (&rest params))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))
(declare-function org-table-goto-line "org-table" (n))

(defvar org-frame-title-format-backup frame-title-format)
(defvar org-time-stamp-formats)


(defgroup org-clock nil
  "Options concerning clocking working time in Org mode."
  :tag "Org Clock"
  :group 'org-progress)

(defcustom org-clock-into-drawer t
  "Non-nil when clocking info should be wrapped into a drawer.

When non-nil, clocking info will be inserted into the same drawer
as log notes (see variable `org-log-into-drawer'), if it exists,
or \"LOGBOOK\" otherwise.  If necessary, the drawer will be
created.

When an integer, the drawer is created only when the number of
clocking entries in an item reaches or exceeds this value.

When a string, it becomes the name of the drawer, ignoring the
log notes drawer altogether.

Do not check directly this variable in a Lisp program.  Call
function `org-clock-into-drawer' instead."
  :group 'org-todo
  :group 'org-clock
  :version "26.1"
  :package-version '(Org . "8.3")
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Only when drawer exists" nil)
	  (integer :tag "When at least N clock entries")
	  (const :tag "Into LOGBOOK drawer" "LOGBOOK")
	  (string :tag "Into Drawer named...")))

(defun org-clock-into-drawer ()
  "Value of `org-clock-into-drawer'. but let properties overrule.

If the current entry has or inherits a CLOCK_INTO_DRAWER
property, it will be used instead of the default value.

Return value is either a string, an integer, or nil."
  (let ((p (org-entry-get nil "CLOCK_INTO_DRAWER" 'inherit t)))
    (cond ((equal p "nil") nil)
	  ((equal p "t") (or (org-log-into-drawer) "LOGBOOK"))
          ((org-string-nw-p p)
	   (if (string-match-p "\\`[0-9]+\\'" p) (string-to-number p) p))
	  ((org-string-nw-p org-clock-into-drawer))
	  ((integerp org-clock-into-drawer) org-clock-into-drawer)
	  ((not org-clock-into-drawer) nil)
	  ((org-log-into-drawer))
	  (t "LOGBOOK"))))

(defcustom org-clock-out-when-done t
  "When non-nil, clock will be stopped when the clocked entry is marked DONE.
\\<org-mode-map>\
DONE here means any DONE-like state.
A nil value means clock will keep running until stopped explicitly with
`\\[org-clock-out]', or until the clock is started in a different item.
Instead of t, this can also be a list of TODO states that should trigger
clocking out."
  :group 'org-clock
  :type '(choice
	  (const :tag "No" nil)
	  (const :tag "Yes, when done" t)
	  (repeat :tag "State list"
		  (string :tag "TODO keyword"))))

(defcustom org-clock-rounding-minutes 0
  "Rounding minutes when clocking in or out.
The default value is 0 so that no rounding is done.
When set to a non-integer value, use the car of
`org-time-stamp-rounding-minutes', like for setting a time-stamp.

E.g. if `org-clock-rounding-minutes' is set to 5, time is 14:47
and you clock in: then the clock starts at 14:45.  If you clock
out within the next 5 minutes, the clock line will be removed;
if you clock out 8 minutes after your clocked in, the clock
out time will be 14:50."
  :group 'org-clock
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (integer :tag "Minutes (0 for no rounding)")
	  (symbol  :tag "Use `org-time-stamp-rounding-minutes'" 'same-as-time-stamp)))

(defcustom org-clock-out-remove-zero-time-clocks nil
  "Non-nil means remove the clock line when the resulting time is zero."
  :group 'org-clock
  :type 'boolean)

(defcustom org-clock-in-switch-to-state nil
  "Set task to a special todo state while clocking it.
The value should be the state to which the entry should be
switched.  If the value is a function, it must take one
parameter (the current TODO state of the item) and return the
state to switch it to."
  :group 'org-clock
  :group 'org-todo
  :type '(choice
	  (const :tag "Don't force a state" nil)
	  (string :tag "State")
	  (symbol :tag "Function")))

(defcustom org-clock-out-switch-to-state nil
  "Set task to a special todo state after clocking out.
The value should be the state to which the entry should be
switched.  If the value is a function, it must take one
parameter (the current TODO state of the item) and return the
state to switch it to."
  :group 'org-clock
  :group 'org-todo
  :type '(choice
	  (const :tag "Don't force a state" nil)
	  (string :tag "State")
	  (symbol :tag "Function")))

(defcustom org-clock-history-length 5
  "Number of clock tasks to remember in history."
  :group 'org-clock
  :type 'integer)

(defcustom org-clock-goto-may-find-recent-task t
  "Non-nil means `org-clock-goto' can go to recent task if no active clock."
  :group 'org-clock
  :type 'boolean)

(defcustom org-clock-heading-function nil
  "When non-nil, should be a function to create `org-clock-heading'.
This is the string shown in the mode line when a clock is running.
The function is called with point at the beginning of the headline."
  :group 'org-clock
  :type '(choice (const nil) (function)))

(defcustom org-clock-string-limit 0
  "Maximum length of clock strings in the mode line.  0 means no limit."
  :group 'org-clock
  :type 'integer)

(defcustom org-clock-in-resume nil
  "If non-nil, resume clock when clocking into task with open clock.
When clocking into a task with a clock entry which has not been closed,
the clock can be resumed from that point."
  :group 'org-clock
  :type 'boolean)

(defcustom org-clock-persist nil
  "When non-nil, save the running clock when Emacs is closed.
The clock is resumed when Emacs restarts.
When this is t, both the running clock, and the entire clock
history are saved.  When this is the symbol `clock', only the
running clock is saved.  When this is the symbol `history', only
the clock history is saved.

When Emacs restarts with saved clock information, the file containing
the running clock as well as all files mentioned in the clock history
will be visited.

All this depends on running `org-clock-persistence-insinuate' in your
Emacs initialization file."
  :group 'org-clock
  :type '(choice
	  (const :tag "Just the running clock" clock)
	  (const :tag "Just the history" history)
	  (const :tag "Clock and history" t)
	  (const :tag "No persistence" nil)))

(defcustom org-clock-persist-file (convert-standard-filename
				   (concat user-emacs-directory "org-clock-save.el"))
  "File to save clock data to."
  :group 'org-clock
  :type 'string)

(defcustom org-clock-persist-query-save nil
  "When non-nil, ask before saving the current clock on exit."
  :group 'org-clock
  :type 'boolean)

(defcustom org-clock-persist-query-resume t
  "When non-nil, ask before resuming any stored clock during load."
  :group 'org-clock
  :type 'boolean)

(defcustom org-clock-sound nil
  "Sound to use for notifications.
Possible values are:

nil        No sound played
t          Standard Emacs beep
file name  Play this sound file, fall back to beep"
  :group 'org-clock
  :type '(choice
	  (const :tag "No sound" nil)
	  (const :tag "Standard beep" t)
	  (file  :tag "Play sound file")))

(defcustom org-clock-mode-line-total 'auto
  "Default setting for the time included for the mode line clock.
This can be overruled locally using the CLOCK_MODELINE_TOTAL property.
Allowed values are:

current  Only the time in the current instance of the clock
today    All time clocked into this task today
repeat   All time clocked into this task since last repeat
all      All time ever recorded for this task
auto     Automatically, either `all', or `repeat' for repeating tasks"
  :group 'org-clock
  :type '(choice
	  (const :tag "Current clock" current)
	  (const :tag "Today's task time" today)
	  (const :tag "Since last repeat" repeat)
	  (const :tag "All task time" all)
	  (const :tag "Automatically, `all' or since `repeat'" auto)))

(defvaralias 'org-task-overrun-text 'org-clock-task-overrun-text)
(defcustom org-clock-task-overrun-text nil
  "Extra mode line text to indicate that the clock is overrun.
The can be nil to indicate that instead of adding text, the clock time
should get a different face (`org-mode-line-clock-overrun').
When this is a string, it is prepended to the clock string as an indication,
also using the face `org-mode-line-clock-overrun'."
  :group 'org-clock
  :version "24.1"
  :type '(choice
	  (const :tag "Just mark the time string" nil)
	  (string :tag "Text to prepend")))

(defcustom org-show-notification-handler nil
  "Function or program to send notification with.
The function or program will be called with the notification
string as argument."
  :group 'org-clock
  :type '(choice
	  (const nil)
	  (string :tag "Program")
	  (function :tag "Function")))

(defgroup org-clocktable nil
  "Options concerning the clock table in Org mode."
  :tag "Org Clock Table"
  :group 'org-clock)

(defcustom org-clocktable-defaults
  (list
   :maxlevel 2
   :lang (or (bound-and-true-p org-export-default-language) "en")
   :scope 'file
   :block nil
   :wstart 1
   :mstart 1
   :tstart nil
   :tend nil
   :step nil
   :stepskip0 nil
   :fileskip0 nil
   :tags nil
   :emphasize nil
   :link nil
   :narrow '40!
   :indent t
   :formula nil
   :timestamp nil
   :level nil
   :tcolumns nil
   :formatter nil)
  "Default properties for clock tables."
  :group 'org-clock
  :version "24.1"
  :type 'plist)

(defcustom org-clock-clocktable-formatter 'org-clocktable-write-default
  "Function to turn clocking data into a table.
For more information, see `org-clocktable-write-default'."
  :group 'org-clocktable
  :version "24.1"
  :type 'function)

;; FIXME: translate es and nl last string "Clock summary at"
(defcustom org-clock-clocktable-language-setup
  '(("en" "File"     "L"  "Timestamp"  "Headline" "Time"  "ALL"   "Total time"   "File time" "Clock summary at")
    ("es" "Archivo"  "N"  "Fecha y hora" "Tarea" "Tiempo" "TODO" "Tiempo total" "Tiempo archivo" "Clock summary at")
    ("fr" "Fichier"  "N"  "Horodatage" "En-tête"  "Durée" "TOUT"  "Durée totale" "Durée fichier" "Horodatage sommaire à")
    ("nl" "Bestand"  "N"  "Tijdstip"   "Hoofding" "Duur"  "ALLES" "Totale duur"  "Bestandstijd" "Clock summary at")
    ("de" "Datei"    "E"  "Zeitstempel" "Kopfzeile" "Dauer" "GESAMT"
     "Gesamtdauer"  "Dateizeit" "Erstellt am"))
  "Terms used in clocktable, translated to different languages."
  :group 'org-clocktable
  :version "24.1"
  :type 'alist)

(defcustom org-clock-clocktable-default-properties '(:maxlevel 2 :scope file)
  "Default properties for new clocktables.
These will be inserted into the BEGIN line, to make it easy for users to
play with them."
  :group 'org-clocktable
  :type 'plist)

(defcustom org-clock-idle-time nil
  "When non-nil, resolve open clocks if the user is idle more than X minutes."
  :group 'org-clock
  :type '(choice
	  (const :tag "Never" nil)
	  (integer :tag "After N minutes")))

(defcustom org-clock-auto-clock-resolution 'when-no-clock-is-running
  "When to automatically resolve open clocks found in Org buffers."
  :group 'org-clock
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Always" t)
	  (const :tag "When no clock is running" when-no-clock-is-running)))

(defcustom org-clock-report-include-clocking-task nil
  "When non-nil, include the current clocking task time in clock reports."
  :group 'org-clock
  :version "24.1"
  :type 'boolean)

(defcustom org-clock-resolve-expert nil
  "Non-nil means do not show the splash buffer with the clock resolver."
  :group 'org-clock
  :version "24.1"
  :type 'boolean)

(defcustom org-clock-continuously nil
  "Non-nil means to start clocking from the last clock-out time, if any."
  :type 'boolean
  :version "24.1"
  :group 'org-clock)

(defcustom org-clock-total-time-cell-format "*%s*"
  "Format string for the total time cells."
  :group 'org-clock
  :version "24.1"
  :type 'string)

(defcustom org-clock-file-time-cell-format "*%s*"
  "Format string for the file time cells."
  :group 'org-clock
  :version "24.1"
  :type 'string)

(defcustom org-clock-clocked-in-display 'mode-line
  "When clocked in for a task, Org can display the current
task and accumulated time in the mode line and/or frame title.
Allowed values are:

both         displays in both mode line and frame title
mode-line    displays only in mode line (default)
frame-title  displays only in frame title
nil          current clock is not displayed"
  :group 'org-clock
  :type '(choice
	  (const :tag "Mode line" mode-line)
	  (const :tag "Frame title" frame-title)
	  (const :tag "Both" both)
	  (const :tag "None" nil)))

(defcustom org-clock-frame-title-format '(t org-mode-line-string)
  "The value for `frame-title-format' when clocking in.

When `org-clock-clocked-in-display' is set to `frame-title'
or `both', clocking in will replace `frame-title-format' with
this value.  Clocking out will restore `frame-title-format'.

`org-frame-title-string' is a format string using the same
specifications than `frame-title-format', which see."
  :version "24.1"
  :group 'org-clock
  :type 'sexp)

(defcustom org-clock-x11idle-program-name "x11idle"
  "Name of the program which prints X11 idle time in milliseconds.

You can find x11idle.c in the contrib/scripts directory of the
Org git distribution. Or, you can do:

    sudo apt-get install xprintidle

if you are using Debian."
  :group 'org-clock
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-clock-goto-before-context 2
  "Number of lines of context to display before currently clocked-in entry.
This applies when using `org-clock-goto'."
  :group 'org-clock
  :type 'integer)

(defcustom org-clock-display-default-range 'thisyear
  "Default range when displaying clocks with `org-clock-display'."
  :group 'org-clock
  :type '(choice (const today)
		 (const yesterday)
		 (const thisweek)
		 (const lastweek)
		 (const thismonth)
		 (const lastmonth)
		 (const thisyear)
		 (const lastyear)
		 (const untilnow)
		 (const :tag "Select range interactively" interactive)))

(defvar org-clock-in-prepare-hook nil
  "Hook run when preparing the clock.
This hook is run before anything happens to the task that
you want to clock in.  For example, you can use this hook
to add an effort property.")
(defvar org-clock-in-hook nil
  "Hook run when starting the clock.")
(defvar org-clock-out-hook nil
  "Hook run when stopping the current clock.")

(defvar org-clock-cancel-hook nil
  "Hook run when canceling the current clock.")
(defvar org-clock-goto-hook nil
  "Hook run when selecting the currently clocked-in entry.")
(defvar org-clock-has-been-used nil
  "Has the clock been used during the current Emacs session?")

(defvar org-clock-stored-history nil
  "Clock history, populated by `org-clock-load'")
(defvar org-clock-stored-resume-clock nil
  "Clock to resume, saved by `org-clock-load'")

(defconst org-clock--oldest-date
  (let* ((dichotomy
	  (lambda (min max pred)
	    (if (funcall pred min) min
	      (cl-incf min)
	      (while (> (- max min) 1)
		(let ((mean (+ (ash min -1) (ash max -1) (logand min max 1))))
		  (if (funcall pred mean) (setq max mean) (setq min mean)))))
	    max))
	 (high
	  (funcall dichotomy
		   most-negative-fixnum
		   0
		   (lambda (m)
                     ;; libc in macOS 10.6 hangs when decoding times
                     ;; around year -2**31.  Limit `high' not to go
                     ;; any earlier than that.
                     (unless (and (eq system-type 'darwin)
                                  (string-match-p
                                   "10\\.6\\.[[:digit:]]"
                                   (shell-command-to-string
                                    "sw_vers -productVersion"))
                                  (<= m -1034058203136))
                       (ignore-errors (decode-time (list m 0)))))))
	 (low
	  (funcall dichotomy
		   most-negative-fixnum
		   0
		   (lambda (m) (ignore-errors (decode-time (list high m)))))))
    (list high low))
  "Internal time for oldest date representable on the system.")

;;; The clock for measuring work time.

(defvar org-mode-line-string "")
(put 'org-mode-line-string 'risky-local-variable t)

(defvar org-clock-mode-line-timer nil)
(defvar org-clock-idle-timer nil)
(defvar org-clock-heading) ; defined in org.el
(defvar org-clock-start-time "")

(defvar org-clock-leftover-time nil
  "If non-nil, user canceled a clock; this is when leftover time started.")

(defvar org-clock-effort ""
  "Effort estimate of the currently clocking task.")

(defvar org-clock-total-time nil
  "Holds total time, spent previously on currently clocked item.
This does not include the time in the currently running clock.")

(defvar org-clock-history nil
  "List of marker pointing to recent clocked tasks.")

(defvar org-clock-default-task (make-marker)
  "Marker pointing to the default task that should clock time.
The clock can be made to switch to this task after clocking out
of a different task.")

(defvar org-clock-interrupted-task (make-marker)
  "Marker pointing to the task that has been interrupted by the current clock.")

(defvar org-clock-mode-line-map (make-sparse-keymap))
(define-key org-clock-mode-line-map [mode-line mouse-2] 'org-clock-goto)
(define-key org-clock-mode-line-map [mode-line mouse-1] 'org-clock-menu)

(defun org-clock--translate (s language)
  "Translate string S into using string LANGUAGE.
Assume S in the English term to translate.  Return S as-is if it
cannot be translated."
  (or (nth (pcase s
	     ("File" 1) ("L" 2) ("Timestamp" 3) ("Headline" 4) ("Time" 5)
	     ("ALL" 6) ("Total time" 7) ("File time" 8) ("Clock summary at" 9))
	   (assoc-string language org-clock-clocktable-language-setup t))
      s))

(defun org-clock-menu ()
  (interactive)
  (popup-menu
   '("Clock"
     ["Clock out" org-clock-out t]
     ["Change effort estimate" org-clock-modify-effort-estimate t]
     ["Go to clock entry" org-clock-goto t]
     ["Switch task" (lambda () (interactive) (org-clock-in '(4))) :active t :keys "C-u C-c C-x C-i"])))

(defun org-clock-history-push (&optional pos buffer)
  "Push a marker to the clock history."
  (setq org-clock-history-length (max 1 (min 35 org-clock-history-length)))
  (let ((m (move-marker (make-marker)
			(or pos (point)) (org-base-buffer
					  (or buffer (current-buffer)))))
	n l)
    (while (setq n (member m org-clock-history))
      (move-marker (car n) nil))
    (setq org-clock-history
	  (delq nil
		(mapcar (lambda (x) (if (marker-buffer x) x nil))
			org-clock-history)))
    (when (>= (setq l (length org-clock-history)) org-clock-history-length)
      (setq org-clock-history
	    (nreverse
	     (nthcdr (- l org-clock-history-length -1)
		     (nreverse org-clock-history)))))
    (push m org-clock-history)))

(defun org-clock-save-markers-for-cut-and-paste (beg end)
  "Save relative positions of markers in region."
  (org-check-and-save-marker org-clock-marker beg end)
  (org-check-and-save-marker org-clock-hd-marker beg end)
  (org-check-and-save-marker org-clock-default-task beg end)
  (org-check-and-save-marker org-clock-interrupted-task beg end)
  (dolist (m org-clock-history)
    (org-check-and-save-marker m beg end)))

(defun org-clock-drawer-name ()
  "Return clock drawer's name for current entry, or nil."
  (let ((drawer (org-clock-into-drawer)))
    (cond ((integerp drawer)
	   (let ((log-drawer (org-log-into-drawer)))
	     (if (stringp log-drawer) log-drawer "LOGBOOK")))
	  ((stringp drawer) drawer)
	  (t nil))))

(defun org-clocking-buffer ()
  "Return the clocking buffer if we are currently clocking a task or nil."
  (marker-buffer org-clock-marker))

(defun org-clocking-p ()
  "Return t when clocking a task."
  (not (equal (org-clocking-buffer) nil)))

(defvar org-clock-before-select-task-hook nil
  "Hook called in task selection just before prompting the user.")

(defun org-clock-select-task (&optional prompt)
  "Select a task that was recently associated with clocking.
Return marker position of the selected task.  Raise an error if
there is no recent clock to choose from."
  (let (och chl sel-list rpl (i 0) s)
    ;; Remove successive dups from the clock history to consider
    (dolist (c org-clock-history)
      (unless (equal c (car och)) (push c och)))
    (setq och (reverse och) chl (length och))
    (if (zerop chl)
	(user-error "No recent clock")
      (save-window-excursion
	(org-switch-to-buffer-other-window
	 (get-buffer-create "*Clock Task Select*"))
	(erase-buffer)
	(when (marker-buffer org-clock-default-task)
	  (insert (org-add-props "Default Task\n" nil 'face 'bold))
	  (setq s (org-clock-insert-selection-line ?d org-clock-default-task))
	  (push s sel-list))
	(when (marker-buffer org-clock-interrupted-task)
	  (insert (org-add-props "The task interrupted by starting the last one\n" nil 'face 'bold))
	  (setq s (org-clock-insert-selection-line ?i org-clock-interrupted-task))
	  (push s sel-list))
	(when (org-clocking-p)
	  (insert (org-add-props "Current Clocking Task\n" nil 'face 'bold))
	  (setq s (org-clock-insert-selection-line ?c org-clock-marker))
	  (push s sel-list))
	(insert (org-add-props "Recent Tasks\n" nil 'face 'bold))
	(dolist (m och)
	  (when (marker-buffer m)
	    (setq i (1+ i)
		  s (org-clock-insert-selection-line
		     (if (< i 10)
			 (+ i ?0)
		       (+ i (- ?A 10))) m))
	    (if (fboundp 'int-to-char) (setf (car s) (int-to-char (car s))))
	    (push s sel-list)))
	(run-hooks 'org-clock-before-select-task-hook)
	(goto-char (point-min))
	;; Set min-height relatively to circumvent a possible but in
	;; `fit-window-to-buffer'
	(fit-window-to-buffer nil nil (if (< chl 10) chl (+ 5 chl)))
	(message (or prompt "Select task for clocking:"))
	(setq cursor-type nil rpl (read-char-exclusive))
	(kill-buffer)
	(cond
	 ((eq rpl ?q) nil)
	 ((eq rpl ?x) nil)
	 ((assoc rpl sel-list) (cdr (assoc rpl sel-list)))
	 (t (user-error "Invalid task choice %c" rpl)))))))

(defun org-clock-insert-selection-line (i marker)
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the marker
pointing to it."
  (when (marker-buffer marker)
    (let (cat task heading prefix)
      (with-current-buffer (org-base-buffer (marker-buffer marker))
	(org-with-wide-buffer
	  (ignore-errors
	    (goto-char marker)
	    (setq cat (org-get-category)
		  heading (org-get-heading 'notags)
		  prefix (save-excursion
			   (org-back-to-heading t)
			   (looking-at org-outline-regexp)
			   (match-string 0))
		  task (substring
			(org-fontify-like-in-org-mode
			 (concat prefix heading)
			 org-odd-levels-only)
			(length prefix))))))
      (when (and cat task)
	(insert (format "[%c] %-12s  %s\n" i cat task))
	(cons i marker)))))

(defvar org-clock-task-overrun nil
  "Internal flag indicating if the clock has overrun the planned time.")
(defvar org-clock-update-period 60
  "Number of seconds between mode line clock string updates.")

(defun org-clock-get-clock-string ()
  "Form a clock-string, that will be shown in the mode line.
If an effort estimate was defined for the current item, use
01:30/01:50 format (clocked/estimated).
If not, show simply the clocked time like 01:50."
  (let ((clocked-time (org-clock-get-clocked-time)))
    (if org-clock-effort
	(let* ((effort-in-minutes (org-duration-to-minutes org-clock-effort))
	       (work-done-str
		(propertize
		 (org-duration-from-minutes clocked-time)
		 'face (if (and org-clock-task-overrun (not org-clock-task-overrun-text))
			   'org-mode-line-clock-overrun 'org-mode-line-clock)))
	       (effort-str (org-duration-from-minutes effort-in-minutes))
	       (clockstr (propertize
			  (concat  " [%s/" effort-str
				   "] (" (replace-regexp-in-string "%" "%%" org-clock-heading) ")")
			  'face 'org-mode-line-clock)))
	  (format clockstr work-done-str))
      (propertize (concat " [" (org-duration-from-minutes clocked-time)
			  "]" (format " (%s)" org-clock-heading))
		  'face 'org-mode-line-clock))))

(defun org-clock-get-last-clock-out-time ()
  "Get the last clock-out time for the current subtree."
  (save-excursion
    (let ((end (save-excursion (org-end-of-subtree))))
      (when (re-search-forward (concat org-clock-string
				       ".*\\]--\\(\\[[^]]+\\]\\)") end t)
	(org-time-string-to-time (match-string 1))))))

(defun org-clock-update-mode-line ()
  (if org-clock-effort
      (org-clock-notify-once-if-expired)
    (setq org-clock-task-overrun nil))
  (setq org-mode-line-string
	(propertize
	 (let ((clock-string (org-clock-get-clock-string))
	       (help-text "Org mode clock is running.\nmouse-1 shows a \
menu\nmouse-2 will jump to task"))
	   (if (and (> org-clock-string-limit 0)
		    (> (length clock-string) org-clock-string-limit))
	       (propertize
		(substring clock-string 0 org-clock-string-limit)
		'help-echo (concat help-text ": " org-clock-heading))
	     (propertize clock-string 'help-echo help-text)))
	 'local-map org-clock-mode-line-map
	 'mouse-face 'mode-line-highlight))
  (if (and org-clock-task-overrun org-clock-task-overrun-text)
      (setq org-mode-line-string
	    (concat (propertize
		     org-clock-task-overrun-text
		     'face 'org-mode-line-clock-overrun) org-mode-line-string)))
  (force-mode-line-update))

(defun org-clock-get-clocked-time ()
  "Get the clocked time for the current item in minutes.
The time returned includes the time spent on this task in
previous clocking intervals."
  (let ((currently-clocked-time
	 (floor (- (float-time)
		   (float-time org-clock-start-time)) 60)))
    (+ currently-clocked-time (or org-clock-total-time 0))))

(defun org-clock-modify-effort-estimate (&optional value)
  "Add to or set the effort estimate of the item currently being clocked.
VALUE can be a number of minutes, or a string with format hh:mm or mm.
When the string starts with a + or a - sign, the current value of the effort
property will be changed by that amount.  If the effort value is expressed
as an `org-effort-durations' (e.g. \"3h\"), the modified value will be
converted to a hh:mm duration.

This command will update the \"Effort\" property of the currently
clocked item, and the value displayed in the mode line."
  (interactive)
  (if (org-clock-is-active)
      (let ((current org-clock-effort) sign)
	(unless value
	  ;; Prompt user for a value or a change
	  (setq value
		(read-string
		 (format "Set effort (hh:mm or mm%s): "
			 (if current
			     (format ", prefix + to add to %s" org-clock-effort)
			   "")))))
	(when (stringp value)
	  ;; A string.  See if it is a delta
	  (setq sign (string-to-char value))
	  (if (member sign '(?- ?+))
	      (setq current (org-duration-to-minutes current)
		    value (substring value 1))
	    (setq current 0))
	  (setq value (org-duration-to-minutes value))
	  (if (equal ?- sign)
	      (setq value (- current value))
	    (if (equal ?+ sign) (setq value (+ current value)))))
	(setq value (max 0 value)
	      org-clock-effort (org-duration-from-minutes value))
	(org-entry-put org-clock-marker "Effort" org-clock-effort)
	(org-clock-update-mode-line)
	(message "Effort is now %s" org-clock-effort))
    (message "Clock is not currently active")))

(defvar org-clock-notification-was-shown nil
  "Shows if we have shown notification already.")

(defun org-clock-notify-once-if-expired ()
  "Show notification if we spent more time than we estimated before.
Notification is shown only once."
  (when (org-clocking-p)
    (let ((effort-in-minutes (org-duration-to-minutes org-clock-effort))
	  (clocked-time (org-clock-get-clocked-time)))
      (if (setq org-clock-task-overrun
		(if (or (null effort-in-minutes) (zerop effort-in-minutes))
		    nil
		  (>= clocked-time effort-in-minutes)))
	  (unless org-clock-notification-was-shown
	    (setq org-clock-notification-was-shown t)
	    (org-notify
	     (format-message "Task `%s' should be finished by now. (%s)"
                             org-clock-heading org-clock-effort)
             org-clock-sound))
	(setq org-clock-notification-was-shown nil)))))

(defun org-notify (notification &optional play-sound)
  "Send a NOTIFICATION and maybe PLAY-SOUND.
If PLAY-SOUND is non-nil, it overrides `org-clock-sound'."
  (org-show-notification notification)
  (if play-sound (org-clock-play-sound play-sound)))

(defun org-show-notification (notification)
  "Show notification.
Use `org-show-notification-handler' if defined,
use libnotify if available, or fall back on a message."
  (cond ((functionp org-show-notification-handler)
	 (funcall org-show-notification-handler notification))
	((stringp org-show-notification-handler)
	 (start-process "emacs-timer-notification" nil
			org-show-notification-handler notification))
	((fboundp 'notifications-notify)
	 (notifications-notify
	  :title "Org mode message"
	  :body notification
	  ;; FIXME how to link to the Org icon?
	  ;; :app-icon "~/.emacs.d/icons/mail.png"
	  :urgency 'low))
	((executable-find "notify-send")
	 (start-process "emacs-timer-notification" nil
			"notify-send" notification))
	;; Maybe the handler will send a message, so only use message as
	;; a fall back option
	(t (message "%s" notification))))

(defun org-clock-play-sound (&optional clock-sound)
  "Play sound as configured by `org-clock-sound'.
Use alsa's aplay tool if available.
If CLOCK-SOUND is non-nil, it overrides `org-clock-sound'."
  (let ((org-clock-sound (or clock-sound org-clock-sound)))
    (cond
     ((not org-clock-sound))
     ((eq org-clock-sound t) (beep t) (beep t))
     ((stringp org-clock-sound)
      (let ((file (expand-file-name org-clock-sound)))
	(if (file-exists-p file)
	    (if (executable-find "aplay")
		(start-process "org-clock-play-notification" nil
			       "aplay" file)
	      (condition-case nil
		  (play-sound-file file)
		(error (beep t) (beep t))))))))))

(defvar org-clock-mode-line-entry nil
  "Information for the mode line about the running clock.")

(defun org-find-open-clocks (file)
  "Search through the given file and find all open clocks."
  (let ((buf (or (get-file-buffer file)
		 (find-file-noselect file)))
	(org-clock-re (concat org-clock-string " \\(\\[.*?\\]\\)$"))
	clocks)
    (with-current-buffer buf
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward org-clock-re nil t)
	  (push (cons (copy-marker (match-end 1) t)
		      (org-time-string-to-time (match-string 1))) clocks))))
    clocks))

(defsubst org-is-active-clock (clock)
  "Return t if CLOCK is the currently active clock."
  (and (org-clock-is-active)
       (= org-clock-marker (car clock))))

(defmacro org-with-clock-position (clock &rest forms)
  "Evaluate FORMS with CLOCK as the current active clock."
  `(with-current-buffer (marker-buffer (car ,clock))
     (org-with-wide-buffer
      (goto-char (car ,clock))
      (beginning-of-line)
      ,@forms)))
(def-edebug-spec org-with-clock-position (form body))
(put 'org-with-clock-position 'lisp-indent-function 1)

(defmacro org-with-clock (clock &rest forms)
  "Evaluate FORMS with CLOCK as the current active clock.
This macro also protects the current active clock from being altered."
  `(org-with-clock-position ,clock
     (let ((org-clock-start-time (cdr ,clock))
	   (org-clock-total-time)
	   (org-clock-history)
	   (org-clock-effort)
	   (org-clock-marker (car ,clock))
	   (org-clock-hd-marker (save-excursion
				  (org-back-to-heading t)
				  (point-marker))))
       ,@forms)))
(def-edebug-spec org-with-clock (form body))
(put 'org-with-clock 'lisp-indent-function 1)

(defsubst org-clock-clock-in (clock &optional resume start-time)
  "Clock in to the clock located by CLOCK.
If necessary, clock-out of the currently active clock."
  (org-with-clock-position clock
    (let ((org-clock-in-resume (or resume org-clock-in-resume)))
      (org-clock-in nil start-time))))

(defsubst org-clock-clock-out (clock &optional fail-quietly at-time)
  "Clock out of the clock located by CLOCK."
  (let ((temp (copy-marker (car clock)
			   (marker-insertion-type (car clock)))))
    (if (org-is-active-clock clock)
	(org-clock-out nil fail-quietly at-time)
      (org-with-clock clock
	(org-clock-out nil fail-quietly at-time)))
    (setcar clock temp)))

(defsubst org-clock-clock-cancel (clock)
  "Cancel the clock located by CLOCK."
  (let ((temp (copy-marker (car clock)
			   (marker-insertion-type (car clock)))))
    (if (org-is-active-clock clock)
	(org-clock-cancel)
      (org-with-clock clock
	(org-clock-cancel)))
    (setcar clock temp)))

(defvar org-clock-clocking-in nil)
(defvar org-clock-resolving-clocks nil)
(defvar org-clock-resolving-clocks-due-to-idleness nil)

(defun org-clock-resolve-clock (clock resolve-to clock-out-time
				      &optional close-p restart-p fail-quietly)
  "Resolve `CLOCK' given the time `RESOLVE-TO', and the present.
`CLOCK' is a cons cell of the form (MARKER START-TIME)."
  (let ((org-clock-resolving-clocks t))
    (cond
     ((null resolve-to)
      (org-clock-clock-cancel clock)
      (if (and restart-p (not org-clock-clocking-in))
	  (org-clock-clock-in clock)))

     ((eq resolve-to 'now)
      (if restart-p
	  (error "RESTART-P is not valid here"))
      (if (or close-p org-clock-clocking-in)
	  (org-clock-clock-out clock fail-quietly)
	(unless (org-is-active-clock clock)
	  (org-clock-clock-in clock t))))

     ((not (time-less-p resolve-to (current-time)))
      (error "RESOLVE-TO must refer to a time in the past"))

     (t
      (if restart-p
	  (error "RESTART-P is not valid here"))
      (org-clock-clock-out clock fail-quietly (or clock-out-time
						  resolve-to))
      (unless org-clock-clocking-in
	(if close-p
	    (setq org-clock-leftover-time (and (null clock-out-time)
					       resolve-to))
	  (org-clock-clock-in clock nil (and clock-out-time
					     resolve-to))))))))

(defun org-clock-jump-to-current-clock (&optional effective-clock)
  (interactive)
  (let ((drawer (org-clock-into-drawer))
	(clock (or effective-clock (cons org-clock-marker
					 org-clock-start-time))))
    (unless (marker-buffer (car clock))
      (error "No clock is currently running"))
    (org-with-clock clock (org-clock-goto))
    (with-current-buffer (marker-buffer (car clock))
      (goto-char (car clock))
      (when drawer
	(org-with-wide-buffer
	 (let ((drawer-re (format "^[ \t]*:%s:[ \t]*$"
				  (regexp-quote (if (stringp drawer) drawer "LOGBOOK"))))
	       (beg (save-excursion (org-back-to-heading t) (point))))
	   (catch 'exit
	     (while (re-search-backward drawer-re beg t)
	       (let ((element (org-element-at-point)))
		 (when (eq (org-element-type element) 'drawer)
		   (when (> (org-element-property :end element) (car clock))
		     (org-flag-drawer nil element))
		   (throw 'exit nil)))))))))))

(defun org-clock-resolve (clock &optional prompt-fn last-valid fail-quietly)
  "Resolve an open Org clock.
An open clock was found, with `dangling' possibly being non-nil.
If this function was invoked with a prefix argument, non-dangling
open clocks are ignored.  The given clock requires some sort of
user intervention to resolve it, either because a clock was left
dangling or due to an idle timeout.  The clock resolution can
either be:

  (a) deleted, the user doesn't care about the clock
  (b) restarted from the current time (if no other clock is open)
  (c) closed, giving the clock X minutes
  (d) closed and then restarted
  (e) resumed, as if the user had never left

The format of clock is (CONS MARKER START-TIME), where MARKER
identifies the buffer and position the clock is open at (and
thus, the heading it's under), and START-TIME is when the clock
was started."
  (cl-assert clock)
  (let* ((ch
	  (save-window-excursion
	    (save-excursion
	      (unless org-clock-resolving-clocks-due-to-idleness
		(org-clock-jump-to-current-clock clock))
	      (unless org-clock-resolve-expert
		(with-output-to-temp-buffer "*Org Clock*"
		  (princ (format-message "Select a Clock Resolution Command:

i/q      Ignore this question; the same as keeping all the idle time.

k/K      Keep X minutes of the idle time (default is all).  If this
         amount is less than the default, you will be clocked out
         that many minutes after the time that idling began, and then
         clocked back in at the present time.

g/G      Indicate that you \"got back\" X minutes ago.  This is quite
         different from `k': it clocks you out from the beginning of
         the idle period and clock you back in X minutes ago.

s/S      Subtract the idle time from the current clock.  This is the
         same as keeping 0 minutes.

C        Cancel the open timer altogether.  It will be as though you
         never clocked in.

j/J      Jump to the current clock, to make manual adjustments.

For all these options, using uppercase makes your final state
to be CLOCKED OUT."))))
	      (org-fit-window-to-buffer (get-buffer-window "*Org Clock*"))
	      (let (char-pressed)
		(while (or (null char-pressed)
			   (and (not (memq char-pressed
					   '(?k ?K ?g ?G ?s ?S ?C
						?j ?J ?i ?q)))
				(or (ding) t)))
		  (setq char-pressed
			(read-char (concat (funcall prompt-fn clock)
					   " [jkKgGSscCiq]? ")
				   nil 45)))
		(and (not (memq char-pressed '(?i ?q))) char-pressed)))))
	 (default
	   (floor (/ (float-time
		      (time-subtract (current-time) last-valid)) 60)))
	 (keep
	  (and (memq ch '(?k ?K))
	       (read-number "Keep how many minutes? " default)))
	 (gotback
	  (and (memq ch '(?g ?G))
	       (read-number "Got back how many minutes ago? " default)))
	 (subtractp (memq ch '(?s ?S)))
	 (barely-started-p (< (- (float-time last-valid)
				 (float-time (cdr clock))) 45))
	 (start-over (and subtractp barely-started-p)))
    (cond
     ((memq ch '(?j ?J))
      (if (eq ch ?J)
	  (org-clock-resolve-clock clock 'now nil t nil fail-quietly))
      (org-clock-jump-to-current-clock clock))
     ((or (null ch)
	  (not (memq ch '(?k ?K ?g ?G ?s ?S ?C))))
      (message ""))
     (t
      (org-clock-resolve-clock
       clock (cond
	      ((or (eq ch ?C)
		   ;; If the time on the clock was less than a minute before
		   ;; the user went away, and they've ask to subtract all the
		   ;; time...
		   start-over)
	       nil)
	      ((or subtractp
		   (and gotback (= gotback 0)))
	       last-valid)
	      ((or (and keep (= keep default))
		   (and gotback (= gotback default)))
	       'now)
	      (keep
	       (time-add last-valid (seconds-to-time (* 60 keep))))
	      (gotback
	       (time-subtract (current-time)
			      (seconds-to-time (* 60 gotback))))
	      (t
	       (error "Unexpected, please report this as a bug")))
       (and gotback last-valid)
       (memq ch '(?K ?G ?S))
       (and start-over
	    (not (memq ch '(?K ?G ?S ?C))))
       fail-quietly)))))

;;;###autoload
(defun org-resolve-clocks (&optional only-dangling-p prompt-fn last-valid)
  "Resolve all currently open Org clocks.
If `only-dangling-p' is non-nil, only ask to resolve dangling
\(i.e., not currently open and valid) clocks."
  (interactive "P")
  (unless org-clock-resolving-clocks
    (let ((org-clock-resolving-clocks t))
      (dolist (file (org-files-list))
	(let ((clocks (org-find-open-clocks file)))
	  (dolist (clock clocks)
	    (let ((dangling (or (not (org-clock-is-active))
				(/= (car clock) org-clock-marker))))
	      (if (or (not only-dangling-p) dangling)
		  (org-clock-resolve
		   clock
		   (or prompt-fn
		       (function
			(lambda (clock)
			  (format
			   "Dangling clock started %d mins ago"
			   (floor (- (float-time)
				     (float-time (cdr clock)))
				  60)))))
		   (or last-valid
		       (cdr clock)))))))))))

(defun org-emacs-idle-seconds ()
  "Return the current Emacs idle time in seconds, or nil if not idle."
  (let ((idle-time (current-idle-time)))
    (if idle-time
	(float-time idle-time)
      0)))

(defun org-mac-idle-seconds ()
  "Return the current Mac idle time in seconds."
  (string-to-number (shell-command-to-string "ioreg -c IOHIDSystem | perl -ane 'if (/Idle/) {$idle=(pop @F)/1000000000; print $idle; last}'")))

(defvar org-x11idle-exists-p
  ;; Check that x11idle exists
  (and (eq window-system 'x)
       (eq 0 (call-process-shell-command
              (format "command -v %s" org-clock-x11idle-program-name)))
       ;; Check that x11idle can retrieve the idle time
       ;; FIXME: Why "..-shell-command" rather than just `call-process'?
       (eq 0 (call-process-shell-command org-clock-x11idle-program-name))))

(defun org-x11-idle-seconds ()
  "Return the current X11 idle time in seconds."
  (/ (string-to-number (shell-command-to-string org-clock-x11idle-program-name)) 1000))

(defun org-user-idle-seconds ()
  "Return the number of seconds the user has been idle for.
This routine returns a floating point number."
  (cond
   ((eq system-type 'darwin)
    (org-mac-idle-seconds))
   ((and (eq window-system 'x) org-x11idle-exists-p)
    (org-x11-idle-seconds))
   (t
    (org-emacs-idle-seconds))))

(defvar org-clock-user-idle-seconds)

(defun org-resolve-clocks-if-idle ()
  "Resolve all currently open Org clocks.
This is performed after `org-clock-idle-time' minutes, to check
if the user really wants to stay clocked in after being idle for
so long."
  (when (and org-clock-idle-time (not org-clock-resolving-clocks)
	     org-clock-marker (marker-buffer org-clock-marker))
    (let* ((org-clock-user-idle-seconds (org-user-idle-seconds))
	   (org-clock-user-idle-start
	    (time-subtract (current-time)
			   (seconds-to-time org-clock-user-idle-seconds)))
	   (org-clock-resolving-clocks-due-to-idleness t))
      (if (> org-clock-user-idle-seconds (* 60 org-clock-idle-time))
	  (org-clock-resolve
	   (cons org-clock-marker
		 org-clock-start-time)
	   (lambda (_)
	     (format "Clocked in & idle for %.1f mins"
		     (/ (float-time
			 (time-subtract (current-time)
					org-clock-user-idle-start))
			60.0)))
	   org-clock-user-idle-start)))))

(defvar org-clock-current-task nil "Task currently clocked in.")
(defvar org-clock-out-time nil) ; store the time of the last clock-out
(defvar org--msg-extra)

;;;###autoload
(defun org-clock-in (&optional select start-time)
  "Start the clock on the current item.

If necessary, clock-out of the currently active clock.

With a `\\[universal-argument]' prefix argument SELECT, offer a list of \
recently clocked
tasks to clock into.

When SELECT is `\\[universal-argument] \ \\[universal-argument]', \
clock into the current task and mark it as
the default task, a special task that will always be offered in the
clocking selection, associated with the letter `d'.

When SELECT is `\\[universal-argument] \\[universal-argument] \
\\[universal-argument]', clock in by using the last clock-out
time as the start time.  See `org-clock-continuously' to make this
the default behavior."
  (interactive "P")
  (setq org-clock-notification-was-shown nil)
  (org-refresh-effort-properties)
  (catch 'abort
    (let ((interrupting (and (not org-clock-resolving-clocks-due-to-idleness)
			     (org-clocking-p)))
	  ts selected-task target-pos (org--msg-extra "")
	  (leftover (and (not org-clock-resolving-clocks)
			 org-clock-leftover-time)))

      (when (and org-clock-auto-clock-resolution
		 (or (not interrupting)
		     (eq t org-clock-auto-clock-resolution))
		 (not org-clock-clocking-in)
		 (not org-clock-resolving-clocks))
	(setq org-clock-leftover-time nil)
	(let ((org-clock-clocking-in t))
	  (org-resolve-clocks)))    ; check if any clocks are dangling

      (when (equal select '(64))
	;; Set start-time to `org-clock-out-time'
	(let ((org-clock-continuously t))
	  (org-clock-in nil org-clock-out-time)))

      (when (equal select '(4))
	(setq selected-task (org-clock-select-task "Clock-in on task: "))
	(if selected-task
	    (setq selected-task (copy-marker selected-task))
	  (error "Abort")))

      (when (equal select '(16))
	;; Mark as default clocking task
	(org-clock-mark-default-task))

      (when interrupting
	;; We are interrupting the clocking of a different task.
	;; Save a marker to this task, so that we can go back.
	;; First check if we are trying to clock into the same task!
	(when (save-excursion
		(unless selected-task
		  (org-back-to-heading t))
		(and (equal (marker-buffer org-clock-hd-marker)
			    (if selected-task
				(marker-buffer selected-task)
			      (current-buffer)))
		     (= (marker-position org-clock-hd-marker)
			(if selected-task
			    (marker-position selected-task)
			  (point)))
		     (equal org-clock-current-task (nth 4 (org-heading-components)))))
	  (message "Clock continues in \"%s\"" org-clock-heading)
	  (throw 'abort nil))
	(move-marker org-clock-interrupted-task
		     (marker-position org-clock-marker)
		     (marker-buffer org-clock-marker))
	(let ((org-clock-clocking-in t))
	  (org-clock-out nil t)))

      ;; Clock in at which position?
      (setq target-pos
	    (if (and (eobp) (not (org-at-heading-p)))
		(point-at-bol 0)
	      (point)))
      (save-excursion
	(when (and selected-task (marker-buffer selected-task))
	  ;; There is a selected task, move to the correct buffer
	  ;; and set the new target position.
	  (set-buffer (org-base-buffer (marker-buffer selected-task)))
	  (setq target-pos (marker-position selected-task))
	  (move-marker selected-task nil))
	(org-with-wide-buffer
	 (goto-char target-pos)
	 (org-back-to-heading t)
	 (or interrupting (move-marker org-clock-interrupted-task nil))
	 (run-hooks 'org-clock-in-prepare-hook)
	 (org-clock-history-push)
	 (setq org-clock-current-task (nth 4 (org-heading-components)))
	 (cond ((functionp org-clock-in-switch-to-state)
		(let ((case-fold-search nil))
		  (looking-at org-complex-heading-regexp))
		(let ((newstate (funcall org-clock-in-switch-to-state
					 (match-string 2))))
		  (when newstate (org-todo newstate))))
	       ((and org-clock-in-switch-to-state
		     (not (looking-at (concat org-outline-regexp "[ \t]*"
					      org-clock-in-switch-to-state
					      "\\>"))))
		(org-todo org-clock-in-switch-to-state)))
	 (setq org-clock-heading
	       (cond ((and org-clock-heading-function
			   (functionp org-clock-heading-function))
		      (funcall org-clock-heading-function))
		     ((nth 4 (org-heading-components))
		      (replace-regexp-in-string
		       "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1"
		       (match-string-no-properties 4)))
		     (t "???")))
	 (org-clock-find-position org-clock-in-resume)
	 (cond
	  ((and org-clock-in-resume
		(looking-at
		 (concat "^[ \t]*" org-clock-string
			 " \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
			 " *\\sw+.? +[012][0-9]:[0-5][0-9]\\)\\][ \t]*$")))
	   (message "Matched %s" (match-string 1))
	   (setq ts (concat "[" (match-string 1) "]"))
	   (goto-char (match-end 1))
	   (setq org-clock-start-time
		 (apply 'encode-time
			(org-parse-time-string (match-string 1))))
	   (setq org-clock-effort (org-entry-get (point) org-effort-property))
	   (setq org-clock-total-time (org-clock-sum-current-item
				       (org-clock-get-sum-start))))
	  ((eq org-clock-in-resume 'auto-restart)
	   ;; called from org-clock-load during startup,
	   ;; do not interrupt, but warn!
	   (message "Cannot restart clock because task does not contain unfinished clock")
	   (ding)
	   (sit-for 2)
	   (throw 'abort nil))
	  (t
	   (insert-before-markers "\n")
	   (backward-char 1)
	   (org-indent-line)
	   (when (and (save-excursion
			(end-of-line 0)
			(org-in-item-p)))
	     (beginning-of-line 1)
	     (indent-line-to (- (org-get-indentation) 2)))
	   (insert org-clock-string " ")
	   (setq org-clock-effort (org-entry-get (point) org-effort-property))
	   (setq org-clock-total-time (org-clock-sum-current-item
				       (org-clock-get-sum-start)))
	   (setq org-clock-start-time
		 (or (and org-clock-continuously org-clock-out-time)
		     (and leftover
			  (y-or-n-p
			   (format
			    "You stopped another clock %d mins ago; start this one from then? "
			    (/ (- (float-time
				   (org-current-time org-clock-rounding-minutes t))
				  (float-time leftover))
			       60)))
			  leftover)
		     start-time
		     (org-current-time org-clock-rounding-minutes t)))
	   (setq ts (org-insert-time-stamp org-clock-start-time
					   'with-hm 'inactive))))
	 (move-marker org-clock-marker (point) (buffer-base-buffer))
	 (move-marker org-clock-hd-marker
		      (save-excursion (org-back-to-heading t) (point))
		      (buffer-base-buffer))
	 (setq org-clock-has-been-used t)
	 ;; add to mode line
	 (when (or (eq org-clock-clocked-in-display 'mode-line)
		   (eq org-clock-clocked-in-display 'both))
	   (or global-mode-string (setq global-mode-string '("")))
	   (or (memq 'org-mode-line-string global-mode-string)
	       (setq global-mode-string
		     (append global-mode-string '(org-mode-line-string)))))
	 ;; add to frame title
	 (when (or (eq org-clock-clocked-in-display 'frame-title)
		   (eq org-clock-clocked-in-display 'both))
	   (setq frame-title-format org-clock-frame-title-format))
	 (org-clock-update-mode-line)
	 (when org-clock-mode-line-timer
	   (cancel-timer org-clock-mode-line-timer)
	   (setq org-clock-mode-line-timer nil))
	 (when org-clock-clocked-in-display
	   (setq org-clock-mode-line-timer
		 (run-with-timer org-clock-update-period
				 org-clock-update-period
				 'org-clock-update-mode-line)))
	 (when org-clock-idle-timer
	   (cancel-timer org-clock-idle-timer)
	   (setq org-clock-idle-timer nil))
	 (setq org-clock-idle-timer
	       (run-with-timer 60 60 'org-resolve-clocks-if-idle))
	 (message "Clock starts at %s - %s" ts org--msg-extra)
	 (run-hooks 'org-clock-in-hook))))))

;;;###autoload
(defun org-clock-in-last (&optional arg)
  "Clock in the last closed clocked item.
When already clocking in, send an warning.
With a universal prefix argument, select the task you want to
clock in from the last clocked in tasks.
With two universal prefix arguments, start clocking using the
last clock-out time, if any.
With three universal prefix arguments, interactively prompt
for a todo state to switch to, overriding the existing value
`org-clock-in-switch-to-state'."
  (interactive "P")
  (if (equal arg '(4)) (org-clock-in arg)
    (let ((start-time (if (or org-clock-continuously (equal arg '(16)))
			  (or org-clock-out-time
			      (org-current-time org-clock-rounding-minutes t))
			(org-current-time org-clock-rounding-minutes t))))
      (if (null org-clock-history)
	  (message "No last clock")
	(let ((org-clock-in-switch-to-state
	       (if (and (not org-clock-current-task) (equal arg '(64)))
		   (completing-read "Switch to state: "
				    (and org-clock-history
					 (with-current-buffer
					     (marker-buffer (car org-clock-history))
					   org-todo-keywords-1)))
		 org-clock-in-switch-to-state))
	      (already-clocking org-clock-current-task))
	  (org-clock-clock-in (list (car org-clock-history)) nil start-time)
	  (or already-clocking
	      ;; Don't display a message if we are already clocking in
	      (message "Clocking back: %s (in %s)"
		       org-clock-current-task
		       (buffer-name (marker-buffer org-clock-marker)))))))))

(defun org-clock-mark-default-task ()
  "Mark current task as default task."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (move-marker org-clock-default-task (point))))

(defun org-clock-get-sum-start ()
  "Return the time from which clock times should be counted.

This is for the currently running clock as it is displayed in the
mode line.  This function looks at the properties LAST_REPEAT and
in particular CLOCK_MODELINE_TOTAL and the corresponding variable
`org-clock-mode-line-total' and then decides which time to use.

The time is always returned as UTC."
  (let ((cmt (or (org-entry-get nil "CLOCK_MODELINE_TOTAL")
		 (symbol-name org-clock-mode-line-total)))
	(lr (org-entry-get nil "LAST_REPEAT")))
    (cond
     ((equal cmt "current")
      (setq org--msg-extra "showing time in current clock instance")
      (current-time))
     ((equal cmt "today")
      (setq org--msg-extra "showing today's task time.")
      (let* ((dt (org-decode-time nil t))
	     (hour (nth 2 dt))
	     (day (nth 3 dt)))
	(if (< hour org-extend-today-until) (setf (nth 3 dt) (1- day)))
	(setf (nth 2 dt) org-extend-today-until)
	(setq dt (append (list 0 0) (nthcdr 2 dt) '(t)))
	(apply #'encode-time dt)))
     ((or (equal cmt "all")
	  (and (or (not cmt) (equal cmt "auto"))
	       (not lr)))
      (setq org--msg-extra "showing entire task time.")
      nil)
     ((or (equal cmt "repeat")
	  (and (or (not cmt) (equal cmt "auto"))
	       lr))
      (setq org--msg-extra "showing task time since last repeat.")
      (and lr (org-time-string-to-time lr t)))
     (t nil))))

(defun org-clock-find-position (find-unclosed)
  "Find the location where the next clock line should be inserted.
When FIND-UNCLOSED is non-nil, first check if there is an unclosed clock
line and position cursor in that line."
  (org-back-to-heading t)
  (catch 'exit
    (let* ((beg (line-beginning-position))
	   (end (save-excursion (outline-next-heading) (point)))
	   (org-clock-into-drawer (org-clock-into-drawer))
	   (drawer (org-clock-drawer-name)))
      ;; Look for a running clock if FIND-UNCLOSED in non-nil.
      (when find-unclosed
	(let ((open-clock-re
	       (concat "^[ \t]*"
		       org-clock-string
		       " \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
		       " *\\sw+ +[012][0-9]:[0-5][0-9]\\)\\][ \t]*$")))
	  (while (re-search-forward open-clock-re end t)
	    (let ((element (org-element-at-point)))
	      (when (and (eq (org-element-type element) 'clock)
			 (eq (org-element-property :status element) 'running))
		(beginning-of-line)
		(throw 'exit t))))))
      ;; Look for an existing clock drawer.
      (when drawer
	(goto-char beg)
	(let ((drawer-re (concat "^[ \t]*:" (regexp-quote drawer) ":[ \t]*$")))
	  (while (re-search-forward drawer-re end t)
	    (let ((element (org-element-at-point)))
	      (when (eq (org-element-type element) 'drawer)
		(let ((cend (org-element-property :contents-end element)))
		  (if (and (not org-log-states-order-reversed) cend)
		      (goto-char cend)
		    (forward-line))
		  (throw 'exit t)))))))
      (goto-char beg)
      (let ((clock-re (concat "^[ \t]*" org-clock-string))
	    (count 0)
	    positions)
	;; Count the CLOCK lines and store their positions.
	(save-excursion
	  (while (re-search-forward clock-re end t)
	    (let ((element (org-element-at-point)))
	      (when (eq (org-element-type element) 'clock)
		(setq positions (cons (line-beginning-position) positions)
		      count (1+ count))))))
	(cond
	 ((null positions)
	  ;; Skip planning line and property drawer, if any.
	  (org-end-of-meta-data)
	  (unless (bolp) (insert "\n"))
	  ;; Create a new drawer if necessary.
	  (when (and org-clock-into-drawer
		     (or (not (wholenump org-clock-into-drawer))
			 (< org-clock-into-drawer 2)))
	    (let ((beg (point)))
	      (insert ":" drawer ":\n:END:\n")
	      (org-indent-region beg (point))
	      (goto-char beg)
	      (org-flag-drawer t)
	      (forward-line))))
	 ;; When a clock drawer needs to be created because of the
	 ;; number of clock items or simply if it is missing, collect
	 ;; all clocks in the section and wrap them within the drawer.
	 ((if (wholenump org-clock-into-drawer)
	      (>= (1+ count) org-clock-into-drawer)
	    drawer)
	  ;; Skip planning line and property drawer, if any.
	  (org-end-of-meta-data)
	  (let ((beg (point)))
	    (insert
	     (mapconcat
	      (lambda (p)
		(save-excursion
		  (goto-char p)
		  (org-trim (delete-and-extract-region
			     (save-excursion (skip-chars-backward " \r\t\n")
					     (line-beginning-position 2))
			     (line-beginning-position 2)))))
	      positions "\n")
	     "\n:END:\n")
	    (let ((end (point-marker)))
	      (goto-char beg)
	      (save-excursion (insert ":" drawer ":\n"))
	      (org-flag-drawer t)
	      (org-indent-region (point) end)
	      (forward-line)
	      (unless org-log-states-order-reversed
		(goto-char end)
		(beginning-of-line -1))
	      (set-marker end nil))))
	 (org-log-states-order-reversed (goto-char (car (last positions))))
	 (t (goto-char (car positions))))))))

;;;###autoload
(defun org-clock-out (&optional switch-to-state fail-quietly at-time)
  "Stop the currently running clock.
Throw an error if there is no running clock and FAIL-QUIETLY is nil.
With a universal prefix, prompt for a state to switch the clocked out task
to, overriding the existing value of `org-clock-out-switch-to-state'."
  (interactive "P")
  (catch 'exit
    (when (not (org-clocking-p))
      (setq global-mode-string
	    (delq 'org-mode-line-string global-mode-string))
      (setq frame-title-format org-frame-title-format-backup)
      (force-mode-line-update)
      (if fail-quietly (throw 'exit t) (user-error "No active clock")))
    (let ((org-clock-out-switch-to-state
	   (if switch-to-state
	       (completing-read "Switch to state: "
				(with-current-buffer
				    (marker-buffer org-clock-marker)
				  org-todo-keywords-1)
				nil t "DONE")
	     org-clock-out-switch-to-state))
	  (now (org-current-time org-clock-rounding-minutes))
	  ts te s h m remove)
      (setq org-clock-out-time now)
      (save-excursion ; Do not replace this with `with-current-buffer'.
	(with-no-warnings (set-buffer (org-clocking-buffer)))
	(save-restriction
	  (widen)
	  (goto-char org-clock-marker)
	  (beginning-of-line 1)
	  (if (and (looking-at (concat "[ \t]*" org-keyword-time-regexp))
		   (equal (match-string 1) org-clock-string))
	      (setq ts (match-string 2))
	    (if fail-quietly (throw 'exit nil) (error "Clock start time is gone")))
	  (goto-char (match-end 0))
	  (delete-region (point) (point-at-eol))
	  (insert "--")
	  (setq te (org-insert-time-stamp (or at-time now) 'with-hm 'inactive))
	  (setq s (- (float-time
		      (apply #'encode-time (org-parse-time-string te nil t)))
		     (float-time
		      (apply #'encode-time (org-parse-time-string ts nil t))))
		h (floor (/ s 3600))
		s (- s (* 3600 h))
		m (floor (/ s 60))
		s (- s (* 60 s)))
	  (insert " => " (format "%2d:%02d" h m))
	  (move-marker org-clock-marker nil)
	  (move-marker org-clock-hd-marker nil)
	  ;; Possibly remove zero time clocks.  However, do not add
	  ;; a note associated to the CLOCK line in this case.
	  (cond ((and org-clock-out-remove-zero-time-clocks
		      (= (+ h m) 0))
		 (setq remove t)
		 (delete-region (line-beginning-position)
				(line-beginning-position 2)))
		(org-log-note-clock-out
		 (org-add-log-setup
		  'clock-out nil nil nil
		  (concat "# Task: " (org-get-heading t) "\n\n"))))
	  (when org-clock-mode-line-timer
	    (cancel-timer org-clock-mode-line-timer)
	    (setq org-clock-mode-line-timer nil))
	  (when org-clock-idle-timer
	    (cancel-timer org-clock-idle-timer)
	    (setq org-clock-idle-timer nil))
	  (setq global-mode-string
		(delq 'org-mode-line-string global-mode-string))
	  (setq frame-title-format org-frame-title-format-backup)
	  (when org-clock-out-switch-to-state
	    (save-excursion
	      (org-back-to-heading t)
	      (let ((org-clock-out-when-done nil))
		(cond
		 ((functionp org-clock-out-switch-to-state)
		  (let ((case-fold-search nil))
		    (looking-at org-complex-heading-regexp))
		  (let ((newstate (funcall org-clock-out-switch-to-state
					   (match-string 2))))
		    (when newstate (org-todo newstate))))
		 ((and org-clock-out-switch-to-state
		       (not (looking-at (concat org-outline-regexp "[ \t]*"
						org-clock-out-switch-to-state
						"\\>"))))
		  (org-todo org-clock-out-switch-to-state))))))
	  (force-mode-line-update)
	  (message (concat "Clock stopped at %s after "
			   (org-duration-from-minutes (+ (* 60 h) m)) "%s")
		   te (if remove " => LINE REMOVED" ""))
	  (run-hooks 'org-clock-out-hook)
	  (unless (org-clocking-p)
	    (setq org-clock-current-task nil)))))))

(add-hook 'org-clock-out-hook 'org-clock-remove-empty-clock-drawer)

(defun org-clock-remove-empty-clock-drawer ()
  "Remove empty clock drawers in current subtree."
  (save-excursion
    (org-back-to-heading t)
    (org-map-tree
     (lambda ()
       (let ((drawer (org-clock-drawer-name))
	     (case-fold-search t))
	 (when drawer
	   (let ((re (format "^[ \t]*:%s:[ \t]*$" (regexp-quote drawer)))
		 (end (save-excursion (outline-next-heading))))
	     (while (re-search-forward re end t)
	       (org-remove-empty-drawer-at (point))))))))))

(defun org-clock-timestamps-up (&optional n)
  "Increase CLOCK timestamps at cursor.
Optional argument N tells to change by that many units."
  (interactive "P")
  (org-clock-timestamps-change 'up n))

(defun org-clock-timestamps-down (&optional n)
  "Increase CLOCK timestamps at cursor.
Optional argument N tells to change by that many units."
  (interactive "P")
  (org-clock-timestamps-change 'down n))

(defun org-clock-timestamps-change (updown &optional n)
  "Change CLOCK timestamps synchronously at cursor.
UPDOWN tells whether to change `up' or `down'.
Optional argument N tells to change by that many units."
  (let ((tschange (if (eq updown 'up) 'org-timestamp-up
		    'org-timestamp-down))
	(timestamp? (org-at-timestamp-p 'lax))
	ts1 begts1 ts2 begts2 updatets1 tdiff)
    (when timestamp?
      (save-excursion
	(move-beginning-of-line 1)
	(re-search-forward org-ts-regexp3 nil t)
	(setq ts1 (match-string 0) begts1 (match-beginning 0))
	(when (re-search-forward org-ts-regexp3 nil t)
	  (setq ts2 (match-string 0) begts2 (match-beginning 0))))
      ;; Are we on the second timestamp?
      (if (<= begts2 (point)) (setq updatets1 t))
      (if (not ts2)
	  ;; fall back on org-timestamp-up if there is only one
	  (funcall tschange n)
	(funcall tschange n)
	(let ((ts (if updatets1 ts2 ts1))
	      (begts (if updatets1 begts1 begts2)))
	  (setq tdiff
		(time-subtract
		 (org-time-string-to-time org-last-changed-timestamp t)
		 (org-time-string-to-time ts t)))
	  (save-excursion
	    (goto-char begts)
	    (org-timestamp-change
	     (round (/ (float-time tdiff)
		       (pcase timestamp?
			 (`minute 60)
			 (`hour 3600)
			 (`day (* 24 3600))
			 (`month (* 24 3600 31))
			 (`year (* 24 3600 365.2)))))
	     timestamp? 'updown)))))))

;;;###autoload
(defun org-clock-cancel ()
  "Cancel the running clock by removing the start timestamp."
  (interactive)
  (when (not (org-clocking-p))
    (setq global-mode-string
	  (delq 'org-mode-line-string global-mode-string))
    (setq frame-title-format org-frame-title-format-backup)
    (force-mode-line-update)
    (error "No active clock"))
  (save-excursion    ; Do not replace this with `with-current-buffer'.
    (with-no-warnings (set-buffer (org-clocking-buffer)))
    (goto-char org-clock-marker)
    (if (looking-back (concat "^[ \t]*" org-clock-string ".*")
		      (line-beginning-position))
	(progn (delete-region (1- (point-at-bol)) (point-at-eol))
	       (org-remove-empty-drawer-at (point)))
      (message "Clock gone, cancel the timer anyway")
      (sit-for 2)))
  (move-marker org-clock-marker nil)
  (move-marker org-clock-hd-marker nil)
  (setq global-mode-string
	(delq 'org-mode-line-string global-mode-string))
  (setq frame-title-format org-frame-title-format-backup)
  (force-mode-line-update)
  (message "Clock canceled")
  (run-hooks 'org-clock-cancel-hook))

;;;###autoload
(defun org-clock-goto (&optional select)
  "Go to the currently clocked-in entry, or to the most recently clocked one.
With prefix arg SELECT, offer recently clocked tasks for selection."
  (interactive "@P")
  (let* ((recent nil)
	 (m (cond
	     (select
	      (or (org-clock-select-task "Select task to go to: ")
		  (error "No task selected")))
	     ((org-clocking-p) org-clock-marker)
	     ((and org-clock-goto-may-find-recent-task
		   (car org-clock-history)
		   (marker-buffer (car org-clock-history)))
	      (setq recent t)
	      (car org-clock-history))
	     (t (error "No active or recent clock task")))))
    (pop-to-buffer-same-window (marker-buffer m))
    (if (or (< m (point-min)) (> m (point-max))) (widen))
    (goto-char m)
    (org-show-entry)
    (org-back-to-heading t)
    (org-cycle-hide-drawers 'children)
    (recenter org-clock-goto-before-context)
    (org-reveal)
    (if recent
	(message "No running clock, this is the most recently clocked task"))
    (run-hooks 'org-clock-goto-hook)))

(defvar-local org-clock-file-total-minutes nil
  "Holds the file total time in minutes, after a call to `org-clock-sum'.")

(defun org-clock-sum-today (&optional headline-filter)
  "Sum the times for each subtree for today."
  (let ((range (org-clock-special-range 'today)))
    (org-clock-sum (car range) (cadr range)
		   headline-filter :org-clock-minutes-today)))

(defun org-clock-sum-custom (&optional headline-filter range propname)
  "Sum the times for each subtree for today."
  (let ((r (or (and (symbolp range) (org-clock-special-range range))
	       (org-clock-special-range
		(intern (completing-read
			 "Range: "
			 '("today" "yesterday" "thisweek" "lastweek"
			   "thismonth" "lastmonth" "thisyear" "lastyear"
			   "interactive")
			 nil t))))))
    (org-clock-sum (car r) (cadr r)
		   headline-filter (or propname :org-clock-minutes-custom))))

;;;###autoload
(defun org-clock-sum (&optional tstart tend headline-filter propname)
  "Sum the times for each subtree.
Puts the resulting times in minutes as a text property on each headline.
TSTART and TEND can mark a time range to be considered.
HEADLINE-FILTER is a zero-arg function that, if specified, is called for
each headline in the time range with point at the headline.  Headlines for
which HEADLINE-FILTER returns nil are excluded from the clock summation.
PROPNAME lets you set a custom text property instead of :org-clock-minutes."
  (org-with-silent-modifications
   (let* ((re (concat "^\\(\\*+\\)[ \t]\\|^[ \t]*"
		      org-clock-string
		      "[ \t]*\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\|=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)\\)"))
	  (lmax 30)
	  (ltimes (make-vector lmax 0))
	  (level 0)
	  (tstart (cond ((stringp tstart) (org-time-string-to-seconds tstart t))
			((consp tstart) (float-time tstart))
			(t tstart)))
	  (tend (cond ((stringp tend) (org-time-string-to-seconds tend t))
		      ((consp tend) (float-time tend))
		      (t tend)))
	  (t1 0)
	  time)
     (remove-text-properties (point-min) (point-max)
			     `(,(or propname :org-clock-minutes) t
			       :org-clock-force-headline-inclusion t))
     (save-excursion
       (goto-char (point-max))
       (while (re-search-backward re nil t)
	 (cond
	  ((match-end 2)
	   ;; Two time stamps.
	   (let* ((ts (float-time
		       (apply #'encode-time
			      (save-match-data
				(org-parse-time-string
				 (match-string 2) nil t)))))
		  (te (float-time
		       (apply #'encode-time
			      (org-parse-time-string (match-string 3) nil t))))
		  (dt (- (if tend (min te tend) te)
			 (if tstart (max ts tstart) ts))))
	     (when (> dt 0) (cl-incf t1 (floor (/ dt 60))))))
	  ((match-end 4)
	   ;; A naked time.
	   (setq t1 (+ t1 (string-to-number (match-string 5))
		       (* 60 (string-to-number (match-string 4))))))
	  (t	 ;A headline
	   ;; Add the currently clocking item time to the total.
	   (when (and org-clock-report-include-clocking-task
		      (eq (org-clocking-buffer) (current-buffer))
		      (eq (marker-position org-clock-hd-marker) (point))
		      tstart
		      tend
		      (>= (float-time org-clock-start-time) tstart)
		      (<= (float-time org-clock-start-time) tend))
	     (let ((time (floor (- (float-time)
				   (float-time org-clock-start-time))
				60)))
	       (setq t1 (+ t1 time))))
	   (let* ((headline-forced
		   (get-text-property (point)
				      :org-clock-force-headline-inclusion))
		  (headline-included
		   (or (null headline-filter)
		       (save-excursion
			 (save-match-data (funcall headline-filter))))))
	     (setq level (- (match-end 1) (match-beginning 1)))
	     (when (>= level lmax)
	       (setq ltimes (vconcat ltimes (make-vector lmax 0)) lmax (* 2 lmax)))
	     (when (or (> t1 0) (> (aref ltimes level) 0))
	       (when (or headline-included headline-forced)
		 (if headline-included
		     (cl-loop for l from 0 to level do
			      (aset ltimes l (+ (aref ltimes l) t1))))
		 (setq time (aref ltimes level))
		 (goto-char (match-beginning 0))
		 (put-text-property (point) (point-at-eol)
				    (or propname :org-clock-minutes) time)
		 (when headline-filter
		   (save-excursion
		     (save-match-data
		       (while (org-up-heading-safe)
			 (put-text-property
			  (point) (line-end-position)
			  :org-clock-force-headline-inclusion t))))))
	       (setq t1 0)
	       (cl-loop for l from level to (1- lmax) do
			(aset ltimes l 0)))))))
       (setq org-clock-file-total-minutes (aref ltimes 0))))))

(defun org-clock-sum-current-item (&optional tstart)
  "Return time, clocked on current item in total."
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (org-clock-sum tstart)
      org-clock-file-total-minutes)))

;;;###autoload
(defun org-clock-display (&optional arg)
  "Show subtree times in the entire buffer.

By default, show the total time for the range defined in
`org-clock-display-default-range'.  With `\\[universal-argument]' \
prefix, show
the total time for today instead.

With `\\[universal-argument] \\[universal-argument]' prefix, \
use a custom range, entered at prompt.

With `\\[universal-argument] \ \\[universal-argument] \
\\[universal-argument]' prefix, display the total time in the
echo area.

Use `\\[org-clock-remove-overlays]' to remove the subtree times."
  (interactive "P")
  (org-clock-remove-overlays)
  (let* ((todayp (equal arg '(4)))
	 (customp (member arg '((16) today yesterday
				thisweek lastweek thismonth
				lastmonth thisyear lastyear
				untilnow interactive)))
	 (prop (cond ((not arg) :org-clock-minutes-default)
		     (todayp :org-clock-minutes-today)
		     (customp :org-clock-minutes-custom)
		     (t :org-clock-minutes)))
	 time h m p)
    (cond ((not arg) (org-clock-sum-custom
		      nil org-clock-display-default-range prop))
	  (todayp (org-clock-sum-today))
	  (customp (org-clock-sum-custom nil arg))
	  (t (org-clock-sum)))
    (unless (eq arg '(64))
      (save-excursion
	(goto-char (point-min))
	(while (or (and (equal (setq p (point)) (point-min))
			(get-text-property p prop))
		   (setq p (next-single-property-change
			    (point) prop)))
	  (goto-char p)
	  (when (setq time (get-text-property p prop))
	    (org-clock-put-overlay time)))
	(setq h (/ org-clock-file-total-minutes 60)
	      m (- org-clock-file-total-minutes (* 60 h)))
	;; Arrange to remove the overlays upon next change.
	(when org-remove-highlights-with-change
	  (add-hook 'before-change-functions 'org-clock-remove-overlays
			nil 'local))))
    (message (concat (format "Total file time%s: "
			     (cond (todayp " for today")
				   (customp " (custom)")
				   (t "")))
		     (org-duration-from-minutes
		      org-clock-file-total-minutes)
		     " (%d hours and %d minutes)")
	     h m)))

(defvar-local org-clock-overlays nil)

(defun org-clock-put-overlay (time)
  "Put an overlays on the current line, displaying TIME.
This creates a new overlay and stores it in `org-clock-overlays', so that it
will be easy to remove."
  (let (ov tx)
    (beginning-of-line)
    (let ((case-fold-search nil))
      (when (looking-at org-complex-heading-regexp)
	(goto-char (match-beginning 4))))
    (setq ov (make-overlay (point) (point-at-eol))
	  tx (concat (buffer-substring-no-properties (point) (match-end 4))
		     (org-add-props
			 (make-string
			  (max 0 (- (- 60 (current-column))
				    (- (match-end 4) (match-beginning 4))
				    (length (org-get-at-bol 'line-prefix))))
			  ?\·)
			 '(face shadow))
		     (org-add-props
			 (format " %9s " (org-duration-from-minutes time))
			 '(face org-clock-overlay))
		     ""))
    (overlay-put ov 'display tx)
    (push ov org-clock-overlays)))

;;;###autoload
(defun org-clock-remove-overlays (&optional _beg _end noremove)
  "Remove the occur highlights from the buffer.
If NOREMOVE is nil, remove this function from the
`before-change-functions' in the current buffer."
  (interactive)
  (unless org-inhibit-highlight-removal
    (mapc #'delete-overlay org-clock-overlays)
    (setq org-clock-overlays nil)
    (unless noremove
      (remove-hook 'before-change-functions
		   'org-clock-remove-overlays 'local))))

(defvar org-state) ;; dynamically scoped into this function
(defun org-clock-out-if-current ()
  "Clock out if the current entry contains the running clock.
This is used to stop the clock after a TODO entry is marked DONE,
and is only done if the variable `org-clock-out-when-done' is not nil."
  (when (and (org-clocking-p)
	     org-clock-out-when-done
	     (marker-buffer org-clock-marker)
	     (or (and (eq t org-clock-out-when-done)
		      (member org-state org-done-keywords))
		 (and (listp org-clock-out-when-done)
		      (member org-state org-clock-out-when-done)))
	     (equal (or (buffer-base-buffer (org-clocking-buffer))
			(org-clocking-buffer))
		    (or (buffer-base-buffer (current-buffer))
			(current-buffer)))
	     (< (point) org-clock-marker)
	     (> (save-excursion (outline-next-heading) (point))
		org-clock-marker))
    ;; Clock out, but don't accept a logging message for this.
    (let ((org-log-note-clock-out nil)
	  (org-clock-out-switch-to-state nil))
      (org-clock-out))))

(add-hook 'org-after-todo-state-change-hook
	  'org-clock-out-if-current)

;;;###autoload
(defun org-clock-get-clocktable (&rest props)
  "Get a formatted clocktable with parameters according to PROPS.
The table is created in a temporary buffer, fully formatted and
fontified, and then returned."
  ;; Set the defaults
  (setq props (plist-put props :name "clocktable"))
  (unless (plist-member props :maxlevel)
    (setq props (plist-put props :maxlevel 2)))
  (unless (plist-member props :scope)
    (setq props (plist-put props :scope 'agenda)))
  (with-temp-buffer
    (org-mode)
    (org-create-dblock props)
    (org-update-dblock)
    (org-font-lock-ensure)
    (forward-line 2)
    (buffer-substring (point) (progn
				(re-search-forward "^[ \t]*#\\+END" nil t)
				(point-at-bol)))))

;;;###autoload
(defun org-clock-report (&optional arg)
  "Create a table containing a report about clocked time.
If the cursor is inside an existing clocktable block, then the table
will be updated.  If not, a new clocktable will be inserted.  The scope
of the new clock will be subtree when called from within a subtree, and
file elsewhere.

When called with a prefix argument, move to the first clock table in the
buffer and update it."
  (interactive "P")
  (org-clock-remove-overlays)
  (when arg
    (org-find-dblock "clocktable")
    (org-show-entry))
  (if (org-in-clocktable-p)
      (goto-char (org-in-clocktable-p))
    (let ((props (if (ignore-errors
		       (save-excursion (org-back-to-heading)))
		     (list :name "clocktable" :scope 'subtree)
		   (list :name "clocktable"))))
      (org-create-dblock
       (org-combine-plists org-clock-clocktable-default-properties props))))
  (org-update-dblock))

(defun org-day-of-week (day month year)
  "Returns the day of the week as an integer."
  (nth 6
       (decode-time
	(date-to-time
	 (format "%d-%02d-%02dT00:00:00" year month day)))))

(defun org-quarter-to-date (quarter year)
  "Get the date (week day year) of the first day of a given quarter."
  (let (startday)
    (cond
     ((= quarter 1)
      (setq startday (org-day-of-week 1 1 year))
      (cond
       ((= startday 0)
	(list 52 7 (- year 1)))
       ((= startday 6)
	(list 52 6 (- year 1)))
       ((<= startday 4)
	(list 1 startday year))
       ((> startday 4)
	(list 53 startday (- year 1)))
       )
      )
     ((= quarter 2)
      (setq startday (org-day-of-week 1 4 year))
      (cond
       ((= startday 0)
	(list 13 startday year))
       ((< startday 4)
	(list 14 startday year))
       ((>= startday 4)
	(list 13 startday year))
       )
      )
     ((= quarter 3)
      (setq startday (org-day-of-week 1 7 year))
      (cond
       ((= startday 0)
	(list 26 startday year))
       ((< startday 4)
	(list 27 startday year))
       ((>= startday 4)
	(list 26 startday year))
       )
      )
     ((= quarter 4)
      (setq startday (org-day-of-week 1 10 year))
      (cond
       ((= startday 0)
	(list 39 startday year))
       ((<= startday 4)
	(list 40 startday year))
       ((> startday 4)
	(list 39 startday year)))))))

(defun org-clock-special-range (key &optional time as-strings wstart mstart)
  "Return two times bordering a special time range.

KEY is a symbol specifying the range and can be one of `today',
`yesterday', `thisweek', `lastweek', `thismonth', `lastmonth',
`thisyear', `lastyear' or `untilnow'.  If set to `interactive',
user is prompted for range boundaries.  It can be a string or an
integer.

By default, a week starts Monday 0:00 and ends Sunday 24:00.  The
range is determined relative to TIME, which defaults to current
time.

The return value is a list containing two internal times, one for
the beginning of the range and one for its end, like the ones
returned by `current time' or `encode-time' and a string used to
display information.  If AS-STRINGS is non-nil, the returned
times will be formatted strings.

If WSTART is non-nil, use this number to specify the starting day
of a week (monday is 1).  If MSTART is non-nil, use this number
to specify the starting day of a month (1 is the first day of the
month).  If you can combine both, the month starting day will
have priority."
  (let* ((tm (decode-time time))
	 (m (nth 1 tm))
	 (h (nth 2 tm))
	 (d (nth 3 tm))
	 (month (nth 4 tm))
	 (y (nth 5 tm))
	 (dow (nth 6 tm))
	 (skey (format "%s" key))
	 (shift 0)
	 (q (cond ((>= month 10) 4)
		  ((>= month 7) 3)
		  ((>= month 4) 2)
		  (t 1)))
	 m1 h1 d1 month1 y1 shiftedy shiftedm shiftedq)
    (cond
     ((string-match "\\`[0-9]+\\'" skey)
      (setq y (string-to-number skey) month 1 d 1 key 'year))
     ((string-match "\\`\\([0-9]+\\)-\\([0-9]\\{1,2\\}\\)\\'" skey)
      (setq y (string-to-number (match-string 1 skey))
	    month (string-to-number (match-string 2 skey))
	    d 1
	    key 'month))
     ((string-match "\\`\\([0-9]+\\)-[wW]\\([0-9]\\{1,2\\}\\)\\'" skey)
      (require 'cal-iso)
      (let ((date (calendar-gregorian-from-absolute
		   (calendar-iso-to-absolute
		    (list (string-to-number (match-string 2 skey))
			  1
			  (string-to-number (match-string 1 skey)))))))
	(setq d (nth 1 date)
	      month (car date)
	      y (nth 2 date)
	      dow 1
	      key 'week)))
     ((string-match "\\`\\([0-9]+\\)-[qQ]\\([1-4]\\)\\'" skey)
      (require 'cal-iso)
      (setq q (string-to-number (match-string 2 skey)))
      (let ((date (calendar-gregorian-from-absolute
		   (calendar-iso-to-absolute
		    (org-quarter-to-date
		     q (string-to-number (match-string 1 skey)))))))
	(setq d (nth 1 date)
	      month (car date)
	      y (nth 2 date)
	      dow 1
	      key 'quarter)))
     ((string-match
       "\\`\\([0-9]+\\)-\\([0-9]\\{1,2\\}\\)-\\([0-9]\\{1,2\\}\\)\\'"
       skey)
      (setq y (string-to-number (match-string 1 skey))
	    month (string-to-number (match-string 2 skey))
	    d (string-to-number (match-string 3 skey))
	    key 'day))
     ((string-match "\\([-+][0-9]+\\)\\'" skey)
      (setq shift (string-to-number (match-string 1 skey))
	    key (intern (substring skey 0 (match-beginning 1))))
      (when (and (memq key '(quarter thisq)) (> shift 0))
	(error "Looking forward with quarters isn't implemented"))))
    (when (= shift 0)
      (pcase key
	(`yesterday (setq key 'today   shift -1))
	(`lastweek  (setq key 'week    shift -1))
	(`lastmonth (setq key 'month   shift -1))
	(`lastyear  (setq key 'year    shift -1))
	(`lastq     (setq key 'quarter shift -1))))
    ;; Prepare start and end times depending on KEY's type.
    (pcase key
      ((or `day `today) (setq m 0 h 0 h1 24 d (+ d shift)))
      ((or `week `thisweek)
       (let* ((ws (or wstart 1))
	      (diff (+ (* -7 shift) (if (= dow 0) (- 7 ws) (- dow ws)))))
	 (setq m 0 h 0 d (- d diff) d1 (+ 7 d))))
      ((or `month `thismonth)
       (setq h 0 m 0 d (or mstart 1) month (+ month shift) month1 (1+ month)))
      ((or `quarter `thisq)
       ;; Compute if this shift remains in this year.  If not, compute
       ;; how many years and quarters we have to shift (via floor*) and
       ;; compute the shifted years, months and quarters.
       (cond
	((< (+ (- q 1) shift) 0)	; Shift not in this year.
	 (let* ((interval (* -1 (+ (- q 1) shift)))
		;; Set tmp to ((years to shift) (quarters to shift)).
		(tmp (cl-floor interval 4)))
	   ;; Due to the use of floor, 0 quarters actually means 4.
	   (if (= 0 (nth 1 tmp))
	       (setq shiftedy (- y (nth 0 tmp))
		     shiftedm 1
		     shiftedq 1)
	     (setq shiftedy (- y (+ 1 (nth 0 tmp)))
		   shiftedm (- 13 (* 3 (nth 1 tmp)))
		   shiftedq (- 5 (nth 1 tmp)))))
	 (setq m 0 h 0 d 1 month shiftedm month1 (+ 3 shiftedm) y shiftedy))
	((> (+ q shift) 0)		; Shift is within this year.
	 (setq shiftedq (+ q shift))
	 (setq shiftedy y)
	 (let ((qshift (* 3 (1- (+ q shift)))))
	   (setq m 0 h 0 d 1 month (+ 1 qshift) month1 (+ 4 qshift))))))
      ((or `year `thisyear)
       (setq m 0 h 0 d 1 month 1 y (+ y shift) y1 (1+ y)))
      ((or `interactive `untilnow))	; Special cases, ignore them.
      (_ (user-error "No such time block %s" key)))
    ;; Format start and end times according to AS-STRINGS.
    (let* ((start (pcase key
		    (`interactive (org-read-date nil t nil "Range start? "))
		    (`untilnow org-clock--oldest-date)
		    (_ (encode-time 0 m h d month y))))
	   (end (pcase key
		  (`interactive (org-read-date nil t nil "Range end? "))
		  (`untilnow (current-time))
		  (_ (encode-time 0
				  (or m1 m)
				  (or h1 h)
				  (or d1 d)
				  (or month1 month)
				  (or y1 y)))))
	   (text
	    (pcase key
	      ((or `day `today) (format-time-string "%A, %B %d, %Y" start))
	      ((or `week `thisweek) (format-time-string "week %G-W%V" start))
	      ((or `month `thismonth) (format-time-string "%B %Y" start))
	      ((or `year `thisyear) (format-time-string "the year %Y" start))
	      ((or `quarter `thisq)
	       (concat (org-count-quarter shiftedq)
		       " quarter of " (number-to-string shiftedy)))
	      (`interactive "(Range interactively set)")
	      (`untilnow "now"))))
      (if (not as-strings) (list start end text)
	(let ((f (cdr org-time-stamp-formats)))
	  (list (format-time-string f start)
		(format-time-string f end)
		text))))))

(defun org-count-quarter (n)
  (cond
   ((= n 1) "1st")
   ((= n 2) "2nd")
   ((= n 3) "3rd")
   ((= n 4) "4th")))

;;;###autoload
(defun org-clocktable-shift (dir n)
  "Try to shift the :block date of the clocktable at point.
Point must be in the #+BEGIN: line of a clocktable, or this function
will throw an error.
DIR is a direction, a symbol `left', `right', `up', or `down'.
Both `left' and `down' shift the block toward the past, `up' and `right'
push it toward the future.
N is the number of shift steps to take.  The size of the step depends on
the currently selected interval size."
  (setq n (prefix-numeric-value n))
  (and (memq dir '(left down)) (setq n (- n)))
  (save-excursion
    (goto-char (point-at-bol))
    (if (not (looking-at "^[ \t]*#\\+BEGIN:[ \t]+clocktable\\>.*?:block[ \t]+\\(\\S-+\\)"))
	(error "Line needs a :block definition before this command works")
      (let* ((b (match-beginning 1)) (e (match-end 1))
	     (s (match-string 1))
	     block shift ins y mw d date wp m)
	(cond
	 ((equal s "yesterday") (setq s "today-1"))
	 ((equal s "lastweek") (setq s "thisweek-1"))
	 ((equal s "lastmonth") (setq s "thismonth-1"))
	 ((equal s "lastyear") (setq s "thisyear-1"))
	 ((equal s "lastq") (setq s "thisq-1")))

	(cond
	 ((string-match "^\\(today\\|thisweek\\|thismonth\\|thisyear\\|thisq\\)\\([-+][0-9]+\\)?$" s)
	  (setq block (match-string 1 s)
		shift (if (match-end 2)
			  (string-to-number (match-string 2 s))
			0))
	  (setq shift (+ shift n))
	  (setq ins (if (= shift 0) block (format "%s%+d" block shift))))
	 ((string-match "\\([0-9]+\\)\\(-\\([wWqQ]?\\)\\([0-9]\\{1,2\\}\\)\\(-\\([0-9]\\{1,2\\}\\)\\)?\\)?" s)
	  ;;               1        1  2   3       3  4                  4  5   6                6  5   2
	  (setq y (string-to-number (match-string 1 s))
		wp (and (match-end 3) (match-string 3 s))
		mw (and (match-end 4) (string-to-number (match-string 4 s)))
		d (and (match-end 6) (string-to-number (match-string 6 s))))
	  (cond
	   (d (setq ins (format-time-string
			 "%Y-%m-%d"
			 (encode-time 0 0 0 (+ d n) m y))))
	   ((and wp (string-match "w\\|W" wp) mw (> (length wp) 0))
	    (require 'cal-iso)
	    (setq date (calendar-gregorian-from-absolute
			(calendar-iso-to-absolute (list (+ mw n) 1 y))))
	    (setq ins (format-time-string
		       "%G-W%V"
		       (encode-time 0 0 0 (nth 1 date) (car date) (nth 2 date)))))
	   ((and wp (string-match "q\\|Q" wp) mw (> (length wp) 0))
	    (require 'cal-iso)
					; if the 4th + 1 quarter is requested we flip to the 1st quarter of the next year
	    (if (> (+ mw n) 4)
		(setq mw 0
		      y (+ 1 y))
	      ())
					; if the 1st - 1 quarter is requested we flip to the 4th quarter of the previous year
	    (if (= (+ mw n) 0)
		(setq mw 5
		      y (- y 1))
	      ())
	    (setq date (calendar-gregorian-from-absolute
			(calendar-iso-to-absolute (org-quarter-to-date (+ mw n) y))))
	    (setq ins (format-time-string
		       (concat (number-to-string y) "-Q" (number-to-string (+ mw n)))
		       (encode-time 0 0 0 (nth 1 date) (car date) (nth 2 date)))))
	   (mw
	    (setq ins (format-time-string
		       "%Y-%m"
		       (encode-time 0 0 0 1 (+ mw n) y))))
	   (y
	    (setq ins (number-to-string (+ y n))))))
	 (t (error "Cannot shift clocktable block")))
	(when ins
	  (goto-char b)
	  (insert ins)
	  (delete-region (point) (+ (point) (- e b)))
	  (beginning-of-line 1)
	  (org-update-dblock)
	  t)))))

;;;###autoload
(defun org-dblock-write:clocktable (params)
  "Write the standard clocktable."
  (setq params (org-combine-plists org-clocktable-defaults params))
  (catch 'exit
    (let* ((scope (plist-get params :scope))
	   (files (pcase scope
		    (`agenda
		     (org-agenda-files t))
		    (`agenda-with-archives
		     (org-add-archive-files (org-agenda-files t)))
		    (`file-with-archives
		     (and buffer-file-name
			  (org-add-archive-files (list buffer-file-name))))
		    ((pred functionp) (funcall scope))
		    ((pred consp) scope)
		    (_ (or (buffer-file-name) (current-buffer)))))
	   (block (plist-get params :block))
	   (ts (plist-get params :tstart))
	   (te (plist-get params :tend))
	   (ws (plist-get params :wstart))
	   (ms (plist-get params :mstart))
	   (step (plist-get params :step))
	   (formatter (or (plist-get params :formatter)
			  org-clock-clocktable-formatter
			  'org-clocktable-write-default))
	   cc)
      ;; Check if we need to do steps
      (when block
	;; Get the range text for the header
	(setq cc (org-clock-special-range block nil t ws ms)
	      ts (car cc)
	      te (nth 1 cc)))
      (when step
	;; Write many tables, in steps
	(unless (or block (and ts te))
	  (error "Clocktable `:step' can only be used with `:block' or `:tstart,:end'"))
	(org-clocktable-steps params)
	(throw 'exit nil))

      (org-agenda-prepare-buffers (if (consp files) files (list files)))

      (let ((origin (point))
	    (tables
	     (if (consp files)
		 (mapcar (lambda (file)
			   (with-current-buffer (find-buffer-visiting file)
			     (save-excursion
			       (save-restriction
				 (org-clock-get-table-data file params)))))
			 files)
	       ;; Get the right restriction for the scope.
	       (save-restriction
		 (cond
		  ((not scope))	     ;use the restriction as it is now
		  ((eq scope 'file) (widen))
		  ((eq scope 'subtree) (org-narrow-to-subtree))
		  ((eq scope 'tree)
		   (while (org-up-heading-safe))
		   (org-narrow-to-subtree))
		  ((and (symbolp scope)
			(string-match "\\`tree\\([0-9]+\\)\\'"
				      (symbol-name scope)))
		   (let ((level (string-to-number
				 (match-string 1 (symbol-name scope)))))
		     (catch 'exit
		       (while (org-up-heading-safe)
			 (looking-at org-outline-regexp)
			 (when (<= (org-reduced-level (funcall outline-level))
				   level)
			   (throw 'exit nil))))
		     (org-narrow-to-subtree))))
		 (list (org-clock-get-table-data nil params)))))
	    (multifile
	     ;; Even though `file-with-archives' can consist of
	     ;; multiple files, we consider this is one extended file
	     ;; instead.
	     (and (consp files) (not (eq scope 'file-with-archives)))))

	(funcall formatter
		 origin
		 tables
		 (org-combine-plists params `(:multifile ,multifile)))))))

(defun org-clocktable-write-default (ipos tables params)
  "Write out a clock table at position IPOS in the current buffer.
TABLES is a list of tables with clocking data as produced by
`org-clock-get-table-data'.  PARAMS is the parameter property list obtained
from the dynamic block definition."
  ;; This function looks quite complicated, mainly because there are a
  ;; lot of options which can add or remove columns.  I have massively
  ;; commented this function, the I hope it is understandable.  If
  ;; someone wants to write their own special formatter, this maybe
  ;; much easier because there can be a fixed format with a
  ;; well-defined number of columns...
  (let* ((lang (or (plist-get params :lang) "en"))
	 (multifile (plist-get params :multifile))
	 (block (plist-get params :block))
	 (sort (plist-get params :sort))
	 (header (plist-get params :header))
	 (link (plist-get params :link))
	 (maxlevel (or (plist-get params :maxlevel) 3))
	 (emph (plist-get params :emphasize))
	 (compact? (plist-get params :compact))
	 (narrow (or (plist-get params :narrow) (and compact? '40!)))
	 (level? (and (not compact?) (plist-get params :level)))
	 (timestamp (plist-get params :timestamp))
	 (properties (plist-get params :properties))
	 (time-columns
	  (if (or compact? (< maxlevel 2)) 1
	    ;; Deepest headline level is a hard limit for the number
	    ;; of time columns.
	    (let ((levels
		   (cl-mapcan
		    (lambda (table)
		      (pcase table
			(`(,_ ,(and (pred wholenump) (pred (/= 0))) ,entries)
			 (mapcar #'car entries))))
		    tables)))
	      (min maxlevel
		   (or (plist-get params :tcolumns) 100)
		   (if (null levels) 1 (apply #'max levels))))))
	 (indent (or compact? (plist-get params :indent)))
	 (formula (plist-get params :formula))
	 (case-fold-search t)
	 (total-time (apply #'+ (mapcar #'cadr tables)))
	 recalc narrow-cut-p)

    (when (and narrow (integerp narrow) link)
      ;; We cannot have both integer narrow and link.
      (message "Using hard narrowing in clocktable to allow for links")
      (setq narrow (intern (format "%d!" narrow))))

    (pcase narrow
      ((or `nil (pred integerp)) nil)	;nothing to do
      ((and (pred symbolp)
	    (guard (string-match-p "\\`[0-9]+!\\'" (symbol-name narrow))))
       (setq narrow-cut-p t)
       (setq narrow (string-to-number (symbol-name narrow))))
      (_ (error "Invalid value %s of :narrow property in clock table" narrow)))

    ;; Now we need to output this table stuff.
    (goto-char ipos)

    ;; Insert the text *before* the actual table.
    (insert-before-markers
     (or header
	 ;; Format the standard header.
	 (format "#+CAPTION: %s %s%s\n"
		 (org-clock--translate "Clock summary at" lang)
		 (format-time-string (org-time-stamp-format t t))
		 (if block
		     (let ((range-text
			    (nth 2 (org-clock-special-range
				    block nil t
				    (plist-get params :wstart)
				    (plist-get params :mstart)))))
		       (format ", for %s." range-text))
		   ""))))

    ;; Insert the narrowing line
    (when (and narrow (integerp narrow) (not narrow-cut-p))
      (insert-before-markers
       "|"				;table line starter
       (if multifile "|" "")		;file column, maybe
       (if level? "|" "")		;level column, maybe
       (if timestamp "|" "")		;timestamp column, maybe
       (if properties			;properties columns, maybe
	   (make-string (length properties) ?|)
	 "")
       (format "<%d>| |\n" narrow)))	;headline and time columns

    ;; Insert the table header line
    (insert-before-markers
     "|"				;table line starter
     (if multifile			;file column, maybe
	 (concat (org-clock--translate "File" lang) "|")
       "")
     (if level?				;level column, maybe
	 (concat (org-clock--translate "L" lang) "|")
       "")
     (if timestamp			;timestamp column, maybe
	 (concat (org-clock--translate "Timestamp" lang) "|")
       "")
     (if properties			;properties columns, maybe
	 (concat (mapconcat #'identity properties "|") "|")
       "")
     (concat (org-clock--translate "Headline" lang)"|")
     (concat (org-clock--translate "Time" lang) "|")
     (make-string (max 0 (1- time-columns)) ?|) ;other time columns
     (if (eq formula '%) "%|\n" "\n"))

    ;; Insert the total time in the table
    (insert-before-markers
     "|-\n"				;a hline
     "|"				;table line starter
     (if multifile (format "| %s " (org-clock--translate "ALL" lang)) "")
					;file column, maybe
     (if level? "|" "")			;level column, maybe
     (if timestamp "|" "")		;timestamp column, maybe
     (make-string (length properties) ?|) ;properties columns, maybe
     (concat (format org-clock-total-time-cell-format
		     (org-clock--translate "Total time" lang))
	     "| ")
     (format org-clock-total-time-cell-format
	     (org-duration-from-minutes (or total-time 0))) ;time
     "|"
     (make-string (max 0 (1- time-columns)) ?|)
     (cond ((not (eq formula '%)) "")
	   ((or (not total-time) (= total-time 0)) "0.0|")
	   (t  "100.0|"))
     "\n")

    ;; Now iterate over the tables and insert the data but only if any
    ;; time has been collected.
    (when (and total-time (> total-time 0))
      (pcase-dolist (`(,file-name ,file-time ,entries) tables)
	(when (or (and file-time (> file-time 0))
		  (not (plist-get params :fileskip0)))
	  (insert-before-markers "|-\n") ;hline at new file
	  ;; First the file time, if we have multiple files.
	  (when multifile
	    ;; Summarize the time collected from this file.
	    (insert-before-markers
	     (format (concat "| %s %s | %s%s"
			     (format org-clock-file-time-cell-format
				     (org-clock--translate "File time" lang))
			     " | *%s*|\n")
		     (file-name-nondirectory file-name)
		     (if level?   "| " "")  ;level column, maybe
		     (if timestamp "| " "") ;timestamp column, maybe
		     (if properties	    ;properties columns, maybe
			 (make-string (length properties) ?|)
		       "")
		     (org-duration-from-minutes file-time)))) ;time

	  ;; Get the list of node entries and iterate over it
	  (when (> maxlevel 0)
	    (pcase-dolist (`(,level ,headline ,ts ,time ,props) entries)
	      (when narrow-cut-p
		(setq headline
		      (if (and (string-match
				(format "\\`%s\\'" org-bracket-link-regexp)
				headline)
			       (match-end 3))
			  (format "[[%s][%s]]"
				  (match-string 1 headline)
				  (org-shorten-string (match-string 3 headline)
						      narrow))
			(org-shorten-string headline narrow))))
	      (cl-flet ((format-field (f) (format (cond ((not emph) "%s |")
							((= level 1) "*%s* |")
							((= level 2) "/%s/ |")
							(t "%s |"))
						  f)))
		(insert-before-markers
		 "|"		       ;start the table line
		 (if multifile "|" "") ;free space for file name column?
		 (if level? (format "%d|" level) "") ;level, maybe
		 (if timestamp (concat ts "|") "")   ;timestamp, maybe
		 (if properties		;properties columns, maybe
		     (concat (mapconcat (lambda (p) (or (cdr (assoc p props)) ""))
					properties
					"|")
			     "|")
		   "")
		 (if indent		;indentation
		     (org-clocktable-indent-string level)
		   "")
		 (format-field headline)
		 ;; Empty fields for higher levels.
		 (make-string (max 0 (1- (min time-columns level))) ?|)
		 (format-field (org-duration-from-minutes time))
		 (make-string (max 0 (- time-columns level)) ?|)
		 (if (eq formula '%)
		     (format "%.1f |" (* 100 (/ time (float total-time))))
		   "")
		 "\n")))))))
    (delete-char -1)
    (cond
     ;; Possibly rescue old formula?
     ((or (not formula) (eq formula '%))
      (let ((contents (org-string-nw-p (plist-get params :content))))
	(when (and contents (string-match "^\\([ \t]*#\\+tblfm:.*\\)" contents))
	  (setq recalc t)
	  (insert "\n" (match-string 1 contents))
	  (beginning-of-line 0))))
     ;; Insert specified formula line.
     ((stringp formula)
      (insert "\n#+TBLFM: " formula)
      (setq recalc t))
     (t
      (user-error "Invalid :formula parameter in clocktable")))
    ;; Back to beginning, align the table, recalculate if necessary.
    (goto-char ipos)
    (skip-chars-forward "^|")
    (org-table-align)
    (when org-hide-emphasis-markers
      ;; We need to align a second time.
      (org-table-align))
    (when sort
      (save-excursion
	(org-table-goto-line 3)
	(org-table-goto-column (car sort))
	(org-table-sort-lines nil (cdr sort))))
    (when recalc (org-table-recalculate 'all))
    total-time))

(defun org-clocktable-indent-string (level)
  "Return indentation string according to LEVEL.
LEVEL is an integer.  Indent by two spaces per level above 1."
  (if (= level 1) ""
    (concat "\\_" (make-string (* 2 (1- level)) ?\s))))

(defun org-clocktable-steps (params)
  "Step through the range to make a number of clock tables."
  (let* ((p1 (copy-sequence params))
	 (ts (plist-get p1 :tstart))
	 (te (plist-get p1 :tend))
	 (ws (plist-get p1 :wstart))
	 (ms (plist-get p1 :mstart))
	 (step0 (plist-get p1 :step))
	 (step (cdr (assoc step0 '((day . 86400) (week . 604800)))))
	 (stepskip0 (plist-get p1 :stepskip0))
	 (block (plist-get p1 :block))
	 cc step-time tsb)
    (when block
      (setq cc (org-clock-special-range block nil t ws ms)
	    ts (car cc)
	    te (nth 1 cc)))
    (cond
     ((numberp ts)
      ;; If ts is a number, it's an absolute day number from
      ;; org-agenda.
      (pcase-let ((`(,month ,day ,year) (calendar-gregorian-from-absolute ts)))
	(setq ts (float-time (encode-time 0 0 0 day month year)))))
     (ts
      (setq ts (float-time (apply #'encode-time (org-parse-time-string ts))))))
    (cond
     ((numberp te)
      ;; Likewise for te.
      (pcase-let ((`(,month ,day ,year) (calendar-gregorian-from-absolute te)))
	(setq te (float-time (encode-time 0 0 0 day month year)))))
     (te
      (setq te (float-time (apply #'encode-time (org-parse-time-string te))))))
    (setq tsb
	  (if (eq step0 'week)
	      (let ((dow (nth 6 (decode-time (seconds-to-time ts)))))
		(if (< dow ws) ts
		  (- ts (* 86400 (- dow ws)))))
	    ts))
    (setq p1 (plist-put p1 :header ""))
    (setq p1 (plist-put p1 :step nil))
    (setq p1 (plist-put p1 :block nil))
    (while (< tsb te)
      (or (bolp) (insert "\n"))
      (setq p1 (plist-put p1 :tstart (format-time-string
				      (org-time-stamp-format nil t)
				      (seconds-to-time (max tsb ts)))))
      (cl-incf tsb (let ((dow (nth 6 (decode-time (seconds-to-time tsb)))))
		     (if (or (eq step0 'day)
			     (= dow ws))
			 step
		       (* 86400 (- ws dow)))))
      (setq p1 (plist-put p1 :tend (format-time-string
				    (org-time-stamp-format nil t)
				    (seconds-to-time (min te tsb)))))
      (insert "\n" (if (eq step0 'day) "Daily report: "
		     "Weekly report starting on: ")
	      (plist-get p1 :tstart) "\n")
      (setq step-time (org-dblock-write:clocktable p1))
      (re-search-forward "^[ \t]*#\\+END:")
      (when (and (equal step-time 0) stepskip0)
	;; Remove the empty table
	(delete-region (point-at-bol)
		       (save-excursion
			 (re-search-backward "^\\(Daily\\|Weekly\\) report"
					     nil t)
			 (point))))
      (end-of-line 0))))

(defun org-clock-get-table-data (file params)
  "Get the clocktable data for file FILE, with parameters PARAMS.
FILE is only for identification - this function assumes that
the correct buffer is current, and that the wanted restriction is
in place.
The return value will be a list with the file name and the total
file time (in minutes) as 1st and 2nd elements.  The third element
of this list will be a list of headline entries.  Each entry has the
following structure:

  (LEVEL HEADLINE TIMESTAMP TIME PROPERTIES)

LEVEL:      The level of the headline, as an integer.  This will be
            the reduced level, so 1,2,3,... even if only odd levels
            are being used.
HEADLINE:   The text of the headline.  Depending on PARAMS, this may
            already be formatted like a link.
TIMESTAMP:  If PARAMS require it, this will be a time stamp found in the
            entry, any of SCHEDULED, DEADLINE, NORMAL, or first inactive,
            in this sequence.
TIME:       The sum of all time spend in this tree, in minutes.  This time
            will of cause be restricted to the time block and tags match
            specified in PARAMS.
PROPERTIES: The list properties specified in the `:properties' parameter
            along with their value, as an alist following the pattern
            (NAME . VALUE)."
  (let* ((maxlevel (or (plist-get params :maxlevel) 3))
	 (timestamp (plist-get params :timestamp))
	 (ts (plist-get params :tstart))
	 (te (plist-get params :tend))
	 (ws (plist-get params :wstart))
	 (ms (plist-get params :mstart))
	 (block (plist-get params :block))
	 (link (plist-get params :link))
	 (tags (plist-get params :tags))
	 (properties (plist-get params :properties))
	 (inherit-property-p (plist-get params :inherit-props))
	 (matcher (and tags (cdr (org-make-tags-matcher tags))))
	 cc st p tbl)

    (setq org-clock-file-total-minutes nil)
    (when block
      (setq cc (org-clock-special-range block nil t ws ms)
	    ts (car cc)
	    te (nth 1 cc)))
    (when (integerp ts) (setq ts (calendar-gregorian-from-absolute ts)))
    (when (integerp te) (setq te (calendar-gregorian-from-absolute te)))
    (when (and ts (listp ts))
      (setq ts (format "%4d-%02d-%02d" (nth 2 ts) (car ts) (nth 1 ts))))
    (when (and te (listp te))
      (setq te (format "%4d-%02d-%02d" (nth 2 te) (car te) (nth 1 te))))
    ;; Now the times are strings we can parse.
    (if ts (setq ts (org-matcher-time ts)))
    (if te (setq te (org-matcher-time te)))
    (save-excursion
      (org-clock-sum ts te
		     (when matcher
		       `(lambda ()
			  (let* ((tags-list (org-get-tags-at))
				 (org-scanner-tags tags-list)
				 (org-trust-scanner-tags t))
			    (funcall ,matcher nil tags-list nil)))))
      (goto-char (point-min))
      (setq st t)
      (while (or (and (bobp) (prog1 st (setq st nil))
		      (get-text-property (point) :org-clock-minutes)
		      (setq p (point-min)))
		 (setq p (next-single-property-change
			  (point) :org-clock-minutes)))
	(goto-char p)
	(let ((time (get-text-property p :org-clock-minutes)))
	  (when (and time (> time 0) (org-at-heading-p))
	    (let ((level (org-reduced-level (org-current-level))))
	      (when (<= level maxlevel)
		(let* ((headline (org-get-heading t t t t))
		       (hdl
			(if (not link) headline
			  (let ((search
				 (org-make-org-heading-search-string headline)))
			    (org-make-link-string
			     (if (not (buffer-file-name)) search
			       (format "file:%s::%s" (buffer-file-name) search))
			     ;; Prune statistics cookies.  Replace
			     ;; links with their description, or
			     ;; a plain link if there is none.
			     (org-trim
			      (org-link-display-format
			       (replace-regexp-in-string
				"\\[[0-9]+%\\]\\|\\[[0-9]+/[0-9]+\\]" ""
				headline)))))))
		       (tsp
			(and timestamp
			     (cl-some (lambda (p) (org-entry-get (point) p))
				      '("SCHEDULED" "DEADLINE" "TIMESTAMP"
					"TIMESTAMP_IA"))))
		       (props
			(and properties
			     (delq nil
				   (mapcar
				    (lambda (p)
				      (let ((v (org-entry-get
						(point) p inherit-property-p)))
					(and v (cons p v))))
				    properties)))))
		  (push (list level hdl tsp time props) tbl)))))))
      (list file org-clock-file-total-minutes (nreverse tbl)))))

;; Saving and loading the clock

(defvar org-clock-loaded nil
  "Was the clock file loaded?")

;;;###autoload
(defun org-clock-update-time-maybe ()
  "If this is a CLOCK line, update it and return t.
Otherwise, return nil."
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (skip-chars-forward " \t")
    (when (looking-at org-clock-string)
      (let ((re (concat "[ \t]*" org-clock-string
			" *[[<]\\([^]>]+\\)[]>]\\(-+[[<]\\([^]>]+\\)[]>]"
			"\\([ \t]*=>.*\\)?\\)?"))
	    ts te h m s neg)
	(cond
	 ((not (looking-at re))
	  nil)
	 ((not (match-end 2))
	  (when (and (equal (marker-buffer org-clock-marker) (current-buffer))
		     (> org-clock-marker (point))
		     (<= org-clock-marker (point-at-eol)))
	    ;; The clock is running here
	    (setq org-clock-start-time
		  (apply 'encode-time
			 (org-parse-time-string (match-string 1))))
	    (org-clock-update-mode-line)))
	 (t
	  (and (match-end 4) (delete-region (match-beginning 4) (match-end 4)))
	  (end-of-line 1)
	  (setq ts (match-string 1)
		te (match-string 3))
	  (setq s (- (float-time
		      (apply #'encode-time (org-parse-time-string te nil t)))
		     (float-time
		      (apply #'encode-time (org-parse-time-string ts nil t))))
		neg (< s 0)
		s (abs s)
		h (floor (/ s 3600))
		s (- s (* 3600 h))
		m (floor (/ s 60))
		s (- s (* 60 s)))
	  (insert " => " (format (if neg "-%d:%02d" "%2d:%02d") h m))
	  t))))))

(defun org-clock-save ()
  "Persist various clock-related data to disk.
The details of what will be saved are regulated by the variable
`org-clock-persist'."
  (when (and org-clock-persist
             (or org-clock-loaded
		 org-clock-has-been-used
		 (not (file-exists-p org-clock-persist-file))))
    (with-temp-file org-clock-persist-file
      (insert (format ";; %s - %s at %s\n"
		      (file-name-nondirectory org-clock-persist-file)
		      (system-name)
		      (format-time-string (org-time-stamp-format t))))
      ;; Store clock to be resumed.
      (when (and (memq org-clock-persist '(t clock))
		 (let ((b (org-base-buffer (org-clocking-buffer))))
		   (and (buffer-live-p b)
			(buffer-file-name b)
			(or (not org-clock-persist-query-save)
			    (y-or-n-p (format "Save current clock (%s) "
					      org-clock-heading))))))
	(insert
	 (format "(setq org-clock-stored-resume-clock '(%S . %d))\n"
		 (buffer-file-name (org-base-buffer (org-clocking-buffer)))
		 (marker-position org-clock-marker))))
      ;; Store clocked task history.  Tasks are stored reversed to
      ;; make reading simpler.
      (when (and (memq org-clock-persist '(t history))
		 org-clock-history)
	(insert
	 (format "(setq org-clock-stored-history '(%s))\n"
		 (mapconcat
		  (lambda (m)
		    (let ((b (org-base-buffer (marker-buffer m))))
		      (when (and (buffer-live-p b)
				 (buffer-file-name b))
			(format "(%S . %d)"
				(buffer-file-name b)
				(marker-position m)))))
		  (reverse org-clock-history)
		  " ")))))))

(defun org-clock-load ()
  "Load clock-related data from disk, maybe resuming a stored clock."
  (when (and org-clock-persist (not org-clock-loaded))
    (if (not (file-readable-p org-clock-persist-file))
	(message "Not restoring clock data; %S not found" org-clock-persist-file)
      (message "Restoring clock data")
      ;; Load history.
      (load-file org-clock-persist-file)
      (setq org-clock-loaded t)
      (pcase-dolist (`(,(and file (pred file-exists-p)) . ,position)
		     org-clock-stored-history)
	(org-clock-history-push position (find-file-noselect file)))
      ;; Resume clock.
      (pcase org-clock-stored-resume-clock
	(`(,(and file (pred file-exists-p)) . ,position)
	 (with-current-buffer (find-file-noselect file)
	   (when (or (not org-clock-persist-query-resume)
		     (y-or-n-p (format "Resume clock (%s) "
				       (save-excursion
					 (goto-char position)
					 (org-get-heading t t)))))
	     (goto-char position)
	     (let ((org-clock-in-resume 'auto-restart)
		   (org-clock-auto-clock-resolution nil))
	       (org-clock-in)
	       (when (org-invisible-p) (org-show-context))))))
	(_ nil)))))

;; Suggested bindings
(org-defkey org-mode-map "\C-c\C-x\C-e" 'org-clock-modify-effort-estimate)

(provide 'org-clock)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; coding: utf-8
;; End:

;;; org-clock.el ends here
