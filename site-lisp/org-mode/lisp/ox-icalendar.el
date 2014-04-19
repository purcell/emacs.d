;;; ox-icalendar.el --- iCalendar Back-End for Org Export Engine

;; Copyright (C) 2004-2014 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;;      Nicolas Goaziou <n dot goaziou at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp
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
;;
;; This library implements an iCalendar back-end for Org generic
;; exporter.  See Org manual for more information.
;;
;; It is expected to conform to RFC 5545.

;;; Code:

(eval-when-compile (require 'cl))
(require 'ox-ascii)
(declare-function org-bbdb-anniv-export-ical "org-bbdb" nil)



;;; User-Configurable Variables

(defgroup org-export-icalendar nil
  "Options specific for iCalendar export back-end."
  :tag "Org Export iCalendar"
  :group 'org-export)

(defcustom org-icalendar-combined-agenda-file "~/org.ics"
  "The file name for the iCalendar file covering all agenda files.
This file is created with the command \\[org-icalendar-combine-agenda-files].
The file name should be absolute.  It will be overwritten without warning."
  :group 'org-export-icalendar
  :type 'file)

(defcustom org-icalendar-alarm-time 0
  "Number of minutes for triggering an alarm for exported timed events.

A zero value (the default) turns off the definition of an alarm trigger
for timed events.  If non-zero, alarms are created.

- a single alarm per entry is defined
- The alarm will go off N minutes before the event
- only a DISPLAY action is defined."
  :group 'org-export-icalendar
  :version "24.1"
  :type 'integer)

(defcustom org-icalendar-combined-name "OrgMode"
  "Calendar name for the combined iCalendar representing all agenda files."
  :group 'org-export-icalendar
  :type 'string)

(defcustom org-icalendar-combined-description ""
  "Calendar description for the combined iCalendar (all agenda files)."
  :group 'org-export-icalendar
  :type 'string)

(defcustom org-icalendar-exclude-tags nil
  "Tags that exclude a tree from export.
This variable allows to specify different exclude tags from other
back-ends.  It can also be set with the ICAL_EXCLUDE_TAGS
keyword."
  :group 'org-export-icalendar
  :type '(repeat (string :tag "Tag")))

(defcustom org-icalendar-use-deadline '(event-if-not-todo todo-due)
  "Contexts where iCalendar export should use a deadline time stamp.

This is a list with several symbols in it.  Valid symbol are:
`event-if-todo'       Deadlines in TODO entries become calendar events.
`event-if-not-todo'   Deadlines in non-TODO entries become calendar events.
`todo-due'            Use deadlines in TODO entries as due-dates"
  :group 'org-export-icalendar
  :type '(set :greedy t
	      (const :tag "Deadlines in non-TODO entries become events"
		     event-if-not-todo)
	      (const :tag "Deadline in TODO entries become events"
		     event-if-todo)
	      (const :tag "Deadlines in TODO entries become due-dates"
		     todo-due)))

(defcustom org-icalendar-use-scheduled '(todo-start)
  "Contexts where iCalendar export should use a scheduling time stamp.

This is a list with several symbols in it.  Valid symbol are:
`event-if-todo'       Scheduling time stamps in TODO entries become an event.
`event-if-not-todo'   Scheduling time stamps in non-TODO entries become an event.
`todo-start'          Scheduling time stamps in TODO entries become start date.
                      Some calendar applications show TODO entries only after
                      that date."
  :group 'org-export-icalendar
  :type '(set :greedy t
	      (const :tag
		     "SCHEDULED timestamps in non-TODO entries become events"
		     event-if-not-todo)
	      (const :tag "SCHEDULED timestamps in TODO entries become events"
		     event-if-todo)
	      (const :tag "SCHEDULED in TODO entries become start date"
		     todo-start)))

(defcustom org-icalendar-categories '(local-tags category)
  "Items that should be entered into the \"categories\" field.

This is a list of symbols, the following are valid:
`category'    The Org mode category of the current file or tree
`todo-state'  The todo state, if any
`local-tags'  The tags, defined in the current line
`all-tags'    All tags, including inherited ones."
  :group 'org-export-icalendar
  :type '(repeat
	  (choice
	   (const :tag "The file or tree category" category)
	   (const :tag "The TODO state" todo-state)
	   (const :tag "Tags defined in current line" local-tags)
	   (const :tag "All tags, including inherited ones" all-tags))))

(defcustom org-icalendar-with-timestamps 'active
  "Non-nil means make an event from plain time stamps.

It can be set to `active', `inactive', t or nil, in order to make
an event from, respectively, only active timestamps, only
inactive ones, all of them or none.

This variable has precedence over `org-export-with-timestamps'.
It can also be set with the #+OPTIONS line, e.g. \"<:t\"."
  :group 'org-export-icalendar
  :type '(choice
	  (const :tag "All timestamps" t)
	  (const :tag "Only active timestamps" active)
	  (const :tag "Only inactive timestamps" inactive)
	  (const :tag "No timestamp" nil)))

(defcustom org-icalendar-include-todo nil
  "Non-nil means create VTODO components from TODO items.

Valid values are:
nil                  don't include any task.
t                    include tasks that are not in DONE state.
`unblocked'          include all TODO items that are not blocked.
`all'                include both done and not done items."
  :group 'org-export-icalendar
  :type '(choice
	  (const :tag "None" nil)
	  (const :tag "Unfinished" t)
	  (const :tag "Unblocked" unblocked)
	  (const :tag "All" all)
	  (repeat :tag "Specific TODO keywords"
		  (string :tag "Keyword"))))

(defcustom org-icalendar-include-bbdb-anniversaries nil
  "Non-nil means a combined iCalendar file should include anniversaries.
The anniversaries are defined in the BBDB database."
  :group 'org-export-icalendar
  :type 'boolean)

(defcustom org-icalendar-include-sexps t
  "Non-nil means export to iCalendar files should also cover sexp entries.
These are entries like in the diary, but directly in an Org mode
file."
  :group 'org-export-icalendar
  :type 'boolean)

(defcustom org-icalendar-include-body t
  "Amount of text below headline to be included in iCalendar export.
This is a number of characters that should maximally be included.
Properties, scheduling and clocking lines will always be removed.
The text will be inserted into the DESCRIPTION field."
  :group 'org-export-icalendar
  :type '(choice
	  (const :tag "Nothing" nil)
	  (const :tag "Everything" t)
	  (integer :tag "Max characters")))

(defcustom org-icalendar-store-UID nil
  "Non-nil means store any created UIDs in properties.

The iCalendar standard requires that all entries have a unique identifier.
Org will create these identifiers as needed.  When this variable is non-nil,
the created UIDs will be stored in the ID property of the entry.  Then the
next time this entry is exported, it will be exported with the same UID,
superseding the previous form of it.  This is essential for
synchronization services.

This variable is not turned on by default because we want to avoid creating
a property drawer in every entry if people are only playing with this feature,
or if they are only using it locally."
  :group 'org-export-icalendar
  :type 'boolean)

(defcustom org-icalendar-timezone (getenv "TZ")
  "The time zone string for iCalendar export.
When nil or the empty string, use output
from (current-time-zone)."
  :group 'org-export-icalendar
  :type '(choice
	  (const :tag "Unspecified" nil)
	  (string :tag "Time zone")))

(defcustom org-icalendar-date-time-format ":%Y%m%dT%H%M%S"
  "Format-string for exporting icalendar DATE-TIME.

See `format-time-string' for a full documentation.  The only
difference is that `org-icalendar-timezone' is used for %Z.

Interesting value are:
 - \":%Y%m%dT%H%M%S\" for local time
 - \";TZID=%Z:%Y%m%dT%H%M%S\" for local time with explicit timezone
 - \":%Y%m%dT%H%M%SZ\" for time expressed in Universal Time"
  :group 'org-export-icalendar
  :version "24.1"
  :type '(choice
	  (const :tag "Local time" ":%Y%m%dT%H%M%S")
	  (const :tag "Explicit local time" ";TZID=%Z:%Y%m%dT%H%M%S")
	  (const :tag "Universal time" ":%Y%m%dT%H%M%SZ")
	  (string :tag "Explicit format")))

(defvar org-icalendar-after-save-hook nil
  "Hook run after an iCalendar file has been saved.
This hook is run with the name of the file as argument.  A good
way to use this is to tell a desktop calendar application to
re-read the iCalendar file.")



;;; Define Back-End

(org-export-define-derived-backend 'icalendar 'ascii
  :translate-alist '((clock . ignore)
		     (footnote-definition . ignore)
		     (footnote-reference . ignore)
		     (headline . org-icalendar-entry)
		     (inlinetask . ignore)
		     (planning . ignore)
		     (section . ignore)
		     (inner-template . (lambda (c i) c))
		     (template . org-icalendar-template))
  :options-alist
  '((:exclude-tags
     "ICALENDAR_EXCLUDE_TAGS" nil org-icalendar-exclude-tags split)
    (:with-timestamps nil "<" org-icalendar-with-timestamps)
    (:with-vtodo nil nil org-icalendar-include-todo)
    ;; The following property will be non-nil when export has been
    ;; started from org-agenda-mode.  In this case, any entry without
    ;; a non-nil "ICALENDAR_MARK" property will be ignored.
    (:icalendar-agenda-view nil nil nil))
  :filters-alist
  '((:filter-headline . org-icalendar-clear-blank-lines))
  :menu-entry
  '(?c "Export to iCalendar"
       ((?f "Current file" org-icalendar-export-to-ics)
	(?a "All agenda files"
	    (lambda (a s v b) (org-icalendar-export-agenda-files a)))
	(?c "Combine all agenda files"
	    (lambda (a s v b) (org-icalendar-combine-agenda-files a))))))



;;; Internal Functions

(defun org-icalendar-create-uid (file &optional bell h-markers)
  "Set ID property on headlines missing it in FILE.
When optional argument BELL is non-nil, inform the user with
a message if the file was modified.  With optional argument
H-MARKERS non-nil, it is a list of markers for the headlines
which will be updated."
  (let ((pt (if h-markers (goto-char (car h-markers)) (point-min)))
	modified-flag)
    (org-map-entries
     (lambda ()
       (let ((entry (org-element-at-point)))
	 (unless (or (< (point) pt) (org-element-property :ID entry))
	   (org-id-get-create)
	   (setq modified-flag t)
	   (forward-line))
	 (when h-markers (setq org-map-continue-from (pop h-markers)))))
     nil nil 'comment)
    (when (and bell modified-flag)
      (message "ID properties created in file \"%s\"" file)
      (sit-for 2))))

(defun org-icalendar-blocked-headline-p (headline info)
  "Non-nil when HEADLINE is considered to be blocked.

INFO is a plist used as a communication channel.

a headline is blocked when either:

  - It has children which are not all in a completed state.

  - It has a parent with the property :ORDERED:, and there are
    siblings prior to it with incomplete status.

  - Its parent is blocked because it has siblings that should be
    done first or is a child of a blocked grandparent entry."
  (or
   ;; Check if any child is not done.
   (org-element-map headline 'headline
     (lambda (hl) (eq (org-element-property :todo-type hl) 'todo))
     info 'first-match)
   ;; Check :ORDERED: node property.
   (catch 'blockedp
     (let ((current headline))
       (mapc (lambda (parent)
	       (cond
		((not (org-element-property :todo-keyword parent))
		 (throw 'blockedp nil))
		((org-not-nil (org-element-property :ORDERED parent))
		 (let ((sibling current))
		   (while (setq sibling (org-export-get-previous-element
					 sibling info))
		     (when (eq (org-element-property :todo-type sibling) 'todo)
		       (throw 'blockedp t)))))
		(t (setq current parent))))
	     (org-export-get-genealogy headline))
       nil))))

(defun org-icalendar-use-UTC-date-time-p ()
  "Non-nil when `org-icalendar-date-time-format' requires UTC time."
  (char-equal (elt org-icalendar-date-time-format
		   (1- (length org-icalendar-date-time-format))) ?Z))

(defvar org-agenda-default-appointment-duration) ; From org-agenda.el.
(defun org-icalendar-convert-timestamp (timestamp keyword &optional end utc)
  "Convert TIMESTAMP to iCalendar format.

TIMESTAMP is a timestamp object.  KEYWORD is added in front of
it, in order to make a complete line (e.g. \"DTSTART\").

When optional argument END is non-nil, use end of time range.
Also increase the hour by two (if time string contains a time),
or the day by one (if it does not contain a time) when no
explicit ending time is specified.

When optional argument UTC is non-nil, time will be expressed in
Universal Time, ignoring `org-icalendar-date-time-format'."
  (let* ((year-start (org-element-property :year-start timestamp))
	 (year-end (org-element-property :year-end timestamp))
	 (month-start (org-element-property :month-start timestamp))
	 (month-end (org-element-property :month-end timestamp))
	 (day-start (org-element-property :day-start timestamp))
	 (day-end (org-element-property :day-end timestamp))
	 (hour-start (org-element-property :hour-start timestamp))
	 (hour-end (org-element-property :hour-end timestamp))
	 (minute-start (org-element-property :minute-start timestamp))
	 (minute-end (org-element-property :minute-end timestamp))
	 (with-time-p minute-start)
	 (equal-bounds-p
	  (equal (list year-start month-start day-start hour-start minute-start)
		 (list year-end month-end day-end hour-end minute-end)))
	 (mi (cond ((not with-time-p) 0)
		   ((not end) minute-start)
		   ((and org-agenda-default-appointment-duration equal-bounds-p)
		    (+ minute-end org-agenda-default-appointment-duration))
		   (t minute-end)))
	 (h (cond ((not with-time-p) 0)
		  ((not end) hour-start)
		  ((or (not equal-bounds-p)
		       org-agenda-default-appointment-duration)
		   hour-end)
		  (t (+ hour-end 2))))
	 (d (cond ((not end) day-start)
		  ((not with-time-p) (1+ day-end))
		  (t day-end)))
	 (m (if end month-end month-start))
	 (y (if end year-end year-start)))
    (concat
     keyword
     (format-time-string
      (cond (utc ":%Y%m%dT%H%M%SZ")
	    ((not with-time-p) ";VALUE=DATE:%Y%m%d")
	    (t (replace-regexp-in-string "%Z"
					 org-icalendar-timezone
					 org-icalendar-date-time-format
					 t)))
      ;; Convert timestamp into internal time in order to use
      ;; `format-time-string' and fix any mistake (i.e. MI >= 60).
      (encode-time 0 mi h d m y)
      (or utc (and with-time-p (org-icalendar-use-UTC-date-time-p)))))))

(defun org-icalendar-dtstamp ()
  "Return DTSTAMP property, as a string."
  (format-time-string "DTSTAMP:%Y%m%dT%H%M%SZ" nil t))

(defun org-icalendar-get-categories (entry info)
  "Return categories according to `org-icalendar-categories'.
ENTRY is a headline or an inlinetask element.  INFO is a plist
used as a communication channel."
  (mapconcat
   'identity
   (org-uniquify
    (let (categories)
      (mapc (lambda (type)
	      (case type
		(category
		 (push (org-export-get-category entry info) categories))
		(todo-state
		 (let ((todo (org-element-property :todo-keyword entry)))
		   (and todo (push todo categories))))
		(local-tags
		 (setq categories
		       (append (nreverse (org-export-get-tags entry info))
			       categories)))
		(all-tags
		 (setq categories
		       (append (nreverse (org-export-get-tags entry info nil t))
			       categories)))))
	    org-icalendar-categories)
      ;; Return list of categories, following specified order.
      (nreverse categories))) ","))

(defun org-icalendar-transcode-diary-sexp (sexp uid summary)
  "Transcode a diary sexp into iCalendar format.
SEXP is the diary sexp being transcoded, as a string.  UID is the
unique identifier for the entry.  SUMMARY defines a short summary
or subject for the event."
  (when (require 'icalendar nil t)
    (org-element-normalize-string
     (with-temp-buffer
       (let ((sexp (if (not (string-match "\\`<%%" sexp)) sexp
		     (concat (substring sexp 1 -1) " " summary))))
	 (put-text-property 0 1 'uid uid sexp)
	 (insert sexp "\n"))
       (org-diary-to-ical-string (current-buffer))))))

(defun org-icalendar-cleanup-string (s)
  "Cleanup string S according to RFC 5545."
  (when s
    ;; Protect "\", "," and ";" characters. and replace newline
    ;; characters with literal \n.
    (replace-regexp-in-string
     "[ \t]*\n" "\\n"
     (replace-regexp-in-string "[\\,;]" "\\\&" s)
     nil t)))

(defun org-icalendar-fold-string (s)
  "Fold string S according to RFC 5545."
  (org-element-normalize-string
   (mapconcat
    (lambda (line)
      ;; Limit each line to a maximum of 75 characters.  If it is
      ;; longer, fold it by using "\n " as a continuation marker.
      (let ((len (length line)))
	(if (<= len 75) line
	  (let ((folded-line (substring line 0 75))
		(chunk-start 75)
		chunk-end)
	    ;; Since continuation marker takes up one character on the
	    ;; line, real contents must be split at 74 chars.
	    (while (< (setq chunk-end (+ chunk-start 74)) len)
	      (setq folded-line
		    (concat folded-line "\n "
			    (substring line chunk-start chunk-end))
		    chunk-start chunk-end))
	    (concat folded-line "\n " (substring line chunk-start))))))
    (org-split-string s "\n") "\n")))



;;; Filters

(defun org-icalendar-clear-blank-lines (headline back-end info)
  "Remove trailing blank lines in HEADLINE export.
HEADLINE is a string representing a transcoded headline.
BACK-END and INFO are ignored."
  (replace-regexp-in-string "^\\(?:[ \t]*\n\\)*" "" headline))



;;; Transcode Functions

;;;; Headline and Inlinetasks

;; The main function is `org-icalendar-entry', which extracts
;; information from a headline or an inlinetask (summary,
;; description...) and then delegates code generation to
;; `org-icalendar--vtodo' and `org-icalendar--vevent', depending
;; on the component needed.

;; Obviously, `org-icalendar--valarm' handles alarms, which can
;; happen within a VTODO component.

(defun org-icalendar-entry (entry contents info)
  "Transcode ENTRY element into iCalendar format.

ENTRY is either a headline or an inlinetask.  CONTENTS is
ignored.  INFO is a plist used as a communication channel.

This function is called on every headline, the section below
it (minus inlinetasks) being its contents.  It tries to create
VEVENT and VTODO components out of scheduled date, deadline date,
plain timestamps, diary sexps.  It also calls itself on every
inlinetask within the section."
  (unless (org-element-property :footnote-section-p entry)
    (let* ((type (org-element-type entry))
	   ;; Determine contents really associated to the entry.  For
	   ;; a headline, limit them to section, if any.  For an
	   ;; inlinetask, this is every element within the task.
	   (inside
	    (if (eq type 'inlinetask)
		(cons 'org-data (cons nil (org-element-contents entry)))
	      (let ((first (car (org-element-contents entry))))
		(and (eq (org-element-type first) 'section)
		     (cons 'org-data
			   (cons nil (org-element-contents first))))))))
      (concat
       (unless (and (plist-get info :icalendar-agenda-view)
		    (not (org-element-property :ICALENDAR-MARK entry)))
	 (let ((todo-type (org-element-property :todo-type entry))
	       (uid (or (org-element-property :ID entry) (org-id-new)))
	       (summary (org-icalendar-cleanup-string
			 (or (org-element-property :SUMMARY entry)
			     (org-export-data
			      (org-element-property :title entry) info))))
	       (loc (org-icalendar-cleanup-string
		     (org-element-property :LOCATION entry)))
	       ;; Build description of the entry from associated
	       ;; section (headline) or contents (inlinetask).
	       (desc
		(org-icalendar-cleanup-string
		 (or (org-element-property :DESCRIPTION entry)
		     (let ((contents (org-export-data inside info)))
		       (cond
			((not (org-string-nw-p contents)) nil)
			((wholenump org-icalendar-include-body)
			 (let ((contents (org-trim contents)))
			   (substring
			    contents 0 (min (length contents)
					    org-icalendar-include-body))))
			(org-icalendar-include-body (org-trim contents)))))))
	       (cat (org-icalendar-get-categories entry info)))
	   (concat
	    ;; Events: Delegate to `org-icalendar--vevent' to
	    ;; generate "VEVENT" component from scheduled, deadline,
	    ;; or any timestamp in the entry.
	    (let ((deadline (org-element-property :deadline entry)))
	      (and deadline
		   (memq (if todo-type 'event-if-todo 'event-if-not-todo)
			 org-icalendar-use-deadline)
		   (org-icalendar--vevent
		    entry deadline (concat "DL-" uid)
		    (concat "DL: " summary) loc desc cat)))
	    (let ((scheduled (org-element-property :scheduled entry)))
	      (and scheduled
		   (memq (if todo-type 'event-if-todo 'event-if-not-todo)
			 org-icalendar-use-scheduled)
		   (org-icalendar--vevent
		    entry scheduled (concat "SC-" uid)
		    (concat "S: " summary) loc desc cat)))
	    ;; When collecting plain timestamps from a headline and
	    ;; its title, skip inlinetasks since collection will
	    ;; happen once ENTRY is one of them.
	    (let ((counter 0))
	      (mapconcat
	       'identity
	       (org-element-map (cons (org-element-property :title entry)
				      (org-element-contents inside))
		   'timestamp
		 (lambda (ts)
		   (let ((uid (format "TS%d-%s" (incf counter) uid)))
		     (org-icalendar--vevent entry ts uid summary loc desc cat)))
		 info nil (and (eq type 'headline) 'inlinetask))
	       ""))
	    ;; Task: First check if it is appropriate to export it.
	    ;; If so, call `org-icalendar--vtodo' to transcode it
	    ;; into a "VTODO" component.
	    (when (and todo-type
		       (case (plist-get info :with-vtodo)
			 (all t)
			 (unblocked
			  (and (eq type 'headline)
			       (not (org-icalendar-blocked-headline-p
				     entry info))))
			 ('t (eq todo-type 'todo))))
	      (org-icalendar--vtodo entry uid summary loc desc cat))
	    ;; Diary-sexp: Collect every diary-sexp element within
	    ;; ENTRY and its title, and transcode them.  If ENTRY is
	    ;; a headline, skip inlinetasks: they will be handled
	    ;; separately.
	    (when org-icalendar-include-sexps
	      (let ((counter 0))
		(mapconcat 'identity
			   (org-element-map
			       (cons (org-element-property :title entry)
				     (org-element-contents inside))
			       'diary-sexp
			     (lambda (sexp)
			       (org-icalendar-transcode-diary-sexp
				(org-element-property :value sexp)
				(format "DS%d-%s" (incf counter) uid)
				summary))
			     info nil (and (eq type 'headline) 'inlinetask))
			   ""))))))
       ;; If ENTRY is a headline, call current function on every
       ;; inlinetask within it.  In agenda export, this is independent
       ;; from the mark (or lack thereof) on the entry.
       (when (eq type 'headline)
	 (mapconcat 'identity
		    (org-element-map inside 'inlinetask
		      (lambda (task) (org-icalendar-entry task nil info))
		      info) ""))
       ;; Don't forget components from inner entries.
       contents))))

(defun org-icalendar--vevent
  (entry timestamp uid summary location description categories)
  "Create a VEVENT component.

ENTRY is either a headline or an inlinetask element.  TIMESTAMP
is a timestamp object defining the date-time of the event.  UID
is the unique identifier for the event.  SUMMARY defines a short
summary or subject for the event.  LOCATION defines the intended
venue for the event.  DESCRIPTION provides the complete
description of the event.  CATEGORIES defines the categories the
event belongs to.

Return VEVENT component as a string."
  (org-icalendar-fold-string
   (if (eq (org-element-property :type timestamp) 'diary)
       (org-icalendar-transcode-diary-sexp
	(org-element-property :raw-value timestamp) uid summary)
     (concat "BEGIN:VEVENT\n"
	     (org-icalendar-dtstamp) "\n"
	     "UID:" uid "\n"
	     (org-icalendar-convert-timestamp timestamp "DTSTART") "\n"
	     (org-icalendar-convert-timestamp timestamp "DTEND" t) "\n"
	     ;; RRULE.
	     (when (org-element-property :repeater-type timestamp)
	       (format "RRULE:FREQ=%s;INTERVAL=%d\n"
		       (case (org-element-property :repeater-unit timestamp)
			 (hour "HOURLY") (day "DAILY") (week "WEEKLY")
			 (month "MONTHLY") (year "YEARLY"))
		       (org-element-property :repeater-value timestamp)))
	     "SUMMARY:" summary "\n"
	     (and (org-string-nw-p location) (format "LOCATION:%s\n" location))
	     (and (org-string-nw-p description)
		  (format "DESCRIPTION:%s\n" description))
	     "CATEGORIES:" categories "\n"
	     ;; VALARM.
	     (org-icalendar--valarm entry timestamp summary)
	     "END:VEVENT"))))

(defun org-icalendar--vtodo
  (entry uid summary location description categories)
  "Create a VTODO component.

ENTRY is either a headline or an inlinetask element.  UID is the
unique identifier for the task.  SUMMARY defines a short summary
or subject for the task.  LOCATION defines the intended venue for
the task.  DESCRIPTION provides the complete description of the
task.  CATEGORIES defines the categories the task belongs to.

Return VTODO component as a string."
  (let ((start (or (and (memq 'todo-start org-icalendar-use-scheduled)
			(org-element-property :scheduled entry))
		   ;; If we can't use a scheduled time for some
		   ;; reason, start task now.
		   (let ((now (decode-time (current-time))))
		     (list 'timestamp
			   (list :type 'active
				 :minute-start (nth 1 now)
				 :hour-start (nth 2 now)
				 :day-start (nth 3 now)
				 :month-start (nth 4 now)
				 :year-start (nth 5 now)))))))
    (org-icalendar-fold-string
     (concat "BEGIN:VTODO\n"
	     "UID:TODO-" uid "\n"
	     (org-icalendar-dtstamp) "\n"
	     (org-icalendar-convert-timestamp start "DTSTART") "\n"
	     (and (memq 'todo-due org-icalendar-use-deadline)
		  (org-element-property :deadline entry)
		  (concat (org-icalendar-convert-timestamp
			   (org-element-property :deadline entry) "DUE")
			  "\n"))
	     "SUMMARY:" summary "\n"
	     (and (org-string-nw-p location) (format "LOCATION:%s\n" location))
	     (and (org-string-nw-p description)
		  (format "DESCRIPTION:%s\n" description))
	     "CATEGORIES:" categories "\n"
	     "SEQUENCE:1\n"
	     (format "PRIORITY:%d\n"
		     (let ((pri (or (org-element-property :priority entry)
				    org-default-priority)))
		       (floor (- 9 (* 8. (/ (float (- org-lowest-priority pri))
					    (- org-lowest-priority
					       org-highest-priority)))))))
	     (format "STATUS:%s\n"
		     (if (eq (org-element-property :todo-type entry) 'todo)
			 "NEEDS-ACTION"
		       "COMPLETED"))
	     "END:VTODO"))))

(defun org-icalendar--valarm (entry timestamp summary)
  "Create a VALARM component.

ENTRY is the calendar entry triggering the alarm.  TIMESTAMP is
the start date-time of the entry.  SUMMARY defines a short
summary or subject for the task.

Return VALARM component as a string, or nil if it isn't allowed."
  ;; Create a VALARM entry if the entry is timed.  This is not very
  ;; general in that:
  ;; (a) only one alarm per entry is defined,
  ;; (b) only minutes are allowed for the trigger period ahead of the
  ;;     start time,
  ;; (c) only a DISPLAY action is defined.                       [ESF]
  (let ((alarm-time
	 (let ((warntime
		(org-element-property :APPT_WARNTIME entry)))
	   (if warntime (string-to-number warntime) 0))))
    (and (or (> alarm-time 0) (> org-icalendar-alarm-time 0))
	 (org-element-property :hour-start timestamp)
	 (format "BEGIN:VALARM
ACTION:DISPLAY
DESCRIPTION:%s
TRIGGER:-P0DT0H%dM0S
END:VALARM\n"
		 summary
		 (if (zerop alarm-time) org-icalendar-alarm-time alarm-time)))))


;;;; Template

(defun org-icalendar-template (contents info)
  "Return complete document string after iCalendar conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  (org-icalendar--vcalendar
   ;; Name.
   (if (not (plist-get info :input-file)) (buffer-name (buffer-base-buffer))
     (file-name-nondirectory
      (file-name-sans-extension (plist-get info :input-file))))
   ;; Owner.
   (if (not (plist-get info :with-author)) ""
     (org-export-data (plist-get info :author) info))
   ;; Timezone.
   (if (org-string-nw-p org-icalendar-timezone) org-icalendar-timezone
     (cadr (current-time-zone)))
   ;; Description.
   (org-export-data (plist-get info :title) info)
   contents))

(defun org-icalendar--vcalendar (name owner tz description contents)
  "Create a VCALENDAR component.
NAME, OWNER, TZ, DESCRIPTION and CONTENTS are all strings giving,
respectively, the name of the calendar, its owner, the timezone
used, a short description and the other components included."
  (concat (format "BEGIN:VCALENDAR
VERSION:2.0
X-WR-CALNAME:%s
PRODID:-//%s//Emacs with Org mode//EN
X-WR-TIMEZONE:%s
X-WR-CALDESC:%s
CALSCALE:GREGORIAN\n"
		  (org-icalendar-cleanup-string name)
		  (org-icalendar-cleanup-string owner)
		  (org-icalendar-cleanup-string tz)
		  (org-icalendar-cleanup-string description))
	  contents
	  "END:VCALENDAR\n"))



;;; Interactive Functions

;;;###autoload
(defun org-icalendar-export-to-ics
  (&optional async subtreep visible-only body-only)
  "Export current buffer to an iCalendar file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"BEGIN:VCALENDAR\" and \"END:VCALENDAR\".

Return ICS file name."
  (interactive)
  (let ((file (buffer-file-name (buffer-base-buffer))))
    (when (and file org-icalendar-store-UID)
      (org-icalendar-create-uid file 'warn-user)))
  ;; Export part.  Since this back-end is backed up by `ascii', ensure
  ;; links will not be collected at the end of sections.
  (let ((outfile (org-export-output-file-name ".ics" subtreep)))
    (org-export-to-file 'icalendar outfile
      async subtreep visible-only body-only '(:ascii-charset utf-8)
      (lambda (file)
	(run-hook-with-args 'org-icalendar-after-save-hook file) nil))))

;;;###autoload
(defun org-icalendar-export-agenda-files (&optional async)
  "Export all agenda files to iCalendar files.
When optional argument ASYNC is non-nil, export happens in an
external process."
  (interactive)
  (if async
      ;; Asynchronous export is not interactive, so we will not call
      ;; `org-check-agenda-file'.  Instead we remove any non-existent
      ;; agenda file from the list.
      (let ((files (org-remove-if-not 'file-exists-p (org-agenda-files t))))
	(org-export-async-start
	    (lambda (results)
	      (mapc (lambda (f) (org-export-add-to-stack f 'icalendar))
		    results))
	  `(let (output-files)
	     (mapc (lambda (file)
		     (with-current-buffer (org-get-agenda-file-buffer file)
		       (push (expand-file-name (org-icalendar-export-to-ics))
			     output-files)))
		   ',files)
	     output-files)))
    (let ((files (org-agenda-files t)))
      (org-agenda-prepare-buffers files)
      (unwind-protect
	  (mapc (lambda (file)
		  (catch 'nextfile
		    (org-check-agenda-file file)
		    (with-current-buffer (org-get-agenda-file-buffer file)
		      (org-icalendar-export-to-ics))))
		files)
	(org-release-buffers org-agenda-new-buffers)))))

;;;###autoload
(defun org-icalendar-combine-agenda-files (&optional async)
  "Combine all agenda files into a single iCalendar file.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

The file is stored under the name chosen in
`org-icalendar-combined-agenda-file'."
  (interactive)
  (if async
      (let ((files (org-remove-if-not 'file-exists-p (org-agenda-files t))))
	(org-export-async-start
	    (lambda (dummy)
	      (org-export-add-to-stack
	       (expand-file-name org-icalendar-combined-agenda-file)
	       'icalendar))
	  `(apply 'org-icalendar--combine-files nil ',files)))
    (apply 'org-icalendar--combine-files nil (org-agenda-files t))))

(defun org-icalendar-export-current-agenda (file)
  "Export current agenda view to an iCalendar FILE.
This function assumes major mode for current buffer is
`org-agenda-mode'."
  (let (org-export-babel-evaluate ; Don't evaluate Babel block
	(org-icalendar-combined-agenda-file file)
	(marker-list
	 ;; Collect the markers pointing to entries in the current
	 ;; agenda buffer.
	 (let (markers)
	   (save-excursion
	     (goto-char (point-min))
	     (while (not (eobp))
	       (let ((m (or (org-get-at-bol 'org-hd-marker)
			    (org-get-at-bol 'org-marker))))
		 (and m (push m markers)))
	       (beginning-of-line 2)))
	   (nreverse markers))))
    (apply 'org-icalendar--combine-files
	   ;; Build restriction alist.
	   (let (restriction)
	     ;; Sort markers in each association within RESTRICTION.
	     (mapcar (lambda (x) (setcdr x (sort (copy-sequence (cdr x)) '<)) x)
		     (dolist (m marker-list restriction)
		       (let* ((pos (marker-position m))
			      (file (buffer-file-name
				     (org-base-buffer (marker-buffer m))))
			      (file-markers (assoc file restriction)))
			 ;; Add POS in FILE association if one exists
			 ;; or create a new association for FILE.
			 (if file-markers (push pos (cdr file-markers))
			   (push (list file pos) restriction))))))
	   (org-agenda-files nil 'ifmode))))

(defun org-icalendar--combine-files (restriction &rest files)
  "Combine entries from multiple files into an iCalendar file.
RESTRICTION, when non-nil, is an alist where key is a file name
and value a list of buffer positions pointing to entries that
should appear in the calendar.  It only makes sense if the
function was called from an agenda buffer.  FILES is a list of
files to build the calendar from."
  (org-agenda-prepare-buffers files)
  (unwind-protect
      (progn
	(with-temp-file org-icalendar-combined-agenda-file
	  (insert
	   (org-icalendar--vcalendar
	    ;; Name.
	    org-icalendar-combined-name
	    ;; Owner.
	    user-full-name
	    ;; Timezone.
	    (or (org-string-nw-p org-icalendar-timezone)
		(cadr (current-time-zone)))
	    ;; Description.
	    org-icalendar-combined-description
	    ;; Contents.
	    (concat
	     ;; Agenda contents.
	     (mapconcat
	      (lambda (file)
		(catch 'nextfile
		  (org-check-agenda-file file)
		  (with-current-buffer (org-get-agenda-file-buffer file)
		    (let ((marks (cdr (assoc (expand-file-name file)
					     restriction))))
		      ;; Create ID if necessary.
		      (when org-icalendar-store-UID
			(org-icalendar-create-uid file t marks))
		      (unless (and restriction (not marks))
			;; Add a hook adding :ICALENDAR_MARK: property
			;; to each entry appearing in agenda view.
			;; Use `apply-partially' because the function
			;; still has to accept one argument.
			(let ((org-export-before-processing-hook
			       (cons (apply-partially
				      (lambda (m-list dummy)
					(mapc (lambda (m)
						(org-entry-put
						 m "ICALENDAR-MARK" "t"))
					      m-list))
				      (sort marks '>))
				     org-export-before-processing-hook)))
			  (org-export-as
			   'icalendar nil nil t
			   (list :ascii-charset 'utf-8
				 :icalendar-agenda-view restriction))))))))
	      files "")
	     ;; BBDB anniversaries.
	     (when (and org-icalendar-include-bbdb-anniversaries
			(require 'org-bbdb nil t))
	       (with-output-to-string (org-bbdb-anniv-export-ical)))))))
	(run-hook-with-args 'org-icalendar-after-save-hook
			    org-icalendar-combined-agenda-file))
    (org-release-buffers org-agenda-new-buffers)))


(provide 'ox-icalendar)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-icalendar.el ends here
