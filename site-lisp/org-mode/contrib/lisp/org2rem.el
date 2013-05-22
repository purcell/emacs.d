;;; org2rem.el --- Convert org appointments into reminders

;; Copyright (C) 2006-2012 Free Software Foundation, Inc.

;; Author: Bastien Guerry and Shatad Pratap
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.09a
;;
;; This file is not part of GNU Emacs.
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; (require 'org2rem)
;; To export, do
;;
;;    M-x org2rem-combine-agenda-files
;;
;; Then you can use reming like this:
;;
;;  $ remind ~/org.rem
;;
;; If you want to use this regualrly, try in .emacs
;;
;;    (add-hook 'org-mode-hook
;;       (lambda() (add-hook 'after-save-hook
;;                 'org-export-remind-all-agenda-files t t)))

(require 'org)
(require 'org-agenda)
(require 'org-exp)
(eval-and-compile
  (require 'cl))

(defgroup org2rem nil
  "Options specific for Remind export of Org-mode files."
  :tag "Org Export Remind"
  :group 'org-export)

(defcustom org-combined-agenda-remind-file "~/org.rem"
  "The file name for the Remind file covering all agenda files.
This file is created with the command \\[org2rem-all-agenda-files].
The file name should be absolute, the file will be overwritten without warning."
  :group 'org2rem
  :type 'file)

(defcustom org-remind-combined-name "OrgMode"
  "Calendar name for the combined Remind representing all agenda files."
  :group 'org2rem
  :type 'string)

(defcustom org-remind-use-deadline '(event-if-not-todo todo-due)
  "Contexts where Remind export should use a deadline time stamp.
This is a list with several symbols in it.  Valid symbol are:

event-if-todo       Deadlines in TODO entries become calendar events.
event-if-not-todo   Deadlines in non-TODO entries become calendar events.
todo-due            Use deadlines in TODO entries as due-dates"
  :group 'org2rem
  :type '(set :greedy t
	      (const :tag "Deadlines in non-TODO entries become events"
		     event-if-not-todo)
	      (const :tag "Deadline in TODO entries become events"
		     event-if-todo)
	      (const :tag "Deadlines in TODO entries become due-dates"
		     todo-due)))

(defcustom org-remind-use-scheduled '(todo-start)
  "Contexts where Remind export should use a scheduling time stamp.
This is a list with several symbols in it.  Valid symbol are:

event-if-todo       Scheduling time stamps in TODO entries become an event.
event-if-not-todo   Scheduling time stamps in non-TODO entries become an event.
todo-start          Scheduling time stamps in TODO entries become start date.
                    Some calendar applications show TODO entries only after
                    that date."
  :group 'org2rem
  :type '(set :greedy t
	      (const :tag
		     "SCHEDULED timestamps in non-TODO entries become events"
		     event-if-not-todo)
	      (const :tag "SCHEDULED timestamps in TODO entries become events"
		     event-if-todo)
	      (const :tag "SCHEDULED in TODO entries become start date"
		     todo-start)))

(defcustom org-remind-categories '(local-tags category)
  "Items that should be entered into the categories field.
This is a list of symbols, the following are valid:

category    The Org-mode category of the current file or tree
todo-state  The todo state, if any
local-tags  The tags, defined in the current line
all-tags    All tags, including inherited ones."
  :group 'org2rem
  :type '(repeat
	  (choice
	   (const :tag "The file or tree category" category)
	   (const :tag "The TODO state" todo-state)
	   (const :tag "Tags defined in current line" local-tags)
	   (const :tag "All tags, including inherited ones" all-tags))))

(defcustom org-remind-include-todo nil
  "Non-nil means export to remind files should also cover TODO items."
  :group 'org2rem
  :type '(choice
	  (const :tag "None" nil)
	  (const :tag "Unfinished" t)
	  (const :tag "All" all)))

(defcustom org-remind-include-sexps t
  "Non-nil means export to Remind files should also cover sexp entries.
These are entries like in the diary, but directly in an Org-mode file."
  :group 'org2rem
  :type 'boolean)

(defcustom org-remind-deadline-over-scheduled t
  "Non-nil means use deadline as target when both deadline and
scheduled present, vice-versa. Default is Non-nil."
  :group 'org2rem
  :type 'boolean)

(defcustom org-remind-escape-percentage t
  "Non-nil means % will be escaped, vice-versa. Default is Non-nil."
  :group 'org2rem
  :type 'boolean)

(defcustom org-remind-extra-warn-days 3
  "Extra days Remind keep reminding."
  :group 'org2rem
  :type 'number)

(defcustom org-remind-advanced-warn-days 3
  "Advanced days Remind start reminding."
  :group 'org2rem
  :type 'number)

(defcustom org-remind-suppress-last-newline nil
  "Non-nil means suppress last newline REM body. Default is nil."
  :group 'org2rem
  :type 'boolean)

(defcustom org-remind-include-body 100
  "Amount of text below headline to be included in Remind export.
This is a number of characters that should maximally be included.
Properties, scheduling and clocking lines will always be removed.
The text will be inserted into the DESCRIPTION field."
  :group 'org2rem
  :type '(choice
	  (const :tag "Nothing" nil)
	  (const :tag "Everything" t)
	  (integer :tag "Max characters")))

(defcustom org-remind-store-UID nil
  "Non-nil means store any created UIDs in properties.
The Remind standard requires that all entries have a unique identifyer.
Org will create these identifiers as needed.  When this variable is non-nil,
the created UIDs will be stored in the ID property of the entry.  Then the
next time this entry is exported, it will be exported with the same UID,
superceeding the previous form of it.  This is essential for
synchronization services.
This variable is not turned on by default because we want to avoid creating
a property drawer in every entry if people are only playing with this feature,
or if they are only using it locally."
  :group 'org2rem
  :type 'boolean)

;;;; Exporting

;;; Remind export

;;;###autoload
(defun org2rem-this-file ()
  "Export current file as an Remind file.
The Remind file will be located in the same directory as the Org-mode
file, but with extension `.rem'."
  (interactive)
  (org2rem nil buffer-file-name))

;;;###autoload
(defun org2rem-all-agenda-files ()
  "Export all files in `org-agenda-files' to Remind .rem files.
Each Remind file will be located in the same directory as the Org-mode
file, but with extension `.rem'."
  (interactive)
  (apply 'org2rem nil (org-agenda-files t)))

;;;###autoload
(defun org2rem-combine-agenda-files ()
  "Export all files in `org-agenda-files' to a single combined Remind file.
The file is stored under the name `org-combined-agenda-remind-file'."
  (interactive)
  (apply 'org2rem t (org-agenda-files t)))

(defun org2rem (combine &rest files)
  "Create Remind files for all elements of FILES.
If COMBINE is non-nil, combine all calendar entries into a single large
file and store it under the name `org-combined-agenda-remind-file'."
  (save-excursion
    (org-prepare-agenda-buffers files)
    (let* ((dir (org-export-directory
		 :ical (list :publishing-directory
			     org-export-publishing-directory)))
	   file rem-file rem-buffer category started org-agenda-new-buffers)
      (and (get-buffer "*rem-tmp*") (kill-buffer "*rem-tmp*"))
      (when combine
	(setq rem-file
	      (if (file-name-absolute-p org-combined-agenda-remind-file)
		  org-combined-agenda-remind-file
		(expand-file-name org-combined-agenda-remind-file dir))
	      rem-buffer (org-get-agenda-file-buffer rem-file))
	(set-buffer rem-buffer) (erase-buffer))
      (while (setq file (pop files))
	(catch 'nextfile
	  (org-check-agenda-file file)
	  (set-buffer (org-get-agenda-file-buffer file))
	  (unless combine
	    (setq rem-file (concat (file-name-as-directory dir)
				    (file-name-sans-extension
				     (file-name-nondirectory buffer-file-name))
				    ".rem"))
	    (setq rem-buffer (org-get-agenda-file-buffer rem-file))
	    (with-current-buffer rem-buffer (erase-buffer)))
	  (setq category (or org-category
			     (file-name-sans-extension
			      (file-name-nondirectory buffer-file-name))))
	  (if (symbolp category) (setq category (symbol-name category)))
	  (let ((standard-output rem-buffer))
	    (if combine
		(and (not started) (setq started t)
		     (org-start-remind-file org-remind-combined-name))
	      (org-start-remind-file category))
	    (org-print-remind-entries combine)
	    (when (or (and combine (not files)) (not combine))
	      (org-finish-remind-file)
	      (set-buffer rem-buffer)
	      (run-hooks 'org-before-save-Remind-file-hook)
	      (save-buffer)
	      (run-hooks 'org-after-save-Remind-file-hook)
	      (and (boundp 'org-wait) (numberp org-wait) (sit-for org-wait))
	      ))))
      (org-release-buffers org-agenda-new-buffers))))

(defvar org-before-save-Remind-file-hook nil
  "Hook run before  an Remind file has been saved.
This can be used to modify the result of the export.")

(defvar org-after-save-Remind-file-hook nil
  "Hook run after an Remind file has been saved.
The Remind buffer is still current when this hook is run.
A good way to use this is to tell a desktop calenndar application to re-read
the Remind file.")

(defvar org-agenda-default-appointment-duration) ; defined in org-agenda.el
(defun org-print-remind-entries (&optional combine)
  "Print Remind entries for the current Org-mode file to `standard-output'.
When COMBINE is non nil, add the category to each line."
  (require 'org-agenda)
  (let ((re1 (concat org-ts-regexp "\\|<%%([^>\n]+>"))
	(re2 (concat "--?-?\\(" org-ts-regexp "\\)"))
	(dts (org-rem-ts-to-string
	      (format-time-string (cdr org-time-stamp-formats) (current-time))
	      "start time:"))
	hd ts ts2 state status (inc t) pos b sexp rrule
	scheduledp deadlinep todo prefix due start
	tmp pri categories entry location summary desc uid
        remind-aw remind-ew (org-rem-ew org-remind-extra-warn-days)
        (org-rem-aw org-remind-advanced-warn-days)
        trigger diff-days (dos org-remind-deadline-over-scheduled)
        (suppress-last-newline org-remind-suppress-last-newline)
	(sexp-buffer (get-buffer-create "*rem-tmp*")))
    (org-refresh-category-properties)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re1 nil t)
	(catch :skip
	  (org-agenda-skip)
	  (when (boundp 'org-remind-verify-function)
	    (unless (funcall org-remind-verify-function)
	      (outline-next-heading)
	      (backward-char 1)
	      (throw :skip nil)))
	  (setq pos (match-beginning 0)
		ts (match-string 0)
		inc t
		hd (condition-case nil
		       (org-remind-cleanup-string
			(org-get-heading))
		     (error (throw :skip nil)))
		summary (org-remind-cleanup-string
			 (org-entry-get nil "SUMMARY"))
		desc (org-remind-cleanup-string
		      (or (org-entry-get nil "DESCRIPTION")
			  (and org-remind-include-body (org-get-entry)))
		      t org-remind-include-body)
		location (org-remind-cleanup-string
			  (org-entry-get nil "LOCATION"))
		uid (if org-remind-store-UID
			(org-id-get-create)
		      (or (org-id-get) (org-id-new)))
		categories (org-export-get-remind-categories)
		deadlinep nil scheduledp nil)
	  (if (looking-at re2)
	      (progn
		(goto-char (match-end 0))
		(setq ts2 (match-string 1)
		      inc (not (string-match "[0-9]\\{1,2\\}:[0-9][0-9]" ts2))))
	    (setq tmp (buffer-substring (max (point-min)
					     (- pos org-ds-keyword-length))
					pos)
		  ts2 (if (string-match "[0-9]\\{1,2\\}:[0-9][0-9]-\\([0-9]\\{1,2\\}:[0-9][0-9]\\)" ts)
			  (progn
			    (setq inc nil)
			    (replace-match "\\1" t nil ts))
			ts)
		  deadlinep (string-match org-deadline-regexp tmp)
		  scheduledp (string-match org-scheduled-regexp tmp)
		  todo (org-get-todo-state)
		  ;; donep (org-entry-is-done-p)
		  ))
	  (when (and
		 deadlinep
		 (if todo
		     (not (memq 'event-if-todo org-remind-use-deadline))
		   (not (memq 'event-if-not-todo org-remind-use-deadline))))
	    (throw :skip t))
	  (when (and
		 scheduledp
		 (if todo
		     (not (memq 'event-if-todo org-remind-use-scheduled))
		   (not (memq 'event-if-not-todo org-remind-use-scheduled))))
	    (throw :skip t))
	  (setq prefix (if deadlinep "DEADLINE-" (if scheduledp "SCHEDULED-" "TS-")))
	  (if (or (string-match org-tr-regexp hd)
		  (string-match org-ts-regexp hd))
	      (setq hd (replace-match "" t t hd)))
	  (if (string-match "\\+\\([0-9]+\\)\\([dwmy]\\)>" ts)
	      (setq rrule               ;is recurrence value. later give it good name.
                    (* (string-to-number
                        (cdr (assoc
                              (match-string 2 ts)
                              '(("d" . "1")("w" . "7")
                                ("m" . "0")("y" . "0")))))
                       (string-to-number (match-string 1 ts))))
	    (setq rrule nil))
	  (setq summary (or summary hd))
	  (if (string-match org-bracket-link-regexp summary)
	      (setq summary
		    (replace-match (if (match-end 3)
				       (match-string 3 summary)
				     (match-string 1 summary))
				   t t summary)))
	  (if deadlinep (setq summary (concat "DEADLINE: " summary)))
	  (if scheduledp (setq summary (concat "SCHEDULED: " summary)))
	  (if (string-match "\\`<%%" ts)
	      (with-current-buffer sexp-buffer
		(insert (substring ts 1 -1) " " summary "\n"))
	    (princ (format "\n## BEGIN:EVENT
## UID: %s
REM %s %s MSG EVENT:%s%s %s%s%%
## CATEGORIES:%s
## END:EVENT\n"
			   (concat prefix uid)
			   (org-rem-ts-to-string ts nil nil rrule)
			   (org-rem-ts-to-string ts2 "UNTIL " inc)
			   summary
			   (if (and desc (string-match "\\S-" desc))
			       (concat "%_\\\n" desc) "")
			   (if (and location (string-match "\\S-" location))
			       (concat "\nLOCATION: " location) "")
			   (if suppress-last-newline "" "%_")
                           categories)))))

      (when (and org-remind-include-sexps
		 (condition-case nil (require 'remind) (error nil))
		 (fboundp 'remind-export-region))
	;; Get all the literal sexps
	(goto-char (point-min))
	(while (re-search-forward "^&?%%(" nil t)
	  (catch :skip
	    (org-agenda-skip)
	    (setq b (match-beginning 0))
	    (goto-char (1- (match-end 0)))
	    (forward-sexp 1)
	    (end-of-line 1)
	    (setq sexp (buffer-substring b (point)))
	    (with-current-buffer sexp-buffer
	      (insert sexp "\n"))))
	;; (princ (org-diary-to-rem-string sexp-buffer))
	(kill-buffer sexp-buffer))

      (when org-remind-include-todo
	(setq prefix "TODO-")
	(goto-char (point-min))
	(while (re-search-forward org-todo-line-regexp nil t)
	  (catch :skip
	    (org-agenda-skip)
	    (when (boundp 'org-remind-verify-function)
	      (unless (funcall org-remind-verify-function)
		(outline-next-heading)
		(backward-char 1)
		(throw :skip nil)))
	    (setq state (match-string 2))
	    (setq status (if (member state org-done-keywords)
			     "COMPLETED" "NEEDS-ACTION"))
	    (when (and state
		       (or (not (member state org-done-keywords))
			   (eq org-remind-include-todo 'all))
		       (not (member org-archive-tag (org-get-tags-at)))
		       )
	      (setq hd (match-string 3)
		    summary (org-remind-cleanup-string
			     (org-entry-get nil "SUMMARY"))
		    desc (org-remind-cleanup-string
			  (or (org-entry-get nil "DESCRIPTION")
			      (and org-remind-include-body (org-get-entry)))
			  t org-remind-include-body)
		    location (org-remind-cleanup-string
			      (org-entry-get nil "LOCATION"))
		    due (and (member 'todo-due org-remind-use-deadline)
			     (org-entry-get nil "DEADLINE"))
		    start (and (member 'todo-start org-remind-use-scheduled)
			     (org-entry-get nil "SCHEDULED"))
		    categories (org-export-get-remind-categories)
		    uid (if org-remind-store-UID
			    (org-id-get-create)
			  (or (org-id-get) (org-id-new))))

              (if (and due start)
                  (setq diff-days (org-rem-time-diff-days due start)))

              (setq remind-aw
                    (if due
                        (if diff-days
                            (if (> diff-days 0)
                                (if dos diff-days 0)
                              (if dos 0 diff-days))
                          1000)))

              (if (and (numberp org-rem-aw) (> org-rem-aw 0))
                  (setq remind-aw (+ (or remind-aw 0) org-rem-aw)))

              (setq remind-ew
                    (if due
                        (if diff-days
                            (if (> diff-days 0) due nil)
                          due)))

              (setq trigger (if dos (if due due start) (if start start due)))
              ;; (and trigger (setq trigger (org-rem-ts-to-string trigger nil nil 1 remind-aw)))
              (if trigger
                   (setq trigger (concat
                                  (format "[trigger('%s')] *%d "
                                          (org-rem-ts-to-remind-date-type trigger) 1)
                                  (if remind-aw (format "++%d" remind-aw)))))
              (and due (setq due (org-rem-ts-to-remind-date-type due)))
              (and start (setq start (org-rem-ts-to-remind-date-type start)))
              (and remind-ew (setq remind-ew (org-rem-ts-to-remind-date-type remind-ew)))

	      (if (string-match org-bracket-link-regexp hd)
		  (setq hd (replace-match (if (match-end 3) (match-string 3 hd)
					    (match-string 1 hd))
					  t t hd)))
	      (if (string-match org-priority-regexp hd)
		  (setq pri (string-to-char (match-string 2 hd))
			hd (concat (substring hd 0 (match-beginning 1))
				   (substring hd (match-end 1))))
		(setq pri org-default-priority))
	      (setq pri (floor (1+ (* 8. (/ (float (- org-lowest-priority pri))
					    (- org-lowest-priority org-highest-priority))))))

	      (princ (format "\n## BEGIN:TODO
## UID: %s
REM %s %s %s MSG TODO: %s%s%s%s%s%s%%
## CATEGORIES:%s
## SEQUENCE:1
## STATUS:%s
## END:TODO\n"
			     (concat prefix uid)
			     (or trigger "") ;; dts)
			     (if remind-ew (format "UNTIL [trigger('%s' + %d)]" remind-ew (or org-rem-ew 0)) "")
                             (if pri (format "PRIORITY %d" pri) "")
			     (or summary hd)
			     (if (and desc (string-match "\\S-" desc))
				 (concat "%_\\\nDESCRIPTION: " desc) "")
			     (if (and location (string-match "\\S-" location))
				 (concat "LOCATION: " location) "")
                             (if start
                                 (concat
                                  "%_\\\n['" start "' - today()] "
                                  "days over, for scheduled date - "
                                  "[trigger('" start "')]") "")
                             (if due
                                 (concat
                                  "%_\\\n[today() - '" due "'] "
                                  "days left, to deadline date - "
                                  "[trigger('" due "')]") "")
                             (if suppress-last-newline "" "%_")
			     categories
			     status)))))))))

(defun org-export-get-remind-categories ()
  "Get categories according to `org-remind-categories'."
  (let ((cs org-remind-categories) c rtn tmp)
    (while (setq c (pop cs))
      (cond
       ((eq c 'category) (push (org-get-category) rtn))
       ((eq c 'todo-state)
	(setq tmp (org-get-todo-state))
	(and tmp (push tmp rtn)))
       ((eq c 'local-tags)
	(setq rtn (append (nreverse (org-get-local-tags-at (point))) rtn)))
       ((eq c 'all-tags)
	(setq rtn (append (nreverse (org-get-tags-at (point))) rtn)))))
    (mapconcat 'identity (nreverse rtn) ",")))

(defun org-remind-cleanup-string (s &optional is-body maxlength)
  "Take out stuff and quote what needs to be quoted.
When IS-BODY is non-nil, assume that this is the body of an item, clean up
whitespace, newlines, drawers, and timestamps, and cut it down to MAXLENGTH
characters."
  (if (or (not s) (string-match "^[ \t\n]*$" s))
      nil
    (when is-body
      (let ((re (concat "\\(" org-drawer-regexp "\\)[^\000]*?:END:.*\n?"))
	    (re2 (concat "^[ \t]*" org-keyword-time-regexp ".*\n?")))
	(while (string-match re s) (setq s (replace-match "" t t s)))
	(while (string-match re2 s) (setq s (replace-match "" t t s)))))
    (if org-remind-escape-percentage
        (let ((start 0))
          (while (string-match "\\([%]\\)" s start)
            (setq start (+ (match-beginning 0) 2)
                  s (replace-match "\\1\\1" nil nil s)))))

    (let ((start 0))
      (while (string-match "\\([\n]\\)" s start)
	(setq start (+ (match-beginning 0) 4) ;; less than 4 is not correct.
	      s (replace-match "%_\\\\\\1" nil nil s))))

    (let ((start 0))
      (while (string-match "\\([[]\\)" s start)
	(setq start (+ (match-beginning 0) 5)
	      s (replace-match (concat "\[" "\"" "\\1" "\"" "\]") nil nil s))))

;;;     (when is-body
;;;       (while (string-match "[ \t]*\n[ \t]*" s)
;;; 	(setq s (replace-match "%_" t t s))))

    (setq s (org-trim s))
    (if is-body
	(if maxlength
	    (if (and (numberp maxlength)
		     (> (length s) maxlength))
		(setq s (substring s 0 maxlength)))))
    s))

(defun org-get-entry ()
  "Clean-up description string."
  (save-excursion
    (org-back-to-heading t)
    (buffer-substring (point-at-bol 2) (org-end-of-subtree t))))

(defun org-start-remind-file (name)
  "Start an Remind file by inserting the header."
  (let ((user user-full-name)
	(name (or name "unknown"))
	(timezone (cadr (current-time-zone))))
    (princ
     (format "# -*- Mode: shell-script; auto-fill-mode: nil -*-
## BEGIN: Reminders
## VERSION:2.0
## Emacs with Org-mode
## Calendar:%s
## Created by: %s
## Timezone:%s
## Calscale:Gregorian\n" name user timezone))))

(defun org-finish-remind-file ()
  "Finish an Remind file by inserting the END statement."
  (princ "\n## END:Reminders\n"))

(defun org-rem-ts-to-remind-date-type (s)
  (format-time-string
         "%Y-%m-%d"
         (apply 'encode-time (butlast (org-parse-time-string s) 3))))

;; (defun org-rem-date-type-to-string (s keyword &optional inc day-repeat day-advance-warn)
;;   (if trigger
;;       (setq trigger
;;             (concat
;;              (format "[trigger('%s')] *%d "
;;                      (org-rem-ts-to-remind-date-type trigger) day-repeat)
;;              (if day-advance-warn (format "++%d" day-advance-warn))))))

;; (format-time-string "%Y"
;;  (apply 'encode-time (butlast (org-parse-time-string "<2008-11-20 Thu 10:30>") 3)))

(defun org-rem-ts-to-string (s keyword &optional inc day-repeat day-advance-warn)
  "Take a time string S and convert it to Remind format.
KEYWORD is added in front, to make a complete line like DTSTART....
When INC is non-nil, increase the hour by two (if time string contains
a time), or the day by one (if it does not contain a time)."
  (let ((t1 (org-parse-time-string s 'nodefault))
	t2 fmt have-time time)
    (if (and (car t1) (nth 1 t1) (nth 2 t1))
	(setq t2 t1 have-time t)
      (setq t2 (org-parse-time-string s)))
    (let ((s (car t2))   (mi (nth 1 t2)) (h (nth 2 t2))
	  (d (nth 3 t2)) (m  (nth 4 t2)) (y (nth 5 t2)))
      (when inc
	(if have-time
	    (if org-agenda-default-appointment-duration
		(setq mi (+ org-agenda-default-appointment-duration mi))
	      (setq h (+ 2 h)))
	  (setq d (1+ d))))
      (setq time (encode-time s mi h d m y)))
    (setq fmt (concat
               "%d %b %Y"
               (if day-advance-warn (format " ++%d" day-advance-warn))
               (if day-repeat (format " *%d" day-repeat))
               (if have-time " AT %H:%M")))
    (concat keyword (format-time-string fmt time))))

(defun org-rem-time-diff-days (end start)
  (floor (/ (apply '- (mapcar
             (lambda (s)
               (let*
                   ((t1 (org-parse-time-string s))
                    (s (car t1)) (mi (nth 1 t1))
                    (h (nth 2 t1)) (d (nth 3 t1))
                    (m  (nth 4 t1)) (y (nth 5 t1)))
                 (float-time (encode-time s mi h d m y))))
             (list end start))) (* 24 60 60))))

(provide 'org2rem)

;;; org-exp.el ends here
