;;; org-datetree.el --- Create date entries in a tree -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017 Free Software Foundation, Inc.

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

;; This file contains code to create entries in a tree where the top-level
;; nodes represent years, the level 2 nodes represent the months, and the
;; level 1 entries days.

;;; Code:

(require 'org)

(defvar org-datetree-base-level 1
  "The level at which years should be placed in the date tree.
This is normally one, but if the buffer has an entry with a
DATE_TREE (or WEEK_TREE for ISO week entries) property (any
value), the date tree will become a subtree under that entry, so
the base level will be properly adjusted.")

(defcustom org-datetree-add-timestamp nil
  "When non-nil, add a time stamp matching date of entry.
Added time stamp is active unless value is `inactive'."
  :group 'org-capture
  :version "24.3"
  :type '(choice
	  (const :tag "Do not add a time stamp" nil)
	  (const :tag "Add an inactive time stamp" inactive)
	  (const :tag "Add an active time stamp" active)))

;;;###autoload
(defun org-datetree-find-date-create (d &optional keep-restriction)
  "Find or create an entry for date D.
If KEEP-RESTRICTION is non-nil, do not widen the buffer.
When it is nil, the buffer will be widened to make sure an existing date
tree can be found.  If it is the symbol `subtree-at-point', then the tree
will be built under the headline at point."
  (setq-local org-datetree-base-level 1)
  (save-restriction
    (if (eq keep-restriction 'subtree-at-point)
	(progn
	  (unless (org-at-heading-p) (error "Not at heading"))
	  (widen)
	  (org-narrow-to-subtree)
	  (setq-local org-datetree-base-level
		      (org-get-valid-level (org-current-level) 1)))
      (unless keep-restriction (widen))
      ;; Support the old way of tree placement, using a property
      (let ((prop (org-find-property "DATE_TREE")))
	(when prop
	  (goto-char prop)
	  (setq-local org-datetree-base-level
		      (org-get-valid-level (org-current-level) 1))
	  (org-narrow-to-subtree))))
    (goto-char (point-min))
    (let ((year (calendar-extract-year d))
	  (month (calendar-extract-month d))
	  (day (calendar-extract-day d)))
      (org-datetree--find-create
       "^\\*+[ \t]+\\([12][0-9]\\{3\\}\\)\\(\\s-*?\
\\([ \t]:[[:alnum:]:_@#%%]+:\\)?\\s-*$\\)"
       year)
      (org-datetree--find-create
       "^\\*+[ \t]+%d-\\([01][0-9]\\) \\w+$"
       year month)
      (org-datetree--find-create
       "^\\*+[ \t]+%d-%02d-\\([0123][0-9]\\) \\w+$"
       year month day))))

;;;###autoload
(defun org-datetree-find-iso-week-create (d &optional keep-restriction)
  "Find or create an ISO week entry for date D.
Compared to `org-datetree-find-date-create' this function creates
entries ordered by week instead of months.
When it is nil, the buffer will be widened to make sure an existing date
tree can be found.  If it is the symbol `subtree-at-point', then the tree
will be built under the headline at point."
  (setq-local org-datetree-base-level 1)
  (save-restriction
    (if (eq keep-restriction 'subtree-at-point)
	(progn
	  (unless (org-at-heading-p) (error "Not at heading"))
	  (widen)
	  (org-narrow-to-subtree)
	  (setq-local org-datetree-base-level
		      (org-get-valid-level (org-current-level) 1)))
      (unless keep-restriction (widen))
      ;; Support the old way of tree placement, using a property
      (let ((prop (org-find-property "WEEK_TREE")))
	(when prop
	  (goto-char prop)
	  (setq-local org-datetree-base-level
		      (org-get-valid-level (org-current-level) 1))
	  (org-narrow-to-subtree))))
    (goto-char (point-min))
    (require 'cal-iso)
    (let* ((year (calendar-extract-year d))
	   (month (calendar-extract-month d))
	   (day (calendar-extract-day d))
	   (time (encode-time 0 0 0 day month year))
	   (iso-date (calendar-iso-from-absolute
		      (calendar-absolute-from-gregorian d)))
	   (weekyear (nth 2 iso-date))
	   (week (nth 0 iso-date)))
      ;; ISO 8601 week format is %G-W%V(-%u)
      (org-datetree--find-create
       "^\\*+[ \t]+\\([12][0-9]\\{3\\}\\)\\(\\s-*?\
\\([ \t]:[[:alnum:]:_@#%%]+:\\)?\\s-*$\\)"
       weekyear nil nil
       (format-time-string "%G" time))
      (org-datetree--find-create
       "^\\*+[ \t]+%d-W\\([0-5][0-9]\\)$"
       weekyear week nil
       (format-time-string "%G-W%V" time))
      ;; For the actual day we use the regular date instead of ISO week.
      (org-datetree--find-create
       "^\\*+[ \t]+%d-%02d-\\([0123][0-9]\\) \\w+$"
       year month day))))

(defun org-datetree--find-create (regex year &optional month day insert)
  "Find the datetree matched by REGEX for YEAR, MONTH, or DAY.
REGEX is passed to `format' with YEAR, MONTH, and DAY as
arguments.  Match group 1 is compared against the specified date
component.  If INSERT is non-nil and there is no match then it is
inserted into the buffer."
  (when (or month day)
    (org-narrow-to-subtree))
  (let ((re (format regex year month day))
	match)
    (goto-char (point-min))
    (while (and (setq match (re-search-forward re nil t))
		(goto-char (match-beginning 1))
		(< (string-to-number (match-string 1)) (or day month year))))
    (cond
     ((not match)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (org-datetree-insert-line year month day insert))
     ((= (string-to-number (match-string 1)) (or day month year))
      (beginning-of-line))
     (t
      (beginning-of-line)
      (org-datetree-insert-line year month day insert)))))

(defun org-datetree-insert-line (year &optional month day text)
  (delete-region (save-excursion (skip-chars-backward " \t\n") (point)) (point))
  (insert "\n" (make-string org-datetree-base-level ?*) " \n")
  (backward-char)
  (when month (org-do-demote))
  (when day (org-do-demote))
  (if text
      (insert text)
    (insert (format "%d" year))
    (when month
      (insert
       (if day
	   (format-time-string "-%m-%d %A" (encode-time 0 0 0 day month year))
	 (format-time-string "-%m %B" (encode-time 0 0 0 1 month year))))))
  (when (and day org-datetree-add-timestamp)
    (save-excursion
      (insert "\n")
      (org-indent-line)
      (org-insert-time-stamp
       (encode-time 0 0 0 day month year)
       nil
       (eq org-datetree-add-timestamp 'inactive))))
  (beginning-of-line))

(defun org-datetree-file-entry-under (txt d)
  "Insert a node TXT into the date tree under date D."
  (org-datetree-find-date-create d)
  (let ((level (org-get-valid-level (funcall outline-level) 1)))
    (org-end-of-subtree t t)
    (org-back-over-empty-lines)
    (org-paste-subtree level txt)))

(defun org-datetree-cleanup ()
  "Make sure all entries in the current tree are under the correct date.
It may be useful to restrict the buffer to the applicable portion
before running this command, even though the command tries to be smart."
  (interactive)
  (goto-char (point-min))
  (let ((dre (concat "\\<" org-deadline-string "\\>[ \t]*\\'"))
	(sre (concat "\\<" org-scheduled-string "\\>[ \t]*\\'")))
    (while (re-search-forward org-ts-regexp nil t)
      (catch 'next
	(let ((tmp (buffer-substring
		    (max (line-beginning-position)
			 (- (match-beginning 0) org-ds-keyword-length))
		    (match-beginning 0))))
	  (when (or (string-suffix-p "-" tmp)
		    (string-match dre tmp)
		    (string-match sre tmp))
	    (throw 'next nil))
	  (let* ((dct (decode-time (org-time-string-to-time (match-string 0))))
		 (date (list (nth 4 dct) (nth 3 dct) (nth 5 dct)))
		 (year (nth 2 date))
		 (month (car date))
		 (day (nth 1 date))
		 (pos (point))
		 (hdl-pos (progn (org-back-to-heading t) (point))))
	    (unless (org-up-heading-safe)
	      ;; No parent, we are not in a date tree.
	      (goto-char pos)
	      (throw 'next nil))
	    (unless (looking-at "\\*+[ \t]+[0-9]+-[0-1][0-9]-[0-3][0-9]")
	      ;; Parent looks wrong, we are not in a date tree.
	      (goto-char pos)
	      (throw 'next nil))
	    (when (looking-at (format "\\*+[ \t]+%d-%02d-%02d" year month day))
	      ;; At correct date already, do nothing.
	      (goto-char pos)
	      (throw 'next nil))
	    ;; OK, we need to refile this entry.
	    (goto-char hdl-pos)
	    (org-cut-subtree)
	    (save-excursion
	      (save-restriction
		(org-datetree-file-entry-under (current-kill 0) date)))))))))

(provide 'org-datetree)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-datetree.el ends here
