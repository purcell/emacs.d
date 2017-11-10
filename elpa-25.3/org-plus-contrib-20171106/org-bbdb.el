;;; org-bbdb.el --- Support for links to BBDB entries -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2017 Free Software Foundation, Inc.

;; Authors: Carsten Dominik <carsten at orgmode dot org>
;;       Thomas Baumann <thomas dot baumann at ch dot tum dot de>
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

;; This file implements links to BBDB database entries from within Org.
;; Org mode loads this module by default - if this is not what you want,
;; configure the variable `org-modules'.

;; It also implements an interface (based on Ivar Rummelhoff's
;; bbdb-anniv.el) for those Org users, who do not use the diary
;; but who do want to include the anniversaries stored in the BBDB
;; into the org-agenda.  If you already include the `diary' into the
;; agenda, you might want to prefer to include the anniversaries in
;; the diary using bbdb-anniv.el.
;;
;; Put the following in /somewhere/at/home/diary.org and make sure
;; that this file is in `org-agenda-files'.
;;
;; %%(org-bbdb-anniversaries)
;;
;; For example my diary.org looks like:
;; * Anniversaries
;; #+CATEGORY: Anniv
;; %%(org-bbdb-anniversaries)
;;
;;
;; To add an anniversary to a BBDB record, press `C-o' in the record.
;; You will be prompted for the field name, in this case it must be
;; "anniversary".  If this is the first time you are using this field,
;; you need to confirm that it should be created.
;;
;; The format of an anniversary field stored in BBDB is the following
;; (items in {} are optional):
;;
;;     YYYY-MM-DD{ CLASS-OR-FORMAT-STRING}
;;     {\nYYYY-MM-DD CLASS-OR-FORMAT-STRING}...
;;
;; CLASS-OR-FORMAT-STRING is one of two things:
;;
;;  - an identifier for a class of anniversaries (eg. birthday or
;;    wedding) from `org-bbdb-anniversary-format-alist' which then
;;    defines the format string for this class
;;  - the (format) string displayed in the diary.
;;
;; You can enter multiple anniversaries for a single BBDB record by
;; separating them with a newline character.  At the BBDB prompt for
;; the field value, type `C-q C-j' to enter a newline between two
;; anniversaries.
;;
;; If you omit the CLASS-OR-FORMAT-STRING entirely, it defaults to the
;; value of `org-bbdb-default-anniversary-format' ("birthday" by
;; default).
;;
;; The substitutions in the format string are (in order):
;;  - the name of the record containing this anniversary
;;  - the number of years
;;  - an ordinal suffix (st, nd, rd, th) for the year
;;
;; See the documentation of `org-bbdb-anniversary-format-alist' for
;; further options.
;;
;; Example
;;
;;       1973-06-22
;;       20??-??-?? wedding
;;       1998-03-12 %s created bbdb-anniv.el %d years ago
;;
;; From Org's agenda, you can use `C-c C-o' to jump to the BBDB
;; link from which the entry at point originates.
;;
;;; Code:

(require 'org)
(require 'cl-lib)

;; Declare external functions and variables

(declare-function bbdb "ext:bbdb-com" (string elidep))
(declare-function bbdb-company "ext:bbdb-com" (string elidep))
(declare-function bbdb-current-record "ext:bbdb-com"
		  (&optional planning-on-modifying))
(declare-function bbdb-name "ext:bbdb-com" (string elidep))
(declare-function bbdb-completing-read-record "ext:bbdb-com"
		  (prompt &optional omit-records))
(declare-function bbdb-record-field "ext:bbdb" (record field))
(declare-function bbdb-record-getprop "ext:bbdb" (record property))
(declare-function bbdb-record-name "ext:bbdb" (record))
(declare-function bbdb-records "ext:bbdb"
		  (&optional dont-check-disk already-in-db-buffer))
(declare-function bbdb-split "ext:bbdb" (string separators))
(declare-function bbdb-string-trim "ext:bbdb" (string))
(declare-function bbdb-record-get-field "ext:bbdb" (record field))
(declare-function bbdb-search-name "ext:bbdb-com" (regexp &optional layout))
(declare-function bbdb-search-organization "ext:bbdb-com" (regexp &optional layout))

;; `bbdb-record-note' was part of BBDB v3.x
(declare-function bbdb-record-note "ext:bbdb" (record label))
;; `bbdb-record-xfield' replaces it in recent BBDB v3.x+
(declare-function bbdb-record-xfield "ext:bbdb" (record label))

(declare-function calendar-leap-year-p "calendar" (year))
(declare-function diary-ordinal-suffix "diary-lib" (n))

(with-no-warnings (defvar date)) ;; unprefixed, from calendar.el

;; Customization

(defgroup org-bbdb-anniversaries nil
  "Customizations for including anniversaries from BBDB into Agenda."
  :group 'org-bbdb)

(defcustom org-bbdb-default-anniversary-format "birthday"
  "Default anniversary class."
  :type  'string
  :group 'org-bbdb-anniversaries
  :require 'bbdb)

(defcustom org-bbdb-general-anniversary-description-after 7
  "When to switch anniversary descriptions to a more general format.

Anniversary descriptions include the point in time, when the
anniversary appears.  This is, in its most general form, just the
date of the anniversary.  Or more specific terms, like \"today\",
\"tomorrow\" or \"in n days\" are used to describe the time span.

If the anniversary happens in less than that number of days, the
specific description is used.  Otherwise, the general one is
used."
  :group 'org-bbdb-anniversaries
  :version "26.1"
  :package-version '(Org . "9.1")
  :type  'integer
  :require 'bbdb
  :safe #'integerp)

(defcustom org-bbdb-anniversary-format-alist
  '(("birthday" .
     (lambda (name years suffix)
       (concat "Birthday: [[bbdb:" name "][" name " ("
    	       (format "%s" years)        ; handles numbers as well as strings
    	       suffix ")]]")))
    ("wedding" .
     (lambda (name years suffix)
       (concat "[[bbdb:" name "][" name "'s "
    	       (format "%s" years)
    	       suffix " wedding anniversary]]"))))
  "How different types of anniversaries should be formatted.
An alist of elements (STRING . FORMAT) where STRING is the name of an
anniversary class and format is either:
1) A format string with the following substitutions (in order):
    - the name of the record containing this anniversary
    - the number of years
    - an ordinal suffix (st, nd, rd, th) for the year

2) A function to be called with three arguments: NAME YEARS SUFFIX
   (string int string) returning a string for the diary or nil.

3) An Emacs Lisp form that should evaluate to a string (or nil) in the
   scope of variables NAME, YEARS and SUFFIX (among others)."
  :type '(alist :key-type   (string   :tag "Class")
		:value-type (function :tag "Function"))
  :group 'org-bbdb-anniversaries
  :require 'bbdb)

(defcustom org-bbdb-anniversary-field 'anniversary
  "The BBDB field which contains anniversaries.
The anniversaries are stored in the following format

YYYY-MM-DD Class-or-Format-String

where class is one of the customized classes for anniversaries;
birthday and wedding are predefined.  Format-String can take three
substitutions 1) the name of the record containing this
anniversary, 2) the number of years, and 3) an ordinal suffix for
the year.

Multiple anniversaries can be separated by \\n."
  :type    'symbol
  :group   'org-bbdb-anniversaries
  :require 'bbdb)

(defcustom org-bbdb-extract-date-fun 'org-bbdb-anniv-extract-date
  "How to retrieve `month date year' from the anniversary field.

Customize if you have already filled your BBDB with dates
different from YYYY-MM-DD.  The function must return a list (month
date year)."
  :type 'function
  :group 'org-bbdb-anniversaries
  :require 'bbdb)

;; Install the link type
(org-link-set-parameters "bbdb"
			 :follow #'org-bbdb-open
			 :export #'org-bbdb-export
			 :complete #'org-bbdb-complete-link
			 :store #'org-bbdb-store-link)

;; Implementation
(defun org-bbdb-store-link ()
  "Store a link to a BBDB database entry."
  (when (eq major-mode 'bbdb-mode)
    ;; This is BBDB, we make this link!
    (let* ((rec (bbdb-current-record))
           (name (bbdb-record-name rec))
	   (company (if (fboundp 'bbdb-record-getprop)
                        (bbdb-record-getprop rec 'company)
                      (car (bbdb-record-field rec 'organization))))
	   (link (concat "bbdb:" name)))
      (org-store-link-props :type "bbdb" :name name :company company
			    :link link :description name)
      link)))

(defun org-bbdb-export (path desc format)
  "Create the export version of a BBDB link specified by PATH or DESC.
If exporting to either HTML or LaTeX FORMAT the link will be
italicized, in all other cases it is left unchanged."
  (when (string= desc (format "bbdb:%s" path))
    (setq desc path))
  (cond
   ((eq format 'html) (format "<i>%s</i>" desc))
   ((eq format 'latex) (format "\\textit{%s}" desc))
   ((eq format 'odt)
    (format "<text:span text:style-name=\"Emphasis\">%s</text:span>" desc))
   (t desc)))

(defun org-bbdb-open (name)
  "Follow a BBDB link to NAME."
  (require 'bbdb-com)
  (let ((inhibit-redisplay (not debug-on-error)))
    (if (fboundp 'bbdb-name)
	(org-bbdb-open-old name)
      (org-bbdb-open-new name))))

(defun org-bbdb-open-old (name)
  (catch 'exit
    ;; Exact match on name
    (bbdb-name (concat "\\`" name "\\'") nil)
    (if (< 0 (buffer-size (get-buffer "*BBDB*"))) (throw 'exit nil))
    ;; Exact match on name
    (bbdb-company (concat "\\`" name "\\'") nil)
    (if (< 0 (buffer-size (get-buffer "*BBDB*"))) (throw 'exit nil))
    ;; Partial match on name
    (bbdb-name name nil)
    (if (< 0 (buffer-size (get-buffer "*BBDB*"))) (throw 'exit nil))
    ;; Partial match on company
    (bbdb-company name nil)
    (if (< 0 (buffer-size (get-buffer "*BBDB*"))) (throw 'exit nil))
    ;; General match including network address and notes
    (bbdb name nil)
    (when (= 0 (buffer-size (get-buffer "*BBDB*")))
      (delete-window (get-buffer-window "*BBDB*"))
      (error "No matching BBDB record"))))

(defun org-bbdb-open-new (name)
  (catch 'exit
    ;; Exact match on name
    (bbdb-search-name (concat "\\`" name "\\'") nil)
    (if (< 0 (buffer-size (get-buffer "*BBDB*"))) (throw 'exit nil))
    ;; Exact match on name
    (bbdb-search-organization (concat "\\`" name "\\'") nil)
    (if (< 0 (buffer-size (get-buffer "*BBDB*"))) (throw 'exit nil))
    ;; Partial match on name
    (bbdb-search-name name nil)
    (if (< 0 (buffer-size (get-buffer "*BBDB*"))) (throw 'exit nil))
    ;; Partial match on company
    (bbdb-search-organization name nil)
    (if (< 0 (buffer-size (get-buffer "*BBDB*"))) (throw 'exit nil))
    ;; General match including network address and notes
    (bbdb name nil)
    (when (= 0 (buffer-size (get-buffer "*BBDB*")))
      (delete-window (get-buffer-window "*BBDB*"))
      (error "No matching BBDB record"))))

(defun org-bbdb-anniv-extract-date (time-str)
  "Convert YYYY-MM-DD to (month date year).
Argument TIME-STR is the value retrieved from BBDB.  If YYYY- is omitted
it will be considered unknown."
  (pcase (org-split-string time-str "-")
    (`(,a ,b nil) (list (string-to-number a) (string-to-number b) nil))
    (`(,a ,b ,c) (list (string-to-number b)
		       (string-to-number c)
		       (string-to-number a)))))

(defun org-bbdb-anniv-split (str)
  "Split multiple entries in the BBDB anniversary field.
Argument STR is the anniversary field in BBDB."
  (let ((pos (string-match "[ \t]" str)))
    (if pos (list (substring str 0 pos)
		  (bbdb-string-trim (substring str pos)))
      (list str nil))))

(defvar org-bbdb-anniv-hash nil
  "A hash holding anniversaries extracted from BBDB.
The hash table is created on first use.")

(defvar org-bbdb-updated-p t
  "This is non-nil if BBDB has been updated since we last built the hash.")

(defun org-bbdb-make-anniv-hash ()
  "Create a hash with anniversaries extracted from BBDB, for fast access.
The anniversaries are assumed to be stored `org-bbdb-anniversary-field'."
  (let ((old-bbdb (fboundp 'bbdb-record-getprop))
	(record-func (if (fboundp 'bbdb-record-xfield)
			 'bbdb-record-xfield
		       'bbdb-record-note))
	split tmp annivs)
    (clrhash org-bbdb-anniv-hash)
    (dolist (rec (bbdb-records))
      (when (setq annivs (if old-bbdb
			     (bbdb-record-getprop
			      rec org-bbdb-anniversary-field)
			   (funcall record-func
				    rec org-bbdb-anniversary-field)))
        (setq annivs (if old-bbdb
			 (bbdb-split annivs "\n")
		       ;; parameter order is reversed in new bbdb
		       (bbdb-split "\n" annivs)))
        (while annivs
          (setq split (org-bbdb-anniv-split (pop annivs)))
	  (pcase-let ((`(,m ,d ,y) (funcall org-bbdb-extract-date-fun
					    (car split))))
	    (setq tmp (gethash (list m d) org-bbdb-anniv-hash))
            (puthash (list m d) (cons (list y
                                            (bbdb-record-name rec)
                                            (cadr split))
                                      tmp)
                     org-bbdb-anniv-hash))))))
  (setq org-bbdb-updated-p nil))

(defun org-bbdb-updated (_rec)
  "Record the fact that BBDB has been updated.
This is used by Org to re-create the anniversary hash table."
  (setq org-bbdb-updated-p t))

(add-hook 'bbdb-after-change-hook 'org-bbdb-updated)

;;;###autoload
(defun org-bbdb-anniversaries ()
  "Extract anniversaries from BBDB for display in the agenda."
  (require 'bbdb)
  (require 'diary-lib)
  (unless (hash-table-p org-bbdb-anniv-hash)
    (setq org-bbdb-anniv-hash
	  (make-hash-table :test 'equal :size 366)))

  (when (or org-bbdb-updated-p
            (= 0 (hash-table-count org-bbdb-anniv-hash)))
    (org-bbdb-make-anniv-hash))

  (let* ((m (car date))    ; month
         (d (nth 1 date))  ; day
         (y (nth 2 date))  ; year
         (annivs (gethash (list m d) org-bbdb-anniv-hash))
         (text ())
         rec recs)

    ;; we don't want to miss people born on Feb. 29th
    (when (and (= m 3) (= d 1)
               (not (null (gethash (list 2 29) org-bbdb-anniv-hash)))
               (not (calendar-leap-year-p y)))
      (setq recs (gethash (list 2 29) org-bbdb-anniv-hash))
      (while (setq rec (pop recs))
        (push rec annivs)))

    (when annivs
      (while (setq rec (pop annivs))
        (when rec
          (let* ((class (or (nth 2 rec)
                            org-bbdb-default-anniversary-format))
                 (form (or (cdr (assoc-string
				 class org-bbdb-anniversary-format-alist t))
                           class))	; (as format string)
                 (name (nth 1 rec))
                 (years (if (eq (car rec) nil)
                            "unknown"
                          (- y (car rec))))
                 (suffix (if (eq (car rec) nil)
                             ""
                           (diary-ordinal-suffix years)))
                 (tmp (cond
                       ((functionp form)
                        (funcall form name years suffix))
                       ((listp form) (eval form))
                       (t (format form name years suffix)))))
	    (org-add-props tmp nil 'org-bbdb-name name)
            (if text
                (setq text (append text (list tmp)))
              (setq text (list tmp)))))
        ))
    text))

;;; Return list of anniversaries for today and the next n-1 (default: n=7) days.
;;; This is meant to be used in an org file instead of org-bbdb-anniversaries:
;;;
;;; %%(org-bbdb-anniversaries-future)
;;;
;;; or
;;;
;;; %%(org-bbdb-anniversaries-future 3)
;;;
;;; to override the 7-day default.

(defun org-bbdb-date-list (d n)
  "Return a list of dates in (m d y) format from the given date D to n-1 days hence."
  (let ((abs (calendar-absolute-from-gregorian d)))
    (mapcar (lambda (i) (calendar-gregorian-from-absolute (+ abs i)))
	    (number-sequence 0 (1- n)))))

(defun org-bbdb-anniversary-description (agenda-date anniv-date)
  "Return a string used to incorporate into an agenda anniversary entry.
The calculation of the anniversary description string is based on
the difference between the anniversary date, given as ANNIV-DATE,
and the date on which the entry appears in the agenda, given as
AGENDA-DATE.  This makes it possible to have different entries
for the same event depending on if it occurs in the next few days
or far away in the future."
  (let ((delta (- (calendar-absolute-from-gregorian anniv-date)
                  (calendar-absolute-from-gregorian agenda-date))))

    (cond
     ((= delta 0) " -- today\\&")
     ((= delta 1) " -- tomorrow\\&")
     ((< delta org-bbdb-general-anniversary-description-after) (format " -- in %d days\\&" delta))
     ((pcase-let ((`(,month ,day ,year) anniv-date))
	(format " -- %d-%02d-%02d\\&" year month day))))))


(defun org-bbdb-anniversaries-future (&optional n)
  "Return list of anniversaries for today and the next n-1 days (default n=7)."
  (let ((n (or n 7)))
    (when (<= n 0)
      (error "The (optional) argument of `org-bbdb-anniversaries-future' \
must be positive"))
    (let (
	  ;; List of relevant dates.
	  (dates (org-bbdb-date-list date n))
	  ;; Function to annotate text of each element of l with the
	  ;; anniversary date d.
	  (annotate-descriptions
	   (lambda (agenda-date d l)
	     (mapcar (lambda (x)
		       ;; The assumption here is that x is a bbdb link
		       ;; of the form [[bbdb:name][description]].
		       ;; This function rather arbitrarily modifies
		       ;; the description by adding the date to it in
		       ;; a fixed format.
		       (let ((desc (org-bbdb-anniversary-description
				    agenda-date d)))
			 (string-match "]]" x)
			 (replace-match desc nil nil x)))
		     l))))
      ;; Map a function that generates anniversaries for each date
      ;; over the dates and nconc the results into a single list. When
      ;; it is no longer necessary to support older versions of Emacs,
      ;; this can be done with a cl-mapcan; for now, we use the (apply
      ;; #'nconc ...) method for compatibility.
      (apply #'nconc
	     (mapcar
	      (lambda (d)
		(let ((agenda-date date)
		      (date d))
		  ;; Rebind 'date' so that org-bbdb-anniversaries will
		  ;; be fooled into giving us the list for the given
		  ;; date and then annotate the descriptions for that
		  ;; date.
		  (funcall annotate-descriptions agenda-date d (org-bbdb-anniversaries))))
	      dates)))))

(defun org-bbdb-complete-link ()
  "Read a bbdb link with name completion."
  (require 'bbdb-com)
  (let ((rec (bbdb-completing-read-record "Name: ")))
    (concat "bbdb:"
	    (bbdb-record-name (if (listp rec)
				  (car rec)
				rec)))))

(defun org-bbdb-anniv-export-ical ()
  "Extract anniversaries from BBDB and convert them to icalendar format."
  (require 'bbdb)
  (require 'diary-lib)
  (unless (hash-table-p org-bbdb-anniv-hash)
    (setq org-bbdb-anniv-hash
	  (make-hash-table :test 'equal :size 366)))
  (when (or org-bbdb-updated-p
	    (= 0 (hash-table-count org-bbdb-anniv-hash)))
    (org-bbdb-make-anniv-hash))
  (maphash 'org-bbdb-format-vevent org-bbdb-anniv-hash))

(defun org-bbdb-format-vevent (key recs)
  (let (rec categ)
    (while (setq rec (pop recs))
      (setq categ (or (nth 2 rec) org-bbdb-default-anniversary-format))
      (princ (format "BEGIN:VEVENT
UID: ANNIV-%4i%02i%02i-%s
DTSTART:%4i%02i%02i
SUMMARY:%s
DESCRIPTION:%s
CATEGORIES:%s
RRULE:FREQ=YEARLY
END:VEVENT\n"
		     (nth 0 rec) (nth 0 key) (nth 1 key)
		     (mapconcat 'identity
				(org-split-string (nth 1 rec) "[^a-zA-Z0-90]+")
				"-")
		     (nth 0 rec) (nth 0 key) (nth 1 key)
		     (nth 1 rec)
		     (concat (capitalize categ) " " (nth 1 rec))
		     categ)))))

(provide 'org-bbdb)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-bbdb.el ends here
