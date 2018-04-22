;;; org-faces.el --- Face definitions -*- lexical-binding: t; -*-

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

;; This file contains the face definitions for Org.

;;; Code:

(defgroup org-faces nil
  "Faces in Org mode."
  :tag "Org Faces"
  :group 'org-appearance)

(defface org-default '((t :inherit default))
  "Face used for default text."
  :group 'org-faces)

(defface org-hide
  '((((background light)) (:foreground "white"))
    (((background dark)) (:foreground "black")))
  "Face used to hide leading stars in headlines.
The foreground color of this face should be equal to the background
color of the frame."
  :group 'org-faces)

(defface org-level-1 '((t :inherit outline-1))
  "Face used for level 1 headlines."
  :group 'org-faces)

(defface org-level-2 '((t :inherit outline-2))
  "Face used for level 2 headlines."
  :group 'org-faces)

(defface org-level-3 '((t :inherit outline-3))
  "Face used for level 3 headlines."
  :group 'org-faces)

(defface org-level-4 '((t :inherit outline-4))
  "Face used for level 4 headlines."
  :group 'org-faces)

(defface org-level-5 '((t :inherit outline-5))
  "Face used for level 5 headlines."
  :group 'org-faces)

(defface org-level-6 '((t :inherit outline-6))
  "Face used for level 6 headlines."
  :group 'org-faces)

(defface org-level-7 '((t :inherit outline-7))
  "Face used for level 7 headlines."
  :group 'org-faces)

(defface org-level-8 '((t :inherit outline-8))
  "Face used for level 8 headlines."
  :group 'org-faces)

(defface org-special-keyword '((t :inherit font-lock-keyword-face))
  "Face used for special keywords."
  :group 'org-faces)

(defface org-drawer	   ;Copied from `font-lock-function-name-face'
  '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
    (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
    (((class color) (min-colors 8)) (:foreground "blue" :bold t))
    (t (:bold t)))
  "Face used for drawers."
  :group 'org-faces)

(defface org-property-value nil
  "Face used for the value of a property."
  :group 'org-faces)

(defface org-column
  '((((class color) (min-colors 16) (background light))
     (:background "grey90" :weight normal :slant normal :strike-through nil
		  :underline nil))
    (((class color) (min-colors 16) (background dark))
     (:background "grey30" :weight normal :slant normal :strike-through nil
		  :underline nil))
    (((class color) (min-colors 8))
     (:background "cyan" :foreground "black"
		  :weight normal :slant normal :strike-through nil
		  :underline nil))
    (t (:inverse-video t)))
  "Face for column display of entry properties.
This is actually only part of the face definition for the text in column view.
The following faces apply, with this priority.

1. The color of the reference face.  This is normally the level fact that
   is used in the outline.  In agenda-mode, it will be the face of the
   first character in the line.  The color is explicitly retained to
   make sure that the column line still looks a bit like the structure
   line it is masking.

2. The `org-column' face.

3. The remaining properties of the reference face.

Since column view works by putting overlays with a display property
over individual characters in the buffer, the face of the underlining
character (this might for example be the a TODO keyword) might still
shine through in some properties.  So when your column view looks
funny, with \"random\" colors, weight, strike-through, try to explicitly
set the properties in the `org-column' face.  For example, set
:underline to nil, or the :slant to `normal'."
  :group 'org-faces)

(defface org-column-title
  '((((class color) (min-colors 16) (background light))
     (:background "grey90" :underline t :weight bold))
    (((class color) (min-colors 16) (background dark))
     (:background "grey30" :underline t :weight bold))
    (((class color) (min-colors 8))
     (:background "cyan" :foreground "black" :underline t :weight bold))
    (t (:inverse-video t)))
  "Face for column display of entry properties."
  :group 'org-faces)

(defface org-agenda-column-dateline '((t :inherit org-column))
  "Face used in agenda column view for datelines with summaries."
  :group 'org-faces)

(defface org-warning '((t :inherit font-lock-warning-face))
  "Face for deadlines and TODO keywords."
  :group 'org-faces)

(defface org-archived '((t :inherit shadow))
  "Face for headline with the ARCHIVE tag."
  :group 'org-faces)

(defface org-link '((t :inherit link))
  "Face for links."
  :group 'org-faces)

(defface org-footnote
  '((((class color) (background light)) (:foreground "Purple" :underline t))
    (((class color) (background dark)) (:foreground "Cyan" :underline t))
    (t (:underline t)))
  "Face for footnotes."
  :group 'org-faces)

(defface org-ellipsis
  '((((class color) (background light)) (:foreground "DarkGoldenrod" :underline t))
    (((class color) (background dark)) (:foreground "LightGoldenrod" :underline t))
    (t (:strike-through t)))
  "Face for the ellipsis in folded text."
  :group 'org-faces)

(defface org-target
  '((((class color) (background light)) (:underline t))
    (((class color) (background dark)) (:underline t))
    (t (:underline t)))
  "Face for link targets."
  :group 'org-faces)

(defface org-date
  '((((class color) (background light)) (:foreground "Purple" :underline t))
    (((class color) (background dark)) (:foreground "Cyan" :underline t))
    (t (:underline t)))
  "Face for date/time stamps."
  :group 'org-faces)

(defface org-date-selected
  '((((class color) (min-colors 16) (background light)) (:foreground "Red1" :inverse-video t))
    (((class color) (min-colors 16) (background dark))  (:foreground "Pink" :inverse-video t))
    (((class color) (min-colors 8)  (background light)) (:foreground "red"  :inverse-video t))
    (((class color) (min-colors 8)  (background dark))  (:foreground "red"  :inverse-video t))
    (t (:inverse-video t)))
  "Face for highlighting the calendar day when using `org-read-date'.
Using a bold face here might cause discrepancies while displaying the
calendar."
  :group 'org-faces)

(defface org-sexp-date
  '((((class color) (background light)) (:foreground "Purple"))
    (((class color) (background dark)) (:foreground "Cyan"))
    (t (:underline t)))
  "Face for diary-like sexp date specifications."
  :group 'org-faces)

(defface org-tag '((t (:bold t)))
  "Default face for tags.
Note that the variable `org-tag-faces' can be used to overrule this face for
specific tags."
  :group 'org-faces)

(defface org-list-dt '((t (:bold t)))
  "Default face for definition terms in lists."
  :group 'org-faces)

(defface org-todo		 ;Copied from `font-lock-warning-face'
  '((((class color) (min-colors 16) (background light)) (:foreground "Red1" :bold t))
    (((class color) (min-colors 16) (background dark))  (:foreground "Pink" :bold t))
    (((class color) (min-colors 8)  (background light)) (:foreground "red"  :bold t))
    (((class color) (min-colors 8)  (background dark))  (:foreground "red"  :bold t))
    (t (:inverse-video t :bold t)))
  "Face for TODO keywords."
  :group 'org-faces)

(defface org-done		    ;Copied from `font-lock-type-face'
  '((((class color) (min-colors 16) (background light)) (:foreground "ForestGreen" :bold t))
    (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen" :bold t))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:bold t)))
  "Face used for todo keywords that indicate DONE items."
  :group 'org-faces)

(defface org-agenda-done	    ;Copied from `font-lock-type-face'
  '((((class color) (min-colors 16) (background light)) (:foreground "ForestGreen"))
    (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:bold nil)))
  "Face used in agenda, to indicate lines switched to DONE.
This face is used to de-emphasize items that where brightly colored in the
agenda because they were things to do, or overdue.  The DONE state itself
is of course immediately visible, but for example a passed deadline is
\(by default) very bright read.  This face could be simply the default face
of the frame, for example."
  :group 'org-faces)

(defface org-headline-done	  ;Copied from `font-lock-string-face'
  '((((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
    (((class color) (min-colors 8)  (background light)) (:bold nil)))
  "Face used to indicate that a headline is DONE.
This face is only used if `org-fontify-done-headline' is set.  If applies
to the part of the headline after the DONE keyword."
  :group 'org-faces)

(defcustom org-faces-easy-properties
  '((todo . :foreground) (tag . :foreground) (priority . :foreground))
  "The property changes by easy faces.
This is an alist, the keys show the area of application, the values
can be `:foreground' or `:background'.  A color string for special
keywords will then be interpreted as either foreground or background
color."
  :group 'org-faces
  :group 'org-todo
  :version "24.1"
  :type '(repeat
	  (cons (choice (const todo) (const tag) (const priority))
		(choice (const :foreground) (const :background)))))

(defcustom org-todo-keyword-faces nil
  "Faces for specific TODO keywords.
This is a list of cons cells, with TODO keywords in the car
and faces in the cdr.  The face can be a symbol, a color
as a string (in which case the rest is inherited from the `org-todo' face),
or a property list of attributes, like
   (:foreground \"blue\" :weight bold :underline t).
If it is a color string, the variable `org-faces-easy-properties'
determines if it is a foreground or a background color."
  :group 'org-faces
  :group 'org-todo
  :type '(repeat
	  (cons
	   (string :tag "Keyword")
	   (choice :tag "Face   "
		   (string :tag "Color")
		   (sexp :tag "Face")))))

(defface org-priority '((t :inherit font-lock-keyword-face))
  "Face used for priority cookies."
  :group 'org-faces)

(defcustom org-priority-faces nil
  "Faces for specific Priorities.
This is a list of cons cells, with priority character in the car
and faces in the cdr.  The face can be a symbol, a color
as a string, or a property list of attributes, like
    (:foreground \"blue\" :weight bold :underline t).
If it is a color string, the variable `org-faces-easy-properties'
determines if it is a foreground or a background color."
  :group 'org-faces
  :group 'org-todo
  :type '(repeat
	  (cons
	   (character :tag "Priority")
	   (choice    :tag "Face    "
		      (string :tag "Color")
		      (sexp :tag "Face")))))

(defvar org-tags-special-faces-re nil)
(defun org-set-tag-faces (var value)
  (set var value)
  (if (not value)
      (setq org-tags-special-faces-re nil)
    (setq org-tags-special-faces-re
	  (concat ":\\(" (mapconcat 'car value "\\|") "\\):"))))

(defface org-checkbox '((t :inherit bold))
  "Face for checkboxes."
  :group 'org-faces)

(defface org-checkbox-statistics-todo '((t (:inherit org-todo)))
  "Face used for unfinished checkbox statistics."
  :group 'org-faces)

(defface org-checkbox-statistics-done '((t (:inherit org-done)))
  "Face used for finished checkbox statistics."
  :group 'org-faces)

(defcustom org-tag-faces nil
  "Faces for specific tags.
This is a list of cons cells, with tags in the car and faces in the cdr.
The face can be a symbol, a foreground color (in which case the rest is
inherited from the `org-tag' face) or a property list of attributes,
like (:foreground \"blue\" :weight bold :underline t).
If you set this variable through customize, it will immediately be effective
in new buffers and in modified lines.
If you set it with Lisp, a restart of Emacs is required to activate the
changes."
  :group 'org-faces
  :group 'org-tags
  :set 'org-set-tag-faces
  :type '(repeat
	  (cons
	   (string :tag "Tag ")
	   (choice :tag "Face"
		   (string :tag "Foreground color")
		   (sexp :tag "Face")))))

(defface org-table	   ;Copied from `font-lock-function-name-face'
  '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
    (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
    (((class color) (min-colors 8)  (background light)) (:foreground "blue"))
    (((class color) (min-colors 8)  (background dark))))
  "Face used for tables."
  :group 'org-faces)

(defface org-formula
  '((((class color) (min-colors 88) (background light)) (:foreground "Firebrick"))
    (((class color) (min-colors 88) (background dark)) (:foreground "chocolate1"))
    (((class color) (min-colors 8)  (background light)) (:foreground "red"))
    (((class color) (min-colors 8)  (background dark)) (:foreground "red"))
    (t (:bold t :italic t)))
  "Face for formulas."
  :group 'org-faces)

(defface org-code '((t :inherit shadow))
  "Face for fixed-width text like code snippets."
  :group 'org-faces
  :version "22.1")

(defface org-meta-line '((t :inherit font-lock-comment-face))
  "Face for meta lines starting with \"#+\"."
  :group 'org-faces
  :version "22.1")

(defface org-document-title
  '((((class color) (background light)) (:foreground "midnight blue" :weight bold))
    (((class color) (background dark)) (:foreground "pale turquoise" :weight bold))
    (t (:weight bold)))
  "Face for document title, i.e. that which follows the #+TITLE: keyword."
  :group 'org-faces)

(defface org-document-info
  '((((class color) (background light)) (:foreground "midnight blue"))
    (((class color) (background dark)) (:foreground "pale turquoise"))
    (t nil))
  "Face for document date, author and email; i.e. that which
follows a #+DATE:, #+AUTHOR: or #+EMAIL: keyword."
  :group 'org-faces)

(defface org-document-info-keyword '((t :inherit shadow))
  "Face for #+TITLE:, #+AUTHOR:, #+EMAIL: and #+DATE: keywords."
  :group 'org-faces)

(defface org-block '((t :inherit shadow))
  "Face text in #+begin ... #+end blocks.
For source-blocks `org-src-block-faces' takes precedence.
See also `org-fontify-quote-and-verse-blocks'."
  :group 'org-faces
  :version "26.1")

(defface org-block-begin-line '((t (:inherit org-meta-line)))
  "Face used for the line delimiting the begin of source blocks."
  :group 'org-faces)

(defface org-block-end-line '((t (:inherit org-block-begin-line)))
  "Face used for the line delimiting the end of source blocks."
  :group 'org-faces)

(defface org-verbatim '((t (:inherit shadow)))
  "Face for fixed-with text like code snippets"
  :group 'org-faces
  :version "22.1")

(defface org-quote '((t (:inherit org-block)))
  "Face for #+BEGIN_QUOTE ... #+END_QUOTE blocks."
  :group 'org-faces)

(defface org-verse '((t (:inherit org-block)))
  "Face for #+BEGIN_VERSE ... #+END_VERSE blocks."
  :group 'org-faces)

(defcustom org-fontify-quote-and-verse-blocks nil
  "Non-nil means, add a special face to #+begin_quote and #+begin_verse block.
When nil, format these as normal Org.  This is the default, because the
content of these blocks will still be treated as Org syntax."
  :group 'org-faces
  :version "24.1"
  :type 'boolean)

(defface org-clock-overlay	    ;Copied from `secondary-selection'
  '((((class color) (min-colors 88) (background light))
     (:background "LightGray" :foreground "black"))
    (((class color) (min-colors 88) (background dark))
     (:background "SkyBlue4" :foreground "white"))
    (((class color) (min-colors 16) (background light))
     (:background "gray" :foreground "black"))
    (((class color) (min-colors 16) (background dark))
     (:background "SkyBlue4" :foreground "white"))
    (((class color) (min-colors 8))
     (:background "cyan" :foreground "black"))
    (t (:inverse-video t)))
  "Basic face for displaying the secondary selection."
  :group 'org-faces)

(defface org-agenda-structure ;Copied from `font-lock-function-name-face'
  '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
    (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
    (((class color) (min-colors 8)) (:foreground "blue" :bold t))
    (t (:bold t)))
  "Face used in agenda for captions and dates."
  :group 'org-faces)

(defface org-agenda-date '((t (:inherit org-agenda-structure)))
  "Face used in agenda for normal days."
  :group 'org-faces)

(defface org-agenda-date-today
  '((t (:inherit org-agenda-date :weight bold :italic t)))
  "Face used in agenda for today."
  :group 'org-faces)

(defface org-agenda-clocking '((t (:inherit secondary-selection)))
  "Face marking the current clock item in the agenda."
  :group 'org-faces)

(defface org-agenda-date-weekend '((t (:inherit org-agenda-date :weight bold)))
  "Face used in agenda for weekend days.

See the variable `org-agenda-weekend-days' for a definition of
which days belong to the weekend."
  :group 'org-faces)

(defface org-scheduled
  '((((class color) (min-colors 88) (background light)) (:foreground "DarkGreen"))
    (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:bold t :italic t)))
  "Face for items scheduled for a certain day."
  :group 'org-faces)

(defface org-scheduled-today
  '((((class color) (min-colors 88) (background light)) (:foreground "DarkGreen"))
    (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:bold t :italic t)))
  "Face for items scheduled for a certain day."
  :group 'org-faces)

(defface org-agenda-dimmed-todo-face
  '((((background light)) (:foreground "grey50"))
    (((background dark)) (:foreground "grey50")))
  "Face used to dim blocked tasks in the agenda."
  :group 'org-faces)

(defface org-scheduled-previously
  '((((class color) (min-colors 88) (background light)) (:foreground "Firebrick"))
    (((class color) (min-colors 88) (background dark)) (:foreground "chocolate1"))
    (((class color) (min-colors 8)  (background light)) (:foreground "red"))
    (((class color) (min-colors 8)  (background dark)) (:foreground "red" :bold t))
    (t (:bold t)))
  "Face for items scheduled previously, and not yet done."
  :group 'org-faces)

(defface org-upcoming-deadline
  '((((class color) (min-colors 88) (background light)) (:foreground "Firebrick"))
    (((class color) (min-colors 88) (background dark)) (:foreground "chocolate1"))
    (((class color) (min-colors 8)  (background light)) (:foreground "red"))
    (((class color) (min-colors 8)  (background dark)) (:foreground "red" :bold t))
    (t (:bold t)))
  "Face for items scheduled previously, and not yet done."
  :group 'org-faces)

(defcustom org-agenda-deadline-faces
  '((1.0 . org-warning)
    (0.5 . org-upcoming-deadline)
    (0.0 . default))
  "Faces for showing deadlines in the agenda.
This is a list of cons cells.  The cdr of each cell is a face to be used,
and it can also just be like \\='(:foreground \"yellow\").
Each car is a fraction of the head-warning time that must have passed for
this the face in the cdr to be used for display.  The numbers must be
given in descending order.  The head-warning time is normally taken
from `org-deadline-warning-days', but can also be specified in the deadline
timestamp itself, like this:

   DEADLINE: <2007-08-13 Mon -8d>

You may use d for days, w for weeks, m for months and y for years.  Months
and years will only be treated in an approximate fashion (30.4 days for a
month and 365.24 days for a year)."
  :group 'org-faces
  :group 'org-agenda-daily/weekly
  :type '(repeat
	  (cons
	   (number :tag "Fraction of head-warning time passed")
	   (sexp :tag "Face"))))

(defface org-agenda-restriction-lock
  '((((class color) (min-colors 88) (background light)) (:background "#eeeeee"))
    (((class color) (min-colors 88) (background dark))  (:background "#1C1C1C"))
    (((class color) (min-colors 16) (background light)) (:background "#eeeeee"))
    (((class color) (min-colors 16) (background dark))  (:background "#1C1C1C"))
    (((class color) (min-colors 8)) (:background "cyan" :foreground "black"))
    (t (:inverse-video t)))
  "Face for showing the agenda restriction lock."
  :group 'org-faces)

(defface org-agenda-filter-tags '((t :inherit mode-line))
  "Face for tag(s) in the mode-line when filtering the agenda."
  :group 'org-faces)

(defface org-agenda-filter-regexp '((t :inherit mode-line))
  "Face for regexp(s) in the mode-line when filtering the agenda."
  :group 'org-faces)

(defface org-agenda-filter-category '((t :inherit mode-line))
  "Face for categories in the mode-line when filtering the agenda."
  :group 'org-faces)

(defface org-agenda-filter-effort '((t :inherit mode-line))
  "Face for effort in the mode-line when filtering the agenda."
  :group 'org-faces)

(defface org-time-grid	   ;Copied from `font-lock-variable-name-face'
  '((((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightGoldenrod"))
    (((class color) (min-colors 8)) (:foreground "yellow" :weight light)))
  "Face used for time grids."
  :group 'org-faces)

(defface org-agenda-current-time '((t (:inherit org-time-grid)))
  "Face used to show the current time in the time grid."
  :group 'org-faces)

(defface org-agenda-diary '((t :inherit default))
  "Face used for agenda entries that come from the Emacs diary."
  :group 'org-faces)

(defface org-agenda-calendar-event '((t :inherit default))
  "Face used to show events and appointments in the agenda."
  :group 'org-faces)

(defface org-agenda-calendar-sexp '((t :inherit default))
  "Face used to show events computed from a S-expression."
  :group 'org-faces)

(defconst org-level-faces
  '(org-level-1 org-level-2 org-level-3 org-level-4
		org-level-5 org-level-6 org-level-7 org-level-8))

(defcustom org-n-level-faces (length org-level-faces)
  "The number of different faces to be used for headlines.
Org mode defines 8 different headline faces, so this can be at most 8.
If it is less than 8, the level-1 face gets re-used for level N+1 etc."
  :type 'integer
  :group 'org-faces)

(defcustom org-cycle-level-faces t
  "Non-nil means level styles cycle after level `org-n-level-faces'.
Then so level org-n-level-faces+1 is styled like level 1.
If nil, then all levels >=org-n-level-faces are styled like
level org-n-level-faces"
  :group 'org-appearance
  :group 'org-faces
  :version "24.1"
  :type 'boolean)

(defface org-latex-and-related
  (let ((font (cond ((assq :inherit custom-face-attributes)
		     '(:inherit underline))
		    (t '(:underline t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "SaddleBrown"))
      (((class color) (background dark))
       (:foreground "burlywood"))
      (t (,@font))))
  "Face used to highlight LaTeX data, entities and sub/superscript."
  :group 'org-faces
  :version "24.4"
  :package-version '(Org . "8.0"))

(defface org-macro '((t :inherit org-latex-and-related))
  "Face for macros."
  :group 'org-faces
  :version "24.4"
  :package-version '(Org . "8.0"))

(defface org-tag-group '((t :inherit org-tag))
  "Face for group tags."
  :group 'org-faces
  :version "24.4"
  :package-version '(Org . "8.0"))

(defface org-mode-line-clock '((t (:inherit mode-line)))
  "Face used for clock display in mode line."
  :group 'org-faces)

(defface org-mode-line-clock-overrun
  '((t (:inherit mode-line :background "red")))
  "Face used for clock display for overrun tasks in mode line."
  :group 'org-faces)

(provide 'org-faces)

;;; org-faces.el ends here
