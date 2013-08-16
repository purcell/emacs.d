;; -*- coding: utf-8-unix -*-
;; org-drill.el - Self-testing using spaced repetition
;;
;; Author: Paul Sexton <eeeickythump@gmail.com>
;; Version: 2.3.5
;; Repository at http://bitbucket.org/eeeickythump/org-drill/
;;
;; This file is not part of GNU Emacs.
;;
;; Synopsis
;; ========
;;
;; Uses the SuperMemo spaced repetition algorithms to conduct interactive
;; "drill sessions", where the material to be remembered is presented to the
;; student in random order. The student rates his or her recall of each item,
;; and this information is used to schedule the item for later revision.
;;
;; Each drill session can be restricted to topics in the current buffer
;; (default), one or several files, all agenda files, or a subtree. A single
;; topic can also be drilled.
;;
;; Different "card types" can be defined, which present their information to
;; the student in different ways.
;;
;; See the file README.org in the repository for more detailed documentation.

(eval-when-compile (require 'cl))
(eval-when-compile (require 'hi-lock))
(require 'org)
(require 'org-id)
(require 'org-learn)


(defgroup org-drill nil
  "Options concerning interactive drill sessions in Org mode (org-drill)."
  :tag "Org-Drill"
  :group 'org-link)


(defcustom org-drill-question-tag
  "drill"
  "Tag which topics must possess in order to be identified as review topics
by `org-drill'."
  :group 'org-drill
  :type 'string)


(defcustom org-drill-maximum-items-per-session
  30
  "Each drill session will present at most this many topics for review.
Nil means unlimited."
  :group 'org-drill
  :type '(choice integer (const nil)))


(defcustom org-drill-maximum-duration
  20
  "Maximum duration of a drill session, in minutes.
Nil means unlimited."
  :group 'org-drill
  :type '(choice integer (const nil)))


(defcustom org-drill-failure-quality
  2
  "If the quality of recall for an item is this number or lower,
it is regarded as an unambiguous failure, and the repetition
interval for the card is reset to 0 days.  If the quality is higher
than this number, it is regarded as successfully recalled, but the
time interval to the next repetition will be lowered if the quality
was near to a fail.

By default this is 2, for SuperMemo-like behaviour. For
Mnemosyne-like behaviour, set it to 1.  Other values are not
really sensible."
  :group 'org-drill
  :type '(choice (const 2) (const 1)))


(defcustom org-drill-forgetting-index
  10
  "What percentage of items do you consider it is 'acceptable' to
forget each drill session? The default is 10%. A warning message
is displayed at the end of the session if the percentage forgotten
climbs above this number."
  :group 'org-drill
  :type 'integer)


(defcustom org-drill-leech-failure-threshold
  15
  "If an item is forgotten more than this many times, it is tagged
as a 'leech' item."
  :group 'org-drill
  :type '(choice integer (const nil)))


(defcustom org-drill-leech-method
  'skip
  "How should 'leech items' be handled during drill sessions?
Possible values:
- nil :: Leech items are treated the same as normal items.
- skip :: Leech items are not included in drill sessions.
- warn :: Leech items are still included in drill sessions,
  but a warning message is printed when each leech item is
  presented."
  :group 'org-drill
  :type '(choice (const warn) (const skip) (const nil)))


(defface org-drill-visible-cloze-face
  '((t (:foreground "darkseagreen")))
  "The face used to hide the contents of cloze phrases."
  :group 'org-drill)


(defface org-drill-visible-cloze-hint-face
  '((t (:foreground "dark slate blue")))
  "The face used to hide the contents of cloze phrases."
  :group 'org-drill)


(defface org-drill-hidden-cloze-face
  '((t (:foreground "deep sky blue" :background "blue")))
  "The face used to hide the contents of cloze phrases."
  :group 'org-drill)


(defcustom org-drill-use-visible-cloze-face-p
  nil
  "Use a special face to highlight cloze-deleted text in org mode
buffers?"
  :group 'org-drill
  :type 'boolean)


(defcustom org-drill-hide-item-headings-p
  nil
  "Conceal the contents of the main heading of each item during drill
sessions? You may want to enable this behaviour if item headings or tags
contain information that could 'give away' the answer."
  :group 'org-drill
  :type 'boolean)


(defcustom org-drill-new-count-color
  "royal blue"
  "Foreground colour used to display the count of remaining new items
during a drill session."
  :group 'org-drill
  :type 'color)

(defcustom org-drill-mature-count-color
  "green"
  "Foreground colour used to display the count of remaining mature items
during a drill session. Mature items are due for review, but are not new."
  :group 'org-drill
  :type 'color)

(defcustom org-drill-failed-count-color
  "red"
  "Foreground colour used to display the count of remaining failed items
during a drill session."
  :group 'org-drill
  :type 'color)

(defcustom org-drill-done-count-color
  "sienna"
  "Foreground colour used to display the count of reviewed items
during a drill session."
  :group 'org-drill
  :type 'color)


(setplist 'org-drill-cloze-overlay-defaults
          '(display "[...]"
                    face org-drill-hidden-cloze-face
                    window t))

(setplist 'org-drill-hidden-text-overlay
          '(invisible t))

(setplist 'org-drill-replaced-text-overlay
          '(display "Replaced text"
                    face default
                    window t))


(defvar org-drill-cloze-regexp
  ;; ver 1   "[^][]\\(\\[[^][][^]]*\\]\\)"
  ;; ver 2   "\\(\\[.*?\\]\\|^[^[[:cntrl:]]*?\\]\\|\\[.*?$\\)"
  ;; ver 3!  "\\(\\[.*?\\]\\|\\[.*?[[:cntrl:]]+.*?\\]\\)"
  "\\(\\[[[:cntrl:][:graph:][:space:]]*?\\)\\(\\||.+?\\)\\(\\]\\)")


(defvar org-drill-cloze-keywords
  `((,org-drill-cloze-regexp
     (1 'org-drill-visible-cloze-face nil)
     (2 'org-drill-visible-cloze-hint-face t)
     (3 'org-drill-visible-cloze-face nil)
     )))


(defcustom org-drill-card-type-alist
  '((nil . org-drill-present-simple-card)
    ("simple" . org-drill-present-simple-card)
    ("twosided" . org-drill-present-two-sided-card)
    ("multisided" . org-drill-present-multi-sided-card)
    ("hide1cloze" . org-drill-present-multicloze-hide1)
    ("hide2cloze" . org-drill-present-multicloze-hide2)
    ("show1cloze" . org-drill-present-multicloze-show1)
    ("show2cloze" . org-drill-present-multicloze-show2)
    ("multicloze" . org-drill-present-multicloze-hide1)
    ("hidefirst" . org-drill-present-multicloze-hide-first)
    ("hidelast" . org-drill-present-multicloze-hide-last)
    ("hide1_firstmore" . org-drill-present-multicloze-hide1-firstmore)
    ("show1_lastmore" . org-drill-present-multicloze-show1-lastmore)
    ("show1_firstless" . org-drill-present-multicloze-show1-firstless)
    ("conjugate" org-drill-present-verb-conjugation
     org-drill-show-answer-verb-conjugation)
    ("spanish_verb" . org-drill-present-spanish-verb)
    ("translate_number" org-drill-present-translate-number
     org-drill-show-answer-translate-number))
  "Alist associating card types with presentation functions. Each entry in the
alist takes one of two forms:
1. (CARDTYPE . QUESTION-FN), where CARDTYPE is a string or nil (for default),
   and QUESTION-FN is a function which takes no arguments and returns a boolean
   value.
2. (CARDTYPE QUESTION-FN ANSWER-FN), where ANSWER-FN is a function that takes
   one argument -- the argument is a function that itself takes no arguments.
   ANSWER-FN is called with the point on the active item's
   heading, just prior to displaying the item's 'answer'. It can therefore be
   used to modify the appearance of the answer. ANSWER-FN must call its argument
   before returning. (Its argument is a function that prompts the user and
   performs rescheduling)."
  :group 'org-drill
  :type '(alist :key-type (choice string (const nil)) :value-type function))


(defcustom org-drill-scope
  'file
  "The scope in which to search for drill items when conducting a
drill session. This can be any of:

file                 The current buffer, respecting the restriction if any.
                     This is the default.
tree                 The subtree started with the entry at point
file-no-restriction  The current buffer, without restriction
file-with-archives   The current buffer, and any archives associated with it.
agenda               All agenda files
agenda-with-archives All agenda files with any archive files associated
                     with them.
directory            All files with the extension '.org' in the same
                     directory as the current file (includes the current
                     file if it is an .org file.)
 (FILE1 FILE2 ...)   If this is a list, all files in the list will be scanned.
"
  ;; Note -- meanings differ slightly from the argument to org-map-entries:
  ;; 'file' means current file/buffer, respecting any restriction
  ;; 'file-no-restriction' means current file/buffer, ignoring restrictions
  ;; 'directory' means all *.org files in current directory
  :group 'org-drill
  :type '(choice (const file) (const tree) (const file-no-restriction)
                 (const file-with-archives) (const agenda)
                 (const agenda-with-archives) (const directory)
                 list))


(defcustom org-drill-save-buffers-after-drill-sessions-p
  t
  "If non-nil, prompt to save all modified buffers after a drill session
finishes."
  :group 'org-drill
  :type 'boolean)


(defcustom org-drill-spaced-repetition-algorithm
  'sm5
  "Which SuperMemo spaced repetition algorithm to use for scheduling items.
Available choices are:
- SM2 :: the SM2 algorithm, used in SuperMemo 2.0
- SM5 :: the SM5 algorithm, used in SuperMemo 5.0
- Simple8 :: a modified version of the SM8 algorithm. SM8 is used in
  SuperMemo 98. The version implemented here is simplified in that while it
  'learns' the difficulty of each item using quality grades and number of
  failures, it does not modify the matrix of values that
  governs how fast the inter-repetition intervals increase. A method for
  adjusting intervals when items are reviewed early or late has been taken
  from SM11, a later version of the algorithm, and included in Simple8."
  :group 'org-drill
  :type '(choice (const sm2) (const sm5) (const simple8)))


(defcustom org-drill-optimal-factor-matrix
  nil
  "DO NOT CHANGE THE VALUE OF THIS VARIABLE.

Persistent matrix of optimal factors, used by the SuperMemo SM5 algorithm.
The matrix is saved (using the 'customize' facility) at the end of each
drill session.

Over time, values in the matrix will adapt to the individual user's
pace of learning."
  :group 'org-drill
  :type 'sexp)


(defcustom org-drill-sm5-initial-interval
  4.0
  "In the SM5 algorithm, the initial interval after the first
successful presentation of an item is always 4 days. If you wish to change
this, you can do so here."
  :group 'org-drill
  :type 'float)


(defcustom org-drill-add-random-noise-to-intervals-p
  nil
  "If true, the number of days until an item's next repetition
will vary slightly from the interval calculated by the SM2
algorithm. The variation is very small when the interval is
small, but scales up with the interval."
  :group 'org-drill
  :type 'boolean)


(defcustom org-drill-adjust-intervals-for-early-and-late-repetitions-p
  nil
  "If true, when the student successfully reviews an item 1 or more days
before or after the scheduled review date, this will affect that date of
the item's next scheduled review, according to the algorithm presented at
 [[http://www.supermemo.com/english/algsm11.htm#Advanced%20repetitions]].

Items that were reviewed early will have their next review date brought
forward. Those that were reviewed late will have their next review
date postponed further.

Note that this option currently has no effect if the SM2 algorithm
is used."
  :group 'org-drill
  :type 'boolean)


(defcustom org-drill-cloze-text-weight
  4
  "For card types 'hide1_firstmore', 'show1_lastmore' and 'show1_firstless',
this number determines how often the 'less favoured' situation
should arise. It will occur 1 in every N trials, where N is the
value of the variable.

For example, with the hide1_firstmore card type, the first piece
of clozed text should be hidden more often than the other
pieces. If this variable is set to 4 (default), the first item
will only be shown 25% of the time (1 in 4 trials). Similarly for
show1_lastmore, the last item will be shown 75% of the time, and
for show1_firstless, the first item would only be shown 25% of the
time.

If the value of this variable is NIL, then weighting is disabled, and
all weighted card types are treated as their unweighted equivalents."
  :group 'org-drill
  :type '(choice integer (const nil)))


(defcustom org-drill-cram-hours
  12
  "When in cram mode, items are considered due for review if
they were reviewed at least this many hours ago."
  :group 'org-drill
  :type 'integer)


;;; NEW items have never been presented in a drill session before.
;;; MATURE items HAVE been presented at least once before.
;;; - YOUNG mature items were scheduled no more than
;;;   ORG-DRILL-DAYS-BEFORE-OLD days after their last
;;;   repetition. These items will have been learned 'recently' and will have a
;;;   low repetition count.
;;; - OLD mature items have intervals greater than
;;;   ORG-DRILL-DAYS-BEFORE-OLD.
;;; - OVERDUE items are past their scheduled review date by more than
;;;   LAST-INTERVAL * (ORG-DRILL-OVERDUE-INTERVAL-FACTOR - 1) days,
;;;   regardless of young/old status.


(defcustom org-drill-days-before-old
  10
  "When an item's inter-repetition interval rises above this value in days,
it is no longer considered a 'young' (recently learned) item."
  :group 'org-drill
  :type 'integer)


(defcustom org-drill-overdue-interval-factor
  1.2
  "An item is considered overdue if its scheduled review date is
more than (ORG-DRILL-OVERDUE-INTERVAL-FACTOR - 1) * LAST-INTERVAL
days in the past. For example, a value of 1.2 means an additional
20% of the last scheduled interval is allowed to elapse before
the item is overdue. A value of 1.0 means no extra time is
allowed at all - items are immediately considered overdue if
there is even one day's delay in reviewing them. This variable
should never be less than 1.0."
  :group 'org-drill
  :type 'float)


(defcustom org-drill-learn-fraction
  0.5
  "Fraction between 0 and 1 that governs how quickly the spaces
between successive repetitions increase, for all items. The
default value is 0.5. Higher values make spaces increase more
quickly with each successful repetition. You should only change
this in small increments (for example 0.05-0.1) as it has an
exponential effect on inter-repetition spacing."
  :group 'org-drill
  :type 'float)


(defvar *org-drill-session-qualities* nil)
(defvar *org-drill-start-time* 0)
(defvar *org-drill-new-entries* nil)
(defvar *org-drill-dormant-entry-count* 0)
(defvar *org-drill-due-entry-count* 0)
(defvar *org-drill-overdue-entry-count* 0)
(defvar *org-drill-due-tomorrow-count* 0)
(defvar *org-drill-overdue-entries* nil
  "List of markers for items that are considered 'overdue', based on
the value of ORG-DRILL-OVERDUE-INTERVAL-FACTOR.")
(defvar *org-drill-young-mature-entries* nil
  "List of markers for mature entries whose last inter-repetition
interval was <= ORG-DRILL-DAYS-BEFORE-OLD days.")
(defvar *org-drill-old-mature-entries* nil
  "List of markers for mature entries whose last inter-repetition
interval was greater than ORG-DRILL-DAYS-BEFORE-OLD days.")
(defvar *org-drill-failed-entries* nil)
(defvar *org-drill-again-entries* nil)
(defvar *org-drill-done-entries* nil)
(defvar *org-drill-current-item* nil
  "Set to the marker for the item currently being tested.")
(defvar *org-drill-cram-mode* nil
  "Are we in 'cram mode', where all items are considered due
for review unless they were already reviewed in the recent past?")
(defvar org-drill-scheduling-properties
  '("LEARN_DATA" "DRILL_LAST_INTERVAL" "DRILL_REPEATS_SINCE_FAIL"
    "DRILL_TOTAL_REPEATS" "DRILL_FAILURE_COUNT" "DRILL_AVERAGE_QUALITY"
    "DRILL_EASE" "DRILL_LAST_QUALITY" "DRILL_LAST_REVIEWED"))


;;; Make the above settings safe as file-local variables.


(put 'org-drill-question-tag 'safe-local-variable 'stringp)
(put 'org-drill-maximum-items-per-session 'safe-local-variable
     '(lambda (val) (or (integerp val) (null val))))
(put 'org-drill-maximum-duration 'safe-local-variable
     '(lambda (val) (or (integerp val) (null val))))
(put 'org-drill-failure-quality 'safe-local-variable 'integerp)
(put 'org-drill-forgetting-index 'safe-local-variable 'integerp)
(put 'org-drill-leech-failure-threshold 'safe-local-variable 'integerp)
(put 'org-drill-leech-method 'safe-local-variable
     '(lambda (val) (memq val '(nil skip warn))))
(put 'org-drill-use-visible-cloze-face-p 'safe-local-variable 'booleanp)
(put 'org-drill-hide-item-headings-p 'safe-local-variable 'booleanp)
(put 'org-drill-spaced-repetition-algorithm 'safe-local-variable
     '(lambda (val) (memq val '(simple8 sm5 sm2))))
(put 'org-drill-sm5-initial-interval 'safe-local-variable 'floatp)
(put 'org-drill-add-random-noise-to-intervals-p 'safe-local-variable 'booleanp)
(put 'org-drill-adjust-intervals-for-early-and-late-repetitions-p
     'safe-local-variable 'booleanp)
(put 'org-drill-cram-hours 'safe-local-variable 'integerp)
(put 'org-drill-learn-fraction 'safe-local-variable 'floatp)
(put 'org-drill-days-before-old 'safe-local-variable 'integerp)
(put 'org-drill-overdue-interval-factor 'safe-local-variable 'floatp)
(put 'org-drill-scope 'safe-local-variable
     '(lambda (val) (or (symbolp val) (listp val))))
(put 'org-drill-save-buffers-after-drill-sessions-p 'safe-local-variable 'booleanp)
(put 'org-drill-cloze-text-weight 'safe-local-variable
     '(lambda (val) (or (null val) (integerp val))))


;;;; Utilities ================================================================


(defun free-marker (m)
  (set-marker m nil))


(defmacro pop-random (place)
  (let ((idx (gensym)))
    `(if (null ,place)
         nil
       (let ((,idx (random* (length ,place))))
         (prog1 (nth ,idx ,place)
           (setq ,place (append (subseq ,place 0 ,idx)
                                (subseq ,place (1+ ,idx)))))))))


(defmacro push-end (val place)
  "Add VAL to the end of the sequence stored in PLACE. Return the new
value."
  `(setq ,place (append ,place (list ,val))))


(defun shuffle-list (list)
  "Randomly permute the elements of LIST (all permutations equally likely)."
  ;; Adapted from 'shuffle-vector' in cookie1.el
  (let ((i 0)
	j
	temp
	(len (length list)))
    (while (< i len)
      (setq j (+ i (random* (- len i))))
      (setq temp (nth i list))
      (setf (nth i list) (nth j list))
      (setf (nth j list) temp)
      (setq i (1+ i))))
  list)


(defun round-float (floatnum fix)
  "Round the floating point number FLOATNUM to FIX decimal places.
Example: (round-float 3.56755765 3) -> 3.568"
  (let ((n (expt 10 fix)))
    (/ (float (round (* floatnum n))) n)))


(defun command-keybinding-to-string (cmd)
  "Return a human-readable description of the key/keys to which the command
CMD is bound, or nil if it is not bound to a key."
  (let ((key (where-is-internal cmd overriding-local-map t)))
    (if key (key-description key))))


(defun time-to-inactive-org-timestamp (time)
  (format-time-string
   (concat "[" (substring (cdr org-time-stamp-formats) 1 -1) "]")
   time))


(defun org-map-drill-entries (func &optional scope &rest skip)
  "Like `org-map-entries', but only drill entries are processed."
  (let ((org-drill-scope (or scope org-drill-scope)))
    (apply 'org-map-entries func
           (concat "+" org-drill-question-tag)
           (case org-drill-scope
             (file nil)
             (file-no-restriction 'file)
             (directory
              (directory-files (file-name-directory (buffer-file-name))
                               t "\\.org$"))
             (t org-drill-scope))
           skip)))


(defmacro with-hidden-cloze-text (&rest body)
  `(progn
     (org-drill-hide-clozed-text)
     (unwind-protect
         (progn
           ,@body)
       (org-drill-unhide-clozed-text))))


(defmacro with-hidden-cloze-hints (&rest body)
  `(progn
     (org-drill-hide-cloze-hints)
     (unwind-protect
         (progn
           ,@body)
       (org-drill-unhide-text))))


(defmacro with-hidden-comments (&rest body)
  `(progn
     (if org-drill-hide-item-headings-p
         (org-drill-hide-heading-at-point))
     (org-drill-hide-comments)
     (unwind-protect
         (progn
           ,@body)
       (org-drill-unhide-text))))


(defun org-drill-days-since-last-review ()
  "Nil means a last review date has not yet been stored for
the item.
Zero means it was reviewed today.
A positive number means it was reviewed that many days ago.
A negative number means the date of last review is in the future --
this should never happen."
  (let ((datestr (org-entry-get (point) "DRILL_LAST_REVIEWED")))
    (when datestr
      (- (time-to-days (current-time))
         (time-to-days (apply 'encode-time
                              (org-parse-time-string datestr)))))))


(defun org-drill-hours-since-last-review ()
  "Like `org-drill-days-since-last-review', but return value is
in hours rather than days."
  (let ((datestr (org-entry-get (point) "DRILL_LAST_REVIEWED")))
    (when datestr
      (floor
       (/ (- (time-to-seconds (current-time))
             (time-to-seconds (apply 'encode-time
                                     (org-parse-time-string datestr))))
          (* 60 60))))))


(defun org-drill-entry-p (&optional marker)
  "Is MARKER, or the point, in a 'drill item'? This will return nil if
the point is inside a subheading of a drill item -- to handle that
situation use `org-part-of-drill-entry-p'."
  (save-excursion
    (when marker
      (org-drill-goto-entry marker))
    (member org-drill-question-tag (org-get-local-tags))))


(defun org-drill-goto-entry (marker)
  (org-pop-to-buffer-same-window (marker-buffer marker))
  (goto-char marker))


(defun org-part-of-drill-entry-p ()
  "Is the current entry either the main heading of a 'drill item',
or a subheading within a drill item?"
  (or (org-drill-entry-p)
      ;; Does this heading INHERIT the drill tag
      (member org-drill-question-tag (org-get-tags-at))))


(defun org-drill-goto-drill-entry-heading ()
  "Move the point to the heading which holds the :drill: tag for this
drill entry."
  (unless (org-at-heading-p)
    (org-back-to-heading))
  (unless (org-part-of-drill-entry-p)
    (error "Point is not inside a drill entry"))
  (while (not (org-drill-entry-p))
    (unless (org-up-heading-safe)
      (error "Cannot find a parent heading that is marked as a drill entry"))))



(defun org-drill-entry-leech-p ()
  "Is the current entry a 'leech item'?"
  (and (org-drill-entry-p)
       (member "leech" (org-get-local-tags))))


;; (defun org-drill-entry-due-p ()
;;   (cond
;;    (*org-drill-cram-mode*
;;     (let ((hours (org-drill-hours-since-last-review)))
;;       (and (org-drill-entry-p)
;;            (or (null hours)
;;                (>= hours org-drill-cram-hours)))))
;;    (t
;;     (let ((item-time (org-get-scheduled-time (point))))
;;       (and (org-drill-entry-p)
;;            (or (not (eql 'skip org-drill-leech-method))
;;                (not (org-drill-entry-leech-p)))
;;            (or (null item-time)         ; not scheduled
;;                (not (minusp             ; scheduled for today/in past
;;                      (- (time-to-days (current-time))
;;                         (time-to-days item-time))))))))))


(defun org-drill-entry-days-overdue ()
  "Returns:
- NIL if the item is not to be regarded as scheduled for review at all.
  This is the case if it is not a drill item, or if it is a leech item
  that we wish to skip, or if we are in cram mode and have already reviewed
  the item within the last few hours.
- 0 if the item is new, or if it scheduled for review today.
- A negative integer - item is scheduled that many days in the future.
- A positive integer - item is scheduled that many days in the past."
  (cond
   (*org-drill-cram-mode*
    (let ((hours (org-drill-hours-since-last-review)))
      (and (org-drill-entry-p)
           (or (null hours)
               (>= hours org-drill-cram-hours))
           0)))
   (t
    (let ((item-time (org-get-scheduled-time (point))))
      (cond
       ((or (not (org-drill-entry-p))
            (and (eql 'skip org-drill-leech-method)
                 (org-drill-entry-leech-p)))
        nil)
       ((null item-time)                ; not scheduled -> due now
        0)
       (t
        (- (time-to-days (current-time))
           (time-to-days item-time))))))))


(defun org-drill-entry-overdue-p (&optional days-overdue last-interval)
  "Returns true if entry that is scheduled DAYS-OVERDUE dasy in the past,
and whose last inter-repetition interval was LAST-INTERVAL, should be
considered 'overdue'. If the arguments are not given they are extracted
from the entry at point."
  (unless days-overdue
    (setq days-overdue (org-drill-entry-days-overdue)))
  (unless last-interval
    (setq last-interval (org-drill-entry-last-interval 1)))
  (and (numberp days-overdue)
       (> days-overdue 1)               ; enforce a sane minimum 'overdue' gap
       ;;(> due org-drill-days-before-overdue)
       (> (/ (+ days-overdue last-interval 1.0) last-interval)
          org-drill-overdue-interval-factor)))



(defun org-drill-entry-due-p ()
  (let ((due (org-drill-entry-days-overdue)))
    (and (not (null due))
         (not (minusp due)))))


(defun org-drill-entry-new-p ()
  (and (org-drill-entry-p)
       (let ((item-time (org-get-scheduled-time (point))))
         (null item-time))))


(defun org-drill-entry-last-quality (&optional default)
  (let ((quality (org-entry-get (point) "DRILL_LAST_QUALITY")))
    (if quality
        (string-to-number quality)
      default)))


(defun org-drill-entry-failure-count ()
  (let ((quality (org-entry-get (point) "DRILL_FAILURE_COUNT")))
    (if quality
        (string-to-number quality)
      0)))


(defun org-drill-entry-average-quality (&optional default)
  (let ((val (org-entry-get (point) "DRILL_AVERAGE_QUALITY")))
    (if val
        (string-to-number val)
      (or default nil))))

(defun org-drill-entry-last-interval (&optional default)
  (let ((val (org-entry-get (point) "DRILL_LAST_INTERVAL")))
    (if val
        (string-to-number val)
      (or default 0))))

(defun org-drill-entry-repeats-since-fail (&optional default)
  (let ((val (org-entry-get (point) "DRILL_REPEATS_SINCE_FAIL")))
    (if val
        (string-to-number val)
      (or default 0))))

(defun org-drill-entry-total-repeats (&optional default)
  (let ((val (org-entry-get (point) "DRILL_TOTAL_REPEATS")))
    (if val
        (string-to-number val)
      (or default 0))))

(defun org-drill-entry-ease (&optional default)
  (let ((val (org-entry-get (point) "DRILL_EASE")))
    (if val
        (string-to-number val)
      default)))


;;; From http://www.supermemo.com/english/ol/sm5.htm
(defun org-drill-random-dispersal-factor ()
  "Returns a random number between 0.5 and 1.5."
  (let ((a 0.047)
        (b 0.092)
        (p (- (random* 1.0) 0.5)))
    (flet ((sign (n)
                 (cond ((zerop n) 0)
                       ((plusp n) 1)
                       (t -1))))
      (/ (+ 100 (* (* (/ -1 b) (log (- 1 (* (/ b a ) (abs p)))))
                   (sign p)))
         100.0))))

(defun pseudonormal (mean variation)
  "Random numbers in a pseudo-normal distribution with mean MEAN, range
    MEAN-VARIATION to MEAN+VARIATION"
  (+  (random* variation)
      (random* variation)
      (- variation)
      mean))


(defun org-drill-early-interval-factor (optimal-factor
                                                optimal-interval
                                                days-ahead)
  "Arguments:
- OPTIMAL-FACTOR: interval-factor if the item had been tested
exactly when it was supposed to be.
- OPTIMAL-INTERVAL: interval for next repetition (days) if the item had been
tested exactly when it was supposed to be.
- DAYS-AHEAD: how many days ahead of time the item was reviewed.

Returns an adjusted optimal factor which should be used to
calculate the next interval, instead of the optimal factor found
in the matrix."
  (let ((delta-ofmax (* (1- optimal-factor)
                    (/ (+ optimal-interval
                          (* 0.6 optimal-interval) -1) (1- optimal-interval)))))
    (- optimal-factor
       (* delta-ofmax (/ days-ahead (+ days-ahead (* 0.6 optimal-interval)))))))


(defun org-drill-get-item-data ()
  "Returns a list of 6 items, containing all the stored recall
  data for the item at point:
- LAST-INTERVAL is the interval in days that was used to schedule the item's
  current review date.
- REPEATS is the number of items the item has been successfully recalled without
  without any failures. It is reset to 0 upon failure to recall the item.
- FAILURES is the total number of times the user has failed to recall the item.
- TOTAL-REPEATS includes both successful and unsuccessful repetitions.
- AVERAGE-QUALITY is the mean quality of recall of the item over
  all its repetitions, successful and unsuccessful.
- EASE is a number reflecting how easy the item is to learn. Higher is easier.
"
  (let ((learn-str (org-entry-get (point) "LEARN_DATA"))
        (repeats (org-drill-entry-total-repeats :missing)))
    (cond
     (learn-str
      (let ((learn-data (or (and learn-str
                                 (read learn-str))
                            (copy-list initial-repetition-state))))
        (list (nth 0 learn-data)        ; last interval
              (nth 1 learn-data)        ; repetitions
              (org-drill-entry-failure-count)
              (nth 1 learn-data)
              (org-drill-entry-last-quality)
              (nth 2 learn-data)        ; EF
              )))
     ((not (eql :missing repeats))
      (list (org-drill-entry-last-interval)
            (org-drill-entry-repeats-since-fail)
            (org-drill-entry-failure-count)
            (org-drill-entry-total-repeats)
            (org-drill-entry-average-quality)
            (org-drill-entry-ease)))
     (t  ; virgin item
      (list 0 0 0 0 nil nil)))))


(defun org-drill-store-item-data (last-interval repeats failures
                                                total-repeats meanq
                                                ease)
  "Stores the given data in the item at point."
  (org-entry-delete (point) "LEARN_DATA")
  (org-set-property "DRILL_LAST_INTERVAL"
                    (number-to-string (round-float last-interval 4)))
  (org-set-property "DRILL_REPEATS_SINCE_FAIL" (number-to-string repeats))
  (org-set-property "DRILL_TOTAL_REPEATS" (number-to-string total-repeats))
  (org-set-property "DRILL_FAILURE_COUNT" (number-to-string failures))
  (org-set-property "DRILL_AVERAGE_QUALITY"
                    (number-to-string (round-float meanq 3)))
  (org-set-property "DRILL_EASE"
                    (number-to-string (round-float ease 3))))



;;; SM2 Algorithm =============================================================


(defun determine-next-interval-sm2 (last-interval n ef quality
                                                  failures meanq total-repeats)
  "Arguments:
- LAST-INTERVAL -- the number of days since the item was last reviewed.
- REPEATS -- the number of times the item has been successfully reviewed
- EF -- the 'easiness factor'
- QUALITY -- 0 to 5

Returns a list: (INTERVAL REPEATS EF FAILURES MEAN TOTAL-REPEATS OFMATRIX), where:
- INTERVAL is the number of days until the item should next be reviewed
- REPEATS is incremented by 1.
- EF is modified based on the recall quality for the item.
- OF-MATRIX is not modified."
  (assert (> n 0))
  (assert (and (>= quality 0) (<= quality 5)))
  (if (<= quality org-drill-failure-quality)
      ;; When an item is failed, its interval is reset to 0,
      ;; but its EF is unchanged
      (list -1 1 ef (1+ failures) meanq (1+ total-repeats)
            org-drill-optimal-factor-matrix)
    ;; else:
    (let* ((next-ef (modify-e-factor ef quality))
           (interval
            (cond
             ((<= n 1) 1)
             ((= n 2)
              (cond
               (org-drill-add-random-noise-to-intervals-p
                (case quality
                  (5 6)
                  (4 4)
                  (3 3)
                  (2 1)
                  (t -1)))
               (t 6)))
             (t (* last-interval next-ef)))))
      (list (if org-drill-add-random-noise-to-intervals-p
                (+ last-interval (* (- interval last-interval)
                                    (org-drill-random-dispersal-factor)))
              interval)
            (1+ n)
            next-ef
            failures meanq (1+ total-repeats)
            org-drill-optimal-factor-matrix))))


;;; SM5 Algorithm =============================================================



(defun initial-optimal-factor-sm5 (n ef)
  (if (= 1 n)
      org-drill-sm5-initial-interval
    ef))

(defun get-optimal-factor-sm5 (n ef of-matrix)
  (let ((factors (assoc n of-matrix)))
    (or (and factors
	     (let ((ef-of (assoc ef (cdr factors))))
	       (and ef-of (cdr ef-of))))
	(initial-optimal-factor-sm5 n ef))))


(defun inter-repetition-interval-sm5 (last-interval n ef &optional of-matrix)
  (let ((of (get-optimal-factor-sm5 n ef (or of-matrix
                                             org-drill-optimal-factor-matrix))))
    (if (= 1 n)
	of
      (* of last-interval))))


(defun determine-next-interval-sm5 (last-interval n ef quality
                                                  failures meanq total-repeats
                                                  of-matrix &optional delta-days)
  (if (zerop n) (setq n 1))
  (if (null ef) (setq ef 2.5))
  (assert (> n 0))
  (assert (and (>= quality 0) (<= quality 5)))
  (unless of-matrix
    (setq of-matrix org-drill-optimal-factor-matrix))
  (setq of-matrix (cl-copy-tree of-matrix))

  (setq meanq (if meanq
                  (/ (+ quality (* meanq total-repeats 1.0))
                     (1+ total-repeats))
                quality))

  (let ((next-ef (modify-e-factor ef quality))
        (old-ef ef)
        (new-of (modify-of (get-optimal-factor-sm5 n ef of-matrix)
                           quality org-drill-learn-fraction))
        (interval nil))
    (when (and org-drill-adjust-intervals-for-early-and-late-repetitions-p
               delta-days (minusp delta-days))
      (setq new-of (org-drill-early-interval-factor
                    (get-optimal-factor-sm5 n ef of-matrix)
                    (inter-repetition-interval-sm5
                     last-interval n ef of-matrix)
                    delta-days)))

    (setq of-matrix
          (set-optimal-factor n next-ef of-matrix
                              (round-float new-of 3))) ; round OF to 3 d.p.

    (setq ef next-ef)

    (cond
     ;; "Failed" -- reset repetitions to 0,
     ((<= quality org-drill-failure-quality)
      (list -1 1 old-ef (1+ failures) meanq (1+ total-repeats)
            of-matrix))     ; Not clear if OF matrix is supposed to be
                                        ; preserved
     ;; For a zero-based quality of 4 or 5, don't repeat
     ;; ((and (>= quality 4)
     ;;       (not org-learn-always-reschedule))
     ;;  (list 0 (1+ n) ef failures meanq
     ;;        (1+ total-repeats) of-matrix))     ; 0 interval = unschedule
     (t
      (setq interval (inter-repetition-interval-sm5
                      last-interval n ef of-matrix))
      (if org-drill-add-random-noise-to-intervals-p
          (setq interval (* interval (org-drill-random-dispersal-factor))))
      (list interval
            (1+ n)
            ef
            failures
            meanq
            (1+ total-repeats)
            of-matrix)))))


;;; Simple8 Algorithm =========================================================


(defun org-drill-simple8-first-interval (failures)
  "Arguments:
- FAILURES: integer >= 0. The total number of times the item has
  been forgotten, ever.

Returns the optimal FIRST interval for an item which has previously been
forgotten on FAILURES occasions."
  (* 2.4849 (exp (* -0.057 failures))))


(defun org-drill-simple8-interval-factor (ease repetition)
  "Arguments:
- EASE: floating point number >= 1.2. Corresponds to `AF' in SM8 algorithm.
- REPETITION: the number of times the item has been tested.
1 is the first repetition (ie the second trial).
Returns:
The factor by which the last interval should be
multiplied to give the next interval. Corresponds to `RF' or `OF'."
  (+ 1.2 (* (- ease 1.2) (expt org-drill-learn-fraction (log repetition 2)))))


(defun org-drill-simple8-quality->ease (quality)
  "Returns the ease (`AF' in the SM8 algorithm) which corresponds
to a mean item quality of QUALITY."
  (+ (* 0.0542 (expt quality 4))
     (* -0.4848 (expt quality 3))
     (* 1.4916 (expt quality 2))
     (* -1.2403 quality)
     1.4515))


(defun determine-next-interval-simple8 (last-interval repeats quality
                                                      failures meanq totaln
                                                      &optional delta-days)
  "Arguments:
- LAST-INTERVAL -- the number of days since the item was last reviewed.
- REPEATS -- the number of times the item has been successfully reviewed
- EASE -- the 'easiness factor'
- QUALITY -- 0 to 5
- DELTA-DAYS -- how many days overdue was the item when it was reviewed.
  0 = reviewed on the scheduled day. +N = N days overdue.
  -N = reviewed N days early.

Returns the new item data, as a list of 6 values:
- NEXT-INTERVAL
- REPEATS
- EASE
- FAILURES
- AVERAGE-QUALITY
- TOTAL-REPEATS.
See the documentation for `org-drill-get-item-data' for a description of these."
  (assert (>= repeats 0))
  (assert (and (>= quality 0) (<= quality 5)))
  (assert (or (null meanq) (and (>= meanq 0) (<= meanq 5))))
  (let ((next-interval nil))
    (setf meanq (if meanq
                    (/ (+ quality (* meanq totaln 1.0)) (1+ totaln))
                  quality))
    (cond
     ((<= quality org-drill-failure-quality)
      (incf failures)
      (setf repeats 0
            next-interval -1))
     ((or (zerop repeats)
          (zerop last-interval))
      (setf next-interval (org-drill-simple8-first-interval failures))
      (incf repeats)
      (incf totaln))
     (t
      (let* ((use-n
              (if (and
                   org-drill-adjust-intervals-for-early-and-late-repetitions-p
                   (numberp delta-days) (plusp delta-days)
                   (plusp last-interval))
                  (+ repeats (min 1 (/ delta-days last-interval 1.0)))
                repeats))
             (factor (org-drill-simple8-interval-factor
                      (org-drill-simple8-quality->ease meanq) use-n))
             (next-int (* last-interval factor)))
        (when (and org-drill-adjust-intervals-for-early-and-late-repetitions-p
                   (numberp delta-days) (minusp delta-days))
          ;; The item was reviewed earlier than scheduled.
          (setf factor (org-drill-early-interval-factor
                        factor next-int (abs delta-days))
                next-int (* last-interval factor)))
        (setf next-interval next-int)
        (incf repeats)
        (incf totaln))))
    (list
     (if (and org-drill-add-random-noise-to-intervals-p
              (plusp next-interval))
         (* next-interval (org-drill-random-dispersal-factor))
       next-interval)
     repeats
     (org-drill-simple8-quality->ease meanq)
     failures
     meanq
     totaln
     )))




;;; Essentially copied from `org-learn.el', but modified to
;;; optionally call the SM2 or simple8 functions.
(defun org-drill-smart-reschedule (quality &optional days-ahead)
  "If DAYS-AHEAD is supplied it must be a positive integer. The
item will be scheduled exactly this many days into the future."
  (let ((delta-days (- (time-to-days (current-time))
                   (time-to-days (or (org-get-scheduled-time (point))
                                     (current-time)))))
        (ofmatrix org-drill-optimal-factor-matrix)
        ;; Entries can have weights, 1 by default. Intervals are divided by the
        ;; item's weight, so an item with a weight of 2 will have all intervals
        ;; halved, meaning you will end up reviewing it twice as often.
        ;; Useful for entries which randomly present any of several facts.
        (weight (org-entry-get (point) "DRILL_CARD_WEIGHT")))
    (if (stringp weight)
        (setq weight (read weight)))
    (destructuring-bind (last-interval repetitions failures
                                       total-repeats meanq ease)
        (org-drill-get-item-data)
      (destructuring-bind (next-interval repetitions ease
                                         failures meanq total-repeats
                                         &optional new-ofmatrix)
          (case org-drill-spaced-repetition-algorithm
            (sm5 (determine-next-interval-sm5 last-interval repetitions
                                              ease quality failures
                                              meanq total-repeats ofmatrix))
            (sm2 (determine-next-interval-sm2 last-interval repetitions
                                              ease quality failures
                                              meanq total-repeats))
            (simple8 (determine-next-interval-simple8 last-interval repetitions
                                                      quality failures meanq
                                                      total-repeats
                                                      delta-days)))
        (if (numberp days-ahead)
            (setq next-interval days-ahead))

        (if (and (null days-ahead)
                 (numberp weight) (plusp weight)
                 (not (minusp next-interval)))
            (setq next-interval
                  (max 1.0 (+ last-interval
                              (/ (- next-interval last-interval) weight)))))

        (org-drill-store-item-data next-interval repetitions failures
                                   total-repeats meanq ease)

        (if (eql 'sm5 org-drill-spaced-repetition-algorithm)
            (setq org-drill-optimal-factor-matrix new-ofmatrix))

        (cond
         ((= 0 days-ahead)
          (org-schedule t))
         ((minusp days-ahead)
          (org-schedule nil (current-time)))
         (t
          (org-schedule nil (time-add (current-time)
                                      (days-to-time
                                       (round next-interval))))))))))


(defun org-drill-hypothetical-next-review-date (quality)
  "Returns an integer representing the number of days into the future
that the current item would be scheduled, based on a recall quality
of QUALITY."
  (let ((weight (org-entry-get (point) "DRILL_CARD_WEIGHT")))
    (destructuring-bind (last-interval repetitions failures
                                       total-repeats meanq ease)
        (org-drill-get-item-data)
      (if (stringp weight)
          (setq weight (read weight)))
      (destructuring-bind (next-interval repetitions ease
                                         failures meanq total-repeats
                                         &optional ofmatrix)
          (case org-drill-spaced-repetition-algorithm
            (sm5 (determine-next-interval-sm5 last-interval repetitions
                                              ease quality failures
                                              meanq total-repeats
                                              org-drill-optimal-factor-matrix))
            (sm2 (determine-next-interval-sm2 last-interval repetitions
                                              ease quality failures
                                              meanq total-repeats))
            (simple8 (determine-next-interval-simple8 last-interval repetitions
                                                      quality failures meanq
                                                      total-repeats)))
        (cond
         ((not (plusp next-interval))
          0)
         ((and (numberp weight) (plusp weight))
          (+ last-interval
             (max 1.0 (/ (- next-interval last-interval) weight))))
         (t
          next-interval))))))


(defun org-drill-hypothetical-next-review-dates ()
  (let ((intervals nil))
    (dotimes (q 6)
      (push (max (or (car intervals) 0)
                 (org-drill-hypothetical-next-review-date q))
            intervals))
    (reverse intervals)))


(defun org-drill-reschedule ()
  "Returns quality rating (0-5), or nil if the user quit."
  (let ((ch nil)
        (input nil)
        (next-review-dates (org-drill-hypothetical-next-review-dates)))
    (save-excursion
      (while (not (memq ch '(?q ?e ?0 ?1 ?2 ?3 ?4 ?5)))
        (setq input (read-key-sequence
                     (if (eq ch ??)
                         (format "0-2 Means you have forgotten the item.
3-5 Means you have remembered the item.

0 - Completely forgot.
1 - Even after seeing the answer, it still took a bit to sink in.
2 - After seeing the answer, you remembered it.
3 - It took you awhile, but you finally remembered. (+%s days)
4 - After a little bit of thought you remembered. (+%s days)
5 - You remembered the item really easily. (+%s days)

How well did you do? (0-5, ?=help, e=edit, t=tags, q=quit)"
                                 (round (nth 3 next-review-dates))
                                 (round (nth 4 next-review-dates))
                                 (round (nth 5 next-review-dates)))
                       "How well did you do? (0-5, ?=help, e=edit, t=tags, q=quit)")))
        (cond
         ((stringp input)
          (setq ch (elt input 0)))
         ((and (vectorp input) (symbolp (elt input 0)))
          (case (elt input 0)
            (up (ignore-errors (forward-line -1)))
            (down (ignore-errors (forward-line 1)))
            (left (ignore-errors (backward-char)))
            (right (ignore-errors (forward-char)))
            (prior (ignore-errors (scroll-down))) ; pgup
            (next (ignore-errors (scroll-up)))))  ; pgdn
         ((and (vectorp input) (listp (elt input 0))
               (eventp (elt input 0)))
          (case (car (elt input 0))
            (wheel-up (ignore-errors (mwheel-scroll (elt input 0))))
            (wheel-down (ignore-errors (mwheel-scroll (elt input 0)))))))
        (if (eql ch ?t)
            (org-set-tags-command))))
    (cond
     ((and (>= ch ?0) (<= ch ?5))
      (let ((quality (- ch ?0))
            (failures (org-drill-entry-failure-count)))
        (save-excursion
          (org-drill-smart-reschedule quality
                                      (nth quality next-review-dates)))
        (push quality *org-drill-session-qualities*)
        (cond
         ((<= quality org-drill-failure-quality)
          (when org-drill-leech-failure-threshold
            ;;(setq failures (if failures (string-to-number failures) 0))
            ;; (org-set-property "DRILL_FAILURE_COUNT"
            ;;                   (format "%d" (1+ failures)))
            (if (> (1+ failures) org-drill-leech-failure-threshold)
                (org-toggle-tag "leech" 'on))))
         (t
          (let ((scheduled-time (org-get-scheduled-time (point))))
            (when scheduled-time
              (message "Next review in %d days"
                       (- (time-to-days scheduled-time)
                          (time-to-days (current-time))))
              (sit-for 0.5)))))
        (org-set-property "DRILL_LAST_QUALITY" (format "%d" quality))
        (org-set-property "DRILL_LAST_REVIEWED"
                          (time-to-inactive-org-timestamp (current-time)))
        quality))
     ((= ch ?e)
      'edit)
     (t
      nil))))


;; (defun org-drill-hide-all-subheadings-except (heading-list)
;;   "Returns a list containing the position of each immediate subheading of
;; the current topic."
;;   (let ((drill-entry-level (org-current-level))
;;         (drill-sections nil)
;;         (drill-heading nil))
;;     (org-show-subtree)
;;     (save-excursion
;;       (org-map-entries
;;        (lambda ()
;;          (when (and (not (outline-invisible-p))
;;                     (> (org-current-level) drill-entry-level))
;;            (setq drill-heading (org-get-heading t))
;;            (unless (and (= (org-current-level) (1+ drill-entry-level))
;;                         (member drill-heading heading-list))
;;              (hide-subtree))
;;            (push (point) drill-sections)))
;;        "" 'tree))
;;     (reverse drill-sections)))



(defun org-drill-hide-subheadings-if (test)
  "TEST is a function taking no arguments. TEST will be called for each
of the immediate subheadings of the current drill item, with the point
on the relevant subheading. TEST should return nil if the subheading is
to be revealed, non-nil if it is to be hidden.
Returns a list containing the position of each immediate subheading of
the current topic."
  (let ((drill-entry-level (org-current-level))
        (drill-sections nil))
    (org-show-subtree)
    (save-excursion
      (org-map-entries
       (lambda ()
         (when (and (not (outline-invisible-p))
                    (> (org-current-level) drill-entry-level))
           (when (or (/= (org-current-level) (1+ drill-entry-level))
                        (funcall test))
             (hide-subtree))
           (push (point) drill-sections)))
       "" 'tree))
    (reverse drill-sections)))


(defun org-drill-hide-all-subheadings-except (heading-list)
  (org-drill-hide-subheadings-if
   (lambda () (let ((drill-heading (org-get-heading t)))
           (not (member drill-heading heading-list))))))


(defun org-drill-presentation-prompt (&rest fmt-and-args)
  (let* ((item-start-time (current-time))
         (input nil)
         (ch nil)
         (last-second 0)
         (mature-entry-count (+ (length *org-drill-young-mature-entries*)
                                (length *org-drill-old-mature-entries*)
                                (length *org-drill-overdue-entries*)))
         (status (first (org-drill-entry-status)))
         (prompt
          (if fmt-and-args
              (apply 'format
                     (first fmt-and-args)
                     (rest fmt-and-args))
            (concat "Press key for answer, "
                    "e=edit, t=tags, s=skip, q=quit."))))
    (setq prompt
          (format "%s %s %s %s %s %s"
                  (propertize
                   (char-to-string
                    (case status
                      (:new ?N) (:young ?Y) (:old ?o) (:overdue ?!)
                      (:failed ?F) (t ??)))
                   'face `(:foreground
                           ,(case status
                              (:new org-drill-new-count-color)
                              ((:young :old) org-drill-mature-count-color)
                              ((:overdue :failed) org-drill-failed-count-color)
                              (t org-drill-done-count-color))))
                  (propertize
                   (number-to-string (length *org-drill-done-entries*))
                   'face `(:foreground ,org-drill-done-count-color)
                   'help-echo "The number of items you have reviewed this session.")
                  (propertize
                   (number-to-string (+ (length *org-drill-again-entries*)
                                        (length *org-drill-failed-entries*)))
                   'face `(:foreground ,org-drill-failed-count-color)
                   'help-echo (concat "The number of items that you failed, "
                                      "and need to review again."))
                  (propertize
                   (number-to-string mature-entry-count)
                   'face `(:foreground ,org-drill-mature-count-color)
                   'help-echo "The number of old items due for review.")
                  (propertize
                   (number-to-string (length *org-drill-new-entries*))
                   'face `(:foreground ,org-drill-new-count-color)
                   'help-echo (concat "The number of new items that you "
                                      "have never reviewed."))
                  prompt))
    (if (and (eql 'warn org-drill-leech-method)
             (org-drill-entry-leech-p))
        (setq prompt (concat
                      (propertize "!!! LEECH ITEM !!!
You seem to be having a lot of trouble memorising this item.
Consider reformulating the item to make it easier to remember.\n"
                                  'face '(:foreground "red"))
                      prompt)))
    (while (memq ch '(nil ?t))
      (setq ch nil)
      (while (not (input-pending-p))
        (let ((elapsed (time-subtract (current-time) item-start-time)))
          (message (concat (if (>= (time-to-seconds elapsed) (* 60 60))
                               "++:++ "
                             (format-time-string "%M:%S " elapsed))
                           prompt))
          (sit-for 1)))
      (setq input (read-key-sequence nil))
      (if (stringp input) (setq ch (elt input 0)))
      (if (eql ch ?t)
          (org-set-tags-command)))
    (case ch
      (?q nil)
      (?e 'edit)
      (?s 'skip)
      (otherwise t))))


(defun org-pos-in-regexp (pos regexp &optional nlines)
  (save-excursion
    (goto-char pos)
    (org-in-regexp regexp nlines)))


(defun org-drill-hide-region (beg end &optional text)
  "Hide the buffer region between BEG and END with an 'invisible text'
visual overlay, or with the string TEXT if it is supplied."
  (let ((ovl (make-overlay beg end)))
    (overlay-put ovl 'category
                 'org-drill-hidden-text-overlay)
    (when (stringp text)
      (overlay-put ovl 'invisible nil)
      (overlay-put ovl 'face 'default)
      (overlay-put ovl 'display text))))


(defun org-drill-hide-heading-at-point (&optional text)
  (unless (org-at-heading-p)
    (error "Point is not on a heading."))
  (save-excursion
    (let ((beg (point)))
      (end-of-line)
      (org-drill-hide-region beg (point) text))))


(defun org-drill-hide-comments ()
  (save-excursion
    (while (re-search-forward "^#.*$" nil t)
      (org-drill-hide-region (match-beginning 0) (match-end 0)))))


(defun org-drill-unhide-text ()
  ;; This will also unhide the item's heading.
  (save-excursion
    (dolist (ovl (overlays-in (point-min) (point-max)))
      (when (eql 'org-drill-hidden-text-overlay (overlay-get ovl 'category))
        (delete-overlay ovl)))))


(defun org-drill-hide-clozed-text ()
  (save-excursion
    (while (re-search-forward org-drill-cloze-regexp nil t)
      ;; Don't hide org links, partly because they might contain inline
      ;; images which we want to keep visible
      (unless (save-match-data
                (org-pos-in-regexp (match-beginning 0)
                                   org-bracket-link-regexp 1))
        (org-drill-hide-matched-cloze-text)))))


(defun org-drill-hide-matched-cloze-text ()
  "Hide the current match with a 'cloze' visual overlay."
  (let ((ovl (make-overlay (match-beginning 0) (match-end 0))))
    (overlay-put ovl 'category
                 'org-drill-cloze-overlay-defaults)
    (when (find ?| (match-string 0))
      (let ((hint (substring-no-properties
                   (match-string 0)
                   (1+ (position ?| (match-string 0)))
                   (1- (length (match-string 0))))))
        (overlay-put
         ovl 'display
         ;; If hint is like `X...' then display [X...]
         ;; otherwise display [...X]
         (format (if (string-match-p "\\.\\.\\." hint) "[%s]" "[%s...]")
                 hint))))))


(defun org-drill-hide-cloze-hints ()
  (save-excursion
    (while (re-search-forward org-drill-cloze-regexp nil t)
      (unless (or (save-match-data
                    (org-pos-in-regexp (match-beginning 0)
                                       org-bracket-link-regexp 1))
                  (null (match-beginning 2))) ; hint subexpression matched
        (org-drill-hide-region (match-beginning 2) (match-end 2))))))


(defmacro with-replaced-entry-text (text &rest body)
  "During the execution of BODY, the entire text of the current entry is
concealed by an overlay that displays the string TEXT."
  `(progn
     (org-drill-replace-entry-text ,text)
     (unwind-protect
         (progn
           ,@body)
       (org-drill-unreplace-entry-text))))


(defun org-drill-replace-entry-text (text)
  "Make an overlay that conceals the entire text of the item, not
including properties or the contents of subheadings. The overlay shows
the string TEXT.
Note: does not actually alter the item."
  (let ((ovl (make-overlay (point-min)
                           (save-excursion
                             (outline-next-heading)
                             (point)))))
    (overlay-put ovl 'category
                 'org-drill-replaced-text-overlay)
    (overlay-put ovl 'display text)))


(defun org-drill-unreplace-entry-text ()
  (save-excursion
    (dolist (ovl (overlays-in (point-min) (point-max)))
      (when (eql 'org-drill-replaced-text-overlay (overlay-get ovl 'category))
        (delete-overlay ovl)))))


(defmacro with-replaced-entry-heading (heading &rest body)
  `(progn
     (org-drill-replace-entry-heading ,heading)
     (unwind-protect
         (progn
           ,@body)
       (org-drill-unhide-text))))


(defun org-drill-replace-entry-heading (heading)
  "Make an overlay that conceals the heading of the item. The overlay shows
the string TEXT.
Note: does not actually alter the item."
  (org-drill-hide-heading-at-point heading))


(defun org-drill-unhide-clozed-text ()
  (save-excursion
    (dolist (ovl (overlays-in (point-min) (point-max)))
      (when (eql 'org-drill-cloze-overlay-defaults (overlay-get ovl 'category))
        (delete-overlay ovl)))))


(defun org-drill-get-entry-text (&optional keep-properties-p)
  (let ((text (org-agenda-get-some-entry-text (point-marker) 100)))
    (if keep-properties-p
        text
      (substring-no-properties text))))


(defun org-drill-entry-empty-p ()
  (zerop (length (org-drill-get-entry-text))))



;;; Presentation functions ====================================================

;; Each of these is called with point on topic heading.  Each needs to show the
;; topic in the form of a 'question' or with some information 'hidden', as
;; appropriate for the card type. The user should then be prompted to press a
;; key. The function should then reveal either the 'answer' or the entire
;; topic, and should return t if the user chose to see the answer and rate their
;; recall, nil if they chose to quit.

(defun org-drill-present-simple-card ()
  (with-hidden-comments
   (with-hidden-cloze-hints
    (with-hidden-cloze-text
     (org-drill-hide-all-subheadings-except nil)
     (org-display-inline-images t)
     (org-cycle-hide-drawers 'all)
     (prog1 (org-drill-presentation-prompt)
       (org-drill-hide-subheadings-if 'org-drill-entry-p))))))


(defun org-drill-present-default-answer (reschedule-fn)
  (org-drill-hide-subheadings-if 'org-drill-entry-p)
  (org-drill-unhide-clozed-text)
  (with-hidden-cloze-hints
   (funcall reschedule-fn)))


(defun org-drill-present-two-sided-card ()
  (with-hidden-comments
   (with-hidden-cloze-hints
    (with-hidden-cloze-text
     (let ((drill-sections (org-drill-hide-all-subheadings-except nil)))
       (when drill-sections
         (save-excursion
           (goto-char (nth (random* (min 2 (length drill-sections)))
                           drill-sections))
           (org-show-subtree)))
       (org-display-inline-images t)
       (org-cycle-hide-drawers 'all)
       (prog1 (org-drill-presentation-prompt)
         (org-drill-hide-subheadings-if 'org-drill-entry-p)))))))



(defun org-drill-present-multi-sided-card ()
  (with-hidden-comments
   (with-hidden-cloze-hints
    (with-hidden-cloze-text
     (let ((drill-sections (org-drill-hide-all-subheadings-except nil)))
       (when drill-sections
         (save-excursion
           (goto-char (nth (random* (length drill-sections)) drill-sections))
           (org-show-subtree)))
       (org-display-inline-images t)
       (org-cycle-hide-drawers 'all)
       (prog1 (org-drill-presentation-prompt)
         (org-drill-hide-subheadings-if 'org-drill-entry-p)))))))


(defun org-drill-present-multicloze-hide-n (number-to-hide
                                            &optional
                                            force-show-first
                                            force-show-last
                                            force-hide-first)
  "Hides NUMBER-TO-HIDE pieces of text that are marked for cloze deletion,
chosen at random.
If NUMBER-TO-HIDE is negative, show only (ABS NUMBER-TO-HIDE) pieces,
hiding all the rest.
If FORCE-HIDE-FIRST is non-nil, force the first piece of text to be one of
the hidden items.
If FORCE-SHOW-FIRST is non-nil, never hide the first piece of text.
If FORCE-SHOW-LAST is non-nil, never hide the last piece of text.
If the number of text pieces in the item is less than
NUMBER-TO-HIDE, then all text pieces will be hidden (except the first or last
items if FORCE-SHOW-FIRST or FORCE-SHOW-LAST is non-nil)."
  (with-hidden-comments
   (with-hidden-cloze-hints
    (let ((item-end nil)
          (match-count 0)
          (body-start (or (cdr (org-get-property-block))
                          (point))))
      (if (and force-hide-first force-show-first)
          (error "FORCE-HIDE-FIRST and FORCE-SHOW-FIRST are mutually exclusive"))
      (org-drill-hide-all-subheadings-except nil)
      (save-excursion
        (outline-next-heading)
        (setq item-end (point)))
      (save-excursion
        (goto-char body-start)
        (while (re-search-forward org-drill-cloze-regexp item-end t)
          (let ((in-regexp? (save-match-data
                              (org-pos-in-regexp (match-beginning 0)
                                                 org-bracket-link-regexp 1))))
            (unless in-regexp?
              (incf match-count)))))
      (if (minusp number-to-hide)
          (setq number-to-hide (+ match-count number-to-hide)))
      (when (plusp match-count)
        (let* ((positions (shuffle-list (loop for i from 1
                                              to match-count
                                              collect i)))
               (match-nums nil)
               (cnt nil))
          (if force-hide-first
              ;; Force '1' to be in the list, and to be the first item
              ;; in the list.
              (setq positions (cons 1 (remove 1 positions))))
          (if force-show-first
              (setq positions (remove 1 positions)))
          (if force-show-last
              (setq positions (remove match-count positions)))
          (setq match-nums
                (subseq positions
                        0 (min number-to-hide (length positions))))
          ;; (dolist (pos-to-hide match-nums)
          (save-excursion
            (goto-char body-start)
            (setq cnt 0)
            (while (re-search-forward org-drill-cloze-regexp item-end t)
              (unless (save-match-data
                        (org-pos-in-regexp (match-beginning 0)
                                           org-bracket-link-regexp 1))
                (incf cnt)
                (if (memq cnt match-nums)
                    (org-drill-hide-matched-cloze-text)))))))
      ;; (loop
      ;;  do (re-search-forward org-drill-cloze-regexp
      ;;                        item-end t pos-to-hide)
      ;;  while (org-pos-in-regexp (match-beginning 0)
      ;;                           org-bracket-link-regexp 1))
      ;; (org-drill-hide-matched-cloze-text)))))
      (org-display-inline-images t)
      (org-cycle-hide-drawers 'all)
      (prog1 (org-drill-presentation-prompt)
        (org-drill-hide-subheadings-if 'org-drill-entry-p)
        (org-drill-unhide-clozed-text))))))


(defun org-drill-present-multicloze-hide-nth (to-hide)
  "Hide the TO-HIDE'th piece of clozed text. 1 is the first piece. If
TO-HIDE is negative, count backwards, so -1 means the last item, -2
the second to last, etc."
  (with-hidden-comments
   (with-hidden-cloze-hints
    (let ((item-end nil)
          (match-count 0)
          (body-start (or (cdr (org-get-property-block))
                          (point)))
          (cnt 0))
      (org-drill-hide-all-subheadings-except nil)
      (save-excursion
        (outline-next-heading)
        (setq item-end (point)))
      (save-excursion
        (goto-char body-start)
        (while (re-search-forward org-drill-cloze-regexp item-end t)
          (let ((in-regexp? (save-match-data
                              (org-pos-in-regexp (match-beginning 0)
                                                 org-bracket-link-regexp 1))))
            (unless in-regexp?
              (incf match-count)))))
      (if (minusp to-hide)
          (setq to-hide (+ 1 to-hide match-count)))
      (cond
       ((or (not (plusp match-count))
            (> to-hide match-count))
        nil)
       (t
        (save-excursion
          (goto-char body-start)
          (setq cnt 0)
          (while (re-search-forward org-drill-cloze-regexp item-end t)
            (unless (save-match-data
                      (org-pos-in-regexp (match-beginning 0)
                                         org-bracket-link-regexp 1))
              (incf cnt)
              (if (= cnt to-hide)
                  (org-drill-hide-matched-cloze-text)))))))
      (org-display-inline-images t)
      (org-cycle-hide-drawers 'all)
      (prog1 (org-drill-presentation-prompt)
        (org-drill-hide-subheadings-if 'org-drill-entry-p)
        (org-drill-unhide-clozed-text))))))


(defun org-drill-present-multicloze-hide1 ()
  "Hides one of the pieces of text that are marked for cloze deletion,
chosen at random."
  (org-drill-present-multicloze-hide-n 1))


(defun org-drill-present-multicloze-hide2 ()
  "Hides two of the pieces of text that are marked for cloze deletion,
chosen at random."
  (org-drill-present-multicloze-hide-n 2))


(defun org-drill-present-multicloze-hide-first ()
  "Hides the first piece of text that is marked for cloze deletion."
  (org-drill-present-multicloze-hide-nth 1))


(defun org-drill-present-multicloze-hide-last ()
  "Hides the last piece of text that is marked for cloze deletion."
  (org-drill-present-multicloze-hide-nth -1))


(defun org-drill-present-multicloze-hide1-firstmore ()
  "Commonly, hides the FIRST piece of text that is marked for
cloze deletion. Uncommonly, hide one of the other pieces of text,
chosen at random.

The definitions of 'commonly' and 'uncommonly' are determined by
the value of `org-drill-cloze-text-weight'."
  ;; The 'firstmore' and 'lastmore' functions used to randomly choose whether
  ;; to hide the 'favoured' piece of text. However even when the chance of
  ;; hiding it was set quite high (80%), the outcome was too unpredictable over
  ;; the small number of repetitions where most learning takes place for each
  ;; item. In other words, the actual frequency during the first 10 repetitions
  ;; was often very different from 80%. Hence we use modulo instead.
  (cond
   ((null org-drill-cloze-text-weight)
    ;; Behave as hide1cloze
    (org-drill-present-multicloze-hide1))
   ((not (and (integerp org-drill-cloze-text-weight)
              (plusp org-drill-cloze-text-weight)))
    (error "Illegal value for org-drill-cloze-text-weight: %S"
           org-drill-cloze-text-weight))
   ((zerop (mod (1+ (org-drill-entry-total-repeats 0))
                org-drill-cloze-text-weight))
    ;; Uncommonly, hide any item except the first
    (org-drill-present-multicloze-hide-n 1 t))
   (t
    ;; Commonly, hide first item
    (org-drill-present-multicloze-hide-first))))


(defun org-drill-present-multicloze-show1-lastmore ()
  "Commonly, hides all pieces except the last. Uncommonly, shows
any random piece. The effect is similar to 'show1cloze' except
that the last item is much less likely to be the item that is
visible.

The definitions of 'commonly' and 'uncommonly' are determined by
the value of `org-drill-cloze-text-weight'."
  (cond
   ((null org-drill-cloze-text-weight)
    ;; Behave as show1cloze
    (org-drill-present-multicloze-show1))
   ((not (and (integerp org-drill-cloze-text-weight)
              (plusp org-drill-cloze-text-weight)))
    (error "Illegal value for org-drill-cloze-text-weight: %S"
           org-drill-cloze-text-weight))
   ((zerop (mod (1+ (org-drill-entry-total-repeats 0))
                org-drill-cloze-text-weight))
    ;; Uncommonly, show any item except the last
    (org-drill-present-multicloze-hide-n -1 nil nil t))
   (t
    ;; Commonly, show the LAST item
    (org-drill-present-multicloze-hide-n -1 nil t))))


(defun org-drill-present-multicloze-show1-firstless ()
  "Commonly, hides all pieces except one, where the shown piece
is guaranteed NOT to be the first piece. Uncommonly, shows any
random piece. The effect is similar to 'show1cloze' except that
the first item is much less likely to be the item that is
visible.

The definitions of 'commonly' and 'uncommonly' are determined by
the value of `org-drill-cloze-text-weight'."
  (cond
   ((null org-drill-cloze-text-weight)
    ;; Behave as show1cloze
    (org-drill-present-multicloze-show1))
   ((not (and (integerp org-drill-cloze-text-weight)
              (plusp org-drill-cloze-text-weight)))
    (error "Illegal value for org-drill-cloze-text-weight: %S"
           org-drill-cloze-text-weight))
   ((zerop (mod (1+ (org-drill-entry-total-repeats 0))
                org-drill-cloze-text-weight))
    ;; Uncommonly, show the first item
    (org-drill-present-multicloze-hide-n -1 t))
   (t
    ;; Commonly, show any item, except the first
    (org-drill-present-multicloze-hide-n -1 nil nil t))))


(defun org-drill-present-multicloze-show1 ()
  "Similar to `org-drill-present-multicloze-hide1', but hides all
the pieces of text that are marked for cloze deletion, except for one
piece which is chosen at random."
  (org-drill-present-multicloze-hide-n -1))


(defun org-drill-present-multicloze-show2 ()
  "Similar to `org-drill-present-multicloze-show1', but reveals two
pieces rather than one."
  (org-drill-present-multicloze-hide-n -2))


;; (defun org-drill-present-multicloze-show1 ()
;;   "Similar to `org-drill-present-multicloze-hide1', but hides all
;; the pieces of text that are marked for cloze deletion, except for one
;; piece which is chosen at random."
;;   (with-hidden-comments
;;    (with-hidden-cloze-hints
;;     (let ((item-end nil)
;;           (match-count 0)
;;           (body-start (or (cdr (org-get-property-block))
;;                           (point))))
;;       (org-drill-hide-all-subheadings-except nil)
;;       (save-excursion
;;         (outline-next-heading)
;;         (setq item-end (point)))
;;       (save-excursion
;;         (goto-char body-start)
;;         (while (re-search-forward org-drill-cloze-regexp item-end t)
;;           (incf match-count)))
;;       (when (plusp match-count)
;;         (let ((match-to-hide (random* match-count)))
;;           (save-excursion
;;             (goto-char body-start)
;;             (dotimes (n match-count)
;;               (re-search-forward org-drill-cloze-regexp
;;                                  item-end t)
;;               (unless (= n match-to-hide)
;;                 (org-drill-hide-matched-cloze-text))))))
;;       (org-display-inline-images t)
;;       (org-cycle-hide-drawers 'all)
;;       (prog1 (org-drill-presentation-prompt)
;;         (org-drill-hide-subheadings-if 'org-drill-entry-p)
;;         (org-drill-unhide-clozed-text))))))


(defun org-drill-present-card-using-text (question &optional answer)
  "Present the string QUESTION as the only visible content of the card."
  (with-hidden-comments
   (with-replaced-entry-text
    question
    (org-drill-hide-all-subheadings-except nil)
    (org-cycle-hide-drawers 'all)
    (prog1 (org-drill-presentation-prompt)
      (org-drill-hide-subheadings-if 'org-drill-entry-p)))))


;;; The following macro is necessary because `org-save-outline-visibility'
;;; currently discards the value returned by its body and returns a garbage
;;; value instead. (as at org mode v 7.5)

(defmacro org-drill-save-visibility (&rest body)
  "Store the current visibility state of the org buffer, and restore it
after executing BODY. Return the value of the last expression
in BODY."
  (let ((retval (gensym)))
    `(let ((,retval nil))
       (org-save-outline-visibility t
         (setq ,retval
               (progn
                 ,@body)))
       ,retval)))


(defun org-drill-entry ()
  "Present the current topic for interactive review, as in `org-drill'.
Review will occur regardless of whether the topic is due for review or whether
it meets the definition of a 'review topic' used by `org-drill'.

Returns a quality rating from 0 to 5, or nil if the user quit, or the symbol
EDIT if the user chose to exit the drill and edit the current item. Choosing
the latter option leaves the drill session suspended; it can be resumed
later using `org-drill-resume'.

See `org-drill' for more details."
  (interactive)
  (org-drill-goto-drill-entry-heading)
  ;;(unless (org-part-of-drill-entry-p)
  ;;  (error "Point is not inside a drill entry"))
  ;;(unless (org-at-heading-p)
  ;;  (org-back-to-heading))
  (let ((card-type (org-entry-get (point) "DRILL_CARD_TYPE"))
        (answer-fn 'org-drill-present-default-answer)
        (cont nil))
    (org-drill-save-visibility
     (save-restriction
       (org-narrow-to-subtree)
       (org-show-subtree)
       (org-cycle-hide-drawers 'all)

       (let ((presentation-fn (cdr (assoc card-type org-drill-card-type-alist))))
         (if (listp presentation-fn)
             (psetq answer-fn (or (second presentation-fn)
                                  'org-drill-present-default-answer)
                    presentation-fn (first presentation-fn)))
         (cond
          ((null presentation-fn)
           (message "%s:%d: Unrecognised card type '%s', skipping..."
                    (buffer-name) (point) card-type)
           (sit-for 0.5)
           'skip)
          (t
           (setq cont (funcall presentation-fn))
           (cond
            ((not cont)
             (message "Quit")
             nil)
            ((eql cont 'edit)
             'edit)
            ((eql cont 'skip)
             'skip)
            (t
             (save-excursion
               (funcall answer-fn
                        (lambda () (org-drill-reschedule)))))))))))))


(defun org-drill-entries-pending-p ()
  (or *org-drill-again-entries*
      (and (not (org-drill-maximum-item-count-reached-p))
           (not (org-drill-maximum-duration-reached-p))
           (or *org-drill-new-entries*
               *org-drill-failed-entries*
               *org-drill-young-mature-entries*
               *org-drill-old-mature-entries*
               *org-drill-overdue-entries*
               *org-drill-again-entries*))))


(defun org-drill-pending-entry-count ()
  (+ (length *org-drill-new-entries*)
     (length *org-drill-failed-entries*)
     (length *org-drill-young-mature-entries*)
     (length *org-drill-old-mature-entries*)
     (length *org-drill-overdue-entries*)
     (length *org-drill-again-entries*)))


(defun org-drill-maximum-duration-reached-p ()
  "Returns true if the current drill session has continued past its
maximum duration."
  (and org-drill-maximum-duration
       *org-drill-start-time*
       (> (- (float-time (current-time)) *org-drill-start-time*)
          (* org-drill-maximum-duration 60))))


(defun org-drill-maximum-item-count-reached-p ()
  "Returns true if the current drill session has reached the
maximum number of items."
  (and org-drill-maximum-items-per-session
       (>= (length *org-drill-done-entries*)
           org-drill-maximum-items-per-session)))


(defun org-drill-pop-next-pending-entry ()
  (block org-drill-pop-next-pending-entry
    (let ((m nil))
      (while (or (null m)
                 (not (org-drill-entry-p m)))
        (setq
         m
         (cond
          ;; First priority is items we failed in a prior session.
          ((and *org-drill-failed-entries*
                (not (org-drill-maximum-item-count-reached-p))
                (not (org-drill-maximum-duration-reached-p)))
           (pop-random *org-drill-failed-entries*))
          ;; Next priority is overdue items.
          ((and *org-drill-overdue-entries*
                (not (org-drill-maximum-item-count-reached-p))
                (not (org-drill-maximum-duration-reached-p)))
           ;; We use `pop', not `pop-random', because we have already
           ;; sorted overdue items into a random order which takes
           ;; number of days overdue into account.
           (pop *org-drill-overdue-entries*))
          ;; Next priority is 'young' items.
          ((and *org-drill-young-mature-entries*
                (not (org-drill-maximum-item-count-reached-p))
                (not (org-drill-maximum-duration-reached-p)))
           (pop-random *org-drill-young-mature-entries*))
          ;; Next priority is newly added items, and older entries.
          ;; We pool these into a single group.
          ((and (or *org-drill-new-entries*
                    *org-drill-old-mature-entries*)
                (not (org-drill-maximum-item-count-reached-p))
                (not (org-drill-maximum-duration-reached-p)))
           (cond
            ((< (random* (+ (length *org-drill-new-entries*)
                            (length *org-drill-old-mature-entries*)))
                (length *org-drill-new-entries*))
             (pop-random *org-drill-new-entries*))
            (t
             (pop-random *org-drill-old-mature-entries*))))
          ;; After all the above are done, last priority is items
          ;; that were failed earlier THIS SESSION.
          (*org-drill-again-entries*
           (pop *org-drill-again-entries*))
          (t                            ; nothing left -- return nil
           (return-from org-drill-pop-next-pending-entry nil)))))
      m)))


(defun org-drill-entries (&optional resuming-p)
  "Returns nil, t, or a list of markers representing entries that were
'failed' and need to be presented again before the session ends.

RESUMING-P is true if we are resuming a suspended drill session."
  (block org-drill-entries
    (while (org-drill-entries-pending-p)
      (let ((m (cond
                ((or (not resuming-p)
                     (null *org-drill-current-item*)
                     (not (org-drill-entry-p *org-drill-current-item*)))
                 (org-drill-pop-next-pending-entry))
                (t                      ; resuming a suspended session.
                 (setq resuming-p nil)
                 *org-drill-current-item*))))
        (setq *org-drill-current-item* m)
        (unless m
          (error "Unexpectedly ran out of pending drill items"))
        (save-excursion
          (org-drill-goto-entry m)
          (cond
           ((not (org-drill-entry-due-p))
            ;; The entry is not due anymore. This could arise if the user
            ;; suspends a drill session, then drills an individual entry,
            ;; then resumes the session.
            (message "Entry no longer due, skipping...")
            (sit-for 0.3)
            nil)
           (t
            (setq result (org-drill-entry))
            (cond
             ((null result)
              (message "Quit")
              (setq end-pos :quit)
              (return-from org-drill-entries nil))
             ((eql result 'edit)
              (setq end-pos (point-marker))
              (return-from org-drill-entries nil))
             ((eql result 'skip)
              nil)                      ; skip this item
             (t
              (cond
               ((<= result org-drill-failure-quality)
                (if *org-drill-again-entries*
                    (setq *org-drill-again-entries*
                          (shuffle-list *org-drill-again-entries*)))
                (push-end m *org-drill-again-entries*))
               (t
                (push m *org-drill-done-entries*))))))))))))



(defun org-drill-final-report ()
  (let ((pass-percent
         (round (* 100 (count-if (lambda (qual)
                                   (> qual org-drill-failure-quality))
                                 *org-drill-session-qualities*))
                (max 1 (length *org-drill-session-qualities*))))
        (prompt nil))
    (setq prompt
          (format
           "%d items reviewed. Session duration %s.
Recall of reviewed items:
 Excellent (5):     %3d%%   |   Near miss (2):      %3d%%
 Good (4):          %3d%%   |   Failure (1):        %3d%%
 Hard (3):          %3d%%   |   Abject failure (0): %3d%%

You successfully recalled %d%% of reviewed items (quality > %s)
%d/%d items still await review (%s, %s, %s, %s, %s).
Tomorrow, %d more items will become due for review.
Session finished. Press a key to continue..."
           (length *org-drill-done-entries*)
           (format-seconds "%h:%.2m:%.2s"
                           (- (float-time (current-time)) *org-drill-start-time*))
           (round (* 100 (count 5 *org-drill-session-qualities*))
                  (max 1 (length *org-drill-session-qualities*)))
           (round (* 100 (count 2 *org-drill-session-qualities*))
                  (max 1 (length *org-drill-session-qualities*)))
           (round (* 100 (count 4 *org-drill-session-qualities*))
                  (max 1 (length *org-drill-session-qualities*)))
           (round (* 100 (count 1 *org-drill-session-qualities*))
                  (max 1 (length *org-drill-session-qualities*)))
           (round (* 100 (count 3 *org-drill-session-qualities*))
                  (max 1 (length *org-drill-session-qualities*)))
           (round (* 100 (count 0 *org-drill-session-qualities*))
                  (max 1 (length *org-drill-session-qualities*)))
           pass-percent
           org-drill-failure-quality
           (org-drill-pending-entry-count)
           (+ (org-drill-pending-entry-count)
              *org-drill-dormant-entry-count*)
           (propertize
            (format "%d failed"
                    (+ (length *org-drill-failed-entries*)
                       (length *org-drill-again-entries*)))
            'face `(:foreground ,org-drill-failed-count-color))
           (propertize
            (format "%d overdue"
                    (length *org-drill-overdue-entries*))
            'face `(:foreground ,org-drill-failed-count-color))
           (propertize
            (format "%d new"
                    (length *org-drill-new-entries*))
            'face `(:foreground ,org-drill-new-count-color))
           (propertize
            (format "%d young"
                    (length *org-drill-young-mature-entries*))
            'face `(:foreground ,org-drill-mature-count-color))
           (propertize
            (format "%d old"
                    (length *org-drill-old-mature-entries*))
            'face `(:foreground ,org-drill-mature-count-color))
           *org-drill-due-tomorrow-count*
           ))

    (while (not (input-pending-p))
      (message "%s" prompt)
      (sit-for 0.5))
    (read-char-exclusive)

    (if (and *org-drill-session-qualities*
             (< pass-percent (- 100 org-drill-forgetting-index)))
        (read-char-exclusive
         (format
          "%s
You failed %d%% of the items you reviewed during this session.
%d (%d%%) of all items scanned were overdue.

Are you keeping up with your items, and reviewing them
when they are scheduled? If so, you may want to consider
lowering the value of `org-drill-learn-fraction' slightly in
order to make items appear more frequently over time."
          (propertize "WARNING!" 'face 'org-warning)
          (- 100 pass-percent)
          *org-drill-overdue-entry-count*
          (round (* 100 *org-drill-overdue-entry-count*)
                 (+ *org-drill-dormant-entry-count*
                    *org-drill-due-entry-count*)))
         ))))



(defun org-drill-free-markers (markers)
  "MARKERS is a list of markers, all of which will be freed (set to
point nowhere). Alternatively, MARKERS can be 't', in which case
all the markers used by Org-Drill will be freed."
  (dolist (m (if (eql t markers)
                 (append  *org-drill-done-entries*
                          *org-drill-new-entries*
                          *org-drill-failed-entries*
                          *org-drill-again-entries*
                          *org-drill-overdue-entries*
                          *org-drill-young-mature-entries*
                          *org-drill-old-mature-entries*)
               markers))
    (free-marker m)))


(defun org-drill-order-overdue-entries (overdue-data)
  (setq *org-drill-overdue-entries*
        (mapcar 'car
                (sort (shuffle-list overdue-data)
                      (lambda (a b) (> (cdr a) (cdr b)))))))


(defun org-drill-entry-status ()
  "Returns a list (STATUS DUE) where DUE is the number of days overdue,
zero being due today, -1 being scheduled 1 day in the future. STATUS is
one of the following values:
- nil, if the item is not a drill entry, or has an empty body
- :unscheduled
- :future
- :new
- :failed
- :overdue
- :young
- :old
"
  (save-excursion
    (unless (org-at-heading-p)
      (org-back-to-heading))
    (let ((due (org-drill-entry-days-overdue))
          (last-int (org-drill-entry-last-interval 1)))
      (list
       (cond
        ((not (org-drill-entry-p))
         nil)
        ((org-drill-entry-empty-p)
         nil)                           ; skip -- item body is empty
        ((null due)                     ; unscheduled - usually a skipped leech
         :unscheduled)
        ;; ((eql -1 due)
        ;;  :tomorrow)
        ((minusp due)                   ; scheduled in the future
         :future)
        ;; The rest of the stati all denote 'due' items ==========================
        ((<= (org-drill-entry-last-quality 9999)
             org-drill-failure-quality)
         ;; Mature entries that were failed last time are
         ;; FAILED, regardless of how young, old or overdue
         ;; they are.
         :failed)
        ((org-drill-entry-new-p)
         :new)
        ((org-drill-entry-overdue-p due last-int)
         ;; Overdue status overrides young versus old
         ;; distinction.
         ;; Store marker + due, for sorting of overdue entries
         :overdue)
        ((<= (org-drill-entry-last-interval 9999)
             org-drill-days-before-old)
         :young)
        (t
         :old))
       due))))


(defun org-drill-progress-message (collected scanned)
  (when (zerop (% scanned 50))
    (let* ((meter-width 40)
           (sym1 (if (oddp (floor scanned (* 50 meter-width))) ?| ?.))
           (sym2 (if (eql sym1 ?.) ?| ?.)))
      (message "Collecting due drill items:%4d %s%s"
              collected
              (make-string (% (ceiling scanned 50) meter-width)
                           sym2)
              (make-string (- meter-width (% (ceiling scanned 50) meter-width))
                           sym1)))))


(defun org-drill (&optional scope resume-p)
  "Begin an interactive 'drill session'. The user is asked to
review a series of topics (headers). Each topic is initially
presented as a 'question', often with part of the topic content
hidden. The user attempts to recall the hidden information or
answer the question, then presses a key to reveal the answer. The
user then rates his or her recall or performance on that
topic. This rating information is used to reschedule the topic
for future review.

Org-drill proceeds by:

- Finding all topics (headings) in SCOPE which have either been
  used and rescheduled before, or which have a tag that matches
  `org-drill-question-tag'.

- All matching topics which are either unscheduled, or are
  scheduled for the current date or a date in the past, are
  considered to be candidates for the drill session.

- If `org-drill-maximum-items-per-session' is set, a random
  subset of these topics is presented. Otherwise, all of the
  eligible topics will be presented.

SCOPE determines the scope in which to search for
questions.  It accepts the same values as `org-drill-scope',
which see.

If RESUME-P is non-nil, resume a suspended drill session rather
than starting a new one."

  (interactive)
  (let ((end-pos nil)
        (overdue-data nil)
        (cnt 0))
    (block org-drill
      (unless resume-p
        (org-drill-free-markers t)
        (setq *org-drill-current-item* nil
              *org-drill-done-entries* nil
              *org-drill-dormant-entry-count* 0
              *org-drill-due-entry-count* 0
              *org-drill-due-tomorrow-count* 0
              *org-drill-overdue-entry-count* 0
              *org-drill-new-entries* nil
              *org-drill-overdue-entries* nil
              *org-drill-young-mature-entries* nil
              *org-drill-old-mature-entries* nil
              *org-drill-failed-entries* nil
              *org-drill-again-entries* nil)
        (setq *org-drill-session-qualities* nil)
        (setq *org-drill-start-time* (float-time (current-time))))
      (setq *random-state* (make-random-state t)) ; reseed RNG
      (unwind-protect
          (save-excursion
            (unless resume-p
              (let ((org-trust-scanner-tags t)
                    (warned-about-id-creation nil))
                (org-map-drill-entries
                 (lambda ()
                   (org-drill-progress-message
                              (+ (length *org-drill-new-entries*)
                                 (length *org-drill-overdue-entries*)
                                 (length *org-drill-young-mature-entries*)
                                 (length *org-drill-old-mature-entries*)
                                 (length *org-drill-failed-entries*))
                              (incf cnt))
                   (cond
                    ((not (org-drill-entry-p))
                     nil)               ; skip
                    (t
                     (when (and (not warned-about-id-creation)
                                (null (org-id-get)))
                       (message (concat "Creating unique IDs for items "
                                        "(slow, but only happens once)"))
                       (sit-for 0.5)
                       (setq warned-about-id-creation t))
                     (org-id-get-create) ; ensure drill entry has unique ID
                     (destructuring-bind (status due) (org-drill-entry-status)
                       (case status
                         (:unscheduled
                          (incf *org-drill-dormant-entry-count*))
                         ;; (:tomorrow
                         ;;  (incf *org-drill-dormant-entry-count*)
                         ;;  (incf *org-drill-due-tomorrow-count*))
                         (:future
                          (incf *org-drill-dormant-entry-count*)
                          (if (eq -1 due)
                              (incf *org-drill-due-tomorrow-count*)))
                         (:new
                          (push (point-marker) *org-drill-new-entries*))
                         (:failed
                          (push (point-marker) *org-drill-failed-entries*))
                         (:young
                          (push (point-marker) *org-drill-young-mature-entries*))
                         (:overdue
                          (push (cons (point-marker) due) overdue-data))
                         (:old
                          (push (point-marker) *org-drill-old-mature-entries*)))))))
                 scope)
                ;; (let ((due (org-drill-entry-days-overdue))
                ;;       (last-int (org-drill-entry-last-interval 1)))
                ;;   (cond
                ;;    ((org-drill-entry-empty-p)
                ;;     nil)           ; skip -- item body is empty
                ;;    ((or (null due) ; unscheduled - usually a skipped leech
                ;;         (minusp due)) ; scheduled in the future
                ;;     (incf *org-drill-dormant-entry-count*)
                ;;     (if (eq -1 due)
                ;;         (incf *org-drill-due-tomorrow-count*)))
                ;;    ((org-drill-entry-new-p)
                ;;     (push (point-marker) *org-drill-new-entries*))
                ;;    ((<= (org-drill-entry-last-quality 9999)
                ;;         org-drill-failure-quality)
                ;;     ;; Mature entries that were failed last time are
                ;;     ;; FAILED, regardless of how young, old or overdue
                ;;     ;; they are.
                ;;     (push (point-marker) *org-drill-failed-entries*))
                ;;    ((org-drill-entry-overdue-p due last-int)
                ;;     ;; Overdue status overrides young versus old
                ;;     ;; distinction.
                ;;     ;; Store marker + due, for sorting of overdue entries
                ;;     (push (cons (point-marker) due) overdue-data))
                ;;    ((<= (org-drill-entry-last-interval 9999)
                ;;         org-drill-days-before-old)
                ;;     ;; Item is 'young'.
                ;;     (push (point-marker)
                ;;           *org-drill-young-mature-entries*))
                ;;    (t
                ;;     (push (point-marker)
                ;;           *org-drill-old-mature-entries*))))
                ;; Order 'overdue' items so that the most overdue will tend to
                ;; come up for review first, while keeping exact order random
                (org-drill-order-overdue-entries overdue-data)
                (setq *org-drill-overdue-entry-count*
                      (length *org-drill-overdue-entries*))))
            (setq *org-drill-due-entry-count* (org-drill-pending-entry-count))
            (cond
             ((and (null *org-drill-new-entries*)
                   (null *org-drill-failed-entries*)
                   (null *org-drill-overdue-entries*)
                   (null *org-drill-young-mature-entries*)
                   (null *org-drill-old-mature-entries*))
              (message "I did not find any pending drill items."))
             (t
              (org-drill-entries resume-p)
              (message "Drill session finished!"))))
        (progn
          (unless end-pos
            (org-drill-free-markers *org-drill-done-entries*)))))
    (cond
     (end-pos
      (when (markerp end-pos)
        (org-drill-goto-entry end-pos))
      (let ((keystr (command-keybinding-to-string 'org-drill-resume)))
        (message
         "You can continue the drill session with the command `org-drill-resume'.%s"
         (if keystr (format "\nYou can run this command by pressing %s." keystr)
           ""))))
     (t
      (org-drill-final-report)
      (if (eql 'sm5 org-drill-spaced-repetition-algorithm)
          (org-drill-save-optimal-factor-matrix))
      (if org-drill-save-buffers-after-drill-sessions-p
          (save-some-buffers))
      (message "Drill session finished!")
      ))))


(defun org-drill-save-optimal-factor-matrix ()
  (message "Saving optimal factor matrix...")
  (customize-save-variable 'org-drill-optimal-factor-matrix
                           org-drill-optimal-factor-matrix))


(defun org-drill-cram (&optional scope)
  "Run an interactive drill session in 'cram mode'. In cram mode,
all drill items are considered to be due for review, unless they
have been reviewed within the last `org-drill-cram-hours'
hours."
  (interactive)
  (let ((*org-drill-cram-mode* t))
    (org-drill scope)))


(defun org-drill-tree ()
  "Run an interactive drill session using drill items within the
subtree at point."
  (interactive)
  (org-drill 'tree))


(defun org-drill-directory ()
  "Run an interactive drill session using drill items from all org
files in the same directory as the current file."
  (interactive)
  (org-drill 'directory))


(defun org-drill-again (&optional scope)
  "Run a new drill session, but try to use leftover due items that
were not reviewed during the last session, rather than scanning for
unreviewed items. If there are no leftover items in memory, a full
scan will be performed."
  (interactive)
  (cond
   ((plusp (org-drill-pending-entry-count))
    (org-drill-free-markers *org-drill-done-entries*)
    (if (markerp *org-drill-current-item*)
        (free-marker *org-drill-current-item*))
    (setq *org-drill-start-time* (float-time (current-time))
          *org-drill-done-entries* nil
          *org-drill-current-item* nil)
    (org-drill scope t))
   (t
    (org-drill scope))))



(defun org-drill-resume ()
  "Resume a suspended drill session. Sessions are suspended by
exiting them with the `edit' or `quit' options."
  (interactive)
  (cond
   ((org-drill-entries-pending-p)
    (org-drill nil t))
   ((and (plusp (org-drill-pending-entry-count))
         ;; Current drill session is finished, but there are still
         ;; more items which need to be reviewed.
         (y-or-n-p (format
                    "You have finished the drill session. However, %d items still
need reviewing. Start a new drill session? "
                    (org-drill-pending-entry-count))))
    (org-drill-again))
   (t
    (message "You have finished the drill session."))))


(defun org-drill-strip-entry-data ()
  (dolist (prop org-drill-scheduling-properties)
    (org-delete-property prop))
  (org-schedule t))


(defun org-drill-strip-all-data (&optional scope)
  "Delete scheduling data from every drill entry in scope. This
function may be useful if you want to give your collection of
entries to someone else.  Scope defaults to the current buffer,
and is specified by the argument SCOPE, which accepts the same
values as `org-drill-scope'."
  (interactive)
  (when (yes-or-no-p
         "Delete scheduling data from ALL items in scope: are you sure?")
    (cond
     ((null scope)
      ;; Scope is the current buffer. This means we can use
      ;; `org-delete-property-globally', which is faster.
      (dolist (prop org-drill-scheduling-properties)
        (org-delete-property-globally prop))
      (org-map-drill-entries (lambda () (org-schedule t)) scope))
     (t
      (org-map-drill-entries 'org-drill-strip-entry-data scope)))
    (message "Done.")))



(defun org-drill-add-cloze-fontification ()
  (when org-drill-use-visible-cloze-face-p
    (font-lock-add-keywords 'org-mode
                            org-drill-cloze-keywords
                            nil)))

(add-hook 'org-mode-hook 'org-drill-add-cloze-fontification)

(org-drill-add-cloze-fontification)


;;; Synching card collections =================================================


(defvar *org-drill-dest-id-table* (make-hash-table :test 'equal))


(defun org-drill-copy-entry-to-other-buffer (dest &optional path)
  "Copy the subtree at point to the buffer DEST. The copy will receive
the tag 'imported'."
  (block org-drill-copy-entry-to-other-buffer
    (save-excursion
      (let ((src (current-buffer))
            (m nil))
        (flet ((paste-tree-here (&optional level)
                                (org-paste-subtree level)
                                (org-drill-strip-entry-data)
                                (org-toggle-tag "imported" 'on)
                                (org-map-drill-entries
                                 (lambda ()
                                   (let ((id (org-id-get)))
                                     (org-drill-strip-entry-data)
                                     (unless (gethash id *org-drill-dest-id-table*)
                                       (puthash id (point-marker)
                                                *org-drill-dest-id-table*))))
                                 'tree)))
          (unless path
            (setq path (org-get-outline-path)))
          (org-copy-subtree)
          (org-pop-to-buffer-same-window dest)
          (setq m
                (condition-case nil
                    (org-find-olp path t)
                  (error                ; path does not exist in DEST
                   (return-from org-drill-copy-entry-to-other-buffer
                     (cond
                      ((cdr path)
                       (org-drill-copy-entry-to-other-buffer
                        dest (butlast path)))
                      (t
                       ;; We've looked all the way up the path
                       ;; Default to appending to the end of DEST
                       (goto-char (point-max))
                       (newline)
                       (paste-tree-here)))))))
          (goto-char m)
          (outline-next-heading)
          (newline)
          (forward-line -1)
          (paste-tree-here (1+ (or (org-current-level) 0)))
          )))))



(defun org-drill-merge-buffers (src &optional dest ignore-new-items-p)
  "SRC and DEST are two org mode buffers containing drill items.
For each drill item in DEST that shares an ID with an item in SRC,
overwrite scheduling data in DEST with data taken from the item in SRC.
This is intended for use when two people are sharing a set of drill items,
one person has made some updates to the item set, and the other person
wants to migrate to the updated set without losing their scheduling data.

By default, any drill items in SRC which do not exist in DEST are
copied into DEST. We attempt to place the copied item in the
equivalent location in DEST to its location in SRC, by matching
the heading hierarchy. However if IGNORE-NEW-ITEMS-P is non-nil,
we simply ignore any items that do not exist in DEST, and do not
copy them across."
  (interactive "bImport scheduling info from which buffer?")
  (unless dest
    (setq dest (current-buffer)))
  (setq src (get-buffer src)
        dest (get-buffer dest))
  (when (yes-or-no-p
         (format
          (concat "About to overwrite all scheduling data for drill items in `%s' "
                  "with information taken from matching items in `%s'. Proceed? ")
          (buffer-name dest) (buffer-name src)))
    ;; Compile list of all IDs in the destination buffer.
    (clrhash *org-drill-dest-id-table*)
    (with-current-buffer dest
      (org-map-drill-entries
       (lambda ()
         (let ((this-id (org-id-get)))
           (when this-id
             (puthash this-id (point-marker) *org-drill-dest-id-table*))))
       'file))
    ;; Look through all entries in source buffer.
    (with-current-buffer src
      (org-map-drill-entries
       (lambda ()
         (let ((id (org-id-get))
               (last-quality nil) (last-reviewed nil)
               (scheduled-time nil))
           (cond
            ((or (null id)
                 (not (org-drill-entry-p)))
             nil)
            ((gethash id *org-drill-dest-id-table*)
             ;; This entry matches an entry in dest. Retrieve all its
             ;; scheduling data, then go to the matching location in dest
             ;; and write the data.
             (let ((marker (gethash id *org-drill-dest-id-table*)))
               (destructuring-bind (last-interval repetitions failures
                                                  total-repeats meanq ease)
                   (org-drill-get-item-data)
                 (setq last-reviewed (org-entry-get (point) "DRILL_LAST_REVIEWED")
                       last-quality (org-entry-get (point) "DRILL_LAST_QUALITY")
                       scheduled-time (org-get-scheduled-time (point)))
                 (save-excursion
                   ;; go to matching entry in destination buffer
                   (org-pop-to-buffer-same-window (marker-buffer marker))
                   (goto-char marker)
                   (org-drill-strip-entry-data)
                   (unless (zerop total-repeats)
                     (org-drill-store-item-data last-interval repetitions failures
                                                total-repeats meanq ease)
                     (if last-quality
                         (org-set-property "LAST_QUALITY" last-quality)
                       (org-delete-property "LAST_QUALITY"))
                     (if last-reviewed
                         (org-set-property "LAST_REVIEWED" last-reviewed)
                       (org-delete-property "LAST_REVIEWED"))
                     (if scheduled-time
                         (org-schedule nil scheduled-time)))))
               (remhash id *org-drill-dest-id-table*)
               (free-marker marker)))
            (t
             ;; item in SRC has ID, but no matching ID in DEST.
             ;; It must be a new item that does not exist in DEST.
             ;; Copy the entire item to the *end* of DEST.
             (unless ignore-new-items-p
               (org-drill-copy-entry-to-other-buffer dest))))))
       'file))
    ;; Finally: there may be some items in DEST which are not in SRC, and
    ;; which have been scheduled by another user of DEST. Clear out the
    ;; scheduling info from all the unmatched items in DEST.
    (with-current-buffer dest
      (maphash (lambda (id m)
                 (goto-char m)
                 (org-drill-strip-entry-data)
                 (free-marker m))
               *org-drill-dest-id-table*))))



;;; Card types for learning languages =========================================

;;; Get spell-number.el from:
;;; http://www.emacswiki.org/emacs/spell-number.el
(autoload 'spelln-integer-in-words "spell-number")


;;; `conjugate' card type =====================================================
;;; See spanish.org for usage

(defvar org-drill-verb-tense-alist
  '(("present" "tomato")
    ("simple present" "tomato")
    ("present indicative" "tomato")
    ;; past tenses
    ("past" "purple")
    ("simple past" "purple")
    ("preterite" "purple")
    ("imperfect" "darkturquoise")
    ("present perfect" "royalblue")
    ;; future tenses
    ("future" "green"))
  "Alist where each entry has the form (TENSE COLOUR), where
TENSE is a string naming a tense in which verbs can be
conjugated, and COLOUR is a string specifying a foreground colour
which will be used by `org-drill-present-verb-conjugation' and
`org-drill-show-answer-verb-conjugation' to fontify the verb and
the name of the tense.")


(defun org-drill-get-verb-conjugation-info ()
  "Auxiliary function used by `org-drill-present-verb-conjugation' and
`org-drill-show-answer-verb-conjugation'."
  (let ((infinitive (org-entry-get (point) "VERB_INFINITIVE" t))
        (inf-hint (org-entry-get (point) "VERB_INFINITIVE_HINT" t))
        (translation (org-entry-get (point) "VERB_TRANSLATION" t))
        (tense (org-entry-get (point) "VERB_TENSE" nil))
        (highlight-face nil))
    (unless (and infinitive translation tense)
      (error "Missing information for verb conjugation card (%s, %s, %s) at %s"
             infinitive translation tense (point)))
    (setq tense (downcase (car (read-from-string tense)))
          infinitive (car (read-from-string infinitive))
          inf-hint (if inf-hint (car (read-from-string inf-hint)))
          translation (car (read-from-string translation)))
    (setq highlight-face
          (list :foreground
                (or (second (assoc-string tense org-drill-verb-tense-alist t))
                    "red")))
    (setq infinitive (propertize infinitive 'face highlight-face))
    (setq translation (propertize translation 'face highlight-face))
    (setq tense (propertize tense 'face highlight-face))
    (list infinitive inf-hint translation tense)))


(defun org-drill-present-verb-conjugation ()
  "Present a drill entry whose card type is 'conjugate'."
  (destructuring-bind (infinitive inf-hint translation tense)
      (org-drill-get-verb-conjugation-info)
    (org-drill-present-card-using-text
     (cond
      ((zerop (random* 2))
       (format "\nTranslate the verb\n\n%s\n\nand conjugate for the %s tense.\n\n"
               infinitive tense))
      (t
       (format "\nGive the verb that means\n\n%s %s\n
and conjugate for the %s tense.\n\n"
               translation
               (if inf-hint (format "  [HINT: %s]" inf-hint) "")
               tense))))))


(defun org-drill-show-answer-verb-conjugation (reschedule-fn)
  "Show the answer for a drill item whose card type is 'conjugate'.
RESCHEDULE-FN must be a function that calls `org-drill-reschedule' and
returns its return value."
  (destructuring-bind (infinitive inf-hint translation tense)
      (org-drill-get-verb-conjugation-info)
    (with-replaced-entry-heading
     (format "%s tense of %s ==> %s\n\n"
             (capitalize tense)
             infinitive translation)
     (funcall reschedule-fn))))


;;; `translate_number' card type ==============================================
;;; See spanish.org for usage

(defvar *drilled-number* 0)
(defvar *drilled-number-direction* 'to-english)

(defun org-drill-present-translate-number ()
  (let ((num-min (read (org-entry-get (point) "DRILL_NUMBER_MIN")))
        (num-max (read (org-entry-get (point) "DRILL_NUMBER_MAX")))
        (language (read (org-entry-get (point) "DRILL_LANGUAGE" t)))
        (highlight-face 'font-lock-warning-face))
    (cond
     ((not (fboundp 'spelln-integer-in-words))
      (message "`spell-number.el' not loaded, skipping 'translate_number' card...")
      (sit-for 0.5)
      'skip)
     ((not (and (numberp num-min) (numberp num-max) language))
      (error "Missing language or minimum or maximum numbers for number card"))
     (t
      (if (> num-min num-max)
          (psetf num-min num-max
                 num-max num-min))
      (setq *drilled-number*
            (+ num-min (random* (abs (1+ (- num-max num-min))))))
      (setq *drilled-number-direction*
            (if (zerop (random* 2)) 'from-english 'to-english))
      (org-drill-present-card-using-text
       (if (eql 'to-english *drilled-number-direction*)
           (format "\nTranslate into English:\n\n%s\n"
                   (let ((spelln-language language))
                     (propertize
                      (spelln-integer-in-words *drilled-number*)
                      'face highlight-face)))
         (format "\nTranslate into %s:\n\n%s\n"
                 (capitalize (format "%s" language))
                 (let ((spelln-language 'english-gb))
                   (propertize
                    (spelln-integer-in-words *drilled-number*)
                    'face highlight-face)))))))))


(defun org-drill-show-answer-translate-number (reschedule-fn)
  (let* ((language (read (org-entry-get (point) "DRILL_LANGUAGE" t)))
         (highlight-face 'font-lock-warning-face)
         (non-english
          (let ((spelln-language language))
            (propertize (spelln-integer-in-words *drilled-number*)
                        'face highlight-face)))
         (english
          (let ((spelln-language 'english-gb))
            (propertize (spelln-integer-in-words *drilled-number*)
                        'face 'highlight-face))))
    (with-replaced-entry-text
     (cond
      ((eql 'to-english *drilled-number-direction*)
       (format "\nThe English translation of %s is:\n\n%s\n"
               non-english english))
      (t
       (format "\nThe %s translation of %s is:\n\n%s\n"
               (capitalize (format "%s" language))
               english non-english)))
     (funcall reschedule-fn))))


;;; `spanish_verb' card type ==================================================
;;; Not very interesting, but included to demonstrate how a presentation
;;; function can manipulate which subheading are hidden versus shown.


(defun org-drill-present-spanish-verb ()
  (let ((prompt nil)
        (reveal-headings nil))
    (with-hidden-comments
     (with-hidden-cloze-hints
      (with-hidden-cloze-text
       (case (random* 6)
         (0
          (org-drill-hide-all-subheadings-except '("Infinitive"))
          (setq prompt
                (concat "Translate this Spanish verb, and conjugate it "
                        "for the *present* tense.")
                reveal-headings '("English" "Present Tense" "Notes")))
         (1
          (org-drill-hide-all-subheadings-except '("English"))
          (setq prompt (concat "For the *present* tense, conjugate the "
                               "Spanish translation of this English verb.")
                reveal-headings '("Infinitive" "Present Tense" "Notes")))
         (2
          (org-drill-hide-all-subheadings-except '("Infinitive"))
          (setq prompt (concat "Translate this Spanish verb, and "
                               "conjugate it for the *past* tense.")
                reveal-headings '("English" "Past Tense" "Notes")))
         (3
          (org-drill-hide-all-subheadings-except '("English"))
          (setq prompt (concat "For the *past* tense, conjugate the "
                               "Spanish translation of this English verb.")
                reveal-headings '("Infinitive" "Past Tense" "Notes")))
         (4
          (org-drill-hide-all-subheadings-except '("Infinitive"))
          (setq prompt (concat "Translate this Spanish verb, and "
                               "conjugate it for the *future perfect* tense.")
                reveal-headings '("English" "Future Perfect Tense" "Notes")))
         (5
          (org-drill-hide-all-subheadings-except '("English"))
          (setq prompt (concat "For the *future perfect* tense, conjugate the "
                               "Spanish translation of this English verb.")
                reveal-headings '("Infinitive" "Future Perfect Tense" "Notes"))))
       (org-cycle-hide-drawers 'all)
       (prog1 (org-drill-presentation-prompt)
         (org-drill-hide-subheadings-if 'org-drill-entry-p)))))))


(provide 'org-drill)
