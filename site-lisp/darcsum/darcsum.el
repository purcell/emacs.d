;;; darcsum.el --- a pcl-cvs like interface for managing darcs patches

;; Copyright (C) 2004  John Wiegley
;; Copyright (C) 2005  Christian Neukirchen
;; Copyright (C) 2005  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Of this fork: Christian Neukirchen <chneukirchen@gmail.com>
;; Keywords: completion convenience tools vc
;; Version: 1.10-chris
;; location: http://www.newartisans.com/johnw/emacs.html
;;           http://chneukirchen.org/repos/darcsum

;; This file is not yet part of GNU Emacs.

;; This module is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This module is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Load this file and run M-x darcsum-whatsnew.  This will display a
;; pcl-cvs like buffer showing modified files.  RET on a file reveals
;; changes; RET on a directory reveals changes to its files.
;;
;; Displayed changes may be recorded with "c", which offers a buffer
;; for inputting the change name (first line) and long description
;; (subsequent lines).  C-c C-c records the patch.
;;
;; If you only want to record a part of your changes, you need to mark
;; those.  If a change is "marked" in the summary buffer with "m"
;; (done on the change, the file (all changes) or the directory (all
;; changes in all files)), only marked changes are recorded,
;; regardless of point.
;;
;; Alternatively, if no changes are marked, then only visible changes
;; are recorded.
;;
;; Move changes between buffers with "M", which prompts for a darcsum
;; buffer to move to (creating one if the buffer doesn't exist).
;;
;; "g" forgets everything and resubmits the "whatsnew" command.
;; Collapsing a file forgets all marks for that file.  Only displayed
;; changes are ever recorded!
;;
;; "n" and "p" move among files.  "q" kills the buffer.

;; TODO (Patches are welcome!):

;; - When merging changesets, check the content of change text too
;; - Better support for moving files
;; - use --interactive with apply, for applying patches from e-mail
;;   via darcsum
;; - Better logfile handling
;; - Interface to darcs replace
;; - Interface to darcs unrecord

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'add-log))

;; Attempt to handle older/other emacs in XEmacs way.
;; If `line-beginning-position' isn't available, use point-at-bol.
(unless (fboundp 'line-beginning-position)
  (defalias 'line-beginning-position 'point-at-bol))

(defgroup darcsum nil
  "Special support for the Darcs versioning system."
;;  :version "21.4"
  :group 'tools
  :prefix "darcsum-")

(defvar darcsum-data nil)
(defvar darcsum-look-for-adds nil)
(defvar darcsum-show-context nil)
(defvar darcsum-pre-ediff-window-configuration nil)
(defvar darcsum-subdirectory ".")

;; Make buffer-local variable storing old window configuration,
;; since "let" bindings die before ediff buffers are killed
(make-variable-buffer-local 'darcsum-pre-ediff-window-configuration)

(defface darcsum-header-face
  '((((class color) (background dark))
     (:foreground "lightyellow" :bold t))
    (((class color) (background light))
     (:foreground "blue4" :bold t))
    (t (:bold t)))
  "Face used to highlight directory changes."
  :group 'darcsum)

(defface darcsum-marked-face
  '((t (:bold t)))
  "Face used to highlight marked changes."
  :group 'darcsum)

(defface darcsum-need-action-face
  '((((class color) (background dark))
     (:foreground "orange"))
    (((class color) (background light))
     (:foreground "orange"))
    (t (:italic t)))
  ""
  :group 'darcsum)

(defface darcsum-need-action-marked-face
  '((((class color) (background dark))
     (:foreground "orange" :bold t))
    (((class color) (background light))
     (:foreground "orange" :bold t))
    (t (:italic t :bold t)))
  ""
  :group 'darcsum)

(defface darcsum-filename-face
  '((((class color) (background dark))
     (:foreground "lightblue"))
    (((class color) (background light))
     (:foreground "blue4"))
    (t ()))
  "Face used to highlight file names."
  :group 'darcsum)

(defface darcsum-change-line-face
  '((((class color) (background dark))
     (:foreground "grey75" :background "grey25"))
    (((class color) (background light))
     (:foreground "grey25" :background "grey75"))
    (t (:bold t)))
  "Face used to highlight file names."
  :group 'darcsum)

(defface darcsum-whitespace-ateol-face
  '((((class color) (background dark))
     (:background "red4"))
    (((class color) (background light))
     (:background "red1")))
  "Face used to highlight whitespace at end of line."
  :group 'darcsum)

(defun darcsum-add-props (str &rest props)
  (add-text-properties 0 (1- (length str)) (list* props) str)
  str)

(defun darcsum-add-face (str face &optional keymap &rest props)
  (when keymap
    (when (keymapp keymap)
      (setq props (list* 'keymap keymap props)))
    (setq props (list* 'mouse-face 'highlight props)))
  (add-text-properties 0 (length str) (list* 'face face props) str)
  str)

;;; Code to work with changesets

;; A changeset is an alist of the following form:
;;
;;   ((PATH (TYPE SELECTED CONTENT...))))
;;
;; where PATH is plain string, but TYPE is of the following
;; possible formats:
;;
;;   LINE     An integer giving the first line of the hunk
;;   SYMBOL   Non-hunk change: 'addfile 'newfile 'rmfile 'binary or 'replace
;;
;; SELECTED is a list of flags, 'mark or 'hide symbols.
;;
;; Each CONTENT is a string which represents a modification to make to the
;; file after the starting line. For hunks, each change begins with either a
;; "+" or "-" to indicate if the line should be removed or added to the
;; file.
;;
;; So, for example, in a buffer with changes visible in report.cc visible
;; and changes in report.h marked:
;;
;; (("./TODO" (addfile (hide)))
;;  ("./report.cc"
;;    (replace nil "[A-Za-z_0-9] indented intended")
;;    (606 nil "-    blah" "+    blah" "+    blah")
;;    (620 nil "-    blah" "+    blah" "+    blah")
;;    (629 nil "-    blah" "+    blah" "+    blah")
;;    (634 nil "-    blah" "+    blah" "+    blah")
;;    (641 nil "-    blah" "+    blah" "+    blah")
;;    (652 nil "-    blah" "+    blah" "+    blah")
;;    (664 nil "-    blah" "+    blah" "+    blah"))
;;  ("./report.h"
;;    (115 (mark) "-    blah" "+    blah" "+    blah")
;;    (126 (mark) "+"))))
;;

(defun darcsum-change-add-flag (change flag)
  "Add FLAG on CHANGE."
  (if (not (memq flag (cadr change)))
      (setcar (cdr change) (cons flag (cadr change)))))

(defun darcsum-change-remove-flag (change flag)
  "Remove FLAG on CHANGE."
  (if (memq flag (cadr change))
      (setcar (cdr change) (delq flag (cadr change)))))

(defun darcsum-change-remove-all-flags (change)
  "Remove all flags on CHANGE."
  (setcar (cdr change) nil))

(defun darcsum-change-toggle-flag (change flag)
  "Toggle FLAG on CHANGE."
  (if (memq flag (cadr change))
      (setcar (cdr change) (delq flag (cadr change)))
    (setcar (cdr change) (cons flag (cadr change)))))

(defun darcsum-change-mark-p (change)
  "Return mark if CHANGE is marked."
  (not (null (memq 'mark (cadr change)))))

(defun darcsum-change-unmark-p (change)
  "Return mark if CHANGE is not marked."
  (null (memq 'mark (cadr change))))

(defun darcsum-change-toggle-mark (change)
  "Toggle mark flag on CHANGE."
  (darcsum-change-toggle-flag change 'mark))

(defun darcsum-change-add-mark (change)
  "Add mark flag on CHANGE."
  (darcsum-change-add-flag change 'mark))

(defun darcsum-change-remove-mark (change)
  "Remove mark flag on CHANGE."
  (darcsum-change-remove-flag change 'mark))

(defun darcsum-change-visible-p (change)
  "Return t if CHANGE is visible."
  (not (memq 'hide (cadr change))))

(defun darcsum-change-toggle-hide (change)
  "Toggle hide flag on CHANGE."
  (darcsum-change-toggle-flag change 'hide))

(defun darcsum-change-add-hide (change)
  "Add hide flag on CHANGE."
  (darcsum-change-add-flag change 'hide))

(defun darcsum-change-remove-hide (change)
  "Add hide flag on CHANGE."
  (darcsum-change-remove-flag change 'hide))

(defun darcsum-changeset-any-p (changeset predicate)
  "Return t if PREDICATE is true for any change in CHANGESET."
  (catch 'exit
    (ignore
     (let (file change)
       (dolist (file changeset)
	 (dolist (change (cdr file))
	   (if (funcall predicate change)
	       (throw 'exit t))))))))

(defsubst darcsum-changeset-any-marked-p (changeset)
  "Return t if CHANGESET has change(s) which have been marked."
  (darcsum-changeset-any-p changeset (function darcsum-change-mark-p)))

(defsubst darcsum-changeset-any-unmarked-p (changeset)
  "Return t if CHANGESET has change(s) which have not been marked."
  (darcsum-changeset-any-p changeset (function darcsum-change-unmark-p)))

(defsubst darcsum-changeset-any-visible-p (changeset)
  "Return t if CHANGESET has change(s) which are visible."
  (darcsum-changeset-any-p changeset (function darcsum-change-visible-p)))

(defun darcsum-changeset-all-p (changeset predicate)
  "Return t if PREDICATE is true for all change in CHANGESET."
  (not (catch 'exit
	 (ignore
	  (let (file change)
	    (dolist (file changeset)
	      (dolist (change (cdr file))
		(if (not (funcall predicate change))
		    (throw 'exit t)))))))))

(defsubst darcsum-changeset-all-marked-p (changeset)
  "Return t if all changes in CHANGESET have been marked."
  (darcsum-changeset-all-p changeset (function darcsum-change-mark-p)))

(defsubst darcsum-changeset-all-visible-p (changeset)
  "Return t if all changes in CHANGESET are visible."
  (darcsum-changeset-all-p changeset (function darcsum-change-visible-p)))

(defun darcsum-changeset-find (changeset predicate)
  "Return changes selected by PREDICATE from CHANGESET."
  (let (file change found)
    (dolist (file changeset)
      (let (changes)
	(dolist (change (cdr file))
	  (if (funcall predicate change)
	      (setq changes (cons change changes))))
	(if changes
	    (setq changes (cons (car file) (nreverse changes))
		  found (cons changes found)))))
    (nreverse found)))

(defun darcsum-changeset-find-visible (changeset)
  "Return visible changes from CHANGESET."
  (darcsum-changeset-find changeset (function darcsum-change-visible-p)))

(defun darcsum-changeset-find-marked (changeset)
  "Return marked changes from CHANGESET."
  (darcsum-changeset-find changeset (function darcsum-change-mark-p)))

(defsubst darcsum-find-change (changeset file line content)
  ;; Return change in CHANGESET with matching FILE, LINE and CONTENT.
  ;; If CONTENT is 'any, it is ignored.
  (let ((change (assoc line (assoc file changeset))))
    (if (or (eq content 'any)
	    (equal (caddr change) content))
	change)))

(defconst darcsum-file-change-status-alist
  '((addfile . "Added")
    (adddir . "Added directory")
    (newfile . "New")
    (newdir . "New directory")
    (rmfile . "Removed")
    (rmdir . "Removed directory")
    (binary . "Modified binary")))

(defun darcsum-file-change-status (change)
  "Return file-change-status displayed with CHANGE."
  (cdr (assq (car change) darcsum-file-change-status-alist)))

(defun darcsum-make-temp-file (&optional template)
  "Create temporary file.  Optional argument TEMPLATE sets the base name.

The template, if present, is passed to `expand-file-name' to construct a
fully qualified base name.  If absent, the string \"_darcs\" is used.

The function `make-temp-file' is preferred, but if it is not available,
`make-temp-name' is used as a fallback."
  (unless template
    (setq template "darcsum"))
  (unless (file-name-absolute-p template)
    (setq template (expand-file-name template "_darcs")))
  (if (fboundp 'make-temp-file)
      (make-temp-file template)
    ;; make-temp-name generates a unique name when it is called, but
    ;; takes no provisions to ensure that it will remain unique. Thus,
    ;; there is a race condition before we use the name. This is
    ;; probably a bad thing.
    (make-temp-name template)))

(defun darcsum-changeset-has-directory-p (changeset dir)
  (and (assoc dir changeset) t))

(defun darcsum-apply-to-changes (data func)
  (let (file change)
    (dolist (file data)
      (dolist (change (cdr file))
	(funcall func change)))))

(defun darcsum-remove-changeset (changeset remove)
  "Remove REMOVE from the CHANGESET."
  (let (file change)
    (dolist (file remove)
      (let ((fentry (assoc (car file) changeset)))
	(dolist (change (cdr file))
	  (setcdr fentry (delete (assoc (car change) (cdr fentry))
				 (cdr fentry))))
	(unless (cdr fentry)
	  (setq changeset (delete fentry changeset))))))
  changeset)

(defconst darcsum-item-numeric-alist
  '((move . -2)
    (addfile . -1)
    (adddir . -1)
    (newfile . -1)
    (newdir . -1)
    (rmfile . -1)
    (rmdir . -1)
    (binary . 0)
    (replace . 0)))

(defun darcsum-change-< (l r)
  (setq l (car l)
	r (car r))
  (< (if (numberp l) l (or (cdr (assq l darcsum-item-numeric-alist)) 0))
     (if (numberp r) r (or (cdr (assq r darcsum-item-numeric-alist)) 0))))

(defun darcsum-add-changeset (changeset add)
  "Add ADD to CHANGESET."
  (let (file fentry change)
    (dolist (file add)
      (if (setq fentry (assoc (car file) changeset))
	  (progn
	    (dolist (change (cdr file))
	      (unless (member change (cdr fentry))
		(nconc fentry (list change))))
	    (setcdr fentry (sort (cdr fentry) (function darcsum-change-<))))
	(setq changeset (cons file changeset)))))
  (sort changeset))

(defun darcsum-merge-changeset (data changeset)
  "Merge CHANGESET into the DATA.

Currently this simply moves 'mark and 'hide from DATA to CHANGESET."
  ;;;;;;; TODO: commute new patches
  ;;;;;;; (iow, behave properly if lines are added or deleted)
  (let (file data-file change data-change)
    (dolist (file changeset)
      (if (setq data-file (assoc (car file) data))
	  (dolist (change (cdr file))
	    (let ((data-change (assoc (car change) data-file))
		  (item (car data-change)))
	      (if (cond
		   ((null item))
		   ((eq item 'replace) (equal (cddr change) (cddr data-change)))
		   ((numberp item) (darcsum-hunk-match (cddr change) (cddr data-change)))
		   (t t))
		  (setcar (cdr change) (car (cdr data-change)))))))))
  changeset)

(defun darcsum-hunk-match (a b)
  "Return t if hunks in A and B match (modify same lines)."
  (if (equal a b)
      t
    (while (string-match "^ " (car a)) (setq a (cdr a)))
    (while (string-match "^ " (car b)) (setq b (cdr b)))
    (while (and a b (string= (car a) (car b)))
      (setq a (cdr a) b (cdr b)))
    (if (or (null a) (null b)
	    (string-match "^[+]" (car a))
	    (string-match "^[+]" (car b)))
	t)))

(defun darcsum-parse-changeset (&optional pending visible)
  "Return the patch in the current buffer as a Lisp changeset."
  (when (looking-at "^{")
    (forward-line))
  (let ((limit (* 10 (count-lines (point-min) (point-max))))
        data change entry)
    (while (and (not (or (eobp) (looking-at "^}")))
                (> limit 0))
      (setq limit (1- limit))
      (cond
       ((looking-at "^\\(addfile\\|adddir\\|rmdir\\|move\\|binary\\|rmfile\\|hunk\\|replace\\)\\s-+\\(.+?\\)\\(\\s-+\\([0-9]+\\|.+\\)\\)?$")
        (forward-line)
        (let* ((item (intern (match-string 1)))
               (path (match-string 2))
               (extra (match-string 4))
               lines)
	  ;; (message (concat "Looking at " (match-string 1)))
          (case item
	    ('hunk
	     (while (looking-at "^\\([+ -].*\\)")
	       (setq lines (cons (match-string 1) lines))
	       (forward-line))
	     (setq item (string-to-number extra)
		   lines (nreverse lines)))
	    ('binary
	     (while (looking-at "^\\(old\\|new\\)hex$")
	       (forward-line)
	       (while (looking-at "^\\*")
		 (forward-line))))
	    ('addfile
	     (if (and (not (eq pending t))
		      (null (assoc path pending)))
		 (setq item 'newfile)))
	    ('adddir
	     (if (and (not (eq pending t))
		      (null (assoc path pending)))
		 (setq item 'newdir)))
	    ('move
	     (setq lines (list extra)))
	    ('replace
	     (setq lines (list extra))))
	  (setq change (cons item (cons (if visible nil (list 'hide)) lines))
		fentry (assoc path data))
	  (if (null fentry)
	      (setq data (cons (cons path (list change)) data))
	    ;; (message path)
	    (setcdr fentry (cons change (cdr fentry))))))
       ))
    (assert (>= limit 0))
    (dolist (entry data)
      (setcdr entry (sort (cdr entry) (function darcsum-change-<))))
    (nreverse data)))

(defun darcsum-read-changeset (&optional visible)
  (let ((pending
         (if (file-readable-p "_darcs/patches/pending")
             (with-temp-buffer
               (insert-file-contents "_darcs/patches/pending")
               (darcsum-parse-changeset t visible)))))
    (goto-char (point-min))
    (when (looking-at "^What's new in \"\\([^\"]*\\)\":")
      (forward-line 2))
    (unless (looking-at "^$")
      (darcsum-parse-changeset pending visible))))

(defun darcsum-display-changeset (data)
  "Display the changeset DATA using a pcl-cvs-like buffer."
  ;; Lines starting with number indicates start of hunk
  ;; Lines starting with "in directory" indicate directory
  ;; Lines starting with \t indicate non-line change
  (erase-buffer)
  ;;(when (file-readable-p "_darcs/prefs/lastrepo")
  ;;  (insert "repository : ")
  ;;  (insert-file-contents "_darcs/prefs/lastrepo")
  ;;  (goto-char (point-max)))
  (insert "Working dir: " default-directory "\n\n\n")
  (unless data
    (insert "There are no changes to review.\n"))
  (let (dir sorted dentry file path status change changes line beg)
    (dolist (file data)
      (setq path (car file)
	    dir (if (memq (caadr file) '(adddir rmdir newdir)) path
		  (directory-file-name (file-name-directory path)))
	    dentry (assoc dir sorted))
      (if dentry
	  (setcdr dentry (cons file (cdr dentry)))
	(setq sorted (cons (cons dir (list file)) sorted))))
    (setq sorted (sort sorted (function (lambda (a b)
					  (string-lessp (car a) (car b))))))
    (dolist (dentry sorted)
      (setq dir (car dentry)
	    data (nreverse (cdr dentry))
	    beg (point))
      (insert "in directory "
	      (darcsum-add-face dir 'darcsum-header-face t)
	      ":\n")
      (add-text-properties beg (point)
			   (list
			    'darcsum-line-type 'dir
			    'darcsum-line-path dir
			    'darcsum-line-change data))
      (dolist (file data)
	(setq path (car file)
	      changes (cdr file)
	      status nil)
	(while changes
	  (setq change (car changes)
		item (car change)
		marked (darcsum-change-mark-p change)
		visible (darcsum-change-visible-p change)
		beg (point))
	  (cond
	   ((eq item 'move)
	    (darcsum-insert-file-line "Moved" path " -> " visible marked)
	    (insert (darcsum-add-face (caddr change) 'darcsum-filename-face t)
		    ?\n))
	   ((memq item '(addfile adddir rmfile rmdir newfile newdir binary))
	    (setq status (darcsum-file-change-status change))
	    (darcsum-insert-file-line status path "\n"
				      visible marked 'file changes)
	    (setq changes nil		; don't show other changes
		  beg (point)))
	   ((eq item 'replace)
	    (unless status
	      (setq status (darcsum-insert-file-line "Modified" path "\n"
						     nil marked 'file changes)
		    beg (point)))
	    (if visible
		(insert "\t      "
			(if marked
			    (darcsum-add-face
			     (format "%24s %s" "replace " (caddr change))
			     'darcsum-marked-face t)
			  (format "%24s %s" "replace " (caddr change)))
			?\n)))
	   ((numberp item)
	    (unless status
	      (setq status (darcsum-insert-file-line "Modified" path "\n"
						     nil marked 'file changes)
		    beg (point)))
	    (unless (not visible)
	      (insert
	       (darcsum-add-face
		(format "%-10d" (car change)) 'darcsum-change-line-face t)
	       ?\n)
	      (dolist (line (cddr change))
		(string-match "[ \t]*$" line 1)
		(let ((nws (substring line 0 (match-beginning 0)))
		      (ws (substring line (match-beginning 0))))
		  (insert
		   (if marked
		       (darcsum-add-face nws 'darcsum-marked-face t)
		     nws)
		   (darcsum-add-face ws 'darcsum-whitespace-ateol-face t)
		   ?\n))))))
	  (if (/= beg (point))
	      (add-text-properties beg (point)
				   (list 'darcsum-line-type 'change
					 'darcsum-line-path path
					 'darcsum-line-change
					 (list (list path change)))))
	  (setq changes (cdr changes))))))
  (insert "
--------------------- End ---------------------\n"))

(defun darcsum-insert-file-line (title path end visible marked
				       &optional line-type changes)
  "Insert per-file line into buffer"
  (let ((begin (point)))
    (if (and marked changes)
	(setq marked (darcsum-changeset-all-marked-p
		      (list (cons path changes)))))
    (insert
     "\t  "
     (if visible
	 (darcsum-add-face " * " 'darcsum-change-line-face t)
       "   ")
     " "
     (darcsum-add-face (format "%-24s" title)
		       (if marked
			   'darcsum-need-action-marked-face
			 'darcsum-need-action-face) t)
     (darcsum-add-face (file-name-nondirectory path) 'darcsum-filename-face t)
     end)
    (if line-type
	(add-text-properties beg (point)
			     (list 'darcsum-line-type 'file
				   'darcsum-line-path path
				   'darcsum-line-change
				   (list (cons path changes))))))
  title)

(defsubst darcsum-get-line-type (&optional position)
  "Get darcsum line type at point or at the given POSITION."
  (get-text-property (or position (point)) 'darcsum-line-type))

;;; Code to determine the current changeset in darcsum-mode

(defun darcsum-changeset-at-point (&optional invisible-too)
  "Return changeset at current point"
  (let ((data (get-text-property (point) 'darcsum-line-change)))
    (if invisible-too
        data
      (darcsum-changeset-find-visible data))))

(defun darcsum-selected-changeset (&optional all-visible)
  "Return the currently selected changeset.

If marks are active, always returned the marked changes.
Otherwise, return the changes related to point, unless ALL-VISIBLE is
non-nil, in which case return all visible changes."
  (cond
   ((darcsum-changeset-any-marked-p darcsum-data)
    (darcsum-changeset-find-marked darcsum-data))
   (all-visible
    (darcsum-changeset-find-visible darcsum-data))
   (t
    (darcsum-changeset-at-point 'invisible-too))))

;;; Code to record the current changeset

;; If there are any marked changes, these are what get recorded.
;; Otherwise, all *visible* changes are recorded.

(defcustom darcsum-program "darcs"
  "*The program name which darcsum will use to invoke darcs."
  :type 'string
  :group 'darcsum)

(defcustom darcsum-default-expanded nil
  "*Non-nil means the *darcsum* buffer will be expanded by default."
  :type 'boolean
  :group 'darcsum)

(defvar darcsum-output-environment
  (list
   "DARCS_DONT_ESCAPE_TRAILING_SPACES=1"
   "DARCS_DONT_COLOR=1"
   "DARCS_DONT_ESCAPE_TRAILING_CR=1")
  "The environment variables to turn off highlighting.")

(defvar darcsum-environment
  nil
  "*The extra environment variables for darcs.")

(defvar darcsum-process-arg nil)
(defvar darcsum-parent-buffer nil)
(defvar darcsum-changeset-to-record nil)
(defvar darcsum-logfile)

(defvar darcsum-window-configuration-temp nil)

(defsubst darcsum-remember-window-configuration ()
  (setq darcsum-window-configuration-temp (list (current-window-configuration)
                                           (point-marker))))
(defsubst darcsum-recall-window-configuration ()
  (if darcsum-window-configuration-temp
      (progn
         (set-window-configuration (car darcsum-window-configuration-temp))
         (goto-char (cadr darcsum-window-configuration-temp)))
    (error "No window configuration to restore.")))

(defsubst darcsum-changes-handled ()
  (if (buffer-live-p darcsum-parent-buffer)
      (let ((changeset darcsum-changeset-to-record))
        (with-current-buffer darcsum-parent-buffer
          (setq darcsum-data
                (darcsum-remove-changeset darcsum-data changeset))
          (darcsum-refresh)))))

(defvar darcsum-darcs-2-options 'not-set)

(defun darcsum-start-process (subcommand args
                                         &optional name value &rest localize)
  "Start darcs process."
  (if (eq darcsum-darcs-2-options 'not-set)
      ;; Check version and set proper darcsum-darcs-2-options
      (with-temp-buffer
	(call-process darcsum-program nil t nil "--version")
	(goto-char (point-min))
	(setq darcsum-darcs-2-options
	      (if (looking-at "2[.]") (list "--quiet")))))
    (let*
        ((buf (generate-new-buffer (format " *darcs %s*" subcommand)))
         (process-environment
	  (append darcsum-environment
		  darcsum-output-environment
		  process-environment))
         (process-connection-type nil)
         (proc (apply 'start-process "darcs"
                      buf darcsum-program subcommand
		      (append darcsum-darcs-2-options args))))
      (set-process-sentinel proc 'darcsum-process-sentinel)
      (set-process-filter proc 'darcsum-process-filter)
      (with-current-buffer buf
        (while name
          (set (make-local-variable name) value)
          (setq name (car localize)
                value (cadr localize)
                localize (cddr localize))))
      proc))

(defun darcsum-process-sentinel (proc string)
  (if (buffer-live-p (process-buffer proc))
      (with-current-buffer (process-buffer proc)
	(save-excursion
	  (goto-char (point-min))
	  (cond
	   ((looking-at "\n*\\(Couldn't get lock [^\n]*\\)")
	    (let ((waiting (match-string 1)))
	      (message waiting)
	      (kill-buffer (current-buffer))))
	   ((string-match "^exited abnormally" string)
	    (message string)))))))

(defun darcsum-process-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
        ;; Insert the text, advancing the process marker.
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))
    (save-excursion
      (goto-char (point-min))

      (if (looking-at "\n*Skipped \\(record\\|add\\|revert\\) of [0-9]+ patch\\(es\\)?\\.\n")
	  (delete-region (point-min) (match-end 0)))

      (cond
       ((looking-at "\n*Finished \\(recording\\|amending\\) patch")
        (message "Changes recorded.")
        (darcsum-changes-handled)
        (when darcsum-logfile (delete-file darcsum-logfile))
        (kill-buffer (current-buffer)))
       ((looking-at "\n*Ok, if you don't want to \\(record\\|amend\\) anything")
        (message "No changes recorded.")
        (when darcsum-logfile (delete-file darcsum-logfile))
        (kill-buffer (current-buffer)))

       ((looking-at "\n*What is the target email address")
        (process-send-string proc darcsum-process-arg)
        (delete-region (point-min) (point-max)))
       ((looking-at "\n*Successfully sent patch bundle")
        (message "Changes sent to `%s'." darcsum-process-arg)
        (kill-buffer (current-buffer)))
       ((looking-at "\n*You don't want to send any patches")
        (message "No changes sent.")
        (kill-buffer (current-buffer)))

       ((looking-at "\n*Do you really want to .+\\? ") ;; Should the last whitespace be there?
        (process-send-string proc "y\n")
        (delete-region (point-min) (point-max)))
       ((looking-at "\n*Finished reverting.")
        (message "Changes reverted.")
        (darcsum-changes-handled)
        (kill-buffer (current-buffer)))
       ((looking-at "\n*If you don't want to revert")
        (message "No changes reverted.")
        (kill-buffer (current-buffer)))

       ((looking-at "\n*\\(Waiting for lock.*\\)\n+")
        (let ((waiting (match-string 1)))
          (message waiting)
          (delete-region (point-min) (match-end 0))))

       ((looking-at "\n*\\(Couldn't get lock.*\\)\n*")
        (let ((waiting (match-string 1)))
          (message waiting)
          (kill-buffer (current-buffer))))

       ((looking-at "\\(.*\n\\)*Shall I amend this patch\\?.*")
        (process-send-string proc "y")
        (delete-region (point-min) (match-end 0)))

       ((looking-at "\n*Darcs needs to know what name")
        (let* ((default-mail (concat user-full-name
                                     " <" user-mail-address ">"))
               (enable-recursive-minibuffers t)
               (mail-address (read-string
                              (format
                               "What is your email address? (default %s) "
                               default-mail)
                              nil nil default-mail)))
          (process-send-string proc mail-address)
          (process-send-string proc "\n"))
        (re-search-forward "What is your email address\\?.*")
        (delete-region (point-min) (point)))

       ((looking-at "\n*\\(move\\|addfile\\|adddir\\|binary\\|rmfile\\|rmdir\\|hunk\\|replace\\)\\s-+\\(.+?\\)\\(\\s-+\\([0-9]+\\)?\\)?\\( \\(.+\\)\\)?$")
        (let* ((kind (intern (match-string 1)))
               (path (match-string 2))
               (start-line (match-string 4))
               (extra (match-string 6))
	       (content 'any))
          (goto-char (match-end 0))
          (forward-line)
	  (case kind
	    ('hunk (setq kind (string-to-number start-line)))
	    ('move (setq content extra))
	    ('replace (setq content extra)))
          (while (looking-at "^\\([+-].*\\)")
            (forward-line))
          (when (looking-at
                 "^Shall I \\(record\\|send\\|revert\\|add\\) this \\(patch\\|change\\)\\?.+[]:] ")
            (let ((end (match-end 0))
                  (reply (darcsum-find-change
			  darcsum-changeset-to-record
			  path kind content)))
	      ;; (message (concat (if reply "Do " "Skip ") (match-string 1) " to " path))
              (process-send-string proc (if reply "y" "n"))
              (delete-region (point-min) end)))))

       ((looking-at "\n*\\(move\\).+")
        (goto-char (match-end 0))
        (forward-line)
        (when (looking-at
               "^Shall I \\(record\\|send\\|revert\\|add\\) this \\(patch\\|change\\)\\?.+[]:] ")
          (let ((end (match-end 0)))
            (process-send-string proc "n")
            (delete-region (point-min) end))))))))

(defun darcsum-really-record ()
  (interactive)
  (let ((tempfile (darcsum-make-temp-file "darcsum"))
        (parent-buf darcsum-parent-buffer)
        (changeset darcsum-changeset-to-record))
    (save-excursion
      (goto-char (point-max))
      (unless (bolp)
        (insert ?\n))
      (goto-char (point-min))
      (when (looking-at "^\\s-*$")
        (error "No record description entered")))
    (write-region (point-min) (point-max) tempfile)
    (kill-buffer (current-buffer))
    (darcsum-recall-window-configuration)
    (message "Recording changes...")
    ;;;;;;;; TODO: optionally pass in e.g. --no-test somehow
    (darcsum-start-process
     "record" (list "--logfile" tempfile)
     'darcsum-logfile tempfile
     'darcsum-changeset-to-record changeset
     'darcsum-parent-buffer parent-buf)))

(defun darcsum-record ()
  "Record selected changeset.
Note that only changes selected for recording are actually recorded.
If some changes are marked \(with \
\\<darcsum-mode-map>\\[darcsum-toggle-mark]\), \
then only those changes are recorded.
Otherwise, only changes which are selected to be displayed in the buffer
\(with \\<darcsum-mode-map>\\[darcsum-toggle]\) are recorded."
  (interactive)
  (darcsum-remember-window-configuration)
  (let ((parent-buf (current-buffer))
        (changeset (darcsum-selected-changeset t))
        (buf))
    (if (null changeset)
	(error "No changes are selected"))
    (if (darcsum-changeset-any-p changeset
				 (function
				  (lambda (change)
				    (memq (car change) '(newdir newfile)))))
	(error "You have to add new directories and files first."))
    (switch-to-buffer-other-window (setq buf (get-buffer-create "*darcs comment*")))
    (if (fboundp 'log-edit)
	;; TODO: add SETUP (nil?) and LISTFUN arguments?  See also `vc-log-edit'
	(log-edit #'darcsum-really-record)
      (darcsum-comment-mode) )
    (set (make-local-variable 'darcsum-changeset-to-record) changeset)
    (set (make-local-variable 'darcsum-parent-buffer) parent-buf)
    (message
     "Title of change on first line, long comment after.  \
C-c C-c to record.")
    (run-hooks 'darcsum-comment-hook)))

(defun darcsum-send (recipient)
  "Send selected changeset via email."
  (interactive "sSend changes to: ")
  (message "Sending changes...")
  (darcsum-start-process
   "send" (list)
   'darcsum-changeset-to-record (darcsum-selected-changeset t)
   'darcsum-parent-buffer (current-buffer)
   'darcsum-process-arg recipient))

(defun darcsum-changes (&optional how-many)
  "Show the changes in another buffer.  Optional argument HOW-MANY limits
the number of changes shown, counting from the most recent changes."
  (interactive "P")
  (let ((proc (darcsum-start-process
               "changes" (if how-many
                             (list "--last" (number-to-string how-many))
                             (list))
               'darcsum-parent-buffer (current-buffer))))
    (set-process-filter proc nil)
    (set-process-sentinel proc 'darcsum-changes-sentinel)
    (switch-to-buffer-other-window (process-buffer proc))
    (process-buffer proc)))

(defun darcsum-changes-sentinel (process event)
  (with-current-buffer (process-buffer process)
    (darcsum-changes-mode)
    (goto-char (point-min))))

(defun darcsum-query-manifest ()
  "List the version-controlled files in the working copy."
  (interactive)
  (let ((proc (darcsum-start-process
               "query" '("manifest")
               'darcsum-parent-buffer (current-buffer))))
    (set-process-filter proc nil)
    (set-process-sentinel proc 'darcsum-query-manifest-sentinel)
    (switch-to-buffer-other-window (process-buffer proc))
    (process-buffer proc)))

(defun darcsum-query-manifest-sentinel (process event)
  (with-current-buffer (process-buffer process)
    (setq buffer-read-only t)
    (darcsum-query-mode)
    (goto-char (point-min))))

(defcustom darcsum-amend-confirmation-function #'darcsum-amend-confirmation
  "*Function to invoke for confirming an amend operation.

The function receives a prompt string as its sole argument; the expectation
is that it would display this string to the user, and prompt for a response.

When the function is invoked, the current buffer is a temporary history
buffer displaying information about the patch which is about to be amended,
and a warning about the possible problems committing this change could cause.

If the function returns nil, `darcsum-amend' will not carry out the
amend operation.

Setting this function to nil will disable the confirmation logic altogether;
however, this is strongly discouraged.

Amending a shared repository can be dangerous; see the Darcs manual
for details."
  :type '(choice (const :tag "darcsum-amend-confirmation (default)"
			#'darcsum-amend-confirmation)
		 (const :tag "Off (strongly discouraged)" nil)
		 function)
  :group 'darcsum)

(defun darcsum-amend-confirmation (prompt)
  "The default confirmation function for `darcsum-amend-confirmation-function';
pauses for two seconds, then invokes `yes-or-no-p'."
  (sit-for 2)
  (yes-or-no-p prompt) )

(defun darcsum-amend ()
  "Amend last patch with selected changeset."
  (interactive)
  (let ((changeset (darcsum-selected-changeset t))
        (parent-buffer (current-buffer)))
    (if (> (length changeset) 0)
        (let ((history-buffer (darcsum-changes 1))
	      amend point)
	  (unwind-protect
	      (with-current-buffer history-buffer
		(setq point (point-max))
		(goto-char point)
		(insert "
WARNINGS: You should ONLY use amend-record on patches which only exist in
a single repository!  Also, running amend-record while another user is
pulling from the same repository may cause repository corruption.\n")
		(goto-char point)
		(setq
		 amend
		 ;; If darcsum-amend-confirmation-function is nil, don't prompt
		 (or
		  (not (functionp darcsum-amend-confirmation-function))
		  (funcall darcsum-amend-confirmation-function 
			   "Amend this latest changeset? (see WARNINGS) ") )) )
	    (kill-buffer history-buffer))
          (when amend
            (darcsum-start-process
             "amend" (list)
             'darcsum-logfile nil
             'darcsum-changeset-to-record changeset
             'darcsum-parent-buffer parent-buffer)))
      (message "You need to select something first"))))

(defun darcsum-revert ()
  "Revert selected changeset."
  (interactive)
  (when (yes-or-no-p "Really revert these changes? ")
    (message "Reverting changes...")
    (darcsum-start-process
     "revert" (list)
     'darcsum-changeset-to-record (darcsum-selected-changeset t)
     'darcsum-parent-buffer (current-buffer))))

(defvar darcsum-comment-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-s" 'darcsum-really-record)
    (define-key map "\C-c\C-c" 'darcsum-really-record)
    map))

(defun darcsum-kill-ancillary-buffer ()
  "Kill an ancillary buffer called by darcsum."
  (interactive)
  (kill-this-buffer)
  (delete-window))

(defun darcsum-changes-mode-next-comment (&optional n)
  "Move to the next comment.

If called with a positive argument then move N comments forward."
  (interactive "p")
  (if (and n (< 0 n))
      (let ((comment-start-regexp "^[A-Z][a-z]\\{2\\} [A-Z][a-z]\\{2\\}.*$"))
	(when (looking-at comment-start-regexp)
	  (forward-line 1))
	(let ((next (re-search-forward comment-start-regexp
				       (point-max) t (or n 1))))
	  (if next
	      (goto-char (point-at-bol))
	    (message "No earlier changes"))))
    (darcsum-changes-mode-previous-comment n)))

(defun darcsum-new-buffer (&optional subdir)
  "Generate new darcsum buffer.  Optional argument SUBDIR selects subdirectory."
  (generate-new-buffer
   (concat "*darcs "
	   (file-name-nondirectory
	    (directory-file-name
	     (file-name-directory default-directory)))
	   (when subdir "/")
	   (when subdir
	     (if (file-name-absolute-p subdir)
		 (file-relative-name subdir)
	       subdir))
	   "*")))

(defun darcsum-changes-mode-previous-comment (&optional n)
  "Move to the previous comment.

If called with a positive argument then move N comments backward."
  (interactive "p")
  (when (and n (< n 0))
    (error "To move forward call `darcsum-changes-mode-next-comment' instead"))
  (let ((comment-start-regexp "^[A-Z][a-z]\\{2\\} [A-Z][a-z]\\{2\\}.*$"))
    (when (looking-at comment-start-regexp)
      (forward-line -1))
    (let ((next (re-search-backward comment-start-regexp
				    (point-min) t (or n 1))))
      (if next
	  (goto-char (point-at-bol))
	(message "No later changes")))))

(defun darcsum-query-kill-buffer ()
  (interactive)
  (kill-this-buffer)
  (delete-window))

(defvar darcsum-query-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'darcsum-query-kill-buffer)
    map))

(defvar darcsum-changes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'darcsum-kill-ancillary-buffer)
    (define-key map "n" 'darcsum-changes-mode-next-comment)
    (define-key map "p" 'darcsum-changes-mode-previous-comment)
    map))

(define-derived-mode darcsum-comment-mode indented-text-mode "Darcs Summary"
  "Major mode for output from \\<darcsum-mode-map>\\[darcsum-comment].

\\{darcsum-comment-mode-map}"
  :group 'darcsum
  (setq truncate-lines t))

(define-derived-mode darcsum-query-mode indented-text-mode "Darcs Query"
  "Major mode for output from \\<darcsum-mode-map>\\[darcsum-query-manifest].

\\{darcsum-query-mode-map}"
  :group 'darcsum
  (setq truncate-lines t))

(define-derived-mode darcsum-changes-mode indented-text-mode "Darcs Changes"
  "Major mode for output from \\<darcsum-mode-map>\\[darcsum-changes].

\\{darcsum-changes-mode-map}"
  :group 'darcsum
  (setq truncate-lines nil)
  (setq buffer-read-only t))

;;; Major Mode

(defun darcsum-check-darcsum-mode ()
  (unless (eq major-mode 'darcsum-mode)
    (error "Not in a darcsum-mode")))

(defun darcsum-reposition ()
  (unless (null (darcsum-get-line-type))
    (goto-char (line-beginning-position))
    (cond
     ((looking-at "in directory") (forward-char 13))
     ((looking-at "\t") (forward-char 31)) ; at column 38
     )))

(defsubst darcsum-other-buffer (other-buffer)
  (let ((buf (or other-buffer (darcsum-new-buffer))))
    (with-current-buffer buf
      (unless (eq major-mode 'darcsum-mode)
        (darcsum-mode))
      (current-buffer))))

(defun darcsum-move (other-buffer)
  "Move the selected changeset to another darcsum buffer OTHER-BUFFER.

In interactive mode, prompts for the name of a buffer to move the changeset to.

Changesets may be moved around in different buffers, to ease
the collection of changes to record in a single darcs patch."
  (interactive "BMove change to (RET creates new patch): ")
  (let ((buf (darcsum-other-buffer other-buffer))
        (changeset (darcsum-selected-changeset))
	(inhibit-redisplay t))
    (setq darcsum-data
          (darcsum-remove-changeset darcsum-data changeset))
    (with-current-buffer buf
      (darcsum-apply-to-changes changeset 'darcsum-change-remove-all-flags)
      (setq darcsum-data (darcsum-add-changeset darcsum-data changeset))
      (darcsum-refresh)))
  (darcsum-refresh))

(defun darcsum-find-file (&optional other view)
  "Open the selected entry.
With a prefix OTHER, open the buffer in another window.
If OTHER is 'dont-select, don't select the buffer.
VIEW non-nil means open in View mode."
  (interactive "P")
  (let ((file (darcsum-path (point)))
	(start (point-at-bol))
	(change-line
	 (and (eq 'change (darcsum-get-line-type))
	      (caadar (darcsum-changeset-at-point t)))))
    (if (numberp change-line)
	(save-excursion
	  (goto-char start)
	  (cond
	   ((looking-at " ")		; We were in context
	    (while (looking-at " ") (forward-line))
	    (if (looking-at "[-+]")	; ..before change
		(setq change-line (- change-line (count-lines start (point))))
	      (goto-char start)		; ...after change
	      (while (looking-at " ")
		(forward-line -1))
	      (setq change-line (+ change-line -1 (count-lines (point) start)))))
	   ((looking-at "[+]")
	    (while (looking-at "[+]")
	      (forward-line -1))
	    (setq change-line (+ change-line -1 (count-lines (point) start)))))))
    (with-current-buffer
	(cond ((eq other 'dont-select)
	       (find-file-noselect file))
	      ((and other view)
	       (view-file-other-window file))
	      (view (view-file file))
	      (other (find-file-other-window file))
	      (t (find-file file)))
      (if (numberp change-line)
	  (goto-line change-line))
      (display-buffer (current-buffer))
      (recenter '(4)))))

(defun darcsum-find-file-other-window ()
  "Select a buffer containing the file with current change in another window"
"possibly moving point to the change's location."
  (interactive)
  (darcsum-check-darcsum-mode)
  (darcsum-find-file t))

(defun darcsum-goto ()
  "Select a buffer containing the file with current change in another window"
"possibly moving point to the change's location."
  (interactive)
  (darcsum-check-darcsum-mode)
  (darcsum-find-file t))

(defun darcsum-display-change ()
  "Display a buffer containing the current change in another window."
  (interactive)
  (darcsum-check-darcsum-mode)
  (darcsum-find-file 'dont-select))

(defun darcsum-toggle-context ()
  (interactive)
  (darcsum-check-darcsum-mode)
  (setq darcsum-show-context (not darcsum-show-context))
  (darcsum-redo))

(defun darcsum-toggle-mark ()
  "Toggle mark on current changeset.

Marked changesets have priority over simply activated ones regarding
the selection of changesets to commit."
  (interactive)
  (darcsum-check-darcsum-mode)
  (let ((changeset (darcsum-changeset-at-point t)))
    (darcsum-apply-to-changes changeset 'darcsum-change-toggle-mark))
  (darcsum-refresh))

(defun darcsum-mouse-toggle-mark ()
  "Move point to mouse and toggle mark on changeset."
  (interactive)
  (unless (not current-mouse-event)
    (mouse-set-point current-mouse-event)
    (darcsum-toggle-mark)))

(defun darcsum-show ()
  "Activate the current changeset."
  (interactive)
  (darcsum-check-darcsum-mode)
  (let ((changeset (darcsum-changeset-at-point t)))
    (darcsum-apply-to-changes changeset 'darcsum-change-remove-hide))
  (darcsum-refresh))

(defun darcsum-toggle ()
  "Toggle the activation of the current changeset.

The activation of a changeset exposes the associated change, and selects
it for later commit."
  (interactive)
  (darcsum-check-darcsum-mode)
  ;;;;;;;; TODO: easier to expose a hunk which was made invisible by mistake
  (let ((changeset (darcsum-changeset-at-point t)))
    (if (darcsum-changeset-any-visible-p changeset)
	(darcsum-apply-to-changes changeset 'darcsum-change-add-hide)
      (darcsum-apply-to-changes changeset 'darcsum-change-toggle-hide)))
  (darcsum-refresh))

(defun darcsum-refresh (&optional line)
  "Refresh the visualization of the changesets.

If LINE is not nil, move to LINE. Otherwise, stay on current line."
  (interactive)
  (darcsum-check-darcsum-mode)
  (let ((inhibit-redisplay t))
    (unless line
      (setq line (count-lines (point-min) (point-at-bol))))
    (darcsum-display-changeset darcsum-data)
    (goto-char (point-min))
    (forward-line line)
    (darcsum-reposition)))

(defun darcsum-line-is (sort)
  (save-excursion
    (beginning-of-line)
    (let ((type (darcsum-get-line-type)))
      (case sort
        ('new (and (eq 'file type) (looking-at "  +New")))
        ('modified (or (and (eq 'file type) (looking-at "\\s-+Modified"))
                       (eq 'change type)))
        ('file (eq 'file type))
        ('change (eq 'change type))
        ('marked
         (memq (get-text-property (point) 'face)
               '(darcsum-marked-face darcsum-need-action-marked-face)))))))

(defun darcsum-next-entity (&optional arg backward)
  "Move to the next file or change.
With ARG, move that many times.
BACKWARD non-nil means to go backwards."
  (interactive "p")
  (let (changeset)
    (dotimes (i (or arg 1))
      (setq changeset (darcsum-changeset-at-point t))
      (beginning-of-line)
      (while (progn
	       (forward-line (if backward -1))
	       (not (or (null (darcsum-get-line-type))
			(and (looking-at "[0-9i\t]") ; stop at headers
			     (not (eq changeset (darcsum-changeset-at-point t))))))))))
  (unless (darcsum-get-line-type)
    (goto-char (if backward (point-max) (point-min))) ;; Wrap around
    (forward-line (if backward -3 3)))
  (darcsum-reposition))

(defun darcsum-next-line (&optional arg)
  "Move to the next file or change.
With ARG, move that many times."
  (interactive "p")
  (darcsum-next-entity arg))

(defun darcsum-previous-line (&optional arg)
  "Move to the previous file or change.
With ARG, move that many times."
  (interactive "p")
  (darcsum-next-entity arg t))

(defun darcsum-mark-and-next-entity (&optional arg)
  "Mark then move to the next unmarked directory, file or change.
With ARG, mark and move that many times."
  (interactive "P")
  (unless
      (darcsum-apply-and-next-entity
       (function darcsum-change-add-mark)
       (function darcsum-changeset-any-unmarked-p)
       arg)
    (message "No more unmarked changes."))
  (darcsum-refresh))

(defun darcsum-unmark-and-next-entity (&optional arg)
  "Unmark then move to the next marked directory, file or change.
With ARG, mark and move that many times."
  (interactive "P")
  (unless
      (darcsum-apply-and-next-entity
       (function darcsum-change-remove-mark)
       (function darcsum-changeset-any-marked-p)
       arg)
    (message "No more marked changes."))
  (darcsum-refresh))

(defun darcsum-apply-and-next-entity (func next-p &optional arg backward)
  "Apply FUNC to current changeset and move forward until NEXT-P changeset.
With ARG, mark and move that many times. With BACKWARD, move to previous.
Return nil if there is no changeset matching NEXT-P."
  (let ((started (point))
	changeset
	(type (darcsum-get-line-type)))
    (if (catch 'exit
	  (ignore
	   (dotimes (i (or arg 1))
	     (setq changeset (darcsum-changeset-at-point t))
	     (darcsum-apply-to-changes changeset func)
	     (beginning-of-line)
	     (while (progn
		      (forward-line (if backward -1))
		      (unless (darcsum-get-line-type)
			(throw 'exit t))
		      (not (and
			    (looking-at "[0-9i\t]") ; stop at headers
			    ; Don't stop at dir unless started from dir
			    (or (eq type 'dir)
				(not (eq 'dir (darcsum-get-line-type))))
			    (funcall next-p (darcsum-changeset-at-point t))))))
	     )))
	(ignore (goto-char started))
      t)))

(defcustom darcsum-diff-switches nil
  "*diff(1) switches used by `darcsum-diff'."
  :type 'string
  :group 'darcsum)

(defun darcsum-diff ()
  "Show the changes made to current selection."
  ; XXX - does not work with darcs2!
  (interactive)
  (let ((type (darcsum-get-line-type))
	(original-path (darcsum-original-path (point))))
    (cond
     ((not original-path)
      (error "No record of this file in darcs"))
     ((eq type 'dir))
     ((or (eq type 'file)
          (eq type 'change))
      (require 'diff)                   ; for `diff-switches'
      (diff original-path
            (darcsum-path (point))
            (or darcsum-diff-switches diff-switches))))))

(defun darcsum-path (pos)
  (expand-file-name (get-text-property pos 'darcsum-line-path)))

(defun darcsum-original-path (pos)
  (let* ((path (get-text-property pos 'darcsum-line-path))
	 (pristine-path (expand-file-name path "_darcs/pristine"))
	 (current-path (expand-file-name path "_darcs/current")))
    (cond ((file-readable-p pristine-path) pristine-path)
	  ((file-readable-p current-path) current-path))))

(defun darcsum-delete ()
  "Remove selected changeset from the view."
  (interactive)
  (setq darcsum-data
        (darcsum-remove-changeset darcsum-data
                                  (darcsum-selected-changeset)))
  (darcsum-refresh))

(defun darcsum-remove ()
  "Remove a file from the repository.

This runs darcs remove (which undoes accidental addfile or adddir).

If you want to remove an existing file or directory, remove file or
directory otherwise and record change."
  (interactive)
  (darcsum-check-darcsum-mode)
  (let ((changeset (darcsum-changeset-at-point t))
	(type (darcsum-get-line-type))
	(path (get-text-property (point) 'darcsum-line-path)))
    (cond
     ((eq (caadar changeset) 'adddir)
      (setq changeset (cdr changeset))
      (while (memq (caadar changeset) '(newfile newdir))
	(setq changeset (cdr changeset)))
      (if changeset
	  (error "Remove pending changes in directory first")))
     ((eq (caadar changeset) 'addfile)
      (setq changeset (cdr changeset))
      (while (numberp (caadar changeset))
	(setq changeset (cdr changeset)))
      (if changeset
	  (error "First undo pending changes in file")))
     (t
      (error "Not added file or directory")))
    (unless (= 0 (call-process darcsum-program nil t nil
			       "remove" path))
      (error "Error running `darcs remove'"))
    (darcsum-redo)))

(defun darcsum-add ()
  "Put new file or directory under Darcs control."
  (interactive)
  (darcsum-check-darcsum-mode)
  (let ((changeset (darcsum-selected-changeset))
	file path change added)
    (dolist (file changeset)
      (setq path (car file)
	    change (cadr file))
      (if (memq (car change) '(newfile newdir))
	  (with-temp-buffer
	    (if (/= 0 (call-process
		       darcsum-program nil t nil "add" path))
		(error "Error running `darcs add' for `%s'" path)
	      (setcar change (cdr (assoc (car change) '((newfile . addfile)
							(newdir . adddir))))))
	    (setq added t))))
    (unless added
      (error "No new entries, cannot add")))
  (darcsum-refresh))

(defun darcsum-add-to-boring (path)
  "Add current file or directory to the boring file.

Propose the insertion of a regexp suitable to permanently ignore
the file or the directory at point into the boring file."
  (interactive
   (let ((type (darcsum-get-line-type))
	 (path (get-text-property (point) 'darcsum-line-path)))
     (if (string-match "^\\./" path)
	 (setq path (substring path 2)))
     (setq path (regexp-quote path))
     (cond
      ((eq type 'dir)
       (setq path (concat "(^|/)" path "($|/)")))
      ((memq type '(file change))
       (setq path (concat "(^|/)" path "$"))))
     (list (read-string "Add to boring list: " path))))
  (save-excursion
    (set-buffer (find-file-noselect "_darcs/prefs/boring"))
    (goto-char (point-max))
    (insert path ?\n)
    (save-buffer)
    (kill-buffer (current-buffer)))
  (darcsum-redo))

(defun darcsum-add-change-log-entry ()
  "Execute `add-change-log-entry' on the current file."
  (interactive)
  (let ((type (darcsum-get-line-type)))
    (cond
     ((eq type 'dir))
     ((or (eq type 'file)
          (eq type 'change))
      (darcsum-goto)
      (add-change-log-entry)))))

(defun darcsum-ediff ()
  "Like `darcsum-diff' but in an Ediff session."
  (interactive)
  (let ((type (darcsum-get-line-type)))
    (cond
     ((eq type 'dir))
     ((or (eq type 'file)
          (eq type 'change))
      (let ( (pristine-filename (darcsum-original-path (point)))
             (working-filename (darcsum-path (point)))
	     ;; Save the current window configuration, before opening ediff
             (old-window-configuration (current-window-configuration))
             )
      (progn
        (save-excursion
	  ;; Pristine copy should not be modified
          (find-file-read-only pristine-filename)
	  ;; It should be clear this is not a buffer you want to touch.
          (rename-buffer (concat "*darcsum-pristine:" pristine-filename "*"))
          )
        (ediff pristine-filename working-filename
               ;;Add this anonymous function as a startup hook in ediff-mode
               (lambda ()
		 (progn
                            (setq darcsum-pre-ediff-window-configuration
				  old-window-configuration)
                            ;; After we quit THIS PARTICULAR ediff buffer,
			    ;; restore the old window configuration
                            (add-hook
			     'ediff-quit-hook
			     (lambda () (set-window-configuration
					 darcsum-pre-ediff-window-configuration)
			       ) nil t)
                            )))
        ))))))

(defun darcsum-ediff-merge ()
  "Start an `ediff-merge' session on the current selection."
  (interactive)
  (let ((type (darcsum-get-line-type)))
    (cond
     ((eq type 'dir))
     ((or (eq type 'file)
          (eq type 'change))
      (ediff-merge (darcsum-original-path (point))
                   (darcsum-path (point)))))))

(defun darcsum-redo (&optional arg)
  "Refresh the status, redoing `darcs whatsnew'."
  (interactive "P")
  (darcsum-check-darcsum-mode)
  (let ((dir (expand-file-name darcsum-subdirectory default-directory))
        (look-for-adds (or arg darcsum-look-for-adds))
        (darcsum-default-expanded t))
    (message "Re-running darcsum-whatsnew")
    (let ((changes (darcsum-whatsnew
                    dir look-for-adds t darcsum-show-context)))
      (setq darcsum-data
            (darcsum-merge-changeset darcsum-data changes)))
    (darcsum-refresh)))

(defun darcsum-quit ()
  "Close the darcsum buffer and quit."
  (interactive)
  (darcsum-check-darcsum-mode)
  (kill-buffer (current-buffer)))


(defun darcsum-add-comment ()
  "Similar to `add-change-log-entry'.

Inserts the entry in the darcs comment file instead of the ChangeLog."
  ;; This is mostly copied from add-log.el and Xtla.  Perhaps it would
  ;; be better to split add-change-log-entry into several functions
  ;; and then use them, but that wouldn't work with older versions of
  ;; Emacs.
  (interactive)
  (require 'add-log)
  (let* ((defun (add-log-current-defun))
         (buf-file-name (if (and (boundp 'add-log-buffer-file-name-function)
                                 add-log-buffer-file-name-function)
                            (funcall add-log-buffer-file-name-function)
                          buffer-file-name))
         (buffer-file (if buf-file-name (expand-file-name buf-file-name)))
;         (file-name (tla-make-log))
         ;; Set ENTRY to the file name to use in the new entry.
         (entry (add-log-file-name buffer-file default-directory))
         beg
         bound
         narrowing)
    (switch-to-buffer-other-window (get-buffer-create "*darcs comment*"))

    (goto-char (point-min))
    (forward-line 1)                    ; skip header
    ;; Now insert the new line for this entry.
    (cond ((re-search-forward "^\\s *\\*\\s *$" bound t)
           ;; Put this file name into the existing empty entry.
           (if entry
               (insert entry)))
          ((let (case-fold-search)
             (re-search-forward
              (concat (regexp-quote (concat "* " entry))
                      ;; Don't accept `foo.bar' when
                      ;; looking for `foo':
                      "\\(\\s \\|[(),:]\\)")
              bound t))
           ;; Add to the existing entry for the same file.
           (re-search-forward "^\\s *$\\|^\\s \\*\\|\\'")
           (goto-char (match-beginning 0))
           ;; Delete excess empty lines; make just 2.
           (while (and (not (eobp)) (looking-at "^\\s *$"))
             (delete-region (point) (line-beginning-position 2)))
           (insert-char ?\n 2)
           (forward-line -2)
           (indent-relative-maybe))
          (t
           ;; Make a new entry.
           (goto-char (point-max))
           (re-search-backward "^." nil t)
           (end-of-line)
           (insert "\n* ")
           (if entry (insert entry))))
    ;; Now insert the function name, if we have one.
    ;; Point is at the entry for this file,
    ;; either at the end of the line or at the first blank line.
    (if defun
        (progn
          ;; Make it easy to get rid of the function name.
          (undo-boundary)
          (unless (save-excursion
                    (beginning-of-line 1)
                    (looking-at "\\s *$"))
            (insert ?\ ))
          ;; See if the prev function name has a message yet or not
          ;; If not, merge the two entries.
          (let ((pos (point-marker)))
            (if (and (skip-syntax-backward " ")
                     (skip-chars-backward "):")
                     (looking-at "):")
                     (progn (delete-region (+ 1 (point)) (+ 2 (point))) t)
                     (> fill-column (+ (current-column) (length defun) 3)))
                (progn (delete-region (point) pos)
                       (insert ", "))
              (goto-char pos)
              (insert "("))
            (set-marker pos nil))
          (insert defun "): "))
      ;; No function name, so put in a colon unless we have just a star.
      (unless (save-excursion
                (beginning-of-line 1)
                (looking-at "\\s *\\(\\*\\s *\\)?$"))
        (insert ": ")))))

(defvar darcsum-mode-abbrev-table nil
  "Abbrev table used while in darcsum-mode mode.")
(define-abbrev-table 'darcsum-mode-abbrev-table ())

(global-set-key "\C-xD" 'darcsum-add-comment)

(defvar darcsum-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map [return] 'darcsum-toggle) ; ??
    (define-key map "\C-m" 'darcsum-toggle)
    (define-key map "\C-\M-m" 'darcsum-show)
    (define-key map "\C-c\C-c" 'darcsum-goto)
    (define-key map [tab] 'darcsum-next-entity)
    (define-key map [space] 'darcsum-mark-and-next-entity)
    (define-key map " " 'darcsum-mark-and-next-entity)
    (define-key map [backspace] 'darcsum-unmark-and-next-entity)
    (define-key map [delete] 'darcsum-unmark-and-next-entity)
    (define-key map "?" 'describe-mode)
    (define-key map "f" 'darcsum-find-file)
    (define-key map "v" 'darcsum-display-change)
    (define-key map "=" 'darcsum-diff)
    (define-key map "e" 'darcsum-ediff)
    (define-key map "E" 'darcsum-ediff-merge)
    (define-key map "g" 'darcsum-redo)
    (define-key map "n" 'darcsum-next-line)
    (define-key map "p" 'darcsum-previous-line)
    (define-key map "a" 'darcsum-add)
    (define-key map "l" 'darcsum-add-change-log-entry)
    (define-key map "c" 'darcsum-record)
    (define-key map "R" 'darcsum-record)
    (define-key map "U" 'darcsum-revert)
    (define-key map "u" 'darcsum-toggle-context)
    (define-key map "d" 'darcsum-delete)
    (define-key map "r" 'darcsum-remove)
    (define-key map "M" 'darcsum-move)
    (define-key map "m" 'darcsum-toggle-mark)
    (define-key map [button2] 'darcsum-mouse-toggle-mark)
    (define-key map "i" 'darcsum-add-to-boring)
    (define-key map "B" 'darcsum-add-to-boring)
    (define-key map "q" 'darcsum-quit)
    map))

(easy-menu-define darcsum-menu darcsum-mode-map "Menu used in `darcsum-mode'."
  '("Darcs summary"
    ["Open file.."              darcsum-find-file
                                (or (darcsum-line-is 'file)
                                    (darcsum-line-is 'change))]
    [" ..other window"          darcsum-find-file-other-window
                                (or (darcsum-line-is 'file)
                                    (darcsum-line-is 'change))]
    ["Display in other window"  darcsum-display-file    t]
    ("Differences"
     ["Interactive diff"        darcsum-ediff           t]
     ["Current diff"            darcsum-diff            t]
     ["Interactive merge"       darcsum-ediff-merge     t])
;;     ["View log"                      darcsum-log             t]
    "--"
    ["Re-examine"               darcsum-redo            t]
    ["Record changes"           darcsum-record          t] ; fixme: condition
    ["Amend last changeset"     darcsum-amend           t] ; fixme: condition
;;     ["Tag"                   darcsum-tag             t]
    ["Undo changes"             darcsum-revert          t] ; fixme: condition
    ["Add"                      darcsum-add             (darcsum-line-is 'new)]
    ["Remove"                   darcsum-remove          (darcsum-line-is 'file)]
    ["Ignore"                   darcsum-add-to-boring   (darcsum-line-is 'file)]
    ["Add ChangeLog"            darcsum-add-change-log-entry t]
    ["Delete"                   darcsum-delete          t]
    "--"
    ["(Un)activate change"      darcsum-toggle          t]
    ["(Un)mark change"          darcsum-toggle-mark
                                :style toggle
                                :selected (darcsum-line-is 'marked)]
    ["Next file/change"         darcsum-next-line       t]
    ["Previous file/change"     darcsum-previous-line   t]
    ["Move changeset"           darcsum-move            t]
    ["Show change context"      darcsum-toggle-context
                                :style toggle :selected darcsum-show-context]
    "--"
    ["Quit"                     darcsum-quit            t]
    ))

(define-derived-mode darcsum-mode fundamental-mode "Darcs"
  "Darcs summary mode is for previewing changes to become part of a patch.
\\{darcsum-mode-map}"
  :group 'darcsum
  (make-local-variable 'darcsum-data)
  (make-local-variable 'darcsum-look-for-adds)
  (make-local-variable 'darcsum-show-context)
  (make-local-variable 'darcsum-subdirectory)
  (setq darcsum-data nil)
  (if (featurep 'xemacs)
      (easy-menu-add darcsum-menu darcsum-mode-map)))

(put 'darcsum-mode 'mode-class 'special)

(defun darcsum-display (changeset &optional look-for-adds sub-directory)
  "Display CHANGESET from SUB-DIRECTORY in a buffer.

If there there already is a buffer for displaying changes in this darcs
repository (and subdirectory within it), use the existing buffer (unless
darcsum-display-with-existing-buffer is nil)."
  (unless sub-directory (setq sub-directory "."))
  (with-current-buffer
      (or (if darcsum-display-with-existing-buffer
	      (darcsum-find-buffer default-directory sub-directory))
	  (darcsum-new-buffer default-directory sub-directory))
    (setq darcsum-data (darcsum-merge-changeset darcsum-data changeset))
    (setq darcsum-look-for-adds look-for-adds)
    (setq darcsum-subdirectory sub-directory)
    (darcsum-refresh 0)
    (darcsum-next-line 0)
    (unless (darcsum-changeset-all-visible-p darcsum-data)
      (message 
       "Press %s to show all changes"
       (darcsum-where-is (function darcsum-show))))
    (switch-to-buffer (current-buffer))))

(defcustom darcsum-display-with-existing-buffer t
  "*If nil, always create new buffer to display changeset."
  :type 'boolean
  :group 'darcsum)

(defun darcsum-new-buffer (&optional dir subdir)
  "Generate new darcsum buffer for (SUBDIR in DIR)."
  (setq dir (file-name-nondirectory
	     (directory-file-name (file-name-directory 
				   (or dir default-directory)))))
  (if (string= subdir ".")
      (setq subdir nil))
  (with-current-buffer
      (generate-new-buffer
       (concat "*darcs " dir
	       (when subdir "/")
	       (when subdir
		 (if (file-name-absolute-p subdir)
		     (file-relative-name subdir)
		   subdir))
	       "*"))
    (darcsum-mode)
    (current-buffer)))

(defun darcsum-find-buffer (&optional dir subdir)
  "Get existing darcsum buffer (for SUBDIR in DIR)."
  (catch 'exit
    (ignore
     (let (buffer locals mode buffer-dir)
       (dolist (buffer (buffer-list))
	 (setq locals (buffer-local-variables buffer)
	       mode (cdr (assq 'major-mode locals))
	       buffer-dir (cdr (assq 'default-directory locals))
	       buffer-subdir (cdr (assq 'darcsum-subdirectory locals)))
	 (if (and (eq mode 'darcsum-mode)
		  (or (null dir) (string= buffer-dir dir))
		  (or (null subdir) (string= buffer-subdir subdir)))
	     (throw 'exit buffer)))))))

(defun darcsum-where-is (command)
  "Return the representation of key sequences that invoke specified COMMAND."
  (let ((keys (where-is-internal command)))
    (if keys 
        (if (featurep 'xemacs)
            (sorted-key-descriptions keys)
          (mapconcat 'key-description keys ", "))
      (format "M-x %s RET" command))))

(defun darcsum-repository-root (&optional start-directory)
  "Return the root of the repository, or nil if there isn't one."
  (let ((dir (or start-directory
                 default-directory
                 (error "No start directory given"))))
    (if (car (directory-files dir t "^_darcs$"))
        dir
      (let ((next-dir (file-name-directory (directory-file-name
                                            (file-truename dir)))))
        (unless (or (equal dir next-dir) (null next-dir))
          (darcsum-repository-root next-dir))))))

(defcustom darcsum-whatsnew-switches nil
  "*Switches for `darcsum-whatsnew'."
  :type 'string
  :group 'darcsum)

(defcustom darcsum-whatsnew-at-toplevel t
  "*Use top-level repository directory as default argument to \
`darcsum-whatsnew'."
  :type 'boolean
  :group 'darcsum)

;;; This is the entry code, M-x darcsum-whatsnew

;;;###autoload
(defun darcsum-whatsnew (directory
                         &optional look-for-adds no-display show-context)
  "Run `darcs whatsnew' in DIRECTORY, displaying the output in `darcsum-mode'.

When invoked interactively, prompt for the directory to display changes for."
  (interactive
   ; fancy "DDirectory: \nP"
   (let ((root
          (if darcsum-whatsnew-at-toplevel
              (darcsum-repository-root)
            default-directory)))
     (list (funcall (if (fboundp 'read-directory-name)
                        'read-directory-name
                      'read-file-name)
                    "Directory: " root root)
           (or darcsum-look-for-adds current-prefix-arg))))
  (with-temp-buffer
    (cd directory)
    (let ((repo (darcsum-repository-root)))
      (unless repo
        (error "Directory `%s' is not under darcs version control"
               directory))
      (cd repo))
    (let* ((process-environment (append
				 darcsum-environment
				 darcsum-output-environment
                                 process-environment))
           (args (append
                  ;; Build a list of arguments for call-process
                  (list darcsum-program nil t nil)
                  (list "whatsnew" "--no-summary")
                  (darcsum-fix-switches darcsum-whatsnew-switches)
                  ; Arguments override user preferences
                  (unless (null look-for-adds) (list "--look-for-adds"))
                  (unless (null show-context) (list "--unified"))
                  (unless (string= directory default-directory)
                    (list (file-relative-name
                           directory default-directory)))
                  nil))
           (result (apply 'call-process args))
	   message)
      (if (/= result 0)
          (if (= result 1)
              (ignore (and (interactive-p) (message "No changes!")))
            (ignore
	     (if (fboundp 'clone-buffer)
		 (progn
		   (condition-case nil (kill-buffer "*darcs-output*") (error nil))
		   (clone-buffer "*darcs-output*" t))
	       (goto-char (point-min))
	       (if (looking-at "\n*darcs failed\\(: .*\\)")
		   (setq message (match-string 1))))
	     (error (concat "Error running darcs whatsnew" message))))
        (let ((changes (darcsum-read-changeset darcsum-default-expanded)))
          (if (and changes (not no-display))
              (darcsum-display changes look-for-adds
			       (directory-file-name
				(file-relative-name directory))))
          changes)))))

; lifted from diff.el
(defun darcsum-fix-switches (switch-spec)
  "Parse SWITCH-SPEC into a list of switches.
Leave it be if it's not a string."
  (if (stringp switch-spec)
      (let (result (start 0))
        (while (string-match "\\(\\S-+\\)" switch-spec start)
          (setq result (cons (substring switch-spec (match-beginning 1)
                                        (match-end 1))
                             result)
                start (match-end 0)))
        (nreverse result))
    switch-spec))

;;;###autoload
(defun darcsum-view (directory)
  "View the contents of the current buffer as a darcs changeset for DIRECTORY.
More precisely, searches forward from point for the next changeset-like region,
and attempts to parse that as a darcs patch.

When invoked interactively, prompts for a directory; by default, the current
working directory is assumed."
  (interactive
   (list (funcall (if (fboundp 'read-directory-name)
                      'read-directory-name
                    'read-file-name)
                  "Directory: "
                  (darcsum-repository-root))))
  (unless (file-directory-p (expand-file-name "_darcs" directory))
    (error "Directory `%s' is not under darcs version control"
           directory))
  (if (or (and (search-forward "{" nil t)
               (goto-char (1- (point))))
          (search-backward "{" nil t))
      (let ((changes (darcsum-parse-changeset))
            (default-directory directory))
        (darcsum-display changes))
    (error "Cannot find a darcs patch in the current buffer")))

;;; Gnus integration code, for viewing darcs patches in a changeset
;;; buffer.  They cannot be recorded from there, however, since the
;;; changes have not been applied to the working tree.  To do this,
;;; you must still pipe the message to "darcs apply".  This code only
;;; works as a browser for now.

(defvar darcsum-install-gnus-code nil)

(when darcsum-install-gnus-code
  (eval-when-compile (require 'gnus)
                     (require 'gnus-sum)
                     (require 'gnus-art)
                     (require 'gnus-fun)
                     (require 'gnus-win)
                     (require 'gnus-util)
                     (require 'mm-view)
                     (require 'mail-parse)

  (defun mm-view-darcs-patch (handle)
    "View HANDLE as a darcs patch, using darcsum.el."
    (let* ((name (mail-content-type-get (mm-handle-type handle) 'name))
           (directory
            (funcall (if (fboundp 'read-directory-name)
                         'read-directory-name
                       'read-file-name)
                     "Apply patch to directory: ")))
      (mm-with-unibyte-buffer
        (mm-insert-part handle)
        (let ((coding-system-for-write 'binary))
          (goto-char (point-min))
          (darcsum-view directory)
          (delete-other-windows)))))

  (defun gnus-mime-view-darcs-patch ()
    "Pipe the MIME part under point to a process."
    (interactive)
    (gnus-article-check-buffer)
    (let ((data (get-text-property (point) 'gnus-data)))
      (when data
        (mm-view-darcs-patch data))))

  (defun gnus-article-view-darcs-patch (n)
    "Pipe MIME part N, which is the numerical prefix."
    (interactive "p")
    (gnus-article-part-wrapper n 'mm-view-darcs-patch))

  (eval-after-load "gnus-art"
    '(progn
       (nconc gnus-mime-action-alist
              '(("apply darcs patch" . gnus-mime-view-darcs-patch)))
       (nconc gnus-mime-button-commands
              '((gnus-mime-view-darcs-patch "V" "Apply darcs patch...")))))

  (defun gnus-summary-view-darcs-patch (directory)
    "Apply the current article as a darcs patch to DIRECTORY."
    (interactive "DApply patch to directory: ")
    (gnus-summary-select-article)
    (let ((mail-header-separator ""))
      (gnus-eval-in-buffer-window gnus-article-buffer
        (save-restriction
          (widen)
          (goto-char (point-min))
          (darcsum-view directory)))))

  (eval-after-load "gnus-sum"
    '(progn
       (define-key gnus-summary-mime-map "V" 'gnus-article-view-darcs-patch)
       (define-key gnus-summary-article-map "V"
         'gnus-summary-view-darcs-patch)))))

(provide 'darcsum)
;;; darcsum.el ends here
