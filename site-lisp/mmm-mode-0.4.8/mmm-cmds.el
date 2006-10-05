;;; mmm-cmds.el --- MMM Mode interactive commands and keymap

;; Copyright (C) 2000 by Michael Abraham Shulman

;; Author: Michael Abraham Shulman <viritrilbia@users.sourceforge.net>
;; Version: $Id: mmm-cmds.el,v 1.18 2003/03/25 21:48:33 viritrilbia Exp $

;;{{{ GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;}}}

;;; Commentary:

;; This file contains the interactive commands for MMM Mode.

;;; Code:

(require 'font-lock)
(require 'mmm-compat)
(require 'mmm-vars)
(require 'mmm-class)

;; APPLYING CLASSES
;;{{{ Applying Predefined Classes

(defun mmm-ify-by-class (class)
  "Add submode regions according to an existing submode class."
  (interactive
   (list (intern
          (completing-read
           "Submode Class: "
           (remove-duplicates
            (mapcar #'(lambda (spec) (list (symbol-name (car spec))))
                    (append
                     (remove-if #'(lambda (spec) (plist-get (cdr spec) :private))
                                mmm-classes-alist)
                     (remove-if #'caddr mmm-autoloaded-classes)))
            :test #'equal)
           nil t))))
  (unless (eq class (intern ""))
    (mmm-apply-class class)
    (mmm-add-to-history class)
    (mmm-update-font-lock-buffer)))

;;}}}
;;{{{ Applying by the Region

(defun mmm-ify-region (submode front back)
  "Add a submode region for SUBMODE coinciding with current region."
  (interactive "aSubmode: \nr")
  (mmm-ify :submode submode :front front :back back)
  (setq front (mmm-make-marker front t nil)
        back (mmm-make-marker back nil nil))
  (mmm-add-to-history `(:submode ,submode :front ,front :back ,back))
  (mmm-enable-font-lock submode))

;;}}}
;;{{{ Applying Simple Regexps

(defun mmm-ify-by-regexp
  (submode front front-offset back back-offset save-matches)
  "Add SUBMODE regions to the buffer delimited by FRONT and BACK.
With prefix argument, prompts for all additional keywords arguments.
See `mmm-classes-alist'."
  (interactive "aSubmode: 
sFront Regexp: 
nOffset from Front Regexp: 
sBack Regexp: 
nOffset from Back Regexp: 
nNumber of matched substrings to save: ")
  (let ((args (mmm-save-keywords submode front back front-offset
                                 back-offset save-matches)))
    (apply #'mmm-ify args)
    (mmm-add-to-history args))
  (mmm-enable-font-lock submode))

;;}}}

;; EDITING WITH REGIONS
;;{{{ Re-parsing Areas

(defun mmm-parse-buffer ()
  "Re-apply all applicable submode classes to current buffer.
Clears all current submode regions, reapplies all past interactive
mmm-ification, and applies `mmm-classes' and mode-extension classes."
  (interactive)
  (message "MMM-ifying buffer...")
  (mmm-apply-all)
  (message "MMM-ifying buffer...done"))

(defun mmm-parse-region (start stop)
  "Re-apply all applicable submode classes between START and STOP.
Clears all current submode regions, reapplies all past interactive
mmm-ification, and applies `mmm-classes' and mode-extension classes."
  (interactive "r")
  (message "MMM-ifying region...")
  (mmm-apply-all :start start :stop stop)
  (message "MMM-ifying region...done"))

(defun mmm-parse-block (&optional lines)
  "Re-parse LINES lines before and after point \(default 1).
Clears all current submode regions, reapplies all past interactive
mmm-ification, and applies `mmm-classes' and mode-extension classes.

This command is intended for use when you have just typed what should
be the delimiters of a submode region and you want to create the
region. However, you may want to look into the various types of
delimiter auto-insertion that MMM Mode provides. See, for example,
`mmm-insert-region'."
  (interactive "p")
  (message "MMM-ifying block...")
  (destructuring-bind (start stop) (mmm-get-block lines)
    (when (< start stop)
      (mmm-apply-all :start start :stop stop)))
  (message "MMM-ifying block...done"))

(defun mmm-get-block (lines)
  (let ((inhibit-point-motion-hooks t))
    (list (save-excursion
            (forward-line (- lines))
            (beginning-of-line)
            (point))
          (save-excursion
            (forward-line lines)
            (end-of-line)
            (point)))))

;;}}}
;;{{{ Reparse Current Region

(defun mmm-reparse-current-region ()
  "Clear and reparse the area of the current submode region.
Use this command if a submode region's boundaries have become wrong."
  (interactive)
  (let ((ovl (mmm-overlay-at (point) 'all)))
    (when ovl
      (let ((beg (save-excursion
                   (goto-char (mmm-front-start ovl))
                   (forward-line -1)
                   (point)))
            (end (save-excursion
                   (goto-char (mmm-back-end ovl))
                   (forward-line 1)
                   (point))))
        (mmm-parse-region beg end)))))

;;}}}
;;{{{ Clear Submode Regions

;; See also `mmm-clear-history' which is interactive.

(defun mmm-clear-current-region ()
  "Deletes the submode region point is currently in, if any."
  (interactive)
  (delete-overlay (mmm-overlay-at (point) 'all)))

(defun mmm-clear-regions (start stop)
  "Deletes all submode regions from START to STOP."
  (interactive "r")
  (mmm-clear-overlays start stop))

(defun mmm-clear-all-regions ()
  "Deletes all submode regions in the current buffer."
  (interactive)
  (mmm-clear-overlays))

;;}}}
;;{{{ End Current Region

(defun* mmm-end-current-region (&optional arg)
  "End current submode region.
If ARG is nil, end it at the most appropriate place, usually its
current back boundary. If ARG is non-nil, end it at point. If the
current region is correctly bounded, the first does nothing, but the
second deletes that delimiter as well.

If the region's BACK property is a string, it is inserted as above and
the overlay moved if necessary. If it is a function, it is called with
two arguments--the overlay, and \(if ARG 'middle t)--and must do the
entire job of this function."
  (interactive "P")
  (let ((ovl (mmm-overlay-at)))
    (when ovl
      (combine-after-change-calls
        (save-match-data
          (save-excursion
            (when (mmm-match-back ovl)
              (if arg
                  (replace-match "")
                (return-from mmm-end-current-region)))))
        (let ((back (overlay-get ovl 'back)))
          (cond ((stringp back)
                 (save-excursion
                   (unless arg (goto-char (overlay-end ovl)))
                   (save-excursion (insert back))
                   (move-overlay ovl (overlay-start ovl) (point))))
                ((functionp back)
                 (funcall back ovl (if arg 'middle t))))))
      (mmm-refontify-maybe (save-excursion (forward-line -1) (point))
                           (save-excursion (forward-line 1) (point))))))

;;}}}
;;{{{ Narrow to Region

(defun mmm-narrow-to-submode-region (&optional pos)
  "Narrow to the submode region at point."
  (interactive)
  ;; Probably don't use mmm-current-overlay here, because this is
  ;; sometimes called from inside messy functions.
  (let ((ovl (mmm-overlay-at pos)))
    (when ovl
      (narrow-to-region (overlay-start ovl) (overlay-end ovl)))))

;; The inverse command is `widen', usually on `C-x n w'

;;}}}

;; INSERTING REGIONS
;;{{{ Insert regions by keystroke

;; This is the "default" binding in the MMM Mode keymap. Keys defined
;; by classes should be control keys, to avoid conflicts with MMM
;; commands.
(defun mmm-insert-region (arg)
  "Insert a submode region based on last character in invoking keys.
Keystrokes after `mmm-mode-prefix-key' which are not bound to an MMM
Mode command \(see `mmm-command-modifiers') are passed on to this
function. If they have the modifiers `mmm-insert-modifiers', then they
are looked up, sans those modifiers, in all current submode classes to
find an insert skeleton. For example, in Mason, `p' \(with appropriate
prefix and modifiers) will insert a <%perl>...</%perl> region."
  (interactive "P")
  (let* ((seq (this-command-keys))
         (event (aref seq (1- (length seq))))
         (mods (event-modifiers event))
         (key (mmm-event-key event)))
    (if (subsetp mmm-insert-modifiers mods)
        (mmm-insert-by-key
         (append (set-difference mods mmm-insert-modifiers)
                 key)
         arg))))

(defun mmm-insert-by-key (key &optional arg)
  "Insert a submode region based on event KEY.
Inspects all the classes of the current buffer to find a matching
:insert key sequence. See `mmm-classes-alist'. ARG, if present, is
passed on to `skeleton-proxy-new' to control wrapping.

KEY must be a list \(MODIFIERS... . BASIC-KEY) where MODIFIERS are
symbols such as shift, control, etc. and BASIC-KEY is a character code
or a symbol such as tab, return, etc. Note that if there are no
MODIFIERS, the dotted list becomes simply BASIC-KEY."
  (multiple-value-bind (class skel str) (mmm-get-insertion-spec key)
    (when skel
      (let ((after-change-functions nil)
            (old-undo buffer-undo-list) undo)
        ;; XEmacs' skeleton doesn't manage positions by itself, so we
        ;; have to do it.
        (if mmm-xemacs (setq skeleton-positions nil))
        (skeleton-proxy-new skel str arg)
        (destructuring-bind (back end beg front) skeleton-positions
          ;; TODO: Find a way to trap invalid-parent signals from
          ;; make-region and undo the skeleton insertion.
          (let ((match-submode (plist-get class :match-submode))
		(match-face (plist-get class :match-face))
		(match-name (plist-get class :match-name))
		(front-form (regexp-quote (buffer-substring front beg)))
		(back-form (regexp-quote (buffer-substring end back)))
		submode face name)
	    (setq submode
		  (mmm-modename->function
		   (if match-submode
		       (mmm-save-all (funcall match-submode front-form))
		     (plist-get class :submode))))
	    (setq face
                  (cond ((functionp match-face)
                         (mmm-save-all
                          (funcall match-face front-form)))
                        (match-face
                         (cdr (assoc front-form match-face)))
                        (t
                         (plist-get class :face))))
	    (setq name
		  (cond ((plist-get class :skel-name)
			 ;; Optimize the name to the user-supplied str
			 ;; if we are so instructed.
			 str)
			;; Call it if it is a function
			((functionp match-name)
			 (mmm-save-all (funcall match-name front-form)))
			;; Now we know it's a string, does it need to
			;; be formatted?
			((plist-get class :save-name)
			 ;; Yes.  Haven't done a match before, so
			 ;; match the front regexp against the given
			 ;; form to format the string
			 (string-match (plist-get class :front)
				       front-form)
			 (mmm-format-matches match-name front-form))
			(t
			 ;; No, just use it as-is
			 match-name)))
            (mmm-make-region
             submode beg end 
	     :face face
	     :name name
	     :front front :back back
	     :match-front front-form :match-back back-form
	     :evaporation 'front
;;;             :beg-sticky (plist-get class :beg-sticky)
;;;             :end-sticky (plist-get class :end-sticky)
             :beg-sticky t :end-sticky t
             :creation-hook (plist-get class :creation-hook))
            (mmm-enable-font-lock submode)))
        ;; Now get rid of intermediate undo boundaries, so that the entire
        ;; insertion can be undone as one action.  This should really be
        ;; skeleton's job, but it doesn't do it.
        (setq undo buffer-undo-list)
        (while (not (eq (cdr undo) old-undo))
          (when (eq (cadr undo) nil)
            (setcdr undo (cddr undo)))
          (setq undo (cdr undo)))))))

(defun mmm-get-insertion-spec (key &optional classlist)
  "Get the insertion info for KEY from all classes in CLASSLIST.
Return \(CLASS SKEL STR) where CLASS is the class spec a match was
found in, SKEL is the skeleton to insert, and STR is the argument.
CLASSLIST defaults to the return value of `mmm-get-all-classes',
including global classes."
  (loop for classname in (or classlist (mmm-get-all-classes t))
        for class = (mmm-get-class-spec classname)
        for inserts = (plist-get class :insert)
        for skel = (cddr (assoc key inserts))
        with str
        ;; If SKEL is a dotted pair, it means call another key's
        ;; insertion spec with an argument.
        unless (consp (cdr skel))
        do (setq str (cdr skel)
                 skel (cddr (assoc (car skel) inserts)))
        if skel return (list class skel str)
        ;; If we have a group class, recurse.
        if (plist-get class :classes)
           if (mmm-get-insertion-spec key it)
              return it
           else
              return nil))

;;}}}
;;{{{ Help on Insertion

(defun mmm-insertion-help ()
  "Display help on currently available MMM insertion commands."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "Available MMM Mode Insertion Commands:\n")
    (princ "Key             Inserts\n")
    (princ "---             -------\n\n")
    (mapcar #'mmm-display-insertion-key
            (mmm-get-all-insertion-keys))))

(defun mmm-display-insertion-key (spec)
  "Print an insertion binding to standard output.
SPEC should be \(KEY NAME ...) where KEY is an insertion key and NAME
is a symbol naming the insertion."
  (let* ((str (make-string 16 ?\ ))
         ;; This gets us a dotted list, because of the way insertion
         ;; keys are specified.
         (key (append mmm-insert-modifiers (car spec)))
         (lastkey (nthcdr (max (1- (safe-length key)) 0) key)))
    ;; Now we make it a true list
    (if (consp key)
        (setcdr lastkey (list (cdr lastkey)))
      (setq key (list key)))
    ;; Get the spacing right
    (store-substring str 0
      (key-description
       (apply #'vector (append mmm-mode-prefix-key (list key)))))
    (princ str)
    ;; Now print the binding symbol
    (princ (cadr spec))
    (princ "\n")))

(defun mmm-get-all-insertion-keys (&optional classlist)
  "Return an alist of all currently available insertion keys.
Elements look like \(KEY NAME ...) where KEY is an insertion key and
NAME is a symbol naming the insertion."
  (remove-duplicates
   (loop for classname in (or classlist (mmm-get-all-classes t))
         for class = (mmm-get-class-spec classname)
         append (plist-get class :insert) into keys
         ;; If we have a group class, recurse.
         if (plist-get class :classes)
         do (setq keys (append keys (mmm-get-all-insertion-keys it)))
         finally return keys)
   :test #'equal
   :key #'(lambda (x) (cons (car x) (cadr x)))
   :from-end t))

;;}}}

;;{{{ Auto Insertion (copied from interactive session);-COM-
;-COM-
;-COM-;; Don't use `mmm-ify-region' of course. And rather than having
;-COM-;; classes define their own functions, we should have them pass a
;-COM-;; skeleton as an attribute. Then our insert function can turn off
;-COM-;; after-change hooks and add the submode region afterward.
;-COM-
;-COM-(define-skeleton mmm-see-inline
;-COM-  "" nil
;-COM-  -1 @ " " _ " " @ "%>"
;-COM-  '(apply #'mmm-ify-region 'cperl-mode (reverse skeleton-positions)))
;-COM-
;-COM-(define-skeleton mmm-see-other
;-COM-  "" nil
;-COM-  @ ";\n" _ "\n" @ "<%/" str ">"
;-COM-  '(apply #'mmm-ify-region 'cperl-mode (reverse skeleton-positions)))
;-COM-
;-COM-(make-local-hook 'after-change-functions)
;-COM-(add-hook 'after-change-functions 'mmm-detect t)
;-COM-
;-COM-(defun mmm-detect (beg end length)
;-COM-  (when (mmm-looking-back-at "<% ")
;-COM-    (mmm-see-inline))
;-COM-  (when (mmm-looking-back-at "<%\\(\\w+\\)>")
;-COM-    (mmm-see-other (match-string 1))))
;-COM-
;;}}}

(provide 'mmm-cmds)

;;; mmm-cmds.el ends here