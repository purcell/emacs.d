;;; mmm-class.el --- MMM submode class variables and functions

;; Copyright (C) 2000, 2004 by Michael Abraham Shulman

;; Author: Michael Abraham Shulman <viritrilbia@users.sourceforge.net>

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

;; This file contains variable and function definitions for
;; manipulating and applying MMM submode classes. See `mmm-vars.el'
;; for variables that list classes.

;;; Code:

(require 'cl)
(require 'mmm-vars)
(require 'mmm-region)

;;; CLASS SPECIFICATIONS
;;{{{ Get Class Specifications

(defun mmm-get-class-spec (class)
  "Get the class specification for CLASS.
CLASS can be either a symbol to look up in `mmm-classes-alist' or a
class specifier itself."
  (cond ((symbolp class)        ; A symbol must be looked up
         (or (cdr (assq class mmm-classes-alist))
             (and (cadr (assq class mmm-autoloaded-classes))
                  (load (cadr (assq class mmm-autoloaded-classes)))
                  (cdr (assq class mmm-classes-alist)))
             (signal 'mmm-invalid-submode-class (list class))))
        ((listp class)          ; A list must be a class spec
         class)
        (t (signal 'mmm-invalid-submode-class (list class)))))

;;}}}
;;{{{ Get and Set Class Parameters

(defun mmm-get-class-parameter (class param)
  "Get the value of the parameter PARAM for CLASS, or nil if none."
  (cadr (member param (mmm-get-class-spec class))))

(defun mmm-set-class-parameter (class param value)
  "Set the value of the parameter PARAM for CLASS to VALUE.
Creates a new parameter if one is not present."
  (let* ((spec (mmm-get-class-spec class))
         (current (member param spec)))
    (if current
        (setcar (cdr current) value)
      (nconc spec (list param value)))))

;;}}}
;;{{{ Apply Classes

(defun* mmm-apply-class
    (class &optional (start (point-min)) (stop (point-max)) face)
  "Apply the submode class CLASS from START to STOP in FACE.
If FACE is nil, the face for CLASS is used, or the default face if
none is specified by CLASS."
  ;; The "special" class t means do nothing. It is used to turn on
  ;; MMM Mode without applying any classes.
  (unless (eq class t)
    (apply #'mmm-ify :start start :stop stop
           (append (mmm-get-class-spec class)
                  (list :face face)))
    (mmm-run-class-hook class)
    ;; Hack in case class hook sets mmm-buffer-mode-display-name etc.
    (mmm-set-mode-line)))

(defun* mmm-apply-classes
    (classes &key (start (point-min)) (stop (point-max)) face)
  "Apply all submode classes in CLASSES, in order.
All classes are applied regardless of any errors that may occur in
other classes. If any errors occur, `mmm-apply-classes' exits with an
error once all classes have been applied."
  (let (invalid-classes)
    (dolist (class classes)
      (condition-case err
          (mmm-apply-class class start stop face)
        (mmm-invalid-submode-class
         ;; Save the name of the invalid class, so we can report them
         ;; all together at the end.
         (add-to-list 'invalid-classes (second err)))))
    (when invalid-classes
      (signal 'mmm-invalid-submode-class invalid-classes))))

;;}}}
;;{{{ Apply All Classes

(defun* mmm-apply-all (&key (start (point-min)) (stop (point-max)))
  "MMM-ify from START to STOP by all submode classes.
The classes come from mode/ext, `mmm-classes', `mmm-global-classes',
and interactive history."
  (mmm-clear-overlays start stop 'strict)
  (mmm-apply-classes (mmm-get-all-classes t) :start start :stop stop)
  (mmm-update-submode-region)
  (syntax-ppss-flush-cache start)
  (syntax-propertize stop)
  (mmm-refontify-maybe start stop))

;;}}}

;;; BUFFER SCANNING
;;{{{ Scan for Regions

(defun* mmm-ify
    (&rest all &key classes handler
	   submode match-submode
           (start (point-min)) (stop (point-max))
           front back save-matches (case-fold-search t)
           (beg-sticky (not (number-or-marker-p front)))
           (end-sticky (not (number-or-marker-p back)))
           include-front include-back
           (front-offset 0) (back-offset 0)
	   (front-delim nil) (back-delim nil)
	   (delimiter-mode mmm-delimiter-mode)
	   front-face back-face
           front-verify back-verify
           front-form back-form
	   creation-hook
           face match-face
	   save-name match-name
	   (front-match 0) (back-match 0)
	   end-not-begin
           ;insert private
           &allow-other-keys
           )
  "Create submode regions from START to STOP according to arguments.
If CLASSES is supplied, it must be a list of valid CLASSes. Otherwise,
the rest of the arguments are for an actual class being applied. See
`mmm-classes-alist' for information on what they all mean."
  ;; Make sure we get the default values in the `all' list.
  (setq all (append
             all
             (list :start start :stop stop
		   :beg-sticky beg-sticky :end-sticky end-sticky
		   :front-offset front-offset :back-offset back-offset
		   :front-delim front-delim :back-delim back-delim
		   :front-match 0 :back-match 0
		   )))
  (cond
   ;; If we have a class list, apply them all.
   (classes
    (mmm-apply-classes classes :start start :stop stop :face face))
   ;; Otherwise, apply this class.
   ;; If we have a handler, call it.
   (handler
    (apply handler all))
   ;; Otherwise, we search from START to STOP for submode regions,
   ;; continuining over errors, until we don't find any more. If FRONT
   ;; and BACK are number-or-markers, this should only execute once.
   (t
    (mmm-save-all
     (goto-char start)
     (loop for (beg end front-pos back-pos matched-front matched-back
                    matched-submode matched-face matched-name
                    invalid-resume ok-resume) =
                    (apply #'mmm-match-region :start (point) all)
           while beg
           if end	       ; match-submode, if present, succeeded.
           do
           (condition-case nil
               (progn
                 (mmm-make-region
		  (or matched-submode submode) beg end
		  :face (or matched-face face)
		  :front front-pos :back back-pos
		  :evaporation 'front
		  :match-front matched-front :match-back matched-back
		  :beg-sticky beg-sticky :end-sticky end-sticky
		  :name matched-name
		  :delimiter-mode delimiter-mode
		  :front-face front-face :back-face back-face
		  :creation-hook creation-hook
		  )
		 (goto-char ok-resume))
             ;; If our region is invalid, go back to the end of the
             ;; front match and continue on.
             (mmm-error (goto-char invalid-resume)))
           ;; If match-submode was unable to find a match, go back to
           ;; the end of the front match and continue on.
           else do (goto-char invalid-resume)
           )))))

;;}}}
;;{{{ Match Regions

(defun* mmm-match-region
    (&key start stop front back front-verify back-verify
          front-delim back-delim
          include-front include-back front-offset back-offset
          front-form back-form save-matches match-submode match-face
	  front-match back-match end-not-begin
	  save-name match-name
          &allow-other-keys)
  "Find the first valid region between point and STOP.
Return \(BEG END FRONT-POS BACK-POS FRONT-FORM BACK-FORM SUBMODE FACE
NAME INVALID-RESUME OK-RESUME) specifying the region.  See
`mmm-match-and-verify' for the valid values of FRONT and BACK
\(markers, regexps, or functions).  A nil value for END means that
MATCH-SUBMODE failed to find a valid submode.  INVALID-RESUME is the
point at which the search should continue if the region is invalid,
and OK-RESUME if the region is valid."
  (when (mmm-match-and-verify front start stop front-verify)
    (let ((beg (mmm-match->point include-front front-offset front-match))
	  (front-pos (if front-delim
			 (mmm-match->point t front-delim front-match)
		       nil))
          (invalid-resume (match-end front-match))
          (front-form (mmm-get-form front-form)))
      (let ((submode (if match-submode
                         (condition-case nil
                             (mmm-save-all
                              (funcall match-submode front-form))
                           (mmm-no-matching-submode
                            (return-from
                                mmm-match-region
                              (values beg nil nil nil nil nil nil nil nil
                                      invalid-resume nil))))
                       nil))
	    (name (cond ((functionp match-name)
			 (mmm-save-all (funcall match-name front-form)))
			((stringp match-name)
			 (if save-name
			     (mmm-format-matches match-name)
			   match-name))))
            (face (cond ((functionp match-face)
                         (mmm-save-all
                          (funcall match-face front-form)))
                        (match-face
                         (cdr (assoc front-form match-face))))))
        (when (mmm-match-and-verify
               (if save-matches
                   (mmm-format-matches back)
                 back)
               beg stop back-verify)
          (let* ((end (mmm-match->point (not include-back)
					back-offset back-match))
		 (back-pos (if back-delim
			       (mmm-match->point nil back-delim back-match)
			     nil))
		 (back-form (mmm-get-form back-form))
		 (ok-resume (if end-not-begin 
				(match-end back-match)
			      end)))
            (values beg end front-pos back-pos front-form back-form
		    submode face name
                    invalid-resume ok-resume)))))))

(defun mmm-match->point (beginp offset match)
  "Find a point of starting or stopping from the match data.  If
BEGINP, start at \(match-beginning MATCH), else \(match-end MATCH),
and move OFFSET.  Handles all values of OFFSET--see `mmm-classes-alist'."
  (save-excursion
    (goto-char (if beginp
		   (match-beginning match)
		 (match-end match)))
    (dolist (spec (if (listp offset) offset (list offset)))
      (if (numberp spec)
          (forward-char (or spec 0))
        (funcall spec)))
    (point)))

(defun mmm-match-and-verify (pos start stop &optional verify)
  "Find first match for POS between point and STOP satisfying VERIFY.
Return non-nil if a match was found, and set match data. POS can be a
number-or-marker, a regexp, or a function.

If POS is a number-or-marker, it is used as-is. If it is a string, it
is searched for as a regexp until VERIFY returns non-nil. If it is a
function, it is called with argument STOP and must return non-nil iff
a match is found, and set the match data. Note that VERIFY is ignored
unless POS is a regexp."
  (cond
   ;; A marker can be used as-is, but only if it's in bounds.
   ((and (number-or-marker-p pos) (>= pos start) (<= pos stop))
    (goto-char pos)
    (looking-at ""))            ; Set the match data
   ;; Strings are searched for as regexps.
   ((stringp pos)
    (loop always (re-search-forward pos stop 'limit)
          until (or (not verify) (mmm-save-all (funcall verify)))))
   ;; Otherwise it must be a function.
   ((functionp pos)
    (funcall pos stop))))

;;}}}
;;{{{ Get Delimiter Forms

(defun mmm-get-form (form)
  "Return the delimiter form specified by FORM.
If FORM is nil, call `mmm-default-get-form'. If FORM is a string,
return it. If FORM is a function, call it. If FORM is a list, return
its `car' \(usually in this case, FORM is a one-element list
containing a function to be used as the delimiter form."
  (cond ((stringp form) form)
        ((not form) (mmm-default-get-form))
        ((functionp form) (mmm-save-all (funcall form)))
        ((listp form) (car form))))

(defun mmm-default-get-form ()
  (regexp-quote (match-string 0)))

;;}}}

(provide 'mmm-class)

;;; mmm-class.el ends here
