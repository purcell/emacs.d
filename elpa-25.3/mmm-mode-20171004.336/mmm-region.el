;;; mmm-region.el --- Manipulating and behavior of MMM submode regions

;; Copyright (C) 2000 by Michael Abraham Shulman
;; Copyright (C) 2012, 2013 by Dmitry Gutov

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

;; This file provides the functions and variables to create, delete,
;; and inspect submode regions, as well as functions that make them
;; behave like the submode with respect to syntax tables, local maps,
;; font lock, etc.

;; See mmm-class.el for functions which scan the buffer and decide
;; where to create regions.

;;; Code:

(require 'cl)
(require 'font-lock)
(require 'mmm-compat)
(require 'mmm-utils)
(require 'mmm-auto)
(require 'mmm-vars)

;; INSPECTION
;;{{{ Current Overlays

;; Emacs counts an overlay starting at POS as "at" POS, but not an
;; overlay ending at POS. XEmacs is more sensible and uses beg- and
;; end-stickiness to determine whether an endpoint is within an
;; extent. Here we want to act like XEmacs does.

(defsubst mmm-overlay-at (&optional pos type)
  "Return the highest-priority MMM Mode overlay at POS.
See `mmm-included-p' for the values of TYPE."
  (car (mmm-overlays-at pos type)))

(defun mmm-overlays-at (&optional pos type)
  "Return a list of the MMM overlays at POS, in decreasing priority.
See `mmm-included-p' for the values of TYPE."
  (or pos (setq pos (point)))
  (mmm-sort-overlays
   (remove-if-not
    #'(lambda (ovl)
	(and (overlay-get ovl 'mmm)
	     (mmm-included-p ovl pos type)))
    ;; XEmacs complains about positions outside the buffer
    (overlays-in (max (1- pos) (point-min))
		 (min (1+ pos) (point-max))))))

(defun mmm-included-p (ovl pos &optional type)
  "Return true if the overlay OVL contains POS.

If OVL strictly contains POS, always return true.  If OVL starts or
ends at POS, return true or false based on the value of TYPE, which
should be one of nil, `beg', `end', `none', or `all'.
* If TYPE is nil, return true for an overlay starting at POS only if
  it is beg-sticky, and for one ending at POS only if it is end-sticky.
* If TYPE is `beg', return true for any overlay starting at POS but
  false for any ending at POS.
* If TYPE is `end', return true for any overlay ending at POS but
  false for any starting at POS.
* If TYPE is `all', return true for any overlay starting or ending at POS.
* If TYPE is `none' \(or any other value), return false for any
  overlay starting or ending at POS."
  (let ((beg (overlay-start ovl))
	(end (overlay-end ovl)))
    (cond ((and (= beg pos) (= end pos))
	   ;; Do the Right Thing for zero-width overlays
	   (case type
	     ((nil) (and (overlay-get ovl 'beg-sticky)
			 (overlay-get ovl 'end-sticky)))
	     ((none) nil)
	     (t t)))
	  ((= beg pos)
	   (case type
	     ((nil) (overlay-get ovl 'beg-sticky))
	     ((beg all) t)
	     (t nil)))
	  ((= end pos)
	   (case type
	     ((nil) (overlay-get ovl 'end-sticky))
	     ((end all) t)
	     (t nil)))
	  ((and (> end pos) (< beg pos))
	   t))))

;;; `mmm-overlays-in' has been retired as altogether too confusing a
;;; name, when what is really meant is one of the following three:

(defun mmm-overlays-containing (start stop)
  "Return all MMM overlays containing the region START to STOP.
The overlays are returned in order of decreasing priority.  No
attention is paid to stickiness."
  (mmm-sort-overlays
   (remove-if-not
    #'(lambda (ovl)
	(and (overlay-get ovl 'mmm)
	     (<= (overlay-start ovl) start)
	     (>= (overlay-end ovl) stop)))
    (overlays-in (max start (point-min))
		 (min stop (point-max))))))

(defun mmm-overlays-contained-in (start stop)
  "Return all MMM overlays entirely contained in START to STOP.
The overlays are returned in order of decreasing priority.  No
attention is paid to stickiness."
  (mmm-sort-overlays
   (remove-if-not
    #'(lambda (ovl)
	(and (overlay-get ovl 'mmm)
	     (>= (overlay-start ovl) start)
	     (<= (overlay-end ovl) stop)))
    (overlays-in (max start (point-min))
		 (min stop (point-max))))))

(defun mmm-overlays-overlapping (start stop)
  "Return all MMM overlays overlapping the region START to STOP.
The overlays are returned in order of decreasing priority.  No
attention is paid to stickiness."
  (mmm-sort-overlays
   (remove-if-not
    #'(lambda (ovl)
	(overlay-get ovl 'mmm))
    (overlays-in (max start (point-min))
		 (min stop (point-max))))))

(defun mmm-sort-overlays (overlays)
  "Sort OVERLAYS in order of decreasing priority."
  (sort (copy-list overlays)
        #'(lambda (x y) (> (or (overlay-get x 'priority) 0)
                           (or (overlay-get y 'priority) 0)))))

;;}}}
;;{{{ Current Submode

(defvar mmm-current-overlay nil
  "What submode region overlay we think we are currently in.
May be out of date; call `mmm-update-current-submode' to correct it.")
(make-variable-buffer-local 'mmm-current-overlay)

(defvar mmm-previous-overlay nil
  "What submode region overlay we were in just before this one.
Set by `mmm-update-current-submode'.")
(make-variable-buffer-local 'mmm-previous-overlay)

(defvar mmm-current-submode nil
  "What submode we think we are currently in.
May be out of date; call `mmm-update-current-submode' to correct it.")
(make-variable-buffer-local 'mmm-current-submode)

(defvar mmm-previous-submode nil
  "What submode we were in just before this one.
Set by `mmm-update-current-submode'.")
(make-variable-buffer-local 'mmm-previous-submode)

(defun mmm-update-current-submode (&optional pos)
  "Update current and previous position variables to POS, or point.
Return non-nil if the current region changed.

Also deletes overlays that ought to evaporate because their delimiters
have disappeared."
  (mapc #'delete-overlay
	(remove-if #'(lambda (ovl)
		       (or (not (eq (overlay-get ovl 'mmm-evap) 'front))
			   (overlay-buffer (overlay-get ovl 'front))))
		   (mmm-overlays-at pos)))
  (let ((ovl (mmm-overlay-at pos)))
    (if (eq ovl mmm-current-overlay)
        nil
      (mmm-set-current-pair (if ovl (overlay-get ovl 'mmm-mode)) ovl)
      t)))

(defun mmm-set-current-pair (mode ovl)
  "Set the current submode to MODE, the current overlay to OVL
and update the saved previous values."
  (setq mmm-previous-overlay mmm-current-overlay
        mmm-previous-submode mmm-current-submode)
  (setq mmm-current-submode mode
        mmm-current-overlay ovl))

(defun mmm-submode-at (&optional pos type)
  "Return the submode at POS \(or point), or NIL if none.
See `mmm-included-p' for values of TYPE."
  (let ((ovl (mmm-overlay-at pos type)))
    (if ovl (overlay-get ovl 'mmm-mode))))

;;}}}
;;{{{ Delimiter Matching and Boundaries

(defun mmm-match-front (ovl)
  "Return non-nil if the front delimiter of OVL matches as it should.
Sets the match data to the front delimiter, if it is a regexp.
Otherwise, calls it as a function with point at the beginning of the
front delimiter overlay \(i.e. where the front delimiter ought to
start) and one argument being the region overlay. The function should
return non-nil if the front delimiter matches correctly, and set the
match data appropriately."
  (let* ((front-ovl (overlay-get ovl 'front))
	 (front (if front-ovl (overlay-get front-ovl 'match))))
    (when front
      (save-excursion
	(goto-char (overlay-start front-ovl))
	(if (stringp front)
	    ;; It's a regexp
	    (looking-at front)
	  ;; It's a function
	  (funcall front ovl))))))

(defun mmm-match-back (ovl)
  "Return non-nil if the back delimiter of OVL matches as it should.
Sets the match data to the back delimiter, if it is a regexp.
Otherwise, calls it as a function with point at the beginning of the
back delimiter overlay \(i.e. where the back delimiter ought to start)
and one argument being the region overlay. The function should return
non-nil if the back delimiter matches correctly, and set the match
data appropriately."
  (let* ((back-ovl (overlay-get ovl 'back))
	 (back (if back-ovl (overlay-get back-ovl 'match))))
    (when back
      (save-excursion
	(goto-char (overlay-start back-ovl))
	(if (stringp back)
	    ;; It's a regexp
	    (looking-at back)
	  ;; It's a function
	  (funcall back ovl))))))

(defun mmm-front-start (ovl)
  "Return the position at which the front delimiter of OVL starts."
  (let ((front (overlay-get ovl 'front)))
    ;; Overlays which have evaporated become "overlays in no buffer"
    (if (and front (overlay-buffer front))
	(overlay-start front)
      (overlay-start ovl))))

(defun mmm-back-end (ovl)
  "Return the position at which the back delimiter of OVL ends."
  (let ((back (overlay-get ovl 'back)))
    ;; Overlays which have evaporated become "overlays in no buffer"
    (if (and back (overlay-buffer back))
	(overlay-end back)
      (overlay-end ovl))))

;;}}}

;; CREATION & DELETION
;;{{{ Make Submode Regions

(defun mmm-valid-submode-region (submode beg end)
  "Check if the region between BEG and END is valid for SUBMODE.
This region must be entirely contained within zero or more existing
submode regions, none of which start or end inside it, and it must be
a valid child of the highest-priority of those regions, if any.
Signals errors, returns `t' if no error."
  ;; First check if the placement is valid.  Every existing region
  ;; that overlaps this one must contain it in its entirety.
  (let ((violators (set-difference
		    (mmm-overlays-overlapping beg end)
		    (mmm-overlays-containing beg end))))
    (if violators
	(signal 'mmm-subregion-invalid-placement
		violators)))
  ;; Now check if it is inside a valid parent
  (let ((parent-mode (mmm-submode-at beg 'beg)))
    (and parent-mode
	 ;; TODO: Actually check parents here.  For present purposes,
	 ;; we just make sure we aren't putting a submode inside one
	 ;; of the same type.  Actually, what we should really be
	 ;; doing is checking classes/names of regions, not just the
	 ;; submodes.
	 (eq submode parent-mode)
	 (signal 'mmm-subregion-invalid-parent
		 (list parent-mode))))
  t)

(defun* mmm-make-region
    (submode beg end &key face
	     front back (evaporation 'front)
	     delimiter-mode front-face back-face
	     display-name
	     (match-front "") (match-back "")
             (beg-sticky t) (end-sticky t)
	     name creation-hook
             )
  "Make a submode region from BEG to END of SUBMODE.

BEG and END are buffer positions or markers with BEG <= END \(although
see EVAPORATION below).  SUBMODE is a major mode function or a valid
argument to `mmm-modename->function'.  FACE is a valid display face.

FRONT and BACK specify the positions of the front and back delimiters
for this region, if any.  If FRONT is a buffer position or marker, the
front delimiter runs from it to BEG.  FRONT can also be a two-element
list \(FRONT-BEG FRONT-END) specifying the exact position of the front
delimiter.  One must have FRONT-BEG < FRONT-END <= BEG.

Similarly, BACK may be a buffer position or marker, in which case the
back delimiter runs from END to BACK.  BACK can also be a two-element
list \(BACK-BEG BACK-END) specifying the exact position, in which case
we must have END <= BACK-BEG < BACK-END.

EVAPORATION specifies under what conditions this submode region should
disappear.
* If `nil', the region never disappears.  This can cause serious
  problems when using cut-and-paste and is not recommended.
* If the value is t, the region disappears whenever it has zero
  length.  This is recommended for manually created regions used for
  temporary editing convenience.
* If the value is `front', the region will disappear whenever the text
  in its front delimiter disappears, that is, whenever the overlay
  which marks its front delimiter has zero width.
The default value is `front'.  However, if the parameter FRONT is nil,
then this makes no sense, so the default becomes `t'.  Note that if
EVAPORATION is `t', then an error is signalled if BEG = END.

MATCH-FRONT \(resp. MATCH-BACK) is a regexp or function to match the
correct delimiters, see `mmm-match-front' \(resp. `mmm-match-back').
It is ignored if FRONT \(resp. BACK) is nil.  At present these are not
used much.

DELIMITER-MODE specifies the major mode to use for delimiter regions.
A `nil' value means they remain in the primary mode.

FACE, FRONT-FACE, and BACK-FACE, are faces to use for the region, the
front delimiter, and the back delimiter, respectively, under high
decoration \(see `mmm-submode-decoration-level').

BEG-STICKY and END-STICKY determine whether the front and back of the
region, respectively, are sticky with respect to new insertion.  The
default is yes.

NAME is a string giving the \"name\" of this submode region.  Submode
regions with the same name are considered part of the same code
fragment and formatted accordingly.

DISPLAY-NAME is a string to display in the mode line when point is in
this submode region.  If nil or not given, the name associated with
SUBMODE is used.  In delimiter regions, \"--\" is shown.

CREATION-HOOK should be a function to run after the region is created,
with point at the start of the new region."
  ;; Check placement of region and delimiters
  (unless (if (eq evaporation t)
              (< beg end)
            (<= beg end))
    (signal 'mmm-subregion-invalid-placement (list beg end)))
  (when front
    (unless (listp front)
      (setq front (list front beg)))
    (unless (and (< (car front) (cadr front))
		 (<= (cadr front) beg))
      (signal 'mmm-subregion-invalid-placement front)))
  (when back
    (unless (listp back)
      (setq back (list end back)))
    (unless (and (< (car back) (cadr back))
		 (<= end (car back)))
      (signal 'mmm-subregion-invalid-placement back)))
  (setq submode (mmm-modename->function submode))
  ;; Check embedding in existing regions
  (mmm-valid-submode-region submode beg end)
  (mmm-mode-on)
  (when submode
    (mmm-update-mode-info submode))
  (and (not front) (eq evaporation 'front) (setq evaporation t))
  (let ((region-ovl
	 (mmm-make-overlay submode beg end name face beg-sticky end-sticky
			   (or (eq evaporation t) nil) display-name)))
    ;; Save evaporation type for checking later
    (overlay-put region-ovl 'mmm-evap evaporation)
    ;; Calculate priority to supersede anything already there.
    ;; XXX: Actually, don't, in order not to hide the region highlighting.
    ;; Let's try omitting the priorities and see if any problems crop up.
    ;; (overlay-put region-ovl 'priority (length (mmm-overlays-at beg)))
    ;; Make overlays for the delimiters, with appropriate pointers.
    (when front
      (let ((front-ovl
	     (mmm-make-overlay delimiter-mode (car front) (cadr front)
			       nil front-face nil nil t "--" t)))
	(overlay-put region-ovl 'front front-ovl)
	(overlay-put front-ovl 'region region-ovl)
	(overlay-put front-ovl 'match match-front)))
    (when back
      (let ((back-ovl
	     (mmm-make-overlay delimiter-mode (car back) (cadr back)
			       nil back-face nil nil t "--" t)))
	(overlay-put region-ovl 'back back-ovl)
	(overlay-put back-ovl 'region region-ovl)
	(overlay-put back-ovl 'match match-back)))
    ;; Update everything and run all the hooks
    (mmm-save-all
     ;; Can be nil when a zero-width region is immediately evaporated
     (when (overlay-start region-ovl)
       (goto-char (overlay-start region-ovl)))
     (mmm-set-current-pair submode region-ovl)
     (mmm-set-local-variables submode region-ovl)
     (mmm-run-submode-hook submode)
     (when creation-hook
       (funcall creation-hook)))
    (mmm-update-submode-region)
    region-ovl))

(defun mmm-make-overlay (submode beg end name face beg-sticky end-sticky evap
				 &optional display-name delim)
  "Internal function to make submode overlays.
Does not handle delimiters.  Use `mmm-make-region'."
  (let ((ovl (make-overlay beg end nil (not beg-sticky) end-sticky)))
    (mapc
     #'(lambda (pair) (overlay-put ovl (car pair) (cadr pair)))
     `((mmm t)				; Mark all submode overlays
       (mmm-mode ,submode)
       ,@(if delim '((delim t)) nil)
       (mmm-local-variables
	;; Have to be careful to make new list structure here
	,(list* (list 'font-lock-cache-state nil)
		(list 'font-lock-cache-position (make-marker))
		(copy-tree
		 (cdr (assq submode mmm-region-saved-locals-defaults)))))
       (name ,name)
       (display-name ,display-name)
       ;; Need to save these, because there's no way of accessing an
       ;; overlay's official "front-advance" parameter once it's created.
       (beg-sticky ,beg-sticky)
       (end-sticky ,end-sticky)
       ;; These have special meaning to Emacs
       (,mmm-evaporate-property ,evap)
       (face ,(mmm-get-face face submode delim))
       ))
    ovl))

(defun mmm-get-face (face submode &optional delim)
  (cond ((= mmm-submode-decoration-level 0) nil)
	((and (= mmm-submode-decoration-level 2) face) face)
	(delim 'mmm-delimiter-face)
	(submode 'mmm-default-submode-face)))

;;}}}
;;{{{ Clear Overlays

;; See also `mmm-clear-current-region'.

(defun mmm-clear-overlays (&optional start stop strict)
  "Clears all MMM overlays overlapping START and STOP.
If STRICT, only clear those entirely included in that region."
  (mapc #'delete-overlay
        (if strict
            (mmm-overlays-contained-in (or start (point-min))
                                       (or stop (point-max)))
          (mmm-overlays-overlapping (or start (point-min))
                                    (or stop (point-max)))))
  (mmm-update-submode-region))

;;}}}

;; BASIC UPDATING
;;{{{ Submode Info

(defun mmm-update-mode-info (mode &optional force)
  "Save the global-saved and buffer-saved variables for MODE.
Global saving is done on properties of the symbol MODE and buffer
saving in `mmm-buffer-saved-locals'.  This function must be called for
both the dominant mode and all submodes, in each file.  Region-saved
variables are initialized from `mmm-region-saved-locals-defaults',
which is set here as well.  See `mmm-save-local-variables'.  If FORCE
is non-nil, don't quit if the info is already there."
  (let ((buffer-entry (assq mode mmm-buffer-saved-locals))
        (region-entry (assq mode mmm-region-saved-locals-defaults))
        global-vars buffer-vars region-vars
        ;; http://debbugs.gnu.org/13836
        buffer-file-truename)
    (unless (and (not force)
                 (get mode 'mmm-local-variables)
                 buffer-entry
                 region-entry)
      (let ((temp-buffer (mmm-make-temp-buffer (current-buffer)
                                               mmm-temp-buffer-name))
            (filename (buffer-file-name))
            (mmm-in-temp-buffer t)
            ;; Don't try to use jit-lock, it's automatically disabled
            ;; starting with 24.4 anyway.
            font-lock-support-mode)
        (unwind-protect
            (with-current-buffer temp-buffer
              ;; Handle stupid modes that need the file name set.
              (when (memq mode mmm-set-file-name-for-modes)
                (setq buffer-file-name filename))
              (funcall mode)
              (when (featurep 'font-lock)
                (put mode 'mmm-font-lock-mode font-lock-mode)
                ;; These can't be in the local variables list, because we
                ;; replace their actual values, but we want to use their
                ;; original values elsewhere.
                (put mode 'mmm-fontify-region-function
                     font-lock-fontify-region-function)
                (put mode 'mmm-beginning-of-syntax-function
                     syntax-begin-function)
                (put mode 'mmm-syntax-propertize-function
                     (and (boundp 'syntax-propertize-function)
                          syntax-propertize-function))
                (put mode 'mmm-indent-line-function indent-line-function))
              ;; Get variables
              (setq global-vars (mmm-get-locals 'global)
                    buffer-vars (mmm-get-locals 'buffer)
                    region-vars (mmm-get-locals 'region))
              (put mode 'mmm-mode-name mode-name))
          (kill-buffer temp-buffer)))
      (put mode 'mmm-local-variables global-vars)
      (if buffer-entry
          (setcdr buffer-entry buffer-vars)
        (push (cons mode buffer-vars) mmm-buffer-saved-locals))
      (if region-entry
          (setcdr region-entry region-vars)
        (push (cons mode region-vars)
              mmm-region-saved-locals-defaults)))))

;;}}}
;;{{{ Updating Hooks

(defun mmm-update-submode-region ()
  "Update all MMM properties correctly for the current position.
This function and those it calls do the actual work of setting the
different keymaps, syntax tables, local variables, etc. for submodes."
  (when (mmm-update-current-submode)
    (mmm-save-changed-local-variables mmm-previous-submode
                                      mmm-previous-overlay)
    (let ((new-mode (or mmm-current-submode mmm-primary-mode))
          (old-mode (or mmm-previous-submode mmm-primary-mode)))
      (mmm-run-constructed-hook old-mode new-mode)
      (mmm-run-constructed-hook old-mode "exit")
      (mmm-run-constructed-hook new-mode "enter")
      (mmm-update-mode-info new-mode)
      (mmm-set-local-variables new-mode mmm-current-overlay)
      (mmm-enable-font-lock new-mode))
    (mmm-set-mode-line)
    (dolist (func (if mmm-current-overlay
		      (overlay-get mmm-current-overlay 'entry-hook)
		    mmm-primary-mode-entry-hook))
      (ignore-errors (funcall func)))))

(defun mmm-add-hooks ()
  (if (featurep 'xemacs)
      (make-local-hook 'post-command-hook))
  (add-hook 'post-command-hook 'mmm-update-submode-region nil t)
  (when mmm-parse-when-idle
    (add-hook 'pre-command-hook 'mmm-mode-reset-timer nil t)
    (add-hook 'after-change-functions 'mmm-mode-edit nil t)))

(defun mmm-remove-hooks ()
  (remove-hook 'post-command-hook 'mmm-update-submode-region t)
  (remove-hook 'pre-command-hook 'mmm-mode-reset-timer t)
  (remove-hook 'after-change-functions 'mmm-mode-edit t))

;;}}}
;;{{{ Local Variables

(defun mmm-get-local-variables-list (type mode)
  "Filter `mmm-save-local-variables' to match TYPE and MODE.
Return a list \(VAR ...).  In some cases, VAR will be a cons cell
\(GETTER . SETTER) -- see `mmm-save-local-variables'."
  (mapcan #'(lambda (element)
              (and (if (and (consp element)
                            (cdr element)
                            (cadr element))
                       (eq (cadr element) type)
                     (eq type 'global))
                   (if (and (consp element)
                            (cddr element)
                            (not (eq (caddr element) t)))
                       (if (functionp (caddr element))
                           (funcall (caddr element))
                         (member mode (caddr element)))
                     t)
                   (list (if (consp element) (car element) element))))
          mmm-save-local-variables))

(defun mmm-get-locals (type)
  "Get the local variables and values for TYPE from this buffer.
Return \((VAR VALUE) ...).  In some cases, VAR will be of the form
\(GETTER . SETTER) -- see `mmm-save-local-variables'."
  (mapcan #'(lambda (var)
              (if (consp var)
                  `((,var ,(funcall (car var))))
                (and (boundp var)
                     ;; This seems logical, but screws things up.
                     ;;(local-variable-p var)
                     `((,var ,(symbol-value var))))))
          (mmm-get-local-variables-list type major-mode)))

;; FIXME: Has no callers. Used for debugging?
(defun mmm-get-saved-local (mode ovl var)
  "Get the value of the local variable VAR saved for MODE and OVL, if any."
  (cadr (assq var (mmm-get-saved-local-variables ovl mode))))

;; FIXME: It's too easy to accidentally pass nil as MODE here.
;; We probably should call this from `mmm-set-current-pair', and not
;; rely on its callers to default to the primary mode when appropriate.
;; Also, incorporate the opmimization from `mmm-fontify-region-list'.
(defun mmm-set-local-variables (mode ovl)
  "Set all the local variables saved for MODE and OVL.
Looks up global, buffer and region saves.  When MODE is nil, just
the region ones."
  (mapcar #'(lambda (var)
              ;; (car VAR) may be (GETTER . SETTER)
              (if (consp (car var))
                  (funcall (cdar var) (cadr var))
                (make-local-variable (car var))
                (set (car var) (cadr var))))
          (mmm-get-saved-local-variables mode ovl)))

;; Used for debugging.
(defun mmm-diff-local-variables (mode ovl)
  (let (res)
    (mapc (lambda (var)
            (let ((current-value (if (consp (car var))
                                     (funcall (caar var))
                                   (symbol-value (car var)))))
              (unless (equal current-value (cadr var))
                (push
                 (message "var: %s, current: %s, saved: %s" (car var)
                          current-value (cadr var))
                 res))))
          (mmm-get-saved-local-variables mode ovl))
    res))

(defun mmm-get-saved-local-variables (mode ovl)
  (append (get mode 'mmm-local-variables)
          (cdr (assq mode mmm-buffer-saved-locals))
          (if ovl
              (overlay-get ovl 'mmm-local-variables)
            mmm-region-saved-locals-for-dominant)))

(defun mmm-save-changed-local-variables (mode ovl)
  "Save by-buffer and by-region variables for MODE and OVL.
Called when we move to a new submode region, with MODE and OVL the
region and mode for the previous position."
  (let ((buffer-vars (cdr (assq (or mode mmm-primary-mode)
                                mmm-buffer-saved-locals)))
        (region-vars (if ovl
                         (overlay-get ovl 'mmm-local-variables)
                       mmm-region-saved-locals-for-dominant))
        (set-local-value
         #'(lambda (var)
             (setcar (cdr var)
                     ;; (car VAR) may be (GETTER . SETTER)
                     (if (consp (car var))
                         (funcall (caar var))
                       (symbol-value (car var)))))))
    (mapc set-local-value buffer-vars)
    (mapc set-local-value region-vars)))

(defun mmm-clear-local-variables ()
  "Clear all buffer- and region-saved variables for current buffer."
  (setq mmm-buffer-saved-locals ()
        mmm-region-saved-locals-defaults ()
        mmm-region-saved-locals-for-dominant ()))

;;}}}

;; FONT LOCK
;;{{{ Enable Font Lock

(defun mmm-enable-font-lock (mode)
  "Turn on font lock if it is not already on and MODE enables it."
  (mmm-update-mode-info mode)
  (and (not font-lock-mode)
       (get mode 'mmm-font-lock-mode)
       (font-lock-mode 1)))

(defun mmm-update-font-lock-buffer ()
  "Turn on font lock if any mode in the buffer enables it."
  (if (some #'(lambda (mode)
                (get mode 'mmm-font-lock-mode))
            (cons mmm-primary-mode
                  (mapcar #'(lambda (ovl)
                              (overlay-get ovl 'mmm-mode))
                          (mmm-overlays-overlapping
                           (point-min) (point-max)))))
      (font-lock-mode 1)
    (font-lock-mode 0)))

(defun mmm-refontify-maybe (&optional start stop)
  "Re-fontify from START to STOP, or entire buffer, if enabled."
  (and font-lock-mode
       (if (or start stop)
           (font-lock-fontify-region (or start (point-min))
                                     (or stop (point-max)))
         (font-lock-fontify-buffer))))

;;}}}
;;{{{ Get Submode Regions

;;; In theory, these are general functions that have nothing to do
;;; with font-lock, but they aren't used anywhere else, so we might as
;;; well have them close.

(defun mmm-submode-changes-in (start stop)
  "Return a list of all submode-change positions from START to STOP.
The list is sorted in order of increasing buffer position."
  (let ((changes (sort (remove-duplicates
                        (mapcan #'(lambda (ovl)
                                    `(,(overlay-start ovl)
                                      ,(overlay-end ovl)))
                                (mmm-overlays-overlapping start stop)))
                       #'<)))
    (when (or (not changes) (< start (car changes)))
      (push start changes))
    (let ((last (last changes)))
      (when (> stop (car last))
        (setcdr last (list stop))))
    changes))

(defun mmm-regions-in (start stop)
  "Return a list of regions of the form (MODE BEG END OVL) whose disjoint
union covers the region from START to STOP, including delimiters."
  (when (> stop start)
    (let ((regions
           (maplist #'(lambda (pos-list)
                        (when (cdr pos-list)
                          (let ((ovl (mmm-overlay-at (car pos-list) 'beg)))
                            (list (if ovl
                                      (overlay-get ovl 'mmm-mode)
                                    mmm-primary-mode)
                                  (car pos-list) (cadr pos-list)
                                  ovl))))
                    (mmm-submode-changes-in start stop))))
      (setcdr (last regions 2) nil)
      regions)))

(defun mmm-regions-alist (start stop)
  "Return a list of lists of the form \(MODE . REGIONS) where REGIONS
is a list of elements of the form \(BEG END OVL). The disjoint union all
of the REGIONS covers START to STOP."
  (let ((regions (mmm-regions-in start stop))
        alist)
    (mapc (lambda (region)
            (let* ((mode (car region))
                   (elem (cdr region))
                   (kv (assoc mode alist)))
              (if kv
                  (push elem (cdr kv))
                (push (cons mode (list elem)) alist))))
          regions)
    (mapcar (lambda (kv)
              (cons (car kv) (nreverse (cdr kv))))
            alist)))

;;}}}
;;{{{ Fontify Regions

(defun mmm-fontify-region (start stop &optional loudly)
  "Fontify from START to STOP keeping track of submodes correctly."
  (let ((saved-mode mmm-current-submode)
        (saved-ovl  mmm-current-overlay))
    (unwind-protect
        (progn
          (when loudly
            (message "Fontifying %s with submode regions..." (buffer-name)))
          ;; Necessary to catch changes in font-lock cache state and position.
          (mmm-save-changed-local-variables
           mmm-current-submode mmm-current-overlay)
          (mapc #'(lambda (elt)
                    (when (get (car elt) 'mmm-font-lock-mode)
                      (mmm-fontify-region-list (car elt) (cdr elt))))
                (mmm-regions-alist start stop)))
      ;; `post-command-hook' contains `mmm-update-submode-region',
      ;; but jit-lock runs later, so we need to restore local vars now.
      (mmm-set-current-pair saved-mode saved-ovl)
      (mmm-set-local-variables (or saved-mode mmm-primary-mode) saved-ovl)))
  (when loudly (message nil)))

(defun mmm-fontify-region-list (mode regions)
  "Fontify REGIONS, each like \(BEG END), in mode MODE."
  (save-excursion
    (let ((func (get mode 'mmm-fontify-region-function))
          font-lock-extend-region-functions)
      (mapc #'(lambda (reg)
                (destructuring-bind (beg end ovl) reg
                  (goto-char beg)
                  ;; Here we do the same sort of thing that
                  ;; `mmm-update-submode-region' does, but we force it
                  ;; to use a specific mode, and don't save anything,
                  ;; fontify, or change the mode line.
                  (mmm-set-current-pair mode ovl)
                  (mmm-set-local-variables (unless (eq mmm-previous-submode mode)
                                             mode)
                                           mmm-current-overlay)
                  (save-restriction
                    (let ((font-lock-dont-widen t)
                          syntax-ppss-last syntax-ppss-cache)
                      ;; TODO: Remove this conditional when cc-mode
                      ;; respects submode boundaries.
                      (when (and ovl (not (memq mode mmm-c-derived-modes)))
                        (narrow-to-region beg end))
                      (funcall func beg end nil)))
                  ;; Catch changes in font-lock cache.
                  (mmm-save-changed-local-variables
                   mmm-current-submode mmm-current-overlay)))
              regions))))

;;}}}
;;{{{ Syntax

(defvar mmm-after-syntax-propertize-functions nil
  "List of functions to call after applying `syntax-table' text
properties to a submode region. It is passed four arguments: the
region overlay, the submode and the bounds of the region.")

(defun mmm-syntax-propertize-function (start stop)
  "Composite function that applies `syntax-table' text properties.
It iterates over all submode regions between START and STOP and
calls each respective submode's `syntax-propertize-function'."
  (let ((saved-mode mmm-current-submode)
        (saved-ovl  mmm-current-overlay))
    (mmm-save-changed-local-variables
     mmm-current-submode mmm-current-overlay)
    (unwind-protect
        (mapc #'(lambda (elt)
                  (let* ((mode (car elt))
                         (func (get mode 'mmm-syntax-propertize-function))
                         (beg (cadr elt)) (end (caddr elt))
                         (ovl (cadddr elt))
                         syntax-ppss-cache
                         syntax-ppss-last)
                    (goto-char beg)
                    (mmm-set-current-pair mode ovl)
                    (mmm-set-local-variables mode mmm-current-overlay)
                    (save-restriction
                      (when mmm-current-overlay
                        (narrow-to-region (overlay-start mmm-current-overlay)
                                          (overlay-end mmm-current-overlay)))
                      (cond
                       (func
                        (funcall func beg end))
                       (font-lock-syntactic-keywords
                        (let ((syntax-propertize-function nil))
                          (font-lock-fontify-syntactic-keywords-region beg end))))
                      (run-hook-with-args 'mmm-after-syntax-propertize-functions
                                          mmm-current-overlay mode beg end))))
              (mmm-regions-in start stop))
      (mmm-set-current-pair saved-mode saved-ovl)
      (mmm-set-local-variables (or saved-mode mmm-primary-mode) saved-ovl))))

;;}}}
;;{{{ Indentation

(defvar mmm-indent-line-function 'mmm-indent-line
  "The function to call to indent a line.
This will be the value of `indent-line-function' for the whole
buffer. It's supposed to delegate to the appropriate submode's
indentation function. See `mmm-indent-line' as the starting point.")

(defun mmm-indent-line-narrowed ()
  "An indent function which works on some modes where `mmm-indent-line' doesn't.
Works like `mmm-indent-line', but narrows the buffer before indenting to
appease modes which rely on constructs like (point-min) to indent."
  (interactive)
  (funcall
   (save-excursion
     (back-to-indentation)
     (mmm-update-submode-region)
     (let ((indent-function (get
                             (if (and mmm-current-overlay
                                      (> (overlay-end mmm-current-overlay) (point)))
                                 mmm-current-submode
                               mmm-primary-mode)
                             'mmm-indent-line-function)))
       (if mmm-current-overlay
           (save-restriction
             (narrow-to-region (overlay-start mmm-current-overlay)
                               (overlay-end mmm-current-overlay))
             indent-function)
         indent-function)))))

(defun mmm-indent-line ()
  (interactive)
  (funcall
   (save-excursion
      (back-to-indentation)
      (mmm-update-submode-region)
      (get
       (if (and mmm-current-overlay
                (> (overlay-end mmm-current-overlay) (point)))
           mmm-current-submode
         mmm-primary-mode)
       'mmm-indent-line-function))))

;;}}}
(provide 'mmm-region)

;;; mmm-region.el ends here
