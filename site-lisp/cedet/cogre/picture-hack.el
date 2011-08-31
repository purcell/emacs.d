;;; picture-hack.el --- Updates to picture mode

;;; Copyright (C) 2001, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: picture
;; X-RCS: $Id: picture-hack.el,v 1.10 2009/01/20 03:40:43 zappo Exp $

;; Semantic is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Picture-hack is a series of modifications to functions in picture.el
;; and rect.el.
;; It also contains new functions which should live in picture.el
;;
;; These are hacks needed by COGRE.  Long term, I would like to see
;; these features merged back into picture mode.

(require 'picture)
(require 'rect)

;;; Code:

;;; XEmacs is missing some stuff
;;
(unless (fboundp 'picture-current-line)
  ;; copied from Emacs 20.6:
  (defun picture-current-line ()
    "Return the vertical position of point.  Top line is 1."
    (+ (count-lines (point-min) (point))
       (if (= (current-column) 0) 1 0))))

(unless (fboundp 'picture-update-desired-column)
  ;; copied from Emacs 20.6:
  ;; If the value of picture-desired-column is far from the current
  ;; column, or if the arg ADJUST-TO-CURRENT is non-nil, set it to the
  ;; current column.   Return the current column.
  (defun picture-update-desired-column (adjust-to-current)
    (let ((current-column (current-column)))
      (if (or adjust-to-current
              (< picture-desired-column (1- current-column))
              (> picture-desired-column (1+ current-column)))
          (setq picture-desired-column current-column))
      current-column)))

(unless (fboundp 'char-width)
  (defun char-width (CH)
    "XEmacs doesn't have this, always return 1."
    1))

(unless (boundp 'picture-rectangle-v)
  (defcustom picture-rectangle-v   ?|
    "*Character `picture-draw-rectangle' uses for vertical lines."
    :type 'character
    :group 'picture))

(unless (boundp 'picture-rectangle-h)
  (defcustom picture-rectangle-h   ?-
    "*Character `picture-draw-rectangle' uses for horizontal lines."
    :type 'character
    :group 'picture))

(unless (boundp 'picture-rectangle-ctl)
  (defcustom picture-rectangle-ctl   ?+
    "*Character `picture-draw-rectangle' uses for top left corners."
    :type 'character
    :group 'picture))

(unless (boundp 'picture-rectangle-ctr)
  (defcustom picture-rectangle-ctr ?+
    "*Character `picture-draw-rectangle' uses for top right corners."
    :type 'character
    :group 'picture)
  )

(unless (boundp 'picture-rectangle-cbr)
  (defcustom picture-rectangle-cbr ?+
    "*Character `picture-draw-rectangle' uses for bottom right corners."
    :type 'character
    :group 'picture)
  )

(unless (boundp 'picture-rectangle-cbl)
  (defcustom picture-rectangle-cbl ?+
    "*Character `picture-draw-rectangle' uses for bottom left corners."
    :type 'character
    :group 'picture)
  )

;;; Changes to exsiting functions
;;
(defun picture-insert-rectangle (rectangle &optional insertp)
  "Overlay RECTANGLE with upper left corner at point.
Optional argument INSERTP, if non-nil causes RECTANGLE to be inserted.
Leaves the region surrounding the rectangle."
  (let ((indent-tabs-mode nil))
    (if (not insertp)
	(save-excursion
	  (delete-rectangle (point)
			    (progn
			      (picture-forward-column
			       (length (car rectangle)))
			      (picture-move-down (1- (length rectangle)))
			      (point)))))
    ;; This line is different from the one in Emacs 21, and enables
    ;; the mark to only be pushed if it is interactivly called.
    (if (interactive-p) (push-mark (point) t))
    ;(picture-hack-insert-rectangle rectangle)
    
    (let ((lines rectangle)
	  (insertcolumn (current-column))
	  (first t))
      (while lines
	(or first
	    (progn
	      (forward-line 1)
	      (or (bolp) (insert ?\n))
	      (move-to-column insertcolumn t)))
	(setq first nil)
	;;(insert-for-yank (car lines))
	;; No need for the above complication, as there are no yank-handlers.
	(insert (car lines))
	(setq lines (cdr lines))))
    
    ))

(if (condition-case nil
	(and (clear-rectangle 0 0 t)
	     nil)
      (error t))

    ;; In emacs 20, FILL is not an argument to clear rectangle as it is
    ;; in emacs 21.  Add it here.  Fortunatly, `operate-on-rectangle' does
    ;; take a fill argument.
    (defun clear-rectangle (start end &optional fill)
      "Blank out rectangle with corners at point and mark.
The text previously in the region is overwritten by the blanks.
When called from a program, requires two args which specify the corners."
      (interactive "r")
      (operate-on-rectangle 'clear-rectangle-line start end t))
)

;; This is a modified version which takes text properties
(defun picture-insert (ch arg &rest textproperties)
  "Insert character CH, and move in the current picture motion direction.
Repeat ARG times.
Apply TEXTPROPERTIES to the character inserted."
  (let* ((width (char-width ch))
	 ;; We must be sure that the succeeding insertion won't delete
	 ;; the just inserted character.
	 (picture-horizontal-step
	  (if (and (= picture-vertical-step 0)
		   (> width 1)
		   (< (abs picture-horizontal-step) 2))
	      (* picture-horizontal-step 2)
	    picture-horizontal-step)))
    (while (> arg 0)
      (setq arg (1- arg))
      ;; The following is in Emacs 21, but it hoses over earlier Emacsen
      ;; which do not have `picture-desired-column'
      ;;
      ;; (if (/= picture-desired-column (current-column))
      ;; (move-to-column picture-desired-column t))
      (let ((col (+ (current-column) width)))
	(or (eolp)
	    (let ((pos (point)))
	      (move-to-column col t)
	      (delete-region pos (point)))))
      (insert ch)
      (forward-char -1)
      (if textproperties
	  (add-text-properties (point) (1+ (point))
			       (append
				;; These two are special defaults
				;; useful for pictures.
				'(rear-nonsticky t detachable t)
				textproperties))
	)
      (picture-move))))

;;; New functions
;;
(defun picture-goto-coordinate (x y)
  "Goto coordinate X, Y."
  (goto-char (point-min))
  (picture-newline y)
  (move-to-column x t)
  )

(defun picture-set-motion (vert horiz)
  "Set VERTICAL and HORIZONTAL increments for movement in Picture mode.
The mode line is updated to reflect the current direction."
  (setq picture-vertical-step vert
	picture-horizontal-step horiz)
  (if (eq major-mode 'picture-mode)
      (progn
	(setq mode-name
	      (format "Picture:%s"
		      (nth (+ 2 (% horiz 3) (* 5 (1+ (% vert 2))))
			   '(wnw nw up ne ene Left left none right Right
				 wsw sw down se ese))))
	(force-mode-line-update)
	(message ""))))

(defun picture-draw-rectilinear-line (x1 y1 x2 y2 &optional direction
					 &rest textproperties)
  "Draw a line from X1, Y1 to X2, Y2.
If optional argument DIRECTION is specified as 'verticle, or 'horizontal,
then the line is drawn with the major direction in that orientation.
If DIRECTION is not specified, the greatest distance between X or Y
coordinates is used to choose.
Arguments TEXTPROPERTIES are applied to the characters inserted.
The line is drawn in a rectilinear fashion."
  ;; A rectilinear line for us (short term) is a line travelling
  ;; in the direction of greatest distance, with a jog in the middle.
  (let (xdir ydir halfway htwiddle
	)
    ;; Travelling
    (if (> x1 x2)
	(setq xdir -1)
      (setq xdir 1))
    (if (> y1 y2)
	(setq ydir -1)
      (setq ydir 1))
    ;; Get there
    (picture-goto-coordinate x1 y1)
    (picture-update-desired-column t)
    ;; Determine primary direction
    (if (or (and direction (eq direction 'horizontal))
	    (and (not direction) (> (abs (- x1 x2)) (abs (- y1 y2)))))
	;; This means that X is primary direction
	(progn
	  (setq halfway (/ (abs (- x1 x2)) 2)
		htwiddle (% (abs (- x1 x2)) 2))
	  (picture-set-motion 0 xdir)
	  (apply 'picture-insert picture-rectangle-h (+ halfway htwiddle)
		 textproperties)
	  (if (/= y1 y2)
	      (progn
		(picture-set-motion ydir 0)
		(apply 'picture-insert
		       (if (< x1 x2)
			   (if (< y1 y2)
			       picture-rectangle-ctr
			     picture-rectangle-cbr)
			 (if (< y1 y2)
			     picture-rectangle-ctl
			   picture-rectangle-cbl))
		       1 textproperties)
		(apply 'picture-insert picture-rectangle-v (1- (abs (- y1 y2)))
		       textproperties)
		(picture-set-motion 0 xdir)
		(apply 'picture-insert
		       (if (< x1 x2)
			   (if (< y1 y2)
			       picture-rectangle-cbl
			     picture-rectangle-ctl)
			 (if (< y1 y2)
			     picture-rectangle-cbr
			   picture-rectangle-ctr))
		       1 textproperties)
		;;(setq halfway (1- halfway))
		)
	    (apply 'picture-insert picture-rectangle-h 1
		   textproperties)
	    )
	  (apply 'picture-insert picture-rectangle-h halfway
		 textproperties)
	  )
      ;; This means that Y is the primary direction
      (setq halfway (/ (abs (- y1 y2)) 2)
	    htwiddle (% (abs (- y1 y2)) 2))
      (picture-set-motion ydir 0)
      (apply 'picture-insert picture-rectangle-v (+ halfway htwiddle)
	     textproperties)
      (if (/= x1 x2)
	  (progn
	    (picture-set-motion 0 xdir)
	    (apply 'picture-insert
		   (if (< y1 y2)
		       (if (< x1 x2)
			   picture-rectangle-cbl
			 picture-rectangle-cbr
			 )
		     (if (< x1 x2)
			 picture-rectangle-ctl
		       picture-rectangle-ctr
		       ))
		   1 textproperties)
	    (apply 'picture-insert picture-rectangle-h (1- (abs (- x1 x2)))
		   textproperties)
	    (picture-set-motion ydir 0)
	    (apply 'picture-insert
		   (if (< y1 y2)
		       (if (< x1 x2)
			   picture-rectangle-ctr
			 picture-rectangle-ctl)
		     (if (< x1 x2)
			 picture-rectangle-cbr
		       picture-rectangle-cbl))
		   1 textproperties)
	    ;(setq halfway (1- halfway))
	    )
	(apply 'picture-insert picture-rectangle-v 1
	       textproperties)
	)
      (apply 'picture-insert picture-rectangle-v halfway
	     textproperties)
      )
    ))

(provide 'picture-hack)

;;; picture-hack.el ends here
