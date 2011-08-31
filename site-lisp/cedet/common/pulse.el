;;; pulse.el --- Pulsing Overlays

;; Copyright (C) 2007, 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: pulse.el,v 1.11 2009/02/10 15:54:30 zappo Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Manage temporary pulsing of faces and overlays.
;;
;; This is a temporal decoration technique where something is to be
;; highlighted briefly.  This adds a gentle pulsing style to the text
;; decorated this way.
;;
;; Useful user functions:
;;
;; `pulse-enable-integration-advice' - Turn on advice to make various
;;      Emacs commands pulse, such as `goto-line', or `find-tag'.
;;
;; The following are useful entry points:
;;
;; `pulse' - Cause `pulse-highlight-face' to shift toward background color.
;;      Assumes you are using a version of Emacs that supports pulsing.
;;
;;
;; `pulse-momentary-highlight-one-line' - Pulse a single line at POINT.
;; `pulse-momentary-highlight-region' - Pulse a region.
;; `pulse-momentary-highlight-overlay' - Pulse an overlay
;;      These three functions will just blink the specified area if
;;      the version of Emacs you are using doesn't support pulsing.
;;    
;; `pulse-line-hook-function' - A simple function that can be used in a
;;      hook that will pulse whatever line the cursor is on.
;;
;;; History:
;;
;; The original pulse code was written for semantic tag highlighting.
;; It has been extracted, and adapted for general purpose pulsing.
;;
;; Pulse is a part of CEDET.  http://cedet.sf.net


(defun  pulse-available-p ()
  "Return non-nil if pulsing is available on the current frame."
  (condition-case nil
      (let ((v (color-values (face-background 'default))))
	(numberp (car-safe v)))
    (error nil)))

(defcustom pulse-flag (pulse-available-p)
  "*Non-nil means to pulse the overlay face for momentary highlighting.
Pulsing involves a bright highlight that slowly shifts to the background
color.  Non-nil just means to highlight with an unchanging color for a short
time.

If `pulse-flag' is non-nil, but `pulse-available-p' is nil, then
this flag is ignored."
  :group 'pulse
  :type 'boolean)

(defface pulse-highlight-start-face
  '((((class color) (background dark))
     (:background "#AAAA33"))
    (((class color) (background light))
     (:background "#FFFFAA")))
  "*Face used at beginning of a highight."
  :group 'pulse)

(defface pulse-highlight-face
  '((((class color) (background dark))
     (:background "#AAAA33"))
    (((class color) (background light))
     (:background "#FFFFAA")))
  "*Face used during a pulse for display.  *DO NOT CUSTOMIZE*
Face used for temporary highlighting of tags for effect."
  :group 'pulse)

;;; Compatibility
;;
(if (featurep 'xemacs)
    (progn
      (defalias 'pulse-overlay-live-p
        (lambda (o)
          (and (extent-live-p o)
               (not (extent-detached-p o))
               (bufferp (extent-buffer o)))))
      (defalias 'pulse-overlay-put 'set-extent-property)
      (defalias 'pulse-overlay-get 'extent-property)
      (defalias 'pulse-overlay-delete 'delete-extent)
      (defalias 'pulse-make-overlay 'make-extent)
      )
  ;; Regular Emacs
  (defalias 'pulse-overlay-live-p 'overlay-buffer)
  (defalias 'pulse-overlay-put 'overlay-put)
  (defalias 'pulse-overlay-get 'overlay-get)
  (defalias 'pulse-overlay-delete 'delete-overlay)
  (defalias 'pulse-make-overlay 'make-overlay)
  )

;;; Code:
;;
(defun pulse-int-to-hex (int &optional nb-digits)
  "Convert integer argument INT to a #XXXXXXXXXXXX format hex string.
Each X in the output string is a hexadecimal digit.
NB-DIGITS is the number of hex digits.  If INT is too large to be
represented with NB-DIGITS, then the result is truncated from the
left.  So, for example, INT=256 and NB-DIGITS=2 returns \"00\", since
the hex equivalent of 256 decimal is 100, which is more than 2 digits.

This function was blindly copied from hexrgb.el by Drew Adams.
http://www.emacswiki.org/cgi-bin/wiki/hexrgb.el"
  (setq nb-digits (or nb-digits 4))
  (substring (format (concat "%0" (int-to-string nb-digits) "X") int) (- nb-digits)))

(defun pulse-color-values-to-hex (values)
  "Convert list of rgb color VALUES to a hex string, #XXXXXXXXXXXX.
Each X in the string is a hexadecimal digit.
Input VALUES is as for the output of `x-color-values'.

This function was blindly copied from hexrgb.el by Drew Adams.
http://www.emacswiki.org/cgi-bin/wiki/hexrgb.el"
  (concat "#"
          (pulse-int-to-hex (nth 0 values) 4) ; red
          (pulse-int-to-hex (nth 1 values) 4) ; green
          (pulse-int-to-hex (nth 2 values) 4))) ; blue

(defcustom pulse-iterations 10
  "Number of iterations in a pulse operation."
  :group 'pulse
  :type 'number)
(defcustom pulse-delay .03
  "Delay between face lightening iterations, as used by `sit-for'."
  :group 'pulse
  :type 'number)

(defun pulse-lighten-highlight ()
  "Lighten the face by 1/`pulse-iterations' toward the background color.
Return t if there is more drift to do, nil if completed."
  (if (>= (get 'pulse-highlight-face :iteration) pulse-iterations)
      nil
    (let* ((frame (color-values (face-background 'default)))
	   (start (color-values (face-background
				 (get 'pulse-highlight-face
				      :startface))))
	   (frac  (list (/ (- (nth 0 frame) (nth 0 start)) pulse-iterations)
			(/ (- (nth 1 frame) (nth 1 start)) pulse-iterations)
			(/ (- (nth 2 frame) (nth 2 start)) pulse-iterations)))
	   (it (get 'pulse-highlight-face :iteration))
	   )
      (set-face-background 'pulse-highlight-face
			   (pulse-color-values-to-hex
			    (list
			     (+ (nth 0 start) (* (nth 0 frac) it))
			     (+ (nth 1 start) (* (nth 1 frac) it))
			     (+ (nth 2 start) (* (nth 2 frac) it)))))
      (put 'pulse-highlight-face :iteration (1+ it))
      (if (>= (1+ it) pulse-iterations)
	  nil
	t))))

(defun pulse-reset-face (&optional face)
  "Reset the pulse highlighting FACE."
  (set-face-background 'pulse-highlight-face
		       (if face
			   (face-background face)
			 (face-background 'pulse-highlight-start-face)
			 ))
  (put 'pulse-highlight-face :startface (or face
					    'pulse-highlight-start-face))
  (put 'pulse-highlight-face :iteration 0))

;;;###autoload
(defun pulse (&optional face)
  "Pulse the colors on our highlight face.
If optional FACE is provide, reset the face to FACE color,
instead of `pulse-highlight-start-face'.
Be sure to call `pulse-reset-face' after calling pulse."
  (unwind-protect
      (progn
	(pulse-reset-face face)
	(while (and (pulse-lighten-highlight)
		    (sit-for pulse-delay))
	  nil))
    ))

;;;###autoload
(defun pulse-test (&optional no-error)
  "Test the lightening function for pulsing a line.
When optional NO-ERROR Don't throw an error if we can't run tests."
  (interactive)
  (if (or (not pulse-flag) (not (pulse-available-p)))
      (if no-error
	  nil
	(error (concat "Pulse test only works on versions of Emacs"
		       " that support pulsing")))
    ;; Run the tests
    (when (interactive-p)
      (message "<Press a key> Pulse one line.")
      (read-char))
    (pulse-momentary-highlight-one-line (point))
    (when (interactive-p)
      (message "<Press a key> Pulse a region.")
      (read-char))
    (pulse-momentary-highlight-region (point)
				      (save-excursion
					(condition-case nil
					    (forward-char 30)
					  (error nil))
					(point)))
    (when (interactive-p)
      (message "<Press a key> Pulse line a specific color.")
      (read-char))
    (pulse-momentary-highlight-one-line (point) 'modeline)
    (when (interactive-p)
      (message "<Press a key> Pulse a pre-existing overlay.")
      (read-char))
    (let* ((start (point-at-bol))
	   (end (save-excursion
		  (end-of-line)
		  (when (not (eobp))
		    (forward-char 1))
		  (point)))
	   (o (pulse-make-overlay start end))
	   )
      (pulse-momentary-highlight-overlay o)
      (if (pulse-overlay-live-p o)
	  (pulse-overlay-delete o)
	(error "Non-temporary overlay was deleted!"))
      )
    (when (interactive-p)
      (message "Done!"))))


;;; Convenience Functions
;;
(defvar pulse-momentary-overlay nil
  "The current pulsing overlay.")

;;;###autoload
(defun pulse-momentary-highlight-overlay (o &optional face)
  "Pulse the overlay O, unhighlighting before next command.
Optional argument FACE specifies the fact to do the highlighting."
  (pulse-overlay-put o 'original-face (pulse-overlay-get o 'face))
  (setq pulse-momentary-overlay o)
  (if (or (not pulse-flag) (not (pulse-available-p)))
      ;; Provide a face... clear on next command
      (progn
	(pulse-overlay-put o 'face (or face 'pulse-highlight-start-face))
	(add-hook 'pre-command-hook
		  'pulse-momentary-unhighlight)
	)
    ;; pulse it.
    (unwind-protect
	(progn
	  (pulse-overlay-put o 'face 'pulse-highlight-face)
	  ;; The pulse function puts FACE onto 'pulse-highlight-face.
	  ;; Thus above we put our face on the overlay, but pulse
	  ;; with a reference face needed for the color.
	  (pulse face))
      (pulse-momentary-unhighlight))
    )
  )

(defun pulse-momentary-unhighlight ()
  "Unhighlight a line recently highlighted."
  ;; If someone passes in an overlay, then pulse-momentary-overlay
  ;; will still be nil, and won't need modifying.
  (when pulse-momentary-overlay
    ;; clear the starting face
    (pulse-overlay-put pulse-momentary-overlay 'face
		       (pulse-overlay-get pulse-momentary-overlay
					  'original-face))
    (pulse-overlay-put pulse-momentary-overlay 'original-face nil)
    ;; Clear the overlay if it needs deleting.
    (if (pulse-overlay-get pulse-momentary-overlay 'pulse-delete)
	(pulse-overlay-delete pulse-momentary-overlay))
    ;; Clear the variable.
    (setq pulse-momentary-overlay nil))

  ;; Reset the pulsing face.
  (pulse-reset-face)

  ;; Remove this hook.
  (remove-hook 'pre-command-hook
	       'pulse-momentary-unhighlight)
  )

;;;###autoload
(defun pulse-momentary-highlight-one-line (point &optional face)
  "Highlight the line around POINT, unhighlighting before next command.
Optional argument FACE specifies the face to do the highlighting."
  (let ((start (point-at-bol))
	(end (save-excursion
	       (end-of-line)
	       (when (not (eobp))
		 (forward-char 1))
	       (point))))
    (pulse-momentary-highlight-region start end face)
    ))

;;;###autoload
(defun pulse-momentary-highlight-region (start end &optional face)
  "Highlight between START and END, unhighlighting before next command.
Optional argument FACE specifies the fact to do the highlighting."
  (let ((o (pulse-make-overlay start end)))
    ;; Mark it for deletion
    (pulse-overlay-put o 'pulse-delete t)
    (pulse-momentary-highlight-overlay o face)))

;;; Random integration with other tools
;;
(defvar pulse-command-advice-flag nil
  "Non-nil means pulse advice is active.
To active pulse advice, use `pulse-enable-integration-advice'.")

;;;###autoload
(defun pulse-toggle-integration-advice (arg)
  "Toggle activation of advised functions that will now pulse.
Wint no ARG, toggle the pulse advice.
With a negative ARG, disable pulse advice.
With a positive ARG, enable pulse advice.
Currently advised functions include:
  `goto-line'
  `exchange-point-and-mark'
  `find-tag'
  `tags-search'
  `tags-loop-continue'
  `pop-tag-mark'
  `imenu-default-goto-function'
Pulsing via `pulse-line-hook-function' has also been added to
the following hook:
  `next-error-hook'"
  (interactive "P")
  (if (null arg)
      (setq pulse-command-advice-flag (not pulse-command-advice-flag))
    (if (< (prefix-numeric-value arg) 0)
	(setq pulse-command-advice-flag nil)
      (setq pulse-command-advice-flag t)
      )
    )
  (if pulse-command-advice-flag
      (message "Pulse advice enabled")
    (message "Pulse advice disabled"))
  )

(defadvice goto-line (after pulse-advice activate)
  "Cause the line that is `goto'd to pulse when the cursor gets there."
  (when (and pulse-command-advice-flag (interactive-p))
    (pulse-momentary-highlight-one-line (point))))

(defadvice exchange-point-and-mark (after pulse-advice activate)
  "Cause the line that is `goto'd to pulse when the cursor gets there."
  (when (and pulse-command-advice-flag (interactive-p)
	     (> (abs (- (point) (mark))) 400))
    (pulse-momentary-highlight-one-line (point))))

(defadvice find-tag (after pulse-advice activate)
  "After going to a tag, pulse the line the cursor lands on."
  (when (and pulse-command-advice-flag (interactive-p))
    (pulse-momentary-highlight-one-line (point))))

(defadvice tags-search (after pulse-advice activate)
  "After going to a hit, pulse the line the cursor lands on."
  (when (and pulse-command-advice-flag (interactive-p))
    (pulse-momentary-highlight-one-line (point))))

(defadvice tags-loop-continue (after pulse-advice activate)
  "After going to a hit, pulse the line the cursor lands on."
  (when (and pulse-command-advice-flag (interactive-p))
    (pulse-momentary-highlight-one-line (point))))

(defadvice pop-tag-mark (after pulse-advice activate)
  "After going to a hit, pulse the line the cursor lands on."
  (when (and pulse-command-advice-flag (interactive-p))
    (pulse-momentary-highlight-one-line (point))))

(defadvice imenu-default-goto-function (after pulse-advice activate)
  "After going to a tag, pulse the line the cursor lands on."
  (when pulse-command-advice-flag
    (pulse-momentary-highlight-one-line (point))))

;;;###autoload
(defun pulse-line-hook-function ()
  "Function used in hooks to pulse the current line.
Only pulses the line if `pulse-command-advice-flag' is non-nil."
  (when pulse-command-advice-flag
    (pulse-momentary-highlight-one-line (point))))

(add-hook 'next-error-hook 'pulse-line-hook-function)

(provide 'pulse)

;;; pulse.el ends here
