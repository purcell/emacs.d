;;; $Id: maxframe.el 435 2007-08-24 15:40:58Z ryan $
;; maximize the emacs frame based on display size

;; Copyright (C) 2007 Ryan McGeary
;; Author: Ryan McGeary
;; Version: 0.4
;; Keywords: display frame window maximize

;; This code is free; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;;; Commentary:
;;
;; Purpose
;; -------
;; maxframe provides the ability to maximize the emacs frame and stay within
;; the display resolution.
;;
;; Usage
;; -----
;; Example of lines to be added to your .emacs:
;;
;;     (require 'maxframe)
;;     (add-hook 'window-setup-hook 'maximize-frame t)
;;
;; If using two framebuffers (monitors), it might be necesssary to specify a
;; mf-max-width value set to the pixel width of main framebuffer.  This is
;; necessary because emacs does not yet support sniffing different
;; framebuffers.  Example:
;;
;;     (require 'maxframe)
;;     (setq mf-max-width 1600)  ;; Pixel width of main monitor.
;;     (add-hook 'window-setup-hook 'maximize-frame t)
;;
;; To restore the frame to it's original dimensions, call restore-frame:
;;
;;     M-x restore-frame
;;
;; How it works
;; ------------
;; puts the emacs frame in the top left corner of the display and calculates
;; the maximum number of columns and rows that can fit in the display
;;
;; Limitations
;; -----------
;; Requires Emacs 22 (for fringe support), but maximize-frame still works
;; under Emacs 21 on Windows.
;;
;; Emacs does not recognize when the display's resolution is changed. This is
;; a problem because I would like to be able to re-maximize the frame after
;; connecting to a display with different resolution. Unfortunately,
;; display-pixel-width and display-pixel-height yield the display resolution
;; values from when emacs was started instead of the current display
;; values. Perhaps there's a way to have emacs re-sniff these values, but I'm
;; not yet sure how.
;;
;; Credits
;; -------
;; The w32 specific functions were borrowed from the Emacs Manual:
;; http://www.gnu.org/software/emacs/windows/big.html#windows-like-window-ops


(defgroup maxframe nil "Handle maximizing frames.")

(defcustom mf-display-padding-width 0
  "*Any extra display padding that you want to account for while
determining the maximize number of columns to fit on a display"
  :type 'integer
  :group 'maxframe)

;; The default accounts for a Mac OS X display with a menubar
;; height of 22 pixels, a titlebar of 23 pixels, and no dock.
(defcustom mf-display-padding-height (+ 22 23)
  "*Any extra display padding that you want to account for while
determining the maximize number of rows to fit on a display"
  :type 'integer
  :group 'maxframe)

(defcustom mf-offset-x 0
  "*The x coordinate of the upper left corner of the frame.
Negative values are interpreted relative to the rightmost
position.  See `set-frame-position'."
  :type 'integer
  :group 'maxframe)

(defcustom mf-offset-y 0
  "*The y coordinate of the upper left corner of the frame.
Negative values are interpreted relative to the bottommost
position.  See `set-frame-position'."
  :type 'integer
  :group 'maxframe)

(defcustom mf-max-width nil
  "*The maximum display width to support.  This helps better
support the true nature of display-pixel-width.  Since multiple
monitors will result in a very large display pixel width, this
value is used to set the stop point for maximizing the frame.
This could also be used to set a fixed frame size without going
over the display dimensions."
  :type 'integer
  :group 'maxframe)

(defcustom mf-max-height nil
  "*The maximum display height to support.  This helps better
support the true nature of display-pixel-height.  See
`mf-max-width'."
  :type 'integer
  :group 'maxframe)

(defvar mf-restore-width nil)
(defvar mf-restore-height nil)
(defvar mf-restore-top nil)
(defvar mf-restore-left nil)

(defun w32-maximize-frame ()
  "Maximize the current frame (windows only)"
  (interactive)
  (w32-send-sys-command 61488))

(defun w32-restore-frame ()
  "Restore a minimized/maximized frame (windows only)"
  (interactive)
  (w32-send-sys-command 61728))

(defun mf-max-columns (width)
  "Calculates the maximum number of columns that can fit in
pixels specified by WIDTH."
  (let ((scroll-bar (or (frame-parameter nil 'scroll-bar-width) 0))
        (left-fringe (or left-fringe-width (nth 0 (window-fringes)) 0))
        (right-fringe (or right-fringe-width (nth 1 (window-fringes)) 0)))
    (/ (- width scroll-bar left-fringe right-fringe
          mf-display-padding-width)
       (frame-char-width))))

(defun mf-max-rows (height)
  "Calculates the maximum number of rows that can fit in pixels
specified by HEIGHT."
  (/ (- height
        mf-display-padding-height)
     (frame-char-height)))

(defun mf-set-frame-pixel-size (frame width height)
  "Sets size of FRAME to WIDTH by HEIGHT, measured in pixels."
  (set-frame-size frame (mf-max-columns width) (mf-max-rows height)))

(defun mf-max-display-pixel-width ()
  (min (display-pixel-width)
       (or mf-max-width (display-pixel-width))))

(defun mf-max-display-pixel-height ()
  (min (display-pixel-height)
       (or mf-max-height (display-pixel-height))))

(defun x-maximize-frame ()
  "Maximize the current frame (x or mac only)"
  (interactive)
  (unless (or mf-restore-width mf-restore-height mf-restore-top mf-restore-left)
    (setq mf-restore-width (frame-width)
          mf-restore-height (frame-height)
          mf-restore-top (frame-parameter (selected-frame) 'top)
          mf-restore-left (frame-parameter (selected-frame) 'left)))
  (mf-set-frame-pixel-size (selected-frame)
                           (mf-max-display-pixel-width)
                           (mf-max-display-pixel-height))
  (set-frame-position (selected-frame) mf-offset-x mf-offset-y))

(defun x-restore-frame ()
  "Restore the current frame (x or mac only)"
  (interactive)
  (when (and mf-restore-width mf-restore-height mf-restore-top mf-restore-left)
    (set-frame-size (selected-frame) mf-restore-width mf-restore-height)
    (set-frame-position (selected-frame) mf-restore-left mf-restore-top))
  (setq mf-restore-width nil
        mf-restore-height nil
        mf-restore-top nil
        mf-restore-left nil))

(defun maximize-frame ()
  "Maximizes the frame to fit the display if under a windowing
system."
  (interactive)
  (cond ((eq window-system 'w32) (w32-maximize-frame))
        ((memq window-system '(x mac)) (x-maximize-frame))))

(defun restore-frame ()
  "Restores a maximized frame.  See `maximize-frame'."
  (interactive)
  (cond ((eq window-system 'w32) (w32-restore-frame))
        ((memq window-system '(x mac)) (x-restore-frame))))

(defalias 'mf 'maximize-frame)

(provide 'maxframe)
