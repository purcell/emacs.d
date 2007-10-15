;;; hl-line+.el --- Extensions to hl-line.el.
;; 
;; Filename: hl-line+.el
;; Description: Extensions to hl-line.el.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2006-2007, Drew Adams, all rights reserved.
;; Created: Sat Aug 26 18:17:18 2006
;; Version: 22.0
;; Last-Updated: Thu Oct 11 10:42:44 2007 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 192
;; URL: http://www.emacswiki.org/cgi-bin/wiki/hl-line+.el
;; Keywords: highlight, cursor, accessibility
;; Compatibility: GNU Emacs 22.x
;; 
;; Features that might be required by this library:
;;
;;   `hl-line'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;;  This library extends standard library `hl-line.el' in these ways:
;;
;;  1. As an alternative to turning on `hl-line' highlighting at all
;;     times, you can turn it on only when Emacs is idle.  To do that,
;;     use command `toggle-hl-line-when-idle' and customize
;;     `global-hl-line-mode' to nil.
;;
;;  2. As another alternative, you can turn it on for only a few
;;     seconds.  To do that, use command `flash-line-highlight' and
;;     customize `global-hl-line-mode' to nil.
;;
;;  3. It provides a face, `hl-line', that you can customize, instead
;;     of using option `hl-line-face'.  
;;
;;     I suggested this to the Emacs developers, and it has been added
;;     to Emacs 22, but with a different default value.  I suggest you
;;     try customizing this to a value similar to what is used here,
;;     especially if you use library `crosshairs.el', so that the
;;     horizontal and vertical highlights will be the same.
;;
;;  To use this library, put this in your Emacs init file (~/.emacs):
;;
;;    (require 'hl-line+) ; Load this file (it will load `hl-line.el')
;;
;;  To turn on `global-hl-line-mode' only when Emacs is idle, by
;;  default, add this line also to your init file:
;;
;;    (toggle-hl-line-when-idle 1) ; Highlight only when idle
;;
;;  You can use command `toggle-hl-line-when-idle' to turn idle
;;  highlighting on and off at any time.  You can use command
;;  `hl-line-when-idle-interval' to change the number of idle seconds
;;  to wait before highlighting.
;;
;;
;;  See also:
;;
;;  * Library `col-highlight.el', which highlights the current column.
;;
;;  * Library `crosshairs.el', which highlights the current line and
;;    the current column, at the same time.  It requires libraries
;;    `col-highlight.el' and `hl-line+.el'.
;;
;;  * Library `cursor-chg.el' or library `oneonone.el', to change the
;;    cursor type when Emacs is idle.
;;
;;
;;  Faces defined here:
;;
;;    `hl-line'.
;;
;;  Commands defined here:
;;
;;    `flash-line-highlight', `hl-line-flash',
;;    `hl-line-toggle-when-idle', `hl-line-when-idle-interval',
;;    `toggle-hl-line-when-idle'.
;;
;;  Non-interactive functions defined here:
;;
;;    `hl-line-highlight-now', `hl-line-unhighlight-now'.
;;
;;  Internal variables defined here:
;;
;;    `hl-line-idle-interval', `hl-line-idle-timer',
;;    `hl-line-when-idle-p'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;;
;; 2007/10/11 dadams
;;     Commentary typo: toggle-cursor-type-when-idle -> toggle-hl-line-when-idle. 
;; 2007/01/10 dadams
;;     Update commentary to indicate that the face is now provided by default.
;; 2006/09/08 dadams
;;     Added: flash-line-highlight, hl-line-flash.
;;     Renamed: hl-line-when-idle(-off) to hl-line-(un)highlight-now.
;; 2006/09/04 dadams
;;     Added: hl-line-when-idle-p, hl-line-idle-interval, hl-line-idle-timer,
;;            hl-line-toggle-when-idle, hl-line-when-idle-interval,
;;            hl-line-when-idle(-off).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'hl-line)

(defvar hl-line-face)                   ; Quiet the byte-compiler.
(defvar global-hl-line-mode)            ; Quiet the byte-compiler.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This will be ignored, since this is now defined by default in Emacs 22.
;; I include this here as a different face that you might want to try.
(defface hl-line '((t (:background "SlateGray3"))) ; Try also (:underline "Yellow")
  "Face to use for `hl-line-face'." :group 'hl-line)
(setq hl-line-face 'hl-line)

(defcustom line-show-period 1
  "Number of seconds to highlight the current line."
  :type 'integer :group 'cursor :group 'hl-line)

(defvar hl-line-idle-interval 5
  "Number of seconds to wait before turning on `global-hl-line-mode'.
Do NOT change this yourself to change the wait period; instead, use
`\\[hl-line-when-idle-interval]'.")

(defvar hl-line-idle-timer
  (progn                                ; Cancel to prevent duplication.
    (when (boundp 'hl-line-idle-timer) (cancel-timer hl-line-idle-timer))
    (run-with-idle-timer hl-line-idle-interval t 'hl-line-highlight-now))
  "Timer used to turn on `global-hl-line-mode' whenever Emacs is idle.")

;; Turn it off, by default.  You must use `toggle-hl-line-when-idle' to turn it on.
(cancel-timer hl-line-idle-timer)

(defvar hl-line-when-idle-p nil
  "Non-nil means to use turn on `global-hl-line-mode' whenever Emacs is idle.
Do NOT change this yourself; instead, use `\\[toggle-hl-line-when-idle]'.")

(defalias 'toggle-hl-line-when-idle 'hl-line-toggle-when-idle)
(defun hl-line-toggle-when-idle (&optional arg)
"Turn on or off using `global-hl-line-mode' when Emacs is idle.
When on, use `global-hl-line-mode' whenever Emacs is idle.
With prefix argument, turn on if ARG > 0; else turn off."
  (interactive "P")
  (setq hl-line-when-idle-p
        (if arg (> (prefix-numeric-value arg) 0) (not hl-line-when-idle-p)))
  (cond (hl-line-when-idle-p
         (timer-activate-when-idle hl-line-idle-timer)
         (add-hook 'pre-command-hook 'hl-line-unhighlight-now)
         (message "Turned ON using `global-hl-line-mode' when Emacs is idle."))
        (t
         (cancel-timer hl-line-idle-timer)
         (remove-hook 'pre-command-hook 'hl-line-unhighlight-now)
         (message "Turned OFF using `global-hl-line-mode' when Emacs is idle."))))

(defun hl-line-when-idle-interval (secs)
  "Set wait until using `global-hl-line-mode' when Emacs is idle.
Whenever Emacs is idle for this many seconds, `global-hl-line-mode'
will be turned on.

To turn on or off using `global-hl-line-mode' when idle,
use `\\[toggle-hl-line-when-idle]."
  (interactive "nSeconds to idle, before using `global-hl-line-mode': ")
  (timer-set-idle-time hl-line-idle-timer
                       (setq hl-line-idle-interval secs)
                       t))

(defun hl-line-highlight-now ()
  "Turn on `global-hl-line-mode' and highlight current line now."
  (unless global-hl-line-mode
    (global-hl-line-mode 1)
    (global-hl-line-highlight)))

(defun hl-line-unhighlight-now ()
  "Turn off `global-hl-line-mode' and unhighlight current line now."
  (global-hl-line-mode -1)
  (global-hl-line-unhighlight))

(defalias 'flash-line-highlight 'hl-line-flash)
(defun hl-line-flash (&optional arg)
  "Highlight the current line for `line-show-period' seconds.
With a prefix argument, highlight for that many seconds."
  (interactive)
  (hl-line-highlight-now)
  (let ((line-period line-show-period))
    (when current-prefix-arg
      (setq line-period (prefix-numeric-value current-prefix-arg)))
    (run-at-time line-period nil #'hl-line-unhighlight-now)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hl-line+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hl-line+.el ends here
