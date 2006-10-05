;;; bigclock --- A great big clock

;;; Copyright (C) 2000 Free Software Foundation

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: amusement
;; X-RCS: $Id: bigclock.el,v 1.3 2000/09/05 01:05:01 zappo Exp $

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Display a big clock in a special frame.

(require 'dframe)

;;; Code:
(defgroup bigclock nil
  "Faces used in dframe."
  :prefix "bigclock-"
  :group 'bigclock)

(defcustom bigclock-frame-parameters
  '((minibuffer . nil)
    (width . 9)
    (height . 2)
    (border-width . 0)
    (menu-bar-lines . 0)
    (unsplittable . t)
    (font . "-*-courier-medium-r-normal-*-*-320-*-*-m-*-iso8859-1"))
  "Frame parameters for the big clock."
  :group 'bigclock
  :type '(repeat (sexp :tag "Parameter:")))

(defcustom bigclock-update-flag dframe-have-timer-flag
  "Non-nil means the clock will be able to update."
  :group 'bigclock
  :type 'boolean)

(defvar bigclock-key-map
  (let ((km (make-sparse-keymap)))
    (dframe-update-keymap km)
    km)
  "Keymap used in the big clock.")
  
(defvar bigclock-buffer nil
  "Bigclocks buffer.")

(defvar bigclock-frame nil
  "Bigclock's frame.")

(defvar bigclock-cached-frame nil
  "Bigclock's cached frame.")

(defcustom bigclock-before-delete-hook nil
  "Hooks called before bigclock is deleted."
  :group 'bigclock
  :type 'hook)

(defcustom bigclock-before-popup-hook nil
  "Hooks called before poping up the bigclock frame."
  :group 'bigclock
  :type 'hook)

(defcustom bigclock-after-create-hook nil
  "Hooks called after creating the bigclock frame."
  :group 'bigclock
  :type 'hook)

(defcustom bigclock-mode-hook nil
  "Hook run when a bigclock buffer is created."
  :group 'bigclock
  :type 'hook)

(defalias 'bigclock 'bigclock-frame-mode)
(defun bigclock-frame-mode (&optional arg)
  "Enable or disable bigclock.
Optional argument ARG enables or disables the bigclock frame."
  (interactive "P")
  ;; Get the buffer to play with
  (if (not (buffer-live-p bigclock-buffer))
      (save-excursion
	(setq bigclock-buffer (get-buffer-create " BIGCLOCK"))
	(set-buffer bigclock-buffer)
	(bigclock-mode)))
  ;; Do the frame thing
  (dframe-frame-mode arg
		     'bigclock-frame
		     'bigclock-cached-frame
		     'bigclock-buffer
		     "Bigclock"
		     #'bigclock-frame-mode
		     (if dframe-xemacsp
			 bigclock-frame-plist
		       bigclock-frame-parameters)
		     bigclock-before-delete-hook
		     bigclock-before-popup-hook
		     bigclock-after-create-hook)
  ;; Start up the timer
  (if (not bigclock-frame)
      (dframe-set-timer nil #'bigclock-timer-fn 'bigclock-update-flag)
    (dframe-set-timer 60 #'bigclock-timer-fn 'bigclock-update-flag)
    ))

(defun bigclock-get-focus ()
  "Change frame focus to or from the bigclock frame.
If the selected frame is not bigclock, then bigclock frame is
selected.  If the bigclock frame is active, then select the attached frame."
  (interactive)
  (dframe-get-focus 'bigclock-frame 'bigclock-frame-mode))

(defun bigclock-mode ()
  "Set the current buffer to be in BIGCLOCK mode.
\\{bigclock-key-map}"
  ;; NOT interactive
  (save-excursion
    (kill-all-local-variables)
    (setq major-mode 'bigclock-mode)
    (setq mode-name "BigClock")
    (setq font-lock-keywords nil) ;; no font-locking please
    (setq truncate-lines t)
    (use-local-map bigclock-key-map)
    (make-local-variable 'frame-title-format)
    (setq frame-title-format "Bigclock "))
    (toggle-read-only 1)
    (setq mode-line-format "")
    ;; Add in our dframe hooks.
    (setq dframe-track-mouse-function nil
	  dframe-help-echo-function nil
	  dframe-mouse-click-function nil
	  dframe-mouse-position-function nil)
    ;;no auto-show for Emacs
    (run-hooks 'bigclock-mode-hook))

(defun bigclock-timer-fn ()
  "Run whenever Emacs is idle to update bigclock."
  (if (or (not bigclock-frame)
	  (not (frame-live-p bigclock-frame)))
      (dframe-set-timer nil 'bigclock-timer-fn 'bigclock-update-flag)
    (save-excursion
      (set-buffer bigclock-buffer)
      (toggle-read-only -1)
      (erase-buffer)
      (insert (format-time-string "%2I:%M %p" (current-time))))))


(provide 'bigclock)

;;; bigclock.el ends here
