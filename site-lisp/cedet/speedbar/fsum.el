;;; fsum --- Frame Summary Mode

;;; Copyright (C) 2005 Free Software Foundation

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: navigation
;; X-RCS: $Id: fsum.el,v 1.2 2005/09/30 20:25:43 zappo Exp $

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Summarize the contents of a buffer by the shape of the text only.
;; Allows for fast navigation.
;;
;; To use, middle click at the location where you want to jump
;; to.

(require 'dframe)

;;; Code:
(defgroup fsum nil
  "Faces used in Frame Summary mode."
  :prefix "fsum-"
  :group 'fsum)

(defvar fsum-font-name "-*-nil-*-*-*-*-*-*-*-*-*-*-*-*"
  "Magic font that makes every text char be 1 pixel.")

(defvar fsum-frame-parameters (append
			       '((minibuffer . nil)
				 (width . 80)
				 (border-width . 0)
				 (menu-bar-lines . 0)
				 (tool-bar-lines . 0)
				 (unsplittable . t)
				 (left-fringe . 0))
			       (list
				(cons 'font fsum-font-name))
			       )
  "Parameters used to create the summary frame.")

(defvar fsum-frame-plist
  (append '(font) (list fsum-font-name)
	  '(minibuffer nil border-width 0
		       internal-border-width 0 unsplittable t
		       default-toolbar-visible-p nil has-modeline-p nil
		       menubar-visible-p nil
		       default-gutter-visible-p nil))
  "Parameters used to create the summary frame.")

(defcustom fsum-update-flag dframe-have-timer-flag
  "Non-nil means the clock will be able to update."
  :group 'fsum
  :type 'boolean)

(defvar fsum-key-map
  (let ((km (make-sparse-keymap)))
    (dframe-update-keymap km)
    km)
  "Keymap used in the big clock.")
  
(defvar fsum-buffer nil
  "Fsums buffer.")

(defvar fsum-frame nil
  "Fsum's frame.")

(defvar fsum-cached-frame nil
  "Fsum's cached frame.")

(defcustom fsum-before-delete-hook nil
  "Hooks called before fsum is deleted."
  :group 'fsum
  :type 'hook)

(defcustom fsum-before-popup-hook nil
  "Hooks called before poping up the fsum frame."
  :group 'fsum
  :type 'hook)

(defcustom fsum-after-create-hook nil
  "Hooks called after creating the fsum frame."
  :group 'fsum
  :type 'hook)

(defcustom fsum-mode-hook nil
  "Hook run when a fsum buffer is created."
  :group 'fsum
  :type 'hook)

(defcustom fsum-buffer-switcheroo nil
  "Non-nil when switching buffers.")

(defalias 'frame-summary 'fsum-frame-mode)
(defun fsum-frame-mode (&optional arg)
  "Enable or disable frame summary mode.
Optional argument ARG enables or disables the fsum frame."
  (interactive "P")
  (if fsum-buffer-switcheroo
      nil
    ;; Get the buffer to play with
    (if (not (buffer-live-p fsum-buffer))
	(fsum-clone-buffer (current-buffer)))
    ;; Do the frame thing
    (dframe-frame-mode arg
		       'fsum-frame
		       'fsum-cached-frame
		       'fsum-buffer
		       "Fsum"
		       #'fsum-frame-mode
		       (if dframe-xemacsp
			   fsum-frame-plist
			 fsum-frame-parameters)
		       fsum-before-delete-hook
		       fsum-before-popup-hook
		       fsum-after-create-hook)
    ;; Start up the timer
    (if (not fsum-frame)
	(dframe-set-timer nil #'fsum-timer-fn 'fsum-update-flag)
      (dframe-set-timer 4 #'fsum-timer-fn 'fsum-update-flag)
      (fsum-reset-faces)
      )))

(defun fsum-reset-faces ()
  "Reset all faces in the fsum frame."
  (let ((faces (frame-face-alist)))
    (while faces
      (set-face-font (car (car faces)) fsum-font-name fsum-frame)
      (setq faces (cdr faces))
      )))

(defun fsum-get-focus ()
  "Change frame focus to or from the fsum frame.
If the selected frame is not fsum, then fsum frame is
selected.  If the fsum frame is active, then select the attached frame."
  (interactive)
  (dframe-get-focus 'fsum-frame 'fsum-frame-mode))

(defvar fsum-buffer-origins nil
  "Original buffer that fsum is summarizing.")
(make-variable-buffer-local 'fsum-buffer-origins)

(defun fsum-clone-buffer (buff)
  "Create an file summary buffer from BUFF."
  (if (and fsum-buffer
	   (buffer-live-p fsum-buffer)
	   (eq (save-excursion
		 (set-buffer fsum-buffer)
		 fsum-buffer-origins)
	       buff))
      ;; All is good, so do nothing.
      nil
    ;; else, we need to kill and reclone.
    (let ((fsum-buffer-switcheroo t)
	  (bw (get-buffer-window fsum-buffer fsum-frame))
	  )
      (if (and fsum-buffer (buffer-live-p fsum-buffer))
	  (save-excursion
	    ;; Disable dedicated windows
	    (set-window-dedicated-p bw nil)
	    ;; Remove the old buffer
	    (kill-buffer fsum-buffer)))
      (let ((newbuff (make-indirect-buffer buff "FSUM" nil)))
	(save-excursion
	  (set-buffer newbuff)
	  (setq fsum-buffer newbuff)
	  (fsum-mode buff)
	  (setq dframe-controlled #'fsum-frame-mode)
	  newbuff)
	))))

(defun fsum-mode (orig)
  "Set the current buffer to be in FSUM mode.
ORIG is the buffer fsum originated from.
\\{fsum-key-map}"
  ;; NOT interactive
  (save-excursion
    (kill-all-local-variables)
    (setq major-mode 'fsum-mode)
    (setq mode-name "Fsum")
    (setq font-lock-keywords nil) ;; no font-locking please
    (setq truncate-lines t)
    (use-local-map fsum-key-map)
    (make-local-variable 'frame-title-format)
    (setq frame-title-format "Fsum "))
    (toggle-read-only 1)
    (setq mode-line-format "")
    ;; set up the originator
    (setq fsum-buffer-origins orig)
    ;; Add in our dframe hooks.
    (setq dframe-track-mouse-function nil
	  dframe-help-echo-function nil
	  dframe-mouse-click-function #'fsum-jump
	  dframe-mouse-position-function nil)
    ;;no auto-show for Emacs
    (run-hooks 'fsum-mode-hook))

(defun fsum-timer-fn ()
  "Run whenever Emacs is idle to update fsum."
  (if (or (not fsum-frame)
	  (not (frame-live-p fsum-frame)))
      (dframe-set-timer nil 'fsum-timer-fn 'fsum-update-flag)

    ;; Create the update
    (let ((cf (dframe-current-frame 'fsum-frame 'fsum-mode))
	  (nb nil))
      (if (and fsum-update-flag
	       cf
	       (frame-visible-p cf)
	       (not (eq (frame-visible-p cf) 'icon)))
	  (let ((af (selected-frame)))
	    (save-window-excursion
	      (dframe-select-attached-frame fsum-frame)
	      (setq nb (fsum-clone-buffer (current-buffer))))
	    (when nb
	      (select-frame fsum-frame)
	      ;; Switch to the new buffer
	      (switch-to-buffer nb)
	      ;; Force the window to be dedicated again.
	      (set-window-dedicated-p (selected-window) t)
	      )))
      (select-frame cf)
      )))


(defun fsum-jump (e)
  "Jump to the location in the originating buffer.
Event E is the event that caused this fcn to be called."
  (let ((cp (point))
	(o fsum-buffer-origins)
	(match nil)
	)
    (dframe-select-attached-frame fsum-frame)
    (when (eq o (current-buffer))
      (setq match t)
      (goto-char cp))
    (select-frame fsum-frame)
    (when match
      (dframe-maybee-jump-to-attached-frame))
    ))

(provide 'fsum)

;;; fsum.el ends here
