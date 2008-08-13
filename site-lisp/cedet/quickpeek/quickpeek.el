;;; quickpeek.el --- display info about current cursor context

;;; Copyright (C) 1999, 2000, 2001 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tools
;; X-RCS: $Id: quickpeek.el,v 1.9 2005/09/30 20:42:14 zappo Exp $

(defvar quickpeek-version "0.5"
  "The current version of quickpeek.")

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Quick peek is a tool designed to provide context information from a
;; source code buffer.  It does this through a floating frame.  By
;; default, it will show three lines of text, corresponding to any
;; relevant prototype, documantation, and completions.
;;
;; Quick peek can be independently configured on a per-language basis
;; to identify, and display useful information about the current
;; context.  When there is nothing useful to do, it does something
;; else. See qp-base for information on supporting new languages.
;; See qp-util for utility functions to make your life easier.
;;
;; This file contains user interface programs to the context collection
;; and completion routines based on qp-base classes.

;;; History:
;;
;; 0.5   Convert to using the dframe library for frame management.
;;       Convert to using qp-base eieio based language extensions.
;;
;; 0.4   Rename as quickpeek to avoid confusion with lazy-lock.
;;       Fixed CASE and constant colors.
;;
;; 0.3   Enable functions where return type and name are on different
;;         lines in the c language.
;;       Added `quickpeek-with-alternate-syntax-table', and use it.
;;       Made `thing-at-point' cover functions.
;;       Added `quickpeek-use-tags' flag variable.thing
;;       Added `quickpeek-tags-completion' as a local convenience.
;;       Added `quickpeek-facep' macro as multi-platform convenience.
;;
;; 0.2   Improved the info gathering fn to report errors more effectivly.
;;       Improved lisp info gathering fn to exclude completion lists for
;;         very short symbols.
;;       Made the c info gatherer do something useful.  Added a
;;         non-desructive tag finder.
;;       Added some display types, including an error and simple type.
;;       Added a generic complex string inserttion function utility.
;;         This permits color use in the lazy frame.
;;       Now require's font lock.
;;       Fixed corrupt syntax table in main buffer.
;;
;; 0.1   First Release as lazy-look

(require 'dframe)
(require 'widget)
(require 'qp-util)

;;; Code:
(defvar quickpeek-object nil
  "An object used to get and display information.
This object must be a child of variable `quickpeek-default'.")
(make-variable-buffer-local 'quickpeek-display-object)

(defcustom quickpeek-frame-parameters '((minibuffer . nil)
					(width . 80)
					(height . 5)
					(border-width . 0)
					(menu-bar-lines . 0)
					(tool-bar-lines . 0)
					(unsplittable . t))
  "*Parameters to use when creating the `quickpeek' frame in Emacs.
Parameters not listed here which will be added automatically are
`height' which will be initialized to the height of the frame `quickpeek'
is attached to."
  :group 'quickpeek
  :type '(repeat (sexp :tag "Parameter:")))

;; These values by Hrvoje Niksic <hniksic@srce.hr>
(defcustom quickpeek-frame-plist
  '(minibuffer nil width 80 height 4 border-width 0
	       internal-border-width 0 unsplittable t
	       default-toolbar-visible-p nil has-modeline-p nil
	       menubar-visible-p nil)
  "*Parameters to use when creating the `quickpeek' frame in XEmacs.
Parameters not listed here which will be added automatically are
`height' which will be initialized to the height of the frame `quickpeek'
is attached to."
  :group 'quickpeek
  :type '(repeat (group :inline t
			(symbol :tag "Property")
			(sexp :tag "Value"))))
;;; Hooks
(defcustom quickpeek-before-delete-hook nil
  "Hooks run before deleting the quickpeek frame."
  :group 'quickpeek
  :type 'hook)

(defcustom quickpeek-before-popup-hook nil
  "Hooks run before poping up the quickpeek frame."
  :group 'quickpeek
  :type 'hook)
  
(defcustom quickpeek-after-create-hook nil
  "Hooks run after creating the quickpeek frame."
  :group 'quickpeek
  :type 'hook)

;; Make sure these are different from speedbar.
(defcustom quickpeek-update-speed dframe-update-speed
  "*Idle time in seconds needed before `quickpeek' will update itself.
Updates occur to allow `quickpeek' to display directory information
relevant to the buffer you are currently editing."
  :group 'quickpeek
  :type 'integer)

(defvar quickpeek-update-flag dframe-have-timer-flag
  "*Non-nil means to automatically update the display.
When this is nil then `quickpeek' will not follow the attached frame's path.
When `quickpeek' is active, use:

\\<quickpeek-key-map> `\\[quickpeek-toggle-updates]'

to toggle this value.")

(defvar quickpeek-syntax-table nil
  "Syntax-table used on the `quickpeek'.")

(if quickpeek-syntax-table
    nil
  (setq quickpeek-syntax-table (make-syntax-table))
  ;; turn off paren matching around here.
  (modify-syntax-entry ?\' " " quickpeek-syntax-table)
;  (modify-syntax-entry ?\" " " quickpeek-syntax-table)
  (modify-syntax-entry ?( "()" quickpeek-syntax-table)
  (modify-syntax-entry ?) ")( " quickpeek-syntax-table)
  (modify-syntax-entry ?{ ")}" quickpeek-syntax-table)
  (modify-syntax-entry ?} "({ " quickpeek-syntax-table)
  (modify-syntax-entry ?[ "(]" quickpeek-syntax-table)
  (modify-syntax-entry ?] ")[ " quickpeek-syntax-table))

(defvar quickpeek-key-map nil
  "Keymap used in `quickpeek' buffer.")

(if quickpeek-key-map
    nil
  (setq quickpeek-key-map (make-keymap))
  (suppress-keymap quickpeek-key-map t)

  ;; control
  (define-key quickpeek-key-map "g" 'quickpeek-refresh)
  (define-key quickpeek-key-map "t" 'quickpeek-toggle-updates)

  ;; dframe
  (dframe-update-keymap quickpeek-key-map)  
  )

(defvar quickpeek-frame nil
  "The frame used for lazy look mode.")
(defvar quickpeek-cached-frame nil
  "The frame used for lazy look mode, then hidden.")
(defvar quickpeek-buffer nil
  "The buffer used for lazy look mode.")
(defvar quickpeek-marker (make-marker)
  "Remember where we last looked from.")

;;; Commands to start quickpeek stuff
;;

;;;###autoload
(defalias 'quickpeek 'quickpeek-frame-mode)
;;;###autoload
(defun quickpeek-frame-mode (&optional arg)
  "Initialize `quickpeek'.
If optional ARG is less than 0, turn off this mode, positive turn on.
If nil, then toggle."
  (interactive "P")
  ;; Prepare our quickpeek buffer
  (if (not (buffer-live-p quickpeek-buffer))
      (save-excursion
	(setq quickpeek-buffer (get-buffer-create " QUICKPEEK"))
	(set-buffer quickpeek-buffer)
	(quickpeek-mode)))
  ;; Do the frame thing
  (dframe-frame-mode arg
		     'quickpeek-frame
		     'quickpeek-cached-frame
		     'quickpeek-buffer
		     "Quick Peek"
		     #'quickpeek-frame-mode
		     (if dframe-xemacsp
			 quickpeek-frame-plist
		       quickpeek-frame-parameters)
		     quickpeek-before-delete-hook
		     quickpeek-before-popup-hook
		     quickpeek-after-create-hook)
  ;; Add dframe support
  (setq dframe-track-mouse-function nil ; #'quickpeek-track-mouse
	dframe-help-echo-function nil	; #'quickpeek-item-info
	dframe-mouse-click-function nil ; #'quickpeek-click
	dframe-mouse-position-function nil ; #'quickpeek-position-cursor-on-line)    
	)
  ;; Start up the timer
  (if (not quickpeek-frame)
      (dframe-set-timer nil 'quickpeek-timer-fn 'quickpeek-update-flag)
    (quickpeek-update-contents)
    (quickpeek-set-timer dframe-update-speed)
    ))

;;;###autoload
(defun quickpeek-get-focus ()
  "Change frame focus to or from the `quickpeek' frame.
If the selected frame is not `quickpeek', then `quickpeek' frame is
selected.  If the `quickpeek' frame is active, then select the attached frame."
  (interactive)
  (dframe-get-focus 'quickpeek-frame 'quickpeek-frame-mode
		    (lambda () (if (not quickpeek-update-flag)
				   (let ((quickpeek-update-flag t))
				     (quickpeek-timer-fn)))))
  )

;;;###autoload
(defun quickpeek-help ()
  "Display a quickpeek buffer in a temporary window.
Like `quickpeek', but without an extra frame, timers, or tracking."
  (interactive)
  (let ((quickpeek-buffer (get-buffer-create "*Peek*")))
    (save-excursion
      (set-buffer quickpeek-buffer)
      (quickpeek-mode))
    (quickpeek-update-contents)
    )
  (let ((bw (get-buffer-window "*Peek*")))
    (if bw (delete-window bw)))
  (pop-to-buffer "*Peek*" t)
  (shrink-window-if-larger-than-buffer (selected-window)))

;;; The quickpeek mode for the full deal
;;
(defun quickpeek-mode ()
  "Major mode for displaying lazy information in the `quickpeek' frame.
This frame can be placed anywhere on your desktop, and will attempt to
display information about the current selected buffer."
  ;; NOT interactive
  (kill-all-local-variables)
  (setq major-mode 'quickpeek-mode)
  (setq mode-name "Quickpeek")
  (set-syntax-table quickpeek-syntax-table)
  (use-local-map quickpeek-key-map)
  (setq font-lock-keywords nil);; no font-locking please
  (setq truncate-lines t)
  (make-local-variable 'frame-title-format)
  (setq frame-title-format "Quickpeek")
  (widget-minor-mode)
  (toggle-read-only 1)
  (quickpeek-set-mode-line-format)
  (quickpeek-update-contents)
  quickpeek-buffer)


;;; Framed version of quickpeek utilities
;;
(defsubst quickpeek-current-frame ()
  "Return the frame to use for quickpeek based on current context."
  (dframe-current-frame 'quickpeek-frame 'quickpeek-mode))

(defmacro quickpeek-frame-width ()
  "Return the width of the `quickpeek' frame in characters.
nil if it doesn't exist."
  '(frame-width (quickpeek-current-frame)))

(defun quickpeek-set-mode-line-format ()
  "Set the format of the modeline based on the `quickpeek' environment.
This gives visual indications of what is up.  It EXPECTS the `quickpeek'
frame and window to be the currently active frame and window."
  (if (and (frame-live-p quickpeek-frame)
	   (or (not dframe-xemacsp)
	       (specifier-instance has-modeline-p)))
      (let ((buff (current-buffer))
	    (nsupp (eq quickpeek-info-function
		       'quickpeek-default-info-function)))
	(save-excursion
	  (set-buffer quickpeek-buffer)
	  (setq frame-title-format
		(format "Quickpeek: %s @ %d%s" (buffer-name buff)
			(or (marker-position quickpeek-marker)
			    0)
			(if nsupp " (Unsupported Mode)" "")))
	  (let* ((w (or (quickpeek-frame-width) 20))
		 (p1 "<<")
		 (p5 ">>")
		 (p3 (if quickpeek-update-flag "QUICK PEEK" "SLOW POKE"))
		 (blank (- w (length p1) (length p3) (length p5)
			   (if line-number-mode 4 0)))
		 (p2 (if (> blank 0)
			 (make-string (/ blank 2) ? )
		       ""))
		 (p4 (if (> blank 0)
			 (make-string (+ (/ blank 2) (% blank 2)) ? )
		       ""))
		 (tf
		  (if line-number-mode
		      (list (concat p1 p2 p3) '(line-number-mode " %3l")
			    (concat p4 p5))
		    (list (concat p1 p2 p3 p4 p5)))))
	    (if (not (equal mode-line-format tf))
		(progn
		  (setq mode-line-format tf)
		  (force-mode-line-update))))))))

(defun quickpeek-toggle-updates ()
  "Toggle automatic update for the `quickpeek' frame."
  (interactive)
  (if quickpeek-update-flag
      (quickpeek-disable-update)
    (quickpeek-enable-update)))

(defun quickpeek-enable-update ()
  "Enable automatic updating in `quickpeek' via timers."
  (interactive)
  (setq quickpeek-update-flag t)
  (quickpeek-set-mode-line-format)
  (quickpeek-set-timer quickpeek-update-speed))

(defun quickpeek-disable-update ()
  "Disable automatic updating and stop consuming resources."
  (interactive)
  (setq quickpeek-update-flag nil)
  (quickpeek-set-mode-line-format)
  (quickpeek-set-timer nil))

(defun quickpeek-set-timer (timeout)
  "Apply a timer with TIMEOUT, or remove a timer if TIMOUT is nil.
TIMEOUT is the number of seconds until the `quickpeek' timer is called
again.  When TIMEOUT is nil, turn off all timeouts.
This function will also change the mode line to match `quickpeek-update-flag'."
  (dframe-set-timer timeout 'quickpeek-timer-fn 'quickpeek-update-flag)
  ;; change this if it changed for some reason
  (quickpeek-set-mode-line-format))

(defun quickpeek-select-window (buffer)
  "Select a window in which BUFFER is shown.
If it is not shown, force it to appear in the default window."
  (let ((win (get-buffer-window buffer (marker-buffer quickpeek-marker))))
    (if win
	(select-window win)
      (set-window-buffer (selected-window) buffer))))

(defun quickpeek-timer-fn ()
  "Run whenever Emacs is idle to update the `quickpeek' item."
  (if (not (frame-live-p quickpeek-frame))
      (quickpeek-set-timer nil)
    ;; Save all the match data so that we don't mess up executing fns
    (save-match-data
      ;; Only do stuff if the frame is visible, not an icon, and if
      ;; it is currently flagged to do something.
      (if (and quickpeek-update-flag
	       (frame-visible-p quickpeek-frame)
	       (not (equal (point-marker) quickpeek-marker))
	       (not (eq (frame-visible-p quickpeek-frame) 'icon))
	       (not (eq (selected-frame) quickpeek-frame))
	       (not (eq (selected-window) (minibuffer-window))))
	  (let ((af (selected-frame)))
	    (save-window-excursion
	      ;; This is magic for the previously reffed frame
	      ;; excludes other dedicated frame applications.
	      (while dframe-attached-frame
		(select-frame (previous-frame (selected-frame))))
	      (quickpeek-update-contents)
	      (select-frame af)))))))

(defun quickpeek-refresh ()
  "Refresh the current `quickpeek' display, disposing of any cached data."
  (interactive)
  (let ((dm (and (boundp 'deactivate-mark) deactivate-mark)))
    (message "Refreshing quickpeek...")
    (quickpeek-update-contents)
    ;; Reset the timer in case it got really hosed for some reason...
    (quickpeek-set-timer quickpeek-update-speed)
    (message "Refreshing quickpeek...done")
    (if (boundp 'deactivate-mark) (setq deactivate-mark dm))))

;;; Quick Peek Minor Mode
;;
(defvar quickpeek-minor-mode nil
  "Non-nil in quickpeek enabled buffers.
Enables the quickpeek keymap.")
(make-variable-buffer-local 'quickpeek-minor-mode)

;; We don't want to waste space.  There is a menu after all.
(add-to-list 'minor-mode-alist '(quickpeek-minor-mode ""))

(defvar quickpeek-minor-keymap
  (let ((map (make-sparse-keymap))
	(pmap (make-sparse-keymap)))
    (define-key pmap "h" 'quickpeek-edit-file-target)
    ;; bind our submap into map
    (define-key map "\C-c/" pmap)
    map)
  "Keymap used in quickpeek minor mode.")

(if quickpeek-minor-keymap
    (progn
      (easy-menu-define
       quickpeek-minor-menu quickpeek-minor-keymap "QuickPeek Minor Mode Menu"
       '("QP"
	 [ "Show Context Information" quickpeek-help ]
	 ))
      ))

;; Allow re-insertion of a new keymap
(let ((a (assoc 'quickpeek-minor-mode minor-mode-map-alist)))
  (if a
      (setcdr a quickpeek-minor-keymap)
    (add-to-list 'minor-mode-map-alist
		 (cons 'quickpeek-minor-mode
		       quickpeek-minor-keymap))
    ))

(defun quickpeek-minor-mode (arg)
  "Enable or disable Quickpeek minor mode.
When this mode is enabled, a menu and keymap are available to access
quickpeek functionality without a quickpeek frame being visible."
  (interactive "P")
  (setq quickpeek-minor-mode
 	(not (or (and (null arg) quickpeek-minor-mode)
 		 (<= (prefix-numeric-value arg) 0))))
  (and dframe-xemacs-p
       (if quickpeek-minor-mode
 	   (easy-menu-add quickpeek-minor-menu)
 	 (easy-menu-remove quickpeek-minor-menu)))
  )

;;; The real deal for any quickpeek mode.
;;
(defun quickpeek-collect-data ()
  "Buffer `major-mode' specialized method for collecting data."
  (prog1
      (save-excursion (quickpeek-collect quickpeek-object))
    (move-marker quickpeek-marker (point) (current-buffer))))

(defun quickpeek-update-contents ()
  "Updated the contents of the `quickpeek' buffer."
  (interactive)
  (save-excursion
    (condition-case fu
	(quickpeek-with-alternate-syntax-table
	  (let ((info (save-match-data (quickpeek-collect-data))))
	    (save-excursion
	      (set-buffer quickpeek-buffer)
	      (quickpeek-with-writable
		(erase-buffer)
		(apply (car info) (cdr info))))))
      (error
       (save-excursion
	 (set-buffer quickpeek-buffer)
	 (quickpeek-with-writable
	   (erase-buffer)
	   (if fu
	       (quickpeek-error-form fu)
	     (quickpeek-goofy-form)))))))
  (quickpeek-set-mode-line-format))

;;; Temporary hack to get everything rolling.
(require 'qp-base)
(require 'qp-elisp)
(require 'qp-c)

(provide 'quickpeek)

;;; quickpeek.el ends here

