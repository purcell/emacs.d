;;; highline.el --- minor mode to highlight current line in buffer

;; Copyright (C) 2000, 2001, 2002, 2006 Vinicius Jose Latorre

;; Author: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Maintainer: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Keywords: faces, frames, editing
;; Time-stamp: <2006/09/13 23:38:49 vinicius>
;; Version: 4.2
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/ViniciusJoseLatorre

;; This file is *NOT* (yet?) part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Introduction
;; ------------
;;
;; This package is a minor mode to highlight the current line in buffer.
;;
;; highline was inspired on:
;;
;;    linemenu.el		  Bill Brodie <wbrodie@panix.com>
;;	 Hook function to highlight current line in buffer.
;;
;;    hl-line.el		  Dave Love <fx@gnu.org>
;;	 Highlight the current line.
;;
;;    highlight-current-line.el	  Christoph Conrad <christoph.conrad@gmx.de>
;;	 Highlight line where the cursor is.
;;
;; To use highline, insert in your ~/.emacs:
;;
;;    (require 'highline)
;;
;; For good performance, be sure to byte-compile highline.el, e.g.
;;
;;    M-x byte-compile-file <give the path to highline.el when prompted>
;;
;; This will generate highline.elc, which will be loaded instead of
;; highline.el.
;;
;; highline was tested with GNU Emacs 20.4.1.
;;
;;
;; Using highline
;; --------------
;;
;; * LOCAL highline (see NOTE 1 below):
;;    + To activate highline locally, type:
;;	    M-x highline-on RET
;;	 Or:
;;	    C-u 1 M-x highline-local-mode RET
;;
;;    + To deactivate highline locally, type:
;;	    M-x highline-off RET
;;	 Or:
;;	    C-u 0 M-x highline-local-mode RET
;;
;;    + To toggle highline locally, type:
;;	    M-x highline-local-mode RET
;;
;; * GLOBAL highline (see NOTE 1 below):
;;    + To activate highline globally, type:
;;	    M-x highline-mode-on RET
;;	 Or:
;;	    C-u 1 M-x highline-mode RET
;;
;;    + To deactivate highline globally, type:
;;	    M-x highline-mode-off RET
;;	 Or:
;;	    C-u 0 M-x highline-mode RET
;;
;;    + To toggle highline globally, type:
;;	    M-x highline-mode RET
;;
;; * INDIRECT highline (see NOTE 2 below):
;;    + To activate indirect highline, type:
;;	    M-x highline-view-on RET
;;	 Or:
;;	    C-u 1 M-x highline-view-mode RET
;;
;;    + To deactivate indirect highline, type:
;;	    M-x highline-view-off RET
;;	 Or:
;;	    C-u 0 M-x highline-view-mode RET
;;
;;    + To toggle indirect highline, type:
;;	    M-x highline-view-mode RET
;;
;; * To customize highline, type:
;;	 M-x highline-customize RET
;;
;; You can also bind `highline-local-mode', `highline-mode', `highline-on',
;; `highline-off', `highline-mode-on', `highline-mode-off',
;; `highline-customize', `highline-view-on', `highline-view-off' and
;; `highline-view-mode' to some key, like:
;;
;;    (global-set-key "\C-c\C-a"     'highline-on)
;;    (global-set-key "\C-c\C-b"     'highline-off)
;;    (global-set-key "\C-c\C-l"     'highline-local-mode)
;;    (global-set-key "\C-c\C-d"     'highline-mode-on)
;;    (global-set-key "\C-c\C-e"     'highline-mode-off)
;;    (global-set-key "\C-c\C-g"     'highline-mode)
;;    (global-set-key "\C-c\C-c"     'highline-customize)
;;    (global-set-key "\C-c\C-v\C-n" 'highline-view-on)
;;    (global-set-key "\C-c\C-v\C-f" 'highline-view-off)
;;    (global-set-key "\C-c\C-v\C-t" 'highline-view-mode)
;;
;; NOTE 1: There is no problem if you mix local and global minor mode usage.
;;
;; NOTE 2: Indirect highline (`highline-view-on', `highline-view-off' and
;;	   `highline-view-mode') is useful when you wish to have various
;;	   "visions" of the same buffer.
;;	   Indirect highline uses an indirect buffer to get the "vision" of the
;;	   buffer.  So, if you kill an indirect buffer, the base buffer is not
;;	   affected; if you kill the base buffer, all indirect buffer related
;;	   with the base buffer is automagicaly killed.  Also, any text
;;	   insertion/deletion in any indirect or base buffer is updated in all
;;	   related buffers.
;;
;;
;; Example
;; -------
;;
;; As an example, try to insert this in your .emacs file:
;;
;;  (require 'highline)
;;  ;; Turn on local highlighting for Dired (C-x d)
;;  (add-hook 'dired-after-readin-hook 'highline-on)
;;  ;; Turn on local highlighting for list-buffers (C-x C-b)
;;  (defadvice list-buffers (after highlight-line activate)
;;    (save-excursion
;;      (set-buffer "*Buffer List*")
;;      (highline-on)))
;;
;;
;; Hooks
;; -----
;;
;; highline has the following hook variables:
;;
;; `highline-hook'
;;    It is evaluated always when highline is turned on globally.
;;
;; `highline-local-hook'
;;    It is evaluated always when highline is turned on locally.
;;
;; `highline-view-hook'
;;    It is evaluated always when indirect highline is turned on.
;;
;; `highline-load-hook'
;;    It is evaluated after highline package is loaded.
;;
;;
;; Options
;; -------
;;
;; Below it's shown a brief description of highline options, please, see the
;; options declaration in the code for a long documentation.
;;
;; `highline-face'			Specify face used to highlight the
;;					current line.
;;
;; `highline-vertical-face'		Specify face used to highlight other
;;					than current line.
;;
;; `highline-line'			Specify which part of line should be
;;					highlighted.
;;
;; `highline-vertical'			Specify how many vertical lines should
;;					be highlighted.
;;
;; `highline-verbose'			Non-nil means generate messages.
;;
;; `highline-ignore-regexp'		Specify regexp for buffers to ignore.
;;
;; `highline-priority'			Specify highline overlay priority.
;;
;; `highline-selected-window'		Non-nil means highlight current line on
;;					current window.
;;
;; To set the above options you may:
;;
;; a) insert the code in your ~/.emacs, like:
;;
;;	 (setq highline-face 'highlight)
;;
;;    This way always keep your default settings when you enter a new Emacs
;;    session.
;;
;; b) or use `set-variable' in your Emacs session, like:
;;
;;	 M-x set-variable RET highline-face RET highlight RET
;;
;;    This way keep your settings only during the current Emacs session.
;;
;; c) or use customization, for example:
;;	 click on menu-bar *Help* option,
;;	 then click on *Customize*,
;;	 then click on *Browse Customization Groups*,
;;	 expand *Editing* group,
;;	 expand *Highline* group
;;	 and then customize highline options.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;; d) or see the option value:
;;
;;	 C-h v highline-face RET
;;
;;    and click the *customize* hypertext button.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;; e) or invoke:
;;
;;	 M-x highline-customize RET
;;
;;    and then customize highline options.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;;
;; Acknowledgements
;; ----------------
;;
;; Thanks to Sandip Chitale <sandip.chitale@brokat.com> for byte-compilation
;; tests.
;;
;; Thanks to Stephan Engelke <engelke@gmx.ne> for XEmacs tests.
;;
;; Thanks to Roman Belenov <roman@nstl.nnov.ru> for `pre-command-hook'
;; suggestion.
;;
;; Thanks to Trey Jackson <bigfaceworm@hotmail.com> for `highline-line'
;; enhancements.
;;
;; Thanks to Fredrik Sundstroem <fresun-7@sm.luth.se> for permanent-local
;; overlay property indication.
;;
;; Thanks to:
;;    Bill Brodie <wbrodie@panix.com>		   linemenu.el
;;    Dave Love <fx@gnu.org>			   hl-line.el
;;    Christoph Conrad <christoph.conrad@gmx.de>   highlight-current-line.el
;; And to all people who contributed with them.
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:


(eval-and-compile
  (cond
   ;; XEmacs
   ((let (case-fold-search)
      (string-match "XEmacs\\|Lucid\\|Epoch" emacs-version))
    ;; XEmacs needs overlay emulation package
    (or (require 'overlay)
	(error "`highline' requires `overlay' package."))
    (defun highline-alive-overlay (overlay-list)
      "Enforce that the car of OVERLAY-LIST isn't a deleted overlay."
      (when overlay-list
	(or (overlay-buffer (car overlay-list))
	    (setcar overlay-list (make-overlay 1 1)))
	overlay-list))
    (defun highline-move-overlay (overlay start end)
      "Move overlay even if the overlay is deleted."
      (and (overlay-buffer overlay)
	   (move-overlay overlay start end)))
    )
   ;; GNU Emacs
   (t
    (defalias 'highline-alive-overlay 'identity)
    (defalias 'highline-move-overlay  'move-overlay)
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Variables:


;;; Interface to the command system

(defgroup highline nil
  "Highlight the current line"
  :link '(emacs-library-link :tag "Source Lisp File" "highline.el")
  :group 'faces
  :group 'frames
  :group 'editing)


(defcustom highline-face 'highline-face
  "*Specify face used to highlight the current line."
  :type 'face
  :group 'highline)


(defface highline-face '((t (:background "paleturquoise")))
  "Face used to highlight current line.")


(defcustom highline-vertical-face 'highline-vertical-face
  "*Specify face used to highlight other than current line.

See also `highline-vertical'."
  :type 'face
  :group 'highline)


(defface highline-vertical-face '((t (:background "lightcyan")))
  "Face used to highlight other than current line.")


(defcustom highline-line nil
  "*Specify which part of line should be highlighted.

Valid values are:

   t			mark up to end of line.

   nil			mark up to window border.  On XEmacs, it behaves as t.
			NOTE: Let me know, if you find a way to mark up to
			      window border on XEmacs.

   INTEGER		mark up from beginning of line to column INTEGER or to
			end of line if INTEGER exceeds line length.  If INTEGER
			is negative, the region marked starts from end of line
			instead of beginning of line.

   (LOWER . UPPER)	mark up the region from column LOWER to column UPPER or
			to end of line if UPPER exceeds line length.  Nothing
			happens if LOWER exceeds line length.
			It must: 0 <= LOWER < UPPER.

   (beyond . INTEGER)	mark up the region from column INTEGER to end of line.
			Nothing happens if INTEGER exceeds line length.
			It must: INTEGER > 0.

   (point . INTEGER)	mark up the region from column
			(- (current-column) INTEGER) to column
			(+ (current-column) INTEGER).  It never goes beyond
			beginning or end of line.
			It must: INTEGER > 0.

Any other value is treated as t."
  :type '(choice :menu-tag "Mark Up To"
		 :tag "Mark Up To"
		 (const :tag "End Of Line" t)
		 (const :tag "Window Border" nil)
		 (integer :tag "Column")
		 (cons :tag "Point" :value (point . 0)
		       (const :tag "Point" point)
		       (integer :tag "To"))
		 (cons :tag "Beyond" :value (beyond . 0)
		       (const :tag "Beyond" beyond)
		       (integer :tag "From"))
		 (cons :tag "Range" :value (0 . 0)
		       (integer :tag "From")
		       (integer :tag "To")))
  :group 'highline)


(defcustom highline-vertical nil
  "*Specify how many vertical lines should be highlighted.

Valid values are:

   nil			Highlight only current line.

   t			Highlight all current window.

   (ABOVE . BELOW)	Highlight the vertical range from line
			(current-line-number - ABOVE) to line
			(current-line-number + BELOW).  ABOVE and BELOW should
			be integers.  There are the following cases:

			1. ABOVE <= 0 and BELOW <= 0
				This is the same as nil, that is, only current
				line is highlighted.  It's recommended to set
				`highline-vertical' to nil instead of (0 . 0),
				it'll have a better performance.

			2. ABOVE <= 0 and BELOW > 0
				Only current line and lines below will be
				highlighted.

			3. ABOVE > 0 and BELOW <= 0
				Only current line and lines above will be
				highlighted.

			4. ABOVE > 0 and BELOW > 0
				Current line, lines above and lines below will
				be highlighted.

Any other value is treated as t."
  :type '(choice :menu-tag ""
		 :tag ""
		 (const :tag "Only Current Line" nil)
		 (const :tag "All Current Window" t)
		 (cons :tag "Vertical Range" :value (1 . 1)
		       (integer :tag "Above")
		       (integer :tag "Below")))
  :group 'highline)


(defcustom highline-verbose t
  "*Non-nil means generate messages."
  :type 'boolean
  :group 'highline)


(defcustom highline-ignore-regexp
  (concat "Faces\\|Colors\\|Minibuf\\|\\*tip\\*"
	  ;; for example:
	  ;; "\\|RMAIL.*summary\\|\\*Group\\|\\*Summary"
	  )
  "*Specify regexp for buffers to ignore.

Set to nil or \"\", to accept any buffer.

Used by `highline-highlight-current-line'."
  :type 'regexp
  :group 'highline)


(defcustom highline-priority 0
  "*Specify highline overlay priority.

Higher integer means higher priority, so highline overlay will have precedence
over overlays with lower priority.  *Don't* use negative number."
  :type 'integer
  :group 'highline)


(defcustom highline-selected-window nil
  "*Non-nil means highlight current line on current window.

This is useful when you have a buffer in two or more windows and wish to
highlight only on current window."
  :type 'boolean
  :group 'highline)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; GNU Emacs
(or (fboundp 'line-beginning-position)
    (defun line-beginning-position (&optional n)
      (save-excursion
	(and n (/= n 1) (forward-line (1- n)))
	(beginning-of-line)
	(point))))


;; GNU Emacs
(or (fboundp 'line-end-position)
    (defun line-end-position (&optional n)
      (save-excursion
	(and n (/= n 1) (forward-line (1- n)))
	(end-of-line)
	(point))))

;; GNU Emacs
(defvar highlight-nonselected-window nil)
(make-variable-buffer-local 'highlight-nonselected-window)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros


(defmacro highline-message (&rest body)
  `(and highline-verbose (interactive-p)
	(message ,@body)))


(defmacro highline-minor-mode (arg mode on off message)
  `(progn
     (if (if arg
	     (> (prefix-numeric-value arg) 0)
	   (not ,mode))
	 (,on)
       (,off))
     (highline-message ,message (if ,mode "on" "off"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization


;;;###autoload
(defun highline-customize ()
  "Customize highline group."
  (interactive)
  (customize-group 'highline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User commands


(defvar highline-mode nil
  "Non-nil means highline global minor mode is enabled (HL on modeline).")


(defvar highline-local-mode nil
  "Non-nil means highline local minor mode is enabled (hl on modeline).")
(make-variable-buffer-local 'highline-local-mode)


(defvar highline-view-mode nil
  "Non-nil means highline view minor mode is enabled (Ihl on modeline).")
(make-variable-buffer-local 'highline-view-mode)


(defvar highline-overlays nil
  "Overlay list to highlight line(s)")
(make-variable-buffer-local 'highline-overlays)

(put 'highline-overlays 'permanent-local t)


;;;###autoload
(defun highline-mode (&optional arg)
  "Toggle global minor mode to highlight line about point (HL on modeline).

With ARG, turn highline mode on if ARG is positive, off otherwise.
Only useful with a windowing system."
  (interactive "P")
  (highline-minor-mode arg highline-mode
		       highline-mode-on highline-mode-off
		       "Highline global mode is %s"))


;;;###autoload
(defun highline-mode-on ()
  "Turn on global minor mode to highlight line about point (HL on modeline)."
  (interactive)
  (save-excursion
    (let ((buffers (buffer-list))
	  (temp (get-buffer-create (make-temp-name " *Temp"))))
      ;; be sure to access global `pre-command-hook' and `post-command-hook'
      (set-buffer temp)
      (setq highline-mode t)
      (add-hook 'mouse-leave-buffer-hook 'highline-unhighlight-current-line)
      (add-hook 'pre-command-hook 'highline-unhighlight-current-line)
      (add-hook 'post-command-hook 'highline-highlight-current-line)
      (add-hook 'window-scroll-functions 'highline-highlight-current-line)
      (while buffers			; adjust all local mode
	(set-buffer (car buffers))
	(unless highline-local-mode
	  (add-hook 'pre-command-hook 'highline-unhighlight-current-line nil t)
	  (add-hook 'post-command-hook 'highline-highlight-current-line nil t)
	  (add-hook 'window-scroll-functions
		    'highline-highlight-current-line nil t)
	  (highline-highlight-current-line))
	(setq buffers (cdr buffers)))
      (highline-highlight-current-line)
      (kill-buffer temp)))
  (run-hooks 'highline-hook)
  (highline-message "Highline global mode is on"))


;;;###autoload
(defun highline-mode-off ()
  "Turn off global minor mode to highlight line about point (HL on modeline)."
  (interactive)
  (save-excursion
    (let ((buffers (buffer-list))
	  (temp (get-buffer-create (make-temp-name " *Temp"))))
      ;; be sure to access global `pre-command-hook' and `post-command-hook'
      (set-buffer temp)
      (setq highline-mode nil)
      (remove-hook 'mouse-leave-buffer-hook 'highline-unhighlight-current-line)
      (remove-hook 'pre-command-hook 'highline-unhighlight-current-line)
      (remove-hook 'post-command-hook 'highline-highlight-current-line)
      (remove-hook 'window-scroll-functions 'highline-highlight-current-line)
      (while buffers			; adjust all local mode
	(set-buffer (car buffers))
	(unless highline-local-mode
	  (remove-hook 'pre-command-hook 'highline-unhighlight-current-line t)
	  (remove-hook 'post-command-hook 'highline-highlight-current-line t)
	  (remove-hook 'window-scroll-functions
		       'highline-highlight-current-line t)
	  (highline-unhighlight-current-line))
	(setq buffers (cdr buffers)))
      (kill-buffer temp)))
  (highline-message "Highline global mode is off"))


;;;###autoload
(defun highline-local-mode (&optional arg)
  "Toggle local minor mode to highlight the line about point (hl on modeline).

With ARG, turn highline mode on if ARG is positive, off otherwise.
Only useful with a windowing system."
  (interactive "P")
  (highline-minor-mode arg highline-local-mode
		       highline-on highline-off
		       "Highline local mode is %s"))


;;;###autoload
(defun highline-on ()
  "Turn on local highlighting of the current line in buffer (hl on modeline)."
  (interactive)
  (setq highline-local-mode t)
  (highline-local-on)
  (run-hooks 'highline-local-hook)
  (highline-message "Highline local mode is on"))


;;;###autoload
(defun highline-off ()
  "Turn off local highlighting of the current line in buffer (hl on modeline)."
  (interactive)
  (setq highline-local-mode nil)
  (highline-local-off)
  (highline-message "Highline local mode is off"))


;;;###autoload
(defun highline-view-mode (&optional arg)
  "Toggle indirect mode to highlight current line in buffer (Ihl on modeline).

With ARG, turn highline mode on if ARG is positive, off otherwise.
Only useful with a windowing system.

Indirect highline (`highline-view-on', `highline-view-off' and
`highline-view-mode') is useful when you wish to have various \"visions\" of
the same buffer.

Indirect highline uses an indirect buffer to get the \"vision\" of the buffer.
So, if you kill an indirect buffer, the base buffer is not affected; if you
kill the base buffer, all indirect buffer related with the base buffer is
automagicaly killed.  Also, any text insertion/deletion in any indirect or base
buffer is updated in all related buffers.

See also `highline-selected-window'."
  (interactive "P")
  (highline-minor-mode arg highline-view-mode
		       highline-view-on highline-view-off
		       "Highline view mode is %s"))


;;;###autoload
(defun highline-view-on ()
  "Turn on indirect highlightining current line in buffer (Ihl on modeline).

Indirect highline (`highline-view-on', `highline-view-off' and
`highline-view-mode') is useful when you wish to have various \"visions\" of
the same buffer.

Indirect highline uses an indirect buffer to get the \"vision\" of the buffer.
So, if you kill an indirect buffer, the base buffer is not affected; if you
kill the base buffer, all indirect buffer related with the base buffer is
automagicaly killed.  Also, any text insertion/deletion in any indirect or base
buffer is updated in all related buffers.

See also `highline-selected-window'."
  (interactive)
  (let* ((local-buffer-read-only buffer-read-only)
	 (buffer (current-buffer))
	 (name (generate-new-buffer-name
		(concat "{"
			(buffer-name (or (buffer-base-buffer buffer) buffer))
			" View}"))))
    (switch-to-buffer (make-indirect-buffer buffer name))
    (setq buffer-read-only local-buffer-read-only))
  (setq highline-view-mode t)
  (highline-local-on)
  (run-hooks 'highline-view-hook)
  (highline-message "Highline view mode is on"))


;;;###autoload
(defun highline-view-off ()
  "Turn off indirect highlightining current line in buffer (Ihl on modeline).

Indirect highline (`highline-view-on', `highline-view-off' and
`highline-view-mode') is useful when you wish to have various \"visions\" of
the same buffer.

Indirect highline uses an indirect buffer to get the \"vision\" of the buffer.
So, if you kill an indirect buffer, the base buffer is not affected; if you
kill the base buffer, all indirect buffer related with the base buffer is
automagicaly killed.  Also, any text insertion/deletion in any indirect or base
buffer is updated in all related buffers.

See also `highline-selected-window'."
  (interactive)
  (when highline-view-mode
    (setq highline-view-mode nil)
    (highline-local-off)
    (let* ((buffer (current-buffer))
	   (base   (buffer-base-buffer buffer)))
      (when base
	(kill-buffer buffer)
	(switch-to-buffer base)))
    (highline-message "Highline view mode is off")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions


(defun highline-local-on ()
  (add-hook 'mouse-leave-buffer-hook 'highline-unhighlight-current-line)
  (add-hook (make-local-variable 'pre-command-hook)
	    'highline-unhighlight-current-line nil t)
  (add-hook (make-local-variable 'post-command-hook)
	    'highline-highlight-current-line nil t)
  (add-hook (make-local-variable 'window-scroll-functions)
	    'highline-highlight-current-line nil t)
  (highline-highlight-current-line))


(defun highline-local-off ()
  (remove-hook 'mouse-leave-buffer-hook 'highline-unhighlight-current-line)
  (remove-hook 'pre-command-hook 'highline-unhighlight-current-line t)
  (remove-hook 'post-command-hook 'highline-highlight-current-line t)
  (remove-hook 'window-scroll-functions 'highline-highlight-current-line t)
  (highline-unhighlight-current-line))


(defsubst highline-column-position (column)
  (save-excursion
    (move-to-column (max 0 column))
    (point)))


(defun highline-unhighlight-current-line (&rest ignore)
  "Unhighlight current line."
  (let ((overs highline-overlays))
    (while (and overs
		(overlay-end (car overs))
		(> (overlay-end (car overs)) 1))
      (highline-move-overlay (car overs) 1 1)
      (setq overs (cdr overs)))))


(defun highline-highlight-current-line (&rest ignore)
  "Highlight current line."
  (unless (and highline-ignore-regexp
	       (not (equal "" highline-ignore-regexp))
	       (string-match highline-ignore-regexp (buffer-name)))
    (setq highlight-nonselected-window (not highline-selected-window))
    (save-excursion
      (let* ((column       (current-column))
	     (overs        highline-overlays)
	     (lines        (highline-vertical))
	     (current-line (cdr lines)))
	(setq lines (car lines))
	(while (let ((ov (car (or (highline-alive-overlay overs)
				  (setq highline-overlays
					(cons (make-overlay 1 1) ; hide it
					      highline-overlays)))))
		     pointp rangep beyondp)
		 (setq overs (cdr overs))
		 ;; set current overlay properties
		 (overlay-put ov 'hilit t)
		 (overlay-put ov 'face (if (= lines current-line)
					   highline-face
					 highline-vertical-face))
		 (overlay-put ov 'priority highline-priority)
		 (and highline-selected-window
		      (overlay-put ov 'window (selected-window)))
		 ;; move highlight to the current line
		 (and (consp highline-line)
		      (integerp (cdr highline-line))
		      (> (cdr highline-line) 0)
		      (or (setq beyondp (eq (car highline-line) 'beyond))
			  (setq pointp  (eq (car highline-line) 'point))
			  (setq rangep  (and (integerp (car highline-line))
					     (>= (car highline-line) 0)
					     (< (car highline-line)
						(cdr highline-line))))))
		 (move-overlay
		  ;; overlay
		  ov
		  ;; start point
		  (cond (rangep		; (LOWER . UPPER)
			 (highline-column-position (car highline-line)))
			(beyondp	; (beyond . INTEGER)
			 (highline-column-position (cdr highline-line)))
			(pointp		; (point . INTEGER)
			 (highline-column-position
			  (- column (cdr highline-line))))
			((integerp highline-line) ; INTEGER
			 (if (>= highline-line 0)
			     (line-beginning-position)
			   (line-end-position)))
			((line-beginning-position))) ; t or nil
		  ;; end point
		  (cond (rangep		; (LOWER . UPPER)
			 (highline-column-position (cdr highline-line)))
			(pointp		; (point . INTEGER)
			 (highline-column-position
			  (+ column (cdr highline-line))))
			((integerp highline-line) ; INTEGER
			 (highline-column-position
			  (if (>= highline-line 0)
			      highline-line
			    (save-excursion
			      (end-of-line)
			      (+ column highline-line)))))
			(highline-line	; t or (beyond . INTEGER)
			 (line-end-position))
			((min (point-max) ; nil
			      (1+ (line-end-position))))))
		 ;; while condition
		 (> (setq lines (1- lines)) 0))
	  ;; while body
	  (forward-line 1))
	;; unhighlight remainding overlays, if any
	(while (and overs (> (overlay-end (car overs)) 1))
	  (highline-move-overlay (car overs) 1 1)
	  (setq overs (cdr overs)))))))


(defun highline-vertical ()
  (cond
   ;; nil - only current line
   ((null highline-vertical)
    '(1 . 1))
   ;; (ABOVE . BELOW) - vertical range
   ((and (consp highline-vertical)
	 (let ((above (car highline-vertical))
	       (below (cdr highline-vertical)))
	   (and (integerp above)
		(integerp below)
		(let ((below (1+ (max below 0))))
		  (cons (if (<= above 0)
			    below
			  (forward-line (- above))
			  (+ above below))
			below))))))
   ;; t - all current window
   (t
    (let ((height (window-height))
	  (start  (window-start)))
      (prog1
	  (cons (1- height)
		(- height
		   (count-lines start (point))
		   (if (zerop (current-column)) 1 0)))
	(goto-char start))))
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-to-list 'minor-mode-alist '(highline-mode " HL"))
(add-to-list 'minor-mode-alist '(highline-local-mode " hl"))
(add-to-list 'minor-mode-alist '(highline-view-mode " Ihl"))


(provide 'highline)


(run-hooks 'highline-load-hook)


;;; highline.el ends here
