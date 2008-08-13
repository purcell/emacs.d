;;; dialog.el - Code for starting, and managing dialogs buffers
;;;
;;; Copyright (C) 1995, 1996, 1997 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; RCS: $Id: dialog-mode.el,v 1.21 1997/03/01 16:39:13 zappo Exp $
;;; Keywords: OO widget dialog
;;;                     
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to:
;;;
;;;              The Free Software Foundation, Inc.
;;;              675 Mass Ave.
;;;              Cambridge, MA 02139, USA. 
;;;
;;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;;

;;;
;;; Commentary:
;;;   Dialog mode requires the use of widget-d and widget-i.  It
;;; supplies mundane functions (basic drawing routines w/ faces) and
;;; also the framework in which the widgets work (The buffer, mode,
;;; keymap, etc).  Using `dialog-mode' lets you create a dialog in
;;; which you can place buttons and text fields within a top-level
;;; shell.  This mode manages the keymap, and the input is distributed
;;; to the correct active widget.
;;;
;;;  Dialog-mode also provides the most basic framework needed for
;;; creating other modes which may need widgets in them.  The general
;;; keymaps and things needed can be re-used to create widgets
;;; embedded in other types of modes.
;;;
;;; To create a new dialog, you must follow these basic steps:
;;; 1) Create a new blank buffer
;;; 2) run `dialog-mode' on it
;;; 3) use `create-widget' to make widgets to make your dialog useful
;;;    (create-widget name class parent &rest resources)
;;;    - widget-label          - static text
;;;    - widget-button         - push it to make something happen
;;;    - widget-toggle-button  - push it to change a value
;;;    - widget-text-field     - a place to edit simple text
;;;    - widget-frame          - make a box around other widgets
;;;    Resources are :keys which specify how your widget behaves
;;; 4) call `dialog-refresh'
;;;
;;; Making widgets talk to eachother:
;;; Asside from `widget-core' there is also a `data-object' which
;;; provides a method for widgets to talk to eachother (ala
;;; fresco/interviews style)  A widget will create a data object if
;;; one is not given to it.  A widget always register's itself with
;;; the data object, and these objects alert viewers if they are
;;; changed.  In this way, a toggle button will automatically update
;;; itself if it's data has changed.
;;;
;;; For examples of how to make widgets interact, examine the function
;;; `dialog-test'

(require 'widget-i)

(eval-and-compile
(defvar dialog-xemacs-p (string-match "XEmacs" emacs-version)
  "Are we running in Xemacs?")
)
;;;
;;; Widget definitions using eieio
;;; 
         
(defvar widget-toplevel-shell nil
  "Buffer local variable containing the definition of the toplevel shell.
This shell is active in the current buffer.  There can be only one
toplevel shell definition in a given buffer.  It should maintain a
buffer-local value as any buffer may wish to have embedded widgets.")
(make-variable-buffer-local 'widget-toplevel-shell)

(defvar dialog-current-parent nil
  "This variable is defined inside a `dialog-build-group' form.
It's value is the widget defined as the first parameter to
`dialog-build-group'")

;;;
;;; Dialog mode variables
;;;
(defun dialog-superbind-alpha (keymap fn)
  "In KEYMAP bind all occurances of alphanumeric keys to FN.
An alphanumeric key is any value between 0 and 128"
  (let ((key "\00"))
    (aset key 0 0)
    (while (< (aref key 0) 128)
      (define-key keymap key fn)
      (aset key 0 (1+ (aref key 0))))))

(defvar dialog-mode-map nil 
  "Keymap used in dialog mode.")

(defvar dialog-meta-map nil
  "Keymap used to trap meta-keys.")

(if dialog-mode-map () 
  ;; create and fill up the keymap with our event handler
  (setq dialog-mode-map (make-keymap))
  (dialog-superbind-alpha dialog-mode-map 'dialog-handle-kbd)
  (setq dialog-meta-map (make-keymap))
  (dialog-superbind-alpha dialog-meta-map 'dialog-handle-meta-kbd)
  ;; some keys we don't want to override
  (define-key dialog-mode-map "\e" dialog-meta-map)
  (define-key dialog-mode-map "\C-x" nil)
  (define-key dialog-mode-map "\C-z" nil)
  (define-key dialog-mode-map "\C-c" nil)
  (define-key dialog-mode-map "\C-h" nil)
  (define-key dialog-mode-map "\C-l" nil)
  (define-key dialog-mode-map "\C-g" nil)
  (define-key dialog-mode-map "\C-u" nil)
  ;; Some keys to capture only sometimes
  (define-key dialog-mode-map "\C-n" 'dialog-handle-kbd-maybe)
  (define-key dialog-mode-map "\C-p" 'dialog-handle-kbd-maybe)
  (define-key dialog-mode-map "\C-f" 'dialog-handle-kbd-maybe)
  (define-key dialog-mode-map "\C-b" 'dialog-handle-kbd-maybe)
  (define-key dialog-mode-map "\C-e" 'dialog-handle-kbd-maybe)
  (define-key dialog-mode-map "\C-a" 'dialog-handle-kbd-maybe)
  ;; Some keys in meta mat should not be overridden
  (define-key dialog-meta-map "x" nil)
  (define-key dialog-meta-map ":" nil)
  ;; Some keys have special meaning that we can grab at this level
  (define-key dialog-meta-map "\t" 'dialog-prev-widget)
  (define-key dialog-mode-map "\C-\M-n" 'dialog-next-widget)
  (define-key dialog-mode-map "\C-\M-p" 'dialog-prev-widget)
  (define-key dialog-mode-map "\C-i" 'dialog-next-widget)
  (define-key dialog-mode-map "\C-c\C-r" 'dialog-refresh)
  (define-key dialog-mode-map "\C-hW" 'dialog-widget-help)

  ;; Differences between Xemacs and Emacs keyboard
  (if (string-match "XEmacs" emacs-version)
      (progn
	;; A note about the meta-map.  XEMacs won't let me create
	;; a special keymap for meta keys the way regulare emacs will.
	;; As a result, the handy meta-keys won't work in XEmacs for
	;; text widgets and such.  If you know how to make this work
	;; let me know, but for now, no meta keys in text fields.

	;; some translations into text
	(define-key dialog-mode-map 'tab 'dialog-next-widget)
	(define-key dialog-mode-map '(meta tab) 'dialog-prev-widget)
	(define-key dialog-mode-map 'up 'dialog-handle-kbd-maybe)
	(define-key dialog-mode-map 'down 'dialog-handle-kbd-maybe)
	(define-key dialog-mode-map 'right 'dialog-handle-kbd-maybe)
	(define-key dialog-mode-map 'left 'dialog-handle-kbd-maybe)
	(define-key dialog-mode-map 'next 'dialog-handle-kbd-maybe)
	(define-key dialog-mode-map 'prev 'dialog-handle-kbd-maybe)

	;; Now some mouse events
	(define-key dialog-mode-map 'button2 'dialog-handle-mouse)
	(define-key dialog-mode-map '(meta button2) 'dialog-mouse-widget-help)
	)
    ;; some translations into text
    (define-key dialog-mode-map [tab] 'dialog-next-widget)
    (define-key dialog-mode-map [shift tab] 'dialog-prev-widget)
    (define-key dialog-mode-map [up] 'dialog-handle-kbd-maybe)
    (define-key dialog-mode-map [down] 'dialog-handle-kbd-maybe)
    (define-key dialog-mode-map [right] 'dialog-handle-kbd-maybe)
    (define-key dialog-mode-map [left] 'dialog-handle-kbd-maybe)
    (define-key dialog-mode-map [next] 'dialog-handle-kbd-maybe)
    (define-key dialog-mode-map [prev] 'dialog-handle-kbd-maybe)
    (define-key dialog-mode-map [home] 'dialog-handle-kbd-maybe)
    (define-key dialog-mode-map [end] 'dialog-handle-kbd-maybe)
    ;; Now some mouse events
    (define-key dialog-mode-map [mouse-2] 'dialog-handle-mouse)
    (define-key dialog-mode-map [down-mouse-2] 'dialog-handle-mouse)
    (define-key dialog-mode-map [drag-mouse-2] 'dialog-handle-mouse)
    ;; Some interactive help on widgets
    (define-key dialog-mode-map [M-mouse-2] 'dialog-mouse-widget-help)
    ))
  
(defun dialog-load-color (sym l-fg l-bg d-fg d-bg &optional bold italic underline)
  "Create a color for SYM with a L-FG and L-BG color, or D-FG and D-BG.
Optionally make BOLD, ITALIC, or UNDERLINED if applicable.  If the
background attribute of the current frame is determined to be light
{white, for example} then L-FG and L-BG is used.  If not, then D-FG
and D-BG is used.  This will allocate the colors in the best possible
mannor.  This will allow me to store multiple defaults and dynamically
determine which colors to use."
  (if window-system
      (let* ((params (frame-parameters))
	     (disp-res (if (fboundp 'x-get-resource)
			   (if dialog-xemacs-p
			       (x-get-resource ".displayType" "DisplayType" 'string)
			     (x-get-resource ".displayType" "DisplayType"))
			 nil))
	     (display-type
	      (cond (disp-res (intern (downcase disp-res)))
		    ((and (fboundp 'x-display-color-p) (x-display-color-p)) 'color)
		    (t 'mono)))
	     (bg-res (if (fboundp 'x-get-resource)
			 (if (eval-when-compile dialog-xemacs-p)
			     (x-get-resource ".backgroundMode" "BackgroundMode" 'string)
			   (x-get-resource ".backgroundMode" "BackgroundMode"))
		       nil))
	     (bgmode
	      (cond (bg-res (intern (downcase bg-res)))
		    ((let* ((bgc (or (cdr (assq 'background-color params))
				     (if (eval-when-compile dialog-xemacs-p)
					 (x-get-resource ".background"
							 "Background" 'string)
				       (x-get-resource ".background"
						       "Background"))
				 ;; if no other options, default is white
				 "white"))
			    (bgcr (if (eval-when-compile dialog-xemacs-p)
				      (color-instance-rgb-components
				       (make-color-instance bgc))
				    (x-color-values bgc)))
			    (wcr (if (eval-when-compile dialog-xemacs-p)
				     (color-instance-rgb-components
				      (make-color-instance "white"))
				   (x-color-values "white"))))
		       (< (apply '+ bgcr) (/ (apply '+ wcr) 3)))
		     'dark)
		    (t 'light)))	;our default
	     (set-p (function (lambda (face-name resource)
				(if dialog-xemacs-p
				    (x-get-resource 
				     (concat face-name ".attribute" resource)
				     (concat "Face.Attribute" resource)
				     'string)
				  (x-get-resource 
				   (concat face-name ".attribute" resource)
				   (concat "Face.Attribute" resource)))
				)))
	     (nbg (cond ((eq bgmode 'dark) d-bg) 
			(t l-bg)))
	     (nfg (cond ((eq bgmode 'dark) d-fg)
			(t l-fg))))

	(if (not (eq display-type 'color))
	    ;; we need a face of some sort, so just make due with default
	    (progn
	      (copy-face 'default sym)
	      (if bold (condition-case nil
			   (make-face-bold sym)
			 (error (message "Cannot make face %s bold!" 
					 (symbol-name sym)))))
	      (if italic (condition-case nil
			     (make-face-italic sym)
			   (error (message "Cannot make face %s italic!"
					   (symbol-name sym)))))
	      (set-face-underline-p sym underline)
	      )
	  ;; make a colorized version of a face.  Be sure to check Xdefaults
	  ;; for possible overrides first!
	  (let ((newface (make-face sym)))
	    ;; For each attribute, check if it might already be set by Xdefaults
	    (if (and nfg (not (funcall set-p (symbol-name sym) "Foreground")))
		(set-face-foreground newface nfg))
	    (if (and nbg (not (funcall set-p (symbol-name sym) "Background")))
		(set-face-background newface nbg))
	
	    (if bold (condition-case nil
			 (make-face-bold newface)
		       (error (message "Cannot make face %s bold!"
				       (symbol-name sym)))))
	    (if italic (condition-case nil
			   (make-face-italic newface)
			 (error (message "Cannot make face %s italic!"
					 (symbol-name newface)))))
	    (set-face-underline-p newface underline)
	    )))))

(dialog-load-color 'widget-default-face nil nil nil nil)
(dialog-load-color 'widget-box-face "gray30" nil "gray" nil)
(dialog-load-color 'widget-frame-label-face "red3" nil "#FFFFAA" nil nil t nil)
(dialog-load-color 'widget-focus-face "green4" nil "light green" nil t)
(dialog-load-color 'widget-arm-face nil "cyan" nil "cyan4")
(dialog-load-color 'widget-indicator-face "blue4" nil "cyan" nil t)
(dialog-load-color 'widget-text-face nil nil nil nil nil nil t)
(dialog-load-color 'widget-text-focus-face nil nil nil nil t nil t)
(dialog-load-color 'widget-text-button-face "black" "cyan" nil "blue3")

(defun dialog-mode ()
  "Major mode for interaction with widgets.

A widget is any of a number of rectangular regions on the screen with
certain visual effects, and user actions.

All keystrokes are interpreted by the widget upon which the cursor
resides.  Thus SPC on a button activates it, but in a text field, it
will insert a space into the character string.

Reference the Info node @xref{(dialog)Top} for details about dialog mode
and all the support widgets.

Getting Help:
  Some dialog boxes will offer widget-level help if you do not know what
a widget is for.  To get at this help, hold down the META key while
clicking with the middle mouse button, or press the command key `C-h W' 
Where W is a capital.

\\<dialog-mode-map>
Commands:
  \\[dialog-next-widget]   - Move to next interactive widget
  \\[dialog-prev-widget] - Move to previous interactive widget
  \\[dialog-refresh] - Refresh the display
  \\[dialog-widget-help] - Get help for widget under the cursor
"
  (kill-all-local-variables)
  (setq mode-name "Dialog")
  (setq major-mode 'dialog-mode)
  (use-local-map dialog-mode-map)
  (setq mode-line-buffer-identification	(list "DIALOG: " "%15b ")
	mode-line-modified '("--")

	widget-toplevel-shell 
	(widget-toplevel "topLevel" :parent t :rx 0 :x 0 :ry 1 :y 1))
  (message "Constructing Dialog...")
  (verify widget-toplevel-shell t)
  (run-hooks 'dialog-mode-hooks))

(defmacro dialog-with-writeable (&rest forms)
  "Allow the buffer to be writable and evaluate FORMS.
Turn read-only back on when done."
  (list 'let '((dialog-with-writeable-buff (current-buffer)))
	'(toggle-read-only -1)
	(cons 'progn forms)
	'(save-excursion (set-buffer dialog-with-writeable-buff)
			 (toggle-read-only 1))))
(put 'dialog-with-writeable 'lisp-indent-function 0)
(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec dialog-with-writeable
	      def-body)))

(defun dialog-refresh () "Refresh all visible widgets in this buffer."
  (interactive)
  (dialog-with-writeable
    (erase-buffer)
    (message "Geometry Management...")
    (or (interactive-p)
	(verify-size widget-toplevel-shell))
    (message "Rendering Dialog...")
    (draw widget-toplevel-shell)))

(defun dialog-quit () "Quits a dialog."
  (bury-buffer))

(defun dialog-widget-help () "Gain help for the widget under the cursor."
  (interactive)
  (dialog-with-writeable
    (let ((dispatch (or (get-text-property (point) 'widget-object)
			widget-toplevel-shell)))
      (help-actions dispatch nil))))
  
(defun dialog-mouse-widget-help (e)
 "Go where the mouse clicks and call `dialog-widget-help'."
 (interactive "e")
 (mouse-set-point e)
 (dialog-widget-help))

(defun dialog-lookup-key (keymap coe)
  "Translate from KEYMAP the event COE. Return the command keybinding.
This task isn't as simple as it should be."
  (let ((cc (cond ((numberp coe)
		   (char-to-string coe))
		  ((stringp coe)
		   coe)
		  (t
		   (or (lookup-key function-key-map (make-vector 1 coe))
		       (make-vector 1 coe))))))
    (lookup-key keymap cc t)))

(defvar dialog-last-maybe-command nil
  "The last command run by `dialog-handle-kbd*' for tracking
last-command setting while running interpreted commands.")

(defun dialog-handle-kbd () "Read the last kbd event, and handle it."
  (interactive)
  (dialog-with-writeable
    (let ((dispatch (or (get-text-property (point) 'widget-object)
			widget-toplevel-shell)))
      (input dispatch (if last-input-char last-input-char last-input-event)))))

(defun dialog-handle-kbd-maybe ()
 "Read the last kbd event, and handle it if a widget is registered.
A registered widget has marked the area of text under point. If the
text is not marked, the default keybinding is run instead."
  (interactive)
  (dialog-with-writeable
    (let* ((dispatch (get-text-property (point) 'widget-object))
	   (hm (and dispatch (oref dispatch handle-motion))))
      (if (and dispatch hm)
	  ;; if the object's motion is traditional, handle it here,
	  ;; but if we exceede the boundaries of the widget, run it's
	  ;; input event with it so it can scroll.
	  (if (eq hm 'traditional)
	      (let ((command (dialog-lookup-key global-map
						(if last-input-char
						    last-input-char
						  last-input-event)))
		    (p (point)))
		(command-execute command t)
		(if (eq dispatch (get-text-property (point) 'widget-object))
		    nil
		  (goto-char p)
		  (input dispatch (if last-input-char last-input-char
				    last-input-event))))
	    ;; run the input of the found object.
	    (input dispatch (if last-input-char last-input-char
			      last-input-event)))
	;; do normal keys here since the widget doesn't care about the
	;; maybe key set.
	(let ((command (dialog-lookup-key global-map
					  (if last-input-char last-input-char
					    last-input-event)))
	      (last-command (if (eq last-command 'dialog-handle-kbd-maybe)
				dialog-last-maybe-command)))
	  (command-execute command t)
	  (setq dialog-last-maybe-command command))))))

(defun dialog-handle-meta-kbd () "Read the last kbd event, and handle it as a meta key."
  (interactive)
  (dialog-with-writeable
    (let ((dispatch (or (get-text-property (point) 'widget-object)
			widget-toplevel-shell)))
      (input dispatch (if (numberp last-input-char)
			  (concat "\e" (char-to-string last-input-char))
			last-input-char)))))

(defun dialog-next-widget (arg) "Move cursor to next logical widget."
  (interactive "P")
  (choose-next-widget widget-toplevel-shell 
		      (cond ((null arg) 1)
			    ((listp arg) (car arg))
			    (t arg)))
  )

(defun dialog-prev-widget (arg) "Move cursor to previous logical widget."
  (interactive "P")
  (choose-next-widget widget-toplevel-shell 
		      (cond ((null arg) -1)
			    ((listp arg) (- (car arg)))
			    (t (- arg))))

  )

(if (eval-when-compile dialog-xemacs-p)

(defun dialog-mouse-event-p (event)
  "Return t if the event is a mouse related event"
  ;; xemacs event is really a dialog-built list, with the real event
  ;; as the second element
  (and (listp event) (mouse-event-p (car (cdr event))))
  )

(defun dialog-mouse-event-p (event)
  "Return t if the event is a mouse related event"
  (if (and (listp event)
	   (member (event-basic-type event)
		   '(mouse-1 mouse-2 mouse-3)))
      t
    nil))
)

(defun dialog-handle-mouse (event) "Reads last mouse event, and handle it"
  (interactive "e")
  ;; First, check to see where the click is, and go there.  The cursor
  ;; will act as our in the widget fields.
  (mouse-set-point event)
  (dialog-with-writeable
    (let ((dispatch (or (get-text-property (point) 'widget-object)
			widget-toplevel-shell)))
      ;; This should make the byte complier optimize this expression
      ;; right out of GNU versions
      (if (eval-when-compile dialog-xemacs-p)
	  ;; translate the event into a FSF event.  (Basically the event
	  ;; key type, followed by the XEmacs event so we can use menu
	  ;; things with it later.
	  (setq event (list (if (button-press-event-p event)
				'mouse-down-2 'mouse-2)
			    event))
	  )
      (input dispatch event))))



;;; Widget creation routines and convenience functions
(defmacro dialog-build-group (widget &rest forms)
  "This is similar to a `progn' where new WIDGET becomes the default parent.
All new widgets created within FORMS will have WIDGET as its parent
unless otherwise specified."
  (list 'let* (list (list 'dw widget)
		    (list 'dialog-current-parent 
			  '(if (stringp dw) 
			       (create-widget dw widget-frame)
			     dw)))
	(cons 'progn forms)))

;; Now fix this macro for various thingies
(put 'dialog-build-group 'lisp-indent-function 1)
(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec dialog-build-group
	      def-body)))

(defun create-widget (name class &rest resources)
  "Creates a dialog widget with name NAME of class CLASS.
The parent will be defined from the current environment created by
`dialog-build-group'.  RESOURCES is a list to be passed tot he CLASS
routine.

  This function is current BACKWARD COMPATIBLE so that an optional 3rd
argument could be the parent widget, overriding any enviroment from
`dialog-build-group'.  This will be removed in future versions."
  (let* ((parent 
	  (if (and (object-p (car resources)) 
		   (obj-of-class-p (car resources) widget-group))
	      (prog1 (car resources)
		(setq resources (cdr resources)))
	    (or dialog-current-parent widget-toplevel-shell)))
	 (con (class-constructor class))
	 (new (apply con name resources))
	 )
    ;; add this child to the parent, which sets news parent field
    (add-child parent new)
    ;; call the verifier on this new widget.  Verify will transfor
    ;; construction values ('below, 'just-right, nil) into valid
    ;; values in pertinent fields by recursivly dropping from high
    ;; level widget restrictions to low-level widget restrictions
    (verify new t)
    new))

;; Not backward compatible because it didn't exist before
(defun create-widget-first (name class &rest resources)
  "Creates a dialog widget with name NAME of class CLASS.
The parent will be defined from the current environment, and RESOURCES
is a list to be passed tot he CLASS routine."
  (let* ((con (class-constructor class))
	 (new (apply con name resources))
	 (parent (or dialog-current-parent widget-toplevel-shell)))
    ;; add this child to the parent, which sets news parent field
    (add-child parent new)
    ;; call the verifier on this new widget.  Verify will transfor
    ;; construction values ('below, 'just-right, nil) into valid
    ;; values in pertinent fields by recursivly dropping from high
    ;; level widget restrictions to low-level widget restrictions
    (verify new t)
    new))

(defun create-widget-parent (name class parent &rest resources)
  "Create a dialog widget with name NAME of class CLASS with PARENT.
PARENT will be the widget this new widget resides in, and RESOURCES is
a list to be passed to the CLASS routine"
  (let* ((con (class-constructor class))
	 (new (apply con name resources)))
    ;; add this child to the parent, which sets news parent field
    (add-child parent new)
    ;; call the verifier on this new widget.  Verify will transfor
    ;; construction values ('below, 'just-right, nil) into valid
    ;; values in pertinent fields by recursivly dropping from high
    ;; level widget restrictions to low-level widget restrictions
    (verify new t)
    new))

(defun create-widget-parent-first (name class parent &rest resources)
  "Create a dialog with name NAME of class CLASS with PARENT.
PARENT will be the widget this new widget resides in, and RESOURCES is
a list to be passed to the CLASS routine."
  ;;(message "Building Dialog... [%s]" name)
  (let* ((con (class-constructor class))
	 (new (apply con name resources)))
    ;; add this child to the parent, which sets news parent field
    (add-child parent new t)
    ;; call the verifier on this new widget.  Verify will transfor
    ;; construction values ('below, 'just-right, nil) into valid
    ;; values in pertinent fields by recursivly dropping from high
    ;; level widget restrictions to low-level widget restrictions
    (verify new t)
    new))

(defun transform-dataobject (thing-or-obj w dval fix)
  "Takes THING-OR-OBJ, and if it's a data-object, returns it,
otherwise create a new data object, and set it's initial value to
THING-OR-OBJ, and set its first watcher to W.  If THING-OR-OBJ is not
an object name id DVAL when created, also, if THING-OR-OBJ is nil,
and not some other value, then set it's value to DVAL instead.  If FIX
is nil, then return nil instead."
  (if (or (not (object-p thing-or-obj))
	  (not (child-of-class-p (object-class thing-or-obj) data-object)))
      (if fix
	  (let ((newo (data-object dval)))
	    ;; Add this to widget
	    (add-reference newo w)
	    (if thing-or-obj
		(set-value newo thing-or-obj w)
	      (set-value newo dval w))
	    newo)
	nil)
    thing-or-obj))

(if (eval-when-compile dialog-xemacs-p)

(defun widget-lock-over (w)
  "Called by a widget which wishes to grab cursor until the
mouse button is released event is recieved."
  ;; For now, XEmacs will just immediatly run the button
  nil
)

(defun widget-lock-over (w)
  "Called by a widget which wishes to grab cursor until the
'click event is recieved."
  (let ((event nil))
    (track-mouse
      (while (progn (setq event (read-event))
		    (or (mouse-movement-p event)
			(eq (car-safe event) 'switch-frame)))
	(if (eq (car-safe event) 'switch-frame)
	    nil
	  ;; (mouse-set-point event)
	  (motion-input w event)))
      (if event (mouse-set-point event)))))
)

(defun dialog-string-to-list (string)
  "Convert STRING into a list.  Substrinsg were separated by CR."
  (if (not (stringp string)) (signal 'wrong-type-argument (list 'string-p string)))
  (let ((newlst nil))
    (while string
      (if (string-match "\n" string)
	  (setq newlst (cons (substring string 0 (match-beginning 0))
			     newlst)
		string (substring string (match-end 0)))
	(setq newlst (cons string newlst)
	      string nil)))
    (nreverse newlst)))

(defun dialog-count-lines (start end)
  "Count lines from START to END, accounting for the error."
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (goto-char end)
    (beginning-of-line)
    (setq end (point))
    (if (>= start end)
	1
      (+ (count-lines start end) 1))))
;;
;; Special menu function designed for lists of various things.
;;
;; this quietes the byte-compiler
(eval-when-compile (defun dialog-popup (event title menu) nil))

(if (eval-when-compile dialog-xemacs-p)

(defun dialog-popup (event title menu)
  "Do the work of of creating a popup for XEmacs.
Return the associated value selected by the user."
  ;; This bit of Emacs to XEmacs brilliance was taken from
  ;; custom-widget by Per Abrahamsen <abraham@iesd.auc.dk>
  (let ((val (get-popup-menu-response 
	      (cons title
		    (mapcar
		     (function
		      (lambda (x)
			(vector (car x) (list (car x)) t)))
		     menu)))))
    (setq val (and val
		   (listp (event-object val))
		   (stringp (car-safe (event-object val)))
		   (car (event-object val))))
    (cdr (assoc val menu))))

(defun dialog-popup (event title menu)
  "Do the work of of creating a popup, and return the associated
value."
  (x-popup-menu event (cons title (list (cons "" menu)))))

)

(defun dialog-list-2-menu (event title list &optional max)
  "Take a list and turn it into a pop-up menu.
It returns an index into said list.  The list may have anything in it,
and they need not be of the same type."

  (let ((menu))
    (setq menu
	  (let ((tail list)
		(head nil)
		(i 1))
	    (progn
	      (while (and tail (or (not max) (<= i max)))
		(setq head (cons
			    (cons
			     (format "%s" 
					; go to smallest element
				     (let ((elt (car tail)))
				       (while (listp elt)
					 (setq elt (car elt)))
				       elt))
			     i)
			    head))
		(setq i (1+ i))
		(setq tail (cdr tail)))
	      (reverse head))))
    (let ((n (dialog-popup event title menu)))
      (if (integerp n)
	  (1- n)			;the nth starts at 0, we must start
					;at 1, or the first elt returns nil
	nil))))


(defun goto-xy (x y)
  "Move cursor to position X Y in buffer, and add spaces and CRs if needed."
  (if (eq major-mode 'dialog-mode)
      (let ((indent-tabs-mode nil)
	    (num (goto-line y)))
	(if (and (= 0 num) (/= 0 (current-column))) (newline 1))
	(if (eobp) (newline num))
	;; Now, a quicky column moveto/forceto method.
	(if (/= (move-to-column x) x)
	    (let ((pnt (point)) (end nil))
	      (indent-to x)
	      (setq end (point))
	      (if (and (/= pnt end) (fboundp 'put-text-property))
		  (progn
		    (put-text-property pnt end 'face nil)
		    (put-text-property pnt end 'mouse-face nil))))))))
  
(defun insert-overwrite-face (string face &optional focus-face object)
  "Insert STRING into buffer at point, and cover it with FACE.
If optional FOCUS-FACE, then also put this as the mouse-face.  If
optional OBJECT is included, then put that down as the text property
`widget-object' so that we can do faster lookups while dishing out
keystrokes, etc."
  (if widget-toplevel-shell
      (let* ((pnt (point))
	     (end (+ pnt (length string))))
	(goto-char pnt)
	(insert string)
	(if (eobp) (save-excursion (insert "\n"))) ;always make sure there's a blank line
	(if (> (length string) (- (save-excursion (end-of-line) (point))
				  (point)))
	    (delete-region (point) (save-excursion (end-of-line) (point)))
	  (delete-char (length string)))
	;; This will compile out the if statement
	(if (eval-when-compile (fboundp 'put-text-property))
	    (progn
	      (put-text-property pnt end 'face face)
	      (put-text-property pnt end 'mouse-face focus-face)
	      (put-text-property pnt end 'widget-object object)
	      )))))

(defun dialog-widget-tree-primitive ()
  "Displays the current dialog box's widget tree in another buffer"
  (interactive)
  (if (not widget-toplevel-shell) (error "Can't generate widget tree from this buffer"))
  (let ((mytls widget-toplevel-shell))
    (display-buffer (get-buffer-create "*WIDGET BROWSE*") t)
    (save-excursion
      (set-buffer (get-buffer "*WIDGET BROWSE*"))
      (erase-buffer)
      (goto-char 0)
      (dialog-browse-tree mytls "" "")
      )))

(defun dialog-browse-tree (this-root prefix ch-prefix)
  "Recursive part of the widget tree browser.
It draws the children of the given class on the screen."
  (if (not (object-p this-root)) (signal 'wrong-type-argument (list 'object-p this-root)))
  (let ((myname (object-name this-root))
	(chl (if (obj-of-class-p this-root widget-group)
		 (get-children this-root) 
	       nil))
	(fprefix (concat ch-prefix "  +--"))
	(mprefix (concat ch-prefix "  |  "))
	(lprefix (concat ch-prefix "     ")))
    (insert prefix myname)
    (if t
	(insert "\n")
      (if chl
	  (if (= (length chl) 1)
	      (insert (format " -- [1 child]\n"))
	    (insert (format " -- [%d children]\n" (length chl))))
	(insert (format " -- [No children]\n"))))
    (while (cdr chl)
      (dialog-browse-tree (car chl) fprefix mprefix)
      (setq chl (cdr chl)))
    (if chl
	(dialog-browse-tree (car chl) fprefix lprefix))
    ))

(defun dialog-test ()
  "Creates a test dialog using as many widget features as currently works."
  (interactive)
  (switch-to-buffer (get-buffer-create "Dialog Test"))
  (toggle-read-only -1)
  (erase-buffer)
  (dialog-mode)
  (let ((mytog (data-object "MyTog" :value t)))

    (dialog-build-group (create-widget "Push Button Frame" widget-frame
				       :frame-label "Push Button Window"
				       ;; :box-sides [ nil t nil t ]
				       :position 'left-bottom)
      (create-widget "Click" widget-push-button
		     :x 1 :y 1 :label-value "Quit"
		     :box-face 'font-lock-comment-face
		     :activate-hook (lambda (obj reason) "Activate Quit Button"
				      (message "Quit!")
				      (dialog-quit)))
      (create-widget "Clack" widget-push-button
		     :x -5 :y t :label-value "Widget\nTree"
		     :box-face 'font-lock-comment-face
		     :activate-hook (lambda (obj reason) "Draw a widget tree"
				      (dialog-widget-tree-primitive)))
      (create-widget "Cluck" widget-push-button
		     :x -5 :y t :label-value "Class\nTree"
		     :box-face 'font-lock-comment-face
		     :activate-hook (lambda (obj reason) "Draw a widget tree"
				      (eieio-browse)))
      (create-widget "Clunk" widget-push-button
		     :x -5 :y t :label-value "About\nDialog Mode"
		     :box-face 'font-lock-comment-face
		     :activate-hook (lambda (obj reason) "Draw a widget tree"
				      (describe-function
				       'dialog-mode))))

    (create-widget "Fred" widget-label :face 'modeline 
		   :x -2 :y t
		   :label-value "This is a label\non several lines\nseparated by \\n\nto make distinctions")

    (dialog-build-group (create-widget "Radio Frame" widget-radio-frame
				       :frame-label "Radio tests"
				       :position 'right-top)
      
      (create-widget "radio 1" widget-radio-button
		     :label-value "First nifty option")
      
      (create-widget "radio 2" widget-radio-button
		     :label-value "Second nifty option")

      (create-widget "radio 3" widget-radio-button
		     :label-value "Third nifty option")

      )

    (dialog-build-group (create-widget "Togg Frame" widget-frame
				       :x -2 :y t
				       :frame-label "Toggle Tests..."
				       :position 'center-top
				       :box-face 'font-lock-reference-face)
      (create-widget "Togg" widget-toggle-button
		     :label-value "Toggle Me"
		     :face 'underline  :ind-face 'highlight
		     :state mytog
		     :activate-hook (lambda (obj reason) "Switcharoo!"
				      (message "Changed value")))
      (create-widget "Forceon" widget-push-button
		     :label-value "Turn On"
		     :activate-hook 
		     (list 'lambda '(obj reason) "Flip Tog"
			   (list 'set-value mytog t)))
      (create-widget "Forceoff" widget-push-button
		     :x -2 :y t :label-value "Turn Off"
		     :face 'underline
		     :activate-hook
		     (list 'lambda '(obj reason) "Flip Tog"
			   (list 'set-value mytog nil))))
      
    (dialog-build-group "Random Widget Things"
      (create-widget "some-stuff" widget-option-button
		     :face 'italic
		     :title "Animal"
		     :option-list '("Moose" "Dog" "Cat" "Mouse" "Monkey" "Penguin")
		     )
      (create-widget "MyText" widget-text-field :x -4 :y t
		     :width 20 :value "My First String")
      (create-widget "MyTextBox" widget-text-box
		     :width 20 :height 3 :value "My first\nno, second\nUm.. third string")
      (create-widget "MyTextGroup" widget-labeled-text
		     :text-length 20 :value "My Composite String"
		     :label "Named String:" :unit "chars")
      (create-widget "MyCombo" widget-option-text
		     :option-list '("one" "two" "three" "four" "five" "six")
		     :text-length 15
		     :value "won"
		     :label "Combo Text:")
      (create-widget "MyScrolledtext" widget-scrolled-text
		     :width 20 :height 5
		     :value "Here are\nSome string\nvalues")


      (create-widget "MyScale" widget-scale)
      (create-widget "MyScroller" widget-scrollbar)
      (create-widget "MyVScale" widget-scale
		     :direction 'vertical
		     :x 50 :y 1)
      (create-widget "MyVScroller" widget-scrollbar
		     :direction 'vertical
		     :x -2 :y 0)
      )
    )
  (dialog-refresh)
  (goto-char (point-min))
  )

;;; end of lisp
(provide 'dialog-mode)
