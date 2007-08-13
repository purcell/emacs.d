;;; widget-d.el - widget class definitions
;;;
;;; Copyright (C) 1995,1996, 1999 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; RCS: $Id: widget-d.el,v 1.14 1999/02/03 19:31:08 zappo Exp $
;;; Keywords: OO widget
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
;;;
;;; This file defines all the classes needed to create and maintain
;;; widgets in an emacs controlled environment using the eieio
;;; package.  Only definitions exist in this file.
;;;

(require 'eieio)

(defvar widget-d-load-hooks nil
  "List of hooks run after this file is loaded.  Permits users to
customize the default widget behavior using `oset-default'")
         
;;; Data object definition
;;;      
;;; A data object, as discussed in the Fresco documentation, is just a
;;; blob where we store stuff.  Widgets store values in these objects,
;;; and follow their interface, so when the data is updated, other
;;; functions (gadgets, widgets, etc) can update themselves to the
;;; changes in the environment.

(defclass data-object ()
  ((value :initarg :value
	  :initform nil
	  :accessor get-value
	  :documentation "Lisp object which represents the data this object maintains."
	  :protection private)
   (reference :initarg :reference
	      :initform nil
	      :documentation "List of objects looking at me.  The method `update-symbol' is called
for each member of `reference' whenever `value' is modified."
	      :protection private)
   )
  "This defines a `data-object' which is used for all widgets
maintaining some value.  For example, a `widget-label' displays a
string or number, and a `widget-scrollbar' edits a number.  These
widgets will use data-object to store their data.")

;;; Widget definitions for the base set of widgets
;;;

(defclass widget-core ()
  ((parent :initarg :parent
	   :initform nil
	   :accessor get-parent
	   :documentation "A widget of type `widget-group' of which this is a child.")
   (watched-symbols :initarg :watched-symbols
		    :initform nil
		    :documentation "List of symbols this widget cares about."
		    :protection private)
   (help-hook :initarg :help-hook
	      :initform nil
	      :documentation "Function to call when help is requested
about this button.  Default value is to display instructions about the
operation of this widget in the minibuffer.  This takes two paramters which
are the widget for which help was requested and the reason, which us
either 'click for a mouse event, or the keypress initiating the call.")

   )
  "Class for core widget.  This is the widget all other widgets are 
based from.")

(defclass widget-gadget-translator (widget-core)
  ((watch :initarg :watch
	  :documentation "A `data-object' to watch.  When it changes, run the translator function.")
   (change :initarg :change
	   :documentation "A `data-object' to change whenever `watch' is modified.")
   (translate-function :initarg :translate-function
		       :initform (lambda-default (watch change) nil)
		       :documentation
		       "Function to call when `watch' changes.  It should modify the `data-object'
`change' from it's value.  It takes two parameters WATCH and CHANGE.")
   )
  "Non-visible class for a gadget translator.  The translator's job is
to convert the `data-object' in `watch' to some new value, and store it
in `change'.  This is useful for translating indices into descriptions
or something like that.")

(defclass widget-visual (widget-core)
  ((x :initarg :x
      :initform nil
      :documentation "The X position in a buffer relative to parent.")
   (y :initarg :y
      :initform nil
      :documentation "The Y position in a buffer relative to parent.")
;   (resizeable :initarg :resizeable
;	       :initform nil
;	       :documentation "(unused) t if this widget has a tendency to resize itself.")
   (nx :initform 0
       :documentation "The normalized X position relative to parent. (After geometry management)")
   (ny :initform 0
       :documentation "The normalized Y position relative to parent. (After geometry management)")
;   (marker :initarg :marker
;	   :initform nil
;	   :protection private
;	   :documentation "(Unused) Marker in the dialog buffer from which all drawing commands are based.")
   (face :initarg :face
	 :initform widget-default-face
	 :protection private
	 :documentation "Face used to draw this widget.")
   (handle-io :initarg :handle-io
	      :initform  nil
	      :documentation "t if this widget accepts keyboard or mouse input.")
   (handle-motion :initform nil
		  :documentation "t if this widget handles it's own motion events,
or 'traditional if it uses traditional motion events.  Traditional
events are not passed to a widget unless the motion moves the cursor
outside of the widget's boundary.")
   (rx :documentation "Real X position in buffer"
       :protection private)
   (ry :documentation "Real Y position in buffer"
       :protection private)
   )
  "Class for visual widget.  This is the widget all visible widgets
are derived from. Its definition includes an X,Y position which
defines it's offset inside the parent, and can include its offset from
other widgets which are children of `parent'.
@xref{(dialog) Geometry Management}") 

(defclass widget-square (widget-visual)
  ((width :initarg :width
	  :documentation "Width in characters")
   (height :initarg :height
	   :documentation "Height in characters")
   (boxed :initarg :boxed
	  :initform nil
	  :documentation "t if a box is to be drawn around this widget")
   (box-face :initarg :box-face
	     :initform widget-box-face
	     :documentation "Face used on the box (if drawn)"
	     :protection private)
   (box-char :initarg :box-char
	     :initform [?+ ?+ ?+ ?+ ?- ?- ?| ?|]
	     :documentation "Character set used the draw the box.  The vector is 
[ upper-right upper-left bottom-right bottom-left horizontal vertical ]"
	     :protection private)
   (box-sides :initarg :box-sides
	      :initform [ t t t t ]
	      :documentation "Vector which represents those sides of the
box which will be drawn, where a t in a position indicates the side is
to be drawn.  The vector is of the form [ left right top bottom ]")
   )
  "This is the definition for visible widgets which have a square
shape.  This provides basic sizing schemes and box drawing utilities
for widgets that are square.")

;;
;; Some group types
;;

(defclass widget-group (widget-square)
  ((child-list :initarg :child-list
	       :initform nil
	       :accessor get-children
	       :documentation "List of children this group needs to manage")
   )
  "Definition for the group widget.  This is an intermediary type
whose job is to provide basic child management for higher level
widgets which contain children such as `widget-toplevel' and
`widget-frame'.  This widget knows how to add new children, and manage
its size based on the positions and sizes of it's children.  It also
knows how to create navigation lists.")

(defclass widget-toplevel (widget-group)
  ((rx :initarg :rx)			;create initargs for real parts
   (ry :initarg :ry)			;for toplevel only
   (buffer :initarg :buffer
	   :initform current-buffer
	   :documentation "The buffer this dialog resides in.")
   (logical-child-list 
    :initform nil
    :documentation 
    "Contains a list of all the children and grand-children in their
logical order for the purpose of tab-stepping across them"
    :protection private)
   (handle-io :initform t)
   )
  "Definition for a top-level shell. This maintains the interface to
emacs' buffer, and is a parent of all children displayed in the
buffer.  This will be created automatically with a call to
`dialog-mode' when designing a screen.")

(defclass widget-frame (widget-group)
  ((handle-io :initform t)
   (boxed :initform t)
   (frame-label :initarg :frame-label
		:initform t
		:documentation 
		"Label to place on the edge of our frame.  An initial value of t means
to use the object name.  An initial value of nil means no title.  If
this is initialized with a string, then that string is used as the
label's string.  The created widget will be a `widget-label'.  If this
is a widget, then that widget is positioned on the edge of the
screen."
		:protection private)
   (position :initarg :position
	     :initform top-left
	     :documentation "Where the `frame-label' will reside.  Valid values are symbols
consisting of substrings of left, right, center, top, and bottom."
	     :protection private)
   )
  "Definition for a frame, which can contain several children grouped
in a box with a `widget-label' on one edge (covering a part of the box).")

(defclass widget-radio-frame (widget-frame)
  ((state :initarg :state
	  :initform 0
	  :documentation "Current index of the selected radio button")
   )
  "Special frame class which behaves as a radio box.  Designed to only
contain widgets of type `widget-radio-button'.")

(defclass widget-labeled-text (widget-group)
  ((handle-io :initform t)
   (label :initarg :label
	  :initform nil
	  :documentation "Text object displayed with a `widget-label' before a `widget-text-field'.")
   (unit :initarg :unit
	 :initform nil
	 :documentation "Text object displayed with a `widget-label' after the `widget-text-field'
which represents some sort of typing which would be useful to know.")
   (value :initarg :value
	  :initform nil
	  :documentation "The `data-object' we are going to edit with the text widget")
   (text-length :initarg :text-length
		:initform 20
		:documentation "The width passed to the `widget-text-field'")
   )
  "Special group widget which makes creating text fields with labels next to
them more convenient.")

(defclass widget-option-text (widget-group)
  ((handle-io :initform t)
   (value :initarg :value
	  :initform nil
	  :documentation "Text object displayed with a `widget-option-button' just to the right
and a label to the left.  This contains the value used by the option button and
the text widget so they can communicate.")
   (label :initarg :label
	  :initform nil
	  :documentation "Text object displayed with a `widget-label' before a `widget-text-field'.")
   (text-length :initarg :text-length
		:initform 20
		:documentation "The width passed to the `widget-text-field'")
   (option-list :initarg :option-list
		:initform nil
		:documentation "List of strings which are the options to appear in the option list.")
   )
  "Specialized text widget which will have an optional `widget-label' followed
by a `widget-text-field' which will be followed by a `widget-option-button'.
The menu button will appear as a down-arrow.  Items selected from the menu
will then appear in the text field.")

(defclass widget-scrolled-text (widget-group)
  ((handle-io :initform t)
   (boxed :initform t)
   (value :initarg :value
	  :initform nil
	  :documentation "The `data-object' we are going to edit with the text widget")
   (state :initarg :state
	  :initform 0
	  :documentation "Current value of the built-in scrollbar")
   (maximum :initarg :maximum
	    :initform 1
	    :documentation "Largest allowed value for the built-in scrollbar")
   (scrollbar :initform nil
	      :documentation "Holder for scrollbar so we don't make too many of them"
	      :protection private)
   )
  "Specialized composite widget which will build a `widget-text-block'
of the same dimensions given for our width/height.  A scrollbar will be
created just off the edge of our box and it's `maximum' and `minimum' will
be controlled by the text widget (as it's text gets larger/smaller), and
the scrollbar's value will alter the text widget's positioning of text.")

;;
;; The important label type
;;
(defclass widget-label (widget-square)
  ((label-value :initarg :label-value
		:initform nil
		:documentation "The `data-object' to display on ourselves")
   (label-list :initarg nil
	       :initform nil
	       :documentation "The `label-value' is transformed into this list, which is broken
into substrings around carriage returns."
	       :protection private)
   (justification :initarg :justification
		  :initform center
		  :documentation "how to justify the text.  Valid values are 'left, 'center, 'right")
   (focus-face :initarg :focus-face
	       :initform nil
	       :documentation "Face used when mouse passes over `label-value'"
	       :protection private)
   (leftmargin :initarg :leftmargin
	       :initform 0
	       :documentation "Size of left space to format around")
   (rightmargin :initarg :rightmargin
		:initform 0
		:documentation "Size of right space to format around")
   (topmargin :initarg :topmargin
	      :initform 0
	      :documentation "Size of space above this label to format around")
   (bottommargin :initarg :bottommargin
		 :initform 0
		 :documentation "Size of space below this label to format around.")
   )
  "Class for displaying labels.  The value of the label is determined
by the `data-object' stored in `label-value' which can be initialized
with a string, number, or other lisp object.  Supports strings with
carriage returns in them.")

;;
;; Button types
;;
(defclass widget-button (widget-label)
  ((arm-face :initarg :arm-face
	     :initform widget-arm-face
	     :documentation "Face used when this button has been pushed."
	     :protection private)
   (focus-face :initform widget-focus-face
	       :protection private)
   (activate-hook :initarg :activate-hook
		  :initform nil
		  :documentation "Function to call when a user clicks this button.
It must take two paramters.  The object representing the object being
clicked, and the reason it was clicked.  This usually has the value
'click, or the keyboard event that caused a press.")
   (handle-io :initarg :handle-io
	      :initform t)
   )
  "Class for a button widget.  This is the starting point for all
interactive buttons.  This button will be CLICKED on, or will have RET
or SPC pressed while selected, and it will then call `activate-hook'.
If a push button is desired, it is better to use a widget of type
`widget-push-button' instead as it has a better visual effects.")

(defclass widget-push-button (widget-button)
  ((boxed :initform t)
   (box-char :initform [?  ?  ?  ?  ?  ?  ?< ?> ]
	     :protection private)
   (box-sides :initform [ t t nil nil ])
   (box-face :initform widget-indicator-face
	     :protection private)
   ;; Add a little bit of margin
   (leftmargin :initform 1)
   (rightmargin :initform 1)
   )
  "Class for a push button.  This button behaves as a `widget-button'
but with a different visual effect.  This is the preferred widget to
use as the `widget-button' is used as a starting point for all button
types.")

(defclass widget-arrow-button (widget-button)
  ((activate-hook :initform (lambda-default (obj reason)
			       "Arrow button Activate-Hook"
			       (let ((state (oref obj state)))
				 (set-value state (+ (get-value state)
						     (oref obj adjustment))))))
   (state :initarg :state
	  :initarg nil
	  :documentation "The value which will be adjusted when we are activated")
   (direction :initarg :direction
	      :initarg 'up
	      :documentation "Direction this arrow button points.  Valid values are 'up,
'down, 'left, and 'right.  The values 'up and 'left will default `adjustment'
to -1, and 'right and 'down will set it to 1.  This field will also
automatically set the `label-value' slot if it is not specified.")
   (adjustment :initarg :adjustment
	       :initarg nil
	       :documentation "How much to adjust `state' by when activated.  If it is not specified
at creation time, it's value will be generated from the value of the 
`direction' slot.")
   )
  "An arrow button is a specialized button used to increment or decrement a
state variable.  Arrow buttons are usually used to adjust `widget-scale'
values.")

(defclass widget-option-button (widget-button)
  ((option-indicator :initarg :option-indicator
		     :initform "<=>"
		     :documentation "String printed to the left of the label in `left-margin' used to show this is an option button.")
   (option-list :initarg :option-list
		:initform nil
		:documentation "List of strings which are the options to appear in the pull down menu.")
   (option-obarray :initform nil
		   :protection private
		   :documentation "Obarray used for command line reading of symbols")
   (title :initarg :title
	  :initform "Option"
	  :documentation "String that appears as the completion-prompt, or
as the title to a popup menu.  When used in a prompt, the form is
\"Select\" title \": \".")
   (ind-face :initarg :ind-face
	     :initform widget-indicator-face
	     :documentation "Face used on the `option-indicator' string"
	     :protection private)
   (justification :initarg :justification
		  :initform left)
   (dynamic-label :initarg :dynamic-label
		  :initform t
		  :documentation "t means that the label of this button will always show the selected
element from option-list.  nil will leave the label string alone."
		  :protection private)
   (boxed :initform nil)
   (state :initarg :state
	  :initform 0
	  :documentation "`data-object' used as a numerical index into
list of strings representing the current value.")
   )
  "Class for option button widget.  This button will provide a menu
when clicked on.  The menu will consist of those items in
`option-list', and the chosen item will appear in the button's text.")

(defclass widget-toggle-button (widget-button)
  ((boxed :initform nil)
   (state :initarg :state
	  :initform nil
	  :documentation "Current value of the toggle button")
   (ind-face :initarg :ind-face
	     :initform widget-indicator-face
	     :documentation "Face used on toggle indicator"
	     :protection private)
   (showvec :initarg :showvec
	    :initform [ "[ ]" "[X]" ]
	    :documentation "Vector [ false true ] of strings used to show the state")
   )
  "Class for toggle button widget.  This button will be CLICKED, and
when successful clicks occur, a boolean value will be turned ON or
OFF, and a visible piece will be modified based on `showvec'.")

(defclass widget-radio-button (widget-toggle-button)
  ((radio-index :initarg :radioindex
		:initform 0
		:documentation "Index indexing the parent's state, which then lets us know if we
are toggled on or off.  ie, if the parent's state is 1, and our index
is 0, then the state of this button will become nil.  This value does
not change during use.")
   (parent-state :initform nil
		 :documentation "Data object pointing the parent's state"
		 :protection private)
   (showvec :initform [ "< >" "<O>" ])	;change type of indicator
   )
  "Subclass of `widget-toggle-button' which knows how to talk with
several other instantiations of itself in order to radio between different
values.")

;;
;; Scrollbar types
;;
(defclass widget-scale (widget-group)
  ((handle-io :initform t)
   (focus-face :initarg :focus-face
	       :initform widget-focus-face
	       :documentation "Face used on thumb and step buttons when the mouse is over them."
	       :protection private)
   (state :initarg :state
	  :initform 0
	  :documentation "Current value of this scale")
   (minimum :initarg :minimum
	    :initform 0
	    :documentation "Smallest allowed value")
   (maximum :initarg :maximum
	    :initform 10
	    :documentation "Largest allowed value")
   (direction :initarg :direction
	      :initform 'horizontal
	      :documentation "Direction to draw the scale")
   (end-buttons :initarg :end-buttons
		:initform nil
		:documentation "t means to create two buttons to inc/dec the scale value")
   (trough-face :initarg :trough-face
		:initform nil
		:documentation "Face used when rendering the trough of a scale widget")
   (trough-chars :initarg :trough-chars
		 :initform [ ?- ?| ]
		 :documentation "Characters used when drawing the trough, the vector is of the
form [ horizontal-char vertical-char ]")
   (thumb :initarg :thumb
	   :initform "#"
	   :documentation "Character used to draw the value thumb button indicator")
   )
  "Class of scale.  A scale is merely a thumb marker displaying the current
value of some number graphically across some random number of text 
characters.")

(defclass widget-scrollbar (widget-scale)
  ((end-buttons :initarg :end-buttons
		:initform t)
   (range :initarg :range
	  :initform 10
	  :documentation "Range of currently viewable area (Not used)"))
  "Class for a scrollbar.  A scrollbar also will have a visual range
where the thumbtack changes size based on RANGE.")
   

;;
;; Text types
;;
(defclass widget-text-field (widget-square)
  ((handle-io :initarg :handle-io
	      :initform t)
   (handle-motion :initform 'traditional)
   (height :initform 1)
   (face :initarg :face
	 :initform widget-text-face
	 :protection private)
   (spface :initarg :spface
	   :initform widget-text-button-face
	   :documentation "Face used on text buttons which appear to the
left and right of the editable text.  They indicate unseen text to the
left or right of the field."
	   :protection private)
   (focus-face :initarg :focus-face
	       :initform widget-text-focus-face
	       :protection private)
   (keymap :initarg :keymap
	   :initform nil
	   :documentation "Keymap used to interpret text.  By default, the
global map is used when this value is nil.  Otherwise, additional
mode-specific keymaps could be substituted to lend additional
behaviors.")
   (display-column :initarg :display-column
		   :initform 0
		   :documentation "Current horizontal position in a text buffer where the display starts")
   (display-row :initarg :display-row
		:initform 0
		:documentation "Current vertical position in a text buffer where the display starts")
   (display-num-rows :initarg :display-num-rows
		     :initform nil
		     :documentation "The number of rows of text displayed in this text widget.  This is
different from the number of rows displayed as some are clipped.")
; This isn't used, but may be useful in the future.
;   (keycache :initform nil
;	     :documentation "Collecting keypresses for multi keystroke keys.")
   (value :initarg :value
	  :initform nil
	  :documentation "A `data-object' representing the string we are editing.")
   )
  "Class for a text field widget.  This will accept user-typed text
which is no more than 1 line high.  Extra text will not be printed,
but characters on either side of the field will display `<' or `>' to
indicate that there is more to see outside of the visible part.")

(defclass widget-text-box (widget-text-field)
  ((height :initform nil)
   (boxed :initform t)
   (face :initform nil
	 :protection private)
   (focus-face :initform nil
	       :protection private))
  "A text box is a multi-line `widget-text-field'.  It specifies differing
features needed to make a multi-line text box look better.")

(run-hooks 'widget-d-load-hooks)

;;; end of lisp
(provide 'widget-d)
