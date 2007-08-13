;;; cogre.el --- COnnected GRaph Editor for Emacs

;;; Copyright (C) 2001, 2002, 2003, 2005, 2007 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: graph, oop, extensions, outlines
;; X-RCS: $Id: cogre.el,v 1.20 2007/04/15 00:52:11 zappo Exp $

(defvar cogre-version "0.5"
  "Current version of Cogre.")

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
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
;; Many types of code can be displayed as a series of connected
;; graphs, such as UML class or sequence diagrams.  COGRE attempts to
;; allow Emacs to display such graphs with data generated from
;; source code.
;;

(require 'cogre-load)
(require 'eieio)
(require 'eieio-opt)
(require 'eieio-base)
(require 'semantic)
(eval-when-compile
  (require 'picture-hack))

;;; Code:

;;; Display Faces
(defgroup cogre nil
  "COnnected GRaph Editor."
  :group 'tools)

(defcustom cogre-horizontal-margins 10
  "*Horizontal margins between nodes when they are being layed out."
  :group 'cogre
  :type 'number)

(defcustom cogre-vertical-margins 7
  "*Horizontal margins between nodes when they are being layed out."
  :group 'cogre
  :type 'number)

;;; Classes
(defclass cogre-graph (eieio-persistent)
  ((extension :initform ".cgr") ;; Override the default
   (name :initarg :name
	 :initform "NewGraph"
	 :type string
	 :custom string
	 :documentation
	 "The name of this graph.
The save file name is based on this name.")
   (buffer :initarg :buffer
	   :initform nil
	   :type (or null buffer)
	   :documentation
	   "When this graph is active, this is the buffer the graph is
displayed in.")
   (elements :initarg :elements
	     :initform nil
	     :type list
	     :documentation
	     "The list of elements in this graph.")
   )
  "A Connected Graph.
a connected graph contains a series of nodes and links which are
rendered in a buffer, or serialized to disk.")

(defclass cogre-graph-element (eieio-named)
  ((dirty :initform t
	  :documentation
	  "Non-nil if this graph element is dirty.
Elements are made dirty when they are erased from the screen.
Elements must be erased before any graphical fields are changed.")
   (name-default :initform "Name"
		 :type string
		 :custom string
		 :allocation :class
		 :documentation
     "The object-name of this node.
Node object-names must be unique within the current graph so that save
references in links can be restored.")
   (menu :initform nil
	 :type list
	 :allocation :class
	 :documentation
	 "List of menu items in Easymenu format of changeable things.
Any given element may have several entries of details which are
modifiable.
Examples could be Add/Removing/Renaming slots, or changing linkages."
	 )
   )
  "A Graph Element.
Graph elements are anything that is drawn into a `cogre-graph'.
Graph elements have a method for marking themselves dirty."
  :abstract t)

(defclass cogre-node (cogre-graph-element)
  ((position :initarg :position
	     :initform [ 0 0 ]
	     :type vector
	     :custom (vector integer integer)
	     :documentation
	     "The X,Y [COL ROW] position as a vector for this node.
The Width/Height if this node is determined by RECTANGLE, which is
a list of strings representing the body of the node."
	     )
   (blank-lines-top :allocation :class
		    :initform 1
		    :documentation
		    "Number of blank lines above the object-name.")
   (blank-lines-bottom :allocation :class
		       :initform 1
		       :documentation
		       "Number of blank lines below the last line of text.")
   (alignment :initform nil
	      :type symbol
	      :allocation :class
	      :documentation
	      "Alignment of text when displayed in the box.")
   (rectangle :initform nil
	      :type list
	      :documentation
	      "A List of strings representing an Emacs rectangle.
This rectangle is used for inserting and moving the block of
characters that represent this node in a buffer.
The rectangle is NOT SAVED.
Other fields in the node are used to build a new RECTANGLE of strings
at load time.")
   )
  "Connected Graph node.
Nodes are regions with a fill color, and some amount of text representing
a status, or values."
  )

(defclass cogre-link (cogre-graph-element)
  ((start :initarg :start
	  :initform nil
	  :type (or null string cogre-node)
	  :documentation "The starting node.
As a string, the object-name of the node we start on.
As an object, the node we start on.")
   (end :initarg :end
	:initform nil
	:type (or null string cogre-node)
	:documentation "The ending node.
As a string, the object-name of the node we end on.
As an object, the node we end on.")
   (start-glyph :initform [ nil nil nil nil ]
		:allocation :class
		:type vector
		:documentation "The starting glyph.
A Glyph can be NULL, meaning nothing, or a vector.
A Vector must be 4 elements long.  This represents glyphs on
the [ TOP BOTTOM LEFT RIGHT ] of the attached node.
Each element of the vector must be a list representing a rectangle.")
   (end-glyph :initform [ nil nil nil nil ]
	      :allocation :class
	      :type vector
	      :documentation "The ending glyph.
See slot `start-glyph'")
   (horizontal-preference-ratio
    :initform .5
    :allocation :class
    :documentation
    "When choosing a link's direction, a weight applied to horizontal.
Since characters are not square, this ratio attempts to handle the visible
space the link spans, not the number of characters in the coordinate
system being used.
Also, some links may want to be vertical or horizontal as often as
possible, thus values of 0 or 10 are also fine to advance a
preference."  )
   (stop-position :initform nil
		  :documentation
		  "After drawing this link, store a place for a tab stop.")
   (layout-direction
    :initform 'any
    :documentation
    "When using the layout engine, the preferred direction this link points.
This can have a value of 'up, 'down, 'left, 'right, 'horizontal,
'vertical, or 'any.")
   )
  "Connected Graph link.
Links are lines drawn between two nodes, or possibly loose in space
as an intermediate step.  Some links have text describing what they
do, and most links have special markers on one end or another, such as
arrows or circles.")

;;; Connecte Graph variables
;;
(defvar cogre-loading-from-file nil
  "Flag indicating that we are loading a graph from a file.")

(defcustom cogre-mode-hooks nil
  "Hooks run in `cogre-mode'."
  :group 'cogre
  :type 'hook)

(defvar cogre-graph nil
  "The current connected graph.")
(make-variable-buffer-local 'cogre-graph)

;;; Buffer initialization
;;
;;;###autoload
(defun cogre (name &optional graph-class)
  "Create a new graph with the Connected Graph Editor.
The new graph will be given NAME.  See `cogre-mode' for details.
Optional argument GRAPH-CLASS indicates the type of graph to create."
  (interactive "sGraph Name: ")
  (let ((newgraph (if graph-class
		      (funcall graph-class name :name name)
		    (cogre-graph name :name name))))
    (switch-to-buffer (get-buffer-create (concat "*Graph " name "*")))
    (setq cogre-graph newgraph)
    ;;(toggle-read-only 1)
    (require 'cogre-mode)
    (cogre-mode)
    ))

;;; Default management
;;
;; Defaults provide a way of quickly creating a bunch of the same type
;; of node/link, or whatever.  By using these functions in `interactive'
;; commands, a set of defaults can be specified which are used
;; continuously.
(defvar cogre-node-history nil
  "The history for reading in node class names.")

(defvar cogre-default-node nil
  "The last node type queried.
Used as the default node type when a user wants a node, and no request
to change it has been made.")

(defun cogre-default-node (&optional node prefix)
  "Return the default node type.
If run interactively, query for a new node to make the default.
If called non-interactivly there is no default, query for one.
If NODE is supplied, use that.
If there is a PREFIX argument, then force a query for one."
  (interactive (list (eieio-read-subclass "Node Type: "
					  cogre-node
					  'cogre-node-history
					  t)
		     current-prefix-arg))
  ;; Save whatever is being set.
  (if node (setq cogre-default-node node))
  ;; If we are not interactive, then check the prefix.
  (if (or prefix (not cogre-default-node))
      (setq cogre-default-node (eieio-read-subclass "Node Type: "
				      cogre-node
				      'cogre-node-history
				      t)))
  ;; Return the cached node.
  cogre-default-node
  )

(defvar cogre-link-history nil
  "The history for reading in link class names.")

(defvar cogre-default-link nil
  "The last link type queried.
Used as the default link type when a user wants a link, and no request
to change it has been made.")

(defun cogre-default-link (&optional link prefix)
  "Return the default link type.
If run interactively, query for a new link to make the default.
If called non-interactivly there is no default, query for one.
If LINK is supplied, use that.
If there is a PREFIX argument, then force a query for one."
  (interactive (list (eieio-read-subclass "Link Type: "
					  cogre-link
					  'cogre-link-history
					  t)
		     current-prefix-arg))
  ;; Save whatever is being set.
  (if link (setq cogre-default-link link))
  ;; If we are not interactive, then check the prefix.
  (if (or prefix (not cogre-default-link))
      (setq cogre-default-link (eieio-read-subclass "Link Type: "
				      cogre-link
				      'cogre-link-history
				      t)))
  ;; Return the cached link.
  cogre-default-link
  )

;;; Commands for Graph Mode
;;
(defun cogre-refresh ()
  "Refresh the current display completely."
  (interactive)
  (cogre-render-buffer cogre-graph t))

;;; Utilities
;;
(defun cogre-map-elements (function)
  "Map FUNCTION onto all current graph elements."
  (cogre-map-graph-elements cogre-graph function))

(defun cogre-map-graph-elements (graph function)
  "For elements of GRAPH, call FUNCTION.
Function must take one argument, which is the element.
This function can also be a method.
Returns a list of return values from each call of function."
  (mapcar function (oref graph elements)))

;;; State Management
;;
(defvar cogre-custom-originating-graph-buffer nil
  "The graph from which a custom buffer originated.")
(make-variable-buffer-local 'cogre-custom-originating-graph-buffer)

(defmethod cogre-activate ((element cogre-graph-element))
  "Activate ELEMENT.
This could be as simple as displaying the current state,
customizing the object, or performing some complex task."
  (let ((b (current-buffer)))
    (require 'eieio-custom)
    (customize-object element)
    (setq cogre-custom-originating-graph-buffer b))
  )

(defmethod eieio-done-customizing ((element cogre-graph-element))
  "Finish customizing a graph element."
  (cogre-set-dirty element t)
  (save-excursion
    (set-buffer cogre-custom-originating-graph-buffer)
    (cogre-render-buffer cogre-graph))
  )

(defmethod cogre-add-element ((graph cogre-graph) elt)
  "Add to GRAPH a new element ELT."
  (object-add-to-list graph 'elements elt t))

(defmethod cogre-delete-element ((graph cogre-graph) elt)
  "Delete from GRAPH the element ELT."
  (object-remove-from-list graph 'elements elt))

(defmethod cogre-unique-name ((graph cogre-graph) name)
  "Within GRAPH, make NAME unique."
  (let ((newname name)
	(obj (object-assoc name :object-name (oref graph elements)))
	(inc 1))
    (while obj
      (setq newname (concat name (int-to-string inc)))
      (setq inc (1+ inc))
      (setq obj (object-assoc newname :object-name (oref graph elements))))
    newname))

(defmethod cogre-set-dirty ((element cogre-graph-element) dirty-state)
  "Set the dirty state for ELEMENT to DIRTY-STATE."
  (oset element dirty dirty-state))

(defmethod cogre-set-dirty ((node cogre-node) dirty-state)
  "Set the dirty state for NODE to DIRTY-STATE."
  (if dirty-state (oset node rectangle nil))
  (call-next-method))

(defmethod initialize-instance ((elt cogre-graph-element) fields)
  "Initialize ELT's name before the main FIELDS are initialized."
  (unless cogre-loading-from-file
    (let ((n (oref elt name-default)))
      (object-set-name-string elt n)))
  (call-next-method))

(defmethod initialize-instance :AFTER ((elt cogre-graph-element) fields)
  "When creating a new element, add it to the current graph.
Argument ELT is the element being created.
Argument FIELDS are ignored."
  (unless cogre-loading-from-file
    (let ((n (oref elt object-name)))
      ;; make sure our name is unique.
      (oset elt object-name (cogre-unique-name cogre-graph n)))
    (cogre-add-element cogre-graph elt)))

;;; Buffer Rendering
;;
(defmethod cogre-render-buffer ((graph cogre-graph) &optional erase)
  "Render the current graph GRAPH.
If optional argument ERASE is non-nil, then erase the buffer,
and render everything.  If ERASE is nil, then only redraw items
with dirty flags set."
  (let ((inhibit-read-only t)
	(x (current-column))
	(y (1- (picture-current-line)))
	(inhibit-point-motion-hooks t))
    (save-excursion
      (if erase
	  (progn
	    (erase-buffer)
	    (cogre-map-elements (lambda (e) (cogre-set-dirty e t)))))
      (cogre-map-elements 'cogre-render))
    (picture-goto-coordinate x y)))

(defmethod cogre-render ((element cogre-graph-element))
  "Render ELEMENT.
By default, an ELEMENT has nothing to see, but assume we
are called from `call-next-method', so reset our dirty flag."
  (cogre-set-dirty element nil))

(defmethod cogre-erase ((element cogre-graph-element))
  "Erase ELEMENT.
By default, an ELEMENT has nothing to erase, but assume we
are called from `call-next-method', so set our dirty flag."
  (cogre-set-dirty element t))

(defmethod cogre-element-pre-serialize ((elt cogre-graph-element))
  "Prepare the current node to be serialized.
Remove all pointers to objects (such as links), and replace
with something reversable."
  )

(defmethod cogre-element-post-serialize ((elt cogre-graph-element))
  "Restore object pointers after being loaded from disk.
Also called after a graph was saved to restore all objects.
Reverses `cogre-graph-pre-serialize'."
  )

(defmethod cogre-entered ((element cogre-graph-element) start end)
  "Method called when the cursor enters ELEMENT.
START and END cover the region with the property."
  (message "%s" (object-name element)))

(defmethod cogre-left ((element cogre-graph-element) start end)
  "Method called when the cursor exits ELEMENT.
START and END cover the region with the property."
  nil)

;;; Nodes
(defmethod cogre-erase ((node cogre-node))
  "Erase NODE from the screen."
  (let ((position (oref node position))
	(rectangle (cogre-node-rectangle node))
	(links (cogre-node-links node)))
    (cogre-erase-rectangle (aref position 0) (aref position 1)
			   (length (car rectangle))
			   (length rectangle))
    (mapcar 'cogre-erase links))
  (call-next-method))

(defmethod cogre-node-links ((node cogre-node))
  "Return a list of links which reference NODE."
  (with-slots (elements) cogre-graph
    (let ((links nil))
      (mapcar (lambda (n) (if (and (obj-of-class-p n cogre-link)
				   (or (eq (oref n start) node)
				       (eq (oref n end) node)))
			      (setq links (cons n links))))
	      elements)
      links)))

(defmethod cogre-node-rectangle  ((node cogre-node))
  "Fetch the rectangle representation for NODE."
  (or (oref node rectangle)
      (cogre-node-rebuild node)))

(defmethod cogre-render ((node cogre-node))
  "Render NODE in the current graph."
  (cogre-node-rectangle node)
  (with-slots (position rectangle) node
    (picture-goto-coordinate (aref position 0) (aref position 1))
    (picture-insert-rectangle rectangle nil))
  (call-next-method))

(defmethod cogre-node-rebuild ((node cogre-node))
  "Create a new value for `:rectangle' in NODE.
The `:rectangle' slot is inserted with rectangle commands.
A Rectangle is basically a list of equal length strings.
Those strings must have the proper face values on them.
Always make the width 2 greater than the widest string."
  (let* ((width (+ (cogre-node-widest-string node) 2))
	 (top-lines (oref node blank-lines-top))
	 (bottom-lines (oref node blank-lines-bottom))
	 (title (cogre-node-title node))
	 (slots (cogre-node-slots node))
	 (align (oref node alignment))
	 (first t)
	 (rect nil))
    (while (> top-lines 0)
      (setq rect (cons (cogre-string-with-face
			""
			(if first
			    (progn (setq first nil)
				   'cogre-box-first-face)
			  'cogre-box-face)
			node width align)
		       rect)
	    top-lines (1- top-lines)))
    (setq title (nreverse title))
    (while title
      (let ((face (cond ((and first (null (cdr title)))
			 '(cogre-box-first-face cogre-box-last-face))
			(first
			 'cogre-box-first-face)
			((and (null (cdr title))
			      (not (and (null slots)
					(/= bottom-lines 0))))
			 'cogre-box-last-face)
			(t 'cogre-box-face))))
	(setq rect (cons (cogre-string-with-face
			  (car title) face
			  node width align)
			 rect)
	      title (cdr title))))
    (while slots
      (let ((sl (car slots)))
	;; If a subnode has nil here, make sure we put in a blank
	;; line placeholder.
	(if (not sl) (setq sl (list "")))
	(while sl
	  (let ((face (cond ((and (= bottom-lines 0)
				  (null (cdr sl)))
			     'cogre-box-last-face)
			    (t 'cogre-box-face))))
	    (setq rect (cons (cogre-string-with-face
			      (car sl) face
			      node width align)
			     rect)
		  sl (cdr sl)))))
      (setq slots (cdr slots)))
    (while (> bottom-lines 0)
      (setq rect (cons (cogre-string-with-face
			""
			(if (= bottom-lines 1)
			    'cogre-box-last-face
			  'cogre-box-face)
			node width align)
		       rect)
	    bottom-lines (1- bottom-lines)))
    (oset node rectangle (nreverse rect))))

(defmethod cogre-move-delta ((node cogre-node) dx dy)
  "Move NODE's position by DX, DY."
  (let ((p (oref node position)))
    (cogre-move node (+ (aref p 0) dx) (+ (aref p 1) dy))))

(defmethod cogre-move ((node cogre-node) x y)
  "Move NODE to position X, Y."
  (if (> 0 x) (setq x 0))
  (if (> 0 y) (setq y 0))
  (oset node position (vector x y))
  )

(defmethod cogre-node-title ((node cogre-node))
  "Return a list of strings representing the title of the NODE.
For example: ( \"Title\" ) or ( \"<Type>\" \"Title\" )"
  (list (oref node object-name)))

(defmethod cogre-node-slots ((node cogre-node))
  "For NODE, return a list of slot lists.
Slots are individual lines of text appearing in the body of a node.
Each list will be prefixed with a line before it."
  nil)

(defmethod cogre-node-widest-string ((node cogre-node))
  "Return the widest string in NODE."
  (let ((namel (length (oref node object-name)))
	(slots (cogre-node-slots node))
	(names nil)
	(ws 0))
    (while slots
      (setq names (car slots))
      (while names
	(if (> (length (car names)) ws)
	    (setq ws (length (car names))))
	(setq names (cdr names)))
      (setq slots (cdr slots)))
    (if (> ws namel) ws namel)))
    

(defun cogre-node-horizontal-distance (node1 node2)
  "Calculate the horizontal distance between NODE1 and NODE2.
This number is positive or negative, depending on the direction
of distance."
  ;; Make sure their rectangle's are up to date.
  (cogre-node-rebuild node1)
  (cogre-node-rebuild node2)
  ;; Get all the details
  (let* ((p1 (oref node1 position))	;position vector
	 (p2 (oref node2 position))
	 (x1 (aref p1 0))		;X,Y for NODE1
	 (x2 (aref p2 0))		;X,Y for NODE2
	 )
    (if (< x1 x2)
	;; positive distance.
	(- x2 x1 (length (car (cogre-node-rectangle node1))))
      (- x1 x2 (length (car (cogre-node-rectangle node2))))
      )))

(defun cogre-node-vertical-distance (node1 node2)
  "Calculate the vertical distance between NODE1 and NODE2.
This number is positive or negative, depending on the direction
of distance."
  ;; Make sure their rectangle's are up to date.
  (cogre-node-rebuild node1)
  (cogre-node-rebuild node2)
  ;; Get all the details
  (let* ((p1 (oref node1 position))	;position vector
	 (p2 (oref node2 position))
	 (y1 (aref p1 1))		;X,Y for NODE1
	 (y2 (aref p2 1))		;X,Y for NODE2
	 )
    (if (< y1 y2)
	;; positive distance.
	(- y2 y1 (length (cogre-node-rectangle node1)))
      (- y1 y2 (length (cogre-node-rectangle node2)))
      )))

(defun cogre-choose-horizontal-link-anchors (node1 node2)
  "Choose horizontal link anchor points between NODE1 and NODE2.
The data returned is (X1 Y1 X2 Y2)."
  (let* ((p1 (oref node1 position))	;position vector
	 (p2 (oref node2 position))
	 (x1 (aref p1 0))		;X,Y for START
	 (y1 (aref p1 1))
	 (x2 (aref p2 0))		;X,Y for END
	 (y2 (aref p2 1))
	 (r1 (cogre-node-rectangle node1)) ;rectangle text
	 (r2 (cogre-node-rectangle node2))
	 (h1 (length r1))		;Height
	 (h2 (length r2))
	 (w1 (length (car r1)))		;Width
	 (w2 (length (car r2)))
	 )
    (if (< x1 x2)
	(list (+ x1 w1) (+ y1 (/ h1 2)) (1- x2) (+ y2 (/ h2 2)))
      (list (1- x1) (+ y1 (/ h1 2)) (+ x2  w2) (+ y2 (/ h2 2))))
    ))

(defun cogre-choose-vertical-link-anchors (node1 node2)
  "Choose vertical link anchor points between NODE1 and NODE2.
The data returned is (X1 Y1 X2 Y2)."
  (let* ((p1 (oref node1 position))	;position vector
	 (p2 (oref node2 position))
	 (x1 (aref p1 0))		;X,Y for START
	 (y1 (aref p1 1))
	 (x2 (aref p2 0))		;X,Y for END
	 (y2 (aref p2 1))
	 (r1 (cogre-node-rectangle node1)) ;rectangle text
	 (r2 (cogre-node-rectangle node2))
	 (h1 (length r1))		;Height
	 (h2 (length r2))
	 (w1 (length (car r1)))		;Width
	 (w2 (length (car r2)))
	 )
    (if (< y1 y2)
	(list (+ x1 (/ w1 2)) (+ y1 h1) (+ x2 (/ w2 2)) (1- y2))
      (list (+ x1 (/ w1 2)) (1- y1) (+ x2  (/ w2 2)) (+ y2 h2)))
      ))

;;; Links
;;
(defmethod cogre-element-pre-serialize ((link cogre-link))
  "Prepare the current node to be serialized.
Remove all pointers to objects (such as links), and replace
with something reversable."
  (call-next-method)
  ;; Remove the node objects from ourselves, and remove ourselves
  ;; from the nodes we point to.
  (with-slots (start end) link
    (setf start (oref start :object-name))
    (setf end (oref end :object-name))
    )
  )

(defmethod cogre-element-post-serialize ((link cogre-link))
  "Restore object pointers in LINK after being loaded from disk.
Also called after a graph was saved to restore all objects.
Reverses `cogre-graph-pre-serialize'."
  (call-next-method)
  ;; Convert the textual names back to object references from the
  ;; current graphs element list.
  (with-slots (start end) link
    (setf start
	  (object-assoc start :object-name (oref cogre-graph elements)))
    (setf end
	  (object-assoc end :object-name (oref cogre-graph elements)))
    )
  )

(defvar cogre-erase-mode nil
  "Non nil means we are in erase mode while rendering this link.")

(defmethod cogre-erase ((link cogre-link))
  "Erase LINK from the screen."
  (let ((picture-rectangle-ctl ? )
	(picture-rectangle-ctr ? )
	(picture-rectangle-cbl ? )
	(picture-rectangle-cbr ? )
	(picture-rectangle-v ? )
	(picture-rectangle-h ? ))
    ;; Links use picture line drawing teqnique to wander about.
    ;; By setting the picture line characters to spaces, we can
    ;; erase the line with the render command.
    (let ((cogre-erase-mode t))
      (cogre-render link))
    (call-next-method)))

(defmethod cogre-render ((link cogre-link))
  "Render LINK in the current graph."
  (with-slots (start end start-glyph end-glyph) link
    (let* ((hd (cogre-node-horizontal-distance start end))
	   (vd (cogre-node-vertical-distance start end))
	   linkcoords
	   dir
	   )
      ;; Calculate starting points in relation to our attached nodes.
      (if (> (* hd (oref link horizontal-preference-ratio)) vd)
	  ;; In this case, the X delta is larger than the Y delta,
	  ;; so the line is going mostly left/right.
	  (setq linkcoords (cogre-choose-horizontal-link-anchors start end)
		dir 'horizontal)
	(setq linkcoords (cogre-choose-vertical-link-anchors start end)
	      dir 'vertical))
      (oset link stop-position (list (car linkcoords) (car (cdr linkcoords))))
      ;; Now draw a rectiliniar line
      (apply 'picture-draw-rectilinear-line
	     (append linkcoords (list dir 'face nil 'element link)))
      ;; Handle start/end glyps.
      (if (and (not start-glyph) (not end-glyph))
	  ;; We need to do nothing if we have no glyphs.
	  nil
	(let* (startrect endrect x1 y1 x2 y2)
	  ;; Calculate the modificates needed to the end points for
	  ;; creating the textual glyph.
	  (setq x1 (nth 0 linkcoords)
		y1 (nth 1 linkcoords)
		x2 (nth 2 linkcoords)
		y2 (nth 3 linkcoords))
	  (if (eq dir 'horizontal)
	      (progn
		(if (< x1 x2)
		    (setq startrect (aref start-glyph 2)
			  endrect (aref end-glyph 3)
			  x2 (- x2 -1 (length (car endrect))))
		  (setq startrect (aref start-glyph 3)
			endrect (aref end-glyph 2)
			x1 (- x1 -1 (length (car startrect)))))
		(setq y1 (- y1 (/ (length startrect) 2))
		      y2 (- y2 (/ (length endrect) 2))))
	    (if (< y1 y2)
		(setq startrect (aref start-glyph 0)
		      endrect (aref end-glyph 1)
		      y2 (- y2 -1 (length endrect)))
	      (setq startrect (aref start-glyph 1)
		    endrect (aref end-glyph 0)
		    y1 (- y1 -1 (length startrect))))
	    (setq x1 (- x1 (/ (length (car startrect)) 2))
		  x2 (- x2 (/ (length (car endrect)) 2))))
	  ;; Ok, splat the glyph
	  (if cogre-erase-mode
	      (progn
		(cogre-erase-rectangle x1 y1
				       (length (car startrect))
				       (length startrect))
		(cogre-erase-rectangle x2 y2
				       (length (car endrect))
				       (length endrect))
		)
	    (picture-goto-coordinate x1 y1)
	    (picture-insert-rectangle startrect nil)
	    (picture-goto-coordinate x2 y2)
	    (picture-insert-rectangle endrect nil)
	    )
	  ))))
  (call-next-method))

;;; Files
;;
;; Save and restore graphs to disk
(defun cogre-save-graph-as (file)
  "Save the current graph into FILE.
This can change the current file assocaited with the current graph."
  (interactive "fFile: ")
  (oset cogre-graph file file)
  (cogre-save cogre-graph))

(defun cogre-save-graph (file)
  "Save the current graph to FILE."
  (interactive (list
		(eieio-persistent-save-interactive cogre-graph
						   "Save In: "
						   (oref cogre-graph name))))
  (cogre-save cogre-graph))

(defmethod cogre-save ((graph cogre-graph))
  "Save the current graph."
  (cogre-map-elements 'cogre-element-pre-serialize)
  (unwind-protect
      (eieio-persistent-save cogre-graph)
    (cogre-map-elements 'cogre-element-post-serialize))
  )

;;;###autoload
(defun cogre-load-graph (file)
  "Load a graph from FILE into a new graph buffer."
  (interactive "fFile: ")
  (let ((graph nil)
	(cogre-loading-from-file t))
    (setq graph (eieio-persistent-read file))
    (oset graph file file)
    (cogre (oref graph name))
    (setq cogre-graph graph)
    (cogre-map-elements 'cogre-element-post-serialize)
    (cogre-render-buffer graph t)))

;;; Low Level Rendering and status
;;

(defun cogre-string-with-face (string face element &optional length align)
  "Using text STRING, apply FACE to that text.
The string in question belongs to the graph ELEMENT.
If optional argument LENGTH is supplied, pad STRING on the left and
right so that it is centered.  If optional argument ALIGN is non-nil,
the align the string either 'left or 'right.
Return the new string."
  (if length
      (let* ((preprops (copy-sequence (text-properties-at 0 string)))
	     (ws (- length (length string)))
	     (sws (cond ((not align)
			 (make-string (/ ws 2) ? ))
			((eq align 'right)
			 (make-string (1- ws) ? ))
			((eq align 'left)
			 " ")
			(t "")
			))
	     (ews (cond ((not align)
			 (make-string (+ (/ ws 2) (% ws 2)) ? ))
			((eq align 'left)
			 (make-string (1- ws) ? ))
			((eq align 'right)
			 " ")
			(t "")
			))
	     )
	(let ((pm (plist-get preprops 'face)))
	  (when pm
	    ;; We don't want to modify the face on this based
	    ;; on the first character.
	    (setq preprops (delq 'face preprops))
	    (setq preprops (delq pm preprops))))
	(setq string (concat sws string ews))
	(add-text-properties 0 (length string) preprops string)
	))
  ;; Add our faces on.  Preserve previously applied faces.
  (when face
    (alter-text-property 0 (length string) 'face
			 (lambda (current-face)
			   (let ((cf
				  (cond ((facep current-face)
					 (list current-face))
					((listp current-face)
					 current-face)
					(t nil)))
				 (nf
				  (cond ((facep face)
					 (list face))
					((listp face)
					 face)
					(t nil))))
			     (append cf nf)))
			 string))
  ;; Add on other properties.
  (add-text-properties 0 (length string)
		       (list 'rear-nonsticky t
			     'detachable t ;; xemacs
			     'element element
			     ;; 'local-map
			     ;; 'modification-hooks
			     'point-entered
			     (lambda (s e)
			       (let ((inhibit-point-motion-hooks t))
				 (when (cogre-current-element)
				   (cogre-entered (cogre-current-element) s e))))
			     'point-left
			     (lambda (s e)
			       (let* ((inhibit-point-motion-hooks t)
				      (el
				       (save-excursion
					 (goto-char s)
					 (cogre-current-element))))
				 (when el (cogre-left el s e)))))
		       string)
  string)

(defun cogre-erase-rectangle (x y width height)
  "Clear out the rectangle at X Y, with dimensions WIDTH HEIGHT."
  (picture-goto-coordinate x y)
  (clear-rectangle (point)
		   (save-excursion
		     (picture-goto-coordinate (+ x width)
					      (+ y height))
		     (point))
		   t))

(defun cogre-current-element (&optional point)
  "Return the element under POINT."
  (get-text-property (or point (point)) 'element))

(defun cogre-current-line ()
  "Get the current line."
  (cond ((eq (point-min) (point))
	 0)
	(t (1- (count-lines (point-min) (point))))))

(provide 'cogre)

;;; cogre.el ends here
