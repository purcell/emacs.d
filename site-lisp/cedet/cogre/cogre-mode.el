;;; cogre-mode.el --- Graph editing mode

;;; Copyright (C) 2001, 2002, 2003, 2007, 2009 Eric M. Ludlam

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
;; COGRE mode is based on a generic editor that can render arbitrary
;; graphs as specified by the COGRE core classes.
;; This depends on EIEIO for graph management.  COGRE mode depends on
;; `picture-mode' for drawing.
;;
;; Because COGRE graphs are meant to be edited in some fashion, COGRE
;; graphs depend on the custom widget library to provide text
;; controls, or toggle buttons for editing state in a graph.

(require 'picture-hack)
(require 'eieio)
(require 'eieio-opt)
(require 'eieio-base)
(require 'cogre)

;;; Code:
(defface cogre-box-face  '((((class color) (background dark))
			    (:background "gray30" :foreground "white"))
			   (((class color) (background light))
			    (:background "gray" :foreground "black")))
  "Face used for rectangles of boxes displaying data."
  :group 'cogre)

(defface cogre-box-first-face  '((((class color) (background dark))
				  (:background "gray30" :foreground "white" :overline "white"))
				 (((class color) (background light))
				  (:background "gray" :foreground "black" :overline "black")))
  "Face used for the first data item in rectangles of boxes displaying data.
This has the `overline' property set to display borders between sections
within a box."
  :group 'cogre)

(defface cogre-box-last-face  '((((class color) (background dark))
				  (:background "gray30" :foreground "white" :underline "white"))
				 (((class color) (background light))
				  (:background "gray" :foreground "black" :underline "black")))
  "Face used for the first data item in rectangles of boxes displaying data.
This has the `overline' property set to display borders between sections
within a box."
  :group 'cogre)

(defvar cogre-mode-map nil
  "Keymap used for COGRE mode.")

(defun cogre-substitute (oldfun newfun)
  "Substitue a key binding in ghe `cogre-mode-map'.
Argument OLDFUN is removed NEWFUN is substituted in."
  (substitute-key-definition oldfun newfun cogre-mode-map global-map))

(if cogre-mode-map
    nil
  (setq cogre-mode-map (make-keymap))
  (suppress-keymap cogre-mode-map)
  ;; Structure Information
  (define-key cogre-mode-map "\C-m" 'cogre-activate-element)
  ;; Structure changes
  (define-key cogre-mode-map "R" 'cogre-refresh)
  (define-key cogre-mode-map "N" 'cogre-new-node)
  (define-key cogre-mode-map "L" 'cogre-new-link)
  (define-key cogre-mode-map "D" 'cogre-delete)
  ;; Changing and Setting Defaults
  (define-key cogre-mode-map "\C-c\C-n" 'cogre-default-node)
  (define-key cogre-mode-map "\C-c\C-l" 'cogre-default-link)
  ;; Modifications
  (define-key cogre-mode-map "n" 'cogre-set-element-name)
  (define-key cogre-mode-map "l" 'cogre-edit-label)
  ;; Move nodes around
  (define-key cogre-mode-map [(meta left)] 'cogre-move-node-left)
  (define-key cogre-mode-map [(meta right)] 'cogre-move-node-right)
  (define-key cogre-mode-map [(meta down)] 'cogre-move-node-down)
  (define-key cogre-mode-map [(meta up)] 'cogre-move-node-up)
  (define-key cogre-mode-map "\M-b" 'cogre-move-node-left)
  (define-key cogre-mode-map "\M-f" 'cogre-move-node-right)
  (define-key cogre-mode-map "\M-n" 'cogre-move-node-down)
  (define-key cogre-mode-map "\M-p" 'cogre-move-node-up)
  ;; Cursor Movement
  (define-key cogre-mode-map "\C-i" 'cogre-next-node)
  (define-key cogre-mode-map "\M-\C-i" 'cogre-prev-node)
  (cogre-substitute 'forward-char  'picture-forward-column)
  (cogre-substitute 'backward-char 'picture-backward-column)
  (cogre-substitute 'next-line     'picture-move-down)
  (cogre-substitute 'previous-line 'picture-move-up)
  ;; File IO
  (define-key cogre-mode-map "\C-x\C-s" 'cogre-save-graph)

  )

(easy-menu-define
  cogre-mode-menu cogre-mode-map "Connected Graph Menu"
  '("Graph"
    ("Insert" :filter cogre-insert-forms-menu)
    ("Navigate"
     ["Next Element" cogre-next-node t ]
     ["Prev Element" cogre-prev-node t ]
     ["Move Node Up"    cogre-move-node-up    (cogre-node-child-p (cogre-current-element)) ]
     ["Move Node Down"  cogre-move-node-down  (cogre-node-child-p (cogre-current-element)) ]
     ["Move Node Left"  cogre-move-node-left  (cogre-node-child-p (cogre-current-element)) ]
     ["Move Node right" cogre-move-node-right (cogre-node-child-p (cogre-current-element)) ]
     )
    ("Change" :filter cogre-change-forms-menu)
    "--"
    [ "Delete" cogre-delete (cogre-current-element) ]
    [ "Refresh" cogre-refresh t ]
    [ "Save Graph" cogre-save-graph t ]
    [ "Save Graph As" cogre-save-graph-as t ]
    ))

(defmethod cogre-insert-class-list ((graph cogre-graph))
  "Return a list of classes GRAPH will accept."
  (eieio-build-class-alist 'cogre-graph-element))

(defun cogre-insert-forms-menu (menu-def)
  "Create a menu for cogre INSERT item.
Argument MENU-DEF is the easy-menu definition."
  (easy-menu-filter-return
   (easy-menu-create-menu
    "Insert Forms"
    (let ((obj (cogre-current-element))
	  (elements (cogre-insert-class-list cogre-graph))
	  (newmenu nil))
      (while elements
	;; Added (car elements) to the menu.
	(setq newmenu (cons
		       (vector (car (car elements))
			       `(progn
				 (cogre-new-node
				  (point)
				  (intern ,(car (car elements))))
				 (cogre-render-buffer cogre-graph)
				 )
			       t)
		       newmenu))
	(setq elements (cdr elements)))
      (append  (list [ "New Link" cogre-new-link t ]
		     [ "New Node" cogre-new-node t ]
		     )
	       (nreverse newmenu))
      ))))

(defun cogre-change-forms-menu (menu-def)
  "Create a menu for cogre CHANGE item.
Argument MENU-DEF is the easy-menu definition."
  (easy-menu-filter-return
   (easy-menu-create-menu
    "Change Forms"
    (let* ((obj (cogre-current-element))
	   (newmenu (if obj (oref obj menu))))
      (append  '( [ "Name" cogre-set-element-name (cogre-current-element) ]
		  [ "View/Edit" cogre-activate-element (cogre-current-element) ]
		  )
	       (nreverse newmenu))
      ))))

;;; Major Mode
;;
;;;###autoload
(defun cogre-mode ()
  "Connected Graph Editor Mode.
\\{cogre-mode-map}"
  (interactive)
  (setq major-mode 'cogre-mode
	mode-name "Cogre")
  (use-local-map cogre-mode-map)
  (setq truncate-lines t)
  (set (make-local-variable 'transient-mark-mode) nil)
  (run-hooks 'cogre-mode-hook)
  (cogre-render-buffer cogre-graph t)
  )
(put 'cogre-mode 'semantic-match-any-mode t)

;;; Interactive utility functions
;;
(defun cogre-node-at-point-interactive (&optional pos)
  "Return the node under POS.
Throw an error if there is no node."
  (let ((e (cogre-current-element (or pos (point)))))
    (if (or (not e) (not (obj-of-class-p e cogre-node)))
	(error "No graph node under point")
      e)))

(defun cogre-link-at-point-interactive (&optional pos)
  "Return the node under POS.
Throw an error if there is no node."
  (let ((e (cogre-current-element (or pos (point)))))
    (if (or (not e) (not (obj-of-class-p e cogre-link)))
	(error "No graph node under point")
      e)))

(defun cogre-element-at-point-interactive (&optional pos)
  "Return the node under POS.
Throw an error if there is no node."
  (let ((e (cogre-current-element (or pos (point)))))
    (if (not e)
	(error "No graph node under point")
      e)))

;;; Edit/View elements
;;
(defun cogre-activate-element (element)
  "View/Edit the ELEMENT.
The default ELEMENT is the one found under the cursor."
  (interactive (list (cogre-current-element)))
  (if element
      (cogre-activate element)
    (error "The cursor is not on an object")))

;;; Insert/Delete
;;
(defun cogre-new-node (point nodetype)
  "Insert a new node at the current point.
Argument POINT is a position to insert this node to.
NODETYPE is the eieio class name for the node to insert."
  (interactive (list (point) (cogre-default-node nil current-prefix-arg)))
  (save-excursion
    (goto-char point)
    (if (not nodetype) (setq nodetype 'cogre-node))
    (let* ((x (current-column))
	   (y (cogre-current-line))
	   (n (make-instance nodetype (oref nodetype name-default)
			     :position (vector x y)))
	   )
      (if (interactive-p)
	  (cogre-render-buffer cogre-graph))
      ;; Return the node.
      n)))

(defun cogre-new-link (mark point &optional linktype)
  "Insert a new link from the node at MARK to POINT of LINKTYPE.
MARK is the node within which the current mark is set.
POINT is the node the cursor is in.
LINKTYPE is the eieio class name for the link to insert."
  (interactive (list (cogre-node-at-point-interactive (mark))
		     (cogre-node-at-point-interactive (point))
		     (cogre-default-link nil current-prefix-arg)))
  (if (not linktype) (setq linktype cogre-link))
  (prog1
      ;; Return the link.
      (make-instance linktype "Link" :start mark :end point)

    (if (interactive-p)
	(cogre-render-buffer cogre-graph))
    ))

(defvar cogre-delete-dont-ask nil
  "Track if we should ask about deleting an object from the graph.")

(defun cogre-delete (element)
  "Delete the graph ELEMENT under the cursor."
  (interactive (list (cogre-element-at-point-interactive (point))))
  (if (or cogre-delete-dont-ask
	  (y-or-n-p (format "Really delete %s? " (object-name element))))
      (let ((cogre-delete-dont-ask t))
	(if (obj-of-class-p element cogre-node)
	    (let ((el (oref cogre-graph elements))
		  (test nil))
	      (while el
		(setq test (car el)
		      el (cdr el))
		(if (and (obj-of-class-p test cogre-link)
			 (or (eq element (oref test start))
			     (eq element (oref test end))))
		    (cogre-delete test)))))
	(cogre-erase element)
	(cogre-delete-element cogre-graph element))
    ))

;;; Navigation
;;
(defun cogre-next-node (&optional arg)
  "Move forward ARG nodes in the hierarchy.
If ARG is unspecified, assume 1."
  (interactive "p")
  (let ((n (cogre-current-element (point)))
	(e (oref cogre-graph elements))
	(next nil))
    (if (not n)
	;; Not on the node?  Tab around.
	(setq next (car e))
      (let* ((l (length e))
	     (i (- l (length (member n e))))
	     (ni (+ i arg)))
	(if (< ni 0) (setq ni (+ l ni))
	  (if (>= ni l) (setq ni (- ni l))))
	(setq next (nth ni e))))
    (if (obj-of-class-p next cogre-node)
	(let ((p (oref next position)))
	  (picture-goto-coordinate (aref p 0) (aref p 1)))
      ;; Else, we have a link
      (with-slots (stop-position) next
	(apply 'picture-goto-coordinate stop-position)
	))))

(defun cogre-prev-node (&optional arg)
  "Move backward ARG nodes in the hierarchy.
If ARG is unspecified, assume 1."
  (interactive "p")
  (cogre-next-node (- arg)))

;;; Node Modification
;;
(defun cogre-set-element-name (node name)
  "Set the name of the current NODE to NAME."
  (interactive (let ((e (cogre-node-at-point-interactive)))
		 (list e  (read-string "New Name: " ""
				       nil (oref e object-name)))))
  (cogre-erase node)
  (oset node object-name (cogre-unique-name cogre-graph name))
  (if (interactive-p)
      (cogre-render-buffer cogre-graph))
  )

(defun cogre-move-node (x y)
  "Set NODE to postion X, Y."
  (interactive "nX: \nnY: ")
  (let ((inhibit-point-motion-hooks t)
	(e (cogre-current-element (point))))
    (cogre-erase e)
    (cogre-move e x y)
    (picture-goto-coordinate x y))
  (if (interactive-p)
      (cogre-render-buffer cogre-graph)))

(defun cogre-node-position ()
  "Get the position of the node at point."
  (let ((e (cogre-current-element (point)))
	)
    (if e (oref e position)
      (error "No node at point %d" (point)))))

(defun cogre-move-node-left (arg)
  "Move NODE left by ARG columns."
  (interactive "p")
  (let* ((p (cogre-node-position)))
    (cogre-move-node (- (aref p 0) arg) (aref p 1))
    (if (interactive-p)
	(cogre-render-buffer cogre-graph))))

(defun cogre-move-node-right (arg)
  "Move NODE right by ARG columns."
  (interactive "p")
  (let* ((p (cogre-node-position)))
    (cogre-move-node (+ (aref p 0) arg) (aref p 1))
    (if (interactive-p)
	(cogre-render-buffer cogre-graph))))

(defun cogre-move-node-up (arg)
  "Move NODE up by ARG columns."
  (interactive "p")
  (let* ((p (cogre-node-position)))
    (cogre-move-node (aref p 0) (- (aref p 1) arg))
    (if (interactive-p)
	(cogre-render-buffer cogre-graph))))

(defun cogre-move-node-down (arg)
  "Move NODE down by ARG columns."
  (interactive "p")
  (let* ((p (cogre-node-position)))
    (cogre-move-node (aref p 0) (+ (aref p 1) arg))
    (if (interactive-p)
	(cogre-render-buffer cogre-graph))))

(provide 'cogre-mode)

;;; cogre-mode.el ends here
