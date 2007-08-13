;;; dialog-tree - use tree mode to create a tree showing widget management
;;;
;;; Copyright (C) 1996 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; Version: 0.1
;;; RCS: $Id: dialog-tree.el,v 1.1 1996/03/28 03:41:40 zappo Exp $
;;; Keywords: OO widget tree
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
;;; Updates can be found at:
;;;    ftp://ftp.ultranet.com/pub/zappo

;;; Commentary:
;;;   Uses tree-mode to display a tree representing the structure of a
;;; contstructed user interface using eieio-widgets.
;;;
(require 'tree)

(defclass widget-tree-node (tree-node)
  ((widget :initarg :widget
	   :initform nil))
  "Class sub-type for for displaying a TREE using TREE mode for eieio.")

(defmethod edit ((dwn widget-tree-node))
  "Don't really edit, but pull up details about the given widget using
`eieio-describe-class'"
  (eieio-describe-class (oref dwn widget)))

(defmethod select ((dwn widget-tree-node))
  "Display a tiny bit of info about this object which might be useful"
  (message "%s" (object-name dwn))
  )

(defun dialog-new-widget-node (widget)
  "Creates a new widget tree node with the specified WIDGET slot"
  (widget-tree-node (aref widget 2) ; widget's name
		    :name (aref widget 2)
		    :widget widget)
  )

(defun dialog-widget-tree ()
  "Displays a widget tree using the TREE package in another buffer
of the current dialog box's widgets."
  (interactive)
  (if (not widget-toplevel-shell) (error "Can't generate widget tree from this buffer"))
  (let ((tlw widget-toplevel-shell))
    (switch-to-buffer (tree-new-buffer "*WIDGET TREE*"))
    (erase-buffer)
    (let ((np (tree-set-root (dialog-new-widget-node tlw))))
      (dialog-widget-tree-grow np)))
  (tree-refresh-tree))

(defun dialog-widget-tree-grow (node)
  "Adds to NODE all children belonging to the widget specified in it's
widget field"
  (let* ((w (oref node widget))
	 (wc (object-class w))
	 (wk (if (child-of-class-p wc widget-group) (oref w child-list) nil))
	 nn)
    (while wk
      (setq nn (dialog-new-widget-node (car wk)))
      (tree-add-child node nn)
      (dialog-widget-tree-grow nn)
      (setq wk (cdr wk))))
  )

;;; end of lisp
(provide 'dialog-tree)