;;; call-tree.el --- Uses tree mode to display a call tree of the
;;                  give emacs lisp function.
;;
;; Copyright (C) 1996, 1998, 2001 Eric M. Ludlam
;;
;; Author: <zappo@gnu.ai.mit.edu>
;; Version: 0.1
;; RCS: $Id: call-tree.el,v 1.5 2001/04/17 03:17:00 emacsman Exp $
;; Keywords: OO, tree, call-graph
;;                                                                          
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.org

;;; Commentary:
;;   This function allows the user to display a call tree for a
;; given function.  Function symbols are expanded only if they are
;; evaluated Lisp expressions.  Compiled functions and (of course)
;; subroutines are not expanded.  Subroutines are not even listed in
;; the tree as they are assumed to be in there.
;;   This was created in the hopes that it would aid me in debugging
;; things by being able to visualize the flow of control.  As a
;; result, symbols are expanded multiple times, and recursion is
;; removed (and assumed)
;;

(require 'tree)

;;; Code:
(defclass call-tree-node (tree-node)
  ((symbol :initarg :symbol
	   :initform nil)
   )
  "Class used to define a tree node representing a lisp function.
This function is assumed to have been called from it's parent node")

(defmethod edit ((tn call-tree-node))
  "Action to take when middle mouse button is clicked."
  (let* ((sym (oref tn symbol))
	 (sff (locate-library (describe-function-find-file sym)))
	 (sffs (if (string-match "\\.elc$" sff)
		   (substring sff 0 (1- (length sff)))
		 sff)))
    (find-file sffs)
    (goto-char (point-min))
    (re-search-forward (concat "def\\(un\\|macro\\|method\\)\\s-+"
			       (symbol-name sym) "\\s-+"))
  ))

(defmethod select ((tn call-tree-node))
  "Action to take when first mouse is clicked."
  (if (featurep 'eldoc)
      (eldoc-print-fnsym-args (oref tn symbol))
    (message "Clicked on node %s" (object-name tn))
  ))

(defun call-tree-new-node (func)
  "Build a variable `call-tree-node' based on the function FUNC."
  (call-tree-node (symbol-name func)
		  :name (symbol-name func)
		  :symbol func))

;;;###autoload
(defun call-tree (func)
  "Build a call tree to show all functions called by FUNC."
  (interactive "aFunction: ")
  (switch-to-buffer (tree-new-buffer (format "*CALL-TREE-%s*" func)))
  (erase-buffer)
  (let ((np (tree-set-root (call-tree-new-node func))))
    (call-tree-grow np))
  (tree-refresh-tree))

(defun call-tree-grow (func)
  "Decompose the function stored in the object FUNC and create children."
  (let* ((fvv (symbol-function (oref func symbol)))
	 (fv (if (and (listp fvv) (listp (cdr fvv))) (cdr (cdr fvv)) nil))
	 (nnl nil))
    (if (and fv (listp fv))
	(progn
	  ;; elimitate the doc-string
	  (if (stringp (car fv)) (setq fv (cdr fv)))
	  (call-tree-grow-recurse func fv)
	  (setq nnl (oref func children))
	  (while nnl
	    (if (not (call-tree-recursive-p func (oref (car nnl) symbol)))
		(call-tree-grow (car nnl)))
	    (setq nnl (cdr nnl)))))))
    
(defun call-tree-grow-recurse (func forms)
  "Recurse down FUNC's FORMS list adding tree nodes to func the whole way."
  (if (and (symbolp (car forms)) (fboundp (car forms)))
      (if (or (equal (car forms) 'macro))
	  (setq forms nil)
	(if (and (not (call-tree-duplicate func (car forms)))
		 (not (subrp (symbol-function (car forms))))
		 (not (and (symbolp (symbol-function (car forms)))
			   (subrp (symbol-function
				   (symbol-function (car forms)))))))
	    (tree-add-child func (call-tree-new-node (car forms))))
	(cond ((equal (car forms) 'let)
	       (setq forms (cdr (cdr forms))))
	      (t
	       (setq forms (cdr forms))))))
  (while (and forms (listp forms))
    (if (and forms (listp forms) (listp (car forms)))
	(call-tree-grow-recurse func (car forms)))
    (setq forms (cdr forms)))
  )

(defun call-tree-duplicate (func newfunc)
  "Scan siblings in FUNC to see if we already have it listed here.
Argument NEWFUNC is a function I cannot devine at this time."
  (let ((fp (oref func children)))
    (while (and fp (not (eq (oref (car fp) symbol) newfunc)))
      (setq fp (cdr fp)))
    fp))
      

(defun call-tree-recursive-p (func newfunc)
  "Scan parents of FUNC for occurance of NEWFUNC."
  (let ((fp func))
    (while (and fp (not (eq newfunc (oref fp symbol))))
      (setq fp (oref fp parent)))
    fp))

(provide 'call-tree)

;;; call-tree.el ends here
