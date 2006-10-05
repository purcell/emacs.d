;;; tree.el --- Draw a tree with text characters an manipulate it.

;;; Copyright (C) 1996, 1998, 1999, 2001 Eric M. Ludlam
;;
;; Author: <zappo@gnu.org>
;; Version: 0.3
;; RCS: $Id: tree.el,v 1.10 2001/04/24 19:41:50 emacsman Exp $
;; Keywords: OO, tree
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
;;   Many new IDEs provide a tree of some structure-or-other to express the
;; structural organization of data.  This is a feature lacking in Emacs,
;; and this is some code to provide that functionality.
;;
;;  The interactive command `tree-test-it-all' will display a demo tree,
;; and `directory-tree-thing' will display a directory hierarchy from
;; the default directory of the current buffer.
;;
;; You can access general tree commands to play with this mode using
;; the functions:
;; tree-test-it-all  - bogus data that lets you see how tree-mode works
;; eieio-class-tree  - show all currently defined eieio classes and
;;                     their current inheritance organization
;; directory-tree-thing - Draw a tree corresponding to the current
;;                     directory.
;; 
;; In all tree modes:
;; RET & mouse-1
;;       will select a node: usually printing something simple in the
;;       minibuffer.
;; e & mouse-2
;;       will edit a node: bring up a file/directory/editor for that
;;       object.
;; x & mouse-3
;;       expand or deflate a node and it's children.  Shrunken nodes
;;       will have elipses `...' in them to indicate that more data
;;       could be expanded.
;;
;; Creating a new tree mode:
;; 1) Look at an example
;; 2) Create a new tree-node class and add whatever slots you need
;; 3) Create a place to make a tree with `tree-new-buffer'
;; 4) Set root of tree ith `tree-set-root'
;; 5) Add nodes ass you see fit with `tree-add-child'
;; 6) Draw the tree with `tree-refresh-tree'
;; 7) Modify select, edit, and change-scope to do whatever you need.
;;
;; REQUIRES: Emacs 19.30 or better and eieio

;; requires emacs 19 with arbitrary text-properties

(require 'eieio)

;;;
;; Variable definitions

;;; Code:
(defvar tree-map nil "Keymap used in tree mode.")
(if tree-map
    ()
  (setq tree-map (make-sparse-keymap))
  (define-key tree-map "\r" 'tree-select-node)
  (define-key tree-map "\n" 'tree-select-node)
  (define-key tree-map [mouse-1] 'tree-select-node-mouse)
  (define-key tree-map "e" 'tree-edit-node)
  (define-key tree-map [mouse-2] 'tree-edit-node-mouse)
  (define-key tree-map "x" 'tree-expand-or-contract-node)
  (define-key tree-map [mouse-3] 'tree-expand-or-contract-node-mouse)
  )

(defconst tree-ul-char "+")
(defconst tree-ur-char "+")
(defconst tree-ll-char "+")
(defconst tree-lr-char "+")
(defconst tree-vertical-char "|")
(defconst tree-horizontal-char "-")

(defvar tree-root-node nil
  "The root node of a tree in a given tree buffer.")
(make-variable-buffer-local 'tree-root-node)

(defvar tree-buffer-mode 'tree-center-box-1
  "Current mode of a tree buffer.

Valid values are:
'tree-center-box-1 - nodes are boxed w/ 1 line of text in center of region
                     this is default if this value is unknown
'tree-top-box-1    - nodes are boxed w/ 1 line of text @ top of region
'tree-bottom-box-1 - nodes are boxed w/ 1 line of text @ bottom of region")
(make-variable-buffer-local 'tree-buffer-mode)

(defvar tree-face 'bold
  "Face used inside tree-boxes.")

;;;
;; Mode management
;;
(defun tree-mode ()
  "Takes the current buffer, and initialize tree mode upon it."
  (kill-all-local-variables)
  (use-local-map tree-map)
  (setq major-mode 'tree-mode
	mode-name "TREE")
  (setq truncate-lines t)	; no line-wrapping
  (run-hooks 'tree-mode-hook)
;  (setq mode-line-format
;	(list
;	 'mode-line-modified
;	 "--TREE: %15b %[(" 'mode-name 'minor-mode-alist "%n)%]--"
;	 '(line-number-mode "L%l--%-")))
	 
)

(defun tree-new-buffer (name)
  "Create a buffer called NAME to display some tree type things.
Return the newly created buffer."
  (save-excursion
    (set-buffer (get-buffer-create name))
    (tree-mode)
    (current-buffer)))

(defun tree-new-frame (name)
  "Create a new frame NAME and set it up to use graphic characters.
Returns the newly created frame"
  (let ((nf (make-frame (list
			 (cons 'name name)
			 '(height . 30)
			 '(width . 80)
			 ;; I'd like to use a nicer font, but...
			 ;; '(font . "-dec-terminal-*-*-*-*-*-*-*-*-*-*-*-*")
			 '(minibuffer . nil))))
	(nb (tree-new-buffer name))
	(cf (selected-frame)))
    ;; set the buffer we are about to play with here...
    (select-frame nf)
    (switch-to-buffer nb)
    (select-frame cf)
    nf))

;;;
;; display management
;;
(defun tree-refresh-tree ()
  "Refresh the tree structure which is currently active in this buffer."
  (message "Refreshing tree...")
  ;; first-things first.  Cache the height of every node in the tree
  (tree-level-height tree-root-node)
  ;; Now fill the buffer with stuff
  (insert (make-string (oref tree-root-node height) ?\n))
  ;; Now loop over every node building the tree
  (tree-draw-node tree-root-node t t
		  (+ (tree-node-width tree-root-node) 2) 1 0)
  (message "Refreshing tree...Done")
  )

(defun tree-draw-node (node first last width toprow leftmargin)
  "Draw the single NODE and it's children at a correct estimated position.
Really calls a function based upon `tree-buffer-mode'.
FIRST LAST WIDTH TOPROW and LEFTMARGIN are passed along."
  (funcall tree-buffer-mode node first last width toprow leftmargin))

(defun tree-center-box-1 (node first last width toprow leftmargin)
  "As `tree-draw-node' except that we draw 1-line text w/ a box around it.
NODE FIRST LAST WIDTH TOPROW and LEFTMARGIN are passed along."
  (tree-box-1 node first last width toprow leftmargin 'center))

(defun tree-top-box-1 (node first last width toprow leftmargin)
  "As `tree-draw-node' except that we draw 1-line text w/ a box around it.
NODE FIRST LAST WIDTH TOPROW and LEFTMARGIN are passed along."
  (tree-box-1 node first last width toprow leftmargin 'top))

(defun tree-bottom-box-1 (node first last width toprow leftmargin)
  "As `tree-draw-node' except that we draw 1-line text w/ a box around it.
NODE FIRST LAST WIDTH TOPROW and LEFTMARGIN are passed along."
  (tree-box-1 node first last width toprow leftmargin 'bottom))

(defun tree-box-1 (node first last width toprow leftmargin &optional pos)
  "Draw a single NODE and it's children at a correct estimated position.
FIRST and LAST are not used.  WIDTH specifies how much space this row
will take.  TOPROW specifies what row this node starts at, and
LEFTMARGIN specifies how far out on the left this node can draw
itself.
Optional argument POS is a postion."
  (with-slots ((h height) (kids children) (p parent) (nm name) (ex expand))
      node
    (let ((tpos
	    (cond ((eq pos 'center) (- (/ h 2) 1))
		  ((eq pos 'top) 0)
		  ((eq pos 'bottom) (- h 3))
		  (t (error "Illegal call to tree-box-1"))))
	   (l 0))
      ;;(message "Refreshing tree...[%s]" nm)
      ;; draw the box
      (tree-goto-xy leftmargin (+ tpos toprow))
      (insert (if p " " "") tree-ul-char)
      (insert (make-string (- (tree-node-width node) 2)
			   (aref tree-horizontal-char 0)))
      (insert tree-ur-char)
      (tree-goto-xy leftmargin (+ tpos toprow 1))
      (insert (if p "-" "") tree-vertical-char)
      (let ((p1 (point)))
	(insert nm)
	(put-text-property p1 (point) 'face tree-face)
	(put-text-property p1 (point) 'node-object node)
	(put-text-property p1 (point) 'mouse-face 'highlight)
	)
      (insert (if (oref node expand) "" "..."))
      (insert tree-vertical-char)
      (if (and kids ex)
	  (let* ((mn (tree-node-width node))
		 (nd (- width mn (if p 2 1)))
		 (l 0))
	    (while (< l nd)
	      (insert "-")
	      (setq l (1+ l)))))
      (tree-goto-xy leftmargin (+ tpos toprow 2))
      (insert (if p " " "") tree-ll-char)
      (insert (make-string (- (tree-node-width node) 2)
			   (aref tree-horizontal-char 0)))
      (insert tree-lr-char)
      ;; draw all the kids
      (while (and kids ex)
	(tree-draw-node (car kids) (= l 0) (cdr kids)
			(+ (tree-level-width node) 3)
			(+ toprow l) (+ leftmargin width))
	(setq l (+ l (oref (car kids) height)))
	(setq kids (cdr kids)))
      ;; draw the connecting lines
      (setq kids (oref node children))
      (if (and kids (oref node expand))
	  (let ((i 1) (ok nil) (h (1- h)))
	    (while (and (< i h) kids)
	      (tree-goto-xy (+ leftmargin width -1) (+ toprow i))
	      (insert
	       (cond ((looking-at " -") (setq ok t kids (cdr kids)) "+")
		     (ok "|")
		     (t " ")))
	      (delete-char 1)
	      (setq i (1+ i))
	      )))
      (tree-goto-xy (+ leftmargin 2) (+ tpos toprow 1))
      (oset node currentpos (point))
      )))

(defun tree-goto-xy (x y)
  "Move cursor to position X Y in buffer, and add spaces and CRs if needed."
  (let ((indent-tabs-mode nil)
	(num (goto-line y)))
    (if (and (= 0 num) (/= 0 (current-column))) (newline 1))
    (if (eobp) (newline num))
    ;; Now, a quicky column moveto/forceto method.
    (or (= (move-to-column x) x) (indent-to x))))


;;;
;; Tree data-structure management
;;
(defclass tree-node ()
  ((name :initarg :name
	 :initform nil)
   (children :initarg :children
	     :initform nil)
   (expand :initarg :expand
	   :initform t)
   (parent :initarg :parent
	   :initform nil)
   (height :initarg :height
	   :initform 3)
   (currentpos :initform 0)
   )
  "Base class for a tree node")

(defmethod select ((tn tree-node))
  "Action to take when first mouse is clicked."
  (message "Clicked on node %s" (object-name tn))
  )
(defmethod edit ((tn tree-node))
  "Action to take when middle mouse button is clicked."
  (let ((nn (read-string "New name: ")))
    (oset tn name nn))
  (erase-buffer)
  (tree-refresh-tree)
  (goto-char (oref tn currentpos))
  )
(defmethod change-scope ((tn tree-node))
  "Action to take when last mouse is clicked on this node"
  (if (oref tn children)
      (progn
	(oset tn expand (not (oref tn expand)))
	(erase-buffer)
	(tree-refresh-tree)
	(goto-char (oref tn currentpos))
	)))

(defun tree-set-root (node)
  "Create a new tree NODE with the specified name, and make it the root."
  (setq tree-root-node node)
  )

(defun tree-new-node (name)
  "Create a new tree node with specified text NAME."
  (tree-node name :name name))

(defun tree-add-child (parent child)
  "Add to PARENT variable `tree-node' the variable `tree-node' CHILD.
Returns child to aid in building quick trees."
  (oset child parent parent)
  (oset parent children (append (oref parent children) (list child)))
  child)

(defun tree-sort-elements (node)
  "Sort all children of NODE, recurse."
  (let ((k (oref node children)))
    (setq k (sort k '(lambda (a b) (string< (oref a name) (oref b name)))))
    (oset node children k)
    (while k
      (tree-sort-elements (car k))
      (setq k (cdr k)))))

(defun tree-trim-below (node depth)
  "Set the expand field for NODE to nil for all nodes below DEPTH."
  (let ((k (oref node children)))
    (if (and k (<= depth 1)) (oset node expand nil))
    (while k
      (tree-trim-below (car k) (1- depth))
      (setq k (cdr k)))))


;;;
;; Tree node statistics
;;

(defun tree-node-width (node)
  "Return the width of NODE."
  (+ (length (oref node name)) 2 (if (not (oref node expand)) 3 0)))

(defun tree-level-width (node)
  "Return the widest box to appear under NODE."
  (let ((kids (oref node children))
	(w 0))
    (while kids
      (let ((tl (tree-node-width (car kids))))
	(if (and (< w tl) (oref (car kids) children) (oref (car kids) expand))
	    (setq w tl)))
      (setq kids (cdr kids)))
    w))

(defun tree-level-height (node)
  "Return the total height in chars of all nodes under NODE.
Cache the height into each node for later use"
  (let ((kids (oref node children))
	(h 0))
    (if (or (not kids) (not (oref node expand)))
	(setq h 3)
      (while kids
	(setq h (+ h (tree-level-height (car kids))))
	(setq kids (cdr kids))))
    (oset node height h)
    h))


;;;
;; Tree keyboard commands
;;

(defun tree-select-node ()
  "Activate the node currently under (point), or bell if none.
Requires text-properties"
  (interactive)
  (let ((node (get-text-property (point) 'node-object)))
    (if node
	(select node)
      (error "There is no tree-node under point"))))

(defun tree-select-node-mouse ()
  "Activate the node currently under (point), or bell if none.
Requires text-properties"
  (interactive)
  (call-interactively 'mouse-set-point)
  (let ((node (get-text-property (point) 'node-object)))
    (if node
	(select node)
      (error "There is no tree-node under point"))))

(defun tree-edit-node ()
  "Activate the node currently under (point), or bell if none.
Requires text-properties"
  (interactive)
  (let ((node (get-text-property (point) 'node-object)))
    (if node
	(edit node)
      (error "There is no tree-node under point"))))

(defun tree-edit-node-mouse ()
  "Activate the node currently under (point), or bell if none.
Requires text-properties"
  (interactive)
  (call-interactively 'mouse-set-point)
  (let ((node (get-text-property (point) 'node-object)))
    (if node
	(edit node)
      (error "There is no tree-node under point"))))

(defun tree-expand-or-contract-node ()
  "Activate the node currently under (point), or bell if none.
Requires text-properties"
  (interactive)
  (let ((node (get-text-property (point) 'node-object)))
    (if node
	(change-scope node)
      (error "There is no tree-node under point"))))

(defun tree-expand-or-contract-node-mouse ()
  "Activate the node currently under (point), or bell if none.
Requires text-properties"
  (interactive)
  (call-interactively 'mouse-set-point)
  (let ((node (get-text-property (point) 'node-object)))
    (if node
	(change-scope node)
      (error "There is no tree-node under point"))))


;;;
;; Tree test case
;;

;;;###autoload
(defun tree-test-it-all ()
  "Try using various features of tree mode in a demo of it's display."
  (interactive)
  ;; create a new buffer
  (switch-to-buffer (tree-new-buffer "*TREE DEMO*"))
  (erase-buffer)
  ;; set up the root node and some children
  (let* ((ntn (tree-set-root (tree-new-node "root")))
	 (stn1 (tree-add-child ntn (tree-new-node "Bob")))
	 (stn2 (tree-add-child ntn (tree-new-node "Stan")))
	 (stn3 (tree-add-child ntn (tree-new-node "Valarie")))
	 (sstn1 (tree-add-child stn1 (tree-new-node "Bob1")))
	 (sstn2 (tree-add-child stn1 (tree-new-node "Bob2")))
	 (sstn3 (tree-add-child stn1 (tree-new-node "Bob3")))
	 (sstn4 (tree-add-child stn1 (tree-new-node "Bob4")))
	 (sstn5 (tree-add-child stn3 (tree-new-node "Valarie2")))
	 (sstn6 (tree-add-child stn3 (tree-new-node "Valarie3")))
	 (ssstn1 (tree-add-child sstn4 (tree-new-node "Bobby1")))
	 (ssstn2 (tree-add-child sstn4 (tree-new-node "Bobby2")))
	 ;(ssstn2 (tree-add-child sstn4 (tree-new-node "Bobby3")))
	 ))
  (tree-refresh-tree)
  )


;;;
;; Tree demo using eieio class structures
;;
(defclass eieio-tree-node (tree-node)
  ((class :initarg :class
	  :initform nil))
  "Tree node used to represent eieio classes")

(defmethod edit ((etn eieio-tree-node))
  "Don't really edit, but pull up details about the given widget using
`eieio-describe-class'"
  (eieio-describe-class (oref etn class)))

(defmethod select ((etn eieio-tree-node))
  "Display a tiny bit of info about this object which might be useful"
  (message "%s" (class-name (oref etn class)))
  )

(defun eieio-new-node (class)
  "Create a new widget tree node with the specified WIDGET slot.
Argument CLASS is the class we are displaying."
  (eieio-tree-node (symbol-name class)
		   :name (symbol-name class)
		   :class class)
  )

;;;###autoload
(defun eieio-class-tree (&optional root-class)
  "Displays a class tree using the TREE package in another buffer.
Optional argument ROOT-CLASS is the starting point."
  (interactive)
  (if (not root-class) (setq root-class 'eieio-default-superclass))
  (switch-to-buffer (tree-new-buffer "*EIEIO CLASS TREE*"))
  (erase-buffer)
  (let ((np (tree-set-root (eieio-new-node root-class))))
    (eieio-tree-grow np))
  (tree-refresh-tree))

(defun eieio-tree-grow (node)
  "Add to NODE all children."
  (let* ((wk (aref (class-v (oref node class)) class-children))
	 nn)
    (while wk
      (setq nn (eieio-new-node (car wk)))
      (tree-add-child node nn)
      (eieio-tree-grow nn)
      (setq wk (cdr wk))))
  )


;;;
;; Tree demos using directories
;;
(defclass dirtree-node (tree-node)
  ((pathname :initarg :path
	     :initform nil)
   (haschildren :initarg :haschildren
		:initform unknown)
   )
  "A tree-node child class for displaying a directory.")

(defmethod edit ((dtn dirtree-node))
  "Action to take when this node is clicked."
  (find-file (format "%s%s" (oref dtn pathname) (oref dtn name)))
)
(defmethod select ((dtn dirtree-node))
  "Action to take when this node is clicked."
  (shell-command (format "ls -ld %s%s" (oref dtn pathname)
			 (oref dtn name)))
)
(defmethod change-scope ((dtn dirtree-node))
  "Action to take when last mouse is clicked on this node"
  ;; check for new nodes...
  (if (equal (oref dtn haschildren) 'unknown)
      (let ((path-path (oref dtn pathname)))
	(directory-tree-more-nodes dtn 1)))
  (if (oref dtn children)
      (progn
	(oset dtn expand (not (oref dtn expand)))
	(erase-buffer)
	(tree-refresh-tree)
	(goto-char (oref dtn currentpos))
	)
    ))

(defun dirtree-new (name path)
  "Create a new directory tree node.
Argument NAME is the name of the tree node.
Argument PATH is the path to that file."
  (dirtree-node name :name name :path path))

;;;###autoload
(defun directory-tree-thing (ppath)
  "Start at the current directory, and build a giant tree of files.
Argument PPATH is the path to the directory we are going to analyze."
  (interactive "fDirectory to graph: ")
  (let ((toppath (if (string-match "/$" ppath)
		     (substring ppath 0 (1- (length ppath)))
		   ppath)))
    (switch-to-buffer
     (tree-new-buffer (format "TREE: %s" (file-name-nondirectory toppath))))
    (erase-buffer)
    (let ((node (tree-set-root (dirtree-new
				(file-name-nondirectory toppath)
				(file-name-directory toppath)
				)))
	  (path-path (file-name-directory toppath)))
      (directory-tree-more-nodes node 2))
    (setq tree-buffer-mode 'tree-top-box-1)
    (message "Refreshing tree...")
    (tree-refresh-tree)
    ))

(defun directory-tree-more-nodes (node dokids)
  "Find more parts of this directory.  Do not expand kids if dokids = 0.
Argument NODE is the node to display.  DOKIDS is a flag to display children."
  (message "Tracing directory... [%s]" (oref node name))
  ;; mark that we checked this guy
  (oset node haschildren 'known)
  (let* ((nm (oref node name))
	 ;;                            path-path is letted in previous call
	 (files (directory-files (concat path-path nm) nil nil t)))
    (while files
      (if (or (string= "." (car files))
	      (string= ".." (car files)))
	  ()
	(if (file-accessible-directory-p (concat path-path nm "/"
						 (car files)))
	    (let ((path-path (concat path-path nm "/"))
		  (newnode (tree-add-child node (dirtree-new
						 (car files)
						 (concat path-path nm "/")
						 ))))
	      ;; These directories never have subdirectories, but
	      ;; often contain many many files!
	      (if (and (not (member (car files)
				    '(".xvpics" "SCCS" "RCS" "CVS")))
		       (< 0 dokids))
		  (progn
		    (directory-tree-more-nodes newnode (1- dokids))
		    (oset newnode children
			  (sort (oref newnode children)
				'(lambda (a b)
				   (string< (oref a name) (oref b name)))
				))
		    )))))
      (setq files (cdr files)))
    ;; mark not to expand..
    (if (and (= dokids 0) (oref node children))
	(oset node expand nil))
    )
  )

(provide 'tree)

;;; tree.el ends here
