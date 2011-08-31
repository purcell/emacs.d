;;; tree-buffer.el --- functions for tree buffers

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools, tree
;; Created: 2000

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id: tree-buffer.el,v 1.180 2009/05/13 17:17:32 berndl Exp $

;;; Commentary:

;; Functions for tree buffers.
;;
;; This file is part of the ECB package which can be found at:
;; http://ecb.sourceforge.net

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Code:

(eval-when-compile
  (require 'silentcomp))

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

;; XEmacs stuff
(silentcomp-defun button-release-event-p)
(silentcomp-defun button-press-event-p)
(silentcomp-defun event-key)
(silentcomp-defun extent-end-position)
(silentcomp-defun event-glyph-extent)
(silentcomp-defun event-over-glyph-p)
(silentcomp-defun display-message)
(silentcomp-defun clear-message)
(silentcomp-defun locate-data-directory)
(silentcomp-defun make-image-specifier)
(silentcomp-defun make-glyph)
(silentcomp-defun popup-menu-and-execute-in-window)
(silentcomp-defun valid-image-instantiator-format-p)
(silentcomp-defvar modeline-map)
;; Emacs
(silentcomp-defvar header-line-format)
(silentcomp-defvar message-log-max)
(silentcomp-defvar message-truncate-lines)
(silentcomp-defun posn-window)
(silentcomp-defun window-fringes)
(silentcomp-defun frame-parameter)
(silentcomp-defun frame-char-width)
(silentcomp-defun event-start)
(silentcomp-defun posn-point)
(silentcomp-defun event-basic-type)
(silentcomp-defun display-images-p)
(silentcomp-defun image-type-available-p)
(silentcomp-defun count-screen-lines)
(silentcomp-defun tmm-prompt)
(silentcomp-defun font-lock-add-keywords)
(silentcomp-defvar cursor-in-non-selected-windows)
;; timer stuff for XEmacs
(silentcomp-defun delete-itimer)
(silentcomp-defun start-itimer)

(defconst tree-buffer-running-xemacs (featurep 'xemacs))

;; miscellaneous differences

(if tree-buffer-running-xemacs
    ;; XEmacs
    (progn
      (defun tree-buffer-facep (face)
        (memq face (face-list)))
      (defalias 'tree-buffer-line-beginning-pos 'point-at-bol)
      (defalias 'tree-buffer-line-end-pos 'point-at-eol)
      (defun tree-buffer-frame-char-width (&optional frame)
        (/ (frame-pixel-width frame) (frame-width frame)))
      (defalias 'tree-buffer-window-display-height 'window-displayed-height)
      (defun tree-buffer-event-to-key (event)
        (typecase event
          (button-release-event 'mouse-release)
          (button-press-event 'mouse-press)
          (otherwise
           ;; the ignore-errors is a little hack because i don't know all
           ;; events of XEmacs so sometimes event-key produces a
           ;; wrong-type-argument error.
           (ignore-errors (event-key event)))))
      (defalias 'tree-buffer-event-window 'event-window)
      (defalias 'tree-buffer-event-point 'event-point)
      ;; stolen from dframe.el of the speedbar-library.
      (defun tree-buffer-mouse-set-point (e)
        "Set POINT based on event E. Handles clicking on images in XEmacs."
        (mouse-set-point e)
        (if (and (fboundp 'event-over-glyph-p) (event-over-glyph-p e))
            ;; We are in XEmacs, and clicked on a picture
            (let ((ext (event-glyph-extent e)))
              ;; This position is back inside the extent where the
              ;; junk we pushed into the property list lives.
              (if (extent-end-position ext)
                  (goto-char (1- (extent-end-position ext))))))))

  ;; GNU Emacs
  (defalias 'tree-buffer-facep 'facep)
  (defalias 'tree-buffer-line-beginning-pos 'line-beginning-position)
  (defalias 'tree-buffer-line-end-pos 'line-end-position)
  ;; Klaus Berndl <klaus.berndl@sdm.de>: Is not really the same as
  ;; `window-displayed-height' of XEmacs, because if the buffer-end is before
  ;; the window-end (i.e. there are "empty" lines between window-end and last
  ;; char of the buffer) then these empty-lines are not counted. But in the
  ;; situations this function is used (only in tree-buffer-recenter) this
  ;; doesn't matter.
  (defalias 'tree-buffer-frame-char-width 'frame-char-width)
  (defalias 'tree-buffer-window-display-height 'window-text-height)
  (defun tree-buffer-event-window (event)
    (posn-window (event-start event)))
  (defun tree-buffer-event-point (event)
    (posn-point (event-start event)))
  (defalias 'tree-buffer-mouse-set-point 'mouse-set-point)
  (defun tree-buffer-event-to-key (event)
    (let ((type (event-basic-type event)))
      (case type
        ((mouse-1 mouse-2 mouse-3) 'mouse-release)
        ((down-mouse-1 down-mouse-2 down-mouse-3) 'mouse-press)
        (otherwise (event-basic-type event)))))
  )

;; overlay/extend stuff

(if (not tree-buffer-running-xemacs)
    (progn
      (defalias 'tree-buffer-make-overlay   'make-overlay)
      (defalias 'tree-buffer-overlay-put    'overlay-put)
      (defalias 'tree-buffer-overlay-move   'move-overlay)
      (defalias 'tree-buffer-overlay-delete 'delete-overlay)
      (defalias 'tree-buffer-overlay-kill   'delete-overlay))
  ;; XEmacs
  (defalias 'tree-buffer-make-overlay   'make-extent)
  (defalias 'tree-buffer-overlay-put    'set-extent-property)
  (defalias 'tree-buffer-overlay-move   'set-extent-endpoints)
  (defalias 'tree-buffer-overlay-delete 'detach-extent)
  (defalias 'tree-buffer-overlay-kill   'delete-extent))


;; timer stuff

(if (not tree-buffer-running-xemacs)
    (progn
      (defalias 'tree-buffer-run-with-idle-timer 'run-with-idle-timer)
      (defalias 'tree-buffer-cancel-timer 'cancel-timer))
  ;; XEmacs
  (if (fboundp 'run-with-idle-timer)
      (defalias 'tree-buffer-run-with-idle-timer 'run-with-idle-timer)
    (defun tree-buffer-run-with-idle-timer (secs repeat function &rest args)
      "Perform an action the next time Emacs is idle for SECS seconds.
If REPEAT is non-nil, do this each time Emacs is idle for SECS seconds.
SECS may be an integer or a floating point number.
The action is to call FUNCTION with arguments ARGS.

This function returns a timer object which you can use in
`tree-buffer-cancel-timer'."
      (start-itimer "tree-buffer-idle-timer"
                    function secs (if repeat secs nil)
                    t (if args t nil) args)))

  (if (fboundp 'cancel-timer)
      (defalias 'tree-buffer-cancel-timer 'cancel-timer)
    (defun tree-buffer-cancel-timer (timer)
      "Remove TIMER from the list of active timers."
      (delete-itimer timer))))  


;; basic utilities

(defun tree-buffer-copy-list (list)
  "Return a copy of a LIST, which may be a dotted list.
The elements of the list are not copied, just the list structure itself."
  (if (fboundp 'copy-sequence)
      (copy-sequence list)
    (if (consp list)
        (let ((res nil))
          (while (consp list) (push (pop list) res))
          (prog1 (nreverse res) (setcdr res list)))
      (car list))))

(defun tree-buffer-member (item list &optional test-fcn)
  "Find the first occurrence of ITEM in LIST.
Return the sublist of LIST whose car is ITEM. Comparison is done with `equal'
unless TEST-FCN is not nil: In this case TEST-FCN will be used to compare ITEM
with the elements of LIST. If TEST-FCN is `eq' then `memq' is called for
optimization."
  (if test-fcn
      (if (eq test-fcn 'eq)
          ;; some optimization
          (memq item list)
        (progn
          (while (and list (not (funcall test-fcn item (car list))))
            (setq list (cdr list)))
          list))
    (member item list)))

(defun tree-buffer-position (seq elem &optional test-fcn)
  "Return the position of ELEM within SEQ counting from 0. Comparison is done
with `equal' unless TEST-FCN is not nil: In this case TEST-FCN will be used to
compare ITEM with the elements of SEQ."
  (if (listp seq)
      (let ((pos (- (length seq) (length (tree-buffer-member elem seq test-fcn)))))
        (if (= pos (length seq))
            nil
          pos))
    (catch 'found
      (dotimes (i (length seq))
        (if (funcall (or test-fcn 'equal) elem (aref seq i))
            (throw 'found i)))
      nil)))

(defun tree-buffer-last (seq)
  "Return the last elem of the sequence SEQ."
  (if (listp seq)
      (car (last seq))
    (if (> (length seq) 0)
        (aref seq (1- (length seq)))
      nil)))

(defun tree-buffer-first (seq)
  "Return the first elem of the sequence SEQ."
  (if (listp seq)
      (car seq)
    (if (> (length seq) 0)
        (aref seq 0)
      nil)))
  

(defun tree-buffer-set-elt (seq n val)
  "Set VAL as new N-th element of SEQ. SEQ can be any sequence. SEQ will be
changed because this is desctructive function. SEQ is returned."
  (if (listp seq)
      (setcar (nthcdr n seq) val)
    (aset seq n val))
  seq)

(defun tree-buffer-remove-elt (seq n)
  "Remove N-th element from SEQ. SEQ can be any sequence. SEQ will be
changed because this is desctructive function. SEQ is returned."
  (delq 'tree-buffer-remove-marker
        (tree-buffer-set-elt seq n 'tree-buffer-remove-marker)))

(defsubst tree-buffer-aset (array idx newelt)
  "Same as `aset' but returns changed ARRAY."
  (aset array idx newelt)
  array)

(defun tree-buffer-nolog-message (&rest args)
  "Works exactly like `message' but does not log the message"
  (let ((msg (cond ((or (null args)
                        (null (car args)))
                    nil)
                   ((null (cdr args))
                    (car args))
                   (t
                    (apply 'format args)))))
    ;; Now message is either nil or the formated string.
    (if tree-buffer-running-xemacs
        ;; XEmacs way of preventing log messages.
        (if msg
            (display-message 'no-log msg)
          (clear-message 'no-log))
      ;; Emacs way of preventing log messages.
      (let ((message-log-max nil)
            (message-truncate-lines nil))
        (if msg
            (message "%s" msg)
          (message nil))))
    msg))

(defsubst tree-buffer-current-line ()
  "Return the current line-number - the first line in a buffer has number 1."
  (+ (count-lines 1 (point)) (if (= (current-column) 0) 1 0)))

(defun tree-buffer-goto-line (line)
  "Goto LINE, counting from line 1 at beginning of buffer.

This function doesn't set the mark."
  ;; Move to the specified line number in that buffer.
  (save-restriction
    (widen)
    (goto-char 1)
    (if (eq selective-display t)
        (re-search-forward "[\n\C-m]" nil 'end (1- line))
      (forward-line (1- line)))))

;; debugging

(defvar tree-buffer-debug-mode nil
  "If not nil then all functions of tree-buffer which are debug-able write
debug-messages to the message-log of Emacs. Ensure that this variable is opnlx
not nil if you want find or report an error!")

(defun tree-buffer-debug-error (&rest args)
  "Run ARGS through `format' and write it to the *Messages*-buffer.
Do nothing if `tree-buffer-debug-mode' is nil!"
  (when tree-buffer-debug-mode
    (message (concat (format "Tree-buffer-debug: [%s] "
                             (format-time-string "%H:%M:%S"))
                     (apply 'format args)))))

;; tree-node

(defstruct (tree-node
            (:constructor -tree-node-new)
            (:copier nil)
            (:conc-name tree-node->))
  name
  type
  data
  children
  parent
  shrink-name
  expandable
  expanded
  displayed-name
  indentstr)

(defun tree-node-new (name type data &optional not-expandable parent
                           shrink-name)
  "Create a new tree-node which can be displayed in a tree-buffer.
A tree-node can have the following slots:

  NAME: The name of the node. Regardless how the node is displayed; see
  SHRINK-NAME and DISPLAYED-NAME.

  TYPE: The type of the node; must currently be an interger!

  DATA: The data of the node; can be arbitrary lisp-structures.

  EXPANDED: If not nil then the node is currently expanded, means its children
  are visible.

  PARENT: The parent tree-node.

  SHRINK-NAME: Decides if the NAME can be shortened when displayed in a
  narrow tree buffer window. The following values are valid:
  - beginning: The NAME is truncated at the beginning so the end is always
    visible.
  - end: The NAME is truncated at the end. If the tree-node is EXPANDABLE the
    name is truncated so that the expand symbol is visible.
  - nil: The NAME is never truncated. In this case DISPLAYED-NAME is equal to
    NAME.

  CHILDREN: List of children tree-nodes.

  EXPANDABLE: If not nil then the node is expandable means has children.

  INDENTSTR: Containes the full indentation-string for the node. So a single
  node can easily be redrawn.

  DISPLAYED-NAME: Contains the current displayed name of the node. The
  displayed name can be different from the NAME according to the value of
  SHRINK-NAME.

For all parameters except NOT-EXPANDABLE the description is available in the
slot-list above. If the first optional argument NOT-EXPANDABLE is set to not
nil then the slot EXPANDABLE will be set to nil; otherwise to t.

See Info node `(ecb)tree-buffer' for all details of using tree-nodes."
  (let ((n (-tree-node-new :name name
                           :type type
                           :data data
                           :expandable (not not-expandable)
                           :parent parent
                           :shrink-name shrink-name
                           :children nil
                           :expanded nil
                           :displayed-name nil
                           :indentstr nil)))
    (when (and parent (tree-node-p parent))
      (tree-node-add-children parent n))
    n))

(defsubst tree-node-indentlength (node)
  "Return the length of slot INDENTSTR."
  (length (tree-node->indentstr node)))

(defsubst tree-node-linelength (node)
  "Return the length of the full node display in current tree-buffer.
This is the length of the indentation \(slot INDENTSTR) plus the length of the
slot DISPLAYED-NAME of NODE."
  (+ (length (tree-node->displayed-name node))
     (tree-node-indentlength node)))

(defsubst tree-node-toggle-expanded (node)
  "Toggle the value of slot EXPANDED."
  (setf (tree-node->expanded node) (not (tree-node->expanded node))))

(defun tree-node-indent-level (node)
  "Return indentation-level of NODE.
Top-level nodes \(children of the root-node) have level 0."
  (let ((parent (tree-node->parent node)))
    (if (eq parent (tree-buffer-get-root))
        0
      (1+ (tree-node-indent-level parent)))))

(defun tree-node-new-root ()
  "Creates a new root node.
The root node has always NAME=\"root\", TYPE=-1 and DATA=nil. The root node
will not be displayed. Only the root-node is allowed to have as TYPE -1!"
  (tree-node-new "root" -1 nil))

(defun tree-node-update (node name type data expandable shrink-name)
  "Update NODE with setable datas.
Each of the arguments NAME, SHRINK-NAME, TYPE, DATA and EXPANDABLE can have
the special value 'use-old-value\; this means that this attribute/slot of NODE
will not be updated."
  (unless (eq name 'use-old-value)
    (setf (tree-node->name node) name))
  (unless (eq shrink-name 'use-old-value)
    (setf (tree-node->shrink-name node) shrink-name))
  (unless (eq type 'use-old-value)
    (setf (tree-node->type node) type))
  (unless (eq data 'use-old-value)
    (setf (tree-node->data node) data))
  (unless (eq expandable 'use-old-value)
    (setf (tree-node->expandable node) expandable)))

(defun tree-node-add-children (node children &optional at-beginning)
  "Add new CHILDREN to the already existing children of NODE.
If the optional arg AT_BEGINNING is not nil then the new CHILDREN will be
added to the beginning of the existing children of NODE otherwise to the end
\(default). CHILDREN must be either a single tree-node object or a list of
tree-nodes."
  (let ((c-list (typecase children
                  (tree-node (list children))
                  (list children)
                  (otherwise
                   (error "Children must be either a single tree-node or a list of tree-nodes.")))))
    ;; set NODE as parent of all new CHILDREN
    (dolist (c c-list)
      (setf (tree-node->parent c) node))
    ;; add the new CHILDREN to the existing ones
    (setf (tree-node->children node)
          (if at-beginning
              (append c-list (tree-node->children node))
            (append (tree-node->children node) c-list)))))

(defsubst tree-node-sort-children (node sortfn)
  "Run `sort' for the children of NODE with SORTFN as sorting-function.
SORTFN must be a function acceptable by `sort'. The sorted children-list
become the new children of NODE."
  (setf (tree-node->children node)
        (sort (tree-node->children node) sortfn)))

(defsubst tree-node-remove-child (node child)
  "Removes the CHILD from the childrens of NODE."
  (setf (tree-node->parent child) nil)
  (setf (tree-node->children node)
        (delq child (tree-node->children node))))

(defun tree-node-find-child-by-data/name (node child-data &optional child-name)
  "Finds the first child with the given CHILD-DATA.
CHILD-DATA will be compared with the data of each children of NODE by calling
`tree-buffer-node-data-equal-p'.
If CHILD-NAME is set then also the name of the child will be compared with
CHILD-NAME and must match."
  (catch 'exit
    (dolist (child (tree-node->children node))
      (when (and (tree-buffer-node-data-equal-p (tree-node->data child)
                                                child-data)
                 (or (null child-name)
                     (string= child-name (tree-node->name child))))
        (throw 'exit child)))))

(defun tree-node-remove-child-by-data/name (node child-data &optional child-name)
  "Removes the first child with the given CHILD-DATA.
Returns the removed child. CHILD-DATA will be compared with the data of each
children of NODE by calling `tree-buffer-node-data-equal-p'.
If CHILD-NAME is set then also the name of the child will be compared with
CHILD-NAME and must match."
  (catch 'exit
    (let ((last-cell nil)
	  (cell (tree-node->children node)))
      (while cell
	(when (and (tree-buffer-node-data-equal-p (tree-node->data (car cell))
                                                  child-data)
                   (or (null child-name)
                       (string= child-name (tree-node->name (car cell)))))
	  (if last-cell
	      (setcdr last-cell (cdr cell))
	    (setf (tree-node->children node) (cdr cell)))
	  (setcdr cell nil)
	  (setf (tree-node->parent (car cell)) nil)
	  (throw 'exit cell))
	(setq last-cell cell)
	(setq cell (cdr cell))))))

(defun tree-node-find-child-by-name (node child-name)
  "Return the first child of NODE with name CHILD-NAME."
  (catch 'exit
    (dolist (child (tree-node->children node))
      (when (equal (tree-node->name child) child-name)
        (throw 'exit child)))))

(defun tree-node-search-subtree-by-data/name (start-node data &optional name)
  "Search the full subtree of START-NODE for the first \(sub-)node with DATA.
If NAME is set then not only the data but also the name must match.
The \"full subtree\" means the START-NODE itself, its children, their grandchildren
etc. The search is done by a depth-first-search. Data-comparison is performed
with `tree-buffer-node-data-equal-p', name-comparison with `string='."
  (if (and (tree-buffer-node-data-equal-p data (tree-node->data start-node))
           (or (null name) (string= name (tree-node->name start-node))))
      start-node
    (catch 'exit
      (dolist (child (tree-node->children start-node))
	(let ((n (tree-node-search-subtree-by-data/name child data name)))
	  (when n
	    (throw 'exit n)))))))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: add this to texi
(defun tree-node-map-subtree (start-node map-fcn)
  "Apply MAP-FCN to full subtree of START-NODE and make a list of the results.
MAP-FCN is a function which gets a node of this subtree as argument.

Full subtree means the START-NODE itself and all its children and
all the grandchildren and so on; to each of these nodes MAP-FCN
is applied. If START-NODE is the root-node of current tree-buffer
then the START-NODE itself is not passed to MAP-FCN.

Often it is recommendable to apply a `delq' nil to the result
when the MAP-FCN does only perform for certain nodes, i.e. return
not nil only for certain nodes.

The subtree is walked by a depth-first-walk."
  (let ((result (unless (equal start-node (tree-buffer-get-root))
                  (mapcar map-fcn (list start-node)))))
    (dolist (child (tree-node->children start-node))
      (setq result
            (append result (tree-node-map-subtree child map-fcn))))
    result))

;; (defun tree-node-map-subtree-test ()
;;   (save-excursion
;;     (set-buffer ecb-methods-buffer-name)
;;     (let ((reslist (tree-node-map-subtree
;;                     (tree-buffer-get-root)
;;                     (function
;;                      (lambda (node)
;;                        (when (= (tree-node->type node)
;;                                 ecb-methods-nodetype-tag)
;;                          (ecb--semantic-tag-name (tree-node->data node))))))))
;;       reslist)))


;; ------- tree-buffer local variables ----------------------------------

(defvar tree-buffer-root nil
  "The \(not displayed) root-node of each tree-buffer.
The value is buffer-local in each tree-buffer.")

(defvar tree-buffer-displayed-nodes nil
  "Contains all the current visible nodes of current tree-buffer in
top-to-bottom order. This variable is buffer-local in each tree-buffer!")

(defsubst tree-buffer-initialize-displayed-nodes ()
  "Initialize the `tree-buffer-displayed-nodes' with nil."
  (setq tree-buffer-displayed-nodes nil))

(defsubst tree-buffer-number-of-displayed-nodes ()
  "Return the number of current displayed nodes."
  (length tree-buffer-displayed-nodes))

(defsubst tree-buffer-nth-displayed-node (n)
  "Return the N-th displayed node of current tree-buffer.
Counts from 0 whereas the 0-th node is the topmost displayed node."
  (nth n tree-buffer-displayed-nodes))

(defun tree-buffer-find-displayed-node-by-data/name (node-data &optional
                                                               node-name start-node)
  "Find the first displayed node in current tree-buffer having data NODA-DATA.
When START-NODE is nil then all currently visible nodes are searched beginning
with the first one otherwise START-NODE is the startpoint for the search.

If NODE-NAME is not nil then it must be A STRING and only a node
is found which has the same node-NAME.

If the search has success then the found node is returend."
  (catch 'exit
    (let ((node-list (if (or (null start-node)
                             (eq start-node (tree-buffer-get-root)))
                         tree-buffer-displayed-nodes
                       ;; we need that sub-list of tree-buffer-displayed-nodes
                       ;; which has the start-node as first elem. But we can
                       ;; not calling `member' for this search because this
                       ;; can result in a stack-overflow in equal for large
                       ;; node-lists especially with complex-data (e.g.
                       ;; semantic tags). Therefore we use `memq'.
                       (or (tree-buffer-member start-node
                                               tree-buffer-displayed-nodes
                                               'eq)
                           tree-buffer-displayed-nodes))))
      (dolist (node node-list)
        (when (and (tree-buffer-node-data-equal-p (tree-node->data node)
                                                  node-data)
                   (or (null node-name)
                       (and (stringp node-name)
                            (string= (tree-node->name node) node-name))))
          (throw 'exit node))))))

(defun tree-buffer-search-displayed-node-list (search-fcn)
  "Call SEARCH-FCN for each currently visible node in current tree-buffer.
Return the first node for which SEARCH-FCN returns not nil."
  (catch 'exit
    (dolist (node tree-buffer-displayed-nodes)
      (when (funcall search-fcn node)
        (throw 'exit node)))))

(defun tree-buffer-displayed-node-nr (node)
  "Return the number of NODE in the node-sequence of current tree-buffer.
Nodes are compared by `eq'! Number is counted from 0 whereas the topmost
displayed node ha number 0."
  (tree-buffer-position tree-buffer-displayed-nodes node 'eq))

(defun tree-buffer-displayed-node-linenr (node)
  "Return the line-number of NODE in current tree-buffer.
Nodes are compared by `eq'."
  (1+ (tree-buffer-displayed-node-nr node)))

(defsubst tree-buffer-add-to-displayed-nodes (node)
  "Add NODE at the end of the displayed nodes `tree-buffer-displayed-nodes'."
  (setq tree-buffer-displayed-nodes
        (append tree-buffer-displayed-nodes (list node))))

(defsubst tree-buffer-displayed-nodes-remove-nth (n)
  "Remove the N-th node from the displayed nodes `tree-buffer-displayed-nodes'."
  (tree-buffer-remove-elt tree-buffer-displayed-nodes n))

(defsubst tree-buffer-displayed-nodes-remove-node (node)
  "Remove NODE from the displayed nodes `tree-buffer-displayed-nodes'."
  (setq tree-buffer-displayed-nodes
        (delq node tree-buffer-displayed-nodes)))

(defsubst tree-buffer-displayed-nodes-replace-nth (n new-node)
  "Replace the N-th node with NEW-NODE in `tree-buffer-displayed-nodes'.
Return the updated list."
  (tree-buffer-set-elt tree-buffer-displayed-nodes n new-node))

(defun tree-buffer-displayed-nodes-replace-node (node new-node)
  "Replace NODE with NEW-NODE in `tree-buffer-displayed-nodes'.
Return the updated list."
  (let ((memq-list (tree-buffer-member node tree-buffer-displayed-nodes 'eq)))
    (if memq-list
        (setcar memq-list new-node)))
  tree-buffer-displayed-nodes)

(defsubst tree-buffer-set-displayed-nodes (displayed-nodes)
  "Set `tree-buffer-displayed-nodes' to DISPLAYED-NODES.
DISPLAYED-NODES which has to be a list of node-objects. Replaces the old list
of displayed-nodes."
  (setq tree-buffer-displayed-nodes displayed-nodes))

(defsubst tree-buffer-displayed-nodes-copy ()
  "Return a copy of the displayed-nodes-list `tree-buffer-displayed-nodes'.
Only the list-structure is copied not the elements itself."
  (tree-buffer-copy-list tree-buffer-displayed-nodes))

(defsubst tree-buffer-map-displayed-nodes (function)
  "Apply function to each node of `tree-buffer-displayed-nodes'.
Make a list of the results. The result is a list just as long as
`tree-buffer-displayed-nodes'."
  (mapcar (function (lambda (n)
                      (funcall function n)))
          tree-buffer-displayed-nodes))

;; rest of tree-buffer local variables

(defvar tree-buffer-frame nil
  "The frame the tree-buffer lives in.
The value is buffer-local in current tree-buffer.")

(defvar tree-buffer-key-map nil
  "The local keymap of current tree-buffer.
The value is buffer-local in current tree-buffer.")

(defvar tree-buffer-highlighted-node nil
  "The data of that node which is currently highlighted.
It's a list with three elements: The first element is the data-slot of the
node, the second one is the name-slot and the third one is the node object
itself.

This variable is only set by `tree-buffer-highlight-node-by-data/name'.

The value is buffer-local in current tree-buffer.")

(defun tree-buffer-highlighted-node-matches-data/name-p (data name)
  "return not nil iff currently highlighted node matches passed data and name.

Currently highlighted node is stored in `tree-buffer-highlighted-node'."
  (and (tree-buffer-node-data-equal-p data (nth 0 tree-buffer-highlighted-node))
       ;; if stored name is nil then it has not been set by
       ;; `tree-buffer-highlight-node-by-data/name' and is therefore not
       ;; valid to compare. If set it must match the passed name.
       (or (null (nth 1 tree-buffer-highlighted-node))
           (and (stringp name)
                (string= name (nth 1 tree-buffer-highlighted-node))))))


(defvar tree-buffer-highlight-overlay nil
  "Overlay \(rsp. extent for XEmacs) used for highlighting current node.
The value is buffer-local in current tree-buffer.")

(defvar tree-buffer-general-overlay nil
  "Overlay \(rsp. extent for XEmacs) used for displaying the whole content.
The value is buffer-local in current tree-buffer.")

(defvar tree-buffer-spec nil
  "A Buffer local object of type tree-buffer-spec.")

(defvar tree-buffer-hscroll-number 0
  "Current value of horizontal tree-buffer-scrolling'.
The value is buffer-local in current tree-buffer.")

(defvar tree-buffer-sticky-parent-node-function nil
  "Function used to get that parent node which should be sticky.
This function gets as argument a node and should either return nil \(if there
is not suitable parent node) or node. This node will be display as sticky in
the header-line of the tree-buffer.")


;; tree-buffer specification

(defstruct (tree-buffer-spec
            (:constructor -tree-buffer-spec-new)
            (:copier nil)
            (:conc-name tree-buffer-spec->))
  (tree-indent nil :read-only t)
  (menu-creator nil :read-only t)
  (menu-titles nil :read-only t)
  (modeline-menu-creator :read-only t)
  (sticky-parent-p :read-only t)
  (sticky-indent-string :read-only t)
  (sticky-parent-fn :read-only t)
  (type-facer nil :read-only t)
  (expand-symbol-before-p nil :read-only t)
  (mouse-action-trigger nil :read-only t)
  (is-click-valid-fn nil :read-only t)
  (node-selected-fn nil :read-only t)
  (node-expanded-fn nil :read-only t)
  (node-collapsed-fn nil :read-only t)
  (node-mouse-over-fn nil :read-only t)
  (mouse-highlight-fn nil :read-only t)
  (node-data-equal-fn nil :read-only t)
  (after-update-hook nil :read-only t)
  (maybe-empty-node-types nil :read-only t)
  (leaf-node-types nil :read-only t)
  (general-face nil :read-only t)
  (incr-search-additional-pattern nil :read-only t)
  (incr-search-p nil :read-only t)
  (reduce-tree-for-incr-search-fn :read-only t)
  (hor-scroll-step nil :read-only t)
  (default-images-dir nil :read-only t)
  (additional-images-dir nil :read-only t)
  (image-file-prefix nil :read-only t)
  (style nil :read-only t)
  (ascii-guide-face nil :read-only t))

(defun* tree-buffer-spec-new (&key
                              tree-indent
                              menu-creator
                              menu-titles
                              modeline-menu-creator
                              sticky-parent-p
                              sticky-indent-string
                              sticky-parent-fn
                              type-facer
                              expand-symbol-before-p
                              mouse-action-trigger
                              is-click-valid-fn
                              node-selected-fn
                              node-expanded-fn
                              node-collapsed-fn
                              node-mouse-over-fn
                              mouse-highlight-fn
                              node-data-equal-fn
                              after-update-hook
                              maybe-empty-node-types
                              leaf-node-types
                              general-face
                              incr-search-additional-pattern
                              incr-search-p
                              reduce-tree-for-incr-search-fn
                              hor-scroll-step
                              default-images-dir
                              additional-images-dir
                              image-file-prefix
                              style
                              ascii-guide-face)
  "Creates and returns a new specification object for current tree-buffer.

The arguments are key-arguments of the form :arg-name arg-value, so for
example a call looks like \(tree-buffer-spec-new :menu-creator 'creator...)
The key-arguments can be arranged in any arbitrary order but all of them are
not-optional! The key-arg-name is always a : followed by the lowercase version
of the mentioned argument \(e.g. MENU-CREATOR --> :menu-creator)

See `tree-buffer-create' for a description of the arguments."
  (let ((my-style (tree-buffer-real-style style)))
    (-tree-buffer-spec-new :menu-creator menu-creator
                           :menu-titles menu-titles
                           :modeline-menu-creator modeline-menu-creator
                           :sticky-parent-p sticky-parent-p
                           :sticky-indent-string sticky-indent-string
                           :sticky-parent-fn sticky-parent-fn
                           :type-facer type-facer
                           :mouse-action-trigger mouse-action-trigger
                           :is-click-valid-fn is-click-valid-fn
                           :node-selected-fn node-selected-fn
                           :node-expanded-fn node-expanded-fn
                           :node-collapsed-fn node-collapsed-fn
                           :node-mouse-over-fn node-mouse-over-fn
                           :mouse-highlight-fn mouse-highlight-fn
                           :node-data-equal-fn node-data-equal-fn
                           :after-update-hook
                           (if (functionp after-update-hook)
                               (list after-update-hook)
                             after-update-hook)
                           :maybe-empty-node-types maybe-empty-node-types
                           :leaf-node-types leaf-node-types
                           :general-face general-face
                           :incr-search-additional-pattern incr-search-additional-pattern
                           :incr-search-p incr-search-p
                           :reduce-tree-for-incr-search-fn
                           (or (and (functionp reduce-tree-for-incr-search-fn)
                                    reduce-tree-for-incr-search-fn)
                               'ignore)
                           :hor-scroll-step hor-scroll-step
                           :default-images-dir default-images-dir
                           :additional-images-dir additional-images-dir
                           :image-file-prefix image-file-prefix
                           :style my-style
                           :expand-symbol-before-p (if (equal 'image my-style)
                                                       t
                                                     expand-symbol-before-p)
                           :tree-indent
                           (cond ((equal 'image my-style)
                                  tree-buffer-indent-with-images)
                                 (expand-symbol-before-p
                                  (if (< tree-indent
                                         tree-buffer-indent-w/o-images-before-min)
                                      tree-buffer-indent-w/o-images-before-min
                                    tree-indent))
                                 (t ;; after
                                  (if (< tree-indent
                                         tree-buffer-indent-w/o-images-after-min)
                                      tree-buffer-indent-w/o-images-after-min
                                    tree-indent)))
                           :ascii-guide-face ascii-guide-face)))

;; incremental search in a tree-buffer 

(defconst tree-buffer-incr-searchpattern-expand-prefix
  "\\(\\[[^][]+\\] ?\\)?\\[?"
  "The prefix ignores all expand/collapse-buttons: \[+], \[x], rsp. \[-]")

(defvar tree-buffer-incr-searchpattern nil
  "Current search pattern when a inremental search is active.
The value is buffer-local in current tree-buffer.")

(defvar tree-buffer-last-incr-searchpattern nil
  "Most recent used search-pattern for incremental search.
Used to compared with the value of `tree-buffer-incr-searchpattern'.
The value is buffer-local in current tree-buffer.")

;; This can not be part of `tree-buffer-spec' because then a call to
;; `tree-buffer-gen-searchpattern-indent-prefix' would be necessary *before*
;; the tree-buffer-spec object is created and this would cause a cyclic
;; dependency in `tree-buffer-real-style'.
(defvar tree-buffer-incr-searchpattern-indent-prefix nil
  "Prefix-pattern which ignores all not interesting basic stuff of a displayed
tag at incr. search. The following contents of a displayed tag are ignored
by this pattern:
- beginning spaces and guide characters \(|`-)
This prefix is computed by `tree-buffer-gen-searchpattern-indent-prefix'!
The value is buffer-local in current tree-buffer.")

;; --- tree-buffer-local data-storage with get- and set-function --------

(defvar tree-buffer-data-store nil
  "Arbitrary data-storage which is buffer-local for each tree-buffer.
Use `tree-buffer-set-data-store' and `tree-buffer-get-data-store' to set and
get the data.")

(defsubst tree-buffer-set-data-store (data)
  "Set `tree-buffer-data-store' to DATA."
  (setq tree-buffer-data-store data))

(defsubst tree-buffer-get-data-store ()
  "Return the current value of `tree-buffer-data-store'."
  tree-buffer-data-store)

;; ------- image support ------------------------------------------------

(defvar tree-buffer-local-image-cache nil
  "Alist with car is one of the names in
`tree-buffer-tree-image-names' and cdr is an associated image-object.")

(defvar tree-buffer-images-can-be-used nil
  "INTERNAL - DO NOT USE AND CHANGE!")
(defvar tree-buffer-images-can-be-used-init-p nil
  "INTERNAL - DO NOT USE AND CHANGE!")

(defun tree-buffer-images-can-be-used ()
  "Not nil if images can be used with current Emacs setup."
  (if tree-buffer-images-can-be-used-init-p
      tree-buffer-images-can-be-used
    (setq tree-buffer-images-can-be-used-init-p t)
    (setq tree-buffer-images-can-be-used
          (and (or (fboundp 'defimage)
                   (fboundp 'make-image-specifier))
               (if (fboundp 'display-images-p)
                   (display-images-p)
                 window-system)))))

(defvar tree-buffer-image-properties-emacs
  '(:ascent center :mask (heuristic t))
  "Properties of GNU Emacs images.")

(defvar tree-buffer-image-properties-xemacs
  nil
  "Properties of XEmacs images.")

(defvar tree-buffer-enable-xemacs-image-bug-hack
  tree-buffer-running-xemacs
  "If true then tree-buffer tries to deal best with the XEmacs-bug to display
adjacent images not correctly. Set this to nil if your XEmacs-version has fixed
this bug.")

(defconst tree-buffer-image-formats
  '((xpm ".xpm") (png ".png") (gif ".gif") (jpeg ".jpg" ".jpeg")
    (xbm ".xbm")))

(defconst tree-buffer-expand-symbol-length 3)
(defconst tree-buffer-indent-with-images 3)
(defconst tree-buffer-indent-w/o-images-before-min 3)
(defconst tree-buffer-indent-w/o-images-after-min 2)

(defconst tree-buffer-tree-image-names
  '(("open"      . ((after . "[-]") (before . "[-]")))
    ("close"     . ((after . "[+]") (before . "[+]")))
    ("empty"     . ((after . "[x]") (before . "[x]")))
    ("leaf"      . ((after . "*")   (before . "*")))
    ("guide"     . ((after . "|")   (before . " |")))
    ("no-guide"  . ((after . " ")   (before . "  ")))
    ("end-guide" . ((after . "`")   (before . " `")))
    ("handle"    . ((after . "-")   (before . "-")))
    ("no-handle" . ((after . " ")   (before . " "))))
  "This alist contains all allowed tree-image-names and their corresponding
ascii-representation. Currently allowed names for tree-images and current
ascii-symbols are: open, close, empty, leaf, guide, noguide, end-guide,
handle, no-handle. See the value of this constant for the ascii-symbols
related to the names.")

(if tree-buffer-running-xemacs
    (progn
      (defsubst tree-buffer-create-image (file type)
        "Create an image of type TYPE from FILE. Return the new image."
        (apply 'make-glyph
               `([,type :file ,file
                        ,@tree-buffer-image-properties-xemacs])))
      (defsubst tree-buffer-image-type-available-p (type)
        "Return non-nil if image type TYPE is available.
Image types are symbols like `xbm' or `jpeg'."
        (valid-image-instantiator-format-p type)))
  (defsubst tree-buffer-create-image (file type)
    (apply 'create-image
           `(,file ,type nil
                   ,@tree-buffer-image-properties-emacs)))
  (defsubst tree-buffer-image-type-available-p (type)
    "Return non-nil if image type TYPE is available.
Image types are symbols like `xbm' or `jpeg'."
    (image-type-available-p type)))

(defun tree-buffer-real-style (&optional style)
  "Return the currently used style of the tree-buffer. If \X)Emacs allows
displaying images then this is simply the value of the slot STYLE of
`tree-buffer-spec'. Otherwise the style 'image is replaced by 'ascii-guides.
If the optional arg STYLE is not nil then this is used instead of the slot
STYLE of `tree-buffer-spec'. Allowed values of STYLE are nil, 'image,
'ascii-guides, ascii-no-guides."
  (let ((my-style (or style (tree-buffer-spec->style tree-buffer-spec))))
    (if (tree-buffer-images-can-be-used)
        my-style
      (if (equal my-style 'image)
          'ascii-guides
        my-style))))

(defsubst tree-buffer-ascii-symbol-4-image-name (name)
  "Return the ascii-symbol which displays the tree-image NAME. This is done
according to the value of the slot EXPAND-SYMBOL-BEFORE-P of
`tree-buffer-spec'. It always returns a copy of the registered string in
`tree-buffer-tree-image-names'!"
  (let ((sym (if (tree-buffer-spec->expand-symbol-before-p tree-buffer-spec)
                 'before
               'after)))
    ;; Klaus Berndl <klaus.berndl@sdm.de>: If there are performance issues
    ;; concerning the tree-buffer-redisplay then maybe this copy-sequence is
    ;; the reason. But must be profiled! If yes, then an alternative could be
    ;; not to return copies but references and write a function which removes
    ;; all text-properties from the strings in `tree-buffer-tree-image-names'.
    ;; This function has either to be called once or within
    ;; `tree-buffer-create'. But for the moment we use copies.
    ;; Background: Without copies or without removing the text-properties from
    ;; the strings in `tree-buffer-tree-image-names' before using tree-buffers
    ;; we also get some images if we switch from image- to ascii-display
    ;; without restarting emacs.
    (copy-sequence
     (cdr (assoc sym (cdr (assoc name tree-buffer-tree-image-names)))))))


(defun tree-buffer-add-image-icon-maybe (start len str image-icon)
  "Add IMAGE-ICON to STR between START \(incl.) and START+LEN \(excl.). If
IMAGE-ICON is not nil \(which must be an image-object in the sense of
\(X)Emacs) then add this image to STR otherwise do nothing. Normally
IMAGE-ICON should be either nil or an image-object returned by
`tree-buffer-find-image'. Always return STR. If IMAGE-ICON is nil or
`tree-buffer-real-style' returns not 'image then START and LEN are ignored!
If an image is added then two text-properties are added to the full length of
STR: 'tree-buffer-image-start which holds START as value and
'tree-buffer-image-length which holds LEN as value."
  (when (equal 'image (tree-buffer-real-style))
    ;; Regular images (created with `insert-image' are intangible
    ;; which (I suppose) make them more compatible with XEmacs 21.
    ;; Unfortunately, there is a giant pile of code dependent on the
    ;; underlying text.  This means if we leave it tangible, then I
    ;; don't have to change said giant piles of code.
    (when image-icon
      (if tree-buffer-running-xemacs
          (add-text-properties (+ start len) start
                               (list 'end-glyph image-icon
                                     'rear-nonsticky (list 'display)
                                     'invisible t
                                     'detachable t)
                               str)
        (add-text-properties start (+ start len)
                             (list 'display image-icon
                                   'rear-nonsticky (list 'display))
                             str))
      (add-text-properties 0 (length str)
                           (list 'tree-buffer-image-start start
                                 'tree-buffer-image-length len)
                           str)))
  str)

(defsubst tree-buffer-image-cache-get (tree-image-name)
  (cdr (assoc tree-image-name
              tree-buffer-local-image-cache)))

(defsubst tree-buffer-image-cache-put (tree-image-name image)
  (setq tree-buffer-local-image-cache
        (cons (cons tree-image-name image)
              tree-buffer-local-image-cache)))

(defun tree-buffer-find-image (tree-image-name)
  "Return an image-object for the TREE-IMAGE-NAME. The needed image-file with
name \"<prefix><TREE-IMAGE-NAME>.<a supported image-file-extension>\" is first
searched in the dir of slot ADDITIONAL-IMAGES-DIR of `tree-buffer-spec' \(if
not nil) and then - if there is no image found for this name - in the dir of
slot DEFAULT-IMAGES-DIR of `tree-buffer-spec'. <prefix> is the value of the
slot IMAGE-FILE-PREFIX of `tree-buffer-spec'. All found and created
image-objectes will be cached so every image is only created once! Returns the
image-object for TREE-IMAGE-NAME."
  (and (equal 'image (tree-buffer-real-style))
       ;; Klaus Berndl <klaus.berndl@sdm.de>: This comes from the XEmacs-bug
       ;; not able to display adjacent images.
       (or (not tree-buffer-enable-xemacs-image-bug-hack)
           (not (member tree-image-name
                        '("handle" "no-handle"))))
       (or (tree-buffer-image-cache-get tree-image-name)
           (let ((dirs (mapcar 'expand-file-name
                               (if (tree-buffer-spec->additional-images-dir
                                    tree-buffer-spec)
                                   (list (tree-buffer-spec->additional-images-dir
                                          tree-buffer-spec)
                                         (tree-buffer-spec->default-images-dir
                                          tree-buffer-spec))
                                 (list (tree-buffer-spec->default-images-dir
                                        tree-buffer-spec)))))
                 (fmt-specs tree-buffer-image-formats)
                 fmt fmt-exts file file-name image loc-dirs)
             (while (and fmt-specs (not file))
               (setq fmt (car (car fmt-specs))
                     fmt-exts (cdr (car fmt-specs))
                     fmt-specs (cdr fmt-specs))
               (when (tree-buffer-image-type-available-p fmt)
                 (while (and fmt-exts (not file))
                   (setq loc-dirs dirs)
                   (while (and loc-dirs (not file))
                     (setq file-name (concat (car loc-dirs) "/"
                                             (tree-buffer-spec->image-file-prefix
                                              tree-buffer-spec)
                                             tree-image-name
                                             (car fmt-exts)))
                     (when (file-readable-p file-name)
                       (setq file file-name))
                     (setq loc-dirs (cdr loc-dirs)))
                   (setq fmt-exts (cdr fmt-exts)))))
             (when file
               (setq image (tree-buffer-create-image file fmt))
               (tree-buffer-image-cache-put tree-image-name
                                            image)
               image)))))

;; ------ tree-buffer global variables -----------------------------------

(defvar tree-buffers nil)

(defvar tree-buffer-syntax-table nil
  "Syntax-table used in a tree-buffer.")

(if tree-buffer-syntax-table
    nil
  (setq tree-buffer-syntax-table (make-syntax-table))
  ;; turn off paren matching around here.
  (modify-syntax-entry ?\' " " tree-buffer-syntax-table)
  (modify-syntax-entry ?\" " " tree-buffer-syntax-table)
  (modify-syntax-entry ?\( " " tree-buffer-syntax-table)
  (modify-syntax-entry ?\) " " tree-buffer-syntax-table)
  (modify-syntax-entry ?\{ " " tree-buffer-syntax-table)
  (modify-syntax-entry ?\} " " tree-buffer-syntax-table)
  (modify-syntax-entry ?\[ " " tree-buffer-syntax-table)
  (modify-syntax-entry ?\] " " tree-buffer-syntax-table))

;; ------- utilities --------------------------------------------------

(defun tree-buffer-get-node-name-start-column (node)
  "Returns the buffer column where the name of the node starts."
  (+ (tree-node-indentlength node)
     (if (and (tree-buffer-spec->expand-symbol-before-p
               tree-buffer-spec)
              (or (tree-node->expandable node)
                  (member (tree-node->type node)
                          (tree-buffer-spec->maybe-empty-node-types
                           tree-buffer-spec))))
         (if (or tree-buffer-enable-xemacs-image-bug-hack
                 (not (equal 'image (tree-buffer-real-style))))
             4 3)
       0)
     (if (and (tree-buffer-spec->expand-symbol-before-p
               tree-buffer-spec)
              (not (tree-node->expandable node))
              (member (tree-node->type node)
                      (tree-buffer-spec->leaf-node-types tree-buffer-spec)))
         (if (or tree-buffer-enable-xemacs-image-bug-hack
                 (not (equal 'image (tree-buffer-real-style))))
             2 1)
       0)))
     

(defun tree-buffer-get-node-name-start-point (node)
  "Returns the buffer point where the name of the NODE starts."
  (let ((linenr (tree-buffer-displayed-node-linenr node)))
    (tree-buffer-debug-error "tree-buffer-get-node-name-start-point: Cur-buf: %s, linenr: %d"
                             (current-buffer) linenr)
    (when linenr
      (save-excursion
        (tree-buffer-goto-line linenr)
        (beginning-of-line)
        (+ (point) (tree-buffer-get-node-name-start-column node))))))

(defun tree-buffer-get-node-name-end-point (node)
  "Returns the buffer point where the name of the NODE ends."
  (tree-buffer-debug-error "tree-buffer-get-node-name-end-point: Cur-buf: %s"
                           (current-buffer))
  (+ (tree-buffer-get-node-name-start-point node)
     (length (tree-node->displayed-name node))))

(defun tree-buffer-point-at-expand-symbol-p (node p)
  "Return not nil if point P is located at the expand-symbol of NODE."
  (tree-buffer-debug-error "tree-buffer-point-at-expand-symbol-p: Cur-buf: %s, p: %d, exp-sym-before: %s"
                           (current-buffer)
                           p (tree-buffer-spec->expand-symbol-before-p tree-buffer-spec))
  (when (or (tree-node->expandable node)
            ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: maybe it would be
            ;; better to handle such nodes as if point can not stay at the
            ;; empty-symbol... 
            (member (tree-node->type node)
                    (tree-buffer-spec->maybe-empty-node-types tree-buffer-spec)))
    (let ((start-point (tree-buffer-get-node-name-start-point node))
          (end-point (tree-buffer-get-node-name-end-point node)))
      (if (tree-buffer-spec->expand-symbol-before-p tree-buffer-spec)
          (and (>= p (- start-point 4))
               (< p (1- start-point)))
        (and (> p end-point)
             (<= p (+ end-point 4)))))))

(defun tree-buffer-point-at-node-name-p (node p)
  "Return not nil if point P is located at the displayed-name of NODE."
  (tree-buffer-debug-error "tree-buffer-point-at-node-name-p: Cur-buf: %s, p: %d"
                           (current-buffer) p)
  (and (>= p (tree-buffer-get-node-name-start-point node))
       (< p (tree-buffer-get-node-name-end-point node))))

(defun tree-buffer-get-node-at-point (&optional p)
  "Returns the node at point P. If p is nil the current point is used."
  (save-excursion
    (if p (goto-char p))
    (tree-buffer-nth-displayed-node (1- (tree-buffer-current-line)))))

(defun tree-buffer-select (mouse-button additional-key-list)
  "If the callback-function in slot IS-CLICK-VALID-FN of `tree-buffer-spec'
returns nil then nothing is done. Otherwise: If either the MOUSE-BUTTON is 0
or point is as the node-name then the callback-function in slot
NODE-SELECTED-FN is called with the needed arguments \(see
`tree-buffer-create'). If point is at the expand/collape-button depending of
the expansion-state either the callback in slot NODE-EXPANDED-FN or
NODE-COLLAPSED-FN is called \(for parameters see again `tree-buffer-create').
None of these callbacks must modify the slot EXPANDED of the passed node
because this is done automatically by this function.
ADDITIONAL-KEY-LIST is either nil or a list of additonal keys pressed. If not
nil only the symbols 'shift, 'control and 'meta are recognized."
  (unless (not (equal (selected-frame) tree-buffer-frame))
    (let ((shift-pressed (member 'shift additional-key-list))
          (control-pressed (member 'control additional-key-list))
          (meta-pressed (member 'meta additional-key-list)))
      (when (and (tree-buffer-spec->is-click-valid-fn tree-buffer-spec)
                 (funcall (tree-buffer-spec->is-click-valid-fn tree-buffer-spec)
                          mouse-button shift-pressed control-pressed meta-pressed
                          (buffer-name)))
        (tree-buffer-debug-error "tree-buffer-select-1: Cur-buf: %s"
                                 (current-buffer))
        (let ((node (tree-buffer-get-node-at-point)))
          (when node
            (tree-buffer-debug-error "tree-buffer-select-2: Cur-buf: %s"
                                     (current-buffer))
            ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Is this the right place
            ;; for this? probably it can cause some erros...... Yep - it causes
            ;; serious XEmacs-sideeffects: clicking into tree-buffer doesn't
            ;; work anymore when doing this during an active isearch! Seems that
            ;; isearch-exit switches the current buffer so the buffer after the
            ;; isearch-exit is not the same as before!! So we comment this out!!
            ;;           (ignore-errors
            ;;             (let ((search-nonincremental-instead nil))
            ;;               (isearch-exit)))
            (tree-buffer-debug-error "tree-buffer-select-3: Cur-buf: %s"
                                     (current-buffer))
            (cond ((or (= mouse-button 0)
                       (tree-buffer-point-at-node-name-p node (point)))
                   (setq tree-buffer-incr-searchpattern "")
                   (when (tree-buffer-spec->node-selected-fn tree-buffer-spec)
                     (funcall (tree-buffer-spec->node-selected-fn tree-buffer-spec)
                              node mouse-button shift-pressed control-pressed meta-pressed
                              (buffer-name))))
                  ((tree-buffer-point-at-expand-symbol-p node (point))
                   (when (and (not (tree-node->expanded node))
                              (tree-buffer-spec->node-expanded-fn tree-buffer-spec))
                     (funcall (tree-buffer-spec->node-expanded-fn tree-buffer-spec)
                              node mouse-button
                              shift-pressed control-pressed meta-pressed
                              (buffer-name)))
                   (when (tree-node->expandable node)
                     (when (and (tree-node->expanded node)
                                (tree-buffer-spec->node-collapsed-fn tree-buffer-spec))
                       (funcall (tree-buffer-spec->node-collapsed-fn tree-buffer-spec)
                                node mouse-button
                                shift-pressed control-pressed meta-pressed
                                (buffer-name)))
                     (tree-node-toggle-expanded node))
                   ;; Update the tree-buffer with optimized display of NODE
                   (tree-buffer-update node)))))))))


(defun tree-buffer-node-data-equal-p (node-data-1 node-data-2)
  "Calls the function stored in slot NODE-DATA-EQUAL-FN of `tree-buffer-spec'
to test NODE-DATA-1 and NODE-DATA-2 for equality."
  (and node-data-1 node-data-2
       ;; if this comparison-function runs into an error we handle this as
       ;; non-equality!
       (ignore-errors
         (funcall (tree-buffer-spec->node-data-equal-fn tree-buffer-spec)
                  node-data-1 node-data-2))))

(defun tree-buffer-get-node-facer (node)
  (let ((facer (cdr (assoc (tree-node->type node)
                           (tree-buffer-spec->type-facer tree-buffer-spec)))))
    (if facer
        facer
      nil)))

(defun tree-buffer-pos-hor-visible-p (pos window)
  "Return non nil if POS is horizontal visible in WINDOW otherwise nil."
  (save-excursion
    (goto-char pos)
    (and (>= (- (current-column) (window-hscroll window)) 0)
         (< (- (current-column) (window-hscroll window))
            (window-width window)))))

(defun tree-buffer-get-event-column (e &optional include-fringe-scrollbar)
  "Return the clicked column on a char-base."
  (let* ((x-point (car (nth 2 (car (cdr e)))))
	 (pixels-per-10-col (/ (* 10 (frame-pixel-width))
			       (frame-width)))
	 (click-col (+ (/ (* 10 x-point) pixels-per-10-col)
                       (if include-fringe-scrollbar
                           (length (tree-buffer-spec->sticky-indent-string
                                    tree-buffer-spec))
                         0))))
    click-col))

(defun tree-buffer-hscroll (amount)
  (ignore-errors
    (let ((current-prefix-arg amount))
      (call-interactively 'scroll-left))))

;; Stolen from dframe.el from the speedbar-library
;; XEmacs: this can be implemented using modeline key-maps, but there
;; is no use, as we have horizontal scrollbar (as the docstring
;; hints.)
(defun tree-buffer-mouse-hscroll (e)
  "Read a mouse event E from the mode line and scroll horizontally.
If the mouse is being clicked on the far left, or far right of the
mode-line.  This is only useful for non-XEmacs"
  (interactive "e")
  (let* ((click-col (tree-buffer-get-event-column e t)))
    (cond ((< click-col 4)
	   (tree-buffer-hscroll (- (tree-buffer-spec->hor-scroll-step tree-buffer-spec))))
	  ((> click-col (- (window-width) 4))
	   (tree-buffer-hscroll (tree-buffer-spec->hor-scroll-step tree-buffer-spec)))
          (t (tree-buffer-nolog-message
	      "Click on the edge of the modeline to scroll left/right")))
    ))

(defun tree-buffer-count-subnodes-to-display (node)
  "Returns the number of ALL subnodes of NODE which will currently be displayed
if NODE is expanded, means the number of all the children of NODE \(if NODE is
expanded) plus recursive the number of the children of each expanded child.
Example:
\[-] NODE
    \[+] child 1
    \[-] child 2
        \[+] child 2.1
        \[-] child 2.2
            \[+] child 2.2.1
            \[+] child 2.2.2
        \[+] child 2.3
    \[-] child 3
        \[+] child 3.1
    \[+] child 4
The result for NODE here is 10"
  (let ((result 0))
    (when (and (tree-node->expandable node)
               (tree-node->expanded node))
      (setq result (+ result (length (tree-node->children node))))
      (dolist (child (tree-node->children node))
        (setq result (+ result (tree-buffer-count-subnodes-to-display child)))))
    result))

(defun tree-buffer-recenter (node window)
  "If NODE is not visible then first recenter the window WINDOW so NODE is
best visible, means NODE is displayed in the middle of the window if possible.
If NODE is expanded then recenter the WINDOW so as much as possible subnodes
of NODE will be visible. If NODE is not expandable then WINDOW is always
displayed without empty-lines at the end, means WINDOW is always best filled."
  (let* ((node-points (save-excursion
                        (tree-buffer-goto-line (tree-buffer-displayed-node-linenr node))
                        (cons (tree-buffer-line-beginning-pos)
                              (tree-buffer-line-end-pos))))
         (node-point (car node-points))
         (point-lines-before (count-lines (point-min) node-point))
         (point-lines-after (1- (count-lines node-point (point-max)))))
    ;; first make point best visible, means display node in the middle of the
    ;; window if possible (if there are enough lines before/after the node).
    (when (not (pos-visible-in-window-p node-point window))
      (if (< node-point (window-start window))
          (set-window-start
           window
           (save-excursion
             (goto-char node-point)
             (forward-line
              (* -1 (min point-lines-before
                         (/ (tree-buffer-window-display-height window) 2))))
             (tree-buffer-line-beginning-pos)))
        (set-window-start window
                          (save-excursion
                            (goto-char (window-start window))
                            (forward-line
                             (- (+ 1
                                   (count-lines (window-start window) node-point)
                                   (min point-lines-after
                                        (/ (tree-buffer-window-display-height window) 2)))
                                (tree-buffer-window-display-height window)))
                            (tree-buffer-line-beginning-pos)))
        ))
    ;; now optimize the window display for displaying as much possible
    ;; subnodes of node.
    (if (tree-node->expanded node)
        (let ((exp-node-children-count (1+ (tree-buffer-count-subnodes-to-display node)))
              (point-window-line (count-lines (window-start window) node-point)))
          ;; if the current node is not already displayed in the first line of
          ;; the window (= condition 1) and if not all of it´s children are
          ;; visible in the window then we can do some optimization.
          (if (and (save-excursion
                     (goto-char node-point)
                     (forward-line -1)
                     (pos-visible-in-window-p (point) window))
                   (not (save-excursion
                          (goto-char node-point)
                          (forward-line exp-node-children-count)
                          (pos-visible-in-window-p (point) window))))
              ;; optimize the display of NODE and it´s children so as much as
              ;; possible are visible.
              (set-window-start window
                                (save-excursion
                                  (goto-char (window-start window))
                                  (forward-line
                                   (min point-window-line
                                        (- (+ 1
                                              ;; Cause of a bug in GNU Emacs
                                              ;; <= 21.3 we would need here an
                                              ;; extra +1 to display all
                                              ;; subnodes (otherwise the last
                                              ;; one is not displayed). But
                                              ;; this extra +1 would only be
                                              ;; needed if the tree-buffer is
                                              ;; not completely displayed in
                                              ;; the frames default font. But
                                              ;; we accept this because with
                                              ;; GNU Emacs >= 21.4 it seems to
                                              ;; be fixed.
                                              point-window-line
                                              exp-node-children-count)
                                           (tree-buffer-window-display-height window))))
                                  (tree-buffer-line-beginning-pos)))))
      ;; maybe there are empty lines in the window after the last non-empty
      ;; line. If they are we scroll until the whole window is filled with
      ;; non-empty lines.
      (if nil; (not (tree-node->expandable node))
          (let ((w-height (tree-buffer-window-display-height window))
                (full-lines-in-window (count-lines (window-start window)
                                                   (window-end window t))))
            (if (< full-lines-in-window
                   w-height)
                (set-window-start window
                                  (save-excursion
                                    (goto-char (window-start window))
                                    (forward-line (- full-lines-in-window w-height))
                                    (tree-buffer-line-beginning-pos)))))))
    (unless tree-buffer-running-xemacs
      (ignore-errors (tree-buffer-hscroll -1000)))
    ;; KB: testcode
;;     (if (and (not tree-buffer-running-xemacs)
;;              (not (tree-buffer-pos-hor-visible-p (cdr node-points) window)))
;;         (ignore-errors (tree-buffer-hscroll -1000)))
    ))

(defun tree-buffer-remove-highlight ()
  "Unhighlight the currently highlighted tree-node."
  (when tree-buffer-highlighted-node
    (tree-buffer-overlay-delete tree-buffer-highlight-overlay))
  (setq tree-buffer-highlighted-node nil))

(defun tree-buffer-highlight-node-by-data/name (node-data &optional node-name start-node
                                                          dont-make-visible)
  "Highlights in current tree-buffer the node which has as data NODE-DATA. If
START-NODE is nil or equal to the root-node then all nodes of current
tree-buffer are searched from beginning until the node with data NODE-DATA has
been found otherwise the search starts with START-NODE. If DONT-MAKE-VISIBLE
is true then no tree-buffer recentering has been done to make this node
visible.

If optional argument NODE-NAME is not nil then it must be string
and a node will only be highlighted if not only NODE-DATA matches
but also NODE-NAME.

If either NODE-DATA is nil or if the node belonging to NODE-DATA
\(and NODE-NAME, if set) can not be found because it is invisible
\(probably because its parent-node is not expanded) then no
highlighting takes place but the existing highlighting is removed
and nil is returned. Otherwise the node is highlighted and not
nil is returned."
  (if node-data
      (let ((node (tree-buffer-find-displayed-node-by-data/name node-data
                                                                node-name start-node))
            (w (get-buffer-window (current-buffer))))
        (if (null node)
            (progn
              ;; node can not be found because maybe the node is a subnode and
              ;; it's parent is not expanded --> then there is no node for
              ;; NODE-DATA; therefore we must remove the highlighting
              (tree-buffer-remove-highlight)
              nil)
          (setq tree-buffer-highlighted-node (list node-data node-name node))
          (save-excursion
            (tree-buffer-overlay-move tree-buffer-highlight-overlay
                                      (tree-buffer-get-node-name-start-point node)
                                      (tree-buffer-get-node-name-end-point node)))
          (when (not dont-make-visible)
            ;; make node visible if not and optimize the windows display for
            ;; the node.
            (tree-buffer-recenter node w))
          ;; we have highlighted the node so we return not nil.
          t))
    (tree-buffer-remove-highlight)
    nil))

(defun tree-buffer-help-echo-fn (win obj pos)
  "This function is the value of the `help-echo' property of each
tree-node. This is only used with GNU Emacs 21!"
  (let* ((window win)
         (position pos)
         (buffer (window-buffer window))
         node)
    (save-excursion
      (set-buffer buffer)
      (setq node (tree-buffer-get-node-at-point position))
      (and (tree-buffer-spec->node-mouse-over-fn tree-buffer-spec)
           node
           (funcall (tree-buffer-spec->node-mouse-over-fn tree-buffer-spec)
                    node window 'no-print)))))

(defun tree-buffer-merge-face-into-text (face start end &optional text)
  "Merge FACE to the already precolored TEXT so the values of all
face-attributes of FACE take effect and the values of all face-attributes
of TEXT which are not set by FACE are preserved."
  (if (null face)
      text
    (if tree-buffer-running-xemacs
        (put-text-property start end 'face
                           (let* ((current-face (get-text-property 0
                                                                   'face
                                                                   text))
                                  (cf
                                   (typecase current-face
                                     (tree-buffer-face (list current-face))
                                     (list current-face)
                                     (otherwise nil)))
                                  (nf
                                   (typecase face
                                     (tree-buffer-face (list face))
                                     (list face)
                                     (otherwise nil))))
                             ;; we must add the new-face in front of
                             ;; current-face to get the right merge!
                              (if (member face cf)
                                  cf
                                (append nf cf)
                                )
                              )
                           text)
      (alter-text-property start end 'face
                           (lambda (current-face)
                             (let ((cf
                                    (typecase current-face
                                      (tree-buffer-face (list current-face))
                                      (list current-face)
                                      (otherwise nil)))
                                   (nf
                                    (typecase face
                                      (tree-buffer-face (list face))
                                      (list face)
                                      (otherwise nil))))
                               ;; we must add the new-face in front of
                               ;; current-face to get the right merge!
                               (if (member face cf)
                                   cf
                                 (append nf cf))))
                           text))))

(defun tree-buffer-insert-text (text &optional facer help-echo mouse-highlight)
  "Insert TEXT at point and faces it with FACER. FACER can be a face then the
text gets this face or it can be a function-symbol which is called to face the
inserted TEXT. Such a function gets two arguments: Point where TEXT has been
inserted and the TEXT itself"
  (when (stringp text)
    (let ((p (point)))
      (insert text)
      (if mouse-highlight
          (put-text-property p (point) 'mouse-face 'highlight))
      (if (and help-echo (not tree-buffer-running-xemacs))
          (put-text-property p (point) 'help-echo
                             'tree-buffer-help-echo-fn))
      (if facer
          (if (functionp facer)
              (funcall facer p text)
            (tree-buffer-merge-face-into-text facer p (point))))
      )))


(defun tree-buffer-node-display-name (node)
  "Computes that string which is used to display the name of NODE. The
display-name will be set in the slot DISPLAYED-NAME of NODE and also
returned."
  (let* ((ww (window-width))
	 (display-name (tree-node->name node))
	 (width (+ (tree-node-indentlength node)
		   (length display-name)
		   (if (tree-node->expandable node) 4 0))))
    ;; Truncate name if necessary
    (when (and (>= width ww)
               (> (length display-name)
                  (+ (if tree-buffer-running-xemacs 5 4) ;; for the "..." + space
                     (- width ww)
                     3))) ;; there should at least remain 3 visible chars of name
      (if (eq 'beginning (tree-node->shrink-name node))
	  (setq display-name
                (concat "..."
                        (substring display-name (+ (if tree-buffer-running-xemacs 5 4)
                                                   (- width ww)))))
	(if (and (not (tree-buffer-spec->expand-symbol-before-p tree-buffer-spec))
		 (tree-node->expandable node)
		 (eq 'end (tree-node->shrink-name node)))
	    (setq display-name
                  (concat (substring display-name 0
                                     (- (+ (if tree-buffer-running-xemacs 5 4)
                                           (- width ww))))
                          "...")))))
    (setf (tree-node->displayed-name node) display-name)
    display-name))
  
(defun tree-buffer-insert-node-display (node &optional no-newline)
  "Insert NODE into the tree-buffer with all necessary buttons before or after
the name of the NODE. This function computes also the name how the NODE has to
be displayed and returns this name. If optional arg NO-NEWLINE is not nil then
no final newline is displayed after inserting the node. Otherwise always a
newline is inserted after the node."
  (let* ((node-type (tree-node->type node))
         (tree-image-name (if (and (tree-node->expanded node)
                                   (tree-node->expandable node))
                              "open"
                            (if (not (tree-node->expandable node))
                                (if (member node-type
                                            (tree-buffer-spec->maybe-empty-node-types
                                             tree-buffer-spec))
                                    "empty"
                                  (if (member node-type
                                              (tree-buffer-spec->leaf-node-types tree-buffer-spec))
                                      "leaf"
                                    nil))
                              "close")))
         (ascii-symbol (tree-buffer-ascii-symbol-4-image-name tree-image-name))
         (display-name (tree-buffer-node-display-name node))
         (mouse-highlight (or (equal t (tree-buffer-spec->mouse-highlight-fn tree-buffer-spec))
                              (and (tree-buffer-spec->mouse-highlight-fn tree-buffer-spec)
                                   (funcall (tree-buffer-spec->mouse-highlight-fn tree-buffer-spec)
                                            node)))))
    (when (and (tree-buffer-spec->expand-symbol-before-p tree-buffer-spec)
	       ascii-symbol tree-image-name)
      (tree-buffer-insert-text 
       (tree-buffer-add-image-icon-maybe
        0 (length ascii-symbol)
        ascii-symbol (tree-buffer-find-image tree-image-name))
       nil nil mouse-highlight)
      (if (or tree-buffer-enable-xemacs-image-bug-hack
              (not (equal 'image (tree-buffer-real-style))))
          (insert " ")))
    (tree-buffer-insert-text display-name
                             (tree-buffer-get-node-facer node)
                             t mouse-highlight)
    (when (and (not (tree-buffer-spec->expand-symbol-before-p tree-buffer-spec))
	       ascii-symbol)
      (insert " ")
      (tree-buffer-insert-text ascii-symbol nil nil mouse-highlight))
    (unless no-newline (insert "\n"))
    display-name))

(defun tree-buffer-gen-guide-strings ()
  "Returns a list with four elements - the correct guide-strings for current
tree-buffer: \(guide-str-handle guide-str-no-handle guide-end-str no-guide-str)"
  (if (equal 'ascii-no-guides (tree-buffer-real-style))
      (make-list 4 (make-string (tree-buffer-spec->tree-indent
                                 tree-buffer-spec) ? ))
    (let* ((indent-fill-up (make-string
                          (- (tree-buffer-spec->tree-indent tree-buffer-spec)
                             (cond ((equal 'image (tree-buffer-real-style))
                                    tree-buffer-indent-with-images)
                                   ((tree-buffer-spec->expand-symbol-before-p
                                     tree-buffer-spec)
                                    tree-buffer-indent-w/o-images-before-min)
                                   (t
                                    tree-buffer-indent-w/o-images-after-min)))
                          ? ))
           (guide-str-handle (concat (tree-buffer-ascii-symbol-4-image-name
                                      "guide")
                                     (tree-buffer-ascii-symbol-4-image-name
                                      "handle")
                                     indent-fill-up))
           (guide-str-no-handle (concat (tree-buffer-ascii-symbol-4-image-name
                                         "guide")
                                        (tree-buffer-ascii-symbol-4-image-name
                                         "no-handle")
                                        indent-fill-up))
           (guide-end-str (concat (tree-buffer-ascii-symbol-4-image-name
                                   "end-guide")
                                  (tree-buffer-ascii-symbol-4-image-name
                                   "handle")
                                  indent-fill-up))
           (no-guide-str (concat (tree-buffer-ascii-symbol-4-image-name
                                  "no-guide")
                                 (tree-buffer-ascii-symbol-4-image-name
                                  "no-handle")
                                 indent-fill-up)))
      (list guide-str-handle guide-str-no-handle guide-end-str no-guide-str))))

(defun tree-buffer-add-node (node indent-str-first-segs indent-str-last-seg
                                  &optional last-children)
  "Insert NODE in current tree-buffer at point.
The indentation is the concatenation of INDENT-STR-FIRST-SEGS and
INDENT-STR-LAST-SEG. If LAST-CHILDREN is not nil then NODE is the last
children of its parent-node; this means it must be displayed with an
end-guide."
  ;; here we save the indentstr in the node itself - we do this as first step
  ;; so all following steps can use the indentstr from the node itself
  (when (tree-buffer-spec->ascii-guide-face tree-buffer-spec)
    (put-text-property 0 (length indent-str-first-segs)
                       'face (tree-buffer-spec->ascii-guide-face tree-buffer-spec)
                       indent-str-first-segs)
    (put-text-property 0 (length indent-str-last-seg)
                       'face (tree-buffer-spec->ascii-guide-face tree-buffer-spec)
                       indent-str-last-seg))
  (setf (tree-node->indentstr node)
        (concat indent-str-first-segs indent-str-last-seg))

  ;; insert the node indentation
  (insert (tree-node->indentstr node))
  ;; insert the node with all its symbols - either as image or ascii and add
  ;; the node to the `tree-buffer-displayed-nodes'
  (tree-buffer-insert-node-display node)
  (tree-buffer-add-to-displayed-nodes node)
  
  ;; compute the indentation-strings for the children and run recursive for
  ;; each child
  (if (tree-node->expanded node)
      (let* ((number-of-childs (length (tree-node->children node)))
             (counter 0)
             (guide-strings (tree-buffer-gen-guide-strings))
             (guide-str (if (and (equal 'image (tree-buffer-real-style))
                                 tree-buffer-enable-xemacs-image-bug-hack)
                            (nth 0 guide-strings)
                          (nth 1 guide-strings)))
             (guide-end-str (nth 2 guide-strings))
             (no-guide-str (nth 3 guide-strings))
             (indent-str-last-seg-copy (copy-sequence indent-str-last-seg))
             (next-indent-str-first-segs
              (if (= 0 (length indent-str-last-seg-copy))
                  ""
                (concat indent-str-first-segs
                        (if last-children
                            (tree-buffer-add-image-icon-maybe
                             2 1
                             (tree-buffer-add-image-icon-maybe
                              0 2 no-guide-str
                              (tree-buffer-find-image "no-guide"))
                             (tree-buffer-find-image "no-handle"))
                          (tree-buffer-add-image-icon-maybe
                           2 1
                           (tree-buffer-aset
                            indent-str-last-seg-copy
                            (1- (cond ((equal 'image (tree-buffer-real-style))
                                       tree-buffer-indent-with-images)
                                      ((tree-buffer-spec->expand-symbol-before-p
                                        tree-buffer-spec)
                                       tree-buffer-indent-w/o-images-before-min)
                                      (t
                                       tree-buffer-indent-w/o-images-after-min)))
                            ? )
                           (tree-buffer-find-image "no-handle"))))))
             (next-indent-str-last-seg-std
              (tree-buffer-add-image-icon-maybe
               2 1
               (tree-buffer-add-image-icon-maybe
                0 2 guide-str
                (tree-buffer-find-image "guide"))
               (tree-buffer-find-image "handle")))
             (next-indent-str-last-seg-end
              (tree-buffer-add-image-icon-maybe
               2 1
               (tree-buffer-add-image-icon-maybe
                0 2 guide-end-str
                (tree-buffer-find-image "end-guide"))
               (tree-buffer-find-image "handle"))))
        (dolist (node (tree-node->children node))
          (setq counter (1+ counter))
          (tree-buffer-add-node node
                                next-indent-str-first-segs
                                (if (= counter number-of-childs )
                                    next-indent-str-last-seg-end
                                  next-indent-str-last-seg-std)
                                (= counter number-of-childs ))))))

(defun tree-buffer-update-node (node name shrink-name type data expandable
                                     &optional redisplay)
  "This function updates the NODE with the new datas NAME, SHRINK-NAME, TYPE,
DATA and EXPANDABLE. If NODE is nil then the node at current point will be
updated. Each of the arguments NAME, SHRINK-NAME, TYPE, DATA and EXPANDABLE
can have the special value 'use-old-value\; this means that attribute of NODE
will not be updated. If first optional arg REDISLAY is not nil then NODE will
be completely redisplayed according to its new data. Nil for REDISLAY makes
sense for example if the caller wants to update a bunch of nodes but wants to
update the display itself first at the end of all node-updates \(for
efficiency). In that case the caller has to ensure that `tree-buffer-update'
is called after updating all needed nodes."
  (let* ((my-node (or node (tree-buffer-get-node-at-point)))
         (node-line (when redisplay
                      ;; Klaus Berndl <klaus.berndl@sdm.de>: We could simply
                      ;; here call (tree-buffer-displayed-node-linenr
                      ;; my-node) but for best possible performance we just
                      ;; use the current linenumber if NODE is nil (means we
                      ;; stay already at the right point and there is no need
                      ;; to waste performance by searching a node we have
                      ;; already "found"...maybe paranoid ;-)
                      (if node
                          (tree-buffer-displayed-node-linenr node)
                        (tree-buffer-current-line))))
         (old-node-data (tree-node->data my-node))
         (old-node-name (tree-node->name my-node))
         (buffer-read-only nil))
    (tree-node-update my-node name type data expandable shrink-name)
    (when node-line ;; we want a redisplay
      (save-excursion
        (tree-buffer-goto-line node-line)
        (beginning-of-line)
        (delete-region (tree-buffer-line-beginning-pos)
                       (tree-buffer-line-end-pos))
        (insert (tree-node->indentstr my-node))
        (tree-buffer-insert-node-display my-node 'no-newline)
        ;; There is no need to update the displayed-node list because we have
        ;; already updated the node-object and this node-object is part of the
        ;; displayed-node list ==> this list is automatically up-to-date now.
        
        ;; rehighlight here the current highlighted node again - this is
        ;; necessary if we have unpdated and redisplayed the currently
        ;; highlighted node. For this check we have to compare the
        ;; old-node-data/name (before the update!) with that node-data/name
        ;; stored in `tree-buffer-highlighted-node' - but the rehighlight has
        ;; to be done with the new node-data/name (after the update) because the
        ;; node is already updated so the node is only findable via the new
        ;; node-data/name!
        (when (tree-buffer-highlighted-node-matches-data/name-p old-node-data old-node-name)
          (tree-buffer-highlight-node-by-data/name (tree-node->data my-node)
                                                   (tree-node->name my-node)
                                                   nil t))))
    ))

(defun tree-buffer-clear-tree ()
  "Clear current tree-buffer, i.e. remove all children of the root-node"
  (dolist (child (tree-node->children (tree-buffer-get-root)))
    (tree-buffer-remove-node child)))

(defun tree-buffer-remove-node (node &optional redisplay empty-parent-types)
  "Remove NODE from current tree-buffer. If NODE is nil then the node at
current point will be removed. If NODE equal the node returned by
`tree-buffer-get-root' then nothing will be done. If first optional arg
REDISLAY is not nil then NODE will be also completely removed from the
tree-display otherwise only from the internal tree-structure. If second
optional arg EMPTY-PARENT-TYPES is not nil and a list of node-types \(see
`tree-buffer-create') and if the node-type of the parent of NODE is contained
in EMPTY-PARENT-TYPES and if NODE is the only children of its parent then its
parent is recursively removed too."
  (let ((my-node (or node (tree-buffer-get-node-at-point))))
    (when (and my-node (not (eq (tree-buffer-get-root) my-node)))
      (let* ((parent (tree-node->parent my-node))
             (parent-type (tree-node->type parent)))
        ;; If parent is the root-node then its type is always -1 (only the
        ;; root-node has type -1) and therefore then the recursion stops here
        ;; savely.
        (if (and (member parent-type empty-parent-types)
                 (= (length (tree-node->children parent)) 1))
            (tree-buffer-remove-node parent redisplay empty-parent-types)
          (tree-node-remove-child parent my-node)
          (when redisplay
            (let ((buffer-read-only nil)
                  (node-line (when redisplay
                               (if node
                                   (tree-buffer-displayed-node-linenr my-node)
                                 (tree-buffer-current-line)))))
              (when node-line
                (save-excursion
                  (tree-buffer-goto-line node-line)
                  (beginning-of-line)
                  (delete-region (tree-buffer-line-beginning-pos)
                                 (1+ (tree-buffer-line-end-pos))))
                (tree-buffer-displayed-nodes-remove-node my-node)
                ))))))))

(defun tree-buffer-build-tree-buffer-display ()
  "Rebuild the variable `tree-buffer-displayed-nodes' from the current
children of `tree-buffer-root'. This also builds the display of current
tree-buffer from scratch. This functions expects the current tree-buffer to be
empty!"
  (tree-buffer-initialize-displayed-nodes)
  (dolist (node (tree-node->children tree-buffer-root))
    (tree-buffer-add-node node "" "")))

(defun tree-buffer-display-in-general-face ()
  "Apply the face in slot GENERAL-FACE of `tree-buffer-spec' of current
tree-buffer to current tree-buffer."
  (when (tree-buffer-spec->general-face tree-buffer-spec)
    (tree-buffer-overlay-move tree-buffer-general-overlay
                              (point-min) (point-max))))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Should be also check if
;; tree-buffer-displayed-nodes is nil - if all things run well this check must
;; not fail.
(defun tree-buffer-empty-p ()
  "Return not nil if current tree-buffer is empty."
  (= (point-min) (point-max)))

(defun tree-buffer-run-after-update-hook ()
  "Run all functions of slot AFTER-UPDATE-HOOK of `tree-buffer-spec'."
  (dolist (f (tree-buffer-spec->after-update-hook tree-buffer-spec))
    (funcall f)))

(defun tree-buffer-update (&optional node content)
  "Updates the current tree-buffer. The buffer will be completely rebuild with
it's current nodes. Window-start and point will be preserved. If NODE is not
nil and a valid and expanded node with at least one child then the display of
this node is optimized so the node itself and as much as possible of it's
children \(and also recursive the children of a child if it's already
expanded, see `tree-buffer-count-subnodes-to-display') are visible in current
tree-buffer. If CONTENT is not nil then it must be a cons-cell where the car
is the whole string of the tree-buffer and the cdr is the value of
`tree-buffer-displayed-nodes'. Then the content of the tree-buffer will not be
rebuild by reinserting all nodes from the tree-node-structure but just by
inserting the car of CONTENT in the tree-buffer and setting
`tree-buffer-displayed-nodes' to cdr of CONTENT."
  (let* ((w (get-buffer-window (current-buffer)))
         (ws (window-start w))
         (p (point))
         (buffer-read-only nil)
         (next-line-add-newlines nil))
    (erase-buffer)
    (if (consp content)
        (progn
          (insert (car content))
          (tree-buffer-set-displayed-nodes (cdr content)))
      (tree-buffer-build-tree-buffer-display))
    (tree-buffer-display-in-general-face)
    (tree-buffer-highlight-node-by-data/name
     (or nil ;;(and node (tree-node->data node))
         (nth 0 tree-buffer-highlighted-node))
     (nth 1 tree-buffer-highlighted-node)
     (nth 2 tree-buffer-highlighted-node)
     nil)
    (goto-char p)
    (set-window-start w ws)
    ;; let's optimize the display of the expanded node NODE and it's children.
    (when node
      (tree-buffer-recenter node w))
    (tree-buffer-run-after-update-hook)))


(defun tree-buffer-scroll-window (point window-start)
  "Scrolls current tree-buffer. The window will start at WINDOW-START and
point will stay on POINT."
  (goto-char point)
  (set-window-start (get-buffer-window (current-buffer)) window-start))

(defun tree-buffer-expand-node (node level
                                     &optional expand-pred-fn collapse-pred-fn)
  "Expand the NODE up to an expand-level of LEVEL.

LEVEL specifies precisely which level of nodes should be expanded. LEVEL means
the indentation-level of the NODE itself and its \(recursive) subnodes
relative to the NODE itself.

A LEVEL value X means that all \(sub)nodes with an indentation-level <= X
relative to NODE are expanded and all other are collapsed. A negative LEVEL
value means that NODE is collapsed.

Examples:

- LEVEL = 0: If NODE is the root-node then this means expand only all nodes
  with no indentation at all. If NODE is any other node then this means expand
  only the NODE itself because it is the only node which has indentation 0 to
  itself. All deeper indented nodes will be collapsed.

- LEVEL = 1: If NODE is the root-node then this means expand all nodes with no
  indentation at all and all subnodes of these nodes - all deeper indented
  nodes will be collapsed. If NODE is any other node then this means expand
  the NODE itself and all of its direct subnodes - because only the direct
  subnodes of NODE have indentation-level 1 relativ to NODE.

This function expands beginning from NODE the NODE itself and all subnodes of
NODE with level <= LEVEL, so the subnodes of these nodes get visible and
collapses all their \(recursive) subnodes with indentation-level > LEVEL.

If a node has to be expanded then first the function in SLOT NODE-EXPANDED-FN
of `tree-buffer-spec' of current tree-buffer \(see `tree-buffer-create') is
called with the argument-values \[node 0 nil nil \(buffer-name)\].

This function gets two optional function-arguments which are called to test if
a node should be excluded from expanding or collapsing; both functions are
called with two arguments, where the first one is the expandable/collapsable
node and the second one is the current level of indentation of this node
relativ to the startnode NODE: EXPAND-PRED-FN is called if a node has to be
expanded and must return nil if this node should not be expanded even if its
indentation level is <= LEVEL and COLLAPSE-PRED-FN is called analogous for a
node which has to be collapsed and must return nil if the node should not be
collapsed even if its indentation level is > then LEVEL.

Examples:
- LEVEL = -1 collapses the NODE.
- LEVEL = 0 expands only the NODE itself because it is the only node which can
  have no indentation relativ to itself.
- LEVEL = 2 expands the NODE itself, its children and its grandchildren -
  these are the nodes which are either not indented \(the NODE itself) or
  indented once \(the children) or twice \(the grandchildren)."
  (if (not (equal (tree-buffer-get-root) node))
      (tree-buffer-expand-node-internal node 0 level
                                        expand-pred-fn collapse-pred-fn)))

(defun tree-buffer-expand-node-internal (node current-level level
                                              expand-pred-fn collapse-pred-fn)
  "Expand NODE if CURRENT-LEVEL \(the indentation-level of NODE) <= LEVEL or
collapses NODE if CURRENT-LEVEL > LEVEL. Do this recursive for subnodes of
NODE with incremented CURRENT-LEVEL. For EXPAND-PRED-FN and COLLAPSE-PRED-FN
see `tree-buffer-expand-node'. This function is not for external usage; use
`tree-buffer-expand-node' instead."
  (when (tree-node->expandable node)
    (when (and (tree-buffer-spec->node-expanded-fn tree-buffer-spec)
               (not (tree-node->expanded node)))
      (funcall (tree-buffer-spec->node-expanded-fn tree-buffer-spec)
               node 0 nil nil nil (buffer-name)))
    (when (or (and (not (tree-node->expanded node))
                   (or (not (functionp expand-pred-fn))
                       (funcall expand-pred-fn node current-level))
                   (<= current-level level))
              (and (tree-node->expanded node)
                   (or (not (functionp collapse-pred-fn))
                       (funcall collapse-pred-fn node current-level))
                   (> current-level level)))
      (tree-node-toggle-expanded node))
    (dolist (child (tree-node->children node))
      (tree-buffer-expand-node-internal child (1+ current-level) level
                                        expand-pred-fn collapse-pred-fn))))

(defun tree-buffer-set-root (root)
  "Set the root-node of current tree-buffer to ROOT.
ROOT must be either that root-node automatically created by
`tree-buffer-create' or a node returned by `tree-node-new-root'!"
  (setq tree-buffer-root root)
  (setf (tree-node->expanded tree-buffer-root) t))

(defun tree-buffer-get-root ()
  "Return the root-node of current tree-buffer."
  tree-buffer-root)

(defun tree-buffer-gen-searchpattern-indent-prefix (&optional count)
  (let ((guide-strings (tree-buffer-gen-guide-strings)))
    (concat "^\\("
            (mapconcat (function (lambda (e)
                                   (format "\\(%s\\)" e)))
                       (list (nth 1 guide-strings)
                             (nth 3 guide-strings)
                             (nth 0 guide-strings)
                             (nth 2 guide-strings))
                       "\\|")
            "\\)"
            (if (integerp count)
                (format "\\{%d\\}" count)
              "*"))))


;; idea is stolen from ido.el, written by Kim F. Storm <stormware@get2net.dk>
(defun tree-buffer-find-common-substring (lis subs &optional only-prefix)
  "Return common substring beginning with SUBS in each element of LIS. If
ONLY-PREFIX is not nil then only common prefix is returned."
  (let ((change-word-sub (concat (if only-prefix
                                     (concat "^"
                                             (car (tree-buffer-spec->incr-search-additional-pattern
                                                   tree-buffer-spec)))
                                   "")
                                 "\\(" (regexp-quote subs) "\\)"))
        res alist)
    (setq res
          (mapcar (function (lambda (word)
                              (save-match-data
                                (if (string-match change-word-sub word)
                                    (substring word
                                               (match-beginning
                                                (if (and only-prefix
                                                         (cdr (tree-buffer-spec->incr-search-additional-pattern
                                                               tree-buffer-spec)))
                                                    (1+ (cdr (tree-buffer-spec->incr-search-additional-pattern
                                                              tree-buffer-spec)))
                                                  1)))
                                  ;; else no match
                                  nil))))
                  lis))
    (setq res (delq nil res)) ;; remove any nil elements (shouldn't happen)
    (setq alist (mapcar (function (lambda (r)
                                    (cons r 1)))
                        res)) ;; could use an  OBARRAY
    ;; try-completion returns t if there is an exact match.
    (let ((completion-ignore-case t))
      (try-completion subs alist))))

(defun tree-buffer-incremental-node-search ()
  "Incremental search for a node in current tree-buffer.
Each display-able key \(e.g. all keys normally bound to `self-insert-command')
is appended to the current search-pattern. The tree-buffer tries to jump to
the current search-pattern. If no match is found then nothing is done. Some
special keys:
- \[backspace] and \[delete]: Delete the last character from the search-pattern.
- \[home]: Delete the complete search-pattern
- \[end]: Expand either to a complete node if current search-pattern is
         already unique or expands to the greatest common prefix of the nodes.
         If there are at least two nodes with the same greatest common-prefix
         than every hit of \[end] jumps to the next node with this common
         prefix.

The current search-pattern is shown in the echo area.
After selecting a node with RET the search-pattern is cleared out.

Do NOT call this function directly. It works only if called from the binding
mentioned above!"
  (interactive)
  (unless (not (equal (selected-frame) tree-buffer-frame))
    (let ((last-comm (tree-buffer-event-to-key last-command-event))
          (full-search-regexp nil))
      (case last-comm
        ((delete backspace)
         ;; reduce by one from the end
         (setq tree-buffer-incr-searchpattern
               (substring tree-buffer-incr-searchpattern
                          0
                          (max 0 (1- (length tree-buffer-incr-searchpattern))))))
        ;; delete the complete search-pattern
        (home
         (setq tree-buffer-incr-searchpattern ""))
        ;; expand to the max. common prefix
        (end
         (let* ((node-name-list (tree-buffer-map-displayed-nodes
                                 'tree-node->name))
                (common-prefix (tree-buffer-find-common-substring
                                node-name-list tree-buffer-incr-searchpattern
                                (if (equal (tree-buffer-spec->incr-search-p tree-buffer-spec)
                                           'prefix)
                                    t))))
           (if (stringp common-prefix)
               (setq tree-buffer-incr-searchpattern common-prefix))))
        ((nil) nil) ;; do nothing
        (otherwise
         ;; add the last command to the end
         (setq tree-buffer-incr-searchpattern
               (concat tree-buffer-incr-searchpattern
                       (char-to-string last-comm)))))
      (setq full-search-regexp
            (concat tree-buffer-incr-searchpattern-indent-prefix
                    tree-buffer-incr-searchpattern-expand-prefix
                    (car (tree-buffer-spec->incr-search-additional-pattern
                          tree-buffer-spec))
                    (if (equal (tree-buffer-spec->incr-search-p tree-buffer-spec)
                               'substring)
                        "[^()\n]*"
                      "")
                    (regexp-quote tree-buffer-incr-searchpattern)))
      (setq tree-buffer-incr-searchpattern
            (or (funcall (tree-buffer-spec->reduce-tree-for-incr-search-fn tree-buffer-spec)
                         tree-buffer-incr-searchpattern full-search-regexp)
                ;; Only if the reduce-function is 'ignore we get nil and then
                ;; we do not change the `tree-buffer-incr-searchpattern'
                tree-buffer-incr-searchpattern))
      (tree-buffer-nolog-message
       "%s node search: [%s]%s"
       (buffer-name (current-buffer))
       tree-buffer-incr-searchpattern
       (if (save-excursion
             (if (not (and (equal last-comm 'end)
                           (string= tree-buffer-incr-searchpattern
                                    tree-buffer-last-incr-searchpattern)))
                 (goto-char (point-min)))
             (re-search-forward full-search-regexp nil t))
           ;; we have found a matching ==> jump to it
           (progn
             (goto-char (match-end 0))
             "")
         " - no match"))
      ;; lets save the search-pattern so we can compare it with the next one.
      (setq tree-buffer-last-incr-searchpattern tree-buffer-incr-searchpattern))))

(defun tree-buffer-create-menu-emacs (menu-def menu-name)
  "Create an Emacs-menu for MENU-DEF with name MENU-NAME.
MENU-DEF must have the same format as the first argument of
`tree-buffer-create-menu'."
  (let ((map (make-sparse-keymap menu-name))
        (counter 0)
        (menu-items (reverse menu-def)))
    (dolist (item menu-items)
      (cond ((string= (car item) "---")
             (define-key map
               (make-vector 1
                            (setq counter (1+ counter)))
               (list "---")))
            ((stringp (cadr item)) ;; menu-entry
             (define-key map
               (make-vector 1
                            (setq counter (1+ counter)))
               (cons (cadr item) (car item))))
            (t ;; submenu
             (define-key map
               (make-vector 1
                            (setq counter (1+ counter)))
               (cons (car item)
                     (tree-buffer-create-menu-emacs (cdr item) (car item)))))))
    map))


(defun tree-buffer-create-menu-xemacs (menu-def &optional node-commands-p)
  "Create a XEmacs-menu for MENU-DEF.
If optional arg NODE-COMMANDS-P is not nil then the menu-commands will be
called with the current node at point. Otherwise the menu-commands will be
called with no argument. MENU-DEF must have the same format as the first
argument of `tree-buffer-create-menu'."
  (when menu-def
    (let ((item (car menu-def)))
      (cons (cond ((string= (car item) "---")
		   (car item))
		  ((stringp (cadr item)) ;; menu-entry
                   (let ((v (make-vector 3 t)))
                     (aset v 0 (cadr item))
                     (aset v 1 (delq nil
                                     (list (car item)
                                           (and node-commands-p
                                                '(tree-buffer-get-node-at-point)))))
                     (aset v 2 t)
                     v))
		  (t ;; submenu
                   `(,(car item)
                     ,@(tree-buffer-create-menu-xemacs (cdr item)
                                                       node-commands-p))))
	    (tree-buffer-create-menu-xemacs (cdr menu-def) node-commands-p)))))

(defun tree-buffer-create-menu (menu-items &optional node-commands-p)
  "Creates a popup menu from the list MENU-ITEMS.
MENU-ITEMS is a list of elements of the following type: Each element defines a
new menu-entry and is either:

a) Menu-command: A list containing two sub-elements, whereas the first is the
   function \(a function symbol) being called if the menu-entry is selected
   and the second is the name of the menu-entry.
b) Separator: A one-element-list and the element is the string \"---\": Then a
   non-selectable menu-separator is displayed.
c) Submenu: A list where the first element is the title of the submenu
   displayed in the main-menu and all other elements are either menu-commands
   \(see a) or separators \(see b) or another submenu \(see c). This allows
   deep nested menu-submenu-structures!

If optional arg NODE-COMMANDS-P is not nil then the function of a
menu-commands will be called with a tree-node argument. Otherwise the
menu-commands will be called with no argument.

If NODE-COMMANDS-P is not nil then the function of a menu-command must follow
the following guidelines: Such a function must be defined with the macro
`tree-buffer-defpopup-command'! This macro defines a new popup-command whereas
the newly defined command gets one argument NODE. See the docstring of
`tree-buffer-defpopup-command' for further details.

Example for the definition of such a popupmenu-command:

\(tree-buffer-defpopup-command ecb-my-special-dir-popup-function
  \"Prints the name of the directory of the node under point.\"
  \(let \(\(node-data=dir \(tree-node->data node)))
     \(message \"Dir under node: %s\" node-data=dir)))"
  (when menu-items
    (if tree-buffer-running-xemacs
        (tree-buffer-create-menu-xemacs menu-items node-commands-p)
      (tree-buffer-create-menu-emacs menu-items "dummy-name"))))


(defun tree-buffer-create-menus (menus &optional node-commands-p)
  "Creates a popup menus from an assoc list with menus.
MENUS is an assoc list containing cons-cells of the form:
The car is a node-type \(see slot TYPE of a tree-node) and the cdr is a menu
in the sense of `tree-buffer-create-menu', i.e. the cdr is a list of
menu-items expected as argument by `tree-buffer-create-menu'.

For a description of NODE-COMMAND-P see `tree-buffer-create-menu'."
  (when menus
    (cons (cons (caar menus)
		(tree-buffer-create-menu (cdar menus) node-commands-p))
	  (tree-buffer-create-menus (cdr menus) node-commands-p))))

;; Klaus Berndl <klaus.berndl@sdm.de>: Seems that the docstring of
;; x-popup-menu is wrong because it seems this function needs offsets related
;; to current window not to frame!
;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: For XEmacs this does not work!
(defun tree-buffer-show-node-menu-keyboard (&optional use-tmm)
  "Activate the popup-menu of current tree-buffer via keyboard. If called with
a prefix-arg then the library tmm.el is used for displaying the popup-menu -
ignored with XEmacs."
  (interactive "P")
  (if use-tmm
      (unless (not (equal (selected-frame) tree-buffer-frame))
        (when (tree-buffer-spec->menu-creator tree-buffer-spec)
          (let ((node (tree-buffer-get-node-at-point)))
            (when (and (not tree-buffer-running-xemacs)
                       node
                       (locate-library "tmm"))
              (let ((menu (cdr (assoc (tree-node->type node)
                                      (tree-buffer-create-menus
                                       (funcall (tree-buffer-spec->menu-creator
                                                 tree-buffer-spec)
                                                (buffer-name) node))))))
                (tmm-prompt menu))))))
    (if tree-buffer-running-xemacs
        (tree-buffer-show-node-menu (get-buffer-window (current-buffer)
                                                       tree-buffer-frame))
      (let ((curr-frame-ypos (* (/ (frame-pixel-height) (frame-height))
                                (count-lines (window-start) (point))))
            (curr-frame-xpos (* (/ (frame-pixel-width) (frame-width))
                                (current-column))))
        (tree-buffer-show-node-menu (list (list curr-frame-xpos curr-frame-ypos)
                                          (selected-window)))))))

(defun tree-buffer-popup-menu (event menu menu-title &optional node)
  "Popup a a context menu.
EVENT is the event which has triggered the menu-popup. Note that EVENT is
different for XEmacs and Emacs. For the former one it is an event as needed by
`popup-menu' and for the latter one as needed by `x-popup-menu'. MENU-TITLE is
the string which should be displayed as menu-title. If optional arg NODE is a
tree-node then the selected menu-command will be called with that node as
argument. If NODE is nil then the selected menu-command will be called with no
argument otherwise with NODE as the only argument."
  (if tree-buffer-running-xemacs
      (if (windowp event)
          (popup-menu-and-execute-in-window (cons menu-title menu)
                                            event)
        (popup-menu (cons menu-title menu)))
    ;; we must set the title for the menu-keymap
    (setcar (member (nth (1- (length menu)) menu) menu)
            menu-title)
    (let* ((menu-selection (apply 'vector
                                  (x-popup-menu event menu)))
           (fn (if (and menu-selection
                        (> (length menu-selection) 0))
                   (lookup-key menu menu-selection))))
      (when (functionp fn)
        (if node
            (funcall fn node)
          (funcall fn))))))

(defun tree-buffer-show-node-menu (event)
  "Display a popup-menu for the node at point.
For an description of EVENT see `tree-buffer-popup-menu'."
  (unless (not (equal (selected-frame) tree-buffer-frame))
    (let ((node (tree-buffer-get-node-at-point))
          (menu-creator (tree-buffer-spec->menu-creator tree-buffer-spec)))
      (when (and node (functionp menu-creator))
        (let* ((menu (cdr (assoc (tree-node->type node)
                                 (tree-buffer-create-menus
                                  (funcall (tree-buffer-spec->menu-creator
                                            tree-buffer-spec)
                                           (buffer-name) node)
                                  t))))
               (menu-title-creator
                (cdr (assoc (tree-node->type node)
                            (tree-buffer-spec->menu-titles tree-buffer-spec))))
               (menu-title (typecase menu-title-creator
                             (string menu-title-creator)
                             (function (funcall menu-title-creator node))
                             (otherwise "Tree-buffer-nodemenu"))))
          (when menu
            (tree-buffer-popup-menu event menu menu-title node)))))))

(defun tree-buffer-show-modeline-menu (event)
  "Display a popup-menu for the modeline of current tree-buffer.
For an description of EVENT see `tree-buffer-popup-menu'."
  (unless (not (equal (selected-frame) tree-buffer-frame))
    (let* ((menu-creator (tree-buffer-spec->modeline-menu-creator tree-buffer-spec))
           (menu (and menu-creator (funcall menu-creator (buffer-name)))))
      (when menu
        (tree-buffer-popup-menu event
                                (tree-buffer-create-menu menu)
                                "Tree-buffer modeline-menu")))))

(defmacro tree-buffer-defpopup-command (name docstring &rest body)
  "Define a new popup-command for a tree-buffer.
NAME is the name of the popup-command to create. It will get one optional
argument NODE \(s.b.) and a list of zero or more extra arguments called
REST-ARG-LIST, so the argument-signature of the generated command is
\(&optional node &rest rest-arg-list). DOCSTRING is a documentation string to
describe the function. BODY is the code evaluated when this command is called
from a popup-menu of a tree-buffer.

BODY can refer to NODE which is bound to the node for which this popup-command
is called \(i.h. that node with the point at call-time of this command) and to
REST-ARG-LIST which is a list of zero or more extra arguments. If the
generated command is called by ECB via the popup-mechanism \(or the
tmm-mechanism) then REST-ARG-LIST is always nil. This argument list is to have
the freedom to program such a command more generally so it can not only be
called via popup but also called from some arbitrary elisp-code which can then
call this command with more arguments than only a NODE - if necessary.

With the function `tree-node->data' the related data of NODE is accessible
and returns for example in case of the directories buffer the directory for
which the popup-menu has been opened. The BODY can do any arbitrary things
with this node-data. In general all accessors \(tree-node->*) for a node
can be used.

Example for the usage of this macro:

\(tree-buffer-defpopup-command ecb-my-special-dir-popup-function
   \"Prints the name of the directory of the node under point.\"
  \(let \(\(node-data=dir \(tree-node->data node))
          \(first-arg-of-rest-args \(car rest-arg-list)))
    \(message \"Dir under node: %s\" node-data=dir)))"
  `(eval-and-compile
     (defun ,name (&optional node &rest rest-arg-list)
       ,(concat docstring
                "\n\n"
                "This is a generated command intended to be called via the popup-menu of ECB.\n"
                "The arguments NODE and REST-ARG-LIST can not be inserted in an interactive\n"
                "way by the end-user but are passed from ECB to this command.\n"
                "NODE is that node-object for which this command is called. If NODE is nil\n"
                "then the current node at point in the currently selected tree-buffer is used.\n"
                "REST-ARG-LIST is a list of zero or more extra arguments passed to this command.")
       (interactive)
       (let ((node (if (and (interactive-p) (null node))
                       (tree-buffer-get-node-at-point)
                     node)))
         (when node
           ,@body)))))

(put 'tree-buffer-defpopup-command 'lisp-indent-function 1)


;; mouse tracking stuff for XEmacs - GNU Emacs uses help-echo!

(defun tree-buffer-follow-mouse (event)
  (interactive "e")
  (when tree-buffer-running-xemacs
    (let ((window (tree-buffer-event-window event))
          (current-window (get-buffer-window (current-buffer))))
      (when (and (or (not (window-minibuffer-p current-window))
                     (not (minibuffer-window-active-p current-window)))
                 (windowp window)
                 (member (window-buffer window) tree-buffers))
        (set-buffer (window-buffer window))
        (let ((p (tree-buffer-event-point event)))
          (when (integer-or-marker-p p)
            ;; (unless (not (equal (selected-frame) tree-buffer-frame))
            (let ((node (tree-buffer-get-node-at-point p)))
              (when (and (tree-buffer-spec->node-mouse-over-fn tree-buffer-spec)
                         node)
                (funcall (tree-buffer-spec->node-mouse-over-fn tree-buffer-spec)
                         node (get-buffer-window (current-buffer)))))))))))

(defun tree-buffer-activate-follow-mouse ()
  "Activates that in all tree-buffer-windows - regardless if the active window
or not - a mouse-over-node-function is called if mouse moves over a node. See
also the NODE-MOUSE-OVER-FN argument of `tree-buffer-create'.

This function does nothing for GNU Emacs; with this version this
functionality is done with the `help-echo'-property and the function
`tree-buffer-help-echo-fn'!"
  (when tree-buffer-running-xemacs
    (dolist (buf tree-buffers)
      (save-excursion
        (set-buffer buf)
        (add-hook 'mode-motion-hook 'tree-buffer-follow-mouse)))))

(defun tree-buffer-deactivate-follow-mouse ()
  "Complementary function to `tree-buffer-activate-follow-mouse'."
  (when tree-buffer-running-xemacs
    (dolist (buf tree-buffers)
      (save-excursion
        (set-buffer buf)
        (remove-hook 'mode-motion-hook 'tree-buffer-follow-mouse)))))

;; pressed keys

(defun tree-buffer-tab-pressed ()
  "Perform the defined action after a TAB-hit."
  (interactive)
  (unless (not (equal (selected-frame) tree-buffer-frame))
    (let ((node (tree-buffer-get-node-at-point)))
      (when (tree-node->expandable node)
	(when (and (tree-buffer-spec->node-expanded-fn tree-buffer-spec)
		   (not (tree-node->expanded node)))
	  (funcall (tree-buffer-spec->node-expanded-fn tree-buffer-spec)
                   node 0 nil nil nil (buffer-name)))
        (when (tree-node->expandable node)
          (when (and (tree-node->expanded node)
                     (tree-buffer-spec->node-collapsed-fn tree-buffer-spec))
            (funcall (tree-buffer-spec->node-collapsed-fn tree-buffer-spec)
                     node 0 nil
                     nil nil (buffer-name)))
          (tree-node-toggle-expanded node))
	;; Update the tree-buffer with optimized display of NODE           
	(tree-buffer-update node)))))

(defun tree-buffer-arrow-pressed ()
  "Perform smart arrow-key navigation/movement."
  (interactive)
  (unless (not (equal (selected-frame) tree-buffer-frame))
    (let ((node (tree-buffer-get-node-at-point))
          (arrow-key (tree-buffer-event-to-key last-command-event)))
      (case arrow-key
        (up
         (forward-line -1)
         (beginning-of-line)
         (re-search-forward tree-buffer-incr-searchpattern-indent-prefix nil t))
        (down
         (forward-line 1)
         (beginning-of-line)
         (re-search-forward tree-buffer-incr-searchpattern-indent-prefix nil t))
        (right
         (if (and (tree-node->expandable node)
                  (not (tree-node->expanded node)))
             (tree-buffer-tab-pressed)
           ;; jump to the first subnode
           (forward-line 1)
           (beginning-of-line)
           (re-search-forward tree-buffer-incr-searchpattern-indent-prefix nil t)))
        (left
         (if (tree-node->expanded node)
             (tree-buffer-tab-pressed)
           ;; jump to next higher node
           (let* ((new-indent-factor (/ (max 0 (- (tree-node-indentlength node)
                                                  (tree-buffer-spec->tree-indent
                                                   tree-buffer-spec)))
                                        (tree-buffer-spec->tree-indent tree-buffer-spec)))
                  (search-string
                   (concat (tree-buffer-gen-searchpattern-indent-prefix new-indent-factor)
                           "[^ `|]")))
             (re-search-backward search-string nil t)
             (beginning-of-line)
             (re-search-forward tree-buffer-incr-searchpattern-indent-prefix nil t))))))))


;; headerline stuff
;; stolen from cedets stickyfunc-minor-mode - thanks to Eric ;-)

(defun tree-buffer-sticky-default-indent-string ()
  (if (and window-system (not tree-buffer-running-xemacs))
      (concat
       (condition-case nil
	   ;; Test scroll bar location
	   (let ((charwidth (tree-buffer-frame-char-width))
		 (scrollpos (frame-parameter (selected-frame)
					     'vertical-scroll-bars))
		 )
	     (if (or (eq scrollpos 'left)
		     ;; Now wait a minute.  If you turn scroll-bar-mode
		     ;; on, then off, the new value is t, not left.
		     ;; Will this mess up older emacs where the default
		     ;; was on the right?  I don't think so since they don't
		     ;; support a header line.
		     (eq scrollpos t))
		 (let ((w (when (boundp 'scroll-bar-width)
			    (symbol-value 'scroll-bar-width))))
		 
		   (if (not w)
		       (setq w (frame-parameter (selected-frame)
						'scroll-bar-width)))

		   ;; in 21.2, the frame parameter is sometimes empty
		   ;; so we need to get the value here.
		   (if (not w)
		       (setq w (+ (get 'scroll-bar-width 'x-frame-parameter)
				  ;; In 21.4, or perhaps 22.1 the x-frame
				  ;; parameter is different from the frame
				  ;; parameter by only 1 pixel.
				  1)))

		   (if (not w)
		       "   "
		     (setq w (+ charwidth w))   ; Some sort of border around
					        ; the scrollbar.
		     (make-string (/ w charwidth) ? )))
	       ""))
	 (error ""))
       (condition-case nil
	   ;; Test fringe size.
	   (let* ((f (window-fringes))
		  (fw (car f))
		  (numspace (/ fw (tree-buffer-frame-char-width)))
		  )
	     (make-string numspace ? ))
	 (error
	  ;; Well, the fancy new Emacs functions failed.  Try older
	  ;; tricks.
	  (condition-case nil
	      ;; I'm not so sure what's up with the 21.1-21.3 fringe.
	      ;; It looks to be about 1 space wide.
	      (if (get 'fringe 'face)
		  " "
		"")
	    (error ""))))
       )
    ;; Not Emacs or a window system means no scrollbar or fringe,
    ;; and perhaps not even a header line to worry about.
    ""))


(defconst tree-buffer-stickynode-header-line-format
  (cond (tree-buffer-running-xemacs
	 nil)
;; 	((>= emacs-major-version 22)
;; 	 '(:eval (list
;; 		  ;; Magic bit I found on emacswiki.
;; 		  (propertize " "
;;                               'display
;;                               '((space :align-to 0)))
;; 		  (tree-buffer-stickynode-fetch-stickyline))))
	((>= emacs-major-version 21)
	 '(:eval (list (tree-buffer-spec->sticky-indent-string tree-buffer-spec)
		       (tree-buffer-stickynode-fetch-stickyline))))
	(t nil))
  "The header line format used by sticky func mode.")

(defun tree-buffer-goto-sticky-node ()
  "Go in current tree-buffer to that node which should be sticky.
Returns the line-number of the sticky node."
  (goto-char (window-start))
  (forward-line -1)
  (end-of-line)
  ;; This is the node under the header-line
  (let* ((node-under-header-line (tree-buffer-get-node-at-point))
         (node-at-window-start (save-excursion
                                 (forward-line 1)
                                 (tree-buffer-get-node-at-point)))
         (parent-node (and node-at-window-start
                           (tree-node->parent node-at-window-start)))
         (node-to-go (if (eq parent-node (tree-buffer-get-root))
                         node-under-header-line
                       parent-node)))
    ;; we must go the node itself so we can get the whole
    ;; line - otherwise we would not get the right icons etc...
    (tree-buffer-goto-line (tree-buffer-displayed-node-linenr node-to-go)))
  (tree-buffer-current-line))


(defun tree-buffer-stickynode-fetch-stickyline ()
  "Make the parent-node at the top of the current tree-window sticky."
  (let ((str
         (if (= 0 (count-lines (point-min) (window-start)))
             ""
           (save-excursion
             ;; here we have at least one node under the header-line
             (tree-buffer-goto-sticky-node)
             (buffer-substring (tree-buffer-line-beginning-pos) (tree-buffer-line-end-pos)))))
	(start 0))
    ;; we must handle the special sign % of head-line-format!
    (save-match-data
      (while (string-match "%" str start)
        (setq str (replace-match "%%" t t str 0)
              start (1+ (match-end 0)))
        ))
    ;; In 21.4 (or 22.1) the heder doesn't expand tabs.  Hmmmm.
    ;; We should replace them here.
    ;;
    ;; This hack assumes that tabs are kept smartly at tab boundaries
    ;; instead of in a tab boundary where it might only represent 4 spaces.
    (save-match-data
      (while (string-match "\t" str start)
        (setq str (replace-match "        " t t str 0))))
    str))


;; tree-buffer creation

(defun tree-buffer-create-mouse-key (button trigger &optional modifier key-qualifier)
  "Create a mouse-key which can be bound to a command via `define-key'.
BUTTON is the number of the mouse-button which can be 1, 2 or 3. TRIGGER
determines when the command is triggered, values can be 'button-press and
'button-release. The third optional modifier can be one of the symbols 'shift,
'control or 'meta. The fourth optional argument KEY-QUALIFIER is only used by
GUN Emacs and can be an additional key-qualifier symbol like 'mode-line or
'header-line."
  (let ((mouse-button (if tree-buffer-running-xemacs
                          (format "button%d%s"
                                  button
                                  (if (equal trigger 'button-press)
                                      ""
                                    "up"))
                        (format "%smouse-%d"
                                (if (equal trigger 'button-press)
                                    "down-"
                                  "")
                                button)))
        (modifier-elem (if tree-buffer-running-xemacs
                           modifier
                         (case modifier
                           (shift "S-")
                           (control "C-")
                           (meta "M-")
                           (otherwise "")))))
    (if tree-buffer-running-xemacs
        (delete nil (list modifier-elem (intern mouse-button)))
      (if (and key-qualifier (symbolp key-qualifier))
          (vector key-qualifier (intern (concat modifier-elem mouse-button)))
        (vector (intern (concat modifier-elem mouse-button)))))))

(defun* tree-buffer-create (name
                            &key
                            frame
                            mouse-action-trigger
                            is-click-valid-fn
                            node-selected-fn
                            node-expanded-fn
                            node-collapsed-fn
                            node-mouse-over-fn
                            mouse-highlight-fn
                            node-data-equal-fn
                            maybe-empty-node-types
                            leaf-node-types
                            menu-creator
                            menu-titles
                            modeline-menu-creator
                            sticky-parent-p
                            sticky-indent-string
                            sticky-parent-fn
                            trunc-lines
                            read-only
                            tree-indent
                            incr-search-p
                            incr-search-additional-pattern
                            reduce-tree-for-incr-search-fn
                            arrow-navigation
                            hor-scroll-step
                            default-images-dir
                            additional-images-dir
                            image-file-prefix
                            tree-style
                            ascii-guide-face
                            type-facer
                            expand-symbol-before-p
                            highlight-node-face
                            general-face
                            after-create-hook
                            after-update-hook)
  "Creates a new tree buffer and returns the newly created buffer.
This function creates also a special data-storage for this tree-buffer which
can be accessed via `tree-buffer-set-data-store' and `tree-buffer-get-data-store'.
The user of this tree-buffer can store any arbitrary data in this storage.
Before using the accessor-functions above the tree-buffer has to be the
current buffer!

NAME: Buffername of the new tree-buffer.

The rest of the arguments are key-arguments of the form :arg-name arg-value,
so for example a call looks like \(tree-buffer-create <buffer-name> :frame
<frame-object> ...). The following key-arguments can be arranged in any
arbitrary order but all of them are not-optional! The key-arg-name is always a
: followed by the lowercase version of the mentioned argument below \(e.g.
FRAME --> :frame, MOUSE-ACTION-TRIGGER --> :mouse-action-trigger)

FRAME: Frame in which the tree-buffer is displayed and valid. All key-bindings
       and interactive functions of the tree-buffer work only if called in
       FRAME otherwise nothing is done!
MOUSE-ACTION-TRIGGER: When a mouse-action is triggered. Allowed values:
                      'button-release and 'button-press.
IS-CLICK-VALID-FN: `tree-buffer-create' rebinds mouse-1, mouse-2, RET \(and
                   TAB) and also in combination with shift and control \(not
                   with TAB). IS-CLICK-VALID-FN is called first if a node or
                   an expand-symbol is clicked. This function is called with
                   five arguments:
                   - mouse-button: The clicked mouse-button or RET or TAB \(0
                     = RET or TAB, 1 = mouse-1, 2 = mouse 2)
                   - shift-pressed: non nil if the SHIFT-key was pressed
                     during mouse-click or RET.
                   - control-pressed: non nil if the CONTROL-key was pressed
                     during mouse-click or RET.
                   - meta-pressed: non nil if the META-key was pressed during
                     mouse-click or RET.
                   - tree-buffer-name: The buffer-name of the tree-buffer
                     where the node has been clicked.
                   The function must return not nil iff exactly this click/hit
                   is accepted. If the function returns nil then really
                   nothing is done by the tree-buffer after this click/hit!
NODE-SELECTED-FN: Function to call if a node has been selected. This function
                  is called with the following parameters:
                  - node: The selected node
                  - mouse-button \(0 = RET, 1 = mouse-1, 2 = mouse 2)
                  - shift-pressed
                  - control-pressed
                  - meta-pressed
                  - tree-buffer-name
                  For the last four arguments see the description above. This
                  function has to ensure that the expandable- and
                  expanded-state of the selected node is correct after
                  returning.
NODE-EXPANDED-FN: Function to call if a node is expandable, point stays onto
                  the expand-symbol and node is not already expanded. This
                  function is called with the following parameters:
                  - node: The selected node
                  - mouse-button \(0 = TAB, 1 = mouse-1, 2 = mouse 2)
                  - shift-pressed
                  - control-pressed
                  - meta-pressed
                  - tree-buffer-name
                  This function should add all children nodes to this node if
                  not already done \(if possible). This function has to ensure
                  that the expandable- and expanded state of the selected node
                  is correct after returning!
NODE-COLLAPSED-FN: Function to call if a node is expandable, point stays
                   onto the expand-symbol and node is already expanded.
                   This function is called with the following parameters:
                   - node: The selected node
                   - mouse-button \(0 = TAB, 1 = mouse-1, 2 = mouse 2)
                   - shift-pressed
                   - control-pressed
                   - meta-pressed
                   - tree-buffer-name
                   This function is only a callback to inform the owner/user
                   of this tree-buffer that this node has been collapsed. This
                   function must not modify the expandable- or expanded state
                   of the selected node!
NODE-MOUSE-OVER-FN: Function to call when the mouse is moved over a node. This
                    function is called with three arguments: NODE, WINDOW,
                    NO-PRINT, each of them related to the current tree-buffer.
                    If NO-PRINT is nil then the function must print the text
                    itself in any manner. This function must always return the
                    text which either is printed by the function itself or by
                    the caller \(if NO-PRINT is not nil). The current buffer
                    for this function is the tree-buffer itself. With XEmacs
                    this function is only called if the tree-buffer
                    track-mouse mechanism is activated \(see the function
                    `tree-buffer-activate-follow-mouse'). With GNU Emacs 21
                    this function is called by the `help-echo' property added
                    to each node.
MOUSE-HIGHLIGHT-FN: If nil then in this tree-buffer no node is highlighted
                    when the mouse moves over it. If t then each node is
                    highlighted when the mouse moves over it. If a function
                    then it is called with the node as argument and if it
                    returns not nil then the node will be highlighted when the
                    mouse moves over it - otherwise no highlighting takes place.
NODE-DATA-EQUAL-FN: Function used by the tree-buffer to test if the data of
                    two tree-nodes are equal. The function is called with two
                    args: The DATA-slots of the two tree-nodes.
MAYBE-EMPTY-NODE-TYPES: Nil or a list of node-types \(a node-type is an
                        integer which must be set for `tree-node-new'). Nodes
                        with one of these types are treated as empty if they
                        are not expandable \(i.e. they have no children) and
                        will be displayed with the empty-symbol \(\[x]); for
                        other nodes see next argument.
LEAF-NODE-TYPES: Nil or a list of node-types \(see above). Nodes
                 with one of these types are treated as leafs and will be
                 displayed with the leaf-symbol \(*). Summary for
                 MAYBE-EMPTY-NODE-TYPES and LEAF-NODE-TYPES:
                 * Expandable nodes will always be displayed either with the
                   open- or with the close-symbol.
                 * Not-expandable nodes with a node-type contained in
                   MAYBE-EMPTY-NODE-TYPES will be displayed with the
                   empty-symbol.
                 * Not-expandable nodes with a node-type contained in
                   LEAF-NODE-TYPES will be displayed with the leaf-symbol.
                 * All other nodes will be displayed with no symbol just with
                   correct indentation.
MENU-CREATOR: Nil or function which has to return nil or a list of conses,
              each cons for a known node-type of this tree-buffer \(the
              node-type of a node is an integer). Example: \(\(0 .
              menu-for-type-0) \(1 . menu-for-type-1)). The cdr of a cons must
              be a menu in the same format `tree-buffer-create-menu' expects
              as argument - see the documentation of this function for
              details. This function gets two argument: The name of the
              tree-buffer and the node for which a popup-menu should be
              opened.
MENU-TITLES: Nil or a list conses, each cons for a node-type. See
             MENU-CREATOR. The cdr of a cons must be either a string or a
             function which will be called with current node under point and
             must return a string which is displayed as the menu-title.
MODELINE-MENU-CREATOR: Nil or a function which has to return nil or a menu in
                       the same format `tree-buffer-create-menu' expects as
                       argument - see the documentation of this function for
                       details. This function gets one argument: The name of
                       the tree-buffer. If the function returns a menu then
                       this menu will be displayed when the user clicks with
                       mouse-button 3 at the modeline of the tree-buffer. The
                       menu-title will be \"Tree-buffer modeline-menu\".
STICKY-PARENT-P: If not nil then with GNU Emacs >= 21 the tree-buffer displays
                 in its header-line the unvisible parent-node if there is any.
                 STICKY-PARENT-FN is used to get this parent node.
STICKY-INDENT-STRING: String used for indendation of the sticky node in the
                      header-line so it matches the tree-display.
STICKY-PARENT-FN: Function used to get that parent node which should be sticky.
                  This function gets as argument a node and
                  should either return nil \(if there is not
                  suitable parent node) or a node. This node will
                  be displayed as sticky in the header-line of the
                  tree-buffer. If nil is returned and STICKY-PARENT-P is not
                  nil then just the node under the header-line is displayed.
                  If this argument is nil and STICKY-PARENT-P is not nil then
                  always the next unvisible parent node will be displayed in
                  the header-line.
TRUNC-LINES: Should lines in this tree buffer be truncated \(not nil).
READ-ONLY: Should the treebuffer be read-only \(not nil).
TREE-INDENT: Spaces subnodes should be indented. Ignored if TREE-STYLE is
             'image \(see below).
INCR-SEARCH-P: Should the incremental search be enabled in the tree-buffer.
               Three choices: 'prefix, 'substring, nil. See the command
               `tree-buffer-incremental-node-search'.
INCR-SEARCH-ADDITIONAL-PATTERN: Every search-pattern is prefixed at least with
                                `tree-buffer-incr-searchpattern-indent-prefix'
                                and `tree-buffer-incr-searchpattern-expand-prefix'
                                to jump over not important stuff. If this
                                argument is not nil then it must be a
                                cons-cell where car is a string which should
                                be a regexp-pattern which is added to the
                                basic-prefix pattern and both of them prefix
                                the incr-search-pattern. The cdr is the number
                                of subexpressions in this pattern.
REDUCE-TREE-FOR-INCR-SEARCH-FN: Nil or a function which is called
                                directly after changing the current
                                incremental search-pattern by typing and
                                directly before the tree-buffer performes this
                                new search. The function gets two arguments:
                                The current search-pattern as typed in by the
                                user and the full serch-regexp build by
                                tree-buffer based on the plain search-pattern.
                                The purpose of such a function is to apply the
                                passed search-pattern as a filter to the
                                tree-contents to reduce the tree to tree-nodes
                                matching this filter. The passed full
                                search-regexp \(the second argument) is build
                                and used by tree-buffer itself to exclude some
                                stuff not relevant for the search \(e.g.
                                guide-lines etc..., see also the argument
                                INCR-SEARCH-ADDITIONAL-PATTERN above). The
                                function can change the search-pattern typed
                                in by the user \(the first argument) if this
                                is necessary. So the function must always
                                return the finaly applied search-pattern based
                                on the first argument passed to the function.
                                The returned value must be a string but can be
                                the empty string \(means no filter has been
                                applied). Current buffer is the tree-buffer.
ARROW-NAVIGATION: If not nil then a smart navigation with arrow keys is offered.
HOR-SCROLL-STEP: Number of columns a hor. scroll in the tree-buffer should scroll.
                 If not nil then M-mouse-1 and M-mouse-2 scroll left and right
                 and also M-<left-arrow> and M-<right-arrow>. Ignored with XEmacs.
DEFAULT-IMAGES-DIR: Full path where the default images for the tree-buffer can
                    be found. It should contain an image for every name of
                    `tree-buffer-tree-image-names'.
ADDITIONAL-IMAGES-DIR: Additional image-dir which should be searched first for
                       images needed for current tree-buffer. If the image can
                       not be found in this directory then DEFAULT-IMAGES-DIR
                       is searched. If the image can't even found here the
                       related ascii-symbol is used.
IMAGE-FILE-PREFIX: Common prefix for all image-files for this tree-buffer,
                   e.g. \"ecb-\".
TREE-STYLE: There are three different styles available:
            Image-style \(value 'image): Very nice and modern because
            image-icons are used to display the tree-buffer. For this style
            the arguments TREE-INDENT and EXPAND-SYMBOL-BEFORE-P have no
            effect!
            
            Ascii-style with guide-lines \(value 'ascii-guides):
            \[-] ECB
             |  \[+] code-save
             `- \[-] ecb-images
                 |  \[-] directories
                 |   |  \[-] height-15
                 |   |   |  * close.xpm
                 |   |   |  * empty.xpm
                 |   |   |  * leaf.xpm
                 |   |   `- * open.xpm
                 |   |  \[+] height-17
                 |   |  \[+] height-19
                 |   `- \[+] height-21
                 |  \[x] history
                 |  \[x] methods
                 `- \[x] sources
            
            Ascii-style without guide-lines \(value 'ascii-no-guides):
            \[-] ECB
                \[+] code-save
                \[-] ecb-images
                    \[-] directories
                        \[-] height-15
                            * close.xpm
                            * empty.xpm
                            * leaf.xpm
                            * open.xpm
                        \[+] height-17
                        \[+] height-19
                        \[+] height-21
                    \[x] history
                    \[x] methods
                    \[x] sources
            
            Both ascii-styles are affected by the args TREE-INDENT and
            EXPAND-SYMBOL-BEFORE-P..
ASCII-GUIDE-FACE: If TREE-STYLE is 'ascii-guides then this defines the face
                  the guides should be displayed with.
TYPE-FACER: Nil or a list of one or more conses, each cons for a node-type \(a
            node-type is an integer which must be set for `tree-node-new').
            The cdr of a cons can be:
            - a face-symbol
            - a function-symbol which gets two arguments \(see
              `tree-buffer-insert-text'). This function can do anything, but
              normally it should face a tree-node.
            - the symbol t. Then the tree-buffer assumes that the node-text is
              already faced and therefore it does not face the node, means it
              does nothing then inserting the node-text, if the tree-buffer is
              updated.
EXPAND-SYMBOL-BEFORE-P: If not nil then the expand-symbol is displayed before
                        the node-text. Ignored when TREE-STYLE is 'image and
                        Emacs can display images.
HIGHLIGHT-NODE-FACE: Face used for highlighting current selected node in this
                     tree-buffer.
GENERAL-FACE: General face in which the whole tree-buffer should be displayed.
AFTER-CREATE-HOOK: A function or a list of functions \(with no arguments)
                   called directly after creating the tree-buffer and defining
                   it's local keymap. For example such a function can add
                   additional key-bindings for this tree-buffer local keymap
                   \(use `local-set-key' for this).
AFTER-UPDATE-HOOK: A function or a list of functions \(with no arguments)
                   called each time after the tree-buffer has been updated via
                   `tree-buffer-update'.

See Info node `(ecb)tree-buffer' for all details of using tree-buffers."
  (let ((nop (function (lambda(e) (interactive "e"))))
        (a-c-h (if (functionp after-create-hook)
                   (list after-create-hook)
                 after-create-hook))
        (mouse-action-trigger-not (if (equal mouse-action-trigger
                                             'button-press)
                                      'button-release
                                    'button-press)))
        
    (set-buffer (get-buffer-create name))

    (make-local-variable 'truncate-lines)
    (make-local-variable 'truncate-partial-width-windows)
    (setq truncate-lines trunc-lines)
    (setq truncate-partial-width-windows trunc-lines)

    (setq cursor-in-non-selected-windows nil)

    (setq buffer-read-only read-only)

    (make-local-variable 'tree-buffer-key-map)
    (setq tree-buffer-key-map (make-sparse-keymap))

    (make-local-variable 'tree-buffer-frame)
    (setq tree-buffer-frame frame)

    (make-local-variable 'tree-buffer-root)
    (setq tree-buffer-root (tree-node-new-root))

    (make-local-variable 'tree-buffer-displayed-nodes)
    (tree-buffer-initialize-displayed-nodes)
    
    (make-local-variable 'tree-buffer-spec)
    (setq tree-buffer-spec
          (tree-buffer-spec-new :tree-indent tree-indent
                                :menu-creator menu-creator
                                :menu-titles menu-titles
                                :modeline-menu-creator modeline-menu-creator
                                :sticky-parent-p sticky-parent-p
                                :sticky-indent-string sticky-indent-string
                                :sticky-parent-fn sticky-parent-fn
                                :type-facer type-facer
                                :expand-symbol-before-p expand-symbol-before-p
                                :mouse-action-trigger mouse-action-trigger
                                :is-click-valid-fn is-click-valid-fn
                                :node-selected-fn node-selected-fn
                                :node-expanded-fn node-expanded-fn
                                :node-collapsed-fn node-collapsed-fn
                                :node-mouse-over-fn node-mouse-over-fn
                                :mouse-highlight-fn mouse-highlight-fn
                                :node-data-equal-fn node-data-equal-fn
                                :after-update-hook after-update-hook
                                :maybe-empty-node-types maybe-empty-node-types
                                :leaf-node-types leaf-node-types
                                :general-face general-face
                                :incr-search-p incr-search-p
                                :incr-search-additional-pattern incr-search-additional-pattern
                                :reduce-tree-for-incr-search-fn reduce-tree-for-incr-search-fn
                                :hor-scroll-step hor-scroll-step
                                :default-images-dir default-images-dir
                                :additional-images-dir additional-images-dir
                                :image-file-prefix image-file-prefix
                                :style tree-style
                                :ascii-guide-face ascii-guide-face))

    (make-local-variable 'tree-buffer-incr-searchpattern)
    (make-local-variable 'tree-buffer-last-incr-searchpattern)
    (make-local-variable 'tree-buffer-incr-searchpattern-indent-prefix)
    (setq tree-buffer-incr-searchpattern "")
    (setq tree-buffer-last-incr-searchpattern "")
    (setq tree-buffer-incr-searchpattern-indent-prefix
          (tree-buffer-gen-searchpattern-indent-prefix))

    (make-local-variable 'tree-buffer-highlight-overlay)
    (setq tree-buffer-highlight-overlay (tree-buffer-make-overlay 1 1))
    (tree-buffer-overlay-put tree-buffer-highlight-overlay
                             'face highlight-node-face)

    (make-local-variable 'tree-buffer-general-overlay)
    (setq tree-buffer-general-overlay (tree-buffer-make-overlay 1 1))
    (tree-buffer-overlay-put tree-buffer-general-overlay 'face
                             general-face)

  
    (make-local-variable 'tree-buffer-highlighted-node)
    (setq tree-buffer-highlighted-node nil)
    (make-local-variable 'tree-buffer-hscroll-number)
    (setq tree-buffer-hscroll-number 0)
    
    ;; initialize the user-data-storage for this tree-buffer.
    (set (make-local-variable 'tree-buffer-data-store) nil)
    ;; initialize the local image-cache for this tree-buffer
    (set (make-local-variable 'tree-buffer-local-image-cache) nil)

    ;; set a special syntax table for tree-buffers
    (set-syntax-table tree-buffer-syntax-table)
    
    ;; keyboard setting
    (when incr-search-p
      ;; settings for the incremental search.
      ;; for all keys which are bound to `self-insert-command' in `global-map'
      ;; we change this binding to `tree-buffer-incremental-node-search'.
      (substitute-key-definition 'self-insert-command
                                 'tree-buffer-incremental-node-search
                                 tree-buffer-key-map
                                 global-map)
      (define-key tree-buffer-key-map [delete]
        'tree-buffer-incremental-node-search)
      (define-key tree-buffer-key-map [backspace]
        'tree-buffer-incremental-node-search)
      (define-key tree-buffer-key-map [home]
        'tree-buffer-incremental-node-search)
      (define-key tree-buffer-key-map [end]
        'tree-buffer-incremental-node-search))
    
    (define-key tree-buffer-key-map (kbd "<RET>")
      (function (lambda ()
                  (interactive)
                  (tree-buffer-select 0 nil))))
    (define-key tree-buffer-key-map (kbd "<S-return>")
      (function (lambda ()
                  (interactive)
                  (tree-buffer-select 0 '(shift)))))
    (define-key tree-buffer-key-map (kbd "<C-return>")
      (function (lambda ()
                  (interactive)
                  (tree-buffer-select 0 '(control)))))
    (define-key tree-buffer-key-map (kbd "<M-return>")
      (function (lambda ()
                  (interactive)
                  (tree-buffer-select 0 '(meta)))))
    (define-key tree-buffer-key-map (kbd "<C-S-return>")
      (function (lambda ()
                  (interactive)
                  (tree-buffer-select 0 '(shift control)))))
    
    (define-key tree-buffer-key-map (kbd "TAB") 'tree-buffer-tab-pressed)

    (when arrow-navigation
      (define-key tree-buffer-key-map (kbd "<up>") 'tree-buffer-arrow-pressed)
      (define-key tree-buffer-key-map (kbd "<down>") 'tree-buffer-arrow-pressed)
      (define-key tree-buffer-key-map (kbd "<right>") 'tree-buffer-arrow-pressed)
      (define-key tree-buffer-key-map (kbd "<left>") 'tree-buffer-arrow-pressed))
    
    (define-key tree-buffer-key-map (kbd "M-m")
      'tree-buffer-show-node-menu-keyboard)

;;     (mapc (function
;;            (lambda (key)
;;              (tree-buffer-define-mouse-key 1 key)))
;;           '(nil shift control meta))

    (macrolet ((tbc-define-mouse-key-1/2 (button key)
                `(progn
                   (define-key tree-buffer-key-map
                     (tree-buffer-create-mouse-key ,button mouse-action-trigger ,key)
                     (function (lambda(e)
                                 (interactive "e")
                                 (tree-buffer-mouse-set-point e)
                                 (tree-buffer-select ,button (list ,key)))))
                   (define-key tree-buffer-key-map
                     (tree-buffer-create-mouse-key ,button mouse-action-trigger-not ,key)
                     nop)))
               (tbc-define-mouse-key-1/2-header
                (button key)
                `(progn
                   (define-key tree-buffer-key-map
                     (tree-buffer-create-mouse-key ,button mouse-action-trigger ,key 'header-line)
                     (function (lambda(e)
                                 (interactive "e")
                                 (tree-buffer-mouse-set-point e)
                                 (when (< 0 (count-lines (point-min) (window-start)))
                                   ;; we have at least one node under the header-line
                                   (let ((click-col (tree-buffer-get-event-column e)))
                                     ;; go to the sticky node
                                     (tree-buffer-goto-sticky-node)
                                     ;; go to beginning of current line
                                     (forward-line 0)
                                     ;; move right
                                     (forward-char click-col))
                                   (tree-buffer-select ,button (list ,key))))))
                   (define-key tree-buffer-key-map
                     (tree-buffer-create-mouse-key ,button mouse-action-trigger-not ,key 'header-line)
                     nop))))

      ;; mouse-1
      (tbc-define-mouse-key-1/2 1 nil)
      (tbc-define-mouse-key-1/2 1 'shift)
      (tbc-define-mouse-key-1/2 1 'control)
      (tbc-define-mouse-key-1/2 1 'meta)
      (define-key tree-buffer-key-map [drag-mouse-1] nop)
      (define-key tree-buffer-key-map [double-mouse-1] nop)
      (define-key tree-buffer-key-map [triple-mouse-1] nop)

      ;; mouse-2
      (tbc-define-mouse-key-1/2 2 nil)
      (tbc-define-mouse-key-1/2 2 'shift)
      (tbc-define-mouse-key-1/2 2 'control)
      (tbc-define-mouse-key-1/2 2 'meta)
      (define-key tree-buffer-key-map [double-mouse-2] nop)
      (define-key tree-buffer-key-map [triple-mouse-2] nop)

      (when (and (not tree-buffer-running-xemacs) sticky-parent-p)
        (setq header-line-format
              tree-buffer-stickynode-header-line-format)
        ;; mouse-1 header-line
        (tbc-define-mouse-key-1/2-header 1 nil)
        (tbc-define-mouse-key-1/2-header 1 'shift)
        (tbc-define-mouse-key-1/2-header 1 'control)
        (tbc-define-mouse-key-1/2-header 1 'meta)
        
        ;; mouse-2 header-line
        (tbc-define-mouse-key-1/2-header 2 nil)
        (tbc-define-mouse-key-1/2-header 2 'shift)
        (tbc-define-mouse-key-1/2-header 2 'control)
        (tbc-define-mouse-key-1/2-header 2 'meta)
        )
      )
    
    ;; mouse-3 - here we use hard button-press because this is consitent to
    ;; standard popup-behavior of (X)Emacs
    (define-key tree-buffer-key-map
      (tree-buffer-create-mouse-key 3 'button-press nil)
      (function (lambda(e)
		  (interactive "e")
                  (tree-buffer-mouse-set-point e)
                  (tree-buffer-show-node-menu e))))
    (when (and (not tree-buffer-running-xemacs) sticky-parent-p)
      (define-key tree-buffer-key-map
        (tree-buffer-create-mouse-key 3 'button-press nil 'header-line)
        (function (lambda(e)
                    (interactive "e")
                    (tree-buffer-mouse-set-point e)
                    (let ((click-col (tree-buffer-get-event-column e)))
                      ;; go to the sticky node
                      (tree-buffer-goto-sticky-node)
                      ;; go to beginning of current line
                      (forward-line 0)
                      ;; move right
                      (forward-char click-col))
                    (tree-buffer-show-node-menu e)))))

    (define-key tree-buffer-key-map
      (tree-buffer-create-mouse-key 3 'button-press 'shift) nop)
    (define-key tree-buffer-key-map
      (tree-buffer-create-mouse-key 3 'button-press 'control) nop)
    (define-key tree-buffer-key-map
      (tree-buffer-create-mouse-key 3 'button-press 'meta) nop)
    (define-key tree-buffer-key-map
      (tree-buffer-create-mouse-key 3 'button-release nil) nop)
    (define-key tree-buffer-key-map
      (tree-buffer-create-mouse-key 3 'button-release 'shift) nop)
    (define-key tree-buffer-key-map
      (tree-buffer-create-mouse-key 3 'button-release 'control) nop)
    (define-key tree-buffer-key-map
      (tree-buffer-create-mouse-key 3 'button-release 'meta) nop)
    (define-key tree-buffer-key-map [double-mouse-3] nop)
    (define-key tree-buffer-key-map [triple-mouse-3] nop)

    ;; modeline bindings....here we use hard coded button-press too - s.a.
    
    (if tree-buffer-running-xemacs
        (progn
          (set (make-local-variable 'modeline-map)
               (make-sparse-keymap 'modeline-map))
          ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: maybe we should use
          ;; here mouse-action-trigger-not instead of hard button-press?! But
          ;; at least when mouse-action-trigger is button-release then it
          ;; works now also for XEmacs... and it is consistent to Emacs and
          ;; standard popup-menu trigger (which is button-press)... so we
          ;; first change this when problems are reported.......
          (define-key modeline-map
            (tree-buffer-create-mouse-key 3 'button-press nil)
            (function (lambda (e)
                        (interactive "e")
                        (tree-buffer-mouse-set-point e)
                        (tree-buffer-show-modeline-menu e))))
          (define-key modeline-map
            (tree-buffer-create-mouse-key 1 'button-press nil)
            'mouse-drag-modeline)
          )
      (define-key tree-buffer-key-map
        (tree-buffer-create-mouse-key 3 'button-press nil 'mode-line)
        (function (lambda (e)
                    (interactive "e")
                    (tree-buffer-mouse-set-point e)
                    (tree-buffer-show-modeline-menu e)))))
    
    ;; scrolling horiz.
    (when (and (not tree-buffer-running-xemacs) hor-scroll-step)
      ;; This lets the GNU Emacs user scroll as if we had a horiz.
      ;; scrollbar...
      (define-key tree-buffer-key-map
        (tree-buffer-create-mouse-key 1 'button-press nil 'mode-line)
        'tree-buffer-mouse-hscroll))

    (use-local-map tree-buffer-key-map)

    (setq tree-buffers (cons (current-buffer) tree-buffers))

    (prog1
        (current-buffer)
      (dolist (f a-c-h)
        (funcall f)))))

(defun tree-buffer-destroy (tree-buffer-name)
  "Destroy the tree-buffer with name TREE-BUFFER-NAME. Does nothing if either
tree-buffer-name is not alive or if it is not a tree-buffer created with
`tree-buffer-create'."
  (when (and tree-buffer-name (member (get-buffer tree-buffer-name) tree-buffers))
    (setq tree-buffers (delq (get-buffer tree-buffer-name) tree-buffers))
    (ignore-errors (kill-buffer tree-buffer-name))))


;; editor goodies
(defconst tree-buffer-font-lock-keywords
  (eval-when-compile
    (let* (
           ;; Function declarations
	   (vf '(
                 "tree-buffer-defpopup-command"
		 ))
           (kf (if vf (regexp-opt vf t) ""))
           ;; Regexp depths
           (kf-depth (if kf (regexp-opt-depth kf) nil))
           (full (concat
                  ;; Declarative things
                  "(\\(" kf "\\)"
                  ;; Whitespaces & name
                  "\\>[ \t]*\\(\\sw+\\)?"
                  ))
           )
      `((,full
         (1 font-lock-keyword-face)
         (,(+ 1 kf-depth 1)
          font-lock-function-name-face
          nil t)))
      ))
  "Highlighted tree-buffer keywords.")

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords 'emacs-lisp-mode
                          tree-buffer-font-lock-keywords)
  )


(silentcomp-provide 'tree-buffer)

;;; tree-buffer.el ends here
