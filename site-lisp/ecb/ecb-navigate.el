;;; ecb-navigate.el --- Browser-navigation for ECB

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2001

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

;; $Id: ecb-navigate.el,v 1.23 2009/04/15 14:22:35 berndl Exp $

;;; Commentary:

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.


;;; Code:

(eval-when-compile
  (require 'silentcomp))

(require 'eieio)
(require 'ecb-util)



;;====================================================
;; 
;;====================================================

(defclass ecb-dlist-node ()
  ((previous :initform nil); :protection :private)
   (next :initform nil); :protection :private)
   (data :initarg :data :initform nil); :protection :private)
   )
  "A node in a double linked list."
  )

(defun ecb-dlist-node-new (data)
  (ecb-dlist-node "node" :data data))

(defmethod ecb-get-data ((node ecb-dlist-node))
  (oref node data))

(defmethod ecb-get-next ((node ecb-dlist-node))
  (oref node next))

(defmethod ecb-get-previous ((node ecb-dlist-node))
  (oref node previous))

(defmethod ecb-set-data ((node ecb-dlist-node) data)
  (oset node data data))

(defmethod ecb-set-next ((node ecb-dlist-node) next)
  (let ((old-next (ecb-get-next node)))
    (when old-next
      (oset old-next previous nil))
    (oset node next next)
    (when next
      (ecb-set-previous next nil)
      (oset next previous node))))

(defmethod ecb-set-previous ((node ecb-dlist-node) previous)
  (let ((old-previous (ecb-get-previous node)))
    (when old-previous
      (oset old-previous next nil))
    (oset node previous previous)
    (when previous
      (ecb-set-next previous nil)
      (oset previous next node))))


;;====================================================
;; 
;;====================================================

(defclass ecb-nav-history-item ()
  ((pos :initarg :pos :initform 0); :protection :private)
   (window-start :initarg :window-start :initform 0); :protection :private)
   )
  )
  
(defmethod ecb-nav-set-pos ((item ecb-nav-history-item) pos)
  (oset item pos pos))

(defmethod ecb-nav-set-window-start ((item ecb-nav-history-item) point)
  (oset item window-start point))

(defmethod ecb-nav-get-pos ((item ecb-nav-history-item))
  (oref item pos))

(defmethod ecb-nav-get-window-start ((item ecb-nav-history-item))
  (oref item window-start))

(defmethod ecb-nav-to-string ((item ecb-nav-history-item))
  (concat (int-to-string (ecb-nav-get-pos item)) ":"
	  (int-to-string (ecb-nav-get-window-start item))))

;; This method must return nil if saving can not be performed and otherwise
;; not nil!
(defmethod ecb-nav-save ((item ecb-nav-history-item))
  t)

(defmethod ecb-nav-is-valid ((item ecb-nav-history-item))
  t)


;;====================================================
;; 
;;====================================================

;; Klaus Berndl <klaus.berndl@sdm.de>: Changed this class from storing the
;; whole tag to storing explicitly the tag-buffer, the marker of the
;; tag-start, the marker of the tag-end. This prevents the stored
;; navigation-items from getting invalid and unusable after a full
;; semantic-reparse because such a reparse makes the overlays contained in the
;; stored tags invalid so we can not uses their informations.
(defclass ecb-nav-tag-history-item (ecb-nav-history-item)
  ((tag-buffer :initarg :tag-buffer :initform nil); :protection :private)
   (tag-start :initarg :tag-start :initform nil) ; :protection :private)
   (tag-end :initarg :tag-end :initform nil) ; :protection :private)
   (tag-name :initarg :tag-name :initform nil) ; :protection :private)
   (narrow :initarg :narrow :initform nil); :protection :private)
   )
  )

(defun ecb-nav-tag-history-item-new (tag-name tag-buffer tag-start
                                                  tag-end &optional narrow)
  (ecb-nav-tag-history-item tag-name
                              :tag-buffer tag-buffer
                              :tag-start tag-start
                              :tag-end tag-end
                              :tag-name tag-name
                              :narrow narrow))

(defmethod ecb-nav-get-tag-buffer ((item ecb-nav-tag-history-item))
  (oref item tag-buffer))

(defmethod ecb-nav-get-tag-start ((item ecb-nav-tag-history-item))
  (oref item tag-start))

(defmethod ecb-nav-get-tag-end ((item ecb-nav-tag-history-item))
  (oref item tag-end))

(defmethod ecb-nav-get-tag-name ((item ecb-nav-tag-history-item))
  (oref item tag-name))

(defmethod ecb-nav-get-narrow ((item ecb-nav-tag-history-item))
  (oref item narrow))

(defmethod ecb-nav-goto ((item ecb-nav-tag-history-item))
  (let ((tag-buffer (ecb-nav-get-tag-buffer item))
        (tag-start (ecb-nav-get-tag-start item))
        (tag-end (ecb-nav-get-tag-end item))
        (win-start (ecb-nav-get-window-start item)))
    (select-window ecb-last-edit-window-with-point)
    (set-window-buffer (selected-window) tag-buffer)
    (widen)
    (goto-char tag-start)
    (when (ecb-nav-get-narrow item)
      (narrow-to-region (ecb-line-beginning-pos) tag-end))
    (goto-char (+ tag-start (ecb-nav-get-pos item)))
    (if win-start
        (set-window-start (selected-window) (+ tag-start win-start)))))

(defmethod ecb-nav-save ((item ecb-nav-tag-history-item))
  "Return only nil if tag-start of ITEM points into a dead buffer. In this
case no position saving is done."
  (let ((tag-start (ecb-nav-get-tag-start item)))
    (if (and tag-start (marker-buffer tag-start))
        (progn
          (save-excursion
            (set-buffer (marker-buffer tag-start))
            (ecb-nav-set-pos item (- (point) tag-start)))
          (ecb-nav-set-window-start
           item
           (if (equal (window-buffer) (marker-buffer tag-start))
               (- (window-start) tag-start)
             nil))
          t)
      nil)))

(defmethod ecb-nav-to-string ((item ecb-nav-tag-history-item))
  (concat (ecb-nav-get-tag-name item) ":" (call-next-method)))

(defmethod ecb-nav-is-valid ((item ecb-nav-tag-history-item))
   (let ((tag-start (ecb-nav-get-tag-start item))
         (tag-buf (ecb-nav-get-tag-buffer item))
         (tag-end (ecb-nav-get-tag-end item)))
     (if (and tag-start (marker-buffer tag-start)
              tag-end (marker-buffer tag-end)
              tag-buf (buffer-live-p tag-buf))
         t)))
 

;;====================================================
;; 
;;====================================================

(defclass ecb-nav-file-history-item (ecb-nav-history-item)
  ((file :initarg :file :initform ""); :protection :private)
   ;; the following is nil if the item does not point to an indirect-buffer
   ;; based on a file-buffer
   (indirect-buffer-name :initarg :indirect-buffer-name :initform "") ; :protection :private)
   )
  )

(defun ecb-nav-file-history-item-new ()
  (let* ((file (ecb-buffer-file-name))
         (ind-buffer-name (and file
                               (buffer-base-buffer)
                               (buffer-name)))
         (item (ecb-nav-file-history-item (buffer-name)
                                          :file file
                                          :indirect-buffer-name ind-buffer-name)))
    (ecb-nav-set-pos item (point))
    (ecb-nav-set-window-start item
                              (window-start (get-buffer-window (current-buffer))))
    item))

(defmethod ecb-nav-get-file ((item ecb-nav-file-history-item))
  (oref item file))

(defmethod ecb-nav-set-file ((item ecb-nav-file-history-item) file)
  (oset item file file))

(defmethod ecb-nav-get-indirect-buffer-name ((item ecb-nav-file-history-item))
  (oref item indirect-buffer-name))

(defmethod ecb-nav-set-indirect-buffer-name ((item ecb-nav-file-history-item) indirect-buffer-name)
  (oset item indirect-buffer-name indirect-buffer-name))

(defmethod ecb-nav-save ((item ecb-nav-file-history-item))
  (ecb-nav-set-pos item (point))
  (ecb-nav-set-window-start item (window-start))
  (ecb-nav-set-file item (ecb-buffer-file-name))
  (ecb-nav-set-indirect-buffer-name item (and (ecb-buffer-file-name)
                                              (buffer-base-buffer)
                                              (buffer-name)))
  t)

(defmethod ecb-nav-goto ((item ecb-nav-file-history-item))
  (when (ecb-nav-get-file item)
    (if (ecb-nav-get-indirect-buffer-name item)
        (switch-to-buffer (ecb-nav-get-indirect-buffer-name item))
      (find-file (ecb-nav-get-file item)))
    (widen)
    (goto-char (ecb-nav-get-pos item))
    (let ((win-start (ecb-nav-get-window-start item)))
      (if win-start
          (set-window-start (selected-window) win-start)))))
  
(defmethod ecb-nav-to-string ((item ecb-nav-file-history-item))
  (concat (ecb-nav-get-file item) "-"
          (ecb-nav-get-indirect-buffer-name item)
          ":" (call-next-method)))

(defmethod ecb-nav-is-valid ((item ecb-nav-file-history-item))
  ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: for saveness we should test if
  ;; file points to a readable file - but what about remote-file (can last
  ;; long)?
  (and (ecb-nav-get-file item)
       (or (null (ecb-nav-get-indirect-buffer-name item))
           (get-buffer (ecb-nav-get-indirect-buffer-name item)))))

;;====================================================
;; 
;;====================================================

(defvar ecb-nav-first-node nil)
(setq ecb-nav-first-node (ecb-dlist-node-new (ecb-nav-history-item "First item")))

(defvar ecb-nav-current-node nil)
(setq ecb-nav-current-node ecb-nav-first-node)


(defun ecb-nav-initialize ()
  (setq ecb-nav-first-node
        (ecb-dlist-node-new (ecb-nav-history-item "First item")))
  (setq ecb-nav-current-node ecb-nav-first-node))
  

(defun ecb-nav-jump-to-tag (file tag &optional narrow)
  (ecb-nav-save-current)
  (find-file file)
  (ecb-nav-add-item (ecb-nav-tag-history-item tag narrow)))

(defun ecb-nav-jump-to-file (file)
  (ecb-nav-save-current)
  (find-file file)
  (ecb-nav-add-item (ecb-nav-file-history-item file)))

(defun ecb-nav-add-item (item)
  (let ((node (ecb-dlist-node-new item)))
    (ecb-set-next node (ecb-get-next ecb-nav-current-node))
    (ecb-set-next ecb-nav-current-node node)
    (setq ecb-nav-current-node node)))

(defun ecb-nav-remove-current-node ()
  (ecb-nav-remove-node ecb-nav-current-node))

(defun ecb-nav-remove-node (node)
  "Remove NODE and set `ecb-nav-first-node' and `ecb-nav-current-node' if
necessary."
  (let ((prev (ecb-get-previous node))
        (next (ecb-get-next node)))
    (if prev
        (ecb-set-next prev (ecb-get-next node)))
    (if next
        (ecb-set-previous next (ecb-get-previous node)))
    (if (eq node ecb-nav-current-node)
        (setq ecb-nav-current-node (or prev
                                       next
                                       ecb-nav-first-node)))
    (if (eq node ecb-nav-first-node)
        (if next
            (setq ecb-nav-first-node next)
        (ecb-nav-initialize)))))

(defun ecb-nav-remove-invalid-nodes ()
  (let ((node ecb-nav-first-node)
        (next-node nil))
    (while node
      (setq next-node (ecb-get-next node))
      (if (not (ecb-nav-is-valid (ecb-get-data node)))
          (ecb-nav-remove-node node))
      (setq node next-node))))

(defun ecb-nav-save-current ()
  (while (not (ecb-nav-save (ecb-get-data ecb-nav-current-node)))
    (ecb-nav-remove-current-node)))

(defun ecb-nav-goto-next ()
  "Go forward in the navigation history list."
  (interactive)
  (ecb-nav-goto--internal (ecb-get-next ecb-nav-current-node)))

(defun ecb-nav-goto-previous ()
  "Go back in the navigation history list."
  (interactive)
  (ecb-nav-goto--internal (ecb-get-previous ecb-nav-current-node)))

(defun ecb-nav-dump-history ()
  (interactive)
  (ecb-nav-remove-invalid-nodes)
  (ecb-nav-dump-history--internal ecb-nav-first-node))

(defun ecb-nav-dump-history--internal (node)
  (when node
    (insert (ecb-nav-to-string (ecb-get-data node)) "\n")
    (ecb-nav-dump-history--internal (ecb-get-next node))))

(defun ecb-nav-goto--internal (node)
  (if (or (not node) (eq ecb-nav-first-node node))
      (message "No more valid history items!")
    ;; before doing something we have to clear the history from now invalid
    ;; nodes means removing nodes which does not point into a live buffer
    (ecb-nav-remove-invalid-nodes)
    (ecb-nav-save-current)
    (setq ecb-nav-current-node node)
    (ecb-nav-goto (ecb-get-data node))))


(silentcomp-provide 'ecb-navigate)

;;; ecb-navigate.el ends here
