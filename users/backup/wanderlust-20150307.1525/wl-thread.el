;;; wl-thread.el --- Thread display modules for Wanderlust.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 1998,1999,2000 Masahiro MURATA  <muse@ba2.so-net.ne.jp>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Masahiro MURATA  <muse@ba2.so-net.ne.jp>
;; Keywords: mail, net news

;; This file is part of Wanderlust (Yet Another Message Interface on Emacsen).

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:
;;

;;; Code:
;;

(require 'wl-summary)
(require 'wl-highlight)
(eval-when-compile (require 'cl))

;; buffer local variables.
;;(defvar wl-thread-top-entity '(nil t nil nil)) ; top entity
(defvar wl-thread-tops nil)		; top number list (number)
(defvar wl-thread-entities nil)
(defvar wl-thread-entity-list nil)	; entity list
(defvar wl-thread-entity-hashtb nil)	; obarray

(make-variable-buffer-local 'wl-thread-entity-hashtb)
(make-variable-buffer-local 'wl-thread-entities)     ; ".wl-thread-entity"
(make-variable-buffer-local 'wl-thread-entity-list)  ; ".wl-thread-entity-list"

;;; global flag
(defvar wl-thread-insert-force-opened nil)

;;;;;; each entity is (number opened-or-not children parent) ;;;;;;;

(defun wl-thread-resume-entity (fld)
  (let (entities top-list)
    (setq entities (wl-summary-load-file-object
		    (expand-file-name wl-thread-entity-file
				      (elmo-folder-msgdb-path fld))))
    (setq top-list
	  (wl-summary-load-file-object
	   (expand-file-name wl-thread-entity-list-file
			     (elmo-folder-msgdb-path fld))))
    (message "Resuming thread structure...")
    ;; set obarray value.
    (setq wl-thread-entity-hashtb (elmo-make-hash (* (length entities) 2)))
    ;; set buffer local variables.
    (setq wl-thread-entities entities)
    (setq wl-thread-entity-list top-list)
    (while entities
      (elmo-set-hash-val (format "#%d" (car (car entities))) (car entities)
			 wl-thread-entity-hashtb)
      (setq entities (cdr entities)))
    (wl-thread-make-number-list)
    (message "Resuming thread structure...done")))

(defun wl-thread-make-number-list ()
  "Make `wl-summary-buffer-number-list', a list of message numbers."
  (if wl-thread-entity-list
      (let* ((node (wl-thread-get-entity (car wl-thread-entity-list)))
	     (children (wl-thread-entity-get-children node))
	     parent sibling)
	(setq wl-summary-buffer-number-list (list (car wl-thread-entity-list)))
	(while children
	  (wl-thread-entity-make-number-list-from-children
	   (wl-thread-get-entity (car children)))
	  (setq children (cdr children)))
	(while node
	  (setq parent (wl-thread-entity-get-parent-entity node)
		sibling (wl-thread-entity-get-younger-brothers
			 node parent))
	  (while sibling
	    (wl-thread-entity-make-number-list-from-children
	     (wl-thread-get-entity (car sibling)))
	    (setq sibling (cdr sibling)))
	  (setq node parent))
	(setq wl-summary-buffer-number-list (nreverse
					     wl-summary-buffer-number-list)))
    (setq wl-summary-buffer-number-list nil)))
  
(defun wl-thread-entity-make-number-list-from-children (entity)
  (let ((msgs (list (car entity)))
	msgs-stack children)
    (while msgs
      (setq wl-summary-buffer-number-list (cons (car entity)
						wl-summary-buffer-number-list))
      (setq msgs (cdr msgs))
      (setq children (wl-thread-entity-get-children entity))
      (if children
	  (progn
	    (wl-push msgs msgs-stack)
	    (setq msgs children))
	(unless msgs
	  (while (and (null msgs) msgs-stack)
	    (setq msgs (wl-pop msgs-stack)))))
      (setq entity (wl-thread-get-entity (car msgs))))))

(defun wl-thread-save-entity (dir)
  (wl-thread-save-entities dir)
  (wl-thread-save-top-list dir))

(defun wl-thread-save-top-list (dir)
  (let ((top-file (expand-file-name wl-thread-entity-list-file dir))
	(entity wl-thread-entity-list)
	print-length)
    (with-temp-buffer
      (when (file-writable-p top-file)
	(prin1 entity (current-buffer))
	(princ "\n" (current-buffer))
	(write-region (point-min) (point-max) top-file nil 'no-msg)))))

(defun wl-thread-save-entities (dir)
  (let ((top-file (expand-file-name wl-thread-entity-file dir))
	(entities wl-thread-entities)
	print-length print-level)
    (with-temp-buffer
      (when (file-writable-p top-file)
	(prin1 entities (current-buffer))
	(princ "\n" (current-buffer))
	(write-region (point-min) (point-max) top-file nil 'no-msg)))))

(defsubst wl-thread-entity-get-number (entity)
  (nth 0 entity))
(defsubst wl-thread-entity-get-opened (entity)
  (nth 1 entity))
(defsubst wl-thread-entity-get-children (entity)
  (nth 2 entity))
(defsubst wl-thread-entity-get-parent (entity)
  (nth 3 entity))
(defsubst wl-thread-entity-get-linked (entity)
  (nth 4 entity))

(defsubst wl-thread-create-entity (num parent &optional opened linked)
  (list num (or opened wl-thread-insert-opened) nil parent linked))

(defsubst wl-thread-get-entity (num)
  (and num
       (elmo-get-hash-val (format "#%d" num) wl-thread-entity-hashtb)))

(defsubst wl-thread-entity-set-parent (entity parent)
  (setcar (cdddr entity) parent)
  entity)

(defsubst wl-thread-entity-set-children (entity children)
  (setcar (cddr entity) children))

(defsubst wl-thread-entity-set-linked (entity linked)
  (if (cddddr entity)
      (setcar (cddddr entity) linked)
    (nconc entity (list linked)))
  entity)

(defsubst wl-thread-reparent-children (children parent)
  (while children
    (wl-thread-entity-set-parent
     (wl-thread-get-entity (car children)) parent)
    (wl-thread-entity-set-linked
     (wl-thread-get-entity (car children)) t)
    (setq children (cdr children))))

(defsubst wl-thread-entity-insert-as-top (entity)
  (when (and entity
	     (car entity))
    (wl-append wl-thread-entity-list (list (car entity)))
    (setq wl-thread-entities (cons entity wl-thread-entities))
    (setq wl-summary-buffer-number-list
	  (nconc wl-summary-buffer-number-list (list (car entity))))
    (elmo-set-hash-val (format "#%d" (car entity)) entity
		       wl-thread-entity-hashtb)))

(defsubst wl-thread-entity-insert-as-children (to entity)
  (let ((children (wl-thread-entity-get-children to))
	curp curc)
    (setq curp to)
    (elmo-list-insert wl-summary-buffer-number-list
 		      (wl-thread-entity-get-number entity)
 		      (progn
 			(while (setq curc
 				     (wl-thread-entity-get-children curp))
 			  (setq curp (wl-thread-get-entity
 				      (nth (- (length curc) 1)
 					   curc))))
 			(wl-thread-entity-get-number curp)))
    (wl-thread-entity-set-children to (wl-append children (list (car entity))))
    (setq wl-thread-entities (cons entity wl-thread-entities))
    (elmo-set-hash-val (format "#%d" (car entity)) entity
		       wl-thread-entity-hashtb)))

(defsubst wl-thread-entity-set-opened (entity opened)
  (setcar (cdr entity) opened))

(defsubst wl-thread-entity-get-children-num (entity)
  (let (children
	ret-val msgs-stack
	(msgs (list (car entity))))
   (while msgs
     (setq msgs (cdr msgs))
     (setq children (wl-thread-entity-get-children entity))
     (if (null children)
	 (while (and (null msgs) msgs-stack)
	   (setq msgs (wl-pop msgs-stack)))
       (setq ret-val (+ (or ret-val 0) (length children)))
       (wl-push msgs msgs-stack)
       (setq msgs children))
     (setq entity (wl-thread-get-entity (car msgs))))
   ret-val))

(defun wl-thread-entity-get-descendant (entity)
  (let (children
	ret-val msgs-stack
	(msgs (list (car entity))))
   (while msgs
     (setq msgs (cdr msgs))
     (setq children (wl-thread-entity-get-children entity))
     (if (null children)
	 (while (and (null msgs) msgs-stack)
	   (setq msgs (wl-pop msgs-stack)))
       (setq ret-val (nconc ret-val (copy-sequence children)))
       (wl-push msgs msgs-stack)
       (setq msgs children))
     (setq entity (wl-thread-get-entity (car msgs))))
   ret-val))

(defsubst wl-thread-entity-get-parent-entity (entity)
  (wl-thread-get-entity (wl-thread-entity-get-parent entity)))

(defun wl-thread-entity-get-top-entity (entity)
  (let ((cur-entity entity)
	p-num)
    (while (setq p-num (wl-thread-entity-get-parent cur-entity))
      (setq cur-entity (wl-thread-get-entity p-num)))
    cur-entity))

(defun wl-thread-entity-parent-invisible-p (entity)
  "If parent of ENTITY is invisible, the top invisible ancestor entity of
ENTITY is returned."
  (let ((cur-entity entity)
	top)
    (catch 'done
      (while (setq cur-entity (wl-thread-entity-get-parent-entity
			       cur-entity))
	(if (null (wl-thread-entity-get-number cur-entity))
	    (throw 'done nil)
	  (when (not (wl-thread-entity-get-opened cur-entity))
	    (setq top cur-entity)))))
    top))

(defun wl-thread-entity-get-nearly-older-brother (entity &optional parent)
  (let ((brothers (wl-thread-entity-get-older-brothers entity parent)))
    (when brothers
      (car (last brothers)))))

(defun wl-thread-entity-get-older-brothers (entity &optional parent)
  (let ((parent (or parent
		    (wl-thread-entity-get-parent-entity entity)))
	brothers ret-val)
    (if parent
	(setq brothers (wl-thread-entity-get-children parent))
      (setq brothers wl-thread-entity-list))
    (while (and brothers
		(not (eq (wl-thread-entity-get-number entity)
			 (car brothers))))
      (wl-append ret-val (list (car brothers)))
      (setq brothers (cdr brothers)))
    ret-val))

(defun wl-thread-entity-get-younger-brothers (entity &optional parent)
  (let* ((parent (or parent
		     (wl-thread-entity-get-parent-entity entity)))
	 (brothers (wl-thread-entity-get-children parent)))
    (if parent
	(cdr (memq (wl-thread-entity-get-number entity)
		   brothers))
      ;; top!!
      (cdr (memq (car entity) wl-thread-entity-list)))))

(defun wl-thread-jump-to-msg (&optional number)
  "Jump to the message with specified number in the current summary."
  (interactive)
  (let ((num (or number
		 (string-to-number
		  (read-from-minibuffer "Jump to Message(No.): ")))))
    (wl-thread-entity-force-open (wl-thread-get-entity num))
    (wl-summary-jump-to-msg num)))

(defun wl-thread-close-all ()
  "Close all top threads."
  (interactive)
  (elmo-with-progress-display
      (wl-thread-close-all (length wl-thread-entity-list))
      "Closing all threads"
    (save-excursion
      (dolist (entity wl-thread-entity-list)
	(when (and (wl-thread-entity-get-opened (wl-thread-get-entity
						 entity))
		   (wl-thread-entity-get-children (wl-thread-get-entity
						   entity)))
	  (wl-summary-jump-to-msg entity)
	  (wl-thread-open-close))
	(elmo-progress-notify 'wl-thread-close-all)))))

(defun wl-thread-open-all ()
  "Open all threads."
  (interactive)
  (elmo-with-progress-display
      (wl-thread-open-all (count-lines (point-min) (point-max)))
      "Opening all threads"
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(if (wl-thread-entity-get-opened
	     (wl-thread-get-entity (wl-summary-message-number)))
	    (forward-line)
	  (wl-thread-force-open)
	  (wl-thread-goto-bottom-of-sub-thread))
	(elmo-progress-notify 'wl-thread-open-all)))))

(defun wl-thread-open-all-unread ()
  (interactive)
  (dolist (number (elmo-folder-list-flagged wl-summary-buffer-elmo-folder
					    'digest 'in-msgdb))
    (wl-thread-entity-force-open (wl-thread-get-entity number))))

(defsubst wl-thread-maybe-get-children-num (msg)
  (let ((entity (wl-thread-get-entity msg)))
    (if (not (wl-thread-entity-get-opened entity))
	(wl-thread-entity-get-children-num entity))))

(defsubst wl-thread-update-line-on-buffer-sub (entity msg &optional parent-msg)
  (let* ((entity (or entity (wl-thread-get-entity msg)))
	 (parent-msg (or parent-msg (wl-thread-entity-get-parent entity)))
	 (buffer-read-only nil)
	 (inhibit-read-only t)
	 message-entity temp-mark summary-line invisible-top)
    (if (wl-thread-delete-line-from-buffer msg)
	(progn
	  (cond
	   ((memq msg wl-summary-buffer-target-mark-list)
	    (setq temp-mark "*"))
	   ((setq temp-mark (wl-summary-registered-temp-mark msg))
	    (setq temp-mark (nth 1 temp-mark)))
	   (t (setq temp-mark (wl-summary-get-score-mark msg))))
	  (when (setq message-entity
		      (elmo-message-entity wl-summary-buffer-elmo-folder
					   msg))
	    (wl-summary-insert-line
	     (wl-summary-create-line
	      message-entity
	      (elmo-message-entity wl-summary-buffer-elmo-folder
				   parent-msg)
	      temp-mark
	      (elmo-message-status wl-summary-buffer-elmo-folder msg)
	      (if wl-thread-insert-force-opened
		  nil
		(wl-thread-maybe-get-children-num msg))
	      (wl-thread-make-indent-string entity)
	      (wl-thread-entity-get-linked entity)))))
      ;; insert thread (moving thread)
      (if (not (setq invisible-top
		     (wl-thread-entity-parent-invisible-p entity)))
	  (wl-summary-update-thread
	   (elmo-message-entity wl-summary-buffer-elmo-folder msg)
	   entity
	   (and parent-msg
		(elmo-message-entity wl-summary-buffer-elmo-folder
				     parent-msg)))
	;; currently invisible.. update closed line.
	(wl-thread-update-children-number invisible-top)))))

(defun wl-thread-update-line-on-buffer (&optional msg parent-msg updates)
  (interactive)
  (let ((msgs (list (or msg (wl-summary-message-number))))
	entity children msgs-stack)
   (while msgs
    (setq msg (wl-pop msgs))
    (setq updates (and updates (delete msg updates)))
    (setq entity (wl-thread-get-entity msg))
    (wl-thread-update-line-on-buffer-sub entity msg parent-msg)
    ;;
    (setq children (wl-thread-entity-get-children entity))
    (if children
	;; update children
	(when (wl-thread-entity-get-opened entity)
	  (wl-push msgs msgs-stack)
	  (setq parent-msg msg
		msgs children))
      (unless msgs
	(while (and (null msgs) msgs-stack)
	  (setq msgs (wl-pop msgs-stack)))
	(when msgs
	  (setq parent-msg
		(wl-thread-entity-get-number
		 (wl-thread-entity-get-parent-entity
		  (wl-thread-get-entity (car msgs)))))))))
   updates))

(defun wl-thread-update-line-msgs (msgs)
  (wl-delete-all-overlays)
  (dolist (message msgs)
    (wl-thread-update-line-on-buffer-sub nil message)
    (elmo-progress-notify 'wl-thread-update-line)))

(defun wl-thread-delete-line-from-buffer (msg)
  "Simply delete msg line."
  (let (beg)
    (if (wl-summary-jump-to-msg msg)
	(progn
	  (setq beg (point))
	  (forward-line)
	  (delete-region beg (point))
	  t)
      nil)))

(defun wl-thread-cleanup-symbols (msgs)
  (let (entity)
    (while msgs
      (when (setq entity (wl-thread-get-entity (car msgs)))
	;; delete entity.
	(setq wl-thread-entities (delq entity wl-thread-entities))
	;; free symbol.
	(elmo-clear-hash-val (format "#%d" (car msgs))
			     wl-thread-entity-hashtb))
      (setq msgs (cdr msgs)))))

(defun wl-thread-get-exist-children (msg &optional include-self)
  (let ((msgs (list msg))
	msgs-stack children
	entity ret-val)
    (while msgs
      (setq children (wl-thread-entity-get-children
		      (setq entity (wl-thread-get-entity (car msgs)))))
      (when (elmo-message-entity wl-summary-buffer-elmo-folder (car msgs))
	(wl-append ret-val (list (car msgs)))
	(setq children nil))
      (setq msgs (cdr msgs))
      (if (null children)
	  (while (and (null msgs) msgs-stack)
	    (setq msgs (wl-pop msgs-stack)))
	(wl-push msgs msgs-stack)
	(setq msgs children)))
    (unless include-self (setq ret-val (delq msg ret-val)))
    ret-val))

(defun wl-thread-delete-message (msg &optional deep update)
  "Delete MSG from entity and buffer."
  (save-excursion
    (let ((entity (wl-thread-get-entity msg))
	  top-child top-entity update-msgs invisible-top)
      (setq wl-summary-buffer-number-list
	    (delq msg wl-summary-buffer-number-list))
      (when entity
	(when deep
	  (setq wl-summary-buffer-number-list
		(elmo-list-delete
		 (wl-thread-entity-get-descendant entity)
		 wl-summary-buffer-number-list
		 #'delq)))
	(let ((parent (wl-thread-entity-get-parent-entity entity)))
	  (if parent
	      ;; has parent.
	      (let (children
		    (older-brothers (wl-thread-entity-get-older-brothers
				     entity parent))
		    (younger-brothers (wl-thread-entity-get-younger-brothers
				       entity parent)))
		(unless deep
		  (setq children (wl-thread-entity-get-children entity))
		  (wl-thread-reparent-children
		   children (wl-thread-entity-get-number parent))
		  (setq update-msgs
			(apply (function nconc)
			       update-msgs
			       (mapcar
				(lambda (message)
				  (wl-thread-get-children-msgs message t))
				children))))
		(wl-thread-entity-set-children
		 parent (append older-brothers children younger-brothers))
		;; If chidren and younger-brothers do not exist,
		;; update nearly older brother.
		(when (and older-brothers
			   (not younger-brothers)
			   (not children))
		  (wl-append
		   update-msgs
		   (wl-thread-get-children-msgs (car (last older-brothers))))))
	    ;; top...oldest child becomes top.
	    (unless deep
	      (let ((children (wl-thread-entity-get-children entity)))
		(when children
		  (setq top-child (car children)
			children (cdr children))
		  (setq top-entity (wl-thread-get-entity top-child))
		  (wl-thread-entity-set-parent top-entity nil)
		  (wl-thread-entity-set-linked top-entity nil)
		  (wl-append update-msgs
			     (wl-thread-get-children-msgs top-child t)))
		(when children
		  (wl-thread-entity-set-children
		   top-entity
		   (append
		    (wl-thread-entity-get-children top-entity)
		    children))
		  (wl-thread-reparent-children children top-child)
		  (wl-append update-msgs children))))
	    ;; delete myself from top list.
	    (let ((match (memq msg wl-thread-entity-list)))
	      (when match
		(if top-child
		    (setcar match top-child)
		  (setq wl-thread-entity-list
			(delq msg wl-thread-entity-list))))))))
      ;;
      (if deep
	  ;; delete thread on buffer
	  (when (wl-summary-jump-to-msg msg)
	    (let ((beg (point)))
	      (wl-thread-goto-bottom-of-sub-thread)
	      (delete-region beg (point))))
	;; delete myself from buffer.
	(unless (wl-thread-delete-line-from-buffer msg)
	  ;; jump to suitable point.
	  ;; just upon the oldest younger-brother of my top.
	  (setq invisible-top
		(car (wl-thread-entity-parent-invisible-p entity)))
	  (if invisible-top
	      (progn
		(wl-append update-msgs (list invisible-top))
		(wl-summary-jump-to-msg invisible-top))
	    (goto-char (point-max))))

	;; insert children if thread is closed or delete top.
	(when (or top-child
		  (not (wl-thread-entity-get-opened entity)))
	  (let (next-top insert-msgs ent grandchildren)
	    (if top-child
		(progn
		  (setq insert-msgs (wl-thread-get-exist-children
				     top-child 'include-self))
		  (setq next-top (car insert-msgs))
		  (setq ent (wl-thread-get-entity next-top))
		  (when (and
			 (wl-thread-entity-get-opened entity) ;; open
			 (not (wl-thread-entity-get-opened ent)) ;; close
			 (setq grandchildren
			       (wl-thread-entity-get-children ent))
			 (wl-summary-jump-to-msg next-top))
		    (forward-line)
		    (setq insert-msgs (append (cdr insert-msgs) grandchildren)))
		  (when top-entity (wl-thread-entity-set-opened top-entity t))
		  (when ent (wl-thread-entity-set-opened ent t)))
	      (when (not invisible-top)
		(setq insert-msgs (wl-thread-get-exist-children msg))
		;; First msg always opened, because first msg maybe becomes top.
		(if (setq ent (wl-thread-get-entity (car insert-msgs)))
		    (wl-thread-entity-set-opened ent t))))
	    ;; insert children
	    (while insert-msgs
	      ;; if no exists in summary, insert entity.
	      (when (and (car insert-msgs)
			 (not (wl-summary-jump-to-msg (car insert-msgs))))
		(setq ent (wl-thread-get-entity (car insert-msgs)))
		(wl-thread-insert-entity 0 ; no mean now...
					 ent entity nil))
	      (setq insert-msgs (cdr insert-msgs))))))
      (if update
	  ;; modify buffer.
	  (while update-msgs
	    (wl-thread-update-line-on-buffer-sub nil (pop update-msgs)))
	;; don't update buffer
	update-msgs)))) ; return value

(defun wl-thread-insert-message (message-entity
				 msg parent-msg &optional update linked)
  "Insert MSG to the entity.
When optional argument UPDATE is non-nil,
Message is inserted to the summary buffer."
  (let ((parent (wl-thread-get-entity parent-msg))
	child-entity invisible-top)
;;; Update the thread view...not implemented yet.
;;;    (when force-insert
;;;      (if parent
;;;	  (wl-thread-entity-force-open parent))
    (when (and wl-summary-max-thread-depth parent)
      (let ((cur parent)
	    (depth 0))
	(while cur
	  (incf depth)
	  (setq cur (wl-thread-entity-get-parent-entity cur)))
	(when (> depth wl-summary-max-thread-depth)
	  (setq parent nil
		parent-msg nil))))
    (if parent
	;; insert as children.
	(wl-thread-entity-insert-as-children
	 parent
	 (setq child-entity
	       (wl-thread-create-entity
		msg (wl-thread-entity-get-number parent) nil linked)))
      ;; insert as top message.
      (wl-thread-entity-insert-as-top
       (wl-thread-create-entity msg nil)))
    (if update
	(if (not (setq invisible-top
		       (wl-thread-entity-parent-invisible-p child-entity)))
	    ;; visible.
	    (progn
	      (wl-summary-update-thread
	       message-entity
	       child-entity
	       (elmo-message-entity wl-summary-buffer-elmo-folder
				    parent-msg))
	      (when parent
		;; use thread structure.
;;;		(wl-thread-entity-get-nearly-older-brother
;;;		 child-entity parent))) ; return value
 		(wl-thread-entity-get-number parent))) ; return value
;;; 	      (setq beg (point))
;;; 	      (wl-thread-goto-bottom-of-sub-thread)
;;; 	      (wl-thread-update-indent-string-region beg (point)))
	  ;; currently invisible.. update closed line.
	  (wl-thread-update-children-number invisible-top)
	  nil))))

;;;(defun wl-thread-get-parent-list (msgs)
;;;  ;; return ancestors
;;;  (let* ((msgs2 msgs)
;;;	 myself)
;;;    (while msgs2
;;;      (setq myself (car msgs2)
;;;	    msgs2 (cdr msgs2))
;;;      (while (not (eq myself (car msgs2)))
;;;	(if (wl-thread-descendant-p myself (car msgs2))
;;;	    (setq msgs (delq (car msgs2) msgs)))
;;;	(setq msgs2 (or (cdr msgs2) msgs)))
;;;      (setq msgs2 (cdr msgs2)))
;;;    msgs))

(defun wl-thread-get-parent-list (msgs)
  ;; return connected ancestors
  (let ((ptr msgs)
	parent ret)
    (while (car ptr)
      (setq parent (wl-thread-entity-get-parent (wl-thread-get-entity (car ptr))))
      (when (or (not parent)
		(not (memq parent msgs)))
	(setq ret (append ret (list (car ptr)))))
      (setq ptr (cdr ptr)))
    ret))

(defun wl-thread-update-indent-string-thread (top-list)
  (let ((top-list (wl-thread-get-parent-list top-list))
	beg)
    (elmo-with-progress-display
	(wl-thread-update-indent-string-thread (length top-list))
	"Updating thread indent"
      (while top-list
	(when (car top-list)
	  (wl-summary-jump-to-msg (car top-list))
	  (setq beg (point))
	  (wl-thread-goto-bottom-of-sub-thread)
	  (wl-thread-update-indent-string-region beg (point)))
	(elmo-progress-notify 'wl-thread-update-indent-string-thread)
	(setq top-list (cdr top-list))))))

(defun wl-thread-update-children-number (entity)
  "Update the children number."
  (wl-thread-update-line-on-buffer (wl-thread-entity-get-number entity)))

;;
;; Thread oriented commands.
;;
(defun wl-thread-call-region-func (func &optional arg)
  (save-excursion
    (if arg
	(wl-summary-goto-top-of-current-thread)
      (beginning-of-line))
    (let ((beg (point)))
      (wl-thread-goto-bottom-of-sub-thread)
      (funcall func beg (point)))))

(defun wl-thread-prefetch (&optional arg)
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-prefetch-region arg))

(defun wl-thread-mark-as-read (&optional arg)
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-mark-as-read-region arg))

(defun wl-thread-mark-as-unread (&optional arg)
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-mark-as-unread-region arg))

(defun wl-thread-mark-as-important (&optional arg)
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-mark-as-important-region arg))

(defun wl-thread-set-flags (&optional arg)
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-set-flags-region arg))

(defun wl-thread-mark-as-answered (&optional arg)
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-mark-as-answered-region arg))

(defun wl-thread-recover-messages (&optional arg)
  "Recover killed messages which are contained current thread."
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-recover-messages-region arg))

(defun wl-thread-unmark (&optional arg)
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-unmark-region arg))

(defun wl-thread-exec (&optional arg)
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-exec-region arg))

(defun wl-thread-save (&optional arg)
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-save-region arg))

(defun wl-thread-force-open (&optional msg-num)
  "force open current folder"
  (when msg-num
    (wl-summary-jump-to-msg msg-num))
  (wl-thread-open-close 'force-open))

(defun wl-thread-entity-force-open (entity)
  (let ((wl-thread-insert-force-opened t)
	notopen)
    (if (null (wl-thread-entity-get-parent entity))
	;; top!!
	(if (and (not (wl-thread-entity-get-opened entity))
		 (wl-thread-entity-get-children entity))
	    (wl-thread-force-open (wl-thread-entity-get-number entity)))
      (if (setq notopen (wl-thread-entity-parent-invisible-p entity))
	  (wl-thread-force-open (wl-thread-entity-get-number notopen))))))

(defun wl-thread-insert-top ()
  (let ((elist wl-thread-entity-list)
	(len (length wl-thread-entity-list)))
    (elmo-with-progress-display
	(wl-thread-insert-entity (length wl-thread-entity-list))
	"Inserting message"
      (wl-delete-all-overlays)
      (while elist
	(wl-thread-insert-entity
	 0
	 (wl-thread-get-entity (car elist))
	 nil
	 len)
	(elmo-progress-notify 'wl-thread-insert-entity)
	(setq elist (cdr elist))))))

(defsubst wl-thread-insert-entity-sub (indent entity parent-entity all)
  (let (msg-num
	message-entity
	temp-mark
	summary-line)
    (when (setq msg-num (wl-thread-entity-get-number entity))
      (unless all ; all...means no temp-mark.
	(cond ((memq msg-num wl-summary-buffer-target-mark-list)
	       (setq temp-mark "*"))
	      ((setq temp-mark (wl-summary-registered-temp-mark msg-num))
	       (setq temp-mark (nth 1 temp-mark)))))
      (unless temp-mark
	(setq temp-mark (wl-summary-get-score-mark msg-num)))
      (setq message-entity
	    (elmo-message-entity wl-summary-buffer-elmo-folder
				 msg-num))
;;;      (wl-delete-all-overlays)
      (when message-entity
	(wl-summary-insert-line
	 (wl-summary-create-line
	  message-entity
	  (elmo-message-entity wl-summary-buffer-elmo-folder
			       (wl-thread-entity-get-number parent-entity))
	  temp-mark
	  (elmo-message-status wl-summary-buffer-elmo-folder msg-num)
	  (if wl-thread-insert-force-opened
	      nil
	    (wl-thread-maybe-get-children-num msg-num))
	  (wl-thread-make-indent-string entity)
	  (wl-thread-entity-get-linked entity)))))))

(defun wl-thread-insert-entity (indent entity parent-entity all)
  "Insert thread entity in current buffer."
  (let ((msgs (list (car entity)))
	children msgs-stack)
    (while msgs
      (wl-thread-insert-entity-sub indent entity parent-entity all)
      (setq msgs (cdr msgs))
      (setq children (wl-thread-entity-get-children entity))
      (if children
	  ;; insert children
	  (when (or wl-thread-insert-force-opened
		    (wl-thread-entity-get-opened entity))
	    (wl-thread-entity-set-opened entity t)
	    (wl-push msgs msgs-stack)
	    (setq msgs children
		  indent (1+ indent)
		  parent-entity entity)))
      (unless msgs
	(while (and (null msgs) msgs-stack)
	  (setq msgs (wl-pop msgs-stack))
	  (setq indent (1- indent)))
	(when msgs
	  (setq entity (wl-thread-get-entity (car msgs)))
	  (setq parent-entity (wl-thread-entity-get-parent-entity entity))))
      (setq entity (wl-thread-get-entity (car msgs))))))

(defun wl-thread-descendant-p (mynumber number)
  (let ((cur (wl-thread-get-entity number))
	num)
    (catch 'done
      (while cur
	(setq cur (wl-thread-entity-get-parent-entity cur))
	(if (null (setq num (wl-thread-entity-get-number cur))) ; top!
	    (throw 'done nil))
	(if (and num
		 (eq mynumber (wl-thread-entity-get-number cur)))
	    (throw 'done t)))
      nil)))

;;;(defun wl-thread-goto-bottom-of-sub-thread ()
;;;  (interactive)
;;;  (let ((depth (wl-thread-get-depth-of-current-line)))
;;;    (forward-line)
;;;    (while (and (not (eobp))
;;; 		(> (wl-thread-get-depth-of-current-line)
;;; 		   depth))
;;;      (forward-line))
;;;    (beginning-of-line)))

(defun wl-thread-goto-bottom-of-sub-thread (&optional msg)
  (interactive)
  (let ((mynumber (or msg (wl-summary-message-number))))
    (forward-line)
    (while (wl-thread-descendant-p mynumber (wl-summary-message-number))
      (forward-line))
    (beginning-of-line)))

(defun wl-thread-remove-argument-region (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
	(wl-summary-remove-argument)
	(forward-line)))))

(defun wl-thread-print-argument-region (beg end)
  (if wl-summary-buffer-temp-mark-list
      (save-excursion
	(save-restriction
	  (narrow-to-region beg end)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (let ((num (wl-summary-message-number))
		  temp-mark pair)
	      (when (and (setq temp-mark
			       (wl-summary-registered-temp-mark num))
			 (nth 2 temp-mark)
			 (setq pair (cons (nth 0 temp-mark)(nth 2 temp-mark))))
		(wl-summary-print-argument (car pair) (cdr pair))))
	    (forward-line))))))

(defsubst wl-thread-get-children-msgs (msg &optional visible-only)
  (let ((msgs (list msg))
	msgs-stack children
	entity ret-val)
    (while msgs
      (wl-append ret-val (list (car msgs)))
      (setq children (wl-thread-entity-get-children
		      (setq entity (wl-thread-get-entity (car msgs)))))
      (if (and visible-only
	       (not (wl-thread-entity-get-opened entity)))
	  (setq children nil))
      (setq msgs (cdr msgs))
      (if (null children)
	  (while (and (null msgs) msgs-stack)
	    (setq msgs (wl-pop msgs-stack)))
	(wl-push msgs msgs-stack)
	(setq msgs children)))
    ret-val))

(defun wl-thread-get-children-msgs-uncached (msg &optional uncached-marks)
  (let ((children-msgs (wl-thread-get-children-msgs msg))
	mark uncached-list)
    (while children-msgs
      (if (and (not (eq msg (car children-msgs))) ; except itself
	       (or (and uncached-marks
			(setq mark (wl-summary-message-mark
				    wl-summary-buffer-elmo-folder
				    (car children-msgs)))
			(member mark uncached-marks))
		   (and (not uncached-marks)
			(null (elmo-file-cache-exists-p
			       (elmo-message-field
				wl-summary-buffer-elmo-folder
				(car children-msgs)
				'message-id))))))
	  (wl-append uncached-list (list (car children-msgs))))
      (setq children-msgs (cdr children-msgs)))
    uncached-list))

(defun wl-thread-get-children-msgs-with-mark (msg mark)
  (let ((children-msgs (wl-thread-get-children-msgs msg))
	(check-func (cond ((string= mark "o")
			   'wl-summary-msg-marked-as-refiled)
			  ((string= mark "O")
			   'wl-summary-msg-marked-as-copied)
			  ((string= mark "D")
			   'wl-summary-msg-marked-as-deleted)
			  ((string= mark "*")
			   'wl-summary-msg-marked-as-target)))
	ret-val)
    (while children-msgs
      (if (funcall check-func (car children-msgs))
	  (wl-append ret-val (list (car children-msgs))))
      (setq children-msgs (cdr children-msgs)))
    ret-val))

(defun wl-thread-close (entity)
  (let (depth beg)
    (wl-thread-entity-set-opened entity nil)
    (setq depth (wl-thread-get-depth-of-current-line))
    (setq beg (point-at-bol))
    (wl-thread-goto-bottom-of-sub-thread)
    (wl-thread-remove-argument-region beg
				      (point))
    (backward-char)	;; needed for mouse-face.
    (delete-region beg (point))
    (wl-thread-insert-entity (- depth 1)
			     entity
			     (wl-thread-get-entity
			      (wl-thread-entity-get-parent entity))
			     nil)
    (delete-char 1) ; delete '\n'
    (wl-thread-print-argument-region beg (point))))

(defun wl-thread-close-children (&optional number)
  (interactive)
  (when (eq wl-summary-buffer-view 'thread)
    (setq number (or number (wl-summary-message-number)))
    (save-excursion
      (let ((inhibit-read-only t)
	    (entity (wl-thread-get-entity number)))
	(when (wl-thread-entity-get-opened entity)
	  (wl-thread-close entity))))))

(defun wl-thread-open (entity)
  (let (depth beg)
    (setq beg (point-at-bol))
    (setq depth (wl-thread-get-depth-of-current-line))
    (delete-region (point-at-bol) (point-at-eol))
    (wl-thread-entity-set-opened entity t)
    (wl-thread-insert-entity depth ;(- depth 1)
			     entity
			     (wl-thread-get-entity
			      (wl-thread-entity-get-parent entity))
			     nil)
    (delete-char 1) ; delete '\n'
    (wl-thread-print-argument-region beg (point))))

(defun wl-thread-open-children (&optional number)
  (interactive)
  (when (eq wl-summary-buffer-view 'thread)
    (setq number (or number (wl-summary-message-number)))
    (save-excursion
      (let ((inhibit-read-only t)
	    (entity (wl-thread-get-entity number)))
	(unless (wl-thread-entity-get-opened entity)
	  (wl-thread-open entity))))))

(defun wl-thread-open-close (&optional force-open)
  (interactive "P")
  (when (eq wl-summary-buffer-view 'thread)
;;;    (if (equal wl-thread-top-entity '(nil t nil nil))
;;;	(error "There's no thread structure"))
    (save-excursion
      (let ((inhibit-read-only t)
	    (buffer-read-only nil)
	    (wl-thread-insert-force-opened
	     (or wl-thread-insert-force-opened
		 force-open))
	    msg entity parent)
	(setq msg (wl-summary-message-number))
	(setq entity (wl-thread-get-entity msg))
	(if (wl-thread-entity-get-opened entity)
	    ;; if already opened, close its child!
	  (if (wl-thread-entity-get-children entity)
	      (wl-thread-close entity)
	    ;; opened, but has no children, close its parent!
	    (when (setq parent (wl-thread-entity-get-parent entity))
	      (wl-summary-jump-to-msg parent)
	      (wl-thread-close
	       (wl-thread-get-entity (wl-summary-message-number)))))
	  ;; if closed (or it is just a thread bottom message)
	  ;; has children, open it!
	  (if (wl-thread-entity-get-children entity)
	      (wl-thread-open entity)
	    ;; closed, and has no children, close its parent!
	    (setq msg (or (wl-thread-entity-get-parent entity)
			  (wl-thread-entity-get-number entity)))
	    (when msg
	      (wl-summary-jump-to-msg msg)
	      (wl-thread-close
	       (wl-thread-get-entity (wl-summary-message-number)))))))
      (when wl-summary-lazy-highlight
	(wl-highlight-summary-window))
      (wl-summary-set-message-modified)
      (set-buffer-modified-p nil))))

(defun wl-thread-get-depth-of-current-line ()
  (let ((entity (wl-thread-get-entity (wl-summary-message-number)))
	(depth 0)
	number)
    (while (setq number (wl-thread-entity-get-parent entity))
      (incf depth)
      (setq entity (wl-thread-get-entity number)))
    depth))

(defun wl-thread-update-indent-string-region (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (save-excursion
	(wl-thread-update-line-on-buffer-sub nil (wl-summary-message-number)))
      (forward-line))))

(defsubst wl-thread-make-indent-string (entity)
  (let ((cur entity)
	(ret-val "")
	(space-str (wl-repeat-string wl-thread-space-str-internal
				     (- wl-thread-indent-level-internal 1)))
	parent)
    (when (wl-thread-entity-get-number
	   (setq parent (wl-thread-entity-get-parent-entity cur)))
      (if (wl-thread-entity-get-younger-brothers cur)
	  (setq ret-val wl-thread-have-younger-brother-str-internal)
	(setq ret-val wl-thread-youngest-child-str-internal))
      (setq ret-val (concat ret-val
			    (wl-repeat-string
			     wl-thread-horizontal-str-internal
			     (- wl-thread-indent-level-internal 1))))
      (setq cur parent)
      (while (wl-thread-entity-get-number
	      (wl-thread-entity-get-parent-entity cur))
	(if (wl-thread-entity-get-younger-brothers cur)
	    (setq ret-val (concat wl-thread-vertical-str-internal
				  space-str
				  ret-val))
	  (setq ret-val (concat wl-thread-space-str-internal
				space-str
				ret-val)))
	(setq cur (wl-thread-entity-get-parent-entity cur))))
    ret-val))

(defun wl-thread-set-parent (&optional parent-number)
  "Set current message's parent interactively."
  (interactive)
  (let ((number (wl-summary-message-number))
	(dst-parent (if (interactive-p)
			(read-from-minibuffer "Parent Message (No.): ")))
	entity dst-parent-entity src-parent children
	update-msgs
	buffer-read-only)
    (if (string= dst-parent "")
	(setq dst-parent nil)
      (if (interactive-p)
	  (setq dst-parent (string-to-number dst-parent))
	(setq dst-parent parent-number)))
    (if (and dst-parent
	     (memq dst-parent (wl-thread-get-children-msgs number)))
	(error "Parent is children or myself"))
    (setq entity (wl-thread-get-entity number))
    (when (and number entity)
      ;; delete thread
      (setq update-msgs (wl-thread-delete-message number 'deep))
      ;; insert as child at new parent
      (setq dst-parent-entity (wl-thread-get-entity dst-parent))
      (if dst-parent-entity
	  (progn
	    (if (setq children
		      (wl-thread-entity-get-children dst-parent-entity))
		(wl-append update-msgs
			   (wl-thread-get-children-msgs
			    (car (last children)) t)))
	    (wl-thread-entity-set-children
	     dst-parent-entity
	     (append children (list number)))
	    (wl-thread-entity-set-linked
	     entity
	     (let ((parent (elmo-message-entity-parent
			    wl-summary-buffer-elmo-folder
			    (elmo-message-entity
			     wl-summary-buffer-elmo-folder
			     number))))
	       (or (null parent)
		   (/= parent-number (elmo-message-entity-number parent))))))
	;; insert as top
	(wl-append wl-thread-entity-list (list number))
	(wl-thread-entity-set-linked entity nil))

      ;; update my thread
      (wl-append update-msgs (wl-thread-get-children-msgs number t))
      (setq update-msgs (elmo-uniq-list update-msgs))
      (wl-thread-entity-set-parent entity dst-parent)
      ;; update thread on buffer
      (wl-thread-make-number-list)
      (wl-thread-update-line-msgs update-msgs))))

(require 'product)
(product-provide (provide 'wl-thread) (require 'wl-version))

;;; wl-thread.el ends here
