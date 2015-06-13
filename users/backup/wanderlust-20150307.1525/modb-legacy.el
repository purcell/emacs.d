;;; modb-legacy.el --- Legacy Implement of MODB.

;; Copyright (C) 2003 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Hiroya Murata <lapis-lazuli@pop06.odn.ne.jp>
;; Keywords: mail, net news

;; This file is part of ELMO (Elisp Library for Message Orchestration).

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

(eval-when-compile (require 'cl))

(require 'elmo-util)
(require 'modb)

;;; legacy implement
;;

(defconst modb-legacy-new-mark "N"
  "Mark for new message.")

(defconst modb-legacy-unread-uncached-mark "U"
  "Mark for unread and uncached message.")

(defconst modb-legacy-unread-cached-mark "!"
  "Mark for unread but already cached message.")

(defconst modb-legacy-read-uncached-mark "u"
  "Mark for read but uncached message.")

(defconst modb-legacy-answered-cached-mark "&"
  "Mark for answered and cached message.")

(defconst modb-legacy-answered-uncached-mark "A"
  "Mark for answered but uncached message.")

(defconst modb-legacy-important-mark "$"
  "Mark for important message.")

(defconst modb-legacy-flag-list
  '(new unread important answered cached read uncached)
  "A list of flag symbol which is supported by legacy msgdb.")

(eval-and-compile
  (luna-define-class modb-legacy (modb-generic)
		     (overview number-alist mark-alist index))
  (luna-define-internal-accessors 'modb-legacy))

;; for internal use only
(defsubst elmo-msgdb-get-overview (msgdb)
  (modb-legacy-overview-internal msgdb))

(defsubst elmo-msgdb-get-number-alist (msgdb)
  (modb-legacy-number-alist-internal msgdb))

(defsubst elmo-msgdb-get-mark-alist (msgdb)
  (modb-legacy-mark-alist-internal msgdb))

(defsubst elmo-msgdb-get-index (msgdb)
  (modb-legacy-index-internal msgdb))

(defsubst elmo-msgdb-get-entity-hashtb (msgdb)
  (car (modb-legacy-index-internal msgdb)))

(defsubst elmo-msgdb-get-mark-hashtb (msgdb)
  (cdr (modb-legacy-index-internal msgdb)))

(defsubst elmo-msgdb-get-path (msgdb)
  (elmo-msgdb-location msgdb))

(defsubst elmo-msgdb-set-overview (msgdb overview)
  (modb-legacy-set-overview-internal msgdb overview))

(defsubst elmo-msgdb-set-number-alist (msgdb number-alist)
  (modb-legacy-set-number-alist-internal msgdb number-alist))

(defsubst elmo-msgdb-set-mark-alist (msgdb mark-alist)
  (modb-legacy-set-mark-alist-internal msgdb mark-alist))

(defsubst elmo-msgdb-set-index (msgdb index)
  (modb-legacy-set-index-internal msgdb index))

(defsubst elmo-msgdb-set-path (msgdb path)
  (modb-generic-set-location-internal msgdb path))

;;;
;; Internal use only (obsolete interface)
;;
(defsubst elmo-msgdb-overview-entity-get-id-internal (entity)
  (and entity (car entity)))

(defsubst elmo-msgdb-overview-entity-get-number-internal (entity)
  (and entity (aref (cdr entity) 0)))

;;; load & save
(defun elmo-msgdb-number-load (dir)
  (elmo-object-load
   (expand-file-name elmo-msgdb-number-filename dir)))

(defun elmo-msgdb-overview-load (dir)
  (elmo-object-load
   (expand-file-name elmo-msgdb-overview-filename dir)))

(defun elmo-msgdb-mark-load (dir)
  (elmo-object-load
   (expand-file-name elmo-msgdb-mark-filename dir)))

(defun elmo-msgdb-number-save (dir obj)
  (elmo-object-save
   (expand-file-name elmo-msgdb-number-filename dir)
   obj))

(defun elmo-msgdb-mark-save (dir obj)
  (elmo-object-save
   (expand-file-name elmo-msgdb-mark-filename dir)
   obj))

(defsubst elmo-msgdb-overview-save (dir overview)
  (elmo-object-save
   (expand-file-name elmo-msgdb-overview-filename dir)
   overview))

;;;

(defsubst modb-legacy-supported-flag-p (flag)
  (memq flag modb-legacy-flag-list))

(defvar modb-legacy-unread-marks-internal nil)
(defsubst modb-legacy-unread-marks ()
  "Return an unread mark list"
  (or modb-legacy-unread-marks-internal
      (setq modb-legacy-unread-marks-internal
	    (list modb-legacy-new-mark
		  modb-legacy-unread-uncached-mark
		  modb-legacy-unread-cached-mark))))

(defvar modb-legacy-answered-marks-internal nil)
(defsubst modb-legacy-answered-marks ()
  "Return an answered mark list"
  (or modb-legacy-answered-marks-internal
      (setq modb-legacy-answered-marks-internal
	    (list modb-legacy-answered-cached-mark
		  modb-legacy-answered-uncached-mark))))

(defvar modb-legacy-uncached-marks-internal nil)
(defsubst modb-legacy-uncached-marks ()
  (or modb-legacy-uncached-marks-internal
      (setq modb-legacy-uncached-marks-internal
	    (list modb-legacy-new-mark
		  modb-legacy-answered-uncached-mark
		  modb-legacy-unread-uncached-mark
		  modb-legacy-read-uncached-mark))))

(defsubst modb-legacy-mark-to-flags (mark)
  (append
   (and (string= mark modb-legacy-new-mark)
	'(new))
   (and (string= mark modb-legacy-important-mark)
	'(important))
   (and (member mark (modb-legacy-unread-marks))
	'(unread))
   (and (member mark (modb-legacy-answered-marks))
	'(answered))
   (and (not (member mark (modb-legacy-uncached-marks)))
	'(cached))))

(defsubst modb-legacy-flags-to-mark (flags)
  (cond ((memq 'new flags)
	 modb-legacy-new-mark)
	((memq 'important flags)
	 modb-legacy-important-mark)
	((memq 'answered flags)
	 (if (memq 'cached flags)
	     modb-legacy-answered-cached-mark
	   modb-legacy-answered-uncached-mark))
	((memq 'unread flags)
	 (if (memq 'cached flags)
	     modb-legacy-unread-cached-mark
	   modb-legacy-unread-uncached-mark))
	(t
	 (if (memq 'cached flags)
	     nil
	   modb-legacy-read-uncached-mark))))

(defsubst elmo-msgdb-get-mark (msgdb number)
  "Get mark string from MSGDB which corresponds to the message with NUMBER."
  (cadr (elmo-get-hash-val (format "#%d" number)
			   (elmo-msgdb-get-mark-hashtb msgdb))))

(defsubst elmo-msgdb-set-mark (msgdb number mark)
  "Set MARK of the message with NUMBER in the MSGDB.
if MARK is nil, mark is removed."
  (let ((elem (elmo-get-hash-val (format "#%d" number)
				 (elmo-msgdb-get-mark-hashtb msgdb))))
    (if elem
	(if mark
	    ;; Set mark of the elem
	    (setcar (cdr elem) mark)
	  ;; Delete elem from mark-alist
	  (elmo-msgdb-set-mark-alist
	   msgdb
	   (delq elem (elmo-msgdb-get-mark-alist msgdb)))
	  (elmo-clear-hash-val (format "#%d" number)
			       (elmo-msgdb-get-mark-hashtb msgdb)))
      (when mark
	;; Append new element.
	(elmo-msgdb-set-mark-alist
	 msgdb
	 (nconc
	  (elmo-msgdb-get-mark-alist msgdb)
	  (list (setq elem (list number mark)))))
	(elmo-set-hash-val (format "#%d" number) elem
			   (elmo-msgdb-get-mark-hashtb msgdb))))
    (modb-generic-set-flag-modified-internal msgdb t)
    ;; return value.
    t))

(defun elmo-msgdb-make-index (msgdb &optional overview mark-alist)
  "Append OVERVIEW and MARK-ALIST to the index of MSGDB.
If OVERVIEW and MARK-ALIST are nil, make index for current MSGDB.
Return a list of message numbers which have duplicated message-ids."
  (when msgdb
    (let* ((overview (or overview (elmo-msgdb-get-overview msgdb)))
	   (mark-alist (or mark-alist (elmo-msgdb-get-mark-alist msgdb)))
	   (index (elmo-msgdb-get-index msgdb))
	   (ehash (or (car index) ;; append
		      (elmo-make-hash (length overview))))
	   (mhash (or (cdr index) ;; append
		      (elmo-make-hash (length overview))))
	   duplicates)
      (while overview
	;; key is message-id
	(if (elmo-get-hash-val (caar overview) ehash) ; duplicated.
	    (setq duplicates (cons
			      (elmo-msgdb-overview-entity-get-number-internal
			       (car overview))
			      duplicates)))
	(if (caar overview)
	    (elmo-set-hash-val (caar overview) (car overview) ehash))
	;; key is number
	(elmo-set-hash-val
	 (format "#%d"
		 (elmo-msgdb-overview-entity-get-number-internal
		  (car overview)))
	 (car overview) ehash)
	(setq overview (cdr overview)))
      (while mark-alist
	;; key is number
	(elmo-set-hash-val
	 (format "#%d" (car (car mark-alist)))
	 (car mark-alist) mhash)
	(setq mark-alist (cdr mark-alist)))
      (setq index (or index (cons ehash mhash)))
      (elmo-msgdb-set-index msgdb index)
      duplicates)))

(defun elmo-msgdb-clear-index (msgdb entity)
  (let ((ehash (elmo-msgdb-get-entity-hashtb msgdb))
	(mhash (elmo-msgdb-get-mark-hashtb msgdb))
	number)
    (when (and entity ehash)
      (and (setq number (elmo-msgdb-overview-entity-get-number-internal
			 entity))
	   (elmo-clear-hash-val (format "#%d" number) ehash))
      (and (car entity) ;; message-id
	   (elmo-clear-hash-val (car entity) ehash)))
    (when (and entity mhash)
      (and (setq number (elmo-msgdb-overview-entity-get-number-internal
			 entity))
	   (elmo-clear-hash-val (format "#%d" number) mhash)))))

;;; Implement
;;
(luna-define-method elmo-msgdb-load ((msgdb modb-legacy))
  (let ((inhibit-quit t)
	(path (elmo-msgdb-location msgdb)))
    (when (file-exists-p (expand-file-name elmo-msgdb-mark-filename path))
      (modb-legacy-set-overview-internal
       msgdb
       (elmo-msgdb-overview-load path))
      (modb-legacy-set-number-alist-internal
       msgdb
       (elmo-msgdb-number-load path))
      (modb-legacy-set-mark-alist-internal
       msgdb
       (elmo-msgdb-mark-load path))
      (elmo-msgdb-make-index msgdb)
      t)))

(luna-define-method elmo-msgdb-save ((msgdb modb-legacy))
  (let ((path (elmo-msgdb-location msgdb)))
    (when (elmo-msgdb-message-modified-p msgdb)
      (elmo-msgdb-overview-save
       path
       (modb-legacy-overview-internal msgdb))
      (elmo-msgdb-number-save
       path
       (modb-legacy-number-alist-internal msgdb))
      (modb-generic-set-message-modified-internal msgdb nil))
    (when (elmo-msgdb-flag-modified-p msgdb)
      (elmo-msgdb-mark-save
       path
       (modb-legacy-mark-alist-internal msgdb))
      (modb-generic-set-flag-modified-internal msgdb nil))))

(luna-define-method elmo-msgdb-append :around ((msgdb modb-legacy)
					       msgdb-append)
  (if (eq (luna-class-name msgdb-append)
	  'modb-legacy)
      (let (duplicates)
	(elmo-msgdb-set-overview
	 msgdb
	 (nconc (elmo-msgdb-get-overview msgdb)
		(elmo-msgdb-get-overview msgdb-append)))
	(elmo-msgdb-set-number-alist
	 msgdb
	 (nconc (elmo-msgdb-get-number-alist msgdb)
		(elmo-msgdb-get-number-alist msgdb-append)))
	(elmo-msgdb-set-mark-alist
	 msgdb
	 (nconc (elmo-msgdb-get-mark-alist msgdb)
		(elmo-msgdb-get-mark-alist msgdb-append)))
	(setq duplicates (elmo-msgdb-make-index
			  msgdb
			  (elmo-msgdb-get-overview msgdb-append)
			  (elmo-msgdb-get-mark-alist msgdb-append)))
	(elmo-msgdb-set-path
	 msgdb
	 (or (elmo-msgdb-get-path msgdb)
	     (elmo-msgdb-get-path msgdb-append)))
	(modb-generic-set-message-modified-internal msgdb t)
	(modb-generic-set-flag-modified-internal msgdb t)
	duplicates)
    (luna-call-next-method)))

(luna-define-method elmo-msgdb-clear :after ((msgdb modb-legacy))
  (elmo-msgdb-set-overview msgdb nil)
  (elmo-msgdb-set-number-alist msgdb nil)
  (elmo-msgdb-set-mark-alist msgdb nil)
  (elmo-msgdb-set-index msgdb nil))

(luna-define-method elmo-msgdb-length ((msgdb modb-legacy))
  (length (modb-legacy-overview-internal msgdb)))

(luna-define-method elmo-msgdb-flag-available-p ((msgdb modb-legacy) flag)
  (modb-legacy-supported-flag-p flag))

(luna-define-method elmo-msgdb-flags ((msgdb modb-legacy) number)
  (modb-legacy-mark-to-flags (elmo-msgdb-get-mark msgdb number)))

(luna-define-method elmo-msgdb-set-flag ((msgdb modb-legacy)
					 number flag)
  (unless (modb-legacy-supported-flag-p flag)
    (error "Flag `%s' is not supported by this msgdb type"
	   (capitalize (symbol-name flag))))
  (case flag
    (read
     (elmo-msgdb-unset-flag msgdb number 'unread))
    (uncached
     (elmo-msgdb-unset-flag msgdb number 'cached))
    (t
     (let* ((cur-mark (elmo-msgdb-get-mark msgdb number))
	    (flags (modb-legacy-mark-to-flags cur-mark))
	    new-mark)
       (and (memq 'new flags)
	    (setq flags (delq 'new flags)))
       (or (memq flag flags)
	   (setq flags (cons flag flags)))
       (when (and (eq flag 'unread)
		  (memq 'answered flags))
	 (setq flags (delq 'answered flags)))
       (setq new-mark (modb-legacy-flags-to-mark flags))
       (unless (string= new-mark cur-mark)
	 (elmo-msgdb-set-mark msgdb number new-mark))))))

(luna-define-method elmo-msgdb-unset-flag ((msgdb modb-legacy)
					   number flag)
  (unless (or (modb-legacy-supported-flag-p flag)
	      (eq flag 'all))
    (error "Flag `%s' is not supported by this msgdb type"
	   (capitalize (symbol-name flag))))
  (case flag
    (read
     (elmo-msgdb-set-flag msgdb number 'unread))
    (uncached
     (elmo-msgdb-set-flag msgdb number 'cached))
    (all
     (elmo-msgdb-set-mark msgdb number nil))
    (t
     (let* ((cur-mark (elmo-msgdb-get-mark msgdb number))
	    (flags (modb-legacy-mark-to-flags cur-mark))
	    new-mark)
       (and (memq 'new flags)
	    (setq flags (delq 'new flags)))
       (and (memq flag flags)
	    (setq flags (delq flag flags)))
       (when (and (eq flag 'unread)
		  (memq 'answered flags))
	 (setq flags (delq 'answered flags)))
       (setq new-mark (modb-legacy-flags-to-mark flags))
       (unless (string= new-mark cur-mark)
	 (elmo-msgdb-set-mark msgdb number new-mark))))))

(luna-define-method elmo-msgdb-flag-count ((msgdb modb-legacy))
  (let ((new 0)
	(unread 0)
	(answered 0))
    (dolist (elem (elmo-msgdb-get-mark-alist msgdb))
      (cond
       ((string= (cadr elem) modb-legacy-new-mark)
	(incf new)
	(incf unread))
       ((member (cadr elem) (modb-legacy-unread-marks))
	(incf unread))
       ((member (cadr elem) (modb-legacy-answered-marks))
	(incf answered))))
    (list (cons 'new new)
	  (cons 'unread unread)
	  (cons 'answered answered))))

(luna-define-method elmo-msgdb-list-messages ((msgdb modb-legacy))
  (mapcar 'elmo-msgdb-overview-entity-get-number-internal
	  (elmo-msgdb-get-overview msgdb)))

(luna-define-method elmo-msgdb-list-flagged ((msgdb modb-legacy) flag)
  (let ((case-fold-search nil)
	mark-regexp matched)
    (case flag
      (new
       (setq mark-regexp (regexp-quote modb-legacy-new-mark)))
      (unread
       (setq mark-regexp (elmo-regexp-opt (modb-legacy-unread-marks))))
      (answered
       (setq mark-regexp (elmo-regexp-opt (modb-legacy-answered-marks))))
      (important
       (setq mark-regexp (regexp-quote modb-legacy-important-mark)))
      (read
       (setq mark-regexp (elmo-regexp-opt (modb-legacy-unread-marks))))
      (digest
       (setq mark-regexp (elmo-regexp-opt
			  (append (modb-legacy-unread-marks)
				  (list modb-legacy-important-mark)))))
      (any
       (setq mark-regexp (elmo-regexp-opt
			  (append
			   (modb-legacy-unread-marks)
			   (modb-legacy-answered-marks)
			   (list modb-legacy-important-mark))))))
    (when mark-regexp
      (if (eq flag 'read)
	  (dolist (number (elmo-msgdb-list-messages msgdb))
	    (let ((mark (elmo-msgdb-get-mark msgdb number)))
	      (unless (and mark (string-match mark-regexp mark))
		(setq matched (cons number matched)))))
	(dolist (elem (elmo-msgdb-get-mark-alist msgdb))
	  (if (string-match mark-regexp (cadr elem))
	      (setq matched (cons (car elem) matched))))))
    matched))

(luna-define-method elmo-msgdb-search ((msgdb modb-legacy)
				       condition &optional numbers)
  (if (vectorp condition)
      (let ((key (elmo-filter-key condition))
	    results)
	(cond
	 ((and (string= key "flag")
	       (eq (elmo-filter-type condition) 'match))
	  (setq results (elmo-msgdb-list-flagged
			 msgdb
			 (intern (elmo-filter-value condition))))
	  (if numbers
	      (elmo-list-filter numbers results)
	    results))
	 ((member key '("first" "last"))
	  (let* ((numbers (or numbers (elmo-msgdb-list-messages msgdb)))
		 (len (length numbers))
		 (lastp (string= key "last"))
		 (value (string-to-number (elmo-filter-value condition))))
	    (when (eq (elmo-filter-type condition) 'unmatch)
	      (setq lastp (not lastp)
		    value (- len value)))
	    (if lastp
		(nthcdr (max (- len value) 0) numbers)
	      (when (> value 0)
		(let* ((numbers (copy-sequence numbers))
		       (last (nthcdr (1- value) numbers)))
		  (when last
		    (setcdr last nil))
		  numbers)))))
	 (t
	  t)))
    t))

(luna-define-method elmo-msgdb-append-entity ((msgdb modb-legacy)
					      entity &optional flags)
  (when entity
    (let ((number (elmo-msgdb-overview-entity-get-number-internal entity))
	  (message-id (elmo-msgdb-overview-entity-get-id-internal entity))
	  mark cell)
      (when (and number message-id)
	(elmo-msgdb-set-overview
	 msgdb
	 (nconc (elmo-msgdb-get-overview msgdb)
		(list entity)))
	(elmo-msgdb-set-number-alist
	 msgdb
	 (nconc (elmo-msgdb-get-number-alist msgdb)
		(list (cons number message-id))))
	(modb-generic-set-message-modified-internal msgdb t)
	(when (setq mark (modb-legacy-flags-to-mark flags))
	  (setq cell (list number mark))
	  (elmo-msgdb-set-mark-alist
	   msgdb
	   (nconc (elmo-msgdb-get-mark-alist msgdb) (list cell)))
	  (modb-generic-set-flag-modified-internal msgdb t))
	(elmo-msgdb-make-index
	 msgdb
	 (list entity)
	 (and cell (list cell)))))))

(luna-define-method elmo-msgdb-delete-messages ((msgdb modb-legacy)
						numbers)
  (let* ((overview (elmo-msgdb-get-overview msgdb))
	 (number-alist (elmo-msgdb-get-number-alist msgdb))
	 (mark-alist (elmo-msgdb-get-mark-alist msgdb))
	 (index (elmo-msgdb-get-index msgdb))
	 ov-entity)
    ;; remove from current database.
    (dolist (number numbers)
      (setq overview
	    (delq
	     (setq ov-entity
		   (elmo-msgdb-message-entity msgdb number))
	     overview))
      (setq number-alist (delq (assq number number-alist) number-alist))
      (setq mark-alist (delq (assq number mark-alist) mark-alist))
      ;;
      (when index (elmo-msgdb-clear-index msgdb ov-entity)))
    (elmo-msgdb-set-overview msgdb overview)
    (elmo-msgdb-set-number-alist msgdb number-alist)
    (elmo-msgdb-set-mark-alist msgdb mark-alist)
    (elmo-msgdb-set-index msgdb index)
    (modb-generic-set-message-modified-internal msgdb t)
    (modb-generic-set-flag-modified-internal msgdb t)
    t)) ;return value

(luna-define-method elmo-msgdb-sort-entities ((msgdb modb-legacy)
					      predicate &optional app-data)
  (message "Sorting...")
  (let ((overview (elmo-msgdb-get-overview msgdb)))
    (elmo-msgdb-set-overview
     msgdb
     (sort overview (lambda (a b) (funcall predicate a b app-data))))
    (message "Sorting...done")
    msgdb))

(luna-define-method elmo-msgdb-message-entity ((msgdb modb-legacy) key)
  (when key
    (elmo-get-hash-val
     (cond ((stringp key) key)
	   ((numberp key) (format "#%d" key)))
     (elmo-msgdb-get-entity-hashtb msgdb))))

(require 'product)
(product-provide (provide 'modb-legacy) (require 'elmo-version))

;;; modb-legacy.el ends here
