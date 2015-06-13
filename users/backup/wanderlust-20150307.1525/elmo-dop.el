;;; elmo-dop.el --- Modules for Disconnected Operations on ELMO.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
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

(require 'elmo)
(require 'elmo-vars)
(require 'elmo-msgdb)
(require 'elmo-util)
(require 'elmo-localdir)

;; global variable.
(defvar elmo-dop-queue nil
  "A list of (folder-name function-to-be-called argument-list).
Automatically loaded/saved.")

(defmacro elmo-make-dop-queue (fname method arguments)
  "Make a dop queue."
  `(vector ,fname ,method ,arguments))

(defmacro elmo-dop-queue-fname (queue)
  "Return the folder name string of the QUEUE."
  `(aref ,queue 0))

(defmacro elmo-dop-queue-method (queue)
  "Return the method symbol of the QUEUE."
  `(aref ,queue 1))

(defmacro elmo-dop-queue-arguments (queue)
  "Return the arguments of the QUEUE."
  `(aref ,queue 2))

(defun elmo-dop-queue-append (folder method arguments)
  "Append to disconnected operation queue."
  (let ((queue (elmo-make-dop-queue (elmo-folder-name-internal folder)
				    method arguments)))
    (setq elmo-dop-queue (nconc elmo-dop-queue (list queue)))))

(defvar elmo-dop-queue-merge-method-list
  '(elmo-folder-set-read-delayed
    elmo-folder-unset-read-delayed
    elmo-folder-set-important-delayed
    elmo-folder-unset-important-delayed
    elmo-folder-set-answered-delayed
    elmo-folder-unset-answered-delayed))

(defvar elmo-dop-queue-method-name-alist
  '((elmo-folder-append-buffer-dop-delayed . "Append")
    (elmo-folder-delete-messages-dop-delayed . "Delete")
    (elmo-message-encache . "Encache")
    (elmo-folder-create-dop-delayed . "Create")
    (elmo-folder-set-read-delayed . "Read")
    (elmo-folder-unset-read-delayed . "Unread")
    (elmo-folder-set-answered-delayed . "Answered")
    (elmo-folder-unset-answered-delayed . "Unanswered")
    (elmo-folder-set-important-delayed . "Important")
    (elmo-folder-unset-important-delayed . "Unimportant")
    (elmo-folder-set-flag . "Set flag")
    (elmo-folder-unset-flag . "Unset flag")))

(defmacro elmo-dop-queue-method-name (queue)
  `(cdr (assq (elmo-dop-queue-method ,queue)
	      elmo-dop-queue-method-name-alist)))

(defun elmo-dop-queue-flush ()
  "Flush disconnected operations that consern plugged folders."
  ;; obsolete
  (unless (or (null elmo-dop-queue)
	      (vectorp (car elmo-dop-queue)))
    (if (y-or-n-p "\
Saved queue is old version(2.6).  Clear all pending operations? ")
	(progn
	  (setq elmo-dop-queue nil)
	  (message "All pending operations are cleared.")
	  (elmo-dop-queue-save))
      (error "Please use 2.6 or earlier")))
  (elmo-dop-queue-merge)
  (let ((queue-all elmo-dop-queue)
	queue
	(count 0)
	len)
    (while queue-all
      (if (elmo-folder-plugged-p
	   (elmo-get-folder (elmo-dop-queue-fname (car queue-all))))
	  (setq queue (append queue (list (car queue-all)))))
      (setq queue-all (cdr queue-all)))
    (setq count (length queue))
    (when (> count 0)
      (if (elmo-y-or-n-p
	   (format "%d pending operation(s) exists.  Perform now? " count)
	   (not elmo-dop-flush-confirm) t)
	  (progn
	    (message "")
	    (sit-for 0)
	    (let ((queue elmo-dop-queue)
		  (performed 0)
		  (i 0)
		  (num (length elmo-dop-queue))
		  folder func failure)
	      (while queue
		;; now perform pending processes.
		(setq failure nil)
		(setq i (+ 1 i))
		(message "Flushing queue....%d/%d." i num)
		(condition-case err
		    (progn
		      (apply (elmo-dop-queue-method (car queue))
			     (prog1
				 (setq folder
				       (elmo-get-folder
					(elmo-dop-queue-fname (car queue))))
			       (elmo-folder-open folder)
			       (unless (elmo-folder-plugged-p folder)
				 (error "Unplugged")))
			     (elmo-dop-queue-arguments (car queue)))
		      (elmo-folder-close folder))
		  (quit  (setq failure t))
		  (error (setq failure err)))
		(if failure
		    ();
		  (setq elmo-dop-queue (delq (car queue) elmo-dop-queue))
		  (setq performed (+ 1 performed)))
		(setq queue (cdr queue)))
	      (message "%d/%d operation(s) are performed successfully."
		       performed num)
	      (sit-for 0) ;
	      (elmo-dop-queue-save)))
	;; when answer=NO against performing dop
	(if (elmo-y-or-n-p "Clear these pending operations? "
			   (not elmo-dop-flush-confirm) t)
	    (progn
	      (while queue
		(when (eq (elmo-dop-queue-method (car queue))
			  'elmo-folder-append-buffer-dop-delayed)
		  (elmo-folder-delete-messages
		   (elmo-dop-spool-folder
		    (elmo-get-folder (elmo-dop-queue-fname (car queue))))
		   (list (nth 1 (elmo-dop-queue-arguments (car queue))))))
		(setq elmo-dop-queue (delq (car queue) elmo-dop-queue))
		(setq queue (cdr queue)))
	      (message "Pending operations are cleared.")
	      (elmo-dop-queue-save))
	  (message "")))
      count)))

(defun elmo-dop-queue-merge ()
  (let ((queue elmo-dop-queue)
	new-queue match-queue que)
    (while (setq que (car queue))
      (if (and
	   (memq (elmo-dop-queue-method que)
		 elmo-dop-queue-merge-method-list)
	   (setq match-queue
		 (car (delete
		       nil
		       (mapcar
			(lambda (nqueue)
			  (if (and
			       (string= (elmo-dop-queue-fname que)
					(elmo-dop-queue-fname nqueue))
			       (string= (elmo-dop-queue-method que)
					(elmo-dop-queue-method nqueue)))
			      nqueue))
			new-queue)))))
	  (setcar (elmo-dop-queue-arguments match-queue)
		  (append (car (elmo-dop-queue-arguments match-queue))
			  (car (elmo-dop-queue-arguments que))))
	(setq new-queue (nconc new-queue (list que))))
      (setq queue (cdr queue)) )
    (setq elmo-dop-queue new-queue)))

;;; dop spool folder
(defun elmo-dop-spool-folder (folder)
  "Return a spool folder for disconnected operations
which is corresponded to the FOLDER."
  (elmo-make-folder
   (concat "+" (expand-file-name "spool" (elmo-folder-msgdb-path folder)))
   nil
   (elmo-folder-mime-charset-internal folder)))

(defun elmo-dop-spool-folder-append-buffer (folder flags)
  "Append current buffer content to the dop spool folder.
FOLDER is the folder structure.
Return a message number."
  (let ((spool (elmo-dop-spool-folder folder)))
    (unless (elmo-folder-exists-p spool)
      (elmo-folder-create spool))
    (let ((new-number (1+ (car (elmo-folder-status spool)))))
      (elmo-folder-append-buffer spool flags new-number)
      new-number)))


(defun elmo-dop-spool-folder-list-messages (folder)
  "List messages in the dop spool folder.
FOLDER is the folder structure."
  (setq folder (elmo-dop-spool-folder folder))
  (if (elmo-folder-exists-p folder)
      (elmo-folder-list-messages folder)))

(defun elmo-dop-list-deleting-messages (folder)
  "List messages which are on the deleting queue for the folder.
FOLDER is the folder structure."
  (let (messages)
    (dolist (queue elmo-dop-queue)
      (if (and (string= (elmo-dop-queue-fname queue)
			(elmo-folder-name-internal folder))
	       (eq (elmo-dop-queue-method queue)
		   'elmo-folder-delete-messages-dop-delayed))
	  (setq messages (nconc messages
				(mapcar
				 'car
				 (car (elmo-dop-queue-arguments queue)))))))))

(defun elmo-dop-filter-pending-messages (numbers)
  (elmo-delete-if (lambda (number) (< number 0)) numbers))

;;; DOP operations.
(defsubst elmo-folder-append-buffer-dop (folder &optional flag number)
  (elmo-dop-queue-append
   folder 'elmo-folder-append-buffer-dop-delayed
   (list flag
	 (elmo-dop-spool-folder-append-buffer folder flag)
	 number)))

(defsubst elmo-folder-delete-messages-dop (folder numbers)
  (let ((spool-folder (elmo-dop-spool-folder folder))
	queue)
    (dolist (number numbers)
      (if (< number 0)
	  (progn
	    ;; delete from queue
	    (elmo-folder-delete-messages spool-folder
					 (list (abs number)))
	    (dolist (queue elmo-dop-queue)
	      (when (and (eq (elmo-dop-queue-fname queue)
			     (elmo-folder-name-internal folder))
			 (eq (elmo-dop-queue-method queue)
			     'elmo-folder-append-buffer-dop-delayed)
			 (eq (abs number)
			     (nth 1 (elmo-dop-queue-arguments queue))))
		(setq elmo-dop-queue (delq queue elmo-dop-queue)))))
	(setq queue (cons number queue))))
    (when queue
      (elmo-dop-queue-append folder 'elmo-folder-delete-messages-dop-delayed
			     (list
			      (mapcar
			       (lambda (number)
				 (cons number (elmo-message-field
					       folder number 'message-id)))
			       queue))))
    t))

(defsubst elmo-message-encache-dop (folder number &optional read)
  (elmo-dop-queue-append folder 'elmo-message-encache (list number read)))

(defsubst elmo-folder-create-dop (folder)
  (elmo-dop-queue-append folder 'elmo-folder-create-dop-delayed nil))

(defsubst elmo-folder-set-flag-dop (folder numbers flag)
  (when (setq numbers (elmo-dop-filter-pending-messages numbers))
    (let ((method (case flag
		    (unread
		     'elmo-folder-unset-read-delayed)
		    (read
		     'elmo-folder-set-read-delayed)
		    (important
		     'elmo-folder-set-important-delayed)
		    (answered
		     'elmo-folder-set-answered-delayed))))
      (if method
	  (elmo-dop-queue-append folder method (list numbers))
	(elmo-dop-queue-append folder 'elmo-folder-set-flag
			       (list numbers flag))))))

(defsubst elmo-folder-unset-flag-dop (folder numbers flag)
  (when (setq numbers (elmo-dop-filter-pending-messages numbers))
    (let ((method (case flag
		    (unread
		     'elmo-folder-set-read-delayed)
		    (read
		     'elmo-folder-unset-read-delayed)
		    (important
		     'elmo-folder-unset-important-delayed)
		    (answered
		     'elmo-folder-unset-answered-delayed))))
      (if method
	  (elmo-dop-queue-append folder method (list numbers))
	(elmo-dop-queue-append folder 'elmo-folder-unset-flag
			       (list numbers flag))))))

;;; Execute as subsutitute for plugged operation.
(defun elmo-folder-status-dop (folder)
  (let ((number-list (elmo-folder-list-messages folder nil 'in-msgdb))
	(spool-folder (elmo-dop-spool-folder folder))
	spool-length
	max-num)
    (setq spool-length (or (if (elmo-folder-exists-p spool-folder)
			       (car (elmo-folder-status spool-folder)))
			   0))
    (setq max-num (if number-list (apply #'max number-list) 0))
    (cons (+ max-num spool-length) (+ (length number-list) spool-length))))

(defun elmo-folder-next-message-number-dop (folder)
  (let ((spool-folder (elmo-dop-spool-folder folder)))
    (- (1+ (elmo-max-of-list (elmo-folder-list-messages spool-folder))))))

;;; Delayed operation (executed at online status).
(defun elmo-folder-append-buffer-dop-delayed (folder flag number set-number)
  (let ((spool-folder (elmo-dop-spool-folder folder))
	flags)
    (with-temp-buffer
      (when (elmo-message-fetch spool-folder number
				(elmo-make-fetch-strategy 'entire)
				'unread)
	(setq flags (or (elmo-message-flags-for-append folder (* -1 number))
			(cond ((listp flag)
			       flag)
			      ;; for compatibility with 2.11.12 or earlier
			      ((eq flag t)
			       nil)
			      (t
			       (list flag)))))
	(when (or (condition-case nil
		      (let ((new-num (elmo-folder-next-message-number folder)))
			(prog1
			    (elmo-folder-append-buffer folder flags set-number)
			  (elmo-emit-signal 'message-number-changed
					    folder (- number) new-num)))
		    (error))
		  ;; Append failed...
		  (elmo-folder-append-buffer
		   (elmo-get-folder elmo-lost+found-folder)
		   flags))
	  (elmo-folder-delete-messages spool-folder (list number)))))
    ;; ignore failure (already dequed)
    t))

(defun elmo-folder-delete-messages-dop-delayed (folder number-alist)
  (ignore-errors
    (elmo-folder-delete-messages
     folder
     ;; messages are deleted only if message-id is not changed.
     (mapcar 'car
	     (elmo-delete-if
	      (lambda (pair)
		(not (string=
		      (cdr pair)
		      (elmo-message-fetch-field folder (car pair)
						'message-id))))
	      number-alist)))
    t)) ; Always success (If failure, just remain)

(defun elmo-folder-create-dop-delayed (folder)
  (unless (elmo-folder-exists-p folder)
    (elmo-folder-create folder)))

(defun elmo-folder-set-important-delayed (folder numbers)
  (elmo-folder-set-flag folder numbers 'important))

(defun elmo-folder-unset-important-delayed (folder numbers)
  (elmo-folder-unset-flag folder numbers 'important))

(defun elmo-folder-set-read-delayed (folder numbers)
  (elmo-folder-unset-flag folder numbers 'unread))

(defun elmo-folder-unset-read-delayed (folder numbers)
  (elmo-folder-set-flag folder numbers 'unread))

(defun elmo-folder-set-answered-delayed (folder numbers)
  (elmo-folder-set-flag folder numbers 'answered))

(defun elmo-folder-unset-answered-delayed (folder numbers)
  (elmo-folder-unset-flag folder numbers 'answered))

;;; Util
(defun elmo-dop-msgdb (msgdb)
  (let ((new-db (elmo-make-msgdb))
	entity)
    (dolist (dop-entity (mapcar
			 (lambda (number)
			   (setq entity (elmo-msgdb-message-entity
					 msgdb number))
			   (elmo-message-entity-set-number
			    entity
			    (* -1 (elmo-message-entity-number entity)))
			   entity)
			 (elmo-msgdb-list-messages msgdb)))
      (elmo-msgdb-append-entity new-db
				dop-entity
				(elmo-msgdb-flags
				 msgdb
				 (abs (elmo-message-entity-number
				       dop-entity)))))
    new-db))

(require 'product)
(product-provide (provide 'elmo-dop) (require 'elmo-version))

;;; elmo-dop.el ends here
