;;; elmo-filter.el --- Filtered Folder Interface for ELMO.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

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
(require 'elmo)
(require 'elmo-signal)
(require 'elmo-msgdb)

(defvar elmo-filter-number-filename "number-list"
  "File name for message number database.")

;;; ELMO filter folder
(eval-and-compile
  (luna-define-class elmo-filter-folder (elmo-folder)
		     (condition target require-msgdb number-list flag-count))
  (luna-define-internal-accessors 'elmo-filter-folder))

(luna-define-method elmo-folder-initialize ((folder elmo-filter-folder)
					    name)
  (let (pair)
    (setq pair (elmo-parse-search-condition name))
    (elmo-filter-folder-set-condition-internal folder (car pair))
    (if (string-match "^ */\\(.*\\)$" (cdr pair))
	(elmo-filter-folder-set-target-internal
	 folder
	 (elmo-get-folder (match-string 1 (cdr pair))))
      (error "Folder syntax error `%s'" (elmo-folder-name-internal folder)))
    (elmo-filter-folder-set-require-msgdb-internal
     folder
     (elmo-folder-search-requires-msgdb-p
      (elmo-filter-folder-target-internal folder)
      (elmo-filter-folder-condition-internal folder)))
    (elmo-filter-folder-set-number-list-internal folder 'not-loaded)
    (elmo-filter-connect-signals
     folder
     (elmo-filter-folder-target-internal folder))
    folder))

(defun elmo-filter-connect-signals (folder target)
  (elmo-connect-signal
   target 'flag-changing folder
   (elmo-define-signal-handler (folder target number old-flags new-flags)
     (elmo-filter-add-flag-count folder old-flags -1)
     (elmo-filter-add-flag-count folder new-flags)
     (elmo-emit-signal 'flag-changing folder number old-flags new-flags))
   (elmo-define-signal-filter (folder target number)
     (memq number (elmo-folder-list-messages folder nil t))))
  (elmo-connect-signal
   target 'flag-changed folder
   (elmo-define-signal-handler (folder target numbers)
     (let ((filterd (elmo-list-filter
		     (elmo-folder-list-messages folder nil t)
		     numbers)))
       (when filterd
	 (elmo-emit-signal 'flag-changed folder filterd)))))
  (elmo-connect-signal
   target 'status-changed folder
   (elmo-define-signal-handler (folder target numbers)
     (let ((filterd (elmo-list-filter
		     (elmo-folder-list-messages folder nil t)
		     numbers)))
       (when filterd
	 (elmo-emit-signal 'status-changed folder filterd)))))
  (elmo-connect-signal
   target 'update-overview folder
   (elmo-define-signal-handler (folder target number)
     (elmo-emit-signal 'update-overview folder number))
   (elmo-define-signal-filter (folder target number)
     (memq number (elmo-folder-list-messages folder nil t)))))

(defun elmo-filter-number-list-load (dir)
  (elmo-object-load
   (expand-file-name elmo-filter-number-filename dir)))

(defun elmo-filter-number-list-save (dir number-list)
  (elmo-object-save
   (expand-file-name elmo-filter-number-filename dir)
   number-list))

(defun elmo-filter-folder-number-list-loaded-p (folder)
  (listp (elmo-filter-folder-number-list-internal folder)))

(defun elmo-filter-folder-number-list (folder)
  (let ((numbers (elmo-filter-folder-number-list-internal folder)))
    (if (listp numbers)
	numbers
      (elmo-filter-folder-set-number-list-internal
       folder
       (elmo-filter-number-list-load (elmo-folder-msgdb-path folder))))))

(defsubst elmo-filter-folder-countup-message-flags (folder numbers
							   &optional delta)
  (let ((flag-count (elmo-filter-folder-flag-count-internal folder))
	(delta (or delta 1))
	elem)
    (dolist (number numbers)
      (dolist (flag (elmo-message-flags folder number))
	(if (setq elem (assq flag flag-count))
	    (setcdr elem (+ (cdr elem) delta))
	  (setq flag-count (cons (cons flag delta) flag-count)))))
    (elmo-filter-folder-set-flag-count-internal folder flag-count)))

(defun elmo-filter-add-flag-count (folder flags &optional delta)
  (let ((flag-count (elmo-filter-folder-flag-count-internal folder))
	(delta (or delta 1))
	elem)
    (dolist (flag flags)
      (if (setq elem (assq flag flag-count))
	  (setcdr elem (+ (cdr elem) delta))
	(setq flag-count (cons (cons flag delta) flag-count))))
    (elmo-filter-folder-set-flag-count-internal folder flag-count)))

(defun elmo-filter-folder-flag-count (folder)
  (or (elmo-filter-folder-flag-count-internal folder)
      (elmo-filter-folder-countup-message-flags
       folder
       (elmo-folder-list-messages folder t t))))

(defun elmo-filter-folder-copy-flag-count (flag-counts)
  (mapcar (lambda (pair) (cons (car pair) (cdr pair))) flag-counts))

(luna-define-method elmo-folder-open :after ((folder elmo-filter-folder)
					     &optional load-msgdb)
  (when load-msgdb
    (elmo-filter-folder-number-list folder)
    (elmo-filter-folder-flag-count folder)
    (elmo-folder-msgdb (elmo-filter-folder-target-internal folder))))

(luna-define-method elmo-folder-open-internal ((folder elmo-filter-folder))
  (elmo-folder-open-internal (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-check ((folder elmo-filter-folder))
  (when (elmo-filter-folder-require-msgdb-internal folder)
    (elmo-folder-synchronize (elmo-filter-folder-target-internal folder))))

(luna-define-method elmo-folder-close-internal ((folder elmo-filter-folder))
  (elmo-folder-close-internal (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-close ((folder elmo-filter-folder))
  (elmo-generic-folder-close folder)
  (elmo-filter-folder-set-number-list-internal folder 'not-loaded)
  (elmo-filter-folder-set-flag-count-internal folder nil)
  (elmo-folder-close (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-commit ((folder elmo-filter-folder))
  (elmo-folder-commit (elmo-filter-folder-target-internal folder))
  (when (elmo-folder-persistent-p folder)
    (elmo-folder-set-info-max-by-numdb
     folder
     (elmo-folder-list-messages folder nil 'in-msgdb))
    (elmo-msgdb-killed-list-save
     (elmo-folder-msgdb-path folder)
     (elmo-folder-killed-list-internal folder))
    (when (elmo-filter-folder-number-list-loaded-p folder)
      (elmo-filter-number-list-save
       (elmo-folder-msgdb-path folder)
       (elmo-filter-folder-number-list folder)))))

(luna-define-method elmo-folder-expand-msgdb-path ((folder
						    elmo-filter-folder))
  (expand-file-name
   (elmo-replace-string-as-filename (elmo-folder-name-internal folder))
   (expand-file-name "filter" elmo-msgdb-directory)))

(luna-define-method elmo-folder-search-requires-msgdb-p ((folder
							  elmo-filter-folder)
							 condition)
  (elmo-folder-search-requires-msgdb-p
   (elmo-filter-folder-target-internal folder) condition))

(luna-define-method elmo-folder-newsgroups ((folder elmo-filter-folder))
  (elmo-folder-newsgroups (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-find-fetch-strategy ((folder elmo-filter-folder)
					      number
					      &optional
					      ignore-cache
					      require-entireness)
  (elmo-find-fetch-strategy
   (elmo-filter-folder-target-internal folder)
   number ignore-cache require-entireness))

(luna-define-method elmo-folder-get-primitive-list ((folder
						     elmo-filter-folder))
  (elmo-folder-get-primitive-list (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-contains-type ((folder elmo-filter-folder)
					       type)
  (elmo-folder-contains-type
   (elmo-filter-folder-target-internal folder)
   type))

(luna-define-method elmo-folder-append-buffer ((folder elmo-filter-folder)
					       &optional flag number
					       return-number)
  (elmo-folder-append-buffer
   (elmo-filter-folder-target-internal folder)
   flag number return-number))

(defun elmo-folder-append-messages-filter-* (dst-folder
					     src-folder
					     numbers
					     same-number)
  (elmo-folder-append-messages dst-folder
			       (elmo-filter-folder-target-internal src-folder)
			       numbers
			       same-number))

(defun elmo-folder-append-messages-*-filter (dst-folder
					     src-folder
					     numbers
					     same-number)
  (elmo-folder-append-messages (elmo-filter-folder-target-internal dst-folder)
			       src-folder
			       numbers
			       same-number))

(luna-define-method elmo-message-fetch-bodystructure ((folder
						       elmo-filter-folder)
						      number strategy)
  (elmo-message-fetch-bodystructure
   (elmo-filter-folder-target-internal folder)
   number strategy))

(luna-define-method elmo-message-fetch ((folder elmo-filter-folder)
					number strategy
					&optional unseen section)
  (elmo-message-fetch (elmo-filter-folder-target-internal folder)
		      number strategy unseen section))

(luna-define-method elmo-folder-delete-messages ((folder elmo-filter-folder)
						 numbers)
  (let ((flag-count (elmo-filter-folder-copy-flag-count
		     (elmo-filter-folder-flag-count-internal folder)))
	(messages (copy-sequence
		   (elmo-filter-folder-number-list folder)))
	success)
    (elmo-folder-detach-messages folder numbers)
    (unless (setq success
		  (elmo-folder-delete-messages
		   (elmo-filter-folder-target-internal folder) numbers))
      (elmo-filter-folder-set-flag-count-internal folder flag-count)
      (elmo-filter-folder-set-number-list-internal folder messages))
    success))

(luna-define-method elmo-folder-list-messages ((folder elmo-filter-folder)
					       &optional visible-only in-msgdb)
  (let ((list (if in-msgdb
		  t
		(elmo-folder-list-messages-internal folder visible-only)))
	(killed-list (elmo-folder-killed-list-internal folder)))
    (unless (listp list)
      ;; Use current list.
      (setq list (elmo-filter-folder-number-list folder)))
    (if visible-only
	(elmo-living-messages list killed-list)
      (if (and in-msgdb killed-list list)
	  (elmo-sort-uniq-number-list
	   (nconc (elmo-number-set-to-number-list killed-list) list))
	list))))

(luna-define-method elmo-folder-list-messages-internal
  ((folder elmo-filter-folder) &optional nohide)
  (let ((target (elmo-filter-folder-target-internal folder)))
    (if (or (elmo-folder-plugged-p target)
	    (not (elmo-folder-persistent-p folder)))
	;; search target folder
	(elmo-folder-search
	 target
	 (elmo-filter-folder-condition-internal folder))
      ;; not available
      t)))

(luna-define-method elmo-folder-list-flagged ((folder elmo-filter-folder)
					      flag
					      &optional in-msgdb)
  (elmo-list-filter
   (elmo-folder-list-messages folder nil t)
   (elmo-folder-list-flagged
    (elmo-filter-folder-target-internal folder) flag in-msgdb)))

(luna-define-method elmo-folder-list-subfolders ((folder elmo-filter-folder)
						 &optional one-level)
  (let* ((target (elmo-filter-folder-target-internal folder))
	 (prefix (and (string-match
		       (concat "^\\(.*\\)"
			       (regexp-quote
				(elmo-folder-name-internal
				 target))
			       "$")
		       (elmo-folder-name-internal folder))
		      (match-string 1 (elmo-folder-name-internal
				       folder)))))
    (elmo-mapcar-list-of-list
     (lambda (x) (concat prefix x))
     (elmo-folder-list-subfolders target one-level))))

(luna-define-method elmo-folder-diff :around ((folder elmo-filter-folder))
  (let ((condition (elmo-filter-folder-condition-internal folder))
	diff)
    (if (vectorp condition)
	(cond
	 ((and (string= (elmo-filter-key condition) "flag")
	       (or (string= (elmo-filter-value condition) "any")
		   (string= (elmo-filter-value condition) "digest")
		   (string= (elmo-filter-value condition) "unread")))
	  (setq diff (elmo-folder-diff (elmo-filter-folder-target-internal
					folder)))
	  (if (consp (cdr diff))
	      ;; new unread unread
	      (list (car diff) (nth 1 diff) (nth 1 diff))
	    (cons (car diff) (car diff))))
	 ((string= "last" (elmo-filter-key condition))
	  (luna-call-next-method))
	 (t
	  (cons nil (cdr (elmo-folder-diff (elmo-filter-folder-target-internal
					    folder))))))
      (luna-call-next-method))))

(luna-define-method elmo-folder-status ((folder elmo-filter-folder))
  (elmo-folder-status
   (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-exists-p ((folder elmo-filter-folder))
  (elmo-folder-exists-p (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-creatable-p ((folder elmo-filter-folder))
  (elmo-folder-creatable-p (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-writable-p ((folder elmo-filter-folder))
  (elmo-folder-writable-p (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-create ((folder elmo-filter-folder))
  (elmo-folder-create (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-search ((folder elmo-filter-folder)
					condition &optional numbers)
  ;; search from messages in this folder
  (elmo-folder-search
   (elmo-filter-folder-target-internal folder)
   condition
   (cond
    ((null numbers)
     (elmo-folder-list-messages folder))
    ((listp numbers)
     numbers)
    (t
     (elmo-folder-list-messages folder 'visible 'in-msgdb)))))

(luna-define-method elmo-message-use-cache-p ((folder elmo-filter-folder)
					      number)
  (elmo-message-use-cache-p (elmo-filter-folder-target-internal folder)
			    number))

(luna-define-method elmo-folder-message-file-p ((folder elmo-filter-folder))
  (elmo-folder-message-file-p (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-local-p ((folder elmo-filter-folder))
  (elmo-folder-local-p (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-plugged-p ((folder elmo-filter-folder))
  (elmo-folder-plugged-p (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-set-plugged ((folder elmo-filter-folder)
					     plugged &optional add)
  (elmo-folder-set-plugged (elmo-filter-folder-target-internal folder)
			   plugged add))

(luna-define-method elmo-message-file-name ((folder elmo-filter-folder)
					    number)
  (elmo-message-file-name (elmo-filter-folder-target-internal folder)
			  number))

(luna-define-method elmo-message-flag-available-p ((folder
						    elmo-filter-folder) number
						    flag)
  (elmo-message-flag-available-p
   (elmo-filter-folder-target-internal folder)
   number flag))

(luna-define-method elmo-message-flags ((folder elmo-filter-folder) number)
  (elmo-message-flags (elmo-filter-folder-target-internal folder)
		      number))

(luna-define-method elmo-message-set-cached ((folder elmo-filter-folder)
					     number cached)
  (elmo-message-set-cached
   (elmo-filter-folder-target-internal folder) number cached))

(luna-define-method elmo-message-number ((folder elmo-filter-folder)
					 message-id)
  (elmo-message-number (elmo-filter-folder-target-internal folder)
		       message-id))

(luna-define-method elmo-message-entity ((folder elmo-filter-folder) key)
  (elmo-message-entity (elmo-filter-folder-target-internal folder) key))

(luna-define-method elmo-message-entity-parent ((folder elmo-filter-folder)
						entity)
  (let ((parent (elmo-message-entity-parent
		 (elmo-filter-folder-target-internal folder)
		 entity)))
    (when (memq (elmo-message-entity-number parent)
		(elmo-filter-folder-number-list folder))
      parent)))

(luna-define-method elmo-folder-flag-table ((folder elmo-filter-folder)
					    &optional if-exists)
  (elmo-folder-flag-table (elmo-filter-folder-target-internal folder)
			  if-exists))

(luna-define-method elmo-folder-close-flag-table ((folder elmo-filter-folder))
  (elmo-folder-close-flag-table (elmo-filter-folder-target-internal folder)))

(luna-define-method elmo-folder-count-flags ((folder elmo-filter-folder))
  (elmo-filter-folder-flag-count folder))

(luna-define-method elmo-folder-set-flag ((folder elmo-filter-folder)
					  numbers
					  flag
					  &optional is-local)
  (elmo-folder-set-flag (elmo-filter-folder-target-internal folder)
			numbers flag is-local))

(luna-define-method elmo-folder-unset-flag ((folder elmo-filter-folder)
					    numbers
					    flag
					    &optional is-local)
  (elmo-folder-unset-flag (elmo-filter-folder-target-internal folder)
			  numbers flag is-local))

(luna-define-method elmo-message-folder ((folder elmo-filter-folder)
					 number)
  (elmo-message-folder (elmo-filter-folder-target-internal folder) number))

(luna-define-method elmo-message-field ((folder elmo-filter-folder)
					number field &optional type)
  (elmo-message-field
   (elmo-filter-folder-target-internal folder) number field type))

(luna-define-method elmo-message-set-field ((folder elmo-filter-folder)
					    number field value)
  (elmo-message-set-field
   (elmo-filter-folder-target-internal folder) number field value))

(luna-define-method elmo-folder-clear ((folder elmo-filter-folder)
				       &optional keep-killed)
  (unless keep-killed
    (elmo-folder-set-killed-list-internal folder nil))
  (elmo-filter-folder-set-number-list-internal folder nil)
  (elmo-filter-folder-set-flag-count-internal folder nil))

(luna-define-method elmo-folder-synchronize ((folder elmo-filter-folder)
					     &optional
					     disable-killed
					     ignore-msgdb
					     no-check
					     mask)
  (let ((killed-list (elmo-folder-killed-list-internal folder))
	numbers)
    (unless no-check
      (when (elmo-filter-folder-require-msgdb-internal folder)
	(elmo-folder-synchronize (elmo-filter-folder-target-internal folder)
				 disable-killed
				 ignore-msgdb
				 no-check
				 mask)))
    (setq numbers (elmo-folder-list-messages folder (not disable-killed)))
    (when (and numbers
	       (not (elmo-filter-folder-require-msgdb-internal folder)))
      (elmo-folder-synchronize (elmo-filter-folder-target-internal folder)
			       'disable-killed
			       ignore-msgdb
			       no-check
			       (if mask
				   (elmo-list-filter mask numbers)
				 numbers)))
    (when (and disable-killed ignore-msgdb)
      (elmo-folder-set-killed-list-internal folder nil))
    (elmo-filter-folder-set-number-list-internal folder numbers)
    (elmo-filter-folder-set-flag-count-internal folder nil)
    0))

(luna-define-method elmo-folder-detach-messages ((folder elmo-filter-folder)
						 numbers)
  (elmo-filter-folder-countup-message-flags folder numbers -1)
  (elmo-filter-folder-set-number-list-internal
   folder
   (elmo-list-delete numbers (elmo-filter-folder-number-list folder) #'delq))
  t)

(luna-define-method elmo-folder-length ((folder elmo-filter-folder))
  (and (elmo-filter-folder-number-list-loaded-p folder)
       (length (elmo-filter-folder-number-list-internal folder))))

(require 'product)
(product-provide (provide 'elmo-filter) (require 'elmo-version))

;;; elmo-filter.el ends here
