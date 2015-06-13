;;; modb-standard.el --- Standartd Implement of MODB.

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

(defcustom modb-standard-divide-number 500
  "*Standard modb divide entity number."
  :type '(choice (const :tag "Not divide" nil)
		 number)
  :group 'elmo)

(defvar modb-standard-entity-filename "entity"
  "Message entity database.")

(defvar modb-standard-flag-filename "flag"
  "Message number <=> Flag status database.")

(defvar modb-standard-msgid-filename "msgid"
  "Message number <=> Message-Id database.")

(eval-and-compile
  (luna-define-class modb-standard (modb-generic)
		     (number-list	; sorted list of message numbers.
		      entity-map	; number, msg-id -> entity mapping.
		      flag-map		; number -> flag-list mapping
		      flag-count	; list of (FLAG . COUNT)
		      overview-handler  ; instance of modb-entity-handler.
		      ))
  (luna-define-internal-accessors 'modb-standard))

;; for internal use only
(defsubst modb-standard-key (number)
  (concat "#" (number-to-string number)))

(defsubst modb-standard-entity-id (entity)
  (if (eq 'autoload (car-safe entity))
      (cddr entity)
    (elmo-msgdb-message-entity-field
     (elmo-message-entity-handler entity)
     entity 'message-id)))

(defsubst modb-standard-entity-map (modb)
  (or (modb-standard-entity-map-internal modb)
      (modb-standard-set-entity-map-internal
       modb
       (elmo-make-hash (elmo-msgdb-length modb)))))

(defsubst modb-standard-flag-map (modb)
  (or (modb-standard-flag-map-internal modb)
      (modb-standard-set-flag-map-internal
       modb
       (elmo-make-hash (elmo-msgdb-length modb)))))

(defsubst modb-standard-set-message-modified (modb number)
  (if modb-standard-divide-number
      (let ((section (/ number modb-standard-divide-number))
	    (modified (modb-generic-message-modified-internal modb)))
	(unless (memq section modified)
	  (modb-generic-set-message-modified-internal
	   modb (cons section modified))))
    (modb-generic-set-message-modified-internal modb t)))

(defsubst modb-standard-set-flag-modified (modb number)
  (modb-generic-set-flag-modified-internal modb t))

(defsubst modb-standard-message-flags (modb number)
  (cdr (elmo-get-hash-val (modb-standard-key number)
			  (modb-standard-flag-map-internal modb))))

(defsubst modb-standard-match-flags (check-flags flags)
  (catch 'done
    (while check-flags
      (when (memq (car check-flags) flags)
	(throw 'done t))
      (setq check-flags (cdr check-flags)))))

(defsubst modb-standard-countup-flags (modb flags &optional delta)
  (let ((flag-count (modb-standard-flag-count-internal modb))
	(delta (or delta 1))
	elem)
    (dolist (flag flags)
      (if (setq elem (assq flag flag-count))
	  (setcdr elem (+ (cdr elem) delta))
	(setq flag-count (cons (cons flag delta) flag-count))))
    (modb-standard-set-flag-count-internal modb flag-count)))

;; save and load functions
(defun modb-standard-load-msgid (modb path)
  (let* ((alist (elmo-object-load
		 (expand-file-name modb-standard-msgid-filename path)))
	 (table (or (modb-standard-entity-map-internal modb)
		    (elmo-make-hash (length alist))))
	 numbers info)
    (dolist (pair alist)
      (setq info (cons 'autoload pair))
      (elmo-set-hash-val (modb-standard-key (car pair)) info table)
      (elmo-set-hash-val (cdr pair) info table)
      (setq numbers (cons (car pair) numbers)))
    (modb-standard-set-number-list-internal modb (nreverse numbers))
    (modb-standard-set-entity-map-internal modb table)))

(defun modb-standard-save-msgid (modb path)
  (let ((table (modb-standard-entity-map-internal modb))
	entity alist)
    (dolist (number (modb-standard-number-list-internal modb))
      (setq entity (elmo-get-hash-val (modb-standard-key number) table))
      (setq alist (cons (cons number (modb-standard-entity-id entity))
			alist)))
    (elmo-object-save
     (expand-file-name modb-standard-msgid-filename path)
     (nreverse alist))))

(defun modb-standard-load-flag (modb path)
  (let ((table (or (modb-standard-flag-map-internal modb)
		   (elmo-make-hash (elmo-msgdb-length modb)))))
    (dolist (info (elmo-object-load
		   (expand-file-name modb-standard-flag-filename path)))
      (modb-standard-countup-flags modb (cdr info))
      (elmo-set-hash-val (modb-standard-key (car info)) info table))
    (modb-standard-set-flag-map-internal modb table)))

(defun modb-standard-save-flag (modb path)
  (let (table flist info)
    (when (setq table (modb-standard-flag-map-internal modb))
      (mapatoms
       (lambda (atom)
	 (setq info (symbol-value atom))
	 (when (cdr info)
	   (setq flist (cons info flist))))
       table))
    (elmo-object-save
     (expand-file-name modb-standard-flag-filename path)
     flist)))

(defsubst modb-standard-entity-filename (section)
  (if section
      (concat modb-standard-entity-filename
	      "-"
	      (number-to-string section))
    modb-standard-entity-filename))

(defsubst modb-standard-loaded-message-id (msgdb number)
  "Get message-id for autoloaded entity."
  (let ((ret (elmo-get-hash-val
	      (modb-standard-key number)
	      (modb-standard-entity-map-internal msgdb))))
    (cond
     ((null ret)
      ;; Garbage entity.
      (elmo-clear-hash-val (modb-standard-key number)
			   (modb-standard-entity-map-internal msgdb))
      nil)				; return nil.
     ((eq (car-safe ret) 'autoload)
      (cdr (cdr ret)))			; message-id.
     ((elmo-msgdb-message-entity-field (elmo-message-entity-handler ret)
				       ret 'message-id)) ; Already loaded.
     (t (error "Internal error: invalid msgdb status")))))

(defun modb-standard-load-entity (modb path &optional section)
  (let ((table (or (modb-standard-entity-map-internal modb)
		   (elmo-make-hash (elmo-msgdb-length modb))))
	(objects (elmo-object-load
		  (expand-file-name
		   (modb-standard-entity-filename section)
		   path)))
	number msgid)
    (cond ((eq (car objects) 'modb-standard-entity-handler)
	   ;; (standard PARAMETERS ENTITY*)
	   (let ((handler (apply #'luna-make-entity
				 (car objects)
				 (car (cdr objects))))
		 entity)
	     (dolist (element (cdr (cdr objects)))
	       (setq entity (cons handler (cons nil element))
		     number (elmo-msgdb-message-entity-number handler entity)
		     msgid  (modb-standard-loaded-message-id modb number))
	       (when msgid
		 (elmo-msgdb-message-entity-set-field
		  handler entity 'message-id msgid)
		 (elmo-set-hash-val (modb-standard-key number) entity table)
		 (elmo-set-hash-val msgid entity table)))))
	  (t
	   ;; legacy format
	   (dolist (entity objects)
	     (setq number (elmo-msgdb-message-entity-number
			   (elmo-message-entity-handler entity)
			   entity)
		   msgid (modb-standard-loaded-message-id modb number))
	     (when msgid
	       (setcar entity msgid)
	       (elmo-set-hash-val (modb-standard-key number) entity table)
	       (elmo-set-hash-val msgid entity table)))))
    (modb-standard-set-entity-map-internal modb table)))

(defsubst modb-standard-save-entity-1 (modb path &optional section)
  (let ((table (modb-standard-entity-map-internal modb))
	(filename (expand-file-name
		   (modb-standard-entity-filename (car section)) path))
	(handler (elmo-msgdb-message-entity-handler modb))
	entity entities)
    (dolist (number (or (cdr section)
			(modb-standard-number-list-internal modb)))
      (when (setq entity (elmo-msgdb-message-entity modb number))
	(unless (modb-entity-handler-equal-p
		 handler
		 (elmo-message-entity-handler entity))
	  (setq entity (elmo-msgdb-copy-message-entity
			(elmo-message-entity-handler entity)
			entity handler)))
	(setq entities (cons (cdr (cdr entity)) entities))))
    (if entities
	(elmo-object-save filename
			  (nconc
			   (list (luna-class-name handler)
				 (modb-entity-handler-dump-parameters handler))
			   entities))
      (ignore-errors (delete-file filename)))))

(defun modb-standard-cleanup-stale-entities (modb path)
  (message "Removing stale entities...")
  (let* ((entity-regex
	  (concat "^" modb-standard-entity-filename "-\\([0-9]+\\)"))
	 (entities (elmo-sort-uniq-number-list
		    (mapcar (lambda (x) (/ x modb-standard-divide-number))
			    (modb-standard-number-list-internal modb))))
	 (files (mapcar (lambda(x)
			  (when (string-match entity-regex x)
			    (string-to-number (match-string 1 x))))
			(directory-files path nil entity-regex))))
    (dolist (entity (car (elmo-list-diff-nonsortable files entities)))
      (ignore-errors (delete-file
		      (expand-file-name
		       (modb-standard-entity-filename entity) path))))))

(defun modb-standard-save-entity (modb path)
  (let ((modified (modb-generic-message-modified-internal modb)))
    (cond ((listp modified)
	   (let ((sections (mapcar 'list modified))
		 section)
	     (dolist (number (modb-standard-number-list-internal modb))
	       (when (setq section (assq (/ number modb-standard-divide-number)
					 sections))
		 (nconc section (list number))))
	     (dolist (section sections)
	       (modb-standard-save-entity-1 modb path section))))
	  (modified
	   (modb-standard-cleanup-stale-entities modb path)))))

;;; Implement
;;
(luna-define-method elmo-msgdb-load ((msgdb modb-standard))
  (let ((inhibit-quit t)
	(path (elmo-msgdb-location msgdb)))
    (when (file-exists-p (expand-file-name modb-standard-flag-filename path))
      (modb-standard-load-msgid msgdb path)
      (modb-standard-load-flag msgdb path)
      (unless modb-standard-divide-number
	(modb-standard-load-entity msgdb path))
      t)))

(luna-define-method elmo-msgdb-save ((msgdb modb-standard))
  (let ((path (elmo-msgdb-location msgdb))
	(inhibit-quit t))
    (when (elmo-msgdb-message-modified-p msgdb)
      (modb-standard-save-msgid  msgdb path)
      (modb-standard-save-entity msgdb path)
      (modb-generic-set-message-modified-internal msgdb nil))
    (when (elmo-msgdb-flag-modified-p msgdb)
      (modb-standard-save-flag msgdb path)
      (modb-generic-set-flag-modified-internal msgdb nil))))

(luna-define-method elmo-msgdb-append :around ((msgdb modb-standard)
					       msgdb-append)
  (when (> (elmo-msgdb-length msgdb-append) 0)
    (if (eq (luna-class-name msgdb-append) 'modb-standard)
	(let ((numbers (modb-standard-number-list-internal msgdb-append))
	      duplicates)
	  ;; number-list
	  (modb-standard-set-number-list-internal
	   msgdb
	   (nconc (modb-standard-number-list-internal msgdb)
		  numbers))
	  ;; entity-map
	  (let ((table (modb-standard-entity-map msgdb))
		entity msg-id)
	    (dolist (number numbers)
	      (setq entity (elmo-msgdb-message-entity msgdb-append number)
		    msg-id (modb-standard-entity-id entity))
	      (if (elmo-get-hash-val msg-id table)
		  (setq duplicates (cons number duplicates))
		(elmo-set-hash-val msg-id entity table))
	      (elmo-set-hash-val (modb-standard-key number)
				 entity
				 table)))
	  ;; flag-map
	  (let ((table (modb-standard-flag-map msgdb)))
	    (mapatoms
	     (lambda (atom)
	       (elmo-set-hash-val (symbol-name atom)
				  (symbol-value atom)
				  table))
	     (modb-standard-flag-map msgdb-append)))
	  ;; flag-count
	  (dolist (pair (modb-standard-flag-count-internal msgdb-append))
	    (modb-standard-countup-flags msgdb (list (car pair)) (cdr pair)))
	  ;; modification flags
	  (dolist (number (modb-standard-number-list-internal msgdb-append))
	    (modb-standard-set-message-modified msgdb number)
	    (modb-standard-set-flag-modified msgdb number))
	  duplicates)
      (luna-call-next-method))))

(luna-define-method elmo-msgdb-clear :after ((msgdb modb-standard))
  (modb-standard-set-number-list-internal msgdb nil)
  (modb-standard-set-entity-map-internal msgdb nil)
  (modb-standard-set-flag-map-internal msgdb nil)
  (modb-standard-set-flag-count-internal msgdb nil))

(luna-define-method elmo-msgdb-length ((msgdb modb-standard))
  (length (modb-standard-number-list-internal msgdb)))

(luna-define-method elmo-msgdb-flag-available-p ((msgdb modb-standard) flag)
  t)

(luna-define-method elmo-msgdb-flags ((msgdb modb-standard) number)
  (modb-standard-message-flags msgdb number))

(luna-define-method elmo-msgdb-set-flag ((msgdb modb-standard)
					 number flag)
  (case flag
    (read
     (elmo-msgdb-unset-flag msgdb number 'unread))
    (uncached
     (elmo-msgdb-unset-flag msgdb number 'cached))
    (t
     (let ((cur-flags (modb-standard-message-flags msgdb number))
	   new-flags diff)
       (unless (memq flag cur-flags)
	 (setq new-flags (cons flag cur-flags))
	 (setq diff (elmo-list-diff-nonsortable new-flags cur-flags))
	 (modb-standard-countup-flags msgdb (car diff))
	 (modb-standard-countup-flags msgdb (cadr diff) -1)
	 (elmo-set-hash-val (modb-standard-key number)
			    (cons number new-flags)
			    (modb-standard-flag-map msgdb))
	 (modb-standard-set-flag-modified msgdb number))))))

(luna-define-method elmo-msgdb-unset-flag ((msgdb modb-standard)
					   number flag)
  (case flag
    (read
     (elmo-msgdb-set-flag msgdb number 'unread))
    (uncached
     (elmo-msgdb-set-flag msgdb number 'cached))
    (all
     (modb-standard-countup-flags msgdb
				  (modb-standard-message-flags msgdb number)
				  -1)
     (elmo-clear-hash-val (modb-standard-key number)
			  (modb-standard-flag-map msgdb)))
    (t
     (let ((cur-flags (modb-standard-message-flags msgdb number))
	   (inhibit-quit t)
	   new-flags diff)
       (when (memq flag cur-flags)
	 (setq new-flags (delq flag (copy-sequence cur-flags)))
	 (setq diff (elmo-list-diff-nonsortable new-flags cur-flags))
	 (modb-standard-countup-flags msgdb (car diff))
	 (modb-standard-countup-flags msgdb (cadr diff) -1)
	 (elmo-set-hash-val (modb-standard-key number)
			    (cons number new-flags)
			    (modb-standard-flag-map msgdb))
	 (modb-standard-set-flag-modified msgdb number))
       (when (eq flag 'unread)
	 (elmo-msgdb-unset-flag msgdb number 'new))))))

(luna-define-method elmo-msgdb-flag-count ((msgdb modb-standard))
  (modb-standard-flag-count-internal msgdb))

(luna-define-method elmo-msgdb-list-messages ((msgdb modb-standard))
  (copy-sequence
   (modb-standard-number-list-internal msgdb)))

(luna-define-method elmo-msgdb-list-flagged ((msgdb modb-standard) flag)
  (let (entry matched)
    (case flag
      (read
       (dolist (number (modb-standard-number-list-internal msgdb))
	 (unless (memq 'unread (modb-standard-message-flags msgdb number))
	   (setq matched (cons number matched)))))
      (uncached
       (dolist (number (modb-standard-number-list-internal msgdb))
	 (unless (memq 'cached (modb-standard-message-flags msgdb number))
	   (setq matched (cons number matched)))))
      (any
       (mapatoms
	(lambda (atom)
	  (setq entry (symbol-value atom))
	  (unless (and (eq (length (cdr entry)) 1)
		       (eq (car (cdr entry)) 'cached))
	    ;; If there is a flag other than cached, then the message
	    ;; matches to `any'.
	    (setq matched (cons (car entry) matched))))
	(modb-standard-flag-map msgdb)))
      (digest
       (let ((flags (append elmo-digest-flags
			    (elmo-get-global-flags t t))))
	 (mapatoms
	  (lambda (atom)
	    (setq entry (symbol-value atom))
	    (when (modb-standard-match-flags flags (cdr entry))
	      (setq matched (cons (car entry) matched))))
	  (modb-standard-flag-map msgdb))))
      (t
       (mapatoms
	(lambda (atom)
	  (setq entry (symbol-value atom))
	  (when (memq flag (cdr entry))
	    (setq matched (cons (car entry) matched))))
	(modb-standard-flag-map msgdb))))
    matched))

(luna-define-method elmo-msgdb-search ((msgdb modb-standard)
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
	  (let* ((numbers (or numbers
			      (modb-standard-number-list-internal msgdb)))
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

(luna-define-method elmo-msgdb-append-entity ((msgdb modb-standard)
					      entity &optional flags)
  (when entity
    (let ((number (elmo-msgdb-message-entity-number
		   (elmo-message-entity-handler entity) entity))
	  (msg-id (elmo-msgdb-message-entity-field
		   (elmo-message-entity-handler entity) entity 'message-id))
	  duplicate)
      (when (and number msg-id)
	;; number-list
	(modb-standard-set-number-list-internal
	 msgdb
	 (nconc (modb-standard-number-list-internal msgdb)
		(list number)))
	;; entity-map
	(let ((table (modb-standard-entity-map msgdb)))
	  (setq duplicate (elmo-get-hash-val msg-id table))
	  (elmo-set-hash-val (modb-standard-key number) entity table)
	  (elmo-set-hash-val msg-id entity table))
	;; modification flags
	(modb-standard-set-message-modified msgdb number)
	;; flag-map
	(when flags
	  (elmo-set-hash-val
	   (modb-standard-key number)
	   (cons number flags)
	   (modb-standard-flag-map msgdb))
	  (modb-standard-countup-flags msgdb flags)
	  (modb-standard-set-flag-modified msgdb number))
	duplicate))))

(luna-define-method elmo-msgdb-update-entity ((msgdb modb-standard)
					      entity values)
  (let ((handler (elmo-message-entity-handler entity)))
    (when (elmo-msgdb-message-entity-update-fields handler entity values)
      (modb-standard-set-message-modified
       msgdb
       (elmo-msgdb-message-entity-number handler entity))
      t)))

(luna-define-method elmo-msgdb-delete-messages ((msgdb modb-standard)
						numbers)
  (let ((number-list (modb-standard-number-list-internal msgdb))
	(entity-map (modb-standard-entity-map-internal msgdb))
	(flag-map (modb-standard-flag-map-internal msgdb))
	key entity)
    (dolist (number numbers)
      (setq key (modb-standard-key number)
	    entity (elmo-get-hash-val key entity-map))
      (when entity
	;; number-list
	(setq number-list (delq number number-list))
	;; entity-map
	(elmo-clear-hash-val key entity-map)
	(elmo-clear-hash-val (modb-standard-entity-id entity) entity-map)
	;; flag-count (must be BEFORE flag-map)
	(modb-standard-countup-flags
	 msgdb
	 (modb-standard-message-flags msgdb number)
	 -1)
	;; flag-map
	(elmo-clear-hash-val key flag-map)
	(modb-standard-set-message-modified msgdb number)
	(modb-standard-set-flag-modified msgdb number)))
    (modb-standard-set-number-list-internal msgdb number-list)
    (modb-standard-set-entity-map-internal msgdb entity-map)
    (modb-standard-set-flag-map-internal msgdb flag-map)
    t))

(luna-define-method elmo-msgdb-sort-entities ((msgdb modb-standard)
					      predicate &optional app-data)
  (message "Sorting...")
  (let ((numbers (modb-standard-number-list-internal msgdb)))
    (modb-standard-set-number-list-internal
     msgdb
     (sort numbers (lambda (a b)
		     (funcall predicate
			      (elmo-msgdb-message-entity msgdb a)
			      (elmo-msgdb-message-entity msgdb b)
			      app-data))))
    (message "Sorting...done")
    msgdb))

(defun modb-standard-message-entity (msgdb key load)
  (let ((ret (elmo-get-hash-val
	      key
	      (modb-standard-entity-map-internal msgdb)))
	(inhibit-quit t))
    (if (eq 'autoload (car-safe ret))
	(when (and load modb-standard-divide-number)
	  (modb-standard-load-entity
	   msgdb
	   (elmo-msgdb-location msgdb)
	   (/ (nth 1 ret) modb-standard-divide-number))
	  (modb-standard-message-entity msgdb key nil))
      ret)))

(luna-define-method elmo-msgdb-message-number ((msgdb modb-standard)
					       message-id)
  (let ((ret (elmo-get-hash-val
	      message-id
	      (modb-standard-entity-map-internal msgdb))))
    (if (eq 'autoload (car-safe ret))
	;; Not loaded yet but can return number.
	(nth 1 ret)
      (elmo-message-entity-number ret))))

(luna-define-method elmo-msgdb-message-field ((msgdb modb-standard)
					      number field &optional type)
  (let ((ret (elmo-get-hash-val
	      (modb-standard-key number)
	      (modb-standard-entity-map-internal msgdb))))
    (if (and (eq 'autoload (car-safe ret)) (eq field 'message-id))
	;; Not loaded yet but can return message-id
	(cdr (cdr ret))
      (elmo-message-entity-field (elmo-msgdb-message-entity
				  msgdb (modb-standard-key number))
				 field type))))

(luna-define-method elmo-msgdb-message-entity ((msgdb modb-standard) key)
  (when key
    (modb-standard-message-entity
     msgdb
     (cond ((stringp key) key)
	   ((numberp key) (modb-standard-key key)))
     'autoload)))

(luna-define-method elmo-msgdb-message-entity-handler ((msgdb modb-standard))
  (or (modb-standard-overview-handler-internal msgdb)
      (modb-standard-set-overview-handler-internal
       msgdb
       (luna-make-entity 'modb-standard-entity-handler
			 :mime-charset
			 (modb-generic-mime-charset-internal msgdb)))))

(require 'product)
(product-provide (provide 'modb-standard) (require 'elmo-version))

;;; modb-standard.el ends here
