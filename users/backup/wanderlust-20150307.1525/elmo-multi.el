;;; elmo-multi.el --- Multiple Folder Interface for ELMO.

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
(require 'elmo-signal)
(require 'luna)

(defvar elmo-multi-divide-number 100000
  "*Multi divider number.")

;;; ELMO Multi folder
(eval-and-compile
  (luna-define-class elmo-multi-folder (elmo-folder)
		     (children divide-number))
  (luna-define-internal-accessors 'elmo-multi-folder))

(defmacro elmo-multi-real-folder-number (folder number)
  "Returns a cons cell of real FOLDER and NUMBER."
  `(cons (nth (-
	       (/ ,number
		  (elmo-multi-folder-divide-number-internal ,folder))
	       1) (elmo-multi-folder-children-internal ,folder))
	 (% ,number (elmo-multi-folder-divide-number-internal
		     ,folder))))

(luna-define-method elmo-folder-initialize ((folder
					     elmo-multi-folder)
					    name)
  (while (> (length (car (setq name (elmo-parse-token name "," nil t)))) 0)
    (elmo-multi-folder-set-children-internal
     folder
     (nconc (elmo-multi-folder-children-internal
	     folder)
	    (list (elmo-get-folder (car name)))))
    (setq name (cdr name))
    (when (and (> (length name) 0)
	       (eq (aref name 0) ?,))
      (setq name (substring name 1))))
  (elmo-multi-folder-set-divide-number-internal
   folder
   elmo-multi-divide-number)
  (elmo-multi-connect-signals folder)
  folder)

(defun elmo-multi-connect-signals (folder)
  (elmo-connect-signal
   nil 'flag-changing folder
   (elmo-define-signal-handler (folder child number old-flags new-flags)
     (elmo-emit-signal 'flag-changing folder
		       (car (elmo-multi-map-numbers folder child (list number)))
		       old-flags new-flags))
   (elmo-define-signal-filter (folder sender)
     (memq sender (elmo-multi-folder-children-internal folder))))
  (elmo-connect-signal
   nil 'flag-changed folder
   (elmo-define-signal-handler (folder child numbers)
     (elmo-emit-signal 'flag-changed folder
		       (elmo-multi-map-numbers folder child numbers)))
   (elmo-define-signal-filter (folder sender)
     (memq sender (elmo-multi-folder-children-internal folder))))
  (elmo-connect-signal
   nil 'status-changed folder
   (elmo-define-signal-handler (folder child numbers)
     (elmo-emit-signal 'status-changed folder
		       (elmo-multi-map-numbers folder child numbers)))
   (elmo-define-signal-filter (folder sender)
     (memq sender (elmo-multi-folder-children-internal folder))))
  (elmo-connect-signal
   nil 'update-overview folder
   (elmo-define-signal-handler (folder child number)
     (elmo-emit-signal
      'update-overview folder
      (car (elmo-multi-map-numbers folder child (list number)))))
   (elmo-define-signal-filter (folder sender)
     (memq sender (elmo-multi-folder-children-internal folder)))))

(defun elmo-multi-map-numbers (folder child numbers)
  (let ((multi (catch 'found
		 (let ((children (elmo-multi-folder-children-internal folder))
		       (index 0))
		   (while children
		     (setq index (1+ index))
		     (when (eq (car children) child)
		       (throw 'found index))
		     (setq children (cdr children)))))))
    (when multi
      (let ((offset (* (elmo-multi-folder-divide-number-internal folder)
		       multi)))
      (mapcar (lambda (number) (+ offset number))
	      numbers)))))


(luna-define-method elmo-folder-open-internal ((folder elmo-multi-folder))
  (dolist (fld (elmo-multi-folder-children-internal folder))
    (elmo-folder-open-internal fld)))

(luna-define-method elmo-folder-check ((folder elmo-multi-folder))
  (dolist (fld (elmo-multi-folder-children-internal folder))
    (elmo-folder-check fld)))

(luna-define-method elmo-folder-close-internal ((folder elmo-multi-folder))
  (dolist (fld (elmo-multi-folder-children-internal folder))
    (elmo-folder-close-internal fld)))

(luna-define-method elmo-folder-close ((folder elmo-multi-folder))
  (elmo-generic-folder-close folder)
  (dolist (fld (elmo-multi-folder-children-internal folder))
    (elmo-folder-close fld)))

(luna-define-method elmo-message-killed-p ((folder elmo-multi-folder) number)
  (let ((pair (elmo-multi-real-folder-number folder number)))
    (elmo-message-killed-p (car pair) (cdr pair))))

(luna-define-method elmo-folder-synchronize ((folder elmo-multi-folder)
					     &optional
					     disable-killed
					     ignore-msgdb
					     no-check
					     mask)
  (if mask
      (dolist (element (elmo-multi-split-numbers folder mask))
	(when (cdr element)
	  (elmo-folder-synchronize (car element)
				   disable-killed
				   ignore-msgdb
				   no-check
				   (cdr element))))
    (dolist (fld (elmo-multi-folder-children-internal folder))
      (elmo-folder-synchronize fld disable-killed ignore-msgdb no-check)))
  0)

(luna-define-method elmo-folder-expand-msgdb-path ((folder
						    elmo-multi-folder))
  (expand-file-name (elmo-replace-string-as-filename
		     (elmo-folder-name-internal folder))
		    (expand-file-name "multi"
				      elmo-msgdb-directory)))

(luna-define-method elmo-folder-newsgroups ((folder elmo-multi-folder))
  (apply  #'nconc
	  (mapcar
	   'elmo-folder-newsgroups
	   (elmo-multi-folder-children-internal folder))))

(luna-define-method elmo-folder-get-primitive-list ((folder elmo-multi-folder))
  (elmo-flatten
   (mapcar
    'elmo-folder-get-primitive-list
    (elmo-multi-folder-children-internal folder))))

(luna-define-method elmo-folder-contains-type ((folder elmo-multi-folder) type)
  (let ((children (elmo-multi-folder-children-internal folder))
	match)
    (while children
      (when (elmo-folder-contains-type (car children) type)
	(setq match t)
	(setq children nil))
      (setq children (cdr children)))
    match))

(luna-define-method elmo-message-folder ((folder elmo-multi-folder)
					 number)
  (nth (- (/ number (elmo-multi-folder-divide-number-internal folder)) 1)
       (elmo-multi-folder-children-internal folder)))

(luna-define-method elmo-message-cached-p ((folder elmo-multi-folder) number)
  (let ((pair (elmo-multi-real-folder-number folder number)))
    (elmo-message-cached-p (car pair) (cdr pair))))

(luna-define-method elmo-message-set-cached ((folder elmo-multi-folder)
					     number cached)
  (let ((pair (elmo-multi-real-folder-number folder number)))
    (elmo-message-set-cached (car pair) (cdr pair) cached)))

(luna-define-method elmo-find-fetch-strategy ((folder elmo-multi-folder)
					      number
					      &optional
					      ignore-cache
					      require-entireness)
  (let ((pair (elmo-multi-real-folder-number folder number)))
    (elmo-find-fetch-strategy (car pair)
			      (cdr pair)
			      ignore-cache
			      require-entireness)))

(luna-define-method elmo-message-number ((folder elmo-multi-folder)
					 message-id)
  (let ((children (elmo-multi-folder-children-internal folder))
	match)
    (while children
      (when (setq match (elmo-message-number (car children) message-id))
	(setq children nil))
      (setq children (cdr children)))
    match))

(luna-define-method elmo-message-entity ((folder elmo-multi-folder) key)
  (cond
   ((numberp key)
    (let* ((pair (elmo-multi-real-folder-number folder key))
	   (entity (elmo-message-entity (car pair) (cdr pair))))
      (when entity
	(setq entity (elmo-message-copy-entity entity))
	(elmo-message-entity-set-number entity key)
	entity)))
   ((stringp key)
    (let ((children (elmo-multi-folder-children-internal folder))
	  (cur-number 0)
	  match)
      (while children
	(setq cur-number (+ cur-number 1))
	(when (setq match (elmo-message-entity (car children) key))
	  (setq match (elmo-message-copy-entity match))
	  (elmo-message-entity-set-number
	   match
	   (+ (* (elmo-multi-folder-divide-number-internal folder)
		 cur-number)
	      (elmo-message-entity-number match)))
	  (setq children nil))
	(setq children (cdr children)))
      match))))

(luna-define-method elmo-message-entity-parent ((folder
						 elmo-multi-folder) entity)
  (let ((references (elmo-message-entity-field entity 'references))
	parent)
    ;; In old msgdb, references's field is a string.
    (when (stringp references)
      (setq references (list references)))
    (while references
      (setq references
	    (if (setq parent (elmo-message-entity folder (car references)))
		nil
	      (cdr references))))
    parent))

(luna-define-method elmo-message-field ((folder elmo-multi-folder)
					number field &optional type)
  (let ((pair (elmo-multi-real-folder-number folder number)))
    (elmo-message-field (car pair) (cdr pair) field type)))

(luna-define-method elmo-message-flag-available-p ((folder
						    elmo-multi-folder) number
						    flag)
  (let ((pair (elmo-multi-real-folder-number folder number)))
    (elmo-message-flag-available-p (car pair) (cdr pair) flag)))

(luna-define-method elmo-message-flags ((folder elmo-multi-folder) number)
  (let ((pair (elmo-multi-real-folder-number folder number)))
    (elmo-message-flags (car pair) (cdr pair))))

(defun elmo-multi-split-numbers (folder numlist &optional as-is)
  (let ((numbers (sort numlist '<))
	(folders (elmo-multi-folder-children-internal folder))
	(divider (elmo-multi-folder-divide-number-internal folder))
	(cur-number 0)
	one-list numbers-list)
    (while numbers
      (setq one-list (list (nth cur-number folders)))
      (setq cur-number (+ cur-number 1))
      (while (and numbers
		  (eq 0
		      (/ (- (car numbers)
			    (* divider cur-number))
			 divider)))
	(setq one-list (nconc
			one-list
			(list
			 (if as-is
			     (car numbers)
			   (% (car numbers)
			      (* divider cur-number))))))
	(setq numbers (cdr numbers)))
      (setq numbers-list (nconc numbers-list (list one-list))))
    numbers-list))

(luna-define-method elmo-folder-process-crosspost ((folder elmo-multi-folder))
  (dolist (child (elmo-multi-folder-children-internal folder))
    (elmo-folder-process-crosspost child)))

(luna-define-method elmo-message-fetch ((folder elmo-multi-folder)
					number strategy
					&optional unseen section)
  (let ((pair (elmo-multi-real-folder-number folder number)))
    (elmo-message-fetch (car pair) (cdr pair) strategy unseen section)))

(luna-define-method elmo-folder-delete-messages ((folder elmo-multi-folder)
						 numbers)
  (dolist (element (elmo-multi-split-numbers folder numbers))
    (when (cdr element)
      (elmo-folder-delete-messages (car element) (cdr element))))
  t)

(luna-define-method elmo-folder-detach-messages ((folder elmo-multi-folder)
						 numbers)
  (dolist (element (elmo-multi-split-numbers folder numbers))
    (when (cdr element)
      (elmo-folder-detach-messages (car element) (cdr element))))
  t)

(luna-define-method elmo-folder-diff ((folder elmo-multi-folder))
  (elmo-multi-folder-diff folder))

(defun elmo-multi-folder-diff (folder)
  (let ((news 0)
	(unreads 0)
	(alls 0)
	diff value)
    (dolist (child (elmo-multi-folder-children-internal folder))
      (setq diff (elmo-folder-diff child))
      (setq news    (and news
			 (setq value (elmo-diff-new diff))
			 (+ news value))
	    unreads (and unreads
			 (setq value (elmo-diff-unread diff))
			 (+ unreads value))
	    alls    (and alls
			 (setq value (elmo-diff-all diff))
			 (+ alls value))))
    (if unreads
	(list news unreads alls)
      (cons news alls))))

(luna-define-method elmo-folder-list-messages
  ((folder elmo-multi-folder) &optional visible-only in-msgdb)
  (let* ((flds (elmo-multi-folder-children-internal folder))
	 (cur-number 0)
	 list numbers)
    (while flds
      (setq cur-number (+ cur-number 1))
      (setq list (elmo-folder-list-messages (car flds) visible-only in-msgdb))
      (setq numbers
	    (nconc
	     numbers
	     (mapcar
	      (lambda (x)
		(+
		 (* (elmo-multi-folder-divide-number-internal
		     folder) cur-number) x))
	      list)))
      (setq flds (cdr flds)))
    numbers))

(luna-define-method elmo-folder-exists-p ((folder elmo-multi-folder))
  (let ((flds (elmo-multi-folder-children-internal folder)))
    (catch 'exists
      (while flds
	(unless (elmo-folder-exists-p (car flds))
	  (throw 'exists nil))
	(setq flds (cdr flds)))
      t)))

(luna-define-method elmo-folder-creatable-p ((folder elmo-multi-folder))
  (let ((flds (elmo-multi-folder-children-internal folder)))
    (catch 'creatable
      (while flds
	(when (and (elmo-folder-creatable-p (car flds))
		   (not (elmo-folder-exists-p (car flds))))
	  ;; If folder already exists, don't to `creatable'.
	  ;; Because this function is called, when folder doesn't exists.
	  (throw 'creatable t))
	(setq flds (cdr flds)))
      nil)))

(luna-define-method elmo-folder-create ((folder elmo-multi-folder))
  (let ((flds (elmo-multi-folder-children-internal folder)))
    (catch 'create
      (while flds
	(unless (or (elmo-folder-exists-p (car flds))
		    (elmo-folder-create (car flds)))
	  (throw 'create nil))
	(setq flds (cdr flds)))
      t)))

(luna-define-method elmo-folder-search ((folder elmo-multi-folder)
					condition &optional numbers)
  (apply 'nconc
	 (delq nil
	       (if (and numbers (listp numbers))
		   (mapcar (lambda (element)
			     (when (cdr element)
			       (elmo-multi-map-numbers
				folder
				(car element)
				(elmo-folder-search
				 (car element) condition (cdr element)))))
			   (elmo-multi-split-numbers folder numbers))
		 (mapcar (lambda (child)
			   (elmo-multi-map-numbers
			    folder child (elmo-folder-search child condition numbers)))
			 (elmo-multi-folder-children-internal folder))))))

(luna-define-method elmo-message-use-cache-p ((folder elmo-multi-folder)
					      number)
  (let ((pair (elmo-multi-real-folder-number folder number)))
    (elmo-message-use-cache-p (car pair) (cdr pair))))

(luna-define-method elmo-message-file-p ((folder elmo-multi-folder) number)
  (let ((pair (elmo-multi-real-folder-number folder number)))
    (elmo-message-file-p (car pair) (cdr pair))))

(luna-define-method elmo-message-file-name ((folder elmo-multi-folder) number)
  (let ((pair (elmo-multi-real-folder-number folder number)))
    (elmo-message-file-name (car pair) (cdr pair))))

(luna-define-method elmo-folder-plugged-p ((folder elmo-multi-folder))
  (let ((flds (elmo-multi-folder-children-internal folder)))
    (catch 'plugged
      (while flds
	(unless (elmo-folder-plugged-p (car flds))
	  (throw 'plugged nil))
	(setq flds (cdr flds)))
      t)))

(luna-define-method elmo-folder-set-plugged ((folder elmo-multi-folder)
					     plugged add)
  (let ((flds  (elmo-multi-folder-children-internal folder)))
    (dolist (fld flds)
      (elmo-folder-set-plugged fld plugged add))))

(defun elmo-multi-folder-numbers-list-assoc (folder folder-numbers)
  (let (ent)
    (while folder-numbers
      (when (string= (elmo-folder-name-internal (car (car folder-numbers)))
		     (elmo-folder-name-internal folder))
	(setq ent (car folder-numbers)
	      folder-numbers nil))
      (setq folder-numbers (cdr folder-numbers)))
    ent))

(defun elmo-multi-make-folder-numbers-list (folder msgs)
  (let ((msg-list msgs)
	pair fld-list
	ret-val)
    (while msg-list
      (when (and (numberp (car msg-list))
		 (> (car msg-list) 0))
	(setq pair (elmo-multi-real-folder-number folder (car msg-list)))
	(if (setq fld-list (elmo-multi-folder-numbers-list-assoc
			    (car pair)
			    ret-val))
	    (setcdr fld-list (cons (cdr pair) (cdr fld-list)))
	  (setq ret-val (cons (cons (car pair) (list (cdr pair))) ret-val))))
      (setq msg-list (cdr msg-list)))
    ret-val))

(luna-define-method elmo-folder-set-flag ((folder elmo-multi-folder)
					  numbers
					  flag
					  &optional is-local)
  (dolist (pair (elmo-multi-make-folder-numbers-list folder numbers))
    (elmo-folder-set-flag (car pair) (cdr pair) flag is-local)))

(luna-define-method elmo-folder-unset-flag ((folder elmo-multi-folder)
					    numbers
					    flag
					    &optional is-local)
  (dolist (pair (elmo-multi-make-folder-numbers-list folder numbers))
    (ignore-errors
     (elmo-folder-unset-flag (car pair) (cdr pair) flag is-local))))

(luna-define-method elmo-folder-list-flagged ((folder elmo-multi-folder)
					      flag
					      &optional in-msgdb)
  (let ((cur-number 0)
	numbers)
    (dolist (child (elmo-multi-folder-children-internal folder))
      (setq cur-number (+ cur-number 1)
	    numbers
	    (nconc
	     numbers
	     (mapcar
	      (lambda (x)
		(+
		 (* (elmo-multi-folder-divide-number-internal folder)
		    cur-number) x))
	      (elmo-folder-list-flagged child flag in-msgdb)))))
    numbers))

(luna-define-method elmo-folder-commit ((folder elmo-multi-folder))
  (dolist (child (elmo-multi-folder-children-internal folder))
    (elmo-folder-commit child)))

(luna-define-method elmo-folder-length ((folder elmo-multi-folder))
  (let ((sum 0))
    (dolist (child (elmo-multi-folder-children-internal folder))
      (setq sum (+ sum (elmo-folder-length child))))
    sum))

(luna-define-method elmo-folder-count-flags ((folder elmo-multi-folder))
  (let (flag-alist element)
    (dolist (child (elmo-multi-folder-children-internal folder))
      (dolist (pair (elmo-folder-count-flags child))
	(if (setq element (assq (car pair) flag-alist))
	    (setcdr element (+ (cdr element) (cdr pair)))
	  (setq flag-alist (cons pair flag-alist)))))
    flag-alist))

(luna-define-method elmo-folder-recover-messages ((folder elmo-multi-folder)
						  numbers)
  (dolist (element (elmo-multi-split-numbers folder numbers))
    (when (cdr element)
      (elmo-folder-recover-messages (car element) (cdr element)))))

(defun elmo-folder-append-messages-multi-* (dst-folder
					    src-folder
					    numbers
					    same-number)
  (if same-number
      (elmo-folder-append-messages dst-folder src-folder numbers same-number
				   'elmo-folder-append-messages-multi-*)
    (let ((divider (elmo-multi-folder-divide-number-internal src-folder))
	  (cur-number 0)
	  succeeds)
      (dolist (element (elmo-multi-split-numbers src-folder numbers))
	(setq cur-number (+ cur-number 1))
	(when (cdr element)
	  (setq succeeds
		(nconc
		 succeeds
		 (mapcar
		  (lambda (x)
		    (+ (* divider cur-number) x))
		  (elmo-folder-append-messages
		   dst-folder (car element) (cdr element)))))))
      succeeds)))

(require 'product)
(product-provide (provide 'elmo-multi) (require 'elmo-version))

;;; elmo-multi.el ends here
