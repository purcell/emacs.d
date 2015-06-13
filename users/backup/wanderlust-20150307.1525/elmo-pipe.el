;;; elmo-pipe.el --- PIPE Interface for ELMO.

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

(require 'elmo)
(require 'elmo-signal)

(defvar elmo-pipe-folder-copied-filename "copied"
  "Copied messages number set.")

;;; ELMO pipe folder
(eval-and-compile
  (luna-define-class elmo-pipe-folder (elmo-folder)
		     (src dst copy))
  (luna-define-internal-accessors 'elmo-pipe-folder))

(luna-define-method elmo-folder-initialize ((folder elmo-pipe-folder)
					    name)
  (when (string-match "^\\([^|]*\\)|\\(:?\\)\\(.*\\)$" name)
    ;; elmo-get-folder might overwrite our match data
    (let ((src (match-string 1 name))
          (dst (match-string 3 name))
          (copy (string= ":" (match-string 2 name))))
      (elmo-pipe-folder-set-src-internal folder (elmo-get-folder src))
      (elmo-pipe-folder-set-dst-internal folder (elmo-get-folder dst))
      (elmo-pipe-folder-set-copy-internal folder copy)))
  (elmo-pipe-connect-signals folder (elmo-pipe-folder-dst-internal folder))
  folder)

(defun elmo-pipe-connect-signals (folder destination)
  (elmo-connect-signal
   destination 'flag-changing folder
   (elmo-define-signal-handler (folder dst number old-flags new-flags)
     (elmo-emit-signal 'flag-changing folder number old-flags new-flags)))
  (elmo-connect-signal
   destination 'flag-changed folder
   (elmo-define-signal-handler (folder dst numbers)
     (elmo-emit-signal 'flag-changed folder numbers)))
  (elmo-connect-signal
   destination 'status-changed folder
   (elmo-define-signal-handler (folder dst numbers)
     (elmo-emit-signal 'status-changed folder numbers)))
  (elmo-connect-signal
   destination 'update-overview folder
   (elmo-define-signal-handler (folder dst number)
     (elmo-emit-signal 'update-overview folder number))))

(luna-define-method elmo-folder-get-primitive-list ((folder elmo-pipe-folder))
  (nconc
   (elmo-folder-get-primitive-list (elmo-pipe-folder-src-internal folder))
   (elmo-folder-get-primitive-list (elmo-pipe-folder-dst-internal folder))))

(luna-define-method elmo-folder-contains-type ((folder elmo-pipe-folder)
					       type)
  (or (elmo-folder-contains-type (elmo-pipe-folder-src-internal folder) type)
      (elmo-folder-contains-type (elmo-pipe-folder-dst-internal folder) type)))

(defun elmo-folder-append-messages-pipe-* (dst-folder
					   src-folder
					   numbers
					   same-number)
  (elmo-folder-append-messages dst-folder
			       (elmo-pipe-folder-dst-internal src-folder)
			       numbers
			       same-number))

(defun elmo-folder-append-messages-*-pipe (dst-folder
					   src-folder
					   numbers
					   same-number)
  (elmo-folder-append-messages (elmo-pipe-folder-dst-internal dst-folder)
			       src-folder
			       numbers
			       same-number))

(luna-define-method elmo-folder-append-buffer ((folder elmo-pipe-folder)
					       &optional flag number
					       return-number)
  (elmo-folder-append-buffer (elmo-pipe-folder-dst-internal folder)
			     flag number return-number))

(luna-define-method elmo-message-fetch ((folder elmo-pipe-folder)
					number strategy
					&optional unseen section)
  (elmo-message-fetch (elmo-pipe-folder-dst-internal folder)
		      number strategy unseen section))

(luna-define-method elmo-folder-clear :after ((folder elmo-pipe-folder)
					      &optional keep-killed)
  (unless keep-killed
    (elmo-pipe-folder-copied-list-save folder nil)))

(luna-define-method elmo-folder-delete-messages ((folder elmo-pipe-folder)
						 numbers)
  (elmo-folder-delete-messages (elmo-pipe-folder-dst-internal folder)
			       numbers))

(luna-define-method elmo-folder-detach-messages ((folder elmo-pipe-folder)
						 numbers)
  (elmo-folder-detach-messages (elmo-pipe-folder-dst-internal folder)
			       numbers))

(defvar elmo-pipe-drained-hook nil "A hook called when the pipe is flushed.")

(defsubst elmo-pipe-folder-list-target-messages (src &optional ignore-list)
  (let ((killed (elmo-folder-killed-list-internal src)))
    (elmo-folder-set-killed-list-internal src ignore-list)
    (unwind-protect
	(elmo-folder-list-messages src t)
      (elmo-folder-set-killed-list-internal src killed))))

(defun elmo-pipe-drain (src dst &optional copy ignore-list)
  "Move or copy all messages of SRC to DST."
  (let ((elmo-inhibit-number-mapping (and (eq (elmo-folder-type-internal src)
					      'pop3)
					  (not copy)))) ; No need to use UIDL
    (message "Checking %s..." (elmo-folder-name-internal src))
    (elmo-folder-open src)
    (unwind-protect
	(let ((msgs (elmo-pipe-folder-list-target-messages src ignore-list)))
	  (elmo-with-progress-display (elmo-folder-move-messages (length msgs))
	      (if copy "Copying messages" "Moving messages")
	    (elmo-folder-move-messages src msgs dst copy))
	  (when (and copy msgs)
	    (setq ignore-list (elmo-number-set-append-list ignore-list msgs))))
      (elmo-folder-close src))
    (run-hooks 'elmo-pipe-drained-hook)
    ignore-list))

(defun elmo-pipe-folder-copied-list-load (folder)
  (elmo-object-load
   (expand-file-name elmo-pipe-folder-copied-filename
		     (expand-file-name
		      (elmo-replace-string-as-filename
		       (elmo-folder-name-internal folder))
		      (expand-file-name "pipe" elmo-msgdb-directory)))
   nil t))

(defun elmo-pipe-folder-copied-list-save (folder copied-list)
  (elmo-object-save
   (expand-file-name elmo-pipe-folder-copied-filename
		     (expand-file-name
		      (elmo-replace-string-as-filename
		       (elmo-folder-name-internal folder))
		      (expand-file-name "pipe" elmo-msgdb-directory)))
   copied-list))

(luna-define-method elmo-folder-msgdb ((folder elmo-pipe-folder))
  (elmo-folder-msgdb (elmo-pipe-folder-dst-internal folder)))

(luna-define-method elmo-folder-open-internal ((folder elmo-pipe-folder))
  (elmo-folder-open-internal (elmo-pipe-folder-dst-internal folder)))

(luna-define-method elmo-folder-close-internal ((folder elmo-pipe-folder))
  (elmo-folder-close-internal (elmo-pipe-folder-dst-internal folder)))

(luna-define-method elmo-folder-close ((folder elmo-pipe-folder))
  (elmo-generic-folder-close folder)
  (elmo-folder-close (elmo-pipe-folder-dst-internal folder)))

(luna-define-method elmo-folder-list-messages ((folder elmo-pipe-folder)
					       &optional visible-only in-msgdb)
  ;; Use target folder's killed-list in the pipe folder.
  (elmo-folder-list-messages (elmo-pipe-folder-dst-internal
			      folder) visible-only in-msgdb))

(luna-define-method elmo-folder-diff ((folder elmo-pipe-folder))
  (elmo-folder-open-internal (elmo-pipe-folder-src-internal folder))
  (elmo-folder-open-internal (elmo-pipe-folder-dst-internal folder))
  (let* ((elmo-inhibit-number-mapping
	  (not (elmo-pipe-folder-copy-internal folder)))
	 (src-length (length (elmo-pipe-folder-list-target-messages
			      (elmo-pipe-folder-src-internal folder)
			      (elmo-pipe-folder-copied-list-load folder))))
	 (dst-diff (elmo-folder-diff (elmo-pipe-folder-dst-internal folder))))
    (prog1
	(cond
	 ((consp (cdr dst-diff)) ; new unread all
	  (mapcar (lambda (number) (+ number src-length)) dst-diff))
	 (t
	  (cons (+ (or (car dst-diff) 0) src-length)
		(+ (or (cdr dst-diff) 0) src-length))))
      ;; No save.
      (elmo-folder-close-internal (elmo-pipe-folder-src-internal folder))
      (elmo-folder-close-internal (elmo-pipe-folder-dst-internal folder)))))

(luna-define-method elmo-folder-exists-p ((folder elmo-pipe-folder))
  (and (elmo-folder-exists-p (elmo-pipe-folder-src-internal folder))
       (elmo-folder-exists-p (elmo-pipe-folder-dst-internal folder))))

(luna-define-method elmo-folder-expand-msgdb-path ((folder
						    elmo-pipe-folder))
  ;; Share with destination...OK?
  (elmo-folder-expand-msgdb-path (elmo-pipe-folder-dst-internal folder)))

(luna-define-method elmo-folder-newsgroups ((folder elmo-pipe-folder))
  (elmo-folder-newsgroups (elmo-pipe-folder-src-internal folder)))

(luna-define-method elmo-folder-creatable-p ((folder elmo-pipe-folder))
  (and (or
	(elmo-folder-exists-p (elmo-pipe-folder-src-internal folder))
	(elmo-folder-creatable-p (elmo-pipe-folder-src-internal folder)))
       (or
	(elmo-folder-exists-p (elmo-pipe-folder-dst-internal folder))
	(elmo-folder-creatable-p (elmo-pipe-folder-dst-internal folder)))))

(luna-define-method elmo-folder-writable-p ((folder elmo-pipe-folder))
  (elmo-folder-writable-p (elmo-pipe-folder-dst-internal folder)))

(luna-define-method elmo-folder-recover-messages ((folder elmo-pipe-folder)
						  numbers)
  (elmo-folder-recover-messages
   (elmo-pipe-folder-dst-internal folder) numbers))

(luna-define-method elmo-folder-create ((folder elmo-pipe-folder))
  (if (and (not (elmo-folder-exists-p (elmo-pipe-folder-src-internal folder)))
	   (elmo-folder-creatable-p (elmo-pipe-folder-src-internal folder)))
      (elmo-folder-create (elmo-pipe-folder-src-internal folder)))
  (if (and (not (elmo-folder-exists-p (elmo-pipe-folder-dst-internal folder)))
	   (elmo-folder-creatable-p (elmo-pipe-folder-dst-internal folder)))
      (elmo-folder-create (elmo-pipe-folder-dst-internal folder))))

(luna-define-method elmo-folder-search ((folder elmo-pipe-folder)
					condition &optional numlist)
  (elmo-folder-search (elmo-pipe-folder-dst-internal folder)
		      condition numlist))

(luna-define-method elmo-message-use-cache-p ((folder elmo-pipe-folder) number)
  (elmo-message-use-cache-p (elmo-pipe-folder-dst-internal folder) number))

(luna-define-method elmo-folder-check ((folder elmo-pipe-folder))
  (elmo-folder-check (elmo-pipe-folder-dst-internal folder)))

(luna-define-method elmo-folder-plugged-p ((folder elmo-pipe-folder))
  (and (elmo-folder-plugged-p (elmo-pipe-folder-src-internal folder))
       (elmo-folder-plugged-p (elmo-pipe-folder-dst-internal folder))))

(luna-define-method elmo-folder-set-plugged ((folder elmo-pipe-folder)
					     plugged &optional add)
  (elmo-folder-set-plugged (elmo-pipe-folder-src-internal folder)
			   plugged add)
  (elmo-folder-set-plugged (elmo-pipe-folder-dst-internal folder)
			   plugged add))

(luna-define-method elmo-folder-message-file-p ((folder elmo-pipe-folder))
  (elmo-folder-message-file-p (elmo-pipe-folder-dst-internal folder)))

(luna-define-method elmo-message-file-p ((folder elmo-pipe-folder) number)
  (elmo-message-file-p (elmo-pipe-folder-dst-internal folder) number))

(luna-define-method elmo-message-file-name ((folder elmo-pipe-folder) number)
  (elmo-message-file-name (elmo-pipe-folder-dst-internal folder) number))

(luna-define-method elmo-folder-message-file-number-p ((folder
							elmo-pipe-folder))
  (elmo-folder-message-file-number-p (elmo-pipe-folder-dst-internal folder)))

(luna-define-method elmo-folder-message-file-directory ((folder
							 elmo-pipe-folder))
  (elmo-folder-message-file-directory
   (elmo-pipe-folder-dst-internal folder)))

(luna-define-method elmo-folder-message-make-temp-file-p
  ((folder elmo-pipe-folder))
  (elmo-folder-message-make-temp-file-p
   (elmo-pipe-folder-dst-internal folder)))

(luna-define-method elmo-folder-message-make-temp-files ((folder
							  elmo-pipe-folder)
							 numbers
							 &optional
							 start-number)
  (elmo-folder-message-make-temp-files
   (elmo-pipe-folder-dst-internal folder) numbers start-number))

(luna-define-method elmo-folder-set-flag ((folder elmo-pipe-folder)
					  numbers
					  flag
					  &optional is-local)
  (elmo-folder-set-flag (elmo-pipe-folder-dst-internal folder)
			numbers flag is-local))

(luna-define-method elmo-folder-unset-flag ((folder elmo-pipe-folder)
					    numbers
					    flag
					    &optional is-local)
  (elmo-folder-unset-flag (elmo-pipe-folder-dst-internal folder)
			  numbers flag is-local))

(luna-define-method elmo-folder-pack-numbers ((folder elmo-pipe-folder))
  (elmo-folder-pack-numbers (elmo-pipe-folder-dst-internal folder)))

(luna-define-method elmo-folder-rename ((folder elmo-pipe-folder) new-name)
  (let* ((new-folder (elmo-get-folder new-name)))
    (unless (string= (elmo-folder-name-internal
		      (elmo-pipe-folder-src-internal folder))
		     (elmo-folder-name-internal
		      (elmo-pipe-folder-src-internal new-folder)))
      (error "Source folder differ"))
    (unless (eq (elmo-folder-type-internal
		 (elmo-pipe-folder-dst-internal folder))
		(elmo-folder-type-internal
		 (elmo-pipe-folder-dst-internal new-folder)))
      (error "Not same folder type"))
    (elmo-folder-rename (elmo-pipe-folder-dst-internal folder)
			(elmo-folder-name-internal
			 (elmo-pipe-folder-dst-internal new-folder)))
    (elmo-global-flag-replace-referrer (elmo-folder-name-internal folder)
				       new-name)
    (elmo-msgdb-rename-path folder new-folder)))

(luna-define-method elmo-folder-synchronize ((folder elmo-pipe-folder)
					     &optional
					     disable-killed
					     ignore-msgdb
					     no-check
					     mask)
  (let ((src-folder (elmo-pipe-folder-src-internal folder))
	(dst-folder (elmo-pipe-folder-dst-internal folder)))
    (when (and (elmo-folder-plugged-p src-folder)
	       (elmo-folder-plugged-p dst-folder))
      (if (elmo-pipe-folder-copy-internal folder)
	  (elmo-pipe-folder-copied-list-save
	   folder
	   (elmo-pipe-drain src-folder
			    dst-folder
			    'copy
			    (elmo-pipe-folder-copied-list-load folder)))
	(elmo-pipe-drain src-folder dst-folder))))
  (elmo-folder-synchronize
   (elmo-pipe-folder-dst-internal folder)
   disable-killed ignore-msgdb no-check mask))

(luna-define-method elmo-folder-list-flagged ((folder elmo-pipe-folder)
					      flag
					      &optional in-msgdb)
  (elmo-folder-list-flagged
   (elmo-pipe-folder-dst-internal folder) flag in-msgdb))

(luna-define-method elmo-folder-commit ((folder elmo-pipe-folder))
  (elmo-folder-commit (elmo-pipe-folder-dst-internal folder)))

(luna-define-method elmo-folder-length ((folder elmo-pipe-folder))
  (elmo-folder-length (elmo-pipe-folder-dst-internal folder)))

(luna-define-method elmo-message-flag-available-p ((folder elmo-pipe-folder)
						   number flag)
  (elmo-message-flag-available-p
   (elmo-pipe-folder-dst-internal folder)
   number flag))

(luna-define-method elmo-folder-count-flags ((folder elmo-pipe-folder))
  (elmo-folder-count-flags (elmo-pipe-folder-dst-internal folder)))

(luna-define-method elmo-message-flags ((folder elmo-pipe-folder) number)
  (elmo-message-flags (elmo-pipe-folder-dst-internal folder) number))

(luna-define-method elmo-message-field ((folder elmo-pipe-folder)
					number field &optional type)
  (elmo-message-field (elmo-pipe-folder-dst-internal folder)
		      number
		      field
		      type))

(luna-define-method elmo-message-killed-p ((folder elmo-pipe-folder) number)
  (elmo-message-killed-p (elmo-pipe-folder-dst-internal folder) number))

(luna-define-method elmo-message-set-cached ((folder elmo-pipe-folder)
					     number cached)
  (elmo-message-set-cached (elmo-pipe-folder-dst-internal folder)
			   number cached))

(luna-define-method elmo-find-fetch-strategy ((folder elmo-pipe-folder)
					      number
					      &optional
					      ignore-cache
					      require-entireness)
  (elmo-find-fetch-strategy (elmo-pipe-folder-dst-internal folder)
			    number
			    ignore-cache
			    require-entireness))

(luna-define-method elmo-message-number ((folder elmo-pipe-folder)
					 message-id)
  (elmo-message-number (elmo-pipe-folder-dst-internal folder)
		       message-id))

(luna-define-method elmo-message-entity ((folder elmo-pipe-folder) key)
  (elmo-message-entity (elmo-pipe-folder-dst-internal folder) key))

(luna-define-method elmo-message-folder ((folder elmo-pipe-folder)
					 number)
  (elmo-message-folder (elmo-pipe-folder-dst-internal folder) number))

(require 'product)
(product-provide (provide 'elmo-pipe) (require 'elmo-version))

;;; elmo-pipe.el ends here
