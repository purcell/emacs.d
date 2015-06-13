;;; elmo-map.el --- A ELMO folder class with message number mapping.

;; Copyright (C) 2000 Yuuichi Teranishi <teranisi@gohome.org>

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
;; Folders which do not have unique message numbers but unique message names
;; should inherit this folder.

;;; Code:
;;
(require 'elmo)
(require 'elmo-msgdb)

(eval-when-compile (require 'cl))

(eval-and-compile
  (luna-define-class elmo-location-map ()
		     (location-alist location-hash max-number)))

(defmacro elmo-location-map-alist (entity)
  `(luna-slot-value ,entity 'location-alist))

(defmacro elmo-location-map-set-alist (entity value)
  `(luna-set-slot-value ,entity 'location-alist ,value))

(defmacro elmo-location-map-hash (entity)
  `(luna-slot-value ,entity 'location-hash))

(defmacro elmo-location-map-set-hash (entity value)
  `(luna-set-slot-value ,entity 'location-hash ,value))

(defmacro elmo-location-map-max-number (entity)
  `(luna-slot-value ,entity 'max-number))

(defmacro elmo-location-map-set-max-number (entity value)
  `(luna-set-slot-value ,entity 'max-number ,value))


(defmacro elmo-location-map-key (number)
  `(concat "#" (number-to-string ,number)))

(defun elmo-location-map-load (location-map directory)
  (elmo-location-map-setup
   location-map
   (elmo-msgdb-location-load directory)))

(defun elmo-location-map-save (location-map directory)
  (let ((alist (elmo-location-map-alist location-map)))
    (elmo-msgdb-location-save
     directory
     (cons (cons (elmo-location-map-max-number location-map) nil)
	   alist))))

(defun elmo-location-map-setup (location-map &optional locations)
  "Setup internal data of LOCATION-MAP by LOCATIONS.
Return a location alist."
  (let ((hash (elmo-make-hash (length locations)))
	(max-number 0))
    ;; Set number-max and hashtables.
    (dolist (pair locations)
      (setq max-number (max max-number (car pair)))
      (when (cdr pair)
	(elmo-set-hash-val (cdr pair) pair hash)
	(elmo-set-hash-val (elmo-location-map-key (car pair)) pair hash)))
    (let ((inhibit-quit t))
      (elmo-location-map-set-max-number location-map max-number)
      (elmo-location-map-set-hash location-map hash)
      (elmo-location-map-set-alist location-map locations))))

(defun elmo-location-map-teardown (location-map)
  (elmo-location-map-set-alist location-map nil)
  (elmo-location-map-set-hash location-map nil))

(defun elmo-location-map-update (location-map locations)
  "Update location alist in LOCATION-MAP by LOCATIONS.
Return new location alist."
  (let ((old-hash (elmo-location-map-hash location-map))
	(new-hash (elmo-make-hash (length locations)))
	(number (elmo-location-map-max-number location-map))
	new-alist)
    (setq new-alist
	  (mapcar
	   (lambda (location)
	     (let ((entry (or (elmo-get-hash-val location old-hash)
			      (cons (setq number (1+ number)) location))))
	       (elmo-set-hash-val (elmo-location-map-key (car entry))
				  entry
				  new-hash)
	       (elmo-set-hash-val location entry new-hash)
	       entry))
	   locations))
    (let ((inhibit-quit t))
      (elmo-location-map-set-max-number location-map number)
      (elmo-location-map-set-hash location-map new-hash)
      (elmo-location-map-set-alist location-map new-alist))))

(defun elmo-location-map-remove-numbers (location-map numbers)
  (let ((alist (elmo-location-map-alist location-map))
	(hash (elmo-location-map-hash location-map)))
    (dolist (number numbers)
      (let* ((key (elmo-location-map-key number))
	     (entry (elmo-get-hash-val key hash))
	     (inhibit-quit t))
	(elmo-location-map-set-alist
	 location-map
	 (setq alist (delq entry alist)))
	(elmo-clear-hash-val key hash)
	(elmo-clear-hash-val (cdr entry) hash)))))

(defun elmo-map-message-number (location-map location)
  "Return number of the message in the MAPPER with LOCATION."
  (car (elmo-get-hash-val
	location
	(elmo-location-map-hash location-map))))

(defun elmo-map-message-location (location-map number)
  "Return location of the message in the MAPPER with NUMBER."
  (cdr (elmo-get-hash-val
	(elmo-location-map-key number)
	(elmo-location-map-hash location-map))))

(defun elmo-map-numbers-to-locations (location-map numbers)
  (let (locations pair)
    (dolist (number numbers)
      (if (setq pair (elmo-get-hash-val
		      (elmo-location-map-key number)
		      (elmo-location-map-hash location-map)))
	  (setq locations (cons (cdr pair) locations))))
    (nreverse locations)))

(defun elmo-map-locations-to-numbers (location-map locations)
  (let (numbers pair)
    (dolist (location locations)
      (if (setq pair (elmo-get-hash-val
		      location
		      (elmo-location-map-hash location-map)))
	  (setq numbers (cons (car pair) numbers))))
    (nreverse numbers)))


(eval-and-compile
  (luna-define-class elmo-map-folder (elmo-folder elmo-location-map))
  (luna-define-internal-accessors 'elmo-map-folder))

(luna-define-generic elmo-map-folder-list-message-locations (folder)
  "Return a location list of the FOLDER.")

(luna-define-generic elmo-map-folder-set-flag (folder locations flag)
  "Set FLAG to LOCATIONS.")

(luna-define-generic elmo-map-folder-unset-flag (folder locations flag)
  "Unset FLAG from LOCATIONS.")

(luna-define-generic elmo-map-message-fetch (folder location
						    strategy
						    &optional
						    section
						    unseen)
  "")

(luna-define-generic elmo-map-folder-delete-messages (folder locations)
  "")

(luna-define-method elmo-folder-status ((folder elmo-map-folder))
  (elmo-folder-open-internal folder)
  (elmo-folder-set-killed-list-internal
   folder
   (elmo-msgdb-killed-list-load (elmo-folder-msgdb-path folder)))
  (let ((numbers (mapcar
		  'car
		  (elmo-location-map-alist folder))))
    (setq numbers (elmo-living-messages
		   numbers
		   (elmo-folder-killed-list-internal folder)))
    (prog1
	(cons (elmo-max-of-list numbers)
	      (length numbers))
      ;; Don't close after status.
      (unless (elmo-folder-reserve-status-p folder)
	(elmo-folder-close-internal folder)))))

(luna-define-method elmo-folder-pack-numbers ((folder elmo-map-folder))
  (let* ((msgdb (elmo-folder-msgdb folder))
	 (numbers
	  (sort (elmo-folder-list-messages folder nil
					   (not elmo-pack-number-check-strict))
		'<))
	 (new-msgdb (elmo-make-msgdb (elmo-folder-msgdb-path folder)))
	 (number 1)
	 location entity)
    (elmo-with-progress-display (elmo-folder-pack-numbers (length numbers))
	"Packing"
      (dolist (old-number numbers)
	(setq entity (elmo-msgdb-message-entity msgdb old-number))
	(elmo-message-entity-set-number entity number)
	(elmo-msgdb-append-entity new-msgdb entity
				  (elmo-msgdb-flags msgdb old-number))
	(setq location
	      (cons (cons number
			  (elmo-map-message-location folder old-number))
		    location))
	(elmo-emit-signal 'message-number-changed folder old-number number)
	(setq number (1+ number))))
    (message "Packing...done")
    (elmo-location-map-setup folder (nreverse location))
    (elmo-folder-set-msgdb-internal folder new-msgdb)))

(luna-define-method elmo-folder-open-internal ((folder elmo-map-folder))
  (unless (elmo-location-map-alist folder)
    (elmo-location-map-load folder (elmo-folder-msgdb-path folder)))
  (when (elmo-folder-plugged-p folder)
    (elmo-location-map-update
     folder
     (elmo-map-folder-list-message-locations folder))))

(luna-define-method elmo-folder-commit :after ((folder elmo-map-folder))
  (when (elmo-folder-persistent-p folder)
    (elmo-location-map-save folder (elmo-folder-msgdb-path folder))))

(luna-define-method elmo-folder-close-internal ((folder elmo-map-folder))
  (elmo-location-map-teardown folder))

(luna-define-method elmo-folder-check ((folder elmo-map-folder))
  (elmo-location-map-update
   folder
   (elmo-map-folder-list-message-locations folder)))

(luna-define-method elmo-folder-next-message-number ((folder elmo-map-folder))
  (1+ (elmo-location-map-max-number folder)))

(luna-define-method elmo-folder-clear :around ((folder elmo-map-folder)
					       &optional keep-killed)
  (unless keep-killed
    (elmo-location-map-setup folder))
  (luna-call-next-method))

(luna-define-method elmo-folder-list-messages-internal
  ((folder elmo-map-folder) &optional nohide)
  (mapcar 'car (elmo-location-map-alist folder)))

(luna-define-method elmo-folder-set-flag :before ((folder elmo-map-folder)
						  numbers
						  flag
						  &optional is-local)
  (unless is-local
    (elmo-map-folder-set-flag
     folder
     (elmo-map-numbers-to-locations folder numbers)
     flag)))

(luna-define-method elmo-folder-unset-flag :before ((folder elmo-map-folder)
						    numbers
						    flag
						    &optional is-local)
  (unless is-local
    (elmo-map-folder-unset-flag
     folder
     (elmo-map-numbers-to-locations folder numbers)
     flag)))

(luna-define-method elmo-message-fetch-internal ((folder elmo-map-folder)
						 number strategy
						 &optional section unread)
  (elmo-map-message-fetch
   folder
   (elmo-map-message-location folder number)
   strategy section unread))

(luna-define-method elmo-folder-list-flagged-internal ((folder elmo-map-folder)
						       flag)
  (let ((locations (elmo-map-folder-list-flagged folder flag)))
    (if (listp locations)
	(elmo-map-locations-to-numbers folder locations)
      t)))

(luna-define-generic elmo-map-folder-list-flagged (folder flag)
  "Return a list of message location in the FOLDER with FLAG.
Return t if the message list is not available.")

(luna-define-method elmo-map-folder-list-flagged ((folder elmo-map-folder)
						  flag)
  t)

(luna-define-method elmo-folder-delete-messages-internal ((folder
							   elmo-map-folder)
							  numbers)
  (elmo-map-folder-delete-messages
   folder
   (elmo-map-numbers-to-locations folder numbers)))

(luna-define-method elmo-folder-detach-messages :around ((folder
							  elmo-map-folder)
							 numbers)
  (when (luna-call-next-method)
    (elmo-location-map-remove-numbers folder numbers)
    t)) ; success

(require 'product)
(product-provide (provide 'elmo-map) (require 'elmo-version))

;;; elmo-map.el ends here
