;;; modb.el --- Message Orchestration DataBase.

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

(require 'luna)
(require 'modb-entity)

(eval-and-compile
  (luna-define-class modb-generic () (location	       ; location for save.
				      message-modified ; message is modified.
				      flag-modified    ; flag is modified.
				      mime-charset     ; for encode & decode.
				      ))
  (luna-define-internal-accessors 'modb-generic))

(luna-define-generic elmo-msgdb-load (msgdb)
  "Load the MSGDB.")

(luna-define-generic elmo-msgdb-save (msgdb)
  "Save the MSGDB.")

(luna-define-generic elmo-msgdb-location (msgdb)
  "Return the location of MSGDB.")

(luna-define-generic elmo-msgdb-message-modified-p (msgdb)
  "Return non-nil if message is modified.")

(luna-define-generic elmo-msgdb-flag-modified-p (msgdb)
  "Return non-nil if flag is modified.")

(luna-define-generic elmo-msgdb-append (msgdb msgdb-append)
  "Append the MSGDB-APPEND to the MSGDB.
Return a list of messages which have duplicated message-id.")

(luna-define-generic elmo-msgdb-clear (msgdb)
  "Clear the MSGDB structure.")

(luna-define-generic elmo-msgdb-length (msgdb)
  "Return number of messages in the MSGDB")

(luna-define-generic elmo-msgdb-flag-available-p (msgdb flag)
  "Return non-nil when FLAG is available.")

(luna-define-generic elmo-msgdb-flags (msgdb number)
  "Return a list of flag which corresponds to the message with NUMBER.")

(luna-define-generic elmo-msgdb-set-flag (msgdb number flag)
  "Set message flag.
MSGDB is the ELMO msgdb.
NUMBER is a message number to set flag.
FLAG is a symbol which is one of the following:
  `new'       ... Message which is new.
  `unread'    ... Message which is not read.
  `important' ... Message which is important.
  `answered'  ... Message which is answered.
  `cached'    ... Message which is cached.
'sugar' flag:
  `read'      ... Message which is already read.
  `uncached'  ... Message which is not cached.")

(luna-define-generic elmo-msgdb-unset-flag (msgdb number flag)
  "Unset message flag.
MSGDB is the ELMO msgdb.
NUMBER is a message number to set flag.
FLAG is a symbol which is one of the following:
  `new'       ... Message which is new.
  `unread'    ... Message which is not read.
  `important' ... Message which is important.
  `answered'  ... Message which is answered.
  `cached'    ... Message which is cached.
'sugar' flag:
  `read'      ... Message which is already read.
  `uncached'  ... Message which is not cached.
  `all'       ... Remove all flags.")

(luna-define-generic elmo-msgdb-flag-count (msgdb)
  "Return a list of cons cell as (flag . count).
The count is number of message which is set flag in the MSGDB.")

(luna-define-generic elmo-msgdb-list-messages (msgdb)
  "Return a list of message numbers in the MSGDB.")

(luna-define-generic elmo-msgdb-list-flagged (msgdb flag)
  "Return a list of message numbers which is set FLAG in the MSGDB.")

(luna-define-generic elmo-msgdb-search (msgdb condition &optional numbers)
  "Search and return list of message numbers.
MSGDB is the ELMO msgdb structure.
CONDITION is a condition structure for searching.
If optional argument NUMBERS is specified and is a list of message numbers,
messages are searched from the list.
Return t if the condition is unsupported.")

(luna-define-generic elmo-msgdb-match-condition (msgdb condition number
						       &optional numbers)
  "Check whether the condition of the message is satisfied or not.
MSGDB is the msgdb to search from.
CONDITION is the search condition.
NUMBER is the message number to check.
If optional argument NUMBERS is specified and is a list of message numbers,
messages are searched from the list.
Return CONDITION itself if no entity exists in msgdb.")

(luna-define-generic elmo-msgdb-append-entity (msgdb entity &optional flags)
  "Append a ENTITY with FLAGS into the MSGDB.
Return non-nil if message-id of entity is duplicated.")

(luna-define-generic elmo-msgdb-update-entity (msgdb entity values)
  "Update a ENTITY with VALUES into the MSGDB.
VALUES is an alist of field-name and field-value.")

(luna-define-generic elmo-msgdb-delete-messages (msgdb numbers)
  "Delete messages which are contained NUMBERS from MSGDB.
Return non-nil if messages is deleted successfully.")

(luna-define-generic elmo-msgdb-sort-entities (msgdb predicate
						     &optional app-data)
  "Sort entities of MSGDB, comparing with PREDICATE.
PREDICATE is called with two entities and APP-DATA.
Should return non-nil if the first entity is \"less\" than the second.")

(luna-define-generic elmo-msgdb-message-number (msgdb message-id)
  "Get message number from MSGDB which corresponds to MESSAGE-ID.")

(luna-define-method elmo-msgdb-message-number ((msgdb modb-generic)
					       message-id)
  (elmo-message-entity-number
   (elmo-msgdb-message-entity msgdb message-id)))

(luna-define-generic elmo-msgdb-message-entity (msgdb key)
  "Return the message-entity structure which matches to the KEY.
KEY is a number or a string.
A number is for message number in the MSGDB.
A string is for message-id of the message.")

(luna-define-generic elmo-msgdb-message-field (msgdb number field
						     &optional type)
  "Get message field value in the MSGDB.
NUMBER is a number of the message.
FIELD is a symbol of the field.
If optional argument TYPE is specified, return converted value.")

(luna-define-method elmo-msgdb-message-field ((msgdb modb-generic)
					      number field &optional type)
  (elmo-message-entity-field (elmo-msgdb-message-entity msgdb number)
			     field type))

(luna-define-generic elmo-msgdb-message-entity-handler (msgdb)
  "Get modb entity handler instance which corresponds to the MSGDB.")

;;; generic implement
;;
(luna-define-method elmo-msgdb-load ((msgdb modb-generic))
  t)

(luna-define-method elmo-msgdb-location ((msgdb modb-generic))
  (modb-generic-location-internal msgdb))

(luna-define-method elmo-msgdb-message-modified-p ((msgdb modb-generic))
  (modb-generic-message-modified-internal msgdb))

(luna-define-method elmo-msgdb-flag-modified-p ((msgdb modb-generic))
  (modb-generic-flag-modified-internal msgdb))

(luna-define-method elmo-msgdb-append ((msgdb modb-generic) msgdb-append)
  (let (duplicates)
    (dolist (number (elmo-msgdb-list-messages msgdb-append))
      (when (elmo-msgdb-append-entity
	     msgdb
	     (elmo-msgdb-message-entity msgdb-append number)
	     (elmo-msgdb-flags msgdb-append number))
	(setq duplicates (cons number duplicates))))
    duplicates))

(luna-define-method elmo-msgdb-clear ((msgdb modb-generic))
  (modb-generic-set-message-modified-internal msgdb nil)
  (modb-generic-set-flag-modified-internal msgdb nil))

(luna-define-method elmo-msgdb-length ((msgdb modb-generic))
  0)

(luna-define-method elmo-msgdb-search ((msgdb modb-generic)
				       condition &optional numbers)
  t)

(luna-define-method elmo-msgdb-match-condition ((msgdb modb-generic)
						condition
						number
						&optional numbers)
  (let ((entity (elmo-msgdb-message-entity msgdb number)))
    (if entity
	(elmo-condition-match
	 condition
	 #'elmo-msgdb-match-condition-primitive
	 (list msgdb number entity
	       (or numbers (elmo-msgdb-list-messages msgdb))))
      condition)))

(defun elmo-msgdb-match-condition-primitive (condition msgdb number entity
						       population)
  (let ((key (elmo-filter-key condition))
	(case-fold-search t))
    (cond
     ((string= key "last")
      (<= (length (memq number population))
	  (string-to-number (elmo-filter-value condition))))
     ((string= key "first")
      (< (- (length population)
	    (length (memq number population)))
	 (string-to-number (elmo-filter-value condition))))
     ((string= key "flag")
      (let ((flags (elmo-msgdb-flags msgdb number)))
	(cond ((string= (elmo-filter-value condition) "any")
	       (and flags (not (equal flags '(cached)))))
	      ((string= (elmo-filter-value condition) "digest")
	       (catch 'found
		 (dolist (flag flags)
		   (when (or (memq flag elmo-digest-flags)
			     (elmo-global-flag-p flag))
		     (throw 'found t)))))
	      ((string= (elmo-filter-value condition) "read")
	       (not (memq 'read flags)))
	      (t
	       (memq (intern (elmo-filter-value condition)) flags)))))
     (t
      (elmo-msgdb-message-match-condition (elmo-message-entity-handler entity)
					  condition entity)))))

(luna-define-method elmo-msgdb-update-entity ((msgdb modb-generic)
					      entity values)
  (when (elmo-msgdb-message-entity-update-fields
	 (elmo-message-entity-handler entity)
	 entity values)
    (modb-generic-set-message-modified-internal msgdb t)
    t))

(luna-define-method elmo-msgdb-message-entity-handler ((msgdb modb-generic))
  (or modb-entity-default-cache-internal
      (setq modb-entity-default-cache-internal
	    (luna-make-entity modb-entity-default-handler))))

;; for on demand loading
(provide 'modb-generic)

(require 'product)
(product-provide (provide 'modb) (require 'elmo-version))

;;; modb.el ends here
