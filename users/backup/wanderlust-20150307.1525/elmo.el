;;; elmo.el --- Elisp Library for Message Orchestration.

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

(require 'luna)

(require 'elmo-version)			; reduce recursive-load-depth
(require 'elmo-vars)
(require 'elmo-util)
(require 'elmo-msgdb)
(require 'elmo-signal)

(eval-when-compile (require 'cl))

(if (or (featurep 'dbm)
	(featurep 'gnudbm)
	(featurep 'berkdb)
	(featurep 'berkeley-db))
    (require 'elmo-database))

(defcustom elmo-message-fetch-threshold 30000
  "Fetch threshold."
  :type '(choice (integer :tag "Threshold (bytes)")
		 (const :tag "No limitation" nil))
  :group 'elmo)

(defcustom elmo-message-fetch-confirm t
  "If non-nil, confirm fetching if message size is larger than
`elmo-message-fetch-threshold'.
Otherwise, entire fetching of the message is aborted without confirmation."
  :type 'boolean
  :group 'elmo)

(defcustom elmo-folder-update-threshold 500
  "Update threshold."
  :type '(choice (integer :tag "Number of messages")
		 (const :tag "No limitation" nil))
  :group 'elmo)

(defcustom elmo-folder-update-confirm t
  "Confirm if update number exceeds `elmo-folder-update-threshold'."
  :type 'boolean
  :group 'elmo)

(defcustom elmo-msgdb-path-encode-threshold nil
  "*Encode msgdb path if its length is longer than this value."
  :type '(choice (const :tag "No encode" nil)
		 number)
  :group 'elmo)

(defvar elmo-message-displaying nil
  "A global switch to indicate message is displaying or not.")

;;; internal
(defvar elmo-folder-type-alist nil)

(defvar elmo-newsgroups-hashtb nil)

(elmo-define-error 'elmo-error "Error" 'error)
(elmo-define-error 'elmo-open-error "Cannot open" 'elmo-error)
(elmo-define-error 'elmo-authenticate-error "Login failed" 'elmo-open-error)
(elmo-define-error 'elmo-imap4-bye-error "IMAP4 session was terminated" 'elmo-open-error)

;; Event declarations
(elmo-define-signal flag-changing (number old-flags new-flags)
  "Notify the changing flag of the messages with NUMBER.")

(elmo-define-signal flag-changed (numbers)
  "Notify the change flag of the messages with NUMBERS.")

(elmo-define-signal status-changed (numbers)
  "Notify the change status of the message with NUMBERS.")

(elmo-define-signal update-overview (number)
  "Notify update overview of the message with NUMBER.")

(elmo-define-signal message-number-changed (old-number new-number)
  "Notify change of message number within the folder.")

;; autoloads
(eval-and-compile
  (autoload 'md5 "md5")
  (autoload 'elmo-dop-queue-flush "elmo-dop")
  (autoload 'elmo-nntp-post "elmo-nntp")
  (autoload 'elmo-global-flag-p "elmo-flag")
  (autoload 'elmo-local-flag-p "elmo-flag")
  (autoload 'elmo-global-flag-detach "elmo-flag")
  (autoload 'elmo-global-flag-detach-messages "elmo-flag")
  (autoload 'elmo-global-flag-set "elmo-flag")
  (autoload 'elmo-global-flag-replace-referrer "elmo-flag")
  (autoload 'elmo-get-global-flags "elmo-flag")
  (autoload 'elmo-global-flags-initialize "elmo-flag")
  (autoload 'elmo-global-mark-migrate "elmo-flag")
  (autoload 'elmo-folder-list-global-flag-messages "elmo-flag")
  (autoload 'elmo-search-register-engine "elmo-search"))

(defun elmo-define-folder (prefix backend)
  "Define a folder.
If a folder name begins with PREFIX, use BACKEND."
  (let ((pair (assq prefix elmo-folder-type-alist)))
    (if pair
	(progn
	  (setcar pair prefix)
	  (setcdr pair backend))
      (setq elmo-folder-type-alist (cons (cons prefix backend)
					 elmo-folder-type-alist)))))

(defmacro elmo-folder-type (name)
  "Get folder type from NAME string."
  `(and (stringp ,name)
	(cdr (assoc (string-to-char ,name) elmo-folder-type-alist))))

;;; ELMO folder
;; A elmo folder provides uniformed (orchestrated) access
;; to the internet messages.
(eval-and-compile
  (luna-define-class elmo-folder () (type   ; folder type symbol.
				     name   ; orignal folder name string.
				     prefix ; prefix for folder name
				     path   ; directory path for msgdb.
				     msgdb  ; msgdb (may be nil).
				     killed-list  ; killed list.
				     flag-table   ; flag table.
				     persistent   ; non-nil if persistent.
				     process-duplicates  ; read or hide
				     biff   ; folder for biff
				     mime-charset ; charset for encode & decode
				     ))
  (luna-define-internal-accessors 'elmo-folder))

(luna-define-generic elmo-folder-initialize (folder name)
  ;; Initialize a FOLDER structure with NAME."
  )

(defmacro elmo-folder-send (folder message &rest args)
  "Let FOLDER receive the MESSAGE with ARGS."
  `(luna-send ,folder ,message ,folder ,@args))

;;;###autoload
(defun elmo-make-folder (name &optional non-persistent mime-charset)
  "Make an ELMO folder structure specified by NAME.
If optional argument NON-PERSISTENT is non-nil, the folder msgdb is not saved.
If optional argument MIME-CHARSET is specified, it is used for
encode and decode a multibyte string."
  (let ((type (elmo-folder-type name))
	prefix split class folder original)
    (setq original (elmo-string name))
    (if type
	(setq prefix (elmo-string name 0 1)
	      name (elmo-string name 1))
      (setq type (intern (car (setq split (split-string name ":")))))
      (if (>= (length split) 2)
	  (setq name (substring name (+ 1 (length (car split)))))
	(error "Error in folder name `%s'" original))
      (setq prefix (concat (car split) ":")))
    (setq class (format "elmo-%s" (symbol-name type)))
    (require (intern class))
    (setq folder (luna-make-entity (intern (concat class "-folder"))
				   :type type
				   :prefix prefix
				   :name original
				   :persistent (not non-persistent)
				   :mime-charset mime-charset))
    (save-match-data
      (elmo-folder-send folder 'elmo-folder-initialize name))))

(defvar elmo-get-folder-function nil)

(defun elmo-get-folder (name)
  (or (and elmo-get-folder-function
	   (funcall elmo-get-folder-function name))
      (elmo-make-folder name)))

;; Note that this function is for internal use only.
(luna-define-generic elmo-folder-msgdb (folder)
  "Return the msgdb of FOLDER (on-demand loading).
\(For internal use only.\)")

(luna-define-method elmo-folder-msgdb ((folder elmo-folder))
  (or (elmo-folder-msgdb-internal folder)
      (elmo-folder-set-msgdb-internal folder
				      (elmo-folder-msgdb-load folder))))

(luna-define-generic elmo-folder-open (folder &optional load-msgdb)
  "Open and setup (load saved status) FOLDER.
If optional LOAD-MSGDB is non-nil, msgdb is loaded.
\(otherwise, msgdb is loaded on-demand)")

(luna-define-generic elmo-folder-open-internal (folder)
  "Open FOLDER (without loading saved folder status).")

(luna-define-generic elmo-folder-check (folder)
  "Check the FOLDER to obtain newest information at the next list operation.")

(luna-define-generic elmo-folder-clear (folder &optional keep-killed)
  "Clear FOLDER to the initial state.
If optional KEEP-KILLED is non-nil, killed-list is not cleared.")

(luna-define-generic elmo-folder-commit (folder)
  "Save current status of FOLDER.")

(luna-define-generic elmo-folder-close (folder)
  "Close, save and clearnup FOLDER.")

(luna-define-generic elmo-folder-close-internal (folder)
  "Close FOLDER (without saving folder status).")

(luna-define-generic elmo-folder-plugged-p (folder)
  "Returns t if FOLDER is plugged.")

(luna-define-generic elmo-folder-set-plugged (folder plugged &optional add)
  "Set FOLDER as plugged.")

(luna-define-generic elmo-net-port-info (folder)
  "Get port information of FOLDER.")

(luna-define-generic elmo-folder-use-flag-p (folder)
  "Returns t if FOLDER treats unread/important flag itself.")

(luna-define-generic elmo-folder-diff (folder)
  "Get diff of FOLDER.
Return value is cons cell or list:
 - a cons cell (new . all)
 - a list (new unread all)")

(luna-define-generic elmo-folder-status (folder)
  "Returns a cons cell of (MAX-NUMBER . MESSAGES) in the FOLDER.")

(luna-define-generic elmo-folder-reserve-status-p (folder)
  "If non-nil, the folder should not close folder after `elmo-folder-status'.")

(luna-define-generic elmo-folder-list-messages (folder &optional visible-only
						       in-msgdb)
  "Return a list of message numbers contained in FOLDER.
If optional VISIBLE-ONLY is non-nil, killed messages are not listed.
If second optional IN-MSGDB is non-nil, only messages in the msgdb are listed.")

(luna-define-method elmo-folder-list-messages ((folder elmo-folder)
					       &optional visible-only in-msgdb)
  (let ((list (if in-msgdb
		  t
		(elmo-folder-list-messages-internal folder visible-only)))
	(killed-list (elmo-folder-killed-list-internal folder)))
    (unless (listp list)
      ;; Use current list.
      (setq list (elmo-msgdb-list-messages (elmo-folder-msgdb folder))))
    (if visible-only
	(elmo-living-messages list killed-list)
      (if (and in-msgdb killed-list)
	  (elmo-sort-uniq-number-list
	   (nconc (elmo-number-set-to-number-list killed-list) list))
	list))))

(luna-define-generic elmo-folder-list-messages-internal (folder &optional
								visible-only)
  ;; Return a list of message numbers contained in FOLDER.
  ;; Return t if the message list is not available.
  )

(luna-define-generic elmo-folder-list-flagged (folder flag &optional in-msgdb)
  "List messages in the FOLDER with FLAG.
FLAG is a symbol which is one of the following:
  `new'        (new messages)
  `unread'     (unread messages (new messages are included))
  `answered'   (answered or forwarded)
  `important'  (marked as important)
'sugar' flags:
  `read'       (not unread)
  `digest'     (unread + important + other flags)
  `any'        (digest + answered + other flags)
If optional IN-MSGDB is non-nil, retrieve flag information from msgdb.")

(luna-define-method elmo-folder-list-flagged ((folder elmo-folder) flag
					      &optional in-msgdb)
  (if in-msgdb
      (elmo-msgdb-list-flagged (elmo-folder-msgdb folder) flag)
    (let ((msgs (elmo-folder-list-flagged-internal folder flag)))
      (unless (listp msgs)
	(setq msgs (elmo-msgdb-list-flagged (elmo-folder-msgdb folder) flag)))
      (elmo-folder-merge-flagged
       folder (elmo-folder-list-global-flag-messages folder flag) msgs))))

(luna-define-generic elmo-folder-list-flagged-internal (folder flag)
  "Return a list of message in the FOLDER with FLAG.
Return t if the message list is not available.")

(luna-define-method elmo-folder-list-flagged-internal ((folder elmo-folder)
						       flag)
  t)

(luna-define-generic elmo-folder-merge-flagged (folder local remote)
  "Merge messages of flag folder and messages of remote folder.
LOCAL is the list of messages from flag folder.
REMOTE is the list of messages from remote folder.")

(luna-define-method elmo-folder-merge-flagged ((folder elmo-folder) local remote)
  (elmo-sort-uniq-number-list (nconc local remote)))

(luna-define-generic elmo-folder-list-subfolders (folder &optional one-level)
  "Returns a list of subfolders contained in FOLDER.
If optional argument ONE-LEVEL is non-nil, only children of FOLDER is returned.
\(a folder which have children is returned as a list\)
Otherwise, all descendent folders are returned.")

(luna-define-generic elmo-folder-have-subfolder-p (folder)
  "Return non-nil when FOLDER has subfolders.")

(luna-define-generic elmo-folder-exists-p (folder)
  "Returns non-nil when FOLDER exists.")

(luna-define-generic elmo-folder-creatable-p (folder)
  "Returns non-nil when FOLDER is creatable.")

(luna-define-generic elmo-folder-writable-p (folder)
  "Returns non-nil when FOLDER is writable.")

(luna-define-generic elmo-folder-persistent-p (folder)
  "Return non-nil when FOLDER is persistent.")

(luna-define-generic elmo-folder-create (folder)
  "Create a FOLDER.")

(luna-define-generic elmo-message-deletable-p (folder number)
  "Returns non-nil when the message in the FOLDER with NUMBER is deletable.")

(luna-define-generic elmo-folder-delete (folder)
  "Delete FOLDER completely.")

(luna-define-generic elmo-folder-rename (folder new-name)
  "Rename FOLDER to NEW-NAME (string).")

(luna-define-generic elmo-folder-delete-messages (folder numbers)
  "Delete messages with msgdb entity.
FOLDER is the ELMO folder structure.
NUMBERS is a list of message numbers to be deleted.
It is not recommended to use this function other than internal use.
Use `elmo-folder-move-messages' with dst-folder 'null instead.")

(luna-define-generic elmo-folder-delete-messages-internal (folder numbers)
  "Delete messages, but no delete msgdb entity.
FOLDER is the ELMO folder structure.
NUMBERS is a list of message numbers to be deleted.
Override this method by each implement of `elmo-folder'.")

(luna-define-generic elmo-folder-search (folder condition &optional numbers)
  "Search and return list of message numbers.
FOLDER is the ELMO folder structure.
CONDITION is a condition structure for searching.
If optional argument NUMBERS is specified and is a list of message numbers,
messages are searched from the list.
If NUMBERS is t, indicates that messages are selected for interactive folder search.")

(luna-define-generic elmo-message-match-condition (folder number
							  condition
							  numbers)
  "Return non-nil when the message in the FOLDER with NUMBER is matched.
CONDITION is a condition structure for testing.
NUMBERS is a list of message numbers,
use to be test for \"last\" and \"first\" predicates.")

(luna-define-generic elmo-folder-msgdb-create (folder numbers flag-table)
  "Create a message database (implemented in each backends).
FOLDER is the ELMO folder structure.
NUMBERS is a list of message numbers to create msgdb.
FLAG-TABLE is a hashtable of message-id and flag.")

(luna-define-generic elmo-folder-set-flag (folder numbers flag
						  &optional is-local)
  "Set messages flag.
FOLDER is a ELMO folder structure.
NUMBERS is a list of message number to set flag.

FLAG is a symbol which is one of the following:
  `unread'    (set the message as unread)
  `answered'  (set the message as answered)
  `important' (set the message as important)
'sugar' flag:
  `read'      (remove new and unread flags)
If optional IS-LOCAL is non-nil, update only local (not server) status.")

(luna-define-generic elmo-folder-unset-flag (folder numbers flag
						    &optional is-local)
  "Unset messages flag.
FOLDER is a ELMO folder structure.
NUMBERS is a list of message number to unset flag.

FLAG is a symbol which is one of the following:
  `unread'    (remove unread and new flag)
  `answered'  (remove answered flag)
  `important' (remove important flag)
'sugar' flag:
  `read'      (set unread flag)
  `all'       (remove all flags)
If optional IS-LOCAL is non-nil, update only local (not server) status.")

(luna-define-generic elmo-message-flag-available-p (folder number flag)
  "Return non-nil when a message in the FOLDER with NUMBER treats FLAG.")

(luna-define-generic elmo-folder-next-message-number (folder)
  "The next message number that will be assigned to a new message.
FOLDER is the ELMO folder structure.")

(luna-define-generic elmo-folder-append-buffer (folder &optional flags
						       number return-number)
  "Append current buffer as a new message.
FOLDER is the destination folder (ELMO folder structure).
FLAGS is the flag list for the appended message (list of symbols).
If FLAGS contain `read', the message is appended as `not-unread'.
If it is nil, the appended message will be treated as `new'.
If optional argument NUMBER is specified, the new message number is set
\(if possible\).
If optional argument RETURN-NUMBER is non-nil, return the number
of the appended message if possible. If the number could not be
obtained return t.
Return nil on failure.")

(luna-define-generic elmo-folder-pack-numbers (folder)
  "Pack message numbers of FOLDER.")

(luna-define-generic elmo-folder-update-number (folder)
  "Update number of FOLDER.")

(luna-define-generic elmo-folder-diff-async (folder)
  "Get diff of FOLDER asynchronously.")

(luna-define-generic elmo-folder-expand-msgdb-path (folder)
  "Expand path for FOLDER.")

(luna-define-generic elmo-folder-get-primitive-list (folder)
  "Get primitive folder structure list contained in FOLDER.")

(luna-define-generic elmo-folder-contains-type (folder type)
  "Returns t if FOLDER contains TYPE.")

(luna-define-generic elmo-folder-local-p (folder)
  "Returns t if FOLDER is local.")

(luna-define-generic elmo-folder-message-file-p (folder)
  "Returns t if all messages in the FOLDER are files.")

;;; Message methods.
(luna-define-generic elmo-message-use-cache-p (folder number)
  "Returns t if the message in the FOLDER with NUMBER uses cache.")

(luna-define-generic elmo-message-file-name (folder number)
  "Return the file name of a message specified by FOLDER and NUMBER.")

;;; For archive

;;; Use original file
(luna-define-generic elmo-folder-message-file-number-p (folder)
  "Return t if the file name in the FOLDER is the message number.")

(luna-define-generic elmo-folder-message-file-directory (folder)
  "Return the directory of the message files of FOLDER.")

;;; Use temporary file
(luna-define-generic elmo-folder-message-make-temp-file-p (folder)
  "Return t if the messages in the FOLDER makes local temporary file.")

(luna-define-generic elmo-folder-message-make-temp-files (folder
							  numbers
							  &optional
							  start-number)
  "Make a new temporary files from the messages in the FOLDER with NUMBERS.
If START-NUMBER is specified, temporary files begin from the number.
Otherwise, same number is used for temporary files.
Return newly created temporary directory name which contains temporary files.")

(luna-define-generic elmo-message-file-p (folder number)
  "Return t if message in the FOLDER with NUMBER is a file.")

(luna-define-generic elmo-message-flags (folder number)
  "Return a list of flags.
FOLDER is a ELMO folder structure.
NUMBER is a number of the message.")

(defun elmo-message-flags-for-append (folder number &optional message-id)
  "Return a list of flags for `elmo-folder-append-buffer'.
FOLDER is a ELMO folder structure.
NUMBER is a number of the message.
If optional argument MESSAGES-ID is not specified, get it from current buffer."
  (let ((this-id (elmo-message-field folder number 'message-id)))
    (and this-id
	 (string= this-id (or message-id
			      (elmo-msgdb-get-message-id-from-buffer)))
	 (or (elmo-message-flags folder number)
	     ;; message exists, but no flag.
	     '(read)))))

(luna-define-method elmo-message-flag-available-p ((folder elmo-folder) number
						   flag)
  (elmo-msgdb-flag-available-p (elmo-folder-msgdb folder) flag))

(luna-define-method elmo-message-flags ((folder elmo-folder) number)
  (elmo-msgdb-flags (elmo-folder-msgdb folder) number))

(defsubst elmo-message-flagged-p (folder number flag)
  "Return non-nil if the message is set FLAG.
FOLDER is a ELMO folder structure.
NUMBER is a message number to test."
  (let ((cur-flags (elmo-message-flags folder number)))
    (case flag
      (read
       (not (memq 'unread cur-flags)))
      (t
       (memq flag cur-flags)))))

(luna-define-generic elmo-find-fetch-strategy (folder number
						      &optional
						      ignore-cache
						      require-entireness)
  "Return the message fetching strategy suitable for the message with NUMBER.
FOLDER is the ELMO folder structure.
If optional argument IGNORE-CACHE is non-nil, existing cache is ignored.
If second optional argument REQUIRE-ENTIRENESS is non-nil,
ensure that entireness of the returned strategy is entire.
Returned value is a elmo-fetch-strategy object.
If return value is nil, message should not be nil.")

(defmacro elmo-make-fetch-strategy (entireness
				    &optional
				    use-cache
				    save-cache
				    cache-path)
  "Make elmo-message-fetching strategy.
ENTIRENESS is 'entire or 'section.
'entire means fetch message entirely at once.
'section means fetch message section by section.
If optional USE-CACHE is non-nil, existing cache is used and otherwise,
existing cache is thrown away.
If SAVE-CACHE is non-nil, fetched message is saved.
CACHE-PATH is the cache path to be used as a message cache file."
  `(vector ,entireness ,use-cache ,save-cache ,cache-path))

(defmacro elmo-fetch-strategy-entireness (strategy)
  "Return entireness of STRATEGY."
  `(aref ,strategy 0))

(defmacro elmo-fetch-strategy-use-cache (strategy)
  "Return use-cache of STRATEGY."
  `(aref ,strategy 1))

(defmacro elmo-fetch-strategy-save-cache (strategy)
  "Return save-cache of STRATEGY."
  `(aref ,strategy 2))

(defmacro elmo-fetch-strategy-cache-path (strategy)
  "Return cache-path of STRATEGY."
  `(aref ,strategy 3))

(luna-define-method elmo-find-fetch-strategy ((folder elmo-folder) number
					      &optional
					      ignore-cache
					      require-entireness)
  (let ((entity (elmo-message-entity folder number)))
    (if (null entity)
	(elmo-make-fetch-strategy 'entire)
      (let* ((size (elmo-message-entity-field entity 'size))
	     (message-id (elmo-message-entity-field entity 'message-id))
	     (cache-file (elmo-file-cache-get message-id))
	     (use-cache (elmo-message-use-cache-p folder number)))
	(if (and (not ignore-cache)
		 use-cache
		 (eq (elmo-file-cache-status cache-file) 'entire))
	    ;; Cache exists and use it.
	    (elmo-make-fetch-strategy
	     'entire
	     t				; Use cache.
	     use-cache			; Save cache.
	     (elmo-file-cache-path cache-file))
	  ;; No cache or ignore-cache.
	  (if (and (not (elmo-folder-local-p folder))
		   (not require-entireness)
		   elmo-message-fetch-threshold
		   (integerp size)
		   (>= size elmo-message-fetch-threshold)
		   (or (not elmo-message-fetch-confirm)
		       (not (prog1
				(y-or-n-p
				 (format "Fetch entire message(%dbytes)? "
					 size))
			      (message "")))))
	      ;; Don't fetch message at all.
	      nil
	    ;; Don't use existing cache and fetch entire message at once.
	    (elmo-make-fetch-strategy
	     'entire
	     nil			; Don't use cache.
	     use-cache			; Save cache.
	     (elmo-file-cache-path cache-file))))))))

(luna-define-method elmo-folder-list-messages-internal
  ((folder elmo-folder) &optional visible-only)
  t)

(luna-define-generic elmo-message-encache (folder number &optional read)
  "Encache message in the FOLDER with NUMBER.
If READ is non-nil, message is flaged as read.")

(luna-define-method elmo-message-encache ((folder elmo-folder) number
					  &optional read)
  (let (path)
    (with-temp-buffer
      (elmo-message-fetch
       folder number
       (elmo-make-fetch-strategy 'entire
				 nil ;use-cache
				 t   ;save-cache
				 (setq path (elmo-file-cache-get-path
					     (elmo-message-field
					      folder number 'message-id))))
       (not read)))
    path))

(luna-define-generic elmo-message-fetch-bodystructure (folder number strategy)
  "Fetch bodystructure of the message in FOLDER with NUMBER using STRATEGY.")

(luna-define-generic elmo-message-fetch (folder number strategy
						&optional
						unread
						section)
  "Fetch a message into current buffer.
FOLDER is the ELMO folder structure.
NUMBER is the number of the message in the FOLDER.
STRATEGY is the message fetching strategy.
If optional argument UNREAD is non-nil, message is not flaged as read.
If second optional argument SECTION is specified, only the
SECTION of the message is fetched (if possible).
Returns non-nil if fetching was succeed.")

(luna-define-generic elmo-message-fetch-internal (folder number strategy
							 &optional
							 section
							 unread)
  "Fetch a message into current buffer.
FOLDER is the ELMO folder structure.
NUMBER is the number of the message in the FOLDER.
STRATEGY is the message fetching strategy.
If optional argument SECTION is specified, only the SECTION of the message
is fetched (if possible).
If second optional argument UNREAD is non-nil, message is not flaged as read.
Returns non-nil if fetching was succeed.")

(luna-define-generic elmo-message-fetch-field (folder number field)
  "Fetch a message field value.
FOLDER is the ELMO folder structure.
NUMBER is the number of the message in the FOLDER.
FIELD is a symbol of the field name.")

(luna-define-generic elmo-message-folder (folder number)
  "Get primitive folder of the message.")

(luna-define-generic elmo-folder-process-crosspost (folder)
  "Process crosspost for FOLDER.
Return a cons cell of (NUMBER-CROSSPOSTS . NEW-FLAG-ALIST).")

(luna-define-generic elmo-folder-newsgroups (folder)
  "Return list of newsgroup name of FOLDER.")

(luna-define-generic elmo-folder-search-requires-msgdb-p (folder condition)
  "Return non-nil if searching in FOLDER by CONDITION requires msgdb fetch.")

(defun elmo-folder-search-requires-msgdb-p-internal (folder condition)
  (if (listp condition)
      (or (elmo-folder-search-requires-msgdb-p-internal
	   folder (nth 1 condition))
	  (elmo-folder-search-requires-msgdb-p-internal
	   folder (nth 2 condition)))
    (and (not (string= (elmo-filter-key condition) "last"))
	 (not (string= (elmo-filter-key condition) "first")))))

(luna-define-method elmo-folder-search-requires-msgdb-p ((folder elmo-folder)
							 condition)
  (elmo-folder-search-requires-msgdb-p-internal folder condition))

(luna-define-method elmo-folder-newsgroups ((folder elmo-folder))
  nil)

(luna-define-method elmo-folder-open ((folder elmo-folder)
				      &optional load-msgdb)
  (elmo-generic-folder-open folder load-msgdb))

(defun elmo-generic-folder-open (folder load-msgdb)
  (let ((inhibit-quit t))
    (if load-msgdb (elmo-folder-msgdb folder))
    (elmo-folder-set-killed-list-internal
     folder
     (elmo-msgdb-killed-list-load (elmo-folder-msgdb-path folder))))
  (elmo-folder-open-internal folder))

(luna-define-method elmo-folder-open-internal ((folder elmo-folder))
  nil ; default is do nothing.
  )

(luna-define-method elmo-folder-check ((folder elmo-folder))
  nil) ; default is noop.

(luna-define-method elmo-folder-commit ((folder elmo-folder))
  (elmo-generic-folder-commit folder))

(defun elmo-generic-folder-commit (folder)
  (when (elmo-folder-persistent-p folder)
    (let ((msgdb (elmo-folder-msgdb-internal folder)))
      (when msgdb
	(when (elmo-msgdb-message-modified-p msgdb)
	  (elmo-folder-set-info-max-by-numdb
	   folder
	   (elmo-folder-list-messages folder nil 'in-msgdb)))
	(elmo-msgdb-save msgdb)))
    (elmo-msgdb-killed-list-save
     (elmo-folder-msgdb-path folder)
     (elmo-folder-killed-list-internal folder))))

(luna-define-method elmo-folder-close-internal ((folder elmo-folder))
  ;; do nothing.
  )

(luna-define-method elmo-folder-close ((folder elmo-folder))
  (elmo-generic-folder-close folder)
  (elmo-folder-close-internal folder))

(defun elmo-generic-folder-close (folder)
  (elmo-folder-commit folder)
  (elmo-folder-set-msgdb-internal folder nil)
  (elmo-folder-set-killed-list-internal folder nil))

(luna-define-method elmo-folder-plugged-p ((folder elmo-folder))
  t) ; default is plugged.

(luna-define-method elmo-folder-set-plugged ((folder elmo-folder) plugged
					     &optional add)
  nil) ; default is do nothing.

(luna-define-method elmo-folder-use-flag-p ((folder elmo-folder))
  nil) ; default is no flag.

(luna-define-method elmo-folder-persistent-p ((folder elmo-folder))
  (elmo-folder-persistent-internal folder))

(luna-define-method elmo-folder-creatable-p ((folder elmo-folder))
  nil) ; default is not creatable.

(luna-define-method elmo-folder-writable-p ((folder elmo-folder))
  nil) ; default is not writable.

(luna-define-method elmo-folder-delete ((folder elmo-folder))
  (when (yes-or-no-p (format "Delete msgdb of \"%s\"? "
			     (elmo-folder-name-internal folder)))
    (elmo-msgdb-delete-path folder)
    t))

(luna-define-method elmo-folder-rename ((folder elmo-folder) new-name)
  (let ((new-folder (elmo-make-folder
		     new-name
		     nil
		     (elmo-folder-mime-charset-internal folder))))
    (unless (eq (elmo-folder-type-internal folder)
		(elmo-folder-type-internal new-folder))
      (error "Not same folder type"))
    (when (or (file-exists-p (elmo-folder-msgdb-path new-folder))
	      (elmo-folder-exists-p new-folder))
      (error "Already exists folder: %s" new-name))
    (elmo-folder-send folder 'elmo-folder-rename-internal new-folder)
    (elmo-global-flag-replace-referrer (elmo-folder-name-internal folder)
				       new-name)
    (elmo-msgdb-rename-path folder new-folder)))

(luna-define-method elmo-folder-delete-messages ((folder elmo-folder)
						 numbers)
  (and (elmo-folder-delete-messages-internal folder numbers)
       (elmo-folder-detach-messages folder numbers)))

(luna-define-method elmo-folder-search ((folder elmo-folder)
					condition
					&optional numbers)
  (let ((msgdb (elmo-folder-msgdb folder))
	results)
    (setq numbers (cond
		   ((null numbers)
		    (elmo-folder-list-messages folder))
		   ((listp numbers)
		    numbers)
		   (t
		    (elmo-folder-list-messages folder 'visible 'in-msgdb)))
	  results (elmo-msgdb-search msgdb condition numbers))
    (if (listp results)
	results
      (elmo-condition-optimize condition)
      (when (and (consp condition)
		 (eq (car condition) 'and)
		 (listp (setq results (elmo-msgdb-search msgdb
							 (nth 1 condition)
							 numbers))))
	(setq numbers results
	      condition (nth 2 condition)))
      (let (matched)
	(elmo-with-progress-display (elmo-folder-search (length numbers))
	    "Searching messages"
	  (dolist (number numbers)
	    (let (result)
	      (setq result (elmo-msgdb-match-condition msgdb
						       condition
						       number
						       numbers))
	      (when (elmo-filter-condition-p result)
		(setq result (elmo-message-match-condition folder
							   number
							   condition
							   numbers)))
	      (when result
		(setq matched (cons number matched))))
	    (elmo-progress-notify 'elmo-folder-search)))
	(nreverse matched)))))

(defun elmo-message-buffer-match-condition (condition number)
  (let* ((handler (luna-make-entity 'modb-buffer-entity-handler))
	 (result (elmo-condition-match
		  condition
		  (lambda (condition handler entity)
		    (elmo-msgdb-message-match-condition handler
							condition
							entity))
		  (list
		   handler
		   (elmo-msgdb-make-message-entity
		    handler
		    :number number
		    :buffer (current-buffer))))))
    (and result (not (elmo-filter-condition-p result)))))

(luna-define-method elmo-message-match-condition ((folder elmo-folder)
						  number condition
						  numbers)
  (let* (cache cache-path
	 (filename (cond
		    ((elmo-message-file-name folder number))
		    ((progn
		       (setq cache (elmo-file-cache-get
				    (elmo-message-field folder number
							'message-id)))
		       (setq cache-path (elmo-file-cache-path cache))
		       (and cache-path
			    (not (elmo-cache-path-section-p cache-path))))
		     cache-path))))
    (when (and filename (file-readable-p filename))
      (with-temp-buffer
	(set-buffer-multibyte nil)
;;;	(insert-file-contents-as-binary filename)
	(elmo-message-fetch folder number
			    (elmo-make-fetch-strategy 'entire
						      (and cache t)
						      nil
						      cache-path)
			    'unread)
	(set-buffer-multibyte default-enable-multibyte-characters)
	(elmo-message-buffer-match-condition condition number)))))

(luna-define-method elmo-folder-pack-numbers ((folder elmo-folder))
  nil) ; default is noop.

(luna-define-method elmo-folder-update-number ((folder elmo-folder))
  nil) ; default is noop.

(luna-define-method elmo-folder-message-file-p ((folder elmo-folder))
  nil) ; default is not file.

(luna-define-method elmo-folder-message-file-number-p ((folder elmo-folder))
  nil) ; default is not number.

(luna-define-method elmo-folder-message-make-temp-file-p ((folder elmo-folder))
  nil) ; default is not make temp file.

(luna-define-method elmo-message-file-name ((folder elmo-folder)
						   number)
  nil) ; default is no name.

(luna-define-method elmo-folder-local-p ((folder elmo-folder))
  t)   ; default is local.

(luna-define-method elmo-folder-have-subfolder-p ((folder elmo-folder))
  t)

;; Flag table
(luna-define-generic elmo-folder-flag-table (folder &optional if-exists)
  "Return the flag-table of FOLDER.
If optional argument IF-EXISTS is nil, load on demand.
\(For internal use only.\)")

(luna-define-generic elmo-folder-close-flag-table (folder)
  "Close flag-table of FOLDER.")

(luna-define-method elmo-folder-flag-table ((folder elmo-folder)
					    &optional if-exists)
  (or (elmo-folder-flag-table-internal folder)
      (unless if-exists
	(elmo-folder-set-flag-table-internal
	 folder
	 (elmo-flag-table-load (elmo-folder-msgdb-path folder))))))

(luna-define-method elmo-folder-close-flag-table ((folder elmo-folder))
  (elmo-flag-table-save (elmo-folder-msgdb-path folder)
			(elmo-folder-flag-table folder))
  (elmo-folder-set-flag-table-internal folder nil))

(defun elmo-folder-preserve-flags (folder msgid flags)
  "Preserve FLAGS into FOLDER for a message that has MSGID."
  (when (and msgid flags)
    (let ((flag-table (elmo-folder-flag-table folder 'if-exists))
	  load-now)
      (when (setq load-now (null flag-table))
	(setq flag-table (elmo-folder-flag-table folder)))
      (elmo-flag-table-set flag-table msgid flags)
      (when load-now
	(elmo-folder-close-flag-table folder)))))

;;; Folder info
;; Folder info is a message number information cache (hashtable)
(defsubst elmo-folder-get-info (folder &optional hashtb)
  "Return FOLDER info from HASHTB (default is `elmo-folder-info-hashtb')."
  (elmo-get-hash-val (elmo-folder-name-internal folder)
		     (or hashtb elmo-folder-info-hashtb)))

(defun elmo-folder-set-info-hashtb (folder max numbers &optional new unread)
  "Set FOLDER info (means MAX, NUMBERS, NEW and UNREAD)."
  (let ((info (elmo-folder-get-info folder)))
    (when info
      (or new     (setq new     (nth 0 info)))
      (or unread  (setq unread  (nth 1 info)))
      (or numbers (setq numbers (nth 2 info)))
      (or max     (setq max     (nth 3 info))))
    (elmo-set-hash-val (elmo-folder-name-internal folder)
		       (list new unread numbers max)
		       elmo-folder-info-hashtb)))

(defun elmo-folder-set-info-max-by-numdb (folder numbers)
  "Set FOLDER info by MSGDB-NUMBER in msgdb."
  (elmo-folder-set-info-hashtb
   folder
   (if numbers (apply #'max numbers) 0)
;;;   (length num-db)
   nil
   ))

(defun elmo-folder-get-info-max (folder)
  "Return max number of FODLER from folder info."
  (nth 3 (elmo-folder-get-info folder)))

(defun elmo-folder-get-info-length (folder)
  "Return length of FODLER from folder info."
  (nth 2 (elmo-folder-get-info folder)))

(defun elmo-folder-get-info-unread (folder)
  "Return unread of FODLER from folder info."
  (nth 1 (elmo-folder-get-info folder)))

(defun elmo-folder-info-make-hashtb (info-alist hashtb)
  "Setup folder info hashtable by INFO-ALIST on HASHTB."
  (let* ((hashtb (or hashtb
		     (elmo-make-hash (length info-alist)))))
    (mapc
     (lambda (x)
       (let ((info (cadr x)))
	 (and (intern-soft (car x) hashtb)
	      (elmo-set-hash-val (car x)
				 (list (nth 2 info)   ;; new
				       (nth 3 info)   ;; unread
				       (nth 1 info)   ;; length
				       (nth 0 info))  ;; max
				 hashtb))))
     info-alist)
    (setq elmo-folder-info-hashtb hashtb)))

(defsubst elmo-diff-new (diff)
  (car diff))

(defsubst elmo-diff-unread (diff)
  (when (consp (cdr diff))
    (nth 1 diff)))

(defsubst elmo-diff-all (diff)
  (if (consp (cdr diff))
      (nth 2 diff)
    (cdr diff)))

(defsubst elmo-strict-folder-diff (folder)
  "Return folder diff information strictly from FOLDER."
  (let ((in-db (sort (elmo-folder-list-messages folder nil 'in-msgdb) '<))
	(in-folder  (elmo-folder-list-messages folder))
	append-list delete-list diff)
    (cons (if (equal in-folder in-db)
	      0
	    (setq diff (elmo-list-diff in-folder in-db))
	    (setq append-list (car diff))
	    (setq delete-list (cadr diff))
	    (if append-list
		(length append-list)
	      (if delete-list
		  (- (length delete-list))
		0)))
	  (length in-folder))))

(luna-define-method elmo-folder-diff ((folder elmo-folder))
  (elmo-generic-folder-diff folder))

(defun elmo-generic-folder-diff (folder)
  (if (elmo-string-match-member (elmo-folder-name-internal folder)
				elmo-strict-diff-folder-list)
      (elmo-strict-folder-diff folder)
    (let ((cached-in-db-max (elmo-folder-get-info-max folder))
	  (in-folder (elmo-folder-status folder))
	  (in-db t)
	  unsync messages
	  in-db-max)
      (if (not cached-in-db-max)
	  (let ((number-list (elmo-folder-list-messages folder
							nil 'in-msgdb)))
	    ;; No info-cache.
	    (setq in-db number-list)
	    (setq in-db-max (if in-db (apply #'max in-db) 0))
	    (elmo-folder-set-info-hashtb folder in-db-max nil))
	(setq in-db-max cached-in-db-max))
      (setq unsync (if (and in-db (car in-folder))
		       (- (car in-folder) in-db-max)
		     (if (and in-folder (null in-db))
			 (cdr in-folder)
		       (car in-folder))))
      (setq messages (cdr in-folder))
      (if (and unsync messages (> unsync messages))
	  (setq unsync messages))
      (cons (or unsync 0) (or messages 0)))))

(defvar elmo-folder-diff-async-callback nil)
(defvar elmo-folder-diff-async-callback-data nil)

(luna-define-method elmo-folder-diff-async ((folder elmo-folder))
  (and elmo-folder-diff-async-callback
       (funcall elmo-folder-diff-async-callback
		folder
		(elmo-folder-diff folder))))

(luna-define-method elmo-folder-get-primitive-list ((folder elmo-folder))
  (list folder))

(luna-define-method elmo-folder-contains-type ((folder elmo-folder) type)
  (eq (elmo-folder-type-internal folder) type))

(luna-define-method elmo-folder-next-message-number ((folder elmo-folder))
  (1+ (elmo-max-of-list (elmo-folder-list-messages folder))))

(eval-and-compile
  (luna-define-class elmo-file-tag))

(defconst elmo-append-messages-dispatch-table
  '(((nil	. null)		. elmo-folder-append-messages-*-null)
    ((filter	. nil)		. elmo-folder-append-messages-filter-*)
    ((nil	. filter)	. elmo-folder-append-messages-*-filter)
    ((pipe	. nil)		. elmo-folder-append-messages-pipe-*)
    ((nil	. pipe)		. elmo-folder-append-messages-*-pipe)
    ((multi	. nil)		. elmo-folder-append-messages-multi-*)
    ((nil	. flag)		. elmo-folder-append-messages-*-flag)
    ((imap4	. imap4)	. elmo-folder-append-messages-imap4-imap4)
    ((elmo-file-tag . localdir)	. elmo-folder-append-messages-*-localdir)
    ((elmo-file-tag . maildir)	. elmo-folder-append-messages-*-maildir)
    ((nil	. archive)	. elmo-folder-append-messages-*-archive)
    ((nil	. nil)		. elmo-generic-folder-append-messages)))

(defun elmo-folder-type-p (folder type)
  (or (null type)
      (eq (elmo-folder-type-internal folder) type)
      (labels ((member-if (predicate list)
			  (and list
			       (or (funcall predicate (car list))
				   (member-if predicate (cdr list)))))
	       (subtypep (name type)
			 (or (eq name type)
			     (let ((class (luna-find-class name)))
			       (and class
				    (member-if (lambda (name)
						 (subtypep name type))
					       (luna-class-parents class)))))))
	(subtypep (luna-class-name folder)
		  (or (intern-soft (format "elmo-%s-folder" type))
		      type)))))

(defun elmo-folder-append-messages (dst-folder src-folder numbers
					       &optional same-number caller)
  "Append messages from folder.
DST-FOLDER is the ELMO folder structure.
Caller should make sure DST-FOLDER is `writable'.
\(Can be checked with `elmo-folder-writable-p'\).
SRC-FOLDER is the source ELMO folder structure.
NUMBERS is the message numbers to be appended in the SRC-FOLDER.
If second optional argument SAME-NUMBER is specified,
message number is preserved \(if possible\).
Returns a list of message numbers successfully appended."
  (let ((rest (if caller
		  (cdr (memq (rassq caller elmo-append-messages-dispatch-table)
			     elmo-append-messages-dispatch-table))
		elmo-append-messages-dispatch-table))
	result)
    (while rest
      (let ((types (car (car rest))))
	(if (and (elmo-folder-type-p src-folder (car types))
		 (elmo-folder-type-p dst-folder (cdr types)))
	    (setq result (funcall (cdr (car rest))
				  dst-folder src-folder numbers same-number)
		  rest nil)
	  (setq rest (cdr rest)))))
    result))

(defun elmo-generic-folder-append-messages (folder src-folder numbers
						   same-number)
  (let ((src-msgdb-exists (not (zerop (elmo-folder-length src-folder))))
	unseen
	succeed-numbers failure cache id)
    (elmo-folder-flag-table folder) ; load
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (while numbers
	(setq failure nil
	      id (and src-msgdb-exists
		      (elmo-message-field src-folder (car numbers)
					  'message-id)))
	(condition-case nil
	    (setq cache (elmo-file-cache-get id)
		  failure
		  (not
		   (and
		    (elmo-message-fetch
		     src-folder (car numbers)
		     (if (elmo-folder-plugged-p src-folder)
			 (elmo-make-fetch-strategy
			  'entire 'maybe nil
			  (and cache (elmo-file-cache-path cache)))
		       (or (and elmo-enable-disconnected-operation
				cache
				(eq (elmo-file-cache-status cache) 'entire)
				(elmo-make-fetch-strategy
				 'entire t nil
				 (elmo-file-cache-path cache)))
			   (error "Unplugged")))
		     'unread)
		    (> (buffer-size) 0)
		    (elmo-folder-append-buffer
		     folder
		     (elmo-message-flags-for-append src-folder (car numbers))
		     (if same-number (car numbers))))))
	  (error (setq failure t)))
	;; FETCH & APPEND finished
	(unless failure
	  (setq succeed-numbers (cons (car numbers) succeed-numbers)))
	(elmo-progress-notify 'elmo-folder-move-messages)
	(setq numbers (cdr numbers)))
      (when (elmo-folder-persistent-p folder)
	(elmo-folder-close-flag-table folder))
      succeed-numbers)))

;; Arguments should be reduced.
(defun elmo-folder-move-messages (src-folder msgs dst-folder
					     &optional
					     no-delete
					     same-number)
  (save-excursion
    (let* ((messages msgs)
	   (len (length msgs))
	   succeeds i result)
      (if (eq dst-folder 'null)
	  (setq succeeds messages)
	(unless (elmo-folder-writable-p dst-folder)
	  (error "move: %d is not writable"
		 (elmo-folder-name-internal dst-folder)))
	(when messages
	  (elmo-folder-open-internal src-folder)
	  (elmo-folder-open-internal dst-folder)
	  (unless (setq succeeds (elmo-folder-append-messages dst-folder
							      src-folder
							      messages
							      same-number))
	    (error "move: append message to %s failed"
		   (elmo-folder-name-internal dst-folder)))
	  (elmo-folder-close dst-folder)))
      (if (and (not no-delete) succeeds)
	  (progn
	    (if (elmo-folder-delete-messages src-folder succeeds)
		(progn
		  (elmo-global-flag-detach-messages
		   src-folder succeeds (eq dst-folder 'null))
		  (setq result t))
	      (message "move: delete messages from %s failed."
		       (elmo-folder-name-internal src-folder))
	      (setq result nil))
	    result)
	(if no-delete
	    (progn
;;;	      (message "Copying messages...done")
	      t)
	  (if (zerop len)
	      (message "No message was moved.")
	    (message "Moving messages failed.")
	    nil ; failure
	    ))))))

(defun elmo-folder-msgdb-path (folder)
  "Return the msgdb path for FOLDER."
  (or (elmo-folder-path-internal folder)
      (elmo-folder-set-path-internal
       folder
       (if (null elmo-msgdb-path-encode-threshold)
	   (elmo-folder-expand-msgdb-path folder)
	 (let* ((path (directory-file-name
		       (elmo-folder-expand-msgdb-path folder)))
		(dirname (file-name-nondirectory path)))
	   (if (<= (length dirname) elmo-msgdb-path-encode-threshold)
	       path
	     (setq dirname (md5 dirname))
	     (when (> (length dirname) elmo-msgdb-path-encode-threshold)
	       (error "Cannot shrink msgdb path for `%s'"
		      (elmo-folder-name-internal folder)))
	     (expand-file-name dirname (file-name-directory path))))))))

(luna-define-generic elmo-message-cached-p (folder number)
  "Return non-nil if the message is cached.")

(luna-define-method elmo-message-cached-p ((folder elmo-folder) number)
  (elmo-message-flagged-p folder number 'cached))

(luna-define-generic elmo-message-killed-p (folder number)
  "Return non-nil if the message is killed.")

(luna-define-method elmo-message-killed-p ((folder elmo-folder) number)
  (let ((killed-list (elmo-folder-killed-list-internal folder)))
    (and killed-list
	 (elmo-number-set-member number killed-list))))

(defun elmo-message-accessible-p (folder number)
  "Get accessibility of the message.
Return non-nil when message is accessible."
  (or (elmo-folder-plugged-p folder)
      (elmo-folder-local-p folder)
      (< number 0) ; in dop spool
      (elmo-message-cached-p folder number)))

(luna-define-generic elmo-message-set-cached (folder number cached)
  "Set cache status of the message in the msgdb.
FOLDER is the ELMO folder structure.
NUMBER is a number of the message.
If CACHED is t, message is set as cached.")

(luna-define-method elmo-message-set-cached ((folder elmo-folder)
					     number cached)
  (if cached
      (elmo-msgdb-set-flag (elmo-folder-msgdb folder) number 'cached)
    (elmo-msgdb-unset-flag (elmo-folder-msgdb folder) number 'cached))
  (elmo-emit-signal 'status-changed folder (list number)))

(defun elmo-message-copy-entity (entity)
  (elmo-msgdb-copy-message-entity (elmo-message-entity-handler entity)
				  entity))

(luna-define-generic elmo-message-number (folder message-id)
  "Get message number from MSGDB which corresponds to MESSAGE-ID.")

(luna-define-method elmo-message-number ((folder elmo-folder) message-id)
  (elmo-msgdb-message-number (elmo-folder-msgdb folder) message-id))

(luna-define-generic elmo-message-entity (folder key)
  "Return the message-entity structure which matches to the KEY.
KEY is a number or a string.
A number is for message number in the FOLDER.
A string is for message-id of the message.")

(luna-define-method elmo-message-entity ((folder elmo-folder) key)
  (elmo-msgdb-message-entity (elmo-folder-msgdb folder) key))

(luna-define-generic elmo-message-entity-parent (folder entity)
  "Return the parent message-entity structure in the FOLDER.
ENTITY is the message-entity to get the parent.")

(luna-define-method elmo-message-entity-parent ((folder elmo-folder) entity)
  (elmo-msgdb-get-parent-entity entity (elmo-folder-msgdb folder)))

(put 'elmo-folder-do-each-message-entity 'lisp-indent-function '1)
(def-edebug-spec elmo-folder-do-each-message-entity
  ((symbolp form &rest form) &rest form))

(defsubst elmo-folder-list-message-entities (folder)
  ;; List all message entities in the FOLDER.
  (mapcar
   (lambda (number) (elmo-message-entity folder number))
   (elmo-folder-list-messages folder nil t))) ; XXX killed-list is not used.

(defmacro elmo-folder-do-each-message-entity (spec &rest form)
  "Iterator for message entity in the folder.
\(elmo-folder-do-each-message-entity \(entity folder\)
 ... do the process using entity...
\)"
  `(dolist (,(car spec) (elmo-folder-list-message-entities ,(car (cdr spec))))
     ,@form))

(luna-define-generic elmo-folder-count-flags (folder)
  "Count flagged message number in the msgdb of the FOLDER.
Return alist of flag and numbers.
Example:
\(\(new . 10\)
  \(unread . 20\)
  \(answered . 3\)\)")

(luna-define-method elmo-folder-count-flags ((folder elmo-folder))
  (elmo-msgdb-flag-count (elmo-folder-msgdb folder)))

(defun elmo-message-set-flag (folder number flag &optional is-local)
  "Set message flag.
FOLDER is a ELMO folder structure.
NUMBER is a message number to set flag.

FLAG is a symbol which is one of the following:
  `unread'    (set the message as unread)
  `answered'  (set the message as answered)
  `important' (set the message as important)
'sugar' flag:
  `read'      (remove new and unread flags)
If optional IS-LOCAL is non-nil, update only local (not server) status."
  ;; XXX Transitional implementation.
  (elmo-folder-set-flag folder (list number) flag is-local))

(defun elmo-message-unset-flag (folder number flag &optional is-local)
  "Unset message flag.
FOLDER is a ELMO folder structure.
NUMBER is a message number to set flag.

FLAG is a symbol which is one of the following:
  `unread'    (remove unread and new flag)
  `answered'  (remove answered flag)
  `important' (remove important flag)
'sugar' flag:
  `read'      (set unread flag)
If optional IS-LOCAL is non-nil, update only local (not server) status."
  ;; XXX Transitional implementation.
  (elmo-folder-unset-flag folder (list number) flag is-local))

(luna-define-generic elmo-message-field (folder number field &optional type)
  "Get message field value in the msgdb.
FOLDER is the ELMO folder structure.
NUMBER is a number of the message.
FIELD is a symbol of the field.
If optional argument TYPE is specified, return converted value.")

(luna-define-method elmo-message-field ((folder elmo-folder)
					number field &optional type)
  (elmo-msgdb-message-field (elmo-folder-msgdb folder) number field type))

(luna-define-generic elmo-message-set-field (folder number field value)
  "Set message field value in the msgdb.
FOLDER is the ELMO folder structure.
NUMBER is a number of the message.
FIELD is a symbol of the field.
VALUE is a value to set.")

(luna-define-method elmo-message-set-field ((folder elmo-folder) number
					    field value)
  (elmo-message-entity-set-field (elmo-message-entity folder number)
				 field value))

(luna-define-method elmo-message-use-cache-p ((folder elmo-folder) number)
  nil) ; default is not use cache.

(luna-define-method elmo-message-folder ((folder elmo-folder) number)
  folder) ; default is folder

(luna-define-method elmo-folder-set-flag ((folder elmo-folder)
					  numbers
					  flag
					  &optional is-local)
  (when (elmo-folder-msgdb-internal folder)
    (dolist (number numbers)
      (let ((old-flags (elmo-message-flags folder number)))
	(when (elmo-global-flag-p flag)
	  (let ((message-id (elmo-message-field folder number 'message-id)))
	    (elmo-global-flag-set flag folder number message-id)))
	(elmo-msgdb-set-flag (elmo-folder-msgdb folder) number flag)
	(elmo-emit-signal 'flag-changing
			  folder
			  number
			  old-flags
			  (elmo-message-flags folder number))))
    (elmo-emit-signal 'flag-changed folder numbers)))

(defun elmo-message-has-global-flag-p (folder number)
  "Return non-nil when the message in the FOLDER with NUMBER has global flag."
  (let ((flags (elmo-message-flags folder number))
	result)
    (while flags
      (when (and (elmo-global-flag-p (car flags))
		 (not (memq (car flags) '(answered unread cached))))
	(setq result t
	      flags nil))
      (setq flags (cdr flags)))
    result))

(defun elmo-message-set-global-flags (folder number flags &optional local)
  "Set global flags of the message in the FOLDER with NUMBER as FLAGS.
If Optional LOCAL is non-nil, don't update server flag."
  (dolist (flag flags)
    (unless (elmo-global-flag-p flag)
      (error "Not a global flag")))
  (let ((old-flags (elmo-get-global-flags (elmo-message-flags folder number))))
    (dolist (flag flags)
      (unless (memq flag old-flags)
	(elmo-message-set-flag folder number flag local)))
    (dolist (flag old-flags)
      (unless (memq flag flags)
	(elmo-message-unset-flag folder number flag local)))))

(luna-define-method elmo-folder-unset-flag ((folder elmo-folder)
					    numbers
					    flag
					    &optional is-local)
  (when (elmo-folder-msgdb-internal folder)
    (dolist (number numbers)
      (let ((old-flags (elmo-message-flags folder number)))
	(when (elmo-global-flag-p flag)
	  (elmo-global-flag-detach flag folder number 'always))
	(elmo-msgdb-unset-flag (elmo-folder-msgdb folder) number flag)
	(elmo-emit-signal 'flag-changing
			  folder
			  number
			  old-flags
			  (elmo-message-flags folder number))))
    (elmo-emit-signal 'flag-changed folder numbers)))

(luna-define-method elmo-folder-process-crosspost ((folder elmo-folder))
  ;; Do nothing.
  )

;;;(luna-define-generic elmo-folder-append-message-entity (folder entity
;;;							       &optional
;;;							       flag-table)
;;;  "Append ENTITY to the folder.")

(defun elmo-msgdb-merge (folder msgdb-merge)
  "Return a list of messages which have duplicated message-id."
  (let (msgdb duplicates)
    (setq msgdb (or (elmo-folder-msgdb-internal folder)
		    (elmo-make-msgdb (elmo-folder-msgdb-path folder))))
    (setq duplicates (elmo-msgdb-append msgdb msgdb-merge))
    (elmo-folder-set-msgdb-internal folder msgdb)
    duplicates))

(defsubst elmo-folder-append-msgdb (folder append-msgdb)
  (if append-msgdb
      (let ((duplicates (elmo-msgdb-merge folder append-msgdb)))
	(cond ((eq (elmo-folder-process-duplicates-internal folder)
		   'hide)
	       ;; Let duplicates be a temporary killed message.
	       (elmo-folder-kill-messages folder duplicates)
	       ;; Should be flag as read.
	       (elmo-folder-unset-flag folder duplicates 'unread))
	      ((eq (elmo-folder-process-duplicates-internal folder)
		   'read)
	       ;; Flag as read duplicates.
	       (elmo-folder-unset-flag folder duplicates 'unread))
	      (t
	       ;; Do nothing.
	       (setq duplicates nil)))
	(length duplicates))
    0))

(defun elmo-folder-confirm-appends (folder appends)
  (let ((len (length appends))
	in)
    (if (and elmo-folder-update-threshold
	     (> len elmo-folder-update-threshold)
	     elmo-folder-update-confirm)
	(if (y-or-n-p (format
		       "Too many messages(%d) in %s.  Update all? "
		       len (elmo-folder-name-internal folder)))
	    appends
	  (setq in elmo-folder-update-threshold)
	  (catch 'end
	    (while t
	      (setq in (read-from-minibuffer "Update number: "
					     (number-to-string in))
		    in (string-to-number in))
	      (if (< len in)
		  (throw 'end len))
	      (if (y-or-n-p (format
			     "%d messages are killed (not appeared). OK? "
			     (max (- len in) 0)))
		  (throw 'end in))))
	  (nthcdr (max (- len in) 0) appends))
      (if (and elmo-folder-update-threshold
	       (> len elmo-folder-update-threshold)
	       (not elmo-folder-update-confirm))
	  (nthcdr (max (- len elmo-folder-update-threshold) 0) appends)
	appends))))

(luna-define-method elmo-message-fetch-bodystructure ((folder elmo-folder)
						      number strategy)
  nil)

(defun elmo-message-fetch-string (folder number strategy
					 &optional
					 unread
					 section)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (when (elmo-message-fetch folder number strategy unread section)
      (buffer-string))))

(luna-define-method elmo-message-fetch ((folder elmo-folder)
					number strategy
					&optional
					unread
					section)
  (erase-buffer)
  (let ((cache-path (elmo-fetch-strategy-cache-path strategy))
	(method-priorities
	 (cond ((eq (elmo-fetch-strategy-use-cache strategy) 'maybe)
		'(entity cache))
	       ((elmo-fetch-strategy-use-cache strategy)
		'(cache entity))
	       (t
		'(entity))))
	result err updated-server-flag)
    (while (and method-priorities
		(not result))
      (setq result
	    (case (car method-priorities)
	      (cache
	       (elmo-file-cache-load cache-path section))
	      (entity
	       (when (condition-case error
			 (elmo-message-fetch-internal folder number
						      strategy
						      section
						      unread)
		       (error (setq err error) nil))
		 (setq updated-server-flag t)
		 (when (and (elmo-fetch-strategy-save-cache strategy)
			    cache-path)
		   (elmo-file-cache-save cache-path section))
		 t)))
	    method-priorities (cdr method-priorities)))
    (if result
	(when (and (not unread)
		   (elmo-message-flagged-p folder number 'unread))
	  (elmo-message-unset-flag folder number 'unread updated-server-flag))
      (when err
	(signal (car err) (cdr err))))
    result))

(defun elmo-folder-kill-messages-range (folder beg end)
  (elmo-folder-set-killed-list-internal
   folder
   (nconc
    (elmo-folder-killed-list-internal folder)
    (list (cons beg end)))))

(defun elmo-folder-kill-messages (folder numbers)
  "Kill(hide) messages in the FOLDER with NUMBERS."
  (elmo-folder-set-killed-list-internal
   folder
   (elmo-number-set-append-list
    (elmo-folder-killed-list-internal folder)
    numbers))
  (elmo-folder-unset-flag folder numbers 'all 'local-only))

(luna-define-generic elmo-folder-recover-messages (folder numbers)
  "Recover killed messages in the FOLDER with NUMBERS.")

(luna-define-method elmo-folder-recover-messages ((folder elmo-folder) numbers)
  (let ((msgdb (elmo-folder-msgdb folder)))
    (elmo-folder-set-killed-list-internal
     folder
     (elmo-number-set-delete-list
      (elmo-folder-killed-list-internal folder)
      numbers))
    (dolist (number numbers)
      (if (elmo-file-cache-exists-p
	   (elmo-message-field folder number 'message-id))
	  (elmo-msgdb-set-flag msgdb number 'cached)
	(elmo-msgdb-unset-flag msgdb number 'cached)))
    (elmo-emit-signal 'status-changed folder numbers)))

(luna-define-method elmo-folder-clear ((folder elmo-folder)
				       &optional keep-killed)
  (unless keep-killed
    (elmo-folder-set-killed-list-internal folder nil))
  (if (eq elmo-msgdb-convert-type 'sync)
      (elmo-folder-set-msgdb-internal
       folder
       (elmo-make-msgdb (elmo-folder-msgdb-path folder)))
    (elmo-msgdb-clear (elmo-folder-msgdb folder))))

(luna-define-generic elmo-folder-synchronize (folder
					      &optional
					      disable-killed
					      ignore-msgdb
					      no-check
					      mask)
  "Synchronize the folder data to the newest status.
FOLDER is the ELMO folder structure.

If optional DISABLE-KILLED is non-nil, killed messages are also synchronized.
If optional IGNORE-MSGDB is non-nil, current msgdb is thrown away except
flag status.
If NO-CHECK is non-nil, rechecking folder is skipped.
If optional argument MASK is specified and is a list of message numbers,
synchronize messages only which are contained the list.
MASK is assumed to be a subset of existing (not deleted) messages.
Return amount of cross-posted messages.
If update process is interrupted, return nil.")

(luna-define-method elmo-folder-synchronize ((folder elmo-folder)
					     &optional
					     disable-killed
					     ignore-msgdb
					     no-check
					     mask)
  (let ((old-msgdb (elmo-folder-msgdb folder))
	(killed-list (elmo-folder-killed-list-internal folder))
	(flag-table (elmo-flag-table-load (elmo-folder-msgdb-path folder)))
	(before-append t))
    (when ignore-msgdb
      (elmo-msgdb-flag-table (elmo-folder-msgdb folder) flag-table)
      (elmo-folder-clear folder (not disable-killed)))
    (unless no-check (elmo-folder-check folder))
    (condition-case nil
	(let ((killed-list (elmo-folder-killed-list-internal folder))
	      diff-new diff-del
	      delete-list new-list new-msgdb crossed)
	  (message "Checking folder diff...")
          ;; If MASK is supplied, compare against messagedb to
          ;; determine what needs to be synchronized.
          (if (and mask (not ignore-msgdb))
              (setq diff-new
                    (car (elmo-list-diff
                          mask
                          (elmo-folder-list-messages folder nil 'in-msgdb))))
            (elmo-set-list
             '(diff-new diff-del)
             (elmo-list-diff (elmo-folder-list-messages folder)
                             (elmo-folder-list-messages folder nil 'in-msgdb))))
	  (if diff-new
	    (unless disable-killed
	      (setq diff-new (elmo-living-messages diff-new killed-list))))
	  (message "Checking folder diff...done")
	  (setq new-list (elmo-folder-confirm-appends folder diff-new))
	  ;; Append to killed list as (MIN-OF-DISAPPEARED . MAX-OF-DISAPPEARED)
	  (when (/= (length diff-new)
		    (length new-list))
	    (let* ((diff (elmo-list-diff diff-new new-list))
		   (disappeared (car diff)))
	      (when disappeared
		(elmo-folder-kill-messages-range folder
						 (car disappeared)
						 (elmo-last disappeared)))))
	  (setq delete-list diff-del)
	  (if (and (null diff-new) (null diff-del))
	      (progn
		(elmo-folder-update-number folder)
		(elmo-folder-process-crosspost folder)
		0)			; `0' means no updates.
	    (when delete-list
	      (elmo-folder-detach-messages folder delete-list))
	    (when new-list
	      (elmo-msgdb-out-of-date-messages (elmo-folder-msgdb folder))
	      (setq new-msgdb (elmo-folder-msgdb-create
			       folder new-list flag-table))
	      ;; Clear flag-table
	      (if (elmo-folder-persistent-p folder)
		  (elmo-flag-table-save (elmo-folder-msgdb-path folder)
					nil))
	      (setq before-append nil)
	      (setq crossed (elmo-folder-append-msgdb folder new-msgdb))
	      ;; process crosspost.
	      ;; Return a cons cell of (NUMBER-CROSSPOSTS . NEW-FLAG-ALIST).
	      (elmo-folder-process-crosspost folder))
	    ;; return value.
	    (or crossed 0)))
      (quit
       ;; Resume to the original status.
       (if before-append (elmo-folder-set-msgdb-internal folder old-msgdb))
       (elmo-folder-set-killed-list-internal folder killed-list)
       nil))))

(luna-define-generic elmo-folder-detach-messages (folder numbers)
  "Remove messages with NUMBERS from MSGDB.")

(luna-define-method elmo-folder-detach-messages ((folder elmo-folder)
						 numbers)
  (when (elmo-msgdb-delete-messages (elmo-folder-msgdb folder) numbers)
    ;; Remove NUMBERS from killed message list.
    (elmo-folder-set-killed-list-internal
     folder
     (elmo-number-set-delete-list
      (elmo-folder-killed-list-internal folder)
      numbers))
    t))

(luna-define-generic elmo-folder-length (folder)
  "Return number of messages in the FOLDER.")

(luna-define-method elmo-folder-length ((folder elmo-folder))
  (if (elmo-folder-msgdb-internal folder)
      (elmo-msgdb-length (elmo-folder-msgdb folder))
    0))

(defun elmo-folder-msgdb-load (folder &optional silent)
  (unless silent
    (message "Loading msgdb for %s..." (elmo-folder-name-internal folder)))
  (let ((msgdb (elmo-load-msgdb (elmo-folder-msgdb-path folder)
				(elmo-folder-mime-charset-internal folder))))
    (elmo-folder-set-info-max-by-numdb
     folder
     (elmo-msgdb-list-messages msgdb))
    (unless silent
      (message "Loading msgdb for %s...done"
	       (elmo-folder-name-internal folder)))
    msgdb))

(defun elmo-msgdb-delete-path (folder)
  (let ((path (elmo-folder-msgdb-path folder)))
    (if (file-directory-p path)
	(elmo-delete-directory path t))))

(defun elmo-msgdb-rename-path (old-folder new-folder)
  (let* ((old (directory-file-name (elmo-folder-msgdb-path old-folder)))
	 (new (directory-file-name (elmo-folder-msgdb-path new-folder)))
	 (new-dir (directory-file-name (file-name-directory new))))
    (if (not (file-directory-p old))
	()
      (if (file-exists-p new)
	  (error "Already exists directory: %s" new)
	(if (not (file-exists-p new-dir))
	    (elmo-make-directory new-dir))
	(rename-file old new)))))

(defun elmo-setup-subscribed-newsgroups (groups)
  "Setup subscribed newsgroups.
GROUPS is a list of newsgroup name string.
Return a hashtable for newsgroups."
  (let ((hashtb (or elmo-newsgroups-hashtb
		    (setq elmo-newsgroups-hashtb
			  (elmo-make-hash (length groups))))))
    (dolist (group groups)
      (or (elmo-get-hash-val group hashtb)
	  (elmo-set-hash-val group nil hashtb)))
    (setq elmo-newsgroups-hashtb hashtb)))

(defvar elmo-crosspost-message-alist-modified nil)
(defun elmo-crosspost-message-alist-load ()
  "Load crosspost message alist."
  (setq elmo-crosspost-message-alist (elmo-crosspost-alist-load))
  (setq elmo-crosspost-message-alist-modified nil))

(defun elmo-crosspost-message-alist-save ()
  "Save crosspost message alist."
  (when elmo-crosspost-message-alist-modified
    (let ((alist elmo-crosspost-message-alist)
	  newsgroups)
      (while alist
	(setq newsgroups
	      (elmo-delete-if
	       (lambda (x)
		 (not (intern-soft x elmo-newsgroups-hashtb)))
	       (nth 1 (car alist))))
	(if newsgroups
	    (setcar (cdar alist) newsgroups)
	  (setq elmo-crosspost-message-alist
		(delete (car alist) elmo-crosspost-message-alist)))
	(setq alist (cdr alist)))
      (elmo-crosspost-alist-save elmo-crosspost-message-alist)
      (setq elmo-crosspost-message-alist-modified nil))))

(defun elmo-folder-make-temporary-directory (folder)
  ;; Make a temporary directory for FOLDER.
  (let ((temp-dir (make-temp-name
		   (concat
		    (file-name-as-directory (elmo-folder-msgdb-path folder))
		    "elmo"))))
    (elmo-make-directory temp-dir)
    temp-dir))

;; ELMO status structure.
(defmacro elmo-message-status (folder number &optional flags killed)
  "Make ELMO status structure from FOLDER and NUMBER.
A value in this structure is cached at first access."
  `(vector ,folder ,number ,flags ,killed))

(defmacro elmo-message-status-folder (status)
  `(aref ,status 0))

(defmacro elmo-message-status-number (status)
  `(aref ,status 1))

(defmacro elmo-message-status-set-flags (status flags)
  `(aset ,status 2 (or ,flags '(read))))

(defsubst elmo-message-status-flags (status)
  (or (aref status 2)
      (elmo-message-status-set-flags
       status
       (elmo-message-flags (elmo-message-status-folder status)
			   (elmo-message-status-number status)))))

(defsubst elmo-message-status-cached-p (status)
  (memq 'cached (elmo-message-status-flags status)))

(defmacro elmo-message-status-set-killed (status killed)
  `(aset ,status 3 (if ,killed 'killed 'living)))

(defsubst elmo-message-status-killed-p (status)
  (eq 'killed
      (or (aref status 3)
	  (elmo-message-status-set-killed
	   status
	   (elmo-message-killed-p (elmo-message-status-folder status)
				  (elmo-message-status-number status))))))

;;;
(defun elmo-init ()
  "Initialize ELMO module."
  (elmo-crosspost-message-alist-load)
  (elmo-resque-obsolete-variables)
  (elmo-dop-queue-load)
  (run-hooks 'elmo-init-hook))

(defun elmo-quit ()
  "Quit and cleanup ELMO."
  (elmo-crosspost-message-alist-save)
  (elmo-dop-queue-save)
  ;; Not implemented yet.
  (let ((types elmo-folder-type-alist)
	class)
    (while types
      (setq class
	    (luna-find-class
	     (intern (format "elmo-%s-folder"
			     (symbol-name (cdr (car types)))))))
      ;; Call all folder's `elmo-quit' method.
      (if class
	  (dolist (func (luna-class-find-functions class 'elmo-quit))
	    (funcall func nil)))
      (setq types (cdr types)))))

(luna-define-method elmo-folder-rename-internal ((folder elmo-folder)
						 new-folder)
  (error "Cannot rename %s folder"
	 (symbol-name (elmo-folder-type-internal folder))))

;;; Define folders.
(elmo-define-folder ?% 'imap4)
(elmo-define-folder ?-  'nntp)
(elmo-define-folder ?\+ 'localdir)
(elmo-define-folder ?\* 'multi)
(elmo-define-folder ?\/ 'filter)
(elmo-define-folder ?\$ 'archive)
(elmo-define-folder ?&  'pop3)
(elmo-define-folder ?=  'localnews)
(elmo-define-folder ?|  'pipe)
(elmo-define-folder ?.  'maildir)
(elmo-define-folder ?'  'internal)
(elmo-define-folder ?\[  'search)
(elmo-define-folder ?@  'shimbun)

;;; Obsolete variables.
(elmo-define-obsolete-variable 'elmo-default-imap4-mailbox
			       'elmo-imap4-default-mailbox)
(elmo-define-obsolete-variable 'elmo-default-imap4-server
			       'elmo-imap4-default-server)
(elmo-define-obsolete-variable 'elmo-default-imap4-authenticate-type
			       'elmo-imap4-default-authenticate-type)
(elmo-define-obsolete-variable 'elmo-default-imap4-user
			       'elmo-imap4-default-user)
(elmo-define-obsolete-variable 'elmo-default-imap4-port
			       'elmo-imap4-default-port)
(elmo-define-obsolete-variable 'elmo-default-imap4-stream-type
			       'elmo-imap4-default-stream-type)
(elmo-define-obsolete-variable 'elmo-default-nntp-server
			       'elmo-nntp-default-server)
(elmo-define-obsolete-variable 'elmo-default-nntp-user
			       'elmo-nntp-default-user)
(elmo-define-obsolete-variable 'elmo-default-nntp-port
			       'elmo-nntp-default-port)
(elmo-define-obsolete-variable 'elmo-default-nntp-stream-type
			       'elmo-nntp-default-stream-type)
(elmo-define-obsolete-variable 'elmo-default-pop3-server
			       'elmo-pop3-default-server)
(elmo-define-obsolete-variable 'elmo-default-pop3-user
			       'elmo-pop3-default-user)
(elmo-define-obsolete-variable 'elmo-default-pop3-authenticate-type
			       'elmo-pop3-default-authenticate-type)
(elmo-define-obsolete-variable 'elmo-default-pop3-port
			       'elmo-pop3-default-port)
(elmo-define-obsolete-variable 'elmo-default-pop3-stream-type
			       'elmo-pop3-default-stream-type)
(elmo-define-obsolete-variable 'elmo-cache-dirname
			       'elmo-cache-directory)
(elmo-define-obsolete-variable 'elmo-msgdb-dir
			       'elmo-msgdb-directory)
(elmo-define-obsolete-variable 'elmo-global-flag-list
			       'elmo-global-flags)
(elmo-define-obsolete-variable 'elmo-nmz-default-index-path
			       'elmo-search-namazu-default-index-path)
(elmo-define-obsolete-variable 'elmo-nmz-index-alias-alist
			       'elmo-search-namazu-index-alias-alist)
(elmo-define-obsolete-variable 'elmo-nmz-use-drive-letter
			       'elmo-search-use-drive-letter)


;; Obsolete functions.
;; 2001-12-11: *-dir -> *-directory
(defalias 'elmo-folder-make-temp-dir 'elmo-folder-make-temporary-directory)
(make-obsolete 'elmo-folder-make-temp-dir
	       'elmo-folder-make-temporary-directory)

(require 'product)
(product-provide (provide 'elmo) (require 'elmo-version))

;;; elmo.el ends here
