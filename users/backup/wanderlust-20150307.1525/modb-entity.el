;;; modb-entity.el --- Message Entity Interface.

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
;; Message entity handling.

;;; Code:

(eval-when-compile (require 'cl))

(require 'luna)
(require 'elmo-vars)
(require 'elmo-util)

(eval-and-compile
  (luna-define-class modb-entity-handler () (mime-charset))
  (luna-define-internal-accessors 'modb-entity-handler))

(defcustom modb-entity-default-handler 'modb-legacy-entity-handler
  "Default entity handler."
  :type 'symbol
  :group 'elmo)

(defcustom modb-entity-field-extractor-alist
  '((ml-info modb-entity-extract-mailing-list-info
	     modb-entity-ml-info-real-fields))
  "*An alist of field name and function to extract field body from buffer."
  :type '(repeat (list (symbol :tag "Field Name")
		       (function :tag "Extractor")
		       (choice :tag "Real Field"
			       (repeat :tag "Field Name List" string)
			       (function :tag "Function"))))
  :group 'elmo)

(defvar modb-entity-default-cache-internal nil)

(defun elmo-message-entity-handler (&optional entity)
  "Get modb entity handler instance which corresponds to the ENTITY."
  (if (and entity
	   (car-safe entity)
	   (not (eq (car entity) t))
	   (not (stringp (car entity))))
      (car entity)
    (or modb-entity-default-cache-internal
	(setq modb-entity-default-cache-internal
	      (luna-make-entity modb-entity-default-handler)))))

(luna-define-generic modb-entity-handler-list-parameters (handler)
  "Return a parameter list of HANDLER.")

(luna-define-generic elmo-msgdb-make-message-entity (handler &rest args)
  "Make a message entity using HANDLER.")

(luna-define-generic elmo-msgdb-message-entity-number (handler entity)
  "Number of the ENTITY.")

(luna-define-generic elmo-msgdb-message-entity-set-number (handler
							   entity number)
  "Set number of the ENTITY.")

(luna-define-generic elmo-msgdb-message-entity-field (handler entity field
							      &optional type)
  "Retrieve field value of the message entity.
HANDLER is the message entity handler.
ENTITY is the message entity structure.
FIELD is a symbol of the field.
If optional argument TYPE is specified, return converted value.")

(luna-define-generic elmo-msgdb-message-entity-set-field (handler
							  entity field value)
  "Set the field value of the message entity.
HANDLER is the message entity handler.
ENTITY is the message entity structure.
FIELD is a symbol of the field.
VALUE is the field value to set.")

(luna-define-generic elmo-msgdb-message-entity-update-fields (handler
							      entity values)
  "Update message entity by VALUES.
HANDLER is the message entity handler.
ENTITY is the message entity structure.
VALUES is an alist of field-name and field-value.")

(luna-define-generic elmo-msgdb-copy-message-entity (handler entity
							     &optional
							     make-handler)
  "Copy message entity.
HANDLER is the message entity handler.
ENTITY is the message entity structure.
If optional argument MAKE-HANDLER is specified, use it to make new entity.")

(luna-define-generic elmo-msgdb-create-message-entity-from-file (handler
								 number
								 file)
  "Create message entity from file.
HANDLER is the message entity handler.
NUMBER is the number of the newly created message entity.
FILE is the message file.")

(luna-define-generic elmo-msgdb-create-message-entity-from-header (handler
								   number
								   &rest args)
  "Create message entity from current buffer.
HANDLER is the message entity handler.
NUMBER is the number of the newly created message entity.
Rest of the ARGS is a plist of message entity field for initial value.
Header region is supposed to be narrowed.")

;; Transitional interface.
(luna-define-generic elmo-msgdb-message-match-condition (handler
							 condition
							 entity)
  "Return non-nil when the entity matches the condition.")

;; Generic implementation.
(luna-define-method initialize-instance :after ((handler modb-entity-handler)
						&rest init-args)
  (unless (modb-entity-handler-mime-charset-internal handler)
    (modb-entity-handler-set-mime-charset-internal handler elmo-mime-charset))
  handler)

(luna-define-method modb-entity-handler-list-parameters
  ((handler modb-entity-handler))
  (list 'mime-charset))

(luna-define-method elmo-msgdb-create-message-entity-from-file
  ((handler modb-entity-handler) number file)
  (when (file-exists-p file)
    (with-temp-buffer
      (setq buffer-file-name file)
      ;; insert header from file.
      (catch 'done
	(condition-case nil
	    (elmo-msgdb-insert-file-header file)
	  (error (setq buffer-file-name nil)
		 (throw 'done nil)))
	(prog1
	    (elmo-msgdb-create-message-entity-from-header
	     handler number)
	  (setq buffer-file-name nil))))))

(luna-define-method elmo-msgdb-make-message-entity ((handler
						     modb-entity-handler)
						    args)
  (cons handler args))

(luna-define-method elmo-msgdb-message-entity-field ((handler
						     modb-entity-handler)
						     entity field
						     &optional type)
  (plist-get (cdr entity) (intern (concat ":" (symbol-name field)))))

(luna-define-method elmo-msgdb-message-entity-number ((handler
						       modb-entity-handler)
						      entity)
  (plist-get (cdr entity) :number))

(luna-define-method elmo-msgdb-message-entity-update-fields
  ((handler modb-entity-handler) entity values)
  (let (updated)
    (dolist (pair values)
      (unless (equal
	       (cdr pair)
	       (elmo-msgdb-message-entity-field handler entity (car pair)))
	(elmo-msgdb-message-entity-set-field handler entity
					     (car pair) (cdr pair))
	(setq updated t)))
    updated))

;; helper functions
(defsubst modb-entity-handler-mime-charset (handler)
  (or (modb-entity-handler-mime-charset-internal handler)
      elmo-mime-charset))

(defun modb-entity-handler-equal-p (handler other)
  "Return non-nil, if OTHER hanlder is equal this HANDLER."
  (and (eq (luna-class-name handler)
	   (luna-class-name other))
       (catch 'mismatch
	 (dolist (slot (modb-entity-handler-list-parameters handler))
	   (when (not (equal (luna-slot-value handler slot)
			     (luna-slot-value other slot)))
	     (throw 'mismatch nil)))
	 t)))

(defun modb-entity-handler-dump-parameters (handler)
  "Return parameters for reconstruct HANDLER as plist."
  (apply #'nconc
	 (mapcar (lambda (slot)
		   (let ((value (luna-slot-value handler slot)))
		     (when value
		       (list (intern (concat ":" (symbol-name slot)))
			     value))))
	 (modb-entity-handler-list-parameters handler))))

;; field in/out converter
(defun modb-set-field-converter (converter type &rest specs)
  "Set convert function of TYPE into CONVERTER.
SPECS must be like `FIELD1 FUNCTION1 FIELD2 FUNCTION2 ...'.
If each field is t, function is set as default converter."
  (when specs
    (let ((alist (symbol-value converter))
	  (type (or type t)))
      (while specs
	(let ((field (pop specs))
	      (function (pop specs))
	      cell)
	  (if (setq cell (assq type alist))
	      (setcdr cell (put-alist field function (cdr cell)))
	    (setq cell  (cons type (list (cons field function)))
		  alist (cons cell alist)))
	  ;; support colon keyword (syntax sugar).
	  (unless (or (eq field t)
		      (string-match "^:" (symbol-name field)))
	    (setcdr cell (put-alist (intern (concat ":" (symbol-name field)))
				    function
				    (cdr cell))))))
      (set converter alist))))
(put 'modb-set-field-converter 'lisp-indent-function 2)

(defsubst modb-convert-field-value (converter field value &optional type)
  (and value
       (let* ((alist (cdr (assq (or type t) converter)))
	      (function (cdr (or (assq field alist)
				 (assq t alist)))))
	 (if function
	     (funcall function field value)
	   value))))

;; mime decode cache
(defvar elmo-msgdb-decoded-cache-hashtb nil)
(make-variable-buffer-local 'elmo-msgdb-decoded-cache-hashtb)

(defsubst elmo-msgdb-get-decoded-cache (string)
  (if elmo-use-decoded-cache
      (let ((hashtb (or elmo-msgdb-decoded-cache-hashtb
			(setq elmo-msgdb-decoded-cache-hashtb
			      (elmo-make-hash 2048))))
	    decoded)
	(or (elmo-get-hash-val string hashtb)
	    (prog1
		(setq decoded
		      (elmo-mime-charset-decode-string
		       string elmo-mime-charset))
	      (elmo-set-hash-val string decoded hashtb))))
    (elmo-mime-charset-decode-string string elmo-mime-charset)))

(defun modb-entity-string-decoder (field value)
  (elmo-msgdb-get-decoded-cache value))

(defun modb-entity-string-encoder (field value)
  (elmo-mime-charset-encode-string value elmo-mime-charset))

(defun modb-entity-parse-date-string (field value)
  (if (stringp value)
      (elmo-time-parse-date-string value)
    value))

(defun modb-entity-make-date-string (field value)
  (if (stringp value)
      value
    (elmo-time-make-date-string value)))

(defun modb-entity-mime-decoder (field value)
  (mime-decode-field-body value (symbol-name field) 'summary))

(defun modb-entity-mime-encoder (field value)
  (mime-encode-field-body value (symbol-name field)))

(defun modb-entity-address-list-decoder (field value)
  (if (stringp value)
      (mapcar (lambda (address)
		(mime-decode-field-body address (symbol-name field)))
	      (elmo-parse-addresses value))
    value))

(defun modb-entity-address-list-encoder (field value)
  (if (stringp value)
      value
    (mime-encode-field-body (mapconcat 'identity value ", ")
			    (symbol-name field))))

(defun modb-entity-parse-address-string (field value)
  (modb-entity-encode-string-recursive
   field
   (if (stringp value)
       (elmo-parse-addresses value)
     value)))

(defun modb-entity-make-address-string (field value)
  (let ((value (modb-entity-decode-string-recursive field value)))
    (if (stringp value)
	value
      (mapconcat 'identity value ", "))))

(defun modb-entity-decode-string-recursive (field value)
  (elmo-map-recursive
   (lambda (element)
     (if (stringp element)
	 (elmo-msgdb-get-decoded-cache element)
       element))
   value))

(defun modb-entity-encode-string-recursive (field value)
  (elmo-map-recursive
   (lambda (element)
     (if (stringp element)
	 (elmo-mime-charset-encode-string element elmo-mime-charset)
       element))
   value))

(defun modb-entity-create-field-indices (slots)
  (let ((index 0)
	indices)
    (while slots
      (setq indices (cons (cons (car slots) index) indices)
	    index   (1+ index)
	    slots   (cdr slots)))
    (append
     indices
     (mapcar (lambda (cell)
	       (cons (intern (concat ":" (symbol-name (car cell))))
		     (cdr cell)))
	     indices))))


;; Legacy implementation.
(eval-and-compile
  (luna-define-class modb-legacy-entity-handler (modb-entity-handler)))

(defconst modb-legacy-entity-field-slots
 '(number
   references
   from
   subject
   date
   to
   cc
   size
   extra))

(defconst modb-legacy-entity-field-indices
  (modb-entity-create-field-indices modb-legacy-entity-field-slots))

(defvar modb-legacy-entity-normalizer nil)
(modb-set-field-converter 'modb-legacy-entity-normalizer nil
  'message-id	nil
  'number	nil
  'references	nil
  'from		#'modb-entity-string-encoder
  'subject	#'modb-entity-string-encoder
  'date		#'modb-entity-make-date-string
  'to		#'modb-entity-address-list-encoder
  'cc		#'modb-entity-address-list-encoder
  'size		nil
  t		#'modb-entity-mime-encoder)

(defvar modb-legacy-entity-specializer nil)
;; default type
(modb-set-field-converter 'modb-legacy-entity-specializer nil
  'message-id	nil
  'number	nil
  'references	nil
  'from		#'modb-entity-string-decoder
  'subject	#'modb-entity-string-decoder
  'date		#'modb-entity-parse-date-string
  'to		#'modb-entity-address-list-decoder
  'cc		#'modb-entity-address-list-decoder
  'size		nil
  t		#'modb-entity-mime-decoder)
;; string type
(modb-set-field-converter 'modb-legacy-entity-specializer 'string
  'message-id	nil
  'number	nil			; not supported
  'references	nil
  'from		#'modb-entity-string-decoder
  'subject	#'modb-entity-string-decoder
  'date		nil
  'size		nil			; not supported
  t		#'modb-entity-mime-decoder)


(defmacro modb-legacy-entity-field-index (field)
  `(cdr (assq ,field modb-legacy-entity-field-indices)))

(defsubst modb-legacy-entity-set-field (entity field value &optional as-is)
  (when entity
    (let (index)
      (unless as-is
	(setq value (modb-convert-field-value
		     modb-legacy-entity-normalizer
		     field value)))
      (cond ((memq field '(message-id :message-id))
	     (setcar entity value))
	    ((setq index (modb-legacy-entity-field-index field))
	     (aset (cdr entity) index value))
	    (t
	     (setq index (modb-legacy-entity-field-index :extra))
	     (let ((extras (and entity (aref (cdr entity) index)))
		   extra)
	       (if (setq extra (assoc (symbol-name field) extras))
		   (setcdr extra value)
		 (aset (cdr entity) index (cons (cons (symbol-name field)
						      value) extras)))))))))

(defsubst modb-legacy-make-message-entity (args)
  "Make an message entity."
  (let ((entity (cons nil (make-vector 9 nil)))
	field value)
    (while args
      (setq field (pop args)
	    value (pop args))
      (when value
	(modb-legacy-entity-set-field entity field value)))
    entity))

(luna-define-method elmo-msgdb-make-message-entity
  ((handler modb-legacy-entity-handler) args)
  (modb-legacy-make-message-entity args))

(luna-define-method elmo-msgdb-create-message-entity-from-header
  ((handler modb-legacy-entity-handler) number args)
  (let ((extras elmo-msgdb-extra-fields)
	(default-mime-charset default-mime-charset)
	entity message-id references from subject to cc date
	extra field-body charset size file-attrib)
    (save-excursion
      (setq entity (modb-legacy-make-message-entity args))
      (set-buffer-multibyte default-enable-multibyte-characters)
      (setq message-id (elmo-msgdb-get-message-id-from-header))
      (and (setq charset (cdr (assoc "charset" (mime-read-Content-Type))))
	   (setq charset (intern-soft charset))
	   (setq default-mime-charset charset))
      (setq references
	    (elmo-msgdb-get-references-from-header)
	    from (elmo-replace-in-string
		  (elmo-mime-string (or (std11-fetch-field "from")
					elmo-no-from))
		  "\t" " ")
	    subject (elmo-replace-in-string
		     (elmo-mime-string (or (std11-fetch-field "subject")
					   elmo-no-subject))
		     "\t" " ")
	    date (or (elmo-decoded-fetch-field "date")
		     (when buffer-file-name
		       (timezone-make-date-arpa-standard
			(current-time-string
			 (nth 5 (or file-attrib
				    (setq file-attrib
					  (file-attributes buffer-file-name)))))
			(current-time-zone))))
	    to   (mapconcat 'identity (elmo-multiple-field-body "to") ",")
	    cc   (mapconcat 'identity (elmo-multiple-field-body "cc") ","))
      (unless (elmo-msgdb-message-entity-field handler entity 'size)
	(setq size
	      (or (std11-fetch-field "content-length")
		  (when buffer-file-name
		    (nth 7 (or file-attrib
			       (setq file-attrib
				     (file-attributes buffer-file-name)))))
		  0))
	(when (stringp size)
	  (setq size (string-to-number size))))
      (while extras
	(if (setq field-body (std11-fetch-field (car extras)))
	    (modb-legacy-entity-set-field
	     entity (intern (downcase (car extras))) field-body 'as-is))
	(setq extras (cdr extras)))
      (dolist (field '(message-id number references from subject
				  date to cc size))
	(when (symbol-value field)
	  (modb-legacy-entity-set-field
	   entity field (symbol-value field) 'as-is)))
      entity)))

(luna-define-method elmo-msgdb-message-entity-number
  ((handler modb-legacy-entity-handler) entity)
  (and entity (aref (cdr entity) 0)))

(luna-define-method elmo-msgdb-message-entity-set-number
  ((handler modb-legacy-entity-handler) entity number)
  (and entity (aset (cdr entity) 0 number)))

(luna-define-method elmo-msgdb-message-entity-field
  ((handler modb-legacy-entity-handler) entity field &optional type)
  (and entity
       (let (index)
	 (modb-convert-field-value
	  modb-legacy-entity-specializer
	  field
	  (cond ((memq field '(message-id :message-id))
		 (car entity))
		((setq index (modb-legacy-entity-field-index field))
		 (aref (cdr entity) index))
		(t
		 (setq index (modb-legacy-entity-field-index :extra))
		 (cdr (assoc (symbol-name field)
			     (aref (cdr entity) index)))))
	  type))))

(luna-define-method elmo-msgdb-message-entity-set-field
  ((handler modb-legacy-entity-handler) entity field value)
  (modb-legacy-entity-set-field entity field value))

(luna-define-method elmo-msgdb-copy-message-entity
  ((handler modb-legacy-entity-handler) entity &optional make-handler)
  (if make-handler
      (let ((copy (elmo-msgdb-make-message-entity make-handler)))
	(dolist (field (append '(message-id number references from subject
					    date to cc size)
			       (mapcar (lambda (extra) (intern (car extra)))
				       (aref (cdr entity) 8))))
	  (elmo-msgdb-message-entity-set-field
	   make-handler copy field
	   (elmo-msgdb-message-entity-field handler entity field)))
	copy)
    (cons (car entity)
	  (copy-sequence (cdr entity)))))

(luna-define-method elmo-msgdb-message-match-condition
  ((handler modb-entity-handler) condition entity)
  (let ((key (elmo-filter-key condition))
	(case-fold-search t)
	field-value)
    (cond
     ((or (string= key "since")
	  (string= key "before"))
      (let ((field-date (elmo-msgdb-message-entity-field
			 handler entity 'date))
	    (specified-date
	     (elmo-datevec-to-time
	      (elmo-date-get-datevec
	       (elmo-filter-value condition)))))
	(if (string= key "since")
	    (not (elmo-time< field-date specified-date))
	  (elmo-time< field-date specified-date))))
     ((or (string= key "larger")
	  (string= key "smaller"))
      (let ((bytes (elmo-msgdb-message-entity-field handler entity 'size))
	    (threshold (string-to-number (elmo-filter-value condition))))
	(if (string= key "larger")
	    (> bytes threshold)
	  (< bytes threshold))))
     ((setq field-value (elmo-msgdb-message-entity-field handler
							 entity
							 (intern key)
							 'string))
      (and (stringp field-value)
	   (string-match (elmo-filter-value condition) field-value)))
     (t
      condition))))


;; Standard implementation.
(eval-and-compile
  (luna-define-class modb-standard-entity-handler (modb-entity-handler)))

(defconst modb-standard-entity-field-slots
  '(number
    from
    subject
    date
    to
    cc
    content-type
    references
    size
    score
    extra))

(defconst modb-standard-entity-field-indices
  (modb-entity-create-field-indices modb-standard-entity-field-slots))

(defvar modb-standard-entity-normalizer nil)
(modb-set-field-converter 'modb-standard-entity-normalizer nil
  'message-id	nil
  'number	nil
  'date		#'modb-entity-parse-date-string
  'to		#'modb-entity-parse-address-string
  'cc		#'modb-entity-parse-address-string
  'references	nil
  'size		nil
  'score	nil
  t		#'modb-entity-encode-string-recursive)

(defvar modb-standard-entity-specializer nil)
(modb-set-field-converter 'modb-standard-entity-specializer nil
  'message-id	nil
  'number	nil
  'date		nil
  'references	nil
  'size		nil
  'score	nil
  t		#'modb-entity-decode-string-recursive)
(modb-set-field-converter 'modb-standard-entity-specializer 'string
  'message-id	nil
  'number	nil
  'date		#'modb-entity-make-date-string
  'to		#'modb-entity-make-address-string
  'cc		#'modb-entity-make-address-string
  'references	nil
  'size		nil
  'score	nil
  'ml-info	#'modb-entity-make-mailing-list-info-string
  t		#'modb-entity-decode-string-recursive)

(defmacro modb-standard-entity-field-index (field)
  `(cdr (assq ,field modb-standard-entity-field-indices)))

(defsubst modb-standard-entity-set-field (entity field value &optional as-is)
  (when entity
    (let (index)
      (unless as-is
	(let ((elmo-mime-charset
	       (modb-entity-handler-mime-charset (car entity))))
	  (setq value (modb-convert-field-value modb-standard-entity-normalizer
						field value))))
      (cond ((memq field '(message-id :message-id))
	     (setcar (cdr entity) value))
	    ((setq index (modb-standard-entity-field-index field))
	     (aset (cdr (cdr entity)) index value))
	    (t
	     (setq index (modb-standard-entity-field-index :extra))
	     (let ((extras (aref (cdr (cdr entity)) index))
		   cell)
	       (if (setq cell (assq field extras))
		   (setcdr cell value)
		 (aset (cdr (cdr entity))
		       index
		       (cons (cons field value) extras)))))))))

(defsubst modb-standard-make-message-entity (handler args)
  (let ((entity (cons handler
		      (cons nil
			    (make-vector
			     (length modb-standard-entity-field-slots)
			     nil))))
	field value)
    (while args
      (setq field (pop args)
	    value (pop args))
      (when value
	(modb-standard-entity-set-field entity field value)))
    entity))

(luna-define-method elmo-msgdb-make-message-entity
  ((handler modb-standard-entity-handler) args)
  (modb-standard-make-message-entity handler args))

(luna-define-method elmo-msgdb-message-entity-number
  ((handler modb-standard-entity-handler) entity)
  (and entity (aref (cdr (cdr entity)) 0)))

(luna-define-method elmo-msgdb-message-entity-set-number
  ((handler modb-standard-entity-handler) entity number)
  (and entity (aset (cdr (cdr entity)) 0 number)))

(luna-define-method elmo-msgdb-message-entity-field
  ((handler modb-standard-entity-handler) entity field &optional type)
  (and entity
       (let ((elmo-mime-charset
	      (modb-entity-handler-mime-charset handler))
	     index)
	 (modb-convert-field-value
	  modb-standard-entity-specializer
	  field
	  (cond ((memq field '(message-id :message-id))
		 (car (cdr entity)))
		((setq index (modb-standard-entity-field-index field))
		 (aref (cdr (cdr entity)) index))
		(t
		 (setq index (modb-standard-entity-field-index :extra))
		 (cdr (assq field (aref (cdr (cdr entity)) index)))))
	  type))))

(luna-define-method elmo-msgdb-message-entity-set-field
  ((handler modb-standard-entity-handler) entity field value)
  (modb-standard-entity-set-field entity field value))

(luna-define-method elmo-msgdb-copy-message-entity
  ((handler modb-standard-entity-handler) entity &optional make-handler)
  (if make-handler
      (let ((copy (elmo-msgdb-make-message-entity make-handler)))
	(dolist (field (nconc
			(delq 'extra
			      (copy-sequence modb-standard-entity-field-slots))
			(mapcar 'car
				(aref
				 (cdr (cdr entity))
				 (modb-standard-entity-field-index :extra)))
			'(message-id)))
	  (elmo-msgdb-message-entity-set-field
	   make-handler copy field
	   (elmo-msgdb-message-entity-field handler entity field)))
	copy)
    (cons handler
	  (cons (car (cdr entity))
		(copy-sequence (cdr (cdr entity)))))))

(luna-define-method elmo-msgdb-create-message-entity-from-header
  ((handler modb-standard-entity-handler) number args)
  (let (entity size field-name field-body extractor file-attrib)
    (save-excursion
      (set-buffer-multibyte default-enable-multibyte-characters)
      (setq entity
	    (modb-standard-make-message-entity
	     handler
	     (append
	      args
	      (list
	       :number
	       number
	       :message-id
	       (elmo-msgdb-get-message-id-from-header)
	       :references
	       (elmo-msgdb-get-references-from-header)
	       :from
	       (elmo-replace-in-string
		(or (elmo-decoded-fetch-field "from" 'summary)
		    elmo-no-from)
		"\t" " ")
	       :subject
	       (elmo-replace-in-string
		(or (elmo-decoded-fetch-field "subject" 'summary)
		    elmo-no-subject)
		"\t" " ")
	       :date
	       (or (elmo-decoded-fetch-field "date" 'summary)
		   (when buffer-file-name
		     (timezone-make-date-arpa-standard
		      (current-time-string
		       (nth 5 (or file-attrib
				  (setq file-attrib
					(file-attributes buffer-file-name)))))
		      (current-time-zone))))
	       :to
	       (mapconcat
		(lambda (field-body)
		  (mime-decode-field-body field-body "to" 'summary))
		(elmo-multiple-field-body "to") ",")
	       :cc
	       (mapconcat
		(lambda (field-body)
		  (mime-decode-field-body field-body "cc" 'summary))
		(elmo-multiple-field-body "cc") ",")
	       :content-type
	       (elmo-decoded-fetch-field "content-type" 'summary)
	       :size
	       (if (setq size (std11-fetch-field "content-length"))
		   (string-to-number size)
		 (or (plist-get args :size)
		     (when buffer-file-name
		       (nth 7 (or file-attrib
				  (setq file-attrib
					(file-attributes buffer-file-name)))))
		     0))))))
      (dolist (extra (cons "newsgroups"
			   (remove "newsgroups" elmo-msgdb-extra-fields)))
	(unless (memq (setq field-name (intern (downcase extra)))
		      '(number message-id references from subject
			       date to cc content-type size))
	  (setq extractor  (nth 1 (assq field-name
					modb-entity-field-extractor-alist))
		field-body (if extractor
			       (funcall extractor field-name)
			     (elmo-decoded-fetch-field extra 'summary)))
	  (when field-body
	    (modb-standard-entity-set-field entity field-name field-body))))
      entity)))


;; mailing list info handling
(defun modb-entity-extract-mailing-list-info (field)
  (let* ((getter (lambda (field)
		   (elmo-decoded-fetch-field (symbol-name field) 'summary)))
	 (name (elmo-find-list-match-value
		elmo-mailing-list-name-spec-list
		getter))
	 (count (elmo-find-list-match-value
		  elmo-mailing-list-count-spec-list
		  getter)))
    (when (or name count)
      (cons name (and count (string-to-number count))))))

(defun modb-entity-ml-info-real-fields (field)
  (elmo-uniq-list
   (mapcar (lambda (entry)
	     (symbol-name (if (consp entry) (car entry) entry)))
	   (append elmo-mailing-list-name-spec-list
		   elmo-mailing-list-count-spec-list))))

(defun modb-entity-make-mailing-list-info-string (field value)
  (when (car value)
    (format (if (cdr value) "(%s %05.0f)" "(%s)")
	    (elmo-msgdb-get-decoded-cache (car value))
	    (cdr value))))

;; message buffer handler
(eval-and-compile
  (luna-define-class modb-buffer-entity-handler (modb-entity-handler)))

(defvar modb-buffer-entity-specializer nil)
(modb-set-field-converter 'modb-buffer-entity-specializer nil
  'date	#'elmo-time-parse-date-string)

(luna-define-method elmo-msgdb-make-message-entity
  ((handler modb-buffer-entity-handler) args)
  (cons handler (cons (or (plist-get args :number)
			  (plist-get args 'number))
		      (or (plist-get args :buffer)
			  (plist-get args 'buffer)
			  (current-buffer)))))

(luna-define-method elmo-msgdb-message-entity-number
  ((handler modb-buffer-entity-handler) entity)
  (car (cdr entity)))

(luna-define-method elmo-msgdb-message-entity-set-number
  ((handler modb-buffer-entity-handler) entity number)
  (and entity (setcar (cdr entity) number)))

(luna-define-method elmo-msgdb-message-entity-field
  ((handler modb-buffer-entity-handler) entity field &optional type)
  (and entity
       (let ((elmo-mime-charset
	      (modb-entity-handler-mime-charset handler)))
	 (modb-convert-field-value
	  modb-buffer-entity-specializer
	  field
	  (if (memq field '(number :number))
	      (car (cdr entity))
	    (with-current-buffer (cdr (cdr entity))
	      (let ((extractor
		     (nth 1 (assq field modb-entity-field-extractor-alist))))
		(if extractor
		    (funcall extractor field)
		  (mapconcat
		   (lambda (field-body)
		     (mime-decode-field-body field-body (symbol-name field)
					     'summary))
		   (elmo-multiple-field-body (symbol-name field))
		   "\n")))))
	  type))))

(luna-define-method elmo-msgdb-message-match-condition :around
  ((handler modb-buffer-entity-handler) condition entity)
  (let ((key (elmo-filter-key condition))
	(case-fold-search t))
    (cond
     ((string= (elmo-filter-key condition) "body")
      (modb-entity-match-entity-body
       (regexp-quote (elmo-filter-value condition))
       (mime-parse-buffer (cdr (cdr entity)))))
     ((string= (elmo-filter-key condition) "raw-body")
      (with-current-buffer (cdr (cdr entity))
	(decode-coding-region (point-min) (point-max)
	 		      elmo-mime-display-as-is-coding-system)
	(goto-char (point-min))
	(and (re-search-forward "^$" nil t)	   ; goto body
	     (search-forward (elmo-filter-value condition) nil t))))
     (t
      (luna-call-next-method)))))

(defun modb-entity-match-entity-body (regexp mime-entity)
  (let ((content-type (mime-entity-content-type mime-entity))
	children result)
    (cond
     ((setq children (mime-entity-children mime-entity))
      (while children
	(when (modb-entity-match-entity-body regexp (car children))
	  (setq result t
		children nil))
	(setq children (cdr children)))
      result)
     ((eq (mime-content-type-primary-type content-type) 'text)
      (string-match regexp
		    (elmo-mime-charset-decode-string
		     (mime-entity-content mime-entity)
		     (or (mime-content-type-parameter content-type "charset")
			 default-mime-charset)
		     'CRLF)))
     (t nil))))

(require 'product)
(product-provide (provide 'modb-entity) (require 'elmo-version))

;;; modb-entity.el ends here
