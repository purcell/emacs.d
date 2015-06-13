;;; mmimap.el --- MIME entity module for IMAP4rev1 (RFC2060).
;;                **** This is EXPERIMENTAL *****

;; Copyright (C) 2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: IMAP, MIME, multimedia, mail, news

;; This file is part of FLIM (Faithful Library about Internet Message).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;

;;; Code:

(require 'mmgeneric)
(require 'mime)
(require 'pces)
(require 'mime-parse)

(eval-and-compile
  (luna-define-class mime-imap-entity (mime-entity)
		     (size header-string body-string new requested))
  (luna-define-internal-accessors 'mime-imap-entity))

;;; @ MIME IMAP location
;;    It should contain server, mailbox and uid (sequence number).
(eval-and-compile
  (luna-define-class mime-imap-location () ()))

(luna-define-generic mime-imap-location-section-body (location section)
  "Return a body string from LOCATION which corresponds to SECTION.
SECTION is a section string which is defined in RFC2060.")

(luna-define-generic mime-imap-location-bodystructure (location)
  "Return a parsed bodystructure of LOCATION.
`NIL' should be converted to nil, `astring' should be converted to a string.")

(luna-define-generic mime-imap-location-fetch-entity-p (location entity)
  "Return non-nil when LOCATION may fetch the ENTITY.")

;;; @ Subroutines
;;

(defun mmimap-entity-section (node-id)
  "Return a section string from NODE-ID"
  (cond
   ((null node-id)
    "1")
   ((numberp node-id)
    (number-to-string (1+ node-id)))
   ((listp node-id)
    (mapconcat
     'mmimap-entity-section
     (reverse node-id)
     "."))))

(eval-and-compile
  (defun-maybe mime-decode-parameters (attrlist)
    (let (ret-val)
      (while attrlist
	(setq ret-val (append ret-val
			      (list (cons (downcase (car attrlist))
					  (car (cdr attrlist))))))
	(setq attrlist (cdr (cdr attrlist))))
      ret-val)))

(defun mmimap-make-mime-entity (bodystructure class location node-id number
					      parent)
  "Analyze parsed IMAP4 BODYSTRUCTURE response and make MIME entity.
CLASS, LOCATION, NODE-ID, PARENT are set to the returned entity."
  (setq node-id (if number (cons number node-id) node-id))
  (cond
   ((listp (car bodystructure)) ; multipart
    (let ((num 0)
	  curp children content-type entity)
      (setq entity
	    (luna-make-entity
	     class
	     :new      t
	     :parent   parent
	     :location location
	     :node-id  node-id))
      (while (and (setq curp (car bodystructure))
		  (listp curp))
	(setq children
	      (nconc children
		     (list
		      (mmimap-make-mime-entity curp class
					       location
					       node-id
					       num
					       entity))))
	(setq num (+ num 1))
	(setq bodystructure (cdr bodystructure)))
      (mime-entity-set-children-internal entity children)
      (mime-entity-set-content-type-internal
       entity
       (make-mime-content-type 'multipart
			       (if (car bodystructure)
				   (intern (downcase
					    (car bodystructure))))
			       (mime-decode-parameters
				(nth 1 bodystructure))))
      entity))
   (t ; singlepart
    (let (content-type entity)
      (setq entity
	    (luna-make-entity
	     class
	     :new  t
	     :size (nth 6 bodystructure)
	     :content-type content-type
	     :location location
	     :parent parent
	     :node-id node-id))
      (mime-entity-set-content-type-internal
       entity
       (make-mime-content-type (intern (downcase (car bodystructure)))
			       (if (nth 1 bodystructure)
				   (intern (downcase
					    (nth 1 bodystructure))))
			       (mime-decode-parameters
				(nth 2 bodystructure))))
      (mime-entity-set-encoding-internal entity
					 (and (nth 5 bodystructure)
					      (downcase
					       (nth 5 bodystructure))))
      (if (and (nth 7 bodystructure)
	       (nth 8 bodystructure))  ; children.
	  (mime-entity-set-children-internal
	   entity
	   (list (mmimap-make-mime-entity
		  (nth 8 bodystructure) class
		  location node-id nil
		  entity))))
      entity))))

(luna-define-method initialize-instance :after ((entity mime-imap-entity)
						&rest init-args)
  ;; To prevent infinite loop...
  (if (mime-imap-entity-new-internal entity)
      entity
    (mmimap-make-mime-entity
     (mime-imap-location-bodystructure
      (mime-entity-location-internal entity))
     (luna-class-name entity)
     (mime-entity-location-internal entity)
     nil nil nil)))

;;; @ entity
;;

(luna-define-method mime-insert-entity ((entity mime-imap-entity))
  (if (mime-root-entity-p entity)
      (progn
	(insert (mime-imap-entity-header-string entity))
	(mime-insert-entity-body entity))
    ;; Insert body if it is not a multipart.
    (unless (eq (mime-content-type-primary-type
		 (mime-entity-content-type entity))
		'multipart)
      (mime-insert-entity-body entity))))

(luna-define-method mime-write-entity ((entity mime-imap-entity) filename)
  (with-temp-buffer
    (mime-insert-entity entity)
    (write-region-as-raw-text-CRLF (point-min) (point-max) filename)))

;;; @ entity body
;;

(luna-define-method mime-entity-body ((entity mime-imap-entity))
  (or (mime-imap-entity-body-string-internal entity)
      (if (or (mime-imap-entity-requested-internal entity) ; second time.
	      (mime-imap-location-fetch-entity-p
	       (mime-entity-location-internal entity)
	       entity))
	  (mime-imap-entity-set-body-string-internal
	   entity
	   (mime-imap-location-section-body
	    (mime-entity-location-internal entity)
	    (mmimap-entity-section
	     (mime-entity-node-id-internal entity))))
	(mime-imap-entity-set-requested-internal entity t)
	"")))

(luna-define-method mime-insert-entity-body ((entity mime-imap-entity))
  (insert (mime-entity-body entity)))

(luna-define-method mime-write-entity-body ((entity mime-imap-entity)
					    filename)
  (with-temp-buffer
    (mime-insert-entity-body entity)
    (write-region-as-binary (point-min) (point-max) filename)))

;;; @ entity content
;;

(luna-define-method mime-entity-content ((entity mime-imap-entity))
  (let ((ret (mime-entity-body entity)))
    (if ret
	(mime-decode-string ret (mime-entity-encoding entity))
      (message "Cannot decode content.")
      nil)))

(luna-define-method mime-insert-entity-content ((entity mime-imap-entity))
  (insert (mime-entity-content entity)))

(luna-define-method mime-write-entity-content ((entity mime-imap-entity)
					       filename)
  (with-temp-buffer
    (mime-insert-entity-body entity)
    (mime-write-decoded-region (point-min) (point-max)
			       filename
			       (or (mime-entity-encoding entity) "7bit"))))

;;; @ header field
;;

(defun mime-imap-entity-header-string (entity)
  (or (mime-imap-entity-header-string-internal entity)
      (mime-imap-entity-set-header-string-internal
       entity
       (mime-imap-location-section-body
	(mime-entity-location-internal entity)
	(if (mime-entity-node-id-internal entity)
	    (concat (mmimap-entity-section
		     (mime-entity-node-id-internal entity))
		    ".HEADER")
	  "HEADER")))))

(luna-define-method mime-entity-fetch-field :around
  ((entity mime-imap-entity) field-name)
  (if (mime-root-entity-p entity)
      (or (luna-call-next-method)
	  (with-temp-buffer
	    (insert (mime-imap-entity-header-string entity))
	    (let ((ret (std11-fetch-field field-name)))
	      (when ret
		(or (symbolp field-name)
		    (setq field-name
			  (intern (capitalize (capitalize field-name)))))
		(mime-entity-set-original-header-internal
		 entity
		 (put-alist field-name ret
			    (mime-entity-original-header-internal entity)))
		ret))))))

(luna-define-method mime-insert-header ((entity mime-imap-entity)
					&optional invisible-fields
					visible-fields)
  (let ((the-buf (current-buffer))
	buf p-min p-max)
    (with-temp-buffer
      (insert (mime-imap-entity-header-string entity))
      (setq buf (current-buffer)
	    p-min (point-min)
	    p-max (point-max))
      (set-buffer the-buf)
      (mime-insert-header-from-buffer buf p-min p-max
				      invisible-fields visible-fields))))

;;; @ end
;;

(provide 'mmimap)

;;; mmimap.el ends here
