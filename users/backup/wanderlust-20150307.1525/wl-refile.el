;;; wl-refile.el --- Refile modules for Wanderlust.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news

;; This file is part of Wanderlust (Yet Another Message Interface on Emacsen).

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

(require 'wl-vars)
(require 'wl-util)

(defvar wl-refile-alist nil)
(defvar wl-refile-alist-file-name "refile-alist")
;; should be renamed to "refile-from-alist"
(defvar wl-refile-msgid-alist nil)
(defvar wl-refile-msgid-alist-file-name "refile-msgid-alist")
(defvar wl-refile-subject-alist nil)
(defvar wl-refile-subject-alist-file-name "refile-subject-alist")

(defvar wl-refile-default-from-folder-path-separator "/")

(defvar wl-refile-alist-max-length 1000)

(defun wl-refile-alist-setup ()
  (let ((flist wl-refile-guess-functions))
    (while flist
      (cond
       ((eq (car flist) 'wl-refile-guess-by-history)
	(setq wl-refile-alist
	      (elmo-object-load
	       (expand-file-name wl-refile-alist-file-name
				 elmo-msgdb-directory) elmo-mime-charset)))
       ((eq (car flist) 'wl-refile-guess-by-msgid)
	(setq wl-refile-msgid-alist
	      (elmo-object-load
	       (expand-file-name wl-refile-msgid-alist-file-name
				 elmo-msgdb-directory) elmo-mime-charset)))
       ((eq (car flist) 'wl-refile-guess-by-subject)
	(setq wl-refile-subject-alist
	      (elmo-object-load
	       (expand-file-name wl-refile-subject-alist-file-name
				 elmo-msgdb-directory) elmo-mime-charset))))
      (setq flist (cdr flist)))))

(defun wl-refile-alist-save ()
  (if wl-refile-alist
      (wl-refile-alist-save-file
       wl-refile-alist-file-name wl-refile-alist))
  (if wl-refile-msgid-alist
      (wl-refile-alist-save-file
       wl-refile-msgid-alist-file-name wl-refile-msgid-alist))
  (if wl-refile-subject-alist
      (wl-refile-alist-save-file
       wl-refile-subject-alist-file-name wl-refile-subject-alist)))

(defun wl-refile-alist-save-file (file-name alist)
  (if (> (length alist) wl-refile-alist-max-length)
      (setcdr (nthcdr (1- wl-refile-alist-max-length) alist) nil))
  (elmo-object-save (expand-file-name file-name elmo-msgdb-directory)
		    alist elmo-mime-charset))

(defun wl-refile-learn (entity dst)
  (let (tocc-list from key hit ml)
    (setq dst (elmo-string dst))
    (setq tocc-list
	  (mapcar (lambda (entity)
		    (downcase (wl-address-header-extract-address entity)))
		  (append
		   (elmo-message-entity-field entity 'to)
		   (elmo-message-entity-field entity 'cc))))
    (while tocc-list
      (if (wl-string-member
	   (car tocc-list)
	   (mapcar (function downcase) wl-subscribed-mailing-list))
	  (setq ml (car tocc-list)
		tocc-list nil)
	(setq tocc-list (cdr tocc-list))))
    (if ml
	(setq key ml) ; subscribed entity!!
      (or (wl-address-user-mail-address-p
	   (setq from
		 (downcase
		  (wl-address-header-extract-address
		   (elmo-message-entity-field entity 'from)))))
	  (setq key from))
      (if (or wl-refile-msgid-alist
	      (memq 'wl-refile-guess-by-msgid
		    wl-refile-guess-functions))
	  (wl-refile-msgid-learn entity dst))
      (if (or wl-refile-subject-alist
	      (memq 'wl-refile-guess-by-subject
		    wl-refile-guess-functions))
	  (wl-refile-subject-learn entity dst)))
    (when key
      (if (setq hit (assoc key wl-refile-alist))
	  (setq wl-refile-alist (delq hit wl-refile-alist)))
      (setq wl-refile-alist (cons (cons key dst)
				  wl-refile-alist)))))

(defun wl-refile-msgid-learn (entity dst)
  (let ((key (elmo-message-entity-field entity 'message-id))
	hit)
    (setq dst (elmo-string dst))
    (if key
	(if (setq hit (assoc key wl-refile-msgid-alist))
	    (setcdr hit dst)
	  (setq wl-refile-msgid-alist (cons (cons key dst)
					    wl-refile-msgid-alist))))))

(defun wl-refile-subject-learn (entity dst)
  (let ((subject (funcall wl-summary-subject-filter-function
			  (elmo-message-entity-field entity 'subject)))
	hit)
    (setq dst (elmo-string dst))
    (if (and subject (not (string= subject "")))
	(if (setq hit (assoc subject wl-refile-subject-alist))
	    (setcdr hit dst)
	  (setq wl-refile-subject-alist (cons (cons subject dst)
					    wl-refile-subject-alist))))))

;;
;; refile guess
;;
(defvar wl-refile-guess-functions
  '(wl-refile-guess-by-rule
    wl-refile-guess-by-msgid
    wl-refile-guess-by-subject
    wl-refile-guess-by-history
    wl-refile-guess-by-from)
  "*Functions in this list are used for guessing refile destination folder.")

;; 2000-11-05: *-func-list -> *-functions
(elmo-define-obsolete-variable 'wl-refile-guess-func-list
			       'wl-refile-guess-functions)

(defun wl-refile-guess (entity &optional functions)
  (let ((flist (or functions wl-refile-guess-functions))
	guess)
    (while flist
      (if (setq guess (funcall (car flist) entity))
	  (setq flist nil)
	(setq flist (cdr flist))))
    guess))

(defun wl-refile-evaluate-rule (rule entity)
  "Return folder string if RULE is matched to ENTITY.
If RULE does not match ENTITY, returns nil."
  (let ((case-fold-search t)
	fields guess pairs value)
    (cond
     ((stringp rule) rule)
     ((listp (car rule))
      (setq fields (car rule))
      (while fields
	(if (setq guess (wl-refile-evaluate-rule (append (list (car fields))
							 (cdr rule))
						 entity))
	    (setq fields nil)
	  (setq fields (cdr fields))))
      guess)
     ((stringp (car rule))
      (setq pairs (cdr rule))
      (setq value (wl-refile-get-field-value entity (car rule)))
      (while pairs
	(if (and (stringp value)
		 (string-match
		  (car (car pairs))
		  value)
		 (setq guess (wl-expand-newtext
			      (wl-refile-evaluate-rule (cdr (car pairs))
						       entity)
			      value)))
	    (setq pairs nil)
	  (setq pairs (cdr pairs))))
      guess)
     (t (error "Invalid structure for wl-refile-rule-alist")))))

(defun wl-refile-get-field-value (entity field)
  "Get FIELD value from ENTITY."
  (elmo-message-entity-field entity (intern (downcase field)) 'string))

(defun wl-refile-guess-by-rule (entity)
  (let ((rules wl-refile-rule-alist)
	guess)
    (while rules
      (if (setq guess (wl-refile-evaluate-rule (car rules) entity))
	  (setq rules nil)
	(setq rules (cdr rules))))
    guess))

(defun wl-refile-guess-by-history (entity)
  (let ((tocc-list
	 (mapcar (lambda (entity)
		   (downcase (wl-address-header-extract-address entity)))
		 (append
		  (elmo-message-entity-field entity 'to)
		  (elmo-message-entity-field entity 'cc))))
	ret-val)
    (setq tocc-list (wl-address-delete-user-mail-addresses tocc-list))
    (while tocc-list
      (if (setq ret-val (cdr (assoc (car tocc-list) wl-refile-alist)))
	  (setq tocc-list nil)
	(setq tocc-list (cdr tocc-list))))
    ret-val))

(defun wl-refile-get-account-part-from-address (address)
  (if (string-match "\\([^@]+\\)@[^@]+" address)
      (wl-match-string 1 address)
    address))

(defun wl-refile-guess-by-from (entity)
  (let ((from (downcase (wl-address-header-extract-address
			 (elmo-message-entity-field entity 'from))))
	(folder (elmo-make-folder wl-refile-default-from-folder))
	(elmo-path-sep wl-refile-default-from-folder-path-separator))
    ;; search from alist
    (or (cdr (assoc from wl-refile-alist))
	(concat
	 (elmo-folder-prefix-internal folder)
	 (elmo-concat-path
	  (substring wl-refile-default-from-folder
		     (length (elmo-folder-prefix-internal folder)))
	  (wl-refile-get-account-part-from-address from))))))

(defun wl-refile-guess-by-msgid (entity)
  (let ((references (elmo-message-entity-field entity 'references))
	result)
    ;; In old msgdb, references's field is a string.
    (when (stringp references)
      (setq references (list references)))
    (while references
      (setq references
	    (if (setq result
		      (cdr (assoc (car references) wl-refile-msgid-alist)))
		nil
	      (cdr references)))
      result)))

(defun wl-refile-guess-by-subject (entity)
  (cdr (assoc (funcall wl-summary-subject-filter-function
		       (elmo-message-entity-field entity 'subject))
	      wl-refile-subject-alist)))

(require 'product)
(product-provide (provide 'wl-refile) (require 'wl-version))

;;; wl-refile.el ends here
