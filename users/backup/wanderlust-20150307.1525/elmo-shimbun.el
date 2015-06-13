;;; elmo-shimbun.el --- Shimbun interface for ELMO.

;; Copyright (C) 2001 Yuuichi Teranishi <teranisi@gohome.org>

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
(require 'elmo-map)
(require 'elmo-dop)
(require 'shimbun)

(eval-when-compile
  (require 'cl)
  (defun-maybe shimbun-servers-list ()))

(defcustom elmo-shimbun-check-interval 60
  "*Check interval for shimbun."
  :type 'integer
  :group 'elmo)

(defcustom elmo-shimbun-default-index-range 2
  "*Default value for the range of header indices."
  :type '(choice (const :tag "all" all)
		 (const :tag "last" last)
		 (integer :tag "number"))
  :group 'elmo)

(defcustom elmo-shimbun-use-cache t
  "*If non-nil, use cache for each article."
  :type 'boolean
  :group 'elmo)

(defcustom elmo-shimbun-index-range-alist nil
  "*Alist of FOLDER-REGEXP and RANGE.
FOLDER-REGEXP is the regexp for shimbun folder name.
RANGE is the range of the header indices .
See `shimbun-headers' for more detail about RANGE."
  :type '(repeat (cons (regexp :tag "Folder Regexp")
		       (choice (const :tag "all" all)
			       (const :tag "last" last)
			       (integer :tag "number"))))
  :group 'elmo)

(defcustom elmo-shimbun-update-overview-folder-list 'all
  "*List of FOLDER-REGEXP.
FOLDER-REGEXP is the regexp of shimbun folder name which should be
update overview when message is fetched.
If it is the symbol `all', update overview for all shimbun folders."
  :type '(choice (const :tag "All shimbun folders" all)
		 (repeat (regexp :tag "Folder Regexp")))
  :group 'elmo)

;; Shimbun header.
(defsubst elmo-shimbun-header-extra-field (header field-name)
  (let ((extra (and header (shimbun-header-extra header))))
    (and extra
	 (cdr (assoc field-name extra)))))

(defsubst elmo-shimbun-header-set-extra-field (header field-name value)
  (let ((extras (and header (shimbun-header-extra header)))
	extra)
    (if (setq extra (assoc field-name extras))
	(setcdr extra value)
      (shimbun-header-set-extra
       header
       (cons (cons field-name value) extras)))))

;; Shimbun mua.
(eval-and-compile
  (luna-define-class shimbun-elmo-mua (shimbun-mua) (folder))
  (luna-define-internal-accessors 'shimbun-elmo-mua))

(luna-define-method shimbun-mua-search-id ((mua shimbun-elmo-mua) id)
  (elmo-message-entity (shimbun-elmo-mua-folder-internal mua) id))

(eval-and-compile
  (luna-define-class elmo-shimbun-folder
		     (elmo-map-folder) (shimbun headers header-hash
						entity-hash
						group range last-check))
  (luna-define-internal-accessors 'elmo-shimbun-folder))

(defun elmo-shimbun-folder-entity-hash (folder)
  (or (elmo-shimbun-folder-entity-hash-internal folder)
      (let ((overviews (elmo-folder-list-message-entities folder))
	    hash id)
	(when overviews
	  (setq hash (elmo-make-hash (length overviews)))
	  (dolist (entity overviews)
	    (elmo-set-hash-val (elmo-message-entity-field entity 'message-id)
			       entity hash)
	    (when (setq id (elmo-message-entity-field entity 'x-original-id))
	      (elmo-set-hash-val id entity hash)))
	  (elmo-shimbun-folder-set-entity-hash-internal folder hash)))))

(defsubst elmo-shimbun-folder-shimbun-header (folder location)
  (let ((hash (elmo-shimbun-folder-header-hash-internal folder)))
    (or (and hash (elmo-get-hash-val location hash))
	(let ((entity (elmo-message-entity folder location))
	      (elmo-hash-minimum-size 63)
	      header)
	  (when entity
	    (setq header (elmo-shimbun-entity-to-header entity))
	    (unless hash
	      (elmo-shimbun-folder-set-header-hash-internal
	       folder
	       (setq hash (elmo-make-hash))))
	    (elmo-set-hash-val (elmo-message-entity-field entity 'message-id)
			       header
			       hash)
	    header)))))

(defsubst elmo-shimbun-lapse-seconds (time)
  (let ((now (current-time)))
    (+ (* (- (car now) (car time)) 65536)
       (- (nth 1 now) (nth 1 time)))))

(defsubst elmo-shimbun-headers-check-p (folder)
  (or (null (elmo-shimbun-folder-last-check-internal folder))
      (and (elmo-shimbun-folder-last-check-internal folder)
	   (> (elmo-shimbun-lapse-seconds
	       (elmo-shimbun-folder-last-check-internal folder))
	      elmo-shimbun-check-interval))))

(defun elmo-shimbun-entity-to-header (entity)
  (let (message-id shimbun-id)
    (if (setq message-id (elmo-message-entity-field entity 'x-original-id))
	(setq shimbun-id (elmo-message-entity-field entity 'message-id))
      (setq message-id (elmo-message-entity-field entity 'message-id)
	    shimbun-id nil))
    (elmo-with-enable-multibyte
      (shimbun-create-header
       (elmo-message-entity-number entity)
       (elmo-message-entity-field entity 'subject)
       (elmo-message-entity-field entity 'from)
       (elmo-time-make-date-string
	(elmo-message-entity-field entity 'date))
       message-id
       (elmo-message-entity-field entity 'references)
       (elmo-message-entity-field entity 'size)
       0
       (elmo-message-entity-field entity 'xref)
       (and shimbun-id
	    (list (cons "x-shimbun-id" shimbun-id)))))))

(defsubst elmo-shimbun-folder-header-hash-setup (folder headers)
  (let ((hash (or (elmo-shimbun-folder-header-hash-internal folder)
		  (elmo-make-hash (length headers)))))
    (dolist (header headers)
      (elmo-set-hash-val (shimbun-header-id header) header hash))
    (elmo-shimbun-folder-set-header-hash-internal folder hash)))

(defun elmo-shimbun-get-headers (folder)
  (let* ((shimbun (elmo-shimbun-folder-shimbun-internal folder))
	 (key (concat (shimbun-server shimbun)
		      "." (shimbun-current-group shimbun)))
	 (elmo-hash-minimum-size 63)
	 headers)
    ;; new headers.
    (setq headers
	  (delq nil
		(mapcar
		 (lambda (x)
		   (unless (elmo-message-entity folder (shimbun-header-id x))
		     x))
		 ;; This takes much time.
		 (shimbun-headers
		  (elmo-shimbun-folder-shimbun-internal folder)
		  (elmo-shimbun-folder-range-internal folder)))))
    (elmo-shimbun-folder-set-headers-internal folder headers)
    (when headers
      (elmo-shimbun-folder-header-hash-setup folder headers))
    (elmo-shimbun-folder-set-last-check-internal folder (current-time))))

(luna-define-method elmo-folder-initialize ((folder
					     elmo-shimbun-folder)
					    name)
  (if (string= name "")
      folder
    (let ((server-group (if (string-match "\\([^.]+\\)\\." name)
			    (list (match-string 1 name)
				  (substring name (match-end 0)))
			  (list name))))
      (when (nth 0 server-group) ; server
	(elmo-shimbun-folder-set-shimbun-internal
	 folder
	 (condition-case nil
	     (shimbun-open (nth 0 server-group)
			   (luna-make-entity 'shimbun-elmo-mua :folder folder))
	   (file-error
	    (luna-make-entity 'shimbun :server (nth 0 server-group))))))
      (when (nth 1 server-group)
	(elmo-shimbun-folder-set-group-internal
	 folder
	 (nth 1 server-group)))
      (elmo-shimbun-folder-set-range-internal
       folder
       (or (cdr (elmo-string-matched-assoc (elmo-folder-name-internal folder)
					   elmo-shimbun-index-range-alist))
	   elmo-shimbun-default-index-range))
      folder)))

(luna-define-method elmo-folder-open-internal ((folder elmo-shimbun-folder))
  (when (elmo-shimbun-folder-shimbun-internal folder)
    (shimbun-open-group
     (elmo-shimbun-folder-shimbun-internal folder)
     (elmo-shimbun-folder-group-internal folder))
    (let ((inhibit-quit t))
      (unless (elmo-location-map-alist folder)
	(elmo-location-map-setup
	 folder
	 (elmo-msgdb-location-load (elmo-folder-msgdb-path folder))))
      (when (and (elmo-folder-plugged-p folder)
		 (elmo-shimbun-headers-check-p folder))
	(elmo-shimbun-get-headers folder)
	(elmo-location-map-update
	 folder
	 (elmo-map-folder-list-message-locations folder))))))

(luna-define-method elmo-folder-reserve-status-p ((folder elmo-shimbun-folder))
  t)

(luna-define-method elmo-folder-local-p ((folder elmo-shimbun-folder))
  nil)

(luna-define-method elmo-message-use-cache-p ((folder elmo-shimbun-folder)
					      number)
  elmo-shimbun-use-cache)

(luna-define-method elmo-folder-close-internal :after ((folder
							elmo-shimbun-folder))
  (shimbun-close-group
   (elmo-shimbun-folder-shimbun-internal folder))
  (elmo-shimbun-folder-set-headers-internal
   folder nil)
  (elmo-shimbun-folder-set-header-hash-internal
   folder nil)
  (elmo-shimbun-folder-set-entity-hash-internal
   folder nil)
  (elmo-shimbun-folder-set-last-check-internal
   folder nil))

(luna-define-method elmo-folder-plugged-p ((folder elmo-shimbun-folder))
  (if (elmo-shimbun-folder-shimbun-internal folder)
      (elmo-plugged-p
       "shimbun"
       (shimbun-server (elmo-shimbun-folder-shimbun-internal folder))
       nil nil
       (shimbun-server (elmo-shimbun-folder-shimbun-internal folder)))
    t))

(luna-define-method elmo-folder-set-plugged ((folder elmo-shimbun-folder)
					     plugged &optional add)
  (elmo-set-plugged plugged
		    "shimbun"
		    (shimbun-server
		     (elmo-shimbun-folder-shimbun-internal folder))
		    nil nil nil
		    (shimbun-server
		     (elmo-shimbun-folder-shimbun-internal folder))
		    add))

(luna-define-method elmo-net-port-info ((folder elmo-shimbun-folder))
  (list "shimbun"
	(shimbun-server
	 (elmo-shimbun-folder-shimbun-internal folder))
	nil))

(luna-define-method elmo-folder-check :around ((folder elmo-shimbun-folder))
  (when (shimbun-current-group
	 (elmo-shimbun-folder-shimbun-internal folder))
    (when (and (elmo-folder-plugged-p folder)
	       (elmo-shimbun-headers-check-p folder))
      (elmo-shimbun-get-headers folder)
      (luna-call-next-method))))

(luna-define-method elmo-folder-clear :around ((folder elmo-shimbun-folder)
					       &optional keep-killed)
  (elmo-shimbun-folder-set-headers-internal folder nil)
  (elmo-shimbun-folder-set-header-hash-internal folder nil)
  (elmo-shimbun-folder-set-entity-hash-internal folder nil)
  (elmo-shimbun-folder-set-last-check-internal folder nil)
  (luna-call-next-method))

(luna-define-method elmo-folder-expand-msgdb-path ((folder
						    elmo-shimbun-folder))
  (expand-file-name
   (concat (shimbun-server
	    (elmo-shimbun-folder-shimbun-internal folder))
	   "/"
	   (elmo-shimbun-folder-group-internal folder))
   (expand-file-name "shimbun" elmo-msgdb-directory)))

(defun elmo-shimbun-msgdb-create-entity (folder number)
  (let ((header (elmo-shimbun-folder-shimbun-header
		 folder
		 (elmo-map-message-location folder number)))
	ov)
    (when header
      (with-temp-buffer
	(shimbun-header-insert
	 (elmo-shimbun-folder-shimbun-internal folder)
	 header)
	(setq ov (elmo-msgdb-create-message-entity-from-header
		  (elmo-msgdb-message-entity-handler
		   (elmo-folder-msgdb-internal folder)) number))
	(elmo-message-entity-set-field
	 ov
	 'xref (shimbun-header-xref header)))
      ov)))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-shimbun-folder)
					      numlist flag-table)
  (let ((new-msgdb (elmo-make-msgdb))
	entity msgid flags)
    (elmo-with-progress-display (elmo-folder-msgdb-create (length numlist))
	"Creating msgdb"
      (dolist (number numlist)
	(setq entity (elmo-shimbun-msgdb-create-entity folder number))
	(when entity
	  (setq msgid (elmo-message-entity-field entity 'message-id)
		flags (elmo-flag-table-get flag-table msgid))
	  (elmo-global-flags-set flags folder number msgid)
	  (elmo-msgdb-append-entity new-msgdb entity flags))
	(elmo-progress-notify 'elmo-folder-msgdb-create)))
    new-msgdb))

(luna-define-method elmo-folder-message-file-p ((folder elmo-shimbun-folder))
  nil)

(defsubst elmo-shimbun-update-overview (folder entity shimbun-id header)
  (let ((message-id (shimbun-header-id header))
	references)
    (when (elmo-msgdb-update-entity
	   (elmo-folder-msgdb folder)
	   entity
	   (nconc
	    (unless (string= shimbun-id message-id)
	      (elmo-shimbun-header-set-extra-field
	       header "x-shimbun-id" shimbun-id)
	      (elmo-set-hash-val message-id
				 entity
				 (elmo-shimbun-folder-entity-hash folder))
	      (elmo-set-hash-val shimbun-id
				 entity
				 (elmo-shimbun-folder-entity-hash folder))
	      (list (cons 'x-original-id message-id)))
	    (list
	     (cons 'from    (shimbun-header-from header 'no-encode))
	     (cons 'subject (shimbun-header-subject header 'no-encode))
	     (cons 'date    (shimbun-header-date header))
	     (cons 'references
		   (elmo-msgdb-get-references-from-header)))))
      (elmo-emit-signal 'update-overview folder
			(elmo-message-entity-number entity)))))

(luna-define-method elmo-map-message-fetch ((folder elmo-shimbun-folder)
					    location strategy
					    &optional section unseen)
  (if (elmo-folder-plugged-p folder)
      (let ((header (elmo-shimbun-folder-shimbun-header
		     folder
		     location))
	    shimbun-id)
	(shimbun-article (elmo-shimbun-folder-shimbun-internal folder)
			 header)
	(when (or (eq elmo-shimbun-update-overview-folder-list 'all)
		  (elmo-string-match-member
		   (elmo-folder-name-internal folder)
		   elmo-shimbun-update-overview-folder-list))
	  (let ((entity (elmo-message-entity folder location)))
	    (when entity
	      (elmo-shimbun-update-overview folder entity location header))))
	(when (setq shimbun-id
		    (elmo-shimbun-header-extra-field header "x-shimbun-id"))
	  (goto-char (point-min))
	  (insert (format "X-Shimbun-Id: %s\n" shimbun-id)))
	t)
    (error "Unplugged")))

(luna-define-method elmo-message-encache :around ((folder
						   elmo-shimbun-folder)
						  number &optional read)
  (if (elmo-folder-plugged-p folder)
      (luna-call-next-method)
    (if elmo-enable-disconnected-operation
	(elmo-message-encache-dop folder number read)
      (error "Unplugged"))))

(luna-define-method elmo-folder-list-messages-internal :around
  ((folder elmo-shimbun-folder) &optional nohide)
  (if (elmo-folder-plugged-p folder)
      (luna-call-next-method)
    t))

(luna-define-method elmo-map-folder-list-message-locations
  ((folder elmo-shimbun-folder))
  (let ((expire-days (shimbun-article-expiration-days
		      (elmo-shimbun-folder-shimbun-internal folder))))
    (elmo-uniq-list
     (nconc
      (delq nil
	    (mapcar
	     (lambda (ov)
	       (when (and (elmo-message-entity-field ov 'xref)
			  (if expire-days
			      (< (elmo-shimbun-lapse-seconds
				  (elmo-message-entity-field ov 'date))
				 (* expire-days 86400 ; seconds per day
				    ))
			    t))
		 (elmo-message-entity-field ov 'message-id)))
	     (elmo-folder-list-message-entities folder)))
      (mapcar
       (lambda (header)
	 (or (elmo-shimbun-header-extra-field header "x-shimbun-id")
	     (shimbun-header-id header)))
       (elmo-shimbun-folder-headers-internal folder))))))

(luna-define-method elmo-folder-list-subfolders ((folder elmo-shimbun-folder)
						 &optional one-level)
  (let ((prefix (elmo-folder-prefix-internal folder)))
    (cond ((elmo-shimbun-folder-shimbun-internal folder)
	   (unless (elmo-shimbun-folder-group-internal folder)
	     (mapcar
	      (lambda (fld)
		(concat prefix
			(shimbun-server
			 (elmo-shimbun-folder-shimbun-internal folder))
			"." fld))
	      (shimbun-groups (elmo-shimbun-folder-shimbun-internal folder)))))
	  ;; the rest are for "@/" group
	  (one-level
	   (mapcar
	    (lambda (server) (list (concat prefix server)))
	    (shimbun-servers-list)))
	  (t
	   (let (folders)
	     (dolist (server (shimbun-servers-list))
	       (setq folders
		     (append folders
			     (mapcar
			      (lambda (group) (concat prefix server "." group))
			      (shimbun-groups
			       (elmo-shimbun-folder-shimbun-internal
				(elmo-get-folder (concat prefix server))))))))
	     folders)))))

(luna-define-method elmo-folder-exists-p ((folder elmo-shimbun-folder))
  (if (elmo-shimbun-folder-group-internal folder)
      (if (fboundp 'shimbun-group-p)
	  (shimbun-group-p (elmo-shimbun-folder-shimbun-internal folder)
			   (elmo-shimbun-folder-group-internal folder))
	(member
	 (elmo-shimbun-folder-group-internal folder)
	 (shimbun-groups (elmo-shimbun-folder-shimbun-internal folder))))
    t))

(luna-define-method elmo-folder-delete-messages ((folder elmo-shimbun-folder)
						 numbers)
  (elmo-folder-kill-messages folder numbers)
  t)

(luna-define-method elmo-message-entity-parent ((folder elmo-shimbun-folder)
						entity)
  (let ((references (elmo-message-entity-field entity 'references))
	parent)
    ;; In old msgdb, references's field is a string.
    (when (stringp references)
      (setq references (list references)))
    (while references
      (setq references
	    (if (setq parent (elmo-get-hash-val
			      (car references)
			      (elmo-shimbun-folder-entity-hash folder)))
		nil
	      (cdr references))))
    parent))

(require 'product)
(product-provide (provide 'elmo-shimbun) (require 'elmo-version))

;;; elmo-shimbun.el ends here
