;;; elmo-cache.el --- Cache modules for ELMO.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 2000 Kenichi OKADA <okada@opaopa.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Kenichi OKADA <okada@opaopa.org>
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
(require 'elmo-vars)
(require 'elmo-util)
(require 'elmo)
(require 'elmo-map)

(eval-and-compile
  (luna-define-class elmo-cache-folder (elmo-map-folder elmo-file-tag)
		     (dir-name directory))
  (luna-define-internal-accessors 'elmo-cache-folder))

(luna-define-method elmo-folder-initialize ((folder elmo-cache-folder)
					    name)
  (when (string-match "\\([^/]*\\)/?\\(.*\\)$" name)
    (elmo-cache-folder-set-dir-name-internal
     folder
     (match-string 2 name))
    (elmo-cache-folder-set-directory-internal
     folder
     (expand-file-name (match-string 2 name)
		       elmo-cache-directory))
    folder))

(luna-define-method elmo-folder-expand-msgdb-path ((folder elmo-cache-folder))
  (expand-file-name (elmo-cache-folder-dir-name-internal folder)
		    (expand-file-name "internal/cache"
				      elmo-msgdb-directory)))

(luna-define-method elmo-map-folder-list-message-locations
  ((folder elmo-cache-folder))
  (elmo-cache-folder-list-message-locations folder))

(defun elmo-cache-folder-list-message-locations (folder)
  (mapcar 'file-name-nondirectory
	  (elmo-delete-if
	   'file-directory-p
	   (directory-files (elmo-cache-folder-directory-internal folder)
			    t "^[^@]+@[^@]+$" t))))

(luna-define-method elmo-folder-list-subfolders ((folder elmo-cache-folder)
						 &optional one-level)
  (delq nil (mapcar
	     (lambda (f) (if (file-directory-p f)
			     (concat (elmo-folder-prefix-internal folder)
				     "cache/"
				     (file-name-nondirectory f))))
	     (directory-files (elmo-cache-folder-directory-internal folder)
			      t "^[01][0-9A-F]$"))))

(luna-define-method elmo-folder-message-file-p ((folder elmo-cache-folder))
  t)

(luna-define-method elmo-message-file-name ((folder elmo-cache-folder)
					    number)
  (expand-file-name
   (elmo-map-message-location folder number)
   (elmo-cache-folder-directory-internal folder)))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-cache-folder)
					      numbers flag-table)
  (let ((new-msgdb (elmo-make-msgdb))
	entity message-id flags)
    (elmo-with-progress-display (elmo-folder-msgdb-create (length numbers))
	"Creating msgdb"
      (dolist (number numbers)
	(setq entity
	      (elmo-msgdb-create-message-entity-from-file
	       (elmo-msgdb-message-entity-handler new-msgdb)
	       number
	       (elmo-message-file-name folder number)))
	(when entity
	  (setq message-id (elmo-message-entity-field entity 'message-id)
		flags (elmo-flag-table-get flag-table message-id))
	  (elmo-global-flags-set flags folder number message-id)
	  (elmo-msgdb-append-entity new-msgdb entity flags))
	(elmo-progress-notify 'elmo-folder-msgdb-create)))
    new-msgdb))

(luna-define-method elmo-folder-append-buffer ((folder elmo-cache-folder)
					       &optional flags number
					       return-number)
  ;; dir-name is changed according to msgid.
  (unless (elmo-cache-folder-dir-name-internal folder)
    (let ((msgid (elmo-msgdb-get-message-id-from-buffer))
	  file dir)
      (when msgid
	(setq file (elmo-file-cache-get-path msgid))
	(setq dir (directory-file-name (file-name-directory file)))
	(unless (file-exists-p dir)
	  (elmo-make-directory dir))
	(when (and (file-writable-p file)
		   (not (file-exists-p file)))
	  (write-region-as-binary
	   (point-min) (point-max) file nil 'no-msg)
	  (elmo-folder-preserve-flags folder msgid flags)
	  t)))))

(luna-define-method elmo-map-folder-delete-messages ((folder elmo-cache-folder)
						     locations)
  (dolist (location locations)
    (elmo-file-cache-delete
     (expand-file-name location
		       (elmo-cache-folder-directory-internal folder))))
  t)

(luna-define-method elmo-message-fetch
  ((folder elmo-cache-folder) number strategy &optional unseen section)
  ;; disable cache process
  (erase-buffer)
  (when (elmo-message-fetch-internal folder number strategy section unseen)
    (when (and (not unseen)
	       (elmo-message-flagged-p folder number 'unread))
      (elmo-message-unset-flag folder number 'unread))
    t))

(luna-define-method elmo-map-message-fetch ((folder elmo-cache-folder)
					    location strategy
					    &optional section unseen)
  (let ((file (expand-file-name
	       location
	       (elmo-cache-folder-directory-internal folder))))
    (when (file-exists-p file)
      (insert-file-contents-as-binary file))))

(luna-define-method elmo-folder-writable-p ((folder elmo-cache-folder))
  t)

(luna-define-method elmo-folder-exists-p ((folder elmo-cache-folder))
  t)

(luna-define-method elmo-message-file-p ((folder elmo-cache-folder) number)
  t)

(require 'product)
(product-provide (provide 'elmo-cache) (require 'elmo-version))

;;; elmo-cache.el ends here
