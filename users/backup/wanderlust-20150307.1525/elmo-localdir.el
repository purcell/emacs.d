;;; elmo-localdir.el --- Localdir Interface for ELMO.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 1998,1999,2000 Masahiro MURATA <muse@ba2.so-net.ne.jp>
;; Copyright (C) 1999,2000      Kenichi OKADA  <okada@opaopa.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Masahiro MURATA <muse@ba2.so-net.ne.jp>
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
(eval-when-compile (require 'cl))

(require 'elmo-msgdb)
(require 'elmo)

(defcustom elmo-localdir-folder-path "~/Mail"
  "*Local mail directory (MH format) path."
  :type 'directory
  :group 'elmo)

(defvar elmo-localdir-lockfile-list nil)

;;; ELMO Local directory folder
(eval-and-compile
  (luna-define-class elmo-localdir-folder (elmo-folder elmo-file-tag)
		     (dir-name directory))
  (luna-define-internal-accessors 'elmo-localdir-folder))

;;; elmo-localdir specific methods.
(luna-define-generic elmo-localdir-folder-path (folder)
  "Return local directory path of the FOLDER.")

(luna-define-generic elmo-localdir-folder-name (folder name)
  "Return directory NAME for FOLDER.")

(luna-define-method elmo-localdir-folder-path ((folder elmo-localdir-folder))
  elmo-localdir-folder-path)

(luna-define-method elmo-localdir-folder-name ((folder elmo-localdir-folder)
					       name)
  name)

(luna-define-method elmo-folder-initialize ((folder
					     elmo-localdir-folder)
					    name)
  (elmo-localdir-folder-set-dir-name-internal folder name)
  (if (file-name-absolute-p name)
      (elmo-localdir-folder-set-directory-internal
       folder
       (expand-file-name name))
    (elmo-localdir-folder-set-directory-internal
     folder
     (expand-file-name
      (elmo-localdir-folder-name folder name)
      (elmo-localdir-folder-path folder))))
  folder)

;; open, check, commit, and close are generic.

(luna-define-method elmo-folder-exists-p ((folder elmo-localdir-folder))
  (file-directory-p (elmo-localdir-folder-directory-internal folder)))

(luna-define-method elmo-folder-expand-msgdb-path ((folder
						    elmo-localdir-folder))
  (let* ((dir-name (elmo-localdir-folder-dir-name-internal folder))
	 (path (mapconcat
		'identity
		(delete ""
			(mapcar
			 'elmo-replace-string-as-filename
			 (split-string
			  (if (file-name-absolute-p dir-name)
			      (expand-file-name dir-name)
			    dir-name)
			  "/")))
		"/")))
    (expand-file-name
     path
     (expand-file-name ;;"localdir" or "localdir-abs"
      (concat
       (symbol-name (elmo-folder-type-internal folder))
       (when (file-name-absolute-p dir-name) "-abs"))
      elmo-msgdb-directory))))

(luna-define-method elmo-message-file-name ((folder
					     elmo-localdir-folder)
					    number)
  (expand-file-name (number-to-string number)
		    (elmo-localdir-folder-directory-internal folder)))

(luna-define-method elmo-folder-message-file-number-p ((folder
							elmo-localdir-folder))
  t)

(luna-define-method elmo-folder-message-file-directory ((folder
							 elmo-localdir-folder))
  (elmo-localdir-folder-directory-internal folder))

(luna-define-method elmo-folder-message-make-temp-file-p
  ((folder elmo-localdir-folder))
  t)

(luna-define-method elmo-folder-message-make-temp-files ((folder
							  elmo-localdir-folder)
							 numbers
							 &optional
							 start-number)
  (let ((temp-dir (elmo-folder-make-temporary-directory folder))
	(cur-number (or start-number 0)))
    (dolist (number numbers)
      (elmo-copy-file
       (expand-file-name
	(number-to-string number)
	(elmo-localdir-folder-directory-internal folder))
       (expand-file-name
	(number-to-string (if start-number cur-number number))
	temp-dir))
      (incf cur-number))
    temp-dir))

(defun elmo-localdir-msgdb-create-entity (msgdb dir number)
  (elmo-msgdb-create-message-entity-from-file
   (elmo-msgdb-message-entity-handler msgdb)
   number (expand-file-name (number-to-string number) dir)))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-localdir-folder)
					      numbers
					      flag-table)
  (when numbers
    (let ((dir (elmo-localdir-folder-directory-internal folder))
	  (new-msgdb (elmo-make-msgdb))
	  entity message-id flags)
      (elmo-with-progress-display (elmo-folder-msgdb-create (length numbers))
	  "Creating msgdb"
	(dolist (number numbers)
	  (setq entity (elmo-localdir-msgdb-create-entity
			new-msgdb dir number))
	  (when entity
	    (setq message-id (elmo-message-entity-field entity 'message-id)
		  flags (elmo-flag-table-get flag-table message-id))
	    (elmo-global-flags-set flags folder number message-id)
	    (elmo-msgdb-append-entity new-msgdb entity flags))
	  (elmo-progress-notify 'elmo-folder-msgdb-create)))
      new-msgdb)))

(luna-define-method elmo-folder-list-subfolders ((folder elmo-localdir-folder)
						 &optional one-level)
  (elmo-mapcar-list-of-list
   (lambda (x) (concat (elmo-folder-prefix-internal folder) x))
   (elmo-list-subdirectories
    (elmo-localdir-folder-path folder)
    (or (elmo-localdir-folder-dir-name-internal folder) "")
    one-level)))

(defsubst elmo-localdir-list-subr (folder &optional nonsort)
  (let ((flist (mapcar 'string-to-number
		       (directory-files
			(elmo-localdir-folder-directory-internal folder)
			nil "^[0-9]+$" t)))
	(killed (elmo-msgdb-killed-list-load (elmo-folder-msgdb-path folder))))
    (if nonsort
	(cons (elmo-max-of-list flist)
	      (if killed
		  (- (length flist)
		     (elmo-msgdb-killed-list-length killed))
		(length flist)))
      (sort flist '<))))

(luna-define-method elmo-folder-append-buffer ((folder elmo-localdir-folder)
					       &optional flags number
					       return-number)
  (let ((filename (elmo-message-file-name
		   folder
		   (or number
		       (1+ (car (elmo-folder-status folder)))))))
    (when (and (file-writable-p filename)
	       (not (file-exists-p filename)))
      (write-region-as-binary
       (point-min) (point-max) filename nil 'no-msg)
      (elmo-folder-preserve-flags
       folder (elmo-msgdb-get-message-id-from-buffer) flags)
      (if return-number
	  (car (elmo-folder-status folder))
	t))))

(defun elmo-folder-append-messages-*-localdir (folder
					       src-folder
					       numbers
					       same-number)
  (let ((src-msgdb-exists (not (zerop (elmo-folder-length src-folder))))
	(dir (elmo-localdir-folder-directory-internal folder))
	(table (elmo-folder-flag-table folder))
	(succeeds numbers)
	(next-num (1+ (car (elmo-folder-status folder))))
	flags id)
    (while numbers
      (setq flags (elmo-message-flags src-folder (car numbers)))
      (elmo-copy-file
       (elmo-message-file-name src-folder (car numbers))
       (expand-file-name
	(number-to-string
	 (if same-number (car numbers) next-num))
	dir))
      ;; save flag-table only when src folder's msgdb is loaded.
      (when (setq id (and src-msgdb-exists
			  (elmo-message-field src-folder (car numbers)
					      'message-id)))
	(elmo-flag-table-set table id flags))
      (elmo-progress-notify 'elmo-folder-move-messages)
      (if (and (setq numbers (cdr numbers))
	       (not same-number))
	  (setq next-num
		(if (elmo-localdir-locked-p)
		    ;; MDA is running.
		    (1+ (car (elmo-folder-status folder)))
		  (1+ next-num)))))
    (when (elmo-folder-persistent-p folder)
      (elmo-folder-close-flag-table folder))
    succeeds))

(luna-define-method elmo-folder-delete-messages-internal
  ((folder elmo-localdir-folder) numbers)
  (dolist (number numbers)
    (elmo-localdir-delete-message folder number))
  t)

(defun elmo-localdir-delete-message (folder number)
  "Delete message in the FOLDER with NUMBER."
  (let ((filename (elmo-message-file-name folder number)))
    (when (and (string-match "[0-9]+" filename) ; for safety.
	       (file-exists-p filename)
	       (file-writable-p filename)
	       (not (file-directory-p filename)))
      (delete-file filename)
      t)))

(luna-define-method elmo-message-fetch-internal ((folder elmo-localdir-folder)
						 number strategy
						 &optional section unread)
  (let ((filename (elmo-message-file-name folder number)))
    (when (file-exists-p filename)
      (insert-file-contents-as-raw-text filename))))

(luna-define-method elmo-folder-list-messages-internal
  ((folder elmo-localdir-folder) &optional nohide)
  (elmo-localdir-list-subr folder))

(luna-define-method elmo-folder-status ((folder elmo-localdir-folder))
  (elmo-localdir-list-subr folder t))

(luna-define-method elmo-folder-creatable-p ((folder elmo-localdir-folder))
  t)

(luna-define-method elmo-folder-writable-p ((folder elmo-localdir-folder))
  t)

(luna-define-method elmo-folder-create ((folder elmo-localdir-folder))
  (let ((dir (elmo-localdir-folder-directory-internal folder)))
    (if (file-directory-p dir)
	()
      (if (file-exists-p dir)
	  (error "Create folder failed")
	(elmo-make-directory dir))
      t)))

(luna-define-method elmo-folder-delete ((folder elmo-localdir-folder))
  (let ((msgs (and (elmo-folder-exists-p folder)
		   (elmo-folder-list-messages folder))))
    (when (yes-or-no-p (format "%sDelete msgdb and substance of \"%s\"? "
			       (if (> (length msgs) 0)
				   (format "%d msg(s) exists. " (length msgs))
				 "")
			       (elmo-folder-name-internal folder)))
      (let ((dir (elmo-localdir-folder-directory-internal folder)))
	(if (not (file-directory-p dir))
	    (error "No such directory: %s" dir)
	  (elmo-delete-match-files dir "[0-9]+" t)))
      (elmo-msgdb-delete-path folder)
      t)))

(luna-define-method elmo-folder-rename-internal ((folder elmo-localdir-folder)
						 new-folder)
  (let* ((old (elmo-localdir-folder-directory-internal folder))
	 (new (elmo-localdir-folder-directory-internal new-folder))
	 (new-dir (directory-file-name (file-name-directory new))))
    (unless (file-directory-p old)
      (error "No such directory: %s" old))
    (when (file-exists-p new)
      (error "Already exists directory: %s" new))
    (unless (file-directory-p new-dir)
      (elmo-make-directory new-dir))
    (rename-file old new)
    t))

(luna-define-method elmo-folder-pack-numbers ((folder elmo-localdir-folder))
  (let* ((dir (elmo-localdir-folder-directory-internal folder))
	 (msgdb (elmo-folder-msgdb folder))
	 (new-msgdb (elmo-make-msgdb (elmo-folder-msgdb-path folder)))
	 (numbers (sort (elmo-folder-list-messages
			 folder
			 nil
			 (not elmo-pack-number-check-strict))
			'<))
	 (new-number 1)		  ; first ordinal position in localdir
	 entity)
    (elmo-with-progress-display (elmo-folder-pack-numbers (length numbers))
	"Packing"
      (dolist (old-number numbers)
	(setq entity (elmo-msgdb-message-entity msgdb old-number))
	(when (/= old-number new-number)
	  (elmo-bind-directory dir
	    ;; xxx  nfs,hardlink
	    (rename-file (number-to-string old-number)
			 (number-to-string new-number) t))
	  (elmo-message-entity-set-number entity new-number))
	(elmo-msgdb-append-entity new-msgdb entity
				  (elmo-msgdb-flags msgdb old-number))
	(elmo-emit-signal 'message-number-changed folder old-number new-number)
	(setq new-number (1+ new-number))))
    (message "Packing...done")
    (elmo-folder-set-msgdb-internal folder new-msgdb)))

(luna-define-method elmo-folder-message-file-p ((folder elmo-localdir-folder))
  t)

(defun elmo-localdir-locked-p ()
  (if elmo-localdir-lockfile-list
      (let ((lock elmo-localdir-lockfile-list))
	(catch 'found
	  (while lock
	    (if (file-exists-p (car lock))
		(throw 'found t))
	    (setq lock (cdr lock)))))))

(autoload 'elmo-global-flags-set "elmo-flag")

(require 'product)
(product-provide (provide 'elmo-localdir) (require 'elmo-version))

;;; elmo-localdir.el ends here
