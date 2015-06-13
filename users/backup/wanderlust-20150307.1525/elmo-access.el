;;; elmo-access.el --- Auto Collect Multiple Folder Interface for ELMO.

;; Copyright (C) 2005 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Hiroya Murata <lapis-lazuli@pop06.odn.ne.jp>
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
(require 'elmo-multi)

(defvar elmo-access-folder-list-filename "flist"
  "Folder list cache.")

(eval-and-compile
  (luna-define-class elmo-access-folder (elmo-multi-folder)
		     (base-folder))
  (luna-define-internal-accessors 'elmo-access-folder))

(luna-define-method elmo-folder-initialize ((folder elmo-access-folder) name)
  (elmo-access-folder-set-base-folder-internal
   folder
   (elmo-get-folder name))
  (elmo-multi-folder-set-children-internal
   folder
   (mapcar #'elmo-get-folder
	   (elmo-object-load
	    (expand-file-name elmo-access-folder-list-filename
			      (elmo-folder-msgdb-path folder)))))
  (elmo-multi-folder-set-divide-number-internal
   folder
   elmo-multi-divide-number)
  (elmo-access-folder-update-children folder)
  (elmo-multi-connect-signals folder)
  folder)

(luna-define-method elmo-folder-expand-msgdb-path ((folder elmo-access-folder))
  (expand-file-name (elmo-replace-string-as-filename
		     (elmo-folder-name-internal folder))
		    (expand-file-name "access" elmo-msgdb-directory)))

(defvar elmo-access-substitute-folder nil)

(defun elmo-access-substitute-folder ()
  (or elmo-access-substitute-folder
      (setq elmo-access-substitute-folder
	    (elmo-make-folder "'null"))))

(defun elmo-access-folder-update-children (folder &optional open expunge)
  (elmo-multi-folder-set-children-internal
   folder
   (if (elmo-folder-plugged-p
	(elmo-access-folder-base-folder-internal folder))
       (let ((subfolders (elmo-folder-list-subfolders
			  (elmo-access-folder-base-folder-internal folder)))
	     children)
	 (dolist (child (elmo-multi-folder-children-internal folder))
	   (let ((name (elmo-folder-name-internal child)))
	     (cond ((member name subfolders)
		    (setq children (nconc children (list child))))
		   (expunge)
		   (t
		    (setq children
			  (nconc children
				 (list (elmo-access-substitute-folder))))))
	     (setq subfolders (delete name subfolders))))
	 (if subfolders
	     (nconc children
		    (mapcar (lambda (name)
			      (let ((folder (elmo-get-folder name)))
				(when open
				  (elmo-folder-open-internal folder))
				folder))
			    subfolders))
	   children))
     (mapcar (lambda (f)
	       (if (elmo-folder-exists-p f)
		   f
		 (elmo-access-substitute-folder)))
	     (elmo-multi-folder-children-internal folder)))))

(luna-define-method elmo-folder-open-internal
  :before ((folder elmo-access-folder))
  (elmo-access-folder-update-children folder))

(luna-define-method elmo-folder-commit :after ((folder elmo-access-folder))
  (when (elmo-folder-persistent-p folder)
    (elmo-object-save
     (expand-file-name elmo-access-folder-list-filename
		       (elmo-folder-msgdb-path folder))
     (mapcar (lambda (f) (elmo-folder-name-internal f))
	     (elmo-multi-folder-children-internal folder)))))

(luna-define-method elmo-folder-check :before ((folder elmo-access-folder))
  (elmo-access-folder-update-children folder 'open))

(luna-define-method elmo-folder-synchronize :before
  ((folder elmo-access-folder)
   &optional disable-killed ignore-msgdb no-check mask)
  (when (or ignore-msgdb
	    (not no-check))
    (elmo-access-folder-update-children folder 'open ignore-msgdb)))

(luna-define-method elmo-folder-creatable-p ((folder elmo-access-folder))
  nil)

(require 'product)
(product-provide (provide 'elmo-access) (require 'elmo-version))

;;; elmo-access.el.el ends here
