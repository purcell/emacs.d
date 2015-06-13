;;; elmo-internal.el --- Internal Interface for ELMO.

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
(require 'elmo)

;;; ELMO internal folder
(luna-define-class elmo-internal-folder (elmo-folder) ())

(defvar elmo-internal-folder-list '(flag cache sendlog null))
(defvar elmo-internal-obsolete-folder-list '((mark flag)))

(luna-define-method elmo-folder-initialize ((folder
					     elmo-internal-folder)
					    name)
  (when (assq (intern name) elmo-internal-obsolete-folder-list)
    (elmo-warning
     "Folder '%s is now obsolete. Use '%s instead."
     name
     (cadr (assq (intern name) elmo-internal-obsolete-folder-list))))
  (elmo-internal-folder-initialize folder name))

(defun elmo-internal-folder-initialize (folder name)
  (let ((fsyms elmo-internal-folder-list)
	fname class sym)
    (if (progn (while fsyms
		 (setq fname (symbol-name (car fsyms)))
		 (when (string-match (concat "^" fname) name)
		   (require (intern (concat "elmo-" fname)))
		   (setq class (intern (concat "elmo-" fname "-folder"))
			 sym (intern fname)
			 fsyms nil))
		 (setq fsyms (cdr fsyms)))
	       class)
	(elmo-folder-initialize
	 (luna-make-entity
	  class
	  :type sym
	  :prefix (elmo-folder-prefix-internal folder)
	  :name (elmo-folder-name-internal folder)
	  :persistent (elmo-folder-persistent-internal folder)
	  :mime-charset (elmo-folder-mime-charset-internal folder))
	 name)
      folder)))

(luna-define-method elmo-folder-list-subfolders ((folder elmo-internal-folder)
						 &optional one-level)
  (if one-level
      (mapcar
       (lambda (x)
	 (let ((name (concat (elmo-folder-prefix-internal folder)
			     (symbol-name x))))
	   (if (elmo-folder-have-subfolder-p (elmo-get-folder name))
	       (list name)
	     name)))
       elmo-internal-folder-list)
    (apply #'nconc
	   (mapcar
	    (lambda (x)
	      (let* ((name (concat (elmo-folder-prefix-internal folder)
				   (symbol-name x)))
		     (subfolder (elmo-get-folder name)))
		(if (elmo-folder-have-subfolder-p subfolder)
		    (elmo-folder-list-subfolders subfolder)
		  (list name))))
	    elmo-internal-folder-list))))

(require 'product)
(product-provide (provide 'elmo-internal) (require 'elmo-version))

;;; elmo-internal.el ends here
