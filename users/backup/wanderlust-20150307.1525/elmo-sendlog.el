;;; elmo-sendlog.el --- Sendlog folder for ELMO.

;; Copyright (C) 2001 Kenichi OKADA <okada@opaopa.org>

;; Author: Kenichi OKADA <okada@opaopa.org>
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

(defvar elmo-sendlog-filename "sendlog")

;;; ELMO sendlog folder
(eval-and-compile
  (luna-define-class elmo-sendlog-folder (elmo-map-folder elmo-file-tag)
		     (dir-name directory))
  (luna-define-internal-accessors 'elmo-sendlog-folder))

(luna-define-method elmo-folder-initialize ((folder elmo-sendlog-folder)
					    name)
  folder)

(luna-define-method elmo-folder-expand-msgdb-path ((folder elmo-sendlog-folder))
  (expand-file-name "sendlog"
		    (expand-file-name "internal"
				      elmo-msgdb-directory)))

(luna-define-method elmo-map-folder-list-message-locations
  ((folder elmo-sendlog-folder))
  (elmo-sendlog-folder-list-message-locations folder))

(defun elmo-sendlog-folder-list-message-locations (folder)
  (let ((filename (expand-file-name elmo-sendlog-filename
				    elmo-msgdb-directory))
	result)
    (if (not (file-readable-p filename))
	nil
      (with-temp-buffer
	(as-binary-input-file
	 (insert-file-contents filename))
	(goto-char (point-min))
	(catch 'done
	  (while t
	    (re-search-forward "id=\\([^@]+@[^@]+\\)$" (point-at-eol) t)
	    (setq result (append result (list (match-string 1))))
	    (if (eq (1+ (point-at-eol)) (point-max))
		(throw 'done nil)
	      (beginning-of-line 2))))))
    result))

(luna-define-method elmo-folder-message-file-p ((folder elmo-sendlog-folder))
  t)

(luna-define-method elmo-message-file-name ((folder elmo-sendlog-folder)
					    number)
  (elmo-file-cache-get-path
   (elmo-map-message-location folder number)))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-sendlog-folder)
					      numbers flag-table)
  (let ((new-msgdb (elmo-make-msgdb))
	entity message-id flags)
    (elmo-with-progress-display (elmo-folder-msgdb-create (length numbers))
	"Creating msgdb"
      (dolist (number numbers)
	(setq entity
	      (elmo-msgdb-create-message-entity-from-file
	       (elmo-msgdb-message-entity-handler new-msgdb) number
	       (elmo-message-file-name folder number)))
	(if (null entity)
	    (elmo-folder-set-killed-list-internal
	     folder
	     (nconc
	      (elmo-folder-killed-list-internal folder)
	      (list number)))
	  (setq message-id (elmo-message-entity-field entity 'message-id)
		flags (elmo-flag-table-get flag-table message-id))
	  (elmo-global-flags-set flags folder number message-id)
	  (elmo-msgdb-append-entity new-msgdb entity flags))
	(elmo-progress-notify 'elmo-folder-msgdb-create)))
    new-msgdb))

(luna-define-method elmo-message-fetch
  ((folder elmo-sendlog-folder) number strategy &optional unseen section)
  ;; disbable cache process
  (erase-buffer)
  (when (elmo-message-fetch-internal folder number strategy section unseen)
    (when (and (not unseen)
	       (elmo-message-flagged-p folder number 'unread))
      (elmo-message-unset-flag folder number 'unread))
    t))

(luna-define-method elmo-map-message-fetch ((folder elmo-sendlog-folder)
					    location strategy
					    &optional section unseen)
  (let ((filename (elmo-file-cache-get-path location)))
    (if (file-exists-p filename)
	(insert-file-contents-as-binary filename)
      (error "Now this message is not cached. Please s all"))))

(luna-define-method elmo-folder-exists-p ((folder elmo-sendlog-folder))
  t)

(luna-define-method elmo-folder-delete-messages ((folder elmo-sendlog-folder)
						 numbers)
  (elmo-folder-kill-messages folder numbers)
  t)

(luna-define-method elmo-message-file-p ((folder elmo-sendlog-folder) number)
  t)

(luna-define-method elmo-folder-have-subfolder-p ((folder elmo-sendlog-folder))
  nil)

(require 'product)
(product-provide (provide 'elmo-sendlog) (require 'elmo-version))

;;; elmo-sendlog.el ends here
