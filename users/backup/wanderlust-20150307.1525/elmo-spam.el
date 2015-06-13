;;; elmo-spam.el --- Spam filtering interface to processor.

;; Copyright (C) 2003 Hiroya Murata <lapis-lazuli@pop06.odn.ne.jp>
;; Copyright (C) 2003 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Hiroya Murata <lapis-lazuli@pop06.odn.ne.jp>
;; Keywords: mail, net news, spam

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

(eval-when-compile (require 'cl))

(require 'luna)
(require 'elmo-util)
(require 'elmo)

(defgroup elmo-spam nil
  "Spam configuration for wanderlust."
  :group 'elmo)

(defcustom elmo-spam-scheme nil
  "*Scheme of spam processor implementation. "
  :type '(choice (const :tag "none" nil)
		 (const :tag "Bogofilter" bogofilter)
		 (const :tag "Spamfilter" spamfilter)
		 (const :tag "SpamAssassin" sa)
		 (const :tag "Header" header)
		 (const :tag "Bsfilter" bsfilter))
  :group 'elmo-spam)

(eval-and-compile
  (luna-define-class elsp-generic ()))

;; required method
(luna-define-generic elmo-spam-buffer-spam-p (processor buffer
							&optional register)
  "Return non-nil if contents of BUFFER is spam.
PROCESSOR is spam processor structure.
If optional augument REGISTER is non-nil,
register according to the classification.")

(luna-define-generic elmo-spam-register-spam-buffer (processor
						     buffer
						     &optional restore)
  "Register contents of BUFFER as spam.
PROCESSOR is spam processor structure.
If optional argument RESTORE is non-nil, unregister from non-spam list.")

(luna-define-generic elmo-spam-register-good-buffer (processor
						     buffer
						     &optional restore)
  "Register contents of BUFFER as non-spam.
PROCESSOR is spam processor structure.
If optional argument RESTORE is non-nil, unregister from spam list.")

;; optional method
(luna-define-generic elmo-spam-modified-p (processor)
  "Return non-nil if status of PROCESSOR is modified.")

(luna-define-generic elmo-spam-save-status (processor)
  "Save status of the PROCESSOR.")

(luna-define-generic elmo-spam-message-spam-p (processor folder number
							 &optional register)
  "Return non-nil if the message in the FOLDER with NUMBER is spam.
PROCESSOR is spam processor structure.
If optional augument REGISTER is non-nil,
register according to the classification.")

(luna-define-generic elmo-spam-list-spam-messages (processor
						   folder &optional numbers)
  "Return a list of message numbers which is classified as spam.
PROCESSOR is spam processor structure.
FOLDER is the ELMO folder structure.
If optional argument NUMBERS is specified and is a list of message numbers,
messages are searched from the list.")

(luna-define-generic elmo-spam-register-spam-messages (processor
						       folder
						       &optional
						       numbers restore)
  "Register contents of messages as spam.
PROCESSOR is spam processor structure.
FOLDER is the ELMO folder structure.
If optional argument NUMBERS is specified and is a list of message numbers,
messages are searched from the list.
If optional argument RESTORE is non-nil, unregister from non-spam list.")

(luna-define-generic elmo-spam-register-good-messages (processor
						       folder
						       &optional
						       numbers restore)
  "Register contents of messages as non spam.
PROCESSOR is spam processor structure.
FOLDER is the ELMO folder structure.
If optional argument NUMBERS is specified and is a list of message numbers,
messages are searched from the list.
If optional argument RESTORE is non-nil, unregister from spam list.")

;; for internal use
(defun elmo-spam-message-fetch (folder number)
  (let (elmo-message-fetch-threshold)
    (when enable-multibyte-characters
      (set-buffer-multibyte nil))
    (elmo-message-fetch
     folder number
     (elmo-find-fetch-strategy folder number nil 'entire)
     'unread)))

(defun elmo-spam-process-messages-as-mbox (folder numbers number-per-process
						  function &rest args)
  (with-temp-buffer
    (while numbers
      (let ((count 0))
	(while (and numbers (< count number-per-process))
	  (insert "From MAILER-DAEMON@example.com\n")
	  (let ((begin (point)))
	    (insert
	     (with-temp-buffer
	       (elmo-spam-message-fetch folder (car numbers))
	       (buffer-string)))
	    (goto-char begin)
	    (while (re-search-forward "^>*From " nil t)
	      (goto-char (match-beginning 0))
	      (insert ?>)
	      (forward-line))
	    (goto-char (point-max))
	    (insert "\n\n"))
	  (setq count (1+ count)
		numbers (cdr numbers)))
	(apply function count args)
	(erase-buffer)))))

;; generic implement
(luna-define-method elmo-spam-message-spam-p ((processor elsp-generic)
					      folder number &optional register)
  (with-temp-buffer
    (elmo-spam-message-fetch folder number)
    (elmo-spam-buffer-spam-p processor (current-buffer) register)))

(luna-define-method elmo-spam-list-spam-messages ((processor elsp-generic)
						  folder &optional numbers)
  (let ((numbers (or numbers (elmo-folder-list-messages folder t t)))
	spam-list)
    (dolist (number numbers)
      (when (elmo-spam-message-spam-p processor folder number)
	(setq spam-list (cons number spam-list)))
      (elmo-progress-notify 'elmo-spam-check-spam))
    (nreverse spam-list)))

(luna-define-method elmo-spam-register-spam-messages ((processor elsp-generic)
						      folder
						      &optional
						      numbers restore)
  (let ((numbers (or numbers (elmo-folder-list-messages folder t t))))
    (with-temp-buffer
      (dolist (number numbers)
	(erase-buffer)
	(elmo-spam-message-fetch folder number)
	(elmo-spam-register-spam-buffer processor (current-buffer) restore)
	(elmo-progress-notify 'elmo-spam-register)))))

(luna-define-method elmo-spam-register-good-messages ((processor elsp-generic)
						      folder
						      &optional
						      numbers restore)
  (let ((numbers (or numbers (elmo-folder-list-messages folder t t))))
    (with-temp-buffer
      (dolist (number numbers)
	(erase-buffer)
	(elmo-spam-message-fetch folder number)
	(elmo-spam-register-good-buffer processor (current-buffer) restore)
	(elmo-progress-notify 'elmo-spam-register)))))

(provide 'elsp-generic)

(defvar elmo-spam-processor-internal nil)

(defun elmo-spam-processor (&optional if-exists)
  (or elmo-spam-processor-internal
      (unless if-exists
	(let* ((scheme (or elmo-spam-scheme 'generic))
	       (class (intern (format "elsp-%s" scheme))))
	  (require class)
	  (setq elmo-spam-processor-internal
		(luna-make-entity class))))))

;; Backend for header match
(eval-and-compile
  (luna-define-class elsp-header (elsp-generic)))

(defgroup elmo-spam-header nil
  "Spam header configuration."
  :group 'elmo-spam)

(defcustom elmo-spam-header-good-alist '(("X-Spam-Flag" . "No"))
  "*Regular expression for positive header good matches."
  :type '(repeat (cons (string :tag "Header name")
		       (regexp :tag "Regular expression to match good header")))
  :group 'elmo-spam-header)

(defcustom elmo-spam-header-spam-alist '(("X-Spam-Flag" . "Yes"))
  "*Regular expression for positive header spam matches."
  :type '(repeat (cons (string :tag "Header name")
		       (regexp :tag "Regular expression to match spam header")))
  :group 'elmo-spam-header)

(defun elmo-spam-header-check-headers (fetch-field-function)
  (catch 'done
    (dolist (pair elmo-spam-header-good-alist)
      (let ((field-body (funcall fetch-field-function (car pair))))
	(when (and field-body (string-match (cdr pair) field-body))
	  (throw 'done nil))))
    (dolist (pair elmo-spam-header-spam-alist)
      (let ((field-body (funcall fetch-field-function (car pair))))
	(when (and field-body (string-match (cdr pair) field-body))
	  (throw 'done t))))))

(luna-define-method elmo-spam-buffer-spam-p ((processor elsp-header)
					     buffer &optional register)
  (with-current-buffer buffer
    (save-restriction
      (std11-narrow-to-header)
      (elmo-spam-header-check-headers #'elmo-decoded-fetch-field))))

(luna-define-method elmo-spam-message-spam-p ((processor elsp-header)
					      folder number &optional register)
  (let ((entity (elmo-message-entity folder number))
	buffer)
    (unwind-protect
	(save-excursion
	  (elmo-spam-header-check-headers
	   (lambda (field-name)
	     (or (elmo-message-entity-field entity
					    (intern (downcase field-name))
					    'string)
		 (progn
		   (unless buffer
		     (setq buffer (get-buffer-create
				   (generate-new-buffer-name
				    " *elmo-spam-work*")))
		     (set-buffer buffer)
		     (elmo-spam-message-fetch folder number)
		     (std11-narrow-to-header))
		   (elmo-decoded-fetch-field field-name))))))
      (and buffer (kill-buffer buffer)))))

(luna-define-method elmo-spam-register-spam-messages ((processor elsp-header)
						      folder
						      &optional
						      numbers restore)
  (elmo-progress-notify 'elmo-spam-register (length numbers)))

(luna-define-method elmo-spam-register-good-messages ((processor elsp-header)
						      folder
						      &optional
						      numbers restore)
  (elmo-progress-notify 'elmo-spam-register (length numbers)))

(provide 'elsp-header)


(require 'product)
(product-provide (provide 'elmo-spam) (require 'elmo-version))

;;; elmo-spam.el ends here
