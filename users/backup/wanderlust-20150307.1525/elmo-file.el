;;; elmo-file.el --- File interface for ELMO.

;; Copyright (C) 2000 Yuuichi Teranishi <teranisi@gohome.org>

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
(eval-when-compile (require 'cl))

(require 'elmo)
(require 'elmo-map)
(require 'mime-edit)

(defun elmo-file-find (files)
  "Return the first existing filename in the FILES."
  (let (file)
    (while files
      (when (file-exists-p (car files))
	(setq file (car files)
	      files nil))
      (setq files (cdr files)))
    (and file (expand-file-name file))))

(defcustom elmo-file-command (exec-installed-p "file")
  "*Program name of the file type detection command `file'."
  :type '(string :tag "Program name of the file")
  :group 'elmo)

(defcustom elmo-file-command-argument
  '("-i")
  "*Argument list for the `file' command.
\(It should return the MIME content type\)"
  :type '(repeat string)
  :group 'elmo)

(defcustom elmo-file-fetch-max-size (* 1024 1024)
  "*Max size of the message fetching."
  :type 'integer
  :group 'elmo)

(eval-and-compile
  (luna-define-class elmo-file-folder (elmo-map-folder elmo-file-tag)
		     (file-path))
  (luna-define-internal-accessors 'elmo-file-folder))

(luna-define-method elmo-folder-initialize ((folder
					     elmo-file-folder)
					    name)
  (elmo-file-folder-set-file-path-internal folder name)
  folder)

(luna-define-method elmo-folder-expand-msgdb-path ((folder
						    elmo-file-folder))
  (expand-file-name
   (elmo-replace-string-as-filename (elmo-folder-name-internal folder))
   (expand-file-name "file" elmo-msgdb-directory)))

(defun elmo-file-make-date-string (attrs)
  (let ((s (current-time-string (nth 5 attrs))))
    (string-match "\\`\\([A-Z][a-z][a-z]\\) +[A-Z][a-z][a-z] +[0-9][0-9]? *[0-9][0-9]?:[0-9][0-9]:[0-9][0-9] *[0-9]?[0-9]?[0-9][0-9]"
		  s)
    (concat (match-string 1 s) ", "
	    (timezone-make-date-arpa-standard s (current-time-zone)))))

(defun elmo-file-detect-content-type (file)
  "Return content-type of the FILE."
  (if (or (not (file-exists-p file))
	  (file-directory-p file))
      "application/octet-stream"
    (let (type)
      (setq type (mime-find-file-type file))
      (if (and (string= (nth 0 type) "application")
	       (string= (nth 1 type) "octet-stream"))
	  (if (and elmo-file-command
		   elmo-file-command-argument)
	      (with-temp-buffer
		(if (zerop (apply 'call-process elmo-file-command
				  nil `(,(current-buffer) nil)
				  nil (append elmo-file-command-argument
					      (list (expand-file-name file)))))
		    (progn
		      (goto-char (point-min))
		      (when (re-search-forward ": *" nil t)
			(setq type (buffer-substring (match-end 0)
						     (point-at-eol))))
		      (if (string-match "/" type)
			  type
			  "application/octet-stream"))
		    "application/octet-stream"))
	      (concat (nth 0 type) "/" (nth 1 type)))
	  (concat (nth 0 type) "/" (nth 1 type))))))

(defun elmo-file-msgdb-create-entity (msgdb folder number)
  "Create msgdb entity for the message in the FOLDER with NUMBER."
  (let* ((file (elmo-message-file-name folder number))
	 (attrs (file-attributes file)))
    (and (not (file-directory-p file))
	 attrs
	 (elmo-msgdb-make-message-entity
	  (elmo-msgdb-message-entity-handler msgdb)
	  :message-id (concat "<" (elmo-replace-in-string
				   file "/" ":")
			      "@" (system-name))
	  :number number
	  :size (nth 7 attrs)
	  :date (elmo-file-make-date-string attrs)
	  :subject (file-name-nondirectory file)
	  :from (concat (user-full-name (nth 2 attrs))
			" <" (user-login-name (nth 2 attrs)) "@"
			(system-name) ">")))))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-file-folder)
					      numlist flag-table)
  (let ((new-msgdb (elmo-make-msgdb))
	entity)
    (elmo-with-progress-display (elmo-folder-msgdb-create (length numlist))
	"Creating msgdb"
      (dolist (number numlist)
	(setq entity (elmo-file-msgdb-create-entity new-msgdb folder number))
	(when entity
	  (elmo-msgdb-append-entity new-msgdb entity '(new unread)))
	(elmo-progress-notify 'elmo-folder-msgdb-create)))
    new-msgdb))

(luna-define-method elmo-folder-message-file-p ((folder elmo-file-folder))
  t)

(luna-define-method elmo-message-file-name ((folder elmo-file-folder)
					    number)
  (expand-file-name (car (split-string
			  (elmo-map-message-location folder number)
			  "/"))
		    (elmo-file-folder-file-path-internal folder)))

(luna-define-method elmo-folder-message-make-temp-file-p
  ((folder elmo-file-folder))
  t)

(luna-define-method elmo-folder-diff ((folder elmo-file-folder))
  (cons nil nil))

(luna-define-method elmo-folder-message-make-temp-files ((folder
							  elmo-file-folder)
							 numbers
							 &optional
							 start-number)
  (let ((temp-dir (elmo-folder-make-temporary-directory folder))
	(cur-number (or start-number 0)))
    (dolist (number numbers)
      (elmo-copy-file
       (elmo-message-file-name folder number)
       (expand-file-name
	(number-to-string (if start-number cur-number number))
	temp-dir))
      (incf cur-number))
    temp-dir))

(luna-define-method elmo-map-message-fetch ((folder elmo-file-folder)
					    location strategy
					    &optional section unseen)
  (let ((file (expand-file-name (car (split-string location "/"))
				(elmo-file-folder-file-path-internal folder)))
	charset guess uid is-text tweak-charset)
    (when (file-exists-p file)
      (set-buffer-multibyte nil)
      (prog1
	  (insert-file-contents-as-binary file nil 0 elmo-file-fetch-max-size)
	(unless (or (std11-field-body "To")
		    (std11-field-body "Cc")
		    (std11-field-body "Subject"))
	  (setq guess (elmo-file-detect-content-type file))
	  (setq is-text (string-match "^text/" guess))
	  (when is-text
	    (when (string-match "; *charset=\\([-_a-zA-Z0-9]+\\)" guess)
	      (setq charset (intern (downcase (match-string 1 guess))))
	      (unless (mime-charset-to-coding-system charset)
		(setq charset nil)))
	    (unless charset
	      (setq tweak-charset t)
	      (set-buffer-multibyte t)
	      (decode-coding-region
	       (point-min) (point-max)
	       elmo-mime-display-as-is-coding-system)
	      (setq charset (detect-mime-charset-region (point-min)
							(point-max)))))
	  (setq uid (nth 2 (file-attributes file)))
	  (insert "From: " (concat (user-full-name uid)
				   " <"(user-login-name uid) "@"
				   (system-name) ">") "\n")
	  (insert "Subject: " (file-name-nondirectory file) "\n")
	  (insert "Date: "
		  (elmo-file-make-date-string (file-attributes file))
		  "\n")
	  (insert "Message-ID: "
		  (concat "<" (elmo-replace-in-string file "/" ":")
			  "@" (system-name) ">\n"))
	  (insert "Content-Type: "
		  guess
		  (or (and is-text tweak-charset
			   (concat
			    "; charset=" (symbol-name charset)))
		      "")
		  "\nMIME-Version: 1.0\n\n")
	  (when tweak-charset
	    (encode-mime-charset-region (point-min) (point-max) charset)
	    (set-buffer-multibyte nil)))))))

(luna-define-method elmo-map-folder-list-message-locations
  ((folder elmo-file-folder))
  (delq nil
	(mapcar
	 (lambda (file)
	   (let ((name (expand-file-name
			file
			(elmo-file-folder-file-path-internal folder))))
	     (when (not (file-directory-p name))
	       (concat
		file "/"
		(mapconcat 'number-to-string (nth 5 (file-attributes name))
			   ":")))))
	 (directory-files (elmo-file-folder-file-path-internal folder)))))

(luna-define-method elmo-folder-exists-p ((folder elmo-file-folder))
  (file-directory-p (elmo-file-folder-file-path-internal folder)))

(luna-define-method elmo-folder-list-subfolders ((folder elmo-file-folder)
						 &optional one-level)
  (when (file-directory-p (elmo-file-folder-file-path-internal folder))
    (append
     (list (elmo-folder-name-internal folder))
     (delq nil
	   (mapcar
	    (lambda (file)
	      (when (and (file-directory-p
			  (expand-file-name
			   file
			   (elmo-file-folder-file-path-internal folder)))
			 (not (string= file "."))
			 (not (string= file "..")))
		(concat (elmo-folder-name-internal folder) "/" file)))
	    (directory-files (elmo-file-folder-file-path-internal
			      folder)))))))

(require 'product)
(product-provide (provide 'elmo-file) (require 'elmo-version))

;;; elmo-file.el ends here
