;;; elmo-search.el --- Search by external program interface for ELMO.

;; Copyright (C) 2005 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Hiroya Murata <lapis-lazuli@pop06.odn.ne.jp>
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

(defcustom elmo-search-use-drive-letter
  (memq system-type '(OS/2 emx windows-nt))
  "*If non-nil, do a drive letter conversion (e.g. /a|/ => a:/)."
  :type '(choice (const :tag "Not use" nil)
		 (other :tag "Use" t))
  :group 'elmo)

(defvar elmo-search-engine-alist nil
  "*An alist of search engines.
Each element looks like (ENGINE CLASS PROPERTIES...)
ENGINE is a symbol, the name of the search engine.
CLASS is a symbol, the class name that performs a search.
PROPERTIES is a plist, it configure an engine with the CLASS.")

(defcustom elmo-search-default-engine 'namazu
  "*Default search engine for elmo-search folder."
  :type 'symbol
  :group 'elmo)


(defconst elmo-search-folder-name-syntax `(pattern (?\] param (?! engine))))


;; Search engine I/F
(eval-and-compile
  (luna-define-class elmo-search-engine () (param))
  (luna-define-internal-accessors 'elmo-search-engine))

(luna-define-generic elmo-search-engine-do-search (engine pattern)
  "Search messages which is match PATTERN by ENGINE.")

(luna-define-generic elmo-search-engine-create-message-entity (engine
							       handler
							       folder number)
  "Create msgdb entity for the message in the FOLDER with NUMBER.")

(luna-define-generic elmo-search-engine-fetch-message (engine location)
  "Fetch a message into current buffer.
ENGINE is the ELMO search engine structure.
LOCATION is the location of the message.
Returns non-nil if fetching was succeed.")

(defun elmo-make-search-engine (type &optional param)
  (let ((spec (or (cdr (assq type elmo-search-engine-alist))
		  (error "Undefined search engine `%s'" type))))
    (require (intern (format "elmo-search-%s" (car spec))))
    (apply 'luna-make-entity
	   (intern (format "elmo-search-engine-%s" (car spec)))
	   :param param
	   (cdr spec))))

(defun elmo-search-register-engine (name class &rest properties)
  (let ((cell (assq name elmo-search-engine-alist))
	(spec (cons class properties)))
    (if cell
	(setcdr cell spec)
      (setq elmo-search-engine-alist
	    (cons (cons name spec) elmo-search-engine-alist)))))

;; ELMO search folder
(eval-and-compile
  (luna-define-class elmo-search-folder (elmo-map-folder)
		     (engine pattern))
  (luna-define-internal-accessors 'elmo-search-folder))

(luna-define-method elmo-folder-initialize ((folder elmo-search-folder)
					    name)
  (when (> (length name) 0)
    (let* ((tokens (car (elmo-parse-separated-tokens
			 name
			 elmo-search-folder-name-syntax)))
	   (engine (cdr (assq 'engine tokens))))
      (elmo-search-folder-set-engine-internal
       folder
       (elmo-make-search-engine (if (> (length engine) 0)
				    (intern engine)
				  elmo-search-default-engine)
				(cdr (assq 'param tokens))))
      (elmo-search-folder-set-pattern-internal
       folder
       (cdr (assq 'pattern tokens)))))
  folder)

(luna-define-method elmo-folder-expand-msgdb-path ((folder elmo-search-folder))
  (expand-file-name
   (elmo-replace-string-as-filename
    (elmo-folder-name-internal folder))
   (expand-file-name "search" elmo-msgdb-directory)))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-search-folder)
					      numbers flag-table)
  (let ((new-msgdb (elmo-make-msgdb))
	entity)
    (elmo-with-progress-display (elmo-folder-msgdb-create (length numbers))
	"Creating msgdb"
      (dolist (number numbers)
	(setq entity (elmo-search-engine-create-message-entity
		      (elmo-search-folder-engine-internal folder)
		      (elmo-msgdb-message-entity-handler new-msgdb)
		      folder number))
	(when entity
	  (elmo-msgdb-append-entity new-msgdb entity '(new unread)))
	(elmo-progress-notify 'elmo-folder-msgdb-create)))
    new-msgdb))

(luna-define-method elmo-folder-message-file-p ((folder elmo-search-folder))
  nil)

(defun elmo-search-location-to-filename (location)
  (when (string-match "^file://" location)
    (let ((filename (substring location (match-end 0))))
      (expand-file-name
       (if (and elmo-search-use-drive-letter
		(string-match "^/\\([A-Za-z]\\)[:|]/\\(.*\\)$" filename))
	   (replace-match "\\1:/\\2" t nil filename)
	 filename)))))

(luna-define-method elmo-message-file-name ((folder elmo-search-folder)
					    number)
  (elmo-search-location-to-filename
   (elmo-map-message-location folder number)))

(luna-define-method elmo-folder-message-make-temp-file-p
  ((folder elmo-search-folder))
  nil)

(luna-define-method elmo-folder-diff ((folder elmo-search-folder))
  (cons nil nil))

(luna-define-method elmo-folder-message-make-temp-files ((folder
							  elmo-search-folder)
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

(luna-define-method elmo-map-message-fetch ((folder elmo-search-folder)
					    location strategy
					    &optional section unseen)
  (elmo-search-engine-fetch-message
   (elmo-search-folder-engine-internal folder)
   location))

(luna-define-method elmo-map-folder-list-message-locations
  ((folder elmo-search-folder))
  (elmo-search-engine-do-search
   (elmo-search-folder-engine-internal folder)
   (elmo-search-folder-pattern-internal folder)))

(luna-define-method elmo-folder-exists-p ((folder elmo-search-folder))
  (elmo-search-folder-pattern-internal folder))

(luna-define-method elmo-folder-have-subfolder-p ((folder elmo-search-folder))
  (null (elmo-search-folder-pattern-internal folder)))

(luna-define-method elmo-folder-list-subfolders ((folder elmo-search-folder)
						 &optional one-level)
  (mapcar
   (lambda (name) (elmo-recover-string-from-filename name))
   (directory-files (expand-file-name "search" elmo-msgdb-directory)
		    nil
		    (concat "^" (regexp-quote
				 (elmo-folder-prefix-internal folder))))))

(luna-define-method elmo-folder-delete-messages ((folder elmo-search-folder)
						 numbers)
  (elmo-folder-kill-messages folder numbers)
  t)


;;; Search engine

;; external program search engine
(eval-and-compile
  (luna-define-class elmo-search-engine-extprog (elmo-search-engine)
		     (prog args charset parser))
  (luna-define-internal-accessors 'elmo-search-engine-extprog))

(luna-define-method elmo-search-engine-do-search
  ((engine elmo-search-engine-extprog) pattern)
  (with-temp-buffer
    (let ((charset (elmo-search-engine-extprog-charset-internal engine))
	  (parser (or (elmo-search-engine-extprog-parser-internal engine)
		      #'elmo-search-parse-filename-list)))
      (apply 'call-process
	     (elmo-search-engine-extprog-prog-internal engine)
	     nil t t
	     (delq
	      nil
	      (elmo-flatten
	       (mapcar
		(lambda (arg)
		  (cond ((stringp arg) arg)
			((eq arg 'pattern)
			 (if charset
			     (encode-mime-charset-string pattern charset)
			   pattern))
			((functionp arg)
			 (condition-case nil
			     (funcall arg engine pattern)
			   (wrong-number-of-arguments
			    (funcall arg engine))))
			((and (symbolp arg)
			      (boundp arg))
			 (symbol-value arg))))
		(elmo-search-engine-extprog-args-internal engine)))))
      (funcall parser))))

;; search engine for local files
(eval-and-compile
  (luna-define-class elmo-search-engine-local-file
		     (elmo-search-engine-extprog))
  (luna-define-internal-accessors 'elmo-search-engine-local-file))

(defun elmo-search-parse-filename-list ()
  (let (bol locations)
    (goto-char (point-min))
    (while (not (eobp))
      (beginning-of-line)
      (when (and elmo-search-use-drive-letter
		 (looking-at "^\\([A-Za-z]\\)[:|]/"))
	(replace-match "/\\1:/")
	(beginning-of-line))
      (unless (looking-at "^file://")
	(insert "file://")
	(beginning-of-line))
      (setq bol (point))
      (end-of-line)
      (setq locations (cons (buffer-substring bol (point)) locations))
      (forward-line))
    (nreverse locations)))

(luna-define-method elmo-search-engine-create-message-entity
  ((engine elmo-search-engine-local-file) handler folder number)
  (let ((filename (elmo-message-file-name folder number))
	entity uid)
    (when (and filename
	       (setq entity (elmo-msgdb-create-message-entity-from-file
			     handler number filename)))
      (unless (or (elmo-message-entity-field entity 'to)
		  (elmo-message-entity-field entity 'cc)
		  (not (string= (elmo-message-entity-field entity 'subject)
				elmo-no-subject)))
	(elmo-message-entity-set-field entity 'subject
				       (file-name-nondirectory filename))
	(setq uid (nth 2 (file-attributes filename)))
	(elmo-message-entity-set-field entity 'from
				       (concat
					(user-full-name uid)
					" <"(user-login-name uid) "@"
					(system-name) ">")))
      entity)))

(luna-define-method elmo-search-engine-fetch-message
  ((engine elmo-search-engine-local-file) location)
  (let ((filename (elmo-search-location-to-filename location)))
    (when (and filename (file-exists-p filename))
      (prog1
	  (insert-file-contents-as-binary filename)
	(unless (or (std11-field-body "To")
		    (std11-field-body "Cc")
		    (std11-field-body "Subject"))
	  (let (charset guess uid)
	    (erase-buffer)
	    (set-buffer-multibyte t)
	    (insert-file-contents filename)
	    (setq charset (detect-mime-charset-region (point-min)
						      (point-max)))
	    (goto-char (point-min))
	    (setq guess (mime-find-file-type filename))
	    (setq uid (nth 2 (file-attributes filename)))
	    (insert "From: " (concat (user-full-name uid)
				     " <"(user-login-name uid) "@"
				     (system-name) ">") "\n")
	    (insert "Subject: " filename "\n")
	    (insert "Content-Type: "
		    (concat (nth 0 guess) "/" (nth 1 guess))
		    "; charset=" (upcase (symbol-name charset))
		    "\nMIME-Version: 1.0\n\n")
	    (encode-mime-charset-region (point-min) (point-max) charset)
	    (set-buffer-multibyte nil)))))))

(provide 'elmo-search-local-file)

;; namazu
(defcustom elmo-search-namazu-default-index-path "~/Mail"
  "*Default index path for namazu.
If the value is a list, all elements are used as index paths for namazu."
  :type '(choice (directory :tag "Index Path")
		 (repeat (directory :tag "Index Path")))
  :group 'elmo)

(defcustom elmo-search-namazu-index-alias-alist nil
  "*Alist of ALIAS and INDEX-PATH."
  :type '(repeat (cons (string :tag "Alias Name")
		       (choice (directory :tag "Index Path")
			       (repeat (directory :tag "Index Path")))))
  :group 'elmo)

(defun elmo-search-namazu-index (engine pattern)
  (let* ((param (elmo-search-engine-param-internal engine))
	 (index (cond
		 ((cdr (assoc param elmo-search-namazu-index-alias-alist)))
		 ((and param (> (length param) 0))
		  param)
		 (t
		  elmo-search-namazu-default-index-path))))
    (if (listp index)
	(mapcar 'expand-file-name index)
      (expand-file-name index))))


;; grep
(defun elmo-search-grep-target (engine pattern)
  (let ((dirname (expand-file-name (elmo-search-engine-param-internal engine)))
	(files (list null-device)))
    (dolist (filename (directory-files dirname))
      (unless (string-match "^\\.\\.?" filename)
	(setq files (cons (expand-file-name filename dirname) files))))
    files))

(defun elmo-search-split-pattern-list (engine pattern)
  "ENGINE is ignored.  Splits query PATTERN into list of strings, with ' and \" quoting phrases."
  (split-string-and-unquote
   (elmo-search-replace-single-quotes engine pattern)))

(defun elmo-search-replace-single-quotes (engine pattern)
  "ENGINE is ignored.  Replace single quotes with double quotes in PATTERN."
  (replace-regexp-in-string "\'" "\"" pattern nil t))

;;; Setup `elmo-search-engine-alist'
(unless noninteractive
  (or (assq 'namazu elmo-search-engine-alist)
      (elmo-search-register-engine
       'namazu 'local-file
       :prog "namazu"
       :args '("--all" "--list" "--early" pattern elmo-search-namazu-index)
       :charset 'iso-2022-jp))
  (or (assq 'grep elmo-search-engine-alist)
      (elmo-search-register-engine
       'grep 'local-file
       :prog "grep"
       :args '("-l" "-e" pattern elmo-search-grep-target)))
  (or (assq 'mu elmo-search-engine-alist)
      (elmo-search-register-engine
       'mu 'local-file
       :prog "mu"
       :args '("find" elmo-search-split-pattern-list "--fields" "l")
       :charset 'utf-8))
  (or (assq 'notmuch elmo-search-engine-alist)
      (elmo-search-register-engine
       'notmuch 'local-file
       :prog "notmuch"
       :args '("search" "--output=files" elmo-search-replace-single-quotes)
       :charset 'utf-8)))

(require 'product)
(product-provide (provide 'elmo-search) (require 'elmo-version))

;;; elmo-search.el ends here
