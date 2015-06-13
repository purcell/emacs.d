;;; elmo-mime.el --- MIME module for ELMO.

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
(require 'elmo-vars)
(require 'mmbuffer)
(require 'mmimap)
(require 'mime-view)

(eval-when-compile
  (require 'luna)
  (require 'elmo)	      ; elmo-folder-do-each-message-entity
  (require 'cl))

;; MIME-Entity
(eval-and-compile
  (luna-define-class elmo-mime-entity))

(luna-define-generic elmo-mime-entity-display-p (entity mime-mode)
  "Return non-nil if ENTITY is able to display with MIME-MODE.

MIME-MODE is a symbol which is one of the following:
  `mime'  (Can display each MIME part)
  `as-is' (Can display raw message)")

(luna-define-generic elmo-mime-entity-reassembled-p (entity)
  "Return non-nil if ENTITY is reassembled message/partial pieces.")

(luna-define-generic elmo-mime-entity-display (entity preview-buffer
						      &optional
						      original-major-mode
						      keymap)
  "Display MIME message ENTITY.
PREVIEW-BUFFER is a view buffer.
Optional argument ORIGINAL-MAJOR-MODE is major-mode of representation
buffer of ENTITY.  If it is nil, current `major-mode' is used.
If optional argument KEYMAP is specified,
use for keymap of representation buffer.")

(luna-define-generic elmo-mime-entity-display-as-is (entity
						     preview-buffer
						     &optional
						     original-major-mode
						     keymap)
  "Display MIME message ENTITY as is.
PREVIEW-BUFFER is a view buffer.
Optional argument ORIGINAL-MAJOR-MODE is major-mode of representation
buffer of ENTITY.  If it is nil, current `major-mode' is used.
If optional argument KEYMAP is specified,
use for keymap of representation buffer.")

(luna-define-method elmo-mime-entity-display ((entity elmo-mime-entity)
					      preview-buffer
					      &optional
					      original-major-mode
					      keymap)
  (let ((elmo-message-displaying t)
	(default-mime-charset 'x-unknown))
    (mime-display-message entity
			  preview-buffer
			  nil
			  keymap
			  original-major-mode)))

(defun elmo-mime-entity-fragment-p (entity)
  (and (not (elmo-mime-entity-reassembled-p entity))
       (eq (mime-entity-media-type entity) 'message)
       (eq (mime-entity-media-subtype entity) 'partial)))

(eval-and-compile
  (luna-define-class mime-elmo-buffer-entity (mime-buffer-entity
					      elmo-mime-entity)
		     (reassembled))
  (luna-define-internal-accessors 'mime-elmo-buffer-entity)
  (luna-define-class mime-elmo-imap-entity (mime-imap-entity
					    elmo-mime-entity)))

;; Provide backend
(provide 'mmelmo-imap)
(provide 'mmelmo-buffer)

(defvar elmo-message-ignored-field-list mime-view-ignored-field-list)
(defvar elmo-message-visible-field-list mime-view-visible-field-list)
(defvar elmo-message-sorted-field-list nil)
(defvar elmo-mime-display-header-analysis t)

(defcustom elmo-mime-header-max-column 'fill-column
  "*Header max column number. Default is `fill-colmn'.
If a symbol of variable is specified, use its value in message buffer.
If a symbol of function is specified, the function is called and its return
value is used."
  :type '(choice (integer :tag "Column Number")
		 (variable :tag "Variable")
		 (function :tag "Function"))
  :group 'elmo)

(luna-define-method initialize-instance :after ((entity mime-elmo-buffer-entity)
						&rest init-args)
  entity)

(luna-define-method initialize-instance :around ((entity mime-elmo-imap-entity)
						 &rest init-args)
  (luna-call-next-method))

;;; Insert sorted header.
(defsubst elmo-mime-insert-header-from-buffer (buffer
					       start end
					       &optional invisible-fields
					       visible-fields
					       sort-fields)
  (let ((the-buf (current-buffer))
	(max-column (cond ((functionp elmo-mime-header-max-column)
			   (funcall elmo-mime-header-max-column))
			  ((and (symbolp elmo-mime-header-max-column)
				(boundp elmo-mime-header-max-column))
			   (symbol-value elmo-mime-header-max-column))
			  (t
			   elmo-mime-header-max-column)))
	vf-alist)
    (with-current-buffer buffer
      (save-restriction
	(narrow-to-region start end)
	(goto-char start)
	(while (re-search-forward std11-field-head-regexp nil t)
	  (let* ((field-start (match-beginning 0))
		 (name-end (match-end 0))
		 (field-name (buffer-substring field-start name-end)))
	    (when (mime-visible-field-p field-name
					visible-fields invisible-fields)
	      (let* ((field (intern
			     (capitalize
			      (buffer-substring field-start (1- name-end)))))
		     (field-body (buffer-substring name-end (std11-field-end)))
		     (field-decoder
		      (and elmo-mime-display-header-analysis
			   (inline (mime-find-field-decoder field 'wide)))))
		(setq vf-alist (cons (list field-name field-body field-decoder)
				     vf-alist)))))))
      (and vf-alist
	   (setq vf-alist
		 (sort vf-alist
		       (lambda (s d)
			 (let ((sf (car s))
			       (df (car d)))
			   (catch 'done
			     (dolist (re sort-fields)
			       (when (string-match re sf)
				 (throw 'done t))
			       (when (string-match re df)
				 (throw 'done nil)))
			     t)))))))
    (set-buffer the-buf)		; verbose. remove me.
    (save-excursion
      (while vf-alist
	(let* ((vf (car vf-alist))
	       (field-name (nth 0 vf))
	       (field-body (nth 1 vf))
	       (field-decoder (nth 2 vf)))
	  (insert field-name)
	  (insert (or (and field-decoder
			   (ignore-errors
			    (funcall field-decoder field-body
				     (string-width field-name)
				     max-column)))
		      ;; Don't decode
		      field-body))
	  (insert "\n"))
	(setq vf-alist (cdr vf-alist)))
      (run-hooks 'mmelmo-header-inserted-hook))))

(luna-define-generic elmo-mime-insert-sorted-header (entity
						     &optional invisible-fields
						     visible-fields
						     sorted-fields)
  "Insert sorted header fields of the ENTITY.")

(luna-define-method elmo-mime-insert-sorted-header ((entity
						     mime-elmo-buffer-entity)
						    &optional invisible-fields
						    visible-fields
						    sorted-fields)
  (elmo-mime-insert-header-from-buffer
   (mime-buffer-entity-buffer-internal entity)
   (mime-buffer-entity-header-start-internal entity)
   (mime-buffer-entity-header-end-internal entity)
   invisible-fields visible-fields sorted-fields))

(luna-define-method elmo-mime-insert-sorted-header ((entity
						     mime-elmo-imap-entity)
						    &optional invisible-fields
						    visible-fields
						    sorted-fields)
  (let ((the-buf (current-buffer))
	buf p-min p-max)
    (with-temp-buffer
      (insert (mime-imap-entity-header-string entity))
      (setq buf (current-buffer)
	    p-min (point-min)
	    p-max (point-max))
      (set-buffer the-buf)
      (elmo-mime-insert-header-from-buffer buf p-min p-max
					   invisible-fields
					   visible-fields
					   sorted-fields))))

(luna-define-method mime-insert-text-content :around
  ((entity mime-elmo-buffer-entity))
  (luna-call-next-method)
  (run-hooks 'elmo-message-text-content-inserted-hook))

(luna-define-method mime-insert-text-content :around
  ((entity mime-elmo-imap-entity))
  (luna-call-next-method)
  (run-hooks 'elmo-message-text-content-inserted-hook))

(defun elmo-mime-insert-header (entity situation)
  (elmo-mime-insert-sorted-header
   entity
   elmo-message-ignored-field-list
   elmo-message-visible-field-list
   elmo-message-sorted-field-list)
  (run-hooks 'elmo-message-header-inserted-hook))

;; mime-elmo-buffer-entity
(luna-define-method elmo-mime-entity-display-p
  ((entity mime-elmo-buffer-entity) mime-mode)
  ;; always return t.
  t)

(luna-define-method elmo-mime-entity-reassembled-p ((entity
						     mime-elmo-buffer-entity))
  (mime-elmo-buffer-entity-reassembled-internal entity))

(luna-define-method elmo-mime-entity-display-as-is ((entity
						     mime-elmo-buffer-entity)
						     preview-buffer
						     &optional
						     original-major-mode
						     keymap)
  (elmo-mime-display-as-is-internal entity
				    preview-buffer
				    nil
				    keymap
				    original-major-mode))

;; mime-elmo-imap-entity
(luna-define-method elmo-mime-entity-display-p
  ((entity mime-elmo-imap-entity) mime-mode)
  (not (eq mime-mode 'as-is)))

(luna-define-method elmo-mime-entity-display-as-is ((entity
						     mime-elmo-imap-entity)
						     preview-buffer
						     &optional
						     original-major-mode
						     keymap)
  (error "Does not support this method"))


(defun elmo-message-mime-entity (folder number rawbuf reassemble
					&optional
					ignore-cache unread entire)
  "Return the mime-entity structure of the message in the FOLDER with NUMBER.
RAWBUF is the output buffer for original message.
If REASSEMBLE is non-nil and MIME media type of the message is message/partial,
the mime-entity is reassembled partial message.
If optional argument IGNORE-CACHE is non-nil, existing cache is ignored.
If second optional argument UNREAD is non-nil,
keep status of the message as unread.
If third optional argument ENTIRE is non-nil, fetch entire message at once."
  (let (id message entity content-type)
    (or (and reassemble
	     (setq entity (elmo-message-entity folder number))
	     (setq id (if (setq content-type (elmo-message-entity-field
					      entity 'content-type))
			  (and (string-match "message/partial" content-type)
			       (mime-content-type-parameter
				(mime-parse-Content-Type content-type) "id"))
			(and (setq message (elmo-message-mime-entity-internal
					    folder number rawbuf
					    ignore-cache unread entire))
			     (eq (mime-entity-media-type message) 'message)
			     (eq (mime-entity-media-subtype message) 'partial)
			     (mime-content-type-parameter
			      (mime-entity-content-type message) "id"))))
	     (elmo-message-reassembled-mime-entity
	      folder id rawbuf
	      (elmo-message-entity-field entity 'subject)
	      ignore-cache
	      unread))
	message
	(elmo-message-mime-entity-internal
	 folder number rawbuf ignore-cache unread entire))))


(defun elmo-message-mime-entity-internal (folder number rawbuf
						 &optional
						 ignore-cache unread entire)
  (let ((strategy (elmo-find-fetch-strategy folder number
					    ignore-cache
					    entire)))
    (cond ((null strategy) nil)
	  ((eq (elmo-fetch-strategy-entireness strategy) 'section)
	   (mime-open-entity
	    'elmo-imap
	    (luna-make-entity 'mime-elmo-imap-location
			      :folder folder
			      :number number
			      :rawbuf rawbuf
			      :strategy strategy)))
	  (t
	   (with-current-buffer rawbuf
	     (let (buffer-read-only)
	       (erase-buffer)
	       (elmo-message-fetch folder number strategy unread)))
	   (mime-open-entity 'elmo-buffer rawbuf)))))


(defconst elmo-mime-inherit-field-list-from-enclosed
  '("^Content-.*:" "^Message-Id:" "^Subject:"
    "^Encrypted.*:" "^MIME-Version:"))

(defsubst elmo-mime-make-reassembled-mime-entity (buffer)
  (let ((entity (mime-open-entity 'elmo-buffer buffer)))
    (mime-elmo-buffer-entity-set-reassembled-internal entity t)
    entity))

(defun elmo-message-reassembled-mime-entity (folder id rawbuf subject
						    &optional
						    ignore-cache
						    unread)
  (let ((cache (elmo-file-cache-get (concat "<" id ">")))
	pieces)
    (if (and (not ignore-cache)
	     (eq (elmo-file-cache-status cache) 'entire))
	;; use cache
	(with-current-buffer rawbuf
	  (let (buffer-read-only)
	    (erase-buffer)
	    (elmo-file-cache-load (elmo-file-cache-path cache) nil))
	  (elmo-mime-make-reassembled-mime-entity rawbuf))
      ;; reassemble fragment of the entity
      (when (setq pieces (elmo-mime-collect-message/partial-pieces
			  folder id
			  (regexp-quote
			   (if (string-match "[0-9\n]+" subject)
			       (substring subject 0 (match-beginning 0))
			     subject))
			  ignore-cache unread))
	(with-current-buffer rawbuf
	  (let (buffer-read-only
		(outer-header (car pieces))
		(pieces (sort (cdr pieces) #'car-less-than-car))
		contents entity)
	    (erase-buffer)
	    (while pieces
	      (insert (cdr (car pieces)))
	      (setq pieces (cdr pieces)))
	    (let ((case-fold-search t))
	      (save-restriction
		(std11-narrow-to-header)
		(goto-char (point-min))
		(while (re-search-forward std11-field-head-regexp nil t)
		  (let ((field-start (match-beginning 0)))
		    (unless (mime-visible-field-p
			     (buffer-substring field-start (match-end 0))
			     elmo-mime-inherit-field-list-from-enclosed
			     '(".*"))
		      (delete-region field-start (1+ (std11-field-end))))))))
	    (goto-char (point-min))
	    (insert outer-header)
	    ;; save cache
	    (elmo-file-cache-save (elmo-file-cache-path cache) nil)
	    (elmo-mime-make-reassembled-mime-entity rawbuf)))))))

(defun elmo-mime-collect-message/partial-pieces (folder id subject-regexp
							&optional
							ignore-cache
							unread)
  (catch 'complete
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let (total header pieces)
	(elmo-folder-do-each-message-entity (entity folder)
	  (when (string-match
		 subject-regexp
		 (elmo-message-entity-field entity 'subject))
	    (erase-buffer)
	    (let* ((message (elmo-message-mime-entity-internal
			     folder
			     (elmo-message-entity-number entity)
			     (current-buffer)
			     ignore-cache
			     unread))
		   (ct (mime-entity-content-type message))
		   (the-id (or (mime-content-type-parameter ct "id") ""))
		   number)
	      (when (string= (downcase the-id)
			     (downcase id))
		(setq number (string-to-number
			      (mime-content-type-parameter ct "number")))
		(setq pieces (cons (cons number (mime-entity-body message))
				   pieces))
		(when (= number 1)
		  (let ((case-fold-search t))
		    (save-restriction
		      (std11-narrow-to-header)
		      (goto-char (point-min))
		      (while (re-search-forward std11-field-head-regexp nil t)
			(let ((field-start (match-beginning 0)))
			  (when (mime-visible-field-p
				 (buffer-substring field-start (match-end 0))
				 nil
				 elmo-mime-inherit-field-list-from-enclosed)
			    (setq header (concat
					  header
					  (buffer-substring
					   field-start (std11-field-end))
					  "\n"))))))))
		(unless total
		  (setq total (ignore-errors
				(string-to-number
				 (mime-content-type-parameter ct "total")))))
		(when (and total
			   (> total 0)
			   (>= (length pieces) total))
		  (throw 'complete (cons header pieces)))))))))
    ;; return value
    nil))


;; Replacement of mime-display-message.
(defun elmo-mime-display-as-is-internal (message
					 &optional preview-buffer
					 mother default-keymap-or-function
					 original-major-mode keymap)
  (mime-maybe-hide-echo-buffer)
  (let ((win-conf (current-window-configuration)))
    (or preview-buffer
	(setq preview-buffer
	      (concat "*Preview-" (mime-entity-name message) "*")))
    (or original-major-mode
	(setq original-major-mode major-mode))
    (let ((inhibit-read-only t))
      (set-buffer (get-buffer-create preview-buffer))
      (widen)
      (erase-buffer)
      (if mother
	  (setq mime-mother-buffer mother))
      (setq mime-preview-original-window-configuration win-conf)
      (setq major-mode 'mime-view-mode)
      (setq mode-name "MIME-View")

      ;; Humm...
      (set-buffer-multibyte nil)
      (insert (mime-entity-body message))
      (set-buffer-multibyte t)
      (decode-coding-region (point-min) (point-max)
			    elmo-mime-display-as-is-coding-system)
      (goto-char (point-min))
      (insert "\n")
      (goto-char (point-min))

      (let ((method (cdr (assq original-major-mode
			       mime-header-presentation-method-alist))))
	(if (functionp method)
	    (funcall method message nil)))

      ;; set original major mode for mime-preview-quit
      (put-text-property (point-min) (point-max)
			 'mime-view-situation
			 `((major-mode . ,original-major-mode)))
      (put-text-property (point-min) (point-max)
			 'elmo-as-is-entity message)
      (use-local-map
       (or keymap
	   (if default-keymap-or-function
	       (mime-view-define-keymap default-keymap-or-function)
	     mime-view-mode-default-map)))
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (run-hooks 'mime-view-mode-hook)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      preview-buffer)))

(require 'product)
(product-provide (provide 'elmo-mime) (require 'elmo-version))

;; elmo-mime.el ends here
