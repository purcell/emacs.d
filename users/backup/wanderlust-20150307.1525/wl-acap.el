;;; wl-acap.el --- ACAP support for Wanderlust.

;; Copyright (C) 2001 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news

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

;;; Code:
;;

;;(cond
;; ((and (not (featurep 'utf-2000))
;;       (module-installed-p 'un-define))
;;  (require 'un-define))
;; ((and (featurep 'xemacs)
;;       (not (featurep 'utf-2000))
;;       (module-installed-p 'xemacs-ucs))
;;  (require 'xemacs-ucs)))
(require 'custom)
(require 'cus-edit)
(require 'wl-vars)
(require 'wl)
(require 'elmo-vars)
(require 'acap)
(require 'slp)

(defconst wl-acap-dataset-class "vendor.wanderlust")
(defconst wl-acap-entry-name "settings")

(defcustom wl-acap-user (or (getenv "USER")
			    (getenv "LOGNAME")
			    (user-login-name))
  "ACAP user."
  :type 'string
  :group 'wl)

(defcustom wl-acap-server nil
  "ACAP server.
If nil, SLP is used to find ACAP server.
If nil and SLP is not available, localhost is assumed."
  :type 'string
  :group 'wl)

(defcustom wl-acap-port nil
  "ACAP server port.
Only valid when `wl-acap-server' is non-nil.
If nil, default acap port is used."
  :type 'string
  :group 'wl)

(defcustom wl-acap-authenticate-type 'cram-md5
  "ACAP authenticate type."
  :type 'symbol
  :group 'wl)

(defcustom wl-acap-stream-type nil
  "ACAP stream type."
  :type 'symbol
  :group 'wl)

(defcustom wl-acap-extra-options nil
  "Extra options to be saved on ACAP server."
  :type '(repeat symbol)
  :group 'wl)

(defcustom wl-acap-cache-filename "acap-cache"
  "ACAP setting cache file."
  :type 'string
  :group 'wl)

;; Encoding string as BASE64 is temporal solution.
;; As far as I know, current implementation of ACAP server
;; (cyrus-smlacapd 0.5) does not accept literal argument for STORE.
(defvar wl-acap-base64-encode-options
  '(wl-template-alist
    wl-draft-config-alist)
  "Options which should be encoded with base64 to store ACAP server.")

(defcustom wl-acap-coding-system 'utf-8
  "Coding system for ACAP."
  :type 'symbol
  :group 'wl)

(defvar wl-acap-original-msgdb-directory nil)

(defun wl-acap-exit ()
  "End ACAP session."
  (when wl-acap-original-msgdb-directory
    (setq elmo-msgdb-directory wl-acap-original-msgdb-directory)))

(defun wl-acap-init ()
  "A candidate for `wl-folder-init-function'."
  (setq wl-acap-original-msgdb-directory nil)
  (condition-case err			; catch error and quit.
      (let ((service (wl-acap-find-acap-service))
	    proc entries settings folder-top type caches msgdb-dir)
	(if (null (car service))
	    (if (setq caches
		      (delq
		       nil
		       (mapcar
			(lambda (dirent)
			  (let ((dir
				 (elmo-localdir-folder-directory-internal
				  (elmo-make-folder dirent))))
			    (if (file-exists-p
				 (setq dir (expand-file-name
					    wl-acap-cache-filename
					    dir)))
				dir)))
			(elmo-folder-list-subfolders
			 (elmo-make-folder (concat "+"
						   (expand-file-name
						    "acap"
						    elmo-msgdb-directory)))))))
		(if (y-or-n-p "No ACAP service found.  Try cache? ")
		    (let (selected rpath alist)
		      (setq alist
			    (mapcar
			     (lambda (dir)
			       (setq rpath (nreverse (split-string dir "/")))
			       (cons (concat (nth 1 rpath) "@" (nth 2 rpath))
				     dir))
			     caches)
			    selected
			    (cdr (assoc
				  (completing-read
				   "Select ACAP cache: " alist nil t)
				  alist))
			    msgdb-dir (file-name-directory selected)
			    entries (elmo-object-load selected)))
		  (error "No ACAP service found"))
	      (error "No ACAP service found"))
	  (setq proc (acap-open (car service)
				wl-acap-user
				(upcase (symbol-name
					 wl-acap-authenticate-type))
				(cdr service)))
	  (setq entries (acap-response-entries
			 (acap-search proc (concat "/"
						   wl-acap-dataset-class
						   "/~/")
				      '((RETURN ("*"))))))
	  (when entries
	    (elmo-object-save
	     (expand-file-name
	      (concat "acap/" (car service) "/" wl-acap-user "/"
		      wl-acap-cache-filename)
	      elmo-msgdb-directory)
	     entries)))
	(while entries
	  (when (string= (acap-response-entry-entry (car entries))
			 wl-acap-entry-name)
	    (setq settings (car (acap-response-entry-return-data-list
				 (car entries)))
		  entries nil))
	  (setq entries (cdr entries)))
	(setq settings
	      (delq
	       'wl-acap-ignored
	       (mapcar (lambda (x)
			 (let ((sym (wl-acap-symbol (car x))))
			   (cond
			    ((and sym (eq sym 'wl-folders))
			     ;; Folders.
			     (setq wl-folder-entity
				   (wl-acap-create-folder-entity (cadr x)))
			     'wl-acap-ignored)
			    ((and sym (boundp sym))
			     (setq type (custom-variable-type sym))
			     (cons
			      sym
			      (when (cadr x)
				(cond
				 ((or (eq (car type) 'string)
				      (and (eq (car type) 'choice)
					   (memq 'string type)))
				  (if (memq sym wl-acap-base64-encode-options)
				      (wl-acap-base64-decode-string (cadr x))
				    (decode-coding-string
				     (cadr x)
				     wl-acap-coding-system)))
				 (t
				  (if (cadr x)
				      (read
				       (if (memq sym
						 wl-acap-base64-encode-options)
					   (wl-acap-base64-decode-string
					    (cadr x))
					 (read (concat
						"\""
						(decode-coding-string
						 (cadr x)
						 wl-acap-coding-system)
						"\""))
					 ))))))))
			    (t 'wl-acap-ignored))))
		       settings)))
	;; Setup options.
	(dolist (setting settings)
	  (set (car setting) (cdr setting)))
	;; Database directory becomes specific to the ACAP server.
	(setq wl-acap-original-msgdb-directory elmo-msgdb-directory)
	(setq elmo-msgdb-directory (or msgdb-dir
				       (expand-file-name
					(concat "acap/" (car service)
						"/" wl-acap-user)
					elmo-msgdb-directory)))
	(when proc (acap-close proc)))
    ((error quit)
     (when wl-acap-original-msgdb-directory
       (setq elmo-msgdb-directory wl-acap-original-msgdb-directory))
     (signal (car err) (cdr err)))))

(defun wl-acap-create-folder-entity (string)
  (with-temp-buffer
    (message "Initializing folder...")
    (let (folders entity)
      (setq string (elmo-base64-decode-string string))
      (setq string (decode-coding-string string wl-acap-coding-system))
      (insert string)
      (goto-char (point-min))
      (while (and (not (eobp))
		  (setq entity (wl-create-folder-entity-from-buffer)))
	(unless (eq entity 'ignore)
	  (wl-append folders (list entity))))
      (message "Initializing folder...done")
      (list wl-folder-desktop-name 'group folders))))

(defun wl-acap-find-acap-service ()
  (or (and wl-acap-server
	   (cons wl-acap-server wl-acap-port))
      (with-temp-buffer
	(message "Searching ACAP server...")
	(prog1 (let ((response (condition-case nil
				   (slp-findsrvs "acap")
				 (error)))
		     selected)
		 (when response
		   (if (> (length (slp-response-body response)) 1)
		       (progn
			 (setq selected
			       (completing-read
				"Select ACAP server: "
				(mapcar (lambda (body)
					  (list
					   (concat
					    (slp-response-srv-url-host
					     body)
					    (when (slp-response-srv-url-port
						   body)
					      (concat
					       ":"
					       (slp-response-srv-url-port
						body))))))
					(slp-response-body response)))
			       response
			       (catch 'done
				 (dolist (entry (slp-response-body response))
				   (when (string=
					  (concat
					   (slp-response-srv-url-host
					    entry)
					   (when
					       (slp-response-srv-url-port
						entry)
					     (concat
					      ":"
					      (slp-response-srv-url-port
					       entry))))
					  selected)
				     (throw 'done entry))))))
		     (setq response (car (slp-response-body response))))
		   (cons (slp-response-srv-url-host response)
			 (slp-response-srv-url-port response))))
	  (message "Searching ACAP server...done")))
      (cons "localhost" nil)))

(defun wl-acap-name (option)
  (let ((name (symbol-name option))
	prefix)
    (cond ((string-match "^wl-" name)
	   (setq name (substring name (match-end 0))
		 prefix "wl"))
	  ((string-match "^elmo-" name)
	   (setq name (substring name (match-end 0))
		 prefix "elmo")))
    (concat
     wl-acap-dataset-class "." prefix "."
     (mapconcat 'capitalize (split-string name "-") ""))))

(defun wl-acap-symbol (name)
  (let (case-fold-search li)
    (when (string-match (concat "^" (regexp-quote wl-acap-dataset-class)
				"\\.\\([^\\.]+\\)\\.") name)
      (setq li (list (match-string 1 name))
	    name (substring name (match-end 0)))
      (while (string-match "^[A-Z][a-z0-9]*" name)
	(setq li (cons (match-string 0 name) li))
	(setq name (substring name (match-end 0))))
      (intern (mapconcat 'downcase (nreverse li) "-")))))

(defun wl-acap-list-options ()
  (nconc (mapcar 'car (append (custom-group-members 'wl-setting nil)
			      (custom-group-members 'elmo-setting nil)))
	 wl-acap-extra-options))

(defun wl-acap-store-folders (proc)
  (with-temp-buffer
    (insert-file-contents wl-folders-file)
    (acap-store
     proc
     (list (concat "/" wl-acap-dataset-class "/~/"
		   wl-acap-entry-name)
	   (concat wl-acap-dataset-class ".wl.Folders")
	   (wl-acap-base64-encode-string (buffer-string))))))

(defun wl-acap-base64-encode-string (string)
  (elmo-base64-encode-string
   (encode-coding-string string wl-acap-coding-system)
   'no-line-break))

(defun wl-acap-base64-decode-string (string)
  (decode-coding-string
   (elmo-base64-decode-string string )
   wl-acap-coding-system))

(defun wl-acap-store ()
  "Store Wanderlust configuration to the ACAP server."
  (interactive)
  (wl-load-profile)
  (elmo-init)
  (let ((service (wl-acap-find-acap-service))
	proc settings type)
    (setq proc (acap-open (car service)
			  wl-acap-user
			  (upcase (symbol-name wl-acap-authenticate-type))
			  (cdr service)))
    (dolist (option (wl-acap-list-options))
      (setq settings
	    (cons (wl-acap-name option) settings)
	    settings
	    (cons (when (symbol-value option)
		    (setq type (custom-variable-type option))
		    (cond
		     ((or (eq (car type) 'string)
			  (and (eq (car type) 'choice)
			       (memq 'string type)))
		      (if (memq option wl-acap-base64-encode-options)
			  (wl-acap-base64-encode-string
			   (symbol-value option))
			(encode-coding-string
			 (symbol-value option)
			 wl-acap-coding-system)))
		     (t (if (memq option wl-acap-base64-encode-options)
			    (wl-acap-base64-encode-string
			     (prin1-to-string (symbol-value option)))
			  (encode-coding-string
			   (prin1-to-string (symbol-value option))
			   wl-acap-coding-system)))))
		  settings)))
    (unwind-protect
	(progn
	  (message "Storing settings...")
	  (acap-store proc
		      (nconc
		       (list
			(concat
			 "/" wl-acap-dataset-class "/~/" wl-acap-entry-name))
		       (nreverse settings)))
	  (message "Storing folders...")
	  (wl-acap-store-folders proc)
;;; Does not work correctly??
;;;	  (acap-setacl proc (list
;;;			     (concat
;;;			      "/" wl-acap-dataset-class "/~/"))
;;;		       "anyone" "") ; protect.
	  )
      (acap-close proc))
    (if (interactive-p)
	(message "Store completed."))))

(require 'product)
(product-provide (provide 'wl-acap) (require 'wl-version))

;;; wl-acap.el ends here
