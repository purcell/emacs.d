;;; elmo-net.el --- Network module for ELMO.

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
(eval-when-compile (require 'cl))

(require 'elmo-util)
(require 'elmo-dop)
(require 'elmo-vars)
(require 'elmo-cache)
(require 'elmo)

(defconst elmo-net-folder-name-syntax '((?@ [server ".+"])
					(?: [port "^[0-9]+$"])
					(?! stream-type)))

;;; ELMO net folder
(eval-and-compile
  (luna-define-class elmo-net-folder
		     (elmo-folder)
		     (user auth server port stream-type))
  (luna-define-internal-accessors 'elmo-net-folder))

;;; Session
(eval-and-compile
  (autoload 'starttls-negotiate "starttls")
  (autoload 'sasl-find-mechanism "sasl")
  (autoload 'sasl-make-client "sasl")
  (autoload 'sasl-mechanism-name "sasl")
  (autoload 'sasl-next-step "sasl")
  (autoload 'sasl-step-data "sasl")
  (autoload 'sasl-step-set-data "sasl"))

(defvar sasl-mechanisms)

(defcustom elmo-network-session-idle-timeout nil
  "Idle timeout of the network cache. Specified in seconds.
If elapsed time since last access is larger than this value,
cached session is not reused.
If nil, network cache is reused."
  :type '(choice number (const nil))
  :group 'elmo)

(defcustom elmo-network-session-retry-count nil
  "Retry count for authentication when open network session.
If nil, just once. If t, until success."
  :type '(choice (integer :tag "Times")
		 (const :tag "Just once" nil)
		 (const :tag "Until success" t))
  :group 'elmo)

;;; Code:
;;
(eval-and-compile
  (luna-define-class elmo-network-session () (name
					      server
					      port
					      user
					      auth
					      stream-type
					      process
					      greeting
					      last-accessed))
  (luna-define-internal-accessors 'elmo-network-session))

(luna-define-generic elmo-network-initialize-session (session)
  "Initialize SESSION (Called before authentication).")

(luna-define-generic elmo-network-initialize-session-buffer (session buffer)
  "Initialize SESSION's BUFFER.")

(luna-define-generic elmo-network-authenticate-session (session)
  "Authenticate SESSION.")

(luna-define-generic elmo-network-setup-session (session)
  "Setup SESSION. (Called after authentication).")

(luna-define-generic elmo-network-close-session (session)
  "Close SESSION.")

(luna-define-method
  elmo-network-initialize-session-buffer ((session
					   elmo-network-session) buffer)
  (with-current-buffer buffer
    (set-buffer-multibyte nil)
    (buffer-disable-undo (current-buffer))))

(luna-define-method elmo-network-close-session ((session elmo-network-session))
  (when (elmo-network-session-process-internal session)
;;;    (memq (process-status (elmo-network-session-process-internal session))
;;;	  '(open run))
    (kill-buffer (process-buffer
		  (elmo-network-session-process-internal session)))
    (delete-process (elmo-network-session-process-internal session))))

(defmacro elmo-network-stream-type-spec-string (stream-type)
  `(nth 0 ,stream-type))

(defmacro elmo-network-stream-type-symbol (stream-type)
  `(nth 1 ,stream-type))

(defmacro elmo-network-stream-type-feature (stream-type)
  `(nth 2 ,stream-type))

(defmacro elmo-network-stream-type-function (stream-type)
  `(nth 3 ,stream-type))

(defsubst elmo-network-session-password-key (session)
  (format "%s:%s/%s@%s:%d"
	  (upcase
	   (nth 1 (split-string (symbol-name
				 (luna-class-name session)) "[4-]")))
	  (elmo-network-session-user-internal session)
	  (elmo-network-session-auth-internal session)
	  (elmo-network-session-server-internal session)
	  (elmo-network-session-port-internal session)))

(defvar elmo-network-session-cache nil)

(defsubst elmo-network-session-cache-key (name folder)
  "Returns session cache key for NAME and FOLDER."
  (format "%s:%s/%s@%s:%d%s"
	  name
	  (elmo-net-folder-user-internal folder)
	  (elmo-net-folder-auth-internal folder)
	  (elmo-net-folder-server-internal folder)
	  (elmo-net-folder-port-internal folder)
	  (or
	   (elmo-network-stream-type-spec-string
	    (elmo-net-folder-stream-type-internal folder)) "")))

(defun elmo-network-clear-session-cache ()
  "Clear session cache."
  (interactive)
  (dolist (pair elmo-network-session-cache)
    (elmo-network-close-session (cdr pair)))
  (setq elmo-network-session-cache nil))

(defsubst elmo-network-session-buffer-name (session)
  (format " *%s session for %s@%s:%d%s"
	  (elmo-network-session-name-internal session)
	  (elmo-network-session-user-internal session)
	  (elmo-network-session-server-internal session)
	  (elmo-network-session-port-internal session)
	  (or (elmo-network-stream-type-spec-string
	       (elmo-network-session-stream-type-internal session))
	      "")))

(defmacro elmo-network-session-buffer (session)
  "Get buffer for SESSION."
  `(process-buffer (elmo-network-session-process-internal ,session)))

(defun elmo-network-get-session (class name folder &optional if-exists)
  "Get network session from session cache or a new network session.
CLASS is the class name of the session.
NAME is the name of the process.
FOLDER is the ELMO folder structure.
Returns a `elmo-network-session' instance.
If optional argument IF-EXISTS is non-nil, it does not return session
if there is no session cache.
if making session failed, returns nil."
  (let (pair session key)
    (if (not (elmo-plugged-p
	      (elmo-net-folder-server-internal folder)
	      (elmo-net-folder-port-internal folder)
	      (elmo-network-stream-type-symbol
	       (elmo-net-folder-stream-type-internal folder))))
	(error "Unplugged"))
    (setq pair (assoc (setq key (elmo-network-session-cache-key name folder))
		      elmo-network-session-cache))
    (when (and pair
	       (or (not (memq (process-status
			       (elmo-network-session-process-internal
				(cdr pair)))
			      '(open run)))
		   (and elmo-network-session-idle-timeout
			(elmo-network-session-last-accessed-internal
			 (cdr pair))
			(elmo-time-expire
			 (elmo-network-session-last-accessed-internal
			  (cdr pair))
			 elmo-network-session-idle-timeout))))
      (setq elmo-network-session-cache
	    (delq pair elmo-network-session-cache))
      (elmo-network-close-session (cdr pair))
      (setq pair nil))
    (if pair
	(progn
	  (elmo-network-session-set-last-accessed-internal
	   (cdr pair) (current-time))
	  (cdr pair))			; connection cache exists.
      (unless if-exists
	(setq session
	      (elmo-network-open-session
	       class
	       name
	       (elmo-net-folder-server-internal folder)
	       (elmo-net-folder-port-internal folder)
	       (elmo-net-folder-user-internal folder)
	       (elmo-net-folder-auth-internal folder)
	       (elmo-net-folder-stream-type-internal folder)))
	(setq elmo-network-session-cache
	      (cons (cons key session)
		    elmo-network-session-cache))
	session))))

(defun elmo-network-session-buffer-create (session)
  (let ((buffer-name (elmo-network-session-buffer-name session))
	buffer)
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (setq buffer (get-buffer-create buffer-name))
    (elmo-network-initialize-session-buffer session buffer)
    buffer))

(defun elmo-network-open-session (class name server port user auth
					stream-type)
  "Open an authenticated network session.
CLASS is the class name of the session.
NAME is the name of the process.
SERVER is the name of the server server.
PORT is the port number of the service.
USER is the user-id for the authenticate.
AUTH is the authenticate method name (symbol).
STREAM-TYPE is the stream type (See also `elmo-network-stream-type-alist').
Returns a process object.  if making session failed, returns nil."
  (let ((session
	 (luna-make-entity class
			   :name name
			   :server server
			   :port port
			   :user user
			   :auth auth
			   :stream-type stream-type
			   :process nil
			   :greeting nil
			   :last-accessed (current-time)))
	(retry elmo-network-session-retry-count)
	success)
    (while (not success)
      (condition-case error
	  (when (elmo-network-session-set-process-internal
		 session
		 (elmo-open-network-stream
		  (elmo-network-session-name-internal session)
		  (elmo-network-session-buffer-create session)
		  server port stream-type))
	    (elmo-network-initialize-session session)
	    (elmo-network-authenticate-session session)
	    (elmo-network-setup-session session)
	    (setq success t))
	(elmo-authenticate-error
	 (elmo-remove-passwd (elmo-network-session-password-key session))
	 (message "Authetication is failed")
	 (sit-for 1)
	 (elmo-network-close-session session)
	 (unless (if (numberp retry)
		     (> (setq retry (1- retry)) 0)
		   retry)
	   (signal (car error) (cdr error))))
	(elmo-open-error
	 (elmo-set-plugged nil server port
			   (elmo-network-stream-type-symbol stream-type)
			   (current-time))
	 (message "Auto plugged off at %s:%d :%s" server port (cadr error))
	 (sit-for 1)
	 (elmo-network-close-session session)
	 (signal (car error) (cdr error)))
	(error
	 (elmo-network-close-session session)
	 (signal (car error) (cdr error)))))
    session))

(defun elmo-open-network-stream (name buffer server service stream-type)
  (let ((auto-plugged (and elmo-auto-change-plugged
			   (> elmo-auto-change-plugged 0)))
	process)
    (if (and stream-type
	     (elmo-network-stream-type-feature stream-type))
	(require (elmo-network-stream-type-feature stream-type)))
    (condition-case err
	(let (process-connection-type)
	  (as-binary-process
	   (setq process
		 (if stream-type
		     (funcall (elmo-network-stream-type-function stream-type)
			      name buffer server service)
		   (open-network-stream name buffer server service)))
	   (unless (and (processp process)
			(memq (process-status process) '(open run)))
	     (error "Open network connection to %s:%d failed"
		    server service))))
      (error
       (when auto-plugged
	 (elmo-set-plugged nil server service
			   (elmo-network-stream-type-symbol stream-type)
			   (current-time))
	 (message "Auto plugged off at %s:%d" server service)
	 (sit-for 1))
       (signal (car err) (cdr err))))
    (when process
      (process-kill-without-query process)
      (when auto-plugged
	(elmo-set-plugged t server service
			  (elmo-network-stream-type-symbol stream-type)))
      process)))

(defun elmo-get-network-stream-type (symbol)
  "Return network stream type corresponding to SYMBOL.
Returned value is searched from `elmo-network-stream-type-alist'."
  (let ((alist elmo-network-stream-type-alist)
	spec)
    (while alist
      (when (eq (nth 1 (car alist)) symbol)
	(setq spec (car alist))
	(setq alist nil))
      (setq alist (cdr alist)))
    spec))

(defun elmo-net-folder-set-parameters (folder params &optional defaults)
  (let ((port (cdr (assq 'port params)))
	(stream-type (cdr (assq 'stream-type params))))
    ;; server
    (elmo-net-folder-set-server-internal
     folder
     (or (cdr (assq 'server params))
	 (plist-get defaults :server)))
    ;; port
    (elmo-net-folder-set-port-internal
     folder
     (or (and port (string-to-number port))
	 (plist-get defaults :port)))
    ;; stream-type
    (elmo-net-folder-set-stream-type-internal
     folder
     (or (and stream-type
	      (assoc (concat "!" stream-type) elmo-network-stream-type-alist))
	 (plist-get defaults :stream-type)))))

(luna-define-method elmo-folder-initialize ((folder elmo-net-folder) name)
  ;; user and auth should be set in subclass.
  (when (string-match "\\(@[^@:/!]+\\)?\\(:[0-9]+\\)?\\(!.*\\)?$" name)
    (elmo-net-folder-set-parameters
     folder
     (car (elmo-parse-separated-tokens
	   (substring name (match-beginning 0))
	   elmo-net-folder-name-syntax))))
  folder)

(luna-define-method elmo-net-port-info ((folder elmo-net-folder))
  (list (elmo-net-folder-server-internal folder)
	(elmo-net-folder-port-internal folder)
	(elmo-network-stream-type-symbol
	 (elmo-net-folder-stream-type-internal folder))))

(defun elmo-net-port-label (folder)
  (concat
   (symbol-name (elmo-folder-type-internal folder))
   (if (elmo-net-folder-stream-type-internal folder)
       (concat "!" (symbol-name
		    (elmo-network-stream-type-symbol
		     (elmo-net-folder-stream-type-internal
		      folder)))))))

(luna-define-method elmo-folder-plugged-p ((folder elmo-net-folder))
  (apply 'elmo-plugged-p
	 (append (elmo-net-port-info folder)
		 (list nil (quote (elmo-net-port-label folder))))))

(luna-define-method elmo-folder-set-plugged ((folder elmo-net-folder)
					     plugged &optional add)
  (apply 'elmo-set-plugged plugged
	 (append (elmo-net-port-info folder)
		 (list nil nil (quote (elmo-net-port-label folder)) add))))

(luna-define-method elmo-folder-create ((folder elmo-net-folder))
  (if (elmo-folder-plugged-p folder)
      (elmo-folder-send folder 'elmo-folder-create-plugged)
    (elmo-folder-send folder 'elmo-folder-create-unplugged)))

(luna-define-method elmo-folder-create-unplugged ((folder elmo-net-folder))
  (if elmo-enable-disconnected-operation
      (elmo-folder-create-dop folder)
    (error "Unplugged")))

(luna-define-method elmo-folder-exists-p ((folder elmo-net-folder))
  (if (elmo-folder-plugged-p folder)
      (elmo-folder-send folder 'elmo-folder-exists-p-plugged)
    ;; If unplugged, guess by msgdb.
    (file-directory-p (elmo-folder-msgdb-path folder))))

(luna-define-method elmo-folder-status ((folder elmo-net-folder))
  (if (elmo-folder-plugged-p folder)
      (elmo-folder-send folder 'elmo-folder-status-plugged)
    (elmo-folder-send folder 'elmo-folder-status-unplugged)))

(luna-define-method elmo-folder-status-unplugged
  ((folder elmo-net-folder))
  (if elmo-enable-disconnected-operation
      (elmo-folder-status-dop folder)
    (error "Unplugged")))

(luna-define-method elmo-folder-next-message-number ((folder elmo-net-folder))
  (if (elmo-folder-plugged-p folder)
      (elmo-folder-send folder 'elmo-folder-next-message-number-plugged)
    (elmo-folder-send folder 'elmo-folder-next-message-number-unplugged)))

(luna-define-method elmo-folder-next-message-number-unplugged
  ((folder elmo-net-folder))
  (if elmo-enable-disconnected-operation
      (elmo-folder-next-message-number-dop folder)
    (error "Unplugged")))

(luna-define-method elmo-folder-list-messages-internal
  ((folder elmo-net-folder) &optional nohide)
  (elmo-net-folder-list-messages-internal folder nohide))

(defun elmo-net-folder-list-messages-internal (folder nohide)
  (if (elmo-folder-plugged-p folder)
      (elmo-folder-send folder 'elmo-folder-list-messages-plugged nohide)
    (elmo-folder-send folder 'elmo-folder-list-messages-unplugged)))

(luna-define-method elmo-folder-list-messages-plugged
  ((folder elmo-net-folder))
  nil)

;; Should consider offline append and removal.
(luna-define-method elmo-folder-list-messages-unplugged ((folder
							  elmo-net-folder))
  (if elmo-enable-disconnected-operation
      (let ((deleting (elmo-dop-list-deleting-messages folder)))
	(nconc
	 ;; delete deleting messages
	 (elmo-delete-if
	  (lambda (number) (memq number deleting))
	  ;; current number-list.
	  (elmo-folder-list-messages folder nil 'in-msgdb))
	 ;; append appending messages
	 (mapcar (lambda (x) (* -1 x))
		 (elmo-dop-spool-folder-list-messages folder))))
    t))

(luna-define-method elmo-folder-list-flagged-internal ((folder elmo-net-folder)
						       flag)
  (if (and (elmo-folder-plugged-p folder)
	   (elmo-folder-use-flag-p folder))
      (elmo-folder-send folder 'elmo-folder-list-flagged-plugged flag)
    ;; Should consider offline append and removal?
    t))

(luna-define-method elmo-folder-list-flagged-plugged ((folder elmo-net-folder)
						      flag)
  t)

(luna-define-method elmo-folder-delete-messages-internal ((folder
							   elmo-net-folder)
							  numbers)
   (if (elmo-folder-plugged-p folder)
       (elmo-folder-send folder 'elmo-folder-delete-messages-plugged numbers)
     (elmo-folder-send folder 'elmo-folder-delete-messages-unplugged numbers)))

(luna-define-method elmo-folder-delete-messages-unplugged ((folder
							    elmo-net-folder)
							   numbers)
  (elmo-folder-delete-messages-dop folder numbers))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-net-folder)
					      numbers flag-table)
  (if (elmo-folder-plugged-p folder)
      (elmo-folder-send folder 'elmo-folder-msgdb-create-plugged
			numbers flag-table)
    (elmo-folder-send folder 'elmo-folder-msgdb-create-unplugged
		      numbers flag-table)))

(luna-define-method elmo-folder-msgdb-create-unplugged ((folder
							 elmo-net-folder)
							numbers
							flag-table)
  ;; XXXX should be appended to already existing msgdb.
  (elmo-dop-msgdb
   (elmo-folder-msgdb-create (elmo-dop-spool-folder folder)
			     (mapcar 'abs numbers)
			     flag-table)))

(luna-define-method elmo-folder-set-flag :before ((folder elmo-net-folder)
						  numbers
						  flag
						  &optional is-local)
  (when (and (not is-local)
	     (elmo-folder-use-flag-p folder))
    (elmo-folder-send folder
		      (if (elmo-folder-plugged-p folder)
			  'elmo-folder-set-flag-plugged
			'elmo-folder-set-flag-unplugged)
		      numbers
		      flag)))

(luna-define-method elmo-folder-unset-flag :before ((folder elmo-net-folder)
						    numbers
						    flag
						    &optional is-local)
  (when (and (not is-local)
	     (elmo-folder-use-flag-p folder))
    (elmo-folder-send folder
		      (if (elmo-folder-plugged-p folder)
			  'elmo-folder-unset-flag-plugged
			'elmo-folder-unset-flag-unplugged)
		      numbers
		      flag)))

(luna-define-method elmo-folder-set-flag-unplugged ((folder elmo-net-folder)
						    numbers flag)
  (elmo-folder-set-flag-dop folder numbers flag))

(luna-define-method elmo-folder-unset-flag-unplugged ((folder elmo-net-folder)
						      numbers flag)
  (elmo-folder-unset-flag-dop folder numbers flag))

(luna-define-method elmo-message-encache :around ((folder elmo-net-folder)
						  number &optional read)
  (if (elmo-folder-plugged-p folder)
      (luna-call-next-method)
    (if elmo-enable-disconnected-operation
	(elmo-message-encache-dop folder number read)
      (error "Unplugged"))))

(luna-define-generic elmo-message-fetch-plugged (folder number strategy
							&optional
							section
							outbuf
							unseen)
  "")

(luna-define-generic elmo-message-fetch-unplugged (folder number strategy
							  &optional
							  section
							  outbuf
							  unseen)
  "")

(luna-define-method elmo-message-fetch-internal ((folder elmo-net-folder)
						 number strategy
						 &optional section unseen)
  (if (elmo-folder-plugged-p folder)
      (elmo-message-fetch-plugged folder number
				  strategy section
				  (current-buffer) unseen)
    (elmo-message-fetch-unplugged folder number
				  strategy section
				  (current-buffer) unseen)))

(luna-define-method elmo-message-fetch-unplugged
  ((folder elmo-net-folder) number strategy  &optional section outbuf unseen)
  (if (and elmo-enable-disconnected-operation
	   (< number 0))
      (elmo-message-fetch-internal
       (elmo-dop-spool-folder folder) (abs number) strategy
       section unseen)
    (error "Unplugged")))

(luna-define-method elmo-folder-check ((folder elmo-net-folder))
  (if (elmo-folder-plugged-p folder)
      (elmo-folder-send folder 'elmo-folder-check-plugged)))

(luna-define-method elmo-folder-close :after ((folder elmo-net-folder))
  (if (elmo-folder-plugged-p folder)
      (elmo-folder-send folder 'elmo-folder-check-plugged)))

(luna-define-method elmo-folder-diff :around ((folder elmo-net-folder))
  (if (and (elmo-folder-use-flag-p folder)
	   (elmo-folder-plugged-p folder))
      (elmo-folder-send folder 'elmo-folder-diff-plugged)
    (luna-call-next-method)))

(luna-define-method elmo-folder-local-p ((folder elmo-net-folder))
  nil)

(luna-define-method elmo-quit ((folder elmo-net-folder))
  (elmo-network-clear-session-cache))

(require 'product)
(product-provide (provide 'elmo-net) (require 'elmo-version))

;;; elmo-net.el ends here
