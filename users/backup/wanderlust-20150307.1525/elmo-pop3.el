;;; elmo-pop3.el --- POP3 Interface for ELMO.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 1999,2000      Kenichi OKADA <okada@opaopa.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
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

(require 'elmo-msgdb)
(require 'elmo-net)
(require 'elmo-map)

(eval-when-compile
  (require 'cl)
  (require 'elmo-util))

(eval-and-compile
  (autoload 'md5 "md5"))

(defcustom elmo-pop3-default-use-uidl t
  "If non-nil, use UIDL on POP3."
  :type 'boolean
  :group 'elmo)

(defvar elmo-pop3-use-uidl-internal t
  "(Internal switch for using UIDL on POP3).")

(defvar elmo-pop3-use-cache t
  "Use cache in pop3 folder.")

(defvar elmo-pop3-send-command-synchronously nil
  "If non-nil, commands are send synchronously.
If server doesn't accept asynchronous commands, this variable should be
set as non-nil.")

(defcustom elmo-pop3-exists-exactly nil
  "If non-nil, POP3 folder existence is checked everytime before the session."
  :type 'boolean
  :group 'elmo)

(defconst elmo-pop3-folder-name-syntax `(([user ".+"])
					 (?/ [auth ".+"])
					 (?: [uidl "^[A-Za-z]+$"])
					 ,@elmo-net-folder-name-syntax))

(defvar sasl-mechanism-alist)

(defvar elmo-pop3-retrieve-progress-reporter nil)

;; For debugging.
(defvar elmo-pop3-debug nil
  "Non-nil forces POP3 folder as debug mode.
Debug information is inserted in the buffer \"*POP3 DEBUG*\"")

;;; Debug
(defsubst elmo-pop3-debug (message &rest args)
  (if elmo-pop3-debug
      (let ((biff (string-match "BIFF-" (buffer-name)))
	    pos)
	(with-current-buffer (get-buffer-create (concat "*POP3 DEBUG*"
							(if biff "BIFF")))
	  (goto-char (point-max))
	  (setq pos (point))
	  (insert (apply 'format message args) "\n")))))

;;; ELMO POP3 folder
(eval-and-compile
  (luna-define-class elmo-pop3-folder (elmo-net-folder elmo-location-map)
		     (use-uidl))
  (luna-define-internal-accessors 'elmo-pop3-folder))

(defsubst elmo-pop3-folder-use-uidl (folder)
  (if elmo-inhibit-number-mapping
      nil
    (elmo-pop3-folder-use-uidl-internal folder)))

(luna-define-method elmo-folder-initialize ((folder elmo-pop3-folder) name)
  (let ((elmo-network-stream-type-alist
	 (if elmo-pop3-stream-type-alist
	     (append elmo-pop3-stream-type-alist
		     elmo-network-stream-type-alist)
	   elmo-network-stream-type-alist))
	tokens auth uidl)
    (setq tokens (car (elmo-parse-separated-tokens
		       name
		       elmo-pop3-folder-name-syntax)))
    ;; user
    (elmo-net-folder-set-user-internal folder
				       (or (cdr (assq 'user tokens))
					   elmo-pop3-default-user))
    ;; auth
    (setq auth (cdr (assq 'auth tokens)))
    (elmo-net-folder-set-auth-internal folder
				       (if auth
					   (intern (downcase auth))
					 elmo-pop3-default-authenticate-type))
    ;; uidl
    (setq uidl (cdr (assq 'uidl tokens)))
    (elmo-pop3-folder-set-use-uidl-internal folder
					    (if uidl
						(string= uidl "uidl")
					      elmo-pop3-default-use-uidl))
    ;; network
    (elmo-net-folder-set-parameters
     folder
     tokens
     (list :server	elmo-pop3-default-server
	   :port	elmo-pop3-default-port
	   :stream-type
	   (elmo-get-network-stream-type elmo-pop3-default-stream-type)))
    folder))

;;; POP3 session
(luna-define-class elmo-pop3-session (elmo-network-session) ())

;; buffer-local
(defvar elmo-pop3-read-point nil)
(defvar elmo-pop3-number-uidl-hash nil) ; number -> uidl
(defvar elmo-pop3-uidl-number-hash nil) ; uidl -> number
(defvar elmo-pop3-size-hash nil) ; number -> size
(defvar elmo-pop3-uidl-done nil)
(defvar elmo-pop3-list-done nil)
(defvar elmo-pop3-lock nil)

(defvar elmo-pop3-local-variables '(elmo-pop3-read-point
				    elmo-pop3-uidl-number-hash
				    elmo-pop3-number-uidl-hash
				    elmo-pop3-uidl-done
				    elmo-pop3-size-hash
				    elmo-pop3-list-done
				    elmo-pop3-lock))

(luna-define-method elmo-network-close-session ((session elmo-pop3-session))
  (when (elmo-network-session-process-internal session)
    (when (memq (process-status
		 (elmo-network-session-process-internal session))
		'(open run))
      (elmo-pop3-send-command (elmo-network-session-process-internal session)
			      "quit")
      ;; process is dead.
      (or (cdr (elmo-pop3-read-response
		(elmo-network-session-process-internal session)
		t))
	  (error "POP error: QUIT failed")))
    (kill-buffer (process-buffer
		  (elmo-network-session-process-internal session)))
    (delete-process (elmo-network-session-process-internal session))))

(defun elmo-pop3-get-session (folder &optional if-exists)
  "Get POP3 session for FOLDER.
If IF-EXISTS is non-nil, don't get new session.
If IF-EXISTS is `any-exists', get BIFF session or normal session if exists."
  (let ((elmo-pop3-use-uidl-internal (elmo-pop3-folder-use-uidl folder)))
    (prog1
	(if (eq if-exists 'any-exists)
	    (or (elmo-network-get-session 'elmo-pop3-session
					  "POP3"
					  folder if-exists)
		(elmo-network-get-session 'elmo-pop3-session
					  "BIFF-POP3"
					  folder if-exists))
	  (elmo-network-get-session 'elmo-pop3-session
				    (concat
				     (if (elmo-folder-biff-internal folder)
					 "BIFF-")
				     "POP3")
				    folder if-exists))
      ;; For saving existency.
      (unless (file-exists-p (elmo-folder-msgdb-path folder))
	(elmo-make-directory (elmo-folder-msgdb-path folder))))))

(defun elmo-pop3-send-command (process command &optional no-erase no-log)
  (with-current-buffer (process-buffer process)
    (unless no-erase
      (erase-buffer))
    (goto-char (point-min))
    (setq elmo-pop3-read-point (point))
    (elmo-pop3-debug "SEND: %s\n" (if no-log "<NO LOGGING>" command))
    (process-send-string process (concat command "\r\n"))))

(defun elmo-pop3-read-response (process &optional not-command)
  "Read response and return a cons cell of \(CODE . BODY\).
PROCESS is the process to read response from.
If optional NOT-COMMAND is non-nil, read only the first line.
CODE is one of the following:
'ok          ... response is OK.
'err         ... response is ERROR.
'login-delay ... user is not allowed to login until the login delay
                 period has expired.
'in-use      ... authentication was successful but the mailbox is in use."
  ;; buffer is in case for process is dead.
  (with-current-buffer (process-buffer process)
    (let (case-fold-search
	  (response-continue t)
	  err match-end
	  (start elmo-pop3-read-point))
      (while response-continue
	(setq match-end elmo-pop3-read-point)
	(while (null (progn (goto-char match-end)
			    (search-forward "\r\n" nil t)))
	  (setq match-end (max (1- (point-max)) elmo-pop3-read-point))
	  (accept-process-output process 1))
	(setq match-end (point))
	(goto-char elmo-pop3-read-point)
	(setq elmo-pop3-read-point match-end)
	(cond
	 ((looking-at "\\+")
	  (setq response-continue nil))
	 ((looking-at "\\-")
	  (setq err (or (when (looking-at "[^ ]+ \\[\\([^]]+\\)\\]")
			  (intern (downcase
				   (buffer-substring (match-beginning 1)
						     (match-end 1)))))
			'err)
		response-continue nil
		start nil))
	 (not-command
	  (setq response-continue nil))))
      (cons (or err 'ok)
	    (when start
	      (elmo-delete-cr (buffer-substring start (- match-end 2))))))))

(defun elmo-pop3-process-filter (process output)
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert output)
      (elmo-pop3-debug "RECEIVED: %s\n" output)
      (when elmo-pop3-retrieve-progress-reporter
	(elmo-progress-notify 'elmo-retrieve-message :set (buffer-size))))))

(defun elmo-pop3-auth-user (session)
  (let ((process (elmo-network-session-process-internal session))
	response)
    ;; try USER/PASS
    (elmo-pop3-send-command
     process
     (format "user %s" (elmo-network-session-user-internal session))
     nil 'no-log)
    (setq response (elmo-pop3-read-response process t))
    (unless (eq (car response) 'ok)
      (signal 'elmo-open-error '(elmo-pop-auth-user)))
    (elmo-pop3-send-command  process
			     (format
			      "pass %s"
			      (elmo-get-passwd
			       (elmo-network-session-password-key session)))
			     nil 'no-log)
    (setq response (elmo-pop3-read-response process t))
    (case (car response)
      (ok)
      (in-use
       (error "Maildrop is currently in use"))
      (login-delay
       (error "Not allowed to login until the login delay period has expired"))
      (t
       (signal 'elmo-authenticate-error '(elmo-pop-auth-user))))
    (car response)))

(defun elmo-pop3-auth-apop (session)
  (unless (string-match "^\\+OK .*\\(<[=!-;?-~]+@[=!-;?-~]+>\\)"
			(elmo-network-session-greeting-internal session))
    (signal 'elmo-open-error '(elmo-pop3-auth-apop)))
  ;; good, APOP ready server
  (elmo-pop3-send-command
   (elmo-network-session-process-internal session)
   (format "apop %s %s"
	   (elmo-network-session-user-internal session)
	   (md5
	    (concat (match-string
		     1
		     (elmo-network-session-greeting-internal session))
		    (elmo-get-passwd
		     (elmo-network-session-password-key session)))))
   nil 'no-log)
  (let ((response (elmo-pop3-read-response
		   (elmo-network-session-process-internal session)
		   t)))
    (case (car response)
      (ok)
      (in-use
       (error "Maildrop is currently in use"))
      (login-delay
       (error "Not allowed to login until the login delay period has expired"))
      (t
       (signal 'elmo-authenticate-error '(elmo-pop-auth-apop))))
    (car response)))

(luna-define-method elmo-network-initialize-session-buffer :after
  ((session elmo-pop3-session) buffer)
  (with-current-buffer buffer
    (mapcar 'make-variable-buffer-local elmo-pop3-local-variables)))

(luna-define-method elmo-network-initialize-session ((session
						      elmo-pop3-session))
  (let ((process (elmo-network-session-process-internal session))
	response mechanism)
    (with-current-buffer (process-buffer process)
      (set-process-filter process 'elmo-pop3-process-filter)
      (setq elmo-pop3-read-point (point-min))
      ;; Skip garbage output from process before greeting.
      (while (and (memq (process-status process) '(open run))
		  (goto-char (point-max))
		  (forward-line -1)
		  (not (looking-at "+OK")))
	(accept-process-output process 1))
      (setq elmo-pop3-read-point (point))
      (or (elmo-network-session-set-greeting-internal
	   session
	   (cdr (elmo-pop3-read-response process t))) ; if ok, cdr is non-nil.
	  (signal 'elmo-open-error
		  '(elmo-network-intialize-session)))
      (when (eq (elmo-network-stream-type-symbol
		 (elmo-network-session-stream-type-internal session))
		'starttls)
	(elmo-pop3-send-command process "stls")
	(if (eq 'ok (car (elmo-pop3-read-response process)))
	    (starttls-negotiate process)
	  (signal 'elmo-open-error '(elmo-pop3-starttls-error)))))))

(luna-define-method elmo-network-authenticate-session ((session
							elmo-pop3-session))
  (with-current-buffer (process-buffer
			(elmo-network-session-process-internal session))
    (let* ((process (elmo-network-session-process-internal session))
	   (auth (elmo-network-session-auth-internal session))
	   (auth (mapcar (lambda (mechanism) (upcase (symbol-name mechanism)))
			 (if (listp auth) auth (list auth)))))
      (or (and (string= "USER" (car auth))
	       (elmo-pop3-auth-user session))
	  (and (string= "APOP" (car auth))
	       (elmo-pop3-auth-apop session))
	  (let (sasl-mechanisms
		client name step response mechanism
		sasl-read-passphrase)
	    (require 'sasl)
	    (setq sasl-mechanisms (mapcar 'car sasl-mechanism-alist))
	    (setq mechanism (sasl-find-mechanism auth))
	    (unless mechanism
	      (signal 'elmo-authenticate-error '(elmo-pop3-auth-no-mechanisms)))
	    (setq client
		  (sasl-make-client
		   mechanism
		   (elmo-network-session-user-internal session)
		   "pop"
		   (elmo-network-session-server-internal session)))
;;;	    (if elmo-pop3-auth-user-realm
;;;		(sasl-client-set-property client 'realm elmo-pop3-auth-user-realm))
	    (setq name (sasl-mechanism-name mechanism))
	    (elmo-network-session-set-auth-internal session
						    (intern (downcase name)))
	    (setq sasl-read-passphrase
		  (lambda (prompt)
		    (elmo-get-passwd
		     (elmo-network-session-password-key session))))
	    (setq step (sasl-next-step client nil))
	    (elmo-pop3-send-command
	     process
	     (concat "AUTH " name
		     (and (sasl-step-data step)
			  (concat
			   " "
			   (elmo-base64-encode-string
			    (sasl-step-data step) 'no-line-break))))
	     nil 'no-log)
	    (catch 'done
	      (while t
		(setq response (elmo-pop3-read-response process t))
		(case (car response)
		  (ok)
		  (in-use
		   (error "Maildrop is currently in use"))
		  (login-delay
		   (error "Not allowed to login \
until the login delay period has expired"))
		  (t
		   (signal 'elmo-authenticate-error
			   (list (intern (concat "elmo-pop3-auth-"
						 (downcase name)))))))
		(if (sasl-next-step client step)
		    ;; Bogus server?
		    (signal 'elmo-authenticate-error
			    (list (intern
				   (concat "elmo-pop3-auth-"
					   (downcase name)))))
		  ;; The authentication process is finished.
		  (throw 'done nil))
		(sasl-step-set-data
		 step
		 (elmo-base64-decode-string
		  (cadr (split-string response " "))))
		(setq step (sasl-next-step client step))
		(elmo-pop3-send-command
		 process
		 (if (sasl-step-data step)
		     (elmo-base64-encode-string (sasl-step-data step)
						'no-line-break)
		   "") nil 'no-log))))))))

(luna-define-method elmo-network-setup-session ((session
						 elmo-pop3-session))
  (let ((process (elmo-network-session-process-internal session))
	count response)
    (with-current-buffer (process-buffer process)
      (setq elmo-pop3-size-hash (elmo-make-hash 31))
      ;; To get obarray of uidl and size
      (elmo-pop3-send-command process "list")
      (if (null (cdr (elmo-pop3-read-response process)))
	  (error "POP LIST command failed"))
      (if (null (setq response
		      (elmo-pop3-read-contents process)))
	  (error "POP LIST command failed"))
      ;; POP server always returns a sequence of serial numbers.
      (setq count (elmo-pop3-parse-list-response response))
      ;; UIDL
      (when elmo-pop3-use-uidl-internal
	(setq elmo-pop3-uidl-number-hash (elmo-make-hash (* count 2)))
	(setq elmo-pop3-number-uidl-hash (elmo-make-hash (* count 2)))
	;; UIDL
	(elmo-pop3-send-command process "uidl")
	(unless (cdr (elmo-pop3-read-response process))
	  (error "POP UIDL failed"))
	(unless (setq response (elmo-pop3-read-contents process))
	  (error "POP UIDL failed"))
	(elmo-pop3-parse-uidl-response response)))))

(defun elmo-pop3-read-contents (process)
  (with-current-buffer (process-buffer process)
    (let ((point elmo-pop3-read-point))
      (while (progn (goto-char (1- point))
		    (null (search-forward "\n.\r\n" nil t)))
	(setq point (max (- (point-max) 2) ; Care of \r\n.\r[EOF] case
			 elmo-pop3-read-point))
	(accept-process-output process 1))
      (elmo-delete-cr
       (buffer-substring elmo-pop3-read-point
			 (- (point) 3))))))

(luna-define-method elmo-folder-expand-msgdb-path ((folder elmo-pop3-folder))
  (convert-standard-filename
   (expand-file-name
    (elmo-safe-filename (elmo-net-folder-user-internal folder))
    (expand-file-name (elmo-net-folder-server-internal folder)
		      (expand-file-name
		       "pop"
		       elmo-msgdb-directory)))))

(luna-define-method elmo-folder-exists-p ((folder elmo-pop3-folder))
  (if (and elmo-pop3-exists-exactly
	   (elmo-folder-plugged-p folder))
      (save-excursion
	(let (elmo-auto-change-plugged  ; don't change plug status.
	      (elmo-inhibit-number-mapping t) ; No need to use uidl.
	      session)
	  (prog1
	      (setq session (elmo-pop3-get-session folder))
	    (if session
		(elmo-network-close-session session)))))
    (or (file-directory-p (elmo-folder-msgdb-path folder))
	;; First time.
	(when (elmo-folder-plugged-p folder)
	  (let ((elmo-pop3-exists-exactly t))
	    (elmo-folder-exists-p folder))))))

(defun elmo-pop3-parse-uidl-response (string)
  (let ((buffer (current-buffer))
	number list size)
    (with-temp-buffer
      (let (number uid list)
	(insert string)
	(goto-char (point-min))
	(while (re-search-forward "^\\([0-9]+\\)[\t ]+\\([^ \n]+\\)$" nil t)
	  (setq number  (elmo-match-buffer 1))
	  (setq uid (elmo-match-buffer 2))
	  (with-current-buffer buffer
	    (elmo-set-hash-val uid number elmo-pop3-uidl-number-hash)
	    (elmo-set-hash-val (concat "#" number) uid
			       elmo-pop3-number-uidl-hash))
	  (setq list (cons uid list)))
	(with-current-buffer buffer (setq elmo-pop3-uidl-done t))
	(nreverse list)))))

(defun elmo-pop3-parse-list-response (string)
  (let ((buffer (current-buffer))
	(count 0)
	alist)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward "^\\([0-9]+\\)[\t ]+\\([0-9]+\\)$" nil t)
	(setq alist
	      (cons
	       (cons (elmo-match-buffer 1)
		     (elmo-match-buffer 2))
	       alist))
	(setq count (1+ count)))
      (with-current-buffer buffer
	(setq elmo-pop3-size-hash (elmo-make-hash (* (length alist) 2)))
	(while alist
	  (elmo-set-hash-val (concat "#" (car (car alist)))
			     (cdr (car alist))
			     elmo-pop3-size-hash)
	  (setq alist (cdr alist)))
	(setq elmo-pop3-list-done t))
      count)))

(defun elmo-pop3-list-location (folder)
  (with-current-buffer (process-buffer
			(elmo-network-session-process-internal
			 (elmo-pop3-get-session folder)))
    (let (locations)
      (if elmo-pop3-uidl-done
	  (progn
	    (mapatoms
	     (lambda (atom)
	       (setq locations (cons (symbol-name atom) locations)))
	     elmo-pop3-uidl-number-hash)
	    (sort locations
		  (lambda (loc1 loc2)
		    (< (elmo-pop3-uidl-to-number loc1)
		       (elmo-pop3-uidl-to-number loc2)))))
	(error "POP3: Error in UIDL")))))

(defun elmo-pop3-list-folder-by-location (folder locations)
  (mapcar #'car (elmo-location-map-update folder locations)))

(defun elmo-pop3-list-by-uidl-subr (folder &optional nonsort)
  (let ((flist (elmo-pop3-list-folder-by-location
		folder
		(elmo-pop3-list-location folder))))
    (if nonsort
	(cons (elmo-max-of-list flist) (length flist))
      (sort flist '<))))

(defun elmo-pop3-list-by-list (folder)
  (with-current-buffer (process-buffer
			(elmo-network-session-process-internal
			 (elmo-pop3-get-session folder)))
    (let (list)
      (if elmo-pop3-list-done
	  (progn
	    (mapatoms (lambda (atom)
			(setq list (cons (string-to-number
					  (substring (symbol-name atom) 1))
					 list)))
		      elmo-pop3-size-hash)
	    (sort list '<))
	(error "POP3: Error in list")))))

(defsubst elmo-pop3-folder-list-messages (folder)
  (if (elmo-pop3-folder-use-uidl folder)
      (elmo-pop3-list-by-uidl-subr folder)
    (elmo-pop3-list-by-list folder)))

(luna-define-method elmo-folder-list-messages-plugged
  ((folder elmo-pop3-folder) &optional nohide)
  (elmo-pop3-folder-list-messages folder))

(luna-define-method elmo-folder-status ((folder elmo-pop3-folder))
  (elmo-folder-open-internal folder)
  (elmo-folder-check folder)
  (if (elmo-pop3-folder-use-uidl folder)
      (prog1
	  (elmo-pop3-list-by-uidl-subr folder 'nonsort)
	(elmo-folder-close-internal folder))
    (let ((process
	   (elmo-network-session-process-internal
	    (elmo-pop3-get-session folder)))
	  (total 0)
	  response)
      (with-current-buffer (process-buffer process)
	(elmo-pop3-send-command process "STAT")
	(setq response (cdr (elmo-pop3-read-response process)))
	;; response: "^\\+OK 2 7570$"
	(if (not (string-match "^\\+OK[ \t]*\\([0-9]*\\)" response))
	    (error "POP STAT command failed")
	  (setq total
		(string-to-number
		 (substring response (match-beginning 1)(match-end 1 ))))
	  (elmo-folder-close-internal folder)
	  (cons total total))))))

(defvar elmo-pop3-header-fetch-chop-length 200)

(defsubst elmo-pop3-next-result-arrived-p ()
  (cond
   ((eq (following-char) ?+)
    (if (re-search-forward "\n\\.\r?\n" nil t)
	t
      nil))
   ((looking-at "-")
    (if (search-forward "\n" nil t)
	t
      nil))
   (t
    nil)))

(defun elmo-pop3-retrieve-headers (process tobuffer articles)
  (with-current-buffer (process-buffer process)
    (erase-buffer)
    (let ((count 0)
	  (received 0)
	  (last-point (point-min)))
      (elmo-with-progress-display (elmo-retrieve-header (length articles))
	  "Getting headers"
	;; Send HEAD commands.
	(while articles
	  (elmo-pop3-send-command process
				  (format "top %s 0" (car articles))
				  'no-erase)
;;;	  (accept-process-output process 1)
	  (setq articles (cdr articles))
	  (setq count (1+ count))
	  ;; Every 200 requests we have to read the stream in
	  ;; order to avoid deadlocks.
	  (when (or elmo-pop3-send-command-synchronously
		    (null articles)	;All requests have been sent.
		    (zerop (% count elmo-pop3-header-fetch-chop-length)))
	    (unless elmo-pop3-send-command-synchronously
	      (accept-process-output process 1))
	    (discard-input)
	    (while (progn
		     (goto-char last-point)
		     ;; Count replies.
		     (while (elmo-pop3-next-result-arrived-p)
		       (setq last-point (point))
		       (setq received (1+ received)))
		     (< received count))
	      (elmo-progress-notify 'elmo-retrieve-header :set received)
	      (accept-process-output process 1)
;;;	      (accept-process-output process)
	      (discard-input)))))
      ;; Replace all CRLF with LF.
      (elmo-delete-cr-buffer)
      (copy-to-buffer tobuffer (point-min) (point-max)))))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-pop3-folder)
					      numlist flag-table)
  (let ((process (elmo-network-session-process-internal
		  (elmo-pop3-get-session folder))))
    (with-current-buffer (process-buffer process)
      (elmo-pop3-msgdb-create-by-header
       folder
       process
       (sort numlist #'<)
       flag-table))))

(defun elmo-pop3-uidl-to-number (uidl)
  (string-to-number (elmo-get-hash-val uidl
				       elmo-pop3-uidl-number-hash)))

(defun elmo-pop3-number-to-uidl (number)
  (elmo-get-hash-val (format "#%d" number)
		     elmo-pop3-number-uidl-hash))

(defun elmo-pop3-number-to-size (number)
  (string-to-number
   (elmo-get-hash-val (format "#%d" number) elmo-pop3-size-hash)))

(defun elmo-pop3-msgdb-create-by-header (folder process numlist
						flag-table)
  (let ((tmp-buffer (get-buffer-create " *ELMO Overview TMP*")))
    (unwind-protect
	(with-current-buffer (process-buffer process)
	  (when (elmo-pop3-folder-use-uidl folder)
	    (setq numlist
		  (delq
		   nil
		   (mapcar
		    (lambda (number)
		      (elmo-pop3-uidl-to-number
		       (elmo-map-message-location folder number)))
		    numlist))))
	  (elmo-pop3-retrieve-headers process tmp-buffer numlist)
	  (elmo-pop3-msgdb-create-message
	   folder
	   tmp-buffer
	   process
	   (length numlist)
	   numlist
	   flag-table))
      (kill-buffer tmp-buffer))))

(defun elmo-pop3-msgdb-create-message (folder
				       buffer
				       process
				       num
				       numlist
				       flag-table)
  (save-excursion
    (let ((new-msgdb (elmo-make-msgdb))
	  beg entity number message-id flags)
      (set-buffer buffer)
      (set-buffer-multibyte default-enable-multibyte-characters)
      (goto-char (point-min))
      (elmo-with-progress-display (elmo-folder-msgdb-create num)
	  "Creating msgdb"
	(while (not (eobp))
	  (setq beg (save-excursion (forward-line) (point)))
	  (elmo-pop3-next-result-arrived-p)
	  (save-excursion
	    (forward-line -1)
	    (save-restriction
	      (narrow-to-region beg (point))
	      (setq entity
		    (elmo-msgdb-create-message-entity-from-header
		     (elmo-msgdb-message-entity-handler new-msgdb)
		     (car numlist)))
	      (setq numlist (cdr numlist))
	      (when entity
		(with-current-buffer (process-buffer process)
		  (elmo-message-entity-set-field
		   entity
		   'size
		   (elmo-pop3-number-to-size
		    (elmo-message-entity-number entity)))
		  (when (setq number
			      (elmo-map-message-number
			       folder
			       (elmo-pop3-number-to-uidl
				(elmo-message-entity-number entity))))
		    (elmo-message-entity-set-number entity number)))
		(setq message-id (elmo-message-entity-field entity 'message-id)
		      flags (elmo-flag-table-get flag-table message-id))
		(elmo-global-flags-set flags folder number message-id)
		(elmo-msgdb-append-entity new-msgdb entity flags))))
	  (elmo-progress-notify 'elmo-folder-msgdb-create)))
      new-msgdb)))

(defun elmo-pop3-read-body (process outbuf)
  (with-current-buffer (process-buffer process)
    (let ((point elmo-pop3-read-point)
	  (read-point elmo-pop3-read-point))
      (while (and (goto-char (1- point))
		  (null (search-forward "\n.\r\n" nil t)))
	(setq point (max (- (point-max) 2) elmo-pop3-read-point))
	(accept-process-output process 1))
      (setq point (point))
      (set-buffer outbuf)
      (erase-buffer)
      (insert-buffer-substring
       (process-buffer process) read-point (- point 3))))
  t)

(luna-define-method elmo-folder-open-internal ((folder elmo-pop3-folder))
  (unless (elmo-location-map-alist folder)
    (when (elmo-pop3-folder-use-uidl folder)
      (elmo-location-map-load folder (elmo-folder-msgdb-path folder)))))

(luna-define-method elmo-folder-commit :after ((folder elmo-pop3-folder))
  (when (and (not elmo-inhibit-number-mapping)
	     (elmo-folder-persistent-p folder))
    (elmo-location-map-save folder (elmo-folder-msgdb-path folder))))

(luna-define-method elmo-folder-close-internal ((folder elmo-pop3-folder))
  (elmo-location-map-teardown folder)
  ;; Just close connection
  (elmo-folder-check folder))

(luna-define-method elmo-message-fetch-plugged ((folder elmo-pop3-folder)
						number strategy
						&optional section
						outbuf unseen)
  (let ((process (elmo-network-session-process-internal
		  (elmo-pop3-get-session folder)))
	size  response errmsg msg)
    (with-current-buffer (process-buffer process)
      (when (elmo-pop3-folder-use-uidl folder)
	(setq number (elmo-pop3-uidl-to-number
		      (elmo-map-message-location folder number))))
      (setq size (elmo-pop3-number-to-size number))
      (when number
	(elmo-with-progress-display
	    (elmo-retrieve-message size elmo-pop3-retrieve-progress-reporter)
	    "Retrieving"
	  (elmo-pop3-send-command process (format "retr %s" number))
	  (when (null (setq response (cdr (elmo-pop3-read-response
					   process t))))
	    (error "Fetching message failed"))
	  (setq response  (elmo-pop3-read-body process outbuf)))
	(set-buffer outbuf)
	(goto-char (point-min))
	(while (re-search-forward "^\\." nil t)
	  (replace-match "")
	  (forward-line))
	(elmo-delete-cr-buffer)
	response))))

(defun elmo-pop3-delete-msg (process number)
  (unless number
    (error "Deleting message failed"))
  (elmo-pop3-send-command process (format "dele %s" number))
  (when (null (cdr (elmo-pop3-read-response process t)))
    (error "Deleting message failed")))

(luna-define-method elmo-folder-delete-messages-plugged ((folder
							  elmo-pop3-folder)
							 msgs)
  (let ((process (elmo-network-session-process-internal
		  (elmo-pop3-get-session folder))))
    (with-current-buffer (process-buffer process)
      (dolist (number (if (elmo-pop3-folder-use-uidl folder)
			  (mapcar
			   (lambda (number)
			     (elmo-pop3-uidl-to-number
			      (elmo-map-message-location folder number)))
			   msgs)
			msgs))
	(elmo-pop3-delete-msg process number))
      t)))

(luna-define-method elmo-message-use-cache-p ((folder elmo-pop3-folder) number)
  elmo-pop3-use-cache)

(luna-define-method elmo-folder-persistent-p ((folder elmo-pop3-folder))
  (and (elmo-folder-persistent-internal folder)
       (elmo-pop3-folder-use-uidl-internal folder)))

(luna-define-method elmo-folder-clear :around ((folder elmo-pop3-folder)
					       &optional keep-killed)
  (unless keep-killed
    (elmo-location-map-setup folder))
  (luna-call-next-method))

(luna-define-method elmo-folder-check ((folder elmo-pop3-folder))
  (if (elmo-folder-plugged-p folder)
      (let ((session (elmo-pop3-get-session folder 'if-exists)))
	(when session
	  (elmo-network-close-session session)))))

(require 'product)
(product-provide (provide 'elmo-pop3) (require 'elmo-version))

;;; elmo-pop3.el ends here
