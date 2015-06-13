;;; elmo-nntp.el --- NNTP Interface for ELMO.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 1998,1999,2000 Masahiro MURATA <muse@ba2.so-net.ne.jp>
;; Copyright (C) 1999,2000      Kenichi OKADA <okada@opaopa.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Masahiro MURATA <muse@ba2.so-net.ne.jp>
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
(eval-when-compile (require 'cl))

(require 'elmo-vars)
(require 'elmo-util)
(require 'elmo-date)
(require 'elmo-msgdb)
(require 'elmo-cache)
(require 'elmo)
(require 'elmo-net)

(defvar elmo-nntp-overview-fetch-chop-length 200
 "*Number of overviews to fetch in one request in nntp.")

(defvar elmo-nntp-use-cache t
  "Use cache in nntp folder.")

(defvar elmo-nntp-max-number-precedes-list-active nil
  "Non-nil means max number of msgdb is set as the max number of `list active'.
\(Needed for inn 2.3 or later?\).")

(defvar elmo-nntp-group-coding-system nil
  "A coding system for newsgroup string.")

(defconst elmo-nntp-folder-name-syntax `(group
					 (?: [user "^\\([A-Za-z]\\|$\\)"])
					 ,@elmo-net-folder-name-syntax))

(defsubst elmo-nntp-encode-group-string (string)
  (if elmo-nntp-group-coding-system
      (encode-coding-string string elmo-nntp-group-coding-system)
    string))

(defsubst elmo-nntp-decode-group-string (string)
  (if elmo-nntp-group-coding-system
      (decode-coding-string string elmo-nntp-group-coding-system)
    string))

;; For debugging.
(defvar elmo-nntp-debug nil
  "Non-nil forces NNTP folder as debug mode.
Debug information is inserted in the buffer \"*NNTP DEBUG*\"")

;;; Debug
(defsubst elmo-nntp-debug (message &rest args)
  (if elmo-nntp-debug
      (let ((biff (string-match "BIFF-" (buffer-name)))
	    pos)
	(with-current-buffer (get-buffer-create (concat "*NNTP DEBUG*"
							(if biff "BIFF")))
	  (goto-char (point-max))
	  (setq pos (point))
	  (insert (apply 'format message args) "\n")))))

;;; ELMO NNTP folder
(eval-and-compile
  (luna-define-class elmo-nntp-folder (elmo-net-folder)
		     (group temp-crosses reads))
  (luna-define-internal-accessors 'elmo-nntp-folder))

(luna-define-method elmo-folder-initialize ((folder elmo-nntp-folder) name)
  (let ((elmo-network-stream-type-alist
	 (if elmo-nntp-stream-type-alist
	     (append elmo-nntp-stream-type-alist
		     elmo-network-stream-type-alist)
	   elmo-network-stream-type-alist))
	tokens)
    (setq tokens (car (elmo-parse-separated-tokens
		       name
		       elmo-nntp-folder-name-syntax)))
    ;; group
    (elmo-nntp-folder-set-group-internal folder
					 (elmo-nntp-encode-group-string
					  (cdr (assq 'group tokens))))
    ;; user
    (elmo-net-folder-set-user-internal folder
				       (let ((user (cdr (assq 'user tokens))))
					 (if user
					     (and (> (length user) 0) user)
					   elmo-nntp-default-user)))
    ;; network
    (elmo-net-folder-set-parameters
     folder
     tokens
     (list :server	elmo-nntp-default-server
	   :port	elmo-nntp-default-port
	   :stream-type
	   (elmo-get-network-stream-type elmo-nntp-default-stream-type)))
    folder))

(luna-define-method elmo-folder-expand-msgdb-path ((folder elmo-nntp-folder))
  (convert-standard-filename
   (expand-file-name
    (elmo-nntp-folder-group-internal folder)
    (expand-file-name (or (elmo-net-folder-server-internal folder) "nowhere")
		      (expand-file-name "nntp"
					elmo-msgdb-directory)))))

(luna-define-method elmo-folder-newsgroups ((folder elmo-nntp-folder))
  (list (elmo-nntp-folder-group-internal folder)))

;;; NNTP Session
(eval-and-compile
  (luna-define-class elmo-nntp-session (elmo-network-session)
		     (current-group))
  (luna-define-internal-accessors 'elmo-nntp-session))

;;
;; internal variables
;;

(defvar elmo-nntp-connection-cache nil
  "Cache of NNTP connection.")
;; buffer local variable

(defvar elmo-nntp-list-folders-use-cache 600
  "*Time to cache of list folders, as the number of seconds.
Don't cache if nil.")

(defvar elmo-nntp-list-folders-cache nil)

(defvar elmo-nntp-groups-async nil)
(defvar elmo-nntp-header-fetch-chop-length 200)

(defvar elmo-nntp-read-point 0)

(defvar elmo-nntp-send-mode-reader t)

(defvar elmo-nntp-opened-hook nil)

(defvar elmo-nntp-get-folders-securely nil)

(defvar elmo-nntp-default-use-xover t)

(defvar elmo-nntp-default-use-listgroup t)

(defvar elmo-nntp-default-use-list-active t)

(defvar elmo-nntp-default-use-xhdr t)

(defvar elmo-nntp-server-command-alist nil)


(defconst elmo-nntp-server-command-index '((xover . 0)
					   (listgroup . 1)
					   (list-active . 2)
					   (xhdr . 3)))

(defmacro elmo-nntp-get-server-command (session)
  `(assoc (cons (elmo-network-session-server-internal ,session)
		(elmo-network-session-port-internal ,session))
	  elmo-nntp-server-command-alist))

(defmacro elmo-nntp-set-server-command (session com value)
  `(let (entry)
     (unless (setq entry (cdr (elmo-nntp-get-server-command
			       ,session)))
       (setq elmo-nntp-server-command-alist
	     (nconc elmo-nntp-server-command-alist
		    (list (cons
			   (cons
			    (elmo-network-session-server-internal ,session)
			    (elmo-network-session-port-internal ,session))
			   (setq entry
				 (vector
				  elmo-nntp-default-use-xover
				  elmo-nntp-default-use-listgroup
				  elmo-nntp-default-use-list-active
				  elmo-nntp-default-use-xhdr)))))))
     (aset entry
	   (cdr (assq ,com elmo-nntp-server-command-index))
	   ,value)))

(defmacro elmo-nntp-xover-p (session)
  `(let ((entry (elmo-nntp-get-server-command ,session)))
     (if entry
	 (aref (cdr entry)
	       (cdr (assq 'xover elmo-nntp-server-command-index)))
       elmo-nntp-default-use-xover)))

(defmacro elmo-nntp-set-xover (session value)
  `(elmo-nntp-set-server-command ,session 'xover ,value))

(defmacro elmo-nntp-listgroup-p (session)
  `(let ((entry (elmo-nntp-get-server-command ,session)))
     (if entry
	 (aref (cdr entry)
	       (cdr (assq 'listgroup elmo-nntp-server-command-index)))
       elmo-nntp-default-use-listgroup)))

(defmacro elmo-nntp-set-listgroup (session value)
  `(elmo-nntp-set-server-command ,session 'listgroup ,value))

(defmacro elmo-nntp-list-active-p (session)
  `(let ((entry (elmo-nntp-get-server-command ,session)))
     (if entry
	 (aref (cdr entry)
	       (cdr (assq 'list-active elmo-nntp-server-command-index)))
       elmo-nntp-default-use-list-active)))

(defmacro elmo-nntp-set-list-active (session value)
  `(elmo-nntp-set-server-command ,session 'list-active ,value))

(defmacro elmo-nntp-xhdr-p (session)
  `(let ((entry (elmo-nntp-get-server-command ,session)))
     (if entry
	 (aref (cdr entry)
	       (cdr (assq 'xhdr elmo-nntp-server-command-index)))
       elmo-nntp-default-use-xhdr)))

(defmacro elmo-nntp-set-xhdr (session value)
  `(elmo-nntp-set-server-command ,session 'xhdr ,value))

(defsubst elmo-nntp-max-number-precedes-list-active-p ()
  elmo-nntp-max-number-precedes-list-active)

(defsubst elmo-nntp-folder-postfix (user server port type)
  (concat
   (and user (concat ":" user))
   (if (and server
	    (null (string= server elmo-nntp-default-server)))
       (concat "@" server))
   (if (and port
	    (null (eq port elmo-nntp-default-port)))
       (concat ":" (if (numberp port)
		       (number-to-string port) port)))
   (unless (eq (elmo-network-stream-type-symbol type)
	       elmo-nntp-default-stream-type)
     (elmo-network-stream-type-spec-string type))))

(defun elmo-nntp-get-session (folder &optional if-exists)
  (elmo-network-get-session
   'elmo-nntp-session
   (concat
    (if (elmo-folder-biff-internal folder)
	"BIFF-")
    "NNTP")
   folder
   if-exists))

(luna-define-method elmo-network-initialize-session ((session
						      elmo-nntp-session))
  (let ((process (elmo-network-session-process-internal session))
	response)
    (set-process-filter (elmo-network-session-process-internal session)
			'elmo-nntp-process-filter)
    (with-current-buffer (elmo-network-session-buffer session)
      (setq elmo-nntp-read-point (point-min))
      ;; Skip garbage output from process before greeting.
      (while (and (memq (process-status process) '(open run))
		  (goto-char (point-max))
		  (forward-line -1)
		  (not (looking-at "^[2-5][0-9][0-9]")))
	(accept-process-output process 1))
      (setq elmo-nntp-read-point (point))
      (setq response (elmo-nntp-read-response session t t))
      (unless (car response)
	  (signal 'elmo-open-error (list (cdr response))))
      (if elmo-nntp-send-mode-reader
	  (elmo-nntp-send-mode-reader session))
      (when (eq (elmo-network-stream-type-symbol
		 (elmo-network-session-stream-type-internal session))
		'starttls)
	(elmo-nntp-send-command session "starttls")
	(or (elmo-nntp-read-response session)
	    (error "Cannot open starttls session"))
	(starttls-negotiate process)))))

(luna-define-method elmo-network-authenticate-session ((session
							elmo-nntp-session))
  (with-current-buffer (elmo-network-session-buffer session)
    (when (elmo-network-session-user-internal session)
      (elmo-nntp-send-command session
			      (format "authinfo user %s"
				      (elmo-network-session-user-internal
				       session))
			      nil
			      'no-log)
      (or (elmo-nntp-read-response session)
	  (signal 'elmo-authenticate-error '(authinfo)))
      (elmo-nntp-send-command
       session
       (format "authinfo pass %s"
	       (elmo-get-passwd (elmo-network-session-password-key session)))
       nil
       'no-log)
      (or (elmo-nntp-read-response session)
	  (signal 'elmo-authenticate-error '(authinfo))))))

(luna-define-method elmo-network-setup-session ((session
						 elmo-nntp-session))
  (run-hooks 'elmo-nntp-opened-hook))

(defun elmo-nntp-process-filter (process output)
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert output)
      (elmo-nntp-debug "RECEIVED: %s\n" output))))

(defun elmo-nntp-send-mode-reader (session)
  (elmo-nntp-send-command session "mode reader")
  (if (null (elmo-nntp-read-response session t))
      (message "Mode reader failed")))

(defun elmo-nntp-send-command (session command &optional noerase no-log)
  (with-current-buffer (elmo-network-session-buffer session)
    (unless noerase
      (erase-buffer)
      (goto-char (point-min)))
    (setq elmo-nntp-read-point (point))
    (elmo-nntp-debug "SEND: %s\n" (if no-log "<NO LOGGING>" command))
    (process-send-string (elmo-network-session-process-internal
			  session) command)
    (process-send-string (elmo-network-session-process-internal
			  session) "\r\n")))

(defun elmo-nntp-read-response (session &optional not-command error-msg)
  (with-current-buffer (elmo-network-session-buffer session)
    (let ((process (elmo-network-session-process-internal session))
	  case-fold-search
	  (response-continue t)
	  response match-end last
	  (start elmo-nntp-read-point))
      (while response-continue
	(setq match-end elmo-nntp-read-point)
	(while (null (progn (goto-char match-end)
			    (search-forward "\r\n" nil t)))
	  (setq match-end (max (1- (point-max)) elmo-nntp-read-point))
	  (accept-process-output process))
	(setq match-end (point)
	      last elmo-nntp-read-point
	      elmo-nntp-read-point match-end)
	(goto-char last)
	(cond
	 ((looking-at "[23][0-9]+ ")
	  (setq response-continue nil))
	 ((looking-at "[^23][0-9]+ ")
	  (setq response-continue nil
		start nil))
	 (not-command
	  (setq response-continue nil))))
      (setq response
	    (and start
		 (elmo-delete-cr (buffer-substring start (- match-end 2)))))
      (if error-msg
	  (cons response (buffer-substring last (- match-end 2)))
	response))))

(defun elmo-nntp-read-raw-response (session)
  (with-current-buffer (elmo-network-session-buffer session)
    (goto-char elmo-nntp-read-point)
    (while (not (search-forward "\r\n" nil t))
      (accept-process-output (elmo-network-session-process-internal
			      session))
      (goto-char elmo-nntp-read-point))
    (buffer-substring elmo-nntp-read-point (- (point) 2))))

(defun elmo-nntp-read-contents (session)
  (with-current-buffer (elmo-network-session-buffer session)
    (let ((point elmo-nntp-read-point))
      (while (null (progn (goto-char (1- point))
			  (search-forward "\n.\r\n" nil t)))
  	(setq point (max (- (point-max) 2) elmo-nntp-read-point))
	(accept-process-output (elmo-network-session-process-internal
				session)))
      (elmo-delete-cr (buffer-substring
		       elmo-nntp-read-point (- (point) 3))))))

(defun elmo-nntp-read-body (session outbuf)
  (with-current-buffer (elmo-network-session-buffer session)
    (let ((point elmo-nntp-read-point))
      (while (null (progn (goto-char (1- point))
			  (search-forward "\n.\r\n" nil t)))
	(setq point (max (- (point-max) 2) elmo-nntp-read-point))
	(accept-process-output
	 (elmo-network-session-process-internal session)))
      (setq point (point))
      (set-buffer outbuf)
      (erase-buffer)
      (insert-buffer-substring
       (elmo-network-session-buffer session) elmo-nntp-read-point (- point 3))
      (elmo-delete-cr-buffer)))
  t)

(defun elmo-nntp-select-group (session group &optional force)
  (let (response)
    (when (or force
	      (not (string= (elmo-nntp-session-current-group-internal session)
			    group)))
      (unwind-protect
	  (progn
	    (elmo-nntp-send-command session (format "group %s" group))
	    (setq response (elmo-nntp-read-response session)))
	(elmo-nntp-session-set-current-group-internal session
						      (and response group))
	response))))

(defun elmo-nntp-list-folders-get-cache (group server buf)
  (when (and elmo-nntp-list-folders-use-cache
	     elmo-nntp-list-folders-cache
	     (string-match (concat "^"
				   (regexp-quote
				    (or
				     (nth 1 elmo-nntp-list-folders-cache)
				     "")))
			   (or group ""))
	     (string-match (concat "^"
				   (regexp-quote
				    (or
				     (nth 2 elmo-nntp-list-folders-cache)
				     "")))
			   (or server "")))
    (let* ((cache-time (car elmo-nntp-list-folders-cache)))
      (unless (elmo-time-expire cache-time
				elmo-nntp-list-folders-use-cache)
	(with-current-buffer buf
	  (erase-buffer)
	  (insert (nth 3 elmo-nntp-list-folders-cache))
	  (goto-char (point-min))
	  (or (string= group "")
	      (and group
		   (keep-lines (concat "^" (regexp-quote group) "\\."))))
	  t
	  )))))

(defsubst elmo-nntp-catchup-msgdb (msgdb max-number)
  (let ((numbers (elmo-msgdb-list-messages msgdb))
	msgdb-max)
    (setq msgdb-max (if numbers (apply #'max numbers) 0))
    (when (and msgdb-max
	       max-number
	       (< msgdb-max max-number))
      (let ((i (1+ msgdb-max))
	    killed)
	(while (<= i max-number)
	  (setq killed (cons i killed))
	  (incf i))
	(nreverse killed)))))

(luna-define-method elmo-folder-list-subfolders ((folder elmo-nntp-folder)
						 &optional one-level)
  (elmo-nntp-folder-list-subfolders folder one-level))

(defun elmo-nntp-folder-list-subfolders (folder one-level)
  (let ((session (elmo-nntp-get-session folder))
	(case-fold-search nil)
	response ret-val top-ng username append-serv use-list-active start)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (if (and (elmo-nntp-folder-group-internal folder)
	       (elmo-nntp-select-group
		session
		(elmo-nntp-folder-group-internal folder)))
	  ;; add top newsgroups
	  (setq ret-val (list (elmo-nntp-folder-group-internal folder))))
      (unless (setq response (elmo-nntp-list-folders-get-cache
			      (elmo-nntp-folder-group-internal folder)
			      (elmo-net-folder-server-internal folder)
			      (current-buffer)))
	(when (setq use-list-active (elmo-nntp-list-active-p session))
	  (elmo-nntp-send-command
	   session
	   (concat "list"
		   (if (and (elmo-nntp-folder-group-internal folder)
			    (not (string= (elmo-nntp-folder-group-internal
					   folder) "")))
		       (concat " active"
			       (format
				" %s.*"
				(elmo-nntp-folder-group-internal folder))))))
	  (if (elmo-nntp-read-response session t)
	      (if (null (setq response (elmo-nntp-read-contents session)))
		  (error "NNTP List folders failed")
		(when elmo-nntp-list-folders-use-cache
		  (setq elmo-nntp-list-folders-cache
			(list (current-time)
			      (elmo-nntp-folder-group-internal folder)
			      (elmo-net-folder-server-internal folder)
			      response)))
		(erase-buffer)
		(insert response))
	    (elmo-nntp-set-list-active session nil)
	    (setq use-list-active nil)))
	(when (null use-list-active)
	  (elmo-nntp-send-command session "list")
	  (if (null (and (elmo-nntp-read-response session t)
			 (setq response (elmo-nntp-read-contents session))))
	      (error "NNTP List folders failed"))
	  (when elmo-nntp-list-folders-use-cache
	    (setq elmo-nntp-list-folders-cache
		  (list (current-time) nil nil response)))
	  (erase-buffer)
	  (setq start nil)
	  (while (string-match (concat "^"
				       (regexp-quote
					(or
					 (elmo-nntp-folder-group-internal
					  folder)
					 "")) ".*$")
			       response start)
	    (insert (match-string 0 response) "\n")
	    (setq start (match-end 0)))))
      (goto-char (point-min))
      (elmo-with-progress-display
	  (elmo-nntp-parse-active (count-lines (point-min) (point-max)))
	  "Parsing active"
	(if one-level
	    (let ((regexp
		   (format "^\\(%s[^. ]+\\)\\([. ]\\).*\n"
			   (if (and (elmo-nntp-folder-group-internal folder)
				    (null (string=
					   (elmo-nntp-folder-group-internal
					    folder) "")))
			       (concat (elmo-nntp-folder-group-internal
					folder)
				       "\\.")
			     ""))))
	      (while (looking-at regexp)
		(setq top-ng (elmo-match-buffer 1))
		(if (string= (elmo-match-buffer 2) " ")
		    (if (not (or (member top-ng ret-val)
				 (assoc top-ng ret-val)))
			(setq ret-val (nconc ret-val (list top-ng))))
		  (if (member top-ng ret-val)
		      (setq ret-val (delete top-ng ret-val)))
		  (if (not (assoc top-ng ret-val))
		      (setq ret-val (nconc ret-val (list (list top-ng))))))
		(elmo-progress-notify 'elmo-nntp-parse-active)
		(forward-line)))
	  (while (re-search-forward "\\([^ ]+\\) .*\n" nil t)
	    (setq ret-val (nconc ret-val
				 (list (elmo-match-buffer 1))))
	    (elmo-progress-notify 'elmo-nntp-parse-active)))))

    (setq username (or (elmo-net-folder-user-internal folder) ""))
    (unless (string= username (or elmo-nntp-default-user ""))
      (setq append-serv (concat append-serv
				":" (elmo-quote-syntactical-element
				     username
				     'user elmo-nntp-folder-name-syntax))))
    (unless (string= (elmo-net-folder-server-internal folder)
		     elmo-nntp-default-server)
      (setq append-serv (concat append-serv
				"@" (elmo-net-folder-server-internal folder))))
    (unless (eq (elmo-net-folder-port-internal folder) elmo-nntp-default-port)
      (setq append-serv (concat append-serv
				":" (number-to-string
				     (elmo-net-folder-port-internal folder)))))
    (unless (eq (elmo-network-stream-type-symbol
		 (elmo-net-folder-stream-type-internal folder))
		elmo-nntp-default-stream-type)
      (setq append-serv
	    (concat append-serv
		    (elmo-network-stream-type-spec-string
		     (elmo-net-folder-stream-type-internal folder)))))
    (mapcar (lambda (fld)
	      (if (consp fld)
		  (list (concat "-" (elmo-nntp-decode-group-string (car fld))
				append-serv))
		(concat "-" (elmo-nntp-decode-group-string fld) append-serv)))
	    ret-val)))

(defun elmo-nntp-make-msglist (beg-str end-str)
  (elmo-make-number-list (string-to-number beg-str)
			 (string-to-number end-str)))

(luna-define-method elmo-folder-list-messages-plugged ((folder
							elmo-nntp-folder)
						       &optional nohide)
  (let ((session (elmo-nntp-get-session folder))
	(group   (elmo-nntp-folder-group-internal folder))
	response numbers use-listgroup)
    (save-excursion
      (when (setq use-listgroup (elmo-nntp-listgroup-p session))
	(elmo-nntp-send-command session
				(format "listgroup %s" group))
	(if (not (elmo-nntp-read-response session t))
	    (progn
	      (elmo-nntp-set-listgroup session nil)
	      (setq use-listgroup nil))
	  (if (null (setq response (elmo-nntp-read-contents session)))
	      (error "Fetching listgroup failed"))
	  (setq numbers (elmo-string-to-list response))
	  (elmo-nntp-session-set-current-group-internal session
							group)))
      (unless use-listgroup
	(elmo-nntp-send-command session (format "group %s" group))
	(if (null (setq response (elmo-nntp-read-response session)))
	    (error "Select group failed"))
	(when (and
	       (string-match
		"211 \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) [^.].+$"
		response)
	       (> (string-to-number (match-string 1 response)) 0))
	  (setq numbers (elmo-nntp-make-msglist
			 (match-string 2 response)
			 (match-string 3 response)))))
      numbers)))

(luna-define-method elmo-folder-status ((folder elmo-nntp-folder))
  (elmo-nntp-folder-status folder))

(defun elmo-nntp-folder-status (folder)
  (let ((killed-list (elmo-msgdb-killed-list-load
		      (elmo-folder-msgdb-path folder)))
	end-num entry)
    (if elmo-nntp-groups-async
	(if (setq entry
		  (elmo-get-hash-val
		   (concat (elmo-nntp-folder-group-internal folder)
			   (elmo-nntp-folder-postfix
			    (elmo-net-folder-user-internal folder)
			    (elmo-net-folder-server-internal folder)
			    (elmo-net-folder-port-internal folder)
			    (elmo-net-folder-stream-type-internal folder)))
		   elmo-newsgroups-hashtb))
	    (progn
	      (setq end-num (nth 2 entry))
	      (when (and killed-list
			 (elmo-number-set-member end-num killed-list))
		;; Max is killed.
		(setq end-num nil))
	      (cons end-num (car entry)))
	  (error "No such newsgroup \"%s\""
		 (elmo-nntp-folder-group-internal folder)))
      (let ((session (elmo-nntp-get-session folder))
	    response e-num)
	(if (null session)
	    (error "Connection failed"))
	(save-excursion
	  (elmo-nntp-send-command session
				  (format
				   "group %s"
				   (elmo-nntp-folder-group-internal folder)))
	  (setq response (elmo-nntp-read-response session))
	  (if (and response
		   (string-match
		    "211 \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) [^.].+$"
		    response))
	      (progn
		(setq end-num (string-to-number (match-string 3 response)))
		(setq e-num (string-to-number (match-string 1 response)))
		(when (and killed-list
			   (elmo-number-set-member end-num killed-list))
		  ;; Max is killed.
		  (setq end-num nil))
		(cons end-num e-num))
	    (if (null response)
		(error "Selecting newsgroup \"%s\" failed"
		       (elmo-nntp-folder-group-internal folder))
	      nil)))))))

(defconst elmo-nntp-overview-index
  '(("number" . 0)
    ("subject" . 1)
    ("from" . 2)
    ("date" . 3)
    ("message-id" . 4)
    ("references" . 5)
    ("size" . 6)
    ("lines" . 7)
    ("xref" . 8)))

(defun elmo-nntp-create-msgdb-from-overview-string (folder
						    str
						    flag-table
						    &optional numlist)
  (let ((new-msgdb (elmo-make-msgdb))
	ov-list message-id entity
	ov-entity num
	field field-index flags)
    (setq ov-list (elmo-nntp-parse-overview-string str))
    (while ov-list
      (setq ov-entity (car ov-list))
;;; INN bug??
;;;      (if (or (> (setq num (string-to-number (aref ov-entity 0)))
;;;		 99999)
;;;	      (<= num 0))
;;;	  (setq num 0))
;;;      (setq num (number-to-string num))
      (setq num (string-to-number (aref ov-entity 0)))
      (when (or (null numlist)
		(memq num numlist))
	(setq entity (elmo-msgdb-make-message-entity
		      (elmo-msgdb-message-entity-handler new-msgdb)
		      :message-id (aref ov-entity 4)
		      :number     num
		      :references (nreverse
				   (mapcar 'std11-msg-id-string
					   (std11-parse-msg-ids-string
					    (aref ov-entity 5))))
		      :from       (elmo-with-enable-multibyte
				    (eword-decode-string
				     (elmo-delete-char  ?\"
							(or (aref ov-entity 2)
							    elmo-no-from))))
		      :subject    (or (elmo-with-enable-multibyte
					(eword-decode-string
					 (aref ov-entity 1)))
				      elmo-no-subject)
		      :date       (aref ov-entity 3)
		      :size       (string-to-number (aref ov-entity 6))))
	(dolist (extra elmo-msgdb-extra-fields)
	  (setq extra (downcase extra))
	  (when (and (setq field-index
			   (cdr (assoc extra elmo-nntp-overview-index)))
		     (> (length ov-entity) field-index))
	    (setq field (aref ov-entity field-index))
	    (when (eq field-index 8) ;; xref
	      (setq field (elmo-msgdb-remove-field-string field)))
	    (elmo-message-entity-set-field entity (intern extra) field)))
	(setq message-id (elmo-message-entity-field entity 'message-id)
	      flags (elmo-flag-table-get flag-table message-id))
	(elmo-global-flags-set flags folder num message-id)
	(elmo-msgdb-append-entity new-msgdb entity flags))
      (setq ov-list (cdr ov-list)))
    new-msgdb))

(luna-define-method elmo-folder-msgdb-create ((folder elmo-nntp-folder)
					      numbers flag-table)
  (elmo-nntp-folder-msgdb-create folder numbers flag-table))

(defun elmo-nntp-folder-msgdb-create (folder numbers flag-table)
  (let ((filter numbers)
	(session (elmo-nntp-get-session folder))
	(new-msgdb (elmo-make-msgdb))
	beg-num end-num cur length
	ov-str use-xover dir)
    (elmo-nntp-select-group session (elmo-nntp-folder-group-internal
				     folder))
    (when (setq use-xover (elmo-nntp-xover-p session))
      (setq beg-num (car numbers)
	    cur beg-num
	    end-num (nth (1- (length numbers)) numbers)
	    length  (+ (- end-num beg-num) 1))
      (elmo-with-progress-display (elmo-retrieve-overview length)
	  "Getting overview"
	(while (<= cur end-num)
	  (elmo-nntp-send-command
	   session
	   (format
	    "xover %s-%s"
	    (number-to-string cur)
	    (number-to-string
	     (+ cur
		elmo-nntp-overview-fetch-chop-length))))
	  (with-current-buffer (elmo-network-session-buffer session)
	    (if ov-str
		(elmo-msgdb-append
		 new-msgdb
		 (elmo-nntp-create-msgdb-from-overview-string
		  folder
		  ov-str
		  flag-table
		  filter))))
	  (if (null (elmo-nntp-read-response session t))
	      (progn
		(setq cur end-num);; exit while loop
		(elmo-nntp-set-xover session nil)
		(setq use-xover nil))
	    (if (null (setq ov-str (elmo-nntp-read-contents session)))
		(error "Fetching overview failed")))
	  (setq cur (+ elmo-nntp-overview-fetch-chop-length cur 1))
	  (elmo-progress-notify 'elmo-retrieve-overview
				:set (+ (- (min cur end-num) beg-num) 1)))))
    (if (not use-xover)
	(setq new-msgdb (elmo-nntp-msgdb-create-by-header
			 session numbers flag-table))
      (with-current-buffer (elmo-network-session-buffer session)
	(if ov-str
	    (elmo-msgdb-append
	     new-msgdb
	     (elmo-nntp-create-msgdb-from-overview-string
	      folder
	      ov-str
	      flag-table
	      filter)))))
    (elmo-folder-set-killed-list-internal
     folder
     (nconc
      (elmo-folder-killed-list-internal folder)
      (car (elmo-list-diff
	    numbers
	    (elmo-msgdb-list-messages new-msgdb)))))
    ;; If there are canceled messages, overviews are not obtained
    ;; to max-number(inn 2.3?).
    (when (and (elmo-nntp-max-number-precedes-list-active-p)
	       (elmo-nntp-list-active-p session))
      (elmo-nntp-send-command session
			      (format "list active %s"
				      (elmo-nntp-folder-group-internal
				       folder)))
      (if (null (elmo-nntp-read-response session))
	  (progn
	    (elmo-nntp-set-list-active session nil)
	    (error "NNTP list command failed")))
      (let ((killed (elmo-nntp-catchup-msgdb
		     new-msgdb
		     (nth 1 (read (concat "(" (elmo-nntp-read-contents
					       session) ")"))))))
	(when killed
	  (elmo-folder-kill-messages folder killed))))
    new-msgdb))

(luna-define-method elmo-folder-update-number ((folder elmo-nntp-folder))
  (when (elmo-nntp-max-number-precedes-list-active-p)
    (let ((session (elmo-nntp-get-session folder)))
      (when (elmo-nntp-list-active-p session)
	(let ((numbers (elmo-folder-list-messages folder nil 'in-msgdb))
	      msgdb-max max-number)
	  ;; If there are canceled messages, overviews are not obtained
	  ;; to max-number(inn 2.3?).
	  (elmo-nntp-select-group session
				  (elmo-nntp-folder-group-internal folder))
	  (elmo-nntp-send-command session
				  (format "list active %s"
					  (elmo-nntp-folder-group-internal
					   folder)))
	  (if (null (elmo-nntp-read-response session))
	      (error "NNTP list command failed"))
	  (setq max-number
		(nth 1 (read (concat "(" (elmo-nntp-read-contents
					  session) ")"))))
	  (setq msgdb-max (if numbers (apply #'max numbers) 0))
	  (when (and msgdb-max
		     max-number
		     (< msgdb-max max-number))
	    (let ((i (1+ msgdb-max))
		  killed)
	      (while (<= i max-number)
		(setq killed (cons i killed))
		(incf i))
	      (elmo-folder-kill-messages folder (nreverse killed)))))))))

(defun elmo-nntp-msgdb-create-by-header (session numbers flag-table)
  (with-temp-buffer
    (elmo-nntp-retrieve-headers session (current-buffer) numbers)
    (elmo-nntp-msgdb-create-message
     (length numbers) flag-table)))

(defun elmo-nntp-parse-xhdr-response (string)
  (let (response)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (not (eobp))
	(if (looking-at "^\\([0-9]+\\) \\(.*\\)$")
	    (setq response (cons (cons (string-to-number (elmo-match-buffer 1))
				       (elmo-match-buffer 2))
				 response)))
	(forward-line)))
    (nreverse response)))

(defun elmo-nntp-parse-overview-string (string)
  (let ((index 0)
	(length (length string))
	next result)
    (while (or (setq next (string-match "\n" string index))
	       (/= index length))
      (setq result (cons (apply 'vector (split-string
					 (substring string index next) "\t"))
			 result)
	    index (if next (1+ next) length)))
    (nreverse result)))

(defun elmo-nntp-get-newsgroup-by-msgid (msgid server user port type)
  "Get nntp header string."
  (save-excursion
    (let ((session (elmo-nntp-get-session
		    (luna-make-entity
		     'elmo-nntp-folder
		     :user user
		     :server server
		     :port port
		     :stream-type type))))
      (elmo-nntp-send-command session
			      (format "head %s" msgid))
      (if (elmo-nntp-read-response session)
	  (elmo-nntp-read-contents session))
      (with-current-buffer (elmo-network-session-buffer session)
	(std11-field-body "Newsgroups")))))

(luna-define-method elmo-message-fetch :around
  ((folder elmo-nntp-folder) number strategy &optional unread section)
  (when (luna-call-next-method)
    (elmo-nntp-setup-crosspost-buffer folder number)
    (unless unread
      (elmo-nntp-folder-update-crosspost-message-alist
       folder (list number)))
    t))

(luna-define-method elmo-message-fetch-plugged ((folder elmo-nntp-folder)
						number strategy
						&optional section outbuf
						unread)
  (elmo-nntp-message-fetch folder number strategy section outbuf unread))

(defun elmo-nntp-message-fetch (folder number strategy section outbuf unread)
  (let ((session (elmo-nntp-get-session folder))
	newsgroups)
    (with-current-buffer (elmo-network-session-buffer session)
      (elmo-nntp-select-group session (elmo-nntp-folder-group-internal folder))
      (elmo-nntp-send-command session (format "article %s" number))
      (if (null (elmo-nntp-read-response session t))
	  (progn
	    (with-current-buffer outbuf (erase-buffer))
	    (message "Fetching message failed")
	    nil)
	(prog1 (elmo-nntp-read-body session outbuf)
	  (with-current-buffer outbuf
	    (goto-char (point-min))
	    (while (re-search-forward "^\\." nil t)
	      (replace-match "")
	      (forward-line))
	    (elmo-nntp-setup-crosspost-buffer folder number)
	    (unless unread
	      (elmo-nntp-folder-update-crosspost-message-alist
	       folder (list number)))))))))

(defun elmo-nntp-post (hostname content-buf)
  (let ((session (elmo-nntp-get-session
		  (luna-make-entity
		   'elmo-nntp-folder
		   :user elmo-nntp-default-user
		   :server hostname
		   :port elmo-nntp-default-port
		   :stream-type
		   (elmo-get-network-stream-type
		    elmo-nntp-default-stream-type))))
	response has-message-id)
    (with-current-buffer content-buf
      (goto-char (point-min))
      (if (search-forward mail-header-separator nil t)
	  (delete-region (match-beginning 0)(match-end 0)))
      (setq has-message-id (let ((elmo-prefer-std11-parser t)) 
			     (elmo-get-message-id-from-buffer 'none)))
      (elmo-nntp-send-command session "post")
      (if (string-match "^340" (setq response
				     (elmo-nntp-read-raw-response session)))
	  (if (string-match "recommended ID \\(<[^@]+@[^>]+>\\)" response)
	      (unless has-message-id
		;; We should remove invalid Message-ID header.
		(save-restriction
		  (save-match-data
		    (std11-narrow-to-header)
		    (goto-char (point-min))
		    (let ((case-fold-search t))
		      (if (re-search-forward "^message-id:[ \t]*" nil t)
			  (delete-region
			   (match-beginning 0)
			   (min (point-max) (1+ (std11-field-end))))))))
		(goto-char (point-min))
		(insert (concat "Message-ID: "
				(match-string 1 response) "\n"))))
	(error "POST failed"))
      (run-hooks 'elmo-nntp-post-pre-hook)
      (elmo-nntp-send-buffer session content-buf)
      (elmo-nntp-send-command session ".")
;;;      (elmo-nntp-read-response buffer process t)
      (if (not (string-match
		"^2" (setq response (elmo-nntp-read-raw-response
				     session))))
	  (error "NNTP error: %s" response)))))

(defsubst elmo-nntp-send-data-line (session line)
  "Send LINE to SESSION."
  ;; Escape "." at start of a line
  (if (eq (string-to-char line) ?.)
      (process-send-string (elmo-network-session-process-internal
			    session) "."))
  (process-send-string (elmo-network-session-process-internal
			session) line)
  (process-send-string (elmo-network-session-process-internal
			session) "\r\n"))

(defun elmo-nntp-send-buffer (session databuf)
  "Send data content of DATABUF to SESSION."
  (let ((data-continue t)
	line bol)
    (with-current-buffer databuf
      (goto-char (point-min))
      (while data-continue
	(beginning-of-line)
	(setq bol (point))
	(end-of-line)
	(setq line (buffer-substring bol (point)))
	(unless (zerop (forward-line)) (setq data-continue nil))
	(elmo-nntp-send-data-line session line)))))

(luna-define-method elmo-folder-delete-messages ((folder elmo-nntp-folder)
						 numbers)
  (elmo-folder-kill-messages folder numbers)
  t)

(luna-define-method elmo-folder-exists-p-plugged ((folder elmo-nntp-folder))
  (let ((session (elmo-nntp-get-session folder)))
    (elmo-nntp-send-command
     session
     (format "group %s"
	     (elmo-nntp-folder-group-internal folder)))
    (elmo-nntp-read-response session)))

(defun elmo-nntp-retrieve-field (spec field from-msgs)
  "Retrieve FIELD values from FROM-MSGS.
Returns a list of cons cells like (NUMBER . VALUE)"
  (let ((session (elmo-nntp-get-session spec))
	result)
    (setq result
	  (if (elmo-nntp-xhdr-p session)
	      (progn
		(elmo-nntp-select-group
		 session (elmo-nntp-folder-group-internal spec))
		(elmo-nntp-send-command
		 session
		 (format "xhdr %s %s"
			 field
			 (if from-msgs
			     (format "%d-%d"
				     (apply 'min from-msgs)
				     (apply 'max from-msgs))
			   "0-")))
		(if (elmo-nntp-read-response session t)
		    (elmo-nntp-parse-xhdr-response
		     (elmo-nntp-read-contents session))
		  (elmo-nntp-set-xhdr session nil)
		  (error "NNTP XHDR command failed")))))
    (if from-msgs
	(delq nil (mapcar (lambda (pair) (when (memq (car pair) from-msgs)
					   pair))
			  result))
      result)))

(defun elmo-nntp-search-primitive (spec condition &optional from-msgs)
  (let ((search-key (elmo-filter-key condition))
	(numbers (cond
		  ((null from-msgs) (elmo-folder-list-messages spec))
		  ((listp from-msgs) from-msgs)
		  (t (elmo-folder-list-messages spec 'visible 'in-msgdb)))))
    (cond
     ((string= "last" search-key)
      (nthcdr (max (- (length numbers)
		      (string-to-number (elmo-filter-value condition)))
		   0)
	      numbers))
     ((string= "first" search-key)
      (car (elmo-list-diff
	    numbers
	    (nthcdr (string-to-number (elmo-filter-value condition))
		    numbers))))
     ((member search-key '("since" "before"))
      (let ((specified-date (elmo-date-make-sortable-string
			     (elmo-date-get-datevec (elmo-filter-value
						     condition))))
	    (since (string= "since" search-key))
	    field-date)
	(if (eq (elmo-filter-type condition) 'unmatch)
	    (setq since (not since)))
	(delq nil
	      (mapcar
	       (lambda (pair)
		 (setq field-date
		       (elmo-date-make-sortable-string
			(timezone-fix-time
			 (cdr pair)
			 (current-time-zone) nil)))
		 (if (if since
			 (null (string< field-date specified-date))
		       (string< field-date specified-date))
		     (car pair)))
	       (elmo-nntp-retrieve-field spec "date" numbers)))))
     (t
      (let ((val (elmo-filter-value condition))
	    (negative (eq (elmo-filter-type condition) 'unmatch))
	    (case-fold-search t))
	(delq nil
	      (mapcar
	       (lambda (pair)
		 (if (string-match val (eword-decode-string
					(decode-mime-charset-string
					 (cdr pair) elmo-mime-charset)))
		     (unless negative (car pair))
		   (if negative (car pair))))
	       (elmo-nntp-retrieve-field spec search-key numbers))))))))

(defun elmo-nntp-search-internal (folder condition from-msgs)
  (let (result)
    (cond
     ((vectorp condition)
      (setq result (elmo-nntp-search-primitive
		    folder condition from-msgs)))
     ((eq (car condition) 'and)
      (setq result (elmo-nntp-search-internal folder
					      (nth 1 condition)
					      from-msgs)
	    result (elmo-list-filter result
				     (elmo-nntp-search-internal
				      folder (nth 2 condition)
				      from-msgs))))
     ((eq (car condition) 'or)
      (setq result (elmo-nntp-search-internal folder
					      (nth 1 condition)
					      from-msgs)
	    result (elmo-sort-uniq-number-list
		    (nconc result
			   (elmo-nntp-search-internal
			    folder (nth 2 condition) from-msgs))
		    ))))))

(defun elmo-nntp-use-server-search-p (condition)
  (if (vectorp condition)
      (not (member (elmo-filter-key condition) '("raw-body" "body" "flag")))
    (and (elmo-nntp-use-server-search-p (nth 1 condition))
	 (elmo-nntp-use-server-search-p (nth 2 condition)))))

(luna-define-method elmo-folder-search :around ((folder elmo-nntp-folder)
						condition &optional from-msgs)
  (if (and (elmo-folder-plugged-p folder)
	   (elmo-nntp-use-server-search-p condition))
      (elmo-nntp-search-internal folder condition from-msgs)
    (luna-call-next-method)))

(defun elmo-nntp-get-folders-info-prepare (folder session-keys)
  (condition-case ()
      (let ((session (elmo-nntp-get-session folder))
	    key count)
	(with-current-buffer (elmo-network-session-buffer session)
	  (unless (setq key (assoc session session-keys))
	    (erase-buffer)
	    (setq key (cons session
			    (vector 0
				    (elmo-net-folder-server-internal folder)
				    (elmo-net-folder-user-internal folder)
				    (elmo-net-folder-port-internal folder)
				    (elmo-net-folder-stream-type-internal
				     folder))))
	    (setq session-keys (nconc session-keys (list key))))
	  (elmo-nntp-send-command session
				  (format "group %s"
					  (elmo-nntp-folder-group-internal
					   folder))
				  'noerase)
	  (if elmo-nntp-get-folders-securely
	      (accept-process-output
	       (elmo-network-session-process-internal session)
	       1))
	  (setq count (aref (cdr key) 0))
	  (aset (cdr key) 0 (1+ count))))
    (error
     (when elmo-auto-change-plugged
       (sit-for 1))
     nil))
  session-keys)

(defun elmo-nntp-get-folders-info (session-keys)
  (let ((sessions session-keys)
	(cur (get-buffer-create " *ELMO NNTP Temp*")))
    (while sessions
      (let* ((session (caar sessions))
	     (key     (cdar sessions))
	     (count   (aref key 0))
	     (server  (aref key 1))
	     (user    (aref key 2))
	     (port    (aref key 3))
	     (type    (aref key 4))
	     (hashtb (or elmo-newsgroups-hashtb
			 (setq elmo-newsgroups-hashtb
			       (elmo-make-hash count)))))
	(save-excursion
	  (elmo-nntp-groups-read-response session cur count)
	  (set-buffer cur)
	  (goto-char (point-min))
	  (let ((case-replace nil)
		(postfix (elmo-nntp-folder-postfix user server port type)))
	    (if (not (string= postfix ""))
		(save-excursion
		  (while (re-search-forward "^\\(211 [0-9]+ [0-9]+ [0-9]+ [^ \n]+\\)\\(.*\\)$" nil t)
		    (replace-match (concat (match-string 1)
					   (elmo-replace-in-string
					    postfix
					    "\\\\" "\\\\\\\\\\\\\\\\")))))))
	  (let (len min max group)
	    (while (not (eobp))
	      (condition-case ()
		  (when (= (following-char) ?2)
		    (read cur)
		    (setq len (read cur)
			  min (read cur)
			  max (read cur))
		    (set (setq group (let ((obarray hashtb)) (read cur)))
			 (list len min max)))
		(error (and group (symbolp group) (set group nil))))
	      (forward-line))))
	(setq sessions (cdr sessions))))
    (kill-buffer cur)))

;; original is 'nntp-retrieve-groups [Gnus]
(defun elmo-nntp-groups-read-response (session outbuf count)
  (let* ((received 0)
	 (last-point (point-min)))
    (with-current-buffer (elmo-network-session-buffer session)
      (accept-process-output
       (elmo-network-session-process-internal session) 1)
      (discard-input)
      ;; Wait for all replies.
      (elmo-with-progress-display (elmo-nntp-groups-read-response count)
	  "Getting folders info"
	(while (progn
		 (goto-char last-point)
		 ;; Count replies.
		 (while (re-search-forward "^[0-9]" nil t)
		   (setq received (1+ received)))
		 (setq last-point (point))
		 (< received count))
	  (accept-process-output
	   (elmo-network-session-process-internal session)
	   1)
	  (discard-input)
	  (elmo-progress-notify 'elmo-nntp-groups-read-response :set received)))
      ;; Wait for the reply from the final command.
      (goto-char (point-max))
      (re-search-backward "^[0-9]" nil t)
      (when (looking-at "^[23]")
	(while (progn
		 (goto-char (point-max))
		 (not (re-search-backward "\r?\n" (- (point) 3) t)))
	  (accept-process-output
	   (elmo-network-session-process-internal session) 1)
	  (discard-input)))
      ;; Now all replies are received.  We remove CRs.
      (elmo-delete-cr-buffer)
      (copy-to-buffer outbuf (point-min) (point-max)))))

;; from nntp.el [Gnus]

(defsubst elmo-nntp-next-result-arrived-p ()
  (if (eq (following-char) ?2)
      (re-search-forward "\n\\.\r?\n" nil t)
    (if (memq (following-char) '(?3 ?4))
	(search-forward "\n" nil t))))

(defun elmo-nntp-retrieve-headers (session outbuf articles)
  "Retrieve the headers of ARTICLES."
  (with-current-buffer (elmo-network-session-buffer session)
    (erase-buffer)
    (let ((number (length articles))
	  (count 0)
	  (received 0)
	  (last-point (point-min))
	  article)
      (elmo-with-progress-display (elmo-retrieve-header number)
	  "Getting headers"
	;; Send HEAD commands.
	(while (setq article (pop articles))
	  (elmo-nntp-send-command session
				  (format "head %s" article)
				  'noerase)
	  (setq count (1+ count))
	  ;; Every 200 requests we have to read the stream in
	  ;; order to avoid deadlocks.
	  (when (or (null articles)	;All requests have been sent.
		    (zerop (% count elmo-nntp-header-fetch-chop-length)))
	    (accept-process-output
	     (elmo-network-session-process-internal session) 1)
	    (discard-input)
	    (while (progn
		     (goto-char last-point)
		     ;; Count replies.
		     (while (elmo-nntp-next-result-arrived-p)
		       (setq last-point (point))
		       (setq received (1+ received)))
		     (< received count))
	      (elmo-progress-notify 'elmo-retrieve-header :set received)
	      (accept-process-output
	       (elmo-network-session-process-internal session) 1)
	      (discard-input)))))
      ;; Replace all CRLF with LF.
      (elmo-delete-cr-buffer)
      (copy-to-buffer outbuf (point-min) (point-max)))))

;; end of from Gnus

(defun elmo-nntp-msgdb-create-message (len flag-table)
  (save-excursion
    (let ((new-msgdb (elmo-make-msgdb))
	  beg entity num message-id)
      (set-buffer-multibyte nil)
      (goto-char (point-min))
      (elmo-with-progress-display (elmo-folder-msgdb-create len)
	  "Creating msgdb"
	(while (not (eobp))
	  (setq beg (save-excursion (forward-line) (point)))
	  (setq num
		(and (looking-at "^2[0-9]*[ ]+\\([0-9]+\\)")
		     (string-to-number
		      (elmo-match-buffer 1))))
	  (elmo-nntp-next-result-arrived-p)
	  (when num
	    (save-excursion
	      (forward-line -1)
	      (save-restriction
		(narrow-to-region beg (point))
		(setq entity
		      (elmo-msgdb-create-message-entity-from-header
		       (elmo-msgdb-message-entity-handler new-msgdb) num))
		(when entity
		  (setq message-id
			(elmo-message-entity-field entity 'message-id))
		  (elmo-msgdb-append-entity
		   new-msgdb
		   entity
		   (elmo-flag-table-get flag-table message-id))))))
	  (elmo-progress-notify 'elmo-folder-msgdb-create)))
      new-msgdb)))

(luna-define-method elmo-message-use-cache-p ((folder elmo-nntp-folder) number)
  elmo-nntp-use-cache)

(defun elmo-nntp-parse-newsgroups (string &optional subscribe-only)
  (let ((nglist (elmo-parse string "[ \t\f\r\n,]*\\([^ \t\f\r\n,]+\\)"))
	ngs)
    (if (not subscribe-only)
	nglist
      (dolist (ng nglist)
	(if (intern-soft ng elmo-newsgroups-hashtb)
	    (setq ngs (cons ng ngs))))
      ngs)))

;;; Crosspost processing.

;; 1. setup crosspost alist.
;;    1.1. When message is fetched and is crossposted message,
;;         it is remembered in `temp-crosses' slot.
;;         temp-crosses slot is a list of cons cell:
;;         (NUMBER . (MESSAGE-ID (LIST-OF-NEWSGROUPS) 'ng))
;;    1.2. In elmo-folder-close, `temp-crosses' slot is cleared,
;;    1.3. In elmo-folder-flag-as-read, move crosspost entry
;;         from `temp-crosses' slot to `elmo-crosspost-message-alist'.

;; 2. process crosspost alist.
;;    2.1. At elmo-folder-process-crosspost, setup `reads' slot from
;;         `elmo-crosspost-message-alist'.
;;    2.2. remove crosspost entry for current newsgroup from
;;         `elmo-crosspost-message-alist'.
;;    2.3. elmo-folder-list-unreads return unread message list according to
;;         `reads' slot.
;;         (There's a problem that if `elmo-folder-list-unreads'
;;           never executed, crosspost information is thrown away.)
;;    2.4. In elmo-folder-close, `read' slot is cleared,

(defun elmo-nntp-setup-crosspost-buffer (folder number)
;;    1.1. When message is fetched and is crossposted message,
;;         it is remembered in `temp-crosses' slot.
;;         temp-crosses slot is a list of cons cell:
;;         (NUMBER . (MESSAGE-ID (LIST-OF-NEWSGROUPS) 'ng))
  (let ((elmo-prefer-std11-parser t)
	newsgroups crosspost-newsgroups message-id)
    (save-restriction
      (std11-narrow-to-header)
      (setq newsgroups (std11-fetch-field "newsgroups")
	    message-id (elmo-get-message-id-from-header 'none)))
    (when newsgroups
      (when (setq crosspost-newsgroups
		  (delete
		   (elmo-nntp-folder-group-internal folder)
		   (elmo-nntp-parse-newsgroups newsgroups t)))
	(unless (assq number
		      (elmo-nntp-folder-temp-crosses-internal folder))
	  (elmo-nntp-folder-set-temp-crosses-internal
	   folder
	   (cons (cons number (list message-id crosspost-newsgroups 'ng))
		 (elmo-nntp-folder-temp-crosses-internal folder))))))))

(luna-define-method elmo-folder-close-internal ((folder elmo-nntp-folder))
;;    1.2. In elmo-folder-close, `temp-crosses' slot is cleared,
  (elmo-nntp-folder-set-temp-crosses-internal folder nil)
  (elmo-nntp-folder-set-reads-internal folder nil)
  )

(defun elmo-nntp-folder-update-crosspost-message-alist (folder numbers)
;;    1.3. In elmo-folder-flag-as-read, move crosspost entry
;;         from `temp-crosses' slot to `elmo-crosspost-message-alist'.
  (let (elem)
    (dolist (number numbers)
      (when (setq elem (assq number
			     (elmo-nntp-folder-temp-crosses-internal folder)))
	(unless (assoc (cdr (cdr elem)) elmo-crosspost-message-alist)
	  (setq elmo-crosspost-message-alist
		(cons (cdr elem) elmo-crosspost-message-alist)))
	(elmo-nntp-folder-set-temp-crosses-internal
	 folder
	 (delq elem (elmo-nntp-folder-temp-crosses-internal folder)))))))

(luna-define-method elmo-folder-set-flag :before ((folder elmo-nntp-folder)
						  numbers
						  flag
						  &optional is-local)
  (when (eq flag 'read)
    (elmo-nntp-folder-update-crosspost-message-alist folder numbers)))

(luna-define-method elmo-folder-unset-flag :before ((folder elmo-nntp-folder)
						    numbers
						    flag
						    &optional is-local)
  (when (eq flag 'unread)
    (elmo-nntp-folder-update-crosspost-message-alist folder numbers)))

(defsubst elmo-nntp-folder-process-crosspost (folder)
;;    2.1. At elmo-folder-process-crosspost, setup `reads' slot from
;;         `elmo-crosspost-message-alist'.
;;    2.2. remove crosspost entry for current newsgroup from
;;         `elmo-crosspost-message-alist'.
  (let (cross-deletes reads entity ngs)
    (dolist (cross elmo-crosspost-message-alist)
      (when (setq entity (elmo-message-entity folder (nth 0 cross)))
	(setq reads (cons (elmo-message-entity-number entity) reads)))
      (when entity
	(if (setq ngs (delete (elmo-nntp-folder-group-internal folder)
			      (nth 1 cross)))
	    (setcar (cdr cross) ngs)
	  (setq cross-deletes (cons cross cross-deletes)))
	(setq elmo-crosspost-message-alist-modified t)))
    (dolist (dele cross-deletes)
      (setq elmo-crosspost-message-alist (delq
					  dele
					  elmo-crosspost-message-alist)))
    (elmo-nntp-folder-set-reads-internal folder reads)))

(luna-define-method elmo-folder-process-crosspost ((folder elmo-nntp-folder))
  (elmo-nntp-folder-process-crosspost folder))

(luna-define-method elmo-folder-list-flagged :around ((folder elmo-nntp-folder)
						      flag &optional in-msgdb)
  ;;    2.3. elmo-folder-list-unreads return unread message list according to
  ;;         `reads' slot.
  (let ((msgs (luna-call-next-method)))
    (if in-msgdb
	msgs
      (case flag
	(unread
	 (elmo-living-messages msgs (elmo-nntp-folder-reads-internal folder)))
	;; Should consider read, digest and any flag?
	(otherwise
	 msgs)))))

(require 'product)
(product-provide (provide 'elmo-nntp) (require 'elmo-version))

;;; elmo-nntp.el ends here
