;;; acap.el --- An ACAP interface.

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: ACAP

;; Copyright (C) 2001 Yuuichi Teranishi <teranisi@gohome.org>

;; This file is not part of GNU Emacs

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
;; acap.el is an elisp library providing an interface for talking to
;; ACAP (RFC2244) servers.
;;
;; This is a transcript of short interactive session for demonstration
;; purposes.

;; (setq proc (acap-open "my.acap.server" "username" "CRAM-MD5"))
;; => #<process ACAP>
;;
;; (acap-search proc "/addressbook/" '((RETURN ("*")))))
;; => ((done-ok nil "search completed")
;;     (modtime . "20010828091433000010")
;;     (entry "user"
;;	   ((("subdataset"
;;	      ("."))
;;	     ("modtime" "20010824004532000003")
;;	     ("entry" "user"))))
;;     (entry ""
;;	   ((("modtime" "20010824004532000002")
;;	     ("entry" "")
;;	     ("dataset.owner" "anonymous")
;;	     ("dataset.acl" ("$anyone	xrwia")))))
;;
;; (acap-close proc)
;; => t
;;
;; Todo:
;;  * Send literal data for STORE.

;;; History:
;;
;; 27 Aug 2001 Created (Some codes are based on imap.el.).

;;; Code:

(eval-when-compile (require 'cl))
(require 'pces)
(require 'sasl)

;; User variables.
(defgroup acap nil
  "Low level ACAP issues."
  :group 'applications)

(defcustom acap-default-user (user-login-name)
  "Default username to use."
  :type 'string
  :group 'acap)

(defcustom acap-default-port 674
  "Default port for ACAP."
  :type 'integer
  :group 'acap)

(defcustom acap-stock-passphrase nil
  "Stock passphrase on memory if t."
  :type 'boolean
  :group 'acap)

;; Constants.
(defconst acap-server-eol "\r\n"
  "The EOL string sent from the server.")

(defconst acap-client-eol "\r\n"
  "The EOL string sent from the server.")

;; Internal variables.
(defvar acap-state 'closed
  "ACAP state.
Valid states are `closed', `initial', `auth'.")

(defvar acap-capability nil
  "Capability for server.")

(defvar acap-reached-tag 0
  "Lower limit on command tags that have been parsed.")

(defvar acap-tag 0
  "Command tag number.")

(defvar acap-auth nil
  "Authenticated mechanism name.")

(defvar acap-process nil
  "Process for the buffer.")

(defvar acap-server nil
  "Server name.")

(defvar acap-port nil
  "Port number.")

(defvar acap-response nil
  "ACAP Response.")

(defvar acap-logging-out nil
  "Non-nil when ACAP is logging out.")

(make-variable-buffer-local 'acap-state)
(make-variable-buffer-local 'acap-auth)
(make-variable-buffer-local 'acap-capability)
(make-variable-buffer-local 'acap-reached-tag)
(make-variable-buffer-local 'acap-failed-tag)
(make-variable-buffer-local 'acap-tag)
(make-variable-buffer-local 'acap-server)
(make-variable-buffer-local 'acap-port)
(make-variable-buffer-local 'acap-response)
(make-variable-buffer-local 'acap-logging-out)

(defvar acap-network-stream-alist
  '((default . open-network-stream-as-binary)))

(defun acap-network-stream-open (buffer server port &optional type)
  (let* ((port (or port acap-default-port))
	 (process (progn
		    (message "Connecting to %s..." server)
		    (funcall (cdr (assq (or type 'default)
					acap-network-stream-alist))
			     "ACAP" buffer server port))))
    (when process
      (with-current-buffer buffer
	(while (and (memq (process-status process) '(open run))
		    (goto-char (point-min))
		    (not (setq acap-capability (acap-parse-greeting))))
	  (message "Waiting for response from %s..." server)
	  (accept-process-output process 1))
	(message "Waiting for response from %s...done" server)
	(when (memq (process-status process) '(open run))
	  process)))))

(defvar acap-passphrase nil)
(defvar acap-rp-user nil)
(defvar acap-rp-server nil)
(defvar acap-rp-auth nil)

(defvar acap-passphrase-alist nil)

(eval-and-compile
  (autoload 'ange-ftp-read-passwd "ange-ftp"))

(defun acap-read-passphrase (prompt)
  "Prompt is not used."
  (or acap-passphrase
      (progn
	(setq prompt (format "%s passphrase for %s@%s: "
			     acap-rp-auth acap-rp-user acap-rp-server))
	(if (functionp 'read-passwd)
	    (read-passwd prompt)
	  (if (load "passwd" t)
	      (read-passwd prompt)
	    (ange-ftp-read-passwd prompt))))))

;;; Debug.
(defvar acap-debug t)
(defvar acap-debug-buffer nil)
(defun acap-debug (string)
  "Insert STRING to the debug buffer."
  (when acap-debug
    (if (or (null acap-debug-buffer)
	    (not (bufferp acap-debug-buffer))
	    (not (buffer-live-p acap-debug-buffer)))
	(setq acap-debug-buffer (get-buffer-create "*Debug acap*")))
    (with-current-buffer acap-debug-buffer
      (goto-char (point-max))
      (insert string))))

;;; Stock passphrase (Not implemented yet)
(defun acap-stock-passphrase (user server auth passphrase)
  (let ((key (format "%s/%s/%s" user server auth))
	pair)
    (when (setq pair (assoc key acap-passphrase-alist))
      (setq acap-passphrase-alist (delete pair acap-passphrase-alist)))
    (setq acap-passphrase-alist (cons
				 (cons key passphrase)
				 acap-passphrase-alist))))

(defun acap-stocked-passphrase (user server auth)
  (when acap-stock-passphrase
    (let ((key (format "%s/%s/%s" user server auth)))
      (cdr (assoc key acap-passphrase-alist)))))

(defun acap-remove-stocked-passphrase (user server auth)
  (let ((key (format "%s/%s/%s" user server auth)))
    (setq acap-passphrase-alist
	  (delq (assoc key acap-passphrase-alist)
		acap-passphrase-alist))))

;;; Open, Close
(defun acap-open (server &optional user auth port type)
  (let* ((user (or user acap-default-user))
	 (buffer (get-buffer-create (concat " *acap on " user " at " server)))
	 process passphrase mechanism tag)
    (with-current-buffer buffer
      (erase-buffer)
      (if acap-process
	  (delete-process acap-process))
      (setq process (acap-network-stream-open buffer server port type)
	    acap-process process)
      (set-buffer-multibyte nil)
      (buffer-disable-undo)
      (setq acap-state 'initial)
      (set-process-filter process 'acap-arrival-filter)
      (set-process-sentinel process 'acap-sentinel)
      (while (and (memq (process-status process) '(open run))
		  (not (eq acap-state 'auth)))
	(setq acap-auth
	      (unwind-protect
		  (let* ((mechanism
			  (sasl-find-mechanism
			   (if auth
			       (list auth)
			     (cdr (or (assq 'Sasl acap-capability)
				      (assq 'SASL acap-capability))))))
			 (sclient
			  (sasl-make-client mechanism user "acap" server))
			 (sasl-read-passphrase 'acap-read-passphrase)
			 (acap-rp-user user)
			 (acap-rp-server server)
			 (acap-rp-auth (sasl-mechanism-name mechanism))
			 acap-passphrase step response cont-string)
		    (unless (string= (sasl-mechanism-name mechanism)
				     "ANONYMOUS")
		      (setq acap-passphrase (acap-read-passphrase nil)))
		    (setq tag (acap-send-command
			       process
			       (concat
				(format "AUTHENTICATE \"%s\""
					(sasl-mechanism-name mechanism))
				(if (and (setq step
					       (sasl-next-step sclient nil))
					 (sasl-step-data step))
				    (concat " " (prin1-to-string
						 (sasl-step-data step)))))))
		    (when (setq response (acap-wait-for-response process tag))
		      (while (acap-response-cont-p response)
			(sasl-step-set-data
			 step (acap-response-cont-string response))
			(acap-response-clear process)
			(if (setq step (sasl-next-step sclient step))
			    (with-temp-buffer
			      (insert (or (sasl-step-data step) ""))
			      (setq response (acap-send-data-wait
					      process (current-buffer) tag)))
			  (setq response nil)))
		      (if (acap-response-ok-p response)
			  (progn
			    (setq acap-state 'auth)
			    mechanism)
			(message "Authentication failed.")
			(sit-for 1))))
		nil)))
      (unless acap-auth
	(message "acap: Connecting to %s...failed" server))
      (setq acap-server server
	    acap-port port)
      process)))

(defun acap-close (process)
  (with-current-buffer (process-buffer process)
    (setq acap-logging-out t)
    (unless (acap-response-ok-p (acap-send-command-wait process "LOGOUT"))
      (message "Server %s didn't let me log out" acap-server))
    (when (memq (process-status process) '(open run))
      (delete-process process))
    (erase-buffer)
    t))

;;; Commands

(defun acap-noop (process)
  "Execute NOOP command on PROCESS."
  (acap-send-command-wait process "NOOP"))

(defun acap-lang (process lang-list)
  "Execute LANG command on PROCESS."
  (acap-send-command-wait process
			  (mapconcat
			   'identity
			   (nconc (list "LANG")
				  (mapcar 'prin1-to-string lang-list))
			   " ")))

(defun acap-search (process target &optional modifier criteria)
  "Execute SEARCH command on PROCESS.
TARGET is a string which specifies what is to be searched
\(dataset or context name\).
MODIFIER is an alist of modifiers. Each element should be a list like
\(MODIFIER-NAME DATA1 DATA2...\).
CRITERIA is a search criteria string.
If CRITERIA is not specified, \"ALL\" is assumed,
Modifiers and search criteria are described in section 6.4.1 of RFC2244.

Examples:
\(acap-search process
	     \"/addressbook/\"
	     '\((DEPTH 3\)
               \(RETURN \(\"addressbook.Alias\"
                        \"addressbook.Email\"
                        \"addressbook.List\"\)\)\)
	     \"OR NOT EQUAL \\\"addressbook.Email\\\" \\\"i\;octed\\\" NIL\\
                 NOT EQUAL \\\"addressbook.Email\\\" \\\"i\;octed\\\" NIL\"\)

\(acap-search process
	     \"/addressbook/user/fred/\"
	     '\(\(RETURN \(\"*\"\)\)
	     \"EQUAL \\\"entry\\\" \\\"i\;octed\\\" \\\"A0345\\\"\"\)"
  (acap-send-command-wait process
			  (concat "SEARCH " (prin1-to-string target)
				  (if modifier " ")
				  (mapconcat
				   'prin1-to-string
				   (acap-flatten modifier)
				   " ")
				  " "
				  (or criteria "ALL"))))

(defun acap-freecontext (process name)
  "Execute FREECONTEXT command on PROCESS."
  (acap-send-command-wait process
			  (concat "FREECONTEXT " name)))

(defun acap-updatecontext (process names)
  "Execute UPDATECONTEXT command on PROCESS."
  (acap-send-command-wait process
			  (mapconcat
			   'identity
			   (nconc (list "FREECONTEXT") names)
			   " ")))

(defun acap-store (process entries)
  "Execute STORE command on PROCESS.
ENTRIES is a store-entry list."
  (with-temp-buffer
    ;; As far as I know, current implementation of ACAP server
    ;; (cyrus-smlacapd 0.5) does not accept literal argument for STORE.
    ;; If literal argument is available, command arguments can be sent using
    ;; function `acap-send-command-wait'.
    (set-buffer-multibyte nil)
    (insert "STORE (")
    (let (beg tag)
      (while entries
	(cond
	 ((stringp (car entries))
	  (setq beg (point))
	  (insert (car entries))
	  (goto-char beg)
	  (while (re-search-forward "\\\\" nil t)
	    (replace-match "\\\\\\\\"))
	  (goto-char beg)
	  (while (re-search-forward "\"" nil t)
	    (replace-match "\\\\\""))
	  (goto-char beg)
	  (insert "\"")
	  (goto-char (point-max))
	  (insert "\""))
	 ((symbolp (car entries))
	  (insert (prin1-to-string (car entries)))))
	(if (cdr entries)(insert " "))
	(setq entries (cdr entries)))
      (insert ")")
      (goto-char (point-min))
      (insert (with-current-buffer (process-buffer process)
		(number-to-string (setq tag (setq acap-tag (1+ acap-tag)))))
	      " ")
      (process-send-region process (point-min) (point-max))
      (acap-debug (concat (buffer-string) acap-client-eol))
      (process-send-string process acap-client-eol)
      (acap-wait-for-response process tag))))

(defun acap-deletedsince (process name time)
  "Execute DELETEDSINCE command on PROCESS."
  (acap-send-command-wait process
			  (concat "DELETEDSINCE "
				  (prin1-to-string name)
				  " "
				  (prin1-to-string (acap-encode-time time)))))

(defun acap-setacl (process object identifier rights)
  "Execute SETACL command on PROCESS."
  (acap-send-command-wait process
			  (concat "SETACL "
				  (prin1-to-string object)
				  " "
				  (prin1-to-string identifier)
				  " "
				  (prin1-to-string rights))))

(defun acap-deleteacl (process object &optional identifier)
  "Execute DELETEACL command on PROCESS."
  (acap-send-command-wait process
			  (concat
			   "DELETEACL "
			   (prin1-to-string object)
			   (if identifier
			       (concat " " (prin1-to-string identifier))))))

(defun acap-myrights (process object)
  "Execute MYRIGHTS command on PROCESS."
  (acap-send-command-wait process
			  (concat
			   "MYRIGHTS "
			   (prin1-to-string object))))

(defun acap-listrights (process object identifier)
  "Execute LISTRIGHTS command on PROCESS."
  (acap-send-command-wait process
			  (concat
			   "LISTRIGHTS "
			   (prin1-to-string object)
			   " "
			   (prin1-to-string identifier))))

(defun acap-getquota (process dataset)
  "Execute GETQUOTA command on PROCESS."
  (acap-send-command-wait process
			  (concat
			   "GETQUOTA "
			   (prin1-to-string dataset))))

;;; response accessor.
(defun acap-response-ok-p (response)
  (assq 'done-ok response))

(defun acap-response-bye-p (response)
  (assq 'bye response))

(defun acap-response-bye-message (response)
  (nth 1 (cdr (assq 'bye response))))

(defun acap-response-cont-p (response)
  (assq 'cont response))

(defun acap-response-cont-string (response)
  (cdr (assq 'cont response)))

(defun acap-response-body (response)
  (cdr (or (assq 'done-ok response)
	   (assq 'done-no response)
	   (assq 'done-bad response))))

(defun acap-response-entries (response)
  (let (entries)
    (dolist (ent response)
      (if (eq (car ent) 'entry)
	  (setq entries (cons ent entries))))
    entries))

(defun acap-response-entry-entry (entry)
  (car (cdr entry)))

(defun acap-response-entry-return-data-list (entry)
  (nth 1 (cdr entry)))

(defun acap-response-return-data-list-get-value (name return-data-list)
  (nth 1 (assoc name return-data-list)))

(defun acap-response-listrights (response)
  (cdr (assq 'listrights response)))

;;; Send command, data.
(defun acap-response-clear (process)
  (with-current-buffer (process-buffer process)
    (setq acap-response nil)))

(defun acap-send-command-wait (process command)
  (acap-wait-for-response process (acap-send-command process command)))

(defun acap-send-data-wait (process string tag)
  (cond ((stringp string)
	 (acap-send-command-1 process string))
	((bufferp string)
	 (with-current-buffer string
	   (acap-response-clear process)
	   (acap-send-command-1 process (format "{%d}" (buffer-size)))
	   (if (acap-response-cont-p (acap-wait-for-response process tag))
	       (with-current-buffer string
		 (acap-response-clear process)
		 (process-send-region process (point-min)
				      (point-max))
		 (process-send-string process acap-client-eol)))
	   (acap-debug (concat (buffer-string) acap-client-eol)))))
  (acap-wait-for-response process tag))

(defun acap-send-command-1 (process cmdstr)
  (acap-debug (concat "<-" cmdstr acap-client-eol))
  (process-send-string process (concat cmdstr acap-client-eol)))

(defun acap-send-command (process command)
  (with-current-buffer (process-buffer process)
    (setq acap-response nil)
    (if (not (listp command)) (setq command (list command)))
    (let ((tag (setq acap-tag (1+ acap-tag)))
	  cmd cmdstr response)
      (setq cmdstr (concat (number-to-string acap-tag) " "))
      (while (setq cmd (pop command))
	(cond ((stringp cmd)
	       (setq cmdstr (concat cmdstr cmd)))
	      ((bufferp cmd)
	       (with-current-buffer cmd
		 (setq cmdstr (concat cmdstr (format "{%d}" (buffer-size)))))
	       (unwind-protect
		   (progn
		     (acap-send-command-1 process cmdstr)
		     (setq cmdstr nil
			   response (acap-wait-for-response process tag))
		     (if (not (acap-response-cont-p response))
			 (setq command nil) ;; abort command if no cont-req
		       (with-current-buffer cmd
			 (process-send-region process (point-min)
					      (point-max))
			 (process-send-string process acap-client-eol))))))
	      (t (error "Unknown command type"))))
      (when cmdstr
	(acap-send-command-1 process cmdstr))
      tag)))

(defun acap-wait-for-response (process tag)
  (with-current-buffer (process-buffer process)
    (while (and (not (acap-response-cont-p acap-response))
		(< acap-reached-tag tag))
      (when (acap-response-bye-p acap-response)
	(if acap-logging-out
	    (setq acap-response nil)
	  (error "%s"
		 (prog1 (acap-response-bye-message acap-response)
		   (setq acap-response nil)))))
      (or (and (not (memq (process-status process) '(open run)))
	       (sit-for 1))
	  (let ((len (/ (point-max) 1024))
		message-log-max)
	    (unless (< len 10)
	      (message "acap read: %dk" len))
	    (accept-process-output process 1))))
    (message "")
    acap-response))

;;; Sentinel, Filter.
(defun acap-sentinel (process string)
  (delete-process process))

(defun acap-find-next-line ()
  (when (re-search-forward (concat acap-server-eol "\\|{\\([0-9+]+\\)}"
				   acap-server-eol)
			   nil t)
    (if (match-string 1)
	(if (< (point-max) (+ (point) (string-to-number (match-string 1))))
	    nil
	  (goto-char (+ (point) (string-to-number (match-string 1))))
	  (acap-find-next-line))
      (point))))

(defun acap-arrival-filter (proc string)
  "ACAP process filter."
  (acap-debug string)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert string)
    (let (end)
      (goto-char (point-min))
      (while (setq end (acap-find-next-line))
	(save-restriction
	  (narrow-to-region (point-min) end)
	  (delete-char (- (length acap-server-eol)))
	  (goto-char (point-min))
	  (unwind-protect
	      (cond ((or (eq acap-state 'auth)
			 (eq acap-state 'initial)
			 (eq acap-state 'nonauth))
		     (acap-parse-response))
		    (t
		     (message "Unknown state %s in arrival filter"
			      acap-state)))
	    (delete-region (point-min) (point-max))))))))

;;; acap parser.
(defsubst acap-forward ()
  (or (eobp) (forward-char)))

(defsubst acap-parse-number ()
  (when (looking-at "[0-9]+")
    (prog1
	(string-to-number (match-string 0))
      (goto-char (match-end 0)))))

(defsubst acap-parse-literal ()
  (when (looking-at "{\\([0-9]+\\)}\r\n")
    (let ((pos (match-end 0))
	  (len (string-to-number (match-string 1))))
      (if (< (point-max) (+ pos len))
	  nil
	(goto-char (+ pos len))
	(buffer-substring pos (+ pos len))))))

(defun acap-parse-greeting ()
  (when (looking-at "* ACAP")
    (goto-char (match-end 0))
    (acap-forward)
    (let (capabilities)
      (while (eq (following-char) ?\()
	(push (read (current-buffer)) capabilities)
	(acap-forward))
      (nreverse capabilities))))

;; resp-body = ["(" resp-code ")" SP] quoted
(defun acap-parse-resp-body ()
  (let ((body (read (current-buffer))))
    (if (listp body) ; resp-code
	(list body (read (current-buffer)))
      (list nil body) ; no resp-code.
      )))

;;   string          = quoted / literal
;;
;;   quoted          = DQUOTE *QUOTED-CHAR DQUOTE
;;
;;   QUOTED-CHAR     = <any TEXT-CHAR except quoted-specials> /
;;                     "\" quoted-specials
;;
;;   quoted-specials = DQUOTE / "\"
;;
;;   TEXT-CHAR       = <any CHAR except CR and LF>

(defsubst acap-parse-string ()
  (cond ((eq (following-char) ?\")
	 (forward-char)
	 (let ((p (point)) (name ""))
	   (skip-chars-forward "^\"\\\\")
	   (setq name (buffer-substring p (point)))
	   (while (eq (following-char) ?\\)
	     (setq p (1+ (point)))
	     (forward-char 2)
	     (skip-chars-forward "^\"\\\\")
	     (setq name (concat name (buffer-substring p (point)))))
	   (forward-char)
	   name))
	((eq (following-char) ?{)
	 (acap-parse-literal))))

;;   nil             = "NIL"

(defsubst acap-parse-nil ()
  (if (looking-at "NIL")
      (goto-char (match-end 0))))

;; entry              = entry-name / entry-path
;; entry-name         = string-utf8
;;                        ;; entry name MUST NOT contain slash
;;                        ;; MUST NOT begin with "."
;; entry-path         = string-utf8
;;                        ;; slash-separated path to entry
;;                        ;; begins with slash

(defsubst acap-parse-quoted ()
  (if (eq (following-char) ?\")
      (read (current-buffer))))

(defun acap-parse-entry ()
  (acap-parse-quoted))

;; value              = string
(defun acap-parse-value ()
  (acap-parse-string))

;; value-list         = "(" [value *(SP value)] ")"
(defun acap-parse-value-list ()
  ;; same as acl.
  (when (eq (following-char) ?\()
    (let (values)
      (while (not (eq (following-char) ?\)))
	(acap-forward)
	(push (acap-parse-value) values))
      (acap-forward)
      (nreverse values))))

;;
;;   return-data-list   = return-data *(SP return-data)
;;
;;   return-data        = return-metadata / return-metalist /
;;                        return-attr-list

(defun acap-parse-return-data-list ()
  (let (rlist r)
    (setq rlist (list (acap-parse-return-metadata-or-return-metalist)))
    (acap-forward)
    (while (setq r (acap-parse-return-metadata-or-return-metalist))
      (setq rlist (nconc rlist (list r)))
      (acap-forward))
    rlist))

(defun acap-parse-return-metadata-or-return-metalist ()
  (or (acap-parse-string)
      (acap-parse-value-or-return-metalist)
      (and (acap-parse-nil) nil)))

(defun acap-parse-value-or-return-metalist ()
  (when (eq (following-char) ?\()
    (let (elems)
      (while (not (eq (following-char) ?\)))
	(acap-forward)
	(push (or (acap-parse-value)
		  (acap-parse-return-metalist))
	      elems))
      (acap-forward)
      (nreverse elems))))

;;   return-metalist    = "(" return-metadata *(SP return-metadata) ")"
;;                        ;; occurs when multiple metadata items requested
;;
(defun acap-parse-return-metalist ()
  (when (eq (following-char) ?\()
    (let (metadatas)
      (while (not (eq (following-char) ?\)))
	(acap-forward)
	(push (acap-parse-return-metadata) metadatas))
      (acap-forward)
      (nreverse metadatas))))

;;   return-metadata    = nil / string / value-list / acl
(defun acap-parse-return-metadata ()
  (or (acap-parse-string)
      (acap-parse-value-list)
      (and (acap-parse-nil) nil)
      ;; (acap-parse-acl) acl is same as value-list.
      ))

;;   return-attr-list   = "(" return-metalist *(SP return-metalist) ")"
;;                        ;; occurs when "*" in RETURN pattern on SEARCH
(defun acap-parse-return-attr-list ()
  (when (eq (following-char) ?\()
    (let (metalists)
      (while (not (eq (following-char) ?\)))
	(acap-forward)
	(push (acap-parse-return-metalist) metalists))
      (acap-forward)
      (nreverse metalists))))

(defun acap-parse-time ()
  (acap-parse-quoted))

;; quoted *(SP quoted)
(defun acap-parse-quoted-list ()
  (let (qlist q)
    (setq qlist (list (acap-parse-quoted)))
    (acap-forward)
    (while (setq q (acap-parse-quoted))
      (setq qlist (nconc qlist (list q)))
      (acap-forward))
    qlist))

(defun acap-parse-any ()
  (read (current-buffer)))

(defun acap-parse-extension-data ()
  (let (elist e)
    (setq elist (list (acap-parse-any)))
    (acap-forward)
    (while (setq e (acap-parse-any))
      (setq elist (nconc elist (list e)))
      (acap-forward))
    elist))

(defun acap-parse-response ()
  "Parse a ACAP command response."
  (let ((token (read (current-buffer)))
	tag)
    (setq
     acap-response
     (cons
      (cond
       ((eq token '+)
	(acap-forward)
	(cons 'cont (acap-parse-string)))
       ((eq token '*)
	;; untagged response.
	(case (prog1 (setq token (read (current-buffer)))
		(acap-forward))
	  (ADDTO (cons 'addto
		       (list (acap-parse-quoted)
			     (progn
			       (acap-forward)
			       (acap-parse-quoted))
			     (progn
			       (acap-forward)
			       (acap-parse-number))
			     (progn
			       (acap-forward)
			       (acap-parse-return-data-list)))))
	  (ALERT
;;;	   (cons 'alert (acap-parse-resp-body))
	   (message "%s" (nth 1 (acap-parse-resp-body))))
	  ((BYE Bye bye)
	   (cons 'bye (acap-parse-resp-body)))
	  (CHANGE (cons 'change
			(list (acap-parse-quoted)
			      (progn
				(acap-forward)
				(acap-parse-quoted))
			      (progn
				(acap-forward)
				(acap-parse-number))
			      (progn
				(acap-forward)
				(acap-parse-number))
			      (progn
				(acap-forward)
				(acap-parse-return-data-list)))))
	  (LANG (cons 'lang (list (acap-parse-quoted-list))))
	  ;; response-stat
	  (OK   (cons 'stat-ok (acap-parse-resp-body)))
	  (NO   (cons 'stat-no (acap-parse-resp-body)))
	  (BAD
;;;	   (cons 'stat-bad (acap-parse-resp-body))
	   ;; XXX cyrus-sml-acap does not return tagged bad response?
	   (error "%s" (nth 1 (acap-parse-resp-body))))))
       ((integerp token)
	;; tagged response.
	(setq tag token)
	(case (prog1 (setq token (read (current-buffer)))
		(acap-forward))
	  (DELETED   (cons 'deleted (acap-parse-quoted)))
	  ;; response-done
	  ((OK Ok ok) (prog1 (cons 'done-ok (acap-parse-resp-body))
			(setq acap-reached-tag tag)))
	  ((NO No no)   (prog1 (cons 'done-no (acap-parse-resp-body))
			  (setq acap-reached-tag tag)))
	  ((BAD Bad bad) (prog1 (cons 'done-bad (acap-parse-resp-body))
			   (setq acap-reached-tag tag)))
	  (ENTRY (cons 'entry
		       (list
			(acap-parse-entry)
			(progn (acap-forward)
			       (acap-parse-return-data-list)))))
	  (LISTRIGHTS (cons 'listrights
			    (acap-parse-quoted-list)))
	  (MODTIME    (cons 'modtime (acap-parse-time)))
	  (MYRIGHTS   (cons 'myrights (acap-parse-quoted)))
	  (QUOTA      (cons 'quota
			    (list (acap-parse-quoted)
				  (progn
				    (acap-forward)
				    (acap-parse-number))
				  (progn
				    (acap-forward)
				    (acap-parse-number))
				  (acap-parse-extension-data))))
	  (REFER      (cons 'refer (list (acap-parse-quoted)
					 (acap-parse-quoted))))
	  (REMOVEFROM (cons 'removefrom
			    (list (acap-parse-quoted)
				  (progn
				    (acap-forward)
				    (acap-parse-quoted))
				  (progn
				    (acap-forward)
				    (acap-parse-number)))))
	  ;; response-extend
	  (t ; extend-token
	   (cons 'extend (list token (acap-parse-extension-data))))))
       (t ; garbage
	(list 'garbage token)))
      acap-response))))

;;; Utilities.
(defun acap-flatten (l)
  "Flatten list-of-list."
  (unless (null l)
    (append
     (if (and (car l)
	      (listp (car l)))
	 (car l)
       (list (car l)))
     (acap-flatten (cdr l)))))

(defun acap-flatten-r (l)
  "Flatten list-of-list recursively."
  (cond
   ((null l) '())
   ((listp l)
    (append (acap-flatten (car l)) (acap-flatten (cdr l))))
   (t (list l))))

(defun acap-encode-time (time)
  (format-time-string "%Y%m%d%H%M%S" (current-time) t)) ; Universal time.

(defun acap-decode-time (acap-time)
  (when (string-match "^\\([0-9][0-9][0-9][0-9]\\)\\([0-1][0-9]\\)\\([0-3][0-9]\\)\\([0-2][0-9]\\)\\([0-5][0-9]\\)\\([0-5][0-9]\\)" acap-time)
    (encode-time (string-to-number (match-string 6 acap-time))
		 (string-to-number (match-string 5 acap-time))
		 (string-to-number (match-string 4 acap-time))
		 (string-to-number (match-string 3 acap-time))
		 (string-to-number (match-string 2 acap-time))
		 (string-to-number (match-string 1 acap-time))
		 t)))

(provide 'acap)

;;; acap.el ends here
