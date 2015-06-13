;;; elmo-vars.el --- User variables for ELMO. -*- coding: iso-2022-jp-unix -*-

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
(require 'poe)
(require 'path-util)

;; silence byte compiler
(eval-when-compile
  (defalias-maybe 'dynamic-link 'ignore)
  (defalias-maybe 'dynamic-call 'ignore))

;; bind colon keywords for old Emacsen.
(dont-compile
  (condition-case nil
      :symbol-for-testing-whether-colon-keyword-is-available-or-not
    (void-variable
     (let ((kwds '(:cc :date :extra :message-id :number :references :subject)))
       (while kwds
	 (set (car kwds) (car kwds))
	 (setq kwds (cdr kwds)))))))

(defgroup elmo nil
  "ELMO, Elisp Library for Message Orchestration."
  :tag "ELMO"
  :prefix "elmo-"
  :group 'news
  :group 'mail)

(defgroup elmo-setting nil
  "ELMO common settings."
  :prefix "elmo-"
  :group 'elmo)

(defcustom elmo-always-prefer-std11-parser nil
  "Always prefer std11 parser over regexp."
  :type 'boolean
  :group 'elmo)

(defcustom elmo-digest-flags '(unread)
  "Flags which are treated as `digest'."
  :type '(repeat (symbol :tag "flag"))
  :group 'elmo)

(defcustom elmo-preserved-flags '(forwarded answered cached new unread)
  "A list to define preserved flags.
Flags in this list can be searched by `any' flag condition.
If you want to treat a flag in this list as a `digest' flag,
you have to add it to `elmo-digest-flags'.
"
  :type '(repeat (symbol :tag "flag"))
  :group 'elmo)

;; Message Database

(defcustom elmo-msgdb-default-type 'standard
  "*Default type of Message Database for ELMO."
  :type '(radio (const legacy)
		(const standard))
  :group 'elmo
  :group 'elmo-setting)

(defcustom elmo-msgdb-convert-type 'auto
  "*MODB conversion type."
  :type '(radio (const sync)
		(const auto)
		(const :tag "No convert" nil))
  :group 'elmo
  :group 'elmo-setting)

(defcustom elmo-init-hook '(elmo-global-mark-migrate)
  "*A hook called when elmo is initialized."
  :type 'hook
  :group 'elmo)

(defvar elmo-msgdb-file-header-chop-length 2048
  "*Number of bytes to get header in one reading from file.")

(defcustom elmo-msgdb-directory "~/.elmo"
  "*ELMO Message Database path."
  :type 'directory
  :group 'elmo
  :group 'elmo-setting)
(defvar elmo-passwd-alist-file-name "passwd"
  "*ELMO Password filename.")
(defcustom elmo-passwd-life-time nil
  "*Duration of ELMO Password in seconds.  nil means infinity."
  :type '(choice (const :tag "Infinity" nil)
		 number)
  :group 'elmo
  :group 'elmo-setting)

(defvar elmo-warning-threshold 30000
  "*Display warning when the bytes of message exceeds this value.")

(defvar elmo-msg-appended-hook nil
  "A hook called when message is appended to database.")
(defvar elmo-msg-deleted-hook nil
  "A hook called when message is deleted from database.")
(defvar elmo-nntp-post-pre-hook nil
  "A hook called just before the nntp posting.")

;;; IMAP4
(defcustom elmo-imap4-set-seen-flag-explicitly
  nil
  "*Set Seen flag explicitly (avoid bug in Google Mail)"
  :type 'boolean
  :group 'elmo
  :group 'elmo-setting)

(defcustom elmo-imap4-default-server "localhost"
  "*Default IMAP4 server."
  :type 'string
  :group 'elmo
  :group 'elmo-setting)

(defcustom elmo-imap4-default-authenticate-type 'login
  "*Default Authentication type for IMAP4."
  :type '(radio (const :tag "encoded password transmission (login)" login)
		(const :tag "CRAM-MD5 authentication (cram-md5)" cram-md5)
		(const :tag "DIGEST-MD5 authentication (digest-md5)" digest-md5)
		(const :tag "plain password transmission (clear)" clear)
		(const :tag "NTLM authentication (ntlm)" ntlm)
		(function :tag "Other"))
  :group 'elmo)

(defcustom elmo-imap4-default-user (or (getenv "USER")
				       (getenv "LOGNAME")
				       (user-login-name))
  "*Default username for IMAP4."
  :type 'string
  :group 'elmo
  :group 'elmo-setting)

(defcustom elmo-imap4-default-port 143
  "*Default Port number of IMAP."
  :type 'integer
  :group 'elmo
  :group 'elmo-setting)

(defcustom elmo-imap4-default-stream-type nil
  "*Default stream type for IMAP4.
Any symbol value of `elmo-network-stream-type-alist' or
`elmo-imap4-stream-type-alist'."
  :type 'symbol
  :group 'elmo)

(defvar elmo-imap4-stream-type-alist nil
  "*Stream bindings for IMAP4.
This is taken precedence over `elmo-network-stream-type-alist'.")

;;; NNTP

;; User options
(defcustom elmo-nntp-default-server "localhost"
  "*Default NNTP server."
  :type 'string
  :group 'elmo
  :group 'elmo-setting)

(defcustom elmo-nntp-default-user nil
  "*Default User of NNTP.  nil means no user authentication."
  :type '(choice (const nil)
		 string)
  :group 'elmo
  :group 'elmo-setting)

(defcustom elmo-nntp-default-port 119
  "*Default Port number of NNTP."
  :type 'integer
  :group 'elmo
  :group 'elmo-setting)

(defcustom elmo-nntp-default-stream-type nil
  "*Default stream type for NNTP.
Any symbol value of `elmo-network-stream-type-alist' or
`elmo-nntp-stream-type-alist'."
  :type 'symbol
  :group 'elmo)

(defvar elmo-nntp-stream-type-alist nil
  "*Stream bindings for NNTP.
This is taken precedence over `elmo-network-stream-type-alist'.")

;;; POP3

;; POP3
(defcustom elmo-pop3-default-user (or (getenv "USER")
				      (getenv "LOGNAME")
				      (user-login-name))
  "*Default username for POP3."
  :type 'string
  :group 'elmo
  :group 'elmo-setting)

(defcustom elmo-pop3-default-server  "localhost"
  "*Default POP3 server."
  :type 'string
  :group 'elmo
  :group 'elmo-setting)

(defcustom elmo-pop3-default-authenticate-type 'user
  "*Default Authentication type for POP3."
  :type '(radio (const :tag "plain password transmission (user)" user)
		(const :tag "APOP authentication (apop)" apop)
		(const :tag "CRAM-MD5 authentication (cram-md5)" cram-md5)
		(const :tag "DIGEST-MD5 authentication (digest-md5)" digest-md5)
		(const :tag "NTLM authentication (ntlm)" ntlm)
		(function :tag "Other"))
  :group 'elmo)

(defcustom elmo-pop3-default-port 110
  "*Default POP3 port."
  :type 'integer
  :group 'elmo
  :group 'elmo-setting)

(defcustom elmo-pop3-default-stream-type nil
  "*Default stream type for POP3.
Any symbol value of `elmo-network-stream-type-alist' or
`elmo-pop3-stream-type-alist'."
  :type 'symbol
  :group 'elmo)

(defvar elmo-pop3-stream-type-alist nil
  "*Stream bindings for POP3.
This is taken precedence over `elmo-network-stream-type-alist'.")

(defcustom elmo-lang "ja"
  "Language for displayed messages."
  :type 'string
  :group 'elmo-setting)

(defvar elmo-mime-charset 'iso-2022-jp)

(defvar elmo-msgdb-mark-filename "mark"
  "Mark database.")
(defvar elmo-msgdb-overview-filename "overview"
  "Overview database.")
(defvar elmo-msgdb-number-filename "number"
  "Message number <=> Message-ID database.")
(defvar elmo-msgdb-location-filename "location"
  "Message number <=> Actual location symbol.")
(defvar elmo-msgdb-seen-filename "seen"
  "Seen message list for append.")
(defvar elmo-msgdb-killed-filename "killed"
  "Deleted messages... contains elmo-killed-msgs-list.")
(defvar elmo-msgdb-validity-filename "validity")
(defvar elmo-msgdb-flist-filename "flist"
  "Folder list cache (for access folder).")
(defvar elmo-msgdb-finfo-filename "finfo"
  "Folder information cache...list of '(filename . '(new unread all)).")
(defvar elmo-msgdb-lock-list-filename "lock"
  "Locked messages...list of message-id.
For disconnected operations.")
(defvar elmo-lost+found-folder "+lost+found"
  "Lost and found.")
(defvar elmo-crosspost-alist-filename "crosspost-alist"
  "Alist of crosspost messages.")

(defvar elmo-use-server-diff t
  "Non-nil forces to get unread message information on server.")

(defvar elmo-strict-diff-folder-list nil
  "List of regexps of folder name which should be checked its diff strictly.")

(defcustom elmo-msgdb-prefer-in-reply-to-for-parent nil
  "*Non-nil to prefer In-Reply-To header for finding parent message on thread,
rather than References header."
  :type 'boolean
  :group 'elmo
  :group 'elmo-setting)

(defcustom elmo-msgdb-extra-fields nil
  "Extra fields for msgdb."
  :type '(repeat string)
  :group 'elmo
  :group 'elmo-setting)

(defcustom elmo-enable-disconnected-operation t
  "*Non-nil enables disconnected operations."
  :type 'boolean
  :group 'elmo
  :group 'elmo-setting)

(defvar elmo-auto-change-plugged 600
  "*Time to expire change plugged state automatically, as the number of seconds.
Don't change plugged state automatically if nil.")
(defvar elmo-plugged-condition 'one
  "*The condition for `elmo-plugged' becomes on.
If `all', when all ports are on.  If `one', when even one port is on.
If `independent', independent port plugged.
If function, return value of function.")

(defvar elmo-plug-on-servers nil)

(defvar elmo-plug-on-exclude-servers
  (list "localhost"
	(system-name)
	(and (string-match "[^.]+" (system-name))
	     (substring (system-name) 0 (match-end 0)))))

(defvar elmo-plugged-alist nil)

(defvar elmo-dop-flush-confirm t
  "*Flush disconnected operations queue with confirmation.")

(defvar elmo-path-sep "/"
  "*Path separator.")
(defvar elmo-plugged t)

(defvar elmo-no-subject "(No Subject in original.)"
  "*A string used when no subject field exists.")
(defvar elmo-no-from "nobody@nowhere?"
  "*A string used when no from field exists.")

;; database dynamic linking
(defvar elmo-database-dl-module
  (expand-file-name "database.so" exec-directory))

(defvar elmo-database-dl-handle
  (if (and (fboundp 'dynamic-link)
	   (file-exists-p
	    elmo-database-dl-module))
      (if (fboundp 'open-database)
	  t ;;
	(dynamic-link elmo-database-dl-module))))

(if (and elmo-database-dl-handle
	 (integerp elmo-database-dl-handle))
    (dynamic-call "emacs_database_init" elmo-database-dl-handle))

(defvar elmo-use-database (or (featurep 'dbm)
			      (featurep 'gnudbm)
			      (featurep 'berkdb)
			      (featurep 'berkeley-db)
			      ;; static/dl-database
			      (fboundp 'open-database)))

(defvar elmo-date-match t
  "Date match is available or not.")

(defvar elmo-network-stream-type-alist
  `(("!" ssl ,@(cond
		((and (fboundp 'gnutls-available-p)
		      (gnutls-available-p))
		 '(gnutls open-gnutls-stream))
		((module-installed-p 'tls)
		 '(tls    open-tls-stream))
		(t
		 '(ssl    open-ssl-stream))))
    ("!!"     starttls  starttls starttls-open-stream)
    ("!socks" socks     socks    socks-open-network-stream)
    ("!direct" direct   nil   open-network-stream))
  "An alist of (SPEC-STRING SYMBOL FEATURE OPEN-STREAM-FUNCTION).
SPEC-STRING is a string for stream-type spec (it must start with '!').
SYMBOL is a symbol which indicates the name of the stream type.
SYMBOL should be identical in this alist.
FEATURE is a symbol of the feature for OPEN-STREAM-FUNCTION.
OPEN-STREAM-FUNCTION is a function to open network stream.
Arguments for this function are NAME, BUFFER, HOST and SERVICE.")

(defvar elmo-folder-info-hashtb nil
  "Array of folder database information '(max length new unread).")

(defvar elmo-crosspost-message-alist nil
  "List of crosspost message.")

(defvar elmo-cache-expire-default-method "size"
  "Default expiration method.")

(defvar elmo-cache-expire-default-size 30000
  "Cache expiration disk size (Kilo bytes).  This must be float value.")

(defvar elmo-cache-expire-default-age 50
  "Cache expiration age (days).")

(defvar elmo-cache-directory (expand-file-name "cache" elmo-msgdb-directory)
  "Directory name for cache storage.")

(defvar elmo-pack-number-check-strict t
  "Pack number strictly.")

(defvar elmo-have-link-count
  (not
   ;; OS/2 (EMX) and Cygwin always return the link count "1" :-(
   (or (memq system-type '(OS/2 emx cygwin))
       ;; Meadow seems to have pseudo link count.(suggestion by S.YAMAGUCHI)
       (and (eq system-type 'windows-nt) (not (featurep 'meadow)))))
  "Your file system has link count, or not.")

(defvar elmo-use-hardlink
  ;; Any Emacsen may have add-name-to-file(), because loadup.el
  ;; requires it. :-p Check make-symbolic-link() instead.
  (fboundp 'make-symbolic-link)
  "Hardlink is available on your file system, or not.")

(defvar elmo-weekday-name-en '["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"])
(defvar elmo-weekday-name-ja '["日" "月" "火" "水" "木" "金" "土"])
(defvar elmo-weekday-name-fr '["Dim" "Lun" "Mar" "Mer" "Jeu" "Ven" "Sam"])
(defvar elmo-weekday-name-de '["Son" "Mon" "Die" "Mit" "Don" "Fre" "Sam"])

(defvar elmo-filename-replace-string-alist
  '((":"  . " c")
    ("*"  . " a")
    ("?"  . " q")
    ("<"  . " l")
    (">"  . " g")
    ("\"" . " d")
    ("|"  . " p")
    ("/"  . " s")
    ("\\" . " b")))

(defvar elmo-hash-minimum-size 1023
  "Minimum size of hash table.")

(defvar elmo-hash-maximum-size 4095
  "Maximum size of hash table.")

(defvar elmo-use-decoded-cache (featurep 'xemacs)
  "Use cache of decoded mime charset string.")

(defvar elmo-inhibit-number-mapping nil
  "Global switch to inhibit number mapping (e.g. Inhibit UIDL on POP3).")

(defvar elmo-dop-queue nil
  "Global variable for storing disconnected operation queues.")

(defcustom elmo-mime-display-as-is-coding-system (if (boundp 'MULE)
						     '*autoconv* 'undecided)
  "*Coding system used when message is displayed as is."
  :type 'symbol
  :group 'elmo)

(defcustom elmo-mailing-list-name-spec-list
  '(x-ml-name
    (x-sequence "^\\([^ ]+\\)")
    (subject "^\\s(\\(\\S)+\\)[ :][0-9]+\\s)[ \t]*")
    (list-post "<mailto:\\(.+\\)@")
    (list-id "<\\([^.]+\\)\\." "^\\([^.]+\\)\\.")
    (mailing-list ("\\(^\\|; \\)contact \\([^@]+\\)-[^-@]+@" . 2))
    (return-path "^<\\([^@>]+\\)-return-[0-9]+-")
    (delivered-to "^mailing list \\([^@]+\\)@"))
  "*List of spec to extract mailing list name from field value."
  :type '(repeat
	  (choice (symbol :tag "Field Name")
		  (list (symbol :tag "Field Name")
			(repeat
			 :inline symbol
			 (choice regexp
				 (cons regexp
				       (integer :tag "Match Index")))))))
  :group 'elmo)

(defcustom elmo-mailing-list-count-spec-list
  '(x-mail-count
    x-ml-count
    (x-sequence "^[^ ]+ \\([^ ]+\\)")
    (subject "^\\s(\\S)+[ :]\\([0-9]+\\)\\s)[ \t]*")
    (return-path "^<[^@>]+-return-\\([0-9]+\\)-"))
  "*List of spec to extract mailing list count from field value."
  :type '(repeat
	  (choice (symbol :tag "Field Name")
		  (list (symbol :tag "Field Name")
			(repeat
			 :inline symbol
			 (choice regexp
				 (cons regexp
				       (integer :tag "Match Index")))))))
  :group 'elmo)

(require 'product)
(product-provide (provide 'elmo-vars) (require 'elmo-version))

;;; elmo-vars.el ends here
