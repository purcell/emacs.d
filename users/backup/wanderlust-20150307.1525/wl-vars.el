;;; wl-vars.el --- Variable definitions for Wanderlust. -*-coding: iso-2022-jp-unix;-*-

;; Copyright (C) 1998,1999,2000,2001 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 1998,1999,2000,2001 Masahiro MURATA <muse@ba2.so-net.ne.jp>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Masahiro MURATA <muse@ba2.so-net.ne.jp>
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
;;

;;; Code:
;;

(require 'elmo-vars)
(require 'elmo-util)
(require 'elmo-msgdb)
(require 'custom)

;;; Customizable Variables

(defgroup wl nil
  "Wanderlust, a news and mail reading software."
  :tag "Wanderlust"
  :link `(custom-manual
	  ,(if (and (boundp 'current-language-environment)
		    (string-equal "Japanese"
				  (symbol-value 'current-language-environment)))
	       "(wl-ja)Top"
	     "(wl)Top"))
  :group 'news
  :group 'mail)

(defgroup wl-pref nil
  "Wanderlust, Preferences."
  :prefix "wl-"
  :group 'wl)

(defgroup wl-folder nil
  "Wanderlust, folder buffer."
  :prefix "wl-"
  :group 'wl)

(defgroup wl-summary nil
  "Wanderlust, summary buffer."
  :prefix "wl-"
  :group 'wl)

(defgroup wl-summary-marks nil
  "Wanderlust, marks used in summary buffers."
  :prefix "wl-summary-"
  :group 'wl-summary)

(defgroup wl-expire nil
  "Wanderlust, Expiring and archiving."
  :prefix "wl-"
  :group 'wl)

(defgroup wl-score nil
  "Wanderlust, Score file handling."
  :prefix "wl-"
  :group 'wl)

(defgroup wl-highlight nil
  "Wanderlust, Highlights."
  :prefix "wl-"
  :group 'wl)

(defgroup wl-draft nil
  "Wanderlust, draft mode."
  :prefix "wl-"
  :group 'wl)

(defgroup wl-setting nil
  "Wanderlust common settings."
  :prefix "wl-"
  :group 'wl)

;;; Emacsen
(defconst wl-on-xemacs (featurep 'xemacs))

(defconst wl-on-emacs21 (and (not wl-on-xemacs)
			     (>= emacs-major-version 21)))

(defconst wl-on-mule (featurep 'mule))

(defconst wl-on-mule3
  (and wl-on-mule (or wl-on-xemacs
		      (> emacs-major-version 19))))

(defconst wl-on-nemacs nil) ; backward compatibility.

(eval-when-compile
  (defun-maybe locate-data-directory (a)))

(defvar wl-cs-noconv
  (cond (wl-on-mule3 'binary)
	(wl-on-mule  '*noconv*)
	(t           nil)))

(defvar wl-cs-autoconv
  (cond (wl-on-mule3 'undecided)
	(wl-on-mule  '*autoconv*)
	(t           nil)))

(defvar wl-cs-local
  (cond (wl-on-mule3  'junet)
	(wl-on-mule   '*junet*)
	(t           nil)))

(defvar wl-cs-cache wl-cs-local)

(defcustom wl-from (and user-mail-address
			(concat (and (user-full-name)
				     (concat (elmo-address-quote-specials
					      (user-full-name))
					     " "))
				"<" user-mail-address ">"))
  "*From string used in draft."
  :type  'string
  :group 'wl
  :group 'wl-setting)

(defcustom wl-user-mail-address-list nil
  "*A list of user's mail addresses.
This list is used to judge whether an address is user's or not.
You should set this variable if you use multiple e-mail addresses.
If you don't have multiple e-mail addresses, you don't have to set this.
NOTE: Non-nil value of `wl-user-mail-address-regexp' supersede this."
  :type '(repeat string)
  :group 'wl
  :group 'wl-setting)

(defcustom wl-user-mail-address-regexp nil
  "*A regexp for user's mail addresses.
Supersede `wl-user-mail-address-list'."
  :type '(choice (const :tag "Use wl-user-mail-address-list" nil)
		 string)
  :group 'wl
  :group 'wl-setting)

(defcustom wl-organization (getenv "ORGANIZATION")
  "Organization name."
  :type '(choice (const :tag "none" nil)
		 string)
  :group 'wl
  :group 'wl-setting)

(defcustom wl-temporary-file-directory "~/tmp/"
  "*Default temporary directory to save message, part."
  :type 'directory
  :group 'wl)

(defcustom wl-icon-directory (or
                              (and (fboundp 'locate-data-directory)
                                   (locate-data-directory "wl"))
                              (let ((icons (expand-file-name "wl/icons/"
							      data-directory)))
                                (if (file-directory-p icons)
                                    icons))
                              (if load-file-name
                                  (let ((icons (expand-file-name
                                                "icons"
                                                (file-name-directory load-file-name))))
                                    (if (file-directory-p icons)
                                        icons))))
  "*Directory to load the icon files from, or nil if none."
  :type '(choice (const :tag "none" nil)
		 string)
  :group 'wl)

(defcustom wl-summary-default-view 'thread
  "Default status of summary view, thread or sequential view."
  :type '(choice (const :tag "Thread" thread)
		 (const :tag "Sequential" sequence))
  :group 'wl-summary)

(defcustom wl-summary-default-view-alist nil
  "An alist of regexp for folder name and summary default view.
If no match, `wl-summary-default-view' is used."
  :type '(repeat (cons (regexp :tag "Folder Regexp")
		       (choice (const :tag "Thread" thread)
			       (const :tag "Sequential" sequence))))
  :group 'wl-summary)

(defvar wl-summary-mode-line-format-spec-alist
  '((?f (if (memq 'modeline wl-use-folder-petname)
	    (wl-folder-get-petname (elmo-folder-name-internal
				    wl-summary-buffer-elmo-folder))
	  (elmo-folder-name-internal wl-summary-buffer-elmo-folder)))
    (?t (if (eq wl-summary-buffer-view 'thread) "T" "S"))
    (?m (upcase (symbol-name wl-summary-buffer-display-mime-mode)))
    (?n wl-summary-buffer-new-count)
    (?u wl-summary-buffer-unread-count)
    (?a (length wl-summary-buffer-number-list)))
  "An alist of format specifications that can appear in summary mode-lines.
Each element is a list of following:
\(SPEC STRING-EXP)
SPEC is a character for format specification.
STRING-EXP is an expression to get string to insert.")

(defcustom wl-summary-mode-line-format "Wanderlust: %f {%t}(%n/%u/%a)"
  "*A format string for summary mode-line of Wanderlust.
It may include any of the following format specifications
which are replaced by the given information:

%f The folder name.
%t The thread status of the summary ('T' for thread, 'S' for sequential).
%m The mime analysis status of the summary ('MIME' for MIME ON)
%n The number of new messages.
%u The number of unread messages (includes new messages).
%a The number of all messages."
  :group 'wl-summary
  :type 'string)

(defvar wl-summary-line-format-spec-alist
  '((?Y (wl-summary-line-year))
    (?M (wl-summary-line-month))
    (?D (wl-summary-line-day))
    (?W (wl-summary-line-day-of-week))
    (?h (wl-summary-line-hour))
    (?m (wl-summary-line-minute))
    (?\[ (if wl-thr-linked "<" "["))
    (?\] (if wl-thr-linked ">" "]"))
    (?t (or wl-thr-indent-string ""))
    (?s (wl-summary-line-subject))
    (?S (wl-summary-line-size))
    (?C (if wl-thr-children-number
	    (concat "[+" (number-to-string wl-thr-children-number) "] ")
	  (if wl-parent-message-entity
	      (if wl-thr-linked ">>" ">")
	    "")))
    (?~ (if (zerop (length wl-line-string)) "" " "))
    (?c (if wl-thr-children-number
	    (concat "+" (number-to-string wl-thr-children-number) ":")
	  ""))
    (?f (wl-summary-line-from))
    (?# (wl-summary-line-list-info))
    (?l (wl-summary-line-list-count))
    (?T (or wl-temp-mark " "))
    (?P (or wl-persistent-mark " "))
    (?n (wl-summary-line-number))
    (?@ (wl-summary-line-attached)))
  "An alist of format specifications that can appear in summary lines.
Each element is a list of following:
\(SPEC STRING-EXP)
SPEC is a character for format specification.
STRING-EXP is an expression to get string to insert.")

(defcustom wl-summary-line-format "%n%T%P%M/%D(%W)%h:%m %t%[%17(%c %f%) %] %s"
  "*A default format string for summary line of Wanderlust.
It may include any of the following format specifications
which are replaced by the given information:

%n The number of the message.
   The width is decided using `wl-summary-default-number-column' and
   `wl-summary-number-column-alist'.
%T The temporal mark (*, D, o, O).
%P The persistent mark (status of the message).
%Y The year of the date field of the message (zero padded).
%M The month of the date field of the message (zero padded).
%D The day of the date field of the message (zero padded).
%W The weekday name of the date field of the message (zero padded).
%h The hour of the date field of the message (zero padded).
%l The number in the mailing list.
%m The minute of the date field of the message (zero padded).
%[ An open bracket.  If the message thread is linked,
   it is replaced with '<'.
%] A close bracket.  If the message thread is linked,
   it is replaced with '>'.
%c The children number of the closed message thread.
   Children number is printed like '+??:'.
%C The children number of the closed message thread.
   Children number is printed like '[+??] '.
   If the message is opened, '>' or '>>' (linked) is displayed.
%f The from: field string of the message.
%s The subject: field string of the message.
%S The size of the message (if available).
%t The branch of the thread.
%@ `@' only if the first MIME part is multipart/mixed.
%~ If the previous spec is not zero-length, replaced with ' '.
%#  mailing list information (`(' ML-name [ ` ' ML-number ] `)')

If the format string contains the specifiers %( and %), the text between
them will have the specified number of columns.

See also variable `wl-summary-width'."
  :group 'wl-summary
  :type 'string)

(defcustom wl-folder-summary-line-format-alist nil
  "An alist of folder name and a summary line format.
If no match, `wl-summary-line-format' is used.
e.x.
      '((\"^%\" . \"%n%T%P%M/%D(%W)%h:%m %t%[%14(%c %f%) %](%S) %s\")
	(\"^@2ch\" . \"%n%T%P%M%/%D/%h:%m %t[%9(%c %f%) ]%s\")))"
  :type '(repeat (cons (regexp :tag "Folder Regexp")
		       (string :tag "line format")))
  :group 'wl-summary)

(defcustom wl-summary-check-line-format t
  "*Check summary line format change if non-nil.
When summary line format is changed, current summary cache is discarded.
It is highly recommended to set this value to t."
  :type 'boolean
  :group 'wl-summary)

(defcustom wl-summary-line-format-file ".wl-summary-line-format"
  "*Cache file for summary line format."
  :type 'file
  :group 'wl-summary)

(defcustom wl-summary-from-function 'wl-summary-default-from
  "*A function for displaying sender (From: field) information."
  :type 'function
  :group 'wl-summary)

(defcustom wl-summary-subject-function 'wl-summary-default-subject
  "*A function for displaying subject."
  :type 'function
  :group 'wl-summary)

(defcustom wl-summary-subject-filter-function 'wl-summary-default-subject-filter
  "*A filter function for comparing subjects."
  :type 'function
  :group 'wl-summary)

(defcustom wl-summary-search-parent-by-subject-regexp "^[ \t]*\\(\\[[^:]+[,: ][0-9]+\\]\\)?[ \t]*re[\\^[:> ]"
  "*If message does not have in-reply-to field nor references field and
subject matches this regexp, search parent message by subject matching.
If nil, never search search parent by subject."
  :type '(choice string
		 (const :tag "Don't search parent" nil))
  :group 'wl-summary)

;;; Mark & Action
(defcustom wl-summary-mark-action-list
  '(("*"
     target-mark
     nil
     wl-summary-register-target-mark
     nil
     wl-highlight-summary-temp-face
     "Put target mark.")
    ("d"
     dispose
     nil
     wl-summary-register-temp-mark
     wl-summary-exec-action-dispose
     wl-highlight-summary-disposed-face
     "Dispose messages according to `wl-dispose-folder-alist'.")
    ("D"
     delete
     nil
     wl-summary-register-temp-mark
     wl-summary-exec-action-delete
     wl-highlight-summary-deleted-face
     "Delete messages immediately.")
    ("o"
     refile
     wl-summary-get-refile-destination
     wl-summary-set-action-refile
     wl-summary-exec-action-refile
     wl-highlight-summary-refiled-face
     "Refile messages to the other folder.")
    ("O"
     copy
     wl-summary-get-copy-destination
     wl-summary-register-temp-mark
     wl-summary-exec-action-copy
     wl-highlight-summary-copied-face
     "Copy messages to the other folder.")
    ("i"
     prefetch
     nil
     wl-summary-register-temp-mark
     wl-summary-exec-action-prefetch
     wl-highlight-summary-prefetch-face
     "Prefetch messages.")
    ("~"
     resend
     wl-summary-get-resend-address
     wl-summary-register-temp-mark
     wl-summary-exec-action-resend
     wl-highlight-summary-resend-face
     "Resend messages."))
  "A variable to define Mark & Action.
Each element of the list should be a list of
\(MARK
  SYMBOL
  ARGUMENT-FUNCTION
  SET-MARK-FUNCTION
  EXEC-FUNCTION
  FACE
  DOC-STRING)

MARK is a temporal mark string to define.
SYMBOL is an action name to define.
ARGUMENT-FUNCTION is a function called to set the argument data for
SET-MARK-FUNCTION.
Its argument is (ACTION NUMBER).
ACTION is same as the SYMBOL.
NUMBER is the message number to determine the argument data.
SET-MARK-FUNCTION is a function called to set the mark.
Its argument is (NUMBER MARK DATA).
NUMBER is the target message number.
MARK is the temporary mark string.
DATA is given by ARGUMENT-FUNCTION.
EXEC-FUNCTION is a function called to execute the action.
Its argument is a list of MARK-INFO.
MARK-INFO is a list of (NUMBER MARK DATA).
FACE is a face for highlighting."
  :type '(repeat (list
		  (string :tag "Temporary mark")
		  (symbol :tag "Action name")
		  (symbol :tag "Argument function")
		  (symbol :tag "Set mark function")
		  (symbol :tag "Exec function")
		  (symbol :tag "Face symbol")
		  (string :tag "Document string")))
  :group 'wl-summary)

;; Important folders
(defcustom wl-default-folder "%inbox"
  "*Default folder used in `wl-summary-goto-folder'."
  :type 'string
  :group 'wl)
(defcustom wl-draft-folder "+draft"
  "*Draft folder"
  :type 'string
  :group 'wl)
(defcustom wl-trash-folder "+trash"
  "*Trash folder"
  :type 'string
  :group 'wl
  :group 'wl-setting)
(defcustom wl-queue-folder "+queue"
  "*Queue folder"
  :type 'string
  :group 'wl)

(defcustom wl-default-spec "%"
  "*Default prefix for folder name initially added in minibuffer"
  :type 'string
  :group 'wl)

(defcustom wl-insert-mail-followup-to nil
  "*Insert Mail-Followup-To: field if non-nil."
  :type 'boolean
  :group 'wl-draft)

(defcustom wl-insert-mail-reply-to nil
  "*Insert Mail-Reply-To: field if non-nil."
  :type 'boolean
  :group 'wl-draft)

(defcustom wl-insert-message-id t
  "*Insert Message-ID: field if non-nil."
  :type 'boolean
  :group 'wl-draft)

(defcustom wl-auto-insert-x-face t
  "*Insert X-Face: field automatically."
  :type 'boolean
  :group 'wl-draft)

(defcustom wl-x-face-file "~/.xface"
  "*X-Face field is inserted using its contents.
If file exists and `wl-auto-insert-x-face' is non-nil."
  :type 'file
  :group 'wl-draft)

(defcustom wl-draft-write-file-function 'wl-draft-save
  "Save function for draft message."
  :type 'function
  :group 'wl-draft)

(defcustom wl-subscribed-mailing-list nil
  "*Subscribed mailing list.
You had better set this variable if you set 'wl-insert-mail-followup-to' as t."
  :type '(repeat string)
  :group 'wl-pref
  :group 'wl-setting)

(defcustom wl-demo t
  "*Display demo at start time."
  :type 'boolean
  :group 'wl-pref)

(defcustom wl-demo-icon-name-alist
  '(((string-match "^... Dec \\([ 01][0-9]\\|2[0-5]\\)" (current-time-string))
     .
     (concat "wl-" (wl-version-status) "-xmas-logo"))
    (t
     .
     (concat "wl-" (wl-version-status) "-logo")))
  "An alist to determine the basename of the logo file."
  :type '(repeat (cons (symbol :tag "condition")
		       (symbol :tag "file name")))
  :group 'wl-pref)

(defcustom wl-demo-image-filter-alist nil
  "An alist of image type and filter function."
  :type '(repeat (cons symbol function))
  :group 'wl-pref)

(defcustom wl-envelope-from nil
  "*Envelope From used in SMTP.
If nil, `wl-from' is used."
  :type '(choice (const :tag "Same as 'From' field." nil)
		 string)
  :group 'wl
  :group 'wl-setting)

(defcustom wl-draft-additional-header-alist nil
  "*Additional headers in the draft."
  :type '(repeat (cons (symbol :tag "Field Name")
		       (choice (string :tag "String")
			       (function :tag "Function"))))
  :group 'wl-draft)

(defcustom wl-draft-add-in-reply-to t
  "*If non-nil, message-id of the cited message is inserted to the
in-reply-to field of the current draft.
Note: default value follows RFC2822."
  :type 'boolean
  :group 'wl-draft)

(defcustom wl-draft-add-references nil
  "*If non-nil, message-id of the cited message is inserted to the
references field of the current draft.
Note: default value follows RFC2822."
  :type 'boolean
  :group 'wl-draft)

(defcustom wl-draft-cite-function 'wl-default-draft-cite
  "*A function for citation."
  :type 'function
  :group 'wl-draft)

(defcustom wl-default-draft-cite-date-format-string t
  "*Format string to use for first line of citation in `wl-default-draft-cite'.
The value is passed to `format-time-string'.
When non-string and non-nil, call `wl-make-date-string' function.
When nil, use original message's Date: field as is."
  :type '(choice (string :tag "Format string")
		 (const :tag "Call wl-make-date-string function" t)
		 (const :tag "Use original Date: field" nil))
  :group 'wl-draft)

(defcustom wl-default-draft-cite-no-date-string "Some time ago"
  "*String to use for first line of citation in `wl-default-draft-cite'.
The value is used as is when original message's Date: field is not found."
  :type 'string
  :group 'wl-draft)

(defcustom wl-default-draft-cite-no-author-string "you"
  "*String to use for second line of citation in `wl-default-draft-cite'.
The value is used as is when original message's From: field is not found."
  :type 'string
  :group 'wl-draft)

(defcustom wl-default-draft-cite-time-locale "C"
  "*Override `system-time-locale' while making time string of citaion header.
XEmacs is not affected."
  :type '(choice (string :tag "Specify locale")
		 (const :tag "Do not override" nil))
  :group 'wl-draft)

(defcustom wl-default-draft-cite-header-format-string "%s,\n%s wrote:\n"
  "*Format string to generate citation header.
The value is passed to `format' function with two string, date and author respectively.  Should be terminated with LF."
  :type 'string
  :group 'wl-draft)

(defcustom wl-default-draft-cite-decorate-author t
  "*If non-nil, the author of cited message is arranged by
`wl-summary-from-func-internal' in `wl-default-draft-cite'."
  :type 'boolean
  :group 'wl-draft)

(defcustom wl-draft-preview-process-pgp nil
  "*When non-nil, PGP processing (encryption, sign) will be done when preview."
  :type 'boolean
  :group 'wl-draft)

(defcustom wl-smtp-connection-type nil
  "*SMTP connection type.
If nil, default smtp connection type is used."
  :type '(choice (const :tag "default" nil)
		 (const :tag "Use STARTTLS" starttls)
		 (const :tag "SMTP over SSL" ssl)
		 symbol)
  :group 'wl)

(defcustom wl-smtp-posting-user nil
  "*SMTP authentication user."
  :type '(choice (const :tag "none" nil)
		 string)
  :group 'wl
  :group 'wl-setting)

(defcustom wl-smtp-posting-server nil
  "*SMTP server name to send mail (wl-draft-send-mail-with-smtp)."
  :type '(choice (const :tag "none" nil)
		 string)
  :group 'wl
  :group 'wl-setting)

(defcustom wl-smtp-posting-port nil
  "*SMTP port number in `wl-smtp-posting-server'.
If nil, default SMTP port number(25) is used."
  :type '(choice (const :tag "Default (25)" nil)
		 integer)
  :group 'wl
  :group 'wl-setting)

(defcustom wl-smtp-authenticate-type nil
  "*SMTP Authentication type.
If nil, don't authenticate."
  :type '(choice (const :tag "none" nil)
		 (const :tag "PLAIN" "plain")
		 (const :tag "CRAM-MD5" "cram-md5")
		 (const :tag "DIGEST-MD5" "digest-md5")
		 (const :tag "LOGIN" "login")
		 (string :tag "Other"))
  :group 'wl
  :group 'wl-setting)

(defcustom wl-smtp-authenticate-realm nil
  "*SMTP Authentication realm.
If you don't need to specify realm, set as nil."
  :type '(choice (const :tag "none" nil)
		 string)
  :group 'wl
  :group 'wl-setting)

(defcustom wl-pop-before-smtp-user nil
  "*POP3 user name to send mail using POP-before-SMTP.
If nil, `elmo-pop3-default-user' is used.
To use POP-before-SMTP,
\(setq wl-draft-send-mail-function 'wl-draft-send-mail-with-pop-before-smtp)"
  :type '(choice (const :tag "none" nil)
		 string)
  :group 'wl
  :group 'wl-setting)

(defcustom wl-pop-before-smtp-server nil
  "*POP3 server for POP-before-SMTP.
If nil, `elmo-pop3-default-server' is used."
  :type '(choice (const :tag "none" nil)
		 string)
  :group 'wl
  :group 'wl-setting)

(defcustom wl-pop-before-smtp-port nil
  "*POP3 port for POP-before-SMTP.
If nil, `elmo-pop3-default-port' is used."
  :type '(choice (const :tag "none" nil)
		 integer string)
  :group 'wl
  :group 'wl-setting)

(defcustom wl-pop-before-smtp-stream-type nil
  "*Stream type for POP-before-SMTP.
If nil, `elmo-pop3-default-stream-type' is used."
  :type '(choice (const :tag "Use `elmo-pop3-default-stream-type'" nil)
		 symbol)
  :group 'wl)

(defcustom wl-pop-before-smtp-authenticate-type nil
  "*Default Authentication type for POP-before-SMTP.
If nil, `elmo-pop3-default-authenticate-type' is used."
  :type '(choice (const :tag "none" nil)
		 (const :tag "APOP" 'apop)
		 (const :tag "POP3" 'user))
  :group 'wl
  :group 'wl-setting)

(defcustom wl-nntp-posting-server nil
  "*NNTP server name to post news.
If nil, `elmo-nntp-default-server' is used."
  :type '(choice (const :tag "none" nil)
		 string)
  :group 'wl
  :group 'wl-setting)
(defcustom wl-nntp-posting-user nil
  "*NNTP user name to post news for authinfo.
If nil, `elmo-nntp-default-user' is used.
If nil, don't authenticate."
  :type '(choice (const :tag "none" nil)
		 string)
  :group 'wl
  :group 'wl-setting)
(defcustom wl-nntp-posting-port nil
  "*NNTP port to post news.
If nil, `elmo-nntp-default-port' is used."
  :type '(choice (const :tag "none" nil)
		 integer string)
  :group 'wl
  :group 'wl-setting)
(defcustom wl-nntp-posting-stream-type nil
  "*Stream type for posting Netnews.
If nil, `elmo-nntp-default-stream-type' is used."
  :type '(choice (const :tag "Use `elmo-nntp-default-stream-type'" nil)
		 symbol)
  :group 'wl)
(defcustom wl-nntp-posting-function 'elmo-nntp-post
  "A function to post news.
Prepared candidate is 'elmo-nntp-post."
  :type '(radio (function-item elmo-nntp-post)
		(function :tag "Other"))
  :group 'wl-draft)
(defcustom wl-nntp-posting-config-alist nil
  "*Alist of configuration on nntp posting.
ex.
'((\",?local.test\" . \"news.media.kyoto-u.ac.jp\")
  (\",?ku\\.\" .
   ((server . \"news.media.kyoto-u.ac.jp\")
    (user . \"newsmaster\")
    (port . 119)
    (function . elmo-nntp-post))
  (\".*\" . \"newsfeed.kuee.kyoto-u.ac.jp\")))"
  :type '(repeat (cons (sexp :tag "Match")
		       (choice (string :tag "Server")
			       (repeat :inlie t
				       (cons (choice (const server)
						     (const user)
						     (const port)
						     (const stream-type)
						     (const function))
					     (sexp :tag "Value"))))))
  :group 'wl-draft
  :group 'wl-setting)

(defcustom wl-prefetch-confirm t
  "*Confirm prefetching if message size is larger than `wl-prefetch-threshold'."
  :type 'boolean
  :group 'wl-pref)

(defcustom wl-prefetch-threshold 30000
  "*Maximum size of message prefetched without confirmation.
If nil, all messages prefetched regardless of its size.
If message size is larger than this value, confirm prefetching
when `wl-prefetch-confirm' is non-nil."
  :type '(choice (integer :tag "Threshold (bytes)")
		 (const :tag "No limitation" nil))
  :group 'wl-pref
  :group 'wl-setting)

(defcustom wl-thread-insert-opened nil
  "*Non-nil forces to insert thread as opened in updating."
  :type 'boolean
  :group 'wl-summary
  :group 'wl-setting)

(defcustom wl-thread-open-reading-thread t
  "*Non-nil forces to open reading thread."
  :type 'boolean
  :group 'wl-summary)

(defcustom wl-additional-search-condition-fields nil
  "*A list of field name which is used for candidates of search condition."
  :type '(repeat (string :tag "Field name"))
  :group 'wl-pref
  :group 'wl-setting)

;;;; Hooks
(defvar wl-folder-mode-hook nil
  "A hook called when wanderlust folder mode is started.
This hook may contain the functions `wl-folder-init-icons' and
`wl-setup-folder' for reasons of system internal to accord facilities
for the Emacs variants.")
(defvar wl-summary-toggle-disp-on-hook nil
  "A hook called when message is toggled.")
(defvar wl-summary-toggle-disp-off-hook nil
  "A hook called when message is disappeared.")
(defvar wl-summary-toggle-disp-folder-on-hook nil
  "A hook called when folder is toggled.")
(defvar wl-summary-toggle-disp-folder-off-hook nil
  "A hook called when folder is disappeared.")
(defvar wl-summary-toggle-disp-folder-message-resumed-hook nil
  "A hook called when message window is resumed when folder is toggled.")
(defvar wl-summary-mode-hook nil
  "A hook called when summary mode is started.
This hook may contain the function `wl-setup-summary' for reasons of
system internal to accord facilities for the Emacs variants.")

(defvar wl-summary-prepared-pre-hook nil
  "A hook called before the summary buffer has been generated.")
(defvar wl-summary-prepared-hook nil
  "A hook called after the summary buffer has been generated.")
(defvar wl-summary-sync-updated-hook nil
  "A hook called when update summary buffer.")
(defvar wl-summary-unread-message-hook nil
  "A hook called when unread message is displayed.")
(defvar wl-summary-edit-addresses-hook nil
  "A hook called when address book is edited.")
(defvar wl-summary-buffer-message-saved-hook nil
  "A hook called when msgdb is saved.")
(defvar wl-summary-buffer-mark-saved-hook nil
  "A hook called when mark is saved.")
(defvar wl-summary-divide-thread-when-subject-changed nil
  "Divide thread when subject is changed.")
(defvar wl-init-hook nil
  "A hook called when initialization is finished.  This hook may contain
the functions `wl-plugged-init-icons' and `wl-biff-init-icons' for
reasons of system internal to accord facilities for the Emacs variants.")
(defvar wl-hook nil
  "A hook called when Wanderlust is invoked.")

(defvar wl-draft-reply-hook
  '((lambda () (wl-draft-setup-parent-flag 'answered)))
  "A hook called when replied.
This hook runs on the draft buffer.")

(defvar wl-draft-forward-hook
  '((lambda () (wl-draft-setup-parent-flag 'forwarded)))
  "A hook called when forwarded.
This hook runs on the draft buffer.")

(defvar wl-draft-kill-pre-hook nil
  "A hook called just before the draft buffer is killed.")

(defvar wl-summary-reply-hook nil
  "A hook called when `wl-summary-reply' is called.
This hook runs on the summary buffer.")

(defvar wl-summary-forward-hook nil
  "A hook called when `wl-summary-forward' is called.
This hook runs on the summary buffer.")

(defvar wl-summary-resend-hook nil
  "A hook runs on the resent message buffer before sending process starts.")

(defvar wl-mail-setup-hook nil
  "A hook called when Draft is prepared.")
(defvar wl-draft-reedit-hook '(wl-draft-remove-text-plain-tag)
  "A hook called when Draft is re-edited.
The cursor point is located at top of the body.")
(defvar wl-draft-send-hook '(wl-draft-config-exec)
  "A hook called on the draft editing buffer before sending process starts.")
(defvar wl-mail-send-pre-hook nil
  "A hook called just before the mail sending process starts.")
(defvar wl-news-send-pre-hook nil
  "A hook called just before the news sending process starts.")
(defvar wl-message-buffer-created-hook nil
  "A hook called when Message buffer is prepared.")
(defvar wl-message-redisplay-hook nil
  "A hook called when Message is displayed.")
(defvar wl-message-exit-hook nil
  "A hook called when quit message.")
(defvar wl-summary-exit-pre-hook nil
  "A hook called before exit summary mode.")
(defvar wl-summary-exit-hook nil
  "A hook called when exit summary mode.")
(defvar wl-highlight-headers-hook nil
  "A hook called when header is highlighted.")
(defvar wl-highlight-message-hook nil
  "A hook called when message is highlighted.")
(defvar wl-save-hook nil
  "A hook called when save summary and folder status.")
(defvar wl-exit-hook nil
  "A hook called when exit wanderlust.")
(defvar wl-folder-suspend-hook nil
  "A hook called when suspend wanderlust.")
(defvar wl-biff-notify-hook '(ding)
  "A hook called when a biff-notification is invoked.")
(defvar wl-biff-unnotify-hook nil
  "A hook called when a biff-notification is removed.")
(defvar wl-auto-check-folder-pre-hook nil
  "A hook called before auto check folders.")
(defvar wl-auto-check-folder-hook nil
  "A hook called when auto check folders.")
(defvar wl-folder-check-entity-pre-hook nil
  "A hook called before check entity.")
(defvar wl-folder-check-entity-hook nil
  "A hook called when check entity.")
(defvar wl-draft-config-exec-hook nil
  "A hook called when execute header-config on draft.")
(defvar wl-summary-expire-pre-hook nil
  "A hook called before expire.")
(defvar wl-summary-expire-hook nil
  "A hook called when expired.")
(defvar wl-summary-archive-pre-hook nil
  "A hook called before archive.")
(defvar wl-summary-archive-hook nil
  "A hook called when archived.")
(defvar wl-summary-line-inserted-hook nil
  "A hook called when summary line is inserted.")
(defvar wl-summary-insert-headers-hook nil
  "A hook called when insert header for search header.")
(defvar wl-message-display-internal-hook nil
  "A hook called when message buffer is created and message is displayed.
This hook may contain the functions `wl-setup-message' for
reasons of system internal to accord facilities for the Emacs variants.")
(defvar wl-thread-update-children-number-hook nil
  "A hook called when children number is updated.")
(defvar wl-folder-update-access-group-hook nil
  "A hook called when update access group folder.")
(defvar wl-draft-cited-hook nil
  "A hook called after a message is cited.")
(defvar wl-draft-insert-x-face-field-hook nil
  "A hook called after a x-face field is inserted.")
(defvar wl-template-mode-hook nil
  "A hook called when template mode is started.")
(defvar wl-score-mode-hook nil
  "A hook called when score mode is started.")
(defvar wl-make-plugged-hook nil
  "A hook called when make plugged alist.")

(defvar wl-plugged-exit-hook nil
  "A hook called when exit plugged mode.")

;;;; functions for draft
(defcustom wl-draft-send-function 'wl-draft-normal-send-func
  "A function to send message."
  :type 'function
  :group 'wl-draft)

(defcustom wl-draft-send-news-function 'wl-draft-elmo-nntp-send
  "A function to send news."
  :type 'function
  :group 'wl-draft)

(defcustom wl-draft-send-mail-function 'wl-draft-send-mail-with-smtp
  "A function to send mail.
Prepared candidates are 'wl-draft-send-mail-with-smtp,
'wl-draft-send-mail-with-sendmail, 'wl-draft-send-mail-with-qmail
and 'wl-draft-send-mail-with-pop-before-smtp."
  :type '(radio (function-item wl-draft-send-mail-with-smtp)
		(function-item wl-draft-send-mail-with-sendmail)
		(function-item wl-draft-send-mail-with-qmail)
		(function-item wl-draft-send-mail-with-pop-before-smtp)
		(function :tag "Other"))
  :group 'wl-draft)

(defcustom wl-draft-send-confirm-type 'scroll-by-SPC/BS
  "*Confirmation type or function to use when send a message."
  :type '(choice
	  (const :tag "y or n with scroll (j/k)" scroll-by-j/k)
	  (const :tag "y or n with scroll (SPC/BS)" scroll-by-SPC/BS)
	  (function-item y-or-n-p)
	  (function-item yes-or-no-p)
	  (function :tag "Other function"))
  :group 'wl-draft)

(defcustom wl-draft-reply-with-argument-list
  '(("From" . (("Reply-To" "Mail-Reply-To" "From")
	       ("Mail-Followup-To" "To" "Cc")
	       ("Followup-To" "Newsgroups"))))
  "Alist of cons cell of
\('condition' .  ('fields for To' 'fields for Cc' 'fields for Newsgroups'))
'condition' is a header name string (non-nil if the header exists in original
message), a function (evaluated in original message buffer) or a list of those
\(means 'AND' condition).
'fields for ***' is a list of strings.
If car of each cons cell returns non-nil value,
cdr of each cons cell is used for preparing headers of draft message.
Default is for 'reply-to-all'."
  :type '(repeat (cons (choice (string :tag "Field Name")
			       (symbol :tag "Function")
			       (const :tag "Replying to self" wl-draft-self-reply-p)
			       (repeat :tag "AND"
				       (choice (string :tag "Field Name")
					       (symbol :tag "Function")
					       (const :tag "Replying to self" wl-draft-self-reply-p))))
		       (list (repeat :tag "Fields For To" string)
			     (repeat :tag "Fields For Cc" string)
			     (repeat :tag "Fields For Newsgroups" string))))
  :group 'wl-draft)

(defcustom wl-draft-reply-without-argument-list
  '(("Followup-To" . (("Mail-Followup-To" "Mail-Reply-To" "Reply-To") nil ("Followup-To")))
    ("Mail-Followup-To" . (("Mail-Followup-To") nil nil))
    ("Newsgroups" . (("Mail-Reply-To" "Reply-To" "To") ("Cc") ("Newsgroups")))
    ("Mail-Reply-To" . (("Mail-Reply-To" "Reply-To") ("To" "Cc") nil))
    ("Reply-To" . (("Reply-To") ("To" "Cc") nil))
    (wl-draft-self-reply-p . (("To") ("Cc") nil))
    ("From" . (("From") ("To" "Cc") nil)))
  "Alist of cons cell of
\('condition' .  ('fields for To' 'fields for Cc' 'fields for Newsgroups'))
'condition' is a header name string (non-nil if the header exists in original
message), a function (evaluated in original message buffer) or a list of those
\(means 'AND' condition).
'fields for ***' is a list of strings.
If car of each cons cell returns non-nil value,
cdr of each cons cell is used for preparing headers of draft message."
  :type '(repeat (cons (choice (string :tag "Field Name")
			       (symbol :tag "Function")
			       (const :tag "Replying to self" wl-draft-self-reply-p)
			       (repeat :tag "AND"
				       (choice (string :tag "Field Name")
					       (symbol :tag "Function")
					       (const :tag "Replying to self" wl-draft-self-reply-p))))
		       (list (repeat :tag "Fields For To" string)
			     (repeat :tag "Fields For Cc" string)
			     (repeat :tag "Fields For Newsgroups" string))))
  :group 'wl-draft)

(defcustom wl-draft-always-delete-myself nil
  "*Always delete myself from recipient if non-nil."
  :type 'boolean
  :group 'wl-draft)

(defcustom wl-draft-delete-myself-from-bcc-fcc nil
  "*Do not insert bcc or fcc if To and Cc fields is a member of
`wl-subscribed-mailing-list'"
  :type 'boolean
  :group 'wl-draft)

(defcustom wl-draft-resume-folder-window t
  "*Resume folder window in `wl-draft-hide'."
  :type 'boolean
  :group 'wl-draft)

(defcustom wl-draft-use-frame nil
  "*Raise new frame when composing draft."
  :type 'boolean
  :group 'wl-draft)

(defcustom wl-draft-qmail-send-plugged nil
  "*Send mail when plugged is on, in the `wl-draft-send-mail-with-qmail'."
  :type 'boolean
  :group 'wl-draft)

(defcustom wl-draft-remove-group-list-contents t
  "*If non-nil, remove group list contents in `wl-draft-send-mail-with-smtp'."
  :type 'boolean
  :group 'wl-draft)

;;;;
(defcustom wl-init-file "~/.wl"
  "*User customization setting file."
  :type 'file
  :group 'wl)

(defcustom wl-folders-file "~/.folders"
  "*Folders file."
  :type 'file
  :group 'wl)

(defcustom wl-address-file "~/.addresses"
  "*Addresses file."
  :type 'file
  :group 'wl)

(defcustom wl-alias-file "~/.im/Aliases"
  "*Alias file for completion."
  :type 'file
  :group 'wl)

(defcustom wl-ldap-server nil
  "*LDAP server."
  :type '(choice (const :tag "Default server(localhost)" nil)
		 (string :tag "Server"))
  :group 'wl
  :group 'wl-setting)

(defcustom wl-ldap-port nil
  "*LDAP port."
  :type '(choice (const :tag "Default port" nil)
		 integer)
  :group 'wl
  :group 'wl-setting)

(defcustom wl-ldap-base nil
  "*LDAP base."
  :type '(choice (const :tag "Default base" nil)
		 (string :tag "Base"))
  :group 'wl
  :group 'wl-setting)

(defcustom wl-use-ldap nil
  "*If non-nil, use LDAP for address completion."
  :type 'boolean
  :group 'wl
  :group 'wl-setting)

(defcustom wl-use-acap nil
  "*If non-nil, use ACAP for configuration."
  :type 'boolean
  :group 'wl)

(defcustom wl-folder-info-save t
  "If non-nil, save elmo-folder-info-alist."
  :type 'boolean
  :group 'wl-folder)

(defcustom wl-summary-persistent-mark-priority-list '(killed
						      flag
						      new
						      answered
						      forwarded
						      unread)
  "List of preserved flag symbols to define the priority to map \
to the persistent mark.
Special symbol `flag' means the user defined flag."
  :type '(repeat (symbol :tag "preserved flag"))
  :group 'wl-summary)

(defcustom wl-summary-flag-alist
  '((important "orange"))
  "An alist to define the global flags for the summary mode.
Each element is a form like:
\(SYMBOL-OF-FLAG COLOR [MARK]\)
Example:
\((important \"orange\"\)
 \(todo \"red\" \"T\"\)
 \(business \"green\" \"B\"\)
 \(private \"blue\"\)\)"
  :type '(repeat (list (symbol :tag "flag")
		       (string :tag "color")
		       (choice (string :tag "mark")
			       (const :tag "Default mark" nil))))
  :group 'wl-summary)

(defcustom wl-summary-killed-mark "X"
  "Mark for killed message."
  :type '(string :tag "Mark")
  :group 'wl-summary-marks)

(defcustom wl-summary-uncached-mark "!"
  "Mark for uncached message with no flag."
  :type '(string :tag "Mark")
  :group 'wl-summary-marks)

(defcustom wl-summary-new-uncached-mark "N"
  "Mark for new and uncached message."
  :type '(string :tag "Mark")
  :group 'wl-summary-marks)

(defcustom wl-summary-new-cached-mark "n"
  "Mark for new but already cached message."
  :type '(string :tag "Mark")
  :group 'wl-summary-marks)

(defcustom wl-summary-unread-uncached-mark "U"
  "Mark for unread and uncached message."
  :type '(string :tag "Mark")
  :group 'wl-summary-marks)

(defcustom wl-summary-unread-cached-mark "u"
  "Mark for unread but already cached message."
  :type '(string :tag "Mark")
  :group 'wl-summary-marks)

(defcustom wl-summary-answered-cached-mark "a"
  "Mark for answered and cached message."
  :type '(string :tag "Mark")
  :group 'wl-summary-marks)

(defcustom wl-summary-answered-uncached-mark "A"
  "Mark for answered but uncached message."
  :type '(string :tag "Mark")
  :group 'wl-summary-marks)

(defcustom wl-summary-flag-mark "$"
  "Mark for the messages which have tags."
  :type '(string :tag "Mark")
  :group 'wl-summary-marks)

(defcustom wl-summary-score-over-mark "+"
  "Score mark used for messages with high scores."
  :type '(string :tag "Mark")
  :group 'wl-summary-marks)

(defcustom wl-summary-score-below-mark "-"
  "Score mark used for messages with low scores."
  :type '(string :tag "Mark")
  :group 'wl-summary-marks)

(defcustom wl-summary-no-mime-folder-list nil
  "*All folders that match this list don't analyze mime."
  :type '(repeat string)
  :group 'wl-summary)

(defcustom wl-summary-display-mime-mode-list '(mime as-is)
  "*Display mime mode list toggled by `wl-summary-toggle-mime'.
Candidates are following:
`mime'        ... header and body are decoded
`header-only' ... only header is decoded
`as-is'       ... header and body are not decoded"
  :type '(repeat (choice (const :tag "MIME"	   mime)
			 (const :tag "HEADER-ONLY" header-only)
			 (const :tag "AS-IS"       as-is)))
  :group 'wl-summary)

(defcustom wl-summary-fix-timezone nil
  "*Time zone of the date string in summary mode.
If nil, it is adjust to the default time zone information
\(system's default time zone or environment variable TZ)."
  :type '(choice (const :tag "Default time zone" nil)
		 string)
  :group 'wl-summary)

(defcustom wl-summary-message-ring-max 16
  "*Maximum size of message ring on summary buffer.
Start discarding off end if gets this big."
  :type 'integer
  :group 'wl-summary)

(defcustom wl-summary-default-score 0
  "*Default message score level.
All scores generated by the score files will be added to this score.
If this variable is nil, scoring will be disabled."
  :type '(choice (const :tag "disable" nil)
		 integer)
  :group 'wl-score)

(defcustom wl-summary-important-above nil
  "*Mark all messages with a score above this variable as important.
This variable is local to the summary buffers."
  :type '(choice (const :tag "off" nil)
		 integer)
  :group 'wl-score)

(defcustom wl-summary-target-above nil
  "*Mark all messages with a score above this variable as target.
This variable is local to the summary buffers."
  :type '(choice (const :tag "off" nil)
		 integer)
  :group 'wl-score)

(defcustom wl-summary-mark-below 0
  "*Mark all messages with a score below this variable as read.
This variable is local to each summary buffer and usually set by the
score file."
  :type 'integer
  :group 'wl-score)

(defcustom wl-summary-expunge-below nil
  "All messages that have a score less than this variable will be expunged.
This variable is local to the summary buffers."
  :type '(choice (const :tag "off" nil)
		 integer)
  :group 'wl-score)

(defcustom wl-summary-score-marks
  (list wl-summary-new-uncached-mark wl-summary-new-cached-mark)
  "Persistent marks to scoring."
  :type '(repeat (string :tag "Mark"))
  :group 'wl-score)

(defcustom wl-use-scoring t
  "*If non-nil, enable scoring."
  :type 'boolean
  :group 'wl-pref)

(defcustom wl-summary-rescore-partial-threshold 200
  "*Summary is not scored entirely if there are messages more than this value.
In sync-all or rescan."
  :type 'integer
  :group 'wl-score)

(defcustom wl-score-files-directory (concat elmo-msgdb-directory elmo-path-sep)
  "*Name of the directory where score files will be stored.
\(default \"~/.elmo\")."
  :type 'directory
  :group 'wl)

(defcustom wl-score-interactive-default-score 1000
  "*Scoring commands will raise/lower the score with this number as the default."
  :type 'integer
  :group 'wl-score)

(defcustom wl-score-expiry-days 7
  "*Number of days before unused score file entries are expired.
If this variable is nil, no score file entries will be expired."
  :type '(choice (const :tag "never" nil)
		 number)
  :group 'wl-score)

(defcustom wl-score-update-entry-dates t
  "*In non-nil, update matching score entry dates.
If this variable is nil, then score entries that provide matches
will be expired along with non-matching score entries."
  :type 'boolean
  :group 'wl-score)

(defcustom wl-score-folder-alist nil
  "*Alist of folder regexp and score file."
  :type '(repeat (list (regexp :tag "Folder Regexp")
		       (repeat :inline t
			       (choice file
				       (symbol :tag "Variable")))))
  :group 'wl-score)

(defcustom wl-score-folder-alist-matchone t
  "*If non-nil, getting only one element of `wl-score-folder-alist'."
  :type 'boolean
  :group 'wl-score)

(defcustom wl-score-default-file "all.SCORE"
  "*Default score file name."
  :type 'file
  :group 'wl-score)

(defcustom wl-score-simplify-fuzzy-regexp
  '("^[ \t]*\\[[^:]+[,: ][0-9]+\\][ \t]*")
  "*Strings to be removed when doing fuzzy matches.
This can either be a regular expression or list of regular expressions."
  :type '(repeat regexp)
  :group 'wl-score)

(defcustom wl-score-header-default-entry
  '(("number" -1000 perm =)
    ("subject" -1000 nil nil)
    ("from" -1000 perm s)
    ("message-id" -1000 temp e)
    ("references" -1000 perm e)
    ("to" -1000 perm s)
    ("cc" -1000 perm s)
    ("date" -1000 temp nil)
    ("xref" -1000 perm s)
    ("extra" -1000 perm s)
    ("chars" -1000 perm >)
    ("lines" -1000 perm >)
    ("followup" -1000 perm s)
    ("thread" -1000 temp s))
  "*Default entry when insert score entry."
  :type '(repeat (list (string :tag "Header")
		       (choice (integer :tag "Score")
			       (const :tag "Ask" nil))
		       (choice (const :tag "Permanent" perm)
			       (const :tag "Temporary" temp)
			       (const :tag "Ask" nil))
		       (choice (const :tag "Regexp string" r)
			       (const :tag "Substring" s)
			       (const :tag "fuzzy string" f)
			       (const :tag "Exact string" e)
			       (const :tag "REGEXP STRING" R)
			       (const :tag "SUBSTRING" S)
			       (const :tag "FUZZY STRING" F)
			       (const :tag "EXACT STRING" E)
			       (const :tag "less than" <)
			       (const :tag "less equal" <=)
			       (const :tag "greater than" >)
			       (const :tag "greater equal" >=)
			       (const :tag "equal" =)
			       (const :tag "Ask" nil))))
  :group 'wl-score)

(defcustom wl-score-mode-mime-charset 'x-ctext
  "*MIME Charset for score file."
  :type 'symbol
  :group 'wl-score)

(defcustom wl-draft-fields
  '("To:" "Cc:" "Bcc:" "Fcc:" "Distribution:" "Organization:"
    "Newsgroups:" "Followup-To:" "Mail-Followup-To:" "From:" "Reply-To:")
  "Fields used in draft mode."
  :type '(repeat (string :tag "Field"))
  :group 'wl-draft)

;; MIME Bcc.
(defcustom wl-draft-mime-bcc-field-name "Ecc"
  "Field name for MIME-encapsulated Bcc."
  :type '(string :tag "Field Name")
  :group 'wl-draft)

(defcustom wl-draft-mime-bcc-body nil
  "Body string for MIME-encapsulated Bcc.
If nil, a string `This is a blind carbon copy.' is used."
  :type '(choice (const :tag "default" nil)
		 (string :tag "Body"))
  :group 'wl-draft)

(defcustom wl-draft-disable-bcc-for-mime-bcc t
  "Disable Bcc while MIME-encapsulated Bcc."
  :type 'boolean
  :group 'wl-draft)

(defcustom wl-draft-disable-fcc-for-mime-bcc t
  "Disable Fcc while MIME-encapsulated Bcc."
  :type 'boolean
  :group 'wl-draft)

(defcustom wl-draft-config-alist nil
  "Alist of condition and actions for dynamical draft modification.
First element of each list is some condition for the draft buffer (regular
expression for header or elisp expression) and remaining elements indicate
actions.
If the first element is `reply' keyword, the next element be the condition
for the message being replied, and remaining elements are actions.

The configuration is applied when `wl-draft-config-exec' is called, or
applied automatically before sending message.

ex.
'((\"^To: .*wl@ml.gentei.org\"
   (\"From\" . my-from-address-for-wl-list)
   (\"Organization\" . my-organization-for-wl-list))
  (reply
   \"^To: .*hogehoge@aaa.ne.jp\"
   (\"From\" . \"Alternative Address <hogehoge@aaa.ne.jp>\")
   my-draft-config-function-hogehoge))

See also variable `wl-draft-parent-folder'."
  :type '(repeat (list (sexp :tag "Match")
		       (repeat
			:inline t
			(choice (cons (sexp :tag "Field(Variable)")
				      (sexp :tag "Value"))
				(sexp :tag "Function")))))
  :group 'wl-draft
  :group 'wl-setting)

(defcustom wl-draft-config-matchone nil
  "*If non-nil, applied only one element of `wl-draft-config-alist'."
  :type 'boolean
  :group 'wl-draft
  :group 'wl-setting)

(defcustom wl-draft-elide-ellipsis "\n[...]\n\n"
  "*The string which is inserted for elided text."
  :type 'string
  :group 'wl-draft)

(defcustom wl-template-alist nil
  "Alist of template.
First element of each list is a string specifies the name of the template.
Remaining elements indicate actions. The format of actions is same as that
of `wl-draft-config-alist'."
  :type '(repeat (list (string :tag "Name")
		       (repeat
			:inline t
			(choice (cons (sexp :tag "Field(Variable)")
				      (sexp :tag "Value"))
				(sexp :tag "Function")))))
  :group 'wl-draft
  :group 'wl-setting)

(defcustom wl-template-visible-select t
  "*If non-nil, select template with visible."
  :type 'boolean
  :group 'wl-draft)

(defcustom wl-template-confirm nil
  "*If non-nil, require your confirmation when selected template."
  :type 'boolean
  :group 'wl-draft)

(defcustom wl-template-buffer-lines 7
  "*Lines of template buffer."
  :type 'integer
  :group 'wl-draft)

;; queued sending.
(defcustom wl-draft-enable-queuing t
  "*Non-nil enables queued sending."
  :type 'boolean
  :group 'wl-draft
  :group 'wl-pref)

(defcustom wl-draft-force-queuing nil
  "*Non-nil forces queued sending for mail and news."
  :type 'boolean
  :group 'wl-draft
  :group 'wl-pref)

(defcustom wl-draft-force-queuing-mail nil
  "*Non-nil forces queued sending for mail."
  :type 'boolean
  :group 'wl-draft
  :group 'wl-pref)

(defcustom wl-draft-force-queuing-news nil
  "*Non-nil forces queued sending for news."
  :type 'boolean
  :group 'wl-draft
  :group 'wl-pref)

(defcustom wl-draft-use-cache nil
  "*If non-nil, sending message is cached."
  :type 'boolean
  :group 'wl-draft
  :group 'wl-pref)

(defcustom wl-auto-flush-queue t
  "*If non-nil, sending queue is flushed when network status is toggled."
  :type 'boolean
  :group 'wl-draft
  :group 'wl-pref)

(defcustom wl-draft-buffer-style 'full
  "Style of draft buffer except for `wl-summary-reply' and `wl-summary-forward'
'keep is to use current window, 'full is to use full frame window and
'split is to split current window.
If it is a function, it is called with the draft buffer as an argument."
  :type '(choice (const :tag "Keep window" keep)
		 (const :tag "Split window" split)
		 (const :tag "Full window" full)
		 (sexp :tag "Use Function"))
  :group 'wl-draft)

(defcustom wl-draft-reply-buffer-style 'split
  "Style of draft buffer for `wl-summary-reply' and `wl-summary-forward'
'keep is to use message buffer window, 'full is to use full frame window and
'split is to split message buffer window.
If it is a function, it is called with the draft buffer as an argument."
  :type '(choice (const :tag "Keep window" keep)
		 (const :tag "Split window" split)
		 (const :tag "Full window" full)
		 (sexp :tag "Use Function"))
  :group 'wl-draft)

(defcustom wl-draft-reply-default-position 'body
  "Begining position of reply buffer.
'body means the top of body.
'bottom means the bottom of body.
'top means the top of header.
\"To\", \"Newsgroups\", \"Subject\" means the position of the header field.
You can also set it to a list of setting.
"
  :type '(choice (repeat
		  (choice
		   (const :tag "Top of body" body)
		   (const :tag "Bottom of body" bottom)
		   (const :tag "Top of header" top)
		   (const "To")
		   (const "Newsgroups")
		   (const "Subject")
		   (string :tag "Header Name")))
		 (const :tag "Top of body" body)
		 (const :tag "Bottom of body" bottom)
		 (const :tag "Top of header" top)
		 (const "To")
		 (const "Newsgroups")
		 (const "Subject")
		 (string :tag "Header Name"))
  :group 'wl-draft)

(defcustom wl-draft-queue-save-variables
  '(wl-envelope-from wl-from
    wl-smtp-posting-server wl-smtp-posting-user wl-smtp-posting-port
    wl-smtp-authenticate-type wl-smtp-connection-type
    wl-pop-before-smtp-server wl-pop-before-smtp-user wl-pop-before-smtp-port
    wl-pop-before-smtp-stream-type wl-pop-before-smtp-authenticate-type
    wl-nntp-posting-server wl-nntp-posting-server
    wl-nntp-posting-user wl-nntp-posting-port wl-nntp-posting-stream-type)
  "*Saving variables in queue info."
  :type '(repeat (sexp :tag "Variable"))
  :group 'wl-draft)

(defcustom wl-draft-sendlog t
  "*Keep send state in log if non-nil."
  :type 'boolean
  :group 'wl-draft)

(defcustom wl-draft-sendlog-max-size 20000
  "*Max file size of sendlog."
  :type '(choice (const :tag "Unlimited" nil)
		 integer)
  :group 'wl-draft)

(defcustom wl-summary-default-number-column 5
  "Number of columns in summary buffer."
  :type 'integer
  :group 'wl-summary)

(defcustom wl-summary-number-column-alist '(("\\*.*" . 6))
  "Alist of folder and its number column.
If no matches, 'wl-summary-default-number-column' is used.
ex.
'((\"^%inbox@qmail-maildir\" . 9)
  (\"^-.*@news-server\" . 6))"
  :type '(repeat (cons (regexp :tag "Folder Regexp") integer))
  :group 'wl-summary)

(defcustom wl-summary-highlight t
  "Non-nil forces Summary buffer to be highlighted."
  :type 'boolean
  :group 'wl-summary
  :group 'wl-highlight)

(defcustom wl-summary-lazy-highlight (boundp 'window-scroll-functions)
  "Non-nil forces lazy summary highlighting using `window-scroll-functions'."
  :type 'boolean
  :group 'wl-summary
  :group 'wl-highlight)

(defcustom wl-summary-highlight-partial-threshold 1000
  "Summary is not highlighted entirely if there are lines more than this value.
Available if only `wl-summary-lazy-highlight' is nil."
  :type 'integer
  :group 'wl-summary
  :group 'wl-highlight)

(defcustom wl-summary-partial-highlight-above-lines 30
  "If Summary has lines more than `wl-summary-highlight-partial-threshold',
Summary lines are highlighted partialy above current position.
Available if only `wl-summary-lazy-highlight' is nil."
  :type 'integer
  :group 'wl-summary
  :group 'wl-highlight)

(defcustom wl-summary-lazy-update-mark (boundp 'window-scroll-functions)
  "Non-nil forces lazy update mark using `window-scroll-functions'."
  :type 'boolean
  :group 'wl-summary)

(defcustom wl-summary-cache-use t
  "Non-nil forces wl-summary to use cache file."
  :type 'boolean
  :group 'wl-summary)

(defcustom wl-summary-auto-sync-marks t
  "Non-nil forces to synchronize unread/important marks."
  :type 'boolean
  :group 'wl-summary)

(defcustom wl-summary-cache-file ".wl-summary-cache"
  "*Cache file for summary mode contents."
  :type 'file
  :group 'wl-summary)
(defcustom wl-summary-view-file ".wl-summary-view"
  "*Current summary view."
  :type 'file
  :group 'wl-summary)
(defcustom wl-thread-top-file ".wl-thread-top"
  "*Current thread top entity... obsolete."
  :type 'file
  :group 'wl-summary)
(defcustom wl-thread-entity-file ".wl-thread-entity"
  "*Thread entities."
  :type 'file
  :group 'wl-summary)
(defcustom wl-thread-entity-list-file ".wl-thread-entity-list"
  "*Thread top entity list."
  :type 'file
  :group 'wl-summary)

(defcustom wl-print-buffer-function 'lpr-buffer
  "A function to print current buffer."
  :type 'function
  :group 'wl-pref)

(defcustom wl-ps-print-buffer-function
  (if window-system 'ps-print-buffer-with-faces 'ps-print-buffer)
  "A function to print current buffer with ps-print."
  :type 'function
  :group 'wl-pref)

;;;; Preferences
(defcustom wl-use-petname t
  "*Display petname in summary and default citation title."
  :type 'boolean
  :group 'wl-pref)

(defcustom wl-use-folder-petname
  '(modeline)
  "*List of situation using folder petname.
Allowed situations are:
  modeline    : displayed on modeline.
  ask-folder  : displayed on minibuffer when ask folder.
  read-folder : can used for completion at `wl-summary-read-folder'."
  :type '(set (const modeline)
	      (const ask-folder)
	      (const read-folder))
  :group 'wl-summary
  :group 'wl-pref)

(defcustom wl-folder-petname-alist nil
  "A list of (realname . petname)."
  :type '(repeat (cons (string :tag "Realname") (string :tag "Petname")))
  :group 'wl-folder)

(defcustom wl-summary-weekday-name-lang
  (if (and (boundp 'current-language-environment)
	   (string-equal "Japanese"
			 (symbol-value 'current-language-environment)))
      "ja" "en")
  "*Language to display week day."
  :type '(choice (const "ja")
		 (const "en")
		 (const "fr")
		 (const "de")
		 (string :tag "Other"))
  :group 'wl-summary
  :group 'wl-pref)

(defcustom wl-message-id-use-message-from
  (if (boundp 'wl-message-id-use-wl-from)
      wl-message-id-use-wl-from t)
  "*When non-nil, use From: header's field value for domain part of Message-ID preferably."
  :type 'boolean
  :group 'wl-pref)

(defcustom wl-message-id-hash-function nil
  "Indicate hash function for the local part when Message-ID is made from mail address.  Hash function receives a string and returns hashed string.  Nil means the local part is not hashed."
  :type '(choice (const :tag "as is" nil)
		 function)
  :group 'wl-pref)

(defcustom wl-local-domain nil
  "*Domain part of this client (without hostname).
Set this if (system-name) does not return FQDN."
  :type '(choice (const :tag "Use System Name" nil)
		 string)
  :group 'wl-pref)

(defcustom wl-message-id-domain nil
  "*Specific domain part of Message-ID."
  :type '(choice (const :tag "Use System Name" nil)
		 string)
  :group 'wl-pref)

(defcustom wl-unique-id-suffix ".wl"
  "*Specific string in generated Message-ID
which appear just before @ (domain based) or % (mail address based)."
  :type 'string
  :group 'wl-pref)

(defcustom wl-break-pages t
  "*Break Pages at ^L."
  :type 'boolean
  :group 'wl-pref)

(defcustom wl-message-auto-reassemble-message/partial nil
  "*Reassemble message/partial messages automatically on show when non-nil."
  :type 'boolean
  :group 'wl-pref)

(defcustom wl-message-use-header-narrowing t
  "Use header narrowing when non-nil."
  :type 'boolean
  :group 'wl-pref)

(defcustom wl-message-header-narrowing-fields '("to" "cc")
  "A list of field name to enable header narrowing."
  :type '(repeat string)
  :group 'wl-pref)

(defcustom wl-message-header-narrowing-lines 4
  "Line number to enable the header narrowing."
  :type 'integer
  :group 'wl-pref)

(defcustom wl-message-header-narrowing-string "..."
  "A string used for header narrowing truncation."
  :type 'string
  :group 'wl-pref)

(defvar wl-message-mode-line-format-spec-alist
  '((?f (if (memq 'modeline wl-use-folder-petname)
	    (wl-folder-get-petname wl-message-buffer-cur-folder)
	  wl-message-buffer-cur-folder))
    (?m (upcase (symbol-name
		 (wl-message-display-type-property
		  wl-message-buffer-cur-display-type
		  :mime))))
    (?F wl-message-buffer-flag-indicator)
    (?n wl-message-buffer-cur-number))
  "An alist of format specifications for message buffer's mode-lines.
Each element is a list of following:
\(SPEC STRING-EXP)
SPEC is a character for format specification.
STRING-EXP is an expression to get string to insert.")

(defcustom wl-message-mode-line-format "Wanderlust: << %f / %n %F>> [%m]"
  "*A format string for message buffer's mode-line of Wanderlust.
It may include any of the following format specifications
which are replaced by the given information:

%f The folder name.
%n The number of the message.
%m The MIME analysis status.
%F The global flag indicator."
  :group 'wl-pref
  :type 'string)

(defcustom wl-message-truncate-lines default-truncate-lines
  "*Truncate lines in Message Buffer."
  :type 'boolean
  :group 'wl-pref)

(defcustom wl-draft-truncate-lines default-truncate-lines
  "*Truncate lines in Draft Buffer."
  :type 'boolean
  :group 'wl-draft
  :group 'wl-pref)

(defcustom wl-message-scroll-amount nil
  "*Scroll amount by SPC key."
  :type '(choice (const :tag "scrolling by screenfuls" nil)
		 integer)
  :group 'wl-pref)

(defcustom wl-message-window-size '(1 . 4)
  "*Size of summary and message window.  cons cell of (Summary : Message)."
  :type '(cons integer integer)
  :group 'wl-pref)

(defcustom wl-message-sort-field-list '("Return-Path" "Received" "^To" "^Cc"
					"Newsgroups" "Subject" "^From")
  "*Sort order of header fields.  Each elements are regexp of field name."
  :type '(repeat (string :tag "Field Regexp"))
  :group 'wl-pref
  :group 'wl-setting)

(defcustom wl-message-ignored-field-list nil
  "All fields that match this list will be hidden in message buffer.
Each elements are regexp of field-name.
You can specify exceptions by `wl-message-visible-field-list'."
  :type '(repeat (string :tag "Field Regexp"))
  :group 'wl-pref
  :group 'wl-setting)

(defcustom wl-message-visible-field-list nil
  "All fields that match this list will be displayed in message buffer.
Each elements are regexp of field-name.
This variable overwhelm `wl-message-ignored-field-list' settings."
  :type '(repeat (string :tag "Field Regexp"))
  :group 'wl-pref
  :group 'wl-setting)

(defcustom wl-message-header-button-alist
  '(("^\\(References\\|Message-Id\\|In-Reply-To\\):"
     "<[^>\n ]+>"
     0 wl-message-button-refer-article  0)
    ("^[^:]+:"
     "\\(<\\(url: \\)?news:\\([^>\n ]*\\)>\\)"
     1 wl-message-button-refer-article 3))
  "Alist of headers and regexps to match buttons in message headers."
  :type '(repeat
	  (list (regexp :tag "Header")
		regexp
		(integer :tag "Button")
		(function :tag "Callback")
		(repeat :tag "Data"
			:inline t
			(integer :tag "Regexp group"))))
  :group 'wl-pref)

(defcustom wl-message-body-button-alist
  '(("<mailto:[^>]+>" 0 'ignore 0 1024)
    ("<[^>\n ]+@[^>\n ]+>" 0 wl-message-button-refer-article 0 1024))
  "Alist of regexps to match buttons in message body."
  :type '(repeat
	  (list regexp
		(integer :tag "Button")
		(function :tag "Callback")
		(repeat :tag "Data"
			:inline t
			(integer :tag "Regexp group"))
		(integer :tag "Max Length")))
  :group 'wl-pref)

(defcustom wl-folder-window-width 20
  "*Width of folder window."
  :type 'integer
  :group 'wl-folder
  :group 'wl-pref)

(defcustom wl-summary-recenter t
  "*Recenter on redisplay."
  :type 'boolean
  :group 'wl-summary
  :group 'wl-pref)

(defcustom wl-folder-use-frame nil
  "*Use dedicated frame for folder mode if non-nil."
  :type 'boolean
  :group 'wl-pref)

(defcustom wl-summary-use-frame nil
  "*Use dedicated frame for each folder summary if non-nil."
  :type 'boolean
  :group 'wl-pref)

(defcustom wl-stay-folder-window nil
  "*Stay folder window when folder is selected if non-nil."
  :type 'boolean
  :group 'wl-pref)

(defcustom wl-reply-subject-prefix "Re: "
  "*Prefix of the subject of the replied message.
The value is string or string valued function to be evalueted in the target
message buffer."
  :type '(choice string function)
  :group 'wl-draft
  :group 'wl-pref)

(defcustom wl-forward-subject-prefix "Forward: "
  "*Prefix of the subject of the forwarded message.
The value is string or string valued function to be evalueted in the target
message buffer."
  :type '(choice string function)
  :group 'wl-draft
  :group 'wl-pref)

(defcustom wl-draft-reply-use-address-with-full-name t
  "*Use address with full-name in the draft of replied message."
  :type 'boolean
  :group 'wl-pref
  :group 'wl-draft)

(defcustom wl-subject-re-prefix-regexp "^[ \t]*\\([Rr][Ee][:>][ \t]*\\)*[ \t]*"
  "*Regexp matching \"Re: \" in the subject line."
  :type 'regexp
  :group 'wl-draft
  :group 'wl-pref)

(defcustom wl-subject-forward-prefix-regexp "^[ \t]*\\(\\([Ff][Oo][Rr][Ww][Aa][Rr][Dd]\\|[Ff][Ww][Dd]\\|[Ff][Ww]\\)[:>][ \t]*\\)*[ \t]*"
  "*Regexp matching \"Forward: \", \"Fwd: \", or \"Fw: \" in the subject line."
  :type 'regexp
  :group 'wl-draft
  :group 'wl-pref)

(defcustom wl-folder-many-unsync-threshold 70
  "*Folders which contains messages more than this number are highlighted
with wl-highlight-folder-many-face."
  :type 'integer
  :group 'wl-folder
  :group 'wl-pref)

(defcustom wl-fcc nil
  "*Folder Carbon Copy target initially added at creating draft buffer."
  :type '(choice (const :tag "disable" nil)
		 string function)
  :group 'wl-draft
  :group 'wl-pref)

(defcustom wl-fcc-force-as-read nil
  "*If non-nil, mark copied message as read."
  :type 'boolean
  :group 'wl-draft
  :group 'wl-pref)

(defcustom wl-bcc nil
  "*Blind Carbon Copy target initially added at creating draft buffer."
  :type '(choice (const :tag "disable" nil)
		 string)
  :group 'wl-draft
  :group 'wl-pref)

(defcustom wl-folder-desktop-name "Desktop"
  "*An implicit name of the folder top entity."
  :type 'string
  :group 'wl-folder
  :group 'wl-pref)

(defcustom wl-summary-indent-length-limit 46
  "*Limit of indent length for thread. Nil means unlimited"
  :type '(choice (const :tag "Unlimited" nil)
		 integer)
  :group 'wl-summary
  :group 'wl-pref)

(defcustom wl-summary-max-thread-depth 30
  "*If thread depth of the message is larger than this value, divide it."
  :type '(choice (const :tag "Unlimited" nil)
		 integer)
  :group 'wl-summary
  :group 'wl-pref)

(defcustom wl-summary-no-from-message "nobody@nowhere?"
  "*A string displayed in summary when no from field exists."
  :type 'string
  :group 'wl-summary)

(defcustom wl-summary-no-subject-message "(WL:No Subject in original.)"
  "*A string displayed in summary when no subject field exists."
  :type 'string
  :group 'wl-summary)

(defcustom wl-summary-cancel-message "I'd like to cancel my message."
  "*The body content of a cancel message."
  :type 'string
  :group 'wl-summary)

(defcustom wl-summary-width 80
  "*Set summary line width if non nil."
  :type '(choice (const :tag "Don't truncate" nil)
		 integer)
  :group 'wl-summary
  :group 'wl-pref)

(defcustom wl-summary-print-argument-within-window nil
  "*If non-nil, always print argument right side of window."
  :type 'boolean
  :group 'wl-summary
  :group 'wl-pref)

(defcustom wl-summary-pick-field-default "Body"
  "*Default field for pick."
  :type '(radio (const "From")
		(const "Subject")
		(const "To")
		(const "Cc")
		(const "Body")
		(const "Raw-Body")
		(const "Since")
		(const "Before")
		(const "Last")
		(const "First")
		(string :tag "Other"))
  :group 'wl-summary)

(defcustom wl-mime-charset (if wl-on-mule 'x-ctext 'iso-8859-1)
  "*MIME Charset for summary and message."
  :type 'symbol
  :group 'wl-summary
  :group 'wl-pref)

(defcustom wl-generate-mailer-string-function 'wl-generate-user-agent-string
  "A function for creating User-Agent field string."
  :type 'function
  :group 'wl-draft)

(defcustom wl-highlight-background-mode  (if (boundp 'hilit-background-mode)
					     (or hilit-background-mode 'dark)
					   'dark)
  "*Background mode of highlight (for Old Emacsen).  'dark or 'light."
  :type '(radio (const dark)
		(const light))
  :group 'wl-highlight)

(defcustom wl-highlight-x-face-function nil
  "A function to display X-Face."
  :type 'function
  :group 'wl-highlight)

(defcustom wl-qmail-inject-program "/var/qmail/bin/qmail-inject"
  "Location of the qmail-inject program."
  :type '(string :tag "Program")
  :group 'wl-draft)

(defcustom wl-qmail-inject-args nil
  "Arguments passed to qmail-inject programs.
This should be a list of strings, one string for each argument.

For e.g., if you wish to set the envelope sender address so that bounces
go to the right place or to deal with listserv's usage of that address, you
might set this variable to '(\"-f\" \"you@some.where\")."
  :type '(repeat (string :tag "Argument"))
  :group 'wl-draft)

(defcustom wl-rejected-letter-start
  "^[\t ]*-+[\t ]+\\(\\(original\\|\\(\\(the \\)?unsent\\)\\) message\\( follows\\)?[\t ]+-+[\t ]*\\|Below this line is a copy of the message\\..*\\)$"
  "Regexp specifying the beginning of the wrapper around a returned letter.
This wrapper is generated by the mail system when rejecting a letter."
  :type 'regexp
  :group 'wl-draft)

(defcustom wl-ignored-forwarded-headers "\\(received\\|return-path\\|x-uidl\\)"
  "*All headers that match this regexp will be deleted when forwarding a message."
  :type 'regexp
  :group 'wl-draft)

(defcustom wl-ignored-resent-headers "\\(return-receipt\\|[bdf]cc\\)"
  "*All headers that match this regexp will be deleted when resending a message."
  :type 'regexp
  :group 'wl-draft)

(defcustom wl-auto-save-drafts-interval 1
  "Idle interval in seconds to save draft buffers automatically.
If you don't want to use this feature, set this to nil."
  :type '(choice (const :tag "Don't use this feature" nil)
		 (number :tag "Secs"))
  :group 'wl-draft)

(defcustom wl-draft-preview-attributes t
  "Non-nil forces to preview the attributes in the `wl-draft-preview-message'.
Attributes specified in the `wl-draft-preview-attributes-list' are displayed."
  :type 'boolean
  :group 'wl-draft)

(defcustom wl-draft-preview-attributes-list '((mail recipients
						    envelope-from
						    send-mail-method
						    smtp-settings
						    pop-before-smtp-settings
						    pgp-processings)
					      (news newsgroups
						    nntp-method
						    nntp-settings
						    pgp-processings))
  "*Attribute symbols to display in the draft preview.
Candidates are following:
`recipients'
`envelope-from'
`send-mail-method'
`smtp-posting-server'
`smtp-posting-port'
`smtp-settings'
`pop-before-smtp-settings'
`newsgroups'
`nntp-posting-server'
`nntp-posting-port'
Also variables which begin with `wl-' can be specified
\(`wl-' have to be removed\)"
  :type '(choice (repeat (cons (choice (const :tag "Mail" mail)
				       (const :tag "News" news))
			       (repeat symbol)))
		 (repeat symbol))
  :group 'wl-draft)

(defcustom wl-draft-preview-attributes-buffer-lines t
  "*Buffer height for the draft attribute preview.  Non-integer means decide height from number of attributes automatically.  Negative-integer means add absolute value to automated height."
  :type '(choice integer
		 (const :tag "Automated" t))
  :group 'wl-draft)

(defcustom wl-draft-preview-attributes-buffer-name "*Preview Attributes*"
  "*Buffer name for the draft attribute preview."
  :type 'string
  :group 'wl-draft)

(defcustom wl-refile-default-from-folder "+from"
  "*Folder name to refile by `wl-refile-guess-by-from'."
  :type '(string :tag "Folder")
  :group 'wl-pref)

(defcustom wl-summary-auto-refile-skip-marks
  (list wl-summary-new-uncached-mark
	wl-summary-new-cached-mark
	wl-summary-unread-uncached-mark
	wl-summary-unread-cached-mark)
  "Persistent marks to skip auto-refiling."
  :type '(repeat (string :tag "Mark"))
  :group 'wl-summary)

(defcustom wl-summary-reserve-mark-list
  (list "o" "O" "D" "d" "i")
  "If a message is already marked as temporal marks in this list,
the message is not marked by any mark command."
  :type '(repeat (string :tag "Temp-Mark"))
  :group 'wl-summary)

(defcustom wl-summary-skip-mark-list
  (list "D" "d")
  "If a message is already marked as temporal marks in this list,
the message is skipped at cursor move."
  :type '(repeat (string :tag "Temp-Mark"))
  :group 'wl-summary)

(defcustom wl-summary-incorporate-marks
  (list wl-summary-new-uncached-mark
	wl-summary-unread-uncached-mark)
  "Persistent marks to prefetch at `wl-summary-incorporate'."
  :type '(repeat (string :tag "Mark"))
  :group 'wl-summary)

(defcustom wl-refile-rule-alist nil
  "Refile rule alist.
e.x.
'((\"From\"
   (\"teranisi@isl.ntt.co.jp\" . \"+teranisi\"))
  (\"x-ml-name\"
   (\"^Wanderlust\"    . \"+wl\")
   (\"^Elips\" . \"+elips\")))"
  :type '(repeat (list (string :tag "Field")
		       (repeat :inline t
			       (cons (regexp :tag "Value")
				     (string :tag "Folder")))))
  :group 'wl-pref)

(defcustom wl-strict-diff-folders nil
  "List of regexps matching folders of which Wanderlust seriously counts unsync messages."
  :type '(choice (const :tag "Off" nil)
		 (repeat (regexp :tag "Folder Regexp")))
  :group 'wl-folder)

(defcustom wl-folder-use-server-diff t
  "Checked unread message number on IMAP4 server.
Only IMAP4 folders have an effect."
  :type 'boolean
  :group 'wl-folder)

(defcustom wl-force-fetch-folders nil
  "Non-nil forces to fetch subfolders when user opened an 'access' folder."
  :type '(choice (const :tag "off" nil)
		 (const :menu-tag "on" t)
		 (repeat (regexp :tag "Folder Regexp")))
  :group 'wl-folder)

(defcustom wl-auto-check-folder-name nil
  "*A folder, a group or a list of folders and groups specified which
will be automatically checked at the startup time."
  :type '(choice (string :tag "Folder")
		 (repeat (string :tag "Folder"))
		 (const none))
  :group 'wl-folder)

(defcustom wl-auto-uncheck-folder-list '("\\$.*")
  "All folders that match this list won't be checked at the startup
time even if they are embedded in some groups specified by
wl-auto-check-folder-name.
Those folders are also skipped when you check on the Desktop.
This value is preceded by wl-auto-check-folder-list.
Each elements are regexp of folder name."
  :type '(repeat (regexp :tag "Folder Regexp"))
  :group 'wl-folder)

(defcustom wl-auto-check-folder-list nil
  "A list of patterns for exceptional folders against
wl-auto-uncheck-folder-list.
Each elements are regexp of folder name."
  :type '(repeat (regexp :tag "Folder Regexp"))
  :group 'wl-folder)

(defcustom wl-show-plug-status-on-modeline t
  "If it is non-nil, show plugged status in modeline."
  :type 'boolean
  :group 'wl-setting)

(defcustom wl-plug-state-indicator-on  " [ON] "
  "String used to show plugged status ON."
  :type 'string
  :group 'wl-setting)

(defcustom wl-plug-state-indicator-off " [--] "
  "String used to show plugged status OFF."
  :type 'string
  :group 'wl-setting)

(defcustom wl-biff-check-folder-list nil
  "All folders that include this list are automatically checked
every intervals specified by `wl-biff-check-interval'."
  :type '(repeat (string :tag "Folder"))
  :group 'wl-setting)

(defcustom wl-biff-check-interval 40
  "Number of seconds between updates of new mails in the mode line."
  :type 'integer
  :group 'wl-setting)

(defcustom wl-biff-use-idle-timer nil
  "Non-nil to use idle timer instead of strict timer for wl-biff"
  :type 'boolean
  :group 'wl-setting)

(defcustom wl-biff-state-indicator-on (if (and (featurep 'xemacs)
					       (not (featurep 'mule)))
					  "[Mail]"
					(decode-coding-string
					 ;; Youbin mark
					 (read "\"[\e$B\\\")\e(B]\"")
					 (if (boundp 'MULE)
					     '*iso-2022-jp*
					   'iso-2022-jp)))
  "String used to show biff status ON."
  :type 'string
  :group 'wl-setting)

(defcustom wl-biff-state-indicator-off (if (and (featurep 'xemacs)
						(not (featurep 'mule)))
					   "[--]"
					  ;; Japanese short hyphen
					 "[‐]")
  "String used to show biff status OFF."
  :type 'string
  :group 'wl-setting)

(defcustom wl-mode-line-display-priority-list '(biff plug title)
  "Displaying order of items to be shown in modeline.  The first item will
be placed in the leftmost.  The significant items are `biff' and `plug';
otherwise, e.g. `title', corresponds to the things except for the biff
staus nor the plugged status.  The default order is '(biff plug title)
even if the value of this option is set to nil.  Here are some samples:

;; Plugged status first:
\(setq wl-mode-line-display-priority-list '(plug))

;; Biff status, Title of Wanderlust, Plugged status:
\(setq wl-mode-line-display-priority-list '(biff title plug))

"
  :type '(repeat (radio (const :format "%v " biff)
			(const :format "%v " plug)
			(sexp :tag "Other" :value title)))
  :group 'wl-setting)

(defcustom wl-interactive-send t
  "*If non-nil, require your confirmation when sending draft message."
  :type 'boolean
  :group 'wl-pref
  :group 'wl-setting)

(defcustom wl-interactive-exit t
  "*If non-nil, require your confirmation when exiting WL."
  :type 'boolean
  :group 'wl-pref)

(defcustom wl-summary-move-order 'unread
  "*The order of priority when move in summary mode.
If this variable is `unread', precede \"U\", \"u\", \"N\", \"n\" mark.
If this variable is `new', precede \"N\", \"n\" mark."
  :type '(radio (const new)
		(const unread))
  :group 'wl-summary
  :group 'wl-setting)

(defvar wl-summary-move-direction-downward t)

(defcustom wl-summary-move-direction-toggle t
  "*If non-nil, search direction for the next message will be determined
depends on previous search direction.
It uses wl-summary-move-direction-downward as a direction flag."
  :type 'boolean
  :group 'wl-summary)

(defcustom wl-auto-select-first nil
  "*If non-nil, display selected first message when enter summary."
  :type 'boolean
  :group 'wl-pref
  :group 'wl-setting)

(defcustom wl-auto-prefetch-first nil
  "*If non-nil, prefetch selected first message when enter summary."
  :type 'boolean
  :group 'wl-pref
  :group 'wl-setting)

(defcustom wl-auto-select-next nil
  "*If non-nil, offer to go to the next folder from the end of the previous.
If the value is the symbol `unread', go to the next folder
that no unread message exists.  If the value is the symbol `skip-no-unread',
skip the folder that no unread message exists.

See also variable `wl-summary-next-no-unread-command'."
  :type '(radio (const :tag "off" nil)
		(const :tag "on" t)
		(const unread)
		(const skip-no-unread))
  :group 'wl-pref
  :group 'wl-setting)

(defcustom wl-message-popup-buffers '(mime-echo-buffer-name epa-info-buffer)
  "*List of buffer or name which is popped up with message buffer."
  :type '(repeat (choice (symbol :tag "Variable")
			 (string :tag "Buffer name")))
  :group 'wl-setting)

(defcustom wl-message-buffer-name " *WL:Message*"
  "*Buffer name for message buffers."
  :group 'wl-pref
  :group 'wl-setting)

(defcustom wl-message-buffer-prefetch-folder-type-list '(imap4 nntp)
  "*All folder types that match this list prefetch next message,
and reserved buffer cache."
  :type `(choice (const :tag "all" t)
		 (const :tag "never" nil)
		 (set (const localdir)
		      (const localnews)
		      (const maildir)
		      (const imap4)
		      (const nntp)
		      (const pop3)
		      (const shimbun)
		      (const search)
		      (const archive)
		      (const mark)
		      (const cache)))
  :group 'wl-pref)

(defcustom wl-message-buffer-prefetch-folder-list nil
  "*All folders that match this list prefetch next message,
and reserved buffer cache.
e.x.
'(\"^[-%]\")"
  :type '(repeat (regexp :tag "Folder Regexp"))
  :group 'wl-pref)

(defcustom wl-message-buffer-prefetch-depth 1
  "*Depth of buffer prefetch in summary mode."
  :type 'integer
  :group 'wl-pref)

(defcustom wl-message-buffer-prefetch-idle-time 1
  "*Idle time of buffer prefetch."
  :type 'number
  :group 'wl-pref)

(defcustom wl-message-buffer-prefetch-threshold 30000
  "*Quit forward cache prefetching if message size is larger than this value."
  :type 'integer
  :group 'wl-pref)

(defcustom wl-summary-always-sticky-folder-list nil
  "All folders that match this list has sticky summary.
Each elements are regexp of folder name."
  :type '(radio (const :tag "none" nil)
		(const :tag "all" t)
		(repeat (regexp :tag "Folder Regexp")))
  :group 'wl-pref)

(defcustom wl-summary-force-prefetch-folder-list nil
    "All folders that match this list are prefetched.
Each elements are regexp of folder name."
    :type '(radio (const :tag "none" nil)
		  (const :tag "all" t)
		  (repeat (regexp :tag "Folder Regexp")))
    :group 'wl-pref)

(defcustom wl-no-save-folder-list '("^/.*$" "^\\[.*$")
  "All folders that match this list won't save its msgdb.
Each elements are regexp of folder name."
  :type '(repeat (regexp :tag "Folder Regexp"))
  :group 'wl-pref)

(defcustom wl-save-folder-list nil
  "All folders that match this list save its msgdb.
Each elements are regexp of folder name."
  :type '(repeat (regexp :tag "Folder Regexp"))
  :group 'wl-pref)

(defcustom wl-folder-mime-charset-alist
  '(("^-alt\\.chinese" . big5)
    ("^-relcom\\." . koi8-r)
    ("^-tw\\." . big5)
    ("^-han\\." . euc-kr)
    ("@sponichi" . shift_jis)
    ("@2ch" . shift_jis))
  "Charset alist.  If no match, `wl-mime-charset' is used."
  :type '(repeat (cons (regexp :tag "Folder Regexp") (symbol :tag "Charset")))
  :group 'wl-summary
  :group 'wl-pref)

(defcustom wl-folder-weekday-name-lang-alist
  '(("^-alt\\.chinese" . "en")
    ("^-relcom\\." . "en")
    ("^-tw\\." . "en")
    ("^-han\\." . "en"))
  "Weekday name lang alist.
If no match, `wl-summary-weekday-name-lang' is used.
e.x.
'((\"xemacs-beta$\" . \"en\")
  (\"^-fj\" . \"ja\"))"
  :type '(repeat (cons (regexp :tag "Folder Regexp")
		       (choice (const "ja")
			       (const "en")
			       (const "fr")
			       (const "de")
			       (string :tag "Other"))))
  :group 'wl-pref)

(defcustom wl-folder-thread-indent-set-alist
  '(("^-alt\\.chinese" . (2 "+" "+" "|" "-" " "))
    ("^-relcom\\." . (2 "+" "+" "|" "-" " "))
    ("^-tw\\." . (2 "+" "+" "|" "-" " "))
    ("^-han\\." . (2 "+" "+" "|" "-" " ")))
  "Thread indent set alist.
If no match, following indent set is used.
\(wl-thread-indent-level
 wl-thread-have-younger-brother-str
 wl-thread-youngest-child-str
 wl-thread-vertical-str
 wl-thread-horizontal-str
 wl-thread-space-str)
e.x.
'((\"xemacs-beta$\" . (2 \"+\" \"+\" \"|\" \"-\" \" \")))"
  :type '(repeat (cons (regexp :tag "Folder Regexp")
		       (group (integer :tag "Indent")
			      (string :tag "Yonger Brother")
			      (string :tag "Yonger Child")
			      (string :tag "Vertical")
			      (string :tag "Horizontal")
			      (string :tag "Space"))))
  :group 'wl-pref)

(defcustom wl-folder-sync-range-alist
  (list (cons 'wl-require-update-all-folder-p "all"))
  "*Default sync range alist.  If no matches, `wl-default-sync-range' is used."
  :type '(repeat (cons (choice (regexp :tag "Folder Regexp")
			       (symbol :tag "A function"))
		       (choice (const "update")
			       (const "all")
			       (const "rescan")
			       (const "no-sync")
			       (const :tag "none" nil))))
  :group 'wl-pref)

(defcustom wl-default-sync-range  "update"
  "*Default sync range."
  :type '(choice (const "update")
		 (const "all")
		 (const "rescan")
		 (const "no-sync")
		 (const :tag "none" nil))
  :group 'wl-pref)

(defcustom wl-ask-range t
  "*If non-nil, ask for a range for summary synchronization.
If nil, always use default."
  :type 'boolean
  :group 'wl-pref)

(defcustom wl-folder-process-duplicates-alist nil
  "Specify process type of duplicated messages.
It should be a list of cons cell like: (REGEXP . TYPE)
REGEXP is a regular expression string of folder name.
TYPE is one of the symbols `hide' or `read'.
`hide' means hide duplicated messages.
`read' means mark as read duplicated messages.
If TYPE is nil, do nothing for duplicated messages."
  :type '(repeat (cons (regexp :tag "Folder regexp")
		       (choice (const :tag "Hide" hide)
			       (const :tag "Mark as read" read))))
  :group 'wl-folder)

(defcustom wl-folder-move-cur-folder nil
  "*Non-nil, move cursor to current folder on folder buffer when goto folder."
  :type 'boolean
  :group 'wl-folder)

(defcustom wl-folder-check-async t
  "*Check the folder asynchronous."
  :type 'boolean
  :group 'wl-folder)

(defcustom wl-folder-notify-deleted nil
  "*Non-nil, display negative number on folder-mode when message is deleted
in folder. If the value is 'sync, msgdb would be synchronized."
  :type '(choice (const :tag "off" nil)
		 (const :tag "on" t)
		 (const sync))
  :group 'wl-folder)

(defcustom wl-summary-exit-next-move t
  "*Non-nil, move to next-unsync or next-entity when exit summary."
  :type 'boolean
  :group 'wl-summary)

(defcustom wl-summary-next-no-unread-command
  '(wl-summary-read wl-summary-down wl-summary-up)
  "*Command list available when the value of `wl-auto-select-next' is 'unread
or 'skip-no-unread."
  :type '(repeat function)
  :group 'wl-summary)

(defcustom wl-summary-search-via-nntp 'confirm
  "*Non-nil, search message via nntp after `wl-summary-jump-to-msg-by-message-id'.
If the value is 'confirm, confirm before search, 'force to search via nntp
regardless of current folder type."
  :type '(choice (const :tag "confirm" confirm)
		 (const :tag "always" force)
		 (const :tag "in nntp folder" t)
		 (const :tag "never" nil))
  :group 'wl-summary)

(defcustom wl-summary-keep-cursor-command
  '(wl-summary-goto-folder wl-summary-goto-last-visited-folder)
  "*Command list to keep cursor position when folder is changed to
already existing summary."
  :type '(repeat function)
  :group 'wl-summary)

(defcustom wl-summary-showto-folder-regexp nil
  "Regexp specifying the folder that shows the To (or Newsgroups) field as
Sender information in summary mode. It is effective when the value of
`wl-summary-from-function' is `wl-summary-default-from'"
  :type '(choice (const :tag "none" nil)
		 regexp)
  :group 'wl-summary)

(defcustom wl-summary-save-file-suffix ".eml"
  "Suffix for the saved file name."
  :type 'string
  :group 'wl-summary)

(defcustom wl-summary-resend-use-cache nil
  "*Non-nil to enable offline resending by using file cache.
Note that strict message identity is not guaranteed when cache is used."
  :type 'boolean
  :group 'wl-summary)

(defcustom wl-folder-removed-mark "#<removed>"
  "Mark for removed folder."
  :type 'string
  :group 'wl-folder)

(defcustom wl-folder-unsubscribe-mark "#"
  "Mark for unsubscribe folder."
  :type 'string
  :group 'wl-folder)

(defcustom wl-dispose-folder-alist '(("^-" . remove)
				     ("^@" . remove))
  "*Alist of folder and dispose policy.
Each element is (folder-regexp . policy).

The policy is one of the followings:
'remove or
'null     : remove message.
string    : refile to the specified folder.
'trash or
otherwise : refile to the `wl-trash-folder'.
ex.
'((\"^%\" . \"%#mh/trash\")
  (\"^-\" . remove)
  (\"^\\\\+\" . trash))"
  :type '(repeat (cons (regexp :tag "Folder Regexp")
		       (choice :tag "Policy"
			       (const remove)
			       (const :tag "remove(null)" null)
			       (const trash)
			       (const :tag "trash(other)" trash)
			       (string :tag "Folder"))))
  :group 'wl-folder)

(defcustom wl-folder-hierarchy-access-folders '("^-[^.]*\\(:\\|@\\|$\\)"
						"^@$" "^'$")
  "*Access group REGEXPs to make hierarchy structure."
  :type '(repeat (string :tag "Regexp"))
  :group 'wl-folder)

(defcustom wl-folder-init-load-access-folders nil
  "*Access group folders to load folder list on `wl-folder-init'.
If this variable is non-nil,
`wl-folder-init-no-load-access-folders' will be ignored."
  :type '(repeat (regexp :tag "Folder Regexp"))
  :group 'wl-folder)

(defcustom wl-folder-init-no-load-access-folders nil
  "*Access group folders to not load folder list on `wl-folder-init'.
If `wl-folder-init-load-access-folders' is non-nil,
this variable will be ignored."
  :type '(repeat (regexp :tag "Folder Regexp"))
  :group 'wl-folder)

(defcustom wl-folder-access-subscribe-alist nil
  "*Subscribe folders to fetching folder entries.
Each element is (group-regexp (subscribe folder-regexp ...)).
If subscribe is non-nil, subscribe when match folder-regexp.
If subscribe is nil, unsubscribe when match folder-regexp.

ex.
'((\"^-fj$\"   . (t   \"^-fj\\\\.\\\\(editor\\\\|mail\\\\|net\\\\|news\\\\)\"))
  (\"^-comp$\" . (t   \"^-comp\\\\.unix\" \"^-comp\\\\.sys\"))
  (\"^-$\"     . (nil \"^-alt\" \"^-rec\")))"
  :type '(repeat (cons (regexp :tag "Folder Regexp")
		       (list (boolean :tag "Subscribed")
			     (repeat :inline t
				     (regexp :tag "Folder Regexp")))))
  :group 'wl-folder)

;;; For Folder Manager

(defcustom wl-interactive-save-folders t
  "*Non-nil require your confirmation when save folders."
  :type 'boolean
  :group 'wl-folder
  :group 'wl-setting)

(defcustom wl-fldmgr-make-backup t
  "*Non-nil make backup file when save folders."
  :type 'boolean
  :group 'wl-folder)

(defcustom wl-fldmgr-folders-indent "\t"
  "*Indent string for folders file."
  :type 'string
  :group 'wl-folder)

(defcustom wl-fldmgr-sort-function 'wl-fldmgr-sort-standard
  "*A function to sort folder."
  :type 'function
  :group 'wl-folder)

(defcustom wl-fldmgr-sort-group-first t
  "*Non-nil Group folder is first when sort."
  :type 'function
  :group 'wl-folder)

(defcustom wl-fldmgr-add-complete-with-current-folder-list nil
  "*If non-nil, completion for adding folder refers current folder list."
  :type 'boolean
  :group 'wl-folder)

(defcustom wl-fldmgr-make-filter-default "Body"
  "*Default filter key while making filter on Folder."
  :type '(radio (const "From")
		(const "Subject")
		(const "To")
		(const "Cc")
		(const "Body")
		(const "Since")
		(const "Before")
		(const "Last")
		(const "First")
		(string :tag "Other"))
  :group 'wl-folder)

(defcustom wl-fldmgr-allow-rename-access-group nil
  "*If non-nil, allow to rename folder in access group."
  :type 'boolean
  :group 'wl-folder)

;;; For Expire and Archive

(defcustom wl-expire-alist nil
  "Alist to decide a policy for expire.
Each element is (folder-regexp (number or date) policy).

The policy is one of the followings:
'remove  : remove messsage.
'trash   : refile `wl-trash-folder'.
string   : refile string folder.
function : call function.

ex.
'((\"^\\\\+ml/wl$\"		(number 500 510) wl-expire-archive-number1 t)
  (\"^\\\\+ml/\"		(number 300 305) wl-expire-archive-number2)
  (\"^\\\\+outbox$\"		(number 300) \"$outbox;lha\")
  (\"^\\\\(\\\\+tmp\\\\|\\\\+trash\\\\)$\"	(date 7) remove)
  (\"^\\\\+misc$\"		(date 14) trash))"
  :type '(repeat (choice (list :tag "No-match"
			       (regexp :tag "Folder Regexp")
			       (const nil))
			 (list :tag "Match"
			       (regexp :tag "Folder Regexp")
			       (list (radio :value number
					    (const number)
					    (const date))
				     (list :inline t
					   integer
					   (repeat :inline t integer)))
			       (choice :tag "Policy"
				       :value remove
				       (const remove)
				       (const trash)
				       (string :tag "folder")
				       function)
			       (repeat :inline t
				       :tag "Arg for function"
				       sexp))))
  :group 'wl-expire)

(defcustom wl-archive-alist '((".*" wl-archive-number1))
  "Alist to decide a policy for archive.
Each element is (folder-regexp policy(function)).

ex.
'((\"\\\\+work$\" wl-archive-date)
  (\"\\\\+ml/\"   wl-archive-number1)
  (\".*\"       wl-archive-number2))"
  :type '(repeat (list (regexp :tag "Folder Regexp")
		       function
		       (repeat :inline t
			       (sexp :tag "Argument"))))
  :group 'wl-expire)

(defcustom wl-summary-expire-reserve-marks
  (list wl-summary-flag-mark
	wl-summary-new-uncached-mark
	wl-summary-new-cached-mark
	wl-summary-unread-uncached-mark
	wl-summary-unread-cached-mark)
  "Permanent marks of reserved message when expire.
Don't reserve temporary mark message.

ex.
'all  : reserved all permanent marks.
'none : not reserve permanent marks.
list  : reserved specified permanent marks."
  :type '(repeat (string :tag "Mark"))
  :group 'wl-expire)

(defcustom wl-expire-number-with-reserve-marks nil
  "If non-nil, include reserve message when expire by number."
  :type 'boolean
  :group 'wl-expire)

(defcustom wl-expire-add-seen-list t
  "*If non-nil, add seen message list when refile message at expire."
  :type 'boolean
  :group 'wl-expire)

(defcustom wl-expire-use-log nil
  "*If non-nil, write a log when expired."
  :type 'boolean
  :group 'wl-expire)

(defcustom wl-expire-folder-update-msgdb t
  "*Non-nil update summary msgdb when expire on folder mode."
  :type 'boolean
  :group 'wl-expire)

;; for wl-expire-archive-{number1|number2}
(defcustom wl-expire-archive-files 100
  "*The number of one archive folder."
  :type 'integer
  :group 'wl-expire)

;; for wl-expire-archive-{number1|number2|date}
(defcustom wl-expire-archive-get-folder-function
  'wl-expire-archive-get-folder
  "*A function to get archive folder name."
  :type 'function
  :group 'wl-expire)

(defcustom wl-expire-delete-oldmsg-confirm t
  "*If non-nil, require your confirmation when delete old message."
  :type 'boolean
  :group 'wl-expire)

;; for wl-expire-archive-get-folder
(defcustom wl-expire-archive-folder-type 'zip
  "*Archiver type of archive folder."
  :type '(radio (const zip)
		(const lha)
		(const zoo)
		(const rar)
		(const tar)
		(const tgz)
		(symbol :tag "Other"))
  :group 'wl-expire)

(defcustom wl-expire-archive-folder-name-fmt "%s-%%05d;%s" ;; $folder-00100;zip
  "*A format string for archive folder name."
  :type 'string
  :group 'wl-expire)

(defcustom wl-expire-archive-folder-num-regexp "-\\([0-9]+\\);"
  "*A regexp string for archive folder name."
  :type 'string
  :group 'wl-expire)

(defcustom wl-expire-archive-date-folder-name-fmt "%s-%%04d%%02d;%s"
						;; $folder-199812;zip
  "*A format string for archive date folder name."
  :type 'string
  :group 'wl-expire)

(defcustom wl-expire-archive-date-folder-num-regexp "-\\([0-9]+\\);"
  "*A regexp string for archive date folder name."
  :type 'string
  :group 'wl-expire)

(defcustom wl-expire-archive-folder-prefix nil
  "*Prefix for archive folder."
  :type '(radio (const :tag "nothing" nil)
		(const :tag "full" t)
		(const short))
  :group 'wl-expire)

;;;; Highlights.

;; highilght about summary
(defcustom wl-highlight-max-summary-lines 10000
  "*If the summary is larger than this lines, don't highlight it."
  :type 'integer
  :group 'wl-highlight)

;; highilght about draft and message
(defcustom wl-highlight-body-too t
  "*In addition to header, highlight the body too.  if non nil."
  :type 'boolean
  :group 'wl-highlight)

(defcustom wl-highlight-message-header-alist
  '(("Subject[ \t]*:" . wl-highlight-message-important-header-contents)
    ("From[ \t]*:\\|To[ \t]*:" . wl-highlight-message-important-header-contents2)
    ("X-[^ \t]*:\\|User-Agent[ \t]*:" . wl-highlight-message-unimportant-header-contents))
  ""
  :type '(repeat (cons regexp face))
  :group 'wl-highlight)

(defcustom wl-highlight-citation-prefix-regexp
  "^[>|:} ]*[>|:}]\\([^ \n>]*>\\)?\\|^[^ <\n>]*>"
  "All lines that match this regexp will be highlighted with
 `wl-highlight-message-cited-text-*' face."
  :type 'regexp
  :group 'wl-highlight)

(defcustom wl-highlight-highlight-citation-too nil
  "*Whether the whole citation line should go in the
`wl-highlight-citation-face' face.
If nil, the text matched by `wl-highlight-citation-prefix-regexp' is in the
default face, and the remainder of the line is in the
wl-highlight-message-cited-text face."
  :type 'boolean
  :group 'wl-highlight)

(defcustom wl-highlight-force-citation-header-regexp
  "^>>>.*$\\|^[ \t]*<[^>]*>[ \t]*$"
  "*The pattern to match the prolog of a cited block.
Text in the body of a message which matches this will be displayed in
the `wl-highlight-message-headers' face."
  :type 'regexp
  :group 'wl-highlight)

(defcustom wl-highlight-citation-header-regexp
  (concat "In article.*$\\|In message.*$\\|In the message.*$\\|"
	  "^At[^\n]+\n[^\n]+wrote:\n\\|"
	  "^.*\\(writes\\|wrote\\|said\\):\n")
  "*The pattern to match the prolog of a cited block.
Text in the body of a message which matches this will be displayed in
the `wl-highlight-message-headers' face."
  :type 'regexp
  :group 'wl-highlight)

(defcustom wl-highlight-max-header-size nil
  "*If the message header is larger than this many chars, don't highlight it.
If this is nil, all headers will be highlighted."
  :type 'integer
  :group 'wl-highlight)

(defcustom wl-highlight-max-message-size 10000
  "*If the message body is larger than this many chars, don't highlight it.
This is to prevent us from wasting time trying to fontify things like
uuencoded files and large digests.  If this is nil, all messages will
be highlighted."
  :type 'integer
  :group 'wl-highlight)

;; highilght about signature (of draft and message)
(defcustom wl-highlight-signature-separator
  '("\n--+\n" "\n\n--+.*\n*\\'")
  "List of regexps matching signature separator.
It will be verified from head to tail looking for a separator.
Verification will be done from the end of the buffer.
No need to specify \"^-- $\" in this list,
because it is verified by default.
This variable can also be a regex."
  :type '(repeat regexp)
  :group 'wl-highlight)

(defcustom wl-max-signature-size 400
  "*If the signature is larger than this chars, don't treat it as a signature."
  :type 'integer
  :group 'wl-highlight)

;; highilght about mouse
(defcustom wl-use-highlight-mouse-line (and window-system
					    (>= emacs-major-version 19))
  "*Highlight mouse line, if non-nil."
  :type 'boolean
  :group 'wl-highlight)

(defcustom wl-highlight-summary-line-help-echo-alist
  '((flag wl-highlight-flag-folder-help-echo)
    (search elmo-message-file-name))
  "*Alist to display help-echo in summary buffer.
Each element is (folder-type handler(function)).
Handler take two arguments elmo-folder and message number and return string."
  :type 'boolean
  :group 'wl-highlight)

;; highilght about folder
(defcustom wl-highlight-folder-with-icon
  (or (and (featurep 'xemacs)
	   (featurep 'xpm))
      wl-on-emacs21)
  "*Highlight folder with icon(XEmacs or Emacs 21)."
  :type 'boolean
  :group 'wl-highlight)

(defcustom wl-highlight-folder-by-numbers t
  "Highlight folder lines by numbers.
If it is a number, only numbers will be highlighted."
  :type '(choice (const :tag "whole line" t)
		 (const :tag "only numbers" 1)
		 (const :tag "don't highlight" nil))
  :group 'wl-highlight)

(defcustom wl-highlight-signature-search-function 'wl-highlight-signature-search
  "Function to search signature area in the message body."
  :type 'function
  :group 'wl-highlight)

(defcustom wl-use-dnd (and wl-on-xemacs
			   (featurep 'dragdrop))
  "If Non-nil, support dragdrop feature in XEmacs."
  :type 'boolean
  :group 'wl-pref)

(defcustom wl-reset-plugged-alist t
  "*If non-nil, reset `elmo-plugged-alist' when startup."
  :type 'boolean
  :group 'wl-pref)

(defcustom wl-demo-display-logo (if (or (featurep 'xemacs)
					(module-installed-p 'image)
					(module-installed-p 'bitmap))
				    t)
  "If it is T, show graphic logo in the startup screen.  You can set it to
a symbol `bitmap', `xbm' or `xpm' in order to force the image format."
  :type '(radio (const :tag "Off" nil)
		(const :tag "On (any format)" t)
		(const xpm)
		(const xbm)
		(const :tag "bitmap (using BITMAP-MULE)" bitmap))
  :group 'wl-pref)

(defcustom wl-invalid-character-message "(WL:Invalid characters.)"
  "*A string displayed when invalid character exists."
  :type 'string
  :group 'wl-pref)

(defcustom wl-use-pgp-module
  (condition-case nil
      (progn
	(require 'epg-config)
	(epg-check-configuration (epg-configuration))
	'epg)
    (error 'pgg))
  "*Which PGG library to be used."
  :type '(choice (const :tag "EasyPG Library" epg)
		 (const :tag "PGG Library" pgg)
		 (const :tag "Don't use PGP" nil))
  :group 'wl-pref)

(defcustom wl-display-progress-threshold
  '((wl-folder-insert-entity . 100)
    (elmo-retrieve-message . 3000)
    (t . 20))
  "*Displaying progress message if number of total are more than this value."
  :type '(choice (const :tag "No display" nil)
		 (const :tag "No limitation" 0)
		 (integer :tag "For all")
		 (repeat :tag "Each label"
			 (cons (choice (const :tag "Default" t)
				       (symbol :tag "Label"))
			       (choice (const :tag "No display" nil)
				       (const :tag "No limitation" 0)
				       (integer :tag "Threshold")))))
  :group 'wl-pref)

(defcustom wl-display-progress-function #'wl-simple-display-progress
  "*A function to display progress message"
  :type '(choice (const :tag "No display" nil)
		 (function :tag "Function"))
  :group 'wl-pref)

;;; Internal variables
(defvar wl-init nil)

;; For disconnected operations.
(defvar wl-plugged-hook nil)
(defvar wl-unplugged-hook nil)
(defcustom wl-plugged t
  "*Plugged state at the startup.  Nil means off-line."
  :type 'boolean
  :group 'wl
  :group 'wl-setting)

;; Internal variables used to modeline identifiers.
(defvar wl-modeline-plug-status nil)
(defvar wl-modeline-plug-state-on wl-plug-state-indicator-on)
(defvar wl-modeline-plug-state-off wl-plug-state-indicator-off)
(defvar wl-modeline-biff-status nil)
(defvar wl-modeline-biff-state-on wl-biff-state-indicator-on)
(defvar wl-modeline-biff-state-off wl-biff-state-indicator-off)

;; Advanced thread view.
(defvar wl-thread-indent-level (if wl-on-mule 1 2)
  "*Indent level for thread.")
(defvar wl-thread-have-younger-brother-str (if wl-on-mule "┣" "+")
  "*A string for thread branch line.  It should contain one character.")
(defvar wl-thread-youngest-child-str       (if wl-on-mule "┗" "+")
  "*A string for thread branch line.  It should contain one character.")
(defvar wl-thread-vertical-str             (if wl-on-mule "┃" "|")
  "*A string for thread branch line.  It should contain one character.")
(defvar wl-thread-horizontal-str           (if wl-on-mule "━" "-")
  "*A string for thread branch line.  It should contain one character.")
(defvar wl-thread-space-str                (if wl-on-mule "　" " ")
  "*A string for thread branch line.  It should contain one character.")

;; folder icons. filename relative to wl-icon-directory
(defvar wl-opened-group-folder-icon "opened.xpm"
  "*Icon file for opened group folder.")
(defvar wl-closed-group-folder-icon "closed.xpm"
  "*Icon file for closed group folder.")
(defvar wl-nntp-folder-icon "news.xpm"
  "*Icon file for nntp folder.")
(defvar wl-imap-folder-icon "imap.xpm"
  "*Icon file for imap folder.")
(defvar wl-pop-folder-icon  "pop.xpm"
  "*Icon file for pop folder.")
(defvar wl-localdir-folder-icon "local.xpm"
  "*Icon file for localdir folder.")
(defvar wl-localnews-folder-icon "localnews.xpm"
  "*Icon file for localnews folder.")
(defvar wl-internal-folder-icon "internal.xpm"
  "*Icon file for internal folder.")
(defvar wl-multi-folder-icon "multi.xpm"
  "*Icon file for multi folder.")
(defvar wl-filter-folder-icon "filter.xpm"
  "*Icon file for filter folder.")
(defvar wl-archive-folder-icon "archive.xpm"
  "*Icon file for archive folder.")
(defvar wl-pipe-folder-icon "pipe.xpm"
  "*Icon file for pipe folder.")
(defvar wl-search-folder-icon "nmz.xpm"
  "*Icon file for search folder.")
(defvar wl-shimbun-folder-icon "shimbun.xpm"
  "*Icon file for shimbun folder.")
(defvar wl-file-folder-icon "file.xpm"
  "*Icon file for file folder.")
(defvar wl-maildir-folder-icon "maildir.xpm"
  "*Icon file for maildir folder.")
(defvar wl-access-folder-icon "access.xpm"
  "*Icon file for access folder.")
(defvar wl-empty-trash-folder-icon "trash-e.xpm"
  "*Icon file for emptied trash folder.")
(defvar wl-trash-folder-icon "trash.xpm"
  "*Icon file for trash folder.")
(defvar wl-draft-folder-icon "draft.xpm"
  "*Icon file for draft folder.")
(defvar wl-queue-folder-icon "queue.xpm"
  "*Icon file for queue folder.")
(defvar wl-plugged-icon "plugged.xpm"
  "*Icon file for plugged state.")
(defvar wl-unplugged-icon "unplugged.xpm"
  "*Icon file for unplugged state.")
(defvar wl-biff-mail-icon "letter.xpm"
  "*Icon file for mail existed state.")
(defvar wl-biff-nomail-icon "no-letter.xpm"
  "*Icon file for no mail existed state.")
(defvar wl-prog-uudecode "uudecode"
  "*uudecode program name.")
(defvar wl-prog-uudecode-arg nil
  "*Arguments for uudecode program.")
(defvar wl-prog-uudecode-no-stdout-option t
  "*If non-nil, uudecode program don't have option for output to stdout.")

;; plug
(defvar wl-plugged-plug-on "ON")
(defvar wl-plugged-plug-off "--")
(defvar wl-plugged-auto-off "**")
(defvar wl-plugged-server-indent 2)
(defvar wl-plugged-port-indent 4)
(defvar wl-plugged-queue-status-column 25)

;;;; Obsolete variables.

;; 2012-08-19
(elmo-define-obsolete-variable 'wl-message-id-use-wl-from
			       'wl-message-id-use-message-from)

;; 2005-01-23
(elmo-define-obsolete-variable 'wl-nmz-folder-icon
			       'wl-search-folder-icon)

;; 2003-11-05
(elmo-define-obsolete-variable 'wl-summary-new-mark
			       'wl-summary-new-uncached-mark)

;; 2003-07-15 delete -> dispose
(elmo-define-obsolete-variable 'wl-delete-folder-alist
			       'wl-dispose-folder-alist)

;; 2002-12-25
(elmo-define-obsolete-variable 'wl-draft-reply-myself-with-argument-list
			       'wl-draft-reply-with-argument-list)
(elmo-define-obsolete-variable 'wl-draft-reply-myself-without-argument-list
			       'wl-draft-reply-without-argument-list)

;; 2001-12-11: *-dir -> *-directory
(elmo-define-obsolete-variable 'wl-icon-dir
			       'wl-icon-directory)
(elmo-define-obsolete-variable 'wl-mime-save-dir
			       'wl-mime-save-directory)
(elmo-define-obsolete-variable 'wl-score-files-dir
			       'wl-score-files-directory)
(elmo-define-obsolete-variable 'wl-tmp-dir
			       'wl-temporary-file-directory)

;; 2001-12-10
(elmo-define-obsolete-variable 'wl-summary-update-confirm-threshold
			       'elmo-folder-update-threshold)
(elmo-define-obsolete-variable 'wl-fetch-confirm-threshold
			       'elmo-message-fetch-threshold)

(elmo-define-obsolete-variable 'wl-cache-prefetch-folder-type-list
			       'wl-message-buffer-prefetch-folder-type-list)
(elmo-define-obsolete-variable 'wl-cache-prefetch-folder-list
			       'wl-message-buffer-prefetch-folder-list)

;; 2001-02-27: *-func -> *-function
(elmo-define-obsolete-variable 'wl-summary-from-func
			       'wl-summary-from-function)
(elmo-define-obsolete-variable 'wl-summary-subject-func
			       'wl-summary-subject-function)
(elmo-define-obsolete-variable 'wl-summary-subject-filter-func
			       'wl-summary-subject-filter-function)
(elmo-define-obsolete-variable 'wl-draft-send-func
			       'wl-draft-send-function)
(elmo-define-obsolete-variable 'wl-draft-send-news-func
			       'wl-draft-send-news-function)
(elmo-define-obsolete-variable 'wl-draft-send-mail-func
			       'wl-draft-send-mail-function)
(elmo-define-obsolete-variable 'wl-print-buffer-func
			       'wl-print-buffer-function)
(elmo-define-obsolete-variable 'wl-ps-print-buffer-func
			       'wl-ps-print-buffer-function)
(elmo-define-obsolete-variable 'wl-generate-mailer-string-func
			       'wl-generate-mailer-string-function)
(elmo-define-obsolete-variable 'wl-highlight-x-face-func
			       'wl-highlight-x-face-function)
(elmo-define-obsolete-variable 'wl-fldmgr-sort-func
			       'wl-fldmgr-sort-function)
(elmo-define-obsolete-variable 'wl-expire-archive-get-folder-func
			       'wl-expire-archive-get-folder-function)
(elmo-define-obsolete-variable 'wl-highlight-signature-search-func
			       'wl-highlight-signature-search-function)

;; 2000-01-25: temp mark -> target mark
(elmo-define-obsolete-variable 'wl-summary-temp-above
			       'wl-summary-target-above)

;; 1999-11-07: Unified with `wl-draft-config-alist'.
(defvar wl-draft-prepared-config-alist nil)
(make-obsolete-variable 'wl-draft-prepared-config-alist
			'wl-draft-config-alist)

;; 1999-10-10
(elmo-define-obsolete-variable 'wl-address-filename
			       'wl-address-file)
(elmo-define-obsolete-variable 'wl-score-default-file-name
			       'wl-score-default-file)


(require 'product)
(product-provide (provide 'wl-vars) (require 'wl-version))

;;; wl-vars.el ends here
