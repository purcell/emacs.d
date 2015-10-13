;; mu4e-draft.el -- part of mu4e, the mu mail user agent for emacs
;;
;; Copyright (C) 2011-2012 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; In this file, various functions to create draft messages

;; Code

(eval-when-compile (byte-compile-disable-warning 'cl-functions))
(require 'cl)

(require 'mu4e-vars)
(require 'mu4e-utils)
(require 'mu4e-message)
(require 'message) ;; mail-header-separator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom mu4e-compose-dont-reply-to-self nil
  "If non-nil, don't include self (that is, any member of
`mu4e-user-mail-address-list') in replies."
  :type 'boolean
  :group 'mu4e-compose)

(defcustom mu4e-compose-cite-function
  (or message-cite-function 'message-cite-original-without-signature)
  "The function to use to cite message in replies and forwarded
messages. This is the mu4e-specific version of
`message-cite-function'."
  :type 'function
  :group 'mu4e-compose)

(defcustom mu4e-compose-signature
  (or (and (stringp message-signature) message-signature)
    "Sent with my mu4e")
  "The message signature (i.e. the blob at the bottom of
messages). This is the mu4e-specific version of
`message-signature'."
  :type 'sexp
  :group 'mu4e-compose)

(defcustom mu4e-compose-signature-auto-include t
  "Whether to automatically include a message-signature in new
messages (if it is set)."
  :type 'boolean
  :group 'mu4e-compose)

(defcustom mu4e-compose-auto-include-date nil
  "Whether to include a date header when starting to draft a
message; if nil, only do so when sending the message."
  :type 'boolean
  :group 'mu4e-compose)

(defcustom mu4e-compose-in-new-frame nil
  "Whether to compose messages in a new frame instead of the
current window."
  :type 'boolean
  :group 'mu4e-compose)

(defun mu4e~draft-user-agent-construct ()
  "Return the User-Agent string for mu4e.
This is either the value of `mu4e-user-agent', or, if not set, a
string based on the versions of mu4e and emacs."
  (format "mu4e %s; emacs %s" mu4e-mu-version emacs-version))

(defun mu4e~draft-cite-original (msg)
  "Return a cited version of the original message MSG as a plist.
This function uses gnus' `mu4e-compose-cite-function', and as such
all its settings apply."
  (with-temp-buffer
    (when (fboundp 'mu4e-view-message-text) ;; keep bytecompiler happy
      (let ((mu4e-view-date-format "%Y-%m-%dT%T%z"))
	(insert (mu4e-view-message-text msg)))
      (message-yank-original)
      (goto-char (point-min))
      (push-mark (point-max))
      ;; set the the signature separator to 'loose', since in the real world,
      ;; many message don't follow the standard...
      (let ((message-signature-separator "^-- *$")
	     (message-signature-insert-empty-line t))
	(funcall mu4e-compose-cite-function))
      (pop-mark)
      (goto-char (point-min))
      (mu4e~fontify-cited)
      (buffer-string))))

(defun mu4e~draft-header (hdr val)
  "Return a header line of the form \"HDR: VAL\".
If VAL is nil, return nil."
  ;; note: the propertize here is currently useless, since gnus sets its own
  ;; later.
  (when val (format "%s: %s\n"
	      (propertize hdr 'face 'mu4e-header-key-face)
	      (propertize val 'face 'mu4e-header-val-face))))

(defun mu4e~draft-references-construct (msg)
  "Construct the value of the References: header based on MSG as a
comma-separated string. Normally, this the concatenation of the
existing References + In-Reply-To (which may be empty, an note
that :references includes the old in-reply-to as well) and the
message-id. If the message-id is empty, returns the old
References. If both are empty, return nil."
  (let* ( ;; these are the ones from the message being replied to / forwarded
	  (refs (mu4e-message-field msg :references))
	  (msgid (mu4e-message-field msg :message-id))
	  ;; now, append in
	  (refs (if (and msgid (not (string= msgid "")))
		  (append refs (list msgid)) refs))
	  ;; no doubles
	  (refs (delete-duplicates refs :test #'equal)))
    (mapconcat (lambda (id) (format "<%s>" id)) refs " ")))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; determine the recipient fields for new messages

(defun mu4e~draft-recipients-list-to-string (lst)
  "Convert a lst LST of address cells into a string with a list of
e-mail addresses. If LST is nil, returns nil."
  (when lst
    (mapconcat
      (lambda (addrcell)
	(let ((name (car addrcell))
	       (email (cdr addrcell)))
	  (if name
	    (format "%s <%s>" (mu4e~rfc822-quoteit name) email)
	    (format "%s" email))))
      lst ", ")))

(defun mu4e~draft-address-cell-equal (cell1 cell2)
  "Return t if CELL1 and CELL2 have the same e-mail address.
The comparison is done case-insensitively. If the cells done
match return nil. CELL1 and CELL2 are cons cells of the
form (NAME . EMAIL)."
  (string=
    (downcase (or (cdr cell1) ""))
    (downcase (or (cdr cell2) ""))))


(defun mu4e~draft-create-to-lst (origmsg)
  "Create a list of address for the To: in a new message, based on
the original message ORIGMSG. If the Reply-To address is set, use
that, otherwise use the From address. Note, whatever was in the To:
field before, goes to the Cc:-list (if we're doing a reply-to-all).
Special case: if we were the sender of the original, we simple copy
the list form the original."
  (let ((reply-to
	  (or (plist-get origmsg :reply-to) (plist-get origmsg :from))))
    (delete-duplicates reply-to :test #'mu4e~draft-address-cell-equal)
    (if mu4e-compose-dont-reply-to-self
      (delete-if
	(lambda (to-cell)
	  (member-if
	    (lambda (addr)
	      (string= (downcase addr) (downcase (cdr to-cell))))
	    mu4e-user-mail-address-list))
	reply-to)
      reply-to)))


(defun mu4e~draft-create-cc-lst (origmsg reply-all)
  "Create a list of address for the Cc: in a new message, based on
the original message ORIGMSG, and whether it's a reply-all."
  (when reply-all
    (let* ((cc-lst ;; get the cc-field from the original, remove dups
	     (delete-duplicates
	       (append
		 (plist-get origmsg :to)
		 (plist-get origmsg :cc))
	       :test #'mu4e~draft-address-cell-equal))
	    ;; now we have the basic list, but we must remove
	    ;; addresses also in the to list
	    (cc-lst
	      (delete-if
		(lambda (cc-cell)
		  (find-if
		    (lambda (to-cell)
		      (mu4e~draft-address-cell-equal cc-cell to-cell))
		    (mu4e~draft-create-to-lst origmsg)))
		cc-lst))
	    ;; finally, we need to remove ourselves from the cc-list
	    ;; unless mu4e-compose-keep-self-cc is non-nil
	    (cc-lst
	      (if (or mu4e-compose-keep-self-cc (null user-mail-address))
		cc-lst
		(delete-if
		  (lambda (cc-cell)
		    (member-if
		      (lambda (addr)
			(string= (downcase addr) (downcase (cdr cc-cell))))
		      mu4e-user-mail-address-list))
		  cc-lst))))
      cc-lst)))

(defun mu4e~draft-recipients-construct (field origmsg &optional reply-all)
  "Create value (a string) for the recipient field FIELD (a
symbol, :to or :cc), based on the original message ORIGMSG,
and (optionally) REPLY-ALL which indicates this is a reply-to-all
message. Return nil if there are no recipients for the particular field."
  (mu4e~draft-recipients-list-to-string
    (case field
      (:to
	(mu4e~draft-create-to-lst origmsg))
      (:cc
	(mu4e~draft-create-cc-lst origmsg reply-all))
      (otherwise
	(mu4e-error "Unsupported field")))))


(defun mu4e~draft-from-construct ()
  "Construct a value for the From:-field of the reply to MSG,
based on `user-full-name' and `user-mail-address'; if the latter is
nil, function returns nil."
  (when user-mail-address
    (if user-full-name
      (format "%s <%s>" user-full-name user-mail-address)
      (format "%s" user-mail-address))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mu4e~draft-insert-mail-header-separator ()
  "Insert `mail-header-separator' in the first empty line of the message.
`message-mode' needs this line to know where the headers end and
the body starts. Note, in `mu4e-compose-mode', we use
`before-save-hook' and `after-save-hook' to ensure that this
separator is never written to the message file. Also see
`mu4e-remove-mail-header-separator'."
  ;; we set this here explicitly, since (as it has happened) a wrong
  ;; value for this (such as "") breaks address completion and other things
  (set (make-local-variable 'mail-header-separator)
    (purecopy "--text follows this line--"))
  (put 'mail-header-separator 'permanent-local t)
  (save-excursion
    ;; make sure there's not one already
    (mu4e~draft-remove-mail-header-separator)
    (let ((sepa (propertize mail-header-separator
		  'intangible t
		  ;; don't make this read-only, message-mode
		  ;; seems to require it being writable in some cases
		  ;;'read-only "Can't touch this"
		  'rear-nonsticky t
		  'font-lock-face 'mu4e-compose-separator-face)))
      (widen)
      ;; search for the first empty line
      (goto-char (point-min))
      (if (search-forward-regexp "^$" nil t)
	  (replace-match sepa)
	  (progn ;; no empty line? then prepend one
	    (goto-char (point-max))
	    (insert "\n" sepa))))))

(defun mu4e~draft-remove-mail-header-separator ()
  "Remove `mail-header-separator; we do this before saving a
file (and restore it afterwards), to ensure that the separator
never hits the disk. Also see `mu4e~draft-insert-mail-header-separator."
  (save-excursion
    (widen)
    (goto-char (point-min))
    ;; remove the --text follows this line-- separator
    (when (search-forward-regexp (concat "^" mail-header-separator) nil t)
      (let ((inhibit-read-only t))
	(replace-match "")))))


(defun mu4e~draft-user-wants-reply-all (origmsg)
  "Ask user whether she wants to reply to *all* recipients.
If there is just one recipient of ORIGMSG do nothing."
  (let* ((recipnum
	   (+ (length (mu4e~draft-create-to-lst origmsg))
	     (length (mu4e~draft-create-cc-lst origmsg t))))
	  (response
	    (if (= recipnum 1)
	      'all ;; with one recipient, we can reply to 'all'....
	      (mu4e-read-option
		"Reply to "
		`( (,(format "all %d recipients" recipnum) . all)
		   ("sender only" . sender-only))))))
    (eq response 'all)))

(defun mu4e~draft-message-filename-construct (&optional flagstr)
  "Construct a randomized name for a message file with flags FLAGSTR.
It looks something like
  <time>-<random>.<hostname>:2,
You can append flags."
  (let* ((hostname
	   (downcase
	     (save-match-data
	       (substring system-name
		 (string-match "^[^.]+" system-name) (match-end 0))))))
    (format "%s-%02x%04x-%s:2,%s"
      (format-time-string "%Y%m%d" (current-time))
      (random 255) (random 65535) hostname (or flagstr ""))))
 
(defun mu4e~draft-common-construct ()
  "Construct the common headers for each message."
  (concat
   (mu4e~draft-header "User-agent" (mu4e~draft-user-agent-construct))
   (when mu4e-compose-auto-include-date
     (mu4e~draft-header "Date" (message-make-date)))))


(defconst mu4e~draft-reply-prefix "Re: "
  "String to prefix replies with.")

(defun mu4e~draft-reply-construct (origmsg)
  "Create a draft message as a reply to original message
ORIGMSG. Replying-to-self is a special; in that case, the To and Cc
fields will be the same as in the original."
  (let* ((reply-to-self (mu4e-message-contact-field-matches-me origmsg :from))
	  (recipnum
	     (+ (length (mu4e~draft-create-to-lst origmsg))
	       (length (mu4e~draft-create-cc-lst origmsg t))))
	  ;; reply-to-self implies reply-all
	  (reply-all (or reply-to-self (mu4e~draft-user-wants-reply-all origmsg)))
	  (old-msgid (plist-get origmsg :message-id))
	  (subject
	    (concat mu4e~draft-reply-prefix
	      (message-strip-subject-re (or (plist-get origmsg :subject) "")))))
    (concat
      (mu4e~draft-header "From" (or (mu4e~draft-from-construct) ""))
      (mu4e~draft-header "Reply-To" mu4e-compose-reply-to-address)

      (if reply-to-self
	;; When we're replying to ourselves, simply keep the same headers.
	(concat
	  (mu4e~draft-header "To" (mu4e~draft-recipients-list-to-string
				    (mu4e-message-field origmsg :to)))
	  (mu4e~draft-header "Cc" (mu4e~draft-recipients-list-to-string
				    (mu4e-message-field origmsg :cc)))) 
	
	;; if there's no-one in To, copy the CC-list
	(if (zerop (length (mu4e~draft-create-to-lst origmsg)))
	  (mu4e~draft-header "To" (mu4e~draft-recipients-construct :cc origmsg reply-all))
	  ;; otherwise...
	  (concat
	    (mu4e~draft-header "To" (mu4e~draft-recipients-construct :to origmsg))
	    (mu4e~draft-header "Cc" (mu4e~draft-recipients-construct :cc origmsg
				      reply-all)))))
      (mu4e~draft-header "Subject" subject)
      (mu4e~draft-header "References"
	(mu4e~draft-references-construct origmsg))
      (mu4e~draft-common-construct)
      (when old-msgid
	(mu4e~draft-header "In-reply-to" (format "<%s>" old-msgid)))
      "\n\n"
      (mu4e~draft-cite-original origmsg))))

(defconst mu4e~draft-forward-prefix "Fwd: "
  "String to prefix replies with.")

(defun mu4e~draft-forward-construct (origmsg)
  "Create a draft forward message for original message ORIGMSG."
  (let ((subject
	  (or (plist-get origmsg :subject) "")))
    (concat
      (mu4e~draft-header "From" (or (mu4e~draft-from-construct) ""))
      (mu4e~draft-header "Reply-To" mu4e-compose-reply-to-address)
      (mu4e~draft-header "To" "")
      (mu4e~draft-common-construct)
      (mu4e~draft-header "References"
	(mu4e~draft-references-construct origmsg))
      (mu4e~draft-header "Subject"
	(concat
	  ;; if there's no Fwd: yet, prepend it
	  (if (string-match "^Fwd:" subject)
	    ""
	    mu4e~draft-forward-prefix)
	  subject))
      "\n\n"
      (mu4e~draft-cite-original origmsg))))

(defun mu4e~draft-newmsg-construct ()
  "Create a new message."
  (concat
    (mu4e~draft-header "From" (or (mu4e~draft-from-construct) ""))
    (mu4e~draft-header "Reply-To" mu4e-compose-reply-to-address)
    (mu4e~draft-header "To" "")
    (mu4e~draft-header "Subject" "")
    (mu4e~draft-common-construct)))

(defvar mu4e~draft-drafts-folder nil
  "The drafts-folder for this compose buffer, based on
`mu4e-drafts-folder', which is evaluated once.")

(defun mu4e-draft-open (compose-type &optional msg)
  "Open a draft file for a new message (when COMPOSE-TYPE is reply, forward or new),
or open an existing draft (when COMPOSE-TYPE is edit).

The name of the draft folder is constructed from the concatenation
of `mu4e-maildir' and `mu4e-drafts-folder' (the latter will be
evaluated). The message file name is a unique name determined by
`mu4e-send-draft-file-name'. The initial contents will be created
from either `mu4e~draft-reply-construct', or
`mu4e~draft-forward-construct' or `mu4e~draft-newmsg-construct'."
  (unless mu4e-maildir (mu4e-error "mu4e-maildir not set"))
  (let ((draft-dir))
    (if (eq compose-type 'edit)
      ;; case-1: re-editing a draft messages. in this case, we do know the full
      ;; path, but we cannot really know 'drafts folder'... we make a guess
      (progn
	(setq draft-dir (mu4e~guess-maildir (mu4e-message-field msg :path)))
        (if (and mu4e-compose-in-new-frame (window-system))
            (find-file-other-frame (mu4e-message-field msg :path))
          (find-file (mu4e-message-field msg :path))))
      ;; case-2: creating a new message; in this case, we can determing
      ;; mu4e-get-drafts-folder
      (progn
	(setq draft-dir (mu4e-get-drafts-folder msg))
	(let ((draft-path
		(format "%s/%s/cur/%s"
		  mu4e-maildir
		  draft-dir
		  (mu4e~draft-message-filename-construct "DS"))))
          (if (and mu4e-compose-in-new-frame (window-system))
              (find-file-other-frame draft-path)
            (find-file draft-path)))
	(insert
	  (case compose-type
	    (reply   (mu4e~draft-reply-construct msg))
	    (forward (mu4e~draft-forward-construct msg))
	    (new     (mu4e~draft-newmsg-construct))
	    (t (mu4e-error "unsupported compose-type %S" compose-type))))
	;; include the message signature (if it's set)
	(if mu4e-compose-signature-auto-include
	  (let ((message-signature (or mu4e-compose-signature "\n"))
		 (message-signature-insert-empty-line t))
	    (save-excursion
	      (message-insert-signature)
	      (mu4e~fontify-signature)))
	  (insert "\n"))))
	  ;; evaluate mu4e~drafts-drafts-folder once, here, and use that value
	  ;; throughout.
    (set (make-local-variable 'mu4e~draft-drafts-folder) draft-dir)
    (put 'mu4e~draft-drafts-folder 'permanent-local t)
    (unless mu4e~draft-drafts-folder
      (mu4e-error "failed to determine drafts folder"))))

 
(provide 'mu4e-draft)
