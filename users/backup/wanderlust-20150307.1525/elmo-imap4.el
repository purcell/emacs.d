;;; elmo-imap4.el --- IMAP4 Interface for ELMO.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 1999,2000      Kenichi OKADA <okada@opaopa.org>
;; Copyright (C) 2000           OKAZAKI Tetsurou <okazaki@be.to>
;; Copyright (C) 2000           Daiki Ueno <ueno@unixuser.org>
;; Copyright (C) 2010           Erik Hetzner <egh@e6h.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Kenichi OKADA <okada@opaopa.org>
;;	OKAZAKI Tetsurou <okazaki@be.to>
;;	Daiki Ueno <ueno@unixuser.org>
;;      Erik Hetzner <egh@e6h.org>
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
;; Origin of IMAP parser part is imap.el, included in Gnus.
;;
;;    Copyright (C) 1998, 1999, 2000
;;    Free Software Foundation, Inc.
;;    Author: Simon Josefsson <jas@pdc.kth.se>
;;

;;; Code:
(require 'elmo-vars)
(require 'elmo-util)
(require 'elmo-date)
(require 'elmo-msgdb)
(require 'elmo-cache)
(require 'elmo)
(require 'elmo-net)
(require 'elmo-mime)
(require 'time-stamp)

(eval-when-compile (require 'cl))

(defvar elmo-imap4-disuse-server-flag-mailbox-regexp "^#mh" ; UW imapd
  "Regexp to match IMAP4 mailbox names whose message flags on server should be ignored (For STATUS command).")

(defvar elmo-imap4-overview-fetch-chop-length 200
  "*Number of overviews to fetch in one request.")

;; c.f. rfc2683 3.2.1.5 Long Command Lines
;;
;; "A client should limit the length of the command lines it generates
;;  to approximately 1000 octets (including all quoted strings but not
;;  including literals). If the client is unable to group things into
;;  ranges so that the command line is within that length, it should
;;  split the request into multiple commands. The client should use
;;  literals instead of long quoted strings, in order to keep the command
;;  length down.
;;  For its part, a server should allow for a command line of at least
;;  8000 octets. This provides plenty of leeway for accepting reasonable
;;  length commands from clients. The server should send a BAD response
;;  to a command that does not end within the server's maximum accepted
;;  command length. "

;; To limit command line length, chop number set.
(defvar elmo-imap4-number-set-chop-length 1000
  "*Number of messages to specify as a number-set argument for one request.")

(defvar elmo-imap4-force-login nil
  "*Non-nil forces to try 'login' if there is no 'auth' capability in imapd.")

(defvar elmo-imap4-use-select-to-update-status nil
  "*Some imapd have to send select command to update status.
\(ex. UW imapd 4.5-BETA?\).  For these imapd, you must set this variable t.")

(defvar elmo-imap4-use-modified-utf7 (coding-system-p 'utf-7-imap)
  "*Use modified UTF-7 (rfc2060) encoding for IMAP4 folder name.")

(defvar elmo-imap4-use-cache t
  "Use cache in imap4 folder.")

(defvar elmo-imap4-extra-namespace-alist
  '(("^\\({.*/nntp}\\).*$" . ".")) ; Default is for UW's remote nntp mailbox...
  "Extra namespace alist.
A list of cons cell like: (REGEXP . DELIMITER).
REGEXP should have a grouping for namespace prefix.")

(defvar elmo-imap4-disabled-extensions nil
  "List of server extensions that are disabled on the client side.")

(defvar elmo-imap4-flags-sync-method 'union
  "Method used when syncing server and client flags.")

(defvar elmo-imap4-strict-flag-availability-check nil
  "Perform a strict check if a flag is available.

When set to a non-nil value a flag is considered to be available
if and only if it appears in the server's FLAGS or PERMAFLAGS
response.

Otherwise a flag is also reported to be available if the
PERMAFLAGS response includes the special flag \* indicating that
a user can create custom flags.")

(defvar elmo-imap4-search-keys
  '("all" "answered" "bcc" "before" "body" "cc" "deleted" "draft"
    "flagged" "from" "header" "keyword" "larger" "new" "not" "old"
    "on" "or" "recent" "seen" "sentbefore" "senton" "sentsince"
    "since" "smaller" "subject" "text" "to" "uid" "unanswered"
    "undeleted" "undraft" "unflagged" "unkeyword" "unseen")
  "List of known IMAP search keys.

This list is used in `elmo-imap4-search-generate-vector' to
differentiate between IMAP4 search keys and searches for message
header values.")

;;; internal variables
;;
(defvar elmo-imap4-seq-prefix "elmo-imap4")
(defvar elmo-imap4-seqno 0)
(defvar elmo-imap4-use-uid t
  "Use UID as message number.")

(defvar elmo-imap4-current-response nil)
(defvar elmo-imap4-status nil)
(defvar elmo-imap4-reached-tag "elmo-imap40")

;;; buffer local variables
(defvar elmo-imap4-default-hierarchy-delimiter "/")

(defvar elmo-imap4-server-capability nil)
(defvar elmo-imap4-server-namespace nil)

(defvar elmo-imap4-parsing nil) ; indicates parsing.

(defvar elmo-imap4-fetch-callback nil)
(defvar elmo-imap4-fetch-callback-data nil)
(defvar elmo-imap4-status-callback nil)
(defvar elmo-imap4-status-callback-data nil)

(defvar elmo-imap4-server-diff-async-callback nil)
(defvar elmo-imap4-server-diff-async-callback-data nil)

;;; progress...(no use?)
(defvar elmo-imap4-count-progress nil)
(defvar elmo-imap4-count-progress-message nil)
(defvar elmo-imap4-progress-count nil)

;;; XXX Temporal implementation
(defvar elmo-imap4-current-msgdb nil)
(defvar elmo-imap4-seen-messages nil)

(defvar elmo-imap4-local-variables
  '(elmo-imap4-status
    elmo-imap4-current-response
    elmo-imap4-seqno
    elmo-imap4-parsing
    elmo-imap4-reached-tag
    elmo-imap4-count-progress
    elmo-imap4-count-progress-message
    elmo-imap4-progress-count
    elmo-imap4-fetch-callback
    elmo-imap4-fetch-callback-data
    elmo-imap4-status-callback
    elmo-imap4-status-callback-data
    elmo-imap4-current-msgdb
    elmo-imap4-seen-messages))

;;;;

(defconst elmo-imap4-quoted-specials-list '(?\\ ?\"))

(defconst elmo-imap4-non-atom-char-regex
  (eval-when-compile
    (concat "[^" "]!#$&'+,./0-9:;<=>?@A-Z[^_`a-z|}~-" "]")))

(defconst elmo-imap4-non-text-char-regex
  (eval-when-compile
    (concat "[^"
            "]\x01-\x09\x0b\x0c\x0e-\x1f\x7f !\"#$%&'()*+,./0-9:;<=>?@A-Z[\\^_`a-z{|}~-"
            "]")))

(defconst elmo-imap4-literal-threshold 1024
 "Limitation of characters that can be used in a quoted string.")

(defconst elmo-imap4-flag-specs '((important "\\Flagged")
                                  (read "\\Seen")
                                  (unread "\\Seen" 'remove)
                                  (answered "\\Answered")
                                  ;; draft-melnikov-imap-keywords-03.txt
                                  (forwarded "$Forwarded")
                                  (work "$Work")
                                  (personal "$Personal")
                                  (shouldreply "$ShouldReply")))

(defconst elmo-imap4-folder-name-syntax
  `(mailbox
    (?: [user "^[A-Za-z0-9]"] (?/ [auth ".+"]))
    ,@elmo-net-folder-name-syntax))

;; For debugging.
(defvar elmo-imap4-debug nil
  "Non-nil forces IMAP4 folder as debug mode.
Debug information is inserted in the buffer \"*IMAP4 DEBUG*\"")

(defvar elmo-imap4-debug-inhibit-logging nil)

;;; ELMO IMAP4 folder
(eval-and-compile
  (luna-define-class elmo-imap4-folder (elmo-net-folder)
                     (mailbox))
  (luna-define-internal-accessors 'elmo-imap4-folder))

;;; Session
(eval-and-compile
  (luna-define-class elmo-imap4-session (elmo-network-session)
                     (capability
                      current-mailbox
                      current-mailbox-size
                      read-only
                      flags))
  (luna-define-internal-accessors 'elmo-imap4-session))

(defmacro elmo-imap4-session-capable-p (session capability)
  `(and (memq ,capability (elmo-imap4-session-capability-internal ,session))
        (not (memq ,capability elmo-imap4-disabled-extensions))))

(defmacro elmo-imap4-mailbox-selected-p (mailbox session)
  "Return non-nil if MAILBOX is selected in SESSION."
  `(string= (elmo-imap4-session-current-mailbox-internal ,session) ,mailbox))

;;; MIME-ELMO-IMAP Location
(eval-and-compile
  (luna-define-class mime-elmo-imap-location
                     (mime-imap-location)
                     (folder number rawbuf strategy))
  (luna-define-internal-accessors 'mime-elmo-imap-location))

;;; Debug
(defmacro elmo-imap4-debug (message &rest args)
  `(if elmo-imap4-debug
       (elmo-imap4-debug-1 ,message ,@args)))

(defun elmo-imap4-debug-1 (message &rest args)
  (with-current-buffer (get-buffer-create "*IMAP4 DEBUG*")
    (goto-char (point-max))
    (if elmo-imap4-debug-inhibit-logging
        (insert "NO LOGGING\n")
      (insert (apply 'format message args) "\n"))))

(defsubst elmo-imap4-decode-folder-string (string)
  (if elmo-imap4-use-modified-utf7
      (if (coding-system-p 'utf-7-imap)
          (decode-coding-string string 'utf-7-imap)
        (utf7-decode string 'imap))
    string))

(defsubst elmo-imap4-encode-folder-string (string)
  (if elmo-imap4-use-modified-utf7
      (if (coding-system-p 'utf-7-imap)
          (encode-coding-string string 'utf-7-imap)
        (utf7-encode string 'imap))
    string))

;;; Response

(defmacro elmo-imap4-response-continue-req-p (response)
  "Returns non-nil if RESPONSE is '+' response."
  `(assq 'continue-req ,response))

(defmacro elmo-imap4-response-ok-p (response)
  "Returns non-nil if RESPONSE is an 'OK' response."
  `(assq 'ok ,response))

(defmacro elmo-imap4-response-bye-p (response)
  "Returns non-nil if RESPONSE is an 'BYE' response."
  `(assq 'bye ,response))

(defmacro elmo-imap4-response-garbage-p (response)
  "Returns non-nil if RESPONSE is an 'garbage' response."
  `(assq 'garbage ,response))

(defmacro elmo-imap4-response-value (response symbol)
  "Get value of the SYMBOL from RESPONSE."
  `(nth 1 (assq ,symbol ,response)))

(defsubst elmo-imap4-response-value-all (response symbol)
  "Get all value of the SYMBOL from RESPONSE."
  (let (matched)
    (while response
      (if (eq (car (car response)) symbol)
          (setq matched (nconc matched (nth 1 (car response)))))
      (setq response (cdr response)))
    matched))

(defmacro elmo-imap4-response-error-text (response)
  "Returns text of NO, BAD, BYE response."
  `(nth 1 (or (elmo-imap4-response-value ,response 'no)
              (elmo-imap4-response-value ,response 'bad)
              (elmo-imap4-response-value ,response 'bye))))

(defmacro elmo-imap4-response-bodydetail-text (response)
  "Returns text of BODY[section]<partial>."
  `(nth 3 (assq 'bodydetail ,response)))

(defun elmo-imap4-mailbox-size-update-maybe (session response)
  "Update size of selected mailbox in SESSION according to RESPONSE."
  (let ((exists (elmo-imap4-response-value-all response 'exists))
        (recent (elmo-imap4-response-value-all response 'recent))
        (current-size (or (elmo-imap4-session-current-mailbox-size-internal
                           session) (cons nil nil))))
    (if exists (setcar current-size (if (atom exists)
                                        exists (car (last exists)))))
    (if recent (setcdr current-size (if (atom recent)
                                        recent (car (last recent)))))
    (elmo-imap4-session-set-current-mailbox-size-internal
     session current-size)
    (elmo-imap4-debug "[%s] -> mailbox size adjusted: %s, %s"
                      (format-time-string "%T")
                      (elmo-imap4-session-current-mailbox-internal session)
                      current-size)))

;;; Session commands.

(defmacro with-elmo-imap4-session-process-buffer (session &rest body)
  `(with-current-buffer (process-buffer (elmo-network-session-process-internal ,session))
     ,@body))
(put 'with-elmo-imap4-session-process-buffer 'lisp-indent-function 1)

(defun elmo-imap4-command-tag (session)
  "Return new command tag for SESSION."
  (with-elmo-imap4-session-process-buffer session
    (concat elmo-imap4-seq-prefix (number-to-string (incf elmo-imap4-seqno)))))

;;;(defun elmo-imap4-send-command-wait (session command)
;;;  "Send COMMAND to the SESSION and wait for response.
;;;Returns RESPONSE (parsed lisp object) of IMAP session."
;;;  (elmo-imap4-read-response session
;;;                         (elmo-imap4-send-command
;;;                          session
;;;                          command)))

(defun elmo-imap4-session-wait-response-maybe (session)
  "Wait for a server response when in parsing state."
  (with-elmo-imap4-session-process-buffer session
    (when elmo-imap4-parsing
      (message "Waiting for IMAP response...")
      (accept-process-output (elmo-network-session-process-internal
                              session))
      (message "Waiting for IMAP response...done"))))

(defun elmo-imap4-send-command-wait (session command)
  "Send COMMAND to the SESSION.
Returns RESPONSE (parsed lisp object) of IMAP session.
If response is not `OK', causes error with IMAP response text."
  (elmo-imap4-accept-ok session
                        (elmo-imap4-send-command
                         session
                         command)))

(defun elmo-imap4-send-command (session command)
  "Send COMMAND to the SESSION.
Returns a TAG string which is assigned to the COMMAND."
  (let* ((command-args (if (listp command)
                           command
                         (list command)))
         (process (elmo-network-session-process-internal session))
         (tag (elmo-imap4-command-tag session))
         cmdlist token kind)
    (with-current-buffer (process-buffer process)
      (push tag cmdlist)
;;; No need.
;;;      (erase-buffer)
      (goto-char (point-min))
      (when (elmo-imap4-response-bye-p elmo-imap4-current-response)
        (elmo-imap4-process-bye session))
      (setq elmo-imap4-current-response nil)
      (elmo-imap4-session-wait-response-maybe session)
      (setq elmo-imap4-parsing t)
      (while (setq token (car command-args))
        (cond ((stringp token)   ; formatted
               (unless (string= "" token)
                 (push token cmdlist)))
              ((listp token)     ; unformatted
               (setq kind (car token))
               (cond ((eq kind 'atom)
                      (push (nth 1 token) cmdlist))
                     ((eq kind 'quoted)
                      (push (elmo-imap4-format-quoted (nth 1 token)) cmdlist))
                     ((eq kind 'literal)
                      (push (format
                             (if (elmo-imap4-session-capable-p session 'literal+) "{%d+}" "{%d}")
                             (nth 2 token)) cmdlist)
                      (elmo-imap4-session-process-send-string session (mapconcat #'identity (nreverse cmdlist) " "))
                      (setq cmdlist nil)
                      (unless (elmo-imap4-session-capable-p session 'literal+)
                        (elmo-imap4-accept-continue-req session))
                      (cond ((stringp (nth 1 token))
                             (push (nth 1 token) cmdlist))
                            ((bufferp (nth 1 token))
                             (with-current-buffer (nth 1 token)
                               (process-send-region
                                process
                                (point-min)
                                (+ (point-min) (nth 2 token)))))
                            (t
                             (error "Wrong argument for literal"))))
                     (t
                      (error "Unknown token kind %s" kind))))
              (t
               (error "Invalid argument")))
        (setq command-args (cdr command-args)))
      (elmo-imap4-session-process-send-string session (mapconcat #'identity (nreverse cmdlist) " "))
      tag)))

(defun elmo-imap4-session-process-send-string (session string)
  "Send STRING to process of SESSION."
  (elmo-imap4-debug "[%s] <-- %s" (format-time-string "%T") string)
  (process-send-string (elmo-network-session-process-internal session) string)
  (process-send-string (elmo-network-session-process-internal session) "\r\n"))

(defun elmo-imap4-send-string (session string)
  "Send STRING to the SESSION."
  (with-current-buffer (process-buffer
                        (elmo-network-session-process-internal session))
    (setq elmo-imap4-current-response nil)
    (goto-char (point-min))
    (elmo-imap4-session-process-send-string session string)))

(defun elmo-imap4-read-response (session tag)
  "Read parsed response from SESSION.
TAG is the tag of the command"
  (with-current-buffer (process-buffer
                        (elmo-network-session-process-internal session))
    (while (not (or (string= tag elmo-imap4-reached-tag)
                    (elmo-imap4-response-bye-p elmo-imap4-current-response)
                    (when (elmo-imap4-response-garbage-p
                           elmo-imap4-current-response)
                      (message "Garbage response: %s"
                               (elmo-imap4-response-value
                                elmo-imap4-current-response
                                'garbage))
                      t)))
      (when (memq (process-status
                   (elmo-network-session-process-internal session))
                  '(open run))
        (accept-process-output (elmo-network-session-process-internal session)
                               1)))
    (elmo-imap4-debug "[%s] => %s" (format-time-string "%T") (prin1-to-string elmo-imap4-current-response))
    (setq elmo-imap4-parsing nil)
    (elmo-imap4-mailbox-size-update-maybe session elmo-imap4-current-response)
    elmo-imap4-current-response))

(defsubst elmo-imap4-read-untagged (process)
  (with-current-buffer (process-buffer process)
    (while (not elmo-imap4-current-response)
      (accept-process-output process 1))
    (elmo-imap4-debug "[%s] =>%s" (format-time-string "%T") (prin1-to-string elmo-imap4-current-response))
    elmo-imap4-current-response))

(defun elmo-imap4-read-continue-req (session)
  "Returns a text following to continue-req in SESSION.
If response is not `+' response, returns nil."
  (elmo-imap4-response-value
   (elmo-imap4-read-untagged
    (elmo-network-session-process-internal session))
   'continue-req))

(defun elmo-imap4-process-bye (session)
  (with-current-buffer (elmo-network-session-buffer session)
    (let ((r elmo-imap4-current-response))
      (setq elmo-imap4-current-response nil)
      (elmo-network-close-session session)
      (signal 'elmo-imap4-bye-error
              (list (concat (elmo-imap4-response-error-text r))
                    "Try Again")))))

(defun elmo-imap4-accept-continue-req (session)
  "Returns non-nil if `+' (continue-req) response is arrived in SESSION.
If response is not `+' response, cause an error."
  (let (response)
    (setq response
          (elmo-imap4-read-untagged
           (elmo-network-session-process-internal session)))
    (or (elmo-imap4-response-continue-req-p response)
        (error "IMAP error: %s"
               (or (elmo-imap4-response-error-text response)
                   "No continut-req from server.")))))

(defun elmo-imap4-read-ok (session tag)
  "Returns non-nil if `OK' response of the command with TAG is arrived
in SESSION. If response is not `OK' response, returns nil."
  (elmo-imap4-response-ok-p
   (elmo-imap4-read-response session tag)))

(defun elmo-imap4-accept-ok (session tag)
  "Accept only `OK' response from SESSION.
If response is not `OK' response, causes error with IMAP response text."
  (let ((response (elmo-imap4-read-response session tag)))
    (if (elmo-imap4-response-ok-p response)
        response
      (if (elmo-imap4-response-bye-p response)
          (elmo-imap4-process-bye session)
        (error "IMAP error: %s"
               (or (elmo-imap4-response-error-text response)
                   "No `OK' response from server."))))))

;;; MIME-ELMO-IMAP Location
(luna-define-method mime-imap-location-section-body ((location
                                                      mime-elmo-imap-location)
                                                     section)
  (if (and (stringp section)
           (string= section "HEADER"))
      ;; Even in the section mode, header fields should be saved to the
      ;; raw buffer .
      (with-current-buffer (mime-elmo-imap-location-rawbuf-internal location)
        (erase-buffer)
        (elmo-message-fetch
         (mime-elmo-imap-location-folder-internal location)
         (mime-elmo-imap-location-number-internal location)
         (mime-elmo-imap-location-strategy-internal location)
         'unseen
         section)
        (buffer-string))
    (elmo-message-fetch-string
     (mime-elmo-imap-location-folder-internal location)
     (mime-elmo-imap-location-number-internal location)
     (mime-elmo-imap-location-strategy-internal location)
     'unseen
     section)))


(luna-define-method mime-imap-location-bodystructure
  ((location mime-elmo-imap-location))
  (elmo-message-fetch-bodystructure
   (mime-elmo-imap-location-folder-internal location)
   (mime-elmo-imap-location-number-internal location)
   (mime-elmo-imap-location-strategy-internal location)))

(luna-define-method mime-imap-location-fetch-entity-p
  ((location mime-elmo-imap-location) entity)
  (or (not elmo-message-displaying) ; Fetching entity to save or force display.
      ;; cache exists
      (file-exists-p
       (expand-file-name
        (mmimap-entity-section (mime-entity-node-id-internal entity))
        (elmo-fetch-strategy-cache-path
         (mime-elmo-imap-location-strategy-internal location))))
      ;; not too large to fetch.
      (> elmo-message-fetch-threshold
         (or (mime-imap-entity-size-internal entity) 0))))

;;;

(defun elmo-imap4-session-check (session)
  (with-current-buffer (elmo-network-session-buffer session)
    (setq elmo-imap4-fetch-callback nil)
    (setq elmo-imap4-fetch-callback-data nil))
  (elmo-imap4-send-command-wait session "check"))

(defun elmo-imap4-atom-p (string)
  "Return t if STRING is an atom defined in rfc2060."
  (if (string= string "")
      nil
    (save-match-data
      (not (string-match elmo-imap4-non-atom-char-regex string)))))

(defun elmo-imap4-quotable-p (string)
  "Return t if STRING can be formatted as a quoted defined in rfc2060."
  (save-match-data
    (not (string-match elmo-imap4-non-text-char-regex string))))

(defun elmo-imap4-nil (string)
  "Return a list represents the special atom \"NIL\" defined in rfc2060, \
if STRING is nil.
Otherwise return nil."
  (if (eq string nil)
      (list 'atom "NIL")))

(defun elmo-imap4-atom (string)
  "Return a list represents STRING as an atom defined in rfc2060.
Return nil if STRING is not an atom.  See `elmo-imap4-atom-p'."
  (if (elmo-imap4-atom-p string)
      (list 'atom string)))

(defun elmo-imap4-quoted (string)
  "Return a list represents STRING as a quoted defined in rfc2060.
Return nil if STRING can not be formatted as a quoted.  See `elmo-imap4-quotable-p'."
  (if (elmo-imap4-quotable-p string)
      (list 'quoted string)))

(defun elmo-imap4-literal-1 (string-or-buffer length)
  "Internal function for `elmo-imap4-literal' and `elmo-imap4-buffer-literal'.
Return a list represents STRING-OR-BUFFER as a literal defined in rfc2060.
STRING-OR-BUFFER must be an encoded string or a single-byte string or a single-byte buffer.
LENGTH must be the number of octets for STRING-OR-BUFFER."
  (list 'literal string-or-buffer length))

(defun elmo-imap4-literal (string)
  "Return a list represents STRING as a literal defined in rfc2060.
STRING must be an encoded or a single-byte string."
  (elmo-imap4-literal-1 string (length string)))

(defun elmo-imap4-buffer-literal (buffer)
  "Return a list represents BUFFER as a literal defined in rfc2060.
BUFFER must be a single-byte buffer."
  (elmo-imap4-literal-1 buffer (with-current-buffer buffer
                                 (buffer-size))))

(defun elmo-imap4-string-1 (string length)
  "Internal function for `elmo-imap4-string' and `elmo-imap4-buffer-string'.
Return a list represents STRING as a string defined in rfc2060.
STRING must be an encoded or a single-byte string.
LENGTH must be the number of octets for STRING."
  (or (elmo-imap4-quoted string)
      (elmo-imap4-literal-1 string length)))

(defun elmo-imap4-string (string)
  "Return a list represents STRING as a string defined in rfc2060.
STRING must be an encoded or a single-byte string."
  (let ((length (length string)))
    (if (< elmo-imap4-literal-threshold length)
        (elmo-imap4-literal-1 string length)
      (elmo-imap4-string-1 string length))))

(defun elmo-imap4-buffer-string (buffer)
  "Return a list represents BUFFER as a string defined in rfc2060.
BUFFER must be a single-byte buffer."
  (let ((length (with-current-buffer buffer
                  (buffer-size))))
    (if (< elmo-imap4-literal-threshold length)
        (elmo-imap4-literal-1 buffer length)
      (elmo-imap4-string-1 (with-current-buffer buffer
                             (buffer-string))
                           length))))

(defun elmo-imap4-astring-1 (string length)
  "Internal function for `elmo-imap4-astring' and `elmo-imap4-buffer-astring'.
Return a list represents STRING as an astring defined in rfc2060.
STRING must be an encoded or a single-byte string.
LENGTH must be the number of octets for STRING."
  (or (elmo-imap4-atom string)
      (elmo-imap4-string-1 string length)))

(defun elmo-imap4-astring (string)
  "Return a list represents STRING as an astring defined in rfc2060.
STRING must be an encoded or a single-byte string."
  (let ((length (length string)))
    (if (< elmo-imap4-literal-threshold length)
        (elmo-imap4-literal-1 string length)
      (elmo-imap4-astring-1 string length))))

(defun elmo-imap4-buffer-astring (buffer)
  "Return a list represents BUFFER as an astring defined in rfc2060.
BUFFER must be a single-byte buffer."
  (let ((length (with-current-buffer buffer
                  (buffer-size))))
    (if (< elmo-imap4-literal-threshold length)
        (elmo-imap4-literal-1 buffer length)
      (elmo-imap4-astring-1 (with-current-buffer buffer
                              (buffer-string))
                            length))))

(defun elmo-imap4-nstring (string)
  "Return a list represents STRING as a nstring defined in rfc2060.
STRING must be an encoded or a single-byte string."
   (or (elmo-imap4-nil string)
       (elmo-imap4-string string)))

(defun elmo-imap4-buffer-nstring (buffer)
  "Return a list represents BUFFER as a nstring defined in rfc2060.
BUFFER must be a single-byte buffer."
   (or (elmo-imap4-nil buffer)
       (elmo-imap4-buffer-string buffer)))

(defalias 'elmo-imap4-mailbox 'elmo-imap4-astring)
(defalias 'elmo-imap4-field-body 'elmo-imap4-astring)
(defalias 'elmo-imap4-userid 'elmo-imap4-astring)
(defalias 'elmo-imap4-password 'elmo-imap4-astring)

(defun elmo-imap4-format-quoted (string)
  "Return STRING in a form of the quoted-string defined in rfc2060."
  (concat "\""
          (std11-wrap-as-quoted-pairs string elmo-imap4-quoted-specials-list)
          "\""))

(defsubst elmo-imap4-response-get-selectable-mailbox-list (response)
  (delq nil
        (mapcar
         (lambda (entry)
           (if (and (eq 'list (car entry))
                    (not (elmo-string-member-ignore-case "\\Noselect" (nth 1 (nth 1 entry)))))
               (car (nth 1 entry))))
         response)))

(luna-define-method elmo-message-fetch-bodystructure ((folder
                                                       elmo-imap4-folder)
                                                      number strategy)
  (if (elmo-fetch-strategy-use-cache strategy)
      (elmo-object-load
       (elmo-file-cache-expand-path
        (elmo-fetch-strategy-cache-path strategy)
        "bodystructure"))
    (let ((session (elmo-imap4-get-session folder))
          bodystructure)
      (elmo-imap4-session-select-mailbox
       session
       (elmo-imap4-folder-mailbox-internal folder))
      (with-current-buffer (elmo-network-session-buffer session)
        (setq elmo-imap4-fetch-callback nil)
        (setq elmo-imap4-fetch-callback-data nil))
      (prog1 (setq bodystructure
                   (elmo-imap4-response-value
                    (elmo-imap4-response-value
                     (elmo-imap4-send-command-wait
                      session
                      (format
                       (if elmo-imap4-use-uid
                           "uid fetch %s bodystructure"
                         "fetch %s bodystructure")
                       number))
                     'fetch)
                    'bodystructure))
        (when (elmo-fetch-strategy-save-cache strategy)
          (elmo-file-cache-delete
           (elmo-fetch-strategy-cache-path strategy))
          (elmo-object-save
           (elmo-file-cache-expand-path
            (elmo-fetch-strategy-cache-path strategy)
            "bodystructure")
           bodystructure))))))

;;; Backend methods.
(luna-define-method elmo-create-folder-plugged ((folder elmo-imap4-folder))
  (elmo-imap4-send-command-wait
   (elmo-imap4-get-session folder)
   (list "create" (elmo-imap4-mailbox
                   (elmo-imap4-folder-mailbox-internal folder)))))

(defun elmo-imap4-get-session (folder &optional if-exists)
  (elmo-network-get-session 'elmo-imap4-session
                            (concat
                             (if (elmo-folder-biff-internal folder)
                                 "BIFF-")
                             "IMAP")
                            folder if-exists))

(defun elmo-imap4-session-select-mailbox (session mailbox
                                                  &optional force no-error)
  "Select MAILBOX in SESSION.
If optional argument FORCE is non-nil, select mailbox even if current mailbox
is same as MAILBOX.
If second optional argument NO-ERROR is non-nil, don't cause an error when
selecting folder was failed.
If NO-ERROR is 'notify-bye, only BYE response is reported as error.
Returns response value if selecting folder succeed. "
  (when (or force
            (not (elmo-imap4-mailbox-selected-p mailbox session)))
    (let (response result)
      (unwind-protect
          (setq response
                (elmo-imap4-read-response
                 session
                 (elmo-imap4-send-command
                  session
                  (list
                   "select"
                   (elmo-imap4-mailbox mailbox)))))
        (if (setq result (elmo-imap4-response-ok-p response))
            (progn
              (elmo-imap4-session-set-current-mailbox-internal session mailbox)
              (elmo-imap4-session-set-read-only-internal
               session
               (nth 1 (assq 'read-only (assq 'ok response))))
              (elmo-imap4-session-set-flags-internal
               session
               (nth 1 (or (assq 'permanentflags response)
                          (assq 'flags response)))))
          (elmo-imap4-session-set-current-mailbox-internal session nil)
          (elmo-imap4-session-set-current-mailbox-size-internal session nil)
          (if (and (eq no-error 'notify-bye)
                   (elmo-imap4-response-bye-p response))
              (elmo-imap4-process-bye session)
            (unless no-error
              (error "%s"
                     (or (elmo-imap4-response-error-text response)
                         (format "Select %s failed" mailbox)))))))
      (and result response))))

(defun elmo-imap4-session-unselect-mailbox (session mailbox)
  "Unselect MAILBOX in SESSION.
Deselecting will exit selected state without causing silent
EXPUNGE for deleted messages."
  (if (elmo-imap4-session-capable-p session 'unselect)
      (elmo-imap4-send-command-wait session "unselect")
    (elmo-imap4-send-command-wait
     session
     (list "examine" (elmo-imap4-mailbox mailbox)))
    (elmo-imap4-send-command-wait session "close"))
  (elmo-imap4-session-set-current-mailbox-internal session nil)
  (elmo-imap4-session-set-current-mailbox-size-internal session nil))

(defun elmo-imap4-check-validity (spec validity-file)
;;; Not used.
;;;(elmo-imap4-send-command-wait
;;;(elmo-imap4-get-session spec)
;;;(list "status "
;;;      (elmo-imap4-mailbox
;;;       (elmo-imap4-spec-mailbox spec))
;;;      " (uidvalidity)")))
  )

(defun elmo-imap4-sync-validity  (spec validity-file)
  ;; Not used.
  )

(defun elmo-imap4-elist (folder query tags)
  (let ((session (elmo-imap4-get-session folder)))
    (elmo-imap4-session-select-mailbox
     session
     (elmo-imap4-folder-mailbox-internal folder))
    (let ((answer (elmo-imap4-response-value
                   (elmo-imap4-send-command-wait
                    session query) 'esearch))
          tag result)
      (while answer
        (setq tag (intern (downcase (car answer))))
        (cond ((eq tag 'uid)
               nil)
              ((memq tag tags)
               (setq result
                     (append result
                             (if (eq tag 'all)
                                 (sort
                                  (elmo-number-set-to-number-list
                                   (mapcar #'(lambda (x)
                                               (let ((y (split-string x ":")))
                                                 (if (null (cdr y))
                                                     (string-to-number (car y))
                                                   (cons (string-to-number (car y))
                                                         (string-to-number (cadr y))))))
                                           (split-string (cadr answer) ","))) '<)
                               (string-to-number (cadr answer))))))
              (t nil))
        (setq answer (cdr answer)))
      result)))

(defun elmo-imap4-list (folder flag)
  (let ((session (elmo-imap4-get-session folder)))
    (elmo-imap4-session-select-mailbox
     session
     (elmo-imap4-folder-mailbox-internal folder))
    (if (elmo-imap4-session-capable-p session 'esearch)
        (elmo-imap4-elist folder
                          (concat (if elmo-imap4-use-uid "uid " "")
                                  "search return (all) " flag) '(all))
      (elmo-imap4-response-value
       (elmo-imap4-send-command-wait
        session
        (format (if elmo-imap4-use-uid "uid search %s"
                  "search %s") flag))
       'search))))

(defun elmo-imap4-session-flag-available-p (session flag)
  (case flag
    ((read unread) (elmo-string-member-ignore-case
                    "\\seen" (elmo-imap4-session-flags-internal session)))
    (important
     (elmo-string-member-ignore-case
      "\\flagged" (elmo-imap4-session-flags-internal session)))
    (digest
     (or (elmo-string-member-ignore-case
          "\\seen" (elmo-imap4-session-flags-internal session))
         (elmo-string-member-ignore-case
          "\\flagged" (elmo-imap4-session-flags-internal session))))
    (answered
     (elmo-string-member-ignore-case
      (concat "\\" (symbol-name flag))
      (elmo-imap4-session-flags-internal session)))
    (t
     (elmo-string-member-ignore-case
      (if elmo-imap4-strict-flag-availability-check
          (symbol-name flag)
        "\\*")
      (elmo-imap4-session-flags-internal session)))))

(defun elmo-imap4-flag-to-imap-search-key (flag)
  (case flag
    (read "seen")
    (unread "unseen")
    (important "flagged")
    (answered "answered")
    (new "new")
    (t (concat
        "keyword "
        (or (car (cdr (assq flag elmo-imap4-flag-specs)))
            (symbol-name flag))))))

(defun elmo-imap4-flag-to-imap-criteria (flag)
  (case flag
    ((any digest)
     (let ((criteria "flagged")
           (global-flags (delq 'important (elmo-get-global-flags t t))))
       (dolist (flag (delete 'new
                             (delete 'cached
                                     (copy-sequence
                                      (case flag
                                        (any
                                         elmo-preserved-flags)
                                        (digest
                                         elmo-digest-flags))))))
         (setq criteria (concat "or "
                                (elmo-imap4-flag-to-imap-search-key flag)
                                " "
                                criteria)))
       (while global-flags
         (setq criteria (concat "or keyword "
                                (symbol-name (car global-flags))
                                " "
                                criteria))
         (setq global-flags (cdr global-flags)))
       criteria))
    (t
     (elmo-imap4-flag-to-imap-search-key flag))))

(defun elmo-imap4-folder-list-flagged (folder flag &optional type)
  "List flagged message numbers in the FOLDER.
FLAG is one of the `unread', `read', `important', `answered',
`any'.
When optional argument TYPE is symbol 'unmatch, negate search
condition."
  (let ((session (elmo-imap4-get-session folder))
        (criteria (concat (if (eq type 'unmatch) "not " "")
                          (elmo-imap4-flag-to-imap-criteria flag))))
    (if (elmo-imap4-session-flag-available-p session flag)
        (elmo-imap4-list folder criteria)
      ;; List flagged messages in the msgdb.
      (elmo-msgdb-list-flagged (elmo-folder-msgdb folder) flag))))

(defvar elmo-imap4-rfc822-size "RFC822\.SIZE")
(defvar elmo-imap4-rfc822-text "RFC822\.TEXT")
(defvar elmo-imap4-rfc822-header "RFC822\.HEADER")
(defvar elmo-imap4-header-fields "HEADER\.FIELDS")

(defun elmo-imap4-make-number-set-list (msg-list &optional chop-length)
  "Make RFC2060's message set specifier from MSG-LIST.
Returns a list of (NUMBER . SET-STRING).
SET-STRING is the message set specifier described in RFC2060.
NUMBER is contained message number in SET-STRING.
Every SET-STRING does not contain number of messages longer than CHOP-LENGTH.
If CHOP-LENGTH is not specified, message set is not chopped."
  (let (count cont-list set-list)
    (setq msg-list (sort (copy-sequence msg-list) '<))
    (while msg-list
      (setq cont-list nil)
      (setq count 0)
      (unless chop-length
        (setq chop-length (length msg-list)))
      (while (and (not (null msg-list))
                  (< count chop-length))
        (setq cont-list
              (elmo-number-set-append
               cont-list (car msg-list)))
        (incf count)
        (setq msg-list (cdr msg-list)))
      (setq set-list
            (cons
             (cons
              count
              (mapconcat
               (lambda (x)
                 (cond ((consp x)
                        (format "%s:%s" (car x) (cdr x)))
                       ((integerp x)
                        (number-to-string x))))
               cont-list
               ","))
             set-list)))
    (nreverse set-list)))

;;
;; app-data:
;; cons of flag-table and folder structure
(defsubst elmo-imap4-fetch-callback-1-subr (entity flags app-data)
  "A msgdb entity callback function."
  (let ((use-flag (elmo-folder-use-flag-p (cdr app-data)))
        (flag-table (car app-data))
        (msg-id (elmo-message-entity-field entity 'message-id))
        saved-flags flag-list)
;;;    (when (elmo-string-member-ignore-case "\\Flagged" flags)
;;;      (elmo-msgdb-global-mark-set msg-id elmo-msgdb-important-mark))
    (setq saved-flags (elmo-flag-table-get flag-table msg-id)
          flag-list
          (if use-flag
              (append
               (and (memq 'new saved-flags)
                    (not (elmo-string-member-ignore-case "\\Seen" flags))
                    '(new))
               (and (elmo-string-member-ignore-case "\\Flagged" flags)
                    '(important))
               (and (not (elmo-string-member-ignore-case "\\Seen" flags))
                    '(unread))
               (and (elmo-string-member-ignore-case "\\Answered" flags)
                    '(answered))
               (and (elmo-file-cache-exists-p msg-id)
                    '(cached)))
            saved-flags))
    (elmo-msgdb-append-entity elmo-imap4-current-msgdb
                              entity
                              flag-list)))

;; Current buffer is process buffer.
(defun elmo-imap4-fetch-callback-1 (element app-data)
  (let ((handler (elmo-msgdb-message-entity-handler elmo-imap4-current-msgdb)))
    (elmo-imap4-fetch-callback-1-subr
     (with-temp-buffer
       (insert (or (elmo-imap4-response-bodydetail-text element)
                   ""))
       ;; Replace all CRLF with LF.
       (elmo-delete-cr-buffer)
       (elmo-msgdb-create-message-entity-from-header
        handler
        (elmo-imap4-response-value element 'uid)
        :size (elmo-imap4-response-value element 'rfc822size)))
     (elmo-imap4-response-value element 'flags)
     app-data)
    (elmo-progress-notify 'elmo-folder-msgdb-create)))

(defun elmo-imap4-parse-capability (string)
  (if (string-match "^\\*\\(.*\\)$" string)
      (read
       (concat "(" (downcase (match-string 1 string)) ")"))))

(defun elmo-imap4-clear-login (session)
  (when (elmo-imap4-session-capable-p session 'logindisabled)
    (signal 'elmo-authenticate-error '(elmo-imap4-clear-login)))
  (let ((elmo-imap4-debug-inhibit-logging t))
    (or
     (elmo-imap4-read-ok
      session
      (elmo-imap4-send-command
       session
       (list "login"
             (elmo-imap4-userid (elmo-network-session-user-internal session))
             (elmo-imap4-password
              (elmo-get-passwd (elmo-network-session-password-key session))))))
     (signal 'elmo-authenticate-error '(elmo-imap4-clear-login)))))

(defun elmo-imap4-auth-login (session)
  (let ((tag (elmo-imap4-send-command session "authenticate login"))
        (elmo-imap4-debug-inhibit-logging t))
    (or (elmo-imap4-read-continue-req session)
        (signal 'elmo-authenticate-error '(elmo-imap4-auth-login)))
    (elmo-imap4-send-string session
                            (elmo-base64-encode-string
                             (elmo-network-session-user-internal session)))
    (or (elmo-imap4-read-continue-req session)
        (signal 'elmo-authenticate-error '(elmo-imap4-auth-login)))
    (elmo-imap4-send-string session
                            (elmo-base64-encode-string
                             (elmo-get-passwd
                              (elmo-network-session-password-key session))))
    (or (elmo-imap4-read-ok session tag)
        (signal 'elmo-authenticate-error '(elmo-imap4-auth-login)))
    (setq elmo-imap4-status 'auth)))

(luna-define-method
  elmo-network-initialize-session-buffer :after ((session
                                                  elmo-imap4-session) buffer)
  (with-current-buffer buffer
    (mapc 'make-variable-buffer-local elmo-imap4-local-variables)
    (setq elmo-imap4-seqno 0)
    (setq elmo-imap4-status 'initial)))

(luna-define-method elmo-network-initialize-session ((session
                                                      elmo-imap4-session))
  (let ((process (elmo-network-session-process-internal session)))
    (with-current-buffer (process-buffer process)
      ;; Skip garbage output from process before greeting.
      (while (and (memq (process-status process) '(open run))
                  (goto-char (point-max))
                  (or (/= (forward-line -1) 0)
                      (not (elmo-imap4-parse-greeting))))
        (accept-process-output process 1))
      (erase-buffer)
      (set-process-filter process 'elmo-imap4-arrival-filter)
      (set-process-sentinel process 'elmo-imap4-sentinel)
;;;      (while (and (memq (process-status process) '(open run))
;;;               (eq elmo-imap4-status 'initial))
;;;	(message "Waiting for server response...")
;;;	(accept-process-output process 1))
;;;      (message "")
      (unless (memq elmo-imap4-status '(nonauth auth))
        (signal 'elmo-open-error
                (list 'elmo-network-initialize-session)))
      (elmo-imap4-session-set-capability-internal
       session
       (elmo-imap4-response-value
        (elmo-imap4-send-command-wait session "capability")
        'capability))
      (when (eq (elmo-network-stream-type-symbol
                 (elmo-network-session-stream-type-internal session))
                'starttls)
        (or (elmo-imap4-session-capable-p session 'starttls)
            (signal 'elmo-open-error
                    '(elmo-imap4-starttls-error)))
        (elmo-imap4-send-command-wait session "starttls")
        (starttls-negotiate process)
        (elmo-imap4-session-set-capability-internal
         session
         (elmo-imap4-response-value
          (elmo-imap4-send-command-wait session "capability")
          'capability))))))

(luna-define-method elmo-network-authenticate-session ((session
                                                        elmo-imap4-session))
  (with-current-buffer (process-buffer
                        (elmo-network-session-process-internal session))
    (let* ((auth (elmo-network-session-auth-internal session))
           (auth (if (listp auth) auth (list auth))))
      (unless (or (eq elmo-imap4-status 'auth)
                  (null auth))
        (cond
         ((eq 'clear (car auth))
          (elmo-imap4-clear-login session))
         ((eq 'login (car auth))
          (elmo-imap4-auth-login session))
         (t
          (let* ((elmo-imap4-debug-inhibit-logging t)
                 (sasl-mechanisms
                  (delq nil
                        (mapcar
                         (lambda (cap)
                           (if (string-match "^auth=\\(.*\\)$"
                                             (symbol-name cap))
                               (match-string 1 (upcase (symbol-name cap)))))
                         (elmo-imap4-session-capability-internal session))))
                 (mechanism
                  (sasl-find-mechanism
                   (delq nil
                         (mapcar (lambda (cap) (upcase (symbol-name cap)))
                                 (if (listp auth)
                                     auth
                                   (list auth)))))) ;)
                 client name step response tag
                 sasl-read-passphrase)
            (unless mechanism
              (if (or elmo-imap4-force-login
                      (y-or-n-p
                       (format
                        "There's no %s capability in server. continue?"
                        (elmo-list-to-string
                         (elmo-network-session-auth-internal session)))))
                  (setq mechanism (sasl-find-mechanism
                                   sasl-mechanisms))
                (signal 'elmo-authenticate-error
                        '(elmo-imap4-auth-no-mechanisms))))
            (setq client
                  (sasl-make-client
                   mechanism
                   (elmo-network-session-user-internal session)
                   "imap"
                   (elmo-network-session-server-internal session)))
;;;         (if elmo-imap4-auth-user-realm
;;;		(sasl-client-set-property client 'realm elmo-imap4-auth-user-realm))
            (setq name (sasl-mechanism-name mechanism)
                  step (sasl-next-step client nil))
            (elmo-network-session-set-auth-internal
             session
             (intern (downcase name)))
            (setq sasl-read-passphrase
                  (lambda (prompt)
                    (elmo-get-passwd
                     (elmo-network-session-password-key session))))
            (setq tag
                  (elmo-imap4-send-command
                   session
                   (concat "AUTHENTICATE " name
                           (and (sasl-step-data step)
                                (concat
                                 " "
                                 (elmo-base64-encode-string
                                  (sasl-step-data step)
                                  'no-lin-break))))))
            (catch 'done
              (while t
                (setq response
                      (elmo-imap4-read-untagged
                       (elmo-network-session-process-internal session)))
                (if (elmo-imap4-response-ok-p response)
                    (if (sasl-next-step client step)
                        ;; Bogus server?
                        (signal 'elmo-authenticate-error
                                (list (intern
                                       (concat "elmo-imap4-auth-"
                                               (downcase name)))))
                      ;; The authentication process is finished.
                      (throw 'done nil)))
                (unless (elmo-imap4-response-continue-req-p response)
                  ;; response is NO or BAD.
                  (signal 'elmo-authenticate-error
                          (list (intern
                                 (concat "elmo-imap4-auth-"
                                         (downcase name))))))
                (sasl-step-set-data
                 step
                 (elmo-base64-decode-string
                  (elmo-imap4-response-value response 'continue-req)))
                (setq step (sasl-next-step client step))
                (setq tag
                      (elmo-imap4-send-string
                       session
                       (if (sasl-step-data step)
                           (elmo-base64-encode-string (sasl-step-data step)
                                                      'no-line-break)
                         ""))))))))
;; Some servers return reduced capabilities when client asks for them
;; before login. It might be a good idea to ask them again, otherwise
;; we can miss some useful feature.
        (elmo-imap4-session-set-capability-internal
         session
         (elmo-imap4-response-value
          (elmo-imap4-send-command-wait session "capability")
          'capability))))))

(luna-define-method elmo-network-setup-session ((session
                                                 elmo-imap4-session))
  (with-current-buffer (elmo-network-session-buffer session)
    (when (elmo-imap4-session-capable-p session 'namespace)
      (setq elmo-imap4-server-namespace
            (elmo-imap4-response-value
             (elmo-imap4-send-command-wait session "namespace")
             'namespace)))))

(defun elmo-imap4-setup-send-buffer (&optional string file)
  (let ((send-buf (get-buffer-create " *elmo-imap4-setup-send-buffer*"))
        (source-buf (unless string (current-buffer))))
    (save-excursion
      (save-match-data
        (set-buffer send-buf)
        (erase-buffer)
        (set-buffer-multibyte nil)
        (cond
         (string
          (insert string))
         (file
          (as-binary-input-file
           (insert-file-contents file)))
         (t
          (with-current-buffer source-buf
            (copy-to-buffer send-buf (point-min) (point-max)))))
        (goto-char (point-min))
        (if (eq (re-search-forward "^$" nil t)
                (point-max))
            (insert "\n"))
        (let (inhibit-eol-conversion)
          (encode-coding-region (point-min) (point-max) 'raw-text-dos))))
    send-buf))

(defun elmo-imap4-setup-send-buffer-from-file (file)
  (elmo-imap4-setup-send-buffer nil file))

(luna-define-method elmo-delete-message-safe ((folder elmo-imap4-folder)
                                              number msgid)
  (let ((session (elmo-imap4-get-session folder))
        candidates)
    (elmo-imap4-session-select-mailbox
     session
     (elmo-imap4-folder-mailbox-internal folder))
    (setq candidates
          (elmo-imap4-response-value
           (elmo-imap4-send-command-wait session
                                         (list
                                          (if elmo-imap4-use-uid
                                              "uid search header message-id "
                                            "search header message-id ")
                                          (elmo-imap4-field-body msgid)))
           'search))
    (if (memq number candidates)
        (elmo-folder-delete-messages folder (list number)))))

(defun elmo-imap4-server-diff-async-callback-1 (status data)
  (funcall elmo-imap4-server-diff-async-callback
           (list (elmo-imap4-response-value status 'recent)
                 (elmo-imap4-response-value status 'unseen)
                 (elmo-imap4-response-value status 'messages))
           data))

(defun elmo-imap4-server-diff-async (folder)
  (let ((session (elmo-imap4-get-session folder)))
    ;; We should `check' folder to obtain newest information here.
    ;; But since there's no asynchronous check mechanism in elmo yet,
    ;; checking is not done here.
    (with-current-buffer (elmo-network-session-buffer session)
      (setq elmo-imap4-status-callback
            'elmo-imap4-server-diff-async-callback-1)
      (setq elmo-imap4-status-callback-data
            elmo-imap4-server-diff-async-callback-data))
    (elmo-imap4-send-command session
                             (list
                              "status"
                              (elmo-imap4-mailbox
                               (elmo-imap4-folder-mailbox-internal folder))
                              "(recent unseen messages)"))))

;;; IMAP parser.

(defvar elmo-imap4-server-eol "\r\n"
  "The EOL string sent from the server.")

(defvar elmo-imap4-client-eol "\r\n"
  "The EOL string we send to the server.")

(defvar elmo-imap4-literal-progress-reporter nil)

(defun elmo-imap4-find-next-line ()
  "Return point at end of current line, taking into account literals.
Return nil if no complete line has arrived."
  (when (re-search-forward (concat elmo-imap4-server-eol "\\|{\\([0-9]+\\)}"
                                   elmo-imap4-server-eol)
                           nil t)
    (if (match-string 1)
        (if (< (point-max) (+ (point) (string-to-number (match-string 1))))
            (progn
              (when elmo-imap4-literal-progress-reporter
                (elmo-progress-notify
                 'elmo-retrieve-message
                 :set (- (point-max) (point))
                 :total (string-to-number (match-string 1))))
              nil)
          (goto-char (+ (point) (string-to-number (match-string 1))))
          (elmo-imap4-find-next-line))
      (point))))

(defun elmo-imap4-sentinel (process string)
  (delete-process process))

(defun elmo-imap4-arrival-filter (proc string)
  "IMAP process filter."
  (when (buffer-live-p (process-buffer proc))
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert string)
    (let (end)
      (goto-char (point-min))
      (while (setq end (elmo-imap4-find-next-line))
        (save-restriction
          (narrow-to-region (point-min) end)
          (delete-char (- (length elmo-imap4-server-eol)))
          (goto-char (point-min))
          (unwind-protect
              (case elmo-imap4-status
                (initial
                 (setq elmo-imap4-current-response
                       (list
                        (list 'greeting (elmo-imap4-parse-greeting)))))
                ((auth nonauth selected examine)
                 (setq elmo-imap4-current-response
                       (cons (elmo-imap4-parse-response)
                             elmo-imap4-current-response)))
                (t
                 (message "Unknown state %s in arrival filter"
                          elmo-imap4-status)))
            (delete-region (point-min) (point-max)))))))))

;; IMAP parser.

(defsubst elmo-imap4-forward ()
  (or (eobp) (forward-char)))

(defsubst elmo-imap4-read-token ()
  (prog1 (let ((token (read (current-buffer))))
           (if (symbolp token)
               (intern (upcase (symbol-name token)))
             token))
    (elmo-imap4-forward)))

(defsubst elmo-imap4-parse-number ()
  (when (looking-at "[0-9]+")
    (prog1
        (string-to-number (match-string 0))
      (goto-char (match-end 0)))))

(defsubst elmo-imap4-parse-literal ()
  (when (looking-at "{\\([0-9]+\\)}\r\n")
    (let ((pos (match-end 0))
          (len (string-to-number (match-string 1))))
      (if (< (point-max) (+ pos len))
          nil
        (goto-char (+ pos len))
        (buffer-substring pos (+ pos len))))))
;;;	(list ' pos (+ pos len))))))

(defsubst elmo-imap4-parse-string ()
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
         (elmo-imap4-parse-literal))))

(defsubst elmo-imap4-parse-nil ()
  (if (looking-at "[Nn][Ii][Ll]")
      (goto-char (match-end 0))))

(defsubst elmo-imap4-parse-nstring ()
  (or (elmo-imap4-parse-string)
      (and (elmo-imap4-parse-nil)
           nil)))

(defsubst elmo-imap4-parse-astring ()
  (or (elmo-imap4-parse-string)
      (buffer-substring (point)
                        (if (re-search-forward "[(){ \r\n%*\"\\]" nil t)
                            (goto-char (1- (match-end 0)))
                          (end-of-line)
                          (point)))))

(defsubst elmo-imap4-parse-address ()
  (let (address)
    (when (eq (following-char) ?\()
      (elmo-imap4-forward)
      (setq address (vector (prog1 (elmo-imap4-parse-nstring)
                              (elmo-imap4-forward))
                            (prog1 (elmo-imap4-parse-nstring)
                              (elmo-imap4-forward))
                            (prog1 (elmo-imap4-parse-nstring)
                              (elmo-imap4-forward))
                            (elmo-imap4-parse-nstring)))
      (when (eq (following-char) ?\))
        (elmo-imap4-forward)
        address))))

(defsubst elmo-imap4-parse-address-list ()
  (if (eq (following-char) ?\()
      (let (address addresses)
        (elmo-imap4-forward)
        (while (and (not (eq (following-char) ?\)))
                    ;; next line for MS Exchange bug
                    (progn (and (eq (following-char) (string-to-char " "))
                                (elmo-imap4-forward)) t)
                    (setq address (elmo-imap4-parse-address)))
          (setq addresses (cons address addresses)))
        (when (eq (following-char) ?\))
          (elmo-imap4-forward)
          (nreverse addresses)))
    (assert (elmo-imap4-parse-nil))))

(defsubst elmo-imap4-parse-mailbox ()
  (let ((mailbox (elmo-imap4-parse-astring)))
    (if (string-equal "INBOX" (upcase mailbox))
        "INBOX"
      mailbox)))

(defun elmo-imap4-parse-greeting ()
  "Parse a IMAP greeting."
  (let ((case-fold-search t))
    (cond ((looking-at "\\* OK ")
           (setq elmo-imap4-status 'nonauth))
          ((looking-at "\\* PREAUTH ")
           (setq elmo-imap4-status 'auth))
          ((looking-at "\\* BYE ")
           (setq elmo-imap4-status 'closed)))))

(defun elmo-imap4-parse-response ()
  "Parse a IMAP command response."
  (elmo-imap4-debug "[%s] -> %s" (format-time-string "%T") (buffer-substring (point) (point-max)))
  (let ((token (read (current-buffer))))
    (case token
      (+ (progn
           (skip-chars-forward " ")
           (list 'continue-req (buffer-substring (point) (point-max)))))
      (* (case (setq token (elmo-imap4-read-token))
           (OK         (elmo-imap4-parse-resp-text-code))
           (NO         (elmo-imap4-parse-resp-text-code))
           (BAD        (elmo-imap4-parse-resp-text-code))
           (BYE        (elmo-imap4-parse-bye))
           (FLAGS      (list 'flags
                             (elmo-imap4-parse-flag-list)))
           (LIST       (list 'list (elmo-imap4-parse-data-list)))
           (LSUB       (list 'lsub (elmo-imap4-parse-data-list)))
           (SEARCH     (list
                        'search
                        (read (concat "("
                                      (buffer-substring (point) (point-max))
                                      ")"))))
           (ESEARCH     (list
                         'esearch
                         (cddr (split-string (buffer-substring (point) (point-max)) " "))))
           (STATUS     (elmo-imap4-parse-status))
           ;; Added
           (NAMESPACE  (elmo-imap4-parse-namespace))
           (CAPABILITY (list 'capability
                             (read
                              (concat "(" (downcase (buffer-substring
                                                     (point) (point-max)))
                                      ")"))))
           (ACL (elmo-imap4-parse-acl))
           (t       (case (elmo-imap4-read-token)
                      (EXISTS  (list 'exists token))
                      (RECENT  (list 'recent token))
                      (EXPUNGE (list 'expunge token))
                      (FETCH   (elmo-imap4-parse-fetch token))
                      (t       (list 'garbage (buffer-string)))))))
      (t (if (not (string-match elmo-imap4-seq-prefix (symbol-name token)))
             (list 'garbage (buffer-string))
           (case (elmo-imap4-read-token)
             (OK  (progn
                    (setq elmo-imap4-parsing nil)
                    (setq token (symbol-name token))
                    (elmo-unintern token)
                    (elmo-imap4-debug "*%s* OK arrived" token)
                    (setq elmo-imap4-reached-tag token)
                    (list 'ok (elmo-imap4-parse-resp-text-code))))
             (NO  (progn
                    (setq elmo-imap4-parsing nil)
                    (setq token (symbol-name token))
                    (elmo-unintern token)
                    (elmo-imap4-debug "*%s* NO arrived" token)
                    (setq elmo-imap4-reached-tag token)
                    (let (code text)
                      (when (eq (following-char) ?\[)
                        (setq code (buffer-substring (point)
                                                     (search-forward "]")))
                        (elmo-imap4-forward))
                      (setq text (buffer-substring (point) (point-max)))
                      (list 'no (list code text)))))
             (BAD (progn
                    (setq elmo-imap4-parsing nil)
                    (elmo-imap4-debug "*%s* BAD arrived" token)
                    (setq token (symbol-name token))
                    (elmo-unintern token)
                    (setq elmo-imap4-reached-tag token)
                    (let (code text)
                      (when (eq (following-char) ?\[)
                        (setq code (buffer-substring (point)
                                                     (search-forward "]")))
                        (elmo-imap4-forward))
                      (setq text (buffer-substring (point) (point-max)))
                      (list 'bad (list code text)))))
             (t   (list 'garbage (buffer-string)))))))))

(defun elmo-imap4-parse-bye ()
  (let (code text)
    (when (eq (following-char) ?\[)
      (setq code (buffer-substring (point)
                                   (search-forward "]")))
      (elmo-imap4-forward))
    (setq text (buffer-substring (point) (point-max)))
    (list 'bye (list code text))))

(defun elmo-imap4-parse-text ()
  (goto-char (point-min))
  (when (search-forward "[" nil t)
    (search-forward "]")
    (elmo-imap4-forward))
  (list 'text (buffer-substring (point) (point-max))))

(defun elmo-imap4-parse-resp-text-code ()
  (when (eq (following-char) ?\[)
    (elmo-imap4-forward)
    (let ((case-fold-search t))
      (cond ((search-forward "PERMANENTFLAGS " nil t)
             (list 'permanentflags (elmo-imap4-parse-flag-list)))
            ((search-forward "UIDNEXT " nil t)
             (list 'uidnext (read (current-buffer))))
            ((search-forward "UNSEEN " nil t)
             (list 'unseen (read (current-buffer))))
            ((looking-at "UIDVALIDITY \\([0-9]+\\)")
             (list 'uidvalidity (match-string 1)))
            ((search-forward "READ-ONLY" nil t)
             (list 'read-only t))
            ((search-forward "READ-WRITE" nil t)
             (list 'read-write t))
            ((search-forward "NEWNAME " nil t)
             (let (oldname newname)
               (setq oldname (elmo-imap4-parse-string))
               (elmo-imap4-forward)
               (setq newname (elmo-imap4-parse-string))
               (list 'newname newname oldname)))
            ((search-forward "TRYCREATE" nil t)
             (list 'trycreate t))
            ((looking-at "APPENDUID \\([0-9]+\\) \\([0-9]+\\)")
             (list 'appenduid
                   (list (match-string 1)
                         (string-to-number (match-string 2)))))
            ((looking-at "COPYUID \\([0-9]+\\) \\([0-9,:]+\\) \\([0-9,:]+\\)")
             (list 'copyuid (list (match-string 1)
                                  (match-string 2)
                                  (match-string 3))))
            ((search-forward "ALERT] " nil t)
             (let ((message (buffer-substring (point) (point-max))))
               (message "IMAP server information: %s" message)
               (list 'alert message)))
            (t (list 'unknown))))))

(defun elmo-imap4-parse-data-list ()
  (let (flags delimiter mailbox)
    (setq flags (elmo-imap4-parse-flag-list))
    (when (looking-at " [Nn][Ii][Ll]\\| \"\\\\?\\(.\\)\"")
      (setq delimiter (match-string 1))
      (goto-char (1+ (match-end 0)))
      (when (setq mailbox (elmo-imap4-parse-mailbox))
        (list mailbox flags delimiter)))))

(defsubst elmo-imap4-parse-header-list ()
  (when (eq (following-char) ?\()
    (let (strlist)
      (while (not (eq (following-char) ?\)))
        (elmo-imap4-forward)
        (push (elmo-imap4-parse-astring) strlist))
      (elmo-imap4-forward)
      (nreverse strlist))))

(defsubst elmo-imap4-parse-fetch-body-section ()
  (let ((section
         (buffer-substring (point)
                           (1-
                            (progn (re-search-forward "[] ]" nil t)
                                   (point))))))
    (if (eq (preceding-char) (string-to-char " "))
        (prog1
            (mapconcat 'identity
                       (cons section (elmo-imap4-parse-header-list)) " ")
          (search-forward "]" nil t))
      section)))

(defun elmo-imap4-parse-fetch (response)
  (when (eq (following-char) ?\()
    (let (element list)
      (while (not (eq (following-char) ?\)))
        (elmo-imap4-forward)
        (let ((token (elmo-imap4-read-token)))
          (setq element
                (cond ((eq token 'UID)
                       (list 'uid (condition-case nil
                                      (read (current-buffer))
                                    (error nil))))
                      ((eq token 'FLAGS)
                       (list 'flags (elmo-imap4-parse-flag-list)))
                      ((eq token 'ENVELOPE)
                       (list 'envelope (elmo-imap4-parse-envelope)))
                      ((eq token 'INTERNALDATE)
                       (list 'internaldate (elmo-imap4-parse-string)))
                      ((eq token 'RFC822)
                       (list 'rfc822 (elmo-imap4-parse-nstring)))
                      ((eq token (intern elmo-imap4-rfc822-header))
                       (list 'rfc822header (elmo-imap4-parse-nstring)))
                      ((eq token (intern elmo-imap4-rfc822-text))
                       (list 'rfc822text (elmo-imap4-parse-nstring)))
                      ((eq token (intern elmo-imap4-rfc822-size))
                       (list 'rfc822size (read (current-buffer))))
                      ((eq token 'BODY)
                       (if (eq (preceding-char) ?\[)
                           (list
                            'bodydetail
                            (upcase (elmo-imap4-parse-fetch-body-section))
                            (and
                             (eq (following-char) ?<)
                             (buffer-substring (1+ (point))
                                               (progn
                                                 (search-forward ">" nil t)
                                                 (point))))
                            (progn (elmo-imap4-forward)
                                   (elmo-imap4-parse-nstring)))
                         (list 'body (elmo-imap4-parse-body))))
                      ((eq token 'BODYSTRUCTURE)
                       (list 'bodystructure (elmo-imap4-parse-body)))))
          (setq list (cons element list))))
      (and elmo-imap4-fetch-callback
           (funcall elmo-imap4-fetch-callback
                    list elmo-imap4-fetch-callback-data))
      (list 'fetch list))))

(defun elmo-imap4-parse-status ()
  (let ((mailbox (elmo-imap4-parse-mailbox))
        status)
    (when (and mailbox (search-forward "(" nil t))
      (while (not (eq (following-char) ?\)))
        (setq status
              (cons
               (let ((token (read (current-buffer))))
                 (case (intern (upcase (symbol-name token)))
                   (MESSAGES
                    (list 'messages (read (current-buffer))))
                   (RECENT
                    (list 'recent (read (current-buffer))))
                   (UIDNEXT
                    (list 'uidnext (read (current-buffer))))
                   (UIDVALIDITY
                    (and (looking-at " \\([0-9]+\\)")
                         (prog1 (list 'uidvalidity (match-string 1))
                           (goto-char (match-end 1)))))
                   (UNSEEN
                    (list 'unseen (read (current-buffer))))
                   (t
                    (message
                     "Unknown status data %s in mailbox %s ignored"
                     token mailbox))))
               status))
        (skip-chars-forward " ")))
    (and elmo-imap4-status-callback
         (funcall elmo-imap4-status-callback
                  status
                  elmo-imap4-status-callback-data))
    (list 'status status)))


(defmacro elmo-imap4-value (value)
  `(if (eq ,value 'NIL)
       nil
     ,value))

(defmacro elmo-imap4-nth (pos list)
  `(let ((value (nth ,pos ,list)))
     (elmo-imap4-value value)))

(defun elmo-imap4-parse-namespace ()
  (list 'namespace
        (nconc
         (copy-sequence elmo-imap4-extra-namespace-alist)
         (elmo-imap4-parse-namespace-subr
          (read (concat "(" (buffer-substring
                             (point) (point-max))
                        ")"))))))

(defun elmo-imap4-parse-namespace-subr (ns)
  (let (prefix delim namespace-alist default-delim)
    ;; 0: personal, 1: other, 2: shared
    (dotimes (i 3)
      (setq namespace-alist
            (nconc namespace-alist
                   (delq nil
                         (mapcar
                          (lambda (namespace)
                            (setq prefix (elmo-imap4-nth 0 namespace)
                                  delim (elmo-imap4-nth 1 namespace))
                            (if (and prefix delim
                                     (string-match
                                      (concat (regexp-quote delim) "\\'")
                                      prefix))
                                (setq prefix (substring prefix 0
                                                        (match-beginning 0))))
                            (if (zerop (length prefix))
                                (progn (setq default-delim delim) nil)
                              (cons
                               (concat "^\\("
                                       (if (string= (downcase prefix) "inbox")
                                           "[Ii][Nn][Bb][Oo][Xx]"
                                         (regexp-quote prefix))
                                       "\\).*$")
                               delim)))
                          (elmo-imap4-nth i ns))))))
    (if default-delim
        (setq namespace-alist
              (nconc namespace-alist
                     (list (cons "^.*$" default-delim)))))
    namespace-alist))

(defun elmo-imap4-parse-acl ()
  (let ((mailbox (elmo-imap4-parse-mailbox))
        identifier rights acl)
    (while (eq (following-char) (string-to-char " "))
      (elmo-imap4-forward)
      (setq identifier (elmo-imap4-parse-astring))
      (elmo-imap4-forward)
      (setq rights (elmo-imap4-parse-astring))
      (setq acl (append acl (list (cons identifier rights)))))
    (list 'acl acl mailbox)))

(defun elmo-imap4-parse-flag-list ()
  (let ((str (buffer-substring (+ (point) 1)
                               (progn (search-forward ")" nil t)
                                      (- (point) 1)))))
    (unless (zerop (length str))
      (split-string str))))

(defun elmo-imap4-parse-envelope ()
  (when (eq (following-char) ?\()
    (elmo-imap4-forward)
    (vector (prog1 (elmo-imap4-parse-nstring);; date
              (elmo-imap4-forward))
            (prog1 (elmo-imap4-parse-nstring);; subject
              (elmo-imap4-forward))
            (prog1 (elmo-imap4-parse-address-list);; from
              (elmo-imap4-forward))
            (prog1 (elmo-imap4-parse-address-list);; sender
              (elmo-imap4-forward))
            (prog1 (elmo-imap4-parse-address-list);; reply-to
              (elmo-imap4-forward))
            (prog1 (elmo-imap4-parse-address-list);; to
              (elmo-imap4-forward))
            (prog1 (elmo-imap4-parse-address-list);; cc
              (elmo-imap4-forward))
            (prog1 (elmo-imap4-parse-address-list);; bcc
              (elmo-imap4-forward))
            (prog1 (elmo-imap4-parse-nstring);; in-reply-to
              (elmo-imap4-forward))
            (prog1 (elmo-imap4-parse-nstring);; message-id
              (elmo-imap4-forward)))))

(defsubst elmo-imap4-parse-string-list ()
  (cond ((eq (following-char) ?\();; body-fld-param
         (let (strlist str)
           (elmo-imap4-forward)
           (while (setq str (elmo-imap4-parse-string))
             (push str strlist)
             (elmo-imap4-forward))
           (nreverse strlist)))
        ((elmo-imap4-parse-nil)
         nil)))

(defun elmo-imap4-parse-body-extension ()
  (if (eq (following-char) ?\()
      (let (b-e)
        (elmo-imap4-forward)
        (push (elmo-imap4-parse-body-extension) b-e)
        (while (eq (following-char) (string-to-char " "))
          (elmo-imap4-forward)
          (push (elmo-imap4-parse-body-extension) b-e))
        (assert (eq (following-char) ?\)))
        (elmo-imap4-forward)
        (nreverse b-e))
    (or (elmo-imap4-parse-number)
        (elmo-imap4-parse-nstring))))

(defsubst elmo-imap4-parse-body-ext ()
  (let (ext)
    (when (eq (following-char) (string-to-char " ")) ; body-fld-dsp
      (elmo-imap4-forward)
      (let (dsp)
        (if (eq (following-char) ?\()
            (progn
              (elmo-imap4-forward)
              (push (elmo-imap4-parse-string) dsp)
              (elmo-imap4-forward)
              (push (elmo-imap4-parse-string-list) dsp)
              (elmo-imap4-forward))
          (assert (elmo-imap4-parse-nil)))
        (push (nreverse dsp) ext))
      (when (eq (following-char) (string-to-char " ")) ; body-fld-lang
        (elmo-imap4-forward)
        (if (eq (following-char) ?\()
            (push (elmo-imap4-parse-string-list) ext)
          (push (elmo-imap4-parse-nstring) ext))
        (while (eq (following-char) (string-to-char " "));; body-extension
          (elmo-imap4-forward)
          (setq ext (append (elmo-imap4-parse-body-extension) ext)))))
    ext))

(defun elmo-imap4-parse-body ()
  (let (body)
    (when (eq (following-char) ?\()
      (elmo-imap4-forward)
      (if (eq (following-char) ?\()
          (let (subbody)
            (while (and (eq (following-char) ?\()
                        (setq subbody (elmo-imap4-parse-body)))
              (push subbody body))
            (elmo-imap4-forward)
            (push (elmo-imap4-parse-string) body);; media-subtype
            (when (eq (following-char) (string-to-char " ")) ; body-ext-mpart:
              (elmo-imap4-forward)
              (if (eq (following-char) ?\();; body-fld-param
                  (push (elmo-imap4-parse-string-list) body)
                (push (and (elmo-imap4-parse-nil) nil) body))
              (setq body
                    (append (elmo-imap4-parse-body-ext) body)));; body-ext-...
            (assert (eq (following-char) ?\)))
            (elmo-imap4-forward)
            (nreverse body))

        (push (elmo-imap4-parse-string) body);; media-type
        (elmo-imap4-forward)
        (push (elmo-imap4-parse-string) body);; media-subtype
        (elmo-imap4-forward)
        ;; next line for Sun SIMS bug
        (and (eq (following-char) (string-to-char " "))
             (elmo-imap4-forward))
        (if (eq (following-char) ?\();; body-fld-param
            (push (elmo-imap4-parse-string-list) body)
          (push (and (elmo-imap4-parse-nil) nil) body))
        (elmo-imap4-forward)
        (push (elmo-imap4-parse-nstring) body);; body-fld-id
        (elmo-imap4-forward)
        (push (elmo-imap4-parse-nstring) body);; body-fld-desc
        (elmo-imap4-forward)
        (push (elmo-imap4-parse-string) body);; body-fld-enc
        (elmo-imap4-forward)
        (push (elmo-imap4-parse-number) body);; body-fld-octets

        ;; ok, we're done parsing the required parts, what comes now is one
        ;; of three things:
        ;;
        ;; envelope       (then we're parsing body-type-msg)
        ;; body-fld-lines (then we're parsing body-type-text)
        ;; body-ext-1part (then we're parsing body-type-basic)
        ;;
        ;; the problem is that the two first are in turn optionally followed
        ;; by the third.  So we parse the first two here (if there are any)...

        (when (eq (following-char) (string-to-char " "))
          (elmo-imap4-forward)
          (let (lines)
            (cond ((eq (following-char) ?\();; body-type-msg:
                   (push (elmo-imap4-parse-envelope) body);; envelope
                   (elmo-imap4-forward)
                   (push (elmo-imap4-parse-body) body);; body
                   (elmo-imap4-forward)
                   (push (elmo-imap4-parse-number) body));; body-fld-lines
                  ((setq lines (elmo-imap4-parse-number));; body-type-text:
                   (push lines body));; body-fld-lines
                  (t
                   (backward-char)))));; no match...

        ;; ...and then parse the third one here...

        (when (eq (following-char) (string-to-char " ")) ; body-ext-1part:
          (elmo-imap4-forward)
          (push (elmo-imap4-parse-nstring) body);; body-fld-md5
          (setq body
                (append (elmo-imap4-parse-body-ext) body)));; body-ext-1part..

        (assert (eq (following-char) ?\)))
        (elmo-imap4-forward)
        (nreverse body)))))

(luna-define-method elmo-folder-initialize ((folder elmo-imap4-folder) name)
  (let ((default-user	elmo-imap4-default-user)
        (default-server elmo-imap4-default-server)
        (default-port	elmo-imap4-default-port)
        (elmo-network-stream-type-alist
         (if elmo-imap4-stream-type-alist
             (append elmo-imap4-stream-type-alist
                     elmo-network-stream-type-alist)
           elmo-network-stream-type-alist))
        tokens)
    (when (string-match "\\(.*\\)@\\(.*\\)" default-server)
      ;; case: imap4-default-server is specified like
      ;; "hoge%imap.server@gateway".
      (setq default-user (match-string 1 default-server))
      (setq default-server (match-string 2 default-server)))
    (setq tokens (car (elmo-parse-separated-tokens
                       name
                       elmo-imap4-folder-name-syntax)))
    ;; mailbox
    (elmo-imap4-folder-set-mailbox-internal folder
                                            (elmo-imap4-encode-folder-string
                                             (cdr (assq 'mailbox tokens))))
    ;; user
    (elmo-net-folder-set-user-internal folder
                                       (or (cdr (assq 'user tokens))
                                           default-user))
    ;; auth
    (elmo-net-folder-set-auth-internal
     folder
     (let ((auth (cdr (assq 'auth tokens))))
       (or (and auth (intern auth))
           elmo-imap4-default-authenticate-type
           'clear)))
    ;; network
    (elmo-net-folder-set-parameters
     folder
     tokens
     (list :server	default-server
           :port	default-port
           :stream-type
           (elmo-get-network-stream-type elmo-imap4-default-stream-type)))
    folder))

;;; ELMO IMAP4 folder
(luna-define-method elmo-folder-expand-msgdb-path ((folder
                                                    elmo-imap4-folder))
  (convert-standard-filename
   (let ((mailbox (elmo-imap4-folder-mailbox-internal folder)))
     (if (string= "inbox" (downcase mailbox))
         (setq mailbox "inbox"))
     (if (eq (string-to-char mailbox) ?/)
         (setq mailbox (substring mailbox 1 (length mailbox))))
     ;; don't use expand-file-name (e.g. %~/something)
     (concat
      (expand-file-name
       (or (elmo-net-folder-user-internal folder) "nobody")
       (expand-file-name (or (elmo-net-folder-server-internal folder)
                             "nowhere")
                         (expand-file-name
                          "imap"
                          elmo-msgdb-directory)))
      "/" mailbox))))

(luna-define-method elmo-folder-status-plugged ((folder
                                                 elmo-imap4-folder))
  (elmo-imap4-folder-status-plugged folder))

(defun elmo-imap4-folder-status-plugged (folder)
  (let* ((session (elmo-imap4-get-session folder))
         (selected (elmo-imap4-mailbox-selected-p
                    (elmo-imap4-folder-mailbox-internal folder) session))
         status)
    (with-current-buffer (elmo-network-session-buffer session)
      (setq elmo-imap4-status-callback nil)
      (setq elmo-imap4-status-callback-data nil))
    (cond
     ((and selected (not elmo-imap4-use-select-to-update-status))
      (elmo-imap4-session-unselect-mailbox
       session
       (elmo-imap4-folder-mailbox-internal folder)))
     ((and (not selected) elmo-imap4-use-select-to-update-status)
      ;; This will result in a violation of RFC3501: calling STATUS on
      ;; a selected mailbox.
      (elmo-imap4-session-select-mailbox
       session
       (elmo-imap4-folder-mailbox-internal folder))))
    (setq status (elmo-imap4-response-value
                  (elmo-imap4-send-command-wait
                   session
                   (list "status"
                         (elmo-imap4-mailbox
                          (elmo-imap4-folder-mailbox-internal folder))
                         "(uidnext messages)"))
                  'status))
    (cons
     (- (elmo-imap4-response-value status 'uidnext) 1)
     (elmo-imap4-response-value status 'messages))))

(defun elmo-imap4-folder-list-range (folder min max)
  (elmo-imap4-list
   folder
   (concat
    (let ((killed (elmo-folder-killed-list-internal folder)))
      (if (and killed
               (eq (length killed) 1)
               (consp (car killed))
               (eq (car (car killed)) 1))
          ;; What about elmo-imap4-use-uid?
          (format "uid %d:%s" (cdr (car killed)) max)
        (format "uid %s:%s" min max)))
    " undeleted")))

(luna-define-method elmo-folder-list-messages-plugged
  ((folder elmo-imap4-folder) &optional enable-killed)
  (let* ((old (elmo-msgdb-list-messages (elmo-folder-msgdb folder)))
         (new (elmo-imap4-folder-list-range
               folder (1+ (or (elmo-folder-get-info-max folder) 0)) "*"))
         (united-old-new (elmo-union old new)))
    (if (= (length united-old-new) (or (elmo-folder-get-info-length folder) 0))
        united-old-new
      (elmo-union new
                  (elmo-imap4-folder-list-range
                   folder
                   1 (1+ (or (elmo-folder-get-info-max folder) 0)))))))

(luna-define-method elmo-folder-list-flagged-plugged
  ((folder elmo-imap4-folder) flag)
  (elmo-imap4-folder-list-flagged folder flag))

(luna-define-method elmo-folder-merge-flagged ((folder elmo-imap4-folder) local remote)
  (case elmo-imap4-flags-sync-method
    (union (elmo-sort-uniq-number-list (nconc local remote)))
    (server->client remote)
    (otherwise (error "Unknown method for syncing flags: %s" elmo-imap4-flags-sync-method))))

(luna-define-method elmo-folder-use-flag-p ((folder elmo-imap4-folder))
  (not (string-match elmo-imap4-disuse-server-flag-mailbox-regexp
                     (elmo-imap4-folder-mailbox-internal folder))))

(luna-define-method elmo-folder-list-subfolders ((folder elmo-imap4-folder)
                                                 &optional one-level)
  (let* ((root (elmo-imap4-folder-mailbox-internal folder))
         (session (elmo-imap4-get-session folder))
         (prefix (elmo-folder-prefix-internal folder))
         (namespace-assoc
                  (elmo-string-matched-assoc
                   root
                   (with-current-buffer (elmo-network-session-buffer session)
                     elmo-imap4-server-namespace)))
         (delim (or (cdr namespace-assoc)
                 elmo-imap4-default-hierarchy-delimiter))
         ;; Append delimiter when root with namespace.
         (root-nodelim root)
         (root (if (and namespace-assoc
                        (match-end 1)
                        (string= (substring root (match-end 1))
                                 ""))
                   (concat root delim)
                 root))
         result append-serv type)
    (setq result (elmo-imap4-response-get-selectable-mailbox-list
                  (elmo-imap4-send-command-wait
                   session
                   (list "list" (elmo-imap4-mailbox root) "*"))))
    ;; The response of Courier-imap doesn't contain a specified folder itself.
    (unless (member root result)
      (setq result
            (append result
                    (elmo-imap4-response-get-selectable-mailbox-list
                     (elmo-imap4-send-command-wait
                      session
                      (list "list \"\"" (elmo-imap4-mailbox
                                          root-nodelim)))))))
    (when (or (not (string= (elmo-net-folder-user-internal folder)
                            elmo-imap4-default-user))
              (not (eq (elmo-net-folder-auth-internal folder)
                       (or elmo-imap4-default-authenticate-type 'clear))))
      (setq append-serv (concat ":"
                                (elmo-quote-syntactical-element
                                 (elmo-net-folder-user-internal folder)
                                 'user elmo-imap4-folder-name-syntax))))
    (unless (eq (elmo-net-folder-auth-internal folder)
                (or elmo-imap4-default-authenticate-type 'clear))
      (setq append-serv
            (concat append-serv "/"
                    (symbol-name (elmo-net-folder-auth-internal folder)))))
    (unless (string= (elmo-net-folder-server-internal folder)
                     elmo-imap4-default-server)
      (setq append-serv (concat append-serv "@"
                                (elmo-net-folder-server-internal folder))))
    (unless (eq (elmo-net-folder-port-internal folder) elmo-imap4-default-port)
      (setq append-serv (concat append-serv ":"
                                (number-to-string
                                 (elmo-net-folder-port-internal folder)))))
    (setq type (elmo-net-folder-stream-type-internal folder))
    (unless (eq (elmo-network-stream-type-symbol type)
                elmo-imap4-default-stream-type)
      (if type
          (setq append-serv (concat append-serv
                                    (elmo-network-stream-type-spec-string
                                     type)))))
    (if one-level
        (let ((re-delim (regexp-quote delim))
              (case-fold-search nil)
              folder ret has-child-p)
          ;; Append delimiter
          (when (and root
                     (not (string= root ""))
                     (not (string-match
                           (concat "\\(.*\\)" re-delim "\\'")
                           root)))
            (setq root (concat root delim)))
          (while (setq folder (car result))
            (setq has-child-p
                  (when (string-match
                         (concat "^\\(" (regexp-quote root) "[^" re-delim "]" "+\\)"
                                 re-delim)
                         folder)
                    (setq folder (match-string 1 folder))))
            (setq result (delq
                          nil
                          (mapcar (lambda (fld)
                                    (if (string-match
                                         (concat "^" (regexp-quote folder)
                                                 "\\(" re-delim "\\|\\'\\)")
                                         fld)
                                        (progn (setq has-child-p t) nil)
                                      fld))
                                  (cdr result)))
                  folder (concat prefix
                                 (elmo-quote-syntactical-element
                                  (elmo-imap4-decode-folder-string folder)
                                  'mailbox elmo-imap4-folder-name-syntax)
                                 (and append-serv
                                      (eval append-serv)))
                  ret (append ret (if has-child-p
                                      (list (list folder))
                                    (list folder)))))
          ret)
      (mapcar (lambda (fld)
                (concat prefix
                        (elmo-quote-syntactical-element
                         (elmo-imap4-decode-folder-string fld)
                         'mailbox elmo-imap4-folder-name-syntax)
                        (and append-serv
                             (eval append-serv))))
              result))))

(luna-define-method elmo-folder-exists-p-plugged ((folder elmo-imap4-folder))
  (let ((session (elmo-imap4-get-session folder)))
    (if (elmo-imap4-mailbox-selected-p
         (elmo-imap4-folder-mailbox-internal folder) session)
        t
      (elmo-imap4-session-select-mailbox
       session
       (elmo-imap4-folder-mailbox-internal folder)
       'force 'notify-bye))))

(luna-define-method elmo-folder-creatable-p ((folder elmo-imap4-folder))
  t)

(luna-define-method elmo-folder-writable-p ((folder elmo-imap4-folder))
  t)

(luna-define-method elmo-folder-delete ((folder elmo-imap4-folder))
  (let* ((exists (elmo-folder-exists-p folder))
         (msgs (and exists
                    (elmo-folder-list-messages folder))))
    (when (yes-or-no-p (format "%sDelete msgdb and substance of \"%s\"? "
                               (if (> (length msgs) 0)
                                   (format "%d msg(s) exists. " (length msgs))
                                 "")
                               (elmo-folder-name-internal folder)))
      (let ((session (elmo-imap4-get-session folder)))
        (when (elmo-imap4-folder-mailbox-internal folder)
          (when msgs (elmo-folder-delete-messages-internal folder msgs))
          ;; close selected mailbox except one with \Noselect attribute
          (when exists
            (elmo-imap4-send-command-wait session "close"))
          (elmo-imap4-send-command-wait
           session
           (list "delete"
                 (elmo-imap4-mailbox
                  (elmo-imap4-folder-mailbox-internal folder)))))
        (elmo-imap4-session-set-current-mailbox-internal session nil)
        (elmo-imap4-session-set-current-mailbox-size-internal session nil)
        )
      (elmo-msgdb-delete-path folder)
      t)))

(luna-define-method elmo-folder-rename-internal ((folder elmo-imap4-folder)
                                                 new-folder)
  (let ((session (elmo-imap4-get-session folder)))
    ;; make sure the folder is selected.
    (elmo-imap4-session-select-mailbox session
                                       (elmo-imap4-folder-mailbox-internal
                                        folder))
    (elmo-imap4-session-unselect-mailbox session
                                         (elmo-imap4-folder-mailbox-internal
                                          folder))
    (elmo-imap4-send-command-wait session "close")
    (elmo-imap4-send-command-wait
     session
     (list "rename"
           (elmo-imap4-mailbox
            (elmo-imap4-folder-mailbox-internal folder))
           " "
           (elmo-imap4-mailbox
            (elmo-imap4-folder-mailbox-internal new-folder))))
    (elmo-imap4-session-set-current-mailbox-internal
     session (elmo-imap4-folder-mailbox-internal new-folder))))

(defun elmo-imap4-copy-messages (src-folder dst-folder numbers)
  (let ((session (elmo-imap4-get-session src-folder))
        (set-list (elmo-imap4-make-number-set-list
                   numbers
                   elmo-imap4-number-set-chop-length))
        succeeds)
    (elmo-imap4-session-select-mailbox session
                                       (elmo-imap4-folder-mailbox-internal
                                        src-folder))
    (while set-list
      (if (elmo-imap4-send-command-wait session
                                        (list
                                         (format
                                          (if elmo-imap4-use-uid
                                              "uid copy %s"
                                            "copy %s")
                                          (cdr (car set-list)))
                                         (elmo-imap4-mailbox
                                          (elmo-imap4-folder-mailbox-internal
                                           dst-folder))))
          (setq succeeds (append succeeds numbers)))
      (setq set-list (cdr set-list)))
    succeeds))

(defun elmo-imap4-set-flag (folder numbers flag &optional remove)
  "Set flag on messages.
FOLDER is the ELMO folder structure.
NUMBERS is the message numbers to be flagged.
FLAG is the flag name.
If optional argument REMOVE is non-nil, remove FLAG."
  (let ((session (elmo-imap4-get-session folder))
        response set-list)
    (elmo-imap4-session-select-mailbox session
                                       (elmo-imap4-folder-mailbox-internal
                                        folder))
    (when (or (elmo-string-member-ignore-case
               flag
               (elmo-imap4-session-flags-internal session))
              (member "\\*" (elmo-imap4-session-flags-internal session))
              (string= flag "\\Deleted")) ; XXX Humm..
      (setq set-list (elmo-imap4-make-number-set-list
                      numbers
                      elmo-imap4-number-set-chop-length))
      (while set-list
        (with-current-buffer (elmo-network-session-buffer session)
          (setq elmo-imap4-fetch-callback nil)
          (setq elmo-imap4-fetch-callback-data nil))
        (unless (elmo-imap4-response-ok-p
                 (elmo-imap4-send-command-wait
                  session
                  (format
                   (if elmo-imap4-use-uid
                       "uid store %s %sflags.silent (%s)"
                     "store %s %sflags.silent (%s)")
                   (cdr (car set-list))
                   (if remove "-" "+")
                   flag)))
          (setq response 'fail))
        (setq set-list (cdr set-list)))
      (not (eq response 'fail)))))

(luna-define-method elmo-folder-delete-messages-plugged
  ((folder elmo-imap4-folder) numbers)
  (let ((session (elmo-imap4-get-session folder))
        (deleted (elmo-imap4-list folder "deleted")))
    (elmo-imap4-session-select-mailbox
     session
     (elmo-imap4-folder-mailbox-internal folder))
    (unless (elmo-imap4-set-flag folder numbers "\\Deleted")
      (error "Failed to set deleted flag"))
    (cond
     ((and deleted elmo-imap4-use-uid
           (elmo-imap4-session-capable-p session 'uidplus))
      (elmo-imap4-send-command-wait
       session (list
                "uid expunge"
                (mapconcat 'number-to-string numbers ","))))
     ((and deleted elmo-imap4-use-uid)
      (unless (elmo-imap4-set-flag folder deleted "\\Deleted" 'remove)
        (error "Failed to temporarily remove deleted flag"))
      (elmo-imap4-send-command-wait session "expunge")
      (unless (elmo-imap4-set-flag folder deleted "\\Deleted")
        (error "Failed to restore deleted flags")))
     ((or (null deleted)
          (y-or-n-p "There are hidden deleted messages.  Expunge anyway?"))
      (elmo-imap4-send-command-wait session "expunge")))
    t))

(defun elmo-imap4-detect-search-charset (string)
  (with-temp-buffer
    (insert string)
    (detect-mime-charset-region (point-min) (point-max))))

(defun elmo-imap4-search-build-full-command (search)
  "Process charset at beginning of SEARCH and build a full IMAP
search command."
  (append '("uid search") (when (car search) (list "CHARSET" (car search))) (cdr search)))

(defun elmo-imap4-search-perform (session search-or-uids)
  "Perform an IMAP search.

SESSION is an imap session.

SEARCH-OR-UIDS is either a list of UIDs or a list of the
form (CHARSET IMAP-SEARCH-COMMAND ...) which is to be evaluated.

Returns a list of UIDs."
  (if (numberp (car search-or-uids))
      search-or-uids
    (elmo-imap4-response-value
     (elmo-imap4-send-command-wait
      session
      (elmo-imap4-search-build-full-command search-or-uids))
     'search)))

(defun elmo-imap4-search-generate-vector (folder filter from-msgs)
  (let ((search-key (elmo-filter-key filter)))
    (cond
     ((string= "last" search-key)
      (let ((numbers (or from-msgs (elmo-folder-list-messages folder))))
        (nthcdr (max (- (length numbers)
                        (string-to-number (elmo-filter-value filter)))
                     0)
                numbers)))
     ((string= "first" search-key)
      (let ((numbers (or from-msgs (elmo-folder-list-messages folder))))
        (car
         (elmo-list-diff
          numbers
          (nthcdr (string-to-number (elmo-filter-value filter)) numbers)))))
     ((string= "flag" search-key)
      (list nil
            (if (eq (elmo-filter-type filter) 'unmatch) "not " "")
            (elmo-imap4-flag-to-imap-criteria
             (intern (elmo-filter-value filter)))))
     ((or (string= "since" search-key)
          (string= "before" search-key))
      (list
       nil
       (if (eq (elmo-filter-type filter)
               'unmatch)
           "not" "")
       (concat "sent" search-key)
       (elmo-date-get-description
        (elmo-date-get-datevec
         (elmo-filter-value filter)))))
     (t
      (let ((charset (elmo-imap4-detect-search-charset
                      (elmo-filter-value filter))))
        (when (string= search-key "raw-body")
          (setq search-key "body"))
        (list
         (elmo-imap4-astring
          (symbol-name charset))
         (if (eq (elmo-filter-type filter)
                 'unmatch)
             "not" "")
         (if (member search-key elmo-imap4-search-keys)
             search-key
           (format "header %s" search-key))
         (elmo-imap4-astring
          (encode-mime-charset-string
           (elmo-filter-value filter) charset))))))))

(defun elmo-imap4-search-mergeable-p (a b)
  "Return t if A and B are two mergeable IMAP searches.

A is the result of a call to elmo-imap4-search-generate.
B is the result of a call to elmo-imap4-search-generate."
  (let ((cara (car a))
        (carb (car b)))
    (and (not (numberp cara))
         (not (numberp carb))
         (or (null cara)
             (null carb)
             (equal cara carb)))))

(defun elmo-imap4-search-mergeable-charset (a b)
  "Return the charset of two searches for merging.

A is the result of a call to elmo-imap4-search-generate.
B is the result of a call to elmo-imap4-search-generate."
  (or (car a)
      (car b)))

(defun elmo-imap4-search-generate-uid (msgs)
  "Return a search for a set of msgs.

A search is a list of the form (CHARSET IMAP-SEARCH-COMMAND ...)
which is to be evaluated at a future time."
  (list nil
        (concat "uid "
                (cdr (car
                      (elmo-imap4-make-number-set-list msgs))))))

(defun elmo-imap4-search-generate-and (session a b)
  "Return a search that returns the intersection of A and B in SESSION.

SESSION is an imap session.
A is the result of a call to elmo-imap4-search-generate.
B is the result of a call to elmo-imap4-search-generate.

A search is either a list of UIDs or a list of the form (CHARSET
IMAP-SEARCH-COMMAND ...) which is to be evaluated at a future
time."
  (if (elmo-imap4-search-mergeable-p a b)
      (append (list (elmo-imap4-search-mergeable-charset a b))
              (cdr a) (cdr b))
    (elmo-list-filter (elmo-imap4-search-perform session a)
                      (elmo-imap4-search-perform session b))))

(defun elmo-imap4-search-generate-or (session a b)
  "Return a search that returns the union of A and B in SESSION.

SESSION is an imap session.
A is the result of a call to elmo-imap4-search-generate.
B is the result of a call to elmo-imap4-search-generate.

A search is either a list of UIDs or a list of the form (CHARSET
IMAP-SEARCH-COMMAND ...) which is to be evaluated at a future
time."
  (if (elmo-imap4-search-mergeable-p a b)
      (append (list (elmo-imap4-search-mergeable-charset a b))
              '("OR " "(") (cdr a) '(")" "(") (cdr b) '(")"))
    (elmo-uniq-list (append (elmo-imap4-search-perform session a)
                            (elmo-imap4-search-perform session b)))))

(defun elmo-imap4-search-generate (folder session condition from-msgs)
  "Return search in FOLDER for CONDITON and FROM-MSGS.

FOLDER is a elmo folder structure.
CONDITION is a search condition.
FROM-MSGS is a set of messages.  When nil, generate vector for all
messages in FOLDER.

A search is either a list of UIDs or a list of the form (CHARSET
IMAP-SEARCH-COMMAND ...) which is to be evaluated at a future
time."
  (if (vectorp condition)
      (elmo-imap4-search-generate-vector folder condition from-msgs)
    (let ((a (elmo-imap4-search-generate folder session (nth 1 condition)
                                          from-msgs))
          (b (elmo-imap4-search-generate folder session (nth 2 condition)
                                          from-msgs)))
      (cond
       ((eq (car condition) 'and)
        (elmo-imap4-search-generate-and session a b))
       ((eq (car condition) 'or)
        (elmo-imap4-search-generate-or session a b))))))

(defun elmo-imap4-search-internal (folder session condition from-msgs)
  (let ((imap-search
         (if (and from-msgs (listp from-msgs))
             (elmo-imap4-search-generate-and
              session
              (elmo-imap4-search-generate-uid from-msgs)
              (elmo-imap4-search-generate folder session condition from-msgs))
           (elmo-imap4-search-generate folder session condition nil))))
    (when imap-search
      (elmo-imap4-search-perform session imap-search))))

(luna-define-method elmo-folder-search :around ((folder elmo-imap4-folder)
                                                condition &optional numbers)
  (if (elmo-folder-plugged-p folder)
      (save-excursion
        (let ((session (elmo-imap4-get-session folder))
              ret)
          (message "Searching...")
          (elmo-imap4-session-select-mailbox
           session
           (elmo-imap4-folder-mailbox-internal folder))
          (setq ret (elmo-imap4-search-internal folder session condition numbers))
          (message "Searching...done")
          ret))
    (luna-call-next-method)))

(luna-define-method elmo-folder-msgdb-create-plugged
  ((folder elmo-imap4-folder) numbers flag-table)
  (when numbers
    (let ((session (elmo-imap4-get-session folder))
          (headers
           (elmo-uniq-list
            (append
             '("Subject" "From" "To" "Cc" "Date"
               "Message-Id" "References" "In-Reply-To")
             (mapcar #'capitalize (elmo-msgdb-extra-fields 'non-virtual)))))
          (total 0)
          print-length print-depth
          rfc2060 set-list)
      (setq rfc2060 (elmo-imap4-session-capable-p session 'imap4rev1))
      (elmo-with-progress-display (elmo-folder-msgdb-create (length numbers))
          "Creating msgdb"
        (elmo-imap4-session-select-mailbox
         session (elmo-imap4-folder-mailbox-internal folder))
        (setq set-list (elmo-imap4-make-number-set-list
                        numbers
                        elmo-imap4-overview-fetch-chop-length))
        ;; Setup callback.
        (with-current-buffer (elmo-network-session-buffer session)
          (setq elmo-imap4-current-msgdb (elmo-make-msgdb)
                elmo-imap4-seen-messages nil
                elmo-imap4-fetch-callback 'elmo-imap4-fetch-callback-1
                elmo-imap4-fetch-callback-data (cons flag-table folder))
          (while set-list
            (elmo-imap4-send-command-wait
             session
             ;; get overview entity from IMAP4
             (format "%sfetch %s (%s rfc822.size flags)"
                     (if elmo-imap4-use-uid "uid " "")
                     (cdr (car set-list))
                     (if rfc2060
                         (format "body.peek[header.fields %s]" headers)
                       (format "%s" headers))))
            (setq set-list (cdr set-list)))
          (when elmo-imap4-seen-messages
            (elmo-imap4-set-flag folder elmo-imap4-seen-messages "\\Seen"))
          ;; cannot setup the global flag while retrieval.
          (dolist (number (elmo-msgdb-list-messages elmo-imap4-current-msgdb))
            (elmo-global-flags-set (elmo-msgdb-flags elmo-imap4-current-msgdb
                                                     number)
                                   folder number
                                   (elmo-message-entity-field
                                    (elmo-msgdb-message-entity
                                     elmo-imap4-current-msgdb number)
                                    'message-id)))
          elmo-imap4-current-msgdb)))))

(luna-define-method elmo-folder-set-flag-plugged ((folder elmo-imap4-folder)
                                                  numbers flag)
  (let ((spec (cdr (assq flag elmo-imap4-flag-specs))))
    (elmo-imap4-set-flag folder numbers (or (car spec)
                                            (capitalize (symbol-name flag)))
                         (nth 1 spec))))

(luna-define-method elmo-folder-unset-flag-plugged ((folder elmo-imap4-folder)
                                                    numbers flag)
  (let ((spec (cdr (assq flag elmo-imap4-flag-specs))))
    (elmo-imap4-set-flag folder numbers (or (car spec)
                                            (capitalize (symbol-name flag)))
                         (not (nth 1 spec)))))

(luna-define-method elmo-message-use-cache-p ((folder elmo-imap4-folder)
                                              number)
  elmo-imap4-use-cache)

(luna-define-method elmo-folder-message-appendable-p ((folder elmo-imap4-folder))
  (if (elmo-folder-plugged-p folder)
      (not (elmo-imap4-session-read-only-internal
            (elmo-imap4-get-session folder)))
    elmo-enable-disconnected-operation)) ; offline refile.

(luna-define-method elmo-folder-check-plugged ((folder elmo-imap4-folder))
  (let ((session (elmo-imap4-get-session folder 'if-exists)))
    (when session
      (if (elmo-imap4-mailbox-selected-p
           (elmo-imap4-folder-mailbox-internal folder)
           session)
          (if elmo-imap4-use-select-to-update-status
              (elmo-imap4-session-select-mailbox
               session
               (elmo-imap4-folder-mailbox-internal folder)
               'force)
            (elmo-imap4-session-check session))))))

(defsubst elmo-imap4-folder-diff-plugged (folder)
  (let ((session (elmo-imap4-get-session folder))
        messages new unread response)
;;;    (elmo-imap4-commit spec)
    (with-current-buffer (elmo-network-session-buffer session)
      (setq elmo-imap4-status-callback nil)
      (setq elmo-imap4-status-callback-data nil))
    (if elmo-imap4-use-select-to-update-status
        (elmo-imap4-session-select-mailbox
         session
         (elmo-imap4-folder-mailbox-internal folder)))
    (if (elmo-imap4-mailbox-selected-p
         (elmo-imap4-folder-mailbox-internal folder)
         session)
        (progn
          (elmo-imap4-send-command-wait session "noop")
          (setq unread (length (elmo-imap4-folder-list-flagged folder 'unread)))
          (setq messages
                (car (elmo-imap4-session-current-mailbox-size-internal
                      session))
                new (cdr (elmo-imap4-session-current-mailbox-size-internal
                          session))))
      (setq response
            (elmo-imap4-send-command-wait session
                                          (list
                                           "status"
                                           (elmo-imap4-mailbox
                                            (elmo-imap4-folder-mailbox-internal
                                             folder))
                                           "(recent unseen messages)")))
      (setq response (elmo-imap4-response-value response 'status))
      (setq messages (elmo-imap4-response-value response 'messages))
      (setq new (elmo-imap4-response-value response 'recent)
            unread (elmo-imap4-response-value response 'unseen)))
    (if (< unread new) (setq new unread))
    (list new unread messages)))

(luna-define-method elmo-folder-diff-plugged ((folder elmo-imap4-folder))
  (elmo-imap4-folder-diff-plugged folder))

(luna-define-method elmo-folder-diff-async ((folder elmo-imap4-folder))
  (setq elmo-imap4-server-diff-async-callback
        elmo-folder-diff-async-callback)
  (setq elmo-imap4-server-diff-async-callback-data
        elmo-folder-diff-async-callback-data)
  (elmo-imap4-server-diff-async folder))

(luna-define-method elmo-folder-open :around ((folder elmo-imap4-folder)
                                              &optional load-msgdb)
  (if (elmo-folder-plugged-p folder)
      (let (session mailbox msgdb result response tag)
        (condition-case err
            (progn
              (setq session (elmo-imap4-get-session folder)
                    mailbox (elmo-imap4-folder-mailbox-internal folder)
                    tag (elmo-imap4-send-command session
                                                 (list "select"
                                                       (elmo-imap4-mailbox
                                                        mailbox))))
              (message "Selecting %s..."
                       (elmo-folder-name-internal folder))
              (if load-msgdb
                  (setq msgdb (elmo-folder-msgdb-load folder 'silent)))
              (elmo-folder-set-killed-list-internal
               folder
               (elmo-msgdb-killed-list-load (elmo-folder-msgdb-path folder)))
              (if (setq result (elmo-imap4-response-ok-p
                                (setq response
                                      (elmo-imap4-read-response session tag))))
                  (progn
                    (let ((exists (assq 'exists response))) ; update message count,
                      (when exists                          ; so merge update can go
                        (elmo-folder-set-info-hashtb folder nil (cadr exists))))
                    (elmo-imap4-session-set-current-mailbox-internal
                     session mailbox)
                    (elmo-imap4-session-set-read-only-internal
                     session
                     (nth 1 (assq 'read-only (assq 'ok response))))
                    (elmo-imap4-session-set-flags-internal
                     session
                     (nth 1 (or (assq 'permanentflags response)
                                (assq 'flags response)))))
                (elmo-imap4-session-set-current-mailbox-internal session nil)
                (elmo-imap4-session-set-current-mailbox-size-internal
                 session nil)
                (if (elmo-imap4-response-bye-p response)
                    (elmo-imap4-process-bye session)
                  (error "%s"
                         (or (elmo-imap4-response-error-text response)
                             (format "Select %s failed" mailbox)))))
              (message "Selecting %s...done"
                       (elmo-folder-name-internal folder))
              (elmo-folder-set-msgdb-internal
               folder msgdb))
          (quit
           (if (elmo-imap4-response-ok-p response)
               (elmo-imap4-session-set-current-mailbox-internal
                session mailbox)
             (and session
                  (elmo-imap4-session-set-current-mailbox-internal
                   session nil))
            ))
          (error
           (if (elmo-imap4-response-ok-p response)
               (elmo-imap4-session-set-current-mailbox-internal
                session mailbox)
             (and session
                  (elmo-imap4-session-set-current-mailbox-internal
                   session nil)
                  (elmo-imap4-session-set-current-mailbox-size-internal
                   session nil))))))
    (luna-call-next-method)))

;; elmo-folder-open-internal: do nothing.

(luna-define-method elmo-find-fetch-strategy ((folder elmo-imap4-folder) number
                                              &optional
                                              ignore-cache
                                              require-entireness)
  (let ((entity (elmo-message-entity folder number)))
    (if (null entity)
        (elmo-make-fetch-strategy 'entire)
      (let* ((size (elmo-message-entity-field entity 'size))
             (message-id (elmo-message-entity-field entity 'message-id))
             (cache-file (elmo-file-cache-get message-id))
             (use-cache (and (not ignore-cache)
                             (elmo-message-use-cache-p folder number)
                             (if require-entireness
                                 (eq (elmo-file-cache-status cache-file)
                                     'entire)
                               (elmo-file-cache-status cache-file)))))
        (elmo-make-fetch-strategy
         (if use-cache
             (elmo-file-cache-status cache-file)
           (if (and (not require-entireness)
                    elmo-message-fetch-threshold
                    (integerp size)
                    (>= size elmo-message-fetch-threshold)
                    (or (not elmo-message-fetch-confirm)
                        (not (prog1
                                 (y-or-n-p
                                  (format
                                   "Fetch entire message at once? (%dbytes)"
                                   size))
                               (message "")))))
               'section
             'entire))
         use-cache
         (elmo-message-use-cache-p folder number)
         (elmo-file-cache-path cache-file))))))

(luna-define-method elmo-folder-create-plugged ((folder elmo-imap4-folder))
  (elmo-imap4-send-command-wait
   (elmo-imap4-get-session folder)
   (list "create"
         (elmo-imap4-mailbox
          (elmo-imap4-folder-mailbox-internal folder)))))

(defun elmo-imap4-flags-to-imap (flags)
  "Convert FLAGS to the IMAP flag string."
  (let ((imap-flag (if (not (memq 'unread flags)) "\\Seen")))
    (dolist (flag flags)
      (unless (memq flag '(new read unread cached))
        (setq imap-flag
              (concat imap-flag
                      (if imap-flag " ")
                      (or (car (cdr (assq flag elmo-imap4-flag-specs)))
                          (capitalize (symbol-name flag)))))))
    imap-flag))

(luna-define-method elmo-folder-append-buffer
  ((folder elmo-imap4-folder) &optional flags number return-number)
  (if (elmo-folder-plugged-p folder)
      (let ((session (elmo-imap4-get-session folder))
            (internaldate (elmo-time-make-imap-date-string (current-time)))
            send-buffer result)
        (elmo-imap4-session-select-mailbox session
                                           (elmo-imap4-folder-mailbox-internal
                                            folder))
        (setq send-buffer (elmo-imap4-setup-send-buffer))
        (unwind-protect
            (setq result
                  (elmo-imap4-send-command-wait
                   session
                   (list
                    "append"
                    (elmo-imap4-mailbox (elmo-imap4-folder-mailbox-internal
                                         folder))
                    (if (and flags (elmo-folder-use-flag-p folder))
                        (concat "(" (elmo-imap4-flags-to-imap flags) ")")
                      "()")
                    (if return-number
                        (concat "\"" internaldate "\"")
                      "")
                    (elmo-imap4-buffer-literal send-buffer))))
          (kill-buffer send-buffer))
        (when result
          (elmo-folder-preserve-flags
           folder (elmo-msgdb-get-message-id-from-buffer) flags)
          (when return-number
            (unless (setq result (cadadr (assq 'appenduid (cdar result))))
              (let ((candidates
                     (elmo-imap4-response-value
                      (elmo-imap4-send-command-wait
                       session
                       (list
                        "uid search since"
                        (car (split-string internaldate " ")))) 'search)))
                (if (null candidates)
                    (setq result t)
                  (setq candidates
                        (elmo-imap4-response-value-all
                         (elmo-imap4-send-command-wait
                          session
                          (list
                           "uid fetch"
                           (mapconcat 'number-to-string candidates ",")
                           "(internaldate)")) 'fetch))
                  (while candidates
                    (if (string= (cadar candidates) internaldate)
                        (setq result (cons
                                      (cadadr candidates)
                                      result)))
                    (setq candidates (cddr candidates)))
                  (setq result (or (null result)
                                   (> (length result) 1)
                                   (car result))))))))
        result)
    ;; Unplugged
    (if elmo-enable-disconnected-operation
        (elmo-folder-append-buffer-dop folder flags number)
      (error "Unplugged"))))

(eval-when-compile
  (defmacro elmo-imap4-identical-system-p (folder1 folder2)
    "Return t if FOLDER1 and FOLDER2 are in the same IMAP4 system."
    `(and (string= (elmo-net-folder-server-internal ,folder1)
                   (elmo-net-folder-server-internal ,folder2))
          (eq (elmo-net-folder-port-internal ,folder1)
              (elmo-net-folder-port-internal ,folder2))
          (string= (elmo-net-folder-user-internal ,folder1)
                   (elmo-net-folder-user-internal ,folder2)))))

(luna-define-method elmo-folder-next-message-number-plugged
  ((folder elmo-imap4-folder))
  (let* ((session (elmo-imap4-get-session folder))
         (selected (elmo-imap4-mailbox-selected-p
                    (elmo-imap4-folder-mailbox-internal folder) session))
         response uidnext)
    (with-current-buffer (elmo-network-session-buffer session)
      (setq elmo-imap4-status-callback nil)
      (setq elmo-imap4-status-callback-data nil))
    (cond
     ((and selected (not elmo-imap4-use-select-to-update-status))
      (elmo-imap4-send-command-wait session "close")
      (elmo-imap4-session-set-current-mailbox-internal session nil)
      (elmo-imap4-session-set-current-mailbox-size-internal session nil))
     ((and (not selected) elmo-imap4-use-select-to-update-status)
      ;; This will result in a violation of RFC3501: calling STATUS on
      ;; a selected mailbox.
      (elmo-imap4-session-select-mailbox
       session
       (elmo-imap4-folder-mailbox-internal folder))))
    (setq response
          (elmo-imap4-send-command-wait session
                                        (list
                                         "status"
                                         (elmo-imap4-mailbox
                                          (elmo-imap4-folder-mailbox-internal
                                           folder))
                                         "(uidnext)"))
          response (elmo-imap4-response-value response 'status))
    (elmo-imap4-response-value response 'uidnext)))

(defun elmo-folder-append-messages-imap4-imap4 (dst-folder
                                                src-folder
                                                numbers
                                                same-number)
  (if (and (elmo-imap4-identical-system-p dst-folder src-folder)
           (elmo-folder-plugged-p dst-folder))
      ;; Plugged
      (prog1
          (elmo-imap4-copy-messages src-folder dst-folder numbers)
        (elmo-progress-notify 'elmo-folder-move-messages (length numbers)))
    (elmo-folder-append-messages dst-folder src-folder numbers same-number
                                 'elmo-folder-append-messages-imap4-imap4)))

(luna-define-method elmo-message-deletable-p ((folder elmo-imap4-folder)
                                              number)
  (if (elmo-folder-plugged-p folder)
      (not (elmo-imap4-session-read-only-internal
            (elmo-imap4-get-session folder)))
    elmo-enable-disconnected-operation)) ; offline refile.

;;;(luna-define-method elmo-message-fetch-unplugged
;;;  ((folder elmo-imap4-folder)
;;;   number strategy  &optional section outbuf unseen)
;;;  (error "%d%s is not cached." number (if section
;;;                                       (format "(%s)" section)
;;;					"")))

(defsubst elmo-imap4-message-fetch (folder number strategy
                                           section outbuf unseen)
  (let ((session (elmo-imap4-get-session folder))
        response)
    (elmo-imap4-session-select-mailbox session
                                       (elmo-imap4-folder-mailbox-internal
                                        folder))
    (with-current-buffer (elmo-network-session-buffer session)
      (setq elmo-imap4-fetch-callback nil)
      (setq elmo-imap4-fetch-callback-data nil))
    (elmo-with-progress-display (elmo-retrieve-message
                                 (elmo-message-field folder number :size)
                                 elmo-imap4-literal-progress-reporter)
        "Retrieving"
      (setq response
            (elmo-imap4-send-command-wait session
                                          (format
                                           (if elmo-imap4-use-uid
                                               "uid fetch %s body%s[%s]"
                                             "fetch %s body%s[%s]")
                                           number
                                           (if unseen ".peek" "")
                                           (or section "")))))
    (if (setq response (elmo-imap4-response-bodydetail-text
                        (elmo-imap4-response-value-all
                         response 'fetch)))
        (with-current-buffer outbuf
          (erase-buffer)
          (insert response)
          (elmo-delete-cr-buffer)
          t))))

(luna-define-method elmo-message-fetch-plugged ((folder elmo-imap4-folder)
                                                number strategy
                                                &optional section
                                                outbuf unseen)
  (when elmo-imap4-set-seen-flag-explicitly
    (elmo-imap4-set-flag folder (list number) "\\Seen"))
  (elmo-imap4-message-fetch folder number strategy section outbuf unseen))

(luna-define-method elmo-message-fetch-field ((folder elmo-imap4-folder)
                                              number field)
  (let ((session (elmo-imap4-get-session folder)))
    (elmo-imap4-session-select-mailbox session
                                       (elmo-imap4-folder-mailbox-internal
                                        folder))
    (with-current-buffer (elmo-network-session-buffer session)
      (setq elmo-imap4-fetch-callback nil)
      (setq elmo-imap4-fetch-callback-data nil))
    (with-temp-buffer
      (insert
       (elmo-imap4-response-bodydetail-text
        (elmo-imap4-response-value
         (elmo-imap4-send-command-wait session
                                       (concat
                                        (if elmo-imap4-use-uid
                                            "uid")
                                        (format
                                         "fetch %s (body.peek[header.fields (%s)])"
                                         number field)))
         'fetch)))
      (elmo-delete-cr-buffer)
      (goto-char (point-min))
      (std11-field-body (symbol-name field)))))

(luna-define-method elmo-folder-search-requires-msgdb-p ((folder
                                                          elmo-imap4-folder)
                                                         condition)
  nil)

(luna-define-method elmo-folder-close-internal ((folder elmo-imap4-folder))
  (when (elmo-folder-plugged-p folder)
    (let ((session (elmo-imap4-get-session folder))
          (mailbox (elmo-imap4-folder-mailbox-internal folder)))
      (when (elmo-imap4-mailbox-selected-p mailbox session)
        (elmo-imap4-session-unselect-mailbox session mailbox)))))

(autoload 'elmo-global-flags-set "elmo-flag")
(autoload 'elmo-get-global-flags "elmo-flag")
(autoload 'utf7-decode "utf7")
(autoload 'utf7-encode "utf7")

(require 'product)
(product-provide (provide 'elmo-imap4) (require 'elmo-version))

;;; elmo-imap4.el ends here
