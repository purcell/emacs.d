;;; mu4e-view.el -- part of mu4e, the mu mail user agent
;;
;; Copyright (C) 2011-2015 Dirk-Jan C. Binnema

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

;; In this file we define mu4e-view-mode (+ helper functions), which is used for
;; viewing e-mail messages

;;; Code:
(require 'mu4e-utils) ;; utility functions
(require 'mu4e-vars)
(require 'mu4e-mark)
(require 'mu4e-proc)
(require 'mu4e-compose)
(require 'mu4e-actions)
(require 'mu4e-message)

(require 'comint)
(require 'browse-url)
(require 'button)
(require 'epa)
(require 'epg)
(require 'thingatpt)

(eval-when-compile (byte-compile-disable-warning 'cl-functions))
(require 'cl)


;; the message view
(defgroup mu4e-view nil
  "Settings for the message view."
  :group 'mu4e)

(defcustom mu4e-view-fields
  '(:from :to  :cc :subject :flags :date :maildir :mailing-list :tags
          :attachments :signature :decryption)
  "Header fields to display in the message view buffer.
For the complete list of available headers, see `mu4e-header-info'."
  :type (list 'symbol)
  :group 'mu4e-view)


(defcustom mu4e-view-show-addresses nil
  "Whether to initially show full e-mail addresses for contacts in
address fields, rather than only their names."
  :type 'boolean
  :group 'mu4e-view)

(make-obsolete-variable 'mu4e-view-wrap-lines nil "0.9.9-dev7")
(make-obsolete-variable 'mu4e-view-hide-cited nil "0.9.9-dev7")

(defcustom mu4e-view-date-format "%c"
  "Date format to use in the message view.
In the format of `format-time-string'."
  :type 'string
  :group 'mu4e-view)

(defcustom mu4e-view-image-max-width 800
  "The maximum width for images to display.
This is only effective if you're using an emacs with Imagemagick
support, and `mu4e-view-show-images' is non-nil."
  :group 'mu4e-view)

(defcustom mu4e-view-image-max-height 600
  "The maximum height for images to display.
This is only effective if you're using an emacs with Imagemagick
support, and `mu4e-view-show-images' is non-nil."
  :group 'mu4e-view)

(defcustom mu4e-view-scroll-to-next t
  "If non-nil, move to the next message when calling
`mu4e-view-scroll-up-or-next' (typically bound to SPC) when at the
end of a message. Otherwise, don't move to the next message.")

(defcustom mu4e-save-multiple-attachments-without-asking nil
  "If non-nil, saving multiple attachments asks once for a
directory and saves all attachments in the chosen directory."
  :type 'boolean
  :group 'mu4e-view)

(defvar mu4e-view-actions
  '( ("capture message" . mu4e-action-capture-message)
     ("view as pdf"     . mu4e-action-view-as-pdf))
  "List of actions to perform on messages in view mode.
The actions are of the form:
  (NAME FUNC)
where:
* NAME is the name of the action (e.g. \"Count lines\")
* FUNC is a function which receives a message plist as an argument.

The first letter of NAME is used as a shortcut character.")

(defvar mu4e-view-attachment-actions
  '( ("wopen-with" . mu4e-view-open-attachment-with)
     ("ein-emacs"  . mu4e-view-open-attachment-emacs)
     ("dimport-in-diary"  . mu4e-view-import-attachment-diary)
     ("|pipe"      . mu4e-view-pipe-attachment))
  "List of actions to perform on message attachments.
The actions are cons-cells of the form:
 (NAME . FUNC)
where:
* NAME is the name of the action (e.g. \"Count lines\")
* FUNC is a function which receives two arguments: the message
  plist and the attachment number.
The first letter of NAME is used as a shortcut character.")

(defvar mu4e-view-fill-headers t
  "If non-nil, automatically fill the headers when viewing them.")

(defvar mu4e-view-contacts-header-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'mu4e~view-compose-contact)
    (define-key map "C"  'mu4e~view-compose-contact)
    (define-key map "c"  'mu4e~view-copy-contact)
    map)
  "Keymap used for the contacts in the header fields.")

(defvar mu4e-view-clickable-urls-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'mu4e~view-browse-url-from-binding)
    (define-key map [?\M-\r] 'mu4e~view-browse-url-from-binding)
    map)
  "Keymap used for the urls inside the body.")

(defvar mu4e-view-attachments-header-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'mu4e~view-open-attach-from-binding)
    (define-key map  [?\M-\r] 'mu4e~view-open-attach-from-binding)
    (define-key map [mouse-2] 'mu4e~view-save-attach-from-binding)
    (define-key map (kbd "<S-return>") 'mu4e~view-save-attach-from-binding)
    map)
  "Keymap used in the \"Attachements\" header field.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar mu4e~view-cited-hidden nil "Whether cited lines are hidden.")
(defvar mu4e~view-link-map nil
  "A map of some number->url so we can jump to url by number.")

(defvar mu4e~path-parent-docid-map (make-hash-table :test 'equal)
  "A map of msg paths --> parent-docids.
This is to determine what is the parent docid for embedded
message extracted at some path.")

(defvar mu4e~view-attach-map nil
  "A mapping of user-visible attachment number to the actual part index.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mu4e-view-message-with-msgid (msgid)
  "View message with MSGID.
This is meant for external programs wanting to show specific
messages - for example, `mu4e-org'."
  ;; note: hackish; if mu4e-decryption-policy is non-nil (ie., t or
  ;; 'ask), we decrypt the message. Since here we don't know if
  ;; message is encrypted or not, when the policy is 'ask'. we simply
  ;; assume the user said yes...  the alternative would be to ask for
  ;; each message, encrypted or not.  maybe we need an extra policy...
  (let ((view-buffer (get-buffer mu4e~view-buffer-name)))
		(when view-buffer
			(kill-buffer view-buffer)))
  (mu4e~proc-view msgid mu4e-view-show-images mu4e-decryption-policy))

(defun mu4e~view-custom-field (msg field)
  "Show some custom header field, or raise an error if it is not
found."
  (let* ((item (or (assoc field mu4e-header-info-custom)
		 (mu4e-error "field %S not found" field)))
	  (func (or (plist-get (cdr-safe item) :function)
		  (mu4e-error "no :function defined for field %S %S"
		    field (cdr item)))))
    (funcall func msg)))


(defun mu4e-view-message-text (msg)
  "Return the message to display (as a string), based on the MSG plist."
  (concat
    (mapconcat
      (lambda (field)
	(let ((fieldval (mu4e-message-field msg field)))
	  (case field
	    (:subject  (mu4e~view-construct-header field fieldval))
	    (:path     (mu4e~view-construct-header field fieldval))
	    (:maildir  (mu4e~view-construct-header field fieldval))
	    ((:flags :tags) (mu4e~view-construct-flags-tags-header field fieldval))

	    ;; contact fields
	    (:to       (mu4e~view-construct-contacts-header msg field))
	    (:from     (mu4e~view-construct-contacts-header msg field))
	    (:cc       (mu4e~view-construct-contacts-header msg field))
	    (:bcc      (mu4e~view-construct-contacts-header msg field))

	    ;; if we (`user-mail-address' are the From, show To, otherwise,
	    ;; show From
	    (:from-or-to
	      (let* ((from (mu4e-message-field msg :from))
		      (from (and from (cdar from))))
		(if (mu4e-user-mail-address-p from)
		  (mu4e~view-construct-contacts-header msg :to)
		  (mu4e~view-construct-contacts-header msg :from))))
	    ;; date
	    (:date
	      (let ((datestr
		      (when fieldval (format-time-string mu4e-view-date-format
				       fieldval))))
		(if datestr (mu4e~view-construct-header field datestr) "")))
	    ;; size
	    (:size
	      (mu4e~view-construct-header field (mu4e-display-size fieldval)))
	    (:mailing-list
	      (mu4e~view-construct-header field fieldval))
	    (:message-id
	      (mu4e~view-construct-header field fieldval))
	    ;; attachments
	    (:attachments (mu4e~view-construct-attachments-header msg))
	    ;; pgp-signatures
	    (:signature   (mu4e~view-construct-signature-header msg))
	    ;; pgp-decryption
	    (:decryption  (mu4e~view-construct-decryption-header msg))
	    (t (mu4e~view-construct-header field
		 (mu4e~view-custom-field msg field))))))
      mu4e-view-fields "")
    "\n"
    (mu4e-message-body-text msg)))

(defun mu4e~view-embedded-winbuf ()
  "Get a buffer (shown in a window) for the embedded message."
  (let* ((buf (get-buffer-create mu4e~view-embedded-buffer-name))
	  (win (or (get-buffer-window buf) (split-window-vertically))))
    (select-window win)
    (switch-to-buffer buf)))

(defun mu4e~delete-all-overlays ()
  "`delete-all-overlays' with compatibility fallback."
  (if (functionp 'delete-all-overlays)
    (delete-all-overlays)
    (remove-overlays)))


(defun mu4e-view (msg headersbuf)
  "Display the message MSG in a new buffer, and keep in sync with HDRSBUF.
'In sync' here means that moving to the next/previous message in
the the message view affects HDRSBUF, as does marking etc.

As a side-effect, a message that is being viewed loses its 'unread'
marking if it still had that."
  (let* ((embedded ;; is it as an embedded msg (ie. message/rfc822 att)?
	   (when (gethash (mu4e-message-field msg :path)
		   mu4e~path-parent-docid-map) t))
	  (buf
	    (if embedded
	      (mu4e~view-embedded-winbuf)
	      (get-buffer-create mu4e~view-buffer-name))))
    ;; note: mu4e~view-mark-as-read will pseudo-recursively call mu4e-view again
    ;; by triggering mu4e~view again as it marks the message as read
    (with-current-buffer buf
      (switch-to-buffer buf)
      (setq mu4e~view-msg msg)
      (when (or embedded (not (mu4e~view-mark-as-read msg)))
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (mu4e~delete-all-overlays)
	  (insert (mu4e-view-message-text msg))
	  (goto-char (point-min))
	  (mu4e~fontify-cited)
	  (mu4e~fontify-signature)
	  (mu4e~view-make-urls-clickable)
	  (mu4e~view-show-images-maybe msg)
	  (setq
	    mu4e~view-buffer buf
	    mu4e~view-headers-buffer headersbuf)
	  (when embedded (local-set-key "q" 'kill-buffer-and-window))
	  (mu4e-view-mode))))))

(defun mu4e~view-get-property-from-event (prop)
  "Get the property PROP at point, or the location of the mouse.
The action is chosen based on the `last-command-event'.
Meant to be evoked from interactive commands."
  (if (and (eventp last-command-event)
	   (mouse-event-p last-command-event))
      (let ((posn (event-end last-command-event)))
        (when (numberp (posn-point posn))
          (get-text-property
           (posn-point posn)
           prop
           (window-buffer (posn-window posn)))
          ))
    (get-text-property (point) prop)))

(defun mu4e~view-construct-header (field val &optional dont-propertize-val)
  "Return header field FIELD (as in `mu4e-header-info') with value
VAL if VAL is non-nil. If DONT-PROPERTIZE-VAL is non-nil, do not
add text-properties to VAL."
  (let* ((info (cdr (assoc field
		      (append mu4e-header-info mu4e-header-info-custom))))
	  (key (plist-get info :name))
	  (help (plist-get info :help)))
    (if (and val (> (length val) 0))
    (with-temp-buffer
      (insert (propertize (concat key ":")
		'face 'mu4e-header-key-face
		'help-echo help) " "
	(if dont-propertize-val
	  val
	  (propertize val 'face 'mu4e-header-value-face)) "\n")
      (when mu4e-view-fill-headers
	;; temporarily set the fill column <margin> positions to the right, so
	;; we can indent the following lines correctly
	(let* ((margin 1)
		(fill-column (max (- fill-column margin) 0)))
	  (fill-region (point-min) (point-max))
	  (goto-char (point-min))
	  (while (and (zerop (forward-line 1)) (not (looking-at "^$")))
	    (indent-to-column margin))))
      (buffer-string))
    "")))

(defun mu4e~view-compose-contact (&optional point)
  "Compose a message for the address at point."
  (interactive)
  (unless (get-text-property (or point (point)) 'email)
    (mu4e-error "No address at point"))
  (mu4e~compose-mail (get-text-property (or point (point)) 'long)))

(defun mu4e~view-copy-contact (&optional full)
  "Compose a message for the address at (point)."
  (interactive "P")
  (let ((email (get-text-property (point) 'email))
	 (long (get-text-property (point) 'long)))
    (unless email (mu4e-error "No address at point"))
    (kill-new (if full long email))
    (mu4e-message "Address copied.")))

(defun mu4e~view-construct-contacts-header (msg field)
  "Add a header for a contact field (ie., :to, :from, :cc, :bcc)."
  (mu4e~view-construct-header field
    (mapconcat
      (lambda(c)
	(let* ((name (when (car c)
		       (replace-regexp-in-string "[[:cntrl:]]" "" (car c))))
		(email (when (cdr c)
			 (replace-regexp-in-string "[[:cntrl:]]" "" (cdr c))))
		(short (or name email)) ;; name may be nil
		(long (if name (format "%s <%s>" name email) email)))
	  (propertize
	    (if mu4e-view-show-addresses long short)
	    'long long
	    'short short
	    'email email
	    'keymap mu4e-view-contacts-header-keymap
	    'face 'mu4e-contact-face
	    'mouse-face 'highlight
	    'help-echo (format "<%s>\n%s" email
			 "[mouse-2] or C to compose a mail for this recipient"))))
	  (mu4e-message-field msg field) ", ") t))


(defun mu4e~view-construct-flags-tags-header (field val)
  "Construct a Flags: header."
  (mu4e~view-construct-header
    field
    (mapconcat
      (lambda (flag)
	(propertize
	  (if (symbolp flag)
	    (symbol-name flag)
	    flag)
	  'face 'mu4e-special-header-value-face))
      val
      (propertize ", " 'face 'mu4e-header-value-face)) t))

(defun mu4e~view-construct-signature-header (msg)
  "Construct a Signature: header, if there are any signed parts."
  (let* ((parts (mu4e-message-field msg :parts))
	  (verdicts
	    (remove-if 'null
	      (mapcar (lambda (part) (mu4e-message-part-field part :signature))
		parts)))
	  (val (when verdicts
		 (mapconcat
		   (lambda (v)
		     (propertize (symbol-name v)
		       'face (if (eq v 'verified)
			       'mu4e-ok-face 'mu4e-warning-face)))
		   verdicts ", ")))
	  (btn (when val
		 (with-temp-buffer
		   (insert-text-button "Details"
		     'action (lambda (b)
			       (mu4e-view-verify-msg-popup
				 (button-get b 'msg))))
		   (buffer-string))))
	  (val (when val (concat val " (" btn ")"))))
    (mu4e~view-construct-header :signature val t)))

(defun mu4e~view-construct-decryption-header (msg)
  "Construct a Decryption: header, if there are any encrypted parts."
  (let* ((parts (mu4e-message-field msg :parts))
	 (verdicts
	  (remove-if 'null
		     (mapcar (lambda (part) (mu4e-message-part-field part :decryption))
			     parts)))
	 (succeeded (remove-if (lambda (v) (eq v 'failed)) verdicts))
	 (failed (remove-if (lambda (v) (eq v 'succeeded)) verdicts))
	 (succ (when succeeded
		 (propertize
		  (concat (number-to-string (length succeeded))
			  " part(s) decrypted")
		  'face 'mu4e-ok-face)))
	 (fail (when failed
		 (propertize
		  (concat (number-to-string (length failed))
			  " part(s) failed")
		  'face 'mu4e-warning-face)))
	 (val (concat succ fail)))
    (mu4e~view-construct-header :decryption val t)))

(defun mu4e~view-open-attach-from-binding ()
  "Open the attachement at point, or click location."
  (interactive)
  (let* (( msg (mu4e~view-get-property-from-event 'mu4e-msg))
         ( attnum (mu4e~view-get-property-from-event 'mu4e-attnum)))
    (when (and msg attnum)
      (mu4e-view-open-attachment msg attnum))))

(defun mu4e~view-save-attach-from-binding ()
  "Save the attachement at point, or click location."
  (interactive)
  (let* (( msg (mu4e~view-get-property-from-event 'mu4e-msg))
         ( attnum (mu4e~view-get-property-from-event 'mu4e-attnum)))
    (when (and msg attnum)
      (mu4e-view-save-attachment-single msg attnum))))

(defun mu4e~view-construct-attachments-header (msg)
  "Display attachment information; the field looks like something like:
   	:parts ((:index 1 :name \"1.part\" :mime-type \"text/plain\"
                 :type (leaf) :attachment nil :size 228)
                (:index 2 :name \"analysis.doc\"
                 :mime-type \"application/msword\"
                 :type (leaf attachment) :attachment nil :size 605196))"
  (setq mu4e~view-attach-map ;; buffer local
    (make-hash-table :size 64 :weakness nil))
  (let* ((id 0)
	  (attachments
	    ;; we only list parts that look like attachments, ie. that have a
	    ;; non-nil :attachment property; we record a mapping between
	    ;; user-visible numbers and the part indices
	    (remove-if-not
	      (lambda (part)
		(let* ((mtype (or (mu4e-message-part-field part :mime-type)
				"application/octet-stream"))
			(attachtype (mu4e-message-part-field part :type))
			(isattach
			  (or ;; we consider parts marked either
			    ;; "attachment" or "inline" as attachment.
			    (member 'attachment attachtype)
			    ;; list inline parts as attachment (so they can be
			    ;; saved), unless they are text/plain, which are
			    ;; usually just message footers in mailing lists
			    (and (member 'inline attachtype)
			      (not (string-match "^text/plain" mtype))))))
		  (or ;; remove if it's not an attach *or* if it's an
		    ;; image/audio/application type (but not a signature)
		    isattach
		    (string-match "^\\(image\\|audio\\)" mtype)
		    (string= "message/rfc822" mtype)
		    (string= "text/calendar" mtype)
		    (and (string-match "^application" mtype)
		      (not (string-match "signature" mtype))))))
	      (mu4e-message-field msg :parts)))
	  (attstr
	    (mapconcat
	      (lambda (part)
		(let ((index (mu4e-message-part-field part :index))
		       (name (mu4e-message-part-field part :name))
		       (size (mu4e-message-part-field part :size)))
		  (incf id)
		  (puthash id index mu4e~view-attach-map)

		  (concat
		    (propertize (format "[%d]" id)
		      'face 'mu4e-attach-number-face)
		    (propertize name 'face 'mu4e-link-face
		      'keymap mu4e-view-attachments-header-keymap
		      'mouse-face 'highlight
		      'help-echo (concat
				  "[mouse-1] or [M-RET] opens the attachment\n"
				  "[mouse-2] or [S-RET] offers to save it")
		      'mu4e-msg msg
		      'mu4e-attnum id
		      )
		    (when (and size (> size 0))
		      (propertize (format "(%s)" (mu4e-display-size size))
                                  'face 'mu4e-header-key-face)))))
	      attachments ", ")))
    (when attachments
      (mu4e~view-construct-header :attachments attstr t))))

(defun mu4e-view-for-each-part (msg func)
  "Apply FUNC to each part in MSG.
FUNC should be a function taking two arguments:
 1. the message MSG, and
 2. a plist describing the attachment. The plist looks like:
    	 (:index 1 :name \"test123.doc\"
          :mime-type \"application/msword\" :attachment t :size 1234)."
  (dolist (part (mu4e-msg-field msg :parts))
    (funcall func msg part)))


(defvar mu4e-view-mode-map nil
  "Keymap for \"*mu4e-view*\" buffers.")
(unless mu4e-view-mode-map
  (setq mu4e-view-mode-map
    (let ((map (make-sparse-keymap)))

      (define-key map  (kbd "C-S-u") 'mu4e-update-mail-and-index)
      (define-key map  (kbd "C-c C-u") 'mu4e-update-mail-and-index)

      (define-key map "q" 'mu4e~view-quit-buffer)

      ;; note, 'z' is by-default bound to 'bury-buffer'
      ;; but that's not very useful in this case
      (define-key map "z" 'mu4e~view-quit-buffer)

      (define-key map "s" 'mu4e-headers-search)
      (define-key map "S" 'mu4e-view-search-edit)
      (define-key map "/" 'mu4e-view-search-narrow)

      (define-key map (kbd "<M-left>")  'mu4e-headers-query-prev)
      (define-key map (kbd "<M-right>") 'mu4e-headers-query-next)

      (define-key map "b" 'mu4e-headers-search-bookmark)
      (define-key map "B" 'mu4e-headers-search-bookmark-edit)

      (define-key map "%" 'mu4e-view-mark-pattern)
      (define-key map "t" 'mu4e-view-mark-subthread)
      (define-key map "T" 'mu4e-view-mark-thread)

      (define-key map "v" 'mu4e-view-verify-msg-popup)

      (define-key map "j" 'mu4e~headers-jump-to-maildir)

      (define-key map "g" 'mu4e-view-go-to-url)
      (define-key map "k" 'mu4e-view-save-url)

      (define-key map "F" 'mu4e-compose-forward)
      (define-key map "R" 'mu4e-compose-reply)
      (define-key map "C" 'mu4e-compose-new)
      (define-key map "E" 'mu4e-compose-edit)

      (define-key map "." 'mu4e-view-raw-message)
      (define-key map "|" 'mu4e-view-pipe)
      (define-key map "a" 'mu4e-view-action)

      ;; toggle header settings
      (define-key map "O" 'mu4e-headers-change-sorting)
      (define-key map "P" 'mu4e-headers-toggle-threading)
      (define-key map "Q" 'mu4e-headers-toggle-full-search)
      (define-key map "W" 'mu4e-headers-toggle-include-related)

      ;; change the number of headers
      (define-key map (kbd "C-+") 'mu4e-headers-split-view-grow)
      (define-key map (kbd "C--") 'mu4e-headers-split-view-shrink)
      (define-key map (kbd "<C-kp-add>") 'mu4e-headers-split-view-grow)
      (define-key map (kbd "<C-kp-subtract>") 'mu4e-headers-split-view-shrink)

      ;; intra-message navigation
      (define-key map (kbd "SPC") 'mu4e-view-scroll-up-or-next)
      (define-key map (kbd "<home>") 'beginning-of-buffer)
      (define-key map (kbd "<end>") 'end-of-buffer)
      (define-key map (kbd "RET") 'mu4e-scroll-up)
      (define-key map (kbd "<backspace>") 'mu4e-scroll-down)

      ;; navigation between messages
      (define-key map "p" 'mu4e-view-headers-prev)
      (define-key map "n" 'mu4e-view-headers-next)
      ;; the same
      (define-key map (kbd "<M-down>") 'mu4e-view-headers-next)
      (define-key map (kbd "<M-up>") 'mu4e-view-headers-prev)

      (define-key map (kbd "]") 'mu4e-view-headers-next-unread)
      (define-key map (kbd "[")
	(lambda() (interactive) (mu4e-view-headers-next-unread t)))
      
      ;; switching to view mode (if it's visible)
      (define-key map "y" 'mu4e-select-other-view)

      ;; attachments
      (define-key map "e" 'mu4e-view-save-attachment)
      (define-key map "o" 'mu4e-view-open-attachment)
      (define-key map "A" 'mu4e-view-attachment-action)

      ;; marking/unmarking
      (define-key map "d" 'mu4e-view-mark-for-trash)
      (define-key map (kbd "<delete>") 'mu4e-view-mark-for-delete)
      (define-key map (kbd "<deletechar>") 'mu4e-view-mark-for-delete)
      (define-key map (kbd "D") 'mu4e-view-mark-for-delete)
      (define-key map (kbd "m") 'mu4e-view-mark-for-move)
      (define-key map (kbd "r") 'mu4e-view-mark-for-refile)

      (define-key map (kbd "?") 'mu4e-view-mark-for-unread)
      (define-key map (kbd "!") 'mu4e-view-mark-for-read)

      (define-key map (kbd "+") 'mu4e-view-mark-for-flag)
      (define-key map (kbd "-") 'mu4e-view-mark-for-unflag)
      (define-key map (kbd "=") 'mu4e-view-mark-for-untrash)
      (define-key map (kbd "&") 'mu4e-view-mark-custom)
      
      (define-key map (kbd "*")             'mu4e-view-mark-for-something)
      (define-key map (kbd "<kp-multiply>") 'mu4e-view-mark-for-something)
      (define-key map (kbd "<insert>")     'mu4e-view-mark-for-something)
      (define-key map (kbd "<insertchar>") 'mu4e-view-mark-for-something)

      (define-key map (kbd "#") 'mu4e-mark-resolve-deferred-marks)

      ;; misc
      (define-key map "w" 'visual-line-mode)
      (define-key map "h" 'mu4e-view-toggle-hide-cited)
      (define-key map (kbd "M-q") 'mu4e-view-fill-long-lines)

      ;; next 3 only warn user when attempt in the message view
      (define-key map "u" 'mu4e-view-unmark)
      (define-key map "U" 'mu4e-view-unmark-all)
      (define-key map "x" 'mu4e-view-marked-execute)

      (define-key map "$" 'mu4e-show-log)
      (define-key map "H" 'mu4e-display-manual)

      ;; menu
      (define-key map [menu-bar] (make-sparse-keymap))
      (let ((menumap (make-sparse-keymap "View")))
	(define-key map [menu-bar headers] (cons "View" menumap))

	(define-key menumap [quit-buffer]
	  '("Quit view" . mu4e~view-quit-buffer))
	(define-key menumap [display-help] '("Help" . mu4e-display-manual))

	(define-key menumap [sepa0] '("--"))
	(define-key menumap [wrap-lines]
	  '("Toggle wrap lines" . visual-line-mode))
	(define-key menumap [hide-cited]
	  '("Toggle hide cited" . mu4e-view-toggle-hide-cited))
	(define-key menumap [raw-view]
	  '("View raw message" . mu4e-view-raw-message))
	(define-key menumap [pipe]
	  '("Pipe through shell" . mu4e-view-pipe))
	;; (define-key menumap [inspect]
	;;   '("Inspect with guile" . mu4e-inspect-message))

	(define-key menumap [sepa8] '("--"))
	(define-key menumap [open-att]
	  '("Open attachment" . mu4e-view-open-attachment))
	(define-key menumap [extract-att]
	  '("Extract attachment" . mu4e-view-save-attachment))
	(define-key menumap [goto-url]
	  '("Visit URL" . mu4e-view-go-to-url))

	(define-key menumap [sepa1] '("--"))
	(define-key menumap [mark-delete]
	  '("Mark for deletion" . mu4e-view-mark-for-delete))
	(define-key menumap [mark-trash]
	  '("Mark for trash" .  mu4e-view-mark-for-trash))
	(define-key menumap [mark-move]
	  '("Mark for move" . mu4e-view-mark-for-move))

	(define-key menumap [sepa2] '("--"))
	(define-key menumap [compose-new]  '("Compose new" . mu4e-compose-new))
	(define-key menumap [forward]  '("Forward" . mu4e-compose-forward))
	(define-key menumap [reply]  '("Reply" . mu4e-compose-reply))
	(define-key menumap [sepa3] '("--"))

	(define-key menumap [query-next]
	  '("Next query" . mu4e-headers-query-next))
	(define-key menumap [query-prev]
	  '("Previous query" . mu4e-headers-query-prev))
	(define-key menumap [narrow-search]
	  '("Narrow search" . mu4e-headers-search-narrow))
	(define-key menumap [bookmark]
	  '("Search bookmark" . mu4e-headers-search-bookmark))
	(define-key menumap [jump]
	  '("Jump to maildir" . mu4e~headers-jump-to-maildir))
	(define-key menumap [refresh]
	  '("Refresh" . mu4e-headers-rerun-search))
	(define-key menumap [search]
	  '("Search" . mu4e-headers-search))


	(define-key menumap [sepa4] '("--"))
	(define-key menumap [next]  '("Next" . mu4e-view-headers-next))
	(define-key menumap [previous]  '("Previous" . mu4e-view-headers-prev)))
      map)))

(fset 'mu4e-view-mode-map mu4e-view-mode-map)

(defcustom mu4e-view-mode-hook nil
  "Hook run when entering Mu4e-View mode."
  :options '(turn-on-visual-line-mode)
  :type 'hook
  :group 'mu4e-view)

(defvar mu4e-view-mode-abbrev-table nil)
(define-derived-mode mu4e-view-mode special-mode "mu4e:view"
  "Major mode for viewing an e-mail message in mu4e.
\\{mu4e-view-mode-map}."
  (use-local-map mu4e-view-mode-map)

  (make-local-variable 'mu4e~view-headers-buffer)
  (make-local-variable 'mu4e~view-msg)
  (make-local-variable 'mu4e~view-link-map)
  (make-local-variable 'mu4e~view-attach-map)
  (make-local-variable 'mu4e~view-cited-hidden)

  (setq buffer-undo-list t) ;; don't record undo info

  ;; autopair mode gives error when pressing RET
  ;; turn it off
  (when (boundp 'autopair-dont-activate)
    (setq autopair-dont-activate t)))

(defun mu4e~view-mark-as-read (msg)
  "Clear the message MSG New/Unread status and set it to Seen.
If the message is not New/Unread, do nothing. Evaluates to t if it
triggers any changes, nil otherwise. If this function does any
changes, it triggers a refresh."
  (when msg
    (let ((flags (mu4e-message-field msg :flags))
	   (msgid (mu4e-message-field msg :message-id))
	   (docid (mu4e-message-field msg :docid)))
      ;; attached (embedded) messages don't have docids; leave them alone
      ;; is it a new message
      (when (and docid (or (member 'unread flags) (member 'new flags)))
	;; mark /all/ messages with this message-id as read, so all copies of
	;; this message will be marked as read.
	(mu4e~proc-move msgid nil "+S-u-N")
	t))))

(defun mu4e~view-browse-url-func (url)
  "Return a function that executes `browse-url' with URL.
What browser is called is depending on
`browse-url-browser-function' and `browse-url-mailto-function'."
  (save-match-data
    (if (string-match "^mailto:" url)
      (lexical-let ((url url))
	(lambda ()
	  (interactive)
	  (mu4e~compose-browse-url-mail url)))
      (lexical-let ((url url))
	(lambda ()
	  (interactive)
	  (browse-url url))))))

(defun mu4e~view-browse-url-from-binding (&optional url)
  "View in browser the url at point, or click location.
If the optional argument URL is provided, browse that instead.
If the url is mailto link, start writing an email to that address."
  (interactive)
  (let* (( url (or url (mu4e~view-get-property-from-event 'mu4e-url))))
    (when url
      (if (string-match-p "^mailto:" url)
	  (mu4e~compose-browse-url-mail url)
	(browse-url url)))))

(defun mu4e~view-show-images-maybe (msg)
  "Show attached images, if `mu4e-show-images' is non-nil."
  (when (and (display-images-p) mu4e-view-show-images)
    (mu4e-view-for-each-part msg
      (lambda (msg part)
	(when (string-match "^image/"
		(or (mu4e-message-part-field part :mime-type)
		  "application/object-stream"))
	  (let ((imgfile (mu4e-message-part-field part :temp)))
	    (when (and imgfile (file-exists-p imgfile))
 	      (save-excursion
		(goto-char (point-max))
		(mu4e-display-image imgfile
		  mu4e-view-image-max-width
		  mu4e-view-image-max-height)))))))))


(defvar mu4e~view-beginning-of-url-regexp
  "https?\\://\\|mailto:"
  "Regexp that matches the beginning of http:/https:/mailto: URLs; match-string 1
will contain the matched URL, if any.")

;; this is fairly simplistic...
(defun mu4e~view-make-urls-clickable ()
  "Turn things that look like URLs into clickable things.
Also number them so they can be opened using `mu4e-view-go-to-url'."
  (let ((num 0))
    (save-excursion
      (setq mu4e~view-link-map ;; buffer local
	(make-hash-table :size 32 :weakness nil))
      (goto-char (point-min))
      (while (re-search-forward mu4e~view-beginning-of-url-regexp nil t)
	(let ((bounds (thing-at-point-bounds-of-url-at-point)))
	  (when bounds
	    (let* ((url (thing-at-point-url-at-point))
		    (ov (make-overlay (car bounds) (cdr bounds))))
	      (puthash (incf num) url mu4e~view-link-map)
	      (add-text-properties
		(car bounds)
		(cdr bounds)
		`(face mu4e-link-face
		   mouse-face highlight
		   mu4e-url ,url
		   keymap ,mu4e-view-clickable-urls-keymap
		   help-echo
		   "[mouse-1] or [M-RET] to open the link"))
	      (overlay-put ov 'after-string
		(propertize (format "[%d]" num)
		  'face 'mu4e-url-number-face)))))))))


(defun mu4e~view-hide-cited ()
  "Toggle hiding of cited lines in the message body."
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (flush-lines mu4e-cited-regexp)
      (setq mu4e~view-cited-hidden t))))

(defmacro mu4e~view-in-headers-context (&rest body)
  "Evaluate BODY in the context of the headers buffer connected to
this view."
  `(progn
     (unless (buffer-live-p mu4e~view-headers-buffer)
       (mu4e-error "no headers-buffer connected"))
     (let* ((msg (mu4e-message-at-point))
	     (docid (mu4e-message-field msg :docid))
	     (curwin (selected-window)))
       (unless docid
	 (mu4e-error "message without docid: action is not possible."))
       (with-current-buffer mu4e~view-headers-buffer
	 (select-window (get-buffer-window))
	 (if (mu4e~headers-goto-docid docid)
	   ,@body
	   (mu4e-error "cannot find message in headers buffer."))))))

(defun mu4e-view-headers-next (&optional n)
  "Move point to the next message header in the headers buffer
connected with this message view. If this succeeds, return the new
docid. Otherwise, return nil. Optionally, takes an integer
N (prefix argument), to the Nth next header."
  (interactive "P")
  (mu4e~view-in-headers-context
    (mu4e~headers-move (or n 1))))

(defun mu4e-view-headers-prev (&optional n)
  "Move point to the previous message header in the headers buffer
connected with this message view. If this succeeds, return the new
docid. Otherwise, return nil. Optionally, takes an integer
N (prefix argument), to the Nth previous header."
  (interactive "P")
  (mu4e~view-in-headers-context
    (mu4e~headers-move (- (or n 1)))))

(defun mu4e-view-headers-next-unread (&optional backwards)
  "Move point to the next or previous (when BACKWARDS is non-`nil')
unread message header in the headers buffer connected with this
message view. If this succeeds, return the new docid. Otherwise,
return nil."
  (interactive "P")
  (mu4e~view-in-headers-context 
    (mu4e-headers-next-unread backwards))
  (mu4e-select-other-view)
  (mu4e-headers-view-message))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interactive functions

(defun mu4e-view-toggle-hide-cited ()
  "Toggle hiding of cited lines in the message body."
  (interactive)
  (if mu4e~view-cited-hidden
    (mu4e-view-refresh)
    (mu4e~view-hide-cited)))

(defun mu4e-view-refresh ()
  "Redisplay the current message."
  (interactive)
  (mu4e-view mu4e~view-msg mu4e~view-headers-buffer)
  (setq mu4e~view-cited-hidden nil))

(defun mu4e-view-action (&optional msg)
  "Ask user for some action to apply on MSG, then do it.
If MSG is nil apply action to message returned
bymessage-at-point.  The actions are specified in
`mu4e-view-actions'."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)))
	  (actionfunc (mu4e-read-option "Action: " mu4e-view-actions)))
    (funcall actionfunc msg)))

(defun mu4e-view-mark-pattern ()
    "Ask user for a kind of mark (move, delete etc.), a field to
match and a regular expression to match with. Then, mark all
matching messages with that mark."
  (interactive)
  (mu4e~view-in-headers-context (mu4e-headers-mark-pattern)))

(defun mu4e-view-mark-thread ()
  "Ask user for a kind of mark (move, delete etc.), and apply it to
all messages in the thread at point in the headers view."
  (interactive)
  (mu4e~view-in-headers-context (mu4e-headers-mark-thread)))

(defun mu4e-view-mark-subthread ()
  "Ask user for a kind of mark (move, delete etc.), and apply it to
all messages in the subthread at point in the headers view."
  (interactive)
  (mu4e~view-in-headers-context (mu4e-headers-mark-subthread)))

(defun mu4e-view-search-narrow ()
  "Run `mu4e-headers-search-narrow' in the headers buffer."
  (interactive)
  (mu4e~view-in-headers-context (call-interactively 'mu4e-headers-search-narrow)))

(defun mu4e-view-search-edit ()
  "Run `mu4e-headers-search-edit' in the headers buffer."
  (interactive)
  (mu4e~view-in-headers-context (mu4e-headers-search-edit)))

(defun mu4e-mark-region-code ()
  "Highlight region marked with `message-mark-inserted-region'.
Add this function to `mu4e-view-mode-hook' to enable this feature."
  (require 'message)
  (let (beg end ov-beg ov-end ov-inv)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (concat "^" message-mark-insert-begin) nil t)
        (setq ov-beg (match-beginning 0)
              ov-end (match-end 0)
              ov-inv (make-overlay ov-beg ov-end)
              beg    ov-end)
        (overlay-put ov-inv 'invisible t)
        (when (re-search-forward
               (concat "^" message-mark-insert-end) nil t)
          (setq ov-beg (match-beginning 0)
                ov-end (match-end 0)
                ov-inv (make-overlay ov-beg ov-end)
                end    ov-beg)
          (overlay-put ov-inv 'invisible t))
        (when (and beg end)
          (let ((ov (make-overlay beg end)))
            (overlay-put ov 'face 'mu4e-region-code))
          (setq beg nil end nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wash functions
(defun mu4e-view-fill-long-lines ()
  "Fill lines that are wider than the window width or `fill-column'."
  (interactive)
  (with-current-buffer mu4e~view-buffer
    (save-excursion
      (let ((inhibit-read-only t)
            (width (window-width (get-buffer-window (current-buffer)))))
        (save-restriction
          (message-goto-body)
          (while (not (eobp))
            (end-of-line)
            (when (>= (current-column) (min fill-column width))
              (narrow-to-region (min (1+ (point)) (point-max))
                                (point-at-bol))
              (let ((goback (point-marker)))
                (fill-paragraph nil)
                (goto-char (marker-position goback)))
              (widen))
            (forward-line 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; attachment handling
(defun mu4e~view-get-attach-num (prompt msg &optional multi)
  "Ask the user with PROMPT for an attachment number for MSG, and
ensure it is valid. The number is [1..n] for attachments
\[0..(n-1)] in the message. If MULTI is nil, return the number for
the attachment; otherwise (MULTI is non-nil), accept ranges of
attachment numbers, as per `mu4e-split-ranges-to-numbers', and
return the corresponding string."
  (let* ((count (hash-table-count mu4e~view-attach-map)) (def))
    (when (zerop count) (mu4e-error "No attachments for this message"))
    (if (not multi)
      (if (= count 1)
	(read-number (mu4e-format "%s: " prompt) 1)
	(read-number (mu4e-format "%s (1-%d): " prompt count)))
      (progn
	(setq def (if (= count 1) "1" (format "1-%d" count)))
	(read-string (mu4e-format "%s (default %s): " prompt def)
	  nil nil def)))))

(defun mu4e~view-get-attach (msg attnum)
  "Return the attachment plist in MSG corresponding to attachment
number ATTNUM."
  (let* ((partid (gethash attnum mu4e~view-attach-map))
	 (attach
	   (find-if
	     (lambda (part)
	       (eq (mu4e-message-part-field part :index) partid))
	     (mu4e-message-field msg :parts))))
    (or attach (mu4e-error "Not a valid attachment"))))


(defun mu4e~view-request-attachment-path (fname path)
  "Ask the user where to save FNAME (default is PATH/FNAME)."
  (let ((fpath (expand-file-name
                (read-file-name
                 (mu4e-format "Save as ")
                 path nil nil fname) path)))
    (if (file-directory-p fpath)
      (expand-file-name fname fpath)
      fpath)))

(defun mu4e~view-request-attachments-dir (path)
  "Ask the user where to save multiple attachments (default is PATH)."
  (let ((fpath (expand-file-name
                (read-directory-name
                 (mu4e-format "Save in directory ")
                 path nil nil nil) path)))
    (if (file-directory-p fpath)
        fpath)))

(defun mu4e-view-save-attachment-single (&optional msg attnum)
  "Save attachment number ATTNUM from MSG.
If MSG is nil use the message returned by `message-at-point'.
If ATTNUM is nil ask for the attachment number."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)))
	  (attnum (or attnum
		    (mu4e~view-get-attach-num "Attachment to save" msg)))
	  (att (mu4e~view-get-attach msg attnum))
	  (fname  (plist-get att :name))
	  (mtype  (plist-get att :mime-type))
	  (path (concat
		  (mu4e~get-attachment-dir fname mtype) "/"))
	  (index (plist-get att :index))
	  (retry t) (fpath))
    (while retry
      (setq fpath (mu4e~view-request-attachment-path fname path))
      (setq retry
	(and (file-exists-p fpath)
	  (not (y-or-n-p (mu4e-format "Overwrite '%s'?" fpath))))))
    (mu4e~proc-extract
      'save (mu4e-message-field msg :docid) index mu4e-decryption-policy fpath)))


(defun mu4e-view-save-attachment-multi (&optional msg)
  "Offer to save multiple email attachments from the current message.
Default is to save all messages, [1..n], where n is the number of
attachments.  You can type multiple values separated by space, e.g.
  1 3-6 8
will save attachments 1,3,4,5,6 and 8.

Furthermore, there is a shortcut \"a\" which so means all
attachments, but as this is the default, you may not need it."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)))
         (attachstr (mu4e~view-get-attach-num
                     "Attachment number range (or 'a' for 'all')" msg t))
         (count (hash-table-count mu4e~view-attach-map))
         (attachnums (mu4e-split-ranges-to-numbers attachstr count)))
    (if mu4e-save-multiple-attachments-without-asking
        (let* ((path (concat (mu4e~get-attachment-dir) "/"))
               (attachdir (mu4e~view-request-attachments-dir path)))
          (dolist (num attachnums)
            (let* ((att (mu4e~view-get-attach msg num))
                   (fname  (plist-get att :name))
                   (index (plist-get att :index))
                   (retry t))
              (while retry
                (setq fpath (expand-file-name (concat attachdir fname) path))
                (setq retry
                      (and (file-exists-p fpath)
                           (not (y-or-n-p (mu4e-format "Overwrite '%s'?" fpath))))))
              (mu4e~proc-extract
               'save (mu4e-message-field msg :docid) index mu4e-decryption-policy fpath))))
      (dolist (num attachnums)
        (mu4e-view-save-attachment-single msg num)))))

(defun mu4e-view-save-attachment (&optional multi)
  "Offer to save attachment(s).
If MULTI (prefix-argument) is nil, save a single one, otherwise,
offer to save a range of attachments."
  (interactive "P")
  (if multi
    (mu4e-view-save-attachment-multi)
    (mu4e-view-save-attachment-single)))

(defun mu4e-view-open-attachment (&optional msg attnum)
  "Open attachment number ATTNUM from MSG.
If MSG is nil use the message returned by `message-at-point'.
If ATTNUM is nil ask for the attachment number."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)))
	  (attnum (or attnum
		    (mu4e~view-get-attach-num "Attachment to open" msg)))
	  (att (or (mu4e~view-get-attach msg attnum)))
	  (index (plist-get att :index))
	  (docid (mu4e-message-field msg :docid))
	  (mimetype (plist-get att :mime-type)))
    (if (and mimetype (string= mimetype "message/rfc822"))
      ;; special handling for message-attachments; we open them in mu4e. we also
      ;; send the docid as parameter (4th arg); we'll get this back from the
      ;; server, and use it to determine the parent message (ie., the current
      ;; message) when showing the embedded message/rfc822, and return to the
      ;; current message when quiting that one.
      (mu4e~view-temp-action docid index "mu4e" docid)
      ;; otherwise, open with the default program (handled in mu-server
      (mu4e~proc-extract 'open docid index mu4e-decryption-policy))))


(defun mu4e~view-temp-action (docid index what &optional param)
  "Open attachment INDEX for message with DOCID, and invoke ACTION."
  (interactive)
  (mu4e~proc-extract 'temp docid index mu4e-decryption-policy nil what param ))

(defvar mu4e~view-open-with-hist nil "History list for the open-with argument.")

(defun mu4e-view-open-attachment-with (msg attachnum &optional cmd)
  "Open MSG's attachment ATTACHNUM with CMD.
If CMD is nil, ask user for it."
  (interactive)
  (let* ((att (mu4e~view-get-attach msg attachnum))
	  (cmd (or cmd
		 (read-string
		   (mu4e-format "Shell command to open it with: ")
		   nil 'mu4e~view-open-with-hist)))
	  (index (plist-get att :index)))
    (mu4e~view-temp-action
      (mu4e-message-field msg :docid) index "open-with" cmd)))

(defvar mu4e~view-pipe-hist nil
  "History list for the pipe argument.")

(defun mu4e-view-pipe-attachment (msg attachnum &optional pipecmd)
  "Feed MSG's attachment ATTACHNUM through pipe PIPECMD.
If PIPECMD is nil, ask user for it."
  (interactive)
  (let* ((att (mu4e~view-get-attach msg attachnum))
	  (pipecmd (or pipecmd
		     (read-string
		       (mu4e-format "Pipe: ")
		       nil
		       'mu4e~view-pipe-hist)))
	  (index (plist-get att :index)))
    (mu4e~view-temp-action
      (mu4e-message-field msg :docid) index "pipe" pipecmd)))


(defun mu4e-view-open-attachment-emacs (msg attachnum)
  "Open MSG's attachment ATTACHNUM in the current emacs instance."
  (interactive)
  (let* ((att (mu4e~view-get-attach msg attachnum))
	  (index (plist-get att :index)))
    (mu4e~view-temp-action (mu4e-message-field msg :docid) index "emacs")))

(defun mu4e-view-import-attachment-diary (msg attachnum)
  "Open MSG's attachment ATTACHNUM in the current emacs instance."
  (interactive)
  (let* ((att (mu4e~view-get-attach msg attachnum))
	  (index (plist-get att :index)))
    (mu4e~view-temp-action (mu4e-message-field msg :docid) index "diary")))

(defun mu4e-view-attachment-action (&optional msg)
  "Ask user what to do with attachments in MSG
If MSG is nil use the message returned by `message-at-point'.
The actions are specified in `mu4e-view-attachment-actions'."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)))
	  (actionfunc (mu4e-read-option
			"Action on attachment: "
			mu4e-view-attachment-actions))
	  (attnum (mu4e~view-get-attach-num "Which attachment" msg)))
    (when (and actionfunc attnum)
      (funcall actionfunc msg attnum))))

;; handler-function to handle the response we get from the server when we
;; want to do something with one of the attachments.
(defun mu4e~view-temp-handler (path what docid param)
  "Handler function for doing things with temp files (ie.,
attachments) in response to a (mu4e~proc-extract 'temp ... )."
  (cond
    ((string= what "open-with")
      ;; 'param' will be the program to open-with
      (start-process "*mu4e-open-with-proc*" "*mu4e-open-with*" param path))
    ((string= what "pipe")
      ;; 'param' will be the pipe command, path the infile for this
      (mu4e-process-file-through-pipe path param))
    ;; if it's mu4e, it's some embedded message; 'param' may contain the docid of
    ;; the parent message.
    ((string= what "mu4e")
      ;; remember the mapping path->docid, which maps the path of the embedded
      ;; message to the docid of its parent
      (puthash path docid mu4e~path-parent-docid-map)
      (mu4e~proc-view-path path mu4e-view-show-images mu4e-decryption-policy))
    ((string= what "emacs")
      (find-file path)
      ;; make the buffer read-only since it usually does not make
      ;; sense to edit the temp buffer; use C-x C-q if you insist...
      (setq buffer-read-only t))
    ((string= what "diary")
     (icalendar-import-file path diary-file))
    (t (mu4e-error "Unsupported action %S" what))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mu4e-view-mark-custom ()
  "Run some custom mark function."
  (mu4e~view-in-headers-context
    (mu4e-headers-mark-custom)))

(defun mu4e~view-split-view-p ()
  "Return t if we're in split-view, nil otherwise."
  (member mu4e-split-view '(horizontal vertical)))

(defun mu4e-view-scroll-up-or-next ()
  "Scroll-up the current message.
If `mu4e-view-scroll-to-next' is non-nil, and we can't scroll-up
anymore, go the next message."
  (interactive)
  (condition-case nil
    (scroll-up)
    (error
      (when mu4e-view-scroll-to-next
	(mu4e-view-headers-next)))))

(defun mu4e-scroll-up ()
  "Scroll text of selected window up one line."
  (interactive)
  (scroll-up 1))

(defun mu4e-scroll-down ()
  "Scroll text of selected window down one line."
  (interactive)
  (scroll-down 1))

(defun mu4e-view-unmark-all ()
  "If we're in split-view, unmark all messages.
Otherwise, warn user that unmarking only works in the header
list."
  (interactive)
  (if (mu4e~view-split-view-p)
    (mu4e~view-in-headers-context (mu4e-mark-unmark-all))
    (mu4e-message "Unmarking needs to be done in the header list view")))

(defun mu4e-view-unmark ()
  "If we're in split-view, unmark message at point.
Otherwise, warn user that unmarking only works in the header
list."
  (interactive)
  (if (mu4e~view-split-view-p)
    (mu4e-view-mark-for-unmark)
    (mu4e-message "Unmarking needs to be done in the header list view")))


(defmacro mu4e~view-defun-mark-for (mark)
  "Define a function mu4e-view-mark-for-MARK."
  (let ((funcname (intern (format "mu4e-view-mark-for-%s" mark)))
	(docstring (format "Mark the current message for %s." mark)))
    `(progn
       (defun ,funcname () ,docstring
	 (interactive)
	 (mu4e~view-in-headers-context
	  (mu4e-headers-mark-and-next ',mark)))
       (put ',funcname 'definition-name ',mark))))

(mu4e~view-defun-mark-for move)
(mu4e~view-defun-mark-for trash)
(mu4e~view-defun-mark-for refile)
(mu4e~view-defun-mark-for delete)
(mu4e~view-defun-mark-for flag)
(mu4e~view-defun-mark-for unflag)
(mu4e~view-defun-mark-for unmark)
(mu4e~view-defun-mark-for something)
(mu4e~view-defun-mark-for read)
(mu4e~view-defun-mark-for unread)

(defun mu4e-view-marked-execute ()
  "Execute the marks."
  (interactive)
  (mu4e~view-in-headers-context
    (mu4e-mark-execute-all)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; URL handling
(defun mu4e~view-get-urls-num (prompt &optional multi)
  "Ask the user with PROMPT for an URL number for MSG, and ensure
it is valid. The number is [1..n] for URLs \[0..(n-1)] in the
message. If MULTI is nil, return the number for the URL;
otherwise (MULTI is non-nil), accept ranges of URL numbers, as
per `mu4e-split-ranges-to-numbers', and return the corresponding
string."
  (let* ((count (hash-table-count mu4e~view-link-map)) (def))
    (when (zerop count) (mu4e-error "No links for this message"))
    (if (not multi)
      (if (= count 1)
	(read-number (mu4e-format "%s: " prompt) 1)
	(read-number (mu4e-format "%s (1-%d): " prompt count)))
      (progn
	(setq def (if (= count 1) "1" (format "1-%d" count)))
	(read-string (mu4e-format "%s (default %s): " prompt def)
	  nil nil def)))))

(defun mu4e-view-go-to-url (&optional multi)
  "Offer to go to url(s). If MULTI (prefix-argument) is nil, go to
a single one, otherwise, offer to go to a range of urls."
  (interactive "P")
  (mu4e~view-handle-urls "URL to visit"
    multi (lambda (url) (mu4e~view-browse-url-from-binding url))))

(defun mu4e-view-save-url (&optional multi)
  "Offer to save urls(s) to the kill-ring. If
MULTI (prefix-argument) is nil, save a single one, otherwise, offer
to save a range of URLs."
  (interactive "P")
  (mu4e~view-handle-urls "URL to save" multi
    (lambda (url)
      (kill-new url)
      (mu4e-message "Saved %s to the kill-ring" url))))

(defun mu4e~view-handle-urls (prompt multi urlfunc)
  "If MULTI is nil, apply URLFUNC to a single uri, otherwise, apply
it to a range of uris. PROMPT is the query to present to the user."
  (interactive "P")
  (if multi
    (mu4e~view-handle-multi-urls prompt urlfunc)
    (mu4e~view-handle-single-url prompt urlfunc)))

(defun mu4e~view-handle-single-url (prompt urlfunc &optional num)
  "Apply URLFUNC to url NUM in the current message, prompting the
user with PROMPT."
  (interactive)
  (let* ((num (or num (mu4e~view-get-urls-num prompt)))
         (url (gethash num mu4e~view-link-map)))
    (unless url (mu4e-warn "Invalid number for URL"))
    (funcall urlfunc url)))

(defun mu4e~view-handle-multi-urls (prompt urlfunc)
  "Apply URLFUNC to a a range of urls in the current message,
prompting the user with PROMPT.

Default is to aplly it to all URLs, [1..n], where n is the number
of urls. You can type multiple values separated by space, e.g.  1
3-6 8 will visit urls 1,3,4,5,6 and 8.

Furthermore, there is a shortcut \"a\" which means all urls, but as
this is the default, you may not need it."
  (interactive)
  (let* ((linkstr (mu4e~view-get-urls-num
		      "URL number range (or 'a' for 'all')" t))
	  (count (hash-table-count mu4e~view-link-map))
	  (linknums (mu4e-split-ranges-to-numbers linkstr count)))
    (dolist (num linknums)
      (mu4e~view-handle-single-url prompt urlfunc num))))

(defun mu4e-view-for-each-uri (func)
  "Execute FUNC (which receives a uri) for each uri in the current
  message."
  (maphash (lambda (num uri) (funcall func uri)) mu4e~view-link-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst mu4e~view-raw-buffer-name " *mu4e-raw-view*"
  "*internal* Name for the raw message view buffer")

(defun mu4e-view-raw-message ()
  "Display the raw contents of message at point in a new buffer."
  (interactive)
  (let ((path (mu4e-message-field-at-point :path))
	 (buf (get-buffer-create mu4e~view-raw-buffer-name)))
    (unless (and path (file-readable-p path))
      (mu4e-error "Not a readable file: %S" path))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert-file-contents path)
	(view-mode)
	(goto-char (point-min))))
    (switch-to-buffer buf)))

(defun mu4e-view-pipe (cmd)
  "Pipe the message at point through shell command CMD, and display
the results."
  (interactive "sShell command: ")
  (let ((path (mu4e-message-field (mu4e-message-at-point) :path)))
    (mu4e-process-file-through-pipe path cmd)))

(defconst mu4e~verify-buffer-name " *mu4e-verify*")

(defun mu4e-view-verify-msg-popup (&optional msg)
  "Pop-up a little signature verification window for (optional) MSG
or message-at-point."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)))
	  (path (mu4e-message-field msg :path))
	  (cmd (format "%s verify --verbose %s %s"
		 mu4e-mu-binary
		 (shell-quote-argument path)
		 (if mu4e-decryption-policy
		     "--decrypt --use-agent"
		     "")))
	  (output (shell-command-to-string cmd))
	    ;; create a new one
	  (buf (get-buffer-create mu4e~verify-buffer-name))
	  (win (or (get-buffer-window buf)
		 (split-window-vertically (- (window-height) 6)))))
    (with-selected-window win
      (let ((inhibit-read-only t))
	;; (set-window-dedicated-p win t)
	(switch-to-buffer buf)
	(erase-buffer)
	(insert output)
	(goto-char (point-min))
	(local-set-key "q" 'kill-buffer-and-window))
      (setq buffer-read-only t))
    (select-window win)))


(defun mu4e~view-quit-buffer ()
  "Quit the mu4e-view buffer.
This is a rather complex function, to ensure we don't disturb
other windows."
  (interactive)
  (unless (eq major-mode 'mu4e-view-mode)
    (mu4e-error "Must be in mu4e-view-mode (%S)" major-mode))
  (let ((curbuf (current-buffer)) (curwin (selected-window))
	 (headers-win))
    (walk-windows
      (lambda (win)
	;; check whether the headers buffer window is visible
	(when (eq mu4e~view-headers-buffer (window-buffer win))
	  (setq headers-win win))
	;; and kill any _other_ (non-selected) window that shows the current
	;; buffer
	(when
	  (and
	    (eq curbuf (window-buffer win)) ;; does win show curbuf?
	    (not (eq curwin win))	    ;; but it's not the curwin?
	    (not (one-window-p))) ;; and not the last one on the frame?
	  (delete-window win))))  ;; delete it!
    ;; now, all *other* windows should be gone.
    ;; if the headers view is also visible, kill ourselves + window; otherwise
    ;; switch to the headers view
    (if (window-live-p headers-win)
      ;; headers are visible
      (progn
	(kill-buffer-and-window) ;; kill the view win
        (setq mu4e~headers-view-win nil)
	(select-window headers-win)) ;; and switch to the headers win...
      ;; headers are not visible...
      (progn
	(kill-buffer)
        (setq mu4e~headers-view-win nil)
	(when (buffer-live-p mu4e~view-headers-buffer)
	  (switch-to-buffer mu4e~view-headers-buffer))))))

(provide 'mu4e-view)
;; end of mu4e-view
