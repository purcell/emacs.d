;;; org-contacts.el --- Contacts management

;; Copyright (C) 2010-2012 Julien Danjou <julien@danjou.info>

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: outlines, hypermedia, calendar
;;
;; This file is NOT part of GNU Emacs.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the code for managing your contacts into Org-mode.

;; To enter new contacts, you can use `org-capture' and a template just like
;; this:

;;         ("c" "Contacts" entry (file "~/Org/contacts.org")
;;          "* %(org-contacts-template-name)
;; :PROPERTIES:
;; :EMAIL: %(org-contacts-template-email)
;; :END:")))
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (require 'org))

(defgroup org-contacts nil
  "Options concerning contacts management."
  :group 'org)

(defcustom org-contacts-files nil
  "List of Org files to use as contacts source.
If set to nil, all your Org files will be used."
  :type '(repeat file)
  :group 'org-contacts)

(defcustom org-contacts-email-property "EMAIL"
  "Name of the property for contact email address."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-address-property "ADDRESS"
  "Name of the property for contact address."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-birthday-property "BIRTHDAY"
  "Name of the property for contact birthday date."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-birthday-format "Birthday: %l (%Y)"
  "Format of the anniversary agenda entry. The following replacements are available:

  %h - Heading name
  %l - Link to the heading
  %y - Number of year
  %Y - Number of year (ordinal)"
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-last-read-mail-property "LAST_READ_MAIL"
  "Name of the property for contact last read email link storage."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-icon-property "ICON"
  "Name of the property for contact icon."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-nickname-property "NICKNAME"
  "Name of the property for IRC nickname match."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-icon-size 32
  "Size of the contacts icons."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-icon-use-gravatar (fboundp 'gravatar-retrieve)
  "Whether use Gravatar to fetch contact icons."
  :type 'boolean
  :group 'org-contacts)

(defcustom org-contacts-completion-ignore-case t
  "Ignore case when completing contacts."
  :type 'boolean
  :group 'org-contacts)

(defcustom org-contacts-group-prefix "+"
  "Group prefix."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-matcher (concat org-contacts-email-property "<>\"\"")
  "Matching rule for finding heading that are contacts.
This can be a tag name, or a property check."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-email-link-description-format "%s (%d)"
  "Format used to store links to email.
This overrides `org-email-link-description-format' if set."
  :group 'org-contacts
  :type 'string)

(defcustom org-contacts-vcard-file "contacts.vcf"
  "Default file for vcard export."
  :group 'org-contacts
  :type 'file)

(defvar org-contacts-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "M" 'org-contacts-view-send-email)
    (define-key map "i" 'org-contacts-view-switch-to-irc-buffer)
    map)
  "The keymap used in `org-contacts' result list.")

(defun org-contacts-files ()
  "Return list of Org files to use for contact management."
  (or org-contacts-files (org-agenda-files t 'ifmode)))

(defun org-contacts-filter (&optional name-match tags-match)
  "Search for a contact maching NAME-MATCH and TAGS-MATCH.
If both match values are nil, return all contacts."
  (let* (todo-only
	(tags-matcher
         (if tags-match
             (cdr (org-make-tags-matcher tags-match))
           t))
        (name-matcher
         (if name-match
             '(org-string-match-p name-match (org-get-heading t))
           t))
        (contacts-matcher
         (cdr (org-make-tags-matcher org-contacts-matcher)))
        markers result)
    (dolist (file (org-contacts-files))
      (org-check-agenda-file file)
      (with-current-buffer (org-get-agenda-file-buffer file)
        (unless (eq major-mode 'org-mode)
          (error "File %s is no in `org-mode'" file))
        (org-scan-tags
         '(add-to-list 'markers (set-marker (make-marker) (point)))
         `(and ,contacts-matcher ,tags-matcher ,name-matcher)
	 todo-only)))
    (dolist (marker markers result)
      (org-with-point-at marker
        (add-to-list 'result
                     (list (org-get-heading t) marker (org-entry-properties marker 'all)))))))

(when (not (fboundp 'completion-table-case-fold))
  ;; That function is new in Emacs 24...
  (defun completion-table-case-fold (table &optional dont-fold)
    (lambda (string pred action)
      (let ((completion-ignore-case (not dont-fold)))
	(complete-with-action action table string pred)))))

(defun org-contacts-complete-name (&optional start)
  "Complete text at START with a user name and email."
  (let* ((end (point))
         (start (or start
                    (save-excursion
                      (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                      (goto-char (match-end 0))
                      (point))))
         (orig (buffer-substring start end))
         (completion-ignore-case org-contacts-completion-ignore-case)
         (group-completion-p (org-string-match-p (concat "^" org-contacts-group-prefix) orig))
         (completion-list
          (if group-completion-p
              (mapcar (lambda (group) (propertize (concat org-contacts-group-prefix group) 'org-contacts-group group))
                      (org-uniquify
                       (loop for contact in (org-contacts-filter)
                             with group-list
                             nconc (org-split-string
                                    (or (cdr (assoc-string "ALLTAGS" (caddr contact))) "") ":"))))
            (loop for contact in (org-contacts-filter)
                  ;; The contact name is always the car of the assoc-list
                  ;; returned by `org-contacts-filter'.
                  for contact-name = (car contact)
                  ;; Build the list of the user email addresses.
                  for email-list = (split-string (or
                                                  (cdr (assoc-string org-contacts-email-property (caddr contact)))
                                                  ""))
                  ;; If the user has email addresses…
                  if email-list
                  ;; … append a list of USER <EMAIL>.
                  nconc (loop for email in email-list
                              collect (org-contacts-format-email contact-name email)))))
         (completion-list (all-completions orig completion-list)))
    ;; If we are completing a group, and that's the only group, just return
    ;; the real result.
    (when (and group-completion-p
               (= (length completion-list) 1))
      (setq completion-list
            (list (concat (car completion-list) ";: "
                          (mapconcat 'identity
                                     (loop for contact in (org-contacts-filter
                                                           nil
                                                           (get-text-property 0 'org-contacts-group (car completion-list)))
                                           ;; The contact name is always the car of the assoc-list
                                           ;; returned by `org-contacts-filter'.
                                           for contact-name = (car contact)
                                           ;; Grab the first email of the contact
                                           for email = (car (split-string (or
                                                                           (cdr (assoc-string org-contacts-email-property (caddr contact)))
                                                                           "")))
                                           ;; If the user has an email address, append USER <EMAIL>.
                                           if email collect (org-contacts-format-email contact-name email))
                                     ", ")))))
    (list start end (completion-table-case-fold completion-list (not org-contacts-completion-ignore-case)))))

(defun org-contacts-message-complete-function ()
  "Function used in `completion-at-point-functions' in `message-mode'."
  (let ((mail-abbrev-mode-regexp
         "^\\(Resent-To\\|To\\|B?Cc\\|Reply-To\\|From\\|Mail-Followup-To\\|Mail-Copies-To\\|Disposition-Notification-To\\|Return-Receipt-To\\):"))
        (when (mail-abbrev-in-expansion-header-p)
          (org-contacts-complete-name))))

(defun org-contacts-gnus-get-name-email ()
  "Get name and email address from Gnus message."
  (if (gnus-alive-p)
      (gnus-with-article-headers
        (mail-extract-address-components
         (or (mail-fetch-field "From") "")))))

(defun org-contacts-gnus-article-from-get-marker ()
  "Return a marker for a contact based on From."
  (let* ((address (org-contacts-gnus-get-name-email))
         (name (car address))
         (email (cadr address)))
    (cadar (or (org-contacts-filter
                nil
                (concat org-contacts-email-property "={\\b" (regexp-quote email) "\\b}"))
               (when name
                 (org-contacts-filter
                  (concat "^" name "$")))))))

(defun org-contacts-gnus-article-from-goto ()
  "Go to contact in the From address of current Gnus message."
  (interactive)
  (let ((marker (org-contacts-gnus-article-from-get-marker)))
    (when marker
      (switch-to-buffer-other-window (marker-buffer marker))
      (goto-char marker)
      (when (eq major-mode 'org-mode)
        (org-show-context 'agenda)
        (save-excursion
          (and (outline-next-heading)
               ;; show the next heading
               (org-flag-heading nil)))))))

(defun org-contacts-anniversaries (&optional field format)
  "Compute FIELD anniversary for each contact, returning FORMAT.
Default FIELD value is \"BIRTHDAY\".

Format is a string matching the following format specification:

  %h - Heading name
  %l - Link to the heading
  %y - Number of year
  %Y - Number of year (ordinal)"
  (let ((calendar-date-style 'american)
        (entry ""))
    (unless format (setq format org-contacts-birthday-format))
    (loop for contact in (org-contacts-filter)
          for anniv = (let ((anniv (cdr (assoc-string
                                         (or field org-contacts-birthday-property)
                                         (caddr contact)))))
                        (when anniv
                          (calendar-gregorian-from-absolute
                           (org-time-string-to-absolute anniv))))
          ;; Use `diary-anniversary' to compute anniversary.
          if (and anniv (apply 'diary-anniversary anniv))
          collect (format-spec format
                               `((?l . ,(org-with-point-at (cadr contact) (org-store-link nil)))
                                 (?h . ,(car contact))
                                 (?y . ,(- (calendar-extract-year date)
                                           (calendar-extract-year anniv)))
                                 (?Y . ,(let ((years (- (calendar-extract-year date)
                                                        (calendar-extract-year anniv))))
                                          (format "%d%s" years (diary-ordinal-suffix years)))))))))

(defun org-completing-read-date (prompt collection
                                        &optional predicate require-match initial-input
                                        hist def inherit-input-method)
  "Like `completing-read' but reads a date.
Only PROMPT and DEF are really used."
  (org-read-date nil nil nil prompt nil def))

(add-to-list 'org-property-set-functions-alist
             `(,org-contacts-birthday-property . org-completing-read-date))

(defun org-contacts-template-name (&optional return-value)
  "Try to return the contact name for a template.
If not found return RETURN-VALUE or something that would ask the user."
  (or (car (org-contacts-gnus-get-name-email))
      return-value
      "%^{Name}"))

(defun org-contacts-template-email (&optional return-value)
  "Try to return the contact email for a template.
If not found return RETURN-VALUE or something that would ask the user."
  (or (cadr (org-contacts-gnus-get-name-email))
      return-value
      (concat "%^{" org-contacts-email-property "}p")))

(defun org-contacts-gnus-store-last-mail ()
  "Store a link between mails and contacts.

This function should be called from `gnus-article-prepare-hook'."
  (let ((marker (org-contacts-gnus-article-from-get-marker)))
    (when marker
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (let* ((org-email-link-description-format (or org-contacts-email-link-description-format
                                                        org-email-link-description-format))
                 (link (gnus-with-article-buffer (org-store-link nil))))
            (org-set-property org-contacts-last-read-mail-property link)))))))

(defun org-contacts-icon-as-string ()
  (let ((image (org-contacts-get-icon)))
    (concat
     (propertize "-" 'display
                 (append
                  (if image
                      image
                    `'(space :width (,org-contacts-icon-size)))
                  '(:ascent center)))
     " ")))

;;;###autoload
(defun org-contacts (name)
  "Create agenda view for contacts matching NAME."
  (interactive (list (read-string "Name: ")))
  (let ((org-agenda-files (org-contacts-files))
        (org-agenda-skip-function
         (lambda () (org-agenda-skip-if nil `(notregexp ,name))))
        (org-agenda-format (propertize
                            "%(org-contacts-icon-as-string)% p% s%(org-contacts-irc-number-of-unread-messages)%+T"
                            'keymap org-contacts-keymap))
        (org-agenda-overriding-header
         (or org-agenda-overriding-header
             (concat "List of contacts matching `" name "':"))))
    (setq org-agenda-skip-regexp name)
    (org-tags-view nil org-contacts-matcher)
    (with-current-buffer org-agenda-buffer-name
      (setq org-agenda-redo-command
            (list 'org-contacts name)))))

(defun org-contacts-completing-read (prompt
                                     &optional predicate
                                     initial-input hist def inherit-input-method)
  "Call `completing-read' with contacts name as collection."
  (org-completing-read
   prompt (org-contacts-filter) predicate t initial-input hist def inherit-input-method))

(defun org-contacts-format-email (name email)
  "Format a mail address."
  (unless email
    (error "`email' cannot be nul"))
  (if name
      (concat name " <" email ">")
    email))

(defun org-contacts-check-mail-address (mail)
  "Add MAIL address to contact at point if it does not have it."
  (let ((mails (org-entry-get (point) org-contacts-email-property)))
    (unless (member mail (split-string mails))
      (when (yes-or-no-p
             (format "Do you want to add this address to %s?" (org-get-heading t)))
        (org-set-property org-contacts-email-property (concat mails " " mail))))))

(defun org-contacts-gnus-check-mail-address ()
  "Check that contact has the current address recorded.
This function should be called from `gnus-article-prepare-hook'."
  (let ((marker (org-contacts-gnus-article-from-get-marker)))
    (when marker
      (org-with-point-at marker
        (org-contacts-check-mail-address (cadr (org-contacts-gnus-get-name-email)))))))

(defun org-contacts-gnus-insinuate ()
  "Add some hooks for Gnus user.
This adds `org-contacts-gnus-check-mail-address' and
`org-contacts-gnus-store-last-mail' to
`gnus-article-prepare-hook'. It also adds a binding on `;' in
`gnus-summary-mode-map' to `org-contacts-gnus-article-from-goto'"
  (require 'gnus)
  (require 'gnus-art)
  (define-key gnus-summary-mode-map ";" 'org-contacts-gnus-article-from-goto)
  (add-hook 'gnus-article-prepare-hook 'org-contacts-gnus-check-mail-address)
  (add-hook 'gnus-article-prepare-hook 'org-contacts-gnus-store-last-mail))

(when (boundp 'completion-at-point-functions)
  (add-hook 'message-mode-hook
	    (lambda ()
	      (add-to-list 'completion-at-point-functions
			   'org-contacts-message-complete-function))))

(defun org-contacts-wl-get-from-header-content ()
  "Retrieve the content of the `From' header of an email.
Works from wl-summary-mode and mime-view-mode - that is while viewing email.
Depends on Wanderlust been loaded."
  (with-current-buffer (org-capture-get :original-buffer)
    (cond
     ((eq major-mode 'wl-summary-mode) (when wl-summary-buffer-elmo-folder
                                         (elmo-message-field
                                          wl-summary-buffer-elmo-folder
                                          (wl-summary-message-number)
                                          'from)))
     ((eq major-mode 'mime-view-mode) (std11-narrow-to-header)
                                      (prog1
                                          (std11-fetch-field "From")
                                        (widen))))))

(defun org-contacts-wl-get-name-email ()
  "Get name and email address from wanderlust email.
See `org-contacts-wl-get-from-header-content' for limitations."
  (let ((from (org-contacts-wl-get-from-header-content)))
    (when from
      (list (wl-address-header-extract-realname from)
	    (wl-address-header-extract-address from)))))

(defun org-contacts-template-wl-name (&optional return-value)
  "Try to return the contact name for a template from wl.
If not found return RETURN-VALUE or something that would ask the user."
  (or (car (org-contacts-wl-get-name-email))
      return-value
      "%^{Name}"))

(defun org-contacts-template-wl-email (&optional return-value)
  "Try to return the contact email for a template from wl.
If not found return RETURN-VALUE or something that would ask the user."
  (or (cadr (org-contacts-wl-get-name-email))
      return-value
      (concat "%^{" org-contacts-email-property "}p")))

(defun org-contacts-view-send-email (&optional ask)
  "Send email to the contact at point.
If ASK is set, ask for the email address even if there's only one address."
  (interactive "P")
  (let ((marker (org-get-at-bol 'org-hd-marker)))
    (org-with-point-at marker
      (let ((emails (org-entry-get (point) org-contacts-email-property)))
        (if emails
            (let ((email-list (split-string emails)))
              (if (and (= (length email-list) 1) (not ask))
                  (compose-mail (org-contacts-format-email
                                 (org-get-heading t) emails))
                (let ((email (completing-read "Send mail to which address: " email-list)))
                  (org-contacts-check-mail-address email)
                  (compose-mail (org-contacts-format-email (org-get-heading t) email)))))
          (error (format "This contact has no mail address set (no %s property)."
                         org-contacts-email-property)))))))

(defun org-contacts-get-icon (&optional pom)
  "Get icon for contact at POM."
  (setq pom (or pom (point)))
  (catch 'icon
    ;; Use `org-contacts-icon-property'
    (let ((image-data (org-entry-get pom org-contacts-icon-property)))
      (when image-data
        (throw 'icon
               (if (fboundp 'gnus-rescale-image)
                   (gnus-rescale-image (create-image image-data)
                                       (cons org-contacts-icon-size org-contacts-icon-size))
                 (create-image image-data)))))
    ;; Next, try Gravatar
    (when org-contacts-icon-use-gravatar
      (let* ((gravatar-size org-contacts-icon-size)
             (email-list (org-entry-get pom org-contacts-email-property))
             (gravatar
              (when email-list
                (loop for email in (split-string email-list)
                      for gravatar = (gravatar-retrieve-synchronously email)
                      if (and gravatar
                              (not (eq gravatar 'error)))
                      return gravatar))))
        (when gravatar (throw 'icon gravatar))))))

(defun org-contacts-irc-buffer (&optional pom)
  "Get the IRC buffer associated with the entry at POM."
  (setq pom (or pom (point)))
  (let ((nick (org-entry-get pom org-contacts-nickname-property)))
    (when nick
      (let ((buffer (get-buffer nick)))
        (when buffer
          (with-current-buffer buffer
            (when (eq major-mode 'erc-mode)
              buffer)))))))

(defun org-contacts-irc-number-of-unread-messages (&optional pom)
  "Return the number of unread messages for contact at POM."
  (when (boundp 'erc-modified-channels-alist)
    (let ((number (cadr (assoc (org-contacts-irc-buffer pom) erc-modified-channels-alist))))
      (if number
          (format (concat "%3d unread message" (if (> number 1) "s" " ") " ") number)
        (make-string 21 ? )))))

(defun org-contacts-view-switch-to-irc-buffer ()
  "Switch to the IRC buffer of the current contact if it has one."
  (interactive)
  (let ((marker (org-get-at-bol 'org-hd-marker)))
    (org-with-point-at marker
      (switch-to-buffer-other-window (org-contacts-irc-buffer)))))

(defun org-contacts-completing-read-nickname (prompt collection
                                                     &optional predicate require-match initial-input
                                                     hist def inherit-input-method)
  "Like `completing-read' but reads a nickname."
  (org-completing-read prompt (append collection (erc-nicknames-list)) predicate require-match
                       initial-input hist def inherit-input-method))

(defun erc-nicknames-list ()
  "Return all nicknames of all ERC buffers."
  (if (fboundp 'erc-buffer-list)
      (loop for buffer in (erc-buffer-list)
            nconc (with-current-buffer buffer
                    (loop for user-entry in (mapcar 'car (erc-get-channel-user-list))
                          collect (elt user-entry 1))))))

(add-to-list 'org-property-set-functions-alist
             `(,org-contacts-nickname-property . org-contacts-completing-read-nickname))

(defun org-contacts-vcard-escape (str)
  "Escape ; , and \n in STR for use in the VCard format.
Thanks to http://www.emacswiki.org/cgi-bin/wiki/bbdb-vcard-export.el for the regexp."
  (when str
    (replace-regexp-in-string "\n" "\\\\n" (replace-regexp-in-string "\\(;\\|,\\|\\\\\\)" "\\\\\\1" str))))

(defun org-contacts-vcard-encode-name (name)
  "Try to encode NAME as VCard's N property. The N property expects FamilyName;GivenName;AdditionalNames;Prefix;Postfix.
Org-contacts does not specify how to encode the name. So we try to do our best."
  (concat (replace-regexp-in-string "\\(\\w+\\) \\(.*\\)" "\\2;\\1" name) ";;;"))

(defun org-contacts-vcard-format (contact)
  "Formats CONTACT in VCard 3.0 format."
  (let* ((properties (caddr contact))
	 (name (org-contacts-vcard-escape (car contact)))
	 (n (org-contacts-vcard-encode-name name))
	 (email (org-contacts-vcard-escape (cdr (assoc-string org-contacts-email-property properties))))
	 (bday (org-contacts-vcard-escape (cdr (assoc-string org-contacts-birthday-property properties))))
	 (addr (cdr (assoc-string org-contacts-address-property properties)))
	 (nick (org-contacts-vcard-escape (cdr (assoc-string org-contacts-nickname-property properties))))

	 (head (format "BEGIN:VCARD\nVERSION:3.0\nN:%s\nFN:%s\n" n name)))
    (concat head
	    (when email (format "EMAIL:%s\n" email))
	    (when addr
	      (format "ADR:;;%s\n" (replace-regexp-in-string "\\, ?" ";" addr)))
	    (when bday
	      (let ((cal-bday (calendar-gregorian-from-absolute (org-time-string-to-absolute bday))))
		(format "BDAY:%04d-%02d-%02d\n"
			(calendar-extract-year cal-bday)
			(calendar-extract-month cal-bday)
			(calendar-extract-day cal-bday))))
	    (when nick (format "NICKNAME:%s\n" nick))
	    "END:VCARD\n\n")))

(defun org-contacts-export-as-vcard (&optional name file to-buffer)
  "Export all contacts matching NAME as VCard 3.0. It TO-BUFFER is nil, the content is written to FILE or `org-contacts-vcard-file'. If TO-BUFFER is non-nil, the buffer is created and the VCard is written into that buffer."
  (interactive) ; TODO ask for name?
  (let* ((filename (or file org-contacts-vcard-file))
	 (buffer (if to-buffer
		     (get-buffer-create to-buffer)
		     (find-file-noselect filename))))

    (message "Exporting...")

    (set-buffer buffer)
    (let ((inhibit-read-only t)) (erase-buffer))
    (fundamental-mode)
    (org-install-letbind)

    (when (fboundp 'set-buffer-file-coding-system)
      (set-buffer-file-coding-system coding-system-for-write))

    (loop for contact in (org-contacts-filter name)
	 do (insert (org-contacts-vcard-format contact)))

    (if to-buffer
	(current-buffer)
	(progn (save-buffer) (kill-buffer)))))

(defun org-contacts-show-map (&optional name)
  "Show contacts on a map. Requires google-maps-el."
  (interactive)
  (unless (fboundp 'google-maps-static-show)
    (error "org-contacts-show-map requires google-maps-el."))
  (google-maps-static-show
   :markers
   (loop
      for contact in (org-contacts-filter name)
      for addr = (cdr (assoc-string org-contacts-address-property (caddr contact)))
      if addr
      collect (cons (list addr) (list :label (string-to-char (car contact)))))))

(provide 'org-contacts)
