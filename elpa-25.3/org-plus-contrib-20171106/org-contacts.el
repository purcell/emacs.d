;;; org-contacts.el --- Contacts management

;; Copyright (C) 2010-2014 Julien Danjou <julien@danjou.info>

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: outlines, hypermedia, calendar
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the code for managing your contacts into Org-mode.

;; To enter new contacts, you can use `org-capture' and a minimal template just like
;; this:

;;         ("c" "Contacts" entry (file "~/Org/contacts.org")
;;          "* %(org-contacts-template-name)
;; :PROPERTIES:
;; :EMAIL: %(org-contacts-template-email)
;; :END:")))
;;
;; You can also use a complex template, for example:
;;
;;         ("c" "Contacts" entry (file "~/Org/contacts.org")
;;          "* %(org-contacts-template-name)
;; :PROPERTIES:
;; :EMAIL: %(org-contacts-template-email)
;; :PHONE:
;; :ALIAS:
;; :NICKNAME:
;; :IGNORE:
;; :ICON:
;; :NOTE:
;; :ADDRESS:
;; :BIRTHDAY:
;; :END:")))
;;
;;; Code:

(require 'cl-lib)
(require 'org)
(require 'gnus-util)
(require 'gnus-art)
(require 'mail-utils)
(require 'org-agenda)
(require 'org-capture)

(defgroup org-contacts nil
  "Options about contacts management."
  :group 'org)

(defcustom org-contacts-files nil
  "List of Org files to use as contacts source.
When set to nil, all your Org files will be used."
  :type '(repeat file)
  :group 'org-contacts)

(defcustom org-contacts-email-property "EMAIL"
  "Name of the property for contact email address."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-tel-property "PHONE"
  "Name of the property for contact phone number."
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

(defcustom org-contacts-note-property "NOTE"
  "Name of the property for contact note."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-alias-property "ALIAS"
  "Name of the property for contact name alias."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-ignore-property "IGNORE"
  "Name of the property, which values will be ignored when
completing or exporting to vcard."
  :type 'string
  :group 'org-contacts)


(defcustom org-contacts-birthday-format "Birthday: %l (%Y)"
  "Format of the anniversary agenda entry.
The following replacements are available:

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

(defcustom org-contacts-tags-props-prefix "#"
  "Tags and properties prefix."
  :type 'string
  :group 'org-contacts)

(defcustom org-contacts-matcher
  (mapconcat #'identity
	     (mapcar (lambda (x) (concat x "<>\"\""))
		     (list org-contacts-email-property
			   org-contacts-alias-property
			   org-contacts-tel-property
			   org-contacts-address-property
			   org-contacts-birthday-property))
	     "|")
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

(defcustom org-contacts-enable-completion t
  "Enable or not the completion in `message-mode' with `org-contacts'."
  :group 'org-contacts
  :type 'boolean)

(defcustom org-contacts-complete-functions
  '(org-contacts-complete-group org-contacts-complete-tags-props org-contacts-complete-name)
  "List of functions used to complete contacts in `message-mode'."
  :group 'org-contacts
  :type 'hook)

;; Decalre external functions and variables
(declare-function org-reverse-string "org")
(declare-function diary-ordinal-suffix "ext:diary-lib")
(declare-function wl-summary-message-number "ext:wl-summary")
(declare-function wl-address-header-extract-address "ext:wl-address")
(declare-function wl-address-header-extract-realname "ext:wl-address")
(declare-function erc-buffer-list "ext:erc")
(declare-function erc-get-channel-user-list "ext:erc")
(declare-function google-maps-static-show "ext:google-maps-static")
(declare-function elmo-message-field "ext:elmo-pipe")
(declare-function std11-narrow-to-header "ext:std11")
(declare-function std11-fetch-field "ext:std11")

(defconst org-contacts-property-values-separators "[,; \f\t\n\r\v]+"
  "The default value of separators for `org-contacts-split-property'.

A regexp matching strings of whitespace, `,' and `;'.")

(defvar org-contacts-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "M" 'org-contacts-view-send-email)
    (define-key map "i" 'org-contacts-view-switch-to-irc-buffer)
    map)
  "The keymap used in `org-contacts' result list.")

(defvar org-contacts-db nil
  "Org Contacts database.")

(defvar org-contacts-last-update nil
  "Last time the Org Contacts database has been updated.")

(defun org-contacts-files ()
  "Return list of Org files to use for contact management."
  (or org-contacts-files (org-agenda-files t 'ifmode)))

(defun org-contacts-db-need-update-p ()
  "Determine whether `org-contacts-db' needs to be refreshed."
  (or (null org-contacts-last-update)
      (cl-find-if (lambda (file)
		     (or (time-less-p org-contacts-last-update
				      (elt (file-attributes file) 5))))
		   (org-contacts-files))
      (org-contacts-db-has-dead-markers-p org-contacts-db)))

(defun org-contacts-db-has-dead-markers-p (org-contacts-db)
  "Returns t if at least one dead marker is found in
ORG-CONTACTS-DB. A dead marker in this case is a marker pointing
to dead or no buffer."
    ;; Scan contacts list looking for dead markers, and return t at first found.
    (catch 'dead-marker-found
      (while org-contacts-db
        (unless (marker-buffer (nth 1 (car org-contacts-db)))
          (throw 'dead-marker-found t))
        (setq org-contacts-db (cdr org-contacts-db)))
      nil))

(defun org-contacts-db ()
  "Return the latest Org Contacts Database."
  (let* ((org--matcher-tags-todo-only nil)
	 (contacts-matcher (cdr (org-make-tags-matcher org-contacts-matcher)))
	 result)
    (when (org-contacts-db-need-update-p)
      (let ((progress-reporter
	     (make-progress-reporter "Updating Org Contacts Database..." 0 (length org-contacts-files)))
	    (i 0))
	(dolist (file (org-contacts-files))
	  (if (catch 'nextfile
                ;; if file doesn't exist and the user agrees to removing it
                ;; from org-agendas-list, 'nextfile is thrown.  Catch it here
                ;; and skip processing the file.
                ;;
                ;; TODO: suppose that the user has set an org-contacts-files
                ;; list that contains an element that doesn't exist in the
                ;; file system: in that case, the org-agenda-files list could
                ;; be updated (and saved to the customizations of the user) if
                ;; it contained the same file even though the org-agenda-files
                ;; list wasn't actually used.  I don't think it is normal that
                ;; org-contacts updates org-agenda-files in this case, but
                ;; short of duplicating org-check-agenda-files and
                ;; org-remove-files, I don't know how to avoid it.
                ;;
                ;; A side effect of the TODO is that the faulty
                ;; org-contacts-files list never gets updated and thus the
                ;; user is always queried about the missing files when
                ;; org-contacts-db-need-update-p returns true.
                (org-check-agenda-file file))
              (message "Skipped %s removed from org-agenda-files list."
                       (abbreviate-file-name file))
	    (with-current-buffer (org-get-agenda-file-buffer file)
	      (unless (eq major-mode 'org-mode)
		(error "File %s is not in `org-mode'" file))
	      (setf result
		    (append result
			    (org-scan-tags 'org-contacts-at-point
					   contacts-matcher
					   org--matcher-tags-todo-only)))))
	  (progress-reporter-update progress-reporter (setq i (1+ i))))
	(setf org-contacts-db result
	      org-contacts-last-update (current-time))
	(progress-reporter-done progress-reporter)))
    org-contacts-db))

(defun org-contacts-at-point (&optional pom)
  "Return the contacts at point-or-marker POM or current position
if nil."
  (setq pom (or pom (point)))
  (org-with-point-at pom
    (list (org-get-heading t) (set-marker (make-marker) pom) (org-entry-properties pom 'all))))

(defun org-contacts-filter (&optional name-match tags-match prop-match)
  "Search for a contact matching any of NAME-MATCH, TAGS-MATCH, PROP-MATCH.
If all match values are nil, return all contacts.

The optional PROP-MATCH argument is a single (PROP . VALUE) cons
cell corresponding to the contact properties.
"
  (if (and (null name-match)
	   (null prop-match)
	   (null tags-match))
      (org-contacts-db)
    (cl-loop for contact in (org-contacts-db)
	     if (or
		 (and name-match
		      (string-match-p name-match
					  (first contact)))
		 (and prop-match
		      (cl-find-if (lambda (prop)
				     (and (string= (car prop-match) (car prop))
					  (string-match-p (cdr prop-match) (cdr prop))))
				   (caddr contact)))
		 (and tags-match
		      (cl-find-if (lambda (tag)
				     (string-match-p tags-match tag))
				   (org-split-string
				    (or (cdr (assoc-string "ALLTAGS" (caddr contact))) "") ":"))))
	     collect contact)))

(when (not (fboundp 'completion-table-case-fold))
  ;; That function is new in Emacs 24...
  (defun completion-table-case-fold (table &optional dont-fold)
    (lambda (string pred action)
      (let ((completion-ignore-case (not dont-fold)))
	(complete-with-action action table string pred)))))

(defun org-contacts-try-completion-prefix (to-match collection &optional predicate)
  "Custom implementation of `try-completion'.
This version works only with list and alist and it looks at all
prefixes rather than just the beginning of the string."
  (cl-loop with regexp = (concat "\\b" (regexp-quote to-match))
	   with ret = nil
	   with ret-start = nil
	   with ret-end = nil

	   for el in collection
	   for string = (if (listp el) (car el) el)

	   for start = (when (or (null predicate) (funcall predicate string))
			 (string-match regexp string))

	   if start
	   do (let ((end (match-end 0))
		    (len (length string)))
		(if (= end len)
		    (cl-return t)
		  (cl-destructuring-bind (string start end)
		      (if (null ret)
			  (values string start end)
			(org-contacts-common-substring
			 ret ret-start ret-end
			 string start end))
		    (setf ret string
			  ret-start start
			  ret-end end))))

	   finally (cl-return
		    (replace-regexp-in-string "\\`[ \t\n]*" "" ret))))

(defun org-contacts-compare-strings (s1 start1 end1 s2 start2 end2 &optional ignore-case)
  "Compare the contents of two strings, using `compare-strings'.

This function works like `compare-strings' excepted that it
returns a cons.
- The CAR is the number of characters that match at the beginning.
- The CDR is T is the two strings are the same and NIL otherwise."
  (let ((ret (compare-strings s1 start1 end1 s2 start2 end2 ignore-case)))
    (if (eq ret t)
	(cons (or end1 (length s1)) t)
      (cons (1- (abs ret)) nil))))

(defun org-contacts-common-substring (s1 start1 end1 s2 start2 end2)
  "Extract the common substring between S1 and S2.

This function extracts the common substring between S1 and S2 and
adjust the part that remains common.

START1 and END1 delimit the part in S1 that we know is common
between the two strings. This applies to START2 and END2 for S2.

This function returns a list whose contains:
- The common substring found.
- The new value of the start of the known inner substring.
- The new value of the end of the known inner substring."
  ;; Given two strings:
  ;; s1: "foo bar baz"
  ;; s2: "fooo bar baz"
  ;; and the inner substring is "bar"
  ;; then: start1 = 4, end1 = 6, start2 = 5, end2 = 7
  ;;
  ;; To find the common substring we will compare two substrings:
  ;; " oof" and " ooof" to find the beginning of the common substring.
  ;; " baz" and " baz" to find the end of the common substring.
  (let* ((len1 (length s1))
	 (start1 (or start1 0))
	 (end1 (or end1 len1))

	 (len2 (length s2))
	 (start2 (or start2 0))
	 (end2 (or end2 len2))

	 (new-start (car (org-contacts-compare-strings
			  (substring (org-reverse-string s1) (- len1 start1)) nil nil
			  (substring (org-reverse-string s2) (- len2 start2)) nil nil)))

	 (new-end (+ end1 (car (org-contacts-compare-strings
				(substring s1 end1) nil nil
				(substring s2 end2) nil nil)))))
    (list (substring s1 (- start1 new-start) new-end)
	  new-start
	  (+ new-start (- end1 start1)))))

(defun org-contacts-all-completions-prefix (to-match collection &optional predicate)
  "Custom version of `all-completions'.
This version works only with list and alist and it looks at all
prefixes rather than just the beginning of the string."
  (cl-loop with regexp = (concat "\\b" (regexp-quote to-match))
	   for el in collection
	   for string = (if (listp el) (car el) el)
	   for match? = (when (and (or (null predicate) (funcall predicate string)))
			  (string-match regexp string))
	   if match?
	   collect (progn
		     (let ((end (match-end 0)))
		       (org-no-properties string)
		       (when (< end (length string))
			 ;; Here we add a text property that will be used
			 ;; later to highlight the character right after
			 ;; the common part between each addresses.
			 ;; See `org-contacts-display-sort-function'.
			 (put-text-property end (1+ end) 'org-contacts-prefix 't string)))
		     string)))

(defun org-contacts-make-collection-prefix (collection)
  "Make a collection function from COLLECTION which will match on prefixes."
  (lexical-let ((collection collection))
    (lambda (string predicate flag)
      (cond ((eq flag nil)
	     (org-contacts-try-completion-prefix string collection predicate))
	    ((eq flag t)
	     ;; `org-contacts-all-completions-prefix' has already been
	     ;; used to compute `all-completions'.
	     collection)
	    ((eq flag 'lambda)
	     (org-contacts-test-completion-prefix string collection predicate))
	    ((and (listp flag) (eq (car flag) 'boundaries))
	     (cl-destructuring-bind (to-ignore &rest suffix)
		 flag
	       (org-contacts-boundaries-prefix string collection predicate suffix)))
	    ((eq flag 'metadata)
	     (org-contacts-metadata-prefix string collection predicate))
	    (t nil			; operation unsupported
	       )))))

(defun org-contacts-display-sort-function (completions)
  "Sort function for contacts display."
  (mapcar (lambda (string)
	    (cl-loop with len = (1- (length string))
		     for i upfrom 0 to len
		     if (memq 'org-contacts-prefix
			      (text-properties-at i string))
		     do (set-text-properties
			 i (1+ i)
			 (list 'font-lock-face
			       (if (char-equal (aref string i)
					       (string-to-char " "))
				   ;; Spaces can't be bold.
				   'underline
				 'bold)) string)
		     else
		     do (set-text-properties i (1+ i) nil string)
		     finally (cl-return string)))
	  completions))

(defun org-contacts-test-completion-prefix (string collection predicate)
  (cl-find-if (lambda (el)
		(and (or (null predicate) (funcall predicate el))
		     (string= string el)))
	      collection))

(defun org-contacts-boundaries-prefix (string collection predicate suffix)
  (list* 'boundaries (completion-boundaries string collection predicate suffix)))

(defun org-contacts-metadata-prefix (string collection predicate)
  '(metadata .
	     ((cycle-sort-function . org-contacts-display-sort-function)
	      (display-sort-function . org-contacts-display-sort-function))))

(defun org-contacts-complete-group (start end string)
  "Complete text at START from a group.

A group FOO is composed of contacts with the tag FOO."
  (let* ((completion-ignore-case org-contacts-completion-ignore-case)
	 (group-completion-p (string-match-p
			      (concat "^" org-contacts-group-prefix) string)))
    (when group-completion-p
      (let ((completion-list
	     (all-completions
	      string
	      (mapcar (lambda (group)
			(propertize (concat org-contacts-group-prefix group)
				    'org-contacts-group group))
		      (org-uniquify
		       (cl-loop for contact in (org-contacts-filter)
				nconc (org-split-string
				       (or (cdr (assoc-string "ALLTAGS" (caddr contact))) "") ":")))))))
	(list start end
	      (if (= (length completion-list) 1)
		  ;; We've found the correct group, returns the address
		  (lexical-let ((tag (get-text-property 0 'org-contacts-group
							(car completion-list))))
		    (lambda (string pred &optional to-ignore)
		      (mapconcat 'identity
				 (cl-loop for contact in (org-contacts-filter
							  nil
							  tag)
					  ;; The contact name is always the car of the assoc-list
					  ;; returned by `org-contacts-filter'.
					  for contact-name = (car contact)
					  ;; Grab the first email of the contact
					  for email = (org-contacts-strip-link
						       (or (car (org-contacts-split-property
								 (or
								  (cdr (assoc-string org-contacts-email-property
										     (cl-caddr contact)))
								  ""))) ""))
					  ;; If the user has an email address, append USER <EMAIL>.
					  if email collect (org-contacts-format-email contact-name email))
				 ", ")))
		;; We haven't found the correct group
		(completion-table-case-fold completion-list
					    (not org-contacts-completion-ignore-case))))))))

(defun org-contacts-complete-tags-props (start end string)
  "Insert emails that match the tags expression.

For example: FOO-BAR will match entries tagged with FOO but not
with BAR.

See (org) Matching tags and properties for a complete
description."
  (let* ((completion-ignore-case org-contacts-completion-ignore-case)
	 (completion-p (string-match-p
			(concat "^" org-contacts-tags-props-prefix) string)))
    (when completion-p
      (let ((result
	     (mapconcat
	      'identity
	      (cl-loop for contact in (org-contacts-db)
		       for contact-name = (car contact)
		       for email = (org-contacts-strip-link (or (car (org-contacts-split-property
								      (or
								       (cdr (assoc-string org-contacts-email-property
											  (cl-caddr contact)))
								       ""))) ""))
		       for tags = (cdr (assoc "TAGS" (nth 2 contact)))
		       for tags-list = (if tags
					   (split-string (substring (cdr (assoc "TAGS" (nth 2 contact))) 1 -1) ":")
					 '())
		       for marker = (nth 1 contact)
		       if (with-current-buffer (marker-buffer marker)
			    (save-excursion
			      (goto-char marker)
			      (let (todo-only)
				(eval (cdr (org-make-tags-matcher (cl-subseq string 1)))))))
		       collect (org-contacts-format-email contact-name email))
	      ",")))
	(when (not (string= "" result))
	  ;; return (start end function)
	  (lexical-let* ((to-return result))
	    (list start end
		  (lambda (string pred &optional to-ignore) to-return))))))))

(defun org-contacts-remove-ignored-property-values (ignore-list list)
  "Remove all ignore-list's elements from list and you can use
   regular expressions in the ignore list."
  (cl-remove-if (lambda (el)
		  (cl-find-if (lambda (x)
				 (string-match-p x el))
			       ignore-list))
		list))

(defun org-contacts-complete-name (start end string)
  "Complete text at START with a user name and email."
  (let* ((completion-ignore-case org-contacts-completion-ignore-case)
         (completion-list
	  (cl-loop for contact in (org-contacts-filter)
		   ;; The contact name is always the car of the assoc-list
		   ;; returned by `org-contacts-filter'.
		   for contact-name = (car contact)

		   ;; Build the list of the email addresses which has
		   ;; been expired
		   for ignore-list = (org-contacts-split-property
				      (or (cdr (assoc-string org-contacts-ignore-property
							     (nth 2 contact))) ""))
		   ;; Build the list of the user email addresses.
		   for email-list = (org-contacts-remove-ignored-property-values
				     ignore-list
				     (org-contacts-split-property
				      (or (cdr (assoc-string org-contacts-email-property
							     (nth 2 contact))) "")))
		   ;; If the user has email addresses…
		   if email-list
		   ;; … append a list of USER <EMAIL>.
		   nconc (cl-loop for email in email-list
				  collect (org-contacts-format-email contact-name (org-contacts-strip-link email)))))
	 (completion-list (org-contacts-all-completions-prefix
			   string
			   (org-uniquify completion-list))))
    (when completion-list
      (list start end
	    (org-contacts-make-collection-prefix completion-list)))))

(defun org-contacts-message-complete-function (&optional start)
  "Function used in `completion-at-point-functions' in `message-mode'."
  ;; Avoid to complete in `post-command-hook'.
  (when completion-in-region-mode
    (remove-hook 'post-command-hook #'completion-in-region--postch))
  (let ((mail-abbrev-mode-regexp
         "^\\(Resent-To\\|To\\|B?Cc\\|Reply-To\\|From\\|Mail-Followup-To\\|Mail-Copies-To\\|Disposition-Notification-To\\|Return-Receipt-To\\):"))
    (when (mail-abbrev-in-expansion-header-p)
      (lexical-let*
	  ((end (point))
	   (start (or start
		      (save-excursion
			(re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
			(goto-char (match-end 0))
			(point))))
	   (string (buffer-substring start end)))
	(run-hook-with-args-until-success
	 'org-contacts-complete-functions start end string)))))

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
    (cl-cadar (or (org-contacts-filter
		   nil
		   nil
		   (cons org-contacts-email-property (concat "\\b" (regexp-quote email) "\\b")))
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
      (when (eq major-mode 'org-mode) (org-show-context 'agenda)))))

(with-no-warnings (defvar date)) ;; unprefixed, from calendar.el
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
    (cl-loop for contact in (org-contacts-filter)
	     for anniv = (let ((anniv (cdr (assoc-string
					    (or field org-contacts-birthday-property)
					    (nth 2 contact)))))
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
  "Return the contact icon as a string."
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
        (org-agenda-prefix-format (propertize
				   "%(org-contacts-icon-as-string)% s%(org-contacts-irc-number-of-unread-messages) "
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

(defun org-contacts-format-name (name)
  "Trim any local formatting to get a bare NAME."
  ;; Remove radio targets characters
  (replace-regexp-in-string org-radio-target-regexp "\\1" name))

(defun org-contacts-format-email (name email)
  "Format an EMAIL address corresponding to NAME."
  (unless email
    (error "`email' cannot be nul"))
  (if name
      (concat (org-contacts-format-name name) " <" email ">")
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
`gnus-article-prepare-hook'.  It also adds a binding on `;' in
`gnus-summary-mode-map' to `org-contacts-gnus-article-from-goto'"
  (require 'gnus)
  (require 'gnus-art)
  (define-key gnus-summary-mode-map ";" 'org-contacts-gnus-article-from-goto)
  (add-hook 'gnus-article-prepare-hook 'org-contacts-gnus-check-mail-address)
  (add-hook 'gnus-article-prepare-hook 'org-contacts-gnus-store-last-mail))

(defun org-contacts-setup-completion-at-point ()
  "Add `org-contacts-message-complete-function' as a new function
to complete the thing at point."
  (add-to-list 'completion-at-point-functions
	       'org-contacts-message-complete-function))

(defun org-contacts-unload-hook ()
  (remove-hook 'message-mode-hook 'org-contacts-setup-completion-at-point))

(when (and org-contacts-enable-completion
	   (boundp 'completion-at-point-functions))
  (add-hook 'message-mode-hook 'org-contacts-setup-completion-at-point))

(defun org-contacts-wl-get-from-header-content ()
  "Retrieve the content of the `From' header of an email.
Works from wl-summary-mode and mime-view-mode - that is while viewing email.
Depends on Wanderlust been loaded."
  (with-current-buffer (org-capture-get :original-buffer)
    (cond
     ((eq major-mode 'wl-summary-mode) (when (and (boundp 'wl-summary-buffer-elmo-folder)
						  wl-summary-buffer-elmo-folder)
                                         (elmo-message-field
                                          wl-summary-buffer-elmo-folder
                                          (wl-summary-message-number)
                                          'from)))
     ((eq major-mode 'mime-view-mode) (std11-narrow-to-header)
      (prog1
	  (std11-fetch-field "From")
	(widen))))))

(defun org-contacts-wl-get-name-email ()
  "Get name and email address from Wanderlust email.
See `org-contacts-wl-get-from-header-content' for limitations."
  (let ((from (org-contacts-wl-get-from-header-content)))
    (when from
      (list (wl-address-header-extract-realname from)
	    (wl-address-header-extract-address from)))))

(defun org-contacts-template-wl-name (&optional return-value)
  "Try to return the contact name for a template from wl.
If not found, return RETURN-VALUE or something that would ask the
user."
  (or (car (org-contacts-wl-get-name-email))
      return-value
      "%^{Name}"))

(defun org-contacts-template-wl-email (&optional return-value)
  "Try to return the contact email for a template from Wanderlust.
If not found return RETURN-VALUE or something that would ask the user."
  (or (cadr (org-contacts-wl-get-name-email))
      return-value
      (concat "%^{" org-contacts-email-property "}p")))

(defun org-contacts-view-send-email (&optional ask)
  "Send email to the contact at point.
If ASK is set, ask for the email address even if there's only one
address."
  (interactive "P")
  (let ((marker (org-get-at-bol 'org-hd-marker)))
    (org-with-point-at marker
      (let ((emails (org-entry-get (point) org-contacts-email-property)))
        (if emails
            (let ((email-list (org-contacts-split-property emails)))
              (if (and (= (length email-list) 1) (not ask))
                  (compose-mail (org-contacts-format-email
                                 (org-get-heading t) emails))
                (let ((email (completing-read "Send mail to which address: " email-list)))
		  (setq email (org-contacts-strip-link email))
                  (org-contacts-check-mail-address email)
                  (compose-mail (org-contacts-format-email (org-get-heading t) email)))))
          (error (format "This contact has no mail address set (no %s property)"
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
                (loop for email in (org-contacts-split-property email-list)
                      for gravatar = (gravatar-retrieve-synchronously (org-contacts-strip-link email))
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
  (loop for buffer in (erc-buffer-list)
	nconc (with-current-buffer buffer
		(loop for user-entry in (mapcar 'car (erc-get-channel-user-list))
		      collect (elt user-entry 1)))))

(add-to-list 'org-property-set-functions-alist
             `(,org-contacts-nickname-property . org-contacts-completing-read-nickname))

(defun org-contacts-vcard-escape (str)
  "Escape ; , and \n in STR for the VCard format."
  ;; Thanks to this library for the regexp:
  ;; http://www.emacswiki.org/cgi-bin/wiki/bbdb-vcard-export.el
  (when str
    (replace-regexp-in-string
     "\n" "\\\\n"
     (replace-regexp-in-string "\\(;\\|,\\|\\\\\\)" "\\\\\\1" str))))

(defun org-contacts-vcard-encode-name (name)
  "Try to encode NAME as VCard's N property.
The N property expects

  FamilyName;GivenName;AdditionalNames;Prefix;Postfix.

Org-contacts does not specify how to encode the name.  So we try
to do our best."
  (concat (replace-regexp-in-string "\\(\\w+\\) \\(.*\\)" "\\2;\\1" name) ";;;"))

(defun org-contacts-vcard-format (contact)
  "Formats CONTACT in VCard 3.0 format."
  (let* ((properties (nth 2 contact))
	 (name (org-contacts-vcard-escape (car contact)))
	 (n (org-contacts-vcard-encode-name name))
	 (email (cdr (assoc-string org-contacts-email-property properties)))
	 (tel (cdr (assoc-string org-contacts-tel-property properties)))
	 (ignore-list (cdr (assoc-string org-contacts-ignore-property properties)))
	 (ignore-list (when ignore-list
			(org-contacts-split-property ignore-list)))
	 (note (cdr (assoc-string org-contacts-note-property properties)))
	 (bday (org-contacts-vcard-escape (cdr (assoc-string org-contacts-birthday-property properties))))
	 (addr (cdr (assoc-string org-contacts-address-property properties)))
	 (nick (org-contacts-vcard-escape (cdr (assoc-string org-contacts-nickname-property properties))))
	 (head (format "BEGIN:VCARD\nVERSION:3.0\nN:%s\nFN:%s\n" n name))
	 emails-list result phones-list)
    (concat head
	    (when email (progn
			  (setq emails-list (org-contacts-remove-ignored-property-values ignore-list (org-contacts-split-property email)))
			  (setq result "")
			  (while emails-list
			    (setq result (concat result  "EMAIL:" (org-contacts-strip-link (car emails-list)) "\n"))
			    (setq emails-list (cdr emails-list)))
			  result))
	    (when addr
	      (format "ADR:;;%s\n" (replace-regexp-in-string "\\, ?" ";" addr)))
	    (when tel (progn
			(setq phones-list (org-contacts-remove-ignored-property-values ignore-list (org-contacts-split-property tel)))
			(setq result "")
			(while phones-list
			  (setq result (concat result  "TEL:" (org-link-unescape (org-contacts-strip-link (car phones-list))) "\n"))
			  (setq phones-list (cdr phones-list)))
			result))
	    (when bday
	      (let ((cal-bday (calendar-gregorian-from-absolute (org-time-string-to-absolute bday))))
		(format "BDAY:%04d-%02d-%02d\n"
			(calendar-extract-year cal-bday)
			(calendar-extract-month cal-bday)
			(calendar-extract-day cal-bday))))
	    (when nick (format "NICKNAME:%s\n" nick))
	    (when note (format "NOTE:%s\n" note))
	    "END:VCARD\n\n")))

(defun org-contacts-export-as-vcard (&optional name file to-buffer)
  "Export org contacts to V-Card 3.0.

By default, all contacts are exported to `org-contacts-vcard-file'.

When NAME is \\[universal-argument], prompts for a contact name.

When NAME is \\[universal-argument] \\[universal-argument],
prompts for a contact name and a file name where to export.

When NAME is \\[universal-argument] \\[universal-argument]
\\[universal-argument], prompts for a contact name and a buffer where to export.

If the function is not called interactively, all parameters are
passed to `org-contacts-export-as-vcard-internal'."
  (interactive "P")
  (when (called-interactively-p 'any)
    (cl-psetf name
	      (when name
		(read-string "Contact name: "
			     (nth 0 (org-contacts-at-point))))
	      file
	      (when (equal name '(16))
		(read-file-name "File: " nil org-contacts-vcard-file))
	      to-buffer
	      (when (equal name '(64))
		(read-buffer "Buffer: "))))
  (org-contacts-export-as-vcard-internal name file to-buffer))

(defun org-contacts-export-as-vcard-internal (&optional name file to-buffer)
  "Export all contacts matching NAME as VCard 3.0.
If TO-BUFFER is nil, the content is written to FILE or
`org-contacts-vcard-file'.  If TO-BUFFER is non-nil, the buffer
is created and the VCard is written into that buffer."
  (let* ((filename (or file org-contacts-vcard-file))
	 (buffer (if to-buffer
		     (get-buffer-create to-buffer)
		   (find-file-noselect filename))))
    (message "Exporting...")
    (set-buffer buffer)
    (let ((inhibit-read-only t)) (erase-buffer))
    (fundamental-mode)
    (when (fboundp 'set-buffer-file-coding-system)
      (set-buffer-file-coding-system coding-system-for-write))
    (loop for contact in (org-contacts-filter name)
	  do (insert (org-contacts-vcard-format contact)))
    (if to-buffer
	(current-buffer)
      (progn (save-buffer) (kill-buffer)))))

(defun org-contacts-show-map (&optional name)
  "Show contacts on a map.
Requires google-maps-el."
  (interactive)
  (unless (fboundp 'google-maps-static-show)
    (error "`org-contacts-show-map' requires `google-maps-el'"))
  (google-maps-static-show
   :markers
   (cl-loop
    for contact in (org-contacts-filter name)
    for addr = (cdr (assoc-string org-contacts-address-property (nth 2 contact)))
    if addr
    collect (cons (list addr) (list :label (string-to-char (car contact)))))))

(defun org-contacts-strip-link (link)
  "Remove brackets, description, link type and colon from an org
link string and return the pure link target."
   (let (startpos colonpos endpos)
     (setq startpos (string-match (regexp-opt '("[[tel:" "[[mailto:")) link))
     (if startpos
         (progn
            (setq colonpos (string-match ":" link))
            (setq endpos (string-match "\\]" link))
            (if endpos (substring link (1+ colonpos) endpos) link))
         (progn
            (setq startpos (string-match "mailto:" link))
            (setq colonpos (string-match ":" link))
            (if startpos (substring link (1+ colonpos)) link)))))

;; Add the link type supported by org-contacts-strip-link
;; so everything is in order for its use in Org files
(org-link-set-parameters "tel")

(defun org-contacts-split-property (string &optional separators omit-nulls)
  "Custom version of `split-string'.
Split a property STRING into sub-strings bounded by matches
for SEPARATORS but keep Org links intact.

The beginning and end of STRING, and each match for SEPARATORS, are
splitting points.  The substrings matching SEPARATORS are removed, and
the substrings between the splitting points are collected as a list,
which is returned.

If SEPARATORS is non-nil, it should be a regular expression
matching text which separates, but is not part of, the
substrings.  If nil it defaults to `org-contacts-property-values-separators',
normally \"[,; \f\t\n\r\v]+\", and OMIT-NULLS is forced to t.

If OMIT-NULLS is t, zero-length substrings are omitted from the list \(so
that for the default value of SEPARATORS leading and trailing whitespace
are effectively trimmed).  If nil, all zero-length substrings are retained."
  (let* ((omit-nulls (if separators omit-nulls t))
	 (rexp (or separators org-contacts-property-values-separators))
	 (inputlist (split-string string rexp omit-nulls))
	 (linkstring "")
	 (bufferstring "")
	 (proplist (list "")))
    (while inputlist
      (setq bufferstring (pop inputlist))
      (if (string-match "\\[\\[" bufferstring)
          (progn
            (setq linkstring (concat bufferstring " "))
            (while (not (string-match "\\]\\]" bufferstring))
              (setq bufferstring (pop inputlist))
              (setq linkstring (concat  linkstring bufferstring " ")))
            (setq proplist (cons (org-trim linkstring) proplist)))
        (setq proplist (cons bufferstring proplist))))
    (cdr (reverse proplist))))

(provide 'org-contacts)

;;; org-contacts.el ends here
