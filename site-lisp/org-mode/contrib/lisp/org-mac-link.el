;;; org-mac-link.el --- Grab links and url from various mac
;; Application and insert them as links into org-mode documents
;;
;; Copyright (c) 2010-2014 Free Software Foundation, Inc.
;;
;; Authors:
;;          Anthony Lander <anthony.lander@gmail.com>
;;          John Wiegley <johnw@gnu.org>
;;          Christopher Suckling <suckling at gmail dot com>
;;          Daniil Frumin <difrumin@gmail.com>
;;
;;
;; Version: 1.1
;; Keywords: org, mac, hyperlink
;;
;; Version: 1.2
;; Keywords: outlook
;; Author: Mike McLean <mike.mclean@pobox.com>
;; Add support for Microsoft Outlook for Mac as Org mode links
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This code allows you to grab either the current selected items, or
;; the frontmost url in various mac appliations, and insert them as
;; hyperlinks into the current org-mode document at point.
;;
;; This code is heavily based on, and indeed incorporates,
;; org-mac-message.el written by John Wiegley and Christopher
;; Suckling.
;;
;; Detailed comments for each application interface are inlined with
;; the code.  Here is a brief overview of how the code interacts with
;; each application:
;;
;; Finder.app - grab links to the selected files in the frontmost window
;; Mail.app - grab links to the selected messages in the message list
;; AddressBook.app - Grab links to the selected addressbook Cards
;; Firefox.app - Grab the url of the frontmost tab in the frontmost window
;; Vimperator/Firefox.app - Grab the url of the frontmost tab in the frontmost window
;; Safari.app - Grab the url of the frontmost tab in the frontmost window
;; Google Chrome.app - Grab the url of the frontmost tab in the frontmost window
;; Together.app - Grab links to the selected items in the library list
;; Skim.app - Grab a link to the selected page in the topmost pdf document
;; Microsoft Outlook.app - Grab a link to the selected message in the message list
;;
;;
;; Installation:
;;
;; add (require 'org-mac-link) to your .emacs, and optionally bind a
;; key to activate the link grabber menu, like this:
;;
;; (add-hook 'org-mode-hook (lambda ()
;;   (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))
;;
;; Usage:
;;
;; Type C-c g (or whatever key you defined, as above), or type M-x
;; org-mac-grab-link RET to activate the link grabber.  This will present
;; you with a menu to choose an application from which to grab a link
;; to insert at point.  You may also type C-g to abort.
;;
;; Customizing:
;;
;; You may customize which applications appear in the grab menu by
;; customizing the group `org-mac-link'.  Changes take effect
;; immediately.
;;
;;
;;; Code:

(require 'org)

(defgroup org-mac-link nil
  "Options concerning grabbing links from external Mac
applications and inserting them in org documents"
  :tag "Org Mac link"
  :group 'org-link)

(defcustom org-mac-grab-Finder-app-p t
  "Enable menu option [F]inder to grab links from the Finder"
  :tag "Grab Finder.app links"
  :group 'org-mac-link
  :type 'boolean)

(defcustom org-mac-grab-Mail-app-p t
  "Enable menu option [m]ail to grab links from Mail.app"
  :tag "Grab Mail.app links"
  :group 'org-mac-link
  :type 'boolean)

(defcustom org-mac-grab-Outlook-app-p t
  "Enable menu option [o]utlook to grab links from Microsoft Outlook.app"
  :tag "Grab Microsoft Outlook.app links"
  :group 'org-mac-link
  :type 'boolean)

(defcustom org-mac-grab-Addressbook-app-p t
  "Enable menu option [a]ddressbook to grab links from AddressBook.app"
  :tag "Grab AddressBook.app links"
  :group 'org-mac-link
  :type 'boolean)

(defcustom org-mac-grab-Safari-app-p t
  "Enable menu option [s]afari to grab links from Safari.app"
  :tag "Grab Safari.app links"
  :group 'org-mac-link
  :type 'boolean)

(defcustom org-mac-grab-Firefox-app-p t
  "Enable menu option [f]irefox to grab links from Firefox.app"
  :tag "Grab Firefox.app links"
  :group 'org-mac-link
  :type 'boolean)

(defcustom org-mac-grab-Firefox+Vimperator-p nil
  "Enable menu option [v]imperator to grab links from Firefox.app running the Vimperator plugin"
  :tag "Grab Vimperator/Firefox.app links"
  :group 'org-mac-link
  :type 'boolean)

(defcustom org-mac-grab-Chrome-app-p t
  "Enable menu option [f]irefox to grab links from Google Chrome.app"
  :tag "Grab Google Chrome.app links"
  :group 'org-mac-link
  :type 'boolean)

(defcustom org-mac-grab-Together-app-p nil
  "Enable menu option [t]ogether to grab links from Together.app"
  :tag "Grab Together.app links"
  :group 'org-mac-link
  :type 'boolean)

(defcustom org-mac-grab-Skim-app-p
  (< 0 (length (shell-command-to-string
                "mdfind kMDItemCFBundleIdentifier == 'net.sourceforge.skim-app.skim'")))
  "Enable menu option [S]kim to grab page links from Skim.app"
  :tag "Grab Skim.app page links"
  :group 'org-mac-link
  :type 'boolean)

(defcustom org-mac-Skim-highlight-selection-p nil
  "Highlight (using notes) the selection (if present) when grabbing the a link from Skim.app"
  :tag "Highlight selection in Skim.app"
  :group 'org-mac-link
  :type 'boolean)

(defgroup org-mac-flagged-mail nil
  "Options concerning linking to flagged Mail.app messages."
  :tag "Org Mail.app"
  :group 'org-link)

(defcustom org-mac-mail-account "customize"
  "The Mail.app account in which to search for flagged messages."
  :group 'org-mac-flagged-mail
  :type 'string)


;; In mac.c, removed in Emacs 23.
(declare-function do-applescript "org-mac-message" (script))
(unless (fboundp 'do-applescript)
  ;; Need to fake this using shell-command-to-string
  (defun do-applescript (script)
    (let (start cmd return)
      (while (string-match "\n" script)
        (setq script (replace-match "\r" t t script)))
      (while (string-match "'" script start)
        (setq start (+ 2 (match-beginning 0))
              script (replace-match "\\'" t t script)))
      (setq cmd (concat "osascript -e '" script "'"))
      (setq return (shell-command-to-string cmd))
      (concat "\"" (org-trim return) "\""))))


(defun org-mac-grab-link ()
  "Prompt the user for an application to grab a link from, then go grab the link, and insert it at point"
  (interactive)
  (let* ((descriptors `(("F" "inder" org-mac-finder-insert-selected ,org-mac-grab-Finder-app-p)
                        ("m" "ail" org-mac-message-insert-selected ,org-mac-grab-Mail-app-p)
                        ("o" "utlook" org-mac-outlook-message-insert-selected ,org-mac-grab-Outlook-app-p)
                        ("a" "ddressbook" org-mac-addressbook-insert-selected ,org-mac-grab-Addressbook-app-p)
                        ("s" "afari" org-mac-safari-insert-frontmost-url ,org-mac-grab-Safari-app-p)
                        ("f" "irefox" org-mac-firefox-insert-frontmost-url ,org-mac-grab-Firefox-app-p)
                        ("v" "imperator" org-mac-vimperator-insert-frontmost-url ,org-mac-grab-Firefox+Vimperator-p)
                        ("c" "hrome" org-mac-chrome-insert-frontmost-url ,org-mac-grab-Chrome-app-p)
                        ("t" "ogether" org-mac-together-insert-selected ,org-mac-grab-Together-app-p)
                        ("S" "kim" org-mac-skim-insert-page ,org-mac-grab-Skim-app-p)))
         (menu-string (make-string 0 ?x))
         input)

    ;; Create the menu string for the keymap
    (mapc '(lambda (descriptor)
            (when (elt descriptor 3)
              (setf menu-string (concat menu-string "[" (elt descriptor 0) "]" (elt descriptor 1) " "))))
          descriptors)
    (setf (elt menu-string (- (length menu-string) 1)) ?:)

    ;; Prompt the user, and grab the link
    (message menu-string)
    (setq input (read-char-exclusive))
    (mapc '(lambda (descriptor)
            (let ((key (elt (elt descriptor 0) 0))
                  (active (elt descriptor 3))
                  (grab-function (elt descriptor 2)))
              (when (and active (eq input key))
                (call-interactively grab-function))))
          descriptors)))

(defun org-mac-paste-applescript-links (as-link-list)
  "Paste in a list of links from an applescript handler. The
   links are of the form <link>::split::<name>"
  (let* ((link-list
          (mapcar
           (lambda (x) (if (string-match "\\`\"\\(.*\\)\"\\'" x) (setq x (match-string 1 x))) x)
           (split-string as-link-list "[\r\n]+")))
         split-link URL description orglink orglink-insert rtn orglink-list)
    (while link-list
      (setq split-link (split-string (pop link-list) "::split::"))
      (setq URL (car split-link))
      (setq description (cadr split-link))
      (when (not (string= URL ""))
        (setq orglink (org-make-link-string URL description))
        (push orglink orglink-list)))
    (setq rtn (mapconcat 'identity orglink-list "\n"))
    (kill-new rtn)
    rtn))



;; Handle links from Firefox.app
;;
;; This code allows you to grab the current active url from the main
;; Firefox.app window, and insert it as a link into an org-mode
;; document. Unfortunately, firefox does not expose an applescript
;; dictionary, so this is necessarily introduces some limitations.
;;
;; The applescript to grab the url from Firefox.app uses the System
;; Events application to give focus to the firefox application, select
;; the contents of the url bar, and copy it. It then uses the title of
;; the window as the text of the link. There is no way to grab links
;; from other open tabs, and further, if there is more than one window
;; open, it is not clear which one will be used (though emperically it
;; seems that it is always the last active window).

(defun org-as-mac-firefox-get-frontmost-url ()
  (let ((result (do-applescript
                    (concat
                     "set oldClipboard to the clipboard\n"
                     "set frontmostApplication to path to frontmost application\n"
                     "tell application \"Firefox\"\n"
                     "	activate\n"
                     "	delay 0.15\n"
                     "	tell application \"System Events\"\n"
                     "		keystroke \"l\" using {command down}\n"
                     "		keystroke \"a\" using {command down}\n"
                     "		keystroke \"c\" using {command down}\n"
                     "	end tell\n"
                     "	delay 0.15\n"
                     "	set theUrl to the clipboard\n"
                     "	set the clipboard to oldClipboard\n"
                     "	set theResult to (get theUrl) & \"::split::\" & (get name of window 1)\n"
                     "end tell\n"
                     "activate application (frontmostApplication as text)\n"
                     "set links to {}\n"
                     "copy theResult to the end of links\n"
                     "return links as string\n"))))
    (car (split-string result "[\r\n]+" t))))

(defun org-mac-firefox-get-frontmost-url ()
  (interactive)
  (message "Applescript: Getting Firefox url...")
  (let* ((url-and-title (org-as-mac-firefox-get-frontmost-url))
         (split-link (split-string url-and-title "::split::"))
         (URL (car split-link))
         (description (cadr split-link))
         (org-link))
    (when (not (string= URL ""))
      (setq org-link (org-make-link-string URL description)))
    (kill-new org-link)
    org-link))

(defun org-mac-firefox-insert-frontmost-url ()
  (interactive)
  (insert (org-mac-firefox-get-frontmost-url)))


;; Handle links from Google Firefox.app running the Vimperator extension
;; Grab the frontmost url from Firefox+Vimperator. Same limitations are
;; Firefox

(defun org-as-mac-vimperator-get-frontmost-url ()
  (let ((result (do-applescript
                    (concat
                     "set oldClipboard to the clipboard\n"
                     "set frontmostApplication to path to frontmost application\n"
                     "tell application \"Firefox\"\n"
                     "	activate\n"
                     "	delay 0.15\n"
                     "	tell application \"System Events\"\n"
                     "		keystroke \"y\"\n"
                     "	end tell\n"
                     "	delay 0.15\n"
                     "	set theUrl to the clipboard\n"
                     "	set the clipboard to oldClipboard\n"
                     "	set theResult to (get theUrl) & \"::split::\" & (get name of window 1)\n"
                     "end tell\n"
                     "activate application (frontmostApplication as text)\n"
                     "set links to {}\n"
                     "copy theResult to the end of links\n"
                     "return links as string\n"))))
    (replace-regexp-in-string "\s+-\s+Vimperator" "" (car (split-string result "[\r\n]+" t)))))


(defun org-mac-vimperator-get-frontmost-url ()
  (interactive)
  (message "Applescript: Getting Vimperator url...")
  (let* ((url-and-title (org-as-mac-vimperator-get-frontmost-url))
         (split-link (split-string url-and-title "::split::"))
         (URL (car split-link))
         (description (cadr split-link))
         (org-link))
    (when (not (string= URL ""))
      (setq org-link (org-make-link-string URL description)))
    (kill-new org-link)
    org-link))

(defun org-mac-vimperator-insert-frontmost-url ()
  (interactive)
  (insert (org-mac-vimperator-get-frontmost-url)))


;; Handle links from Google Chrome.app
;; Grab the frontmost url from Google Chrome. Same limitations as
;; Firefox because Chrome doesn't publish an Applescript dictionary

(defun org-as-mac-chrome-get-frontmost-url ()
  (let ((result (do-applescript
                    (concat
                     "set oldClipboard to the clipboard\n"
                     "set frontmostApplication to path to frontmost application\n"
                     "tell application \"Google Chrome\"\n"
                     "	activate\n"
                     "	delay 0.15\n"
                     "	tell application \"System Events\"\n"
                     "		keystroke \"l\" using command down\n"
                     "		keystroke \"c\" using command down\n"
                     "	end tell\n"
                     "	delay 0.15\n"
                     "	set theUrl to the clipboard\n"
                     "	set the clipboard to oldClipboard\n"
                     "	set theResult to (get theUrl) & \"::split::\" & (get name of window 1)\n"
                     "end tell\n"
                     "activate application (frontmostApplication as text)\n"
                     "set links to {}\n"
                     "copy theResult to the end of links\n"
                     "return links as string\n"))))
    (substring (car (split-string result "[\r\n]+" t)) 1 -1)))

(defun org-mac-chrome-get-frontmost-url ()
  (interactive)
  (message "Applescript: Getting Chrome url...")
  (let* ((url-and-title (org-as-mac-chrome-get-frontmost-url))
         (split-link (split-string url-and-title "::split::"))
         (URL (car split-link))
         (description (cadr split-link))
         (org-link))
    (when (not (string= URL ""))
      (setq org-link (org-make-link-string URL description)))
    (kill-new org-link)
    org-link))

(defun org-mac-chrome-insert-frontmost-url ()
  (interactive)
  (insert (org-mac-chrome-get-frontmost-url)))


;; Handle links from Safari.app
;; Grab the frontmost url from Safari.

(defun org-as-mac-safari-get-frontmost-url ()
  (let ((result (do-applescript
                    (concat
                     "tell application \"Safari\"\n"
                     "	set theUrl to URL of document 1\n"
                     "	set theName to the name of the document 1\n"
                     "	return theUrl & \"::split::\" & theName & \"\n\"\n"
                     "end tell\n"))))
    (car (split-string result "[\r\n]+" t))))

(defun org-mac-safari-get-frontmost-url ()
  (interactive)
  (message "Applescript: Getting Safari url...")
  (let* ((url-and-title (org-as-mac-safari-get-frontmost-url))
         (split-link (split-string url-and-title "::split::"))
         (URL (car split-link))
         (description (cadr split-link))
         (org-link))
    (when (not (string= URL ""))
      (setq org-link (org-make-link-string URL description)))
    (kill-new org-link)
    org-link))

(defun org-mac-safari-insert-frontmost-url ()
  (interactive)
  (insert (org-mac-safari-get-frontmost-url)))


;;
;;
;; Handle links from together.app
;;
;;

(org-add-link-type "x-together-item" 'org-mac-together-item-open)

(defun org-mac-together-item-open (uid)
  "Open the given uid, which is a reference to an item in Together"
  (shell-command (concat "open -a Together \"x-together-item:" uid "\"")))

(defun as-get-selected-together-items ()
  (do-applescript
      (concat
       "tell application \"Together\"\n"
       "	set theLinkList to {}\n"
       "	set theSelection to selected items\n"
       "	repeat with theItem in theSelection\n"
       "		set theLink to (get item link of theItem) & \"::split::\" & (get name of theItem) & \"\n\"\n"
       "		copy theLink to end of theLinkList\n"
       "	end repeat\n"
       "	return theLinkList as string\n"
       "end tell")))

(defun org-mac-together-get-selected ()
  (interactive)
  (message "Applescript: Getting Togther items...")
  (org-mac-paste-applescript-links (as-get-selected-together-items)))

(defun org-mac-together-insert-selected ()
  (interactive)
  (insert (org-mac-together-get-selected)))


;;
;;
;; Handle links from Finder.app
;;
;;

(defun as-get-selected-finder-items ()
  (do-applescript
      (concat
       "tell application \"Finder\"\n"
       " set theSelection to the selection\n"
       " set links to {}\n"
       " repeat with theItem in theSelection\n"
       " set theLink to \"file://\" & (POSIX path of (theItem as string)) & \"::split::\" & (get the name of theItem) & \"\n\"\n"
       " copy theLink to the end of links\n"
       " end repeat\n"
       " return links as string\n"
       "end tell\n")))

(defun org-mac-finder-item-get-selected ()
  (interactive)
  (message "Applescript: Getting Finder items...")
  (org-mac-paste-applescript-links (as-get-selected-finder-items)))

(defun org-mac-finder-insert-selected ()
  (interactive)
  (insert (org-mac-finder-item-get-selected)))


;;
;;
;; Handle links from AddressBook.app
;;
;;

(org-add-link-type "addressbook" 'org-mac-addressbook-item-open)

(defun org-mac-addressbook-item-open (uid)
  "Open the given uid, which is a reference to an item in Together"
  (shell-command (concat "open \"addressbook:" uid "\"")))

(defun as-get-selected-addressbook-items ()
  (do-applescript
      (concat
       "tell application \"Address Book\"\n"
       "	set theSelection to the selection\n"
       "	set links to {}\n"
       "	repeat with theItem in theSelection\n"
       "		set theLink to \"addressbook://\" & (the id of theItem) & \"::split::\" & (the name of theItem) & \"\n\"\n"
       "		copy theLink to the end of links\n"
       "	end repeat\n"
       "	return links as string\n"
       "end tell\n")))

(defun org-mac-addressbook-item-get-selected ()
  (interactive)
  (message "Applescript: Getting Address Book items...")
  (org-mac-paste-applescript-links (as-get-selected-addressbook-items)))

(defun org-mac-addressbook-insert-selected ()
  (interactive)
  (insert (org-mac-addressbook-item-get-selected)))

;;
;;
;; Handle links from Skim.app
;;
;; Original code & idea by Christopher Suckling (org-mac-protocol)

(org-add-link-type "skim" 'org-mac-skim-open)

(defun org-mac-skim-open (uri)
  "Visit page of pdf in Skim"
  (let* ((page (when (string-match "::\\(.+\\)\\'" uri)
                 (match-string 1 uri)))
         (document (substring uri 0 (match-beginning 0))))
    (do-applescript
        (concat
         "tell application \"Skim\"\n"
         "activate\n"
         "set theDoc to \"" document "\"\n"
         "set thePage to " page "\n"
         "open theDoc\n"
         "go document 1 to page thePage of document 1\n"
         "end tell"))))


(defun as-get-skim-page-link ()
  (do-applescript
      (concat
       "tell application \"Skim\"\n"
       "set theDoc to front document\n"
       "set theTitle to (name of theDoc)\n"
       "set thePath to (path of theDoc)\n"
       "set thePage to (get index for current page of theDoc)\n"
       "set theSelection to selection of theDoc\n"
       "set theContent to contents of (get text for theSelection)\n"
       "if theContent is missing value then\n"
       "    set theContent to theTitle & \", p. \" & thePage\n"
       (when org-mac-Skim-highlight-selection-p
         (concat
          "else\n"
          "    tell theDoc\n"
          "        set theNote to make note with properties {type:highlight note, selection:theSelection}\n"
          "         set text of theNote to (get text for theSelection)\n"
          "    end tell\n"))
       "end if\n"
       "set theLink to \"skim://\" & thePath & \"::\" & thePage & "
       "\"::split::\" & theContent\n"
       "end tell\n"
       "return theLink as string\n")))

(defun org-mac-skim-get-page ()
  (interactive)
  (message "Applescript: Getting Skim page link...")
  (let* ((link-and-descr (as-get-skim-page-link))
         (split-link (split-string link-and-descr "::split::"))
         (link (car split-link))
         (description (cadr split-link))
         (org-link))
    (when (not (string= link ""))
      (setq org-link (org-make-link-string link description)))
    (kill-new org-link)
    org-link))

(defun org-mac-skim-insert-page ()
  (interactive)
  (insert (org-mac-skim-get-page)))



;;
;;
;; Handle links from Microsoft Outlook.app
;;

(org-add-link-type "mac-outlook" 'org-mac-outlook-message-open)

(defun org-mac-outlook-message-open (msgid)
  "Open a message in outlook"
  (let* ((record-id-string (format "mdfind com_microsoft_outlook_recordID==%s" msgid))
	(found-message (replace-regexp-in-string "\n$" ""
              (shell-command-to-string record-id-string))))
    (if (string= found-message "")
      (message "org-mac-link: error could not find Outlook message %s" 	(substring-no-properties msgid))
      (shell-command (format "open \"`mdfind com_microsoft_outlook_recordID==%s`\"" msgid)))))

(defun org-as-get-selected-outlook-mail ()
  "AppleScript to create links to selected messages in Microsoft Outlook.app."
  (do-applescript
   (concat
    "tell application \"Microsoft Outlook\"\n"
    "set msgCount to count current messages\n"
    "if (msgCount < 1) then\n"
    "return\n"
    "end if\n"
    "set theLinkList to {}\n"
    "set theSelection to (get current messages)\n"
    "repeat with theMessage in theSelection\n"
    "set theID to id of theMessage as string\n"
    "set theURL to \"mac-outlook:\" & theID\n"
    "set theSubject to subject of theMessage\n"
    "set theLink to theURL & \"::split::\" & theSubject & \"\n\"\n"
    "copy theLink to end of theLinkList\n"
    "end repeat\n"
    "return theLinkList as string\n"
    "end tell")))

(defun org-sh-get-flagged-outlook-mail ()
  "Shell commands to create links to flagged messages in Microsoft Outlook.app."
  (mapconcat
   (lambda (x) ""
     (concat
      "mac-outlook:"
      (mapconcat
       (lambda (y) "" y)
       (split-string
	(shell-command-to-string
	 (format "mdls -raw -name com_microsoft_outlook_recordID -name kMDItemDisplayName \"%s\"" x))
	"\000")
       "::split::")
      "\n"))
   (with-temp-buffer
     (let ((coding-system-for-read (or file-name-coding-system 'utf-8))
	   (coding-system-for-write 'utf-8))
       (shell-command
	"mdfind com_microsoft_outlook_flagged==1"
	(current-buffer)))
     (split-string
      (buffer-string) "\n" t))
   ""))

(defun org-mac-outlook-message-get-links (&optional select-or-flag)
  "Create links to the messages currently selected or flagged in Microsoft Outlook.app.
This will use AppleScript to get the message-id and the subject of the
messages in Microsoft Outlook.app and make a link out of it.
When SELECT-OR-FLAG is \"s\", get the selected messages (this is also
the default).  When SELECT-OR-FLAG is \"f\", get the flagged messages.
The Org-syntax text will be pushed to the kill ring, and also returned."
  (interactive "sLink to (s)elected or (f)lagged messages: ")
  (setq select-or-flag (or select-or-flag "s"))
  (message "Org Mac Outlook: searching mailboxes...")
  (let* ((as-link-list
          (if (string= select-or-flag "s")
              (org-as-get-selected-outlook-mail)
	    (if (string= select-or-flag "f")
		(org-sh-get-flagged-outlook-mail)
	      (error "Please select \"s\" or \"f\""))))
         (link-list
          (mapcar
           (lambda (x) (if (string-match "\\`\"\\(.*\\)\"\\'" x) (setq x (match-string 1 x))) x)
           (split-string as-link-list "[\r\n]+")))
         split-link URL description orglink orglink-insert rtn orglink-list)
    (while link-list
      (setq split-link (split-string (pop link-list) "::split::"))
      (setq URL (car split-link))
      (setq description (cadr split-link))
      (when (not (string= URL ""))
        (setq orglink (org-make-link-string URL description))
        (push orglink orglink-list)))
    (setq rtn (mapconcat 'identity orglink-list "\n"))
    (kill-new rtn)
    rtn))

(defun org-mac-outlook-message-insert-selected ()
  "Insert a link to the messages currently selected in Microsoft Outlook.app.
This will use AppleScript to get the message-id and the subject of the
active mail in Microsoft Outlook.app and make a link out of it."
  (interactive)
  (insert (org-mac-outlook-message-get-links "s")))

(defun org-mac-outlook-message-insert-flagged (org-buffer org-heading)
  "Asks for an org buffer and a heading within it, and replace message links.
If heading exists, delete all mac-outlook:// links within heading's first
level.  If heading doesn't exist, create it at point-max.  Insert
list of mac-outlook:// links to flagged mail after heading."
  (interactive "bBuffer in which to insert links: \nsHeading after which to insert links: ")
  (with-current-buffer org-buffer
    (goto-char (point-min))
    (let ((isearch-forward t)
          (message-re "\\[\\[\\(mac-outlook:\\)\\([^]]+\\)\\]\\(\\[\\([^]]+\\)\\]\\)?\\]"))
      (if (org-goto-local-search-headings org-heading nil t)
          (if (not (eobp))
              (progn
                (save-excursion
                  (while (re-search-forward
                          message-re (save-excursion (outline-next-heading)) t)
                    (delete-region (match-beginning 0) (match-end 0)))
                  (insert "\n" (org-mac-outlook-message-get-links "f")))
                (flush-lines "^$" (point) (outline-next-heading)))
	    (insert "\n" (org-mac-outlook-message-get-links "f")))
	(goto-char (point-max))
	(insert "\n")
	(org-insert-heading nil t)
	(insert org-heading "\n" (org-mac-outlook-message-get-links "f"))))))



;;
;;
;; Handle links from Mail.app
;;

(org-add-link-type "message" 'org-mac-message-open)

(defun org-mac-message-open (message-id)
  "Visit the message with the given MESSAGE-ID.
This will use the command `open' with the message URL."
  (start-process (concat "open message:" message-id) nil
                 "open" (concat "message://<" (substring message-id 2) ">")))

(defun org-as-get-selected-mail ()
  "AppleScript to create links to selected messages in Mail.app."
  (do-applescript
      (concat
       "tell application \"Mail\"\n"
       "set theLinkList to {}\n"
       "set theSelection to selection\n"
       "repeat with theMessage in theSelection\n"
       "set theID to message id of theMessage\n"
       "set theSubject to subject of theMessage\n"
       "set theLink to \"message://\" & theID & \"::split::\" & theSubject & \"\n\"\n"
       "copy theLink to end of theLinkList\n"
       "end repeat\n"
       "return theLinkList as string\n"
       "end tell")))

(defun org-as-get-flagged-mail ()
  "AppleScript to create links to flagged messages in Mail.app."
  (do-applescript
      (concat
       ;; Is Growl installed?
       "tell application \"System Events\"\n"
       "set growlHelpers to the name of every process whose creator type contains \"GRRR\"\n"
       "if (count of growlHelpers) > 0 then\n"
       "set growlHelperApp to item 1 of growlHelpers\n"
       "else\n"
       "set growlHelperApp to \"\"\n"
       "end if\n"
       "end tell\n"

       ;; Get links
       "tell application \"Mail\"\n"
       "set theMailboxes to every mailbox of account \"" org-mac-mail-account "\"\n"
       "set theLinkList to {}\n"
       "repeat with aMailbox in theMailboxes\n"
       "set theSelection to (every message in aMailbox whose flagged status = true)\n"
       "repeat with theMessage in theSelection\n"
       "set theID to message id of theMessage\n"
       "set theSubject to subject of theMessage\n"
       "set theLink to \"message://\" & theID & \"::split::\" & theSubject & \"\n\"\n"
       "copy theLink to end of theLinkList\n"

       ;; Report progress through Growl
       ;; This "double tell" idiom is described in detail at
       ;; http://macscripter.net/viewtopic.php?id=24570 The
       ;; script compiler needs static knowledge of the
       ;; growlHelperApp.  Hmm, since we're compiling
       ;; on-the-fly here, this is likely to be way less
       ;; portable than I'd hoped.  It'll work when the name
       ;; is still "GrowlHelperApp", though.
       "if growlHelperApp is not \"\" then\n"
       "tell application \"GrowlHelperApp\"\n"
       "tell application growlHelperApp\n"
       "set the allNotificationsList to {\"FlaggedMail\"}\n"
       "set the enabledNotificationsList to allNotificationsList\n"
       "register as application \"FlaggedMail\" all notifications allNotificationsList default notifications enabledNotificationsList icon of application \"Mail\"\n"
       "notify with name \"FlaggedMail\" title \"Importing flagged message\" description theSubject application name \"FlaggedMail\"\n"
       "end tell\n"
       "end tell\n"
       "end if\n"
       "end repeat\n"
       "end repeat\n"
       "return theLinkList as string\n"
       "end tell")))

(defun org-mac-message-get-links (&optional select-or-flag)
  "Create links to the messages currently selected or flagged in Mail.app.
This will use AppleScript to get the message-id and the subject of the
messages in Mail.app and make a link out of it.
When SELECT-OR-FLAG is \"s\", get the selected messages (this is also
the default).  When SELECT-OR-FLAG is \"f\", get the flagged messages.
The Org-syntax text will be pushed to the kill ring, and also returned."
  (interactive "sLink to (s)elected or (f)lagged messages: ")
  (setq select-or-flag (or select-or-flag "s"))
  (message "AppleScript: searching mailboxes...")
  (let* ((as-link-list
          (if (string= select-or-flag "s")
              (org-as-get-selected-mail)
              (if (string= select-or-flag "f")
                  (org-as-get-flagged-mail)
                  (error "Please select \"s\" or \"f\""))))
         (link-list
          (mapcar
           (lambda (x) (if (string-match "\\`\"\\(.*\\)\"\\'" x) (setq x (match-string 1 x))) x)
           (split-string as-link-list "[\r\n]+")))
         split-link URL description orglink orglink-insert rtn orglink-list)
    (while link-list
      (setq split-link (split-string (pop link-list) "::split::"))
      (setq URL (car split-link))
      (setq description (cadr split-link))
      (when (not (string= URL ""))
        (setq orglink (org-make-link-string URL description))
        (push orglink orglink-list)))
    (setq rtn (mapconcat 'identity orglink-list "\n"))
    (kill-new rtn)
    rtn))

(defun org-mac-message-insert-selected ()
  "Insert a link to the messages currently selected in Mail.app.
This will use AppleScript to get the message-id and the subject of the
active mail in Mail.app and make a link out of it."
  (interactive)
  (insert (org-mac-message-get-links "s")))

;; The following line is for backward compatibility
(defalias 'org-mac-message-insert-link 'org-mac-message-insert-selected)

(defun org-mac-message-insert-flagged (org-buffer org-heading)
  "Asks for an org buffer and a heading within it, and replace message links.
If heading exists, delete all message:// links within heading's first
level.  If heading doesn't exist, create it at point-max.  Insert
list of message:// links to flagged mail after heading."
  (interactive "bBuffer in which to insert links: \nsHeading after which to insert links: ")
  (with-current-buffer org-buffer
    (goto-char (point-min))
    (let ((isearch-forward t)
          (message-re "\\[\\[\\(message:\\)\\([^]]+\\)\\]\\(\\[\\([^]]+\\)\\]\\)?\\]"))
      (if (org-goto-local-search-headings org-heading nil t)
          (if (not (eobp))
              (progn
                (save-excursion
                  (while (re-search-forward
                          message-re (save-excursion (outline-next-heading)) t)
                    (delete-region (match-beginning 0) (match-end 0)))
                  (insert "\n" (org-mac-message-get-links "f")))
                (flush-lines "^$" (point) (outline-next-heading)))
              (insert "\n" (org-mac-message-get-links "f")))
          (goto-char (point-max))
          (insert "\n")
          (org-insert-heading nil t)
          (insert org-heading "\n" (org-mac-message-get-links "f"))))))


(provide 'org-mac-link)

;;; org-mac-link.el ends here
