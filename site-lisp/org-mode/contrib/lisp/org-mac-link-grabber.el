;;; org-mac-link-grabber.el --- Grab links and url from various mac
;;; application and insert them as links into org-mode documents
;;
;; Copyright (c) 2010-2012 Free Software Foundation, Inc.
;;
;; Author: Anthony Lander <anthony.lander@gmail.com>
;; Version: 1.0.1
;; Keywords: org, mac, hyperlink
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
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;; Commentary:
;;
;; This code allows you to grab either the current selected items, or
;; the frontmost url in various mac appliations, and insert them as
;; hyperlinks into the current org-mode document at point.
;;
;; This code is heavily based on, and indeed requires,
;; org-mac-message.el written by John Weigley and Christopher
;; Suckling.
;;
;; Detailed comments for each application interface are inlined with
;; the code. Here is a brief overview of how the code interacts with
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
;;
;;
;; Installation:
;;
;; add (require 'org-mac-link-grabber) to your .emacs, and optionally
;; bind a key to activate the link grabber menu, like this:
;;
;; (add-hook 'org-mode-hook (lambda ()
;;   (define-key org-mode-map (kbd "C-c g") 'omlg-grab-link)))
;;
;;
;; Usage:
;;
;; Type C-c g (or whatever key you defined, as above), or type M-x
;; omlg-grab-link RET to activate the link grabber. This will present
;; you with a menu to choose an application from which to grab a link
;; to insert at point. You may also type C-g to abort.
;;
;; Customizing:
;;
;; You may customize which applications appear in the grab menu by
;; customizing the group org-mac-link-grabber. Changes take effect
;; immediately.
;;
;;
;;; Code:

(require 'org)
(require 'org-mac-message)

(defgroup org-mac-link-grabber nil
  "Options concerning grabbing links from external Mac
applications and inserting them in org documents"
  :tag "Org Mac link grabber"
  :group 'org-link)

(defcustom org-mac-grab-Finder-app-p t
  "Enable menu option [F]inder to grab links from the Finder"
  :tag "Grab Finder.app links"
  :group 'org-mac-link-grabber
  :type 'boolean)

(defcustom org-mac-grab-Mail-app-p t
  "Enable menu option [m]ail to grab links from Mail.app"
  :tag "Grab Mail.app links"
  :group 'org-mac-link-grabber
  :type 'boolean)

(defcustom org-mac-grab-Addressbook-app-p t
  "Enable menu option [a]ddressbook to grab links from AddressBook.app"
  :tag "Grab AddressBook.app links"
  :group 'org-mac-link-grabber
  :type 'boolean)

(defcustom org-mac-grab-Safari-app-p t
  "Enable menu option [s]afari to grab links from Safari.app"
  :tag "Grab Safari.app links"
  :group 'org-mac-link-grabber
  :type 'boolean)

(defcustom org-mac-grab-Firefox-app-p t
  "Enable menu option [f]irefox to grab links from Firefox.app"
  :tag "Grab Firefox.app links"
  :group 'org-mac-link-grabber
  :type 'boolean)

(defcustom org-mac-grab-Firefox+Vimperator-p nil
  "Enable menu option [v]imperator to grab links from Firefox.app running the Vimperator plugin"
  :tag "Grab Vimperator/Firefox.app links"
  :group 'org-mac-link-grabber
  :type 'boolean)

(defcustom org-mac-grab-Chrome-app-p t
  "Enable menu option [f]irefox to grab links from Google Chrome.app"
  :tag "Grab Google Chrome.app links"
  :group 'org-mac-link-grabber
  :type 'boolean)

(defcustom org-mac-grab-Together-app-p nil
  "Enable menu option [t]ogether to grab links from Together.app"
  :tag "Grab Together.app links"
  :group 'org-mac-link-grabber
  :type 'boolean)


(defun omlg-grab-link ()
  "Prompt the user for an application to grab a link from, then go grab the link, and insert it at point"
  (interactive)
  (let* ((descriptors `(("F" "inder" org-mac-finder-insert-selected ,org-mac-grab-Finder-app-p)
						("m" "ail" org-mac-message-insert-selected ,org-mac-grab-Mail-app-p)
						("a" "ddressbook" org-mac-addressbook-insert-selected ,org-mac-grab-Addressbook-app-p)
						("s" "afari" org-mac-safari-insert-frontmost-url ,org-mac-grab-Safari-app-p)
						("f" "irefox" org-mac-firefox-insert-frontmost-url ,org-mac-grab-Firefox-app-p)
						("v" "imperator" org-mac-vimperator-insert-frontmost-url ,org-mac-grab-Firefox+Vimperator-p)
						("c" "hrome" org-mac-chrome-insert-frontmost-url ,org-mac-grab-Chrome-app-p)
						("t" "ogether" org-mac-together-insert-selected ,org-mac-grab-Together-app-p)))
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

(defalias 'omgl-grab-link 'omlg-grab-link
  "Renamed, and this alias will be obsolete next revision.")

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

(defun as-mac-firefox-get-frontmost-url ()
  (let ((result (do-applescript
					(concat
					 "set oldClipboard to the clipboard\n"
					 "set frontmostApplication to path to frontmost application\n"
					 "tell application \"Firefox\"\n"
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
	(car (split-string result "[\r\n]+" t))))

(defun org-mac-firefox-get-frontmost-url ()
  (interactive)
  (message "Applescript: Getting Firefox url...")
  (let* ((url-and-title (as-mac-firefox-get-frontmost-url))
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

(defun as-mac-vimperator-get-frontmost-url ()
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
  (let* ((url-and-title (as-mac-vimperator-get-frontmost-url))
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
;; Grab the frontmost url from Google Chrome. Same limitations are
;; Firefox because Chrome doesn't publish an Applescript dictionary

(defun as-mac-chrome-get-frontmost-url ()
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
	(car (split-string result "[\r\n]+" t))))

(defun org-mac-chrome-get-frontmost-url ()
  (interactive)
  (message "Applescript: Getting Chrome url...")
  (let* ((url-and-title (as-mac-chrome-get-frontmost-url))
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

(defun as-mac-safari-get-frontmost-url ()
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
  (let* ((url-and-title (as-mac-safari-get-frontmost-url))
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


(provide 'org-mac-link-grabber)

;;; org-mac-link-grabber.el ends here
