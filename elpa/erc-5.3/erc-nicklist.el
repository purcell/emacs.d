;;; erc-nicklist.el --- Display channel nicknames in a side buffer.

;; Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Filename: erc-nicklist.el
;; Author: Lawrence Mitchell <wence@gmx.li>
;; Created: 2004-04-30
;; Keywords: IRC chat client Internet

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This provides a minimal mIRC style nicklist buffer for ERC.  To
;; activate, do M-x erc-nicklist RET in the channel buffer you want
;; the nicklist to appear for.  To close and quit the nicklist
;; buffer, do M-x erc-nicklist-quit RET from within the nicklist buffer.
;;
;; TODO:
;; o Somehow associate nicklist windows with channel windows so they
;;   appear together, and if one gets buried, then the other does.
;;
;; o Make "Query" and "Message" work.
;;
;; o Prettify the actual list of nicks in some way.
;;
;; o Add a proper erc-module that people can turn on and off, figure
;;   out a way of creating the nicklist window at an appropriate time
;;   --- probably in `erc-join-hook'.
;;
;; o Ensure XEmacs compatibility --- the mouse-menu support is likely
;;   broken.
;;
;; o Add option to display in a separate frame --- will again need to
;;   be able to associate the nicklist with the currently active
;;   channel buffer or something similar.
;;
;; o Allow toggling of visibility of nicklist via ERC commands.

;;; History:
;;

;; Changes by Edgar Gonçalves <edgar.goncalves@inesc-id.pt>
;; Jun 25 2005:
;;     - images are changed to a standard set of names.
;;     - /images now contain gaim's status icons.
;; May 31 2005:
;;     - tooltips are improved. they try to access bbdb for a nice nick!
;; Apr 26 2005:
;;     - erc-nicklist-channel-users-info was fixed (sorting bug)
;;     - Away names don't need parenthesis when using icons
;; Apr 26 2005:
;;     - nicks can display icons of their connection type (msn, icq, for now)
;; Mar 15 2005:
;;     - nicks now are different for unvoiced and op users
;;     - nicks now have tooltips displaying more info
;; Mar 18 2005:
;;     - queries now work ok, both on menu and keyb shortcut RET.
;;     - nicklist is now sorted ignoring the case. Voiced nicks will
;;       appear according to `erc-nicklist-voiced-position'.

;;; Code:

(require 'erc)
(condition-case nil
    (require 'erc-bbdb)
  (error nil))
(eval-when-compile (require 'cl))

(defgroup erc-nicklist nil
  "Display a list of nicknames in a separate window."
  :group 'erc)

(defcustom erc-nicklist-use-icons t
  "*If non-nil, display an icon instead of the name of the chat medium.
By \"chat medium\", we mean IRC, AOL, MSN, ICQ, etc."
  :group 'erc-nicklist
  :type 'boolean)

(defcustom erc-nicklist-icons-directory
  (let ((dir (locate-library "erc-nicklist.el")))
    (when dir
      (concat (file-name-directory dir) "images/")))
  "*Directory of the PNG files for chat icons.
Icons are displayed if `erc-nicklist-use-icons' is non-nil."
  :group 'erc-nicklist
  :type 'directory)

(defcustom erc-nicklist-voiced-position 'bottom
  "*Position of voiced nicks in the nicklist.
The value can be `top', `bottom' or nil (don't sort)."
  :group 'erc-nicklist
  :type  '(choice
	   (const :tag "Top" top)
	   (const :tag "Bottom" bottom)
	   (const :tag "Mixed" nil)))

(defcustom erc-nicklist-window-size 20.0
  "*The size of the nicklist window.

This specifies a percentage of the channel window width.

A negative value means the nicklist window appears on the left of the
channel window, and vice versa."
  :group 'erc-nicklist
  :type 'float)


(defun erc-nicklist-buffer-name (&optional buffer)
  "Return the buffer name for a nicklist associated with BUFFER.

If BUFFER is nil, use the value of `current-buffer'."
  (format " *%s-nicklist*" (buffer-name (or buffer (current-buffer)))))

(defun erc-nicklist-make-window ()
  "Create an ERC nicklist window.

See also `erc-nicklist-window-size'."
  (let ((width (floor (* (window-width) (/ erc-nicklist-window-size 100.0))))
	(buffer (erc-nicklist-buffer-name))
	window)
    (split-window-horizontally (- width))
    (setq window (next-window))
    (set-window-buffer window (get-buffer-create buffer))
    (with-current-buffer buffer
      (set-window-dedicated-p window t))))


(defvar erc-nicklist-images-alist '()
  "Alist that maps a connection type to an icon.")

(defun erc-nicklist-insert-medium-name-or-icon (host channel is-away)
  "Inserts an icon or a string identifying the current host type.
This is configured using `erc-nicklist-use-icons' and
`erc-nicklist-icons-directory'."
  ;; identify the network (for bitlebee usage):
  (let ((bitlbee-p (save-match-data
		     (string-match "\\`&bitlbee\\b"
				   (buffer-name channel)))))
    (cond ((and bitlbee-p
		(string= "login.icq.com" host))
	   (if erc-nicklist-use-icons
	       (if is-away
		   (insert-image (cdr (assoc 'icq-away
					     erc-nicklist-images-alist)))
		 (insert-image (cdr (assoc 'icq
					   erc-nicklist-images-alist))))
	     (insert "ICQ")))
	  (bitlbee-p
	   (if erc-nicklist-use-icons
	       (if is-away
		   (insert-image (cdr (assoc 'msn-away
					     erc-nicklist-images-alist)))
		 (insert-image (cdr (assoc 'msn
					   erc-nicklist-images-alist))))
	     (insert "MSN")))
	  (t
	   (if erc-nicklist-use-icons
	       (if is-away
		   (insert-image (cdr (assoc 'irc-away
					     erc-nicklist-images-alist)))
		 (insert-image (cdr (assoc 'irc
					   erc-nicklist-images-alist))))
	     (insert "IRC"))))
    (insert " ")))

(defun erc-nicklist-search-for-nick (finger-host)
  "Return the bitlbee-nick field for this contact given FINGER-HOST.
Seach for the BBDB record of this contact.  If not found, return nil."
  (when (boundp 'erc-bbdb-bitlbee-name-field)
    (let ((record (car
		   (erc-member-if
		    #'(lambda (r)
			(let ((fingers (bbdb-record-finger-host r)))
			  (when fingers
			    (string-match finger-host
					  (car (bbdb-record-finger-host r))))))
		    (bbdb-records)))))
      (when record
	(bbdb-get-field record erc-bbdb-bitlbee-name-field)))))

(defun erc-nicklist-insert-contents (channel)
  "Insert the nicklist contents, with text properties and the optional images."
  (setq buffer-read-only nil)
  (erase-buffer)
  (dolist (u (erc-nicklist-channel-users-info channel))
    (let* ((server-user (car u))
	   (channel-user (cdr u))
	   (nick     (erc-server-user-nickname server-user))
	   (host     (erc-server-user-host server-user))
	   (login    (erc-server-user-login server-user))
	   (full-name(erc-server-user-full-name server-user))
	   (info     (erc-server-user-info server-user))
	   (channels (erc-server-user-buffers server-user))
	   (op       (erc-channel-user-op channel-user))
	   (voice    (erc-channel-user-voice channel-user))
	   (bbdb-nick (or (erc-nicklist-search-for-nick
			   (concat login "@" host))
			  ""))
	   (away-status (if voice "" "\n(Away)"))
	   (balloon-text (concat bbdb-nick (if (string= "" bbdb-nick)
					       "" "\n")
				 "Login: " login "@" host
				 away-status)))
      (erc-nicklist-insert-medium-name-or-icon host channel (not voice))
      (unless (or voice erc-nicklist-use-icons)
	(setq nick (concat "(" nick ")")))
      (when op
	(setq nick (concat nick " (OP)")))
      (insert (erc-propertize nick
			      'erc-nicklist-nick nick
			      'mouse-face 'highlight
			      'erc-nicklist-channel channel
			      'help-echo balloon-text)
	      "\n")))
  (erc-nicklist-mode))


(defun erc-nicklist ()
  "Create an ERC nicklist buffer."
  (interactive)
  (let ((channel (current-buffer)))
    (unless (or (not erc-nicklist-use-icons)
		erc-nicklist-images-alist)
      (setq erc-nicklist-images-alist
	    `((msn      . ,(create-image (concat erc-nicklist-icons-directory
						 "msn-online.png")))
	      (msn-away . ,(create-image (concat erc-nicklist-icons-directory
						 "msn-offline.png")))
	      (irc      . ,(create-image (concat erc-nicklist-icons-directory
						 "irc-online.png")))
	      (irc-away . ,(create-image (concat erc-nicklist-icons-directory
						 "irc-offline.png")))
	      (icq      . ,(create-image (concat erc-nicklist-icons-directory
						 "icq-online.png")))
	      (icq-away . ,(create-image (concat erc-nicklist-icons-directory
						 "icq-offline.png"))))))
    (erc-nicklist-make-window)
    (with-current-buffer (get-buffer (erc-nicklist-buffer-name channel))
      (erc-nicklist-insert-contents channel)))
  (add-hook 'erc-channel-members-changed-hook #'erc-nicklist-update))

(defun erc-nicklist-update ()
  "Update the ERC nicklist buffer."
  (let ((b (get-buffer (erc-nicklist-buffer-name)))
	(channel (current-buffer)))
    (when b
      (with-current-buffer b
	(erc-nicklist-insert-contents channel)))))

(defvar erc-nicklist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<down-mouse-3>") 'erc-nicklist-menu)
    (define-key map "\C-j" 'erc-nicklist-kbd-menu)
    (define-key map "q"  'erc-nicklist-quit)
    (define-key map (kbd "RET") 'erc-nicklist-kbd-cmd-QUERY)
    map)
  "Keymap for `erc-nicklist-mode'.")

(define-derived-mode erc-nicklist-mode fundamental-mode
  "Nicklist"
  "Major mode for the ERC nicklist buffer."
  (setq buffer-read-only t))

(defun erc-nicklist-call-erc-command (command point buffer window)
  "Call an ERC COMMAND.

Depending on what COMMAND is, it's called with one of POINT, BUFFER,
or WINDOW as arguments."
  (when command
    (let* ((p (text-properties-at point))
	   (b (plist-get p 'erc-nicklist-channel)))
      (if (memq command '(erc-nicklist-quit ignore))
	  (funcall command window)
	;; EEEK!  Horrble, but it's the only way we can ensure the
	;; response goes to the correct buffer.
	(erc-set-active-buffer b)
	(switch-to-buffer-other-window b)
	(funcall command (plist-get p 'erc-nicklist-nick))))))

(defun erc-nicklist-cmd-QUERY (user &optional server)
  "Opens a query buffer with USER."
  ;; FIXME: find a way to switch to that buffer afterwards...
  (let ((send (if server
		  (format "QUERY %s %s" user server)
		  (format "QUERY %s" user))))
    (erc-cmd-QUERY user)
    t))

(defun erc-nicklist-kbd-cmd-QUERY (&optional window)
  (interactive)
  (let* ((p      (text-properties-at (point)))
	 (server (plist-get p 'erc-nicklist-channel))
	 (nick   (plist-get p 'erc-nicklist-nick))
	 (nick   (or (and (string-match "(\\(.*\\))" nick)
			  (match-string 1 nick))
		     nick))
	 (nick   (or (and (string-match "\\+\\(.*\\)" nick)
			  (match-string 1 nick))
		     nick))
	 (send   (format "QUERY %s %s" nick server)))
    (switch-to-buffer-other-window server)
    (erc-cmd-QUERY nick)))


(defvar erc-nicklist-menu
  (let ((map (make-sparse-keymap "Action")))
    (define-key map [erc-cmd-WHOIS]
      '("Whois" . erc-cmd-WHOIS))
    (define-key map [erc-cmd-DEOP]
      '("Deop" . erc-cmd-DEOP))
    (define-key map [erc-cmd-MSG]
      '("Message" . erc-cmd-MSG)) ;; TODO!
    (define-key map [erc-nicklist-cmd-QUERY]
      '("Query" . erc-nicklist-kbd-cmd-QUERY))
    (define-key map [ignore]
      '("Cancel" . ignore))
    (define-key map [erc-nicklist-quit]
      '("Close nicklist" . erc-nicklist-quit))
    map)
  "Menu keymap for the ERC nicklist.")

(defun erc-nicklist-quit (&optional window)
  "Delete the ERC nicklist.

Deletes WINDOW and stops updating the nicklist buffer."
  (interactive)
  (let ((b (window-buffer window)))
    (with-current-buffer b
      (set-buffer-modified-p nil)
      (kill-this-buffer)
      (remove-hook 'erc-channel-members-changed-hook 'erc-nicklist-update))))


(defun erc-nicklist-kbd-menu ()
  "Show the ERC nicklist menu."
  (interactive)
  (let* ((point (point))
	 (window (selected-window))
	 (buffer (current-buffer)))
    (with-current-buffer buffer
      (erc-nicklist-call-erc-command
       (car (x-popup-menu point
			  erc-nicklist-menu))
       point
       buffer
       window))))

(defun erc-nicklist-menu (&optional arg)
  "Show the ERC nicklist menu.

ARG is a parametrized event (see `interactive')."
  (interactive "e")
  (let* ((point (nth 1 (cadr arg)))
	 (window (car (cadr arg)))
	 (buffer (window-buffer window)))
    (with-current-buffer buffer
      (erc-nicklist-call-erc-command
       (car (x-popup-menu arg
			  erc-nicklist-menu))
       point
       buffer
       window))))


(defun erc-nicklist-channel-users-info (channel)
  "Return a nick-sorted list of all users on CHANNEL.
Result are elements in the form (SERVER-USER . CHANNEL-USER). The
list has all the voiced users according to
`erc-nicklist-voiced-position'."
  (let* ((nicks (erc-sort-channel-users-alphabetically
		 (with-current-buffer channel (erc-get-channel-user-list)))))
    (if erc-nicklist-voiced-position
	(let ((voiced-nicks (erc-remove-if-not
			     #'(lambda (x)
				 (null (erc-channel-user-voice (cdr x))))
			     nicks))
	      (devoiced-nicks (erc-remove-if-not
			       #'(lambda (x)
				   (erc-channel-user-voice
				    (cdr x)))
			       nicks)))
	  (cond ((eq erc-nicklist-voiced-position 'top)
		 (append devoiced-nicks voiced-nicks))
		((eq erc-nicklist-voiced-position 'bottom)
		 (append voiced-nicks devoiced-nicks))))
      nicks)))



(provide 'erc-nicklist)

;;; erc-nicklist.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; coding: utf-8
;; End:

;; arch-tag: db37a256-87a7-4544-bd90-e5f16c9f5ca5
