;;; erc-join.el --- autojoin channels on connect and reconnects

;; Copyright (C) 2002, 2003, 2004, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; Keywords: irc
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ErcAutoJoin

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

;; This allows us to customize an `erc-autojoin-channels-alist'.  As
;; we /JOIN and /PART channels, this alist is updated to reflect our
;; current setup, so that when we reconnect, we rejoin the same
;; channels.  The alist can be customized, so that the customized
;; value will be used when we reconnect in our next Emacs session.

;;; Code:

(require 'erc)
(eval-when-compile (require 'cl))

(defgroup erc-autojoin nil
  "Enable autojoining."
  :group 'erc)

;;;###autoload (autoload 'erc-autojoin-mode "erc-join" nil t)
(define-erc-module autojoin nil
  "Makes ERC autojoin on connects and reconnects."
  ((add-hook 'erc-after-connect 'erc-autojoin-channels)
   (add-hook 'erc-server-JOIN-functions 'erc-autojoin-add)
   (add-hook 'erc-server-PART-functions 'erc-autojoin-remove))
  ((remove-hook 'erc-after-connect 'erc-autojoin-channels)
   (remove-hook 'erc-server-JOIN-functions 'erc-autojoin-add)
   (remove-hook 'erc-server-PART-functions 'erc-autojoin-remove)))

(defcustom erc-autojoin-channels-alist nil
  "Alist of channels to autojoin on IRC networks.
Every element in the alist has the form (SERVER . CHANNELS).
SERVER is a regexp matching the server, and channels is the
list of channels to join.

Customize this variable to set the value for your first connect.
Once you are connected and join and part channels, this alist
keeps track of what channels you are on, and will join them
again when you get disconnected.  When you restart Emacs, however,
those changes are lost, and the customization you saved the last
time is used again."
  :group 'erc-autojoin
  :type '(repeat (cons :tag "Server"
		       (regexp :tag "Name")
		       (repeat :tag "Channels"
			       (string :tag "Name")))))

(defcustom erc-autojoin-domain-only t
  "Truncate host name to the domain name when joining a server.
If non-nil, and a channel on the server a.b.c is joined, then
only b.c is used as the server for `erc-autojoin-channels-alist'.
This is important for networks that redirect you to other
servers, presumably in the same domain."
  :group 'erc-autojoin
  :type 'boolean)

(defun erc-autojoin-channels (server nick)
  "Autojoin channels in `erc-autojoin-channels-alist'."
  (dolist (l erc-autojoin-channels-alist)
    (when (string-match (car l) server)
      (dolist (chan (cdr l))
	(erc-server-send (concat "join " chan))))))

(defun erc-autojoin-add (proc parsed)
  "Add the channel being joined to `erc-autojoin-channels-alist'."
  (let* ((chnl (erc-response.contents parsed))
	 (nick (car (erc-parse-user (erc-response.sender parsed))))
	 (server (with-current-buffer (process-buffer proc)
		   (or erc-server-announced-name erc-session-server))))
    (when (erc-current-nick-p nick)
      (when (and erc-autojoin-domain-only
		 (string-match "[^.\n]+\\.\\([^.\n]+\\.[^.\n]+\\)$" server))
	(setq server (match-string 1 server)))
      (let ((elem (assoc server erc-autojoin-channels-alist)))
	(if elem
	    (unless (member chnl (cdr elem))
	      (setcdr elem (cons chnl (cdr elem))))
	  (setq erc-autojoin-channels-alist
		(cons (list server chnl)
		      erc-autojoin-channels-alist))))))
  ;; We must return nil to tell ERC to continue running the other
  ;; functions.
  nil)

;; (erc-parse-user "kensanata!~user@dclient217-162-233-228.hispeed.ch")

(defun erc-autojoin-remove (proc parsed)
  "Remove the channel being left from `erc-autojoin-channels-alist'."
  (let* ((chnl (car (erc-response.command-args parsed)))
	 (nick (car (erc-parse-user (erc-response.sender parsed))))
	 (server (with-current-buffer (process-buffer proc)
		   (or erc-server-announced-name erc-session-server))))
    (when (erc-current-nick-p nick)
      (when (and erc-autojoin-domain-only
		 (string-match "[^.\n]+\\.\\([^.\n]+\\.[^.\n]+\\)$" server))
	(setq server (match-string 1 server)))
      (let ((elem (assoc server erc-autojoin-channels-alist)))
	(when elem
	  (setcdr elem (delete chnl (cdr elem)))
	  (unless (cdr elem)
	    (setq erc-autojoin-channels-alist
		  (delete elem erc-autojoin-channels-alist)))))))
  ;; We must return nil to tell ERC to continue running the other
  ;; functions.
  nil)

(provide 'erc-join)

;;; erc-join.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:

;; arch-tag: d62d8b15-8e31-49d6-8a73-12f11e717414
