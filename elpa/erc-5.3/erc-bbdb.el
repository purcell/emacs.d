;;; erc-bbdb.el --- Integrating the BBDB into ERC

;; Copyright (C) 2001, 2002, 2004, 2005, 2006, 2007
;;   2008 Free Software Foundation, Inc.

;; Author: Andreas Fuchs <asf@void.at>
;; Maintainer: Mario Lang <mlang@delysid.org>

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

;; This mode connects the BBDB to ERC.  Whenever a known nick
;; connects, the corresponding BBDB record pops up.  To identify
;; users, use the irc-nick field.  Define it, if BBDB asks you about
;; that.  When you use /WHOIS on a known nick, the corresponding
;; record will be updated.

;;; History

;; Andreas Fuchs <asf@void.at> wrote zenirc-bbdb-whois.el, which was
;; adapted for ERC by Mario Lang <mlang@delysid.org>.

;; Changes by Edgar Gonçalves <edgar.goncalves@inesc-id.pt>
;; May 31 2005:
;;     - new variable: erc-bbdb-bitlbee-name-field - the field name for the
;;       msn/icq/etc nick
;;     - nick doesn't go the the name. now it asks for an existing record to
;;       merge with. If none, then create a new one with the nick as name.

;;; Code:

(require 'erc)
(require 'bbdb)
(require 'bbdb-com)
(require 'bbdb-gui)
(require 'bbdb-hooks)

(defgroup erc-bbdb nil
  "Variables related to BBDB usage."
  :group 'erc)

(defcustom erc-bbdb-auto-create-on-whois-p nil
  "*If nil, don't create bbdb records automatically when a WHOIS is done.
Leaving this at nil is a good idea, but you can turn it
on if you want to have lots of People named \"John Doe\" in your BBDB."
  :group 'erc-bbdb
  :type 'boolean)

(defcustom erc-bbdb-auto-create-on-join-p nil
  "*If nil, don't create bbdb records automatically when a person joins a channel.
Leaving this at nil is a good idea, but you can turn it
on if you want to have lots of People named \"John Doe\" in your BBDB."
  :group 'erc-bbdb
  :type 'boolean)

(defcustom erc-bbdb-auto-create-on-nick-p nil
  "*If nil, don't create bbdb records automatically when a person changes her nick.
Leaving this at nil is a good idea, but you can turn it
on if you want to have lots of People named \"John Doe\" in your BBDB."
  :group 'erc-bbdb
  :type 'boolean)

(defcustom erc-bbdb-popup-type 'visible
  "*If t, pop up a BBDB buffer showing the record of a WHOISed person
or the person who has just joined a channel.

If set to 'visible, the BBDB buffer only pops up when someone was WHOISed
or a person joined a channel visible on any frame.

If set to nil, never pop up a BBDD buffer."
  :group 'erc-bbdb
  :type '(choice (const :tag "When visible" visible)
		 (const :tag "When joining" t)
		 (const :tag "Never" nil)))

(defcustom erc-bbdb-irc-nick-field 'irc-nick
  "The notes field name to use for annotating IRC nicknames."
  :group 'erc-bbdb
  :type 'symbol)

(defcustom erc-bbdb-irc-channel-field 'irc-channel
  "The notes field name to use for annotating IRC channels."
  :group 'erc-bbdb
  :type 'symbol)

(defcustom erc-bbdb-irc-highlight-field 'irc-highlight
  "The notes field name to use for highlighting a person's messages."
  :group 'erc-bbdb
  :type 'symbol)

(defcustom erc-bbdb-bitlbee-name-field 'bitlbee-name
  "The notes field name to use for annotating bitlbee displayed name.
This is the name that a bitlbee (AIM/MSN/ICQ) contact provides as
their \"displayed name\"."
  :group 'erc-bbdb
  :type 'symbol)

(defcustom erc-bbdb-elide-display nil
  "*If t, show BBDB popup buffer elided."
  :group 'erc-bbdb
  :type 'boolean)

(defcustom erc-bbdb-electric-p nil
  "*If t, BBDB popup buffer is electric."
  :group 'erc-bbdb
  :type 'boolean)

(defun erc-bbdb-search-name-and-create (create-p name nick finger-host silent)
  (let* ((ircnick (cons erc-bbdb-irc-nick-field (concat "^"
							(regexp-quote nick))))
	 (finger (cons bbdb-finger-host-field (regexp-quote finger-host)))
	 (record (or (bbdb-search (bbdb-records) nil nil nil ircnick)
		     (and name (bbdb-search-simple name nil))
		     (bbdb-search (bbdb-records) nil nil nil finger)
		     (unless silent
		       (bbdb-completing-read-one-record
			"Merge using record of (C-g to skip, RET for new): "))
		     (when create-p
		       (bbdb-create-internal (or name
						 "John Doe")
					     nil nil nil nil nil)))))
    ;; sometimes, the record will be a list. I don't know why.
    (if (listp record)
	(car record)
      record)))

(defun erc-bbdb-show-entry (record channel proc)
  (let ((bbdb-display-layout (bbdb-grovel-elide-arg erc-bbdb-elide-display))
	(bbdb-electric-p erc-bbdb-electric-p))
    (when (and record (or (eq erc-bbdb-popup-type t)
			  (and (eq erc-bbdb-popup-type 'visible)
			       (and channel
				    (or (eq channel t)
					(get-buffer-window (erc-get-buffer
							    channel proc)
							   'visible))))))
      (bbdb-display-records (list record)))))

(defun erc-bbdb-insinuate-and-show-entry-1 (create-p proc nick name finger-host silent &optional chan new-nick)
  (let ((record (erc-bbdb-search-name-and-create
		 create-p nil nick finger-host silent))) ;; don't search for a name
    (when record
      (bbdb-annotate-notes record (or new-nick nick) erc-bbdb-irc-nick-field)
      (bbdb-annotate-notes record finger-host bbdb-finger-host-field)
      (and name
	   (bbdb-annotate-notes record name erc-bbdb-bitlbee-name-field t))
      (and chan
	   (not (eq chan t))
	   (bbdb-annotate-notes record chan erc-bbdb-irc-channel-field))
      (erc-bbdb-highlight-record record)
      (erc-bbdb-show-entry record chan proc))))

(defun erc-bbdb-insinuate-and-show-entry (create-p proc nick name finger-host silent &optional chan new-nick)
  ;; run this outside of the IRC filter process, to avoid an annoying
  ;; error when the user hits C-g
  (run-at-time 0.1 nil
	       #'erc-bbdb-insinuate-and-show-entry-1
	       create-p proc nick name finger-host silent chan new-nick))

(defun erc-bbdb-whois (proc parsed)
  (let (; We could use server name too, probably
	(nick (second (erc-response.command-args parsed)))
	(name (erc-response.contents parsed))
	(finger-host (concat (third (erc-response.command-args parsed))
			     "@"
			     (fourth (erc-response.command-args parsed)))))
    (erc-bbdb-insinuate-and-show-entry erc-bbdb-auto-create-on-whois-p proc
				       nick name finger-host nil t)))

(defun erc-bbdb-JOIN (proc parsed)
  (let* ((sender (erc-parse-user (erc-response.sender parsed)))
	 (nick (nth 0 sender)))
    (unless (string= nick (erc-current-nick))
      (let* ((channel (erc-response.contents parsed))
	     (finger-host (concat (nth 1 sender) "@" (nth 2 sender))))
	  (erc-bbdb-insinuate-and-show-entry
	   erc-bbdb-auto-create-on-join-p proc
	   nick nil finger-host t channel)))))

(defun erc-bbdb-NICK (proc parsed)
  "Annotate new nick name to a record in case it already exists."
  (let* ((sender (erc-parse-user (erc-response.sender parsed)))
	 (nick (nth 0 sender)))
    (unless (string= nick (erc-current-nick))
      (let* ((finger-host (concat (nth 1 sender) "@" (nth 2 sender))))
	(erc-bbdb-insinuate-and-show-entry
	 erc-bbdb-auto-create-on-nick-p proc
	 nick nil finger-host t nil (erc-response.contents parsed))))))

(defun erc-bbdb-init-highlighting-hook-fun (proc parsed)
  (erc-bbdb-init-highlighting))

(defun erc-bbdb-init-highlighting ()
  "Initialize the highlighting based on BBDB fields.
This function typically gets called on a successful server connect.
The field name in the BBDB which controls highlighting is specified by
`erc-bbdb-irc-highlight-field'. Fill in either \"pal\"
\"dangerous-host\" or \"fool\". They work exactly like their
counterparts `erc-pals', `erc-dangerous-hosts' and `erc-fools'."
  (let* ((irc-highlight (cons erc-bbdb-irc-highlight-field
			      ".+"))
	(matching-records (bbdb-search (bbdb-records)
				       nil nil nil irc-highlight)))
    (mapcar 'erc-bbdb-highlight-record matching-records)))

(defun erc-bbdb-highlight-record (record)
  (let* ((notes (bbdb-record-raw-notes record))
	 (highlight-field (assoc erc-bbdb-irc-highlight-field notes))
	 (nick-field      (assoc erc-bbdb-irc-nick-field notes)))
    (if (and highlight-field
	     nick-field)
	(let ((highlight-types (split-string (cdr highlight-field)
					     bbdb-notes-default-separator))
	      (nick-names (split-string (cdr nick-field)
					(concat "\\(\n\\|"
						bbdb-notes-default-separator
						"\\)"))))
	  (mapcar
	   (lambda (highlight-type)
	     (mapcar
	      (lambda (nick-name)
		(if (member highlight-type
			    '("pal" "dangerous-host" "fool"))
		    (add-to-list (intern (concat "erc-" highlight-type "s"))
				 (regexp-quote nick-name))
		  (error (format "\"%s\" (in \"%s\") is not a valid highlight type!"
				 highlight-type nick-name))))
	      nick-names))
	   highlight-types)))))

;;;###autoload (autoload 'erc-bbdb-mode "erc-bbdb")
(define-erc-module bbdb nil
  "In ERC BBDB mode, you can directly interact with your BBDB."
  ((add-hook 'erc-server-311-functions 'erc-bbdb-whois t)
   (add-hook 'erc-server-JOIN-functions 'erc-bbdb-JOIN t)
   (add-hook 'erc-server-NICK-functions 'erc-bbdb-NICK t)
   (add-hook 'erc-server-376-functions 'erc-bbdb-init-highlighting-hook-fun t))
  ((remove-hook 'erc-server-311-functions 'erc-bbdb-whois)
   (remove-hook 'erc-server-JOIN-functions 'erc-bbdb-JOIN)
   (remove-hook 'erc-server-NICK-functions 'erc-bbdb-NICK)
   (remove-hook 'erc-server-376-functions 'erc-bbdb-init-highlighting-hook-fun)))

(provide 'erc-bbdb)

;;; erc-bbdb.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; coding: utf-8
;; End:

;; arch-tag: 1edf3729-cd49-47dc-aced-70fcfc28c815
