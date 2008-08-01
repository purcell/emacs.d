;;; erc-chess.el --- CTCP chess playing support for ERC

;; Copyright (C) 2002, 2004, 2007, 2008 Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: games, comm

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

;; This module requires chess.el by John Wiegley.
;; You need to have chess.el installed (load-path properly set)

;;; Code:

(require 'erc)
(require 'chess-network)
(require 'chess-display)
(require 'chess)

;;;; Variables

(defgroup erc-chess nil
  "Playing chess over IRC."
  :group 'erc)

(defcustom erc-chess-verbose-flag nil
  "*If non-nil, inform about bogus CTCP CHESS messages in the server buffer."
  :group 'erc-chess
  :type 'boolean)

(defcustom erc-chess-debug-flag t
  "*If non-nil, print all chess CTCP messages received in the server buffer."
  :group 'erc-chess
  :type 'boolean)

;;;###autoload
(defvar erc-ctcp-query-CHESS-hook '(erc-chess-ctcp-query-handler))

(defvar erc-chess-alist nil
  "Alist of chess sessions. It has the form of (NICK ENGINE)")
(make-variable-buffer-local 'erc-chess-alist)

(defvar erc-chess-regexp-alist chess-network-regexp-alist)
(defvar erc-chess-partner)
(make-variable-buffer-local 'erc-chess-partner)

;;;; Catalog messages

(erc-define-catalog
 'english
 '((ctcp-chess-debug   . "CTCPchess: %n (%u@%h) sent: '%m'")
   (ctcp-chess-quit    . "Chess game with %n (%u@%h) quit")))


(defun erc-chess-response-handler (event &rest args)
  (when (and (eq event 'accept)
	     (eq chess-engine-pending-offer 'match))
    (let ((display (chess-game-data (chess-engine-game nil) 'display)))
      (chess-display-enable-popup display)
      (chess-display-popup display)))

  (apply 'chess-engine-default-handler event args))


(defun erc-chess-handler (game event &rest args)
  "Handle erc-chess events.
This is the main handler for the erc-chess module."
  (cond
   ((eq event 'initialize)
    (setq erc-chess-partner (car args))
    (setq erc-server-process (nth 1 args))
    t)

   ((eq event 'send)
    ;; Transmit the string given in `(car args)' to the nick
    ;; saved in `erc-chess-partner'.
    (let ((nick erc-chess-partner)
	  (msg (substring (car args) 0 (1- (length (car args))))))
      (erc-with-server-buffer
	(erc-send-ctcp-message nick (concat "CHESS " msg) t))))

   (t
    (cond
     ((eq event 'accept)
      (let ((display (chess-game-data (chess-engine-game nil) 'display)))
	(chess-display-enable-popup display)
	(chess-display-popup display)))

     ((eq event 'destroy)
      (let* ((buf (process-buffer erc-server-process))
	     (nick (erc-downcase erc-chess-partner))
	     (engine (current-buffer)))
	(erc-with-server-buffer
	  (let ((elt (assoc nick erc-chess-alist)))
	    (when (and elt (eq (nth 1 elt) engine))
	      (message "Removed from erc-chess-alist in destroy event")
	      (setq erc-chess-alist (delq elt erc-chess-alist))))))))

    ;; Pass all other events down to chess-network
    (apply 'chess-network-handler game event args))))

;;;; Game initialisation

(defun erc-chess-engine-create (nick)
  "Initialize a game for a particular nick.
This function adds to `erc-chess-alist' too."
  ;; Maybe move that into the connect callback?
  (let* ((objects (chess-session 'erc-chess t 'erc-chess-response-handler
				 nick erc-server-process))
	 (engine (car objects))
	 (display (cadr objects)))
    (when engine
      (if display
	  (chess-game-set-data (chess-display-game display)
			       'display display))
      (push (list (erc-downcase nick) engine) erc-chess-alist)
      engine)))

;;;; IRC /commands

;;;###autoload
(defun erc-cmd-CHESS (line &optional force)
  "Initiate a chess game via CTCP to NICK.
NICK should be the first and only arg to /chess"
  (cond
   ((string-match (concat "^\\s-*\\(" erc-valid-nick-regexp "\\)\\s-*$") line)
    (let ((nick (match-string 1 line)))
      (erc-with-server-buffer
	(if (assoc (erc-downcase nick) erc-chess-alist)
	    ;; Maybe check for correctly connected game, and switch here.
	    (erc-display-message
	     nil 'notice 'active
	     (concat "Invitation for a game already sent to " nick))
	  (with-current-buffer (erc-chess-engine-create nick)
	    (erc-chess-handler nil 'match)
	    t)))))
   (t nil)))

;;; CTCP handler
;;;###autoload
(defun erc-chess-ctcp-query-handler (proc nick login host to msg)
  (if erc-chess-debug-flag
      (erc-display-message
       nil 'notice (current-buffer)
       'ctcp-chess-debug ?n nick ?m msg ?u login ?h host))
  (when (string-match "^CHESS\\s-+\\(.*\\)$" msg)
    (let ((str (concat (match-string 1 msg) "\n"))
	  (elt (assoc (erc-downcase nick) erc-chess-alist)))
      (if (not elt)
	    (chess-engine-submit (erc-chess-engine-create nick) str)
	(if (buffer-live-p (nth 1 elt))
	    (chess-engine-submit (nth 1 elt) str)
	  (setq erc-chess-alist (delq elt erc-chess-alist)))))))

(provide 'erc-chess)

;;; erc-chess.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:

;; arch-tag: beb148d1-db16-48da-8145-9f3a7ff27b7b
