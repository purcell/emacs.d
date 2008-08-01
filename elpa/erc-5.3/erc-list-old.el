;;; erc-list-old.el --- Provide a faster channel listing mechanism

;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007
;;   2008 Free Software Foundation, Inc.
;; Copyright (C) 2004 Brian Palmer

;; Author: Mario Lang <mlang@lexx.delysid.org>
;; Keywords: comm

;; This file is part of ERC.

;; ERC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; ERC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ERC; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file provides a simple derived mode for viewing Channel lists.
;; It also serves as a demonstration of how the new server hook facility
;; can be used.

;;; Code:

(require 'erc)
(require 'erc-networks)
(require 'sort)
(unless (fboundp 'make-overlay)
  (require 'overlay))
(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User customizable variables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup erc-list-old nil
  "Display IRC channels in another window when using /LIST"
  :group 'erc)

(defcustom erc-chanlist-progress-message t
  "*Show progress message while accumulating channel list."
  :group 'erc-list-old
  :type 'boolean)

(defcustom erc-no-list-networks nil
  "*A list of network names on which the /LIST command refuses to work."
  :group 'erc-list-old
  :type '(repeat string))

(defcustom erc-chanlist-frame-parameters nil
  "*If nil, the channel list is displayed in a new window; if non-nil,
this variable holds the frame parameters used to make a frame to
display the channel list."
  :group 'erc-list-old
  :type 'list)

(defcustom erc-chanlist-hide-modeline nil
  "*If nil, the channel list buffer has a modeline, otherwise the modeline is hidden."
  :group 'erc-list-old
  :type 'boolean)

(defface erc-chanlist-header-face '((t (:bold t)))
  "Face used for the headers in erc's channel list."
  :group 'erc-faces)

(defface erc-chanlist-odd-line-face '((t (:inverse-video t)))
  "Face used for the odd lines in erc's channel list."
  :group 'erc-faces)

(defface erc-chanlist-even-line-face '((t (:inverse-video nil)))
  "Face used for the even lines in erc's channel list."
  :group 'erc-faces)

(defface erc-chanlist-highlight '((t (:foreground "red")))
  "Face used to highlight the current line in the channel list."
  :group 'erc-faces)

;; This should perhaps be a defface that inherits values from the highlight face
;; but xemacs does not support inheritance
(defcustom erc-chanlist-highlight-face 'erc-chanlist-highlight
  "Face used for highlighting the current line in a list."
  :type 'face
  :group 'erc-faces)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All variables below this line are for internal use only.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar erc-chanlist-channel-line-regexp "^\\([#&\\*][^ \t\n]*\\)\\s-+[0-9]+"
  "Regexp that matches a channel line in the channel list buffer.")

(defvar erc-chanlist-buffer nil)
(make-variable-buffer-local 'erc-chanlist-buffer)

(defvar erc-chanlist-last-time 0
  "A time value used to throttle the progress indicator.")

(defvar erc-chanlist-frame nil
  "The frame displaying the most recent channel list buffer.")

(defvar erc-chanlist-sort-state 'channel
  "The sort mode of the channel list buffer.  Either 'channel or 'users.")
(make-variable-buffer-local 'erc-chanlist-sort-state)

(defvar erc-chanlist-highlight-overlay nil
  "The overlay used for erc chanlist highlighting")
(make-variable-buffer-local 'erc-chanlist-highlight-overlay)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define erc-chanlist-mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom erc-chanlist-mode-hook nil
  "Hook run by erc-chanlist-mode."
  :group 'erc-list-old
  :type 'hook)

(define-derived-mode erc-chanlist-mode fundamental-mode "ERC Channel List"
  "Mode for viewing a channel list of a particular server.

\\{erc-chanlist-mode-map}"
  (local-set-key "\C-c\C-j" 'erc-join-channel)
  (local-set-key "j" 'erc-chanlist-join-channel)
  (local-set-key "n" 'next-line)
  (local-set-key "p" 'previous-line)
  (local-set-key "q" 'erc-chanlist-quit)
  (local-set-key "s" 'erc-chanlist-toggle-sort-state)
  (local-set-key "t" 'toggle-truncate-lines)
  (setq erc-chanlist-sort-state 'channel)
  (setq truncate-lines t)
  (add-hook 'post-command-hook 'erc-chanlist-post-command-hook 'append 'local))

;; Define module:
;;;###autoload (autoload 'erc-list-old-mode "erc-list-old")
(define-erc-module list nil
  "List channels nicely in a separate buffer."
  ((defalias 'erc-cmd-LIST 'erc-list-channels))
  ((defalias 'erc-cmd-LIST 'erc-list-channels-simple)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun erc-list-channels (&rest channel)
  "Display a buffer containing a list of channels on the current server.
Optional argument CHANNEL specifies a single channel to list (instead of every
available channel)."
  (interactive
   (remove "" (split-string
	       (read-from-minibuffer "List channels (RET for all): ") " ")))
  (if (and (null channel)
	   (erc-member-ignore-case (erc-network-name) erc-no-list-networks))
      (erc-display-line "ERC is configured not to allow the /LIST command on this network!"
			(current-buffer))
    (erc-display-line (erc-make-notice (concat "Listing channel"
					       (if channel
						   "."
						 "s.  This may take a while."))))
    (erc-chanlist channel))
  t)

(defun erc-list-channels-simple (&optional line)
  "Send the LIST command to the current server with optional channels LINE."
  (when (string-match "^\\s-*\\(.*\\)$" line)
    (let ((channels (match-string 1 line)))
      (erc-log (format "cmd: LIST: %s" channels))
      (erc-server-send
       (if (string= channels "")
	   "LIST"
	 (concat "LIST :" channels))))
    t))
(put 'erc-list-channels-simple 'do-not-parse-args t)

;;;###autoload
(defun erc-chanlist (&optional channels)
  "Show a channel listing of the current server in a special mode.
Please note that this function only works with IRC servers which conform
to RFC and send the LIST header (#321) at start of list transmission."
  (interactive)
  (erc-with-server-buffer
    (erc-once-with-server-event
     321
     '(progn
	(add-hook 'erc-server-322-functions 'erc-chanlist-322 nil t)

	(erc-once-with-server-event
	 323
	 '(progn
	    (remove-hook 'erc-server-322-functions 'erc-chanlist-322 t)
	    (let ((buf erc-chanlist-buffer))
	      (if (not (buffer-live-p buf))
		  (error "`erc-chanlist-buffer' does not refer to a live buffer"))

	      (set-buffer buf)
	      (buffer-disable-undo)
	      (let (buffer-read-only
		    (sort-fold-case t))
		(sort-lines nil (point-min) (point-max))
		(setq erc-chanlist-sort-state 'channel)

		(let ((sum (count-lines (point-min) (point-max))))
		  (goto-char (point-min))
		  (insert (substitute-command-keys
			   (concat "'\\[erc-chanlist-toggle-sort-state]' toggle sort mode.\n"
				   "'\\[erc-chanlist-quit]' kill this buffer.\n"
				   "'\\[toggle-truncate-lines]' toggle line truncation.\n"
				   "'\\[erc-chanlist-join-channel]' join the channel listed on the current line.\n\n")))
		  (insert (format "%d channels (sorted by %s).\n\n"
				  sum (if (eq erc-chanlist-sort-state 'channel)
					  "channel name"
					"number of users"))))

		(insert (format "%-25s%5s %s\n------------------------ ----- ----------------------------\n"
				"Channel"
				"Users"
				"Topic"))

		;; Display the channel list buffer.
		(if erc-chanlist-frame-parameters
		    (progn
		      (if (or (null erc-chanlist-frame)
			      (not (frame-live-p erc-chanlist-frame)))
			  (setq erc-chanlist-frame
				(make-frame `((name . ,(format "Channels on %s"
							       erc-session-server))
					      ,@erc-chanlist-frame-parameters))))
		      (select-frame erc-chanlist-frame)
		      (switch-to-buffer buf)
		      (erc-prettify-channel-list))
		  (pop-to-buffer buf)
		  (erc-prettify-channel-list))))
	    (goto-char (point-min))
	    (search-forward-regexp "^------" nil t)
	    (forward-line 1)
	    (erc-chanlist-highlight-line)
	    (message "")
	    t))

	(setq erc-chanlist-buffer (get-buffer-create
				   (format "*Channels on %s*"
					   (erc-response.sender parsed))))
	(with-current-buffer erc-chanlist-buffer
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (erc-chanlist-mode)
	  (setq erc-server-process proc)
	  (if erc-chanlist-hide-modeline
	      (setq mode-line-format nil))
	  (setq buffer-read-only t))
	t))

    ;; Now that we've setup our callbacks, pull the trigger.
    (if (interactive-p)
	(message "Collecting channel list for server %s" erc-session-server))
    (erc-server-send (if (null channels)
			  "LIST"
			(concat "LIST "
				(mapconcat #'identity channels ","))))))

(defun erc-chanlist-322 (proc parsed)
  "Process an IRC 322 message.

The message carries information about one channel for the LIST
command."
  (multiple-value-bind (channel num-users)
      (cdr (erc-response.command-args parsed))
    (let ((topic (erc-response.contents parsed)))
      (with-current-buffer erc-chanlist-buffer
	(save-excursion
	  (goto-char (point-max))
	  (let (buffer-read-only)
	    (insert (format "%-26s%4s %s\n" (erc-controls-strip channel)
			    num-users
			    (erc-controls-strip topic))))

	  ;; Maybe display a progress indicator in the minibuffer.
	  (when (and erc-chanlist-progress-message
		     (> (erc-time-diff
			 erc-chanlist-last-time (erc-current-time))
			3))
	    (setq erc-chanlist-last-time (erc-current-time))
	    (message "Accumulating channel list ... %c"
		     (aref [?/ ?| ?\\ ?- ?! ?O ?o] (random 7))))

	  ;; Return success to prevent other hook functions from being run.
	  t)))))

(defun erc-chanlist-post-command-hook ()
  "Keep the current line highlighted."
  (ignore-errors
    (save-excursion
      (beginning-of-line)
      (if (looking-at erc-chanlist-channel-line-regexp)
	  (erc-chanlist-highlight-line)
	(erc-chanlist-dehighlight-line)))))

(defun erc-chanlist-highlight-line ()
  "Highlight the current line."
  (unless erc-chanlist-highlight-overlay
    (setq erc-chanlist-highlight-overlay
	  (make-overlay (point-min) (point-min)))
    ;; Detach it from the buffer.
    (delete-overlay erc-chanlist-highlight-overlay)
    (overlay-put erc-chanlist-highlight-overlay
		 'face erc-chanlist-highlight-face)
    ;; Expressly put it at a higher priority than the text
    ;; properties used for faces later on. Gnu emacs promises that
    ;; right now overlays are higher priority than text properties,
    ;; but why take chances?
    (overlay-put erc-chanlist-highlight-overlay 'priority 1))
  (move-overlay erc-chanlist-highlight-overlay (point) (1+ (point-at-eol))))

(defun erc-chanlist-dehighlight-line ()
  "Remove the line highlighting."
  (delete-overlay erc-chanlist-highlight-overlay))

(defun erc-prettify-channel-list ()
  "Make the channel list buffer look pretty.
When this function runs, the current buffer must be the channel
list buffer, or it does nothing."
  (if (eq major-mode 'erc-chanlist-mode)
      (save-excursion
	(let ((inhibit-read-only t))
	  (goto-char (point-min))
	  (when (search-forward-regexp "^-------" nil t)
	    (add-text-properties
	     (point-min) (1+ (point-at-eol)) '(face erc-chanlist-header-face))
	    (forward-line 1))

	  (while (not (eobp))
	    (add-text-properties
	     (point) (1+ (point-at-eol)) '(face erc-chanlist-odd-line-face))
	    (forward-line 1)
	    (unless (eobp)
	      (add-text-properties
	       (point) (1+ (point-at-eol)) '(face erc-chanlist-even-line-face)))
	    (forward-line 1))))))

(defun erc-chanlist-toggle-sort-state ()
  "Toggle the channel list buffer sorting method.
Either sort by channel names or by number of users in each channel."
  (interactive)
  (let ((inhibit-read-only t)
	(sort-fold-case t))
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp "^-----" nil t)
      (forward-line 1)
      (unless (eobp)
	(if (eq erc-chanlist-sort-state 'channel)
	    (progn
	      (sort-numeric-fields 2 (point) (point-max))
	      (reverse-region (point) (point-max))
	      (setq erc-chanlist-sort-state 'users))
	  (sort-lines nil (point) (point-max))
	  (setq erc-chanlist-sort-state 'channel))

	(goto-char (point-min))
	(if (search-forward-regexp "^[0-9]+ channels (sorted by \\(.*\\)).$"
				   nil t)
	    (replace-match (if (eq erc-chanlist-sort-state 'channel)
			       "channel name"
			     "number of users")
			   nil nil nil 1))

	(goto-char (point-min))
	(search-forward-regexp "^-----" nil t)
	(forward-line 1)
	(recenter -1)

	(erc-prettify-channel-list)))))

(defun erc-chanlist-quit ()
  "Quit Chanlist mode.
Kill the channel list buffer, window, and frame (if there's a frame
devoted to the channel list)."
  (interactive)
  (kill-buffer (current-buffer))
  (if (eq (selected-frame) erc-chanlist-frame)
      (delete-frame)
    (delete-window)))

(defun erc-chanlist-join-channel ()
  "Join the channel listed on the current line of the channel list buffer.
Private channels, which are shown as asterisks (*), are ignored."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at erc-chanlist-channel-line-regexp)
      (let ((channel-name (match-string 1)))
	(when (and (stringp channel-name)
		   (not (string= channel-name "*")))
	  (run-at-time 0.5 nil 'erc-join-channel channel-name))))))

(provide 'erc-list-old)

;;; erc-list-old.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:

;; arch-tag: 4a13196a-a61b-465a-9926-044dfbc7e5ff
