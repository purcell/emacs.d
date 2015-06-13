;;; wl-action.el --- Mark and actions in the Summary mode for Wanderlust.

;; Copyright (C) 2003 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news

;; This file is part of Wanderlust (Yet Another Message Interface on Emacsen).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:
;;

;;; Code:
;;

(require 'wl-summary)

(eval-when-compile
  (defalias-maybe 'wl-summary-target-mark 'ignore)
  (defalias-maybe 'wl-summary-target-mark-region 'ignore))

(defsubst wl-summary-action-mark (action)
  (nth 0 action))
(defsubst wl-summary-action-symbol (action)
  (nth 1 action))
(defsubst wl-summary-action-argument-function (action)
  (nth 2 action))
(defsubst wl-summary-action-set-function (action)
  (nth 3 action))
(defsubst wl-summary-action-exec-function (action)
  (nth 4 action))
(defsubst wl-summary-action-face (action)
  (nth 5 action))
(defsubst wl-summary-action-docstring (action)
  (concat (nth 6 action)
	  "\nThis function is defined by `wl-summary-define-mark-action'."))

(defsubst wl-summary-action-unmark-docstring (action)
  (concat "Unmark `" (wl-summary-action-mark action) "' from the current line."
	  "\nIf NUMBER is non-nil, unmark the summary line specified by NUMBER."
	  "\nThis function is defined by `wl-summary-define-mark-action'."))

;; Set mark
(defun wl-summary-set-mark (&optional set-mark number interactive data)
  "Set temporary mark SET-MARK on the message with NUMBER.
NUMBER is the message number to set the mark on.
INTERACTIVE is set as t if it have to run interactively.
DATA is passed to the set-action function of the action as an argument.
Return number if put mark succeed"
  (let* ((set-mark (or set-mark
		       (completing-read "Mark: " wl-summary-mark-action-list)))
	 (current (wl-summary-message-number))
	 (action (assoc set-mark wl-summary-mark-action-list))
	 visible mark cur-mark)
    (when (zerop (elmo-folder-length wl-summary-buffer-elmo-folder))
      (error "Set mark failed"))
    (prog1
	(save-excursion
	  ;; Put mark
	  (if number
	      ;; Jump to message if cursor is not on the message.
	      (when (and (setq visible (wl-summary-message-visible-p number))
			 (not (eq number current)))
		(wl-summary-jump-to-msg number))
	    (setq visible t
		  number current))
	  (setq cur-mark (nth 1 (wl-summary-registered-temp-mark number)))
	  (unless number
	    (error "No message"))
	  (if (wl-summary-reserve-temp-mark-p cur-mark)
	      (when interactive
		(error "Already marked as `%s'" cur-mark))
	    (when (and interactive
		       (null data)
		       (wl-summary-action-argument-function action))
	      (setq data (funcall (wl-summary-action-argument-function action)
				  (wl-summary-action-symbol action)
				  number)))
	    ;; Unset the current mark.
	    (wl-summary-unset-mark number)
	    ;; Set action.
	    (funcall (wl-summary-action-set-function action)
		     number
		     (wl-summary-action-mark action)
		     data)
	    (when visible
	      (wl-summary-put-temp-mark set-mark)
	      (when wl-summary-highlight
		(wl-highlight-summary-current-line))
	      (when data
		(wl-summary-print-argument number data)))
	    (when (and (eq wl-summary-buffer-view 'thread)
		       interactive)
	      (wl-thread-open-children number))
	    (set-buffer-modified-p nil)
	    ;; Return value.
	    number))
      ;; Move the cursor.
      (if interactive
	  (if (eq wl-summary-move-direction-downward nil)
	      (wl-summary-prev)
	    (wl-summary-next))))))

(defun wl-summary-register-target-mark (number mark data)
  (or (memq number wl-summary-buffer-target-mark-list)
      (setq wl-summary-buffer-target-mark-list
	    (cons number wl-summary-buffer-target-mark-list))))

(defun wl-summary-unregister-target-mark (number)
  (setq wl-summary-buffer-target-mark-list
	(delq number wl-summary-buffer-target-mark-list)))

(defun wl-summary-have-target-mark-p (number)
  (memq number wl-summary-buffer-target-mark-list))

(defun wl-summary-target-mark-set-action (action)
  (unless (eq (wl-summary-action-symbol action) 'target-mark)
    (unless wl-summary-buffer-target-mark-list (error "no target"))
    (save-excursion
      (goto-char (point-min))
      (let ((numlist wl-summary-buffer-number-list)
	    number mlist data)
	;; use firstly marked message.
	(when (wl-summary-action-argument-function action)
	  (while numlist
	    (if (memq (car numlist) wl-summary-buffer-target-mark-list)
		(setq number (car numlist)
		      numlist nil))
	    (setq numlist (cdr numlist)))
	  (wl-summary-jump-to-msg number)
	  (setq data (funcall (wl-summary-action-argument-function action)
			      (wl-summary-action-symbol action) number)))
	(while (not (eobp))
	  (when (string= (wl-summary-temp-mark) "*")
	    (let (wl-summary-buffer-disp-msg)
	      (when (setq number (wl-summary-message-number))
		(wl-summary-set-mark (wl-summary-action-mark action)
				     nil nil data)
		(setq wl-summary-buffer-target-mark-list
		      (delq number wl-summary-buffer-target-mark-list)))))
	  (forward-line))
	(setq mlist wl-summary-buffer-target-mark-list)
	(while mlist
	  (wl-summary-register-temp-mark (car mlist)
					 (wl-summary-action-mark action) data)
	  (setq wl-summary-buffer-target-mark-list
		(delq (car mlist) wl-summary-buffer-target-mark-list))
	  (setq mlist (cdr mlist)))))))

;; wl-summary-buffer-temp-mark-list specification
;; ((1 "D" nil)(2 "o" "+fuga")(3 "O" "+hoge"))
(defun wl-summary-register-temp-mark (number mark mark-info)
  (let ((elem (assq number wl-summary-buffer-temp-mark-list)))
    (setq wl-summary-buffer-temp-mark-list
	  (delq elem wl-summary-buffer-temp-mark-list)))
  (setq wl-summary-buffer-temp-mark-list
	(cons (list number mark mark-info) wl-summary-buffer-temp-mark-list)))

(defun wl-summary-unregister-temp-mark (number)
  (let ((elem (assq number wl-summary-buffer-temp-mark-list)))
    (setq wl-summary-buffer-temp-mark-list
	  (delq elem wl-summary-buffer-temp-mark-list))))

(defun wl-summary-registered-temp-mark (number)
  (and wl-summary-buffer-temp-mark-list
       (assq number wl-summary-buffer-temp-mark-list)))

(defun wl-summary-collect-temp-mark (mark &optional begin end)
  (if (or begin end)
      (save-excursion
	(save-restriction
	  (let (mark-list)
	    (narrow-to-region (or begin (point-min))(or end (point-max)))
	    (goto-char (point-min))
	    ;; for thread...
	    (if (eq wl-summary-buffer-view 'thread)
		(let (number entity mark-info)
		  (while (not (eobp))
		    (setq number (wl-summary-message-number)
			  entity (wl-thread-get-entity number)
			  mark-info (wl-summary-registered-temp-mark number))
		    ;; toplevel message mark.
		    (when (string= (nth 1 mark-info) mark)
		      (setq mark-list (cons mark-info mark-list)))
		    ;; When thread is closed...children should also be checked.
		    (unless (wl-thread-entity-get-opened entity)
		      (dolist (msg (wl-thread-get-children-msgs number))
			(setq mark-info (wl-summary-registered-temp-mark
					 msg))
			(when (string= (nth 1 mark-info) mark)
			  (setq mark-list (cons mark-info mark-list)))))
		    (forward-line)))
	      (let (number mark-info)
		(while (not (eobp))
		  (setq number (wl-summary-message-number)
			mark-info (wl-summary-registered-temp-mark number))
		  (when (string= (nth 1 mark-info) mark)
		    (setq mark-list (cons mark-info mark-list)))
		  (forward-line))))
	    mark-list)))
    (let (mark-list)
      (dolist (mark-info wl-summary-buffer-temp-mark-list)
	(when (string= (nth 1 mark-info) mark)
	  (setq mark-list (cons mark-info mark-list))))
      mark-list)))

;; Unset mark
(defun wl-summary-unset-mark (&optional number interactive force)
  "Unset temporary mark of the message with NUMBER.
NUMBER is the message number to unset the mark.
If not specified, the message on the cursor position is treated.
Optional INTERACTIVE is non-nil when it should be called interactively.
If optional FORCE is non-nil, remove scored mark too.
Return number if put mark succeed"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((buffer-read-only nil)
	  visible mark action)
      (if number
	  ;; Jump to message
	  (when (and (setq visible (wl-summary-message-visible-p number))
		     (not (eq number (wl-summary-message-number))))
	    (wl-summary-jump-to-msg number))
	(setq visible t
	      number (wl-summary-message-number)))
      (setq mark (wl-summary-temp-mark))
      ;; Remove from temporal mark structure.
      (wl-summary-unregister-target-mark number)
      (wl-summary-unregister-temp-mark number)
      ;; Delete mark on buffer.
      (when visible
	(unless (string= mark " ")
	  (wl-summary-put-temp-mark
	   (or (unless force (wl-summary-get-score-mark number))
	       " "))
	  (setq action (assoc mark wl-summary-mark-action-list))
	  (when wl-summary-highlight
	    (wl-highlight-summary-current-line))
	  (when (wl-summary-action-argument-function action)
	    (wl-summary-remove-argument)))
	(set-buffer-modified-p nil))))
;;; Move the cursor.
;;;  (if (or interactive (interactive-p))
;;;      (if (eq wl-summary-move-direction-downward nil)
;;;	  (wl-summary-prev)
;;;	(wl-summary-next))))
  )

(defun wl-summary-make-destination-numbers-list (mark-list)
  (let (dest-numbers dest-number)
    (dolist (elem mark-list)
      (setq dest-number (assoc (nth 2 elem) dest-numbers))
      (if dest-number
	  (unless (memq (car elem) (cdr dest-number))
	    (nconc dest-number (list (car elem))))
	(setq dest-numbers (nconc dest-numbers
				  (list
				   (list (nth 2 elem)
					 (car elem)))))))
    dest-numbers))

(defun wl-summary-move-mark-list-messages (mark-list folder-name message)
  (if (null mark-list)
      (message "No marks")
    (save-excursion
      (let ((start (point))
	    (refiles (mapcar 'car mark-list))
	    (refile-failures 0)
	    dst-msgs			; loop counter
	    result)
	;; begin refile...
	(goto-char start)		; avoid moving cursor to
					; the bottom line.
	(elmo-with-progress-display
	    (elmo-folder-move-messages (length refiles))
	    message
	  (setq result nil)
	  (condition-case nil
	      (setq result (elmo-folder-move-messages
			    wl-summary-buffer-elmo-folder
			    refiles
			    (if (eq folder-name 'null)
				'null
			      (wl-folder-get-elmo-folder folder-name))))
	    (error nil))
	  (when result		; succeeded.
	    ;; update buffer.
	    (wl-summary-delete-messages-on-buffer refiles)
	    ;; update wl-summary-buffer-temp-mark-list.
	    (dolist (mark-info mark-list)
	      (setq wl-summary-buffer-temp-mark-list
		    (delq mark-info wl-summary-buffer-temp-mark-list)))))
	(wl-summary-set-message-modified)
	;; Return the operation failed message numbers.
	(if result
	    0
	  (length refiles))))))

(defun wl-summary-get-refile-destination-subr (action number learn)
  (let* ((number (or number (wl-summary-message-number)))
	 (msgid (and number
		     (elmo-message-field wl-summary-buffer-elmo-folder
					 number 'message-id)))
	 (entity (and number
		      (elmo-message-entity wl-summary-buffer-elmo-folder
					   number)))
	 folder cur-mark tmp-folder)
    (catch 'done
      (when (null entity)
	(message "Cannot decide destination.")
	(throw 'done nil))
      (when (null number)
	(message "No message.")
	(throw 'done nil))
      (setq folder (wl-summary-read-folder
		    (or (wl-refile-guess entity) wl-trash-folder)
		    (format "for %s " action)))
      ;; Cache folder hack by okada@opaopa.org
      (when (and (eq (elmo-folder-type-internal
		      (wl-folder-get-elmo-folder
		       (wl-folder-get-realname folder))) 'cache)
		 (not (string= folder
			       (setq tmp-folder
				     (concat "'cache/"
					     (elmo-cache-get-path-subr
					      (elmo-msgid-to-cache msgid)))))))
	(setq folder tmp-folder)
	(message "Force refile to %s." folder))
      (if (string= folder (wl-summary-buffer-folder-name))
	  (error "Same folder"))
      (if (or (not (elmo-folder-writable-p (wl-folder-get-elmo-folder folder)))
	      (string= folder wl-queue-folder)
	      (string= folder wl-draft-folder))
	  (error "Don't set as target: %s" folder))
      ;; learn for refile.
      (when learn
	(wl-refile-learn entity folder))
      folder)))

;;; Actions
(defun wl-summary-define-mark-action ()
  (interactive)
  (dolist (action wl-summary-mark-action-list)
    (fset (intern (format "wl-summary-%s" (wl-summary-action-symbol action)))
	  `(lambda (&optional number data)
	     ,(wl-summary-action-docstring action)
	     (interactive)
	     (wl-summary-set-mark ,(wl-summary-action-mark action)
				  number (interactive-p) data)))
    (fset (intern (format "wl-summary-unmark-%s"
			  (wl-summary-action-symbol action)))
	  `(lambda (&optional number)
	     ,(wl-summary-action-unmark-docstring action)
	     (interactive)
	     (wl-summary-unmark number ,(wl-summary-action-mark action))))
    (fset (intern (format "wl-summary-%s-region"
			  (wl-summary-action-symbol action)))
	  `(lambda (beg end)
	     ,(wl-summary-action-docstring action)
	     (interactive "r")
	     (save-excursion
	       (goto-char beg)
	       (wl-summary-mark-region-subr
		(quote ,(intern (format "wl-summary-%s"
					(wl-summary-action-symbol action))))
		beg end
		(if (quote ,(wl-summary-action-argument-function action))
		    (funcall (function
			      ,(wl-summary-action-argument-function action))
			     (quote ,(wl-summary-action-symbol action))
			     (wl-summary-message-number)))))))
    (fset (intern (format "wl-summary-target-mark-%s"
			  (wl-summary-action-symbol action)))
	  `(lambda ()
	     ,(wl-summary-action-docstring action)
	     (interactive)
	     (wl-summary-target-mark-set-action (quote ,action))))
    (fset (intern (format "wl-thread-%s"
			  (wl-summary-action-symbol action)))
	  `(lambda (arg)
	     ,(wl-summary-action-docstring action)
	     (interactive "P")
	     (wl-thread-call-region-func
	      (quote ,(intern (format "wl-summary-%s-region"
				      (wl-summary-action-symbol action))))
	      arg)
	     (if arg
		 (wl-summary-goto-top-of-current-thread))
	     (if (not wl-summary-move-direction-downward)
		 (wl-summary-prev)
	       (wl-thread-goto-bottom-of-sub-thread)
	       (if wl-summary-buffer-disp-msg
		   (wl-summary-redisplay)))))))

(defun wl-summary-get-dispose-folder (folder)
  (if (string= folder wl-trash-folder)
      'null
    (let* ((type (or (wl-get-assoc-list-value wl-dispose-folder-alist folder)
		     'trash)))
      (cond ((stringp type)
	     type)
	    ((memq type '(remove null))
	     'null)
	    (t;; (equal type 'trash)
	     (let ((trash-folder (wl-folder-get-elmo-folder wl-trash-folder)))
	       (unless (elmo-folder-exists-p trash-folder)
		 (if (y-or-n-p
		      (format "Trash Folder %s does not exist, create it? "
			      wl-trash-folder))
		     (elmo-folder-create trash-folder)
		   (error "Trash Folder is not created"))))
	     wl-trash-folder)))))

;; Dispose action.
(defun wl-summary-exec-action-dispose (mark-list)
  (wl-summary-move-mark-list-messages mark-list
				      (wl-summary-get-dispose-folder
				       (wl-summary-buffer-folder-name))
				      "Disposing messages"))

;; Delete action.
(defun wl-summary-exec-action-delete (mark-list)
  (wl-summary-move-mark-list-messages mark-list
				      'null
				      "Deleting messages"))

;; Refile action
(defun wl-summary-set-action-refile (number mark data)
  (when (null data)
    (error "Destination folder is empty"))
  (wl-summary-register-temp-mark number mark data)
  (setq wl-summary-buffer-prev-refile-destination data))

(defun wl-summary-get-refile-destination (action number)
  "Decide refile destination."
  (wl-summary-get-refile-destination-subr action number t))

(defun wl-summary-exec-action-refile (mark-list)
  (save-excursion
    (let ((start (point))
	  (failures 0)
	  dst-msgs)
      ;; begin refile...
      (setq dst-msgs (wl-summary-make-destination-numbers-list mark-list))
      (goto-char start)	; avoid moving cursor to the bottom line.
      (elmo-with-progress-display
	  (elmo-folder-move-messages (length mark-list))
	  "Refiling messages"
	(dolist (pair dst-msgs)
	  (if (condition-case nil
		  (elmo-folder-move-messages
		   wl-summary-buffer-elmo-folder
		   (cdr pair)
		   (wl-folder-get-elmo-folder (car pair)))
		(error nil))
	      (progn
		;; update buffer.
		(wl-summary-delete-messages-on-buffer (cdr pair))
		(setq wl-summary-buffer-temp-mark-list
		      (wl-delete-associations
		       (cdr pair)
		       wl-summary-buffer-temp-mark-list)))
	    (setq failures (+ failures (length (cdr pair)))))))
      failures)))

;; Copy action
(defun wl-summary-get-copy-destination (action number)
  (wl-summary-get-refile-destination-subr action number nil))

(defun wl-summary-exec-action-copy (mark-list)
  (save-excursion
    (let ((start (point))
	  (failures 0)
	  dst-msgs)
      ;; begin refile...
      (setq dst-msgs
	    (wl-summary-make-destination-numbers-list mark-list))
      (goto-char start)	; avoid moving cursor to the bottom line.
      (elmo-with-progress-display
	  (elmo-folder-move-messages (length mark-list))
	  "Copying messages"
	(dolist (pair dst-msgs)
	  (if (condition-case nil
		  (elmo-folder-move-messages
		   wl-summary-buffer-elmo-folder
		   (cdr pair)
		   (wl-folder-get-elmo-folder (car pair))
		   'no-delete)
		(error nil))
	      (progn
		;; update buffer.
		(wl-summary-delete-copy-marks-on-buffer (cdr pair))
		(setq wl-summary-buffer-temp-mark-list
		      (wl-delete-associations
		       (cdr pair)
		       wl-summary-buffer-temp-mark-list)))
	    (setq failures (+ failures (length (cdr pair)))))))
      failures)))

;; Prefetch.
(defun wl-summary-exec-action-prefetch (mark-list)
  (save-excursion
    (let* ((count 0)
	   (length (length mark-list))
	   (mark-list-copy (copy-sequence mark-list))
	   (pos (point))
	   (failures 0))
      (dolist (mark-info mark-list-copy)
	(message "Prefetching...(%d/%d)"
		 (setq count (+ 1 count)) length)
	(if (wl-summary-prefetch-msg (car mark-info))
	    (progn
	      (wl-summary-unset-mark (car mark-info))
	      (sit-for 0))
	  (incf failures)))
      (message "Prefetching...done")
      0)))

;; Resend.
(defun wl-summary-get-resend-address (action number)
  "Decide resend address."
  (wl-address-read-from-minibuffer "Resend message to: "))

(defun wl-summary-exec-action-resend (mark-list)
  (let ((failure 0))
    (dolist (mark-info mark-list)
      (if (condition-case nil
	      (progn
		(wl-summary-exec-action-resend-subr (car mark-info)
						    (nth 2 mark-info))
		t)
	    (error))
	  (wl-summary-unmark (car mark-info))
	(incf failure)))
    failure))

(defun wl-summary-exec-action-resend-subr (number address)
  "Resend the message with NUMBER to ADDRESS."
  (message "Resending message to %s..." address)
  (let ((folder wl-summary-buffer-elmo-folder))
    (with-current-buffer (get-buffer-create " *wl-draft-resend*")
      ;; We first set up a normal mail buffer.
      (set-buffer-multibyte nil)
      (erase-buffer)
      (setq wl-sent-message-via nil)
      ;; Insert our usual headers.
      (wl-draft-insert-from-field)
      (wl-draft-insert-date-field)
      (insert "To: " address "\n")
      (goto-char (point-min))
      ;; Rename them all to "Resent-*".
      (while (re-search-forward "^[A-Za-z]" nil t)
	(backward-char)
	(insert "Resent-"))
      (widen)
      (forward-line)
      (delete-region (point) (point-max))
      (let ((beg (point)))
	;; Insert the message to be resent.
	(insert
	 ;; elmo-message-fetch is erase current buffer before fetch message
	 (elmo-message-fetch-string folder number
				    (if wl-summary-resend-use-cache
					(elmo-make-fetch-strategy
					 'entire 'maybe nil
					 (elmo-file-cache-get-path
					  (elmo-message-field
					   folder number 'message-id)))
				      (elmo-make-fetch-strategy 'entire))
				    'unread))
	(goto-char (point-min))
	(search-forward "\n\n")
	(backward-char)
	(save-restriction
	  (narrow-to-region beg (point))
	  (wl-draft-delete-fields wl-ignored-resent-headers)
	  (goto-char (point-max)))
	(insert mail-header-separator)
	;; Rename all old ("Previous-")Resent headers.
	(while (re-search-backward "^\\(Previous-\\)*Resent-" beg t)
	  (beginning-of-line)
	  (insert "Previous-"))
	;; Quote any "From " lines at the beginning.
	(goto-char beg)
	(when (looking-at "From ")
	  (replace-match "X-From-Line: ")))
      (run-hooks 'wl-summary-resend-hook)
      ;; Send it.
      (wl-draft-dispatch-message)
      (kill-buffer (current-buffer)))
    (message "Resending message to %s...done" address)))

;;;
(defun wl-summary-remove-argument ()
  (save-excursion
    (let ((inhibit-read-only t)
	  (buffer-read-only nil)
	  (buf (current-buffer))
	  sol eol rs re)
      (setq sol (point-at-bol))
      (beginning-of-line)
      (search-forward "\r")
      (backward-char)
      (setq eol (point))
      (setq rs (next-single-property-change sol 'wl-summary-action-argument
					    buf eol))
      (setq re (next-single-property-change rs 'wl-summary-action-argument
					    buf eol))
      (put-text-property rs re 'wl-summary-action-argument nil)
      (put-text-property rs re 'invisible nil)
      (goto-char re)
      (delete-char (- eol re)))))

(defun wl-summary-collect-numbers-region (begin end)
  "Return a list of message number in the region specified by BEGIN and END."
  (save-excursion
    (save-restriction
      (let (numbers)
	(wl-summary-narrow-to-region (or begin (point-min))(or end (point-max)))
	(goto-char (point-min))
	;; for thread...
	(if (eq wl-summary-buffer-view 'thread)
	    (let (number entity)
	      (while (not (eobp))
		(setq numbers (cons (wl-summary-message-number) numbers)
		      entity (wl-thread-get-entity number))
		;; When thread is closed...children should also be checked.
		(unless (wl-thread-entity-get-opened entity)
		  (dolist (msg (wl-thread-get-children-msgs number))
		    (setq numbers (cons msg numbers))))
		(forward-line)))
	  (let (number)
	    (while (not (eobp))
	      (setq numbers (cons (wl-summary-message-number) numbers))
	      (forward-line))))
	(nreverse (delq nil numbers))))))

(defun wl-summary-exec (&optional numbers)
  (interactive)
  (let ((failures 0)
	collected pair action modified)
    (dolist (action wl-summary-mark-action-list)
      (setq collected (cons (cons
			     (wl-summary-action-mark action)
			     nil) collected)))
    (dolist (mark-info wl-summary-buffer-temp-mark-list)
      (setq pair
	    (when (or (null numbers)
		      (memq (nth 0 mark-info) numbers))
	      (assoc (nth 1 mark-info) collected)))
      (if pair
	  (setcdr pair (cons mark-info (cdr pair)))))
    ;; collected is a pair of
    ;; mark-string and a list of mark-info
    (dolist (pair collected)
      (when (cdr pair)
	(setq action (assoc (car pair) wl-summary-mark-action-list))
	(when (wl-summary-action-exec-function action)
	  (setq modified t)
	  (setq failures (+ failures (funcall
				      (wl-summary-action-exec-function action)
				      (cdr pair)))))))
    (when modified
      (wl-summary-set-message-modified))
    (run-hooks 'wl-summary-exec-hook)
    ;; message buffer is not up-to-date
    (unless (and wl-message-buffer
		 (eq (wl-summary-message-number)
		     (with-current-buffer wl-message-buffer
		       wl-message-buffer-cur-number)))
      (wl-summary-toggle-disp-msg 'off)
      (setq wl-message-buffer nil))
    (set-buffer-modified-p nil)
    (when (> failures 0)
      (message "%d execution(s) were failed" failures))))

(defun wl-summary-exec-region (beg end)
  (interactive "r")
  (wl-summary-exec
   (wl-summary-collect-numbers-region beg end)))

(defun wl-summary-read-folder (default &optional purpose ignore-error
				no-create init)
  (let ((fld (completing-read
	      (format "Folder name %s(%s): " (or purpose "")
		      default)
	      'wl-folder-complete-folder
	      nil nil (or init wl-default-spec)
	      'wl-read-folder-history default)))
    (when (string= fld wl-default-spec)
      (setq fld default))
    (setq fld (elmo-string (wl-folder-get-realname fld)))
    (if (string-match "\n" fld)
	(error "Not supported folder name: %s" fld))
    (unless no-create
      (if ignore-error
	  (condition-case nil
	      (wl-folder-confirm-existence
	       (wl-folder-get-elmo-folder
		fld))
	    (error))
	(wl-folder-confirm-existence (wl-folder-get-elmo-folder
				      fld))))
    fld))

(defun wl-summary-print-argument (msg-num data)
  "Print action argument on line."
  (when data
    (wl-summary-remove-argument)
    (save-excursion
      (let ((inhibit-read-only t)
	    (data (copy-sequence data))
	    (buffer-read-only nil)
	    len rs re c)
	(setq len (string-width data))
	(if (< len 1) ()
	  ;;(end-of-line)
	  (beginning-of-line)
	  (search-forward "\r")
	  (backward-char)
	  (setq re (point))
	  (let ((width (cond (wl-summary-width
			      (1- wl-summary-width))
			     (wl-summary-print-argument-within-window
			      (1- (window-width)))))
		(c (current-column))
		(padding 0))
	    (if (and width
		     (> (+ c len) width))
		(progn
		  (move-to-column width)
		  (setq c (current-column))
		  (while (> (+ c len) width)
		    (backward-char)
		    (setq c (current-column)))
		  (when (< (+ c len) width)
		    (setq data (concat " " data)))
		  (setq rs (point))
		  (put-text-property rs re 'invisible t))
	      (when (and width
			 (> (setq padding (- width len c)) 0))
		(setq data (concat (make-string padding (string-to-char " "))
				   data)))
	      (setq rs (1- re))))
	  (put-text-property rs re 'wl-summary-action-argument t)
	  (goto-char re)
	  (wl-highlight-action-argument-string data)
	  (insert data)
	  (set-buffer-modified-p nil))))))

(defsubst wl-summary-reserve-temp-mark-p (mark)
  "Return t if temporal MARK should be reserved."
  (member mark wl-summary-reserve-mark-list))

;; Refile prev destination
(defun wl-summary-refile-prev-destination ()
  "Refile message to previously refiled destination."
  (interactive)
  (funcall (symbol-function 'wl-summary-refile)
	   (wl-summary-message-number)
	   wl-summary-buffer-prev-refile-destination)
  (if (and (interactive-p)
	   (eq wl-summary-move-direction-downward nil))
      (wl-summary-prev)
    (wl-summary-next)))

(defun wl-summary-refile-prev-destination-region (beg end)
  "Refile messages in the region to previously refiled destination."
  (interactive "r")
  (wl-summary-mark-region-subr 'wl-summary-refile
			       beg end
			       wl-summary-buffer-prev-refile-destination))

(defun wl-thread-refile-prev-destination (arg)
  "Refile messages in the thread to previously refiled destination."
  (interactive "P")
  (wl-thread-call-region-func
   'wl-summary-refile-prev-destination-region
   arg))

(defun wl-summary-target-mark-refile-prev-destination ()
  "Refile messages with target mark to previously refiled destination."
  (interactive)
  (let ((elem wl-summary-mark-action-list)
	action)
    (while elem
      (when (eq (wl-summary-action-symbol (car elem)) 'refile)
	(setq action (car elem))
	(setq elem nil))
      (setq elem (cdr elem)))
    (wl-summary-target-mark-set-action
     (list
      (car action)
      'refile-prev-destination
      (lambda (&rest args) wl-summary-buffer-prev-refile-destination)
      (nth 2 action)
      (nth 3 action)
      (nth 4 action)
      (nth 6 action)))))

(defsubst wl-summary-no-auto-refile-message-p (number)
  (member (wl-summary-message-mark wl-summary-buffer-elmo-folder number)
	  wl-summary-auto-refile-skip-marks))

(defvar wl-auto-refile-guess-functions
  '(wl-refile-guess-by-rule)
  "*List of functions which is used for guessing refile destination folder.")

(defun wl-summary-auto-refile (&optional open-all)
  "Set refile mark automatically according to 'wl-refile-guess-by-rule'."
  (interactive "P")
  (message "Marking...")
  (save-excursion
    (if (and (eq wl-summary-buffer-view 'thread)
	     open-all)
	(wl-thread-open-all))
    (let* ((spec (wl-summary-buffer-folder-name))
	   checked-dsts
	   (count 0)
	   number dst thr-entity)
      (goto-char (point-min))
      (while (not (eobp))
	(setq number (wl-summary-message-number))
	(dolist (number (cons number
			      (and (eq wl-summary-buffer-view 'thread)
				   ;; process invisible children.
				   (not (wl-thread-entity-get-opened
					 (setq thr-entity
					       (wl-thread-get-entity number))))
				   (wl-thread-entity-get-descendant
				    thr-entity))))
	  (when (and (not (wl-summary-no-auto-refile-message-p number))
		     (not (wl-summary-reserve-temp-mark-p
			   (nth 1 (wl-summary-registered-temp-mark number))))
		     (setq dst
			   (wl-folder-get-realname
			    (wl-refile-guess
			     (elmo-message-entity wl-summary-buffer-elmo-folder
						  number)
			     wl-auto-refile-guess-functions)))
		     (not (equal dst spec))
		     (let ((pair (assoc dst checked-dsts))
			   ret)
		       (if pair
			   (cdr pair)
			 (setq ret
			       (condition-case nil
				   (progn
				     (wl-folder-confirm-existence
				      (wl-folder-get-elmo-folder dst))
				     t)
				 (error)))
			 (setq checked-dsts (cons (cons dst ret) checked-dsts))
			 ret)))
	    (if (funcall (symbol-function 'wl-summary-refile) number dst)
		(incf count))
	    (message "Marking...%d message(s)." count)))
	(forward-line))
      (if (zerop count)
	  (message "No message was marked.")
	(message "Marked %d message(s)." count)))))

(defun wl-summary-unmark (&optional number mark)
  "Unmark temporary marks of the current line.
If NUMBER is non-nil, remove the mark of the summary line specified by NUMBER.
If MARK is non-nil, remove only the specified MARK from the summary line."
  (interactive)
  (if (or (null mark)
	  (string= mark (wl-summary-temp-mark number)))
      (wl-summary-unset-mark number (interactive-p))))

(defun wl-summary-unmark-region (beg end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (wl-summary-narrow-to-region beg end)
      (goto-char (point-min))
      (if (eq wl-summary-buffer-view 'thread)
	  (progn
	    (while (not (eobp))
	      (let* ((number (wl-summary-message-number))
		     (entity (wl-thread-get-entity number)))
		(if (wl-thread-entity-get-opened entity)
		    ;; opened...unmark line.
		    (wl-summary-unmark)
		  ;; closed
		  (wl-summary-delete-marks-on-buffer
		   (wl-thread-get-children-msgs number))))
	      (forward-line)))
	(while (not (eobp))
	  (wl-summary-unmark)
	  (forward-line))))))

(defun wl-summary-mark-region-subr (function beg end data)
  (save-excursion
    (save-restriction
      (wl-summary-narrow-to-region beg end)
      (goto-char (point-min))
      (if (eq wl-summary-buffer-view 'thread)
	  (progn
	    (while (not (eobp))
	      (let* ((number (wl-summary-message-number))
		     (entity (wl-thread-get-entity number))
		     (wl-summary-move-direction-downward t)
		     children)
		(if (wl-thread-entity-get-opened entity)
		    ;; opened...delete line.
		    (funcall function nil data)
		  ;; closed
		  (setq children (wl-thread-get-children-msgs number))
		  (while children
		    (funcall function (pop children) data)))
		(forward-line))))
	(while (not (eobp))
	  (funcall function nil data)
	  (forward-line))))))

(defun wl-summary-target-mark-all ()
  (interactive)
  (wl-summary-target-mark-region (point-min) (point-max)))

(defun wl-summary-delete-all-mark (mark)
  (goto-char (point-min))
  (while (not (eobp))
    (wl-summary-unmark nil mark)
    (forward-line))
  (if (string= mark "*")
      (setq wl-summary-buffer-target-mark-list nil)
    (let (deleted)
      (dolist (mark-info wl-summary-buffer-temp-mark-list)
	(when (string= (nth 1 mark-info) mark)
	  (setq deleted (cons mark-info deleted))))
      (dolist (delete deleted)
	(setq wl-summary-buffer-temp-mark-list
	      (delq delete wl-summary-buffer-temp-mark-list))))))

(defun wl-summary-unmark-all ()
  "Unmark all according to what you input."
  (interactive)
  (let ((unmarks (string-to-char-list (read-from-minibuffer "Unmark: ")))
	cur-mark)
    (save-excursion
      (while unmarks
	(setq cur-mark (char-to-string (car unmarks)))
	(wl-summary-delete-all-mark cur-mark)
	(setq unmarks (cdr unmarks))))))

(defun wl-summary-target-mark-thread ()
  (interactive)
  (wl-thread-call-region-func 'wl-summary-target-mark-region t))

(require 'product)
(product-provide (provide 'wl-action) (require 'wl-version))

;;; wl-action.el ends here
