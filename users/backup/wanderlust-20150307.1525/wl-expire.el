;;; wl-expire.el --- Message expire modules for Wanderlust.

;; Copyright (C) 1998,1999,2000 Masahiro MURATA <muse@ba2.so-net.ne.jp>
;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Masahiro MURATA <muse@ba2.so-net.ne.jp>
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

(require 'wl-summary)
(require 'wl-thread)
(require 'wl-folder)
(require 'elmo)

(eval-when-compile
  (require 'wl-util)
  (require 'elmo-archive))

;; Variables

(defvar wl-expired-alist nil)
(defvar wl-expired-alist-file-name "expired-alist")
(defvar wl-expired-log-alist nil)
(defvar wl-expired-log-alist-file-name "expired-log")
(defvar wl-expire-test nil)	;; for debug (no execute)

(defun wl-expired-alist-load ()
  (elmo-object-load (expand-file-name
		     wl-expired-alist-file-name
		     elmo-msgdb-directory)))

(defun wl-expired-alist-save (&optional alist)
  (elmo-object-save (expand-file-name
		     wl-expired-alist-file-name
		     elmo-msgdb-directory)
		    (or alist wl-expired-alist)))

(defsubst wl-expire-msg-p (msg-num mark-alist)
  (cond ((consp wl-summary-expire-reserve-marks)
	 (let ((mark (nth 1 (assq msg-num mark-alist))))
	   (not (or (member mark wl-summary-expire-reserve-marks)
		    (and wl-summary-buffer-disp-msg
			 (eq msg-num wl-summary-buffer-current-msg))))))
	((eq wl-summary-expire-reserve-marks 'all)
	 (not (or (assq msg-num mark-alist)
		  (and wl-summary-buffer-disp-msg
		       (eq msg-num wl-summary-buffer-current-msg)))))
	((eq wl-summary-expire-reserve-marks 'none)
	 t)
	(t
	 (error "Invalid marks: %s" wl-summary-expire-reserve-marks))))

(defsubst wl-expire-make-sortable-date (date)
  (timezone-make-sortable-date
   (aref date 0) (aref date 1) (aref date 2)
   (timezone-make-time-string
    (aref date 3) (aref date 4) (aref date 5))))

;; New functions to avoid accessing to the msgdb directly.
(defsubst wl-expire-message-p (folder number)
  "Return non-nil when a message in the FOLDER with NUMBER can be expired."
  (cond ((consp wl-summary-expire-reserve-marks)
	 (let ((mark (wl-summary-message-mark folder number)))
	   (not (or (member mark wl-summary-expire-reserve-marks)
		    (and wl-summary-buffer-disp-msg
			 (eq number wl-summary-buffer-current-msg))))))
	((eq wl-summary-expire-reserve-marks 'all)
	 (not (or (wl-summary-message-mark folder number)
		  (and wl-summary-buffer-disp-msg
		       (eq number wl-summary-buffer-current-msg)))))
	((eq wl-summary-expire-reserve-marks 'none)
	 t)
	(t
	 (error "Invalid marks: %s" wl-summary-expire-reserve-marks))))

(defun wl-expire-delete-reserved-messages (msgs folder)
  "Delete a number from NUMBERS when a message with the number is reserved."
  (let ((dlist msgs))
    (while dlist
      (unless (wl-expire-message-p folder (car dlist))
	(setq msgs (delq (car dlist) msgs)))
      (setq dlist (cdr dlist)))
    msgs))
;; End New functions.

(defun wl-expire-delete (folder delete-list &optional no-reserve-marks)
  "Delete message for expire."
  (unless no-reserve-marks
    (setq delete-list
	  (wl-expire-delete-reserved-messages delete-list folder)))
  (when delete-list
    (let ((mess
	   (format "Expiring (delete) %s msgs..."
		   (length delete-list))))
      (message "%s" mess)
      (if (elmo-folder-move-messages folder delete-list 'null)
	  (progn
	    (wl-expire-append-log
	     (elmo-folder-name-internal folder)
	     delete-list nil 'delete)
	    (message "%sdone" mess))
	(error "%sfailed!" mess))))
  (cons delete-list (length delete-list)))

(defun wl-expire-refile (folder refile-list dst-folder
				&optional no-reserve-marks preserve-number copy)
  "Refile message for expire. If COPY is non-nil, copy message."
  (when (not (string= (elmo-folder-name-internal folder) dst-folder))
    (unless no-reserve-marks
      (setq refile-list
	    (wl-expire-delete-reserved-messages refile-list folder)))
    (when refile-list
      (let* ((dst-name dst-folder)
	     (dst-folder (wl-folder-get-elmo-folder dst-folder))
	     (action (format (if copy "Copying to %s" "Expiring (move to %s)")
			     dst-name)))
	(elmo-with-progress-display
	    (elmo-folder-move-messages (length refile-list))
	    action
	  (if wl-expire-test
	      nil
	    (unless (or (elmo-folder-exists-p dst-folder)
			(elmo-folder-create dst-folder))
	      (error "Create folder failed: %s" dst-name))
	    (unless (elmo-folder-move-messages folder
					       refile-list
					       dst-folder
					       copy
					       preserve-number)
	      (error "%s is failed" action))
	    (wl-expire-append-log
	     (elmo-folder-name-internal folder)
	     refile-list
	     dst-name
	     (if copy 'copy 'move))))))
    (cons refile-list (length refile-list))))

(defun wl-expire-refile-with-copy-reserve-msg
  (folder refile-list dst-folder
	  &optional no-reserve-marks preserve-number copy)
  "Refile message for expire.
If REFILE-LIST includes reserve mark message, so copy."
  (when (not (string= (elmo-folder-name-internal folder) dst-folder))
    (let ((msglist refile-list)
	  (dst-folder (wl-folder-get-elmo-folder dst-folder))
	  (ret-val t)
	  (copy-reserve-message)
	  (copy-len 0)
	  msg msg-id)
      (message "Expiring (move %s) %s msgs..."
	       (elmo-folder-name-internal dst-folder) (length refile-list))
      (if wl-expire-test
	  (setq copy-len (length refile-list))
	(unless (or (elmo-folder-exists-p dst-folder)
		  (elmo-folder-create dst-folder))
	(error "%s: create folder failed" (elmo-folder-name-internal
					   dst-folder)))
	(while (setq msg (wl-pop msglist))
	  (unless (wl-expire-message-p folder msg)
	    (setq msg-id (elmo-message-field folder msg 'message-id))
	    (if (assoc msg-id wl-expired-alist)
		;; reserve mark message already refiled or expired
		(setq refile-list (delq msg refile-list))
	      ;; reserve mark message not refiled
	      (wl-append wl-expired-alist (list
					   (cons msg-id
						 (elmo-folder-name-internal
						  dst-folder))))
	      (setq copy-reserve-message t))))
	(when refile-list
	  (unless
	      (setq ret-val
		    (elmo-folder-move-messages folder
					       refile-list
					       dst-folder
					       copy-reserve-message
					       preserve-number))
	    (error "Expire: move msgs to %s failed"
		   (elmo-folder-name-internal dst-folder)))
	  (wl-expire-append-log (elmo-folder-name-internal folder)
				refile-list
				(elmo-folder-name-internal dst-folder)
				(if copy-reserve-message 'copy 'move))
	  (setq copy-len (length refile-list))
	  (when copy-reserve-message
	    (setq refile-list
		  (wl-expire-delete-reserved-messages refile-list folder))
	    (when refile-list
	      (if (setq ret-val
			(elmo-folder-move-messages folder refile-list 'null))
		  (progn
		    (wl-expire-append-log
		     (elmo-folder-name-internal folder)
		     refile-list nil 'delete))))))
	(let ((mes (format "Expiring (move %s) %s msgs..."
			   (elmo-folder-name-internal dst-folder)
			   (length refile-list))))
	  (if ret-val
	      (message "%sdone" mes)
	    (error "%sfailed!" mes))))
      (cons refile-list copy-len))))

(defun wl-expire-archive-get-folder (src-folder &optional fmt dst-folder-arg)
  "Get archive folder name from SRC-FOLDER."
  (let* ((fmt (or fmt wl-expire-archive-folder-name-fmt))
	 (src-folde-name (substring
			  (elmo-folder-name-internal src-folder)
			  (length (elmo-folder-prefix-internal src-folder))))
	 (archive-spec (char-to-string
			(car (rassq 'archive elmo-folder-type-alist))))
	 dst-folder-base dst-folder-fmt prefix)
    (cond (dst-folder-arg
	   (setq dst-folder-base (concat archive-spec dst-folder-arg)))
	  ((eq (elmo-folder-type-internal src-folder) 'localdir)
	   (setq dst-folder-base
		 (concat archive-spec src-folde-name)))
	  (t
	   (setq dst-folder-base
		 (elmo-concat-path
		  (format "%s%s" archive-spec (elmo-folder-type-internal
					       src-folder))
		  src-folde-name))))
    (setq dst-folder-fmt (format fmt
				 dst-folder-base
				 wl-expire-archive-folder-type))
    (setq dst-folder-base (format "%s;%s"
				  dst-folder-base
				  wl-expire-archive-folder-type))
    (when wl-expire-archive-folder-prefix
      (cond ((eq wl-expire-archive-folder-prefix 'short)
	     (setq prefix (file-name-nondirectory
			   src-folde-name)))
	    (t
	     (setq prefix src-folde-name)))
      (setq dst-folder-fmt (concat dst-folder-fmt ";" prefix))
      (setq dst-folder-base (concat dst-folder-base ";" prefix)))
    (cons dst-folder-base dst-folder-fmt)))

(defsubst wl-expire-archive-get-max-number (dst-folder-base &optional regexp)
  (let ((files (reverse (sort (elmo-folder-list-subfolders
			       (elmo-make-folder dst-folder-base))
			      'string<)))
	(regexp (or regexp wl-expire-archive-folder-num-regexp))
	filenum in-folder)
    (catch 'done
      (while files
	(when (string-match regexp (car files))
	  (setq filenum (match-string 1 (car files)))
	  (setq in-folder (elmo-folder-status
			   (wl-folder-get-elmo-folder (car files))))
	  (throw 'done (cons in-folder filenum)))
	(setq files (cdr files))))))

(defun wl-expire-archive-number-delete-old (dst-folder-base
					    preserve-number msgs folder
					    &optional no-confirm regexp file)
  (let ((len 0) (max-num 0)
	folder-info dels)
    (if (or (and file (setq folder-info
			    (cons (elmo-folder-status
				   (wl-folder-get-elmo-folder file))
				  nil)))
	    (setq folder-info (wl-expire-archive-get-max-number
			       dst-folder-base
			       regexp)))
	(progn
	  (setq len (cdar folder-info))
	  (when preserve-number
	    ;; delete small number than max number of dst-folder
	    (setq max-num (caar folder-info))
	    (while (and msgs (>= max-num (car msgs)))
	      (wl-append dels (list (car msgs)))
	      (setq msgs (cdr msgs)))
	    (setq dels (wl-expire-delete-reserved-messages dels folder))
	    (unless (and dels
			 (or (or no-confirm (not
					     wl-expire-delete-oldmsg-confirm))
			     (progn
			       (if (eq major-mode 'wl-summary-mode)
				   (wl-thread-jump-to-msg (car dels)))
			       (y-or-n-p (format "Delete old messages %s? "
						 dels)))))
	      (setq dels nil)))
	  (list msgs dels max-num (cdr folder-info) len))
      (list msgs dels 0 "0" 0))))

(defun wl-expire-archive-number1 (folder delete-list
				  &optional preserve-number dst-folder-arg
					    no-delete)
  "Standard function for `wl-summary-expire'.
Refile to archive folder followed message number."
  (let* ((elmo-archive-treat-file t)	;; treat archive folder as a file.
	 (dst-folder-expand (and dst-folder-arg
				 (wl-expand-newtext
				  dst-folder-arg
				  (elmo-folder-name-internal folder))))
	 (dst-folder-fmt (funcall
			  wl-expire-archive-get-folder-function
			  folder nil dst-folder-expand))
	 (dst-folder-base (car dst-folder-fmt))
	 (dst-folder-fmt (cdr dst-folder-fmt))
	 (refile-func (if no-delete
			  'wl-expire-refile
			'wl-expire-refile-with-copy-reserve-msg))
	 tmp dels dst-folder
	 prev-arcnum arcnum msg arcmsg-list
	 deleted-list ret-val)
    (setq tmp (wl-expire-archive-number-delete-old
	       dst-folder-base preserve-number delete-list
	       folder
	       no-delete))
    (when (and (not no-delete)
	       (setq dels (nth 1 tmp)))
      (wl-append deleted-list (car (wl-expire-delete folder dels))))
    (setq delete-list (car tmp))
    (catch 'done
      (while t
	(if (setq msg (wl-pop delete-list))
	    (setq arcnum (/ msg wl-expire-archive-files))
	  (setq arcnum nil))
	(when (and prev-arcnum
		   (not (eq arcnum prev-arcnum)))
	  (setq dst-folder (format dst-folder-fmt
				   (* prev-arcnum wl-expire-archive-files)))
	  (and (setq ret-val
		     (funcall
		      refile-func
		      folder arcmsg-list dst-folder t preserve-number
		      no-delete))
	       (wl-append deleted-list (car ret-val)))
	  (setq arcmsg-list nil))
	(if (null msg)
	    (throw 'done t))
	(wl-append arcmsg-list (list msg))
	(setq prev-arcnum arcnum)))
    deleted-list))

(defun wl-expire-archive-number2 (folder delete-list
				  &optional preserve-number dst-folder-arg
					    no-delete)
  "Standard function for `wl-summary-expire'.
Refile to archive folder followed the number of message in one archive folder."
  (let* ((elmo-archive-treat-file t)	;; treat archive folder as a file.
	 (dst-folder-expand (and dst-folder-arg
				 (wl-expand-newtext
				  dst-folder-arg
				  (elmo-folder-name-internal folder))))
	 (dst-folder-fmt (funcall
			  wl-expire-archive-get-folder-function
			  folder nil dst-folder-expand))
	 (dst-folder-base (car dst-folder-fmt))
	 (dst-folder-fmt (cdr dst-folder-fmt))
	 (refile-func (if no-delete
			  'wl-expire-refile
			'wl-expire-refile-with-copy-reserve-msg))
	 (len 0) (filenum 0)
	 tmp dels dst-folder
	 arc-len msg arcmsg-list
	 deleted-list ret-val)
    (setq tmp (wl-expire-archive-number-delete-old
	       dst-folder-base preserve-number delete-list
	       folder
	       no-delete))
    (when (and (not no-delete)
	       (setq dels (nth 1 tmp)))
      (wl-append deleted-list (car (wl-expire-delete folder dels))))
    (setq delete-list (car tmp)
	  filenum (string-to-number (nth 3 tmp))
	  len (nth 4 tmp)
	  arc-len len)
    (catch 'done
      (while t
	(if (setq msg (wl-pop delete-list))
	    (setq len (1+ len))
	  (setq len (1+ wl-expire-archive-files)))
	(when (> len wl-expire-archive-files)
	  (when arcmsg-list
	    (setq dst-folder (format dst-folder-fmt filenum))
	    (and (setq ret-val
		       (funcall
			refile-func
			folder arcmsg-list dst-folder t preserve-number
			no-delete))
		 (wl-append deleted-list (car ret-val)))
	    (setq arc-len (+ arc-len (cdr ret-val))))
	  (setq arcmsg-list nil)
	  (if (< arc-len wl-expire-archive-files)
	      (setq len (1+ arc-len))
	    (setq filenum (+ filenum wl-expire-archive-files)
		  len (- len arc-len)	;; maybe 1
		  arc-len (1- len)	;; maybe 0
		  )))
	(if (null msg)
	    (throw 'done t))
	(wl-append arcmsg-list (list msg))))
    deleted-list))

(defun wl-expire-archive-date (folder delete-list
			       &optional preserve-number dst-folder-arg
					 no-delete)
  "Standard function for `wl-summary-expire'.
Refile to archive folder followed message date."
  (let* ((elmo-archive-treat-file t)	;; treat archive folder as a file.
	 (dst-folder-expand (and dst-folder-arg
				 (wl-expand-newtext
				  dst-folder-arg
				  (elmo-folder-name-internal folder))))
	 (dst-folder-fmt (funcall
			  wl-expire-archive-get-folder-function
			  folder
			  wl-expire-archive-date-folder-name-fmt
			  dst-folder-expand
			  ))
	 (dst-folder-base (car dst-folder-fmt))
	 (dst-folder-fmt (cdr dst-folder-fmt))
	 (refile-func (if no-delete
			  'wl-expire-refile
			'wl-expire-refile-with-copy-reserve-msg))
	 tmp dels dst-folder date time
	 msg arcmsg-alist arcmsg-list
	 deleted-list ret-val)
    (setq tmp (wl-expire-archive-number-delete-old
	       dst-folder-base preserve-number delete-list
	       folder
	       no-delete
	       wl-expire-archive-date-folder-num-regexp))
    (when (and (not no-delete)
	       (setq dels (nth 1 tmp)))
      (wl-append deleted-list (car (wl-expire-delete folder dels))))
    (setq delete-list (car tmp))
    (while (setq msg (wl-pop delete-list))
      (setq time (or (elmo-time-to-datevec
		      (elmo-message-field folder msg 'date))
		     (make-vector 7 0)))
      (if (= (aref time 1) 0)	;; if (month == 0)
	  (aset time 0 0))	;;    year = 0
      (setq dst-folder (format dst-folder-fmt
			       (aref time 0)  ;; year
			       (aref time 1)  ;; month
			       ))
      (setq arcmsg-alist
	    (wl-append-assoc-list
	     dst-folder
	     msg
	     arcmsg-alist)))
    (while arcmsg-alist
      (setq dst-folder (caar arcmsg-alist))
      (setq arcmsg-list (cdar arcmsg-alist))
      (and (setq ret-val
		 (funcall
		  refile-func
		  folder arcmsg-list dst-folder t preserve-number
		  no-delete))
	   (wl-append deleted-list (car ret-val)))
      (setq arcmsg-alist (cdr arcmsg-alist)))
    deleted-list))

;;; wl-expire-localdir-date
(defvar wl-expire-localdir-date-folder-name-fmt "%s/%%04d_%%02d")

(defcustom wl-expire-localdir-get-folder-function
  'wl-expire-localdir-get-folder
  "*A function to get localdir folder name."
  :type 'function
  :group 'wl-expire)

(defun wl-expire-localdir-get-folder (src-folder fmt dst-folder-arg)
  "Get localdir folder name from src-folder."
  (let* ((src-folder-name (substring
			   (elmo-folder-name-internal src-folder)
			   (length (elmo-folder-prefix-internal src-folder))))
	 (dst-folder-spec (char-to-string
			   (car (rassq 'localdir elmo-folder-type-alist))))
	 dst-folder-base dst-folder-fmt)
    (cond (dst-folder-arg
	   (setq dst-folder-base (concat dst-folder-spec dst-folder-arg)))
	  ((eq (elmo-folder-type-internal src-folder) 'localdir)
	   (setq dst-folder-base (concat dst-folder-spec src-folder-name)))
	  (t
	   (setq dst-folder-base
		 (elmo-concat-path
		  (format "%s%s"
			  dst-folder-spec
			  (elmo-folder-type-internal src-folder))
		  src-folder-name))))
    (setq dst-folder-fmt
	  (format fmt dst-folder-base))
    (cons dst-folder-base dst-folder-fmt)))

(defun wl-expire-localdir-date (folder delete-list
				       &optional preserve-number dst-folder-arg
				       no-delete)
  "Function for `wl-summary-expire'.
Refile to localdir folder by message date.
ex. +ml/wl/1999_11/, +ml/wl/1999_12/."
  (let* ((dst-folder-expand (and dst-folder-arg
				 (wl-expand-newtext
				  dst-folder-arg
				  (elmo-folder-name-internal folder))))
	 (dst-folder-fmt (funcall
			  wl-expire-localdir-get-folder-function
			  folder
			  wl-expire-localdir-date-folder-name-fmt
			  dst-folder-expand))
	 (dst-folder-base (car dst-folder-fmt))
	 (dst-folder-fmt (cdr dst-folder-fmt))
	 (refile-func (if no-delete
			  'wl-expire-refile
			'wl-expire-refile-with-copy-reserve-msg))
	 tmp dels dst-folder date time
	 msg arcmsg-alist arcmsg-list
	 deleted-list ret-val)
    (while (setq msg (wl-pop delete-list))
      (setq time (or (elmo-time-to-datevec
		      (elmo-message-field folder msg 'date))
		     (make-vector 7 0)))
      (if (= (aref time 1) 0)	;; if (month == 0)
	  (aset time 0 0))	;;    year = 0
      (setq dst-folder (format dst-folder-fmt
			       (aref time 0);; year
			       (aref time 1);; month
			       ))
      (setq arcmsg-alist
	    (wl-append-assoc-list
	     dst-folder
	     msg
	     arcmsg-alist)))
    (while arcmsg-alist
      (setq dst-folder (caar arcmsg-alist))
      (setq arcmsg-list (cdar arcmsg-alist))
      (and (setq ret-val
		 (funcall
		  refile-func
		  folder arcmsg-list dst-folder t preserve-number
		  no-delete))
	   (wl-append deleted-list (car ret-val)))
      (setq arcmsg-alist (cdr arcmsg-alist)))
    deleted-list))

(defun wl-expire-hide (folder hide-list &optional no-reserve-marks)
  "Hide message for expire."
  (unless no-reserve-marks
    (setq hide-list
	  (wl-expire-delete-reserved-messages hide-list folder)))
  (let ((mess (format "Hiding %s msgs..." (length hide-list))))
    (message "%s" mess)
    (elmo-folder-detach-messages folder hide-list)
    (elmo-folder-kill-messages folder hide-list)
    (elmo-folder-commit folder)
    (message "%sdone" mess)
    (cons hide-list (length hide-list))))

(defsubst wl-expire-folder-p (entity)
  "Return non-nil, when ENTITY matched `wl-expire-alist'."
  (wl-get-assoc-list-value wl-expire-alist entity))

(defsubst wl-archive-folder-p (entity)
  "Return non-nil, when ENTITY matched `wl-archive-alist'."
  (wl-get-assoc-list-value wl-archive-alist entity))

(defun wl-summary-expire (&optional folder notsummary all)
  "Expire messages of current summary."
  (interactive
   (list wl-summary-buffer-elmo-folder
	 nil
	 current-prefix-arg))
  (let* ((folder (or folder wl-summary-buffer-elmo-folder))
	 (folder-name (elmo-folder-name-internal folder))
	 (rule (wl-expire-folder-p folder-name)))
    (if (not rule)
	(and (interactive-p)
	     (error "No match %s in `wl-expire-alist'" folder-name))
      (when (or (not (interactive-p))
		(y-or-n-p (format "Expire %s? " folder-name)))
	(save-excursion
	  (run-hooks 'wl-summary-expire-pre-hook)
	  (let ((expired (apply #'wl-expire-folder folder all rule)))
	    (when (and (not wl-expire-test)
		       (not notsummary)
		       expired)
	      (wl-summary-delete-messages-on-buffer expired)
	      (wl-summary-folder-info-update)
	      (wl-summary-set-message-modified)
	      (sit-for 0)
	      (set-buffer-modified-p nil))
	    (run-hooks 'wl-summary-expire-hook)
	    (if expired
		(message "Expiring %s is done" folder-name)
	      (and (interactive-p)
		   (message "No expire")))
	    expired))))))

(defun wl-expire-folder (folder all condition action &rest args)
  (let ((folder-name (elmo-folder-name-internal folder))
	(val-type (car condition))
	(value (nth 1 condition))
	targets)
    (cond
     ((eq val-type nil))
     ((eq val-type 'number)
      (let* ((msgs (elmo-folder-list-messages folder (not all) (not all)))
	     (msglen (length msgs))
	     count)
	(when (>= msglen (or (nth 2 condition) (1+ value)))
	  (setq count (- msglen value))
	  (while (and msgs (> count 0))
	    (when (elmo-message-entity folder (car msgs))
	      ;; don't expire new message
	      (wl-append targets (list (car msgs)))
	      (when (or (not wl-expire-number-with-reserve-marks)
			(wl-expire-message-p folder (car msgs)))
		(setq count (1- count))))
	    (setq msgs (cdr msgs))))))
     ((eq val-type 'date)
      (let ((key-date (elmo-datevec-to-time
		       (elmo-date-get-offset-datevec
			(timezone-fix-time (current-time-string)
					   (current-time-zone) nil)
			value t))))
	(elmo-folder-do-each-message-entity (entity folder)
	  (when (elmo-time<
		 (elmo-message-entity-field entity 'date)
		 key-date)
	    (wl-append targets
		       (list (elmo-message-entity-number entity)))))))
     (t
      (error "%s: not supported" val-type)))
    (when targets
      (or wl-expired-alist
	  (setq wl-expired-alist (wl-expired-alist-load)))
      ;; evaluate string-match for wl-expand-newtext
      (wl-expire-folder-p folder-name)
      (prog1
	  (cond ((eq action nil) nil)
		((eq action 'remove)
		 (car (wl-expire-delete folder targets)))
		((eq action 'trash)
		 (car (wl-expire-refile folder targets wl-trash-folder)))
		((eq action 'hide)
		 (car (wl-expire-hide folder targets)))
		((stringp action)
		 (car (wl-expire-refile
		       folder
		       targets
		       (wl-expand-newtext action folder-name))))
		((fboundp action)
		 (apply action folder targets args))
		(t
		 (error "%s: invalid type" action)))
	(wl-expired-alist-save)))))

(defun wl-folder-expire-entity (entity)
  (cond
   ((consp entity)
    (let ((flist (nth 2 entity)))
      (while flist
	(wl-folder-expire-entity (car flist))
	(setq flist (cdr flist)))))
   ((stringp entity)
    (when (wl-expire-folder-p entity)
      (let ((folder (wl-folder-get-elmo-folder entity))
	    (summary (wl-summary-get-buffer entity))
	    (update-msgdb (cond
			   ((consp wl-expire-folder-update-msgdb)
			    (wl-string-match-member
			     entity
			     wl-expire-folder-update-msgdb))
			   (t
			    wl-expire-folder-update-msgdb))))
	(when update-msgdb
	  (wl-folder-sync-entity entity))
	(if summary
	    (save-selected-window
	      (with-current-buffer summary
		(let ((win (get-buffer-window summary t)))
		  (when win
		    (select-window win)))
		(when (wl-summary-expire folder)
		  (wl-summary-save-status))))
	  (when (wl-summary-expire folder 'no-summary)
	    (wl-folder-check-entity entity))))))))

;; Command

(defun wl-folder-expire-current-entity ()
  (interactive)
  (let ((entity-name (wl-folder-get-entity-from-buffer))
	(type (if (wl-folder-buffer-group-p)
		  'group
		'folder)))
    (when (and entity-name
	       (or (not (interactive-p))
		   (y-or-n-p (format "Expire %s? " entity-name))))
      (wl-folder-expire-entity
       (wl-folder-search-entity-by-name entity-name
					wl-folder-entity
					type))
      (message "Expiring %s is done" entity-name))))

;;; Archive

(defun wl-folder-archive-current-entity ()
  (interactive)
  (let ((entity-name (wl-folder-get-entity-from-buffer))
	(type (if (wl-folder-buffer-group-p)
		  'group
		'folder)))
    (when (and entity-name
	       (or (not (interactive-p))
		   (y-or-n-p (format "Archive %s? " entity-name))))
      (wl-folder-archive-entity
       (wl-folder-search-entity-by-name entity-name
					wl-folder-entity
					type))
      (message "Archiving %s is done" entity-name))))

(defun wl-archive-number1 (folder archive-list &optional dst-folder-arg)
  (wl-expire-archive-number1 folder archive-list t dst-folder-arg t))

(defun wl-archive-number2 (folder archive-list &optional dst-folder-arg)
  (wl-expire-archive-number2 folder archive-list t dst-folder-arg t))

(defun wl-archive-date (folder archive-list &optional dst-folder-arg)
  (wl-expire-archive-date folder archive-list t dst-folder-arg t))

(defun wl-archive-folder (folder archive-list dst-folder)
  (let* ((elmo-archive-treat-file t)	;; treat archive folder as a file.
	 copied-list ret-val)
    (setq archive-list
	  (car (wl-expire-archive-number-delete-old
		nil t archive-list
		folder
		t ;; no-confirm
		nil dst-folder)))
    (when archive-list
      (and (setq ret-val
		 (wl-expire-refile
		  folder archive-list dst-folder t t t)) ;; copy!!
	   (wl-append copied-list ret-val)))
    copied-list))

(defun wl-summary-archive (&optional arg folder notsummary nolist)
  ""
  (interactive "P")
  (let* ((folder (or folder wl-summary-buffer-elmo-folder))
	 (msgs (if (not nolist)
		   (elmo-folder-list-messages folder)
		 (elmo-folder-list-messages folder 'visible 'in-msgdb)))
	 (alist wl-archive-alist)
	 archives func args dst-folder archive-list)
    (if arg
	(let ((wl-default-spec (char-to-string
				(car (rassq 'archive
					    elmo-folder-type-alist)))))
	  (setq dst-folder (wl-summary-read-folder
			    (concat wl-default-spec
				    (substring
				     (elmo-folder-name-internal folder) 1))
			    "for archive"))))
    (run-hooks 'wl-summary-archive-pre-hook)
    (if dst-folder
	(wl-archive-folder folder msgs dst-folder)
      (when (and (or (setq archives (wl-archive-folder-p
				     (elmo-folder-name-internal folder)))
		     (progn (and (interactive-p)
				 (message "No match %s in wl-archive-alist"
					  (elmo-folder-name-internal folder)))
			    nil))
		 (or (not (interactive-p))
		     (y-or-n-p (format "Archive %s? "
				       (elmo-folder-name-internal folder)))))
	(setq func (car archives)
	      args (cdr archives))
	(setq archive-list
	      (apply func (append (list folder msgs) args)))
	(run-hooks 'wl-summary-archive-hook)
	(if archive-list
	    (message "Archiving %s is done" (elmo-folder-name-internal folder))
	  (and (interactive-p)
	       (message "No archive")))))))

(defun wl-folder-archive-entity (entity)
  (cond
   ((consp entity)
    (let ((flist (nth 2 entity)))
      (while flist
	(wl-folder-archive-entity (car flist))
	(setq flist (cdr flist)))))
   ((stringp entity)
    (wl-summary-archive nil (wl-folder-get-elmo-folder entity) t))))

;; append log

(defun wl-expire-append-log (src-folder msgs dst-folder action)
  (when wl-expire-use-log
    (save-excursion
      (let ((tmp-buf (get-buffer-create " *wl-expire work*"))
	    (filename (expand-file-name wl-expired-log-alist-file-name
					elmo-msgdb-directory)))
	(set-buffer tmp-buf)
	(erase-buffer)
	(if dst-folder
	    (insert (format "%s\t%s -> %s\t%s\n"
			    action
			    src-folder dst-folder msgs))
	  (insert (format "%s\t%s\t%s\n"
			  action
			  src-folder msgs)))
	(if (file-writable-p filename)
	    (write-region (point-min) (point-max)
			  filename t 'no-msg)
	  (message "%s is not writable." filename))
	(kill-buffer tmp-buf)))))

(require 'product)
(product-provide (provide 'wl-expire) (require 'wl-version))

;;; wl-expire.el ends here
