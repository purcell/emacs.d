;;; wl-spam.el --- Spam filtering interface for Wanderlust.

;; Copyright (C) 2003 Hiroya Murata <lapis-lazuli@pop06.odn.ne.jp>
;; Copyright (C) 2003 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Hiroya Murata <lapis-lazuli@pop06.odn.ne.jp>
;; Keywords: mail, net news, spam

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

(eval-when-compile (require 'cl))

(require 'elmo-spam)
(require 'wl-summary)
(require 'wl-action)
(require 'wl-highlight)

(defgroup wl-spam nil
  "Spam configuration for wanderlust."
  :group 'wl)

(defcustom wl-spam-folder "+spam"
  "*Spam folder."
  :type 'string
  :group 'wl-spam)

(defcustom wl-spam-undecided-folder-list nil
  "*List of folder name which is contained undecided domain.
If an element is symbol, use symbol-value instead."
  :type '(repeat (choice (string :tag "Folder name")
			 (variable :tag "Variable")))
  :group 'wl-spam)

(defcustom wl-spam-undecided-folder-regexp-list '("inbox")
  "*List of folder regexp which is contained undecided domain."
  :type '(repeat (regexp :tag "Folder Regexp"))
  :group 'wl-spam)

(defcustom wl-spam-ignored-folder-list '(wl-draft-folder
					 wl-trash-folder
					 wl-queue-folder)
  "*List of folder name which is contained ignored domain.
If an element is symbol, use symbol-value instead."
  :type '(repeat (choice (string :tag "Folder name")
			 (variable :tag "Variable")))
  :group 'wl-spam)

(defcustom wl-spam-ignored-folder-regexp-list nil
  "*List of folder regexp which is contained ignored domain."
  :type '(repeat (regexp :tag "Folder Regexp"))
  :group 'wl-spam)

(defcustom wl-spam-auto-check-folder-regexp-list nil
  "*List of Folder regexp which check spam automatically."
  :type '(repeat (regexp :tag "Folder Regexp"))
  :group 'wl-spam)

(defcustom wl-spam-auto-check-marks
  (list wl-summary-new-uncached-mark
	wl-summary-new-cached-mark)
  "Persistent marks to check spam automatically."
  :type '(choice (const :tag "All marks" all)
		 (repeat (string :tag "Mark")))
  :group 'wl-spam)

(wl-defface wl-highlight-summary-spam-face
  '((((type tty)
      (background dark))
     (:foreground "blue"))
    (((class color))
     (:foreground "LightSlateGray")))
  "Face used for displaying messages mark as spam."
  :group 'wl-summary-faces
  :group 'wl-faces)

(defcustom wl-spam-mark-action-list
  '(("s"
     spam
     nil
     wl-summary-register-temp-mark
     wl-summary-exec-action-spam
     wl-highlight-summary-spam-face
     "Mark messages as spam."))
  "A variable to define Mark & Action for spam.
Append this value to `wl-summary-mark-action-list' by `wl-spam-setup'.

See `wl-summary-mark-action-list' for the detail of element."
  :type '(repeat (list
		  (string :tag "Temporary mark")
		  (symbol :tag "Action name")
		  (symbol :tag "Argument function")
		  (symbol :tag "Set mark function")
		  (symbol :tag "Exec function")
		  (symbol :tag "Face symbol")
		  (string :tag "Document string")))
  :group 'wl-spam)

(defsubst wl-spam-string-member-p (string list regexp-list)
  (or (wl-string-member string list)
      (wl-string-match-member string regexp-list)))

(defun wl-spam-domain (folder-name)
  (cond ((string= folder-name wl-spam-folder)
	 'spam)
	((wl-spam-string-member-p folder-name
				  wl-spam-undecided-folder-list
				  wl-spam-undecided-folder-regexp-list)
	 'undecided)
	((wl-spam-string-member-p folder-name
				  wl-spam-ignored-folder-list
				  wl-spam-ignored-folder-regexp-list)
	 'ignore)
	(t
	 'good)))

(defun wl-spam-split-numbers (folder numbers)
  (let (alist)
    (dolist (number numbers)
      (let* ((domain (wl-spam-domain
		      (elmo-folder-name-internal
		       (elmo-message-folder folder number))))
	     (cell (assq domain alist)))
	(if cell
	    (setcdr cell (cons number (cdr cell)))
	  (setq alist (cons (list domain number) alist)))))
    alist))

(defsubst wl-spam-auto-check-message-p (folder number)
  (or (eq wl-spam-auto-check-marks 'all)
      (member (wl-summary-message-mark folder number)
	      wl-spam-auto-check-marks)))

(defsubst wl-spam-map-spam-messages (folder numbers function &rest args)
  (elmo-with-progress-display (elmo-spam-check-spam (length numbers))
      "Checking spam"
    (dolist (number (elmo-spam-list-spam-messages (elmo-spam-processor)
						  folder
						  numbers))
      (apply function number args))))

(defun wl-spam-apply-partitions (folder partitions function msg)
  (when partitions
    (let ((total 0))
      (dolist (partition partitions)
	(setq total (+ total (length (cdr partition)))))
      (elmo-with-progress-display (elmo-spam-register total) msg
	(dolist (partition partitions)
	  (funcall function folder (cdr partition) (car partition)))))))

(defun wl-spam-register-spam-messages (folder numbers)
  "Register messages specified by FOLDER and NUMBERS as spam.
Put spam mark unless FOLDER is a spam folder."
  (elmo-with-progress-display (elmo-spam-register (length numbers))
      "Registering spam"
    (elmo-spam-register-spam-messages (elmo-spam-processor)
				      folder
				      numbers))
  (unless (eq (wl-spam-domain (elmo-folder-name-internal folder))
	      'spam)
    (dolist (number numbers)
      (wl-summary-spam number))))

(defun wl-spam-register-good-messages (folder numbers)
  "Register messages specified by FOLDER and NUMBERS as non-spam.
Remove spam mark."
  (elmo-with-progress-display (elmo-spam-register (length numbers))
      "Registering good"
    (elmo-spam-register-good-messages (elmo-spam-processor)
				      folder
				      numbers))
  (dolist (number numbers)
    (wl-summary-unmark-spam number)))

(defun wl-spam-save-status (&optional force)
  (interactive "P")
  (let ((processor (elmo-spam-processor (not force))))
    (when (or force
	      (and processor (elmo-spam-modified-p processor)))
      (elmo-spam-save-status processor))))

;; insinuate into summary mode
(defvar wl-summary-spam-map nil)

(unless wl-summary-spam-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" 'wl-summary-spam)
    (define-key map "c" 'wl-summary-test-spam)
    (define-key map "C" 'wl-summary-mark-spam)
    (define-key map "s" 'wl-summary-register-as-spam)
    (define-key map "S" 'wl-summary-register-as-spam-all)
    (define-key map "n" 'wl-summary-register-as-good)
    (define-key map "N" 'wl-summary-register-as-good-all)
    (setq wl-summary-spam-map map)))

(eval-when-compile
  ;; Avoid compile warnings
  (defalias-maybe 'wl-summary-spam 'ignore)
  (defalias-maybe 'wl-summary-unmark-spam 'ignore))

(defun wl-summary-test-spam (&optional folder number)
  (interactive)
  (let ((folder (or folder wl-summary-buffer-elmo-folder))
	(number (or number (wl-summary-message-number)))
	spam)
    (message "Checking spam...")
    (if (setq spam (elmo-spam-message-spam-p (elmo-spam-processor)
					     folder number))
	(wl-summary-spam number)
      (wl-summary-unmark-spam number))
    (message "Checking spam...done")
    (when (interactive-p)
      (message "No: %d is %sa spam message." number (if spam "" "not ")))))

(defun wl-summary-test-spam-messages (folder numbers &rest args)
  (elmo-with-progress-display (elmo-spam-check-spam (length numbers))
      "Checking spam"
    (let* ((spams (elmo-spam-list-spam-messages (elmo-spam-processor)
						folder
						numbers))
	   (goods (car (elmo-list-diff numbers spams))))
      (dolist (number spams)
	(wl-summary-spam number args))
      (dolist (number goods)
	(wl-summary-unmark-spam number)))))

(defun wl-summary-test-spam-region (beg end)
  (interactive "r")
  (let ((numbers (wl-summary-collect-numbers-region beg end)))
    (cond (numbers
	   (wl-summary-test-spam-messages wl-summary-buffer-elmo-folder
					  numbers))
	  ((interactive-p)
	   (message "No message to test.")))))

(defun wl-thread-test-spam (&optional arg)
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-test-spam-region arg))

(defun wl-summary-mark-spam (&optional all)
  "Set spam mark to messages which is spam classification."
  (interactive "P")
  (let (numbers)
    (if all
	(setq numbers wl-summary-buffer-number-list)
      (dolist (number wl-summary-buffer-number-list)
	(when (wl-spam-auto-check-message-p wl-summary-buffer-elmo-folder
					    number)
	  (setq numbers (cons number numbers)))))
    (cond (numbers
	   (wl-spam-map-spam-messages wl-summary-buffer-elmo-folder
				      numbers
				      #'wl-summary-spam))
	  ((interactive-p)
	   (message "No message to test.")))))

(defun wl-summary-register-as-spam ()
  "Register current message as spam.
Put spam mark unless current folder is a spam folder."
  (interactive)
  (let ((number (wl-summary-message-number)))
    (when number
      (wl-spam-register-spam-messages wl-summary-buffer-elmo-folder
				      (list number)))))

(defun wl-summary-register-as-spam-region (beg end)
  "Register messages in the region between BEG and END as spam.
Put spam mark unless current folder is a spam folder."
  (interactive "r")
  (let ((numbers (wl-summary-collect-numbers-region beg end)))
    (cond (numbers
	   (wl-spam-register-spam-messages wl-summary-buffer-elmo-folder
					   numbers))
	  ((interactive-p)
	   (message "No message to register as spam.")))))

(defun wl-thread-register-as-spam (&optional arg)
  "Register messages which are the descendant of the current thread as spam.
Put spam mark unless current folder is a spam folder.
With prefix argument, it affects on the all messages in the thread tree."
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-register-as-spam-region arg))

(defun wl-summary-register-as-spam-all ()
  "Register all messages in the folder as spam.
Put spam mark unless current folder is a spam folder."
  (interactive)
  (wl-spam-register-spam-messages wl-summary-buffer-elmo-folder
				  wl-summary-buffer-number-list))

(defun wl-summary-target-mark-register-as-spam ()
  "Register messages with the target mark as spam.
Put spam mark unless current folder is a spam folder."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t)
	  (buffer-read-only nil)
	  wl-summary-buffer-disp-msg)
      (wl-spam-register-spam-messages wl-summary-buffer-elmo-folder
				      wl-summary-buffer-target-mark-list)
      (dolist (number wl-summary-buffer-target-mark-list)
	(wl-summary-unset-mark number)))))

(defun wl-summary-register-as-good ()
  "Register current message as non-spam.
Remove spam mark."
  (interactive)
  (let ((number (wl-summary-message-number)))
    (when number
      (wl-spam-register-good-messages wl-summary-buffer-elmo-folder
				      (list number)))))

(defun wl-summary-register-as-good-region (beg end)
  "Register messages in the region between BEG and END as non-spam.
Remove spam mark."
  (interactive "r")
  (let ((numbers (wl-summary-collect-numbers-region beg end)))
    (cond (numbers
	   (wl-spam-register-good-messages wl-summary-buffer-elmo-folder
					   numbers))
	  ((interactive-p)
	   (message "No message to register as good.")))))

(defun wl-thread-register-as-good (&optional arg)
  "Register messages which are the descendant of the current thread as non-spam.
Remove spam mark.
With prefix argument, it affects on the all messages in the thread tree."
  (interactive "P")
  (wl-thread-call-region-func 'wl-summary-register-as-good-region arg))

(defun wl-summary-register-as-good-all ()
  "Register all messages in the folder as non-spam.
Remove spam mark."
  (interactive)
  (wl-spam-register-good-messages wl-summary-buffer-elmo-folder
				  wl-summary-buffer-number-list))

(defun wl-summary-target-mark-register-as-good ()
  "Register messages with the target mark as non-spam.
Remove spam mark."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t)
	  (buffer-read-only nil)
	  wl-summary-buffer-disp-msg)
      (wl-spam-register-good-messages wl-summary-buffer-elmo-folder
				      wl-summary-buffer-target-mark-list)
      (dolist (number wl-summary-buffer-target-mark-list)
	(wl-summary-unset-mark number)))))

;; hook functions and other
(defun wl-summary-auto-check-spam ()
  (when (elmo-string-match-member (wl-summary-buffer-folder-name)
				  wl-spam-auto-check-folder-regexp-list)
    (wl-summary-mark-spam)))

(defun wl-summary-exec-action-spam (mark-list)
  (let ((folder wl-summary-buffer-elmo-folder))
    (wl-folder-confirm-existence (wl-folder-get-elmo-folder wl-spam-folder))
    (wl-spam-apply-partitions
     folder
     (wl-filter-associations
      '(undecided good)
      (wl-spam-split-numbers folder (mapcar #'car mark-list)))
     (lambda (folder numbers domain)
       (elmo-spam-register-spam-messages (elmo-spam-processor)
					 folder numbers
					 (eq domain 'good)))
     "Registering spam")
    (wl-summary-move-mark-list-messages mark-list
					wl-spam-folder
					"Refiling spam")))

(defun wl-summary-exec-action-refile-with-register (mark-list)
  (let ((folder wl-summary-buffer-elmo-folder)
	spam-list good-list)
    (dolist (info mark-list)
      (case (wl-spam-domain (nth 2 info))
	(spam
	 (setq spam-list (cons (car info) spam-list)))
	(good
	 (setq good-list (cons (car info) good-list)))))
    (wl-spam-apply-partitions
     folder
     (wl-filter-associations '(undecided good)
			     (wl-spam-split-numbers folder spam-list))
     (lambda (folder numbers domain)
       (elmo-spam-register-spam-messages (elmo-spam-processor)
					 folder numbers
					 (eq domain 'good)))
     "Registering spam")
    (wl-spam-apply-partitions
     folder
     (wl-filter-associations '(undecided spam)
			     (wl-spam-split-numbers folder good-list))
     (lambda (folder numbers domain)
       (elmo-spam-register-good-messages (elmo-spam-processor)
					 folder numbers
					 (eq domain 'spam)))
     "Registering good")
    ;; execute refile messages
    (wl-summary-exec-action-refile mark-list)))

(defun wl-message-check-spam ()
  (let ((original (wl-message-get-original-buffer))
	(number wl-message-buffer-cur-number)
	spam)
    (message "Checking spam...")
    (when (setq spam (elmo-spam-buffer-spam-p (elmo-spam-processor) original))
      (with-current-buffer wl-message-buffer-cur-summary-buffer
	(wl-summary-spam number)))
    (message "Checking spam...done")
    (message "No: %d is %sa spam message." number (if spam "" "not "))))

(defun wl-refile-guess-by-spam (entity)
  (when (elmo-spam-message-spam-p (elmo-spam-processor)
				  wl-summary-buffer-elmo-folder
				  (elmo-message-entity-number entity))
    wl-spam-folder))

(defun wl-spam-setup ()
  (add-hook 'wl-summary-sync-updated-hook #'wl-summary-auto-check-spam)
  (let ((actions wl-summary-mark-action-list)
	action)
    (while actions
      (setq action  (car actions)
	    actions (cdr actions))
      (when (eq (wl-summary-action-symbol action) 'refile)
	(setcar (nthcdr 4 action) 'wl-summary-exec-action-refile-with-register)
	(setq actions nil))))
  (when wl-spam-mark-action-list
    (setq wl-summary-mark-action-list (append
				       wl-summary-mark-action-list
				       wl-spam-mark-action-list))
    (dolist (action wl-spam-mark-action-list)
      (setq wl-summary-reserve-mark-list
	    (cons (wl-summary-action-mark action)
		  wl-summary-reserve-mark-list))
      (setq wl-summary-skip-mark-list
	    (cons (wl-summary-action-mark action)
		  wl-summary-skip-mark-list))))
  (define-key wl-summary-mode-map "k" wl-summary-spam-map)
  (define-key
    wl-summary-mode-map "rkm" 'wl-summary-spam-region)
  (define-key
    wl-summary-mode-map "rkc" 'wl-summary-test-spam-region)
  (define-key
    wl-summary-mode-map "rks" 'wl-summary-register-as-spam-region)
  (define-key
    wl-summary-mode-map "rkn" 'wl-summary-register-as-good-region)
  (define-key
    wl-summary-mode-map "tkm" 'wl-thread-spam)
  (define-key
    wl-summary-mode-map "tkc" 'wl-thread-test-spam)
  (define-key
    wl-summary-mode-map "tks" 'wl-thread-register-as-spam)
  (define-key
    wl-summary-mode-map "tkn" 'wl-thread-register-as-good)
  (define-key
    wl-summary-mode-map "mk" 'wl-summary-target-mark-spam)
  (define-key
    wl-summary-mode-map "ms" 'wl-summary-target-mark-register-as-spam)
  (define-key
    wl-summary-mode-map "mn" 'wl-summary-target-mark-register-as-good))

(require 'product)
(product-provide (provide 'wl-spam) (require 'wl-version))

(unless noninteractive
  (wl-spam-setup))

;;; wl-spam.el ends here
