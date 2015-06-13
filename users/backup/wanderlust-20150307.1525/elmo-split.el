;;; elmo-split.el --- Split messages according to the user defined rules.

;; Copyright (C) 2002 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news

;; This file is part of ELMO (Elisp Library for Message Orchestration).

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
;; Put following lines on your .emacs.
;;
;; (autoload 'elmo-split "elmo-split" "Split messages on the folder." t)
;;
;; A command elmo-split is provided.  If you enter:
;;
;; M-x elmo-split
;;
;; Messages in the `elmo-split-folder' are splitted to the folders
;; according to the definition of `elmo-split-rule'.
;;

;;; Code:
(eval-when-compile (require 'cl))
(require 'elmo)

(eval-when-compile
  ;; Avoid compile warnings
  (require 'elmo-spam))

(defcustom elmo-split-rule nil
  "Split rule for the command `elmo-split'.
The format of this variable is a list of RULEs which has form like:
\(CONDITION ACTION [continue]\)

The 1st element CONDITION is a sexp which consists of following.

1. Functions which accept arguments FIELD-NAME and VALUE.
FIELD-NAME is a symbol of the field name.

`equal'             ... True if the field value equals to VALUE.
                        Case of the letters are ignored.
`match'             ... True if the field value matches to VALUE.
                        VALUE can contain \\& and \\N which will substitute
                        from matching \\(\\) patterns in the previous VALUE.
`address-equal'     ... True if one of the addresses in the field equals to
                        VALUE. Case of the letters are ignored.
`address-match'     ... True if one of the addresses in the field matches to
                        VALUE.
                        VALUE can contain \\& and \\N which will substitute
                        from matching \\(\\) patterns in the previous VALUE.

FIELD-NAME can be a list of field names, return true if any of the fields
satisfy the condition.

2. Functions which accept an argument SIZE, SIZE is some number.

`<'                 ... True if the size of the message is less than SIZE.
`>'                 ... True if the size of the message is greater than SIZE.

3. Functions which accept any number of arguments.

`or'                ... True if one of the argument returns true.
`and'               ... True if all of the arguments return true.

`spam-p'            ... True if contents of the message is guessed as spam.
                        Rest arguments are property list which consists
                        following.

                        `:register' ... If this value is non-nil,
                                        Register according to
                                        the classification.

5. A symbol.

When a symbol is specified, it is evaluated.

The 2nd element ACTION is the name of the destination folder or some symbol.
If CONDITION is satisfied, the message is splitted according to this value.

If ACTION is a string, it will be considered as the name of destination folder.
Symbol `delete' means that the substance of the message will be removed. On the
other hand, symbol `noop' is used to do nothing and keep the substance of the
message as it is. Or, if some function is specified, it will be called.

When the 3rd element `continue' is specified as symbol, evaluating rules is
not stopped even when the condition is satisfied.

Example:

\(setq elmo-split-rule
      ;; Messages from spammers are stored in `+junk'
      '(((or (address-equal from \"i.am@spammer\")
	     (address-equal from \"dull-work@dull-boy\")
	     (address-equal from \"death-march@software\")
	     (address-equal from \"ares@aon.at\")
	     (address-equal from \"get-money@richman\"))
	 \"+junk\")
	;; Messages from mule mailing list are stored in `%mule'
	((equal x-ml-name \"mule\") \"%mule\")
	;; Messages from wanderlust mailing list are stored in `%wanderlust'
	;; and continue evaluating following rules.
	((equal x-ml-name \"wanderlust\") \"%wanderlust\" continue)
	;; Messages from DoCoMo user are stored in `+docomo-{username}'.
	((match from \"\\\\(.*\\\\)@docomo\\\\.ne\\\\.jp\")
	 \"+docomo-\\\\1\")
	;; Unmatched mails go to `+inbox'.
	(t \"+inbox\")))"
  :group 'elmo
  :type 'sexp)

(defcustom elmo-split-folder "%inbox"
  "Target folder or list of folders for splitting."
  :type '(choice (string :tag "folder name")
		 (repeat (string :tag "folder name")))
  :group 'elmo)

(defcustom elmo-split-default-action 'noop
  "Default action for messages which pass all rules.
It can be some ACTION as in `elmo-split-rule'."
  :type '(choice (const :tag "do not touch" noop)
		 (const :tag "delete" delete)
		 (string :tag "folder name")
		 (function :tag "function"))
  :group 'elmo)

(defcustom elmo-split-log-coding-system 'x-ctext
  "A coding-system for writing log file."
  :type 'coding-system
  :group 'elmo)

(defcustom elmo-split-log-file "~/.elmo/split-log"
  "The file name of the split log."
  :type 'file
  :group 'elmo)

;;;
(defvar elmo-split-match-string-internal nil
  "Internal variable for string matching.  Don't touch this variable by hand.")

(defvar elmo-split-message-entity nil
  "Buffer local variable to store mime-entity.")
(make-variable-buffer-local 'elmo-split-message-entity)

;;;
(defun elmo-split-or (buffer &rest args)
  (catch 'done
    (dolist (arg args)
      (if (elmo-split-eval buffer arg)
	  (throw 'done t)))
    nil))

(defun elmo-split-and (buffer &rest args)
  (catch 'done
    (dolist (arg args)
      (unless (elmo-split-eval buffer arg)
	(throw 'done nil)))
    t))

(defun elmo-split-> (buffer size)
  (> (buffer-size buffer) size))

(defun elmo-split-< (buffer size)
  (< (buffer-size buffer) size))

(defun elmo-split-address-equal (buffer field-or-fields value)
  (with-current-buffer buffer
    (let (result)
      (dolist (field (if (listp field-or-fields)
			 field-or-fields
		       (list field-or-fields)))
	(let ((addrs (mapcar
		      'std11-address-string
		      (std11-parse-addresses-string
		       (std11-field-body (symbol-name field)))))
	      (case-fold-search t))
	  (while addrs
	    (when (string-match (concat "^"
					(regexp-quote value)
					"$") (car addrs))
	      (setq addrs nil
		    result t))
	    (setq addrs (cdr addrs)))))
      result)))

(defun elmo-split-address-match (buffer field-or-fields value)
  (with-current-buffer buffer
    (let (result)
      (dolist (field (if (listp field-or-fields)
			 field-or-fields
		       (list field-or-fields)))
	(let ((addrs (mapcar
		      'std11-address-string
		      (std11-parse-addresses-string
		       (std11-field-body (symbol-name field))))))
	  (while addrs
	    (when (string-match value (car addrs))
	      (setq elmo-split-match-string-internal (car addrs)
		    addrs nil
		    result t))
	    (setq addrs (cdr addrs)))))
      result)))

(defun elmo-split-fetch-decoded-field (entity field-name)
  (let ((sym (intern (capitalize field-name)))
	(field-body (mime-entity-fetch-field entity field-name)))
    (when field-body
      (mime-decode-field-body field-body sym 'plain))))

(defun elmo-split-equal (buffer field-or-fields value)
  (with-current-buffer buffer
    (let (result)
      (dolist (field (if (listp field-or-fields)
			 field-or-fields
		       (list field-or-fields)))
	(let ((field-value (and
			    elmo-split-message-entity
			    (elmo-split-fetch-decoded-field
			     elmo-split-message-entity
			     (symbol-name field)))))
	  (setq result (or result
			   (equal field-value value)))))
      result)))

(defun elmo-split-spam-p (buffer &rest plist)
  (require 'elmo-spam)
  (elmo-spam-buffer-spam-p (elmo-spam-processor)
			   buffer
			   (plist-get plist :register)))

(defun elmo-split-match (buffer field-or-fields value)
  (with-current-buffer buffer
    (let (result)
      (dolist (field (if (listp field-or-fields)
			 field-or-fields
		       (list field-or-fields)))
	(let ((field-value (and elmo-split-message-entity
				(elmo-split-fetch-decoded-field
				 elmo-split-message-entity
				 (symbol-name field)))))
	  (and field-value
	       (when (string-match value field-value)
		 (setq result t)
		 (setq elmo-split-match-string-internal field-value)))))
      result)))

(defun elmo-split-eval (buffer sexp)
  (cond
   ((consp sexp)
    (apply (intern (concat "elmo-split-" (symbol-name (car sexp))))
	   buffer
	   (cdr sexp)))
   ((stringp sexp)
    (std11-field-body sexp))
   (t (eval sexp))))

(defun elmo-split-log (message reharsal)
  (with-current-buffer (get-buffer-create "*elmo-split*")
    (goto-char (point-max))
    (let ((start (point))
	  (coding-system-for-write elmo-split-log-coding-system))
      (insert message)
      (if reharsal
	  (progn
	    (pop-to-buffer (current-buffer))
	    (sit-for 0))
	(write-region start (point) elmo-split-log-file t 'no-msg)))))

;;;###autoload
(defun elmo-split (&optional arg)
  "Split messages in the `elmo-split-folder' according to `elmo-split-rule'.
If prefix argument ARG is specified, do a reharsal (no harm)."
  (interactive "P")
  (unless elmo-split-rule
    (error "Split rule does not exist.  Set `elmo-split-rule' first"))
  (let ((folders (if (listp elmo-split-folder)
		     elmo-split-folder
		   (list elmo-split-folder)))
	(count 0)
	(fcount 0)
	ret)
    (dolist (folder folders)
      (setq ret (elmo-split-subr (elmo-get-folder folder) arg)
	    count (+ count (car ret))
	    fcount (+ fcount (cdr ret))))
    (run-hooks 'elmo-split-hook)
    (message
     (concat
      (cond
       ((zerop count)
	"No message is splitted")
       ((eq count 1)
	"1 message is splitted")
       (t
	(format "%d messages are splitted" count)))
      (if (zerop fcount)
	  "."
	(format " (%d failure)." fcount))))
    count))

(defun elmo-split-subr (folder &optional reharsal)
  (let ((count 0)
	(fcount 0)
	(default-rule `((t ,elmo-split-default-action)))
	msgs action target-folder failure delete-substance
	record-log log-string flags)
    (message "Splitting...")
    (elmo-folder-open-internal folder)
    (setq msgs (elmo-folder-list-messages folder))
    (elmo-with-progress-display (elmo-split (length msgs)) "Splitting messages"
      (unwind-protect
	  (with-temp-buffer
	    (set-buffer-multibyte nil)
	    (dolist (msg msgs)
	      (erase-buffer)
	      (when (ignore-errors
		      (elmo-message-fetch folder msg
					  (elmo-make-fetch-strategy 'entire)
					  'unread))
		(run-hooks 'elmo-split-fetch-hook)
		(setq elmo-split-message-entity (mime-parse-buffer))
		(setq flags (elmo-message-flags-for-append folder msg))
		(catch 'terminate
		  (dolist (rule (append elmo-split-rule default-rule))
		    (setq elmo-split-match-string-internal nil)
		    (when (elmo-split-eval (current-buffer) (car rule))
		      (if (and (stringp (nth 1 rule))
			       elmo-split-match-string-internal)
			  (setq action (elmo-expand-newtext
					(nth 1 rule)
					elmo-split-match-string-internal))
			(setq action (nth 1 rule)))
		      ;; 1. ACTION & DELETION
		      (unless reharsal
			(setq failure nil
			      delete-substance nil
			      record-log nil
			      log-string nil)
			(cond
			 ((stringp action)
			  (condition-case nil
			      (progn
				(setq target-folder (elmo-get-folder action))
				(unless (elmo-folder-exists-p target-folder)
				  (when
				      (and
				       (elmo-folder-creatable-p target-folder)
				       (y-or-n-p
					(format
					 "Folder %s does not exist, Create it? "
					 action)))
				    (elmo-folder-create target-folder)))
				(elmo-folder-open-internal target-folder)
				(setq failure (not
					       (elmo-folder-append-buffer
						target-folder
						flags)))
				(elmo-folder-close-internal target-folder))
			    (error (setq failure t)
				   (incf fcount)))
			  (setq record-log t
				delete-substance
				(not (or failure
					 (eq (nth 2 rule) 'continue))))
			  (incf count))
			 ((eq action 'delete)
			  (setq record-log t
				delete-substance t))
			 ((eq action 'noop)
			  ;; do nothing
			  )
			 ((functionp action)
			  (funcall action))
			 (t
			  (error "Wrong action specified in elmo-split-rule")))
			(when delete-substance
			  (ignore-errors
			    (elmo-folder-delete-messages folder (list msg)))))
		      ;; 2. RECORD LOG
		      (when (or record-log
				reharsal)
			(elmo-split-log
			 (concat "From "
				 (nth 1 (std11-extract-address-components
					 (or (std11-field-body "from") "")))
				 "  " (or (std11-field-body "date") "") "\n"
				 " Subject: "
				 (eword-decode-string (or (std11-field-body
							   "subject") ""))
				 "\n"
				 (if reharsal
				     (cond
				      ((stringp action)
				       (concat "  Test: " action "\n"))
				      ((eq action 'delete)
				       "  Test: /dev/null\n")
				      ((eq action 'noop)
				       "  Test: do nothing\n")
				      ((functionp action)
				       (format "  Test: function:%s\n"
					       (prin1-to-string action)))
				      (t
				       "  ERROR: wrong action specified\n"))
				   (cond
				    (failure
				     (concat "  FAILED: " action "\n"))
				    ((stringp action)
				     (concat "  Folder: " action "\n"))
				    ((eq action 'delete)
				     "  Deleted\n")
				    (log-string
				     log-string)
				    (t
				     (debug)))))
			 reharsal))
		      ;; 3. CONTINUATION CHECK
		      (unless (eq (nth 2 rule) 'continue)
			(throw 'terminate nil))))))
	      (elmo-progress-notify 'elmo-split)))
	(elmo-folder-close-internal folder)))
    (cons count fcount)))

(require 'product)
(product-provide (provide 'elmo-split) (require 'elmo-version))

;;; elmo-split.el ends here
