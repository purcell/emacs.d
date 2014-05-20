;;; keyfreq.el --- track command frequencies
;; -*- coding: utf-8 -*-
;;
;; Copyright 2009-2010 by David Capello
;; Copyright 2008 by Xah Lee
;; Copyright 2006 by Michal Nazarewicz
;; Copyright 2006 by Ryan Yeske
;;
;; Author: Ryan Yeske, Michal Nazarewicz (mina86/AT/mina86.com)
;; Maintainer: David Capello, Xah lee
;; Created: 2006
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; Version 1.4 - 2010-09 - David Capello
;; * Renamed from command-frequency to keyfreq
;; * Now keyfreq-table holds "deltas"
;; * Removed a lot of "facility" functions to keep the code simple.
;; * Rename keyfreq-summarize-all-modes -> keyfreq-groups-major-modes.
;; * Added keyfreq-filter-major-mode
;; * Added lock file to avoid overwrite the file by two processes at
;;   the same time.
;;
;; Version 1.3 - 2009-09 - David Capello
;; * Added keyfreq-summarize-all-modes
;;
;; Version 1.2 - 2009-09 - David Capello
;; * Now each hash hash-key is a (major-mode . command) cons. Now only
;;   symbols are recorded.
;;
;; Version 1.1 - 2008-09
;; - Replaced the use of this-command var by real-last-command, so
;;   that the commands backward-kill-word, kill-word, kill-line,
;;   kill-region, do not all get counted as kill-region. Changed
;;   post-command-hook to pre-command-hook
;;
;; Version 1.0 - 2007
;; - Made into a full featured minor mode.  Added full doc
;;   strings. Added feature to save and read to disk the frequency
;;   hash table. Added ability to set user preference using emacs's
;;   customization system. Code is ~400 lines. This version is made by
;;   Michal Nazarewicz in 2007.
;;
;; Version 0.1 - 2006
;; - First version by Ryan Yeske. A quick hack of about 40 lines.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HOW TO USE IT?
;;
;; Include the following lines in your .emacs file:
;;
;;   (require 'keyfreq)
;;   (keyfreq-mode 1)
;;   (keyfreq-autosave-mode 1)
;;
;; And use keyfreq-show to see how many times you used a command.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup keyfreq nil
  "Customization group for keyfreq mode.  This mode stores
number of times each command was called and provides it as
a statistical data."
  :package-version '(keyfreq . "1.4")
  :group 'local
  :prefix "keyfreq")

;;;###autoload
(define-minor-mode keyfreq-mode
  "Keyfreq mode records number of times each command was
called making it possible to access usage statistics through
various keyfreq-* functions."
  :global t
  :init-value nil
  :lighter nil
  :keymap nil
  :group 'keyfreq

  (if keyfreq-mode
      (add-hook 'pre-command-hook 'keyfreq-pre-command-hook)
    (remove-hook 'pre-command-hook 'keyfreq-pre-command-hook)))


(defcustom keyfreq-buffer "*frequencies*"
  "Buffer where frequencies are displayed."
  :group 'keyfreq
  :type 'string)


(defcustom keyfreq-file "~/.emacs.keyfreq"
  "File `keyfreq-table' is saved to/loaded from by
`keyfreq-table-save' and `keyfreq-table-load' functions
by default."
  :group 'keyfreq
  :type 'file)

(defcustom keyfreq-file-lock "~/.emacs.keyfreq.lock"
  "Lock file to update the `keyfreq-file'."
  :group 'keyfreq
  :type 'file)


(defvar keyfreq-table (make-hash-table :test 'equal :size 128)
  "Hash table storing number of times each command was called in each major mode
since the last time the frequencies were saved in `keyfreq-file'.")


(defun keyfreq-pre-command-hook ()
  "Records command execution in `keyfreq-table' hash."

  (let ((command real-last-command) count)
    (when (and command (symbolp command))
      (setq count (gethash (cons major-mode command) keyfreq-table))
      (puthash (cons major-mode command) (if count (1+ count) 1)
	       keyfreq-table))))


(defun keyfreq-groups-major-modes (table)
  "Groups major modes by command. Returns a hash table where
each entry has COMMAND as key and COUNTER as value."

  (let ((new-table (make-hash-table :test 'equal :size 128)))
    (maphash (lambda (k v)
	       (puthash (cdr k) (+ (gethash (cdr k) new-table 0) v) new-table))
	     table)
    new-table))


(defun keyfreq-filter-major-mode (table major-mode)
  "Leave the frequencies of  the specified major mode. Returns a
hash table where each entry has COMMAND as key and COUNTER as value."

  (let ((new-table (make-hash-table :test 'equal :size 128)))
    (maphash (lambda (k v)
	       (when (eq (car k) major-mode)
		 (puthash (cdr k) (+ (gethash (cdr k) new-table 0) v) new-table)))
	     table)
    new-table))


(defun keyfreq-used-major-modes (table)
  "Returns a list with the used major-modes (major modes
contained in the TABLE)."

  (let ((list))
    (maphash (lambda (k v)
	       (add-to-list 'list (car k)))
	     table)
    list))


(defun keyfreq-list (table &optional reverse limit)
  "Returns a cons which car is sum of times any command was used
and cdr is a list of (command . count) pairs.  If REVERSE is nil
sorts it starting from the most used command; if it is 'no-sort
the list is not sorted; if it is non-nil and not 'no-sort sorts
it from the least used commands.  If LIMIT is positive number
only commands which were used more then LIMIT times will be
added.  If it is negative number only commands which were used
less then -LIMIT times will be added."

  (let (l (sum 0))
    (maphash
     (cond
      ((or (not (numberp limit)) (= limit 0))
       (lambda (k v) (setq l (cons (cons k v) l) sum (+ sum v))))
      ((= limit -1) (lambda (k v) (setq sum (+ sum v))))
      ((< limit 0)
       (setq limit (- limit))
       (lambda (k v) (setq sum (+ sum v))
	 (if (< v limit) (setq l (cons (cons k v) l)))))
      (t
       (lambda (k v) (setq sum (+ sum v))
	 (if (> v limit) (setq l (cons (cons k v) l))))))
     table)
    (cons sum
	  (cond
	   ((equal reverse 'no-sort) l)
	   (reverse (sort l (lambda (a b) (< (cdr a) (cdr b)))))
	   (t       (sort l (lambda (a b) (> (cdr a) (cdr b)))))))))


(defun keyfreq-format-list (list &optional func)
  "Returns formatted string with command usage statistics.

The LIST is the `keyfreq-table' converted to a list using the `keyfreq-list'.

If FUNC is nil each line contains number of times command was
called and the command; if it is t percentage usage is added in
the middle; if it is 'raw each line will contain number an
command separated by single line (with no formatting) otherwise
FUNC must be a function returning a string which will be called
for each entry with three arguments: number of times command was
called, percentage usage and the command."
  (let* ((sum (car list))
         (max-len
          (reduce (lambda (a b) (max a (length (symbol-name (car b)))))
                  (cdr list)
                  :initial-value 0)))
    (mapconcat
     (cond
      ((not func) (lambda (e) (format "%7d  %s\n" (cdr e) (car e))))
      ((equal func t)
       (lambda (e) (format (concat "%7d  %6.2f%%  %- "
                              (format "%d" max-len)
                              "s %s\n")
			   (cdr e) (/ (* 1e2 (cdr e)) sum) (car e)
                           (ignore-errors (keyfreq-where-is (car e))))))
      ((equal func 'raw) (lambda (e) (format "%d %s\n" (cdr e) (car e))))
      (t (lambda (e) (funcall func (cdr e) (/ (* 1e2 (cdr e)) sum) (car e)))))
     (cdr list) "")))

(defun keyfreq-where-is (command)
  (mapconcat 'key-description
             (where-is-internal command)
             ", "))

(defun keyfreq-show (&optional major-mode-symbol)
  "Shows command usage statistics in `keyfreq-buffer' using
`keyfreq-string' function.

If MAJOR-MODE-SYMBOL is given, the function shows the statistics
for that particular major-mode only.

With universal argument, the major-mode of the current
buffer is used as MAJOR-MODE-SYMBOL argument."

  (interactive (list (cond (current-prefix-arg major-mode)
			   (t nil))))

  (let ((table (copy-hash-table keyfreq-table)))
    ;; Merge with the values in .emacs.keyfreq file
    (keyfreq-table-load table)

    (let* ((list (keyfreq-list
		  (cond
		   (major-mode-symbol (keyfreq-filter-major-mode table major-mode-symbol))
		   (t (keyfreq-groups-major-modes table)))))
	   (formatted-list (keyfreq-format-list list t)))

      ;; Display the table
      (display-message-or-buffer (concat (if major-mode-symbol
					     (concat "For " (symbol-name major-mode))
					   (concat "For all major modes"))
					 ":\n\n"
					 formatted-list)
				 keyfreq-buffer)
      )))


(defun keyfreq-html (filename &optional confirm)
  "Saves an HTML file with all the statistics of each mode."

  (interactive
   (list (if buffer-file-name
	     (read-file-name "Write HTML file: "
			     nil nil nil nil)
	   (read-file-name "Write HTML file: " default-directory
			   (expand-file-name
			    (file-name-nondirectory (buffer-name))
			    default-directory)
			   nil nil))
	 (not current-prefix-arg)))

  (and confirm
       (file-exists-p filename)
       (or (y-or-n-p (format "File `%s' exists; overwrite? " filename))
	   (error "Canceled")))

  (let ((table (copy-hash-table keyfreq-table))
	(htmltable (lambda (list)
		     (insert "<table>\n")
		     (insert "<thead><tr><th>Times</th><th>Percetage</th><th>Command</th></tr></thead>\n")
		     (insert "<tbody>\n")
		     (keyfreq-format-list list
					  (lambda (count perc command)
					    (insert (format "<tr><td>%d</td><td>%.2f%%</td><td>%s</td></tr>\n" count perc command))))
		     (insert "</tbody>\n")
		     (insert "</table>\n"))))

    ;; Merge with the values in .emacs.keyfreq file
    (keyfreq-table-load table)

    (with-temp-file filename
      (insert "<html>\n<body>\n")
      (insert "<h1>Keyfreq Report</h1>\n")
      (insert "<ul>\n")
      (insert "<li><a href=\"#all\">All major modes</a></li>\n")
      (mapc
       (lambda (major-mode-symbol)
	 (insert (format "<li><a href=\"#%s\">%s</a></li>\n"
			 (symbol-name major-mode-symbol)
			 (symbol-name major-mode-symbol))))
       (keyfreq-used-major-modes table))
      (insert "</ul>\n")

      (insert "<h2><a name=\"all\">All major modes</a></h2>\n")
      (funcall htmltable (keyfreq-list (keyfreq-groups-major-modes table)))

      (mapc
       (lambda (major-mode-symbol)
	 (insert (format "<h2><a name=\"%s\">%s</a></h2>\n"
			 (symbol-name major-mode-symbol)
			 (symbol-name major-mode-symbol)))
	 (funcall htmltable (keyfreq-list (keyfreq-filter-major-mode table major-mode-symbol))))
       (keyfreq-used-major-modes table))

      (insert "</body>\n</html>\n")
      )
    ))


(defun keyfreq-json-encode (table)
  "Returns a JSON representation of the table of frequencies."
  (require 'json)
  (let ((commands-indexes (make-hash-table :test 'equal :size 128))
	commands-list frequencies-matrix i)

    ;; Build the "commands" property of the JSON object.
    (setq i 0)
    (maphash
     (lambda (command-symbol counter)
       (add-to-list 'commands-list command-symbol t)
       (puthash command-symbol i commands-indexes)
       (setq i (+ i 1))
       )
     (keyfreq-groups-major-modes table))

    ;; Build the "frequencies" property.
    (mapc
     (lambda (major-mode-symbol)
       (let ((cmd-count-pairs '()))
	 (keyfreq-format-list (keyfreq-list (keyfreq-filter-major-mode table major-mode-symbol))
			      (lambda (count perc command)
				(add-to-list 'cmd-count-pairs (gethash command commands-indexes) t)
				(add-to-list 'cmd-count-pairs count t)))
	 (add-to-list 'frequencies-matrix major-mode-symbol t)
	 (add-to-list 'frequencies-matrix (append cmd-count-pairs nil) t)))
     (keyfreq-used-major-modes table))

    (json-encode `((:format . 1)
		   (:commands . ,commands-list)
		   (:frequencies . ,frequencies-matrix)))))


(defun keyfreq-json (filename &optional confirm)
  "Saves a file with a JSON structure of the data."

  (interactive
   (list (if buffer-file-name
	     (read-file-name "Write JSON file: "
			     nil nil nil nil)
	   (read-file-name "Write JSON file: " default-directory
			   (expand-file-name
			    (file-name-nondirectory (buffer-name))
			    default-directory)
			   nil nil))
	 (not current-prefix-arg)))

  (and confirm
       (file-exists-p filename)
       (or (y-or-n-p (format "File `%s' exists; overwrite? " filename))
	   (error "Canceled")))

  (let ((table (copy-hash-table keyfreq-table)))

    ;; Merge with the values in .emacs.keyfreq file
    (keyfreq-table-load table)

    (with-temp-file filename
      (insert (keyfreq-json-encode table)))))


(defun keyfreq-file-owner ()
  "Return the PID of the Emacs process that owns the table file lock file."
  (let (owner)
    (and (file-exists-p keyfreq-file-lock)
	 (with-temp-buffer
	   (insert-file-contents-literally keyfreq-file-lock)
	   (goto-char (point-min))
	   (setq owner (read (current-buffer)))
	   (integerp owner))
	 owner)))


(defun keyfreq-file-claim-lock ()
  (write-region (number-to-string (emacs-pid)) nil
		keyfreq-file-lock nil 'nomessage))


(defun keyfreq-file-release-lock ()
  (when (file-exists-p keyfreq-file-lock)
    (delete-file keyfreq-file-lock)))


(defun keyfreq-file-is-unlocked ()
  ;; If the lock file exists....
  (if (file-exists-p keyfreq-file-lock)
      ;; If the process which has the lock does not exist
      (if (not (memql (keyfreq-file-owner) (list-system-processes)))
	  ;; Delete the lock
	  (delete-file keyfreq-file-lock)))
  ;; Check again the lock existence (just in case...)
  (not (file-exists-p keyfreq-file-lock)))


(defun keyfreq-table-save (table)
  "Appends all values from the specified TABLE into the
`keyfreq-file' as a sexp of an alist. Then resets the TABLE
if it was successfully merged."

  ;; Check that the lock file does not exist
  (when (keyfreq-file-is-unlocked)
    ;; Lock the file
    (keyfreq-file-claim-lock)

    ;; Check that we have the lock
    (if (eq (keyfreq-file-owner) (emacs-pid))
	(unwind-protect
	    (progn
	      ;; Load values and merge them with the current keyfreq-table
	      (keyfreq-table-load table)

	      ;; Write the new frequencies
	      (with-temp-file keyfreq-file
		(prin1 (cdr (keyfreq-list table 'no-sort)) (current-buffer))))

	  ;; Release the lock and reset the hash table.
	  (keyfreq-file-release-lock)
	  (clrhash table))
      )))


(defun keyfreq-table-load (table)
  "Loads all values from the `keyfreq-file' and adds them in the TABLE.
The table is not reset, so the values are appended to the table."

  ;; Does `keyfreq-file' exist?
  (if (file-exists-p keyfreq-file)
      ;; Load sexp
      (let ((l (with-temp-buffer
		 (insert-file-contents keyfreq-file)
		 (goto-char (point-min))
		 (read (current-buffer)))))

	;; Add the values in the table
	(while (and (listp l) l)
	  (if (listp (car l))
	      (puthash (caar l) (+ (gethash (caar l) table 0) (cdar l)) table))
	  (setq l (cdr l)))
	)))

;;;###autoload
(define-minor-mode keyfreq-autosave-mode
  "Keyfreq Autosave mode automatically saves
`keyfreq-table' every `keyfreq-autosave-timeout' seconds
and when emacs is killed."
  :global t
  :init-value nil
  :lighter nil
  :keymap nil
  :group 'keyfreq

  (when keyfreq-autosave--timer
    (cancel-timer keyfreq-autosave--timer)
    (setq keyfreq-autosave--timer nil))

  (if keyfreq-autosave-mode
      (progn
	(setq keyfreq-autosave--timer
	      (run-at-time t keyfreq-autosave-timeout
			   'keyfreq-autosave--do))
	(add-hook 'kill-emacs-hook 'keyfreq-autosave--do))
    (remove-hook 'kill-emacs-hook 'keyfreq-autosave--do)))


(defcustom keyfreq-autosave-timeout 600
  "How often in seconds `keyfreq-table' should be saved
when `keyfreq-autosave-mode' is enabled.  Setting this
value will take effect only after (re)enabling
`keyfreq-autosave-mode'."
  :group 'keyfreq
  :type 'number)


(defvar keyfreq-autosave--timer nil)


(defun keyfreq-autosave--do ()
  "Function executed periodically to save the `keyfreq-table' in `keyfreq-file'."
  ;; I want to exit emacs as usually even there is exception here
  (condition-case nil
      (keyfreq-table-save keyfreq-table)
    (error
     (message "~/.emacs.keyfreq is corrupt"))))


(provide 'keyfreq)

;;; keyfreq.el ends here
