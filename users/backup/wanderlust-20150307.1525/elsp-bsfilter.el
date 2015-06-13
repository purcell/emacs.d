;;; elsp-bsfilter.el --- Bsfilter support for elmo-spam.

;; Copyright (C) 2004 Hiroya Murata <lapis-lazuli@pop06.odn.ne.jp>
;; Copyright (C) 2004 Yuuichi Teranishi <teranisi@gohome.org>

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
(require 'elmo-spam)
(require 'luna)

(defgroup elmo-spam-bsfilter nil
  "Spam bsfilter configuration."
  :group 'elmo-spam)

(defcustom elmo-spam-bsfilter-shell-program "ruby"
  "*"
  :type 'string
  :group 'elmo-spam-bsfilter)

(defcustom elmo-spam-bsfilter-shell-switch nil
  "*"
  :type 'string
  :group 'elmo-spam-bsfilter)

(defcustom elmo-spam-bsfilter-program (exec-installed-p "bsfilter")
  "*Program name of the Bsfilter."
  :type '(string :tag "Program name of the bsfilter")
  :group 'elmo-spam-bsfilter)

(defcustom elmo-spam-bsfilter-args nil
  "*Argument list for bsfilter."
  :type '(repeat string)
  :group 'elmo-spam-bsfilter)

(defcustom elmo-spam-bsfilter-update-switch "--auto-update"
  "*The switch that Bsfilter uses to update database with classify."
  :type 'string
  :group 'elmo-spam-bsfilter)

(defcustom elmo-spam-bsfilter-database-directory nil
  "*Directory path of the Bsfilter databases."
  :type '(choice (directory :tag "Location of the Bsfilter database directory")
		 (const :tag "Use the default"))
  :group 'elmo-spam-bsfilter)

(defcustom elmo-spam-bsfilter-max-files-per-process 100
  "Number of files processed at once."
  :type 'integer
  :group 'elmo-spam-bsfilter)

(defcustom elmo-spam-bsfilter-max-messages-per-process 30
  "Number of messages processed at once."
  :type 'integer
  :group 'elmo-spam-bsfilter)

(defcustom elmo-spam-bsfilter-use-remote
  (and elmo-spam-bsfilter-shell-program
       (let ((program (file-name-nondirectory
		       elmo-spam-bsfilter-shell-program))
	     (remote-shells '("ssh" "rsh")))
	 (catch 'found
	   (dolist (shell remote-shells)
	     (when (string-match (concat "\\`" shell) program)
	       (throw 'found t)))
	   nil)))
  "*Non-nil disables local file feature."
  :type 'boolean
  :group 'elmo-spam-bsfilter)

(defcustom elmo-spam-bsfilter-debug nil
  "Non-nil to debug elmo bsfilter spam backend."
  :type 'boolean
  :group 'elmo-spam-bsfilter)

(eval-and-compile
  (luna-define-class elsp-bsfilter (elsp-generic)))

(defsubst elsp-bsfilter-call-bsfilter (&rest args)
  (apply #'call-process-region
	 (point-min) (point-max)
	 elmo-spam-bsfilter-shell-program
	 nil (if elmo-spam-bsfilter-debug
		 (get-buffer-create "*Debug ELMO Bsfilter*"))
	 nil
	 (append (if elmo-spam-bsfilter-shell-switch
		     (list elmo-spam-bsfilter-shell-switch))
		 (if elmo-spam-bsfilter-program
		     (list elmo-spam-bsfilter-program))
		 elmo-spam-bsfilter-args
		 (if elmo-spam-bsfilter-database-directory
		     (list "--homedir" elmo-spam-bsfilter-database-directory))
		 (elmo-flatten args))))

(luna-define-method elmo-spam-buffer-spam-p ((processor elsp-bsfilter)
					     buffer &optional register)
  (with-current-buffer buffer
    (= 0 (elsp-bsfilter-call-bsfilter
	  (if register elmo-spam-bsfilter-update-switch)))))

(defun elsp-bsfilter-list-spam-filter (process output)
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (save-excursion
	(goto-char (process-mark process))
	(insert output)
	(set-marker (process-mark process) (point)))
      (while (re-search-forward "^combined probability.+\r?\n" nil t)
	(delete-region (match-beginning 0) (match-end 0))
	(elmo-progress-notify 'elmo-spam-check-spam))
      (when elmo-spam-bsfilter-debug
	(with-current-buffer (get-buffer-create "*Debug ELMO Bsfilter*")
	  (goto-char (point-max))
	  (insert output))))))

(defsubst elsp-bsfilter-start-list-spam (targets)
  (let ((process
	 (apply #'start-process
		"elsp-bsfilter"
		(current-buffer)
		elmo-spam-bsfilter-shell-program
		(append (if elmo-spam-bsfilter-shell-switch
			    (list elmo-spam-bsfilter-shell-switch))
			(if elmo-spam-bsfilter-program
			    (list elmo-spam-bsfilter-program))
			elmo-spam-bsfilter-args
			(list "--list-spam")
			(if elmo-spam-bsfilter-database-directory
			    (list "--homedir"
				  elmo-spam-bsfilter-database-directory))
			targets))))
    (set-process-filter process #'elsp-bsfilter-list-spam-filter)
    process))

(defsubst elsp-bsfilter-read-list-spam (results hash)
  (goto-char (point-min))
  (while (not (eobp))
    (let* ((filename (buffer-substring (point) (save-excursion
						 (end-of-line)
						 (point))))
	   (number (elmo-get-hash-val filename hash)))
      (when number
	(setq results (cons number results)))
      (forward-line)))
  results)

(luna-define-method elmo-spam-list-spam-messages :around
  ((processor elsp-bsfilter) folder &optional numbers)
  (if (or elmo-spam-bsfilter-use-remote
	  (not (elmo-folder-message-file-p folder)))
      (luna-call-next-method)
    (let* ((nth-of-targets (1- (or elmo-spam-bsfilter-max-files-per-process
				   100)))
	   (numbers (or numbers (elmo-folder-list-messages folder t t)))
	   (hash (elmo-make-hash (length numbers)))
	   (targets (mapcar
		     (lambda (number)
		       (let ((filename (elmo-message-file-name folder number)))
			 (elmo-set-hash-val filename number hash)
			 filename))
		     numbers))
	   results)
      (with-temp-buffer
	(while targets
	  (let* ((last (nthcdr nth-of-targets targets))
		 (next (cdr last)))
	    (when last
	      (setcdr last nil))
	    (let ((process (elsp-bsfilter-start-list-spam targets)))
	      (while (memq (process-status process) '(open run))
		(accept-process-output process 1))
	      (setq results (elsp-bsfilter-read-list-spam results hash)))
	    (erase-buffer)
	    (setq targets next))))
      results)))


(defsubst elsp-bsfilter-register-buffer (buffer spam restore &optional mbox)
  (with-current-buffer buffer
    (elsp-bsfilter-call-bsfilter
     "--update"
     (if restore (if spam "--sub-clean" "--sub-spam"))
     (if spam "--add-spam" "--add-clean")
     (if mbox "--mbox"))))

(luna-define-method elmo-spam-register-spam-buffer ((processor elsp-bsfilter)
						    buffer &optional restore)
  (elsp-bsfilter-register-buffer buffer t restore))

(luna-define-method elmo-spam-register-good-buffer ((processor elsp-bsfilter)
						    buffer &optional restore)
  (elsp-bsfilter-register-buffer buffer nil restore))

(defsubst elmo-spam-bsfilter-register-messages (folder numbers spam restore)
  (let ((numbers (or numbers (elmo-folder-list-messages folder t t))))
    (if (and (> (length numbers) 1)
	     elmo-spam-bsfilter-max-messages-per-process
	     (> elmo-spam-bsfilter-max-messages-per-process 0))
	(elmo-spam-process-messages-as-mbox
	 folder numbers
	 elmo-spam-bsfilter-max-messages-per-process
	 (lambda (count spam restore)
	   (elsp-bsfilter-register-buffer (current-buffer) spam restore 'mbox)
	   (elmo-progress-notify 'elmo-spam-register count))
	 spam restore)
      (luna-call-next-method))))

(luna-define-method elmo-spam-register-spam-messages :around
  ((processor elsp-bsfilter) folder &optional numbers restore)
  (elmo-spam-bsfilter-register-messages folder numbers t restore))

(luna-define-method elmo-spam-register-good-messages :around
  ((processor elsp-bsfilter) folder &optional numbers restore)
  (elmo-spam-bsfilter-register-messages folder numbers nil restore))

(require 'product)
(product-provide (provide 'elsp-bsfilter) (require 'elmo-version))

;;; elsp-bsfilter.el ends here
