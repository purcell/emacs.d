;;; elsp-sa.el --- SpamAssassin support for elmo-spam.
;; Copyright (C) 2004 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
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

(defgroup elmo-spam-spamassassin nil
  "Spam SpamAssassin configuration."
  :group 'elmo-spam)

(defcustom elmo-spam-spamassassin-program "spamassassin"
  "Program name for SpamAssassin."
  :type '(file :tag "Program name of SpamAssassin.")
  :group 'elmo-spam-spamassassin)

(defcustom elmo-spam-spamassassin-learn-program "sa-learn"
  "Program name for SpamAssassin Learner."
  :type '(file :tag "Program name of SpamAssassin Learner.")
  :group 'elmo-spam-spamassassin)

(defcustom elmo-spam-spamassassin-program-arguments '("-e")
  "Program argument list for SpamAssassin."
  :type '(file :tag "Program name of SpamAssassin Learner.")
  :group 'elmo-spam-spamassassin)

(defcustom elmo-spam-spamassassin-learn-program-arguments nil
  "Program argument list for SpamAssassin Learner."
  :type '(file :tag "Program name of SpamAssassin Learner.")
  :group 'elmo-spam-spamassassin)

(defcustom elmo-spam-spamassassin-max-messages-per-process 30
  "Number of messages processed at once."
  :type 'integer
  :group 'elmo-spam-spamassassin)

(defcustom elmo-spamassassin-debug nil
  "Non-nil to debug elmo spamassassin spam backend."
  :type 'boolean
  :group 'elmo-spam-spamassassin)

(eval-and-compile
  (luna-define-class elsp-sa (elsp-generic))
  (luna-define-internal-accessors 'elsp-sa))

(defun elmo-spamassassin-call (type &rest args)
  (let ((pair (cond
	       ((eq type 'check)
		(cons elmo-spam-spamassassin-program
		      elmo-spam-spamassassin-program-arguments))
	       ((eq type 'learn)
		(cons
		 elmo-spam-spamassassin-learn-program
		 elmo-spam-spamassassin-learn-program-arguments))
	       (t (error "Internal error")))))
    (apply #'call-process-region
	   (point-min) (point-max)
	   (car pair)
	   nil (if elmo-spamassassin-debug
		   (get-buffer-create "*Debug ELMO SpamAssassin*"))
	   nil (delq nil (append (cdr pair) args)))))

(luna-define-method elmo-spam-buffer-spam-p ((processor elsp-sa)
					     buffer &optional register)
  (let ((result (with-current-buffer buffer
		  (not (eq 0 (elmo-spamassassin-call 'check))))))
    (when register
      (if result
	  (elmo-spam-register-spam-buffer processor buffer)
	(elmo-spam-register-good-buffer processor buffer)))
    result))

(luna-define-method elmo-spam-register-spam-buffer ((processor elsp-sa)
						    buffer &optional restore)
  (with-current-buffer buffer
    (eq 0 (apply 'elmo-spamassassin-call 'learn
		 (list "--spam")))))

(luna-define-method elmo-spam-register-good-buffer ((processor elsp-sa)
						    buffer &optional restore)
  (with-current-buffer buffer
    (eq 0 (apply 'elmo-spamassassin-call 'learn
		 (list "--ham")))))

(defsubst elmo-spam-spamassassin-register-messages (folder
						    numbers
						    spam
						    restore)
  (if (not (< 0 elmo-spam-spamassassin-max-messages-per-process))
      (error "\
non-positive value for `elmo-spam-spamassassin-max-messages-per-process'"))
  (elmo-spam-process-messages-as-mbox
   folder numbers elmo-spam-spamassassin-max-messages-per-process
   (lambda (count spam restore)
     (apply 'elmo-spamassassin-call 'learn
	    (delq nil
		  (list "--mbox"
			(if spam "--spam" "--ham"))))
     (elmo-progress-notify 'elmo-spam-register count))
   spam restore))

(luna-define-method elmo-spam-register-spam-messages :around
  ((processor elsp-sa) folder &optional numbers restore)
  (let ((numbers (or numbers (elmo-folder-list-messages folder t t))))
    (if (> (length numbers) 1)
	(elmo-spam-spamassassin-register-messages folder numbers t restore)
      (luna-call-next-method))))

(luna-define-method elmo-spam-register-good-messages :around
  ((processor elsp-sa) folder &optional numbers restore)
  (let ((numbers (or numbers (elmo-folder-list-messages folder t t))))
    (if (> (length numbers) 1)
	(elmo-spam-spamassassin-register-messages folder numbers nil restore)
      (luna-call-next-method))))

(require 'product)
(product-provide (provide 'elsp-sa) (require 'elmo-version))

;;; elsp-sa.el ends here
