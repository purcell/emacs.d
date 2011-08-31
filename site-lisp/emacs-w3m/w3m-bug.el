;;; w3m-bug.el --- command to report emacs-w3m bugs -*- coding: euc-japan -*-

;; Copyright (C) 2002, 2003, 2005, 2007, 2010
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: w3m, WWW, hypermedia

;; This file is a part of emacs-w3m.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; `M-x report-emacs-w3m-bug' starts an email note to the emacs-w3m
;; developers describing a problem.

;;; Code:

(defvar report-emacs-w3m-bug-address "emacs-w3m@namazu.org"
  "*Address of mailing list for emacs-w3m bugs.")

(defvar report-emacs-w3m-bug-no-explanations nil
  "*If non-nil, suppress the explanations given for the sake of novice users.")

(defconst report-emacs-w3m-bug-system-informations
  (eval
   '`(emacs-w3m-version
      emacs-version
      ,@(if (or (boundp 'mule-version)
		(functionp 'mule-version))
	    '(mule-version))
      ,@(cond ((featurep 'xemacs)
	       '((featurep 'mule)
		 (featurep 'file-coding)))
	      ((or (boundp 'Meadow-version)
		   (functionp 'Meadow-version))
	       '(Meadow-version)))
      system-type
      (featurep 'gtk)
      w3m-version
      w3m-type
      w3m-compile-options
      w3m-language
      w3m-command-arguments
      w3m-command-arguments-alist
      w3m-command-environment
      w3m-input-coding-system
      w3m-output-coding-system
      w3m-use-mule-ucs))
  "List of the system informations.  Users should NEVER modify the value."
  ;; For the developers:
  ;; It is possible that it would be a security hole.  To prevent those
  ;; rogue attacks, this constant should be reloaded for each time to
  ;; send a bug report.  Each element can be the symbol of a variable,
  ;; a Lisp function with no argument or any Lisp form to be evaluated.
  )

(eval-when-compile
  (require 'cl))

(defun report-emacs-w3m-bug (topic &optional buffer)
  "Report a bug in emacs-w3m.
Prompts for bug subject.  Leaves you in a mail buffer."
  (interactive
   (let* ((buffer (current-buffer))
	  (buffers (cons buffer (delq buffer (buffer-list))))
	  (inhibit-point-motion-hooks t)
	  keymap)
     (save-current-buffer
       (while buffers
	 (setq buffer (car buffers)
	       buffers (cdr buffers))
	 (set-buffer buffer)
	 (save-restriction
	   (widen)
	   (if (or (eq major-mode 'w3m-mode)
		   (and (keymapp (setq keymap
				       (or (get-text-property
					    (max (1- (point-max)) (point-min))
					    'keymap)
					   (get-text-property
					    (max (1- (point-max)) (point-min))
					    'local-map)))))
		   (where-is-internal 'w3m-print-current-url keymap))
	       (setq buffers nil)
	     (setq buffer nil)))))
     (list (read-string "Bug Subject: ") buffer)))
  (let (after-load-alist)
    ;; See the comment for `report-emacs-w3m-bug-system-informations'.
    (load "w3m-bug"))
  (compose-mail report-emacs-w3m-bug-address topic nil 'new)
  (goto-char (point-min))
  (re-search-forward (concat "^" (regexp-quote mail-header-separator) "$"))
  (forward-line 1)
  (unless buffer
    (insert
     (if (and (boundp 'w3m-language)
	      (equal (symbol-value 'w3m-language) "Japanese"))
	 "もし可能なら emacs-w3m を起動してからやり直してください。\n"
       "It is if possible, please redo after starting emacs-w3m.\n")
     "\
================================================================\n"))
  (unless report-emacs-w3m-bug-no-explanations
    ;; Insert warnings for the novice users.
    (if (and (boundp 'w3m-language)
	     (equal (symbol-value 'w3m-language) "Japanese"))
	(progn
	  (insert "このバグリポートは emacs-w3m 開発チームに送られます。\n")
	  (put-text-property (point)
			     (progn
			       (insert "\
あなたのローカルサイトの管理者宛てではありません!!")
			       (point))
			     'face 'underline)
	  (insert "\n\nできるだけ簡潔に述べてください:
\t- 何が起きましたか?
\t- 本当はどうなるべきだったと思いますか?
\t- そのとき何をしましたか? (正確に)

もし Lisp のバックトレースがあれば添付してください。\n"))
      (insert "\
This bug report will be sent to the emacs-w3m development team,\n")
      (put-text-property (point)
			 (progn
			   (insert " not to your local site managers!!")
			   (point))
			 'face 'italic)
      (insert "\nPlease write in ")
      (put-text-property (point) (progn
				   (insert "simple")
				   (point))
			 'face 'italic)
      (insert " English, because the emacs-w3m developers
aren't good at English reading. ;-)

Please describe as succinctly as possible:
\t- What happened.
\t- What you thought should have happened.
\t- Precisely what you were doing at the time.

Please also include any Lisp back-traces that you may have.\n"))
    (insert "\
================================================================\n"))
  (insert "Dear Bug Team!\n\n")
  (let ((user-point (point))
	(print-escape-newlines t)
	(print-quoted t)
	infos print-length print-level)
    (insert "\n
================================================================

System Info to help track down your bug:
---------------------------------------\n")
    (with-current-buffer (or buffer (current-buffer))
      (dolist (info report-emacs-w3m-bug-system-informations)
	(push (prin1-to-string info) infos)
	(push "\n => " infos)
	(push (cond ((functionp info)
		     (prin1-to-string (condition-case code
					  (funcall info)
					(error
					 code))))
		    ((symbolp info)
		     (prin1-to-string (condition-case code
					  (symbol-value info)
					(error
					 code))))
		    ((consp info)
		     (prin1-to-string (condition-case code
					  (eval info)
					(error
					 code)))))
	      infos)
	(push "\n" infos)))
    (apply 'insert (nreverse infos))
    (goto-char user-point)))

;;; w3m-bug.el ends here
