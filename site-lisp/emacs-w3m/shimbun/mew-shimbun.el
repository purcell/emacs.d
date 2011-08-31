;; -*- mode: emacs-lisp -*-
;; mew-shimbun.el --- View shimbun contents with Mew

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2010
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;;         Hideyuki SHIRAI <shirai@meadowy.org>
;; Keywords: Mew, shimbun, w3m, WWW, hypermedia

;; This file is a part of emacs-w3m.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This package is `Shimbun' interface for Mew version 2.1 or later.
;; SHIMBUN 

;;; Instalation & Usage:
;; Please read the emacs-w3m info (C-h i m emacs-w3m(-ja) RET m Mew Shimbun RET).
;;

;;; Code:
;; disable runtime cl
(eval-when-compile
  (require 'cl))

(eval-when-compile
  (unless (dolist (var nil t))
    (load "cl-macs" nil t)))

(eval-and-compile
  (require 'shimbun)
  (require 'mew))

;; Avoid byte-compile warnings,
(eval-when-compile
  (unless (fboundp 'MEW-FLD)
    (defun MEW-FLD () ()))
  (unless (fboundp 'MEW-ID)
    (defun MEW-ID () ()))
  (unless (fboundp 'MEW-TO)
    (defun MEW-TO () ())
  (unless (fboundp 'MEW-SHIMBUN-STS)
    (defun MEW-SHIMBUN-STS () ())))
  (unless (fboundp 'mew-set-file-modes)
    (defun mew-set-file-modes (path)))
  (unless (fboundp 'mew-biff-setup)
    (defun mew-biff-setup ()))
  (unless (fboundp 'mew-biff-clean-up)
    (defun mew-biff-clean-up ()))
  (defvar mew-use-biff)
  (defvar mew-file-mode)
  (defvar mew-folder-list)
  (defvar mew-local-folder-list)
  (defvar mew-local-folder-alist))

;; Variables
(defgroup mew-shimbun nil
  "SHIMBUN environment for Mew."
  :group 'mew)

(defcustom mew-shimbun-folder "+shimbun"
  "*The folder where SHIMBUN are contained."
  :group 'shimbun
  :group 'mew-shimbun
  :type '(string :size 0))

(defcustom mew-shimbun-folder-groups nil
  "*Alist of `shimbun folder name (exclude `mew-shimbun-folder')'
and included `shimbun server.groups' and its `range parameters',
show below example,
  '((\"yomiuri\"		;; \"shimbun folder\"
     (\"yomiuri.shakai\" . 2)	;; (\"server.group\" . range)
     (\"yomiuri.sports\". 2)
     (\"yomiuri.seiji\" . 2)
     (\"yomiuri.kokusai\". 1))
    (\"comp\"
     (\"cnet.comp\" . last)
     (\"zdnet.comp\" . last))
    (\"mew/mgp\"
     (\"mew.mgp-users\" . last)
     (\"mew.mgp-users-jp\" . last))
    (\"mew/mew-int\"
     (\"mew.mew-int\" . last)))
"
  :group 'shimbun
  :group 'mew-shimbun
  :type '(repeat
	  (cons
	   :format "%v" :indent 2
	   (string :format "Folder: %v\n" :size 0)
	   (repeat
	    :format "%{Server.Group + Index_Checking_Range%}:\n %v%i\n"
	    :indent 3 :sample-face underline
	    (cons :format "%v" :indent 6
		  (string :format "Server.Group: %v\n" :size 0)
		  (radio :format "Range: %v " :value all
			 (const :format "%v " all)
			 (const :format "%v " last)
			 (integer :format "Pages: %v\n" :size 0)))))))

(defcustom mew-shimbun-db-file ".mew-shimbun-db"
  "*File name of mew-shimbun database."
  :group 'shimbun
  :group 'mew-shimbun
  :type '(file :size 0))

(defcustom mew-shimbun-expires nil
  "*Alist of `shimbun folder name' and expire days.
Show below expire,
  '((\"yomiuri\" . 7)
    (\"comp\" . 3)
    (\"mew/mgp\" . nil)) ;; not expire
"
  :group 'shimbun
  :group 'mew-shimbun
  :type '(repeat
	  (cons :format "%v" :indent 11
		(string :format "Folder: %v\n" :size 0)
		(integer :format "Days: %v\n" :size 0))))

(defcustom mew-shimbun-use-expire-pack nil
  "*If non-nin, exec `pack' after expire."
  :group 'shimbun
  :group 'mew-shimbun
  :type 'boolean)

(defcustom mew-shimbun-db-length nil
  "*Max length of mew-shimbun database.
If nil, same 'mew-lisp-max-length'.
If integer, all server.group limit 'integer'.
If alist, each cell has shimbun folder names and their max length,
show below example,

  '((\"mew/mgp\" . 1000)
    (\"tcup/meadow\" . 20)
    (\"asahi\" . 100)
    (\"slashdot-jp/story\" . 3000)
    (t . 2000))
"
  :group 'shimbun
  :group 'mew-shimbun
  :type '(radio
	  (const :tag "Same as `mew-lisp-max-length'" nil)
	  (integer :format "Limit for all groups: %v\n"
		   :size 0 :value 2000)
	  (repeat :indent 4 :tag "Alist of folders and lengths"
		  (cons :format "%v" :indent 8
			(radio :format "%v" :value t
			       (const :format "Other " t)
			       (string :format "Folder: %v\n" :size 0))
			(integer :format "Maximum length of database: %v\n"
				 :size 0 :value 2000)))))

(defcustom mew-shimbun-unknown-from "foo@bar.baz"
  "*Shimbun mail address when From header is strange."
  :group 'shimbun
  :group 'mew-shimbun
  :type '(string :size 0))

(defcustom mew-shimbun-mark-re-retrieve (or (and (boundp 'mew-mark-multi)
						 mew-mark-multi)
					    mew-mark-review)
  "*Shimbun re-retrieve mark."
  :group 'shimbun
  :group 'mew-shimbun
  :type 'character)

(defcustom mew-shimbun-mark-unseen (or (and (boundp 'mew-mark-unread)
					    mew-mark-unread)
				       mew-mark-review)
  "*Shimbun unseen mark."
  :group 'shimbun
  :group 'mew-shimbun
  :type 'character)

(defcustom mew-shimbun-use-unseen nil
  "*If non-nil, SHIMBUN folder support the 'unseen' mark."
  :group 'shimbun
  :group 'mew-shimbun
  :type 'boolean)

(defcustom mew-shimbun-use-unseen-cache-save nil
  "*If non-nin, save '.mew-cache' whenever remove the 'unseen' mark."
  :group 'shimbun
  :group 'mew-shimbun
  :type 'boolean)

(defcustom mew-shimbun-before-retrieve-hook nil
  "*Hook run after mew-shimbun-retrieve called."
  :group 'shimbun
  :group 'mew-shimbun
  :type 'hook)

(defcustom mew-shimbun-retrieve-hook nil
  "*Hook run after mew-shimbun-retrieve called."
  :group 'shimbun
  :group 'mew-shimbun
  :type 'hook)

(defconst mew-shimbun-id-format "%s+%s:%s")
(defconst mew-shimbun-db-buffer-name " *mew-shimbun-overview*")
(defconst mew-shimbun-article-buffer-name " *mew-shimbun-article*")

(defvar mew-shimbun-unseen-regex nil)

(defvar mew-shimbun-folder-regex
  (mew-folder-regex (file-name-as-directory mew-shimbun-folder)))

(defvar mew-shimbun-db nil)
(defvar mew-shimbun-db2 nil)
(defvar mew-shimbun-input-hist nil)

;;; Macro:
(eval-when-compile
  (if (fboundp 'static-if)
      nil
    (defmacro static-if (cond then &rest else)
      ;; Like `if', but evaluate COND at compile time.
      (if (eval cond)
	  then
	`(progn ,@else)))))

(defmacro mew-shimbun-db-search-id (id)
  `(assoc ,id mew-shimbun-db))

(defmacro mew-shimbun-db-search-id2 (id)
  `(assoc ,id mew-shimbun-db2))

(defsubst mew-shimbun-folder-p (fld)
  (if (string-match mew-shimbun-folder-regex fld) t nil))

(defvar mew-shimbun-lock-format1 "<%s@%s>")
(defvar mew-shimbun-lock-format2 "<%s@%s:%d/%d/%d>")

(defmacro mew-shimbun-element-body (sgr group server &rest body)
  `(when (string-match "\\([^.]+\\)\\.\\(.+\\)" (car ,sgr))
     (let ((server (match-string 1 (car ,sgr)))
	   (group (match-string 2 (car ,sgr)))
	   (range (cdr ,sgr)))
       (mew-summary-lock 'shimbun
			 (format mew-shimbun-lock-format1 ,group ,server))
       (force-mode-line-update)
       ,@body)))

(put 'mew-shimbun-element-body 'lisp-indent-function 1)

(defmacro mew-shimbun-headers (shimbun range)
  `(let ((w3m-process-wait-discard-input t))
     (shimbun-headers ,shimbun ,range)))

(defmacro mew-shimbun-article (shimbun head)
  `(let ((w3m-process-wait-discard-input t))
     (shimbun-article ,shimbun ,head)))

(defsubst mew-shimbun-mode-display (group server get count sum)
  (mew-summary-lock 'shimbun
		    (format mew-shimbun-lock-format2 group server get count sum))
  (force-mode-line-update))

(static-if (fboundp 'mew-summary-visit-folder)
    (defalias 'mew-shimbun-visit-folder 'mew-summary-visit-folder)
  (defun mew-shimbun-visit-folder (folder)
    (mew-summary-ls
     (mew-summary-switch-to-folder folder))))

(defun mew-shimbun-unseen-regex ()
  (static-if (boundp 'mew-regex-msg)
      ;; Mew3
      (setq mew-shimbun-unseen-regex
	    (concat mew-regex-msg (regexp-quote (string mew-shimbun-mark-unseen))))
    ;; Mew4
    (setq mew-shimbun-unseen-regex
	  (concat "^" (regexp-quote (string mew-shimbun-mark-unseen))))))
  
(defun mew-shimbun-set-form (fld)
  (static-if (fboundp 'mew-summary-scan-form)
      ;; Mew3
      (unless (mew-sinfo-get-scan-form)
	(mew-sinfo-set-scan-form (mew-summary-scan-form fld)))
    ;; Mew4
    (unless (mew-sinfo-get-summary-form)
      (mew-sinfo-set-summary-form (mew-get-summary-form fld)))))

(static-if (fboundp 'mew-expand-file)
    ;; Mew 5
    (defalias 'mew-shimbun-folder-file 'mew-expand-file)
  (defun mew-shimbun-folder-file (fld file)
    (expand-file-name file (mew-expand-folder fld))))

(static-if (fboundp 'mew-expand-msg)
    ;; Mew 5
    (defalias 'mew-shimbun-expand-msg 'mew-expand-msg)
  (defun mew-shimbun-expand-msg (fld msg)
    (expand-file-name msg (mew-expand-folder fld))))

(if (featurep 'xemacs)
    nil
  (eval-and-compile
    (autoload 'ad-arglist "advice"))
  (eval-when-compile
    (defmacro function-max-args (function)
      ;; Return the maximum number of arguments a function may be called with.
      ;; The function may be any form that can be passed to `funcall',
      ;; any special form, or any macro.
      ;; If the function takes an arbitrary number of arguments or is
      ;; a built-in special form, nil is returned."
      (let ((fn (make-symbol "emulating-function-max-args-function"))
	    (arglist (make-symbol "emulating-function-max-args-arglist")))
	`(let* ((,fn ,function)
		(,arglist (ad-arglist (progn
					(while (symbolp ,fn)
					  (setq ,fn (symbol-function ,fn)))
					,fn))))
	   (cond ((memq '&rest ,arglist)
		  nil)
		 ((memq '&optional ,arglist)
		  (1- (length ,arglist)))
		 (t
		  (length ,arglist))))))))

;;; Main:
;;;###autoload
(defun mew-shimbun-goto-unseen-folder ()
  "Goto folder for SHIMBUN to have a few new messages."
  (interactive)
  (mew-shimbun-goto-folder t))

;;;###autoload
(defun mew-shimbun-goto-folder (&optional args)
  "Goto folder for SHIMBUN.
If called with '\\[universal-argument]', goto folder to have a few new messages."
  (interactive "P")
  (let ((flds (or (and (boundp 'mew-folder-list) mew-folder-list)
		  (and (boundp 'mew-local-folder-list) mew-local-folder-list)
		  (and (boundp 'mew-local-folder-alist)
		       (mapcar 'car mew-local-folder-alist))))
	sbflds alst fld cfile removes)
    (save-excursion
      (dolist (fld flds)
	(when (and (mew-shimbun-folder-p fld)
		   (file-exists-p
		    (expand-file-name mew-shimbun-db-file
				      (mew-expand-folder fld))))
	  (when (string-match "/$" fld)
	    (setq removes (cons (substring fld 0 (match-beginning 0)) removes)))
	  (if (null args)
	      (setq sbflds (cons fld sbflds))
	    (if (mew-shimbun-folder-new-p fld)
		(setq sbflds (cons fld sbflds))
	      (if (get-buffer fld)
		  (with-current-buffer fld
		    (goto-char (point-min))
		    (when (re-search-forward (or mew-shimbun-unseen-regex
						 (mew-shimbun-unseen-regex)) nil t)
		      (setq sbflds (cons fld sbflds))))
		(setq cfile (mew-shimbun-folder-file fld mew-summary-cache-file))
		(when (file-readable-p cfile)
		  (with-temp-buffer
		    (mew-frwlet
		     mew-cs-text-for-read mew-cs-dummy
		     (insert-file-contents cfile nil)
		     (goto-char (point-min))
		     (when (re-search-forward (or mew-shimbun-unseen-regex
						  (mew-shimbun-unseen-regex)) nil t)
		       (setq sbflds (cons fld sbflds))))))))))))
    (mapc (lambda (x)
	    (unless (member x removes)
	      (setq alst (cons (list x) alst))))
	  sbflds)
    (let ((completion-ignore-case mew-complete-folder-ignore-case))
      (setq fld (completing-read
		 (if args
		     "Shimbun UNREAD folder: "
		   "Shimbun folder: ")
		 alst
		 nil t (file-name-as-directory mew-shimbun-folder)
		 'mew-shimbun-input-hist)))
    (when (string-match "[*%]$" fld)
      (setq fld (substring fld 0 (match-beginning 0)))
      (setcar mew-shimbun-input-hist fld))
    (setq mew-input-folder-hist (cons fld mew-input-folder-hist))
    (setq fld (directory-file-name fld))
    (let ((newfld (mew-summary-switch-to-folder fld)))
      (if (eq 1 (function-max-args 'mew-summary-ls))
	  (mew-summary-ls newfld)
	(dont-compile;; To avoid a byte-compile warnning.
	  (mew-summary-ls newfld newfld))))))

;;;###autoload
(defun mew-shimbun-retrieve (&optional newfld)
  "Retrieve articles via SHIMBUN on this folder."
  (interactive)
  (when (mew-summary-exclusive-p)
    (mew-summary-only
     (let ((fld (mew-summary-folder-name 'ext))
	   (mua (luna-make-entity 'shimbun-mew-mua))
	   (count 0)
	   alst server group range)
       (if (not (mew-shimbun-folder-p fld))
	   (message "This command can not execute here")
	 (setq alst (assoc (substring fld (match-end 0)) mew-shimbun-folder-groups))
	 (if (null alst)
	     (message "%s is not include 'mew-shimbun-folder-groups'" fld)
	   (run-hooks 'mew-shimbun-before-retrieve-hook)
	   (mew-window-configure 'summary)
	   (mew-current-set nil nil nil)
	   (mew-decode-syntax-delete)
	   (mew-shimbun-set-form fld)
	   (save-excursion
	     (dolist (sgr (cdr alst))
	       (mew-shimbun-element-body sgr group server
		 (setq count
		       (+ (mew-shimbun-retrieve-article
			   mua server group range fld newfld)
			  count)))))
	   (run-hooks 'mew-shimbun-retrieve-hook)
	   (message "Getting %s %s in '%s' done"
		    (if (= count 0) "no" (number-to-string count))
		    (if (> count 1) "messages" "message")
		    fld)
	   (when (> count 0)
	     (mew-summary-folder-cache-save))))))))

;;;###autoload
(defun mew-shimbun-retrieve-all ()
  "Retrieve all articles via SHIMBUN."
  (interactive)
  (mew-summary-only
   (let ((mua (luna-make-entity 'shimbun-mew-mua))
	 (cfld (mew-summary-folder-name 'ext))
	 fld dir server group range newfld)
     (run-hooks 'mew-shimbun-before-retrieve-hook)
     (mew-window-configure 'summary)
     (mew-current-set nil nil nil)
     (mew-decode-syntax-delete)
     (save-excursion
       (dolist (fldgrp mew-shimbun-folder-groups)
	 (setq fld (concat (file-name-as-directory mew-shimbun-folder)
			   (car fldgrp)))
	 (setq dir (mew-expand-folder fld))
	 (unless (file-directory-p dir)
	   (mew-make-directory dir)
	   (setq newfld t))
	 (mew-shimbun-visit-folder fld)
	 (sit-for 0.5)
	 (mew-rendezvous mew-summary-buffer-process)
	 (mew-shimbun-retrieve newfld)
	 (unless (eq (get-buffer cfld) (current-buffer))
	   (mew-kill-buffer (current-buffer)))))
     (mew-shimbun-visit-folder cfld)
     (message "Getting done"))))

(defun mew-shimbun-retrieve-article (mua server group range fld &optional newfld)
  "Retrieve articles via SHIMBUN."
  (luna-define-method shimbun-mua-search-id ((mua shimbun-mew-mua) id)
    (let ((shimbun (shimbun-mua-shimbun mua)))
      (mew-shimbun-db-search-id
       (format mew-shimbun-id-format
	       (shimbun-server shimbun)
	       (shimbun-current-group shimbun)
	       id))))
  (let ((shimbun (shimbun-open server mua))
	(biff (if (and (boundp 'mew-use-biff)
		       (fboundp 'mew-biff-setup)
		       (fboundp 'mew-biff-clean-up))
		  mew-use-biff))
	(count 0)
	(dispcount 0)
	msg file)
    (if biff (mew-biff-clean-up))
    (shimbun-open-group shimbun group)
    (unless (file-exists-p (mew-expand-folder fld))
      (setq newfld t)
      (mew-make-directory (mew-expand-folder fld)))
    (mew-shimbun-db-setup fld)
    (unwind-protect
	(let* ((headers (mew-shimbun-headers shimbun range))
	       (sum (length headers)))
	  (setq headers (sort headers
			      (lambda (x y)
				(string< (mew-time-rfc-to-sortkey (or (elt x 3) ""))
					 (mew-time-rfc-to-sortkey (or (elt y 3) ""))))))
	  (dolist (head headers)
	    (let ((id (format mew-shimbun-id-format
			      server group
			      (shimbun-header-id head)))
		  buf md5)
	      (unless (mew-shimbun-db-search-id id)
		(setq buf (get-buffer-create mew-shimbun-article-buffer-name))
		(with-current-buffer buf
		  (mew-erase-buffer)
		  (set-buffer-multibyte nil)
		  (mew-shimbun-article shimbun head)
		  (setq md5 (mew-shimbun-md5))
		  (when (and (> (buffer-size) 0)
			     (mew-shimbun-db-add-id id md5))
		    (setq count (1+ count))
		    (goto-char (point-min))
		    (insert (format "X-Shimbun-Id: %s\n" id))
		    (mew-shimbun-sanity-convert)
		    (setq msg (mew-folder-new-message fld 'numonly))
		    (setq file (mew-shimbun-expand-msg fld msg))
		    (mew-frwlet
		     mew-cs-dummy mew-cs-text-for-write
		     (write-region (point-min) (point-max) file nil 'nomsg))
		    (if (boundp 'mew-file-mode)
			(set-file-modes file mew-file-mode)
		      (mew-set-file-modes file))
		    (mew-shimbun-scan-message fld msg)))
		(kill-buffer buf))
	      (setq dispcount (1+ dispcount))
	      (mew-shimbun-mode-display group server count dispcount sum))))
      (mew-summary-unlock)
      (when newfld
	(static-if (fboundp 'mew-local-folder-insert)
	    (mew-local-folder-insert fld)
	  (mew-folder-insert fld)))
      (if biff (mew-biff-setup))
      (shimbun-close-group shimbun)
      (shimbun-close shimbun)
      (mew-shimbun-db-shutdown fld count))
    count))

;;;###autoload
(defun mew-shimbun-re-retrieve (&optional args)
  "Re-retrieve this message.
If called with '\\[universal-argument]', re-retrieve messages marked with
'mew-shimbun-mark-re-retrieve'."
  (interactive "P")
  (when (mew-summary-exclusive-p)
    (mew-summary-only
     (let* ((fld (mew-summary-folder-name 'ext))
	    (msgs (list (progn (mew-summary-goto-message)
			       (mew-summary-message-number))))
	    (mua (luna-make-entity 'shimbun-mew-mua))
	    (newcount 0) (rplcount 0) (same 0)
	    countlst id-msgs alst server group range)
       (if (not (mew-shimbun-folder-p fld))
	   (message "This command can not execute here")
	 (setq alst (assoc (substring fld (match-end 0))
			   mew-shimbun-folder-groups))
	 (if (null alst)
	     (message "%s is not include 'mew-shimbun-folder-groups'" fld)
	   (run-hooks 'mew-shimbun-before-retrieve-hook)
	   (mew-window-configure 'summary)
	   (mew-current-set nil nil nil)
	   (mew-decode-syntax-delete)
	   (mew-shimbun-set-form fld)
	   (when args
	     (setq msgs (mew-summary-mark-collect mew-shimbun-mark-re-retrieve)))
	   (if (null msgs)
	       (message "No message re-retrieve.")
	     (setq id-msgs (mew-shimbun-get-id-msgs 'list fld msgs))
	     (if id-msgs
		 (save-excursion
		   (dolist (sgr (cdr alst))
		     (mew-shimbun-element-body sgr group server
		       (setq countlst
			     (mew-shimbun-re-retrieve-article
			      mua server group range fld id-msgs))
		       (setq rplcount (+ rplcount (nth 0 countlst)))
		       (setq newcount (+ newcount (nth 1 countlst)))
		       (setq same (+ same (nth 2 countlst)))))
		   (message "Replace %s, new %s, same %s messages in '%s' done"
			    rplcount newcount same fld)
		   (when (> (+ newcount rplcount) 0)
		     (mew-summary-folder-cache-save)))
	       (message "No detect 'X-Shimbun-Id:'"))
	     (run-hooks 'mew-shimbun-retrieve-hook))))))))

;;;###autoload
(defun mew-shimbun-re-retrieve-all (&optional arg)
  "Re-retrieve all messages in this folder.
If called with '\\[universal-argument]', re-retrieve messages in the region."
  (interactive "P")
  (when (mew-summary-exclusive-p)
    (mew-summary-only
     (let* ((fld (mew-summary-folder-name 'ext))
	    (mua (luna-make-entity 'shimbun-mew-mua))
	    (begend (cons (point-min) (point-max)))
	    (newcount 0) (rplcount 0) (same 0)
	    countlst id-msgs begmsg endmsg alst server group range)
       (if (not (mew-shimbun-folder-p fld))
	   (message "This command can not execute here")
	 (setq alst (assoc (substring fld (match-end 0))
			   mew-shimbun-folder-groups))
	 (if (null alst)
	     (message "%s is not include 'mew-shimbun-folder-groups'" fld)
	   (when arg
	     (setq begend (mew-summary-get-region)))
	   (save-excursion
	     (save-restriction
	       (narrow-to-region (car begend) (cdr begend))
	       (goto-char (point-min))
	       (mew-summary-goto-message)
	       (setq begmsg (mew-summary-message-number))
	       (goto-char (point-max))
	       (mew-summary-goto-message)
	       (setq endmsg (mew-summary-message-number))))
	   (setq id-msgs (mew-shimbun-get-id-msgs 'range fld begmsg endmsg))
	   (mew-shimbun-set-form fld)
	   (mew-window-configure 'summary)
	   (mew-current-set nil nil nil)
	   (mew-decode-syntax-delete)
	   (run-hooks 'mew-shimbun-before-retrieve-hook)
	   (if id-msgs
	       (save-excursion
		 (dolist (sgr (cdr alst))
		   (mew-shimbun-element-body sgr group server
		     (setq countlst
			   (mew-shimbun-re-retrieve-article
			    mua server group range fld id-msgs))
		     (setq rplcount (+ rplcount (nth 0 countlst)))
		     (setq newcount (+ newcount (nth 1 countlst)))
		     (setq same (+ same (nth 2 countlst)))))
		 (message "Replace %s, new %s, same %s messages in '%s' done"
			  rplcount newcount same fld)
		 (when (> (+ newcount rplcount) 0)
		   (mew-summary-folder-cache-save)))
	     (message "No detect 'X-Shimbun-Id:'"))
	   (run-hooks 'mew-shimbun-retrieve-hook)))))))

(defun mew-shimbun-re-retrieve-article (mua server group range fld id-msgs)
  "Re-retrieve articles via SHIMBUN."
  (luna-define-method shimbun-mua-search-id ((mua shimbun-mew-mua) id)
    (let ((shimbun (shimbun-mua-shimbun mua)))
      (mew-shimbun-db-search-id2
       (format mew-shimbun-id-format
	       (shimbun-server shimbun)
	       (shimbun-current-group shimbun)
	       id))))
  (let ((shimbun (shimbun-open server mua))
	(biff (if (and (boundp 'mew-use-biff) (fboundp 'mew-biff-setup))
		  mew-use-biff))
	(newcount 0) (rplcount 0) (same 0) (dispcount 0))
    (if biff (mew-biff-clean-up))
    (shimbun-open-group shimbun group)
    (mew-shimbun-db-setup2 fld id-msgs)
    (unwind-protect
	(let* ((headers (mew-shimbun-headers shimbun range))
	       (sum (length headers)))
	  (setq headers (sort headers
			      (lambda (x y)
				(string< (mew-time-rfc-to-sortkey (or (elt x 3) ""))
					 (mew-time-rfc-to-sortkey (or (elt y 3) ""))))))
	  (dolist (head headers)
	    (let ((newid (format mew-shimbun-id-format
				 server group
				 (shimbun-header-id head)))
		  newmd5 oldmd5
		  buf alst msg file)
	      (unless (mew-shimbun-db-search-id2 newid)
		(if (setq alst (assoc newid id-msgs))
		    ;; message replace?
		    (progn
		      (setq rplcount (1+ rplcount))
		      (setq msg (cdr alst))
		      (setq oldmd5 (cdr (mew-shimbun-db-search-id newid))))
		  ;; new message
		  (setq newcount (1+ newcount))
		  (setq msg (mew-folder-new-message fld 'numonly))
		  (setq oldmd5 nil))
		(setq file (mew-shimbun-expand-msg fld msg))
		(setq buf (get-buffer-create mew-shimbun-article-buffer-name))
		(with-current-buffer buf
		  (mew-erase-buffer)
		  (set-buffer-multibyte nil)
		  (mew-shimbun-article shimbun head)
		  (when (> (buffer-size) 0)
		    (setq newmd5 (mew-shimbun-md5))
		    (if (and (stringp oldmd5) (string= oldmd5 newmd5))
			;; same message
			(setq rplcount (1- rplcount) same (1+ same))
		      (mew-shimbun-db-add-id newid newmd5 (stringp oldmd5))
		      (goto-char (point-min))
		      (insert (format "X-Shimbun-Id: %s\n" newid))
		      (mew-shimbun-sanity-convert)
		      (mew-frwlet
		       mew-cs-dummy mew-cs-text-for-write
		       (write-region (point-min) (point-max) file nil 'nomsg))
		      (if (boundp 'mew-file-mode)
			  (set-file-modes file mew-file-mode)
			(mew-set-file-modes file))
		      (mew-shimbun-scan-message fld msg))))
		(kill-buffer buf))
	      (setq dispcount (1+ dispcount))
	      (mew-shimbun-mode-display group server
					(+ newcount rplcount) dispcount sum))))
      (mew-summary-unlock)
      (if biff (mew-biff-setup))
      (shimbun-close-group shimbun)
      (shimbun-close shimbun)
      (mew-shimbun-db-shutdown2 fld (+ newcount rplcount)))
    (list rplcount newcount same)))

;;;###autoload
(defun mew-shimbun-expire-all ()
  "Expire all shimbun folder."
  (interactive)
  (let ((cfld (mew-summary-folder-name 'ext)) fld)
    (dolist (alst mew-shimbun-expires)
      (setq fld (concat (file-name-as-directory mew-shimbun-folder)
			(car alst)))
      (when (and (file-directory-p (mew-expand-folder fld))
		 (file-exists-p (expand-file-name mew-shimbun-db-file
						  (mew-expand-folder fld))))
	(mew-shimbun-visit-folder fld)
	(sit-for 0.5)
	(mew-rendezvous mew-summary-buffer-process)
	(mew-shimbun-expire)
	(unless (eq (get-buffer cfld) (current-buffer))
	  (mew-kill-buffer (current-buffer)))))
    (mew-shimbun-visit-folder cfld)))

(defun mew-shimbun-pick (&rest args)
  (apply 'call-process
	 (static-if (boundp 'mew-prog-mewl) mew-prog-mewl mew-prog-mewls)
	 nil t nil args))

(defun mew-shimbun-jump-msg (msg)
  (static-if (fboundp 'mew-regex-jmp-msg)
      (re-search-forward (mew-regex-jmp-msg msg) nil t)
    (re-search-forward (format "\r  %s " msg) nil t)))

;;;###autoload
(defun mew-shimbun-expire ()
  "Expire this shimbun folder."
  (interactive)
  (when (mew-summary-exclusive-p)
    (mew-summary-only
     (let* ((fld (mew-summary-folder-name 'ext))
	    (days (mew-shimbun-expire-day fld))
	    (i 0)
	    file msgs msg-alist begmsg endmsg t1)
       (if (not (mew-shimbun-folder-p fld))
	   (message "This command can not execute here")
	 (if (not days)
	     (message "%s does not have an expire rule." fld)
	   (mew-decode-syntax-delete)
	   (message "Gathering date header in %s..." fld)
	   (save-excursion
	     (save-restriction
	       (widen)
	       (goto-char (point-min))
	       (mew-summary-goto-message)
	       (setq begmsg (mew-summary-message-number))
	       (goto-char (point-max))
	       (mew-summary-goto-message)
	       (setq endmsg (mew-summary-message-number))
	       (with-temp-buffer
		 (mew-piolet
		  mew-cs-text-for-read mew-cs-text-for-write
		  (mew-shimbun-pick "-b" mew-mail-path
				    "-d" "Date:"
				    "-s" (format "%s %s-%s"
						 fld begmsg endmsg))
		  (goto-char (point-min))
		  (while (not (eobp))
		    (when (looking-at "^\\([1-9][0-9]*\\): *\\([^\n]+\\)$")
		      (setq msg-alist (cons
				       (cons (match-string 1)
					     (mew-time-rfc-to-sortkey (match-string 2)))
				       msg-alist)))
		    (forward-line 1))))
	       (setq t1 (decode-time (current-time)))
	       (setq t1 (append (list (nth 0 t1) (nth 1 t1) (nth 2 t1)
				      (- (nth 3 t1) days))
				(nthcdr 4 t1)))
	       (setq days (format-time-string "%Y%m%d%H%M%S"
					      (apply 'encode-time t1)))
	       (dolist (x msg-alist)
		 (when (string< (cdr x) days)
		   (setq msgs (cons (car x) msgs))))
	       (setq msgs (sort msgs
				(lambda (x y)
				  (< (string-to-number x) (string-to-number y)))))
	       (setq t1 (length msgs))
	       (if (zerop t1)
		   (message "No expire (%s)" fld)
		 (message "Expire (%s) 1/%d..." fld t1)
		 (goto-char (point-min))
		 (dolist (msg msgs)
		   (setq i (1+ i))
		   (when (zerop (% i 10))
		     (message "Expire (%s) %d/%d..." fld i t1))
		   (when (mew-shimbun-jump-msg msg)
		     (beginning-of-line)
		     (mew-elet
		      (delete-region (point)
				     (progn (forward-line) (point)))))
		   (setq file (mew-shimbun-expand-msg fld msg))
		   (when (and (file-exists-p file)
			      (file-readable-p file)
			      (file-writable-p file))
		     (delete-file file)))
		 (mew-elet
		  (mew-summary-folder-cache-save)
		  (set-buffer-modified-p nil))
		 (when (and mew-shimbun-use-expire-pack
			    (> t1 0))
		   (if (eq 1 (function-max-args 'mew-summary-pack-body))
		       (dont-compile
			 (mew-summary-pack-body fld))
		     (dont-compile
		       (mew-summary-pack-body))))
		 (message "Expire (%s) %d/%d...done" fld t1 t1))))))))))

(defun mew-shimbun-expire-day (fld)
  (catch 'det
    (dolist (x mew-shimbun-expires)
      (when (string-match (concat "^" (regexp-quote
				       (concat
					(file-name-as-directory mew-shimbun-folder)
					(car x))))
			  fld)
	(throw 'det (cdr x))))))

(defun mew-shimbun-get-id-msgs (type &rest args)
  (let (id-msgs)
    (cond
     ((eq type 'list)
      ;; folder msgs
      (with-temp-buffer
	(dolist (msg (car (cdr args)))
	  (erase-buffer)
	  (mew-insert-message (car args) msg mew-cs-text-for-read 512)
	  (goto-char (point-min))
	  (when (re-search-forward "^X-Shimbun-Id: \\(.+\\)\n" nil t)
	    (setq id-msgs (cons (cons (match-string 1) msg) id-msgs)))))
      (nreverse id-msgs))
     ((eq type 'range)
      ;; folder begin-message end-message
      (with-temp-buffer
	(mew-piolet
	 mew-cs-text-for-read mew-cs-text-for-write
	 (mew-shimbun-pick "-b" mew-mail-path
			   "-d" "X-Shimbun-Id:"
			   "-s" (format "%s %s-%s" (nth 0 args) (nth 1 args) (nth 2 args))))
	(goto-char (point-min))
	(while (re-search-forward "^\\([1-9][0-9]*\\): \\([^\n]+\\)" nil t)
	  (setq id-msgs (cons (cons (match-string 2) (match-string 1)) id-msgs))))
      (nreverse id-msgs))
     ;; something error
     (t nil))))

;;; Mew interface funcitions:
(defun mew-shimbun-scan-message (fld msg)
  (set-buffer-multibyte t)
  (let ((width (1- (mew-scan-width)))
	(vec (static-if (fboundp 'mew-pop-scan-header)
		 (mew-pop-scan-header)
	       (mew-scan-header))))
    (mew-scan-set-folder vec fld)
    (mew-scan-set-message vec msg)
    (set-buffer-multibyte nil)
    (mew-scan-insert-line fld vec width msg nil)
    (when mew-shimbun-use-unseen
      ;; xxxxx more fast
      (with-current-buffer fld
	(goto-char (point-min))
	(when (mew-shimbun-jump-msg msg)
	  (mew-mark-put-mark mew-shimbun-mark-unseen 'nomsg))
	(forward-line)))
    ;; for summary redraw
    (sit-for 0.01)))

(defun mew-shimbun-sanity-convert ()
  (if (re-search-forward mew-eoh nil t)
      (beginning-of-line)
    (goto-char (point-max))
    (insert "\n"))
  (save-restriction
    (let ((case-fold-search t)
	  (unknown-from mew-shimbun-unknown-from)
	  beg end from from13)
      (narrow-to-region (point-min) (point))
      (goto-char (point-min))
      (if (not (re-search-forward mew-from: nil t))
	  ;; No From:
	  (progn
	    (goto-char (point-max))
	    (insert (concat mew-from: " " unknown-from "\n")))
	(setq beg (match-end 0))
	(forward-line)
	(mew-header-goto-next)
	(setq end (1- (point)))
	(setq from (or (buffer-substring beg end) ""))
	(setq from (or (mew-addrstr-parse-address from) ""))
	(unless (string-match
		 "^[-A-Za-z0-9._!%]+@[A-Za-z0-9][-A-Za-z0-9._!]+[A-Za-z0-9]$"
		 from)
	  ;; strange From:
	  (goto-char (point-min))
	  (when (re-search-forward "^From-R13:" nil t)
	    ;; From-R13:
	    (setq beg (match-end 0))
	    (forward-line)
	    (mew-header-goto-next)
	    (setq from13 (buffer-substring beg (1- (point))))
	    (when (setq from13 (mew-shimbun-sanity-convert-rot13 from13))
	      (setq unknown-from from13)))
	  (goto-char end)
	  (insert " <" unknown-from ">"))))))

(defun mew-shimbun-sanity-convert-rot13 (from13)
  (with-temp-buffer
    (insert from13)
    ;; from13 is binary
    (mew-cs-decode-region (point-min) (point-max) mew-cs-autoconv)
    (goto-char (point-min))
    ;; Extent rot14(@,A-Z,[) + rot13(a-z)
    (while (< (point) (point-max))
      (let* ((chr (char-after (point))))
	(cond
	 ((and (<= ?@ chr) (<= chr ?\[))
	  (setq chr (+ chr 14))
	  (when (> chr ?\[) (setq chr (- chr 28)))
	  (delete-char 1)
	  (insert chr))
	 ((and (<= ?a chr) (<= chr ?z))
	  (setq chr (+ chr 13))
	  (when (> chr ?z) (setq chr (- chr 26)))
	  (delete-char 1)
	  (insert chr))
	 (t (forward-char)))))
    (setq from13 (buffer-substring (point-min) (point-max)))
    (mew-addrstr-parse-address from13)))

;;; Message-ID database:
(defun mew-shimbun-db-setup (fld)
  (setq mew-shimbun-db
	(mew-lisp-load
	 (expand-file-name mew-shimbun-db-file
			   (mew-expand-folder fld)))))

(defun mew-shimbun-db-setup2 (fld id-msgs)
  (mew-shimbun-db-setup fld)
  (setq mew-shimbun-db2 (copy-sequence mew-shimbun-db))
  (dolist (x id-msgs)
    (setq mew-shimbun-db2
	  (delq (assoc (car x) mew-shimbun-db2)
		mew-shimbun-db2))))

(defun mew-shimbun-db-shutdown (fld count)
  (when (> count 0)
    (let ((mew-lisp-max-length (mew-shimbun-db-length fld)))
      (mew-lisp-save
       (expand-file-name mew-shimbun-db-file (mew-expand-folder fld))
       mew-shimbun-db)
      (mew-touch-folder fld)))
  (setq mew-shimbun-db nil))

(defun mew-shimbun-db-shutdown2 (fld count)
  (mew-shimbun-db-shutdown fld count)
  (setq mew-shimbun-db2 nil))

(defun mew-shimbun-db-add-id (id md5 &optional replace)
  (let ((alist (mew-shimbun-db-search-id id)))
    (if (null alist)
	;; new
	(setq mew-shimbun-db (cons (cons id md5) mew-shimbun-db))
      (when replace
	;; replace
	(setq mew-shimbun-db
	      (cons (cons id md5) (delq alist mew-shimbun-db)))))))

(defun mew-shimbun-db-length (fld)
  (cond
   ((null mew-shimbun-db-length)
    mew-lisp-max-length)
   ((numberp mew-shimbun-db-length)
    mew-shimbun-db-length)
   (t
    (catch 'det
      (dolist (x mew-shimbun-db-length)
	(when (and (stringp (car x))
		   (string-match
		    (concat "^" (regexp-quote
				 (concat
				  (file-name-as-directory mew-shimbun-folder)
				  (car x))))
		    fld))
	  (throw 'det (cdr x))))
      (or (cdr (assq t mew-shimbun-db-length))
	  mew-lisp-max-length)))))

(luna-define-class shimbun-mew-mua (shimbun-mua) ())

;;; Misc
(defun mew-shimbun-md5 ()
  "Calculate MD5 with boundary remove."
  (let ((str (mew-buffer-substring
	      (point-min)
	      (min (point-max) (+ (point-min) 6144)))) ;; (* 4096 1.5)
	(case-fold-search nil)
	beg)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      ;; boundary include current-time()
      (while (re-search-forward "===shimbun_[0-9]+_[0-9]+_[0-9]+===" nil t)
	(replace-match ""))
      (goto-char (point-min))
      ;; delete X-Face:
      (when (re-search-forward "^X-Face:" nil t)
	(beginning-of-line)
	(setq beg (point))
	(forward-line)
	(mew-header-goto-next)
	(delete-region beg (point)))
      (md5 (string-as-unibyte
	    (mew-buffer-substring (point-min)
				  (min (point-max) (+ (point-min) 4096))))
	   nil nil 'binary))))

(defvar mew-shimbun-touch-folder-p
  (static-if (boundp 'mew-touch-folder-p)
      mew-touch-folder-p
    t)) ;; Mew 4

(defun mew-shimbun-folder-new-p (fld)
  (let* ((dir (file-chase-links (mew-expand-folder fld)))
	 (tdir (if mew-shimbun-touch-folder-p
		   (mew-file-get-time
		    (expand-file-name mew-summary-touch-file
				      (mew-expand-folder dir)))
		 (mew-file-get-time dir)))
	 (cache (expand-file-name mew-summary-cache-file dir))
	 (tcache (mew-file-get-time cache)))
    (cond
     ((null tdir) nil)
     ((null tcache) t) ;; do update
     ((> (nth 0 tdir) (nth 0 tcache)) t)
     ((= (nth 0 tdir) (nth 0 tcache))
      (if (> (nth 1 tdir) (nth 1 tcache)) t nil))
     (t nil))))

;;; Unseen
(defun mew-shimbun-unseen-remove-advice ()
  "Remove 'unseen' mark."
  (let ((fld (mew-summary-folder-name)))
    (when (mew-shimbun-folder-p fld)
      (let* ((vfld (mew-summary-folder-name 'ext))
	     (msg (mew-summary-message-number))
	     (part (mew-syntax-nums)))
	(when (and fld msg (null part))
	  (save-excursion
	    (beginning-of-line)
	    (when (looking-at (or mew-shimbun-unseen-regex
				  (mew-shimbun-unseen-regex)))
	      ;; in normal or thread folder
	      (mew-mark-unmark)
	      (set-buffer-modified-p nil)
	      (when (and (not (string= fld vfld)) (get-buffer fld))
		;; thread => normal shimbun folder
		(mew-summary-unmark-in-physical fld msg)))))))))

(defun mew-shimbun-unseen-setup ()
  "`Shimbun unseen mark' support advices."
  (interactive)
  (when mew-shimbun-use-unseen
    (unless (boundp 'mew-mark-unread)
      (defadvice mew-summary-cursor-postscript (before shimbun-unseen activate)
	(mew-shimbun-unseen-remove-advice)))

    (when mew-shimbun-use-unseen-cache-save
      ;; "C-cC-q"
      (defadvice mew-kill-buffer (before shimbun-cache-save activate)
	(let* ((buf (or buf (current-buffer)))
	       (fld (if (bufferp buf) (buffer-name buf) buf)))
	  (when (and (get-buffer buf) (mew-shimbun-folder-p fld))
	    (with-current-buffer buf
	      (unless (mew-summary-folder-dir-newp)
		(mew-summary-folder-cache-save))))))

      ;; "Q" or exit Emacs
      (defadvice mew-mark-clean-up (before shimbun-cache-save activate)
	(save-current-buffer
	  (dolist (fld mew-buffers)
	    (when (and (get-buffer fld) (mew-shimbun-folder-p fld))
	      (set-buffer fld)
	      (unless (mew-summary-folder-dir-newp)
		(mew-summary-folder-cache-save))))))
      )))

;;; unseen setup
(when mew-shimbun-use-unseen
  (mew-shimbun-unseen-setup))

(provide 'mew-shimbun)
;;; mew-shimbun.el ends here.
