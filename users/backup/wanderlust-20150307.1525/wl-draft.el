;;; wl-draft.el --- Message draft mode for Wanderlust.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 1998,1999,2000 Masahiro MURATA <muse@ba2.so-net.ne.jp>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Masahiro MURATA <muse@ba2.so-net.ne.jp>
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
(require 'elmo)
(require 'elmo-net)
(require 'sendmail)
(require 'wl-template)
(require 'emu)
(require 'timezone nil t)
(require 'std11)
(require 'eword-encode)
(require 'wl-util)
(require 'wl-vars)

(defvar x-face-add-x-face-version-header)
(defvar mail-reply-buffer)
(defvar mail-from-style)

(eval-when-compile
  (require 'cl)
  (require 'static)
  (require 'elmo-pop3)
  (defalias-maybe 'x-face-insert 'ignore)
  (defalias-maybe 'x-face-insert-version-header 'ignore)
  (defalias-maybe 'wl-init 'ignore)
  (defalias-maybe 'wl-draft-mode 'ignore))

(eval-and-compile
  (autoload 'wl-addrmgr "wl-addrmgr"))

(defvar wl-draft-buffer-message-number nil)
(defvar wl-draft-field-completion-list nil)
(defvar wl-draft-verbose-send t)
(defvar wl-draft-verbose-msg nil)
(defvar wl-draft-queue-flushing nil)
(defvar wl-draft-config-variables nil)
(defvar wl-draft-config-exec-flag t)
(defvar wl-draft-buffer-cur-summary-buffer nil)
(defvar wl-draft-clone-local-variable-regexp "^\\(wl\\|mime\\)")
(defvar wl-draft-sendlog-filename "sendlog")
(defvar wl-draft-queue-save-filename "qinfo")
(defvar wl-draft-config-save-filename "config")
(defvar wl-draft-queue-flush-send-function 'wl-draft-dispatch-message)
(defvar wl-sent-message-via nil)
(defvar wl-sent-message-modified nil)
(defvar wl-sent-message-queued nil)
(defvar wl-draft-fcc-list nil)
(defvar wl-draft-reedit nil)
(defvar wl-draft-reply-buffer nil)
(defvar wl-draft-forward nil)
(defvar wl-draft-doing-mime-bcc nil)

(defvar wl-draft-parent-folder nil
  "Folder name of the summary in which current draft is invoked.
This variable is local in each draft buffer.
You can refer its value in `wl-draft-config-alist'.

e.g.
\(setq wl-draft-config-alist
      '(((string-match \".*@domain1$\" wl-draft-parent-folder)
         (\"From\" . \"user@domain1\"))
        ((string-match \".*@domain2$\" wl-draft-parent-folder)
         (\"From\" . \"user@domain2\"))))")

(defvar wl-draft-parent-number nil)
(defvar wl-draft-parent-flag nil)

(defconst wl-draft-parent-variables
  '(wl-draft-parent-folder
    wl-draft-parent-number
    wl-draft-parent-flag))

(defvar wl-draft-config-sub-func-alist
  '((body		. wl-draft-config-sub-body)
    (top		. wl-draft-config-sub-top)
    (bottom		. wl-draft-config-sub-bottom)
    (header		. wl-draft-config-sub-header)
    (header-top		. wl-draft-config-sub-header-top)
    (header-bottom	. wl-draft-config-sub-header)
    (part-top		. wl-draft-config-sub-part-top)
    (part-bottom	. wl-draft-config-sub-part-bottom)
    (body-file		. wl-draft-config-sub-body-file)
    (top-file		. wl-draft-config-sub-top-file)
    (bottom-file	. wl-draft-config-sub-bottom-file)
    (header-file	. wl-draft-config-sub-header-file)
    (template		. wl-draft-config-sub-template)
    (x-face		. wl-draft-config-sub-x-face)))

(make-variable-buffer-local 'wl-draft-buffer-message-number)
(make-variable-buffer-local 'wl-draft-buffer-cur-summary-buffer)
(make-variable-buffer-local 'wl-draft-config-variables)
(make-variable-buffer-local 'wl-draft-config-exec-flag)
(make-variable-buffer-local 'wl-sent-message-via)
(make-variable-buffer-local 'wl-sent-message-queued)
(make-variable-buffer-local 'wl-draft-fcc-list)
(make-variable-buffer-local 'wl-draft-reply-buffer)
(make-variable-buffer-local 'wl-draft-parent-folder)
(make-variable-buffer-local 'wl-draft-parent-number)
(make-variable-buffer-local 'wl-draft-parent-flag)

(defvar wl-draft-folder-internal nil
  "Internal variable for caching `opened' draft folder.")

(defsubst wl-smtp-password-key (user mechanism server)
  (format "SMTP:%s/%s@%s"
	  user mechanism server))

(defmacro wl-smtp-extension-bind (&rest body)
  `(let ((smtp-sasl-mechanisms
	  (if wl-smtp-authenticate-type
	      (mapcar 'upcase
		      (if (listp wl-smtp-authenticate-type)
			  wl-smtp-authenticate-type
			(list wl-smtp-authenticate-type)))))
	 (smtp-use-sasl wl-smtp-authenticate-type)
	 (smtp-use-starttls (eq wl-smtp-connection-type 'starttls))
	 (smtp-open-connection-function
	  (if (eq wl-smtp-connection-type 'ssl)
	      (let ((stream-type (elmo-get-network-stream-type 'ssl)))
		(require (elmo-network-stream-type-feature stream-type))
		(elmo-network-stream-type-function stream-type))
	    smtp-open-connection-function))
	 (smtp-sasl-user-name wl-smtp-posting-user)
	 (smtp-sasl-properties (when wl-smtp-authenticate-realm
				 (list 'realm wl-smtp-authenticate-realm)))
	 sasl-read-passphrase)
     (setq sasl-read-passphrase
	   (function
	    (lambda (prompt)
	      (elmo-get-passwd
	       (wl-smtp-password-key
		smtp-sasl-user-name
		(car smtp-sasl-mechanisms)
		smtp-server)))))
     ,@body))

(def-edebug-spec wl-smtp-extension-bind (body))

(defun wl-draft-insert-date-field ()
  "Insert Date field."
  (insert "Date: " (wl-make-date-string) "\n"))

(defun wl-draft-insert-from-field ()
  "Insert From field."
  ;; Put the "From:" field in unless for some odd reason
  ;; they put one in themselves.
  (let (from)
    (condition-case err
	(setq from (wl-draft-eword-encode-address-list wl-from))
      (error (error "Please look at `wl-from' again")))
    (insert "From: " from "\n")))

(defun wl-draft-insert-x-face-field ()
  "Insert X-Face header."
  (interactive)
  (if (not (file-exists-p wl-x-face-file))
      (error "File %s does not exist" wl-x-face-file)
    (goto-char (point-min))
    (search-forward mail-header-separator nil t)
    (beginning-of-line)
    (wl-draft-insert-x-face-field-here)
    (run-hooks 'wl-draft-insert-x-face-field-hook))) ; highlight it if you want.

(defun wl-draft-insert-x-face-field-here ()
  "Insert X-Face field at point."
  (let ((x-face-string (elmo-get-file-string wl-x-face-file)))
    (when (string-match "^\\(X-Face:\\)?[ \t\n]*" x-face-string)
      (setq x-face-string (substring x-face-string (match-end 0))))
    (insert "X-Face: " x-face-string))
  (when (not (= (preceding-char) ?\n))	; for chomped (choped) x-face-string
    (insert ?\n))
  ;; Insert X-Face-Version: field
  (when (and (fboundp 'x-face-insert-version-header)
	     (boundp 'x-face-add-x-face-version-header)
	     x-face-add-x-face-version-header)
    (x-face-insert-version-header)))

(defun wl-draft-setup ()
  (let ((field wl-draft-fields)
	cl)
    (while field
      (setq cl (append cl
		       (list (cons (concat (car field) " ")
				   (concat (car field) " ")))))
      (setq field (cdr field)))
    (setq cl
	  (cons (cons (concat wl-draft-mime-bcc-field-name  ": ")
		      (concat wl-draft-mime-bcc-field-name  ": "))
		cl))
    (setq wl-draft-field-completion-list cl)
    (setq wl-address-complete-header-regexp
	  (wl-regexp-opt
	   (append wl-address-complete-header-list
		   (list (concat wl-draft-mime-bcc-field-name  ":")))))))

(defun wl-draft-make-mail-followup-to (recipients)
  (let ((rlist (wl-address-delete-user-mail-addresses recipients)))
    (if (elmo-list-member rlist (mapcar 'downcase
					wl-subscribed-mailing-list))
	rlist
      (append rlist (list (wl-address-header-extract-address
			   wl-from))))))

(defun wl-draft-delete-myself-from-cc (to cc)
  (cond (wl-draft-always-delete-myself ; always-delete option
	 (wl-address-delete-user-mail-addresses cc))
	((elmo-list-member (append to cc) ; subscribed mailing-list
			   (mapcar 'downcase wl-subscribed-mailing-list))
	 (wl-address-delete-user-mail-addresses cc))
	(t cc)))

(defsubst wl-draft-strip-subject-regexp (subject regexp)
  "Remove REGEXP from SUBJECT string."
  (if (string-match regexp subject)
      (substring subject (match-end 0))
    subject))

(defun wl-draft-forward-make-subject (original-subject)
  "Generate subject string for forwarding."
  (cond ((functionp wl-forward-subject-prefix)
	 (concat (funcall wl-forward-subject-prefix)
		 original-subject))
	((stringp wl-forward-subject-prefix)
	 (concat wl-forward-subject-prefix
		 (wl-draft-strip-subject-regexp
		  (or original-subject "")
		  wl-subject-forward-prefix-regexp)))
	(t original-subject)))

(defun wl-draft-reply-make-subject (original-subject)
  "Generate subject string for replying."
  (cond ((functionp wl-reply-subject-prefix)
	 (concat (funcall wl-reply-subject-prefix)
		 original-subject))
	((stringp wl-reply-subject-prefix)
	 (concat wl-reply-subject-prefix
		 (wl-draft-strip-subject-regexp
		  (or original-subject "")
		  wl-subject-re-prefix-regexp)))
	(t original-subject)))

(defun wl-draft-forward (original-subject summary-buf &optional number)
  (let (references parent-folder subject)
    (with-current-buffer summary-buf
      (setq parent-folder (wl-summary-buffer-folder-name)))
    (let ((decoder (mime-find-field-decoder 'Subject 'plain)))
      (setq subject (if (and original-subject decoder)
			(funcall decoder original-subject) original-subject)))
    (with-current-buffer (wl-message-get-original-buffer)
      (setq subject (wl-draft-forward-make-subject subject))
      (setq references (nconc
			(std11-field-bodies '("References" "In-Reply-To"))
			(list (elmo-get-message-id-from-buffer))))
      (setq references (delq nil references)
	    references (mapconcat 'identity references " ")
	    references (wl-draft-parse-msg-id-list-string references)
	    references (wl-delete-duplicates references)
	    references (when references
			 (mapconcat 'identity references "\n\t"))))
    (and wl-draft-use-frame
	 (get-buffer-window summary-buf)
	 (select-window (get-buffer-window summary-buf)))
    (wl-draft (list (cons 'To "")
		    (cons 'Subject subject)
		    (cons 'References references))
	      nil nil nil nil parent-folder number))
  (goto-char (point-max))
  (wl-draft-insert-message)
  (mail-position-on-field "To")
  (setq wl-draft-config-variables
	(append wl-draft-parent-variables
		wl-draft-config-variables))
  (wl-draft-config-info-operation wl-draft-buffer-message-number 'save)
  (run-hooks 'wl-draft-forward-hook))

(defun wl-draft-self-reply-p ()
  "Return t when From address in the current message is user's self one or not."
  (wl-address-user-mail-address-p (or (std11-field-body "From") "")))

(defun wl-draft-find-reply-headers (rule-symbol)
  (let ((rule-list (symbol-value rule-symbol))
	condition-match-p result)
    (setq condition-match-p
	  (lambda (condition)
	    (cond ((stringp condition)
		   (std11-field-body condition))
		  ((functionp condition)
		   (funcall condition))
		  ((consp condition)
		   (and (funcall condition-match-p (car condition))
			(funcall condition-match-p (cdr condition))))
		  ((null condition))
		  (t
		   (error "Unkown condition in `%s'" rule-symbol)))))
    (while (and (null result) rule-list)
      (let ((rule (car rule-list)))
	(when (funcall condition-match-p (car rule))
	  (setq result (cdr rule)))
	(setq rule-list (cdr rule-list))))
    result))

(defun wl-draft-reply (buf with-arg summary-buf &optional number)
  "Create draft for replying to the message in buffer BUF.
Recipients are prepared along `wl-draft-reply-without-argument-list',
or `wl-draft-reply-with-argument-list' if WITH-ARG argument is non-nil."
;;;(save-excursion
  (let ((rule-list (if with-arg
		       'wl-draft-reply-with-argument-list
		     'wl-draft-reply-without-argument-list))
	reply-headers
	to mail-followup-to cc subject in-reply-to references newsgroups
	to-alist cc-alist decoder parent-folder)
    (when (buffer-live-p summary-buf)
      (with-current-buffer summary-buf
	(setq parent-folder (wl-summary-buffer-folder-name))))
    (set-buffer (or buf mime-mother-buffer))
    (setq reply-headers
	  (or (wl-draft-find-reply-headers rule-list)
	      (error "No match field: check your `%s'" rule-list)))
    (let ((r-to-list (nth 0 reply-headers))
	  (r-cc-list (nth 1 reply-headers))
	  (r-ng-list (nth 2 reply-headers)))
      (setq to (wl-concat-list
		(nconc
		 (if (functionp r-to-list)
		     (funcall r-to-list)
		   (elmo-multiple-fields-body-list r-to-list))
		 (and (member "Followup-To" r-ng-list)
		      (string= (std11-field-body "Followup-To") "poster")
		      (progn
			(setq r-ng-list (delete "Followup-To"
						(copy-sequence r-ng-list)))
			(elmo-multiple-fields-body-list '("From")))))
		","))
      (setq cc (wl-concat-list
		(if (functionp r-cc-list)
		    (funcall r-cc-list)
		  (elmo-multiple-fields-body-list r-cc-list))
		","))
      (setq newsgroups (wl-concat-list
			(if (functionp r-ng-list)
			    (funcall r-ng-list)
			  (std11-field-bodies r-ng-list))
			",")))
    (setq subject (std11-field-body "Subject"))
    (setq to (wl-parse-addresses to)
	  cc (wl-parse-addresses cc))
    (with-temp-buffer			; to keep raw buffer unibyte.
      (set-buffer-multibyte default-enable-multibyte-characters)
      (setq decoder (mime-find-field-decoder 'Subject 'plain))
      (setq subject (if (and subject decoder)
			(funcall decoder subject) subject))
      (setq to-alist
	    (mapcar
	     (lambda (addr)
	       (setq decoder (mime-find-field-decoder 'To 'plain))
	       (cons (nth 1 (std11-extract-address-components addr))
		     (if decoder (funcall decoder addr) addr)))
	     to))
      (setq cc-alist
	    (mapcar
	     (lambda (addr)
	       (setq decoder (mime-find-field-decoder 'Cc 'plain))
	       (cons (nth 1 (std11-extract-address-components addr))
		     (if decoder (funcall decoder addr) addr)))
	     cc)))
    (setq subject (wl-draft-reply-make-subject subject))
    (setq in-reply-to (elmo-get-message-id-from-buffer))
    (setq references (nconc
		      (std11-field-bodies '("References" "In-Reply-To"))
		      (list in-reply-to)))
    (setq to (delq nil (mapcar 'car to-alist)))
    (setq cc (delq nil (mapcar 'car cc-alist)))
    ;; if subscribed mailing list is contained in cc or to
    ;; and myself is contained in cc,
    ;; delete myself from cc.
    (setq cc (wl-draft-delete-myself-from-cc to cc))
    (when wl-insert-mail-followup-to
      (setq mail-followup-to
	    (wl-draft-make-mail-followup-to (append to cc)))
      (setq mail-followup-to (wl-delete-duplicates mail-followup-to nil t)))
    (with-temp-buffer			; to keep raw buffer unibyte.
      (set-buffer-multibyte default-enable-multibyte-characters)
      (setq newsgroups (elmo-parse newsgroups
				 "[ \t\f\r\n,]*\\([^ \t\f\r\n,]+\\)")
	    newsgroups (wl-delete-duplicates newsgroups)
	    newsgroups
	    (if newsgroups
		(mapconcat
		 (lambda (grp)
		   (setq decoder (mime-find-field-decoder 'Newsgroups 'plain))
		   (if decoder (funcall decoder grp) grp))
		 newsgroups ","))))
    (setq to (wl-delete-duplicates to nil t))
    (setq cc (wl-delete-duplicates
	      (append (wl-delete-duplicates cc nil t)
		      to (copy-sequence to))
	      t t))
    (and to (setq to (mapconcat
		      (lambda (addr)
			(if wl-draft-reply-use-address-with-full-name
			    (or (cdr (assoc addr to-alist)) addr)
			  addr))
		      to ",\n\t")))
    (and cc (setq cc (mapconcat
		      (lambda (addr)
			(if wl-draft-reply-use-address-with-full-name
			    (or (cdr (assoc addr cc-alist)) addr)
			  addr))
		      cc ",\n\t")))
    (and mail-followup-to
	 (setq mail-followup-to
	       (mapconcat
		(lambda (addr)
		  (if wl-draft-reply-use-address-with-full-name
		      (or (cdr (assoc addr (append to-alist cc-alist))) addr)
		    addr))
		mail-followup-to ",\n\t")))
    (and (null to) (setq to cc cc nil))
    (setq references (delq nil references)
	  references (mapconcat 'identity references " ")
	  references (wl-draft-parse-msg-id-list-string references)
	  references (wl-delete-duplicates references)
	  references (if references
			 (mapconcat 'identity references "\n\t")))
    (and wl-draft-use-frame
	 (get-buffer-window summary-buf)
	 (select-window (get-buffer-window summary-buf)))
    (wl-draft (list (cons 'To to)
		    (cons 'Cc cc)
		    (cons 'Newsgroups newsgroups)
		    (cons 'Subject subject)
		    (cons 'In-Reply-To in-reply-to)
		    (cons 'References references)
		    (cons 'Mail-Followup-To mail-followup-to))
	      nil nil nil nil parent-folder number)
    (setq wl-draft-reply-buffer buf)
    (setq wl-draft-config-variables
	  (append wl-draft-parent-variables
		  wl-draft-config-variables))
    (wl-draft-config-info-operation wl-draft-buffer-message-number 'save))
  (run-hooks 'wl-draft-reply-hook))

(defun wl-draft-reply-position (position)
  (cond ((eq position 'body)
	 (wl-draft-body-goto-top))
	((eq position 'bottom)
	 (wl-draft-body-goto-bottom))
	((eq position 'top)
	 (goto-char (point-min)))
	((and (stringp position)
	      (std11-field-body position))
	 (progn (mail-position-on-field position)
		(wl-draft-beginning-of-line)))
	((listp position)
	 (while (car position)
	   (wl-draft-reply-position (car position))
	   (setq position (cdr position))))))

(defun wl-draft-add-references ()
  (wl-draft-add-in-reply-to "References"))

(defun wl-draft-add-in-reply-to (&optional alt-field)
  (let* ((mes-id (with-current-buffer mail-reply-buffer
		   (elmo-get-message-id-from-buffer)))
	 (field (or alt-field "In-Reply-To"))
	 (ref (std11-field-body field))
	 (ref-list nil) (st nil))
    (when (and mes-id ref)
      (while (string-match "<[^>]+>" ref st)
	(setq ref-list
	      (cons (substring ref (match-beginning 0) (setq st (match-end 0)))
		    ref-list)))
      (when (and ref-list
		 (member mes-id ref-list))
	(setq mes-id nil)))
    (when mes-id
      (save-excursion
	(when (mail-position-on-field field)
	  (forward-line)
	  (while (looking-at "^[ \t]")
	    (forward-line))
	  (setq mes-id (concat "\t" mes-id "\n")))
	(insert mes-id))
      t)))

(defun wl-draft-yank-from-mail-reply-buffer (decode-it
					     &optional ignored-fields)
  (interactive)
  (save-restriction
    (narrow-to-region (point)(point))
    (insert
     (string-as-multibyte
      (with-current-buffer mail-reply-buffer
	(when decode-it
	  (decode-mime-charset-region (point-min) (point-max)
				      wl-mime-charset))
	(buffer-substring-no-properties
	 (point-min) (point-max)))))
    (when ignored-fields
      (goto-char (point-min))
      (wl-draft-delete-fields ignored-fields))
    (goto-char (point-max))
    (push-mark (point) nil t)
    (goto-char (point-min)))
  (let ((beg (point)))
    (cond (mail-citation-hook (run-hooks 'mail-citation-hook))
	  (mail-yank-hooks (run-hooks 'mail-yank-hooks))
	  (wl-draft-cite-function (funcall wl-draft-cite-function))) ; default cite
    (run-hooks 'wl-draft-cited-hook)
    (when (if wl-draft-add-references
	      (wl-draft-add-references)
	    (if wl-draft-add-in-reply-to
		(wl-draft-add-in-reply-to)))
      (unless wl-draft-jit-highlight
        (wl-highlight-headers 'for-draft))) ; highlight when added References:
    (when (and wl-highlight-body-too (not wl-draft-jit-highlight))
      (wl-highlight-body-region beg (point-max)))))

(defun wl-message-news-p ()
  "If exist valid Newsgroups field, return non-nil."
  (std11-field-body "Newsgroups"))

(defun wl-message-field-exists-p (field)
  "If FIELD exist and FIELD value is not empty, return non-nil."
  (let ((value (std11-field-body field)))
    (and value
	 (not (string= value "")))))

(defun wl-message-mail-p ()
  "If exist To, Cc or Bcc field, return non-nil."
  (or (wl-message-field-exists-p "To")
      (wl-message-field-exists-p "Resent-to")
      (wl-message-field-exists-p "Cc")
      (wl-message-field-exists-p "Bcc")
      (wl-message-field-exists-p wl-draft-mime-bcc-field-name)
;;; This may be needed..
;;;   (wl-message-field-exists-p "Fcc")
      ))

(defun wl-draft-edit-string (string)
  (let ((cur-buf (current-buffer))
	(tmp-buf (get-buffer-create " *wl-draft-edit-string*"))
	to subject in-reply-to cc references newsgroups mail-followup-to
	content-type content-transfer-encoding from
	body-beg)
    (set-buffer tmp-buf)
    (erase-buffer)
    (insert string)
    (setq to (std11-field-body "To"))
    (setq to (and to
		  (eword-decode-string
		   (decode-mime-charset-string
		    to
		    wl-mime-charset))))
    (setq subject (std11-field-body "Subject"))
    (setq subject (and subject
		       (eword-decode-string
			(decode-mime-charset-string
			 subject
			 wl-mime-charset))))
    (setq from (std11-field-body "From")
	  from (and from
		    (eword-decode-string
		     (decode-mime-charset-string
		      from
		      wl-mime-charset))))
    (setq in-reply-to (std11-field-body "In-Reply-To"))
    (setq cc (std11-field-body "Cc"))
    (setq cc (and cc
		  (eword-decode-string
		   (decode-mime-charset-string
		    cc
		    wl-mime-charset))))
    (setq references (std11-field-body "References"))
    (setq newsgroups (std11-field-body "Newsgroups"))
    (setq mail-followup-to (std11-field-body "Mail-Followup-To"))
    (setq content-type (std11-field-body "Content-Type"))
    (setq content-transfer-encoding (std11-field-body "Content-Transfer-Encoding"))
    (goto-char (point-min))
    (or (re-search-forward "\n\n" nil t)
	(search-forward (concat mail-header-separator "\n") nil t))
    (unwind-protect
	(set-buffer
	 (wl-draft (list
		    (cons 'From
			  (if (wl-address-user-mail-address-p from) from))
		    (cons 'To to)
		    (cons 'Cc cc)
		    (cons 'Subject subject)
		    (cons 'Newsgroups newsgroups)
		    (cons 'Mail-Followup-To mail-followup-to)
		    (cons 'In-Reply-To in-reply-to)
		    (cons 'References references))
		   content-type content-transfer-encoding
		   (buffer-substring (point) (point-max))
		   'edit-again))
      (kill-buffer tmp-buf))
    ;; Set cursor point to the top.
    (goto-char (point-min))
    (search-forward (concat mail-header-separator "\n") nil t)
    (run-hooks 'wl-draft-reedit-hook)
    (and to (mail-position-on-field "To"))))

(defun wl-draft-insert-current-message (dummy)
  (interactive)
  (let (original-buffer
	mail-reply-buffer
	mail-citation-hook mail-yank-hooks
	wl-draft-add-references wl-draft-add-in-reply-to
	wl-draft-cite-function)
    (if (and wl-draft-buffer-cur-summary-buffer
	     (with-current-buffer wl-draft-buffer-cur-summary-buffer
	       (and wl-message-buffer
		    (with-current-buffer wl-message-buffer
		      (setq original-buffer (wl-message-get-original-buffer))
		      (not (zerop (with-current-buffer original-buffer
				    (buffer-size))))))))
	(progn
	  (setq mail-reply-buffer original-buffer)
	  (wl-draft-yank-from-mail-reply-buffer
	   nil
	   wl-ignored-forwarded-headers))
      (when (string= (mime-make-tag "message" "rfc822")
		     (buffer-substring-no-properties (point-at-bol 0)(point-at-eol 0)))
	(delete-region (point-at-bol 0) (1+ (point-at-eol 0))))
      (error "No current message"))))

(defun wl-draft-insert-get-message (dummy)
  (let ((fld (completing-read
	      "Folder name: "
	      (if (memq 'read-folder wl-use-folder-petname)
		  (wl-folder-get-entity-with-petname)
		wl-folder-entity-hashtb)
	      nil nil wl-default-spec
	      'wl-read-folder-history))
	(number (call-interactively
		 (function (lambda (num)
			     (interactive "nNumber: ")
			     num))))
	(mail-reply-buffer (get-buffer-create "*wl-draft-insert-get-message*"))
	mail-citation-hook mail-yank-hooks
	wl-draft-cite-function)
    (unwind-protect
	(progn
	  (with-current-buffer mail-reply-buffer
	    (erase-buffer)
	    (elmo-message-fetch (wl-folder-get-elmo-folder fld)
				number
				;; No cache.
				(elmo-make-fetch-strategy 'entire)))
	  (wl-draft-yank-from-mail-reply-buffer nil))
      (kill-buffer mail-reply-buffer))))

;;
;; default body citation func
;;
(defun wl-default-draft-cite ()
  (let ((mail-yank-ignored-headers "[^:]+:")
	(mail-yank-prefix "> ")
	date from cite-title)
    (save-restriction
      (if (< (mark t) (point))
	  (exchange-point-and-mark))
      (narrow-to-region (point)(point-max))
      (setq date (std11-field-body "date")
	    from (std11-field-body "from")))
    (when (or date from)
      (insert
       (format wl-default-draft-cite-header-format-string
	       (if date
		   (if (stringp wl-default-draft-cite-date-format-string)
		       (let ((system-time-locale
			      (or wl-default-draft-cite-time-locale
				  system-time-locale)))
			 (format-time-string
			  wl-default-draft-cite-date-format-string
			  (elmo-time-parse-date-string date)))
		     (concat "On " (if wl-default-draft-cite-date-format-string
				       (wl-make-date-string
					(elmo-time-parse-date-string date))
				     date)))
		 wl-default-draft-cite-no-date-string)
	       (funcall (if wl-default-draft-cite-decorate-author
			    wl-summary-from-function
			  #'identity)
			(or from wl-default-draft-cite-no-author-string)))))
    (mail-indent-citation)))

(defvar wl-draft-buffer nil "Draft buffer to yank content.")
(defun wl-draft-yank-to-draft-buffer (buffer)
  "Yank BUFFER content to `wl-draft-buffer'."
  (set-buffer wl-draft-buffer)
  (let ((mail-reply-buffer buffer))
    (wl-draft-yank-from-mail-reply-buffer nil)
    (kill-buffer buffer)))

(defun wl-draft-yank-original (&optional arg)
  "Yank original message."
  (interactive "P")
  (if arg
      (let ((draft-buffer (current-buffer))
	    mail-reply-buffer)
	(with-temp-buffer
	  (insert "\n")
	  (yank)
	  (setq mail-reply-buffer (current-buffer))
	  (with-current-buffer draft-buffer
	    (wl-draft-yank-from-mail-reply-buffer nil))))
    (wl-draft-yank-current-message-entity)))

(defun wl-draft-hide (editing-buffer)
  "Hide the editing draft buffer if possible."
  (when (and editing-buffer
	     (buffer-live-p editing-buffer)
	     (get-buffer-window editing-buffer))
    (select-window (get-buffer-window editing-buffer))
    (let ((sum-buf wl-draft-buffer-cur-summary-buffer)
	  fld-buf sum-win fld-win)
      (if (and wl-draft-use-frame
	       (> (length (visible-frame-list)) 1))
	  ;; hide draft frame
	  (delete-frame)
	;; hide draft window
	(or (one-window-p)
	    (delete-window))
	;; stay folder window if required
	(when wl-stay-folder-window
	  (if (setq fld-buf (get-buffer wl-folder-buffer-name))
	      (if (setq fld-win (get-buffer-window fld-buf))
		  (select-window fld-win)
		(if wl-draft-resume-folder-window ;; resume folder window
		    (switch-to-buffer fld-buf)))))
	(if (buffer-live-p sum-buf)
	    (if (setq sum-win (get-buffer-window sum-buf t))
		;; if Summary is on the frame, select it.
		(select-window sum-win)
	      ;; if summary is not on the frame, switch to it.
	      (if (and wl-stay-folder-window
		       (or wl-draft-resume-folder-window fld-win))
		  (wl-folder-select-buffer sum-buf)
		(switch-to-buffer sum-buf))))))))

(defun wl-draft-delete (editing-buffer)
  "Kill the editing draft buffer and delete the file corresponds to it."
  (when editing-buffer
    (with-current-buffer editing-buffer
      (when wl-draft-buffer-message-number
	(elmo-folder-delete-messages (wl-draft-get-folder)
				     (list
				      wl-draft-buffer-message-number))
	(wl-draft-config-info-operation wl-draft-buffer-message-number
					'delete))
      (set-buffer-modified-p nil)		; force kill
      (kill-buffer editing-buffer))))

(defun wl-draft-kill (&optional force-kill)
  "Kill current draft buffer and quit editing."
  (interactive "P")
  (save-excursion
    (when (and (or (eq major-mode 'wl-draft-mode)
		   (eq major-mode 'mail-mode))
	       (or force-kill
		   (yes-or-no-p "Kill Current Draft? ")))
      (let ((cur-buf (current-buffer)))
	(run-hooks 'wl-draft-kill-pre-hook)
	(wl-draft-hide cur-buf)
	(wl-draft-delete cur-buf)))
    (message "")))

(defun wl-draft-fcc ()
  "Add a new Fcc field, with file name completion."
  (interactive)
  (or (mail-position-on-field "fcc" t)  ;Put new field after exiting Fcc.
      (mail-position-on-field "to"))
  (insert "\nFcc: "))

;; Imported from message.el.
(defun wl-draft-elide-region (b e)
  "Elide the text in the region.
An ellipsis (from `wl-draft-elide-ellipsis') will be inserted where the
text was killed."
  (interactive "r")
  (kill-region b e)
  (insert wl-draft-elide-ellipsis))

;; Imported from message.el.
(defun wl-draft-beginning-of-line (&optional n)
  "Move point to beginning of header value or to beginning of line."
  (interactive "p")
  (let ((zrs 'zmacs-region-stays))
    (when (and (interactive-p) (boundp zrs))
      (set zrs t)))
  (if (wl-draft-point-in-header-p)
      (let* ((here (point))
	     (bol (progn (beginning-of-line n) (point)))
	     (eol (line-end-position))
	     (eoh (and (looking-at "[^ \t]")
		       (re-search-forward ": *" eol t))))
	(if (and eoh (or (> here eoh) (= here bol)))
	    (goto-char eoh)
	  (goto-char bol)))
    (beginning-of-line n)))

(defun wl-draft-point-in-header-p ()
  "Return t if point is in the header."
  (save-excursion
    (let ((p (point)))
      (goto-char (point-min))
      (not (re-search-forward
	    (concat "^" (regexp-quote mail-header-separator) "\n")
	    p t)))))

;; function for wl-sent-message-via

(defmacro wl-draft-sent-message-p (type)
  `(eq (nth 1 (assq ,type wl-sent-message-via)) 'sent))

(defmacro wl-draft-set-sent-message (type result &optional server-port)
  `(let ((element (assq ,type wl-sent-message-via)))
     (if element
	 (unless (eq (nth 1 element) ,result)
	   (setcdr element (list ,result ,server-port))
	   (setq wl-sent-message-modified t))
       (push (list ,type ,result ,server-port) wl-sent-message-via)
       (setq wl-sent-message-modified t))))

(defun wl-draft-sent-message-results ()
  (let ((results wl-sent-message-via)
	unplugged-via sent-via)
    (while results
      (cond ((eq (nth 1 (car results)) 'unplugged)
	     (push (caar results) unplugged-via))
	    ((eq (nth 1 (car results)) 'sent)
	     (push (caar results) sent-via)))
      (setq results (cdr results)))
    (list unplugged-via sent-via)))

(defun wl-draft-write-sendlog (status proto server to id)
  "Write send log file, if `wl-draft-sendlog' is non-nil."
  (when wl-draft-sendlog
    (with-temp-buffer
      (let* ((filename (expand-file-name wl-draft-sendlog-filename
					 elmo-msgdb-directory))
	     (filesize (nth 7 (file-attributes filename)))
	     (server (if server (concat " server=" server) ""))
	     (to (if to (cond
			 ((memq proto '(fcc queue))
			  (format " folder=\"%s\"" to))
			 ((eq proto 'nntp)
			  (format " ng=<%s>" to))
			 (t
			  (concat " to="
				  (mapconcat
				   'identity
				   (mapcar (lambda (x) (format "<%s>" x)) to)
				   ","))))
		   ""))
	     (id (if id (concat " id=" id) ""))
	     (time (format-time-string "%Y/%m/%d %T")))
	(insert (format "%s proto=%s stat=%s%s%s%s\n"
			time proto status server to id))
	(if (and wl-draft-sendlog-max-size filesize
		 (> filesize wl-draft-sendlog-max-size))
	    (rename-file filename (concat filename ".old") t))
	(if (file-writable-p filename)
	    (write-region-as-binary (point-min) (point-max)
				    filename t 'no-msg)
	  (message "%s is not writable." filename))))))

(defun wl-draft-get-header-delimiter (&optional delete)
  ;; If DELETE is non-nil, replace the header delimiter with a blank line
  (let (delimline)
    (goto-char (point-min))
    (when (re-search-forward
	   (concat "^" (regexp-quote mail-header-separator) "$\\|^$") nil t)
      (replace-match "")
      (if delete
	  (backward-char))
      (setq delimline (point-marker)))
    delimline))

(defun wl-draft-send-mail-with-qmail ()
  "Pass the prepared message buffer to qmail-inject.
Refer to the documentation for the variable `send-mail-function'
to find out how to use this."
  (if (and wl-draft-qmail-send-plugged
	   (not (elmo-plugged-p)))
      (wl-draft-set-sent-message 'mail 'unplugged)
    ;; send the message
    (run-hooks 'wl-mail-send-pre-hook) ;; X-PGP-Sig, Cancel-Lock
    (let ((id (elmo-get-message-id-from-buffer))
	  (to (std11-field-body "To")))
      (case
	  (as-binary-process
	   (apply
	    'call-process-region 1 (point-max) wl-qmail-inject-program
	    nil nil nil
	    wl-qmail-inject-args))
	;; qmail-inject doesn't say anything on it's stdout/stderr,
	;; we have to look at the retval instead
	(0   (progn
	       (wl-draft-set-sent-message 'mail 'sent)
	       (wl-draft-write-sendlog 'ok 'qmail nil (list to) id)))
	(1   (error "`qmail-inject' reported permanent failure"))
	(111 (error "`qmail-inject' reported transient failure"))
	;; should never happen
	(t   (error "`qmail-inject' reported unknown failure"))))))

(defun wl-draft-parse-msg-id-list-string (string)
  "Get msg-id list from STRING."
  (let (msg-id-list)
    (dolist (parsed-id (std11-parse-msg-ids-string string))
      (when (eq (car parsed-id) 'msg-id)
	(setq msg-id-list (cons (std11-msg-id-string parsed-id)
				msg-id-list))))
    (nreverse msg-id-list)))

(defun wl-draft-eword-encode-address-list (string &optional column)
  "Encode header field STRING as list of address, and return the result.
Cause an error when STRING contains invalid address.
Optional argument COLUMN is start-position of the field."
  (car (eword-encode-rword-list
	(or column eword-encode-default-start-column)
	(eword-encode-addresses-to-rword-list
	 (wl-draft-std11-parse-addresses (std11-lexical-analyze string))))))

(defun wl-draft-std11-parse-addresses (lal)
  (let ((ret (std11-parse-address lal)))
    (when (and (not (and (eq (length lal) 1)
			 (eq (car (car lal)) 'spaces)))
	       (null ret))
      (error "Error while parsing address"))
    (if ret
	(let ((dest (list (car ret))))
	  (setq lal (cdr ret))
	  (while (and (setq ret (std11-parse-ascii-token lal))
		      (string-equal (cdr (assq 'specials (car ret))) ",")
		      (setq ret (std11-parse-address (cdr ret)))
		      )
	    (setq dest (cons (car ret) dest))
	    (setq lal (cdr ret)))
	  (while (eq 'spaces (car (car lal)))
	    (setq lal (cdr lal)))
	  (if lal (error "Error while parsing address"))
	  (nreverse dest)))))

(defun wl-draft-parse-mailbox-list (field &optional remove-group-list)
  "Get mailbox list of FIELD from current buffer.
The buffer is expected to be narrowed to just the headers of the message.
If optional argument REMOVE-GROUP-LIST is non-nil, remove group list content
from current buffer."
  (save-excursion
    (let ((case-fold-search t)
	  (inhibit-read-only t)
	  addresses address
	  mailbox-list beg seq has-group-list)
      (goto-char (point-min))
      (while (re-search-forward (concat "^" (regexp-quote field) "[\t ]*:")
				nil t)
	(setq beg (point))
	(re-search-forward "^[^ \t]" nil 'move)
	(beginning-of-line)
	(skip-chars-backward "\n")
	(setq seq (std11-lexical-analyze
		   (buffer-substring-no-properties beg (point))))
	(setq addresses (wl-draft-std11-parse-addresses seq))
	(while addresses
	  (cond ((eq (car (car addresses)) 'group)
		 (setq has-group-list t)
		 (setq mailbox-list
		       (nconc mailbox-list
			      (mapcar
			       'std11-address-string
			       (nth 2 (car addresses))))))
		((eq (car (car addresses)) 'mailbox)
		 (setq address (nth 1 (car addresses)))
		 (setq mailbox-list
		       (nconc mailbox-list
			      (list
			       (std11-addr-to-string
				(if (eq (car address) 'phrase-route-addr)
				    (nth 2 address)
				  (cdr address))))))))
	  (setq addresses (cdr addresses)))
	(when (and remove-group-list has-group-list)
	  (delete-region beg (point))
	  (insert (wl-address-string-without-group-list-contents seq))))
      mailbox-list)))

(defun wl-draft-deduce-address-list (buffer header-start header-end)
  "Get address list suitable for smtp RCPT TO:<address>.
Group list content is removed if `wl-draft-remove-group-list-contents' is
non-nil."
  (let ((fields (if (and wl-draft-doing-mime-bcc
			 wl-draft-disable-bcc-for-mime-bcc)
		    '("to" "cc")
		  '("to" "cc" "bcc")))
	(resent-fields '("resent-to" "resent-cc" "resent-bcc"))
	(case-fold-search t)
	addrs recipients)
    (save-excursion
      (save-restriction
	(narrow-to-region header-start header-end)
	(goto-char (point-min))
	(save-excursion
	  (if (re-search-forward "^resent-to[\t ]*:" nil t)
	      (setq fields resent-fields)))
	(while fields
	  (setq recipients
		(nconc recipients
		       (wl-draft-parse-mailbox-list
			(car fields)
			wl-draft-remove-group-list-contents)))
	  (setq fields (cdr fields)))
	recipients))))

;;
;; from Semi-gnus
;;
(defun wl-draft-send-mail-with-smtp ()
  "Send the prepared message buffer with SMTP."
  (require 'smtp)
  (let* ((errbuf (if mail-interactive
		     (generate-new-buffer " smtp errors")
		   0))
	 (case-fold-search t)
	 (default-case-fold-search t)
	 (sender (or wl-envelope-from
		     (wl-address-header-extract-address wl-from)))
	 (delimline (save-excursion
		      (goto-char (point-min))
		      (re-search-forward
		       (concat "^" (regexp-quote mail-header-separator)
			       "$\\|^$") nil t)
		      (point-marker)))
	 (smtp-server
	  (or wl-smtp-posting-server smtp-server "localhost"))
	 (smtp-service (or wl-smtp-posting-port smtp-service))
	 (smtp-local-domain (or smtp-local-domain wl-local-domain))
	 (id (elmo-get-message-id-from-buffer))
	 recipients)
    (if (not (elmo-plugged-p smtp-server smtp-service))
	(wl-draft-set-sent-message 'mail 'unplugged
				   (cons smtp-server smtp-service))
      (unwind-protect
	  (save-excursion
	    ;; Instead of `smtp-deduce-address-list'.
	    (setq recipients (wl-draft-deduce-address-list
			      (current-buffer) (point-min) delimline))
	    (unless recipients (error "No recipients"))
	    ;; Insert an extra newline if we need it to work around
	    ;; Sun's bug that swallows newlines.
	    (goto-char (1+ delimline))
	    (if (eval mail-mailer-swallows-blank-line)
		(newline))
	    (run-hooks 'wl-mail-send-pre-hook) ;; X-PGP-Sig, Cancel-Lock
	    (if mail-interactive
		(with-current-buffer errbuf
		  (erase-buffer)))
	    (wl-draft-delete-field "bcc" delimline)
	    (wl-draft-delete-field "resent-bcc" delimline)
	    (let (process-connection-type)
	      (as-binary-process
	       (when recipients
		 (wl-smtp-extension-bind
		  (condition-case err
		      (smtp-send-buffer sender recipients (current-buffer))
		    (error
		     (wl-draft-write-sendlog 'failed 'smtp smtp-server
					     recipients id)
		     (if (and (eq (car err) 'smtp-response-error)
			      (= (nth 1 err) 535))
			 (elmo-remove-passwd
			  (wl-smtp-password-key
			   smtp-sasl-user-name
			   (car smtp-sasl-mechanisms)
			   smtp-server)))
		     (signal (car err) (cdr err)))
		    (quit
		     (wl-draft-write-sendlog 'uncertain 'smtp smtp-server
					     recipients id)
		     (signal (car err) (cdr err)))))
		 (wl-draft-set-sent-message 'mail 'sent)
		 (wl-draft-write-sendlog
		  'ok 'smtp smtp-server recipients id)))))
	(if (bufferp errbuf)
	    (kill-buffer errbuf))))))

(defun wl-draft-send-mail-with-pop-before-smtp ()
  "Send the prepared message buffer with POP-before-SMTP."
  (require 'elmo-pop3)
  (let ((folder
	 (luna-make-entity
	  'elmo-pop3-folder
	  :user   (or wl-pop-before-smtp-user
		      elmo-pop3-default-user)
	  :server (or wl-pop-before-smtp-server
		      elmo-pop3-default-server)
	  :port   (or wl-pop-before-smtp-port
		      elmo-pop3-default-port)
	  :auth   (or wl-pop-before-smtp-authenticate-type
		      elmo-pop3-default-authenticate-type)
	  :stream-type (elmo-get-network-stream-type
			(or wl-pop-before-smtp-stream-type
			    elmo-pop3-default-stream-type))))
	session)
    (condition-case error
	(progn
	  (setq session (elmo-pop3-get-session folder))
	  (when session (elmo-network-close-session session)))
      (error
       (unless (string= (nth 1 error) "Unplugged")
	 (signal (car error) (cdr error))))))
  (wl-draft-send-mail-with-smtp))

(defun wl-draft-send-mail-with-sendmail ()
  "Send the prepared message buffer with `sendmail-send-it'.
The function `sendmail-send-it' uses the external program
`sendmail-program'."
  (let ((id (elmo-get-message-id-from-buffer))
	(to (std11-field-body "to")))
    (run-hooks 'wl-mail-send-pre-hook)
    (require 'sendmail)
    (condition-case err
	;; Prevent select-message-coding-system checks from checking for
	;; a MIME charset -- message is already encoded.
	(let (select-safe-coding-system-function)
	  (setq buffer-file-coding-system 'raw-text)
	  (sendmail-send-it))
      (error
       (wl-draft-write-sendlog 'failed 'sendmail nil (list to) id)
       (signal (car err) (cdr err))))
    (wl-draft-set-sent-message 'mail 'sent)
    (wl-draft-write-sendlog 'ok 'sendmail nil (list to) id)))

(defun wl-draft-insert-required-fields (&optional force-msgid)
  "Insert Message-ID, Date, and From field.
If FORCE-MSGID, insert message-id regardless of `wl-insert-message-id'."
  ;; Insert Message-Id field...
  (goto-char (point-min))
  (when (and (or force-msgid
		 wl-insert-message-id)
	     (not (re-search-forward "^Message-ID[ \t]*:" nil t)))
    (insert (concat "Message-ID: "
		    (funcall wl-message-id-function)
		    "\n")))
  ;; Insert date field.
  (goto-char (point-min))
  (or (re-search-forward "^Date[ \t]*:" nil t)
      (wl-draft-insert-date-field))
  ;; Insert from field.
  (goto-char (point-min))
  (or (re-search-forward "^From[ \t]*:" nil t)
      (wl-draft-insert-from-field)))

(defun wl-draft-normal-send-func (editing-buffer kill-when-done)
  "Send the message in the current buffer."
  (save-restriction
    (narrow-to-region (goto-char (point-min))
		      (if (re-search-forward
			   (concat
			    "^" (regexp-quote mail-header-separator) "$")
			   nil t)
			  (match-beginning 0)
			(point-max)))
    (wl-draft-insert-required-fields)
    ;; ignore any blank lines in the header
    (while (progn (goto-char (point-min))
		  (re-search-forward "\n[ \t]*\n\n*" nil t))
      (replace-match "\n"))
    (goto-char (point-min))
    (while (re-search-forward
	    "^[^ \t\n:]+:[ \t]*\\(.*\\(\n[ \t].*\\)*\\)\n"
	    nil t)
      (when (string= "" (match-string 1))
	(replace-match ""))))
;;;  (run-hooks 'wl-mail-send-pre-hook) ; X-PGP-Sig, Cancel-Lock
  (wl-draft-dispatch-message)
  (when kill-when-done
    ;; hide editing-buffer.
    (wl-draft-hide editing-buffer)
    ;; delete editing-buffer and its file.
    (wl-draft-delete editing-buffer)))

(defun wl-draft-dispatch-message (&optional mes-string)
  "Send the message in the current buffer.  Not modified the header fields."
  (let (delimline mime-bcc)
    (if (and wl-draft-verbose-send mes-string)
	(message "%s" mes-string))
    ;; get fcc folders.
    (setq delimline (wl-draft-get-header-delimiter t))
    (unless wl-draft-fcc-list
      (setq wl-draft-fcc-list (wl-draft-get-fcc-list delimline)))
    ;;
    (setq wl-sent-message-modified nil)
    (unwind-protect
	(progn
	  (if (and (wl-message-mail-p)
		   (not (wl-draft-sent-message-p 'mail)))
	      (if (or (not (or wl-draft-force-queuing
			       wl-draft-force-queuing-mail))
		      (memq 'mail wl-sent-message-queued))
		  (progn
		    (setq mime-bcc (wl-draft-mime-bcc-field))
		    (funcall wl-draft-send-mail-function)
		    (when (not (zerop (length mime-bcc)))
		      (wl-draft-do-mime-bcc mime-bcc)))
		(push 'mail wl-sent-message-queued)
		(wl-draft-set-sent-message 'mail 'unplugged)))
	  (if (and (wl-message-news-p)
		   (not (wl-draft-sent-message-p 'news))
		   (not (wl-message-field-exists-p "Resent-to")))
	      (if (or (not (or wl-draft-force-queuing
			       wl-draft-force-queuing-news))
		      (memq 'news wl-sent-message-queued))
		  (funcall wl-draft-send-news-function)
		(push 'news wl-sent-message-queued)
		(wl-draft-set-sent-message 'news 'unplugged))))
      (let* ((status (wl-draft-sent-message-results))
	     (unplugged-via (car status))
	     (sent-via (nth 1 status)))
	;; If one sent, process fcc folder.
	(if (and sent-via wl-draft-fcc-list)
	    (progn
	      (wl-draft-do-fcc (wl-draft-get-header-delimiter)
			       wl-draft-fcc-list)
	      (setq wl-draft-fcc-list nil)))
	(if wl-draft-use-cache
	    (let ((id (std11-field-body "Message-ID"))
		  (elmo-enable-disconnected-operation t))
	      (elmo-file-cache-save (elmo-file-cache-get-path id)
				    nil)))
	;; If one unplugged, append queue.
	(when (and unplugged-via
		   wl-sent-message-modified)
	  (if wl-draft-enable-queuing
	      (progn
		(wl-draft-queue-append wl-sent-message-via)
		(setq wl-sent-message-modified 'requeue))
	    (error "Unplugged")))
	(when wl-draft-verbose-send
	  (if (and unplugged-via sent-via);; combined message
	      (progn
		(setq wl-draft-verbose-msg
		      (format "Sending%s and Queuing%s..."
			      sent-via unplugged-via))
		(message "%sdone" wl-draft-verbose-msg))
	    (if mes-string
		(message "%s%s"
			 mes-string
			 (if sent-via "done" "failed"))))))))
  (not wl-sent-message-modified)) ;; return value

(defun wl-draft-raw-send (&optional kill-when-done force-pre-hook mes-string)
  "Force send current buffer as raw message."
  (interactive)
  (save-excursion
    (let (wl-interactive-send
;;;	  wl-draft-verbose-send
	  (wl-mail-send-pre-hook (and force-pre-hook wl-mail-send-pre-hook))
	  (wl-news-send-pre-hook (and force-pre-hook wl-news-send-pre-hook))
	  mail-send-hook
	  mail-send-actions)
      (wl-draft-send kill-when-done mes-string))))

(defun wl-draft-clone-local-variables ()
  (let ((locals (buffer-local-variables))
	result)
    (while locals
      (when (and (consp (car locals))
		 (car (car locals))
		 (string-match wl-draft-clone-local-variable-regexp
			       (symbol-name (car (car locals)))))
	(wl-append result (list (car (car locals)))))
      (setq locals (cdr locals)))
    result))

(defcustom wl-draft-send-confirm-with-preview t
  "*Non-nil to invoke preview through confirmation of sending.
This variable is valid when `wl-interactive-send' has non-nil value."
  :type 'boolean
  :group 'wl-draft)

(defun wl-draft-send-confirm ()
  (unwind-protect
      (condition-case nil
	  (progn
	    (when wl-draft-send-confirm-with-preview
	      (let (wl-draft-send-hook
		    (pgg-decrypt-automatically nil))
		(wl-draft-preview-message)))
	    (save-excursion
	      (goto-char (point-min)) ; to show recipients in header
	      (funcall
	       (if (functionp wl-draft-send-confirm-type)
		   wl-draft-send-confirm-type
		 (lambda (prompt)
		   (wl-y-or-n-p-with-scroll
		    prompt
		    (eq wl-draft-send-confirm-type 'scroll-by-SPC/BS))))
	       "Send current draft? ")))
	(quit nil))
    (when (and wl-draft-send-confirm-with-preview
	       (eq major-mode 'mime-view-mode))
      (wl-mime-quit-preview))))

(defun wl-draft-send (&optional kill-when-done mes-string)
  "Send current draft message.
If KILL-WHEN-DONE is non-nil, current draft buffer is killed"
  (interactive)
;;; Don't call this explicitly.
;;; Added to 'wl-draft-send-hook (by teranisi)
;;;  (wl-draft-config-exec)
  (run-hooks 'wl-draft-send-hook)
  (when (or (not wl-interactive-send)
	    (wl-draft-send-confirm))
    (let ((send-mail-function 'wl-draft-raw-send)
	  (editing-buffer (current-buffer))
	  (sending-buffer (wl-draft-generate-clone-buffer
			   " *wl-draft-sending-buffer*"
			   (append wl-draft-config-variables
				   (wl-draft-clone-local-variables))))
	  (parent-flag wl-draft-parent-flag)
	  (parent-number wl-draft-parent-number)
	  (parent-folder wl-draft-parent-folder)
	  (wl-draft-verbose-msg nil)
	  err)
      (unwind-protect
	  (with-current-buffer sending-buffer
	    (if (and (not (wl-message-mail-p))
		     (not (wl-message-news-p)))
		(error "No recipient is specified"))
	    (expand-abbrev)		; for mail-abbrevs
	    (let ((mime-header-encode-method-alist
		   (append
		    '((wl-draft-eword-encode-address-list
		       .  (To Cc Bcc Resent-To Resent-Cc Resent-Bcc From)))
		    (if (boundp 'mime-header-encode-method-alist)
			(symbol-value 'mime-header-encode-method-alist)))))
	      (run-hooks 'mail-send-hook) ; translate buffer
	      )
	    ;;
	    (if wl-draft-verbose-send
		(message "%s" (or mes-string "Sending...")))
	    ;; Set flag before send-function because
	    ;; there's no need to change current mailbox at this time.
	    ;; If flag is set after send-function, the current mailbox
	    ;; might changed by Fcc.
	    ;; It causes a huge loss in the IMAP folder.
	    (when (and parent-flag parent-number
		       (not (zerop (length parent-folder))))
	      (condition-case nil
		  (wl-folder-set-persistent-mark
		   parent-folder parent-number parent-flag)
		(error
		 (message "Set mark (%s) failed" (symbol-name parent-flag)))))
	    (funcall wl-draft-send-function editing-buffer kill-when-done)
	    ;; Now perform actions on successful sending.
	    (while mail-send-actions
	      (condition-case ()
		  (apply (car (car mail-send-actions))
			 (cdr (car mail-send-actions)))
		(error))
	      (setq mail-send-actions (cdr mail-send-actions)))
	    (if wl-draft-verbose-send
		(message "%sdone"
			 (or wl-draft-verbose-msg
			     mes-string
			     "Sending..."))))
	;; kill sending buffer, anyway.
	(and (buffer-live-p sending-buffer)
	     (kill-buffer sending-buffer))))))

(defun wl-draft-mime-bcc-field ()
  "Return the MIME-Bcc field body.  The field is deleted."
  (prog1 (std11-field-body wl-draft-mime-bcc-field-name)
    (wl-draft-delete-field wl-draft-mime-bcc-field-name)))

(defun wl-draft-do-mime-bcc (field-body)
  "Send MIME-Bcc (Encapsulated blind carbon copy)."
  (let ((orig-from (mime-decode-field-body (std11-field-body "from")
					   'From))
	(orig-subj (mime-decode-field-body (or (std11-field-body "subject")
					       "")
					   'Subject))
	(recipients (wl-parse-addresses field-body))
	(draft-buffer (current-buffer))
	wl-draft-use-frame)
    (save-window-excursion
      (when (and (not wl-draft-doing-mime-bcc) ; To avoid infinite loop.
		 (not (zerop (length field-body))))
	(let ((wl-draft-doing-mime-bcc t))
	  (dolist (recipient recipients)
	    (wl-draft-create-buffer)
	    (wl-draft-create-contents
	     (append `((From . ,orig-from)
		       (To . ,recipient)
		       (Subject . ,(concat "A blind carbon copy ("
					   orig-subj
					   ")")))
		     (wl-draft-default-headers)))
	    (wl-draft-insert-mail-header-separator)
	    (wl-draft-prepare-edit)
	    (goto-char (point-max))
	    (insert (or wl-draft-mime-bcc-body
			"This is a blind carbon copy.")
		    "\n")
	    (mime-edit-insert-tag "message" "rfc822")
	    (insert-buffer-substring draft-buffer)
	    (let (wl-interactive-send)
	      (wl-draft-send 'kill-when-done))))))))

(defun wl-draft-save ()
  "Save current draft."
  (interactive)
  (if (buffer-modified-p)
      (progn
	(message "Saving...")
	(let ((msg (buffer-substring-no-properties (point-min) (point-max)))
	      (current-number wl-draft-buffer-message-number)
	      next-number)
	  ;; If no header separator, insert it.
	  (with-temp-buffer
	    (insert msg)
	    (goto-char (point-min))
	    (unless (re-search-forward
		     (concat "^" (regexp-quote mail-header-separator) "$")
		     nil t)
	      (goto-char (point-min))
	      (if (re-search-forward "\n\n" nil t)
		  (replace-match (concat "\n" mail-header-separator "\n"))
		(goto-char (point-max))
		(insert (if (eq (preceding-char) ?\n) "" "\n")
			mail-header-separator "\n")))
	    (let ((mime-header-encode-method-alist
		   (append
		    '((eword-encode-unstructured-field-body
		       .  (To Cc Bcc Resent-To Resent-Cc Resent-Bcc From)))
		    (if (boundp 'mime-header-encode-method-alist)
			(symbol-value 'mime-header-encode-method-alist)))))
	      (mime-edit-translate-buffer))
	    (wl-draft-get-header-delimiter t)
	    (when current-number
	      (elmo-folder-delete-messages (wl-draft-get-folder)
					   (list current-number))
	      (wl-draft-config-info-operation current-number 'delete))
	    (elmo-folder-check (wl-draft-get-folder))
	    (setq next-number
		  (elmo-folder-append-buffer (wl-draft-get-folder) nil nil t)))
	  (elmo-folder-check (wl-draft-get-folder))
	  (elmo-folder-commit (wl-draft-get-folder))
	  (if (not (numberp next-number))
	      (setq wl-draft-buffer-message-number nil)
	    (setq wl-draft-buffer-message-number next-number)
	    (rename-buffer (format "%s/%d" (if (memq 'modeline wl-use-folder-petname)
					       (wl-folder-get-petname wl-draft-folder)
					     wl-draft-folder) next-number) t)
	    (wl-draft-config-info-operation wl-draft-buffer-message-number 'save))
	  (setq buffer-file-name (buffer-name))
	  (set-buffer-modified-p nil)
	  (message "Saving...done")))
    (message "(No changes need to be saved)")))

(defun wl-draft-mimic-kill-buffer ()
  "Kill the current (draft) buffer with query."
  (interactive)
  (let ((bufname (read-buffer (format "Kill buffer: (default %s) "
				      (buffer-name))))
	wl-draft-use-frame)
    (if (or (not bufname)
	    (string-equal bufname "")
	    (string-equal bufname (buffer-name)))
	(let ((bufname (current-buffer)))
	  (when (or (not (buffer-modified-p))
		    (yes-or-no-p
		     (format "Buffer %s modified; kill anyway? " bufname)))
	    (set-buffer-modified-p nil)
	    (wl-draft-hide bufname)
	    (kill-buffer bufname)))
      (kill-buffer bufname))))

(defun wl-draft-save-and-exit ()
  "Save current draft and exit current draft mode."
  (interactive)
  (wl-draft-save)
  (let ((editing-buffer (current-buffer)))
    (wl-draft-hide editing-buffer)
    (kill-buffer editing-buffer)))

(defun wl-draft-send-and-exit ()
  "Send current draft message and kill it."
  (interactive)
  (wl-draft-send t))

(defun wl-draft-send-from-toolbar ()
  (interactive)
  (let ((wl-interactive-send t))
    (wl-draft-send-and-exit)))

(defun wl-draft-delete-field (field &optional delimline replace)
  (wl-draft-delete-fields (regexp-quote field) delimline replace))

(defun wl-draft-delete-fields (field &optional delimline replace)
  (save-restriction
    (unless delimline
      (goto-char (point-min))
      (if (search-forward "\n\n" nil t)
	  (setq delimline (point))
	(setq delimline (point-max))))
    (narrow-to-region (point-min) delimline)
    (goto-char (point-min))
    (let ((regexp (concat "^" field ":"))
	  (case-fold-search t))
      (while (not (eobp))
	(if (looking-at regexp)
	    (progn
	      (delete-region
	       (point)
	       (progn
		 (forward-line)
		 (if (re-search-forward "^[^ \t]" nil t)
		     (goto-char (match-beginning 0))
		   (point-max))))
	      (if replace
		  (insert (concat field ": " replace "\n"))))
	  (forward-line)
	  (if (re-search-forward "^[^ \t]" nil t)
	      (goto-char (match-beginning 0))
	    (point-max)))))))

(defun wl-draft-get-fcc-list (header-end)
  (if (and wl-draft-doing-mime-bcc
	   wl-draft-disable-fcc-for-mime-bcc)
      (progn
	(wl-draft-delete-field "fcc")
	nil)
    (let (fcc-list
	  (case-fold-search t))
      (or (markerp header-end) (error "HEADER-END must be a marker"))
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^Fcc:[ \t]*" header-end t)
	  (save-match-data
	    (setq fcc-list
		  (append fcc-list
			  (split-string
			   (buffer-substring-no-properties
			    (point)
			    (progn
			      (end-of-line)
			      (skip-chars-backward " \t")
			      (point)))
			   ",[ \t]*")))
	    (dolist (folder fcc-list)
	      (wl-folder-confirm-existence
	       (wl-folder-get-elmo-folder (eword-decode-string folder)))))
	  (delete-region (match-beginning 0)
			 (progn (forward-line) (point)))))
      fcc-list)))

(defcustom wl-draft-fcc-append-read-folder-history t
  "Non-nil to append fcc'ed folder to `wl-read-folder-history'."
  :type 'boolean
  :group 'wl-draft)

(defun wl-draft-do-fcc (header-end &optional fcc-list)
  (let ((send-mail-buffer (current-buffer))
	(case-fold-search t)
	beg end)
    (or (markerp header-end) (error "HEADER-END must be a marker"))
    (unless fcc-list
      (setq fcc-list (wl-draft-get-fcc-list header-end)))
    (with-temp-buffer
      ;; insert just the headers to avoid moving the gap more than
      ;; necessary (the message body could be arbitrarily huge.)
      (insert-buffer-substring send-mail-buffer 1 header-end)
      (wl-draft-insert-required-fields t)
      (goto-char (point-max))
      (insert-buffer-substring send-mail-buffer header-end)
      (let ((id (std11-field-body "Message-ID"))
	    (elmo-enable-disconnected-operation t))
	(while fcc-list
          (let ((folder (wl-folder-get-elmo-folder
                         (eword-decode-string (car fcc-list)))))
            (elmo-folder-open folder)
            (if (elmo-folder-append-buffer folder (and wl-fcc-force-as-read '(read)))
                (wl-draft-write-sendlog 'ok 'fcc nil (car fcc-list) id)
              (wl-draft-write-sendlog 'failed 'fcc nil (car fcc-list) id))
            (if (and wl-draft-fcc-append-read-folder-history
                     (boundp 'wl-read-folder-history))
                (or (equal (car fcc-list) (car wl-read-folder-history))
                    (setq wl-read-folder-history
                          (append (list (car fcc-list)) wl-read-folder-history))))
            (setq fcc-list (cdr fcc-list))))))))

(defun wl-draft-on-field-p ()
  (if (< (point)
	 (save-excursion
	   (goto-char (point-min))
	   (search-forward (concat "\n" mail-header-separator "\n") nil 0)
	   (point)))
      (if (bolp)
	  (if (bobp)
	      t
	    (save-excursion
	      (forward-line -1)
	      (if (or (looking-at ".*,[ \t]?$")
		      (looking-at "^[^ \t]+:[ \t]+.*:$")); group list name
		  nil t)))
	(let ((pos (point)))
	  (save-excursion
	    (beginning-of-line)
	    (if (looking-at "^[ \t]")
		nil
	      (if (re-search-forward ":" pos t) nil t)))))))

;;;;;;;;;;;;;;;;
;;;###autoload
(defun wl-draft (&optional header-alist
			   content-type content-transfer-encoding
			   body edit-again
			   parent-folder
			   parent-number)
  "Write and send mail/news message with Wanderlust."
  (interactive)
  (require 'wl)
  (unless wl-init
    (wl-load-profile)
    (wl-folder-init)
    (elmo-init)
    (wl-plugged-init t))
  (let (wl-demo)
    (wl-init)) ; returns immediately if already initialized.

  (wl-start-save-drafts)
  (let (buffer header-alist-internal)
    (setq buffer (wl-draft-create-buffer parent-folder parent-number))
    (unless (cdr (assq 'From header-alist))
      (setq header-alist
	    (append (list (cons 'From wl-from)) header-alist)))
    (unless (cdr (assq 'To header-alist))
      (let ((to))
	(when (setq to (and
			(interactive-p)
			""))
	  (if (assq 'To header-alist)
	      (setcdr (assq 'To header-alist) to)
	    (setq header-alist
		  (append header-alist
			  (list (cons 'To to))))))))
    (unless (cdr (assq 'Subject header-alist))
      (if (assq 'Subject header-alist)
	  (setcdr (assq 'Subject header-alist) "")
	(setq header-alist
	      (append header-alist (list (cons 'Subject ""))))))
    (setq header-alist (append header-alist
			       (wl-draft-default-headers)
			       wl-draft-additional-header-alist
			       (if body (list "" (cons 'Body body)))))
    (wl-draft-create-contents header-alist)
    (if edit-again
	(wl-draft-decode-body
	 content-type content-transfer-encoding))
    (wl-draft-insert-mail-header-separator)
    (wl-draft-prepare-edit)
    (if (interactive-p)
	(run-hooks 'wl-mail-setup-hook))
    (goto-char (point-min))
    (setq buffer-undo-list nil)
    (wl-user-agent-compose-internal) ;; user-agent
    (cond ((and
	    (interactive-p)
	    (string= (cdr (assq 'To header-alist)) ""))
	   (mail-position-on-field "To"))
	  (t
	   (goto-char (point-max))))
    buffer))

(defun wl-draft-create-buffer (&optional parent-folder parent-number)
  (let* ((draft-folder (wl-draft-get-folder))
	 (reply-or-forward
	  (or (eq this-command 'wl-summary-reply)
	      (eq this-command 'wl-summary-reply-with-citation)
	      (eq this-command 'wl-summary-forward)
	      (eq this-command 'wl-summary-target-mark-forward)
	      (eq this-command 'wl-summary-target-mark-reply-with-citation)))
	 (buffer (generate-new-buffer "*draft*"))) ; Just for initial name.
    (set-buffer buffer)
    ;; switch-buffer according to draft buffer style.
    (if wl-draft-use-frame
	(switch-to-buffer-other-frame buffer)
      (if reply-or-forward
	  (case wl-draft-reply-buffer-style
	    (split
	     (split-window-vertically)
	     (other-window 1)
	     (switch-to-buffer buffer))
	    (keep
	     (switch-to-buffer buffer))
	    (full
	     (delete-other-windows)
	     (switch-to-buffer buffer))
	    (t
	     (if (functionp wl-draft-reply-buffer-style)
		 (funcall wl-draft-reply-buffer-style buffer)
	       (error "Invalid value for wl-draft-reply-buffer-style"))))
	(case wl-draft-buffer-style
	  (split
	   (when (eq major-mode 'wl-summary-mode)
	     (wl-summary-toggle-disp-msg 'off))
	   (split-window-vertically)
	   (other-window 1)
	   (switch-to-buffer buffer))
	  (keep
	   (switch-to-buffer buffer))
	  (full
	   (delete-other-windows)
	   (switch-to-buffer buffer))
	  (t (if (functionp wl-draft-buffer-style)
		 (funcall wl-draft-buffer-style buffer)
	       (error "Invalid value for wl-draft-buffer-style"))))))
    (auto-save-mode -1)
    (let (change-major-mode-hook)
      (wl-draft-mode))
    (set-buffer-multibyte t)		; draft buffer is always multibyte.
    (make-local-variable 'truncate-partial-width-windows)
    (setq truncate-partial-width-windows nil)
    (setq truncate-lines wl-draft-truncate-lines)
    (setq wl-sent-message-via nil)
    (setq wl-sent-message-queued nil)
    (setq wl-draft-config-exec-flag t)
    (setq wl-draft-parent-folder (or parent-folder ""))
    (setq wl-draft-parent-number parent-number)
    (or (eq this-command 'wl-folder-write-current-folder)
	(setq wl-draft-buffer-cur-summary-buffer
	      (wl-summary-get-buffer parent-folder)))
    buffer))

(defun wl-draft-create-contents (header-alist)
  "header-alist' sample
'(function  ;; funcall
  string    ;; insert string
  (symbol . string)    ;;  insert symbol-value: string
  (symbol . function)  ;;  (funcall) and if it returns string,
                       ;;  insert symbol-value: string
  (symbol . nil)       ;;  do nothing
  nil                  ;;  do nothing
  )"
  (unless (eq major-mode 'wl-draft-mode)
    (error "`wl-draft-create-header' must be use in wl-draft-mode"))
  (let ((halist header-alist)
	field value)
    (while halist
      (cond
       ;; function
       ((functionp (car halist)) (funcall (car halist)))
       ;; string
       ((stringp (car halist)) (insert (car halist) "\n"))
       ;; cons
       ((consp (car halist))
	(setq field (car (car halist)))
	(setq value (cdr (car halist)))
	(cond
	 ((symbolp field)
	  (cond
	   ((eq field 'Body) ; body
	    (insert value))
	   ((stringp value) (insert (symbol-name field) ": " value "\n"))
	   ((functionp value)
	    (let ((value-return (funcall value)))
	      (when (stringp value-return)
		(insert (symbol-name field) ": " value-return "\n"))))
	   ((not value))
	   (t
	    (debug))))
	 ;;
	 ((not field))
	 (t
	  (debug))
	 )))
      (setq halist (cdr halist)))))

(defun wl-draft-prepare-edit ()
  (unless (eq major-mode 'wl-draft-mode)
    (error "`wl-draft-create-header' must be use in wl-draft-mode"))
  (let (change-major-mode-hook)
    (wl-draft-editor-mode)
    (static-when (boundp 'auto-save-file-name-transforms)
      (make-local-variable 'auto-save-file-name-transforms)
      (setq auto-save-file-name-transforms
	    (cons (list (concat (regexp-quote wl-draft-folder)
				"/\\([0-9]+\\)")
			(concat (expand-file-name
				 "auto-save-"
				 (elmo-folder-msgdb-path
				  (wl-draft-get-folder)))
				"\\1"))
		  auto-save-file-name-transforms)))
    (when wl-draft-write-file-function
      (add-hook 'local-write-file-hooks wl-draft-write-file-function))
    (wl-draft-overload-functions)
    (unless wl-draft-jit-highlight
      (wl-highlight-headers 'for-draft))
    (wl-draft-save)
    (clear-visited-file-modtime)))

(defun wl-draft-decode-header ()
  (save-excursion
    (std11-narrow-to-header)
    (wl-draft-decode-message-in-buffer)
    (widen)))

(defun wl-draft-decode-body (&optional content-type content-transfer-encoding)
  (let ((content-type
	 (or content-type
		(std11-field-body "content-type")))
	(content-transfer-encoding
	 (or content-transfer-encoding
	     (std11-field-body "content-transfer-encoding")))
	delimline)
    (save-excursion
      (std11-narrow-to-header)
      (wl-draft-delete-field "content-type")
      (wl-draft-delete-field "content-transfer-encoding")
      (goto-char (point-max))
      (setq delimline (point-marker))
      (widen)
      (narrow-to-region delimline (point-max))
      (goto-char (point-min))
      (when content-type
	(insert "Content-type: " content-type "\n"))
      (when content-transfer-encoding
	(insert "Content-Transfer-Encoding: " content-transfer-encoding "\n"))
      (wl-draft-decode-message-in-buffer)
      (goto-char (point-min))
      (unless (re-search-forward "^$" (point-at-eol) t)
	(insert "\n"))
      (widen)
      delimline)))

;;; subroutine for wl-draft-create-contents
;;; must be used in wl-draft-mode
(defun wl-draft-check-new-line ()
  (if (not (= (preceding-char) ?\n))
      (insert ?\n)))

(defsubst wl-draft-trim-ccs (cc)
  (let ((field
	 (if (functionp cc)
	     (funcall cc)
	   cc)))
    (if (and field
	     (null (and wl-draft-delete-myself-from-bcc-fcc
			(elmo-list-member
			 (mapcar 'wl-address-header-extract-address
				 (append
				  (wl-parse-addresses (std11-field-body "To"))
				  (wl-parse-addresses (std11-field-body "Cc"))))
			 (mapcar 'downcase wl-subscribed-mailing-list)))))
	field
      nil)))

(defun wl-draft-default-headers ()
  (list
   (cons 'Mail-Reply-To (and wl-insert-mail-reply-to
			     (wl-address-header-extract-address
			      wl-from)))
   (cons 'User-Agent wl-generate-mailer-string-function)
   (cons 'Reply-To mail-default-reply-to)
   (cons 'Bcc (function
	       (lambda ()
		 (wl-draft-trim-ccs
		  (or wl-bcc (and mail-self-blind (user-login-name)))))))
   (cons 'Fcc (function
	       (lambda ()
		 (wl-draft-trim-ccs wl-fcc))))
   (cons 'Organization wl-organization)
   (and wl-auto-insert-x-face
	(file-exists-p wl-x-face-file)
	'wl-draft-insert-x-face-field-here) ;; allow nil
   mail-default-headers
   ;; check \n at th end of line for `mail-default-headers'
   'wl-draft-check-new-line
   ))

(defun wl-draft-insert-mail-header-separator (&optional delimline)
  (save-excursion
    (if delimline
	(goto-char delimline)
      (goto-char (point-min))
      (if (search-forward "\n\n" nil t)
	  (delete-char -1)
	(goto-char (point-max))))
    (wl-draft-check-new-line)
    (put-text-property (point)
		       (progn
			 (insert mail-header-separator "\n")
			 (1- (point)))
		       'category 'mail-header-separator)
    (point)))

;;;;;;;;;;;;;;;;

(defun wl-draft-elmo-nntp-send ()
  (let ((elmo-nntp-post-pre-hook wl-news-send-pre-hook)
	(elmo-nntp-default-user
	 (or wl-nntp-posting-user elmo-nntp-default-user))
	(elmo-nntp-default-server
	 (or wl-nntp-posting-server elmo-nntp-default-server))
	(elmo-nntp-default-port
	 (or wl-nntp-posting-port elmo-nntp-default-port))
	(elmo-nntp-default-stream-type
	 (or wl-nntp-posting-stream-type elmo-nntp-default-stream-type))
	(elmo-nntp-default-function wl-nntp-posting-function)
	condition)
    (if (setq condition (cdr (elmo-string-matched-assoc
			      (std11-field-body "Newsgroups")
			      wl-nntp-posting-config-alist)))
	(if (stringp condition)
	    (setq elmo-nntp-default-server condition)
	  (while (car condition)
	    (set (intern (format "elmo-nntp-default-%s"
				 (symbol-name (caar condition))))
		 (cdar condition))
	    (setq condition (cdr condition)))))
    (unless elmo-nntp-default-function
      (error "wl-draft-nntp-send: posting-function is nil"))
    (if (not (elmo-plugged-p elmo-nntp-default-server elmo-nntp-default-port))
	(wl-draft-set-sent-message 'news 'unplugged
				   (cons elmo-nntp-default-server
					 elmo-nntp-default-port))
      (funcall elmo-nntp-default-function
	       elmo-nntp-default-server (current-buffer))
      (wl-draft-set-sent-message 'news 'sent)
      (wl-draft-write-sendlog 'ok 'nntp elmo-nntp-default-server
			      (std11-field-body "Newsgroups")
			      (std11-field-body "Message-ID")))))

(defun wl-draft-generate-clone-buffer (name &optional local-variables)
  "Generate clone of current buffer named NAME."
  (let ((editing-buffer (current-buffer)))
    (with-current-buffer (generate-new-buffer name)
      (erase-buffer)
      (wl-draft-mode)
      (wl-draft-editor-mode)
      (insert-buffer-substring editing-buffer)
      (message "")
      (while local-variables
	(make-local-variable (car local-variables))
	(set (car local-variables)
	     (with-current-buffer editing-buffer
	       (symbol-value (car local-variables))))
	(setq local-variables (cdr local-variables)))
      (current-buffer))))

(defun wl-draft-remove-text-plain-tag ()
  "Remove text/plain tag of mime-edit."
  (when (string= (mime-make-text-tag "plain")
		 (buffer-substring-no-properties (point-at-bol)(point-at-eol)))
    (delete-region (point-at-bol)(1+ (point-at-eol)))))

(defun wl-draft-reedit (number)
  (let ((draft-folder (wl-draft-get-folder))
	(wl-draft-reedit t)
	(num 0)
	buffer change-major-mode-hook body-top)
    (setq buffer (get-buffer-create (format "%s/%d" (if (memq 'modeline wl-use-folder-petname)
							(wl-folder-get-petname wl-draft-folder)
						      wl-draft-folder)
					    number)))
    (if wl-draft-use-frame
	(switch-to-buffer-other-frame buffer)
      (switch-to-buffer buffer))
    (set-buffer buffer)
    (elmo-message-fetch draft-folder number (elmo-make-fetch-strategy 'entire))
    (elmo-delete-cr-buffer)
    (let ((mime-edit-again-ignored-field-regexp
	   "^\\(Content-.*\\|Mime-Version\\):"))
      (wl-draft-decode-message-in-buffer))
    (setq body-top (wl-draft-insert-mail-header-separator))
    (auto-save-mode -1)
    (wl-draft-mode)
    (make-local-variable 'truncate-partial-width-windows)
    (setq truncate-partial-width-windows nil)
    (setq truncate-lines wl-draft-truncate-lines)
    (setq wl-sent-message-via nil)
    (setq wl-sent-message-queued nil)
    (wl-draft-config-info-operation number 'load)
    (goto-char (point-min))
    (wl-draft-overload-functions)
    (wl-draft-editor-mode)
    (static-when (boundp 'auto-save-file-name-transforms)
      (make-local-variable 'auto-save-file-name-transforms)
      (setq auto-save-file-name-transforms
	    (cons (list (concat (regexp-quote wl-draft-folder)
				"/\\([0-9]+\\)")
			(concat (expand-file-name
				 "auto-save-"
				 (elmo-folder-msgdb-path
				  (wl-draft-get-folder)))
				"\\1"))
		  auto-save-file-name-transforms)))
    (setq buffer-file-name (buffer-name)
	  wl-draft-buffer-message-number number)
    (unless wl-draft-parent-folder
      (setq wl-draft-parent-folder ""))
    (when wl-draft-write-file-function
      (add-hook 'local-write-file-hooks wl-draft-write-file-function))
    (unless wl-draft-jit-highlight
      (wl-highlight-headers 'for-draft))
    (goto-char body-top)
    (run-hooks 'wl-draft-reedit-hook)
    (goto-char (point-max))
    buffer))

(defun wl-draft-body-goto-top ()
  (goto-char (point-min))
  (if (re-search-forward mail-header-separator nil t)
      (forward-char)
    (goto-char (point-max))))

(defun wl-draft-body-goto-bottom ()
  (goto-char (point-max)))

(defun wl-draft-config-body-goto-header ()
  (goto-char (point-min))
  (if (re-search-forward mail-header-separator nil t)
      (beginning-of-line)
    (goto-char (point-max))))

(defsubst wl-draft-config-sub-eval-insert (content &optional newline)
  (let (content-value)
    (when (and content
	       (stringp (setq content-value (eval content))))
      (insert content-value)
      (if newline (insert "\n")))))

(defun wl-draft-config-sub-body (content)
  (wl-draft-body-goto-top)
  (delete-region (point) (point-max))
  (wl-draft-config-sub-eval-insert content))

(defun wl-draft-config-sub-top (content)
  (wl-draft-body-goto-top)
  (wl-draft-config-sub-eval-insert content))

(defun wl-draft-config-sub-bottom (content)
  (wl-draft-body-goto-bottom)
  (wl-draft-config-sub-eval-insert content))

(defun wl-draft-config-sub-header (content)
  (wl-draft-config-body-goto-header)
  (wl-draft-config-sub-eval-insert content 'newline))

(defun wl-draft-config-sub-header-top (content)
  (goto-char (point-min))
  (wl-draft-config-sub-eval-insert content 'newline))

(defun wl-draft-config-sub-part-top (content)
  (goto-char (mime-edit-content-beginning))
  (wl-draft-config-sub-eval-insert content 'newline))

(defun wl-draft-config-sub-part-bottom (content)
  (goto-char (mime-edit-content-end))
  (wl-draft-config-sub-eval-insert content 'newline))

(defsubst wl-draft-config-sub-file (content)
  (let ((coding-system-for-read wl-cs-autoconv)
	(file (expand-file-name (eval content))))
    (if (file-exists-p file)
	(insert-file-contents file)
      (error "%s: no exists file" file))))

(defun wl-draft-config-sub-body-file (content)
  (wl-draft-body-goto-top)
  (delete-region (point) (point-max))
  (wl-draft-config-sub-file content))

(defun wl-draft-config-sub-top-file (content)
  (wl-draft-body-goto-top)
  (wl-draft-config-sub-file content))

(defun wl-draft-config-sub-bottom-file (content)
  (wl-draft-body-goto-bottom)
  (wl-draft-config-sub-file content))

(defun wl-draft-config-sub-header-file (content)
  (wl-draft-config-body-goto-header)
  (wl-draft-config-sub-file content))

(defun wl-draft-config-sub-template (content)
  (setq wl-draft-config-variables
	(wl-template-insert (eval content))))

(defun wl-draft-config-sub-x-face (content)
  (if (and (string-match "\\.xbm\\(\\.gz\\)?$" content)
	   (fboundp 'x-face-insert)) ; x-face.el is installed.
      (x-face-insert content)
    (wl-draft-replace-field "X-Face" (elmo-get-file-string content t) t)))

(defsubst wl-draft-config-sub-func (field content)
  (let (func)
    (if (setq func (assq field wl-draft-config-sub-func-alist))
	(let (wl-draft-config-variables)
	  (funcall (cdr func) content)
	  ;; for wl-draft-config-sub-template
	  (cons t wl-draft-config-variables)))))

(defsubst wl-draft-config-exec-sub (clist)
  (let (config local-variables)
    (while clist
      (setq config (car clist))
      (cond
       ((functionp config)
	(funcall config))
       ((consp config)
	(let ((field (car config))
	      (content (cdr config))
	      ret-val)
	  (cond
	   ((stringp field)
	    (wl-draft-replace-field field (eval content) t))
	   ((setq ret-val (wl-draft-config-sub-func field content))
	    (if (cdr ret-val) ;; for wl-draft-config-sub-template
		(wl-append local-variables (cdr ret-val))))
	   ((boundp field) ;; variable
	    (make-local-variable field)
	    (set field (eval content))
	    (wl-append local-variables (list field)))
	   (t
	    (error "%s: not variable" field)))))
       (t
	(error "%s: not supported type" config)))
      (setq clist (cdr clist)))
    local-variables))

(defun wl-draft-prepared-config-exec (&optional config-alist reply-buf)
  "Change headers in draft preparation time."
  (interactive)
  (unless wl-draft-reedit
    (let ((config-alist
	   (or config-alist
	       (and (boundp 'wl-draft-prepared-config-alist)
		    wl-draft-prepared-config-alist)	;; For compatible.
	       wl-draft-config-alist)))
      (if config-alist
	  (wl-draft-config-exec config-alist reply-buf)))))

(defun wl-draft-config-exec (&optional config-alist reply-buf)
  "Change headers according to the value of `wl-draft-config-alist'.
Automatically applied in draft sending time."
  (interactive)
  (let ((case-fold-search t)
	(alist (or config-alist wl-draft-config-alist))
	(reply-buf (or reply-buf (and (buffer-live-p wl-draft-reply-buffer)
				      wl-draft-reply-buffer)))
	(local-variables wl-draft-config-variables)
	wl-draft-idle-highlight
	key clist found)
    (when (and (or (interactive-p)
		   wl-draft-config-exec-flag)
	       alist)
      (save-excursion
	(catch 'done
	  (while alist
	    (setq key (caar alist)
		  clist (cdar alist))
	    (cond
	     ((eq key 'reply)
	      (when (and
		     reply-buf
		     (with-current-buffer reply-buf
		       (save-restriction
			 (std11-narrow-to-header)
			 (goto-char (point-min))
			 (re-search-forward (car clist) nil t))))
		(wl-draft-config-exec-sub (cdr clist))
		(setq found t)))
	     ((stringp key)
	      (when (save-restriction
		      (std11-narrow-to-header mail-header-separator)
		      (goto-char (point-min))
		      (re-search-forward key nil t))
		(wl-append local-variables
			   (wl-draft-config-exec-sub clist))
		(setq found t)))
	     ((eval key)
	      (wl-append local-variables
			 (wl-draft-config-exec-sub clist))
	      (setq found t)))
	    (if (and found wl-draft-config-matchone)
		(throw 'done t))
	    (setq alist (cdr alist)))))
      (if found
	  (setq wl-draft-config-exec-flag nil))
      (run-hooks 'wl-draft-config-exec-hook)
      (put-text-property (point-min)(point-max) 'face nil)
      (unless wl-draft-jit-highlight
        (wl-highlight-message (point-min)(point-max) t))
      (setq wl-draft-config-variables
	    (elmo-uniq-list local-variables)))))

(defun wl-draft-replace-field (field content &optional add)
  (save-excursion
    (save-restriction
      (let ((case-fold-search t)
	    (inhibit-read-only t) ;; added by teranisi.
	    beg)
	(std11-narrow-to-header mail-header-separator)
	(goto-char (point-min))
	(if (re-search-forward (concat "^" (regexp-quote field) ":") nil t)
	    (if content
		;; replace field
		(progn
		  (setq beg (point))
		  (re-search-forward "^[^ \t]" nil 'move)
		  (beginning-of-line)
		  (skip-chars-backward "\n")
		  (delete-region beg (point))
		  (insert " " content))
	      ;; delete field
	      (save-excursion
		(beginning-of-line)
		(setq beg (point)))
	      (re-search-forward "^[^ \t]" nil 'move)
	      (beginning-of-line)
	      (delete-region beg (point)))
	  (when (and add content)
	    ;; add field
	    (goto-char (point-max))
	    (insert (concat field ": " content "\n"))))))))

(defsubst wl-draft-config-info-filename (number msgdb-dir)
  (expand-file-name
   (format "%s-%d" wl-draft-config-save-filename number)
   msgdb-dir))

(defun wl-draft-config-info-operation (msg operation)
  (let* ((msgdb-dir (elmo-folder-msgdb-path (wl-draft-get-folder)))
	 (filename (wl-draft-config-info-filename msg msgdb-dir))
	 element alist variable)
    (cond
     ((eq operation 'save)
      (let ((variables (elmo-uniq-list wl-draft-config-variables)))
	(while (setq variable (pop variables))
	  (when (boundp variable)
	    (wl-append alist
		       (list (cons variable (eval variable))))))
	(elmo-object-save filename alist)))
     ((eq operation 'load)
      (setq alist (elmo-object-load filename))
      (while (setq element (pop alist))
	(set (make-local-variable (car element)) (cdr element))
	(wl-append wl-draft-config-variables (list (car element)))))
     ((eq operation 'delete)
      (if (file-exists-p filename)
	  (delete-file filename))))))

(defun wl-draft-queue-info-operation (msg operation
					  &optional add-sent-message-via)
  (let* ((msgdb-dir (elmo-folder-msgdb-path
		     (wl-folder-get-elmo-folder wl-queue-folder)))
	 (filename
	  (expand-file-name
	   (format "%s-%d" wl-draft-queue-save-filename msg)
	   msgdb-dir))
	 element alist variable)
    (cond
     ((eq operation 'save)
      (let ((variables (elmo-uniq-list
			(append wl-draft-queue-save-variables
				wl-draft-config-variables
				(list 'wl-draft-fcc-list)))))
	(if add-sent-message-via
	    (progn
	      (push 'wl-sent-message-queued variables)
	      (push 'wl-sent-message-via variables)))
	(while (setq variable (pop variables))
	  (when (boundp variable)
	    (wl-append alist
		       (list (cons variable (eval variable))))))
	(elmo-object-save filename alist)))
     ((eq operation 'load)
      (setq alist (elmo-object-load filename))
      (while (setq element (pop alist))
	(set (make-local-variable (car element)) (cdr element))))
     ((eq operation 'get-sent-via)
      (setq alist (elmo-object-load filename))
      (cdr (assq 'wl-sent-message-via alist)))
     ((eq operation 'delete)
      (if (file-exists-p filename)
	  (delete-file filename))))))

(defun wl-draft-queue-append (wl-sent-message-via)
  (if wl-draft-verbose-send
      (message "Queuing..."))
  (let ((send-buffer (current-buffer))
	(folder (wl-folder-get-elmo-folder wl-queue-folder))
	(message-id (std11-field-body "Message-ID")))
    (if (elmo-folder-append-buffer folder)
	(progn
	  (wl-draft-queue-info-operation
	   (car (elmo-folder-status folder))
	   'save wl-sent-message-via)
	  (wl-draft-write-sendlog 'ok 'queue nil wl-queue-folder message-id)
	  (when wl-draft-verbose-send
	    (setq wl-draft-verbose-msg "Queuing...")
	    (message "Queuing...done")))
      (wl-draft-write-sendlog 'failed 'queue nil wl-queue-folder message-id)
      (error "Queuing failed"))))

(defun wl-draft-queue-flush ()
  "Flush draft queue."
  (interactive)
  (let* ((queue-folder (wl-folder-get-elmo-folder wl-queue-folder))
	 (msgs2 (progn
		  (elmo-folder-open-internal queue-folder)
		  (elmo-folder-list-messages queue-folder)))
	 (i 0)
	 (performed 0)
	 (wl-draft-queue-flushing t)
	 msgs failure len buffer msgid sent-via)
    ;; get plugged send message
    (while msgs2
      (setq sent-via (wl-draft-queue-info-operation (car msgs2) 'get-sent-via))
      (catch 'found
	(while sent-via
	  (when (and (eq (nth 1 (car sent-via)) 'unplugged)
		     (or (not (nth 2 (car sent-via)))
			 (elmo-plugged-p
			  (car (nth 2 (car sent-via)))
			  (cdr (nth 2 (car sent-via))))))
	    (wl-append msgs (list (car msgs2)))
	    (throw 'found t))
	  (setq sent-via (cdr sent-via))))
      (setq msgs2 (cdr msgs2)))
    (when (> (setq len (length msgs)) 0)
      (if (elmo-y-or-n-p (format
			  "%d message(s) are in the sending queue.  Send now? "
			  len)
			 (not elmo-dop-flush-confirm) t)
	  (progn
	    (save-excursion
	      (setq buffer (get-buffer-create " *wl-draft-queue-flush*"))
	      (set-buffer buffer)
	      (while msgs
		;; reset buffer local variables
		(kill-all-local-variables)
		(erase-buffer)
		(setq i (+ 1 i)
		      failure nil)
		(setq wl-sent-message-via nil)
		(wl-draft-queue-info-operation (car msgs) 'load)
		(elmo-message-fetch queue-folder
				    (car msgs)
				    (elmo-make-fetch-strategy 'entire))
		(condition-case err
		    (setq failure (funcall
				   wl-draft-queue-flush-send-function
				   (format "Sending (%d/%d)..." i len)))
;;;		  (wl-draft-raw-send nil nil
;;;				     (format "Sending (%d/%d)..." i len))
		  (error
		   (elmo-display-error err t)
		   (setq failure t))
		  (quit
		   (setq failure t)))
		(if (eq wl-sent-message-modified 'requeue)
		    (progn
		      (elmo-folder-delete-messages
		       queue-folder (cons (car msgs) nil))
		      (wl-draft-queue-info-operation (car msgs) 'delete))
		  (unless failure
		    (elmo-folder-delete-messages
		     queue-folder (cons (car msgs) nil))
		    (wl-draft-queue-info-operation (car msgs) 'delete)
		    (setq performed (+ 1 performed))))
		(setq msgs (cdr msgs)))
	      (kill-buffer buffer)
	      (message "%d message(s) are sent." performed)))
	(message "%d message(s) are remained to be sent." len))
      (elmo-folder-close queue-folder)
      len)))

(defun wl-jump-to-draft-buffer (&optional arg)
  "Jump to the draft if exists."
  (interactive "P")
  (if arg
      (wl-jump-to-draft-folder)
    (let ((draft-bufs (wl-collect-draft))
	  buf)
      (cond
       ((null draft-bufs)
	(message "No draft buffer exist."))
       (t
	(setq draft-bufs
	      (sort (mapcar 'buffer-name draft-bufs)
		    (function (lambda (a b)
				(not (string< a b))))))
	(if (setq buf (cdr (member (buffer-name)
				   draft-bufs)))
	    (setq buf (car buf))
	  (setq buf (car draft-bufs)))
	(switch-to-buffer buf))))))

(defun wl-jump-to-draft-folder ()
  (let ((msgs (reverse (elmo-folder-list-messages (wl-draft-get-folder))))
	(mybuf (buffer-name))
	msg buf)
    (if (not msgs)
	(message "No draft message exist.")
      (if (string-match (concat "^" wl-draft-folder "/") mybuf)
	  (setq msg (cadr (memq
			   (string-to-number (substring mybuf (match-end 0)))
			   msgs))))
      (or msg (setq msg (car msgs)))
      (if (setq buf (get-buffer (format "%s/%d" wl-draft-folder msg)))
	  (switch-to-buffer buf)
	(wl-draft-reedit msg)))))

(defun wl-draft-highlight ()
  (when wl-highlight-body-too
    (let ((modified (buffer-modified-p))
	  wl-draft-idle-highlight)
      (unwind-protect
	  (progn
	    (put-text-property (point-min) (point-max) 'face nil)
	    (wl-highlight-message (point-min) (point-max) t))
	(set-buffer-modified-p modified)))))

(defun wl-draft-highlight-and-recenter (&optional n)
  (interactive "P")
  (wl-draft-highlight)
  (static-when (featurep 'xemacs)
    ;; Cope with one of many XEmacs bugs that `recenter' takes
    ;; a long time if there are a lot of invisible text lines.
    (redraw-frame))
  (recenter n))

;; insert element from history
(defvar wl-draft-current-history-position nil)
(defvar wl-draft-history-backup-word "")

(defun wl-draft-previous-history-element (n)
  (interactive "p")
  (let (bol history beg end prev new)
    (when (and (not (wl-draft-on-field-p))
	       (< (point)
		  (save-excursion
		    (goto-char (point-min))
		    (search-forward (concat "\n" mail-header-separator "\n") nil 0)
		    (point)))
	       (save-excursion
		 (beginning-of-line)
		 (while (and (looking-at "^[ \t]")
			     (not (= (point) (point-min))))
		   (forward-line -1))
		 (cond
		  ((looking-at wl-folder-complete-header-regexp)
		   (and (boundp 'wl-read-folder-history)
			(setq history wl-read-folder-history)))
;;;		  ((looking-at wl-address-complete-header-regexp)
;;;		   (setq history .....))
		  (t
		   nil)))
	       (eolp))
      (setq bol (save-excursion (beginning-of-line) (point)))
      (cond ((and (or (eq last-command 'wl-draft-previous-history-element)
		      (eq last-command 'wl-draft-next-history-element))
		  wl-draft-current-history-position)
	     (setq end (point))
	     (or (search-backward-regexp ",[ \t]*\\(.*\\)" bol t)
		 (search-backward-regexp "^[ \t]\\(.*\\)" bol t)
		 (search-backward-regexp "^[^ \t]*: \\(.*\\)" bol t))
	     (setq prev (match-string 1))
	     (goto-char (match-beginning 1))
	     (setq beg (point))
	     (if (cond ((< n 0)
			(>= (+ n wl-draft-current-history-position) 0))
		       ((> n 0)
			(<= (+ n wl-draft-current-history-position)
			    (length history))))
		 (progn
		   (setq wl-draft-current-history-position
			 (+ n wl-draft-current-history-position))
		   (setq new
			 (nth wl-draft-current-history-position
			      (append (list wl-draft-history-backup-word)
				      history)))
		   (delete-region beg end)
		   (insert new))
	       (goto-char end)
	       (cond ((< n 0)
		      (message "End of history; no next item"))
		     ((> n 0)
		      (message "Beginning of history; no preceding item")))))
	    ((and (> n 0)
		  (save-excursion
		    (or (search-backward-regexp ",[ \t]*\\(.*\\)" bol t)
			(search-backward-regexp "^[ \t]\\(.*\\)" bol t)
			(search-backward-regexp "^[^ \t]*: \\(.*\\)" bol t)))
		  (car history))
	     (setq wl-draft-current-history-position 1)
	     (setq wl-draft-history-backup-word (match-string 1))
	     (delete-region (match-beginning 1) (match-end 1))
	     (insert (car history)))
	    (t
	     (setq wl-draft-current-history-position nil))))))

(defun wl-draft-next-history-element (n)
  (interactive "p")
  (wl-draft-previous-history-element (- n)))

;;;; user-agent support by Sen Nagata

;; this appears to be necessarily global...
(defvar wl-user-agent-compose-p nil)
(defvar wl-user-agent-headers-and-body-alist nil)

;; this should be a generic function for mail-mode -- i wish there was
;; something like it in sendmail.el
(defun wl-user-agent-insert-header (header-name header-value)
  "Insert HEADER-NAME w/ value HEADER-VALUE into a message."
  ;; it seems like overriding existing headers is acceptable -- should
  ;; we provide an option?

  ;; plan was: unfold header (might be folded), remove existing value, insert
  ;;           new value
  ;; wl doesn't seem to fold header lines yet anyway :-)

  (let ((kill-whole-line t)
	end-of-line)
    (mail-position-on-field (capitalize header-name))
    (setq end-of-line (point))
    (beginning-of-line)
    (re-search-forward ":" end-of-line)
    (insert (concat " " header-value "\n"))
    (kill-line)))

;; this should be a generic function for mail-mode -- i wish there was
;; something like it in sendmail.el
;;
;; ** haven't dealt w/ case where the body is already set **
(defun wl-user-agent-insert-body (body-text)
  "Insert a body of text, BODY-TEXT, into a message."
  ;; code defensively... :-P
  (goto-char (point-min))
  (search-forward mail-header-separator)
  (forward-line)
  (insert body-text)
  (or (bolp) (insert "\n")))

;;;###autoload
(defun wl-user-agent-compose (&optional to subject other-headers continue
					switch-function yank-action
					send-actions return-action)
  "Support the `compose-mail' interface for wl.
Only support for TO, SUBJECT, and OTHER-HEADERS has been implemented.
Support for CONTINUE, YANK-ACTION, SEND-ACTIONS and RETURN-ACTION has not
been implemented yet.  Partial support for SWITCH-FUNCTION now supported."

  (unless (featurep 'wl)
    (require 'wl))
  (or switch-function
      (setq switch-function 'keep))
  ;; protect these -- to and subject get bound at some point, so it looks
  ;; to be necessary to protect the values used w/in
  (let ((wl-user-agent-headers-and-body-alist other-headers)
	(wl-draft-use-frame (eq switch-function 'switch-to-buffer-other-frame))
	(wl-draft-buffer-style switch-function)
	tem)
    (if to
	(if (setq tem (wl-string-match-assoc
		       "\\`to\\'"
		       wl-user-agent-headers-and-body-alist
		       'ignore-case))
	    (setcdr tem to)
	  (setq wl-user-agent-headers-and-body-alist
		(cons (cons "to" to)
		      wl-user-agent-headers-and-body-alist))))
    (if subject
	(if (setq tem (wl-string-match-assoc
		       "\\`subject\\'"
		       wl-user-agent-headers-and-body-alist
		       'ignore-case))
	    (setcdr tem subject)
	  (setq wl-user-agent-headers-and-body-alist
		(cons (cons "subject" subject)
		      wl-user-agent-headers-and-body-alist))))
    ;; i think this is what we want to use...
    (unwind-protect
	(progn
	  ;; tell the hook-function to do its stuff
	  (setq wl-user-agent-compose-p t)
	  ;; because to get the hooks working, wl-draft has to think it has
	  ;; been called interactively
	  (call-interactively 'wl-draft))
      (setq wl-user-agent-compose-p nil))))

(defun wl-user-agent-compose-internal ()
  "Manipulate headers and/or a body of a draft message."
  ;; being called from wl-user-agent-compose?
  (if wl-user-agent-compose-p
      (progn
	;; insert headers
	(let ((headers wl-user-agent-headers-and-body-alist)
	      (case-fold-search t))
	  (while headers
	    ;; skip body
	    (if (not (string-match "^body$" (car (car headers))))
		(wl-user-agent-insert-header
		 (car (car headers)) (cdr (car headers)))
	      t)
	    (setq headers (cdr headers))))
	;; highlight headers (from wl-draft in wl-draft.el)
        (unless wl-draft-jit-highlight
          (wl-highlight-headers 'for-draft))
	;; insert body
	(let ((body (wl-string-match-assoc "\\`body\\'"
					   wl-user-agent-headers-and-body-alist
					   'ignore-case)))
	  (if body
	      (wl-user-agent-insert-body (cdr body)))))
    t))

(defun wl-draft-setup-parent-flag (flag)
  "Setup a FLAG for parent message."
  (when (and (> (length wl-draft-parent-folder) 0)
	     wl-draft-parent-number)
    (setq wl-draft-parent-flag flag)
    (wl-draft-config-info-operation wl-draft-buffer-message-number 'save)))

(defun wl-draft-buffer-change-number (old-number new-number)
  (when (eq wl-draft-buffer-message-number old-number)
    (setq wl-draft-buffer-message-number new-number)
    (rename-buffer (format "%s/%d" (if (memq 'modeline wl-use-folder-petname)
				       (wl-folder-get-petname wl-draft-folder)
				     wl-draft-folder) new-number) t)
    (setq buffer-file-name (buffer-name))
    (set-buffer-modified-p nil)))

(defun wl-draft-rename-saved-config (old-number new-number)
  (let* ((msgdb-dir (elmo-folder-msgdb-path (wl-draft-get-folder)))
	 (old-name (wl-draft-config-info-filename old-number msgdb-dir))
	 (new-name (wl-draft-config-info-filename new-number msgdb-dir)))
    (when (file-exists-p old-name)
      (rename-file old-name new-name 'ok-if-already-exists))))

;; Real-time draft highlighting
(defcustom wl-draft-jit-highlight (featurep 'jit-lock)
  "When non-nil, enable real-time highlighting using jit-lock-mode.
This only works in Emacs 21 and later."
  :type 'boolean
  :group 'wl-draft)

(defcustom wl-draft-jit-highlight-function 'wl-draft-default-jit-highlight
  "The function used for real-time highlighting using jit-lock-mode."
  :type 'function
  :group 'wl-draft)

(defcustom wl-draft-idle-highlight t
  "When non-nil, enable real-time highlighting using a timer.
This is ignored when wl-draft-jit-highlight is set."
  :type 'boolean
  :group 'wl-draft)

(defcustom wl-draft-idle-highlight-idle-time 0.5
  "Do real-time highlighting after indicated idle time (second)."
  :type 'number
  :group 'wl-draft)

(defcustom wl-draft-idle-highlight-function 'wl-draft-default-idle-highlight
  "The function for real-time highlighting using a timer."
  :type 'function
  :group 'wl-draft)

(defun wl-draft-default-jit-highlight (start end)
  (goto-char start)
  (let ((in-header (wl-draft-point-in-header-p)))
    ;; check for multi-line header, extend region if necessary
    (when in-header
      (while (and (> start (point-min)) (looking-at "^[ \t]"))
        (forward-line -1))
      (setq start (point)))
    ;; check for multi-line attribution
    (when (not in-header)
      (forward-line -1)
      (when (looking-at wl-highlight-citation-prefix-regexp)
        (setq start (point))))
    ;; check for signature
    (let ((hack-sig
           (cond
             ((= end (point-max)) t)
             ((< end (- (point-max) wl-max-signature-size)) nil)
             (t
              (let ((sig (funcall wl-highlight-signature-search-function
                                  (- (point-max) wl-max-signature-size)
                                  (point-max))))
                (cond
                  ((>= start sig) (setq start sig end (point-max)) t)
                  ((>= end sig) (setq end (point-max)) t)
                  (t nil)))))))
      (put-text-property start end 'face nil)
      (wl-highlight-message start end hack-sig (not in-header)))))

(defvar wl-draft-idle-highlight-timer nil)

(defun wl-draft-idle-highlight (&optional state)
  "Toggle real-time highlighting.
If STATE is positive, enable real-time highlighting, and disable it otherwise.  When called non-interactively, enable it if STATE is omitted or nil, and toggle it if STATE is `toggle'."
  (interactive (if current-prefix-arg "P" '(toggle)))
  (setq wl-draft-idle-highlight
	(if (eq state 'toggle)
	    (null wl-draft-idle-highlight)
	  (> (prefix-numeric-value state) 0)))
  (when (interactive-p)
    (message "Real-time highlighting is %sabled"
	     (if wl-draft-idle-highlight "en" "dis")))
  wl-draft-idle-highlight)

(defun wl-draft-default-idle-highlight ()
  (save-match-data (wl-draft-highlight)))

(defun wl-draft-idle-highlight-timer (buffer)
  (when (and (not wl-draft-jit-highlight) wl-draft-idle-highlight
	     (buffer-live-p buffer))
    (with-current-buffer buffer
      (funcall wl-draft-idle-highlight-function))))

(defun wl-draft-idle-highlight-set-timer (beg end len)
  (when (and (not wl-draft-jit-highlight) wl-draft-idle-highlight)
    (require 'timer)
    (when (timerp wl-draft-idle-highlight-timer)
      (cancel-timer wl-draft-idle-highlight-timer))
    (setq wl-draft-idle-highlight-timer
	  (run-with-idle-timer
	   wl-draft-idle-highlight-idle-time nil
	   'wl-draft-idle-highlight-timer (current-buffer)))))

(require 'product)
(product-provide (provide 'wl-draft) (require 'wl-version))

;;; wl-draft.el ends here
