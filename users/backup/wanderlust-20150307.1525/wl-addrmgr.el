;;; wl-addrmgr.el --- Address manager for Wanderlust.

;; Copyright (C) 2001 Kitamoto Tsuyoshi <tsuyoshi.kitamoto@city.sapporo.jp>
;; Copyright (C) 2001 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Kitamoto Tsuyoshi <tsuyoshi.kitamoto@city.sapporo.jp>
;;         Yuuichi Teranishi <teranisi@gohome.org>
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
;;   Edit To:, Cc:, Bcc: fields interactively from E-Mail address list
;;   on ~/.address file.

;;; Code:
;;

(require 'wl-address)
(require 'wl-draft)
(eval-when-compile (require 'cl))

;; Variables
(defgroup wl-addrmgr nil
  "Wanderlust Address manager."
  :prefix "wl-"
  :group 'wl)

(defcustom wl-addrmgr-buffer-lines 10
  "*Buffer lines for ADDRMGR buffer for draft."
  :type 'integer
  :group 'wl-addrmgr)

(defcustom wl-addrmgr-default-sort-key 'realname
  "Default element for sort."
  :type '(choice '(address realname petname none))
  :group 'wl-addrmgr)

(defcustom wl-addrmgr-default-sort-order 'ascending
  "Default element for sort."
  :type '(choice '(ascending descending))
  :group 'wl-addrmgr)

(defcustom wl-addrmgr-realname-width 17
  "Width for realname."
  :type 'integer
  :group 'wl-addrmgr)

(defcustom wl-addrmgr-petname-width 10
  "Width for petname."
  :type 'integer
  :group 'wl-addrmgr)

(defcustom wl-addrmgr-line-width 78
  "Width for each line."
  :type 'integer
  :group 'wl-addrmgr)

(defcustom wl-addrmgr-realname-face 'wl-highlight-summary-normal-face
  "Face for realname."
  :type 'face
  :group 'wl-addrmgr)

(defcustom wl-addrmgr-petname-face 'wl-highlight-summary-unread-face
  "Face for petname."
  :type 'face
  :group 'wl-addrmgr)

(defcustom wl-addrmgr-address-face 'wl-highlight-summary-new-face
  "Face for address."
  :type 'face
  :group 'wl-addrmgr)

(defcustom wl-addrmgr-default-method 'local
  "Default access method for address entries."
  :type 'symbol
  :group 'wl-addrmgr)

(defvar wl-addrmgr-buffer-name "Address")
(defvar wl-addrmgr-mode-map nil)
(defvar wl-addrmgr-method-list '(local))

;; buffer local variable.
(defvar wl-addrmgr-draft-buffer nil)
(defvar wl-addrmgr-unknown-list nil)
(defvar wl-addrmgr-sort-key nil)
(defvar wl-addrmgr-sort-order nil)
(defvar wl-addrmgr-method nil)
(defvar wl-addrmgr-list nil)
(defvar wl-addrmgr-method-name nil)

(make-variable-buffer-local 'wl-addrmgr-draft-buffer)
(make-variable-buffer-local 'wl-addrmgr-unknown-list)
(make-variable-buffer-local 'wl-addrmgr-sort-key)
(make-variable-buffer-local 'wl-addrmgr-sort-order)
(make-variable-buffer-local 'wl-addrmgr-method)
(make-variable-buffer-local 'wl-addrmgr-list)
(make-variable-buffer-local 'wl-addrmgr-method-name)

;;; Code

(if wl-addrmgr-mode-map
    nil
  (setq wl-addrmgr-mode-map (make-sparse-keymap))
  (define-key wl-addrmgr-mode-map "<"    'wl-addrmgr-goto-top)
  (define-key wl-addrmgr-mode-map ">"    'wl-addrmgr-goto-bottom)
  (define-key wl-addrmgr-mode-map "t"    'wl-addrmgr-mark-set-to)
  (define-key wl-addrmgr-mode-map "b"    'wl-addrmgr-mark-set-bcc)
  (define-key wl-addrmgr-mode-map "c"    'wl-addrmgr-mark-set-cc)
  (define-key wl-addrmgr-mode-map "u"    'wl-addrmgr-unmark)
  (define-key wl-addrmgr-mode-map "x"    'wl-addrmgr-apply)

  (define-key wl-addrmgr-mode-map "\C-c\C-c" 'wl-addrmgr-apply)

  (define-key wl-addrmgr-mode-map "n"    'wl-addrmgr-next)
  (define-key wl-addrmgr-mode-map "j"    'wl-addrmgr-next)
  (define-key wl-addrmgr-mode-map "k"    'wl-addrmgr-prev)
  (define-key wl-addrmgr-mode-map "p"    'wl-addrmgr-prev)
  (define-key wl-addrmgr-mode-map [down] 'wl-addrmgr-next)
  (define-key wl-addrmgr-mode-map [up]   'wl-addrmgr-prev)

  (define-key wl-addrmgr-mode-map "s"    'wl-addrmgr-sort)

  (define-key wl-addrmgr-mode-map "a"    'wl-addrmgr-add)
  (define-key wl-addrmgr-mode-map "d"    'wl-addrmgr-delete)
  (define-key wl-addrmgr-mode-map "e"    'wl-addrmgr-edit)
  (define-key wl-addrmgr-mode-map "\n"    'wl-addrmgr-edit)
  (define-key wl-addrmgr-mode-map "\r"    'wl-addrmgr-edit)

  (define-key wl-addrmgr-mode-map "q"    'wl-addrmgr-quit)
  (define-key wl-addrmgr-mode-map "\C-c\C-k" 'wl-addrmgr-quit)

  (define-key wl-addrmgr-mode-map "C"    'wl-addrmgr-change-method)

  (define-key wl-addrmgr-mode-map "Z"    'wl-addrmgr-reload)
  (define-key wl-addrmgr-mode-map "\C-c\C-l" 'wl-addrmgr-redraw))

(defun wl-addrmgr-mode ()
  "Major mode for Wanderlust address management.
See info under Wanderlust for full documentation.

\\{wl-addrmgr-mode-map}"
  (kill-all-local-variables)
  (setq mode-name "Address"
	major-mode 'wl-addrmgr-mode)
  (wl-mode-line-buffer-identification
   '("Wanderlust: Address (" wl-addrmgr-method-name ")"))
  (use-local-map wl-addrmgr-mode-map)
  (when (boundp 'bidi-paragraph-direction)
    (set 'bidi-paragraph-direction 'left-to-right))
  (setq buffer-read-only t))

(defun wl-addrmgr-address-entry-list (field)
  "Return address list."
  (mapcar
   (lambda (addr)
     (nth 1 (std11-extract-address-components addr)))
   (wl-parse-addresses
    (mapconcat
     'identity
     (elmo-multiple-fields-body-list (list field) mail-header-separator)
     ","))))

(defun wl-addrmgr-pickup-entry-list (buffer)
  "Return a list of address entiry from BUFFER."
  (when buffer
    (with-current-buffer buffer
      (mapcar
       (lambda (addr)
	 (let ((structure (std11-extract-address-components addr)))
	   (list (cadr structure)
		 (or (car structure) "")
		 (or (car structure) ""))))
       (wl-parse-addresses
	(mapconcat
	 'identity
	 (elmo-multiple-fields-body-list '("to" "cc" "bcc")
					 mail-header-separator)
	 ","))))))

(defun wl-addrmgr-merge-entries (base-list append-list)
  "Return a merged list of address entiry."
  (dolist (entry append-list)
    (unless (assoc (car entry) base-list)
      (setq base-list (nconc base-list (list entry)))))
  base-list)

;;;###autoload
(defun wl-addrmgr ()
  "Start an Address manager."
  (interactive)
  (let ((buffer (if (eq major-mode 'wl-draft-mode) (current-buffer)))
	(already-list (list (cons 'to (wl-addrmgr-address-entry-list "to"))
			    (cons 'cc (wl-addrmgr-address-entry-list "cc"))
			    (cons 'bcc (wl-addrmgr-address-entry-list "bcc")))))
    (if (eq major-mode 'wl-draft-mode)
	(if (get-buffer-window wl-addrmgr-buffer-name)
	    nil
	  (split-window (selected-window)
			(- (window-height (selected-window))
			   wl-addrmgr-buffer-lines))
	  (select-window (next-window))
	  ;;  Non-nil means display-buffer should make new windows.
	  (let ((pop-up-windows nil))
	    (switch-to-buffer
	     (get-buffer-create wl-addrmgr-buffer-name))))
      (switch-to-buffer (get-buffer-create wl-addrmgr-buffer-name)))
    (set-buffer wl-addrmgr-buffer-name)
    (wl-addrmgr-mode)
    (unless wl-addrmgr-method
      (setq wl-addrmgr-method wl-addrmgr-default-method
	    wl-addrmgr-method-name (symbol-name wl-addrmgr-default-method)))
    (unless wl-addrmgr-sort-key
      (setq wl-addrmgr-sort-key wl-addrmgr-default-sort-key))
    (unless wl-addrmgr-sort-order
      (setq wl-addrmgr-sort-order wl-addrmgr-default-sort-order))
    (setq wl-addrmgr-draft-buffer buffer)
    (setq wl-addrmgr-list
	  (wl-addrmgr-merge-entries (wl-addrmgr-list)
				    (wl-addrmgr-pickup-entry-list buffer)))
    (wl-addrmgr-draw already-list)
    (setq wl-addrmgr-unknown-list already-list)
    (wl-addrmgr-goto-top)))

(defun wl-addrmgr-goto-top ()
  (interactive)
  (goto-char (point-min))
  (forward-line 2)
  (condition-case nil
      (forward-char 4)
    (error)))

(defun wl-addrmgr-goto-bottom ()
  (interactive)
  (goto-char (point-max))
  (beginning-of-line)
  (forward-char 4))

(defun wl-addrmgr-reload ()
  "Reload addresses entries."
  (interactive)
  (setq wl-addrmgr-list (wl-addrmgr-list 'reload))
  (wl-addrmgr-redraw))

(defun wl-addrmgr-redraw ()
  "Redraw addresses entries."
  (interactive)
  (let ((rcpt (wl-addrmgr-mark-check)))
    (wl-addrmgr-draw (list (cons 'to (nth 0 rcpt))
			   (cons 'cc (nth 1 rcpt))
			   (cons 'bcc (nth 2 rcpt)))))
  (wl-addrmgr-goto-top))

(defun wl-addrmgr-sort-list (key list order)
  (let ((pos (case key
	       (address 0)
	       (petname 1)
	       (realname 2)))
	sorted)
    (if pos
	(progn
	  (setq sorted (sort list `(lambda (a b) (string< (nth ,pos a)
							  (nth ,pos b)))))
	  (if (eq order 'descending)
	      (nreverse sorted)
	    sorted))
      list)))

(defun wl-addrmgr-insert-line (entry)
  (let ((real (nth 2 entry))
	(pet  (nth 1 entry))
	(addr (nth 0 entry))
	beg)
    (insert "     ")
    (setq beg (point))
    (setq real (wl-set-string-width wl-addrmgr-realname-width real))
    (put-text-property 0 (length real) 'face
		       wl-addrmgr-realname-face
		       real)
    (setq pet (wl-set-string-width wl-addrmgr-petname-width pet))
    (put-text-property 0 (length pet) 'face
		       wl-addrmgr-petname-face
		       pet)
    (setq addr (copy-sequence addr))
    (put-text-property 0 (length addr) 'face
		       wl-addrmgr-address-face
		       addr)
    (insert
     (wl-set-string-width
      (- wl-addrmgr-line-width 4)
      (concat real " " pet " " addr)))
    (put-text-property beg (point) 'wl-addrmgr-entry entry)))

(defun wl-addrmgr-search-forward-address (address)
  "Search forward from point for ADDRESS.
Return nil if no ADDRESS exists."
  (let ((pos (point)))
    (if (catch 'found
	    (while (not (eobp))
	      (if (string= address (car (wl-addrmgr-address-entry)))
		  (throw 'found t)
		(forward-line))))
	(point)
      (goto-char pos)
      nil)))

(defun wl-addrmgr-draw (already-list)
  "Show recipients mail addresses."
  (save-excursion
    (let ((buffer-read-only nil)
	  list field addrs beg real pet addr)
      (erase-buffer)
      (goto-char (point-min))
      (insert
       "Mark "
       (wl-set-string-width wl-addrmgr-realname-width
			    "Realname")
       " "
       (wl-set-string-width wl-addrmgr-petname-width
			    "Petname")
       " Address\n")
      (insert "---- "
	      (make-string wl-addrmgr-realname-width ?-)
	      " "
	      (make-string wl-addrmgr-petname-width ?-)
	      " ---------------")
      (unless wl-addrmgr-list (insert "\n"))
      (dolist (entry (wl-addrmgr-sort-list wl-addrmgr-sort-key
					   (copy-sequence wl-addrmgr-list)
					   wl-addrmgr-sort-order))
	(insert "\n")
	(wl-addrmgr-insert-line entry))
      (set-buffer-modified-p nil)
      (while already-list
	(setq list (car already-list)
	      field (car list)
	      addrs (cdr list))
	(while addrs
	  (goto-char (point-min))
	  (when (wl-addrmgr-search-forward-address (car addrs))
	    (wl-addrmgr-mark-write field)
	    (setcdr list (delq (car addrs) (cdr list))))
	  (setq addrs (cdr addrs)))
	(setq already-list (cdr already-list))))))

(defun wl-addrmgr-next ()
  "Move cursor next line."
  (interactive)
  (end-of-line)
  (let ((current (count-lines (point-min) (point)))
	first)
    (cond
     ((<= current 2)
      (when (setq first (next-single-property-change (point) 'wl-addrmgr-entry
						     nil))
	(goto-char first)
	(beginning-of-line)
	(forward-char 4)))
     (t
      (forward-line)
      (beginning-of-line)
      (forward-char 4)))))

(defun wl-addrmgr-prev ()
  "Move cursor prev line."
  (interactive)
  (let ((current (count-lines (point-min) (point))))
    (cond
     ((= current 3)
      (beginning-of-line)
      (forward-char 4))
     ((< current 3)
      (goto-char (point-min))
      (forward-line 2)
      (forward-char 4))
     (t
      (forward-line -1)
      (forward-char 4)))))

(defun wl-addrmgr-quit-yes ()
  (let ((draft-buffer wl-addrmgr-draft-buffer))
    (if (and draft-buffer
	     (buffer-live-p draft-buffer)
	     (null (get-buffer-window draft-buffer 'visible)))
	(switch-to-buffer draft-buffer)
      (unless (one-window-p)
	(delete-window)))
    (kill-buffer wl-addrmgr-buffer-name)
    (if (and draft-buffer (not (one-window-p)))
	(switch-to-buffer-other-window draft-buffer))))

(defun wl-addrmgr-quit ()
  "Exit from electric reference mode without inserting reference."
  (interactive)
  (let ((rcpt (wl-addrmgr-mark-check)))
    (if (or (nth 0 rcpt)
	    (nth 1 rcpt)
	    (nth 2 rcpt))
	(when (y-or-n-p "There is marked address. Quit wl-addrmgr really? ")
	  (wl-addrmgr-quit-yes))
      (wl-addrmgr-quit-yes)))
  (message ""))

(defun wl-addrmgr-mark-set-to ()
  "Marking To: sign."
  (interactive)
  (wl-addrmgr-mark-write 'to)
  (wl-addrmgr-next))

(defun wl-addrmgr-mark-set-cc ()
  "Marking Cc: sign."
  (interactive)
  (wl-addrmgr-mark-write 'cc)
  (wl-addrmgr-next))

(defun wl-addrmgr-mark-set-bcc ()
  "Marking Bcc: sign."
  (interactive)
  (wl-addrmgr-mark-write 'bcc)
  (wl-addrmgr-next))

(defun wl-addrmgr-unmark ()
  "Erase Marked sign."
  (interactive)
  (let ((entry (wl-addrmgr-address-entry))
	buffer-read-only)
    (save-excursion
      (delete-region (point-at-bol) (point-at-eol))
      (wl-addrmgr-insert-line entry))
    (set-buffer-modified-p nil)
    (wl-addrmgr-next)))

(defun wl-addrmgr-sort ()
  "Sort address entry."
  (interactive)
  (setq wl-addrmgr-sort-key (intern
			     (completing-read
			      (format "Sort By (%s): "
				      (symbol-name wl-addrmgr-sort-key))
			      '(("address")("realname")("petname")("none"))
			      nil t nil nil
			      (symbol-name wl-addrmgr-sort-key))))
  (if (eq wl-addrmgr-sort-key 'none)
      (wl-addrmgr-reload)
    (setq wl-addrmgr-sort-order (intern
				 (completing-read
				  (format "Sort Order (%s): "
					  (symbol-name wl-addrmgr-sort-order))
				  '(("ascending") ("descending"))
				  nil t nil nil
				  (symbol-name wl-addrmgr-sort-order))))
    (wl-addrmgr-redraw)))

;;; Backend methods.
(defun wl-addrmgr-method-call (method &rest args)
  (apply (intern (concat "wl-addrmgr-"
			 (symbol-name wl-addrmgr-method)
			 "-" (symbol-name method)))
	 args))

(defun wl-addrmgr-change-method ()
  (interactive)
  (setq wl-addrmgr-method (intern
			   (setq wl-addrmgr-method-name
				 (completing-read
				  (format "Method (%s): "
					  (symbol-name wl-addrmgr-method))
				  (mapcar (lambda (method)
					    (list (symbol-name method)))
					  wl-addrmgr-method-list)
				  nil t nil nil
				  (symbol-name wl-addrmgr-method)))))
  (wl-addrmgr-redraw))

(defun wl-addrmgr-list (&optional reload)
  "List address entries."
  (wl-addrmgr-method-call 'list reload))

(defun wl-addrmgr-add ()
  "Add address entry."
  (interactive)
  (let ((entry (wl-addrmgr-method-call 'add)))
    (if (eq wl-addrmgr-sort-key 'none)
	(wl-addrmgr-reload)
      (setq wl-addrmgr-list (cons entry wl-addrmgr-list))
      (wl-addrmgr-redraw))
    (message "Added `%s'." (wl-string (car entry)))))

(defun wl-addrmgr-delete ()
  "Delete address entry."
  (interactive)
  (let ((addr (wl-string (car (wl-addrmgr-address-entry))))
	lines)
    (when (and addr
	       (y-or-n-p (format "Delete '%s'? " addr)))
      (setq lines (count-lines (point-min) (point)))
      (wl-addrmgr-method-call 'delete addr)
      (setq wl-addrmgr-list (delq (assoc addr wl-addrmgr-list)
				  wl-addrmgr-list))
      (wl-addrmgr-redraw)
      (forward-line (- lines 2))
      (message "Deleted `%s'." addr))))

(defun wl-addrmgr-edit ()
  "Edit address entry."
  (interactive)
  (let ((orig (wl-addrmgr-address-entry))
	entry lines)
    (setq entry (wl-addrmgr-method-call 'edit (wl-string (car orig))))
    (setq lines (count-lines (point-min) (point)))
    (if (eq wl-addrmgr-sort-key 'none)
	(wl-addrmgr-reload)
      (setq wl-addrmgr-list (delq (assoc (car orig) wl-addrmgr-list)
				  wl-addrmgr-list)
	    wl-addrmgr-list (cons entry wl-addrmgr-list))
      (wl-addrmgr-redraw))
    (forward-line (- lines 1))
    (message "Modified `%s'." (wl-string (car entry)))))

;;; local address book implementation.
(defun wl-addrmgr-local-list (reload)
  (if (or (null wl-address-list) reload)
      (wl-address-init))
  (copy-sequence wl-address-list))

(defun wl-addrmgr-local-add ()
  (wl-address-add-or-change nil nil 'addr-too))

(defun wl-addrmgr-local-edit (address)
  (wl-address-add-or-change address nil 'addr-too))

(defun wl-addrmgr-local-delete (address)
  (wl-address-delete address))

;;; LDAP implementation (Implement Me)

;;; Operations.

(defun wl-addrmgr-address-entry ()
  (get-text-property (previous-single-property-change
		      (point-at-eol) 'wl-addrmgr-entry nil
		      (point-at-bol))
		     'wl-addrmgr-entry))

(defun wl-addrmgr-mark-write (&optional mark)
  "Set MARK to the current address entry."
  (save-excursion
    (unless (< (count-lines (point-min) (point-at-eol)) 3)
      (let ((buffer-read-only nil) beg end)
	(beginning-of-line)
	(delete-char 4)
	(insert (case mark
		  (to "To: ")
		  (cc "Cc: ")
		  (bcc "Bcc:")
		  (t "    ")))
	(insert (make-string (- 4 (current-column)) (string-to-char " ")))
	(setq beg (point-at-bol))
	(setq end (point-at-eol))
	(put-text-property beg end 'face nil)
	(wl-highlight-message beg end nil))
      (set-buffer-modified-p nil))))

(defun wl-addrmgr-apply ()
  (interactive)
  (let ((rcpt (wl-addrmgr-mark-check 'full)))
    (when (or (or (nth 0 rcpt)
		  (nth 1 rcpt)
		  (nth 2 rcpt))
	      (or (cdr (assq 'to wl-addrmgr-unknown-list))
		  (cdr (assq 'cc wl-addrmgr-unknown-list))
		  (cdr (assq 'bcc wl-addrmgr-unknown-list))))
      (wl-addrmgr-apply-exec (wl-addrmgr-mark-check 'full)))
    (wl-addrmgr-quit-yes)))

(defun wl-addrmgr-mark-check (&optional full)
  "Return list of recipients (TO CC BCC)."
  (save-excursion			; save cursor POINT
    (goto-char (point-min))
    (forward-line 2)
    (let (to-list cc-list bcc-list mark addr realname)
      (while (and (not (eobp))
		  (re-search-forward "^\\([^ ]+:\\) " nil t))
	(setq mark (match-string 1))
	(setq addr (car (wl-addrmgr-address-entry)))
	(setq realname (nth 2 (wl-addrmgr-address-entry)))
	(cond
	 ((string= mark "To:")
	  (setq to-list (cons
			 (if (and full
				  (not (or (string= realname "")
					   (string-match ".*:.*;$" addr))))
			     (concat
			      (wl-address-quote-specials realname)
			      " <" addr">")
			   addr)
			 to-list)))
	 ((string= mark "Cc:")
	  (setq cc-list (cons
			 (if (and full
				  (not (or (string= realname "")
					   (string-match ".*:.*;$" addr))))
			     (concat
			      (wl-address-quote-specials realname)
			      " <" addr">")
			   addr)
			 cc-list)))
	 ((string= mark "Bcc:")
	  (setq bcc-list (cons
			  (if (and full
				   (not (or (string= realname "")
					    (string-match ".*:.*;$" addr))))
			      (concat
			       (wl-address-quote-specials realname)
			       " <" addr">")
			    addr)
			  bcc-list)))))
      (list to-list cc-list bcc-list))))

(defun wl-addrmgr-apply-exec (rcpt)
  (let ((to (nconc (nth 0 rcpt) (cdr (assq 'to wl-addrmgr-unknown-list))))
	(cc (nconc (nth 1 rcpt) (cdr (assq 'cc wl-addrmgr-unknown-list))))
	(bcc (nconc (nth 2 rcpt) (cdr (assq 'bcc wl-addrmgr-unknown-list))))
	from clist)
    (setq clist (list (cons "Bcc" (if bcc (mapconcat 'identity bcc ",\n\t")))
		      (cons "Cc" (if cc (mapconcat 'identity cc ",\n\t")))
		      (cons "To" (if to (mapconcat 'identity to ",\n\t")))))
    (when (or (null wl-addrmgr-draft-buffer)
	      (not (buffer-live-p wl-addrmgr-draft-buffer)))
      (setq wl-addrmgr-draft-buffer (save-window-excursion
				      (call-interactively 'wl-draft)
				      (current-buffer))))
    (with-current-buffer wl-addrmgr-draft-buffer
      (setq from (std11-field-body "From"))
      (if from
	  (setq clist (append clist (list (cons "From" from)))))
      (wl-addrmgr-mark-exec-sub clist))))

(defun wl-addrmgr-replace-field (field content)
  "Insert FIELD with CONTENT to the top of the header fields."
  (save-excursion
    (save-restriction
      (let ((case-fold-search t)
	    (inhibit-read-only t) ;; added by teranisi.
	    beg)
	(std11-narrow-to-header mail-header-separator)
	(goto-char (point-min))
	(while (re-search-forward (concat "^" (regexp-quote field) ":") nil t)
	  ;; delete field
	  (progn
	    (setq beg (point-at-bol))
	    (re-search-forward "^[^ \t]" nil 'move)
	    (delete-region beg (point-at-bol))
	    (beginning-of-line)))
	(when content
	  ;; add field to top.
	  (goto-char (point-min))
	  (insert (concat field ": " content "\n")))))))

(defun wl-addrmgr-mark-exec-sub (list)
  (dolist (pair list)
    (wl-addrmgr-replace-field (car pair) (cdr pair)))
  ;; from wl-template.el
  ;; rehighlight
  (if wl-highlight-body-too
      (let ((beg (point-min))
	    (end (point-max)))
	(put-text-property beg end 'face nil)
	(wl-highlight-message beg end t))))

(require 'product)
(product-provide (provide 'wl-addrmgr) (require 'wl-version))

;;; wl-addrmgr.el ends here
