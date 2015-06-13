;;; wl.el --- Wanderlust bootstrap.

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

(require 'std11)
(require 'elmo)
(require 'wl-version)			; reduce recursive-load-depth

;; from x-face.el
(unless (and (fboundp 'defgroup)
	     (fboundp 'defcustom))
  (require 'backquote)
  (defmacro defgroup (&rest args))
  (defmacro defcustom (symbol value &optional doc &rest args)
    (let ((doc (concat "*" (or doc ""))))
      `(defvar ,symbol ,value ,doc))))

(require 'wl-vars)
(require 'wl-util)

(cond (wl-on-xemacs
       (require 'wl-xmas))
      (wl-on-emacs21
       (require 'wl-e21))
      (t
       (require 'wl-mule)))

(provide 'wl)				; circular dependency
(require 'wl-folder)
(require 'wl-summary)
(require 'wl-action)
(require 'wl-thread)
(require 'wl-address)
(require 'wl-news nil t)

(wl-draft-mode-setup)
(require 'wl-draft)
(wl-draft-key-setup)

(require 'wl-demo)
(require 'wl-highlight)

(eval-when-compile
  (require 'cl)
  (require 'smtp)
  (require 'wl-score)
  (require 'wl-fldmgr)
  (require 'wl-mime)
  (require 'wl-spam))

(defun wl-plugged-init (&optional make-alist)
  (setq elmo-plugged wl-plugged)
  (if wl-reset-plugged-alist
      (elmo-set-plugged elmo-plugged))
  (when make-alist
    (wl-make-plugged-alist))
  ;; Plug status.
  (setq elmo-plugged (setq wl-plugged (elmo-plugged-p))
	wl-modeline-plug-status wl-plugged)
  (if wl-plugged
      (wl-toggle-plugged t 'flush)))

(defun wl-toggle-plugged (&optional arg queue-flush-only)
  (interactive)
  (elmo-quit) ; Disconnect current connection.
  (unless queue-flush-only
    (cond
     ((eq arg 'on)
      (setq wl-plugged t))
     ((eq arg 'off)
      (setq wl-plugged nil))
     (t (setq wl-plugged (not wl-plugged))))
    (elmo-set-plugged wl-plugged))
  (setq elmo-plugged wl-plugged
	wl-modeline-plug-status wl-plugged)
  (save-excursion
    (let ((summaries (wl-collect-summary)))
      (while summaries
	(set-buffer (pop summaries))
	(wl-summary-save-view)
	(elmo-folder-commit wl-summary-buffer-elmo-folder))))
  (setq wl-biff-check-folders-running nil)
  (if wl-plugged
      (progn
	;; flush queue!!
	(elmo-dop-queue-flush)
	(unless queue-flush-only
	  (when wl-biff-check-folder-list
	    (wl-biff-check-folders)
	    (wl-biff-start)))
	(if (and wl-draft-enable-queuing
		 wl-auto-flush-queue)
	    (wl-draft-queue-flush))
;;; 	(when (and (eq major-mode 'wl-summary-mode)
;;; 		   (elmo-folder-plugged-p wl-summary-buffer-elmo-folder))
;;; 	  (let* ((msgdb-dir (elmo-folder-msgdb-path
;;; 			     wl-summary-buffer-elmo-folder))
;;; 		 (seen-list (elmo-msgdb-seen-load msgdb-dir)))
;;;	    (setq seen-list
;;; 		  (wl-summary-flush-pending-append-operations seen-list))
;;; 	    (elmo-msgdb-seen-save msgdb-dir seen-list)))
	(run-hooks 'wl-plugged-hook))
    (wl-biff-stop)
    (run-hooks 'wl-unplugged-hook))
  (force-mode-line-update t))

;;; wl-plugged-mode

(defvar wl-plugged-port-label-alist
  (list (cons 119 "nntp")
	(cons 143 "imap4")
	(cons 110 "pop3")
	(cons 25 "smtp")))
	;;(cons elmo-pop-before-smtp-port "pop3")

(defconst wl-plugged-switch-variables
  '(("Queuing" . wl-draft-enable-queuing)
    ("AutoFlushQueue" . wl-auto-flush-queue)
    ("DisconnectedOperation" . elmo-enable-disconnected-operation)))

(defvar wl-plugged-buf-name "Plugged")
(defvar wl-plugged-mode-map nil)
(defvar wl-plugged-alist nil)
(defvar wl-plugged-switch nil)
(defvar wl-plugged-winconf nil)
(defvar wl-plugged-sending-queue-alist nil)
(defvar wl-plugged-dop-queue-alist nil)
(defvar wl-plugged-alist-modified nil)

(defvar wl-plugged-mode-menu-spec
  '("Plugged"
    ["Toggle plugged" wl-plugged-toggle t]
    ["Toggle All plugged" wl-plugged-toggle-all t]
    ["Prev Port"      wl-plugged-move-to-previous t]
    ["Next Port"      wl-plugged-move-to-next t]
    ["Prev Server"    wl-plugged-move-to-previous-server t]
    ["Next Server"    wl-plugged-move-to-next-server t]
    ["Flush queue"    wl-plugged-flush-queue t]
    "----"
    ["Exit"           wl-plugged-exit t]))

(eval-and-compile
  (if wl-on-xemacs
      (defun wl-plugged-setup-mouse ()
	(define-key wl-plugged-mode-map 'button2 'wl-plugged-click))
    (defun wl-plugged-setup-mouse ()
      (define-key wl-plugged-mode-map [mouse-2] 'wl-plugged-click))))

(unless wl-plugged-mode-map
  (setq wl-plugged-mode-map (make-sparse-keymap))
  (define-key wl-plugged-mode-map " "    'wl-plugged-toggle)
  (define-key wl-plugged-mode-map "\C-m" 'wl-plugged-toggle)
  (define-key wl-plugged-mode-map "\M-t" 'wl-plugged-toggle-all)
  (define-key wl-plugged-mode-map "q"    'wl-plugged-exit)
  (define-key wl-plugged-mode-map "\C-t" 'wl-plugged-exit)
  (define-key wl-plugged-mode-map "F"    'wl-plugged-flush-queue)
  (define-key wl-plugged-mode-map "P"    'wl-plugged-move-to-previous-server)
  (define-key wl-plugged-mode-map "N"    'wl-plugged-move-to-next-server)
  (define-key wl-plugged-mode-map "p"    'wl-plugged-move-to-previous)
  (define-key wl-plugged-mode-map "n"    'wl-plugged-move-to-next)
  (define-key wl-plugged-mode-map "\e\t" 'wl-plugged-move-to-previous)
  (define-key wl-plugged-mode-map "\t"   'wl-plugged-move-to-next)
  (wl-plugged-setup-mouse)
  (easy-menu-define
   wl-plugged-mode-menu
   wl-plugged-mode-map
   "Menu used in Plugged mode."
   wl-plugged-mode-menu-spec))

(defun wl-plugged-mode ()
  "Mode for setting Wanderlust plugged.
See info under Wanderlust for full documentation.

Special commands:
\\{wl-plugged-mode-map}

Entering Plugged mode calls the value of `wl-plugged-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map wl-plugged-mode-map)
  (setq major-mode 'wl-plugged-mode)
  (setq mode-name "Plugged")
  (easy-menu-add wl-plugged-mode-menu)
  (wl-mode-line-buffer-identification)
  (setq wl-plugged-switch wl-plugged)
  (setq wl-plugged-alist-modified nil)
  (setq buffer-read-only t)
  (run-hooks 'wl-plugged-mode-hook))

(defun wl-plugged-string (plugged &optional time)
  (if time
      wl-plugged-auto-off
    (if plugged
	wl-plugged-plug-on
      wl-plugged-plug-off)))

(defun wl-plugged-server-indent ()
  (make-string wl-plugged-server-indent (string-to-char " ")))

(defun wl-plugged-set-variables ()
  (setq wl-plugged-sending-queue-alist
	(wl-plugged-sending-queue-info))
  (setq wl-plugged-dop-queue-alist
	(wl-plugged-dop-queue-info))
  (setq wl-plugged-alist
	(sort (copy-sequence elmo-plugged-alist)
	      (lambda (a b)
		(string< (caar a) (caar b))))))

(defun wl-plugged-sending-queue-info ()
  ;; sending queue status
  (let (alist msgs sent-via server port)
    (setq msgs (elmo-folder-list-messages
		(wl-folder-get-elmo-folder wl-queue-folder)))
    (while msgs
      (setq sent-via (wl-draft-queue-info-operation (car msgs) 'get-sent-via))
      (while sent-via
	(when (eq (nth 1 (car sent-via)) 'unplugged)
	  (setq server (car (nth 2 (car sent-via)))
		port (cdr (nth 2 (car sent-via))))
	  (elmo-plugged-p server port)	;; add elmo-plugged-alist if nothing.
	  (setq alist
		(wl-append-assoc-list
		 (cons server port)
		 (car msgs)
		 alist)))
	(setq sent-via (cdr sent-via)))
      (setq msgs (cdr msgs)))
    alist))

(defun wl-plugged-sending-queue-status (qinfo)
  ;; sending queue status
  (let ((len (length (cdr qinfo))))
    (concat (wl-plugged-set-folder-icon
	     wl-queue-folder
	     (wl-folder-get-petname wl-queue-folder))
	    (if (> len 1)
		(format ": %d msgs (" len)
	      (format ": %d msg (" len))
	    (mapconcat (function number-to-string) (cdr qinfo) ",")
	    ")")))

(defun wl-plugged-dop-queue-info ()
  ;; dop queue status
  (let* ((count 0)
	 (elmo-dop-queue (copy-sequence elmo-dop-queue))
	 dop-queue last alist server-info
	 ope operation)
;;;    (elmo-dop-queue-load)
    (elmo-dop-queue-merge)
    (setq dop-queue (sort elmo-dop-queue (lambda (a b)
					   (string< (elmo-dop-queue-fname a)
						    (elmo-dop-queue-fname b)))))
    (wl-append dop-queue (list nil)) ;; terminate(dummy)
    (when (car dop-queue)
      (setq last (elmo-dop-queue-fname (car dop-queue)))) ;; first
    (while dop-queue
      (when (car dop-queue)
	(setq ope (cons (elmo-dop-queue-method-name (car dop-queue))
			(length
			 (if (listp
			      (car
			       (elmo-dop-queue-arguments (car dop-queue))))
			     (car (elmo-dop-queue-arguments
				   (car dop-queue))))))))
      (if (and (car dop-queue)
	       (string= last (elmo-dop-queue-fname (car dop-queue))))
	  (wl-append operation (list ope))
;;;	(setq count (1+ count))
	(when (and last (setq server-info (elmo-net-port-info
					   (wl-folder-get-elmo-folder last))))
	  (setq alist
		(wl-append-assoc-list
		 server-info
		 (cons last operation)
		 alist)))
	(when (car dop-queue)
	  (setq last (elmo-dop-queue-fname (car dop-queue))
		operation (list ope))))
      (setq dop-queue (cdr dop-queue)))
    alist))

(defun wl-plugged-dop-queue-status (qinfo &optional column)
  ;; dop queue status
  (let ((operations (cdr qinfo))
	(column (or column wl-plugged-queue-status-column)))
    (mapconcat
     (lambda (folder-ope)
       (concat (wl-plugged-set-folder-icon
		(car folder-ope)
		(wl-folder-get-petname (car folder-ope)))
	       "("
	       (let ((opes (cdr folder-ope))
		     pair shrinked)
		 (while opes
		   (if (setq pair (assoc (car (car opes)) shrinked))
		       (setcdr pair (+ (cdr pair)
				       (max (cdr (car opes)) 1)))
		     (setq shrinked (cons
				     (cons (car (car opes))
					   (max (cdr (car opes)) 1))
				     shrinked)))
		   (setq opes (cdr opes)))
		 (mapconcat
		  (lambda (ope)
		    (if (> (cdr ope) 0)
			(format "%s:%d" (car ope) (cdr ope))
		      (format "%s" (car ope))))
		  (nreverse shrinked) ","))
	       ")"))
     operations
     (concat "\n" (wl-set-string-width column "")))))

(defun wl-plugged-drawing (plugged-alist)
  (let ((buffer-read-only nil)
	(alist plugged-alist)
	(vars wl-plugged-switch-variables)
	last server port stream-type label plugged time
	line len qinfo column)
    (erase-buffer)
    (while vars
      (insert (format "%s:[%s]%s"
		      (caar vars)
		      (wl-plugged-string (symbol-value (cdar vars)))
		      (if (cdr vars) " " "")))
      (setq vars (cdr vars)))
    (insert "\n")
    (let ((elmo-plugged wl-plugged-switch))
      (setq line (format "[%s](wl-plugged)"
			 (wl-plugged-string (elmo-plugged-p))))
      ;; sending queue status
      (when (setq qinfo (assoc (cons nil nil) wl-plugged-sending-queue-alist))
	(setq line (concat
		    (wl-set-string-width wl-plugged-queue-status-column line)
		    (wl-plugged-sending-queue-status qinfo))))
      (insert line "\n"))
    (while alist
      (setq server (nth 0 (caar alist))
	    port (nth 1 (caar alist))
	    stream-type (nth 2 (caar alist))
	    label (nth 1 (car alist))
	    plugged (nth 2 (car alist))
	    time (nth 3 (car alist)))
      (unless (string= last server)
	;; server plug
	(insert (format "%s[%s]%s\n"
			(wl-plugged-server-indent)
			(wl-plugged-string
			 (elmo-plugged-p server nil plugged-alist))
			server))
	(setq last server))
      ;; port plug
      (setq line
	    (format "%s[%s]%s"
		    (make-string wl-plugged-port-indent (string-to-char " "))
		    (wl-plugged-string plugged time)
		    (cond
		     ((stringp port)
		      port)
		     (t
		      (format "%s(%d)"
			      (or label
				  (cdr (assq port wl-plugged-port-label-alist))
				  "")
			      port)))))
      (setq column (max (if line (1+ (string-width line)) 0)
			wl-plugged-queue-status-column))
      (cond
       ;; sending queue status
       ((setq qinfo (assoc (cons server port) wl-plugged-sending-queue-alist))
	(setq line
	      (concat
	       (wl-set-string-width column line)
	       (wl-plugged-sending-queue-status qinfo))))
       ;; dop queue status
       ((setq qinfo (assoc (list server port stream-type)
			   wl-plugged-dop-queue-alist))
	(setq line
	      (concat
	       (wl-set-string-width column line)
	       (wl-plugged-dop-queue-status qinfo column)))))
      (insert line "\n")
      (setq alist (cdr alist)))
    (delete-region (1- (point-max)) (point-max)) ;; delete line at the end.
    (goto-char (point-min))
    (while (not (eobp))
      (wl-highlight-plugged-current-line)
      (forward-line)))
  (set-buffer-modified-p nil)
  (count-lines (point-min) (point-max)))

(defun wl-plugged-redrawing-switch (indent switch &optional time)
  (beginning-of-line)
  (when (re-search-forward
	 (format "^%s\\[\\([^]]+\\)\\]"
		 (make-string indent (string-to-char " "))))
    (goto-char (match-beginning 1))
    (delete-region (match-beginning 1) (match-end 1))
    (insert (wl-plugged-string switch time))
    (wl-highlight-plugged-current-line)
    (forward-line)))

(defun wl-plugged-redrawing (plugged-alist)
  (let ((buffer-read-only nil)
	(alist plugged-alist)
	last server port plugged time)
    (goto-char (point-min))
    (wl-plugged-redrawing-switch 0 (elmo-plugged-p))
    (while alist
      (setq server (caaar alist)
	    port (cdaar alist)
	    plugged (nth 2 (car alist))
	    time (nth 3 (car alist)))
      (unless (string= last server)
	;; server plug
	(wl-plugged-redrawing-switch
	 wl-plugged-server-indent
	 (elmo-plugged-p server nil plugged-alist))
	(setq last server))
      ;; port plug
      (wl-plugged-redrawing-switch
       wl-plugged-port-indent plugged time)
      (setq alist (cdr alist))))
  (sit-for 0)
  (set-buffer-modified-p nil))

(defun wl-plugged-change ()
  (interactive)
  (if (not elmo-plugged-alist)
      (message "No plugged info")
    (setq wl-plugged-winconf (current-window-configuration))
    (let* ((cur-win (selected-window))
	   (max-lines (if (eq major-mode 'wl-summary-mode)
			  (/ (frame-height) 2)
			(window-height)))
	   window-lines lines)
      (with-current-buffer (get-buffer-create wl-plugged-buf-name)
	(wl-plugged-mode)
	(buffer-disable-undo (current-buffer))
	(delete-windows-on (current-buffer))
	(wl-plugged-set-variables)
	(setq lines (wl-plugged-drawing wl-plugged-alist)))
      (select-window cur-win)
      (setq window-lines (min max-lines (max lines window-min-height)))
      (when (> (- (window-height) window-lines) window-min-height)
	(split-window cur-win (- (window-height) window-lines)))
      (switch-to-buffer wl-plugged-buf-name)
      (condition-case nil
	  (progn
	    (enlarge-window (- window-lines (window-height)))
	    (when (fboundp 'pos-visible-in-window-p)
	      (goto-char (point-min))
	      (while (and (< (window-height) max-lines)
			  (not (pos-visible-in-window-p (1- (point-max)))))
		(enlarge-window 2))))
	(error))
      (goto-char (point-min))
      (forward-line)
      (wl-plugged-move-to-next)))) ;; goto first entry

(defsubst wl-plugged-get-server ()
  (save-excursion
    (end-of-line)
    (wl-plugged-move-to-previous-server)
    (beginning-of-line)
    (when (looking-at (format "^%s\\[[^]]+\\]\\(.*\\)"
			      (wl-plugged-server-indent)))
      (elmo-match-buffer 1))))

(defun wl-plugged-toggle ()
  (interactive)
  (let ((cur-point (point)))
    (save-excursion
      (beginning-of-line)
      (cond
       ;; switch variable
       ((bobp)
	(let (variable switch name)
	  (goto-char cur-point)
	  (when (and (not (bobp))
		     (not (eq (preceding-char) (string-to-char " "))))
	    (if (re-search-backward " [^ ]+" nil t)
		(forward-char)
	      (re-search-backward "^[^ ]+" nil t)))
	  (when (looking-at "\\([^ :[]+\\):?\\[\\([^]]+\\)\\]")
	    (setq name (elmo-match-buffer 1))
	    (setq switch (not (string= (elmo-match-buffer 2) wl-plugged-plug-on)))
	    (when (setq variable (cdr (assoc name wl-plugged-switch-variables)))
	      (set variable switch))
	    (goto-char (match-beginning 2))
	    (let ((buffer-read-only nil))
	      (delete-region (match-beginning 2) (match-end 2))
	      (insert (wl-plugged-string switch))
	      (set-buffer-modified-p nil)))))
       ;; switch plug
       ((looking-at "^\\( *\\)\\[\\([^]]+\\)\\]\\([^ \n]*\\)")
	(let* ((indent (length (elmo-match-buffer 1)))
	       (switch (elmo-match-buffer 2))
	       (name (elmo-match-buffer 3))
	       (plugged (not (string= switch wl-plugged-plug-on)))
	       (alist wl-plugged-alist)
	       server port stream-type name-1)
	  (cond
	   ((eq indent wl-plugged-port-indent)  ;; toggle port plug
	    (cond
	     ((string-match "\\([^([]*\\)(\\([^)[]+\\))" name)
	      (setq port (string-to-number (match-string 2 name)))
	      (if (string-match "!" (setq name-1 (match-string 1 name)))
		  (setq stream-type
			(intern (substring name-1 (match-end 0))))))
	     (t
	      (setq port name)))
	    (setq server (wl-plugged-get-server))
	    (elmo-set-plugged plugged server port stream-type nil alist))
	   ((eq indent wl-plugged-server-indent)  ;; toggle server plug
	    (elmo-set-plugged plugged name nil nil nil alist))
	   ((zerop indent)  ;; toggle all plug
	    (elmo-set-plugged plugged nil nil nil nil alist)))
	  ;; redraw
	  (wl-plugged-redrawing wl-plugged-alist)
	  ;; show plugged status in modeline
	  (let ((elmo-plugged wl-plugged-switch))
	    (setq wl-plugged-switch (elmo-plugged-p)
		  wl-modeline-plug-status wl-plugged-switch)
	    (force-mode-line-update t))))))
    (setq wl-plugged-alist-modified t)
    (goto-char cur-point)))

(defun wl-plugged-click (e)
  (interactive "e")
  (mouse-set-point e)
  (wl-plugged-toggle))

(defun wl-plugged-toggle-all ()
  (interactive)
  (let ((cur-point (point)))
    (setq wl-plugged-switch (not wl-plugged-switch))
    (elmo-set-plugged wl-plugged-switch nil nil nil nil wl-plugged-alist)
    (wl-plugged-redrawing wl-plugged-alist)
    (goto-char cur-point)
    (setq wl-plugged-alist-modified t)
    ;; show plugged status in modeline
    (setq wl-modeline-plug-status wl-plugged-switch)
    (force-mode-line-update t)))

(defun wl-plugged-exit ()
  (interactive)
  (setq wl-plugged wl-plugged-switch
;;;	elmo-plugged-alist wl-plugged-alist
	wl-plugged-alist nil
	wl-plugged-sending-queue-alist nil
	wl-plugged-dop-queue-alist nil)
  (run-hooks 'wl-plugged-exit-hook)
  (when wl-plugged-alist-modified
    (wl-toggle-plugged (if wl-plugged 'on 'off) t))
  (kill-buffer (current-buffer))
  (if wl-plugged-winconf
      (set-window-configuration wl-plugged-winconf)))

(defun wl-plugged-flush-queue ()
  (interactive)
  (let ((cur-point (point))
	(dop-status (elmo-dop-queue-flush))
	(send-status (wl-draft-queue-flush)))
    (unless (or dop-status send-status)
      (message "No processing queue."))
    (wl-plugged-set-variables)
    (wl-plugged-drawing wl-plugged-alist)
    (goto-char cur-point)))

(defun wl-plugged-move-to-next ()
  (interactive)
  (when (re-search-forward "\\[\\([^]]+\\)\\]" nil t)
    (let ((pos (match-beginning 1)))
      (if (invisible-p pos)
	  (goto-char (next-visible-point pos))
	(goto-char pos)))))

(defun wl-plugged-move-to-previous ()
  (interactive)
  (if (eq (preceding-char) ?\]) (backward-char))
  (when (re-search-backward "\\[\\([^]]+\\)\\]" nil t)
    (let ((pos (match-beginning 1)))
      (if (invisible-p pos)
	  (goto-char (next-visible-point pos))
	(goto-char pos)))))

(defun wl-plugged-move-to-next-server ()
  (interactive)
  (let ((regexp
	 (format "^%s\\[\\([^]]+\\)\\]" (wl-plugged-server-indent)))
	point)
    (save-excursion
      (end-of-line)
      (if (re-search-forward regexp nil t)
	  (setq point (match-beginning 1))))
    (if point (goto-char point))))

(defun wl-plugged-move-to-previous-server ()
  (interactive)
  (let ((regexp
	 (format "^%s\\[\\([^]]+\\)\\]" (wl-plugged-server-indent))))
    (if (re-search-backward regexp nil t)
	(goto-char (match-beginning 1)))))

;;; end of wl-plugged-mode

(defun wl-save ()
  "Save summary and folder status."
  (interactive)
  (wl-save-status 'keep-summary)
  (run-hooks 'wl-save-hook))

(defun wl-execute-temp-marks ()
  "Execute temporary marks in summary buffers."
  (interactive)
  (let ((summaries (wl-collect-summary)))
    (while summaries
      (with-current-buffer (car summaries)
	(wl-summary-exec-with-confirmation)
	(wl-summary-save-status))
      (setq summaries (cdr summaries)))))

(defun wl-save-status (&optional keep-summary)
  (message "Saving summary and folder status...")
  (save-excursion
    (let ((summaries (wl-collect-summary)))
      (while summaries
	(with-current-buffer (car summaries)
	  (unless keep-summary
	    (wl-summary-cleanup-temp-marks))
	  (wl-summary-save-view)
	  (elmo-folder-commit wl-summary-buffer-elmo-folder)
	  (unless keep-summary
	    (kill-buffer (car summaries))))
	(setq summaries (cdr summaries)))))
  (wl-refile-alist-save)
  (wl-folder-info-save)
  (and (featurep 'wl-fldmgr) (wl-fldmgr-exit))
  (and (featurep 'wl-spam) (wl-spam-save-status))
  (elmo-crosspost-message-alist-save)
  (message "Saving summary and folder status...done"))

(defun wl-exit ()
  (interactive)
  (when (or (not wl-interactive-exit)
	    (y-or-n-p "Do you really want to quit Wanderlust? "))
    (elmo-quit)
    (when wl-use-acap (funcall (symbol-function 'wl-acap-exit)))
    (wl-biff-stop)
    (elmo-clear-signal-slots)
    (run-hooks 'wl-exit-hook)
    (wl-save-status)
    (wl-folder-cleanup-variables)
    (wl-message-buffer-cache-clean-up)
    (wl-kill-buffers
     (format "^\\(%s\\)$"
	     (mapconcat 'identity
			(list wl-folder-buffer-name
			      wl-plugged-buf-name)
			"\\|")))
    (when wl-delete-startup-frame-function
      (funcall wl-delete-startup-frame-function))
;;;    (if (and wl-folder-use-frame
;;;	     (> (length (visible-frame-list)) 1))
;;;	(delete-frame))
    (setq wl-init nil)
    (remove-hook 'kill-emacs-hook 'wl-save-status)
    (elmo-passwd-alist-clear)
    t)
  (message "") ; empty minibuffer.
  )

(defun wl-init ()
  (when (not wl-init)
    (require 'mime-setup)
    (setq elmo-plugged wl-plugged)
    (add-hook 'kill-emacs-hook 'wl-save-status)
    (wl-address-init)
    (wl-draft-setup)
    (wl-refile-alist-setup)
    (require 'wl-mime)
    ;; defined above.
    (wl-mime-setup)
    (fset 'wl-summary-from-func-internal
	  (symbol-value 'wl-summary-from-function))
    (fset 'wl-summary-subject-func-internal
	  (symbol-value 'wl-summary-subject-function))
    (fset 'wl-summary-subject-filter-func-internal
	  (symbol-value 'wl-summary-subject-filter-function))
    (wl-summary-define-sort-command)
    (wl-summary-define-mark-action)
    (dolist (spec wl-summary-flag-alist)
      (set-face-foreground
       (make-face (intern
		   (format "wl-highlight-summary-%s-flag-face" (car spec))))
       (nth 1 spec)))
    (setq elmo-get-folder-function #'wl-folder-make-elmo-folder
	  elmo-progress-callback-function #'wl-progress-callback-function)
    (setq elmo-no-from wl-summary-no-from-message)
    (setq elmo-no-subject wl-summary-no-subject-message)
    (elmo-global-flags-initialize (mapcar 'car wl-summary-flag-alist))
    (elmo-connect-signal
     nil
     'message-number-changed
     'wl-draft
     (elmo-define-signal-handler (listener folder old-number new-number)
       (dolist (buffer (wl-collect-draft))
	 (with-current-buffer buffer
	   (wl-draft-buffer-change-number old-number new-number)))
       (wl-draft-rename-saved-config old-number new-number))
     (elmo-define-signal-filter (listener folder old-number new-number)
       (and folder
	    (string= (elmo-folder-name-internal folder) wl-draft-folder))))
    (when (featurep 'wl-news)
      (wl-news-check))
    (setq wl-init t)
    ;; This hook may contain the functions `wl-plugged-init-icons' and
    ;; `wl-biff-init-icons' for reasons of system internal to accord
    ;; facilities for the Emacs variants.
    (run-hooks 'wl-init-hook)))

(defun wl-check-environment (no-check-folder)
  ;; wl-from
  (let* ((lal (std11-lexical-analyze wl-from))
	 (ret (std11-parse-mailbox lal))
	 address)
    (unless
	;; Copied and modified from wl-draft-std11-parse-addresses.
	(when (or (and (eq (length lal) 1)
		       (eq (car (car lal)) 'spaces))
		  ret)
	  (setq lal (cdr ret))
	  (while (eq 'spaces (car (car lal)))
	    (setq lal (cdr lal)))
	  (null lal))
      (error "Please set `wl-from' to your mail address"))
    (setq address (std11-address-string (car ret)))
    (when (string-match "@[^.]+$" address)
      (elmo-warning
       "Domain portion of `wl-from' seems to be a local hostname.")))

  ;; Message-ID
  (when wl-insert-message-id
    (let ((message-id (funcall wl-message-id-function))
	  (custom-msgid-function
	   (null (eq wl-message-id-function
		     'wl-draft-make-message-id-string)))
	  domain)
      (unless (std11-parse-msg-id
	       (std11-lexical-analyze message-id))
	;; Invalid Message-ID
	(error
	 (cond
	  (custom-msgid-function
	   "Check around `wl-message-id-function' to get valid Message-ID")
	  (wl-message-id-use-message-from
	   ;; `wl-from' is already checked.
	   "Check `wl-message-id-hash-function' and `wl-draft-make-message-id-from-address-delimiter' to get valid Message-ID")
	  ((and wl-message-id-domain
		(string-match "@" wl-message-id-domain))
	   "Remove `@' from `wl-message-id-domain'")
	  (t
	   "Check `wl-message-id-domain' to get valid Message-ID"))))
      (when (string-match "@[^.]+$" message-id)
	(elmo-warning
	 (cond
	  (custom-msgid-function
	   "Please check around `wl-message-id-function' to get valid Message-ID.")
	  (wl-message-id-use-message-from
	   "`wl-from' address is used to make Message-ID string.")
	  (wl-message-id-domain
	   "`wl-message-id-domain' seems to be a local hostname.")
	  (t
	   "Please set `wl-message-id-domain' to get valid Message-ID."))))
      ))

  ;; folders
  (when (not no-check-folder)
    (let ((draft-folder (wl-folder-get-elmo-folder wl-draft-folder))
	  (queue-folder (wl-folder-get-elmo-folder wl-queue-folder))
	  (lost+found-folder (wl-folder-get-elmo-folder
			      elmo-lost+found-folder)))
      (unless (elmo-folder-exists-p draft-folder)
	(if (y-or-n-p
	     (format "Draft Folder %s does not exist, create it? "
		     wl-draft-folder))
	    (elmo-folder-create draft-folder)
	  (error "Draft Folder is not created")))
      (if (and wl-draft-enable-queuing
	       (not (elmo-folder-exists-p queue-folder)))
	  (if (y-or-n-p
	       (format "Queue Folder %s does not exist, create it? "
		       wl-queue-folder))
	      (elmo-folder-create queue-folder)
	    (error "Queue Folder is not created")))
      (when (not (eq no-check-folder 'wl-draft))
	(unless (elmo-folder-exists-p lost+found-folder)
	  (elmo-folder-create lost+found-folder)))
      ;; tmp dir
      (unless (file-exists-p wl-temporary-file-directory)
	(if (y-or-n-p
	     (format "Temp directory (to save multipart) %s does not exist, create it now? "
		     wl-temporary-file-directory))
	    (make-directory wl-temporary-file-directory)
	  (error "Temp directory is not created"))))))

(defconst wl-check-variables-alist
  '((numberp . elmo-pop3-default-port)
    (symbolp . elmo-pop3-default-authenticate-type)
    (numberp . elmo-imap4-default-port)
    (symbolp . elmo-imap4-default-authenticate-type)
    (numberp . elmo-nntp-default-port)
    (numberp . wl-pop-before-smtp-port)
    (symbolp . wl-pop-before-smtp-authenticate-type)))

(defun wl-check-variables ()
  (let ((type-variables wl-check-variables-alist)
	type)
    (while (setq type (car type-variables))
      (if (and (eval (cdr type))
	       (not (funcall (car type)
			     (eval (cdr type)))))
	  (error "%s must be %s: %S"
		 (cdr type)
		 (substring (format "%s" (car type)) 0 -1)
		 (eval (cdr type))))
      (setq type-variables (cdr type-variables)))))

(defun wl-check-variables-2 ()
  (if (< wl-message-buffer-cache-size 1)
      (error "`wl-message-buffer-cache-size' must be larger than 0"))
  (when wl-message-buffer-prefetch-depth
    (if (not (< wl-message-buffer-prefetch-depth
		wl-message-buffer-cache-size))
	(error (concat
		"`wl-message-buffer-prefetch-depth' must be smaller than "
		"`wl-message-buffer-cache-size' - 1.")))))

;;;###autoload
(defun wl (&optional arg)
  "Start Wanderlust -- Yet Another Message Interface On Emacsen.
If ARG (prefix argument) is specified, folder checkings are skipped."
  (interactive "P")
  (unless wl-init
    (wl-load-profile)
    (elmo-init))
  (let (demo-buf check)
    (unless wl-init
      (if wl-demo (setq demo-buf (wl-demo)))
      (setq check t))
    (wl-init)
    (condition-case obj
	(progn
	  (if check
	      (progn
		(message "Checking environment...")
		(wl-check-environment arg)
		(message "Checking environment...done")
		(message "Checking type of variables...")
		(wl-check-variables)
		(wl-check-variables-2)
		(message "Checking type of variables...done")))
	  (let ((inhibit-quit t))
	    (wl-plugged-init (wl-folder)))
	  (unless arg
	    (run-hooks 'wl-auto-check-folder-pre-hook)
	    (wl-folder-auto-check)
	    (run-hooks 'wl-auto-check-folder-hook)))
      (error
       (if (buffer-live-p demo-buf)
	   (kill-buffer demo-buf))
       (signal (car obj)(cdr obj)))
      (quit))
    (when wl-biff-check-folder-list
      (unless arg (wl-biff-check-folders))
      (wl-biff-start))
    (if (buffer-live-p demo-buf)
	(kill-buffer demo-buf)))
  (run-hooks 'wl-hook))

(defvar wl-delete-startup-frame-function nil)

;;;###autoload
(defun wl-other-frame (&optional arg)
  "Pop up a frame to read messages via Wanderlust."
  (interactive)
  (if wl-folder-use-frame
      (wl arg)
    (let ((focusing-functions (append '(raise-frame select-frame)
				      (if (fboundp 'x-focus-frame)
					  '(x-focus-frame)
					'(focus-frame))))
	  (folder (get-buffer wl-folder-buffer-name))
	  window frame wl-folder-use-frame)
      (if (and folder
	       (setq window (get-buffer-window folder t))
	       (window-live-p window)
	       (setq frame (window-frame window)))
	  (progn
	    (while focusing-functions
	      (funcall (car focusing-functions) frame)
	      (setq focusing-functions (cdr focusing-functions)))
	    (wl arg))
	(setq frame (make-frame))
	(while focusing-functions
	  (funcall (car focusing-functions) frame)
	  (setq focusing-functions (cdr focusing-functions)))
	(setq wl-delete-startup-frame-function
	      `(lambda ()
		 (setq wl-delete-startup-frame-function nil)
		 (let ((frame ,frame))
		   (if (eq (selected-frame) frame)
		       (delete-frame frame)))))
	(wl arg)))))

;; Define some autoload functions WL might use.
(eval-and-compile
  ;; This little mapc goes through the list below and marks the
  ;; symbols in question as autoloaded functions.
  (mapc
   (lambda (package)
     (let ((interactive (nth 1 (memq ':interactive package))))
       (mapc
	(lambda (function)
	  (let (keymap)
	    (when (consp function)
	      (setq keymap (car (memq 'keymap function)))
	      (setq function (car function)))
	    (autoload function (car package) nil interactive keymap)))
	(if (eq (nth 1 package) ':interactive)
	    (cdddr package)
	  (cdr package)))))
   '(("wl-fldmgr" :interactive t
      wl-fldmgr-access-display-all wl-fldmgr-access-display-normal
      wl-fldmgr-add wl-fldmgr-clear-cut-entity-list wl-fldmgr-copy
      wl-fldmgr-copy-region wl-fldmgr-cut wl-fldmgr-cut-region
      wl-fldmgr-make-access-group wl-fldmgr-make-filter
      wl-fldmgr-make-group wl-fldmgr-make-multi
      wl-fldmgr-reconst-entity-hashtb wl-fldmgr-rename wl-fldmgr-delete
      wl-fldmgr-save-folders wl-fldmgr-set-petname wl-fldmgr-sort
      wl-fldmgr-subscribe wl-fldmgr-subscribe-region
      wl-fldmgr-unsubscribe wl-fldmgr-unsubscribe-region wl-fldmgr-yank )
     ("wl-acap" wl-acap-init)
     ("wl-acap" :interactive t wl-acap-store)
     ("wl-fldmgr"
      (wl-fldmgr-mode-map keymap)
      wl-fldmgr-add-entity-hashtb)
     ("wl-expire" :interactive t
      wl-folder-archive-current-entity
      wl-folder-expire-current-entity wl-summary-archive
      wl-summary-expire )
     ("wl-score"
      wl-score-save wl-summary-rescore-msgs wl-summary-score-headers
      wl-summary-score-update-all-lines )
     ("wl-score" :interactive t
      wl-score-change-score-file wl-score-edit-current-scores
      wl-score-edit-file wl-score-flush-cache wl-summary-rescore
      wl-score-set-mark-below wl-score-set-expunge-below
      wl-summary-increase-score wl-summary-lower-score )
     ("wl-draft" wl-draft-rename-saved-config))))

;; for backward compatibility
(defalias 'wl-summary-from-func-petname 'wl-summary-default-from)

(require 'product)
(product-provide (provide 'wl) (require 'wl-version))

;;; wl.el ends here
