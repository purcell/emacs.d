;;; wl-e21.el --- Wanderlust modules for Emacs 21.

;; Copyright (C) 2000,2001 Katsumi Yamaoka <yamaoka@jpl.org>
;; Copyright (C) 2000,2001 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
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
;; This module uses `before-string' overlay property to show icon
;; images instead of `insert-image', so don't delete such overlays
;; sloppily.  Here is a sample code to show icons in the buffer.
;;
;;(let (image icon from to overlay)
;;  ;; The function `find-image' will look for an image first on `load-path'
;;  ;; and then in `data-directory'.
;;  (let ((load-path (cons wl-icon-directory load-path)))
;;    (setq image (find-image (list (list :type 'xpm :file wl-nntp-folder-icon
;;					:ascent 'center)))))
;;  ;; `propertize' is a convenient function in such a case.
;;  ;; String must have one or more length to wear an image.
;;  (setq icon (propertize "any string" 'display image))
;;  (pop-to-buffer (get-buffer-create "*wl-e21-demo*"))
;;  (erase-buffer)
;;  (insert "   ")
;;  (setq from (point))
;;  (insert "-fj.wanderlust:0/0/0")
;;  (setq to (point))
;;  (insert "\n")
;;  (setq overlay (make-overlay from to))
;;  ;; Put an image.
;;  (overlay-put overlay 'before-string icon)
;;  ;; Put a mark to indicate that this overlay is made by `wl-e21'.
;;  ;; It is not always necessarily.
;;  (overlay-put overlay 'wl-e21-icon t)
;;  ;; Make it to be removable.
;;  (overlay-put overlay 'evaporate t))
;;
;; Note that a port of Emacs to some platforms (e.g. MS-Windoze) does
;; not yet support images.  It is a pity that neither icons nor tool-
;; bars will not be displayed in such systems.

;;; Code:
;;

(require 'elmo)
(eval-when-compile
  (require 'wl-folder)
  (require 'wl-summary)
  (require 'wl-draft)
  (require 'wl-message)
  (require 'wl-highlight)
  (defvar-maybe wl-draft-mode-map (make-sparse-keymap)))

(add-hook 'wl-folder-mode-hook 'wl-setup-folder)
(add-hook 'wl-folder-mode-hook 'wl-folder-init-icons)

(add-hook 'wl-init-hook 'wl-biff-init-icons)
(add-hook 'wl-init-hook 'wl-plugged-init-icons)

(add-hook 'wl-summary-mode-hook 'wl-setup-summary)

(add-hook 'wl-message-display-internal-hook 'wl-setup-message)

(defvar wl-use-toolbar (image-type-available-p 'xpm))
(defvar wl-plugged-image nil)
(defvar wl-unplugged-image nil)
(defvar wl-biff-mail-image nil)
(defvar wl-biff-nomail-image nil)

(defvar wl-folder-toolbar
  '([wl-folder-jump-to-current-entity
     wl-folder-jump-to-current-entity t "Enter Current Folder"]
    [wl-folder-next-entity
     wl-folder-next-entity t "Next Folder"]
    [wl-folder-prev-entity
     wl-folder-prev-entity t "Previous Folder"]
    [wl-folder-check-current-entity
     wl-folder-check-current-entity t "Check Current Folder"]
    [wl-folder-sync-current-entity
     wl-folder-sync-current-entity t "Sync Current Folder"]
    [wl-draft
     wl-draft t "Write a New Message"]
    [wl-folder-goto-draft-folder
     wl-folder-goto-draft-folder t "Go to Draft Folder"]
    [wl-folder-empty-trash
     wl-folder-empty-trash t "Empty Trash"]
    [wl-exit
     wl-exit t "Quit Wanderlust"]
    )
  "The Folder buffer toolbar.")

(defvar wl-summary-toolbar
  '([wl-summary-read
     wl-summary-read t "Read Messages"]
    [wl-summary-next
     wl-summary-next t "Next Message"]
    [wl-summary-prev
     wl-summary-prev t "Previous Message"]
    [wl-summary-jump-to-current-message
     wl-summary-jump-to-current-message t "Jump to Current Message"]
    [wl-summary-sync-force-update
     wl-summary-sync-force-update t "Sync Current Folder"]
    [wl-summary-dispose
     wl-summary-dispose t "Dispose Current Message"]
    [wl-summary-set-flags
     wl-summary-set-flags t "Set Flags"]
    [wl-draft
     wl-summary-write-current-folder t "Write for Current Folder"]
    [wl-summary-reply
     wl-summary-reply t "Reply to Current Message" ]
    [wl-summary-reply-with-citation
     wl-summary-reply-with-citation t "Reply to Current Message with Citation"]
    [wl-summary-forward
     wl-summary-forward t "Forward Current Message"]
    [wl-summary-exit
     wl-summary-exit t "Exit Current Summary"]
    )
  "The Summary buffer toolbar.")

(defvar wl-message-toolbar
  '([wl-message-read
     wl-message-read t "Read Contents"]
    [wl-message-next-content
     wl-message-next-content t "Next Content"]
    [wl-message-prev-content
     wl-message-prev-content t "Previous Content"]
    [wl-message-quit
     wl-message-quit t "Back to Summary"]
    [wl-message-play-content
     wl-message-play-content t "Play Content"]
    [wl-message-extract-content
     wl-message-extract-content t "Extract Content"]
    )
  "The Message buffer toolbar.")

;; for draft toolbar.
(defalias 'wl-draft-insert-signature
  (if (and (boundp 'mime-setup-use-signature) mime-setup-use-signature)
      'insert-signature
    'mime-edit-insert-signature))

(defvar wl-draft-toolbar
  '([wl-draft-send-from-toolbar
     wl-draft-send-from-toolbar t "Send Current Draft"]
    [wl-draft-yank-original
     wl-draft-yank-original t "Yank Displaying Message"]
    [wl-draft-insert-signature
     wl-draft-insert-signature t "Insert Signature"]
    [wl-draft-kill
     wl-draft-kill t "Kill Current Draft"]
    [wl-draft-save-and-exit
     wl-draft-save-and-exit t "Save Draft and Exit"]
    )
  "The Draft buffer toolbar.")

(eval-when-compile
  (defmacro wl-e21-display-image-p ()
    '(and wl-highlight-folder-with-icon
	  (image-type-available-p 'xpm))))

(eval-and-compile
  (if (boundp 'image-load-path)
      (defun wl-e21-find-image (specs)
	(let ((image-load-path (cons 'wl-icon-directory image-load-path)))
	  (find-image specs)))
    (defun wl-e21-find-image (specs)
      (let ((load-path (cons wl-icon-directory load-path)))
	(find-image specs)))))

(defun wl-e21-setup-toolbar (bar)
  (when (and wl-use-toolbar
	     (wl-e21-display-image-p))
    (let ((props '(:type xpm :ascent center
			 :color-symbols (("backgroundToolBarColor" . "None"))
			 :file))
	  (success t)
	  icon up down disabled name)
      (while bar
	(setq icon (aref (pop bar) 0))
	(unless (boundp icon)
	  (setq name (symbol-name icon)
		up (wl-e21-find-image `((,@props ,(concat name "-up.xpm")))))
	  (if up
	      (progn
		(setq down (wl-e21-find-image
			    `((,@props ,(concat name "-down.xpm"))))
		      disabled (wl-e21-find-image
				`((,@props ,(concat name "-disabled.xpm")))))
		(if (and down disabled)
		    (set icon (vector down up disabled disabled))
		  (set icon up)))
	    (setq bar nil
		  success nil))))
      success)))

(defvar wl-e21-toolbar-configurations
  '((auto-resize-tool-bars       . t)
    (auto-raise-tool-bar-buttons . t)
    (tool-bar-button-margin      . 2)
    (tool-bar-button-relief      . 1)))

(defun wl-e21-make-toolbar-buttons (keymap defs)
  (let ((configs wl-e21-toolbar-configurations)
	config)
    (while (setq config (pop configs))
      (set (make-local-variable (car config)) (cdr config))))
  ;; Invalidate the default bindings.
  (let ((keys (cdr (key-binding [tool-bar] t)))
	item)
    (unless (eq (caar keys) 'keymap) ;; Emacs >= 24
      (while (setq item (pop keys))
	(when (setq item (car-safe item))
	  (define-key keymap (vector 'tool-bar item) 'undefined)))))
  (let ((n (length defs))
	def)
    (while (>= n 0)
      (setq n (1- n)
	    def (nth n defs))
      (define-key keymap (vector 'tool-bar (aref def 1))
	(list 'menu-item (aref def 3) (aref def 1)
	      :enable (aref def 2)
	      :image (symbol-value (aref def 0)))))))

(defun wl-e21-setup-folder-toolbar ()
  (when (wl-e21-setup-toolbar wl-folder-toolbar)
    (wl-e21-make-toolbar-buttons wl-folder-mode-map wl-folder-toolbar)))

(defun wl-e21-setup-summary-toolbar ()
  (when (wl-e21-setup-toolbar wl-summary-toolbar)
    (wl-e21-make-toolbar-buttons wl-summary-mode-map wl-summary-toolbar)))

(eval-when-compile
  (defsubst wl-e21-setup-draft-toolbar ()
    (when (wl-e21-setup-toolbar wl-draft-toolbar)
      (wl-e21-make-toolbar-buttons wl-draft-mode-map wl-draft-toolbar))))

(defun wl-e21-setup-message-toolbar ()
  (when (wl-e21-setup-toolbar wl-message-toolbar)
    (wl-e21-make-toolbar-buttons (current-local-map) wl-message-toolbar)))

(defvar wl-folder-toggle-icon-list
  '((wl-folder-opened-image       . wl-opened-group-folder-icon)
    (wl-folder-closed-image       . wl-closed-group-folder-icon)))

(eval-when-compile
  (defsubst wl-e21-highlight-folder-group-line (start end icon numbers)
    (let (image)
      (when (wl-e21-display-image-p)
	(let (overlay)
	  (let ((overlays (overlays-in start end)))
	    (while (and (setq overlay (pop overlays))
			(not (overlay-get overlay 'wl-e21-icon)))))
	  (unless overlay
	    (setq overlay (make-overlay start end))
	    (overlay-put overlay 'wl-e21-icon t)
	    (overlay-put overlay 'evaporate t))
	  (setq image (get icon 'image))
	  (unless image
	    (let ((name (symbol-value
			 (cdr (assq icon wl-folder-toggle-icon-list)))))
	      (setq image (wl-e21-find-image
			   `((:type xpm :file ,name :ascent center))))))
	  (overlay-put overlay 'display image)))
      (when (and wl-use-highlight-mouse-line (display-mouse-p))
	(let ((inhibit-read-only t))
	  (put-text-property (if image
				 (max (1- start) (line-beginning-position))
			       start)
			     (line-end-position)
			     'mouse-face 'highlight)))))

  (defsubst wl-e21-highlight-folder-by-numbers (start end text-face numbers)
    (when (display-color-p)
      (let ((inhibit-read-only t))
	(if (and wl-highlight-folder-by-numbers
		 numbers (nth 0 numbers) (nth 1 numbers)
		 (re-search-forward "[-[:digit:]]+/[-[:digit:]]+/[-[:digit:]]+"
				    (line-end-position) t))
	    (let* ((unsync (nth 0 numbers))
		   (unread (nth 1 numbers))
		   (face (cond ((and unsync (zerop unsync))
				(if (and unread (zerop unread))
				    'wl-highlight-folder-zero-face
				  'wl-highlight-folder-unread-face))
			       ((and unsync
				     (>= unsync
					 wl-folder-many-unsync-threshold))
				'wl-highlight-folder-many-face)
			       (t
				'wl-highlight-folder-few-face))))
	      (if (numberp wl-highlight-folder-by-numbers)
		  (progn
		    (put-text-property start (match-beginning 0)
				       'face text-face)
		    (put-text-property (match-beginning 0) (match-end 0)
				       'face face))
		(put-text-property start (match-end 0) 'face face)))
	  (put-text-property start (line-end-position) 'face text-face))))))

(defun wl-highlight-folder-current-line (&optional numbers)
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let (fld-name start end)
      (cond
       ;; opened folder group
       ((and (wl-folder-buffer-group-p)
	     (looking-at wl-highlight-folder-opened-regexp))
	(setq start (match-beginning 1)
	      end (match-end 1))
	(wl-e21-highlight-folder-group-line start end
					    'wl-folder-opened-image
					    numbers)
	(wl-e21-highlight-folder-by-numbers start end
					    'wl-highlight-folder-opened-face
					    numbers))
       ;; closed folder group
       ((and (wl-folder-buffer-group-p)
	     (looking-at wl-highlight-folder-closed-regexp))
	(setq start (match-beginning 1)
	      end (match-end 1))
	(wl-e21-highlight-folder-group-line start end
					    'wl-folder-closed-image
					    numbers)
	(wl-e21-highlight-folder-by-numbers start end
					    'wl-highlight-folder-closed-face
					    numbers))
       ;; basic folder
       ((and (setq fld-name (wl-folder-get-folder-name-by-id
			     (get-text-property (point) 'wl-folder-entity-id)))
	     (looking-at "[[:blank:]]+\\([^[:blank:]\n]+\\)"))
	(setq start (match-beginning 1)
	      end (match-end 1))
	(let (image)
	  (when (wl-e21-display-image-p)
	    (let (overlay)
	      (let ((overlays (overlays-in start end)))
		(while (and (setq overlay (pop overlays))
			    (not (overlay-get overlay 'wl-e21-icon)))))
	      (unless overlay
		(setq overlay (make-overlay start end))
		(overlay-put overlay 'wl-e21-icon t)
		(overlay-put overlay 'evaporate t))
	      (let (type)
		(unless (get (caar wl-folder-internal-icon-list) 'image)
		  (wl-folder-init-icons))
		(setq image
		      (cond
		       ;; trash folder
		       ((string= fld-name wl-trash-folder)
			(let ((num (nth 2 numbers))) ; number of messages
			  (get (if (or (not num) (zerop num))
				   'wl-folder-trash-empty-image
				 'wl-folder-trash-image)
			       'image)))
		       ;; draft folder
		       ((string= fld-name wl-draft-folder)
			(get 'wl-folder-draft-image 'image))
		       ;; queue folder
		       ((string= fld-name wl-queue-folder)
			(get 'wl-folder-queue-image 'image))
		       ;; and one of many other folders
		       ((setq type (or (elmo-folder-type fld-name)
				       (elmo-folder-type-internal
					(elmo-make-folder fld-name))))
			(get (intern (format "wl-folder-%s-image" type))
			     'image)))))
	      (overlay-put overlay 'before-string
			   (propertize " " 'display image
				       'invisible t))))
	  (when (and wl-use-highlight-mouse-line (display-mouse-p))
	    (let ((inhibit-read-only t))
	      (put-text-property (if image
				     (max (1- start)
					  (line-beginning-position))
				   start)
				 (line-end-position)
				 'mouse-face 'highlight))))
	(when (display-color-p)
	  (wl-e21-highlight-folder-by-numbers
	   start end
	   (if (looking-at (format "^[[:blank:]]*\\(?:%s\\|%s\\)"
				   wl-folder-unsubscribe-mark
				   wl-folder-removed-mark))
	       'wl-highlight-folder-killed-face
	     'wl-highlight-folder-unknown-face)
	   numbers)))))))

(defun wl-highlight-plugged-current-line ()
  (interactive)
  (when (wl-e21-display-image-p)
    (save-excursion
      (beginning-of-line)
      (when (looking-at "[[:blank:]]*\\(\\[\\([^]]+\\)\\]\\)")
	(let* ((start (match-beginning 1))
	       (end (match-end 1))
	       (status (match-string-no-properties 2))
	       (image (if (string-equal wl-plugged-plug-on status)
			  wl-plugged-image
			wl-unplugged-image)))
	  (when image
	    (let (overlay)
	      (let ((overlays (overlays-in start end)))
		(while (and (setq overlay (pop overlays))
			    (not (overlay-get overlay 'wl-e21-icon)))))
	      (unless overlay
		(setq overlay (make-overlay start end))
		(overlay-put overlay 'wl-e21-icon t)
		(overlay-put overlay 'evaporate t))
	      (overlay-put overlay 'display image))))))))

(defun wl-plugged-set-folder-icon (folder string)
  (let (image type)
    (when (wl-e21-display-image-p)
      (setq image
	    (cond ((string= folder wl-queue-folder)
		   (get 'wl-folder-queue-image 'image))
		  ((setq type (or (elmo-folder-type folder)
				  (elmo-folder-type-internal
				   (elmo-make-folder folder))))
		   (get (intern (format "wl-folder-%s-image" type))
			'image)))))
    (if image
	(concat (propertize " " 'display image 'invisible t) string)
      string)))

(defvar wl-folder-internal-icon-list
  ;; alist of (image . icon-file)
  '((wl-folder-nntp-image	  . wl-nntp-folder-icon)
    (wl-folder-imap4-image	  . wl-imap-folder-icon)
    (wl-folder-pop3-image	  . wl-pop-folder-icon)
    (wl-folder-localdir-image     . wl-localdir-folder-icon)
    (wl-folder-localnews-image    . wl-localnews-folder-icon)
    (wl-folder-internal-image     . wl-internal-folder-icon)
    (wl-folder-multi-image	  . wl-multi-folder-icon)
    (wl-folder-filter-image       . wl-filter-folder-icon)
    (wl-folder-archive-image      . wl-archive-folder-icon)
    (wl-folder-pipe-image	  . wl-pipe-folder-icon)
    (wl-folder-maildir-image      . wl-maildir-folder-icon)
    (wl-folder-search-image	  . wl-search-folder-icon)
    (wl-folder-shimbun-image      . wl-shimbun-folder-icon)
    (wl-folder-file-image	  . wl-file-folder-icon)
    (wl-folder-access-image	  . wl-access-folder-icon)
    (wl-folder-trash-empty-image  . wl-empty-trash-folder-icon)
    (wl-folder-draft-image	  . wl-draft-folder-icon)
    (wl-folder-queue-image	  . wl-queue-folder-icon)
    (wl-folder-trash-image	  . wl-trash-folder-icon)))

(defun wl-folder-init-icons ()
  (when (wl-e21-display-image-p)
    (let ((icons wl-folder-internal-icon-list)
	  icon name image)
      (while (setq icon (pop icons))
	(unless (get (car icon) 'image)
	  (setq name (symbol-value (cdr icon))
		image (wl-e21-find-image
		       `((:type xpm :file ,name :ascent center))))
	  (when image
	    (put (car icon) 'image image)))))))

(defun wl-plugged-init-icons ()
  (let ((props (when (display-mouse-p)
		 (list 'local-map (purecopy (make-mode-line-mouse-map
					     'mouse-2 #'wl-toggle-plugged))
		       'help-echo "mouse-2 toggles plugged status"))))
    (if (wl-e21-display-image-p)
	(progn
	  (unless wl-plugged-image
	    (setq wl-plugged-image (wl-e21-find-image
				    `((:type xpm
					     :file ,wl-plugged-icon
					     :ascent center)))
		  wl-unplugged-image (wl-e21-find-image
				      `((:type xpm
					       :file ,wl-unplugged-icon
					       :ascent center)))))
	  (setq wl-modeline-plug-state-on
		(apply 'propertize wl-plug-state-indicator-on
		       `(display ,wl-plugged-image ,@props))
		wl-modeline-plug-state-off
		(apply 'propertize wl-plug-state-indicator-off
		       `(display ,wl-unplugged-image ,@props))))
      (if props
	  (setq wl-modeline-plug-state-on
		(apply 'propertize wl-plug-state-indicator-on props)
		wl-modeline-plug-state-off
		(apply 'propertize wl-plug-state-indicator-off props))
	(setq wl-modeline-plug-state-on wl-plug-state-indicator-on
	      wl-modeline-plug-state-off wl-plug-state-indicator-off)))))

(defun wl-biff-init-icons ()
  (let ((props (when (display-mouse-p)
		 (list 'local-map (purecopy (make-mode-line-mouse-map
					     'mouse-2 #'wl-biff-check-folders))
		       'help-echo "mouse-2 checks new mails"))))
    (if (wl-e21-display-image-p)
	(progn
	  (unless wl-biff-mail-image
	    (setq wl-biff-mail-image (wl-e21-find-image
				      `((:type xpm
					       :file ,wl-biff-mail-icon
					       :ascent center)))
		  wl-biff-nomail-image (wl-e21-find-image
					`((:type xpm
						 :file ,wl-biff-nomail-icon
						 :ascent center)))))
	  (setq wl-modeline-biff-state-on
		(apply 'propertize wl-biff-state-indicator-on
		       `(display ,wl-biff-mail-image ,@props))
		wl-modeline-biff-state-off
		(apply 'propertize wl-biff-state-indicator-off
		       `(display ,wl-biff-nomail-image ,@props))))
      (if props
	  (setq wl-modeline-biff-state-on
		(apply 'propertize wl-biff-state-indicator-on props)
		wl-modeline-biff-state-off
		(apply 'propertize wl-biff-state-indicator-off props))
	(setq wl-modeline-biff-state-on wl-biff-state-indicator-on
	      wl-modeline-biff-state-off wl-biff-state-indicator-off)))))

(defun wl-make-date-string (&optional time)
  (let ((system-time-locale "C"))
    (format-time-string "%a, %d %b %Y %T %z" time)))

(defalias 'wl-setup-folder 'wl-e21-setup-folder-toolbar)

(defalias 'wl-setup-summary 'wl-e21-setup-summary-toolbar)

(defun wl-message-define-keymap ()
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "D" 'wl-message-delete-current-part)
    (define-key keymap "l" 'wl-message-toggle-disp-summary)
    (define-key keymap "\C-c:d" 'wl-message-decrypt-pgp-nonmime)
    (define-key keymap "\C-c:v" 'wl-message-verify-pgp-nonmime)
    (define-key keymap "w" 'wl-draft)
    (define-key keymap [mouse-4] 'wl-message-wheel-down)
    (define-key keymap [mouse-5] 'wl-message-wheel-up)
    (define-key keymap [S-mouse-4] 'wl-message-wheel-down)
    (define-key keymap [S-mouse-5] 'wl-message-wheel-up)
    ;; Meadow2
    (define-key keymap [mouse-wheel1] 'wl-message-wheel-dispatcher)
    (define-key keymap [S-mouse-wheel1] 'wl-message-wheel-dispatcher)
    (set-keymap-parent wl-message-button-map keymap)
    (define-key wl-message-button-map
      [mouse-2] 'wl-message-button-dispatcher)
    keymap))

(defalias 'wl-setup-message 'wl-e21-setup-message-toolbar)

;; Wheel handling for Meadow2
(defun wl-message-wheel-dispatcher (event)
  (interactive "e")
  (if (< (nth 4 (nth 1 event)) 0)
      (wl-message-wheel-up event)
    (wl-message-wheel-down event)))

(defun wl-summary-wheel-dispatcher (event)
  (interactive "e")
  (if (< (nth 4 (nth 1 event)) 0)
      (if (memq 'shift (event-modifiers event))
	  (wl-summary-down)
	(wl-summary-next))
    (if (memq 'shift (event-modifiers event))
	(wl-summary-up)
      (wl-summary-prev))))

(defun wl-message-wheel-up (event)
  (interactive "e")
  (if (string-match (regexp-quote wl-message-buffer-name)
		    (regexp-quote (buffer-name)))
      (wl-message-next-page)
    (let ((cur-buf (current-buffer))
	  proceed)
      (save-selected-window
	(select-window (posn-window (event-start event)))
	(set-buffer cur-buf)
	(setq proceed (wl-message-next-page)))
      (if proceed
	  (if (memq 'shift (event-modifiers event))
	      (wl-summary-down t)
	    (wl-summary-next t))))))

(defun wl-message-wheel-down (event)
  (interactive "e")
  (if (string-match (regexp-quote wl-message-buffer-name)
		    (regexp-quote (buffer-name)))
      (wl-message-prev-page)
    (let ((cur-buf (current-buffer))
	  proceed)
      (save-selected-window
	(select-window (posn-window (event-start event)))
	(set-buffer cur-buf)
	(setq proceed (wl-message-prev-page)))
      (if proceed
	  (if (memq 'shift (event-modifiers event))
	      (wl-summary-up t)
	    (wl-summary-prev t))))))

(defun wl-draft-overload-menubar ()
  (let ((keymap (current-local-map)))
    (define-key keymap [menu-bar mail send]
      '("Send Message" . wl-draft-send-and-exit))
    (define-key keymap [menu-bar mail send-stay]
      '("Send, Keep Editing" . wl-draft-send))
    (define-key-after (lookup-key keymap [menu-bar mail])
      [mail-sep-send] '("--")
      'send-stay)
    (define-key keymap [menu-bar mail cancel]
      '("Kill Current Draft" . wl-draft-kill))
    (define-key-after (lookup-key keymap [menu-bar mail])
      [save] '("Save Draft and Exit" . wl-draft-save-and-exit)
      'cancel)
    (define-key-after (lookup-key keymap [menu-bar mail])
      [mail-sep-exit] '("--")
      'save)
    (define-key-after (lookup-key keymap [menu-bar mail])
      [preview] '("Preview Message" . wl-draft-preview-message)
      'mail-sep-exit)
    (define-key keymap [menu-bar mail yank]
      '("Cite Message" . wl-draft-yank-original))
    (define-key keymap [menu-bar mail signature]
      '("Insert Signature" . wl-draft-insert-signature))
    (define-key keymap [menu-bar headers fcc]
      '("Fcc" . wl-draft-fcc))))

(defun wl-draft-mode-setup ()
  (require 'derived)
  (define-derived-mode wl-draft-mode mail-mode "Draft"
    "draft mode for Wanderlust derived from mail mode.
See info under Wanderlust for full documentation.

Special commands:
\\{wl-draft-mode-map}"
    (setq font-lock-defaults nil)
    (if wl-draft-jit-highlight
      (jit-lock-register wl-draft-jit-highlight-function)
      (add-hook 'after-change-functions
                'wl-draft-idle-highlight-set-timer nil t))))

(defun wl-draft-key-setup ()
  (define-key wl-draft-mode-map "\C-c\C-y" 'wl-draft-yank-original)
  (define-key wl-draft-mode-map "\C-c\C-s" 'wl-draft-send)
  (define-key wl-draft-mode-map "\C-c\C-c" 'wl-draft-send-and-exit)
  (define-key wl-draft-mode-map "\C-c\C-z" 'wl-draft-save-and-exit)
  (define-key wl-draft-mode-map "\C-c\C-k" 'wl-draft-kill)
  (define-key wl-draft-mode-map "\C-l" 'wl-draft-highlight-and-recenter)
  (define-key wl-draft-mode-map "\C-i" 'wl-complete-field-body-or-tab)
  (define-key wl-draft-mode-map "\C-c\C-r" 'wl-draft-caesar-region)
  (define-key wl-draft-mode-map "\M-t" 'wl-toggle-plugged)
  (define-key wl-draft-mode-map "\C-c\C-o" 'wl-jump-to-draft-buffer)
  (define-key wl-draft-mode-map "\C-c\C-e" 'wl-draft-config-exec)
  (define-key wl-draft-mode-map "\C-c\C-j" 'wl-template-select)
  (define-key wl-draft-mode-map "\C-c\C-p" 'wl-draft-preview-message)
  (define-key wl-draft-mode-map "\C-c\C-a" 'wl-addrmgr)
;;;  (define-key wl-draft-mode-map "\C-x\C-s" 'wl-draft-save)
  (define-key wl-draft-mode-map "\C-xk"    'wl-draft-mimic-kill-buffer)
  (define-key wl-draft-mode-map "\C-c\C-d" 'wl-draft-elide-region)
  (define-key wl-draft-mode-map "\C-a" 'wl-draft-beginning-of-line)
  (define-key wl-draft-mode-map "\M-p" 'wl-draft-previous-history-element)
  (define-key wl-draft-mode-map "\M-n" 'wl-draft-next-history-element))

(defun wl-draft-overload-functions ()
  (wl-mode-line-buffer-identification)
;;;  (local-set-key "\C-c\C-s" 'wl-draft-send) ; override
  (wl-e21-setup-draft-toolbar)
  (wl-draft-overload-menubar))

(defalias 'wl-defface 'defface)

(defun wl-read-event-char (&optional prompt)
  "Get the next event."
  (let ((event (read-event prompt)))
    (cons (and (numberp event) event) event)))

(put 'wl-modeline-biff-state-on 'risky-local-variable t)
(put 'wl-modeline-biff-state-off 'risky-local-variable t)
(put 'wl-modeline-plug-state-on 'risky-local-variable t)
(put 'wl-modeline-plug-state-off 'risky-local-variable t)

(require 'product)
(product-provide (provide 'wl-e21) (require 'wl-version))

;;; wl-e21.el ends here
