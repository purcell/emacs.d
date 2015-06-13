;;; wl-folder.el --- Folder mode for Wanderlust.

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

(require 'elmo-vars)
(require 'elmo-util)
(require 'elmo)
(require 'wl-vars)
(require 'easymenu nil t) ; needed here.

(eval-when-compile
  (require 'cl)
  (require 'wl-util)
  (provide 'wl-folder)
  (require 'wl)
  (require 'elmo-nntp))

(defcustom wl-folder-init-hook nil
  "A hook called after folder initialization is finished."
  :type 'hook
  :group 'wl)

(defvar wl-folder-buffer-name "Folder")
(defvar wl-folder-entity nil)		; desktop entity.
(defvar wl-folder-group-alist nil)	; opened or closed
(defvar wl-folder-entity-id nil) ; id
(defvar wl-folder-entity-hashtb nil)
(defvar wl-folder-entity-id-name-hashtb nil)
(defvar wl-folder-elmo-folder-hashtb nil)  ; name => elmo folder structure

(defvar wl-folder-newsgroups-hashtb nil)
(defvar wl-folder-info-alist-modified nil)

(defvar wl-folder-mode-map nil)

(defvar wl-folder-buffer-disp-summary nil)
(defvar wl-folder-buffer-cur-entity-id nil)
(defvar wl-folder-buffer-last-visited-entity-id nil)
(defvar wl-folder-buffer-cur-path nil)
(defvar wl-folder-buffer-cur-point nil)

(make-variable-buffer-local 'wl-folder-buffer-disp-summary)
(make-variable-buffer-local 'wl-folder-buffer-cur-entity-id)
(make-variable-buffer-local 'wl-folder-buffer-last-visited-entity-id)
(make-variable-buffer-local 'wl-folder-buffer-cur-path)
(make-variable-buffer-local 'wl-folder-buffer-cur-point)

(defconst wl-folder-entity-regexp "^\\([ ]*\\)\\(\\[[\\+-]\\]\\)?\\([^\\[].+\\):[-*0-9]+/[-*0-9]+/[-*0-9]+")
(defconst wl-folder-group-regexp  "^\\([ ]*\\)\\[\\([\\+-]\\)\\]\\(.+\\):[0-9-]+/[0-9-]+/[0-9-]+\n")
;;				1:indent 2:opened 3:group-name
(defconst wl-folder-unsync-regexp ":[^0\\*][0-9]*/[0-9\\*-]+/[0-9\\*-]+$")

(defvar wl-folder-mode-menu-spec
  '("Folder"
    ["Enter Current Folder" wl-folder-jump-to-current-entity t]
    ["Prev Folder"          wl-folder-prev-entity t]
    ["Next Folder"          wl-folder-next-entity t]
    ["Check Current Folder" wl-folder-check-current-entity t]
    ["Sync Current Folder"  wl-folder-sync-current-entity t]
;;;    ["Drop Current Folder" wl-folder-drop-unsync-current-entity t]
    ["Prefetch Current Folder" wl-folder-prefetch-current-entity t]
    "----"
    ["Mark as Read all Current Folder" wl-folder-mark-as-read-all-current-entity t]
    ["Expire Current Folder" wl-folder-expire-current-entity t]
    "----"
    ["Go to Draft Folder" wl-folder-goto-draft-folder t]
    ["Empty trash" wl-folder-empty-trash t]
    ["Flush queue" wl-folder-flush-queue t]
    "----"
    ["Open All" wl-folder-open-all t]
    ["Open All Unread folder" wl-folder-open-all-unread-folder t]
    ["Close All" wl-folder-close-all t]
    ("Folder Manager"
     ["Add folder" wl-fldmgr-add t]
     ["Add group" wl-fldmgr-make-group t]
     ["Copy" wl-fldmgr-copy t]
     ["Cut" wl-fldmgr-cut t]
     ["Paste" wl-fldmgr-yank t]
     ["Set petname" wl-fldmgr-set-petname t]
     ["Rename" wl-fldmgr-rename t]
     ["Save" wl-fldmgr-save-folders t]
     "----"
     ["Unsubscribe" wl-fldmgr-unsubscribe t]
     ["Display all" wl-fldmgr-access-display-all t])
    "----"
    ["Write a message" wl-draft t]
    ["Write for current folder" wl-folder-write-current-folder t]
    "----"
    ["Wanderlust NEWS" wl-news t]
    "----"
    ["Toggle Plug Status" wl-toggle-plugged t]
    ["Change Plug Status" wl-plugged-change t]
    "----"
    ["Save Current Status"  wl-save t]
    ["Update Status"        wl-status-update t]
    ["Exit"                 wl-exit t]
    ))

(if wl-on-xemacs
    (defun wl-folder-setup-mouse ()
      (define-key wl-folder-mode-map 'button2 'wl-folder-click)
      (define-key wl-folder-mode-map 'button4 'wl-folder-prev-entity)
      (define-key wl-folder-mode-map 'button5 'wl-folder-next-entity)
      (define-key wl-folder-mode-map [(shift button4)]
	'wl-folder-prev-unread)
      (define-key wl-folder-mode-map [(shift button5)]
	'wl-folder-next-unread))
  (defun wl-folder-setup-mouse ()
    (define-key wl-folder-mode-map [mouse-2] 'wl-folder-click)
    (define-key wl-folder-mode-map [mouse-4] 'wl-folder-prev-entity)
    (define-key wl-folder-mode-map [mouse-5] 'wl-folder-next-entity)
    (define-key wl-folder-mode-map [S-mouse-4] 'wl-folder-prev-unread)
    (define-key wl-folder-mode-map [S-mouse-5] 'wl-folder-next-unread)))

(if wl-folder-mode-map
    nil
  (setq wl-folder-mode-map (make-sparse-keymap))
  (define-key wl-folder-mode-map " "    'wl-folder-jump-to-current-entity)
;;;  (define-key wl-folder-mode-map "\M- " 'wl-folder-open-close)
  (define-key wl-folder-mode-map "/"    'wl-folder-open-close)
  (define-key wl-folder-mode-map "\C-m" 'wl-folder-jump-to-current-entity)
  (define-key wl-folder-mode-map [(shift return)] 'wl-folder-jump-to-current-entity-with-arg)
  (define-key wl-folder-mode-map "\M-\C-m" 'wl-folder-update-recursive-current-entity)
  (define-key wl-folder-mode-map "rc"    'wl-folder-mark-as-read-all-region)
  (define-key wl-folder-mode-map "c"    'wl-folder-mark-as-read-all-current-entity)
  (define-key wl-folder-mode-map "g"    'wl-folder-goto-folder)
  (define-key wl-folder-mode-map "G"    'wl-folder-goto-folder-sticky)
  (define-key wl-folder-mode-map "j"    'wl-folder-jump-to-current-entity)
  (define-key wl-folder-mode-map "\C-i" 'wl-folder-revisit-last-visited-folder)
  (define-key wl-folder-mode-map "w"    'wl-draft)
  (define-key wl-folder-mode-map "W"    'wl-folder-write-current-folder)
  (define-key wl-folder-mode-map "\C-c\C-o" 'wl-jump-to-draft-buffer)
  (define-key wl-folder-mode-map "\C-c\C-a" 'wl-addrmgr)
  (define-key wl-folder-mode-map "\C-c\C-p" 'wl-folder-jump-to-previous-summary)
  (define-key wl-folder-mode-map "\C-c\C-n" 'wl-folder-jump-to-next-summary)
  (define-key wl-folder-mode-map "rS"   'wl-folder-sync-region)
  (define-key wl-folder-mode-map "S"    'wl-folder-sync-current-entity)
  (define-key wl-folder-mode-map "rs"   'wl-folder-check-region)
  (define-key wl-folder-mode-map "s"    'wl-folder-check-current-entity)
  (define-key wl-folder-mode-map "I"    'wl-folder-prefetch-current-entity)
;;;  (define-key wl-folder-mode-map "D"    'wl-folder-drop-unsync-current-entity)
  (define-key wl-folder-mode-map "p"    'wl-folder-prev-entity)
  (define-key wl-folder-mode-map "n"    'wl-folder-next-entity)
  (define-key wl-folder-mode-map "v"    'wl-folder-toggle-disp-summary)
  (define-key wl-folder-mode-map "P"    'wl-folder-prev-unread)
  (define-key wl-folder-mode-map "N"    'wl-folder-next-unread)
  (define-key wl-folder-mode-map "J"    'wl-folder-jump-folder)
  (define-key wl-folder-mode-map "f"    'wl-folder-goto-first-unread-folder)
  (define-key wl-folder-mode-map "o"    'wl-folder-open-all-unread-folder)
  (define-key wl-folder-mode-map "["    'wl-folder-open-all)
  (define-key wl-folder-mode-map "]"    'wl-folder-close-all)
  (define-key wl-folder-mode-map "e"    'wl-folder-expire-current-entity)
  (define-key wl-folder-mode-map "E"    'wl-folder-empty-trash)
  (define-key wl-folder-mode-map "F"    'wl-folder-flush-queue)
  (define-key wl-folder-mode-map "V"    'wl-folder-virtual)
  (define-key wl-folder-mode-map "?"    'wl-folder-pick)
  (define-key wl-folder-mode-map "q"    'wl-exit)
  (define-key wl-folder-mode-map "z"    'wl-folder-suspend)
  (define-key wl-folder-mode-map "x"    'wl-execute-temp-marks)
  (define-key wl-folder-mode-map "\M-t" 'wl-toggle-plugged)
  (define-key wl-folder-mode-map "\C-t" 'wl-plugged-change)
  (define-key wl-folder-mode-map "<"    'beginning-of-buffer)
  (define-key wl-folder-mode-map ">"    'end-of-buffer)
  ;; wl-fldmgr
  (define-key wl-folder-mode-map "m"    'wl-fldmgr-mode-map)
  (define-key wl-folder-mode-map "*"    'wl-fldmgr-make-multi)
  (define-key wl-folder-mode-map "+"    'wl-fldmgr-make-group)
  (define-key wl-folder-mode-map "|"    'wl-fldmgr-make-filter)
  (define-key wl-folder-mode-map "\M-c" 'wl-fldmgr-copy)
  (define-key wl-folder-mode-map "\M-w" 'wl-fldmgr-copy-region)
  (define-key wl-folder-mode-map "\C-k" 'wl-fldmgr-cut)
  (define-key wl-folder-mode-map "\C-w" 'wl-fldmgr-cut-region)
  (define-key wl-folder-mode-map "\C-y" 'wl-fldmgr-yank)
  (define-key wl-folder-mode-map "R"    'wl-fldmgr-rename)
  (define-key wl-folder-mode-map "u"    'wl-fldmgr-unsubscribe)
  (define-key wl-folder-mode-map "ru"   'wl-fldmgr-unsubscribe-region)
  (define-key wl-folder-mode-map "U"    'wl-fldmgr-unsubscribe-region)
  (define-key wl-folder-mode-map "l"    'wl-fldmgr-access-display-normal)
  (define-key wl-folder-mode-map "L"    'wl-fldmgr-access-display-all)
  (define-key wl-folder-mode-map "Z"    'wl-status-update)
  (define-key wl-folder-mode-map "\C-x\C-s" 'wl-save)
  (define-key wl-folder-mode-map "\M-s"     'wl-save)
  (define-key wl-folder-mode-map "\C-xk"    'wl-folder-mimic-kill-buffer)
  (define-key wl-folder-mode-map "\M-\C-a"
    'wl-folder-goto-top-of-current-folder)
  (define-key wl-folder-mode-map "\M-\C-e"
    'wl-folder-goto-bottom-of-current-folder)

  (wl-folder-setup-mouse)
  (easy-menu-define
   wl-folder-mode-menu
   wl-folder-mode-map
   "Menu used in Folder mode."
   wl-folder-mode-menu-spec))

(defun wl-folder-unread-regex (group)
  (concat "^[ ]*.+:[0-9\\*-]+/[^0\\*][0-9]*/[0-9\\*-]+$"
	  (if group
	      "\\|^[ ]*\\[[+-]\\]"
	    "")))

(defun wl-folder-buffer-group-p ()
  (get-text-property (point) 'wl-folder-is-group))

(defun wl-folder-buffer-search-group (group)
  (let ((prev-point (point))
	(group-regexp (concat
		       "^\\([ \t]*\\)\\[[\\+-]\\]"
		       (regexp-quote group) ":[-0-9-]+/[0-9-]+/[0-9-]+")))
    (or (catch 'found
	  (while (re-search-forward group-regexp nil t)
	    (if (wl-folder-buffer-group-p)
		(throw 'found (point)))))
	(progn ; not found
	  (goto-char prev-point)
	  nil))))

(defun wl-folder-buffer-search-entity (folder &optional searchname)
  (let ((regexp (concat
		 "^[ \t]*"
		 (regexp-quote (or searchname (wl-folder-get-petname folder)))
		 ":[-0-9\\*-]+/[0-9\\*-]+/[0-9\\*-]+"))
	case-fold-search
	result)
    (catch 'found
      (while (setq result (re-search-forward regexp nil t))
	(when (string= (wl-folder-get-entity-from-buffer) folder)
	  (throw 'found result))))))

(defsubst wl-folder-get-folder-name-by-id (entity-id &optional hashtb)
  (and (numberp entity-id)
       (elmo-get-hash-val (format "#%d" entity-id)
			  (or hashtb wl-folder-entity-id-name-hashtb))))

(defsubst wl-folder-set-id-name (entity-id entity &optional hashtb)
  (and (numberp entity-id)
       (elmo-set-hash-val (format "#%d" entity-id)
			  entity (or hashtb wl-folder-entity-id-name-hashtb))))

(defmacro wl-folder-get-entity-id (entity)
  `(get-text-property 0 'wl-folder-entity-id ,entity))

(defun wl-folder-get-entity-from-buffer (&optional getid)
  (let ((id (get-text-property (point)
			       'wl-folder-entity-id)))
    (if getid
	id
      (wl-folder-get-folder-name-by-id id))))

(defmacro wl-folder-entity-exists-p (entity &optional hashtb)
  `(let ((sym (intern-soft ,entity (or ,hashtb wl-folder-entity-hashtb))))
     (and sym (boundp sym))))

(defmacro wl-folder-clear-entity-info (entity &optional hashtb)
  `(elmo-clear-hash-val ,entity (or ,hashtb wl-folder-entity-hashtb)))

(defmacro wl-folder-get-entity-info (entity &optional hashtb)
  `(elmo-get-hash-val ,entity (or ,hashtb wl-folder-entity-hashtb)))

(defmacro wl-folder-set-entity-info (entity value &optional hashtb)
  `(let* ((hashtb (or ,hashtb wl-folder-entity-hashtb))
	  (info (wl-folder-get-entity-info ,entity hashtb)))
     (elmo-set-hash-val (elmo-string ,entity)
			(if (< (length ,value) 4)
			    (append ,value (list (nth 3 info)))
			  ,value)
			hashtb)))

(defun wl-folder-persistent-p (folder)
  (or (and (wl-folder-search-entity-by-name folder wl-folder-entity
					    'folder)
	   t) ; on Folder mode.
      (catch 'found
	(let ((li wl-save-folder-list))
	  (while li
	    (if (string-match (car li) folder)
		(throw 'found t))
	    (setq li (cdr li)))))
      (not (catch 'found
	     (let ((li wl-no-save-folder-list))
	       (while li
		 (if (string-match (car li) folder)
		     (throw 'found t))
		 (setq li (cdr li))))))))

;;; ELMO folder structure with cache.
(defmacro wl-folder-elmo-folder-cache-get (name &optional hashtb)
  "Returns a elmo folder structure associated with NAME from HASHTB.
Default HASHTB is `wl-folder-elmo-folder-hashtb'."
  `(elmo-get-hash-val ,name
		      (or ,hashtb wl-folder-elmo-folder-hashtb)))

(defmacro wl-folder-elmo-folder-cache-put (name folder &optional hashtb)
  "Get folder elmo folder structure on HASHTB for folder with NAME.
Default HASHTB is `wl-folder-elmo-folder-hashtb'."
  `(elmo-set-hash-val ,name ,folder
		      (or ,hashtb wl-folder-elmo-folder-hashtb)))

(defun wl-draft-get-folder ()
  "A function to obtain `opened' draft elmo folder structure."
  (if (and wl-draft-folder-internal
	   (string= (elmo-folder-name-internal wl-draft-folder-internal)
		    wl-draft-folder))
      wl-draft-folder-internal
    (setq wl-draft-folder-internal (wl-folder-make-elmo-folder
				    wl-draft-folder))
    (wl-folder-confirm-existence wl-draft-folder-internal)
    (elmo-folder-open wl-draft-folder-internal 'load-msgdb)
    wl-draft-folder-internal))

(defun wl-folder-mime-charset (folder-name)
  (or (wl-get-assoc-list-value wl-folder-mime-charset-alist folder-name)
      wl-mime-charset))

(defun wl-folder-make-elmo-folder (folder-name)
  (elmo-make-folder folder-name nil (wl-folder-mime-charset folder-name)))

(defsubst wl-folder-get-elmo-folder (entity &optional no-cache)
  "Get elmo folder structure from ENTITY."
  (let ((name (elmo-string entity)))
    (if no-cache
	(wl-folder-make-elmo-folder name)
      (if (string= name wl-draft-folder)
	  (wl-draft-get-folder)
	(or (wl-folder-elmo-folder-cache-get name)
	    (let ((folder (wl-folder-make-elmo-folder name)))
	      (wl-folder-elmo-folder-cache-put name folder)
	      folder))))))

(defsubst wl-folder-put-folder-property (beg end id is-group &optional object)
  (put-text-property beg end 'wl-folder-entity-id id object)
  (put-text-property beg end 'wl-folder-is-group is-group object))

(defun wl-folder-prev-entity ()
  (interactive)
  (forward-line -1))

(defun wl-folder-next-entity ()
  (interactive)
  (forward-line))

(defun wl-folder-prev-entity-skip-invalid (&optional hereto)
  "Move to previous entity. skip unsubscribed or removed entity."
  (interactive)
  (if hereto
      (end-of-line))
  (if (re-search-backward wl-folder-entity-regexp nil t)
      (beginning-of-line)
    (goto-char (point-min))))

(defun wl-folder-next-entity-skip-invalid (&optional hereto)
  "Move to next entity. skip unsubscribed or removed entity."
  (interactive)
  (beginning-of-line)
  (if (not hereto)
      (forward-line))
  (if (re-search-forward wl-folder-entity-regexp nil t)
      (beginning-of-line)
    (goto-char (point-max))))

(defun wl-folder-search-group-entity-by-name (name entity)
  (wl-folder-search-entity-by-name name entity 'group))

(defun wl-folder-search-entity-by-name (name entity &optional type)
  (let ((entities (list entity))
	entity-stack)
    (catch 'done
      (while entities
	(setq entity (wl-pop entities))
	(cond
	 ((consp entity)
	  (if (and (not (eq type 'folder))
		   (string= name (car entity)))
	      (throw 'done entity))
	  (and entities
	       (wl-push entities entity-stack))
	  (setq entities (nth 2 entity)))
	 ((and (not (eq type 'group))
	       (stringp entity))
	  (if (string= name entity)
	      (throw 'done entity))))
	(unless entities
	  (setq entities (wl-pop entity-stack)))))))

(defun wl-folder-search-entity-list-by-name (name entity &optional get-id)
  (let ((entities (list entity))
	entity-stack ret-val)
    (while entities
      (setq entity (wl-pop entities))
      (cond
       ((consp entity)
	(and entities
	     (wl-push entities entity-stack))
	(setq entities (nth 2 entity)))
       ((stringp entity)
	(if (string= name entity)
	    (wl-append ret-val (if get-id
				   (list (wl-folder-get-entity-id entity))
				 (list entity))))))
      (unless entities
	(setq entities (wl-pop entity-stack))))
    ret-val))

(defun wl-folder-get-prev-folder (id &optional unread)
  (let ((name (if (stringp id)
		  id
		(wl-folder-get-folder-name-by-id id)))
	entity entity-stack last-entity finfo
	(entities (list wl-folder-entity)))
    (catch 'done
      (while entities
	(setq entity (wl-pop entities))
	(cond
	 ((consp entity)
;;;	  (if (and (string= name (car entity))
;;;		   (eq id (wl-folder-get-entity-id (car entity))))
;;;	      (throw 'done last-entity))
	  (and entities
	       (wl-push entities entity-stack))
	  (setq entities (nth 2 entity)))
	 ((stringp entity)
	  (if (and (string= name entity)
		   (eq id (wl-folder-get-entity-id entity)))
	      (throw 'done last-entity))
	  (if (or (not unread)
		  (and (setq finfo (wl-folder-get-entity-info entity))
		       (and (nth 0 finfo)(nth 1 finfo))
		       (> (+ (nth 0 finfo)(nth 1 finfo)) 0)))
	      (setq last-entity entity))))
	(unless entities
	  (setq entities (wl-pop entity-stack)))))))

(defun wl-folder-get-next-folder (id &optional unread)
  (let ((name (if (stringp id)
		  id
		(wl-folder-get-folder-name-by-id id)))
	entity entity-stack found finfo
	(entities (list wl-folder-entity)))
    (catch 'done
      (while entities
	(setq entity (wl-pop entities))
	(cond
	 ((consp entity)
;;;	  (if (and (string= name (car entity))
;;;		   (eq id (wl-folder-get-entity-id (car entity))))
;;;	      (setq found t))
	  (and entities
	       (wl-push entities entity-stack))
	  (setq entities (nth 2 entity)))
	 ((stringp entity)
	  (if found
	      (when (or (not unread)
			(and (setq finfo (wl-folder-get-entity-info entity))
			     (and (nth 0 finfo)(nth 1 finfo))
			     (> (+ (nth 0 finfo)(nth 1 finfo)) 0)))
		(throw 'done entity))
	    (if (and (string= name entity)
		     (eq id (wl-folder-get-entity-id entity)))
		(setq found t)))))
	(unless entities
	  (setq entities (wl-pop entity-stack)))))))

(defun wl-folder-flush-queue ()
  "Flush queue."
  (interactive)
  (let ((cur-buf (current-buffer))
	(wl-auto-select-first nil)
	(wl-plugged t)
	emptied)
    (if elmo-enable-disconnected-operation
	(elmo-dop-queue-flush))
    (if (not (elmo-folder-list-messages
	      (wl-folder-get-elmo-folder wl-queue-folder)))
	(message "No sending queue exists.")
      (if wl-stay-folder-window
	  (wl-folder-select-buffer
	   (wl-summary-get-buffer-create wl-queue-folder)))
      (wl-summary-goto-folder-subr wl-queue-folder 'force-update nil)
      (unwind-protect
	  (wl-draft-queue-flush)
	(if (get-buffer-window cur-buf)
	    (select-window (get-buffer-window cur-buf)))
	(set-buffer cur-buf)
	(if wl-stay-folder-window
	    (wl-folder-toggle-disp-summary 'off wl-queue-folder)
	  (switch-to-buffer cur-buf))))))

(defun wl-folder-set-persistent-mark (folder number flag)
  "Set a persistent mark which corresponds to the specified flag on message."
  (let ((buffer (wl-summary-get-buffer folder)))
    (if buffer
	(with-current-buffer buffer
	  (wl-summary-set-persistent-mark flag number))
      ;; Parent buffer does not exist.
      (let ((elmo-folder (wl-folder-get-elmo-folder folder)))
	(elmo-folder-open elmo-folder 'load-msgdb)
	(elmo-folder-set-flag elmo-folder (list wl-draft-parent-number) flag)
	(elmo-folder-close elmo-folder)))))

(defun wl-folder-empty-trash ()
  "Empty trash."
  (interactive)
  (let ((cur-buf (current-buffer))
	(wl-auto-select-first nil)
	trash-buf emptied)
    (wl-summary-goto-folder-subr wl-trash-folder 'force-update)
    (setq trash-buf (wl-summary-get-buffer-create wl-trash-folder))
    (if wl-stay-folder-window
	(wl-folder-select-buffer trash-buf)
      (switch-to-buffer trash-buf))
    (unwind-protect
	(setq emptied (wl-summary-delete-all-msgs))
      (when emptied
	(setq wl-thread-entities nil
	      wl-thread-entity-list nil)
	(if wl-summary-cache-use (wl-summary-save-view-cache))
	(elmo-folder-commit wl-summary-buffer-elmo-folder))
      (if (get-buffer-window cur-buf)
	  (select-window (get-buffer-window cur-buf)))
      (set-buffer cur-buf)
      (if emptied
	  (wl-folder-set-folder-updated wl-trash-folder '(0 0 0)))
      (if wl-stay-folder-window
	  (wl-folder-toggle-disp-summary 'off wl-trash-folder)
	(switch-to-buffer cur-buf))
      (and trash-buf
	   (kill-buffer trash-buf)))))

(defun wl-folder-goto-top-of-current-folder (&optional arg)
  "Move backward to the top of the current folder group.
Optional argument ARG is repeart count."
  (interactive "P")
  (if (re-search-backward
       "^ *\\[[\\+-]\\]" nil t (if arg (prefix-numeric-value arg)))
      (beginning-of-line)
    (goto-char (point-min))))

(defun wl-folder-goto-bottom-of-current-folder (indent)
  "Move forward to the bottom of the current folder group."
  (interactive
   (let ((indent
	  (save-excursion
	    (beginning-of-line)
	    (if (looking-at "^ *")
		(buffer-substring (match-beginning 0)(1- (match-end 0)))
	      ""))))
     (list indent)))
  (if (catch 'done
	(while (re-search-forward "^ *" nil t)
	  (if (<= (length (match-string 0))
		  (length indent))
	      (throw 'done nil)))
	(throw 'done t))
      (goto-char (point-max))))

(defun wl-folder-update-group (entity diffs &optional is-group)
  (save-excursion
    (let ((path (wl-folder-get-path
		 wl-folder-entity
		 (wl-folder-get-entity-id entity)
		 entity)))
      (if (not is-group)
	  ;; delete itself from path
	  (setq path (delete (nth (- (length path) 1) path) path)))
      (goto-char (point-min))
      (catch 'done
	(while path
	  ;; goto the path line.
	  (if (or (eq (car path) 0) ; update desktop
		  (wl-folder-buffer-search-group
		   (wl-folder-get-petname
		    (if (stringp (car path))
			(car path)
		      (wl-folder-get-folder-name-by-id
		       (car path))))))
	      ;; update it.
	      (wl-folder-update-diff-line diffs)
	    (throw 'done t))
	  (setq path (cdr path)))))))

(defun wl-folder-maybe-load-folder-list (entity)
  (when (null (caddr entity))
    (setcdr (cdr entity)
	    (elmo-msgdb-flist-load (car entity)))
    (when (cddr entity)
      (let (diffs)
	(save-excursion
	  (wl-folder-entity-assign-id entity
				      wl-folder-entity-id-name-hashtb
				      t)
	  (setq diffs (wl-fldmgr-add-entity-hashtb (list entity)))
	  (unless (equal diffs '(0 0 0))
	    (wl-folder-update-group (car entity) diffs t)))))))

(defsubst wl-folder-force-fetch-p (entity)
  (cond
   ((consp wl-force-fetch-folders)
    (wl-string-match-member entity wl-force-fetch-folders))
   (t
    wl-force-fetch-folders)))

(defun wl-folder-jump-to-current-entity-with-arg ()
  (interactive)
  (wl-folder-jump-to-current-entity t))

(defun wl-folder-jump-to-current-entity (&optional arg)
  "Enter the current folder.  If optional ARG exists, update folder list."
  (interactive "P")
  (let ((fld-name (wl-folder-get-entity-from-buffer))
	entity beg end indent opened err)
    (unless fld-name
      (error "No folder"))
    (beginning-of-line)
    (if (and (wl-folder-buffer-group-p)
	     (looking-at wl-folder-group-regexp))
	;; folder group
	(save-excursion
	  (setq indent (wl-match-buffer 1))
	  (setq opened (wl-match-buffer 2))
	  (if (string= opened "+")
	      (progn
		(setq entity (wl-folder-search-group-entity-by-name
			      fld-name
			      wl-folder-entity))
		(setq beg (point))
		(if arg
		    (wl-folder-update-recursive-current-entity entity)
		  ;; insert as opened
		  (setcdr (assoc (car entity) wl-folder-group-alist) t)
		  (if (eq 'access (cadr entity))
		      (wl-folder-maybe-load-folder-list entity))
		  ;(condition-case errobj
		  (progn
		    (if (or (wl-folder-force-fetch-p (car entity))
			    (and
			     (eq 'access (cadr entity))
			     (null (caddr entity))))
			(wl-folder-update-newest indent entity)
		      (wl-folder-insert-entity indent entity))
		    (wl-highlight-folder-path wl-folder-buffer-cur-path))
;;;		  (quit
;;;		   (setq err t)
;;;		   (setcdr (assoc fld-name wl-folder-group-alist) nil))
;;;		  (error
;;;		   (elmo-display-error errobj t)
;;;		   (ding)
;;;		   (setq err t)
;;;		   (setcdr (assoc fld-name wl-folder-group-alist) nil)))
		  (if (not err)
		      (let ((buffer-read-only nil))
			(delete-region (save-excursion (beginning-of-line)
						       (point))
				       (save-excursion (end-of-line)
						       (+ 1 (point))))))))
	    (setq beg (point))
	    (end-of-line)
	    (save-match-data
	      (setq end
		    (progn (wl-folder-goto-bottom-of-current-folder indent)
			   (beginning-of-line)
			   (point))))
	    (setq entity (wl-folder-search-group-entity-by-name
			  fld-name
			  wl-folder-entity))
	    (let ((buffer-read-only nil))
	      (delete-region beg end))
	    (setcdr (assoc (car entity) wl-folder-group-alist) nil)
	    (wl-folder-insert-entity indent entity) ; insert entity
	    (forward-line -1)
	    (wl-highlight-folder-path wl-folder-buffer-cur-path)
;;;	    (wl-delete-all-overlays)
;;;	    (wl-highlight-folder-current-line)
	    ))
      ;; ordinal folder
      (wl-folder-set-current-entity-id
       (get-text-property (point) 'wl-folder-entity-id))
      (setq fld-name (wl-folder-get-folder-name-by-id
		      wl-folder-buffer-cur-entity-id))
      (let ((summary-buf (wl-summary-get-buffer-create fld-name arg))
	    error-selecting)
	(if (or wl-stay-folder-window wl-summary-use-frame)
	    (wl-folder-select-buffer summary-buf)
	  (if (and summary-buf
		   (get-buffer-window summary-buf))
	      (delete-window)))
	(wl-summary-goto-folder-subr fld-name
				     (wl-summary-get-sync-range
				      (wl-folder-get-elmo-folder fld-name))
				     nil arg t))))
  (set-buffer-modified-p nil))

(defun wl-folder-close-entity (entity)
  (let ((entities (list entity))
	entity-stack)
    (while entities
      (setq entity (wl-pop entities))
      (cond
       ((consp entity)
	(setcdr (assoc (car entity) wl-folder-group-alist) nil)
	(and entities
	     (wl-push entities entity-stack))
	(setq entities (nth 2 entity))))
      (unless entities
	(setq entities (wl-pop entity-stack))))))

(defun wl-folder-update-recursive-current-entity (&optional entity)
  (interactive)
  (beginning-of-line)
  (when (and (wl-folder-buffer-group-p)
	     (looking-at wl-folder-group-regexp))
    (cond
     ((string= (wl-match-buffer 2) "+")
      (save-excursion
	(if entity ()
	  (setq entity
		(wl-folder-search-group-entity-by-name
		 (wl-folder-get-entity-from-buffer)
		 wl-folder-entity)))
	(let ((inhibit-read-only t)
	      (entities (list entity))
	      entity-stack err indent)
	  (while (and entities (not err))
	    (setq entity (wl-pop entities))
	    (cond
	     ((consp entity)
	      (wl-folder-close-entity entity)
	      (setcdr (assoc (car entity) wl-folder-group-alist) t)
	      (unless (wl-folder-buffer-search-group
		       (wl-folder-get-petname (car entity)))
		(error "%s: not found group" (car entity)))
	      (setq indent (wl-match-buffer 1))
	      (if (eq 'access (cadr entity))
		  (wl-folder-maybe-load-folder-list entity))
	      (beginning-of-line)
	      (setq err nil)
	      (save-excursion
		(condition-case errobj
		    (wl-folder-update-newest indent entity)
		  (quit
		   (setq err t)
		   (setcdr (assoc (car entity) wl-folder-group-alist) nil))
		  (error
		   (elmo-display-error errobj t)
		   (ding)
		   (setq err t)
		   (setcdr (assoc (car entity) wl-folder-group-alist) nil)))
		(if (not err)
		    (delete-region (save-excursion (beginning-of-line)
						   (point))
				   (save-excursion (end-of-line)
						   (+ 1 (point))))))
	      ;;
	      (and entities
		   (wl-push entities entity-stack))
	      (setq entities (nth 2 entity))))
	    (unless entities
	      (setq entities (wl-pop entity-stack)))))
	(set-buffer-modified-p nil)))
     (t
      (wl-folder-jump-to-current-entity)))))

(defun wl-folder-no-auto-check-folder-p (folder)
  (if (stringp folder)
      (if (catch 'found
	    (let ((li wl-auto-check-folder-list))
	      (while li
		(if (string-match (car li) folder)
		    (throw 'found t))
		(setq li (cdr li)))))
	  nil
	(catch 'found
	  (let ((li wl-auto-uncheck-folder-list))
	    (while li
	      (if (string-match (car li) folder)
		  (throw 'found t))	; no check!
	      (setq li (cdr li))))))))

(defsubst wl-folder-add-folder-info (pre-value value)
  (list
   (+ (or (nth 0 pre-value) 0) (or (nth 0 value) 0))
   (+ (or (nth 1 pre-value) 0) (or (nth 1 value) 0))
   (+ (or (nth 2 pre-value) 0) (or (nth 2 value) 0))))

(defun wl-folder-check-entity (entity &optional auto)
  "Check unsync message number."
  (let ((start-pos (point))
	ret-val)
    (run-hooks 'wl-folder-check-entity-pre-hook)
    (if (and (consp entity)		;; group entity
	     wl-folder-check-async)	;; very fast
	(setq ret-val (wl-folder-check-entity-async entity auto))
      (save-excursion
	(cond
	 ((consp entity)
	  (let ((flist (if auto
			   (elmo-delete-if
			    'wl-folder-no-auto-check-folder-p
			    (nth 2 entity))
			 (nth 2 entity)))
		(wl-folder-check-entity-pre-hook nil)
		(wl-folder-check-entity-hook nil)
		new unread all)
	    (while flist
	      (setq ret-val
		    (wl-folder-add-folder-info
		     ret-val
		     (wl-folder-check-entity (car flist))))
	      (setq flist (cdr flist)))
;;;	    (wl-folder-buffer-search-entity (car entity))
;;;	    (wl-folder-update-line ret-val)
	    ))
	 ((stringp entity)
	  (message "Checking \"%s\"" entity)
	  (setq ret-val (wl-folder-check-one-entity
			 entity))
	  (goto-char start-pos)
	  (sit-for 0))
	 (t
	  (message "Uncheck(unplugged) \"%s\"" entity)))))
    (if ret-val
	(message "Checking \"%s\" is done"
		 (if (consp entity) (car entity) entity)))
    (run-hooks 'wl-folder-check-entity-hook)
    ret-val))

(defun wl-folder-check-one-entity (entity &optional biff)
  (let* ((folder (wl-folder-get-elmo-folder entity biff))
	 (nums (condition-case err
		   (progn
		     (if biff (elmo-folder-set-biff-internal folder t))
		     (if (wl-string-match-member entity wl-strict-diff-folders)
			 (elmo-strict-folder-diff folder)
		       (elmo-folder-diff folder)))
		 (elmo-open-error
		  (signal (car err) (cdr err)))
		 (error
		  ;; maybe not exist folder.
		  (if (not (elmo-folder-exists-p folder))
		      (wl-folder-create-subr folder)
		    (signal (car err) (cdr err))))))
	 (new    (elmo-diff-new nums))
	 (unread (elmo-diff-unread nums))
	 (all    (elmo-diff-all nums)))
    (if (and (eq wl-folder-notify-deleted 'sync)
	     (or (and new    (> 0 new))
		 (and unread (> 0 unread))
		 (and all    (> 0 all))))
	(progn
	  (wl-folder-sync-entity entity)
	  (setq nums (elmo-folder-diff folder)
		new    (elmo-diff-new nums)
		unread (elmo-diff-unread nums)
		all    (elmo-diff-all nums)))
      (unless wl-folder-notify-deleted
	(setq new    (and new    (max 0 new))
	      unread (and unread (max 0 unread))
	      all    (and all    (max 0 all))))
      (setq unread (if unread
		       (- unread (or new 0))
		     (or (elmo-folder-get-info-unread folder)
			 (cdr (assq 'unread (elmo-folder-count-flags folder)))
			 0)))
      (wl-folder-entity-hashtb-set wl-folder-entity-hashtb entity
				   (list new unread all)
				   (get-buffer wl-folder-buffer-name)))
    (setq wl-folder-info-alist-modified t)
    (sit-for 0)
    (list new unread all)))

(defun wl-folder-check-entity-async (entity &optional auto)
  (let ((elmo-nntp-groups-async t)
	(elist (if auto
		   (elmo-delete-if
		    'wl-folder-no-auto-check-folder-p
		    (wl-folder-get-entity-list entity))
		 (wl-folder-get-entity-list entity)))
	(nntp-connection-keys nil)
	name folder folder-list
	sync-folder-list
	async-folder-list
	server
	ret-val)
    (while elist
      (setq folder (wl-folder-get-elmo-folder (car elist)))
      (if (not (elmo-folder-plugged-p folder))
	  (message "Uncheck \"%s\"" (car elist))
	(setq folder-list
	      (elmo-folder-get-primitive-list folder))
	(cond ((elmo-folder-contains-type folder 'nntp)
	       (wl-append async-folder-list (list folder))
	       (while folder-list
		 (when (eq (elmo-folder-type-internal (car folder-list))
			   'nntp)
		   (when (not (string=
			       server
			       (elmo-net-folder-server-internal
				(car folder-list))))
		     (setq server (elmo-net-folder-server-internal
				   (car folder-list)))
		     (message "Checking on \"%s\"" server))
		   (setq nntp-connection-keys
			 (elmo-nntp-get-folders-info-prepare
			  (car folder-list)
			  nntp-connection-keys)))
		 (setq folder-list (cdr folder-list))))
	      (t
	       (wl-append sync-folder-list (list folder)))))
      (setq elist (cdr elist)))
    ;; check local entity at first
    (while (setq folder (pop sync-folder-list))
      (if (not (elmo-folder-plugged-p folder))
	  (message "Uncheck \"%s\"" (elmo-folder-name-internal folder))
	(message "Checking \"%s\"" (elmo-folder-name-internal folder))
	(setq ret-val
	      (wl-folder-add-folder-info
	       ret-val
	       (wl-folder-check-one-entity (elmo-folder-name-internal
					    folder))))
;;;	(sit-for 0)
	))
    ;; check network entity at last
    (when async-folder-list
      (elmo-nntp-get-folders-info nntp-connection-keys)
      (while (setq folder (pop async-folder-list))
	(if (not (elmo-folder-plugged-p folder))
	    (message "Uncheck \"%s\"" (elmo-folder-name-internal folder))
	  (message "Checking \"%s\"" (elmo-folder-name-internal folder))
	  (setq ret-val
		(wl-folder-add-folder-info
		 ret-val
		 (wl-folder-check-one-entity (elmo-folder-name-internal
					      folder))))
;;;	  (sit-for 0)
	  )))
    ret-val))

;;
(defun wl-folder-resume-entity-hashtb-by-finfo (entity-hashtb info-alist)
  "Resume unread info for entity alist."
  (let (info)
    (while info-alist
      (setq info (nth 1 (car info-alist)))
      (wl-folder-set-entity-info (caar info-alist)
				 (list (nth 2 info)(nth 3 info)(nth 1 info))
				 entity-hashtb)
      (setq info-alist (cdr info-alist)))))

(defun wl-folder-move-path (path)
  (let ((fp (if (consp path)
		path
	      ;; path is entity-id
	      (wl-folder-get-path wl-folder-entity path))))
    (goto-char (point-min))
    (while (and fp
		(not (eobp)))
      (when (equal (car fp)
		   (wl-folder-get-entity-from-buffer t))
	(setq fp (cdr fp))
	(setq wl-folder-buffer-cur-point (point)))
      (forward-line))
    (and wl-folder-buffer-cur-point
	 (goto-char wl-folder-buffer-cur-point))))

(defun wl-folder-set-current-entity-id (entity-id)
  (let* ((buf (get-buffer wl-folder-buffer-name))
	 (buf-win (get-buffer-window buf)))
    (if buf
	(save-current-buffer
	  (save-selected-window
	    (if buf-win
		(select-window buf-win)
	      (set-buffer buf))
	    (when (and wl-folder-buffer-cur-entity-id
		       (not (eq wl-folder-buffer-cur-entity-id entity-id)))
	      (setq wl-folder-buffer-last-visited-entity-id wl-folder-buffer-cur-entity-id))
	    (setq wl-folder-buffer-cur-entity-id entity-id)
	    (setq wl-folder-buffer-cur-path
		  (wl-folder-get-path wl-folder-entity entity-id))
	    (wl-highlight-folder-path wl-folder-buffer-cur-path)
	    (and wl-folder-move-cur-folder
		 wl-folder-buffer-cur-point
		 (goto-char wl-folder-buffer-cur-point)))))))

(defun wl-folder-check-current-entity ()
  "Check folder at position.
If current line is group folder, check all sub entries."
  (interactive)
  (let* ((entity-name (wl-folder-get-entity-from-buffer))
	 (group (wl-folder-buffer-group-p))
	 (desktop (string= entity-name wl-folder-desktop-name)))
    (when entity-name
      (wl-folder-check-entity
       (if group
	   (wl-folder-search-group-entity-by-name entity-name
						  wl-folder-entity)
	 entity-name)
       desktop))))

(defun wl-folder-sync-entity (entity &optional unread-only)
  "Synchronize the msgdb of ENTITY."
  (cond
   ((consp entity)
    (let ((flist (nth 2 entity)))
      (while flist
	(wl-folder-sync-entity (car flist) unread-only)
	(setq flist (cdr flist)))))
   ((stringp entity)
    (let* ((folder (wl-folder-get-elmo-folder entity))
	   (nums (wl-folder-get-entity-info entity))
	   (new (or (car nums) 0))
	   (unread (or (cadr nums) 0)))
      (when (or (not unread-only)
		(or (> new 0) (> unread 0)))
	(let ((summary (wl-summary-get-buffer entity))
	      (range (wl-summary-get-sync-range folder)))
	  (if summary
	      (save-selected-window
		(with-current-buffer summary
		  (let ((win (get-buffer-window summary t)))
		    (when win
		      (select-window win)))
		  (wl-summary-sync 'unset-cursor range)
		  (wl-summary-save-status)))
	    (elmo-folder-open folder 'load-msgdb)
	    (unwind-protect
		(progn
		  (elmo-folder-synchronize folder nil (eq range 'all))
		  (wl-folder-set-folder-updated
		   entity
		   (list
		    0
		    (or (cdr (assq 'unread (elmo-folder-count-flags folder)))
			0)
		    (elmo-folder-length folder))))
	      (elmo-folder-close folder)))))))))

(defun wl-folder-sync-current-entity (&optional unread-only)
  "Synchronize the folder at position.
If current line is group folder, check all subfolders."
  (interactive "P")
  (save-excursion
    (let ((entity-name (wl-folder-get-entity-from-buffer))
	  (group (wl-folder-buffer-group-p)))
      (when (and entity-name
		 (y-or-n-p (format "Sync %s? " entity-name)))
	(wl-folder-sync-entity
	 (if group
	     (wl-folder-search-group-entity-by-name entity-name
						    wl-folder-entity)
	   entity-name)
	 unread-only)
	(message "Syncing %s is done!" entity-name)))))

(defun wl-folder-mark-as-read-all-entity (entity)
  "Mark as read all messages in the ENTITY."
  (cond
   ((consp entity)
    (let ((flist (nth 2 entity)))
      (while flist
	(wl-folder-mark-as-read-all-entity (car flist))
	(setq flist (cdr flist)))))
   ((stringp entity)
    (let* ((folder (wl-folder-get-elmo-folder entity))
	   (nums (wl-folder-get-entity-info entity))
	   (new (or (car nums) 0))
	   (unread (or (cadr nums) 0)))
      (when (or (> new 0) (> unread 0))
	(let ((summary (wl-summary-get-buffer entity))
	      (range (wl-summary-get-sync-range folder)))
	  (if summary
	      (save-selected-window
		(with-current-buffer summary
		  (let ((win (get-buffer-window summary t)))
		    (when win
		      (select-window win)))
		  (wl-summary-sync 'unset-cursor range)
		  (wl-summary-mark-as-read-all)
		  (wl-summary-save-status)))
	    (elmo-folder-open folder 'load-msgdb)
	    (unwind-protect
		(progn
		  (elmo-folder-synchronize folder nil (eq range 'all))
		  (elmo-folder-unset-flag
		   folder
		   (elmo-folder-list-flagged folder 'unread 'in-msgdb)
		   'unread)
		  (wl-folder-set-folder-updated
		   entity
		   (list 0 0 (elmo-folder-length folder))))
	      (elmo-folder-close folder)))))))))

(defun wl-folder-mark-as-read-all-current-entity ()
  "Mark as read all messages in the folder at position.
If current line is group folder, all subfolders are marked."
  (interactive)
  (save-excursion
    (let ((entity-name (wl-folder-get-entity-from-buffer))
	  (group (wl-folder-buffer-group-p))
	  summary-buf)
      (when (and entity-name
		 (y-or-n-p (format "Mark all messages in %s as read? "
				   entity-name)))
	(wl-folder-mark-as-read-all-entity
	 (if group
	     (wl-folder-search-group-entity-by-name entity-name
						    wl-folder-entity)
	   entity-name))
	(message "All messages in %s are marked!" entity-name)))))

(defun wl-folder-check-region (beg end)
  (interactive "r")
  (goto-char beg)
  (beginning-of-line)
  (setq beg (point))
  (goto-char end)
  (beginning-of-line)
  (setq end (point))
  (goto-char beg)
  (let ((inhibit-read-only t)
	entity)
    (while (< (point) end)
      ;; normal folder entity
      (if (looking-at "^[\t ]*\\([^\\[]+\\):\\(.*\\)\n")
	  (save-excursion
	    (setq entity (wl-folder-get-entity-from-buffer))
	    (if (not (elmo-folder-plugged-p (wl-folder-get-elmo-folder
					     entity)))
		(message "Uncheck %s" entity)
	      (message "Checking %s" entity)
	      (wl-folder-check-one-entity entity)
	      (sit-for 0))))
      (forward-line)))
  (message ""))

(defun wl-folder-sync-region (beg end)
  (interactive "r")
  (goto-char beg)
  (beginning-of-line)
  (setq beg (point))
  (goto-char end)
  (end-of-line)
  (setq end (point))
  (goto-char beg)
  (while (< (point) end)
    ;; normal folder entity
    (if (looking-at "^[\t ]*\\([^\\[]+\\):\\(.*\\)\n")
	(save-excursion
	  (let ((inhibit-read-only t)
		entity)
	    (setq entity (wl-folder-get-entity-from-buffer))
	    (wl-folder-sync-entity entity)
	    (message "Syncing %s is done!" entity)
	    (sit-for 0))))
    (forward-line))
  (message ""))

(defun wl-folder-mark-as-read-all-region (beg end)
  (interactive "r")
  (goto-char beg)
  (beginning-of-line)
  (setq beg (point))
  (goto-char end)
  (end-of-line)
  (setq end (point))
  (goto-char beg)
  (while (< (point) end)
    ;; normal folder entity
    (if (looking-at "^[\t ]*\\([^\\[]+\\):\\(.*\\)\n")
	(save-excursion
	  (let ((inhibit-read-only t)
		entity)
	    (setq entity (wl-folder-get-entity-from-buffer))
	    (wl-folder-mark-as-read-all-entity entity)
	    (message "All messages in %s are marked!" entity)
	    (sit-for 0))))
    (forward-line))
  (message ""))

(defsubst wl-create-access-init-load-p (folder)
  (let ((no-load-regexp (when (and
			       (not wl-folder-init-load-access-folders)
			       wl-folder-init-no-load-access-folders)
			  (mapconcat 'identity
				     wl-folder-init-no-load-access-folders
				     "\\|")))
	(load-regexp (and wl-folder-init-load-access-folders
			  (mapconcat 'identity
				     wl-folder-init-load-access-folders
				     "\\|"))))
    (cond (load-regexp (string-match load-regexp folder))
	  (t (not (and no-load-regexp
		       (string-match no-load-regexp folder)))))))

(defun wl-create-access-folder-entity (name)
  (let (flists flist)
    (when (wl-create-access-init-load-p name)
      (setq flists (elmo-msgdb-flist-load name)) ; load flist.
      (setq flist (car flists))
      (while flist
	(when (consp (car flist))
	  (setcdr (cdar flist)
		  (wl-create-access-folder-entity (caar flist))))
	(setq flist (cdr flist)))
      flists)))

(defun wl-create-folder-entity-from-buffer ()
  "Create folder entity recursively."
  (cond
   ((looking-at "^[ \t]*$")		; blank line
    (goto-char (+ 1(match-end 0)))
    'ignore)
   ((looking-at "^#.*$")		; comment
    (goto-char (+ 1 (match-end 0)))
    'ignore)
   ((looking-at "^[\t ]*\\(.+\\)[\t ]*{[\t ]*$") ; group definition
    (let (name entity flist)
      (setq name (wl-match-buffer 1))
      (goto-char (+ 1 (match-end 0)))
      (while (setq entity (wl-create-folder-entity-from-buffer))
	(unless (eq entity 'ignore)
	  (wl-append flist (list entity))))
      (if (looking-at "^[\t ]*}[\t ]*$") ; end of group
	  (progn
	    (goto-char (+ 1 (match-end 0)))
	    (if (wl-string-assoc name wl-folder-petname-alist)
		(error "%s already defined as petname" name))
	    (list name 'group flist))
	(error "Syntax error in folder definition"))))
   ((looking-at "^[\t ]*\\([^\t \n]+\\)[\t ]*/$") ; access it!
    (let (name)
      (setq name (wl-match-buffer 1))
      (goto-char (+ 1 (match-end 0)))
;;;      (condition-case ()
;;;	  (unwind-protect
;;;	      (setq flist (elmo-list-folders name)))
;;;	(error (message "Access to folder %s failed." name)))
;;;      (setq flist (elmo-msgdb-flist-load name)) ; load flist.
;;;      (setq unsublist (nth 1 flist))
;;;      (setq flist (car flist))
;;;      (list name 'access flist unsublist)))
      (append (list name 'access) (wl-create-access-folder-entity name))))
;;;   ((looking-at "^[\t ]*\\([^\t \n}]+\\)[\t ]*\\(\"[^\"]*\"\\)?[\t ]*$") ; normal folder entity
   ((looking-at "^[\t ]*=[ \t]+\\([^\n]+\\)$"); petname definition
    (goto-char (+ 1 (match-end 0)))
    (let ((rest (elmo-match-buffer 1))
	  petname)
      (when (string-match "\\(\"[^\"]*\"\\)[\t ]*$" rest)
	(setq petname (elmo-delete-char ?\" (match-string 1 rest)))
	(setq rest (substring rest 0 (match-beginning 0))))
      (when (string-match "^[\t ]*\\(.*[^\t ]+\\)[\t ]+$" rest)
	(wl-folder-append-petname (match-string 1 rest) petname))
      'ignore))
   ((looking-at "^[ \t]*}[ \t]*$") ; end of group
    nil)
   ((looking-at "^.*$") ; normal folder entity
    (goto-char (+ 1 (match-end 0)))
    (let ((rest (elmo-match-buffer 0))
	  realname petname)
      (if (string-match "\\(\"[^\"]*\"\\)[\t ]*$" rest)
	  (progn
	    (setq petname (elmo-delete-char ?\" (match-string 1 rest)))
	    (setq rest (substring rest 0 (match-beginning 0)))
	    (when (string-match "^[\t ]*\\(.*[^\t ]+\\)[\t ]+$" rest)
	      (wl-folder-append-petname
	       (setq realname (match-string 1 rest)) petname)
	      realname))
	(if (string-match "^[\t ]*\\(.+\\)$" rest)
	    (match-string 1 rest)
	  rest))))))

(defun wl-folder-create-folder-entity ()
  "Create folder entries."
  (let ((tmp-buf (get-buffer-create " *wl-folder-tmp*"))
	entity ret-val)
    (condition-case ()
	(progn
	  (with-current-buffer tmp-buf
	    (erase-buffer)
	    (insert-file-contents wl-folders-file)
	    (goto-char (point-min))
	    (while (and (not (eobp))
			(setq entity (wl-create-folder-entity-from-buffer)))
	      (unless (eq entity 'ignore)
		(wl-append ret-val (list entity)))))
	  (kill-buffer tmp-buf))
      (file-error nil))
    (setq ret-val (list wl-folder-desktop-name 'group ret-val))))

(defun wl-folder-entity-assign-id (entity &optional hashtb on-noid)
  (let ((hashtb (or hashtb
		    (setq wl-folder-entity-id-name-hashtb
			  (elmo-make-hash wl-folder-entity-id))))
	(entities (list entity))
	entity-stack)
    (while entities
      (setq entity (wl-pop entities))
      (cond
       ((consp entity)
	(when (not (and on-noid
			(get-text-property 0
					   'wl-folder-entity-id
					   (car entity))))
	  (wl-folder-put-folder-property
	   0 (length (car entity))
	   wl-folder-entity-id
	   'is-group
	   (car entity))
	  (wl-folder-set-id-name wl-folder-entity-id
				 (car entity) hashtb)
	  (setq wl-folder-entity-id (+ 1 wl-folder-entity-id)))
	(and entities
	     (wl-push entities entity-stack))
	(setq entities (nth 2 entity)))
       ((stringp entity)
	(when (not (and on-noid
			(get-text-property 0
					   'wl-folder-entity-id
					   entity)))
	  (wl-folder-put-folder-property
	   0 (length entity)
	   wl-folder-entity-id
	   nil
	   entity)
	  (wl-folder-set-id-name wl-folder-entity-id
				 entity hashtb)
	  (setq wl-folder-entity-id (+ 1 wl-folder-entity-id)))))
      (unless entities
	(setq entities (wl-pop entity-stack))))))

(defun wl-folder-click (e)
  (interactive "e")
  (mouse-set-point e)
  (beginning-of-line)
  (save-excursion
    (wl-folder-jump-to-current-entity)))

(defun wl-folder-select-buffer (buffer)
  (let ((gbw (get-buffer-window buffer))
	exists)
    (if gbw
	(progn (select-window gbw)
	       (setq exists t))
      (unless wl-summary-use-frame
	(condition-case ()
	    (unwind-protect
		(split-window-horizontally wl-folder-window-width)
	      (other-window 1))
	  (error nil))))
    (set-buffer buffer)
    (if wl-summary-use-frame
	(switch-to-buffer-other-frame buffer)
      (switch-to-buffer buffer))
    exists))

(defun wl-folder-toggle-disp-summary (&optional arg folder)
  (interactive)
  (if (or (and folder (assoc folder wl-folder-group-alist))
	  (and (interactive-p) (wl-folder-buffer-group-p)))
      (error "This command is not available on Group"))
  (beginning-of-line)
  (let (wl-auto-select-first
	(wl-stay-folder-window t))
    (cond
     ((eq arg 'on)
      (setq wl-folder-buffer-disp-summary t))
     ((eq arg 'off)
      (setq wl-folder-buffer-disp-summary nil)
      ;; hide wl-summary window.
      (let ((cur-buf (current-buffer))
	    (summary-buffer (wl-summary-get-buffer folder)))
	(when summary-buffer
	  (wl-folder-select-buffer summary-buffer)
	  (delete-window))
	(select-window (get-buffer-window cur-buf))))
     (t
      (setq wl-folder-buffer-disp-summary
	    (not wl-folder-buffer-disp-summary))
      (let ((cur-buf (current-buffer))
	    folder-name)
	(when (looking-at "^[ ]*\\([^\\[].+\\):.*\n")
	  (setq folder-name (wl-folder-get-entity-from-buffer))
	  (if wl-folder-buffer-disp-summary
	      (progn
		(wl-folder-select-buffer
		 (wl-summary-get-buffer-create folder-name))
		(unwind-protect
		    (wl-summary-goto-folder-subr folder-name 'no-sync nil)
		  (select-window (get-buffer-window cur-buf))))
	    (let ((summary-buffer (wl-summary-get-buffer folder-name)))
	      (when summary-buffer
		(wl-folder-select-buffer summary-buffer)
		(delete-window))
	      (select-window (get-buffer-window cur-buf))))))))))

(defun wl-folder-prev-unsync ()
  "Move cursor to the previous unsync folder."
  (interactive)
  (let (start-point)
    (setq start-point (point))
    (beginning-of-line)
    (if (re-search-backward wl-folder-unsync-regexp nil t)
	(beginning-of-line)
      (goto-char start-point)
      (message "No more unsync folder"))))

(defun wl-folder-next-unsync (&optional plugged)
  "Move cursor to the next unsync."
  (interactive)
  (let (start-point entity)
    (setq start-point (point))
    (end-of-line)
    (if (catch 'found
	  (while (re-search-forward wl-folder-unsync-regexp nil t)
	    (if (or (wl-folder-buffer-group-p)
		    (not plugged)
		    (setq entity
			  (wl-folder-get-entity-from-buffer))
		    (elmo-folder-plugged-p entity))
		(throw 'found t))))
	(beginning-of-line)
      (goto-char start-point)
      (message "No more unsync folder"))))

(defun wl-folder-prev-unread (&optional group)
  "Move cursor to the previous unread folder."
  (interactive "P")
  (let (start-point)
    (setq start-point (point))
    (beginning-of-line)
    (if (re-search-backward (wl-folder-unread-regex group) nil t)
	(progn
	  (beginning-of-line)
	  (wl-folder-get-entity-from-buffer))
      (goto-char start-point)
      (message "No more unread folder")
      nil)))

(defun wl-folder-next-unread (&optional group)
  "Move cursor to the next unread folder."
  (interactive "P")
  (let (start-point)
    (setq start-point (point))
    (end-of-line)
    (if (re-search-forward (wl-folder-unread-regex group) nil t)
	(progn
	  (beginning-of-line)
	  (wl-folder-get-entity-from-buffer))
      (goto-char start-point)
      (message "No more unread folder")
      nil)))

(defun wl-folder-mode ()
  "Major mode for Wanderlust Folder.
See Info under Wanderlust for full documentation.

Special commands:
\\{wl-folder-mode-map}

Entering Folder mode calls the value of `wl-folder-mode-hook'."
  (interactive)
  (setq major-mode 'wl-folder-mode)
  (setq mode-name "Folder")
  (use-local-map wl-folder-mode-map)
  (setq buffer-read-only t)
  (setq inhibit-read-only nil)
  (setq truncate-lines t)
  (setq wl-folder-buffer-cur-entity-id nil
	wl-folder-buffer-cur-path nil
	wl-folder-buffer-cur-point nil)
  (when (boundp 'bidi-paragraph-direction)
    (set 'bidi-paragraph-direction 'left-to-right))
  (wl-mode-line-buffer-identification)
  (easy-menu-add wl-folder-mode-menu)
  ;; This hook may contain the functions `wl-folder-init-icons' and
  ;; `wl-setup-folder' for reasons of system internal to accord
  ;; facilities for the Emacs variants.
  (run-hooks 'wl-folder-mode-hook))

(defun wl-folder-append-petname (realname petname)
  (let ((pentry (wl-string-assoc realname wl-folder-petname-alist)))
    (when pentry
      (setq wl-folder-petname-alist
	    (delete pentry wl-folder-petname-alist))))
  (wl-append wl-folder-petname-alist
	     (list (cons realname petname))))

(defun wl-folder ()
  (let (initialize folder-buf)
    (if (setq folder-buf (get-buffer wl-folder-buffer-name))
	(if wl-folder-use-frame
	    (let (select-frame)
	      (save-selected-window
		(dolist (frame (visible-frame-list))
		  (select-frame frame)
		  (if (get-buffer-window folder-buf)
		      (setq select-frame frame))))
	      (if select-frame
		  (select-frame select-frame)
		(switch-to-buffer folder-buf)))
	  (switch-to-buffer folder-buf))
      (if wl-folder-use-frame
	  (progn
	    (switch-to-buffer-other-frame
	     (get-buffer-create wl-folder-buffer-name))
	    (let ((frame (selected-frame)))
	      (setq wl-delete-startup-frame-function
		    `(lambda ()
		       (setq wl-delete-startup-frame-function nil)
		       (let ((frame ,frame))
			 (if (eq (selected-frame) frame)
			     (delete-frame frame)))))))
	(switch-to-buffer (get-buffer-create wl-folder-buffer-name)))
      (set-buffer wl-folder-buffer-name)
      (wl-folder-mode)
      ;; Initialization.
      (unless wl-folder-entity
	(wl-folder-init))
      (setq wl-folder-entity-id 0)
      (wl-folder-entity-assign-id wl-folder-entity)
      (setq wl-folder-entity-hashtb
	    (wl-folder-create-entity-hashtb wl-folder-entity))
      (setq wl-folder-elmo-folder-hashtb (elmo-make-hash wl-folder-entity-id))
      (setq wl-folder-group-alist
	    (wl-folder-create-group-alist wl-folder-entity))
      (setq wl-folder-newsgroups-hashtb
	    (wl-folder-create-newsgroups-hashtb wl-folder-entity))
      (wl-folder-init-info-hashtb)
      (let ((inhibit-read-only t)
	    (buffer-read-only nil))
	(erase-buffer)
	(setcdr (assoc (car wl-folder-entity) wl-folder-group-alist) t)
	(save-excursion
	  (wl-folder-insert-entity " " wl-folder-entity)))
      (sit-for 0)
      (set-buffer-modified-p nil)
      (setq initialize t))
    initialize))

(defun wl-folder-auto-check ()
  "Check and update folders in `wl-auto-check-folder-name'."
  (interactive)
  (when (get-buffer wl-folder-buffer-name)
    (switch-to-buffer  wl-folder-buffer-name)
    (cond
     ((eq wl-auto-check-folder-name 'none))
     ((or (consp wl-auto-check-folder-name)
	  (stringp wl-auto-check-folder-name))
      (let ((folder-list (if (consp wl-auto-check-folder-name)
			     wl-auto-check-folder-name
			   (list wl-auto-check-folder-name)))
	    entity)
	(while folder-list
	  (if (setq entity (wl-folder-search-entity-by-name
			    (car folder-list)
			    wl-folder-entity))
	      (wl-folder-check-entity entity 'auto))
	  (setq folder-list (cdr folder-list)))))
     (t
      (wl-folder-check-entity wl-folder-entity 'auto)))))

(defun wl-folder-set-folder-updated (name value)
  (save-excursion
    (let ((buf (get-buffer wl-folder-buffer-name)))
      (if buf
	  (wl-folder-entity-hashtb-set
	   wl-folder-entity-hashtb name value buf))
      (setq wl-folder-info-alist-modified t))))

(defun wl-folder-calc-finfo (entity)
  ;; calcurate finfo without inserting.
  (let ((entities (list entity))
	entity-stack
	new unread all nums)
    (while entities
      (setq entity (wl-pop entities))
      (cond
       ((consp entity)
	(and entities
	     (wl-push entities entity-stack))
	(setq entities (nth 2 entity)))
       ((stringp entity)
	(setq nums (wl-folder-get-entity-info entity))
	(setq new    (+ (or new 0) (or (nth 0 nums) 0)))
	(setq unread (+ (or unread 0)
			(or (and (nth 0 nums)(nth 1 nums)
				 (+ (nth 0 nums)(nth 1 nums))) 0)))
	(setq all    (+ (or all 0) (or (nth 2 nums) 0)))))
      (unless entities
	(setq entities (wl-pop entity-stack))))
    (list new unread all)))

(defsubst wl-folder-make-save-access-list (list)
  (mapcar (lambda (x)
	    (cond
	     ((consp x)
	      (list (elmo-string (car x)) 'access))
	     (t
	      (elmo-string x))))
	  list))

(defun wl-folder-update-newest (indent entity)
  (let (ret-val new unread all)
    (cond
     ((consp entity)
      (let ((inhibit-read-only t)
	    (buffer-read-only nil)
	    (flist (nth 2 entity))
	    (as-opened (cdr (assoc (car entity) wl-folder-group-alist)))
	    beg
	    )
	(setq beg (point))
	(if as-opened
	    (let (update-flist flist-unsub new-flist removed group-name-end)
	      (when (and (eq (cadr entity) 'access)
			 (elmo-folder-plugged-p
			  (wl-folder-get-elmo-folder (car entity))))
		(message "Fetching folder entries...")
		(when (setq new-flist
			    (elmo-folder-list-subfolders
			     (wl-folder-get-elmo-folder (car entity))
			     (wl-string-match-member
			      (car entity)
			      wl-folder-hierarchy-access-folders)))
		  (setq update-flist
			(wl-folder-update-access-group entity new-flist))
		  (setq flist (nth 1 update-flist))
		  (when (car update-flist) ;; diff
		    (setq flist-unsub (nth 2 update-flist))
		    (setq removed (nth 3 update-flist))
		    (elmo-msgdb-flist-save
		     (car entity)
		     (list
		      (wl-folder-make-save-access-list flist)
		      (wl-folder-make-save-access-list flist-unsub)))
		    (wl-folder-entity-assign-id
		     entity
		     wl-folder-entity-id-name-hashtb
		     t)
		    (setq wl-folder-entity-hashtb
			  (wl-folder-create-entity-hashtb
			   entity
			   wl-folder-entity-hashtb
			   t))
		    (setq wl-folder-newsgroups-hashtb
			  (or
			   (wl-folder-create-newsgroups-hashtb
			    entity nil)
			   wl-folder-newsgroups-hashtb))))
		(message "Fetching folder entries...done"))
	      (wl-folder-insert-entity indent entity))))))))

(defun wl-folder-insert-entity (indent entity &optional onlygroup)
  (let (ret-val new unread all)
    (cond
     ((consp entity)
      (let ((inhibit-read-only t)
	    (buffer-read-only nil)
	    (flist (nth 2 entity))
	    (as-opened (cdr (assoc (car entity) wl-folder-group-alist)))
	    beg
	    )
;;;	(insert indent "[" (if as-opened "-" "+") "]" (car entity) "\n")
;;;	(save-excursion (forward-line -1)
;;;			(wl-highlight-folder-current-line))
	(setq beg (point))
	(if (and as-opened
		 (not onlygroup))
	    (let (update-flist flist-unsub new-flist removed group-name-end)
;;;	      (when (and (eq (cadr entity) 'access)
;;;			 newest)
;;;		(message "fetching folder entries...")
;;;		(when (setq new-flist
;;;			    (elmo-list-folders
;;;			     (elmo-string (car entity))
;;;			     (wl-string-member
;;;			      (car entity)
;;;			      wl-folder-hierarchy-access-folders)
;;;			     ))
;;;		  (setq update-flist
;;;			(wl-folder-update-access-group entity new-flist))
;;;		  (setq flist (nth 1 update-flist))
;;;		  (when (car update-flist) ;; diff
;;;		    (setq flist-unsub (nth 2 update-flist))
;;;		    (setq removed (nth 3 update-flist))
;;;		    (elmo-msgdb-flist-save
;;;		     (car entity)
;;;		     (list
;;;		      (wl-folder-make-save-access-list flist)
;;;		      (wl-folder-make-save-access-list flist-unsub)))
;;;		    ;;
;;;		    ;; reconstruct wl-folder-entity-id-name-hashtb and
;;;		    ;;		 wl-folder-entity-hashtb
;;;		    ;;
;;;		    (wl-folder-entity-assign-id
;;;		     entity
;;;		     wl-folder-entity-id-name-hashtb
;;;		     t)
;;;		    (setq wl-folder-entity-hashtb
;;;			  (wl-folder-create-entity-hashtb
;;;			   entity
;;;			   wl-folder-entity-hashtb
;;;			   t))
;;;		    (setq wl-folder-newsgroups-hashtb
;;;			  (or
;;;			   (wl-folder-create-newsgroups-hashtb
;;;			    entity nil)
;;;			   wl-folder-newsgroups-hashtb))))
;;;		(message "fetching folder entries...done"))
	      (insert indent "[" (if as-opened "-" "+") "]"
		      (if (eq (cadr entity) 'access)
			  (wl-folder-get-petname (car entity))
			(car entity)))
	      (setq group-name-end (point))
	      (insert ":0/0/0\n")
	      (wl-folder-put-folder-property
	       beg (point)
	       (get-text-property 0 'wl-folder-entity-id (car entity))
	       'is-group)
	      (when removed
		(setq beg (point))
		(while removed
		  (insert indent "  "
			  wl-folder-removed-mark
			  (if (listp (car removed))
			      (concat "[+]" (caar removed))
			    (car removed))
			  "\n")
		  (save-excursion (forward-line -1)
				  (wl-highlight-folder-current-line))
		  (setq removed (cdr removed)))
		(remove-text-properties beg (point) '(wl-folder-entity-id)))
	      (elmo-with-progress-display
		  (wl-folder-insert-entity (length flist))
		  (format "Inserting group %s" (car entity))
		(while flist
		  (setq ret-val
			(wl-folder-insert-entity
			 (concat indent "  ") (car flist)))
		  (setq new    (+ (or new 0) (or (nth 0 ret-val) 0)))
		  (setq unread (+ (or unread 0) (or (nth 1 ret-val) 0)))
		  (setq all    (+ (or all 0) (or (nth 2 ret-val) 0)))
		  (elmo-progress-notify 'wl-folder-insert-entity)
		  (setq flist (cdr flist))))
	      (save-excursion
		(goto-char group-name-end)
		(delete-region (point) (save-excursion (end-of-line)
						       (point)))
		(insert (format ":%d/%d/%d" (or new 0)
				(or unread 0) (or all 0)))
		(setq ret-val (list new unread all))
		(wl-highlight-folder-current-line ret-val)))
	  (setq ret-val (wl-folder-calc-finfo entity))
	  (insert indent "[" (if as-opened "-" "+") "]"
		  (if (eq (cadr entity) 'access)
		      (wl-folder-get-petname (car entity))
		    (car entity))
		  (format ":%d/%d/%d"
			  (or (nth 0 ret-val) 0)
			  (or (nth 1 ret-val) 0)
			  (or (nth 2 ret-val) 0))
		  "\n")
	  (wl-folder-put-folder-property
	   beg (point)
	   (get-text-property 0 'wl-folder-entity-id (car entity))
	   'is-group)
	  (save-excursion (forward-line -1)
			  (wl-highlight-folder-current-line ret-val)))))
     ((stringp entity)
      (let* ((inhibit-read-only t)
	     (buffer-read-only nil)
	     (nums (wl-folder-get-entity-info entity))
	     beg)
	(setq beg (point))
	(insert indent (wl-folder-get-petname entity)
		(format ":%s/%s/%s\n"
			(or (setq new (nth 0 nums)) "*")
			(or (setq unread (and (nth 0 nums)(nth 1 nums)
					      (+ (nth 0 nums)(nth 1 nums))))
			    "*")
			(or (setq all (nth 2 nums)) "*")))
	(wl-folder-put-folder-property
	 beg (point)
	 (get-text-property 0 'wl-folder-entity-id entity)
	 nil)
	(save-excursion (forward-line -1)
			(wl-highlight-folder-current-line nums))
	(setq ret-val (list new unread all)))))
    (set-buffer-modified-p nil)
    ret-val))

(defun wl-folder-check-all ()
  (interactive)
  (wl-folder-check-entity wl-folder-entity))

(defun wl-folder-entity-hashtb-set (entity-hashtb name value buffer)
  (let ((cur-val (wl-folder-get-entity-info name entity-hashtb))
	(new-diff 0)
	(unread-diff 0)
	(all-diff 0)
	diffs)
    (setq new-diff    (- (or (nth 0 value) 0) (or (nth 0 cur-val) 0)))
    (setq unread-diff
	  (+ new-diff
	     (- (or (nth 1 value) 0) (or (nth 1 cur-val) 0))))
    (setq all-diff    (- (or (nth 2 value) 0) (or (nth 2 cur-val) 0)))
    (setq diffs (list new-diff unread-diff all-diff))
    (unless (and (nth 0 cur-val)
		 (equal diffs '(0 0 0)))
      (wl-folder-set-entity-info name value entity-hashtb)
      (save-match-data
	(with-current-buffer buffer
	  (save-excursion
	    (let ((entity-list (wl-folder-search-entity-list-by-name
				name wl-folder-entity)))
	      (while entity-list
		(wl-folder-update-group (car entity-list) diffs)
		(setq entity-list (cdr entity-list))))
	    (goto-char (point-min))
	    (while (wl-folder-buffer-search-entity name)
	      (wl-folder-update-line value))))))))

(defun wl-folder-update-unread (folder unread)
  (let ((buf (get-buffer wl-folder-buffer-name))
	(value (wl-folder-get-entity-info folder))
	cur-unread
	(unread-diff 0)
	newvalue)
;;; Update folder-info
;;;    (elmo-folder-set-info-hashtb fld nil nil nil unread)
    (setq cur-unread (or (nth 1 value) 0))
    (setq unread-diff (- (or unread 0) cur-unread))
    (setq newvalue (list (nth 0 value)
			 unread
			 (nth 2 value)))
    (wl-folder-set-entity-info folder newvalue)
    (setq wl-folder-info-alist-modified t)
    (when (and buf
	       (not (zerop unread-diff)))
      (save-match-data
	(with-current-buffer buf
	  (save-excursion
	    (let ((entity-list (wl-folder-search-entity-list-by-name
				folder wl-folder-entity)))
	      (while entity-list
		(wl-folder-update-group (car entity-list) (list 0
								unread-diff
								0))
		(setq entity-list (cdr entity-list))))
	    (goto-char (point-min))
	    (while (wl-folder-buffer-search-entity folder)
	      (wl-folder-update-line newvalue))))))))

(defun wl-folder-create-entity-hashtb (entity &optional hashtb reconst)
  (let ((hashtb (or hashtb (elmo-make-hash wl-folder-entity-id)))
	(entities (list entity))
	entity-stack)
    (while entities
      (setq entity (wl-pop entities))
      (cond
       ((consp entity)
	(and entities
	     (wl-push entities entity-stack))
	(setq entities (nth 2 entity)))
       ((stringp entity)
	(when (not (and reconst
			(wl-folder-get-entity-info entity)))
	  (wl-folder-set-entity-info entity
				     nil
				     hashtb))))
      (unless entities
	(setq entities (wl-pop entity-stack))))
    hashtb))

;; Unsync number is reserved.
;;;(defun wl-folder-reconstruct-entity-hashtb (entity &optional hashtb id-name)
;;;  (let* ((hashtb (or hashtb (elmo-make-hash wl-folder-entity-id)))
;;;	 (entities (list entity))
;;;	 entity-stack)
;;;    (while entities
;;;      (setq entity (wl-pop entities))
;;;      (cond
;;;       ((consp entity)
;;;	(if id-name
;;;	    (wl-folder-set-id-name (wl-folder-get-entity-id (car entity))
;;;				   (car entity)))
;;;	(and entities
;;;	     (wl-push entities entity-stack))
;;;	(setq entities (nth 2 entity))
;;;	)
;;;       ((stringp entity)
;;;	(wl-folder-set-entity-info entity
;;;				   (wl-folder-get-entity-info entity)
;;;				   hashtb)
;;;	(if id-name
;;;	    (wl-folder-set-id-name (wl-folder-get-entity-id entity)
;;;				   entity))))
;;;      (unless entities
;;;	(setq entities (wl-pop entity-stack))))
;;;    hashtb))

(defun wl-folder-create-newsgroups-from-nntp-access (entity)
  (let ((flist (nth 2 entity))
	folders)
    (while flist
      (wl-append folders
		 (cond
		  ((consp (car flist))
		   (wl-folder-create-newsgroups-from-nntp-access (car flist)))
		  (t
		   (list
		    (elmo-nntp-folder-group-internal
		     (wl-folder-get-elmo-folder (car flist)))))))
      (setq flist (cdr flist)))
    folders))

(defun wl-folder-create-newsgroups-hashtb (entity &optional is-list info)
  "Create NNTP group hashtable for ENTITY."
  (let ((entities (if is-list entity (list entity)))
	entity-stack folder-list newsgroups newsgroup make-hashtb)
    (and info (message "Creating newsgroups..."))
    (while entities
      (setq entity (wl-pop entities))
      (cond
       ((consp entity)
	(if (eq (nth 1 entity) 'access)
	    (when (eq (elmo-folder-type-internal
		       (elmo-make-folder (car entity))) 'nntp)
	      (wl-append newsgroups
			 (wl-folder-create-newsgroups-from-nntp-access entity))
	      (setq make-hashtb t))
	  (and entities
	       (wl-push entities entity-stack))
	  (setq entities (nth 2 entity))))
       ((stringp entity)
	(setq folder-list (elmo-folder-get-primitive-list
			   (elmo-make-folder entity)))
	(while folder-list
	  (when (and (eq (elmo-folder-type-internal (car folder-list))
			 'nntp)
		     (setq newsgroup (elmo-nntp-folder-group-internal
				      (car folder-list))))
	    (wl-append newsgroups (list (elmo-string newsgroup))))
	  (setq folder-list (cdr folder-list)))))
      (unless entities
	(setq entities (wl-pop entity-stack))))
    (and info (message "Creating newsgroups...done"))
    (if (or newsgroups make-hashtb)
	(elmo-setup-subscribed-newsgroups newsgroups))))

(defun wl-folder-get-path (entity target-id &optional string)
  (let ((entities (list entity))
	entity-stack result-path)
    (reverse
     (catch 'done
       (while entities
	 (setq entity (wl-pop entities))
	 (cond
	  ((consp entity)
	   (if (and (or (not string) (string= string (car entity)))
		    (eq target-id (wl-folder-get-entity-id (car entity))))
	       (throw 'done
		      (wl-push target-id result-path))
	     (wl-push (wl-folder-get-entity-id (car entity)) result-path))
	   (wl-push entities entity-stack)
	   (setq entities (nth 2 entity)))
	  ((stringp entity)
	   (if (and (or (not string) (string= string entity))
		    (eq target-id (wl-folder-get-entity-id entity)))
	       (throw 'done
		      (wl-push target-id result-path)))))
	 (unless entities
	   (while (and entity-stack
		       (not entities))
	     (setq result-path (cdr result-path))
	     (setq entities (wl-pop entity-stack)))))))))

(defun wl-folder-create-group-alist (entity)
  (if (consp entity)
      (let ((flist (nth 2 entity))
	    (cur-alist (list (cons (car entity) nil)))
	     append-alist)
	(while flist
	  (if (consp (car flist))
	      (wl-append append-alist
			 (wl-folder-create-group-alist (car flist))))
	  (setq flist (cdr flist)))
	(append cur-alist append-alist))))

(defun wl-folder-init-info-hashtb ()
  (let ((info-alist (and wl-folder-info-save
			 (elmo-msgdb-finfo-load))))
    (elmo-folder-info-make-hashtb
     info-alist
     wl-folder-entity-hashtb)))
;;; (wl-folder-resume-entity-hashtb-by-finfo
;;;  wl-folder-entity-hashtb
;;;  info-alist)))

(defun wl-folder-cleanup-variables ()
  (setq wl-folder-entity nil
	wl-folder-entity-hashtb nil
	wl-folder-entity-id-name-hashtb nil
	wl-folder-group-alist nil
	wl-folder-petname-alist nil
	wl-folder-newsgroups-hashtb nil
	wl-fldmgr-cut-entity-list nil
	wl-fldmgr-modified nil
	wl-fldmgr-modified-access-list nil
	wl-score-cache nil
	))

(defun wl-make-plugged-alist ()
  (let ((entity-list (wl-folder-get-entity-list wl-folder-entity))
	(add (not wl-reset-plugged-alist)))
    (while entity-list
      (elmo-folder-set-plugged
       (wl-folder-get-elmo-folder (car entity-list)) wl-plugged add)
      (setq entity-list (cdr entity-list)))
    ;; smtp posting server
    (when wl-smtp-posting-server
      (elmo-set-plugged wl-plugged
			wl-smtp-posting-server  ; server
			(or wl-smtp-posting-port
			    (and (boundp 'smtp-service) smtp-service)
			    "smtp")	; port
			wl-smtp-connection-type
			nil nil "smtp" add))
    ;; nntp posting server
    (when wl-nntp-posting-server
      (elmo-set-plugged wl-plugged
			wl-nntp-posting-server
			wl-nntp-posting-port
			wl-nntp-posting-stream-type
			nil nil "nntp" add))
    (run-hooks 'wl-make-plugged-hook)))

(defvar wl-folder-init-function 'wl-local-folder-init)

(defun wl-folder-init ()
  "Return top-level folder entity."
  (interactive)
  (if wl-use-acap
      (wl-acap-init)
    (funcall wl-folder-init-function))
  (run-hooks 'wl-folder-init-hook))

(defun wl-local-folder-init ()
  "Initialize local folder."
  (message "Initializing folder...")
  (setq wl-folder-entity (wl-folder-create-folder-entity))
  (message "Initializing folder...done"))

(defun wl-folder-get-realname (petname)
  (or (car
       (wl-string-rassoc
	petname
	wl-folder-petname-alist))
      petname))

(defun wl-folder-get-petname (name)
  (or (cdr
       (wl-string-assoc
	name
	wl-folder-petname-alist))
      name))

(defun wl-folder-get-entity-with-petname ()
  (let ((alist wl-folder-petname-alist)
	(hashtb (copy-sequence wl-folder-entity-hashtb)))
    (while alist
      (wl-folder-set-entity-info (cdar alist) nil hashtb)
      (setq alist (cdr alist)))
    hashtb))

(defun wl-folder-get-newsgroups (folder)
  "Return Newsgroups field value string for FOLDER newsgroup.
If FOLDER is multi, return comma separated string (cross post)."
  (let ((nlist (elmo-folder-newsgroups
			(wl-folder-get-elmo-folder folder))))
    (if nlist
	(list nil nil (mapconcat 'identity nlist ","))
      nil)))

(defun wl-folder-guess-mailing-list-by-refile-rule (entity)
  "Return ML address guess by ENTITY.
Use `wl-subscribed-mailing-list' and `wl-refile-rule-alist'."
  (let ((flist
	 (elmo-folder-get-primitive-list
	  (wl-folder-get-elmo-folder entity)))
	fld mladdr to)
    (while (setq fld (car flist))
      (setq mladdr (wl-folder-guess-mailing-list-by-refile-rule-subr
		    (elmo-folder-name-internal fld)))
      (when mladdr
	(setq to (if (stringp to)
		     (concat to ", " mladdr)
		   mladdr)))
      (setq flist (cdr flist)))
    (if (stringp to)
	(list to nil nil)
      nil)))

(defun wl-folder-guess-mailing-list-by-refile-rule-subr (entity)
  (unless (memq (elmo-folder-type entity)
		'(localnews nntp))
    (let ((rules wl-refile-rule-alist)
	  tokey toalist)
      (while rules
	(if (or (and (stringp (car (car rules)))
		     (string-match "[Tt]o" (car (car rules))))
		(and (listp (car (car rules)))
		     (elmo-string-matched-member "to" (car (car rules))
						 'case-ignore)))
	    (setq toalist (append toalist (cdr (car rules)))))
	(setq rules (cdr rules)))
      (setq tokey (car (rassoc entity toalist)))
;;;     (setq histkey (car (rassoc folder wl-refile-alist)))
      ;; case-ignore search `wl-subscribed-mailing-list'
      (if (stringp tokey)
	  (elmo-string-matched-member tokey wl-subscribed-mailing-list t)))))

(defun wl-folder-guess-mailing-list-by-folder-name (entity)
  "Return ML address guess by ENTITY name's last hierarchy.
Use `wl-subscribed-mailing-list'."
  (let ((flist
	 (elmo-folder-get-primitive-list
	  (wl-folder-get-elmo-folder entity)))
	fld mladdr to)
    (while (setq fld (car flist))
      (setq mladdr (wl-folder-guess-mailing-list-by-folder-name-subr
		    (elmo-folder-name-internal fld)))
      (when mladdr
	(setq to (if (stringp to)
		     (concat to ", " mladdr)
		   mladdr)))
      (setq flist (cdr flist)))
    (if (stringp to)
	(list to nil nil)
      nil)))

(defun wl-folder-guess-mailing-list-by-folder-name-subr (entity)
  (when (memq (elmo-folder-type entity)
	      '(localdir imap4 maildir))
    (let (key foldername)
      ;; Get foldername and Remove folder type symbol.
      (setq foldername (substring entity 1))
      (if (string-match "@" foldername)
	  (setq foldername (substring foldername 0 (match-beginning 0))))
      (when (string-match "[^\\./]+$" foldername)
	(setq key (regexp-quote
		   (concat (substring foldername (match-beginning 0)) "@")))
	(elmo-string-matched-member
	 key wl-subscribed-mailing-list 'case-ignore)))))

(defun wl-folder-update-diff-line (diffs)
  (let ((inhibit-read-only t)
	(buffer-read-only nil)
	cur-new new-new
	cur-unread new-unread
	cur-all new-all
	id is-group)
    (save-excursion
      (beginning-of-line)
      (setq id (get-text-property (point) 'wl-folder-entity-id))
      (setq is-group (get-text-property (point) 'wl-folder-is-group))
      (when (looking-at "^[ ]*\\(.*\\):\\([0-9\\*-]*\\)/\\([0-9\\*-]*\\)/\\([0-9\\*]*\\)")
	;;(looking-at "^[ ]*\\([^\\[].+\\):\\([0-9\\*-]*/[0-9\\*-]*/[0-9\\*]*\\)")
	(setq cur-new (string-to-number
		       (wl-match-buffer 2)))
	(setq cur-unread (string-to-number
			  (wl-match-buffer 3)))
	(setq cur-all (string-to-number
		       (wl-match-buffer 4)))
	(delete-region (match-beginning 2)
		       (match-end 4))
	(goto-char (match-beginning 2))
	(insert (format "%s/%s/%s"
			(setq new-new (+ cur-new (nth 0 diffs)))
			(setq new-unread (+ cur-unread (nth 1 diffs)))
			(setq new-all (+ cur-all (nth 2 diffs)))))
	(wl-folder-put-folder-property (match-beginning 2) (point) id is-group)
	(if wl-use-highlight-mouse-line
	    (put-text-property (match-beginning 2) (point)
			       'mouse-face 'highlight))
	(wl-highlight-folder-group-line (list new-new new-unread new-all))
	(setq buffer-read-only t)
	(set-buffer-modified-p nil)))))

(defun wl-folder-update-line (nums &optional is-group)
  (let ((inhibit-read-only t)
	(buffer-read-only nil)
	id is-group)
    (save-excursion
      (beginning-of-line)
      (setq id (get-text-property (point) 'wl-folder-entity-id))
      (setq is-group (get-text-property (point) 'wl-folder-is-group))
      (if (looking-at "^[ ]*\\(.*\\):\\([0-9\\*-]*/[0-9\\*-]*/[0-9\\*]*\\)")
;;;	  (looking-at "^[ ]*\\([^\\[].+\\):\\([0-9\\*-]*/[0-9\\*-]*/[0-9\\*]*\\)")
	  (progn
	    (delete-region (match-beginning 2)
			   (match-end 2))
	    (goto-char (match-beginning 2))
	    (insert (format "%s/%s/%s"
			    (or (nth 0 nums) "*")
			    (or (and (nth 0 nums)(nth 1 nums)
				     (+ (nth 0 nums)(nth 1 nums)))
				"*")
			    (or (nth 2 nums) "*")))
	    (wl-folder-put-folder-property (match-beginning 2) (point) id is-group)
	    (if is-group
		;; update only colors
		(wl-highlight-folder-group-line nums)
	      (wl-highlight-folder-current-line nums))
	    (beginning-of-line)
	    (set-buffer-modified-p nil))))))

(defun wl-folder-goto-folder (&optional arg)
  "Visit some folder."
  (interactive "P")
  (wl-folder-goto-folder-subr nil arg))

(defun wl-folder-goto-folder-sticky ()
  "Visit some folder and make it sticky."
  (interactive)
  (wl-folder-goto-folder-subr nil t))

(defun wl-folder-goto-draft-folder (&optional arg)
  "Visit draft folder."
  (interactive "P")
  (wl-folder-goto-folder-subr wl-draft-folder arg))

(defun wl-folder-revisit-last-visited-folder (&optional arg)
  "Revisit last visited folder."
  (interactive "P")
  (let ((folder
	 (wl-folder-get-folder-name-by-id wl-folder-buffer-last-visited-entity-id)))
    (if (and folder
	     (y-or-n-p (format "Revisit %s? " folder)))
	(wl-folder-goto-folder-subr folder arg))))

(defun wl-folder-goto-folder-subr (&optional folder sticky)
  (beginning-of-line)
  (let (summary-buf fld-name entity id error-selecting)
;;; (setq fld-name (wl-folder-get-entity-from-buffer))
;;; (if (or (null fld-name)
;;;	    (assoc fld-name wl-folder-group-alist))
    (setq fld-name wl-default-folder)
    (setq fld-name (or folder
		       (let (this-command)
			 (wl-summary-read-folder fld-name))))
    (if (and (setq entity
		   (wl-folder-search-entity-by-name fld-name
						    wl-folder-entity
						    'folder))
	     (setq id (wl-folder-get-entity-id entity)))
	(wl-folder-set-current-entity-id id))
    (setq summary-buf (wl-summary-get-buffer-create fld-name sticky))
    (if (or wl-stay-folder-window wl-summary-use-frame)
	(wl-folder-select-buffer summary-buf)
      (if (and summary-buf
	       (get-buffer-window summary-buf))
	  (delete-window)))
    (wl-summary-goto-folder-subr fld-name
				 (wl-summary-get-sync-range
				  (wl-folder-get-elmo-folder fld-name))
				 nil sticky t)))

(defun wl-folder-suspend ()
  (interactive)
  (run-hooks 'wl-folder-suspend-hook)
  (wl-folder-info-save)
  (elmo-crosspost-message-alist-save)
  (elmo-quit)
  ;(if (fboundp 'mmelmo-cleanup-entity-buffers)
  ;(mmelmo-cleanup-entity-buffers))
  (bury-buffer wl-folder-buffer-name)
  (dolist (summary-buf (wl-collect-summary))
    (bury-buffer summary-buf))
  (dolist (draft-buf (wl-collect-draft))
    (bury-buffer draft-buf))
  (delete-windows-on wl-folder-buffer-name t))

(defun wl-folder-info-save ()
  (when (and wl-folder-info-save
	     wl-folder-info-alist-modified)
    (let ((entities (list wl-folder-entity))
	  entity entity-stack info-alist info)
      (while entities
	(setq entity (wl-pop entities))
	(cond
	 ((consp entity)
	  (and entities
	       (wl-push entities entity-stack))
	  (setq entities (nth 2 entity)))
	 ((stringp entity)
	  (when (and (setq info (elmo-folder-get-info
				 (wl-folder-get-elmo-folder entity)))
		     (not (equal info '(nil))))
	    (if (listp info)
		(wl-append info-alist (list (list (elmo-string entity)
						  (list (nth 3 info)  ;; max
							(nth 2 info)  ;; length
							(nth 0 info)  ;; new
							(nth 1 info)) ;; unread
						  )))))))
	(unless entities
	  (setq entities (wl-pop entity-stack))))
      (elmo-msgdb-finfo-save info-alist)
      (setq wl-folder-info-alist-modified nil))))

(defun wl-folder-goto-first-unread-folder (&optional arg)
  (interactive "P")
  (let ((entities (list wl-folder-entity))
	entity entity-stack ret-val
	first-entity finfo)
    (setq first-entity
	  (catch 'done
	    (while entities
	      (setq entity (wl-pop entities))
	      (cond
	       ((consp entity)
		(and entities
		     (wl-push entities entity-stack))
		(setq entities (nth 2 entity)))
	       ((stringp entity)
		(if (and (setq finfo (wl-folder-get-entity-info entity))
			 (and (nth 0 finfo)(nth 1 finfo))
			 (> (+ (nth 0 finfo)(nth 1 finfo)) 0))
		    (throw 'done entity))
		(wl-append ret-val (list entity))))
	      (unless entities
		(setq entities (wl-pop entity-stack))))))
    (if first-entity
	(progn
	  (when arg
	    (wl-folder-jump-folder first-entity)
	    (sit-for 0))
	  (wl-folder-goto-folder-subr first-entity))
      (message "No unread folder"))))

(defun wl-folder-jump-folder (&optional fld-name noopen)
  (interactive)
  (if (not fld-name)
      (setq fld-name (wl-summary-read-folder wl-default-folder)))
  (goto-char (point-min))
  (if (not noopen)
      (wl-folder-open-folder fld-name))
  (and (wl-folder-buffer-search-entity fld-name)
       (beginning-of-line)))

(defun wl-folder-get-entity-list (entity)
  (let ((entities (list entity))
	entity-stack ret-val)
    (while entities
      (setq entity (wl-pop entities))
      (cond
       ((consp entity)
	(and entities
	     (wl-push entities entity-stack))
	(setq entities (nth 2 entity)))
       ((stringp entity)
	(wl-append ret-val (list entity))))
      (unless entities
	(setq entities (wl-pop entity-stack))))
    ret-val))

(defun wl-folder-open-unread-folder (entity)
  (save-excursion
    (let ((alist (wl-folder-get-entity-list entity))
	  (unread 0)
	  finfo path-list path id)
      (while alist
	(when (and (setq finfo (wl-folder-get-entity-info (car alist)))
		   (nth 0 finfo) (nth 1 finfo)
		   (> (+ (nth 0 finfo)(nth 1 finfo)) 0))
	  (setq unread (+ unread (+ (nth 0 finfo)(nth 1 finfo))))
	  (setq id (wl-folder-get-entity-id (car alist)))
	  (setq path (delete id (wl-folder-get-path
				 wl-folder-entity
				 id
				 (car alist))))
	  (if (not (member path path-list))
	      (wl-append path-list (list path))))
	(setq alist (cdr alist)))
      (while path-list
	(wl-folder-open-folder-sub (car path-list))
	(setq path-list (cdr path-list)))
      (message "%s unread message(s)"
	       (if (> unread 0) unread "No")))))

(defun wl-folder-open-unread-current-entity ()
  (interactive)
  (let ((entity-name (wl-folder-get-entity-from-buffer))
	(group (wl-folder-buffer-group-p)))
    (when entity-name
      (wl-folder-open-unread-folder
       (if group
	   (wl-folder-search-group-entity-by-name entity-name
						  wl-folder-entity)
	 entity-name)))))

(defun wl-folder-open-only-unread-folder ()
  (interactive)
  (let ((id (progn
	      (wl-folder-prev-entity-skip-invalid t)
	      (wl-folder-get-entity-from-buffer t))))
    (wl-folder-open-all-unread-folder)
    (save-excursion
      (goto-char (point-max))
      (while (and (re-search-backward
		   "^[ ]*\\[[-]\\].+:0/0/[0-9-]+" nil t)
		  (not (bobp)))
	(wl-folder-jump-to-current-entity) ;; close it
	))
    (wl-folder-move-path id)
    (recenter)))

(defun wl-folder-open-all-unread-folder (&optional arg)
  (interactive "P")
  (let ((id (progn
	      (wl-folder-prev-entity-skip-invalid t)
	      (wl-folder-get-entity-from-buffer t))))
    (wl-folder-open-unread-folder wl-folder-entity)
    (if (not arg)
	(wl-folder-move-path id)
      (goto-char (point-min))
      (wl-folder-next-unread t))))

(defun wl-folder-open-folder (&optional fld-name)
  (interactive)
  (if (not fld-name)
      (setq fld-name (wl-summary-read-folder wl-default-folder)))
  (let ((entity (wl-folder-search-entity-by-name
		 fld-name wl-folder-entity 'folder)))
    (if entity
	(let* ((id (wl-folder-get-entity-id entity))
	       (path (and id (wl-folder-get-path wl-folder-entity id))))
	  (if path (wl-folder-open-folder-sub path)))
      (message "%s: not found" fld-name))))

(defun wl-folder-open-folder-sub (path)
  (let ((inhibit-read-only t)
	(buffer-read-only nil)
	indent name entity
	err)
    (save-excursion
      (goto-char (point-min))
      (while (and path
		  (wl-folder-buffer-search-group
		   (wl-folder-get-petname
		    (if (stringp (car path))
			(car path)
		      (wl-folder-get-folder-name-by-id
		       (car path))))))
	(beginning-of-line)
	(setq path (cdr path))
	(if (and (wl-folder-buffer-group-p)
		  (looking-at wl-folder-group-regexp)
		 (string= "+" (wl-match-buffer 2)));; closed group
	    (save-excursion
	      (setq indent (wl-match-buffer 1))
	      (setq name (wl-folder-get-entity-from-buffer))
	      (setq entity (wl-folder-search-group-entity-by-name
			    name
			    wl-folder-entity))
	      ;; insert as opened
	      (setcdr (assoc (car entity) wl-folder-group-alist) t)
	      (if (eq 'access (cadr entity))
		  (wl-folder-maybe-load-folder-list entity))
	      (wl-folder-insert-entity indent entity)
	      (delete-region (save-excursion (beginning-of-line)
					     (point))
			     (save-excursion (end-of-line)
					     (+ 1 (point)))))))
      (set-buffer-modified-p nil))))

(defun wl-folder-open-all-pre ()
  (let ((entities (list wl-folder-entity))
	entity entity-stack group-entry)
    (while entities
      (setq entity (wl-pop entities))
      (cond
       ((consp entity)
	(unless (or (not (setq group-entry
			       (assoc (car entity) wl-folder-group-alist)))
		    (cdr group-entry))
	  (setcdr group-entry t)
	  (when (eq 'access (cadr entity))
	    (wl-folder-maybe-load-folder-list entity)))
	(and entities
	     (wl-push entities entity-stack))
	(setq entities (nth 2 entity))))
      (unless entities
	(setq entities (wl-pop entity-stack))))))

(defun wl-folder-open-all (&optional refresh)
  (interactive "P")
  (let* ((inhibit-read-only t)
	 (buffer-read-only nil)
	 (len (length wl-folder-group-alist))
	 (i 0)
	 indent name entity)
    (if refresh
	(let ((id (progn
		    (wl-folder-prev-entity-skip-invalid t)
		    (wl-folder-get-entity-from-buffer t)))
	      (alist wl-folder-group-alist))
	  (while alist
	    (setcdr (pop alist) t))
	  (erase-buffer)
	  (wl-folder-insert-entity " " wl-folder-entity)
	  (wl-folder-move-path id))
      (elmo-with-progress-display
	  (wl-folder-open-all (length wl-folder-group-alist))
	  "Opening all folders"
	(wl-folder-open-all-pre)
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward
		  "^\\([ ]*\\)\\[\\([+]\\)\\]\\(.+\\):[-0-9-]+/[0-9-]+/[0-9-]+$"
		  nil t)
	    (setq indent (wl-match-buffer 1))
	    (setq name (wl-folder-get-entity-from-buffer))
	    (setq entity (wl-folder-search-group-entity-by-name
			  name
			  wl-folder-entity))
	    ;; insert as opened
	    (setcdr (assoc (car entity) wl-folder-group-alist) t)
	    (beginning-of-line)
	    (wl-folder-insert-entity indent entity)
	    (delete-region (save-excursion (beginning-of-line)
					   (point))
			   (save-excursion (end-of-line)
					   (+ 1 (point))))
	    (elmo-progress-notify 'wl-folder-open-all)))))
    (wl-highlight-folder-path wl-folder-buffer-cur-path)
    (set-buffer-modified-p nil)))

(defun wl-folder-close-all ()
  (interactive)
  (let ((inhibit-read-only t)
	(buffer-read-only nil)
	(alist wl-folder-group-alist)
	(id (progn
	      (wl-folder-prev-entity-skip-invalid t)
	      (wl-folder-get-entity-from-buffer t))))
    (while alist
      (setcdr (car alist) nil)
      (setq alist (cdr alist)))
    (setcdr (assoc (car wl-folder-entity) wl-folder-group-alist) t)
    (erase-buffer)
    (wl-folder-insert-entity " " wl-folder-entity)
    (wl-folder-move-path id)
    (wl-highlight-folder-path wl-folder-buffer-cur-path)
    (recenter)
    (set-buffer-modified-p nil)))

(defun wl-folder-open-close ()
  "Open or close parent entity."
  (interactive)
  (unless (wl-folder-get-entity-from-buffer)
    (error "No folder"))
  (beginning-of-line)
  (if (wl-folder-buffer-group-p)
      ;; if group (whether opend or closed.)
      (wl-folder-jump-to-current-entity)
    ;; if folder
    (let (indent)
      (setq indent (save-excursion
		     (re-search-forward "\\([ ]*\\)." nil t)
		     (wl-match-buffer 1)))
      (while (looking-at indent)
	(forward-line -1)))
    (wl-folder-jump-to-current-entity)))

(defsubst wl-folder-access-subscribe-p (group folder)
  (let (subscr regexp match)
    (if (setq subscr (wl-get-assoc-list-value
		      wl-folder-access-subscribe-alist
		      group))
	(progn
	  (setq regexp (mapconcat 'identity (cdr subscr) "\\|"))
	  (setq match (string-match regexp folder))
	  (if (car subscr)
	      match
	    (not match)))
      t)))

(defun wl-folder-update-access-group (entity new-flist)
  (let ((flist (nth 2 entity))
	(unsubscribes (nth 3 entity))
	diff new-unsubscribes removes
	subscribed-list folder group entry)
    (elmo-with-progress-display
	(wl-folder-update-access-group (+ (length flist) (length unsubscribes)))
	"Updating access group"
      ;; check subscribed groups
      (while flist
	(cond
	 ((listp (car flist))	;; group
	  (setq group (elmo-string (caar flist)))
	  (cond
	   ((assoc group new-flist)	;; found in new-flist
	    (setq new-flist (delete (assoc group new-flist)
				    new-flist))
	    (if (wl-folder-access-subscribe-p (car entity) group)
		(wl-append subscribed-list (list (car flist)))
	      (wl-append new-unsubscribes (list (car flist)))
	      (setq diff t)))
	   (t
	    (setq wl-folder-group-alist
		  (delete (wl-string-assoc group wl-folder-group-alist)
			  wl-folder-group-alist))
	    (wl-append removes (list (list group))))))
	 (t			;; folder
	  (setq folder (elmo-string (car flist)))
	  (cond
	   ((member folder new-flist)	;; found in new-flist
	    (setq new-flist (delete folder new-flist))
	    (if (wl-folder-access-subscribe-p (car entity) folder)
		(wl-append subscribed-list (list (car flist)))
	      (wl-append new-unsubscribes (list folder))
	      (setq diff t)))
	   (t
	    (wl-append removes (list folder))))))
	(elmo-progress-notify 'wl-folder-update-access-group)
	(setq flist (cdr flist)))
      ;; check unsubscribed groups
      (while unsubscribes
	(cond
	 ((listp (car unsubscribes))
	  (when (setq entry (assoc (caar unsubscribes) new-flist))
	    (setq new-flist (delete entry new-flist))
	    (wl-append new-unsubscribes (list (car unsubscribes)))))
	 (t
	  (when (member (car unsubscribes) new-flist)
	    (setq new-flist (delete (car unsubscribes) new-flist))
	    (wl-append new-unsubscribes (list (car unsubscribes))))))
	(elmo-progress-notify 'wl-folder-update-access-group)
	(setq unsubscribes (cdr unsubscribes)))
      ;;
      (if (or new-flist removes)
	  (setq diff t))
      (setq new-flist
	    (mapcar (lambda (x)
		      (cond ((consp x) (list (car x) 'access))
			    (t x)))
		    new-flist))
      ;; check new groups
      (let ((new-list new-flist))
	(while new-list
	  (if (not (wl-folder-access-subscribe-p
		    (car entity)
		    (if (listp (car new-list))
			(caar new-list)
		      (car new-list))))
	      ;; auto unsubscribe
	      (progn
		(wl-append new-unsubscribes (list (car new-list)))
		(setq new-flist (delete (car new-list) new-flist)))
	    (cond
	     ((listp (car new-list))
	      ;; check group exists
	      (if (wl-string-assoc (caar new-list) wl-folder-group-alist)
		  (progn
		    (message "%s: group already exists." (caar new-list))
		    (sit-for 1)
		    (wl-append new-unsubscribes (list (car new-list)))
		    (setq new-flist (delete (car new-list) new-flist)))
		(wl-append wl-folder-group-alist
			   (list (cons (caar new-list) nil)))))))
	  (setq new-list (cdr new-list)))))
    (when new-flist
      (message "%d new folder(s)." (length new-flist)))
    (wl-append new-flist subscribed-list)	;; new is first
    (run-hooks 'wl-folder-update-access-group-hook)
    (setcdr (cdr entity) (list new-flist new-unsubscribes))
    (list diff new-flist new-unsubscribes removes)))

(defun wl-folder-prefetch-entity (entity)
  "Prefetch all new messages in the ENTITY."
  (cond
   ((consp entity)
    (let ((flist (nth 2 entity))
	  (sum-done 0)
	  (sum-all 0)
	  result)
      (while flist
	(setq result (wl-folder-prefetch-entity (car flist)))
	(setq sum-done (+ sum-done (car result)))
	(setq sum-all (+ sum-all (cdr result)))
	(setq flist (cdr flist)))
      (message "Prefetched %d/%d message(s) in \"%s\"."
	       sum-done sum-all
	       (wl-folder-get-petname (car entity)))
      (cons sum-done sum-all)))
   ((stringp entity)
    (let* ((folder (wl-folder-get-elmo-folder entity))
	   (nums (wl-folder-get-entity-info entity))
	   (wl-summary-highlight (if (or (wl-summary-sticky-p folder)
					 (wl-summary-always-sticky-folder-p
					  folder))
				     wl-summary-highlight))
	   wl-summary-exit-next-move
	   wl-auto-select-first ret-val
	   count sticky)
      (setq count (or (car nums) 0))
      (setq count (+ count (wl-folder-count-incorporates folder)))
      (if (or (null (car nums)) ; unknown
	      (< 0 count))
	  (save-window-excursion
	    (save-excursion
	      (let ((wl-summary-buffer-name
		     (if (setq sticky (get-buffer
				       (wl-summary-sticky-buffer-name
					(elmo-folder-name-internal
					 folder))))
			 ;; Sticky folder exists.
			 (wl-summary-sticky-buffer-name
			  (elmo-folder-name-internal folder))
		       (concat
			wl-summary-buffer-name
			(symbol-name this-command))))
		    (wl-summary-use-frame nil)
		    (wl-summary-always-sticky-folder-list nil))
		(wl-summary-goto-folder-subr entity
					     (wl-summary-get-sync-range
					      folder)
					     nil)
		(setq ret-val (wl-summary-incorporate))
		(if sticky
		    (wl-summary-save-status)
		  (wl-summary-exit))
		ret-val)))
	(cons 0 0))))))

(defun wl-folder-count-incorporates (folder)
  (let ((sum 0))
    (dolist (number (elmo-folder-list-flagged folder 'any))
      (when (member (wl-summary-message-mark folder number)
		    wl-summary-incorporate-marks)
	(incf sum)))
    sum))

(defun wl-folder-prefetch-current-entity (&optional no-check)
  "Prefetch all uncached messages in the folder at position.
If current line is group folder, all subfolders are prefetched."
  (interactive "P")
  (save-excursion
    (let ((entity-name (wl-folder-get-entity-from-buffer))
	  (group (wl-folder-buffer-group-p))
	  wl-folder-check-entity-hook
	  summary-buf entity)
      (when entity-name
	(setq entity
	      (if group
		  (wl-folder-search-group-entity-by-name entity-name
							 wl-folder-entity)
		entity-name))
	(if (not no-check)
	    (wl-folder-check-entity entity))
	(wl-folder-prefetch-entity entity)))))

;;;(defun wl-folder-drop-unsync-entity (entity)
;;;  "Drop all unsync messages in the ENTITY."
;;;  (cond
;;;   ((consp entity)
;;;    (let ((flist (nth 2 entity)))
;;;      (while flist
;;;	(wl-folder-drop-unsync-entity (car flist))
;;;	(setq flist (cdr flist)))))
;;;   ((stringp entity)
;;;    (let ((nums (wl-folder-get-entity-info entity))
;;;	  wl-summary-highlight wl-auto-select-first new)
;;;      (setq new (or (car nums) 0))
;;;      (if (< 0 new)
;;;	  (save-window-excursion
;;;	    (save-excursion
;;;	      (let ((wl-summary-buffer-name (concat
;;;					     wl-summary-buffer-name
;;;					     (symbol-name this-command))))
;;;		(wl-summary-goto-folder-subr entity 'no-sync nil)
;;;		(wl-summary-drop-unsync)
;;;		(wl-summary-exit)))))))))

;;;(defun wl-folder-drop-unsync-current-entity (&optional force-check)
;;;  "Drop all unsync messages in the folder at position.
;;;If current line is group folder, all subfolders are dropped.
;;;If optional arg exists, don't check any folders."
;;;  (interactive "P")
;;;  (save-excursion
;;;    (let ((entity-name (wl-folder-get-entity-from-buffer))
;;;	  (group (wl-folder-buffer-group-p))
;;;	  wl-folder-check-entity-hook
;;;	  summary-buf entity)
;;;      (when (and entity-name
;;;		 (y-or-n-p (format
;;;			    "Drop all unsync messages in %s? " entity-name)))
;;;	(setq entity
;;;	      (if group
;;;		  (wl-folder-search-group-entity-by-name entity-name
;;;							 wl-folder-entity)
;;;		entity-name))
;;;	(if (null force-check)
;;;	    (wl-folder-check-entity entity))
;;;	(wl-folder-drop-unsync-entity entity)
;;;	(message "All unsync messages in %s are dropped!" entity-name)))))

(defun wl-folder-write-current-folder ()
  "Write message to current folder's newsgroup or mailing-list.
Call `wl-summary-write-current-folder' with current folder name."
  (interactive)
  (unless (wl-folder-buffer-group-p)
    (wl-summary-write-current-folder
     (wl-folder-get-entity-from-buffer))))

(defun wl-folder-mimic-kill-buffer ()
  "Kill the current (Folder) buffer with query."
  (interactive)
  (let ((bufname (read-buffer (format "Kill buffer: (default %s) "
				      (buffer-name))))
	wl-interactive-exit)
    (if (or (not bufname)
	    (string-equal bufname "")
	    (string-equal bufname (buffer-name)))
	(wl-exit)
      (kill-buffer bufname))))

(defun wl-folder-create-subr (folder)
  (let ((name (elmo-folder-name-internal folder)))
    (unless (elmo-folder-creatable-p folder)
      (error "Folder %s does not exist" name))
    (unless (y-or-n-p (format "Folder %s does not exist, create it? " name))
      (error "Folder %s is not created" name))
    (message "")
    (setq wl-folder-entity-hashtb
	  (wl-folder-create-entity-hashtb name wl-folder-entity-hashtb))
    (unless (elmo-folder-create folder)
      (error "Create folder failed"))))

(defun wl-folder-confirm-existence (folder &optional force)
  (if force
      (unless (elmo-folder-exists-p folder)
	(wl-folder-create-subr folder))
    (unless (or (wl-folder-entity-exists-p (elmo-folder-name-internal folder))
		(and (elmo-folder-msgdb-path folder)
		     (file-exists-p (elmo-folder-msgdb-path folder)))
		(elmo-folder-exists-p folder))
      (wl-folder-create-subr folder))))

(defun wl-folder-virtual ()
  "Goto virtual folder."
  (interactive)
  (let ((entity (wl-folder-get-entity-from-buffer)))
    (if (wl-folder-buffer-group-p)
	(setq entity
	      (concat
	       "*"
	       (mapconcat 'identity
			  (wl-folder-get-entity-list
			   (wl-folder-search-group-entity-by-name
			    entity
			    wl-folder-entity)) ","))))
    (unless entity (error "No folder"))
    (wl-folder-goto-folder-subr
     (concat "/"
	     (wl-read-search-condition
	      wl-fldmgr-make-filter-default)
	     "/" entity))))

(defun wl-folder-pick ()
  (interactive)
  (save-excursion
    (let* ((condition (car (elmo-parse-search-condition
			    (wl-read-search-condition
			     wl-summary-pick-field-default))))
	   (entity (wl-folder-get-entity-from-buffer))
	   (folder-list
	    (if (wl-folder-buffer-group-p)
		(wl-folder-get-entity-list
		 (wl-folder-search-group-entity-by-name
		  entity
		  wl-folder-entity))
	      (list entity)))
	   results ret)
      (while (car folder-list)
	(setq ret (elmo-folder-search
		   (wl-folder-get-elmo-folder (car folder-list))
		   condition
		   t))
	(if ret
	    (setq results
		  (append results
			  (list (cons (car folder-list) ret)))))
	(setq folder-list (cdr folder-list)))
      (if results
	  (message "%s are picked."
		   (mapconcat (lambda (res)
				(format "%s(%d)"
					(car res)
					(length (cdr res))))
			      results
			      ","))
	(message "No message was picked.")))))

(defun wl-folder-jump-to-next-summary ()
  (interactive)
  (when (wl-collect-summary)
    (if (get-buffer-window (car (wl-collect-summary)))
	(switch-to-buffer-other-window (car (wl-collect-summary))))
    (wl-summary-next-buffer)))

(defun wl-folder-jump-to-previous-summary ()
  (interactive)
  (when (wl-collect-summary)
    (if (get-buffer-window (car (wl-collect-summary)))
	(switch-to-buffer-other-window (car (wl-collect-summary))))
    (wl-summary-previous-buffer)))

;;;
;; Completion
(defvar wl-folder-complete-folder-candidate nil)

(defun wl-folder-complete-folder (string predicate flag)
  (cond ((or (string-match "^\\(/[^/]*/\\)\\(.*\\)$" string) ; filter
	     (string-match "^\\(\*\\|\*.*,\\)\\([^,]*\\)$" string) ; multi
	     (string-match "^\\(|[^|]*|:?\\)\\(.*\\)$" string) ;pipe-src
	     (string-match "^\\(|\\)\\([^|]*\\)$" string)) ;pipe-dst
	 (let* ((str1 (match-string 1 string))
		(str2 (match-string 2 string))
		(str2-comp (wl-folder-complete-folder str2 predicate flag)))
	   (cond
	    ((listp str2-comp) ; flag=t
	     (mapcar (lambda (x) (concat str1 x)) str2-comp))
	    ((stringp str2-comp)
	     (concat str1 str2-comp))
	    (t
	     str2-comp))))
	((string-match "^\\(/\\)\\([^/]*\\)$" string) ; filter-condition
	 (let* ((str1 (match-string 1 string))
		(str2 (match-string 2 string))
		(str2-comp
		 (wl-folder-complete-filter-condition str2 predicate flag)))
	   (cond
	    ((listp str2-comp) ; flag=t
	     (mapcar (lambda (x) (concat str1 x)) str2-comp))
	    ((stringp str2-comp)
	     (concat str1 str2-comp))
	    (t
	     str2-comp))))
	(t
	 (let ((candidate
		(or wl-folder-complete-folder-candidate
		    (if (memq 'read-folder wl-use-folder-petname)
			(wl-folder-get-entity-with-petname)
		      wl-folder-entity-hashtb))))
	   (if (not flag)
	       (try-completion string candidate)
	     (all-completions string candidate))))))

(defun wl-folder-complete-filter-condition (string predicate flag)
  (cond
   ((string-match "^\\(.*|\\|.*&\\|.*(\\)\\([^:]*\\)$" string)
    (let* ((str1 (match-string 1 string))
	   (str2 (match-string 2 string))
	   (str2-comp
	    (wl-folder-complete-filter-condition str2 predicate flag)))
      (cond
       ((listp str2-comp) ; flag=t
	(mapcar (lambda (x) (concat str1 x)) str2-comp))
       ((stringp str2-comp)
	(concat str1 str2-comp))
       (t
	str2-comp))))
   (t
    (let ((candidate
	   (mapcar (lambda (x) (list (concat (downcase x) ":")))
		   (wl-search-condition-fields))))
      (if (not flag)
	  (try-completion string candidate)
	(all-completions string candidate))))))

(require 'product)
(product-provide (provide 'wl-folder) (require 'wl-version))

;;; wl-folder.el ends here
