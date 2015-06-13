;;; wl-fldmgr.el --- Folder manager for Wanderlust.

;; Copyright 1998,1999,2000 Masahiro MURATA <muse@ba2.so-net.ne.jp>
;;                          Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Masahiro MURATA <muse@ba2.so-net.ne.jp>
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

(require 'wl-folder)
(require 'wl-summary)
(require 'wl-highlight)
(require 'wl-version)
(eval-when-compile
  (require 'wl-util))

;;; Global Variable

(defvar wl-fldmgr-modified nil)
(defvar wl-fldmgr-modified-access-list nil)
(defvar wl-fldmgr-cut-entity-list nil)
(defvar wl-fldmgr-entity-list nil)
(defvar wl-fldmgr-group-insert-opened nil)

(defconst wl-fldmgr-folders-header
  (format
   "#
# Folder definition file
# This file is generated automatically by %s.
#
# If you edit this file by hand, be sure that comment lines
# will be washed out by wl-fldmgr.
#

" (product-string-1 'wl-version t)))

;;; Initial setup

(defvar wl-fldmgr-mode-map nil)
(if wl-fldmgr-mode-map
    nil
  (define-prefix-command 'wl-fldmgr-mode-map)
  (define-key wl-fldmgr-mode-map "\C-s"   'wl-fldmgr-save-folders)
  (define-key wl-fldmgr-mode-map "m"      'wl-fldmgr-make-multi)
  (define-key wl-fldmgr-mode-map "g"      'wl-fldmgr-make-group)
  (define-key wl-fldmgr-mode-map "A"      'wl-fldmgr-make-access-group)
  (define-key wl-fldmgr-mode-map "f"      'wl-fldmgr-make-filter)
  (define-key wl-fldmgr-mode-map "p"      'wl-fldmgr-set-petname)
  (define-key wl-fldmgr-mode-map "a"      'wl-fldmgr-add)
  (define-key wl-fldmgr-mode-map "d"      'wl-fldmgr-delete)
  (define-key wl-fldmgr-mode-map "R"      'wl-fldmgr-rename)
  (define-key wl-fldmgr-mode-map "c"      'wl-fldmgr-copy)
  (define-key wl-fldmgr-mode-map "k"      'wl-fldmgr-cut)
  (define-key wl-fldmgr-mode-map "W"      'wl-fldmgr-copy-region)
  (define-key wl-fldmgr-mode-map "\C-w"   'wl-fldmgr-cut-region)
  (define-key wl-fldmgr-mode-map "y"      'wl-fldmgr-yank)
  (define-key wl-fldmgr-mode-map "s"      'wl-fldmgr-sort)
  (define-key wl-fldmgr-mode-map "l"      'wl-fldmgr-access-display-normal)
  (define-key wl-fldmgr-mode-map "L"      'wl-fldmgr-access-display-all)
  (define-key wl-fldmgr-mode-map "q"      'wl-fldmgr-clear-cut-entity-list)
  (define-key wl-fldmgr-mode-map "r"      'wl-fldmgr-reconst-entity-hashtb)
  (define-key wl-fldmgr-mode-map "u"      'wl-fldmgr-unsubscribe)
  (define-key wl-fldmgr-mode-map "U"      'wl-fldmgr-unsubscribe-region))

(add-hook 'wl-folder-mode-hook 'wl-fldmgr-init)

(defun wl-fldmgr-init ()
  (setq wl-fldmgr-cut-entity-list nil)
  (setq wl-fldmgr-modified nil)
  (setq wl-fldmgr-modified-access-list nil))

(defun wl-fldmgr-exit ()
  (when (and wl-fldmgr-modified
	     (or (not wl-interactive-save-folders)
		 (y-or-n-p
		  (concat "Folder view was modified"
			  (and wl-fldmgr-cut-entity-list
			       (format " (%s in cut stack)"
				       (length wl-fldmgr-cut-entity-list)))
			  ".  Save current folders? "))))
    (wl-fldmgr-save-folders)))

;;; Macro and misc Function
;;

(defun wl-fldmgr-delete-line ()
  (delete-region (point-at-bol) (1+ (point-at-eol))))

(defun wl-fldmgr-make-indent (level)
  (concat " " (make-string (* 2 level) (string-to-char " "))))

(defmacro wl-fldmgr-get-entity-id (&optional entity)
  `(get-text-property (if ,entity
			  0
			(point))
		      'wl-folder-entity-id
		      ,entity))

(defmacro wl-fldmgr-assign-id (entity &optional id)
  `(let ((entity-id (or ,id wl-folder-entity-id)))
     (put-text-property 0 (length ,entity)
			'wl-folder-entity-id
			entity-id
			,entity)))

(defsubst wl-fldmgr-read-string (str)
  (if (string-match "\n" str)
      (error "Not supported name: %s" str)
    (elmo-string str)))

(defsubst wl-fldmgr-add-modified-access-list (group)
  (if (not (member group wl-fldmgr-modified-access-list))
      (wl-append wl-fldmgr-modified-access-list (list group))))

(defsubst wl-fldmgr-delete-modified-access-list (group)
  (if (member group wl-fldmgr-modified-access-list)
      (setq wl-fldmgr-modified-access-list
	    (delete group wl-fldmgr-modified-access-list))))

(defsubst wl-fldmgr-add-group (group)
  (or (assoc group wl-folder-group-alist)
      (wl-append wl-folder-group-alist
		 (list (cons group
			     wl-fldmgr-group-insert-opened)))))

(defsubst wl-fldmgr-delete-group (group)
  (wl-fldmgr-delete-modified-access-list group)
  (setq wl-folder-group-alist
	(delete (assoc group wl-folder-group-alist)
		wl-folder-group-alist)))

(defun wl-fldmgr-add-entity-hashtb (entities)
  "Update `wl-folder-entity-hashtb', `wl-folder-newsgroups-hashtb'.
Return value is diffs '(new unread all)."
  (let* ((new-diff 0)
	 (unread-diff 0)
	 (all-diff 0)
	 val entity entity-stack)
    (setq wl-folder-newsgroups-hashtb
	  (or (wl-folder-create-newsgroups-hashtb entities t)
	      wl-folder-newsgroups-hashtb))
    (while entities
      (setq entity (wl-pop entities))
      (cond
       ((consp entity)
	(wl-fldmgr-add-group (car entity))
	(and entities
	     (wl-push entities entity-stack))
	(setq entities (nth 2 entity)))
       ((stringp entity)
	(if (not (setq val (wl-folder-get-entity-info entity)))
	    (wl-folder-set-entity-info entity nil)
	  (setq new-diff    (+ new-diff    (or (nth 0 val) 0)))
	  (setq unread-diff (+ unread-diff (or (nth 1 val) 0)))
	  (setq all-diff    (+ all-diff    (or (nth 2 val) 0))))))
      (unless entities
	(setq entities (wl-pop entity-stack))))
    (setq unread-diff (+ unread-diff new-diff))
    (list new-diff unread-diff all-diff)))

(defun wl-fldmgr-delete-entity-hashtb (entities &optional clear)
  "Update `wl-folder-entity-hashtb'.
return value is diffs '(-new -unread -all)."
  (let* ((new-diff 0)
	 (unread-diff 0)
	 (all-diff 0)
	 entity val
	 entity-stack)
    (while entities
      (setq entity (wl-pop entities))
      (cond
       ((consp entity)
	(wl-fldmgr-delete-group (car entity))
	(and entities
	     (wl-push entities entity-stack))
	(setq entities (nth 2 entity)))
       ((stringp entity)
	(when (setq val (wl-folder-get-entity-info entity))
	  (setq new-diff    (+ new-diff    (or (nth 0 val) 0)))
	  (setq unread-diff (+ unread-diff (or (nth 1 val) 0)))
	  (setq all-diff    (+ all-diff    (or (nth 2 val) 0)))
	  (and clear (wl-folder-clear-entity-info entity)))))
      (unless entities
	(setq entities (wl-pop entity-stack))))
    (setq unread-diff (+ unread-diff new-diff))
    (list (- new-diff) (- unread-diff) (- all-diff))))

;; return value
;; example: '(("Desktop" group) ("+ml" access) "+ml/wl")

(defun wl-fldmgr-get-path (entity target-entity &optional group-target)
  (let* ((target-id (wl-fldmgr-get-entity-id target-entity))
	 (entities (list entity))
	 entity-stack result-path)
    (reverse
     (catch 'done
       (while entities
	 (setq entity (wl-pop entities))
	 (cond
	  ((consp entity)
	   (if (and (string= target-entity (car entity))
		    (eq target-id (wl-fldmgr-get-entity-id (car entity))))
	       (throw 'done
		      (wl-push (if group-target
				   (car entity)
				 (list (car entity) (nth 1 entity)))
			       result-path))
	     (wl-push (list (car entity) (nth 1 entity))
		      result-path))
	   (wl-push entities entity-stack)
	   (setq entities (nth 2 entity)))
	  ((stringp entity)
	   (if (and (string= target-entity entity)
		    (eq target-id (wl-fldmgr-get-entity-id entity)))
	       (throw 'done
		      (wl-push entity result-path)))))
	 (unless entities
	   (while (and entity-stack
		       (not entities))
	     (setq result-path (cdr result-path))
	     (setq entities (wl-pop entity-stack)))))))))

;;;(defun wl-fldmgr-get-previous-entity (entity key-id)
;;;  (cdr (wl-fldmgr-get-previous-entity-internal '(nil . nil) entity key-id)))
;;;
;;;(defun wl-fldmgr-get-previous-entity-internal (result entity key-id)
;;;  (cond
;;;   ((stringp entity)
;;;    (if (eq key-id (wl-fldmgr-get-entity-id entity))
;;;	(cons t result)
;;;      (cons nil (cons entity entity))))
;;;   ((consp entity)
;;;    (if (eq key-id (wl-fldmgr-get-entity-id (car entity)))
;;;	(cons t result)
;;;      (setcar result (car entity))
;;;      (let ((flist (nth 2 entity))
;;;	    return found)
;;;	(while (and flist (not found))
;;;	  (if (car (setq return
;;;			 (wl-fldmgr-get-previous-entity-internal
;;;			  result (car flist) key-id)))
;;;	      (setq found t))
;;;	  (setq result (cdr return))
;;;	  (setq flist (cdr flist)))
;;;	(cons found result))))))

;; path is get `wl-fldmgr-get-path-from-buffer'.
(defun wl-fldmgr-update-group (path diffs)
  (save-excursion
    (while (and path (consp (car path)))
      (if (string= (caar path) wl-folder-desktop-name) ; update desktop
	  (progn
	    (goto-char (point-min))
	    (wl-folder-update-diff-line diffs))
	;; goto the path line.
	(goto-char (point-min))
	(if (wl-folder-buffer-search-group
	     (wl-folder-get-petname (caar path)))
	    (wl-folder-update-diff-line diffs)))
      (setq path (cdr path)))))

;;; Function for wl-folder-entity
;;

;; usage:
;; (wl-delete-entity '(("Desktop") ("ML") "+ml/wl") '("+ml/wl") wl-folder-entity)
;; (wl-delete-entity '(("Desktop") "ML") '("+inbox" "ML") wl-folder-entity)
;; (wl-delete-entity '(("Desktop") "ML") nil wl-folder-entity)

(defun wl-delete-entity (key-path delete-list entity &optional clear)
  (let (wl-fldmgr-entity-list)
    (when (and (string= (caar key-path) (car entity))
	       (wl-delete-entity-sub (cdr key-path) delete-list entity clear))
      ;; return value is non-nil (diffs)
      (wl-fldmgr-delete-entity-hashtb wl-fldmgr-entity-list clear))))

(defun wl-delete-entity-sub (key-path delete-list entity clear)
  (let ((flist (nth 2 entity))
	(key (car key-path))
	next)
    (cond
     ((consp key);; into group
      (if (setq next (assoc (car key) flist))
	  (wl-delete-entity-sub (cdr key-path)
				delete-list
				next
				clear)
	;; not found
	nil))
     ((stringp key) ;; delete entities
      (if (not delete-list)
	  (setq delete-list (list key)))
      (let* ((group (car entity))
	     (access (eq (nth 1 entity) 'access))
	     (unsubscribes (and access (nth 3 entity)))
	     (update t)
	     cut-entity is-group)
	(catch 'done
	  (while delete-list
	    (setq key (car delete-list))
	    (cond ((member key flist);; entity
		   (setq flist (delete key flist))
		   (unless clear
		     (wl-push key wl-fldmgr-cut-entity-list))
		   (wl-append wl-fldmgr-entity-list (list key))
		   (setq is-group nil))
		  ((setq cut-entity (assoc key flist));; group
		   (setq flist (delete cut-entity flist))
		   (unless clear
		     (wl-push cut-entity wl-fldmgr-cut-entity-list))
		   (wl-append wl-fldmgr-entity-list (list cut-entity))
		   (setq is-group t))
		  (t
		   ;; not found
		   (message "%s not found" key)
		   (setq update nil)
		   (throw 'done t)))
	    (when (and access (not clear))
	      (if is-group
		  (wl-append unsubscribes
			     (list (list (elmo-string key) 'access nil)))
		(wl-append unsubscribes (list (elmo-string key)))))
	    (setq delete-list (cdr delete-list))))
	(when update
	  (setcdr (cdr entity) (list flist unsubscribes))
	  (when access
	    (wl-fldmgr-add-modified-access-list group))
	  t
	  ))))))

;; usage:
;; (wl-add-entity '(("Desktop") ("ML") "ml/wl") '("+ml/new") wl-folder-entity 12)
;; (wl-add-entity '(("Desktop") "ML") '("+ml/new")  wl-folder-entity 10)

(defun wl-add-entity (key-path new entity prev-entity-id &optional errmes)
  (when (string= (caar key-path) (car entity))
    (let ((entities new))
      (while entities
	(wl-folder-entity-assign-id
	 (pop entities) wl-folder-entity-id-name-hashtb t)))
    (when (wl-add-entity-sub (cdr key-path) new entity errmes)
      ;; return value is non-nil (diffs)
      (wl-fldmgr-add-entity-hashtb new))))

(defun wl-add-entity-sub (key-path new entity &optional errmes)
  (let ((flist (nth 2 entity))
	entry)
    (catch 'success
      (cond
       ((consp (car key-path));; into group
	(if (setq entry (assoc (caar key-path) flist))
	    (if (not (wl-add-entity-sub (cdr key-path)
					new
					entry
					errmes))
		(throw 'success nil))
	  (and errmes (message "%s not found" (caar key-path)))
	  (throw 'success nil)))
       (t;; insert entities
	(let* ((new2 new)
	       (group (car entity))
	       (access (eq (nth 1 entity) 'access))
	       (unsubscribes (and access (nth 3 entity))))
	  ;; check
	  (while new2
	    (cond
	     ((stringp (car new2)) ;; folder
	      (cond
	       ((elmo-string-member (car new2) flist)
		(and errmes (message "%s: already exists" (car new2)))
		(throw 'success nil))
	       ((and access
		     (not (elmo-string-member (car new2) unsubscribes)))
		(and errmes (message "%s: not access group folder" (car new2)))
		(throw 'success nil))))
	     (t			   ;; group
	      (when (and access
			 (not (wl-string-assoc (caar new2) unsubscribes)))
		(and errmes (message "%s: can't insert access group"
				     (caar new2)))
		(throw 'success nil))))
	    (setq new2 (cdr new2)))
	  ;; do it
	  (when access
	    ;; remove from unsubscribe
	    (setq new2 new)
	    (while new2
	      (if (consp (car new2))
		  (setq unsubscribes
			(delq (wl-string-assoc (car (car new2)) unsubscribes)
			      unsubscribes))
		(setq unsubscribes (delete (elmo-string (car new2))
					   unsubscribes)))
	      (setq new2 (cdr new2)))
	    (setcdr (cddr entity) (list unsubscribes))
	    (wl-fldmgr-add-modified-access-list group))
	  (if (not key-path);; insert group top
	      (if (cddr entity)
		  (setcar (cddr entity) (append new flist))
		(setcdr (cdr entity) (list new)))
	    (let (akey)
	      (if (catch 'done
		    (while flist
		      (setq akey (car flist))
		      (cond ((consp akey);; group
			     (if (equal (car key-path) (car akey))
				 (throw 'done t)))
			    (t
			     (if (equal (car key-path) akey)
				 (throw 'done t))))
		      (setq flist (cdr flist))))
		  (setcdr flist (append new (cdr flist)))
		(and errmes (message "%s not found" (car key-path)))
		(throw 'success nil)))))))
      (throw 'success t))))

;; return value is
;; (path indent-level (group . type) previous-entity-id target-entity)
;; previous-entity-id is (id-name-alist-prev-id . entity-alist-prev-id)
;; example:
;; '((("Desktop" group) ("ML" group) "+ml/wl") '(3 2) ("ML" . group) nil "+ml/wl")

(defun wl-fldmgr-get-path-from-buffer (&optional prev)
  (let ((indent-level 0)
	(group-target t)
	folder-path group-type previous-entity entity)
    (save-excursion
      (beginning-of-line)
      (when prev
;;;	(wl-folder-next-entity-skip-invalid t)
;;;	(and (setq previous-entity
;;;		   (wl-fldmgr-get-previous-entity wl-folder-entity
;;;						  (wl-fldmgr-get-entity-id)))
;;;	     ;; change entity to id
;;;	     (setq previous-entity
;;;		   (cons
;;;		    (and (car previous-entity)
;;;			 (wl-fldmgr-get-entity-id (car previous-entity)))
;;;		    (and (cdr previous-entity)
;;;			 (wl-fldmgr-get-entity-id (cdr previous-entity))))))
	(wl-folder-prev-entity-skip-invalid))
      (if (and prev
	       (wl-folder-buffer-group-p)
	       (looking-at wl-folder-group-regexp)
	       (string= (wl-match-buffer 2) "-"))
	  (setq group-target nil)
	(if (and prev (bobp))
	    (error "Out of desktop group")))
      (setq folder-path (wl-fldmgr-get-path wl-folder-entity
					    (wl-folder-get-entity-from-buffer)
;;;					    (wl-fldmgr-get-entity-id)
					    group-target))
      (let ((fp folder-path))
	(while fp
	  (if (consp (car fp))
	      (progn
		(setq indent-level (1+ indent-level))
		(setq group-type (cons (caar fp) (nth 1 (car fp)))))
	    (setq entity (car fp)))
	  (setq fp (cdr fp))))
      (list folder-path indent-level group-type previous-entity entity))))

;;; Command
;;

(defun wl-fldmgr-clear-cut-entity-list ()
  (interactive)
  (setq wl-fldmgr-cut-entity-list nil)
  (message "Cleared cut entity list"))

(defun wl-fldmgr-reconst-entity-hashtb (&optional arg nomes)
  (interactive "P")
  (or nomes (message "Reconstructing entity alist..."))
  (when (not arg)
    (setq wl-folder-entity-id 0)
    (wl-folder-entity-assign-id wl-folder-entity))
  (setq wl-folder-entity-hashtb
	(wl-folder-create-entity-hashtb
	 wl-folder-entity
	 wl-folder-entity-hashtb
	 t))
  ;; reset property on buffer
  (when (not arg)
    (let ((inhibit-read-only t)
	  (cur-point (point)))
      (erase-buffer)
      (wl-folder-insert-entity " " wl-folder-entity)
      (goto-char cur-point)
      (set-buffer-modified-p nil)))
  (or nomes (message "Reconstructing entity alist...done")))


(defun wl-fldmgr-cut-region ()
  (interactive)
  (let* ((p1 (region-beginning))
	 (p2 (region-end))
	 (r1 (progn
	       (goto-char p1)
	       (beginning-of-line)
	       (point)))
	 (r2 (progn
	       (goto-char p2)
	       (beginning-of-line)
	       (point)))
	 (from (min r1 r2))
	 (to (max r1 r2))
	 (count 0)
	 (errmes nil)
	 (cut-list nil)
	 name pre-indent indent)
    (catch 'err
      (save-excursion
	(goto-char from)
	(and (looking-at "^\\([ ]*\\)")
	     (setq pre-indent (wl-match-buffer 1)))
	(while (< (point) to)
	  (and (looking-at "^\\([ ]*\\)")
	       (setq indent (wl-match-buffer 1)))
	  (cond ((= (length pre-indent) (length indent))
		 (setq pre-indent indent)
		 (setq count (1+ count))
		 (and (setq name (wl-folder-get-entity-from-buffer))
		      (wl-append cut-list (list name)))
		 (forward-line))
		((< (length pre-indent) (length indent))
		 (wl-folder-goto-bottom-of-current-folder pre-indent)
		 (beginning-of-line))
		(t
		 (setq errmes "bad region")
		 (throw 'err t))))
	(unless (eq (point) to)
	  (setq errmes "bad region")
	  (throw 'err t)))
      (save-excursion
	(let ((count2 (length cut-list))
	      tmp path ent diffs)
	  (goto-char from)
	  (save-excursion
	    (wl-folder-next-entity-skip-invalid t)
	    (setq tmp (wl-fldmgr-get-path-from-buffer)))
	  (setq path (car tmp))
	  (setq diffs
		(wl-delete-entity path cut-list wl-folder-entity))
	  (catch 'done
	    (while (> count 0)
	      (setq ent (looking-at wl-folder-entity-regexp))
	      (if (not (wl-fldmgr-cut (and ent tmp)
				      (and ent (pop cut-list))))
		  (throw 'done nil))
	      (setq count (1- count))))
	  (if (> count2 0)
	      (wl-push count2 wl-fldmgr-cut-entity-list))
	  (if diffs
	      (wl-fldmgr-update-group path diffs))
	  t))
      (throw 'err nil))
    (if errmes
	(message "%s" errmes))))

(defun wl-fldmgr-cut (&optional tmp entity clear)
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((ret-val nil)
	  (inhibit-read-only t)
	  path diffs)
      (if (bobp)
	  (message "Can't remove desktop group")
	(or tmp (setq tmp (wl-fldmgr-get-path-from-buffer)))
	(setq path (car tmp))
	(if (not path)
	    (if (not (eobp))
		(wl-fldmgr-delete-line))   ;; unsubscribe or removed folder
	  (if (not entity)
	      (setq diffs
		    (wl-delete-entity path nil wl-folder-entity clear)))
	  (setq wl-fldmgr-modified t)
	  ;;
	  (if (and (wl-folder-buffer-group-p)
		   (looking-at wl-folder-group-regexp))
	      ;; group
	      (let (beg end indent opened)
		(setq indent (wl-match-buffer 1))
		(setq opened (wl-match-buffer 2))
		(if (string= opened "+")
		    (wl-fldmgr-delete-line)
		  (setq beg (point))
		  (end-of-line)
		  (save-match-data
		    (setq end
			  (progn
			    (wl-folder-goto-bottom-of-current-folder indent)
			    (beginning-of-line)
			    (point))))
		  (delete-region beg end)))
	    ;; entity
	    (wl-fldmgr-delete-line))
	  (if diffs
	      (wl-fldmgr-update-group path diffs))
	  (set-buffer-modified-p nil))
	(setq ret-val t))
      ret-val)))

(defun wl-fldmgr-copy-region ()
  (interactive)
  (let* ((p1 (region-beginning))
	 (p2 (region-end))
	 (r1 (progn
	       (goto-char p1)
	       (beginning-of-line)
	       (point)))
	 (r2 (progn
	       (goto-char p2)
	       (beginning-of-line)
	       (point)))
	 (from (min r1 r2))
	 (to (max r1 r2))
	 (errmes nil)
	 (cut-list nil)
	 (count 0)
	 name
	 pre-indent indent)
    (catch 'err
      (save-excursion
	(goto-char from)
	(when (bobp)
	  (setq errmes "can't copy desktop group")
	  (throw 'err t))
	(and (looking-at "^\\([ ]*\\)")
	     (setq pre-indent (wl-match-buffer 1)))
	(while (< (point) to)
	  (and (looking-at "^\\([ ]*\\)")
	       (setq indent (wl-match-buffer 1)))
	  (if (wl-folder-buffer-group-p)
	      (progn
		(setq errmes "can't copy group folder")
		(throw 'err t)))
	  (cond ((= (length pre-indent) (length indent))
		 (if (setq name (wl-folder-get-entity-from-buffer))
		     (progn
		       (setq pre-indent indent)
		       (wl-push name cut-list)))
		 (forward-line))
		((< (length pre-indent) (length indent))
		 (wl-folder-goto-bottom-of-current-folder pre-indent)
		 (beginning-of-line))
		(t
		 (setq errmes "bad region")
		 (throw 'err t))))
	(unless (eq (point) to)
	  (setq errmes "bad region")
	  (throw 'err t)))
      (catch 'done
	(setq cut-list (reverse cut-list))
	(while cut-list
	  (setq name (pop cut-list))
	  (unless (wl-fldmgr-copy name)
	    (throw 'done nil))
	  (setq count (1+ count)))
	(wl-push count wl-fldmgr-cut-entity-list)
	(message "Copy %s folders" count)
	(throw 'err nil)))
    (if errmes
	(message "%s" errmes))))

(defun wl-fldmgr-copy (&optional ename)
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    (let ((ret-val nil))
      (if (and (not ename)
	       (wl-folder-buffer-group-p))
	  (message "Can't copy group folder")
	(let* ((name (or ename (wl-folder-get-entity-from-buffer)))
	       (entity (elmo-string name)))
	  (when name
	    (if (member entity wl-fldmgr-cut-entity-list)
		(setq wl-fldmgr-cut-entity-list
		      (delete entity wl-fldmgr-cut-entity-list)))
	    (wl-push entity wl-fldmgr-cut-entity-list)
	    (or ename
		(message "Copy: %s" name))
	    (setq ret-val t))))
      ret-val)))

(defun wl-fldmgr-yank ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (bobp)
	(message "Can't insert in the out of desktop group")
      (let ((inhibit-read-only t)
	    (top (car wl-fldmgr-cut-entity-list))
	    tmp indent path count new
	    access new-list diffs)
	(if (not top)
	    (message "No cut buffer")
	  (setq tmp (wl-fldmgr-get-path-from-buffer t))
	  (setq path (car tmp))
	  (setq indent (wl-fldmgr-make-indent (nth 1 tmp)))
	  (if (numberp top)
	      (setq count (pop wl-fldmgr-cut-entity-list))
	    (setq count 1))
	  (if (catch 'err
		(let ((count count)
		      (cut-list wl-fldmgr-cut-entity-list))
		  ;; check insert entity
		  (while (> count 0)
		    (setq new (car cut-list))
		    (wl-push new new-list)
		    (when (consp new);; group
		      (cond
		       (access
			(message "Can't insert group in access")
			(throw 'err t))
		       ((wl-string-assoc (car new) wl-folder-group-alist)
			(message "%s: group already exists" (car new))
			(throw 'err t))))
		    (setq cut-list (cdr cut-list))
		    (setq count (1- count))))
		(if (not (setq diffs
			       (wl-add-entity
				path new-list wl-folder-entity (nth 3 tmp) t)))
		    (throw 'err t))
		(while (> count 0)
		  (setq new (pop wl-fldmgr-cut-entity-list))
		  (save-excursion
		    (wl-folder-insert-entity indent new)
		    (setq wl-fldmgr-modified t))
		  (setq count (1- count)))
		(wl-fldmgr-update-group path diffs)
		(set-buffer-modified-p nil))
	      ;; error
	      (wl-push count wl-fldmgr-cut-entity-list)))))))

(defvar wl-fldmgr-add-completion-hashtb (make-vector 7 0))

(defun wl-fldmgr-add-completion-all-completions (string)
  (let ((table
	 (catch 'found
	   (mapatoms
	    (lambda (atom)
	      (if (string-match (symbol-name atom) string)
		  (throw 'found (symbol-value atom))))
	    wl-fldmgr-add-completion-hashtb)))
	(pattern
	 (if (string-match "\\.$"
			   (elmo-folder-prefix-internal
			    (wl-folder-get-elmo-folder string)))
	     (substring string 0 (match-beginning 0))
	   (concat string nil))))
    (or table
	(setq table (elmo-folder-list-subfolders
		     (wl-folder-get-elmo-folder pattern)))
	(and table
	     (or (/= (length table) 1)
		 (elmo-folder-exists-p (wl-folder-get-elmo-folder
					(car table)))))
	(setq pattern
	      (if (string-match "\\.[^\\.]+$" string)
		  (substring string 0 (match-beginning 0))
		(char-to-string (aref string 0)))
	      table (elmo-folder-list-subfolders
		     (wl-folder-get-elmo-folder pattern))))
    (setq pattern (concat "^" (regexp-quote pattern)))
    (unless (intern-soft pattern wl-fldmgr-add-completion-hashtb)
      (set (intern pattern wl-fldmgr-add-completion-hashtb) table))
    table))

(defun wl-fldmgr-add-completion-subr (string predicate flag)
  (let ((table
	 (if (string= string "")
	     (mapcar (lambda (spec)
		       (list (char-to-string (car spec))))
		     elmo-folder-type-alist)
	   (when (assq (aref string 0) elmo-folder-type-alist)
	     (delq nil (mapcar
			(function list)
			(condition-case nil
			    (wl-fldmgr-add-completion-all-completions string)
			  (error nil))))))))
    (cond
     ((null flag)
      (try-completion string table predicate))
     ((eq flag 'lambda)
      (eq t (try-completion string table predicate)))
     (t
      (all-completions string table predicate)))))

(defun wl-fldmgr-add (&optional name)
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((ret-val nil)
	  (inhibit-read-only t)
	  (wl-folder-complete-folder-candidate
	   (if wl-fldmgr-add-complete-with-current-folder-list
	       (function wl-fldmgr-add-completion-subr)))
	  tmp indent path diffs)
      (if (bobp)
	  (message "Can't insert in the out of desktop group")
	(setq tmp (wl-fldmgr-get-path-from-buffer t))
	(setq path (car tmp))
	(setq indent (wl-fldmgr-make-indent (nth 1 tmp)))
	(or name
	    (setq name (wl-fldmgr-read-string
			(wl-summary-read-folder wl-default-folder "to add"))))
	;; maybe add elmo-plugged-alist.
	(elmo-folder-set-plugged (wl-folder-get-elmo-folder
				  (if (listp name) (car name) name))
				 wl-plugged t)
	(when (setq diffs
		    (wl-add-entity
		     path (list name) wl-folder-entity (nth 3 tmp) t))
	  (wl-folder-insert-entity indent name)
	  (wl-fldmgr-update-group path diffs)
	  (setq wl-fldmgr-modified t)
	  (set-buffer-modified-p nil)
	  (setq ret-val t)))
      ret-val)))

(defun wl-fldmgr-delete ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (wl-folder-buffer-group-p)
	(error "Can't delete group folder"))
    (let* ((inhibit-read-only t)
	   (tmp (wl-fldmgr-get-path-from-buffer))
	   (entity (elmo-string (nth 4 tmp)))
	   (folder (wl-folder-get-elmo-folder entity)))
      (when (elmo-folder-delete folder)
	(wl-folder-clear-entity-info entity)
	(wl-fldmgr-cut tmp nil t)
	(wl-fldmgr-save-access-list)))))

(defun wl-fldmgr-rename ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (bobp)
	(message "Can't rename desktop group")
      (cond
       ((and (wl-folder-buffer-group-p)
	     (looking-at wl-folder-group-regexp)) ;; group
	(let* ((indent (wl-match-buffer 1))
	       (old-group (wl-folder-get-entity-from-buffer))
	       (group-entity (wl-folder-search-group-entity-by-name
			      old-group wl-folder-entity))
	       group)
	  (if (eq (nth 1 group-entity) 'access)
	      (message "%s: can't rename access group folder" old-group)
	    (setq group (wl-fldmgr-read-string
			 (read-from-minibuffer "Rename: " old-group)))
	    (if (string-match "/$" group)
		(message "Remove tail slash.")
	      (cond
	       ((or (string= group "")
		    (string= old-group group))
		nil)
	       (t
		(if (wl-string-assoc group wl-folder-group-alist)
		    (message "%s: group already exists" group)
		  (let ((inhibit-read-only t)
			(id (wl-fldmgr-get-entity-id
			     (car group-entity))))
		    (wl-fldmgr-assign-id group id)
		    (setcar group-entity group)
		    (setcar (wl-string-assoc old-group wl-folder-group-alist)
			    group)
;;;		    (setcdr (assq id wl-folder-entity-id-name-alist) group)
		    (wl-folder-set-id-name id group)
		    (wl-fldmgr-delete-line)
		    (wl-folder-insert-entity
		     indent
		     group-entity t)
		    (setq wl-fldmgr-modified t)
		    (set-buffer-modified-p nil)))))))))
       (t ;; folder
	(let* ((tmp (wl-fldmgr-get-path-from-buffer))
	       (old-folder (nth 4 tmp))
	       new-folder)
	  (unless old-folder (error "No folder"))
	  (setq new-folder
		(wl-fldmgr-read-string
		 (wl-summary-read-folder old-folder "to rename" t t old-folder)))
	  (if (or (wl-folder-entity-exists-p new-folder)
		  (file-exists-p (elmo-folder-msgdb-path
				  (wl-folder-get-elmo-folder new-folder))))
	      (error "Already exists folder: %s" new-folder))
	  (if (and (eq (cdr (nth 2 tmp)) 'access)
		   (null wl-fldmgr-allow-rename-access-group)
		   (null (string-match
			  (format "^%s" (regexp-quote (car (nth 2 tmp))))
			  new-folder)))
	      (error "Can't rename access folder"))
	  (elmo-folder-rename (wl-folder-get-elmo-folder old-folder)
			      new-folder)
	  (wl-folder-set-entity-info
	   new-folder
	   (wl-folder-get-entity-info old-folder))
	  (wl-folder-clear-entity-info old-folder)
	  (setq wl-folder-info-alist-modified t)
	  (if (eq (cdr (nth 2 tmp)) 'access)

	      ;; force update access group
	      (progn
		(wl-folder-open-close)
		(wl-folder-jump-to-current-entity t)
		(message "%s is renamed to %s" old-folder new-folder)
		(sit-for 1))
	    ;; update folder list
	    (when (wl-fldmgr-cut tmp nil t)
	      (wl-fldmgr-add new-folder)))))))))

(defun wl-fldmgr-make-access-group ()
  (interactive)
  (wl-fldmgr-make-group nil t))

(defun wl-fldmgr-make-group (&optional group-name access)
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (bobp)
	(message "Can't insert in the out of desktop group")
      (let ((inhibit-read-only t)
	    (type 'group)
	    group tmp indent path new prev-id flist diffs)
	(setq tmp (wl-fldmgr-get-path-from-buffer t))
	(setq path (car tmp))
	(setq indent (wl-fldmgr-make-indent (nth 1 tmp)))
	(setq prev-id (nth 3 tmp))
	(if (eq (cdr (nth 2 tmp)) 'access)
	    (message "Can't insert access group")
	  (setq group (or group-name
			  (wl-fldmgr-read-string
			   (read-from-minibuffer
			    (if access "Access Type Group: " "Group: ")))))
	  ;; To check the folder name is correct.
	  (if access (elmo-make-folder group))
	  (when (or access (string-match "[\t ]*/$" group))
	    (setq group (if access group
			  (substring group 0 (match-beginning 0))))
	    (setq type 'access)
	    (setq flist (wl-create-access-folder-entity group)))
	  (if (string= group "")
	      nil
	    (if (wl-string-assoc group wl-folder-group-alist)
		(message "%s: group already exists" group)
	      (setq new (append (list group type) flist))
	      (when (setq diffs (wl-add-entity path
					       (list new)
					       wl-folder-entity
					       prev-id))
		(wl-folder-insert-entity indent new)
		(wl-fldmgr-update-group path diffs)
		(setq wl-fldmgr-modified t)
		(set-buffer-modified-p nil)))))))))

(defun wl-fldmgr-make-multi ()
  (interactive)
  (if (not wl-fldmgr-cut-entity-list)
      (message "No cut buffer")
    (let ((cut-entity wl-fldmgr-cut-entity-list)
	  (new-entity "")
	  (first t)
	  status)
      (setq status
	    (catch 'done
	      (while cut-entity
		(cond
		 ((numberp (car cut-entity))
		  nil)
		 ((consp (car cut-entity))
		  (message "Can't make multi included group folder")
		  (throw 'done nil))
		 (t
		  (let ((folder (wl-folder-get-elmo-folder
				 (car cut-entity)))
			multi-fld)
		    (if (eq (elmo-folder-type-internal folder) 'multi)
			(setq multi-fld
			      (substring (car cut-entity) 1)))
		    (setq new-entity
			  (format "%s%s%s"
				  (or multi-fld (car cut-entity))
				  (if first "" ",")
				  new-entity))
		    (setq first nil))))
		(setq cut-entity (cdr cut-entity)))
	      (throw 'done t)))
      (when status
	(setq new-entity (concat "*" new-entity))
	(wl-fldmgr-add new-entity)))))

(defun wl-fldmgr-make-filter ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((tmp (wl-fldmgr-get-path-from-buffer))
	  entity)
      (if (eq (cdr (nth 2 tmp)) 'access)
	  (message "Can't change access group")
	(if (wl-folder-buffer-group-p)
	    (setq entity
		  (concat
		   "*"
		   (mapconcat 'identity
			      (wl-folder-get-entity-list
			       (wl-folder-search-group-entity-by-name
				(nth 4 tmp)
				wl-folder-entity)) ",")))
	  (setq entity (nth 4 tmp)))
	(unless entity (error "No folder"))
	(wl-fldmgr-add (concat "/"
			       (wl-read-search-condition
				wl-fldmgr-make-filter-default)
			       "/" entity))))))

(defun wl-fldmgr-sort (&optional arg)
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    (let ((inhibit-read-only t)
	  entity flist indent opened)
      (when (and (wl-folder-buffer-group-p)
		 (looking-at wl-folder-group-regexp)
		 (prog1
		     (y-or-n-p (format "Sort subfolders of %s? "
				       (wl-folder-get-entity-from-buffer)))
		   (message nil)))
	(setq indent (wl-match-buffer 1))
	(setq opened (wl-match-buffer 2))
	(setq entity (wl-folder-search-group-entity-by-name
		      (wl-folder-get-entity-from-buffer)
		      wl-folder-entity))
	(message "Sorting...")
	(setq flist (sort (nth 2 entity) wl-fldmgr-sort-function))
	(when arg (setq flist (nreverse flist)))
	(setcar (cddr entity) flist)
	(wl-fldmgr-add-modified-access-list (car entity))
	(setq wl-fldmgr-modified t)
	(when (string= opened "-")
	  (let (beg end)
	    (setq beg (point))
	    (end-of-line)
	    (save-match-data
	      (setq end
		    (progn
		      (wl-folder-goto-bottom-of-current-folder indent)
		      (beginning-of-line)
		      (point))))
	    (delete-region beg end)
	    (wl-folder-insert-entity indent entity)))
	(message "Sorting...done")
	(set-buffer-modified-p nil)))))

(defun wl-fldmgr-sort-standard (x y)
  (cond ((and (consp x) (not (consp y)))
	 wl-fldmgr-sort-group-first)
	((and (not (consp x)) (consp y))
	 (not wl-fldmgr-sort-group-first))
	((and (consp x) (consp y))
	 (string-lessp (car x) (car y)))
	(t
	 (string-lessp x y))))

(defun wl-fldmgr-subscribe-region ()
  (interactive)
  (wl-fldmgr-unsubscribe-region -1))

(defun wl-fldmgr-unsubscribe-region (&optional arg)
  (interactive "P")
  (let* ((p1 (region-beginning))
	 (p2 (region-end))
	 (r1 (progn
	       (goto-char p1)
	       (beginning-of-line)
	       (point)))
	 (r2 (progn
	       (goto-char p2)
	       (beginning-of-line)
	       (point)))
	 (from (min r1 r2))
	 (to (max r1 r2))
	 (count 0))
    (goto-char from)
    (while (< (point) to)
      (setq count (1+ count))
      (forward-line))
    (goto-char from)
    (message "Unsubscribe region...")
    (while (and (> count 0)
		(wl-fldmgr-unsubscribe (or arg 1) t))
      (setq count (1- count)))
    (message "Unsubscribe region...done")))

(defun wl-fldmgr-subscribe ()
  (interactive)
  (wl-fldmgr-unsubscribe -1))

(defun wl-fldmgr-unsubscribe (&optional arg force)
  (interactive "P")
  (let ((type (and arg (prefix-numeric-value arg)))
	execed is-group)
    (save-excursion
      (beginning-of-line)
      (let ((inhibit-read-only t)
	    folder
	    tmp indent beg)
	(cond
	 ((looking-at (format "^[ ]*%s\\[[+-]\\]\\(.*\\)" wl-folder-unsubscribe-mark))
	  (if (and type (> type 0))
	      nil
	    (setq folder (list (wl-match-buffer 1) 'access nil))
	    (if (wl-string-assoc (car folder) wl-folder-group-alist)
		(message "%s: group already exists" (car folder))
	      (wl-fldmgr-delete-line)
	      (when (wl-fldmgr-add folder)
		(wl-folder-maybe-load-folder-list folder)
;;;		(wl-folder-search-group-entity-by-name (car folder)
;;;						       wl-folder-entity)
		(setq execed t)))))
	 ((looking-at (format "^[ ]*%s\\(.*\\)" wl-folder-unsubscribe-mark))
	  (if (and type (> type 0))
	      nil
	    (setq folder (wl-match-buffer 1))
	    (wl-fldmgr-delete-line)
	    (when (wl-fldmgr-add folder)
	      (setq execed t))))
	 (t
	  (if (and type (< type 0))
	      nil
	    (setq is-group (wl-folder-buffer-group-p))
	    (setq tmp (wl-fldmgr-get-path-from-buffer))
	    (setq indent (wl-fldmgr-make-indent (nth 1 tmp)))
	    (if (eq (cdr (nth 2 tmp)) 'access)
		(when (wl-fldmgr-cut tmp)
		  ;; don't leave cut-list
		  (setq wl-fldmgr-cut-entity-list (cdr wl-fldmgr-cut-entity-list))
		  (setq beg (point))
		  (insert indent wl-folder-unsubscribe-mark
			  (if is-group
			      (concat "[+]" (nth 4 tmp))
			    (nth 4 tmp))
			  "\n")
		  (save-excursion (forward-line -1)
				  (wl-highlight-folder-current-line))
		  (remove-text-properties beg (point) '(wl-folder-entity-id))
		  (setq execed t))
	      (message "not an access group folder")))))
	(set-buffer-modified-p nil)))
    (if (or force execed)
	(progn
	  (forward-line)
	  t))))

(defun wl-fldmgr-access-display-normal (&optional arg)
  (interactive "P")
  (wl-fldmgr-access-display-all (not arg)))

(defun wl-fldmgr-access-display-all (&optional arg)
  (interactive "P")
  (let ((id (save-excursion
	      (wl-folder-prev-entity-skip-invalid t)
	      (wl-fldmgr-get-entity-id))))
    (save-excursion
    (beginning-of-line)
    (let ((inhibit-read-only t)
	  entity indent opened
	  unsubscribes beg)
      (when (not
	     (and (wl-folder-buffer-group-p)
		  (looking-at wl-folder-group-regexp)))
	(wl-folder-goto-top-of-current-folder)
	(looking-at wl-folder-group-regexp))
      (setq indent (wl-match-buffer 1))
      (setq opened (wl-match-buffer 2))
      (setq entity (wl-folder-search-group-entity-by-name
		    (wl-folder-get-entity-from-buffer)
		    wl-folder-entity))
      (when (eq (nth 1 entity) 'access)
	(save-excursion
	  (if (string= opened "-")
	      (let (beg end)
		(setq beg (point))
		(end-of-line)
		(save-match-data
		  (setq end
			(progn
			  (wl-folder-goto-bottom-of-current-folder indent)
			  (beginning-of-line)
			  (point))))
		(delete-region beg end))
	    (wl-fldmgr-delete-line)
	    (setcdr (assoc (car entity) wl-folder-group-alist) t));; set open
	  (wl-folder-insert-entity indent entity))
	(when (not arg)
	  (setq unsubscribes (nth 3 entity))
	  (forward-line)
	  (while unsubscribes
	    (setq beg (point))
	    (insert indent "  " wl-folder-unsubscribe-mark
		    (if (consp (car unsubscribes))
			(concat "[+]" (caar unsubscribes))
		      (car unsubscribes))
		    "\n")
	    (remove-text-properties beg (point) '(wl-folder-entity-id))
	    (save-excursion (forward-line -1)
			    (wl-highlight-folder-current-line))
	    (setq unsubscribes (cdr unsubscribes))))
	(set-buffer-modified-p nil))))
    (wl-folder-move-path id)))

(defun wl-fldmgr-set-petname ()
  (interactive)
  (save-excursion
    (beginning-of-line)
      (let* ((is-group (wl-folder-buffer-group-p))
	     (name (wl-folder-get-entity-from-buffer))
	     (searchname (wl-folder-get-petname name))
	     (pentry (wl-string-assoc name wl-folder-petname-alist))
	     (old-petname (or (cdr pentry) ""))
	     (change)
	     petname)
	(unless name (error "No folder"))
	(if (and is-group
		 (not (eq (nth 1 (wl-folder-search-group-entity-by-name
				  name wl-folder-entity))
			  'access)))
	    (message "Can't set petname. please rename.")
	(setq petname (wl-fldmgr-read-string
		       (read-from-minibuffer "Petname: " old-petname)))
	(cond
	 ((string= petname "")
	  (when pentry
	    (setq wl-folder-petname-alist
		  (delete pentry wl-folder-petname-alist))
	    (setq change t)))
	 (t
	  (if (string= petname old-petname)
	      nil
	    (if (or (rassoc petname wl-folder-petname-alist)
		    (and is-group
			 (wl-string-assoc petname wl-folder-group-alist)))
		(message "%s: already exists" petname)
	      (wl-folder-append-petname name petname)
	      (setq change t)))))
	(when change
	  (let ((inhibit-read-only t)
		indent)
	    (goto-char (point-min))
	    (if is-group
		(progn
		  (if (string= old-petname "")
		      (setq old-petname name))
		  (while (wl-folder-buffer-search-group old-petname)
		    (beginning-of-line)
		    (and (looking-at "^\\([ ]*\\)")
			 (setq indent (wl-match-buffer 1)))
		    (wl-fldmgr-delete-line)
		    (wl-folder-insert-entity
		     indent
		     (wl-folder-search-group-entity-by-name
		      name wl-folder-entity)
		     t)))
	      (while (wl-folder-buffer-search-entity name searchname)
		(save-excursion
		  (beginning-of-line)
		  (and (looking-at "^\\([ ]*\\)")
		       (setq indent (wl-match-buffer 1)))
		  (wl-fldmgr-delete-line))
		(wl-folder-insert-entity indent name)))
	    (setq wl-fldmgr-modified t)
	    (set-buffer-modified-p nil)))))))

;;; Function for save folders
;;

(defun wl-fldmgr-insert-folders-buffer (indent entities &optional pet-entities)
  (let ((flist entities)
	name petname)
    (while flist
      (setq name (car flist))
      (cond ((stringp name)
	     (if (setq petname (cdr (wl-string-assoc name wl-folder-petname-alist)))
		 (wl-append pet-entities (list name)))
	     (insert indent name
		     (if petname
			 (concat "\t\"" petname "\"")
		       "")
		     "\n"))
	    ((consp name)
	     (let ((group (car name))
		   (type (nth 1 name)))
	       (cond ((eq type 'group)
		      (insert indent group "{\n")
		      (setq pet-entities
			    (wl-fldmgr-insert-folders-buffer
			     (concat indent wl-fldmgr-folders-indent)
			     (nth 2 name) pet-entities))
		      (insert indent "}\n"))
		     ((eq type 'access)
		      (insert indent group "/\n"))))))
      (setq flist (cdr flist))))
  pet-entities)

(defun wl-fldmgr-insert-petname-buffer (pet-entities)
  (let ((alist wl-folder-petname-alist))
    (while alist
      (if (wl-string-member (caar alist) pet-entities)
	  nil
	(insert "=\t" (caar alist) "\t\"" (cdar alist) "\"\n"))
      (setq alist (cdr alist)))))

(defun wl-fldmgr-delete-disused-petname ()
  (let ((alist wl-folder-petname-alist))
    (while alist
      (unless (wl-folder-search-entity-by-name (caar alist) wl-folder-entity)
	(setq wl-folder-petname-alist
	      (delete (car alist) wl-folder-petname-alist)))
      (setq alist (cdr alist)))))

(defun wl-fldmgr-save-folders ()
  (interactive)
  (let ((tmp-buf (get-buffer-create " *wl-fldmgr-tmp*"))
	save-petname-entities)
    (message "Saving folders...")
    (set-buffer tmp-buf)
    (erase-buffer)
    (insert wl-fldmgr-folders-header)
    (wl-fldmgr-delete-disused-petname)
    (setq save-petname-entities
	  (wl-fldmgr-insert-folders-buffer "" (nth 2 wl-folder-entity)))
    (insert "\n# petname definition (access group, folder in access group)\n")
    (wl-fldmgr-insert-petname-buffer save-petname-entities)
    (insert "\n# end of file.\n")
    (if (and wl-fldmgr-make-backup
	     (file-exists-p wl-folders-file))
	(rename-file wl-folders-file (concat wl-folders-file ".bak") t))
    (let ((output-coding-system (mime-charset-to-coding-system
				 wl-mime-charset)))
      (write-region
       (point-min)
       (point-max)
       wl-folders-file
       nil
       'no-msg)
      (set-file-modes wl-folders-file (+ (* 64 6) (* 8 0) 0))) ; chmod 0600
    (kill-buffer tmp-buf)
    (wl-fldmgr-save-access-list)
    (setq wl-fldmgr-modified nil)
    (message "Saving folders...done")))

(defun wl-fldmgr-save-access-list ()
  (let ((access-list wl-fldmgr-modified-access-list)
	entity)
    (while access-list
      (setq entity (wl-folder-search-group-entity-by-name
		    (car access-list) wl-folder-entity))
      (elmo-msgdb-flist-save
       (car access-list)
       (list
	(wl-folder-make-save-access-list (nth 2 entity))
	(wl-folder-make-save-access-list (nth 3 entity))))
      (setq access-list (cdr access-list)))
    (setq wl-fldmgr-modified-access-list nil)))

(require 'product)
(product-provide (provide 'wl-fldmgr) (require 'wl-version))

;;; wl-fldmgr.el ends here
