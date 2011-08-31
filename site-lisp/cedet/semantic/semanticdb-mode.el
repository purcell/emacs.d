;;; semanticdb-mode.el --- Semanticdb Minor Mode

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semanticdb-mode.el,v 1.3 2008/12/10 22:10:42 zappo Exp $

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
;;
;; Major mode for managing Semantic Databases automatically.

(require 'semanticdb)
;;; Code:
;;;###autoload
(defvar semanticdb-current-database nil
  "For a given buffer, this is the currently active database.")
(make-variable-buffer-local 'semanticdb-current-database)

(defvar semanticdb-current-table nil
  "For a given buffer, this is the currently active database table.")
(make-variable-buffer-local 'semanticdb-current-table)

;;;###autoload
(defcustom semanticdb-global-mode nil
  "*If non-nil enable the use of `semanticdb-minor-mode'."
  :group 'semantic
  :type 'boolean
  :require 'semanticdb
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-semanticdb-minor-mode (if val 1 -1))
         (custom-set-default sym val)))

(defcustom semanticdb-mode-hooks nil
  "*Hooks run whenever `global-semanticdb-minor-mode' is run.
Use `semanticdb-minor-mode-p' to determine if the mode has been turned
on or off."
  :group 'semanticdb
  :type 'hook)

;;; Start/Stop database use
;;
(defvar semanticdb-hooks
  '((semanticdb-semantic-init-hook-fcn semantic-init-db-hooks)
    (semanticdb-synchronize-table semantic-after-toplevel-cache-change-hook)
    (semanticdb-partial-synchronize-table semantic-after-partial-cache-change-hook)
    (semanticdb-revert-hook before-revert-hook)
    (semanticdb-kill-hook kill-buffer-hook)
    (semanticdb-kill-hook change-major-mode-hook) ;; Not really a kill, but we need the same effect.
    (semanticdb-kill-emacs-hook kill-emacs-hook)
    (semanticdb-save-all-db-idle auto-save-hook)
    )
  "List of hooks and values to add/remove when configuring semanticdb.")

;;; SEMANTICDB-MODE
;;
;;;###autoload
(defun semanticdb-minor-mode-p ()
  "Return non-nil if `semanticdb-minor-mode' is active."
  (member (car (car semanticdb-hooks))
	  (symbol-value (car (cdr (car semanticdb-hooks))))))

;;;###autoload
(defun global-semanticdb-minor-mode (&optional arg)
  "Toggle the use of `semanticdb-minor-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (if (not arg)
      (if (semanticdb-minor-mode-p)
	  (setq arg -1)
	(setq arg 1)))
  (let ((fn 'add-hook)
	(h semanticdb-hooks)
	(changed nil))
    (if (< arg 0)
	(setq changed semanticdb-global-mode
	      semanticdb-global-mode nil
              fn 'remove-hook)
      (setq changed (not semanticdb-global-mode)
	    semanticdb-global-mode t))
    ;(message "ARG = %d" arg)
    (when changed
      (while h
	(funcall fn (car (cdr (car h))) (car (car h)))
	(setq h (cdr h)))
      ;; Call a hook
      (run-hooks 'semanticdb-mode-hooks))
    ))

(defun semanticdb-toggle-global-mode ()
  "Toggle use of the Semantic Database feature.
Update the environment of Semantic enabled buffers accordingly."
  (interactive)
  (if (semanticdb-minor-mode-p)
      ;; Save databases before disabling semanticdb.
      (semanticdb-save-all-db))
  ;; Toggle semanticdb minor mode.
  (global-semanticdb-minor-mode))

;;; Hook Functions:
;;
;; Functions used in hooks to keep SemanticDB operating.
;;
(defun semanticdb-semantic-init-hook-fcn ()
  "Function saved in `semantic-init-db-hooks'.
Sets up the semanticdb environment."
  ;; Only initialize semanticdb if we have a file name.
  ;; There is no reason to cache a tag table if there is no
  ;; way to load it back in later.
  (when (buffer-file-name)
    (let* ((ans (semanticdb-create-table-for-file (buffer-file-name)))
	   (cdb (car ans))
	   (ctbl (cdr ans))
	   )
      ;; Get the current DB for this directory
      (setq semanticdb-current-database cdb)
      ;; We set the major mode because we know what it is.
      (oset ctbl major-mode major-mode)
      ;; Local state
      (setq semanticdb-current-table ctbl)
      ;; Try to swap in saved tags
      (if (or (not (slot-boundp ctbl 'tags)) (not (oref ctbl tags))
	      (/= (or (oref ctbl pointmax) 0) (point-max))
	      )
	  (semantic-clear-toplevel-cache)
	;; Unmatched syntax
	(condition-case nil
	    (semantic-set-unmatched-syntax-cache
	     (oref ctbl unmatched-syntax))
	  (unbound-slot
	   ;; Old version of the semanticdb table can miss the unmatched
	   ;; syntax slot.  If so, just clear the unmatched syntax cache.
	   (semantic-clear-unmatched-syntax-cache)
	   ;; Make sure it has a value.
	   (oset ctbl unmatched-syntax nil)
	   ))
	;; Keep lexical tables up to date.  Don't load
	;; semantic-spp if it isn't needed.
	(let ((lt (oref ctbl lexical-table)))
	  (when lt
	    (require 'semantic-lex-spp)
	    (semantic-lex-spp-set-dynamic-table lt)))
	;; Set the main tag cache.
	;; This must happen after setting up buffer local variables
	;; since this will turn around and re-save those variables.
	(semantic--set-buffer-cache (oref ctbl tags))
	;; Don't need it to be dirty.  Set dirty due to hooks from above.
	(oset ctbl dirty nil) ;; Special case here.
	(oset ctbl buffer (current-buffer))
	;; Bind into the buffer.
	(semantic--tag-link-cache-to-buffer)
	)
      )))

(defun semanticdb-revert-hook ()
  "Hook run before a revert buffer.
We can't track incremental changes due to a revert, so just clear the cache.
This will prevent the next batch of hooks from wasting time parsing things
that don't need to be parsed."
  (if (and (semantic-active-p)
	   semantic--buffer-cache
	   semanticdb-current-table)
      (semantic-clear-toplevel-cache)))

(defun semanticdb-kill-hook ()
  "Function run when a buffer is killed.
If there is a semantic cache, slurp out the overlays, and store
it in our database.  If that buffer has no cache, ignore it, we'll
handle it later if need be."
  (when (and (semantic-active-p)
	     semantic--buffer-cache
	     semanticdb-current-table)
      
    ;; Try to get a fast update.
    (semantic-fetch-tags-fast)

    ;; If the buffer is in a bad state, don't save anything...
    (if (semantic-parse-tree-needs-rebuild-p)
	;; If this is the case, don't save anything.
	(progn
	  (semantic-clear-toplevel-cache)
	  (oset semanticdb-current-table pointmax 0)
	  (oset semanticdb-current-table fsize 0)
	  (oset semanticdb-current-table lastmodtime nil)
	  )
      ;; We have a clean buffer, save it off.
      (condition-case nil
	  (progn
	    (semantic--tag-unlink-cache-from-buffer)
	    ;; Set pointmax only if we had some success in the unlink.
	    (oset semanticdb-current-table pointmax (point-max))
	    (let ((fattr (file-attributes
			  (semanticdb-full-filename
			   semanticdb-current-table))))
	      (oset semanticdb-current-table fsize (nth 7 fattr))
	      (oset semanticdb-current-table lastmodtime (nth 5 fattr))
	      (oset semanticdb-current-table buffer nil)
	      ))
	;; If this messes up, just clear the system
	(error
	 (semantic-clear-toplevel-cache)
	 (message "semanticdb: Failed to deoverlay tag cache.")))
      )
    ))

(defun semanticdb-kill-emacs-hook ()
  "Function called when Emacs is killed.
Save all the databases."
  (semanticdb-save-all-db))

;;; SYNCHRONIZATION HOOKS
;;
(defun semanticdb-synchronize-table (new-table)
  "Function run after parsing.
Argument NEW-TABLE is the new table of tags."
  (when semanticdb-current-table
    (semanticdb-synchronize semanticdb-current-table new-table)))

(defun semanticdb-partial-synchronize-table (new-table)
  "Function run after parsing.
Argument NEW-TABLE is the new table of tags."
  (when semanticdb-current-table
    (semanticdb-partial-synchronize semanticdb-current-table new-table)))


(provide 'semanticdb-mode)
;;; semanticdb-mode.el ends here
