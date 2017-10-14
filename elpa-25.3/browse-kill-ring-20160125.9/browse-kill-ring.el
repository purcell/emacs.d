;;; browse-kill-ring.el --- interactively insert items from kill-ring -*- coding: utf-8 -*-

;; Copyright (C) 2001, 2002 Colin Walters <walters@verbum.org>

;; Author: Colin Walters <walters@verbum.org>
;; Maintainer: browse-kill-ring <browse-kill-ring@tonotdo.com>
;; Created: 7 Apr 2001
;; Version: 2.0.0
;; Package-Version: 20160125.9
;; URL: https://github.com/browse-kill-ring/browse-kill-ring
;; Keywords: convenience

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Ever feel that 'C-y M-y M-y M-y ...' is not a great way of trying
;; to find that piece of text you know you killed a while back?  Then
;; browse-kill-ring.el is for you.

;; This package is simple to install; add (require 'browse-kill-ring)
;; to your ~/.emacs file, after placing this file somewhere in your
;; `load-path'.  If you want to use 'M-y' to invoke
;; `browse-kill-ring', also add (browse-kill-ring-default-keybindings)
;; to your ~/.emacs file.  Alternatively, you can bind it to another
;; key such as "C-c k", with:
;; (global-set-key (kbd "C-c k") 'browse-kill-ring)

;; Note that the command keeps track of the last window displayed to
;; handle insertion of chosen text; this might have unexpected
;; consequences if you do 'M-x browse-kill-ring', then switch your
;; window configuration, and try to use the same *Kill Ring* buffer
;; again.

;;; Code:

(require 'cl-lib)
(require 'cl)
(require 'derived)

(defgroup browse-kill-ring nil
  "A package for browsing and inserting the items in `kill-ring'."
  :link '(url-link "https://github.com/browse-kill-ring/browse-kill-ring")
  :group 'convenience)

(defvar browse-kill-ring-display-styles
  '((separated . browse-kill-ring-insert-as-separated)
    (one-line . browse-kill-ring-insert-as-one-line)))

(defcustom browse-kill-ring-display-style 'separated
  "How to display the kill ring items.

If `one-line', then replace newlines with \"\\n\" for display.

If `separated', then display `browse-kill-ring-separator' between
entries."
  :type '(choice (const :tag "One line" one-line)
                 (const :tag "Separated" separated))
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-quit-action 'save-and-restore
  "What action to take when `browse-kill-ring-quit' is called.

If `bury-buffer', then simply bury the *Kill Ring* buffer, but keep
the window.

If `bury-and-delete-window', then bury the buffer, and (if there is
more than one window) delete the window.

If `save-and-restore', then save the window configuration when
`browse-kill-ring' is called, and restore it at quit.  This is
the default.

If `kill-and-delete-window', then kill the *Kill Ring* buffer, and
delete the window on close.

Otherwise, it should be a function to call."
  :type '(choice (const :tag "Bury buffer" :value bury-buffer)
                 (const :tag "Delete window" :value delete-window)
                 (const :tag "Save and restore" :value save-and-restore)
                 (const :tag "Bury buffer and delete window" :value bury-and-delete-window)
                 (const :tag "Kill buffer and delete window" :value kill-and-delete-window)
                 function)
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-resize-window nil
  "Whether to resize the `browse-kill-ring' window to fit its contents.
Value is either t, meaning yes, or a cons pair of integers,
 (MAXIMUM . MINIMUM) for the size of the window.  MAXIMUM defaults to
the window size chosen by `pop-to-buffer'; MINIMUM defaults to
`window-min-height'."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 (cons (integer :tag "Maximum") (integer :tag "Minimum")))
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-separator "-------"
  "The string separating entries in the `separated' style.
See `browse-kill-ring-display-style'."
  :type 'string
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-recenter nil
  "If non-nil, then always keep the current entry at the top of the window."
  :type 'boolean
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-highlight-current-entry nil
  "If non-nil, highlight the currently selected `kill-ring' entry."
  :type 'boolean
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-highlight-inserted-item
  browse-kill-ring-highlight-current-entry
  "If non-nil, then temporarily highlight the inserted `kill-ring' entry.
The value selected controls how the inserted item is highlighted,
possible values are `solid' (highlight the inserted text for a
fixed period of time), or `pulse' (use the `pulse' library, a
part of `cedet', to fade out the highlighting gradually).
Setting this variable to the value `t' will select the default
highlighting style, which is currently `pulse'.

The variable `browse-kill-ring-inserted-item-face' contains the
face used for highlighting."
  :type '(choice (const nil) (const t) (const solid) (const pulse))
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-separator-face 'bold
  "The face in which to highlight the `browse-kill-ring-separator'."
  :type 'face
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-current-entry-face 'highlight
  "The face in which to highlight the browse kill current entry."
  :type 'face
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-inserted-item-face 'highlight
  "The face in which to highlight the inserted item."
  :type 'face
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-maximum-display-length nil
  "Whether or not to limit the length of displayed items.

If this variable is an integer, the display of `kill-ring' will be
limited to that many characters.
Setting this variable to nil means no limit."
  :type '(choice (const :tag "None" nil)
                 integer)
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-display-duplicates t
  "If non-nil, then display duplicate items in `kill-ring'."
  :type 'boolean
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-display-leftmost-duplicate t
  "When `browse-kill-ring-display-duplicates' nil,
if non-nil, then display leftmost(last) duplicate items in `kill-ring'."
  :type 'boolean
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-depropertize nil
  "If non-nil, remove text properties from `kill-ring' items.
This only changes the items for display and insertion from
`browse-kill-ring'; if you call `yank' directly, the items will be
inserted with properties."
  :type 'boolean
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-hook nil
  "A list of functions to call after `browse-kill-ring'."
  :type 'hook
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-replace-yank t
  "If non-nil, browse-kill-ring will replace just-yanked items
when it inserts its own. That is, if you call `yank', and then
`browse-kill-ring', and then insert something via
`browse-kill-ring', the yanked text that you originally inserted
will be deleted. This makes browse-kill-ring behave more like
`yank-pop'.

This doesn't change the behavior of `yank-pop' or
`browse-kill-ring-default-keybindings'. Instead, for this to take
effect, you will have to bind a key to `browse-kill-ring'
directly."
  :type 'boolean
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-show-preview t
  "If non-nil, browse-kill-ring will show a preview of what the
buffer would look like if the item under point were inserted.

If you find the preview distracting, or something about your
setup leaves the preview in place after you're done with it, you
can disable it by setting this to nil."
  :type 'boolean
  :group 'browse-kill-ring)

(defvar browse-kill-ring-original-window-config nil
  "The window configuration to restore for `browse-kill-ring-quit'.")
(make-variable-buffer-local 'browse-kill-ring-original-window-config)

(defvar browse-kill-ring-original-window nil
  "The window in which chosen kill ring data will be inserted.
It is probably not a good idea to set this variable directly; simply
call `browse-kill-ring' again.")

(defvar browse-kill-ring-original-buffer nil
  "The buffer in which chosen kill ring data will be inserted.
It is probably not a good idea to set this variable directly; simply
call `browse-kill-ring' again.")

(defvar browse-kill-ring-preview-overlay nil
  "The overlay used to preview what would happen if the user
  inserted the given text.")

(defvar browse-kill-ring-this-buffer-replace-yanked-text nil
  "Whether or not to replace yanked text before an insert.")

(defvar browse-kill-ring-previous-overlay nil
  "Previous overlay within *Kill Ring* buffer.")

(defun browse-kill-ring-mouse-insert (e)
  "Insert the chosen text, and close the *Kill Ring* buffer afterwards."
  (interactive "e")
  (let* ((data (save-excursion
                 (mouse-set-point e)
                 (cons (current-buffer) (point))))
         (buf (car data))
         (pt (cdr data)))
    (browse-kill-ring-do-insert buf pt t)))

(if (fboundp 'fit-window-to-buffer)
    (defalias 'browse-kill-ring-fit-window 'fit-window-to-buffer)
  (defun browse-kill-ring-fit-window (window max-height min-height)
    (setq min-height (or min-height window-min-height))
    (setq max-height (or max-height (- (frame-height) (window-height) 1)))
    (let* ((window-min-height min-height)
           (windows (count-windows))
           (config (current-window-configuration)))
      (enlarge-window (- max-height (window-height)))
      (when (> windows (count-windows))
        (set-window-configuration config))
      (if (/= (point-min) (point-max))
          (shrink-window-if-larger-than-buffer window)
        (shrink-window (- (window-height) window-min-height))))))

(defun browse-kill-ring-resize-window ()
  (when browse-kill-ring-resize-window
    (apply #'browse-kill-ring-fit-window (selected-window)
           (if (consp browse-kill-ring-resize-window)
               (list (car browse-kill-ring-resize-window)
                     (or (cdr browse-kill-ring-resize-window)
                         window-min-height))
             (list nil window-min-height)))))

(defun browse-kill-ring-undo-other-window ()
  "Undo the most recent change in the other window's buffer.
You most likely want to use this command for undoing an insertion of
yanked text from the *Kill Ring* buffer."
  (interactive)
  (with-current-buffer (window-buffer browse-kill-ring-original-window)
    (undo)))

(defun browse-kill-ring-insert (&optional quit)
  "Insert the kill ring item at point into the last selected buffer.
If optional argument QUIT is non-nil, close the *Kill Ring* buffer as
well."
  (interactive "P")
  (browse-kill-ring-do-insert (current-buffer)
                              (point)
                              quit))

(defun browse-kill-ring-insert-new (insert-action post-action &optional quit)
  "Insert the kill ring item at point into the last selected buffer.
`insert-action' can be 'insert 'append 'prepend.
`post-action' can be nil 'move 'delete.
If optional argument QUIT is non-nil, close the *Kill Ring* buffer as
well."
  (interactive "P")
  (let* ((buf (current-buffer))
        (pt (point))
        (str (browse-kill-ring-current-string buf pt)))
    (case insert-action
      ('insert (browse-kill-ring-do-insert buf pt nil))
      ('append (browse-kill-ring-do-append-insert buf pt nil))
      ('prepend (browse-kill-ring-do-prepend-insert buf pt nil))
      (t (error "Unknown insert-action: %s" insert-action)))
    (case post-action
      ('move
        (browse-kill-ring-delete)
        (kill-new str))
      ('delete (browse-kill-ring-delete))
      (t (error "Unknown post-action: %s" post-action)))
    (if quit
      (browse-kill-ring-quit)
      (browse-kill-ring-update))))

(defun browse-kill-ring-insert-and-delete (&optional quit)
  "Insert the kill ring item at point, and remove it from the kill ring.
If optional argument QUIT is non-nil, close the *Kill Ring* buffer as
well."
  (interactive "P")
  (browse-kill-ring-do-insert (current-buffer)
                              (point)
                              quit)
  (browse-kill-ring-delete))

(defun browse-kill-ring-insert-and-quit ()
  "Like `browse-kill-ring-insert', but close the *Kill Ring* buffer afterwards."
  (interactive)
  (browse-kill-ring-insert t))

(defun browse-kill-ring-insert-and-move (&optional quit)
  "Like `browse-kill-ring-insert', but move the entry to the front."
  (interactive "P")
  (let ((buf (current-buffer))
        (pt (point)))
    (browse-kill-ring-do-insert buf pt quit)
    (let ((str (browse-kill-ring-current-string buf pt)))
      (browse-kill-ring-delete)
      (kill-new str)))
  (unless quit
    (browse-kill-ring-update)))

(defun browse-kill-ring-insert-move-and-quit ()
  "Like `browse-kill-ring-insert-and-move', but close the *Kill Ring* buffer."
  (interactive)
  (browse-kill-ring-insert-new 'insert 'move t))

(defun browse-kill-ring-prepend-insert (&optional quit)
  "Like `browse-kill-ring-insert', but it places the entry at the beginning
of the buffer as opposed to point.  Point is left unchanged after inserting."
  (interactive "P")
  (browse-kill-ring-do-prepend-insert (current-buffer)
                                      (point)
                                      quit))

(defun browse-kill-ring-prepend-insert-and-quit ()
  "Like `browse-kill-ring-prepend-insert', but close the *Kill Ring* buffer."
  (interactive)
  (browse-kill-ring-prepend-insert t))

(defun browse-kill-ring-prepend-insert-and-move (&optional quit)
  "Like `browse-kill-ring-prepend-insert', but move the entry to the front
of the *Kill Ring*."
  (interactive "P")
  (let ((buf (current-buffer))
        (pt (point)))
    (browse-kill-ring-do-prepend-insert buf pt quit)
    (let ((str (browse-kill-ring-current-string buf pt)))
      (browse-kill-ring-delete)
      (kill-new str)))
  (unless quit
    (browse-kill-ring-update)))

(defun browse-kill-ring-prepend-insert-move-and-quit ()
  "Like `browse-kill-ring-prepend-insert-and-move', but close the
*Kill Ring* buffer."
  (interactive)
  (browse-kill-ring-prepend-insert-and-move t))

(defun browse-kill-ring-highlight-inserted (start end)
  (when browse-kill-ring-highlight-inserted-item
    ;; First, load the `pulse' library if needed.
    (when (or (eql browse-kill-ring-highlight-inserted-item 'pulse)
              (eql browse-kill-ring-highlight-inserted-item 't))
      (unless (and (require 'pulse nil t)
                   (fboundp 'pulse-momentary-highlight-region))
        (warn "Unable to load `pulse' library")
        (setq browse-kill-ring-highlight-inserted-item 'solid)))

    (case browse-kill-ring-highlight-inserted-item
      ((pulse t)
       (let ((pulse-delay .05) (pulse-iterations 10))
         (with-no-warnings
           (pulse-momentary-highlight-region
          start end browse-kill-ring-inserted-item-face))))
      ('solid
       (let ((o (make-overlay start end)))
         (overlay-put o 'face browse-kill-ring-inserted-item-face)
         (sit-for 0.5)
         (delete-overlay o))))))

(defmacro browse-kill-ring-prepare-to-insert (quit &rest body)
  "Restore window and buffer ready to insert `kill-ring' item.
Temporarily restore `browse-kill-ring-original-window' and
`browse-kill-ring-original-buffer' then evaluate BODY."
  `(progn
     (if ,quit
         (browse-kill-ring-quit)
       (browse-kill-ring-clear-preview))
     (with-selected-window browse-kill-ring-original-window
       (with-current-buffer browse-kill-ring-original-buffer
         (progn ,@body)
         (unless ,quit
           (browse-kill-ring-setup-preview-overlay
            (current-buffer)))))))

(defun browse-kill-ring-insert-and-highlight (str)
  "Helper function to insert text at point, highlighting it if appropriate."
      (let ((before-insert (point)))
        (let (deactivate-mark)
          (insert-for-yank str))
        (browse-kill-ring-highlight-inserted
         before-insert
         (point))))

(defun browse-kill-ring-do-prepend-insert (buf pt quit)
  (let ((str (browse-kill-ring-current-string buf pt)))
    (browse-kill-ring-prepare-to-insert
     quit
     (save-excursion
       (goto-char (point-min))
       (browse-kill-ring-insert-and-highlight str)))))

(defun browse-kill-ring-append-insert (&optional quit)
  "Like `browse-kill-ring-insert', but places the entry at the end of the
buffer as opposed to point.  Point is left unchanged after inserting."
  (interactive "P")
  (browse-kill-ring-do-append-insert (current-buffer)
                                     (point)
                                     quit))

(defun browse-kill-ring-append-insert-and-quit ()
  "Like `browse-kill-ring-append-insert', but close the *Kill Ring* buffer."
  (interactive)
  (browse-kill-ring-append-insert t))

(defun browse-kill-ring-append-insert-and-move (&optional quit)
  "Like `browse-kill-ring-append-insert', but move the entry to the front
of the *Kill Ring*."
  (interactive "P")
  (let ((buf (current-buffer))
        (pt (point)))
    (browse-kill-ring-do-append-insert buf pt quit)
    (let ((str (browse-kill-ring-current-string buf pt)))
      (browse-kill-ring-delete)
      (kill-new str)))
  (unless quit
    (browse-kill-ring-update)))

(defun browse-kill-ring-append-insert-move-and-quit ()
  "Like `browse-kill-ring-append-insert-and-move', but close the
*Kill Ring* buffer."
  (interactive)
  (browse-kill-ring-append-insert-and-move t))

(defun browse-kill-ring-do-append-insert (buf pt quit)
  (let ((str (browse-kill-ring-current-string buf pt)))
    (browse-kill-ring-prepare-to-insert
     quit
     (save-excursion
       (goto-char (point-max))
       (browse-kill-ring-insert-and-highlight str)))))

(defun browse-kill-ring-delete ()
  "Remove the item at point from the `kill-ring'."
  (interactive)
  (forward-line 0)
  (unwind-protect
    (let* ((over (browse-kill-ring-target-overlay-at (point)))
           (target (overlay-get over 'browse-kill-ring-target))
           (inhibit-read-only t))
      (delete-region (overlay-start over) (1+ (overlay-end over)))
      (setq kill-ring (delete target kill-ring))
      (cond
       ;; Don't try to delete anything else in an empty buffer.
       ((and (bobp) (eobp)) t)
       ;; The last entry was deleted, remove the preceeding separator.
       ((eobp)
        (progn
          (browse-kill-ring-forward -1)
          (let ((over (browse-kill-ring-target-overlay-at (point))))
            (delete-region (1+ (overlay-end over)) (point-max)))))
       ;; Deleted a middle entry, delete following separator.
       ((get-text-property (point) 'browse-kill-ring-extra)
        (let ((prev (previous-single-property-change (point) 'browse-kill-ring-extra))
              (next (next-single-property-change (point) 'browse-kill-ring-extra)))
          (when prev (incf prev))
          (when next (incf next))
          (delete-region (or prev (point-min)) (or next (point-max))))))))
  (browse-kill-ring-resize-window)
  (browse-kill-ring-forward 0))

;; code from browse-kill-ring+.el
(defun browse-kill-ring-target-overlay-at (position &optional no-error)
  "Return overlay at POSITION that has property `browse-kill-ring-target'.
If no such overlay, raise an error unless NO-ERROR is true, in which
case retun nil."
  (let ((ovs  (overlays-at (point))))
    (catch 'browse-kill-ring-target-overlay-at
      (dolist (ov  ovs)
        (when (overlay-get ov 'browse-kill-ring-target)
          (throw 'browse-kill-ring-target-overlay-at ov)))
      (unless no-error
        (error "No selection-ring item here")))))

;; Find the string to insert at the point by looking for the overlay.
(defun browse-kill-ring-current-string (buf pt &optional no-error)
  (let ((o (browse-kill-ring-target-overlay-at pt t)))
    (if o
        (overlay-get o 'browse-kill-ring-target)
      (unless no-error
        (error "No kill ring item here")))))

(defun browse-kill-ring-do-insert (buf pt quit)
  (let ((str (browse-kill-ring-current-string buf pt)))
    (setq kill-ring-yank-pointer
          (browse-kill-ring-current-kill-ring-yank-pointer buf pt))
    (browse-kill-ring-prepare-to-insert
     quit
     (when browse-kill-ring-this-buffer-replace-yanked-text
       (delete-region (mark) (point)))
     (when (and delete-selection-mode
                (not buffer-read-only)
                transient-mark-mode mark-active)
       (delete-active-region))
     (browse-kill-ring-insert-and-highlight str))))

(defun browse-kill-ring-update-highlighed-entry ()
  (when browse-kill-ring-highlight-current-entry
    (browse-kill-ring-update-highlighed-entry-1)))

(defun browse-kill-ring-clear-highlighed-entry ()
  (when browse-kill-ring-previous-overlay
    (assert (overlayp browse-kill-ring-previous-overlay))
    (overlay-put browse-kill-ring-previous-overlay 'face nil)))

(defun browse-kill-ring-update-highlighed-entry-1 ()
  (let ((current-overlay (browse-kill-ring-target-overlay-at (point) t)))
    (case current-overlay
      ;; No overlay at point.  Just clear all current highlighting.
      ((nil) (browse-kill-ring-clear-highlighed-entry))
      ;; Still on the previous overlay.
      (browse-kill-ring-previous-overlay t)
      ;; Otherwise, we've changed overlay.  Clear current
      ;; highlighting, and highlight the new overlay.
      (t
       (assert (overlay-get current-overlay
                            'browse-kill-ring-target) t)
       (browse-kill-ring-clear-highlighed-entry)
       (setq browse-kill-ring-previous-overlay current-overlay)
       (overlay-put current-overlay 'face
                    browse-kill-ring-current-entry-face)))))

(defun browse-kill-ring-forward (&optional arg)
  "Move forward by ARG `kill-ring' entries."
  (interactive "p")
  (beginning-of-line)
  (while (not (zerop arg))
    (let ((o (browse-kill-ring-target-overlay-at (point) t)))
      (if (< arg 0)
          (progn
            (incf arg)
            (when o
              (goto-char (overlay-start o))
              (setq o nil))
            (while (not (or o (bobp)))
              (goto-char (previous-overlay-change (point)))
              (setq o (browse-kill-ring-target-overlay-at (point) t))))
        (progn
          (decf arg)
          ;; We're on a browse-kill-ring overlay, skip to the end of it.
          (when o
            (goto-char (overlay-end o))
            (setq o nil))
          (while (not (or o (eobp)))
            (goto-char (next-overlay-change (point)))
            (setq o (browse-kill-ring-target-overlay-at (point) t)))))))
  (when browse-kill-ring-recenter
    (recenter 1)))

(defun browse-kill-ring-previous (&optional arg)
  "Move backward by ARG `kill-ring' entries."
  (interactive "p")
  (browse-kill-ring-forward (- arg)))

(defun browse-kill-ring-read-regexp (msg &optional empty-is-nil-p)
  (let* ((default (car regexp-history))
         (input
          (read-from-minibuffer
           (if (and default (not empty-is-nil-p))
               (format "%s for regexp (default `%s'): "
                       msg
                       default)
             (format "%s (regexp): " msg))
           nil
           nil
           nil
           'regexp-history
           (if empty-is-nil-p default nil))))
    (if (equal input "")
        (if empty-is-nil-p nil default)
      input)))

(defun browse-kill-ring-search-forward (regexp &optional backwards)
  "Move to the next `kill-ring' entry matching REGEXP from point.
If optional arg BACKWARDS is non-nil, move to the previous matching
entry."
  (interactive
   (list (browse-kill-ring-read-regexp "Search forward")
         current-prefix-arg))
  (let ((orig (point)))
    (browse-kill-ring-forward (if backwards -1 1))
    (let ((over (browse-kill-ring-target-overlay-at (point) t)))
      (while (and over
                  (not (if backwards (bobp) (eobp)))
                  (not (string-match regexp
                                     (overlay-get over
                                                  'browse-kill-ring-target))))
        (browse-kill-ring-forward (if backwards -1 1))
        (setq over (browse-kill-ring-target-overlay-at (point) t)))
      (unless (and over
                   (string-match regexp
                                 (overlay-get over
                                              'browse-kill-ring-target)))
        (progn
          (goto-char orig)
          (message "No more `kill-ring' entries matching %s" regexp))))))

(defun browse-kill-ring-search-backward (regexp)
  "Move to the previous `kill-ring' entry matching REGEXP from point."
  (interactive
   (list (browse-kill-ring-read-regexp "Search backward")))
  (browse-kill-ring-search-forward regexp t))

(defun browse-kill-ring-quit ()
  "Take the action specified by `browse-kill-ring-quit-action'."
  (interactive)
  (browse-kill-ring-cleanup-on-exit)
  (case browse-kill-ring-quit-action
    (save-and-restore
      (if (< emacs-major-version 24)
        (let (buf (current-buffer))
             (set-window-configuration browse-kill-ring-original-window-config)
           (kill-buffer buf))
       (quit-window)))
    (kill-and-delete-window
     (kill-buffer (current-buffer))
     (unless (= (count-windows) 1)
       (delete-window)))
    (bury-and-delete-window
     (bury-buffer)
     (unless (= (count-windows) 1)
       (delete-window)))
    (t
     (funcall browse-kill-ring-quit-action))))

(put 'browse-kill-ring-mode 'mode-class 'special)
(define-derived-mode browse-kill-ring-mode fundamental-mode
  "Kill Ring"
  "A major mode for browsing the `kill-ring'.
You most likely do not want to call `browse-kill-ring-mode' directly; use
`browse-kill-ring' instead.

\\{browse-kill-ring-mode-map}"
  (set (make-local-variable 'font-lock-defaults)
       '(nil t nil nil nil
             (font-lock-fontify-region-function . browse-kill-ring-fontify-region)))
  (define-key browse-kill-ring-mode-map (kbd "q") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "U") 'browse-kill-ring-undo-other-window)
  (define-key browse-kill-ring-mode-map (kbd "d") 'browse-kill-ring-delete)
  (define-key browse-kill-ring-mode-map (kbd "s") 'browse-kill-ring-search-forward)
  (define-key browse-kill-ring-mode-map (kbd "r") 'browse-kill-ring-search-backward)
  (define-key browse-kill-ring-mode-map (kbd "g") 'browse-kill-ring-update)
  (define-key browse-kill-ring-mode-map (kbd "l") 'browse-kill-ring-occur)
  (define-key browse-kill-ring-mode-map (kbd "e") 'browse-kill-ring-edit)
  (define-key browse-kill-ring-mode-map (kbd "n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "p") 'browse-kill-ring-previous)
  (define-key browse-kill-ring-mode-map [(mouse-2)] 'browse-kill-ring-mouse-insert)
  (define-key browse-kill-ring-mode-map (kbd "?") 'describe-mode)
  (define-key browse-kill-ring-mode-map (kbd "h") 'describe-mode)
  (define-key browse-kill-ring-mode-map (kbd "y") 'browse-kill-ring-insert)
  (define-key browse-kill-ring-mode-map (kbd "u") 'browse-kill-ring-insert-move-and-quit)
  (define-key browse-kill-ring-mode-map (kbd "M-<return>") 'browse-kill-ring-insert-move-and-quit)
  (define-key browse-kill-ring-mode-map (kbd "i") 'browse-kill-ring-insert)
  (define-key browse-kill-ring-mode-map (kbd "o") 'browse-kill-ring-insert-and-move)
  (define-key browse-kill-ring-mode-map (kbd "x") 'browse-kill-ring-insert-and-delete)
  (define-key browse-kill-ring-mode-map (kbd "RET") 'browse-kill-ring-insert-and-quit)
  (define-key browse-kill-ring-mode-map (kbd "b") 'browse-kill-ring-prepend-insert)
  (define-key browse-kill-ring-mode-map (kbd "a") 'browse-kill-ring-append-insert))

;;;###autoload
(defun browse-kill-ring-default-keybindings ()
  "Set up M-y (`yank-pop') so that it can invoke `browse-kill-ring'.
Normally, if M-y was not preceeded by C-y, then it has no useful
behavior.  This function sets things up so that M-y will invoke
`browse-kill-ring'."
  (interactive)
  (defadvice yank-pop (around kill-ring-browse-maybe (arg))
    "If last action was not a yank, run `browse-kill-ring' instead."
    ;; yank-pop has an (interactive "*p") form which does not allow
    ;; it to run in a read-only buffer.  We want browse-kill-ring to
    ;; be allowed to run in a read only buffer, so we change the
    ;; interactive form here.  In that case, we need to
    ;; barf-if-buffer-read-only if we're going to call yank-pop with
    ;; ad-do-it
    (interactive "p")
    (if (not (eq last-command 'yank))
        (browse-kill-ring)
      (barf-if-buffer-read-only)
      ad-do-it))
  (ad-activate 'yank-pop))

(define-derived-mode browse-kill-ring-edit-mode fundamental-mode
  "Kill Ring Edit"
  "A major mode for editing a `kill-ring' entry.
You most likely do not want to call `browse-kill-ring-edit-mode'
directly; use `browse-kill-ring' instead.

\\{browse-kill-ring-edit-mode-map}"
  (define-key browse-kill-ring-edit-mode-map
    (kbd "C-c C-c") 'browse-kill-ring-edit-finish)
  (define-key browse-kill-ring-edit-mode-map
    (kbd "C-c C-k") 'browse-kill-ring-edit-abort))

(defvar browse-kill-ring-edit-target nil)
(make-variable-buffer-local 'browse-kill-ring-edit-target)

(defun browse-kill-ring-edit ()
  "Edit the `kill-ring' entry at point."
  (interactive)
  (let* ((over (browse-kill-ring-target-overlay-at (point)))
         (target (overlay-get over 'browse-kill-ring-target))
         (target-cell (member target kill-ring)))
    (unless target-cell
      (error "Item deleted from the kill-ring"))
    (switch-to-buffer (get-buffer-create "*Kill Ring Edit*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert target)
    (goto-char (point-min))
    (browse-kill-ring-resize-window)
    (browse-kill-ring-edit-mode)
    (setq header-line-format
          '(:eval
            (substitute-command-keys
             "Edit, then \\[browse-kill-ring-edit-finish] to \
update entry and quit -- \\[browse-kill-ring-edit-abort] to abort.")))
    (when browse-kill-ring-show-preview
      (add-hook 'post-command-hook
                'browse-kill-ring-preview-update-for-edit nil t))
    (setq browse-kill-ring-edit-target target-cell)))

(defun browse-kill-ring-edit-finalise (entry)
  "Common code called after `browse-kill-ring-edit' has finished

This common code is called after `browse-kill-ring-edit-finish'
and `browse-kill-ring-edit-abort'.  It kills the edit buffer, and
reselects ENTRY in the `*Kill Ring*' buffer."
  ;; Kill the edit buffer.  Maybe we should do more to keep track of
  ;; the edit buffer so we can kill it even if we're not in it?
  (when (eq major-mode 'browse-kill-ring-edit-mode)
    (kill-buffer))
  ;; The user might have rearranged the windows
  (when (eq major-mode 'browse-kill-ring-mode)
    (browse-kill-ring-setup (current-buffer)
                            browse-kill-ring-original-buffer
                            browse-kill-ring-original-window
                            nil
                            browse-kill-ring-original-window-config)
    (browse-kill-ring-resize-window)
    (when entry
      (browse-kill-ring-find-entry entry))))

(defun browse-kill-ring-edit-finish ()
  "Commit the edit changes to the `kill-ring'."
  (interactive)
  (unless browse-kill-ring-edit-target
    (error "Not editing a kill-ring item"))
  (let* ((updated-entry (buffer-string))
         (delete-entry? (string= updated-entry ""))
         (current-entry browse-kill-ring-edit-target)
         (select-entry nil))
    (setq browse-kill-ring-edit-target nil)
    (if delete-entry?
        ;; Find the previous entry in the list to select, then
        ;; delete the entry that was just edited to empty.
        (progn
          (setq select-entry
                (cadr current-entry))
          (setq kill-ring
                (delete (car current-entry) kill-ring))
          (unless select-entry
            (setq select-entry (car (last kill-ring)))))
      ;; Update the entry that was just edited, and arrange to select
      ;; it.
      (setcar current-entry updated-entry)
      (setq select-entry updated-entry))
    (browse-kill-ring-edit-finalise select-entry)))

(defun browse-kill-ring-edit-abort ()
  "Abort the edit of the `kill-ring' item."
  (interactive)
  (let ((current-entry (if browse-kill-ring-edit-target
                           (car browse-kill-ring-edit-target)
                         nil)))
    (setq browse-kill-ring-edit-target nil)
    (browse-kill-ring-edit-finalise current-entry)))

(defmacro browse-kill-ring-add-overlays-for (item &rest body)
  (let ((beg (cl-gensym "browse-kill-ring-add-overlays-"))
        (end (cl-gensym "browse-kill-ring-add-overlays-")))
    `(let ((,beg (point))
           (,end
            (progn
              ,@body
              (point))))
       (let ((o (make-overlay ,beg ,end)))
         (overlay-put o 'browse-kill-ring-target ,item)
         (overlay-put o 'mouse-face 'highlight)))))
;; (put 'browse-kill-ring-add-overlays-for 'lisp-indent-function 1)

(defun browse-kill-ring-elide (str)
  (if (and browse-kill-ring-maximum-display-length
           (> (length str)
              browse-kill-ring-maximum-display-length))
      (concat (substring str 0 (- browse-kill-ring-maximum-display-length 3))
              (propertize "..." 'browse-kill-ring-extra t))
    str))

(defun browse-kill-ring-insert-as-one-line (items)
  (dolist (item items)
    (browse-kill-ring-add-overlays-for item
      (let* ((item (browse-kill-ring-elide item))
             (len (length item))
             (start 0)
             (newl (propertize "\\n" 'browse-kill-ring-extra t)))
        (while (and (< start len)
                    (string-match "\n" item start))
          (insert (substring item start (match-beginning 0))
                  newl)
          (setq start (match-end 0)))
        (insert (substring item start len))))
    (insert "\n")))

(defun browse-kill-ring-insert-as-separated (items)
  (while (cdr items)
    (browse-kill-ring-insert-as-separated-1 (car items) t)
    (setq items (cdr items)))
  (when items
    (browse-kill-ring-insert-as-separated-1 (car items) nil)))

(defun browse-kill-ring-insert-as-separated-1 (origitem separatep)
  (let* ((item (browse-kill-ring-elide origitem))
         (len (length item)))
    (browse-kill-ring-add-overlays-for origitem
                                       (insert item))
    ;; When the kill-ring has items with read-only text property at
    ;; **the end of** string, browse-kill-ring-setup fails with error
    ;; `Text is read-only'.  So inhibit-read-only here.
    ;; See http://bugs.debian.org/225082
    ;; - INOUE Hiroyuki <dombly@kc4.so-net.ne.jp>
    (let ((inhibit-read-only t))
      (insert "\n")
      (when separatep
        (insert (propertize browse-kill-ring-separator
                                             'browse-kill-ring-extra t
                                             'browse-kill-ring-separator t))
        (insert "\n")))))

(defun browse-kill-ring-occur (regexp)
  "Display all `kill-ring' entries matching REGEXP."
  (interactive
   (list (browse-kill-ring-read-regexp
          "Display kill ring entries matching" t)))
  (assert (eq major-mode 'browse-kill-ring-mode))
  (browse-kill-ring-setup (current-buffer)
                          browse-kill-ring-original-buffer
                          browse-kill-ring-original-window
                          regexp)
  (browse-kill-ring-resize-window))

(defun browse-kill-ring-fontify-on-property (prop face beg end)
  (save-excursion
    (goto-char beg)
    (let ((prop-end nil))
      (while
          (setq prop-end
                (let ((prop-beg (or (and (get-text-property (point) prop) (point))
                                    (next-single-property-change (point) prop nil end))))
                  (when (and prop-beg (not (= prop-beg end)))
                    (let ((prop-end (next-single-property-change prop-beg prop nil end)))
                      (when (and prop-end (not (= prop-end end)))
                        (put-text-property prop-beg prop-end 'face face)
                        prop-end)))))
        (goto-char prop-end)))))

(defun browse-kill-ring-fontify-region (beg end &optional verbose)
  (when verbose (message "Fontifying..."))
  (let ((buffer-read-only nil))
    (browse-kill-ring-fontify-on-property 'browse-kill-ring-extra 'bold beg end)
    (browse-kill-ring-fontify-on-property 'browse-kill-ring-separator
                                          browse-kill-ring-separator-face beg end))
  (when verbose (message "Fontifying...done")))

(defun browse-kill-ring-update ()
  "Update the buffer to reflect outside changes to `kill-ring'."
  (interactive)
  (assert (eq major-mode 'browse-kill-ring-mode))
  (browse-kill-ring-setup (current-buffer)
                          browse-kill-ring-original-buffer
                          browse-kill-ring-original-window)
  (browse-kill-ring-resize-window))

(defun browse-kill-ring-preview-update-text (preview-text)
  "Update `browse-kill-ring-preview-overlay' to show `PREVIEW-TEXT`."
  ;; If preview-text is nil, replacement should be nil too.
  (assert (overlayp browse-kill-ring-preview-overlay))
  (let ((replacement (when preview-text
                       (propertize preview-text 'face 'highlight))))
    (overlay-put browse-kill-ring-preview-overlay
                 'before-string replacement)))

(defun browse-kill-ring-preview-update-by-position (&optional pt)
  "Update `browse-kill-ring-preview-overlay' to match item at PT.
This function is called whenever the selection in the `*Kill
Ring*' buffer is adjusted, the `browse-kill-ring-preview-overlay'
is udpated to preview the text of the selection at PT (or the
current point if not specified)."
  (let ((new-text (browse-kill-ring-current-string
                   (current-buffer) (or pt (point)) t)))
    (browse-kill-ring-preview-update-text new-text)))

(defun browse-kill-ring-preview-update-for-edit ()
  "Update `browse-kill-ring-preview-overlay' after edits.
Callback triggered after a change in the *Kill Ring Edit* buffer,
update the preview in the original buffer."
  (browse-kill-ring-preview-update-text (buffer-string)))

(defun browse-kill-ring-current-index (buf pt)
  "Return current index."
  (let ((overlay-start-point
         (overlay-start
          (browse-kill-ring-target-overlay-at pt t)))
        (current-index 0)
        (stop-search nil)
        current-overlay-start-point)
    (save-excursion
      (goto-char (point-min))
      (while (not stop-search)
        (setq current-overlay-start-point
              (overlay-start
               (browse-kill-ring-target-overlay-at (point))))
        (if (eq overlay-start-point current-overlay-start-point)
            (setq stop-search t))
        (if (not stop-search)
          (progn
            (browse-kill-ring-forward 1)
            (setq current-index (1+ current-index))))))
    current-index))

(defun browse-kill-ring-current-kill-ring-yank-pointer (buf pt)
  "Return current kill-ring-yank-pointer."
  (let ((result-yank-pointer kill-ring)
        (current-string (browse-kill-ring-current-string buf pt))
        (found nil)
        (i 0))
    (if browse-kill-ring-display-duplicates
      (setq result-yank-pointer (nthcdr (browse-kill-ring-current-index buf pt) kill-ring))
      (if browse-kill-ring-display-leftmost-duplicate
        ;; search leftmost duplicate
        (while (< i (length kill-ring))
          (if (and (not found) (equal (substring-no-properties current-string) (substring-no-properties (elt kill-ring i))))
            (progn
              (setq result-yank-pointer (nthcdr i kill-ring))
              (setq found t)))
          (setq i (1+ i)))
        ;; search rightmost duplicate
        (setq i (1- (length kill-ring)))
        (while (<= 0 i)
          (if (and (not found) (equal (substring-no-properties current-string) (substring-no-properties (elt kill-ring i))))
            (progn
              (setq result-yank-pointer (nthcdr i kill-ring))
              (setq found t)))
          (setq i (1- i)))))
    result-yank-pointer))

(defun browse-kill-ring-clear-preview ()
  (when browse-kill-ring-preview-overlay
    (delete-overlay browse-kill-ring-preview-overlay)))

(defun browse-kill-ring-cleanup-on-exit ()
  "Function called when the user is finished with `browse-kill-ring'.
This function performs any cleanup that is required when the user
has finished interacting with the `*Kill Ring*' buffer.  For now
the only cleanup performed is to remove the preview overlay, if
it's turned on."
  (browse-kill-ring-clear-preview))

(defun browse-kill-ring-setup-preview-overlay (orig-buf)
  (with-current-buffer orig-buf
    (let* ((will-replace
           (or browse-kill-ring-this-buffer-replace-yanked-text
               (region-active-p)))
           (start (if will-replace
                      (min (point) (mark))
                    (point)))
           (end (if will-replace
                    (max (point) (mark))
                  (point))))
      (when browse-kill-ring-show-preview
        (browse-kill-ring-clear-preview)
        (setq browse-kill-ring-preview-overlay
              (make-overlay start end orig-buf))
        (overlay-put browse-kill-ring-preview-overlay
                     'invisible t)))))

(defun browse-kill-ring-setup (kill-buf orig-buf window &optional regexp window-config)
  (setq browse-kill-ring-this-buffer-replace-yanked-text
        (and
         browse-kill-ring-replace-yank
         (eq last-command 'yank)))
  (browse-kill-ring-setup-preview-overlay orig-buf)
  (with-current-buffer kill-buf
    (unwind-protect
        (progn
          (browse-kill-ring-mode)
          (setq buffer-read-only nil)
          (when (eq browse-kill-ring-display-style
                    'one-line)
            (setq truncate-lines t))
          (let ((inhibit-read-only t))
            (erase-buffer))
          (setq browse-kill-ring-original-buffer orig-buf
                browse-kill-ring-original-window window
                browse-kill-ring-original-window-config
                (or window-config
                    (current-window-configuration)))
          (let ((browse-kill-ring-maximum-display-length
                 (if (and browse-kill-ring-maximum-display-length
                          (<= browse-kill-ring-maximum-display-length 3))
                     4
                   browse-kill-ring-maximum-display-length))
                (items (mapcar
                        (if browse-kill-ring-depropertize
                            #'substring-no-properties
                          #'copy-sequence)
                        kill-ring)))
            (when (not browse-kill-ring-display-duplicates)
              ;; display leftmost or rightmost duplicate.
              ;; if `browse-kill-ring-display-leftmost-duplicate' is t,
              ;; display leftmost(last) duplicate.
              (cl-delete-duplicates items
                                 :test #'equal
                                 :from-end browse-kill-ring-display-leftmost-duplicate))
            (when (stringp regexp)
              (setq items (delq nil
                                (mapcar
                                 #'(lambda (item)
                                     (when (string-match regexp item)
                                       item))
                                 items))))
            (funcall (or (cdr (assq browse-kill-ring-display-style
                                    browse-kill-ring-display-styles))
                         (error "Invalid `browse-kill-ring-display-style': %s"
                                browse-kill-ring-display-style))
                     items)
            (when browse-kill-ring-show-preview
              (browse-kill-ring-preview-update-by-position (point-min))
              ;; Local post-command-hook, only happens in the *Kill
              ;; Ring* buffer
              (add-hook 'post-command-hook
                        'browse-kill-ring-preview-update-by-position
                        nil t)
              (add-hook 'kill-buffer-hook
                        'browse-kill-ring-cleanup-on-exit
                        nil t))
            (when browse-kill-ring-highlight-current-entry
              (add-hook 'post-command-hook
                        'browse-kill-ring-update-highlighed-entry
                        nil t))
;; Code from Michael Slass <mikesl@wrq.com>
            (message
             (let ((entry (if (= 1 (length kill-ring)) "entry" "entries")))
               (concat
                (if (and (not regexp)
                         browse-kill-ring-display-duplicates)
                    (format "%s %s in the kill ring."
                            (length kill-ring) entry)
                  (format "%s (of %s) %s in the kill ring shown."
                          (length items) (length kill-ring) entry))
                (substitute-command-keys
                 (concat "    Type \\[browse-kill-ring-quit] to quit.  "
                         "\\[describe-mode] for help.")))))
;; End code from Michael Slass <mikesl@wrq.com>
            (set-buffer-modified-p nil)
            (goto-char (point-min))
            (browse-kill-ring-forward 0)
            (setq mode-name (if regexp
                                (concat "Kill Ring [" regexp "]")
                              "Kill Ring"))
            (run-hooks 'browse-kill-ring-hook)))
      (progn
        (setq buffer-read-only t)))))

(defun browse-kill-ring-find-entry (entry-string)
  "Select entry matching ENTRY-STRING in current buffer.
Helper function that should be invoked in the *Kill Ring* buffer,
move the selection forward to the entry matching ENTRY-STRING.
If there's no matching entry then leave point at the start the
start of the buffer."
  (goto-char (point-min))
  (let ((stop-search nil)
        (search-found nil)
        current-target-string)
    (while (not stop-search)
      (setq current-target-string
            (browse-kill-ring-current-string (current-buffer) (point)))
      (if (not current-target-string)
          (setq stop-search t)
        (if (equal current-target-string entry-string)
            (progn
              (setq search-found t)
              (setq stop-search t))))
      (unless stop-search
        (browse-kill-ring-forward 1)))
    (unless search-found
      (goto-char (point-min)))))

;;;###autoload
(defun browse-kill-ring ()
  "Display items in the `kill-ring' in another buffer."
  (interactive)
  (if (eq major-mode 'browse-kill-ring-mode)
      (error "Already viewing the kill ring"))

  (let* ((orig-win (selected-window))
         (orig-buf (window-buffer orig-win))
         (buf (get-buffer-create "*Kill Ring*"))
         (kill-ring-yank-pointer-string
          (if kill-ring-yank-pointer
              (substring-no-properties (car kill-ring-yank-pointer)))))
    (browse-kill-ring-setup buf orig-buf orig-win)
    (pop-to-buffer buf)
    (browse-kill-ring-resize-window)
    (unless (eq kill-ring kill-ring-yank-pointer)
      (browse-kill-ring-find-entry kill-ring-yank-pointer-string))))

(provide 'browse-kill-ring)

;;; browse-kill-ring.el ends here
