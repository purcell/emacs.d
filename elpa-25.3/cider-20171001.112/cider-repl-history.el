;;; cider-repl-history.el --- REPL input history browser

;; Copyright (c) 2017 John Valente and browse-kill-ring authors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;; Based heavily on browse-kill-ring
;; https://github.com/browse-kill-ring/browse-kill-ring

;;; Commentary:

;; REPL input history browser for CIDER.

;; Allows you to browse the full input history for your REPL buffer, and
;; insert previous commands at the prompt.

;;; Code:

(require 'cl-lib)
(require 'cider-compat)
(require 'cider-popup)
(require 'clojure-mode)
(require 'derived)
(require 'pulse)

(defconst cider-repl-history-buffer "*cider-repl-history*")
(add-to-list 'cider-ancillary-buffers cider-repl-history-buffer)

(defgroup cider-repl-history nil
  "A package for browsing and inserting the items in the CIDER command history."
  :prefix "cider-repl-history-"
  :group 'cider)

(defvar cider-repl-history-display-styles
  '((separated . cider-repl-history-insert-as-separated)
    (one-line . cider-repl-history-insert-as-one-line)))

(defcustom cider-repl-history-display-style 'separated
  "How to display the CIDER command history items.

If `one-line', then replace newlines with \"\\n\" for display.

If `separated', then display `cider-repl-history-separator' between
entries."
  :type '(choice (const :tag "One line" one-line)
                 (const :tag "Separated" separated))
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-repl-history-quit-action 'quit-window
  "What action to take when `cider-repl-history-quit' is called.

If `bury-buffer', then simply bury the *cider-repl-history* buffer, but keep
the window.

If `bury-and-delete-window', then bury the buffer, and (if there is
more than one window) delete the window.

If `delete-and-restore', then restore the window configuration to what it was
before `cider-repl-history' was called, and kill the *cider-repl-history*
buffer.

If `quit-window', then restore the window configuration to what
it was before `cider-repl-history' was called, and bury *cider-repl-history*.
This is the default.

If `kill-and-delete-window', then kill the *cider-repl-history* buffer, and
delete the window on close.

Otherwise, it should be a function to call."
  ;; Note, if you use one of the non-"delete" options, after you "quit",
  ;; the *cider-repl-history* buffer is still available.  If you are using
  ;; `cider-repl-history-show-preview', and you switch to *cider-repl-history* (i.e.,
  ;; with C-x b), it will not give the preview unless and until you "update"
  ;; the *cider-repl-history* buffer.
  ;;
  ;; This really should not be an issue, because there's no reason to "switch"
  ;; back to the buffer.  If you want to get it back, you can just do C-c M-p
  ;; from the REPL buffer.

  ;; If you get in this situation and find it annoying, you can either disable
  ;; the preview, or set `cider-repl-history-quit-action' to 'delete-and-restore.
  ;; Then you will simply not have the *cider-repl-history* buffer after you quit,
  ;; and it won't be an issue.

  :type '(choice (const :tag "Bury buffer"
                        :value bury-buffer)
                 (const :tag "Bury buffer and delete window"
                        :value bury-and-delete-window)
                 (const :tag "Delete window"
                        :value delete-and-restore)
                 (const :tag "Save and restore"
                        :value quit-window)
                 (const :tag "Kill buffer and delete window"
                        :value kill-and-delete-window)
                 function)
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-repl-history-resize-window nil
  "Whether to resize the `cider-repl-history' window to fit its contents.
Value is either t, meaning yes, or a cons pair of integers,
 (MAXIMUM . MINIMUM) for the size of the window.  MAXIMUM defaults to
the window size chosen by `pop-to-buffer'; MINIMUM defaults to
`window-min-height'."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 (cons (integer :tag "Maximum") (integer :tag "Minimum")))
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-repl-history-separator ";;;;;;;;;;"
  "The string separating entries in the `separated' style.
See `cider-repl-history-display-style'."
  ;; The (default) separator is a Clojure comment, to preserve fontification
  ;; in the buffer.
  :type 'string
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-repl-history-recenter nil
  "If non-nil, then always keep the current entry at the top of the window."
  :type 'boolean
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-repl-history-highlight-current-entry nil
  "If non-nil, highlight the currently selected command history entry."
  :type 'boolean
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-repl-history-highlight-inserted-item nil
  "If non-nil, then temporarily highlight the inserted command history entry.
The value selected controls how the inserted item is highlighted,
possible values are `solid' (highlight the inserted text for a
fixed period of time), or `pulse' (fade out the highlighting gradually).
Setting this variable to the value t will select the default
highlighting style, which currently `pulse'.

The variable `cider-repl-history-inserted-item-face' contains the
face used for highlighting."
  :type '(choice (const nil) (const t) (const solid) (const pulse))
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-repl-history-separator-face 'bold
  "The face in which to highlight the `cider-repl-history-separator'."
  :type 'face
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-repl-history-current-entry-face 'highlight
  "The face in which to highlight the command history current entry."
  :type 'face
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-repl-history-inserted-item-face 'highlight
  "The face in which to highlight the inserted item."
  :type 'face
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-repl-history-maximum-display-length nil
  "Whether or not to limit the length of displayed items.

If this variable is an integer, the display of the command history will be
limited to that many characters.
Setting this variable to nil means no limit."
  :type '(choice (const :tag "None" nil)
                 integer)
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-repl-history-display-duplicates t
  "If non-nil, then display duplicate items in the command history."
  :type 'boolean
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-repl-history-display-duplicate-highest t
  "When `cider-repl-history-display-duplicates' is nil, then display highest (most recent) duplicate items in the command history."
  :type 'boolean
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-repl-history-text-properties nil
  "If non-nil, maintain text properties of the command history items."
  :type 'boolean
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-repl-history-hook nil
  "A list of functions to call after `cider-repl-history'."
  :type 'hook
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-repl-history-show-preview nil
  "If non-nil, show a preview of the inserted text in the REPL buffer.

The REPL buffer would show a preview of what the buffer would look like
if the item under point were inserted."

  :type 'boolean
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defvar cider-repl-history-repl-window nil
  "The window in which chosen command history data will be inserted.
It is probably not a good idea to set this variable directly; simply
call `cider-repl-history' again.")

(defvar cider-repl-history-repl-buffer nil
  "The buffer in which chosen command history data will be inserted.
It is probably not a good idea to set this variable directly; simply
call `cider-repl-history' again.")

(defvar cider-repl-history-preview-overlay nil
  "The overlay used to preview what would happen if the user inserted the given text.")

(defvar cider-repl-history-previous-overlay nil
  "Previous overlay within *cider-repl-history* buffer.")


(defun cider-repl-history-get-history ()
  "Function to retrieve history from the REPL buffer."
  (if cider-repl-history-repl-buffer
      (buffer-local-value
       'cider-repl-input-history
       cider-repl-history-repl-buffer)
    (error "Variable `cider-repl-history-repl-buffer' not bound to a buffer")))

(defun cider-repl-history-resize-window ()
  "If variable `cider-repl-history-resize-window' is non-nil, resize the *cider-repl-history* window."
  (when cider-repl-history-resize-window
    (apply #'fit-window-to-buffer (selected-window)
           (if (consp cider-repl-history-resize-window)
               (list (car cider-repl-history-resize-window)
                     (or (cdr cider-repl-history-resize-window)
                         window-min-height))
             (list nil window-min-height)))))

(defun cider-repl-history-read-regexp (msg use-default-p)
  "Get a regular expression from the user, prompting with MSG; previous entry is default if USE-DEFAULT-P."
  (let* ((default (car regexp-history))
         (input
          (read-from-minibuffer
           (if (and default use-default-p)
               (format "%s for regexp (default `%s'): "
                       msg
                       default)
             (format "%s (regexp): " msg))
           nil
           nil
           nil
           'regexp-history
           (if use-default-p nil default))))
    (if (equal input "")
        (if use-default-p default nil)
      input)))

(defun cider-repl-history-clear-preview ()
  "Clear the preview, if one is present."
  (interactive)
  (when cider-repl-history-preview-overlay
    (cl-assert (overlayp cider-repl-history-preview-overlay))
    (delete-overlay cider-repl-history-preview-overlay)))

(defun cider-repl-history-cleanup-on-exit ()
  "Function called when the user is finished with `cider-repl-history'.
This function performs any cleanup that is required when the user
has finished interacting with the *cider-repl-history* buffer.  For now
the only cleanup performed is to remove the preview overlay, if
it's turned on."
  (cider-repl-history-clear-preview))

(defun cider-repl-history-quit ()
  "Take the action specified by `cider-repl-history-quit-action'."
  (interactive)
  (cider-repl-history-cleanup-on-exit)
  (pcase cider-repl-history-quit-action
    (`delete-and-restore
     (quit-restore-window (selected-window) 'kill))
    (`quit-window
     (quit-window))
    (`kill-and-delete-window
     (kill-buffer (current-buffer))
     (unless (= (count-windows) 1)
       (delete-window)))
    (`bury-and-delete-window
     (bury-buffer)
     (unless (= (count-windows) 1)
       (delete-window)))
    (_
     (funcall cider-repl-history-quit-action))))

(defun cider-repl-history-preview-overlay-setup (orig-buf)
  "Setup the preview overlay in ORIG-BUF."
  (when cider-repl-history-show-preview
    (with-current-buffer orig-buf
      (let* ((will-replace (region-active-p))
             (start (if will-replace
                        (min (point) (mark))
                      (point)))
             (end (if will-replace
                      (max (point) (mark))
                    (point))))
        (cider-repl-history-clear-preview)
        (setq cider-repl-history-preview-overlay
              (make-overlay start end orig-buf))
        (overlay-put cider-repl-history-preview-overlay
                     'invisible t)))))

(defun cider-repl-history-highlight-inserted (start end)
  "Insert the text between START and END."
  (pcase cider-repl-history-highlight-inserted-item
    ((or `pulse `t)
     (let ((pulse-delay .05) (pulse-iterations 10))
       (with-no-warnings
         (pulse-momentary-highlight-region
          start end cider-repl-history-inserted-item-face))))
    (`solid
     (let ((o (make-overlay start end)))
       (overlay-put o 'face cider-repl-history-inserted-item-face)
       (sit-for 0.5)
       (delete-overlay o)))))

(defun cider-repl-history-insert-and-highlight (str)
  "Helper function to insert STR at point, highlighting it if appropriate."
  (let ((before-insert (point)))
    (let (deactivate-mark)
      (insert-for-yank str))
    (cider-repl-history-highlight-inserted
     before-insert
     (point))))

(defun cider-repl-history-target-overlay-at (position &optional no-error)
  "Return overlay at POSITION that has property `cider-repl-history-target'.
If no such overlay, raise an error unless NO-ERROR is true, in which
case retun nil."
  (let ((ovs  (overlays-at (point))))
    (catch 'cider-repl-history-target-overlay-at
      (dolist (ov ovs)
        (when (overlay-get ov 'cider-repl-history-target)
          (throw 'cider-repl-history-target-overlay-at ov)))
      (unless no-error
        (error "No CIDER history item here")))))

(defun cider-repl-history-current-string (pt &optional no-error)
  "Find the string to insert into the REPL by looking for the overlay at PT; might error unless NO-ERROR set."
  (let ((o (cider-repl-history-target-overlay-at pt t)))
    (if o
        (overlay-get o 'cider-repl-history-target)
      (unless no-error
        (error "No CIDER history item in this buffer")))))

(defun cider-repl-history-do-insert (buf pt)
  "Helper function to insert text from BUF at PT into the REPL buffer and kill *cider-repl-history*."
  ;; Note: as mentioned at the top, this file is based on browse-kill-ring,
  ;; which has numerous insertion options.  The functionality of
  ;; browse-kill-ring allows users to insert at point, and move point to the end
  ;; of the inserted text; or insert at the beginning or end of the buffer,
  ;; while leaving point alone.  And each of these had the option of leaving the
  ;; history buffer in place, or getting rid of it.  That was appropriate for a
  ;; generic paste tool, but for inserting a previous command into an
  ;; interpreter, I felt the only useful option would be inserting it at the end
  ;; and quitting the history buffer, so that is all that's provided.
  (let ((str (cider-repl-history-current-string pt)))
    (cider-repl-history-quit)
    (with-selected-window cider-repl-history-repl-window
      (with-current-buffer cider-repl-history-repl-buffer
        (let ((max (point-max)))
          (if (= max (point))
              (cider-repl-history-insert-and-highlight str)
            (save-excursion
              (goto-char max)
              (cider-repl-history-insert-and-highlight str))))))))

(defun cider-repl-history-insert-and-quit ()
  "Insert the item into the REPL buffer, and close *cider-repl-history*.

The text is always inserted at the very bottom of the REPL buffer.  If your
cursor is already at the bottom, it is advanced to the end of the inserted
text.  If your cursor is somewhere else, the cursor is not moved, but the
text is still inserted at the end."
  (interactive)
  (cider-repl-history-do-insert (current-buffer) (point)))

(defun cider-repl-history-mouse-insert (e)
  "Insert the item at E into the REPL buffer, and close *cider-repl-history*.

The text is always inserted at the very bottom of the REPL buffer.  If your
cursor is already at the bottom, it is advanced to the end of the inserted
text.  If your cursor is somewhere else, the cursor is not moved, but the
text is still inserted at the end."
  (interactive "e")
  (let* ((data (save-excursion
                 (mouse-set-point e)
                 (cons (current-buffer) (point))))
         (buf (car data))
         (pt (cdr data)))
    (cider-repl-history-do-insert buf pt)))

(defun cider-repl-history-clear-highlighed-entry ()
  "Clear the highlighted entry, when one exists."
  (when cider-repl-history-previous-overlay
    (cl-assert (overlayp cider-repl-history-previous-overlay)
               nil "not an overlay")
    (overlay-put cider-repl-history-previous-overlay 'face nil)))

(defun cider-repl-history-update-highlighed-entry ()
  "Update highlighted entry, when feature is turned on."
  (when cider-repl-history-highlight-current-entry
    (if-let ((current-overlay (cider-repl-history-target-overlay-at (point) t)))
        (unless (equal cider-repl-history-previous-overlay current-overlay)
          ;; We've changed overlay.  Clear current highlighting,
          ;; and highlight the new overlay.
          (cl-assert (overlay-get current-overlay 'cider-repl-history-target) t)
          (cider-repl-history-clear-highlighed-entry)
          (setq cider-repl-history-previous-overlay current-overlay)
          (overlay-put current-overlay 'face
                       cider-repl-history-current-entry-face))
      ;; No overlay at point.  Just clear all current highlighting.
      (cider-repl-history-clear-highlighed-entry))))

(defun cider-repl-history-forward (&optional arg)
  "Move forward by ARG command history entries."
  (interactive "p")
  (beginning-of-line)
  (while (not (zerop arg))
    (let ((o (cider-repl-history-target-overlay-at (point) t)))
      (cond
       ((>= arg 0)
        (setq arg (1- arg))
        ;; We're on a cider-repl-history overlay, skip to the end of it.
        (when o
          (goto-char (overlay-end o))
          (setq o nil))
        (while (not (or o (eobp)))
          (goto-char (next-overlay-change (point)))
          (setq o (cider-repl-history-target-overlay-at (point) t))))
       (t
        (setq arg (1+ arg))
        (when o
          (goto-char (overlay-start o))
          (setq o nil))
        (while (not (or o (bobp)))
          (goto-char (previous-overlay-change (point)))
          (setq o (cider-repl-history-target-overlay-at (point) t)))))))
  (when cider-repl-history-recenter
    (recenter 1)))

(defun cider-repl-history-previous (&optional arg)
  "Move backward by ARG command history entries."
  (interactive "p")
  (cider-repl-history-forward (- arg)))

(defun cider-repl-history-search-forward (regexp &optional backwards)
  "Move to the next command history entry matching REGEXP from point.
If optional arg BACKWARDS is non-nil, move to the previous matching
entry."
  (interactive
   (list (cider-repl-history-read-regexp "Search forward" t)
         current-prefix-arg))
  (let ((orig (point)))
    (cider-repl-history-forward (if backwards -1 1))
    (let ((over (cider-repl-history-target-overlay-at (point) t)))
      (while (and over
                  (not (if backwards (bobp) (eobp)))
                  (not (string-match regexp
                                     (overlay-get over
                                                  'cider-repl-history-target))))
        (cider-repl-history-forward (if backwards -1 1))
        (setq over (cider-repl-history-target-overlay-at (point) t)))
      (unless (and over
                   (string-match regexp
                                 (overlay-get over
                                              'cider-repl-history-target)))
        (goto-char orig)
        (message "No more command history entries matching %s" regexp)))))

(defun cider-repl-history-search-backward (regexp)
  "Move to the previous command history entry matching REGEXP from point."
  (interactive
   (list (cider-repl-history-read-regexp "Search backward" t)))
  (cider-repl-history-search-forward regexp t))

(defun cider-repl-history-elide (str)
  "If STR is too long, abbreviate it with an ellipsis; otherwise, return it unchanged."
  (if (and cider-repl-history-maximum-display-length
           (> (length str)
              cider-repl-history-maximum-display-length))
      (concat (substring str 0 (- cider-repl-history-maximum-display-length 3))
              (propertize "..." 'cider-repl-history-extra t))
    str))

(defmacro cider-repl-history-add-overlays-for (item &rest body)
  "Add overlays for ITEM, and execute BODY."
  (let ((beg (cl-gensym "cider-repl-history-add-overlays-"))
        (end (cl-gensym "cider-repl-history-add-overlays-")))
    `(let ((,beg (point))
           (,end
            (progn
              ,@body
              (point))))
       (let ((o (make-overlay ,beg ,end)))
         (overlay-put o 'cider-repl-history-target ,item)
         (overlay-put o 'mouse-face 'highlight)))))

(defun cider-repl-history-insert-as-separated (items)
  "Insert ITEMS into the current buffer, with separators between items."
  (while items
    (let* ((origitem (car items))
           (item (cider-repl-history-elide origitem))
           (len (length item)))
      (cider-repl-history-add-overlays-for origitem (insert item))
      ;; When the command history has items with read-only text property at
      ;; **the end of** string, cider-repl-history-setup fails with error
      ;; `Text is read-only'.  So inhibit-read-only here.
      ;; See http://bugs.debian.org/225082
      (let ((inhibit-read-only t))
        (insert "\n")
        (when (cdr items)
          (insert (propertize cider-repl-history-separator
                              'cider-repl-history-extra t
                              'cider-repl-history-separator t))
          (insert "\n"))))
    (setq items (cdr items))))

(defun cider-repl-history-insert-as-one-line (items)
  "Insert ITEMS into the current buffer, formatting each item as a single line.

An explicit newline character will replace newlines so that the text retains its
spacing when it's actually inserted into the REPL buffer."
  (dolist (item items)
    (cider-repl-history-add-overlays-for
     item
     (let* ((item (cider-repl-history-elide item))
            (len (length item))
            (start 0)
            (newl (propertize "\\n" 'cider-repl-history-extra t)))
       (while (and (< start len)
                   (string-match "\n" item start))
         (insert (substring item start (match-beginning 0))
                 newl)
         (setq start (match-end 0)))
       (insert (substring item start len))))
    (insert "\n")))

(defun cider-repl-history-preview-update-text (preview-text)
  "Update `cider-repl-history-preview-overlay' to show `PREVIEW-TEXT`."
  ;; If preview-text is nil, replacement should be nil too.
  (cl-assert (overlayp cider-repl-history-preview-overlay))
  (let ((replacement (when preview-text
                       (propertize preview-text 'face 'highlight))))
    (overlay-put cider-repl-history-preview-overlay
                 'before-string replacement)))

(defun cider-repl-history-preview-update-by-position (&optional pt)
  "Update `cider-repl-history-preview-overlay' to match item at PT.

This function is called whenever the selection in the *cider-repl-history*
buffer is adjusted, the `cider-repl-history-preview-overlay'
is udpated to preview the text of the selection at PT (or the
current point if not specified)."
  (let ((new-text (cider-repl-history-current-string
                   (or pt (point)) t)))
    (cider-repl-history-preview-update-text new-text)))

(defun cider-repl-history-undo-other-window ()
  "Undo the most recent change in the other window's buffer.
You most likely want to use this command for undoing an insertion of
text from the *cider-repl-history* buffer."
  (interactive)
  (with-current-buffer cider-repl-history-repl-buffer
    (undo)))

(defun cider-repl-history-setup (repl-win repl-buf history-buf &optional regexp)
  "Setup: REPL-WIN and REPL-BUF are where to insert commands, HISTORY-BUF is the history, and optional arg REGEXP is a filter."
  (cider-repl-history-preview-overlay-setup repl-buf)
  (with-current-buffer history-buf
    (unwind-protect
        (progn
          (cider-repl-history-mode)
          (setq buffer-read-only nil)
          (when (eq 'one-line cider-repl-history-display-style)
            (setq truncate-lines t))
          (let ((inhibit-read-only t))
            (erase-buffer))
          (setq cider-repl-history-repl-buffer repl-buf)
          (setq cider-repl-history-repl-window repl-win)
          (let* ((cider-repl-history-maximum-display-length
                  (if (and cider-repl-history-maximum-display-length
                           (<= cider-repl-history-maximum-display-length 3))
                      4
                    cider-repl-history-maximum-display-length))
                 (cider-command-history (cider-repl-history-get-history))
                 (items (mapcar
                         (if cider-repl-history-text-properties
                             #'copy-sequence
                           #'substring-no-properties)
                         cider-command-history)))
            (when (not cider-repl-history-display-duplicates)
              ;; display highest or lowest duplicate.
              ;; if `cider-repl-history-display-duplicate-highest' is t,
              ;; display highest (most recent) duplicate.
              (cl-delete-duplicates
               items
               :test #'equal
               :from-end cider-repl-history-display-duplicate-highest))
            (when (stringp regexp)
              (setq items (delq nil
                                (mapcar
                                 #'(lambda (item)
                                     (when (string-match regexp item)
                                       item))
                                 items))))
            (funcall (or (cdr (assq cider-repl-history-display-style
                                    cider-repl-history-display-styles))
                         (error "Invalid `cider-repl-history-display-style': %s"
                                cider-repl-history-display-style))
                     items)
            (when cider-repl-history-show-preview
              (cider-repl-history-preview-update-by-position (point-min))
              ;; Local post-command-hook, only happens in *cider-repl-history*
              (add-hook 'post-command-hook
                        'cider-repl-history-preview-update-by-position
                        nil t)
              (add-hook 'kill-buffer-hook
                        'cider-repl-history-cleanup-on-exit
                        nil t))
            (when cider-repl-history-highlight-current-entry
              (add-hook 'post-command-hook
                        'cider-repl-history-update-highlighed-entry
                        nil t))
            (message
             (let ((entry (if (= 1 (length cider-command-history))
                              "entry"
                            "entries")))
               (concat
                (if (and (not regexp)
                         cider-repl-history-display-duplicates)
                    (format "%s %s in the command history."
                            (length cider-command-history) entry)
                  (format "%s (of %s) %s in the command history shown."
                          (length items) (length cider-command-history) entry))
                (substitute-command-keys
                 (concat "  Type \\[cider-repl-history-quit] to quit.  "
                         "\\[describe-mode] for help.")))))
            (set-buffer-modified-p nil)
            (goto-char (point-min))
            (cider-repl-history-forward 0)
            (setq mode-name (if regexp
                                (concat "History [" regexp "]")
                              "History"))
            (run-hooks 'cider-repl-history-hook)))
      (setq buffer-read-only t))))

(defun cider-repl-history-update ()
  "Update the history buffer to reflect the latest state of the command history."
  (interactive)
  (cl-assert (eq major-mode 'cider-repl-history-mode))
  (cider-repl-history-setup cider-repl-history-repl-window
                       cider-repl-history-repl-buffer
                       (current-buffer))
  (cider-repl-history-resize-window))

(defun cider-repl-history-occur (regexp)
  "Display all command history entries matching REGEXP."
  (interactive
   (list (cider-repl-history-read-regexp
          "Display command history entries matching" nil)))
  (cl-assert (eq major-mode 'cider-repl-history-mode))
  (cider-repl-history-setup cider-repl-history-repl-window
                       cider-repl-history-repl-buffer
                       (current-buffer)
                       regexp)
  (cider-repl-history-resize-window))

(put 'cider-repl-history-mode 'mode-class 'special)
(define-derived-mode cider-repl-history-mode clojure-mode "History"
  "Major mode for browsing the entries in the command input history.

\\{cider-repl-history-mode-map}"
  (define-key cider-repl-history-mode-map (kbd "n") 'cider-repl-history-forward)
  (define-key cider-repl-history-mode-map (kbd "p") 'cider-repl-history-previous)
  (define-key cider-repl-history-mode-map (kbd "SPC") 'cider-repl-history-insert-and-quit)
  (define-key cider-repl-history-mode-map (kbd "RET") 'cider-repl-history-insert-and-quit)
  (define-key cider-repl-history-mode-map [(mouse-2)] 'cider-repl-history-mouse-insert)
  (define-key cider-repl-history-mode-map (kbd "l") 'cider-repl-history-occur)
  (define-key cider-repl-history-mode-map (kbd "s") 'cider-repl-history-search-forward)
  (define-key cider-repl-history-mode-map (kbd "r") 'cider-repl-history-search-backward)
  (define-key cider-repl-history-mode-map (kbd "g") 'cider-repl-history-update)
  (define-key cider-repl-history-mode-map (kbd "q") 'cider-repl-history-quit)
  (define-key cider-repl-history-mode-map (kbd "U") 'cider-repl-history-undo-other-window)
  (define-key cider-repl-history-mode-map (kbd "?") 'describe-mode)
  (define-key cider-repl-history-mode-map (kbd "h") 'describe-mode))

;;;###autoload
(defun cider-repl-history ()
  "Display items in the CIDER command history in another buffer."
  (interactive)
  (when (eq major-mode 'cider-repl-history-mode)
    (user-error "Already viewing the CIDER command history"))

  (let* ((repl-win (selected-window))
         (repl-buf (window-buffer repl-win))
         (buf (get-buffer-create cider-repl-history-buffer)))
    (cider-repl-history-setup repl-win repl-buf buf)
    (pop-to-buffer buf)
    (cider-repl-history-resize-window)))

(provide 'cider-repl-history)

;;; cider-repl-history.el ends here
