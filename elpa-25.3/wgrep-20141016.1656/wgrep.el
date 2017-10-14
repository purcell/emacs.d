;;; wgrep.el --- Writable grep buffer and apply the changes to files

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: grep edit extensions
;; Package-Version: 20141016.1656
;; URL: http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep.el
;; Emacs: GNU Emacs 22 or later
;; Version: 2.1.10

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; wgrep allows you to edit a grep buffer and apply those changes to
;; the file buffer.

;;; Install:

;; Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (require 'wgrep)

;;; Usage:

;; You can edit the text in the *grep* buffer after typing C-c C-p.
;; After that the changed text is highlighted.
;; The following keybindings are defined:

;; C-c C-e : Apply the changes to file buffers.
;; C-c C-u : All changes are unmarked and ignored.
;; C-c C-d : Mark as delete to current line (including newline).
;; C-c C-r : Remove the changes in the region (these changes are not
;;           applied to the files. Of course, the remaining
;;           changes can still be applied to the files.)
;; C-c C-p : Toggle read-only area.
;; C-c C-k : Discard all changes and exit.
;; C-x C-q : Exit wgrep mode.

;; * To save all buffers that wgrep has changed, run
;;
;;   M-x wgrep-save-all-buffers

;; * To save buffer automatically when `wgrep-finish-edit'.
;;
;; (setq wgrep-auto-save-buffer t)

;; * You can change the default key binding to switch to wgrep.
;;
;; (setq wgrep-enable-key "r")

;; * To apply all changes wheather or not buffer is read-only.
;;
;; (setq wgrep-change-readonly-file t)

;;; History:

;; This program is a forked version. the original version can be downloaded from
;; http://www.bookshelf.jp/elc/grep-edit.el

;; Following added implementations and differences.
;; * Support GNU grep context option -A -B and -C
;; * Some bugfix. (wrong coloring text etc..)
;; * wdired.el like interface.
;; * Remove all advice.
;; * Bind to local variables. (grep-a-lot.el works well)
;; * After save buffer, colored face will be removed.
;; * Change face easy to see.
;; * Reinforce checking error.
;; * Support removing whole line include new-line.

;;; Code:

(require 'grep)

(declare-function image-get-display-property "image-mode.el" ())
(declare-function image-mode-as-text "image-mode.el" ())

(defgroup wgrep nil
  "Customize wgrep"
  :prefix "wgrep-"
  :group 'grep)

(defcustom wgrep-change-readonly-file nil
  "Non-nil means to enable change read-only files."
  :group 'wgrep
  :type 'boolean)

(defcustom wgrep-enable-key "\C-c\C-p"
  "This variable will be obsoleted in the future release.
Key to enable `wgrep-mode'."
  :group 'wgrep
  :type 'string)

(defcustom wgrep-auto-save-buffer nil
  "Non-nil means do `save-buffer' automatically while `wgrep-finish-edit'."
  :group 'wgrep
  :type 'boolean)

(defvar wgrep-setup-hook nil
  "Hooks to run when setting up wgrep.")

(defface wgrep-face
  '((((class color)
      (background dark))
     (:background "SlateGray1" :foreground "Black"))
    (((class color)
      (background light))
     (:background "ForestGreen" :foreground "white"))
    (t
     ()))
  "*Face used for the changed text in the grep buffer."
  :group 'wgrep)

(defface wgrep-delete-face
  '((((class color)
      (background dark))
     (:background "SlateGray1" :foreground "pink"))
    (((class color)
      (background light))
     (:background "ForestGreen" :foreground "pink"))
    (t
     ()))
  "*Face used for the deleted whole line in the grep buffer."
  :group 'wgrep)

(defface wgrep-file-face
  '((((class color)
      (background dark))
     (:background "gray30" :foreground "white"))
    (((class color)
      (background light))
     (:background "ForestGreen" :foreground "white"))
    (t
     ()))
  "*Face used for the changed text in the file buffer."
  :group 'wgrep)

(defface wgrep-reject-face
  '((((class color)
      (background dark))
     (:foreground "HotPink" :weight bold))
    (((class color)
      (background light))
     (:foreground "Red" :weight bold))
    (t
     ()))
  "*Face used for the line in the grep buffer that can not be applied to
a file."
  :group 'wgrep)

(defface wgrep-done-face
  '((((class color)
      (background dark))
     (:foreground "LightSkyBlue"))
    (((class color)
      (background light))
     (:foreground "Blue"))
    (t
     ()))
  "*Face used for the line in the grep buffer that can be applied to a file."
  :group 'wgrep)

(defvar wgrep-readonly-state nil)
(make-variable-buffer-local 'wgrep-readonly-state)

(defvar wgrep-prepared nil)
(make-variable-buffer-local 'wgrep-prepared)

(defvar wgrep-sibling-buffer nil)
(make-variable-buffer-local 'wgrep-sibling-buffer)

(defvar wgrep-original-mode-map nil)
(make-variable-buffer-local 'wgrep-original-mode-map)

;; Suppress elint warning
;; GNU Emacs have this variable at least version 21 or later
(defvar auto-coding-regexp-alist)

(defvar wgrep-acceptable-modes nil)
(make-obsolete 'wgrep-acceptable-modes nil "2.1.1")

(defconst wgrep-default-line-header-regexp
  ;; This regexp come from Emacs-25 grep.el
  ;; Capture subexp 2 is still exists for the backward compatibility.
  ;; But will be removed in future release.
  "^\\(.*?[^/\n]\\)\\(:[ \t]*\\)\\([1-9][0-9]*\\)[ \t]*:")

(defvar wgrep-line-file-regexp wgrep-default-line-header-regexp
  "Regexp that match to line header of grep result.

That capture 1: filename 3: line-number
End of this match equals start of file contents.
")
(defvar wgrep-results-parser 'wgrep-parse-command-results)
(defvar wgrep-header/footer-parser 'wgrep-prepare-header/footer)

(defvar wgrep-inhibit-modification-hook nil)

(defvar wgrep-mode-map nil)
(unless wgrep-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "\C-c\C-c" 'wgrep-finish-edit)
    (define-key map "\C-c\C-d" 'wgrep-mark-deletion)
    (define-key map "\C-c\C-e" 'wgrep-finish-edit)
    (define-key map "\C-c\C-p" 'wgrep-toggle-readonly-area)
    (define-key map "\C-c\C-r" 'wgrep-remove-change)
    (define-key map "\C-x\C-s" 'wgrep-finish-edit)
    (define-key map "\C-c\C-u" 'wgrep-remove-all-change)
    (define-key map "\C-c\C-[" 'wgrep-remove-all-change)
    (define-key map "\C-c\C-k" 'wgrep-abort-changes)
    (define-key map "\C-x\C-q" 'wgrep-exit)

    (setq wgrep-mode-map map)))

;;;###autoload
(defun wgrep-setup ()
  "Setup wgrep preparation."
  (wgrep-setup-internal))

(defun wgrep-setup-internal ()
  (setq wgrep-original-mode-map (current-local-map))
  (define-key wgrep-original-mode-map
    wgrep-enable-key 'wgrep-change-to-wgrep-mode)
  ;; delete previous wgrep overlays
  (wgrep-cleanup-overlays (point-min) (point-max))
  (remove-hook 'post-command-hook 'wgrep-maybe-echo-error-at-point t)
  (run-hooks 'wgrep-setup-hook))

(defun wgrep-maybe-echo-error-at-point ()
  (when (null (current-message))
    (let ((ov (catch 'found
                (dolist (o (overlays-in
                            (line-beginning-position) (line-end-position)))
                  (when (overlay-get o 'wgrep-reject-message)
                    (throw 'found o))))))
      (when ov
        (let (message-log-max)
          (message "%s" (overlay-get ov 'wgrep-reject-message)))))))

(defun wgrep-set-readonly-area (state)
  (let ((inhibit-read-only t)
        (wgrep-inhibit-modification-hook t)
        pos start end)
    (save-excursion
      ;; set readonly grep result filename
      (setq pos (point-min))
      (while (setq start (next-single-property-change
                          pos 'wgrep-line-filename))
        (setq end (next-single-property-change
                   start 'wgrep-line-filename))
        (put-text-property start end 'read-only state)
        (put-text-property (1- end) end 'rear-nonsticky t)
        ;; set readonly all newline at end of grep line
        (when (eq (char-before start) ?\n)
          (put-text-property (1- start) start 'read-only state))
        (setq pos end))
      (setq pos (point-min))
      (while (setq start (next-single-property-change
                          pos 'wgrep-ignore))
        (setq end (next-single-property-change
                   start 'wgrep-ignore))
        (put-text-property start end 'read-only state)
        ;; set readonly all newline at end of grep line
        (when (eq (char-before start) ?\n)
          (put-text-property (1- start) start 'read-only state))
        (setq pos end))
      ;; set readonly last of grep line
      (let ((footer (or (next-single-property-change (point-min) 'wgrep-footer)
                        ;; to consider empty footer.
                        (point-max))))
        (when (eq (char-before footer) ?\n)
          (put-text-property (1- footer) footer 'read-only state))))
    (setq wgrep-readonly-state state)))

(defun wgrep-after-change-function (beg end leng-before)
  (cond
   (wgrep-inhibit-modification-hook nil)
   ((= (point-min) (point-max))
    ;; cleanup when first executing
    (wgrep-cleanup-overlays (point-min) (point-max)))
   (t
    (wgrep-put-change-face beg end))))

(put 'wgrep-error 'error-conditions '(wgrep-error error))
(put 'wgrep-error 'error-message "wgrep error")

(defun wgrep-get-file-buffer (file)
  (unless (file-exists-p file)
    (signal 'wgrep-error (list "File does not exist.")))
  (unless (file-writable-p file)
    (signal 'wgrep-error (list "File is not writable.")))
  (or (get-file-buffer file)
      (find-file-noselect file)))

(defun wgrep-check-buffer ()
  "Check the file's status. If it is possible to change the file, return t"
  (when (and (not wgrep-change-readonly-file)
             buffer-read-only)
    (signal 'wgrep-error
            (list (format "Buffer \"%s\" is read-only." (buffer-name))))))

(defun wgrep-display-physical-data ()
  (cond
   ;; `funcall' is a trick to suppress the elint warnings.
   ((derived-mode-p 'image-mode)
    ;; toggle to raw data if buffer has image.
    (when (image-get-display-property)
      (image-mode-as-text)))
   (t nil)))

(defun wgrep-let-destructive-overlay (ov)
  (dolist (prop '(modification-hooks insert-in-front-hooks insert-behind-hooks))
    (overlay-put
     ov prop
     `((lambda (ov after-p &rest ignore)
         (when after-p
           (delete-overlay ov)))))))

(defun wgrep-after-save-hook ()
  (remove-hook 'after-save-hook 'wgrep-after-save-hook t)
  (dolist (ov (wgrep-file-overlays))
    (delete-overlay ov)))

(defun wgrep-file-overlays ()
  (save-restriction
    (widen)
    (let (res)
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (overlay-get ov 'wgrep)
          (setq res (cons ov res))))
      (nreverse res))))

(defun wgrep-replace-to-new-line (new-text)
  ;; delete grep extracted region (restricted to a line)
  (delete-region (line-beginning-position) (line-end-position))
  (let ((beg (point))
        end)
    (insert new-text)
    (let* ((end (point))
           ;; hilight the changed line
           (ov (wgrep-put-overlay-to-file-buffer beg end)))
      ;; make overlay volatile.
      (wgrep-let-destructive-overlay ov))))

(defun wgrep-flush-whole-line ()
  (wgrep-put-overlay-to-file-buffer
   (line-beginning-position) (line-end-position))
  (wgrep-delete-whole-line))

;;Hack function
(defun wgrep-string-replace-bom (string cs)
  (let ((regexp (car (rassq (coding-system-base cs) auto-coding-regexp-alist)))
        ;;TODO check ack-grep
        ;; FIXME: `find-operation-coding-system' is not exactly correct.
        ;;        However almost case is ok like this bom function.
        ;;        e.g. (let ((default-process-coding-system 'some-coding))
        ;;               (call-interactively 'grep))
        (grep-cs (or (find-operation-coding-system 'call-process grep-program)
                     (terminal-coding-system)))
        str)
    (if (and regexp
             (setq str (encode-coding-string string grep-cs))
             (string-match regexp str))
        (decode-coding-string (substring str (match-end 0)) cs)
      string)))

(defun wgrep-put-overlay-to-file-buffer (beg end)
  "*Highlight the changes in the file"
  (let ((ov
         (catch 'done
           (dolist (o (overlays-in beg end))
             (when (overlay-get o 'wgrep)
               (move-overlay o beg end)
               (throw 'done o)))
           (wgrep-make-overlay beg end))))
    (overlay-put ov 'face 'wgrep-file-face)
    (overlay-put ov 'priority 0)
    (add-hook 'after-save-hook 'wgrep-after-save-hook nil t)
    ov))

(defun wgrep-put-done-result (ov)
  (wgrep-set-result ov 'wgrep-done-face))

(defun wgrep-put-reject-result (ov error-data)
  (let ((message (mapconcat (lambda (x) (format "%s" x)) error-data " ")))
    (wgrep-set-result ov 'wgrep-reject-face message)))

(defun wgrep-set-result (ov face &optional message)
  (overlay-put ov 'face face)
  (overlay-put ov 'priority 1)
  (overlay-put ov 'wgrep-reject-message message))

(defun wgrep-edit-overlays ()
  (let (res)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'wgrep-changed)
        (setq res (cons ov res))))
    (nreverse res)))

(defun wgrep-put-change-face (beg end)
  (save-excursion
    ;; `looking-at' may destroy match data while replace by regexp.
    (save-match-data
      (let ((ov (wgrep-editing-overlay beg end)))
        ;; delete overlay if text is same as old value.
        (cond
         ;; not a valid point
         ((null ov))
         ((string= (overlay-get ov 'wgrep-old-text)
                   (overlay-get ov 'wgrep-edit-text))
          ;; back to unchanged
          (delete-overlay ov))
         (t
          (overlay-put ov 'face 'wgrep-face)))))))

;; get overlay BEG and END is passed by `after-change-functions'
(defun wgrep-editing-overlay (&optional start end)
  (let ((beg (or start (line-beginning-position)))
        (fin (or end (line-end-position)))
        ov bol eol
        ;; beginning/end of grep
        bog eog)
    (goto-char beg)
    (setq bol (line-beginning-position))
    (goto-char fin)
    (setq eol (line-end-position))
    (catch 'done
      (dolist (o (overlays-in bol eol))
        ;; find overlay that have changed by user.
        (when (overlay-get o 'wgrep-changed)
          (setq ov o)
          (throw 'done o))))
    (if ov
        (setq bog (min beg (overlay-start ov))
              eog (max (overlay-end ov) fin))
      (setq bog bol
            eog eol))
    (goto-char bog)
    (cond
     ;; When handling whole line, BOL equal beginning of edit.
     ((and (null ov) start (= bog start)))
     ((get-text-property (point) 'wgrep-line-filename)
      (let* ((header-end
              (next-single-property-change (point) 'wgrep-line-filename nil eol))
             (filename (get-text-property (point) 'wgrep-line-filename))
             (linum (get-text-property (point) 'wgrep-line-number))
             (value (buffer-substring-no-properties header-end eog))
             contents-begin)
        (goto-char header-end)
        (setq contents-begin (point-marker))
        ;; create editing overlay
        (cond
         ((null ov)
          (let ((old (wgrep-get-old-text filename linum)))
            (setq ov (wgrep-make-overlay bog eog))
            (overlay-put ov 'wgrep-contents-begin contents-begin)
            (overlay-put ov 'wgrep-filename filename)
            (overlay-put ov 'wgrep-linum linum)
            (overlay-put ov 'wgrep-changed t)
            (overlay-put ov 'priority 0)
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'wgrep-old-text old)))
         (t
          (move-overlay ov bog eog)))
        (overlay-put ov 'wgrep-edit-text value))))
    ov))

(defun wgrep-to-original-mode ()
  (kill-local-variable 'query-replace-skip-read-only)
  (remove-hook 'after-change-functions 'wgrep-after-change-function t)
  ;; do not remove `wgrep-maybe-echo-error-at-point' that display
  ;; errors at point
  (use-local-map wgrep-original-mode-map)
  (set-buffer-modified-p nil)
  (setq buffer-undo-list nil)
  (setq buffer-read-only t))

(defun wgrep-finish-edit ()
  "Apply changes to file buffers.
These changes are not immediately saved to disk unless
`wgrep-auto-save-buffer' is non-nil."
  (interactive)
  (let ((all-tran (wgrep-compute-transaction))
        done)
    (dolist (buf-tran all-tran)
      (let ((commited (wgrep-commit-buffer (car buf-tran) (cdr buf-tran))))
        (setq done (append done commited))))
    (wgrep-cleanup-temp-buffer)
    (wgrep-to-original-mode)
    (let ((msg (format "(%d changed)" (length done)))
          (ovs (wgrep-edit-overlays)))
      (cond
       ((null ovs)
        (if (= (length done) 0)
            (message "(No changes to be performed)")
          (message "Successfully finished. %s" msg)))
       ((= (length ovs) 1)
        (message "There is an unapplied change. %s" msg))
       (t
        (message "There are %d unapplied changes. %s"
                 (length ovs) msg))))))

(defun wgrep-exit ()
  "Return to original mode."
  (interactive)
  (if (and (buffer-modified-p)
           (y-or-n-p (format "Buffer %s modified; save changes? "
                             (current-buffer))))
      (wgrep-finish-edit)
    (wgrep-abort-changes)))

(defun wgrep-abort-changes ()
  "Discard all changes and return to original mode."
  (interactive)
  (wgrep-cleanup-overlays (point-min) (point-max))
  (wgrep-restore-from-temp-buffer)
  (wgrep-to-original-mode)
  (message "Changes discarded"))

(defun wgrep-remove-change (beg end)
  "Remove changes in the region between BEG and END."
  (interactive "r")
  (wgrep-cleanup-overlays beg end)
  (setq mark-active nil))

(defun wgrep-remove-all-change ()
  "Remove changes in the whole buffer."
  (interactive)
  (wgrep-cleanup-overlays (point-min) (point-max)))

(defun wgrep-toggle-readonly-area ()
  "Toggle read-only area to remove a whole line.

See the following example: you obviously don't want to edit the first line.
If grep matches a lot of lines, it's hard to edit the grep buffer.
After toggling to editable, you can call
`delete-matching-lines', `delete-non-matching-lines'.

Example:
----------------------------------------------
./.svn/text-base/some.el.svn-base:87:(hoge)
./some.el:87:(hoge)
----------------------------------------------
"
  (interactive)
  (let ((modified (buffer-modified-p))
        (read-only (not wgrep-readonly-state)))
    (wgrep-set-readonly-area read-only)
    (wgrep-set-header/footer-read-only read-only)
    (set-buffer-modified-p modified)
    (if wgrep-readonly-state
        (message "Removing the whole line is now disabled.")
      (message "Removing the whole line is now enabled."))))

(defun wgrep-change-to-wgrep-mode ()
  "Change to wgrep mode.

When the *grep* buffer is huge, this might freeze your Emacs
for several minutes.
"
  (interactive)
  (unless (wgrep-process-exited-p)
    (error "Active process working"))
  (wgrep-prepare-to-edit)
  (wgrep-set-readonly-area t)
  (set (make-local-variable 'query-replace-skip-read-only) t)
  (add-hook 'after-change-functions 'wgrep-after-change-function nil t)
  (add-hook 'post-command-hook 'wgrep-maybe-echo-error-at-point nil t)
  (use-local-map wgrep-mode-map)
  (buffer-disable-undo)
  (wgrep-clone-to-temp-buffer)
  (setq buffer-read-only nil)
  (buffer-enable-undo)
  ;; restore modified status
  (set-buffer-modified-p (wgrep-edit-overlays))
  (setq buffer-undo-list nil)
  (message "%s" (substitute-command-keys
                 "Press \\[wgrep-finish-edit] when finished \
or \\[wgrep-abort-changes] to abort changes.")))

(defun wgrep-save-all-buffers ()
  "Save the buffers that wgrep changed."
  (interactive)
  (let ((count 0))
    (dolist (b (buffer-list))
      (with-current-buffer b
        (let ((ovs (wgrep-file-overlays)))
        (when (and ovs (buffer-modified-p))
          (basic-save-buffer)
          (setq count (1+ count))))))
    (cond
     ((= count 0)
      (message "No buffer has been saved."))
     ((= count 1)
      (message "Buffer has been saved."))
     (t
      (message "%d buffers have been saved." count)))))

(defun wgrep-mark-deletion ()
  "Mark as delete to current line.
This change will be applied when \\[wgrep-finish-edit]."
  (interactive)
  (save-excursion
    (let ((ov (wgrep-editing-overlay)))
      (unless ov
        (error "Not a grep result"))
      (condition-case nil
          (progn
            (overlay-put ov 'wgrep-edit-text nil)
            (let ((wgrep-inhibit-modification-hook t)
                  (begin (overlay-get ov 'wgrep-contents-begin))
                  (end (overlay-end ov)))
              (delete-region begin end)
              (overlay-put ov 'face 'wgrep-delete-face)))
        (error
         (delete-overlay ov))))))

(defun wgrep-prepare-context ()
  (save-restriction
    (let ((start (wgrep-goto-first-found))
          (end (wgrep-goto-end-of-found)))
      (narrow-to-region start end)
      (goto-char (point-min))
      (funcall wgrep-results-parser))))

(defun wgrep-parse-command-results ()
  (let ((cache (make-hash-table)))
    (while (not (eobp))
      (cond
       ((looking-at wgrep-line-file-regexp)
        (let* ((fn (match-string-no-properties 1))
               (line (string-to-number (match-string 3)))
               (start (match-beginning 0))
               (end (match-end 0))
               (fstart (match-beginning 1))
               (fend (match-end 1))
               (lstart (match-beginning 3))
               (lend (match-end 3))
               (fprop (wgrep-construct-filename-property fn))
               (flen (length fn)))
          ;; check relative path grep result
          ;; grep result may be --context result with number between 2 colon.
          ;; ./filename-1-:10:
          ;; that make misunderstand font-locking
          ;; check file existence decrease risk of the misunderstanding.
          (when (or (gethash fn cache nil)
                    (and (file-exists-p fn)
                         (puthash fn t cache)))
            (put-text-property start end 'wgrep-line-filename fn)
            (put-text-property start end 'wgrep-line-number line)
            (put-text-property start (+ start flen) fprop fn)
            ;; handle backward and forward following options.
            ;; -A (--after-context) -B (--before-context) -C (--context)
            (save-excursion
              (wgrep-prepare-context-while fn line -1 fprop flen))
            (wgrep-prepare-context-while fn line 1 fprop flen)
            ;; end of context output `--'.
            (forward-line -1))))
       (t
        ;; Add property but this may be removed by `wgrep-prepare-context-while'
        (put-text-property
         (line-beginning-position) (line-end-position)
         'wgrep-ignore t)))
      (forward-line 1))))

(defun wgrep-delete-whole-line ()
  (delete-region (line-beginning-position) (line-beginning-position 2)))

(defun wgrep-goto-first-found ()
  (let ((header (previous-single-property-change (point-max) 'wgrep-header)))
    (cond
     (header
      (goto-char header)
      header)
     (t
      (goto-char (point-min))
      (point)))))

(defun wgrep-goto-end-of-found ()
  (let ((footer (next-single-property-change (point-min) 'wgrep-footer)))
    (cond
     (footer
      (goto-char footer)
      footer)
     (t
      (goto-char (point-max))
      (point-max)))))

(defun wgrep-goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

;; -A -B -C output may be misunderstood and set read-only.
;; Context match break font-lock if context have at least two `:'.
;; e.g.
;; filename-1-2010-01-01 23:59:99
;; filename:2:hoge
;; filename-3-20:10:25
(defun wgrep-prepare-context-while (filename line direction fprop flen)
  (let* ((next (+ direction line))
         (fregexp (regexp-quote filename)))
    (forward-line direction)
    (while (looking-at (format "^%s-%d-" fregexp next))
      (let ((start (match-beginning 0))
            (end (match-end 0))
            (bol (line-beginning-position))
            (eol (line-end-position)))
        (put-text-property start end 'wgrep-line-filename filename)
        (put-text-property start end 'wgrep-line-number next)
        (put-text-property start (+ start flen) fprop filename)
        (remove-text-properties bol eol '(wgrep-ignore))
        (forward-line direction)
        (setq next (+ direction next))))))

(defun wgrep-construct-filename-property (filename)
  (intern (format "wgrep-fn-%s" filename)))

(defun wgrep-process-exited-p ()
  (let ((proc (get-buffer-process (current-buffer))))
    (or (null proc)
        (eq (process-status proc) 'exit))))

(defun wgrep-prepare-to-edit ()
  (unless wgrep-prepared
    (save-excursion
      (let ((inhibit-read-only t)
            (wgrep-inhibit-modification-hook t)
            buffer-read-only)
        (funcall wgrep-header/footer-parser)
        (wgrep-prepare-context)
        (setq wgrep-prepared t)))))

(defun wgrep-prepare-header/footer ()
  (let (beg end)
    ;; Set read-only grep result header
    (goto-char (point-min))
    (setq beg (point-min))
    ;; See `compilation-start'
    (forward-line 4)
    (setq end (point))
    (put-text-property beg end 'read-only t)
    (put-text-property beg end 'wgrep-header t)
    ;; Set read-only grep result footer
    (goto-char (point-max))
    (forward-line -1)
    (when (re-search-backward "^$" end t)
      (setq beg (point))
      (setq end (point-max))
      (when beg
        (put-text-property beg end 'read-only t)
        (put-text-property beg end 'wgrep-footer t)))))

(defun wgrep-set-header/footer-read-only (state)
  (let ((inhibit-read-only t)
        (wgrep-inhibit-modification-hook t))
    ;; header
    (let ((header-end (next-single-property-change (point-min) 'wgrep-header)))
      (when header-end
        (put-text-property (point-min) header-end 'read-only state)))
    ;; footer
    (let ((footer-beg (next-single-property-change (point-min) 'wgrep-footer)))
      (when footer-beg
        (put-text-property footer-beg (point-max) 'read-only state)))))

(defun wgrep-cleanup-overlays (beg end)
  (dolist (ov (overlays-in beg end))
    (when (overlay-get ov 'wgrep)
      (delete-overlay ov))))

(defun wgrep-make-overlay (beg end)
  (let ((o (make-overlay beg end nil nil t)))
    (overlay-put o 'wgrep t)
    o))

(defun wgrep-clone-to-temp-buffer ()
  (wgrep-cleanup-temp-buffer)
  (let ((grepbuf (current-buffer))
        (tmpbuf (generate-new-buffer " *wgrep temp* ")))
    (setq wgrep-sibling-buffer tmpbuf)
    (add-hook 'kill-buffer-hook 'wgrep-cleanup-temp-buffer nil t)
    (append-to-buffer tmpbuf (point-min) (point-max))
    (with-current-buffer tmpbuf
      (setq wgrep-sibling-buffer grepbuf))
    tmpbuf))

(defun wgrep-restore-from-temp-buffer ()
  (cond
   ((and wgrep-sibling-buffer
         (buffer-live-p wgrep-sibling-buffer))
    (let ((grepbuf (current-buffer))
          (tmpbuf wgrep-sibling-buffer)
          (header (wgrep-current-file-and-linum))
          (savedc (current-column))
          (savedp (point))
          (inhibit-read-only t)
          (wgrep-inhibit-modification-hook t)
          buffer-read-only)
      (erase-buffer)
      (with-current-buffer tmpbuf
        (append-to-buffer grepbuf (point-min) (point-max)))
      (goto-char (point-min))
      ;; restore previous cursor
      (or (and header
               (apply 'wgrep-goto-grep-line header)
               (move-to-column savedc))
          (goto-char (min (point-max) savedp)))
      (wgrep-cleanup-temp-buffer)))
   (t
    ;; non fatal error
    (message "Error! Saved buffer is unavailable."))))

(defun wgrep-cleanup-temp-buffer ()
  "Cleanup temp buffer in *grep* buffer."
  (let ((origin-buffer (current-buffer)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq origin-buffer wgrep-sibling-buffer)
          (kill-buffer buf)))))
  (setq wgrep-sibling-buffer nil))

(defun wgrep-current-file-and-linum ()
  (save-excursion
    (forward-line 0)
    (let ((fn (get-text-property (point) 'wgrep-line-filename))
          (linum (get-text-property (point) 'wgrep-line-number)))
      (when (and fn linum)
        (list fn linum)))))

(defun wgrep-get-old-text (file number)
  (when (and wgrep-sibling-buffer
             (buffer-live-p wgrep-sibling-buffer))
    (with-current-buffer wgrep-sibling-buffer
      (when (wgrep-goto-grep-line file number)
        (buffer-substring-no-properties
         (point) (line-end-position))))))

(defun wgrep-goto-grep-line (file number)
  (let ((first (point))
        (fprop (wgrep-construct-filename-property file))
        fn next)
    (catch 'found
      ;; FIXME
      ;; In a huge buffer, `next-single-property-change' loop make
      ;; slow down the program.
      ;; 1. sketchy move by filename (wgrep-fn-* property).
      ;; 2. search filename and line-number in text property.
      ;; 3. return to 1. while search is done or EOB.

      (goto-char (point-min))

      (while (setq next (next-single-property-change (point) fprop))
        (goto-char next)
        (while (and (not (eobp))
                    (or (null (setq fn (get-text-property
                                        (line-beginning-position)
                                        'wgrep-line-filename)))
                        (string= fn file)))
          (when fn
            (let ((num (get-text-property (point) 'wgrep-line-number))
                  (start (next-single-property-change (point) 'wgrep-line-number)))
              (when (eq number num)
                (goto-char start)
                (throw 'found t))))
          (forward-line 1)))
      (goto-char first)
      nil)))

;; return alist like following
;; key ::= buffer
;; value ::= linum old-text new-text result-overlay edit-overlay
(defun wgrep-transaction-editing-list ()
  (let (res)
    (dolist (ov (wgrep-edit-overlays))
      (goto-char (overlay-start ov))
      (forward-line 0)
      (cond
       ;; ignore removed line or removed overlay
       ((eq (overlay-start ov) (overlay-end ov)))
       ((get-text-property (point) 'wgrep-line-filename)
        (let* ((name (get-text-property (point) 'wgrep-line-filename))
               (linum (get-text-property (point) 'wgrep-line-number))
               (start (next-single-property-change
                       (point) 'wgrep-line-filename nil (line-end-position)))
               (file (expand-file-name name default-directory))
               (file-error nil)
               (buffer (condition-case err
                           (wgrep-get-file-buffer file)
                         (wgrep-error
                          (setq file-error (cdr err))
                          nil)))
               (old (overlay-get ov 'wgrep-old-text))
               (new (overlay-get ov 'wgrep-edit-text))
               result)
          ;; wgrep-result overlay show the commiting of this editing
          (catch 'done
            (dolist (o (overlays-in (overlay-start ov) (overlay-end ov)))
              (when (overlay-get o 'wgrep-result)
                ;; get existing overlay
                (setq result o)
                (throw 'done t)))
            ;; create overlay to show result of committing
            (setq result (wgrep-make-overlay start (overlay-end ov)))
            (overlay-put result 'wgrep-result t))
          (if file-error
              (wgrep-put-reject-result result file-error)
            (setq res
                  (cons
                   (list buffer linum old new result ov)
                   res)))))))
    (nreverse res)))

(defun wgrep-compute-transaction ()
  (let ((edit-list (wgrep-transaction-editing-list))
        ;; key ::= buffer
        ;; value ::= edit [edit ...]
        ;; edit ::= linum old-text new-text result-overlay edit-overlay
        all-tran)
    (dolist (x edit-list)
      (let* ((buffer (car x))
             (edit (cdr x))
             (pair (assq buffer all-tran)))
        (unless pair
          (setq pair (cons buffer nil))
          (setq all-tran (cons pair all-tran)))
        ;; construct with current settings
        (setcdr pair (cons edit (cdr pair)))))
    ;; Convert linum to marker.
    ;; When new text contains newline destroy linum access.
    (dolist (buffer-tran all-tran)
      (let ((buffer (car buffer-tran))
            (edit-tran (cdr buffer-tran)))
        (with-current-buffer buffer
          (save-restriction
            (widen)
            (dolist (edit edit-tran)
              (let ((linum (car edit)))
                ;; get a marker
                (wgrep-goto-line linum)
                (setcar edit (point-marker))))))))
    all-tran))

(defun wgrep-commit-buffer (buffer tran)
  ;; Apply TRAN to BUFFER.
  ;; See `wgrep-compute-transaction'
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (wgrep-display-physical-data)
      (let ((inhibit-read-only wgrep-change-readonly-file)
            done)
        (dolist (info tran)
          (let ((marker (nth 0 info))
                (old (nth 1 info))
                (new (nth 2 info))
                (result (nth 3 info))
                (ov (nth 4 info)))
            (condition-case err
                (progn
                  ;; check the buffer while each of overlays
                  ;; to set a result message.
                  (wgrep-check-buffer)
                  (wgrep-apply-change marker old new)
                  (wgrep-put-done-result result)
                  (delete-overlay ov)
                  (setq done (cons ov done)))
              (error
               (wgrep-put-reject-result result (cdr err))))))
        (when (and wgrep-auto-save-buffer done)
          (basic-save-buffer))
        (nreverse done)))))

(defun wgrep-apply-change (marker old new)
  "The changes in the *grep* buffer are applied to the file.
NEW may be nil this means deleting whole line."
  (let ((coding buffer-file-coding-system))
    (goto-char marker)
    ;; check BOM
    (when (and (= (point-min-marker) marker)
               coding
               (coding-system-get coding :bom))
      (setq old (wgrep-string-replace-bom old coding))
      (when new
        (setq new (wgrep-string-replace-bom new coding))))
    ;; Check buffer line was modified after execute grep.
    (unless (string= old
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
      (signal 'wgrep-error (list "Buffer was changed after grep.")))
    (cond
     (new
      (wgrep-replace-to-new-line new))
     (t
      ;; new nil means flush whole line.
      (wgrep-flush-whole-line)))))

;;;
;;; activate/deactivate marmalade install or github install.
;;;

;;;###autoload
(add-hook 'grep-setup-hook 'wgrep-setup)

;; For `unload-feature'
(defun wgrep-unload-function ()
  (remove-hook 'grep-setup-hook 'wgrep-setup))

(provide 'wgrep)

;;; wgrep.el ends here
