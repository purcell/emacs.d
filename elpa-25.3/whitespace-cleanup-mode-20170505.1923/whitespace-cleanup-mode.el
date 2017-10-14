;;; whitespace-cleanup-mode.el --- Intelligently call whitespace-cleanup on save

;; Copyright (C) 2013-2015 Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Package-Version: 20170505.1923
;; Package-X-Original-Version: 0
;; URL: https://github.com/purcell/whitespace-cleanup-mode
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `whitespace-cleanup' is a handy function, but putting it in
;; `before-save-hook' for every buffer is overkill, and causes messy diffs
;; when editing code that did not initially have clean whitespace.

;; Additionally, whitespace preferences are often project-specific, and
;; it's inconvenient to set up `before-save-hook' in a ".dir-locals.el" file.

;; `whitespace-cleanup-mode' is a minor mode which calls `whitespace-cleanup'
;; before saving the current buffer, but only if the whitespace in the buffer
;; was initially clean.

;; Set `whitespace-cleanup-mode' to t in ".dir-locals.el" to enable the mode
;; project-wide, or add it to the hook for the major mode(s) of your choice.

;; To clean up whitespace everywhere by default, enable
;; `global-whitespace-cleanup-mode'.

;; To clean up whitespace at save even if it was intitially dirty,
;; unset `whitespace-cleanup-mode-only-if-initially-clean'.

;; This mode is built upon some functionality provided by `whitespace-mode', namely
;; `whitespace-action': if you would rather see a warning when saving a file with
;; bogus whitespace, or even have the save aborted, then set that variable.

;;; Code:

(require 'whitespace)

;;;###autoload
(defgroup whitespace-cleanup nil
  "Automatically clean up whitespace on save."
  :group 'convenience)

(defcustom whitespace-cleanup-mode-preserve-point nil
  "When non-nil, the column position of point will be restored after cleanup.
When the point is positioned after trailing indentation at
save-time, the normal behaviour is that the point will jump back
to the end of the line's trimmed content.  When this variable is
non-nil, then the trimmed space is re-added after save, but
without marking the buffer as modified.  This allows
uninterrupted editing with very short autosave intervals."
  :group 'whitespace-cleanup)

(defcustom whitespace-cleanup-mode-only-if-initially-clean t
  "When non-nil, only clean up whitespace at save if it was clean initially.
The check for initial cleanliness is done when `whitespace-cleanup-mode' is
enabled."
  :group 'whitespace-cleanup)

(defcustom whitespace-cleanup-mode-ignore-modes
  '(special-mode view-mode comint-mode cider-repl-mode haskell-interactive-mode)
  "List of major modes in which cleanup will not be automatically enabled.
If the major mode of a buffer is derived from one of these, then
 `global-whitespace-cleanup-mode' will not enable `whitespace-cleanup-mode'
 in that buffer."
  :type '(repeat symbol)
  :group 'whitespace-cleanup)

(defvar whitespace-cleanup-mode-initially-clean nil
  "Records whether `whitespace-cleanup' was a no-op when the mode launched.")
(make-variable-buffer-local 'whitespace-cleanup-mode-initially-clean)

(defun whitespace-cleanup-mode-buffer-is-clean-p ()
  "Return t iff the whitespace in the current buffer is clean."
  (let ((contents (buffer-substring-no-properties (point-min) (point-max)))
        (orig-indent-tabs-mode indent-tabs-mode)
        (orig-whitespace-style whitespace-style))
    (with-temp-buffer
      (insert contents)
      (set-buffer-modified-p nil)
      (setq indent-tabs-mode orig-indent-tabs-mode
            whitespace-style orig-whitespace-style)
      (whitespace-cleanup)
      (not (buffer-modified-p)))))

(defun whitespace-cleanup-mode-mode-line ()
  "Return a string for mode-line.
Use '!' to signify that the buffer was not initially clean."
  (concat " WSC"
          (unless whitespace-cleanup-mode-initially-clean
            "!")))

;;;###autoload
(define-minor-mode whitespace-cleanup-mode
  "Automatically call `whitespace-cleanup' on save."
  nil
  (:eval (whitespace-cleanup-mode-mode-line))
  nil
  (if whitespace-cleanup-mode
      (progn
        (setq whitespace-cleanup-mode-initially-clean
              (whitespace-cleanup-mode-buffer-is-clean-p))
        (add-hook 'write-file-functions 'whitespace-cleanup-mode-write-file nil t))
    (remove-hook 'write-file-functions 'whitespace-cleanup-mode-write-file t)))

;;;###autoload
(put 'whitespace-cleanup-mode 'safe-local-variable 'booleanp)

;;;###autoload
(define-globalized-minor-mode global-whitespace-cleanup-mode
  whitespace-cleanup-mode
  turn-on-whitespace-cleanup-mode)

(defun turn-on-whitespace-cleanup-mode ()
  "Enable `whitespace-cleanup-mode' if appropriate in this buffer."
  (unless (or (minibufferp)
              (apply 'derived-mode-p whitespace-cleanup-mode-ignore-modes))
    (whitespace-cleanup-mode 1)))

(defun whitespace-cleanup-mode-write-file ()
  "Function added to `write-file-functions'."
  (when (and whitespace-cleanup-mode
             (not buffer-read-only)
             (or (not whitespace-cleanup-mode-only-if-initially-clean)
                 whitespace-cleanup-mode-initially-clean))
    (let ((whitespace-action (or whitespace-action '(auto-cleanup)))
          (col (current-column)))
      (whitespace-write-file-hook)
      (when whitespace-cleanup-mode-preserve-point
        (move-to-column col t)
        (set-buffer-modified-p nil)))))

(defadvice whitespace-cleanup-region (around whitespace-cleanup-mode-mark-clean (start end) activate)
  "When `whitespace-cleanup' is called, mark the buffer as initially clean."
  (let ((cleaning-up-whole-buffer (and (eq start (point-min))
                                       (eq end (point-max)))))
    ad-do-it
    (when cleaning-up-whole-buffer
      (setq whitespace-cleanup-mode-initially-clean t))))

(provide 'whitespace-cleanup-mode)
;;; whitespace-cleanup-mode.el ends here
