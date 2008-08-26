;;; rdebug-secondary.el --- Rdebug support windows.

;; Copyright (C) 2008 Rocky Bernstein (rocky@gnu.org)
;; Copyright (C) 2008 Anders Lindgren

;; $Id: rdebug-secondary.el 733 2008-02-29 04:34:44Z rockyb $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; See the manual and the file `rdebug.el' for more information.

;; Key bindings and menu for secondary buffers.

;;; Code:

(require 'rdebug)

(defun rdebug-populate-secondary-buffer-map (map)
  "Bind all common keys and menu used in the rdebug secondary buffers.
This includes the keys bound to `gud-key-prefix' (typically C-x
C-a)."
  (rdebug-populate-secondary-buffer-map-plain map)
  (rdebug-populate-common-keys map)
  (rdebug-populate-debugger-menu map)
  (let ((prefix-map (make-sparse-keymap)))
    (rdebug-populate-secondary-buffer-map-plain prefix-map)
    (define-key map gud-key-prefix prefix-map)))


(defun rdebug-display-cmd-buffer ()
  "Display the rdebug debugger command buffer."
  (interactive)
  (rdebug-display-secondary-buffer "cmd"))

(defun rdebug-pick-secondary-window-categorize (win name orig-win)
  "Return how suiteable this window is to display the a secondary buffer.
The higher score the better."
  (let ((buffer (window-buffer win)))
    (save-current-buffer
      (set-buffer buffer)
      (cond (rdebug-secondary-buffer
             (cond ((eq win orig-win)
                    ;; If the user issued the command inside a
                    ;; secondary window, use that window.
                    5)
                   ((and (member name '("variables" "watch"))
                         (memq major-mode '(rdebug-variables-mode
                                            rdebug-watch-mode)))
                    ;; Let "Watch" and "Variables" switch content.
                    4)
                   (t
                    ;; Any other secondary window.
                    3)))
            ((eq major-mode 'ruby-mode)
             ;; Avoid source windows.
             0)
            ((eq major-mode 'gud-mode)
             ;; Avoid the debugger shell window.
             1)
            (t
             ;; Just any other window.
             2)))))


(defun rdebug-display-secondary-buffer (name)
  "Display one of the rdebug secondary buffers.
If the buffer doesn't exist, do nothing. If the buffer is already
displayed, switch to it. Otherwise if the current buffer is a
secondary buffer, bury it replacing with the requested
buffer. Failing that, if there is secondary buffer visible, that
is replaced instead.  And finally failing all of the preceding,
we'll just pick a visible buffer to bury and replace."
  (let* ((buf-name (rdebug-get-secondary-buffer-name name))
         (buf (get-buffer buf-name))
         (orig-win (selected-window)))
    (if (null buf)
        (message "Buffer %s not found" buf-name)
      ;; Find a suitable window to display the buffer in.
      (let ((win (get-buffer-window buf (selected-frame))))
        (if win
            ;; Buffer already displayed, switch to it.
            (select-window win)
          ;;
          (let ((candidate nil)
                (candidate-score -1))
            (dolist (win (window-list (selected-frame)))
              (let ((score (rdebug-pick-secondary-window-categorize
                            win name orig-win)))
                (if (> score candidate-score)
                    (progn
                      (setq candidate       win)
                      (setq candidate-score score)))))
            (select-window candidate)))))
    (switch-to-buffer buf)))


;; Note: The generic `gud' framework contains special code to handle
;; this for GDB (see `gud-display-line') which we, unfortuately can't
;; use. Instead, we call `rdebug-pick-source-window' from
;; `gud-rdebug-marker-filter'. When gud becomes more generic we could
;; hopefully solve this in another way.
;;
;; The machanism is that `rdebug-pick-source-window' displays the
;; source file in the window of our choice, and gud kindly re-uses
;; that window.


(defun rdebug-display-source-window-categorize (win)
  "Return how suitable this window WIN is to display the source buffer.
The higher score the better."
  (let ((buffer (window-buffer win)))
    (cond ((eq buffer gud-comint-buffer)
           0)
          ((buffer-local-value 'rdebug-secondary-buffer buffer)
           1)
          ((eq (buffer-local-value 'major-mode buffer) 'ruby-mode)
           3)                           ; Pick me! Pick me!
          (t
           2))))

(defun rdebug-display-pick-source-window ()
  "Return the window that should get replaced by the source window."
  (rdebug-debug-enter "rdebug-display-pick-source-window"
    (let ((candidate nil)
          (candidate-score -1))
      (dolist (win (window-list (selected-frame)))
        (let ((score
               (rdebug-display-source-window-categorize win)))
          (if (> score candidate-score)
              (progn
                (setq candidate       win)
                (setq candidate-score score)))))
      candidate)))

(defun rdebug-frame-source-buffer (frame)
  "Return the buffer corresponding to the source file given in FRAME, or nil if none."
  (and frame
       gud-comint-buffer
       (save-current-buffer
         (set-buffer gud-comint-buffer)
         (gud-find-file (car frame)))))


(defun rdebug-current-source-buffer ()
  "Return the latest source buffer, or nil."
  (or (rdebug-frame-source-buffer gud-last-frame)
      (rdebug-frame-source-buffer gud-last-last-frame)))


(defun rdebug-display-source-buffer ()
  "Display the current source buffer."
  (interactive)
  (rdebug-debug-enter "rdebug-display-source-buffer"
    (let ((buffer (rdebug-current-source-buffer))
          (last-buffer (rdebug-frame-source-buffer gud-last-last-frame)))
      (if buffer
          (let ((window
                 (or
                  ;; Buffer is already visible, re-use the window.
                  (get-buffer-window buffer)
                  ;; Re-use the last window
                  (and last-buffer
                       (get-buffer-window last-buffer))
                  ;; Find a non-rdebug window.
                  (rdebug-display-pick-source-window))))
            (select-window window)
            (switch-to-buffer buffer))))))


(defun rdebug-pick-source-window ()
  "Display the source file, but do not switch window."
  (save-selected-window
    (rdebug-display-source-buffer)))


(defun rdebug-display-source-buffer-resync ()
  "Resync output and display the source buffer."
  (interactive)
  (call-interactively 'gud-source-resync)
  (rdebug-display-source-buffer))


(defun rdebug-delete-frame-or-window ()
  "Delete frame if there is only one window.  Otherwise delete the window."
  (interactive)
  (if (one-window-p) (delete-frame)
    (delete-window)))

(defun rdebug-goto-entry-try (str)
  "See if thre is an entry with number STR.  If not return nil."
  (goto-char (point-min))
  (if (re-search-forward (concat "^[^0-9]*\\(" str "\\)[^0-9]") nil t)
      (progn
        (goto-char (match-end 1))
        t)
    nil))


;; The following is split in two to facilitate debugging.
(defun rdebug-goto-entry-n-internal (keys)
  (if (and (stringp keys)
           (= (length keys) 1))
      (progn
        (setq rdebug-goto-entry-acc (concat rdebug-goto-entry-acc keys))
        ;; Try to find the longest suffix.
        (let ((acc rdebug-goto-entry-acc)
              (p (point)))
          (while (not (string= acc ""))
            (if (not (rdebug-goto-entry-try acc))
                (setq acc (substring acc 1))
              (setq p (point))
              ;; Break loop.
              (setq acc "")))
          (goto-char p)))
    (message "`rdebug-goto-entry-n' must be bound to a number key")))


(defun rdebug-goto-entry-n ()
  "Go to an entry number.

Breakpoints, Display expressions and Stack Frames all have
numbers associated with them which are distinct from line
numbers.  In a secondary buffer, this function is usually bound to
a numeric key which will position you at that entry number.  To
go to an entry above 9, just keep entering the number.  For
example, if you press 1 and then 9, you should jump to entry
1 (if it exists) and then 19 (if that exists).  Entering any
non-digit will start entry number from the beginning again."
  (interactive)
  (if (not (eq last-command 'rdebug-goto-entry-n))
      (setq rdebug-goto-entry-acc ""))
  (rdebug-goto-entry-n-internal (this-command-keys)))


;; -------------------------------------------------------------------
;; The end.
;;

(provide 'rdebug-secondary)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-secondary.el ends here
