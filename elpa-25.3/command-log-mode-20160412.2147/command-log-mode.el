;;; command-log-mode.el --- log keyboard commands to buffer

;; homepage: https://github.com/lewang/command-log-mode

;; Copyright (C) 2013 Nic Ferrier
;; Copyright (C) 2012 Le Wang
;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Michael Weber <michaelw@foldr.org>
;; Keywords: help
;; Package-Version: 20160412.2147
;; Initial-version: <2004-10-07 11:41:28 michaelw>
;; Time-stamp: <2004-11-06 17:08:11 michaelw>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This add-on can be used to demo Emacs to an audience.  When
;; activated, keystrokes get logged into a designated buffer, along
;; with the command bound to them.

;; To enable, use e.g.:
;;
;; (require 'command-log-mode)
;; (add-hook 'LaTeX-mode-hook 'command-log-mode)
;;
;; To see the log buffer, call M-x clm/open-command-log-buffer.

;; The key strokes in the log are decorated with ISO9601 timestamps on
;; the property `:time' so if you want to convert the log for
;; screencasting purposes you could use the time stamp as a key into
;; the video beginning.

;;; Code:

(eval-when-compile (require 'cl))

(defvar clm/log-text t
  "A non-nil setting means text will be saved to the command log.")

(defvar clm/log-repeat nil
  "A nil setting means repetitions of the same command are merged into the single log line.")

(defvar clm/recent-history-string ""
  "This string will hold recently typed text.")

(defun clm/recent-history ()
  (setq clm/recent-history-string
	(concat clm/recent-history-string
		(buffer-substring-no-properties (- (point) 1) (point)))))

(add-hook 'post-self-insert-hook 'clm/recent-history)

(defun clm/zap-recent-history ()
  (unless (or (member this-original-command
		      clm/log-command-exceptions*)
	      (eq this-original-command #'self-insert-command))
    (setq clm/recent-history-string "")))

(add-hook 'post-command-hook 'clm/zap-recent-history)

(defvar clm/time-string "%Y-%m-%dT%H:%M:%S"
  "The string sent to `format-time-string' when command time is logged.")

(defvar clm/logging-dir "~/log/"
  "Directory in which to store files containing logged commands.")

(defvar clm/log-command-exceptions*
  '(nil self-insert-command backward-char forward-char
        delete-char delete-backward-char backward-delete-char
        backward-delete-char-untabify
        universal-argument universal-argument-other-key
        universal-argument-minus universal-argument-more
        beginning-of-line end-of-line recenter
        move-end-of-line move-beginning-of-line
        handle-switch-frame
        newline previous-line next-line)
  "A list commands which should not be logged, despite logging being enabled.
Frequently used non-interesting commands (like cursor movements) should be put here.")

(defvar clm/command-log-buffer nil
  "Reference of the currenly used buffer to display logged commands.")
(defvar clm/command-repetitions 0
  "Count of how often the last keyboard commands has been repeated.")
(defvar clm/last-keyboard-command nil
  "Last logged keyboard command.")


(defvar clm/log-command-indentation 11
  "*Indentation of commands in command log buffer.")

(defgroup command-log nil
  "Customization for the command log.")

(defcustom command-log-mode-auto-show nil
  "Show the command-log window or frame automatically."
  :group 'command-log
  :type 'boolean)

(defcustom command-log-mode-window-size 40
  "The size of the command-log window."
  :group 'command-log
  :type 'integer)

(defcustom command-log-mode-window-font-size 2
  "The font-size of the command-log window."
  :group 'command-log
  :type 'integer)

(defcustom command-log-mode-key-binding-open-log "C-c o"
  "The key binding used to toggle the log window."
  :group 'command-log
  :type '(radio
          (const :tag "No key" nil)
          (key-sequence "C-c o"))) ;; this is not right though it works for kbd

(defcustom command-log-mode-open-log-turns-on-mode nil
  "Does opening the command log turn on the mode?"
  :group 'command-log
  :type 'boolean)

(defcustom command-log-mode-is-global nil
  "Does turning on command-log-mode happen globally?"
  :group 'command-log
  :type 'boolean)

;;;###autoload
(define-minor-mode command-log-mode
  "Toggle keyboard command logging."
  :init-value nil
  :lighter " command-log"
  :keymap nil
  (if command-log-mode
      (when (and
             command-log-mode-auto-show
             (not (get-buffer-window clm/command-log-buffer)))
        (clm/open-command-log-buffer))
      ;; We can close the window though
      (clm/close-command-log-buffer)))

(define-global-minor-mode global-command-log-mode command-log-mode
  command-log-mode)

(defun clm/buffer-log-command-p (cmd &optional buffer)
  "Determines whether keyboard command CMD should be logged.
If non-nil, BUFFER specifies the buffer used to determine whether CMD should be logged.
If BUFFER is nil, the current buffer is assumed."
  (let ((val (if buffer
		 (buffer-local-value command-log-mode buffer)
	       command-log-mode)))
    (and (not (null val))
	 (null (member cmd clm/log-command-exceptions*)))))

(defmacro clm/save-command-environment (&rest body)
  (declare (indent 0))
  `(let ((deactivate-mark nil) ; do not deactivate mark in transient
                                        ; mark mode
	 ;; do not let random commands scribble over
	 ;; {THIS,LAST}-COMMAND
	 (this-command this-command)
	 (last-command last-command))
     ,@body))

(defun clm/open-command-log-buffer (&optional arg)
  "Opens (and creates, if non-existant) a buffer used for logging keyboard commands.
If ARG is Non-nil, the existing command log buffer is cleared."
  (interactive "P")
  (with-current-buffer 
      (setq clm/command-log-buffer
            (get-buffer-create " *command-log*"))
    (text-scale-set 1))
  (when arg
    (with-current-buffer clm/command-log-buffer
      (erase-buffer)))
  (let ((new-win (split-window-horizontally
                  (- 0 command-log-mode-window-size))))
    (set-window-buffer new-win clm/command-log-buffer)
    (set-window-dedicated-p new-win t)))

(defun clm/close-command-log-buffer ()
  "Close the command log window."
  (interactive)
  (with-current-buffer
      (setq clm/command-log-buffer
            (get-buffer-create " *command-log*"))
    (let ((win (get-buffer-window (current-buffer))))
      (when (windowp win)
        (delete-window win)))))

;;;###autoload
(defun clm/toggle-command-log-buffer (&optional arg)
  "Toggle the command log showing or not."
  (interactive "P")
  (when (and command-log-mode-open-log-turns-on-mode
             (not command-log-mode))
    (if command-log-mode-is-global
        (global-command-log-mode)
        (command-log-mode)))
  (with-current-buffer
      (setq clm/command-log-buffer
            (get-buffer-create " *command-log*"))
    (let ((win (get-buffer-window (current-buffer))))
      (if (windowp win)
          (clm/close-command-log-buffer)
          ;; Else open the window
          (clm/open-command-log-buffer arg)))))

(defun clm/scroll-buffer-window (buffer &optional move-fn)
  "Updates `point' of windows containing BUFFER according to MOVE-FN.
If non-nil, MOVE-FN is called on every window which displays BUFFER.
If nil, MOVE-FN defaults to scrolling to the bottom, making the last line visible.

Scrolling up can be accomplished with:
\(clm/scroll-buffer-window buf (lambda () (goto-char (point-min))))
"
  (let ((selected (selected-window))
	(point-mover (or move-fn
			 (function (lambda () (goto-char (point-max)))))))
    (walk-windows (function (lambda (window)
			      (when (eq (window-buffer window) buffer)
				(select-window window)
				(funcall point-mover)
				(select-window selected))))
		  nil t)))

(defmacro clm/with-command-log-buffer (&rest body)
  (declare (indent 0))
  `(when (and (not (null clm/command-log-buffer))
	      (buffer-name clm/command-log-buffer))
     (with-current-buffer clm/command-log-buffer
       ,@body)))

(defun clm/log-command (&optional cmd)
  "Hook into `pre-command-hook' to intercept command activation."
  (clm/save-command-environment
    (setq cmd (or cmd this-command))
    (when (clm/buffer-log-command-p cmd)
      (clm/with-command-log-buffer
        (let ((current (current-buffer)))
          (goto-char (point-max))
          (cond ((and (not clm/log-repeat) (eq cmd clm/last-keyboard-command))
                 (incf clm/command-repetitions)
                 (save-match-data
                   (when (and (> clm/command-repetitions 1)
                              (search-backward "[" (line-beginning-position -1) t))
                     (delete-region (point) (line-end-position))))
                 (backward-char) ; skip over either ?\newline or ?\space before ?\[
                 (insert " [")
                 (princ (1+ clm/command-repetitions) current)
                 (insert " times]"))
                (t ;; (message "last cmd: %s cur: %s" last-command cmd)
                 ;; showing accumulated text with interleaved key presses isn't very useful
		 (when (and clm/log-text (not clm/log-repeat))
		   (if (eq clm/last-keyboard-command 'self-insert-command)
		       (insert "[text: " clm/recent-history-string "]\n")))
                 (setq clm/command-repetitions 0)
                 (insert
                  (propertize
                   (key-description (this-command-keys))
                   :time  (format-time-string clm/time-string (current-time))))
                 (when (>= (current-column) clm/log-command-indentation)
                   (newline))
                 (move-to-column clm/log-command-indentation t)
                 (princ (if (byte-code-function-p cmd) "<bytecode>" cmd) current)
                 (newline)
                 (setq clm/last-keyboard-command cmd)))
          (clm/scroll-buffer-window current))))))

(defun clm/command-log-clear ()
  "Clear the command log buffer."
  (interactive)
  (with-current-buffer clm/command-log-buffer
    (erase-buffer)))

(defun clm/save-log-line (start end)
  "Helper function for `clm/save-command-log' to export text properties."
  (save-excursion
    (goto-char start)
    (let ((time (get-text-property (point) :time)))
      (if time
	  (list (cons start (if time 
				(concat "[" (get-text-property (point) :time) "] ")
			      "")))))))

(defun clm/save-command-log ()
  "Save commands to today's log.
Clears the command log buffer after saving."
  (interactive)
  (save-window-excursion
    (set-buffer (get-buffer " *command-log*"))
    (goto-char (point-min))
    (let ((now (format-time-string "%Y-%m-%d"))
	  (write-region-annotate-functions '(clm/save-log-line)))
      (while (and (re-search-forward "^.*" nil t)
		  (not (eobp)))
	(append-to-file (line-beginning-position) (1+ (line-end-position)) (concat clm/logging-dir now))))
    (clm/command-log-clear)))

(add-hook 'pre-command-hook 'clm/log-command)

(eval-after-load 'command-log-mode
  '(when command-log-mode-key-binding-open-log
    (global-set-key
     (kbd command-log-mode-key-binding-open-log)
     'clm/toggle-command-log-buffer)))

(provide 'command-log-mode)

;;; command-log-mode.el ends here
