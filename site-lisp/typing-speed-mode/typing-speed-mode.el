;;; typing-speed.el --- Minor mode which displays your typing speed

;; Copyright (C) 2008 Wangdera Corporation

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; Author: Craig Andera <candera@wangdera.com>

;; Commentary: Invoke this minor mode to have your typing speed
;; continuously displayed in the mode line, in the format [75 WPM]
;; To use, just load this file and invoke (typing-speed-mode) or
;; (turn-on-typing-speed-mode)

(define-minor-mode typing-speed-mode
    "Displays your typing speed in the status bar."
  :lighter typing-speed-mode-text
  :group typing-speed
  (if typing-speed-mode
      (progn
    (add-hook 'post-command-hook 'typing-speed-post-command-hook)
    (setq typing-speed-event-queue '())
    (setq typing-speed-update-timer (run-with-timer 0 typing-speed-update-interval 'typing-speed-update)))
      (progn
    (remove-hook 'post-command-hook 'typing-speed-post-command-hook)
    (cancel-timer typing-speed-update-timer))))

(defcustom typing-speed-window 5
  "The window (in seconds) over which typing speed should be evaluated."
  :group 'typing-speed)

(defcustom typing-speed-mode-text-format " [%s WPM]"
  "A format string that controls how the typing speed is displayed in the mode line.
Must contain exactly one %s delimeter where the typing speed will be inserted."
  :group 'typing-speed)

(defcustom typing-speed-update-interval 1
  "How often the typing speed will update in the mode line, in seconds.
It will always also update after every command."
  :group 'typing-speed)

(defvar typing-speed-mode-text (format typing-speed-mode-text-format 0))
(defvar typing-speed-event-queue '())
(defvar typing-speed-update-timer nil)

(defun typing-speed-post-command-hook ()
  "When typing-speed-mode is enabled, fires after every command. If the
command is self-insert-command, log it as a keystroke and update the
typing speed."
  (if (eq this-command 'self-insert-command)
    (let ((current-time (float-time)))
      (push current-time typing-speed-event-queue)
      (typing-speed-update))))

(defun typing-speed-update ()
  "Calculate and display the typing speed."
  (let ((current-time (float-time)))
    (setq typing-speed-event-queue
      (typing-speed-remove-old-events
       (- current-time typing-speed-window)
       typing-speed-event-queue))
    (typing-speed-message-update)))

(defun typing-speed-message-update ()
  "Updates the status bar with the current typing speed"
  (let* ((chars-per-second (/ (length typing-speed-event-queue) (float typing-speed-window)))
     (chars-per-min (* chars-per-second 60))
     (words-per-min (/ chars-per-min 5)))
    (setq typing-speed-mode-text (format " [%s WPM]" (floor words-per-min)))
    (force-mode-line-update)))

(defun typing-speed-remove-old-events (threshold queue)
  "Removes events older than than the threshold (in seconds) from the specified queue"
  (if (or (null queue)
      (> threshold (car queue)))
      nil
      (cons (car queue)
        (typing-speed-remove-old-events threshold (cdr queue)))))

(defun turn-on-typing-speed ()
  "Turns on typing-speed-mode"
  (if (not typing-speed-mode)
      (typing-speed-mode)))

(defun turn-off-typing-speed ()
  "Turns off typing-speed-mode"
  (if typing-speed-mode
      (typing-speed-mode)))

(provide 'typing-speed-mode)