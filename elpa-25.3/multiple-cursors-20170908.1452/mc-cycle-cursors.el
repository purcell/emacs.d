;;; mc-cycle-cursors.el

;; Copyright (C) 2012-2016 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: editing cursors

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

;; This scrolls the buffer to center each cursor in turn.
;; Scroll down with C-v, scroll up with M-v
;; This is nice when you have cursors that's outside of your view.

;;; Code:

(require 'multiple-cursors-core)

(defun mc/next-fake-cursor-after-point ()
  (let ((pos (point))
        (next-pos (1+ (point-max)))
        next)
    (mc/for-each-fake-cursor
     (let ((cursor-pos (overlay-get cursor 'point)))
       (when (and (< pos cursor-pos)
                  (< cursor-pos next-pos))
         (setq next-pos cursor-pos)
         (setq next cursor))))
    next))

(defun mc/prev-fake-cursor-before-point ()
  (let ((pos (point))
        (prev-pos (1- (point-min)))
        prev)
    (mc/for-each-fake-cursor
     (let ((cursor-pos (overlay-get cursor 'point)))
       (when (and (> pos cursor-pos)
                  (> cursor-pos prev-pos))
         (setq prev-pos cursor-pos)
         (setq prev cursor))))
    prev))

(defcustom mc/cycle-looping-behaviour 'continue
  "What to do if asked to cycle beyond the last cursor or before the first cursor."
  :type '(radio (const :tag "Loop around to beginning/end of document." continue)
                (const :tag "Warn and then loop around." warn)
                (const :tag "Signal an error." error)
                (const :tag "Don't loop." stop))
  :group 'multiple-cursors)

(defun mc/handle-loop-condition (error-message)
  (cl-ecase mc/cycle-looping-behaviour
    (error (error error-message))
    (warn  (message error-message))
    (continue 'continue)
    (stop 'stop)))

(defun mc/first-fake-cursor-after (point)
  "Very similar to mc/furthest-cursor-before-point, but ignores (mark) and (point)."
  (let* ((cursors (mc/all-fake-cursors))
         (cursors-after-point (cl-remove-if (lambda (cursor)
                                              (< (mc/cursor-beg cursor) point))
                                            cursors))
         (cursors-in-order (cl-sort cursors-after-point '< :key 'mc/cursor-beg)))
    (car cursors-in-order)))

(defun mc/last-fake-cursor-before (point)
  "Very similar to mc/furthest-cursor-before-point, but ignores (mark) and (point)."
  (let* ((cursors (mc/all-fake-cursors))
         (cursors-before-point (cl-remove-if (lambda (cursor)
                                               (> (mc/cursor-end cursor) point))
                                             cursors))
         (cursors-in-order (cl-sort cursors-before-point '> :key 'mc/cursor-end)))
    (car cursors-in-order)))

(cl-defun mc/cycle (next-cursor fallback-cursor loop-message)
  (when (null next-cursor)
    (when (eql 'stop (mc/handle-loop-condition loop-message))
      (return-from mc/cycle nil))
    (setf next-cursor fallback-cursor))
  (mc/create-fake-cursor-at-point)
  (mc/pop-state-from-overlay next-cursor)
  (recenter))

(defun mc/cycle-forward ()
  (interactive)
  (mc/cycle (mc/next-fake-cursor-after-point)
            (mc/first-fake-cursor-after (point-min))
             "We're already at the last cursor."))

(defun mc/cycle-backward ()
  (interactive)
  (mc/cycle (mc/prev-fake-cursor-before-point)
            (mc/last-fake-cursor-before (point-max))
            "We're already at the last cursor"))

(define-key mc/keymap (kbd "C-v") 'mc/cycle-forward)
(define-key mc/keymap (kbd "M-v") 'mc/cycle-backward)

(provide 'mc-cycle-cursors)


;; Local Variables:
;; coding: utf-8
;; End:

;;; mc-cycle-cursors.el ends here
