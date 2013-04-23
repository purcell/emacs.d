;;; stripe-buffer.el --- Use a different background for even and odd lines

;; Copyright (C) 2008-2009  Andy Stewart
;; Copyright (C) 2012-2013  sabof

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: sabof <esabof@gmail.com>
;; URL: https://github.com/sabof/stripe-buffer
;; Package-Requires: ((cl-lib "1.0"))
;; Version: 0.2

;;; Commentary:

;; Use different background colors for even and odd lines.  With the
;; help of library `hl-line' yet another color can be used for the
;; current line.

;; The project is hosted at https://github.com/sabof/stripe-buffer

;;; License:

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'cl-lib)

(defgroup stripe-buffer nil
  "Use different background for even and odd lines."
  :group 'wp)

(defface stripe-highlight
    '((((class color) (background dark))
       (:background "#444444"))
      (((class color) (background light))
       (:background "#CCCCCC")))
  "Face for stripes."
  :group 'stripe-buffer)

(defface stripe-hl-line
    `((t
       :background ,(face-attribute 'default :foreground)
       :foreground ,(face-attribute 'default :background)))
  "Bold face for highlighting the current line in Hl-Line mode."
  :group 'stripe-buffer)

(defcustom stripe-height 1
  "Height of stripes."
  :group 'stripe-buffer
  :type 'integer)

(defcustom stripe-in-table-regex
  "^[ \t]*\\(?1:[|+].+[|+]\\) *$"
  "Regular expression that determines whether a line contains a table row.
Used by `stripe-table-mode' Only the first matching group will be painted."
  :group 'stripe-buffer
  :type 'string)

(defvar stripe-highlight-face 'stripe-highlight)
(defvar sb/overlays nil)
(defvar sb/is-listified nil)
(defvar sb/modified-flag nil)
(defvar sb/timer nil)
(mapc 'make-variable-buffer-local
      '(sb/is-listified
        sb/timer
        sb/modified-flag
        sb/overlays))

(defun sb/window-limits (&optional window)
  (save-excursion
    (let (( win-start (window-start window)))
      (goto-char (window-end window t))
      (unless (= (line-beginning-position) (point))
        (forward-line 1))
      (cons win-start (point)))))

(defun sb/buffer-visible-regions (&optional buffer-or-name)
  (mapcar 'sb/window-limits
          (get-buffer-window-list buffer-or-name nil t)))

(defun sb/compress-ranges (ranges)
  (let* (( dirty (cl-sort (cl-copy-list ranges)
                          '< :key 'car))
         clean)
    (while (cond
             ( (not dirty)
               nil)
             ( (null (cdr dirty))
               (push (pop dirty) clean)
               nil)
             ( (>= (cdr (car dirty))
                   (car (cadr dirty)))
               (setq dirty (cons (cons (car (car dirty))
                                       (cdr (cadr dirty)))
                                 (nthcdr 2 dirty))))
             ( t (push (pop dirty) clean))))
    (nreverse clean)))

(defun sb/buffer-visible-regions-compressed ()
  (sb/compress-ranges (sb/buffer-visible-regions)))

(defun sb/clear-stripes (&rest ignore)
  "Clear stripe overlays in current buffer."
  (mapc 'delete-overlay sb/overlays)
  (setq sb/overlays nil))

(defun sb/redraw-regions (regions available)
  (let* (( interval
           (* 2 stripe-height))
         ( get-overlay-create
           (lambda (start end)
             (let ((old-overlay (pop available)))
               (if old-overlay
                   (progn
                     (move-overlay old-overlay start end)
                     old-overlay)
                   (make-overlay start end)))))
         ( draw-stripe
           (lambda (height)
             ;; `region' available through dynamic binding
             (when (< (point) (cdr region))
               (let* (( stripe-region
                        (list (point)
                              (progn
                                (forward-line height)
                                (if (<= (point) (cdr region))
                                    (point)
                                    (progn
                                      (goto-char (cdr region))
                                      (point))))))
                      ( overlay (apply get-overlay-create stripe-region)))
                 (overlay-put overlay 'face stripe-highlight-face)
                 (overlay-put overlay 'is-stripe t)
                 (push overlay sb/overlays)))))
         ( goto-start-pos
           (lambda ()
             (let (( start-offset (mod (1- (line-number-at-pos)) interval)))
               (if (< start-offset stripe-height) ; in first part
                   (progn
                     (forward-line (- stripe-height start-offset))
                     (funcall draw-stripe stripe-height))
                   (funcall draw-stripe (- interval start-offset))
                   )))))
    (save-excursion
      (cl-dolist (region regions)
        (goto-char (car region))
        (funcall goto-start-pos)
        (while (< (point) (cdr region))
          (forward-line stripe-height)
          (funcall draw-stripe stripe-height)
          ))
      (mapc 'delete-overlay available))))

(defun sb/redraw-window (&optional window &rest ignore)
  (let* (( region (sb/window-limits window))
         ( old-overlays
           (cl-remove-if-not
            (lambda (ov) (overlay-get ov 'is-stripe))
            (overlays-in (car region) (cdr region)))))
    (setq sb/overlays
          (cl-set-difference sb/overlays
                             old-overlays))
    (sb/redraw-regions (list region) old-overlays)
    ))

(defun sb/redraw-all-windows (&rest ignore)
  (sb/redraw-regions (sb/buffer-visible-regions-compressed)
                     (prog1 sb/overlays
                       (setq sb/overlays nil))))

(defun sb/visible-table-ranges ()
  (let (( visible-ranges (sb/buffer-visible-regions-compressed))
        ranges)
    (cl-dolist (vr visible-ranges)
      (save-excursion
        (goto-char (car vr))
        (while (search-forward-regexp stripe-in-table-regex (cdr vr) t)
          (push (cons (match-beginning 1) (match-end 1)) ranges)
          )))
    (sb/compress-ranges ranges)))

(defun sb/redraw-all-tables (&rest ignore)
  (sb/redraw-regions (sb/visible-table-ranges)
                     (prog1 sb/overlays
                       (setq sb/overlays nil))))

(defun sb/add-hooks (hooks)
  (cl-dolist (hook hooks)
    (add-hook (car hook) (cdr hook) nil t)))

(defun sb/remove-hooks (hooks)
  (cl-dolist (hook hooks)
    (remove-hook (car hook) (cdr hook) t)))

;;; Interface

(defun sb/set-timer (redraw-func)
  (unless sb/timer
    (setq sb/timer
          (run-with-idle-timer
           0 nil `(lambda ()
                    (with-current-buffer ,(current-buffer)
                      (funcall ',redraw-func)
                      (setq sb/timer nil)))))))

(defun sb/cancel-timer ()
  (when sb/timer
    (cancel-timer sb/timer)
    (setq sb/timer nil)))

(define-minor-mode stripe-buffer-mode
    "Stripe buffer mode"
  nil nil nil
  (let* (( after-change
           (lambda (&rest ignore)
             (setq sb/modified-flag t)
             ;; For cases when a change is made by a timer, or a process filter
             (sb/set-timer 'sb/redraw-all-windows)))
         ( post-command
           (lambda (&rest ignore)
             (if sb/modified-flag
                 (progn
                   (sb/redraw-all-windows)
                   (sb/cancel-timer))
                 (sb/set-timer 'sb/redraw-all-windows))
             (setq sb/modified-flag nil)))
         ( hooks `((after-change-functions . ,after-change)
                   (post-command-hook . ,post-command)
                   (window-scroll-functions . sb/redraw-window)
                   (change-major-mode-hook . sb/clear-stripes)
                   (window-configuration-change-hook . sb/redraw-all-windows))))
    (if stripe-buffer-mode
        (progn
          (stripe-table-mode -1)
          (sb/add-hooks hooks)
          (sb/redraw-all-windows))
        (progn
          (sb/remove-hooks hooks)
          (sb/clear-stripes)
          ))))

(defun turn-on-stripe-buffer-mode ()
  "Turn on `stripe-buffer-mode'."
  (interactive)
  (stripe-buffer-mode 1))

(define-minor-mode stripe-table-mode
    "Stripe table mode"
  nil nil nil
  (let* (( after-change
           (lambda (&rest ignore)
             (setq sb/modified-flag t)
             (sb/set-timer 'sb/redraw-all-tables)))
         ( post-command
           (lambda (&rest ignore)
             (if sb/modified-flag
                 (progn
                   (sb/redraw-all-tables)
                   (sb/cancel-timer))
                 (sb/set-timer 'sb/redraw-all-tables))
             (setq sb/modified-flag nil)))
         ( hooks
           `((after-change-functions . ,after-change)
             (post-command-hook . ,post-command)
             (window-scroll-functions . sb/redraw-all-tables)
             (change-major-mode-hook . sb/clear-stripes)
             (window-configuration-change-hook . sb/redraw-all-tables))))
    (if stripe-table-mode
        (progn
          (stripe-buffer-mode -1)
          (sb/add-hooks hooks)
          (sb/redraw-all-tables))
        (progn
          (sb/remove-hooks hooks)
          (sb/clear-stripes)
          ))))

(defun turn-on-stripe-table-mode ()
  "Turn on `stripe-table-mode'."
  (interactive)
  (stripe-table-mode 1))

(defun org-table-stripes-enable ()
  "Backward compatibility"
  (interactive)
  (stripe-table-mode 1))

(defun stripe-listify-buffer ()
  "Turn on `stripe-buffer-mode' and `hl-line-mode'."
  (interactive)
  (setq sb/is-listified t)
  (setq cursor-type nil)
  (stripe-buffer-mode 1)
  (setq-local face-remapping-alist
              `((hl-line stripe-hl-line)))
  (hl-line-mode 1))

(defadvice hl-line-highlight (after stripe-set-priority activate)
  (when stripe-buffer-mode
    (overlay-put hl-line-overlay 'priority 10)))

(defun stripe-wdired-enable-cursor ()
  (when sb/is-listified
    (hl-line-mode -1)
    (setq cursor-type t)))

(add-hook 'wdired-mode-hook 'stripe-wdired-enable-cursor)

(defadvice wdired-finish-edit (before stripe-hide-cursor activate)
  (when sb/is-listified
    (hl-line-mode 1)
    (setq cursor-type nil)))

(provide 'stripe-buffer)
;; Local Variables:
;; indent-tabs-mode: nil
;; lisp-backquote-indentation: t
;; End:
;;; stripe-buffer.el ends here
