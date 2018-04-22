;;; visual-fill-column.el --- fill-column for visual-line-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2016 Joost Kremers
;; Copyright (C) 2016 Martin Rudalics
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 2015
;; Version: 1.9
;; Package-Version: 20170502.1300
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

;; visual-fill-column is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; visual-fill-column is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `visual-fill-column-mode' is a small Emacs minor mode that mimics the effect of `fill-column'
;; in `visual-line-mode'.  Instead of wrapping lines at the window edge, which
;; is the standard behaviour of `visual-line-mode', it wraps lines at
;; `fill-column'.  If `fill-column' is too large for the window, the text is
;; wrapped at the window edge.

;;; Code:

(defgroup visual-fill-column nil "Wrap lines according to `fill-column' in `visual-line-mode'."
  :group 'wp
  :prefix "visual-fill-column-")

(defcustom visual-fill-column-width nil
  "Width of the text area.
By default, the global value of `fill-column' is used, but if
this option is set to a value, it is used instead."
  :group 'visual-fill-column
  :type '(choice (const :tag "Use `fill-column'" :value nil)
                 (integer :tag "Specify width" :value 70)))
(make-variable-buffer-local 'visual-fill-column-width)
(put 'visual-fill-column-width 'safe-local-variable 'numberp)

(defcustom visual-fill-column-fringes-outside-margins t
  "Put the fringes outside the margins."
  :group 'visual-fill-column
  :type '(choice (const :tag "Put fringes outside the margins" t)
                 (const :tag "Keep the fringes inside the margins" nil)))
(make-variable-buffer-local 'visual-fill-column-fringes-outside-margins)
(put 'visual-fill-column-fringes-outside-margins 'safe-local-variable 'symbolp)

(defcustom visual-fill-column-center-text nil
  "If set, center the text area in the window."
  :group 'visual-fill-column
  :type '(choice (const :tag "Display text area at window margin" nil)
                 (const :tag "Center text area" t)))
(make-variable-buffer-local 'visual-fill-column-center-text)
(put 'visual-fill-column-center-text 'safe-local-variable 'symbolp)

;;;###autoload
(define-minor-mode visual-fill-column-mode
  "Wrap lines according to `fill-column' in `visual-line-mode'."
  :init-value nil :lighter nil :global nil
  (if visual-fill-column-mode
      (visual-fill-column-mode--enable)
    (visual-fill-column-mode--disable)))

;;;###autoload
(define-globalized-minor-mode global-visual-fill-column-mode visual-fill-column-mode turn-on-visual-fill-column-mode
  :require 'visual-fill-column-mode
  :group 'visual-fill-column)

(defun turn-on-visual-fill-column-mode ()
  "Turn on `visual-fill-column-mode'.
Note that `visual-fill-column-mode' is only turned on in buffers
in which `visual-line-mode' is active as well."
  (when visual-line-mode
    (visual-fill-column-mode 1)))

(defun visual-fill-column-mode--enable ()
  "Set up `visual-fill-column-mode' for the current buffer."
  (add-hook 'window-configuration-change-hook #'visual-fill-column--adjust-window 'append 'local)
  (visual-fill-column--adjust-window))

(defun visual-fill-column-mode--disable ()
  "Disable `visual-fill-column-mode' for the current buffer."
  (remove-hook 'window-configuration-change-hook #'visual-fill-column--adjust-window 'local)
  (set-window-fringes (get-buffer-window (current-buffer)) nil)
  (set-window-margins (get-buffer-window (current-buffer)) nil))

(defun visual-fill-column-split-window (&optional window size side pixelwise)
  "Split WINDOW, unsetting its margins first.
SIZE, SIDE, and PIXELWISE are passed on to `split-window'.  This
function is for use in the window parameter `split-window'."
  (let ((horizontal (memq side '(t left right)))
	margins new)
    (when horizontal
      ;; Reset margins.
      (setq margins (window-margins window))
      (set-window-margins window nil))
    ;; Now try to split the window.
    (set-window-parameter window 'split-window nil)
    (unwind-protect
	(setq new (split-window window size side pixelwise))
      (set-window-parameter window 'split-window #'visual-fill-column-split-window)
      ;; Restore old margins if we failed.
      (when (and horizontal (not new))
	(set-window-margins window (car margins) (cdr margins))))))

;;;###autoload
(defun visual-fill-column-split-window-sensibly (&optional window)
  "Split WINDOW sensibly, unsetting its margins first.
This function unsets the window margins and calls
`split-window-sensibly'.

By default, `split-window-sensibly' does not split a window
vertically if it has wide margins, even if there is enough space
for a vertical split.  This function can be used as the value of
`split-window-preferred-function' to enable vertically splitting
windows with wide margins."
  (let ((margins (window-margins window))
        new)
    ;; unset the margins and try to split the window
    (when (buffer-local-value 'visual-fill-column-mode (window-buffer window))
      (set-window-margins window nil))
    (unwind-protect
        (setq new (split-window-sensibly window))
      (when (not new)
        (set-window-margins window (car margins) (cdr margins))))))

(defun visual-fill-column--adjust-window ()
  "Adjust the window margins and fringes."
  ;; Only run when we're really looking at a buffer that has v-f-c-mode enabled. See #22.
  (when (buffer-local-value 'visual-fill-column-mode (window-buffer (selected-window)))
    (set-window-fringes (get-buffer-window (current-buffer)) nil nil visual-fill-column-fringes-outside-margins)
    (if (>= emacs-major-version 25)
        (set-window-parameter (get-buffer-window (current-buffer)) 'split-window #'visual-fill-column-split-window))
    (visual-fill-column--set-margins)))

(defun visual-fill-column-adjust (&optional _inc)
  "Adjust the window margins and fringes.
This function is for use as advice to `text-scale-adjust'.  It
calls `visual-fill-column--adjust-window', but only if
`visual-fill-column' is active."
  (if visual-fill-column-mode
      (visual-fill-column--adjust-window)))

(defun visual-fill-column--window-max-text-width (&optional window)
  "Return the maximum possible text width of WINDOW.
The maximum possible text width is the width of the current text
area plus the margins, but excluding the fringes, scroll bar and
right divider.  WINDOW defaults to the selected window.  The
return value is scaled to account for `text-scale-mode-amount'
and `text-scale-mode-step'."
  (or window (setq window (get-buffer-window (current-buffer))))
  (let* ((margins (window-margins window))
         (buffer (window-buffer window))
         (scale (if (and (boundp 'text-scale-mode-step)
                         (boundp 'text-scale-mode-amount))
                    (with-current-buffer buffer
                      (expt text-scale-mode-step
                            text-scale-mode-amount))
                  1.0)))
    (truncate (/ (+ (window-width window)
                    (or (car margins) 0)
                    (or (cdr margins) 0))
                 (float scale)))))

(defun visual-fill-column--set-margins ()
  "Set window margins for the current window."
  ;; calculate left & right margins
  (let* ((window (get-buffer-window (current-buffer)))
         (total-width (visual-fill-column--window-max-text-width window))
         (width (or visual-fill-column-width
                    fill-column))
         (margins (if (< (- total-width width) 0) ; margins must be >= 0
                      0
                    (- total-width width)))
         (left (if visual-fill-column-center-text
                   (/ margins 2)
                 0))
         (right (if visual-fill-column-center-text
                    (/ margins 2)
                  margins)))

    ;; put an explicitly R2L buffer on the right side of the window
    (when (and (eq bidi-paragraph-direction 'right-to-left)
               (= left 0))
      (setq left right)
      (setq right 0))

    (set-window-margins window left right)))

(provide 'visual-fill-column)

;;; visual-fill-column.el ends here
