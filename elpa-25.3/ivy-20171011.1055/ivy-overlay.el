;;; ivy-overlay.el --- Overlay display functions for Ivy  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2017  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
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
;;
;; This package allows to setup Ivy's completion at point to actually
;; show the candidates and the input at point, instead of in the
;; minibuffer.

;;; Code:
(defface ivy-cursor
  '((t (:background "black"
        :foreground "white")))
  "Cursor face for inline completion."
  :group 'ivy-faces)

(defvar ivy--old-cursor-type t)

(defvar ivy-overlay-at nil
  "Overlay variable for `ivy-display-function-overlay'.")

(defun ivy-left-pad (str width)
  "Pad STR from left with WIDTH spaces."
  (let ((padding (make-string width ?\ )))
    (mapconcat (lambda (x)
                 (setq x (concat padding x))
                 (if (> (length x) (window-width))
                     (concat
                      (substring x 0 (- (window-width) 4))
                      "...")
                   x))
               (split-string str "\n")
               "\n")))

(declare-function company-abort "ext:company")

(defun ivy-overlay-cleanup ()
  "Clean up after `ivy-display-function-overlay'."
  (when (overlayp ivy-overlay-at)
    (delete-overlay ivy-overlay-at)
    (setq ivy-overlay-at nil))
  (unless cursor-type
    (setq cursor-type ivy--old-cursor-type))
  (when (fboundp 'company-abort)
    (company-abort)))

(defun ivy-overlay-show-after (str)
  "Display STR in an overlay at point.

First, fill each line of STR with spaces to the current column.
Then attach the overlay the character before point."
  (if ivy-overlay-at
      (progn
        (move-overlay ivy-overlay-at (1- (point)) (line-end-position))
        (overlay-put ivy-overlay-at 'invisible nil))
    (setq ivy-overlay-at (make-overlay (1- (point)) (line-end-position)))
    (overlay-put ivy-overlay-at 'priority 9999))
  (overlay-put ivy-overlay-at 'display str)
  (overlay-put ivy-overlay-at 'after-string ""))

(declare-function org-current-level "org")
(defvar org-indent-indentation-per-level)
(defvar ivy-last)
(defvar ivy-text)
(defvar ivy-completion-beg)
(declare-function ivy--get-window "ivy")

(defun ivy-display-function-overlay (str)
  "Called from the minibuffer, display STR in an overlay in Ivy window.
Hide the minibuffer contents and cursor."
  (if (save-selected-window
        (select-window (ivy-state-window ivy-last))
        (or
         (< (- (window-width) (current-column))
            (length (ivy-state-current ivy-last)))
         (<= (window-height) (+ ivy-height 2))))
      (let ((buffer-undo-list t))
        (save-excursion
          (forward-line 1)
          (insert str)))
    (add-face-text-property (minibuffer-prompt-end) (point-max)
                            '(:foreground "white"))
    (let ((cursor-pos (1+ (- (point) (minibuffer-prompt-end))))
          (ivy-window (ivy--get-window ivy-last)))
      (setq cursor-type nil)
      (with-selected-window ivy-window
        (when cursor-type
          (setq ivy--old-cursor-type cursor-type))
        (setq cursor-type nil)
        (let ((overlay-str
               (concat
                (buffer-substring (max 1 (1- (point))) (point))
                ivy-text
                (if (eolp)
                    " "
                  "")
                (buffer-substring (point) (line-end-position))
                (ivy-left-pad
                 str
                 (+ (if (eq major-mode 'org-mode)
                        (* org-indent-indentation-per-level (org-current-level))
                      0)
                    (save-excursion
                      (goto-char ivy-completion-beg)
                      (current-column)))))))
          (add-face-text-property cursor-pos (1+ cursor-pos)
                                  'ivy-cursor t overlay-str)
          (ivy-overlay-show-after overlay-str))))))

(provide 'ivy-overlay)
;;; ivy-overlay.el ends here
