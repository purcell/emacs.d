;;; rectangular-region-mode.el

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

;; (global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)

;; Think of this one as `set-mark` except you're marking a rectangular region. It is
;; an exceedingly quick way of adding multiple cursors to multiple lines.

;;; Code:

(require 'multiple-cursors-core)

(defvar rrm/anchor (make-marker)
  "The position in the buffer that anchors the rectangular region.")

(defvar rectangular-region-mode-map (make-sparse-keymap)
  "Keymap for rectangular region is mainly for rebinding C-g")

(define-key rectangular-region-mode-map (kbd "C-g") 'rrm/keyboard-quit)
(define-key rectangular-region-mode-map (kbd "<return>") 'rrm/switch-to-multiple-cursors)

(defvar rectangular-region-mode nil)

(defun rrm/keyboard-quit ()
  "Exit rectangular-region-mode."
  (interactive)
  (rectangular-region-mode 0)
  (rrm/remove-rectangular-region-overlays)
  (deactivate-mark))

;; Bind this to a key (for instance H-SPC) to start rectangular-region-mode
;;;###autoload
(defun set-rectangular-region-anchor ()
  "Anchors the rectangular region at point.

Think of this one as `set-mark' except you're marking a rectangular region. It is
an exceedingly quick way of adding multiple cursors to multiple lines."
  (interactive)
  (set-marker rrm/anchor (point))
  (push-mark (point))
  (rectangular-region-mode 1))

(defun rrm/remove-rectangular-region-overlays ()
  "Remove all rectangular-region overlays."
  (mc/remove-fake-cursors)
  (mapc #'(lambda (o)
            (when (eq (overlay-get o 'type) 'additional-region)
              (delete-overlay o)))
        (overlays-in (point-min) (point-max))))

(defun rrm/repaint ()
  "Start from the anchor and draw a rectangle between it and point."
  (if (not rectangular-region-mode)
      (remove-hook 'post-command-hook 'rrm/repaint t)
    ;; else
    (rrm/remove-rectangular-region-overlays)
    (let* ((annoying-arrows-mode nil)
           (point-column (current-column))
           (point-line (line-number-at-pos))
           (anchor-column (save-excursion (goto-char rrm/anchor) (current-column)))
           (anchor-line (save-excursion (goto-char rrm/anchor) (line-number-at-pos)))
           (left-column (if (< point-column anchor-column) point-column anchor-column))
           (right-column (if (> point-column anchor-column) point-column anchor-column))
           (navigation-step (if (< point-line anchor-line) 1 -1)))
      (move-to-column anchor-column)
      (set-mark (point))
      (move-to-column point-column)
      (mc/save-excursion
       (while (not (= anchor-line (line-number-at-pos)))
         (forward-line navigation-step)
         (move-to-column anchor-column)
         (when (= anchor-column (current-column))
           (set-mark (point))
           (move-to-column point-column)
           (when (= point-column (current-column))
             (mc/create-fake-cursor-at-point))))))))

(defun rrm/switch-to-multiple-cursors (&rest forms)
  "Switch from rectangular-region-mode to multiple-cursors-mode."
  (interactive)
  (rectangular-region-mode 0)
  (multiple-cursors-mode 1))

(defadvice er/expand-region (before switch-from-rrm-to-mc activate)
  (when rectangular-region-mode
    (rrm/switch-to-multiple-cursors)))

(defadvice kill-ring-save (before switch-from-rrm-to-mc activate)
  (when rectangular-region-mode
    (rrm/switch-to-multiple-cursors)))

;;;###autoload
(define-minor-mode rectangular-region-mode
  "A mode for creating a rectangular region to edit"
  nil " rr" rectangular-region-mode-map
  (if rectangular-region-mode
      (progn
        (add-hook 'after-change-functions 'rrm/switch-to-multiple-cursors t t)
        (add-hook 'post-command-hook 'rrm/repaint t t))
    (remove-hook 'after-change-functions 'rrm/switch-to-multiple-cursors t)
    (remove-hook 'post-command-hook 'rrm/repaint t)
    (set-marker rrm/anchor nil)))

(provide 'rectangular-region-mode)

;;; rectangular-region-mode.el ends here
