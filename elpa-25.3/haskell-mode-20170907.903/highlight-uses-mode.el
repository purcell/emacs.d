;;; highlight-uses-mode.el --- Mode for highlighting uses -*- lexical-binding: t -*-

;; Copyright (c) 2014 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)

(defvar highlight-uses-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'highlight-uses-mode-next)
    (define-key map (kbd "S-TAB") 'highlight-uses-mode-prev)
    (define-key map (kbd "<backtab>") 'highlight-uses-mode-prev)
    (define-key map (kbd "RET") 'highlight-uses-mode-stop-here)
    (define-key map (kbd "C-g") 'highlight-uses-mode)
    map)
  "Keymap for using `highlight-uses-mode'.")

(defvar-local highlight-uses-mode-point nil)

;;;###autoload
(define-minor-mode highlight-uses-mode
  "Minor mode for highlighting and jumping between uses."
  :lighter " Uses"
  :keymap highlight-uses-mode-map
  (if highlight-uses-mode
      (setq highlight-uses-mode-point (point))
    (when highlight-uses-mode-point
      (goto-char highlight-uses-mode-point)))
  (remove-overlays (point-min) (point-max) 'highlight-uses-mode-highlight t))

(defun highlight-uses-mode-replace ()
  "Replace all highlighted instances in the buffer with something
  else."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((o (highlight-uses-mode-next)))
      (when o
        (let ((replacement (read-from-minibuffer (format "Replace uses %s with: "
                                                         (buffer-substring
                                                          (overlay-start o)
                                                          (overlay-end o))))))

          (while o
            (goto-char (overlay-start o))
            (delete-region (overlay-start o)
                           (overlay-end o))
            (insert replacement)
            (setq o (highlight-uses-mode-next))))))))

(defun highlight-uses-mode-stop-here ()
  "Stop at this point."
  (interactive)
  (setq highlight-uses-mode-point (point))
  (highlight-uses-mode -1))

(defun highlight-uses-mode-next ()
  "Jump to next result."
  (interactive)
  (let ((os (sort (cl-remove-if (lambda (o)
                                  (or (<= (overlay-start o) (point))
                                      (not (overlay-get o 'highlight-uses-mode-highlight))))
                                (overlays-in (point) (point-max)))
                  (lambda (a b)
                    (< (overlay-start a)
                       (overlay-start b))))))
    (when os
      (goto-char (overlay-start (car os)))
      (car os))))

(defun highlight-uses-mode-prev ()
  "Jump to previous result."
  (interactive)
  (let ((os (sort (cl-remove-if (lambda (o)
                                  (or (>= (overlay-end o) (point))
                                      (not (overlay-get o 'highlight-uses-mode-highlight))))
                                (overlays-in (point-min) (point)))
                  (lambda (a b)
                    (> (overlay-start a)
                       (overlay-start b))))))
    (when os
      (goto-char (overlay-start (car os)))
      (car os))))

(defun highlight-uses-mode-highlight (start end)
  "Make a highlight overlay at the given span."
  (let ((o (make-overlay start end)))
    (overlay-put o 'priority 999)
    (overlay-put o 'face 'isearch)
    (overlay-put o 'highlight-uses-mode-highlight t)))

(provide 'highlight-uses-mode)
