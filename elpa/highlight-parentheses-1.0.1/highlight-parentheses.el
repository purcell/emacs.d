;;; highlight-parentheses.el --- highlight surrounding parentheses
;;
;; Copyright (C) 2007, 2009 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 1.0.1
;; Keywords: faces, matching
;; URL: http://nschum.de/src/emacs/highlight-parentheses/
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Add the following to your .emacs file:
;; (require 'highlight-parentheses)
;;
;; Enable `highlight-parentheses-mode'.
;;
;;; Change Log:
;;
;; 2009-03-19 (1.0.1)
;;    Added setter for color variables.
;;
;; 2007-07-30 (1.0)
;;    Added background highlighting and faces.
;;
;; 2007-05-15 (0.9.1)
;;    Support for defcustom.
;;
;; 2007-04-26 (0.9)
;;    Initial Release.
;;
;;; Code:

(eval-when-compile (require 'cl))

(defgroup highlight-parentheses nil
  "Highlight surrounding parentheses"
  :group 'faces
  :group 'matching)

(defun hl-paren-set (variable value)
  (set variable value)
  (when (fboundp 'hl-paren-color-update)
    (hl-paren-color-update)))

(defcustom hl-paren-colors
  '("firebrick1" "IndianRed1" "IndianRed3" "IndianRed4")
  "*List of colors for the highlighted parentheses.
The list starts with the the inside parentheses and moves outwards."
  :type '(repeat color)
  :set 'hl-paren-set
  :group 'highlight-parentheses)

(defcustom hl-paren-background-colors nil
  "*List of colors for the background highlighted parentheses.
The list starts with the the inside parentheses and moves outwards."
  :type '(repeat color)
  :set 'hl-paren-set
  :group 'highlight-parentheses)

(defface hl-paren-face nil
  "*Face used for highlighting parentheses.
Color attributes might be overriden by `hl-paren-colors' and
`hl-paren-background-colors'."
  :group 'highlight-parentheses)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar hl-paren-overlays nil
  "This buffers currently active overlays.")
(make-variable-buffer-local 'hl-paren-overlays)

(defvar hl-paren-last-point 0
  "The last point for which parentheses were highlighted.
This is used to prevent analyzing the same context over and over.")
(make-variable-buffer-local 'hl-paren-last-point)

(defun hl-paren-highlight ()
  "Highlight the parentheses around point."
  (unless (= (point) hl-paren-last-point)
    (setq hl-paren-last-point (point))
    (let ((overlays hl-paren-overlays)
          pos1 pos2
          (pos (point)))
      (save-excursion
        (condition-case err
            (while (and (setq pos1 (cadr (syntax-ppss pos1)))
                        (cddr overlays))
              (move-overlay (pop overlays) pos1 (1+ pos1))
              (when (setq pos2 (scan-sexps pos1 1))
                (move-overlay (pop overlays) (1- pos2) pos2)
                ))
          (error nil))
        (goto-char pos))
      (dolist (ov overlays)
        (move-overlay ov 1 1)))))

;;;###autoload
(define-minor-mode highlight-parentheses-mode
  "Minor mode to highlight the surrounding parentheses."
  nil " hl-p" nil
  (if highlight-parentheses-mode
      (progn
        (hl-paren-create-overlays)
        (add-hook 'post-command-hook 'hl-paren-highlight nil t))
    (mapc 'delete-overlay hl-paren-overlays)
    (kill-local-variable 'hl-paren-overlays)
    (kill-local-variable 'hl-paren-point)
    (remove-hook 'post-command-hook 'hl-paren-highlight t)))

;;; overlays ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hl-paren-create-overlays ()
  (let ((fg hl-paren-colors)
        (bg hl-paren-background-colors)
        attributes)
    (while (or fg bg)
      (setq attributes (face-attr-construct 'hl-paren-face))
      (when (car fg)
        (setq attributes (plist-put attributes :foreground (car fg))))
      (pop fg)
      (when (car bg)
        (setq attributes (plist-put attributes :background (car bg))))
      (pop bg)
      (dotimes (i 2) ;; front and back
        (push (make-overlay 0 0) hl-paren-overlays)
        (overlay-put (car hl-paren-overlays) 'face attributes)))
    (setq hl-paren-overlays (nreverse hl-paren-overlays))))

(defun hl-paren-color-update ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when hl-paren-overlays
        (mapc 'delete-overlay hl-paren-overlays)
        (setq hl-paren-overlays nil)
        (hl-paren-create-overlays)
        (let ((hl-paren-last-point -1)) ;; force update
          (hl-paren-highlight))))))

(provide 'highlight-parentheses)

;;; highlight-parentheses.el ends here
