;;; highlight-parentheses.el --- highlight surrounding parentheses
;;
;; Copyright (C) 2007 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 0.9.1
;; Keywords: faces, matching
;; URL: http://nschum.de/src/emacs/highlight-parentheses/highlight-parentheses.el
;; Compatibility: GNU Emacs 22.x
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
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;
;;; Commentary:
;;
;; Add the following to your .emacs file:
;; (require 'highlight-parentheses)
;;
;; Enable `highlight-symbol-mode'.
;;
;;; Changes Log:
;;
;; 2007-05-15 (0.9.1)
;;    Support for defcustom.  Changed from vector to list.
;;
;; 2007-04-26 (0.9)
;;    Initial Release.
;;
;;; Code:

(require 'cl)

(defgroup highlight-parentheses nil
  "Highlight surrounding parentheses"
  :group 'faces
  :group 'matching)

(defcustom hl-paren-colors
  '("firebrick1" "IndianRed4" "IndianRed")
  "*List of colors for the highlighted parentheses.
The list starts with the the inside parentheses and moves outwards."
  :type '(repeat color)
  :group 'highlight-parentheses)

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
    (save-excursion
      (let ((pos (point))
            (match-pos (point))
            (level -1)
            (max (1- (length hl-paren-overlays))))
        (while (and match-pos (< level max))
          (setq match-pos
                (when (setq pos (cadr (syntax-ppss pos)))
                  (ignore-errors (scan-sexps pos 1))))
          (when match-pos
            (hl-paren-put-overlay pos (incf level))
            (hl-paren-put-overlay (1- match-pos) (incf level))))
        (while (< level max)
          (hl-paren-put-overlay nil (incf level)))))
    (setq hl-paren-last-point (point))))

(defun hl-paren-put-overlay (pos n)
  "Move or create the N'th overlay so its shown at POS."
  (let ((ov (elt hl-paren-overlays n)))
    (if pos
        (if ov
            (move-overlay ov pos (1+ pos))
          (setq ov (make-overlay pos (1+ pos)))
          (aset hl-paren-overlays n ov)
          (overlay-put ov 'face
                       (cons 'foreground-color
                             (nth (/ n 2) hl-paren-colors))))
      (when ov
        (delete-overlay ov)
        (aset hl-paren-overlays n nil)))))

;;;###autoload
(define-minor-mode highlight-parentheses-mode
  "Minor mode to highlight the surrounding parentheses."
  nil " hl-p" nil
  (if highlight-parentheses-mode
      (progn
        (setq hl-paren-overlays
              (make-vector (* 2 (length hl-paren-colors)) nil))
        (add-hook 'post-command-hook 'hl-paren-highlight nil t))
    (let (ov)
      (dotimes (i (length hl-paren-overlays))
        (when (setq ov (elt hl-paren-overlays i))
          (delete-overlay ov))))
    (kill-local-variable 'hl-paren-overlays)
    (kill-local-variable 'hl-paren-point)
    (remove-hook 'post-command-hook 'hl-paren-highlight t)))

(provide 'highlight-parentheses)

;;; highlight-parentheses.el ends here
