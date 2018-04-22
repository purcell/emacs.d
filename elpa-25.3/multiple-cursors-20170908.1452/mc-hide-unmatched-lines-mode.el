;;; mc-hide-unmatched-lines.el

;; Copyright (C) 2014 Aleksey Fedotov

;; Author: Aleksey Fedotov <lexa@cfotr.com>
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

;; This minor mode when enabled hides all lines where no cursors (and
;; also hum/lines-to-expand below and above) To make use of this mode
;; press "C-'" while multiple-cursor-mode is active. You can still
;; edit lines while you are in mc-hide-unmatched-lines mode. To leave
;; this mode press "<return>" or "C-g"
;;

;;; Code:

(require 'multiple-cursors-core)
(require 'mc-mark-more)

(defvar hum/hide-unmatched-lines-mode-map (make-sparse-keymap)
  "Keymap for hide unmatched lines is mainly for rebinding C-g")

(define-key hum/hide-unmatched-lines-mode-map (kbd "C-g") 'hum/keyboard-quit)
(define-key hum/hide-unmatched-lines-mode-map (kbd "<return>") 'hum/keyboard-quit)

(defun hum/keyboard-quit ()
  "Leave hide-unmatched-lines mode"
  (interactive)
  (mc-hide-unmatched-lines-mode 0))

;; used only in in multiple-cursors-mode-disabled-hook
(defun hum/disable-hum-mode ()
  (mc-hide-unmatched-lines-mode 0))

;;;###autoload
(define-minor-mode mc-hide-unmatched-lines-mode
  "Minor mode when enabled hides all lines where no cursors (and
also hum/lines-to-expand below and above) To make use of this
mode press \"C-'\" while multiple-cursor-mode is active. You can
still edit lines while you are in mc-hide-unmatched-lines
mode. To leave this mode press <return> or \"C-g\""
  nil " hu"
  hum/hide-unmatched-lines-mode-map
  (if mc-hide-unmatched-lines-mode
      ;;just in case if mc mode will be disabled while hide-unmatched-lines is active
      (progn
        (hum/hide-unmatched-lines)
        (add-hook 'multiple-cursors-mode-disabled-hook 'hum/disable-hum-mode t t))
    (progn
      (hum/unhide-unmatched-lines)
      (remove-hook 'multiple-cursors-mode-disabled-hook 'hum/disable-hum-mode))))

(defconst hum/invisible-overlay-name 'hum/invisible-overlay-name)

(defcustom hum/lines-to-expand 2
  "How many lines below and above cursor to show"
  :type '(integer)
  :group 'multiple-cursors)

(defcustom hum/placeholder "..."
  "Placeholder which will be placed insted of hiden text"
  :type '(string)
  :group 'multiple-cursors)

(defun hum/add-invisible-overlay (begin end)
  (let ((overlay (make-overlay begin
                               end
                               (current-buffer)
                               t
                               nil
                               )))
    (overlay-put overlay hum/invisible-overlay-name t)
    (overlay-put overlay 'invisible t)
    (overlay-put overlay 'intangible t)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'after-string hum/placeholder)))

(defun hum/hide-unmatched-lines ()
  (let ((begin (point-min)))
    (mc/for-each-cursor-ordered
     (save-excursion
       (goto-char (mc/cursor-beg cursor))
       (if (< begin (line-beginning-position (- hum/lines-to-expand)))
           (hum/add-invisible-overlay begin (line-end-position (- hum/lines-to-expand))))
       (setq begin (line-beginning-position (+ 2 hum/lines-to-expand)))))
    (hum/add-invisible-overlay begin (point-max))))

(defun hum/unhide-unmatched-lines ()
  (remove-overlays nil nil hum/invisible-overlay-name t))

(provide 'mc-hide-unmatched-lines-mode)
(define-key mc/keymap (kbd "C-'") 'mc-hide-unmatched-lines-mode)
