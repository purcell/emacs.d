;;; haskell-collapse.el --- Collapse expressions -*- lexical-binding: t -*-

;; Copyright (c) 2014 Chris Done. All rights reserved.
;; Copyright (c) 2017 Vasantha Ganesh Kanniappan <vasanthaganesh.k@tuta.io>.

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

(require 'hideshow)

;;; TODO:
;;; -> Make it work for braces

(defun haskell-hide-toggle ()
  "Toggle visibility of existing forms at point. "
  (interactive)
  (hs-minor-mode 1)
  (save-excursion
    (let* ((modified (buffer-modified-p))
           (inhibit-read-only t)
           (position (haskell-indented-block))
           (beg (car position))
           (end (cdr position)))
      (if (and beg end)
          (if (overlays-in beg end)
              (hs-discard-overlays beg end)
            (hs-make-overlay beg end 'code)))
      (set-buffer-modified-p modified))))

(defun haskell-blank-line-p ()
  "Returns `t' if line is empty or composed only of whitespace."
  (save-excursion
    (beginning-of-line)
    (= (point-at-eol)
       (progn (skip-chars-forward "[:blank:]") (point)))))

(defun haskell-indented-block ()
  "return (start-of-indentation . end-of-indentation)"
  (let ((cur-indent (current-indentation))
        (nxt-line-indent (haskell-next-line-indentation 1))
        (prev-line-indent (haskell-next-line-indentation -1))
        (beg-of-line (save-excursion (end-of-line)
                                     (point))))
    (cond ((and (= cur-indent 0)
                (= nxt-line-indent 0)) nil)
          ((haskell-blank-line-p) nil)
          ((> nxt-line-indent cur-indent)
           (cons beg-of-line
                 (haskell-find-line-with-indentation '> 1)))
          ((or (= nxt-line-indent cur-indent)
               (<= prev-line-indent cur-indent))
           (cons (haskell-find-line-with-indentation '>= -1)
                 (haskell-find-line-with-indentation '>= 1)))
          (t nil))))

(defun haskell-next-line-indentation (dir)
  "returns (integer) indentation of the next if dir=1, previous line
indentation if dir=-1"
  (save-excursion
    (progn
      (while (and (zerop (forward-line dir))
                  (haskell-blank-line-p)))
      (current-indentation))))

(defun haskell-find-line-with-indentation (comparison direction)
  "comparison is >= or >, direction if 1 finds forward, if -1 finds backward"
  (save-excursion
    (let ((start-indent (current-indentation)))
      (progn
        (while (and (zerop (forward-line direction))
                    (or (haskell-blank-line-p)
                        (funcall comparison (current-indentation) start-indent))))
        (when (= direction 1) (forward-line -1))
        (end-of-line)
        (point)))))

(defun haskell-hide-toggle-all ()
  "hides all top level functions"
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (zerop (forward-line -1))
      (goto-char (point-at-bol))
      (when (= (current-indentation) 0) (haskell-hide-toggle)))))

(defvar haskell-collapse-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c @ C-c") 'haskell-hide-toggle)
    (define-key map (kbd "C-c @ C-M-c") 'haskell-hide-toggle-all)
    (define-key map (kbd "C-c @ C-M-s") 'haskell-hide-toggle-all)
    (define-key map (kbd "C-c @ C-M-h") 'haskell-hide-toggle-all)
    map)
  "Keymap for using `haskell-collapse-mode'.")

;;;###autoload
(define-minor-mode haskell-collapse-mode
  "Minor mode to collapse and expand haskell expressions"
  :init-value nil
  :lighter " Haskell-Collapse"
  :keymap haskell-collapse-mode-map)

(provide 'haskell-collapse)
