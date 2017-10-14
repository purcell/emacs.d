;;; haskell-navigate-imports.el --- A function for cycling through Haskell import lists -*- lexical-binding: t -*-

;; Copyright (C) 2010  Chris Done

;; Author: Chris Done <chrisdone@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The cycling step will stop once at the last import list so
;; that it is easy to add a new import list.

;; This module works completely independently of any libraries
;; (including haskell-mode).

;; Exports three interactive functions:
;; 1. haskell-navigate-imports
;; 2. haskell-navigate-imports-go
;; 3. haskell-navigate-imports-return

;; Example usage:

;; (require 'haskell-navigate-imports)
;; (define-key haskell-mode-map (kbd "<f8>") 'haskell-navigate-imports)

;;; Code:

(defvar haskell-navigate-imports-start-point nil)

;;;###autoload
(defun haskell-navigate-imports (&optional return)
  "Cycle the Haskell import lines or return to point (with prefix arg)."
  (interactive "P")
  (if return
      (haskell-navigate-imports-return)
    (haskell-navigate-imports-go)))

;;;###autoload
(defun haskell-navigate-imports-go ()
  "Go to the first line of a list of consecutive import lines. Cycles."
  (interactive)
  (unless (or (haskell-navigate-imports-line)
              (equal (line-beginning-position) (point-min))
              (save-excursion (forward-line -1)
                              (haskell-navigate-imports-line)))
    (setq haskell-navigate-imports-start-point (point)))
  (haskell-navigate-imports-go-internal))

;;;###autoload
(defun haskell-navigate-imports-return ()
  "Return to the non-import point we were at before going to the module list.
   If we were originally at an import list, we can just cycle through easily."
  (interactive)
  (when haskell-navigate-imports-start-point
    (goto-char haskell-navigate-imports-start-point)))

(defun haskell-navigate-imports-go-internal ()
  "Go to the first line of a list of consecutive import lines. Cycle."
  (if (haskell-navigate-imports-line)
      (progn (haskell-navigate-imports-goto-end)
             (when (haskell-navigate-imports-find-forward-line)
               (haskell-navigate-imports-go-internal)))
    (let ((point (haskell-navigate-imports-find-forward-line)))
      (if point
          (goto-char point)
        (progn (goto-char (point-min))
               (if (haskell-navigate-imports-find-forward-line)
                   (haskell-navigate-imports-go-internal)
                 (when (search-forward-regexp "^module" nil t 1)
                   (search-forward "\n\n" nil t 1))))))))

(defun haskell-navigate-imports-goto-end ()
  "Skip a bunch of consecutive import lines."
  (while (not (or (equal (point)
                         (point-max))
                  (not (haskell-navigate-imports-line))))
    (forward-line)))

(defun haskell-navigate-imports-find-forward-line ()
  "Return a point with at an import line, or nothing."
  (save-excursion
    (while (not (or (equal (point) (point-max))
                    (haskell-navigate-imports-after-imports-p) ;; This one just speeds it up.
                    (haskell-navigate-imports-line)))
      (forward-line))
    (if (haskell-navigate-imports-line)
        (point)
        nil)))

(defun haskell-navigate-imports-line ()
  "Try to match the current line as a regexp."
  (let ((line (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position))))
    (if (string-match "^import " line)
        line
      nil)))

(defun haskell-navigate-imports-after-imports-p ()
  "Are we after the imports list? Just for a speed boost."
  (save-excursion
    (goto-char (line-beginning-position))
    (not (not (search-forward-regexp "\\( = \\|\\<instance\\>\\| :: \\)"
                                     (line-end-position) t 1)))))

(provide 'haskell-navigate-imports)

;;; haskell-navigate-imports.el ends here
