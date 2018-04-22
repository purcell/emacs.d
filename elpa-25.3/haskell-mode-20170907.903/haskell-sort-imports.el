;;; haskell-sort-imports.el --- Sort the list of Haskell imports at the point alphabetically -*- lexical-binding: t -*-

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

;; If the region is active it sorts the imports within the
;; region.

;; This will align and sort the columns of the current import
;; list.  It's more or less the coolest thing on the planet.

;;; Code:

(require 'cl-lib)

(defvar haskell-sort-imports-regexp
  (concat "^import[ ]+"
          "\\(qualified \\)?"
          "[ ]*\\(\"[^\"]*\" \\)?"
          "[ ]*\\([A-Za-z0-9_.']*.*\\)"))

;;;###autoload
(defun haskell-sort-imports ()
  "Sort the import list at point. It sorts the current group
i.e. an import list separated by blank lines on either side.

If the region is active, it will restrict the imports to sort
within that region."
  (interactive)
  (when (haskell-sort-imports-at-import)
    (let* ((points (haskell-sort-imports-decl-points))
           (current-string (buffer-substring-no-properties (car points)
                                                           (cdr points)))
           (current-offset (- (point) (car points))))
      (if (region-active-p)
          (progn (goto-char (region-beginning))
                 (haskell-sort-imports-goto-import-start))
        (haskell-sort-imports-goto-group-start))
      (let* ((start (point))
             (imports (haskell-sort-imports-collect-imports))
             (sorted (sort (cl-copy-list imports)
                           (lambda (a b)
                             (string< (haskell-sort-imports-normalize a)
                                      (haskell-sort-imports-normalize b))))))
        (when (not (equal imports sorted))
          (delete-region start (point))
          (mapc (lambda (import) (insert import "\n")) sorted))
        (goto-char start)
        (when (search-forward current-string nil t 1)
          (forward-char (- (length current-string)))
          (forward-char current-offset))))))

(defun haskell-sort-imports-normalize (i)
  "Normalize an import, if possible, so that it can be sorted."
  (if (string-match haskell-sort-imports-regexp
                    i)
      (match-string 3 i)
    i))

(defun haskell-sort-imports-collect-imports ()
  (let ((imports (list)))
    (while (looking-at "import")
      (let* ((points (haskell-sort-imports-decl-points))
             (string (buffer-substring-no-properties (car points)
                                                     (cdr points))))
        (goto-char (min (1+ (cdr points))
                        (point-max)))
        (setq imports (cons string imports))))
    (reverse (delq nil (delete-dups imports)))))

(defun haskell-sort-imports-goto-group-start ()
  "Go to the start of the import group."
  (or (and (search-backward "\n\n" nil t 1)
           (goto-char (+ 2 (line-end-position))))
      (when (search-backward-regexp "^module " nil t 1)
        (goto-char (1+ (line-end-position))))
      (goto-char (point-min))))

(defun haskell-sort-imports-at-import ()
  "Are we at an import?"
  (save-excursion
    (haskell-sort-imports-goto-import-start)
    (looking-at "import")))

(defun haskell-sort-imports-goto-import-start ()
  "Go to the start of the import."
  (goto-char (car (haskell-sort-imports-decl-points))))

(defun haskell-sort-imports-decl-points ()
  "Get the points of the declaration."
  (save-excursion
    (let ((start (or (progn (goto-char (line-end-position))
                            (search-backward-regexp "^[^ \n]" nil t 1)
                            (unless (or (looking-at "^-}$")
                                        (looking-at "^{-$"))
                              (point)))
                     0))
          (end (progn (goto-char (1+ (point)))
                      (or (when (search-forward-regexp "[\n]+[^ \n]" nil t 1)
                            (forward-char -1)
                            (search-backward-regexp "[^\n ]" nil t)
                            (line-end-position))
                          (when (search-forward-regexp "\n" nil t 1)
                            (1- (point)))
                          (point-max)))))
      (cons start end))))

(provide 'haskell-sort-imports)

;;; haskell-sort-imports.el ends here
