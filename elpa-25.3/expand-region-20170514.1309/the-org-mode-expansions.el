;;; the-org-mode-expansions.el --- Expansions for expand-region to be used in org-mode

;; Copyright (C) 2012 Magnar Sveen

;; Author: Magnar Sveen
;; Based on text-mode-expansions by: Ivan Andrus
;; Keywords: marking region

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

;; The file needs to be weirdly name (prefixed with the-) to avoid
;; conflict with org-reload, which bases its functionality on the names
;; of files, for some reason.
;;
;; Feel free to contribute any other expansions for org-mode at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(require 'expand-region-core)

(declare-function org-up-element "org")
(declare-function org-mark-subtree "org")

(defun er/mark-sentence ()
  "Marks one sentence."
  (interactive)
  (forward-char 1)
  (backward-sentence 1)
  (set-mark (point))
  (forward-sentence 1)
  (exchange-point-and-mark))

(defun er/mark-paragraph ()
  "Marks one paragraph."
  (interactive)
  (mark-paragraph)
  (exchange-point-and-mark)
  (skip-chars-backward er--space-str)
  (exchange-point-and-mark)
  (skip-chars-forward er--space-str))

(defun er/mark-org-code-block ()
  "Marks an org-code-block."
  (interactive)
  (let ((case-fold-search t)
        (re "#\\+begin_\\(\\sw+\\)"))
    (unless (looking-at re)
      (search-backward-regexp re))
    (set-mark (point))
    (search-forward (concat "#+end_" (match-string 1)))
    (exchange-point-and-mark)))

(defun er/mark-org-parent ()
  "Marks a heading 1 level up from current subheading"
  (interactive)
  (org-up-element)
  (org-mark-subtree))

(defun er/add-org-mode-expansions ()
  "Adds org-specific expansions for buffers in org-mode"
  (set (make-local-variable 'er/try-expand-list)
       (append
        (remove #'er/mark-defun er/try-expand-list)
        '(org-mark-subtree
          er/mark-org-code-block
          er/mark-sentence
          er/mark-org-parent
          er/mark-paragraph))))

(er/enable-mode-expansions 'org-mode 'er/add-org-mode-expansions)

(provide 'the-org-mode-expansions)
