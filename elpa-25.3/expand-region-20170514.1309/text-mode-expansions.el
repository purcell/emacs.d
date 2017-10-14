;;; text-mode-expansions.el --- Expansions for expand-region to be used in text

;; Copyright (C) 2012 Ivan Andrus

;; Author: Ivan Andrus
;; Based on js-mode-expansions by: Magnar Sveen <magnars@gmail.com>
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

;; Feel free to contribute any other expansions for normal text at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(require 'expand-region-core)

(defun er/mark-text-sentence ()
  "Marks one sentence."
  (interactive)
  ;; The obvious
  ;; (backward-sentence 1) (mark-end-of-sentence 1)
  ;; doesn't work here because it's repeated and the selection keeps
  ;; growing by sentences, which isn't what's wanted.
  (forward-sentence 1)
  (set-mark (point))
  (backward-sentence 1))

(defun er/mark-text-paragraph ()
  "Marks one paragraph."
  (interactive)
  (mark-paragraph)
  (skip-chars-forward er--space-str))

(defun er/add-text-mode-expansions ()
  "Adds expansions for buffers in `text-mode' except for `html-mode'.
Unfortunately `html-mode' inherits from `text-mode' and
text-mode-expansions don't work well in `html-mode'."
  (unless (member major-mode expand-region-exclude-text-mode-expansions)
    (set (make-local-variable 'er/try-expand-list)
         (append
          er/try-expand-list
          '(er/mark-text-sentence
            er/mark-text-paragraph
            mark-page)))))

(er/enable-mode-expansions 'text-mode 'er/add-text-mode-expansions)

(provide 'text-mode-expansions)

;; text-mode-expansions.el ends here
