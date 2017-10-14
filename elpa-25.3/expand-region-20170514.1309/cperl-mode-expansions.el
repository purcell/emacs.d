;;; cperl-mode-expansions.el --- perl-specific expansions for expand-region

;; Copyright (C) 2012 Kang-min Liu

;; Author: Kang-min Liu <gugod@gugod.org>
;; Keywords: marking region cperl

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

;;; Code:

(require 'expand-region-core)

(defun er/mark-cperl-variable-name ()
  "Marks one perl variable"
  (interactive)
  (forward-word)
  (backward-word)
  (search-backward-regexp "[@$%]" (line-beginning-position))
  (set-mark (point))
  (forward-char)
  (search-forward-regexp "[^a-z_]" (line-end-position))
  (backward-char)
  (exchange-point-and-mark))

(defun er/mark-cperl-package-name ()
  "Marks one perl package name"
  (interactive)
  (forward-sexp)
  (backward-sexp)
  (set-mark (point))
  (forward-sexp)
  (search-backward "::" (line-beginning-position))
  (exchange-point-and-mark))

(defun er/mark-cperl-subroutine ()
  "Marks current subroutine body."
  (interactive)
  (end-of-defun)
  (set-mark (point))
  (beginning-of-defun))

(defun er/add-cperl-mode-expansions ()
  "Add cprel mode expansinos"
  (set (make-local-variable 'er/try-expand-list) (append
                                                  er/try-expand-list
                                                  '(er/mark-cperl-variable-name
                                                    er/mark-cperl-package-name
                                                    er/mark-cperl-subroutine
                                                    ))))

(er/enable-mode-expansions 'cperl-mode 'er/add-cperl-mode-expansions)

(provide 'cperl-mode-expansions)

;; cperl-mode-expansions.el ends here
