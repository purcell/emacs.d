;;; feature-mode-expansions.el --- cucumber-specific expansions for expand-region

;; Copyright (C) 2012 Raimon Grau

;; Author: Raimon Grau
;; Based on js-mode-expansions by: Raimon Grau <raimonster@gmail.com>
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


;; expanders to mark feature semantic objects like step or scenario
;;
;; Expansions:
;;
;;
;;  er/mark-feature-scenario
;;  er/mark-feature-step

(require 'expand-region-core)

(defun er--block-between-keywords (start-keywords-regexp &optional end-keywords-regexp)
  (let* ((start-key-words (concat "^\\( \\)*" start-keywords-regexp))
         (end-key-words (concat "^\\( \\)*" (or end-keywords-regexp start-keywords-regexp))))
    (when (looking-at-p "[^\\s-]")
      (skip-syntax-forward "w."))
    (if (looking-at-p start-keywords-regexp)
        (progn (beginning-of-line)
               (exchange-point-and-mark))
      (re-search-backward start-key-words)
      (set-mark (point))
      (re-search-forward start-key-words))
    (unless (re-search-forward end-key-words (point-max) t)
      (goto-char (point-max)))
   (forward-line 0)
   (exchange-point-and-mark)))

(defun er/mark-feature-scenario ()
  (interactive)
  (er--block-between-keywords "\\(Background:\\|Scenario:\\|Feature:\\)"))

(defun er/mark-feature-step ()
  (interactive)
  (er--block-between-keywords "\\(And\\|Given\\|When\\|Then\\)"  "\\(And\\|Given\\|When\\|Then\\|Scenario:\\)"))

(defun er/add-feature-mode-expansions ()
  "Adds cucumber-specific expansions for buffers in feature-mode"
  (set (make-local-variable 'er/try-expand-list) (append
                                                  er/try-expand-list
                                                  '(er/mark-feature-scenario
                                                    er/mark-feature-step))))

(er/enable-mode-expansions 'feature-mode 'er/add-feature-mode-expansions)

(provide 'feature-mode-expansions)
