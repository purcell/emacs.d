;;; html-mode-expansions.el --- HTML-specific expansions for expand-region

;; Copyright (C) 2011 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
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

;; Extra expansions for HTML that I've found useful so far:
;;
;;     er/mark-html-attribute
;;     er/mark-inner-tag
;;     er/mark-outer-tag
;;
;; Feel free to contribute any other expansions for HTML at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(require 'expand-region-core)
(require 'sgml-mode)

(defun er/mark-html-attribute ()
  "Mark html-attribute presumes that point is at the assignment part of attr=\"value\".
If point is inside the value-string, the quotes will be marked
first anyway.  Does not support html-attributes with spaces
around the equal sign or unquotes attributes atm."
  (interactive)
  (when (or (looking-at "\\(\\s_\\|\\sw\\)*=")
            (er/looking-back-exact "="))
    (search-backward " ")
    (forward-char 1)
    (set-mark (point))
    (search-forward "=")
    (forward-sexp 1)
    (exchange-point-and-mark)))

(defun er--looking-at-marked-tag ()
  "Is point looking at a tag that is entirely marked?"
  (and (looking-at "<")
       (>= (mark)
           (save-excursion
             (sgml-skip-tag-forward 1)
             (point)))))

(defun er--inside-tag-p ()
  "Is point inside a tag?"
  (save-excursion
    (not (null (sgml-get-context)))))

(defun er/mark-outer-tag ()
  "Mark from opening to closing tag, including the tags."
  (interactive)
  (when (and (er--inside-tag-p)
             (or (not (looking-at "<"))
                 (er--looking-at-marked-tag)))
    (goto-char (aref (car (last (sgml-get-context))) 2)))
  (when (looking-at "<")
    (set-mark (point))
    (sgml-skip-tag-forward 1)
    (exchange-point-and-mark)))

(defun er/mark-inner-tag ()
  "Mark the contents of an open tag, not including the tags."
  (interactive)
  (goto-char (aref (car (last (sgml-get-context))) 3))
  (set-mark (point))
  (backward-char 1)
  (sgml-skip-tag-forward 1)
  (search-backward "</")
  (exchange-point-and-mark))

(defun er/add-html-mode-expansions ()
  "Adds HTML-specific expansions for buffers in html-mode"
  (set (make-local-variable 'er/try-expand-list) (append
                                                  er/try-expand-list
                                                  '(er/mark-html-attribute
                                                    er/mark-inner-tag
                                                    er/mark-outer-tag))))

(er/enable-mode-expansions 'html-mode 'er/add-html-mode-expansions)
(er/enable-mode-expansions 'rhtml-mode 'er/add-html-mode-expansions)
(er/enable-mode-expansions 'nxhtml-mode 'er/add-html-mode-expansions)
(er/enable-mode-expansions 'web-mode 'er/add-html-mode-expansions)

(provide 'html-mode-expansions)

;; html-mode-expansions.el ends here
