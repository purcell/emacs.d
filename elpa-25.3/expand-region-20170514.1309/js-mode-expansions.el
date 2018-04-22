;;; js-mode-expansions.el --- JS-specific expansions for expand-region

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

;; Extra expansions for JavaScript that I've found useful so far:
;;
;;    er/mark-js-function
;;    er/mark-js-object-property-value
;;    er/mark-js-object-property
;;    er/mark-js-if
;;    er/mark-js-inner-return
;;    er/mark-js-outer-return
;;
;; Feel free to contribute any other expansions for JavaScript at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(require 'expand-region-core)

(defun er/mark-js-function ()
  "Mark the current JavaScript function."
  (interactive)
  (condition-case nil
      (forward-char 8)
    (error nil))
  (word-search-backward "function")
  (while (or (er--point-inside-string-p)
             (er--point-is-in-comment-p))
    (word-search-backward "function"))
  (set-mark (point))
  (while (not (looking-at "{"))
    (forward-char))
  (forward-list)
  (exchange-point-and-mark))

(defun er/mark-js-outer-return ()
  "Mark the current return statement, including return and ending semi-colon"
  (interactive)
  (condition-case nil
      (forward-char 6)
    (error nil))
  (word-search-backward "return")
  (while (or (er--point-inside-string-p)
             (er--point-is-in-comment-p))
    (word-search-backward "return"))
  (set-mark (point))
  (while (not (looking-at ";"))
    (if (looking-at "\\s(")
        (forward-list)
      (forward-char)))
  (forward-char)
  (exchange-point-and-mark))

(defun er/mark-js-inner-return ()
  "Mark contents of the current return statement, not including return or semi-colon"
  (interactive)
  (condition-case nil
      (forward-char 6)
    (error nil))
  (word-search-backward "return")
  (while (or (er--point-inside-string-p)
             (er--point-is-in-comment-p))
    (word-search-backward "return"))
  (search-forward " ")
  (set-mark (point))
  (while (not (looking-at ";"))
    (if (looking-at "\\s(")
        (forward-list)
      (forward-char)))
  (exchange-point-and-mark))

(defun er/mark-js-if ()
  "Mark the current if-statement."
  (interactive)
  (condition-case nil
      (forward-char 2)
    (error nil))
  (word-search-backward "if")
  (while (or (er--point-inside-string-p)
             (er--point-is-in-comment-p))
    (word-search-backward "if"))
  (set-mark (point))
  (while (not (looking-at "("))
    (forward-char))
  (forward-list)
  (while (not (looking-at "{"))
    (forward-char))
  (forward-list)
  (exchange-point-and-mark))

(defun er/mark-js-object-property-value ()
  "Mark the current object property value, ie. from : to , or }"
  (interactive)
  (unless (er--point-inside-pairs-p)
    (error "Point is not inside an object"))
  (search-backward ":")
  (forward-char)
  (search-forward-regexp "[^\s]")
  (backward-char)
  (set-mark (point))
  (while (not (looking-at "[},]"))
    (if (looking-at "\\s(")
        (forward-list)
      (forward-char)))
  (when (er/looking-back-max "[\s\n]" 400)
    (search-backward-regexp "[^\s\n]")
    (forward-char))
  (exchange-point-and-mark))

(defun er/mark-js-object-property ()
  "Mark js-object-property presumes that point is at the assignment part of key: value.
If point is inside the value, that will be marked first anyway."
  (interactive)
  (when (or (looking-at "\"?\\(\\s_\\|\\sw\\| \\)*\":")
            (looking-at "\\(\\s_\\|\\sw\\)*:")
            (er/looking-back-max ": ?" 2))
    (search-backward-regexp "[{,]")
    (forward-char)
    (search-forward-regexp "[^\s\n]")
    (backward-char)
    (set-mark (point))
    (search-forward ":")
    (while (or (not (looking-at "[},]"))
               (er--point-inside-string-p))
      (if (looking-at "\\s(")
          (forward-list)
        (forward-char)))
    (when (er/looking-back-max "[\s\n]" 400)
      (search-backward-regexp "[^\s\n]")
      (forward-char))
    (exchange-point-and-mark)))

(defun er/mark-js-call ()
  "Mark the current symbol (including dots) and then parens or squares."
  (interactive)
  (let ((symbol-regexp "\\(\\s_\\|\\sw\\|\\.\\)+"))
    (when (or (looking-at symbol-regexp)
              (er/looking-back-on-line symbol-regexp))
      (skip-syntax-backward "_w.")
      (when (looking-at "!")
        (forward-char 1))
      (set-mark (point))
      (when (looking-at symbol-regexp)
        (goto-char (match-end 0)))
      (if (looking-at "\\[\\|(")
          (forward-list))
      (exchange-point-and-mark))))

(defun er/add-js-mode-expansions ()
  "Adds JS-specific expansions for buffers in js-mode"
  (set (make-local-variable 'er/try-expand-list) (append
                                                  er/try-expand-list
                                                  '(er/mark-js-function
                                                    er/mark-js-object-property-value
                                                    er/mark-js-object-property
                                                    er/mark-js-if
                                                    er/mark-js-inner-return
                                                    er/mark-js-outer-return
                                                    er/mark-js-call))))

(er/enable-mode-expansions 'js-mode 'er/add-js-mode-expansions)
(er/enable-mode-expansions 'js2-mode 'er/add-js-mode-expansions)
(er/enable-mode-expansions 'js3-mode 'er/add-js-mode-expansions)

(provide 'js-mode-expansions)

;; js-mode-expansions.el ends here
