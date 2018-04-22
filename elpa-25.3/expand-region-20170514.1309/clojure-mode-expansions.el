;;; clojure-mode-expansions.el --- Clojure-specific expansions for expand-region

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

;; Extra expansions for clojure-mode:
;;
;; * `er/mark-clj-word` - includes dashes, but not slashes.
;; * `er/mark-clj-regexp-literal`
;; * `er/mark-clj-function-literal`
;;
;; Feel free to contribute any other expansions for Clojure at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(require 'expand-region-core)
(require 'er-basic-expansions)

(defun er/mark-clj-word ()
  "Mark the entire word around or in front of point, including dashes."
  (interactive)
  (let ((word-regexp "\\(\\sw\\|-\\)"))
    (when (or (looking-at word-regexp)
              (er/looking-back-on-line word-regexp))
      (while (looking-at word-regexp)
        (forward-char))
      (set-mark (point))
      (while (er/looking-back-on-line word-regexp)
        (backward-char)))))

(defun er/mark-clj-set-literal ()
  "Mark clj-set-literal presumes that point is outside the brackets.
If point is inside the brackets, those will be marked first anyway."
  (interactive)
  (when (or (looking-at "#{")
            (er/looking-back-exact "#"))
    (forward-char 1)
    (search-backward "#")
    (set-mark (point))
    (search-forward "{")
    (forward-char -1)
    (forward-list 1)
    (exchange-point-and-mark)))

(defun er/mark-clj-regexp-literal ()
  "Mark clj-regexp-literal presumes that point is outside the string.
If point is inside the string, the quotes will be marked first anyway."
  (interactive)
  (when (or (looking-at "#\"")
            (er/looking-back-exact "#"))
    (forward-char 1)
    (search-backward "#")
    (set-mark (point))
    (search-forward "\"")
    (forward-char 1)
    (er--move-point-forward-out-of-string)
    (exchange-point-and-mark)))

(defun er/mark-clj-function-literal ()
  "Mark clj-function-literal presumes that point is outside the parens.
If point is inside the parens, they will be marked first anyway."
  (interactive)
  (when (or (looking-at "#(")
            (er/looking-back-exact "#"))
    (forward-char)
    (search-backward "#")
    (set-mark (point))
    (search-forward "(")
    (backward-char)
    (forward-list)
    (exchange-point-and-mark)))

(defun er/add-clojure-mode-expansions ()
  "Adds clojure-specific expansions for buffers in clojure-mode"
  (set (make-local-variable 'er/try-expand-list) (append
                                                  er/try-expand-list
                                                  '(er/mark-clj-word
                                                    er/mark-clj-regexp-literal
                                                    er/mark-clj-set-literal
                                                    er/mark-clj-function-literal))))

(er/enable-mode-expansions 'clojure-mode 'er/add-clojure-mode-expansions)
(er/enable-mode-expansions 'nrepl-mode 'er/add-clojure-mode-expansions)

(provide 'clojure-mode-expansions)

;; clojure-mode-expansions.el ends here
