;;; css-mode-expansions.el --- CSS-specific expansions for expand-region

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

;; For now I have only found the need for mark-css-declaration.
;;
;; Feel free to contribute any other expansions for CSS at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(require 'expand-region-core)

(defun er/mark-css-declaration ()
  "Marks one CSS declaration, eg. font-weight: bold;"
  (interactive)
  (search-backward-regexp "[;{] ?" (line-beginning-position))
  (forward-char)
  (set-mark (point))
  (search-forward ";" (line-end-position))
  (exchange-point-and-mark))

(defun er/add-css-mode-expansions ()
  "Adds CSS-specific expansions for buffers in css-mode"
  (set (make-local-variable 'er/try-expand-list) (append
                                                  er/try-expand-list
                                                  '(er/mark-css-declaration))))

(er/enable-mode-expansions 'css-mode 'er/add-css-mode-expansions)

(provide 'css-mode-expansions)

;; css-mode-expansions.el ends here
