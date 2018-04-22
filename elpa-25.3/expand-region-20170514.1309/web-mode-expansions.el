;;; web-mode-expansions.el --- Thin layer for adapting fxbois's web-mode-mark-and-expand function
;;; to expand-region

;; Copyright (C) 2012 Rotem Yaari

;; Authors: Rotem Yaari
;; Based on, and makes use of web-mode.el by fxbois

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

(defun er/add-web-mode-expansions ()
  (set (make-local-variable 'er/try-expand-list)
       (append er/try-expand-list
               '(web-mode-mark-and-expand))))

(add-hook 'web-mode-hook 'er/add-web-mode-expansions)

(provide 'web-mode-expansions)
