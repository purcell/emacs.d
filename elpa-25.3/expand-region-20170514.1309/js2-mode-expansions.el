;;; js2-mode-expansions.el --- Additional expansions for js2-mode

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

;; Extra expansions specifically for js2-mode, since it has
;; a semantic parser.
;;
;; Feel free to contribute any other expansions for JavaScript at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(require 'expand-region-core)
(declare-function js2-node-parent-stmt "js2-mode")
(declare-function js2-node-at-point "js2-mode")
(declare-function js2-node-abs-pos "js2-mode")
(declare-function js2-node-len "js2-mode")

(defun js2-mark-parent-statement ()
  (interactive)
  (let* ((parent-statement (if (not (er/looking-back-exact ";"))
                               (js2-node-parent-stmt (js2-node-at-point))
                             (forward-char -1)
                             (js2-node-at-point)))
         (beg (js2-node-abs-pos parent-statement))
         (end (+ beg (js2-node-len parent-statement))))
    (goto-char beg)
    (set-mark end)))

(defun er/add-js2-mode-expansions ()
  "Adds expansions for buffers in js2-mode"
  (set (make-local-variable 'er/try-expand-list) (append
                                                  er/try-expand-list
                                                  '(js2-mark-parent-statement))))

(er/enable-mode-expansions 'js2-mode 'er/add-js2-mode-expansions)

(provide 'js2-mode-expansions)

;; js2-mode-expansions.el ends here
