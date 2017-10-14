;;; enh-ruby-mode-expansions.el --- Expansions for enh-ruby-mode

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

;; enh-ruby-mode doesn't use ruby-mode's mark-defun - it has its own.
;;
;; Feel free to contribute any other expansions for enh-ruby-mode at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(defun er/add-enh-ruby-mode-expansions ()
  "Adds Ruby-specific expansions for buffers in enh-ruby-mode"
  (require 'ruby-mode-expansions)

  (set (make-local-variable 'er/try-expand-list) (append
                                                  (remove 'er/mark-defun er/try-expand-list)
                                                  '(er/mark-ruby-instance-variable
                                                    er/mark-ruby-block-up))))

(er/enable-mode-expansions 'enh-ruby-mode 'er/add-enh-ruby-mode-expansions)

(provide 'enh-ruby-mode-expansions)
