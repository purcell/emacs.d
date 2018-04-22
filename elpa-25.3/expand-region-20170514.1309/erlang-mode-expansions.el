;;; erlang-mode-expansions.el --- Erlang-specific expansions for expand-region

;; Copyright (C) 2012 Gleb Peregud

;; Author: Gleb Peregud
;; Based on python-mode-expansions by: Ivan Andrus <darthandrus@gmail.com>
;; Keywords: marking region erlang

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

;; Feel free to contribute any other expansions for Erlang at
;;
;;     https://github.com/magnars/expand-region.el

;;; Bugs:

;; Doesn't handle many Erlang syntax constructs, just the basics

;;; Code:

(require 'expand-region-core)

(defun er/add-erlang-mode-expansions ()
  "Adds Erlang-specific expansions for buffers in erlang-mode"
  (set (make-local-variable 'er/try-expand-list) (append
                                                  er/try-expand-list
                                                  '(erlang-mark-function
                                                    erlang-mark-clause))))

(er/enable-mode-expansions 'erlang-mode 'er/add-erlang-mode-expansions)

(provide 'erlang-mode-expansions)

;; erlang-mode-expansions.el ends here
