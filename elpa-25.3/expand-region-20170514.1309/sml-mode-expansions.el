;;; sml-mode-expansions.el --- Expansions for expand-region to be used in sml-mode

;; Copyright (C) 2012 Alexis Gallagher

;; Author: Alexis Gallagher
;; Based on js-mode-expansions by: Magnar Sveen <magnars@gmail.com>
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

;; Provides extra expansions for sml-mode:
;; - various expression (case, if, let)
;; - fun bindings
;; 
;; Tested with sml-mode version 6.3
;; 
;; Feel free to contribute any other expansions for SML at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(require 'expand-region-core)
(declare-function sml-find-matching-starter "sml-mode")

;; TODO: comma-delimited elements within a list,tuple,record
;; TODO: match expression, patterns
;; TODO: individual field, record type
;; TODO: head-or-tail, then cons expression

(defun er/sml-mark-keyword-prefixed-expression ()
  "Mark the surrounding expression."
  (interactive)
  (progn 
    (sml-find-matching-starter '("case" "let" "if" "raise"))
    (mark-sexp)))


(defun er/add-sml-mode-expansions ()
  "Adds expansions for buffers in `sml-mode'."
  (set (make-local-variable 'er/try-expand-list)
       (append er/try-expand-list
	       '(sml-mark-function
		 er/sml-mark-keyword-prefixed-expression
		 mark-sexp))))

(er/enable-mode-expansions 'sml-mode 'er/add-sml-mode-expansions)

(provide 'sml-mode-expansions)

;; sml-mode-expansions.el ends here
