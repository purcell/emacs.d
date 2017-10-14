;;; jsp-expansions.el --- JSP-specific expansions for expand-region

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

;; Extra expansions for editing JSP files. To be used in conjunction
;; with the html-mode expansions
;;
;;     er/mark-jstl-escape
;;
;; These expansions aren't loaded by default, so you'll have to explicitly
;; ask for them in your init file with:
;;
;;     (eval-after-load 'sgml-mode '(require 'jsp-expansions))
;;
;; Feel free to contribute any other expansions for JSP at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(require 'expand-region-core)

(defun er/mark-jstl-escape ()
    "Mark jstl-escape presumes that point is outside the brackets.
If point is inside the brackets, they will be marked first anyway."
    (interactive)
      (when (or (looking-at "\\${")
            (er/looking-back-exact "$"))
    (forward-char 1)
    (search-backward "\$")
    (set-mark (point))
    (forward-char 1)
    (forward-list)
    (exchange-point-and-mark)))

(defun er/add-jsp-expansions ()
  "Adds JSP-specific expansions to the buffer"
  (set (make-local-variable 'er/try-expand-list) (append
                                                  er/try-expand-list
                                                  '(er/mark-jstl-escape))))

(er/enable-mode-expansions 'html-mode 'er/add-jsp-expansions)

(provide 'jsp-expansions)

;; jsp-expansions.el ends here
