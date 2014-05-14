;;; company-abbrev.el --- company-mode completion back-end for abbrev

;; Copyright (C) 2009-2011  Free Software Foundation, Inc.

;; Author: Nikolaj Schumacher

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(require 'company)
(eval-when-compile (require 'cl))
(require 'abbrev)

(defun company-abbrev-insert (match)
  "Replace MATCH with the expanded abbrev."
  (expand-abbrev))

;;;###autoload
(defun company-abbrev (command &optional arg &rest ignored)
  "`company-mode' completion back-end for abbrev."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-abbrev
                                        'company-abbrev-insert))
    (prefix (company-grab-symbol))
    (candidates (nconc
                 (delete "" (all-completions arg global-abbrev-table))
                 (delete "" (all-completions arg local-abbrev-table))))
    (meta (abbrev-expansion arg))
    (require-match t)))

(provide 'company-abbrev)
;;; company-abbrev.el ends here
