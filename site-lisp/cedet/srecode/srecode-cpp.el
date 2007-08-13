;;; srecode-cpp.el --- C++ specific handlers for Semantic Recoder

;; Copyright (C) 2007 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: srecode-cpp.el,v 1.2 2007/03/19 02:38:11 zappo Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Supply some C++ specific dictionary fillers and helpers

;;; Code:

;;; :cpp ARGUMENT HANDLING
;;
;; When a :cpp argument is required, fill the dictionary with
;; information about the current C++ file.
;;
;; Error if not in a C++ mode.

;;;###autoload
(defun srecode-semantic-handle-:cpp (dict)
  "Add macros into the dictionary DICT based on the current c++ file.
Adds the following:
FILENAME_SYMBOL - filename converted into a C compat symbol.
HEADER - Shown section if in a header file."
  ;; A symbol representing
  (let ((fsym (file-name-nondirectory (buffer-file-name)))
	(case-fold-search t))

    ;; Are we in a header file?
    (if (string-match "\\.\\(h\\|hh\\|hpp\\|h++\\)$" fsym)
	(srecode-dictionary-show-section dict "HEADER")
      (srecode-dictionary-show-section dict "NOTHEADER"))

    ;; Strip out bad characters
    (while (string-match "\\.\\| " fsym)
      (setq fsym (replace-match "_" t t fsym)))
    (srecode-dictionary-set-value
     dict "FILENAME_SYMBOL" fsym)
    )
  )



(provide 'srecode-cpp)

;;; srecode-cpp.el ends here
