;;; srecode-java.el --- Srecode Java support

;; Copyright (C) 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: srecode-java.el,v 1.1 2009/01/06 02:41:09 zappo Exp $

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
;; Special support for the Java language.

;;; Code:
;;;###autoload
(defun srecode-semantic-handle-:java (dict)
  "Add macros into the dictionary DICT based on the current java file.
Adds the following:
FILENAME_AS_PACKAGE - file/dir converted into a java package name.
FILENAME_AS_CLASS - file converted to a Java class name."
  ;; A symbol representing
  (let* ((fsym (file-name-nondirectory (buffer-file-name)))
	 (fnox (file-name-sans-extension fsym))
	 (dir (file-name-directory (buffer-file-name)))
	 (fpak fsym)
	 )
    (while (string-match "\\.\\| " fpak)
      (setq fpak (replace-match "_" t t fpak)))
    (if (string-match "src/" dir)
	(setq dir (substring dir (match-end 0)))
      (setq dir (file-name-nondirectory (directory-file-name dir))))
    (while (string-match "/" dir)
      (setq dir (replace-match "_" t t dir)))
    (srecode-dictionary-set-value dict "FILENAME_AS_PACKAGE"
				  (concat dir "." fpak))
    (srecode-dictionary-set-value dict "FILENAME_AS_CLASS" fnox)
    ))

(provide 'srecode-java)
;;; srecode-java.el ends here
