;;; semantic-inc.el --- Include file handling for Semantic token streams

;;; Copyright (C) 2000 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-inc.el,v 1.2 2005/09/30 20:20:50 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic-inc is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; If you need to search for the files found in an `include'
;; statement, use the functions defined here.  To make sure your
;; language supports these functions, set values to the variables
;; found here.  The defaults may be capable of handling many
;; languages.

(require 'semantic)

;;; Code:
(defvar semantic-inc-include-path nil
  "Defines the include path used when searching for files.
This should be a list of directories to search which is specific to
the file being included.
This variable can also be set to a single function.  If it is a
function, it will be called with one arguments, the file to find as a
string, and  it should return the full path to that file, or nil.")
(make-variable-buffer-local `semantic-inc-include-path)

(defun semantic-inc-find (semantic-include-token &optional buffer)
  "Find the file specified in SEMANTIC-INCLUDE-TOKEN.
Depends on `semantic-in-include-path' for searching.  Always searches
`.' first, then searches additional paths.
Optional argument BUFFER specifies the buffer the token came from."
  (save-excursion
    (if buffer (set-buffer buffer))
    (let ((name (semantic-token-name semantic-include-token)))
      (cond ((file-exists-p name)
	     (expand-file-name name))
	    ((and (symbolp semantic-inc-include-path)
		  (fboundp semantic-inc-include-path))
	     (funcall semantic-inc-include-path name))
	    (t
	     (let ((p semantic-inc-include-path)
		   (found nil))
	       (while (and p (not found))
		 (if (file-exists-p (concat (car p) "/" name))
		     (setq found (concat (car p) "/" name)))
		 (setq p (cdr p)))
	       found))))))


(provide 'semantic-inc)

;;; semantic-inc.el ends here
