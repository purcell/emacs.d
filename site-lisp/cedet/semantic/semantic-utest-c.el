;;; semantic-utest-c.el --- C based parsing tests.

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semantic-utest-c.el,v 1.2 2008/05/03 14:24:05 zappo Exp $

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
;; Run some C based parsing tests.

(defvar semantic-utest-c-comparisons
  '( ("testsppreplace.c" . "testsppreplaced.c")
     )
  "List of files to parse and compare against eachother.")

;;; Code:
;;;###autoload
(defun semantic-utest-c ()
  "Run parsing test for C from the test directory."
  (interactive)
  (dolist (fp semantic-utest-c-comparisons)
    (let* ((sem (locate-library "semantic"))
	   (sdir (file-name-directory sem))
	   (tags-actual
	    (save-excursion
	      (set-buffer (find-file-noselect (expand-file-name (concat "tests/" (car fp)) sdir)))
	      (semantic-clear-toplevel-cache)
	      (semantic-fetch-tags)))
	   (tags-expected
	    (save-excursion
	      (set-buffer (find-file-noselect (expand-file-name (concat "tests/" (cdr fp)) sdir)))
	      (semantic-clear-toplevel-cache)
	      (semantic-fetch-tags))))
      ;; Now that we have the tags, compare them for SPP accuracy.
      (dolist (tag tags-actual)
	(if (and (semantic-tag-of-class-p tag 'variable)
		 (semantic-tag-variable-constant-p tag))
	    nil; skip the macros.
	  (if (semantic-tag-similar-with-subtags-p tag (car tags-expected))
	      (setq tags-expected (cdr tags-expected))
	    (error "Tag mismatch: %S -- %S"
		   tag (car tags-expected)))
	  ))
      ;; Passed?
      (message "PASSED!")
      )))
  

(provide 'semantic-utest-c)
;;; semantic-utest-c.el ends here
