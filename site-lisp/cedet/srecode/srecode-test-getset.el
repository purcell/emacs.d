;;; srecode-test-getset.el --- Test the getset inserter.

;; Copyright (C) 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: srecode-test-getset.el,v 1.4 2009/01/24 03:38:33 zappo Exp $

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
;; Unit tests for the getset inserter application.

(require 'cedet-utests)

;;; Code:
(defvar srecode-utest-getset-pre-fill
  "// Test Class for getset tests in c++.

class myClass {
public:
  myClass() { };
  ~myClass() { };
  /** miscFunction
   */
  int miscFunction(int);

private:
  int fStartingField;

};

"
  "The pre-fill class for the getset tests.")


;;; Master Harness
;;
(defvar srecode-utest-getset-testfile "/tmp/srecode-utest-getset.cpp"
  "File used to do testing.")

;;;###autoload
(defun srecode-utest-getset-output ()
  "Test various template insertion options."
  (interactive)

  (cedet-utest-log-setup "SRECODE Get/Set")

  (save-excursion
    (let ((testbuff (find-file-noselect srecode-utest-getset-testfile))
	  (srecode-insert-getset-fully-automatic-flag t))

      (set-buffer testbuff)

      (srecode-load-tables-for-mode major-mode)
      (srecode-load-tables-for-mode major-mode 'getset)

      (if (not (srecode-table))
	  (error "No template table found for mode %s" major-mode))

      (condition-case nil
	  (erase-buffer)
	(error nil))
      (insert srecode-utest-getset-pre-fill)
      (goto-char (point-min))
      (cedet-utest-log " * Test Pre-fill")
      (srecode-utest-getset-tagcheck '("public"
				       "myClass"
				       "myClass"
				       "miscFunction"
				       "private"
				       "fStartingField"))
      (srecode-utest-getset-jumptotag "fStartingField")

      ;; Startup with fully automatic selection.
      (srecode-insert-getset)

      (cedet-utest-log " * Post get-set \"StartingField\"")
      (srecode-utest-getset-tagcheck '("public"
				       "myClass"
				       "myClass"
				       "getStartingField"
				       "setStartingField"
				       "miscFunction"
				       "private"
				       "fStartingField"))

      ;; Now try convenience args.
      (goto-char (point-min))
      (srecode-utest-getset-jumptotag "fStartingField")
      (end-of-line)
      (insert "\n")

      (srecode-insert-getset nil "AutoInsertField")

      (cedet-utest-log " * Post get-set \"AutoInsertField\"")
      (srecode-utest-getset-tagcheck '("public"
				       "myClass"
				       "myClass"
				       "getStartingField"
				       "setStartingField"
				       "getAutoInsertField"
				       "setAutoInsertField"
				       "miscFunction"
				       "private"
				       "fStartingField"
				       "fAutoInsertField"))

      ;; Make sure all the comments are in the right place.
      (srecode-utest-getset-jumptotag "miscFunction")
      (let ((pos (point)))
	(skip-chars-backward " \t\n") ; xemacs forward-comment is different.
	(forward-comment -1)
	(re-search-forward "miscFunction" pos))
      
      ))
  (cedet-utest-log-shutdown
   "SRECODE Get/Set"
   nil ; How to detect a problem?
   )
  )

(defun srecode-utest-getset-tagcheck (expected-members)
  "Make sure that the tags in myClass have EXPECTED-MEMBERS."
  (semantic-fetch-tags)
  (let* ((mc (semantic-find-tags-by-name "myClass" (current-buffer)))
	 (mem (semantic-tag-type-members (car mc))))
    (while (and mem expected-members)
      (when (not (string= (semantic-tag-name (car mem))
			  (car expected-members)))
	(switch-to-buffer (current-buffer))
	(error "Did not find %s" (car expected-members)))
      (setq mem (cdr mem)
	    expected-members (cdr expected-members)))
    (when expected-members
      (switch-to-buffer (current-buffer))
      (error "Did not find all expected tags in class"))
    (when mem
      (switch-to-buffer (current-buffer))
      (error "Found extra tags in class"))))

(defun srecode-utest-getset-jumptotag (tagname)
  "Jump to the tag named TAGNAME."
  (semantic-fetch-tags)
  (let ((tag (semantic-deep-find-tags-by-name tagname (current-buffer))))
    (if tag
	(semantic-go-to-tag (car tag))
      (error "Failed to jump to tag %s" tagname))))

(provide 'srecode-test-getset)
;;; srecode-test-getset.el ends here
