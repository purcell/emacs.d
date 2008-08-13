;;; semantic-ia-utest.el --- Analyzer unit tests

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semantic-ia-utest.el,v 1.12 2008/07/01 21:09:16 zappo Exp $

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
;; Use marked-up files in the test directory and run the analyzer
;; on them.  Make sure the answers are correct.
;;
;; Each file has cursor keys in them of the form:
;;   // -#- ("ans1" "ans2" )
;; where # is 1, 2, 3, etc, and some sort of answer list.

;;; Code:
(require 'semantic)

(defvar semantic-ia-utest-file-list
  '( "tests/testdoublens.cpp"
     "tests/testsubclass.cpp"
     "tests/testtypedefs.cpp"
     "tests/testfriends.cpp"
     )
  "List of files with analyzer completion test points.")

;;;###autoload
(defun semantic-ia-utest ()
  "Run the semantic ia unit test against stored sources."
  (interactive)
  (save-excursion

    (set-buffer (find-file-noselect
		 (locate-library "semantic-ia-utest.el")))

    (let ((fl semantic-ia-utest-file-list))

      (semantic-ia-utest-start-log)

      (while fl

	(let ((fb (find-buffer-visiting (car fl)))
	      (b (find-file-noselect (car fl) t)))

	  ;; Run the test on it.
	  (save-excursion
	    (set-buffer b)
	    (semantic-ia-utest-buffer))

	  ;; If it wasn't already in memory, whack it.
	  (when (not fb)
	    (kill-buffer b))
	  )
	(setq fl (cdr fl)))))
  (pop-to-buffer "*UTEST LOG*" t t)
  (goto-char (point-max))
  (search-backward "semantic-ia-utest")
  (beginning-of-line)
  (recenter 1)
  )

(defun semantic-ia-utest-buffer ()
  "Run a unit-test pass in the current buffer."
  (interactive)

  ;; This line will also force the include, scope, and typecache.
  (semantic-clear-toplevel-cache)
  ;; Force tags to be parsed.
  (semantic-fetch-tags)

  (let* ((idx 1)
	 (regex-p nil)
	 (regex-a nil)
	 (p nil)
	 (a nil)
	 (pass nil)
	 (fail nil)
	 (actual nil)
	 (desired nil)
	 ;; Exclude unpredictable system files in the
	 ;; header include list.
	 (semanticdb-find-default-throttle
	  (remq 'system semanticdb-find-default-throttle))
	 )
    ;; Keep looking for test points until we run out.
    (while (save-excursion
	     (setq regex-p (concat "//\\s-*-" (number-to-string idx) "-" )
		   regex-a (concat "//\\s-*#" (number-to-string idx) "#" ))
	     (goto-char (point-min))
	     (save-match-data
	       (when (re-search-forward regex-p nil t)
		 (setq p (match-beginning 0))))
	     (save-match-data
	       (when (re-search-forward regex-a nil t)
		 (setq a (match-end 0))))
	     (and p a))

      (save-excursion

	(goto-char p)
	
	(let* ((ctxt (semantic-analyze-current-context))
	       (acomp
		(condition-case nil
		    (semantic-analyze-possible-completions ctxt)
		  (error nil))))
	  (setq actual (mapcar 'semantic-tag-name acomp)))
	
	(goto-char a)

	(let ((bss (buffer-substring-no-properties (point) (point-at-eol))))
	  (condition-case nil
	      (setq desired (read bss))
	    (error (setq desired (format "FAILED TO PARSE: %S"
					 bss)))))

	(if (equal actual desired)
	    (setq pass (cons idx pass))
	  (setq fail (cons idx fail))
	  (semantic-ia-utest-log
	   "Failed %d.  Desired: %S Actual %S"
	   idx desired actual)
	  )
	)

      (setq p nil a nil)
      (setq idx (1+ idx)))
    
    (if fail
	(progn
	  (semantic-ia-utest-log
	   "Unit tests in %s failed tests %S"
	   (buffer-name) (reverse fail))
	  )
      (semantic-ia-utest-log "Unit tests in %s passed (%d total)"
			     (buffer-name)
			     (- idx 1)))

    ))

(defun semantic-ia-utest-start-log ()
  "Start up a testlog for a run."
  (let ((b (get-buffer-create "*UTEST LOG*"))
	)
    (save-excursion
      (set-buffer b)
      (goto-char (point-max))
      (insert "\n\nsemantic-ia-utest log at ")
      (insert (current-time-string))
      (insert "\n\n"))))

(defun semantic-ia-utest-log (&rest args)
  "Log some test results.
Pass ARGS to format to create the log message."
  (let ((b (get-buffer-create "*UTEST LOG*"))
	)
    (set-buffer b)
    (goto-char (point-max))
    (insert (apply 'format args))
    (insert "\n")
    ))

(provide 'semantic-ia-utest)
;;; semantic-ia-utest.el ends here
