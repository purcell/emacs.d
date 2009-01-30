;;; srecode-test.el --- SRecode Core Template tests.

;; Copyright (C) 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: srecode-test.el,v 1.8 2009/01/29 03:15:59 zappo Exp $

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
;; Tests of SRecode template insertion routines and tricks.
;;

(require 'srecode-insert)
(require 'srecode-dictionary)
(require 'cedet-utests)

;;; Code:

;;; OUTPUT TESTING
;;
(defclass srecode-utest-output ()
  ((name :initarg :name
	 :documentation "Name of the template tested.")
   (output :initarg :output
	   :documentation "Expected Outupt of the template.")
   (dict-entries :initarg :dict-entries
		 :initform nil
		 :documentation "Extra dictionary entries to specify.")
   (pre-fill :initarg :pre-fill
	     :initform nil
	     :documentation "Text to prefill a buffer with.
Place cursor on the ! and delete it.
If there is a second !, the put the mark there.")
   )
  "A single template test.")

(defmethod srecode-utest-test ((o srecode-utest-output))
  "Perform the insertion and test the output.
Assumes that the current buffer is the testing buffer."
  (erase-buffer)
  
  (insert (or (oref o pre-fill) ""))
  (goto-char (point-min))
  (let ((start nil))
    (when (re-search-forward "!" nil t)
      (goto-char (match-beginning 0))
      (setq start (point))
      (replace-match ""))
    (when (re-search-forward "!" nil t)
      (push-mark (match-beginning 0) t t)
      (replace-match ""))
    (when start (goto-char start)))

  (let* ((dict (srecode-create-dictionary))
	 (temp (srecode-template-get-table (srecode-table)
					   (oref o name)
					   "test"
					   'tests
					   ))
	 (srecode-handle-region-when-non-active-flag t)
	 )
    (when (not temp)
      (srecode-map-update-map)
      (setq temp (srecode-template-get-table (srecode-table)
					     (oref o name)
					     "test"
					     'tests
					     ))
      )
    (when (not temp)
      (error "Test template \"%s\" for `%s' not loaded!"
	     (oref o name) major-mode))

    ;; RESOLVE AND INSERT
    (srecode-resolve-arguments temp dict)
    (let ((entries (oref o dict-entries)))
      (while entries
	(srecode-dictionary-set-value dict
				      (car entries)
				      (car (cdr entries)))
	(setq entries (cdr (cdr entries)))))
    (srecode-insert-fcn temp dict)

    ;; COMPARE THE OUTPUT
    (if (string= (oref o output) (buffer-string))
	(cedet-utest-log " * Entry %s passed." (object-print o))
      
      (goto-char (point-max))
      (insert "\n\n ------------- ^^ actual ^^ ------------\n\n
 ------------- v expected vv ------------\n\n" (oref o output))
      (pop-to-buffer (current-buffer))
      (error "Entry %s failed!" (object-name o)))
  ))

;;; TEST POINTS
;;
(defvar srecode-utest-output-entries
  (list
   (srecode-utest-output
    "test1" :name "test"
    :output (concat ";; " (user-full-name) "\n"
		    ";; " (upcase (user-full-name))) )
   (srecode-utest-output
    "subs" :name "subs"
    :output ";; Before Loop
;; After Loop" )
   (srecode-utest-output
    "firstlast" :name "firstlast"
    :output "
;; << -- FIRST
;; I'm First
;; I'm Not Last
;; -- >>

;; << -- MIDDLE
;; I'm Not First
;; I'm Not Last
;; -- >>

;; << -- LAST
;; I'm Not First
;; I'm Last
;; -- >>
" )
   (srecode-utest-output
    "gapsomething" :name "gapsomething"
    :output ";; First Line
### ALL ALONE ON A LINE ###
;;Second Line"
    :pre-fill ";; First Line
!;;Second Line")
   (srecode-utest-output
    "wrapsomething" :name "wrapsomething"
    :output ";; Put this line in front:
;; First Line
;; Put this line at the end:"
    :pre-fill "!;; First Line
!")
   (srecode-utest-output
    "inlinetext" :name "inlinetext"
    :output ";; A big long comment XX *In the middle* XX with cursor in middle"
    :pre-fill ";; A big long comment XX!XX with cursor in middle")

   (srecode-utest-output
    "wrapinclude-basic" :name "wrapinclude-basic"
    :output ";; An includable  we could use.
;; 
;; Text after a point inserter."
    )
   (srecode-utest-output
    "wrapinclude-basic2" :name "wrapinclude-basic"
    :output ";; An includable MOOSE we could use.
;; 
;; Text after a point inserter."
    :dict-entries '("COMMENT" "MOOSE")
    )
   (srecode-utest-output
    "wrapinclude-around" :name "wrapinclude-around"
    :output ";; An includable  we could use.
;; Intermediate Comments
;; Text after a point inserter."
    )
   (srecode-utest-output
    "wrapinclude-around1" :name "wrapinclude-around"
    :output ";; An includable PENGUIN we could use.
;; Intermediate Comments
;; Text after a point inserter."
    :dict-entries '("COMMENT" "PENGUIN")
    )
   (srecode-utest-output
    "complex-subdict" :name "complex-subdict"
    :output ";; I have a cow and a dog.")
   (srecode-utest-output
    "wrap-new-template" :name "wrap-new-template"
    :output "template newtemplate
\"A nice doc string goes here.\"
----
Random text in the new template
----
bind \"a\""
    :dict-entries '( "NAME" "newtemplate" "KEY" "a" )
    )
   (srecode-utest-output
    "column-data" :name "column-data"
    :output "Table of Values:
Left Justified       | Right Justified
FIRST                |                FIRST
VERY VERY LONG STRIN | VERY VERY LONG STRIN
MIDDLE               |               MIDDLE
S                    |                    S
LAST                 |                 LAST")
   )
  "Test point entries for the template output tests.")



;;; Master Harness
;;
(defvar srecode-utest-testfile "/tmp/srecode-utest.srt"
  "File used to do testing.")

;;;###autoload
(defun srecode-utest-template-output ()
  "Test various template insertion options."
  (interactive)

  (save-excursion
    (let ((testbuff (find-file-noselect srecode-utest-testfile)))

      (set-buffer testbuff)

      (srecode-load-tables-for-mode major-mode)
      (srecode-load-tables-for-mode major-mode 'tests)

      (if (not (srecode-table major-mode))
	  (error "No template table found for mode %s" major-mode))

      ;; Loop over the output testpoints.
      ;;(cedet-utest-log-start "srecode: templates")
      (cedet-utest-log-setup "SRECODE Templates")

      (dolist (p srecode-utest-output-entries)
	(set-buffer testbuff) ;; XEmacs causes a buffer switch.  I don't know why
	(srecode-utest-test p)
	)

      (cedet-utest-log-shutdown 
       "SRECODE Templates"
       nil ; How to detect a problem?
       )
      )))



(provide 'srecode-test)
;;; srecode-test.el ends here
