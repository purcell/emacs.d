;;; cedet-utests.el --- Run all unit tests in the CEDET suite.

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cedet-utests.el,v 1.3 2008/12/01 20:35:20 zappo Exp $

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
;; Remembering to run all the unit tests available in CEDET one at a
;; time is a bit time consuming.  This links all the tests together
;; into one command.

(require 'cedet)
;;; Code:
(defvar cedet-utest-test-alist
  '(
    ;; Test inversion
    ("inversion" . inversion-unit-test)

    ;; EZ Image dumping.
    ("ezimage associations" . ezimage-image-association-dump)
    ("ezimage images" . ezimage-image-dump)

    ;; WORKGING interactive tests.
    ("working: wait-for-keypress" . working-wait-for-keypress)
    ("working: sleep" . working-verify-sleep)

    ;; PULSE
    ("pulse interactive test" . pulse-test)

    ;; The EIEIO unit test suite.
    ("eieio" . (lambda () (let ((lib (locate-library "eieio-tests.el"
						     t)))
			    (load-file lib))))
    ("eieio: browser" . eieio-browse)
    ("eieio: custom" . (lambda ()
			 (require 'eieio-custom)
			 (customize-variable 'eieio-widge-test)))
    ("eieio: chart" . chart-test-it-all)

    ;; SEMANTIC tests
    ("semantic: lex spp table write" .
     (lambda ()
       (let* ((sem (locate-library "semantic.el"))
	      (dir (file-name-directory sem)))
       (save-excursion
	 (set-buffer (find-file-noselect
		      (expand-file-name "tests/testsppreplace.c"
					dir)))
	 (semantic-lex-spp-write-test)))))
    ("semantic: multi-lang parsing" . semantic-utest-main)
    ("semantic: C preprocessor" . semantic-utest-c)
    ("semantic: analyzer tests" . semantic-ia-utest)
    ("semanticdb: data cache" . semantic-test-data-cache)
    ("semantic: throw-on-input" . semantic-test-throw-on-input)

    ;; SRecode
    ("srecode: templates" . srecode-utest-template-output)
    ("srecode: show maps" . srecode-get-maps)
    ("srecode: getset" . srecode-utest-getset-output)
   )
  "Alist of all the ttests in CEDET we should run.")

;;;###autoload
(defun cedet-utest ()
  "Run the CEDET unittests."
  (interactive)
  (cedet-utest-log-setup)
  (let ((tl cedet-utest-test-alist)
	(notes nil)
	(err nil))
    (dolist (T tl)
      (cedet-utest-add-log-item-start (car T))
      (setq notes nil err nil)
      (condition-case Cerr
	  (progn
	    (funcall (cdr T))
	    )
	(error
	 (setq err (format "ERROR: %S" Cerr))))
      (cedet-utest-add-log-item-done notes err)
      ))
  (cedet-utest-add-log-item-done "All Tests Complete" nil t))

;;; Logging utility.
;;
(defvar cedet-utest-frame nil
  "Frame used during cedet unit test logging.")
(defvar cedet-utest-buffer nil
  "Frame used during cedet unit test logging.")
(defvar cedet-utest-frame-parameters
  '((name . "CEDET-UTEST")
    (width . 80)
    (height . 25)
    (minibuffer . t))
  "Frame parameters used for the cedet utest log frame.")

(defvar cedet-utest-last-log-item nil
  "Remember the last item we were logging for.")

(defun cedet-utest-log-setup ()
  "Setup a frame and buffer for unit testing."
  (when (or (not cedet-utest-frame) (not (frame-live-p cedet-utest-frame)))
    (setq cedet-utest-frame (make-frame cedet-utest-frame-parameters)))
  (when (or (not cedet-utest-buffer) (not (buffer-live-p cedet-utest-buffer)))
    (setq cedet-utest-buffer (get-buffer-create "*CEDET utest log*")))
  (save-excursion
    (set-buffer cedet-utest-buffer)
    (setq cedet-utest-last-log-item nil)
    (erase-buffer)
    (insert "Setting up tests to run @ " (current-time-string) "\n\n"))
  (let ((oframe (selected-frame)))
    (unwind-protect
	(progn
	  (select-frame cedet-utest-frame)
	  (switch-to-buffer cedet-utest-buffer t))
      (select-frame oframe)))
  )

(defun cedet-utest-show-log-end ()
  "Show the end of the current unit test log."
  (let* ((cb (current-buffer))
	 (cf (selected-frame))
	 (bw (get-buffer-window cedet-utest-buffer t))
	 (lf (window-frame bw))
	 )
    (select-frame lf)
    (select-window bw)
    (goto-char (point-max))
    (select-frame cf)
    (set-buffer cb)
    ))

(defun cedet-utest-post-command-hook ()
  "Hook run after the current log command was run."
    (save-excursion
      (set-buffer cedet-utest-buffer)
      (goto-char (point-max))
      (insert "\n\n"))
    (setq cedet-utest-last-log-item nil)
    (remove-hook 'post-command-hook 'cedet-utest-post-command-hook)
    )

(defun cedet-utest-add-log-item-start (item)
  "Add ITEM into the log as being started."
  (unless (equal item cedet-utest-last-log-item)
    (setq cedet-utest-last-log-item item)
    ;; This next line makes sure we clear out status during logging.
    (add-hook 'post-command-hook 'cedet-utest-post-command-hook)
    (save-excursion
      (set-buffer cedet-utest-buffer)
      (goto-char (point-max))
      (when (not (bolp)) (insert "\n"))
      (insert "Running " item " ... ")
      (sit-for 0)
      )
    (cedet-utest-show-log-end)
    ))

(defun cedet-utest-add-log-item-done (&optional notes err precr)
  "Add into the log that the last item is done.
Apply NOTES to the doneness of the log.
Apply ERR if there was an error in previous item.
Optional argument PRECR indicates to prefix the done msg w/ a newline."
  (save-excursion
    (set-buffer cedet-utest-buffer)
    (goto-char (point-max))
    (when precr (insert "\n"))
    (if err
	(insert err)
      (insert "done")
      (when notes (insert " (" notes ")")))
    (insert "\n")
    (setq cedet-utest-last-log-item nil)
    (sit-for 0)
    ))

;;; INDIVIDUAL TEST API
;;
;; Use these APIs to start and log information.
;;
;; The other fcns will be used to log across all the tests at once.
(defun cedet-utest-log-start (testname)
  "Setup the log for the test TESTNAME."
  ;; Make sure we have a log buffer.
  (when (or (not cedet-utest-buffer)
	    (not (buffer-live-p cedet-utest-buffer))
	    (not (get-buffer-window cedet-utest-buffer t))
	    )
    (cedet-utest-log-setup))
  ;; Add our startup message.
  (cedet-utest-add-log-item-start testname)
  )

(defun cedet-utest-log(format &rest args)
  "Log the text string FORMAT.
The rest of the ARGS are used to fill in FORMAT with `format'."
  (save-excursion
    (set-buffer cedet-utest-buffer)
    (goto-char (point-max))
    (when (not (bolp)) (insert "\n"))
    (insert (apply 'format format args))
    (insert "\n")
    (sit-for 0)
    )
  (cedet-utest-show-log-end)
  )


(provide 'cedet-utests)
;;; cedet-utests.el ends here
