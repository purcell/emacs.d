;; -*- emacs-lisp -*-
;; This program has to be run from the directory it is currently in and
;; the rdebug code has to be in the parent directory
(load-file "./elk-test.el")

(setq load-path (cons ".." load-path))
(require 'rdebug-gud)
(setq load-path (cdr load-path))

(defun y-or-n-p (prompt)
  "Replacement of y-or-n-p() for rdebug testing"
  (assert-nil "y-or-n-p should not have been called"))

(defun error (msg)
  "Replacement error() for rdebug testing"
  (assert-nil "error should not have been called"))

;; -------------------------------------------------------------------

(deftest "test-rdebug-find-file"
  ;; Set to cause a warning in find-file-no-select and 
  ;; check that it is ignored.
  (let ((large-file-warning-threshold 1)) 
    (gud-rdebug-find-file "elk-test.el")))


;; -------------------------------------------------------------------
;; Build and run the test suite.
;;

(build-suite "rdebug-gud-suite"
	     "test-rdebug-find-file")

(run-elk-test "rdebug-gud-suite"
              "test some rdebug-gud code")
