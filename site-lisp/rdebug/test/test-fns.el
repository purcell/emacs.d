;; -*- emacs-lisp -*-
;; This program has to be run from the directory it is currently in and
;; the rdebug code has to be in the parent directory
(load-file "./elk-test.el")

(setq load-path (cons ".." load-path))
(require 'rdebug-fns)
(require 'rdebug-locring)
(setq load-path (cdr load-path))

;; -------------------------------------------------------------------

(deftest "test-add-to-ring"
  (let ((location-ring (make-ring 5)))
    (assert-equal t (ring-empty-p location-ring))
    (rdebug-locring-add 'first location-ring)
    (assert-equal 'first (ring-ref location-ring 0))
    (assert-equal 1 (ring-length location-ring))
    ;; Trying to add the same entry should not again.
    (rdebug-locring-add 'first location-ring)
    (assert-equal 1 (ring-length location-ring))

    ;; Second should go in as last item.
    (rdebug-locring-add 'second location-ring)
    (assert-equal 'second (ring-ref location-ring 1))
    ;; First item is still 0.
    (assert-equal 'first  (ring-ref location-ring 0))))

(deftest "test-chomp"
  (assert-equal "" (chomp ""))
  (assert-equal "hi" (chomp "hi"))
  (assert-equal "hi" (chomp "hi\n"))
  (assert-equal "hi\n" (chomp "hi\n\n"))
  (assert-equal "hi" (chomp "hi\n\n" t)))

(deftest "test-set-frame-arrow"
  (let ((rdebug-frames-current-frame-number 0))
    (rdebug-set-frame-arrow (current-buffer))
    (assert-equal '((overlay-arrow . right-triangle))
		  fringe-indicator-alist)
    (setq rdebug-frames-current-frame-number 1)
    (rdebug-set-frame-arrow (current-buffer))
    (assert-equal '((overlay-arrow . hollow-right-triangle))
		  fringe-indicator-alist)))

(require 'shell)
(deftest "test-dead-process-p"
  (assert-equal t (rdebug-dead-process-p))
  (let ((gud-comint-buffer nil))
    (assert-equal t (rdebug-dead-process-p))
    (setq gud-comint-buffer (shell))
    (assert-equal nil (rdebug-dead-process-p))))

;; -------------------------------------------------------------------
;; Build and run the test suite.
;;

(build-suite "rdebug-gud-suite"
	     "test-add-to-ring"
	     "test-chomp"
	     "test-dead-process-p"
	     "test-set-frame-arrow")

(run-elk-test "rdebug-gud-suite"
              "test some rdebug-error code")
