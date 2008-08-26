;; -*- emacs-lisp -*-
;; This program has to be run from the directory it is currently in and
;; the rdebug code has to be in the parent directory
(load-file "./elk-test.el")

;; FIXME? Should we use "require 'rdebug" here.
;; Would have to prepend . to load-path. 
(load-file "../rdebug.el")
(load-file "../rdebug-annotate.el")

(defvar last-annotation nil
  "Value of the last annotation processed")

;; Redefine functions to make them harmless for testing
(defun rdebug-process-annotation (name contents)
  (setq last-annotation name))

(make-variable-buffer-local 'gud-rdebug-marker-acc)

;; -------------------------------------------------------------------
;; Test harness for testing the filter.
;;

(require 'advice)

(defvar rdebug-test-cmd-list '())

;; Override, partially because tooltip-show doesn't work in batch
;; mode, and partially because we collect the output here.
(defun tooltip-show (text)
  (setq rdebug-test-cmd-list (cons text rdebug-test-cmd-list)))

(defun assert-filter (output str &optional cmd-list)
  (setq rdebug-test-cmd-list '())
  (setq gud-marker-acc "")
  (let ((orig-queue rdebug-call-queue))
    (let ((real-output (gud-rdebug-marker-filter str)))
      (assert-equal output real-output)
      (assert-equal cmd-list (reverse rdebug-test-cmd-list)))

    ;;
    ;; Feed the filter one character at a time -- the end result should
    ;; be the same.
    ;;
    (setq rdebug-test-cmd-list '())
    (setq gud-marker-acc "")
    (let ((real-output "")
          (len (length str))
          (i 0)
          (rdebug-call-queue orig-queue))
      (while (< i len)
        (setq real-output
              (concat real-output
                      (gud-rdebug-marker-filter
                       (substring str i (if (equal (+ 1 i) len)
                                            nil
                                          (+ 1 i))))))
        (setq i (+ 1 i)))
      (assert-equal output real-output)
      (assert-equal cmd-list (reverse rdebug-test-cmd-list)))))


(deftest "rdebug-filter"
;;;   (assert-filter "X" "X")
;;;   (assert-filter "XYZ" "XYZ")
;;;   (assert-filter "" "\n")
;;;   (assert-filter "Testing 1 2 3" "Testing 1 2 3")
;;;   (assert-filter "Testing 1 2 3" "Testing 1 2 3")
;;;   (assert-filter "ABC" "\
;;; breakpoints
;;; No breakpoints
;;; 
;;; ABC")

  ;; Some systems (read: Mac) echoes the command.
  (setq rdebug-call-queue '(("pp 100" :tooltip)))
  (assert-filter "pp 100\n100\n(rdb:1) " "\
prompt
pp 100\n100
pre-prompt
\(rdb:1) \nprompt\n"
                 '("100\n"))

  ;; Some systems don't echo the command.
  (setq rdebug-call-queue '(("pp 100" :tooltip)))
  (assert-filter "100\n(rdb:1) " "\
prompt
100
pre-prompt
\(rdb:1) \nprompt\n"
                 '("100\n"))
  )


;; -------------------------------------------------------------------
;; Build and run the test suite.
;;

(build-suite "rdebug-suite" 
             "rdebug-filter")

(run-elk-test "rdebug-suite"
              "test regular expressions used in tracking lines")
