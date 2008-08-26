;; -*- emacs-lisp -*-
;; This program has to be run from the directory it is currently in and
;; the rdebug code has to be in the parent directory
(load-file "./elk-test.el")

(setq load-path (cons ".." load-path))
(require 'rdebug-core)
(require 'rdebug-gud)
(setq load-path (cdr load-path))

(defvar last-gud-call nil
  "Value of the last gud-call")

;; Redefine functions to make them harmless for testing
(defun gud-call (command)
  (setq last-gud-call command))

(defun rdebug-call (command)
  (setq last-gud-call command))

(deftest "rdebug-goto-frame-test"
  (let ((buf (generate-new-buffer "testing")))
    (save-excursion
      (switch-to-buffer buf)
      (insert "#0 ERB.result(b#Binding) at line /usr/lib/ruby/1.8/erb.rb:736\n")
      (insert "#1 Listings.build at line erbtest.rb:24\n")
      (insert "#2 at line erbtest.rb:33\n")
      (insert "#10 Listings.build at line erbtest.rb:23")
      (goto-char (point-min))
      (setq last-gud-call nil)
      (setq rdebug-goto-entry-acc "")

      ;; --------------------
      ;; The tests

      (rdebug-goto-frame-n-internal "5")
      (assert-equal nil last-gud-call)
      (rdebug-goto-frame-n-internal "1")
      (assert-equal "frame 1" last-gud-call)
      (rdebug-goto-frame-n-internal "0")
      (assert-equal "frame 10" last-gud-call))
    (kill-buffer buf)))


;; -------------------------------------------------------------------
;; Check breakpoint toggle commands
;;

(deftest "rdebug-toggle-breakpoints"
  (let ((buf (generate-new-buffer "*rdebug-breakpoint-test.rb*"))
        ;; Needed by `rdebug-breakpoint-parse-and-update-cache'.
        (gud-comint-buffer (current-buffer)))
    (save-excursion
      (switch-to-buffer buf)
      (insert "Num Enb What\n")
      (insert "  1 y   at /test.rb:10\n")
      (insert "  2 n   at /test.rb:11\n")
      (insert "  3 y   at /test.rb:12\n")
      (insert "  4 y   at /test.rb:13\n")
      (rdebug-breakpoint-parse-and-update-cache))
    (setq gud-target-name "test.rb")

    ;; ----------
    ;; Toggle break point
    (assert-equal 4 (length (rdebug-all-breakpoints)))

    ;; ----------
    ;; Toggle break point

    ;; Add new.
    (rdebug-toggle-source-breakpoint "/test.rb" 20)
    (assert-equal "break /test.rb:20" last-gud-call)
    ;; Delete enabled.
    (rdebug-toggle-source-breakpoint "/test.rb" 10)
    (assert-equal "delete 1" last-gud-call)
    ;; Delete disabled.
    (rdebug-toggle-source-breakpoint "/test.rb" 11)
    (assert-equal "delete 2" last-gud-call)

    ;; ----------
    ;; Toggle enable/disable.

    ;; Add new.
    (rdebug-toggle-source-breakpoint-enabled "/test.rb" 30)
    (assert-equal "break /test.rb:30" last-gud-call)

    ;; Toggle enabled.
    (rdebug-toggle-source-breakpoint-enabled "/test.rb" 10)
    (assert-equal "disable 1" last-gud-call)
    ;; Toggle disabled.
    (rdebug-toggle-source-breakpoint-enabled "/test.rb" 11)
    (assert-equal "enable 2" last-gud-call)))


;; -------------------------------------------------------------------
;; Check rdebug-next with prefix toggling commands
;;
(deftest "rdebug-stepping-test"
  (setq rdebug-stepping-prefix "")
  (assert-equal "next 1" (rdebug-next))
  (setq rdebug-stepping-prefix "-")
  (assert-equal "next- 2" (rdebug-next 2))
  (setq rdebug-stepping-prefix "+")
  (assert-equal "step+ 1" (rdebug-step))
  )

;; -------------------------------------------------------------------
;; Build and run the test suite.
;;

(build-suite "rdebug-cmd-suite"
	     "rdebug-goto-frame-test"
	     "rdebug-stepping-test"
	     "rdebug-toggle-breakpoints")
(run-elk-test "rdebug-cmd-suite"
              "test some rdebug-core code")
