;; -*- emacs-lisp -*-
;; This program has to be run from the directory it is currently in and
;; the rdebug code has to be in the parent directory
(load-file "./elk-test.el")

;; FIXME? Should we use "require 'rdebug" here.
;; Would have to prepend . to load-path. 
(load-file "../rdebug.el")
(load-file "../rdebug-core.el")

;; Redefine functions to make them harmless for testing
(defun rdebug-process-annotation (name contents)
  (message name)
  )

(make-variable-buffer-local 'gud-rdebug-marker-acc)

(deftest "rdebug-get-script-name-test"
  (assert-equal '("foo" nil) (rdebug-get-script-name '("rdebug" "foo")))
  (assert-equal '("foo" nil) (rdebug-get-script-name '("rdebug" "-m" "foo")))
  (assert-equal '("foo" t) (rdebug-get-script-name
                            '("rdebug" "--emacs" "3" "foo")))
  (assert-equal '("foo" t) (rdebug-get-script-name
                            '("myrdebug" "--annotate=1" "foo")))
  (assert-equal '("foo" t) (rdebug-get-script-name
                            '("ruby" "rdebug" "--annotate" "1" "foo")))
  (assert-equal '("foo" nil) (rdebug-get-script-name
			      '("/usr/bin/ruby19" "rdebug" "--emacs-basic" "foo")))
  (assert-equal '("foo" nil) (rdebug-get-script-name
			      '("rdbg.rb" "foo")))
  (assert-equal '("rdbg.rb" nil) (rdebug-get-script-name
				  '("rdebug" "rdbg.rb" "foo")))
  (assert-equal '("foo" t) (rdebug-get-script-name '("rdebug" "-A" "1" "foo")))
  (assert-equal '("foo" nil)
		(rdebug-get-script-name
		 '("rdebug" "--include" "me" "-n" "foo")))
  (assert-equal '("foo" nil) (rdebug-get-script-name
			      '("rdebug" "--server" "-d" "--host"
				"localhost" "foo" "-1")))
  )

(deftest "rdebug-goto-entry-test"
  (let ((buf (generate-new-buffer "testing")))
    (save-excursion
      (switch-to-buffer buf)
      (insert "#0 at line /tmp/gcd.rb:4\n")
      (goto-char (point-min))
      (assert-equal t (rdebug-goto-entry-try "0"))
      (assert-equal nil (rdebug-goto-entry-try "1"))
      (insert "  1 y   at gcd.rb:10\n")
      (goto-char (point-min))
      ;; Don't know why this doesn't work.
      ;;(assert-equal t (rdebug-goto-entry-try "1"))
      (insert "5: 1 + 2 = 3\n")
      (goto-char (point-min))
      (assert-equal t (rdebug-goto-entry-try "5"))
      (goto-char (point-min))
      (assert-equal nil (rdebug-goto-entry-try "3")))
    (kill-buffer buf)))

(defun rdebug-test-call-entry-n (str)
  "Call `rdebug-goto-entry-n', return the line we landed on."
  (rdebug-goto-entry-n-internal str)
  (beginning-of-line)
  (count-lines (point-min) (point)))

;; The original implementation could not go to "10" if there was no "1" entry.
(deftest "rdebug-goto-entry-test-2"
  (let ((buf (generate-new-buffer "testing")))
    (save-excursion
      (switch-to-buffer buf)
      (insert "#0 at line /tmp/gcd.rb:4\n")
      (insert "#2 at line /tmp/gcd.rb:44\n")
      (insert "#13 at line /tmp/gcd.rb:444\n")
      (goto-char (point-min))
      (setq rdebug-goto-entry-acc "")
      ;; Goto "0"
      (assert-equal 0 (rdebug-test-call-entry-n "0"))
      ;; Goto "2"
      (assert-equal 1 (rdebug-test-call-entry-n "2"))
      ;; There is no "1" or "21" or "021", so stay.
      (assert-equal 1 (rdebug-test-call-entry-n "1"))
      ;; Goto "13"
      (assert-equal 2 (rdebug-test-call-entry-n "3"))
      ;; There is no "5", "35", or "135", so stay.
      (assert-equal 2 (rdebug-test-call-entry-n "5"))
      ;; Goto "0"
      (assert-equal 0 (rdebug-test-call-entry-n "0"))
      ;; Goto "2"
      (assert-equal 1 (rdebug-test-call-entry-n "2")))
    (kill-buffer buf)))


;; -------------------------------------------------------------------
;; Build and run the test suite.
;;

(build-suite "rdebug-suite" 
	     "rdebug-get-script-name-test"
	     "rdebug-goto-entry-test"
	     "rdebug-goto-entry-test-2")
(run-elk-test "rdebug-suite"
              "test things in rdebug-core.el")

