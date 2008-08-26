;; -*- emacs-lisp -*-
;; This program has to be run from the directory it is currently in and
;; the rdebug code has to be in the parent directory
(load-file "./elk-test.el")

;; FIXME? Should we use "require 'rdebug" here.
;; Would have to prepend . to load-path. 
(load-file "../rdebug.el")
(load-file "../rdebug-core.el")

(make-variable-buffer-local 'gud-rdebug-marker-acc)

(defun regexp-breakpoint-test (location-str pos-str enabled-str file-str line-str)
  "Test to see that location-str parses rdebug-breakpoint-regexp properly"
  (assert-equal 0 (string-match rdebug-breakpoint-regexp location-str))
  (assert-equal pos-str
		(substring location-str (match-beginning 1)  (match-end 1)))
  (assert-equal enabled-str
		(substring location-str (match-beginning 2)  (match-end 2)))
  (assert-equal file-str
		(substring location-str (match-beginning 3)  (match-end 3)))
  (assert-equal line-str
		(substring location-str (match-beginning 4)  (match-end 4)))
  )
(deftest "rdebug-regexp-breakpoint-test"

  (regexp-breakpoint-test 
   "  1 y   at gcd.rb:6"
   "1" "y" "gcd.rb" "6"
   )
  (regexp-breakpoint-test 
   "  1 y   at gcd.rb:6 if 1 == a"
   "1" "y" "gcd.rb" "6"
   )
  )

(defun regexp-file-test (location-str file-str)
  "Test to see that location-str matches gud-rdebug-marker-regexp"
  (assert-equal 0 (string-match gud-rdebug-marker-regexp location-str))
  (assert-equal file-str
		(substring location-str (match-beginning 1)  (match-end 1)))
  )
(deftest "rdebug-regexp-file-test"

  (regexp-file-test 
   "\032\032./hanoi.rb:3\n"
   "./hanoi.rb"
   )
  (regexp-file-test 
   "\032\032source ./hanoi.rb:3\n"
   "./hanoi.rb"
   )
  (regexp-file-test 
   "\032\032C:/tmp/gcd.rb:29\n"
   "C:/tmp/gcd.rb"
   )
  (regexp-file-test 
   "\032\032source \\sources\\capfilterscanner\\capanalyzer.rb:3:  <module>\n"
   "\\sources\\capfilterscanner\\capanalyzer.rb"
   )
  )

(deftest "rdebug-regexp-marker-filter-test"
  (assert-equal "Testing 1 2 3" (gud-rdebug-marker-filter "Testing 1 2 3"))
  (assert-equal "ABC" (gud-rdebug-marker-filter 
                       "\
breakpoints
No breakpoints

ABC")))

(defun regexp-stack-test (location-str pos-str file-str line-str)
  "Test to see that location-str parses rdebug-stack-frame-regexp properly"
  (assert-equal 0 (string-match rdebug-stack-frame-regexp location-str))
  (assert-equal pos-str
		(substring location-str (match-beginning 2)  (match-end 2)))
  (assert-equal file-str
		(substring location-str (match-beginning 4)  (match-end 4)))
  (assert-equal line-str
		(substring location-str (match-beginning 5)  (match-end 5)))
  )
(deftest "rdebug-regexp-stack-test"

  (regexp-stack-test 
   "--> #0 at line /home/rocky/ruby/gcd.rb:18"
   "0" "/home/rocky/ruby/gcd.rb" "18"
   )
  )

(defun regexp-traceback-test (location-str file-str line-str)
  "Test to see that location-str matches position-regexp-file-test with the correct
file and line submatches."
  (assert-equal 0 (string-match rdebug-traceback-line-re location-str))
  (assert-equal file-str (match-string 1 location-str))
  (assert-equal line-str (match-string 2 location-str))
  )

(deftest "rdebug-regexp-traceback-test"

  (regexp-traceback-test 
   "	from /home/rocky/ruby/gcd.rb:15:in `gcd'"
   "/home/rocky/ruby/gcd.rb" "15"
   )
  (regexp-traceback-test 
   "	from /home/rocky/ruby/gcd.rb:19"
   "/home/rocky/ruby/gcd.rb" "19"
   )
  )
   
(defun regexp-unittest-traceback-test (location-str file-str line-str)
  "Test to see that location-str matches position-regexp-file-test with the correct
file and line submatches."
  (assert-equal 0 (string-match rdebug-dollarbang-traceback-line-re 
				location-str))
  (assert-equal file-str (match-string 1 location-str))
  (assert-equal line-str (match-string 2 location-str))
  )

(deftest "rdebug-regexp-unittest-traceback-test"

  (regexp-unittest-traceback-test 
   "    [test-frame.rb:26:in `test_basic'"
   "test-frame.rb" "26"
   )
  (regexp-unittest-traceback-test 
   "     test-frame.rb:22:in `test_basic']:"
   "test-frame.rb" "22"
   )
  )

;; -------------------------------------------------------------------
;; Build and run the test suite.
;;

(build-suite "rdebug-suite" 
	     "rdebug-regexp-breakpoint-test" 
	     "rdebug-regexp-file-test" 
	     "rdebug-regexp-marker-filter-test" 
	     "rdebug-regexp-stack-test"
	     "rdebug-regexp-traceback-test" 
	     "rdebug-regexp-unittest-traceback-test")

(run-elk-test "rdebug-suite"
              "test regular expressions used in tracking lines")
