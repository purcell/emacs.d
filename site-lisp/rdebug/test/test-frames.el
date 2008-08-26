;; -*- emacs-lisp -*-
;; This program has to be run from the directory it is currently in and
;; the rdebug code has to be in the parent directory
(load-file "./elk-test.el")

;; FIXME? Should we use "require 'rdebug" here.
;; Would have to prepend . to load-path. 
(load-file "../rdebug.el")
(load-file "../rdebug-regexp.el")
(load-file "../rdebug-frames.el")

(make-variable-buffer-local 'gud-rdebug-marker-acc)

(deftest "rdebug-stack-buffer-field-test"
  (let ((buf (generate-new-buffer "testing")))
    (save-excursion
      (switch-to-buffer buf)
      (insert 
       "--> #0 Object.gcd(a#Fixnum, b#Fixnum) at line /tmp/gcd.rb:4\n")
      (insert
       "       at line /foo/bar/custom_require.rb:27\n")

      (goto-char (point-min))
      (let* ((b (line-beginning-position)) (e (line-end-position))
	     (s (buffer-substring b e))
	     (file nil) (line nil))
	(assert-nonnil (string-match rdebug-stack-frame-regexp s))
	(assert-equal "/tmp/gcd.rb" (rdebug-stack-buffer-field
				     s b
				     rdebug-stack-frame-file-group
				     font-lock-comment-face))
	(assert-equal "4" (rdebug-stack-buffer-field
			   s b
			   rdebug-stack-frame-line-group
			   font-lock-constant-face))
	(forward-line)
	(setq b (line-beginning-position))
	(setq e (line-end-position))
	(setq s (buffer-substring b e))
	(assert-nonnil (string-match rdebug-stack-frame-2nd-regexp s))
	(assert-equal "/foo/bar/custom_require.rb" 
		      (rdebug-stack-buffer-field
		       s b
		       rdebug-stack-frame-2nd-file-group
		       font-lock-comment-face))
	(assert-equal "27" (rdebug-stack-buffer-field
			    s b
			    rdebug-stack-frame-2nd-line-group
			    font-lock-constant-face))
	))
    (kill-buffer buf)))


;; -------------------------------------------------------------------
;; Build and run the test suite.
;;

(build-suite "rdebug-suite" 
	     "rdebug-stack-buffer-field-test")
(run-elk-test "rdebug-suite"
              "test things in rdebug-frames.el")

