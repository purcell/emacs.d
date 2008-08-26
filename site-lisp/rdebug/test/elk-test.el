;;; elk-test.el --- Emacs Lisp testing suite

;; Copyright (C) 2006 Nikolaj Schumacher <bugs * nschum , de>

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Usage

;; Use `deftest' to define a test and `run-elk-test' to run it.
;; Create test bundles with `defsuite' or `build-suite'.
;; Verify your code with  `assert-equal', `assert-eq', `assert-eql',
;; `assert-nonnil', `assert-t', `assert-nil' and `assert-error'
;; to verify your code.

;;; Examples

;; (deftest "test1"
;;   (assert-equal t t)
;;   (assert-eq t 'foo))

;; (defsuite "suite1"
;;   (deftest "test1" (assert-equal t t)))

;; (deftest "test2"
;;   (assert-equal t t))
;; (build-suite "combined-suite" "test1" "test2")

;; (run-elk-test "combined-suite")
;; (run-elk-test)

(require 'cl)

(defvar elk-test-run-on-define nil
  "If non-nil, run elk-test tests/suites immediately when defining them.")

(defvar elk-test-map (make-hash-table :test 'equal)
  "A map of elk-test test/suite names to their implementation.")

(defvar elk-test-list nil
  "A list of all defined elk-test tests/suites.")

(defun elk-test-clear ()
  "Remove all tests from memory."
  (setq elk-test-map (make-hash-table :test 'equal)
        elk-test-list nil))

(defun run-elk-test (name &optional string-result)
  "Run the test case defined as NAME.
The result is a list of errors strings, unless STRING-RESULT is set, in which
case a message describing the errors or success is displayed and returned."
  (interactive
   (list (completing-read "Test name: " elk-test-list nil t)))
  (let ((name name))
  (let ((elk-test-errors nil)
        (test-or-suite (gethash name elk-test-map)))
    (if (not test-or-suite)
        (error "Undefined test <%s>" name)
      (if (equal (car test-or-suite) 'suite)
          ;; is test suite
          (let ((map (cadr test-or-suite)))
            (dolist (test (caddr test-or-suite))
              (setq elk-test-errors
                    (append elk-test-errors
                            (run-elk-test-internal (gethash test map))))))
        ;; is simple test
        (setq elk-test-errors (run-elk-test-internal test-or-suite)))
      (if (or string-result (interactive-p))
          (message (if elk-test-errors
                       (mapconcat 'identity elk-test-errors "\n")
                     "Test run was successful."))
        elk-test-errors)))))

(defun run-elk-tests-buffer (&optional buffer)
  "Execute BUFFER as lisp code and run all tests therein."
  (interactive)
  (let* ((elk-test-list)
         (elk-test-map (make-hash-table :test 'equal))
         (elk-test-run-on-define nil)
         (inhibit-read-only t)
         (buffer-name (buffer-name buffer))
         (success t)
         (parse-res (condition-case err (eval-buffer buffer) (error err))))
    (if parse-res
        (message "Parsing buffer <%s> failed:\n%s"
                 buffer-name parse-res)
      (let ((out-buffer (get-buffer-create
                         (concat "*elk-test run " buffer-name "*")))
            failure)
        (with-current-buffer out-buffer
          (erase-buffer)
          (dolist (test elk-test-list)
            (message "running <%s>" test)
            (let ((results (run-elk-test test)))
              (when results
                (setq failure t)
                (insert "test <" test "> failed:\n")
                (dolist (result results)
                  (insert "* " result "\n"))))))
        (if failure
            (display-buffer out-buffer)
          (kill-buffer out-buffer)
          (message "Test run was successful."))))))

(defun run-elk-test-internal (test)
  (let ((elk-test-errors nil))
    (dolist (sexpr test)
      (let ((problem (condition-case err (progn (eval sexpr) nil) (error err))))
        (when problem
          (push (message "%s" problem) elk-test-errors))))
    elk-test-errors))

(defmacro elk-test-error (format-string &rest args)
  "Create an error string as the result of a failed elk-test assertion.
The first argument is a format control string, and the rest are data to be
formatted under control of the string.  See `format' for details.

The result will be displayed, returned and if called inside of `run-elk-test'
added to the internal error list."
  `(let ((string (message ,format-string ,@args)))
     (when (boundp 'elk-test-errors)
       (push string elk-test-errors))
     string))

(defmacro assert-equal (expected actual)
  "Assert that ACTUAL equals EXPECTED, or signal a warning."
  `(unless (equal ,expected ,actual)
    (elk-test-error "assert-equal for <%s> failed: expected <%s>, was <%s>"
                    ',actual ,expected ,actual)))

(defmacro assert-eq (expected actual)
  "Assert that ACTUAL equals EXPECTED, or signal a warning."
  `(unless (eq ,expected ,actual)
    (elk-test-error "assert-eq for <%s> failed: expected <%s>, was <%s>"
                    ',actual ,expected ,actual)))

(defmacro assert-eql (expected actual)
  "Assert that ACTUAL equals EXPECTED, or signal a warning."
  `(unless (eql ,expected ,actual)
    (elk-test-error "assert-eql for <%s> failed: expected <%s>, was <%s>"
                    ',actual ,expected ,actual)))

(defmacro assert-nonnil (value)
  "Assert that VALUE is not nil, or signal a warning."
  `(unless ,value
     (elk-test-error "assert-nonnil for <%s> failed: was <%s>"
                     ',value ,value)))

(defmacro assert-t (value)
  "Assert that VALUE is t, or signal a warning."
  `(unless (eq ,value t)
     (elk-test-error "assert-t for <%s> failed: was <%s>"
                     ',value ,value)))

(defmacro assert-nil (value)
  "Assert that VALUE is nil, or signal a warning."
  `(when ,value
     (elk-test-error "assert-nil for <%s> failed: was <%s>"
                     ',value ,value)))

(defmacro assert-error (error-message &rest body)
  "Assert that BODY raises an `error', or signal a warning.
ERROR-MESSAGE is the expected error string, use nil to accept any error.  Use
nil with caution, as errors like 'wrong-number-of-arguments' (likely caused by
typos) will also be caught!"
  `(let ((elk-test-error
          (condition-case elk-test-error
              (progn ,@body)
            (error (cons 'elk-test-error (cadr elk-test-error))))))
     (if (not (eq (car elk-test-error) 'elk-test-error))
         ;; no error
         (elk-test-error "assert-error for <%s> failed: did not raise an error"
                         (append '(progn) ',body))
       (when (and ,error-message
                  (not (equal ,error-message (cdr elk-test-error))))
         (elk-test-error (concat "assert-error for <%s> failed: expected <%s>, "
                                 "raised <%s>")
                         (append '(progn) ',body)
                         ,error-message (cdr elk-test-error))))))

(defmacro deftest (name &rest body)
  "Define a test case.
Use `assert-equal', `assert-eq', `assert-eql', `assert-nonnil', `assert-t',
`assert-nil' and `assert-error' to verify the code."
  `(progn (unless (gethash ,name elk-test-map)
            (push ,name elk-test-list))
          (puthash ,name ',body elk-test-map)
          ,(if elk-test-run-on-define
               `(run-elk-test ',name ,t)
             name)))

(defmacro defsuite (name &rest body)
  "Define a test suite using a collection of `deftest' forms.
The resulting suite can be called with `run-elk-test' and parameter NAME."
  `(let ((suite
          (let ((elk-test-map (make-hash-table :test 'equal))
                (elk-test-list nil))
            ,@body
            (list 'suite elk-test-map (reverse elk-test-list)))))
     (unless (gethash ,name elk-test-map)
       (push ,name elk-test-list))
     (puthash ,name suite elk-test-map)
     ,(if elk-test-run-on-define
          `(run-elk-test ,name t)
        name)))

(defun build-suite (name &rest tests)
  "Define a test suite using a collection of test names.
The resulting suite can be run by calling `run-elk-test' with parameter NAME."
  (unless (gethash name elk-test-map)
    (push name elk-test-list))
  (puthash name
           (let ((map (make-hash-table :test 'equal))
                 (list nil))
             (dolist (test-name tests)
               (push test-name list)
               (when (gethash test-name map)
                 (error "Test used twice"))
               (let ((test (gethash test-name elk-test-map)))
                 (unless test
                   (error "Undefined test <%s>" test-name))
                 (puthash test-name test map)))
             (list 'suite map (reverse list)))
           elk-test-map)
  (if elk-test-run-on-define
      (run-elk-test "sample suite" t)
    name))

(provide 'elk-test)
