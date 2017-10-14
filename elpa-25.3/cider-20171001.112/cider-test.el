;;; cider-test.el --- Test result viewer -*- lexical-binding: t -*-

;; Copyright © 2014-2017 Jeff Valk, Bozhidar Batsov and CIDER contributors

;; Author: Jeff Valk <jv@jeffvalk.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This provides execution, reporting, and navigation support for Clojure tests,
;; specifically using the `clojure.test' machinery.  This functionality replaces
;; the venerable `clojure-test-mode' (deprecated in June 2014), and relies on
;; nREPL middleware for report running and session support.

;;; Code:

(require 'cider-common)
(require 'cider-client)
(require 'cider-popup)
(require 'cider-stacktrace)
(require 'subr-x)
(require 'cider-compat)
(require 'cider-overlays)

(require 'button)
(require 'easymenu)
(require 'seq)

;;; Variables

(defgroup cider-test nil
  "Presentation and navigation for test results."
  :prefix "cider-test-"
  :group 'cider)

(defcustom cider-test-show-report-on-success nil
  "Whether to show the `*cider-test-report*` buffer on passing tests."
  :type 'boolean
  :group 'cider-test
  :package-version '(cider . "0.8.0"))

(defcustom cider-auto-select-test-report-buffer t
  "Determines if the test-report buffer should be auto-selected."
  :type 'boolean
  :group 'cider-test
  :package-version '(cider . "0.9.0"))

(defcustom cider-test-defining-forms '("deftest" "defspec")
  "Forms that define individual tests.
CIDER considers the \"top-level\" form around point to define a test if
the form starts with one of these forms.
Add to this list to have CIDER recognize additional test defining macros."
  :type '(repeat string)
  :group 'cider-test
  :package-version '(cider . "0.15.0"))

(defvar cider-test-last-summary nil
  "The summary of the last run test.")

(defvar cider-test-last-results nil
  "The results of the last run test.")

(defconst cider-test-report-buffer "*cider-test-report*"
  "Buffer name in which to display test reports.")
(add-to-list 'cider-ancillary-buffers cider-test-report-buffer)


;;; Faces

(defface cider-test-failure-face
  '((((class color) (background light))
     :background "orange red")
    (((class color) (background dark))
     :background "firebrick"))
  "Face for failed tests."
  :group 'cider-test
  :package-version '(cider . "0.7.0"))

(defface cider-test-error-face
  '((((class color) (background light))
     :background "orange1")
    (((class color) (background dark))
     :background "orange4"))
  "Face for erring tests."
  :group 'cider-test
  :package-version '(cider . "0.7.0"))

(defface cider-test-success-face
  '((((class color) (background light))
     :foreground "black"
     :background "green")
    (((class color) (background dark))
     :foreground "black"
     :background "green"))
  "Face for passing tests."
  :group 'cider-test
  :package-version '(cider . "0.7.0"))


;; Colors & Theme Support

(defvar cider-test-items-background-color
  (cider-scale-background-color)
  "Background color for test assertion items.")

(defadvice enable-theme (after cider-test-adapt-to-theme activate)
  "When theme is changed, update `cider-test-items-background-color'."
  (setq cider-test-items-background-color (cider-scale-background-color)))


(defadvice disable-theme (after cider-test-adapt-to-theme activate)
  "When theme is disabled, update `cider-test-items-background-color'."
  (setq cider-test-items-background-color (cider-scale-background-color)))


;;; Report mode & key bindings
;; The primary mode of interacting with test results is the report buffer, which
;; allows navigation among tests, jumping to test definitions, expected/actual
;; diff-ing, and cause/stacktrace inspection for test errors.

(defvar cider-test-commands-map
  (let ((map (define-prefix-command 'cider-test-commands-map)))
    ;; Duplicates of keys below with C- for convenience
    (define-key map (kbd "C-r") #'cider-test-rerun-failed-tests)
    (define-key map (kbd "C-t") #'cider-test-run-test)
    (define-key map (kbd "C-g") #'cider-test-rerun-test)
    (define-key map (kbd "C-n") #'cider-test-run-ns-tests)
    (define-key map (kbd "C-l") #'cider-test-run-loaded-tests)
    (define-key map (kbd "C-p") #'cider-test-run-project-tests)
    (define-key map (kbd "C-b") #'cider-test-show-report)
    ;; Single-key bindings defined last for display in menu
    (define-key map (kbd "r")   #'cider-test-rerun-failed-tests)
    (define-key map (kbd "t")   #'cider-test-run-test)
    (define-key map (kbd "g")   #'cider-test-rerun-test)
    (define-key map (kbd "n")   #'cider-test-run-ns-tests)
    (define-key map (kbd "l")   #'cider-test-run-loaded-tests)
    (define-key map (kbd "p")   #'cider-test-run-project-tests)
    (define-key map (kbd "b")   #'cider-test-show-report)
    map))

(defconst cider-test-menu
  '("Test"
    ["Run test" cider-test-run-test]
    ["Run namespace tests" cider-test-run-ns-tests]
    ["Run all loaded tests" cider-test-run-loaded-tests]
    ["Run all project tests" cider-test-run-project-tests]
    ["Run tests after load-file" cider-auto-test-mode
     :style toggle :selected cider-auto-test-mode]
    "--"
    ["Interrupt running tests" cider-interrupt]
    ["Rerun failed/erring tests" cider-test-rerun-failed-tests]
    ["Show test report" cider-test-show-report]
    "--"
    ["Configure testing" (customize-group 'cider-test)])
  "CIDER test submenu.")

(defvar cider-test-report-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c ,")   'cider-test-commands-map)
    (define-key map (kbd "C-c C-t") 'cider-test-commands-map)
    (define-key map (kbd "M-p") #'cider-test-previous-result)
    (define-key map (kbd "M-n") #'cider-test-next-result)
    (define-key map (kbd "M-.") #'cider-test-jump)
    (define-key map (kbd "<backtab>") #'cider-test-previous-result)
    (define-key map (kbd "TAB") #'cider-test-next-result)
    (define-key map (kbd "RET") #'cider-test-jump)
    (define-key map (kbd "t") #'cider-test-jump)
    (define-key map (kbd "d") #'cider-test-ediff)
    (define-key map (kbd "e") #'cider-test-stacktrace)
    ;; `f' for "run failed".
    (define-key map "f" #'cider-test-rerun-failed-tests)
    (define-key map "n" #'cider-test-run-ns-tests)
    (define-key map "l" #'cider-test-run-loaded-tests)
    (define-key map "p" #'cider-test-run-project-tests)
    ;; `g' generally reloads the buffer.  The closest thing we have to that is
    ;; "run the test at point".  But it's not as nice as rerunning all tests in
    ;; this buffer.
    (define-key map "g" #'cider-test-run-test)
    (define-key map "q" #'cider-popup-buffer-quit-function)
    (easy-menu-define cider-test-report-mode-menu map
      "Menu for CIDER's test result mode"
      '("Test-Report"
        ["Previous result" cider-test-previous-result]
        ["Next result" cider-test-next-result]
        "--"
        ["Rerun current test" cider-test-run-test]
        ["Rerun failed/erring tests" cider-test-rerun-failed-tests]
        ["Run all ns tests" cider-test-run-ns-tests]
        ["Run all loaded tests" cider-test-run-loaded-tests]
        ["Run all project tests" cider-test-run-project-tests]
        "--"
        ["Jump to test definition" cider-test-jump]
        ["Display test error" cider-test-stacktrace]
        ["Display expected/actual diff" cider-test-ediff]))
    map))

(define-derived-mode cider-test-report-mode fundamental-mode "Test Report"
  "Major mode for presenting Clojure test results.

\\{cider-test-report-mode-map}"
  (setq buffer-read-only t)
  (when cider-special-mode-truncate-lines
    (setq-local truncate-lines t))
  (setq-local electric-indent-chars nil))

;; Report navigation

(defun cider-test-show-report ()
  "Show the test report buffer, if one exists."
  (interactive)
  (if-let ((report-buffer (get-buffer cider-test-report-buffer)))
      (switch-to-buffer report-buffer)
    (message "No test report buffer")))

(defun cider-test-previous-result ()
  "Move point to the previous test result, if one exists."
  (interactive)
  (with-current-buffer (get-buffer cider-test-report-buffer)
    (when-let ((pos (previous-single-property-change (point) 'type)))
      (if (get-text-property pos 'type)
          (goto-char pos)
        (when-let ((pos (previous-single-property-change pos 'type)))
          (goto-char pos))))))

(defun cider-test-next-result ()
  "Move point to the next test result, if one exists."
  (interactive)
  (with-current-buffer (get-buffer cider-test-report-buffer)
    (when-let ((pos (next-single-property-change (point) 'type)))
      (if (get-text-property pos 'type)
          (goto-char pos)
        (when-let ((pos (next-single-property-change pos 'type)))
          (goto-char pos))))))

(defun cider-test-jump (&optional arg)
  "Find definition for test at point, if available.
The prefix ARG and `cider-prompt-for-symbol' decide whether to
prompt and whether to use a new window.  Similar to `cider-find-var'."
  (interactive "P")
  (let ((ns   (get-text-property (point) 'ns))
        (var  (get-text-property (point) 'var))
        (line (get-text-property (point) 'line)))
    (if (and ns var)
        (cider-find-var arg (concat ns "/" var) line)
      (cider-find-var arg))))

;;; Error stacktraces

(defvar cider-auto-select-error-buffer)

(defun cider-test-stacktrace-for (ns var index)
  "Display stacktrace for the erring NS VAR test with the assertion INDEX."
  (let (causes)
    (cider-nrepl-send-request
     (nconc `("op" "test-stacktrace"
              "ns" ,ns
              "var" ,var
              "index" ,index)
            (when (cider--pprint-fn)
              `("pprint-fn" ,(cider--pprint-fn)))
            (when cider-stacktrace-print-length
              `("print-length" ,cider-stacktrace-print-length))
            (when cider-stacktrace-print-level
              `("print-level" ,cider-stacktrace-print-level)))
     (lambda (response)
       (nrepl-dbind-response response (class status)
         (cond (class  (setq causes (cons response causes)))
               (status (when causes
                         (cider-stacktrace-render
                          (cider-popup-buffer cider-error-buffer
                                              cider-auto-select-error-buffer
                                              #'cider-stacktrace-mode)
                          (reverse causes))))))))))

(defun cider-test-stacktrace ()
  "Display stacktrace for the erring test at point."
  (interactive)
  (let ((ns    (get-text-property (point) 'ns))
        (var   (get-text-property (point) 'var))
        (index (get-text-property (point) 'index))
        (err   (get-text-property (point) 'error)))
    (if (and err ns var index)
        (cider-test-stacktrace-for ns var index)
      (message "No test error at point"))))


;;; Expected vs actual diffing

(defvar cider-test-ediff-buffers nil
  "The expected/actual buffers used to display diff.")

(defun cider-test-ediff ()
  "Show diff of the expected vs actual value for the test at point.
With the actual value, the outermost '(not ...)' s-expression is removed."
  (interactive)
  (let ((expected (get-text-property (point) 'expected))
        (actual   (get-text-property (point) 'actual)))
    (if (and expected actual)
        (let ((expected-buffer (generate-new-buffer " *expected*"))
              (actual-buffer   (generate-new-buffer " *actual*")))
          (with-current-buffer expected-buffer
            (insert expected)
            (clojure-mode))
          (with-current-buffer actual-buffer
            (insert actual)
            (goto-char (point-min))
            (forward-char)
            (forward-sexp)
            (forward-whitespace 1)
            (let ((beg (point)))
              (forward-sexp)
              (let ((actual* (buffer-substring beg (point))))
                (erase-buffer)
                (insert actual*)))
            (clojure-mode))
          (apply 'ediff-buffers
                 (setq cider-test-ediff-buffers
                       (list (buffer-name expected-buffer)
                             (buffer-name actual-buffer)))))
      (message "No test failure at point"))))

(defun cider-test-ediff-cleanup ()
  "Cleanup expected/actual buffers used for diff."
  (interactive)
  (mapc (lambda (b) (when (get-buffer b) (kill-buffer b)))
        cider-test-ediff-buffers))

(add-hook 'ediff-cleanup-hook #'cider-test-ediff-cleanup)


;;; Report rendering

(defun cider-test-type-face (type)
  "Return the font lock face for the test result TYPE."
  (pcase type
    ("pass"  'cider-test-success-face)
    ("fail"  'cider-test-failure-face)
    ("error" 'cider-test-error-face)
    (_       'default)))

(defun cider-test-type-simple-face (type)
  "Return a face for the test result TYPE using the highlight color as foreground."
  (let ((face (cider-test-type-face type)))
    `(:foreground ,(face-attribute face :background))))

(defun cider-test-render-summary (buffer summary)
  "Emit into BUFFER the report SUMMARY statistics."
  (with-current-buffer buffer
    (nrepl-dbind-response summary (ns var test pass fail error)
      (insert (format "Tested %d namespaces\n" ns))
      (insert (format "Ran %d assertions, in %d test functions\n" test var))
      (unless (zerop fail)
        (cider-insert (format "%d failures" fail) 'cider-test-failure-face t))
      (unless (zerop error)
        (cider-insert (format "%d errors" error) 'cider-test-error-face t))
      (when (zerop (+ fail error))
        (cider-insert (format "%d passed" pass) 'cider-test-success-face t))
      (insert "\n\n"))))

(defun cider-test-render-assertion (buffer test)
  "Emit into BUFFER report detail for the TEST assertion."
  (with-current-buffer buffer
    (nrepl-dbind-response test (var context type message expected actual error gen-input)
      (cider-propertize-region (cider-intern-keys (cdr test))
        (let ((beg (point))
              (type-face (cider-test-type-simple-face type))
              (bg `(:background ,cider-test-items-background-color)))
          (cider-insert (capitalize type) type-face nil " in ")
          (cider-insert var 'font-lock-function-name-face t)
          (when context  (cider-insert context 'font-lock-doc-face t))
          (when message  (cider-insert message 'font-lock-doc-string-face t))
          (when expected
            (cider-insert "expected: " 'font-lock-comment-face nil
                          (cider-font-lock-as-clojure expected)))
          (when actual
            (cider-insert "  actual: " 'font-lock-comment-face nil
                          (cider-font-lock-as-clojure actual)))
          (when error
            (cider-insert "   error: " 'font-lock-comment-face nil)
            (insert-text-button error
                                'follow-link t
                                'action '(lambda (_button) (cider-test-stacktrace))
                                'help-echo "View causes and stacktrace")
            (insert "\n"))
          (when gen-input
            (cider-insert "   input: " 'font-lock-comment-face nil
                          (cider-font-lock-as-clojure gen-input)))
          (overlay-put (make-overlay beg (point)) 'font-lock-face bg))
        (insert "\n")))))

(defun cider-test-non-passing (tests)
  "For a list of TESTS, each an nrepl-dict, return only those that did not pass."
  (seq-filter (lambda (test)
                (unless (equal (nrepl-dict-get test "type") "pass")
                  test))
              tests))

(defun cider-test-render-report (buffer summary results)
  "Emit into BUFFER the report for the SUMMARY, and test RESULTS."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (cider-test-report-mode)
      (cider-insert "Test Summary" 'bold t)
      (dolist (ns (nrepl-dict-keys results))
        (insert (cider-propertize ns 'ns) "\n"))
      (cider-insert "\n")
      (cider-test-render-summary buffer summary)
      (nrepl-dbind-response summary (fail error)
        (unless (zerop (+ fail error))
          (cider-insert "Results" 'bold t "\n")
          ;; Results are a nested dict, keyed first by ns, then var. Within each
          ;; var is a sequence of test assertion results.
          (nrepl-dict-map
           (lambda (ns vars)
             (nrepl-dict-map
              (lambda (_var tests)
                (let* ((problems (cider-test-non-passing tests))
                       (count (length problems)))
                  (when (< 0 count)
                    (insert (format "%s\n%d non-passing tests:\n\n"
                                    (cider-propertize ns 'ns) count))
                    (dolist (test problems)
                      (cider-test-render-assertion buffer test)))))
              vars))
           results)))
      (goto-char (point-min))
      (current-buffer))))


;;; Message echo

(defun cider-test-echo-running (ns &optional test)
  "Echo a running message for the test NS, which may be a keyword.
The optional arg TEST denotes an individual test name."
  (if test
      (message "Running test %s in %s..."
               (cider-propertize test 'bold)
               (cider-propertize ns 'ns))
    (message "Running tests in %s..."
             (concat (cider-propertize
                      (cond ((stringp ns) ns)
                            ((eq :non-passing ns) "failing")
                            ((eq :loaded ns)  "all loaded")
                            ((eq :project ns) "all project"))
                      'ns)
                     (unless (stringp ns) " namespaces")))))

(defun cider-test-echo-summary (summary results)
  "Echo SUMMARY statistics for a test run returning RESULTS."
  (nrepl-dbind-response summary (ns test var fail error)
    (if (nrepl-dict-empty-p results)
        (message (concat (propertize "No assertions (or no tests) were run." 'face 'cider-test-error-face)
                         "Did you forget to use `is' in your tests?"))
      (message (propertize
                "%sRan %d assertions, in %d test functions. %d failures, %d errors."
                'face (cond ((not (zerop error)) 'cider-test-error-face)
                            ((not (zerop fail))  'cider-test-failure-face)
                            (t                   'cider-test-success-face)))
               (concat (if (= 1 ns)     ; ns count from summary
                           (cider-propertize (car (nrepl-dict-keys results)) 'ns)
                         (propertize (format "%d namespaces" ns) 'face 'default))
                       (propertize ": " 'face 'default))
               test var fail error))))

;;; Test definition highlighting
;; On receipt of test results, failing/erring test definitions are highlighted.
;; Highlights are cleared on the next report run, and may be cleared manually
;; by the user.

;; NOTE If keybindings specific to test sources are desired, it would be
;; straightforward to turn this into a `cider-test-mode' minor mode, which we
;; enable on test sources, much like the legacy `clojure-test-mode'. At present,
;; though, there doesn't seem to be much value in this, since the report buffer
;; provides the primary means of interacting with test results.

(defun cider-test-highlight-problem (buffer test)
  "Highlight the BUFFER test definition for the non-passing TEST."
  (with-current-buffer buffer
    (nrepl-dbind-response test (type file line message expected actual)
      ;; we have to watch out for vars without proper location metadata
      ;; right now everything evaluated interactively lacks this data
      ;; TODO: Figure out what to do when the metadata is missing
      (when (and file line (not (cider--tooling-file-p file)))
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- line))
          (search-forward "(" nil t)
          (let ((beg (point)))
            (forward-sexp)
            (cider--make-overlay beg (point) 'cider-test
                                 'font-lock-face (cider-test-type-face type)
                                 'type type
                                 'help-echo message
                                 'message message
                                 'expected expected
                                 'actual actual)))))))

(defun cider-find-var-file (ns var)
  "Return the buffer visiting the file in which the NS VAR is defined.
Or nil if not found."
  (cider-ensure-op-supported "info")
  (when-let ((info (cider-var-info (concat ns "/" var)))
             (file (nrepl-dict-get info "file")))
    (cider-find-file file)))

(defun cider-test-highlight-problems (results)
  "Highlight all non-passing tests in the test RESULTS."
  (nrepl-dict-map
   (lambda (ns vars)
     (nrepl-dict-map
      (lambda (var tests)
        (when-let ((buffer (cider-find-var-file ns var)))
          (dolist (test tests)
            (nrepl-dbind-response test (type)
              (unless (equal "pass" type)
                (cider-test-highlight-problem buffer test))))))
      vars))
   results))

(defun cider-test-clear-highlights ()
  "Clear highlighting of non-passing tests from the last test run."
  (interactive)
  (when cider-test-last-results
    (nrepl-dict-map
     (lambda (ns vars)
       (dolist (var (nrepl-dict-keys vars))
         (when-let ((buffer (cider-find-var-file ns var)))
           (with-current-buffer buffer
             (remove-overlays nil nil 'category 'cider-test)))))
     cider-test-last-results)))


;;; Test namespaces
;; Test namespace inference exists to enable DWIM test running functions: the
;; same "run-tests" function should be able to be used in a source file, and in
;; its corresponding test namespace. To provide this, we need to map the
;; relationship between those namespaces.

(defcustom cider-test-infer-test-ns 'cider-test-default-test-ns-fn
  "Function to infer the test namespace for NS.
The default implementation uses the simple Leiningen convention of appending
'-test' to the namespace name."
  :type 'symbol
  :group 'cider-test
  :package-version '(cider . "0.7.0"))

(defun cider-test-default-test-ns-fn (ns)
  "For a NS, return the test namespace, which may be the argument itself.
This uses the Leiningen convention of appending '-test' to the namespace name."
  (when ns
    (let ((suffix "-test"))
      ;; string-suffix-p is only available in Emacs 24.4+
      (if (string-match-p (rx-to-string `(: ,suffix eos) t) ns)
          ns
        (concat ns suffix)))))


;;; Test execution

(declare-function cider-emit-interactive-eval-output "cider-interaction")
(declare-function cider-emit-interactive-eval-err-output "cider-interaction")

(defun cider-test-execute (ns &optional tests silent)
  "Run tests for NS, which may be a keyword, optionally specifying TESTS.

This tests a single NS, or multiple namespaces when using keywords `:project',
`:loaded' or `:non-passing'.  Optional TESTS are only honored when a single
namespace is specified.  Upon test completion, results are echoed and a test
report is optionally displayed.  When test failures/errors occur, their sources
are highlighted.
If SILENT is non-nil, suppress all messages other then test results."
  (cider-test-clear-highlights)
  (cider-map-connections
   (lambda (conn)
     (unless silent
       (if (and tests (= (length tests) 1))
           ;; we generate a different message when running individual tests
           (cider-test-echo-running ns (car tests))
         (cider-test-echo-running ns)))
     (cider-nrepl-send-request
      `("op"     ,(cond ((stringp ns)         "test")
                        ((eq :project ns)     "test-all")
                        ((eq :loaded ns)      "test-all")
                        ((eq :non-passing ns) "retest"))
        "ns"     ,(when (stringp ns) ns)
        "tests"  ,(when (stringp ns) tests)
        "load?"  ,(when (or (stringp ns)
                            (eq :project ns))
                    "true"))
      (lambda (response)
        (nrepl-dbind-response response (summary results status out err)
          (cond ((member "namespace-not-found" status)
                 (unless silent
                   (message "No test namespace: %s" (cider-propertize ns 'ns))))
                (out (cider-emit-interactive-eval-output out))
                (err (cider-emit-interactive-eval-err-output err))
                (results
                 (nrepl-dbind-response summary (error fail)
                   (setq cider-test-last-summary summary)
                   (setq cider-test-last-results results)
                   (cider-test-highlight-problems results)
                   (cider-test-echo-summary summary results)
                   (if (or (not (zerop (+ error fail)))
                           cider-test-show-report-on-success)
                       (cider-test-render-report
                        (cider-popup-buffer cider-test-report-buffer
                                            cider-auto-select-test-report-buffer)
                        summary results)
                     (when (get-buffer cider-test-report-buffer)
                       (with-current-buffer cider-test-report-buffer
                         (let ((inhibit-read-only t))
                           (erase-buffer)))
                       (cider-test-render-report
                        cider-test-report-buffer
                        summary results))))))))
      conn))
   :clj))

(defun cider-test-rerun-failed-tests ()
  "Rerun failed and erring tests from the last test run."
  (interactive)
  (if cider-test-last-summary
      (nrepl-dbind-response cider-test-last-summary (fail error)
        (if (not (zerop (+ error fail)))
            (cider-test-execute :non-passing)
          (message "No prior failures to retest")))
    (message "No prior results to retest")))

(defun cider-test-run-loaded-tests ()
  "Run all tests defined in currently loaded namespaces."
  (interactive)
  (cider-test-execute :loaded))

(defun cider-test-run-project-tests ()
  "Run all tests defined in all project namespaces, loading these as needed."
  (interactive)
  (cider-test-execute :project))

(defun cider-test-run-ns-tests (suppress-inference &optional silent)
  "Run all tests for the current Clojure namespace context.

If SILENT is non-nil, suppress all messages other then test results.
With a prefix arg SUPPRESS-INFERENCE it will try to run the tests in the
current ns."
  (interactive "P")
  (if-let ((ns (if suppress-inference
                   (cider-current-ns t)
                 (funcall cider-test-infer-test-ns (cider-current-ns t)))))
      (cider-test-execute ns nil silent)
    (if (eq major-mode 'cider-test-report-mode)
        (when (y-or-n-p (concat "Test report does not define a namespace. "
                                "Rerun failed/erring tests?"))
          (cider-test-rerun-failed-tests))
      (unless silent
        (message "No namespace to test in current context")))))

(defvar cider-test-last-test-ns nil
  "The ns of the last test ran with `cider-test-run-test'.")
(defvar cider-test-last-test-var nil
  "The var of the last test ran with `cider-test-run-test'.")

(defun cider-test-update-last-test (ns var)
  "Update the last test by setting NS and VAR.

See `cider-test-rerun-test'."
  (setq cider-test-last-test-ns ns
        cider-test-last-test-var var))

(defun cider-test-run-test ()
  "Run the test at point.
The test ns/var exist as text properties on report items and on highlighted
failed/erred test definitions.  When not found, a test definition at point
is searched."
  (interactive)
  (let ((ns  (get-text-property (point) 'ns))
        (var (get-text-property (point) 'var)))
    (if (and ns var)
        (progn
          (cider-test-update-last-test ns var)
          (cider-test-execute ns (list var)))
      (let ((ns  (clojure-find-ns))
            (def (clojure-find-def)))
        (if (and ns (member (car def) cider-test-defining-forms))
            (progn
              (cider-test-update-last-test ns (cdr def))
              (cider-test-execute ns (cdr def)))
          (message "No test at point"))))))

(defun cider-test-rerun-test ()
  "Re-run the test that was previously ran."
  (interactive)
  (if (and cider-test-last-test-ns cider-test-last-test-var)
      (cider-test-execute cider-test-last-test-ns cider-test-last-test-var)
    (user-error "No test to re-run")))

;;; Auto-test mode
(defun cider--test-silently ()
  "Like `cider-test-run-tests', but with less feedback.
Only notify the user if there actually were any tests to run and only after
the results are received."
  (when (cider-connected-p)
    (let ((cider-auto-select-test-report-buffer nil)
          (cider-test-show-report-on-success nil))
      (cider-test-run-ns-tests nil 'soft))))

;;;###autoload
(define-minor-mode cider-auto-test-mode
  "Toggle automatic testing of Clojure files.

When enabled this reruns tests every time a Clojure file is loaded.
Only runs tests corresponding to the loaded file's namespace and does
nothing if no tests are defined or if the file failed to load."
  nil (cider-mode " Test") nil
  :global t
  (if cider-auto-test-mode
      (add-hook 'cider-file-loaded-hook #'cider--test-silently)
    (remove-hook 'cider-file-loaded-hook #'cider--test-silently)))

(provide 'cider-test)

;;; cider-test.el ends here
