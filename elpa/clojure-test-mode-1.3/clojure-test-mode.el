;;; clojure-test-mode.el --- Minor mode for Clojure tests

;; Copyright (C) 2009 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: http://emacswiki.org/cgi-bin/wiki/ClojureTestMode
;; Version: 1.3
;; Keywords: languages, lisp
;; Package-Requires: ((swank-clojure "1.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides support for running Clojure tests (using the
;; clojure.test framework) via SLIME and seeing feedback in the test
;; buffer about which tests failed or errored.

;;; Installation:

;; If you use ELPA, you can install via the M-x package-list-packages
;; interface. This is preferrable as you will have access to updates
;; automatically.

;; If you need to install by hand for some reason:

;; (0) Add this file to your load-path, usually the ~/.emacs.d directory.
;; (1) Either:
;;     Add these lines to your .emacs:
;;      (autoload 'clojure-test-mode "clojure-test-mode" "Clojure test mode" t)
;;      (autoload 'clojure-test-maybe-enable "clojure-test-mode" "" t)
;;      (add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)
;;
;;     Or generate autoloads with the `update-directory-autoloads' function.

;; This library does not currently support clojure.contrib.test-is
;; from Clojure Contrib's 1.0-compatibility branch. If you need it,
;; please use version 1.2 of clojure-test-mode:

;; http://github.com/technomancy/clojure-mode/tree/test-1.2

;;; Usage:

;; Once you have a SLIME session active, you can run the tests in the
;; current buffer with C-c C-,. Failing tests and errors will be
;; highlighted using overlays. To clear the overlays, use C-c k.

;; You can jump between implementation and test files with C-c t if
;; your project is laid out in a way that clojure-test-mode
;; expects. Your project root should have a src/ directory containing
;; files that correspond to their namespace. It should also have a
;; test/ directory containing files that correspond to their
;; namespace, and the test namespaces should mirror the implementation
;; namespaces with the addition of "test" as the second-to-last
;; segment of the namespace.

;; So my.project.frob would be found in src/my/project/frob.clj and
;; its tests would be in test/my/project/test/frob.clj in the
;; my.project.test.frob namespace.

;;; History:

;; 1.0: 2009-03-12
;;  * Initial Release

;; 1.1: 2009-04-28
;;  * Fix to work with latest version of test-is. (circa Clojure 1.0)

;; 1.2: 2009-05-19
;;  * Add clojure-test-jump-to-(test|implementation).

;; 1.3: 2009-11-10
;;  * Update to use clojure.test instead of clojure.contrib.test-is.
;;  * Fix bug suppressing test report output in repl.

;;; TODO:

;; * Implement next-problem command
;; * Error messages need line number.
;; * Currently show-message needs point to be on the line with the
;;   "is" invocation; this could be cleaned up.

;;; Code:

(require 'clojure-mode)
(require 'cl)
(require 'slime)
(require 'swank-clojure)
(require 'which-func)

;; Faces

(defface clojure-test-failure-face
  '((((class color) (background light))
     :background "orange red") ;; TODO: Hard to read strings over this.
    (((class color) (background dark))
     :background "firebrick"))
  "Face for failures in Clojure tests."
  :group 'clojure-test-mode)

(defface clojure-test-error-face
  '((((class color) (background light))
     :background "orange1")
    (((class color) (background dark))
     :background "orange4"))
  "Face for errors in Clojure tests."
  :group 'clojure-test-mode)

;; Counts

(defvar clojure-test-count 0)
(defvar clojure-test-failure-count 0)
(defvar clojure-test-error-count 0)

;; Consts

(defconst clojure-test-ignore-results
  '(:end-test-ns :begin-test-var :end-test-var)
  "Results from test-is that we don't use")

;; Support Functions

(defun clojure-test-eval (string &optional handler)
  (slime-eval-async `(swank:eval-and-grab-output ,string)
                    (or handler #'identity)))

(defun clojure-test-eval-sync (string)
  (slime-eval `(swank:eval-and-grab-output ,string)))

(defun clojure-test-load-reporting ()
  "Redefine the test-is report function to store results in metadata."
  (clojure-test-eval-sync
   "(require 'clojure.test) (ns clojure.test)

    (defonce old-report report)
    (defn report [event]
     (if-let [current-test (last *testing-vars*)]
             (alter-meta! current-test
                          assoc :status (conj (:status ^current-test)
                                          [(:type event) (:message event)
                                           (str (:expected event))
                                           (str (:actual event))
                                           ((file-position 2) 1)])))
     (binding [*test-out* *out*]
       (old-report event)))"))

(defun clojure-test-get-results (result)
  (clojure-test-eval
   (concat "(map #(cons (str (:name (meta %)))
                (:status (meta %))) (vals (ns-interns '"
           (slime-current-package) ")))")
   #'clojure-test-extract-results))

(defun clojure-test-extract-results (results)
  (let ((result-vars (read (cadr results))))
    ;; slime-eval-async hands us a cons with a useless car
    (mapc #'clojure-test-extract-result result-vars)
    (message "Ran %s tests. %s failures, %s errors."
             clojure-test-count
             clojure-test-failure-count clojure-test-error-count)))

(defun clojure-test-extract-result (result)
  "Parse the result from a single test. May contain multiple is blocks."
  (dolist (is-result (rest result))
    (unless (member (aref is-result 0) clojure-test-ignore-results)
      (incf clojure-test-count)
      (destructuring-bind (event msg expected actual line) (coerce is-result 'list)
        (if (equal :fail event)
            (progn (incf clojure-test-failure-count)
                   (clojure-test-highlight-problem
                    line event (format "Expected %s, got %s" expected actual)))
          (when (equal :error event)
            (incf clojure-test-error-count)
            (clojure-test-highlight-problem line event actual)))))))

(defun clojure-test-highlight-problem (line event message)
  (save-excursion
    (goto-line line)
    (set-mark-command nil)
    (end-of-line)
    (let ((overlay (make-overlay (mark) (point))))
      (overlay-put overlay 'face (if (equal event :fail)
                                     'clojure-test-failure-face
                                   'clojure-test-error-face))
      (overlay-put overlay 'message message))))

;; File navigation

(defun clojure-test-implementation-for (namespace)
  (let* ((segments (split-string namespace "\\."))
         (common-segments (butlast segments 2))
         (impl-segments (append common-segments (last segments))))
    (mapconcat 'identity impl-segments "/")))

(defun clojure-test-test-for (namespace)
  (let* ((segments (split-string namespace "\\."))
         (common-segments (butlast segments))
         (test-segments (append common-segments '("test")))
         (test-segments (append test-segments (last segments))))
    (mapconcat 'identity test-segments "/")))

;; Commands

(defun clojure-test-run-tests ()
  "Run all the tests in the current namespace."
  (interactive)
  (save-some-buffers nil (lambda () (equal major-mode 'clojure-mode)))
  (clojure-test-clear
   (lambda (&rest args)
     (clojure-test-eval (format "(load-file \"%s\")"
                                (buffer-file-name))
                        (lambda (&rest args)
                          ;; clojure-test-eval will wrap in with-out-str
                          (slime-eval-async `(swank:interactive-eval
                                              "(clojure.test/run-tests)")
                                            #'clojure-test-get-results))))))

(defun clojure-test-run-test ()
  "Run the test at point."
  (interactive)
  (save-some-buffers nil (lambda () (equal major-mode 'clojure-mode)))
  (clojure-test-clear
   (lambda (&rest args)
     (let ((test-name (first (which-function))))
       (slime-eval-async
        `(swank:interactive-eval
          ,(format "(do (load-file \"%s\")
                      (when (:test ^#'%s) (%s) (cons nil (:status ^#'%s))))"
                   (buffer-file-name) test-name test-name test-name))
        (lambda (result-str)
          (let ((result (read result-str)))
            (if (cdr result)
                (clojure-test-extract-result result)
              (message "Not in a test.")))))))))

(defun clojure-test-show-result ()
  "Show the result of the test under point."
  (interactive)
  (let ((overlay (find-if (lambda (o) (overlay-get o 'message))
                          (overlays-at (point)))))
    (if overlay
        (message (replace-regexp-in-string "%" "%%"
                                           (overlay-get overlay 'message))))))

(defun clojure-test-clear (&optional callback)
  "Remove overlays and clear stored results."
  (interactive)
  (remove-overlays)
  (setq clojure-test-count 0
        clojure-test-failure-count 0
        clojure-test-error-count 0)
  (clojure-test-eval
   "(doseq [t (vals (ns-interns *ns*))]
      (alter-meta! t assoc :status [])
      (alter-meta! t assoc :test nil))"
   callback))

(defun clojure-test-jump-to-implementation ()
  "Jump from test file to implementation."
  (interactive)
  (find-file (format "%s/src/%s.clj"
                     (locate-dominating-file buffer-file-name "src/")
                     (clojure-test-implementation-for (slime-current-package)))))

(defun clojure-test-jump-to-test ()
  "Jump from implementation file to test."
  (interactive)
  (find-file (format "%s/test/%s.clj"
                     (locate-dominating-file buffer-file-name "src/")
                     (clojure-test-test-for (slime-current-package)))))

(defvar clojure-test-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-,") 'clojure-test-run-tests)
    (define-key map (kbd "C-c ,")   'clojure-test-run-tests)
    (define-key map (kbd "C-c M-,") 'clojure-test-run-test)
    (define-key map (kbd "C-c C-'") 'clojure-test-show-result)
    (define-key map (kbd "C-c '")   'clojure-test-show-result)
    (define-key map (kbd "C-c k")   'clojure-test-clear)
    (define-key map (kbd "C-c t")   'clojure-test-jump-to-implementation)
    map)
  "Keymap for Clojure test mode.")

(define-key clojure-mode-map (kbd "C-c t") 'clojure-test-jump-to-test)

;;;###autoload
(define-minor-mode clojure-test-mode
  "A minor mode for running Clojure tests."
  nil " Test" clojure-test-mode-map
  (when (slime-connected-p)
    (run-hooks 'slime-connected-hook)))

(add-hook 'slime-connected-hook 'clojure-test-load-reporting)

;;;###autoload
(progn
  (defun clojure-test-maybe-enable ()
    "Enable clojure-test-mode if the current buffer contains Clojure tests.
Also will enable it if the file is in a test directory."
    (save-excursion
      (goto-char (point-min))
      (if (or (search-forward "(deftest" nil t)
              (search-forward "(with-test" nil t)
              (string-match "/test/$" default-directory))
          (clojure-test-mode t))))
  (add-hook 'clojure-mode-hook 'clojure-test-maybe-enable))

(provide 'clojure-test-mode)
;;; clojure-test-mode.el ends here
