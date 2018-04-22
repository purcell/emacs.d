;;; flycheck-ycmd.el --- flycheck integration for ycmd
;; Copyright (c) 2014-2017 Austin Bingham
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 0.1
;; Package-Version: 20170614.1434
;; URL: https://github.com/abingham/emacs-ycmd
;; Package-Requires: ((emacs "24") (dash "2.13.0") (flycheck "0.22") (ycmd "1.2") (let-alist "1.0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Description:
;;
;; This provides flycheck integration for ycmd. It allows flycheck to
;; use ycmd's parse results for it display. It essentially works by
;; caching the ycmd parse results and then using them when the checker
;; is invoked.
;;
;; Basic usage:
;;
;;  (require 'flycheck-ycmd)
;;  (flycheck-ycmd-setup)
;;
;;; License:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Code:

(eval-when-compile
  (require 'let-alist))
(require 'dash)
(require 'flycheck)
(require 'ycmd)

;; See http://www.lunaryorn.com/2014/12/03/generic-syntax-checkers-in-flycheck.html for more info

;; This maps ycmd result 'kinds' to flycheck 'levels'.
(defconst flycheck-ycmd--level-map
  '(("ERROR" . error)
    ("WARNING" . warning)))

(defvar-local flycheck-ycmd--cache nil
  "Cache for parse results.")

(defun flycheck-ycmd--result-to-error (result checker)
  "Convert ycmd parse RESULT for CHECKER into a flycheck error object."
  (let-alist result
    (when (string-equal .location.filepath (buffer-file-name))
      (flycheck-error-new
       :line .location.line_num
       :column .location.column_num
       :buffer (current-buffer)
       :filename .location.filepath
       :message (concat .text (when (eq .fixit_available t) " (FixIt available)"))
       :checker checker
       :level (assoc-default .kind flycheck-ycmd--level-map 'string-equal 'error)))))

(defun flycheck-ycmd--start (checker callback)
  "Start ycmd flycheck CHECKER using CALLBACK to communicate with flycheck."
  (let ((errors (delq
                 nil
                 (mapcar (lambda (result)
                           (flycheck-ycmd--result-to-error result checker))
                         flycheck-ycmd--cache))))
    (funcall callback 'finished errors))

  ;; OR call (callback 'errored some-message)
  )

(defun flycheck-ycmd--cache-parse-results (results)
  "Cache ycmd output RESULTS for error display.

We cache the results and use them as the basis for the error
display."
  (setq flycheck-ycmd--cache results)
  (flycheck-buffer-automatically))

(defun flycheck-ycmd--in-supported-mode ()
  "Determines if buffer is in `ycmd-mode` and another mode supported by ycmd."
  (and ycmd-mode (ycmd-file-types-with-diagnostics major-mode)))

;;;###autoload
(defun flycheck-ycmd-setup ()
  "Convenience function to setup the ycmd flycheck checker.

This adds a hook to watch for ycmd parse results, and it adds the
ycmd checker to the list of flycheck checkers."
  (add-hook 'ycmd-file-parse-result-hook 'flycheck-ycmd--cache-parse-results)
  (add-to-list 'flycheck-checkers 'ycmd)
  (add-hook 'ycmd-after-teardown-hook #'flycheck-ycmd--teardown))

(flycheck-define-generic-checker 'ycmd
  "A flycheck checker using parse results from ycmd."
  :start #'flycheck-ycmd--start
  :predicate #'flycheck-ycmd--in-supported-mode
  :modes (ycmd-major-modes-with-diagnostics))

(defun flycheck-ycmd--teardown ()
  "Reset `flycheck-ycmd--cache'."
  (setq flycheck-ycmd--cache nil))

(provide 'flycheck-ycmd)

;;; flycheck-ycmd.el ends here
