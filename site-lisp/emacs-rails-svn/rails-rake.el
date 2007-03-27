;;; rails-rake.el --- emacs-rails integraions with rake tasks.

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,

;; Keywords: ruby rails languages oop
;; $URL: svn+ssh://rubyforge/var/svn/emacs-rails/trunk/rails-scripts.el $
;; $Id: rails-scripts.el 117 2007-03-25 23:37:37Z dimaexe $

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

(eval-when-compile
  (require 'rails-scripts))

(defvar rails-rake:tests-running-history (list))

(defvar rails-rake:tasks-regexp "^rake \\([^ ]*\\).*# \\(.*\\)"
  "Regexp to match tasks list in `rake --tasks` output.")

(defconst rails-rake:output-mode-error-label-regexp
  "^ + \\([0-9]+) +\\(Error\\|Failure\\):\\)"
  "Regexp to match error labels")

(defconst rails-rake:output-mode-result-regexp
  "\\([0-9]+ tests, [0-9]+ assertions, [0-9]+ failures, [0-9]+ errors\\)")

(defconst rails-rake:output-mode-font-lock-ketwords
  (list
   (list rails-rake:output-mode-error-label-regexp  1 font-lock-warning-face)
   '("^Started$"                                    . font-lock-keyword-face)
   '("^Finished in .* seconds."                     . font-lock-keyword-face)
   (list rails-rake:output-mode-result-regexp       1 font-lock-keyword-face)
   '("^[.FE]+$"                                     . font-lock-keyword-face)
   '("\\([a-z09_]+([A-Z][a-zA-Z0-9]+Test)\\)[ :]"   . font-lock-function-name-face)
   '("\\[\\(\\.[^:]+:[0-9]+\\)\\]"                  1 font-lock-constant-face)
   '("^ *\\([\\.]?[^:]+:[0-9]+\\)\\(:in `.*\\)?$"   1 font-lock-constant-face)))

;; output-mode

(defun rails-rake:output-mode-make-links (start end len)
  )

(defun rails-rake:report-result ()
  (with-current-buffer (get-buffer rails-script:buffer-name)
    (let ((msg (list)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward rails-rake:output-mode-result-regexp (point-max) t)
          (add-to-list 'msg (match-string-no-properties 1))))
      (unless (zerop (length msg))
        (message (strings-join " || " (reverse msg)))))))

(defun rails-rake:report-progress-of-test (start end len)
  (let (content)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^Started" end t)
        (line-move 1)
        (save-match-data
          (let ((progress (string=~ "^[\\.EF]+$" (current-line-string) $m)))
            (when progress
              (setq content (concat content progress)))))))
    (when content
      (message "Progress of %s: %s" rails-script:running-script-name content))))

(define-derived-mode rails-rake:output-mode rails-script:output-mode "Rails Rake Output"
  "Major mode to Rails Rake Output."
  (setq rails-script:popup-buffer-after-stop-if-success nil)
  (remove-hook 'rails-script:output-mode-push-first-button t)
  (add-hook 'rails-script:output-mode-after-stop-hook 'rails-rake:report-result nil t)
  (setq font-lock-defaults
        '((rails-rake:output-mode-font-lock-ketwords) nil t))
  (add-hook 'after-change-functions 'rails-rake:report-progress-of-test))

;; tasks

(defun rails-rake:create-tasks-cache (file-name)
  "Create a cache file from rake --tasks output."
  (let ((tasks (loop for str in (split-string (rails-cmd-proxy:shell-command-to-string "rake --tasks") "\n")
                     for task = (when (string-not-empty str)
                                  (string=~ rails-rake:tasks-regexp str $1))
                     when task collect task)))
    (write-string-to-file file-name (prin1-to-string tasks))
    tasks))

(defun rails-rake:tasks-list ()
  "Return all tasks list and create tasks cache file."
  (rails-core:in-root
   (let* ((cache-file (rails-core:file "tmp/.tasks-cache")))
     (if (file-exists-p cache-file)
         (read-from-file cache-file)
       (rails-rake:create-tasks-cache cache-file)))))

(defun rails-rake:test-tasks-list ()
  "Return all tests tasks list."
  (append (list "all")
          (delete* nil
                   (mapcar
                    #'(lambda (task) (string=~ "^test\\:\\([^ ]+\\)" task $1))
                    (rails-rake:tasks-list))
                   :if #'(lambda (it) (not it)))))

(defun rails-rake:task (&optional task)
  "Run a Rake task in RAILS_ROOT."
  (interactive (list (completing-read "Rake task (use autocomplete): " (list->alist (rails-rake:tasks-list)))))
  (save-some-buffers)
  (rails-script:run "rake" (list task) 'rails-rake:output-mode))

(defun rails-rake:test (&optional task)
  "Run Rake tests in RAILS_ROOT."
  (interactive (list (completing-read (concat "What test-run?"
                                              (when (car rails-rake:tests-running-history)
                                                (format " (%s)" (car rails-rake:tests-running-history)))
                                              ": ")
                                      (list->alist (rails-rake:test-tasks-list)) ; table
                                      nil ; predicate
                                      t ; require-match
                                      nil ; initial-input
                                      'rails-rake:tests-running-history ; hist
                                      (car rails-rake:tests-running-history)))) ; def
  (unless task
    (setq task "all")
    (add-to-list rails-rake:tests-running-history task))
  (let ((task-name (if (string= "all" task) "test"
                     (concat "test:" task))))
    (rails-rake:task task-name)))

(defun rails-rake:run-test-file (file &optional param)
  (let ((param (if param (append (list file) (list param))
                 (list file))))
    (rails-script:run "ruby" param 'rails-rake:output-mode)))

(defun rails-rake:test-current (&optional method)
  (interactive)
  (let* ((model (rails-core:current-model))
         (controller (rails-core:current-controller))
         (func-test (rails-core:functional-test-file controller))
         (unit-test (rails-core:unit-test-file model))
         (mailer-test (rails-core:unit-test-file controller)))
    (cond
     ;; model
     ((and model unit-test)
      (rails-rake:run-test-file unit-test))
     ;; controller
     ((and controller (not (rails-core:mailer-p controller)) func-test)
      (rails-rake:run-test-file func-test))
     ;; mailer
     ((and controller (rails-core:mailer-p controller) unit-test)
      (rails-rake:run-test-file unit-test)))))

(defun rails-rake:test-current-method ()
  (interactive)
  (let ((file (substring (buffer-file-name) (length (rails-core:root))))
        (method (rails-core:current-method-name)))
    (when method
      (rails-rake:run-test-file file (format "--name=%s" method)))))

(provide 'rails-rake)
