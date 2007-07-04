;;; rails-test.el --- tests integration with the compile library

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,

;; Keywords: ruby rails languages oop
;; $URL: svn+ssh://rubyforge/var/svn/emacs-rails/trunk/rails-ws.el $
;; $Id: rails-ws.el 140 2007-03-27 23:33:36Z dimaexe $

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

;;; Code:

(defvar rails-test:history nil)

(defconst rails-test:result-regexp
  "\\([0-9]+ tests, [0-9]+ assertions, \\([0-9]+\\) failures, \\([0-9]+\\) errors\\)")

(defconst rails-test:progress-regexp
  "^[\\.EF]+$")

(defun rails-test:file-ext-regexp ()
  (let ((rails-templates-list (append rails-templates-list (list "rb"))))
    (substring (rails-core:regex-for-match-view) 0 -1)))

(defun rails-test:line-regexp (&optional append prepend)
  (concat
   append
   (format
    "\\(#{RAILS_ROOT}\/\\)?\\(\\(\\.\\|[A-Za-z]:\\)?\\([a-z/_.]+%s\\)\\):\\([0-9]+\\)"
    (rails-test:file-ext-regexp))
   prepend))

(defun rails-test:error-regexp-alist ()
  (list
   (list 'rails-test-trace
         (rails-test:line-regexp) 2 6 nil 0)
   (list 'rails-test-failure
         (rails-test:line-regexp "\\[" "\\]") 2 6 nil 2)
   (list 'rails-test-error
         (rails-test:line-regexp nil ".*\n$") 2 6 nil 2)))

(defun rails-test:print-result ()
  (with-current-buffer (get-buffer rails-script:buffer-name)
    (let ((msg (list))
          (failures 0)
          (errors 0))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward rails-test:result-regexp (point-max) t)
          (setq failures (+ failures (string-to-number (match-string-no-properties 2))))
          (setq errors (+ errors (string-to-number (match-string-no-properties 3))))
          (add-to-list 'msg (match-string-no-properties 1))))
      (unless (zerop (length msg))
        (message (strings-join " || " (reverse msg))))
      (when (and (or (not (zerop rails-script:output-mode-ret-value))
                     (not (zerop errors))
                     (not (zerop failures)))
                 (not (buffer-visible-p (current-buffer))))
        (rails-script:popup-buffer)))))

(defun rails-test:print-progress (start end len)
  (let (content)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^Started" end t)
        (line-move 1)
        (save-match-data
          (let ((progress (string=~ rails-test:progress-regexp
                                    (current-line-string) $m)))
            (when progress
              (setq content (concat content progress)))))))
    (when content
      (message "Progress of %s: %s" rails-script:running-script-name content))))

(define-derived-mode rails-test:compilation-mode compilation-mode "RTest"
  "Major mode for RoR tests."
  (rails-script:setup-output-buffer)
  (set (make-local-variable 'compilation-error-regexp-alist-alist)
       (rails-test:error-regexp-alist))
  (set (make-local-variable 'compilation-error-regexp-alist)
       '(rails-test-error
         rails-test-failure
         rails-test-trace))
  (add-hook 'after-change-functions 'rails-test:print-progress nil t)
  (add-hook 'rails-script:run-after-stop-hook 'rails-test:print-result nil t)
  (add-hook 'rails-script:show-buffer-hook
            #'(lambda()
                (let ((win (get-buffer-window (current-buffer))))
                  (when (window-live-p win)
                    (set-window-point win 0)
                    (unless (buffer-visible-p (current-buffer))
                      (compilation-set-window-height win)))))
            t t))

(defun rails-test:list-of-tasks ()
  "Return a list contains test tasks."
  (append (list "all")
          (delete* nil
                   (mapcar
                    #'(lambda (task) (string=~ "^test\\:\\([^ ]+\\)" task $1))
                    (rails-rake:list-of-tasks))
                   :if 'null)))

(defun rails-test:run (task)
  "Run rake tests in RAILS_ROOT."
  (interactive (rails-completing-read "What test run"
                                      (rails-test:list-of-tasks)
                                      'rails-test:history t))
  (unless task
    (setq task "all")
    (add-to-list rails-test:history task))
  (let ((task-name
         (if (string= "all" task)
             "test"
           (concat "test:" task))))
    (rails-rake:task task-name 'rails-test:compilation-mode)))

(defun rails-test:run-single-file (file &optional param)
  "Run test for single file FILE."
  (let ((param (if param (append (list file) (list param))
                 (list file))))
    (rails-script:run "ruby" param 'rails-test:compilation-mode)))

(defun rails-test:run-current ()
  "Run a test for the current controller/model/mailer."
  (interactive)
  (let* ((model (rails-core:current-model))
         (controller (rails-core:current-controller))
         (func-test (rails-core:functional-test-file controller))
         (unit-test (rails-core:unit-test-file model))
         (mailer-test (rails-core:unit-test-file controller)))
    (rails-test:run-single-file
     (cond
      ;; model
      ((and model unit-test) unit-test)
      ;; controller
      ((and controller (not (rails-core:mailer-p controller)) func-test)
       func-test)
     ;; mailer
     ((and controller (rails-core:mailer-p controller) unit-test)
      unit-test)))))

(defun rails-test:run-current-method ()
  "Run a test for the current method."
  (interactive)
  (let ((file (substring (buffer-file-name) (length (rails-project:root))))
        (method (rails-core:current-method-name)))
    (when method
      (rails-test:run-single-file file (format "--name=%s" method)))))

(provide 'rails-test)