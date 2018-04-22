;;; ensime-expand-region.el --- Ensime expansions for Expand Region  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Sebastian Wiesner

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>

;; License: http://www.gnu.org/licenses/gpl.html

;;; Commentary:
;;
;;  Optional ENSIME integration support for expand-region[1].
;;
;;  This file must be loaded explicitly as it is not included by `ensime.el'.
;;
;;  [1] https://github.com/magnars/expand-region.el
;;
;;; Code:

(require 'ensime-client)
(require 'ensime-editor)
(require 'expand-region-core)

(defun ensime-expand-region-mark-syntactic-context ()
  "Mark the next outer syntactic context."
  (when (ensime-connected-p)
    (ensime-expand-selection (mark) (point))))

(defun ensime-expand-region-add-expansions ()
  "Add expansions for Ensime."
  (set (make-local-variable 'er/try-expand-list)
       (append er/try-expand-list
               '(ensime-expand-region-mark-syntactic-context))))

(defun ensime-expand-region-enable ()
  "Enable Ensime expansions for all Ensime Mode buffers."
  (add-hook 'ensime-mode-hook #'ensime-expand-region-add-expansions)
  (save-window-excursion
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when ensime-mode
          (ensime-expand-region-add-expansions))))))

(eval-after-load 'expand-region
  '(ensime-expand-region-enable))

(provide 'ensime-expand-region)

;;; ensime-expand-region.el ends here
