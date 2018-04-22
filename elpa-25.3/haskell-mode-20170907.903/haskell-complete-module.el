;;; haskell-complete-module.el --- A fast way to complete Haskell module names -*- lexical-binding: t -*-

;; Copyright (c) 2014 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)

(defcustom haskell-complete-module-preferred
  '()
  "Override ordering of module results by specifying preferred modules."
  :group 'haskell
  :type '(repeat string))

(defcustom haskell-complete-module-max-display
  10
  "Maximum items to display in minibuffer."
  :group 'haskell
  :type 'number)

(defun haskell-complete-module-read (prompt candidates)
  "Interactively auto-complete from a list of candidates."
  (let ((stack (list))
        (pattern "")
        (result nil))
    (delete-dups candidates)
    (setq candidates
          (sort candidates
                (lambda (a b)
                  (let ((a-mem (member a haskell-complete-module-preferred))
                        (b-mem (member b haskell-complete-module-preferred)))
                    (cond
                     ((and a-mem (not b-mem))
                      t)
                     ((and b-mem (not a-mem))
                      nil)
                     (t
                      (string< a b)))))))
    (while (not result)
      (let ((key
             (key-description
              (vector
               (read-key
                (concat (propertize prompt 'face 'minibuffer-prompt)
                        (propertize pattern 'face 'font-lock-type-face)
                        "{"
                        (mapconcat #'identity
                                   (let* ((i 0))
                                     (cl-loop for candidate in candidates
                                              while (<= i haskell-complete-module-max-display)
                                              do (cl-incf i)
                                              collect (cond ((> i haskell-complete-module-max-display)
                                                             "...")
                                                            ((= i 1)
                                                             (propertize candidate 'face 'ido-first-match-face))
                                                            (t candidate))))
                                   " | ")
                        "}"))))))
        (cond
         ((string= key "C-g")
          (keyboard-quit))
         ((string= key "DEL")
          (unless (null stack)
            (setq candidates (pop stack)))
          (unless (string= "" pattern)
            (setq pattern (substring pattern 0 -1))))
         ((string= key "RET")
          (setq result (or (car candidates)
                           pattern)))
         ((string= key "<left>")
          (setq candidates
                (append (last candidates)
                        (butlast candidates))))
         ((string= key "<right>")
          (setq candidates
                (append (cdr candidates)
                        (list (car candidates)))))
         (t
          (when (string-match "[A-Za-z0-9_'.]+" key)
            (push candidates stack)
            (setq pattern (concat pattern key))
            (setq candidates (haskell-complete-module pattern candidates)))))))
    result))

(defun haskell-complete-module (pattern candidates)
  "Filter the CANDIDATES using PATTERN."
  (let ((case-fold-search t))
    (cl-loop for candidate in candidates
             when (haskell-complete-module-match pattern candidate)
             collect candidate)))

(defun haskell-complete-module-match (pattern text)
  "Match PATTERN against TEXT."
  (string-match (haskell-complete-module-regexp pattern)
                text))

(defun haskell-complete-module-regexp (pattern)
  "Make a regular expression for the given module pattern. Example:

\"c.m.s\" -> \"^c[^.]*\\.m[^.]*\\.s[^.]*\"

"
  (let ((components (mapcar #'haskell-complete-module-component
                            (split-string pattern "\\." t))))
    (concat "^"
            (mapconcat #'identity
                       components
                       "\\."))))

(defun haskell-complete-module-component (component)
  "Make a regular expression for the given component. Example:

\"co\" -> \"c[^.]*o[^.]*\"

"
  (replace-regexp-in-string "\\(.\\)" "\\1[^.]*" component))

(provide 'haskell-complete-module)
