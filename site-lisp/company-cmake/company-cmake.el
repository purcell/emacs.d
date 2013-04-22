;;; company-cmake.el --- company-mode completion back-end for CMake

;; Copyright (C) 2013 Chen Bin

;; Author: Chen Bin <chenbin DOT sh AT gmail>
;; Version: 0.1
;; URL: https://github.com/company-mode/company-cmake
;; Package-Requires: ((company "0.6.8"))

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; company-cmake offers completions for module names, variable names and
;; commands used by CMake. And their descriptions.
;;
;;; Change Log:
;;
;; 2013-04-21, support meta command
;; 2013-04-20, first version

;;; Code:
(require 'company)

(defgroup company-cmake nil
  "Completion back-end for CMake."
  :group 'company)

(defcustom company-cmake-executable
  (executable-find "cmake")
  "Location of cmake executable."
  :type 'file)

(defvar company-cmake-executable-arguments
  '("--help-command-list"
    "--help-module-list"
    "--help-variable-list")
  "The arguments we pass to cmake, separately.
They affect which types of symbols we get completion candidates for.")

(defvar company-cmake--completion-pattern
  "^\\(%s[a-zA-Z0-9_]%s\\)$"
  "Regexp to match the candidates.")

(defvar company-cmake-modes '(cmake-mode)
  "Major modes in which cmake may complete.")

(defvar company-cmake--meta-command-cache nil
  "Cache for command arguments to retrieve descriptions for the candidates.")

(defun company-cmake--parse-output (prefix cmd)
  "Analyze the temp buffer and collect lines."
  (goto-char (point-min))
  (let ((pattern (format company-cmake--completion-pattern
                         (regexp-quote prefix)
                         (if (zerop (length prefix)) "+" "*")))
        (case-fold-search nil)
        lines match)
    (while (re-search-forward pattern nil t)
      (setq match (match-string-no-properties 1))
      (puthash match cmd company-cmake--meta-command-cache)
      (push match lines))
    lines))

(defun company-cmake--candidates (prefix)
  (let ((res 0)
        results
        cmd)
    (setq company-cmake--meta-command-cache (make-hash-table :test 'equal))
    (dolist (arg company-cmake-executable-arguments)
      (with-temp-buffer
        (setq res (call-process company-cmake-executable nil t nil arg))
        (unless (eq 0 res)
          (message "cmake executable exited with error=%d" res))
        (setq cmd (replace-regexp-in-string "-list$" "" arg) )
        (setq results (nconc results (company-cmake--parse-output prefix cmd)))))
    results))

(defun company-cmake--meta (prefix)
  (let ((cmd-opts (gethash prefix company-cmake--meta-command-cache))
        result)
    (with-temp-buffer
      (call-process company-cmake-executable nil t nil cmd-opts prefix)
      ;; Go to the third line, trim it and return the result.
      ;; Tested with cmake 2.8.9.
      (goto-char (point-min))
      (forward-line 2)
      (setq result (buffer-substring-no-properties (line-beginning-position)
                                                   (line-end-position)))
      (setq result (replace-regexp-in-string "^[ \t\n\r]+" "" result))
      result)))

(defun company-cmake (command &optional arg &rest ignored)
  "`company-mode' completion back-end for CMake.
CMake is a cross-platform, open-source make system."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-cmake))
    (init (when (memq major-mode company-cmake-modes)
            (unless company-cmake-executable
              (error "Company found no cmake executable"))))
    (prefix (and (memq major-mode company-cmake-modes)
                 (not (company-in-string-or-comment))
                 (company-grab-symbol)))
    (candidates (company-cmake--candidates arg))
    (meta (company-cmake--meta arg))))

(provide 'company-cmake)
;;; company-cmake.el ends here