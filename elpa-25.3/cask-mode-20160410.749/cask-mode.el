;;; cask-mode.el --- major mode for editing Cask files  -*- lexical-binding: t; -*-

;; Copyright (C) 2016

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 0.2
;; Package-Version: 20160410.749
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; cask-mode is a major mode for editing Cask files. It provides syntax
;; highlighting, comment toggling and indentation.

;;; Code:

(defvar cask-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?: "_" table)
    table))

(defface cask-mode-source-face
  '((t :inherit font-lock-variable-name-face))
  "Face for known cask sources."
  :group 'cask-mode)

;; Emacs naming is inconsistent. Some major modes (e.g. elisp) use the term
;; 'keyword' to refer to 'def', 'const', whereas other major modes
;; (e.g. clojure) use the term 'keyword' for ':foo', ':bar'.
(defface cask-mode-symbol-face
  '((t :inherit font-lock-constant-face))
  "Face for highlighting symbols (e.g. :git) in Cask files."
  :group 'cask-mode)

;; TODO: is this necessary?
(defvar cask-mode-source-face 'cask-mode-source-face
  "Face name to use for highlighting sources.")
(defvar cask-mode-symbol-face 'cask-mode-symbol-face
  "Face name to use for highlighting sources.")

(defvar cask-mode-font-lock-keywords
  `((,(regexp-opt
       ;; Full list taken from http://cask.readthedocs.org/en/latest/guide/dsl.html
       '("package" "package-file" "files" "depends-on" "development" "source")
       'symbols)
     . font-lock-keyword-face)
    (,(regexp-opt
       '("gnu" "melpa-stable" "melpa" "marmalade" "SC" "org")
       'symbols)
     . cask-mode-source-face)
    (,(rx symbol-start
          (or ":github" ":gitlab" "bitbucket" "wiki"
              ":git" ":bzr" ":hg" ":darcs" ":fossil" ":svn" ":cvs")
          symbol-end)
     . cask-mode-symbol-face)))

;;;###autoload
(define-derived-mode cask-mode prog-mode "Cask"
  "Major mode for editing Cask files.
See http://cask.readthedocs.org/en/latest/guide/dsl.html
for more details on the DSL accepted by Cask."
  :syntax-table cask-mode-syntax-table
  (setq font-lock-defaults '(cask-mode-font-lock-keywords))
  ;; FIXME: toggling comments only applies to the current line,
  ;; breaking multiline sexps.
  (setq-local comment-start ";; ")
  (setq-local comment-end "")
  (setq-local indent-line-function #'lisp-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("/Cask\\'" . cask-mode))

(provide 'cask-mode)
;;; cask-mode.el ends here
