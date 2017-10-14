;;; toml-mode.el --- Major mode for editing TOML files -*- lexical-binding: t -*-

;; Copyright (C) 2013 Felix Chern

;; Author: Felix Chern <idryman@gmail.com>
;; Keywords: data toml
;; Package-Version: 20161107.1000
;; Version: 0.1.3
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
;; URL: https://github.com/dryman/toml-mode.el

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;; This is a major mode for editing files in TOML data format

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'align)
(require 'conf-mode)

(defvar toml-mode-font-lock-keywords
  `((,(regexp-opt '("true" "false") 'symbols) . font-lock-constant-face)
    ("^[ \t]*\\[\\{1,2\\}\\(.+[^]]\\)\\]\\{1,2\\}" 1 'font-lock-type-face)
    ,@conf-font-lock-keywords))

(defvar toml-mode-syntax-table
  (let ((table (make-syntax-table conf-mode-syntax-table)))
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\" "\"    " table)
    ;; override
    (modify-syntax-entry ?\; "." table)
    table)
  "Syntax table in use `toml-mode' buffers.")

(defconst toml-mode-align-rules
  '((toml-equals
     (regexp . "\\(\\s-*\\)=\\(\\s-*\\)")
     (group  . (1 2))
     (modes  . '(toml-mode))
     (separate . entire)))
  "Align rules for Toml Mode.")

;;;###autoload
(define-derived-mode toml-mode conf-mode "Toml"
  "Major mode for TOML files.

Comments start with `#'.

For details see `https://github.com/toml-lang/toml'."
  (conf-mode-initialize "#" 'toml-mode-font-lock-keywords)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'tab-stop-list) (cl-loop for n from 4 below 120 by 4 collect n))
  (set (make-local-variable 'indent-tabs-mode) nil)
  (setq align-mode-rules-list toml-mode-align-rules))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode))

(provide 'toml-mode)

;;; toml-mode.el ends here
