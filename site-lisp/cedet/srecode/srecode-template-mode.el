;;; srecode-template-mode.el --- Major mode for writing screcode macros

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; 

(require 'srecode-template)
(require 'semantic)
(require 'wisent)

;;; Code:
(defvar srecode-template-mode-syntax-table
  (let ((table (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?\; ". 12"  table) ;; SEMI, Comment start ;;
    (modify-syntax-entry ?\n ">"     table) ;; Comment end
    (modify-syntax-entry ?\" "\""    table) ;; String
    (modify-syntax-entry ?\- "_"     table) ;; Symbol
    (modify-syntax-entry ?\: "_"     table) ;; Symbol
    (modify-syntax-entry ?\\ "\\"    table) ;; Quote
    (modify-syntax-entry ?\` "'"     table) ;; Prefix ` (backquote)
    (modify-syntax-entry ?\' "'"     table) ;; Prefix ' (quote)
    (modify-syntax-entry ?\, "'"     table) ;; Prefix , (comma)
    
    table)
  "Syntax table used in semantic recoder macro buffers.")

(defvar srecode-font-lock-keywords
  '(
    ;; Template
    ("^\\(template\\)\\s-+\\(\\w*\\)\\(\\( \\(:\\w+\\)\\|\\)+\\)$"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face)
     (3 font-lock-builtin-face ))
    ;; Variable type setting
    ("^\\(set\\)\\s-+\\(\\w+\\)\\s-+\""
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))
    ;; Context type setting
    ("^\\(context\\)\\s-+\\(\\w+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-builtin-face))
    ;; Prompting setting
    ("^\\(prompt\\)\\s-+\\(\\w+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))
    ("\\(default\\)\\s-+\\(\\w+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-type-face))
    ;; Macro separators
    ("^----$" 0 'bold)
    )
  "Keywords for use with srecode macros and font-lock.")

(defvar srecode-template-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "\C-c\C-c" 'srecode-compile-templates)
    km)
  "Keymap used in srecode mode.")

;;;###autoload
(defun srecode-template-mode ()
  "Major-mode for writing srecode macros."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'srecode-template-mode
        mode-name "SRecoder"
	comment-start ";;"
	comment-end "")
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  (set-syntax-table srecode-template-mode-syntax-table)
  (use-local-map srecode-template-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(srecode-font-lock-keywords
         nil  ;; perform string/comment fontification
         nil  ;; keywords are case sensitive.
         ;; This puts _ & - as a word constituant,
         ;; simplifying our keywords significantly
         ((?_ . "w") (?- . "w"))))
  (srecode-template-setup-parser)
  (run-hooks 'srecode-template-mode-hook)
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.srt$" . srecode-template-mode))

;;; MMM-Mode support ??
(condition-case foo
    (require 'mmm-mode)
  (error (message "SRecoder Template Mode: No multi-mode not support.")))

(defun srecode-template-add-submode ()
  "Add a submode to the current template file using mmm-mode.
If mmm-mode isn't available, then do nothing."
  (if (not (featurep 'mmm-mode))
      nil  ;; Nothing to do.
    ;; Else, set up mmm-mode in this buffer.
    (let ((submode (semantic-find-tags-by-name "mode")))
      (if (not submode)
	  nil  ;; Nothing to do.
	;; Well, we have a mode, lets try turning on mmm-mode.

	(mmm-mode-on)
    
	

	))))


(provide 'srecode-template-mode)

;;; srecode-template-mode.el ends here
