;;; srecode-template.el --- SRecoder template language parser support.

;;; Copyright (C) 2005, 2007, 2008 Eric M. Ludlam

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
;; Parser setup for the semantic recoder template parser.

;;; Code:
(require 'semantic-wisent)
(require 'semantic)
(require 'srecode-template-wy)

(define-mode-local-override semantic-tag-components
  srecode-template-mode (tag)
  "Return sectiondictionary tags."
  (when (semantic-tag-of-class-p tag 'function)
    (let ((dicts (semantic-tag-get-attribute tag :dictionaries))
	  (ans nil))
      (while dicts
	(setq ans (append ans (cdr (car dicts))))
	(setq dicts (cdr dicts)))
      ans)      
    ))

;;;###autoload
(defun srecode-template-setup-parser ()
  "Setup buffer for parse."
  (srecode-template-wy--install-parser)

  (setq
   ;; Lexical Analysis
   semantic-lex-analyzer 'wisent-srecode-template-lexer
   ;; Parsing
   ;; Environment
   semantic-imenu-summary-function 'semantic-format-tag-name
   imenu-create-index-function 'semantic-create-imenu-index
   semantic-command-separation-character "\n"
   semantic-lex-comment-regex ";;"
   ;; Speedbar
   semantic-symbol->name-assoc-list
   '((function . "Template")
     (variable . "Variable")
     )
   ;; Navigation
   senator-step-at-tag-classes '(function variable)
   ))

;;;###autoload
(add-hook 'srecode-template-mode-hook 'srecode-template-setup-parser)

(provide 'srecode-template)

;;; srecode-template.el ends here
