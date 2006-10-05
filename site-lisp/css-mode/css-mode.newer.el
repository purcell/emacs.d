;;; css-mode.el

;; css-mode.el             -*- Emacs-Lisp -*-

;; Mode for editing Cascading Style Sheets

;; Created:    <Sat Feb 12 13:51:49 EST 2000>
;; Time-stamp: <2002-11-25 10:21:39 foof>
;; Author:     Alex Shinn <foof@synthcode.com>
;; Version:    0.3
;; Keywords:   html, style sheets, languages

;; Copyright (C) 2000-2002 Alex Shinn

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;;
;; This file provides a major mode for editing level 1 and 2 Cascading
;; Style Sheets.  It offers syntax highlighting, indentation, and
;; auto-completion of various CSS elements.
;;
;; To use it, put the following in your .emacs:
;;
;; (autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
;;
;; You may also want something like:
;;
;; (setq auto-mode-alist
;;       (append '(("\\.css$" . css-mode))
;;               auto-mode-alist))
;;

;;; ChangeLog:
;;
;; 2002/11/25 (version 0.3):
;;   * changed to use indent-to to obey tab preference (Vasily Korytov)


(defgroup css nil
  "Customizations for editing Cascading Style Sheets"
  :group 'languages)

(defcustom css-mode-hook nil
  "*Hook to be run when `css-mode' is entered."
  :group 'css
  :type  'hook)

(defcustom css-electric-semi-behavior nil
  "If non-nil semicolons are electric in css mode"
  :group 'css
  :type  'boolean)

(defcustom css-electric-brace-behavior nil
  "If non-nil braces are electric in css mode"
  :group 'css
  :type  'boolean)

(defcustom css-indent-offset 4
  "Number of spaces to indent lines in CSS mode"
  :group 'css
  :type  'integer)

(defcustom css-tab-mode 'auto
  "Behavior of tab in CSS mode"
  :group 'css
  :type  '(choice (const :tag "Always insert" insert)
                  (const :tag "Always indent" indent)
                  (const :tag "Always complete" complete)
                  (const :tag "Auto" auto) ))

(defvar css-mode-abbrev-table nil
  "Abbreviation table used in `css-mode' buffers.")
(define-abbrev-table 'css-mode-abbrev-table ())


(defvar css-at-rule-keywords nil
  "Keywords for CSS at rules" )
(if css-at-rule-keywords nil
  (setq css-at-rule-keywords
        '("import" "media" "page" "font-face" "charset") ))

(defvar css-at-rule-table nil
  "Table for CSS at rules" )
(if css-at-rule-table nil
  (setq css-at-rule-table (make-vector 5 0))
  (mapcar (lambda (x) (intern x css-at-rule-table))
          css-at-rule-keywords ))

(defvar css-element-keywords nil
  "Common CSS elements" )
(if css-element-keywords nil
  (setq css-element-keywords 
        '("A" "ADDRESS" "B" "BLOCKQUOTE" "BODY" "BR" "CITE"
          "CODE" "DIR" "DIV" "DD" "DL" "DT" "EM" "FORM" "H1"
          "H2" "H3" "H4" "H5" "H6" "HR" "I" "IMG" "KBD" "LI"
          "MENU" "OL" "P" "PRE" "SAMP" "SPAN" "STRONG" "TABLE"
          "TR" "TH" "TD" "TT" "UL" "VAR" )))

(defvar css-element-table nil
  "Table for CSS elements" )
(if css-element-table nil
  (setq css-element-table (make-vector 5 0))
  (mapcar (lambda (x) (intern x css-element-table))
          css-element-keywords ))


(defvar css-property-keywords nil "CSS properties" )
(if css-property-keywords nil
  (setq css-property-keywords
'("azimuth" "background" "background-attachment" "background-color"
  "background-image" "background-position" "background-repeat" "border"
  "border-collapse" "border-color" "border-spacing" "border-style"
  "border-top" "border-right" "border-bottom" "border-left"
  "border-top-color" "border-right-color" "border-bottom-color"
  "border-left-color" "border-top-style" "border-right-style"
  "border-bottom-style" "border-left-style" "border-top-width"
  "border-right-width" "border-bottom-width" "border-left-width"
  "border-width" "bottom" "caption-side" "clear" "clip" "color"
  "content" "counter-increment" "counter-reset" "cue" "cue-after"
  "cue-before" "cursor" "direction" "display" "elevation" "empty-cells"
  "float" "font" "font-family" "font-size" "font-size-adjust"
  "font-stretch" "font-style" "font-variant" "font-weight" "height"
  "left" "letter-spacing" "line-height" "list-style" "list-style-image"
  "list-style-position" "list-style-type" "margin" "margin-top"
  "margin-right" "margin-bottom" "margin-left" "marker-offset" "marks"
  "max-height" "max-width" "min-height" "min-width" "orphans" "outline"
  "outline-color" "outline-style" "outline-width" "overflow" "padding"
  "padding-top" "padding-right" "padding-bottom" "padding-left" "page"
  "page-break-after" "page-break-before" "page-break-inside" "pause"
  "pause-after" "pause-before" "pitch" "pitch-range" "play-during"
  "position" "quotes" "richness" "right" "size" "speak" "speak-header"
  "speak-numeral" "speak-punctuation" "speech-rate" "stress"
  "table-layout" "text-align" "text-decoration" "text-indent"
  "text-shadow" "text-transform" "top" "unicode-bidi" "vertical-align"
  "visibility" "voice-family" "volume" "white-space" "widows" "width"
  "word-spacing" "z-index" )))

(defvar css-property-table nil
  "Table for CSS properties" )
(if css-property-table nil
  (setq css-property-table (make-vector 5 0))
  (mapcar (lambda (x) (intern x css-property-table))
          css-property-keywords ))


;; Three levels of highlighting

(defconst css-font-lock-keywords-1 nil
  "Subdued level highlighting for C modes.")

(defconst css-font-lock-keywords-2 nil
  "Medium level highlighting for C modes.")

(defconst css-font-lock-keywords-3 nil
  "Gaudy level highlighting for C modes.")

(defvar css-font-keywords nil
  "Font lock keywords for `css-mode'." )

(let* ((css-keywords  "\\(url\\|![ \t]*important\\)")
       (css-nmstart   "[a-zA-Z]")
       (css-nmchar    "[a-zA-Z0-9-]")
       (css-ident     (concat css-nmstart css-nmchar "*"))
       (css-at-rule   (concat "\\(@" css-ident "\\)"))
       (css-element-s (concat "^\\(" css-ident "\\)"))
       (css-element (concat "\\(?:[,+>][ \t]*\\)\\(" css-ident "\\)"))
       (css-class  (concat css-element "?\\.\\(" css-ident "\\)"))
       (css-pseudo (concat ":\\(" css-ident "\\)"))
       (css-attr (concat "\\[\\(" css-ident "\\)\\]"))
       (css-id (concat "#\\(" css-ident "\\)"))
       (css-declaration (concat "[ \t][ \t]*\\(\\<" css-ident "\\>\\):")) )
  (setq css-font-lock-keywords-1
   (list
    (list css-keywords    1 'font-lock-keyword-face)
    (list css-at-rule     1 'font-lock-keyword-face)
    (list css-element-s   1 'font-lock-function-name-face)
    (list css-element     1 'font-lock-function-name-face)
    (list css-class       2 'font-lock-type-face)
    (list css-pseudo      1 'font-lock-constant-face)
    (list css-attr        1 'font-lock-variable-name-face)
    (list css-id          1 'font-lock-string-face)
    (list css-declaration 1 'font-lock-variable-name-face) ))
  (setq css-font-lock-keywords-2 css-font-lock-keywords-1)
  (setq css-font-lock-keywords-3 css-font-lock-keywords-2) )

(defvar css-mode-syntax-table nil
  "Syntax table used in `css-mode' buffers.")

(if css-mode-syntax-table nil
  (setq css-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?+  "."    css-mode-syntax-table)
  (modify-syntax-entry ?=  "."    css-mode-syntax-table)
  (modify-syntax-entry ?<  "."    css-mode-syntax-table)
  (modify-syntax-entry ?>  "."    css-mode-syntax-table)
  (modify-syntax-entry ?-  "w"    css-mode-syntax-table)
  (modify-syntax-entry ?/  "w"    css-mode-syntax-table)
  (modify-syntax-entry ?.  "w"    css-mode-syntax-table)
  (modify-syntax-entry ?\' "\""   css-mode-syntax-table)
  (cond
   ;; XEmacs 19 & 20
   ((memq '8-bit c-emacs-features)
    (modify-syntax-entry ?/  ". 1456" css-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23"   css-mode-syntax-table))
   ;; Emacs 19 & 20
   ((memq '1-bit c-emacs-features)
    (modify-syntax-entry ?/  ". 124b" css-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23"   css-mode-syntax-table))
   ;; incompatible
   (t (error "CSS Mode is incompatible with this version of Emacs")) )
  (modify-syntax-entry ?\n "> b"  css-mode-syntax-table)
  ;; Give CR the same syntax as newline, for selective-display
  (modify-syntax-entry ?\^m "> b" css-mode-syntax-table) )


(defvar css-mode-map nil
  "Keymap used in `css-mode' buffers.")

(if css-mode-map nil
  (setq css-mode-map (make-sparse-keymap))
  (define-key css-mode-map ";"        'css-electric-semicolon)
  (define-key css-mode-map "{"        'css-electric-brace)
  (define-key css-mode-map "}"        'css-electric-brace)
  (define-key css-mode-map "\t"       'css-tab-function)
  (define-key css-mode-map "\C-c\C-c" 'css-comment-region)
  (define-key css-mode-map "\C-c\C-a" 'css-complete-at-keyword)
  (define-key css-mode-map "\C-c\C-e" 'css-complete-element)
  (define-key css-mode-map "\C-c\C-p" 'css-complete-property) )


;;; Utility functions

(defun css-in-comment-p ()
  "Check whether we are currently in a comment"
  (let ((here (point)))
    (and (search-backward "/*" nil t)
         (prog1
             (not (search-forward "*/" here t))
           (goto-char here) ))))


(defun css-complete-symbol (&optional table predicate prettify)
  (let* ((end (point))
	 (beg (save-excursion
		(skip-syntax-backward "w")
		(point)))
	 (pattern (buffer-substring beg end))
	 (table (or table obarray))
	 (completion (try-completion pattern table predicate)))
    (cond ((eq completion t))
	  ((null completion)
	   (error "Can't find completion for \"%s\"" pattern))
	  ((not (string-equal pattern completion))
	   (delete-region beg end)
	   (insert completion))
	  (t
	   (message "Making completion list...")
	   (let ((list (all-completions pattern table predicate)))
	     (if prettify
		 (setq list (funcall prettify list)))
	     (with-output-to-temp-buffer "*Help*"
	       (display-completion-list list)))
	   (message "Making completion list...%s" "done")))))


(defun css-indent-line ()
  "Indent the current line"
  (if (or (css-in-comment-p)
          (looking-at "[ \t]*/\\*") )
      nil
    (save-excursion
      (let ((here (point))
            (depth 0))
        (while (and (forward-line -1)
                    (or (looking-at "^[ \t]*$")
                        (css-in-comment-p) ))
          ; Jump to a non comment/white-space line
          )
        (cond ((looking-at "\\([ \t]*\\)\\([^ \t].*\\)?{[ \t]*$")
               (setq depth (+ (- (match-end 1) (match-beginning 1))
                              css-indent-offset )))
              ((looking-at "\\([ \t]*\\)[^ \t]")
               (setq depth (- (match-end 1) (match-beginning 1))) )
              (t (setq depth 0)) )
        (goto-char here)
        (beginning-of-line)
        (if (looking-at "[ \t]*}")
            (setq depth (max (- depth css-indent-offset) 0)) )
        (if (looking-at "\\([ \t]*\\)")
            (if (= depth (- (match-end 1) (match-beginning 1)))
                nil
              (delete-region (match-beginning 1) (match-end 1))
              (indent-to depth))
          (if (> depth 0)
              (indent-to depth)))))
    (if (looking-at "[ \t]*")
        (end-of-line) )))


(defun css-indent-region (start end)
  "Indent the current line"
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (and (not (eobp)) (forward-line 1))
        (css-indent-line) ))))


;;; Commands

(defun css-electric-semicolon (arg)
  "Insert a semi-colon, and possibly indent line.
If numeric argument is not given, or is 1, auto-indent according to
`css-electric-semi-behavior'.  If arg is 0, do not auto-indent, if
arg is 2 always auto-indent, and if arg is anything else invert the
usual behavior."
  (interactive "P")
  ;; insert a semicolon
  (self-insert-command 1)
  ;; maybe do electric behavior
  (or (css-in-comment-p)
      (and (eq arg 1)
           css-electric-semi-behavior
           (css-indent-line) )
      (and (eq arg 2)
           (css-indent-line) )
      (eq arg 0)
      (or (not css-electric-semi-behavior)
          (css-indent-line) )))


(defun css-electric-brace (arg)
  "Insert a brace, and possibly indent line.
If numeric argument is not given, or is 1, auto-indent according to
`css-electric-brace-behavior'.  If arg is 0, do not auto-indent, if
arg is 2 always auto-indent, and if arg is anything else invert the
usual behavior."
  (interactive "P")
  ;; insert a brace
  (self-insert-command 1)
  ;; maybe do electric behavior
  (or (css-in-comment-p)
      (and (eq arg 1)
           css-electric-brace-behavior
           (css-indent-line) )
      (and (eq arg 2)
           (css-indent-line) )
      (eq arg 0)
      (or (not css-electric-brace-behavior)
          (css-indent-line) )))

(defun css-complete-at-keyword ()
  "Complete the standard element at point"
  (interactive)
  (let ((completion-ignore-case t))
    (css-complete-symbol css-at-rule-table) ))

(defun css-complete-element ()
  "Complete the standard element at point"
  (interactive)
  (let ((completion-ignore-case t))
    (css-complete-symbol css-element-table) ))

(defun css-complete-property ()
  "Complete the standard element at point"
  (interactive)
  (let ((completion-ignore-case t))
    (css-complete-symbol css-property-table) ))


(defun css-tab-function (&optional arg)
  "Function to call when tab is pressed in CSS mode.

With a prefix arg, insert a literal tab.  Otherwise behavior depends
on the value of `css-tab-mode'.  If it's 'insert, insert a literal
tab.  If it's 'indent, indent the current line, and if it's 'complete,
try to complete the expression before point.  A value of 'auto means
to inspect the current line, and indent if point is at the beginning
or end of the line, but complete if it's at a word.

There are three possible completions to perform:
`css-complete-at-keyword' if the point is after an '@',
`css-complete-property' if point is inside a block, and
`css-complete-element' otherwise."
  (interactive "P")
  (let* ((end (point))
         (start (prog2
                    (beginning-of-line)
                    (point)
                  (goto-char end) ))
         (prefix (buffer-substring start end)) )
    (cond ((or arg (eq css-tab-mode 'insert))
           (insert "\t"))
          ((eq css-tab-mode 'indent)
           (css-indent-line))
          ((and (not (eq css-tab-mode 'complete))
                (or (string-match "^[ \t]*[{}]?[ \t]*$" prefix)
                    (string-match "^.*;[ \t]*" prefix) ))
           ;; indent at the beginning or end of a line
           (css-indent-line))
          ((string-match "^.*@[a-zA-Z0-9-]*$" prefix)
           (css-complete-at-keyword))
          ((string-match "^\\([ \t]+.*\\|.*\{[ \t]*[a-zA-Z]+\\)$" prefix)
           ;; complete properties on non-starting lines
           (css-complete-property))
          ;; otherwise try an element
          (t (css-complete-element)) )))


;;;###autoload
(defun css-mode ()
  "Major mode for editing CSS files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table css-mode-syntax-table)
  (setq major-mode 'css-mode
	mode-name "CSS"
	local-abbrev-table css-mode-abbrev-table)
  (use-local-map css-mode-map)
  ;; local variables
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'block-comment-start)
  (make-local-variable 'block-comment-end)
  (make-local-variable 'block-comment-left)
  (make-local-variable 'block-comment-right)
  (make-local-variable 'block-comment-top-right)
  (make-local-variable 'block-comment-bot-left)
  (make-local-variable 'block-comment-char)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((css-font-lock-keywords-1
                              css-font-lock-keywords-2
                              css-font-lock-keywords-3)))
  ;; now set their values
  (setq parse-sexp-ignore-comments t
	comment-start-skip "/\\*+ *\\|// *"
	comment-start "/\\*"
	comment-end   "\\*/")
  (setq block-comment-start     "/*"
        block-comment-end       "*/"
        block-comment-left      " * "
        block-comment-right     " *"
        block-comment-top-right ""
        block-comment-bot-left  " "
        block-comment-char      ?* )
  (setq indent-line-function   'css-indent-line
        indent-region-function 'css-indent-region
	paragraph-ignore-fill-prefix t
	paragraph-start (concat "\\|$" page-delimiter)
	paragraph-separate paragraph-start)
  (run-hooks 'css-mode-hook))


(provide 'css-mode)

;;; css-mode.el ends here.
