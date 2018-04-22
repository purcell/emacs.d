;;; smarty-mode.el --- major mode for editing smarty templates

;; Copyright (C) 2003 Benj Carson

;; Maintainer: Benj Carson
;; Keywords: smarty php languages templates
;; Package-Version: 20100703.458
;; Created: 2003-08-23
;; Modified: 2003-09-06
;; X-URL:   none yet

(defconst smarty-version "0.1.0"
  "Smarty Mode version number.")

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

;;; Usage

;; Rename this file to smarty-mode.el if it isn't already then place it in
;; your Emacs lisp path (eg. site-lisp) and add to your .emacs file:
;;   (require 'smarty-mode)

;; If you want colorization, turn on global-font-lock or
;; add this to your .emacs:
;;   (add-hook 'smarty-mode-user-hook 'turn-on-font-lock)

;;; Commentary:

;; Smarty-mode is for highlighting html templates written for the smarty
;; template engine (http://smarty.php.net).  This mode *does not* inherit
;; from html-mode since that mode seems like it might interfere with 
;; what template designers might be trying to do.  This mode makes an 
;; attempt to deliberately ignore html and highlight only smarty code.

;; Since this is my first go at writing a major mode, this is pretty basic,
;; and may very well contain bugs.

;;; Changelog:

;; 0.1.0 Here goes nothing...  Note strings are not highlighted since I
;;       have yet to figure out how to get them highlighted only within
;;       { and }.

;;; Code:

;; mode-hook allows user to execute their own code when the mode is run,
;; mode-map allows user-defined keymaps
(require 'font-lock)
(require 'regexp-opt)

(defvar smarty-mode-hook nil)
(defvar smarty-mode-map nil
  "Keymap for Smarty major mode")

;; Assign default keymap
(if smarty-mode-map nil
  (setq smarty-mode-map (make-keymap)))

;;;###autoload
(setq auto-mode-alist
	  (append
	   '(("\\.tpl\\'" . smarty-mode))
	   auto-mode-alist))

(defconst smarty-functions
  (eval-when-compile
	(regexp-opt
	 '(;; standard built-in & custom functions (i.e. those listed in the docs)
	   "capture" "config_load" "foreach" "foreachelse" "include" 
	   "include_php" "insert" "if" "elseif" "else" "ldelim" "rdelim"
	   "literal" "php" "section" "sectionelse" "strip" "assign" "counter"
	   "cycle" "debug" "eval" "fetch" "html_checkboxes" "html_image"
	   "html_option" "html_radios" "html_select_date" "html_select_time"
	   "html_table" "math" "mailto" "popup_init" "popup" "textformat") t))
	"Smarty built-in & custom functions.")

(defconst smarty-constants
  (eval-when-compile
	(regexp-opt
	 '("TRUE" "FALSE" "NULL") t))
  "Smarty constants.")
	   
	
(defconst smarty-font-lock-keywords-1
  (list
   
   ;; Fontify built-in functions
   (cons
	(concat "\\<\\(" smarty-functions "\\)\\>")
	'font-lock-keyword-face)

   (cons
	(concat "\\<\\(" smarty-constants "\\)\\>")
	'font-lock-constant-face)

   )
  "Subdued level highlighting for Smarty mode.") 

(defconst smarty-font-lock-keywords-2
  (append
   smarty-font-lock-keywords-1  
   (list

	;; Fontify variable names (\\sw\\|\\s_\\) matches any word character +
	;; underscore
	'("\\$\\(\\(?:\\sw\\|\\s_\\)+\\)" (1 font-lock-variable-name-face)) ; $variable
	'("->\\(\\(?:\\sw\\|\\s_\\)+\\)" (1 font-lock-variable-name-face t t)) ; ->variable
	'("\\.\\(\\(?:\\sw\\|\\s_\\)+\\)" (1 font-lock-variable-name-face t t)) ; .variable
	'("->\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" (1 font-lock-function-name-face t t)) ; ->function_call
	'("\\<\\(\\(?:\\sw\\|\\s_\\)+\\s-*\\)(" (1 font-lock-function-name-face)) ; word(
	'("\\<\\(\\(?:\\sw\\|\\s_\\)+\\s-*\\)[[]" (1 font-lock-variable-name-face)) ; word[
	'("\\<[0-9]+" . default)			; number (also matches word)

	;; Fontify strings
	;'("\"\\([^\"]\\)\"[^\"]+"
	;  (1 font-lock-string-face t t))
	))
  
   "Medium level highlighting for Smarty mode.")

(defconst smarty-font-lock-keywords-3
  (append
   smarty-font-lock-keywords-2
   (list
	;; Fontify modifiers
	'("[^|]|\\{1\\}\\([^:|}\n]+\\)"
	  (1 font-lock-function-name-face t t))
	;; Fontify config vars
	'("{\\(#\\(?:\\sw\\|\\s_\\)+#\\)}"
	  (1 font-lock-constant-face))))
  "Balls-out highlighting for Smarty mode.")

(defvar smarty-font-lock-keywords smarty-font-lock-keywords-3
  "Default highlighting level for Smarty mode")

;; Syntax table creation
(defvar smarty-mode-syntax-table nil
  "Syntax table for smarty-mode.")

(defun smarty-create-syntax-table ()
  (if smarty-mode-syntax-table
	  ()
	(setq smarty-mode-syntax-table (make-syntax-table))

	; Add comment start & end ({* & *})
	(modify-syntax-entry ?{ "( 1" smarty-mode-syntax-table)
	(modify-syntax-entry ?* ". 23" smarty-mode-syntax-table)
	(modify-syntax-entry ?} ") 4" smarty-mode-syntax-table)
	
	;; Make | a punctuation character
	(modify-syntax-entry ?| "." smarty-mode-syntax-table)
	;; Make " a punctuation character so highlighing works withing html strings
	(modify-syntax-entry ?\" "." smarty-mode-syntax-table)
	)
  (set-syntax-table smarty-mode-syntax-table))

;;;###autoload
(defun smarty-mode ()
  "Major mode for editing Smarty template files"
  (interactive)
  (kill-all-local-variables)
  (smarty-create-syntax-table)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
		'((smarty-font-lock-keywords)
		nil ; Keywords only (i.e. no comment or string highlighting
		t   ; case fold
		nil ; syntax-alist
		nil ; syntax-begin
		))
  
  (setq font-lock-maximum-decoration t
		case-fold-search t)

  (setq major-mode 'smarty-mode)
  (setq mode-name "Smarty")
  (run-hooks 'smarty-mode-hook)
)

(provide 'smarty-mode)
;;; smarty-mode.el ends here
