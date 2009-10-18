;;; lua-mode.el --- a major-mode for editing Lua scripts

;; Copyright (C) 1997, 2001, 2004, 2006, 2007 Free Software Foundation, Inc.

;; Author: 2006 Juergen Hoetzel <juergen@hoetzel.info>
;;         2004 various (support for Lua 5 and byte compilation)
;;         2001 Christian Vogler <cvogler@gradient.cis.upenn.edu>
;;         1997 Bret Mogilefsky <mogul-lua@gelatinous.com> starting from
;;              tcl-mode by Gregor Schmid <schmid@fb3-s7.math.tu-berlin.de>
;;              with tons of assistance from
;;              Paul Du Bois <pld-lua@gelatinous.com> and
;;              Aaron Smith <aaron-lua@gelatinous.com>.
;; URL:		http://lua-mode.luaforge.net/
;; Version:	20070703
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

(defconst lua-version "20071122"
  "Lua Mode version number.")

;; Keywords: languages, processes, tools


;;; Commentary:

;; Thanks to Tobias Polzin <polzin<at>gmx.de> for function indenting
;; patch: Indent "(" like "{"

;; Thanks to  Fabien <fleutot<at>gmail.com> for imenu patches.

;; Special Thanks to Simon Marshall <simonm@mail.esrin.esa.it> for
;; font-lock patches.

;; Additional font-lock highlighting and indentation tweaks by
;; Adam D. Moss <adam@gimp.org> <aspirin@icculus.org>

;; This file was written with emacs using Jamie Lokier's folding mode
;; That's what the funny ;;{{{ ;;}}} marks are there for

;;{{{ INSTALLATION:

;; To install, just drop this file into a directory on your load-path (and
;; byte-compile it).  To set up Emacs to automatically edit files ending in
;; ".lua" using lua-mode add the following to your ~/.emacs file (GNU
;; Emacs) or ~/.xemacs/init.el file (XEmacs):
;;    (setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
;;    (autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;;}}}
;;{{{ Usage

;; Lua-mode supports c-mode style formatting and sending of
;; lines/regions/files to a Lua interpreter. An interpreter (see
;; variable `lua-default-application') will be started if you try to
;; send some code and none is running. You can use the process-buffer
;; (named after the application you chose) as if it were an
;; interactive shell. See the documentation for `comint.el' for
;; details.

;;}}}
;;{{{ Key-bindings

;; To see all the keybindings for Lua mode, look at `lua-setup-keymap'
;; or start `lua-mode' and type `\C-h m'.
;; The keybindings may seem strange, since I prefer to use them with
;; lua-prefix-key set to nil, but since those keybindings are already used
;; the default for `lua-prefix-key' is `\C-c', which is the conventional
;; prefix for major-mode commands.

;; You can customise the keybindings either by setting `lua-prefix-key'
;; or by putting the following in your .emacs
;;      (setq lua-mode-map (make-sparse-keymap))
;; and
;;      (define-key lua-mode-map <your-key> <function>)
;; for all the functions you need.

;;}}}

;;; Code:
(defconst lua-using-xemacs (string-match "XEmacs" emacs-version)
  "Nil unless using XEmacs).")

;; We need that !
(require 'comint)

;;{{{ variables

;; Local variables
(defgroup lua nil
  "Major mode for editing lua code."
  :prefix "lua-"
  :group 'languages)

(defcustom lua-default-application "lua"
  "Default application to run in lua subprocess."
  :type 'string
  :group 'lua)

(defcustom lua-default-command-switches (list "-i")
  "Command switches for `lua-default-application'.
Should be a list of strings."
  :type '(repeat string)
  :group 'lua)

(defcustom lua-always-show t
  "*Non-nil means display lua-process-buffer after sending a command."
  :type 'boolean
  :group 'lua)

(defcustom lua-search-url-prefix "http://www.lua.org/manual/5.1/manual.html#pdf-"
  "*URL at which to search for documentation on a word"
  :type 'string
  :group 'lua)

(defvar lua-process nil
  "The active Lua subprocess")

(defvar lua-process-buffer nil
  "Buffer used for communication with Lua subprocess")

(defvar lua-mode-map nil
  "Keymap used with lua-mode.")

(defvar lua-electric-flag t
"If t, electric actions (like automatic reindentation)  will happen when an electric
 key like `{' is pressed") 
(make-variable-buffer-local 'lua-electric-flag)

(defcustom lua-prefix-key "\C-c"
  "Prefix for all lua-mode commands."
  :type 'string
  :group 'lua)

(defcustom lua-prompt-regexp "[^\n]*\\(>[\t ]+\\)+$"
  "Regexp which matches the Lua program's prompt."
  :group 'lua
  :type  'regexp
  )

(defcustom lua-traceback-line-re
  "^\\(?:[\t ]*\\|.*>[\t ]+\\)\\([^\n\t ]+\\):\\([0-9]+\\):"
  "Regular expression that describes tracebacks and errors."
  :group 'lua
  :type  'regexp
  )

(defcustom lua-jump-on-traceback t
  "*Jump to innermost traceback location in *lua* buffer.  When this
variable is non-nil and a traceback occurs when running Lua code in a
subprocess, jump immediately to the source code of the innermost
traceback location."
  :group 'lua
  :type 'boolean
  )

(defvar lua-mode-hook nil
  "Hooks called when Lua mode fires up.")

(defvar lua-region-start (make-marker)
  "Start of special region for Lua communication.")

(defvar lua-region-end (make-marker)
  "End of special region for Lua communication.")

(defvar lua-indent-level 3
  "Amount by which Lua subexpressions are indented.")

(defvar lua-mode-menu (make-sparse-keymap "Lua")
  "Keymap for lua-mode's menu.")

(defvar lua-xemacs-menu
  '(["Restart With Whole File" lua-restart-with-whole-file t]
    ["Kill Process" lua-kill-process t]
    ["Hide Process Buffer" lua-hide-process-buffer t]
    ["Show Process Buffer" lua-show-process-buffer t]
    ["Beginning Of Proc" lua-beginning-of-proc t]
    ["End Of Proc" lua-end-of-proc t]
    ["Set Lua-Region Start" lua-set-lua-region-start t]
    ["Set Lua-Region End" lua-set-lua-region-end t]
    ["Send Lua-Region" lua-send-lua-region t]
    ["Send Current Line" lua-send-current-line t]
    ["Send Region" lua-send-region t]
    ["Send Proc" lua-send-proc t]
    ["Send Buffer" lua-send-buffer t]
    ["Search Documentation" lua-search-documentation t])
  "XEmacs menu for Lua mode.")

(defvar lua-font-lock-keywords
  (eval-when-compile
    (list
     ;;
     ;; Function name declarations.
     '("^[ \t]*\\<\\(\\(local[ \t]+\\)?function\\)\\>[ \t]+\\(\\(\\sw:\\|\\sw\\.\\|\\sw_\\|\\sw\\)+\\)"
       (1 font-lock-keyword-face) (3 font-lock-function-name-face nil t))
     
     ;; Handle function names in assignments
     '("\\(\\(\\sw:\\|\\sw\\.\\|\\sw_\\|\\sw\\)+\\)[ \t]*=[ \t]*\\(function\\)\\>"
       (1 font-lock-function-name-face nil t) (3 font-lock-keyword-face))

     ; Highlight multi-line comment blocks; since font-lock-mode doesn't
     ; claim to handle the highlighting of multi-line expressions elegantly
     ; this works best with lazy-lock-mode if your Emacs supports it, e.g.
     ; try (setq font-lock-support-mode 'lazy-lock-mode) in your ~/.emacs

     ;; Multi-line comment blocks.
     `("\\(?:^\\|[^-]\\)\\(--\\[\\(=*\\)\\[\\(?:.\\|\n\\)*?\\]\\2\\]\\)"
       (1 font-lock-comment-face t))

     ;;
     ;; Keywords.
     ;; (concat "\\<"
     ;;         (regexp-opt '("and" "break" "do" "else" "elseif" "end" "false"
     ;;                       "for" "function" "if" "in" "local" "nil" "not"
     ;;                       "or" "repeat" "return" "then" "true" "until"
     ;;                       "while") t)
     ;;         "\\>")

     ; Insert expanded regexp-opt here for the benefit of those who
     ; don't have regexp-opt available.

     "\\<\\(and\\|break\\|do\\|e\\(lse\\(if\\)?\\|nd\\)\\|f\\(alse\\|or\\|unction\\)\\|i[fn]\\|local\\|n\\(il\\|ot\\)\\|or\\|re\\(peat\\|turn\\)\\|t\\(hen\\|rue\\)\\|until\\|while\\)\\>"

     "Default expressions to highlight in Lua mode.")))

(defvar lua-imenu-generic-expression
  '((nil "^[ \t]*\\(?:local[ \t]+\\)?function[ \t]+\\(\\(\\sw:\\|\\sw_\\|\\sw\\.\\|\\sw\\)+\\)" 1))
  "Imenu generic expression for lua-mode.  See `imenu-generic-expression'.")

(defvar lua-mode-abbrev-table nil
  "Abbreviation table used in lua-mode buffers.")

(defvar lua-sexp-alist '(("then" . "end")
                        ("function" . "end")
                        ("do" . "end")))

(define-abbrev-table 'lua-mode-abbrev-table
  '(
        ("end" "end" lua-indent-line 0)
        ("else" "else" lua-indent-line 0)
        ("elseif" "elseif" lua-indent-line 0)
        ))

(defconst lua-indent-whitespace " \t"
  "Character set that constitutes whitespace for indentation in lua.")

;;}}}
;;{{{ lua-make-temp-file

(eval-and-compile
  (defalias 'lua-make-temp-file
    (if (fboundp 'make-temp-file)
	'make-temp-file
      (lambda (prefix &optional dir-flag) ;; Simple implementation
	(expand-file-name
	 (make-temp-name prefix)
	 (if (fboundp 'temp-directory)
	     (temp-directory)
	   temporary-file-directory))))))

;;}}}
;;{{{ replace-in-string

(eval-and-compile
  (if (not (fboundp 'replace-in-string)) ;GNU emacs doesn't have it
      (defun replace-in-string  (string regexp newtext &optional literal)
	(replace-regexp-in-string regexp newtext string nil literal))))

;;}}}
;;{{{ lua-mode

;;;###autoload
(defun lua-mode ()
  "Major mode for editing Lua code.
The following keys are bound:
\\{lua-mode-map}
"
  (interactive)
  (let ((switches nil)
                  s)
    (kill-all-local-variables)
    (setq major-mode 'lua-mode)
    (setq mode-name "Lua")
    (setq comint-prompt-regexp lua-prompt-regexp)
    (make-local-variable 'lua-default-command-switches)
    (set (make-local-variable 'indent-line-function) 'lua-indent-line)
    (set (make-local-variable 'comment-start) "--")
    (set (make-local-variable 'comment-start-skip) "--")
    (set (make-local-variable 'font-lock-defaults)
                        '(lua-font-lock-keywords nil nil ((?_ . "w"))))
    (set (make-local-variable 'imenu-generic-expression)
                        lua-imenu-generic-expression)
         (setq local-abbrev-table lua-mode-abbrev-table)
         (abbrev-mode 1)
    (make-local-variable 'lua-default-eval)
    (or lua-mode-map
                  (lua-setup-keymap))
    (use-local-map lua-mode-map)
    (set-syntax-table (copy-syntax-table))
    (modify-syntax-entry ?+ ".")
    (modify-syntax-entry ?- ". 12")
    (modify-syntax-entry ?* ".")
    (modify-syntax-entry ?/ ".")
    (modify-syntax-entry ?^ ".")
    ;; This might be better as punctuation, as for C, but this way you
    ;; can treat table index as symbol.
    (modify-syntax-entry ?. "_")	; e.g. `io.string'
    (modify-syntax-entry ?> ".")
    (modify-syntax-entry ?< ".")
    (modify-syntax-entry ?= ".")
    (modify-syntax-entry ?~ ".")
    (modify-syntax-entry ?\n ">")
    (modify-syntax-entry ?\' "\"")
    (modify-syntax-entry ?\" "\"")
    ;; _ needs to be part of a word, or the regular expressions will
    ;; incorrectly regognize end_ to be matched by "\\<end\\>"!
    (modify-syntax-entry ?_ "w")
    (if (and lua-using-xemacs
	     (featurep 'menubar)
	     current-menubar
	     (not (assoc "Lua" current-menubar)))
	(progn
	  (set-buffer-menubar (copy-sequence current-menubar))
	  (add-menu nil "Lua" lua-xemacs-menu)))
    ;; Append Lua menu to popup menu for XEmacs.
    (if (and lua-using-xemacs (boundp 'mode-popup-menu))
	(setq mode-popup-menu
	      (cons (concat mode-name " Mode Commands") lua-xemacs-menu)))

    ;; hideshow setup
    (unless (assq 'lua-mode hs-special-modes-alist)
      (add-to-list 'hs-special-modes-alist
		   `(lua-mode  
		     ,(regexp-opt (mapcar 'car lua-sexp-alist) 'words);start
		     ,(regexp-opt (mapcar 'cdr lua-sexp-alist) 'words) ;end
		     nil lua-forward-sexp)))
    (run-hooks 'lua-mode-hook)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

;;}}}
;;{{{ lua-setup-keymap

(defun lua-setup-keymap ()
  "Set up keymap for Lua mode.
If the variable `lua-prefix-key' is nil, the bindings go directly
to `lua-mode-map', otherwise they are prefixed with `lua-prefix-key'."
  (setq lua-mode-map (make-sparse-keymap))
  (define-key lua-mode-map [menu-bar lua-mode]
    (cons "Lua" lua-mode-menu))
  (define-key lua-mode-map "}" 'lua-electric-match)
  (define-key lua-mode-map "]" 'lua-electric-match)
  (define-key lua-mode-map ")" 'lua-electric-match)
  (let ((map (if lua-prefix-key
                                          (make-sparse-keymap)
                                        lua-mode-map)))

         ;; communication
         (define-key map "\M-[" 'lua-beginning-of-proc)
         (define-key map "\M-]" 'lua-end-of-proc)
         (define-key map "\C-c" 'comment-region)
	 (define-key map "\C-l" 'lua-send-buffer)
	 (define-key map "\C-f" 'lua-search-documentation)
         (if lua-prefix-key
                  (define-key lua-mode-map lua-prefix-key map))
         ))

;;}}}
;;{{{ lua-electric-match

(defun lua-electric-match (arg)
  "Insert character and adjust indentation."
  (interactive "P")
  (insert-char last-command-char (prefix-numeric-value arg))
  (if lua-electric-flag 
      (lua-indent-line))
  (blink-matching-open))

;;}}}

;;{{{ private functions
(defun lua-syntax-status ()
  "Returns the syntactic status of the character after the point."
  (parse-partial-sexp (save-excursion (beginning-of-line) (point))
		      (point)))


(defun lua-string-p ()
  "Returns true if the point is in a string."
  (elt (lua-syntax-status) 3))

(defun lua-comment-p ()
  "Returns true if the point is in a comment."
    (elt (lua-syntax-status) 4))

(defun lua-comment-or-string-p ()
  "Returns true if the point is in a comment or string."
  (let ((parse-result (lua-syntax-status)))
    (or (elt parse-result 3) (elt parse-result 4))))

;;}}}
;;{{{ lua-indent-line

(defun lua-indent-line ()
  "Indent current line for Lua mode.
Return the amount the indentation changed by."
  (let ((indent (max 0 (- (lua-calculate-indentation nil)
			  (lua-calculate-indentation-left-shift))))
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward lua-indent-whitespace)
    (setq shift-amt (- indent (current-column)))
    (when (not (zerop shift-amt))
      (delete-region beg (point))
      (indent-to indent))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))
    shift-amt
    indent))

;;}}}
;;{{{ lua-find-regexp

(defun lua-find-regexp (direction regexp &optional limit ignore-p)
  "Searches for a regular expression in the direction specified.
Direction is one of 'forward and 'backward.
By default, matches in comments and strings are ignored, but what to ignore is
configurable by specifying ignore-p. If the regexp is found, returns point
position, nil otherwise.
ignore-p returns true if the match at the current point position should be
ignored, nil otherwise."
  (let ((ignore-func (or ignore-p 'lua-comment-or-string-p))
	(search-func (if (eq direction 'forward)
			 're-search-forward 're-search-backward))
	(case-fold-search nil))
    (catch 'found
      (while (funcall search-func regexp limit t)
	(if (not (funcall ignore-func))
	    (throw 'found (point)))))))

;;}}}
;;{{{ lua-backwards-to-block-begin-or-end

(defun lua-backwards-to-block-begin-or-end ()
  "Move backwards to nearest block begin or end.  Returns nil if not successful."
  (interactive)
  (lua-find-regexp 'backward lua-block-regexp))

;;}}}
;;{{{ var. constants

(defconst lua-block-regexp
  (eval-when-compile
    ;; This is the code we used to generate the regexp:
    (concat
     "\\(\\<"
     (regexp-opt '("do" "function" "repeat" "then"
		   "else" "elseif" "end" "until") t)
     "\\>\\)\\|"
     (regexp-opt '("{" "(" "[" "]" ")" "}") t))

	))

(defconst lua-block-token-alist
  ;; The absence of "else" is deliberate. This construct in a way both
  ;; opens and closes a block. As a result, it is difficult to handle
  ;; cleanly. It is also ambiguous - if we are looking for the match
  ;; of "else", should we look backward for "then/elseif" or forward
  ;; for "end"?
  ;; Maybe later we will find a way to handle it.
  '(("do"       "\\<end\\>"                                   open)
    ("function" "\\<end\\>"                                   open)
    ("repeat"   "\\<until\\>"                                 open)
    ("then"     "\\<\\(e\\(lseif\\|nd\\)\\)\\>"               open)
    ("{"        "}"                                           open)
    ("["        "]"                                           open)
    ("("        ")"                                           open)
    ("elseif"   "\\<then\\>"                                  close)
    ("end"      "\\<\\(do\\|function\\|then\\)\\>"            close)
    ("until"    "\\<repeat\\>"                                close)
    ("}"        "{"                                           close)
    ("]"        "\\["                                         close)
    (")"        "("                                           close)))


(defconst lua-indentation-modifier-regexp
    ;; The absence of else is deliberate, since it does not modify the
    ;; indentation level per se. It only may cause the line, in which the
    ;; else is, to be shifted to the left.
    ;; This is the code we used to generate the regexp:
    (concat
     "\\(\\<"
     ; n.b. "local function" is a bit of a hack, allowing only a single space
     (regexp-opt '("do" "local function" "function" "repeat" "then") t)
     "\\>\\|"
     (regexp-opt '("{" "(" "["))
     "\\)\\|\\(\\<"
     (regexp-opt '("elseif" "end" "until") t)
     "\\>\\|"
     (regexp-opt '("]" ")" "}"))
     "\\)")

    )

;;}}}
;;{{{ lua-find-matching-token-word

(defun lua-find-matching-token-word (token search-start)
  (let* ((token-info (assoc token lua-block-token-alist))
	 (match (car (cdr token-info)))
	 (match-type (car (cdr (cdr token-info))))
	 (search-direction (if (eq match-type 'open) 'forward 'backward)))
    ;; if we are searching forward from the token at the current point
    ;; (i.e. for a closing token), need to step one character forward
    ;; first, or the regexp will match the opening token.
    (if (eq match-type 'open) (forward-char 1))
    (if search-start (goto-char search-start))
    (catch 'found
      (while (lua-find-regexp search-direction lua-indentation-modifier-regexp)
	;; have we found a valid matching token?
	(let ((found-token (match-string 0))
	      (found-pos (match-beginning 0)))
	  (if (string-match match found-token)
	      (throw 'found found-pos))
	    ;; no - then there is a nested block. If we were looking for
	    ;; a block begin token, found-token must be a block end
	    ;; token; likewise, if we were looking for a block end token,
	    ;; found-token must be a block begin token, otherwise there
	    ;; is a grammatical error in the code.
	    (if (not (and
		      (eq (car (cdr (cdr (assoc found-token lua-block-token-alist))))
			  match-type)
		      (lua-find-matching-token-word found-token nil)))
	      (throw 'found nil)))))))

;;}}}
;;{{{ lua-goto-matching-block-token 

(defun lua-goto-matching-block-token (&optional search-start parse-start)
  "Find block begion/end token matching the one at the point.
This function moves the point to the token that matches the one
at the current point. Returns the point position of the first character of
the matching token if successful, nil otherwise."
  (if parse-start (goto-char parse-start))
  (let ((case-fold-search nil))
    (if (looking-at lua-indentation-modifier-regexp)
	(let ((position (lua-find-matching-token-word (match-string 0)
						      search-start)))
	  (and position
	       (goto-char position))))))


;; The following may be useful to speed up the search in the future.
;      (let ((token-type (char-syntax (string-to-char token-to-match)))
;	    matching-pos)
;	(cond ((eq token-type ?\()
;	       (setq matching-pos (scan-sexps (point) 1 (current-buffer) t))
;	       (when matching-pos (goto-char matching-pos)))

;	      ((eq token-type ?\))
;	       ;; need to move one char forward, because scan-sexps
;	       ;; expects the point to be one past the closing parenthesis
;	       (forward-char 1)
;	       (setq matching-pos (scan-sexps (point) -1 (current-buffer) t))
;	       (when matching-pos (goto-char matching-pos)))

;	      (t
;	       (lua-goto-matching-token-word token-to-match search-start)))))))


;;}}}
;;{{{ lua-goto-matching-block

(defun lua-goto-matching-block (&optional noreport)
  "Go to the keyword balancing the one under the point.
If the point is on a keyword/brace that starts a block, go to the
matching keyword that ends the block, and vice versa."
  (interactive)
  ;; search backward to the beginning of the keyword if necessary
  (if (eq (char-syntax (following-char)) ?w)
      (re-search-backward "\\<" nil t))
  (let ((position (lua-goto-matching-block-token)))
    (if (and (not position)
	     (not noreport))
	(error "Not on a block control keyword or brace.")
      position)))

;;}}}
;;{{{ lua-goto-nonblank-previous-line

(defun lua-goto-nonblank-previous-line ()
  "Puts the point at the first previous line that is not blank.
Returns the point, or nil if it reached the beginning of the buffer"
  (catch 'found
    (beginning-of-line)
    (while t
      (if (bobp) (throw 'found nil))
      (forward-char -1)
      (beginning-of-line)
      (if (not (looking-at "\\s *\\(--.*\\)?$")) (throw 'found (point))))))

;;}}}
;;{{{ lua-goto-nonblank-next-line

(defun lua-goto-nonblank-next-line ()
  "Puts the point at the first next line that is not blank.
Returns the point, or nil if it reached the end of the buffer"
  (catch 'found
    (end-of-line)
    (while t
      (forward-line)
      (if (eobp) (throw 'found nil))
      (beginning-of-line)
      (if (not (looking-at "\\s *\\(--.*\\)?$")) (throw 'found (point))))))

(eval-when-compile
  (defconst lua-operator-class
    "-+*/^.=<>~"))

;;}}}
;;{{{ var. constans

(defconst lua-cont-eol-regexp
  (eval-when-compile
    ;; expression used to generate the regexp
    (concat
     "\\(\\<"
     (regexp-opt '("and" "or" "not" "in" "for" "while"
		   "local" "function") t)
     "\\>\\|"
     "\\(^\\|[^" lua-operator-class "]\\)"
     (regexp-opt '("+" "-" "*" "/" "^" ".." "==" "=" "<" ">" "<=" ">=" "~=") t)
     "\\)"
     "\\s *\\=")

    ))


(defconst lua-cont-bol-regexp
  (eval-when-compile
    ;; expression used to generate the regexp
    (concat
     "\\=\\s *"
     "\\(\\<"
     (regexp-opt '("and" "or" "not") t)
     "\\>\\|"
     (regexp-opt '("+" "-" "*" "/" "^" ".." "==" "=" "<" ">" "<=" ">=" "~=") t)
     "\\($\\|[^" lua-operator-class "]\\)"
     "\\)")

    ))

;;}}}
;;{{{ lua-last-token-continues-p

(defun lua-last-token-continues-p ()
  "Returns true if the last token on this line is a continuation token."
  (let (line-begin
	line-end)
    (save-excursion
      (beginning-of-line)
      (setq line-begin (point))
      (end-of-line)
      (setq line-end (point))
      ;; we need to check whether the line ends in a comment and
      ;; skip that one.
      (while (lua-find-regexp 'backward "-" line-begin 'lua-string-p)
	(if (looking-at "--")
	    (setq line-end (point))))
      (goto-char line-end)
      (re-search-backward lua-cont-eol-regexp line-begin t))))

;;}}}
;;{{{ lua-first-token-continues-p

(defun lua-first-token-continues-p ()
  "Returns true if the first token on this line is a continuation token."
  (let (line-end)
    (save-excursion
      (end-of-line)
      (setq line-end (point))
      (beginning-of-line)
      (re-search-forward lua-cont-bol-regexp line-end t))))

;;}}}
;;{{{ lua-is-continuing-statement-p

(defun lua-is-continuing-statement-p (&optional parse-start)
  "Return nonnil if the line continues a statement.
More specifically, return the point in the line that is continued.
The criteria for a continuing statement are:

* the last token of the previous line is a continuing op,
  OR the first token of the current line is a continuing op

"
  (let ((prev-line nil))
    (save-excursion
      (if parse-start (goto-char parse-start))
      (save-excursion (setq prev-line (lua-goto-nonblank-previous-line)))
      (and prev-line
	   (or (lua-first-token-continues-p)
	       (and (goto-char prev-line)
		    ;; check last token of previous nonblank line
		    (lua-last-token-continues-p)))))))

;;}}}
;;{{{ lua-make-indentation-info-pair

(defun lua-make-indentation-info-pair ()
  "This is a helper function to lua-calculate-indentation-info. Don't
use standalone."
  (cond ((string-equal found-token "function")
	 ;; this is the location where we need to start searching for the
	 ;; matching opening token, when we encounter the next closing token.
	 ;; It is primarily an optimization to save some searchingt ime.
	 (cons 'absolute (+ (save-excursion (goto-char found-pos)
					    (current-column))
			    lua-indent-level)))
	((or (string-equal found-token "{")
         (string-equal found-token "("))
	 (save-excursion 
	   ;; expression follows -> indent at start of next expression
	   (if (and (not (search-forward-regexp "[[:space:]]--" (line-end-position) t))
		    (search-forward-regexp "[^[:space:]]" (line-end-position) t))
	       	 (cons 'absolute (1- (current-column)))
	     	 (cons 'relative lua-indent-level))))
	;; closing tokens follow
	((string-equal found-token "end")
	 (save-excursion
	   (lua-goto-matching-block-token nil found-pos)
	   (if (looking-at "\\<function\\>")
	       (cons 'absolute
		     (+ (current-indentation)
			(lua-calculate-indentation-block-modifier
			 nil (point))))
	     (cons 'relative (- lua-indent-level)))))
	((or (string-equal found-token ")")
	     (string-equal found-token "}"))
	 (save-excursion
	   (lua-goto-matching-block-token nil found-pos)
	   (cons 'absolute
		 (+ (current-indentation)
		    (lua-calculate-indentation-block-modifier
		     nil (point))))))
	(t
	 (cons 'relative (if (nth 2 (match-data))
			     ;; beginning of a block matched
			     lua-indent-level
			   ;; end of a block matched
			   (- lua-indent-level))))))


;;}}}
;;{{{ lua-calculate-indentation-info

(defun lua-calculate-indentation-info (&optional parse-start parse-end)
  "For each block token on the line, computes how it affects the indentation.
The effect of each token can be either a shift relative to the current
indentation level, or indentation to some absolute column. This information
is collected in a list of indentation info pairs, which denote absolute
and relative each, and the shift/column to indent to."
  (let* ((line-end (save-excursion (end-of-line) (point)))
	 (search-stop (if parse-end (min parse-end line-end) line-end))
	 (indentation-info nil))
    (if parse-start (goto-char parse-start))
    (save-excursion
      (beginning-of-line)
      (while (lua-find-regexp 'forward lua-indentation-modifier-regexp
			      search-stop)
	(let ((found-token (match-string 0))
	      (found-pos (match-beginning 0))
	      (found-end (match-end 0))
	      (data (match-data)))
	  (setq indentation-info
		(cons (lua-make-indentation-info-pair) indentation-info)))))
    indentation-info))

;;}}}
;;{{{ lua-accumulate-indentation-info

(defun lua-accumulate-indentation-info (info)
  "Accumulates the indentation information previously calculated by
lua-calculate-indentation-info. Returns either the relative indentation
shift, or the absolute column to indent to."
  (let ((info-list (reverse info))
	(type 'relative)
	(accu 0))
    (mapcar (lambda (x)
	    (setq accu (if (eq 'absolute (car x))
			   (progn (setq type 'absolute)
				  (cdr x))
			 (+ accu (cdr x)))))
	  info-list)
    (cons type accu)))

;;}}}
;;{{{ lua-calculate-indentation-block-modifier

(defun lua-calculate-indentation-block-modifier (&optional parse-start
							   parse-end)
  "Return amount by which this line modifies the indentation.
Beginnings of blocks add lua-indent-level once each, and endings
of blocks subtract lua-indent-level once each. This function is used
to determine how the indentation of the following line relates to this
one."
  (if parse-start (goto-char parse-start))
  (let ((case-fold-search nil)
	(indentation-info (lua-accumulate-indentation-info
			   (lua-calculate-indentation-info nil parse-end))))
    (if (eq (car indentation-info) 'absolute)
	(- (cdr indentation-info)
	   (current-indentation)
	   ;; reduce indentation if this line also starts new continued statement 
	   ;; or next line cont. this line
	   ;;This is for aesthetic reasons: the indentation should be
	   ;;dosomething(d +
	   ;;   e + f + g)
	   ;;not
	   ;;dosomething(d +
	   ;;      e + f + g)"
	   (save-excursion
	     (or (and (lua-last-token-continues-p) lua-indent-level)
		 (and (lua-goto-nonblank-next-line) (lua-first-token-continues-p) lua-indent-level)
		 0)))
      (+ (lua-calculate-indentation-left-shift)
	 (cdr indentation-info)
	 (if (lua-is-continuing-statement-p) (- lua-indent-level) 0)))))
;;}}}
;;{{{ constants

(defconst lua-left-shift-regexp-1
  (concat "\\("
	  "\\(\\<" (regexp-opt '("else" "elseif" "until") t)
	  "\\>\\)\\($\\|\\s +\\)"
	  "\\)"))

(defconst lua-left-shift-regexp-2
  (concat "\\(\\<"
	  (regexp-opt '("end") t)
	  "\\>\\)"))


(defconst lua-left-shift-regexp
  ;; This is the code we used to generate the regexp:
  ;; ("else", "elseif", "until" followed by whitespace, or "end"/closing
  ;; brackets followed by
  ;; whitespace, punctuation, or closing parentheses)
  (concat lua-left-shift-regexp-1
	  "\\|\\(\\("
	  lua-left-shift-regexp-2
	  "\\|\\("
	  (regexp-opt '("]" "}" ")"))
	  "\\)\\)\\($\\|\\(\\s \\|\\s.\\)*\\)"
	  "\\)"))

(defconst lua-left-shift-pos-1
  2)

(defconst lua-left-shift-pos-2
  (+ 3 (regexp-opt-depth lua-left-shift-regexp-1)))

(defconst lua-left-shift-pos-3
  (+ lua-left-shift-pos-2
     (regexp-opt-depth lua-left-shift-regexp-2)))

;;}}} 
;;{{{ lua-calculate-indentation-left-shift

(defun lua-calculate-indentation-left-shift (&optional parse-start)
  "Return amount, by which this line should be shifted left.
Look for an uninterrupted sequence of block-closing tokens that starts
at the beginning of the line. For each of these tokens, shift indentation
to the left by the amount specified in lua-indent-level."
  (let (line-begin
	(indentation-modifier 0)
	(case-fold-search nil)
	(block-token nil))
    (save-excursion
      (if parse-start (goto-char parse-start))
      (beginning-of-line)
      (setq line-begin (point))
      ;; Look for the block-closing token sequence
      (skip-chars-forward lua-indent-whitespace)
      (catch 'stop
	(while (and (looking-at lua-left-shift-regexp)
		    (not (lua-comment-or-string-p)))
	  (let ((last-token (or (match-string lua-left-shift-pos-1)
				(match-string lua-left-shift-pos-2)
				(match-string lua-left-shift-pos-3))))
	    (if (not block-token) (setq block-token last-token))
	    (if (not (string-equal block-token last-token)) (throw 'stop nil))
	    (setq indentation-modifier (+ indentation-modifier
					  lua-indent-level))
		(forward-char (length (match-string 0))))))
      indentation-modifier)))

;;}}}
;;{{{ lua-calculate-indentation

(defun lua-calculate-indentation (&optional parse-start)
  "Return appropriate indentation for current line as Lua code.
In usual case returns an integer: the column to indent to."
  (let ((pos (point))
	shift-amt)
    (save-excursion
      (if parse-start (setq pos (goto-char parse-start)))
      (beginning-of-line)
      (setq shift-amt (if (lua-is-continuing-statement-p) lua-indent-level 0))
      (if (bobp)          ; If we're at the beginning of the buffer, no change.
	  (+ (current-indentation) shift-amt)
	;; This code here searches backwards for a "block beginning/end"
	;; It snarfs the indentation of that, plus whatever amount the
	;; line was shifted left by, because of block end tokens. It
	;; then adds the indentation modifier of that line to obtain the
	;; final level of indentation.
	;; Finally, if this line continues a statement from the
	;; previous line, add another level of indentation.
	(if (lua-backwards-to-block-begin-or-end)
	    ;; now we're at the line with block beginning or end.
	    (max (+ (current-indentation)
		    (lua-calculate-indentation-block-modifier)
		    shift-amt)
		 0)
	  ;; Failed to find a block begin/end.
	  ;; Just use the previous line's indent.
	  (goto-char pos)
	  (beginning-of-line)
	  (forward-line -1)
	  (+ (current-indentation) shift-amt))))))

;;}}}
;;{{{ lua-beginning-of-proc

(defun lua-beginning-of-proc (&optional arg)
  "Move backward to the beginning of a lua proc (or similar).
With argument, do it that many times.  Negative arg -N
means move forward to Nth following beginning of proc.
Returns t unless search stops due to beginning or end of buffer."
  (interactive "P")
  (or arg
      (setq arg 1))
  (let ((found nil)
                  (ret t))
    (if (and (< arg 0)
                                 (looking-at "^function[ \t]"))
                  (forward-char 1))
    (while (< arg 0)
      (if (re-search-forward "^function[ \t]" nil t)
                         (setq arg (1+ arg)
                                         found t)
                  (setq ret nil
                                  arg 0)))
    (if found
                  (beginning-of-line))
    (while (> arg 0)
      (if (re-search-backward "^function[ \t]" nil t)
                         (setq arg (1- arg))
                  (setq ret nil
                                  arg 0)))
    ret))

;;}}}
;;{{{ lua-end-of-proc

(defun lua-end-of-proc (&optional arg)
  "Move forward to next end of lua proc (or similar).
With argument, do it that many times.  Negative argument -N means move
back to Nth preceding end of proc.

This function just searches for a `end' at the beginning of a line."
  (interactive "P")
  (or arg
      (setq arg 1))
  (let ((found nil)
	(ret t))
    (if (and (< arg 0)
	     (not (bolp))
	     (save-excursion
	       (beginning-of-line)
	       (eq (following-char) ?})))
	(forward-char -1))
    (while (> arg 0)
      (if (re-search-forward "^end" nil t)
	  (setq arg (1- arg)
		found t)
	(setq ret nil
	      arg 0)))
    (while (< arg 0)
      (if (re-search-backward "^end" nil t)
	  (setq arg (1+ arg)
		found t)
	(setq ret nil
	      arg 0)))
    (if found
	(end-of-line))
    ret))

;;}}}
;;{{{ lua-start-process

(defun lua-start-process (name &optional program startfile &rest switches)
  "Start a lua process named NAME, running PROGRAM."
  (or switches
      (setq switches lua-default-command-switches))
  (setq program (or program name))
  (setq lua-process-buffer (apply 'make-comint name program startfile switches))
  (setq lua-process (get-buffer-process lua-process-buffer))
  ;; wait for prompt
  (with-current-buffer lua-process-buffer
    (while (not (lua-prompt-line))
      (accept-process-output (get-buffer-process (current-buffer)))
      (goto-char (point-max)))))

;;}}}
;;{{{ lua-kill-process

(defun lua-kill-process ()
  "Kill lua subprocess and its buffer."
  (interactive)
  (if lua-process-buffer
      (kill-buffer lua-process-buffer)))

;;}}}
;;{{{ lua-set-lua-region-start

(defun lua-set-lua-region-start (&optional arg)
  "Set start of region for use with `lua-send-lua-region'."
  (interactive)
  (set-marker lua-region-start (or arg (point))))

;;}}}
;;{{{ lua-set-lua-region-end

(defun lua-set-lua-region-end (&optional arg)
  "Set end of region for use with `lua-send-lua-region'."
  (interactive)
  (set-marker lua-region-end (or arg (point))))

;;}}}
;;{{{ lua-send-current-line

(defun lua-send-current-line ()
  "Send current line to lua subprocess, found in `lua-process'.
If `lua-process' is nil or dead, start a new process first."
  (interactive)
  (let ((start (save-excursion (beginning-of-line) (point)))
        (end (save-excursion (end-of-line) (point))))
    (lua-send-region start end)))

;;}}}
;;{{{ lua-send-region

(defun lua-send-region (start end)
  "Send region to lua subprocess."
  (interactive "r")
  ;; make temporary lua file
  (let ((tempfile (lua-make-temp-file "lua-"))
	(last-prompt nil)
	(prompt-found nil)
	(lua-stdin-line-offset (count-lines (point-min) start))
	(lua-stdin-buffer (current-buffer))
	current-prompt )
    (write-region start end tempfile)
    (or (and lua-process
	     (comint-check-proc lua-process-buffer))
	(lua-start-process lua-default-application))
    ;; kill lua process without query
    (if (fboundp 'process-kill-without-query) 
	(process-kill-without-query lua-process)) 
    ;; send dofile(tempfile)
    (with-current-buffer lua-process-buffer   
      (goto-char (point-max))
      (setq last-prompt (point-max))
      (comint-simple-send (get-buffer-process (current-buffer)) 
			  (format "dofile(\"%s\")"  
				  (replace-in-string tempfile "\\\\" "\\\\\\\\" )))
      ;; wait for prompt
      (while (not prompt-found) 
	(accept-process-output (get-buffer-process (current-buffer)))
	(goto-char (point-max))
	(setq prompt-found (and (lua-prompt-line) (< last-prompt (point-max)))))
    ;; remove temp. lua file
    (delete-file tempfile)
    (lua-postprocess-output-buffer lua-process-buffer last-prompt lua-stdin-line-offset)    
    (if lua-always-show
	(display-buffer lua-process-buffer)))))

;;}}}
;;{{{ lua-postprocess-output-buffer

(defun lua-postprocess-output-buffer (buf start &optional lua-stdin-line-offset)
  "Highlight tracebacks found in buf. If an traceback occurred return
t, otherwise return nil.  BUF must exist."
  (let ((lua-stdin-line-offset (or lua-stdin-line-offset 0))
	line file bol err-p)
    (save-excursion
      (set-buffer buf)
      (goto-char start)
      (beginning-of-line)
      (if (re-search-forward lua-traceback-line-re nil t)
	  (setq file (match-string 1)
		line (string-to-int (match-string 2)))))
    (when (and lua-jump-on-traceback line)
      (beep)
      ;; TODO: highlight
      (lua-jump-to-traceback file line lua-stdin-line-offset)
      (setq err-p t))
    err-p))
  
;;}}}
;;{{{ lua-jump-to-tracebackw

(defun lua-jump-to-traceback (file line lua-stdin-line-offset)
  "Jump to the Lua code in FILE at LINE."
  ;; sanity check: temporary-file-directory
  (if (string= (substring file 0 3)  "...")
      (message "Lua traceback output truncated: customize 'temporary-file-directory' or increase 'LUA_IDSIZE' in 'luaconf.h'.")
    (let ((buffer (cond ((or (string-equal file tempfile) (string-equal file "stdin"))
		       (setq line (+ line lua-stdin-line-offset))
		       lua-stdin-buffer)
			(t (find-file-noselect file)))))
      (pop-to-buffer buffer)
      ;; Force Lua mode
      (if (not (eq major-mode 'lua-mode))
	  (lua-mode))
      ;; TODO fix offset when executing region
      (goto-line line)			
      (message "Jumping to error in file %s on line %d" file line))))

;;}}}
;;{{{ lua-prompt-line

(defun lua-prompt-line ()
  (save-excursion 
    (save-match-data
      (forward-line 0)
      (if (looking-at comint-prompt-regexp)
	  (match-end 0)))))

;;{{{ lua-send-lua-region
;;}}}

(defun lua-send-lua-region ()
  "Send preset lua region to lua subprocess."
  (interactive)
  (or (and lua-region-start lua-region-end)
      (error "lua-region not set"))
  (or (and lua-process
           (comint-check-proc lua-process-buffer))
      (lua-start-process lua-default-application))
  (comint-simple-send lua-process
                              (buffer-substring lua-region-start lua-region-end)
)
  (if lua-always-show
      (display-buffer lua-process-buffer)))

;;}}}
;;{{{ lua-send-proc

(defun lua-send-proc ()
  "Send proc around point to lua subprocess."
  (interactive)
  (let (beg end)
    (save-excursion
      (lua-beginning-of-proc)
      (setq beg (point))
      (lua-end-of-proc)
      (setq end (point)))
    (or (and lua-process
             (comint-check-proc lua-process-buffer))
        (lua-start-process lua-default-application))
    (comint-simple-send lua-process
                                (buffer-substring beg end))
    (if lua-always-show
        (display-buffer lua-process-buffer))))

;;}}}
;;{{{ lua-send-buffer

; This needs work... -Bret
(defun lua-send-buffer ()
  "Send whole buffer to lua subprocess."
  (interactive)
  (lua-send-region (point-min) (point-max)))

;;}}}
;;{{{ lua-restart-with-whole-file

(defun lua-restart-with-whole-file ()
  "Restart lua subprocess and send whole file as input."
  (interactive)
  (lua-kill-process)
  (lua-start-process lua-default-application)
  (lua-send-buffer))

;;}}}
;;{{{ lua-show-process-buffer

(defun lua-show-process-buffer ()
  "Make sure `lua-process-buffer' is being displayed."
  (interactive)
  (display-buffer lua-process-buffer))

;;}}}
;;{{{ lua-hide-process-buffer

(defun lua-hide-process-buffer ()
  "Delete all windows that display `lua-process-buffer'."
  (interactive)
  (delete-windows-on lua-process-buffer))

;;}}}
;;{{{ lua-search-documentation

(defun lua-search-documentation ()
  "Search Lua documentation for the word at the point."
  (interactive)
  (browse-url (concat lua-search-url-prefix (current-word t))))

;;}}}
;;{{{ lua-calculate-state

(defun lua-calculate-state (arg prevstate)
  ;; Calculate the new state of PREVSTATE, t or nil, based on arg. If
  ;; arg is nil or zero, toggle the state. If arg is negative, turn
  ;; the state off, and if arg is positive, turn the state on
  (if (or (not arg)
	  (zerop (setq arg (prefix-numeric-value arg))))
      (not prevstate)
    (> arg 0)))

;;}}}
;;{{{ lua-toggle-electric-state

(defun lua-toggle-electric-state (&optional arg)
  "Toggle the electric indentation feature.
Optional numeric ARG, if supplied, turns on electric indentation when
positive, turns it off when negative, and just toggles it when zero or
left out."
  (interactive "P")
  (setq lua-electric-flag (lua-calculate-state arg lua-electric-flag)))

;;}}}
;;{{{ lua-forward-sexp

(defun lua-forward-sexp (&optional count)
 "Forward to block end"
 (interactive "p")
 (save-match-data
 (let* ((count (or count 1))
	(stackheight 0)
	(block-start (mapcar 'car lua-sexp-alist))
	(block-end (mapcar 'cdr lua-sexp-alist))
	(block-regex (regexp-opt (append  block-start block-end) 'words))
	current-exp
	)
   (while (> count 0)
     ;; skip whitespace
     (skip-chars-forward " \t\n")
     (if (looking-at (regexp-opt block-start 'words)) 
	 (let ((keyword (match-string 1)))
	   (lua-find-matching-token-word keyword nil))
       ;; If the current keyword is not a "begin" keyword, then just
       ;; perform the normal forward-sexp.
       (forward-sexp 1))
     (setq count (1- count))))))


;;}}}
;;{{{ menu bar

(define-key lua-mode-menu [restart-with-whole-file]
  '("Restart With Whole File" .  lua-restart-with-whole-file))
(define-key lua-mode-menu [kill-process]
  '("Kill Process" . lua-kill-process))

(define-key lua-mode-menu [hide-process-buffer]
  '("Hide Process Buffer" . lua-hide-process-buffer))
(define-key lua-mode-menu [show-process-buffer]
  '("Show Process Buffer" . lua-show-process-buffer))

(define-key lua-mode-menu [end-of-proc]
  '("End Of Proc" . lua-end-of-proc))
(define-key lua-mode-menu [beginning-of-proc]
  '("Beginning Of Proc" . lua-beginning-of-proc))

(define-key lua-mode-menu [send-lua-region]
  '("Send Lua-Region" . lua-send-lua-region))
(define-key lua-mode-menu [set-lua-region-end]
  '("Set Lua-Region End" . lua-set-lua-region-end))
(define-key lua-mode-menu [set-lua-region-start]
  '("Set Lua-Region Start" . lua-set-lua-region-start))

(define-key lua-mode-menu [send-current-line]
  '("Send Current Line" . lua-send-current-line))
(define-key lua-mode-menu [send-region]
  '("Send Region" . lua-send-region))
(define-key lua-mode-menu [send-proc]
  '("Send Proc" . lua-send-proc))
(define-key lua-mode-menu [send-buffer]
  '("Send Buffer" . lua-send-buffer))
(define-key lua-mode-menu [search-documentation]
  '("Search Documentation" . lua-search-documentation))

;;}}}

(provide 'lua-mode)


;;{{{ Emacs local variables

;; Local Variables:
;; folded-file: t
;; End:

;;}}}

;;; lua-mode.el ends here
