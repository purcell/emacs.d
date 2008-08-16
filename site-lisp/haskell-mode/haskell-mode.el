;;; haskell-mode.el --- A Haskell editing mode    -*-coding: iso-8859-1;-*-

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008  Free Software Foundation, Inc
;; Copyright (C) 1992, 1997-1998 Simon Marlow, Graeme E Moss, and Tommy Thorn

;; Authors: 1992      Simon Marlow
;;          1997-1998 Graeme E Moss <gem@cs.york.ac.uk> and
;;                    Tommy Thorn <thorn@irisa.fr>,
;;          2001-2002 Reuben Thomas (>=v1.4)
;;          2003      Dave Love <fx@gnu.org>
;; Keywords: faces files Haskell
;; Version: $Name:  $
;; URL: http://www.haskell.org/haskell-mode/

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; Purpose:
;;
;; To provide a pleasant mode to browse and edit Haskell files, linking
;; into the following supported modules:
;;
;; `haskell-font-lock', Graeme E Moss and Tommy Thorn
;;   Fontifies standard Haskell keywords, symbols, functions, etc.
;;
;; `haskell-decl-scan', Graeme E Moss
;;   Scans top-level declarations, and places them in a menu.
;;
;; `haskell-doc', Hans-Wolfgang Loidl
;;   Echoes types of functions or syntax of keywords when the cursor is idle.
;;
;; `haskell-indent', Guy Lapalme
;;   Intelligent semi-automatic indentation.
;;
;; `haskell-simple-indent', Graeme E Moss and Heribert Schuetz
;;   Simple indentation.
;;
;; `inf-haskell'
;;   Interaction with an inferior Haskell process.
;;   It replaces the previous two modules:
;;     `haskell-hugs', Guy Lapalme
;;     `haskell-ghci', Chris Web
;;
;;
;; This mode supports full Haskell 1.4 including literate scripts.
;; In some versions of (X)Emacs it may only support Latin-1, not Unicode.
;;
;; Installation:
;; 
;; Put in your ~/.emacs:
;;
;;    (setq auto-mode-alist
;;          (append auto-mode-alist
;;                  '(("\\.[hg]s$"  . haskell-mode)
;;                    ("\\.hi$"     . haskell-mode)
;;                    ("\\.l[hg]s$" . literate-haskell-mode))))
;;
;;    (autoload 'haskell-mode "haskell-mode"
;;       "Major mode for editing Haskell scripts." t)
;;    (autoload 'literate-haskell-mode "haskell-mode"
;;       "Major mode for editing literate Haskell scripts." t)
;;
;; with `haskell-mode.el' accessible somewhere on the load-path.
;; To add a directory `~/lib/emacs' (for example) to the load-path,
;; add the following to .emacs:
;;
;;    (setq load-path (cons "~/lib/emacs" load-path))
;;
;; To turn any of the supported modules on for all buffers, add the
;; appropriate line(s) to .emacs:
;;
;;    (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
;;    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;    (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;;
;; Make sure the module files are also on the load-path.  Note that
;; the two indentation modules are mutually exclusive: Use only one.
;;
;;
;; Customisation:
;;
;; Set the value of `haskell-literate-default' to your preferred
;; literate style: `bird' or `tex', within .emacs as follows:
;;
;;    (setq haskell-literate-default 'tex)
;;
;; Also see the customisations of the modules.
;;
;;
;; History:
;;
;; This mode is based on an editing mode by Simon Marlow 11/1/92
;; and heavily modified by Graeme E Moss and Tommy Thorn 7/11/98.
;; 
;; If you have any problems or suggestions specific to a supported
;; module, consult that module for a list of known bugs, and an
;; author to contact via email.  For general problems or suggestions,
;; consult the list below, then email gem@cs.york.ac.uk and
;; thorn@irisa.fr quoting the version of the mode you are using, the
;; version of Emacs you are using, and a small example of the problem
;; or suggestion.
;;
;; Version 1.43:
;;   Various tweaks to doc strings and customization support from
;;   Ville Skyttä <scop@xemacs.org>.
;;
;; Version 1.42:
;;   Added autoload for GHCi inferior mode (thanks to Scott
;;   Williams for the bug report and fix).
;;
;; Version 1.41:
;;   Improved packaging, and made a couple more variables
;;   interactively settable.
;;
;; Version 1.4:
;;   Added GHCi mode from Chris Webb, and tidied up a little.
;;
;; Version 1.3:
;;   The literate or non-literate style of a buffer is now indicated
;;   by just the variable haskell-literate: nil, `bird', or `tex'.
;;   For literate buffers with ambiguous style, the value of
;;   haskell-literate-default is used.
;;
;; Version 1.2:
;;   Separated off font locking, declaration scanning and simple
;;   indentation, and made them separate modules.  Modules can be
;;   added easily now.  Support for modules haskell-doc,
;;   haskell-indent, and haskell-hugs.  Literate and non-literate
;;   modes integrated into one mode, and literate buffer indicated by
;;   value of haskell-literate(-bird-style).
;;
;; Version 1.1:
;;   Added support for declaration scanning under XEmacs via
;;   func-menu.  Moved operators to level two fontification.
;;
;; Version 1.0:
;;   Added a nice indention support from Heribert Schuetz
;;   <Heribert.Schuetz@informatik.uni-muenchen.de>:
;;
;;     I have just hacked an Emacs Lisp function which you might prefer
;;     to `indent-relative' in haskell-mode.el.  See below.  It is not
;;     really Haskell-specific because it does not take into account
;;     keywords like `do', `of', and `let' (where the layout rule
;;     applies), but I already find it useful.
;;
;;   Cleaned up the imenu support.  Added support for literate scripts.
;;
;; Version 0.103 [HWL]:
;;   From Hans Wolfgang Loidl <hwloidl@dcs.gla.ac.uk>:
;;
;;   I (HWL) added imenu support by copying the appropriate functions
;;   from hugs-mode.  A menu-bar item "Declarations" is now added in
;;   haskell mode.  The new code, however, needs some clean-up.
;;
;; Version 0.102:
;;
;;   Moved C-c C-c key binding to comment-region.  Leave M-g M-g to do
;;   the work.  comment-start-skip is changed to comply with comment-start.
;;
;; Version 0.101:
;;
;;   Altered indent-line-function to indent-relative.
;;
;; Version 0.100:
;; 
;;   First official release.

;; Present Limitations/Future Work (contributions are most welcome!):
;;
;; . Would like RET in Bird-style literate mode to add a ">" at the
;;   start of a line when previous line starts with ">".  Or would
;;   "> " be better?
;;
;; . Support for GreenCard?
;;

;;; Code:

(eval-when-compile (require 'cl))

;; All functions/variables start with `(literate-)haskell-'.

;; Version of mode.
(defconst haskell-version "$Name:  $"
  "`haskell-mode' version number.")
(defun haskell-version ()
  "Echo the current version of `haskell-mode' in the minibuffer."
  (interactive)
  (message "Using haskell-mode version %s" haskell-version))

(defgroup haskell nil
  "Major mode for editing Haskell programs."
  :group 'languages
  :prefix "haskell-")

;; Set up autoloads for the modules we supply
(autoload 'turn-on-haskell-decl-scan "haskell-decl-scan"
  "Turn on Haskell declaration scanning." t)
(autoload 'turn-on-haskell-doc-mode "haskell-doc"
  "Turn on Haskell Doc minor mode." t)
(autoload 'turn-on-haskell-indent "haskell-indent"
  "Turn on Haskell indentation." t)
(autoload 'turn-on-haskell-simple-indent "haskell-simple-indent"
  "Turn on simple Haskell indentation." t)

;; Functionality provided in other files.
(autoload 'haskell-ds-create-imenu-index "haskell-decl-scan")
(autoload 'haskell-font-lock-choose-keywords "haskell-font-lock")
(autoload 'haskell-doc-current-info "haskell-doc")

;; Obsolete functions.
(defun turn-on-haskell-font-lock ()
  (turn-on-font-lock)
  (message "turn-on-haskell-font-lock is obsolete.  Use turn-on-font-lock instead."))
(defun turn-on-haskell-hugs () (message "haskell-hugs is obsolete."))
(defun turn-on-haskell-ghci () (message "haskell-ghci is obsolete."))


;; Are we looking at a literate script?
(defvar haskell-literate nil
  "*If not nil, the current buffer contains a literate Haskell script.
Possible values are: `bird' and `tex', for Bird-style and LaTeX-style
literate scripts respectively.  Set by `haskell-mode' and
`literate-haskell-mode'.  For an ambiguous literate buffer -- ie. does
not contain either \"\\begin{code}\" or \"\\end{code}\" on a line on
its own, nor does it contain \">\" at the start of a line -- the value
of `haskell-literate-default' is used.")
(make-variable-buffer-local 'haskell-literate)
(put 'haskell-literate 'safe-local-variable 'symbolp)
;; Default literate style for ambiguous literate buffers.
(defcustom haskell-literate-default 'bird
  "Default value for `haskell-literate'.
Used if the style of a literate buffer is ambiguous.  This variable should
be set to the preferred literate style."
  :type '(choice (const bird) (const tex) (const nil)))

;; Mode maps.
(defvar haskell-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Bindings for the inferior haskell process:
    ;; (define-key map [?\M-C-x]     'inferior-haskell-send-defun)
    ;; (define-key map [?\C-x ?\C-e] 'inferior-haskell-send-last-sexp)
    ;; (define-key map [?\C-c ?\C-r] 'inferior-haskell-send-region)
    (define-key map [?\C-c ?\C-z] 'switch-to-haskell)
    (define-key map [?\C-c ?\C-l] 'inferior-haskell-load-file)
    ;; I think it makes sense to bind inferior-haskell-load-and-run to C-c
    ;; C-r, but since it used to be bound to `reload' until june 2007, I'm
    ;; going to leave it out for now.
    ;; (define-key map [?\C-c ?\C-r] 'inferior-haskell-load-and-run)
    (define-key map [?\C-c ?\C-b] 'switch-to-haskell)
    ;; (define-key map [?\C-c ?\C-s] 'inferior-haskell-start-process)
    ;; That's what M-; is for.
    ;; (define-key map "\C-c\C-c" 'comment-region)

    (define-key map (kbd "C-c C-t") 'inferior-haskell-type)
    (define-key map (kbd "C-c C-i") 'inferior-haskell-info)
    (define-key map (kbd "C-c M-.") 'inferior-haskell-find-definition)
    (define-key map (kbd "C-c C-d") 'inferior-haskell-find-haddock)

    (define-key map [remap delete-indentation] 'haskell-delete-indentation)
    map)
  "Keymap used in Haskell mode.")

(easy-menu-define haskell-mode-menu haskell-mode-map
  "Menu for the Haskell major mode."
  ;; Suggestions from Pupeno <pupeno@pupeno.com>:
  ;; - choose the underlying interpreter
  ;; - look up docs
  `("Haskell"
    ["Indent line" indent-according-to-mode]
    ["Indent region" indent-region mark-active]
    ["(Un)Comment region" comment-region mark-active]
    "---"
    ["Start interpreter" switch-to-haskell]
    ["Load file" inferior-haskell-load-file]
    "---"
    ,(if (default-boundp 'eldoc-documentation-function)
         ["Doc mode" eldoc-mode
          :style toggle :selected (bound-and-true-p eldoc-mode)]
       ["Doc mode" haskell-doc-mode
        :style toggle :selected (and (boundp 'haskell-doc-mode) haskell-doc-mode)])
    ["Customize" (customize-group 'haskell)]
    ))

;; Syntax table.
(defvar haskell-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\  " " table)
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\'" table)
    (modify-syntax-entry ?_  "w" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[  "(]" table)
    (modify-syntax-entry ?\]  ")[" table)

    (cond ((featurep 'xemacs)
	   ;; I don't know whether this is equivalent to the below
	   ;; (modulo nesting).  -- fx
	   (modify-syntax-entry ?{  "(}5" table)
	   (modify-syntax-entry ?}  "){8" table)
	   (modify-syntax-entry ?-  "_ 1267" table))
	  (t
	   ;; In Emacs 21, the `n' indicates that they nest.
	   ;; The `b' annotation is actually ignored because it's only
	   ;; meaningful on the second char of a comment-starter, so
	   ;; on Emacs 20 and before we get wrong results.  --Stef
	   (modify-syntax-entry ?\{  "(}1nb" table)
	   (modify-syntax-entry ?\}  "){4nb" table)
	   (modify-syntax-entry ?-  "_ 123" table)))
    (modify-syntax-entry ?\n ">" table)

    (let (i lim)
      (map-char-table
       (lambda (k v)
	 (when (equal v '(1))
	   ;; The current Emacs 22 codebase can pass either a char
	   ;; or a char range.
	   (if (consp k)
	       (setq i (car k)
		     lim (cdr k))
	     (setq i k 
		   lim k))
	   (while (<= i lim)
	     (when (> i 127)
	       (modify-syntax-entry i "_" table))
	     (setq i (1+ i)))))
       (standard-syntax-table)))
    
    (modify-syntax-entry ?\` "$`" table)
    (modify-syntax-entry ?\\ "\\" table)
    (mapc (lambda (x)
            (modify-syntax-entry x "_" table))
          ;; Some of these are actually OK by default.
          "!#$%&*+./:<=>?@^|~")
    (unless (featurep 'mule)
      ;; Non-ASCII syntax should be OK, at least in Emacs.
      (mapc (lambda (x)
              (modify-syntax-entry x "_" table))
            (concat "¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿"
                    "×÷"))
      (mapc (lambda (x)
              (modify-syntax-entry x "w" table))
            (concat "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ"
                    "ØÙÚÛÜÝÞß"
                    "àáâãäåæçèéêëìíîïðñòóôõö"
                    "øùúûüýþÿ")))
    table)
  "Syntax table used in Haskell mode.")

(defun haskell-ident-at-point ()
  "Return the identifier under point, or nil if none found.
May return a qualified name."
  (save-excursion
    (let ((case-fold-search nil))
      (multiple-value-bind (start end)
          (if (looking-at "\\s_")
              (values (progn (skip-syntax-backward "_") (point))
                      (progn (skip-syntax-forward "_") (point)))
            (values
             (progn (skip-syntax-backward "w'")
                    (skip-syntax-forward "'") (point))
             (progn (skip-syntax-forward "w'") (point))))
        ;; If we're looking at a module ID that qualifies further IDs, add
        ;; those IDs.
        (goto-char start)
        (while (and (looking-at "[[:upper:]]") (eq (char-after end) ?.)
                    ;; It's a module ID that qualifies further IDs.
                    (goto-char (1+ end))
                    (save-excursion
                      (when (not (zerop (skip-syntax-forward
                                         (if (looking-at "\\s_") "_" "w'"))))
                        (setq end (point))))))
        ;; If we're looking at an ID that's itself qualified by previous
        ;; module IDs, add those too.
        (goto-char start)
        (if (eq (char-after) ?.) (forward-char 1)) ;Special case for "."
        (while (and (eq (char-before) ?.)
                    (progn (forward-char -1)
                           (not (zerop (skip-syntax-backward "w'"))))
                    (skip-syntax-forward "'")
                    (looking-at "[[:upper:]]"))
          (setq start (point)))
        ;; This is it.
        (buffer-substring-no-properties start end)))))

(defun haskell-delete-indentation (&optional arg)
  "Like `delete-indentation' but ignoring Bird-stlye \">\"."
  (interactive "*P")
  (let ((fill-prefix (or fill-prefix (if (eq haskell-literate 'bird) ">"))))
    (delete-indentation arg)))

;; Various mode variables.

(defcustom haskell-mode-hook nil
  "Hook run after entering Haskell mode."
  :type 'hook
  :options '(turn-on-haskell-indent turn-on-font-lock turn-on-eldoc-mode
	     imenu-add-menubar-index))

(defvar eldoc-print-current-symbol-info-function)

;; The main mode functions
;;;###autoload
(define-derived-mode haskell-mode fundamental-mode "Haskell"
  "Major mode for editing Haskell programs.
Blank lines separate paragraphs, comments start with `-- '.
\\<haskell-mode-map>
Literate scripts are supported via `literate-haskell-mode'.
The variable `haskell-literate' indicates the style of the script in the
current buffer.  See the documentation on this variable for more details.

Modules can hook in via `haskell-mode-hook'.  The following modules
are supported with an `autoload' command:

   `haskell-decl-scan', Graeme E Moss
     Scans top-level declarations, and places them in a menu.

   `haskell-doc', Hans-Wolfgang Loidl
     Echoes types of functions or syntax of keywords when the cursor is idle.

   `haskell-indent', Guy Lapalme
     Intelligent semi-automatic indentation.

   `haskell-simple-indent', Graeme E Moss and Heribert Schuetz
     Simple indentation.

Module X is activated using the command `turn-on-X'.  For example,
`haskell-indent' is activated using `turn-on-haskell-indent'.
For more information on a module, see the help for its `X-mode'
function.  Some modules can be deactivated using `turn-off-X'.  (Note
that `haskell-doc' is irregular in using `turn-(on/off)-haskell-doc-mode'.)

Use `haskell-version' to find out what version this is.

Invokes `haskell-mode-hook'."
  (set (make-local-variable 'paragraph-start) (concat "^$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'comment-start) "-- ")
  (set (make-local-variable 'comment-padding) 0)
  (set (make-local-variable 'comment-start-skip) "[-{]-[ \t]*")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-end-skip) "[ \t]*\\(-}\\|\\s>\\)")
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; Set things up for eldoc-mode.
  (set (make-local-variable 'eldoc-documentation-function)
       'haskell-doc-current-info)
  ;; Set things up for imenu.
  (set (make-local-variable 'imenu-create-index-function)
       'haskell-ds-create-imenu-index)
  ;; Set things up for font-lock.
  (set (make-local-variable 'font-lock-defaults)
       '(haskell-font-lock-choose-keywords
	 nil nil ((?\' . "w") (?_  . "w")) nil
	 (font-lock-syntactic-keywords
	  . haskell-font-lock-choose-syntactic-keywords)
	 (font-lock-syntactic-face-function
	  . haskell-syntactic-face-function)
	 ;; Get help from font-lock-syntactic-keywords.
	 (parse-sexp-lookup-properties . t)))
  ;; Haskell's layout rules mean that TABs have to be handled with extra care.
  ;; The safer option is to avoid TABs.  The second best is to make sure
  ;; TABs stops are 8 chars apart, as mandated by the Haskell Report.  --Stef
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'tab-width) 8)
  (setq haskell-literate nil))

;;;###autoload
(define-derived-mode literate-haskell-mode haskell-mode "LitHaskell"
  "As `haskell-mode' but for literate scripts."
  (setq haskell-literate
        (save-excursion
          (goto-char (point-min))
          (cond
           ((re-search-forward "^\\\\\\(begin\\|end\\){code}$" nil t) 'tex)
           ((re-search-forward "^>" nil t) 'bird)
           (t haskell-literate-default))))
  (if (eq haskell-literate 'bird)
      ;; fill-comment-paragraph isn't much use there, and even gets confused
      ;; by the syntax-table text-properties we add to mark the first char
      ;; of each line as a comment-starter.
      (set (make-local-variable 'fill-paragraph-handle-comment) nil))
  (set (make-local-variable 'mode-line-process)
       '("/" (:eval (symbol-name haskell-literate)))))

;;;###autoload(add-to-list 'auto-mode-alist '("\\.\\(?:[gh]s\\|hi\\)\\'" . haskell-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("\\.l[gh]s\\'" . literate-haskell-mode))
;;;###autoload(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;;###autoload(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(defcustom haskell-hoogle-command
  (if (executable-find "hoogle") "hoogle")
  "Name of the command to use to query Hoogle.
If nil, use the Hoogle web-site."
  :type '(choice (const :tag "Use Web-site" nil)
                 string))

;;;###autoload
(defun haskell-hoogle (query)
  "Do a Hoogle search for QUERY."
  (interactive
   (let ((def (haskell-ident-at-point)))
     (if (and def (symbolp def)) (setq def (symbol-name def)))
     (list (read-string (if def
                            (format "Hoogle query (default %s): " def)
                          "Hoogle query: ")
                        nil nil def))))
  (if (null haskell-hoogle-command)
      (browse-url (format "http://haskell.org/hoogle/?q=%s" query))
    (if (fboundp 'help-setup-xref)
        (help-setup-xref (list 'haskell-hoogle query) (interactive-p)))
    (with-output-to-temp-buffer
        (if (fboundp 'help-buffer) (help-buffer) "*Help*")
      (with-current-buffer standard-output
        (start-process "hoogle" (current-buffer) haskell-hoogle-command
                       query)))))

;; Provide ourselves:

(provide 'haskell-mode)

;; arch-tag: b2237ec0-ddb0-4c86-9339-52d410264980
;;; haskell-mode.el ends here
