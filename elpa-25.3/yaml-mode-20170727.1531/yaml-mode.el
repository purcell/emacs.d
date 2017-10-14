;;; yaml-mode.el --- Major mode for editing YAML files

;; Copyright (C) 2010-2014 Yoshiki Kurihara

;; Author: Yoshiki Kurihara <clouder@gmail.com>
;;         Marshall T. Vandegrift <llasram@gmail.com>
;; Maintainer: Vasilij Schneidermann <v.schneidermann@gmail.com>
;; Package-Requires: ((emacs "24.1"))
;; Package-Version: 20170727.1531
;; Keywords: data yaml
;; Version: 0.0.13

;; This file is not part of Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;; Commentary:

;; This is a major mode for editing files in the YAML data
;; serialization format.  It was initially developed by Yoshiki
;; Kurihara and many features were added by Marshall Vandegrift.  As
;; YAML and Python share the fact that indentation determines
;; structure, this mode provides indentation and indentation command
;; behavior very similar to that of python-mode.

;;; Installation:

;; To install, just drop this file into a directory in your
;; `load-path' and (optionally) byte-compile it.  To automatically
;; handle files ending in '.yml', add something like:
;;
;;    (require 'yaml-mode)
;;    (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
;;
;; to your .emacs file.
;;
;; Unlike python-mode, this mode follows the Emacs convention of not
;; binding the ENTER key to `newline-and-indent'.  To get this
;; behavior, add the key definition to `yaml-mode-hook':
;;
;;    (add-hook 'yaml-mode-hook
;;     '(lambda ()
;;        (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;; Known Bugs:

;; YAML is easy to write but complex to parse, and this mode doesn't
;; even really try.  Indentation and highlighting will break on
;; abnormally complicated structures.

;;; Code:


;; User definable variables

;;;###autoload
(defgroup yaml nil
  "Support for the YAML serialization format"
  :group 'languages
  :prefix "yaml-")

(defcustom yaml-mode-hook nil
  "*Hook run by `yaml-mode'."
  :type 'hook
  :group 'yaml)

(defcustom yaml-indent-offset 2
  "*Amount of offset per level of indentation."
  :type 'integer
  :safe 'natnump
  :group 'yaml)

(defcustom yaml-backspace-function 'backward-delete-char-untabify
  "*Function called by `yaml-electric-backspace' when deleting backwards.
It will receive one argument, the numeric prefix value."
  :type 'function
  :group 'yaml)

(defcustom yaml-block-literal-search-lines 100
  "*Maximum number of lines to search for start of block literals."
  :type 'integer
  :group 'yaml)

(defcustom yaml-block-literal-electric-alist
  '((?| . "") (?> . "-"))
  "*Characters for which to provide electric behavior.
The association list key should be a key code and the associated value
should be a string containing additional characters to insert when
that key is pressed to begin a block literal."
  :type 'alist
  :group 'yaml)

(defface yaml-tab-face
   '((((class color)) (:background "red" :foreground "red" :bold t))
     (t (:reverse-video t)))
  "Face to use for highlighting tabs in YAML files."
  :group 'faces
  :group 'yaml)

(defcustom yaml-imenu-generic-expression
  '((nil  "^\\(:?[a-zA-Z_-]+\\):"          1))
  "The imenu regex to parse an outline of the yaml file."
  :type 'string
  :group 'yaml)


;; Constants

(defconst yaml-mode-version "0.0.13" "Version of `yaml-mode'.")

(defconst yaml-blank-line-re "^ *$"
  "Regexp matching a line containing only (valid) whitespace.")

(defconst yaml-directive-re "^\\(?:--- \\)? *%\\(\\w+\\)"
  "Regexp matching a line contatining a YAML directive.")

(defconst yaml-document-delimiter-re "^\\(?:---\\|[.][.][.]\\)"
  "Rexexp matching a YAML document delimiter line.")

(defconst yaml-node-anchor-alias-re "[&*][a-zA-Z0-9_-]+"
  "Regexp matching a YAML node anchor or alias.")

(defconst yaml-tag-re "!!?[^ \n]+"
  "Rexexp matching a YAML tag.")

(defconst yaml-bare-scalar-re
  "\\(?:[^-:,#!\n{\\[ ]\\|[^#!\n{\\[ ]\\S-\\)[^#\n]*?"
  "Rexexp matching a YAML bare scalar.")

(defconst yaml-hash-key-re
  (concat "\\(?:^\\(?:--- \\)?\\|{\\|\\(?:[-,] +\\)+\\) *"
          "\\(?:" yaml-tag-re " +\\)?"
          "\\(" yaml-bare-scalar-re "\\) *:"
          "\\(?: +\\|$\\)")
  "Regexp matching a single YAML hash key.")

(defconst yaml-scalar-context-re
  (concat "\\(?:^\\(?:--- \\)?\\|{\\|\\(?: *[-,] +\\)+\\) *"
          "\\(?:" yaml-bare-scalar-re " *: \\)?")
  "Regexp indicating the beginning of a scalar context.")

(defconst yaml-nested-map-re
  (concat ".*: *\\(?:&.*\\|{ *\\|" yaml-tag-re " *\\)?$")
  "Regexp matching a line beginning a YAML nested structure.")

(defconst yaml-block-literal-base-re " *[>|][-+0-9]* *\\(?:\n\\|\\'\\)"
  "Regexp matching the substring start of a block literal.")

(defconst yaml-block-literal-re
  (concat yaml-scalar-context-re
          "\\(?:" yaml-tag-re "\\)?"
          yaml-block-literal-base-re)
  "Regexp matching a line beginning a YAML block literal.")

(defconst yaml-nested-sequence-re
  (concat "^\\(?:\\(?: *- +\\)+\\|\\(:? *-$\\)\\)"
          "\\(?:" yaml-bare-scalar-re " *:\\(?: +.*\\)?\\)?$")
  "Regexp matching a line containing one or more nested YAML sequences.")

(defconst yaml-constant-scalars-re
  (concat "\\(?:^\\|\\(?::\\|-\\|,\\|{\\|\\[\\) +\\) *"
          (regexp-opt
           '("~" "null" "Null" "NULL"
             ".nan" ".NaN" ".NAN"
             ".inf" ".Inf" ".INF"
             "-.inf" "-.Inf" "-.INF"
             "y" "Y" "yes" "Yes" "YES" "n" "N" "no" "No" "NO"
             "true" "True" "TRUE" "false" "False" "FALSE"
             "on" "On" "ON" "off" "Off" "OFF") t)
          " *$")
  "Regexp matching certain scalar constants in scalar context.")


;; Mode setup

(defvar yaml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "|" 'yaml-electric-bar-and-angle)
    (define-key map ">" 'yaml-electric-bar-and-angle)
    (define-key map "-" 'yaml-electric-dash-and-dot)
    (define-key map "." 'yaml-electric-dash-and-dot)
    (define-key map (kbd "DEL") 'yaml-electric-backspace)
    map)
  "Keymap used in `yaml-mode' buffers.")

(defvar yaml-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\' "\"" syntax-table)
    (modify-syntax-entry ?\" "\"" syntax-table)
    (modify-syntax-entry ?# "<" syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)
    (modify-syntax-entry ?\\ "\\" syntax-table)
    (modify-syntax-entry ?- "_" syntax-table)
    (modify-syntax-entry ?_ "_" syntax-table)
    (modify-syntax-entry ?& "." syntax-table)
    (modify-syntax-entry ?* "." syntax-table)
    (modify-syntax-entry ?\( "." syntax-table)
    (modify-syntax-entry ?\) "." syntax-table)
    (modify-syntax-entry ?\{ "(}" syntax-table)
    (modify-syntax-entry ?\} "){" syntax-table)
    (modify-syntax-entry ?\[ "(]" syntax-table)
    (modify-syntax-entry ?\] ")[" syntax-table)
    syntax-table)
  "Syntax table in use in `yaml-mode' buffers.")

;;;###autoload
(define-derived-mode yaml-mode text-mode "YAML"
  "Simple mode to edit YAML.

\\{yaml-mode-map}"
  :syntax-table yaml-mode-syntax-table
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+ *")
  (set (make-local-variable 'indent-line-function) 'yaml-indent-line)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'fill-paragraph-function) 'yaml-fill-paragraph)

  (set (make-local-variable 'syntax-propertize-function)
       'yaml-mode-syntax-propertize-function)
  (setq font-lock-defaults '(yaml-font-lock-keywords)))


;; Font-lock support

(defvar yaml-font-lock-keywords
  `((,yaml-constant-scalars-re . (1 font-lock-constant-face))
    (,yaml-tag-re . (0 font-lock-type-face))
    (,yaml-node-anchor-alias-re . (0 font-lock-function-name-face))
    (,yaml-hash-key-re . (1 font-lock-variable-name-face))
    (,yaml-document-delimiter-re . (0 font-lock-comment-face))
    (,yaml-directive-re . (1 font-lock-builtin-face))
    (yaml-font-lock-block-literals 0 font-lock-string-face)
    ("^[\t]+" 0 'yaml-tab-face t))
   "Additional expressions to highlight in YAML mode.")

(defun yaml-mode-syntax-propertize-function (beg end)
  "Unhighlight foo#bar tokens between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (search-forward "#" end t)
      (save-excursion
        (forward-char -1)
        ;; both ^# and [ \t]# are comments
        (when (and (not (bolp))
                   (not (memq (preceding-char) '(?\s ?\t))))
          (put-text-property (point) (1+ (point))
                             'syntax-table (string-to-syntax "_")))))))

(defun yaml-font-lock-block-literals (bound)
  "Find lines within block literals.
Find the next line of the first (if any) block literal after point and
prior to BOUND.  Returns the beginning and end of the block literal
line in the match data, as consumed by `font-lock-keywords' matcher
functions.  The function begins by searching backwards to determine
whether or not the current line is within a block literal.  This could
be time-consuming in large buffers, so the number of lines searched is
artificially limitted to the value of
`yaml-block-literal-search-lines'."
  (if (eolp) (goto-char (1+ (point))))
  (unless (or (eobp) (>= (point) bound))
    (let ((begin (point))
          (end (min (1+ (point-at-eol)) bound)))
      (goto-char (point-at-bol))
      (while (and (looking-at yaml-blank-line-re) (not (bobp)))
        (forward-line -1))
      (let ((nlines yaml-block-literal-search-lines)
            (min-level (current-indentation)))
      (forward-line -1)
      (while (and (/= nlines 0)
                  (/= min-level 0)
                  (not (looking-at yaml-block-literal-re))
                  (not (bobp)))
        (set 'nlines (1- nlines))
        (unless (looking-at yaml-blank-line-re)
          (set 'min-level (min min-level (current-indentation))))
        (forward-line -1))
      (cond
       ((and (< (current-indentation) min-level)
             (looking-at yaml-block-literal-re))
          (goto-char end) (set-match-data (list begin end)) t)
         ((progn
            (goto-char begin)
            (re-search-forward (concat yaml-block-literal-re
                                       " *\\(.*\\)\n")
                               bound t))
          (set-match-data (nthcdr 2 (match-data))) t))))))


;; Indentation and electric keys

(defun yaml-compute-indentation ()
  "Calculate the maximum sensible indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at yaml-document-delimiter-re) 0
      (forward-line -1)
      (while (and (looking-at yaml-blank-line-re)
                  (> (point) (point-min)))
        (forward-line -1))
      (+ (current-indentation)
         (if (looking-at yaml-nested-map-re) yaml-indent-offset 0)
         (if (looking-at yaml-nested-sequence-re) yaml-indent-offset 0)
         (if (looking-at yaml-block-literal-re) yaml-indent-offset 0)))))

(defun yaml-indent-line ()
  "Indent the current line.
The first time this command is used, the line will be indented to the
maximum sensible indentation.  Each immediately subsequent usage will
back-dent the line by `yaml-indent-offset' spaces.  On reaching column
0, it will cycle back to the maximum sensible indentation."
  (interactive "*")
  (let ((ci (current-indentation))
        (cc (current-column))
        (need (yaml-compute-indentation)))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (if (and (equal last-command this-command) (/= ci 0))
          (indent-to (* (/ (- ci 1) yaml-indent-offset) yaml-indent-offset))
        (indent-to need)))
      (if (< (current-column) (current-indentation))
          (forward-to-indentation 0))))

(defun yaml-electric-backspace (arg)
  "Delete characters or back-dent the current line.
If invoked following only whitespace on a line, will back-dent to the
immediately previous multiple of `yaml-indent-offset' spaces."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column)) (bolp))
      (funcall yaml-backspace-function arg)
    (let ((ci (current-column)))
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to (* (/ (- ci (* arg yaml-indent-offset))
                       yaml-indent-offset)
                    yaml-indent-offset)))))

(defun yaml-electric-bar-and-angle (arg)
  "Insert the bound key and possibly begin a block literal.
Inserts the bound key.  If inserting the bound key causes the current
line to match the initial line of a block literal, then inserts the
matching string from `yaml-block-literal-electric-alist', a newline,
and indents appropriately."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (let ((extra-chars
         (assoc last-command-event
                yaml-block-literal-electric-alist)))
    (cond
     ((and extra-chars (not arg) (eolp)
           (save-excursion
             (beginning-of-line)
             (looking-at yaml-block-literal-re)))
      (insert (cdr extra-chars))
      (newline-and-indent)))))

(defun yaml-electric-dash-and-dot (arg)
  "Insert the bound key and possibly de-dent line.
Inserts the bound key.  If inserting the bound key causes the current
line to match a document delimiter, de-dent the line to the left
margin."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (save-excursion
    (beginning-of-line)
    (if (and (not arg) (looking-at yaml-document-delimiter-re))
        (delete-horizontal-space))))

(defun yaml-narrow-to-block-literal ()
  "Narrow the buffer to block literal if the point is in it,
otherwise do nothing."
  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    (while (and (looking-at-p yaml-blank-line-re) (not (bobp)))
      (forward-line -1))
    (let ((nlines yaml-block-literal-search-lines)
	  (min-level (current-indentation))
	  beg)
      (forward-line -1)
      (while (and (/= nlines 0)
		  (/= min-level 0)
		  (not (looking-at-p yaml-block-literal-re))
		  (not (bobp)))
	(set 'nlines (1- nlines))
	(unless (looking-at-p yaml-blank-line-re)
	  (set 'min-level (min min-level (current-indentation))))
	(forward-line -1))
      (when (and (< (current-indentation) min-level)
		  (looking-at-p yaml-block-literal-re))
	(set 'min-level (current-indentation))
	(forward-line)
	(setq beg (point))
	(while (and (not (eobp))
		    (or (looking-at-p yaml-blank-line-re)
			(> (current-indentation) min-level)))
	  (forward-line))
	(narrow-to-region beg (point))))))

(defun yaml-fill-paragraph (&optional justify region)
  "Fill paragraph.
This behaves as `fill-paragraph' except that filling does not
cross boundaries of block literals."
  (interactive "*P")
  (save-restriction
    (yaml-narrow-to-block-literal)
    (let ((fill-paragraph-function nil))
      (fill-paragraph justify region))))

(defun yaml-set-imenu-generic-expression ()
  (make-local-variable 'imenu-generic-expression)
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'imenu-default-create-index-function)
  (setq imenu-generic-expression yaml-imenu-generic-expression))

(add-hook 'yaml-mode-hook 'yaml-set-imenu-generic-expression)


(defun yaml-mode-version ()
  "Display version of `yaml-mode'."
  (interactive)
  (message "yaml-mode %s" yaml-mode-version)
  yaml-mode-version)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode))

(provide 'yaml-mode)

;;; yaml-mode.el ends here
