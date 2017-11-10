;;; org-src.el --- Source code examples in Org       -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2004-2017 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;;	   Bastien Guerry <bzg@gnu.org>
;;         Dan Davison <davison at stats dot ox dot ac dot uk>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the code dealing with source code examples in
;; Org mode.

;;; Code:

(require 'cl-lib)
(require 'org-macs)
(require 'org-compat)
(require 'ob-keys)
(require 'ob-comint)

(declare-function org-base-buffer "org" (buffer))
(declare-function org-do-remove-indentation "org" (&optional n))
(declare-function org-element-at-point "org-element" ())
(declare-function org-element-class "org-element" (datum &optional parent))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-lineage "org-element"
		  (blob &optional types with-self))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))
(declare-function org-footnote-goto-definition "org-footnote"
		  (label &optional location))
(declare-function org-get-indentation "org" (&optional line))
(declare-function org-switch-to-buffer-other-window "org" (&rest args))
(declare-function org-trim "org" (s &optional keep-lead))

(defvar org-inhibit-startup)

(defcustom org-edit-src-turn-on-auto-save nil
  "Non-nil means turn `auto-save-mode' on when editing a source block.
This will save the content of the source code editing buffer into
a newly created file, not the base buffer for this source block.

If you want to regularly save the base buffer instead of the source
code editing buffer, see `org-edit-src-auto-save-idle-delay' instead."
  :group 'org-edit-structure
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-edit-src-auto-save-idle-delay 0
  "Delay before saving a source code buffer back into its base buffer.
When a positive integer N, save after N seconds of idle time.
When 0 (the default), don't auto-save.

If you want to save the source code buffer itself, don't use this.
Check `org-edit-src-turn-on-auto-save' instead."
  :group 'org-edit-structure
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'integer)

(defcustom org-coderef-label-format "(ref:%s)"
  "The default coderef format.
This format string will be used to search for coderef labels in literal
examples (EXAMPLE and SRC blocks).  The format can be overwritten in
an individual literal example with the -l option, like

#+BEGIN_SRC pascal +n -r -l \"((%s))\"
...
#+END_SRC

If you want to use this for HTML export, make sure that the format does
not introduce special font-locking, and avoid the HTML special
characters `<', `>', and `&'.  The reason for this restriction is that
the labels are searched for only after htmlize has done its job."
  :group 'org-edit-structure ; FIXME this is not in the right group
  :type 'string)

(defcustom org-edit-fixed-width-region-mode 'artist-mode
  "The mode that should be used to edit fixed-width regions.
These are the regions where each line starts with a colon."
  :group 'org-edit-structure
  :type '(choice
	  (const artist-mode)
	  (const picture-mode)
	  (const fundamental-mode)
	  (function :tag "Other (specify)")))

(defcustom org-src-preserve-indentation nil
  "If non-nil preserve leading whitespace characters on export.
\\<org-mode-map>
If non-nil leading whitespace characters in source code blocks
are preserved on export, and when switching between the org
buffer and the language mode edit buffer.

When this variable is nil, after editing with `\\[org-edit-src-code]',
the minimum (across-lines) number of leading whitespace characters
are removed from all lines, and the code block is uniformly indented
according to the value of `org-edit-src-content-indentation'."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-edit-src-content-indentation 2
  "Indentation for the content of a source code block.

This should be the number of spaces added to the indentation of the #+begin
line in order to compute the indentation of the block content after
editing it with `\\[org-edit-src-code]'.

It has no effect if `org-src-preserve-indentation' is non-nil."
  :group 'org-edit-structure
  :type 'integer)

(defcustom org-edit-src-persistent-message t
  "Non-nil means show persistent exit help message while editing src examples.
The message is shown in the header-line, which will be created in the
first line of the window showing the editing buffer."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-src-ask-before-returning-to-edit-buffer t
  "Non-nil means ask before switching to an existing edit buffer.
If nil, when `org-edit-src-code' is used on a block that already
has an active edit buffer, it will switch to that edit buffer
immediately; otherwise it will ask whether you want to return to
the existing edit buffer."
  :group 'org-edit-structure
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-src-window-setup 'reorganize-frame
  "How the source code edit buffer should be displayed.
Possible values for this option are:

current-window    Show edit buffer in the current window, keeping all other
                  windows.
other-window      Use `switch-to-buffer-other-window' to display edit buffer.
reorganize-frame  Show only two windows on the current frame, the current
                  window and the edit buffer.  When exiting the edit buffer,
                  return to one window.
other-frame       Use `switch-to-buffer-other-frame' to display edit buffer.
                  Also, when exiting the edit buffer, kill that frame."
  :group 'org-edit-structure
  :type '(choice
	  (const current-window)
	  (const other-frame)
	  (const other-window)
	  (const reorganize-frame)))

(defvar org-src-mode-hook nil
  "Hook run after Org switched a source code snippet to its Emacs mode.
\\<org-mode-map>
This hook will run:
- when editing a source code snippet with `\\[org-edit-special]'
- when formatting a source code snippet for export with htmlize.

You may want to use this hook for example to turn off `outline-minor-mode'
or similar things which you want to have when editing a source code file,
but which mess up the display of a snippet in Org exported files.")

(defcustom org-src-lang-modes
  '(("ocaml" . tuareg) ("elisp" . emacs-lisp) ("ditaa" . artist)
    ("asymptote" . asy) ("dot" . fundamental) ("sqlite" . sql)
    ("calc" . fundamental) ("C" . c) ("cpp" . c++) ("C++" . c++)
    ("screen" . shell-script) ("shell" . sh) ("bash" . sh))
  "Alist mapping languages to their major mode.
The key is the language name, the value is the string that should
be inserted as the name of the major mode.  For many languages this is
simple, but for language where this is not the case, this variable
provides a way to simplify things on the user side.
For example, there is no ocaml-mode in Emacs, but the mode to use is
`tuareg-mode'."
  :group 'org-edit-structure
  :type '(repeat
	  (cons
	   (string "Language name")
	   (symbol "Major mode"))))

(defcustom org-src-block-faces nil
  "Alist of faces to be used for source-block.
Each element is a cell of the format

     (\"language\" FACE)

Where FACE is either a defined face or an anonymous face.

For instance, the following value would color the background of
emacs-lisp source blocks and python source blocks in purple and
green, respectability.

    \\='((\"emacs-lisp\" (:background \"#EEE2FF\"))
      (\"python\" (:background \"#e5ffb8\")))"
  :group 'org-edit-structure
  :type '(repeat (list (string :tag "language")
                       (choice
                        (face :tag "Face")
                        (sexp :tag "Anonymous face"))))
  :version "26.1"
  :package-version '(Org . "9.0"))

(defcustom org-src-tab-acts-natively nil
  "If non-nil, the effect of TAB in a code block is as if it were
issued in the language major mode buffer."
  :type 'boolean
  :version "24.1"
  :group 'org-babel)



;;; Internal functions and variables

(defvar org-src--allow-write-back t)
(defvar org-src--auto-save-timer nil)
(defvar org-src--babel-info nil)
(defvar org-src--beg-marker nil)
(defvar org-src--block-indentation nil)
(defvar org-src--end-marker nil)
(defvar org-src--from-org-mode nil)
(defvar org-src--overlay nil)
(defvar org-src--preserve-indentation nil)
(defvar org-src--remote nil)
(defvar org-src--saved-temp-window-config nil)
(defvar org-src--source-type nil
  "Type of element being edited, as a symbol.")
(defvar org-src--tab-width nil
  "Contains `tab-width' value from Org source buffer.
However, if `indent-tabs-mode' is nil in that buffer, its value
is 0.")

(defun org-src--construct-edit-buffer-name (org-buffer-name lang)
  "Construct the buffer name for a source editing buffer."
  (concat "*Org Src " org-buffer-name "[ " lang " ]*"))

(defun org-src--edit-buffer (beg end)
  "Return buffer editing area between BEG and END.
Return nil if there is no such buffer."
  (catch 'exit
    (dolist (b (buffer-list))
      (with-current-buffer b
	(and (org-src-edit-buffer-p)
	     (= beg org-src--beg-marker)
	     (eq (marker-buffer beg) (marker-buffer org-src--beg-marker))
	     (= end org-src--end-marker)
	     (eq (marker-buffer end) (marker-buffer org-src--end-marker))
	     (throw 'exit b))))))

(defun org-src--source-buffer ()
  "Return source buffer edited by current buffer."
  (unless (org-src-edit-buffer-p) (error "Not in a source buffer"))
  (or (marker-buffer org-src--beg-marker)
      (error "No source buffer available for current editing session")))

(defun org-src--get-lang-mode (lang)
  "Return major mode that should be used for LANG.
LANG is a string, and the returned major mode is a symbol."
  (intern
   (concat
    (let ((l (or (cdr (assoc lang org-src-lang-modes)) lang)))
      (if (symbolp l) (symbol-name l) l))
    "-mode")))

(defun org-src--coordinates (pos beg end)
  "Return coordinates of POS relatively to BEG and END.
POS, BEG and END are buffer positions.  Return value is either
a cons cell (LINE . COLUMN) or symbol `end'.  See also
`org-src--goto-coordinates'."
  (if (>= pos end) 'end
    (org-with-wide-buffer
     (goto-char (max beg pos))
     (cons (count-lines beg (line-beginning-position))
	   ;; Column is relative to the end of line to avoid problems of
	   ;; comma escaping or colons appended in front of the line.
	   (- (current-column)
	      (progn (end-of-line) (current-column)))))))

(defun org-src--goto-coordinates (coord beg end)
  "Move to coordinates COORD relatively to BEG and END.
COORD are coordinates, as returned by `org-src--coordinates',
which see.  BEG and END are buffer positions."
  (goto-char
   (if (eq coord 'end) (max (1- end) beg)
     ;; If BEG happens to be located outside of the narrowed part of
     ;; the buffer, widen it first.
     (org-with-wide-buffer
      (goto-char beg)
      (forward-line (car coord))
      (end-of-line)
      (org-move-to-column (max (+ (current-column) (cdr coord)) 0))
      (point)))))

(defun org-src--contents-area (datum)
  "Return contents boundaries of DATUM.
DATUM is an element or object.  Return a list (BEG END CONTENTS)
where BEG and END are buffer positions and CONTENTS is a string."
  (let ((type (org-element-type datum)))
    (org-with-wide-buffer
     (cond
      ((eq type 'footnote-definition)
       (let* ((beg (progn
		     (goto-char (org-element-property :post-affiliated datum))
		     (search-forward "]")))
	      (end (or (org-element-property :contents-end datum) beg)))
	 (list beg end (buffer-substring-no-properties beg end))))
      ((eq type 'inline-src-block)
       (let ((beg (progn (goto-char (org-element-property :begin datum))
			 (search-forward "{" (line-end-position) t)))
	     (end (progn (goto-char (org-element-property :end datum))
			 (search-backward "}" (line-beginning-position) t))))
	 (list beg end (buffer-substring-no-properties beg end))))
      ((org-element-property :contents-begin datum)
       (let ((beg (org-element-property :contents-begin datum))
	     (end (org-element-property :contents-end datum)))
	 (list beg end (buffer-substring-no-properties beg end))))
      ((memq type '(example-block export-block src-block))
       (list (progn (goto-char (org-element-property :post-affiliated datum))
		    (line-beginning-position 2))
	     (progn (goto-char (org-element-property :end datum))
		    (skip-chars-backward " \r\t\n")
		    (line-beginning-position 1))
	     (org-element-property :value datum)))
      ((memq type '(fixed-width latex-environment table))
       (let ((beg (org-element-property :post-affiliated datum))
	     (end (progn (goto-char (org-element-property :end datum))
			 (skip-chars-backward " \r\t\n")
			 (line-beginning-position 2))))
	 (list beg
	       end
	       (if (eq type 'fixed-width) (org-element-property :value datum)
		 (buffer-substring-no-properties beg end)))))
      (t (error "Unsupported element or object: %s" type))))))

(defun org-src--make-source-overlay (beg end edit-buffer)
  "Create overlay between BEG and END positions and return it.
EDIT-BUFFER is the buffer currently editing area between BEG and
END."
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'face 'secondary-selection)
    (overlay-put overlay 'edit-buffer edit-buffer)
    (overlay-put overlay 'help-echo
		 "Click with mouse-1 to switch to buffer editing this segment")
    (overlay-put overlay 'face 'secondary-selection)
    (overlay-put overlay 'keymap
		 (let ((map (make-sparse-keymap)))
		   (define-key map [mouse-1] 'org-edit-src-continue)
		   map))
    (let ((read-only
	   (list
	    (lambda (&rest _)
	      (user-error
	       "Cannot modify an area being edited in a dedicated buffer")))))
      (overlay-put overlay 'modification-hooks read-only)
      (overlay-put overlay 'insert-in-front-hooks read-only)
      (overlay-put overlay 'insert-behind-hooks read-only))
    overlay))

(defun org-src--remove-overlay ()
  "Remove overlay from current source buffer."
  (when (overlayp org-src--overlay) (delete-overlay org-src--overlay)))

(defun org-src--on-datum-p (datum)
  "Non-nil when point is on DATUM.
DATUM is an element or an object.  Consider blank lines or white
spaces after it as being outside."
  (and (>= (point) (org-element-property :begin datum))
       (<= (point)
	   (org-with-wide-buffer
	    (goto-char (org-element-property :end datum))
	    (skip-chars-backward " \r\t\n")
	    (if (eq (org-element-class datum) 'element)
		(line-end-position)
	      (point))))))

(defun org-src--contents-for-write-back ()
  "Return buffer contents in a format appropriate for write back.
Assume point is in the corresponding edit buffer."
  (let ((indentation-offset
	 (if org-src--preserve-indentation 0
	   (+ (or org-src--block-indentation 0)
	      (if (memq org-src--source-type '(example-block src-block))
		  org-edit-src-content-indentation
		0))))
	(use-tabs? (and (> org-src--tab-width 0) t))
	(source-tab-width org-src--tab-width)
	(contents (org-with-wide-buffer (buffer-string)))
	(write-back org-src--allow-write-back))
    (with-temp-buffer
      ;; Reproduce indentation parameters from source buffer.
      (setq-local indent-tabs-mode use-tabs?)
      (when (> source-tab-width 0) (setq-local tab-width source-tab-width))
      ;; Apply WRITE-BACK function on edit buffer contents.
      (insert (org-no-properties contents))
      (goto-char (point-min))
      (when (functionp write-back) (save-excursion (funcall write-back)))
      ;; Add INDENTATION-OFFSET to every non-empty line in buffer,
      ;; unless indentation is meant to be preserved.
      (when (> indentation-offset 0)
	(while (not (eobp))
	  (skip-chars-forward " \t")
	  (unless (eolp)		;ignore blank lines
	    (let ((i (current-column)))
	      (delete-region (line-beginning-position) (point))
	      (indent-to (+ i indentation-offset))))
	  (forward-line)))
      (buffer-string))))

(defun org-src--edit-element
    (datum name &optional initialize write-back contents remote)
  "Edit DATUM contents in a dedicated buffer NAME.

INITIALIZE is a function to call upon creating the buffer.

When WRITE-BACK is non-nil, assume contents will replace original
region.  Moreover, if it is a function, apply it in the edit
buffer, from point min, before returning the contents.

When CONTENTS is non-nil, display them in the edit buffer.
Otherwise, show DATUM contents as specified by
`org-src--contents-area'.

When REMOTE is non-nil, do not try to preserve point or mark when
moving from the edit area to the source.

Leave point in edit buffer."
  (setq org-src--saved-temp-window-config (current-window-configuration))
  (let* ((area (org-src--contents-area datum))
	 (beg (copy-marker (nth 0 area)))
	 (end (copy-marker (nth 1 area) t))
	 (old-edit-buffer (org-src--edit-buffer beg end))
	 (contents (or contents (nth 2 area))))
    (if (and old-edit-buffer
	     (or (not org-src-ask-before-returning-to-edit-buffer)
		 (y-or-n-p "Return to existing edit buffer ([n] will revert changes)? ")))
	;; Move to existing buffer.
	(org-src-switch-to-buffer old-edit-buffer 'return)
      ;; Discard old edit buffer.
      (when old-edit-buffer
	(with-current-buffer old-edit-buffer (org-src--remove-overlay))
	(kill-buffer old-edit-buffer))
      (let* ((org-mode-p (derived-mode-p 'org-mode))
	     (source-tab-width (if indent-tabs-mode tab-width 0))
	     (type (org-element-type datum))
	     (ind (org-with-wide-buffer
		   (goto-char (org-element-property :begin datum))
		   (org-get-indentation)))
	     (preserve-ind
	      (and (memq type '(example-block src-block))
		   (or (org-element-property :preserve-indent datum)
		       org-src-preserve-indentation)))
	     ;; Store relative positions of mark (if any) and point
	     ;; within the edited area.
	     (point-coordinates (and (not remote)
				     (org-src--coordinates (point) beg end)))
	     (mark-coordinates (and (not remote)
				    (org-region-active-p)
				    (let ((m (mark)))
				      (and (>= m beg) (>= end m)
					   (org-src--coordinates m beg end)))))
	     ;; Generate a new edit buffer.
	     (buffer (generate-new-buffer name))
	     ;; Add an overlay on top of source.
	     (overlay (org-src--make-source-overlay beg end buffer)))
	;; Switch to edit buffer.
	(org-src-switch-to-buffer buffer 'edit)
	;; Insert contents.
	(insert contents)
	(remove-text-properties (point-min) (point-max)
				'(display nil invisible nil intangible nil))
	(unless preserve-ind (org-do-remove-indentation))
	(set-buffer-modified-p nil)
	(setq buffer-file-name nil)
	;; Initialize buffer.
	(when (functionp initialize)
	  (let ((org-inhibit-startup t))
	    (condition-case e
		(funcall initialize)
	      (error (message "Initialization fails with: %S"
			      (error-message-string e))))))
	;; Transmit buffer-local variables for exit function.  It must
	;; be done after initializing major mode, as this operation
	;; may reset them otherwise.
	(setq-local org-src--tab-width source-tab-width)
	(setq-local org-src--from-org-mode org-mode-p)
	(setq-local org-src--beg-marker beg)
	(setq-local org-src--end-marker end)
	(setq-local org-src--remote remote)
	(setq-local org-src--source-type type)
	(setq-local org-src--block-indentation ind)
	(setq-local org-src--preserve-indentation preserve-ind)
	(setq-local org-src--overlay overlay)
	(setq-local org-src--allow-write-back write-back)
	;; Start minor mode.
	(org-src-mode)
	;; Move mark and point in edit buffer to the corresponding
	;; location.
	(if remote
	    (progn
	      ;; Put point at first non read-only character after
	      ;; leading blank.
	      (goto-char
	       (or (text-property-any (point-min) (point-max) 'read-only nil)
		   (point-max)))
	      (skip-chars-forward " \r\t\n"))
	  ;; Set mark and point.
	  (when mark-coordinates
	    (org-src--goto-coordinates mark-coordinates (point-min) (point-max))
	    (push-mark (point) 'no-message t)
	    (setq deactivate-mark nil))
	  (org-src--goto-coordinates
	   point-coordinates (point-min) (point-max)))))))



;;; Fontification of source blocks

(defun org-src-font-lock-fontify-block (lang start end)
  "Fontify code block.
This function is called by emacs automatic fontification, as long
as `org-src-fontify-natively' is non-nil."
  (let ((lang-mode (org-src--get-lang-mode lang)))
    (when (fboundp lang-mode)
      (let ((string (buffer-substring-no-properties start end))
	    (modified (buffer-modified-p))
	    (org-buffer (current-buffer)))
	(remove-text-properties start end '(face nil))
	(with-current-buffer
	    (get-buffer-create
	     (format " *org-src-fontification:%s*" lang-mode))
	  (let ((inhibit-modification-hooks nil))
	    (erase-buffer)
	    ;; Add string and a final space to ensure property change.
	    (insert string " "))
	  (unless (eq major-mode lang-mode) (funcall lang-mode))
	  (org-font-lock-ensure)
	  (let ((pos (point-min)) next)
	    (while (setq next (next-property-change pos))
	      ;; Handle additional properties from font-lock, so as to
	      ;; preserve, e.g., composition.
	      (dolist (prop (cons 'face font-lock-extra-managed-props))
		(let ((new-prop (get-text-property pos prop)))
		  (put-text-property
		   (+ start (1- pos)) (1- (+ start next)) prop new-prop
		   org-buffer)))
	      (setq pos next))))
	;; Add Org faces.
	(let ((src-face (nth 1 (assoc-string lang org-src-block-faces t))))
          (when (or (facep src-face) (listp src-face))
            (font-lock-append-text-property start end 'face src-face))
	  (font-lock-append-text-property start end 'face 'org-block))
	(add-text-properties
	 start end
	 '(font-lock-fontified t fontified t font-lock-multiline t))
	(set-buffer-modified-p modified)))))


;;; Escape contents

(defun org-escape-code-in-region (beg end)
  "Escape lines between BEG and END.
Escaping happens when a line starts with \"*\", \"#+\", \",*\" or
\",#+\" by appending a comma to it."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (while (re-search-backward "^[ \t]*\\(,*\\(?:\\*\\|#\\+\\)\\)" beg t)
      (save-excursion (replace-match ",\\1" nil nil nil 1)))))

(defun org-escape-code-in-string (s)
  "Escape lines in string S.
Escaping happens when a line starts with \"*\", \"#+\", \",*\" or
\",#+\" by appending a comma to it."
  (replace-regexp-in-string "^[ \t]*\\(,*\\(?:\\*\\|#\\+\\)\\)" ",\\1"
			    s nil nil 1))

(defun org-unescape-code-in-region (beg end)
  "Un-escape lines between BEG and END.
Un-escaping happens by removing the first comma on lines starting
with \",*\", \",#+\", \",,*\" and \",,#+\"."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (while (re-search-backward "^[ \t]*,*\\(,\\)\\(?:\\*\\|#\\+\\)" beg t)
      (save-excursion (replace-match "" nil nil nil 1)))))

(defun org-unescape-code-in-string (s)
  "Un-escape lines in string S.
Un-escaping happens by removing the first comma on lines starting
with \",*\", \",#+\", \",,*\" and \",,#+\"."
  (replace-regexp-in-string
   "^[ \t]*,*\\(,\\)\\(?:\\*\\|#\\+\\)" "" s nil nil 1))



;;; Org src minor mode

(defvar org-src-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c'" 'org-edit-src-exit)
    (define-key map "\C-c\C-k" 'org-edit-src-abort)
    (define-key map "\C-x\C-s" 'org-edit-src-save)
    map))

(define-minor-mode org-src-mode
  "Minor mode for language major mode buffers generated by Org.
\\<org-mode-map>
This minor mode is turned on in two situations:
  - when editing a source code snippet with `\\[org-edit-special]'
  - when formatting a source code snippet for export with htmlize.

\\{org-src-mode-map}

See also `org-src-mode-hook'."
  nil " OrgSrc" nil
  (when org-edit-src-persistent-message
    (setq-local
     header-line-format
     (substitute-command-keys
      (if org-src--allow-write-back
	  "Edit, then exit with `\\[org-edit-src-exit]' or abort with \
`\\[org-edit-src-abort]'"
	"Exit with `\\[org-edit-src-exit]' or abort with \
`\\[org-edit-src-abort]'"))))
  ;; Possibly activate various auto-save features (for the edit buffer
  ;; or the source buffer).
  (when org-edit-src-turn-on-auto-save
    (setq buffer-auto-save-file-name
	  (concat (make-temp-name "org-src-")
		  (format-time-string "-%Y-%d-%m")
		  ".txt")))
  (unless (or org-src--auto-save-timer (zerop org-edit-src-auto-save-idle-delay))
    (setq org-src--auto-save-timer
	  (run-with-idle-timer
	   org-edit-src-auto-save-idle-delay t
	   (lambda ()
	     (save-excursion
	       (let (edit-flag)
		 (dolist (b (buffer-list))
		   (with-current-buffer b
		     (when (org-src-edit-buffer-p)
		       (unless edit-flag (setq edit-flag t))
		       (when (buffer-modified-p) (org-edit-src-save)))))
		 (unless edit-flag
		   (cancel-timer org-src--auto-save-timer)
		   (setq org-src--auto-save-timer nil)))))))))

(defun org-src-mode-configure-edit-buffer ()
  (when (bound-and-true-p org-src--from-org-mode)
    (add-hook 'kill-buffer-hook #'org-src--remove-overlay nil 'local)
    (if (bound-and-true-p org-src--allow-write-back)
	(progn
	  (setq buffer-offer-save t)
	  (setq buffer-file-name
		(concat (buffer-file-name (marker-buffer org-src--beg-marker))
			"[" (buffer-name) "]"))
	  (setq-local write-contents-functions '(org-edit-src-save)))
      (setq buffer-read-only t))))

(add-hook 'org-src-mode-hook #'org-src-mode-configure-edit-buffer)



;;; Babel related functions

(defun org-src-associate-babel-session (info)
  "Associate edit buffer with comint session."
  (interactive)
  (let ((session (cdr (assq :session (nth 2 info)))))
    (and session (not (string= session "none"))
	 (org-babel-comint-buffer-livep session)
	 (let ((f (intern (format "org-babel-%s-associate-session"
                                  (nth 0 info)))))
           (and (fboundp f) (funcall f session))))))

(defun org-src-babel-configure-edit-buffer ()
  (when org-src--babel-info
    (org-src-associate-babel-session org-src--babel-info)))

(add-hook 'org-src-mode-hook #'org-src-babel-configure-edit-buffer)


;;; Public API

(defmacro org-src-do-at-code-block (&rest body)
  "Execute BODY from an edit buffer in the Org mode buffer."
  (declare (debug (body)))
  `(let ((beg-marker org-src--beg-marker))
     (when beg-marker
       (with-current-buffer (marker-buffer beg-marker)
	 (goto-char beg-marker)
	 ,@body))))

(defun org-src-do-key-sequence-at-code-block (&optional key)
  "Execute key sequence at code block in the source Org buffer.
The command bound to KEY in the Org-babel key map is executed
remotely with point temporarily at the start of the code block in
the Org buffer.

This command is not bound to a key by default, to avoid conflicts
with language major mode bindings.  To bind it to C-c @ in all
language major modes, you could use

  (add-hook \\='org-src-mode-hook
            (lambda () (define-key org-src-mode-map \"\\C-c@\"
                    \\='org-src-do-key-sequence-at-code-block)))

In that case, for example, C-c @ t issued in code edit buffers
would tangle the current Org code block, C-c @ e would execute
the block and C-c @ h would display the other available
Org-babel commands."
  (interactive "kOrg-babel key: ")
  (if (equal key (kbd "C-g")) (keyboard-quit)
    (org-edit-src-save)
    (org-src-do-at-code-block
     (call-interactively (lookup-key org-babel-map key)))))

(defun org-src-edit-buffer-p (&optional buffer)
  "Non-nil when current buffer is a source editing buffer.
If BUFFER is non-nil, test it instead."
  (let ((buffer (org-base-buffer (or buffer (current-buffer)))))
    (and (buffer-live-p buffer)
	 (local-variable-p 'org-src--beg-marker buffer)
	 (local-variable-p 'org-src--end-marker buffer))))

(defun org-src-switch-to-buffer (buffer context)
  (pcase org-src-window-setup
    (`current-window (pop-to-buffer-same-window buffer))
    (`other-window
     (switch-to-buffer-other-window buffer))
    (`other-frame
     (pcase context
       (`exit
	(let ((frame (selected-frame)))
	  (switch-to-buffer-other-frame buffer)
	  (delete-frame frame)))
       (`save
	(kill-buffer (current-buffer))
	(pop-to-buffer-same-window buffer))
       (_ (switch-to-buffer-other-frame buffer))))
    (`reorganize-frame
     (when (eq context 'edit) (delete-other-windows))
     (org-switch-to-buffer-other-window buffer)
     (when (eq context 'exit) (delete-other-windows)))
    (`switch-invisibly (set-buffer buffer))
    (_
     (message "Invalid value %s for `org-src-window-setup'"
	      org-src-window-setup)
     (pop-to-buffer-same-window buffer))))

(defun org-src-coderef-format (&optional element)
  "Return format string for block at point.

When optional argument ELEMENT is provided, use that block.
Otherwise, assume point is either at a source block, at an
example block.

If point is in an edit buffer, retrieve format string associated
to the remote source block."
  (cond
   ((and element (org-element-property :label-fmt element)))
   ((org-src-edit-buffer-p) (org-src-do-at-code-block (org-src-coderef-format)))
   ((org-element-property :label-fmt (org-element-at-point)))
   (t org-coderef-label-format)))

(defun org-src-coderef-regexp (fmt &optional label)
  "Return regexp matching a coderef format string FMT.

When optional argument LABEL is non-nil, match coderef for that
label only.

Match group 1 contains the full coderef string with surrounding
white spaces.  Match group 2 contains the same string without any
surrounding space.  Match group 3 contains the label.

A coderef format regexp can only match at the end of a line."
  (format "\\([ \t]*\\(%s\\)[ \t]*\\)$"
	  (replace-regexp-in-string
	   "%s"
	   (if label (regexp-quote label) "\\([-a-zA-Z0-9_][-a-zA-Z0-9_ ]*\\)")
	   (regexp-quote fmt)
	   nil t)))

(defun org-edit-footnote-reference ()
  "Edit definition of footnote reference at point."
  (interactive)
  (let* ((context (org-element-context))
	 (label (org-element-property :label context)))
    (unless (and (eq (org-element-type context) 'footnote-reference)
		 (org-src--on-datum-p context))
      (user-error "Not on a footnote reference"))
    (unless label (user-error "Cannot edit remotely anonymous footnotes"))
    (let* ((definition (org-with-wide-buffer
			(org-footnote-goto-definition label)
			(backward-char)
			(org-element-context)))
	   (inline? (eq 'footnote-reference (org-element-type definition)))
	   (contents
	    (org-with-wide-buffer
	     (buffer-substring-no-properties
	      (or (org-element-property :post-affiliated definition)
		  (org-element-property :begin definition))
	      (cond
	       (inline? (1+ (org-element-property :contents-end definition)))
	       ((org-element-property :contents-end definition))
	       (t (goto-char (org-element-property :post-affiliated definition))
		  (line-end-position)))))))
      (add-text-properties
       0
       (progn (string-match (if inline? "\\`\\[fn:.*?:" "\\`.*?\\]") contents)
	      (match-end 0))
       '(read-only "Cannot edit footnote label" front-sticky t rear-nonsticky t)
       contents)
      (when inline?
	(let ((l (length contents)))
	  (add-text-properties
	   (1- l) l
	   '(read-only "Cannot edit past footnote reference"
		       front-sticky nil rear-nonsticky nil)
	   contents)))
      (org-src--edit-element
       definition
       (format "*Edit footnote [%s]*" label)
       (let ((source (current-buffer)))
	 (lambda ()
	   (org-mode)
	   (org-clone-local-variables source)))
       (lambda ()
	 (if (not inline?) (delete-region (point) (search-forward "]"))
	   (delete-region (point) (search-forward ":" nil t 2))
	   (delete-region (1- (point-max)) (point-max))
	   (when (re-search-forward "\n[ \t]*\n" nil t)
	     (user-error "Inline definitions cannot contain blank lines"))
	   ;; If footnote reference belongs to a table, make sure to
	   ;; remove any newline characters in order to preserve
	   ;; table's structure.
	   (when (org-element-lineage definition '(table-cell))
	     (while (search-forward "\n" nil t) (replace-match "")))))
       contents
       'remote))
    ;; Report success.
    t))

(defun org-edit-table.el ()
  "Edit \"table.el\" table at point.
\\<org-src-mode-map>
A new buffer is created and the table is copied into it.  Then
the table is recognized with `table-recognize'.  When done
editing, exit with `\\[org-edit-src-exit]'.  The edited text will \
then replace
the area in the Org mode buffer.

Throw an error when not at such a table."
  (interactive)
  (let ((element (org-element-at-point)))
    (unless (and (eq (org-element-type element) 'table)
		 (eq (org-element-property :type element) 'table.el)
		 (org-src--on-datum-p element))
      (user-error "Not in a table.el table"))
    (org-src--edit-element
     element
     (org-src--construct-edit-buffer-name (buffer-name) "Table")
     #'text-mode t)
    (when (bound-and-true-p flyspell-mode) (flyspell-mode -1))
    (table-recognize)
    t))

(defun org-edit-latex-environment ()
  "Edit LaTeX environment at point.
\\<org-src-mode-map>
The LaTeX environment is copied into a new buffer.  Major mode is
set to the one associated to \"latex\" in `org-src-lang-modes',
or to `latex-mode' if there is none.

When done, exit with `\\[org-edit-src-exit]'.  The edited text \
will then replace
the LaTeX environment in the Org mode buffer."
  (interactive)
  (let ((element (org-element-at-point)))
    (unless (and (eq (org-element-type element) 'latex-environment)
		 (org-src--on-datum-p element))
      (user-error "Not in a LaTeX environment"))
    (org-src--edit-element
     element
     (org-src--construct-edit-buffer-name (buffer-name) "LaTeX environment")
     (org-src--get-lang-mode "latex")
     t)
    t))

(defun org-edit-export-block ()
  "Edit export block at point.
\\<org-src-mode-map>
A new buffer is created and the block is copied into it, and the
buffer is switched into an appropriate major mode.  See also
`org-src-lang-modes'.

When done, exit with `\\[org-edit-src-exit]'.  The edited text \
will then replace
the area in the Org mode buffer.

Throw an error when not at an export block."
  (interactive)
  (let ((element (org-element-at-point)))
    (unless (and (eq (org-element-type element) 'export-block)
		 (org-src--on-datum-p element))
      (user-error "Not in an export block"))
    (let* ((type (downcase (or (org-element-property :type element)
			       ;; Missing export-block type.  Fallback
			       ;; to default mode.
			       "fundamental")))
	   (mode (org-src--get-lang-mode type)))
      (unless (functionp mode) (error "No such language mode: %s" mode))
      (org-src--edit-element
       element
       (org-src--construct-edit-buffer-name (buffer-name) type)
       mode
       (lambda () (org-escape-code-in-region (point-min) (point-max)))))
    t))

(defun org-edit-src-code (&optional code edit-buffer-name)
  "Edit the source or example block at point.
\\<org-src-mode-map>
The code is copied to a separate buffer and the appropriate mode
is turned on.  When done, exit with `\\[org-edit-src-exit]'.  This \
will remove the
original code in the Org buffer, and replace it with the edited
version.  See `org-src-window-setup' to configure the display of
windows containing the Org buffer and the code buffer.

When optional argument CODE is a string, edit it in a dedicated
buffer instead.

When optional argument EDIT-BUFFER-NAME is non-nil, use it as the
name of the sub-editing buffer."
  (interactive)
  (let* ((element (org-element-at-point))
	 (type (org-element-type element)))
    (unless (and (memq type '(example-block src-block))
		 (org-src--on-datum-p element))
      (user-error "Not in a source or example block"))
    (let* ((lang
	    (if (eq type 'src-block) (org-element-property :language element)
	      "example"))
	   (lang-f (and (eq type 'src-block) (org-src--get-lang-mode lang)))
	   (babel-info (and (eq type 'src-block)
			    (org-babel-get-src-block-info 'light)))
	   deactivate-mark)
      (when (and (eq type 'src-block) (not (functionp lang-f)))
	(error "No such language mode: %s" lang-f))
      (org-src--edit-element
       element
       (or edit-buffer-name
	   (org-src--construct-edit-buffer-name (buffer-name) lang))
       lang-f
       (and (null code)
	    (lambda () (org-escape-code-in-region (point-min) (point-max))))
       (and code (org-unescape-code-in-string code)))
      ;; Finalize buffer.
      (setq-local org-coderef-label-format
		  (or (org-element-property :label-fmt element)
		      org-coderef-label-format))
      (when (eq type 'src-block)
	(setq-local org-src--babel-info babel-info)
	(let ((edit-prep-func (intern (concat "org-babel-edit-prep:" lang))))
	  (when (fboundp edit-prep-func)
	    (funcall edit-prep-func babel-info))))
      t)))

(defun org-edit-inline-src-code ()
  "Edit inline source code at point."
  (interactive)
  (let ((context (org-element-context)))
    (unless (and (eq (org-element-type context) 'inline-src-block)
		 (org-src--on-datum-p context))
      (user-error "Not on inline source code"))
    (let* ((lang (org-element-property :language context))
	   (lang-f (org-src--get-lang-mode lang))
	   (babel-info (org-babel-get-src-block-info 'light))
	   deactivate-mark)
      (unless (functionp lang-f) (error "No such language mode: %s" lang-f))
      (org-src--edit-element
       context
       (org-src--construct-edit-buffer-name (buffer-name) lang)
       lang-f
       (lambda ()
	 ;; Inline src blocks are limited to one line.
	 (while (re-search-forward "\n[ \t]*" nil t) (replace-match " "))
	 ;; Trim contents.
	 (goto-char (point-min))
	 (skip-chars-forward " \t")
	 (delete-region (point-min) (point))
	 (goto-char (point-max))
	 (skip-chars-backward " \t")
	 (delete-region (point) (point-max))))
      ;; Finalize buffer.
      (setq-local org-src--babel-info babel-info)
      (setq-local org-src--preserve-indentation t)
      (let ((edit-prep-func (intern (concat "org-babel-edit-prep:" lang))))
	(when (fboundp edit-prep-func) (funcall edit-prep-func babel-info)))
      ;; Return success.
      t)))

(defun org-edit-fixed-width-region ()
  "Edit the fixed-width ASCII drawing at point.
\\<org-src-mode-map>
This must be a region where each line starts with a colon
followed by a space or a newline character.

A new buffer is created and the fixed-width region is copied into
it, and the buffer is switched into the major mode defined in
`org-edit-fixed-width-region-mode', which see.

When done, exit with `\\[org-edit-src-exit]'.  The edited text \
will then replace
the area in the Org mode buffer."
  (interactive)
  (let ((element (org-element-at-point)))
    (unless (and (eq (org-element-type element) 'fixed-width)
		 (org-src--on-datum-p element))
      (user-error "Not in a fixed-width area"))
    (org-src--edit-element
     element
     (org-src--construct-edit-buffer-name (buffer-name) "Fixed Width")
     org-edit-fixed-width-region-mode
     (lambda () (while (not (eobp)) (insert ": ") (forward-line))))
    ;; Return success.
    t))

(defun org-edit-src-abort ()
  "Abort editing of the src code and return to the Org buffer."
  (interactive)
  (let (org-src--allow-write-back) (org-edit-src-exit)))

(defun org-edit-src-continue (e)
  "Unconditionally return to buffer editing area under point.
Throw an error if there is no such buffer."
  (interactive "e")
  (mouse-set-point e)
  (let ((buf (get-char-property (point) 'edit-buffer)))
    (if buf (org-src-switch-to-buffer buf 'continue)
      (user-error "No sub-editing buffer for area at point"))))

(defun org-edit-src-save ()
  "Save parent buffer with current state source-code buffer."
  (interactive)
  (unless (org-src-edit-buffer-p) (user-error "Not in a sub-editing buffer"))
  (set-buffer-modified-p nil)
  (let ((edited-code (org-src--contents-for-write-back))
	(beg org-src--beg-marker)
	(end org-src--end-marker)
	(overlay org-src--overlay))
    (with-current-buffer (org-src--source-buffer)
      (undo-boundary)
      (goto-char beg)
      ;; Temporarily disable read-only features of OVERLAY in order to
      ;; insert new contents.
      (delete-overlay overlay)
      (delete-region beg end)
      (let ((expecting-bol (bolp)))
	(insert edited-code)
	(when (and expecting-bol (not (bolp))) (insert "\n")))
      (save-buffer)
      (move-overlay overlay beg (point))))
  ;; `write-contents-functions' requires the function to return
  ;; a non-nil value so that other functions are not called.
  t)

(defun org-edit-src-exit ()
  "Kill current sub-editing buffer and return to source buffer."
  (interactive)
  (unless (org-src-edit-buffer-p) (error "Not in a sub-editing buffer"))
  (let* ((beg org-src--beg-marker)
	 (end org-src--end-marker)
	 (write-back org-src--allow-write-back)
	 (remote org-src--remote)
	 (coordinates (and (not remote)
			   (org-src--coordinates (point) 1 (point-max))))
	 (code (and write-back (org-src--contents-for-write-back))))
    (set-buffer-modified-p nil)
    ;; Switch to source buffer.  Kill sub-editing buffer.
    (let ((edit-buffer (current-buffer))
	  (source-buffer (marker-buffer beg)))
      (unless source-buffer (error "Source buffer disappeared.  Aborting"))
      (org-src-switch-to-buffer source-buffer 'exit)
      (kill-buffer edit-buffer))
    ;; Insert modified code.  Ensure it ends with a newline character.
    (org-with-wide-buffer
     (when (and write-back (not (equal (buffer-substring beg end) code)))
       (undo-boundary)
       (goto-char beg)
       (delete-region beg end)
       (let ((expecting-bol (bolp)))
	 (insert code)
	 (when (and expecting-bol (not (bolp))) (insert "\n")))))
    ;; If we are to return to source buffer, put point at an
    ;; appropriate location.  In particular, if block is hidden, move
    ;; to the beginning of the block opening line.
    (unless remote
      (goto-char beg)
      (cond
       ;; Block is hidden; move at start of block.
       ((cl-some (lambda (o) (eq (overlay-get o 'invisible) 'org-hide-block))
		 (overlays-at (point)))
	(beginning-of-line 0))
       (write-back (org-src--goto-coordinates coordinates beg end))))
    ;; Clean up left-over markers and restore window configuration.
    (set-marker beg nil)
    (set-marker end nil)
    (when org-src--saved-temp-window-config
      (set-window-configuration org-src--saved-temp-window-config)
      (setq org-src--saved-temp-window-config nil))))


(provide 'org-src)

;;; org-src.el ends here
