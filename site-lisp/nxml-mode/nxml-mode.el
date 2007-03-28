;;; nxml-mode.el --- a new XML mode

;; Copyright (C) 2003, 2004 Free Software Foundation, Inc.

;; Author: James Clark
;; Keywords: XML

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; To use this include rng-auto.el in your .emacs.

;; See nxml-rap.el for description of parsing strategy.

;; The font locking here is independent of font-lock.el.  We want to
;; do more sophisticated handling of changes and we want to use the
;; same xmltok rather than regexps for parsing so that we parse
;; consistently and correctly.

;;; Code:

(when (featurep 'mucs)
  (error "nxml-mode is not compatible with Mule-UCS"))

(require 'xmltok)
(require 'nxml-enc)
(require 'nxml-glyph)
(require 'nxml-util)
(require 'nxml-rap)
(require 'nxml-outln)

;;; Customization

(defgroup nxml nil
  "New XML editing mode"
  :group 'languages
  :group 'wp)

(defgroup nxml-highlighting-faces nil
  "Faces for XML syntax highlighting."
  :group 'nxml
  :group 'font-lock-highlighting-faces)

(defcustom nxml-syntax-highlight-flag t
  "*Non-nil means nxml-mode should perform syntax highlighting."
  :group 'nxml
  :type 'boolean)

(defcustom nxml-char-ref-display-glyph-flag t
  "*Non-nil means display glyph following character reference.
The glyph is displayed in `nxml-glyph-face'.  The hook
`nxml-glyph-set-hook' can be used to customize for which characters
glyphs are displayed."
  :group 'nxml
  :type 'boolean)

(defcustom nxml-mode-hook nil
  "Hook run by command `nxml-mode'."
  :group 'nxml
  :type 'hook)

(defcustom nxml-sexp-element-flag nil
  "*Non-nil means sexp commands treat an element as a single expression."
  :group 'nxml
  :type 'boolean)

(defcustom nxml-slash-auto-complete-flag nil
  "*Non-nil means typing a slash automatically completes the end-tag.
This is used by `nxml-electric-slash'."
  :group 'nxml
  :type 'boolean)

(defcustom nxml-child-indent 2
  "*Indentation for the children of an element relative to the start-tag.
This only applies when the line or lines containing the start-tag contains
nothing else other than that start-tag."
  :group 'nxml
  :type 'integer)

(defcustom nxml-attribute-indent 4
  "*Indentation for the attributes of an element relative to the start-tag.
This only applies when the first attribute of a tag starts a line. In other
cases, the first attribute on one line is indented the same as the first
attribute on the previous line."
  :group 'nxml
  :type 'integer)

(defvar nxml-fontify-chunk-size 500)

(defcustom nxml-bind-meta-tab-to-complete-flag (not window-system)
  "*Non-nil means bind M-TAB in `nxml-mode-map' to `nxml-complete'.
C-return will be bound to `nxml-complete' in any case.
M-TAB gets swallowed by many window systems/managers, and
`documentation' will show M-TAB rather than C-return as the
binding `rng-complete' when both are bound.  So it's better
to bind M-TAB only when it will work."
  :group 'nxml
  :set (lambda (sym flag)
	 (set-default sym flag)
	 (when (and (boundp 'nxml-mode-map) nxml-mode-map)
	   (define-key nxml-mode-map "\M-\t" (and flag 'nxml-complete))))
  :type 'boolean)

(defcustom nxml-prefer-utf-16-to-utf-8-flag nil
  "*Non-nil means prefer UTF-16 to UTF-8 when saving a buffer.
This is used only when a buffer does not contain an encoding declaration
and when its current `buffer-file-coding-system' specifies neither UTF-16
nor UTF-8."
  :group 'nxml
  :type 'boolean)

(defcustom nxml-prefer-utf-16-little-to-big-endian-flag (eq system-type
							    'windows-nt)
  "*Non-nil means prefer little-endian to big-endian byte-order for UTF-16.
This is used only for saving a buffer; when reading the byte-order is
auto-detected. It may be relevant both when there is no encoding declaration
and when the encoding declaration specifies `UTF-16'."
  :group 'nxml
  :type 'boolean)

(defcustom nxml-default-buffer-file-coding-system nil
  "*Default value for `buffer-file-coding-system' for a buffer for a new file.
Nil means use the default value of `buffer-file-coding-system' as normal.
A buffer's `buffer-file-coding-system' affects what \\[nxml-insert-xml-declaration] inserts."
  :group 'nxml
  :type 'coding-system)

(defcustom nxml-auto-insert-xml-declaration-flag nil
  "*Non-nil means automatically insert an XML declaration in a new file.
The XML declaration is inserted using `nxml-insert-xml-declaration'."
  :group 'nxml
  :type 'boolean)

;; The following are the colors we use with a light background.
;; The two blues have the same hue but contrasting saturation/value.
;; The hue of the green is 120 degrees different from that of the
;; blue.  The red used for highlighting errors is 120 degrees
;; different again.  We use the light blue only for refs and
;; delimiters, since these are short (long stretches in a light color
;; would be too hard to read).  The dark blue is closest to black
;; (which we use by default for text), so we use it for attribute
;; values, which are similar to text.

(defconst nxml-light-blue-color "#9292C9") ; hue 240
(defconst nxml-dark-blue-color "#3A3A7B") ; hue 240
(defconst nxml-green-color "#257A25") ; hue 120

;; Similar principles apply with a dark background.  However,
;; we switch green and blue, because darker blues are very hard to
;; read (for me anyway) on a dark background.

(defconst nxml-sky-blue-color "#ACACFC") ; hue 240
(defconst nxml-dark-green-color "#00AD00") ; hue 120
(defconst nxml-light-green-color "#70F170") ; hue 120

(defface nxml-delimited-data-face
  `((((class color) (background light)) (:foreground ,nxml-dark-blue-color))
    (((class color) (background dark)) (:foreground ,nxml-light-green-color)))
  "Face used to highlight data enclosed between delimiters.
By default, this is inherited by `nxml-attribute-value-face'
and `nxml-processing-instruction-content-face'."
  :group 'nxml-highlighting-faces)

(defface nxml-name-face
  `((((class color) (background light)) (:foreground ,nxml-green-color))
    (((class color) (background dark)) (:foreground ,nxml-sky-blue-color)))
  "Face used to highlight various names.
This includes element and attribute names, processing
instruction targets and the CDATA keyword in a CDATA section.
This is not used directly, but only via inheritance by other faces."
  :group 'nxml-highlighting-faces)

(defface nxml-ref-face
  `((((class color) (background light)) (:foreground ,nxml-light-blue-color))
    (((class color) (background dark)) (:foreground ,nxml-dark-green-color)))
  "Face used to highlight character and entity references.
This is not used directly, but only via inheritance by other faces."
  :group 'nxml-highlighting-faces)

(defface nxml-delimiter-face
  `((((class color) (background light)) (:foreground ,nxml-light-blue-color))
    (((class color) (background dark)) (:foreground ,nxml-dark-green-color))
    (t (:bold t)))
  "Face used to highlight delimiters.
This is not used directly, but only via inheritance by other faces."
  :group 'nxml-highlighting-faces)

(defface nxml-text-face
  nil
  "Face used to highlight text."
  :group 'nxml-highlighting-faces)

(defface nxml-comment-content-face
  '((t (:italic t)))
  "Face used to highlight the content of comments."
  :group 'nxml-highlighting-faces)

(defface nxml-comment-delimiter-face
  '((t (:inherit nxml-delimiter-face)))
  "Face used for the delimiters of comments, i.e <!-- and -->."
  :group 'nxml-highlighting-faces)

(defface nxml-processing-instruction-delimiter-face
  '((t (:inherit nxml-delimiter-face)))
  "Face used for the delimiters of processing instructions, i.e <? and ?>."
  :group 'nxml-highlighting-faces)

(defface nxml-processing-instruction-target-face
  '((t (:inherit nxml-name-face)))
  "Face used for the target of processing instructions."
  :group 'nxml-highlighting-faces)

(defface nxml-processing-instruction-content-face
  '((t (:inherit nxml-delimited-data-face)))
  "Face used for the content of processing instructions."
  :group 'nxml-highlighting-faces)

(defface nxml-cdata-section-delimiter-face
  '((t (:inherit nxml-delimiter-face)))
  "Face used for the delimiters of CDATA sections, i.e <![, [, and ]]>."
  :group 'nxml-highlighting-faces)

(defface nxml-cdata-section-CDATA-face
  '((t (:inherit nxml-name-face)))
  "Face used for the CDATA keyword in CDATA sections."
  :group 'nxml-highlighting-faces)

(defface nxml-cdata-section-content-face
  '((t (:inherit nxml-text-face)))
  "Face used for the content of CDATA sections."
  :group 'nxml-highlighting-faces)

(defface nxml-char-ref-number-face
  '((t (:inherit nxml-ref-face)))
  "Face used for the number in character references.
This includes ths `x' in hex references."
  :group 'nxml-highlighting-faces)

(defface nxml-char-ref-delimiter-face
  '((t (:inherit nxml-ref-face)))
  "Face used for the delimiters of character references, i.e &# and ;."
  :group 'nxml-highlighting-faces)

(defface nxml-entity-ref-name-face
  '((t (:inherit nxml-ref-face)))
  "Face used for the entity name in general entity references."
  :group 'nxml-highlighting-faces)

(defface nxml-entity-ref-delimiter-face
  '((t (:inherit nxml-ref-face)))
  "Face used for the delimiters of entity references, i.e & and ;."
  :group 'nxml-highlighting-faces)

(defface nxml-tag-delimiter-face
  '((t (:inherit nxml-delimiter-face)))
  "Face used for the angle brackets delimiting tags.
`nxml-tag-slash-face' is used for slashes."
  :group 'nxml-highlighting-faces)

(defface nxml-tag-slash-face
  '((t (:inherit nxml-name-face)))
  "Face used for slashes in tags, both in end-tags and empty-elements."
  :group 'nxml-highlighting-faces)

(defface nxml-element-prefix-face
  '((t (:inherit nxml-name-face)))
  "Face used for the prefix of elements."
  :group 'nxml-highlighting-faces)

(defface nxml-element-colon-face
  '((t (:inherit nxml-name-face)))
  "Face used for the colon in element names."
  :group 'nxml-highlighting-faces)

(defface nxml-element-local-name-face
  '((t (:inherit nxml-name-face)))
  "Face used for the local name of elements."
  :group 'nxml-highlighting-faces)

(defface nxml-attribute-prefix-face
  '((t (:inherit nxml-name-face)))
  "Face used for the prefix of attributes."
  :group 'nxml-highlighting-faces)

(defface nxml-attribute-colon-face
  '((t (:inherit nxml-name-face)))
  "Face used for the colon in attribute names."
  :group 'nxml-highlighting-faces)
  
(defface nxml-attribute-local-name-face
  '((t (:inherit nxml-name-face)))
  "Face used for the local name of attributes."
  :group 'nxml-highlighting-faces)

(defface nxml-namespace-attribute-xmlns-face
  '((t (:inherit nxml-name-face)))
  "Face used for `xmlns' in namespace attributes."
  :group 'nxml-highlighting-faces)

(defface nxml-namespace-attribute-colon-face
  '((t (:inherit nxml-name-face)))
  "Face used for the colon in namespace attributes."
  :group 'nxml-highlighting-faces)

(defface nxml-namespace-attribute-prefix-face
  '((t (:inherit nxml-name-face)))
  "Face used for the prefix declared in namespace attributes."
  :group 'nxml-highlighting-faces)

(defface nxml-attribute-value-face
  '((t (:inherit nxml-delimited-data-face)))
  "Face used for the value of attributes."
  :group 'nxml-highlighting-faces)

(defface nxml-attribute-value-delimiter-face
  '((t (:inherit nxml-delimiter-face)))
  "Face used for the delimiters of attribute values."
  :group 'nxml-highlighting-faces)

(defface nxml-namespace-attribute-value-face
  '((t (:inherit nxml-attribute-value-face)))
  "Face used for the value of namespace attributes."
  :group 'nxml-highlighting-faces)

(defface nxml-namespace-attribute-value-delimiter-face
  '((t (:inherit nxml-attribute-value-delimiter-face)))
  "Face used for the delimiters of namespace attribute values."
  :group 'nxml-highlighting-faces)

(defface nxml-prolog-literal-delimiter-face
  '((t (:inherit nxml-delimiter-face)))
  "Face used for the delimiters of literals in the prolog."
  :group 'nxml-highlighting-faces)

(defface nxml-prolog-literal-content-face
  '((t (:inherit nxml-delimited-data-face)))
  "Face used for the content of literals in the prolog."
  :group 'nxml-highlighting-faces)

(defface nxml-prolog-keyword-face
  '((t (:inherit nxml-name-face)))
  "Face used for keywords in the prolog."
  :group 'nxml-highlighting-faces)

(defface nxml-markup-declaration-delimiter-face
  '((t (:inherit nxml-delimiter-face)))
  "Face used for the delimiters of markup declarations in the prolog.
The delimiters are <! and >."
  :group 'nxml-highlighting-faces)

(defface nxml-hash-face
  '((t (:inherit nxml-name-face)))
  "Face used for # before a name in the prolog."
  :group 'nxml-highlighting-faces)

(defface nxml-glyph-face
  '((((type x))
     (:family
      "misc-fixed"
      :background
      "light grey"
      :foreground
      "black"
      :weight
      normal 
      :slant
      normal))
    (t
     (:background
      "light grey"
      :foreground
      "black"
      :weight
      normal 
      :slant
      normal)))
  "Face used for glyph for char references."
  :group 'nxml-highlighting-faces)

;;; Global variables

;; This is initialized in rng-auto.el.
(defvar nxml-version nil
  "*The version of nxml-mode that is being used.")

(defvar nxml-prolog-regions nil
  "List of regions in the prolog to be fontified.
See the function `xmltok-forward-prolog' for more information.")
(make-variable-buffer-local 'nxml-prolog-regions)

(defvar nxml-last-fontify-end nil
  "Position where fontification last ended.
Nil if the buffer changed since the last fontification.")
(make-variable-buffer-local 'nxml-last-fontify-end)

(defvar nxml-degraded nil
  "Non-nil if currently operating in degraded mode.
Degraded mode is enabled when an internal error is encountered in the
fontification or after-change functions.")
(make-variable-buffer-local 'nxml-degraded)

(defvar nxml-completion-hook nil
  "Hook run by `nxml-complete'.
This hook is run until success.")

(defvar nxml-in-mixed-content-hook nil
  "Hook to determine whether point is in mixed content.
The hook is called without arguments.  It should return nil if it is
definitely not mixed; non-nil otherwise.  The hook will be run until
one of the functions returns nil.")

(defvar nxml-mixed-scan-distance 4000
  "Maximum distance from point to scan when checking for mixed content.")

(defvar nxml-end-tag-indent-scan-distance 4000
  "Maximum distance from point to scan backwards when indenting end-tag.")

(defvar nxml-char-ref-extra-display t
  "Non-nil means display extra information for character references.
The extra information consists of a tooltip with the character name
and, if `nxml-char-ref-display-glyph-flag' is non-nil, a glyph
corresponding to the referenced character following the character
reference.")
(make-variable-buffer-local 'nxml-char-ref-extra-display)

(defvar nxml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-\C-u" 'nxml-backward-up-element)
    (define-key map "\M-\C-d" 'nxml-down-element)
    (define-key map "\M-\C-n" 'nxml-forward-element)
    (define-key map "\M-\C-p" 'nxml-backward-element)
    (define-key map "\M-{" 'nxml-backward-paragraph)
    (define-key map "\M-}" 'nxml-forward-paragraph)
    (define-key map "\M-h" 'nxml-mark-paragraph)
    (define-key map "\C-c\C-f" 'nxml-finish-element)
    (define-key map "\C-c\C-m" 'nxml-split-element)
    (define-key map "\C-c\C-b" 'nxml-balanced-close-start-tag-block)
    (define-key map "\C-c\C-i" 'nxml-balanced-close-start-tag-inline)
    (define-key map "\C-c\C-x" 'nxml-insert-xml-declaration)
    (define-key map "\C-c\C-d" 'nxml-dynamic-markup-word)
    ;; u is for Unicode
    (define-key map "\C-c\C-u" 'nxml-insert-named-char)
    (define-key map "\C-c\C-o" nxml-outline-prefix-map)
    (define-key map [S-mouse-2] 'nxml-mouse-hide-direct-text-content)
    (define-key map "/" 'nxml-electric-slash)
    (define-key map [C-return] 'nxml-complete) 
    (when nxml-bind-meta-tab-to-complete-flag
      (define-key map "\M-\t" 'nxml-complete))
    map)
  "Keymap for nxml-mode.")

(defsubst nxml-set-face (start end face)
  (when (and face (< start end))
    (put-text-property start end 'face face)))

(defun nxml-clear-face (start end)
  (remove-text-properties start end '(face nil))
  (nxml-clear-char-ref-extra-display start end))

(defsubst nxml-set-fontified (start end)
  (put-text-property start end 'fontified t))

(defsubst nxml-clear-fontified (start end)
  (remove-text-properties start end '(fontified nil)))

;;;###autoload
(defun nxml-mode ()
  ;; We use C-c C-i instead of \\[nxml-balanced-close-start-tag-inline]
  ;; because Emacs turns C-c C-i into C-c TAB which is hard to type and
  ;; not mnemonic.
  "Major mode for editing XML.

Syntax highlighting is performed unless the variable
`nxml-syntax-highlight-flag' is nil.

\\[nxml-finish-element] finishes the current element by inserting an end-tag.
C-c C-i closes a start-tag with `>' and then inserts a balancing end-tag
leaving point between the start-tag and end-tag. 
\\[nxml-balanced-close-start-tag-block] is similar but for block rather than inline elements:
the start-tag, point, and end-tag are all left on separate lines.
If `nxml-slash-auto-complete-flag' is non-nil, then inserting a `</'
automatically inserts the rest of the end-tag.

\\[nxml-complete] performs completion on the symbol preceding point.

\\[nxml-dynamic-markup-word] uses the contents of the current buffer
to choose a tag to put around the word preceding point.

Sections of the document can be displayed in outline form.  The
variable `nxml-section-element-name-regexp' controls when an element
is recognized as a section.  The same key sequences that change
visibility in outline mode are used except that they start with C-c C-o
instead of C-c.

Validation is provided by the related minor-mode `rng-validate-mode'.
This also makes completion schema- and context- sensitive.  Element
names, attribute names, attribute values and namespace URIs can all be
completed. By default, `rng-validate-mode' is automatically enabled by
`rng-nxml-mode-init' which is normally added to `nxml-mode-hook'. You
can toggle it using \\[rng-validate-mode].

\\[indent-for-tab-command] indents the current line appropriately.
This can be customized using the variable `nxml-child-indent'
and the variable `nxml-attribute-indent'.

\\[nxml-insert-named-char] inserts a character reference using
the character's name (by default, the Unicode name). \\[universal-argument] \\[nxml-insert-named-char]
inserts the character directly.

The Emacs commands that normally operate on balanced expressions will
operate on XML markup items.  Thus \\[forward-sexp] will move forward
across one markup item; \\[backward-sexp] will move backward across
one markup item; \\[kill-sexp] will kill the following markup item;
\\[mark-sexp] will mark the following markup item.  By default, each
tag each treated as a single markup item; to make the complete element
be treated as a single markup item, set the variable
`nxml-sexp-element-flag' to t.  For more details, see the function
`nxml-forward-balanced-item'.

\\[nxml-backward-up-element] and \\[nxml-down-element] move up and down the element structure.

Many aspects this mode can be customized using
\\[customize-group] nxml RET."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'nxml-mode)
  (setq mode-name "nXML")
  ;; We'll determine the fill prefix ourselves
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil)
  (make-local-variable 'forward-sexp-function)
  (setq forward-sexp-function 'nxml-forward-balanced-item)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'nxml-indent-line)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'nxml-do-fill-paragraph)
  ;; Comment support
  ;; This doesn't seem to work too well;
  ;; I think we should probably roll our own nxml-comment-dwim function.
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'nxml-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "<!--")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "<!--[ \t\r\n]*")
  (make-local-variable 'comment-end)
  (setq comment-end "-->")
  (make-local-variable 'comment-end-skip)
  (setq comment-end-skip "[ \t\r\n]*-->")
  (make-local-variable 'comment-line-break-function)
  (setq comment-line-break-function 'nxml-newline-and-indent)
  (use-local-map nxml-mode-map)
  (save-excursion
    (save-restriction
      (widen)
      (nxml-clear-dependent-regions (point-min) (point-max))
      (setq nxml-scan-end (copy-marker (point-min) nil))
      (nxml-with-unmodifying-text-property-changes
	(when nxml-syntax-highlight-flag
	  (nxml-clear-fontified (point-min) (point-max)))
	(nxml-clear-inside (point-min) (point-max))
	(nxml-with-invisible-motion
	  (nxml-scan-prolog)))))
  (when nxml-syntax-highlight-flag
    (add-hook 'fontification-functions 'nxml-fontify nil t))
  (add-hook 'after-change-functions 'nxml-after-change nil t)
  (add-hook 'write-contents-hooks 'nxml-prepare-to-save)
  (when (not (and (buffer-file-name) (file-exists-p (buffer-file-name))))
    (when (and nxml-default-buffer-file-coding-system
	       (not (local-variable-p 'buffer-file-coding-system)))
      (setq buffer-file-coding-system nxml-default-buffer-file-coding-system))
    (when nxml-auto-insert-xml-declaration-flag
      (nxml-insert-xml-declaration)))
  (run-hooks 'nxml-mode-hook))

(defun nxml-degrade (context err)
  (message "Internal nXML mode error in %s (%s), degrading"
	   context
	   (error-message-string err))
  (ding)
  (setq nxml-degraded t)
  (setq nxml-prolog-end 1)
  (save-excursion
    (save-restriction
      (widen)
      (nxml-with-unmodifying-text-property-changes
	(nxml-clear-face (point-min) (point-max))
	(nxml-set-fontified (point-min) (point-max))
	(nxml-clear-inside (point-min) (point-max)))
      (setq mode-name "nXML/degraded"))))

;;; Change management

(defun nxml-after-change (start end pre-change-length)
  ;; Work around bug in insert-file-contents.
  (when (> end (1+ (buffer-size)))
    (setq start 1)
    (setq end (1+ (buffer-size))))
  (unless nxml-degraded
    (condition-case err
	(save-excursion
	  (save-restriction
	    (widen)
	    (save-match-data
	      (nxml-with-invisible-motion
		(nxml-with-unmodifying-text-property-changes
		  (nxml-after-change1 start end pre-change-length))))))
      (error
       (nxml-degrade 'nxml-after-change err)))))

(defun nxml-after-change1 (start end pre-change-length)
  (setq nxml-last-fontify-end nil)
  (let ((pre-change-end (+ start pre-change-length)))
    (setq start
	  (nxml-adjust-start-for-dependent-regions start
						   end
						   pre-change-length))
    (when (<= start
	      ;; Add 2 so as to include the < and following char
	      ;; that start the instance, since changing these
	      ;; can change where the prolog ends.
	      (+ nxml-prolog-end 2))
      ;; end must be extended to at least the end of the old prolog
      (when (< pre-change-end nxml-prolog-end)
	(setq end
	      ;; don't let end get out of range even if pre-change-length
	      ;; is bogus
	      (min (point-max)
		   (+ end (- nxml-prolog-end pre-change-end)))))
      (nxml-scan-prolog)))
  (cond ((<= end nxml-prolog-end)
	 (setq end nxml-prolog-end)
	 (goto-char start)
	 ;; This is so that Emacs redisplay works
	 (setq start (line-beginning-position)))
	((and (<= start nxml-scan-end)
	      (> start (point-min))
	      (nxml-get-inside (1- start)))
	 ;; The closing delimiter might have been removed.
	 ;; So we may need to redisplay from the beginning
	 ;; of the token.
	 (goto-char (1- start))
	 (nxml-move-outside-backwards)
	 ;; This is so that Emacs redisplay works
	 (setq start (line-beginning-position))
	 (setq end (max (nxml-scan-after-change (point) end)
			end)))
	(t
	 (goto-char start)
	 ;; This is both for redisplay and to move back
	 ;; past any incomplete opening delimiters
	 (setq start (line-beginning-position))
	 (setq end (max (nxml-scan-after-change start end)
			end))))
  (when nxml-syntax-highlight-flag
    (when (>= start end)
      ;; Must clear at least one char so as to trigger redisplay.
      (cond ((< start (point-max))
	     (setq end (1+ start)))
	    (t
	     (setq end (point-max))
	     (goto-char end)
	     (setq start (line-beginning-position)))))
    (nxml-clear-fontified start end)))
  
;;; Encodings

(defun nxml-insert-xml-declaration ()
  "Insert an XML declaration at the beginning of buffer.
The XML declaration will declare an encoding depending on the buffer's
`buffer-file-coding-system'."
  (interactive "*")
  (let ((coding-system
	 (if (and buffer-file-coding-system
		  (coding-system-p buffer-file-coding-system)
		  (coding-system-get buffer-file-coding-system
				     'mime-charset))
	     buffer-file-coding-system
	   (nxml-choose-utf-coding-system))))
    (goto-char (point-min))
    (insert (format "<?xml version=\"1.0\" encoding=\"%s\"?>\n"
		    (nxml-coding-system-name coding-system)))))

(defun nxml-prepare-to-save ()
  (unless (and (not enable-multibyte-characters)
	       (local-variable-p 'buffer-file-coding-system)
	       buffer-file-coding-system
	       (or (eq (coding-system-type buffer-file-coding-system) 5)
		   (eq buffer-file-coding-system 'no-conversion)))
    (save-excursion
      (setq buffer-file-coding-system (nxml-select-coding-system))))
  ;; nil from a function in `write-contents-hooks' means
  ;; to continue and write the file as normal
  nil)

(defun nxml-select-coding-system ()
  (let* ((suitable-coding-systems
	  (find-coding-systems-region (point-min) (point-max)))
	 (enc-pos (progn
		    (goto-char (point-min))
		    (xmltok-get-declared-encoding-position)))
	 (enc-name
	  (and (consp enc-pos)
	       (buffer-substring-no-properties (car enc-pos)
					       (cdr enc-pos))))
	 (coding-system
	  (cond (enc-name
		 (if (string= (downcase enc-name) "utf-16")
		     (nxml-choose-utf-16-coding-system)
		   (nxml-mime-charset-coding-system enc-name)))
		(enc-pos (nxml-choose-utf-coding-system)))))
    ;; Make sure we have a coding-system
    (unless coding-system
      (setq coding-system
	    (and (not buffer-read-only)
		 (nxml-choose-suitable-coding-system
		  suitable-coding-systems)))
      (let ((message
	     (if enc-name
		 (format "Unknown encoding %s" enc-name)
	       "XML declaration is not well-formed")))
	(cond ((not coding-system)
	       (error "%s" message))
	      ((y-or-n-p
		(concat message
			". "
			(format (if enc-name
				    "Save with %s"
				  "Modify and save with encoding %s")
				(nxml-coding-system-name coding-system))
			" "))
	       (nxml-fix-encoding-declaration enc-pos coding-system))
	      (t (signal 'quit nil)))))
    ;; Make sure it can encode all the characters in the buffer
    (unless (or (memq (coding-system-base coding-system)
		      suitable-coding-systems)
		(equal suitable-coding-systems '(undecided)))
      (let ((message
	     (nxml-unsuitable-coding-system-message coding-system
						    enc-name)))
	(setq coding-system
	      (and (not buffer-read-only)
		   (nxml-choose-suitable-coding-system
		    suitable-coding-systems)))
	(cond ((not coding-system) (error "%s" message))
	      ((y-or-n-p (concat message
				 (format ". Save with %s "
					 (nxml-coding-system-name
					  coding-system))))
	       (nxml-fix-encoding-declaration enc-pos coding-system))
	      (t (signal 'quit nil)))))
    ;; Merge the newline type of our existing encoding
    (let ((current-eol-type
	   (coding-system-eol-type buffer-file-coding-system)))
      (when (and current-eol-type (integerp current-eol-type))
	(setq coding-system
	      (coding-system-change-eol-conversion coding-system
						   current-eol-type))))
    coding-system))

(defun nxml-unsuitable-coding-system-message (coding-system &optional enc-name)
  (if (nxml-coding-system-unicode-p coding-system)
      "Cannot translate some characters to Unicode"
    (format "Cannot encode some characters with %s"
	    (or enc-name
		(nxml-coding-system-name coding-system)))))

(defconst nxml-utf-16-coding-systems (and (coding-system-p 'utf-16-be)
					  (coding-system-p 'utf-16-le)
					  '(utf-16-be utf-16-le)))

(defconst nxml-utf-coding-systems (cons 'utf-8 nxml-utf-16-coding-systems))

(defun nxml-coding-system-unicode-p (coding-system)
  (nxml-coding-system-member (coding-system-base coding-system)
			     nxml-utf-coding-systems))

(defun nxml-coding-system-name (coding-system)
  (setq coding-system (coding-system-base coding-system))
  (symbol-name
   (if (nxml-coding-system-member coding-system nxml-utf-16-coding-systems)
       'utf-16
     (or (coding-system-get coding-system 'mime-charset)
	 coding-system))))

(defun nxml-fix-encoding-declaration (enc-pos coding-system)
  (let ((charset (nxml-coding-system-name coding-system)))
    (cond ((consp enc-pos)
	   (delete-region (car enc-pos) (cdr enc-pos))
	   (goto-char (car enc-pos))
	   (insert charset))
	  ((integerp enc-pos)
	   (goto-char enc-pos)
	   (insert " encoding=\"" charset ?\"))
	  (t
	   (goto-char (point-min))
	   (insert "<?xml version=\"1.0\" encoding=\""
		   charset
		   "\"?>\n")
	   (when (and (not enc-pos)
		      (let ((case-fold-search t))
			(looking-at xmltok-bad-xml-decl-regexp)))
	     (delete-region (point) (match-end 0)))))))

(defun nxml-choose-suitable-coding-system (suitable-coding-systems)
  (let (ret coding-system)
    (if (and buffer-file-coding-system
	     (memq (coding-system-base buffer-file-coding-system)
		   suitable-coding-systems))
	buffer-file-coding-system
      (while (and suitable-coding-systems (not ret))
	(setq coding-system (car suitable-coding-systems))
	(if (coding-system-get coding-system 'mime-charset)
	    (setq ret coding-system)
	  (setq suitable-coding-systems (cdr suitable-coding-systems))))
      ret)))

(defun nxml-choose-utf-coding-system ()	     
  (let ((cur (and (local-variable-p 'buffer-file-coding-system)
		  buffer-file-coding-system
		  (coding-system-base buffer-file-coding-system))))
    (cond ((car (nxml-coding-system-member cur nxml-utf-coding-systems)))
	  ((and nxml-prefer-utf-16-to-utf-8-flag
		(coding-system-p 'utf-16-le)
		(coding-system-p 'utf-16-be))
	   (if nxml-prefer-utf-16-little-to-big-endian-flag
	       'utf-16-le
	     'utf-16-be))
	  (t 'utf-8))))

(defun nxml-choose-utf-16-coding-system ()
  (let ((cur (and (local-variable-p 'buffer-file-coding-system)
		  buffer-file-coding-system
		  (coding-system-base buffer-file-coding-system))))
    (cond ((car (nxml-coding-system-member cur nxml-utf-16-coding-systems)))
	  (nxml-prefer-utf-16-little-to-big-endian-flag
	   (and (coding-system-p 'utf-16-le) 'utf-16-le))
	  (t (and (coding-system-p 'utf-16-be) 'utf-16-be)))))

(defun nxml-coding-system-member (coding-system coding-systems)
  (let (ret)
    (while (and coding-systems (not ret))
      (if (coding-system-equal coding-system
			       (car coding-systems))
	  (setq ret coding-systems)
	(setq coding-systems (cdr coding-systems))))
    ret))

;;; Fontification

(defun nxml-fontify (start)
  (condition-case err
      (save-excursion
	(save-restriction
	  (widen)
	  (save-match-data
	    (nxml-with-invisible-motion
	      (nxml-with-unmodifying-text-property-changes
		(if (or nxml-degraded
			;; just in case we get called in the wrong buffer
			(not nxml-prolog-end))
		    (nxml-set-fontified start (point-max))
		  (nxml-fontify1 start)))))))
    (error
     (nxml-degrade 'nxml-fontify err))))

(defun nxml-fontify1 (start)
  (cond ((< start nxml-prolog-end)
	 (nxml-fontify-prolog)
	 (nxml-set-fontified (point-min)
			     nxml-prolog-end))
	(t
	 (goto-char start)
	 (when (not (eq nxml-last-fontify-end start))
	   (when (not (equal (char-after) ?\<))
	     (search-backward "<" nxml-prolog-end t))
	   (nxml-ensure-scan-up-to-date)
	   (nxml-move-outside-backwards))
	 (let ((start (point)))
	   (nxml-do-fontify (min (point-max)
				 (+ start nxml-fontify-chunk-size)))
	   (setq nxml-last-fontify-end (point))
	   (nxml-set-fontified start nxml-last-fontify-end)))))

(defun nxml-fontify-buffer ()
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (nxml-with-invisible-motion
	(goto-char (point-min))
	(nxml-with-unmodifying-text-property-changes
	  (nxml-fontify-prolog)
	  (goto-char nxml-prolog-end)
	  (nxml-do-fontify))))))

(defun nxml-fontify-prolog ()
  "Fontify the prolog.
The buffer is assumed to be prepared for fontification.
This does not set the fontified property, but it does clear
faces appropriately."
  (let ((regions nxml-prolog-regions))
    (nxml-clear-face (point-min) nxml-prolog-end)
    (while regions
      (let ((region (car regions)))
	(nxml-apply-fontify-rule (aref region 0)
				 (aref region 1)
				 (aref region 2)))
      (setq regions (cdr regions)))))

(defun nxml-do-fontify (&optional bound)
  "Fontify at least as far as bound.
Leave point after last fontified position."
  (unless bound (setq bound (point-max)))
  (let (xmltok-dependent-regions
	xmltok-errors)
    (while (and (< (point) bound)
		(nxml-tokenize-forward))
      (nxml-clear-face xmltok-start (point))
      (nxml-apply-fontify-rule))))

;; Vectors identify a substring of the token to be highlighted in some face.

;; Token types returned by xmltok-forward.

(put 'start-tag
     'nxml-fontify-rule
     '([nil 1 nxml-tag-delimiter-face]
       [-1 nil nxml-tag-delimiter-face]
       (element-qname . 1)
       attributes))

(put 'partial-start-tag
     'nxml-fontify-rule
     '([nil 1 nxml-tag-delimiter-face]
       (element-qname . 1)
       attributes))

(put 'end-tag
     'nxml-fontify-rule
     '([nil 1 nxml-tag-delimiter-face]
       [1 2 nxml-tag-slash-face]
       [-1 nil nxml-tag-delimiter-face]
       (element-qname . 2)))

(put 'partial-end-tag
     'nxml-fontify-rule
     '([nil 1 nxml-tag-delimiter-face]
       [1 2 nxml-tag-slash-face]
       (element-qname . 2)))

(put 'empty-element
     'nxml-fontify-rule
     '([nil 1 nxml-tag-delimiter-face]
       [-2 -1 nxml-tag-slash-face]
       [-1 nil nxml-tag-delimiter-face]
       (element-qname . 1)
       attributes))

(put 'partial-empty-element
     'nxml-fontify-rule
     '([nil 1 nxml-tag-delimiter-face]
       [-1 nil nxml-tag-slash-face]
       (element-qname . 1)
       attributes))

(put 'char-ref
     'nxml-fontify-rule
     '([nil 2 nxml-char-ref-delimiter-face]
       [2 -1 nxml-char-ref-number-face]
       [-1 nil nxml-char-ref-delimiter-face]
       char-ref))

(put 'entity-ref
     'nxml-fontify-rule
     '([nil 1 nxml-entity-ref-delimiter-face]
       [1 -1 nxml-entity-ref-name-face]
       [-1 nil nxml-entity-ref-delimiter-face]))

(put 'comment
     'nxml-fontify-rule
     '([nil 4 nxml-comment-delimiter-face]
       [4 -3 nxml-comment-content-face]
       [-3 nil nxml-comment-delimiter-face]))

(put 'processing-instruction
     'nxml-fontify-rule
     '([nil 2 nxml-processing-instruction-delimiter-face]
       [-2 nil nxml-processing-instruction-delimiter-face]
       processing-instruction-content))

(put 'cdata-section
     'nxml-fontify-rule
     '([nil 3 nxml-cdata-section-delimiter-face] ; <![
       [3 8 nxml-cdata-section-CDATA-face] ; CDATA
       [8 9 nxml-cdata-section-delimiter-face] ; [
       [9 -3 nxml-cdata-section-content-face] ; ]]>
       [-3 nil nxml-cdata-section-delimiter-face]))

(put 'data
     'nxml-fontify-rule
     '([nil nil nxml-text-face]))

;; Prolog region types in list returned by xmltok-forward-prolog.

(put 'xml-declaration
     'nxml-fontify-rule
     '([nil 2 nxml-processing-instruction-delimiter-face]
       [2 5 nxml-processing-instruction-target-face]
       [-2 nil nxml-processing-instruction-delimiter-face]))

(put 'xml-declaration-attribute-name
     'nxml-fontify-rule
     '([nil nil nxml-attribute-local-name-face]))

(put 'xml-declaration-attribute-value
     'nxml-fontify-rule
     '([nil 1 nxml-attribute-value-delimiter-face]
       [1 -1 nxml-attribute-value-face]
       [-1 nil nxml-attribute-value-delimiter-face]))

(put 'processing-instruction-left
     'nxml-fontify-rule
     '([nil 2 nxml-processing-instruction-delimiter-face]
       [2 nil nxml-processing-instruction-target-face]))

(put 'processing-instruction-right
     'nxml-fontify-rule
     '([nil -2 nxml-processing-instruction-content-face]
       [-2 nil nxml-processing-instruction-delimiter-face]))

(put 'literal
     'nxml-fontify-rule
     '([nil 1 nxml-prolog-literal-delimiter-face]
       [1 -1 nxml-prolog-literal-content-face]
       [-1 nil nxml-prolog-literal-delimiter-face]))

(put 'keyword
     'nxml-fontify-rule
     '([nil nil nxml-prolog-keyword-face]))

(put 'markup-declaration-open
     'nxml-fontify-rule
     '([0 2 nxml-markup-declaration-delimiter-face]
       [2 nil nxml-prolog-keyword-face]))

(put 'markup-declaration-close
     'nxml-fontify-rule
     '([nil nil nxml-markup-declaration-delimiter-face]))

(put 'internal-subset-open
     'nxml-fontify-rule
     '([nil nil nxml-markup-declaration-delimiter-face]))

(put 'internal-subset-close
     'nxml-fontify-rule
     '([nil 1 nxml-markup-declaration-delimiter-face]
       [-1 nil nxml-markup-declaration-delimiter-face]))

(put 'hash-name
     'nxml-fontify-rule
     '([nil 1 nxml-hash-face]
       [1 nil nxml-prolog-keyword-face]))

(defun nxml-apply-fontify-rule (&optional type start end)
  (let ((rule (get (or type xmltok-type) 'nxml-fontify-rule)))
    (unless start (setq start xmltok-start))
    (unless end (setq end (point)))
    (while rule
      (let* ((action (car rule)))
	(setq rule (cdr rule))
	(cond ((vectorp action)
	       (nxml-set-face (let ((offset (aref action 0)))
				(cond ((not offset) start)
				      ((< offset 0) (+ end offset))
				      (t (+ start offset))))
			      (let ((offset (aref action 1)))
				(cond ((not offset) end)
				      ((< offset 0) (+ end offset))
				      (t (+ start offset))))
			      (aref action 2)))
	      ((and (consp action)
		    (eq (car action) 'element-qname))
	       (when xmltok-name-end ; maybe nil in partial-end-tag case
		 (nxml-fontify-qname (+ start (cdr action))
				     xmltok-name-colon
				     xmltok-name-end
				     'nxml-element-prefix-face
				     'nxml-element-colon-face
				     'nxml-element-local-name-face)))
	      ((eq action 'attributes)
	       (nxml-fontify-attributes))
	      ((eq action 'processing-instruction-content)
	       (nxml-set-face (+ start 2)
			      xmltok-name-end
			      'nxml-processing-instruction-target-face)
	       (nxml-set-face (save-excursion
				(goto-char xmltok-name-end)
				(skip-chars-forward " \t\r\n")
				(point))
			      (- end 2)
			      'nxml-processing-instruction-content-face))
	      ((eq action 'char-ref)
	       (nxml-char-ref-display-extra start
					    end
					    (xmltok-char-number start end)))
	      (t (error "Invalid nxml-fontify-rule action %s" action)))))))

(defun nxml-fontify-attributes ()
  (while xmltok-namespace-attributes
    (nxml-fontify-attribute (car xmltok-namespace-attributes)
			    'namespace)
    (setq xmltok-namespace-attributes
	  (cdr xmltok-namespace-attributes)))
  (while xmltok-attributes
    (nxml-fontify-attribute (car xmltok-attributes))
    (setq xmltok-attributes
	  (cdr xmltok-attributes))))

(defun nxml-fontify-attribute (att &optional namespace-declaration)
  (if namespace-declaration
      (nxml-fontify-qname (xmltok-attribute-name-start att)
			  (xmltok-attribute-name-colon att)
			  (xmltok-attribute-name-end att)
			  'nxml-namespace-attribute-xmlns-face
			  'nxml-namespace-attribute-colon-face
			  'nxml-namespace-attribute-prefix-face
			  'nxml-namespace-attribute-xmlns-face)
    (nxml-fontify-qname (xmltok-attribute-name-start att)
			(xmltok-attribute-name-colon att)
			(xmltok-attribute-name-end att)
			'nxml-attribute-prefix-face
			'nxml-attribute-colon-face
			'nxml-attribute-local-name-face))
  (let ((start (xmltok-attribute-value-start att))
	(end (xmltok-attribute-value-end att))
	(refs (xmltok-attribute-refs att))
	(delimiter-face (if namespace-declaration
			    'nxml-namespace-attribute-value-delimiter-face
			  'nxml-attribute-value-delimiter-face))
	(value-face (if namespace-declaration
			'nxml-namespace-attribute-value-face
		      'nxml-attribute-value-face)))
    (when start
      (nxml-set-face (1- start) start delimiter-face)
      (nxml-set-face end (1+ end) delimiter-face)
      (while refs
	(let* ((ref (car refs))
	       (ref-type (aref ref 0))
	       (ref-start (aref ref 1))
	       (ref-end (aref ref 2)))
	  (nxml-set-face start ref-start value-face)
	  (nxml-apply-fontify-rule ref-type ref-start ref-end)
	  (setq start ref-end))
	(setq refs (cdr refs)))
      (nxml-set-face start end value-face))))

(defun nxml-fontify-qname (start
			   colon
			   end
			   prefix-face
			   colon-face
			   local-name-face
			   &optional
			   unprefixed-face)
  (cond (colon (nxml-set-face start colon prefix-face)
	       (nxml-set-face colon (1+ colon) colon-face)
	       (nxml-set-face (1+ colon) end local-name-face))
	(t (nxml-set-face start end (or unprefixed-face
					local-name-face)))))

;;; Editing

(defun nxml-electric-slash (arg)
  "Insert a slash.

With a prefix ARG, do nothing other than insert the slash.

Otherwise, if `nxml-slash-auto-complete-flag' is non-nil, insert the
rest of the end-tag or empty-element if the slash is potentially part
of an end-tag or the close of an empty-element.

If the slash is part of an end-tag that is the first non-whitespace
on the line, reindent the line."
  (interactive "*P")
  (nxml-ensure-scan-up-to-date)
  (let* ((slash-pos (point))
	 (end-tag-p (and (eq (char-before slash-pos) ?<)
			 (not (nxml-get-inside slash-pos))))
	 (at-indentation (save-excursion
			   (back-to-indentation)
			   (eq (point) (1- slash-pos)))))
    (self-insert-command (prefix-numeric-value arg))
    (unless arg
      (if nxml-slash-auto-complete-flag
	  (if end-tag-p
	      (condition-case err
		  (let ((start-tag-end
			 (nxml-scan-element-backward (1- slash-pos) t)))
		    (when start-tag-end
		      (insert (xmltok-start-tag-qname) ">")
		      ;; copy the indentation of the start-tag
		      (when (and at-indentation
				 (save-excursion
				   (goto-char xmltok-start)
				   (back-to-indentation)
				   (eq (point) xmltok-start)))
			(save-excursion
			  (indent-line-to (save-excursion
					    (goto-char xmltok-start)
					    (current-column)))))))
		(nxml-scan-error nil))
	    (when (and (eq (nxml-token-before) (point))
		       (eq xmltok-type 'partial-empty-element))
	      (insert ">")))
	(when (and end-tag-p at-indentation)
	  (nxml-indent-line))))))

(defun nxml-balanced-close-start-tag-block ()
  "Close the start-tag before point with `>' and insert a balancing end-tag.
Point is left between the start-tag and the end-tag.
If there is nothing but whitespace before the `<' that opens the
start-tag, then put point on a blank line, and put the end-tag on
another line aligned with the start-tag."
  (interactive "*")
  (nxml-balanced-close-start-tag 'block))

(defun nxml-balanced-close-start-tag-inline ()
  "Close the start-tag before point with `>' and insert a balancing end-tag.
Point is left between the start-tag and the end-tag.
No extra whitespace is inserted."
  (interactive "*")
  (nxml-balanced-close-start-tag 'inline))

(defun nxml-balanced-close-start-tag (block-or-inline)
  (let ((token-end (nxml-token-before))
	(pos (1+ (point))))
    (unless (or (eq xmltok-type 'partial-start-tag)
		(and (memq xmltok-type '(start-tag
					 empty-element
					 partial-empty-element))
		     (>= token-end pos)))
      (error "Not in a start-tag"))
    (insert "></"
	    (buffer-substring-no-properties (+ xmltok-start 1)
					    (min xmltok-name-end (point)))
	    ">")
    (if (eq block-or-inline 'inline)
	(goto-char pos)
      (goto-char xmltok-start)
      (back-to-indentation)
      (if (= (point) xmltok-start)
	  (let ((indent (current-column)))
	  (goto-char pos)
	  (insert "\n")
	  (indent-line-to indent)
	  (goto-char pos)
	  (insert "\n")
	  (indent-line-to (+ nxml-child-indent indent)))
	(goto-char pos)))))
	
(defun nxml-finish-element ()
  "Finish the current element by inserting an end-tag."
  (interactive "*")
  (nxml-finish-element-1 nil))

(defvar nxml-last-split-position nil
  "Position where `nxml-split-element' split the current element.")

(defun nxml-split-element ()
  "Split the current element by inserting an end-tag and a start-tag.
Point is left after the newly inserted start-tag.  When repeated,
split immediately before the previously inserted start-tag and leave
point unchanged."
  (interactive "*")
  (setq nxml-last-split-position
	(if (and (eq last-command this-command)
		 nxml-last-split-position)
	    (save-excursion
	      (goto-char nxml-last-split-position)
	      (nxml-finish-element-1 t))
	  (nxml-finish-element-1 t))))

(defun nxml-finish-element-1 (startp)
  "Insert an end-tag for the current element and optionally a start-tag.
The start-tag is inserted if STARTP is non-nil.  Return the position
of the inserted start-tag or nil if none was inserted."
  (interactive "*")
  (let* ((token-end (nxml-token-before))
	 (start-tag-end
	  (save-excursion
	    (when (and (< (point) token-end)
		       (memq xmltok-type
			     '(cdata-section
			       processing-instruction
			       comment
			       start-tag
			       end-tag
			       empty-element)))
	      (error "Point is inside a %s"
		     (nxml-token-type-friendly-name xmltok-type)))
	    (nxml-scan-element-backward token-end t)))
	 (starts-line
	  (save-excursion
	    (unless (eq xmltok-type 'start-tag)
	      (error "No matching start-tag"))
	    (goto-char xmltok-start)
	    (back-to-indentation)
	    (eq (point) xmltok-start)))
	 (ends-line
	  (save-excursion
	    (goto-char start-tag-end)
	    (looking-at "[ \t\r\n]*$")))
	 (start-tag-indent (save-excursion
			     (goto-char xmltok-start)
			     (current-column)))
	 (qname (xmltok-start-tag-qname))
	 inserted-start-tag-pos)
    (when (and starts-line ends-line)
      ;; start-tag is on a line by itself
      ;; => put the end-tag on a line by itself
      (unless (<= (point)
		  (save-excursion
		    (back-to-indentation)
		    (point)))
	(insert "\n"))
      (indent-line-to start-tag-indent))
    (insert "</" qname ">")
    (when startp
      (when starts-line
	(insert "\n")
	(indent-line-to start-tag-indent))
      (setq inserted-start-tag-pos (point))
      (insert "<" qname ">")
      (when (and starts-line ends-line)
	(insert "\n")
	(indent-line-to (save-excursion
			  (goto-char xmltok-start)
			  (forward-line 1)
			  (back-to-indentation)
			  (if (= (current-column)
				 (+ start-tag-indent nxml-child-indent))
			      (+ start-tag-indent nxml-child-indent)
			    start-tag-indent)))))
    inserted-start-tag-pos))

;;; Indentation

(defun nxml-indent-line ()
  "Indent current line as XML."
  (let ((indent (nxml-compute-indent))
	(from-end (- (point-max) (point))))
    (when indent
      (beginning-of-line)
      (let ((bol (point)))
	(skip-chars-forward " \t")
	(delete-region bol (point)))
      (indent-to indent)
      (when (> (- (point-max) from-end) (point))
	(goto-char (- (point-max) from-end))))))

(defun nxml-compute-indent ()
  "Return the indent for the line containing point."
  (or (nxml-compute-indent-from-matching-start-tag)
      (nxml-compute-indent-from-previous-line)))

(defun nxml-compute-indent-from-matching-start-tag ()
  "Compute the indent for a line with an end-tag using the matching start-tag.
When the line containing point ends with an end-tag and does not start
in the middle of a token, return the indent of the line containing the
matching start-tag, if there is one and it occurs at the beginning of
its line.  Otherwise return nil."
  (save-excursion
    (back-to-indentation)
    (let ((bol (point)))
      (let ((inhibit-field-text-motion t))
	(end-of-line))
      (skip-chars-backward " \t")
      (and (= (nxml-token-before) (point))
	   (memq xmltok-type '(end-tag partial-end-tag))
	   ;; start of line must not be inside a token
	   (or (= xmltok-start bol)
	       (save-excursion
		 (goto-char bol)
		 (nxml-token-after)
		 (= xmltok-start bol))
	       (eq xmltok-type 'data))
	   (condition-case err
	       (nxml-scan-element-backward
		(point)
		nil
		(- (point)
		   nxml-end-tag-indent-scan-distance))
	     (nxml-scan-error nil))
	   (< xmltok-start bol)
	   (progn
	     (goto-char xmltok-start)
	     (skip-chars-backward " \t")
	     (bolp))
	   (current-indentation)))))

(defun nxml-compute-indent-from-previous-line ()
  "Compute the indent for a line using the indentation of a previous line."
  (save-excursion
    (end-of-line)
    (let ((eol (point))
	  bol prev-bol ref
	  before-context after-context)
      (back-to-indentation)
      (setq bol (point))
      (catch 'indent
	;; Move backwards until the start of a non-blank line that is
	;; not inside a token.
	(while (progn
		 (when (= (forward-line -1) -1)
		   (throw 'indent 0))
		 (back-to-indentation)
		 (if (looking-at "[ \t]*$")
		     t
		   (or prev-bol
		       (setq prev-bol (point)))
		   (nxml-token-after)
		   (not (or (= xmltok-start (point))
			    (eq xmltok-type 'data))))))
	(setq ref (point))
	;; Now scan over tokens until the end of the line to be indented.
	;; Determine the context before and after the beginning of the
	;; line.
	(while (< (point) eol)
	  (nxml-tokenize-forward)
	  (cond ((<= bol xmltok-start)
		 (setq after-context
		       (nxml-merge-indent-context-type after-context)))
		((and (<= (point) bol)
		      (not (and (eq xmltok-type 'partial-start-tag)
				(= (point) bol))))
		 (setq before-context
		       (nxml-merge-indent-context-type before-context)))
		((eq xmltok-type 'data)
		 (setq before-context
		       (nxml-merge-indent-context-type before-context))
		 (setq after-context
		       (nxml-merge-indent-context-type after-context)))
		;; If in the middle of a token that looks inline,
		;; then indent relative to the previous non-blank line
		((eq (nxml-merge-indent-context-type before-context)
		     'mixed)
		 (goto-char prev-bol)
		 (throw 'indent (current-column)))
		(t
		 (throw 'indent
			(nxml-compute-indent-in-token bol))))
	  (skip-chars-forward " \t\r\n"))
	(goto-char ref)
	(+ (current-column)
	   (* nxml-child-indent
	      (+ (if (eq before-context 'start-tag) 1 0)
		 (if (eq after-context 'end-tag) -1 0))))))))

(defun nxml-merge-indent-context-type (context)
  "Merge the indent context type CONTEXT with the token in `xmltok-type'.
Return the merged indent context type.  An indent context type is
either nil or one of the symbols start-tag, end-tag, markup, comment,
mixed."
  (cond ((memq xmltok-type '(start-tag partial-start-tag))
	 (if (memq context '(nil start-tag comment))
	     'start-tag
	   'mixed))
	((memq xmltok-type '(end-tag partial-end-tag))
	 (if (memq context '(nil end-tag comment))
	     'end-tag
	   'mixed))
	((eq xmltok-type 'comment)
	 (cond ((memq context '(start-tag end-tag comment))
		context)
	       (context 'mixed)
	       (t 'comment)))
	(context 'mixed)
	(t 'markup)))

(defun nxml-compute-indent-in-token (pos)
  "Return the indent for a line that starts inside a token.
POS is the position of the first non-whitespace character of the line.
This expects the xmltok-* variables to be set up as by `xmltok-forward'."
  (cond ((memq xmltok-type '(start-tag
			     partial-start-tag
			     empty-element
			     partial-empty-element))
	 (nxml-compute-indent-in-start-tag pos))
	((eq xmltok-type 'comment)
	 (nxml-compute-indent-in-delimited-token pos "<!--" "-->"))
	((eq xmltok-type 'cdata-section)
	 (nxml-compute-indent-in-delimited-token pos "<![CDATA[" "]]>"))
	((eq xmltok-type 'processing-instruction)
	 (nxml-compute-indent-in-delimited-token pos "<?" "?>"))
	(t
	 (goto-char pos)
	 (if (and (= (forward-line -1) 0)
		  (< xmltok-start (point)))
	     (back-to-indentation)
	   (goto-char xmltok-start))
	 (current-column))))

(defun nxml-compute-indent-in-start-tag (pos)
  "Return the indent for a line that starts inside a start-tag.
Also for a line that starts inside an empty element.
POS is the position of the first non-whitespace character of the line.
This expects the xmltok-* variables to be set up as by `xmltok-forward'."
  (let ((value-boundary (nxml-attribute-value-boundary pos))
	(off 0))
    (if value-boundary
	;; inside an attribute value
	(let ((value-start (car value-boundary))
	      (value-end (cdr value-boundary)))
	  (goto-char pos)
	  (forward-line -1)
	  (if (< (point) value-start)
	      (goto-char value-start)
	    (back-to-indentation)))
      ;; outside an attribute value
      (goto-char pos)
      (while (and (= (forward-line -1) 0)
		  (nxml-attribute-value-boundary (point))))
      (cond ((<= (point) xmltok-start)
	     (goto-char xmltok-start)
	     (setq off nxml-attribute-indent)
	     (let ((atts (xmltok-merge-attributes)))
	       (when atts
		 (let* ((att (car atts))
			(start (xmltok-attribute-name-start att)))
		   (when (< start pos)
		     (goto-char start)
		     (setq off 0))))))
	    (t
	     (back-to-indentation))))
    (+ (current-column) off)))

(defun nxml-attribute-value-boundary (pos)
  "Return a pair (START . END) if POS is inside an attribute value.
Otherwise return nil.  START and END are the positions of the start
and end of the attribute value containing POS.  This expects the
xmltok-* variables to be set up as by `xmltok-forward'."
  (let ((atts (xmltok-merge-attributes))
	att value-start value-end value-boundary)
    (while atts
      (setq att (car atts))
      (setq value-start (xmltok-attribute-value-start att))
      (setq value-end (xmltok-attribute-value-end att))
      (cond ((and value-start (< pos value-start))
	     (setq atts nil))
	    ((and value-start value-end (<= pos value-end))
	     (setq value-boundary (cons value-start value-end))
	     (setq atts nil))
	    (t (setq atts (cdr atts)))))
    value-boundary))
	     
(defun nxml-compute-indent-in-delimited-token (pos open-delim close-delim)
  "Return the indent for a line that starts inside a token with delimiters.
OPEN-DELIM and CLOSE-DELIM are strings giving the opening and closing
delimiters.  POS is the position of the first non-whitespace character
of the line.  This expects the xmltok-* variables to be set up as by
`xmltok-forward'."
  (cond ((let ((end (+ pos (length close-delim))))
	   (and (<= end (point-max))
		(string= (buffer-substring-no-properties pos end)
			 close-delim)))
	 (goto-char xmltok-start))
	((progn
	   (goto-char pos)
	   (forward-line -1)
	   (<= (point) xmltok-start))
	 (goto-char (+ xmltok-start (length open-delim)))
	 (when (and (string= open-delim "<!--")
		    (looking-at " "))
	   (goto-char (1+ (point)))))
	(t (back-to-indentation)))
  (current-column))

;;; Completion

(defun nxml-complete ()
  "Perform completion on the symbol preceding point.

Inserts as many characters as can be completed.  However, if not even
one character can be completed, then a buffer with the possibilities
is popped up and the symbol is read from the minibuffer with
completion. If the symbol is complete, then any characters that must
follow the symbol are also inserted.

The name space used for completion and what is treated as a symbol
depends on the context.  The contexts in which completion is performed
depend on `nxml-completion-hook'."
  (interactive)
  (unless (run-hook-with-args-until-success 'nxml-completion-hook)
    ;; Eventually we will complete on entity names here.
    (ding)
    (message "Cannot complete in this context")))

;;; Movement

(defun nxml-forward-balanced-item (&optional arg)
  "Move forward across one balanced item.
With ARG, do it that many times.  Negative arg -N means
move backward across N balanced expressions.
This is the equivalent of `forward-sexp' for XML.

An element contains as items strings with no markup, tags, processing
instructions, comments, CDATA sections, entity references and
characters references. However, if the variable
`nxml-sexp-element-flag' is non-nil, then an element is treated as a
single markup item.  A start-tag contains an element name followed by
one or more attributes.  An end-tag contains just an element name.  An
attribute value literals contains strings with no markup, entity
references and character references.  A processing instruction
consists of a target and a content string.  A comment or a CDATA
section contains a single string.  An entity reference contains a
single name.  A character reference contains a character number."
  (interactive "p")
  (or arg (setq arg 1))
  (cond ((> arg 0)
	 (while (progn
		  (nxml-forward-single-balanced-item)
		  (> (setq arg (1- arg)) 0))))
	((< arg 0)
	 (while (progn
		  (nxml-backward-single-balanced-item)
		  (< (setq arg (1+ arg)) 0))))))

(defun nxml-forward-single-balanced-item ()
  (condition-case err
      (goto-char (let ((end (nxml-token-after)))
		   (save-excursion
		     (while (eq xmltok-type 'space)
		       (goto-char end)
		       (setq end (nxml-token-after)))
		     (cond ((/= (point) xmltok-start)
			    (nxml-scan-forward-within end))
			   ((and nxml-sexp-element-flag
				 (eq xmltok-type 'start-tag))
			    ;; can't ever return nil here
			    (nxml-scan-element-forward xmltok-start))
			   ((and nxml-sexp-element-flag
				 (memq xmltok-type
				       '(end-tag partial-end-tag)))
			    (error "Already at end of element"))
			   (t end)))))
    (nxml-scan-error
     (goto-char (cadr err))
     (apply 'error (cddr err)))))

(defun nxml-backward-single-balanced-item ()
  (condition-case err
      (goto-char (let ((end (nxml-token-before)))
		   (save-excursion
		     (while (eq xmltok-type 'space)
		       (goto-char xmltok-start)
		       (setq end (nxml-token-before)))
		     (cond ((/= (point) end)
			    (nxml-scan-backward-within end))
			   ((and nxml-sexp-element-flag
				 (eq xmltok-type 'end-tag))
			    ;; can't ever return nil here
			    (nxml-scan-element-backward end)
			    xmltok-start)
			   ((and nxml-sexp-element-flag
				 (eq xmltok-type 'start-tag))
			    (error "Already at start of element"))
			   (t xmltok-start)))))
    (nxml-scan-error
     (goto-char (cadr err))
     (apply 'error (cddr err)))))

(defun nxml-scan-forward-within (end)
  (setq end (- end (nxml-end-delimiter-length xmltok-type)))
  (when (<= end (point))
    (error "Already at end of %s"
	   (nxml-token-type-friendly-name xmltok-type)))
  (cond ((memq xmltok-type '(start-tag
			     empty-element
			     partial-start-tag
			     partial-empty-element))
	 (if (< (point) xmltok-name-end)
	     xmltok-name-end
	   (let ((att (nxml-find-following-attribute)))
	     (cond ((not att) end)
		   ((and (xmltok-attribute-value-start att)
			 (<= (xmltok-attribute-value-start att)
			     (point)))
		    (nxml-scan-forward-in-attribute-value att))
		   ((xmltok-attribute-value-end att)
		    (1+ (xmltok-attribute-value-end att)))
		   ((save-excursion
		      (goto-char (xmltok-attribute-name-end att))
		      (looking-at "[ \t\r\n]*="))
		    (match-end 0))
		   (t (xmltok-attribute-name-end att))))))
	((and (eq xmltok-type 'processing-instruction)
	      (< (point) xmltok-name-end))
	 xmltok-name-end)
	(t end)))

(defun nxml-scan-backward-within (end)
  (setq xmltok-start
	(+ xmltok-start
	   (nxml-start-delimiter-length xmltok-type)))
  (when (<= (point) xmltok-start)
    (error "Already at start of %s"
	   (nxml-token-type-friendly-name xmltok-type)))
  (cond ((memq xmltok-type '(start-tag
			     empty-element
			     partial-start-tag
			     partial-empty-element))
	 (let ((att (nxml-find-preceding-attribute)))
	   (cond ((not att) xmltok-start)
		 ((and (xmltok-attribute-value-start att)
		       (<= (xmltok-attribute-value-start att)
			   (point))
		       (<= (point)
			   (xmltok-attribute-value-end att)))
		  (nxml-scan-backward-in-attribute-value att))
		 (t (xmltok-attribute-name-start att)))))
	((and (eq xmltok-type 'processing-instruction)
	      (let ((content-start (save-excursion
				     (goto-char xmltok-name-end)
				     (skip-chars-forward " \r\t\n")
				     (point))))
		(and (< content-start (point))
		     content-start))))
	(t xmltok-start)))

(defun nxml-scan-forward-in-attribute-value (att)
  (when (= (point) (xmltok-attribute-value-end att))
    (error "Already at end of attribute value"))
  (let ((refs (xmltok-attribute-refs att))
	ref)
    (while refs
      (setq ref (car refs))
      (if (< (point) (aref ref 2))
	  (setq refs nil)
	(setq ref nil)
	(setq refs (cdr refs))))
    (cond ((not ref)
	   (xmltok-attribute-value-end att))
	  ((< (point) (aref ref 1))
	   (aref ref 1))
	  ((= (point) (aref ref 1))
	   (aref ref 2))
	  (t
	   (let ((end (- (aref ref 2)
			 (nxml-end-delimiter-length (aref ref 0)))))
	     (if (< (point) end)
		 end
	       (error "Already at end of %s"
		      (nxml-token-type-friendly-name (aref ref 0)))))))))

(defun nxml-scan-backward-in-attribute-value (att)
  (when (= (point) (xmltok-attribute-value-start att))
    (error "Already at start of attribute value"))
  (let ((refs (reverse (xmltok-attribute-refs att)))
	ref)
    (while refs
      (setq ref (car refs))
      (if (< (aref ref 1) (point))
	  (setq refs nil)
	(setq ref nil)
	(setq refs (cdr refs))))
    (cond ((not ref)
	   (xmltok-attribute-value-start att))
	  ((< (aref ref 2) (point))
	   (aref ref 2))
	  ((= (point) (aref ref 2))
	   (aref ref 1))
	  (t
	   (let ((start (+ (aref ref 1)
			   (nxml-start-delimiter-length (aref ref 0)))))
	     (if (< start (point))
		 start
	       (error "Already at start of %s"
		      (nxml-token-type-friendly-name (aref ref 0)))))))))

(defun nxml-find-following-attribute ()
  (let ((ret nil)
	(atts (or xmltok-attributes xmltok-namespace-attributes))
	(more-atts (and xmltok-attributes xmltok-namespace-attributes)))
    (while atts
      (let* ((att (car atts))
	     (name-start (xmltok-attribute-name-start att)))
	(cond ((and (<= name-start (point))
		    (xmltok-attribute-value-end att)
		    ;; <= because end is before quote
		    (<= (point) (xmltok-attribute-value-end att)))
	       (setq atts nil)
	       (setq ret att))
	      ((and (< (point) name-start)
		    (or (not ret)
			(< name-start
			   (xmltok-attribute-name-start ret))))
	       (setq ret att))))
      (setq atts (cdr atts))
      (unless atts
	(setq atts more-atts)
	(setq more-atts nil)))
    ret))

(defun nxml-find-preceding-attribute ()
  (let ((ret nil)
	(atts (or xmltok-attributes xmltok-namespace-attributes))
	(more-atts (and xmltok-attributes xmltok-namespace-attributes)))
    (while atts
      (let* ((att (car atts))
	     (name-start (xmltok-attribute-name-start att)))
	(cond ((and (< name-start (point))
		    (xmltok-attribute-value-end att)
		    ;; <= because end is before quote
		    (<= (point) (xmltok-attribute-value-end att)))
	       (setq atts nil)
	       (setq ret att))
	      ((and (< name-start (point))
		    (or (not ret)
			(< (xmltok-attribute-name-start ret)
			   name-start)))
	       (setq ret att))))
      (setq atts (cdr atts))
      (unless atts
	(setq atts more-atts)
	(setq more-atts nil)))
    ret))

(defun nxml-up-element (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (nxml-backward-up-element (- arg))
    (condition-case err
	(while (and (> arg 0)
		    (< (point) (point-max)))
	  (let ((token-end (nxml-token-after)))
	    (goto-char (cond ((or (memq xmltok-type '(end-tag
						      partial-end-tag))
				  (and (memq xmltok-type
					     '(empty-element
					       partial-empty-element))
				       (< xmltok-start (point))))
			      token-end)
			     ((nxml-scan-element-forward
			       (if (and (eq xmltok-type 'start-tag)
					(= (point) xmltok-start))
				   xmltok-start
				 token-end)
			       t))
			     (t (error "No parent element")))))
	  (setq arg (1- arg)))
      (nxml-scan-error
       (goto-char (cadr err))
       (apply 'error (cddr err))))))

(defun nxml-backward-up-element (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (nxml-up-element (- arg))
    (condition-case err
	(while (and (> arg 0)
		    (< (point-min) (point)))
	  (let ((token-end (nxml-token-before)))
	    (goto-char (cond ((or (memq xmltok-type '(start-tag
						      partial-start-tag))
				  (and (memq xmltok-type
					     '(empty-element
					       partial-empty-element))
				       (< (point) token-end)))
			      xmltok-start)
			     ((nxml-scan-element-backward
			       (if (and (eq xmltok-type 'end-tag)
					(= (point) token-end))
				   token-end
				 xmltok-start)
			       t)
			      xmltok-start)
			     (t (error "No parent element")))))
	  (setq arg (1- arg)))
      (nxml-scan-error
       (goto-char (cadr err))
       (apply 'error (cddr err))))))

(defun nxml-down-element (&optional arg)
  "Move forward down into the content of an element.
With ARG, do this that many times.
Negative ARG means move backward but still down."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (nxml-backward-down-element (- arg))
    (while (> arg 0)
      (goto-char
       (let ((token-end (nxml-token-after)))
	 (save-excursion
	   (goto-char token-end)
	   (while (progn
		    (when (memq xmltok-type '(nil end-tag partial-end-tag))
		      (error "No following start-tags in this element"))
		    (not (memq xmltok-type '(start-tag partial-start-tag))))
	     (nxml-tokenize-forward))
	   (point))))
      (setq arg (1- arg)))))

(defun nxml-backward-down-element (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (nxml-down-element (- arg))
    (while (> arg 0)
      (goto-char
	 (save-excursion
	   (nxml-token-before)
	   (goto-char xmltok-start)
	   (while (progn
		    (when (memq xmltok-type '(start-tag
					      partial-start-tag
					      prolog
					      nil))
		      (error "No preceding end-tags in this element"))
		    (not (memq xmltok-type '(end-tag partial-end-tag))))
	     (if (or (<= (point) nxml-prolog-end)
		     (not (search-backward "<" nxml-prolog-end t)))
		 (setq xmltok-type nil)
	       (nxml-move-outside-backwards)
	       (xmltok-forward)))
	   xmltok-start))
      (setq arg (1- arg)))))

(defun nxml-forward-element (&optional arg)
  "Move forward over one element.
With ARG, do it that many times.
Negative ARG means move backward."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (nxml-backward-element (- arg))
    (condition-case err
	(while (and (> arg 0)
		    (< (point) (point-max)))
	  (goto-char
	   (or (nxml-scan-element-forward (nxml-token-before))
	       (error "No more elements")))
	  (setq arg (1- arg)))
    (nxml-scan-error
     (goto-char (cadr err))
     (apply 'error (cddr err))))))

(defun nxml-backward-element (&optional arg)
  "Move backward over one element.
With ARG, do it that many times.
Negative ARG means move forward."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (nxml-forward-element (- arg))
    (condition-case err
	(while (and (> arg 0)
		    (< (point-min) (point)))
	  (goto-char
	   (or (and (nxml-scan-element-backward (progn
						  (nxml-token-after)
						  xmltok-start))
		    xmltok-start)
	       (error "No preceding elements")))
	  (setq arg (1- arg)))
    (nxml-scan-error
     (goto-char (cadr err))
     (apply 'error (cddr err))))))

(defun nxml-mark-token-after ()
  (interactive)
  (push-mark (nxml-token-after) nil t)
  (goto-char xmltok-start)
  (message "Marked %s" xmltok-type))

;;; Paragraphs

(defun nxml-mark-paragraph ()
  "Put point at beginning of this paragraph, mark at end.
The paragraph marked is the one that contains point or follows point."
  (interactive)
  (nxml-forward-paragraph)
  (push-mark nil t t)
  (nxml-backward-paragraph))

(defun nxml-forward-paragraph (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (cond ((< arg 0)
	 (nxml-backward-paragraph (- arg)))
	((> arg 0)
	 (forward-line 0)
	 (while (and (nxml-forward-single-paragraph)
		     (> (setq arg (1- arg)) 0))))))

(defun nxml-backward-paragraph (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (cond ((< arg 0)
	 (nxml-forward-paragraph (- arg)))
	((> arg 0)
	 (unless (bolp)
	   (let ((inhibit-field-text-motion t))
	     (end-of-line)))
	 (while (and (nxml-backward-single-paragraph)
		     (> (setq arg (1- arg)) 0))))))

(defun nxml-forward-single-paragraph ()
  "Move forward over a single paragraph.
Return nil at end of buffer, t otherwise."
  (let* ((token-end (nxml-token-after))
	 (offset (- (point) xmltok-start))
	 pos had-data)
    (goto-char token-end)
    (while (and (< (point) (point-max))
		(not (setq pos
			   (nxml-paragraph-end-pos had-data offset))))
      (when (nxml-token-contains-data-p offset)
	(setq had-data t))
      (nxml-tokenize-forward)
      (setq offset 0))
    (when pos (goto-char pos))))

(defun nxml-backward-single-paragraph ()
  "Move backward over a single paragraph.
Return nil at start of buffer, t otherwise."
  (let* ((token-end (nxml-token-before))
	 (offset (- token-end (point)))
	 (last-tag-pos xmltok-start)
	 pos had-data last-data-pos)
    (goto-char token-end)
    (unless (setq pos (nxml-paragraph-start-pos nil offset))
      (setq had-data (nxml-token-contains-data-p nil offset))
      (goto-char xmltok-start)
      (while (and (not pos) (< (point-min) (point)))
	(cond ((search-backward "<" nxml-prolog-end t)
	       (nxml-move-outside-backwards)
	       (save-excursion
		 (while (< (point) last-tag-pos)
		   (xmltok-forward)
		   (when (and (not had-data) (nxml-token-contains-data-p))
		     (setq pos nil)
		     (setq last-data-pos xmltok-start))
		   (let ((tem (nxml-paragraph-start-pos had-data 0)))
		     (when tem (setq pos tem)))))
	       (when (and (not had-data) last-data-pos (not pos))
		 (setq had-data t)
		 (save-excursion
		   (while (< (point) last-data-pos)
		     (xmltok-forward))
		   (let ((tem (nxml-paragraph-start-pos had-data 0)))
		     (when tem (setq pos tem)))))
	       (setq last-tag-pos (point)))
	      (t (goto-char (point-min))))))
    (when pos (goto-char pos))))

(defun nxml-token-contains-data-p (&optional start end)
  (setq start (+ xmltok-start (or start 0)))
  (setq end (- (point) (or end 0)))
  (when (eq xmltok-type 'cdata-section)
    (setq start (max start (+ xmltok-start 9)))
    (setq end (min end (- (point) 3))))
  (or (and (eq xmltok-type 'data)
	   (eq start xmltok-start)
	   (eq end (point)))
      (eq xmltok-type 'char-ref)
      (and (memq xmltok-type '(data cdata-section))
	   (< start end)
	   (save-excursion
	     (goto-char start)
	     (re-search-forward "[^ \t\r\n]" end t)))))

(defun nxml-paragraph-end-pos (had-data offset)
  "Return the position of the paragraph end if contained in the current token.
Return nil if the current token does not contain the paragraph end.
Only characters after OFFSET from the start of the token are eligible.
HAD-DATA says whether there have been non-whitespace data characters yet."
  (cond ((not had-data)
	 (cond ((memq xmltok-type '(data cdata-section))
		(save-excursion
		  (let ((end (point)))
		    (goto-char (+ xmltok-start
				  (max (if (eq xmltok-type 'cdata-section)
					   9
					 0)
				       offset)))
		    (and (re-search-forward "[^ \t\r\n]" end t)
			 (re-search-forward "^[ \t]*$" end t)
			 (match-beginning 0)))))
	       ((and (eq xmltok-type 'comment)
		     (nxml-token-begins-line-p)
		     (nxml-token-ends-line-p))
		(save-excursion
		  (let ((end (point)))
		    (goto-char (+ xmltok-start (max 4 offset)))
		    (when (re-search-forward "[^ \t\r\n]" (- end 3) t)
		      (if (re-search-forward "^[ \t]*$" end t)
			  (match-beginning 0)
			(goto-char (- end 3))
			(skip-chars-backward " \t")
			(unless (bolp)
			  (beginning-of-line 2))
			(point))))))))
	((memq xmltok-type '(data space cdata-section))
	 (save-excursion
	   (let ((end (point)))
	     (goto-char (+ xmltok-start offset))
	     (and (re-search-forward "^[ \t]*$" end t)
		  (match-beginning 0)))))
	((and (memq xmltok-type '(start-tag 
				  end-tag
				  empty-element
				  comment
				  processing-instruction
				  entity-ref))
	      (nxml-token-begins-line-p)
	      (nxml-token-ends-line-p))
	 (save-excursion
	   (goto-char xmltok-start)
	   (skip-chars-backward " \t")
	   (point)))
	((and (eq xmltok-type 'end-tag)
	      (looking-at "[ \t]*$")
	      (not (nxml-in-mixed-content-p t)))
	 (save-excursion
	   (or (search-forward "\n" nil t) 
	       (point-max))))))

(defun nxml-paragraph-start-pos (had-data offset)
  "Return the position of the paragraph start if contained in the current token.
Return nil if the current token does not contain the paragraph start.
Only characters before OFFSET from the end of the token are eligible.
HAD-DATA says whether there have been non-whitespace data characters yet."
  (cond ((not had-data)
	 (cond ((memq xmltok-type '(data cdata-section))
		(save-excursion
		  (goto-char (- (point)
				(max (if (eq xmltok-type 'cdata-section)
					 3
				       0)
				     offset)))
		  (and (re-search-backward "[^ \t\r\n]" xmltok-start t)
		       (re-search-backward "^[ \t]*$" xmltok-start t)
		       (match-beginning 0))))
	       ((and (eq xmltok-type 'comment)
		     (nxml-token-ends-line-p)
		     (nxml-token-begins-line-p))
		(save-excursion
		  (goto-char (- (point) (max 3 offset)))
		  (when (and (< (+ xmltok-start 4) (point))
			     (re-search-backward "[^ \t\r\n]"
						 (+ xmltok-start 4)
						 t))
		    (if (re-search-backward "^[ \t]*$" xmltok-start t)
			(match-beginning 0)
		      (goto-char xmltok-start)
		      (if (looking-at "<!--[ \t]*\n")
			  (match-end 0)
			(skip-chars-backward " \t")
			(point))))))))
	((memq xmltok-type '(data space cdata-section))
	 (save-excursion
	   (goto-char (- (point) offset))
	   (and (re-search-backward "^[ \t]*$" xmltok-start t)
		(match-beginning 0))))
	((and (memq xmltok-type '(start-tag 
				  end-tag
				  empty-element
				  comment
				  processing-instruction
				  entity-ref))
	      (nxml-token-ends-line-p)
	      (nxml-token-begins-line-p))
	 (or (search-forward "\n" nil t) 
	     (point-max)))
	((and (eq xmltok-type 'start-tag)
	      (nxml-token-begins-line-p)
	      (not (save-excursion
		     (goto-char xmltok-start)
		     (nxml-in-mixed-content-p nil))))
	 (save-excursion
	   (goto-char xmltok-start)
	   (skip-chars-backward " \t")
	   ;; include any blank line before
	   (or (and (eq (char-before) ?\n)
		    (save-excursion
		      (goto-char (1- (point)))
		      (skip-chars-backward " \t")
		      (and (bolp) (point))))
	       (point))))))

(defun nxml-token-ends-line-p () (looking-at "[ \t]*$"))

(defun nxml-token-begins-line-p ()
  (save-excursion
    (goto-char xmltok-start)
    (skip-chars-backward " \t")
    (bolp)))

(defun nxml-in-mixed-content-p (endp)
  "Return non-nil if point is in mixed content.
Point must be after an end-tag or before a start-tag.
ENDP is t in the former case, nil in the latter."
  (let (matching-tag-pos)
    (cond ((not (run-hook-with-args-until-failure
		 'nxml-in-mixed-content-hook))
	   nil)
	  ;; See if the matching tag does not start or end a line.
	  ((condition-case err
	       (progn
		 (setq matching-tag-pos
		       (xmltok-save
			 (if endp
			     (and (nxml-scan-element-backward (point))
				  xmltok-start)
			   (nxml-scan-element-forward (point)))))
		 (and matching-tag-pos
		      (save-excursion
			(goto-char matching-tag-pos)
			(not (if endp
				 (progn
				   (skip-chars-backward " \t")
				   (bolp))
			       (looking-at "[ \t]*$"))))))
	     (nxml-scan-error nil))
	   t)
	  ;; See if there's data at the same level.
	  ((let (start end)
	     (if endp
		 (setq start matching-tag-pos
		       end (point))
	       (setq start (point)
		     end matching-tag-pos))
	     (save-excursion
	       (or (when start
		     (goto-char start)
		     (nxml-preceding-sibling-data-p))
		   (when end
		     (goto-char end)
		     (nxml-following-sibling-data-p)))))
	   t)
	  ;; Otherwise, treat as not mixed
	  (t nil))))

(defun nxml-preceding-sibling-data-p ()
  "Return non-nil if there is a previous sibling that is data."
  (let ((lim (max (- (point) nxml-mixed-scan-distance)
		  nxml-prolog-end))
	(level 0)
	found end)
    (xmltok-save
      (save-excursion
	(while (and (< lim (point))
		    (>= level 0)
		    (not found)
		    (progn
		      (setq end (point))
		      (search-backward "<" lim t)))
	  (nxml-move-outside-backwards)
	  (save-excursion
	    (xmltok-forward)
	    (let ((prev-level level))
	      (cond ((eq xmltok-type 'end-tag)
		     (setq level (1+ level)))
		    ((eq xmltok-type 'start-tag)
		     (setq level (1- level))))
	      (when (eq prev-level 0)
		(while (and (< (point) end) (not found))
		  (xmltok-forward)
		  (when (memq xmltok-type '(data cdata-section char-ref))
		    (setq found t)))))))))
    found))

(defun nxml-following-sibling-data-p ()
  (let ((lim (min (+ (point) nxml-mixed-scan-distance)
		  (point-max)))
	(level 0)
	found)
    (xmltok-save
      (save-excursion
	(while (and (< (point) lim)
		    (>= level 0)
		    (nxml-tokenize-forward)
		    (not found))
	  (cond ((eq xmltok-type 'start-tag)
		 (setq level (1+ level)))
		((eq xmltok-type 'end-tag)
		 (setq level (1- level)))
		((and (eq level 0)
		      (memq xmltok-type '(data cdata-section char-ref)))
		 (setq found t))))))
    found))

;;; Filling

(defun nxml-do-fill-paragraph (arg)
  (let (fill-paragraph-function
	fill-prefix
	start end)
    (save-excursion
      (nxml-forward-paragraph)
      (setq end (point))
      (nxml-backward-paragraph)
      (skip-chars-forward " \t\r\n")
      (setq start (point))
      (beginning-of-line)
      (setq fill-prefix (buffer-substring-no-properties (point) start))
      (when (and (not (nxml-get-inside (point)))
		 (looking-at "[ \t]*<!--"))
	(setq fill-prefix (concat fill-prefix "     ")))
      (fill-region-as-paragraph start end arg))
    (skip-line-prefix fill-prefix)
    fill-prefix))
    
(defun nxml-newline-and-indent (soft)
  (delete-horizontal-space)
  (if soft (insert-and-inherit ?\n) (newline 1))
  (nxml-indent-line))


;;; Dynamic markup

(defvar nxml-dynamic-markup-prev-pos nil)
(defvar nxml-dynamic-markup-prev-lengths nil)
(defvar nxml-dynamic-markup-prev-found-marker nil)
(defvar nxml-dynamic-markup-prev-start-tags (make-hash-table :test 'equal))

(defun nxml-dynamic-markup-word ()
  "Dynamically markup the word before point.
This attempts to find a tag to put around the word before point based
on the contents of the current buffer. The end-tag will be inserted at
point.  The start-tag will be inserted at or before the beginning of
the word before point; the contents of the current buffer is used to
decide where.

It works in a similar way to \\[dabbrev-expand]. It searches first
backwards from point, then forwards from point for an element whose
content is a string which matches the contents of the buffer before
point and which includes at least the word before point. It then
copies the start- and end-tags from that element and uses them to
surround the matching string before point.

Repeating \\[nxml-dynamic-markup-word] immediately after successful
\\[nxml-dynamic-markup-word] removes the previously inserted markup
and attempts to find another possible way to do the markup."
  (interactive "*")
  (let (search-start-pos done)
    (if (and (integerp nxml-dynamic-markup-prev-pos)
	     (= nxml-dynamic-markup-prev-pos (point))
	     (eq last-command this-command)
	     nxml-dynamic-markup-prev-lengths)
	(let* ((end-tag-open-pos
		(- nxml-dynamic-markup-prev-pos
		   (nth 2 nxml-dynamic-markup-prev-lengths)))
	       (start-tag-close-pos
		(- end-tag-open-pos
		   (nth 1 nxml-dynamic-markup-prev-lengths)))
	       (start-tag-open-pos
		(- start-tag-close-pos
		   (nth 0 nxml-dynamic-markup-prev-lengths))))
	  (delete-region end-tag-open-pos nxml-dynamic-markup-prev-pos)
	  (delete-region start-tag-open-pos start-tag-close-pos)
	  (setq search-start-pos
		(marker-position nxml-dynamic-markup-prev-found-marker)))
      (clrhash nxml-dynamic-markup-prev-start-tags))
    (setq nxml-dynamic-markup-prev-pos nil)
    (setq nxml-dynamic-markup-prev-lengths nil)
    (setq nxml-dynamic-markup-prev-found-marker nil)
    (goto-char
     (save-excursion
       (let* ((pos (point))
	      (word (progn
		      (backward-word 1)
		      (unless (< (point) pos)
			(error "No word to markup"))
		      (buffer-substring-no-properties (point) pos)))
	      (search (concat word "</"))
	      done)
	 (when search-start-pos
	   (goto-char search-start-pos))
	 (while (and (not done)
		     (or (and (< (point) pos)
			      (or (search-backward search nil t)
				  (progn (goto-char pos) nil)))
			 (search-forward search nil t)))
	   (goto-char (- (match-end 0) 2))
	   (setq done (nxml-try-copy-markup pos)))
	 (or done
	     (error (if (zerop (hash-table-count
				nxml-dynamic-markup-prev-start-tags))
			"No possible markup found for `%s'"
		      "No more markup possibilities found for `%s'")
		    word)))))))

(defun nxml-try-copy-markup (word-end-pos)
  (save-excursion
    (let ((end-tag-pos (point)))
      (when (and (not (nxml-get-inside end-tag-pos))
		 (search-backward "<" nil t)
		 (not (nxml-get-inside (point))))
	(xmltok-forward)
	(when (and (eq xmltok-type 'start-tag)
		   (< (point) end-tag-pos))
	  (let* ((start-tag-close-pos (point))
		 (start-tag
		  (buffer-substring-no-properties xmltok-start
						  start-tag-close-pos))
		 (words
		  (nreverse
		   (split-string
		    (buffer-substring-no-properties start-tag-close-pos
						    end-tag-pos)
		    "[ \t\r\n]+"))))
	    (goto-char word-end-pos)
	    (while (and words
			(re-search-backward (concat
					     (regexp-quote (car words))
					     "\\=")
					    nil
					    t))
	      (setq words (cdr words))
	      (skip-chars-backward " \t\r\n"))
	    (when (and (not words)
		       (progn
			 (skip-chars-forward " \t\r\n")
			 (not (gethash (cons (point) start-tag)
				       nxml-dynamic-markup-prev-start-tags)))
		       (or (< end-tag-pos (point))
			   (< word-end-pos xmltok-start)))
	      (setq nxml-dynamic-markup-prev-found-marker
		    (copy-marker end-tag-pos t))
	      (puthash (cons (point) start-tag)
		       t
		       nxml-dynamic-markup-prev-start-tags)
	      (setq nxml-dynamic-markup-prev-lengths
		    (list (- start-tag-close-pos xmltok-start)
			  (-  word-end-pos (point))
			  (+ (- xmltok-name-end xmltok-start) 2)))
	      (let ((name (xmltok-start-tag-qname)))
		(insert start-tag)
		(goto-char (+ word-end-pos
			      (- start-tag-close-pos xmltok-start)))
		(insert "</" name ">")
		(setq nxml-dynamic-markup-prev-pos (point))))))))))
							   

;;; Character names

(defvar nxml-char-name-ignore-case nil)

(defvar nxml-char-name-alist nil
  "Alist of character names.
Each member of the list has the form (NAME CODE . NAMESET),
where NAME is a string naming a character, NAMESET is a symbol
identifying a set of names and CODE is an integer specifying the
Unicode scalar value of the named character.
The NAME will only be used for completion if NAMESET has
a non-nil `nxml-char-name-set-enabled' property.
If NAMESET does does not have `nxml-char-name-set-defined' property,
then it must have a `nxml-char-name-set-file' property and `load'
will be applied to the value of this property if the nameset
is enabled.")

(defvar nxml-char-name-table (make-hash-table :test 'eq)
  "Hash table for mapping char codes to names.
Each key is a Unicode scalar value.
Each value is a list of pairs of the form (NAMESET . NAME),
where NAMESET is a symbol identifying a set of names,
and NAME is a string naming a character.")

(defvar nxml-autoload-char-name-set-list nil
  "List of char namesets that can be autoloaded.")

(defun nxml-enable-char-name-set (nameset)    
  (put nameset 'nxml-char-name-set-enabled t))

(defun nxml-disable-char-name-set (nameset)    
  (put nameset 'nxml-char-name-set-enabled nil))

(defun nxml-char-name-set-enabled-p (nameset)
  (get nameset 'nxml-char-name-set-enabled))

(defun nxml-autoload-char-name-set (nameset file)
  (unless (memq nameset nxml-autoload-char-name-set-list)
    (setq nxml-autoload-char-name-set-list
	  (cons nameset nxml-autoload-char-name-set-list)))
  (put nameset 'nxml-char-name-set-file file))

(defun nxml-define-char-name-set (nameset alist)
  "Define a set of character names.
NAMESET is a symbol identifying the set.
Alist is a list where each member has the form (NAME CODE),
where NAME is a string naming a character and code
is an integer giving the Unicode scalar value of the character."
  (when (get nameset 'nxml-char-name-set-defined)
    (error "Nameset `%s' already defined" nameset))
  (let ((iter alist))
    (while iter
      (let* ((name-code (car iter))
	     (name (car name-code))
	     (code (cadr name-code)))
	(puthash code
		 (cons (cons nameset name)
		       (gethash code nxml-char-name-table))
		 nxml-char-name-table))
      (setcdr (cdr (car iter)) nameset)
      (setq iter (cdr iter))))
  (setq nxml-char-name-alist
	(nconc alist nxml-char-name-alist))
  (put nameset 'nxml-char-name-set-defined t))

(defun nxml-get-char-name (code)
  (mapcar 'nxml-maybe-load-char-name-set nxml-autoload-char-name-set-list)
  (let ((names (gethash code nxml-char-name-table))
	name)
    (while (and names (not name))
      (if (nxml-char-name-set-enabled-p (caar names))
	  (setq name (cdar names))
	(setq names (cdr names))))
    name))

(defvar nxml-named-char-history nil)

(defun nxml-insert-named-char (arg)
  "Insert a character using its name.
The name is read from the minibuffer.
Normally, inserts the character as a numeric character reference.
With a prefix argument, inserts the character directly."
  (interactive "*P")
  (mapcar 'nxml-maybe-load-char-name-set nxml-autoload-char-name-set-list)
  (let ((name
	 (let ((completion-ignore-case nxml-char-name-ignore-case))
	   (completing-read "Character name: "
			    nxml-char-name-alist
			    (lambda (member)
			      (get (cddr member) 'nxml-char-name-set-enabled))
			    t
			    nil
			    'nxml-named-char-history)))
	(alist nxml-char-name-alist)
	elt code)
    (while (and alist (not code))
      (setq elt (assoc name alist))
      (if (get (cddr elt) 'nxml-char-name-set-enabled)
	  (setq code (cadr elt))
	(setq alist (cdr (member elt alist)))))
    (when code
      (insert (if arg
		  (or (decode-char 'ucs code)
		      (error "Character %x is not supported by Emacs"
			     code))
		(format "&#x%X;" code))))))
  
(defun nxml-maybe-load-char-name-set (sym)
  (when (and (get sym 'nxml-char-name-set-enabled)
	     (not (get sym 'nxml-char-name-set-defined))
	     (stringp (get sym 'nxml-char-name-set-file)))
    (load (get sym 'nxml-char-name-set-file))))

(defun nxml-toggle-char-ref-extra-display (arg)
  "*Toggle the display of extra information for character references."
  (interactive "P")
  (let ((new (if (null arg)
		 (not nxml-char-ref-extra-display)
	       (> (prefix-numeric-value arg) 0))))
    (when (not (eq new nxml-char-ref-extra-display))
      (setq nxml-char-ref-extra-display new)
      (save-excursion
	(save-restriction
	  (widen)
	  (if nxml-char-ref-extra-display
	      (nxml-with-unmodifying-text-property-changes
		(nxml-clear-fontified (point-min) (point-max)))
	    (nxml-clear-char-ref-extra-display (point-min) (point-max))))))))

(put 'nxml-char-ref 'evaporate t)

(defun nxml-char-ref-display-extra (start end n)
  (when nxml-char-ref-extra-display
    (let ((name (nxml-get-char-name n))
	  (glyph-string (and nxml-char-ref-display-glyph-flag
			     (nxml-glyph-display-string n 'nxml-glyph-face)))
	  ov)
    (when (or name glyph-string)
      (setq ov (make-overlay start end nil t))
      (overlay-put ov 'category 'nxml-char-ref)
      (when name
	(overlay-put ov 'help-echo name))
      (when glyph-string
	(overlay-put ov
		     'after-string
		     (propertize glyph-string 'face 'nxml-glyph-face)))))))

(defun nxml-clear-char-ref-extra-display (start end)
  (let ((ov (overlays-in start end)))
    (while ov
      (when (eq (overlay-get (car ov) 'category) 'nxml-char-ref)
	(delete-overlay (car ov)))
      (setq ov (cdr ov)))))

;;; Versioning

(defun nxml-version ()
  "Show the version of nXML mode that is being used."
  (interactive)
  (if nxml-version
      (message "nXML mode version %s" nxml-version)
    (message "nXML mode version unknown")))


(defun nxml-start-delimiter-length (type)
  (or (get type 'nxml-start-delimiter-length)
      0))
			
(put 'cdata-section 'nxml-start-delimiter-length 9)
(put 'comment 'nxml-start-delimiter-length 4)
(put 'processing-instruction 'nxml-start-delimiter-length 2)
(put 'start-tag 'nxml-start-delimiter-length 1)
(put 'empty-element 'nxml-start-delimiter-length 1)
(put 'partial-empty-element 'nxml-start-delimiter-length 1)
(put 'entity-ref 'nxml-start-delimiter-length 1)
(put 'char-ref 'nxml-start-delimiter-length 2)

(defun nxml-end-delimiter-length (type)
  (or (get type 'nxml-end-delimiter-length)
      0))
			
(put 'cdata-section 'nxml-end-delimiter-length 3)
(put 'comment 'nxml-end-delimiter-length 3)
(put 'processing-instruction 'nxml-end-delimiter-length 2)
(put 'start-tag 'nxml-end-delimiter-length 1)
(put 'empty-element 'nxml-end-delimiter-length 2)
(put 'partial-empty-element 'nxml-end-delimiter-length 1)
(put 'entity-ref 'nxml-end-delimiter-length 1)
(put 'char-ref 'nxml-end-delimiter-length 1)

(defun nxml-token-type-friendly-name (type)
  (or (get type 'nxml-friendly-name)
      (symbol-name type)))

(put 'cdata-section 'nxml-friendly-name "CDATA section")
(put 'processing-instruction 'nxml-friendly-name "processing instruction")
(put 'entity-ref 'nxml-friendly-name "entity reference")
(put 'char-ref 'nxml-friendly-name "character reference")

(provide 'nxml-mode)

;;; nxml-mode.el ends here
