;;; mmm-vars.el --- Variables for MMM Mode

;; Copyright (C) 2000, 2004 by Michael Abraham Shulman
;; Copyright (C) 2012, 2013 by Dmitry Gutov

;; Author: Michael Abraham Shulman <viritrilbia@users.sourceforge.net>

;;{{{ GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;}}}

;;; Commentary:

;; This file provides the definitions for the variables used by MMM
;; Mode, as well as several functions to manipulate them. It also
;; defines the errors that MMM Mode can signal.

;;; Code:

(require 'mmm-compat)
(require 'cl)

;; MISCELLANEOUS
;;{{{ Shut up the Byte Compiler

;; Otherwise it complains about undefined variables.
(eval-when-compile
  (defvar mmm-current-submode)
  (defvar mmm-save-local-variables)
  (defvar mmm-mode-string)
  (defvar mmm-submode-mode-line-format)
  (defvar mmm-mode-ext-classes-alist)
  (defvar mmm-mode-prefix-key)
  (defvar mmm-global-mode)
  (defvar mmm-primary-mode)
  (defvar mmm-classes-alist))

;;}}}
;;{{{ Error Conditions

;; Most of these should be caught internally and never seen by the
;; user, except when the user is creating submode regions manually.

;; Signalled when we try to put a submode region inside one where it
;; isn't meant to go.
(put 'mmm-subregion-invalid-parent
     'error-conditions
     '(mmm-subregion-invalid-parent mmm-error error))
(put 'mmm-subregion-invalid-parent
     'error-message
     "Invalid submode region parent")

;; Signalled when we try to put a submode region overlapping others in
;; an invalid way.
(put 'mmm-subregion-invalid-placement
     'error-conditions
     '(mmm-subregion-invalid-placement mmm-error error))
(put 'mmm-subregion-invalid-placement
     'error-message
     "Submode region placement invalid")

;; Signalled when we try to apply a submode class that doesn't exist.
(put 'mmm-invalid-submode-class
     'error-conditions
     '(mmm-invalid-submode-class mmm-error error))
(put 'mmm-invalid-submode-class
     'error-message
     "Invalid or undefined submode class")

;; Signalled by :match-submode functions when they are unable to
;; resolve a submode.  This error should *always* be caught internally
;; and never seen by the user.
(put 'mmm-no-matching-submode
     'error-conditions
     '(mmm-no-matching-submode mmm-error error))
(put 'mmm-no-matching-submode
     'error-message
     "Internal error: no matching submode.")

;;}}}

;; USER VARIABLES
;;{{{ Customization Group

(defgroup mmm nil
  "Multiple Major Modes in one buffer."
  :group 'tools)

;;}}}
;;{{{ Save Local Variables

(defvar mmm-c-derived-modes
  '(c-mode c++-mode objc-mode pike-mode java-mode jde-mode javascript-mode
    php-mode))

(defvar mmm-save-local-variables
  `(;; Don't use `function' (#') here!!  We're already inside `quote'!
    major-mode
    comment-start
    comment-end
    (comment-line-start-skip buffer (fortran-mode))
    comment-start-skip
    (comment-column buffer)
    comment-indent-function
    comment-line-break-function
    sentence-end
    ,@(when mmm-xemacs
        '(mode-popup-menu
          (((lambda () current-menubar) . set-buffer-menubar))
          ))
    (font-lock-keywords buffer)
    font-lock-set-defaults
    font-lock-major-mode
    font-lock-keywords-only
    font-lock-keywords-case-fold-search
    font-lock-syntax-table
    font-lock-mark-block-function       ; Override this?
    font-lock-syntactic-keywords
    font-lock-syntactic-face-function
    parse-sexp-ignore-comments  ; Fixes indentation in PHP-mode?
    ;; Can be different in different buffers
    (c-basic-offset
     buffer (c-mode c++-mode objc-mode pike-mode java-mode jde-mode))
    ;; These are necessary for C syntax parsing
    (c-class-key nil ,mmm-c-derived-modes)
    (c-extra-toplevel-key nil ,mmm-c-derived-modes)
    (c-inexpr-class-key nil ,mmm-c-derived-modes)
    (c-conditional-key nil ,mmm-c-derived-modes)
    semantic-bovinate-toplevel-override
    semantic-toplevel-bovine-table
    ;; Indentation style control variables.
    ;; These have to be localized in Emacs: see `mmm-mode-on'.
    ,@(mapcar
       #'(lambda (var) (list var nil mmm-c-derived-modes))
       '(c++-template-syntax-table
	 c-<-op-cont-regexp 
	 c->-op-cont-regexp
         c-after-brace-list-key
	 c-after-suffixed-type-decl-key
	 c-after-suffixed-type-maybe-decl-key
	 c-any-class-key
	 c-asm-stmt-kwds
	 c-assignment-op-regexp
	 c-backslash-column
	 c-basic-offset
         c-before-context-fontification-functions
	 c-bitfield-kwds
	 c-block-comment-prefix
	 c-block-decls-with-vars
         c-block-prefix-charset
         c-block-stmt-1-2-key
	 c-block-stmt-1-key
	 c-block-stmt-1-kwds
	 c-block-stmt-2-key
	 c-block-stmt-2-kwds
	 c-brace-list-key
         c-case-kwds-regexp
	 c-cast-parens 
	 c-class-key
	 c-class-kwds
	 c-cleanup-list
	 c-colon-type-list-re 
	 c-comment-only-line-offset
	 c-comment-prefix-regexp
	 c-comment-start-regexp
	 c-cpp-defined-fns
	 c-current-comment-prefix
	 c-decl-block-key
         c-decl-hangon-key
         c-decl-prefix-or-start-re
	 c-decl-prefix-re
	 c-decl-spec-kwds
         c-decl-start-kwds
         c-decl-start-re
	 c-doc-comment-start-regexp
	 c-expr-kwds
	 c-file-offsets
	 c-file-style
         c-not-primitive-type-keywords-regexp
	 c-hanging-braces-alist
	 c-hanging-colons-alist
	 c-hanging-comment-ender-p
	 c-hanging-comment-starter-p
	 c-hanging-semi\&comma-criteria
	 c-identifier-key 
	 c-identifier-last-sym-match
	 c-identifier-start 
	 c-identifier-syntax-modifications
	 c-identifier-syntax-table
	 c-in-comment-lc-prefix
	 c-indent-comment-alist
	 c-indent-comments-syntactically-p
	 c-indentation-style
	 c-inexpr-block-kwds
	 c-inexpr-class-kwds
	 c-keywords
	 c-keywords-obarray
	 c-keywords-regexp
	 c-known-type-key
         c-label-kwds
	 c-label-kwds-regexp
	 c-label-minimum-indentation
	 c-lambda-kwds
	 c-literal-start-regexp 
	 c-macro-with-semi-re
         c-nonlabel-token-key
         c-nonlabel-token-2-key
	 c-nonsymbol-chars 
	 c-nonsymbol-token-regexp
	 c-not-decl-init-keywords
	 c-offsets-alist
	 c-opt-<>-arglist-start 
	 c-opt-<>-arglist-start-in-paren
	 c-opt-<>-sexp-key 
	 c-opt-access-key
	 c-opt-asm-stmt-key
	 c-opt-bitfield-key
	 c-opt-block-decls-with-vars-key
	 c-opt-block-stmt-key
	 c-opt-cpp-prefix 
	 c-opt-cpp-start 
	 c-opt-decl-spec-key
	 c-opt-friend-key
	 c-opt-identifier-concat-key
	 c-opt-inexpr-block-key
	 c-opt-inexpr-brace-list-key
	 c-opt-inexpr-class-key
	 c-opt-lambda-key
	 c-opt-method-key
	 c-opt-postfix-decl-spec-key
	 c-opt-type-component-key
	 c-opt-type-concat-key 
	 c-opt-type-modifier-key 
	 c-opt-type-suffix-key 
	 c-other-decl-block-key
	 c-other-decl-block-kwds
	 c-other-decl-kwds
	 c-overloadable-operators-regexp
	 c-paragraph-separate 
	 c-paragraph-start 
	 c-paren-stmt-key 
	 c-primary-expr-regexp 
	 c-primitive-type-key 
	 c-primitive-type-kwds
	 c-protection-kwds
         c-postfix-decl-spec-key
	 c-recognize-<>-arglists 
	 c-recognize-knr-p
	 c-recognize-paren-inits 
	 c-recognize-typeless-decls
	 c-regular-keywords-regexp
	 c-simple-stmt-key 
	 c-simple-stmt-kwds
	 c-special-brace-lists
	 c-specifier-key 
	 c-specifier-kwds
	 c-stmt-delim-chars 
	 c-stmt-delim-chars-with-comma
         c-symbol-char-key
	 c-symbol-key
	 c-symbol-start 
	 c-syntactic-eol
	 c-syntactic-ws-end 
	 c-syntactic-ws-start 
	 c-type-decl-prefix-key 
	 c-type-decl-suffix-key 
	 c-type-prefix-key
         c-typeof-key
         c-prefix-spec-kwds-re
         c-typedef-key
	 c-typedef-decl-key
	 comment-end 
	 comment-start 
	 comment-start-skip))
    ,@(mapcar
       (lambda (var) (list var nil '(js-mode)))
       '(js--quick-match-re
         js--quick-match-re-func))
    ;; Skeleton insertion
    skeleton-transformation
    ;; Abbrev mode
    abbrev-mode
    local-abbrev-table
    ;; And finally the syntax table and local map.
    ((syntax-table . set-syntax-table) buffer)
    ((current-local-map . use-local-map) buffer)
    paragraph-separate
    paragraph-start
    (whitespace-active-style buffer)
    (whitespace-display-table buffer)
    (whitespace-display-table-was-local buffer)
    (whitespace-font-lock buffer)
    (whitespace-font-lock-mode buffer)
    (whitespace-font-lock-keywords buffer)
    (whitespace-mode buffer)
    forward-sexp-function
    smie-rules-function
    smie-grammar
    smie-forward-token-function
    smie-backward-token-function
    )
  "Which local variables to save for major mode regions.
Each element has the form \(VARIABLE [TYPE [MODES]]), causing VARIABLE
to be saved for all major modes in the list MODES.  If MODES is t or
absent, the variable is saved for all major modes.  MODES can also be
a function of no arguments which returns non-nil whenever the variable
should be saved.

TYPE should be either the symbol `global', meaning to save the
variable globally, the symbol `buffer', meaning to save it per buffer,
or the symbol `region', meaning to save it for each submode region.
If TYPE has any other value, such as nil, or is absent, the variable
is saved globally.  If all optional parameters are omitted, the
element may be simply VARIABLE instead of \(VARIABLE).

It is possible for VARIABLE to be not a symbol but a cons cell of the
form \(GETTER . SETTER), thus specifying special functions to set and
get the value of the \"variable\".  This is used for objects like
local maps, syntax tables, etc. which need to be installed in a
special way.  GETTER should be a function of no arguments, and SETTER
a function of one.  In this case, even if TYPE and MODES are omitted,
the list cannot be flattened--it must be \((GETTER . SETTER)).
\"Variables\" of this type cannot be seen with `mmm-get-saved-local'.

A single variable may appear more than once in this list, with
different modes and/or types.  If the same mode appears more than once
for the same variable with different types, the behavior is undefined.
Changing the value of this variable after MMM Mode has been activated
in some buffer may produce unpredictable results.

Globally saved variables are saved in the mmm-local-variables property
of the mode symbol.  Buffer saved variables are saved in the alist
`mmm-buffer-saved-locals'.  Region saved variables are saved in the
mmm-local-variables property of the overlay.")

(defvar mmm-buffer-saved-locals ()
  "Stores saved local variables for this buffer, by mode.
Each element looks like \(MODE \(VAR VALUE) ...).")
(make-variable-buffer-local 'mmm-buffer-saved-locals)

(defvar mmm-region-saved-locals-defaults ()
  "Stores saved defaults for region-saved locals, by mode.
Each element looks like \(MODE \(VAR VALUE) ...).  Used to initialize
new submode regions.")
(make-variable-buffer-local 'mmm-region-saved-locals-defaults)

(defvar mmm-region-saved-locals-for-dominant ()
  "Stores saved region locals for the dominant major mode.
The dominant major mode is considered to be one region for purposes of
saving region variables.  Region-saved variables for submode regions
are saved as overlay properties.")
(make-variable-buffer-local 'mmm-region-saved-locals-for-dominant)

;;}}}
;;{{{ Submode Faces

(defgroup mmm-faces nil
  "Faces and coloring for submode regions.
In general, only background colors should be set, to avoid interfering
with font-lock."
  :group 'mmm)

(defcustom mmm-submode-decoration-level 1
  "*Amount of coloring to use in submode regions.
Should be either 0, 1, or 2, representing None, Low, and High amounts
of coloring respectively.
* None (0) means to use no coloring at all.
* Low (1) means to use `mmm-default-submode-face' for all submode
  regions \(except for \"non-submode\" regions, i.e. those that are of
  the primary mode) and `mmm-delimiter-face' for region delimiters.
* High (2) means to use different faces for different types of submode
  regions and delimiters, such as initialization code, expressions that
  are output, declarations, and so on, as specified by the submode
  class.  The default faces are still used for regions that do not
  specify a face."
  :group 'mmm-faces
  :type '(choice (const :tag "None" 0)
                 (const :tag "Low" 1)
                 (const :tag "High" 2)))

(defface mmm-init-submode-face '((((background light)) (:background "Pink"))
				 (((background dark)) (:background "MediumOrchid"))
				 (t (:background "Pink")))
  "Face used for submodes containing initialization code."
  :group 'mmm-faces)

(defface mmm-cleanup-submode-face '((((background light)) (:background "Wheat"))
				    (((background dark)) (:background "peru"))
				    (t (:background "Wheat")))
  "Face used for submodes containing cleanup code."
  :group 'mmm-faces)

(defface mmm-declaration-submode-face '((((background light)) (:background "Aquamarine"))
					(((background dark)) (:background "DarkTurquoise"))
					(t (:background "Aquamarine")))
  "Face used for submodes containing declarations."
  :group 'mmm-faces)

(defface mmm-comment-submode-face '((((background light)) (:background "SkyBlue"))
				    (((background dark)) (:background "SteelBlue"))
				    (t (:background "SkyBlue")))
  "Face used for submodes containing comments and documentation."
  :group 'mmm-faces)

(defface mmm-output-submode-face '((((background light)) (:background "Plum"))
				    (((background dark)) (:background "MediumVioletRed"))
				    (t (:background "Plum")))
  "Face used for submodes containing expression that are output."
  :group 'mmm-faces)

(defface mmm-special-submode-face '((((background light)) (:background "MediumSpringGreen"))
				    (((background dark)) (:background "ForestGreen"))
				    (t (:background "MediumSpringGreen")))
  "Face used for special submodes not fitting any other category."
  :group 'mmm-faces)

(defface mmm-code-submode-face '((((background light)) (:background "LightGray"))
				 (((background dark)) (:background "DimGray"))
				 (t (:background "LightGray")))
  "Face used for submodes containing ordinary code."
  :group 'mmm-faces)

(defface mmm-default-submode-face '((((background light)) (:background "gray85"))
				    (((background dark)) (:background "gray20"))
				    (t (:background "gray85")))
  "Face used for all submodes at decoration level 1.
Also used at decoration level 2 for submodes not specifying a type."
  :group 'mmm-faces)

(defface mmm-delimiter-face nil
  "Face used to mark submode delimiters."
  :group 'mmm-faces)

;;}}}
;;{{{ Mode Line Format

(defcustom mmm-mode-string " MMM"
  "*String to display in mode line as MMM minor mode indicator."
  :group 'mmm
  :type 'string)

(defcustom mmm-submode-mode-line-format "~M[~m]"
  "*Format of the mode-line display when point is in a submode region.

~M is replaced by the name of the primary major mode \(which may be
replaced by a combined-mode function, see the info documentation).

~m is replaced by the submode region overlay's `display-name'
property, if it has one.  Otherwise it is replaced by the mode name of
the submode region.

If `mmm-primary-mode-display-name' is non-nil, then this variable is
used even when point is not in a submode region \(i.e. it is in a
primary mode region), with ~m being replaced by the value of that
variable."
  :group 'mmm
  :type 'string)

(defvar mmm-primary-mode-display-name nil
  "If non-nil, displayed as the primary mode name in the mode line.
See also `mmm-buffer-mode-display-name'.")
(make-variable-buffer-local 'mmm-primary-mode-display-name)

(defvar mmm-buffer-mode-display-name nil
  "If non-nil, displayed in the mode line instead of the primary mode
name, which is then shown next to it as if it were a submode when in a
primary mode region, i.e. outside all submode regions.")
(make-variable-buffer-local 'mmm-buffer-mode-display-name)

(defun mmm-set-mode-line ()
  "Set the mode line display correctly for the current submode,
according to `mmm-submode-mode-line-format'."
  (let ((primary (or mmm-primary-mode-display-name
		     (get mmm-primary-mode 'mmm-mode-name)))
	(submode (and mmm-current-overlay
		      (or (overlay-get mmm-current-overlay 'display-name)
			  (get mmm-current-submode 'mmm-mode-name)))))
    (if mmm-buffer-mode-display-name
	(setq mode-name
	      (mmm-format-string mmm-submode-mode-line-format
				 `(("~M" . ,mmm-buffer-mode-display-name)
				   ("~m" . ,(or submode primary)))))
      (if submode
	  (setq mode-name
		(mmm-format-string mmm-submode-mode-line-format
				   `(("~M" . ,primary)
				     ("~m" . ,submode))))
	(setq mode-name primary))))
  (force-mode-line-update))

;;}}}
;;{{{ Submode Classes

(defvar mmm-classes nil
  "*List of submode classes that apply to a buffer.
Generally set in a file local variables list.  Can either be one
symbol, or a list of symbols.  Automatically buffer-local.")
(make-variable-buffer-local 'mmm-classes)

(defvar mmm-global-classes '(universal)
  "*List of submode classes that apply to all buffers.
Can be overridden in a file local variables list.")

;;}}}
;;{{{ Modes and Extensions

(defcustom mmm-mode-ext-classes-alist nil
  "Alist of submode classes for major modes and/or file extensions.
This variable can now be directly modified.

Elements look like \(MODE EXT CLASS), where MODE is a major mode, EXT
is a regexp to match a filename such as in `auto-mode-alist', and
CLASS is a submode class. CLASS is activated in all buffers in mode
MODE \(if non-nil) and whose filenames match EXT \(if non-nil). If
both MODE and EXT are nil, CLASS is activated in all buffers. If CLASS
is the symbol t, MMM Mode is turned on in all buffers matching MODE
and EXT, but no classes are activated.

See `mmm-global-mode'."
  :group 'mmm
  :type '(repeat (list (symbol :tag "Major Mode")
                       (string :tag "Filename Regexp")
                       (symbol :tag "Class")))
  :require 'mmm-mode)

(defun mmm-add-mode-ext-class (mode ext class)
  "Add an element to `mmm-mode-ext-classes-alist', which see.
That variable can now be directly modified, so this function is
unnecessary. It probably won't go away, though."
  (add-to-list 'mmm-mode-ext-classes-alist (list mode ext class)))

;;}}}
;;{{{ Preferred Major Modes

(defcustom mmm-major-mode-preferences
  '((perl cperl-mode perl-mode)
    (python python-mode python-mode)
    (javascript javascript-mode c++-mode)
    (java jde-mode java-mode c++-mode)
    (css css-mode c++-mode))
  "User preferences about what major modes to use.
Each element has the form \(LANGUAGE . MODES) where LANGUAGE is the
name of a programming language such as `perl' as a symbol, and MODES
is a list of possible major modes to use, such as `cperl-mode' or
`perl-mode'.  The first element of MODES which is `fboundp' is used
for submodes of LANGUAGE.  The last element of MODES should be a mode
which will always be available."
  :group 'mmm
  :type '(repeat (cons symbol
                       (repeat
                        (restricted-sexp :match-alternatives
                                         (fboundp))))))

(defun mmm-add-to-major-mode-preferences (language mode &optional default)
  "Add major mode MODE as acceptable for LANGUAGE.
This sets the value of `mmm-major-mode-preferences'. If DEFAULT
is non-nil, MODE is added at the front of the list of modes for
LANGUAGE. Otherwise, it is added at the end. This may be used by
packages to ensure that some mode is present, but not override
any user-specified mode."
  (let ((pair (assq language mmm-major-mode-preferences)))
    (if pair
        ;; Existing mode preferences
        (if default
            (setcdr pair (cons mode (cdr pair)))
          (setcdr pair (append (cdr pair) (list mode))))
      ;; No existing mode preference
      (add-to-list 'mmm-major-mode-preferences (list language mode)))))

(defun mmm-ensure-modename (symbol)
  "Return SYMBOL if it is a valid submode name, else nil.
Valid submode names are either `fboundp' or present as the `car' of an
element in `mmm-major-mode-preferences'."
  (if (or (fboundp symbol)
          (assq symbol mmm-major-mode-preferences))
      symbol
    nil))

(defun mmm-modename->function (mode)
  "Convert MODE to a mode function, nil if impossible.
Valid submode names are either `fboundp' or present as the `car' of an
element in `mmm-major-mode-preferences'.  In the latter case, the
first `fboundp' element of the `cdr' is returned, or nil if none."
  (if (fboundp mode)
      mode
    (car (remove-if-not
          #'fboundp
          (cdr (assq mode mmm-major-mode-preferences))))))

;;}}}
;;{{{ Delimiter Regions

(defcustom mmm-delimiter-mode 'fundamental-mode
  "Major mode used by default for delimiter regions.
Classes are encouraged to override this by providing a delimiter-mode
parameter-- see `mmm-classes-alist'."
  :group 'mmm
  :type 'function)

;;}}}
;;{{{ Key Bindings

(defcustom mmm-mode-prefix-key [(control ?c) ?%]
  "Prefix key for the MMM Minor Mode Keymap."
  :group 'mmm
  :type 'vector)

(defcustom mmm-command-modifiers '(control)
  "List of key modifiers for MMM command keys.
The MMM commands in the MMM Mode map, after `mmm-mode-prefix-key',
are bound to default keys with these modifiers added. This variable
must be set before MMM Mode is loaded to have an effect.

It is suggested that the value of this variable be either nil or
\(control), as the default keys are either plain keys or have only a
meta modifier. The shift modifier is not particularly portable between
Emacsen. The values of this variable and `mmm-insert-modifiers' should
be disjoint."
  :group 'mmm
  :type '(repeat (symbol :tag "Modifier")))

(defcustom mmm-insert-modifiers '()
  "List of key modifiers for MMM submode insertion keys.
When a key pressed after `mmm-mode-prefix-key' has no MMM Mode command
binding, and its modifiers include these, then its basic type, plus any
modifiers in addition to these, is looked up in classes' :insert
specifications.

It is suggested that the value of this variable be either nil or
\(control), allowing submode classes to specify the presence or
absence of the meta modifier. The shift modifier is not particularly
portable between Emacsen. The values of `mmm-command-modifiers' and
this variable should be disjoint."
  :group 'mmm
  :type '(repeat (symbol :tag "Modifier")))

(defcustom mmm-use-old-command-keys nil
  "Non-nil means to Use the old command keys for MMM Mode.
MMM Mode commands then have no modifier while insertion commands have
a control modifier, i.e. `mmm-command-modifiers' is set to nil and
`mmm-insert-modifiers' is set to \(control). If nil, the values of
these variables are as the default, or whatever the user has set them
to. This variable must be set before MMM Mode is loaded."
  :group 'mmm
  :type 'boolean)

(defun mmm-use-old-command-keys ()
  "Use the old command keys \(no control modifer) in MMM Mode."
  (setq mmm-command-modifiers '()
        mmm-insert-modifiers '(control)))

;;}}}
;;{{{ MMM Hooks

(defcustom mmm-mode-hook ()
  "Hook run when MMM Mode is enabled in a buffer.

A hook named mmm-<major-mode>-hook is also run, if it exists. For
example, `mmm-html-mode-hook' is run whenever MMM Mode is entered with
HTML mode the dominant mode.

A hook named mmm-<submode>-submode-hook is run when a submode region
of a given mode is created. For example, `mmm-cperl-mode-submode-hook'
is run whenever a CPerl mode submode region is created, in any buffer.
When this hooks are run, point is guaranteed to be at the start of
the newly created submode region.

Finally, a hook named mmm-<class>-class-hook is run whenever a buffer
is first mmm-ified with a given submode class. For example,
`mmm-mason-class-hook' is run whenever the `mason' class is first
applied in a buffer."
  :group 'mmm
  :type 'hook)

(defun mmm-run-constructed-hook (body &optional suffix)
  "Run the hook named `mmm-<BODY>-<SUFFIX>-hook', if it exists.
If SUFFIX is nil or unsupplied, run `mmm-<BODY>-hook' instead."
  (let ((hook (intern-soft (if suffix
                               (format "mmm-%s-%s-hook" body suffix)
                             (format "mmm-%s-hook" body)))))
    (if hook (run-hooks hook))))

(defun mmm-run-major-hook ()
  (mmm-run-constructed-hook mmm-primary-mode))

(defun mmm-run-submode-hook (submode)
  (mmm-run-constructed-hook submode "submode"))

(defvar mmm-class-hooks-run ()
  "List of submode classes for which hooks have already been run in
the current buffer.")
(make-variable-buffer-local 'mmm-class-hooks-run)

(defun mmm-run-class-hook (class)
  (unless (member class mmm-class-hooks-run)
    (mmm-run-constructed-hook class "class")
    (add-to-list 'mmm-class-hooks-run class)))

(defvar mmm-primary-mode-entry-hook nil
  "Hook run when point moves into a region of the primary mode.
Each submode region can have an `entry-hook' property which is run
when they are entered, but since primary mode regions have no overlay
to store properties, this is a buffer-local variable.

N.B. This variable is not a standard Emacs hook.  Unlike Emacs'
\"local hooks\" it has *no* global value, only a local one.  Its value
should always be a list of functions \(possibly empty) and never a
single function.  It may be used with `add-hook', however.")
(make-variable-buffer-local 'mmm-primary-mode-entry-hook)

;;}}}
;;{{{ Major Mode Hook

(defcustom mmm-major-mode-hook ()
  "Hook run whenever a new major mode is finished starting up.
MMM Mode implements this with a hack \(see comments in the source) so
that `mmm-global-mode' will function correctly, but makes this hook
available so that others can take advantage of the hack as well.

Note that file local variables have *not* been processed by the time
this hook is run. If a function needs to inspect them, it should also
be added to `find-file-hooks'. However, `find-file-hooks' is not run
when creating a non-file-based buffer, or when changing major modes in
an existing buffer."
  :group 'mmm
  :type 'hook)

(defun mmm-run-major-mode-hook ()
  (dolist (func mmm-major-mode-hook)
    (ignore-errors (funcall func))))

;;}}}
;;{{{ MMM Global Mode

;;; There's a point to be made that this variable should default to
;;; `maybe' (i.e. not nil and not t), because that's what practically
;;; everyone wants.  I subscribe, however, to the view that simply
;;; *loading* a lisp extension should not change the (user-visible)
;;; behavior of Emacs, until it is configured or turned on in some
;;; way, which dictates that the default for this must be nil.
(defcustom mmm-global-mode nil
  "*Specify in which buffers to turn on MMM Mode automatically.

- If nil, MMM Mode is never enabled automatically.
- If t, MMM Mode is enabled automatically in all buffers.
- If any other symbol, MMM mode is enabled only in those buffers that
  have submode classes associated with them. See `mmm-classes' and
  `mmm-mode-ext-classes-alist' for more information."
  :group 'mmm
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (other :tag "Maybe" maybe))
  :require 'mmm-mode)

;; These are not traditional editing modes, so mmm makes no sense, and
;; can mess things up seriously if it doesn't know not to try.
(defcustom mmm-never-modes
  '(
    help-mode
    Info-mode
    dired-mode
    comint-mode
    telnet-mode
    shell-mode
    eshell-mode
    forms-mode
    )
  "List of modes in which MMM Mode is never activated."
  :group 'mmm
  :type '(repeat (symbol :tag "Mode")))

;;}}}
;;{{{ Buffer File Name

(defvar mmm-set-file-name-for-modes '(mew-draft-mode)
  "List of modes for which the temporary buffers MMM creates have a
file name.  In these modes, this file name is the same as that of the
parent buffer.  In general, this has been found to cause more problems
than it solves, but some modes require it.")

;;}}}
;;{{{ Idle Parsing

(defcustom mmm-parse-when-idle nil
  "Non-nil to automatically reparse the buffer when it has some
  modifications and Emacs has been idle for `mmm-idle-timer-delay'."
  :type 'boolean
  :group 'mmm)

(defcustom mmm-idle-timer-delay 0.2
  "Delay in secs before re-parsing after user makes changes."
  :type 'number
  :group 'mmm)
(make-variable-buffer-local 'mmm-idle-timer-delay)

(defvar mmm-mode-parse-timer nil "Private variable.")
(make-variable-buffer-local 'mmm-mode-parse-timer)
(defvar mmm-mode-buffer-dirty nil "Private variable.")
(make-variable-buffer-local 'mmm-mode-buffer-dirty)

(defun mmm-mode-edit (beg end len)
  (setq mmm-mode-buffer-dirty t)
  (mmm-mode-reset-timer))

(defun mmm-mode-reset-timer ()
  (when mmm-mode-parse-timer
    (cancel-timer mmm-mode-parse-timer))
  (setq mmm-mode-parse-timer
        (run-with-idle-timer mmm-idle-timer-delay nil
                             #'mmm-mode-idle-reparse (current-buffer))))

(defun mmm-mode-idle-reparse (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when mmm-mode-buffer-dirty
        (mmm-apply-all)
        (setq mmm-mode-buffer-dirty nil)
        (setq mmm-mode-parse-timer nil)))))

;;}}}

;; NON-USER VARIABLES
;;{{{ Mode Variable

(defvar mmm-mode nil
  "Non-nil means MMM Mode is turned on in this buffer.
Do not set this variable directly; use the function `mmm-mode'.")
(make-variable-buffer-local 'mmm-mode)

;;}}}
;;{{{ Primary Mode

(defvar mmm-primary-mode nil
  "The primary major mode in the current buffer.")
(make-variable-buffer-local 'mmm-primary-mode)

;;}}}
;;{{{ Classes Alist

;; Notes:
;; 1. :parent could be an all-class argument.  Same with :keymap.
;; 2. :match-submode really does have to be distinct from :submode,
;; because 'functionp' isn't enough to distinguish which is meant.
(defvar mmm-classes-alist nil
  "Alist containing all defined mmm submode classes.
A submode class is a named recipe for parsing a document into submode
regions, and sometimes for inserting new ones while editing.

Each element of this alist looks like \(CLASS . ARGS) where CLASS is a
symbol naming the submode class and ARGS is a list of keyword
arguments, called a \"class specifier\". There are a large number of
accepted keyword arguments in the class specifier.

The argument CLASSES, if supplied, must be a list of other submode
class names, or class specifiers, representing other classes to call
recursively.  The FACE arguments of these classes are overridden by
the FACE argument of this class.  If the argument CLASSES is supplied,
all other arguments to this class are ignored.  That is, \"grouping\"
classes can do nothing but group other classes.

The argument HANDLER, if supplied, also overrides any other processing.
It must be a function, and all the arguments are passed to it as
keywords, and it must do everything. See `mmm-ify' for what sorts of
things it must do. This back-door interface should be cleaned up.

The optional argument FACE gives the display face of the submode
regions under high decoration (see `mmm-submode-decoration-level').
It must be a valid face.  The standard faces used for submode regions
are `mmm-*-submode-face' where * is one of `init', `cleanup',
`declaration', `comment', `output', `special', or `code'.  A more
flexible alternative is the argument MATCH-FACE.  MATCH-FACE can be a
function, which is called with one argument, the form of the front
delimiter \(found from FRONT-FORM, below), and should return the face
to use.  It can also be an alist, with each element of the form
\(DELIM . FACE).

If neither CLASSES nor HANDLER are supplied, either SUBMODE or
MATCH-SUBMODE must be.  SUBMODE specifies the submode to use for the
submode regions, a symbol such as `cperl-mode' or `emacs-lisp-mode',
while MATCH-SUBMODE must be a function to be called immediately after
a match is found for FRONT, which is passed one argument, the form of
the front delimiter \(found from FRONT-FORM, below), and return a
symbol such as SUBMODE would be set to.  If MATCH-SUBMODE detects an
invalid match--for example a specified mode which is not `fboundp'--it
should \(signal 'mmm-no-matching-submode nil).

FRONT and BACK are the means to find the submode regions, and can be
either buffer positions \(number-or-markers), regular expressions, or
functions. If they are absolute buffer positions, only one submode
region is created, from FRONT to BACK. This is generally not used in
named classes. \(Unnamed classes are created by interactive commands
in `mmm-interactive-history').

If FRONT is a regexp, then that regexp is searched for, and the end of
its FRONT-MATCH'th match \(or the beginning thereof, if INCLUDE-FRONT
is non-nil), plus FRONT-OFFSET, becomes the beginning of the submode
region.  If FRONT is a function, that function is called instead, and
must act somewhat like a search, in that it should start at point,
take one argument as a search bound, and set the match data.  A
similar pattern is followed for BACK \(the search starts at the
beginning of the submode region), save that the beginning of its
BACK-MATCH'th match \(or the end, if INCLUDE-BACK is non-nil) becomes
the end of the submode region, plus BACK-OFFSET.

If SAVE-MATCHES is non-nil, then BACK, if it is a regexp, is formatted
by replacing strings of the form \"~N\" by the corresponding value of
\(match-string n) after matching FRONT.

FRONT-MATCH and BACK-MATCH default to zero.  They specify which
sub-match of the FRONT and BACK regexps to treat as the delimiter.
This number will be passed to any calls to `match-beginning' and
company.

FRONT- and BACK-OFFSET default to 0.  In addition to numbers, they can
also be functions to call which should move point to the correct
position for the beginning or end of the submode region.  Common
choices include `beginning-of-line' and `end-of-line', and new
functions can of course be written.  They can also be lists which will
be applied in sequence, such as \(end-of-line 1) meaning move to end
of line and then forward one character.

FRONT-VERIFY and BACK-VERIFY, if supplied, must be functions that
inspect the match data to see if a match found by FRONT or BACK
respectively is valid.

FRONT-DELIM \(resp. BACK-DELIM), if supplied, can take values like
those of FRONT-OFFSET \(resp. BACK-OFFSET), specifying the offset from
the start \(resp. end) of the match for FRONT \(resp. BACK) to use as
the starting \(resp. ending) point for the front \(resp. back)
delimiter.  If nil, it means not to make a region for the respective
delimiter at all.

DELIMITER-MODE, if supplied, specifies what submode to use for the
delimiter regions, if any.  If `nil', the primary mode is used.  If
not supplied, `mmm-delimiter-mode' is used.

FRONT-FACE and BACK-FACE specify faces to use for displaying the
delimiter regions, under high decoration.

FRONT-FORM and BACK-FORM, if given, must supply a regexp used to match
the *actual* delimiter.  If they are strings, they are used as-is.  If
they are functions, they are called and must inspect the match data.
If they are lists, their `car' is taken as the delimiter.  The default
for both is \(regexp-quote \(match-string 0)).

The last case--them being a list--is usually used to set the delimiter
to a function.  Such a function must take 1-2 arguments, the first
being the overlay in question, and the second meaning to insert the
delimiter and adjust the overlay rather than just matching the
delimiter.  See `mmm-match-front', `mmm-match-back', and
`mmm-end-current-region'.

CASE-FOLD-SEARCH, if specified, controls whether the search is
case-insensitive. See `case-fold-search'. It defaults to `t'.

CREATION-HOOK, if specified, should be a function which is run
whenever a submode region is created, with point at the beginning of
the new region.  One use for it is to set region-saved local variables
\(see `mmm-save-local-variables').

INSERT specifies the keypress insertion spec for such submode regions.
INSERT's value should be list of elements of the form \(KEY NAME .
SPEC). Each KEY should be either a character, a function key symbol,
or a dotted list \(MOD . KEY) where MOD is a symbol for a modifier
key. The use of any other modifier than meta is discouraged, as
`mmm-insert-modifiers' is sometimes set to \(control), and other
modifiers are not very portable. Each NAME should be a symbol
representing the insertion for that key. Each SPEC can be either a
skeleton, suitable for passing to `skeleton-insert' to create a
submode region, or a dotted pair \(OTHER-KEY . ARG) meaning to use the
skeleton defined for OTHER-KEY but pass it the argument ARG as the
`str' variable, possible replacing a prompt string. Skeletons for
insertion should have the symbol `_' where point \(or wrapped text)
should go, and the symbol `@' in four different places: at the
beginning of the front delimiter, the beginning of the submode region,
the end of the submode region, and the end of the back delimiter.

If END-NOT-BEGIN is non-nil, it specifies that a BACK delimiter cannot
begin a new submode region.

MATCH-NAME, if supplied, specifies how to determine the \"name\" for
each submode region.  It must be a string or a function.  If it is a
function, it is passed the value of FRONT-FORM and must return the
name to use.  If it is a string, it is used as-is unless SAVE-NAME has
a non-nil value, in which case, the string is interpreted the same as
BACK when SAVE-MATCHES is non-nil.  If MATCH-NAME is not specified,
the regions are unnamed.  Regions with the same name are considered
part of the same chunk of code, and formatted as such, while unnamed
regions are not grouped with any others.

As a special optimization for insertion, if SKEL-NAME is non-nil, the
insertion code will use the user-prompted string value as the region
name, instead of going through the normal matching procedure.

PRIVATE, if supplied and non-nil, means that this class is a private
or internal class, usually one invoked by another class via :classes,
and is not for the user to see.")

;;;###autoload
(defun mmm-add-classes (classes)
  "Add the submode classes CLASSES to `mmm-classes-alist'."
  (dolist (class classes)
    (add-to-list 'mmm-classes-alist class)))

(defun mmm-add-group (group classes)
  "Add CLASSES and a \"grouping class\" named GROUP which calls them all.
The CLASSES are all made private, i.e. non-user-visible."
  (mmm-add-classes (mapcar #'(lambda (class)
                               (append class
                                       '(:private t)))
                           classes))
  (add-to-list 'mmm-classes-alist
               (list group :classes (mapcar #'first classes))))

(defun mmm-add-to-group (group classes)
  "Add CLASSES to the \"grouping class\" named GROUP.
The CLASSES are all made private, i.e. non-user-visible."
  (mmm-add-classes (mapcar #'(lambda (class)
                               (append class
                                       '(:private t)))
                           classes))
  (mmm-set-class-parameter group :classes
			   (append  (mmm-get-class-parameter group :classes)
				    (mapcar #'first classes))))

;;}}}
;;{{{ Version Number

(defconst mmm-version "0.5.4"
  "Current version of MMM Mode.")

(defun mmm-version ()
  (interactive)
  (message "MMM Mode version %s by Michael Abraham Shulman" mmm-version))

;;}}}
;;{{{ Temp Buffer Name

(defvar mmm-temp-buffer-name "mmm-temp-buffer"
  "Name for temporary buffers created by MMM Mode.
Using non-special name, so that font-lock-mode will be enabled
automatically when appropriate, and will set all related vars.")

(defvar mmm-in-temp-buffer nil
  "Bound to t when working in the temp buffer.")

;;}}}
;;{{{ Interactive History

(defvar mmm-interactive-history nil
  "History of interactive mmm-ification in the current buffer.
Elements are either submode class symbols or class specifications. See
`mmm-classes-alist' for more information.")
(make-variable-buffer-local 'mmm-interactive-history)

(defun mmm-add-to-history (class)
  (add-to-list 'mmm-interactive-history class))

(defun mmm-clear-history ()
  "Clears history of interactive mmm-ification in current buffer."
  (interactive)
  (setq mmm-interactive-history nil))

;;}}}
;;{{{ Mode/Ext Manipulation

(defvar mmm-mode-ext-classes ()
  "List of classes associated with current buffer by mode and filename.
Set automatically from `mmm-mode-ext-classes-alist'.")
(make-variable-buffer-local 'mmm-mode-ext-classes)

(defun mmm-get-mode-ext-classes ()
  "Return classes for current buffer from major mode and filename.
Uses `mmm-mode-ext-classes-alist' to find submode classes."
  (or mmm-mode-ext-classes
      (setq mmm-mode-ext-classes
            (mapcar #'third
                    (remove-if-not #'mmm-mode-ext-applies
                                   mmm-mode-ext-classes-alist)))))

(defun mmm-clear-mode-ext-classes ()
  "Clear classes added by major mode and filename."
  (setq mmm-mode-ext-classes nil))

(defun mmm-mode-ext-applies (element)
  (destructuring-bind (mode ext class) element
    (and (if mode
             (eq mode
                 ;; If MMM is on in this buffer, use the primary mode,
                 ;; otherwise use the normal indicator.
                 (or mmm-primary-mode major-mode))
           t)
         (if ext
             (and (buffer-file-name)
                  (save-match-data
                    (string-match ext (buffer-file-name))))
           t))))

(defun mmm-get-all-classes (global)
  "Return a list of all classes applicable to the current buffer.
These come from mode/ext associations, `mmm-classes', and interactive
history, as well as `mmm-global-classes' if GLOBAL is non-nil."
  (append mmm-interactive-history
          (if (listp mmm-classes) mmm-classes (list mmm-classes))
          (if global mmm-global-classes ())
          (mmm-get-mode-ext-classes)))

;;}}}

(provide 'mmm-vars)

;;; mmm-vars.el ends here
