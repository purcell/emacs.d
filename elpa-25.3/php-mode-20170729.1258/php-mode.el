;;; php-mode.el --- Major mode for editing PHP code

;; Copyright (C) 1999, 2000, 2001, 2003, 2004 Turadg Aleahmad
;;               2008 Aaron S. Hawley
;;               2011, 2012, 2013, 2014, 2015, 2016, 2017 Eric James Michael Ritz

;; Author: Eric James Michael Ritz
;; URL: https://github.com/ejmr/php-mode
;; Version: 1.18.3
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

(defconst php-mode-version-number "1.18.3"
  "PHP Mode version number.")

(defconst php-mode-modified "2017-06-22"
  "PHP Mode build date.")

;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Usage

;; Put this file in your Emacs lisp path (eg. site-lisp) and add to
;; your .emacs file:
;;
;;   (require 'php-mode)

;; To use abbrev-mode, add lines like this:
;;   (add-hook 'php-mode-hook
;;     '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))

;; To make php-mode compatible with html-mode, see http://php-mode.sf.net

;; Many options available under Help:Customize
;; Options specific to php-mode are in
;;  Programming/Languages/Php
;; Since it inherits much functionality from c-mode, look there too
;;  Programming/Languages/C

;;; Commentary:

;; PHP Mode is a major mode for editing PHP source code.  It's an
;; extension of C mode; thus it inherits all C mode's navigation
;; functionality.  But it colors according to the PHP grammar and
;; indents according to the PEAR coding guidelines.  It also includes
;; a couple handy IDE-type features such as documentation search and a
;; source and class browser.

;;; Code:

(require 'cc-mode)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

;; Boilerplate from other `cc-mode' derived modes. See
;; http://cc-mode.sourceforge.net/derived-mode-ex.el for details on how this all
;; fits together.
(eval-and-compile
  (c-add-language 'php-mode 'java-mode))

(require 'font-lock)
(require 'add-log)
(require 'custom)
(require 'flymake)
(require 'etags)
(require 'speedbar)

(require 'cl-lib)

(eval-when-compile
  (require 'regexp-opt)
  (defvar c-vsemi-status-unknown-p)
  (defvar syntax-propertize-via-font-lock))

;; Work around emacs bug#18845, cc-mode expects cl to be loaded
;; while php-mode only uses cl-lib (without compatibility aliases)
(eval-and-compile
  (if (and (= emacs-major-version 24) (>= emacs-minor-version 4))
    (require 'cl)))

;; Work around https://github.com/ejmr/php-mode/issues/310.
;;
;; In emacs 24.4 and 24.5, lines after functions with a return type
;; are incorrectly analyzed as member-init-cont.
;;
;; Before emacs 24.4, c member initializers are not supported this
;; way. Starting from emacs 25.1, cc-mode only detects member
;; initializers when the major mode is c++-mode.
(eval-and-compile
  (if (and (= emacs-major-version 24) (or (= emacs-minor-version 4)
                                          (= emacs-minor-version 5)))
      (defun c-back-over-member-initializers ()
        ;; Override of cc-engine.el, cc-mode in emacs 24.4 and 24.5 are too
        ;; optimistic in recognizing c member initializers. Since we don't
        ;; need it in php-mode, just return nil.
        nil)))


;; Local variables
;;;###autoload
(defgroup php nil
  "Major mode `php-mode' for editing PHP code."
  :tag "PHP"
  :prefix "php-"
  :group 'languages
  :link '(url-link :tag "Official Site" "https://github.com/ejmr/php-mode")
  :link '(url-link :tag "PHP Mode Wiki" "https://github.com/ejmr/php-mode/wiki"))

(defcustom php-executable (or (executable-find "php")
                              "/usr/bin/php")
  "The location of the PHP executable."
  :type 'string)

(defcustom php-default-face 'default
  "Default face in `php-mode' buffers."
  :type 'face)

(defcustom php-speedbar-config t
  "When set to true automatically configures Speedbar to observe PHP files.
Ignores php-file patterns option; fixed to expression \"\\.\\(inc\\|php[s345]?\\)\""
  :type 'boolean
  :set (lambda (sym val)
         (set-default sym val)
         (when val
             (speedbar-add-supported-extension
              "\\.\\(inc\\|php[s345]?\\|phtml\\)"))))

(defcustom php-mode-speedbar-open nil
  "Normally `php-mode' starts with the speedbar closed.
Turning this on will open it whenever `php-mode' is loaded."
  :type 'boolean
  :set (lambda (sym val)
         (set-default sym val)
         (when val
             (speedbar 1))))

(defcustom php-template-compatibility t
  "Should detect presence of html tags."
  :type 'boolean)

(defsubst php-in-string-p ()
  (nth 3 (syntax-ppss)))

(defsubst php-in-comment-p ()
  (nth 4 (syntax-ppss)))

(defsubst php-in-string-or-comment-p ()
  (nth 8 (syntax-ppss)))

(defun php-mode-extra-constants-create-regexp(kwds)
  "Create regexp for the list of extra constant keywords KWDS."
   (concat "[^_$]?\\<\\("
           (regexp-opt
             (append kwds
                     (when (boundp 'web-mode-extra-php-constants) web-mode-extra-php-constants)))
           "\\)\\>[^_]?"))

(defun php-mode-extra-constants-set(sym value)
  "Apply the list of extra constant keywords VALUE.

This function is called when the custom variable php-extra-constants
is updated. The web-mode-extra-constants list is appended to the list
of constants when set."
  ;; remove old keywords
  (when (boundp 'php-extra-constants)
    (font-lock-remove-keywords
     'php-mode `((,(php-mode-extra-constants-create-regexp php-extra-constants) 1 'php-constant))))
  ;; add new keywords
  (when value
    (font-lock-add-keywords
     'php-mode `((,(php-mode-extra-constants-create-regexp value) 1 'php-constant))))
  (set sym value))

(defcustom php-lineup-cascaded-calls nil
  "Indent chained method calls to the previous line"
  :type 'boolean)

;;;###autoload
(defcustom php-extra-constants '()
  "A list of additional strings to treat as PHP constants."
  :type 'list
  :set 'php-mode-extra-constants-set)

(defun php-create-regexp-for-method (visibility)
  "Make a regular expression for methods with the given VISIBILITY.

VISIBILITY must be a string that names the visibility for a PHP
method, e.g. 'public'.  The parameter VISIBILITY can itself also
be a regular expression.

The regular expression this function returns will check for other
keywords that can appear in method signatures, e.g. 'final' and
'static'.  The regular expression will have one capture group
which will be the name of the method."
  (concat
   ;; Initial space with possible 'abstract' or 'final' keywords
   "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?"
   ;; 'static' keyword may come either before or after visibility
   "\\(?:" visibility "\\(?:\\s-+static\\)?\\|\\(?:static\\s-+\\)?" visibility "\\)\\s-+"
   ;; Make sure 'function' comes next with some space after
   "function\\s-+"
   ;; Capture the name as the first group and the regexp and make sure
   ;; by the end we see the opening parenthesis for the parameters.
   "\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*("))

(defun php-create-regexp-for-classlike (type)
  "Accepts a `type' of a 'classlike' object as a string, such as
'class' or 'interface', and returns a regexp as a string which
can be used to match against definitions for that classlike."
  (concat
   ;; First see if 'abstract' or 'final' appear, although really these
   ;; are not valid for all values of `type' that the function
   ;; accepts.
   "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?"
   ;; The classlike type
   type
   ;; Its name, which is the first captured group in the regexp.  We
   ;; allow backslashes in the name to handle namespaces, but again
   ;; this is not necessarily correct for all values of `type'.
   "\\s-+\\(\\(?:\\sw\\|\\\\\\|\\s_\\)+\\)"))

(defvar php-imenu-generic-expression
  `(("Namespaces"
    ,(php-create-regexp-for-classlike "namespace") 1)
   ("Classes"
    ,(php-create-regexp-for-classlike "class") 1)
   ("Interfaces"
    ,(php-create-regexp-for-classlike "interface") 1)
   ("Traits"
    ,(php-create-regexp-for-classlike "trait") 1)
   ("All Methods"
    ,(php-create-regexp-for-method "\\(?:\\sw\\|\\s_\\)+") 1)
   ("Private Methods"
    ,(php-create-regexp-for-method "private") 1)
   ("Protected Methods"
    ,(php-create-regexp-for-method "protected")  1)
   ("Public Methods"
    ,(php-create-regexp-for-method "public") 1)
   ("Anonymous Functions"
    "\\<\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*=\\s-*function\\s-*(" 1)
   ("Named Functions"
    "^\\s-*function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1))
 "Imenu generic expression for PHP Mode. See `imenu-generic-expression'.")

(defcustom php-site-url "http://php.net/"
  "Default PHP.net site URL.

The URL to use open PHP manual and search word.
You can find a mirror site closer to you."
  :type 'string
  :link '(url-link :tag "List of Mirror Sites" "http://php.net/mirrors.php"))

(defcustom php-manual-url 'en
  "URL at which to find PHP manual.
You can replace \"en\" with your ISO language code."
  :type '(choice (const  :tag "English" 'en)
                 (const  :tag "Brazilian Portuguese" 'pt_BR)
                 (const  :tag "Chinese (Simplified)" 'zh)
                 (const  :tag "French" 'fr)
                 (const  :tag "German" 'de)
                 (const  :tag "Japanese" 'ja)
                 (const  :tag "Romanian" 'ro)
                 (const  :tag "Russian" 'ru)
                 (const  :tag "Spanish" 'es)
                 (const  :tag "Turkish" 'tr)
                 (string :tag "PHP manual URL")))

(defcustom php-search-url nil
  "URL at which to search for documentation on a word."
  :type '(choice (string :tag "URL to search PHP documentation")
                 (const  :tag "Use `php-site-url' variable" nil)))

(defcustom php-completion-file ""
  "Path to the file which contains the function names known to PHP."
  :type 'string)

(defcustom php-manual-path ""
  "Path to the directory which contains the PHP manual."
  :type 'string)

;;;###autoload
(add-to-list 'interpreter-mode-alist
             ;; Match php, php-3, php5, php7, php5.5, php-7.0.1, etc.
             (cons "php\\(?:-?[3457]\\(?:\\.[0-9]+\\)*\\)?" 'php-mode))

(defcustom php-mode-hook nil
  "List of functions to be executed on entry to `php-mode'."
  :type 'hook)

(defcustom php-mode-pear-hook nil
  "Hook called when a PHP PEAR file is opened with `php-mode'."
  :type 'hook)

(defcustom php-mode-drupal-hook nil
  "Hook called when a Drupal file is opened with `php-mode'."
  :type 'hook)

(defcustom php-mode-wordpress-hook nil
  "Hook called when a WordPress file is opened with `php-mode'."
  :type 'hook)

(defcustom php-mode-symfony2-hook nil
  "Hook called when a Symfony2 file is opened with `php-mode'."
  :type 'hook)

(defcustom php-mode-psr2-hook nil
  "Hook called when a PSR-2 file is opened with `php-mode'."
  :type 'hook)

(defcustom php-mode-force-pear nil
  "Normally PEAR coding rules are enforced only when the filename contains \"PEAR.\"
Turning this on will force PEAR rules on all PHP files."
  :type 'boolean)

(defcustom php-mode-warn-if-mumamo-off t
  "Warn once per buffer if you try to indent a buffer without
mumamo-mode turned on. Detects if there are any HTML tags in the
buffer before warning, but this is is not very smart; e.g. if you
have any tags inside a PHP string, it will be fooled."
  :type '(choice (const :tag "Warg" t) (const "Don't warn" nil)))

(defcustom php-mode-coding-style 'pear
  "Select default coding style to use with php-mode.
This variable can take one of the following symbol values:

`Default' - use a reasonable default style for PHP.

`PEAR' - use coding styles preferred for PEAR code and modules.

`Drupal' - use coding styles preferred for working with Drupal projects.

`WordPress' - use coding styles preferred for working with WordPress projects.

`Symfony2' - use coding styles preferred for working with Symfony2 projects.

`PSR-2' - use coding styles preferred for working with projects using PSR-2 standards."
  :type '(choice (const :tag "Default" default)
                 (const :tag "PEAR" pear)
                 (const :tag "Drupal" drupal)
                 (const :tag "WordPress" wordpress)
                 (const :tag "Symfony2" symfony2)
                 (const :tag "PSR-2" psr2))
  :set 'php-mode-custom-coding-style-set
  :initialize 'custom-initialize-default)

(defun php-mode-custom-coding-style-set (sym value)
  (when (eq major-mode 'php-mode)
    (set         sym value)
    (set-default sym value)
    (cond ((eq value 'pear)
           (php-enable-pear-coding-style))
          ((eq value 'default)
           (php-enable-default-coding-style))
          ((eq value 'drupal)
           (php-enable-drupal-coding-style))
          ((eq value 'wordpress)
           (php-enable-wordpress-coding-style))
          ((eq value 'symfony2)
           (php-enable-symfony2-coding-style))
          ((eq value 'psr2)
           (php-enable-psr2-coding-style)))))

(defun php-mode-version ()
  "Display string describing the version of PHP Mode."
  (interactive)
  (message "PHP Mode %s of %s"
           php-mode-version-number php-mode-modified))

(defvar php-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map [menu-bar php]
    ;;   (cons "PHP" (make-sparse-keymap "PHP")))

    ;; (define-key map [menu-bar php complete-function]
    ;;   '("Complete function name" . php-complete-function))
    ;; (define-key map [menu-bar php browse-manual]
    ;;   '("Browse manual" . php-browse-manual))
    ;; (define-key map [menu-bar php search-documentation]
    ;;   '("Search documentation" . php-search-documentation))

    ;; By default PHP Mode binds C-M-h to c-mark-function, which it
    ;; inherits from cc-mode.  But there are situations where
    ;; c-mark-function fails to properly mark a function.  For
    ;; example, if we use c-mark-function within a method definition
    ;; then the region will expand beyond the method and into the
    ;; class definition itself.
    ;;
    ;; Changing the default to mark-defun provides behavior that users
    ;; are more likely to expect.
    (define-key map (kbd "C-M-h") 'mark-defun)

    ;; Many packages based on cc-mode provide the 'C-c C-w' binding
    ;; to toggle Subword Mode.  See the page
    ;;
    ;;     https://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html
    ;;
    ;; for more information about Submode Word.
    (if (boundp 'subword-mode)
        (if subword-mode
            (subword-mode nil)
          (subword-mode t)))

    ;; We inherit c-beginning-of-defun and c-end-of-defun from CC Mode
    ;; but we have two replacement functions specifically for PHP.  We
    ;; remap the commands themselves and not their default
    ;; key-bindings so that our PHP-specific versions will work even
    ;; if the user has reconfigured their keys, e.g. if they rebind
    ;; c-end-of-defun to something other than C-M-e.
    (define-key map [remap c-beginning-of-defun] 'php-beginning-of-defun)
    (define-key map [remap c-end-of-defun] 'php-end-of-defun)

    (define-key map [(control c) (control f)] 'php-search-documentation)
    (define-key map [(meta tab)] 'php-complete-function)
    (define-key map [(control c) (control m)] 'php-browse-manual)
    (define-key map [(control .)] 'php-show-arglist)
    (define-key map [(control c) (control r)] 'php-send-region)
    ;; Use the Emacs standard indentation binding. This may upset c-mode
    ;; which does not follow this at the moment, but I see no better
    ;; choice.
    (define-key map [tab] 'indent-for-tab-command)
    map)
  "Keymap for `php-mode'")

(c-lang-defconst c-mode-menu
  php (append '(["Complete function name" php-complete-function t]
                ["Browse manual" php-browse-manual t]
                ["Search documentation" php-search-documentation t]
                ["----" t])
              (c-lang-const c-mode-menu)))

(c-lang-defconst c-at-vsemi-p-fn
  php 'php-c-at-vsemi-p)

(c-lang-defconst c-vsemi-status-unknown-p-fn
  php 'php-c-vsemi-status-unknown-p)

;; Make php-mode recognize opening tags as preprocessor macro's.
;;
;; This is a workaround, the tags must be recognized as something
;; in order for the syntactic guesses of code below the tag
;; to be correct and as a result not break indentation.
;;
;; Note that submatches or \\| here are not expected by cc-mode.
(c-lang-defconst c-opt-cpp-prefix
  php "\\s-*<\\?")

(c-lang-defconst c-identifier-ops
  php '(
        (left-assoc "\\" "::" "->")
        (prefix "\\" "::")))

;; Allow '\' when scanning from open brace back to defining
;; construct like class
(c-lang-defconst c-block-prefix-disallowed-chars
  php (cl-set-difference (c-lang-const c-block-prefix-disallowed-chars)
                      '(?\\)))

;; Allow $ so variables are recognized in cc-mode and remove @. This
;; makes cc-mode highlight variables and their type hints in arglists.
(c-lang-defconst c-symbol-start
  php (concat "[" c-alpha "_$]"))

;; All string literals can possibly span multiple lines
(c-lang-defconst c-multiline-string-start-char
  php t)

(c-lang-defconst c-assignment-operators
  ;; falls back to java, so no need to specify the language
  php (append (remove ">>>=" (c-lang-const c-assignment-operators))
              '(".=")))

(c-lang-defconst beginning-of-defun-function
  php 'php-beginning-of-defun)

(c-lang-defconst end-of-defun-function
  php 'php-end-of-defun)

(c-lang-defconst c-primitive-type-kwds
  php '("int" "integer" "bool" "boolean" "float" "double" "real"
        "string" "object" "void"))

(c-lang-defconst c-class-decl-kwds
  "Keywords introducing declarations where the following block (if any)
contains another declaration level that should be considered a class."
  php '("class" "trait" "interface"))

(c-lang-defconst c-brace-list-decl-kwds
  "Keywords introducing declarations where the following block (if
any) is a brace list.

PHP does not have an \"enum\"-like keyword."
  php nil)

(c-lang-defconst c-typeless-decl-kwds
  php (append (c-lang-const c-class-decl-kwds) '("function")))

(c-lang-defconst c-modifier-kwds
  php '("abstract" "const" "final" "static"))

(c-lang-defconst c-protection-kwds
  "Access protection label keywords in classes."
  php '("private" "protected" "public"))

(c-lang-defconst c-postfix-decl-spec-kwds
  php '("implements" "extends"))

(c-lang-defconst c-type-list-kwds
  php '("new" "use" "implements" "extends" "namespace" "instanceof" "insteadof"))

(c-lang-defconst c-ref-list-kwds
  php nil)

(c-lang-defconst c-block-stmt-2-kwds
  php (append '("elseif" "foreach" "declare")
              (remove "synchronized" (c-lang-const c-block-stmt-2-kwds))))

(c-lang-defconst c-simple-stmt-kwds
  php (append '("include" "include_once" "require" "require_once"
                "echo" "print" "die" "exit")
              (c-lang-const c-simple-stmt-kwds)))

(c-lang-defconst c-constant-kwds
  php '("true"
        "false"
        "null"))

(c-lang-defconst c-lambda-kwds
  php '("function"
        "use"))

(c-lang-defconst c-other-block-decl-kwds
  php '("namespace"))

(c-lang-defconst c-other-kwds
  "Keywords not accounted for by any other `*-kwds' language constant."
  php '(
    "__halt_compiler"
    "and"
    "array"
    "callable"
    "iterable"
    "as"
    "break"
    "catch all"
    "catch"
    "clone"
    "default"
    "empty"
    "enddeclare"
    "endfor"
    "endforeach"
    "endif"
    "endswitch"
    "endwhile"
    "eval"
    "global"
    "isset"
    "list"
    "or"
    "parent"
    "static"
    "unset"
    "var"
    "xor"
    "yield"
    "yield from"

    ;; Below keywords are technically not reserved keywords, but
    ;; threated no differently by php-mode from actual reserved
    ;; keywords
    ;;
    ;;; declare directives:
    "encoding"
    "ticks"
    "strict_types"

    ;;; self for static references:
    "self"
    ))

;; PHP does not have <> templates/generics
(c-lang-defconst c-recognize-<>-arglists
  php nil)

(c-lang-defconst c-enums-contain-decls
  php nil)

(c-lang-defconst c-nonlabel-token-key
  "Regexp matching things that can't occur in generic colon labels.

This overrides cc-mode `c-nonlabel-token-key' to support switching on
double quoted strings and true/false/null.

Note: this regexp is also applied to goto-labels, a future improvement
might be to handle switch and goto labels differently."
  php (concat
     ;; All keywords except `c-label-kwds' and `c-constant-kwds'.
     (c-make-keywords-re t
       (cl-set-difference (c-lang-const c-keywords)
                       (append (c-lang-const c-label-kwds)
                               (c-lang-const c-constant-kwds))
                       :test 'string-equal))))

(defun php-lineup-cascaded-calls (langelem)
  "Line up chained methods using `c-lineup-cascaded-calls',
but only if the setting is enabled"
  (if php-lineup-cascaded-calls
    (c-lineup-cascaded-calls langelem)))

(c-add-style
 "php"
 '((c-basic-offset . 4)
   (c-offsets-alist . ((arglist-close . php-lineup-arglist-close)
                       (arglist-cont . (first php-lineup-cascaded-calls 0))
                       (arglist-cont-nonempty . (first php-lineup-cascaded-calls c-lineup-arglist))
                       (arglist-intro . php-lineup-arglist-intro)
                       (case-label . +)
                       (class-open . 0)
                       (comment-intro . 0)
                       (inlambda . 0)
                       (inline-open . 0)
                       (namespace-open . 0)
                       (lambda-intro-cont . +)
                       (label . +)
                       (statement-cont . (first php-lineup-cascaded-calls php-lineup-string-cont +))
                       (substatement-open . 0)
                       (topmost-intro-cont . (first php-lineup-cascaded-calls +))))))

(defun php-enable-default-coding-style ()
  "Set PHP Mode to use reasonable default formatting."
  (interactive)
  (c-set-style "php"))

(c-add-style
 "pear"
 '("php"
   (c-basic-offset . 4)
   (c-offsets-alist . ((case-label . 0)))))

(defun php-enable-pear-coding-style ()
  "Sets up php-mode to use the coding styles preferred for PEAR
code and modules."
  (interactive)
  (setq tab-width 4
        indent-tabs-mode nil)
  (c-set-style "pear")

  ;; Undo drupal/PSR-2 coding style whitespace effects
  (set (make-local-variable 'show-trailing-whitespace)
       (default-value 'show-trailing-whitespace)))

(c-add-style
 "drupal"
 '("php"
   (c-basic-offset . 2)))

(defun php-enable-drupal-coding-style ()
  "Makes php-mode use coding styles that are preferable for
working with Drupal."
  (interactive)
  (setq tab-width 2
        indent-tabs-mode nil
        fill-column 78)
  (set (make-local-variable 'show-trailing-whitespace) t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
  (c-set-style "drupal"))

(c-add-style
  "wordpress"
  '("php"
    (c-basic-offset . 4)))

(defun php-enable-wordpress-coding-style ()
  "Makes php-mode use coding styles that are preferable for
working with Wordpress."
  (interactive)
  (setq indent-tabs-mode t
        fill-column 78
        tab-width 4
        c-indent-comments-syntactically-p t)
  (c-set-style "wordpress")

  ;; Undo drupal/PSR-2 coding style whitespace effects
  (set (make-local-variable 'show-trailing-whitespace)
       (default-value 'show-trailing-whitespace)))

(c-add-style
  "symfony2"
  '("php"
    (c-offsets-alist . ((statement-cont . php-lineup-hanging-semicolon)))))

(defun php-enable-symfony2-coding-style ()
  "Makes php-mode use coding styles that are preferable for
working with Symfony2."
  (interactive)
  (setq indent-tabs-mode nil
        fill-column 78
        c-indent-comments-syntactically-p t
        require-final-newline t)
  (c-set-style "symfony2")

  ;; Undo drupal/PSR-2 coding style whitespace effects
  (set (make-local-variable 'show-trailing-whitespace)
       (default-value 'show-trailing-whitespace)))

(c-add-style
  "psr2"
  '("php"
    (c-offsets-alist . ((statement-cont . +)))))

(defun php-enable-psr2-coding-style ()
  "Makes php-mode comply to the PSR-2 coding style"
  (interactive)
  (setq indent-tabs-mode nil
        fill-column 78
        c-indent-comments-syntactically-p t
        require-final-newline t)
  (c-set-style "psr2")

  ;; Apply drupal-like coding style whitespace effects
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'show-trailing-whitespace) t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))

(defconst php-beginning-of-defun-regexp
  "^\\s-*\\(?:\\(?:abstract\\|final\\|private\\|protected\\|public\\|static\\)\\s-+\\)*function\\s-+&?\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*("
  "Regular expression for a PHP function.")

(defun php-beginning-of-defun (&optional arg)
  "Move to the beginning of the ARGth PHP function from point.
Implements PHP version of `beginning-of-defun-function'."
  (interactive "p")
  (let ((arg (or arg 1)))
    (while (> arg 0)
      (re-search-backward php-beginning-of-defun-regexp
                          nil 'noerror)
      (setq arg (1- arg)))
    (while (< arg 0)
      (end-of-line 1)
      (let ((opoint (point)))
        (beginning-of-defun 1)
        (forward-list 2)
        (forward-line 1)
        (if (eq opoint (point))
            (re-search-forward php-beginning-of-defun-regexp
                               nil 'noerror))
        (setq arg (1+ arg))))))

(defun php-end-of-defun (&optional arg)
  "Move the end of the ARGth PHP function from point.
Implements PHP version of `end-of-defun-function'

See `php-beginning-of-defun'."
  (interactive "p")
  (php-beginning-of-defun (- (or arg 1))))


(defvar php-warned-bad-indent nil)

;; Do it but tell it is not good if html tags in buffer.
(defun php-check-html-for-indentation ()
  (let ((html-tag-re "^\\s-*</?\\sw+.*?>")
        (here (point)))
    (goto-char (line-beginning-position))
    (if (or (when (boundp 'mumamo-multi-major-mode) mumamo-multi-major-mode)
            ;; Fix-me: no idea how to check for mmm or multi-mode
            (save-match-data
              (not (or (re-search-forward html-tag-re (line-end-position) t)
                       (re-search-backward html-tag-re (line-beginning-position) t)))))
        (progn
          (goto-char here)
          t)
      (goto-char here)
      (setq php-warned-bad-indent t)
      (let* ((known-multi-libs '(("mumamo" mumamo (lambda () (nxhtml-mumamo)))
                                 ("mmm-mode" mmm-mode (lambda () (mmm-mode 1)))
                                 ("multi-mode" multi-mode (lambda () (multi-mode 1)))
                                 ("web-mode" web-mode (lambda () (web-mode)))))
             (known-names (mapcar (lambda (lib) (car lib)) known-multi-libs))
             (available-multi-libs (delq nil
                                         (mapcar
                                          (lambda (lib)
                                            (when (locate-library (car lib)) lib))
                                          known-multi-libs)))
             (available-names (mapcar (lambda (lib) (car lib)) available-multi-libs))
             (base-msg
              (concat
               "Indentation fails badly with mixed HTML/PHP in the HTML part in
plain `php-mode'.  To get indentation to work you must use an
Emacs library that supports 'multiple major modes' in a buffer.
Parts of the buffer will then be in `php-mode' and parts in for
example `html-mode'.  Known such libraries are:\n\t"
               (mapconcat 'identity known-names ", ")
               "\n"
               (if available-multi-libs
                   (concat
                    "You have these available in your `load-path':\n\t"
                    (mapconcat 'identity available-names ", ")
                    "\n\n"
                    "Do you want to turn any of those on? ")
                 "You do not have any of those in your `load-path'.")))
             (is-using-multi
              (catch 'is-using
                (dolist (lib available-multi-libs)
                  (when (and (boundp (cadr lib))
                             (symbol-value (cadr lib)))
                    (throw 'is-using t))))))
        (unless is-using-multi
          (if available-multi-libs
              (if (not (y-or-n-p base-msg))
                  (message "Did not do indentation, but you can try again now if you want")
                (let* ((name
                        (if (= 1 (length available-multi-libs))
                            (car available-names)
                          ;; Minibuffer window is more than one line, fix that first:
                          (message "")
                          (completing-read "Choose multiple major mode support library: "
                                           available-names nil t
                                           (car available-names)
                                           '(available-names . 1)
                                           )))
                       (mode (when name
                               (cl-caddr (assoc name available-multi-libs)))))
                  (when mode
                    ;; Minibuffer window is more than one line, fix that first:
                    (message "")
                    (load name)
                    (funcall mode))))
            (lwarn 'php-indent :warning base-msg)))
        nil))))

(defun php-cautious-indent-region (start end &optional quiet)
  (if (or (not php-mode-warn-if-mumamo-off)
          php-warned-bad-indent
          (php-check-html-for-indentation))
      (funcall 'c-indent-region start end quiet)))

(defun php-cautious-indent-line ()
  (if (or (not php-mode-warn-if-mumamo-off)
          php-warned-bad-indent
          (php-check-html-for-indentation))
      (let ((here (point))
            doit)
        (move-beginning-of-line nil)
        ;; Don't indent heredoc end mark
        (save-match-data
          (unless (and (looking-at "[a-zA-Z0-9_]+;\n")
                       (php-in-string-p))
            (setq doit t)))
        (goto-char here)
        (when doit
          (funcall 'c-indent-line)))))

(defun php-c-at-vsemi-p (&optional pos)
  "Return t on html lines (including php region border), otherwise nil.
POS is a position on the line in question.

This is was done due to the problem reported here:

  URL `https://answers.launchpad.net/nxhtml/+question/43320'"
  (if (not php-template-compatibility)
      nil
    (setq pos (or pos (point)))
    (let ((here (point))
          ret)
      (save-match-data
        (goto-char pos)
        (beginning-of-line)
        (setq ret (looking-at
                   (rx
                    (or (seq
                         bol
                         (0+ space)
                         "<"
                         (in "a-z\\?"))
                        (seq
                         (0+ not-newline)
                         (in "a-z\\?")
                         ">"
                         (0+ space)
                         eol))))))
      (goto-char here)
      ret)))

(defun php-c-vsemi-status-unknown-p ()
  "See `php-c-at-vsemi-p'."
  )

(defun php-lineup-string-cont (langelem)
  "Line up string toward equal sign or dot
e.g.
$str = 'some'
     . 'string';
this ^ lineup"
  (save-excursion
    (goto-char (cdr langelem))
    (let (ret finish)
      (while (and (not finish) (re-search-forward "[=.]" (line-end-position) t))
        (unless (php-in-string-or-comment-p)
          (setq finish t
                ret (vector (1- (current-column))))))
      ret)))

(defun php-lineup-arglist-intro (langelem)
  (save-excursion
    (goto-char (cdr langelem))
    (vector (+ (current-column) c-basic-offset))))

(defun php-lineup-arglist-close (langelem)
  (save-excursion
    (goto-char (cdr langelem))
    (vector (current-column))))

(defun php-lineup-arglist (_langelem)
  (save-excursion
    (beginning-of-line)
    (if (looking-at-p "\\s-*->") '+ 0)))

(defun php-lineup-hanging-semicolon (_langelem)
  (save-excursion
    (beginning-of-line)
    (if (looking-at-p "\\s-*;\\s-*$") 0 '+)))

(defconst php-heredoc-start-re
  "<<<\\(?:\\w+\\|'\\w+'\\)$"
  "Regular expression for the start of a PHP heredoc.")

(defun php-heredoc-end-re (heredoc-start)
  "Build a regular expression for the end of a heredoc started by
the string HEREDOC-START."
  ;; Extract just the identifier without <<< and quotes.
  (string-match "\\w+" heredoc-start)
  (concat "^\\(" (match-string 0 heredoc-start) "\\)\\W"))

(defun php-syntax-propertize-function (start end)
  "Apply propertize rules from START to END."
  ;; (defconst php-syntax-propertize-function
  ;;   (syntax-propertize-rules
  ;;    (php-heredoc-start-re (0 (ignore (php-heredoc-syntax))))))
  (goto-char start)
  (while (and (< (point) end)
              (re-search-forward php-heredoc-start-re end t))
    (php-heredoc-syntax))
  (goto-char start)
  (while (re-search-forward "['\"]" end t)
    (when (php-in-comment-p)
      (c-put-char-property (match-beginning 0)
                           'syntax-table (string-to-syntax "_")))))

(defun php-heredoc-syntax ()
  "Mark the boundaries of searched heredoc."
  (goto-char (match-beginning 0))
  (c-put-char-property (point) 'syntax-table (string-to-syntax "|"))
  (if (re-search-forward (php-heredoc-end-re (match-string 0)) nil t)
      (goto-char (match-end 1))
    ;; Did not find the delimiter so go to the end of the buffer.
    (goto-char (point-max)))
  (c-put-char-property (1- (point)) 'syntax-table (string-to-syntax "|")))

(defun php-syntax-propertize-extend-region (start end)
  "Extend the propertize region if START or END falls inside a
PHP heredoc."
  (let ((new-start)
        (new-end))
    (goto-char start)
    (when (re-search-backward php-heredoc-start-re nil t)
      (let ((maybe (point)))
        (when (and (re-search-forward
                    (php-heredoc-end-re (match-string 0)) nil t)
                   (> (point) start))
          (setq new-start maybe))))
    (goto-char end)
    (when (re-search-backward php-heredoc-start-re nil t)
      (if (re-search-forward
           (php-heredoc-end-re (match-string 0)) nil t)
          (when (> (point) end)
            (setq new-end (point)))
        (setq new-end (point-max))))
    (when (or new-start new-end)
      (cons (or new-start start) (or new-end end)))))

(easy-menu-define php-mode-menu php-mode-map "PHP Mode Commands"
  (cons "PHP" (c-lang-const c-mode-menu php)))


;; Faces

;;;###autoload
(defgroup php-faces nil
  "Faces used in PHP Mode"
  :tag "PHP Faces"
  :group 'php
  :group 'faces)

(defface php-string '((t (:inherit font-lock-string-face)))
  "PHP Mode face used to highlight string literals."
  :group 'php-faces)

(defface php-keyword '((t (:inherit font-lock-keyword-face)))
  "PHP Mode face used to highlight keywords."
  :group 'php-faces)

(defface php-builtin '((t (:inherit font-lock-builtin-face)))
  "PHP Mode face used to highlight builtins."
  :group 'php-faces)

(defface php-function-name '((t (:inherit font-lock-function-name-face)))
  "PHP Mode face used to highlight function names."
  :group 'php-faces)

(defface php-function-call '((t (:inherit default)))
  "PHP Mode face used to highlight function names in calles."
  :group 'php-faces)

(defface php-method-call '((t (:inherit php-function-call)))
  "PHP Mode face used to highlight method names in calles."
  :group 'php-faces)

(defface php-static-method-call '((t (:inherit php-method-call)))
  "PHP Mode face used to highlight static method names in calles."
  :group 'php-faces)

(defface php-variable-name '((t (:inherit font-lock-variable-name-face)))
  "PHP Mode face used to highlight variable names."
  :group 'php-faces)

(defface php-property-name '((t (:inherit php-variable-name)))
  "PHP Mode face used to highlight property names."
  :group 'php-faces)

(defface php-variable-sigil '((t (:inherit default)))
  "PHP Mode face used to highlight variable sigils ($)."
  :group 'php-faces)

(defface php-object-op '((t (:inherit default)))
  "PHP Mode face used to object operators (->)."
  :group 'php-faces)

(defface php-paamayim-nekudotayim '((t (:inherit default)))
  "PHP Mode face used to highlight \"Paamayim Nekudotayim\" scope resolution operators (::)."
  :group 'php-faces)

(defface php-type '((t (:inherit font-lock-type-face)))
  "PHP Mode face used to highlight types."
  :group 'php-faces)

(defface php-constant '((t (:inherit font-lock-constant-face)))
  "PHP Mode face used to highlight constants."
  :group 'php-faces)

(defface php-$this '((t (:inherit php-constant)))
  "PHP Mode face used to highlight $this variables."
  :group 'php-faces)

(defface php-$this-sigil '((t (:inherit php-constant)))
  "PHP Mode face used to highlight sigils($) of $this variable."
  :group 'php-faces)

(defface php-php-tag '((t (:inherit font-lock-constant-face)))
  "PHP Mode face used to highlight PHP tags."
  :group 'php-faces)

(defface php-doc-annotation-tag '((t . (:inherit font-lock-constant-face)))
  "Face used to highlight annotation tags in doc-comment."
  :group 'php-faces)

(defface php-doc-variable-sigil '((t (:inherit font-lock-variable-name-face)))
  "PHP Mode face used to highlight variable sigils($)."
  :group 'php-faces)

(defface php-doc-$this '((t (:inherit php-type)))
  "PHP Mode face used to highlight $this variable in doc-comment."
  :group 'php-faces)

(defface php-doc-$this-sigil '((t (:inherit php-type)))
  "PHP Mode face used to highlight sigil of $this variable in doc-comment."
  :group 'php-faces)

(defface php-doc-class-name '((t (:inherit php-string)))
  "Face used to class names in doc-comment."
  :group 'php-faces)

(define-obsolete-face-alias 'php-annotations-annotation-face 'php-doc-annotation-tag "1.19.0")


;;;###autoload
(define-derived-mode php-mode c-mode "PHP"
  "Major mode for editing PHP code.

\\{php-mode-map}"

  (c-initialize-cc-mode t)
  (c-init-language-vars php-mode)
  (c-common-init 'php-mode)

  (set (make-local-variable font-lock-string-face) 'php-string)
  (set (make-local-variable font-lock-keyword-face) 'php-keyword)
  (set (make-local-variable font-lock-builtin-face) 'php-builtin)
  (set (make-local-variable font-lock-function-name-face) 'php-function-name)
  (set (make-local-variable font-lock-variable-name-face) 'php-variable-name)
  (set (make-local-variable font-lock-constant-face) 'php-constant)

  (modify-syntax-entry ?_    "_" php-mode-syntax-table)
  (modify-syntax-entry ?`    "\"" php-mode-syntax-table)
  (modify-syntax-entry ?\"   "\"" php-mode-syntax-table)
  (modify-syntax-entry ?#    "< b" php-mode-syntax-table)
  (modify-syntax-entry ?\n   "> b" php-mode-syntax-table)
  (modify-syntax-entry ?$    "'" php-mode-syntax-table)

  (set (make-local-variable 'syntax-propertize-via-font-lock)
       '(("\\(\"\\)\\(\\\\.\\|[^\"\n\\]\\)*\\(\"\\)" (1 "\"") (3 "\""))
         ("\\(\'\\)\\(\\\\.\\|[^\'\n\\]\\)*\\(\'\\)" (1 "\"") (3 "\""))))

  (add-to-list (make-local-variable 'syntax-propertize-extend-region-functions)
               #'php-syntax-propertize-extend-region)
  (set (make-local-variable 'syntax-propertize-function)
       #'php-syntax-propertize-function)

  (setq imenu-generic-expression php-imenu-generic-expression)

  ;; PHP vars are case-sensitive
  (setq case-fold-search t)

  ;; Do not force newline at end of file.  Such newlines can cause
  ;; trouble if the PHP file is included in another file before calls
  ;; to header() or cookie().
  (set (make-local-variable 'require-final-newline) nil)
  (set (make-local-variable 'next-line-add-newlines) nil)

  ;; PEAR coding standards
  (add-hook 'php-mode-pear-hook 'php-enable-pear-coding-style
             nil t)

  ;; ;; Drupal coding standards
  (add-hook 'php-mode-drupal-hook 'php-enable-drupal-coding-style
             nil t)

  ;; ;; WordPress coding standards
  (add-hook 'php-mode-wordpress-hook 'php-enable-wordpress-coding-style
             nil t)

  ;; ;; Symfony2 coding standards
  (add-hook 'php-mode-symfony2-hook 'php-enable-symfony2-coding-style
             nil t)

  ;; ;; PSR-2 coding standards
  (add-hook 'php-mode-psr2-hook 'php-enable-psr2-coding-style
             nil t)

  (cond ((eq php-mode-coding-style 'pear)
         (php-enable-pear-coding-style)
         (run-hooks 'php-mode-pear-hook))
        ((eq php-mode-coding-style 'drupal)
         (php-enable-drupal-coding-style)
         (run-hooks 'php-mode-drupal-hook))
        ((eq php-mode-coding-style 'wordpress)
         (php-enable-wordpress-coding-style)
         (run-hooks 'php-mode-wordpress-hook))
        ((eq php-mode-coding-style 'symfony2)
         (php-enable-symfony2-coding-style)
         (run-hooks 'php-mode-symfony2-hook))
        ((eq php-mode-coding-style 'psr2)
         (php-enable-psr2-coding-style)
         (run-hooks 'php-mode-psr2-hook)))

  (if (or php-mode-force-pear
          (and (stringp buffer-file-name)
               (string-match "PEAR\\|pear"
                             (buffer-file-name))
               (string-match "\\.php$" (buffer-file-name))))
      (run-hooks 'php-mode-pear-hook))

  (setq indent-line-function 'php-cautious-indent-line)
  (setq indent-region-function 'php-cautious-indent-region)
  (setq c-at-vsemi-p-fn 'php-c-at-vsemi-p)
  (setq c-vsemi-status-unknown-p 'php-c-vsemi-status-unknown-p)

  ;; syntax-begin-function is obsolete in Emacs 25.1
  (with-no-warnings
    (set (make-local-variable 'syntax-begin-function)
         'c-beginning-of-syntax))

  ;; We map the php-{beginning,end}-of-defun functions so that they
  ;; replace the similar commands that we inherit from CC Mode.
  ;; Because of our remapping we may not actually need to keep the
  ;; following two local variables, but we keep them for now until we
  ;; are completely sure their removal will not break any current
  ;; behavior or backwards compatibility.
  (set (make-local-variable 'beginning-of-defun-function)
       'php-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'php-end-of-defun)

  (set (make-local-variable 'open-paren-in-column-0-is-defun-start)
       nil)
  (set (make-local-variable 'defun-prompt-regexp)
       "^\\s-*function\\s-+&?\\s-*\\(\\(\\sw\\|\\s_\\)+\\)\\s-*")
  (set (make-local-variable 'add-log-current-defun-header-regexp)
       php-beginning-of-defun-regexp)

  (when (>= emacs-major-version 25)
    (with-silent-modifications
      (save-excursion
        (php-syntax-propertize-function (point-min) (point-max))))))


;; Define function name completion function
(defvar php-completion-table nil
  "Obarray of tag names defined in current tags table and functions known to PHP.")

(defun php-complete-function ()
  "Perform function completion on the text around point.
Completes to the set of names listed in the current tags table
and the standard php functions.
The string to complete is chosen in the same way as the default
for \\[find-tag] (which see)."
  (interactive)
  (let ((pattern (php-get-pattern))
        beg
        completion
        (php-functions (php-completion-table)))
    (if (not pattern) (message "Nothing to complete")
        (if (not (search-backward pattern nil t))
            (message "Can't complete here")
          (setq beg (point))
          (forward-char (length pattern))
          (setq completion (try-completion pattern php-functions nil))
          (cond ((eq completion t))
                ((null completion)
                 (message "Can't find completion for \"%s\"" pattern)
                 (ding))
                ((not (string= pattern completion))
                 (delete-region beg (point))
                 (insert completion))
                (t
                 (message "Making completion list...")
                 (with-output-to-temp-buffer "*Completions*"
                   (display-completion-list
                    (all-completions pattern php-functions)))
                 (message "Making completion list...%s" "done")))))))

(defun php-completion-table ()
  "Build variable `php-completion-table' on demand.
The table includes the PHP functions and the tags from the
current `tags-file-name'."
  (or (and tags-file-name
           (save-excursion (tags-verify-table tags-file-name))
           php-completion-table)
      (let ((tags-table
             (when tags-file-name
               (with-current-buffer (get-file-buffer tags-file-name)
                 (etags-tags-completion-table))))
            (php-table
             (cond ((and (not (string= "" php-completion-file))
                         (file-readable-p php-completion-file))
                    (php-build-table-from-file php-completion-file))
                   (php-manual-path
                    (php-build-table-from-path php-manual-path))
                   (t nil))))
        (unless (or php-table tags-table)
          (error
           (concat "No TAGS file active nor are "
                   "`php-completion-file' or `php-manual-path' set")))
        (when tags-table
          ;; Combine the tables.
          (mapatoms (lambda (sym) (intern (symbol-name sym) php-table))
                    tags-table))
        (setq php-completion-table php-table))))

(defun php-build-table-from-file (filename)
  (let ((table (make-vector 1022 0))
        (buf (find-file-noselect filename)))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (re-search-forward
              "^\\([-a-zA-Z0-9_.]+\\)\n"
              nil t)
        (intern (buffer-substring (match-beginning 1) (match-end 1))
                table)))
    (kill-buffer buf)
    table))

(defun php-build-table-from-path (path)
  (let ((table (make-vector 1022 0))
        (files (directory-files
                path
                nil
                "^function\\..+\\.html$")))
    (mapc (lambda (file)
            (string-match "\\.\\([-a-zA-Z_0-9]+\\)\\.html$" file)
            (intern
             (replace-regexp-in-string
              "-" "_" (substring file (match-beginning 1) (match-end 1)) t)
             table))
          files)
    table))

;; Find the pattern we want to complete
;; find-tag-default from GNU Emacs etags.el
(defun php-get-pattern ()
  (save-excursion
    (while (looking-at "\\sw\\|\\s_")
      (forward-char 1))
    (if (or (re-search-backward "\\sw\\|\\s_"
                                (save-excursion (beginning-of-line) (point))
                                t)
            (re-search-forward "\\(\\sw\\|\\s_\\)+"
                               (save-excursion (end-of-line) (point))
                               t))
        (progn (goto-char (match-end 0))
               (buffer-substring-no-properties
                (point)
                (progn (forward-sexp -1)
                       (while (looking-at "\\s'")
                         (forward-char 1))
                       (point))))
      nil)))

(defun php-show-arglist ()
  (interactive)
  (let* ((tagname (php-get-pattern))
         (buf (find-tag-noselect tagname nil nil))
         arglist)
    (with-current-buffer buf
      (goto-char (point-min))
      (when (re-search-forward
             (format "function\\s-+%s\\s-*(\\([^{]*\\))" tagname)
             nil t)
        (setq arglist (buffer-substring-no-properties
                       (match-beginning 1) (match-end 1)))))
    (if arglist
        (message "Arglist for %s: %s" tagname arglist)
        (message "Unknown function: %s" tagname))))

(defcustom php-search-documentation-browser-function nil
  "Function to display PHP documentation in a WWW browser.

If non-nil, this shadows the value of `browse-url-browser-function' when
calling `php-search-documentation' or `php-search-local-documentation'."
  :group 'php
  :type '(choice (const :tag "default" nil) function)
  :link '(variable-link browse-url-browser-function))

(defun php-browse-documentation-url (url)
  "Browse a documentation URL using the configured browser function.

See `php-search-documentation-browser-function'."
  (let ((browse-url-browser-function
         (or php-search-documentation-browser-function
             browse-url-browser-function)))
    (browse-url url)))

(defvar php-search-local-documentation-types
  (list "function" "control-structures" "class" "book")
  ;; "intro" and "ref" also look interesting, but for all practical purposes
  ;; their terms are sub-sets of the "book" terms (with the few exceptions
  ;; being very unlikely search terms).
  "The set (and priority sequence) of documentation file prefixes
under which to search for files in the local documentation directory.")

(defvar php-search-local-documentation-words-cache nil)

(defun php--search-documentation-read-arg ()
  "Obtain interactive argument for searching documentation."
  ;; Cache the list of documentation words available for completion,
  ;; based on the defined types-of-interest.
  (let ((types-list php-search-local-documentation-types)
        (words-cache php-search-local-documentation-words-cache)
        (local-manual (and (stringp php-manual-path)
                           (not (string= php-manual-path "")))))
    (when (and local-manual
               (not (assq types-list words-cache)))
      ;; Generate the cache on the first run, or if the types changed.
      ;; We read the filenames matching our types list in the local
      ;; documentation directory, and extract the 'middle' component
      ;; of each. e.g. "function.array-map.html" => "array_map".
      (let* ((types-opt (regexp-opt types-list))
             (pattern (concat "\\`" types-opt "\\.\\(.+\\)\\.html\\'"))
             (collection
              (mapcar (lambda (filename) (subst-char-in-string
                                          ?- ?_ (replace-regexp-in-string
                                                 pattern "\\1" filename)))
                      (directory-files php-manual-path nil pattern))))
        ;; Replace the entire cache. If the types changed, we don't need
        ;; to retain the collection for the previous value.
        (setq words-cache (list (cons types-list collection)))
        (setq php-search-local-documentation-words-cache words-cache)))
    ;; By default we search for (current-word) immediately, without prompting.
    ;; With a prefix argument, or if there is no (current-word), we perform a
    ;; completing read for a word from the cached collection.
    (let* ((default (current-word))
           (prompt (if default
                       (format "Search PHP docs (%s): " default)
                     "Search PHP docs: "))
           (collection (and local-manual
                            (cdr (assq types-list words-cache))))
           (word (if (or current-prefix-arg (not default))
                     (completing-read prompt collection nil nil nil nil default)
                   default)))
      ;; Return interactive argument list.
      (list word))))

(defun php-search-local-documentation (word)
  "Search the local PHP documentation (i.e. in `php-manual-path') for
the word at point.  The function returns t if the requested documentation
exists, and nil otherwise.

With a prefix argument, prompt (with completion) for a word to search for."
  (interactive (php--search-documentation-read-arg))
  (let ((file (catch 'found
                (cl-loop for type in php-search-local-documentation-types do
                         (let* ((doc-html (format "%s.%s.html"
                                                  type
                                                  (replace-regexp-in-string
                                                   "_" "-" (downcase word))))
                                (file (expand-file-name doc-html  php-manual-path)))
                           (when (file-exists-p file)
                             (throw 'found file)))))))
    (when file
      (let ((file-url (if (string-prefix-p "file://" file)
                          file
                        (concat "file://" file))))
        (php-browse-documentation-url file-url))
      t)))

(defsubst php-search-web-documentation (word)
  "Return URL to search PHP manual search by `WORD'."
  (php-browse-documentation-url (concat (or php-search-url php-site-url) word)))

;; Define function documentation function
(defun php-search-documentation (word)
  "Search PHP documentation for the `WORD' at point.

If `php-manual-path' has a non-empty string value then the command
will first try searching the local documentation.  If the requested
documentation does not exist it will fallback to searching the PHP
website.

With a prefix argument, prompt for a documentation word to search
for.  If the local documentation is available, it is used to build
a completion list."
  (interactive (php--search-documentation-read-arg))
  (if (and (stringp php-manual-path)
           (not (string= php-manual-path "")))
      (or (php-search-local-documentation word)
          (php-search-web-documentation word))
    (php-search-web-documentation word)))

;; Define function for browsing manual
(defun php-browse-manual ()
  "Bring up manual for PHP."
  (interactive)
  (browse-url (if (stringp php-manual-url)
                  php-manual-url
                (format "%smanual/%s/" php-site-url php-manual-url))))


;; Font Lock
(defconst php-phpdoc-type-keywords
  (list "string" "integer" "int" "boolean" "bool" "float"
        "double" "object" "mixed" "array" "resource"
        "void" "null" "false" "true" "self" "static"
        "callable" "iterable" "number"))

(defconst php-phpdoc-type-tags
  (list "param" "property" "property-read" "property-write" "return" "var"))

(defconst php-phpdoc-font-lock-doc-comments
  `(("{@[-[:alpha:]]+\\s-\\([^}]*\\)}" ; "{@foo ...}" markup.
     (0 'php-doc-annotation-tag prepend nil)
     (1 'php-string prepend nil))
    (,(rx (group "$") (group (in "A-Za-z_") (* (in "0-9A-Za-z_"))))
     (1 'php-doc-variable-sigil prepend nil)
     (2 'php-variable-name prepend nil))
    ("\\(\\$\\)\\(this\\)\\>" (1 'php-doc-$this-sigil prepend nil) (2 'php-doc-$this prepend nil))
    (,(concat "\\s-@" (regexp-opt php-phpdoc-type-tags) "\\s-+"
              "\\(" (rx (+ (? "?") (? "\\") (+ (in "0-9A-Z_a-z")) (? "[]") (? "|"))) "\\)+")
     1 'php-string prepend nil)
    (,(concat "\\(?:|\\|\\?\\|\\s-\\)\\("
              (regexp-opt php-phpdoc-type-keywords 'words)
              "\\)")
     1 font-lock-type-face prepend nil)
    ("https?://[^\n\t ]+"
     0 'link prepend nil)
    ("^\\(?:/\\*\\)?\\(?:\\s \\|\\*\\)*\\(@[[:alpha:]][-[:alpha:]\\]*\\)" ; "@foo ..." markup.
     1 'php-doc-annotation-tag prepend nil)))

(defvar php-phpdoc-font-lock-keywords
  `((,(lambda (limit)
	(c-font-lock-doc-comments "/\\*\\*" limit
	  php-phpdoc-font-lock-doc-comments)))))

(defconst php-font-lock-keywords-1 (c-lang-const c-matchers-1 php)
  "Basic highlighting for PHP Mode.")

(defconst php-font-lock-keywords-2 (c-lang-const c-matchers-2 php)
  "Medium level highlighting for PHP Mode.")

(defconst php-font-lock-keywords-3
  (append
   php-phpdoc-font-lock-keywords
   ;; php-mode patterns *before* cc-mode:
   ;;  only add patterns here if you want to prevent cc-mode from applying
   ;;  a different face.
   '(
     ;; Highlight variables, e.g. 'var' in '$var' and '$obj->var', but
     ;; not in $obj->var()
     ("\\(->\\)\\(\\sw+\\)\\s-*(" (1 'php-object-op) (2 'php-method-call))

     ;; Highlight special variables
     ("\\(\\$\\)\\(this\\|that\\)\\_>" (1 'php-$this-sigil) (2 'php-$this))
     ("\\(\\$\\)\\([a-zA-Z0-9_]+\\)" (1 'php-variable-sigil) (2 'php-variable-name))
     ("\\(->\\)\\([a-zA-Z0-9_]+\\)" (1 'php-object-op) (2 'php-property-name))

     ;; Highlight function/method names
     ("\\<function\\s-+&?\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1 'php-function-name)

     ;; The dollar sign should not get a variable-name face, below
     ;; pattern resets the face to default in case cc-mode sets the
     ;; variable-name face (cc-mode does this for variables prefixed
     ;; with type, like in arglist)
     ("\\(\\$\\)\\(\\sw+\\)" 1 'php-variable-sigil)

     ;; Array is a keyword, except in the following situations:
     ;; - when used as cast, so that (int) and (array) look the same
     ;; - when used as a type hint
     ;; - when used as a return type
     ("(\\(array\\))" 1 font-lock-type-face)
     ("\\b\\(array\\)\\s-+\\$" 1 font-lock-type-face)
     (")\\s-*:\\s-*\\??\\(array\\)\\b" 1 font-lock-type-face)

     ;; Support the ::class constant in PHP5.6
     ("\\sw+\\(::\\)\\(class\\)\\b" (1 'php-paamayim-nekudotayim) (2 'php-constant)))

   ;; cc-mode patterns
   (c-lang-const c-matchers-3 php)

   ;; php-mode patterns *after* cc-mode:
   ;;   most patterns should go here, faces will only be applied if not
   ;;   already fontified by another pattern. Note that using OVERRIDE
   ;;   is usually overkill.
   `(
     ;; Highlight variables, e.g. 'var' in '$var' and '$obj->var', but
     ;; not in $obj->var()
     ("->\\(\\sw+\\)\\s-*(" 1 'php-method-call)

     ("\\(\\$\\|->\\)\\([a-zA-Z0-9_]+\\)" 2 'php-property-name)

     ;; Highlight all upper-cased symbols as constant
     ("\\<\\([A-Z_][A-Z0-9_]+\\)\\>" 1 'php-constant)

     ;; Highlight all statically accessed class names as constant,
     ;; another valid option would be using type-face, but using
     ;; constant-face because this is how it works in c++-mode.
     ("\\(\\sw+\\)\\(::\\)" (1 'php-constant) (2 'php-paamayim-nekudotayim))

     ;; Highlight class name after "use .. as"
     ("\\<as\\s-+\\(\\sw+\\)" 1 font-lock-type-face)

     ;; Class names are highlighted by cc-mode as defined in
     ;; c-class-decl-kwds, below regexp is a workaround for a bug
     ;; where the class names are not highlighted right after opening
     ;; a buffer (editing a file corrects it).
     ;;
     ;; This behaviour is caused by the preceding '<?php', which
     ;; cc-mode cannot handle easily. Registering it as a cpp
     ;; preprocessor works well (i.e. the next line is not a
     ;; statement-cont) but the highlighting glitch remains.
     (,(concat (regexp-opt (c-lang-const c-class-decl-kwds php))
               " \\(\\sw+\\)")
      1 font-lock-type-face)

     ;; Highlight return types in functions and methods.
     ("function.+:\\s-*\\??\\(\\(?:\\sw\\|\\s_\\)+\\)" 1 font-lock-type-face)
     (")\\s-*:\\s-*\\??\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*\{" 1 font-lock-type-face)

     ;; Highlight class names used as nullable type hints
     ("\\?\\(\\(:?\\sw\\|\\s_\\)+\\)\\s-+\\$" 1 font-lock-type-face)

     ;; While c-opt-cpp-* highlights the <?php opening tags, it is not
     ;; possible to make it highlight short open tags and closing tags
     ;; as well. So we force the correct face on all cases that
     ;; c-opt-cpp-* lacks for this purpose.
     ;;
     ;; Note that starting a file with <% breaks indentation, a
     ;; limitation we can/should live with.
     (,(regexp-opt '("?>" "<?" "<%" "%>")) 0 'php-php-tag)))
  "Detailed highlighting for PHP Mode.")

(defvar php-font-lock-keywords php-font-lock-keywords-3
  "Default expressions to highlight in PHP Mode.")

;;; Provide support for Flymake so that users can see warnings and
;;; errors in real-time as they write code.

(defun flymake-php-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list php-executable (list "-f" local-file "-l"))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.php[345s]?$"
               flymake-php-init
               flymake-simple-cleanup
               flymake-get-real-file-name))

(add-to-list 'flymake-err-line-patterns
             '("\\(Parse\\|Fatal\\) error: \\(.*?\\) in \\(.*?\\) on line \\([0-9]+\\)" 3 4 nil 2))


(defun php-send-region (start end)
  "Send the region between `start' and `end' to PHP for execution.
The output will appear in the buffer *PHP*."
  (interactive "r")
  (let ((php-buffer (get-buffer-create "*PHP*"))
        (code (buffer-substring start end)))
    ;; Calling 'php -r' will fail if we send it code that starts with
    ;; '<?php', which is likely.  So we run the code through this
    ;; function to check for that prefix and remove it.
    (let ((cleaned-php-code (if (string-prefix-p "<?php" code t)
                                (substring code 5)
                              code)))
      (call-process php-executable nil php-buffer nil "-r" cleaned-php-code))))


(defconst php-string-interpolated-variable-regexp
  "{\\$[^}\n\\\\]*\\(?:\\\\.[^}\n\\\\]*\\)*}\\|\\${\\sw+}\\|\\$\\sw+")

(defun php-string-intepolated-variable-font-lock-find (limit)
  (while (re-search-forward php-string-interpolated-variable-regexp limit t)
    (let ((quoted-stuff (nth 3 (syntax-ppss))))
      (when (and quoted-stuff (member quoted-stuff '(?\" ?`)))
        (put-text-property (match-beginning 0) (match-end 0)
                           'face 'php-variable-name))))
  nil)

(eval-after-load 'php-mode
  '(progn
     (font-lock-add-keywords
      'php-mode
      `((php-string-intepolated-variable-font-lock-find))
      'append)))



;;; Correct the behavior of `delete-indentation' by modifying the
;;; logic of `fixup-whitespace'.
(defadvice fixup-whitespace (after php-mode-fixup-whitespace)
  "Remove whitespace before certain characters in PHP Mode."
  (let* ((no-behind-space ";\\|,\\|->\\|::")
         (no-front-space "->\\|::"))
    (when (and (eq major-mode 'php-mode)
               (or (looking-at-p (concat " \\(" no-behind-space "\\)"))
                   (save-excursion
                     (forward-char -2)
                     (looking-at-p no-front-space))))
      (delete-char 1))))

(ad-activate 'fixup-whitespace)

;; Advice `font-lock-fontify-keywords-region' to support namespace
;; separators in class names. Use word syntax for backslashes when
;; doing keyword fontification, but not when doing syntactic
;; fontification because that breaks \ as escape character in strings.
;;
;; Special care is taken to restore the original syntax, because we
;; want \ not to be word for functions like forward-word.
(defadvice font-lock-fontify-keywords-region (around backslash-as-word activate)
  "Fontify keywords with backslash as word character."
  (let ((old-syntax (string (char-syntax ?\\))))
    (modify-syntax-entry ?\\ "w")
    ad-do-it
    (modify-syntax-entry ?\\ old-syntax)))


(defcustom php-class-suffix-when-insert "::"
  "Suffix for inserted class."
  :group 'php
  :type 'string)

(defcustom php-namespace-suffix-when-insert "\\"
  "Suffix for inserted namespace."
  :group 'php
  :type 'string)

(defvar php--re-namespace-pattern
  (php-create-regexp-for-classlike "namespace"))

(defvar php--re-classlike-pattern
  (php-create-regexp-for-classlike (regexp-opt '("class" "interface" "trait"))))

(defun php-get-current-element (re-pattern)
  "Return backward matched element by RE-PATTERN."
  (save-excursion
    (when (re-search-backward re-pattern nil t)
      (match-string-no-properties 1))))

;;;###autoload
(defun php-current-class ()
  "Insert current class name if cursor in class context."
  (interactive)
  (let ((matched (php-get-current-element php--re-classlike-pattern)))
    (when matched
      (insert (concat matched php-class-suffix-when-insert)))))

;;;###autoload
(defun php-current-namespace ()
  "Insert current namespace if cursor in namespace context."
  (interactive)
  (let ((matched (php-get-current-element php--re-namespace-pattern)))
    (when matched
      (insert (concat matched php-namespace-suffix-when-insert)))))


;;;###autoload
(dolist (pattern '("\\.php[s345t]?\\'" "/\\.php_cs\\(\\.dist\\)?\\'" "\\.phtml\\'" "/Amkfile\\'" "\\.amk\\'"))
  (add-to-list 'auto-mode-alist `(,pattern . php-mode) t))

(provide 'php-mode)

;;; php-mode.el ends here

;; Local Variables:
;; firestarter: ert-run-tests-interactively
;; End:
