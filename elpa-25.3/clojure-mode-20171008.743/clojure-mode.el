;;; clojure-mode.el --- Major mode for Clojure code -*- lexical-binding: t; -*-

;; Copyright © 2007-2017 Jeffrey Chu, Lennart Staflin, Phil Hagelberg
;; Copyright © 2013-2017 Bozhidar Batsov, Artur Malabarba
;;
;; Authors: Jeffrey Chu <jochu0@gmail.com>
;;       Lennart Staflin <lenst@lysator.liu.se>
;;       Phil Hagelberg <technomancy@gmail.com>
;;       Bozhidar Batsov <bozhidar@batsov.com>
;;       Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/clojure-emacs/clojure-mode
;; Package-Version: 20171008.743
;; Keywords: languages clojure clojurescript lisp
;; Version: 5.7.0-snapshot
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides font-lock, indentation, navigation and basic refactoring for the
;; Clojure programming language (http://clojure.org).

;; Using clojure-mode with paredit or smartparens is highly recommended.

;; Here are some example configurations:

;;   ;; require or autoload paredit-mode
;;   (add-hook 'clojure-mode-hook #'paredit-mode)

;;   ;; require or autoload smartparens
;;   (add-hook 'clojure-mode-hook #'smartparens-strict-mode)

;; See inf-clojure (http://github.com/clojure-emacs/inf-clojure) for
;; basic interaction with Clojure subprocesses.

;; See CIDER (http://github.com/clojure-emacs/cider) for
;; better interaction with subprocesses via nREPL.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:


(eval-when-compile
  (defvar calculate-lisp-indent-last-sexp)
  (defvar font-lock-beg)
  (defvar font-lock-end)
  (defvar paredit-space-for-delimiter-predicates)
  (defvar paredit-version)
  (defvar paredit-mode))

(require 'cl-lib)
(require 'imenu)
(require 'newcomment)
(require 'align)
(require 'subr-x)

(declare-function lisp-fill-paragraph  "lisp-mode" (&optional justify))

(defgroup clojure nil
  "Major mode for editing Clojure code."
  :prefix "clojure-"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/clojure-emacs/clojure-mode")
  :link '(emacs-commentary-link :tag "Commentary" "clojure-mode"))

(defconst clojure-mode-version "5.7.0-snapshot"
  "The current version of `clojure-mode'.")

(defface clojure-keyword-face
  '((t (:inherit font-lock-constant-face)))
  "Face used to font-lock Clojure keywords (:something)."
  :package-version '(clojure-mode . "3.0.0"))

(defface clojure-character-face
  '((t (:inherit font-lock-string-face)))
  "Face used to font-lock Clojure character literals."
  :package-version '(clojure-mode . "3.0.0"))

(defface clojure-interop-method-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face used to font-lock interop method names (camelCase)."
  :package-version '(clojure-mode . "3.0.0"))

(defcustom clojure-indent-style :always-align
  "Indentation style to use for function forms and macro forms.
There are two cases of interest configured by this variable.

- Case (A) is when at least one function argument is on the same
  line as the function name.
- Case (B) is the opposite (no arguments are on the same line as
  the function name).  Note that the body of macros is not
  affected by this variable, it is always indented by
  `lisp-body-indent' (default 2) spaces.

Note that this variable configures the indentation of function
forms (and function-like macros), it does not affect macros that
already use special indentation rules.

The possible values for this variable are keywords indicating how
to indent function forms.

    `:always-align' - Follow the same rules as `lisp-mode'.  All
    args are vertically aligned with the first arg in case (A),
    and vertically aligned with the function name in case (B).
    For instance:
        (reduce merge
                some-coll)
        (reduce
         merge
         some-coll)

    `:always-indent' - All args are indented like a macro body.
        (reduce merge
          some-coll)
        (reduce
          merge
          some-coll)

    `:align-arguments' - Case (A) is indented like `lisp', and
    case (B) is indented like a macro body.
        (reduce merge
                some-coll)
        (reduce
          merge
          some-coll)"
  :safe #'keywordp
  :type '(choice (const :tag "Same as `lisp-mode'" :always-align)
                 (const :tag "Indent like a macro body" :always-indent)
                 (const :tag "Indent like a macro body unless first arg is on the same line"
                        :align-arguments))
  :package-version '(clojure-mode . "5.2.0"))

(define-obsolete-variable-alias 'clojure-defun-style-default-indent
  'clojure-indent-style "5.2.0")

(defcustom clojure-use-backtracking-indent t
  "When non-nil, enable context sensitive indentation."
  :type 'boolean
  :safe 'booleanp)

(defcustom clojure-max-backtracking 3
  "Maximum amount to backtrack up a list to check for context."
  :type 'integer
  :safe 'integerp)

(defcustom clojure-docstring-fill-column fill-column
  "Value of `fill-column' to use when filling a docstring."
  :type 'integer
  :safe 'integerp)

(defcustom clojure-docstring-fill-prefix-width 2
  "Width of `fill-prefix' when filling a docstring.
The default value conforms with the de facto convention for
Clojure docstrings, aligning the second line with the opening
double quotes on the third column."
  :type 'integer
  :safe 'integerp)

(defcustom clojure-omit-space-between-tag-and-delimiters '(?\[ ?\{)
  "Allowed opening delimiter characters after a reader literal tag.
For example, \[ is allowed in :db/id[:db.part/user]."
  :type '(set (const :tag "[" ?\[)
              (const :tag "{" ?\{)
              (const :tag "(" ?\()
              (const :tag "\"" ?\"))
  :safe (lambda (value)
          (and (listp value)
               (cl-every 'characterp value))))

(defcustom clojure-build-tool-files '("project.clj" "build.boot" "build.gradle")
  "A list of files, which identify a Clojure project's root.
Out-of-the box `clojure-mode' understands lein, boot and gradle."
  :type '(repeat string)
  :package-version '(clojure-mode . "5.0.0")
  :safe (lambda (value)
          (and (listp value)
               (cl-every 'stringp value))))

(defcustom clojure-project-root-function #'clojure-project-root-path
  "Function to locate clojure project root directory."
  :type 'function
  :risky t
  :package-version '(clojure-mode . "5.7.0"))

(defcustom clojure-refactor-map-prefix (kbd "C-c C-r")
  "Clojure refactor keymap prefix."
  :type 'string
  :package-version '(clojure-mode . "5.6.0"))

(defvar clojure-refactor-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-t") #'clojure-thread)
    (define-key map (kbd "t") #'clojure-thread)
    (define-key map (kbd "C-u") #'clojure-unwind)
    (define-key map (kbd "u") #'clojure-unwind)
    (define-key map (kbd "C-f") #'clojure-thread-first-all)
    (define-key map (kbd "f") #'clojure-thread-first-all)
    (define-key map (kbd "C-l") #'clojure-thread-last-all)
    (define-key map (kbd "l") #'clojure-thread-last-all)
    (define-key map (kbd "C-a") #'clojure-unwind-all)
    (define-key map (kbd "a") #'clojure-unwind-all)
    (define-key map (kbd "C-p") #'clojure-cycle-privacy)
    (define-key map (kbd "p") #'clojure-cycle-privacy)
    (define-key map (kbd "C-(") #'clojure-convert-collection-to-list)
    (define-key map (kbd "(") #'clojure-convert-collection-to-list)
    (define-key map (kbd "C-'") #'clojure-convert-collection-to-quoted-list)
    (define-key map (kbd "'") #'clojure-convert-collection-to-quoted-list)
    (define-key map (kbd "C-{") #'clojure-convert-collection-to-map)
    (define-key map (kbd "{") #'clojure-convert-collection-to-map)
    (define-key map (kbd "C-[") #'clojure-convert-collection-to-vector)
    (define-key map (kbd "[") #'clojure-convert-collection-to-vector)
    (define-key map (kbd "C-#") #'clojure-convert-collection-to-set)
    (define-key map (kbd "#") #'clojure-convert-collection-to-set)
    (define-key map (kbd "C-i") #'clojure-cycle-if)
    (define-key map (kbd "i") #'clojure-cycle-if)
    (define-key map (kbd "C-w") #'clojure-cycle-when)
    (define-key map (kbd "w") #'clojure-cycle-when)
    (define-key map (kbd "C-o") #'clojure-cycle-not)
    (define-key map (kbd "o") #'clojure-cycle-not)
    (define-key map (kbd "n i") #'clojure-insert-ns-form)
    (define-key map (kbd "n h") #'clojure-insert-ns-form-at-point)
    (define-key map (kbd "n u") #'clojure-update-ns)
    (define-key map (kbd "n s") #'clojure-sort-ns)
    (define-key map (kbd "s i") #'clojure-introduce-let)
    (define-key map (kbd "s m") #'clojure-move-to-let)
    (define-key map (kbd "s f") #'clojure-let-forward-slurp-sexp)
    (define-key map (kbd "s b") #'clojure-let-backward-slurp-sexp)
    map)
  "Keymap for Clojure refactoring commands.")
(fset 'clojure-refactor-map clojure-refactor-map)

(defvar clojure-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-:") #'clojure-toggle-keyword-string)
    (define-key map (kbd "C-c SPC") #'clojure-align)
    (define-key map clojure-refactor-map-prefix 'clojure-refactor-map)
    (easy-menu-define clojure-mode-menu map "Clojure Mode Menu"
      '("Clojure"
        ["Toggle between string & keyword" clojure-toggle-keyword-string]
        ["Align expression" clojure-align]
        ["Cycle privacy" clojure-cycle-privacy]
        ["Cycle if, if-not" clojure-cycle-if]
        ["Cycle when, when-not" clojure-cycle-when]
        ["Cycle not" clojure-cycle-not]
        ("ns forms"
         ["Insert ns form at the top" clojure-insert-ns-form]
         ["Insert ns form here" clojure-insert-ns-form-at-point]
         ["Update ns form" clojure-update-ns]
         ["Sort ns form" clojure-sort-ns])
        ("Convert collection"
         ["Convert to list" clojure-convert-collection-to-list]
         ["Convert to quoted list" clojure-convert-collection-to-quoted-list]
         ["Convert to map" clojure-convert-collection-to-map]
         ["Convert to vector" clojure-convert-collection-to-vector]
         ["Convert to set" clojure-convert-collection-to-set])
        ("Refactor -> and ->>"
         ["Thread once more" clojure-thread]
         ["Fully thread a form with ->" clojure-thread-first-all]
         ["Fully thread a form with ->>" clojure-thread-last-all]
         "--"
         ["Unwind once" clojure-unwind]
         ["Fully unwind a threading macro" clojure-unwind-all])
        ("Let expression"
         ["Introduce let" clojure-introduce-let]
         ["Move to let" clojure-move-to-let]
         ["Forward slurp form into let" clojure-let-forward-slurp-sexp]
         ["Backward slurp form into let" clojure-let-backward-slurp-sexp])
        ("Documentation"
         ["View a Clojure guide" clojure-view-guide]
         ["View a Clojure reference section" clojure-view-reference-section]
         ["View the Clojure cheatsheet" clojure-view-cheatsheet]
         ["View the Clojure Grimoire" clojure-view-grimoire]
         ["View the Clojure style guide" clojure-view-style-guide])
        "--"
        ["Report a clojure-mode bug" clojure-mode-report-bug]
        ["Clojure-mode version" clojure-mode-display-version]))
    map)
  "Keymap for Clojure mode.")

(defvar clojure-mode-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?? "_ p" table) ; ? is a prefix outside symbols
    (modify-syntax-entry ?# "_ p" table) ; # is allowed inside keywords (#399)
    (modify-syntax-entry ?~ "'" table)
    (modify-syntax-entry ?^ "'" table)
    (modify-syntax-entry ?@ "'" table)
    table)
  "Syntax table for Clojure mode.
Inherits from `emacs-lisp-mode-syntax-table'.")

(defconst clojure--prettify-symbols-alist
  '(("fn"  . ?λ)))

(defvar-local clojure-expected-ns-function nil
  "The function used to determine the expected namespace of a file.
`clojure-mode' ships a basic function named `clojure-expected-ns'
that does basic heuristics to figure this out.
CIDER provides a more complex version which does classpath analysis.")

(defun clojure-mode-display-version ()
  "Display the current `clojure-mode-version' in the minibuffer."
  (interactive)
  (message "clojure-mode (version %s)" clojure-mode-version))

(defconst clojure-mode-report-bug-url "https://github.com/clojure-emacs/clojure-mode/issues/new"
  "The URL to report a `clojure-mode' issue.")

(defun clojure-mode-report-bug ()
  "Report a bug in your default browser."
  (interactive)
  (browse-url clojure-mode-report-bug-url))

(defconst clojure-guides-base-url "https://clojure.org/guides/"
  "The base URL for official Clojure guides.")

(defconst clojure-guides '(("Getting Started" . "getting_started")
                           ("FAQ" . "faq")
                           ("spec" . "spec")
                           ("Destructuring" . "destructuring")
                           ("Threading Macros" . "threading_macros")
                           ("Comparators" . "comparators")
                           ("Reader Conditionals" . "reader_conditionals"))
  "A list of all official Clojure guides.")

(defun clojure-view-guide ()
  "Open a Clojure guide in your default browser.

The command will prompt you to select one of the available guides."
  (interactive)
  (let ((guide (completing-read "Select a guide: " (mapcar #'car clojure-guides))))
    (when guide
      (let ((guide-url (concat clojure-guides-base-url (cdr (assoc guide clojure-guides)))))
        (browse-url guide-url)))))

(defconst clojure-reference-base-url "https://clojure.org/reference/"
  "The base URL for the official Clojure reference.")

(defconst clojure-reference-sections '(("The Reader" . "reader")
                                       ("The REPL and main" . "repl_and_main")
                                       ("Evaluation" . "evaluation")
                                       ("Special Forms" . "special_forms")
                                       ("Macros" . "macros")
                                       ("Other Functions" . "other_functions")
                                       ("Data Structures" . "data_structures")
                                       ("Datatypes" . "datatypes")
                                       ("Sequences" . "sequences")
                                       ("Transients" . "transients")
                                       ("Transducers" . "transducers")
                                       ("Multimethods and Hierarchies" . "multimethods")
                                       ("Protocols" . "protocols")
                                       ("Metadata" . "metadata")
                                       ("Namespaces" . "namespaces")
                                       ("Libs" . "libs")
                                       ("Vars and Environments" . "vars")
                                       ("Refs and Transactions" . "refs")
                                       ("Agents" . "agents")
                                       ("Atoms" . "atoms")
                                       ("Reducers" . "reducers")
                                       ("Java Interop" . "java_interop")
                                       ("Compilation and Class Generation" . "compilation")
                                       ("Other Libraries" . "other_libraries")
                                       ("Differences with Lisps" . "lisps")))

(defun clojure-view-reference-section ()
  "Open a Clojure reference section in your default browser.

The command will prompt you to select one of the available sections."
  (interactive)
  (let ((section (completing-read "Select a reference section: " (mapcar #'car clojure-reference-sections))))
    (when section
      (let ((section-url (concat clojure-reference-base-url (cdr (assoc section clojure-reference-sections)))))
        (browse-url section-url)))))

(defconst clojure-cheatsheet-url "http://clojure.org/api/cheatsheet"
  "The URL of the official Clojure cheatsheet.")

(defun clojure-view-cheatsheet ()
  "Open the Clojure cheatsheet in your default browser."
  (interactive)
  (browse-url clojure-cheatsheet-url))

(defconst clojure-grimoire-url "https://www.conj.io/"
  "The URL of the Grimoire community documentation site.")

(defun clojure-view-grimoire ()
  "Open the Clojure Grimoire in your default browser."
  (interactive)
  (browse-url clojure-grimoire-url))

(defconst clojure-style-guide-url "https://github.com/bbatsov/clojure-style-guide"
  "The URL of the Clojure style guide.")

(defun clojure-view-style-guide ()
  "Open the Clojure style guide in your default browser."
  (interactive)
  (browse-url clojure-style-guide-url))

(defun clojure-space-for-delimiter-p (endp delim)
  "Prevent paredit from inserting useless spaces.
See `paredit-space-for-delimiter-predicates' for the meaning of
ENDP and DELIM."
  (or endp
      (not (memq delim '(?\" ?{ ?\( )))
      (not (or (derived-mode-p 'clojure-mode)
               (derived-mode-p 'cider-repl-mode)))
      (save-excursion
        (backward-char)
        (cond ((eq (char-after) ?#)
               (and (not (bobp))
                    (or (char-equal ?w (char-syntax (char-before)))
                        (char-equal ?_ (char-syntax (char-before))))))
              ((and (eq delim ?\()
                    (eq (char-after) ??)
                    (eq (char-before) ?#))
               nil)
              (t)))))

(defun clojure-no-space-after-tag (endp delimiter)
  "Prevent inserting a space after a reader-literal tag?

When a reader-literal tag is followed be an opening delimiter
listed in `clojure-omit-space-between-tag-and-delimiters', this
function returns t.

This allows you to write things like #db/id[:db.part/user]
without inserting a space between the tag and the opening
bracket.

See `paredit-space-for-delimiter-predicates' for the meaning of
ENDP and DELIMITER."
  (if endp
      t
    (or (not (member delimiter clojure-omit-space-between-tag-and-delimiters))
        (save-excursion
          (let ((orig-point (point)))
            (not (and (re-search-backward
                       "#\\([a-zA-Z0-9._-]+/\\)?[a-zA-Z0-9._-]+"
                       (line-beginning-position)
                       t)
                      (= orig-point (match-end 0)))))))))

(declare-function paredit-open-curly "ext:paredit")
(declare-function paredit-close-curly "ext:paredit")
(declare-function paredit-convolute-sexp "ext:paredit")

(defun clojure--replace-let-bindings-and-indent (orig-fun &rest args)
  "Advise ORIG-FUN to replace let bindings.

Sexps are replace by their bound name if a let form was
convoluted.

ORIG-FUN should be `paredit-convolute-sexp'.

ARGS are passed to ORIG-FUN, as with all advice."
  (save-excursion
    (backward-sexp)
    (when (looking-back clojure--let-regexp)
      (clojure--replace-sexps-with-bindings-and-indent))))

(defun clojure-paredit-setup (&optional keymap)
  "Make \"paredit-mode\" play nice with `clojure-mode'.

If an optional KEYMAP is passed the changes are applied to it,
instead of to `clojure-mode-map'.
Also advice `paredit-convolute-sexp' when used on a let form as drop in
replacement for `cljr-expand-let`."
  (when (>= paredit-version 21)
    (let ((keymap (or keymap clojure-mode-map)))
      (define-key keymap "{" #'paredit-open-curly)
      (define-key keymap "}" #'paredit-close-curly))
    (add-to-list 'paredit-space-for-delimiter-predicates
                 #'clojure-space-for-delimiter-p)
    (add-to-list 'paredit-space-for-delimiter-predicates
                 #'clojure-no-space-after-tag)
    (advice-add 'paredit-convolute-sexp :after #'clojure--replace-let-bindings-and-indent)))

(defun clojure-mode-variables ()
  "Set up initial buffer-local variables for Clojure mode."
  (add-to-list 'imenu-generic-expression '(nil clojure-match-next-def 0))
  (setq-local indent-tabs-mode nil)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local outline-regexp ";;;\\(;* [^ \t\n]\\)\\|(")
  (setq-local outline-level 'lisp-outline-level)
  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-add 1) ; default to `;;' in comment-region
  (setq-local comment-column 40)
  (setq-local comment-use-syntax t)
  (setq-local multibyte-syntax-as-symbol t)
  (setq-local electric-pair-skip-whitespace 'chomp)
  (setq-local electric-pair-open-newline-between-pairs nil)
  (setq-local fill-paragraph-function #'clojure-fill-paragraph)
  (setq-local adaptive-fill-function #'clojure-adaptive-fill-function)
  (setq-local normal-auto-fill-function #'clojure-auto-fill-function)
  (setq-local comment-start-skip
              "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (setq-local indent-line-function #'clojure-indent-line)
  (setq-local indent-region-function #'clojure-indent-region)
  (setq-local lisp-indent-function #'clojure-indent-function)
  (setq-local lisp-doc-string-elt-property 'clojure-doc-string-elt)
  (setq-local clojure-expected-ns-function #'clojure-expected-ns)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local prettify-symbols-alist clojure--prettify-symbols-alist)
  (setq-local open-paren-in-column-0-is-defun-start nil))

;;;###autoload
(define-derived-mode clojure-mode prog-mode "Clojure"
  "Major mode for editing Clojure code.

\\{clojure-mode-map}"
  (clojure-mode-variables)
  (clojure-font-lock-setup)
  (add-hook 'paredit-mode-hook #'clojure-paredit-setup)
  ;; `electric-layout-post-self-insert-function' prevents indentation in strings
  ;; and comments, force indentation in docstrings:
  (add-hook 'electric-indent-functions
            (lambda (char) (if (clojure-in-docstring-p) 'do-indent))))

(defcustom clojure-verify-major-mode t
  "If non-nil, warn when activating the wrong `major-mode'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(clojure-mode "5.3.0"))

(defun clojure--check-wrong-major-mode ()
  "Check if the current `major-mode' matches the file extension.

If it doesn't, issue a warning if `clojure-verify-major-mode' is
non-nil."
  (when (and clojure-verify-major-mode
             (stringp (buffer-file-name)))
    (let* ((case-fold-search t)
           (problem (cond ((and (string-match "\\.clj\\'" (buffer-file-name))
                                (not (eq major-mode 'clojure-mode)))
                           'clojure-mode)
                          ((and (string-match "\\.cljs\\'" (buffer-file-name))
                                (not (eq major-mode 'clojurescript-mode)))
                           'clojurescript-mode)
                          ((and (string-match "\\.cljc\\'" (buffer-file-name))
                                (not (eq major-mode 'clojurec-mode)))
                           'clojurec-mode)
                          ((and (string-match "\\.cljx\\'" (buffer-file-name))
                                (not (eq major-mode 'clojurex-mode)))
                           'clojurex-mode))))
      (when problem
        (message "[WARNING] %s activated `%s' instead of `%s' in this buffer.
This could cause problems.
\(See `clojure-verify-major-mode' to disable this message.)"
                 (if (eq major-mode real-this-command)
                     "You have"
                   "Something in your configuration")
                 major-mode
                 problem)))))

(add-hook 'clojure-mode-hook #'clojure--check-wrong-major-mode)

(defsubst clojure-in-docstring-p ()
  "Check whether point is in a docstring."
  (let ((ppss (syntax-ppss)))
    ;; are we in a string?
    (when (nth 3 ppss)
      ;; check font lock at the start of the string
      (eq (get-text-property (nth 8 ppss) 'face)
          'font-lock-doc-face))))

(defsubst clojure-docstring-fill-prefix ()
  "The prefix string used by `clojure-fill-paragraph'.
It is simply `clojure-docstring-fill-prefix-width' number of spaces."
  (make-string clojure-docstring-fill-prefix-width ? ))

(defun clojure-adaptive-fill-function ()
  "Clojure adaptive fill function.
This only takes care of filling docstring correctly."
  (when (clojure-in-docstring-p)
    (clojure-docstring-fill-prefix)))

(defun clojure-fill-paragraph (&optional justify)
  "Like `fill-paragraph', but can handle Clojure docstrings.
If JUSTIFY is non-nil, justify as well as fill the paragraph."
  (if (clojure-in-docstring-p)
      (let ((paragraph-start
             (concat paragraph-start
                     "\\|\\s-*\\([(:\"[]\\|~@\\|`(\\|#'(\\)"))
            (paragraph-separate
             (concat paragraph-separate "\\|\\s-*\".*[,\\.]$"))
            (fill-column (or clojure-docstring-fill-column fill-column))
            (fill-prefix (clojure-docstring-fill-prefix)))
        ;; we are in a string and string start pos (8th element) is non-nil
        (let* ((beg-doc (nth 8 (syntax-ppss)))
               (end-doc (save-excursion
                          (goto-char beg-doc)
                          (or (ignore-errors (forward-sexp) (point))
                              (point-max)))))
          (save-restriction
            (narrow-to-region beg-doc end-doc)
            (fill-paragraph justify))))
    (let ((paragraph-start (concat paragraph-start
                                   "\\|\\s-*\\([(:\"[]\\|`(\\|#'(\\)"))
          (paragraph-separate
           (concat paragraph-separate "\\|\\s-*\".*[,\\.[]$")))
      (or (fill-comment-paragraph justify)
          (fill-paragraph justify))
      ;; Always return `t'
      t)))

(defun clojure-auto-fill-function ()
  "Clojure auto-fill function."
  ;; Check if auto-filling is meaningful.
  (let ((fc (current-fill-column)))
    (when (and fc (> (current-column) fc))
      (let ((fill-column (if (clojure-in-docstring-p)
                             clojure-docstring-fill-column
                           fill-column))
            (fill-prefix (clojure-adaptive-fill-function)))
        (do-auto-fill)))))


;;; #_ comments font-locking
;; Code heavily borrowed from Slime.
;; https://github.com/slime/slime/blob/master/contrib/slime-fontifying-fu.el#L186
(defvar clojure--comment-macro-regexp
  (rx "#_" (* " ") (group-n 1 (not (any " "))))
  "Regexp matching the start of a comment sexp.
The beginning of match-group 1 should be before the sexp to be
marked as a comment.  The end of sexp is found with
`clojure-forward-logical-sexp'.")

(defvar clojure--reader-and-comment-regexp
  "#_ *\\(?1:[^ ]\\)\\|\\(?1:(comment\\_>\\)"
  "Regexp matching both `#_' macro and a comment sexp." )

(defcustom clojure-comment-regexp clojure--comment-macro-regexp
  "Comment mode.

The possible values for this variable are keywords indicating
what is considered a comment (affecting font locking).

    - Reader macro `#_' only - the default
    - Reader macro `#_' and `(comment)'"
  :type '(choice (const :tag "Reader macro `#_' and `(comment)'" clojure--reader-and-comment-regexp)
                 (other :tag "Reader macro `#_' only" clojure--comment-macro-regexp))
  :package-version '(clojure-mode . "5.7.0"))

(defun clojure--search-comment-macro-internal (limit)
  "Search for a comment forward stopping at LIMIT."
  (when (search-forward-regexp clojure-comment-regexp limit t)
    (let* ((md (match-data))
           (start (match-beginning 1))
           (state (syntax-ppss start)))
      ;; inside string or comment?
      (if (or (nth 3 state)
              (nth 4 state))
          (clojure--search-comment-macro-internal limit)
        (goto-char start)
        (clojure-forward-logical-sexp 1)
        ;; Data for (match-end 1).
        (setf (elt md 3) (point))
        (set-match-data md)
        t))))

(defun clojure--search-comment-macro (limit)
  "Find comment macros and set the match data.
Search from point up to LIMIT.  The region that should be
considered a comment is between `(match-beginning 1)'
and `(match-end 1)'."
  (let ((result 'retry))
    (while (and (eq result 'retry) (<= (point) limit))
      (condition-case nil
          (setq result (clojure--search-comment-macro-internal limit))
        (end-of-file (setq result nil))
        (scan-error  (setq result 'retry))))
    result))


;;; General font-locking
(defun clojure-match-next-def ()
  "Scans the buffer backwards for the next \"top-level\" definition.
Called by `imenu--generic-function'."
  ;; we have to take into account namespace-definition forms
  ;; e.g. s/defn
  (when (re-search-backward "^[ \t]*(\\([a-z0-9.-]+/\\)?\\(def\\sw*\\)" nil t)
    (save-excursion
      (let (found?
            (deftype (match-string 2))
            (start (point)))
        (down-list)
        (forward-sexp)
        (while (not found?)
          (ignore-errors
            (forward-sexp))
          (or (when (char-equal ?\[ (char-after (point)))
                (backward-sexp))
              (when (char-equal ?\) (char-after (point)))
                (backward-sexp)))
          (cl-destructuring-bind (def-beg . def-end) (bounds-of-thing-at-point 'sexp)
            (if (char-equal ?^ (char-after def-beg))
                (progn (forward-sexp) (backward-sexp))
              (setq found? t)
              (when (string= deftype "defmethod")
                (setq def-end (progn (goto-char def-end)
                                     (forward-sexp)
                                     (point))))
              (set-match-data (list def-beg def-end)))))
        (goto-char start)))))

(eval-and-compile
  (defconst clojure--sym-forbidden-rest-chars "][\";\'@\\^`~\(\)\{\}\\,\s\t\n\r"
    "A list of chars that a Clojure symbol cannot contain.
See definition of 'macros': URL `http://git.io/vRGLD'.")
  (defconst clojure--sym-forbidden-1st-chars (concat clojure--sym-forbidden-rest-chars "0-9:")
    "A list of chars that a Clojure symbol cannot start with.
See the for-loop: URL `http://git.io/vRGTj' lines: URL
`http://git.io/vRGIh', URL `http://git.io/vRGLE' and value
definition of 'macros': URL `http://git.io/vRGLD'.")
  (defconst clojure--sym-regexp
    (concat "[^" clojure--sym-forbidden-1st-chars "][^" clojure--sym-forbidden-rest-chars "]*")
    "A regexp matching a Clojure symbol or namespace alias.
Matches the rule `clojure--sym-forbidden-1st-chars' followed by
any number of matches of `clojure--sym-forbidden-rest-chars'."))

(defconst clojure-font-lock-keywords
  (eval-when-compile
    `( ;; Top-level variable definition
      (,(concat "(\\(?:clojure.core/\\)?\\("
                (regexp-opt '("def" "defonce"))
                ;; variable declarations
                "\\)\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-variable-name-face nil t))
      ;; Type definition
      (,(concat "(\\(?:clojure.core/\\)?\\("
                (regexp-opt '("defstruct" "deftype" "defprotocol"
                              "defrecord"))
                ;; type declarations
                "\\)\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-type-face nil t))
      ;; Function definition (anything that starts with def and is not
      ;; listed above)
      (,(concat "(\\(?:" clojure--sym-regexp "/\\)?"
                "\\(def[^ \r\n\t]*\\)"
                ;; Function declarations
                "\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      ;; (fn name? args ...)
      (,(concat "(\\(?:clojure.core/\\)?\\(fn\\)[ \t]+"
                ;; Possibly type
                "\\(?:#?^\\sw+[ \t]*\\)?"
                ;; Possibly name
                "\\(\\sw+\\)?" )
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      ;; lambda arguments - %, %&, %1, %2, etc
      ("\\<%[&1-9]?" (0 font-lock-variable-name-face))
      ;; Special forms
      (,(concat
         "("
         (regexp-opt
          '("def" "do" "if" "let" "let*" "var" "fn" "fn*" "loop" "loop*"
            "recur" "throw" "try" "catch" "finally"
            "set!" "new" "."
            "monitor-enter" "monitor-exit" "quote") t)
         "\\>")
       1 font-lock-keyword-face)
      ;; Built-in binding and flow of control forms
      (,(concat
         "(\\(?:clojure.core/\\)?"
         (regexp-opt
          '("letfn" "case" "cond" "cond->" "cond->>" "condp"
            "for" "when" "when-not" "when-let" "when-first" "when-some"
            "if-let" "if-not" "if-some"
            ".." "->" "->>" "as->" "doto" "and" "or"
            "dosync" "doseq" "dotimes" "dorun" "doall"
            "ns" "in-ns"
            "with-open" "with-local-vars" "binding"
            "with-redefs" "with-redefs-fn"
            "declare") t)
         "\\>")
       1 font-lock-keyword-face)
      ;; Macros similar to let, when, and while
      (,(rx symbol-start
            (or "let" "when" "while") "-"
            (1+ (or (syntax word) (syntax symbol)))
            symbol-end)
       0 font-lock-keyword-face)
      (,(concat
         "\\<"
         (regexp-opt
          '("*1" "*2" "*3" "*agent*"
            "*allow-unresolved-vars*" "*assert*" "*clojure-version*"
            "*command-line-args*" "*compile-files*"
            "*compile-path*" "*data-readers*" "*default-data-reader-fn*"
            "*e" "*err*" "*file*" "*flush-on-newline*"
            "*in*" "*macro-meta*" "*math-context*" "*ns*" "*out*"
            "*print-dup*" "*print-length*" "*print-level*"
            "*print-meta*" "*print-readably*"
            "*read-eval*" "*source-path*"
            "*unchecked-math*"
            "*use-context-classloader*" "*warn-on-reflection*")
          t)
         "\\>")
       0 font-lock-builtin-face)
      ;; Dynamic variables - *something* or @*something*
      ("\\(?:\\<\\|/\\)@?\\(\\*[a-z-]*\\*\\)\\>" 1 font-lock-variable-name-face)
      ;; Global constants - nil, true, false
      (,(concat
         "\\<"
         (regexp-opt
          '("true" "false" "nil") t)
         "\\>")
       0 font-lock-constant-face)
      ;; Character literals - \1, \a, \newline, \u0000
      ("\\\\\\([[:punct:]]\\|[a-z0-9]+\\>\\)" 0 'clojure-character-face)
      ;; foo/ Foo/ @Foo/ /FooBar
      (,(concat "\\(?:\\<:?\\|\\.\\)@?\\(" clojure--sym-regexp "\\)\\(/\\)")
       (1 font-lock-type-face) (2 'default))
      ;; Constant values (keywords), including as metadata e.g. ^:static
      ("\\<^?\\(:\\(\\sw\\|\\s_\\)+\\(\\>\\|\\_>\\)\\)" 1 'clojure-keyword-face append)
      ;; Java interop highlighting
      ;; CONST SOME_CONST (optionally prefixed by /)
      ("\\(?:\\<\\|/\\)\\([A-Z]+\\|\\([A-Z]+_[A-Z1-9_]+\\)\\)\\>" 1 font-lock-constant-face)
      ;; .foo .barBaz .qux01 .-flibble .-flibbleWobble
      ("\\<\\.-?[a-z][a-zA-Z0-9]*\\>" 0 'clojure-interop-method-face)
      ;; Foo Bar$Baz Qux_ World_OpenUDP Foo. Babylon15.
      ("\\(?:\\<\\|\\.\\|/\\|#?^\\)\\([A-Z][a-zA-Z0-9_]*[a-zA-Z0-9$_]+\\.?\\>\\)" 1 font-lock-type-face)
      ;; foo.bar.baz
      ("\\<^?\\([a-z][a-z0-9_-]+\\.\\([a-z][a-z0-9_-]*\\.?\\)+\\)" 1 font-lock-type-face)
      ;; (ns namespace) - special handling for single segment namespaces
      (,(concat "(\\<ns\\>[ \r\n\t]*"
                ;; Possibly metadata
                "\\(?:\\^?{[^}]+}[ \r\n\t]*\\)*"
                ;; namespace
                "\\([a-z0-9-]+\\)")
       (1 font-lock-type-face nil t))
      ;; fooBar
      ("\\(?:\\<\\|/\\)\\([a-z]+[A-Z]+[a-zA-Z0-9$]*\\>\\)" 1 'clojure-interop-method-face)
      ;; #_ and (comment ...) macros.
      (clojure--search-comment-macro 1 font-lock-comment-face t)
      ;; Highlight `code` marks, just like `elisp'.
      (,(rx "`" (group-n 1 (optional "#'")
                         (+ (or (syntax symbol) (syntax word)))) "`")
       (1 'font-lock-constant-face prepend))
      ;; Highlight escaped characters in strings.
      (clojure-font-lock-escaped-chars 0 'bold prepend)
      ;; Highlight grouping constructs in regular expressions
      (clojure-font-lock-regexp-groups
       (1 'font-lock-regexp-grouping-construct prepend))))
  "Default expressions to highlight in Clojure mode.")

(defun clojure-font-lock-syntactic-face-function (state)
  "Find and highlight text with a Clojure-friendly syntax table.

This function is passed to `font-lock-syntactic-face-function',
which is called with a single parameter, STATE (which is, in
turn, returned by `parse-partial-sexp' at the beginning of the
highlighted region)."
  (if (nth 3 state)
      ;; This might be a (doc)string or a |...| symbol.
      (let ((startpos (nth 8 state)))
        (if (eq (char-after startpos) ?|)
            ;; This is not a string, but a |...| symbol.
            nil
          (let* ((listbeg (nth 1 state))
                 (firstsym (and listbeg
                                (save-excursion
                                  (goto-char listbeg)
                                  (and (looking-at "([ \t\n]*\\(\\(\\sw\\|\\s_\\)+\\)")
                                       (match-string 1)))))
                 (docelt (and firstsym
                              (function-get (intern-soft firstsym)
                                            lisp-doc-string-elt-property))))
            (if (and docelt
                     ;; It's a string in a form that can have a docstring.
                     ;; Check whether it's in docstring position.
                     (save-excursion
                       (when (functionp docelt)
                         (goto-char (match-end 1))
                         (setq docelt (funcall docelt)))
                       (goto-char listbeg)
                       (forward-char 1)
                       (ignore-errors
                         (while (and (> docelt 0) (< (point) startpos)
                                     (progn (forward-sexp 1) t))
                           ;; ignore metadata and type hints
                           (unless (looking-at "[ \n\t]*\\(\\^[A-Z:].+\\|\\^?{.+\\)")
                             (setq docelt (1- docelt)))))
                       (and (zerop docelt) (<= (point) startpos)
                            (progn (forward-comment (point-max)) t)
                            (= (point) (nth 8 state)))))
                font-lock-doc-face
              font-lock-string-face))))
    font-lock-comment-face))

(defun clojure-font-lock-setup ()
  "Configures font-lock for editing Clojure code."
  (setq-local font-lock-multiline t)
  (add-to-list 'font-lock-extend-region-functions
               #'clojure-font-lock-extend-region-def t)
  (setq font-lock-defaults
        '(clojure-font-lock-keywords    ; keywords
          nil nil
          (("+-*/.<>=!?$%_&:" . "w")) ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . clojure-font-lock-syntactic-face-function))))

(defun clojure-font-lock-def-at-point (point)
  "Range between the top-most def* and the fourth element after POINT.
Note that this means that there is no guarantee of proper font
locking in def* forms that are not at top level."
  (goto-char point)
  (ignore-errors
    (beginning-of-defun))

  (let ((beg-def (point)))
    (when (and (not (= point beg-def))
               (looking-at "(def"))
      (ignore-errors
        ;; move forward as much as possible until failure (or success)
        (forward-char)
        (dotimes (_ 4)
          (forward-sexp)))
      (cons beg-def (point)))))

(defun clojure-font-lock-extend-region-def ()
  "Set region boundaries to include the first four elements of def* forms."
  (let ((changed nil))
    (let ((def (clojure-font-lock-def-at-point font-lock-beg)))
      (when def
        (cl-destructuring-bind (def-beg . def-end) def
          (when (and (< def-beg font-lock-beg)
                     (< font-lock-beg def-end))
            (setq font-lock-beg def-beg
                  changed t)))))
    (let ((def (clojure-font-lock-def-at-point font-lock-end)))
      (when def
        (cl-destructuring-bind (def-beg . def-end) def
          (when (and (< def-beg font-lock-end)
                     (< font-lock-end def-end))
            (setq font-lock-end def-end
                  changed t)))))
    changed))

(defun clojure--font-locked-as-string-p (&optional regexp)
  "Non-nil if the char before point is font-locked as a string.
If REGEXP is non-nil, also check whether current string is
preceeded by a #."
  (let ((face (get-text-property (1- (point)) 'face)))
    (and (or (and (listp face)
                  (memq 'font-lock-string-face face))
             (eq 'font-lock-string-face face))
         (or (clojure-string-start t)
             (unless regexp
               (clojure-string-start nil))))))

(defun clojure-font-lock-escaped-chars (bound)
  "Highlight \escaped chars in strings.
BOUND denotes a buffer position to limit the search."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward "\\\\." bound t))

      (setq found (clojure--font-locked-as-string-p)))
    found))

(defun clojure-font-lock-regexp-groups (bound)
  "Highlight grouping constructs in regular expression.

BOUND denotes the maximum number of characters (relative to the
point) to check."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward (eval-when-compile
                                     (concat
                                      ;; A group may start using several alternatives:
                                      "\\(\\(?:"
                                      ;; 1. (? special groups
                                      "(\\?\\(?:"
                                      ;; a) non-capturing group (?:X)
                                      ;; b) independent non-capturing group (?>X)
                                      ;; c) zero-width positive lookahead (?=X)
                                      ;; d) zero-width negative lookahead (?!X)
                                      "[:=!>]\\|"
                                      ;; e) zero-width positive lookbehind (?<=X)
                                      ;; f) zero-width negative lookbehind (?<!X)
                                      "<[=!]\\|"
                                      ;; g) named capturing group (?<name>X)
                                      "<[[:alnum:]]+>"
                                      "\\)\\|" ;; end of special groups
                                      ;; 2. normal capturing groups (
                                      ;; 3. we also highlight alternative
                                      ;; separarators |, and closing parens )
                                      "[|()]"
                                      "\\)\\)"))
                                   bound t))
      (setq found (clojure--font-locked-as-string-p 'regexp)))
    found))

;; Docstring positions
(put 'ns 'clojure-doc-string-elt 2)
(put 'def 'clojure-doc-string-elt 2)
(put 'defn 'clojure-doc-string-elt 2)
(put 'defn- 'clojure-doc-string-elt 2)
(put 'defmulti 'clojure-doc-string-elt 2)
(put 'defmacro 'clojure-doc-string-elt 2)
(put 'definline 'clojure-doc-string-elt 2)
(put 'defprotocol 'clojure-doc-string-elt 2)

;;; Vertical alignment
(defcustom clojure-align-forms-automatically nil
  "If non-nil, vertically align some forms automatically.
Automatically means it is done as part of indenting code.  This
applies to binding forms (`clojure-align-binding-forms'), to cond
forms (`clojure-align-cond-forms') and to map literals.  For
instance, selecting a map a hitting \\<clojure-mode-map>`\\[indent-for-tab-command]'
will align the values like this:
    {:some-key 10
     :key2     20}"
  :package-version '(clojure-mode . "5.1")
  :safe #'booleanp
  :type 'boolean)

(defcustom clojure-align-binding-forms
  '("let" "when-let" "when-some" "if-let" "if-some" "binding" "loop"
    "doseq" "for" "with-open" "with-local-vars" "with-redefs")
  "List of strings matching forms that have binding forms."
  :package-version '(clojure-mode . "5.1")
  :safe #'listp
  :type '(repeat string))

(defcustom clojure-align-cond-forms '("condp" "cond" "cond->" "cond->>" "case" "are")
  "List of strings identifying cond-like forms."
  :package-version '(clojure-mode . "5.1")
  :safe #'listp
  :type '(repeat string))

(defun clojure--position-for-alignment ()
  "Non-nil if the sexp around point should be automatically aligned.
This function expects to be called immediately after an
open-brace or after the function symbol in a function call.

First check if the sexp around point is a map literal, or is a
call to one of the vars listed in `clojure-align-cond-forms'.  If
it isn't, return nil.  If it is, return non-nil and place point
immediately before the forms that should be aligned.

For instance, in a map literal point is left immediately before
the first key; while, in a let-binding, point is left inside the
binding vector and immediately before the first binding
construct."
  ;; Are we in a map?
  (or (and (eq (char-before) ?{)
           (not (eq (char-before (1- (point))) ?\#)))
      ;; Are we in a cond form?
      (let* ((fun    (car (member (thing-at-point 'symbol) clojure-align-cond-forms)))
             (method (and fun (clojure--get-indent-method fun)))
             ;; The number of special arguments in the cond form is
             ;; the number of sexps we skip before aligning.
             (skip   (cond ((numberp method) method)
                           ((null method) 0)
                           ((sequencep method) (elt method 0)))))
        (when (and fun (numberp skip))
          (clojure-forward-logical-sexp skip)
          (comment-forward (point-max))
          fun)) ; Return non-nil (the var name).
      ;; Are we in a let-like form?
      (when (member (thing-at-point 'symbol)
                    clojure-align-binding-forms)
        ;; Position inside the binding vector.
        (clojure-forward-logical-sexp)
        (backward-sexp)
        (when (eq (char-after) ?\[)
          (forward-char 1)
          (comment-forward (point-max))
          ;; Return non-nil.
          t))))

(defun clojure--find-sexp-to-align (end)
  "Non-nil if there's a sexp ahead to be aligned before END.
Place point as in `clojure--position-for-alignment'."
  ;; Look for a relevant sexp.
  (let ((found))
    (while (and (not found)
                (search-forward-regexp
                 (concat "{\\|(" (regexp-opt
                                  (append clojure-align-binding-forms
                                          clojure-align-cond-forms)
                                  'symbols))
                 end 'noerror))

      (let ((ppss (syntax-ppss)))
        ;; If we're in a string or comment.
        (unless (or (elt ppss 3)
                    (elt ppss 4))
          ;; Only stop looking if we successfully position
          ;; the point.
          (setq found (clojure--position-for-alignment)))))
    found))

(defun clojure--search-whitespace-after-next-sexp (&optional bound _noerror)
  "Move point after all whitespace after the next sexp.

Set the match data group 1 to be this region of whitespace and
return point.

BOUND is bounds the whitespace search."
  (unwind-protect
      (ignore-errors
        (clojure-forward-logical-sexp 1)
        (search-forward-regexp "\\([,\s\t]*\\)" bound)
        (pcase (syntax-after (point))
          ;; End-of-line, try again on next line.
          (`(12) (clojure--search-whitespace-after-next-sexp bound))
          ;; Closing paren, stop here.
          (`(5 . ,_) nil)
          ;; Anything else is something to align.
          (_ (point))))
    (when (and bound (> (point) bound))
      (goto-char bound))))

(defun clojure-align (beg end)
  "Vertically align the contents of the sexp around point.
If region is active, align it.  Otherwise, align everything in the
current \"top-level\" sexp.
When called from lisp code align everything between BEG and END."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (save-excursion
                   (let ((end (progn (end-of-defun)
                                     (point))))
                     (clojure-backward-logical-sexp)
                     (list (point) end)))))
  (setq end (copy-marker end))
  (save-excursion
    (goto-char beg)
    (while (clojure--find-sexp-to-align end)
      (let ((sexp-end (save-excursion
                        (backward-up-list)
                        (forward-sexp 1)
                        (point-marker)))
            (clojure-align-forms-automatically nil)
            (count 1))
        ;; For some bizarre reason, we need to `align-region' once for each
        ;; group.
        (save-excursion
          (while (search-forward-regexp "^ *\n" sexp-end 'noerror)
            (cl-incf count)))
        (dotimes (_ count)
          (align-region (point) sexp-end nil
                        '((clojure-align (regexp . clojure--search-whitespace-after-next-sexp)
                                         (group . 1)
                                         (separate . "^ *$")
                                         (repeat . t)))
                        nil))
        ;; Reindent after aligning because of #360.
        (indent-region (point) sexp-end)))))

;;; Indentation
(defun clojure-indent-region (beg end)
  "Like `indent-region', but also maybe align forms.
Forms between BEG and END are aligned according to
`clojure-align-forms-automatically'."
  (prog1 (let ((indent-region-function nil))
           (indent-region beg end))
    (when clojure-align-forms-automatically
      (condition-case nil
          (clojure-align beg end)
        (scan-error nil)))))

(defun clojure-indent-line ()
  "Indent current line as Clojure code."
  (if (clojure-in-docstring-p)
      (save-excursion
        (beginning-of-line)
        (when (and (looking-at "^\\s-*")
                   (<= (string-width (match-string-no-properties 0))
                       (string-width (clojure-docstring-fill-prefix))))
          (replace-match (clojure-docstring-fill-prefix))))
    (lisp-indent-line)))

(defvar clojure-get-indent-function nil
  "Function to get the indent spec of a symbol.
This function should take one argument, the name of the symbol as
a string.  This name will be exactly as it appears in the buffer,
so it might start with a namespace alias.

This function is analogous to the `clojure-indent-function'
symbol property, and its return value should match one of the
allowed values of this property.  See `clojure-indent-function'
for more information.")

(defun clojure--get-indent-method (function-name)
  "Return the indent spec for the symbol named FUNCTION-NAME.
FUNCTION-NAME is a string.  If it contains a `/', also try only
the part after the `/'.

Look for a spec using `clojure-get-indent-function', then try the
`clojure-indent-function' and `clojure-backtracking-indent'
symbol properties."
  (or (when (functionp clojure-get-indent-function)
        (funcall clojure-get-indent-function function-name))
      (get (intern-soft function-name) 'clojure-indent-function)
      (get (intern-soft function-name) 'clojure-backtracking-indent)
      (when (string-match "/\\([^/]+\\)\\'" function-name)
        (or (get (intern-soft (match-string 1 function-name))
                 'clojure-indent-function)
            (get (intern-soft (match-string 1 function-name))
                 'clojure-backtracking-indent)))
      (when (string-match (rx (or "let" "when" "while") (syntax symbol))
                          function-name)
        (clojure--get-indent-method (substring (match-string 0 function-name) 0 -1)))))

(defvar clojure--current-backtracking-depth 0)

(defun clojure--find-indent-spec-backtracking ()
  "Return the indent sexp that applies to the sexp at point.
Implementation function for `clojure--find-indent-spec'."
  (when (and (>= clojure-max-backtracking clojure--current-backtracking-depth)
             (not (looking-at "^")))
    (let ((clojure--current-backtracking-depth (1+ clojure--current-backtracking-depth))
          (pos 0))
      ;; Count how far we are from the start of the sexp.
      (while (ignore-errors (clojure-backward-logical-sexp 1)
                            (not (or (bobp)
                                     (eq (char-before) ?\n))))
        (cl-incf pos))
      (let* ((function (thing-at-point 'symbol))
             (method (or (when function ;; Is there a spec here?
                           (clojure--get-indent-method function))
                         (ignore-errors
                           ;; Otherwise look higher up.
                           (pcase (syntax-ppss)
                             (`(,(pred (< 0)) ,start . ,_)
                              (goto-char start)
                              (clojure--find-indent-spec-backtracking)))))))
        (when (numberp method)
          (setq method (list method)))
        (pcase method
          ((pred functionp)
           (when (= pos 0)
             method))
          ((pred sequencep)
           (pcase (length method)
             (`0 nil)
             (`1 (let ((head (elt method 0)))
                   (when (or (= pos 0) (sequencep head))
                     head)))
             (l (if (>= pos l)
                    (elt method (1- l))
                  (elt method pos)))))
          ((or `defun `:defn)
           (when (= pos 0)
             :defn))
          (_
           (message "Invalid indent spec for `%s': %s" function method)
           nil))))))

(defun clojure--find-indent-spec ()
  "Return the indent spec that applies to current sexp.
If `clojure-use-backtracking-indent' is non-nil, also do
backtracking up to a higher-level sexp in order to find the
spec."
  (if clojure-use-backtracking-indent
      (save-excursion
        (clojure--find-indent-spec-backtracking))
    (let ((function (thing-at-point 'symbol)))
      (clojure--get-indent-method function))))

(defun clojure--normal-indent (last-sexp indent-mode)
  "Return the normal indentation column for a sexp.
Point should be after the open paren of the _enclosing_ sexp, and
LAST-SEXP is the start of the previous sexp (immediately before
the sexp being indented).  INDENT-MODE is any of the values
accepted by `clojure-indent-style'."
  (goto-char last-sexp)
  (forward-sexp 1)
  (clojure-backward-logical-sexp 1)
  (let ((last-sexp-start nil))
    (if (ignore-errors
          ;; `backward-sexp' until we reach the start of a sexp that is the
          ;; first of its line (the start of the enclosing sexp).
          (while (string-match
                  "[^[:blank:]]"
                  (buffer-substring (line-beginning-position) (point)))
            (setq last-sexp-start (prog1 (point)
                                    (forward-sexp -1))))
          t)
        ;; Here we have found an arg before the arg we're indenting which is at
        ;; the start of a line. Every mode simply aligns on this case.
        (current-column)
      ;; Here we have reached the start of the enclosing sexp (point is now at
      ;; the function name), so the behaviour depends on INDENT-MODE and on
      ;; whether there's also an argument on this line (case A or B).
      (let ((case-a ; The meaning of case-a is explained in `clojure-indent-style'.
             (and last-sexp-start
                  (< last-sexp-start (line-end-position)))))
        (cond
         ;; For compatibility with the old `clojure-defun-style-default-indent', any
         ;; value other than these 3 is equivalent to `always-body'.
         ((not (memq indent-mode '(:always-align :align-arguments nil)))
          (+ (current-column) lisp-body-indent -1))
         ;; There's an arg after the function name, so align with it.
         (case-a (goto-char last-sexp-start)
                 (current-column))
         ;; Not same line.
         ((eq indent-mode :align-arguments)
          (+ (current-column) lisp-body-indent -1))
         ;; Finally, just align with the function name.
         (t (current-column)))))))

(defun clojure--not-function-form-p ()
  "Non-nil if form at point doesn't represent a function call."
  (or (member (char-after) '(?\[ ?\{))
      (save-excursion ;; Catch #?@ (:cljs ...)
        (skip-chars-backward "\r\n[:blank:]")
        (when (eq (char-before) ?@)
          (forward-char -1))
        (and (eq (char-before) ?\?)
             (eq (char-before (1- (point))) ?\#)))
      ;; Car of form is not a symbol.
      (not (looking-at ".\\(?:\\sw\\|\\s_\\)"))))

;; Check the general context, and provide indentation for data structures and
;; special macros. If current form is a function (or non-special macro),
;; delegate indentation to `clojure--normal-indent'.
(defun clojure-indent-function (indent-point state)
  "When indenting a line within a function call, indent properly.

INDENT-POINT is the position where the user typed TAB, or equivalent.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Clojure function with a
non-nil property `clojure-indent-function', that specifies how to do
the indentation.

The property value can be

- `defun', meaning indent `defun'-style;
- an integer N, meaning indent the first N arguments specially
  like ordinary function arguments and then indent any further
  arguments like a body;
- a function to call just as this function was called.
  If that function returns nil, that means it doesn't specify
  the indentation.
- a list, which is used by `clojure-backtracking-indent'.

This function also returns nil meaning don't specify the indentation."
  ;; Goto to the open-paren.
  (goto-char (elt state 1))
  ;; Maps, sets, vectors and reader conditionals.
  (if (clojure--not-function-form-p)
      (1+ (current-column))
    ;; Function or macro call.
    (forward-char 1)
    (let ((method (clojure--find-indent-spec))
          (last-sexp calculate-lisp-indent-last-sexp)
          (containing-form-column (1- (current-column))))
      (pcase method
        ((or (pred integerp) `(,method))
         (let ((pos -1))
           (condition-case nil
               (while (and (<= (point) indent-point)
                           (not (eobp)))
                 (clojure-forward-logical-sexp 1)
                 (cl-incf pos))
             ;; If indent-point is _after_ the last sexp in the
             ;; current sexp, we detect that by catching the
             ;; `scan-error'. In that case, we should return the
             ;; indentation as if there were an extra sexp at point.
             (scan-error (cl-incf pos)))
           (cond
            ;; The first non-special arg. Rigidly reduce indentation.
            ((= pos (1+ method))
             (+ lisp-body-indent containing-form-column))
            ;; Further non-special args, align with the arg above.
            ((> pos (1+ method))
             (clojure--normal-indent last-sexp :always-align))
            ;; Special arg. Rigidly indent with a large indentation.
            (t
             (+ (* 2 lisp-body-indent) containing-form-column)))))
        (`:defn
         (+ lisp-body-indent containing-form-column))
        ((pred functionp)
         (funcall method indent-point state))
        ;; No indent spec, do the default.
        (`nil
         (let ((function (thing-at-point 'symbol)))
           (cond
            ;; Preserve useful alignment of :require (and friends) in `ns' forms.
            ((and function (string-match "^:" function))
             (clojure--normal-indent last-sexp :always-align))
            ;; This is should be identical to the :defn above.
            ((and function
                  (string-match "\\`\\(?:\\S +/\\)?\\(def[a-z]*\\|with-\\)"
                                function)
                  (not (string-match "\\`default" (match-string 1 function))))
             (+ lisp-body-indent containing-form-column))
            ;; Finally, nothing special here, just respect the user's
            ;; preference.
            (t (clojure--normal-indent last-sexp clojure-indent-style)))))))))

;;; Setting indentation
(defun put-clojure-indent (sym indent)
  "Instruct `clojure-indent-function' to indent the body of SYM by INDENT."
  (put sym 'clojure-indent-function indent))

(defmacro define-clojure-indent (&rest kvs)
  "Call `put-clojure-indent' on a series, KVS."
  `(progn
     ,@(mapcar (lambda (x) `(put-clojure-indent
                             (quote ,(car x)) ,(cadr x)))
               kvs)))

(defun add-custom-clojure-indents (name value)
  "Allow `clojure-defun-indents' to indent user-specified macros.

Requires the macro's NAME and a VALUE."
  (custom-set-default name value)
  (mapcar (lambda (x)
            (put-clojure-indent x 'defun))
          value))

(defcustom clojure-defun-indents nil
  "List of additional symbols with defun-style indentation in Clojure.

You can use this to let Emacs indent your own macros the same way
that it indents built-in macros like with-open.  This variable
only works when set via the customize interface (`setq' won't
work).  To set it from Lisp code, use
     (put-clojure-indent \\='some-symbol :defn)."
  :type '(repeat symbol)
  :set 'add-custom-clojure-indents)

(define-clojure-indent
  ;; built-ins
  (ns 1)
  (fn :defn)
  (def :defn)
  (defn :defn)
  (bound-fn :defn)
  (if 1)
  (if-not 1)
  (case 1)
  (cond 0)
  (condp 2)
  (cond-> 1)
  (cond->> 1)
  (when 1)
  (while 1)
  (when-not 1)
  (when-first 1)
  (do 0)
  (future 0)
  (comment 0)
  (doto 1)
  (locking 1)
  (proxy '(2 nil nil (:defn)))
  (as-> 2)

  (reify '(:defn (1)))
  (deftype '(2 nil nil (:defn)))
  (defrecord '(2 nil nil (:defn)))
  (defprotocol '(1 (:defn)))
  (definterface '(1 (:defn)))
  (extend 1)
  (extend-protocol '(1 :defn))
  (extend-type '(1 :defn))
  ;; specify and specify! are from ClojureScript
  (specify '(1 :defn))
  (specify! '(1 :defn))
  (try 0)
  (catch 2)
  (finally 0)

  ;; binding forms
  (let 1)
  (letfn '(1 ((:defn)) nil))
  (binding 1)
  (loop 1)
  (for 1)
  (doseq 1)
  (dotimes 1)
  (when-let 1)
  (if-let 1)
  (when-some 1)
  (if-some 1)
  (this-as 1) ; ClojureScript

  (defmethod :defn)

  ;; clojure.test
  (testing 1)
  (deftest :defn)
  (are 2)
  (use-fixtures :defn)

  ;; core.logic
  (run :defn)
  (run* :defn)
  (fresh :defn)

  ;; core.async
  (alt! 0)
  (alt!! 0)
  (go 0)
  (go-loop 1)
  (thread 0))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Better docstring filling for clojure-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clojure-string-start (&optional regex)
  "Return the position of the \" that begins the string at point.
If REGEX is non-nil, return the position of the # that begins the
regex at point.  If point is not inside a string or regex, return
nil."
  (when (nth 3 (syntax-ppss)) ;; Are we really in a string?
    (save-excursion
      (save-match-data
        ;; Find a quote that appears immediately after whitespace,
        ;; beginning of line, hash, or an open paren, brace, or bracket
        (re-search-backward "\\(\\s-\\|^\\|#\\|(\\|\\[\\|{\\)\\(\"\\)")
        (let ((beg (match-beginning 2)))
          (when beg
            (if regex
                (and (char-before beg) (eq ?# (char-before beg)) (1- beg))
              (when (not (eq ?# (char-before beg)))
                beg))))))))

(defun clojure-char-at-point ()
  "Return the char at point or nil if at buffer end."
  (when (not (= (point) (point-max)))
    (buffer-substring-no-properties (point) (1+ (point)))))

(defun clojure-char-before-point ()
  "Return the char before point or nil if at buffer beginning."
  (when (not (= (point) (point-min)))
    (buffer-substring-no-properties (point) (1- (point)))))

(defun clojure-toggle-keyword-string ()
  "Convert the string or keyword at point to keyword or string."
  (interactive)
  (let ((original-point (point)))
    (while (and (> (point) 1)
                (not (equal "\"" (buffer-substring-no-properties (point) (+ 1 (point)))))
                (not (equal ":" (buffer-substring-no-properties (point) (+ 1 (point))))))
      (backward-char))
    (cond
     ((equal 1 (point))
      (error "Beginning of file reached, this was probably a mistake"))
     ((equal "\"" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert ":" (substring (clojure-delete-and-extract-sexp) 1 -1)))
     ((equal ":" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "\"" (substring (clojure-delete-and-extract-sexp) 1) "\"")))
    (goto-char original-point)))

(defun clojure-delete-and-extract-sexp ()
  "Delete the surrounding sexp and return it."
  (let ((begin (point)))
    (forward-sexp)
    (let ((result (buffer-substring begin (point))))
      (delete-region begin (point))
      result)))



(defun clojure-project-dir (&optional dir-name)
  "Return the absolute path to the project's root directory.

Call is delegated down to `clojure-project-root-function' with
optional DIR-NAME as argument."
  (funcall clojure-project-root-function dir-name))

(defun clojure-project-root-path (&optional dir-name)
  "Return the absolute path to the project's root directory.

Use `default-directory' if DIR-NAME is nil.
Return nil if not inside a project."
  (let* ((dir-name (or dir-name default-directory))
         (choices (delq nil
                        (mapcar (lambda (fname)
                                  (locate-dominating-file dir-name fname))
                                clojure-build-tool-files))))
    (when (> (length choices) 0)
      (car (sort choices #'file-in-directory-p)))))

(defun clojure-project-relative-path (path)
  "Denormalize PATH by making it relative to the project root."
  (file-relative-name path (clojure-project-dir)))


;;; ns manipulation
(defun clojure-expected-ns (&optional path)
  "Return the namespace matching PATH.

PATH is expected to be an absolute file path.

If PATH is nil, use the path to the file backing the current buffer."
  (let* ((path (or path (file-truename (buffer-file-name))))
         (relative (clojure-project-relative-path path))
         (sans-file-type (substring relative 0 (- (length (file-name-extension path t)))))
         (sans-file-sep (mapconcat 'identity (cdr (split-string sans-file-type "/")) "."))
         (sans-underscores (replace-regexp-in-string "_" "-" sans-file-sep)))
    ;; Drop prefix from ns for projects with structure src/{clj,cljs,cljc}
    (replace-regexp-in-string "\\`clj[scx]?\\." "" sans-underscores)))

(defun clojure-insert-ns-form-at-point ()
  "Insert a namespace form at point."
  (interactive)
  (insert (format "(ns %s)" (funcall clojure-expected-ns-function))))

(defun clojure-insert-ns-form ()
  "Insert a namespace form at the beginning of the buffer."
  (interactive)
  (widen)
  (goto-char (point-min))
  (clojure-insert-ns-form-at-point))

(defun clojure-update-ns ()
  "Update the namespace of the current buffer.
Useful if a file has been renamed."
  (interactive)
  (let ((nsname (funcall clojure-expected-ns-function)))
    (when nsname
      (save-excursion
        (save-match-data
          (if (clojure-find-ns)
              (progn (replace-match nsname nil nil nil 4)
                     (message "ns form updated"))
            (error "Namespace not found")))))))

(defun clojure--sort-following-sexps ()
  "Sort sexps between point and end of current sexp.
Comments at the start of a line are considered part of the
following sexp.  Comments at the end of a line (after some other
content) are considered part of the preceding sexp."
  ;; Here we're after the :require/:import symbol.
  (save-restriction
    (narrow-to-region (point) (save-excursion
                                (up-list)
                                (1- (point))))
    (skip-chars-forward "\r\n[:blank:]")
    (sort-subr nil
               (lambda () (skip-chars-forward "\r\n[:blank:]"))
               ;; Move to end of current top-level thing.
               (lambda ()
                 (condition-case nil
                     (while t (up-list))
                   (scan-error nil))
                 ;; We could be inside a symbol instead of a sexp.
                 (unless (looking-at "\\s-\\|$")
                   (clojure-forward-logical-sexp))
                 ;; move past comments at the end of the line.
                 (search-forward-regexp "$"))
               ;; Move to start of ns name.
               (lambda ()
                 (comment-forward)
                 (skip-chars-forward "[:blank:]\n\r[(")
                 (clojure-forward-logical-sexp)
                 (forward-sexp -1)
                 nil)
               ;; Move to end of ns name.
               (lambda ()
                 (clojure-forward-logical-sexp)))
    (goto-char (point-max))
    ;; Does the last line now end in a comment?
    (when (nth 4 (parse-partial-sexp (point-min) (point)))
      (insert "\n"))))

(defun clojure-sort-ns ()
  "Internally sort each sexp inside the ns form."
  (interactive)
  (comment-normalize-vars)
  (if (clojure-find-ns)
      (save-excursion
        (goto-char (match-beginning 0))
        (redisplay)
        (let ((beg (point))
              (ns))
          (forward-sexp 1)
          (setq ns (buffer-substring beg (point)))
          (forward-char -1)
          (while (progn (forward-sexp -1)
                        (looking-at "(:[a-z]"))
            (save-excursion
              (forward-char 1)
              (forward-sexp 1)
              (clojure--sort-following-sexps)))
          (goto-char beg)
          (if (looking-at (regexp-quote ns))
              (message "ns form is already sorted")
            (sleep-for 0.1)
            (redisplay)
            (message "ns form has been sorted")
            (sleep-for 0.1))))
    (user-error "Namespace not found")))

(defconst clojure-namespace-name-regex
  (rx line-start
      "("
      (zero-or-one (group (regexp "clojure.core/")))
      (zero-or-one (submatch "in-"))
      "ns"
      (zero-or-one "+")
      (one-or-more (any whitespace "\n"))
      (zero-or-more (or (submatch (zero-or-one "#")
                                  "^{"
                                  (zero-or-more (not (any "}")))
                                  "}")
                        (zero-or-more "^:"
                                      (one-or-more (not (any whitespace)))))
                    (one-or-more (any whitespace "\n")))
      (zero-or-one (any ":'")) ;; (in-ns 'foo) or (ns+ :user)
      (group (one-or-more (not (any "()\"" whitespace))) symbol-end)))

(defun clojure-find-ns ()
  "Return the namespace of the current Clojure buffer.
Return the namespace closest to point and above it.  If there are
no namespaces above point, return the first one in the buffer."
  (save-excursion
    (save-restriction
      (widen)
      ;; The closest ns form above point.
      (when (or (re-search-backward clojure-namespace-name-regex nil t)
                ;; Or any form at all.
                (and (goto-char (point-min))
                     (re-search-forward clojure-namespace-name-regex nil t)))
        (match-string-no-properties 4)))))

(defconst clojure-def-type-and-name-regex
  (concat "(\\(?:\\(?:\\sw\\|\\s_\\)+/\\)?"
          ;; Declaration
          "\\(def\\(?:\\sw\\|\\s_\\)*\\)\\>"
          ;; Any whitespace
          "[ \r\n\t]*"
          ;; Possibly type or metadata
          "\\(?:#?^\\(?:{[^}]*}\\|\\(?:\\sw\\|\\s_\\)+\\)[ \r\n\t]*\\)*"
          ;; Symbol name
          "\\(\\(?:\\sw\\|\\s_\\)+\\)"))

(defun clojure-find-def ()
  "Find the var declaration macro and symbol name of the current form.
Returns a list pair, e.g. (\"defn\" \"abc\") or (\"deftest\" \"some-test\")."
  (save-excursion
    (unless (looking-at clojure-def-type-and-name-regex)
      (beginning-of-defun))
    (when (search-forward-regexp clojure-def-type-and-name-regex nil t)
      (list (match-string-no-properties 1)
            (match-string-no-properties 2)))))


;;; Sexp navigation
(defun clojure--looking-at-non-logical-sexp ()
  "Return non-nil if text after point is \"non-logical\" sexp.
\"Non-logical\" sexp are ^metadata and #reader.macros."
  (comment-normalize-vars)
  (comment-forward (point-max))
  (looking-at-p "\\^\\|#[[:alpha:]]"))

(defun clojure-forward-logical-sexp (&optional n)
  "Move forward N logical sexps.
This will skip over sexps that don't represent objects, so that ^hints and
#reader.macros are considered part of the following sexp."
  (interactive "p")
  (unless n (setq n 1))
  (if (< n 0)
      (clojure-backward-logical-sexp (- n))
    (let ((forward-sexp-function nil))
      (while (> n 0)
        (while (clojure--looking-at-non-logical-sexp)
          (forward-sexp 1))
        ;; The actual sexp
        (forward-sexp 1)
        (skip-chars-forward ",")
        (setq n (1- n))))))

(defun clojure-backward-logical-sexp (&optional n)
  "Move backward N logical sexps.
This will skip over sexps that don't represent objects, so that ^hints and
#reader.macros are considered part of the following sexp."
  (interactive "p")
  (unless n (setq n 1))
  (if (< n 0)
      (clojure-forward-logical-sexp (- n))
    (let ((forward-sexp-function nil))
      (while (> n 0)
        ;; The actual sexp
        (backward-sexp 1)
        ;; Non-logical sexps.
        (while (and (not (bobp))
                    (ignore-errors
                      (save-excursion
                        (backward-sexp 1)
                        (clojure--looking-at-non-logical-sexp))))
          (backward-sexp 1))
        (setq n (1- n))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Refactoring support
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Threading macros related
(defcustom clojure-thread-all-but-last nil
  "Non-nil means do not thread the last expression.
This means that `clojure-thread-first-all' and
`clojure-thread-last-all' not thread the deepest sexp inside the
current sexp."
  :package-version '(clojure-mode . "5.4.0")
  :safe #'booleanp
  :type 'boolean)

(defun clojure--point-after (&rest actions)
  "Return POINT after performing ACTIONS.

An action is either the symbol of a function or a two element
list of (fn args) to pass to `apply''"
  (save-excursion
    (dolist (fn-and-args actions)
      (let ((f (if (listp fn-and-args) (car fn-and-args) fn-and-args))
            (args (if (listp fn-and-args) (cdr fn-and-args) nil)))
        (apply f args)))
    (point)))

(defun clojure--maybe-unjoin-line ()
  "Undo a `join-line' done by a threading command."
  (when (get-text-property (point) 'clojure-thread-line-joined)
    (remove-text-properties (point) (1+ (point)) '(clojure-thread-line-joined t))
    (insert "\n")))

(defun clojure--unwind-last ()
  "Unwind a thread last macro once.

Point must be between the opening paren and the ->> symbol."
  (forward-sexp)
  (save-excursion
    (let ((beg (point))
          (contents (clojure-delete-and-extract-sexp)))
      (when (looking-at " *\n")
        (join-line 'following))
      (clojure--ensure-parens-around-function-names)
      (let* ((sexp-beg-line (line-number-at-pos))
             (sexp-end-line (progn (forward-sexp)
                                   (line-number-at-pos)))
             (multiline-sexp-p (not (= sexp-beg-line sexp-end-line))))
        (down-list -1)
        (if multiline-sexp-p
            (insert "\n")
          ;; `clojure--maybe-unjoin-line' only works when unwinding sexps that were
          ;; threaded in the same Emacs session, but it also catches cases that
          ;; `multiline-sexp-p' doesn't.
          (clojure--maybe-unjoin-line))
        (insert contents))))
  (forward-char))

(defun clojure--ensure-parens-around-function-names ()
  "Insert parens around function names if necessary."
  (clojure--looking-at-non-logical-sexp)
  (unless (looking-at "(")
    (insert-parentheses 1)
    (backward-up-list)))

(defun clojure--unwind-first ()
  "Unwind a thread first macro once.

Point must be between the opening paren and the -> symbol."
  (forward-sexp)
  (save-excursion
    (let ((contents (clojure-delete-and-extract-sexp)))
      (when (looking-at " *\n")
        (join-line 'following))
      (clojure--ensure-parens-around-function-names)
      (down-list)
      (forward-sexp)
      (insert contents)
      (forward-sexp -1)
      (clojure--maybe-unjoin-line)))
  (forward-char))

(defun clojure--pop-out-of-threading ()
  "Raise a sexp up a level to unwind a threading form."
  (save-excursion
    (down-list 2)
    (backward-up-list)
    (raise-sexp)))

(defun clojure--nothing-more-to-unwind ()
  "Return non-nil if a threaded form cannot be unwound further."
  (save-excursion
    (let ((beg (point)))
      (forward-sexp)
      (down-list -1)
      (backward-sexp 2) ;; the last sexp, the threading macro
      (when (looking-back "(\\s-*" (line-beginning-position))
        (backward-up-list)) ;; and the paren
      (= beg (point)))))

(defun clojure--fix-sexp-whitespace (&optional move-out)
  "Fix whitespace after unwinding a threading form.

Optional argument MOVE-OUT, if non-nil, means moves up a list
before fixing whitespace."
  (save-excursion
    (when move-out (backward-up-list))
    (let ((sexp (bounds-of-thing-at-point 'sexp)))
      (clojure-indent-region (car sexp) (cdr sexp))
      (delete-trailing-whitespace (car sexp) (cdr sexp)))))

;;;###autoload
(defun clojure-unwind ()
  "Unwind thread at point or above point by one level.
Return nil if there are no more levels to unwind."
  (interactive)
  (save-excursion
    (let ((limit (save-excursion
                   (beginning-of-defun)
                   (point))))
      (ignore-errors
        (when (looking-at "(")
          (forward-char 1)
          (forward-sexp 1)))
      (search-backward-regexp "([^-]*->" limit)
      (if (clojure--nothing-more-to-unwind)
          (progn (clojure--pop-out-of-threading)
                 (clojure--fix-sexp-whitespace)
                 nil)
        (down-list)
        (prog1 (cond
                ((looking-at "[^-]*->\\_>")  (clojure--unwind-first))
                ((looking-at "[^-]*->>\\_>") (clojure--unwind-last)))
          (clojure--fix-sexp-whitespace 'move-out))
        t))))

;;;###autoload
(defun clojure-unwind-all ()
  "Fully unwind thread at point or above point."
  (interactive)
  (while (clojure-unwind)))

(defun clojure--remove-superfluous-parens ()
  "Remove extra parens from a form."
  (when (looking-at "([^ )]+)")
    (delete-pair)))

(defun clojure--thread-first ()
  "Thread a nested sexp using ->."
  (down-list)
  (forward-symbol 1)
  (unless (looking-at ")")
    (let ((contents (clojure-delete-and-extract-sexp)))
      (backward-up-list)
      (just-one-space 0)
      (save-excursion
        (insert contents "\n")
        (clojure--remove-superfluous-parens))
      (when (looking-at "\\s-*\n")
        (join-line 'following)
        (forward-char 1)
        (put-text-property (point) (1+ (point))
                           'clojure-thread-line-joined t))
      t)))

(defun clojure--thread-last ()
  "Thread a nested sexp using ->>."
  (forward-sexp 2)
  (down-list -1)
  (backward-sexp)
  (unless (eq (char-before) ?\()
    (let ((contents (clojure-delete-and-extract-sexp)))
      (just-one-space 0)
      (backward-up-list)
      (insert contents "\n")
      (clojure--remove-superfluous-parens)
      ;; cljr #255 Fix dangling parens
      (forward-sexp)
      (when (looking-back "^\\s-*\\()+\\)\\s-*" (line-beginning-position))
        (let ((pos (match-beginning 1)))
          (put-text-property pos (1+ pos) 'clojure-thread-line-joined t))
        (join-line))
      t)))

(defun clojure--threadable-p ()
  "Return non-nil if a form can be threaded."
  (save-excursion
    (forward-symbol 1)
    (looking-at "[\n\r\t ]*(")))

;;;###autoload
(defun clojure-thread ()
  "Thread by one more level an existing threading macro."
  (interactive)
  (ignore-errors
    (when (looking-at "(")
      (forward-char 1)
      (forward-sexp 1)))
  (search-backward-regexp "([^-]*->")
  (down-list)
  (when (clojure--threadable-p)
    (prog1 (cond
            ((looking-at "[^-]*->\\_>")  (clojure--thread-first))
            ((looking-at "[^-]*->>\\_>") (clojure--thread-last)))
      (clojure--fix-sexp-whitespace 'move-out))))

(defun clojure--thread-all (first-or-last-thread but-last)
  "Fully thread the form at point.

FIRST-OR-LAST-THREAD is \"->\" or \"->>\".

When BUT-LAST is non-nil, the last expression is not threaded.
Default value is `clojure-thread-all-but-last'."
  (save-excursion
    (insert-parentheses 1)
    (insert first-or-last-thread))
  (while (save-excursion (clojure-thread)))
  (when (or but-last clojure-thread-all-but-last)
    (clojure-unwind)))

;;;###autoload
(defun clojure-thread-first-all (but-last)
  "Fully thread the form at point using ->.

When BUT-LAST is non-nil, the last expression is not threaded.
Default value is `clojure-thread-all-but-last'."
  (interactive "P")
  (clojure--thread-all "-> " but-last))

;;;###autoload
(defun clojure-thread-last-all (but-last)
  "Fully thread the form at point using ->>.

When BUT-LAST is non-nil, the last expression is not threaded.
Default value is `clojure-thread-all-but-last'."
  (interactive "P")
  (clojure--thread-all "->> " but-last))

;;; Cycling stuff

(defcustom clojure-use-metadata-for-privacy nil
  "If nil, `clojure-cycle-privacy' will use (defn- f []).
If t, it will use (defn ^:private f [])."
  :package-version '(clojure-mode . "5.5.0")
  :safe #'booleanp
  :type 'boolean)

;;;###autoload
(defun clojure-cycle-privacy ()
  "Make public the current private def, or vice-versa.
See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-cycle-privacy"
  (interactive)
  (save-excursion
    (ignore-errors (forward-char 7))
    (search-backward-regexp "(defn?\\(-\\| ^:private\\)?\\_>")
    (if (match-string 1)
        (replace-match "" nil nil nil 1)
      (goto-char (match-end 0))
      (insert (if (or clojure-use-metadata-for-privacy
                      (equal (match-string 0) "(def"))
                  " ^:private"
                "-")))))

(defun clojure--convert-collection (coll-open coll-close)
  "Convert the collection at (point) by unwrapping it an wrapping it between COLL-OPEN and COLL-CLOSE."
  (save-excursion
    (while (and
            (not (bobp))
            (not (looking-at "(\\|{\\|\\[")))
      (backward-char))
    (when (or (eq ?\# (char-before))
              (eq ?\' (char-before)))
      (delete-char -1))
    (when (and (bobp)
               (not (memq (char-after) '(?\{ ?\( ?\[))))
      (user-error "Beginning of file reached, collection is not found"))
    (insert coll-open (substring (clojure-delete-and-extract-sexp) 1 -1) coll-close)))

;;;###autoload
(defun clojure-convert-collection-to-list ()
  "Convert collection at (point) to list."
  (interactive)
  (clojure--convert-collection "(" ")"))

;;;###autoload
(defun clojure-convert-collection-to-quoted-list ()
  "Convert collection at (point) to quoted list."
  (interactive)
  (clojure--convert-collection "'(" ")"))

;;;###autoload
(defun clojure-convert-collection-to-map ()
  "Convert collection at (point) to map."
  (interactive)
  (clojure--convert-collection "{" "}"))

;;;###autoload
(defun clojure-convert-collection-to-vector ()
  "Convert collection at (point) to vector."
  (interactive)
  (clojure--convert-collection "[" "]"))

;;;###autoload
(defun clojure-convert-collection-to-set ()
  "Convert collection at (point) to set."
  (interactive)
  (clojure--convert-collection "#{" "}"))

(defun clojure--goto-if ()
  "Find the first surrounding if or if-not expression."
  (when (in-string-p)
    (while (or (not (looking-at "("))
               (in-string-p))
      (backward-char)))
  (while (not (looking-at "\\((if \\)\\|\\((if-not \\)"))
    (condition-case nil
        (backward-up-list)
      (scan-error (user-error "No if or if-not found")))))

;;;###autoload
(defun clojure-cycle-if ()
  "Change a surrounding if to if-not, or vice-versa.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-cycle-if"
  (interactive)
  (save-excursion
    (clojure--goto-if)
    (cond
     ((looking-at "(if-not")
      (forward-char 3)
      (delete-char 4)
      (forward-sexp 2)
      (transpose-sexps 1))
     ((looking-at "(if")
      (forward-char 3)
      (insert "-not")
      (forward-sexp 2)
      (transpose-sexps 1)))))

;; TODO: Remove code duplication with `clojure--goto-if'.
(defun clojure--goto-when ()
  "Find the first surrounding when or when-not expression."
  (when (in-string-p)
    (while (or (not (looking-at "("))
               (in-string-p))
      (backward-char)))
  (while (not (looking-at "\\((when \\)\\|\\((when-not \\)"))
    (condition-case nil
        (backward-up-list)
      (scan-error (user-error "No when or when-not found")))))

;;;###autoload
(defun clojure-cycle-when ()
  "Change a surrounding when to when-not, or vice-versa."
  (interactive)
  (save-excursion
    (clojure--goto-when)
    (cond
     ((looking-at "(when-not")
      (forward-char 9)
      (delete-char -4))
     ((looking-at "(when")
      (forward-char 5)
      (insert "-not")))))

(defun clojure-cycle-not ()
  "Add or remove a not form around the current form."
  (interactive)
  (save-excursion
    (condition-case nil
        (backward-up-list)
      (scan-error (user-error "`clojure-cycle-not' must be invoked inside a list")))
    (if (looking-back "(not ")
        (progn
          (delete-char -5)
          (forward-sexp)
          (delete-char 1))
      (insert "(not ")
      (forward-sexp)
      (insert ")"))))

;;; let related stuff

(defvar clojure--let-regexp
  "\(\\(when-let\\|if-let\\|let\\)\\(\\s-*\\|\\[\\)"
  "Regexp matching let like expressions, i.e. \"let\", \"when-let\", \"if-let\".

The first match-group is the let expression.

The second match-group is the whitespace or the opening square
bracket if no whitespace between the let expression and the
bracket.")

(defun clojure--goto-let ()
  "Go to the beginning of the nearest let form."
  (when (in-string-p)
    (while (or (not (looking-at "("))
               (in-string-p))
      (backward-char)))
  (ignore-errors
    (while (not (looking-at clojure--let-regexp))
      (backward-up-list)))
  (looking-at clojure--let-regexp))

(defun clojure--inside-let-binding-p ()
  "Return non-nil if point is inside a let binding."
  (ignore-errors
    (save-excursion
      (let ((pos (point)))
        (clojure--goto-let)
        (re-search-forward "\\[")
        (if (< pos (point))
            nil
          (forward-sexp)
          (up-list)
          (< pos (point)))))))

(defun clojure--beginning-of-current-let-binding ()
  "Move before the bound name of the current binding.
Assume that point is in the binding form of a let."
  (let ((current-point (point)))
    (clojure--goto-let)
    (search-forward "[")
    (forward-char)
    (while (> current-point (point))
      (forward-sexp))
    (backward-sexp 2)))

(defun clojure--previous-line ()
  "Keep the column position while go the previous line."
  (let ((col (current-column)))
    (forward-line -1)
    (move-to-column col)))

(defun clojure--prepare-to-insert-new-let-binding ()
  "Move to right place in the let form to insert a new binding and indent."
  (if (clojure--inside-let-binding-p)
      (progn
        (clojure--beginning-of-current-let-binding)
        (newline-and-indent)
        (clojure--previous-line)
        (indent-for-tab-command))
    (clojure--goto-let)
    (search-forward "[")
    (backward-up-list)
    (forward-sexp)
    (down-list -1)
    (backward-char)
    (if (looking-at "\\[\\s-*\\]")
        (forward-char)
      (forward-char)
      (newline-and-indent))))

(defun clojure--sexp-regexp (sexp)
  "Return a regexp for matching SEXP."
  (concat "\\([^[:word:]^-]\\)"
          (mapconcat #'identity (mapcar 'regexp-quote (split-string sexp))
                     "[[:space:]\n\r]+")
          "\\([^[:word:]^-]\\)"))

(defun clojure--replace-sexp-with-binding (bound-name init-expr)
  "Replace a binding with its bound name in the let form.

BOUND-NAME is the name (left-hand side) of a binding.

INIT-EXPR is the value (right-hand side) of a binding."
  (save-excursion
    (while (re-search-forward
            (clojure--sexp-regexp init-expr)
            (clojure--point-after 'clojure--goto-let 'forward-sexp)
            t)
      (replace-match (concat "\\1" bound-name "\\2")))))

(defun clojure--replace-sexps-with-bindings (bindings)
  "Replace bindings with their respective bound names in the let form.

BINDINGS is the list of bound names and init expressions."
  (let ((bound-name (pop bindings))
        (init-expr (pop bindings)))
    (when bound-name
      (clojure--replace-sexp-with-binding bound-name init-expr)
      (clojure--replace-sexps-with-bindings bindings))))

(defun clojure--replace-sexps-with-bindings-and-indent ()
  "Replace sexps with bindings."
  (clojure--replace-sexps-with-bindings
   (clojure--read-let-bindings))
  (clojure-indent-region
   (clojure--point-after 'clojure--goto-let)
   (clojure--point-after 'clojure--goto-let 'forward-sexp)))

(defun clojure--read-let-bindings ()
  "Read the bound-name and init expression pairs in the binding form.
Return a list: odd elements are bound names, even elements init expressions."
  (clojure--goto-let)
  (down-list 2)
  (let* ((start (point))
         (sexp-start start)
         (end (save-excursion
                (backward-char)
                (forward-sexp)
                (down-list -1)
                (point)))
         bindings)
    (while (/= sexp-start end)
      (forward-sexp)
      (push
       (string-trim (buffer-substring-no-properties sexp-start (point)))
       bindings)
      (skip-chars-forward "\r\n\t[:blank:]")
      (setq sexp-start (point)))
    (nreverse bindings)))

(defun clojure--introduce-let-internal (name &optional n)
  "Create a let form, binding the form at point with NAME.

Optional numeric argument N, if non-nil, introduces the let N
lists up."
  (if (numberp n)
      (let ((init-expr-sexp (clojure-delete-and-extract-sexp)))
        (insert name)
        (ignore-errors (backward-up-list n))
        (insert "(let" (clojure-delete-and-extract-sexp) ")")
        (backward-sexp)
        (down-list)
        (forward-sexp)
        (insert " [" name " " init-expr-sexp "]\n")
        (clojure--replace-sexps-with-bindings-and-indent))
    (insert "[ " (clojure-delete-and-extract-sexp) "]")
    (backward-sexp)
    (insert "(let " (clojure-delete-and-extract-sexp) ")")
    (backward-sexp)
    (down-list 2)
    (insert name)
    (forward-sexp)
    (up-list)
    (newline-and-indent)
    (insert name)))

(defun clojure--move-to-let-internal (name)
  "Bind the form at point to NAME in the nearest let."
  (if (not (save-excursion (clojure--goto-let)))
      (clojure--introduce-let-internal name)
    (let ((contents (clojure-delete-and-extract-sexp)))
      (insert name)
      (clojure--prepare-to-insert-new-let-binding)
      (insert contents)
      (backward-sexp)
      (insert " ")
      (backward-char)
      (insert name)
      (clojure--replace-sexps-with-bindings-and-indent))))

(defun clojure--let-backward-slurp-sexp-internal ()
  "Slurp the s-expression before the let form into the let form."
  (clojure--goto-let)
  (backward-sexp)
  (let ((sexp (string-trim (clojure-delete-and-extract-sexp))))
    (delete-blank-lines)
    (down-list)
    (forward-sexp 2)
    (newline-and-indent)
    (insert sexp)
    (clojure--replace-sexps-with-bindings-and-indent)))

;;;###autoload
(defun clojure-let-backward-slurp-sexp (&optional n)
  "Slurp the s-expression before the let form into the let form.
With a numberic prefix argument slurp the previous N s-expression into the let form."
  (interactive "p")
  (unless n (setq n 1))
  (dotimes (k n)
    (save-excursion (clojure--let-backward-slurp-sexp-internal))))

(defun clojure--let-forward-slurp-sexp-internal ()
  "Slurp the next s-expression after the let form into the let form."
  (clojure--goto-let)
  (forward-sexp)
  (let ((sexp (string-trim (clojure-delete-and-extract-sexp))))
    (down-list -1)
    (newline-and-indent)
    (insert sexp)
    (clojure--replace-sexps-with-bindings-and-indent)))

;;;###autoload
(defun clojure-let-forward-slurp-sexp (&optional n)
  "Slurp the next s-expression after the let form into the let form.
With a numeric prefix argument slurp the next N s-expressions into the let form."
  (interactive "p")
  (unless n (setq n 1))
  (dotimes (k n)
    (save-excursion (clojure--let-forward-slurp-sexp-internal))))

;;;###autoload
(defun clojure-introduce-let (&optional n)
  "Create a let form, binding the form at point.
With a numeric prefix argument the let is introduced N lists up."
  (interactive "P")
  (clojure--introduce-let-internal (read-from-minibuffer "Name of bound symbol: ") n))

;;;###autoload
(defun clojure-move-to-let ()
  "Move the form at point to a binding in the nearest let."
  (interactive)
  (clojure--move-to-let-internal (read-from-minibuffer "Name of bound symbol: ")))


;;; ClojureScript
(defconst clojurescript-font-lock-keywords
  (eval-when-compile
    `(;; ClojureScript built-ins
      (,(concat "(\\(?:\.*/\\)?"
                (regexp-opt '("js-obj" "js-delete" "clj->js" "js->clj"))
                "\\>")
       0 font-lock-builtin-face)))
  "Additional font-locking for `clojurescript-mode'.")

;;;###autoload
(define-derived-mode clojurescript-mode clojure-mode "ClojureScript"
  "Major mode for editing ClojureScript code.

\\{clojurescript-mode-map}"
  (font-lock-add-keywords nil clojurescript-font-lock-keywords))

;;;###autoload
(define-derived-mode clojurec-mode clojure-mode "ClojureC"
  "Major mode for editing ClojureC code.

\\{clojurec-mode-map}")

(defconst clojurex-font-lock-keywords
  ;; cljx annotations (#+clj and #+cljs)
  '(("#\\+cljs?\\>" 0 font-lock-preprocessor-face))
  "Additional font-locking for `clojurex-mode'.")

;;;###autoload
(define-derived-mode clojurex-mode clojure-mode "ClojureX"
  "Major mode for editing ClojureX code.

\\{clojurex-mode-map}"
  (font-lock-add-keywords nil clojurex-font-lock-keywords))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist
               '("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojurec-mode))
  (add-to-list 'auto-mode-alist '("\\.cljx\\'" . clojurex-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))
  ;; boot build scripts are Clojure source files
  (add-to-list 'auto-mode-alist '("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode)))

(provide 'clojure-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; clojure-mode.el ends here
