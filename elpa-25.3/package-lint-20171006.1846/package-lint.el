;;; package-lint.el --- A linting library for elisp package authors -*- lexical-binding: t -*-

;; Copyright (C) 2014-2017  Steve Purcell, Fanael Linithien

;; Author: Steve Purcell <steve@sanityinc.com>
;;         Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/purcell/package-lint
;; Package-Version: 20171006.1846
;; Keywords: lisp
;; Version: 0
;; Package-Requires: ((cl-lib "0.5") (emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a list of issues with the package metadata of a file,
;; e.g. the package dependencies it requires.

;; See function `package-lint-buffer'.

;; Checks will currently be enabled only if a "Package-Requires:" or
;; "Package-Version:" header is present in the file.

;;; Code:

(eval-when-compile (require 'pcase))    ; `pcase-dolist' is not autoloaded
(require 'cl-lib)
(require 'package)
(require 'lisp-mnt)
(require 'finder)
(require 'imenu)


;;; Compatibility

(defalias 'package-lint--package-desc-summary
  (if (fboundp 'package-desc-summary)
      'package-desc-summary
    'package-desc-doc))

(defalias 'package-lint--package-desc-name
  (if (fboundp 'package-desc-name)
      'package-desc-name
    (lambda (desc) (intern (elt desc 0)))))


;;; Machinery

(defvar package-lint--errors nil
  "List of errors and warnings for the current buffer.
This is bound dynamically while the checks run.")

(defmacro package-lint--match-symbols (&rest symbols)
  "Return a regexp matching the string names of all given SYMBOLS."
  (regexp-opt (mapcar #'symbol-name symbols)))

(defconst package-lint--libraries-added-alist
  (list
   (cons '(24 4)
         (package-lint--match-symbols
          nadvice
          subr-x))
   (cons '(25 1)
         (package-lint--match-symbols
          cl-generic
          js-jsx-mode
          map
          pinentry
          thunk)))
  "An alist of library names and when they were added to Emacs.")

(defconst package-lint--functions-and-macros-added-alist
  (list
   (cons '(24)
         (package-lint--match-symbols
          bidi-string-mark-left-to-right
          condition-case-unless-debug
          current-bidi-paragraph-direction
          file-selinux-context
          letrec
          make-composed-keymap
          pcase
          pcase-dolist
          pcase-let
          pcase-let*
          prog-mode
          read-char-choice
          run-hook-wrapped
          server-eval-at
          set-file-selinux-context
          special-variable-p
          string-prefix-p
          url-queue-retrieve
          window-body-height
          window-stage-get
          window-stage-put
          window-total-width
          window-valid-p
          with-wrapper-hook))
   (cons '(24 3)
         (package-lint--match-symbols
          autoload-do-load
          autoloadp
          buffer-narrowed-p
          defvar-local
          file-name-base
          function-get
          posnp
          read-only-mode
          setq-local
          system-groups
          system-users
          tty-top-frame
          url-encode-url
          user-error
          with-temp-buffer-window))
   (cons '(24 4)
         (package-lint--match-symbols
          add-face-text-property
          add-function
          advice-add
          advice-remove
          cl-tagbody
          completion-table-merge
          completion-table-with-cache
          define-alternative
          define-error
          display-monitor-attributes-list
          file-acl
          file-extended-attributes
          fill-single-char-nobreak-p
          frame-monitor-attributes
          get-pos-property
          group-gid
          group-real-gid
          hash-table-keys
          hash-table-values
          macrop
          remove-function
          set-file-acl
          special-form-p
          string-blank-p
          string-empty-p
          string-join
          string-remove-prefix
          string-remove-suffix
          string-reverse
          string-suffix-p
          string-trim
          string-trim-left
          string-trim-right
          window-bottom-divider-width
          window-header-line-height
          window-mode-line-height
          window-right-divider-width
          window-scroll-bar-width
          window-text-pixel-size
          with-eval-after-load
          zlib-decompress-region))
   (cons '(25)
         (package-lint--match-symbols
          alist-get
          backward-word-strictly
          bidi-find-overridden-directionality
          buffer-substring-with-bidi-context
          bufferpos-to-filepos
          char-fold-to-regexp
          checkdoc-file
          cl-digit-char-p
          cl-fresh-line
          cl-parse-integer
          default-font-width
          define-advice
          define-inline
          directory-files-recursively
          directory-name-p
          file-notify-valid-p
          filepos-to-bufferpos
          font-lock-ensure
          font-lock-flush
          format-message
          forward-word-strictly
          frame-edges
          frame-geometry
          frame-scroll-bar-height
          funcall-interactively
          function-put
          horizontal-scroll-bars-available-p
          if-let
          macroexpand-1
          make-process
          mouse-absolute-pixel-position
          pcase-defmacro
          pcase-exhaustive
          pcase-lambda
          set-binary-mode
          set-mouse-absolute-pixel-position
          string-collate-equalp
          string-collate-lessp
          string-greaterp
          thread-first
          thread-last
          toggle-horizontal-scroll-bar
          when-let
          window-absolute-pixel-position
          window-font-height
          window-font-width
          window-max-chars-per-line
          window-preserve-size
          window-scroll-bar-height
          with-displayed-buffer-window
          with-file-modes)))
  "An alist of function/macro names and when they were added to Emacs.")

(defconst package-lint--sane-prefixes
  (rx
   string-start
   (or
    "org-dblock-write:"
    "org-babel-execute:"
    "org-babel-prep-session:"
    "org-babel-variable-assignments:"
    "org-babel-default-header-args:"))
  "A regexp matching whitelisted non-standard symbol prefixes.")

(defun package-lint--check-all ()
  "Return a list of errors/warnings for the current buffer."
  (let ((package-lint--errors '())
        (case-fold-search nil))
    (save-match-data
      (save-excursion
        (save-restriction
          (widen)
          (package-lint--check-reserved-keybindings)
          (package-lint--check-keywords-list)
          (package-lint--check-url-header)
          (package-lint--check-package-version-present)
          (package-lint--check-lexical-binding-is-on-first-line)
          (package-lint--check-objects-by-regexp
           "(define-minor-mode\\s-"
           #'package-lint--check-minor-mode)
          (package-lint--check-objects-by-regexp
           "(define-global\\(?:ized\\)?-minor-mode\\s-"
           #'package-lint--check-globalized-minor-mode)
          (package-lint--check-objects-by-regexp
           "(defgroup\\s-" #'package-lint--check-defgroup)
          (let ((desc (package-lint--check-package-el-can-parse)))
            (when desc
              (package-lint--check-package-summary desc)
              (package-lint--check-provide-form desc)))
          (let ((deps (package-lint--check-dependency-list)))
            (package-lint--check-lexical-binding-requires-emacs-24 deps)
            (package-lint--check-libraries-available-in-emacs deps)
            (package-lint--check-macros-functions-available-in-emacs deps))
          (package-lint--check-for-literal-emacs-path)
          (package-lint--check-commentary-existence)
          (let ((definitions (package-lint--get-defs)))
            (package-lint--check-autoloads-on-private-functions definitions)
            (package-lint--check-defs-prefix definitions)
            (package-lint--check-symbol-separators definitions)))))
    (sort package-lint--errors
          (lambda (a b)
            (pcase-let ((`(,a-line ,a-column ,_ ,a-message) a)
                        (`(,b-line ,b-column ,_ ,b-message) b))
              (cond
               ((/= a-line b-line) (< a-line b-line))
               ((/= a-column b-column) (< a-column b-column))
               (t
                (string-lessp a-message b-message))))))))

(defun package-lint--error (line col type message)
  "Construct a datum for error at LINE and COL with TYPE and MESSAGE."
  (push (list line col type message) package-lint--errors))

(defun package-lint--error-at-point (type message)
  "Construct a datum for error at the point with TYPE and MESSAGE."
  (package-lint--error (line-number-at-pos) (current-column) type message))


;;; Checks

(defun package-lint--check-reserved-keybindings ()
  "Warn about reserved keybindings."
  (let ((re (rx "(" (*? space) (or "kbd" "global-set-key" "local-set-key" "define-key") symbol-end)))
    (goto-char (point-min))
    (while (re-search-forward re nil t)
      (unless (nth 8 (save-match-data (syntax-ppss)))
        ;; Read form and get key-sequence
        (goto-char (match-beginning 0))
        (let ((seq (package-lint--extract-key-sequence
                    (read (current-buffer)))))
          (when seq
            (let ((message (package-lint--test-keyseq seq)))
              (when message
                (package-lint--error-at-point 'warning message)))))))))

(defun package-lint--check-commentary-existence ()
  "Warn about nonexistent or empty commentary section."
  (let ((start (lm-commentary-start)))
    (if (null start)
        (package-lint--error
         1 1 'error
         "Package should have a ;;; Commentary section.")
      ;; Skip over the section header.
      (goto-char start)
      (forward-line)
      (when (package-lint--region-empty-p (point) (lm-commentary-end))
        (goto-char start)
        (package-lint--error-at-point
         'error
         "Package should have a non-empty ;;; Commentary section.")))))

(defun package-lint--check-autoloads-on-private-functions (definitions)
  "Verify that private functions DEFINITIONS don't have autoload cookies."
  (pcase-dolist (`(,symbol . ,position) definitions)
    (when (string-match-p (rx "--") symbol)
      (goto-char position)
      (forward-line -1)
      (when (looking-at-p (rx ";;;###autoload"))
        (package-lint--error-at-point
         'warning
         "Private functions generally should not be autoloaded.")))))

(defun package-lint--check-for-literal-emacs-path ()
  "Verify package does not refer to \"\.emacs\.d\" literally.
Instead it should use `user-emacs-directory' or `locate-user-emacs-file'."
  (goto-char (point-min))
  ;; \b won't find a word boundary between a symbol and the "." in
  ;; ".emacs.d". / is a valid symbol constituent in Emacs Lisp, so
  ;; must be explicitly blacklisted.
  (while (re-search-forward "\\(?:\\_<\\|/\\)\\.emacs\\.d\\b" nil t)
    (unless (nth 4 (syntax-ppss))
      ;; Not in a comment
      (package-lint--error-at-point
       'warning
       "Use variable `user-emacs-directory' or function `locate-user-emacs-file' instead of a literal path to the Emacs user directory or files."))))

(defun package-lint--check-keywords-list ()
  "Verify that package keywords are listed in `finder-known-keywords'."
  (when (package-lint--goto-header "Keywords")
    (let ((line-no (line-number-at-pos))
          (keywords (lm-keywords-list)))
      (unless (cl-some (lambda (keyword) (assoc (intern keyword) finder-known-keywords)) keywords)
        (package-lint--error
         line-no 1 'warning
         (format "You should include standard keywords: see the variable `finder-known-keywords'."))))))

(defun package-lint--check-url-header ()
  "Verify that the package has an HTTPS or HTTP Homepage/URL header."
  (if (package-lint--goto-header "\\(?:URL\\|Homepage\\)")
      (let ((url (match-string 3))
            (url-start (match-beginning 3)))
        (unless (and (equal (thing-at-point 'url) url)
                     (string-match-p "^https?://" url))
          (package-lint--error
           (line-number-at-pos)
           (1+ (- url-start (line-beginning-position)))
           'error
           "Package URLs should be a single HTTPS or HTTP URL.")))
    (package-lint--error
     1 1 'error
     "Package should have a Homepage or URL header.")))

(defun package-lint--check-dependency-list ()
  "Check the contents of the \"Package-Requires\" header.
Return a list of well-formed dependencies, same as
`package-lint--check-well-formed-dependencies'."
  (when (package-lint--goto-header "Package-Requires")
    (let ((position (match-beginning 3))
          (line-no (line-number-at-pos))
          (deps (match-string 3)))
      (condition-case err
          (pcase-let ((`(,parsed-deps . ,parse-end-pos) (read-from-string deps)))
            (unless (= parse-end-pos (length deps))
              (package-lint--error
               line-no 1 'error
               "More than one expression provided."))
            (let ((deps (package-lint--check-well-formed-dependencies position line-no parsed-deps)))
              (package-lint--check-packages-installable deps)
              (package-lint--check-deps-use-non-snapshot-version deps)
              (package-lint--check-deps-do-not-use-zero-versions deps)
              (package-lint--check-do-not-depend-on-cl-lib-1.0 deps)
              deps))
        (error
         (package-lint--error
          line-no 1 'error
          (format "Couldn't parse \"Package-Requires\" header: %s" (error-message-string err)))
         nil)))))

(defun package-lint--check-well-formed-dependencies (position line-no parsed-deps)
  "Check that dependencies listed at POSITION on LINE-NO are well-formed.
These PARSED-DEPS must have the format (name \"version\").
Return a list of well-formed dependencies, where each element is of
the form (PACKAGE-NAME PACKAGE-VERSION LINE-NO LINE-BEGINNING-OFFSET)."
  (let (valid-deps)
    (dolist (entry parsed-deps)
      (pcase entry
        ((and `(,package-name ,package-version)
              (guard (symbolp package-name))
              (guard (stringp package-version)))
         ;; Find the column at which the dependency is declared so we can
         ;; properly report the position of errors.
         (let ((offset
                (save-excursion
                  (goto-char position)
                  (let ((line-start (line-beginning-position))
                        (pattern
                         (format "( *\\(%s\\)\\(?:)\\|[^[:alnum:]_\\-].*?)\\)"
                                 (regexp-quote (symbol-name package-name)))))
                    (if (re-search-forward pattern (line-end-position) t)
                        (- (1+ (match-beginning 1)) line-start)
                      1)))))
           (if (ignore-errors (version-to-list package-version))
               (push (list package-name
                           (version-to-list package-version)
                           line-no
                           offset)
                     valid-deps)
             (package-lint--error
              line-no offset 'error
              (format "%S is not a valid version string: see `version-to-list'."
                      package-version)))))
        (_
         (package-lint--error
          line-no 1 'error
          (format "Expected (package-name \"version-num\"), but found %S." entry)))))
    valid-deps))

(defun package-lint--check-packages-installable (valid-deps)
  "Check that all VALID-DEPS are available for installation."
  (pcase-dolist (`(,package-name ,package-version ,line-no ,offset) valid-deps)
    (if (eq 'emacs package-name)
        (unless (version-list-<= '(24) package-version)
          (package-lint--error
           line-no offset 'error
           "You can only depend on Emacs version 24 or greater: package.el for Emacs 23 does not support the \"emacs\" pseudopackage."))
      ;; Not 'emacs
      (let ((archive-entry (assq package-name package-archive-contents)))
        (if archive-entry
            (let ((best-version (package-lint--lowest-installable-version-of package-name)))
              (when (version-list-< best-version package-version)
                (package-lint--error
                 line-no offset 'warning
                 (format "Version dependency for %s appears too high: try %s" package-name
                         (package-version-join best-version)))))
          (package-lint--error
           line-no offset 'error
           (format "Package %S is not installable." package-name)))))))

(defun package-lint--check-deps-use-non-snapshot-version (valid-deps)
  "Warn about any VALID-DEPS on snapshot versions of packages."
  (pcase-dolist (`(,package-name ,package-version ,line-no ,offset) valid-deps)
    (unless (version-list-< package-version '(19001201 1))
      (package-lint--error
       line-no offset 'warning
       (format "Use a non-snapshot version number for dependency on \"%S\" if possible."
               package-name)))))

(defun package-lint--check-deps-do-not-use-zero-versions (valid-deps)
  "Warn about VALID-DEPS on \"0\" versions of packages."
  (pcase-dolist (`(,package-name ,package-version ,line-no ,offset) valid-deps)
    (when (equal package-version '(0))
      (package-lint--error
       line-no offset 'warning
       (format "Use a properly versioned dependency on \"%S\" if possible."
               package-name)))))

(defun package-lint--check-lexical-binding-requires-emacs-24 (valid-deps)
  "Warn about use of `lexical-binding' when Emacs 24 is not among VALID-DEPS."
  (goto-char (point-min))
  (when (package-lint--lexical-binding-declared-in-header-line-p)
    (let* ((lexbind-line (line-number-at-pos))
           (lexbind-col (1+ (- (match-beginning 1) (line-beginning-position)))))
      (unless (assq 'emacs valid-deps)
        (package-lint--error
         lexbind-line lexbind-col 'warning
         "You should depend on (emacs \"24\") if you need lexical-binding.")))))

(defun package-lint--inside-comment-or-string-p ()
  "Return non-nil if point is inside a comment or string."
  (let ((ppss (save-match-data (syntax-ppss))))
    (or (nth 3 ppss) (nth 4 ppss))))

(defun package-lint--seen-fboundp-check-for (sym)
  "Return non-nil if a `fboundp' check for SYM is present before point."
  (save-excursion
    (save-match-data
      (and (re-search-backward
            (concat "(fboundp\\s-+'" (regexp-quote sym) "\\_>") (point-min) t)
           (not (package-lint--inside-comment-or-string-p))))))

(defun package-lint--check-version-regexp-list (valid-deps list rx-start rx-end)
  "Warn about any match of REGEXP when VERSION is not in VALID-DEPS.
LIST is an alist of (VERSION . REGEXP*).
REGEXP is (concat RX-START REGEXP* RX-END) for each REGEXP*."
  (let ((emacs-version-dep (or (cadr (assq 'emacs valid-deps)) '(0))))
    (pcase-dolist (`(,added-in-version . ,regexp) list)
      (when (version-list-< emacs-version-dep added-in-version)
        (goto-char (point-min))
        (while (re-search-forward (concat rx-start regexp rx-end) nil t)
          (unless (package-lint--inside-comment-or-string-p)
            (let ((sym (match-string-no-properties 1)))
              (unless (package-lint--seen-fboundp-check-for sym)
                (save-excursion
                  (goto-char (match-beginning 1))
                  (package-lint--error-at-point
                   'error
                   (format "You should depend on (emacs \"%s\") if you need `%s'."
                           (mapconcat #'number-to-string added-in-version ".")
                           sym)))))))))))

(defun package-lint--check-libraries-available-in-emacs (valid-deps)
  "Warn about use of libraries that are not available in the Emacs version in VALID-DEPS."
  (package-lint--check-version-regexp-list
   valid-deps
   package-lint--libraries-added-alist
   "(\\s-*?require\\s-*?'\\("
   ;; Match the ending paren so we can be sure it's a single argument
   ;; `require'. If there are additional arguments, we don't want to warn,
   ;; because (require 'foo nil t) indicates an optional dependency and
   ;; (require 'foo "filename") is very uncommon.
   "\\)\\_>\\s-*?)"))

(defun package-lint--check-macros-functions-available-in-emacs (valid-deps)
  "Warn about use of functions/macros that are not available in the Emacs version in VALID-DEPS."
  (package-lint--check-version-regexp-list
   valid-deps
   package-lint--functions-and-macros-added-alist
   "(\\s-*?\\("
   "\\)\\_>"))

(defun package-lint--check-lexical-binding-is-on-first-line ()
  "Check that any `lexical-binding' declaration is on the first line of the file."
  (cl-block return
    (let ((original-buffer (current-buffer)))
      (with-temp-buffer
        (let ((lexical-binding-found-at-end nil))
          (insert-buffer-substring-no-properties original-buffer)
          (condition-case err
              (cl-letf (((symbol-function #'hack-local-variables-apply) #'ignore)
                        ((symbol-function #'hack-local-variables-filter)
                         (lambda (variables _dir-name)
                           (setq file-local-variables-alist variables)))
                        ;; Silence any messages Emacs may want to share with the user.
                        ;; There's no user.
                        ((symbol-function #'display-warning) #'ignore)
                        ((symbol-function #'message) #'ignore))
                ;; HACK: this is an internal variable!
                ;; Unfortunately, Emacsen that have this variable also have
                ;; `hack-local-variables' that doesn't store `lexical-binding'
                ;; in `file-local-variables-alist'.
                (defvar enable-dir-local-variables)
                (defvar hack-local-variables--warned-lexical)
                (let ((hack-local-variables--warned-lexical nil)
                      (enable-dir-local-variables nil)
                      (enable-local-variables t)
                      (local-enable-local-variables t))
                  (hack-local-variables)
                  (setq lexical-binding-found-at-end
                        hack-local-variables--warned-lexical)))
            (error
             (package-lint--error 1 1 'error (error-message-string err))
             (cl-return-from return nil)))
          (when (or lexical-binding-found-at-end
                    ;; In case this is an Emacs from before `hack-local-variables'
                    ;; started to warn about `lexical-binding' on a line other
                    ;; than the first.
                    (and (cdr (assq 'lexical-binding file-local-variables-alist))
                         (not (package-lint--lexical-binding-declared-in-header-line-p))))
            (package-lint--error
             1 1 'error
             "`lexical-binding' must be set in the first line.")))))))

(defun package-lint--check-do-not-depend-on-cl-lib-1.0 (valid-deps)
  "Check that any dependency in VALID-DEPS on \"cl-lib\" is on a remotely-installable version."
  (let ((cl-lib-dep (assq 'cl-lib valid-deps)))
    (when cl-lib-dep
      (let ((cl-lib-version (nth 1 cl-lib-dep)))
        (when (version-list-<= '(1) cl-lib-version)
          (package-lint--error
           (nth 2 cl-lib-dep) (nth 3 cl-lib-dep) 'error
           (format "Depend on the latest 0.x version of cl-lib rather than on version \"%S\".
Alternatively, depend on (emacs \"24.3\") or greater, in which cl-lib is bundled."
                   cl-lib-version)))))))

(defun package-lint--check-package-version-present ()
  "Check that a valid \"Version\" header is present."
  (let ((version (package-lint--goto-header (rx (? "Package-") "Version"))))
    (if version
        (unless (ignore-errors (version-to-list version))
          (package-lint--error
           (line-number-at-pos)
           (1+ (- (match-beginning 3) (line-beginning-position)))
           'warning
           (format "\"%s\" is not a valid version. MELPA will handle this, but other archives will not." version)))
      (package-lint--error
       1 1 'warning
       "\"Version:\" or \"Package-Version:\" header is missing. MELPA will handle this, but other archives will not."))))

(defun package-lint--check-package-el-can-parse ()
  "Check that `package-buffer-info' can read metadata from this file.
If it can, return the read metadata."
  (condition-case err
      (let ((orig-buffer (current-buffer)))
        ;; We've reported version header issues separately, so rule them out here
        (with-temp-buffer
          (insert-buffer-substring-no-properties orig-buffer)
          (package-lint--update-or-insert-version "0")
          (package-buffer-info)))
    (error
     (package-lint--error
      1 1
      'error
      (format "package.el cannot parse this buffer: %s" (error-message-string err)))
     nil)))

(defun package-lint--check-package-summary (desc)
  "Check the summary for package with descriptor DESC.
DESC is a struct as returned by `package-buffer-info'."
  (let ((summary (package-lint--package-desc-summary desc)))
    (cond
     ((string= summary "")
      (package-lint--error
       1 1
       'warning
       "Package should have a non-empty summary."))
     ((> (length summary) 50)
      (package-lint--error
       1 1
       'warning
       "The package summary is too long. It should be at most 50 characters.")))
    (when (save-match-data
            (let ((case-fold-search t))
              (and (string-match "[^.]\\<emacs\\>" summary)
                   (not (string-match-p "[[:space:]]+lisp" summary (match-end 0))))))
      (package-lint--error
       1 1
       'warning
       "Including \"Emacs\" in the package description is usually redundant."))))

(defun package-lint--check-provide-form (desc)
  "Check the provide form for package with descriptor DESC.
DESC is a struct as returned by `package-buffer-info'."
  (let ((name (package-lint--package-desc-name desc))
        (feature (package-lint--provided-feature)))
    (unless (string-equal (symbol-name name) feature)
      (package-lint--error
       1 1
       'error
       (format "There is no (provide '%s) form." name)))))

(defun package-lint--check-symbol-separators (definitions)
  "Check that symbol DEFINITIONS don't contain non-standard separators."
  (pcase-dolist (`(,name . ,position) definitions)
    (when (and (string-match "[:/]" name)
               (not (string-match-p package-lint--sane-prefixes name)))
      (let ((match-pos (match-beginning 0)))
        ;; As a special case, allow `/=' when at the end of a symbol.
        (when (or (not (string-match (rx "/=" string-end) name))
                  (/= match-pos (match-beginning 0)))
          (goto-char position)
          (package-lint--error
           (line-number-at-pos) 1 'error
           (format "`%s' contains a non-standard separator `%s', use hyphens instead (see Elisp Coding Conventions)."
                   name (substring-no-properties name match-pos (1+ match-pos)))))))))

(defun package-lint--check-defs-prefix (definitions)
  "Verify that symbol DEFINITIONS start with package prefix."
  (let ((prefix (package-lint--get-package-prefix)))
    (when prefix
      (let ((prefix-re
             (rx-to-string
              `(seq string-start
                    (or (seq ,prefix (or "-" string-end))
                        (seq "global-"
                             ,prefix
                             (or "-mode"
                                 (seq "-" (* any) "-mode"))
                             string-end))))))
        (pcase-dolist (`(,name . ,position) definitions)
          (unless (or (string-match-p prefix-re name)
                      (string-match-p package-lint--sane-prefixes name)
                      (progn
                        (goto-char position)
                        (looking-at-p (rx (*? space) "(" (*? space) "defadvice" symbol-end))))
            (let ((line-no (line-number-at-pos position)))
              (package-lint--error
               line-no 1 'error
               (format "\"%s\" doesn't start with package's prefix \"%s\"."
                       name prefix)))))))))

(defun package-lint--check-minor-mode (def)
  "Offer up concerns about the minor mode definition DEF."
  (when (cl-search '(:global t) def)
    (package-lint--check-globalized-minor-mode def)))

(defun package-lint--check-globalized-minor-mode (def)
  "Offer up concerns about the global minor mode definition DEF."
  (let ((feature (intern (package-lint--provided-feature))))
    (unless (cl-search `(:require ',feature) def :test #'equal)
      (package-lint--error-at-point
       'error
       (format
        "Global minor modes must `:require' their defining file (i.e. \":require '%s\"), to support the customization variable of the same name." feature)))))

(defun package-lint--check-defgroup (def)
  "Offer up concerns about the customization group definition DEF."
  (when (symbolp (cadr def))
    (let ((group-name (symbol-name (cadr def))))
      (when (string-match "\\(.*\\)-mode$" group-name)
        (let ((parent (intern (match-string 1 group-name))))
          (unless (cl-search `(:group ',parent) def :test #'equal)
            (package-lint--error-at-point
             'error
             "Customization groups should not end in \"-mode\" unless that name would conflict with their parent group."))))))

  (unless (memq :group def)
    (package-lint--error-at-point
     'error
     "Customization groups should specify a parent via `:group'.")))


;;; Helpers

(defun package-lint--extract-key-sequence (form)
  "Extract the key sequence from FORM."
  (pcase form
    (`(kbd ,seq)
     (package-lint--extract-key-sequence seq))
    ((or `(global-set-key ,seq ,_) `(local-set-key ,seq ,_))
     (package-lint--extract-key-sequence seq))
    (`(define-key ,_ ,seq ,_)
     (package-lint--extract-key-sequence seq))
    ((pred stringp)
     (listify-key-sequence (read-kbd-macro form)))
    ((pred vectorp)
     (unless (listp (elt form 0))
       (listify-key-sequence form)))))

(defun package-lint--test-keyseq (lks)
  "Return a message if the listified key sequence LKS is invalid, otherwise nil."
  (let* ((modifiers (event-modifiers lks))
         (basic-type (event-basic-type lks)))
    (when (or (and (> (length lks) 1) (equal (car (last lks)) ?\C-g))
              (and (equal (car (last lks)) ?\e)
                   (not (equal (nthcdr (- (length lks) 2) lks)
                               '(?\e ?\e))))
              (equal (car (last lks)) ?\C-h)
              (and (equal modifiers '(control))
                   (equal ?c basic-type)
                   (cdr lks)
                   (let ((v (event-basic-type (cdr lks)))
                         (m (event-modifiers (cdr lks))))
                     (and (<= v ?z)
                          (>= v ?a)
                          (or (null m) (equal '(shift) m)))))
              (member basic-type '(f5 f6 f7 f8 f9)))
      "This key sequence is reserved (see Key Binding Conventions in the Emacs Lisp manual)")))

(defun package-lint--region-empty-p (start end)
  "Return t iff the region between START and END has no non-empty lines.

Lines consisting only of whitespace or empty comments are considered empty."
  (save-excursion
    (save-restriction
      (let ((inhibit-changing-match-data t))
        (narrow-to-region start end)
        (goto-char start)
        (while (and (looking-at "^[[:space:]]*;*[[:space:]]*$")
                    (= 0 (forward-line))))
        (eobp)))))

(defun package-lint--lowest-installable-version-of (package)
  "Return the lowest version of PACKAGE available for installation."
  (let ((descriptors (cdr (assq package package-archive-contents))))
    (if (fboundp 'package-desc-version)
        (car (sort (mapcar 'package-desc-version descriptors)
                   #'version-list-<))
      (aref descriptors 0))))

(defun package-lint--goto-header (header-name)
  "Move to the first occurrence of HEADER-NAME in the file.
If the return value is non-nil, then point will be at the end of
the file, and the second and third match groups will contain the name and
value of the header with any leading or trailing whitespace removed."
  (let ((initial-point (point)))
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (re-search-forward (concat (lm-get-header-re header-name) "\\(.*?\\) *$") nil t)
          (match-string-no-properties 3)
        (goto-char initial-point)
        nil))))

(defun package-lint--update-or-insert-version (version)
  "Ensure current buffer has a \"Version: VERSION\" header."
  (if (package-lint--goto-header "Version")
      (move-beginning-of-line nil)
    (forward-line))
  (insert (format ";; Version: %s" version))
  (newline))

(defun package-lint--get-header-line-file-local-variables ()
  "Return local variables specified in the -*- line.
Returns an alist of elements (VAR . VAL), where VAR is a variable
and VAL is the specified value.

For details, see `hack-local-variables-prop-line'."
  (cl-letf (((symbol-function #'message) #'ignore))
    (hack-local-variables-prop-line)))

(defun package-lint--lexical-binding-declared-in-header-line-p ()
  "Test if `lexical-binding' is declared in the -*- line."
  ;; Test the `cdr' to see if it's actually true, because
  ;; -*- lexical-binding: nil -*-
  ;; is legal, if silly.
  (cdr (assq 'lexical-binding (package-lint--get-header-line-file-local-variables))))

(defvar semantic-imenu-summary-function)

(defun package-lint--get-defs ()
  "Return a list of all variables and functions defined in the current buffer.

The returned list is of the form (SYMBOL-NAME . POSITION)."
  ;; We probably could use Semantic instead, but it's a *global* minor mode and
  ;; it tends to be quite heavy, so use Imenu instead; if the user has Semantic
  ;; enabled, Imenu will use its index anyway.
  (let ((result '())
        (index
         (save-excursion
           ;; Use the default imenu expression list so that we're not confused
           ;; by user customizations.
           (let ((imenu-generic-expression lisp-imenu-generic-expression)
                 ;; In case it's actually Semantic, tell it not to decorate
                 ;; symbol names.
                 (semantic-imenu-summary-function 'semantic-format-tag-name))
             (funcall imenu-create-index-function)))))
    (dolist (entry index)
      (pcase entry
        ((and `(,submenu-name . ,submenu-elements)
              (guard (consp submenu-elements)))
         (when (member submenu-name '("Variables" "Defuns"))
           (setq result (nconc (reverse submenu-elements) result))))
        (_
         (push entry result))))
    ;; If it's Semantic, then it returns overlays, not positions. Convert
    ;; them.
    (dolist (entry result)
      (when (overlayp (cdr entry))
        (setcdr entry (overlay-start (cdr entry)))))
    (nreverse result)))

(defun package-lint--provided-feature ()
  "Return the first-provided feature name, as a string, or nil if none."
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward (rx "(provide '" (group (1+ (or (syntax word) (syntax symbol))))) nil t)
      (match-string-no-properties 1))))

(defun package-lint--get-package-prefix ()
  "Return package prefix string (i.e. the symbol the package `provide's).
Prefix is returned without any `-mode' suffix."
  (let ((feature (package-lint--provided-feature)))
    (when feature
      (replace-regexp-in-string "-mode\\'" "" feature))))

(defun package-lint--check-objects-by-regexp (regexp function)
  "Check all objects with the literal printed form matching REGEXP.

The objects are parsed with `read'. The FUNCTION is passed the
read object, with the point at the beginning of the match.

S-expressions in comments or comments, partial s-expressions, or
otherwise invalid read forms are ignored."
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (save-excursion
      (goto-char (match-beginning 0))
      (let ((obj (unless (package-lint--inside-comment-or-string-p)
                   (save-excursion
                     (ignore-errors (read (current-buffer)))))))
        (when obj (funcall function obj))))))


;;; Public interface

;;;###autoload
(defun package-lint-buffer (&optional buffer)
  "Get linter errors and warnings for BUFFER.

Returns a list, each element of which is list of

   (LINE COL TYPE MESSAGE)

where TYPE is either 'warning or 'error.

Current buffer is used if none is specified."
  (with-current-buffer (or buffer (current-buffer))
    (package-lint--check-all)))

;;;###autoload
(defun package-lint-current-buffer ()
  "Display lint errors and warnings for the current buffer."
  (interactive)
  (let ((errs (package-lint-buffer))
        (buf "*Package-Lint*"))
    (with-current-buffer (get-buffer-create buf)
      (let ((buffer-read-only nil))
        (erase-buffer)
        (cond
         ((null errs) (insert "No issues found."))
         ((null (cdr errs)) (insert "1 issue found:\n\n"))
         (t (insert (format "%d issues found:\n\n" (length errs)))))
        (pcase-dolist (`(,line ,col ,type ,message) errs)
          (insert (format "%d:%d: %s: %s\n" line col type message))))
      (special-mode)
      (view-mode 1))
    (display-buffer buf)))

;;;###autoload
(defun package-lint-batch-and-exit ()
  "Run `package-lint-buffer' on the files remaining on the command line.
Use this only with -batch, it won't work interactively.

When done, exit Emacs with status 0 if there were no errors nor warnings or 1
otherwise."
  (unless noninteractive
    (error "`package-lint-batch-and-exit' is to be used only with -batch"))
  ;; Make sure package.el is initialized so we can query its database.
  (package-initialize)
  (let ((success t))
    (dolist (file command-line-args-left)
      (with-temp-buffer
        (insert-file-contents file t)
        (emacs-lisp-mode)
        (let ((checking-result (package-lint-buffer)))
          (when checking-result
            (setq success nil)
            (message "In `%s':" file)
            (pcase-dolist (`(,line ,col ,type ,message) checking-result)
              (message "  at %d:%d: %s: %s" line col type message))))))
    (kill-emacs (if success 0 1))))

;;;###autoload
(defun package-lint-looks-like-a-package-p ()
  "Return non-nil if the current buffer appears to be intended as a package."
  (save-match-data
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (re-search-forward
         (concat lm-header-prefix
                 (rx (or "Version" "Package-Version" "Package-Requires")))
         nil t)))))

(provide 'package-lint)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; package-lint.el ends here
