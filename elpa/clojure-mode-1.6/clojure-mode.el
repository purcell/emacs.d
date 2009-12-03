;;; clojure-mode.el --- Major mode for Clojure code

;; Copyright (C) 2007, 2008, 2009 Jeffrey Chu, Lennart Staflin, Phil Hagelberg
;;
;; Authors: Jeffrey Chu <jochu0@gmail.com>
;;          Lennart Staflin <lenst@lysator.liu.se>
;;          Phil Hagelberg <technomancy@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/ClojureMode
;; Version: 1.6
;; Keywords: languages, lisp

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides font-lock, indentation, and navigation for the Clojure
;; language. (http://clojure.org)

;;; Installation:

;; If you use ELPA (http://tromey.com/elpa), you can install via the
;; M-x package-list-packages interface. This is preferrable as you
;; will have access to updates automatically.

;; If you need to install by hand for some reason:

;; (0) Add this file to your load-path, usually the ~/.emacs.d directory.
;; (1) Either:
;;     Add this to your .emacs config: (require 'clojure-mode)
;;     Or generate autoloads with the `update-directory-autoloads' function.

;; See also the swank-clojure package for better interaction with
;; Clojure subprocesses. Note that M-x clojure-install functionality
;; has been moved to that package and is deprecated here.

;; Users of older Emacs (pre-22) should get version 1.4:
;; http://github.com/technomancy/clojure-mode/tree/1.4

;; Paredit users:

;; Download paredit v21 or greater
;;    http://mumble.net/~campbell/emacs/paredit.el

;; Use paredit as you normally would with any other mode; for instance:
;;
;;   ;; require or autoload paredit-mode
;;   (defun lisp-enable-paredit-hook () (paredit-mode 1))
;;   (add-hook 'clojure-mode-hook 'lisp-enable-paredit-hook)

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

(require 'cl)

(defgroup clojure-mode nil
  "A mode for Clojure"
  :prefix "clojure-mode-"
  :group 'applications)

(defcustom clojure-mode-font-lock-comment-sexp nil
  "Set to non-nil in order to enable font-lock of (comment...)
forms. This option is experimental. Changing this will require a
restart (ie. M-x clojure-mode) of existing clojure mode buffers."
  :type 'boolean
  :group 'clojure-mode)

(defcustom clojure-mode-load-command  "(clojure.core/load-file \"%s\")\n"
  "*Format-string for building a Clojure expression to load a file.
This format string should use `%s' to substitute a file name
and should result in a Clojure expression that will command the inferior
Clojure to load that file."
  :type 'string
  :group 'clojure-mode)

(defcustom clojure-mode-use-backtracking-indent nil
  "Set to non-nil to enable backtracking/context sensitive indentation."
  :type 'boolean
  :group 'clojure-mode)

(defcustom clojure-max-backtracking 3
  "Maximum amount to backtrack up a list to check for context."
  :type 'integer
  :group 'clojure-mode)

(defvar clojure-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map "\e\C-x" 'lisp-eval-defun)
    (define-key map "\C-x\C-e" 'lisp-eval-last-sexp)
    (define-key map "\C-c\C-e" 'lisp-eval-last-sexp)
    (define-key map "\C-c\C-l" 'clojure-load-file)
    (define-key map "\C-c\C-r" 'lisp-eval-region)
    (define-key map "\C-c\C-z" 'run-lisp)
    map)
  "Keymap for Clojure mode. Inherits from `lisp-mode-shared-map'.")

(defvar clojure-mode-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?~ "'   " table)
    (modify-syntax-entry ?, "    " table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?^ "'" table)
    table))

(defvar clojure-mode-abbrev-table nil
  "Abbrev table used in clojure-mode buffers.")

(define-abbrev-table 'clojure-mode-abbrev-table ())

(defvar clojure-prev-l/c-dir/file nil
  "Record last directory and file used in loading or compiling.
This holds a cons cell of the form `(DIRECTORY . FILE)'
describing the last `clojure-load-file' or `clojure-compile-file' command.")

(defvar clojure-def-regexp "^\\s *\\((def\\S *\\s +\\(\[^ \n\t\]+\\)\\)"
  "A regular expression to match any top-level definitions.")

;;;###autoload
(defun clojure-mode ()
  "Major mode for editing Clojure code - similar to Lisp mode..
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{clojure-mode-map}
Note that `run-lisp' may be used either to start an inferior Lisp job
or to switch back to an existing one.

Entry to this mode calls the value of `clojure-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map clojure-mode-map)
  (setq major-mode 'clojure-mode)
  (setq mode-name "Clojure")
  (lisp-mode-variables nil)
  (set-syntax-table clojure-mode-syntax-table)

  (setq local-abbrev-table clojure-mode-abbrev-table)

  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (set (make-local-variable 'lisp-indent-function)
       'clojure-indent-function)
  (set (make-local-variable 'lisp-doc-string-elt-property)
	   'clojure-doc-string-elt)
  (set (make-local-variable 'font-lock-multiline) t)

  (setq lisp-imenu-generic-expression
        `((nil ,clojure-def-regexp 2)))
  (setq imenu-create-index-function
        (lambda ()
          (imenu--generic-function lisp-imenu-generic-expression)))

  (add-to-list 'font-lock-extend-region-functions
               'clojure-font-lock-extend-region-def t)

  (when clojure-mode-font-lock-comment-sexp
    (add-to-list 'font-lock-extend-region-functions
                 'clojure-font-lock-extend-region-comment t)
    (make-local-variable 'clojure-font-lock-keywords)
    (add-to-list 'clojure-font-lock-keywords
                 'clojure-font-lock-mark-comment t)
    (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil))

  (setq font-lock-defaults
        '(clojure-font-lock-keywords    ; keywords
          nil nil
          (("+-*/.<>=!?$%_&~^:@" . "w")) ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . lisp-font-lock-syntactic-face-function)))

  (run-mode-hooks 'clojure-mode-hook)

  ;; Enable curly braces when paredit is enabled in clojure-mode-hook
  (when (and (featurep 'paredit) paredit-mode (>= paredit-version 21))
    (define-key clojure-mode-map "{" 'paredit-open-curly)
    (define-key clojure-mode-map "}" 'paredit-close-curly)))

;; (define-key clojure-mode-map "{" 'self-insert-command)
;; (define-key clojure-mode-map "}" 'self-insert-command)

(defun clojure-font-lock-def-at-point (point)
  "Find the position range between the top-most def* and the
fourth element afterwards. Note that this means there's no
gaurantee of proper font locking in def* forms that are not at
top-level."
  (goto-char point)
  (condition-case nil
      (beginning-of-defun)
    (error nil))

  (let ((beg-def (point)))
    (when (and (not (= point beg-def))
               (looking-at "(def"))
      (condition-case nil
          (progn
            ;; move forward as much as possible until failure (or success)
            (forward-char)
            (dotimes (i 4)
              (forward-sexp)))
        (error nil))
      (cons beg-def (point)))))

(defun clojure-font-lock-extend-region-def ()
  "Move fontification boundaries to always include the first four
elements of a def* forms."
  (let ((changed nil))
    (let ((def (clojure-font-lock-def-at-point font-lock-beg)))
      (when def
        (destructuring-bind (def-beg . def-end) def
          (when (and (< def-beg font-lock-beg)
                     (< font-lock-beg def-end))
            (setq font-lock-beg def-beg
                  changed t)))))

    (let ((def (clojure-font-lock-def-at-point font-lock-end)))
      (when def
        (destructuring-bind (def-beg . def-end) def
          (when (and (< def-beg font-lock-end)
                     (< font-lock-end def-end))
            (setq font-lock-end def-end
                  changed t)))))
    changed))

(defun clojure-font-lock-extend-region-comment ()
  "Move fontification boundaries to always contain
  entire (comment ..) sexp. Does not work if you have a
  white-space between ( and comment, but that is omitted to make
  this run faster."
  (let ((changed nil))
    (goto-char font-lock-beg)
    (condition-case nil (beginning-of-defun) (error nil))
    (let ((pos (re-search-forward "(comment\\>" font-lock-end t)))
      (when pos
        (forward-char -8)
        (when (< (point) font-lock-beg)
          (setq font-lock-beg (point)
                changed t))
        (condition-case nil (forward-sexp) (error nil))
        (when (> (point) font-lock-end)
          (setq font-lock-end (point)
                changed t))))
    changed))

(defun clojure-font-lock-mark-comment (limit)
  "Marks all (comment ..) forms with font-lock-comment-face."
  (let (pos)
    (while (and (< (point) limit)
                (setq pos (re-search-forward "(comment\\>" limit t)))
      (when pos
        (forward-char -8)
        (condition-case nil
            (add-text-properties (1+ (point)) (progn
                                                (forward-sexp) (1- (point)))
                                 '(face font-lock-comment-face multiline t))
          (error (forward-char 8))))))
  nil)

(defconst clojure-font-lock-keywords
  (eval-when-compile
    `( ;; Definitions.
      (,(concat "(\\(?:clojure.core/\\)?\\("
                (regexp-opt '("defn" "defn-"
                              "defmulti" "defmethod"
                              "defmacro"
                              "deftest"
                              "defstruct"
                              "def" "defonce"))
                ;; Function declarations.
                "\\)\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)?"

                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      ;; Control structures
      (,(concat
         "(\\(?:clojure.core/\\)?"
         (regexp-opt
          '("let" "letfn" "do"
            "cond" "condp"
            "for" "loop" "recur"
            "when" "when-not" "when-let" "when-first"
            "if" "if-let" "if-not"
            "." ".." "->" "doto"
            "and" "or"
            "dosync" "doseq" "dotimes" "dorun" "doall"
            "load" "import" "unimport" "ns" "in-ns" "refer"
            "try" "catch" "finally" "throw"
            "with-open" "with-local-vars" "binding"
            "gen-class" "gen-and-load-class" "gen-and-save-class") t)
         "\\>")
       .  1)
      ;; Built-ins
      (,(concat
         "(\\(?:clojure.core/\\)?"
         (regexp-opt
          '("implement" "proxy" "lazy-seq" "with-meta"
            "struct" "struct-map" "delay" "locking" "sync" "time" "apply"
            "remove" "merge" "interleave" "interpose" "distinct" "for"
            "cons" "concat" "lazy-cat" "cycle" "rest" "frest" "drop"
            "drop-while" "nthrest" "take" "take-while" "take-nth" "butlast"
            "reverse" "sort" "sort-by" "split-at" "partition" "split-with"
            "first" "ffirst" "rfirst" "when-first" "zipmap" "into" "set" "vec"
            "to-array-2d" "not-empty" "seq?" "not-every?" "every?" "not-any?"
            "map" "mapcat" "vector?" "list?" "hash-map" "reduce" "filter"
            "vals" "keys" "rseq" "subseq" "rsubseq" "count" "empty?"
            "fnseq" "repeatedly" "iterate" "drop-last"
            "repeat" "replicate" "range"  "into-array"
            "line-seq" "resultset-seq" "re-seq" "re-find" "tree-seq" "file-seq"
            "iterator-seq" "enumeration-seq" "declare"  "xml-seq"
            "symbol?" "string?" "vector" "conj" "str"
            "pos?" "neg?" "zero?" "nil?" "inc" "format"
            "alter" "commute" "ref-set" "floor" "assoc" "send" "send-off" ) t)
         "\\>")
       1 font-lock-builtin-face)
      ;; (fn name? args ...)
      (,(concat "(\\(?:clojure.core/\\)?\\(fn\\)[ \t]+"
                ;; Possibly type
                "\\(?:#^\\sw+[ \t]*\\)?"
                ;; Possibly name
                "\\(\\sw+\\)?" )
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      ;; Constant values (keywords).
      ("\\<:\\(\\sw\\|#\\)+\\>" 0 font-lock-builtin-face)
      ;; Meta type annotation #^Type
      ("#^\\sw+" 0 font-lock-type-face)
      ("\\<io\\!\\>" 0 font-lock-warning-face)))
  "Default expressions to highlight in Clojure mode.")

;; Docstring positions
(put 'defn 'clojure-doc-string-elt 2)
(put 'defn- 'clojure-doc-string-elt 2)
(put 'defmulti 'clojure-doc-string-elt 2)
(put 'defmacro 'clojure-doc-string-elt 2)

(defun clojure-load-file (file-name)
  "Load a Lisp file into the inferior Lisp process."
  (interactive (comint-get-source "Load Clojure file: "
                                  clojure-prev-l/c-dir/file
                                  '(clojure-mode) t))
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq clojure-prev-l/c-dir/file (cons (file-name-directory file-name)
                                        (file-name-nondirectory file-name)))
  (comint-send-string (inferior-lisp-proc)
                      (format clojure-mode-load-command file-name))
  (switch-to-lisp t))

(defun clojure-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
It is used when indenting a line within a function call, to see if the
called function says anything special about how to indent the line.

INDENT-POINT is the position where the user typed TAB, or equivalent.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function
which has a non-nil property `lisp-indent-function',
that specifies how to do the indentation.  The property value can be
* `defun', meaning indent `defun'-style;
* an integer N, meaning indent the first N arguments specially
  like ordinary function arguments and then indent any further
  arguments like a body;
* a function to call just as this function was called.
  If that function returns nil, that means it doesn't specify
  the indentation.

This function also returns nil meaning don't specify the indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (if (and (eq (char-after (point)) ?\[)
                   (eq (char-after (elt state 1)) ?\())
              (+ (current-column) 2) ;; this is probably inside a defn
            (current-column)))
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            (open-paren (elt state 1))
            method)
        (setq method (get (intern-soft function) 'clojure-indent-function))

        (cond ((member (char-after open-paren) '(?\[ ?\{))
               (goto-char open-paren)
               (1+ (current-column)))
              ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`\\(?:\\S +/\\)?def\\|with-"
                                      function)))
               (lisp-indent-defform state indent-point))

              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))
              (clojure-mode-use-backtracking-indent
               (clojure-backtracking-indent
                indent-point state normal-indent)))))))

(defun clojure-backtracking-indent (indent-point state normal-indent)
  "Experimental backtracking support. Will upwards in an sexp to
check for contextual indenting."
  (let (indent (path) (depth 0))
    (goto-char (elt state 1))
    (while (and (not indent)
                (< depth clojure-max-backtracking))
      (let ((containing-sexp (point)))
        (parse-partial-sexp (1+ containing-sexp) indent-point 1 t)
        (when (looking-at "\\sw\\|\\s_")
          (let* ((start (point))
                 (fn (buffer-substring start (progn (forward-sexp 1) (point))))
                 (meth (get (intern-soft fn) 'clojure-backtracking-indent)))
            (let ((n 0))
              (when (< (point) indent-point)
                (condition-case ()
                    (progn
                      (forward-sexp 1)
                      (while (< (point) indent-point)
                        (parse-partial-sexp (point) indent-point 1 t)
                        (incf n)
                        (forward-sexp 1)))
                  (error nil)))
              (push n path))
            (when meth
              (let ((def meth))
                (dolist (p path)
                  (if (and (listp def)
                           (< p (length def)))
                      (setq def (nth p def))
                    (if (listp def)
                        (setq def (car (last def)))
                      (setq def nil))))
                (goto-char (elt state 1))
                (when def
                  (setq indent (+ (current-column) def)))))))
        (goto-char containing-sexp)
        (condition-case ()
            (progn
              (backward-up-list 1)
              (incf depth))
          (error (setq depth clojure-max-backtracking)))))
    indent))

;; clojure backtracking indent is experimental and the format for these
;; entries are subject to change
(put 'implement 'clojure-backtracking-indent '(4 (2)))
(put 'letfn 'clojure-backtracking-indent '((2) 2))
(put 'proxy 'clojure-backtracking-indent '(4 4 (2)))

(defun put-clojure-indent (sym indent)
  (put sym 'clojure-indent-function indent)
  (put (intern (format "clojure/%s" (symbol-name sym)))
       'clojure-indent-function indent))

(defmacro define-clojure-indent (&rest kvs)
  `(progn
     ,@(mapcar (lambda (x) `(put-clojure-indent
                        (quote ,(first x)) ,(second x))) kvs)))

(define-clojure-indent
  (catch 2)
  (defmuti 1)
  (do 0)
  (for 1)
  (if 1)
  (if-not 1)
  (let 1)
  (letfn 1)
  (loop 1)
  (struct-map 1)
  (assoc 1)
  (condp 2)

  (fn 'defun)
  (testing 1))

;; built-ins
(define-clojure-indent
  (ns 1)
  (binding 1)
  (comment 0)
  (defstruct 1)
  (doseq 1)
  (dotimes 1)
  (doto 1)
  (implement 1)
  (let 1)
  (when-let 1)
  (if-let 1)
  (locking 1)
  (proxy 2)
  (sync 1)
  (when 1)
  (when-first 1)
  (when-let 1)
  (when-not 1)
  (with-local-vars 1)
  (with-open 1)
  (with-precision 1))

;;; SLIME integration

;;;###autoload
(progn
  (defcustom clojure-src-root (expand-file-name "~/src")
    "Directory that contains checkouts for clojure, clojure-contrib,
slime, and swank-clojure. This value is used by `clojure-install'
and `clojure-slime-config'."
    :type 'string
    :group 'clojure-mode)

  ;; We want this function to be able to be loaded without loading the
  ;; whole of clojure-mode.el since it runs at every startup.
  (defun clojure-slime-config (&optional src-root)
    "Load Clojure SLIME support out of the `clojure-src-root' directory.

Since there's no single conventional place to keep Clojure, this
is bundled up as a function so that you can call it after you've set
`clojure-src-root' in your personal config."

    (if src-root (setq clojure-src-root src-root))

    (add-to-list 'load-path (concat clojure-src-root "/slime"))
    (add-to-list 'load-path (concat clojure-src-root "/slime/contrib"))
    (add-to-list 'load-path (concat clojure-src-root "/swank-clojure"))

    (require 'slime-autoloads)
    (require 'swank-clojure-autoload)

    (slime-setup '(slime-fancy))

    (setq swank-clojure-classpath
          (list
           (concat clojure-src-root "/clojure/clojure.jar")
           (concat clojure-src-root "/clojure-contrib/clojure-contrib.jar")))
    (eval-after-load 'slime
      '(progn (require 'swank-clojure)
              (setq slime-lisp-implementations
                    (cons `(clojure ,(swank-clojure-cmd) :init
                                    swank-clojure-init)
                          (remove-if #'(lambda (x) (eq (car x) 'clojure))
                                     slime-lisp-implementations)))))))

;;;###autoload
(defun clojure-install (src-root)
  "Perform the initial Clojure install along with Emacs support libs.

This requires git, a JVM, ant, and an active Internet connection.
Deprecated in favour of functionality in swank-clojure."
  (interactive (list
                (read-string (concat "Install Clojure in (default: "
                                     clojure-src-root "): ")
                             nil nil clojure-src-root)))

  (when (y-or-n-p
         "This function is deprecated in favour of swank-clojure. \
See http://technomancy.us/swank-clojure for details. Abort? ")
    (error "Aborted!"))

  (let ((orig-directory default-directory))
    (make-directory src-root t)
    (cd src-root)

    (if (file-exists-p (concat src-root "/clojure"))
        (error "Clojure is already installed at %s/clojure" src-root))

    (message "Checking out source... this will take a while...")
    (dolist (cmd '("git clone git://github.com/richhickey/clojure.git"
                   "git clone git://github.com/richhickey/clojure-contrib.git"
                   "git clone --depth 2 git://github.com/technomancy/slime.git"
                   "git clone git://github.com/technomancy/swank-clojure.git"))
      (unless (= 0 (shell-command cmd))
        (error "Clojure installation step failed: %s" cmd)))

    (cd (format "%s/clojure" src-root))
    (shell-command "git checkout 1.0")
    (unless (= 0 (shell-command "ant"))
      (error "Couldn't compile Clojure."))

    (cd (format "%s/clojure-contrib" src-root))
    (shell-command "git checkout clojure-1.0-compatible")
    (unless (= 0 (shell-command "ant -Dclojure.jar=../clojure/clojure.jar"))
      (error "Couldn't compile Contrib."))

    (with-output-to-temp-buffer "clojure-install-note"
      (princ
       (if (equal src-root clojure-src-root)
           "Add a call to \"\(clojure-slime-config\)\"
to your .emacs so you can use SLIME in future sessions."
         (setq clojure-src-root src-root)
         (format "You've installed clojure in a non-default location. If you
want to use this installation in the future, you will need to add the following
lines to your personal Emacs config somewhere:

\(clojure-slime-config \"%s\"\)" src-root)))
      (princ "\n\n Press M-x slime to launch Clojure."))

    (clojure-slime-config)
    (cd orig-directory)))

(defun clojure-update ()
  "Update clojure-related repositories from upstream master and recompile clojure.

Works with clojure etc. installed via `clojure-install'. Code
should be checked out in the `clojure-src-root' directory."
  (interactive)

  (message "Updating...")
  (let ((orig-directory default-directory))
    (dolist (repo '("clojure" "clojure-contrib" "swank-clojure" "slime"))
      (cd (concat clojure-src-root "/" repo))
      (unless (= 0 (shell-command "git checkout master && git pull"))
        (error "Clojure update failed: %s" repo)))

    (message "Compiling...")
    (cd (format "%s/clojure" clojure-src-root))
    (unless (= 0 (shell-command "ant"))
      (error "Couldn't compile Clojure."))

    (cd (format "%s/clojure-contrib" clojure-src-root))
    (unless (= 0 (shell-command "ant -Dclojure.jar=../clojure/clojure.jar"))
      (error "Couldn't compile Contrib."))
    
    (message "Finished updating Clojure.")
    (cd orig-directory)))

(defun clojure-enable-slime-on-existing-buffers ()
  (interactive)
  (add-hook 'clojure-mode-hook 'swank-clojure-slime-mode-hook)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if (equal major-mode 'clojure-mode)
          (swank-clojure-slime-mode-hook)))))

(add-hook 'slime-connected-hook 'clojure-enable-slime-on-existing-buffers)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(provide 'clojure-mode)
;;; clojure-mode.el ends here
