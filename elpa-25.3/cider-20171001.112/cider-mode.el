;;; cider-mode.el --- Minor mode for REPL interactions -*- lexical-binding: t -*-

;; Copyright © 2012-2013 Tim King, Phil Hagelberg, Bozhidar Batsov
;; Copyright © 2013-2017 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
;;         Artur Malabarba <bruce.connor.am@gmail.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Minor mode for REPL interactions.

;;; Code:

(require 'clojure-mode)
(require 'cider-interaction)
(require 'cider-test)
(require 'cider-eldoc)
(require 'cider-resolve)
(require 'cider-doc)
(require 'subr-x)
(require 'cider-compat)

(defcustom cider-mode-line-show-connection t
  "If the mode-line lighter should detail the connection."
  :group 'cider
  :type 'boolean
  :package-version '(cider "0.10.0"))

(defun cider--modeline-info ()
  "Return info for the `cider-mode' modeline.

Info contains project name and host:port endpoint."
  (if-let ((current-connection (ignore-errors (cider-current-connection))))
      (with-current-buffer current-connection
        (concat
         cider-repl-type
         (when cider-mode-line-show-connection
           (format ":%s@%s:%s"
                   (or (cider--project-name nrepl-project-dir) "<no project>")
                   (pcase (car nrepl-endpoint)
                     ("localhost" "")
                     (x x))
                   (cadr nrepl-endpoint)))))
    "not connected"))

;;;###autoload
(defcustom cider-mode-line
  '(:eval (format " cider[%s]" (cider--modeline-info)))
  "Mode line lighter for `cider-mode'.

The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for
details about mode line templates.

Customize this variable to change how `cider-mode' displays its
status in the mode line.  The default value displays the current connection.
Set this variable to nil to disable the mode line
entirely."
  :group 'cider
  :type 'sexp
  :risky t
  :package-version '(cider "0.7.0"))


;;; Switching between REPL & source buffers
(defvar-local cider-last-clojure-buffer nil
  "A buffer-local variable holding the last Clojure source buffer.
`cider-switch-to-last-clojure-buffer' uses this variable to jump
back to last Clojure source buffer.")

(defun cider-remember-clojure-buffer (buffer)
  "Try to remember the BUFFER from which the user jumps.
The BUFFER needs to be a Clojure buffer and current major mode needs
to be `cider-repl-mode'.  The user can use `cider-switch-to-last-clojure-buffer'
to jump back to the last Clojure source buffer."
  (when (and buffer
             (with-current-buffer buffer
               (derived-mode-p 'clojure-mode))
             (derived-mode-p 'cider-repl-mode))
    (setq cider-last-clojure-buffer buffer)))

(defun cider--switch-to-repl-buffer (repl-buffer &optional set-namespace)
  "Select the REPL-BUFFER, when possible in an existing window.

Hint: You can use `display-buffer-reuse-frames' and
`special-display-buffer-names' to customize the frame in which
the buffer should appear.

When SET-NAMESPACE is t, sets the namespace in the REPL buffer to
that of the namespace in the Clojure source buffer."
  (cider-ensure-connected)
  (let ((buffer (current-buffer)))
    ;; first we switch to the REPL buffer
    (if cider-repl-display-in-current-window
        (pop-to-buffer-same-window repl-buffer)
      (pop-to-buffer repl-buffer))
    ;; then if necessary we update its namespace
    (when set-namespace
      (cider-repl-set-ns (with-current-buffer buffer (cider-current-ns))))
    (cider-remember-clojure-buffer buffer)
    (goto-char (point-max))))

(defun cider-switch-to-repl-buffer (&optional set-namespace)
  "Select the REPL buffer, when possible in an existing window.
The buffer chosen is based on the file open in the current buffer.  If
multiple REPL buffers are associated with current connection the most
recent is used.

If the REPL buffer cannot be unambiguously determined, the REPL
buffer is chosen based on the current connection buffer and a
message raised informing the user.

Hint: You can use `display-buffer-reuse-frames' and
`special-display-buffer-names' to customize the frame in which
the buffer should appear.

With a prefix arg SET-NAMESPACE sets the namespace in the REPL buffer to that
of the namespace in the Clojure source buffer."
  (interactive "P")
  (let* ((connections (cider-connections))
         (buffer (seq-find (lambda (b) (member b connections))
                           (buffer-list))))
    (cider--switch-to-repl-buffer buffer set-namespace)))

(declare-function cider-load-buffer "cider-interaction")

(defun cider-load-buffer-and-switch-to-repl-buffer (&optional set-namespace)
  "Load the current buffer into the matching REPL buffer and switch to it.
When SET-NAMESPACE is true, we'll also set the REPL's ns to match that of the
Clojure buffer."
  (interactive "P")
  (cider-load-buffer)
  (cider-switch-to-repl-buffer set-namespace))

(defun cider-switch-to-last-clojure-buffer ()
  "Switch to the last Clojure buffer.
The default keybinding for this command is
the same as `cider-switch-to-repl-buffer',
so that it is very convenient to jump between a
Clojure buffer and the REPL buffer."
  (interactive)
  (if (and (derived-mode-p 'cider-repl-mode)
           (buffer-live-p cider-last-clojure-buffer))
      (if cider-repl-display-in-current-window
          (pop-to-buffer-same-window cider-last-clojure-buffer)
        (pop-to-buffer cider-last-clojure-buffer))
    (message "Don't know the original Clojure buffer")))

(defun cider-find-and-clear-repl-output (&optional clear-repl)
  "Find the current REPL buffer and clear it.
With a prefix argument CLEAR-REPL the command clears the entire REPL buffer.
Returns to the buffer in which the command was invoked."
  (interactive "P")
  (let ((origin-buffer (current-buffer)))
    (switch-to-buffer (cider-current-repl-buffer))
    (if clear-repl
        (cider-repl-clear-buffer)
      (cider-repl-clear-output))
    (switch-to-buffer origin-buffer)))


;;; The menu-bar
(defconst cider-mode-menu
  `("CIDER"
    ["Start a REPL" cider-jack-in
     :help "Starts an nREPL server (with lein, boot, or maven) and connects a REPL to it."]
    ["Connect to a REPL" cider-connect
     :help "Connects to a REPL that's already running."]
    ["Quit" cider-quit :active cider-connections]
    ["Restart" cider-restart :active cider-connections]
    ("Clojurescript"
     ["Start a Clojure REPL, and a ClojureScript REPL" cider-jack-in-clojurescript
      :help "Starts an nREPL server, connects a Clojure REPL to it, and then a ClojureScript REPL.
Configure `cider-cljs-*-repl' to change the ClojureScript REPL to use for your build tool."]
     ["Create a ClojureScript REPL from a Clojure REPL" cider-create-sibling-cljs-repl]
     ["Form for launching a ClojureScript REPL via Leiningen" (customize-variable 'cider-cljs-lein-repl)]
     ["Form for launching a ClojureScript REPL via Boot" (customize-variable 'cider-cljs-boot-repl)]
     ["Form for launching a ClojureScript REPL via Gradle" (customize-variable 'cider-cljs-gradle-repl)])
    "--"
    ["Connection info" cider-display-connection-info
     :active cider-connections]
    ["Rotate default connection" cider-rotate-default-connection
     :active (cdr cider-connections)]
    ["Select any CIDER buffer" cider-selector]
    "--"
    ["Configure CIDER" (customize-group 'cider)]
    "--"
    ["A sip of CIDER" cider-drink-a-sip]
    ["View manual online" cider-view-manual]
    ["View refcard online" cider-view-refcard]
    ["Report a bug" cider-report-bug]
    ["Version info" cider-version]
    "--"
    ["Close ancillary buffers" cider-close-ancillary-buffers
     :active (seq-remove #'null cider-ancillary-buffers)]
    ("nREPL" :active cider-connections
     ["Describe session" cider-describe-nrepl-session]
     ["Close session" cider-close-nrepl-session]
     ["Toggle message logging" nrepl-toggle-message-logging]))
  "Menu for CIDER mode.")

(defconst cider-mode-eval-menu
  '("CIDER Eval" :visible cider-connections
    ["Eval top-level sexp" cider-eval-defun-at-point]
    ["Eval current sexp" cider-eval-sexp-at-point]
    ["Eval last sexp" cider-eval-last-sexp]
    ["Eval selected region" cider-eval-region]
    ["Eval ns form" cider-eval-ns-form]
    "--"
    ["Interrupt evaluation" cider-interrupt]
    "--"
    ["Eval last sexp and insert" cider-eval-print-last-sexp
     :keys "\\[universal-argument] \\[cider-eval-last-sexp]"]
    ["Eval last sexp in popup buffer" cider-pprint-eval-last-sexp]
    ["Eval last sexp and replace" cider-eval-last-sexp-and-replace]
    ["Eval last sexp to REPL" cider-eval-last-sexp-to-repl]
    ["Eval last sexp and pretty-print to REPL" cider-pprint-eval-last-sexp-to-repl]
    ["Insert last sexp in REPL" cider-insert-last-sexp-in-repl]
    ["Eval top-level sexp to comment" cider-eval-defun-to-comment]
    "--"
    ["Load this buffer" cider-load-buffer]
    ["Load another file" cider-load-file]
    ["Recursively load all files in directory" cider-load-all-files]
    ["Load all project files" cider-load-all-project-ns]
    ["Refresh loaded code" cider-refresh]
    ["Run project (-main function)" cider-run])
  "Menu for CIDER mode eval commands.")

(defconst cider-mode-interactions-menu
  `("CIDER Interactions" :visible cider-connections
    ["Complete symbol" complete-symbol]
    "--"
    ("REPL"
     ["Set REPL to this ns" cider-repl-set-ns]
     ["Switch to REPL" cider-switch-to-repl-buffer]
     ["REPL Pretty Print" cider-repl-toggle-pretty-printing
      :style toggle :selected cider-repl-use-pretty-printing]
     ["Clear latest output" cider-find-and-clear-repl-output]
     ["Clear all output" (cider-find-and-clear-repl-output t)
      :keys "\\[universal-argument] \\[cider-find-and-clear-repl-output]"]
     "--"
     ["Configure the REPL" (customize-group 'cider-repl)])
    ,cider-doc-menu
    ("Find (jump to)"
     ["Find definition" cider-find-var]
     ["Find resource" cider-find-resource]
     ["Go back" cider-pop-back])
    ("Macroexpand"
     ["Macroexpand-1" cider-macroexpand-1]
     ["Macroexpand-all" cider-macroexpand-all])
    ,cider-test-menu
    ("Debug"
     ["Inspect" cider-inspect]
     ["Toggle var tracing" cider-toggle-trace-var]
     ["Toggle ns tracing" cider-toggle-trace-ns]
     "--"
     ["Debug top-level form" cider-debug-defun-at-point
      :keys "\\[universal-argument] \\[cider-eval-defun-at-point]"]
     ["List instrumented defs" cider-browse-instrumented-defs]
     "--"
     ["Configure the Debugger" (customize-group 'cider-debug)])
    ("Browse"
     ["Browse namespace" cider-browse-ns]
     ["Browse all namespaces" cider-browse-ns-all]
     ["Browse spec" cider-browse-spec]
     ["Browse all specs" cider-browse-spec-all]
     ["Browse REPL input history" cider-repl-history]
     ["Browse classpath" cider-classpath]
     ["Browse classpath entry" cider-open-classpath-entry])
    ("Misc"
     ["Flush completion cache" cider-completion-flush-caches]))
  "Menu for CIDER interactions.")

(defconst cider-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") 'cider-doc-map)
    (define-key map (kbd "M-.") #'cider-find-var)
    (define-key map (kbd "C-c C-.") #'cider-find-ns)
    (define-key map (kbd "M-,") #'cider-pop-back)
    (define-key map (kbd "C-c M-.") #'cider-find-resource)
    (define-key map (kbd "M-TAB") #'complete-symbol)
    (define-key map (kbd "C-M-x")   #'cider-eval-defun-at-point)
    (define-key map (kbd "C-c C-c") #'cider-eval-defun-at-point)
    (define-key map (kbd "C-x C-e") #'cider-eval-last-sexp)
    (define-key map (kbd "C-c C-e") #'cider-eval-last-sexp)
    (define-key map (kbd "C-c C-v") 'cider-eval-commands-map)
    (define-key map (kbd "C-c M-;") #'cider-eval-defun-to-comment)
    (define-key map (kbd "C-c M-e") #'cider-eval-last-sexp-to-repl)
    (define-key map (kbd "C-c M-p") #'cider-insert-last-sexp-in-repl)
    (define-key map (kbd "C-c C-p") #'cider-pprint-eval-last-sexp)
    (define-key map (kbd "C-c C-f") #'cider-pprint-eval-defun-at-point)
    (define-key map (kbd "C-c M-:") #'cider-read-and-eval)
    (define-key map (kbd "C-c C-u") #'cider-undef)
    (define-key map (kbd "C-c C-m") #'cider-macroexpand-1)
    (define-key map (kbd "C-c M-m") #'cider-macroexpand-all)
    (define-key map (kbd "C-c M-n") #'cider-repl-set-ns)
    (define-key map (kbd "C-c M-i") #'cider-inspect)
    (define-key map (kbd "C-c M-t v") #'cider-toggle-trace-var)
    (define-key map (kbd "C-c M-t n") #'cider-toggle-trace-ns)
    (define-key map (kbd "C-c C-z") #'cider-switch-to-repl-buffer)
    (define-key map (kbd "C-c M-z") #'cider-load-buffer-and-switch-to-repl-buffer)
    (define-key map (kbd "C-c C-o") #'cider-find-and-clear-repl-output)
    (define-key map (kbd "C-c C-k") #'cider-load-buffer)
    (define-key map (kbd "C-c C-l") #'cider-load-file)
    (define-key map (kbd "C-c C-M-l") #'cider-load-all-files)
    (define-key map (kbd "C-c C-b") #'cider-interrupt)
    (define-key map (kbd "C-c ,")   'cider-test-commands-map)
    (define-key map (kbd "C-c C-t") 'cider-test-commands-map)
    (define-key map (kbd "C-c M-s") #'cider-selector)
    (define-key map (kbd "C-c M-r") #'cider-rotate-default-connection)
    (define-key map (kbd "C-c M-d") #'cider-display-connection-info)
    (define-key map (kbd "C-c C-x") #'cider-refresh)
    (define-key map (kbd "C-c C-q") #'cider-quit)
    (dolist (variable '(cider-mode-interactions-menu
                        cider-mode-eval-menu
                        cider-mode-menu))
      (easy-menu-do-define (intern (format "%s-open" variable))
                           map
                           (get variable 'variable-documentation)
                           (cider--menu-add-help-strings (symbol-value variable))))
    map))

;; This menu works as an easy entry-point into CIDER.  Even if cider.el isn't
;; loaded yet, this will be shown in Clojure buffers next to the "Clojure"
;; menu.
;;;###autoload
(eval-after-load 'clojure-mode
  '(easy-menu-define cider-clojure-mode-menu-open clojure-mode-map
     "Menu for Clojure mode.
  This is displayed in `clojure-mode' buffers, if `cider-mode' is not active."
     `("CIDER" :visible (not cider-mode)
       ["Start a REPL" cider-jack-in
        :help "Starts an nREPL server (with lein, boot, or maven) and connects a REPL to it."]
       ["Connect to a REPL" cider-connect
        :help "Connects to a REPL that's already running."]
       ["Start a Clojure REPL, and a ClojureScript REPL" cider-jack-in-clojurescript
        :help "Starts an nREPL server, connects a Clojure REPL to it, and then a ClojureScript REPL.
  Configure `cider-cljs-lein-repl', `cider-cljs-boot-repl' and `cider-cljs-gradle-repl' to change the ClojureScript REPL to use."]
       "--"
       ["View manual online" cider-view-manual])))

;;; Dynamic indentation
(defcustom cider-dynamic-indentation t
  "Whether CIDER should aid Clojure(Script) indentation.
If non-nil, CIDER uses runtime information (such as the \":style/indent\"
metadata) to improve standard `clojure-mode' indentation.
If nil, CIDER won't interfere with `clojure-mode's indentation.

Toggling this variable only takes effect after a file is closed and
re-visited."
  :type 'boolean
  :package-version '(cider . "0.11.0")
  :group 'cider)

(defun cider--get-symbol-indent (symbol-name)
  "Return the indent metadata for SYMBOL-NAME in the current namespace."
  (let* ((ns (cider-current-ns)))
    (if-let ((meta (cider-resolve-var ns symbol-name))
             (indent (or (nrepl-dict-get meta "style/indent")
                         (nrepl-dict-get meta "indent"))))
        (let ((format (format ":indent metadata on ‘%s’ is unreadable! \nERROR: %%s"
                              symbol-name)))
          (with-demoted-errors format
            (cider--deep-vector-to-list (read indent))))
      ;; There's no indent metadata, but there might be a clojure-mode
      ;; indent-spec with fully-qualified namespace.
      (when (string-match cider-resolve--prefix-regexp symbol-name)
        (when-let ((sym (intern-soft (replace-match (save-match-data
                                                      (cider-resolve-alias ns (match-string 1 symbol-name)))
                                                    t t symbol-name 1))))
          (get sym 'clojure-indent-function))))))


;;; Dynamic font locking
(defcustom cider-font-lock-dynamically '(macro core deprecated)
  "Specifies how much dynamic font-locking CIDER should use.
Dynamic font-locking this refers to applying syntax highlighting to vars
defined in the currently active nREPL connection.  This is done in addition
to `clojure-mode's usual (static) font-lock, so even if you set this
variable to nil you'll still see basic syntax highlighting.

The value is a list of symbols, each one indicates a different type of var
that should be font-locked:
   `macro' (default): Any defined macro gets the `font-lock-builtin-face'.
   `function': Any defined function gets the `font-lock-function-face'.
   `var': Any non-local var gets the `font-lock-variable-face'.
   `deprecated' (default): Any deprecated var gets the `cider-deprecated-face'
   face.
   `core' (default): Any symbol from clojure.core (face depends on type).

The value can also be t, which means to font-lock as much as possible."
  :type '(choice (set :tag "Fine-tune font-locking"
                      (const :tag "Any defined macro" macro)
                      (const :tag "Any defined function" function)
                      (const :tag "Any defined var" var)
                      (const :tag "Any defined deprecated" deprecated)
                      (const :tag "Any symbol from clojure.core" core))
                 (const :tag "Font-lock as much as possible" t))
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defcustom cider-font-lock-reader-conditionals t
  "Apply font-locking to unused reader conditional expressions depending on the buffer CIDER connection type."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.15.0"))

(defface cider-deprecated-face
  '((((background light)) :background "light goldenrod")
    (((background dark)) :background "#432"))
  "Face used on deprecated vars."
  :group 'cider)

(defface cider-instrumented-face
  '((((type graphic)) :box (:color "#c00" :line-width -1))
    (t :underline t :background "#800"))
  "Face used to mark code being debugged."
  :group 'cider-debug
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defface cider-traced-face
  '((((type graphic)) :box (:color "cyan" :line-width -1))
    (t :underline t :background "#066"))
  "Face used to mark code being traced."
  :group 'cider
  :package-version '(cider . "0.11.0"))

(defface cider-reader-conditional-face
  '((t (:inherit font-lock-comment-face)))
  "Face used to mark unused reader conditional expressions."
  :group 'cider
  :package-version '(cider . "0.15.0"))

(defconst cider-reader-conditionals-regexp "\\(?:#\\?@?[[:space:]\n]*(\\)"
  "Regexp for matching reader conditionals with a non-capturing group.
Starts from the reader macro characters to the opening parentheses.")

(defvar cider--reader-conditionals-match-data (list nil nil)
  "Reusable list for `match-data` in reader conditionals font lock matchers.")

(defun cider--search-reader-conditionals (limit)
  "Matcher for finding reader conditionals.
Search is done with the given LIMIT."
  (when (and cider-font-lock-reader-conditionals
             (cider-connected-p))
    (when (search-forward-regexp cider-reader-conditionals-regexp limit t)
      (let ((start (match-beginning 0))
            (state (syntax-ppss)))
        (if (or (nth 3 state) (nth 4 state)) ; inside string or comment?
            (cider--search-reader-conditionals limit)
          (when (<= (point) limit)
            (ignore-errors
              (let ((md (match-data nil cider--reader-conditionals-match-data)))
                (setf (nth 0 md) start)
                (setf (nth 1 md) (point))
                (set-match-data md)
                t))))))))

(defun cider--anchored-search-suppressed-forms-internal (limit)
  "Helper function for `cider--anchored-search-suppressed-forms`.
LIMIT is the same as the LIMIT in `cider--anchored-search-suppressed-forms`"
  (let ((types (cider-project-connections-types)))
    (when (= (length types) 1)
      (let ((type (car types))
            (expr (read (current-buffer)))
            (start (save-excursion (backward-sexp) (point))))
        (when (<= (point) limit)
          (forward-sexp)
          (if (not (string-equal (symbol-name expr) (concat ":" type)))
              (ignore-errors
                (cl-assert (<= (point) limit))
                (let ((md (match-data nil cider--reader-conditionals-match-data)))
                  (setf (nth 0 md) start)
                  (setf (nth 1 md) (point))
                  (set-match-data md)
                  t))
            (cider--anchored-search-suppressed-forms-internal limit)))))))

(defun cider--anchored-search-suppressed-forms (limit)
  "Matcher for finding unused reader conditional expressions.
An unused reader conditional expression is an expression for a platform
that does not match the CIDER connection for the buffer.  Search is done
with the given LIMIT."
  (let ((result 'retry))
    (while (and (eq result 'retry) (<= (point) limit))
      (condition-case condition
          (setq result (cider--anchored-search-suppressed-forms-internal limit))
        (invalid-read-syntax
         (setq result 'retry))
        (wrong-type-argument
         (setq result 'retry))
        (scan-error
         (setq result 'retry))
        (end-of-file
         (setq result nil))
        (error
         (setq result nil)
         (display-warning
          '(cider warning)
          (format
           (concat "Caught error during fontification while searching for forms\n"
                   "that are suppressed by reader-conditionals. The error was: %S.")
           condition)))))
    (if (eq result 'retry) (setq result nil))
    result))

(defconst cider--reader-conditionals-font-lock-keywords
  '((cider--search-reader-conditionals
     (cider--anchored-search-suppressed-forms
      (save-excursion
        (let* ((state (syntax-ppss))
               (list-pt (nth 1 state)))
          (when list-pt
            (goto-char list-pt)
            (forward-list)
            (backward-char)
            (point))))
      nil
      (0 'cider-reader-conditional-face t))))
  "Font Lock keywords for unused reader conditionals in CIDER mode.")

(defun cider--unless-local-match (value)
  "Return VALUE, unless `match-string' is a local var."
  (unless (or (get-text-property (point) 'cider-block-dynamic-font-lock)
              (member (match-string 0)
                      (get-text-property (point) 'cider-locals)))
    value))

(defun cider--compile-font-lock-keywords (symbols-plist core-plist)
  "Return a list of font-lock rules for the symbols in SYMBOLS-PLIST and CORE-PLIST."
  (let ((cider-font-lock-dynamically (if (eq cider-font-lock-dynamically t)
                                         '(function var macro core deprecated)
                                       cider-font-lock-dynamically))
        deprecated enlightened
        macros functions vars instrumented traced)
    (cl-labels ((handle-plist
                 (plist)
                 (let ((do-function (memq 'function cider-font-lock-dynamically))
                       (do-var (memq 'var cider-font-lock-dynamically))
                       (do-macro (memq 'macro cider-font-lock-dynamically))
                       (do-deprecated (memq 'deprecated cider-font-lock-dynamically)))
                   (while plist
                     (let ((sym (pop plist))
                           (meta (pop plist)))
                       (pcase (nrepl-dict-get meta "cider/instrumented")
                         (`nil nil)
                         (`"\"breakpoint-if-interesting\""
                          (push sym instrumented))
                         (`"\"light-form\""
                          (push sym enlightened)))
                       ;; The ::traced keywords can be inlined by MrAnderson, so
                       ;; we catch that case too.
                       ;; FIXME: This matches values too, not just keys.
                       (when (seq-find (lambda (k) (and (stringp k)
                                                        (string-match (rx "clojure.tools.trace/traced" eos) k)))
                                       meta)
                         (push sym traced))
                       (when (and do-deprecated (nrepl-dict-get meta "deprecated"))
                         (push sym deprecated))
                       (cond ((and do-macro (nrepl-dict-get meta "macro"))
                              (push sym macros))
                             ((and do-function (or (nrepl-dict-get meta "fn")
                                                   (nrepl-dict-get meta "arglists")))
                              (push sym functions))
                             (do-var (push sym vars))))))))
      (when (memq 'core cider-font-lock-dynamically)
        (let ((cider-font-lock-dynamically '(function var macro core deprecated)))
          (handle-plist core-plist)))
      (handle-plist symbols-plist))
    `(
      ,@(when macros
          `((,(concat (rx (or "(" "#'")) ; Can't take the value of macros.
                      "\\(" (regexp-opt macros 'symbols) "\\)")
             1 (cider--unless-local-match font-lock-keyword-face))))
      ,@(when functions
          `((,(regexp-opt functions 'symbols) 0
             (cider--unless-local-match font-lock-function-name-face))))
      ,@(when vars
          `((,(regexp-opt vars 'symbols) 0
             (cider--unless-local-match font-lock-variable-name-face))))
      ,@(when deprecated
          `((,(regexp-opt deprecated 'symbols) 0
             (cider--unless-local-match 'cider-deprecated-face) append)))
      ,@(when enlightened
          `((,(regexp-opt enlightened 'symbols) 0
             (cider--unless-local-match 'cider-enlightened-face) append)))
      ,@(when instrumented
          `((,(regexp-opt instrumented 'symbols) 0
             (cider--unless-local-match 'cider-instrumented-face) append)))
      ,@(when traced
          `((,(regexp-opt traced 'symbols) 0
             (cider--unless-local-match 'cider-traced-face) append))))))

(defconst cider--static-font-lock-keywords
  (eval-when-compile
    `((,(regexp-opt '("#break" "#dbg" "#light") 'symbols) 0 font-lock-warning-face)))
  "Default expressions to highlight in CIDER mode.")

(defvar-local cider--dynamic-font-lock-keywords nil)

(defun cider-refresh-dynamic-font-lock (&optional ns)
  "Ensure that the current buffer has up-to-date font-lock rules.
NS defaults to `cider-current-ns', and it can also be a dict describing the
namespace itself."
  (interactive)
  (when (and cider-font-lock-dynamically
             font-lock-mode)
    (font-lock-remove-keywords nil cider--dynamic-font-lock-keywords)
    (when-let ((ns (or ns (cider-current-ns)))
               (symbols (cider-resolve-ns-symbols ns)))
      (setq-local cider--dynamic-font-lock-keywords
                  (cider--compile-font-lock-keywords
                   symbols (cider-resolve-ns-symbols (cider-resolve-core-ns))))
      (font-lock-add-keywords nil cider--dynamic-font-lock-keywords 'end))
    (cider--font-lock-flush)))


;;; Detecting local variables
(defun cider--read-locals-from-next-sexp ()
  "Return a list of all locals inside the next logical sexp."
  (save-excursion
    (ignore-errors
      (clojure-forward-logical-sexp 1)
      (let ((out nil)
            (end (point)))
        (forward-sexp -1)
        ;; FIXME: This returns locals found inside the :or clause of a
        ;; destructuring map.
        (while (search-forward-regexp "\\_<[^:&]\\(\\sw\\|\\s_\\)*\\_>" end 'noerror)
          (push (match-string-no-properties 0) out))
        out))))

(defun cider--read-locals-from-bindings-vector ()
  "Return a list of all locals inside the next bindings vector."
  (save-excursion
    (ignore-errors
      (cider-start-of-next-sexp)
      (when (eq (char-after) ?\[)
        (forward-char 1)
        (let ((out nil))
          (setq out (append (cider--read-locals-from-next-sexp) out))
          (while (ignore-errors (clojure-forward-logical-sexp 3)
                                (unless (eobp)
                                  (forward-sexp -1)
                                  t))
            (setq out (append (cider--read-locals-from-next-sexp) out)))
          out)))))

(defun cider--read-locals-from-arglist ()
  "Return a list of all locals in current form's arglist(s)."
  (let ((out nil))
    (save-excursion
      (ignore-errors
        (cider-start-of-next-sexp)
        ;; Named fn
        (when (looking-at-p "\\s_\\|\\sw")
          (cider-start-of-next-sexp 1))
        ;; Docstring
        (when (eq (char-after) ?\")
          (cider-start-of-next-sexp 1))
        ;; Attribute map
        (when (eq (char-after) ?{)
          (cider-start-of-next-sexp 1))
        ;; The arglist
        (pcase (char-after)
          (?\[ (setq out (cider--read-locals-from-next-sexp)))
          ;; FIXME: This returns false positives. It takes all arglists of a
          ;; function and returns all args it finds. The logic should be changed
          ;; so that each arglist applies to its own scope.
          (?\( (ignore-errors
                 (while (eq (char-after) ?\()
                   (save-excursion
                     (forward-char 1)
                     (setq out (append (cider--read-locals-from-next-sexp) out)))
                   (cider-start-of-next-sexp 1)))))))
    out))

(defun cider--parse-and-apply-locals (end &optional outer-locals)
  "Figure out local variables between point and END.
A list of these variables is set as the `cider-locals' text property over
the code where they are in scope.
Optional argument OUTER-LOCALS is used to specify local variables defined
before point."
  (while (search-forward-regexp "(\\(ns\\_>\\|def\\|fn\\|for\\b\\|loop\\b\\|with-\\|do[a-z]+\\|\\([a-z]+-\\)?let\\b\\)"
                                end 'noerror)
    (goto-char (match-beginning 0))
    (let ((sym (match-string 1))
          (sexp-end (save-excursion
                      (or (ignore-errors (forward-sexp 1)
                                         (point))
                          end))))
      ;; #1324: Don't do dynamic font-lock in `ns' forms, they are special
      ;; macros where nothing is evaluated, so we'd get a lot of false
      ;; positives.
      (if (equal sym "ns")
          (add-text-properties (point) sexp-end '(cider-block-dynamic-font-lock t))
        (forward-char 1)
        (forward-sexp 1)
        (let ((locals (append outer-locals
                              (pcase sym
                                ((or "fn" "def" "") (cider--read-locals-from-arglist))
                                (_ (cider--read-locals-from-bindings-vector))))))
          (add-text-properties (point) sexp-end (list 'cider-locals locals))
          (clojure-forward-logical-sexp 1)
          (cider--parse-and-apply-locals sexp-end locals)))
      (goto-char sexp-end))))

(defun cider--update-locals-for-region (beg end)
  "Update the `cider-locals' text property for region from BEG to END."
  (save-excursion
    (goto-char beg)
    ;; If the inside of a `ns' form changed, reparse it from the start.
    (when (and (not (bobp))
               (get-text-property (1- (point)) 'cider-block-dynamic-font-lock))
      (ignore-errors (beginning-of-defun)))
    (save-excursion
      ;; Move up until we reach a sexp that encloses the entire region (or
      ;; a top-level sexp), and set that as the new BEG.
      (goto-char end)
      (while (and (or (> (point) beg)
                      (not (eq (char-after) ?\()))
                  (condition-case nil
                      (progn (backward-up-list) t)
                    (scan-error nil))))
      (setq beg (min beg (point)))
      ;; If there are locals above the current sexp, reapply them to the
      ;; current sexp.
      (let ((locals-above (when (> beg (point-min))
                            (get-text-property (1- beg) 'cider-locals))))
        (condition-case nil
            (clojure-forward-logical-sexp 1)
          (error (goto-char end)))
        (add-text-properties beg (point) `(cider-locals ,locals-above))
        ;; Extend the region being font-locked to include whole sexps.
        (setq end (max end (point)))
        (goto-char beg)
        (ignore-errors
          (cider--parse-and-apply-locals end locals-above))))))

(defun cider--docview-as-string (sym info)
  "Return a string of what would be displayed by `cider-docview-render'.
SYM and INFO is passed to `cider-docview-render'"
  (with-temp-buffer
    (cider-docview-render (current-buffer) sym info)
    (goto-char (point-max))
    (forward-line -1)
    (replace-regexp-in-string
     "[`']" "\\\\=\\&"
     (buffer-substring-no-properties (point-min) (1- (point))))))

(defcustom cider-use-tooltips t
  "If non-nil, CIDER displays mouse-over tooltips."
  :group 'cider
  :type 'boolean
  :package-version '(cider "0.12.0"))

(defvar cider--debug-mode-response)
(defvar cider--debug-mode)

(defun cider--help-echo (_ obj pos)
  "Return the help-echo string for OBJ at POS.
See \(info \"(elisp) Special Properties\")"
  (while-no-input
    (when (and (bufferp obj) (cider-connected-p)
               cider-use-tooltips (not help-at-pt-display-when-idle))
      (with-current-buffer obj
        (ignore-errors
          (save-excursion
            (goto-char pos)
            (when-let ((sym (cider-symbol-at-point)))
              (if (member sym (get-text-property (point) 'cider-locals))
                  (concat (format "`%s' is a local" sym)
                          (when cider--debug-mode
                            (let* ((locals (nrepl-dict-get cider--debug-mode-response "locals"))
                                   (local-val (cadr (assoc sym locals))))
                              (format " with value:\n%s" local-val))))
                (let* ((info (cider-sync-request:info sym))
                       (candidates (nrepl-dict-get info "candidates")))
                  (if candidates
                      (concat "There were ambiguities resolving this symbol:\n\n"
                              (mapconcat (lambda (x) (cider--docview-as-string sym x))
                                         candidates
                                         (concat "\n\n" (make-string 60 ?-) "\n\n")))
                    (cider--docview-as-string sym info)))))))))))

(defun cider--wrap-fontify-locals (func)
  "Return a function that will call FUNC after parsing local variables.
The local variables are stored in a list under the `cider-locals' text
property."
  (lambda (beg end &rest rest)
    (with-silent-modifications
      (remove-text-properties beg end '(cider-locals nil cider-block-dynamic-font-lock nil))
      (add-text-properties beg end '(help-echo cider--help-echo))
      (when cider-font-lock-dynamically
        (cider--update-locals-for-region beg end)))
    (apply func beg end rest)))


;;; Minor-mode definition
(defvar x-gtk-use-system-tooltips)

;;;###autoload
(define-minor-mode cider-mode
  "Minor mode for REPL interaction from a Clojure buffer.

\\{cider-mode-map}"
  nil
  cider-mode-line
  cider-mode-map
  (if cider-mode
      (progn
        (cider-eldoc-setup)
        (make-local-variable 'completion-at-point-functions)
        (add-to-list 'completion-at-point-functions
                     #'cider-complete-at-point)
        (font-lock-add-keywords nil cider--static-font-lock-keywords)
        (cider-refresh-dynamic-font-lock)
        (font-lock-add-keywords nil cider--reader-conditionals-font-lock-keywords)
        ;; `font-lock-mode' might get enabled after `cider-mode'.
        (add-hook 'font-lock-mode-hook #'cider-refresh-dynamic-font-lock nil 'local)
        (setq-local font-lock-fontify-region-function
                    (cider--wrap-fontify-locals font-lock-fontify-region-function))
        ;; GTK tooltips look bad, and we have no control over the face.
        (setq-local x-gtk-use-system-tooltips nil)
        ;; `tooltip' has variable-width by default, which looks terrible.
        (set-face-attribute 'tooltip nil :inherit 'unspecified)
        (when cider-dynamic-indentation
          (setq-local clojure-get-indent-function #'cider--get-symbol-indent))
        (setq-local clojure-expected-ns-function #'cider-expected-ns)
        (setq next-error-function #'cider-jump-to-compilation-error))
    (mapc #'kill-local-variable '(completion-at-point-functions
                                  next-error-function
                                  x-gtk-use-system-tooltips
                                  font-lock-fontify-region-function
                                  clojure-get-indent-function))
    (remove-hook 'font-lock-mode-hook #'cider-refresh-dynamic-font-lock 'local)
    (font-lock-add-keywords nil cider--reader-conditionals-font-lock-keywords)
    (font-lock-remove-keywords nil cider--dynamic-font-lock-keywords)
    (font-lock-remove-keywords nil cider--static-font-lock-keywords)
    (cider--font-lock-flush)))

(defun cider-set-buffer-ns (ns)
  "Set this buffer's namespace to NS and refresh font-locking."
  (setq-local cider-buffer-ns ns)
  (when (or cider-mode (derived-mode-p 'cider-repl-mode))
    (cider-refresh-dynamic-font-lock ns)))

(provide 'cider-mode)

;;; cider-mode.el ends here
