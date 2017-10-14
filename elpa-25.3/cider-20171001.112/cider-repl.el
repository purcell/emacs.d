;;; cider-repl.el --- REPL interactions -*- lexical-binding: t -*-

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

;; REPL interactions.

;;; Code:

(require 'cider-client)
(require 'cider-doc)
(require 'cider-test)
(require 'cider-eldoc) ; for cider-eldoc-setup
(require 'cider-common)
(require 'subr-x)
(require 'cider-compat)
(require 'cider-util)
(require 'cider-resolve)

(require 'clojure-mode)
(require 'easymenu)
(require 'cl-lib)

(eval-when-compile
  (defvar paredit-version)
  (defvar paredit-space-for-delimiter-predicates))


(defgroup cider-repl nil
  "Interaction with the REPL."
  :prefix "cider-repl-"
  :group 'cider)

(defface cider-repl-prompt-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for the prompt in the REPL buffer."
  :group 'cider-repl)

(defface cider-repl-stdout-face
  '((t (:inherit font-lock-string-face)))
  "Face for STDOUT output in the REPL buffer."
  :group 'cider-repl)

(defface cider-repl-stderr-face
  '((t (:inherit font-lock-warning-face)))
  "Face for STDERR output in the REPL buffer."
  :group 'cider-repl
  :package-version '(cider . "0.6.0"))

(defface cider-repl-input-face
  '((t (:bold t)))
  "Face for previous input in the REPL buffer."
  :group 'cider-repl)

(defface cider-repl-result-face
  '((t ()))
  "Face for the result of an evaluation in the REPL buffer."
  :group 'cider-repl)

(defcustom cider-repl-pop-to-buffer-on-connect t
  "Controls whether to pop to the REPL buffer on connect.

When set to nil the buffer will only be created, and not displayed.  When
set to `display-only' the buffer will be displayed, but it will not become
focused.  Otherwise the buffer is displayed and focused."
  :type '(choice (const :tag "Create the buffer, but don't display it" nil)
                 (const :tag "Create and display the buffer, but don't focus it"
                   display-only)
                 (const :tag "Create, display, and focus the buffer" t))
  :group 'cider-repl)

(defcustom cider-repl-display-in-current-window nil
  "Controls whether the REPL buffer is displayed in the current window."
  :type 'boolean
  :group 'cider-repl)

(defcustom cider-repl-scroll-on-output t
  "Controls whether the REPL buffer auto-scrolls on new output.

When set to t (the default), if the REPL buffer contains more lines than the
size of the window, the buffer is automatically re-centered upon completion
of evaluating an expression, so that the bottom line of output is on the
bottom line of the window.

If this is set to nil, no re-centering takes place."
  :type 'boolean
  :group 'cider-repl
  :package-version '(cider . "0.11.0"))

(defcustom cider-repl-use-pretty-printing nil
  "Control whether the results in REPL are pretty-printed or not.
The `cider-toggle-pretty-printing' command can be used to interactively
change the setting's value."
  :type 'boolean
  :group 'cider-repl)

(defcustom cider-repl-pretty-print-width nil
  "Control the width of pretty printing on the REPL.
This sets the wrap point for pretty printing on the repl.  If nil, it
defaults to the variable `fill-column'."
  :type '(restricted-sexp  :match-alternatives
                          (integerp 'nil))
  :group 'cider-repl
  :package-version '(cider . "0.15.0"))

(defcustom cider-repl-use-clojure-font-lock t
  "Non-nil means to use Clojure mode font-locking for input and result.
Nil means that `cider-repl-input-face' and `cider-repl-result-face'
will be used."
  :type 'boolean
  :group 'cider-repl
  :package-version '(cider . "0.10.0"))

(defcustom cider-repl-result-prefix ""
  "The prefix displayed in the REPL before a result value."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.5.0"))

(defcustom cider-repl-tab-command 'cider-repl-indent-and-complete-symbol
  "Select the command to be invoked by the TAB key.
The default option is `cider-repl-indent-and-complete-symbol'.  If
you'd like to use the default Emacs behavior use
`indent-for-tab-command'."
  :type 'symbol
  :group 'cider-repl)

(defcustom cider-repl-display-help-banner t
  "When non-nil a bit of help text will be displayed on REPL start."
  :type 'boolean
  :group 'cider-repl
  :package-version '(cider . "0.11.0"))


;;;; REPL buffer local variables
(defvar-local cider-repl-input-start-mark nil)

(defvar-local cider-repl-prompt-start-mark nil)

(defvar-local cider-repl-old-input-counter 0
  "Counter used to generate unique `cider-old-input' properties.
This property value must be unique to avoid having adjacent inputs be
joined together.")

(defvar-local cider-repl-input-history '()
  "History list of strings read from the REPL buffer.")

(defvar-local cider-repl-input-history-items-added 0
  "Variable counting the items added in the current session.")

(defvar-local cider-repl-output-start nil
  "Marker for the start of output.
Currently its only purpose is to facilitate `cider-repl-clear-buffer'.")

(defvar-local cider-repl-output-end nil
  "Marker for the end of output.
Currently its only purpose is to facilitate `cider-repl-clear-buffer'.")

(defun cider-repl-tab ()
  "Invoked on TAB keystrokes in `cider-repl-mode' buffers."
  (interactive)
  (funcall cider-repl-tab-command))

(defun cider-repl-reset-markers ()
  "Reset all REPL markers."
  (dolist (markname '(cider-repl-output-start
                      cider-repl-output-end
                      cider-repl-prompt-start-mark
                      cider-repl-input-start-mark))
    (set markname (make-marker))
    (set-marker (symbol-value markname) (point))))


;;; REPL init

(defvar-local cider-repl-ns-cache nil
  "A dict holding information about all currently loaded namespaces.
This cache is stored in the connection buffer.  Other buffer's access it
via `cider-current-connection'.")

(defvar cider-mode)
(declare-function cider-refresh-dynamic-font-lock "cider-mode")

(defun cider-repl--state-handler (response)
  "Handle server state contained in RESPONSE."
  (with-demoted-errors "Error in `cider-repl--state-handler': %s"
    (when (member "state" (nrepl-dict-get response "status"))
      (nrepl-dbind-response response (repl-type changed-namespaces)
        (when repl-type
          (setq cider-repl-type repl-type))
        (unless (nrepl-dict-empty-p changed-namespaces)
          (setq cider-repl-ns-cache (nrepl-dict-merge cider-repl-ns-cache changed-namespaces))
          (dolist (b (buffer-list))
            (with-current-buffer b
              ;; Metadata changed, so signatures may have changed too.
              (setq cider-eldoc-last-symbol nil)
              (when (or cider-mode (derived-mode-p 'cider-repl-mode))
                (when-let ((ns-dict (or (nrepl-dict-get changed-namespaces (cider-current-ns))
                                        (let ((ns-dict (cider-resolve--get-in (cider-current-ns))))
                                          (when (seq-find (lambda (ns) (nrepl-dict-get changed-namespaces ns))
                                                          (nrepl-dict-get ns-dict "aliases"))
                                            ns-dict)))))
                  (cider-refresh-dynamic-font-lock ns-dict))))))))))

(declare-function cider-default-err-handler "cider-interaction")

(defun cider-repl-create (endpoint)
  "Create a REPL buffer and install `cider-repl-mode'.
ENDPOINT is a plist as returned by `nrepl-connect'."
  ;; Connection might not have been set as yet. Please don't send requests here.
  (let* ((reuse-buff (not (eq 'new nrepl-use-this-as-repl-buffer)))
         (buff-name (nrepl-make-buffer-name nrepl-repl-buffer-name-template nil
                                            (plist-get endpoint :host)
                                            (plist-get endpoint :port)
                                            reuse-buff)))
    ;; when reusing, rename the buffer accordingly
    (when (and reuse-buff
               (not (equal buff-name nrepl-use-this-as-repl-buffer)))
      ;; uniquify as it might be Nth connection to the same endpoint
      (setq buff-name (generate-new-buffer-name buff-name))
      (with-current-buffer nrepl-use-this-as-repl-buffer
        (rename-buffer buff-name)))
    (with-current-buffer (get-buffer-create buff-name)
      (unless (derived-mode-p 'cider-repl-mode)
        (cider-repl-mode)
        (setq cider-repl-type "clj"))
      (setq nrepl-err-handler #'cider-default-err-handler)
      (cider-repl-reset-markers)
      (add-hook 'nrepl-response-handler-functions #'cider-repl--state-handler nil 'local)
      (add-hook 'nrepl-connected-hook 'cider--connected-handler nil 'local)
      (add-hook 'nrepl-disconnected-hook 'cider--disconnected-handler nil 'local)
      (current-buffer))))

(declare-function cider-set-buffer-ns "cider-mode")
(defun cider-repl-require-repl-utils-and-set-ns (buffer)
  "Require standard REPL util functions and set the ns of the REPL's BUFFER.
Namespace is \"user\" by default, but can be overridden in apps like
lein (:init-ns).  Both of these operations need to be done as a sync
request at the beginning of the session.  Bundling them together for
efficiency."
  ;; we don't want to get a timeout during init
  (let ((nrepl-sync-request-timeout nil))
    (with-current-buffer buffer
      (let* ((command "(do (when (clojure.core/resolve 'clojure.main/repl-requires)
                            (clojure.core/map clojure.core/require clojure.main/repl-requires))
                           (str *ns*))")
             (response (nrepl-send-sync-request
                        (lax-plist-put (nrepl--eval-request command)
                                       "inhibit-cider-middleware" "true")
                        (cider-current-connection)))
             (initial-ns (or (read (nrepl-dict-get response "value"))
                             "user")))
        (cider-set-buffer-ns initial-ns)))))

(defvar cider-current-clojure-buffer nil
  "This variable holds current buffer temporarily when connecting to a REPL.
It is set to current buffer when `cider' or `cider-jack-in' is called.
After the REPL buffer is created, the value of this variable is used
to call `cider-remember-clojure-buffer'.")

(declare-function cider-remember-clojure-buffer "cider-mode")

(defun cider-repl-init (buffer &optional no-banner)
  "Initialize the REPL in BUFFER.
BUFFER must be a REPL buffer with `cider-repl-mode' and a running
client process connection.  Unless NO-BANNER is non-nil, insert a banner."
  (when cider-repl-display-in-current-window
    (add-to-list 'same-window-buffer-names (buffer-name buffer)))
  (pcase cider-repl-pop-to-buffer-on-connect
    (`display-only (display-buffer buffer))
    ((pred identity) (pop-to-buffer buffer)))
  (cider-repl-require-repl-utils-and-set-ns buffer)
  (unless no-banner
    (cider-repl--insert-banner-and-prompt buffer))
  (cider-remember-clojure-buffer cider-current-clojure-buffer)
  buffer)

(defun cider-repl--insert-banner-and-prompt (buffer)
  "Insert REPL banner and REPL prompt in BUFFER."
  (with-current-buffer buffer
    (when (zerop (buffer-size))
      (insert (propertize (cider-repl--banner) 'font-lock-face 'font-lock-comment-face))
      (when cider-repl-display-help-banner
        (insert (propertize (cider-repl--help-banner) 'font-lock-face 'font-lock-comment-face))))
    (goto-char (point-max))
    (cider-repl--mark-output-start)
    (cider-repl--mark-input-start)
    (cider-repl--insert-prompt cider-buffer-ns)))

(defun cider-repl--banner ()
  "Generate the welcome REPL buffer banner."
  (let ((host (cider--connection-host (current-buffer)))
        (port (cider--connection-port (current-buffer))))
    (format ";; Connected to nREPL server - nrepl://%s:%s
;; CIDER %s, nREPL %s
;; Clojure %s, Java %s
;;     Docs: (doc function-name)
;;           (find-doc part-of-name)
;;   Source: (source function-name)
;;  Javadoc: (javadoc java-object-or-class)
;;     Exit: <C-c C-q>
;;  Results: Stored in vars *1, *2, *3, an exception in *e;"
            host
            port
            (cider--version)
            (cider--nrepl-version)
            (cider--clojure-version)
            (cider--java-version))))

(defun cider-repl--help-banner ()
  "Generate the help banner."
  (substitute-command-keys
   "\n;; ======================================================================
;; If you're new to CIDER it is highly recommended to go through its
;; manual first. Type <M-x cider-view-manual> to view it.
;; In case you're seeing any warnings you should consult the manual's
;; \"Troubleshooting\" section.
;;
;; Here are few tips to get you started:
;;
;; * Press <\\[describe-mode]> to see a list of the keybindings available (this
;;   will work in every Emacs buffer)
;; * Press <\\[cider-repl-handle-shortcut]> to quickly invoke some REPL command
;; * Press <\\[cider-switch-to-last-clojure-buffer]> to switch between the REPL and a Clojure file
;; * Press <\\[cider-find-var]> to jump to the source of something (e.g. a var, a
;;   Java method)
;; * Press <\\[cider-doc]> to view the documentation for something (e.g.
;;   a var, a Java method)
;; * Enable `eldoc-mode' to display function & method signatures in the minibuffer.
;; * Print CIDER's refcard and keep it close to your keyboard.
;;
;; CIDER is super customizable - try <M-x customize-group cider> to
;; get a feel for this. If you're thirsty for knowledge you should try
;; <M-x cider-drink-a-sip>.
;;
;; If you think you've encountered a bug (or have some suggestions for
;; improvements) use <M-x cider-report-bug> to report it.
;;
;; Above all else - don't panic! In case of an emergency - procure
;; some (hard) cider and enjoy it responsibly!
;;
;; You can remove this message with the `cider-repl-clear-help-banner' command.
;; You can disable it from appearing on start by setting
;; `cider-repl-display-help-banner' to nil.
;; ======================================================================
"))


;;; REPL interaction

(defun cider-repl--in-input-area-p ()
  "Return t if in input area."
  (<= cider-repl-input-start-mark (point)))

(defun cider-repl--current-input (&optional until-point-p)
  "Return the current input as string.
The input is the region from after the last prompt to the end of
buffer.  If UNTIL-POINT-P is non-nil, the input is until the current
point."
  (buffer-substring-no-properties cider-repl-input-start-mark
                                  (if until-point-p
                                      (point)
                                    (point-max))))

(defun cider-repl-previous-prompt ()
  "Move backward to the previous prompt."
  (interactive)
  (cider-repl--find-prompt t))

(defun cider-repl-next-prompt ()
  "Move forward to the next prompt."
  (interactive)
  (cider-repl--find-prompt))

(defun cider-repl--find-prompt (&optional backward)
  "Find the next prompt.
If BACKWARD is non-nil look backward."
  (let ((origin (point))
        (cider-repl-prompt-property 'field))
    (while (progn
             (cider-search-property-change cider-repl-prompt-property backward)
             (not (or (cider-end-of-proprange-p cider-repl-prompt-property) (bobp) (eobp)))))
    (unless (cider-end-of-proprange-p cider-repl-prompt-property)
      (goto-char origin))))

(defun cider-search-property-change (prop &optional backward)
  "Search forward for a property change to PROP.
If BACKWARD is non-nil search backward."
  (cond (backward
         (goto-char (previous-single-char-property-change (point) prop)))
        (t
         (goto-char (next-single-char-property-change (point) prop)))))

(defun cider-end-of-proprange-p (property)
  "Return t if at the the end of a property range for PROPERTY."
  (and (get-char-property (max (point-min) (1- (point))) property)
       (not (get-char-property (point) property))))

(defun cider-repl--mark-input-start ()
  "Mark the input start."
  (set-marker cider-repl-input-start-mark (point) (current-buffer)))

(defun cider-repl--mark-output-start ()
  "Mark the output start."
  (set-marker cider-repl-output-start (point))
  (set-marker cider-repl-output-end (point)))

(defun cider-repl-mode-beginning-of-defun (&optional arg)
  "Move to the beginning of defun.
If given a negative value of ARG, move to the end of defun."
  (if (and arg (< arg 0))
      (cider-repl-mode-end-of-defun (- arg))
    (dotimes (_ (or arg 1))
      (cider-repl-previous-prompt))))

(defun cider-repl-mode-end-of-defun (&optional arg)
  "Move to the end of defun.
If given a negative value of ARG, move to the beginning of defun."
  (if (and arg (< arg 0))
      (cider-repl-mode-beginning-of-defun (- arg))
    (dotimes (_ (or arg 1))
      (cider-repl-next-prompt))))

(defun cider-repl-beginning-of-defun ()
  "Move to beginning of defun."
  (interactive)
  ;; We call `beginning-of-defun' if we're at the start of a prompt
  ;; already, to trigger `cider-repl-mode-beginning-of-defun' by means
  ;; of the locally bound `beginning-of-defun-function', in order to
  ;; jump to the start of the previous prompt.
  (if (and (not (cider-repl--at-prompt-start-p))
           (cider-repl--in-input-area-p))
      (goto-char cider-repl-input-start-mark)
    (beginning-of-defun)))

(defun cider-repl-end-of-defun ()
  "Move to end of defun."
  (interactive)
  ;; C.f. `cider-repl-beginning-of-defun'
  (if (and (not (= (point) (point-max)))
           (cider-repl--in-input-area-p))
      (goto-char (point-max))
    (end-of-defun)))

(defun cider-repl-bol-mark ()
  "Set the mark and go to the beginning of line or the prompt."
  (interactive)
  (unless mark-active
    (set-mark (point)))
  (move-beginning-of-line 1))

(defun cider-repl--at-prompt-start-p ()
  "Return t if point is at the start of prompt.
This will not work on non-current prompts."
  (= (point) cider-repl-input-start-mark))

(defun cider-repl--show-maximum-output ()
  "Put the end of the buffer at the bottom of the window."
  (when (and cider-repl-scroll-on-output (eobp))
    (let ((win (get-buffer-window (current-buffer) t)))
      (when win
        (with-selected-window win
          (set-window-point win (point-max))
          (recenter -1))))))

(defmacro cider-save-marker (marker &rest body)
  "Save MARKER and execute BODY."
  (declare (debug t))
  (let ((pos (make-symbol "pos")))
    `(let ((,pos (marker-position ,marker)))
       (prog1 (progn . ,body)
         (set-marker ,marker ,pos)))))

(put 'cider-save-marker 'lisp-indent-function 1)

(defun cider-repl-prompt-default (namespace)
  "Return a prompt string that mentions NAMESPACE."
  (format "%s> " namespace))

(defun cider-repl-prompt-abbreviated (namespace)
  "Return a prompt string that abbreviates NAMESPACE."
  (format "%s> " (cider-abbreviate-ns namespace)))

(defun cider-repl-prompt-lastname (namespace)
  "Return a prompt string with the last name in NAMESPACE."
  (format "%s> " (cider-last-ns-segment namespace)))

(defcustom cider-repl-prompt-function #'cider-repl-prompt-default
  "A function that returns a prompt string.
Takes one argument, a namespace name.
For convenience, three functions are already provided for this purpose:
`cider-repl-prompt-lastname', `cider-repl-prompt-abbreviated', and
`cider-repl-prompt-default'"
  :type '(choice (const :tag "Full namespace" cider-repl-prompt-default)
                 (const :tag "Abbreviated namespace" cider-repl-prompt-abbreviated)
                 (const :tag "Last name in namespace" cider-repl-prompt-lastname)
                 (function :tag "Custom function"))
  :group 'cider-repl
  :package-version '(cider . "0.9.0"))

(defun cider-repl--insert-prompt (namespace)
  "Insert the prompt (before markers!), taking into account NAMESPACE.
Set point after the prompt.
Return the position of the prompt beginning."
  (goto-char cider-repl-input-start-mark)
  (cider-save-marker cider-repl-output-start
    (cider-save-marker cider-repl-output-end
      (unless (bolp) (insert-before-markers "\n"))
      (let ((prompt-start (point))
            (prompt (funcall cider-repl-prompt-function namespace)))
        (cider-propertize-region
            '(font-lock-face cider-repl-prompt-face read-only t intangible t
                             field cider-repl-prompt
                             rear-nonsticky (field read-only font-lock-face intangible))
          (insert-before-markers prompt))
        (set-marker cider-repl-prompt-start-mark prompt-start)
        prompt-start))))

(defun cider-repl--flush-ansi-color-context ()
  "Flush ansi color context after printing.
When there is a possible unfinished ansi control sequence,
 `ansi-color-context` maintains this list."
  (when (and ansi-color-context (stringp (cadr ansi-color-context)))
    (insert-before-markers (cadr ansi-color-context))
    (setq ansi-color-context nil)))

(defvar-local cider-repl--ns-forms-plist nil
  "Plist holding ns->ns-form mappings within each connection.")

(defun cider-repl--ns-form-changed-p (ns-form connection)
  "Return non-nil if NS-FORM for CONNECTION changed since last eval."
  (when-let ((ns (cider-ns-from-form ns-form)))
    (not (string= ns-form
                  (lax-plist-get
                   (buffer-local-value 'cider-repl--ns-forms-plist connection)
                   ns)))))

(defvar cider-repl--root-ns-highlight-template "\\_<\\(%s\\)[^$/: \t\n()]+"
  "Regexp used to highlight root ns in REPL buffers.")

(defvar-local cider-repl--root-ns-regexp nil
  "Cache of root ns regexp in REPLs.")

(defvar-local cider-repl--ns-roots nil
  "List holding all past root namespaces seen during interactive eval.")

(defun cider-repl--cache-ns-form (ns-form connection)
  "Given NS-FORM cache root ns in CONNECTION."
  (with-current-buffer connection
    (when-let ((ns (cider-ns-from-form ns-form)))
      ;; cache ns-form
      (setq cider-repl--ns-forms-plist
            (lax-plist-put cider-repl--ns-forms-plist ns ns-form))
      ;; cache ns roots regexp
      (when (string-match "\\([^.]+\\)" ns)
        (let ((root (match-string-no-properties 1 ns)))
          (unless (member root cider-repl--ns-roots)
            (push root cider-repl--ns-roots)
            (let ((roots (mapconcat
                          ;; Replace _ or - with regexp patter to accommodate "raw" namespaces
                          (lambda (r) (replace-regexp-in-string "[_-]+" "[_-]+" r))
                          cider-repl--ns-roots "\\|")))
              (setq cider-repl--root-ns-regexp
                    (format cider-repl--root-ns-highlight-template roots)))))))))

(defvar cider-repl-spec-keywords-regexp
  (concat
   (regexp-opt '("In:" " val:"
                 " at:" "fails at:"
                 " spec:" "fails spec:"
                 " predicate:" "fails predicate:"))
   "\\|^"
   (regexp-opt '(":clojure.spec.alpha/spec"
                 ":clojure.spec.alpha/value")
               "\\("))
  "Regexp matching clojure.spec `explain` keywords.")

(defun cider-repl-highlight-spec-keywords (string)
  "Highlight clojure.spec `explain` keywords in STRING.
Foreground of `clojure-keyword-face' is used for highlight."
  (cider-add-face cider-repl-spec-keywords-regexp
                  'clojure-keyword-face t nil string)
  string)

(defun cider-repl-highlight-current-project (string)
  "Fontify project's root namespace to make stacktraces more readable.
Foreground of `cider-stacktrace-ns-face' is used to propertize matched
namespaces.  STRING is REPL's output."
  (cider-add-face cider-repl--root-ns-regexp 'cider-stacktrace-ns-face
                  t nil string)
  string)

(defun cider-repl-add-locref-help-echo (string)
  "Set help-echo property of STRING to `cider-locref-help-echo'."
  (put-text-property 0 (length string) 'help-echo 'cider-locref-help-echo string)
  string)

(defvar cider-repl-preoutput-hook '(ansi-color-apply
                                    cider-repl-highlight-current-project
                                    cider-repl-highlight-spec-keywords
                                    cider-repl-add-locref-help-echo)
  "Hook run on output string before it is inserted into the REPL buffer.
Each functions takes a string and must return a modified string.  Also see
`cider-run-chained-hook'.")

(defun cider-repl--emit-output-at-pos (buffer string output-face position &optional bol)
  "Using BUFFER, insert STRING (applying to it OUTPUT-FACE) at POSITION.
If BOL is non-nil insert at the beginning of line.  Run
`cider-repl-preoutput-hook' on STRING."
  (with-current-buffer buffer
    (save-excursion
      (cider-save-marker cider-repl-output-start
        (cider-save-marker cider-repl-output-end
          (goto-char position)
          ;; TODO: Review the need for bol
          (when (and bol (not (bolp))) (insert-before-markers "\n"))
          (setq string (propertize string
                                   'font-lock-face output-face
                                   'rear-nonsticky '(font-lock-face)))
          (setq string (cider-run-chained-hook 'cider-repl-preoutput-hook string))
          (insert-before-markers string)
          (cider-repl--flush-ansi-color-context)
          (when (and (= (point) cider-repl-prompt-start-mark)
                     (not (bolp)))
            (insert-before-markers "\n")
            (set-marker cider-repl-output-end (1- (point)))))))
    (cider-repl--show-maximum-output)))

(defun cider-repl--emit-interactive-output (string face)
  "Emit STRING as interactive output using FACE."
  (with-current-buffer (cider-current-repl-buffer)
    (let ((pos (cider-repl--end-of-line-before-input-start))
          (string (replace-regexp-in-string "\n\\'" "" string)))
      (cider-repl--emit-output-at-pos (current-buffer) string face pos t))))

(defun cider-repl-emit-interactive-stdout (string)
  "Emit STRING as interactive output."
  (cider-repl--emit-interactive-output string 'cider-repl-stdout-face))

(defun cider-repl-emit-interactive-stderr (string)
  "Emit STRING as interactive err output."
  (cider-repl--emit-interactive-output string 'cider-repl-stderr-face))

(defun cider-repl-manual-warning (section-id format &rest args)
  "Emit a warning to the REPL and link to the online manual.
SECTION-ID is the section to link to.  The link is added on the last line.
FORMAT is a format string to compile with ARGS and display on the REPL."
  (let ((message (apply #'format format args)))
    (cider-repl-emit-interactive-stderr
     (concat "WARNING: " message "\n         "
             (cider--manual-button "More information" section-id)
             "."))))

(defun cider-repl--emit-output (buffer string face &optional bol)
  "Using BUFFER, emit STRING font-locked with FACE.
If BOL is non-nil, emit at the beginning of the line."
  (with-current-buffer buffer
    (cider-repl--emit-output-at-pos buffer string face cider-repl-input-start-mark bol)))

(defun cider-repl-emit-stdout (buffer string)
  "Using BUFFER, emit STRING as standard output."
  (cider-repl--emit-output buffer string 'cider-repl-stdout-face))

(defun cider-repl-emit-stderr (buffer string)
  "Using BUFFER, emit STRING as error output."
  (cider-repl--emit-output buffer string 'cider-repl-stderr-face))

(defun cider-repl-emit-prompt (buffer)
  "Emit the REPL prompt into BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (cider-save-marker cider-repl-output-start
        (cider-save-marker cider-repl-output-end
          (cider-repl--insert-prompt cider-buffer-ns))))
    (cider-repl--show-maximum-output)))

(defun cider-repl-emit-result (buffer string &optional bol)
  "Emit into BUFFER the result STRING and mark it as an evaluation result.
If BOL is non-nil insert at the beginning of the line."
  (with-current-buffer buffer
    (save-excursion
      (cider-save-marker cider-repl-output-start
        (cider-save-marker cider-repl-output-end
          (goto-char cider-repl-input-start-mark)
          (when (and bol (not (bolp)))
            (insert-before-markers "\n"))
          (insert-before-markers (propertize cider-repl-result-prefix 'font-lock-face 'font-lock-comment-face))
          (if cider-repl-use-clojure-font-lock
              (insert-before-markers (cider-font-lock-as-clojure string))
            (cider-propertize-region
                '(font-lock-face cider-repl-result-face rear-nonsticky (font-lock-face))
              (insert-before-markers string))))))
    (cider-repl--show-maximum-output)))

(defun cider-repl-newline-and-indent ()
  "Insert a newline, then indent the next line.
Restrict the buffer from the prompt for indentation, to avoid being
confused by strange characters (like unmatched quotes) appearing
earlier in the buffer."
  (interactive)
  (save-restriction
    (narrow-to-region cider-repl-prompt-start-mark (point-max))
    (insert "\n")
    (lisp-indent-line)))

(defun cider-repl-indent-and-complete-symbol ()
  "Indent the current line and perform symbol completion.
First indent the line.  If indenting doesn't move point, complete
the symbol."
  (interactive)
  (let ((pos (point)))
    (lisp-indent-line)
    (when (= pos (point))
      (if (save-excursion (re-search-backward "[^() \n\t\r]+\\=" nil t))
          (completion-at-point)))))

(defun cider-repl-kill-input ()
  "Kill all text from the prompt to point."
  (interactive)
  (cond ((< (marker-position cider-repl-input-start-mark) (point))
         (kill-region cider-repl-input-start-mark (point)))
        ((= (point) (marker-position cider-repl-input-start-mark))
         (cider-repl-delete-current-input))))

(defun cider-repl--input-complete-p (start end)
  "Return t if the region from START to END is a complete sexp."
  (save-excursion
    (goto-char start)
    (cond ((looking-at-p "\\s *[@'`#]?[(\"]")
           (ignore-errors
             (save-restriction
               (narrow-to-region start end)
               ;; Keep stepping over blanks and sexps until the end of
               ;; buffer is reached or an error occurs. Tolerate extra
               ;; close parens.
               (cl-loop do (skip-chars-forward " \t\r\n)")
                        until (eobp)
                        do (forward-sexp))
               t)))
          (t t))))

(defun cider-repl-handler (buffer)
  "Make an nREPL evaluation handler for the REPL BUFFER."
  (nrepl-make-response-handler buffer
                               (lambda (buffer value)
                                 (cider-repl-emit-result buffer value t))
                               (lambda (buffer out)
                                 (cider-repl-emit-stdout buffer out))
                               (lambda (buffer err)
                                 (cider-repl-emit-stderr buffer err))
                               (lambda (buffer)
                                 (cider-repl-emit-prompt buffer))
                               nrepl-err-handler
                               (lambda (buffer pprint-out)
                                 (cider-repl-emit-result buffer pprint-out nil))))

(defun cider-repl--send-input (&optional newline)
  "Go to the end of the input and send the current input.
If NEWLINE is true then add a newline at the end of the input."
  (unless (cider-repl--in-input-area-p)
    (error "No input at point"))
  (goto-char (point-max))
  (let ((end (point)))             ; end of input, without the newline
    (cider-repl--add-to-input-history (buffer-substring cider-repl-input-start-mark end))
    (when newline
      (insert "\n")
      (cider-repl--show-maximum-output))
    (let ((inhibit-modification-hooks t))
      (add-text-properties cider-repl-input-start-mark
                           (point)
                           `(cider-old-input
                             ,(cl-incf cider-repl-old-input-counter))))
    (unless cider-repl-use-clojure-font-lock
      (let ((overlay (make-overlay cider-repl-input-start-mark end)))
        ;; These properties are on an overlay so that they won't be taken
        ;; by kill/yank.
        (overlay-put overlay 'read-only t)
        (overlay-put overlay 'font-lock-face 'cider-repl-input-face))))
  (let ((input (cider-repl--current-input))
        (input-start (save-excursion (cider-repl-beginning-of-defun) (point))))
    (goto-char (point-max))
    (cider-repl--mark-input-start)
    (cider-repl--mark-output-start)
    (cider-nrepl-request:eval
     input
     (cider-repl-handler (current-buffer))
     (cider-current-ns)
     (line-number-at-pos input-start)
     (cider-column-number-at-pos input-start)
     (unless (or (not cider-repl-use-pretty-printing)
                 (string-match-p "\\`[ \t\r\n]*\\'" input))
       (cider--nrepl-pprint-request-plist (cider--pretty-print-width))))))

(defun cider-repl-return (&optional end-of-input)
  "Evaluate the current input string, or insert a newline.
Send the current input ony if a whole expression has been entered,
i.e. the parenthesis are matched.
When END-OF-INPUT is non-nil, send the input even if the parentheses
are not balanced."
  (interactive "P")
  (cond
   (end-of-input
    (cider-repl--send-input))
   ((and (get-text-property (point) 'cider-old-input)
         (< (point) cider-repl-input-start-mark))
    (cider-repl--grab-old-input end-of-input)
    (cider-repl--recenter-if-needed))
   ((cider-repl--input-complete-p cider-repl-input-start-mark (point-max))
    (cider-repl--send-input t))
   (t
    (cider-repl-newline-and-indent)
    (message "[input not complete]"))))

(defun cider-repl--recenter-if-needed ()
  "Make sure that the point is visible."
  (unless (pos-visible-in-window-p (point-max))
    (save-excursion
      (goto-char (point-max))
      (recenter -1))))

(defun cider-repl--grab-old-input (replace)
  "Resend the old REPL input at point.
If REPLACE is non-nil the current input is replaced with the old
input; otherwise the new input is appended.  The old input has the
text property `cider-old-input'."
  (cl-multiple-value-bind (beg end) (cider-property-bounds 'cider-old-input)
    (let ((old-input (buffer-substring beg end)) ;;preserve
          ;;properties, they will be removed later
          (offset (- (point) beg)))
      ;; Append the old input or replace the current input
      (cond (replace (goto-char cider-repl-input-start-mark))
            (t (goto-char (point-max))
               (unless (eq (char-before) ?\ )
                 (insert " "))))
      (delete-region (point) (point-max))
      (save-excursion
        (insert old-input)
        (when (equal (char-before) ?\n)
          (delete-char -1)))
      (forward-char offset))))

(defun cider-repl-closing-return ()
  "Evaluate the current input string after closing all open parenthesized or bracketed expressions."
  (interactive)
  (goto-char (point-max))
  (save-restriction
    (narrow-to-region cider-repl-input-start-mark (point))
    (let ((matching-delimiter nil))
      (while (ignore-errors (save-excursion
                              (backward-up-list 1)
                              (setq matching-delimiter (cdr (syntax-after (point))))) t)
        (insert-char matching-delimiter))))
  (cider-repl-return))

(defun cider-repl-toggle-pretty-printing ()
  "Toggle pretty-printing in the REPL."
  (interactive)
  (setq cider-repl-use-pretty-printing (not cider-repl-use-pretty-printing))
  (message "Pretty printing in REPL %s."
           (if cider-repl-use-pretty-printing "enabled" "disabled")))

(defun cider--pretty-print-width ()
  "Return the width to use for pretty-printing."
  (or cider-repl-pretty-print-width
      fill-column
      80))

(defun cider-repl-switch-to-other ()
  "Switch between the Clojure and ClojureScript REPLs for the current project."
  (interactive)
  (if-let (other-connection (cider-other-connection))
      (switch-to-buffer other-connection)
    (message "There's no other REPL for the current project")))

(defvar cider-repl-clear-buffer-hook)

(defun cider-repl--clear-region (start end)
  "Delete the output and its overlays between START and END."
  (mapc #'delete-overlay (overlays-in start end))
  (delete-region start end))

(defun cider-repl-clear-buffer ()
  "Delete the output generated by the Clojure process."
  (interactive)
  (let ((inhibit-read-only t))
    (cider-repl--clear-region (point-min) cider-repl-prompt-start-mark)
    (cider-repl--clear-region cider-repl-output-start cider-repl-output-end)
    (when (< (point) cider-repl-input-start-mark)
      (goto-char cider-repl-input-start-mark))
    (recenter t))
  (run-hooks 'cider-repl-clear-buffer-hook))

(defun cider-repl--end-of-line-before-input-start ()
  "Return the position of the end of the line preceding the beginning of input."
  (1- (previous-single-property-change cider-repl-input-start-mark 'field nil
                                       (1+ (point-min)))))

(defun cider-repl-clear-output (&optional clear-repl)
  "Delete the output inserted since the last input.
With a prefix argument CLEAR-REPL it will clear the entire REPL buffer instead."
  (interactive "P")
  (if clear-repl
      (cider-repl-clear-buffer)
    (let ((start (save-excursion
                   (cider-repl-previous-prompt)
                   (ignore-errors (forward-sexp))
                   (forward-line)
                   (point)))
          (end (cider-repl--end-of-line-before-input-start)))
      (when (< start end)
        (let ((inhibit-read-only t))
          (cider-repl--clear-region start end)
          (save-excursion
            (goto-char start)
            (insert
             (propertize ";; output cleared" 'font-lock-face 'font-lock-comment-face))))))))

(defun cider-repl-clear-banners ()
  "Delete the REPL banners."
  (interactive)
  ;; TODO: Improve the boundaries detecting logic
  ;; probably it should be based on text properties
  ;; the current implemetation will clear warnings as well
  (let ((start (point-min))
        (end (save-excursion
               (goto-char (point-min))
               (cider-repl-next-prompt)
               (forward-line -1)
               (end-of-line)
               (point))))
    (when (< start end)
      (let ((inhibit-read-only t))
        (cider-repl--clear-region start (1+ end))))))

(defun cider-repl-clear-help-banner ()
  "Delete the help REPL banner."
  (interactive)
  ;; TODO: Improve the boundaries detecting logic
  ;; probably it should be based on text properties
  (let ((start (save-excursion
                 (goto-char (point-min))
                 (search-forward ";; =")
                 (beginning-of-line)
                 (point)))
        (end (save-excursion
               (goto-char (point-min))
               (cider-repl-next-prompt)
               (search-backward ";; =")
               (end-of-line)
               (point))))
    (when (< start end)
      (let ((inhibit-read-only t))
        (cider-repl--clear-region start (1+ end))))))

(defun cider-repl-switch-ns-handler (buffer)
  "Make an nREPL evaluation handler for the REPL BUFFER's ns switching."
  (nrepl-make-response-handler buffer
                               (lambda (_buffer _value))
                               (lambda (buffer out)
                                 (cider-repl-emit-stdout buffer out))
                               (lambda (buffer err)
                                 (cider-repl-emit-stderr buffer err))
                               (lambda (buffer)
                                 (cider-repl-emit-prompt buffer))))

(defun cider-repl-set-ns (ns)
  "Switch the namespace of the REPL buffer to NS.

If called from a cljc or cljx buffer act on both the Clojure and
ClojureScript REPL if there are more than one REPL present.

If invoked in a REPL buffer the command will prompt for the name of the
namespace to switch to."
  (interactive (list (if (or (derived-mode-p 'cider-repl-mode)
                             (null (cider-ns-form)))
                         (completing-read "Switch to namespace: "
                                          (cider-sync-request:ns-list))
                       (cider-current-ns))))
  (when (or (not ns) (equal ns ""))
    (user-error "No namespace selected"))
  (cider-map-connections
   (lambda (connection)
     (cider-nrepl-request:eval (format "(in-ns '%s)" ns)
                               (cider-repl-switch-ns-handler connection)))
   :both))


;;; Location References

(defcustom cider-locref-regexp-alist
  '((stdout-stacktrace "[ \t]\\(at \\([^$(]+\\).*(\\([^:()]+\\):\\([0-9]+\\))\\)" 1 2 3 4)
    (aviso-stacktrace  "^[ \t]*\\(\\([^$/ \t]+\\).*? +\\([^:]+\\): +\\([0-9]+\\)\\)" 1 2 3 4)
    (print-stacktrace  "\\[\\([^][$ \t]+\\).* +\\([^ \t]+\\) +\\([0-9]+\\)\\]" 0 1 2 3)
    (timbre-log        "\\(TRACE\\|INFO\\|DEBUG\\|WARN\\|ERROR\\) +\\(\\[\\([^:]+\\):\\([0-9]+\\)\\]\\)" 2 3 nil 4))
  "Alist holding regular expressions for inline location references.
Each element in the alist has the form (NAME REGEXP HIGHLIGHT VAR FILE
LINE), where NAME is the identifier of the regexp, REGEXP - regexp matching
a location, HIGHLIGHT - sub-expression matching region to highlight on
mouse-over, VAR - sub-expression giving Clojure VAR to look up.  FILE is
currently only used when VAR is nil and must be full resource path in that
case."
  :type '(alist :key-type sexp)
  :group 'cider-repl
  :package-version '(cider. "0.16.0"))

(defun cider--locref-at-point-1 (reg-list &optional pos)
  "Workhorse for getting locref at POS.
REG-LIST is an entry in `cider-locref-regexp-alist'."
  (save-excursion
    (let ((pos (or pos (point))))
      (goto-char pos)
      (beginning-of-line)
      (when (re-search-forward (nth 1 reg-list) (point-at-eol) t)
        (let ((ix-highlight (or (nth 2 reg-list) 0))
              (ix-var (nth 3 reg-list))
              (ix-file (nth 4 reg-list))
              (ix-line (nth 5 reg-list)))
          (list
           :type (car reg-list)
           :highlight (cons (match-beginning ix-highlight) (match-end ix-highlight))
           :var  (and ix-var
                      (replace-regexp-in-string "_" "-"
                                                (match-string-no-properties ix-var)
                                                nil t))
           :file (and ix-file (match-string-no-properties ix-file))
           :line (and ix-line (string-to-number (match-string-no-properties ix-line)))))))))

(defun cider-locref-at-point (&optional pos)
  "Return a plist of components of the location reference at POS.
Limit search to current line only and return nil if no location has been
found.  Returned keys are :type, :highlight, :var, :file, :line, where
:highlight is a cons of positions, :var and :file are strings or nil, :line
is a number.  See `cider-locref-regexp-alist' for how to specify regexes
for locref look up."
  (seq-some (lambda (rl) (cider--locref-at-point-1 rl pos))
            cider-locref-regexp-alist))

(defun cider-jump-to-locref-at-point (&optional pos)
  "Identify location reference at POS and navigate to it.
This function is used from help-echo property inside REPL buffers and uses
regexes from `cider-locref-regexp-alist' to infer locations at point."
  (interactive)
  (if-let ((loc (cider-locref-at-point pos)))
      (let* ((var (plist-get loc :var))
             (line (plist-get loc :line))
             (file (or
                    ;; retrieve from info middleware
                    (when var
                      (or (cider-sync-request:ns-path var)
                          (nrepl-dict-get (cider-sync-request:info var) "file")))
                    ;; when not found, return the file detected by regexp
                    (plist-get loc :file))))
        (if file
            (cider--jump-to-loc-from-info (nrepl-dict "file" file "line" line) t)
          (error "No source location for %s" var)))
    (user-error "No location reference at point")))

(defvar cider-locref-hoover-overlay
  (let ((o (make-overlay 1 1)))
    (overlay-put o 'category 'cider-error-hoover)
    ;; (overlay-put o 'face 'highlight)
    (overlay-put o 'pointer 'hand)
    (overlay-put o 'mouse-face 'highlight)
    (overlay-put o 'follow-link 'mouse)
    (overlay-put o 'keymap
                 (let ((map (make-sparse-keymap)))
                   (define-key map [return]  'cider-jump-to-locref-at-point)
                   (define-key map [mouse-2] 'cider-jump-to-locref-at-point)
                   map))
    o)
  "Overlay used during hoovering on location references in REPL buffers.
One for all REPLs.")

(defun cider-locref-help-echo (win buffer pos)
  "Function for help-echo property in REPL buffers.
WIN, BUFFER and POS are the window, buffer and point under mouse position."
  (with-current-buffer buffer
    (if-let ((hl (plist-get (cider-locref-at-point pos) :highlight)))
        (move-overlay cider-locref-hoover-overlay (car hl) (cdr hl))
      (delete-overlay cider-locref-hoover-overlay))
    nil))


;;; History

(defcustom cider-repl-wrap-history nil
  "T to wrap history around when the end is reached."
  :type 'boolean
  :group 'cider-repl)

;; These two vars contain the state of the last history search.  We
;; only use them if `last-command' was `cider-repl--history-replace',
;; otherwise we reinitialize them.

(defvar cider-repl-input-history-position -1
  "Newer items have smaller indices.")

(defvar cider-repl-history-pattern nil
  "The regexp most recently used for finding input history.")

(defun cider-repl--add-to-input-history (string)
  "Add STRING to the input history.
Empty strings and duplicates are ignored."
  (unless (or (equal string "")
              (equal string (car cider-repl-input-history)))
    (push string cider-repl-input-history)
    (cl-incf cider-repl-input-history-items-added)))

(defun cider-repl-delete-current-input ()
  "Delete all text after the prompt."
  (goto-char (point-max))
  (delete-region cider-repl-input-start-mark (point-max)))

(defun cider-repl--replace-input (string)
  "Replace the current REPL input with STRING."
  (cider-repl-delete-current-input)
  (insert-and-inherit string))

(defun cider-repl--position-in-history (start-pos direction regexp)
  "Return the position of the history item starting at START-POS.
Search in DIRECTION for REGEXP.
Return -1 resp the length of the history if no item matches."
  ;; Loop through the history list looking for a matching line
  (let* ((step (cl-ecase direction
                 (forward -1)
                 (backward 1)))
         (history cider-repl-input-history)
         (len (length history)))
    (cl-loop for pos = (+ start-pos step) then (+ pos step)
             if (< pos 0) return -1
             if (<= len pos) return len
             if (string-match-p regexp (nth pos history)) return pos)))

(defun cider-repl--history-replace (direction &optional regexp)
  "Replace the current input with the next line in DIRECTION.
DIRECTION is 'forward' or 'backward' (in the history list).
If REGEXP is non-nil, only lines matching REGEXP are considered."
  (setq cider-repl-history-pattern regexp)
  (let* ((min-pos -1)
         (max-pos (length cider-repl-input-history))
         (pos0 (cond ((cider-history-search-in-progress-p)
                      cider-repl-input-history-position)
                     (t min-pos)))
         (pos (cider-repl--position-in-history pos0 direction (or regexp "")))
         (msg nil))
    (cond ((and (< min-pos pos) (< pos max-pos))
           (cider-repl--replace-input (nth pos cider-repl-input-history))
           (setq msg (format "History item: %d" pos)))
          ((not cider-repl-wrap-history)
           (setq msg (cond ((= pos min-pos) "End of history")
                           ((= pos max-pos) "Beginning of history"))))
          (cider-repl-wrap-history
           (setq pos (if (= pos min-pos) max-pos min-pos))
           (setq msg "Wrapped history")))
    (when (or (<= pos min-pos) (<= max-pos pos))
      (when regexp
        (setq msg (concat msg "; no matching item"))))
    (message "%s%s" msg (cond ((not regexp) "")
                              (t (format "; current regexp: %s" regexp))))
    (setq cider-repl-input-history-position pos)
    (setq this-command 'cider-repl--history-replace)))

(defun cider-history-search-in-progress-p ()
  "Return t if a current history search is in progress."
  (eq last-command 'cider-repl--history-replace))

(defun cider-terminate-history-search ()
  "Terminate the current history search."
  (setq last-command this-command))

(defun cider-repl-previous-input ()
  "Cycle backwards through input history.
If the `last-command' was a history navigation command use the
same search pattern for this command.
Otherwise use the current input as search pattern."
  (interactive)
  (cider-repl--history-replace 'backward (cider-repl-history-pattern t)))

(defun cider-repl-next-input ()
  "Cycle forwards through input history.
See `cider-previous-input'."
  (interactive)
  (cider-repl--history-replace 'forward (cider-repl-history-pattern t)))

(defun cider-repl-forward-input ()
  "Cycle forwards through input history."
  (interactive)
  (cider-repl--history-replace 'forward (cider-repl-history-pattern)))

(defun cider-repl-backward-input ()
  "Cycle backwards through input history."
  (interactive)
  (cider-repl--history-replace 'backward (cider-repl-history-pattern)))

(defun cider-repl-previous-matching-input (regexp)
  "Find the previous input matching REGEXP."
  (interactive "sPrevious element matching (regexp): ")
  (cider-terminate-history-search)
  (cider-repl--history-replace 'backward regexp))

(defun cider-repl-next-matching-input (regexp)
  "Find then next input matching REGEXP."
  (interactive "sNext element matching (regexp): ")
  (cider-terminate-history-search)
  (cider-repl--history-replace 'forward regexp))

(defun cider-repl-history-pattern (&optional use-current-input)
  "Return the regexp for the navigation commands.
If USE-CURRENT-INPUT is non-nil, use the current input."
  (cond ((cider-history-search-in-progress-p)
         cider-repl-history-pattern)
        (use-current-input
         (cl-assert (<= cider-repl-input-start-mark (point)))
         (let ((str (cider-repl--current-input t)))
           (cond ((string-match-p "^[ \n]*$" str) nil)
                 (t (concat "^" (regexp-quote str))))))
        (t nil)))

;;; persistent history
(defcustom cider-repl-history-size 500
  "The maximum number of items to keep in the REPL history."
  :type 'integer
  :safe #'integerp
  :group 'cider-repl)

(defcustom cider-repl-history-file nil
  "File to save the persistent REPL history to."
  :type 'string
  :safe #'stringp
  :group 'cider-repl)

(defun cider-repl--history-read-filename ()
  "Ask the user which file to use, defaulting `cider-repl-history-file'."
  (read-file-name "Use CIDER REPL history file: "
                  cider-repl-history-file))

(defun cider-repl--history-read (filename)
  "Read history from FILENAME and return it.
It does not yet set the input history."
  (if (file-readable-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (when (> (buffer-size (current-buffer)) 0)
          (read (current-buffer))))
    '()))

(defun cider-repl-history-load (&optional filename)
  "Load history from FILENAME into current session.
FILENAME defaults to the value of `cider-repl-history-file' but user
defined filenames can be used to read special history files.

The value of `cider-repl-input-history' is set by this function."
  (interactive (list (cider-repl--history-read-filename)))
  (let ((f (or filename cider-repl-history-file)))
    ;; TODO: probably need to set cider-repl-input-history-position as well.
    ;; in a fresh connection the newest item in the list is currently
    ;; not available.  After sending one input, everything seems to work.
    (setq cider-repl-input-history (cider-repl--history-read f))))

(defun cider-repl--history-write (filename)
  "Write history to FILENAME.
Currently coding system for writing the contents is hardwired to
utf-8-unix."
  (let* ((mhist (cider-repl--histories-merge cider-repl-input-history
                                             cider-repl-input-history-items-added
                                             (cider-repl--history-read filename)))
         ;; newest items are at the beginning of the list, thus 0
         (hist (cl-subseq mhist 0 (min (length mhist) cider-repl-history-size))))
    (unless (file-writable-p filename)
      (error (format "History file not writable: %s" filename)))
    (let ((print-length nil) (print-level nil))
      (with-temp-file filename
        ;; TODO: really set cs for output
        ;; TODO: does cs need to be customizable?
        (insert ";; -*- coding: utf-8-unix -*-\n")
        (insert ";; Automatically written history of CIDER REPL session\n")
        (insert ";; Edit at your own risk\n\n")
        (prin1 (mapcar #'substring-no-properties hist) (current-buffer))))))

(defun cider-repl-history-save (&optional filename)
  "Save the current REPL input history to FILENAME.
FILENAME defaults to the value of `cider-repl-history-file'."
  (interactive (list (cider-repl--history-read-filename)))
  (let* ((file (or filename cider-repl-history-file)))
    (cider-repl--history-write file)))

(defun cider-repl-history-just-save ()
  "Just save the history to `cider-repl-history-file'.
This function is meant to be used in hooks to avoid lambda
constructs."
  (cider-repl-history-save cider-repl-history-file))

;; SLIME has different semantics and will not save any duplicates.
;; we keep track of how many items were added to the history in the
;; current session in `cider-repl--add-to-input-history' and merge only the
;; new items with the current history found in the file, which may
;; have been changed in the meantime by another session.
(defun cider-repl--histories-merge (session-hist n-added-items file-hist)
  "Merge histories from SESSION-HIST adding N-ADDED-ITEMS into FILE-HIST."
  (append (cl-subseq session-hist 0 n-added-items)
          file-hist))


;;; REPL shortcuts
(defcustom cider-repl-shortcut-dispatch-char ?\,
  "Character used to distinguish REPL commands from Lisp forms."
  :type '(character)
  :group 'cider-repl)

(defvar cider-repl-shortcuts (make-hash-table :test 'equal))

(defun cider-repl-add-shortcut (name handler)
  "Add a REPL shortcut command, defined by NAME and HANDLER."
  (puthash name handler cider-repl-shortcuts))

(declare-function cider-restart "cider-interaction")
(declare-function cider-quit "cider-interaction")
(declare-function cider-toggle-trace-ns "cider-interaction")
(declare-function cider-undef "cider-interaction")
(declare-function cider-browse-ns "cider-browse-ns")
(declare-function cider-classpath "cider-classpath")
(declare-function cider-repl-history "cider-repl-history")
(declare-function cider-run "cider-interaction")
(declare-function cider-refresh "cider-interaction")
(cider-repl-add-shortcut "clear-output" #'cider-repl-clear-output)
(cider-repl-add-shortcut "clear" #'cider-repl-clear-buffer)
(cider-repl-add-shortcut "clear-banners" #'cider-repl-clear-banners)
(cider-repl-add-shortcut "clear-help-banner" #'cider-repl-clear-help-banner)
(cider-repl-add-shortcut "ns" #'cider-repl-set-ns)
(cider-repl-add-shortcut "toggle-pretty" #'cider-repl-toggle-pretty-printing)
(cider-repl-add-shortcut "browse-ns" (lambda () (cider-browse-ns (cider-current-ns))))
(cider-repl-add-shortcut "classpath" #'cider-classpath)
(cider-repl-add-shortcut "history" #'cider-repl-history)
(cider-repl-add-shortcut "trace-ns" #'cider-toggle-trace-ns)
(cider-repl-add-shortcut "undef" #'cider-undef)
(cider-repl-add-shortcut "refresh" #'cider-refresh)
(cider-repl-add-shortcut "help" #'cider-repl-shortcuts-help)
(cider-repl-add-shortcut "test-ns" #'cider-test-run-ns-tests)
(cider-repl-add-shortcut "test-all" #'cider-test-run-loaded-tests)
(cider-repl-add-shortcut "test-project" #'cider-test-run-project-tests)
(cider-repl-add-shortcut "test-report" #'cider-test-show-report)
(cider-repl-add-shortcut "run" #'cider-run)
(cider-repl-add-shortcut "conn-info" #'cider-display-connection-info)
(cider-repl-add-shortcut "conn-rotate" #'cider-rotate-default-connection)
(cider-repl-add-shortcut "hasta la vista" #'cider-quit)
(cider-repl-add-shortcut "adios" #'cider-quit)
(cider-repl-add-shortcut "sayonara" #'cider-quit)
(cider-repl-add-shortcut "quit" #'cider-quit)
(cider-repl-add-shortcut "restart" #'cider-restart)
(cider-repl-add-shortcut "version" #'cider-version)

(defconst cider-repl-shortcuts-help-buffer "*CIDER REPL Shortcuts Help*")

(defun cider-repl-shortcuts-help ()
  "Display a help buffer."
  (interactive)
  (ignore-errors (kill-buffer cider-repl-shortcuts-help-buffer))
  (with-current-buffer (get-buffer-create cider-repl-shortcuts-help-buffer)
    (insert "CIDER REPL shortcuts:\n\n")
    (maphash (lambda (k v) (insert (format "%s:\n\t%s\n" k v))) cider-repl-shortcuts)
    (goto-char (point-min))
    (help-mode)
    (display-buffer (current-buffer) t))
  (cider-repl-handle-shortcut)
  (current-buffer))

(defun cider-repl--available-shortcuts ()
  "Return the available REPL shortcuts."
  (cider-util--hash-keys cider-repl-shortcuts))

(defun cider-repl-handle-shortcut ()
  "Execute a REPL shortcut."
  (interactive)
  (if (> (point) cider-repl-input-start-mark)
      (insert (string cider-repl-shortcut-dispatch-char))
    (let ((command (completing-read "Command: "
                                    (cider-repl--available-shortcuts))))
      (if (not (equal command ""))
          (let ((command-func (gethash command cider-repl-shortcuts)))
            (if command-func
                (call-interactively (gethash command cider-repl-shortcuts))
              (error "Unknown command %S.  Available commands: %s"
                     command-func
                     (mapconcat 'identity (cider-repl--available-shortcuts) ", "))))
        (error "No command selected")))))


;;;;; CIDER REPL mode
(defvar cider-repl-mode-hook nil
  "Hook executed when entering `cider-repl-mode'.")

(defvar cider-repl-mode-syntax-table
  (copy-syntax-table clojure-mode-syntax-table))

(declare-function cider-eval-region "cider-interaction")
(declare-function cider-eval-last-sexp "cider-interaction")
(declare-function cider-refresh "cider-interaction")
(declare-function cider-toggle-trace-ns "cider-interaction")
(declare-function cider-toggle-trace-var "cider-interaction")
(declare-function cider-find-resource "cider-interaction")
(declare-function cider-restart "cider-interaction")
(declare-function cider-find-ns "cider-interaction")
(declare-function cider-switch-to-last-clojure-buffer "cider-mode")

(defvar cider-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") 'cider-doc-map)
    (define-key map (kbd "C-c ,")   'cider-test-commands-map)
    (define-key map (kbd "C-c C-t") 'cider-test-commands-map)
    (define-key map (kbd "M-.") #'cider-find-var)
    (define-key map (kbd "C-c C-.") #'cider-find-ns)
    (define-key map (kbd "M-,") #'cider-pop-back)
    (define-key map (kbd "C-c M-.") #'cider-find-resource)
    (define-key map (kbd "RET") #'cider-repl-return)
    (define-key map (kbd "TAB") #'cider-repl-tab)
    (define-key map (kbd "C-<return>") #'cider-repl-closing-return)
    (define-key map (kbd "C-j") #'cider-repl-newline-and-indent)
    (define-key map (kbd "C-c C-o") #'cider-repl-clear-output)
    (define-key map (kbd "C-c M-n") #'cider-repl-set-ns)
    (define-key map (kbd "C-c C-u") #'cider-repl-kill-input)
    (define-key map (kbd "C-S-a") #'cider-repl-bol-mark)
    (define-key map [S-home] #'cider-repl-bol-mark)
    (define-key map (kbd "C-<up>") #'cider-repl-backward-input)
    (define-key map (kbd "C-<down>") #'cider-repl-forward-input)
    (define-key map (kbd "M-p") #'cider-repl-previous-input)
    (define-key map (kbd "M-n") #'cider-repl-next-input)
    (define-key map (kbd "M-r") #'cider-repl-previous-matching-input)
    (define-key map (kbd "M-s") #'cider-repl-next-matching-input)
    (define-key map (kbd "C-c C-n") #'cider-repl-next-prompt)
    (define-key map (kbd "C-c C-p") #'cider-repl-previous-prompt)
    (define-key map (kbd "C-c C-b") #'cider-interrupt)
    (define-key map (kbd "C-c C-c") #'cider-interrupt)
    (define-key map (kbd "C-c C-m") #'cider-macroexpand-1)
    (define-key map (kbd "C-c M-m") #'cider-macroexpand-all)
    (define-key map (kbd "C-c C-z") #'cider-switch-to-last-clojure-buffer)
    (define-key map (kbd "C-c M-o") #'cider-repl-switch-to-other)
    (define-key map (kbd "C-c M-s") #'cider-selector)
    (define-key map (kbd "C-c M-d") #'cider-display-connection-info)
    (define-key map (kbd "C-c C-q") #'cider-quit)
    (define-key map (kbd "C-c M-i") #'cider-inspect)
    (define-key map (kbd "C-c M-p") #'cider-repl-history)
    (define-key map (kbd "C-c M-t v") #'cider-toggle-trace-var)
    (define-key map (kbd "C-c M-t n") #'cider-toggle-trace-ns)
    (define-key map (kbd "C-c C-x") #'cider-refresh)
    (define-key map (kbd "C-x C-e") #'cider-eval-last-sexp)
    (define-key map (kbd "C-c C-r") #'cider-eval-region)
    (define-key map (string cider-repl-shortcut-dispatch-char) #'cider-repl-handle-shortcut)
    (easy-menu-define cider-repl-mode-menu map
      "Menu for CIDER's REPL mode"
      `("REPL"
        ["Complete symbol" complete-symbol]
        "--"
        ,cider-doc-menu
        "--"
        ("Find"
         ["Find definition" cider-find-var]
         ["Find resource" cider-find-resource]
         ["Go back" cider-pop-back])
        "--"
        ["Switch to Clojure buffer" cider-switch-to-last-clojure-buffer]
        ["Switch to other REPL" cider-repl-switch-to-other]
        "--"
        ("Macroexpand"
         ["Macroexpand-1" cider-macroexpand-1]
         ["Macroexpand-all" cider-macroexpand-all])
        "--"
        ,cider-test-menu
        "--"
        ["Run project (-main function)" cider-run]
        ["Inspect" cider-inspect]
        ["Toggle var tracing" cider-toggle-trace-var]
        ["Toggle ns tracing" cider-toggle-trace-ns]
        ["Refresh loaded code" cider-refresh]
        "--"
        ["Set REPL ns" cider-repl-set-ns]
        ["Toggle pretty printing" cider-repl-toggle-pretty-printing]
        "--"
        ["Browse classpath" cider-classpath]
        ["Browse classpath entry" cider-open-classpath-entry]
        ["Browse namespace" cider-browse-ns]
        ["Browse all namespaces" cider-browse-ns-all]
        ["Browse spec" cider-browse-spec]
        ["Browse all specs" cider-browse-spec-all]
        "--"
        ["Next prompt" cider-repl-next-prompt]
        ["Previous prompt" cider-repl-previous-prompt]
        ["Clear output" cider-repl-clear-output]
        ["Clear buffer" cider-repl-clear-buffer]
        ["Clear banners" cider-repl-clear-banners]
        ["Clear help banner" cider-repl-clear-help-banner]
        ["Kill input" cider-repl-kill-input]
        "--"
        ["Interrupt evaluation" cider-interrupt]
        "--"
        ["Connection info" cider-display-connection-info]
        "--"
        ["Close ancillary buffers" cider-close-ancillary-buffers]
        ["Quit" cider-quit]
        ["Restart" cider-restart]
        "--"
        ["A sip of CIDER" cider-drink-a-sip]
        ["View manual online" cider-view-manual]
        ["View refcard online" cider-view-refcard]
        ["Report a bug" cider-report-bug]
        ["Version info" cider-version]))
    map))

(defun cider-repl-wrap-fontify-function (func)
  "Return a function that will call FUNC narrowed to input region."
  (lambda (beg end &rest rest)
    (when (and cider-repl-input-start-mark
               (> end cider-repl-input-start-mark))
      (save-restriction
        (narrow-to-region cider-repl-input-start-mark (point-max))
        (let ((font-lock-dont-widen t))
          (apply func (max beg cider-repl-input-start-mark) end rest))))))

(declare-function cider-complete-at-point "cider-interaction")
(defvar cider--static-font-lock-keywords)

(define-derived-mode cider-repl-mode fundamental-mode "REPL"
  "Major mode for Clojure REPL interactions.

\\{cider-repl-mode-map}"
  (clojure-mode-variables)
  (clojure-font-lock-setup)
  (font-lock-add-keywords nil cider--static-font-lock-keywords)
  (setq-local font-lock-fontify-region-function
              (cider-repl-wrap-fontify-function font-lock-fontify-region-function))
  (setq-local font-lock-unfontify-region-function
              (cider-repl-wrap-fontify-function font-lock-unfontify-region-function))
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions
               #'cider-complete-at-point)
  (set-syntax-table cider-repl-mode-syntax-table)
  (cider-eldoc-setup)
  ;; At the REPL, we define beginning-of-defun and end-of-defun to be
  ;; the start of the previous prompt or next prompt respectively.
  ;; Notice the interplay with `cider-repl-beginning-of-defun'.
  (setq-local beginning-of-defun-function #'cider-repl-mode-beginning-of-defun)
  (setq-local end-of-defun-function #'cider-repl-mode-end-of-defun)
  (setq-local prettify-symbols-alist clojure--prettify-symbols-alist)
  ;; apply dir-local variables to REPL buffers
  (hack-dir-local-variables-non-file-buffer)
  (when cider-repl-history-file
    (cider-repl-history-load cider-repl-history-file)
    (add-hook 'kill-buffer-hook #'cider-repl-history-just-save t t)
    (add-hook 'kill-emacs-hook #'cider-repl-history-just-save))
  (add-hook 'paredit-mode-hook (lambda () (clojure-paredit-setup cider-repl-mode-map))))

(provide 'cider-repl)

;;; cider-repl.el ends here
