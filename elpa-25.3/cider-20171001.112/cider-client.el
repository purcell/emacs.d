;;; cider-client.el --- A layer of abstraction above the actual client code. -*- lexical-binding: t -*-

;; Copyright © 2013-2017 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>

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

;; A layer of abstraction above the actual client code.

;;; Code:

(require 'spinner)
(require 'ewoc)
(require 'nrepl-client)
(require 'cider-common)
(require 'cider-util)
(require 'clojure-mode)

(require 'subr-x)
(require 'cider-compat)
(require 'seq)

;;; Connection Buffer Management

(defcustom cider-request-dispatch 'dynamic
  "Controls the request dispatch mechanism when several connections are present.
Dynamic dispatch tries to infer the connection based on the current project
& currently visited file, while static dispatch simply uses the default
connection.

Project metadata is attached to connections when they are created with commands
like `cider-jack-in' and `cider-connect'."
  :type '(choice (const :tag "dynamic" dynamic)
                 (const :tag "static" static))
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defcustom cider-connection-message-fn #'cider-random-words-of-inspiration
  "The function to use to generate the message displayed on connect.
When set to nil no additional message will be displayed.

A good alternative to the default is `cider-random-tip'."
  :type 'function
  :group 'cider
  :package-version '(cider . "0.11.0"))

(defvar cider-connections nil
  "A list of connections.")

(defun cider-connected-p ()
  "Return t if CIDER is currently connected, nil otherwise."
  (not (null (cider-connections))))

(defun cider-ensure-connected ()
  "Ensure there is a cider connection present.
An error is signaled in the absence of a connection."
  (unless (cider-connected-p)
    (user-error "`%s' needs an active nREPL connection" this-command)))

(defsubst cider--in-connection-buffer-p ()
  "Return non-nil if current buffer is connected to a server."
  (and (derived-mode-p 'cider-repl-mode)
       (process-live-p
        (get-buffer-process (current-buffer)))))

(defun cider-default-connection (&optional no-error)
  "The default (fallback) connection to use for nREPL interaction.
When NO-ERROR is non-nil, don't throw an error when no connection has been
found."
  (or (car (cider-connections))
      (unless no-error
        (error "No nREPL connection buffer"))))

(defun cider-connections ()
  "Return the list of connection buffers.
If the list is empty and buffer-local, return the global value."
  (or (setq cider-connections
            (seq-filter #'buffer-live-p cider-connections))
      (when (local-variable-p 'cider-connect)
        (kill-local-variable 'cider-connections)
        (seq-filter #'buffer-live-p cider-connections))))

(defun cider-repl-buffers ()
  "Return the list of REPL buffers."
  (seq-filter
   (lambda (buffer)
     (with-current-buffer buffer (derived-mode-p 'cider-repl-mode)))
   (buffer-list)))

(defun cider-make-connection-default (connection-buffer)
  "Make the nREPL CONNECTION-BUFFER the default connection.
Moves CONNECTION-BUFFER to the front of variable `cider-connections'."
  (interactive (list (if (cider--in-connection-buffer-p)
                         (current-buffer)
                       (user-error "Not in a REPL buffer"))))
  ;; maintain the connection list in most recently used order
  (let ((buf (get-buffer connection-buffer)))
    (setq cider-connections
          (cons buf (delq buf cider-connections))))
  (cider--connections-refresh))

(declare-function cider--close-buffer "cider-interaction")
(defun cider--close-connection-buffer (conn-buffer)
  "Close CONN-BUFFER, removing it from variable `cider-connections'.
Also close associated REPL and server buffers."
  (let ((buffer (get-buffer conn-buffer))
        (nrepl-messages-buffer (and nrepl-log-messages
                                    (nrepl-messages-buffer conn-buffer))))
    (setq cider-connections
          (delq buffer cider-connections))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when spinner-current (spinner-stop))
        (when nrepl-tunnel-buffer
          (cider--close-buffer nrepl-tunnel-buffer)))
      ;; If this is the only (or last) REPL connected to its server, the
      ;; kill-process hook will kill the server.
      (cider--close-buffer buffer)
      (when nrepl-messages-buffer
        (kill-buffer nrepl-messages-buffer)))))


;;; Current connection logic
(defvar-local cider-repl-type nil
  "The type of this REPL buffer, usually either \"clj\" or \"cljs\".")

(defun cider-find-connection-buffer-for-project-directory (&optional project-directory all-connections)
  "Return the most appropriate connection-buffer for the current project.

By order of preference, this is any connection whose directory matches
`clojure-project-dir', followed by any connection whose directory is nil,
followed by any connection at all.

If PROJECT-DIRECTORY is provided act on that project instead.

Only return nil if variable `cider-connections' is empty,
i.e there are no connections.

If more than one connection satisfy a given level of preference, return the
connection buffer closer to the start of variable `cider-connections'.  This is
usally the connection that was more recently created, but the order can be
changed.  For instance, the function `cider-make-connection-default' can be
used to move a connection to the head of the list, so that it will take
precedence over other connections associated with the same project.

If ALL-CONNECTIONS is non-nil, the return value is a list and all matching
connections are returned, instead of just the most recent."
  (when-let ((project-directory (or project-directory
                                    (clojure-project-dir (cider-current-dir))))
             (fn (if all-connections #'seq-filter #'seq-find)))
    (or (funcall fn (lambda (conn)
                      (when-let ((conn-proj-dir (with-current-buffer conn
                                                  nrepl-project-dir)))
                        (equal (file-truename project-directory)
                               (file-truename conn-proj-dir))))
                 cider-connections)
        (funcall fn (lambda (conn)
                      (with-current-buffer conn
                        (not nrepl-project-dir)))
                 cider-connections)
        (if all-connections
            cider-connections
          (car cider-connections)))))

(defun cider-connection-type-for-buffer ()
  "Return the matching connection type (clj or cljs) for the current buffer.
In cljc and cljx buffers return \"multi\". This function infers connection
type based on the major mode. See `cider-project-connections-types' for a
list of types of actual connections within a project."
  (cond
   ((derived-mode-p 'clojurescript-mode) "cljs")
   ((derived-mode-p 'clojurec-mode) "multi")
   ((derived-mode-p 'clojurex-mode) "multi")
   ((derived-mode-p 'clojure-mode) "clj")
   (cider-repl-type)))

(defun cider-project-connections-types ()
  "Return a list of types of connections within current project."
  (let ((connections (cider-find-connection-buffer-for-project-directory nil :all-connections)))
    (seq-uniq (seq-map #'cider--connection-type connections))))

(defun cider-read-connection (prompt)
  "Completing read for connections using PROMPT."
  (get-buffer (completing-read prompt (mapcar #'buffer-name (cider-connections)))))

(defun cider-assoc-project-with-connection (&optional project connection)
  "Associate a Clojure PROJECT with an nREPL CONNECTION.

Useful for connections created using `cider-connect', as for them
such a link cannot be established automatically."
  (interactive)
  (cider-ensure-connected)
  (let ((conn-buf (or connection (cider-read-connection "Connection: ")))
        (project-dir (or project (read-directory-name "Project directory: " (clojure-project-dir)))))
    (when conn-buf
      (with-current-buffer conn-buf
        (setq nrepl-project-dir project-dir)))))

(defun cider-assoc-buffer-with-connection ()
  "Associate the current buffer with a connection.

Useful for connections created using `cider-connect', as for them
such a link cannot be established automatically."
  (interactive)
  (cider-ensure-connected)
  (let ((conn (cider-read-connection "Connection: ")))
    (when conn
      (setq-local cider-connections (list conn)))))

(defun cider-toggle-buffer-connection ()
  "Toggle the current buffer's connection between Clojure and ClojureScript."
  (interactive)
  (cider-ensure-connected)
  (let ((other-conn (cider-other-connection)))
    (if other-conn
        (setq-local cider-connections (list other-conn))
      (user-error "No other connection available"))))

(defun cider-clear-buffer-local-connection ()
  "Remove association between the current buffer and a connection."
  (interactive)
  (cider-ensure-connected)
  (kill-local-variable 'cider-connections))

(defun cider-toggle-request-dispatch ()
  "Toggle the value of `cider-request-dispatch' between static and dynamic.

Handy when you're using dynamic dispatch, but you want to quickly force all
evaluation commands to use a particular connection."
  (interactive)
  (let ((new-value (if (eq cider-request-dispatch 'static) 'dynamic 'static)))
    (setq cider-request-dispatch new-value)
    (message "Toggled CIDER request dispatch to %s." new-value)))

(defun cider-current-connection (&optional type)
  "Return the REPL buffer relevant for the current Clojure source buffer.
A REPL is relevant if its `nrepl-project-dir' is compatible with the
current directory (see `cider-find-connection-buffer-for-project-directory').

When there are multiple relevant connections of the same TYPE, return the
most recently used one.

If TYPE is provided, it is either \"clj\" or \"cljs\", and only a
connection of that type is returned.  If no connections of that TYPE exist,
return nil.

If TYPE is nil, then connections whose type matches the current file
extension are given preference, but if none exist, any connection is
returned.  In this case, only return nil if there are no active connections
at all."
  ;; If TYPE was specified, we only return that type (or nil).  OW, we prefer
  ;; that TYPE, but ultimately allow any type.
  (cl-labels ((right-type-p (c type)
                            (when (or (not type)
                                      (equal type "multi")
                                      (and (buffer-live-p c)
                                           (equal (cider--connection-type c) type)))
                              c))
              (most-recent-buf (connections type)
                               (when connections
                                 (seq-find (lambda (c)
                                             (and (member c connections)
                                                  (right-type-p c type)))
                                           (buffer-list)))))
    (let ((connections (cider-connections)))
      (cond
       ((not connections) nil)
       ;; if you're in a REPL buffer, it's the connection buffer
       ((and (derived-mode-p 'cider-repl-mode) (right-type-p (current-buffer) type)))
       ((eq cider-request-dispatch 'static) (car connections))
       ((= 1 (length connections)) (right-type-p (car connections) type))
       (t (let ((project-connections (cider-find-connection-buffer-for-project-directory
                                      nil :all-connections))
                (guessed-type (or type (cider-connection-type-for-buffer))))
            (or
             ;; cljc
             (and (equal guessed-type "multi")
                  (most-recent-buf project-connections nil))
             ;; clj or cljs
             (and guessed-type
                  (or (most-recent-buf project-connections guessed-type)
                      (most-recent-buf connections guessed-type)))
             ;; when type was not specified or guessed
             (most-recent-buf project-connections type)
             (most-recent-buf connections type))))))))

(defun cider-other-connection (&optional connection)
  "Return the first connection of another type than CONNECTION.
Only return connections in the same project or nil.
CONNECTION defaults to `cider-current-connection'."
  (when-let ((connection (or connection (cider-current-connection)))
             (connection-type (cider--connection-type connection)))
    (cider-current-connection (pcase connection-type
                                (`"clj" "cljs")
                                (_ "clj")))))

(defvar cider--has-warned-about-bad-repl-type nil)

(defun cider--guess-cljs-connection ()
  "Hacky way to find a ClojureScript REPL.
DO NOT USE THIS FUNCTION.
It was written only to be used in `cider-map-connections', as a workaround
to a still-undetermined bug in the state-stracker backend."
  (when-let ((project-connections (cider-find-connection-buffer-for-project-directory
                                   nil :all-connections))
             (cljs-conn
              ;; So we have multiple connections. Look for the connection type we
              ;; want, prioritizing the current project.
              (or (seq-find (lambda (c) (string-match "\\bCLJS\\b" (buffer-name c)))
                            project-connections)
                  (seq-find (lambda (c) (string-match "\\bCLJS\\b" (buffer-name c)))
                            (cider-connections)))))
    (unless cider--has-warned-about-bad-repl-type
      (setq cider--has-warned-about-bad-repl-type t)
      (read-key
       (concat "The ClojureScript REPL seems to be is misbehaving."
               (substitute-command-keys
                "\nWe have applied a workaround, but please also file a bug report with `\\[cider-report-bug]'.")
               "\nPress any key to continue.")))
    cljs-conn))

(defun cider-map-connections (function which &optional any-mode)
  "Call FUNCTION once for each appropriate connection.
The function is called with one argument, the connection buffer.
The appropriate connections are found by inspecting the current buffer.  If
the buffer is associated with a .cljc or .cljx file, BODY will be executed
multiple times.

WHICH is one of the following keywords identifying which connections to map
over.
 :any - Act the connection whose type matches the current buffer.
 :clj - Like :any, but signal a `user-error' in `clojurescript-mode' or if
        there is no Clojure connection (use this for commands only
        supported in Clojure).
 :cljs - Like :clj, but demands a ClojureScript connection instead.
 :both - In `clojurec-mode' or `clojurex-mode' act on both connections,
         otherwise function like :any.  Obviously, this option might run
         FUNCTION twice.

If ANY-MODE is non-nil, :clj and :cljs don't signal errors due to being in
the wrong major mode (they still signal if the desired connection type
doesn't exist).  Use this for commands that only apply to a specific
connection but can be invoked from any buffer (like `cider-refresh')."
  (cl-labels ((err (msg) (user-error (concat "`%s' " msg) this-command)))
    ;; :both in a clj or cljs buffer just means :any.
    (let* ((which (if (and (eq which :both)
                           (not (cider--cljc-or-cljx-buffer-p)))
                      :any
                    which))
           (curr
            (pcase which
              (`:any (let ((type (cider-connection-type-for-buffer)))
                       (or (cider-current-connection type)
                           (when (equal type "cljs")
                             (cider--guess-cljs-connection))
                           (err (substitute-command-keys
                                 (format "needs a Clojure%s REPL.\nIf you don't know what that means, you probably need to jack-in (%s)."
                                         (if (equal type "cljs") "Script" "")
                                         (if (equal type "cljs") "`\\[cider-jack-in-clojurescript]'" "`\\[cider-jack-in]'")))))))
              (`:both (or (cider-current-connection)
                          (err "needs an active REPL connection")))
              (`:clj (cond ((and (not any-mode)
                                 (derived-mode-p 'clojurescript-mode))
                            (err "doesn't support ClojureScript"))
                           ((cider-current-connection "clj"))
                           ((err "needs a Clojure REPL"))))
              (`:cljs (cond ((and (not any-mode)
                                  (eq major-mode 'clojure-mode))
                             (err "doesn't support Clojure"))
                            ((cider-current-connection "cljs"))
                            ((err "needs a ClojureScript REPL")))))))
      (funcall function curr)
      (when (eq which :both)
        (when-let ((other-connection (cider-other-connection curr)))
          (funcall function other-connection))))))


;;; Connection Browser
(defvar cider-connections-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" #'cider-connections-make-default)
    (define-key map "g" #'cider-connection-browser)
    (define-key map "k" #'cider-connections-close-connection)
    (define-key map (kbd "RET") #'cider-connections-goto-connection)
    (define-key map "?" #'describe-mode)
    (define-key map "h" #'describe-mode)
    map))

(declare-function cider-popup-buffer-mode "cider-popup")
(define-derived-mode cider-connections-buffer-mode cider-popup-buffer-mode
  "CIDER Connections"
  "CIDER Connections Buffer Mode.
\\{cider-connections-buffer-mode-map}
\\{cider-popup-buffer-mode-map}"
  (when cider-special-mode-truncate-lines
    (setq-local truncate-lines t)))

(defvar cider--connection-ewoc)
(defconst cider--connection-browser-buffer-name "*cider-connections*")

(defun cider-connection-browser ()
  "Open a browser buffer for nREPL connections."
  (interactive)
  (if-let ((buffer (get-buffer cider--connection-browser-buffer-name)))
      (progn
        (cider--connections-refresh-buffer buffer)
        (unless (get-buffer-window buffer)
          (select-window (display-buffer buffer))))
    (cider--setup-connection-browser)))

(defun cider--connections-refresh ()
  "Refresh the connections buffer, if the buffer exists.
The connections buffer is determined by
`cider--connection-browser-buffer-name'"
  (when-let ((buffer (get-buffer cider--connection-browser-buffer-name)))
    (cider--connections-refresh-buffer buffer)))

(add-hook 'nrepl-disconnected-hook #'cider--connections-refresh)

(defun cider--connections-refresh-buffer (buffer)
  "Refresh the connections BUFFER."
  (cider--update-connections-display
   (buffer-local-value 'cider--connection-ewoc buffer)
   cider-connections))

(defun cider--setup-connection-browser ()
  "Create a browser buffer for nREPL connections."
  (with-current-buffer (get-buffer-create cider--connection-browser-buffer-name)
    (let ((ewoc (ewoc-create
                 'cider--connection-pp
                 "  REPL                           Host             Port    Project          Type\n")))
      (setq-local cider--connection-ewoc ewoc)
      (cider--update-connections-display ewoc cider-connections)
      (setq buffer-read-only t)
      (cider-connections-buffer-mode)
      (display-buffer (current-buffer)))))

(defun cider-client-name-repl-type (type)
  "Return a human-readable name for a connection TYPE.
TYPE can be any of the possible values of `cider-repl-type'."
  (pcase type
    ("clj" "Clojure")
    ("cljs" "ClojureScript")
    (_ "Unknown")))

(defun cider-project-name (project-dir)
  "Extract the project name from PROJECT-DIR."
  (if (and project-dir (not (equal project-dir "")))
      (file-name-nondirectory (directory-file-name project-dir))
    "-"))

(defun cider--connection-pp (connection)
  "Print an nREPL CONNECTION to the current buffer."
  (let* ((buffer-read-only nil)
         (buffer (get-buffer connection))
         (project-name (cider-project-name (buffer-local-value 'nrepl-project-dir buffer)))
         (repl-type (cider-client-name-repl-type (buffer-local-value 'cider-repl-type buffer)))
         (endpoint (buffer-local-value 'nrepl-endpoint buffer)))
    (insert
     (format "%s %-30s %-16s %5s   %-16s %s"
             (if (equal connection (car cider-connections)) "*" " ")
             (buffer-name connection)
             (car endpoint)
             (prin1-to-string (cadr endpoint))
             project-name
             repl-type))))

(defun cider--update-connections-display (ewoc connections)
  "Update the connections EWOC to show CONNECTIONS."
  (ewoc-filter ewoc (lambda (n) (member n connections)))
  (let ((existing))
    (ewoc-map (lambda (n) (setq existing (cons n existing))) ewoc)
    (let ((added (seq-difference connections existing)))
      (mapc (apply-partially 'ewoc-enter-last ewoc) added)
      (save-excursion (ewoc-refresh ewoc)))))

(defun cider--ewoc-apply-at-point (f)
  "Apply function F to the ewoc node at point.
F is a function of two arguments, the ewoc and the data at point."
  (let* ((ewoc cider--connection-ewoc)
         (node (and ewoc (ewoc-locate ewoc))))
    (when node
      (funcall f ewoc (ewoc-data node)))))

(defun cider-connections-make-default ()
  "Make default the connection at point in the connection browser."
  (interactive)
  (save-excursion
    (cider--ewoc-apply-at-point #'cider--connections-make-default)))

(defun cider--connections-make-default (ewoc data)
  "Make the connection in EWOC specified by DATA default.
Refreshes EWOC."
  (interactive)
  (cider-make-connection-default data)
  (ewoc-refresh ewoc))

(defun cider-connections-close-connection ()
  "Close connection at point in the connection browser."
  (interactive)
  (cider--ewoc-apply-at-point #'cider--connections-close-connection))

(defun cider--connections-close-connection (ewoc data)
  "Close the connection in EWOC specified by DATA."
  (cider--close-connection-buffer (get-buffer data))
  (cider--update-connections-display ewoc cider-connections))

(defun cider-connections-goto-connection ()
  "Goto connection at point in the connection browser."
  (interactive)
  (cider--ewoc-apply-at-point #'cider--connections-goto-connection))

(defun cider--connections-goto-connection (_ewoc data)
  "Goto the REPL for the connection in _EWOC specified by DATA."
  (when (buffer-live-p data)
    (select-window (display-buffer data))))


(defun cider-display-connected-message ()
  "Message displayed on successful connection."
  (message
   (concat "Connected."
           (if cider-connection-message-fn
               (format "  %s" (funcall cider-connection-message-fn))
             ""))))

;; TODO: Replace direct usage of such hooks with CIDER hooks,
;; that are connection type independent
(add-hook 'nrepl-connected-hook 'cider-display-connected-message)


;;; Eval spinner
(defcustom cider-eval-spinner-type 'progress-bar
  "Appearance of the evaluation spinner.

Value is a symbol.  The possible values are the symbols in the
`spinner-types' variable."
  :type 'symbol
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defcustom cider-show-eval-spinner t
  "When true, show the evaluation spinner in the mode line."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defcustom cider-eval-spinner-delay 1
  "Amount of time, in seconds, after which the evaluation spinner will be shown."
  :type 'integer
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defun cider-spinner-start (buffer)
  "Start the evaluation spinner in BUFFER.
Do nothing if `cider-show-eval-spinner' is nil."
  (when cider-show-eval-spinner
    (with-current-buffer buffer
      (spinner-start cider-eval-spinner-type nil
                     cider-eval-spinner-delay))))

(defun cider-eval-spinner-handler (eval-buffer original-callback)
  "Return a response handler to stop the spinner and call ORIGINAL-CALLBACK.
EVAL-BUFFER is the buffer where the spinner was started."
  (lambda (response)
    ;; buffer still exists and
    ;; we've got status "done" from nrepl
    ;; stop the spinner
    (when (and (buffer-live-p eval-buffer)
               (let ((status (nrepl-dict-get response "status")))
                 (or (member "done" status)
                     (member "eval-error" status)
                     (member "error" status))))
      (with-current-buffer eval-buffer
        (when spinner-current (spinner-stop))))
    (funcall original-callback response)))


;;; Evaluation helpers
(defun cider-ns-form-p (form)
  "Check if FORM is an ns form."
  (string-match-p "^[[:space:]]*\(ns\\([[:space:]]*$\\|[[:space:]]+\\)" form))

(defun cider-ns-from-form (ns-form)
  "Get ns substring from NS-FORM."
  (when (string-match "^[ \t\n]*\(ns[ \t\n]+\\([^][ \t\n(){}]+\\)" ns-form)
    (match-string-no-properties 1 ns-form)))

(defvar-local cider-buffer-ns nil
  "Current Clojure namespace of some buffer.

Useful for special buffers (e.g. REPL, doc buffers) that have to
keep track of a namespace.

This should never be set in Clojure buffers, as there the namespace
should be extracted from the buffer's ns form.")

(defun cider-current-ns (&optional no-default)
  "Return the current ns.
The ns is extracted from the ns form for Clojure buffers and from
`cider-buffer-ns' for all other buffers.  If it's missing, use the current
REPL's ns, otherwise fall back to \"user\".

When NO-DEFAULT is non-nil, it will return nil instead of \"user\"."
  (or cider-buffer-ns
      (clojure-find-ns)
      (when-let ((repl-buf (cider-current-connection)))
        (buffer-local-value 'cider-buffer-ns repl-buf))
      (if no-default nil "user")))

(defun cider-expected-ns (&optional path)
  "Return the namespace string matching PATH, or nil if not found.

PATH is expected to be an absolute file path.
If PATH is nil, use the path to the file backing the current buffer.

The command falls back to `clojure-expected-ns' in the absence of an
active nREPL connection."
  (if (cider-connected-p)
      (let* ((path (or path (file-truename (buffer-file-name))))
             (relpath (thread-last (cider-sync-request:classpath)
                        (seq-map
                         (lambda (cp)
                           (when (string-prefix-p cp path)
                             (substring path (length cp)))))
                        (seq-filter #'identity)
                        (seq-sort (lambda (a b)
                                    (< (length a) (length b))))
                        (car))))
        (if relpath
            (thread-last (substring relpath 1) ; remove leading /
              (file-name-sans-extension)
              (replace-regexp-in-string "/" ".")
              (replace-regexp-in-string "_" "-"))
          (clojure-expected-ns path)))
    (clojure-expected-ns path)))

(defun cider-nrepl-op-supported-p (op)
  "Check whether the current connection supports the nREPL middleware OP."
  (nrepl-op-supported-p op (cider-current-connection)))

(defvar cider-version)
(defun cider-ensure-op-supported (op)
  "Check for support of middleware op OP.
Signal an error if it is not supported."
  (unless (cider-nrepl-op-supported-p op)
    (user-error "`%s' requires the nREPL op \"%s\".  Please, install (or update) cider-nrepl %s and restart CIDER" this-command op (upcase cider-version))))

(defun cider-nrepl-send-request (request callback &optional connection)
  "Send REQUEST and register response handler CALLBACK.
REQUEST is a pair list of the form (\"op\" \"operation\" \"par1-name\"
\"par1\" ... ).
If CONNECTION is provided dispatch to that connection instead of
the current connection.

Return the id of the sent message."
  (nrepl-send-request request callback (or connection (cider-current-connection))))

(defun cider-nrepl-send-sync-request (request &optional connection abort-on-input)
  "Send REQUEST to the nREPL server synchronously using CONNECTION.
Hold till final \"done\" message has arrived and join all response messages
of the same \"op\" that came along and return the accumulated response.
If ABORT-ON-INPUT is non-nil, the function will return nil
at the first sign of user input, so as not to hang the
interface."
  (nrepl-send-sync-request request
                           (or connection (cider-current-connection))
                           abort-on-input))

(defun cider-nrepl-send-unhandled-request (request)
  "Send REQUEST to the nREPL server and ignore any responses.
Immediately mark the REQUEST as done.
Return the id of the sent message."
  (let* ((conn (cider-current-connection))
         (id (nrepl-send-request request #'ignore conn)))
    (with-current-buffer conn
      (nrepl--mark-id-completed id))
    id))

(defun cider-nrepl-request:eval (input callback &optional ns line column additional-params connection)
  "Send the request INPUT and register the CALLBACK as the response handler.
If NS is non-nil, include it in the request.  LINE and COLUMN, if non-nil,
define the position of INPUT in its buffer.  ADDITIONAL-PARAMS is a plist
to be appended to the request message.  CONNECTION is the connection
buffer, defaults to (cider-current-connection)."
  (let ((connection (or connection (cider-current-connection))))
    (nrepl-request:eval input
                        (if cider-show-eval-spinner
                            (cider-eval-spinner-handler connection callback)
                          callback)
                        connection
                        ns line column additional-params)
    (cider-spinner-start connection)))

(defun cider-nrepl-sync-request:eval (input &optional connection ns)
  "Send the INPUT to the nREPL CONNECTION synchronously.
If NS is non-nil, include it in the eval request."
  (nrepl-sync-request:eval input
                           (or connection (cider-current-connection))
                           ns))

(defcustom cider-pprint-fn 'pprint
  "Sets the function to use when pretty-printing evaluation results.

The value must be one of the following symbols:

  `pprint' - to use \\=`clojure.pprint/pprint\\=`

  `fipp' - to use the Fast Idiomatic Pretty Printer, approximately 5-10x
          faster than \\=`clojure.core/pprint\\=` (this is the default)

  `puget' - to use Puget, which provides canonical serialization of data on
           top of fipp, but at a slight performance cost

Alternatively, can be the namespace-qualified name of a Clojure function of
one argument.  If the function cannot be resolved, an exception will be
thrown.

The function is assumed to respect the contract of \\=`clojure.pprint/pprint\\=`
with respect to the bound values of \\=`*print-length*\\=`, \\=`*print-level*\\=`,
\\=`*print-meta*\\=`, and \\=`clojure.pprint/*print-right-margin*\\=`."
  :type '(choice (const pprint)
                 (const fipp)
                 (const puget)
                 string)
  :group 'cider
  :package-version '(cider . "0.11.0"))

(defun cider--pprint-fn ()
  "Return the value to send in the pprint-fn slot of messages."
  (pcase cider-pprint-fn
    (`pprint "clojure.pprint/pprint")
    (`fipp "cider.nrepl.middleware.pprint/fipp-pprint")
    (`puget "cider.nrepl.middleware.pprint/puget-pprint")
    (_ cider-pprint-fn)))

(defun cider--nrepl-pprint-request-plist (right-margin &optional pprint-fn)
  "Plist to be appended to an eval request to make it use pprint.
PPRINT-FN is the name of the Clojure function to use.
RIGHT-MARGIN specifies the maximum column-width of the pretty-printed
result, and is included in the request if non-nil."
  (nconc `("pprint" "true"
           "pprint-fn" ,(or pprint-fn (cider--pprint-fn)))
         (and right-margin `("print-right-margin" ,right-margin))))

(defun cider-tooling-eval (input callback &optional ns)
  "Send the request INPUT and register the CALLBACK as the response handler.
NS specifies the namespace in which to evaluate the request."
  ;; namespace forms are always evaluated in the "user" namespace
  (nrepl-request:eval input
                      callback
                      (cider-current-connection)
                      ns nil nil nil t  ; tooling
                      ))

(defalias 'cider-current-repl-buffer #'cider-current-connection
  "The current REPL buffer.
Return the REPL buffer given by `cider-current-connection'.")

(declare-function cider-interrupt-handler "cider-interaction")
(defun cider-interrupt ()
  "Interrupt any pending evaluations."
  (interactive)
  (with-current-buffer (cider-current-connection)
    (let ((pending-request-ids (cider-util--hash-keys nrepl-pending-requests)))
      (dolist (request-id pending-request-ids)
        (nrepl-request:interrupt
         request-id
         (cider-interrupt-handler (current-buffer))
         (cider-current-connection))))))

(defun cider-current-session ()
  "Return the eval nREPL session id of the current connection."
  (cider-session-for-connection (cider-current-connection)))

(defun cider-session-for-connection (connection)
  "Create a CIDER session for CONNECTION."
  (with-current-buffer connection
    nrepl-session))

(defun cider-current-messages-buffer ()
  "The nREPL messages buffer, matching the current connection."
  (nrepl-messages-buffer (cider-current-connection)))

(defun cider-current-tooling-session ()
  "Return the tooling nREPL session id of the current connection."
  (with-current-buffer (cider-current-connection)
    nrepl-tooling-session))

(defun cider--var-choice (var-info)
  "Prompt to choose from among multiple VAR-INFO candidates, if required.
This is needed only when the symbol queried is an unqualified host platform
method, and multiple classes have a so-named member.  If VAR-INFO does not
contain a `candidates' key, it is returned as is."
  (let ((candidates (nrepl-dict-get var-info "candidates")))
    (if candidates
        (let* ((classes (nrepl-dict-keys candidates))
               (choice (completing-read "Member in class: " classes nil t))
               (info (nrepl-dict-get candidates choice)))
          info)
      var-info)))

(defun cider-var-info (var &optional all)
  "Return VAR's info as an alist with list cdrs.
When multiple matching vars are returned you'll be prompted to select one,
unless ALL is truthy."
  (when (and var (not (string= var "")))
    (let ((var-info (cider-sync-request:info var)))
      (if all var-info (cider--var-choice var-info)))))

(defun cider-member-info (class member)
  "Return the CLASS MEMBER's info as an alist with list cdrs."
  (when (and class member)
    (cider-sync-request:info nil class member)))

(defun cider--find-var-other-window (var &optional line)
  "Find the definition of VAR, optionally at a specific LINE.

Display the results in a different window."
  (if-let ((info (cider-var-info var)))
      (progn
        (if line (setq info (nrepl-dict-put info "line" line)))
        (cider--jump-to-loc-from-info info t))
    (user-error "Symbol %s not resolved" var)))

(defun cider--find-var (var &optional line)
  "Find the definition of VAR, optionally at a specific LINE."
  (if-let ((info (cider-var-info var)))
      (progn
        (if line (setq info (nrepl-dict-put info "line" line)))
        (cider--jump-to-loc-from-info info))
    (user-error "Symbol %s not resolved" var)))

(defun cider-find-var (&optional arg var line)
  "Find definition for VAR at LINE.

Prompt according to prefix ARG and `cider-prompt-for-symbol'.
A single or double prefix argument inverts the meaning of
`cider-prompt-for-symbol'.  A prefix of `-` or a double prefix argument causes
the results to be displayed in a different window.  The default value is
thing at point."
  (interactive "P")
  (cider-ensure-op-supported "info")
  (if var
      (cider--find-var var line)
    (funcall (cider-prompt-for-symbol-function arg)
             "Symbol"
             (if (cider--open-other-window-p arg)
                 #'cider--find-var-other-window
               #'cider--find-var))))


;;; Requests

(declare-function cider-load-file-handler "cider-interaction")
(defun cider-request:load-file (file-contents file-path file-name &optional connection callback)
  "Perform the nREPL \"load-file\" op.
FILE-CONTENTS, FILE-PATH and FILE-NAME are details of the file to be
loaded.

If CONNECTION is nil, use `cider-current-connection'.
If CALLBACK is nil, use `cider-load-file-handler'."
  (cider-nrepl-send-request `("op" "load-file"
                              "file" ,file-contents
                              "file-path" ,file-path
                              "file-name" ,file-name)
                            (or callback
                                (cider-load-file-handler (current-buffer)))
                            connection))


;;; Sync Requests

(defcustom cider-filtered-namespaces-regexps
  '("^cider.nrepl" "^refactor-nrepl" "^clojure.tools.nrepl")
  "List of regexps used to filter out some vars/symbols/namespaces.
When nil, nothing is filtered out.  Otherwise, all namespaces matching any
regexp from this list are dropped out of the \"ns-list\" op.
Also, \"apropos\" won't include vars from such namespaces.
This list is passed on to the nREPL middleware without any pre-processing.
So the regexps have to be in Clojure format (with twice the number of
backslashes) and not Emacs Lisp."
  :type '(repeat string)
  :safe #'listp
  :group 'cider
  :package-version '(cider . "0.13.0"))

(defun cider-sync-request:apropos (query &optional search-ns docs-p privates-p case-sensitive-p)
  "Send \"apropos\" request for regexp QUERY.

Optional arguments include SEARCH-NS, DOCS-P, PRIVATES-P, CASE-SENSITIVE-P."
  (let* ((query (replace-regexp-in-string "[ \t]+" ".+" query))
         (response (cider-nrepl-send-sync-request
                    `("op" "apropos"
                      "ns" ,(cider-current-ns)
                      "query" ,query
                      ,@(when search-ns `("search-ns" ,search-ns))
                      ,@(when docs-p '("docs?" "t"))
                      ,@(when privates-p '("privates?" "t"))
                      ,@(when case-sensitive-p '("case-sensitive?" "t"))
                      "filter-regexps" ,cider-filtered-namespaces-regexps))))
    (if (member "apropos-regexp-error" (nrepl-dict-get response "status"))
        (user-error "Invalid regexp: %s" (nrepl-dict-get response "error-msg"))
      (nrepl-dict-get response "apropos-matches"))))

(defun cider-sync-request:classpath ()
  "Return a list of classpath entries."
  (cider-ensure-op-supported "classpath")
  (thread-first '("op" "classpath")
    (cider-nrepl-send-sync-request)
    (nrepl-dict-get "classpath")))

(defun cider-sync-request:complete (str context)
  "Return a list of completions for STR using nREPL's \"complete\" op.
CONTEXT represents a completion context for compliment."
  (when-let ((dict (thread-first `("op" "complete"
                                   "ns" ,(cider-current-ns)
                                   "symbol" ,str
                                   "context" ,context)
                     (cider-nrepl-send-sync-request nil 'abort-on-input))))
    (nrepl-dict-get dict "completions")))

(defun cider-sync-request:complete-flush-caches ()
  "Send \"complete-flush-caches\" op to flush Compliment's caches."
  (cider-nrepl-send-sync-request (list "op" "complete-flush-caches"
                                       "session" (cider-current-session))
                                 'abort-on-input))

(defun cider-sync-request:info (symbol &optional class member)
  "Send \"info\" op with parameters SYMBOL or CLASS and MEMBER."
  (let ((var-info (thread-first `("op" "info"
                                  "ns" ,(cider-current-ns)
                                  ,@(when symbol `("symbol" ,symbol))
                                  ,@(when class `("class" ,class))
                                  ,@(when member `("member" ,member)))
                    (cider-nrepl-send-sync-request))))
    (if (member "no-info" (nrepl-dict-get var-info "status"))
        nil
      var-info)))

(defun cider-sync-request:eldoc (symbol &optional class member)
  "Send \"eldoc\" op with parameters SYMBOL or CLASS and MEMBER."
  (when-let ((eldoc (thread-first `("op" "eldoc"
                                    "ns" ,(cider-current-ns)
                                    ,@(when symbol `("symbol" ,symbol))
                                    ,@(when class `("class" ,class))
                                    ,@(when member `("member" ,member)))
                      (cider-nrepl-send-sync-request nil 'abort-on-input))))
    (if (member "no-eldoc" (nrepl-dict-get eldoc "status"))
        nil
      eldoc)))

(defun cider-sync-request:eldoc-datomic-query (symbol)
  "Send \"eldoc-datomic-query\" op with parameter SYMBOL."
  (when-let ((eldoc (thread-first `("op" "eldoc-datomic-query"
                                    "ns" ,(cider-current-ns)
                                    ,@(when symbol `("symbol" ,symbol)))
                      (cider-nrepl-send-sync-request nil 'abort-on-input))))
    (if (member "no-eldoc" (nrepl-dict-get eldoc "status"))
        nil
      eldoc)))

(defun cider-sync-request:spec-list (&optional filter-regex)
  "Get a list of the available specs in the registry.

Optional argument FILTER-REGEX filters specs.  By default, all specs are
returned."
  (setq filter-regex (or filter-regex ""))
  (thread-first `("op" "spec-list"
                  "filter-regex" ,filter-regex)
    (cider-nrepl-send-sync-request)
    (nrepl-dict-get "spec-list")))

(defun cider-sync-request:spec-form (spec)
  "Get SPEC's form from registry."
  (thread-first `("op" "spec-form"
                  "spec-name" ,spec)
    (cider-nrepl-send-sync-request)
    (nrepl-dict-get "spec-form")))

(defun cider-sync-request:spec-example (spec)
  "Get an example for SPEC."
  (thread-first `("op" "spec-example"
                  "spec-name" ,spec)
    (cider-nrepl-send-sync-request)
    (nrepl-dict-get "spec-example")))

(defun cider-sync-request:ns-list ()
  "Get a list of the available namespaces."
  (thread-first `("op" "ns-list"
                  "filter-regexps" ,cider-filtered-namespaces-regexps)
    (cider-nrepl-send-sync-request)
    (nrepl-dict-get "ns-list")))

(defun cider-sync-request:ns-vars (ns)
  "Get a list of the vars in NS."
  (thread-first `("op" "ns-vars"
                  "ns" ,ns)
    (cider-nrepl-send-sync-request)
    (nrepl-dict-get "ns-vars")))

(defun cider-sync-request:ns-path (ns)
  "Get the path to the file containing NS."
  (thread-first `("op" "ns-path"
                  "ns" ,ns)
    (cider-nrepl-send-sync-request)
    (nrepl-dict-get "path")))

(defun cider-sync-request:ns-vars-with-meta (ns)
  "Get a map of the vars in NS to its metadata information."
  (thread-first `("op" "ns-vars-with-meta"
                  "ns" ,ns)
    (cider-nrepl-send-sync-request)
    (nrepl-dict-get "ns-vars-with-meta")))

(defun cider-sync-request:ns-load-all ()
  "Load all project namespaces."
  (thread-first '("op" "ns-load-all")
    (cider-nrepl-send-sync-request)
    (nrepl-dict-get "loaded-ns")))

(defun cider-sync-request:resource (name)
  "Perform nREPL \"resource\" op with resource name NAME."
  (thread-first `("op" "resource"
                  "name" ,name)
    (cider-nrepl-send-sync-request)
    (nrepl-dict-get "resource-path")))

(defun cider-sync-request:resources-list ()
  "Return a list of all resources on the classpath."
  (thread-first '("op" "resources-list")
    (cider-nrepl-send-sync-request)
    (nrepl-dict-get "resources-list")))

(defun cider-sync-request:format-code (code)
  "Perform nREPL \"format-code\" op with CODE."
  (thread-first `("op" "format-code"
                  "code" ,code)
    (cider-nrepl-send-sync-request)
    (nrepl-dict-get "formatted-code")))

(defun cider-sync-request:format-edn (edn right-margin)
  "Perform \"format-edn\" op with EDN and RIGHT-MARGIN."
  (let* ((response (thread-first `("op" "format-edn"
                                   "edn" ,edn)
                     (append (cider--nrepl-pprint-request-plist right-margin))
                     (cider-nrepl-send-sync-request)))
         (err (nrepl-dict-get response "err")))
    (when err
      ;; err will be a stacktrace with a first line that looks like:
      ;; "clojure.lang.ExceptionInfo: Unmatched delimiter ]"
      (error (car (split-string err "\n"))))
    (nrepl-dict-get response "formatted-edn")))


;;; Connection info
(defun cider--java-version ()
  "Retrieve the underlying connection's Java version."
  (with-current-buffer (cider-current-connection "clj")
    (when nrepl-versions
      (thread-first nrepl-versions
        (nrepl-dict-get "java")
        (nrepl-dict-get "version-string")))))

(defun cider--clojure-version ()
  "Retrieve the underlying connection's Clojure version."
  (with-current-buffer (cider-current-connection "clj")
    (when nrepl-versions
      (thread-first nrepl-versions
        (nrepl-dict-get "clojure")
        (nrepl-dict-get "version-string")))))

(defun cider--nrepl-version ()
  "Retrieve the underlying connection's nREPL version."
  (with-current-buffer (cider-current-connection "clj")
    (when nrepl-versions
      (thread-first nrepl-versions
        (nrepl-dict-get "nrepl")
        (nrepl-dict-get "version-string")))))

(defun cider--connection-info (connection-buffer)
  "Return info about CONNECTION-BUFFER.

Info contains project name, current REPL namespace, host:port
endpoint and Clojure version."
  (with-current-buffer connection-buffer
    (format "%s%s@%s:%s (Java %s, Clojure %s, nREPL %s)"
            (upcase (concat cider-repl-type " "))
            (or (cider--project-name nrepl-project-dir) "<no project>")
            (car nrepl-endpoint)
            (cadr nrepl-endpoint)
            (cider--java-version)
            (cider--clojure-version)
            (cider--nrepl-version))))

(defun cider--connection-properties (conn-buffer)
  "Extract the essential properties of CONN-BUFFER."
  (with-current-buffer conn-buffer
    (list
     :type cider-repl-type
     :host (car nrepl-endpoint)
     :port (cadr nrepl-endpoint)
     :project-dir nrepl-project-dir)))

(defun cider--connection-type (conn-buffer)
  "Get CONN-BUFFER's type.

Return value matches `cider-repl-type'."
  (plist-get (cider--connection-properties conn-buffer) :type))

(defun cider--connection-host (conn-buffer)
  "Get CONN-BUFFER's host."
  (plist-get (cider--connection-properties conn-buffer) :host))

(defun cider--connection-port (conn-buffer)
  "Get CONN-BUFFER's port."
  (plist-get (cider--connection-properties conn-buffer) :port))

(defun cider--connection-project-dir (conn-buffer)
  "Get CONN-BUFFER's project dir."
  (plist-get (cider--connection-properties conn-buffer) :project-dir))

(defun cider-display-connection-info (&optional show-default)
  "Display information about the current connection.

With a prefix argument SHOW-DEFAULT it will display info about the
default connection."
  (interactive "P")
  (message "%s" (cider--connection-info (if show-default
                                            (cider-default-connection)
                                          (cider-current-connection)))))

(defun cider-rotate-default-connection ()
  "Rotate and display the default nREPL connection."
  (interactive)
  (cider-ensure-connected)
  (setq cider-connections
        (append (cdr cider-connections)
                (list (car cider-connections))))
  (message "Default nREPL connection: %s"
           (cider--connection-info (car cider-connections))))

(defun cider-replicate-connection (&optional conn)
  "Establish a new connection based on an existing connection.
The new connection will use the same host and port.
If CONN is not provided the user will be prompted to select a connection."
  (interactive)
  (let* ((conn (or conn (cider-read-connection "Select connection to replicate: ")))
         (host (cider--connection-host conn))
         (port (cider--connection-port conn))
         (project-dir (cider--connection-project-dir conn)))
    (cider-connect host port project-dir)))

(defun cider-extract-designation-from-current-repl-buffer ()
  "Extract the designation from the cider repl buffer name."
  (let ((repl-buffer-name (buffer-name (cider-current-repl-buffer)))
        (template (split-string nrepl-repl-buffer-name-template "%s")))
    (string-match (format "^%s\\(.*\\)%s"
                          (regexp-quote (concat (car template) nrepl-buffer-name-separator))
                          (regexp-quote (cadr template)))
                  repl-buffer-name)
    (or (match-string 1 repl-buffer-name) "<no designation>")))

(defun cider-change-buffers-designation (designation)
  "Change the DESIGNATION in cider buffer names.
Buffer names changed are cider-repl and nrepl-server."
  (interactive (list (read-string (format "Change CIDER buffer designation from '%s': "
                                          (cider-extract-designation-from-current-repl-buffer)))))
  (cider-ensure-connected)
  (let ((new-repl-buffer-name (nrepl-format-buffer-name-template
                               nrepl-repl-buffer-name-template designation)))
    (with-current-buffer (cider-current-repl-buffer)
      (rename-buffer new-repl-buffer-name)
      (when nrepl-server-buffer
        (let ((new-server-buffer-name (nrepl-format-buffer-name-template
                                       nrepl-server-buffer-name-template designation)))
          (with-current-buffer nrepl-server-buffer
            (rename-buffer new-server-buffer-name)))))
    (message "CIDER buffer designation changed to: %s" designation)))

(provide 'cider-client)

;;; cider-client.el ends here
