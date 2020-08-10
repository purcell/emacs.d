;; nu.el --- Nubank Emacs functions -*- lexical-binding: t; -*-

;; Copyright © 2019 Nubank

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Useful stuff to make everything work.

;; Put this in your .emacs.d file (or .spacemacs under
;; `dotspacemacs/user-config`, if you use Spacemacs)
;; to load this file:

;; (let ((nudev-emacs-path "~/dev/nu/nudev/ides/emacs/"))
;;   (when (file-directory-p nudev-emacs-path)
;;     (add-to-list 'load-path nudev-emacs-path)
;;     (require 'nu)))

;;; Code:

(require 'cl-lib)
(require 'nu-country)
(require 'nu-environment)

;; Add s/defn and s/defschema to `dumb-jump`, so go to definition improves
(require 'dumb-jump nil 'noerror)
(when (boundp 'dumb-jump-find-rules)
  (push '(:type "function"
                :supports ("ag" "grep" "rg" "git-grep")
                :language "clojure"
                :regex "\\(s\\/defn-?(\\s+\\^:[-a-z]+)*\\s+JJJ\\j")
        dumb-jump-find-rules)
  (push '(:type "function"
                :supports ("ag" "grep" "rg" "git-grep")
                :language "clojure"
                :regex "\\(defnk\\s+JJJ\\j")
        dumb-jump-find-rules)
  (push '(:type "type"
                :supports ("ag" "grep" "rg" "git-grep")
                :language "clojure"
                :regex "\\(s\\/defschema\\s+JJJ\\j")
        dumb-jump-find-rules))

(when (require 'exec-path-from-shell nil 'noerror)
  (exec-path-from-shell-copy-envs '("NU_HOME"
                                    "NUCLI_HOME"
                                    "HOME"
                                    "DOCKER_TLS_VERIFY"
                                    "DOCKER_HOST"
                                    "DOCKER_MACHINE_NAME"
                                    "DOCKER_CERT_PATH"
                                    "AWS_DEFAULT_REGION"
                                    "AWS_ACCESS_KEY"
                                    "AWS_SECRET_ACCESS_KEY"
                                    "AWS_ACCESS_KEY_ID"
                                    "AWS_SECRET_KEY"
                                    "ARTIFACTS_AWS_ACCESS_KEY_ID"
                                    "ARTIFACTS_AWS_SECRET_ACCESS_KEY")))

(defun num-prefix-args (arg)
  "Calculate how many times user added prefix arg from ARG.
See https://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Command-Arguments.html for more info."
  (assert (or (not arg) (zerop (mod (car arg) 4))) nil
          "Arg should be nil or a multiple of four inside a cons cell.")
  (cond
   ((not arg) 0)
   ((= (car arg) 4) 1)
   (t (+ 1 (num-prefix-args (list (/ (car arg) 4)))))))

(defun maybe-map-docker-path (docker-path docker-prefix new-prefix)
  "Try to map DOCKER-PATH by replacing DOCKER-PREFIX with NEW-PREFIX.
Return nil if not possible."
  (let ((pattern (concat "^\\(.*:\\)?" docker-prefix)))
    (when (string-match pattern docker-path)
      (let ((path-prefix (concat (match-string 1 docker-path) new-prefix)))
        (replace-regexp-in-string pattern path-prefix docker-path)))))

(defun docker-to-local-path (docker-path)
  "Try to map DOCKER-PATH to local directories."
  (with-current-buffer (cider-current-connection) ;; needed to get project's root
    (or (maybe-map-docker-path docker-path "/project/" nrepl-project-dir)
        (maybe-map-docker-path docker-path "/root/.m2/" (concat (getenv "HOME") "/.m2/"))
        docker-path)))

(add-hook 'cider-mode-hook (lambda ()
                             (defadvice cider-find-file (around nu-docker-cider-find-file)
                               "Expand filename and dir before calling 'cider-find-file'."
                               (ad-set-arg 0 (docker-to-local-path (ad-get-arg 0))) ;; expand filename
                               ad-do-it)
                             (ad-activate 'cider-find-file)))

(add-hook 'cider-mode-hook (lambda ()
                             (add-to-list 'cider-test-defining-forms "defflow" "defflow-loopback-false")))

(defun get-docker-machine-vars (machine-name)
  "Get env vars for MACHINE-NAME.  Return pairs of `(var-name . value)`."
  (with-temp-buffer
    (insert (shell-command-to-string (concat "docker-machine env " machine-name)))
    (keep-lines "^export " (point-min) (point-max) nil)
    (goto-char (point-min))
    (while (re-search-forward "\\(export \\|\"\\)" nil t)
      (replace-match ""))
    (mapcar (lambda (s) (apply #'cons (split-string s "=" t)))
            (split-string (buffer-string) "\n" t))))

(defun docker-machine-env (machine-name)
  "When using docker-machine, activate dockermachine env for MACHINE-NAME."
  (interactive "sMachine name: ")
  (dolist (kv (get-docker-machine-vars machine-name))
    (setenv (car kv) (cdr kv)))
  (message "Activated docker-machine env"))

(defun ip-from-url (u)
  "Get ip from an url U."
  (when (string-match "[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+" u)
    (match-string 0 u)))

(defun nu-cider-connect (port)
  "Connect to dockerized lein repl in PORT."
  (interactive "sPort: ")
  (cider-connect (ip-from-url (getenv "DOCKER_HOST"))
                 port))
;; nu indentation for tests

(defun set-nu-clj-indent ()
  "Define custom indent for our macros.

How does this work?

The definition follows cider's indent spec: https://cider.readthedocs.io/en/latest/indent_spec/
Common arguments are (in short):
- 'defun   = every argument in the first line is 'special', all the rest is part of the body
- A number = number of arguments that are 'special' (not part of the body)

Being part of the body means:
- If there are no 'non-special' arguments in the first line, align using default settings (usually 2 spaces)
- If there are 'non-special' arguments in the first line, align below the first 'non-special' argument

Tip: most of the time you want 'defun
"
  (define-clojure-indent
    (against-background 'defun)
    (alet 'defun)
    (as-> 1)
    (as-customer 1)
    (as-of 1)
    (constraint-fn 'defun)
    (data-fn 'defun)
    (defflow 'defun)
    (defflow-loopback-false 'defun)
    (fact 'defun)
    (facts 'defun)
    (flow 'defun)
    (for-all 'defun)
    (future-fact 'defun)
    (let-entities 'defun)
    (log-messages 'defun)
    (match? 'defun)
    (mlet 'defun)
    (provided 'defun)
    (providing 'defun)
    (request-context 'defun)
    (tabular 'defun)
    (tabular-flow 'defun)
    (verify 'defun)))

(eval-after-load 'clojure-mode
  '(set-nu-clj-indent))

(eval-after-load 'midje-mode
  '(set-nu-clj-indent))

(defun nu-mordor-make-query-file (query)
  "Save QUERY to a temporary file.
Returns the file path."
  (let ((fname (make-temp-file "nu-mor-query" nil ".edn")))
    (with-temp-file fname
      (insert query))
    fname))

(defun nu-mordor-make-buffer (accept-type)
  "Create buffer to display result from mordor query according to ACCEPT-TYPE."
  (let ((buff (get-buffer-create "*nu-mordor*")))
    (with-current-buffer buff
      (erase-buffer)
      (cond ((string= accept-type "application/edn")
             (clojure-mode))
            ((and (string= accept-type "application/json") (require 'json-mode nil 'noerror))
             (json-mode))
            (t (fundamental-mode))))
    buff))

(defconst nu-mordor-content-types '("application/edn"
                                    "application/json"
                                    "application/transit+json"
                                    "text/csv"))

(defconst nu-mordor-query-default-args
  `((prototype . "s0")
    (env . ,(nu-environment-id (nu-environment-by-id "prod")))
    (accept . ,(car nu-mordor-content-types))))

(defvar *nu-mordor-query-args-cache* nil)

(defun nu-mordor-query-args (&optional arg)
  "Request arguments for querying.
ARG is the prefix arg.  If no prefix arg is present, will use the
values cached in `*nu-mordor-query-args*'.  If one prefix arg is
present, will ask everything again."
  (let* ((base-args (or *nu-mordor-query-args-cache*
                        nu-mordor-query-default-args))
         (default-prototype (alist-get 'prototype base-args))
         (default-env (alist-get 'env base-args))
         (default-content-type (alist-get 'accept base-args)))
    (setf *nu-mordor-query-args-cache*
          (if (or (not *nu-mordor-query-args-cache*)
                  (>= (num-prefix-args arg) 1))
              (append `((prototype . ,(read-string (format "Prototype (default %s): " default-prototype)
                                                   nil nil default-prototype))
                        (env . ,(nu-environment-completing-read-id default-env))
                        (accept . ,(completing-read (format "Accept (default %s): " default-content-type)
                                                    nu-mordor-content-types nil t nil nil
                                                    default-content-type))))
            base-args))))

(defun nu-mordor-get-query-string ()
  "Get query string based on context.
If mark is active, will consider the marked region as the query
string.  Returns the defun at point, according to CIDER."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (cider-defun-at-point)))

(defun nu-mordor-query (&optional arg)
  "Send current top sexp to mordor.
ARG is the prefix argument.  If called with two prefix arguments,
will print the raw return from mordor.  For other behaviors
related to prefix arguments, see `nu-mordor-query-args' for more
info."
  (interactive "P")
  (require 'cider)
  (let* ((args (nu-mordor-query-args arg))
         (prototype (alist-get 'prototype args))
         (env (alist-get 'env args))
         (accept (alist-get 'accept args))
         (fname (nu-mordor-make-query-file (nu-mordor-get-query-string)))
         (mordor-cmd (format "nu mordor query --env %s --accept %s %s %s" env accept prototype fname))
         (result-buffer (nu-mordor-make-buffer accept))
         (dbg-mode (>= (num-prefix-args arg) 2))
         (shell-cmd-format (if dbg-mode "%s 2>&1" "%s 2>&1 | tail -n +2")))
    (message (format "Nu mordor query command: %s" mordor-cmd))
    (with-current-buffer result-buffer
      (call-process shell-file-name nil t t
                    shell-command-switch
                    (format shell-cmd-format mordor-cmd))
      (switch-to-buffer result-buffer)
      (cond ((and (string= accept "application/json") (require 'json-mode nil 'noerror))
             (json-pretty-print-buffer))
            ((string= accept "application/edn")
             (cider-format-edn-buffer))
            (t (fundamental-mode))))))

(defun nu-mordor-insert-template ()
  "Insert template for a query."
  (interactive)
  (call-process shell-file-name nil t t
                shell-command-switch "nu mordor template"))
(defvar *nu-mordor-schema-request-cache* nil)

(setq mordor-schema-cmd "nu mordor schema s0")

(defun nu-list-sample-entities (db-name attribute)
  (require 'json-mode nil 'noerror)
  (let* ((sample-entities-query (nu--query-entities-samples db-name attribute))
         (query-temp-file (nu-mordor-make-query-file sample-entities-query))
         (result-buffer (nu-mordor-make-buffer "application/json"))
         (shard "s0"))
    (switch-to-buffer result-buffer)
    (message (format "Querying for entities with %s in %s..." attribute shard))
    (insert (shell-command-to-string (format "nu mordor query %s --accept application/json %s" shard query-temp-file)))
    (json-pretty-print-buffer)
    (goto-char (point-min))))

(cl-defun nu--make-query
    (&key (find-elems "") (with-clause "")
          (inputs-clause "") (where-clauses "") (args-query ""))
  (format
   "{:query\n\
  {:find  [%s]\n\
    :with  [%s]\n\
    :in    [%s]\n\
    :where [%s]}\n\
    :args [%s]}" find-elems with-clause
   inputs-clause where-clauses args-query))

(defun nu--make-clojure-buffer (clojure-buffer-name)
  (let ((buff (get-buffer-create clojure-buffer-name)))
    (with-current-buffer buff
      (clojure-mode))
    buff))

(defun nu--query-entities-samples (db-name attribute)
  "Returns a string that represents a Datomic query
that gets some entities with `attribute`"
  (let* ((find-clause "(sample 10 ?entity)")
         (pulled-entity-var-name "?entity")
         (where-clause-attribute (format "[%s ?e %s _]" db-name attribute))
         (where-clause-pull-entity (format "[(datomic.api/pull %s (quote [*]) ?e) ?entity]" db-name)))
    (nu--make-query :find-elems find-clause :where-clauses (concat where-clause-attribute where-clause-pull-entity))))

(defun nu--query-attribute-query (db-name attribute)
  "Returns a string that represents a Datomic query
with a where clause for `attribute`."
  (let* ((entity-var-name "?e")
         (pull-expr (format "(pull %s %s [*])" db-name entity-var-name))
         (value-var-name "?value")
         (where-clause (format "[%s %s %s %s]" db-name entity-var-name attribute value-var-name)))
    (nu--make-query :find-elems pull-expr :where-clauses where-clause)))

(defun nu--query-attribute (db-name attribute)
  "Creates and switches to a buffer that has
a query for `attribute`."
  (let* ((query-str (nu--query-attribute-query db-name attribute))
         (nu-query-buffer-name (generate-new-buffer-name (concat "nu-mordor-query-attribute-" attribute)))
         (result-buffer (nu--make-clojure-buffer nu-query-buffer-name)))
    (switch-to-buffer result-buffer)
    (insert query-str)
    (goto-char (point-min))))

(setq nu--mordor-schema-completing-options '((samples . "list samples")
                                             (insert-query . "insert query")))

(defun nu--modor-schema-options-completions ()
  (mapcar 'cdr nu--mordor-schema-completing-options))

(defun nu-mordor-schema ()
  "List all Datomic database schemas."
  (interactive)
  (when-let ((parsed-response (or *nu-mordor-schema-request-cache* (nu-mordor--schema-request))))
    (let* ((attribute-line (thread-last
                               parsed-response
                             (nu-modor--flatten-json-schema)
                             (nu-mordor--schema-format-entries)
                             (completing-read "Schema: ")))
           (split-selected-result (split-string attribute-line " " t))
           (attribute-name (second split-selected-result))
           (db-name (concat "$" (first split-selected-result)))
           (attribute-keyword (concat ":" attribute-name))
           (action-str (completing-read "action: " (nu--modor-schema-options-completions)))
           (action (car (rassoc action-str nu--mordor-schema-completing-options))))
      (cond ((eq 'samples action)
             (nu-list-sample-entities db-name attribute-keyword))
            ((eq 'insert-query action)
             (nu--query-attribute db-name attribute-keyword))))))

(defun nu-mordor--schema-request ()
  (message "Getting schemas...")
  (condition-case err
      (let* ((response (shell-command-to-string mordor-schema-cmd)))
        (setq *nu-mordor-schema-request-cache* (json-read-from-string response)))
    (error (message (format "Can't get schemas: %s" err))
           *nu-mordor-schema-request-cache*)))

(defun nu-modor--flatten-json-schema (json)
  (require 'dash)
  (-flatten-n 1 (mapcar (lambda (db-and-its-attrs)
                          (let* ((db (car db-and-its-attrs))
                                 (attributes (cdr db-and-its-attrs)))
                            (mapcar (lambda (attr)
                                      (push `(database . ,(symbol-name db)) attr))
                                    attributes)))
                        json)))

(defun nu-mordor--schema-format-entries (entries)
  (let* ((max-db-length (seq-reduce #'max
                                    (mapcar (lambda (i) (length (alist-get 'database i)))
                                            entries)
                                    0)))
    (mapcar (lambda (entry)
              (let* ((db-value (alist-get 'database entry))
                     (name-value (alist-get 'name entry))
                     (cardinality-value (alist-get 'cardinality entry))
                     (type-value (alist-get 'type entry))
                     (format-str (concat "%-" (number-to-string max-db-length) "s  %-50s   %-20s   %s")))
                `(,(format format-str
                           (nu-mordor--schema-format-db db-value)
                           name-value
                           cardinality-value
                           type-value))))
            entries)))

(defun nu-mordor--schema-format-db (str)
  (propertize
   ;; remove $ from db name
   (substring str 1 (length str))
   'face 'compilation-info))

(defvar *nu-mordor-list-queries-request-cache* nil)
(setq mordor-list-queries-cmd "nu ser curl GET s0 mordor /api/admin/queries")

(defun nu--mordor-query-completions (queries)
  (mapcar (lambda (elem)
            (let* ((email-and-query-name (alist-get 'name elem))
                   (split-name (s-split-up-to "\\/" email-and-query-name 1))
                   (email (car split-name))
                   (username (car (split-string email "@")))
                   (query-name (cadr split-name)))
              (concat
               username
               " "
               (propertize query-name 'face '(bold default)))))
          queries))

(defun nu--mordor-query-make-hash-table (queries)
  "Creates a hash table from QUERIES whose keys are the name of the queries
(in the format email@nubank.com.br/query-name) and the values are the queries."
  (let* ((queries-hash (make-hash-table :test 'equal)))
    (mapc
     (lambda (i) (puthash (alist-get 'name i) i queries-hash))
     queries)
    queries-hash))

(defun nu--mordor-query-get-key-from-completion (completion)
  "Given the minibuffer selection from COMPLETION, gets the key
to be searched in a hashtable that contains all queries."
  (let* ((user-and-query-name (s-split-up-to " " completion 1))
         (username (car user-and-query-name))
         (query (cadr user-and-query-name))
         (email (concat username "@nubank.com.br")))
    (concat email "/" query)))

(defun nu--mordor-list-queries-request ()
  (condition-case err
      (let* ((response (shell-command-to-string mordor-list-queries-cmd)))
        (setq *nu-mordor-list-queries-request-cache* (alist-get 'queries (json-read-from-string response))))
    (error (message (format "Can't get list of queries: %s" err))
           *nu-mordor-list-queries-request-cache*)))

(defun nu-mordor-list-queries ()
  (interactive)
  (or (require 's nil 'noerror) (message "Please install s.el"))
  (let* ((response (or *nu-mordor-list-queries-request-cache* (nu--mordor-list-queries-request)))
         (completions (nu--mordor-query-completions response))
         (completions-hash-table (nu--mordor-query-make-hash-table response))
         (chosen-str (completing-read "queries: " completions))
         (query (gethash (nu--mordor-query-get-key-from-completion chosen-str) completions-hash-table))
         (query-name (alist-get 'name query))
         (query-buffer-name (generate-new-buffer-name (concat "nu-mordor-query " query-name))))
    (with-current-buffer (get-buffer-create query-buffer-name)
      (erase-buffer)
      (clojure-mode)
      (insert (alist-get 'user-input query))
      (switch-to-buffer query-buffer-name)
      (goto-char (point-min)))))

(setq uuid-regex "[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}")

(defun nu-uuid-bounds-of-uuid-at-point ()
  "Return the start and end points of an uuid at the current point."
  (interactive)
  (save-excursion
    (skip-chars-backward "[:alnum:]-")
    (if (looking-at uuid-regex)
        (cons (point) (match-end 0))
      nil)))

(put 'uuid 'bounds-of-thing-at-point
     'nu-uuid-bounds-of-uuid-at-point)

(defun nu-open-customer-at-point (arg)
  "Opens person view for person-id (either customer or prospect id) at point.
On Linux, uses the $BROWSER env var, so it must be set.
If called with prefix argument, use staging."
  (interactive "P")
  (let* ((customer-id-at-point (thing-at-point 'uuid))
         (cmd (concat "nu person open " (if arg "--env staging " "") customer-id-at-point)))
    (if customer-id-at-point
        (shell-command cmd)
      (message "Can't find id at point."))))

(defun nu-open-account-at-point (arg)
  "Opens person view for account-id at point.
On Linux, uses the $BROWSER env var, so it must be set.
If called with prefix argument, use staging."
  (interactive "P")
  (let* ((account-id-at-point (thing-at-point 'uuid))
         (cmd (concat "nu person open " (if arg "--env staging " "") " --account-id " account-id-at-point)))
    (if account-id-at-point
        (shell-command cmd)
      (message "Can't find id at point."))))

;; general

(defun nu-service-p (project-name)
  (member project-name (nu-services-list)))

(defun nu-services-list ()
  ;; TODO: check if definition exists
  (let* ((cmd (concat "ls $NU_HOME/definition/resources/br/services"))
         (output-cmd (shell-command-to-string cmd))
         (split-output (split-string output-cmd "\n")))
    (mapcar (lambda (file-name)
              (file-name-sans-extension file-name))
            split-output)))

(defun nu--input-current-project ()
  "List all nu projects and pre-select the current cached one, if any."
  (let* ((services (nu-services-list))
         (current-project-name (projectile-project-name)))
    (completing-read "Service: " services nil nil (if (member current-project-name services)
                                                      current-project-name
                                                    nil))))

(defun nu--insert-in-shell (str)
  (shell)
  (goto-char (point-max))
  (insert str))

;; nu-routes

(defun nu--routes-format-method (method)
  (pcase method
    ("get" (propertize method 'face '(:foreground "blue")))
    ("delete" (propertize method 'face '(:foreground "red")))
    ("post" (propertize method 'face '(:foreground "purple")))
    ("put" (propertize method 'face '(:foreground "pink")))
    (_ method)))

(defun nu--routes-format-completions (lines)
  (mapcar (lambda (elem)
            (let* ((split-response (split-string elem ","))
                   (url (car split-response))
                   (method (cadr split-response))
                   (scopes (car (cdr (cdr split-response)))))
              (format "%s %s %s"
                      (propertize url 'face '(bold default))
                      (nu--routes-format-method method)
                      (if scopes
                          (propertize scopes 'face '(font-lock-comment-face default))
                        ""))))
          lines))

(defun nu-routes ()
  "List all routes and scopes of a service"
  (interactive)
  (or (require 's nil 'noerror) (message "Please install s.el"))
  (let* ((project-name (nu--input-current-project))
         (cmd (concat "nu ser routes " project-name))
         (output-cmd (shell-command-to-string cmd))
         (split-output (split-string output-cmd "\n"))
         (fmt-result (nu--routes-format-completions (seq-remove 's-blank? split-output)))
         (input-line (completing-read "Routes: " fmt-result))
         (split-input-line (split-string input-line " "))
         (url (car split-input-line))
         (method (cadr split-input-line)))
    (nu--insert-in-shell (format "nu ser curl %s s0 %s %s | jq" method project-name url))))

;; nu-code-search

(defun nu-code-search (begin end)
  "Search for the given input on LiveGrep. Default input is the active region."
  (interactive "r")
  (let* ((default-str (if (use-region-p)
                          (buffer-substring-no-properties begin end)
                        ""))
         (str-search (read-string "search: " default-str)))
    (browse-url (format "https://codesearch.nubank.com.br/search?q=%s" str-search))))

;; nu-splunk-search

(defun nu-splunk-search-fmt-search-str (service staging-p search-str)
  "Formats the splunk search given SERVICE and SEARCH-STR, both which might be nil."
  (let ((nu-service? (nu-service-p (projectile-project-name)))
        (index (if staging-p "staging" "main")))
    (cond
     ((and nu-service? search-str)
      (format "index=%s source=%s %s" index service search-str))

     ((and nu-service? (not search-str))
      (format "index=%s source=%s " index service))

     ((and (not nu-service?) search-str)
      (format "index=%s %s " index search-str))

     ((and (not nu-service?) (not search-str))
      (format "index=%s " index)))))


(defun nu-splunk-search-str (query-str earliest-date)
  (let* ((splunk-base-url "https://nubank.splunkcloud.com/en-US/app/search/search?earliest=%s&q=%s")
         (splunk-url (format splunk-base-url earliest-date query-str)))
    (browse-url splunk-url)))

(defun nu-splunk-search-region (begin end arg)
  "Searches in Splunk. Default input is the active region. If current buffer
belongs to a Nubank project, adds source to the input.
If called with prefix argument, use index=staging."
  (interactive "r\nP")
  (let* ((region-or-nil (if (use-region-p)
                            (buffer-substring-no-properties begin end)
                          nil))
         (search-str (nu-splunk-search-fmt-search-str (projectile-project-name) arg region-or-nil))
         (input-str (read-string "search: " search-str))
         (earliest-date (completing-read "Time range: " '("@d" "-15m" "-60m" "-4h" "-24h" "-7d" "-30d"))))
    (nu-splunk-search-str input-str earliest-date)))

(defun nu-open-gocd-pipeline ()
  (interactive)
  (let* ((project-name (nu--input-current-project))
         (gocd-url (concat "https://go.nubank.com.br/go/pipelines#!/" project-name)))
    (browse-url gocd-url)))

;; nu-list-cli-commands

(setq nu--available-commands-cmd "nu --available-commands")

(defun nu--list-cli-commands-completions (out-contexts)
  (require 'dash)
  (thread-last
      out-contexts
    (-partition-before-pred (lambda (line)
                              (string-match-p "^[^|]" line)))
    (-mapcat (lambda (partition)
               (-let [(context-name . context-commands) partition]
                 (or (--map (->> (concat context-name it)
                                 (replace-regexp-in-string "|--" " "))
                            context-commands)
                     partition))))))

(defun nu-list-cli-commands ()
  "Lists all nu cli commands. Opens a shell with the chosen command after selection."
  (interactive)
  (let* ((output-cmd (shell-command-to-string nu--available-commands-cmd))
         (split-output (split-string output-cmd "\n"))
         (selected-command (completing-read "commands: "
                                            (nu--list-cli-commands-completions split-output))))
    (nu--insert-in-shell (format "nu %s" selected-command))))

(defun nu-refresh-token ()
  "Run `nu auth refresh-token` in the chosen env."
  (interactive)
  (let ((env (completing-read "Env:" '("prod" "staging"))))
    (shell-command (format "nu auth refresh-token --env %s" env))))

(defun nu-list-scopes ()
  "Lists all scopes (equivalent to `nu security scope show`)"
  (interactive)
  (shell-command "nu security scope show $(cat $NU_HOME/.nu/about/me/iam_user) --env prod"))

(with-eval-after-load 'cider
  (setf cider-test-defining-forms (append cider-test-defining-forms '("defflow" "defflow-loopback-false"))))

(provide 'nu)
;;; nu.el ends here
