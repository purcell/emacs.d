;;; httprepl.el --- An HTTP REPL  -*- lexical-binding: t -*-

;; Author: Greg Sexton <gregsexton@gmail.com>
;; Version: 1.0
;; Package-Version: 20141101.1034
;; Keywords: http, repl
;; URL: https://github.com/gregsexton/httprepl.el
;; Package-Requires: ((s "1.9.0") (dash "2.5.0") (emacs "24"))

;; The MIT License (MIT)

;; Copyright (c) 2014 Greg Sexton

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;; Provides an interactive interface for making HTTP
;; requests. Inspiration was drawn from looking at the ielm
;; source. comint.el and url or curl do much of the heavy lifting and
;; their respective variables will control some of the behaviour of
;; httprepl.

;;; Code:

(require 'comint)
(require 'dash)
(require 's)
(require 'url)

;;; customisation and variables

(defcustom httprepl-buffer-name "*httprepl*"
  "Name of the buffer httprepl will look for or create on
startup."
  :type 'string
  :group 'httprepl)

(defcustom httprepl-response-buffer-name "*http-response*"
  "Name of the buffer httprepl will use when opening responses."
  :type 'string
  :group 'httprepl)

(defcustom httprepl-prompt "> "
  "Prompt used by httprepl."
  :type 'string
  :group 'httprepl)

(defcustom httprepl-curl-exec (executable-find "curl")
  "Path to the curl executable. Ensure this is set correctly when
using the curl backend."
  :type 'string
  :group 'httprepl)

(defcustom httprepl-curl-args '("-isS")
  "List of args supplied to curl when executing requests. There
is no need to escape arguments for a shell. Just add to this
list. Execute a request with a prefix to alter this for a single
request."
  :type 'sexp
  :group 'httprepl)

(defcustom httprepl-url-middleware '(url-encode-url)
  "Functions applied to a request url in sequence. Each function
should take the url and return the transformed url. You could
make use of this to add custom signing logic for example or
whatever really."
  :type 'sexp
  :group 'httprepl)

(defcustom httprepl-response-middleware
  '(httprepl-apply-content-type-middleware httprepl-comment-headers)
  "Functions applied to a response buffer in sequence. Each
function should take the buffer and return the buffer, after
manipulating it as desired. For example, you may wish to add
`httprepl-delete-headers' to this list if you do not wish to see
the headers."
  :type 'sexp
  :group 'httprepl)

(defcustom httprepl-content-type-alist
  '(("text/html"              . html)
    ("application/json"       . js)
    ("application/javascript" . js)
    ("text/xml"               . xml)
    ("text/plain"             . text)
    ("application/xml"        . xml)
    ("html"                   . html)
    ("json"                   . js)
    ("javascript"             . js)
    ("xml"                    . xml)
    ("text"                   . text))
  "alist mapping content-type values to keys to be looked up in
`httprepl-content-type-middleware-alist'. Defined in descending
precedence order. If the function
`httprepl-apply-content-type-middleware' is in
`httprepl-response-middleware', then this list is consulted to work
out which functions to apply to the request buffer. Each entry
should be of the form (REGEX . KEY)."
  :type 'sexp
  :group 'httprepl)

(defcustom httprepl-content-type-middleware-alist
  '((html . ((lambda (b) (html-mode) b)))
    (js   . ((lambda (b) (js-mode) b)))
    (xml  . ((lambda (b) (xml-mode) b)))
    (text . ((lambda (b) (text-mode) b))))
  "alist mapping a content-type key - indirectly looked up using
`httprepl-content-type-alist' - to a list of functions to apply
to a response buffer. Each function should take a buffer,
manipulate it as desired and then return the buffer for the next
function to work with. Add any manipulation functions you like
here. This is where you should add a pretty-printing function for
example."
  :type 'sexp
  :group 'httprepl)

(defcustom httprepl-backend (if httprepl-curl-exec 'curl 'url)
  "Symbol specifying the evaluation backend. Choices are 'url or
'curl. 'url will use the built in url package and 'curl will use
the binary. Ensure `httprepl-curl-exec' is correctly set when
using 'curl. This should default to 'curl when the binary can be
found and this is the recommended backend."
  :type 'symbol
  :group 'httprepl)

(defvar httprepl-header "*** Welcome to HTTP REPL ***")

;;; utils

(defun httprepl-apply-middleware (middleware input)
  (-reduce-from (lambda (acc ware) (funcall ware acc))
                input
                middleware))

(defun httprepl-find-headers-end (buffer)
  (save-excursion
    (with-current-buffer buffer
      (goto-char (point-min))
      (+ 1 (re-search-forward "^$")))))

(defun httprepl-get-content-type (buffer)
  (save-excursion
    (with-current-buffer buffer
      (goto-char (point-min))
      (-when-let (header-val (re-search-forward
                              "^[[:space:]]*Content-Type[[:space:]]*:[[:space:]]*"
                              (httprepl-find-headers-end buffer) t))
        (buffer-substring-no-properties header-val (point-at-eol))))))

;;; lexer

(defun httprepl-get-token (input)
  (let ((rexps '((http-method . "\\(GET\\|POST\\|PUT\\|DELETE\\|OPTIONS\\|HEAD\\|TRACE\\|CONNECT\\|PATCH\\)")
                 (header-sep . ":")
                 (newline . "\n")
                 (ws . "[ \t]+")
                 (token . "[^ \t\n:]+")
                 (err . ".+"))))
    (car
     (-drop-while 'null
                  (-map (lambda (rx)
                          (-when-let (m (s-match (s-concat "\\`" (cdr rx)) input))
                            (cons (car rx) (car m))))
                        rexps)))))

(defun httprepl-tokenize (input)
  (let (token tokens)
    (while (setq token (httprepl-get-token input))
      (push token tokens)
      (setq input (substring input (length (cdr token)) nil)))
    (reverse tokens)))

;;; parser combinator support

(defun httprepl-p-prim-parser (err f p)
  "Primitive used to create parsers. ERR should be a function
taking a token and returning an error string. F is a function
that manipulates state, taking a state and token and returning a
state. P is a predicate, takes a token and returns a truthy value
if it is to be consumed falsey otherwise. If the predicate
returns the symbol 'ignore', the parser will be deemed successful
but will not advance the token stream."
  (lambda (tokens state)
    (let* ((token (car tokens))
           (new-state (funcall f state token))
           (success (funcall p token)))
      (if success
          (list (if (equal success 'ignore) tokens (cdr tokens))
                new-state)
        (funcall err token)))))

(defun httprepl-p-error-p (result)
  "Check if the result of a parser was an error."
  (stringp result))

(defun httprepl-p-get-state (result)
  (cadr result))

(defun httprepl-p-get-remaining-tokens (result)
  (car result))

(defun httprepl-p-token (test-token &optional f)
  (httprepl-p-prim-parser
   (lambda (token)
     (format "Parse error - not expecting token: %s" (cdr token)))
   (lambda (old-state token)
     (if f (funcall f old-state token)
       old-state))
   (lambda (token)
     (equal test-token (car token)))))

(defun httprepl-p-state (x)
  (httprepl-p-prim-parser
   (lambda (token) "")
   (lambda (old-state token) x)
   (lambda (token) 'ignore)))

(defun httprepl-p-seq (&rest parsers)
  (-reduce-r (lambda (parser acc)
               (lambda (tokens state)
                 (let ((result (funcall parser tokens state)))
                   (if (httprepl-p-error-p result)
                       result
                     (apply #'funcall acc result)))))
             parsers))

(defun httprepl-p-choice (&rest parsers)
  (-reduce (lambda (acc parser)
             (lambda (tokens state)
               (let ((result (funcall acc tokens state)))
                 (if (httprepl-p-error-p result)
                     (funcall parser tokens state)
                   result))))
           parsers))

(defun httprepl-p-true ()
  (httprepl-p-prim-parser (lambda (token) "")
                          (lambda (old-state token) old-state)
                          (lambda (token) 'ignore)))

(defun httprepl-p-optional (parser)
  (httprepl-p-choice parser (httprepl-p-true)))

(defun httprepl-p-0+ (parser)
  ;; this was originally recursive and built out of optional and seq
  ;; but hits the stack size limit quickly. Not sure if elisp has
  ;; tail-recursion optimization and/or lazy evaluation?
  (lambda (tokens state)
    (let ((prev-result (list tokens state))
          (result (funcall parser tokens state)))
     (while (not (httprepl-p-error-p result))
       (setq prev-result result)
       (setq result (apply 'funcall parser result)))
     prev-result)))

(defun httprepl-p-1+ (parser)
  (httprepl-p-seq parser (httprepl-p-0+ parser)))

(defun httprepl-p-comp (parser f &optional initial-state)
  "Compose a parser into a composite. INITIAL-STATE is the state
passed to PARSER. F is a function that takes the currently
accumulated state and the output state of PARSER and produces a
new state."
  (lambda (tokens state)
    (let ((result (funcall parser tokens initial-state)))
      (if (httprepl-p-error-p result) result
        (list (httprepl-p-get-remaining-tokens result)
              (funcall f state (httprepl-p-get-state result)))))))

;;; parser

(defun httprepl-parse (tokens)
  (let* ((concat-token-val (lambda (old-state token)
                             (s-concat old-state (cdr token))))
         (build-assoc (lambda (key)
                        (lambda (acc-state state) (cons (cons key state) acc-state))))
         (anything (httprepl-p-seq
                    (httprepl-p-state "")
                    (httprepl-p-1+
                     (httprepl-p-choice
                      (httprepl-p-token 'http-method concat-token-val)
                      (httprepl-p-token 'header-sep  concat-token-val)
                      (httprepl-p-token 'ws          concat-token-val)
                      (httprepl-p-token 'token       concat-token-val)))))
         (header (httprepl-p-seq
                  (httprepl-p-token 'token (lambda (old-state token) (cdr token)))
                  (httprepl-p-optional (httprepl-p-token 'ws))
                  (httprepl-p-token 'header-sep)
                  (httprepl-p-optional (httprepl-p-token 'ws))
                  (httprepl-p-comp anything (lambda (acc-state state)
                                              (cons acc-state state)))))
         (headers (httprepl-p-seq
                   (httprepl-p-state '())
                   (httprepl-p-0+ (httprepl-p-seq
                                   (httprepl-p-token 'newline)
                                   (httprepl-p-comp header (lambda (acc-state state)
                                                             (cons state acc-state)))))))
         (entity (httprepl-p-optional
                  (httprepl-p-seq
                   (httprepl-p-state "")
                   (httprepl-p-token 'newline)
                   (httprepl-p-token 'newline)
                   (httprepl-p-0+ (httprepl-p-choice
                                   (httprepl-p-comp anything (lambda (acc-state state)
                                                               (s-concat acc-state state)))
                                   (httprepl-p-token 'newline concat-token-val))))))
         (request (httprepl-p-seq
                   (httprepl-p-token 'http-method
                                     (lambda (old-state token)
                                       (cons (cons 'method (cdr token)) old-state)))
                   (httprepl-p-token 'ws)
                   (httprepl-p-comp anything (funcall build-assoc 'url))
                   (httprepl-p-comp headers (funcall build-assoc 'headers))
                   (httprepl-p-comp entity (funcall build-assoc 'entity)))))
    (funcall request tokens '())))

;;; reader

(defun httprepl-read (input)
  (let ((result (httprepl-parse (httprepl-tokenize input))))
    (if (httprepl-p-error-p result) result
      (let ((tokens (httprepl-p-get-remaining-tokens result)))
        (if tokens "Parse error - could not consume all tokens"
          (httprepl-p-get-state result))))))

;;; evaluator

(defun httprepl-eval-curl-header-args (headers)
  (-mapcat (lambda (header)
             (list "-H" (s-concat (car header) ":" (cdr header))))
           headers))

(defun httprepl-eval-curl-args (method url headers entity)
  (let ((args (-concat httprepl-curl-args
                       (list "-X" method)
                       (httprepl-eval-curl-header-args headers)
                       (when entity
                         (list "-d" entity))
                       (list url))))
    (if current-prefix-arg
        (read-from-minibuffer "curl args: " (prin1-to-string args) nil t)
      args)))

(defun httprepl-insertion-filter (buffer)
  (lambda (proc string)
    (with-current-buffer buffer
      (httprepl-insert string))))

(defun httprepl-eval-curl (method url headers entity)
  (let* ((args (httprepl-eval-curl-args method url headers entity))
         (process (apply 'start-process "httprepl-curl" nil httprepl-curl-exec args)))
    (set-process-filter process (httprepl-insertion-filter (current-buffer)))
    ;; print a new prompt once process has finished
    (set-process-sentinel process (lambda (proc event)
                                    (when (not (process-live-p proc))
                                      (httprepl-print ""))))
    nil))

(defun httprepl-eval-url-callback (buffer)
  (lambda (status)
    (let ((response (buffer-string)))
      (with-current-buffer buffer
        (httprepl-print response)))))

(defun httprepl-eval-url (method url headers entity)
  (let ((url-request-data entity)
        (url-request-method method)
        (url-request-extra-headers headers))
    (url-retrieve url (httprepl-eval-url-callback (current-buffer)))
    nil))

(defun httprepl-eval-dispatch (&rest args)
  (apply (if (eq httprepl-backend 'url)
             'httprepl-eval-url 'httprepl-eval-curl) args))

(defun httprepl-eval (expr)
  (if (httprepl-p-error-p expr) expr
    (let* ((url (httprepl-apply-middleware
                 httprepl-url-middleware (cdr (assoc 'url expr))))
           (method (s-upcase (cdr (assoc 'method expr))))
           (headers (cdr (assoc 'headers expr)))
           (entity (cdr (assoc 'entity expr))))
      (httprepl-eval-dispatch method url headers entity))))

;;; open response

(defmacro httprepl-response-middleware (&rest body)
  "Assumes expands in an environment with a bound var 'buffer'."
  `(save-excursion
     (with-current-buffer buffer
       ,@body)
     buffer))

(defun httprepl-delete-headers (buffer)
  (httprepl-response-middleware
   (kill-region (point-min) (httprepl-find-headers-end buffer))))

(defun httprepl-comment-headers (buffer)
  (httprepl-response-middleware
   (comment-region (point-min) (httprepl-find-headers-end buffer))))

(defun httprepl-apply-content-type-middleware (buffer)
  (-when-let (content-type (httprepl-get-content-type buffer))
    (-> httprepl-content-type-alist
      (->> (-first (lambda (alist) (s-matches-p (car alist) content-type))))
      cdr
      (assoc httprepl-content-type-middleware-alist)
      cdr
      (httprepl-apply-middleware buffer))))

(defun httprepl-get-response ()
  (let ((proc (get-buffer-process (current-buffer))))
    (save-excursion
      (let ((pmark (progn (goto-char (process-mark proc))
                          (forward-line -1)
                          (end-of-line)
                          (point-marker))))
        (buffer-substring-no-properties comint-last-input-end pmark)))))

(defun httprepl-display-response (response)
  (let ((buffer (get-buffer-create httprepl-response-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (fundamental-mode)
      (insert response))
    (pop-to-buffer buffer)))

(defun httprepl-open-response ()
  (interactive)
  (->> (httprepl-get-response)
    httprepl-display-response
    (httprepl-apply-middleware httprepl-response-middleware)))

;;; interface

(defun httprepl-insert (&rest args)
  (dolist (string args)
    (when string
      (comint-output-filter (get-buffer-process (current-buffer)) string))))

(defun httprepl-print (result)
  (when result
    (httprepl-insert result
                     (when (not (s-ends-with-p "\n" result)) "\n")
                     httprepl-prompt)))

(defun httprepl-rep (input)
  (-> input httprepl-read httprepl-eval httprepl-print))

(defun httprepl-input-sender (proc string) nil)

(defun httprepl-send-input ()
  (interactive)
  (let ((input (buffer-substring
                (process-mark (get-buffer-process (current-buffer)))
                (point-max))))
    (comint-send-input)        ;ends up invoking httprepl-input-sender
    (httprepl-rep input)))

(defvar httprepl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'httprepl-send-input)
    (define-key map (kbd "C-c C-c") 'httprepl-open-response)
    map)
  "Keymap for `httprepl-mode'.")

(define-derived-mode httprepl-mode comint-mode "Httprepl"
  "Major mode for interactively evaluating HTTP requests. Derives
from comint-mode.

Requests should be of the form:

------------
> GET|POST|PUT|DELETE|OPTIONS|HEAD|PATCH|TRACE|CONNECT uri
Header: value
Another-Header: value

Body of the request.
------------

For example:

------------
PUT http://httpbin.org/put
Content-Type: text/plain

This is the body of the request
------------

Extra headers and a request body are optional.

By default, you may use M-j to enter line breaks without sending
the request for evaluation.

`httprepl-send-input' is used for evaluating requests.

`httprepl-open-response' is used to open the last response in the
buffer defined by `httprepl-response-buffer-name'. This will be
manipulated by the functions specified in
`httprepl-response-middleware'

Customized bindings may be defined in `httprepl-mode-map'."
  :group 'httprepl

  (setq comint-prompt-regexp (concat "^" (regexp-quote httprepl-prompt)))
  (setq comint-input-sender 'httprepl-input-sender)
  (setq comint-process-echoes nil)

  (unless (comint-check-proc (current-buffer))
    (let ((process (condition-case nil
                       (start-process "httprepl" (current-buffer) "hexl")
                     (file-error (start-process "httprepl" (current-buffer) "cat")))))
      (set-process-query-on-exit-flag process nil)
      (unless comint-use-prompt-regexp
        (let ((inhibit-read-only t))
          (add-text-properties
           (point-min) (point-max)
           '(rear-nonsticky t field output inhibit-line-move-field-capture t))))
      (httprepl-print httprepl-header))))

;;;###autoload
(defun httprepl ()
  "Interactively evaluate HTTP requests at a REPL-like
interface. Switches to the buffer specified by
`httprepl-buffer-name', or creates it if it does not exist. See
`httprepl-mode' for a reference on constructing requests."
  (interactive)
  (let ((buffer (get-buffer httprepl-buffer-name)))
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'httprepl-mode)))
         (get-buffer-create (or buffer httprepl-buffer-name))
       (current-buffer)))
    (httprepl-mode)))

(provide 'httprepl)

;;; httprepl.el ends here
