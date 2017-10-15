;;; company-ycmd.el --- company-mode backend for ycmd -*- lexical-binding: t -*-
;;
;; Copyright (c) 2014-2017 Austin Bingham, Peter Vasil
;;
;; Authors: Austin Bingham <austin.bingham@gmail.com>
;;          Peter Vasil <mail@petervasil.net>
;; version: 0.2
;; Package-Version: 20171010.2258
;; URL: https://github.com/abingham/emacs-ycmd
;; Package-Requires: ((ycmd "1.3") (company "0.9.3") (deferred "0.5.1") (s "1.11.0") (dash "2.13.0") (let-alist "1.0.5") (f "0.19.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Description:
;;
;; ycmd is a modular code-completion framework. It includes, for
;; example, completion for C/C++/ObjC and Python. This module supplies
;; a company-mode backend for these completions.
;;
;; For more details, see the project page at
;; https://github.com/abingham/emacs-ycmd.
;;
;; Installation:
;;
;; Copy this file to to some location in your emacs load path. Then add
;; "(require 'company-ycmd)" to your emacs initialization (.emacs,
;; init.el, or something).
;;
;; Example config:
;;
;;   (require 'company-ycmd)
;;
;;; License:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(eval-when-compile
  (require 'let-alist))
(require 'cl-lib)
(require 'company)
(require 'company-template)
(require 'deferred)
(require 'ycmd)
(require 's)
(require 'f)
(require 'dash)
(require 'rx)

(defgroup company-ycmd nil
  "Company-mode completion backend for ycmd."
  :group 'company
  :group 'ycmd)

(defcustom company-ycmd-insert-arguments t
  "When non-nil, insert function arguments as a template after completion.

Only supported by modes in `company-ycmd--extended-features-modes'"
  :type 'boolean)

(defcustom company-ycmd-enable-fuzzy-matching t
  "When non-nil, use fuzzy matching for completion candidates.

Setting this to nil enables the `company-mode' internal cache
feature."
  :type 'boolean)

(defcustom company-ycmd-show-completion-kind t
  "Show kind of completion entry."
  :type 'boolean)

(defcustom company-ycmd-request-sync-timeout 0.05
  "Timeout for synchronous ycmd completion request.
When 0, do not use synchronous completion request at all."
  :type 'number)

(defconst company-ycmd--extended-features-modes
  '(c++-mode
    c-mode
    go-mode
    objc-mode
    rust-mode
    python-mode
    js-mode
    typescript-mode)
  "Major modes which have extended features in `company-ycmd'.")

(defun company-ycmd--extended-features-p ()
  "Check whether to use extended features."
  (memq major-mode company-ycmd--extended-features-modes))

(defun company-ycmd--prefix-candidate-p (candidate prefix)
  "Return t if CANDIDATE string begins with PREFIX."
  (let ((insertion-text (cdr (assq 'insertion_text candidate))))
    (s-starts-with? prefix insertion-text t)))

(defun company-ycmd--filename-completer-p (extra-info)
  "Check whether candidate's EXTRA-INFO indicates a filename completion."
  (-contains? '("[File]" "[Dir]" "[File&Dir]") extra-info))

(defun company-ycmd--identifier-completer-p (extra-info)
  "Check if candidate's EXTRA-INFO indicates a identifier completion."
  (s-equals? "[ID]" extra-info))

(defmacro company-ycmd--with-destructured-candidate (candidate &rest body)
  (declare (indent 1) (debug t))
  `(let-alist ,candidate
     (if (or (company-ycmd--identifier-completer-p .extra_menu_info)
             (company-ycmd--filename-completer-p .extra_menu_info))
         (propertize .insertion_text 'return_type .extra_menu_info)
       ,@body)))

(defun company-ycmd--extract-params-clang (function-signature)
  "Extract parameters from FUNCTION-SIGNATURE if possible."
  (let ((params (company-ycmd--extract-params-clang-1
                 function-signature)))
    (if (not (and params (string-prefix-p "(*)" params)))
        params
      (with-temp-buffer
        (insert params)
        (search-backward ")")
        (let ((pt (1+ (point))))
          (re-search-forward ".\\_>" nil t)
          (delete-region pt (point)))
        (buffer-string)))))

(defun company-ycmd--extract-params-clang-1 (function-signature)
  "Extract parameters from FUNCTION-SIGNATURE if possible."
  (cond
   ((null function-signature) nil)
   ((string-match "[^:]:[^:]" function-signature)
    (substring function-signature (1+ (match-beginning 0))))
   ((string-match "\\((.*)[ a-z]*\\'\\)" function-signature)
    (let ((paren (match-beginning 1)))
      (if (not (and (eq (aref function-signature (1- paren)) ?>)
                    (s-contains?
                     "<" (substring function-signature 0 (1- paren)))))
          (match-string 1 function-signature)
        (with-temp-buffer
          (insert function-signature)
          (goto-char paren)
          (substring function-signature (1- (search-backward "<")))))))))

(defun company-ycmd--convert-kind-clang (kind)
  "Convert KIND string for display."
  (pcase kind
    ("STRUCT" "struct")
    ("CLASS" "class")
    ("ENUM" "enum")
    ("TYPE" "type")
    ("MEMBER" "member")
    ("FUNCTION" "fn")
    ("VARIABLE" "var")
    ("MACRO" "macro")
    ("PARAMETER" "parameter")
    ("NAMESPACE" "namespace")))

(defun company-ycmd--construct-candidate-clang (candidate)
  "Construct a completion string(s) from a CANDIDATE for cpp file-types.

Returns a list with one candidate or multiple candidates for
overloaded functions."
  (company-ycmd--with-destructured-candidate candidate
    (let* ((overloads (and company-ycmd-insert-arguments
                           (stringp .detailed_info)
                           (s-split "\n" .detailed_info t)))
           (items (or overloads (list .menu_text)))
           candidates)
      (when (eq major-mode 'objc-mode)
        (setq .insertion_text (s-chop-suffix ":" .insertion_text)))
      (dolist (it (delete-dups items) candidates)
        (let* ((meta (if overloads it .detailed_info))
               (kind (company-ycmd--convert-kind-clang .kind))
               (params (and (or (string= kind "fn") (string= kind "class"))
                            (company-ycmd--extract-params-clang it)))
               (return-type (or (and overloads
                                     (let ((case-fold-search nil))
                                       (and (string-match
                                             (concat "\\(.*\\) [^ ]*"
                                                     (regexp-quote .insertion_text))
                                             it)
                                            (match-string 1 it))))
                                .extra_menu_info))
               (doc .extra_data.doc_string))
          (setq candidates
                (cons (propertize .insertion_text 'return_type return-type
                                  'meta meta 'kind kind 'doc doc 'params params)
                      candidates)))))))

(defun company-ycmd--construct-candidate-go (candidate)
  "Construct completion string from a CANDIDATE for go file-types."
  (company-ycmd--with-destructured-candidate candidate
    (let* ((is-func (and .extra_menu_info
                         (string-prefix-p "func" .extra_menu_info)))
           (meta (and .kind .menu_text .extra_menu_info
                      (concat .kind " " .menu_text
                              (if is-func
                                  (substring .extra_menu_info 4 nil)
                                (concat " " .extra_menu_info)))))
           (return-type (and .extra_menu_info
                             (string-match "^func(.*) \\(.*\\)" .extra_menu_info)
                             (match-string 1 .extra_menu_info)))
           (params (and .extra_menu_info
                        (or (string-match "^func\\((.*)\\) .*" .extra_menu_info)
                            (string-match "^func\\((.*)\\)\\'" .extra_menu_info))
                        (match-string 1 .extra_menu_info)))
           (kind (if (and .extra_menu_info (not is-func))
                     (concat .kind ": " .extra_menu_info)
                   .kind)))
      (propertize .insertion_text 'return_type return-type
                  'meta meta 'kind kind 'params params))))

(defun company-ycmd--remove-self-from-function-args (args)
  "Remove function argument `self' from ARGS string."
  (if (s-contains? "self" args)
      (when (string-match "(\\(.*\\))" args)
        (->> (s-split "," (match-string 1 args) t)
             (cl-remove-if (lambda (s) (string-match-p "self" s)))
             (s-join ",")
             (s-trim-left)
             (s-prepend (substring args 0 (match-beginning 1)))
             (s-append (substring args (match-end 1)))))
    args))

(defun company-ycmd--remove-template-args-from-function-args (args)
  "Remove template arguments from ARGS string."
  (if (s-starts-with? "<" args)
      (substring args (+ 1 (s-index-of ">" args)))
    args))

(defun company-ycmd--extract-params-python (function-sig function-name)
  "Extract function arguments from FUNCTION-SIG.
Use FUNCTION-NAME as part of the regex to match arguments.
Replace any newline characters with spaces."
  (when (and function-sig
             (string-match
              (concat (regexp-quote function-name)
                      ;; Regex to match everything between parentheses, including
                      ;; newline.
                      ;; https://www.emacswiki.org/emacs/MultilineRegexp
                      "\\(([\0-\377[:nonascii:]]*?)\\).*")
              function-sig))
    (s-replace "\n" " " (match-string 1 function-sig))))

(defun company-ycmd--extract-meta-python (doc-string)
  "Extract string for meta usage from DOC-STRING.
Remove newline characters in function arguments and replace them
with spaces."
  (when doc-string
    (if (string-match "\n" doc-string)
        (let (meta)
          (setq meta (substring doc-string 0 (match-beginning 0)))
          (while (and (string-match-p "(" meta)
                      (not (string-match-p ")" meta)))
            (if (string-match "\n" doc-string (match-end 0))
                (setq meta (substring doc-string 0 (match-beginning 0)))
              (setq meta doc-string)))
          (s-replace "\n" " " meta))
      doc-string)))

(defun company-ycmd--construct-candidate-python (candidate)
  "Construct completion string from a CANDIDATE for python file-types."
  (company-ycmd--with-destructured-candidate candidate
    (let* ((kind (s-replace "\n" " " .extra_menu_info))
           (params (and (s-prefix-p "def" kind)
                        (company-ycmd--extract-params-python
                         .detailed_info .insertion_text)))
           (meta (company-ycmd--extract-meta-python .detailed_info))
           (filepath .extra_data.location.filepath)
           (line-num .extra_data.location.line_num))
      (propertize .insertion_text 'meta meta 'doc .detailed_info 'kind kind
                  'params params 'filepath filepath 'line_num line-num))))

;; The next two function are taken from racer.el
;; https://github.com/racer-rust/emacs-racer
(defun company-ycmd--file-and-parent (path)
  "Convert PATH /foo/bar/baz/q.txt to baz/q.txt."
  (let ((file (f-filename path))
        (parent (f-filename (f-parent path))))
    (f-join parent file)))

(defun company-ycmd--trim-up-to (needle s)
  "Return content after the occurrence of NEEDLE in S."
  (-if-let (idx (s-index-of needle s))
      (substring s (+ idx (length needle)))
    s))

(defun company-ycmd--construct-candidate-rust (candidate)
  "Construct completion string from CANDIDATE for rust file-types."
  (company-ycmd--with-destructured-candidate candidate
    (let* ((meta .extra_menu_info)
           (context (pcase .kind
                      ("Module"
                       (if (string= .insertion_text .extra_menu_info)
                           ""
                         (concat " " (company-ycmd--file-and-parent
                                      .extra_menu_info))))
                      ("StructField"
                       (concat " " .extra_menu_info))
                      (_
                       (->> .extra_menu_info
                            (company-ycmd--trim-up-to .insertion_text)
                            (s-chop-suffixes '(" {" "," ";"))))))
           (annotation (concat context
                               (when (s-present? .kind)
                                 (format " [%s]" .kind))))
           (params (and (string= "Function" .kind)
                        (if (string-match "\\(.*?\\) -> .*" context)
                            (match-string 1 context)
                          context)))
           (filepath .extra_data.location.filepath)
           (line-num .extra_data.location.line_num)
           (column-num .extra_data.location.column_num))
      (propertize .insertion_text 'meta meta 'kind .kind
                  'params params 'annotation annotation
                  'filepath filepath 'line_num line-num
                  'column_num column-num))))

(defun company-ycmd--construct-candidate-javascript (candidate)
  "Construct completion string from CANDIDATE for js file-types."
  (company-ycmd--with-destructured-candidate candidate
    (let* ((kind (or (and (string-match "^fn" .extra_menu_info)
                          (match-string 0 .extra_menu_info))
                     .extra_menu_info))
           (meta .extra_menu_info)
           (params (and .extra_menu_info
                        (string-match "^fn\\((.*)\\).*" .extra_menu_info)
                        (match-string 1 .extra_menu_info)))
           (return-type (and .extra_menu_info
                             (string-match "^fn(.*) -> \\(.*\\)" .extra_menu_info)
                             (match-string 1 .extra_menu_info)))
           (doc .detailed_info))
      (propertize .insertion_text 'meta meta 'params params
                  'return_type return-type 'kind kind 'doc doc))))

(defun company-ycmd--construct-candidate-typescript (candidate)
  "Generic function to construct completion string from a CANDIDATE."
  (company-ycmd--with-destructured-candidate candidate
    (let* ((kind .kind)
           (meta (and .menu_text
                      (if .extra_data
                          (concat "(" .extra_data ") " .menu_text)
                        (if (string-match (concat (regexp-quote .insertion_text)
                                                  "\s*\\(.*\\)")
                                          .menu_text)
                            (match-string 1 .menu_text)
                          .menu_text))))
           (base-regexp (concat "^" (regexp-quote .insertion_text)
                                "\s*(" (regexp-quote .kind) ") "
                                ".*\." (regexp-quote .insertion_text)))
           (params (and .menu_text
                        (string-match (concat base-regexp "\\((.*)\\):.*")
                                      .menu_text)
                        (match-string 1 .menu_text)))
           (return-type (and .menu_text
                             (string-match
                              (concat base-regexp
                                      (and params (regexp-quote params))
                                      ": \\(.*\\)")
                              .menu_text)
                             (match-string 1 .menu_text))))
      (propertize .insertion_text 'kind kind 'meta meta
                  'params params 'return_type return-type))))

(defun company-ycmd--construct-candidate-generic (candidate)
  "Generic function to construct completion string from a CANDIDATE."
  (company-ycmd--with-destructured-candidate candidate .insertion_text))

(defun company-ycmd--construct-candidates (completions
                                           prefix
                                           start-col
                                           construct-candidate-fn)
  "Construct candidates list from COMPLETIONS.

PREFIX is the prefix we calculated for doing the completion, and
START-COL is the column on which ycmd indicates we should place
the completion candidates.  If START-COL differs from start column
offset of PREFIX, we need to calculate the substring from PREFIX
for that difference and prepend it to the insertion-text.
CONSTRUCT-CANDIDATE-FN is a function to construct a completion
candidate.  See `company-ycmd--get-construct-candidate-fn'.

When `company-ycmd-enable-fuzzy-matching' is nil, check if
candidate starts with PREFIX, whether to include candidate in
candidates list."
  (let* ((prefix-start-col (- (+ 1 (ycmd--column-in-bytes)) (length prefix)))
         (prefix-size (- start-col prefix-start-col))
         (prefix-diff (substring-no-properties prefix 0 prefix-size))
         (prefix-diff-p (s-present? prefix-diff))
         candidates)
    (dolist (candidate completions (nreverse candidates))
      (when prefix-diff-p
        (let ((it (cdr (assq 'insertion_text candidate))))
          (setf it (s-prepend prefix-diff it))))
      (when (or company-ycmd-enable-fuzzy-matching
                (company-ycmd--prefix-candidate-p candidate prefix))
        (let ((result (funcall construct-candidate-fn candidate)))
          (if (listp result)
              (setq candidates (append result candidates))
            (setq candidates (cons result candidates))))))))

(defun company-ycmd--get-construct-candidate-fn ()
  "Return function to construct candidate(s) for current `major-mode'."
  (pcase (car-safe (ycmd-major-mode-to-file-types major-mode))
    ((or `"cpp" `"c" `"objc") 'company-ycmd--construct-candidate-clang)
    ("go" 'company-ycmd--construct-candidate-go)
    ("python" 'company-ycmd--construct-candidate-python)
    ("rust" 'company-ycmd--construct-candidate-rust)
    ("javascript" 'company-ycmd--construct-candidate-javascript)
    ("typescript" 'company-ycmd--construct-candidate-typescript)
    (_ 'company-ycmd--construct-candidate-generic)))

(defun company-ycmd--get-candidates (completions prefix &optional cb)
  "Get candidates for COMPLETIONS and PREFIX.

If CB is non-nil, call it with candidates."
  (let-alist completions
    (funcall
     (or cb 'identity)
     (company-ycmd--construct-candidates
      .completions prefix .completion_start_column
      (company-ycmd--get-construct-candidate-fn)))))

(defun company-ycmd--get-candidates-deferred (prefix cb)
  "Get completion candidates with PREFIX and call CB deferred."
  (let ((request-window (selected-window))
        (request-point (point))
        (request-tick (buffer-chars-modified-tick)))
    (ycmd-with-handled-server-exceptions (deferred:try (ycmd-get-completions)
                                           :catch (lambda (_err) nil))
      :bind-current-buffer t
      (if (or (not (equal request-window (selected-window)))
              (with-current-buffer (window-buffer request-window)
                (or (not (equal request-buffer (current-buffer)))
                    (not (equal request-point (point)))
                    (not (equal request-tick (buffer-chars-modified-tick))))))
          (message "Skip ycmd completion response")
        (company-ycmd--get-candidates response prefix cb)))))

(defun company-ycmd--meta (candidate)
  "Fetch the metadata text-property from a CANDIDATE string."
  (let ((meta (get-text-property 0 'meta candidate)))
    (if (stringp meta)
        (let ((meta-trimmed (s-trim meta)))
          (if (company-ycmd--extended-features-p)
              (ycmd--fontify-code meta-trimmed)
            meta-trimmed))
      meta)))

(defun company-ycmd--annotation (candidate)
  "Fetch the annotation text-property from a CANDIDATE string."
  (-if-let (annotation (get-text-property 0 'annotation candidate))
      annotation
    (let ((kind (and company-ycmd-show-completion-kind
                     (get-text-property 0 'kind candidate)))
          (return-type (get-text-property 0 'return_type candidate))
          (params (get-text-property 0 'params candidate)))
      (concat params
              (when (s-present? return-type)
                (s-prepend " -> " return-type))
              (when (s-present? kind)
                (format " [%s]" kind))))))

(defconst company-ycmd--include-declaration
  (rx line-start "#" (zero-or-more blank) (or "include" "import")
      (one-or-more blank)
      (submatch (in "<\"") (zero-or-more (not (in ">\"")))))
  "Regular expression to find C/C++/ObjC include directives.")

(defun company-ycmd--in-include ()
  "Check if text before point is an include statement."
  (looking-back company-ycmd--include-declaration
                (line-beginning-position)))

(defun company-ycmd--prefix ()
  "Prefix-command handler for the company backend."
  (and ycmd-mode
       buffer-file-name
       (ycmd-running-p)
       (or (not (company-in-string-or-comment))
           (company-ycmd--in-include))
       (or (company-grab-symbol-cons "\\.\\|->\\|::\\|/" 2)
           'stop)))

(defun company-ycmd--candidates (prefix)
  "Candidates-command handler for the company backend for PREFIX."
  (let ((fetcher (cons :async
                       (lambda (cb)
                         (company-ycmd--get-candidates-deferred prefix cb)))))
    (if (> company-ycmd-request-sync-timeout 0)
        (let ((result (ycmd-deferred:sync!
                       (ycmd-deferred:timeout company-ycmd-request-sync-timeout
                         (funcall (cdr fetcher) nil)))))
          (if (eq result 'timeout) fetcher result))
      fetcher)))

(defun company-ycmd--post-completion (candidate)
  "Insert function arguments after completion for CANDIDATE."
  (--when-let (and (company-ycmd--extended-features-p)
                   company-ycmd-insert-arguments
                   (get-text-property 0 'params candidate))
    (when (memq major-mode '(python-mode rust-mode))
      (setq it (company-ycmd--remove-self-from-function-args it))
      (when (eq major-mode 'rust-mode)
        (setq it (company-ycmd--remove-template-args-from-function-args it))))
    (insert it)
    (if (string-match "\\`:[^:]" it)
        (company-template-objc-templatify it)
      (company-template-c-like-templatify
       (concat candidate it)))))

(defun company-ycmd--doc-buffer (candidate)
  "Return buffer with docstring for CANDIDATE if it is available."
  (let ((doc (get-text-property 0 'doc candidate)))
    (when (s-present? doc)
      (company-doc-buffer doc))))

(defun company-ycmd--location (candidate)
  "Return location for CANDIDATE."
  (-when-let* ((filepath (get-text-property 0 'filepath candidate))
               (line-num (get-text-property 0 'line_num candidate)))
    (cons filepath line-num)))

(defun company-ycmd (command &optional arg &rest ignored)
  "The company-backend command handler for ycmd."
  (interactive (list 'interactive))
  (cl-case command
    (interactive     (company-begin-backend 'company-ycmd))
    (prefix          (company-ycmd--prefix))
    (candidates      (company-ycmd--candidates arg))
    (meta            (company-ycmd--meta arg))
    (annotation      (company-ycmd--annotation arg))
    (no-cache        company-ycmd-enable-fuzzy-matching)
    (sorted          't)
    (post-completion (company-ycmd--post-completion arg))
    (doc-buffer      (company-ycmd--doc-buffer arg))
    (location        (company-ycmd--location arg))))

;;;###autoload
(defun company-ycmd-setup ()
  "Add company-ycmd to the front of company-backends."
  (add-to-list 'company-backends 'company-ycmd))

(defun company-ycmd--init ()
  (unless (eq company-minimum-prefix-length
              ycmd-min-num-chars-for-completion)
    (setq-local company-minimum-prefix-length
                ycmd-min-num-chars-for-completion)))

(add-hook 'ycmd-mode-hook #'company-ycmd--init)

(provide 'company-ycmd)

;;; company-ycmd.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
