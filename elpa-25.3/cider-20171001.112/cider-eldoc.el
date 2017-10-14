;;; cider-eldoc.el --- eldoc support for Clojure -*- lexical-binding: t -*-

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

;; eldoc support for Clojure.

;;; Code:

(require 'cider-client)
(require 'cider-common) ; for cider-symbol-at-point
(require 'subr-x)
(require 'cider-compat)
(require 'cider-util)
(require 'nrepl-dict)

(require 'seq)

(require 'eldoc)

(defvar cider-extra-eldoc-commands '("yas-expand")
  "Extra commands to be added to eldoc's safe commands list.")

(defcustom cider-eldoc-max-num-sexps-to-skip 30
  "The maximum number of sexps to skip while searching the beginning of current sexp."
  :type 'integer
  :group 'cider
  :package-version '(cider . "0.10.1"))

(defvar-local cider-eldoc-last-symbol nil
  "The eldoc information for the last symbol we checked.")

(defcustom cider-eldoc-ns-function #'identity
  "A function that returns a ns string to be used by eldoc.
Takes one argument, a namespace name.
For convenience, some functions are already provided for this purpose:
`cider-abbreviate-ns', and `cider-last-ns-segment'."
  :type '(choice (const :tag "Full namespace" identity)
                 (const :tag "Abbreviated namespace" cider-abbreviate-ns)
                 (const :tag "Last name in namespace" cider-last-ns-segment)
                 (function :tag "Custom function"))
  :group 'cider
  :package-version '(cider . "0.13.0"))

(defcustom cider-eldoc-max-class-names-to-display 3
  "The maximum number of classes to display in an eldoc string.
An eldoc string for Java interop forms can have a number of classes prefixed to
it, when the form belongs to more than 1 class.  When, not nil we only display
the names of first `cider-eldoc-max-class-names-to-display' classes and add
a \"& x more\" suffix. Otherwise, all the classes are displayed."
  :type 'integer
  :safe #'integerp
  :group 'cider
  :package-version '(cider . "0.13.0"))

(defcustom cider-eldoc-display-for-symbol-at-point t
  "When non-nil, display eldoc for symbol at point if available.
So in (map inc ...) when the cursor is over inc its eldoc would be
displayed.  When nil, always display eldoc for first symbol of the sexp."
  :type 'boolean
  :safe #'booleanp
  :group 'cider
  :package-version '(cider . "0.13.0"))

(defcustom cider-eldoc-display-context-dependent-info nil
  "When non-nil, display context dependent info in the eldoc where possible.
CIDER will try to add expected function arguments based on the current context,
for example for the datomic.api/q function where it will show the expected
inputs of the query at point."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.15.0"))

(defun cider--eldoc-format-class-names (class-names)
  "Return a formatted CLASS-NAMES prefix string.
CLASS-NAMES is a list of classes to which a Java interop form belongs.
Only keep the first `cider-eldoc-max-class-names-to-display' names, and
add a \"& x more\" suffix.  Return nil if the CLASS-NAMES list is empty or
mapping `cider-eldoc-ns-function' on it returns an empty list."
  (when-let ((eldoc-class-names (seq-remove #'null (mapcar (apply-partially cider-eldoc-ns-function) class-names)))
             (eldoc-class-names-length (length eldoc-class-names)))
    (cond
     ;; truncate class-names list and then format it
     ((and cider-eldoc-max-class-names-to-display
           (> eldoc-class-names-length cider-eldoc-max-class-names-to-display))
      (format "(%s & %s more)"
              (thread-first eldoc-class-names
                (seq-take cider-eldoc-max-class-names-to-display)
                (string-join " ")
                (cider-propertize 'ns))
              (- eldoc-class-names-length cider-eldoc-max-class-names-to-display)))

     ;; format the whole list but add surrounding parentheses
     ((> eldoc-class-names-length 1)
      (format "(%s)"
              (thread-first eldoc-class-names
                (string-join " ")
                (cider-propertize 'ns))))

     ;; don't add the parentheses
     (t (format "%s" (car eldoc-class-names))))))

(defun cider-eldoc-format-thing (ns symbol thing type)
  "Format the eldoc subject defined by NS, SYMBOL, THING and TYPE.
THING represents the thing at point which triggered eldoc.  Normally NS and
SYMBOL are used (they are derived from THING), but when empty we fallback to
THING (e.g. for Java methods).  Format it as a function, if FUNCTION-P
is non-nil.  Else format it as a variable."
  (if-let ((method-name (if (and symbol (not (string= symbol "")))
                            symbol
                          thing))
           (propertized-method-name (cider-propertize method-name type))
           (ns-or-class (if (and ns (stringp ns))
                            (funcall cider-eldoc-ns-function ns)
                          (cider--eldoc-format-class-names ns))))
      (format "%s/%s"
              ;; we set font-lock properties of classes in `cider--eldoc-format-class-names'
              ;; to avoid font locking the parentheses and "& x more"
              ;; so we only propertize ns-or-class if not already done
              (if (get-text-property 1 'face ns-or-class)
                  ;; it is already propertized
                  ns-or-class
                (cider-propertize ns-or-class 'ns))
              propertized-method-name)
    ;; in case ns-or-class is nil
    propertized-method-name))

(defun cider-eldoc-format-sym-doc (var ns docstring)
  "Return the formatted eldoc string for VAR and DOCSTRING.

Consider the value of `eldoc-echo-area-use-multiline-p' while formatting.
If the entire line cannot fit in the echo area, the var name may be
truncated or eliminated entirely from the output to make room for the
description.

Try to truncate the var with various strategies, so that the var and
the docstring can be displayed in the minibuffer without resizing the window.
We start with `cider-abbreviate-ns' and `cider-last-ns-segment'.
Next, if the var is in current namespace, we remove NS from the eldoc string.
Otherwise, only the docstring is returned."
  (let* ((ea-multi eldoc-echo-area-use-multiline-p)
         ;; Subtract 1 from window width since emacs will not write
         ;; any chars to the last column, or in later versions, will
         ;; cause a wraparound and resize of the echo area.
         (ea-width (1- (window-width (minibuffer-window))))
         (strip (- (+ (length var) (length docstring)) ea-width))
         (newline (string-match-p "\n" docstring))
         ;; Truncated var can be ea-var long
         ;; Subtract 2 to account for the : and / added when including
         ;; the namespace prefixed form in eldoc string
         (ea-var (- (- ea-width (length docstring)) 2)))
    (cond
     ((or (eq ea-multi t)
          (and (<= strip 0) (null newline))
          (and ea-multi (or (> (length docstring) ea-width) newline)))
      (format "%s: %s" var docstring))

     ;; Now we have to truncate either the docstring or the var
     (newline (cider-eldoc-format-sym-doc var ns (substring docstring 0 newline)))

     ;; Only return the truncated docstring
     ((> (length docstring) ea-width)
      (substring docstring 0 ea-width))

     ;; Try to truncate the var with cider-abbreviate-ns
     ((<= (length (cider-abbreviate-ns var)) ea-var)
      (format "%s: %s" (cider-abbreviate-ns var) docstring))

     ;; Try to truncate var with cider-last-ns-segment
     ((<= (length (cider-last-ns-segment var)) ea-var)
      (format "%s: %s" (cider-last-ns-segment var) docstring))

     ;; If the var is in current namespace, we try to truncate the var by
     ;; skipping the namespace from the returned eldoc string
     ((and (string-equal ns (cider-current-ns))
           (<= (- (length var) (length ns)) ea-var))
      (format "%s: %s"
              (replace-regexp-in-string (format "%s/" ns) "" var)
              docstring))

     ;; We couldn't fit the var and docstring in the available space,
     ;; so we just display the docstring
     (t docstring))))

(defun cider-eldoc-format-variable (thing pos eldoc-info)
  "Return the formatted eldoc string for a variable.
THING is the variable name.  POS will always be 0 here.
ELDOC-INFO is a p-list containing the eldoc information."
  (let* ((ns (lax-plist-get eldoc-info "ns"))
         (symbol (lax-plist-get eldoc-info "symbol"))
         (docstring (lax-plist-get eldoc-info "docstring"))
         (formatted-var (cider-eldoc-format-thing ns symbol thing 'var)))
    (when docstring
      (cider-eldoc-format-sym-doc formatted-var ns docstring))))

(defun cider-eldoc-format-function (thing pos eldoc-info)
  "Return the formatted eldoc string for a function.
THING is the function name.  POS is the argument-index of the functions
arglists.  ELDOC-INFO is a p-list containing the eldoc information."
  (let ((ns (lax-plist-get eldoc-info "ns"))
        (symbol (lax-plist-get eldoc-info "symbol"))
        (arglists (lax-plist-get eldoc-info "arglists")))
    (format "%s: %s"
            (cider-eldoc-format-thing ns symbol thing 'fn)
            (cider-eldoc-format-arglist arglists pos))))

(defun cider-highlight-args (arglist pos)
  "Format the the function ARGLIST for eldoc.
POS is the index of the currently highlighted argument."
  (let* ((rest-pos (cider--find-rest-args-position arglist))
         (i 0))
    (mapconcat
     (lambda (arg)
       (let ((argstr (format "%s" arg)))
         (if (string= arg "&")
             argstr
           (prog1
               (if (or (= (1+ i) pos)
                       (and rest-pos
                            (> (1+ i) rest-pos)
                            (> pos rest-pos)))
                   (propertize argstr 'face
                               'eldoc-highlight-function-argument)
                 argstr)
             (setq i (1+ i)))))) arglist " ")))

(defun cider--find-rest-args-position (arglist)
  "Find the position of & in the ARGLIST vector."
  (seq-position arglist "&"))

(defun cider-highlight-arglist (arglist pos)
  "Format the ARGLIST for eldoc.
POS is the index of the argument to highlight."
  (concat "[" (cider-highlight-args arglist pos) "]"))

(defun cider-eldoc-format-arglist (arglist pos)
  "Format all the ARGLIST for eldoc.
POS is the index of current argument."
  (concat "("
          (mapconcat (lambda (args) (cider-highlight-arglist args pos))
                     arglist
                     " ")
          ")"))

(defun cider-eldoc-beginning-of-sexp ()
  "Move to the beginning of current sexp.

Return the number of nested sexp the point was over or after.  Return nil
if the maximum number of sexps to skip is exceeded."
  (let ((parse-sexp-ignore-comments t)
        (num-skipped-sexps 0))
    (condition-case _
        (progn
          ;; First account for the case the point is directly over a
          ;; beginning of a nested sexp.
          (condition-case _
              (let ((p (point)))
                (forward-sexp -1)
                (forward-sexp 1)
                (when (< (point) p)
                  (setq num-skipped-sexps 1)))
            (error))
          (while
              (let ((p (point)))
                (forward-sexp -1)
                (when (< (point) p)
                  (setq num-skipped-sexps
                        (unless (and cider-eldoc-max-num-sexps-to-skip
                                     (>= num-skipped-sexps
                                         cider-eldoc-max-num-sexps-to-skip))
                          ;; Without the above guard,
                          ;; `cider-eldoc-beginning-of-sexp' could traverse the
                          ;; whole buffer when the point is not within a
                          ;; list. This behavior is problematic especially with
                          ;; a buffer containing a large number of
                          ;; non-expressions like a REPL buffer.
                          (1+ num-skipped-sexps)))))))
      (error))
    num-skipped-sexps))

(defun cider-eldoc-thing-type (eldoc-info)
  "Return the type of the ELDOC-INFO being displayed by eldoc.
It can be a function or var now."
  (pcase (lax-plist-get eldoc-info "type")
    ("function" 'fn)
    ("variable" 'var)))

(defun cider-eldoc-info-at-point ()
  "Return eldoc info at point.
First go to the beginning of the sexp and check if the eldoc is to be
considered (i.e sexp is a method call) and not a map or vector literal.
Then go back to the point and return its eldoc."
  (save-excursion
    (unless (cider-in-comment-p)
      (let* ((current-point (point)))
        (cider-eldoc-beginning-of-sexp)
        (unless (member (or (char-before (point)) 0) '(?\" ?\{ ?\[))
          (goto-char current-point)
          (when-let (eldoc-info (cider-eldoc-info
                                 (cider--eldoc-remove-dot (cider-symbol-at-point))))
            `("eldoc-info" ,eldoc-info
              "thing" ,(cider-symbol-at-point)
              "pos" 0)))))))

(defun cider-eldoc-info-at-sexp-beginning ()
  "Return eldoc info for first symbol in the sexp."
  (save-excursion
    (when-let ((beginning-of-sexp (cider-eldoc-beginning-of-sexp))
               ;; If we are at the beginning of function name, this will be -1
               (argument-index (max 0 (1- beginning-of-sexp))))
      (unless (or (memq (or (char-before (point)) 0)
                        '(?\" ?\{ ?\[))
                  (cider-in-comment-p))
        (when-let (eldoc-info (cider-eldoc-info
                               (cider--eldoc-remove-dot (cider-symbol-at-point))))
          `("eldoc-info" ,eldoc-info
            "thing" ,(cider-symbol-at-point)
            "pos" ,argument-index))))))

(defun cider-eldoc-info-in-current-sexp ()
  "Return eldoc information from the sexp.
If `cider-eldoc-display-for-symbol-at-poin' is non-nil and
the symbol at point has a valid eldoc available, return that.
Otherwise return the eldoc of the first symbol of the sexp."
  (or (when cider-eldoc-display-for-symbol-at-point
        (cider-eldoc-info-at-point))
      (cider-eldoc-info-at-sexp-beginning)))

(defun cider-eldoc--convert-ns-keywords (thing)
  "Convert THING values that match ns macro keywords to function names."
  (pcase thing
    (":import" "clojure.core/import")
    (":refer-clojure" "clojure.core/refer-clojure")
    (":use" "clojure.core/use")
    (":refer" "clojure.core/refer")
    (_ thing)))

(defun cider-eldoc-info (thing)
  "Return the info for THING.
This includes the arglist and ns and symbol name (if available)."
  (let ((thing (cider-eldoc--convert-ns-keywords thing)))
    (when (and (cider-nrepl-op-supported-p "eldoc")
               thing
               ;; ignore empty strings
               (not (string= thing ""))
               ;; ignore strings
               (not (string-prefix-p "\"" thing))
               ;; ignore regular expressions
               (not (string-prefix-p "#" thing))
               ;; ignore chars
               (not (string-prefix-p "\\" thing))
               ;; ignore numbers
               (not (string-match-p "^[0-9]" thing)))
      ;; check if we can used the cached eldoc info
      (cond
       ;; handle keywords for map access
       ((string-prefix-p ":" thing) (list "symbol" thing
                                          "type" "function"
                                          "arglists" '(("map") ("map" "not-found"))))
       ;; handle Classname. by displaying the eldoc for new
       ((string-match-p "^[A-Z].+\\.$" thing) (list "symbol" thing
                                                    "type" "function"
                                                    "arglists" '(("args*"))))
       ;; generic case
       (t (if (equal thing (car cider-eldoc-last-symbol))
              (cadr cider-eldoc-last-symbol)
            (when-let ((eldoc-info (cider-sync-request:eldoc thing)))
              (let* ((arglists (nrepl-dict-get eldoc-info "eldoc"))
                     (docstring (nrepl-dict-get eldoc-info "docstring"))
                     (type (nrepl-dict-get eldoc-info "type"))
                     (ns (nrepl-dict-get eldoc-info "ns"))
                     (class (nrepl-dict-get eldoc-info "class"))
                     (name (nrepl-dict-get eldoc-info "name"))
                     (member (nrepl-dict-get eldoc-info "member"))
                     (ns-or-class (if (and ns (not (string= ns "")))
                                      ns
                                    class))
                     (name-or-member (if (and name (not (string= name "")))
                                         name
                                       (format ".%s" member)))
                     (eldoc-plist (list "ns" ns-or-class
                                        "symbol" name-or-member
                                        "arglists" arglists
                                        "docstring" docstring
                                        "type" type)))
                ;; add context dependent args if requested by defcustom
                ;; do not cache this eldoc info to avoid showing info
                ;: of the previous context
                (if cider-eldoc-display-context-dependent-info
                    (cond
                     ;; add inputs of datomic query
                     ((and (equal ns-or-class "datomic.api")
                           (equal name-or-member "q"))
                      (let ((arglists (lax-plist-get eldoc-plist "arglists")))
                        (lax-plist-put eldoc-plist "arglists"
                                       (cider--eldoc-add-datomic-query-inputs-to-arglists arglists))))
                     ;; if none of the clauses is successful, do cache the eldoc
                     (t (setq cider-eldoc-last-symbol (list thing eldoc-plist))))
                  ;; middleware eldoc lookups are expensive, so we
                  ;; cache the last lookup.  This eliminates the need
                  ;; for extra middleware requests within the same sexp.
                  (setq cider-eldoc-last-symbol (list thing eldoc-plist)))
                eldoc-plist))))))))

(defun cider--eldoc-remove-dot (sym)
  "Remove the preceding \".\" from a namespace qualified SYM and return sym.
Only useful for interop forms.  Clojure forms would be returned unchanged."
  (when sym (replace-regexp-in-string "/\\." "/" sym)))

(defun cider--eldoc-edn-file-p (file-name)
  "Check whether FILE-NAME is representing an EDN file."
  (and file-name (equal (file-name-extension file-name) "edn")))

(defun cider--eldoc-add-datomic-query-inputs-to-arglists (arglists)
  "Add the expected inputs of the datomic query to the ARGLISTS."
  (if (cider-second-sexp-in-list)
    (let* ((query (cider-second-sexp-in-list))
           (query-inputs (nrepl-dict-get
                          (cider-sync-request:eldoc-datomic-query query)
                          "inputs")))
      (if query-inputs
          (thread-first
              (thread-last arglists
                (car)
                (remove "&")
                (remove "inputs"))
            (append (car query-inputs))
            (list))
        arglists))
    arglists))

(defun cider-eldoc ()
  "Backend function for eldoc to show argument list in the echo area."
  (when (and (cider-connected-p)
             ;; don't clobber an error message in the minibuffer
             (not (member last-command '(next-error previous-error)))
             ;; don't try to provide eldoc in EDN buffers
             (not (cider--eldoc-edn-file-p buffer-file-name)))
    (let* ((sexp-eldoc-info (cider-eldoc-info-in-current-sexp))
           (eldoc-info (lax-plist-get sexp-eldoc-info "eldoc-info"))
           (pos (lax-plist-get sexp-eldoc-info "pos"))
           (thing (lax-plist-get sexp-eldoc-info "thing")))
      (when eldoc-info
        (if (equal (cider-eldoc-thing-type eldoc-info) 'fn)
            (cider-eldoc-format-function thing pos eldoc-info)
          (cider-eldoc-format-variable thing pos eldoc-info))))))

(defun cider-eldoc-setup ()
  "Setup eldoc in the current buffer.
eldoc mode has to be enabled for this to have any effect."
  (setq-local eldoc-documentation-function #'cider-eldoc)
  (apply #'eldoc-add-command cider-extra-eldoc-commands))

(provide 'cider-eldoc)

;;; cider-eldoc.el ends here
