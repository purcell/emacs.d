;; cider-util.el --- Common utility functions that don't belong anywhere else -*- lexical-binding: t -*-

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

;; Common utility functions that don't belong anywhere else.

;;; Code:

(require 'seq)
(require 'clojure-mode)
(require 'subr-x)
(require 'cider-compat)
(require 'nrepl-dict)
(require 'ansi-color)

(defalias 'cider-pop-back 'pop-tag-mark)

(defcustom cider-font-lock-max-length 10000
  "The max length of strings to fontify in `cider-font-lock-as'.

Setting this to nil removes the fontification restriction."
  :group 'cider
  :type 'boolean
  :package-version '(cider . "0.10.0"))

(defun cider-util--hash-keys (hashtable)
  "Return a list of keys in HASHTABLE."
  (let ((keys '()))
    (maphash (lambda (k _v) (setq keys (cons k keys))) hashtable)
    keys))

(defun cider-util--clojure-buffers ()
  "Return a list of all existing `clojure-mode' buffers."
  (seq-filter
   (lambda (buffer) (with-current-buffer buffer (derived-mode-p 'clojure-mode)))
   (buffer-list)))

(defun cider-current-dir ()
  "Return the directory of the current buffer."
  (if buffer-file-name
      (file-name-directory buffer-file-name)
    default-directory))

(defun cider-in-string-p ()
  "Return non-nil if point is in a string."
  (let ((beg (save-excursion (beginning-of-defun) (point))))
    (nth 3 (parse-partial-sexp beg (point)))))

(defun cider-in-comment-p ()
  "Return non-nil if point is in a comment."
  (let ((beg (save-excursion (beginning-of-defun) (point))))
    (nth 4 (parse-partial-sexp beg (point)))))

(defun cider--tooling-file-p (file-name)
  "Return t if FILE-NAME is not a 'real' source file.
Currently, only check if the relative file name starts with 'form-init'
which nREPL uses for temporary evaluation file names."
  (let ((fname (file-name-nondirectory file-name)))
    (string-match-p "^form-init" fname)))

(defun cider--cljc-or-cljx-buffer-p (&optional buffer)
  "Return non-nil if the current buffer is visiting a cljc or cljx file.

If BUFFER is provided act on that buffer instead."
  (with-current-buffer (or buffer (current-buffer))
    (or (derived-mode-p 'clojurec-mode) (derived-mode-p 'clojurex-mode))))


;;; Thing at point
(defun cider-defun-at-point (&optional bounds)
  "Return the text of the top level sexp at point.
If BOUNDS is non-nil, return a list of its starting and ending position
instead."
  (save-excursion
    (save-match-data
      (end-of-defun)
      (let ((end (point)))
        (clojure-backward-logical-sexp 1)
        (funcall (if bounds #'list #'buffer-substring-no-properties)
                 (point) end)))))

(defun cider-ns-form ()
  "Retrieve the ns form."
  (when (clojure-find-ns)
    (save-excursion
      (goto-char (match-beginning 0))
      (cider-defun-at-point))))

(defun cider-symbol-at-point (&optional look-back)
  "Return the name of the symbol at point, otherwise nil.
Ignores the REPL prompt.  If LOOK-BACK is non-nil, move backwards trying to
find a symbol if there isn't one at point."
  (or (when-let ((str (thing-at-point 'symbol)))
        (unless (text-property-any 0 (length str) 'field 'cider-repl-prompt str)
          (substring-no-properties str)))
      (when look-back
        (save-excursion
          (ignore-errors
            (while (not (looking-at "\\sw\\|\\s_\\|\\`"))
              (forward-sexp -1)))
          (cider-symbol-at-point)))))


;;; sexp navigation
(defun cider-sexp-at-point (&optional bounds)
  "Return the sexp at point as a string, otherwise nil.
If BOUNDS is non-nil, return a list of its starting and ending position
instead."
  (when-let ((b (or (and (equal (char-after) ?\()
                         (member (char-before) '(?\' ?\, ?\@))
                         ;; hide stuff before ( to avoid quirks with '( etc.
                         (save-restriction
                           (narrow-to-region (point) (point-max))
                           (bounds-of-thing-at-point 'sexp)))
                    (bounds-of-thing-at-point 'sexp))))
    (funcall (if bounds #'list #'buffer-substring-no-properties)
             (car b) (cdr b))))

(defun cider-last-sexp (&optional bounds)
  "Return the sexp preceding the point.
If BOUNDS is non-nil, return a list of its starting and ending position
instead."
  (apply (if bounds #'list #'buffer-substring-no-properties)
         (save-excursion
           (clojure-backward-logical-sexp 1)
           (list (point)
                 (progn (clojure-forward-logical-sexp 1)
                        (skip-chars-forward "[:blank:]")
                        (when (looking-at-p "\n") (forward-char 1))
                        (point))))))

(defun cider-start-of-next-sexp (&optional skip)
  "Move to the start of the next sexp.
Skip any non-logical sexps like ^metadata or #reader macros.
If SKIP is an integer, also skip that many logical sexps first.
Can only error if SKIP is non-nil."
  (while (clojure--looking-at-non-logical-sexp)
    (forward-sexp 1))
  (when (and skip (> skip 0))
    (dotimes (_ skip)
      (forward-sexp 1)
      (cider-start-of-next-sexp))))

(defun cider-second-sexp-in-list ()
  "Return the second sexp in the list at point."
  (condition-case nil
      (save-excursion
        (backward-up-list)
        (forward-char)
        (forward-sexp 2)
        (cider-sexp-at-point))
    (error nil)))

;;; Text properties

(defun cider-maybe-intern (name)
  "If NAME is a symbol, return it; otherwise, intern it."
  (if (symbolp name) name (intern name)))

(defun cider-intern-keys (plist)
  "Copy PLIST, with any non-symbol keys replaced with symbols."
  (when plist
    (cons (cider-maybe-intern (pop plist))
          (cons (pop plist) (cider-intern-keys plist)))))

(defmacro cider-propertize-region (props &rest body)
  "Execute BODY and add PROPS to all the inserted text.
More precisely, PROPS are added to the region between the point's
positions before and after executing BODY."
  (declare (indent 1)
           (debug (sexp body)))
  (let ((start (make-symbol "start")))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
         (add-text-properties ,start (point) ,props)))))

(put 'cider-propertize-region 'lisp-indent-function 1)

(defun cider-property-bounds (prop)
  "Return the the positions of the previous and next change to PROP.
PROP is the name of a text property."
  (let ((end (next-single-char-property-change (point) prop)))
    (list (previous-single-char-property-change end prop) end)))

(defun cider-insert (text &optional face break more-text)
  "Insert TEXT with FACE, optionally followed by a line BREAK and MORE-TEXT."
  (insert (if face (propertize text 'font-lock-face face) text))
  (when more-text (insert more-text))
  (when break (insert "\n")))


;;; Hooks

(defun cider-run-chained-hook (hook arg)
  "Like `run-hook-with-args' but pass intermediate return values through.
HOOK is a name of a hook (a symbol).  You can use `add-hook' or
`remove-hook' to add functions to this variable.  ARG is passed to first
function.  Its return value is passed to the second function and so forth
till all functions are called or one of them returns nil.  Return the value
return by the last called function."
  (let ((functions (copy-sequence (symbol-value hook))))
    (while (and functions arg)
      (if (eq (car functions) t)
          ;; global value of the hook
          (let ((functions (default-value hook)))
            (while (and functions arg)
              (setq arg (funcall (car functions) arg))
              (setq functions (cdr functions))))
        (setq arg (funcall (car functions) arg)))
      (setq functions (cdr functions)))
    arg))


;;; Font lock

(defalias 'cider--font-lock-ensure
  (if (fboundp 'font-lock-ensure)
      #'font-lock-ensure
    (with-no-warnings
      (lambda (&optional _beg _end)
        (when font-lock-mode
          (font-lock-fontify-buffer))))))

(defalias 'cider--font-lock-flush
  (if (fboundp 'font-lock-flush)
      #'font-lock-flush
    (with-no-warnings
      (lambda (&optional _beg _end)
        (when font-lock-mode
          (font-lock-fontify-buffer))))))

(defvar cider--mode-buffers nil
  "A list of buffers for different major modes.")

(defun cider--make-buffer-for-mode (mode)
  "Return a temp buffer using `major-mode' MODE.
This buffer is not designed to display anything to the user.  For that, use
`cider-make-popup-buffer' instead."
  (setq cider--mode-buffers (seq-filter (lambda (x) (buffer-live-p (cdr x)))
                                        cider--mode-buffers))
  (or (cdr (assq mode cider--mode-buffers))
      (let ((b (generate-new-buffer (format " *cider-temp %s*" mode))))
        (push (cons mode b) cider--mode-buffers)
        (with-current-buffer b
          ;; suppress major mode hooks as we care only about their font-locking
          ;; otherwise modes like whitespace-mode and paredit might interfere
          (setq-local delay-mode-hooks t)
          (setq delayed-mode-hooks nil)
          (funcall mode))
        b)))

(defun cider-ansi-color-string-p (string)
  "Return non-nil if STRING is an ANSI string."
  (string-match "^\\[" string))

(defun cider-font-lock-as (mode string)
  "Use MODE to font-lock the STRING."
  (let ((string (if (cider-ansi-color-string-p string)
                    (substring-no-properties (ansi-color-apply string))
                  string)))
    (if (or (null cider-font-lock-max-length)
            (< (length string) cider-font-lock-max-length))
        (with-current-buffer (cider--make-buffer-for-mode mode)
          (erase-buffer)
          (insert string)
          (font-lock-fontify-region (point-min) (point-max))
          (buffer-string))
      string)))

(defun cider-font-lock-region-as (mode beg end &optional buffer)
  "Use MODE to font-lock text between BEG and END.

Unless you specify a BUFFER it will default to the current one."
  (with-current-buffer (or buffer (current-buffer))
    (let ((text (buffer-substring beg end)))
      (delete-region beg end)
      (goto-char beg)
      (insert (cider-font-lock-as mode text)))))

(defun cider-font-lock-as-clojure (string)
  "Font-lock STRING as Clojure code."
  (cider-font-lock-as 'clojure-mode string))

;; Button allowing use of `font-lock-face', ignoring any inherited `face'
(define-button-type 'cider-plain-button
  'face nil)

(defun cider-add-face (regexp face &optional foreground-only sub-expr object)
  "Propertize all occurrences of REGEXP with FACE.
If FOREGROUND-ONLY is non-nil, change only the foreground of matched
regions.  SUB-EXPR is a sub-expression of REGEXP to be
propertized (defaults to 0).  OBJECT is an object to be
propertized (defaults to current buffer)."
  (setq sub-expr (or sub-expr 0))
  (when (and regexp face)
    (let ((beg 0)
          (end 0))
      (with-current-buffer (or (and (bufferp object) object)
                               (current-buffer))
        (while (if (stringp object)
                   (string-match regexp object end)
                 (re-search-forward regexp nil t))
          (setq beg (match-beginning sub-expr)
                end (match-end sub-expr))
          (if foreground-only
              (let ((face-spec (list (cons 'foreground-color
                                           (face-attribute face :foreground nil t)))))
                (font-lock-prepend-text-property beg end 'face face-spec object))
            (put-text-property beg end 'face face object)))))))


;;; Colors

(defun cider-scale-color (color scale)
  "For a COLOR hex string or name, adjust intensity of RGB components by SCALE."
  (let* ((rgb (color-values color))
         (scaled-rgb (mapcar (lambda (n)
                               (format "%04x" (round (+ n (* scale 65535)))))
                             rgb)))
    (apply #'concat "#" scaled-rgb)))

(defun cider-scale-background-color ()
  "Scale the current background color to get a slighted muted version."
  (let ((color (frame-parameter nil 'background-color))
        (dark (eq (frame-parameter nil 'background-mode) 'dark)))
    (cider-scale-color color (if dark 0.05 -0.05))))

(autoload 'pkg-info-version-info "pkg-info.el")

(defvar cider-version)
(defvar cider-codename)

(defun cider--version ()
  "Retrieve CIDER's version.
A codename is added to stable versions."
  (let ((version (condition-case nil
                     (pkg-info-version-info 'cider)
                   (error cider-version))))
    (if (string-match-p "-snapshot" cider-version)
        version
      (format "%s (%s)" version cider-codename))))


;;; Strings

(defun cider-join-into-alist (candidates &optional separator)
  "Make an alist from CANDIDATES.
The keys are the elements joined with SEPARATOR and values are the original
elements.  Useful for `completing-read' when candidates are complex
objects."
  (mapcar (lambda (el)
            (if (listp el)
                (cons (string-join el (or separator ":")) el)
              (cons el el)))
          candidates))

(defun cider-add-to-alist (symbol car cadr)
  "Add '(CAR CADR) to the alist stored in SYMBOL.
If CAR already corresponds to an entry in the alist, destructively replace
the entry's second element with CADR.

This can be used, for instance, to update the version of an injected
plugin or dependency with:
  (cider-add-to-alist 'cider-jack-in-lein-plugins
                  \"plugin/artifact-name\" \"THE-NEW-VERSION\")"
  (let ((alist (symbol-value symbol)))
    (if-let ((cons (assoc car alist)))
        (setcdr cons (list cadr))
      (set symbol (cons (list car cadr) alist)))))

(defun cider-namespace-qualified-p (sym)
  "Return t if SYM is namespace-qualified."
  (string-match-p "[^/]+/" sym))

(defvar cider-version)

(defconst cider-manual-url "http://cider.readthedocs.org/en/%s/"
  "The URL to CIDER's manual.")

(defun cider--manual-version ()
  "Convert the version to a ReadTheDocs-friendly version."
  (if (string-match-p "-snapshot" cider-version)
      "latest"
    "stable"))

(defun cider-manual-url ()
  "The CIDER manual's url."
  (format cider-manual-url (cider--manual-version)))

;;;###autoload
(defun cider-view-manual ()
  "View the manual in your default browser."
  (interactive)
  (browse-url (cider-manual-url)))

(defun cider--manual-button (label section-id)
  "Return a button string that links to the online manual.
LABEL is the displayed string, and SECTION-ID is where it points
to."
  (with-temp-buffer
    (insert-text-button
     label
     'follow-link t
     'action (lambda (&rest _) (interactive)
               (browse-url (concat (cider-manual-url)
                                   section-id))))
    (buffer-string)))

(defconst cider-refcard-url "https://github.com/clojure-emacs/cider/raw/%s/doc/cider-refcard.pdf"
  "The URL to CIDER's refcard.")

(defun cider--github-version ()
  "Convert the version to a GitHub-friendly version."
  (if (string-match-p "-snapshot" cider-version)
      "master"
    (concat "v" cider-version)))

(defun cider-refcard-url ()
  "The CIDER manual's url."
  (format cider-refcard-url (cider--github-version)))

(defun cider-view-refcard ()
  "View the refcard in your default browser."
  (interactive)
  (browse-url (cider-refcard-url)))

(defconst cider-report-bug-url "https://github.com/clojure-emacs/cider/issues/new"
  "The URL to report a CIDER issue.")

(defun cider-report-bug ()
  "Report a bug in your default browser."
  (interactive)
  (browse-url cider-report-bug-url))

(defun cider--project-name (dir)
  "Extracts a project name from DIR, possibly nil.
The project name is the final component of DIR if not nil."
  (when dir
    (file-name-nondirectory (directory-file-name dir))))

;;; Vectors
(defun cider--deep-vector-to-list (x)
  "Convert vectors in X to lists.
If X is a sequence, return a list of `cider--deep-vector-to-list' applied to
each of its elements.
Any other value is just returned."
  (if (sequencep x)
      (mapcar #'cider--deep-vector-to-list x)
    x))


;;; Help mode

;; Same as https://github.com/emacs-mirror/emacs/blob/86d083438dba60dc00e9e96414bf7e832720c05a/lisp/help-mode.el#L355
;; the original function uses some buffer local variables, but the buffer used
;; is not configurable. It defaults to (help-buffer)

(defun cider--help-setup-xref (item interactive-p buffer)
  "Invoked from commands using the \"*Help*\" buffer to install some xref info.

ITEM is a (FUNCTION . ARGS) pair appropriate for recreating the help
buffer after following a reference.  INTERACTIVE-P is non-nil if the
calling command was invoked interactively.  In this case the stack of
items for help buffer \"back\" buttons is cleared.  Use BUFFER for the
buffer local variables.

This should be called very early, before the output buffer is cleared,
because we want to record the \"previous\" position of point so we can
restore it properly when going back."
  (with-current-buffer buffer
    (when help-xref-stack-item
      (push (cons (point) help-xref-stack-item) help-xref-stack)
      (setq help-xref-forward-stack nil))
    (when interactive-p
      (let ((tail (nthcdr 10 help-xref-stack)))
        ;; Truncate the stack.
        (if tail (setcdr tail nil))))
    (setq help-xref-stack-item item)))

(defcustom cider-doc-xref-regexp "`\\(.*?\\)`"
  "The regexp used to search Clojure vars in doc buffers."
  :type 'regexp
  :safe #'stringp
  :group 'cider
  :package-version '(cider . "0.13.0"))

(defun cider--find-symbol-xref ()
  "Parse and return the first clojure symbol in current buffer.
Use `cider-doc-xref-regexp' for the search.  Set match data and return a
string of the Clojure symbol.  Return nil if there are no more matches in
the buffer."
  (when (re-search-forward cider-doc-xref-regexp nil t)
    (match-string 1)))

(declare-function cider-doc-lookup "cider-doc")
(declare-function cider--eldoc-remove-dot "cider-eldoc")

;; Taken from: https://github.com/emacs-mirror/emacs/blob/65c8c7cb96c14f9c6accd03cc8851b5a3459049e/lisp/help-mode.el#L551-L565
(defun cider--make-back-forward-xrefs (&optional buffer)
  "Insert special references `back' and `forward', as in `help-make-xrefs'.

Optional argument BUFFER is the buffer in which to insert references.
Default is current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (insert "\n")
    (when (or help-xref-stack help-xref-forward-stack)
      (insert "\n"))
    ;; Make a back-reference in this buffer if appropriate.
    (when help-xref-stack
      (help-insert-xref-button help-back-label 'help-back
                               (current-buffer)))
    ;; Make a forward-reference in this buffer if appropriate.
    (when help-xref-forward-stack
      (when help-xref-stack
        (insert "\t"))
      (help-insert-xref-button help-forward-label 'help-forward
                               (current-buffer)))
    (when (or help-xref-stack help-xref-forward-stack)
      (insert "\n"))))

;; Similar to https://github.com/emacs-mirror/emacs/blob/65c8c7cb96c14f9c6accd03cc8851b5a3459049e/lisp/help-mode.el#L404
(defun cider--doc-make-xrefs ()
  "Parse and hyperlink documentation cross-references in current buffer.
Find cross-reference information in a buffer and activate such cross
references for selection with `help-xref'.  Cross-references are parsed
using `cider--find-symbol-xref'.

Special references `back' and `forward' are made to go back and forth
through a stack of help buffers.  Variables `help-back-label' and
`help-forward-label' specify the text for that."
  (interactive "b")

  ;; parse the docstring and create xrefs for symbols
  (save-excursion
    (goto-char (point-min))
    (let ((symbol))
      (while (setq symbol (cider--find-symbol-xref))
        (replace-match "")
        (insert-text-button symbol
                            'type 'help-xref
                            'help-function (apply-partially #'cider-doc-lookup
                                                            (cider--eldoc-remove-dot symbol))))))
  (cider--make-back-forward-xrefs))


;;; Words of inspiration
(defun cider-user-first-name ()
  "Find the current user's first name."
  (let ((name (if (string= (user-full-name) "")
                  (user-login-name)
                (user-full-name))))
    (string-match "^[^ ]*" name)
    (capitalize (match-string 0 name))))

(defvar cider-words-of-inspiration
  `("The best way to predict the future is to invent it. -Alan Kay"
    "A point of view is worth 80 IQ points. -Alan Kay"
    "Lisp isn't a language, it's a building material. -Alan Kay"
    "Simple things should be simple, complex things should be possible. -Alan Kay"
    "Everything should be as simple as possible, but not simpler. -Albert Einstein"
    "Measuring programming progress by lines of code is like measuring aircraft building progress by weight. -Bill Gates"
    "Controlling complexity is the essence of computer programming. -Brian Kernighan"
    "The unavoidable price of reliability is simplicity. -C.A.R. Hoare"
    "You're bound to be unhappy if you optimize everything. -Donald Knuth"
    "Simplicity is prerequisite for reliability. -Edsger W. Dijkstra"
    "Elegance is not a dispensable luxury but a quality that decides between success and failure. -Edsger W. Dijkstra"
    "Deleted code is debugged code. -Jeff Sickel"
    "The key to performance is elegance, not battalions of special cases. -Jon Bentley and Doug McIlroy"
    "First, solve the problem. Then, write the code. -John Johnson"
    "Simplicity is the ultimate sophistication. -Leonardo da Vinci"
    "Programming is not about typing... it's about thinking. -Rich Hickey"
    "Design is about pulling things apart. -Rich Hickey"
    "Programmers know the benefits of everything and the tradeoffs of nothing. -Rich Hickey"
    "Code never lies, comments sometimes do. -Ron Jeffries"
    "The true delight is in the finding out rather than in the knowing. -Isaac Asimov"
    "If paredit is not for you, then you need to become the sort of person that paredit is for. -Phil Hagelberg"
    "Express Yourself. -Madonna"
    "Put on your red shoes and dance the blues. -David Bowie"
    "Do. Or do not. There is no try. -Yoda"
    "The enjoyment of one's tools is an essential ingredient of successful work. -Donald E. Knuth"
    "Not all those who wander are lost. -J.R.R. Tolkien"
    "The best way to learn is to do. -P.R. Halmos"
    "If you wish to make an apple pie from scratch, you must first invent the universe. -Carl Sagan"
    "Learn the rules like a pro, so you can break them like an artist. -Pablo Picasso"
    "The only way of discovering the limits of the possible is to venture a little way past them into the impossible. -Arthur C. Clarke"
    "Don't wish it were easier. Wish you were better. -Jim Rohn"
    "One chord is fine. Two chords is pushing it. Three chords and you're into jazz. -Lou Reed"
    "We are all apprentices in a craft where no one ever becomes a master. -Ernest Hemingway"
    "Clojure isn't a language, it's a building material."
    "Think big!"
    "Think bold!"
    "Think fun!"
    "Code big!"
    "Code bold!"
    "Code fun!"
    "Take this REPL, fellow hacker, and may it serve you well."
    "Let the hacking commence!"
    "Hacks and glory await!"
    "Hack and be merry!"
    "Your hacking starts... NOW!"
    "May the Source be with you!"
    "May the Source shine upon thy REPL!"
    "Code long and prosper!"
    "Happy hacking!"
    "nREPL server is up, CIDER REPL is online!"
    "CIDER REPL operational!"
    "Your imagination is the only limit to what you can do with this REPL!"
    "This REPL is yours to command!"
    "Fame is but a hack away!"
    "The REPL is not enough, but it is such a perfect place to start..."
    "Keep on codin' in the free world!"
    "What we do in the REPL echoes in eternity!"
    "Evaluating is believing."
    "To infinity... and beyond."
    "Showtime!"
    "Unfortunately, no one can be told what CIDER is. You have to figure this out yourself."
    "Procure a bottle of cider to achieve optimum programming results."
    "In parentheses we trust!"
    "Write you some Clojure for Great Good!"
    "Oh, what a day... what a lovely day!"
    "What a day! What cannot be accomplished on such a splendid day!"
    "Home is where your REPL is."
    ,(format "%s, I've a feeling we're not in Kansas anymore."
             (cider-user-first-name))
    ,(format "%s, this could be the start of a beautiful program."
             (cider-user-first-name)))
  "Scientifically-proven optimal words of hackerish encouragement.")

(defun cider-random-words-of-inspiration ()
  "Select a random entry from `cider-words-of-inspiration'."
  (eval (nth (random (length cider-words-of-inspiration))
             cider-words-of-inspiration)))

(defvar cider-tips
  '("Press <\\[cider-connect]> to connect to a running nREPL server."
    "Press <\\[cider-quit]> to quit the current connection."
    "Press <\\[cider-view-manual]> to view CIDER's manual."
    "Press <\\[cider-view-refcard]> to view CIDER's refcard."
    "Press <\\[describe-mode]> to see a list of the keybindings available (this will work in every Emacs buffer)."
    "Press <\\[cider-repl-handle-shortcut]> to quickly invoke some REPL command."
    "Press <\\[cider-switch-to-last-clojure-buffer]> to switch between the REPL and a Clojure source buffer."
    "Press <\\[cider-find-var]> to jump to the source of something (e.g. a var, a Java method)."
    "Press <\\[cider-doc]> to view the documentation for something (e.g. a var, a Java method)."
    "Press <\\[cider-find-resource]> to find a resource on the classpath."
    "Press <\\[cider-selector]> to quickly select a CIDER buffer."
    "Press <\\[cider-test-run-ns-tests]> to run the tests for the current namespace."
    "Press <\\[cider-test-run-loaded-tests]> to run all loaded tests."
    "Press <\\[cider-test-run-project-tests]> to run all tests for the current project."
    "Press <\\[cider-apropos]> to look for a symbol by some search string."
    "Press <\\[cider-apropos-documentation]> to look for a symbol that has some string in its docstring."
    "Press <\\[cider-eval-defun-at-point]> to eval the top-level form at point."
    "Press <\\[cider-eval-buffer]> to eval the entire source buffer."
    "Press <\\[cider-scratch]> to create a Clojure scratchpad. Pretty handy for prototyping."
    "Press <\\[cider-read-and-eval]> to evaluate some Clojure expression directly in the minibuffer."
    "Press <\\[cider-drink-a-sip]> to get more CIDER tips."
    "Press <\\[cider-browse-ns-all]> to start CIDER's namespace browser."
    "Press <\\[cider-classpath]> to start CIDER's classpath browser."
    "Press <\\[cider-repl-history]> to start CIDER's REPL input history browser."
    "Press <\\[cider-macroexpand-1]> to expand the preceding macro."
    "Press <\\[cider-inspect]> to inspect the preceding expression's result."
    "Press <C-u \\[cider-inspect]> to inspect the defun at point's result."
    "Press <C-u C-u \\[cider-inspect]> to read Clojure code from the minibuffer and inspect its result."
    "Press <\\[cider-refresh]> to reload modified and unloaded namespaces."
    "You can define Clojure functions to be called before and after `cider-refresh' (see `cider-refresh-before-fn' and `cider-refresh-after-fn'."
    "Press <\\[cider-display-connection-info]> to view information about the connection."
    "Press <\\[cider-undef]> to undefine a symbol in the current namespace."
    "Press <\\[cider-interrupt]> to interrupt an ongoing evaluation."
    "Use <M-x customize-group RET cider RET> to see every possible setting you can customize."
    "Use <M-x customize-group RET cider-repl RET> to see every possible REPL setting you can customize."
    "Enable `eldoc-mode' to display function & method signatures in the minibuffer."
    "Enable `cider-enlighten-mode' to display the locals of a function when it's executed."
    "Use <\\[cider-close-ancillary-buffers]> to close all ancillary buffers created by CIDER (e.g. *cider-doc*)."
    "Exploring CIDER's menu-bar entries is a great way to discover features."
    "Keep in mind that some commands don't have a keybinding by default. Explore CIDER!"
    "Tweak `cider-repl-prompt-function' to customize your REPL prompt."
    "Tweak `cider-eldoc-ns-function' to customize the way namespaces are displayed by eldoc.")
  "Some handy CIDER tips."
  )

(defun cider-random-tip ()
  "Select a random tip from `cider-tips'."
  (substitute-command-keys (nth (random (length cider-tips)) cider-tips)))

(defun cider-drink-a-sip ()
  "Show a random tip."
  (interactive)
  (message (cider-random-tip)))

(defun cider-column-number-at-pos (pos)
  "Analog to `line-number-at-pos'.
Return buffer column number at position POS."
  (save-excursion
    (goto-char pos)
    ;; we have to adjust the column number by 1 to account for the fact
    ;; that Emacs starts counting columns from 0 and Clojure from 1
    (1+ (current-column))))

(defun cider-propertize (text kind)
  "Propertize TEXT as KIND.
KIND can be the symbols `ns', `var', `emph', `fn', or a face name."
  (propertize text 'face (pcase kind
                           (`fn 'font-lock-function-name-face)
                           (`var 'font-lock-variable-name-face)
                           (`ns 'font-lock-type-face)
                           (`emph 'font-lock-keyword-face)
                           (face face))))

(defun cider--menu-add-help-strings (menu-list)
  "Add a :help entries to items in MENU-LIST."
  (mapcar (lambda (x)
            (cond
             ((listp x) (cider--menu-add-help-strings x))
             ((and (vectorp x)
                   (not (plist-get (append x nil) :help))
                   (functionp (elt x 1)))
              (vconcat x `[:help ,(documentation (elt x 1))]))
             (t x)))
          menu-list))

(provide 'cider-util)

;;; cider-util.el ends here
