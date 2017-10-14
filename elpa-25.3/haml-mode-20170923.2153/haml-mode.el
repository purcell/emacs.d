;;; haml-mode.el --- Major mode for editing Haml files

;; Copyright (c) 2007, 2008 Natalie Weizenbaum

;; Author: Natalie Weizenbaum
;; URL: https://github.com/nex3/haml-mode
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
;; Package-Version: 20170923.2153
;; Package-X-Original-Version: 0
;; Created: 2007-03-08
;; By: Natalie Weizenbaum
;; Keywords: markup, languages, html

;;; Commentary:

;; Because Haml's indentation schema is similar
;; to that of YAML and Python, many indentation-related
;; functions are similar to those in yaml-mode and python-mode.

;; To install, save this on your load path and add the following to
;; your .emacs file:
;;
;; (require 'haml-mode)

;;; Code:

(require 'cl-lib)
(require 'ruby-mode)

;; Additional (optional) libraries for fontification
(require 'css-mode nil t)
(require 'textile-mode nil t)
(require 'markdown-mode nil t)
(require 'js nil t)

;; User definable variables

(defgroup haml nil
  "Support for the Haml template language."
  :group 'languages
  :prefix "haml-")

(defcustom haml-mode-hook nil
  "Hook run when entering Haml mode."
  :type 'hook
  :group 'haml)

(defcustom haml-indent-offset 2
  "Amount of offset per level of indentation."
  :type 'integer
  :group 'haml)

(defcustom haml-backspace-backdents-nesting t
  "Non-nil to have `haml-electric-backspace' re-indent blocks of code.
This means that all code nested beneath the backspaced line is
re-indented along with the line itself."
  :type 'boolean
  :group 'haml)

(defvar haml-indent-function 'haml-indent-p
  "A function for checking if nesting is allowed.
This function should look at the current line and return t
if the next line could be nested within this line.

The function can also return a positive integer to indicate
a specific level to which the current line could be indented.")

(defconst haml-tag-beg-re
  "^[ \t]*\\([%\\.#][a-z0-9_:\\-]+\\)+\\(?:(.*)\\|{.*}\\|\\[.*\\]\\)*"
  "A regexp matching the beginning of a Haml tag, through (), {}, and [].")

(defvar haml-block-openers
  `(,(concat haml-tag-beg-re "[><]*[ \t]*$")
    "^[ \t]*[&!]?[-=~].*do[ \t]*\\(|.*|[ \t]*\\)?$"
    ,(concat "^[ \t]*[&!]?[-=~][ \t]*\\("
             (regexp-opt '("if" "unless" "while" "until" "else" "for"
                           "begin" "elsif" "rescue" "ensure" "when"))
             "\\)")
    "^[ \t]*/\\(\\[.*\\]\\)?[ \t]*$"
    "^[ \t]*-#"
    "^[ \t]*:")
  "A list of regexps that match lines of Haml that open blocks.
That is, a Haml line that can have text nested beneath it should
be matched by a regexp in this list.")


;; Font lock

(defun haml-nested-regexp (re)
  "Create a regexp to match a block starting with RE.
The line containing RE is matched, as well as all lines indented beneath it."
  (concat "^\\([ \t]*\\)\\(" re "\\)\\([ \t]*\\(?:\n\\(?:\\1 +[^\n]*\\)?\\)*\n?\\)$"))

(defconst haml-font-lock-keywords
  `((haml-highlight-interpolation         1 font-lock-variable-name-face prepend)
    (haml-highlight-ruby-tag 1 font-lock-preprocessor-face)
    (haml-highlight-ruby-script 1 font-lock-preprocessor-face)
    ;; TODO: distinguish between "/" comments, which can contain HAML
    ;; output directives, and "-#", which are completely ignored
    haml-highlight-comment
    haml-highlight-filter
    ("^!!!.*"                             0 font-lock-constant-face)
    ("\\s| *$"                            0 font-lock-string-face)))

(defconst haml-filter-re (haml-nested-regexp ":[[:alnum:]_\\-]+"))
(defconst haml-comment-re (haml-nested-regexp "\\(?:-\\#\\|/\\)[^\n]*"))

(defun haml-highlight-comment (limit)
  "Highlight any -# or / comment found up to LIMIT."
  (when (re-search-forward haml-comment-re limit t)
    (let ((beg (match-beginning 0))
          (end (match-end 0)))
      (put-text-property beg end 'face 'font-lock-comment-face)
      (goto-char end))))

;; Fontifying sub-regions for other languages

(defun haml-fontify-region
    (beg end keywords syntax-table syntax-propertize-fn)
  "Fontify a region between BEG and END using another mode's fontification.

KEYWORDS, SYNTAX-TABLE, SYNTACTIC-KEYWORDS and
SYNTAX-PROPERTIZE-FN are the values of that mode's
`font-lock-keywords', `font-lock-syntax-table',
`font-lock-syntactic-keywords', and `syntax-propertize-function'
respectively."
  (save-excursion
    (save-match-data
      (let ((font-lock-keywords keywords)
            (font-lock-syntax-table syntax-table)
            (syntax-propertize-function syntax-propertize-fn)
            (font-lock-multiline 'undecided)
            (font-lock-dont-widen t)
            font-lock-keywords-only
            font-lock-extend-region-functions
            font-lock-keywords-case-fold-search)
        (save-restriction
          (narrow-to-region (1- beg) end)
          ;; font-lock-fontify-region apparently isn't inclusive,
          ;; so we have to move the beginning back one char
          (font-lock-fontify-region (1- beg) end))))))

(defun haml-fontify-region-as-ruby (beg end)
  "Use Ruby's font-lock variables to fontify the region between BEG and END."
  (haml-fontify-region beg end ruby-font-lock-keywords
                       ruby-font-lock-syntax-table
                       (if (fboundp 'ruby-syntax-propertize)
                           'ruby-syntax-propertize
                         'ruby-syntax-propertize-function)))

(defun haml-fontify-region-as-css (beg end)
  "Fontify CSS code from BEG to END.

This requires that `css-mode' is available.
`css-mode' is included with Emacs 23."
  (when (boundp 'css-font-lock-keywords)
    (haml-fontify-region beg end
                         css-font-lock-keywords
                         css-mode-syntax-table
                         'css-syntax-propertize-function)))

(defun haml-fontify-region-as-javascript (beg end)
  "Fontify javascript code from BEG to END.

This requires that Karl Landström's javascript mode be available, either as the
\"js.el\" bundled with Emacs >= 23, or as \"javascript.el\" found in ELPA and
elsewhere."
  (when js--font-lock-keywords-3
    (when (and (fboundp 'js--update-quick-match-re)
               (null js--quick-match-re-func))
      (js--update-quick-match-re))
    (haml-fontify-region beg end
                         js--font-lock-keywords-3
                         js-mode-syntax-table
                         #'js-syntax-propertize)))

(defun haml-fontify-region-as-textile (beg end)
  "Highlight textile from BEG to END.

This requires that `textile-mode' be available.

Note that the results are not perfect, since `textile-mode' expects
certain constructs such as \"h1.\" to be at the beginning of a line,
and indented Haml filters always have leading whitespace."
  (if (boundp 'textile-font-lock-keywords)
      (haml-fontify-region beg end textile-font-lock-keywords textile-mode-syntax-table nil)))

(defun haml-fontify-region-as-markdown (beg end)
  "Highlight markdown from BEG to END.

This requires that `markdown-mode' be available."
  (if (boundp 'markdown-mode-font-lock-keywords)
      (haml-fontify-region beg end
                           markdown-mode-font-lock-keywords
                           markdown-mode-syntax-table
                           nil)))

(defvar haml-fontify-filter-functions-alist
  '(("ruby"       . haml-fontify-region-as-ruby)
    ("css"        . haml-fontify-region-as-css)
    ("javascript" . haml-fontify-region-as-javascript)
    ("textile"    . haml-fontify-region-as-textile)
    ("markdown"   . haml-fontify-region-as-markdown))
  "An alist of (FILTER-NAME . FUNCTION) used to fontify code regions.
FILTER-NAME is a string and FUNCTION is a function which will be
used to fontify the filter's indented code region.  FUNCTION will
be passed the extents of that region in two arguments BEG and
END.")

(defun haml-highlight-filter (limit)
  "Highlight any :filter region found in the text up to LIMIT."
  (when (re-search-forward haml-filter-re limit t)
    ;; fontify the filter name
    (put-text-property (match-beginning 2) (1+ (match-end 2))
                       'face font-lock-preprocessor-face)
    (let ((filter-name (substring (match-string 2) 1))
          (code-start (1+ (match-beginning 3)))
          (code-end (match-end 3)))
      (save-match-data
        (funcall (or (cdr (assoc filter-name haml-fontify-filter-functions-alist))
                     #'(lambda (beg end)
                         (put-text-property beg end
                                            'face
                                            'font-lock-string-face)))
                 code-start code-end))
      (goto-char (match-end 0)))))

(defconst haml-possibly-multiline-code-re
  "\\(\\(?:.*?,[ \t]*\n\\)*.*\\)"
  "Regexp to match trailing ruby code which may continue onto subsequent lines.")

(defconst haml-ruby-script-re
  (concat "^[ \t]*\\(-\\|[&!]?\\(?:=\\|~\\)\\)[^=]" haml-possibly-multiline-code-re)
  "Regexp to match -, = or ~ blocks and any continued code lines.")

(defun haml-highlight-ruby-script (limit)
  "Highlight a Ruby script expression (-, =, or ~).
LIMIT works as it does in `re-search-forward'."
  (when (re-search-forward haml-ruby-script-re limit t)
    (haml-fontify-region-as-ruby (match-beginning 2) (match-end 2))))

(defun haml-move (re)
  "Try matching and moving to the end of regular expression RE.
Returns non-nil if the expression was sucessfully matched."
  (when (looking-at re)
    (goto-char (match-end 0))
    t))

(defun haml-highlight-ruby-tag (limit)
  "Highlight Ruby code within a Haml tag.
LIMIT works as it does in `re-search-forward'.

This highlights the tag attributes and object refs of the tag,
as well as the script expression (-, =, or ~) following the tag.

For example, this will highlight all of the following:
  %p{:foo => 'bar'}
  %p[@bar]
  %p= 'baz'
  %p{:foo => 'bar'}[@bar]= 'baz'"
  (when (re-search-forward "^[ \t]*[%.#]" limit t)
    (forward-char -1)

    ;; Highlight tag, classes, and ids
    (while (haml-move "\\([.#%]\\)[a-z0-9_:\\-]*")
      (put-text-property (match-beginning 0) (match-end 0) 'face
                         (cl-case (char-after (match-beginning 1))
                           (?% font-lock-keyword-face)
                           (?# font-lock-function-name-face)
                           (?. font-lock-variable-name-face))))

    (cl-block loop
      (while t
        (let ((eol (save-excursion (end-of-line) (point))))
          (cl-case (char-after)
            ;; Highlight obj refs
            (?\[
             (forward-char 1)
             (let ((beg (point)))
               (haml-limited-forward-sexp eol)
               (haml-fontify-region-as-ruby beg (point))))
            ;; Highlight new attr hashes
            (?\(
             (forward-char 1)
             (while
                 (and (haml-parse-new-attr-hash
                       (lambda (type beg end)
                         (cl-case type
                           (name (put-text-property beg end
                                                    'face
                                                    font-lock-constant-face))
                           (value (haml-fontify-region-as-ruby beg end)))))
                      (not (eobp)))
               (forward-line 1)
               (beginning-of-line)))
            ;; Highlight old attr hashes
            (?\{
             (let ((beg (point)))
               (haml-limited-forward-sexp eol)

               ;; Check for multiline
               (while (and (eolp) (eq (char-before) ?,) (not (eobp)))
                 (forward-line)
                 (let ((eol (save-excursion (end-of-line) (point))))
                   ;; If no sexps are closed,
                   ;; we're still continuing a  multiline hash
                   (if (>= (car (parse-partial-sexp (point) eol)) 0)
                       (end-of-line)
                     ;; If sexps have been closed,
                     ;; set the point at the end of the total sexp
                     (goto-char beg)
                     (haml-limited-forward-sexp eol))))

               (haml-fontify-region-as-ruby (1+ beg) (point))))
            (t (cl-return-from loop))))))

    ;; Move past end chars
    (haml-move "[<>&!]+")
    ;; Highlight script
    (if (looking-at (concat "\\([=~]\\) " haml-possibly-multiline-code-re))
        (haml-fontify-region-as-ruby (match-beginning 2) (match-end 2))
      ;; Give font-lock something to highlight
      (forward-char -1)
      (looking-at "\\(\\)"))
    t))

(defun haml-highlight-interpolation (limit)
  "Highlight Ruby interpolation (#{foo}).
LIMIT works as it does in `re-search-forward'."
  (when (re-search-forward "\\(#{\\)" limit t)
    (save-match-data
      (forward-char -1)
      (let ((beg (point)))
        (haml-limited-forward-sexp limit)
        (haml-fontify-region-as-ruby (1+ beg) (point)))
      (when (eq (char-before) ?\})
        (put-text-property (1- (point)) (point)
                           'face font-lock-variable-name-face))
      t)))

(defun haml-limited-forward-sexp (limit &optional arg)
  "Move forward using `forward-sexp' or to LIMIT, whichever comes first.
With ARG, do it that many times."
  (let (forward-sexp-function)
    (condition-case err
        (save-restriction
          (narrow-to-region (point) limit)
          (forward-sexp arg))
      (scan-error
       (unless (equal (nth 1 err) "Unbalanced parentheses")
         (signal 'scan-error (cdr err)))
       (goto-char limit)))))

(defun haml-find-containing-block (re)
  "If point is inside a block matching RE, return (start . end) for the block."
  (save-excursion
    (let ((pos (point))
          start end)
      (beginning-of-line)
      (when (and
             (or (looking-at re)
                 (when (re-search-backward re nil t)
                   (looking-at re)))
             (< pos (match-end 0)))
        (setq start (match-beginning 0)
              end (match-end 0)))
      (when start
        (cons start end)))))

(defun haml-maybe-extend-region (extender)
  "Maybe extend the font lock region using EXTENDER.
With point at the beginning of the font lock region, EXTENDER is called.
If it returns a (START . END) pair, those positions are used to possibly
extend the font lock region."
  (let ((old-beg font-lock-beg)
        (old-end font-lock-end))
    (save-excursion
      (goto-char font-lock-beg)
      (let ((new-bounds (funcall extender)))
        (when new-bounds
          (setq font-lock-beg (min font-lock-beg (car new-bounds))
                font-lock-end (max font-lock-end (cdr new-bounds))))))
    (or (/= old-beg font-lock-beg)
        (/= old-end font-lock-end))))

(defun haml-extend-region-nested-below ()
  "Extend the font-lock region to any subsequent indented lines."
  (haml-maybe-extend-region
   (lambda ()
     (beginning-of-line)
     (when (looking-at (haml-nested-regexp "[^ \t].*"))
       (cons (match-beginning 0) (match-end 0))))))

(defun haml-extend-region-to-containing-block (re)
  "Extend the font-lock region to the smallest containing block matching RE."
  (haml-maybe-extend-region
   (lambda ()
     (haml-find-containing-block re))))

(defun haml-extend-region-filter ()
  "Extend the font-lock region to an enclosing filter."
  (haml-extend-region-to-containing-block haml-filter-re))

(defun haml-extend-region-comment ()
  "Extend the font-lock region to an enclosing comment."
  (haml-extend-region-to-containing-block haml-comment-re))

(defun haml-extend-region-ruby-script ()
  "Extend the font-lock region to encompass any current -/=/~ line."
  (haml-extend-region-to-containing-block haml-ruby-script-re))

(defun haml-extend-region-multiline-hashes ()
  "Extend the font-lock region to encompass multiline attribute hashes."
  (haml-maybe-extend-region
   (lambda ()
     (let ((attr-props (haml-parse-multiline-attr-hash))
           multiline-end
           start)
       (when attr-props
         (setq start (cdr (assq 'point attr-props)))

         (end-of-line)
         ;; Move through multiline attrs
         (when (eq (char-before) ?,)
           (save-excursion
             (while (progn (end-of-line)
                           (and (eq (char-before) ?,) (not (eobp))))
               (forward-line))

             (forward-line -1)
             (end-of-line)
             (setq multiline-end (point))))

         (goto-char (+ (cdr (assq 'point attr-props))
                       (cdr (assq 'hash-indent attr-props))
                       -1))
         (haml-limited-forward-sexp
          (or multiline-end
              (save-excursion (end-of-line) (point))))
         (cons start (point)))))))

(defun haml-extend-region-contextual ()
  "Extend the font lock region piecemeal.

The result of calling this function repeatedly until it returns
nil is that (FONT-LOCK-BEG . FONT-LOCK-END) will be the smallest
possible region in which font-locking could be affected by
changes in the initial region."
  (or
   (haml-extend-region-filter)
   (haml-extend-region-comment)
   (haml-extend-region-ruby-script)
   (haml-extend-region-multiline-hashes)
   (haml-extend-region-nested-below)
   (font-lock-extend-region-multiline)))


;; Mode setup

(defvar haml-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?' "\"" table)
    table)
  "Syntax table in use in `haml-mode' buffers.")

(defvar haml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [backspace] 'haml-electric-backspace)
    (define-key map "\C-?" 'haml-electric-backspace)
    (define-key map "\C-c\C-f" 'haml-forward-sexp)
    (define-key map "\C-c\C-b" 'haml-backward-sexp)
    (define-key map "\C-c\C-u" 'haml-up-list)
    (define-key map "\C-c\C-d" 'haml-down-list)
    (define-key map "\C-c\C-k" 'haml-kill-line-and-indent)
    (define-key map "\C-c\C-r" 'haml-output-region)
    (define-key map "\C-c\C-l" 'haml-output-buffer)
    map))

;;;###autoload
(define-derived-mode haml-mode prog-mode "Haml"
  "Major mode for editing Haml files.

\\{haml-mode-map}"
  (setq font-lock-extend-region-functions '(haml-extend-region-contextual))
  (set (make-local-variable 'jit-lock-contextually) t)
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'indent-line-function) 'haml-indent-line)
  (set (make-local-variable 'indent-region-function) 'haml-indent-region)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'comment-start) "-#")
  (setq font-lock-defaults '((haml-font-lock-keywords) t t))
  (when (boundp 'electric-indent-inhibit)
    (setq electric-indent-inhibit t))
  (setq indent-tabs-mode nil))

;; Useful functions

(defun haml-comment-block ()
  "Comment the current block of Haml code."
  (interactive)
  (save-excursion
    (let ((indent (current-indentation)))
      (back-to-indentation)
      (insert "-#")
      (newline)
      (indent-to indent)
      (beginning-of-line)
      (haml-mark-sexp)
      (haml-reindent-region-by haml-indent-offset))))

(defun haml-uncomment-block ()
  "Uncomment the current block of Haml code."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (while (not (looking-at haml-comment-re))
      (haml-up-list)
      (beginning-of-line))
    (haml-mark-sexp)
    (kill-line 1)
    (haml-reindent-region-by (- haml-indent-offset))))

(defun haml-replace-region (start end)
  "Replace the current block of Haml code with the HTML equivalent.
Called from a program, START and END specify the region to indent."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (let ((ci (current-indentation)))
      (while (re-search-forward "^ +" end t)
        (replace-match (make-string (- (current-indentation) ci) ? ))))
    (shell-command-on-region start end "haml" "haml-output" t)))

(defun haml-output-region (start end)
  "Displays the HTML output for the current block of Haml code.
Called from a program, START and END specify the region to indent."
  (interactive "r")
  (kill-new (buffer-substring start end))
  (with-temp-buffer
    (yank)
    (haml-indent-region (point-min) (point-max))
    (shell-command-on-region (point-min) (point-max) "haml" "haml-output")))

(defun haml-output-buffer ()
  "Displays the HTML output for entire buffer."
  (interactive)
  (haml-output-region (point-min) (point-max)))

;; Navigation

(defun haml-forward-through-whitespace (&optional backward)
  "Move the point forward through any whitespace.
The point will move forward at least one line, until it reaches
either the end of the buffer or a line with no whitespace.

If BACKWARD is non-nil, move the point backward instead."
  (let ((arg (if backward -1 1))
        (endp (if backward 'bobp 'eobp)))
    (cl-loop do (forward-line arg)
             while (and (not (funcall endp))
                        (looking-at "^[ \t]*$")))))

(defun haml-at-indent-p ()
  "Return non-nil if the point is before any text on the line."
  (let ((opoint (point)))
    (save-excursion
      (back-to-indentation)
      (>= (point) opoint))))

(defun haml-forward-sexp (&optional arg)
  "Move forward across one nested expression.
With ARG, do it that many times.  Negative arg -N means move
backward across N balanced expressions.

A sexp in Haml is defined as a line of Haml code as well as any
lines nested beneath it."
  (interactive "p")
  (or arg (setq arg 1))
  (if (and (< arg 0) (not (haml-at-indent-p)))
      (back-to-indentation)
    (while (/= arg 0)
      (let ((indent (current-indentation)))
        (cl-loop do (haml-forward-through-whitespace (< arg 0))
                 while (and (not (eobp))
                            (not (bobp))
                            (> (current-indentation) indent)))
        (unless (eobp)
          (back-to-indentation))
        (setq arg (+ arg (if (> arg 0) -1 1)))))))

(defun haml-backward-sexp (&optional arg)
  "Move backward across one nested expression.
With ARG, do it that many times.  Negative arg -N means move
forward across N balanced expressions.

A sexp in Haml is defined as a line of Haml code as well as any
lines nested beneath it."
  (interactive "p")
  (haml-forward-sexp (if arg (- arg) -1)))

(defun haml-up-list (&optional arg)
  "Move out of one level of nesting.
With ARG, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (let ((indent (current-indentation)))
      (cl-loop do (haml-forward-through-whitespace t)
               while (and (not (bobp))
                          (>= (current-indentation) indent)))
      (setq arg (1- arg))))
  (back-to-indentation))

(defun haml-down-list (&optional arg)
  "Move down one level of nesting.
With ARG, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (let ((indent (current-indentation)))
      (haml-forward-through-whitespace)
      (when (<= (current-indentation) indent)
        (haml-forward-through-whitespace t)
        (back-to-indentation)
        (error "Nothing is nested beneath this line"))
      (setq arg (1- arg))))
  (back-to-indentation))

(defun haml-mark-sexp ()
  "Mark the next Haml block."
  (let ((forward-sexp-function 'haml-forward-sexp))
    (mark-sexp)))

(defun haml-mark-sexp-but-not-next-line ()
  "Mark the next Haml block, but not the next line.
Put the mark at the end of the last line of the sexp rather than
the first non-whitespace character of the next line."
  (haml-mark-sexp)
  (set-mark
   (save-excursion
     (goto-char (mark))
     (unless (eobp)
       (forward-line -1)
       (end-of-line))
     (point))))

;; Indentation and electric keys

(defvar haml-empty-elements
  '("area" "base" "br" "col" "command" "embed" "hr" "img" "input"
    "keygen" "link" "meta" "param" "source" "track" "wbr")
  "A list of html elements which may not contain content.

See http://www.w3.org/TR/html-markup/syntax.html.")

(defun haml-unnestable-tag-p ()
  "Return t if the current line is an empty element tag, or one with content."
  (when (looking-at haml-tag-beg-re)
    (save-excursion
      (goto-char (match-end 0))
      (or (string-match-p (concat "%" (regexp-opt haml-empty-elements) "\\b")
                          (match-string 1))
          (progn
            (when (looking-at "[{(]")
              (ignore-errors (forward-sexp)))
            (looking-at "\\(?:=\\|==\\| \\)[[:blank:]]*[^[:blank:]\r\n]+"))))))

(defun haml-indent-p ()
  "Return t if the current line can have lines nested beneath it."
  (let ((attr-props (haml-parse-multiline-attr-hash)))
    (if attr-props
        (if (haml-unclosed-attr-hash-p)
            (cdr (assq 'hash-indent attr-props))
          (+ (cdr (assq 'indent attr-props)) haml-indent-offset))
      (unless (or (haml-unnestable-tag-p))
        (cl-loop for opener in haml-block-openers
                 if (looking-at opener) return t
                 finally return nil)))))

(cl-defun haml-parse-multiline-attr-hash ()
  "Parses a multiline attribute hash, and returns
an alist with the following keys:

INDENT is the indentation of the line beginning the hash.

HASH-INDENT is the indentation of the first character
within the attribute hash.

POINT is the character position at the beginning of the line
beginning the hash."
  (save-excursion
    (while t
      (beginning-of-line)
      (if (looking-at (concat haml-tag-beg-re "\\([{(]\\)"))
          (progn
            (goto-char (1- (match-end 0)))
            (haml-limited-forward-sexp (save-excursion (end-of-line) (point)))
            (cl-return-from haml-parse-multiline-attr-hash
              (when (or (string-equal (match-string 1) "(") (eq (char-before) ?,))
                `((indent . ,(current-indentation))
                  (hash-indent . ,(- (match-end 0) (match-beginning 0)))
                  (point . ,(match-beginning 0))))))
        (when (bobp) (cl-return-from haml-parse-multiline-attr-hash))
        (forward-line -1)
        (unless (haml-unclosed-attr-hash-p)
          (cl-return-from haml-parse-multiline-attr-hash))))))

(cl-defun haml-unclosed-attr-hash-p ()
  "Return t if this line has an unclosed attribute hash, new or old."
  (save-excursion
    (end-of-line)
    (when (eq (char-before) ?,) (cl-return-from haml-unclosed-attr-hash-p t))
    (re-search-backward "(\\|^")
    (haml-move "(")
    (haml-parse-new-attr-hash)))

(cl-defun haml-parse-new-attr-hash (&optional (fn (lambda (type beg end) ())))
  "Parse a new-style attribute hash on this line, and returns
t if it's not finished on the current line.

FN should take three parameters: TYPE, BEG, and END.
TYPE is the type of text parsed ('name or 'value)
and BEG and END delimit that text in the buffer."
  (let ((eol (save-excursion (end-of-line) (point))))
    (while (not (haml-move ")"))
      (haml-move "[ \t]*")
      (unless (haml-move "[a-z0-9_:\\-]+")
        (cl-return-from haml-parse-new-attr-hash (haml-move "[ \t]*$")))
      (funcall fn 'name (match-beginning 0) (match-end 0))
      (haml-move "[ \t]*")
      (when (haml-move "=")
        (haml-move "[ \t]*")
        (unless (looking-at "[\"'@a-z0-9]") (cl-return-from haml-parse-new-attr-hash))
        (let ((beg (point)))
          (haml-limited-forward-sexp eol)
          (funcall fn 'value beg (point)))
        (haml-move "[ \t]*")))
    nil))

(defun haml-compute-indentation ()
  "Calculate the maximum sensible indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (if (bobp) (list 0 nil)
      (haml-forward-through-whitespace t)
      (let ((indent (funcall haml-indent-function)))
        (cond
         ((consp indent) indent)
         ((integerp indent) (list indent t))
         (indent (list (+ (current-indentation) haml-indent-offset) nil))
         (t (list (current-indentation) nil)))))))

(defun haml-indent-region (start end)
  "Indent each nonblank line in the region.
This is done by indenting the first line based on
`haml-compute-indentation' and preserving the relative
indentation of the rest of the region.  START and END specify the
region to indent.

If this command is used multiple times in a row, it will cycle
between possible indentations."
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (let (this-line-column current-column
                           (next-line-column
                            (if (and (equal last-command this-command) (/= (current-indentation) 0))
                                (* (/ (1- (current-indentation)) haml-indent-offset) haml-indent-offset)
                              (car (haml-compute-indentation)))))
      (while (< (point) end)
        (setq this-line-column next-line-column
              current-column (current-indentation))
        ;; Delete whitespace chars at beginning of line
        (delete-horizontal-space)
        (unless (eolp)
          (setq next-line-column (save-excursion
                                   (cl-loop do (forward-line 1)
                                            while (and (not (eobp)) (looking-at "^[ \t]*$")))
                                   (+ this-line-column
                                      (- (current-indentation) current-column))))
          ;; Don't indent an empty line
          (unless (eolp) (indent-to this-line-column)))
        (forward-line 1)))
    (move-marker end nil)))

(defun haml-indent-line ()
  "Indent the current line.
The first time this command is used, the line will be indented to the
maximum sensible indentation.  Each immediately subsequent usage will
back-dent the line by `haml-indent-offset' spaces.  On reaching column
0, it will cycle back to the maximum sensible indentation."
  (interactive "*")
  (let ((ci (current-indentation))
        (cc (current-column)))
    (cl-destructuring-bind (need strict) (haml-compute-indentation)
      (save-excursion
        (beginning-of-line)
        (delete-horizontal-space)
        (if (and (not strict) (equal last-command this-command) (/= ci 0))
            (indent-to (* (/ (1- ci) haml-indent-offset) haml-indent-offset))
          (indent-to need))))
    (when (< (current-column) (current-indentation))
      (forward-to-indentation 0))))

(defun haml-reindent-region-by (n)
  "Add N spaces to the beginning of each line in the region.
If N is negative, will remove the spaces instead.  Assumes all
lines in the region have indentation >= that of the first line."
  (let* ((ci (current-indentation))
         (indent-rx
          (concat "^"
                  (if indent-tabs-mode
                      (concat (make-string (/ ci tab-width) ?\t)
                              (make-string (mod ci tab-width) ?\t))
                    (make-string ci ?\s)))))
    (save-excursion
      (while (re-search-forward indent-rx (mark) t)
        (let ((ci (current-indentation)))
          (delete-horizontal-space)
          (beginning-of-line)
          (indent-to (max 0 (+ ci n))))))))

(defun haml-electric-backspace (arg)
  "Delete characters or back-dent the current line.
If invoked following only whitespace on a line, will back-dent
the line and all nested lines to the immediately previous
multiple of `haml-indent-offset' spaces.  With ARG, do it that
many times.

Set `haml-backspace-backdents-nesting' to nil to just back-dent
the current line."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column))
          (bolp)
          (save-excursion
            (beginning-of-line)
            (looking-at "^[ \t]+$")))
      (backward-delete-char arg)
    (save-excursion
      (beginning-of-line)
      (unwind-protect
          (progn
            (if haml-backspace-backdents-nesting
                (haml-mark-sexp-but-not-next-line)
              (set-mark (save-excursion (end-of-line) (point))))
            (haml-reindent-region-by (* (- arg) haml-indent-offset)))
        (pop-mark)))
    (back-to-indentation)))

(defun haml-kill-line-and-indent ()
  "Kill the current line, and re-indent all lines nested beneath it."
  (interactive)
  (beginning-of-line)
  (haml-mark-sexp-but-not-next-line)
  (kill-line 1)
  (haml-reindent-region-by (* -1 haml-indent-offset)))

(defun haml-indent-string ()
  "Return the indentation string for `haml-indent-offset'."
  (mapconcat 'identity (make-list haml-indent-offset " ") ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))


;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; eval: (checkdoc-minor-mode 1)
;; End:

(provide 'haml-mode)
;;; haml-mode.el ends here
