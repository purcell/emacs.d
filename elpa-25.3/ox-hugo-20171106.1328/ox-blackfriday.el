;;; ox-blackfriday.el --- Blackfriday Markdown Back-End for Org Export Engine  -*- lexical-binding: t -*-

;; Authors: Matt Price <moptop99@gmail.com>
;;          Kaushal Modi <kaushal.modi@gmail.com>
;; URL: https://ox-hugo.scripter.co
;; Package-Requires: ((emacs "24.5"))
;; Version: 0.1

;;; Commentary:

;; This library implements a Markdown back-end (Blackfriday flavor
;; (https://github.com/russross/blackfriday)) for Org exporter, based
;; on the ox-md exporter.

;; It started off as a clone of Lars Tveito's GitHub Flavored Markdown
;; exporter (https://github.com/larstvei/ox-gfm).

;;; Code:

(require 'ox-md)
(require 'ox-publish)


;;; Variables

(defvar org-blackfriday-width-cookies nil)
(defvar org-blackfriday-width-cookies-table nil)

(defconst org-blackfriday-table-left-border "")
(defconst org-blackfriday-table-right-border " ")
(defconst org-blackfriday-table-separator "| ")

(defvar org-blackfriday--hrule-inserted nil
  "State variable to track if the horizontal rule was inserted.
This check is specifically track if that horizontal rule was
inserted after the first row of the table.")


;;; User-Configurable Variables

(defgroup org-export-blackfriday nil
  "Options for exporting Org mode files to Blackfriday Markdown."
  :tag "Org Export Blackfriday"
  :group 'org-export)


;;; Define Back-End

(org-export-define-derived-backend 'blackfriday 'md
  :filters-alist '((:filter-parse-tree . org-md-separate-elements))
  ;; Do not clutter the *Org Exporter Dispatch* menu.
  ;; :menu-entry
  ;; '(?b "Export to Blackfriday Flavored Markdown"
  ;;      ((?B "To temporary buffer"
  ;;           (lambda (a s v b) (org-blackfriday-export-as-markdown a s v)))
  ;;       (?b "To file" (lambda (a s v b) (org-blackfriday-export-to-markdown a s v)))
  ;;       (?o "To file and open"
  ;;           (lambda (a s v b)
  ;;             (if a (org-blackfriday-export-to-markdown t s v)
  ;;               (org-open-file (org-blackfriday-export-to-markdown nil s v)))))))
  :translate-alist '((example-block . org-blackfriday-example-block)
                     (fixed-width . org-blackfriday-fixed-width) ;Org Babel Results
                     (footnote-reference . org-blackfriday-footnote-reference)
                     (inner-template . org-blackfriday-inner-template)
                     (italic . org-blackfriday-italic)
                     (item . org-blackfriday-item)
                     (latex-fragment . org-blackfriday-latex-fragment)
                     (plain-list . org-blackfriday-plain-list)
                     (quote-block . org-blackfriday-quote-block)
                     (src-block . org-blackfriday-src-block)
                     (strike-through . org-blackfriday-strike-through)
                     (table-cell . org-blackfriday-table-cell)
                     (table-row . org-blackfriday-table-row)
                     (table . org-blackfriday-table)
                     (verse-block . org-blackfriday-verse-block)))


;;; Miscellaneous Helper Functions

;;;; Table of contents
(defun org-blackfriday-format-toc (headline info)
  "Return an appropriate table of contents entry for HEADLINE.

INFO is a plist used as a communication channel."
  (let* ((title (org-export-data (org-export-get-alt-title headline info) info))
         (level (1- (org-element-property :level headline)))
         (indent (concat (make-string (* level 2) ? )))
         (anchor (or (org-element-property :CUSTOM_ID headline)
                     (org-export-get-reference headline info))))
    (concat indent "- [" title "]" "(#" anchor ")")))

;;;; Footnote section
(defun org-blackfriday-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (let ((fn-alist (org-export-collect-footnote-definitions info))
        ;; Fri Jul 21 14:33:25 EDT 2017 - kmodi
        ;; TODO: Need to learn using cl-loop
        ;; Below form from ox-md did not work.
        ;; (fn-alist-stripped
        ;;  (cl-loop for (n raw) in fn-alist collect
        ;;           (cons n (org-trim (org-export-data raw info)))))
        fn-alist-stripped)
    (let ((n 1)
          def)
      (dolist (fn fn-alist)
        ;; (message "fn: %S" fn)
        ;; (message "fn: %s" (org-export-data fn info)) ;This gives error
        ;; (message "fn nth 2 car: %s" (org-export-data (nth 2 fn) info))
        (setq def (org-trim (org-export-data (nth 2 fn) info)))
        ;; Support multi-line footnote definitions by folding all
        ;; footnote definition lines into a single line as Blackfriday
        ;; does not support that.
        (setq def (replace-regexp-in-string "\n" " " def))
        ;; Replace multiple consecutive spaces with a single space.
        (setq def (replace-regexp-in-string "[[:blank:]]+" " " def))
        (push (cons n def) fn-alist-stripped)
        (setq n (1+ n))))
    (when fn-alist-stripped
      (mapconcat (lambda (fn)
                   ;; (message "dbg: fn: %0d -- %s" (car fn) (cdr fn))
                   (format "[^fn:%d]: %s"
                           (car fn)     ;footnote number
                           (cdr fn)))   ;footnote definition
                 (nreverse fn-alist-stripped)
                 "\n"))))

;;;; Table-Common
(defun org-blackfriday-table-col-width (table column info)
  "Return width of TABLE at given COLUMN using INFO.

INFO is a plist used as communication channel.  Width of a column
is determined either by inquiring
`org-blackfriday-width-cookies' in the column, or by the maximum
cell with in the column."
  (let ((cookie (when (hash-table-p org-blackfriday-width-cookies)
                  (gethash column org-blackfriday-width-cookies))))
    (if (and (eq table org-blackfriday-width-cookies-table)
             (not (eq nil cookie)))
        cookie
      (progn
        (unless (and (eq table org-blackfriday-width-cookies-table)
                     (hash-table-p org-blackfriday-width-cookies))
          (setq org-blackfriday-width-cookies (make-hash-table))
          (setq org-blackfriday-width-cookies-table table))
        (let ((max-width 0)
              (specialp (org-export-table-has-special-column-p table)))
          (org-element-map
              table
              'table-row
            (lambda (row)
              (setq max-width
                    (max (length
                          (org-export-data
                           (org-element-contents
                            (elt (if specialp
                                     (car (org-element-contents row))
                                   (org-element-contents row))
                                 column))
                           info))
                         max-width)))
            info)
          (puthash column max-width org-blackfriday-width-cookies))))))

(defun org-blackfriday-make-hline-builder (table info char)
  "Return a function to horizontal lines in TABLE.
Draw the lines using INFO with given CHAR.

INFO is a plist used as a communication channel."
  `(lambda (col)
     (let ((max-width (max 3 (+ 1 (org-blackfriday-table-col-width ,table col ,info)))))
       (when (< max-width 1)
         (setq max-width 1))
       (make-string max-width ,char))))

;;;; Plain List Helper
(defun org-blackfriday--ordered-list-with-custom-counter-p (plain-list)
  "Return non-nil is PLAIN-LIST element has an item with custom counter.
Returns nil immediately if PLAIN-LIST is not an ordered list."
  (let ((type (org-element-property :type plain-list))
        has-custom-counter)
    (when (eq 'ordered type)
      (let* ((list-contents (org-element-contents plain-list)))
        (dolist (el list-contents)
          (when (eq 'item (car el))
            (let* ((item-plist (car (cdr el)))
                   (counter (plist-get item-plist :counter)))
              ;; (message "dbg: %S" counter)
              (when counter
                (setq has-custom-counter t)))))))
    ;; (message "has custom counter: %S" has-custom-counter)
    has-custom-counter))


;;; Transcode Functions

;;;; Example Block
(defun org-blackfriday-example-block (example-block _contents info)
  "Transcode an EXAMPLE-BLOCK element into Blackfriday Markdown format.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "```text\n%s```"
          (org-export-format-code-default example-block info)))

;;;; Fixed Width
(defun org-blackfriday-fixed-width (fixed-width _contents info)
  "Transcode a FIXED-WIDTH element into Blackfriday Markdown format.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "```text\n%s```"
          (let ((org-src-preserve-indentation t))
            ;; Preserve leading whitespace in the Org Babel Results
            ;; blocks.
            (org-export-format-code-default fixed-width info))))

;;;; Footnote Reference
(defun org-blackfriday-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element into Blackfriday Markdown format.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; (message "footref: %s" footnote-reference)
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (and (eq (org-element-type prev) 'footnote-reference)
          (plist-get info :html-footnote-separator)))
   (format "[^fn:%d]" (org-export-get-footnote-number footnote-reference info))))

;;;; Inner Template
(defun org-blackfriday-inner-template (contents info)
  "Return body of document after converting it to Markdown syntax.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ((depth (plist-get info :with-toc))
         (headlines (and depth (org-export-collect-headlines info depth)))
         (toc-tail (if headlines "\n\n" ""))
         (toc-string ""))

    (when headlines
      (dolist (headline headlines)
        (setq toc-string (concat toc-string
                                 (org-blackfriday-format-toc headline info)
                                 "\n"))))
    (org-trim (concat toc-string toc-tail contents "\n" (org-blackfriday-footnote-section info)))))

;;;; Italic
(defun org-blackfriday-italic (_italic contents _info)
  "Transcode ITALIC object into Markdown format.
CONTENTS is the text within italic markup.  INFO is a plist used
as a communication channel."
  ;; (format "*%s*" contents)
  ;; While above also works in almost all cases, it fails in cases
  ;; like "*This is in italic, **and this is in bold-italics**, and
  ;; back to just italic.*".
  ;; As `org-md-bold' uses ** to mark bold text, switching to using
  ;; underscores only for italics.
  (format "_%s_" contents))

;;;; Item (list item)
(defun org-blackfriday-item (item contents info)
  "Transcode an ITEM element into Blackfriday Markdown format.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((parent-list (org-export-get-parent item)))
    ;; If this item is in an ordered list and if this or any other
    ;; item in this list is using a custom counter, export this list
    ;; item in HTML.
    (if (org-blackfriday--ordered-list-with-custom-counter-p parent-list)
        (org-html-format-list-item contents 'ordered nil info
                                   (org-element-property :counter item))
      (org-md-item item contents info))))

;;;; Latex Fragment
(defun org-blackfriday-latex-fragment (latex-fragment _contents info)
  "Transcode a LATEX-FRAGMENT object into Blackfriday Markdown format.
INFO is a plist holding contextual information."
  (let ((latex-frag (org-element-property :value latex-fragment))
        (processing-type (plist-get info :with-latex)))
    (cond
     ((memq processing-type '(t mathjax))
      (let* ((frag (org-html-format-latex latex-frag 'mathjax info))
             ;; https://gohugo.io/content-management/formats#solution
             (frag (replace-regexp-in-string "_" "\\\\_" frag)) ;_ -> \_
             ;; Need to escape the backslash in "\(", "\)", .. to
             ;; make Blackfriday happy. So \( -> \\(, \) -> \\),
             ;; \[ -> \\[ and \] -> \\].
             (frag (replace-regexp-in-string "\\(\\\\[]()[]\\)" "\\\\\\1" frag)))
        frag))
     ((assq processing-type org-preview-latex-process-alist)
      (let ((formula-link
             (org-html-format-latex latex-frag processing-type info)))
        (when (and formula-link (string-match "file:\\([^]]*\\)" formula-link))
          (org-html--format-image (match-string 1 formula-link) nil info))))
     (t latex-frag))))

;;;; Plain List
(defun org-blackfriday-plain-list (plain-list contents info)
  "Transcode PLAIN-LIST element into Blackfriday Markdown format.
CONTENTS is the plain-list contents.  INFO is a plist used as a
communication channel."
  (if (org-blackfriday--ordered-list-with-custom-counter-p plain-list)
      ;; If this is an ordered list and if any item in this list is
      ;; using a custom counter, export this list in HTML.
      (org-html-plain-list plain-list contents info)
    (let* ((next (org-export-get-next-element plain-list info))
           (next-type (org-element-type next))
           (next-is-list (eq 'plain-list next-type)))
      (concat contents
              ;; Two consecutive lists in Markdown can be separated by
              ;; a comment.
              (when next-is-list
                "\n<!--listend-->")))))

;;;; Quote Block
(defun org-blackfriday-quote-block (quote-block contents info)
  "Transcode QUOTE-BLOCK element into Blackfriday Markdown format.
CONTENTS is the quote-block contents.  INFO is a plist used as a
communication channel."
  (let* ((next (org-export-get-next-element quote-block info))
         (next-type (org-element-type next))
         (next-is-quote (eq 'quote-block next-type))
         (contents (org-md-quote-block quote-block contents info)))
    (concat contents
            ;; Two consecutive blockquotes in Markdown can be
            ;; separated by a comment.
            (when next-is-quote
              "\n\n<!--quoteend-->"))))

;;;; Src Block
(defun org-blackfriday-src-block (src-block _contents info)
  "Transcode SRC-BLOCK element into Blackfriday Markdown format.

INFO is a plist used as a communication channel."
  (let* ((lang (org-element-property :language src-block))
         (code (org-export-format-code-default src-block info))
         (parent-element (org-export-get-parent src-block))
         (parent-type (car parent-element)))
    ;; (message "ox-bf [dbg] code: %s" code)
    ;; (message "dbg parent type: %S" parent-type)
    ;; Hack to avert a bug in Blackfriday
    ;; Details: https://github.com/kaushalmodi/ox-hugo/issues/57
    ;; Prefix the ASTERISK (0x2a), PLUS SIGN (0x2b) and HYPHEN-MINUS
    ;; (0x2d) characters with ZERO WIDTH SPACE (0x200b), if they
    ;; appear at BOL.
    ;; FIXME: Remove this hack in the when form when
    ;; https://github.com/russross/blackfriday/issues/239 is resolved.
    (when (equal 'item parent-type)
      (setq code (replace-regexp-in-string "^[-+*] " "​\\&" code)))
    ;; There's a ZERO WIDTH SPACE char (0x200b) here ^^,
    ;;                            (after «"», but before «\\&"» above)
    ;; It's not visible (because zero width), but it's there.
    (format "```%s\n%s```" lang code)))

;;;; Strike-Through
(defun org-blackfriday-strike-through (_strike-through contents _info)
  "Transcode strike-through text into Blackfriday Markdown format.
CONTENTS contains the text with strike-through markup."
  (format "~~%s~~" contents))

;;;; Table-Cell
(defun org-blackfriday-table-cell (table-cell contents info)
  "Transcode TABLE-CELL element into Blackfriday Markdown format.

CONTENTS is content of the cell.  INFO is a plist used as a
communication channel."
  ;; (message "[ox-bf-table-cell DBG] In contents: %s" contents)
  (let* ((table (org-export-get-parent-table table-cell))
         (column (cdr (org-export-table-cell-address table-cell info)))
         (width (org-blackfriday-table-col-width table column info))
         (left-border (if (org-export-table-cell-starts-colgroup-p table-cell info) "| " " "))
         (right-border " |")
         (data (or contents ""))
         (cell (concat left-border
                       data
                       (make-string (max 0 (- width (string-width data))) ?\s)
                       right-border))
         (cell-width (length cell)))
    ;; Each cell needs to be at least 3 characters wide (4 chars,
    ;; including the table border char "|"); otherwise the export
    ;; is not rendered as a table
    (when (< cell-width 4)
      (setq cell (concat (make-string (- 4 cell-width) ? ) cell)))
    ;; (message "[ox-bf-table-cell DBG] Cell:\n%s" cell)
    cell))

;;;; Table-Row
(defun org-blackfriday-table-row (table-row contents info)
  "Transcode TABLE-ROW element into Blackfriday Markdown format.

CONTENTS is cell contents of TABLE-ROW.  INFO is a plist used as a
communication channel."
  (let* ((table (org-export-get-parent-table table-row))
         (row-num (cl-position          ;Begins with 0
                   table-row
                   (org-element-map table 'table-row #'identity info)))
         (row contents)) ;If CONTENTS is `nil', row has to be returned as `nil' too
    ;; Reset the state variable when the first row of the table is
    ;; received.
    (when (eq 0 row-num)
      (setq org-blackfriday--hrule-inserted nil))

    ;; (message "[ox-bf-table-row DBG] Row # %0d In contents: %s,\ntable-row: %S" row-num contents table-row)
    (when row
      (progn
        (when (and (eq 'rule (org-element-property :type table-row))
                   ;; In Blackfriday, rule is valid only at second row.
                   (eq 1 row-num))
          (let* ((table (org-export-get-parent-table table-row))
                 ;; (headerp (org-export-table-row-starts-header-p table-row info))
                 (build-rule (org-blackfriday-make-hline-builder table info ?-))
                 (cols (cdr (org-export-table-dimensions table info))))
            (setq row (concat org-blackfriday-table-left-border
                              (mapconcat (lambda (col)
                                           (funcall build-rule col))
                                         (number-sequence 0 (- cols 1))
                                         org-blackfriday-table-separator)
                              org-blackfriday-table-right-border))))

        ;; If the first table row is "abc | def", it needs to have a rule
        ;; under it for Blackfriday to detect the whole object as a table.
        (when (and (stringp row)
                   (null org-blackfriday--hrule-inserted))
          (let ((rule (replace-regexp-in-string "[^|]" "-" row)))
            (setq row (concat row "\n" rule))
            (setq org-blackfriday--hrule-inserted t)))))
    ;; (message "[ox-bf-table-row DBG] Row:\n%s" row)
    row))

;;;; Table
(defun org-blackfriday-table (table contents info)
  "Transcode TABLE element into Blackfriday Markdown format.

CONTENTS is contents of the table.  INFO is a plist holding
contextual information."
  ;; (message "[ox-bf-table DBG] In contents: %s" contents)
  (let* ((rows (org-element-map table 'table-row 'identity info))
         (no-header (or (<= (length rows) 1)))
         (cols (cdr (org-export-table-dimensions table info)))
         (build-dummy-header
          (function
           (lambda ()
             (let ((build-empty-cell (org-blackfriday-make-hline-builder table info ?\s))
                   (build-rule (org-blackfriday-make-hline-builder table info ?-))
                   (columns (number-sequence 0 (- cols 1))))
               (concat org-blackfriday-table-left-border
                       (mapconcat (lambda (col)
                                    (funcall build-empty-cell col))
                                  columns
                                  org-blackfriday-table-separator)
                       org-blackfriday-table-right-border "\n" org-blackfriday-table-left-border
                       (mapconcat (lambda (col)
                                    (funcall build-rule col))
                                  columns
                                  org-blackfriday-table-separator)
                       org-blackfriday-table-right-border "\n")))))
         (tbl (concat (when no-header
                        (funcall build-dummy-header))
                      (replace-regexp-in-string "\n\n" "\n" contents))))
    ;; (message "[ox-bf-table DBG] Tbl:\n%s" tbl)
    tbl))

;;;; Verse Block
(defun org-blackfriday-verse-block (_verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to partial HTML.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  (let* ((ret contents)
         ;; Org removes all the leading whitespace only from the first
         ;; line.  So the trick is to use the ">" character before any
         ;; intended indentation on the first non-blank line.
         (ret (replace-regexp-in-string "\\`\\([[:blank:]\n\r]*\\)>" "\\1" ret))
         (br (org-html-close-tag "br" nil info))
         (re (format "\\(?:%s\\)?[ \t]*\n" (regexp-quote br)))
         ;; Replace each newline character with line break.  Also
         ;; remove any trailing "br" close-tag so as to avoid
         ;; duplicates.
         (ret (replace-regexp-in-string re (concat br "\n") ret))
         ;; Replace leading white spaces with non-breaking spaces.
         (ret (replace-regexp-in-string
               "^[[:blank:]]+"
               (lambda (m)
                 (org-html--make-string (length m) "&nbsp;"))
               ret)))
    ret))


;;; Interactive functions

;;;###autoload
(defun org-blackfriday-export-as-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Github Flavored Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org BLACKFRIDAY Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'blackfriday "*Org BLACKFRIDAY Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-blackfriday-convert-region-to-md ()
  "Convert text in the current region to Blackfriday Markdown.
The text is assumed to be in Org mode format.

This can be used in any buffer.  For example, you can write an
itemized list in Org mode syntax in a Markdown buffer and use
this command to convert it."
  (interactive)
  (org-export-replace-region-by 'blackfriday))

;;;###autoload
(defun org-blackfriday-export-to-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Github Flavored Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'blackfriday outfile async subtreep visible-only)))

;;;###autoload
(defun org-blackfriday-publish-to-blackfriday (plist filename pub-dir)
  "Publish an Org file to Blackfriday Markdown file.

PLIST is the property list for the given project.  FILENAME is
the filename of the Org file to be published.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'blackfriday filename ".md" plist pub-dir))


(provide 'ox-blackfriday)

;;; ox-blackfriday.el ends here
