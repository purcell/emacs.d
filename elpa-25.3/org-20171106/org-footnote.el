;;; org-footnote.el --- Footnote support in Org      -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2009-2017 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the code dealing with footnotes in Org mode.

;;; Code:

;;;; Declarations

(require 'cl-lib)
(require 'org-macs)
(require 'org-compat)

(declare-function org-at-comment-p "org" ())
(declare-function org-at-heading-p "org" (&optional ignored))
(declare-function org-back-over-empty-lines "org" ())
(declare-function org-edit-footnote-reference "org-src" ())
(declare-function org-element-at-point "org-element" ())
(declare-function org-element-class "org-element" (datum &optional parent))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-lineage "org-element" (blob &optional types with-self))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))
(declare-function org-end-of-subtree "org"  (&optional invisible-ok to-heading))
(declare-function org-fill-paragraph "org" (&optional justify))
(declare-function org-in-block-p "org" (names))
(declare-function org-in-regexp "org" (re &optional nlines visually))
(declare-function org-in-verbatim-emphasis "org" ())
(declare-function org-inside-LaTeX-fragment-p "org" ())
(declare-function org-inside-latex-macro-p "org" ())
(declare-function org-mark-ring-push "org" (&optional pos buffer))
(declare-function org-show-context "org" (&optional key))
(declare-function org-trim "org" (s &optional keep-lead))
(declare-function outline-next-heading "outline")

(defvar electric-indent-mode)
(defvar org-blank-before-new-entry)	; defined in org.el
(defvar org-bracket-link-regexp)	; defined in org.el
(defvar org-complex-heading-regexp)	; defined in org.el
(defvar org-odd-levels-only)		; defined in org.el
(defvar org-outline-regexp)		; defined in org.el
(defvar org-outline-regexp-bol)		; defined in org.el


;;;; Constants

(defconst org-footnote-re
  "\\[fn:\\(?:\\(?1:[-_[:word:]]+\\)?\\(:\\)\\|\\(?1:[-_[:word:]]+\\)\\]\\)"
  "Regular expression for matching footnotes.
Match group 1 contains footnote's label.  It is nil for anonymous
footnotes.  Match group 2 is non-nil only when footnote is
inline, i.e., it contains its own definition.")

(defconst org-footnote-definition-re "^\\[fn:\\([-_[:word:]]+\\)\\]"
  "Regular expression matching the definition of a footnote.
Match group 1 contains definition's label.")

(defconst org-footnote-forbidden-blocks '("comment" "example" "export" "src")
  "Names of blocks where footnotes are not allowed.")


;;;; Customization

(defgroup org-footnote nil
  "Footnotes in Org mode."
  :tag "Org Footnote"
  :group 'org)

(defcustom org-footnote-section "Footnotes"
  "Outline heading containing footnote definitions.

This can be nil, to place footnotes locally at the end of the
current outline node.  If can also be the name of a special
outline heading under which footnotes should be put.

This variable defines the place where Org puts the definition
automatically, i.e. when creating the footnote, and when sorting
the notes.  However, by hand you may place definitions
*anywhere*.

If this is a string, during export, all subtrees starting with
this heading will be ignored.

If you don't use the customize interface to change this variable,
you will need to run the following command after the change:

  `\\[universal-argument] \\[org-element-cache-reset]'"
  :group 'org-footnote
  :initialize 'custom-initialize-default
  :set (lambda (var val)
	 (set var val)
	 (when (fboundp 'org-element-cache-reset)
	   (org-element-cache-reset 'all)))
  :type '(choice
	  (string :tag "Collect footnotes under heading")
	  (const :tag "Define footnotes locally" nil)))

(defcustom org-footnote-define-inline nil
  "Non-nil means define footnotes inline, at reference location.
When nil, footnotes will be defined in a special section near
the end of the document.  When t, the [fn:label:definition] notation
will be used to define the footnote at the reference position."
  :group 'org-footnote
  :type 'boolean)

(defcustom org-footnote-auto-label t
  "Non-nil means define automatically new labels for footnotes.
Possible values are:

nil        Prompt the user for each label.
t          Create unique labels of the form [fn:1], [fn:2], etc.
confirm    Like t, but let the user edit the created value.
           The label can be removed from the minibuffer to create
           an anonymous footnote.
random	   Automatically generate a unique, random label."
  :group 'org-footnote
  :type '(choice
	  (const :tag "Prompt for label" nil)
	  (const :tag "Create automatic [fn:N]" t)
	  (const :tag "Offer automatic [fn:N] for editing" confirm)
	  (const :tag "Create a random label" random)))

(defcustom org-footnote-auto-adjust nil
  "Non-nil means automatically adjust footnotes after insert/delete.
When this is t, after each insertion or deletion of a footnote,
simple fn:N footnotes will be renumbered, and all footnotes will be sorted.
If you want to have just sorting or just renumbering, set this variable
to `sort' or `renumber'.

The main values of this variable can be set with in-buffer options:

#+STARTUP: fnadjust
#+STARTUP: nofnadjust"
  :group 'org-footnote
  :type '(choice
	  (const :tag "No adjustment" nil)
	  (const :tag "Renumber" renumber)
	  (const :tag "Sort" sort)
	  (const :tag "Renumber and Sort" t)))

(defcustom org-footnote-fill-after-inline-note-extraction nil
  "Non-nil means fill paragraphs after extracting footnotes.
When extracting inline footnotes, the lengths of lines can change a lot.
When this option is set, paragraphs from which an inline footnote has been
extracted will be filled again."
  :group 'org-footnote
  :type 'boolean)


;;;; Predicates

(defun org-footnote-in-valid-context-p ()
  "Is point in a context where footnotes are allowed?"
  (save-match-data
    (not (or (org-at-comment-p)
	     (org-inside-LaTeX-fragment-p)
	     ;; Avoid literal example.
	     (org-in-verbatim-emphasis)
	     (save-excursion
	       (beginning-of-line)
	       (looking-at "[ \t]*:[ \t]+"))
	     ;; Avoid forbidden blocks.
	     (org-in-block-p org-footnote-forbidden-blocks)))))

(defun org-footnote-at-reference-p ()
  "Is the cursor at a footnote reference?

If so, return a list containing its label, beginning and ending
positions, and the definition, when inlined."
  (when (and (org-footnote-in-valid-context-p)
	     (or (looking-at org-footnote-re)
		 (org-in-regexp org-footnote-re)
		 (save-excursion (re-search-backward org-footnote-re nil t)))
	     (/= (match-beginning 0) (line-beginning-position)))
    (let* ((beg (match-beginning 0))
	   (label (match-string-no-properties 1))
	   ;; Inline footnotes don't end at (match-end 0) as
	   ;; `org-footnote-re' stops just after the second colon.
	   ;; Find the real ending with `scan-sexps', so Org doesn't
	   ;; get fooled by unrelated closing square brackets.
	   (end (ignore-errors (scan-sexps beg 1))))
      ;; Point is really at a reference if it's located before true
      ;; ending of the footnote.
      (when (and end
		 (< (point) end)
		 ;; Verify match isn't a part of a link.
		 (not (save-excursion
			(goto-char beg)
			(let ((linkp
			       (save-match-data
				 (org-in-regexp org-bracket-link-regexp))))
			  (and linkp (< (point) (cdr linkp))))))
		 ;; Verify point doesn't belong to a LaTeX macro.
		 (not (org-inside-latex-macro-p)))
	(list label beg end
	      ;; Definition: ensure this is an inline footnote first.
	      (and (match-end 2)
		   (org-trim
		    (buffer-substring-no-properties
		     (match-end 0) (1- end)))))))))

(defun org-footnote-at-definition-p ()
  "Is point within a footnote definition?

This matches only pure definitions like [1] or [fn:name] at the
beginning of a line.  It does not match references like
\[fn:name:definition], where the footnote text is included and
defined locally.

The return value will be nil if not at a footnote definition, and
a list with label, start, end and definition of the footnote
otherwise."
  (when (save-excursion (beginning-of-line) (org-footnote-in-valid-context-p))
    (save-excursion
      (end-of-line)
      ;; Footnotes definitions are separated by new headlines, another
      ;; footnote definition or 2 blank lines.
      (let ((lim (save-excursion
		   (re-search-backward
		    (concat org-outline-regexp-bol
			    "\\|^\\([ \t]*\n\\)\\{2,\\}") nil t))))
	(when (re-search-backward org-footnote-definition-re lim t)
	  (let ((label (match-string-no-properties 1))
		(beg (match-beginning 0))
		(beg-def (match-end 0))
		(end (if (progn
			   (end-of-line)
			   (re-search-forward
			    (concat org-outline-regexp-bol "\\|"
				    org-footnote-definition-re "\\|"
				    "^\\([ \t]*\n\\)\\{2,\\}") nil 'move))
			 (match-beginning 0)
		       (point))))
	    (list label beg end
		  (org-trim (buffer-substring-no-properties beg-def end)))))))))


;;;; Internal functions

(defun org-footnote--allow-reference-p ()
  "Non-nil when a footnote reference can be inserted at point."
  ;; XXX: This is similar to `org-footnote-in-valid-context-p' but
  ;; more accurate and usually faster, except in some corner cases.
  ;; It may replace it after doing proper benchmarks as it would be
  ;; used in fontification.
  (unless (bolp)
    (let* ((context (org-element-context))
	   (type (org-element-type context)))
      (cond
       ;; No footnote reference in attributes.
       ((let ((post (org-element-property :post-affiliated context)))
	  (and post (< (point) post)))
	nil)
       ;; Paragraphs and blank lines at top of document are fine.
       ((memq type '(nil paragraph)))
       ;; So are contents of verse blocks.
       ((eq type 'verse-block)
	(and (>= (point) (org-element-property :contents-begin context))
	     (< (point) (org-element-property :contents-end context))))
       ;; In an headline or inlinetask, point must be either on the
       ;; heading itself or on the blank lines below.
       ((memq type '(headline inlinetask))
	(or (not (org-at-heading-p))
	    (and (save-excursion
		   (beginning-of-line)
		   (and (let ((case-fold-search t))
			  (not (looking-at-p "\\*+ END[ \t]*$")))
			(let ((case-fold-search nil))
			  (looking-at org-complex-heading-regexp))))
		 (match-beginning 4)
		 (>= (point) (match-beginning 4))
		 (or (not (match-beginning 5))
		     (< (point) (match-beginning 5))))))
       ;; White spaces after an object or blank lines after an element
       ;; are OK.
       ((>= (point)
	    (save-excursion (goto-char (org-element-property :end context))
			    (skip-chars-backward " \r\t\n")
			    (if (eq (org-element-class context) 'object) (point)
			      (1+ (line-beginning-position 2))))))
       ;; Other elements are invalid.
       ((eq (org-element-class context) 'element) nil)
       ;; Just before object is fine.
       ((= (point) (org-element-property :begin context)))
       ;; Within recursive object too, but not in a link.
       ((eq type 'link) nil)
       ((let ((cbeg (org-element-property :contents-begin context))
	      (cend (org-element-property :contents-end context)))
	  (and cbeg (>= (point) cbeg) (<= (point) cend))))))))

(defun org-footnote--clear-footnote-section ()
  "Remove all footnote sections in buffer and create a new one.
New section is created at the end of the buffer, before any file
local variable definition.  Leave point within the new section."
  (when org-footnote-section
    (goto-char (point-min))
    (let ((regexp
	   (format "^\\*+ +%s[ \t]*$"
		   (regexp-quote org-footnote-section))))
      (while (re-search-forward regexp nil t)
	(delete-region
	 (match-beginning 0)
	 (progn (org-end-of-subtree t t)
		(if (not (eobp)) (point)
		  (org-footnote--goto-local-insertion-point)
		  (skip-chars-forward " \t\n")
		  (if (eobp) (point) (line-beginning-position)))))))
    (goto-char (point-max))
    (org-footnote--goto-local-insertion-point)
    (when (and (cdr (assq 'heading org-blank-before-new-entry))
	       (zerop (save-excursion (org-back-over-empty-lines))))
      (insert "\n"))
    (insert "* " org-footnote-section "\n")))

(defun org-footnote--set-label (label)
  "Set label of footnote at point to string LABEL.
Assume point is at the beginning of the reference or definition
to rename."
  (forward-char 4)
  (cond ((eq (char-after) ?:) (insert label))
	((looking-at "\\([-_[:word:]]+\\)") (replace-match label nil nil nil 1))
	(t nil)))

(defun org-footnote--collect-references (&optional anonymous)
  "Collect all labeled footnote references in current buffer.

Return an alist where associations follow the pattern

  (LABEL MARKER TOP-LEVEL SIZE)

with

  LABEL     the label of the of the definition,
  MARKER    a marker pointing to its beginning,
  TOP-LEVEL a boolean, nil when the footnote is contained within
            another one,
  SIZE      the length of the inline definition, in characters,
            or nil for non-inline references.

When optional ANONYMOUS is non-nil, also collect anonymous
references.  In such cases, LABEL is nil.

References are sorted according to a deep-reading order."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((regexp (if anonymous org-footnote-re "\\[fn:[-_[:word:]]+[]:]"))
	 references nested)
     (save-excursion
       (while (re-search-forward regexp nil t)
	 ;; Ignore definitions.
	 (unless (and (eq (char-before) ?\])
		      (= (line-beginning-position) (match-beginning 0)))
	   ;; Ensure point is within the reference before parsing it.
	   (backward-char)
	   (let ((object (org-element-context)))
	     (when (eq (org-element-type object) 'footnote-reference)
	       (let* ((label (org-element-property :label object))
		      (begin (org-element-property :begin object))
		      (size
		       (and (eq (org-element-property :type object) 'inline)
			    (- (org-element-property :contents-end object)
			       (org-element-property :contents-begin object)))))
		 (let ((d (org-element-lineage object '(footnote-definition))))
		   (push (list label (copy-marker begin) (not d) size)
			 references)
		   (when d
		     ;; Nested references are stored in alist NESTED.
		     ;; Associations there follow the pattern
		     ;;
		     ;;   (DEFINITION-LABEL . REFERENCES)
		     (let* ((def-label (org-element-property :label d))
			    (labels (assoc def-label nested)))
		       (if labels (push label (cdr labels))
			 (push (list def-label label) nested)))))))))))
     ;; Sort the list of references.  Nested footnotes have priority
     ;; over top-level ones.
     (letrec ((ordered nil)
	      (add-reference
	       (lambda (ref allow-nested)
		 (when (or allow-nested (nth 2 ref))
		   (push ref ordered)
		   (dolist (r (mapcar (lambda (l) (assoc l references))
				      (reverse
				       (cdr (assoc (nth 0 ref) nested)))))
		     (funcall add-reference r t))))))
       (dolist (r (reverse references) (nreverse ordered))
	 (funcall add-reference r nil))))))

(defun org-footnote--collect-definitions (&optional delete)
  "Collect all footnote definitions in current buffer.

Return an alist where associations follow the pattern

  (LABEL . DEFINITION)

with LABEL and DEFINITION being, respectively, the label and the
definition of the footnote, as strings.

When optional argument DELETE is non-nil, delete the definition
while collecting them."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let (definitions seen)
     (while (re-search-forward org-footnote-definition-re nil t)
       (backward-char)
       (let ((element (org-element-at-point)))
	 (let ((label (org-element-property :label element)))
	   (when (and (eq (org-element-type element) 'footnote-definition)
		      (not (member label seen)))
	     (push label seen)
	     (let* ((beg (progn
			   (goto-char (org-element-property :begin element))
			   (skip-chars-backward " \r\t\n")
			   (if (bobp) (point) (line-beginning-position 2))))
		    (end (progn
			   (goto-char (org-element-property :end element))
			   (skip-chars-backward " \r\t\n")
			   (line-beginning-position 2)))
		    (def (org-trim (buffer-substring-no-properties beg end))))
	       (push (cons label def) definitions)
	       (when delete (delete-region beg end)))))))
     definitions)))

(defun org-footnote--goto-local-insertion-point ()
  "Find insertion point for footnote, just before next outline heading.
Assume insertion point is within currently accessible part of the buffer."
  (org-with-limited-levels (outline-next-heading))
  ;; Skip file local variables.  See `modify-file-local-variable'.
  (when (eobp)
    (let ((case-fold-search t))
      (re-search-backward "^[ \t]*# +Local Variables:"
			  (max (- (point-max) 3000) (point-min))
			  t)))
  (skip-chars-backward " \t\n")
  (forward-line)
  (unless (bolp) (insert "\n")))


;;;; Navigation

(defun org-footnote-get-next-reference (&optional label backward limit)
  "Return complete reference of the next footnote.

If LABEL is provided, get the next reference of that footnote.  If
BACKWARD is non-nil, find previous reference instead.  LIMIT is
the buffer position bounding the search.

Return value is a list like those provided by `org-footnote-at-reference-p'.
If no footnote is found, return nil."
  (save-excursion
    (let* ((label-fmt (if label (format "\\[fn:%s[]:]" label) org-footnote-re)))
      (catch 'exit
	(while t
	  (unless (funcall (if backward #'re-search-backward #'re-search-forward)
			   label-fmt limit t)
	    (throw 'exit nil))
	  (unless backward (backward-char))
	  (let ((ref (org-footnote-at-reference-p)))
	    (when ref (throw 'exit ref))))))))

(defun org-footnote-next-reference-or-definition (limit)
  "Move point to next footnote reference or definition.

LIMIT is the buffer position bounding the search.

Return value is a list like those provided by
`org-footnote-at-reference-p' or `org-footnote-at-definition-p'.
If no footnote is found, return nil."
  (let* (ref (origin (point)))
    (catch 'exit
      (while t
	(unless (re-search-forward org-footnote-re limit t)
	  (goto-char origin)
	  (throw 'exit nil))
	;; Beware: with non-inline footnotes point will be just after
	;; the closing square bracket.
	(backward-char)
	(cond
	 ((setq ref (org-footnote-at-reference-p))
	  (throw 'exit ref))
	 ;; Definition: also grab the last square bracket, matched in
	 ;; `org-footnote-re' for non-inline footnotes.
	 ((save-match-data (org-footnote-at-definition-p))
	  (let ((end (match-end 0)))
	    (throw 'exit
		   (list nil (match-beginning 0)
			 (if (eq (char-before end) ?\]) end (1+ end)))))))))))

(defun org-footnote-goto-definition (label &optional location)
  "Move point to the definition of the footnote LABEL.

LOCATION, when non-nil specifies the buffer position of the
definition.

Throw an error if there is no definition or if it cannot be
reached from current narrowed part of buffer.  Return a non-nil
value if point was successfully moved."
  (interactive "sLabel: ")
  (let* ((label (org-footnote-normalize-label label))
	 (def-start (or location (nth 1 (org-footnote-get-definition label)))))
    (cond
     ((not def-start)
      (user-error "Cannot find definition of footnote %s" label))
     ((or (> def-start (point-max)) (< def-start (point-min)))
      (user-error "Definition is outside narrowed part of buffer")))
    (org-mark-ring-push)
    (goto-char def-start)
    (looking-at (format "\\[fn:%s[]:] ?" (regexp-quote label)))
    (goto-char (match-end 0))
    (org-show-context 'link-search)
    (when (derived-mode-p 'org-mode)
      (message "%s" (substitute-command-keys
		     "Edit definition and go back with \
`\\[org-mark-ring-goto]' or, if unique, with `\\[org-ctrl-c-ctrl-c]'.")))
    t))

(defun org-footnote-goto-previous-reference (label)
  "Find the first closest (to point) reference of footnote with label LABEL."
  (interactive "sLabel: ")
  (org-mark-ring-push)
  (let ((label (org-footnote-normalize-label label))
	ref)
    (save-excursion
      (setq ref (or (org-footnote-get-next-reference label t)
		    (org-footnote-get-next-reference label)
		    (save-restriction
		      (widen)
		      (or
		       (org-footnote-get-next-reference label t)
		       (org-footnote-get-next-reference label))))))
    (if (not ref)
	(error "Cannot find reference of footnote %s" label)
      (goto-char (nth 1 ref))
      (org-show-context 'link-search))))


;;;; Getters

(defun org-footnote-normalize-label (label)
  "Return LABEL without \"fn:\" prefix.
If LABEL is the empty string or constituted of white spaces only,
return nil instead."
  (pcase (org-trim label)
    ("" nil)
    ((pred (string-prefix-p "fn:")) (substring label 3))
    (_ label)))

(defun org-footnote-get-definition (label)
  "Return label, boundaries and definition of the footnote LABEL."
  (let* ((label (regexp-quote (org-footnote-normalize-label label)))
	 (re (format "^\\[fn:%s\\]\\|.\\[fn:%s:" label label)))
    (org-with-wide-buffer
     (goto-char (point-min))
     (catch 'found
       (while (re-search-forward re nil t)
	 (let* ((datum (progn (backward-char) (org-element-context)))
		(type (org-element-type datum)))
	   (when (memq type '(footnote-definition footnote-reference))
	     (throw 'found
		    (list
		     label
		     (org-element-property :begin datum)
		     (org-element-property :end datum)
		     (let ((cbeg (org-element-property :contents-begin datum)))
		       (if (not cbeg) ""
			 (replace-regexp-in-string
			  "[ \t\n]*\\'"
			  ""
			  (buffer-substring-no-properties
			   cbeg
			   (org-element-property :contents-end datum))))))))))
       nil))))

(defun org-footnote-all-labels ()
  "List all defined footnote labels used throughout the buffer.
This function ignores narrowing, if any."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let (all)
     (while (re-search-forward org-footnote-re nil t)
       (backward-char)
       (let ((context (org-element-context)))
	 (when (memq (org-element-type context)
		     '(footnote-definition footnote-reference))
	   (let ((label (org-element-property :label context)))
	     (when label (cl-pushnew label all :test #'equal))))))
     all)))

(defun org-footnote-unique-label (&optional current)
  "Return a new unique footnote label.

The function returns the first numeric label currently unused.

Optional argument CURRENT is the list of labels active in the
buffer."
  (let ((current (or current (org-footnote-all-labels))))
    (let ((count 1))
      (while (member (number-to-string count) current)
	(cl-incf count))
      (number-to-string count))))


;;;; Adding, Deleting Footnotes

(defun org-footnote-new ()
  "Insert a new footnote.
This command prompts for a label.  If this is a label referencing an
existing label, only insert the label.  If the footnote label is empty
or new, let the user edit the definition of the footnote."
  (interactive)
  (unless (org-footnote--allow-reference-p)
    (user-error "Cannot insert a footnote here"))
  (let* ((all (org-footnote-all-labels))
	 (label
	  (if (eq org-footnote-auto-label 'random)
	      (format "%x" (random most-positive-fixnum))
	    (org-footnote-normalize-label
	     (let ((propose (org-footnote-unique-label all)))
	       (if (eq org-footnote-auto-label t) propose
		 (completing-read
		  "Label (leave empty for anonymous): "
		  (mapcar #'list all) nil nil
		  (and (eq org-footnote-auto-label 'confirm) propose))))))))
    (cond ((not label)
	   (insert "[fn::]")
	   (backward-char 1))
	  ((member label all)
	   (insert "[fn:" label "]")
	   (message "New reference to existing note"))
	  (org-footnote-define-inline
	   (insert "[fn:" label ":]")
	   (backward-char 1)
	   (org-footnote-auto-adjust-maybe))
	  (t
	   (insert "[fn:" label "]")
	   (let ((p (org-footnote-create-definition label)))
	     ;; `org-footnote-goto-definition' needs to be called
	     ;; after `org-footnote-auto-adjust-maybe'.  Otherwise
	     ;; both label and location of the definition are lost.
	     ;; On the contrary, it needs to be called before
	     ;; `org-edit-footnote-reference' so that the remote
	     ;; editing buffer can display the correct label.
	     (if (ignore-errors (org-footnote-goto-definition label p))
		 (org-footnote-auto-adjust-maybe)
	       ;; Definition was created outside current scope: edit
	       ;; it remotely.
	       (org-footnote-auto-adjust-maybe)
	       (org-edit-footnote-reference)))))))

(defun org-footnote-create-definition (label)
  "Start the definition of a footnote with label LABEL.
Return buffer position at the beginning of the definition.  This
function doesn't move point."
  (let ((label (org-footnote-normalize-label label))
	electric-indent-mode)		; Prevent wrong indentation.
    (org-with-wide-buffer
     (cond
      ((not org-footnote-section) (org-footnote--goto-local-insertion-point))
      ((save-excursion
	 (goto-char (point-min))
	 (re-search-forward
	  (concat "^\\*+[ \t]+" (regexp-quote org-footnote-section) "[ \t]*$")
	  nil t))
       (goto-char (match-end 0))
       (forward-line)
       (unless (bolp) (insert "\n")))
      (t (org-footnote--clear-footnote-section)))
     (when (zerop (org-back-over-empty-lines)) (insert "\n"))
     (insert "[fn:" label "] \n")
     (line-beginning-position 0))))

(defun org-footnote-delete-references (label)
  "Delete every reference to footnote LABEL.
Return the number of footnotes removed."
  (save-excursion
    (goto-char (point-min))
    (let (ref (nref 0))
      (while (setq ref (org-footnote-get-next-reference label))
	(goto-char (nth 1 ref))
	(delete-region (nth 1 ref) (nth 2 ref))
	(cl-incf nref))
      nref)))

(defun org-footnote-delete-definitions (label)
  "Delete every definition of the footnote LABEL.
Return the number of footnotes removed."
  (save-excursion
    (goto-char (point-min))
    (let ((def-re (format "^\\[fn:%s\\]" (regexp-quote label)))
	  (ndef 0))
      (while (re-search-forward def-re nil t)
	(pcase (org-footnote-at-definition-p)
	  (`(,_ ,start ,end ,_)
	   ;; Remove the footnote, and all blank lines before it.
	   (delete-region (progn
			    (goto-char start)
			    (skip-chars-backward " \r\t\n")
			    (if (bobp) (point) (line-beginning-position 2)))
			  (progn
			    (goto-char end)
			    (skip-chars-backward " \r\t\n")
			    (if (bobp) (point) (line-beginning-position 2))))
	   (cl-incf ndef))))
      ndef)))

(defun org-footnote-delete (&optional label)
  "Delete the footnote at point.
This will remove the definition (even multiple definitions if they exist)
and all references of a footnote label.

If LABEL is non-nil, delete that footnote instead."
  (catch 'done
    (let* ((nref 0) (ndef 0) x
	   ;; 1. Determine LABEL of footnote at point.
	   (label (cond
		   ;; LABEL is provided as argument.
		   (label)
		   ;; Footnote reference at point.  If the footnote is
		   ;; anonymous, delete it and exit instead.
		   ((setq x (org-footnote-at-reference-p))
		    (or (car x)
			(progn
			  (delete-region (nth 1 x) (nth 2 x))
			  (message "Anonymous footnote removed")
			  (throw 'done t))))
		   ;; Footnote definition at point.
		   ((setq x (org-footnote-at-definition-p))
		    (car x))
		   (t (error "Don't know which footnote to remove")))))
      ;; 2. Now that LABEL is non-nil, find every reference and every
      ;; definition, and delete them.
      (setq nref (org-footnote-delete-references label)
	    ndef (org-footnote-delete-definitions label))
      ;; 3. Verify consistency of footnotes and notify user.
      (org-footnote-auto-adjust-maybe)
      (message "%d definition(s) of and %d reference(s) of footnote %s removed"
	       ndef nref label))))


;;;; Sorting, Renumbering, Normalizing

(defun org-footnote-renumber-fn:N ()
  "Order numbered footnotes into a sequence in the document."
  (interactive)
  (let ((references (org-footnote--collect-references)))
    (unwind-protect
	(let* ((c 0)
	       (references (cl-remove-if-not
			    (lambda (r) (string-match-p "\\`[0-9]+\\'" (car r)))
			    references))
	       (alist (mapcar (lambda (l) (cons l (number-to-string (cl-incf c))))
			      (delete-dups (mapcar #'car references)))))
	  (org-with-wide-buffer
	   ;; Re-number references.
	   (dolist (ref references)
	     (goto-char (nth 1 ref))
	     (org-footnote--set-label (cdr (assoc (nth 0 ref) alist))))
	   ;; Re-number definitions.
	   (goto-char (point-min))
	   (while (re-search-forward "^\\[fn:\\([0-9]+\\)\\]" nil t)
	     (replace-match (or (cdr (assoc (match-string 1) alist))
				;; Un-referenced definitions get
				;; higher numbers.
				(number-to-string (cl-incf c)))
			    nil nil nil 1))))
      (dolist (r references) (set-marker (nth 1 r) nil)))))

(defun org-footnote-sort ()
  "Rearrange footnote definitions in the current buffer.
Sort footnote definitions so they match order of footnote
references.  Also relocate definitions at the end of their
relative section or within a single footnote section, according
to `org-footnote-section'.  Inline definitions are ignored."
  (let ((references (org-footnote--collect-references)))
    (unwind-protect
	(let ((definitions (org-footnote--collect-definitions 'delete)))
	  (org-with-wide-buffer
	   (org-footnote--clear-footnote-section)
	   ;; Insert footnote definitions at the appropriate location,
	   ;; separated by a blank line.  Each definition is inserted
	   ;; only once throughout the buffer.
	   (let (inserted)
	     (dolist (cell references)
	       (let ((label (car cell))
		     (nested (not (nth 2 cell)))
		     (inline (nth 3 cell)))
		 (unless (or (member label inserted) inline)
		   (push label inserted)
		   (unless (or org-footnote-section nested)
		     ;; If `org-footnote-section' is non-nil, or
		     ;; reference is nested, point is already at the
		     ;; correct position.  Otherwise, move at the
		     ;; appropriate location within the section
		     ;; containing the reference.
		     (goto-char (nth 1 cell))
		     (org-footnote--goto-local-insertion-point))
		   (insert "\n"
			   (or (cdr (assoc label definitions))
			       (format "[fn:%s] DEFINITION NOT FOUND." label))
			   "\n"))))
	     ;; Insert un-referenced footnote definitions at the end.
	     (let ((unreferenced
		    (cl-remove-if (lambda (d) (member (car d) inserted))
				  definitions)))
	       (dolist (d unreferenced) (insert "\n" (cdr d) "\n"))))))
      ;; Clear dangling markers in the buffer.
      (dolist (r references) (set-marker (nth 1 r) nil)))))

(defun org-footnote-normalize ()
  "Turn every footnote in buffer into a numbered one."
  (interactive)
  (let ((references (org-footnote--collect-references 'anonymous)))
    (unwind-protect
	(let ((n 0)
	      (translations nil)
	      (definitions nil))
	  (org-with-wide-buffer
	   ;; Update label for reference.  We need to do this before
	   ;; clearing definitions in order to rename nested footnotes
	   ;; before they are deleted.
	   (dolist (cell references)
	     (let* ((label (car cell))
		    (anonymous (not label))
		    (new
		     (cond
		      ;; In order to differentiate anonymous
		      ;; references from regular ones, set their
		      ;; labels to integers, not strings.
		      (anonymous (setcar cell (cl-incf n)))
		      ((cdr (assoc label translations)))
		      (t (let ((l (number-to-string (cl-incf n))))
			   (push (cons label l) translations)
			   l)))))
	       (goto-char (nth 1 cell))	; Move to reference's start.
	       (org-footnote--set-label
		(if anonymous (number-to-string new) new))
	       (let ((size (nth 3 cell)))
		 ;; Transform inline footnotes into regular references
		 ;; and retain their definition for later insertion as
		 ;; a regular footnote definition.
		 (when size
		   (let ((def (concat
			       (format "[fn:%s] " new)
			       (org-trim
				(substring
				 (delete-and-extract-region
				  (point) (+ (point) size 1))
				 1)))))
		     (push (cons (if anonymous new label) def) definitions)
		     (when org-footnote-fill-after-inline-note-extraction
		       (org-fill-paragraph)))))))
	   ;; Collect definitions.  Update labels according to ALIST.
	   (let ((definitions
		   (nconc definitions
			  (org-footnote--collect-definitions 'delete)))
		 (inserted))
	     (org-footnote--clear-footnote-section)
	     (dolist (cell references)
	       (let* ((label (car cell))
		      (anonymous (integerp label))
		      (pos (nth 1 cell)))
		 ;; Move to appropriate location, if required.  When
		 ;; there is a footnote section or reference is
		 ;; nested, point is already at the expected location.
		 (unless (or org-footnote-section (not (nth 2 cell)))
		   (goto-char pos)
		   (org-footnote--goto-local-insertion-point))
		 ;; Insert new definition once label is updated.
		 (unless (member label inserted)
		   (push label inserted)
		   (let ((stored (cdr (assoc label definitions)))
			 ;; Anonymous footnotes' label is already
			 ;; up-to-date.
			 (new (if anonymous label
				(cdr (assoc label translations)))))
		     (insert "\n"
			     (cond
			      ((not stored)
			       (format "[fn:%s] DEFINITION NOT FOUND." new))
			      (anonymous stored)
			      (t
			       (replace-regexp-in-string
				"\\`\\[fn:\\(.*?\\)\\]" new stored nil nil 1)))
			     "\n")))))
	     ;; Insert un-referenced footnote definitions at the end.
	     (let ((unreferenced
		    (cl-remove-if (lambda (d) (member (car d) inserted))
				  definitions)))
	       (dolist (d unreferenced)
		 (insert "\n"
			 (replace-regexp-in-string
			  org-footnote-definition-re
			  (format "[fn:%d]" (cl-incf n))
			  (cdr d))
			 "\n"))))))
      ;; Clear dangling markers.
      (dolist (r references) (set-marker (nth 1 r) nil)))))

(defun org-footnote-auto-adjust-maybe ()
  "Renumber and/or sort footnotes according to user settings."
  (when (memq org-footnote-auto-adjust '(t renumber))
    (org-footnote-renumber-fn:N))
  (when (memq org-footnote-auto-adjust '(t sort))
    (let ((label (car (org-footnote-at-definition-p))))
      (org-footnote-sort)
      (when label
	(goto-char (point-min))
	(and (re-search-forward (format "^\\[fn:%s\\]" (regexp-quote label))
				nil t)
	     (progn (insert " ")
		    (just-one-space)))))))


;;;; End-user interface

;;;###autoload
(defun org-footnote-action (&optional special)
  "Do the right thing for footnotes.

When at a footnote reference, jump to the definition.

When at a definition, jump to the references if they exist, offer
to create them otherwise.

When neither at definition or reference, create a new footnote,
interactively if possible.

With prefix arg SPECIAL, or when no footnote can be created,
offer additional commands in a menu."
  (interactive "P")
  (let* ((context (and (not special) (org-element-context)))
	 (type (org-element-type context)))
    (cond
     ;; On white space after element, insert a new footnote.
     ((and context
	   (> (point)
	      (save-excursion
		(goto-char (org-element-property :end context))
		(skip-chars-backward " \t")
		(point))))
      (org-footnote-new))
     ((eq type 'footnote-reference)
      (let ((label (org-element-property :label context)))
	(cond
	 ;; Anonymous footnote: move point at the beginning of its
	 ;; definition.
	 ((not label)
	  (goto-char (org-element-property :contents-begin context)))
	 ;; Check if a definition exists: then move to it.
	 ((let ((p (nth 1 (org-footnote-get-definition label))))
	    (when p (org-footnote-goto-definition label p))))
	 ;; No definition exists: offer to create it.
	 ((yes-or-no-p (format "No definition for %s.  Create one? " label))
	  (let ((p (org-footnote-create-definition label)))
	    (or (ignore-errors (org-footnote-goto-definition label p))
		;; Since definition was created outside current scope,
		;; edit it remotely.
		(org-edit-footnote-reference)))))))
     ((eq type 'footnote-definition)
      (org-footnote-goto-previous-reference
       (org-element-property :label context)))
     ((or special (not (org-footnote--allow-reference-p)))
      (message "Footnotes: [s]ort | [r]enumber fn:N | [S]=r+s | [n]ormalize | \
\[d]elete")
      (pcase (read-char-exclusive)
	(?s (org-footnote-sort))
	(?r (org-footnote-renumber-fn:N))
	(?S (org-footnote-renumber-fn:N)
	    (org-footnote-sort))
	(?n (org-footnote-normalize))
	(?d (org-footnote-delete))
	(char (error "No such footnote command %c" char))))
     (t (org-footnote-new)))))


(provide 'org-footnote)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-footnote.el ends here
