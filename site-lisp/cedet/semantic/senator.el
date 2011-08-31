;;; senator.el --- SEmantic NAvigaTOR

;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 10 Nov 2000
;; Keywords: syntax
;; X-RCS: $Id: senator.el,v 1.138 2009/02/07 02:28:19 zappo Exp $

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library defines commands and a minor mode to navigate between
;; semantic language tags in the current buffer.
;;
;; The commands `senator-next-tag' and `senator-previous-tag'
;; navigate respectively to the tag after or before the point.  The
;; command `senator-jump' directly jumps to a particular semantic
;; symbol.
;;
;; Also, for each built-in search command `search-forward',
;; `search-backward', `re-search-forward', `re-search-backward',
;; `word-search-forward' and `word-search-backward', there is an
;; equivalent `senator-<search-command>' defined which searches only
;; in semantic tag names.
;;
;; The command `senator-isearch-toggle-semantic-mode' toggles semantic
;; search in isearch mode.  When semantic search is enabled, isearch
;; is restricted to tag names.
;;
;; Finally, the library provides a `senator-minor-mode' to easily
;; enable or disable the SEmantic NAvigaTOR stuff for the current
;; buffer.
;;
;; The best way to use navigation commands is to bind them to keyboard
;; shortcuts.  Senator minor mode uses the common prefix key "C-c ,".
;; The following default key bindings are provided when semantic minor
;; mode is enabled:
;;
;;    key             binding
;;    ---             -------
;;    C-c , n         `senator-next-tag'
;;    C-c , p         `senator-previous-tag'
;;    C-c , j         `senator-jump'
;;    C-c , i         `senator-isearch-toggle-semantic-mode'
;;    C-c , TAB       `senator-complete-symbol'
;;    C-c , SPC       `senator-completion-menu-popup'
;;    S-mouse-3       `senator-completion-menu-popup'
;;    C-c , C-y       `senator-yank-tag'
;;    C-c , C-w       `senator-kill-tag'
;;    C-c , M-w       `senator-copy-tag'
;;    C-c , r         `senator-copy-tag-to-register'
;;    C-c , t         `senator-transpose-tags-up'
;;
;; You can customize the `senator-step-at-tag-classes' to navigate (and
;; search) only between tags of a particular class.  (Such as
;; functions and variables.)
;;
;; Customize `senator-step-at-start-end-tag-classes' to stop at the
;; start and end of the specified tag classes.
;;
;; To have a mode specific customization, do something like this in a
;; hook:
;;
;; (add-hook 'mode-hook
;;           (lambda ()
;;             (setq senator-step-at-tag-classes '(function variable))
;;             (setq senator-step-at-start-end-tag-classes '(function))
;;             ))
;;
;; The above example specifies to navigate (and search) only between
;; functions and variables, and to step at start and end of functions
;; only.

;;; History:
;;

;;; Code:
(require 'semantic)
(require 'semantic-ctxt)
(require 'semantic-imenu)
(eval-when-compile
  (require 'semanticdb)
  (require 'semanticdb-find)
  (require 'cl)
  )

;;; Customization
(defgroup senator nil
  "SEmantic NAvigaTOR."
  :group 'semantic)

;;;###autoload
(defcustom global-senator-minor-mode nil
  "*If non-nil enable global use of senator minor mode."
  :group 'senator
  :type 'boolean
  :require 'senator
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-senator-minor-mode (if val 1 -1))))

(defcustom senator-minor-mode-hook nil
  "Hook run at the end of function `senator-minor-mode'."
  :group 'senator
  :type 'hook)

;;;###autoload
(defcustom senator-step-at-tag-classes nil
  "*List of tag classes where to step.
A tag class is a symbol like 'variable, 'function, 'type, or other.
If nil navigation steps at any tag found.  This is a buffer local
variable.  It can be set in a mode hook to get a specific langage
navigation."
  :group 'senator
  :type '(repeat (symbol)))
(make-variable-buffer-local 'senator-step-at-tag-classes)
(semantic-varalias-obsolete 'semantic-step-at-token-ids
                            'semantic-step-at-tag-classes)

;;;###autoload
(defcustom senator-step-at-start-end-tag-classes '(function)
  "*List of tag classes where to step at start and end.
A tag class is a symbol like 'variable, 'function, 'type, or other.
If nil, navigation only step at beginning of tags.  If t, step at
start and end of any tag where it is allowed to step.  Also, stepping
at start and end of a tag prevent stepping inside its components.
This is a buffer local variable.  It can be set in a mode hook to get
a specific langage navigation."
  :group 'senator
  :type '(choice :tag "Identifiers"
                 (repeat :menu-tag "Symbols" (symbol))
                 (const  :tag "All" t)))
(make-variable-buffer-local 'senator-step-at-start-end-tag-classes)
(semantic-varalias-obsolete 'senator-step-at-start-end-token-ids
                            'senator-step-at-start-end-tag-classes)

(defcustom senator-highlight-found t
  "*If non-nil highlight tags found.
This option requires semantic 1.3 and above.  This is a buffer
local variable.  It can be set in a mode hook to get a specific
langage behaviour."
  :group 'senator
  :type 'boolean)
(make-variable-buffer-local 'senator-highlight-found)

;;; Faces
(defface senator-momentary-highlight-face
  '((((class color) (background dark))
     (:background "gray30"))
    (((class color) (background light))
     (:background "gray70")))
  "Face used to momentarily highlight tags."
  :group 'semantic-faces)

(defface senator-intangible-face
  '((((class color) (background light))
     (:foreground "gray25"))
    (((class color) (background dark))
     (:foreground "gray75")))
  "Face placed on intangible text."
  :group 'semantic-faces)

(defface senator-read-only-face
  '((((class color) (background dark))
     (:background "#664444"))
    (((class color) (background light))
     (:background "#CCBBBB")))
  "Face placed on read-only text."
  :group 'semantic-faces)

;;;;
;;;; Common functions
;;;;

(defsubst senator-parse ()
  "Parse the current buffer and return the tags where to navigate."
  (semantic-fetch-tags))

(defun senator-force-refresh ()
  "Force a full refresh of the current buffer's tags.
Throws away all the old tags, and recreates the tag database for
this buffer."
  (interactive)
  (semantic-clear-toplevel-cache)
  (senator-parse))

(defsubst senator-current-tag ()
  "Return the current tag in the current buffer.
Raise an error is there is no tag here."
  (or (semantic-current-tag)
      (error "No semantic tag here")))
(semantic-alias-obsolete 'senator-current-token 'senator-current-tag)

(defun senator-momentary-highlight-tag (tag)
  "Momentary highlight TAG.
Does nothing if `senator-highlight-found' is nil."
  (and senator-highlight-found
       (semantic-momentary-highlight-tag
        tag 'senator-momentary-highlight-face)))

(defun senator-step-at-start-end-p (tag)
  "Return non-nil if must step at start and end of TAG."
  (and tag
       (or (eq senator-step-at-start-end-tag-classes t)
           (memq (semantic-tag-class tag)
                 senator-step-at-start-end-tag-classes))))

(defun senator-skip-p (tag)
  "Return non-nil if must skip TAG."
  (and tag
       senator-step-at-tag-classes
       (not (memq (semantic-tag-class tag)
                  senator-step-at-tag-classes))))

(defun senator-middle-of-tag-p (pos tag)
  "Return non-nil if POS is between start and end of TAG."
  (and (> pos (semantic-tag-start tag))
       (< pos (semantic-tag-end   tag))))

(defun senator-step-at-parent (tag)
  "Return TAG's outermost parent if must step at start/end of it.
Return nil otherwise."
  (if tag
      (let (parent parents)
        (setq parents (semantic-find-tag-by-overlay
                       (semantic-tag-start tag)))
        (while (and parents (not parent))
          (setq parent  (car parents)
                parents (cdr parents))
          (if (or (eq tag parent)
                  (senator-skip-p parent)
                  (not (senator-step-at-start-end-p parent)))
              (setq parent nil)))
        parent)))

(defun senator-previous-tag-or-parent (pos)
  "Return the tag before POS or one of its parent where to step."
  (let (ol tag)
    (while (and pos (> pos (point-min)) (not tag))
      (setq pos (semantic-overlay-previous-change pos))
      (when pos
        ;; Get overlays at position
        (setq ol (semantic-overlays-at pos))
        ;; find the overlay that belongs to semantic
        ;; and STARTS or ENDS at the found position.
        (while (and ol (not tag))
          (setq tag (semantic-overlay-get (car ol) 'semantic))
          (unless (and tag (semantic-tag-p tag)
                       (or (= (semantic-tag-start tag) pos)
                           (= (semantic-tag-end   tag) pos)))
            (setq tag nil
                  ol (cdr ol))))))
    (or (senator-step-at-parent tag) tag)))

(defun senator-full-tag-name (tag parent)
  "Compose a full name from TAG name and PARENT names.
That is append to TAG name PARENT names each one separated by
`semantic-type-relation-separator-character'.  The PARENT list is in
reverse order."
  (let ((sep  (car semantic-type-relation-separator-character))
        (name ""))
    (while parent
      (setq name (concat name sep
                         (semantic-tag-name (car parent)))
            parent (cdr parent)))
    (concat (semantic-tag-name tag) name)))
(semantic-alias-obsolete 'senator-full-token-name
                         'senator-full-tag-name)

(defvar senator-completion-cache nil
  "The latest full completion list is cached here.")
(make-variable-buffer-local 'senator-completion-cache)

(defun senator-completion-cache-flush-fcn (&optional ignore)
  "Hook run to clear the completion list cache.
It is called each time the semantic cache is changed.
IGNORE arguments."
  (setq senator-completion-cache nil))

(defun senator-completion-flatten-stream (stream parents &optional top-level)
  "Return a flat list of all tags available in STREAM.
PARENTS is the list of parent tags.  Each element of the list is a
pair (TAG . PARENTS) where PARENTS is the list of TAG parent
tags or nil.  If TOP-LEVEL is non-nil the completion list will
contain only tags at top level.  Otherwise all component tags are
included too."
  (let (fs e tag components)
    (while stream
      (setq tag  (car stream)
            stream (cdr stream)
            e      (cons tag parents)
            fs     (cons e fs))
      (and (not top-level)
           ;; Not include function arguments
           (not (semantic-tag-of-class-p tag 'function))
           (setq components (semantic-tag-components tag))
           (setq fs (append fs (senator-completion-flatten-stream
                                components e)))))
    fs))

(defun senator-completion-function-args (tag)
  "Return a string of argument names from function TAG."
  (mapconcat #'(lambda (arg)
                 (if (semantic-tag-p arg)
                     (semantic-tag-name arg)
                   (format "%s" arg)))
             (semantic-tag-function-arguments tag)
             semantic-function-argument-separation-character))

(defun senator-completion-refine-name (elt)
  "Refine the name part of ELT.
ELT has the form (NAME . (TAG . PARENTS)).  The NAME refinement is
done in the following incremental way:

- If TAG is a function, append the list of argument names to NAME.

- If TAG is a type, append \"{}\" to NAME.

- If TAG is an include, append \"#\" to NAME.

- If TAG is a package, append \"=\" to NAME.

- If TAG has PARENTS append to NAME, the first separator in
  `semantic-type-relation-separator-character', followed by the next
  parent name.

- Otherwise NAME is set to \"tag-name@tag-start-position\"."
  (let* ((sep     (car semantic-type-relation-separator-character))
         (name    (car elt))
         (tag     (car (cdr elt)))
         (parents (cdr (cdr elt)))
         (oname   (semantic-tag-name tag))
         (class   (semantic-tag-class tag)))
    (cond
     ((and (eq class 'function) (string-equal name oname))
      (setq name (format "%s(%s)" name
                         (senator-completion-function-args tag))))
     ((and (eq class 'type) (string-equal name oname))
      (setq name (format "%s{}" name)))
     ((and (eq class 'include) (string-equal name oname))
      (setq name (format "%s#" name)))
     ((and (eq class 'package) (string-equal name oname))
      (setq name (format "%s=" name)))
     (parents
      (setq name (format "%s%s%s" name
                         (if (semantic-tag-of-class-p
                              (car parents) 'function)
                             ")" sep)
                         (semantic-tag-name (car parents)))
            parents (cdr parents)))
     (t
      (setq name (format "%s@%d" oname
                         (semantic-tag-start tag)))))
    (setcar elt name)
    (setcdr elt (cons tag parents))))

(defun senator-completion-uniquify-names (completion-stream)
  "Uniquify names in COMPLETION-STREAM.
That is refine the name part of each COMPLETION-STREAM element until
there is no duplicated names.  Each element of COMPLETION-STREAM has
the form (NAME . (TAG . PARENTS)).  See also the function
`senator-completion-refine-name'."
  (let ((completion-stream (sort completion-stream
                                 #'(lambda (e1 e2)
                                     (string-lessp (car e1)
                                                   (car e2)))))
        (dupp t)
        clst elt dup name)
    (while dupp
      (setq dupp nil
            clst completion-stream)
      (while clst
        (setq elt  (car clst)
              name (car elt)
              clst (cdr clst)
              dup  (and clst
                        (string-equal name (car (car clst)))
                        elt)
              dupp (or dupp dup))
        (while dup
          (senator-completion-refine-name dup)
          (setq elt (car clst)
                dup (and elt (string-equal name (car elt)) elt))
          (and dup (setq clst (cdr clst))))))
    ;; Return a usable completion alist where each element has the
    ;; form (NAME . TAG).
    (setq clst completion-stream)
    (while clst
      (setq elt  (car clst)
            clst (cdr clst))
      (setcdr elt (car (cdr elt))))
    completion-stream))

(defun senator-completion-stream (stream &optional top-level)
  "Return a useful completion list from tags in STREAM.
That is an alist of all (COMPLETION-NAME . TAG) available.
COMPLETION-NAME is an unique tag name (see also the function
`senator-completion-uniquify-names').  If TOP-LEVEL is non-nil the
completion list will contain only tags at top level.  Otherwise all
sub tags are included too."
  (let* ((fs (senator-completion-flatten-stream stream nil top-level))
         cs elt tag)
    ;; Transform each FS element from (TAG . PARENTS)
    ;; to (NAME . (TAG . PARENT)).
    (while fs
      (setq elt (car fs)
            tag (car elt)
            fs  (cdr fs)
            cs  (cons (cons (semantic-tag-name tag) elt) cs)))
    ;; Return a completion list with unique COMPLETION-NAMEs.
    (senator-completion-uniquify-names cs)))

(defun senator-current-type-context ()
  "Return tags in the type context at point or nil if not found."
  (let ((context (semantic-find-tags-by-class
                  'type (semantic-find-tag-by-overlay))))
    (if context
        (semantic-tag-type-members
         (nth (1- (length context)) context)))))

(defun senator-completion-list (&optional in-context)
  "Return a useful completion list from tags in current buffer.
If IN-CONTEXT is non-nil return only the top level tags in the type
context at point or the top level tags in the current buffer if no
type context exists at point."
  (let (stream)
    (if in-context
        (setq stream (senator-current-type-context)))
    (or stream (setq stream (senator-parse)))
    ;; IN-CONTEXT completion doesn't use nor set the cache.
    (or (and (not in-context) senator-completion-cache)
        (let ((clst (senator-completion-stream stream in-context)))
          (or in-context
              (setq senator-completion-cache clst))
          clst))))

(defun senator-find-tag-for-completion (prefix)
  "Find all tags with a name starting with PREFIX.
Uses `semanticdb' when available."
  (let ((tagsa nil)
	(tagsb nil))
    (when (and (featurep 'semantic-analyze))
      (let ((ctxt (semantic-analyze-current-context)))
	(when ctxt
	  (condition-case nil
	      (setq tagsa (semantic-analyze-possible-completions
			   ctxt))
	    (error nil)))))

    (if tagsa
	tagsa
      ;; If the analyzer fails, then go into boring completion
      (setq tagsb
	    (if (and (featurep 'semanticdb) (semanticdb-minor-mode-p))
		;; semanticdb version returns a list of (DB-TABLE . TAG-LIST)
		(semanticdb-deep-find-tags-for-completion prefix)
	      ;; semantic version returns a TAG-LIST
	      (semantic-deep-find-tags-for-completion prefix (current-buffer))))
      (semanticdb-fast-strip-find-results tagsb))))

;;; Senator stream searching functions: no more supported.
;;
(defun senator-find-nonterminal-by-name (&rest ignore)
  (error "Use the semantic and semanticdb find API instead"))

(defun senator-find-nonterminal-by-name-regexp (&rest ignore)
  (error "Use the semantic and semanticdb find API instead"))

;;;;
;;;; Search functions
;;;;

(defun senator-search-tag-name (tag)
  "Search for TAG name in current buffer.
Limit the search to TAG bounds.
If found, set point to the end of the name, and return point.  The
beginning of the name is at (match-beginning 0).
Return nil if not found, that is if TAG name doesn't come from the
source."
  (let ((name (semantic-tag-name tag)))
    (setq name (if (string-match "\\`\\([^[]+\\)[[]" name)
                   (match-string 1 name)
                 name))
    (goto-char (semantic-tag-start tag))
    (when (re-search-forward (concat
                              ;; The tag name is expected to be
                              ;; between word delimiters, whitespaces,
                              ;; or punctuations.
                              "\\(\\<\\|\\s-+\\|\\s.\\)"
                              (regexp-quote name)
                              "\\(\\>\\|\\s-+\\|\\s.\\)")
                             (semantic-tag-end tag)
                             t)
      (goto-char (match-beginning 0))
      (search-forward name))))

(defcustom senator-search-ignore-tag-classes
  '(code block)
  "*List of ignored tag classes.
Tags of those classes are excluded from search."
  :group 'senator
  :type '(repeat (symbol :tag "class")))

(defun senator-search-default-tag-filter (tag)
  "Default function that filters searched tags.
Ignore tags of classes in `senator-search-ignore-tag-classes'"
  (not (memq (semantic-tag-class tag)
             senator-search-ignore-tag-classes)))

(defvar senator-search-tag-filter-functions
  '(senator-search-default-tag-filter)
  "List of functions to be called to filter searched tags.
Each function is passed a tag. If one of them returns nil, the tag is
excluded from the search.")

(defun senator-search (searcher text &optional bound noerror count)
  "Use the SEARCHER function to search from point for TEXT in a tag name.
SEARCHER is typically the function `search-forward', `search-backward',
`word-search-forward', `word-search-backward', `re-search-forward', or
`re-search-backward'.  See one of the above function to see how the
TEXT, BOUND, NOERROR, and COUNT arguments are interpreted."
  (let* ((origin (point))
         (count  (or count 1))
         (step   (cond ((> count 0) 1)
                       ((< count 0) (setq count (- count)) -1)
                       (0)))
         found next sstart send tag tstart tend)
    (or (zerop step)
        (while (and (not found)
                    (setq next (funcall searcher text bound t step)))
          (setq sstart (match-beginning 0)
                send   (match-end 0))
          (if (= sstart send)
              (setq found t)
            (and (setq tag (semantic-current-tag))
                 (run-hook-with-args-until-failure
                  'senator-search-tag-filter-functions tag)
                 (setq tend   (senator-search-tag-name tag))
                 (setq tstart (match-beginning 0)
                       found  (and (>= sstart tstart)
                                   (<= send tend)
                                   (zerop (setq count (1- count))))))
            (goto-char next))))
    (cond ((null found)
           (setq next origin
                 send origin))
          ((= next sstart)
           (setq next send
                 send sstart))
          (t
           (setq next sstart)))
    (goto-char next)
    ;; Setup the returned value and the `match-data' or maybe fail!
    (funcall searcher text send noerror step)))

;;;;
;;;; Navigation commands
;;;;

;;;###autoload
(defun senator-next-tag ()
  "Navigate to the next Semantic tag.
Return the tag or nil if at end of buffer."
  (interactive)
  (let ((pos (point))
        (tag (semantic-current-tag))
        where)
    (if (and tag
             (not (senator-skip-p tag))
             (senator-step-at-start-end-p tag)
             (or (= pos (semantic-tag-start tag))
                 (senator-middle-of-tag-p pos tag)))
        nil
      (if (setq tag (senator-step-at-parent tag))
          nil
        (setq tag (semantic-find-tag-by-overlay-next pos))
        (while (and tag (senator-skip-p tag))
          (setq tag (semantic-find-tag-by-overlay-next
                       (semantic-tag-start tag))))))
    (if (not tag)
        (progn
          (goto-char (point-max))
          (working-message "End of buffer"))
      (cond ((and (senator-step-at-start-end-p tag)
                  (or (= pos (semantic-tag-start tag))
                      (senator-middle-of-tag-p pos tag)))
             (setq where "end")
             (goto-char (semantic-tag-end tag)))
            (t
             (setq where "start")
             (goto-char (semantic-tag-start tag))))
      (senator-momentary-highlight-tag tag)
      (working-message "%S: %s (%s)"
                       (semantic-tag-class tag)
                       (semantic-tag-name  tag)
                       where))
    tag))
(semantic-alias-obsolete 'senator-next-token 'senator-next-tag)

;;;###autoload
(defun senator-previous-tag ()
  "Navigate to the previous Semantic tag.
Return the tag or nil if at beginning of buffer."
  (interactive)
  (let ((pos (point))
        (tag (semantic-current-tag))
        where)
    (if (and tag
             (not (senator-skip-p tag))
             (senator-step-at-start-end-p tag)
             (or (= pos (semantic-tag-end tag))
                 (senator-middle-of-tag-p pos tag)))
        nil
      (if (setq tag (senator-step-at-parent tag))
          nil
        (setq tag (senator-previous-tag-or-parent pos))
        (while (and tag (senator-skip-p tag))
          (setq tag (senator-previous-tag-or-parent
                       (semantic-tag-start tag))))))
    (if (not tag)
        (progn
          (goto-char (point-min))
          (working-message "Beginning of buffer"))
      (cond ((or (not (senator-step-at-start-end-p tag))
                 (= pos (semantic-tag-end tag))
                 (senator-middle-of-tag-p pos tag))
             (setq where "start")
             (goto-char (semantic-tag-start tag)))
            (t
             (setq where "end")
             (goto-char (semantic-tag-end tag))))
      (senator-momentary-highlight-tag tag)
      (working-message "%S: %s (%s)"
                       (semantic-tag-class tag)
                       (semantic-tag-name  tag)
                       where))
    tag))
(semantic-alias-obsolete 'senator-previous-token 'senator-previous-tag)

(defvar senator-jump-completion-list nil
  "`senator-jump' stores here its current completion list.
Then use `assoc' to retrieve the tag associated to a symbol.")

(defun senator-jump-interactive (prompt &optional in-context no-default require-match)
  "Called interactively to provide completion on some tag name.

Use PROMPT.  If optional IN-CONTEXT is non-nil jump in the local
type's context \(see function `senator-current-type-context').  If
optional NO-DEFAULT is non-nil do not provide a default value.  If
optional REQUIRE-MATCH is non-nil an explicit match must be made.

The IN-CONTEXT and NO-DEFAULT switches are combined using the
following prefix arguments:

- \\[universal-argument]       IN-CONTEXT.
- \\[universal-argument] -     NO-DEFAULT.
- \\[universal-argument] \\[universal-argument]   IN-CONTEXT + NO-DEFAULT."
  (let* ((arg (prefix-numeric-value current-prefix-arg))
         (no-default
          (or no-default
              ;; The `completing-read' function provided by XEmacs
              ;; (21.1) don't allow a default value argument :-(
              (featurep 'xemacs)
              (= arg -1)                ; C-u -
              (= arg 16)))              ; C-u C-u
         (in-context
          (or in-context
              (= arg 4)                 ; C-u
              (= arg 16)))              ; C-u C-u
         (context
          (and (not no-default)
               (or (semantic-ctxt-current-symbol)
                   (semantic-ctxt-current-function))))
         (completing-read-args
          (list (if (and context (car context))
                    (format "%s(default: %s) " prompt (car context))
                  prompt)
                (setq senator-jump-completion-list
                      (senator-completion-list in-context))
                nil
                require-match
                ""
                'semantic-read-symbol-history)))
    (list
     (apply #'completing-read
            (if (and context (car context))
                (append completing-read-args context)
              completing-read-args))
     in-context no-default)))

(defun senator-jump-noselect (sym &optional next-p regexp-p)
  "Jump to the semantic symbol SYM.
If NEXT-P is non-nil, then move the the next tag in the search
assuming there was already one jump for the given symbol.
If REGEXP-P is non nil, then treat SYM as a regular expression.
Return the tag jumped to.
Note: REGEXP-P doesn't work yet.  This needs to be added to get
the etags override to be fully functional."
  (let ((tag (cdr (assoc sym senator-jump-completion-list))))
    (when tag
      (set-buffer (semantic-tag-buffer tag))
      (goto-char (semantic-tag-start tag))
      tag)))

;;;###autoload
(defun senator-jump (sym &optional in-context no-default)
  "Jump to the semantic symbol SYM.

If optional IN-CONTEXT is non-nil jump in the local type's context
\(see function `senator-current-type-context').  If optional
NO-DEFAULT is non-nil do not provide a default value.

When called interactively you can combine the IN-CONTEXT and
NO-DEFAULT switches like this:

- \\[universal-argument]       IN-CONTEXT.
- \\[universal-argument] -     NO-DEFAULT.
- \\[universal-argument] \\[universal-argument]   IN-CONTEXT + NO-DEFAULT."
  (interactive (senator-jump-interactive "Jump to: " nil nil t))
  (push-mark)
  (let ((tag (senator-jump-noselect sym no-default)))
    (when tag
      (switch-to-buffer (semantic-tag-buffer tag))
      (senator-momentary-highlight-tag tag)
      (working-message "%S: %s "
                       (semantic-tag-class tag)
                       (semantic-tag-name  tag)))))

;;;###autoload
(defun senator-jump-regexp (symregex &optional in-context no-default)
  "Jump to the semantic symbol SYMREGEX.
SYMREGEX is treated as a regular expression.

If optional IN-CONTEXT is non-nil jump in the local type's context
\(see function `senator-current-type-context').  If optional
NO-DEFAULT is non-nil do not provide a default value and move to the
next match of SYMREGEX.  NOTE: Doesn't actually work yet.

When called interactively you can combine the IN-CONTEXT and
NO-DEFAULT switches like this:

- \\[universal-argument]       IN-CONTEXT.
- \\[universal-argument] -     NO-DEFAULT.
- \\[universal-argument] \\[universal-argument]   IN-CONTEXT + NO-DEFAULT."
  (interactive (senator-jump-interactive "Jump to: "))
  (let ((tag (senator-jump-noselect symregex no-default)))
    (when tag
      (switch-to-buffer (semantic-tag-buffer tag))
      (senator-momentary-highlight-tag tag)
      (working-message "%S: %s "
                       (semantic-tag-class tag)
                       (semantic-tag-name  tag)))))

(defvar senator-last-completion-stats nil
  "The last senator completion was here.
Of the form (BUFFER STARTPOS INDEX REGEX COMPLIST...)")

(defsubst senator-current-symbol-start ()
  "Return position of start of the current symbol under point or nil."
  (let* ((sb (semantic-ctxt-current-symbol-and-bounds (point)))
	 (bounds (nth 2 sb)))
    (car bounds)))

;;  (condition-case nil
;;      (save-excursion (forward-sexp -1) (point))
;;    (error nil)))

;;;###autoload
(defun senator-complete-symbol (&optional cycle-once)
  "Complete the current symbol under point.
If optional argument CYCLE-ONCE is non-nil, only cycle through the list
of completions once, doing nothing where there are no more matches."
  (interactive)
  (let ((symstart (senator-current-symbol-start))
        regex complst)
    (if symstart
        ;; Get old stats if apropriate.
        (if (and senator-last-completion-stats
                 ;; Check if completing in the same buffer
                 (eq (car senator-last-completion-stats) (current-buffer))
                 ;; Check if completing from the same point
                 (= (nth 1 senator-last-completion-stats) symstart)
                 ;; Check if completing the same symbol
                 (save-excursion
                   (goto-char symstart)
                   (looking-at (nth 3 senator-last-completion-stats))))
            
            (setq complst (nthcdr 4 senator-last-completion-stats))
          
          (setq regex (regexp-quote (buffer-substring symstart (point)))
                complst (senator-find-tag-for-completion regex)
                senator-last-completion-stats (append (list (current-buffer)
                                                            symstart
                                                            0
                                                            regex)
                                                      complst))))
    ;; Do the completion if appropriate.
    (if complst
        (let ((ret   t)
              (index (nth 2 senator-last-completion-stats))
              newtok)
          (if (= index (length complst))
              ;; Cycle to the first completion tag.
              (setq index  0
                    ;; Stop completion if CYCLE-ONCE is non-nil.
                    ret (not cycle-once)))
          ;; Get the new completion tag.
          (setq newtok (nth index complst))
          (when ret
            ;; Move index to the next completion tag.
            (setq index (1+ index)
                  ;; Return the completion string (useful to hippie
                  ;; expand for example)
                  ret   (semantic-tag-name newtok))
            ;; Replace the string.
            (delete-region symstart (point))
            (insert ret))
          ;; Update the completion index.
          (setcar (nthcdr 2 senator-last-completion-stats) index)
          ret))))

;;;;
;;;; Completion menu
;;;;

(defcustom senator-completion-menu-summary-function
  'semantic-format-tag-concise-prototype
  "*Function to use when creating items in completion menu.
Some useful functions are in `semantic-format-tag-functions'."
  :group 'senator
  :type semantic-format-tag-custom-list)
(make-variable-buffer-local 'senator-completion-menu-summary-function)

(defcustom senator-completion-menu-insert-function
  'senator-completion-menu-insert-default
  "*Function to use to insert an item from completion menu.
It will receive a Semantic tag as argument."
  :group 'senator
  :type '(radio (const senator-completion-menu-insert-default)
                (function)))
(make-variable-buffer-local 'senator-completion-menu-insert-function)

(defun senator-completion-menu-insert-default (tag)
  "Insert a text representation of TAG at point."
  (insert (semantic-tag-name tag)))

(defun senator-completion-menu-do-complete (tag-array)
  "Replace the current syntactic expression with a chosen completion.
Argument TAG-ARRAY is an array of one element containting the tag
choosen from the completion menu."
  (let ((tag (aref tag-array 0))
        (symstart (senator-current-symbol-start))
        (finsert (if (fboundp senator-completion-menu-insert-function)
                     senator-completion-menu-insert-function
                   #'senator-completion-menu-insert-default)))
    (if symstart
        (progn
          (delete-region symstart (point))
          (funcall finsert tag)))))

(defun senator-completion-menu-item (tag)
  "Return a completion menu item from TAG.
That is a pair (MENU-ITEM-TEXT . TAG-ARRAY).  TAG-ARRAY is an
array of one element containing TAG.  Can return nil to discard a
menu item."
  (cons (funcall (if (fboundp senator-completion-menu-summary-function)
		     senator-completion-menu-summary-function
		   #'semantic-format-tag-prototype) tag)
	(vector tag)))

(defun senator-completion-menu-window-offsets (&optional window)
  "Return offsets of WINDOW relative to WINDOW's frame.
Return a cons cell (XOFFSET . YOFFSET) so the position (X . Y) in
WINDOW is equal to the position ((+ X XOFFSET) .  (+ Y YOFFSET)) in
WINDOW'S frame."
  (let* ((window  (or window (selected-window)))
         (e       (window-edges window))
         (left    (nth 0 e))
         (top     (nth 1 e))
         (right   (nth 2 e))
         (bottom  (nth 3 e))
         (x       (+ left (/ (- right left) 2)))
         (y       (+ top  (/ (- bottom top) 2)))
         (wpos    (coordinates-in-window-p (cons x y) window))
         (xoffset 0)
         (yoffset 0))
    (if (consp wpos)
        (let* ((f  (window-frame window))
               (cy (/ 1.0 (float (frame-char-height f)))))
          (setq xoffset (- x (car wpos))
                yoffset (float (- y (cdr wpos))))
          ;; If Emacs 21 add to:
          ;; - XOFFSET the WINDOW left margin width.
          ;; - YOFFSET the height of header lines above WINDOW.
          (if (> emacs-major-version 20)
              (progn
                (setq wpos    (cons (+ left xoffset) 0.0)
                      bottom  (float bottom))
                (while (< (cdr wpos) bottom)
                  (if (eq (coordinates-in-window-p wpos window)
                          'header-line)
                      (setq yoffset (+ yoffset cy)))
                  (setcdr wpos (+ (cdr wpos) cy)))
                (setq xoffset (floor (+ xoffset
                                        (or (car (window-margins window))
                                            0))))))
          (setq yoffset (floor yoffset))))
    (cons xoffset yoffset)))

(defun senator-completion-menu-point-as-event()
  "Returns the text cursor position as an event.
Also move the mouse pointer to the cursor position."
  (let* ((w (get-buffer-window (current-buffer)))
         (x (mod (- (current-column) (window-hscroll))
                 (window-width)))
         (y (save-excursion
              (save-restriction
                (widen)
                (narrow-to-region (window-start) (point))
                (goto-char (point-min))
                (1+ (vertical-motion (buffer-size))))))
         )
    (if (featurep 'xemacs)
        (let* ((at (progn (set-mouse-position w x (1- y))
                          (cdr (mouse-pixel-position))))
               (x  (car at))
               (y  (cdr at)))
          (make-event 'button-press
                      (list 'button 3
                            'modifiers nil
                            'x x
                            'y y)))
      ;; Emacs
      (let ((offsets (senator-completion-menu-window-offsets w)))
        ;; Convert window position (x,y) to the equivalent frame
        ;; position and move the mouse pointer to it.
        (set-mouse-position (window-frame w)
                            (+ x (car offsets))
                            (+ y (cdr offsets)))
        t))))

;;;###autoload
(defun senator-completion-menu-popup ()
  "Popup a completion menu for the symbol at point.
The popup menu displays all of the possible completions for the symbol
it was invoked on.  To automatically split large menus this function
use `imenu--mouse-menu' to handle the popup menu."
  (interactive)
  (let ((symstart (senator-current-symbol-start))
        symbol regexp complst
	;; Turn off tag jumping for this menu.
	(imenu-default-goto-function (lambda (name pos &optional rest) pos)))
    (if symstart
        (setq symbol  (buffer-substring-no-properties symstart (point))
              regexp  (regexp-quote symbol)
              complst (senator-find-tag-for-completion regexp)))
    (if (not complst)
        (error "No completions available"))
    ;; We have a completion list, build a menu
    (let ((index (delq nil
                       (mapcar #'senator-completion-menu-item
                               complst)))
          title item)
      (cond ;; Here index is a menu structure like:
       
       ;; -1- (("menu-item1" . [tag1]) ...)
       ((vectorp (cdr (car index)))
        ;; There are more than one item, setup the popup title.
        (if (cdr index)
            (setq title (format "%S completion" symbol))
          ;; Only one item , no need to popup the menu.
          (setq item (car index))))
       
       ;; -2- (("menu-title1" ("menu-item1" . [tag1]) ...) ...)
       (t
        ;; There are sub-menus.
        (if (cdr index)
            ;; Several sub-menus, setup the popup title.
            (setq title (format "%S completion" symbol))
          ;; Only one sub-menu, convert it to a main menu and add the
          ;; sub-menu title (filename) to the popup title.
          (setq title (format "%S completion (%s)"
                              symbol (car (car index)))
                index (cdr (car index)))
          ;; But...
          (or (cdr index)
              ;; ... If only one menu item, no need to popup the menu.
              (setq item (car index))))))
      (or item
          ;; `imenu--mouse-menu' automagically splits large menu into
          ;; several submenus, displays the popup menu, and returns
          ;; the selected item :-)
          (setq item (imenu--mouse-menu
                      index
                      ;; popup at point
                      (senator-completion-menu-point-as-event)
                      title)))
      (if item
          (senator-completion-menu-do-complete (cdr item))))))

;;;;
;;;; Search commands
;;;;

;;;###autoload
(defun senator-search-forward (string &optional bound noerror count)
  "Search in tag names forward from point for STRING.
Set point to the end of the occurrence found, and return point.
See also the function `search-forward' for details on the BOUND,
NOERROR and COUNT arguments."
  (interactive "sSemantic search: ")
  (senator-search 'search-forward string bound noerror count))

;;;###autoload
(defun senator-re-search-forward (regexp &optional bound noerror count)
  "Search in tag names forward from point for regular expression REGEXP.
Set point to the end of the occurrence found, and return point.
See also the function `re-search-forward' for details on the BOUND,
NOERROR and COUNT arguments."
  (interactive "sSemantic regexp search: ")
  (senator-search 're-search-forward regexp bound noerror count))

;;;###autoload
(defun senator-word-search-forward (word &optional bound noerror count)
  "Search in tag names forward from point for WORD.
Set point to the end of the occurrence found, and return point.
See also the function `word-search-forward' for details on the BOUND,
NOERROR and COUNT arguments."
  (interactive "sSemantic word search: ")
  (senator-search 'word-search-forward word bound noerror count))

;;;###autoload
(defun senator-search-backward (string &optional bound noerror count)
  "Search in tag names backward from point for STRING.
Set point to the beginning of the occurrence found, and return point.
See also the function `search-backward' for details on the BOUND,
NOERROR and COUNT arguments."
  (interactive "sSemantic backward search: ")
  (senator-search 'search-backward string bound noerror count))

;;;###autoload
(defun senator-re-search-backward (regexp &optional bound noerror count)
  "Search in tag names backward from point for regular expression REGEXP.
Set point to the beginning of the occurrence found, and return point.
See also the function `re-search-backward' for details on the BOUND,
NOERROR and COUNT arguments."
  (interactive "sSemantic backward regexp search: ")
  (senator-search 're-search-backward regexp bound noerror count))

;;;###autoload
(defun senator-word-search-backward (word &optional bound noerror count)
  "Search in tag names backward from point for WORD.
Set point to the beginning of the occurrence found, and return point.
See also the function `word-search-backward' for details on the BOUND,
NOERROR and COUNT arguments."
  (interactive "sSemantic backward word search: ")
  (senator-search 'word-search-backward word bound noerror count))

;;;;
;;;; Others useful search commands (minor mode menu)
;;;;

;;; Compatibility
(or (not (featurep 'xemacs))
    (fboundp 'isearch-update-ring)

    ;; Provide `isearch-update-ring' function.
    ;; (from XEmacs 21.1.9 isearch-mode.el)
    (defun isearch-update-ring (string &optional regexp)
      "Add STRING to the beginning of the search ring.
REGEXP says which ring to use."
      (if (> (length string) 0)
          ;; Update the ring data.
          (if regexp
              (if (not (setq regexp-search-ring-yank-pointer
                             (member string regexp-search-ring)))
                  (progn
                    (setq regexp-search-ring
                          (cons string regexp-search-ring)
                          regexp-search-ring-yank-pointer regexp-search-ring)
                    (if (> (length regexp-search-ring) regexp-search-ring-max)
                        (setcdr (nthcdr (1- regexp-search-ring-max) regexp-search-ring)
                                nil))))
            (if (not (setq search-ring-yank-pointer
                           ;; really need equal test instead of eq.
                           (member string search-ring)))
                (progn
                  (setq search-ring (cons string search-ring)
                        search-ring-yank-pointer search-ring)
                  (if (> (length search-ring) search-ring-max)
                      (setcdr (nthcdr (1- search-ring-max) search-ring) nil)))))))
    
    )

(defvar senator-last-search-type nil
  "Type of last non-incremental search command called.")

(defun senator-nonincremental-repeat-search-forward ()
  "Search forward for the previous search string or regexp."
  (interactive)
  (cond
   ((and (eq senator-last-search-type 'string)
         search-ring)
    (senator-search-forward (car search-ring)))
   ((and (eq senator-last-search-type 'regexp)
         regexp-search-ring)
    (senator-re-search-forward (car regexp-search-ring)))
   (t
    (error "No previous search"))))

(defun senator-nonincremental-repeat-search-backward ()
  "Search backward for the previous search string or regexp."
  (interactive)
  (cond
   ((and (eq senator-last-search-type 'string)
         search-ring)
    (senator-search-backward (car search-ring)))
   ((and (eq senator-last-search-type 'regexp)
         regexp-search-ring)
    (senator-re-search-backward (car regexp-search-ring)))
   (t
    (error "No previous search"))))

(defun senator-nonincremental-search-forward (string)
  "Search for STRING  nonincrementally."
  (interactive "sSemantic search for string: ")
  (setq senator-last-search-type 'string)
  (if (equal string "")
      (senator-search-forward (car search-ring))
    (isearch-update-ring string nil)
    (senator-search-forward string)))

(defun senator-nonincremental-search-backward (string)
  "Search backward for STRING nonincrementally."
  (interactive "sSemantic search for string: ")
  (setq senator-last-search-type 'string)
  (if (equal string "")
      (senator-search-backward (car search-ring))
    (isearch-update-ring string nil)
    (senator-search-backward string)))

(defun senator-nonincremental-re-search-forward (string)
  "Search for the regular expression STRING nonincrementally."
  (interactive "sSemantic search for regexp: ")
  (setq senator-last-search-type 'regexp)
  (if (equal string "")
      (senator-re-search-forward (car regexp-search-ring))
    (isearch-update-ring string t)
    (senator-re-search-forward string)))

(defun senator-nonincremental-re-search-backward (string)
  "Search backward for the regular expression STRING nonincrementally."
  (interactive "sSemantic search for regexp: ")
  (setq senator-last-search-type 'regexp)
  (if (equal string "")
      (senator-re-search-backward (car regexp-search-ring))
    (isearch-update-ring string t)
    (senator-re-search-backward string)))

(defvar senator--search-filter nil)

(defun senator-search-set-tag-class-filter (&optional classes)
  "In current buffer, limit search scope to tag CLASSES.
CLASSES is a list of tag class symbols or nil.  If nil only global
filters in `senator-search-tag-filter-functions' remain active."
  (interactive "sClasses: ")
  (setq classes
        (cond
         ((null classes)
          nil)
         ((symbolp classes)
          (list classes))
         ((stringp classes)
          (mapcar 'read (split-string classes)))
         (t
          (signal 'wrong-type-argument (list classes)))
         ))
  ;; Clear previous filter.
  (remove-hook 'senator-search-tag-filter-functions
               senator--search-filter t)
  (kill-local-variable 'senator--search-filter)
  (if classes
      (let ((tag   (make-symbol "tag"))
            (names (mapconcat 'symbol-name classes "', `")))
        (set (make-local-variable 'senator--search-filter)
             `(lambda (,tag)
                (memq (semantic-tag-class ,tag) ',classes)))
        (semantic-make-local-hook
         'senator-search-tag-filter-functions)
        (add-hook 'senator-search-tag-filter-functions
                  senator--search-filter nil t)
        (message "Limit search to `%s' tags" names))
    (message "Default search filter restored")))

;;;;
;;;; Tag Properties
;;;;

(defun senator-toggle-read-only (&optional tag)
  "Toggle the read-only status of the current TAG."
  (interactive)
  (let* ((tag  (or tag (senator-current-tag)))
         (read (semantic-tag-read-only-p tag)))
    (semantic-set-tag-read-only tag read)
    (semantic-set-tag-face
     tag
     (if read nil 'senator-read-only-face))))

(defun senator-toggle-intangible (&optional tag)
  "Toggle the tangibility of the current TAG."
  (interactive)
  (let* ((tag (or tag (senator-current-tag)))
         (tang (semantic-tag-intangible-p tag)))
    (semantic-set-tag-intangible tag tang)
    (semantic-set-tag-face
     tag
     (if tang nil 'senator-intangible-face))))

(defun senator-set-face (face &optional tag)
  "Set the foreground FACE of the current TAG."
  (interactive (list (read-face-name
                      (if (featurep 'xemacs)
                          "Face: "
                        ;; GNU Emacs already append ": "
                        "Face"))))
  (let ((tag (or tag (senator-current-tag))))
    (semantic-set-tag-face tag face)))

(defun senator-set-foreground (color &optional tag)
  "Set the foreground COLOR of the current TAG."
  ;; This was copied from facemenu
  (interactive (list (facemenu-read-color "Foreground color: ")))
  (let ((face (intern (concat "fg:" color))))
    (or (facemenu-get-face face)
        (error "Unknown color: %s" color))
    (senator-set-face face)))

(defun senator-set-background (color &optional tag)
  "Set the background COLOR of the current TAG."
  ;; This was copied from facemenu
  (interactive (list (facemenu-read-color "Background color: ")))
  (let ((face (intern (concat "bg:" color))))
    (or (facemenu-get-face face)
        (error "Unknown color: %s" color))
    (senator-set-face face)))

(defun senator-clear-tag (&optional tag)
  "Clear all properties from TAG."
  (interactive)
  (let ((tag (or tag (senator-current-tag))))
    (semantic-set-tag-read-only  tag t)
    (semantic-set-tag-intangible tag t)
    (semantic-set-tag-face       tag nil)))
(semantic-alias-obsolete 'senator-clear-token 'senator-clear-tag)

;;; Folding
;;
;; Use new folding state.  It might be wise to extend the idea
;; of folding for hiding all but this, or show all children, etc.

(defun senator-fold-tag (&optional tag)
  "Fold the current TAG."
  (interactive)
  (semantic-set-tag-folded (or tag (semantic-current-tag)) t))

(defun senator-unfold-tag (&optional tag)
  "Fold the current TAG."
  (interactive)
  (semantic-set-tag-folded (or tag (semantic-current-tag)) nil))

(defun senator-fold-tag-toggle (&optional tag)
  "Fold the current TAG."
  (interactive)
  (let ((tag (or tag (semantic-current-tag))))
    (if (semantic-tag-folded-p tag)
	(senator-unfold-tag tag)
      (senator-fold-tag tag))))

;;; Pulsing
;;
;; Useful for debugging parsing state.
;;
(defun senator-pulse-tag (&optional tag)
  "Pulse the current TAG."
  (interactive)
  (senator-force-refresh)
  (let ((tag (semantic-current-tag)))
    (when tag
      (message "%s" (semantic-format-tag-summarize tag))
      (pulse-momentary-highlight-overlay (semantic-tag-overlay tag)))
    ))

(defun senator-adebug-tag (&optional tag)
  "Pulse the current TAG."
  (interactive)
  (senator-force-refresh)
  (let ((tag (semantic-current-tag))
	(w (selected-window)))
    (when tag
      (message "%s" (semantic-format-tag-summarize tag))
      (data-debug-new-buffer "*Senator Tag ADEBUG*")
      (data-debug-insert-tag tag "* " "")
      (forward-line -1)
      (data-debug-insert-tag-parts-from-point (point))
      (select-window w t)
      (pulse-momentary-highlight-overlay (semantic-tag-overlay tag))
      )
    ))

;;;;
;;;;
;;;;
(defun senator-go-to-up-reference (&optional tag)
  "Move up one reference from the current TAG.
A \"reference\" could be any interesting feature of TAG.
In C++, a function may have a 'parent' which is non-local.
If that parent which is only a reference in the function tag
is found, we can jump to it.
Some tags such as includes have other reference features."
  (interactive)
  (let ((newtag (semantic-up-reference (or tag (semantic-current-tag)))))
    (if (not newtag)
	(error "No up reference found")
      (push-mark)
      (semantic-go-to-tag newtag)
      (switch-to-buffer (current-buffer))
      (semantic-momentary-highlight-tag newtag))))

(define-overloadable-function semantic-up-reference (tag)
  "Return a tag that is referredto by TAG.
A \"reference\" could be any interesting feature of TAG.
In C++, a function may have a 'parent' which is non-local.
If that parent which is only a reference in the function tag
is found, we can jump to it.
Some tags such as includes have other reference features.")

(defun semantic-up-reference-default (tag)
  "Return a tag that is referredto by TAG.
Makes C/C++ language like assumptions."
  (cond ((semantic-tag-faux-p tag)
	 ;; Faux tags should have a real tag in some other location.
	 (let ((options (semantic-tag-external-class tag)))
	   ;; I should do something a little better than
	   ;; this.  Oy!
	   (car options)
	   ))
	((eq (semantic-tag-class tag) 'include)
	 ;; Include always point to another file.
	 tag
	 ;; Note: if you then call 'semantic-go-to-tag', then
	 ;; you would just to the source of this tag.
	 )
	((and (eq (semantic-tag-class tag) 'function)
	      (semantic-tag-function-parent tag))
	 ;; Is there a parent of the function to jump to?
	 (let* ((p (semantic-tag-function-parent tag))
		(sr1 (semanticdb-find-tags-by-name p))
		(sr2 (when sr1
		       (semanticdb-find-tags-by-class 'type sr1)))
		)
	   (semanticdb-find-result-nth-in-buffer sr2 0)
	   )
	 )
	((and (eq (semantic-tag-class tag) 'function)
	      (semantic-tag-get-attribute tag :prototype-flag))
	 ;; Is there a parent of the function to jump to?
	 (let* ((p (semantic-tag-name tag))
		(sr1 (semanticdb-find-tags-by-name p))
		(sr2 (when sr1
		       (semanticdb-find-tags-by-class
			(semantic-tag-class tag)
			sr1)))
		(int 0)
		(len (semanticdb-find-result-length sr2)))
	   (while (and (< int len)
		       (semantic-tag-get-attribute
			(semanticdb-find-result-nth-in-buffer sr2 int)
			:prototype-flag))
	     (setq int (1+ int)))
	   (semanticdb-find-result-nth-in-buffer sr2 int)
	   ))
	((semantic-tag-type tag)
	 ;; Get the data type, and try to find that.
	 (let* ((type (semantic-tag-type tag))
		(tn (cond ((stringp type)
			   type)
			  ((semantic-tag-p type)
			   (semantic-tag-name type))
			  (t (error "No known type"))))
		(sr1 (semanticdb-find-tags-by-name tn))
		(sr2 (when sr1
		       (semanticdb-find-tags-by-class 'type sr1))))
	   (semanticdb-find-result-nth-in-buffer sr2 0)
	   ))
	(t nil)))

;;;;
;;;; Misc. menu stuff.
;;;;

(defalias 'senator-menu-item 'semantic-menu-item)

;;;;
;;;; The dynamic sub-menu of Semantic minor modes.
;;;;
(defvar senator-registered-mode-entries  nil)
(defvar senator-registered-mode-settings nil)
(defvar senator-modes-menu-cache nil)

(defun senator-register-command-menu (spec global)
  "Register the minor mode menu item specified by SPEC.
Return a menu item allowing to change the corresponding minor mode
setting.  If GLOBAL is non-nil SPEC defines a global setting else a
local setting.

SPEC must be a list of the form:

\(CALLBACK [ KEYWORD ARG ] ... )

Where KEYWORD is one of those recognized by `easy-menu-define' plus:

:save VARIABLE

VARIABLE is a variable that will be saved by Custom when using the
\"Modes/Save global settings\" menu item.  This keyword is ignored if
GLOBAL is nil.

By default the returned menu item is setup with:

:active t :style toggle :selected CALLBACK.

So when :selected is not specified the function assumes that CALLBACK
is a symbol which refer to a bound variable too."
  (and (consp spec)
       (symbolp (car spec))
       (fboundp (car spec))
       (let* ((callback (car spec))     ; callback function
              (props    (cdr spec))     ; properties
              (selected callback)       ; selected default to callback
              (active   t)              ; active by default
              (style    'toggle)        ; toggle style by default
              (save     nil)            ; what to save via custom
              item key val)
         (while props
           (setq key   (car  props)
                 val   (cadr props)
                 props (cddr props))
           (cond
            ((eq key :save)
             (setq save val))
            ((eq key :active)
             (setq active val))
            ((eq key :style)
             (setq style val))
            ((eq key :selected)
             (setq selected val))
            (t
             (setq item (cons key (cons val item))))))
         (if (and global save (symbolp save) (boundp save))
             (add-to-list 'senator-registered-mode-settings save))
         (setq item (cons :selected (cons selected item))
               item (cons :active   (cons active   item))
               item (cons :style    (cons style    item))
               item (cons callback  item)))))

(defun senator-register-custom-menu (spec)
  "Register SPEC as a menu item entry for customizing some aspect of a mode.
SPEC can either be one entry, or a list of SPEC entries.  A SPEC is of
the form

\(SYMBOL [ :KEYWORD ARG ] ...)

Valid keywords include:

  :name - ARG represents the string used in the menu item.

  :style - ARG represents the style of menu item this is.  Values for ARG
           include:
     :group  - SYMBOL is a group that `customize-group' will be called on.
     :variable - SYMBOL is a variable that `customize-variable' will be called on.
     :face - SYMBOL is a face that `customize-face' will be called on.
     :toggle - SYMBOL is a variable that will be toggled on and off.

     If :style is not specified, the symbol is queried to try and
     predict the correct style to use.

  :option-symbol - SYMBOL is a variable that contains a value that is from
            a list of symbols.  ARG should be a list of symbols that can assign
            to the variable SYMBOL."
  (if (and (not (null spec)) (not (consp spec)))
      (signal 'wrong-type-argument (list spec)))
  ;; Turn spec into a list of specs if it is not so already.
  (if (and spec (not (consp (car spec)))) (setq spec (list spec)))
  (let ((menulist nil)
	)
    (while spec
      (let* ((sym (car (car spec)))
	     (pl (cdr (car spec)))
	     (name (car-safe (cdr-safe (member :name pl))))
	     (style (car-safe (cdr-safe (member :style pl)))))
	(if (not style)
	    (setq style
		  (cond
		   ((get sym 'custom-group)
		    :group)
		   ((facep sym)
		    :face)
		   ((boundp sym)
		    :variable))))
	(if (not name)
	    (setq name (symbol-name sym)))
	(setq menulist
	      (cons
	       (cond
		((eq style :group)
		 (senator-menu-item
		  (vector (concat "Customize Group " name)
			  `(lambda (ARG) (interactive "p")
			     (customize-group (quote ,sym)))
			  :help (format "Customize Group %s" name)))
		 )
		((eq style :variable)
		 (senator-menu-item
		  (vector (concat "Customize " name)
			  `(lambda (ARG) (interactive "p")
			     (customize-variable (quote ,sym)))
			  :help (format "Customize Variable %s" name)))
		 )
		((eq style :face)
		 (senator-menu-item
		  (vector (concat "Customize " name)
			  `(lambda (ARG) (interactive "p")
			     (customize-face (quote ,sym)))
			  :help (format "Customize Face %s" name)))
		 )
		((eq style :toggle)
		 ))
	       menulist))
	)
      (setq spec (cdr spec)))
    menulist))

(defun senator-register-mode-menu-entry (name local global &optional custom)
  "Register a minor mode menu entry.
This will add menu items to the \"Modes\" menu allowing to change the
minor mode settings.  NAME is the name displayed in the menu.  LOCAL
and GLOBAL define command menu items to respectively change the minor
mode local and global settings.  nil means to omit the corresponding
menu item.  See the function `senator-register-command-menu' for the
command menu specification.  If NAME is already registered the
corresponding entry will be updated with the given LOCAL and GLOBAL
definitions.  If LOCAL and GLOBAL are both nil the NAME entry is
unregistered if present.
Optional fourth argument CUSTOM represents a menu item, or submenu
item that will customize something about the mode being registered.
See the function `senator-register-custom-menu' for the details on
what this menu looks like."
  ;; Clear the cached menu to rebuild it.
  (setq senator-modes-menu-cache nil)
  (let* ((entry (assoc name senator-registered-mode-entries))
         (local-item  (senator-register-command-menu local nil))
         (global-item (senator-register-command-menu global t))
	 (custom-item (senator-register-custom-menu custom))
	 (entry-construct (append (list local-item global-item)
				  custom-item))
	 )
    
    (if (not (or local-item global-item))
        (setq senator-registered-mode-entries
              (delq entry senator-registered-mode-entries))
      (if entry
          (setcdr entry entry-construct)
        (setq entry (cons name entry-construct)
              senator-registered-mode-entries
              (nconc senator-registered-mode-entries (list entry))))
      entry)))

(defsubst senator-build-command-menu-item (label props)
  "Return a command menu item with an unique name based on LABEL.
PROPS is the list of properties of this menu item."
  (if props
      (senator-menu-item (apply #'vector (cons label props)))))

(defcustom senator-mode-menu-local-toggle-label  "In this buffer"
  "*Label of menu item that toggles a Semantic minor mode locally."
  :group 'senator
  :type 'string
  :set (lambda (sym val)
         (set-default sym val)
         (setq senator-modes-menu-cache nil)))

(defcustom senator-mode-menu-global-toggle-label "Globally"
  "*Label of menu item that toggles a Semantic minor mode globally."
  :group 'senator
  :type 'string
  :set (lambda (sym val)
         (set-default sym val)
         (setq senator-modes-menu-cache nil)))

(defun senator-build-mode-sub-menu (entry)
  "Return a sub-menu for the registered minor mode ENTRY.
The sub-menu displayed in the Senator/Modes menu looks like this:

  Entry-Name
            [x] In this buffer
            [x] Globally
            Custom1
            Custom2
            ...

The menu item \"In this buffer\" toggles the minor mode locally.
The menu item \"Globally\" toggles the minor mode globally."
  (let ((mode (nth 1  entry))
	(global (nth 2 entry))
	(customs (nthcdr 3 entry)))
    (delq nil
	  (append
	   (list (car entry)
		 (senator-build-command-menu-item
		  senator-mode-menu-local-toggle-label mode)
		 (senator-build-command-menu-item
		  senator-mode-menu-global-toggle-label global))
	   (if (and (symbolp customs) (fboundp customs))
	       (funcall customs)
	     customs)))))

(defun senator-build-modes-menu (&rest ignore)
  "Build and return the \"Modes\" menu.
It is dynamically build from registered minor mode entries.  See also
the function `senator-register-mode-menu-entry'.
IGNORE any arguments.
This function is a menu :filter."
  (or senator-modes-menu-cache
      (setq senator-modes-menu-cache
            (nconc (mapcar 'senator-build-mode-sub-menu
                           senator-registered-mode-entries)
                   (list "--"
                         (senator-menu-item
                          [ "Save global settings"
                            senator-save-registered-mode-settings
                            :help "\
Save global settings of Semantic minor modes in your init file."
                            ]))))))

(defun senator-save-registered-mode-settings ()
  "Save current value of registered minor modes global setting.
The setting is saved by Custom.  See the function
`senator-register-mode-menu-entry' for details on how to register a
minor mode entry."
  (interactive)
  (dolist (opt senator-registered-mode-settings)
    (condition-case nil
        (customize-save-variable opt (default-value opt))
      (error nil))))

;; Register the various minor modes settings used by Semantic.
(senator-register-mode-menu-entry
 "Senator"
 '(senator-minor-mode
   :help "Turn off Senator minor mode."
   )
 '(global-senator-minor-mode
   :help "Automatically turn on Senator on all Semantic buffers."
   :save global-senator-minor-mode
   )
 '(senator)
 )

(senator-register-mode-menu-entry
 "Highlight changes"
 '(semantic-highlight-edits-mode
   :help "Highlight changes tracked by Semantic."
   )
 '(global-semantic-highlight-edits-mode
   :help "Automatically highlight changes in all Semantic buffers."
   :save global-semantic-highlight-edits-mode
   )
 '(semantic-highlight-edits-face)
 )

(senator-register-mode-menu-entry
 "Show parser state"
 '(semantic-show-parser-state-mode
   :help "`-': OK, `!': will parse all, `~': will parse part, `@': running."
   )
 '(global-semantic-show-parser-state-mode
   :help "Automatically show parser state in all Semantic buffers."
   :save global-semantic-show-parser-state-mode
   )
 )

(senator-register-mode-menu-entry
 "Highlight Unmatched Syntax"
 '(semantic-show-unmatched-syntax-mode
   :help "Highlight syntax which is not recognized valid syntax."
   )
 '(global-semantic-show-unmatched-syntax-mode
   :help "Automatically highlight unmatched syntax in all Semantic buffers."
   :save global-semantic-show-unmatched-syntax-mode
   )
 '(semantic-unmatched-syntax-face)
 )

(senator-register-mode-menu-entry
 "Idle Scheduler"
 '(semantic-idle-scheduler-mode
   :help "Schedule idle time to automatically parse buffer following changes."
   )
 '(global-semantic-idle-scheduler-mode
   :help "Schedule idle time to automatically parse all Semantic buffer following changes."
   :save global-semantic-idle-scheduler-mode
   )
 '((semantic-idle-scheduler-idle-time)
   (semantic-idle-scheduler-max-buffer-size)
   (semantic-idle-scheduler-verbose-flag)
   )
 )

(senator-register-mode-menu-entry
 "Idle Summaries"
 '(semantic-idle-summary-mode
   :help "Show tag summaries in idle time."
   )
 '(global-semantic-idle-summary-mode
   :help "Show tag summaries in idle time in all buffers."
   :save global-semantic-idle-summary-mode
   )
 '((semantic-idle-summary-function)
   )
 )

(senator-register-mode-menu-entry
 "Idle Completion"
 '(semantic-idle-completions-mode
   :help "Show completion tips in idle time."
   )
 '(global-semantic-idle-completions-mode
   :help "Show completion tips in idle time in all buffers."
   :save global-semantic-idle-completions-mode
   )
 '((semantic-complete-inline-analyzer-displayor-class)
   )
 )

(senator-register-mode-menu-entry
 "Sticky Func"
 '(semantic-stickyfunc-mode
   :help "Cause function declaration to become stuck to the header line."
   )
 '(global-semantic-stickyfunc-mode
   :help "Automatically enable sticky function mode in all Semantic buffers."
   :save global-semantic-stickyfunc-mode
   )
 )

(senator-register-mode-menu-entry
 "Highlight Func"
 '(semantic-highlight-func-mode
   :help "Cause function declaration the cursor is in be highlighted."
   )
 '(global-semantic-highlight-func-mode
   :help "Automatically enable highlight function mode in all Semantic buffers."
   :save global-semantic-highlight-func-mode
   )
 )

(senator-register-mode-menu-entry
 "Tag Decoration"
 '(semantic-decoration-mode
   :help "Decorate Tags."
   )
 '(global-semantic-decoration-mode
   :help "Automatically enable decoration mode in all Semantic buffers."
   :save global-semantic-decoration-mode
   )
  )

(senator-register-mode-menu-entry
 "MRU Bookmark"
 nil
 '(global-semantic-mru-bookmark-mode
   :help "Automatically enable MRU bookmark tracking at a tag level."
   :save global-semantic-mru-bookmark-mode
   )
  )


;;;;
;;;; Global minor mode to show tag names in the mode line
;;;;

(condition-case nil
    (require 'which-func)
  (error nil))

(let ((select (cond
               ;; Emacs 21
               ((boundp 'which-function-mode)
                'which-function-mode)
               ;; Emacs < 21
               ((boundp 'which-func-mode-global)
                'which-func-mode-global)
               (t nil))))
  (if (and (fboundp 'which-func-mode) select)
      (senator-register-mode-menu-entry
       "Which Function"
       nil
       (list 'which-func-mode
             :select select
             :help "Enable `which-func-mode' and use it in Semantic buffers."
             :save select
             ))
    ))

(senator-register-mode-menu-entry
 "Semantic Database"
 nil
 '(semanticdb-toggle-global-mode
   :active (featurep 'semanticdb)
   :selected (and (featurep 'semanticdb) (semanticdb-minor-mode-p))
   :suffix (if (and (featurep 'semanticdb) (semanticdb-minor-mode-p))
               (if (semanticdb-write-directory-p
                    semanticdb-current-database)
                   "[persist]"
                 "[session]")
             "")
   :help "Cache tags for killed buffers and between sessions."
   :save semanticdb-global-mode
   )
 )

;;;;
;;;; Senator minor mode
;;;;

(defvar senator-status nil
  "Minor mode status displayed in the mode line.")
(make-variable-buffer-local 'senator-status)

(defvar senator-isearch-semantic-mode nil
  "Non-nil if isearch does semantic search.
This is a buffer local variable.")
(make-variable-buffer-local 'senator-isearch-semantic-mode)

(defvar senator-prefix-key [(control ?c) ?,]
  "The common prefix key in senator minor mode.")

(defvar senator-prefix-map
  (let ((km (make-sparse-keymap)))
    (define-key km "f"    'senator-search-set-tag-class-filter)
    (define-key km "i"    'senator-isearch-toggle-semantic-mode)
    (define-key km "j"    'semantic-complete-jump-local) ;senator-jump)
    (define-key km "J"    'semantic-complete-jump)
    (define-key km "p"    'senator-previous-tag)
    (define-key km "n"    'senator-next-tag)
    (define-key km "u"    'senator-go-to-up-reference)
    (define-key km "\t"   'senator-complete-symbol)
    (define-key km " "    'senator-completion-menu-popup)
    (define-key km "\C-w" 'senator-kill-tag)
    (define-key km "\M-w" 'senator-copy-tag)
    (define-key km "\C-y" 'senator-yank-tag)
    (define-key km "r"    'senator-copy-tag-to-register)
    (define-key km "t"    'senator-transpose-tags-up)
    (define-key km "-"    'senator-fold-tag)
    (define-key km "+"    'senator-unfold-tag)
    (define-key km "?"    'senator-pulse-tag)
    (define-key km "/"    'senator-adebug-tag)
    
    km)
  "Default key bindings in senator minor mode.")

(defvar senator-menu-bar
  (list
   "Senator"
   (senator-menu-item
    ["Force Tag Refresh"
     senator-force-refresh
     :active t
     :help "Force a full reparse of the current buffer."])
   (list
    "Navigate"
    (senator-menu-item
     ["Next"
      senator-next-tag
      :active t
      :help "Go to the next tag found"
      ])
    (senator-menu-item
     ["Previous"
      senator-previous-tag
      :active t
      :help "Go to the previous tag found"
      ])
    (senator-menu-item
     ["Up Reference"
      senator-go-to-up-reference
      :active t
      :help "Navigate up one reference by tag."
      ])
    (senator-menu-item
     ["Jump in this file..."
      semantic-complete-jump-local
      :active t
      :help "Jump to a semantic symbol"
      ])
    (senator-menu-item
     ["Jump to any tag..."
      semantic-complete-jump
      :active t
      :help "Jump to any semantic symbol in this project"
      ])
    (senator-menu-item
     ["Narrow to tag"
      senator-narrow-to-defun
      :active (semantic-current-tag)
      :help "Narrow to the bounds of the current tag."
      ])
    )
   (list
    "Search"
    (senator-menu-item
     ["String Forward..."
      senator-nonincremental-search-forward
      :active t
      :help "Search forward for a string"
      ])
    (senator-menu-item
     ["String Backwards..."
      senator-nonincremental-search-backward
      :active t
      :help "Search backwards for a string"
      ])
    (senator-menu-item
     ["Regexp Forward..."
      senator-nonincremental-re-search-forward
      :active t
      :help "Search forward for a regular expression"
      ])
    (senator-menu-item
     ["Regexp Backwards..."
      senator-nonincremental-re-search-backward
      :active t
      :help "Search backwards for a regular expression"
      ])
    "--"
    (senator-menu-item
     ["Repeat Forward"
      senator-nonincremental-repeat-search-forward
      :active (or (and (eq senator-last-search-type 'string)
                       search-ring)
                  (and (eq senator-last-search-type 'regexp)
                       regexp-search-ring))
      :help "Repeat last search forward"
      ])
    (senator-menu-item
     ["Repeat Backwards"
      senator-nonincremental-repeat-search-backward
      :active (or (and (eq senator-last-search-type 'string)
                       search-ring)
                  (and (eq senator-last-search-type 'regexp)
                       regexp-search-ring))
      :help "Repeat last search backwards"
      ])
    "--"
    (senator-menu-item
     ["Limit search..."
      senator-search-set-tag-class-filter
      :active t
      :help "In current buffer, limit search to certain classes of tag"
      ])
    (senator-menu-item
     ["Semantic isearch mode"
      senator-isearch-toggle-semantic-mode
      :active t
      :style toggle :selected senator-isearch-semantic-mode
      :help "Toggle semantic search in isearch mode"
      ])
    )
   (list
    "Tag Copy/Paste"
    (senator-menu-item
     [ "Copy Tag"
       senator-copy-tag
       :active (semantic-current-tag)
       :help "Copy the current tag to the tag ring"
       ])
    (senator-menu-item
     [ "Kill Tag"
       senator-kill-tag
       :active (semantic-current-tag)
       :help "Kill tag text to the kill ring, and copy the tag to the tag ring"
       ])
    (senator-menu-item
     [ "Yank Tag"
       senator-yank-tag
       :active (not (ring-empty-p senator-tag-ring))
       :help "Yank a tag from the tag ring, inserting a summary/prototype"
       ])
    (senator-menu-item
     [ "Copy Tag to Register"
       senator-copy-tag-to-register
       :active (semantic-current-tag)
       :help "Copy the current tag to a register"
       ])
    (senator-menu-item
     [ "Transpose Tag Up"
       senator-transpose-tags-up
       :active (semantic-current-tag)
       :help "Transpose the current tag up"
       ])
    (senator-menu-item
     [ "Transpose Tag Down"
       senator-transpose-tags-down
       :active (semantic-current-tag)
       :help "Transpose the current tag down"
       ])

    )
   (list
    "Tag Properties"
    (senator-menu-item
     [ "Fold Tag"
       senator-fold-tag-toggle
       :active (semantic-current-tag)
       :style toggle
       :selected (let ((tag (semantic-current-tag)))
                   (and tag (semantic-tag-folded-p tag)))
       :help "Fold the current tag to one line"
       ])
    (senator-menu-item
     [ "Read Only"
       senator-toggle-read-only
       :active (semantic-current-tag)
       :style toggle
       :selected (let ((tag (semantic-current-tag)))
                   (and tag (semantic-tag-read-only-p tag)))
       :help "Make the current tag read-only"
       ])
    (senator-menu-item
     [ "Intangible"
       senator-toggle-intangible
       ;; XEmacs extent `intangible' property seems to not exists.
       :active (and (not (featurep 'xemacs))
                    (semantic-current-tag))
       :style toggle
       :selected (and (not (featurep 'xemacs))
                      (let ((tag (semantic-current-tag)))
                        (and tag (semantic-tag-intangible-p tag))))
       :help "Make the current tag intangible"
       ])
    (senator-menu-item
     [ "Set Tag Face"
       senator-set-face
       :active (semantic-current-tag)
       :help "Set the face on the current tag"
       ])
    (senator-menu-item
     [ "Set Tag Foreground"
       senator-set-foreground
       :active (semantic-current-tag)
       :help "Set the foreground color on the current tag"
       ])
    (senator-menu-item
     [ "Set Tag Background"
       senator-set-background
       :active (semantic-current-tag)
       :help "Set the background color on the current tag"
       ])
    (senator-menu-item
     [ "Remove all properties"
       senator-clear-tag
       :active (semantic-current-tag)
       :help "Remove all special face properties on the current tag "
       ] )
    )
   (if (or (featurep 'xemacs) (> emacs-major-version 20))
       (list "Tag Decorations" :filter 'semantic-build-decoration-mode-menu)
     ;; The :filter feature seems broken in GNU Emacs versions before
     ;; 21.1.  So dont delay the menu creation.  This also means that
     ;; new registered decoration entries will not be added "on the
     ;; fly" to the menu :-(
     (cons "Tag Decorations" (semantic-build-decoration-mode-menu)))
   "--"
   (list
    "Analyze"
    (senator-menu-item
     [ "Inline Smart completion"
       semantic-complete-analyze-inline
       :active t
       :help "Complete the symbol in the current buffer."
       ])
    (senator-menu-item
     [ "Speedbar Analyzer Mode"
       semantic-speedbar-analysis
       :active t
       :help "Start speedbar in Context Analysis/Completion mode."
       ])
    (senator-menu-item
     [ "Context Analysis Dump"
       semantic-analyze-current-context
       :active t
       :help "Show a dump of an analysis of the current local context"
       ])
    (senator-menu-item
     [ "Smart Completion Dump"
       semantic-analyze-possible-completions
       :active t
       :help "Show a dump of the semantic analyzer's guess at possible completions"
       ])
    (senator-menu-item
     [ "Analyzer Debug Assitant"
       semantic-analyze-debug-assist
       :active t
       :help "Debug why the analyzer may not be working for you."
       ])
   (senator-menu-item
    [ "Summarize includes current buffer"
      semantic-decoration-all-include-summary
     :active t
     :help "Show a summary of what Semantic has done with your includes for this buffer." ])
   )
   (list
    "Find References"
    (senator-menu-item
     ["Find This Tag"
      semantic-symref
      :active (semantic-current-tag)
      :help "Find uses of the tag the cursor is in."])
    (senator-menu-item
     ["Find Symbol"
      semantic-symref
      :active t
      :help "Find uses of any arbitrary symbol."])
    )
   (list
    "Chart"
    (senator-menu-item
     [ "Chart Tags by Class"
       semantic-chart-tags-by-class
       :active t
       :help "Catagorize all tags by class, and chart the volume for each class"
       ])
    (senator-menu-item
     [ "Chart Tags by Complexity"
       semantic-chart-tag-complexity
       :active t
       :help "Choose the most complex tags, and chart them by complexity"
       ])
    (senator-menu-item
     [ "Chart File Complexity"
       semantic-chart-database-size
       :active (and (featurep 'semanticdb) (semanticdb-minor-mode-p))
       :help "Choose the files with the most tags, and chart them by volume"
       ])
    (senator-menu-item
     [ "Chart Analyzer Overhead"
       semantic-chart-analyzer
       :active t
       :help "Calculate the overhead of running the analyzer, and chart it out."
       ])
    )
   (if (or (featurep 'xemacs) (> emacs-major-version 20))
       (list "Modes" :filter 'senator-build-modes-menu)
     ;; The :filter feature seems broken in GNU Emacs versions before
     ;; 21.1.  So dont delay the menu creation.  This also means that
     ;; new registered minor mode entries will not be added "on the
     ;; fly" to the menu :-(
     (cons "Modes" (senator-build-modes-menu)))
   "--"
   (list
    "Imenu Config"
    (list
     "Tag Sorting Function"
     (senator-menu-item
      [ "Do not sort"
        (setq semantic-imenu-sort-bucket-function nil)
        :active t
        :style radio
        :selected (eq semantic-imenu-sort-bucket-function nil)
        :help "Do not sort imenu items"
        ])
     (senator-menu-item
      [ "Increasing by name"
        (setq semantic-imenu-sort-bucket-function
              'semantic-sort-tags-by-name-increasing)
        :active t
        :style radio
        :selected (eq semantic-imenu-sort-bucket-function
                      'semantic-sort-tags-by-name-increasing)
        :help "Sort tags by name increasing"
        ])
     (senator-menu-item
      [ "Decreasing by name"
        (setq semantic-imenu-sort-bucket-function
              'semantic-sort-tags-by-name-decreasing)
        :active t
        :style radio
        :selected (eq semantic-imenu-sort-bucket-function
                      'semantic-sort-tags-by-name-decreasing)
        :help "Sort tags by name decreasing"
        ])
     (senator-menu-item
      [ "Increasing Case Insensitive by Name"
        (setq semantic-imenu-sort-bucket-function
              'semantic-sort-tags-by-name-increasing-ci)
        :active t
        :style radio
        :selected (eq semantic-imenu-sort-bucket-function
                      'semantic-sort-tags-by-name-increasing-ci)
        :help "Sort tags by name increasing and case insensitive"
        ])
     (senator-menu-item
      [ "Decreasing Case Insensitive by Name"
        (setq semantic-imenu-sort-bucket-function
              'semantic-sort-tags-by-name-decreasing-ci)
        :active t
        :style radio
        :selected (eq semantic-imenu-sort-bucket-function
                      'semantic-sort-tags-by-name-decreasing-ci)
        :help "Sort tags by name decreasing and case insensitive"
        ])
     )
    (senator-menu-item
     [ "Bin tags by class"
       (setq semantic-imenu-bucketize-file
             (not semantic-imenu-bucketize-file))
       :active t
       :style toggle
       :selected semantic-imenu-bucketize-file
       :help "Organize tags in bins by class of tag"
       ])
    (senator-menu-item
     [ "Bins are submenus"
       (setq semantic-imenu-buckets-to-submenu
             (not semantic-imenu-buckets-to-submenu))
       :active t
       :style toggle
       :selected semantic-imenu-buckets-to-submenu
       :help "Organize tags into submenus by class of tag"
       ])
    (senator-menu-item
     [ "Bin tags in components"
       (setq semantic-imenu-bucketize-type-members
             (not semantic-imenu-bucketize-type-members))
       :active t
       :style toggle
       :selected semantic-imenu-bucketize-type-members
       :help "When listing tags inside another tag; bin by tag class"
       ])
    (senator-menu-item
     [ "List other files"
       (setq semantic-imenu-index-directory (not semantic-imenu-index-directory))
       :active (and (featurep 'semanticdb) (semanticdb-minor-mode-p))
       :style toggle
       :selected semantic-imenu-index-directory
       :help "List all files in the current database in the Imenu menu"
       ])
    (senator-menu-item
     [ "Auto-rebuild other buffers"
       (setq semantic-imenu-auto-rebuild-directory-indexes
             (not semantic-imenu-auto-rebuild-directory-indexes))
       :active (and (featurep 'semanticdb) (semanticdb-minor-mode-p))
       :style toggle
       :selected semantic-imenu-auto-rebuild-directory-indexes
       :help "If listing other buffers, update all buffer menus after a parse"
       ])
    )
   (list
    "Options"
    (senator-menu-item
     ["Semantic..."
      (customize-group "semantic")
      :active t
      :help "Customize Semantic options"
      ])
    (senator-menu-item
     ["Senator..."
      (customize-group "senator")
      :active t
      :help "Customize SEmantic NAvigaTOR options"
      ])
    (senator-menu-item
     ["Semantic Imenu..."
      (customize-group "semantic-imenu")
      :active t
      :help "Customize Semantic Imenu options"
      ])
    (senator-menu-item
     ["Semantic Database..."
      (customize-group "semanticdb")
      :active t
      :help "Customize Semantic Database options"
      ])
    )
   )
  "Menu for senator minor mode.")

(defvar senator-minor-menu nil
  "Menu keymap build from `senator-menu-bar'.")

(defvar senator-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km senator-prefix-key senator-prefix-map)
    (define-key km [(shift mouse-3)] 'senator-completion-menu-popup)
    (easy-menu-define senator-minor-menu km "Senator Minor Mode Menu"
                      senator-menu-bar)
    km)
  "Keymap for senator minor mode.")

(defvar senator-minor-mode nil
  "Non-nil if Senator minor mode is enabled.
Use the command `senator-minor-mode' to change this variable.")
(make-variable-buffer-local 'senator-minor-mode)

(defconst senator-minor-mode-name "n"
  "Name shown in the mode line when senator minor mode is on.
Not displayed if the minor mode is globally enabled.")

(defconst senator-minor-mode-isearch-suffix "i"
  "String appended to the mode name when senator isearch mode is on.")

(defun senator-mode-line-update ()
  "Update the modeline to show the senator minor mode state.
If `senator-isearch-semantic-mode' is non-nil append
`senator-minor-mode-isearch-suffix' to the value of the variable
`senator-minor-mode-name'."
  (if (not (and senator-minor-mode senator-minor-mode-name))
      (setq senator-status "")
    (setq senator-status
          (format "%s%s" senator-minor-mode-name
                  (if senator-isearch-semantic-mode
                      (or senator-minor-mode-isearch-suffix "")
                    ""))))
  (semantic-mode-line-update))

(defun senator-minor-mode-setup ()
  "Actually setup the senator minor mode.
The minor mode can be turned on only if semantic feature is available
and the current buffer was set up for parsing.  When minor mode is
enabled parse the current buffer if needed.  Return non-nil if the
minor mode is enabled."
  (if senator-minor-mode
      (if (not (and (featurep 'semantic) (semantic-active-p)))
          (progn
            ;; Disable minor mode if semantic stuff not available
            (setq senator-minor-mode nil)
            (error "Buffer %s was not set up for parsing"
                   (buffer-name)))
        ;; XEmacs needs this
        (if (featurep 'xemacs)
            (easy-menu-add senator-minor-menu senator-mode-map))
        ;; Add completion hooks
        (semantic-make-local-hook
         'semantic-after-toplevel-cache-change-hook)
        (add-hook 'semantic-after-toplevel-cache-change-hook
                  'senator-completion-cache-flush-fcn nil t)
        (semantic-make-local-hook
         'semantic-after-partial-cache-change-hook)
        (add-hook 'semantic-after-partial-cache-change-hook
                  'senator-completion-cache-flush-fcn nil t))
    ;; XEmacs needs this
    (if (featurep 'xemacs)
        (easy-menu-remove senator-minor-menu))
    ;; Remove completion hooks
    (remove-hook 'semantic-after-toplevel-cache-change-hook
                 'senator-completion-cache-flush-fcn t)
    (remove-hook 'semantic-after-partial-cache-change-hook
                 'senator-completion-cache-flush-fcn t)
    ;; Disable semantic isearch
    (setq senator-isearch-semantic-mode nil))
  senator-minor-mode)

;;;###autoload
(defun senator-minor-mode (&optional arg)
  "Toggle senator minor mode.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{senator-mode-map}"
  (interactive
   (list (or current-prefix-arg
             (if senator-minor-mode 0 1))))
  (setq senator-minor-mode
        (if arg
            (>
             (prefix-numeric-value arg)
             0)
          (not senator-minor-mode)))
  (senator-minor-mode-setup)
  (run-hooks 'senator-minor-mode-hook)
  (if (interactive-p)
      (message "Senator minor mode %sabled"
               (if senator-minor-mode "en" "dis")))
  (senator-mode-line-update)
  senator-minor-mode)

(semantic-add-minor-mode 'senator-minor-mode
                         'senator-status
                         senator-mode-map)

;; To show senator isearch mode in the mode line
(semantic-add-minor-mode 'senator-isearch-semantic-mode
                         'senator-status
                         nil)
;;; Emacs 21 goodies
(and (not (featurep 'xemacs))
     (> emacs-major-version 20)
     (progn
       
       ;; Add Senator to the the minor mode menu in the mode line
       (define-key mode-line-mode-menu [senator-minor-mode]
         `(menu-item "Senator" senator-minor-mode
                     :button  (:toggle . senator-minor-mode)
                     :visible (and (featurep 'semantic)
                                   (semantic-active-p))))
       
       ))

;;;###autoload
(defun global-senator-minor-mode (&optional arg)
  "Toggle global use of senator minor mode.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (setq global-senator-minor-mode
        (semantic-toggle-minor-mode-globally
         'senator-minor-mode arg)))

;;;;
;;;; Useful advices
;;;;

(defun senator-beginning-of-defun (&optional arg)
  "Move backward to the beginning of a defun.
Use semantic tags to navigate.
ARG is the number of tags to navigate (not yet implemented)."
  (senator-parse)
  (let* ((senator-highlight-found nil)
         ;; Step at beginning of next tag with class specified in
         ;; `senator-step-at-tag-classes'.
         (senator-step-at-start-end-tag-classes t)
         (tag (senator-previous-tag)))
    (when tag
      (if (= (point) (semantic-tag-end tag))
          (goto-char (semantic-tag-start tag)))
      (beginning-of-line))
    (working-message nil)))

(defun senator-end-of-defun (&optional arg)
  "Move forward to next end of defun.
Use semantic tags to navigate.
ARG is the number of tags to navigate (not yet implemented)."
  (senator-parse)
  (let* ((senator-highlight-found nil)
         ;; Step at end of next tag with class specified in
         ;; `senator-step-at-tag-classes'.
         (senator-step-at-start-end-tag-classes t)
         (tag (senator-next-tag)))
    (when tag
      (if (= (point) (semantic-tag-start tag))
          (goto-char (semantic-tag-end tag)))
      (skip-chars-forward " \t")
      (if (looking-at "\\s<\\|\n")
          (forward-line 1)))
    (working-message nil)))

(defun senator-narrow-to-defun ()
  "Make text outside current defun invisible.
The defun visible is the one that contains point or follows point.
Use semantic tags to navigate."
  (interactive)
  (senator-parse)
  (save-excursion
    (widen)
    (senator-end-of-defun)
    (let ((end (point)))
      (senator-beginning-of-defun)
      (narrow-to-region (point) end))))

(defun senator-mark-defun ()
  "Put mark at end of this defun, point at beginning.
The defun marked is the one that contains point or follows point.
Use semantic tags to navigate."
  (interactive)
  (let ((origin (point))
        (end    (progn (senator-end-of-defun) (point)))
        (start  (progn (senator-beginning-of-defun) (point))))
    (goto-char origin)
    (push-mark (point))
    (goto-char end) ;; end-of-defun
    (push-mark (point) nil t)
    (goto-char start) ;; beginning-of-defun
    (re-search-backward "^\n" (- (point) 1) t)))

(defadvice beginning-of-defun (around senator activate)
  "Move backward to the beginning of a defun.
If semantic tags are available, use them to navigate."
  (if (and senator-minor-mode (interactive-p))
      (senator-beginning-of-defun (ad-get-arg 0))
    ad-do-it))

(defadvice end-of-defun (around senator activate)
  "Move forward to next end of defun.
If semantic tags are available, use them to navigate."
  (if (and senator-minor-mode (interactive-p))
      (senator-end-of-defun (ad-get-arg 0))
    ad-do-it))

(defadvice narrow-to-defun (around senator activate)
  "Make text outside current defun invisible.
The defun visible is the one that contains point or follows point.
If semantic tags are available, use them to navigate."
  (if (and senator-minor-mode (interactive-p))
      (senator-narrow-to-defun)
    ad-do-it))

(defadvice mark-defun (around senator activate)
  "Put mark at end of this defun, point at beginning.
The defun marked is the one that contains point or follows point.
If semantic tags are available, use them to navigate."
  (if (and senator-minor-mode (interactive-p))
      (senator-mark-defun)
    ad-do-it))

(defadvice c-mark-function (around senator activate)
  "Put mark at end of this defun, point at beginning.
The defun marked is the one that contains point or follows point.
If semantic tags are available, use them to navigate."
  (if (and senator-minor-mode (interactive-p))
      (senator-mark-defun)
    ad-do-it))

(defvar senator-add-log-tags '(function variable type)
  "When advising `add-log-current-defun', tag classes used.
Semantic tags that are of these classes will be used to find the name
used by add log.")
(semantic-varalias-obsolete 'senator-add-log-tokens
                            'senator-add-log-tags)

(defadvice add-log-current-defun (around senator activate)
  "Return name of function definition point is in, or nil."
  (if senator-minor-mode
      (let ((tag (semantic-current-tag))
            (name nil))
        (if (and tag (memq (semantic-tag-class tag)
			   senator-add-log-tags))
	    (progn
	      (setq name
		    (semantic-format-tag-canonical-name
		     tag (semantic-current-tag-parent)))
	      (setq ad-return-value name))
          ad-do-it))
    ad-do-it))

;;;;
;;;; Tag Cut & Paste
;;;;

;; To copy a tag, means to put a tag definition into the tag
;; ring.  To kill a tag, put the tag into the tag ring AND put
;; the body of the tag into the kill-ring.
;;
;; To retrieve a killed tag's text, use C-y (yank), but to retrieve
;; the tag as a reference of some sort, use senator-yank-tag.

(defvar senator-tag-ring (make-ring 20)
  "Ring of tags for use with cut and paste.")

(make-obsolete-overload 'semantic-insert-foreign-token
                        'semantic-insert-foreign-tag)

(semantic-alias-obsolete 'senator-insert-foreign-token
                         'semantic-insert-foreign-tag)

(defun senator-copy-tag ()
  "Take the current tag, and place it in the tag ring."
  (interactive)
  (senator-parse)
  (let ((ft (semantic-obtain-foreign-tag)))
    (when ft
      (ring-insert senator-tag-ring ft)
      (kill-ring-save (semantic-tag-start ft) (semantic-tag-end ft))
      (when (interactive-p)
	(message "Use C-y to yank text.  Use `senator-yank-tag' for prototype insert."))
      )
    ft))
(semantic-alias-obsolete 'senator-copy-token 'senator-copy-tag)

(defun senator-kill-tag ()
  "Take the current tag, place it in the tag ring, and kill it.
Killing the tag removes the text for that tag, and places it into
the kill ring.  Retrieve that text with \\[yank]."
  (interactive)
  (let ((ct (senator-copy-tag))) ;; this handles the reparse for us.
    (kill-region (semantic-tag-start ct)
                 (semantic-tag-end ct))
    (when (interactive-p)
      (message "Use C-y to yank text.  Use `senator-yank-tag' for prototype insert."))
    ))
(semantic-alias-obsolete 'senator-kill-token 'senator-kill-tag)

(defun senator-yank-tag ()
  "Yank a tag from the tag ring.
The form the tag takes is differnet depending on where it is being
yanked to."
  (interactive)
  (or (ring-empty-p senator-tag-ring)
      (let ((ft (ring-ref senator-tag-ring 0)))
          (semantic-foreign-tag-check ft)
          (semantic-insert-foreign-tag ft)
	  (when (interactive-p)
	    (message "Use C-y to recover the yank the text of %s."
		     (semantic-tag-name ft)))
	  )))
(semantic-alias-obsolete 'senator-yank-token 'senator-yank-tag)

(defun senator-copy-tag-to-register (register &optional kill-flag)
  "Copy the current tag into REGISTER.
Optional argument KILL-FLAG will delete the text of the tag to the
kill ring."
  (interactive "cTag to register: \nP")
  (senator-parse)
  (let ((ft (semantic-obtain-foreign-tag)))
    (when ft
      (set-register register ft)
      (if kill-flag
          (kill-region (semantic-tag-start ft)
                       (semantic-tag-end ft))))))
(semantic-alias-obsolete 'senator-copy-token-to-register
                         'senator-copy-tag-to-register)

(defadvice insert-register (around senator activate)
  "Insert contents of register REGISTER as a tag.
If senator is not active, use the original mechanism."
  (let ((val (get-register (ad-get-arg 0))))
    (if (and senator-minor-mode (interactive-p)
             (semantic-foreign-tag-p val))
        (semantic-insert-foreign-tag val)
      ad-do-it)))

(defadvice jump-to-register (around senator activate)
  "Insert contents of register REGISTER as a tag.
If senator is not active, use the original mechanism."
  (let ((val (get-register (ad-get-arg 0))))
    (if (and senator-minor-mode (interactive-p)
             (semantic-foreign-tag-p val))
        (progn
          (switch-to-buffer (semantic-tag-buffer val))
          (goto-char (semantic-tag-start val)))
      ad-do-it)))

(defun senator-transpose-tags-up ()
  "Transpose the current tag, and the preceeding tag."
  (interactive)
  (senator-parse)
  (let* ((current-tag (semantic-current-tag))
	 (prev-tag (save-excursion
		     (goto-char (semantic-tag-start current-tag))
		     (semantic-find-tag-by-overlay-prev)))
	 (ct-parent (semantic-find-tag-parent-by-overlay current-tag))
	 (pt-parent (semantic-find-tag-parent-by-overlay prev-tag)))
    (if (not (eq ct-parent pt-parent))
	(error "Cannot transpose tags"))
    (let ((txt (buffer-substring (semantic-tag-start current-tag)
				 (semantic-tag-end current-tag)))
	  (line (count-lines (semantic-tag-start current-tag)
			     (point)))
	  (insert-point nil)
	  )
      (delete-region (semantic-tag-start current-tag)
		     (semantic-tag-end current-tag))
      (delete-blank-lines)
      (goto-char (semantic-tag-start prev-tag))
      (setq insert-point (point))
      (insert txt)
      (if (/= (current-column) 0)
	  (insert "\n"))
      (insert "\n")
      (goto-char insert-point)
      (forward-line line)
      )))

(defun senator-transpose-tags-down ()
  "Transpose the current tag, and the following tag."
  (interactive)
  (senator-parse)
  (let* ((current-tag (semantic-current-tag))
	 (next-tag (save-excursion
		     (goto-char (semantic-tag-end current-tag))
		     (semantic-find-tag-by-overlay-next)))
	 (end-pt (point-marker))
	 )
    (goto-char (semantic-tag-start next-tag))
    (forward-char 1)
    (senator-transpose-tags-up)
    ;; I know that the above fcn deletes the next tag, so our pt marker
    ;; will be stable.
    (goto-char end-pt)))

;;; HIPPIE EXPAND
;;
;; Senator has a nice completion mechanism.  Use it to add a new
;; hippie expand try method.

(eval-when-compile (require 'hippie-exp))

(defvar senator-try-function-already-enabled nil
  "Non-nil if `hippie-expand' semantic completion was already enabled.
This flag remember `senator-hippie-expand-hook' to not remove
`senator-try-expand-semantic' from `hippie-expand-try-functions-list'
if it was previously put here by any sort of user's customization.")

(defun senator-hippie-expand-hook ()
  "Enable or disable use of semantic completion with `hippie-expand'.
Depending on the value of the variable `senator-minor-mode'.
Run as `senator-minor-mode-hook'."
  (make-local-variable 'hippie-expand-try-functions-list)
  (make-local-variable 'senator-try-function-already-enabled)
  (if senator-minor-mode
      (progn
        ;; Does nothing if semantic completion is already enabled (via
        ;; customization for example).
        (setq senator-try-function-already-enabled
              (memq 'senator-try-expand-semantic
                    hippie-expand-try-functions-list))
        (or senator-try-function-already-enabled
            (setq hippie-expand-try-functions-list
                  (cons 'senator-try-expand-semantic
                        hippie-expand-try-functions-list))))
    ;; Does nothing if semantic completion wasn't enabled here.
    (or senator-try-function-already-enabled
        (setq hippie-expand-try-functions-list
              (delq 'senator-try-expand-semantic
                    hippie-expand-try-functions-list)))))

(add-hook 'senator-minor-mode-hook 'senator-hippie-expand-hook)

;;;###autoload
(defun senator-try-expand-semantic (old)
  "Attempt inline completion at the cursor.
Use Semantic, or the semantic database to look up possible
completions.  The argument OLD has to be nil the first call of this
function.  It returns t if a unique, possibly partial, completion is
found, nil otherwise."

  ;;@TODO - Can I support smart completion in here?

  (if (semantic-active-p)
      (let (symstart)
        ;; If the hippie says so, start over.
        (if (not old)
            (if (setq symstart (senator-current-symbol-start))
                (progn
                  (he-init-string symstart (point))
                  (setq senator-last-completion-stats nil))))
        ;; do completion with senator's mechanism.
        (when (or old symstart)
	  ;; This bit will turn off parsing on lexical errors.
	  (semantic-lex-catch-errors senator-hippie-expand
	    (semantic-fetch-tags))
	  ;; Do the completion
	  (let ((ret (senator-complete-symbol t)))
	    (cond (ret
		   ;; Found a new completion, update the end marker.
		   (set-marker he-string-end (point))
		   ;; Update the tried table so other hippie expand
		   ;; try functions can see whether an expansion has
		   ;; already been tried.
		   (setq he-tried-table (cons ret he-tried-table)))
		  ;; No more completion
		  (old
		   ;; Reset the initial completed string for other
		   ;; hippie-expand try functions.
		   (he-reset-string)))
	    ret)))))

;;;;
;;;; Using semantic search in isearch mode
;;;;

;;; Compatibility
(cond
 ( ;; GNU Emacs 21.0 lazy highlighting
  (fboundp 'isearch-lazy-highlight-cleanup)
  
  ;; Provide this function used by senator
  (defun senator-lazy-highlight-update ()
    "Force lazy highlight update."
    (funcall 'isearch-lazy-highlight-cleanup t)
    (set 'isearch-lazy-highlight-last-string nil)
    (setq isearch-adjusted t)
    (isearch-update))
  
  ) ;; End of GNU Emacs 21 lazy highlighting
 
 ( ;; XEmacs 21.4 lazy highlighting
  (fboundp 'isearch-highlight-all-cleanup)
  
  ;; Provide this function used by senator
  (defun senator-lazy-highlight-update ()
    "Force lazy highlight update."
    (funcall 'isearch-highlight-all-cleanup)
    (set 'isearch-highlight-last-string nil)
    (setq isearch-adjusted t)
    (isearch-update))
  
  ) ;; End of XEmacs 21.4 lazy highlighting
 
 ( ;; GNU Emacs 20 lazy highlighting via ishl
  (fboundp 'ishl-cleanup)
  
  ;; Provide this function used by senator
  (defun senator-lazy-highlight-update ()
    "Force lazy highlight update."
    (funcall 'ishl-cleanup t)
    (set 'ishl-last-string nil)
    (setq isearch-adjusted t)
    (isearch-update))
  
  ) ;; End of GNU Emacs 20 lazy highlighting
 
 (t ;; No lazy highlighting
  
  ;; Ignore this function used by senator
  (defalias 'senator-lazy-highlight-update 'ignore)
  
  ))

(defmacro senator-define-search-advice (searcher)
  "Advice the built-in SEARCHER function to do semantic search.
That is to call the Senator counterpart searcher when variables
`isearch-mode' and `senator-isearch-semantic-mode' are non-nil."
  (let ((senator-searcher (intern (format "senator-%s" searcher))))
    `(defadvice ,searcher (around senator activate)
       (if (and isearch-mode senator-isearch-semantic-mode
                ;; The following condition ensure to do a senator
                ;; semantic search on the `isearch-string' only!
                (string-equal (ad-get-arg 0) isearch-string))
           (unwind-protect
               (progn
                 ;; Temporarily set `senator-isearch-semantic-mode' to
                 ;; nil to avoid an infinite recursive call of the
                 ;; senator semantic search function!
                 (setq senator-isearch-semantic-mode nil)
                 (setq ad-return-value
                       (funcall ',senator-searcher
                                (ad-get-arg 0)     ; string
                                (ad-get-arg 1)     ; bound
                                (ad-get-arg 2)     ; no-error
                                (ad-get-arg 3)     ; count
                                )))
             (setq senator-isearch-semantic-mode t))
         ad-do-it))))

;; Recent versions of GNU Emacs allow to override the isearch search
;; function for special needs, and avoid to advice the built-in search
;; function :-)
(defun senator-isearch-search-fun ()
  "Return the function to use for the search.
Use a senator search function when semantic isearch mode is enabled."
  (intern
   (concat (if senator-isearch-semantic-mode
               "senator-"
             "")
           (cond (isearch-word "word-")
                 (isearch-regexp "re-")
                 (t ""))
           "search-"
           (if isearch-forward
               "forward"
             "backward"))))

(unless (boundp 'isearch-search-fun-function)
  ;; Advice the built-in search functions to do semantic search when
  ;; `isearch-mode' and `senator-isearch-semantic-mode' are on.
  (senator-define-search-advice search-forward)
  (senator-define-search-advice re-search-forward)
  (senator-define-search-advice word-search-forward)
  (senator-define-search-advice search-backward)
  (senator-define-search-advice re-search-backward)
  (senator-define-search-advice word-search-backward)
  )
;;; End of compatibility stuff

(defun senator-isearch-toggle-semantic-mode ()
  "Toggle semantic searching on or off in isearch mode.
\\<senator-mode-map>\\[senator-isearch-toggle-semantic-mode] toggle semantic searching."
  (interactive)
  (when senator-minor-mode
    (setq senator-isearch-semantic-mode
          (not senator-isearch-semantic-mode))
    (senator-mode-line-update)
    (if isearch-mode
        ;; force lazy highlight update
        (senator-lazy-highlight-update)
      (working-message "Isearch semantic mode %s"
                       (if senator-isearch-semantic-mode
                           "enabled"
                         "disabled")))))

;; Needed by XEmacs isearch to not terminate isearch mode when
;; toggling semantic search.
(put 'senator-isearch-toggle-semantic-mode 'isearch-command t)

;; Keyboard shortcut to toggle semantic search in isearch mode.
(define-key isearch-mode-map
  [(control ?,)]
  'senator-isearch-toggle-semantic-mode)

(defvar senator-old-isearch-search-fun nil
  "Hold previous value of `isearch-search-fun-function'.")

(defun senator-isearch-mode-hook ()
  "Isearch mode hook to setup semantic searching."
  (or senator-minor-mode
      (setq senator-isearch-semantic-mode nil))
  (when (boundp 'isearch-search-fun-function)
    (if (and isearch-mode senator-isearch-semantic-mode)
        (progn
          ;; When `senator-isearch-semantic-mode' is on save the
          ;; previous `isearch-search-fun-function' and install the
          ;; senator one.
          (when (and (local-variable-p 'isearch-search-fun-function)
                     (not (local-variable-p 'senator-old-isearch-search-fun)))
            (set (make-local-variable 'senator-old-isearch-search-fun)
                 isearch-search-fun-function))
          (set (make-local-variable 'isearch-search-fun-function)
               'senator-isearch-search-fun))
      ;; When `senator-isearch-semantic-mode' is off restore the
      ;; previous `isearch-search-fun-function'.
      (when (eq isearch-search-fun-function 'senator-isearch-search-fun)
        (if (local-variable-p 'senator-old-isearch-search-fun)
            (progn
              (set (make-local-variable 'isearch-search-fun-function)
                   senator-old-isearch-search-fun)
              (kill-local-variable 'senator-old-isearch-search-fun))
          (kill-local-variable 'isearch-search-fun-function)))))
  (senator-mode-line-update))


(add-hook 'isearch-mode-hook     'senator-isearch-mode-hook)
(add-hook 'isearch-mode-end-hook 'senator-isearch-mode-hook)

(provide 'senator)

;;; senator.el ends here
