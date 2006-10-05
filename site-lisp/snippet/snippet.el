;;; snippet.el -- insert snippets of text into a buffer

;; Copyright (C) 2005 Pete Kazmier

;; Version: 0.2
;; Author: Pete Kazmier

;; This file is not part of GNU Emacs, but it is distributed under
;; the same terms as GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Description:

;; A quick stab at providing a simple template facility like the one
;; present in TextMate (an OSX editor).  The general idea is that a
;; snippet of text (called a template) is inserted into a buffer
;; (perhaps triggered by an abbrev), and while the point is within the
;; snippet, a special keymap is active to permit the user to cycle the
;; point to any of the defined fields (placeholders) within the
;; template via `snippet-next-field' and `snippet-prev-field'.

;; For example, the following template might be a useful while editing
;; HTML:

;;   <a href="$$">$$</a>

;; This template might be useful for python developers.  In this
;; example, reasonable defaults have been supplied:

;;   for $${element} in $${sequence}:
;;       match = $${regexp}.search($${element})

;; When a template is inserted into a buffer (could be triggered by an
;; abbrev expansion, or simply bound to some key), point is moved to
;; the first field denoted by the "$$" characters (configurable via
;; `snippet-field-identifier').  The optional default for a field is
;; specified by the "{default}" (the delimiters are configurable via
;; `snippet-field-default-beg-char' and `snippet-field-defaul-end-char'.

;; If present, the default will be inserted and highlighted.  The user
;; then has the option of accepting the default by simply tabbing over
;; to the next field (any other key bound to `snippet-next-field' in
;; `snippet-map' can be used).  Alternatively, the user can start
;; typing their own value for the field which will cause the default
;; to be immediately replaced with the user's own input.  If two or
;; more fields have the same default value, they are linked together
;; (changing one will change the other dynamically as you type).

;; `snippet-next-field' (bound to <tab> by default) moves the point to
;; the next field.  `snippet-prev-field' (bound to <S-tab> by default)
;; moves the point to the previous field.  When the snippet has been
;; completed, the user simply tabs past the last field which causes
;; the snippet to revert to plain text in a buffer.  The idea is that
;; snippets should get out of a user's way as soon as they have been
;; filled and completed.

;; After tabbing past all of the fields, point is moved to the end of
;; the snippet, unless the user has specified a place within the
;; template with the `snippet-exit-identifier' ("$." by default).  For
;; example: 

;;   if ($${test} {
;;       $.
;;   }

;; Indentation can be controlled on a per line basis by including the
;; `snippet-indent' string within the template.  Most often one would
;; include this at the beginning of a line; however, there are times
;; when indentation is better performed in other parts of the line.
;; The following shows how to use the functionality:

;;   if ($${test}) {
;;   $>this line would be indented
;;   this line will be indented after being inserted$>
;;   }

;;; Usage:

;; Snippets are inserted with the `snippet-insert' function.  This
;; function inserts the snippet into the current buffer.  It expects a
;; single argument which is the template that is to be inserted.  For
;; example:

;;   (snippet-insert "for $${element} in $${sequence}:")

;; `snippet-insert' can be called interactively in which case the user
;; is prompted for the template to insert.  This is hardly useful at
;; all unless you are testing the functionality of this code.

;; Snippets are much more effective when they are bound to expansions
;; for abbreviations.  When binding a snippet to an abbreviation, it
;; is important that you disable the insertion of the character that
;; triggered the expansion (typically some form of whitespace).  For
;; example, this is what you should NOT do:

;;   (define-abbrev python-mode-abbrev-table  ; abbrev table
;;                  "for"                     ; name
;;                  ""                        ; expansion
;;                  '(lambda ()               ; expansion hook
;;                     (snippet-insert 
;;                      "for $${element} in $${sequence}:")))

;; The above example does not work as expected because after the
;; expansion hook is called, the snippet is inserted, and the point is
;; moved to the first field.  The problem occurs because when the user
;; typed "f o r <Spc>", the "<Spc>" character is inserted after the
;; snippet has been inserted.  The point happens to be located at the
;; first field and thus the "<Spc>" will delete any field default that
;; was present.

;; Fortunately, this is easy to fix.  According to the documentation
;; for `define-abbrev', if the hook function is a symbol whose
;; `no-self-insert' property is non-nil, then hook can control whether
;; or not the insertion of the character that triggered the abbrev
;; expansion is inserted.  `insert-snippet' returns non-nil and thus
;; the proper way of defining the abbrev is as follows:

;;   (defun python-foo-expansion ()
;;     (snippet-insert "for $${element} in $${sequence}:"))

;;   (put 'python-foo-expansion 'no-self-insert t)

;;   (define-abbrev python-mode-abbrev-table    ; abbrev table
;;                  "for"                       ; name
;;                  ""                          ; expansion
;;                  'python-foo-expansion)      ; expansion hook

;; Unfortunately, this is a lot of work to define what should be a
;; simple abbrev.  For convenience, this package provides a macro
;; `snippet-abbrev' that can be used with much less effort:

;;   (snippet-abbrev 'python-mode-abbrev-table            ; table
;;                   "for"                               ; name
;;                   "for $${element} in $${sequence}:") ; template

;; For even more convevience, when defining a lot of abbrevs in a
;; particular abbrev table, the package provides another macro
;; `snippet-with-abbrev-table':

;;   (snippet-with-abbrev-table 'python-mode-abbrev-table
;;     ("for" .  "for $${element} in $${sequence}:")
;;     ("im"  .  "import $$")
;;     ("if"  .  "if $${True}:")
;;     ("wh"  .  "while $${True}:"))

;; Be sure that the appropriate abbrev-table is loaded before using
;; the above otherwise you'll get an error.  I use the above in my
;; python-mode-hook.

;;; Implementation Notes:

;; This is my first significant chunk of elisp code.  I have very
;; little experience coding with elisp; however, I have tried to
;; document the code for anyone trying to follow along.  Here are some
;; brief notes on the implementation.

;; When a snippet is inserted, the entire template of text has an
;; overlay applied.  This overlay is referred to as the "bound"
;; overlay in the code.  It is used to bold-face the snippet as well
;; as provide the keymap that is used while the point is located
;; within the snippet (so users can tab between fields).  This overlay
;; is actually one character longer than the template.  The reason is
;; to make sure that our local keymap is still in effect when a user
;; is typing in a field that happens to be at the end of the
;; template.

;; In addition, for each field (denoted by snippet-field-identifier),
;; an overlay is created.  These overlays are used to provide the
;; highlighting of the field values, the location of where the point
;; should move when tab is pressed (the start of the overlay is used
;; for this purpose), as well as the hooks to delete the default value
;; if a user starts to type their own value (the modification hooks of
;; the overlay are used for this purpose).

;; Once the user has tabbed out of the snippet, all overlays are
;; deleted and the snippet then becomes normal text.  Moving the
;; cursor back into the snippet has no affect (the snippet is not
;; activated again).  The idea is that the snippet concept should get
;; out of the users way as quickly as possible.

;;; Comparisons to Other Packages

;; tempo.el
;;  - Template definition is very lispy (although powerful).  In
;;    contrast, snippets are simple strings with minimal syntax.
;;  - Template parameters can be prompted via minibuffer.  In
;;    contrast, snippets use overlays to visually cue the user for
;;    parameters.
;;  + Templates can be wrapped around regions of text.
;;

;;; Known Limitations:

;; - When one uses something like `dabbrev-expand', when the text is
;;   inserted, it blows away a lot of the snippet.  Not sure why yet.
;; - Using 'indent-according-to-mode' does not seem to behave well
;;   with Python mode.  I have no idea why, the overlays end up
;;   getting shifted around incorrectly.

;;; Code:

(require 'cl)

(defgroup snippet nil 
  "Insert a template with fields that con contain optional defaults."
  :prefix "snippet-"
  :group 'abbrev
  :group 'convenience)

(defcustom snippet-bound-face 'bold
  "*Face used for the body of the snippet."
  :type 'face
  :group 'snippet)

(defcustom snippet-field-face 'highlight
  "*Face used for the fields' default values."
  :type 'face
  :group 'snippet)

(defcustom snippet-field-identifier "$$"
  "*String used to identify field placeholders."
  :type 'string
  :group 'snippet)

(defcustom snippet-exit-identifier "$."
  "*String used to identify the exit point of the snippet."
  :type 'string
  :group 'snippet)

(defcustom snippet-field-default-beg-char ?{
  "*Character used to identify the start of a field's default value."
  :type 'character
  :group 'snippet)

(defcustom snippet-field-default-end-char ?}
  "*Character used to identify the stop of a field's default value."
  :type 'character
  :group 'snippet)

(defcustom snippet-indent "$>"
  "*String used to indicate that a line is to be indented."
  :type 'character
  :group 'snippet)

(defcustom snippet-line-terminator "\n"
  "*String used to indicate the end of line in a snippet template."
  :type 'string
  :group 'snippet)

(defvar snippet-map (make-sparse-keymap)
  "Keymap used while the point is located within a snippet.")

;; Default key bindings
(define-key snippet-map (kbd "TAB")             'snippet-next-field)
(define-key snippet-map (kbd "<S-tab>")         'snippet-prev-field)
(define-key snippet-map (kbd "<S-iso-lefttab>") 'snippet-prev-field)

(defstruct snippet 
  "Structure containing the overlays used to display a snippet.

The BOUND slot contains an overlay to bound the entire text of the
template.  This overlay is used to provide a different face
configurable via `snippet-bound-face' as well as the keymap that
enables tabbing between fields.

The FIELDS slot contains a list of overlays used to indicate the
position of each field.  In addition, if a field has a default, the
field overlay is used to provide a different face configurable via
`snippet-field-face'.

The EXIT-MARKER slot contains a marker where point should be placed
after the user has cycled through all available fields."
  bound fields exit-marker)

(defvar snippet nil
  "Snippet in the current buffer.
There is no more than one snippet per buffer.  This variable is buffer
local.")

(make-variable-buffer-local 'snippet)

(defun snippet-make-bound-overlay ()
  "Create an overlay to bound a snippet.
Add the appropriate properties for the overlay to provide: a face used
to display the snippet, the keymap to use while within the snippet,
and the modification hooks to clean up the overlay in the event it is
deleted."
  (let ((bound (make-overlay (point) (point) (current-buffer) nil nil)))
    (overlay-put bound 'keymap snippet-map)
    (overlay-put bound 'face snippet-bound-face)
    (overlay-put bound 'modification-hooks '(snippet-bound-modified))
    bound))

(defun snippet-make-field-overlay (&optional name)
  "Create an overlay for a field in a snippet.  
Add the appropriate properties for the overlay to provide: a face used
to display a field's default value, and modification hooks to remove
the default text if the user starts typing."
  (let ((field (make-overlay (point) (point) (current-buffer) nil t)))
    (overlay-put field 'face snippet-field-face)
    (overlay-put field 'insert-in-front-hooks '(snippet-field-insert
                                                snippet-field-update))
    (overlay-put field 'insert-behind-hooks '(snippet-field-modified
                                              snippet-field-update))
    (overlay-put field 'modification-hooks '(snippet-field-modified
                                             snippet-field-update))
    (overlay-put field 'name (when name (intern name)))
    field))

(defun snippet-fields-with-name (name)
  "Return a list of fields whose name property is equal to NAME."
  (loop for field in (snippet-fields snippet) 
        when (eq name (overlay-get field 'name))
        collect field))

(defun snippet-bound-modified (bound after beg end &optional change)
  "Ensure the overlay that bounds a snippet is cleaned up.
This modification hook is triggered when the overlay that bounds the
snippet is modified.  It runs after the change has been made and
ensures that if the snippet has been deleted by the user, the
appropriate cleanup occurs."
  (when (and after (> 2 (- (overlay-end bound) (overlay-start bound))))
    (snippet-cleanup)))

(defun snippet-field-insert (field after beg end &optional change)
  "Delete the default field value.
This insertion hook is triggered when a user starts to type when the
point is positioned at the beginning of a field (this occurs when the
user chooses to replace the field default).  In this case, the hook
deletes the field default."
  (let ((inhibit-modification-hooks t))
    (when (not after)
      (delete-region (overlay-start field) (overlay-end field)))))

(defun snippet-field-modified (field after beg end &optional change)
  "Shrink the field overlay.
This modification hook is triggered when a user starts to type when
the point is positioned in the middle or at the end of a field (this
occurs when the user chooses to edit the field default).  It is used
to ensure that the bound overlay always covers the entirety of all
field overlays, if not, its extends the bound overlay appropriately."
  (let ((bound (snippet-bound snippet)))
    (when (and after bound (> (overlay-end field) (overlay-end bound)))
      (move-overlay bound (overlay-start bound) (overlay-end field)))))

(defun snippet-field-update (field after beg end &optional change)
  "Update all fields that have the same name.
This modificition hook is triggered when a user edits any field and is
responsible for updating all other fields that share a common name."
  (let ((name (overlay-get field 'name))
        (value (buffer-substring (overlay-start field) (overlay-end field)))
        (inhibit-modification-hooks t))
    (when (and name after)
      (save-excursion
        (dolist (like-field (set-difference (snippet-fields-with-name name) 
                                            (list field)))
          (goto-char (overlay-start like-field))
          (delete-region (overlay-start like-field)
                         (overlay-end like-field))
          (insert value))))))

(defun snippet-exit-snippet ()
  "Move point to `snippet-exit-identifier' or end of bound.
If the snippet has defined `snippet-exit-identifier' in the template,
move the point to that location.  Otherwise, move it to the end of the
snippet."
  (goto-char (snippet-exit-marker snippet))
  (snippet-cleanup))

(defun snippet-next-field ()
  "Move point forward to the next field in the `snippet'.
If there are no more fields in the snippet, point is moved to the end
of the snippet or the location specified by `snippet-exit-identifier',
and the snippet reverts to normal text."
  (interactive)
  (let* ((bound (snippet-bound snippet))
         (fields (snippet-fields snippet))
         (exit (snippet-exit-marker snippet))
         (next-pos (loop for field in fields
                         for start = (overlay-start field)
                         when (< (point) start) return start)))
    (if (not (null next-pos))
        (goto-char next-pos)
      (goto-char exit)
      (snippet-cleanup))))

(defun snippet-prev-field ()
  "Move point backward to the previous field in the `snippet'.
If there are no more fields in the snippet, point is moved to the end
of the snippet or the location specified by `snippet-exit-identifier',
and the snippet reverts to normal text."
  (interactive)
  (let* ((bound (snippet-bound snippet))
         (fields (snippet-fields snippet))
         (exit (snippet-exit-marker snippet))         
         (prev-pos (loop for field in (reverse fields)
                         for start = (overlay-start field)
                         when (> (point) start) return start)))
    (if (not (null prev-pos))
        (goto-char prev-pos)
      (goto-char exit)
      (snippet-cleanup))))

(defun snippet-cleanup ()
  "Delete all overlays associated with `snippet'.
This effectively reverts the snippet to normal text in the buffer."
  (when snippet
    (when (snippet-bound snippet)
      (delete-overlay (snippet-bound snippet)))
    (dolist (field (snippet-fields snippet))
      (delete-overlay field))
    (setq snippet nil)))

(defun snippet-field-regexp ()
  "Return a regexp that is used to search for fields within a template."
  (let ((beg (char-to-string snippet-field-default-beg-char))
        (end (char-to-string snippet-field-default-end-char)))
    (concat (regexp-quote snippet-field-identifier)
            "\\("
            (regexp-quote beg)
            "\\([^"
            (regexp-quote end)
            "]+\\)"
            (regexp-quote end)
            "\\)?")))

(defun snippet-split-string (string &optional separators include-separators-p)
  "Split STRING into substrings and separators at SEPARATORS.
Return a list of substrings and optional include the separators in the
list if INCLUDE-SEPARATORS-P is non-nil."
  (let ((start 0) (list '()))
    (while (string-match (or separators snippet-line-terminator) string start)
      (when (< start (match-beginning 0))
        (push (substring string start (match-beginning 0)) list))
      (when include-separators-p
        (push (substring string (match-beginning 0) (match-end 0)) list))
      (setq start (match-end 0)))
    (when (< start (length string))
      (push (substring string start) list))
    (nreverse list)))

(defun snippet-split-regexp ()
  "Return a regexp to split the template into component parts."
  (concat (regexp-quote snippet-line-terminator)
          "\\|"
          (regexp-quote snippet-indent)))

(defun snippet-insert (template)
  "Insert a snippet into the current buffer at point.  
TEMPLATE is a string that may optionally contain fields which are
specified by `snippet-field-identifier'.  Fields may optionally also
include default values delimited by `snippet-field-default-beg-char'
and `snippet-field-default-end-char'.

For example, the following template specifies two fields which have
the default values of \"element\" and \"sequence\":

  \"for $${element} in $${sequence}:\"

In the next example, only one field is specified and no default has
been provided:

  \"import $$\"

This function may be called interactively, in which case, the TEMPLATE
is prompted for.  However, users do not typically invoke this function
interactively, rather it is most often called as part of an abbrev
expansion.  See `snippet-abbrev' and `snippet-with-abbrev-table' for
more information."
  (interactive "sSnippet template: ")

  ;; Step 1: Ensure only one snippet exists at a time
  (snippet-cleanup)

  ;; Step 2: Create a new snippet and add the overlay to bound the
  ;; template body.  It should be noted that the bounded overlay is
  ;; sized to be one character larger than the template body text.
  ;; This enables our keymap to be active when a field happens to be
  ;; the last item in a template.  We disable abbrev mode to prevent
  ;; our template from triggering another abbrev expansion (I do not
  ;; know if the use of `insert' will actually trigger abbrevs).
  (let ((abbrev-mode nil))
    (setq snippet (make-snippet :bound (snippet-make-bound-overlay)))
    (let ((start (point))
          (count 0))
      (dolist (line (snippet-split-string template (snippet-split-regexp) t))
        (cond ((string-equal snippet-line-terminator line)
               (insert "\n"))
              ((string-equal snippet-indent line)
               (indent-according-to-mode))
              (t
               (insert line))))
      (move-overlay (snippet-bound snippet) start (1+ (point))))


    ;; Step 3: Insert the exit marker so we know where to move point
    ;; to when user is done with snippet.  If they did not specify
    ;; where point should land, set the exit marker to the end of the
    ;; snippet. 
    (goto-char (overlay-start (snippet-bound snippet)))
    (while (re-search-forward (regexp-quote snippet-exit-identifier)
                              (overlay-end (snippet-bound snippet)) 
                              t)
      (replace-match "")
      (setf (snippet-exit-marker snippet) (point-marker)))
    
    (unless (snippet-exit-marker snippet)
      (let ((end (overlay-end (snippet-bound snippet))))
        (goto-char (if (= end (point-max)) end (1- end))))
      (setf (snippet-exit-marker snippet) (point-marker)))
  
    (set-marker-insertion-type (snippet-exit-marker snippet) t)

    ;; Step 4: Create field overlays for each field and insert any
    ;; default values for the field.
    (goto-char (overlay-start (snippet-bound snippet)))
    (while (re-search-forward (snippet-field-regexp)
                              (overlay-end (snippet-bound snippet)) 
                              t)
      (let ((field (snippet-make-field-overlay (match-string 2)))
            (start (match-beginning 0)))
        (push field (snippet-fields snippet))
        (replace-match (if (match-beginning 2) "\\2" ""))
        (move-overlay field start (point))))
    
    ;; These are reversed so they are in order of how they appeared in
    ;; the template as we index into this list when cycling field to
    ;; field. 
    (setf (snippet-fields snippet) (reverse (snippet-fields snippet))))

  ;; Step 5: Position the point at the first field or the end of the
  ;; template body if no fields are present.  We need to take into
  ;; consideration the special case where the first field is at the
  ;; start of the snippet (otherwise the call to snippet-next-field
  ;; will go past it).
  (let ((bound (snippet-bound snippet))
        (first (car (snippet-fields snippet))))
    (if (and first (= (overlay-start bound) (overlay-start first)))
        (goto-char (overlay-start first))
      (goto-char (overlay-start (snippet-bound snippet)))
      (snippet-next-field))))

(defun snippet-strip-abbrev-table-suffix (str)
  "Strip a suffix of \"-abbrev-table\" if one is present."
  (if (string-match "^\\(.*\\)-abbrev-table$" str)
      (match-string 1 str)
      str))

(defun snippet-make-abbrev-expansion-hook (abbrev-table abbrev-name template)
  "Define a function with the `no-self-insert' property set non-nil.
The function name is composed of \"snippet-abbrev-\", the abbrev table
name, and the name of the abbrev.  If the abbrev table name ends in
\"-abbrev-table\", it is stripped."
  (let ((abbrev-expansion (intern
                           (concat "snippet-abbrev-" 
                                   (snippet-strip-abbrev-table-suffix
                                    (symbol-name abbrev-table))
                                   "-"
                                   abbrev-name))))
    (fset abbrev-expansion 
          `(lambda ()
             ,(format (concat "Abbrev expansion hook for \"%s\".\n"
                              "Expands to the following snippet:\n\n%s")
                      abbrev-name
                      template)
             (snippet-insert ,template)))
    (put abbrev-expansion 'no-self-insert t)
    abbrev-expansion))

(defmacro snippet-abbrev (abbrev-table abbrev-name template)
  "Establish an abbrev for a snippet template.
Set up an abbreviation called ABBREV-NAME in the ABBREV-TABLE (note
that ABBREV-TABLE must be quoted) that expands into a snippet using
the specified TEMPLATE string.

This macro facilitates the creation of a function for the expansion
hook to be used in `define-abbrev'.  In addition, it also sets the
`no-self-insert' property on the function to prevent `abbrev-mode'
from inserting the character that triggered the expansion (typically
whitespace) which would otherwise interfere with the first field of a
snippet."
  (let ((name (gensym))
        (table (gensym)))
    `(let ((,name ,abbrev-name)
           (,table ,abbrev-table))
       (define-abbrev (symbol-value ,table) ,name ""
         (snippet-make-abbrev-expansion-hook ,table ,name ,template)))))

(defmacro snippet-with-abbrev-table (abbrev-table &rest snippet-alist)
  "Establish a set of abbrevs for snippet templates.
Set up a series of snippet abbreviations in the ABBREV-TABLE (note
that ABBREV-TABLE must be quoted.  The abbrevs are specified in
SNIPPET-ALIST.  For example:

  (snippet-with-abbrev-table 'python-mode-abbrev-table
    (\"for\" . \"for $${element} in $${sequence}:\")
    (\"im\"  . \"import $$\"))

See also `snippet-abbrev."
  (let ((table (gensym)))
    `(let ((,table ,abbrev-table))
       (progn
         ,@(loop for (name . template) in snippet-alist
              collect (list 'snippet-abbrev table name template))))))

(provide 'snippet)
