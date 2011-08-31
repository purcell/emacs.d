;;; ecb-method-browser.el --- the method-browser of Emacs

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2000

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id: ecb-method-browser.el,v 1.94 2009/05/08 14:05:55 berndl Exp $

;;; Commentary:

;; This file contains the code for the method-browser of ECB


(require 'tree-buffer)
(require 'ecb-util)
(require 'ecb-layout)
(require 'ecb-mode-line)
(require 'ecb-navigate)
(require 'ecb-face)
(require 'ecb-speedbar)
(require 'ecb-common-browser)

(require 'ecb-cedet-wrapper)
;; This loads the semantic-setups for the major-modes.
(require 'semantic-load)

;; various loads
(require 'assoc)

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

(eval-when-compile
  (require 'silentcomp))

(silentcomp-defun hs-minor-mode)
(silentcomp-defun hs-show-block)
(silentcomp-defun hs-hide-block)
(silentcomp-defvar hs-minor-mode)
(silentcomp-defvar hs-block-start-regexp)
(silentcomp-defvar imenu--index-alist)

(silentcomp-defvar semantic-idle-scheduler-mode)

(silentcomp-defun ecb-get-tags-for-non-semantic-files)
(silentcomp-defun ecb-create-non-semantic-tree)

(defvar ecb-selected-tag nil
  "The currently selected Semantic tag.")
(make-variable-buffer-local 'ecb-selected-tag)

(defvar ecb-methods-root-node nil
  "Path to currently selected source.")

(defconst ecb-methods-nodetype-tag 0)
(defconst ecb-methods-nodetype-bucket 1)
(defconst ecb-methods-nodetype-externtag 2)

(defun ecb-method-browser-initialize-caches ()
  "Initialize the caches of the method-browser of ECB."
  (ecb-clear-tag-tree-cache))

(defun ecb-method-browser-initialize (&optional no-caches)
  "Initialize the method-browser of ECB. If optional arg NO-CACHES is not nil
then the caches used by the method-browser will not be initialized."
  (setq ecb-selected-tag nil)
  (setq ecb-methods-root-node nil)
  (setq ecb-methods-user-filter-alist nil)
  (setq ecb-current-post-processed-tag-table nil)
  (unless no-caches
    (ecb-method-browser-initialize-caches)))

;;====================================================
;; Customization
;;====================================================

(defgroup ecb-methods nil
  "Settings for the methods-buffer in the Emacs code browser."
  :group 'ecb
  :prefix "ecb-")


(defgroup ecb-non-semantic nil
  "Settings for parsing and displaying non-semantic files."
  :group 'ecb
  :prefix "ecb-")


(defcustom ecb-methods-buffer-name " *ECB Methods*"
  "*Name of the ECB methods buffer.
Because it is not a normal buffer for editing you should enclose the name with
stars, e.g. \"*ECB Methods*\".

If it is necessary for you you can get emacs-lisp access to the buffer-object of
the ECB-methods-buffer by this name, e.g. by a call of `set-buffer'.

Changes for this option at runtime will take affect only after deactivating and
then activating ECB again!"
  :group 'ecb-methods
  :type 'string)


(defcustom ecb-auto-expand-tag-tree 'expand-spec
  "*Expand the methods-tag-tree automatically if node invisible.
This option has only an effect if option `ecb-highlight-tag-with-point' is
switched on too. There are three possible choices:
- nil: No auto. expanding of the method buffer.
- expand-spec: Auto expand the method-buffer nodes if the node belonging to
  current tag under point is invisible because its parent-node is collapsed.
  But expanding is only done if the type of the tag under point in the
  edit-buffer is contained in `ecb-methods-nodes-expand-spec'.
- all: Like expand-spec but expands all tags regardless of the setting in
  `ecb-methods-nodes-expand-spec'.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :type '(radio (const :tag "No auto. expand" :value nil)
                (const :tag "Expand as specified" :value expand-spec)
                (const :tag "Expand all" :value all)))


(defcustom ecb-auto-expand-tag-tree-collapse-other nil
  "*Auto. expanding the tag-tree collapses all not related nodes.
There are several choices:
- Only if on tag: This means collapsing all nodes which have no relevance for
  the currently highlighted node will be collapsed, because they are not
  necessary to make the highlighted node visible. But do this only if point
  stays onto a tag in the selected edit-window.
- Always: Same as before but collapse also when point doesn't stays on a tag
  \(e.g. between two defuns in elisp) in the selected edit-window. This means
  in such a situation a full collapsing of the methods-buffer.
- Never: Do not automatically collapse the methods-buffer."
  :group 'ecb-methods
  :type '(radio (const :tag "Collapse only when point stays on a tag"
                       :value only-if-on-tag)
                (const :tag "Collapse always" :value always)
                (const :tag "Never" :value nil)))

(defcustom ecb-expand-methods-switch-off-auto-expand t
  "*Switch off auto expanding in the ECB-method buffer.
If on then auto expanding is switched off after explicit expanding or
collapsing by `ecb-expand-methods-nodes'.

This is done with `ecb-toggle-auto-expand-tag-tree' so after the switch off
the auto expanding feature can again switched on quickly.

But after explicitly expanding/collapsing the methods-buffer to a certain
level the auto. expanding could undo this when the node belonging to current
tag under point in the edit-window is invisible after
`ecb-expand-methods-nodes' - then the auto. expand feature would make this
node immediately visible and destroys the explicitly set expand-level."
  :group 'ecb-methods
  :type 'boolean)

(defcustom ecb-auto-update-methods-after-save t
  "*Automatically updating the ECB method buffer after saving a source."
  :group 'ecb-methods
  :type 'boolean)


(defcustom ecb-font-lock-tags t
  "*Adds font-locking \(means highlighting) to the ECB-method buffer.
This options takes only effect for semantic-sources \(ie. sources supported
by semantic!) and if the font-lock-feature is loaded."
  :group 'ecb-methods
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (ecb-clear-tag-tree-cache)))
  :type 'boolean
  :initialize 'custom-initialize-default)

(defsubst ecb-font-lock-tags ()
  "Returns not nil if `ecb-font-lock-tags' is not nil and font-lock loaded."
  (and ecb-font-lock-tags
       (featurep 'font-lock)))

(defcustom ecb-tag-jump-sets-mark t
  "*Set the mark after jumping to a tag from the ECB-method buffer.
If set the user can easily jump back."
  :group 'ecb-methods
  :type 'boolean)

(defconst ecb-tag->text-functions
  (mapcar (lambda (fkt-elem)
            (cons (intern
                   (concat "ecb-"
                           (mapconcat 'identity
                                      (cdr (split-string (symbol-name fkt-elem) "-"))
                                      "-")))
                  (intern
                   (concat "ecb--" (symbol-name fkt-elem)))))
          ecb--semantic-format-function-list)
  "Alist containing one element for every member of 
`ecb--semantic-format-function-list'")

(defcustom ecb-tag-display-function '((default . ecb-format-tag-uml-prototype))
  "*Function to use for displaying tags in the methods buffer.
This functionality is set on major-mode base, i.e. for every major-mode a
different function can be used. The value of this option is a list of
cons-cells:
- The car is either a major-mode symbol or the special symbol 'default which
  means if no function for a certain major-mode is defined then the cdr of
  the 'default cons-cell is used.
- The cdr is the function used for displaying a tag in the related
  major-mode.
Every function is called with 3 arguments:
1. The tag
2. The parent-tag of tag \(can be nil)
3. The value of the function `ecb-font-lock-tags'.
Every function must return the display of the tag as string, colorized if
the third argument is not nil.

The following functions are predefined:
- For each element E of `ecb--semantic-format-function-list' exists a
  function with name \"ecb--<E>\". These functions are just aliase to
  the builtin format-functions of semantic. See the docstring of these
  functions to see what they do.
  Example: `semantic-format-tag-name' is an
  element of `ecb--semantic-format-function-list'. Therefore the
  alias-function for this element is named `ecb--semantic-format-tag-name'.
- For each element in `ecb--semantic-format-function-list' with name
  \"semantic-XYZ\" a function with name \"ecb-XYC\" is predefined. The
  differences between the semantic- and the ECB-version are:
  + The ECB-version displays for type tags only the type-name and nothing
    else \(exceptions: In c++-mode a template specifier is appended to the
    type-name if a template instead a normal class. If the tag-name of a
    type-tag is the empty-string \(tag has no name) then always the
    type-specifier is displayed - see `ecb-type-tag-display'.).
  + The ECB-version displays type-tags according to the setting in
    `ecb-type-tag-display'. This is useful for better recognizing
    different classes, structs etc. in the ECB-method window.
  For all tags which are not types the display of the ECB-version is
  identical to the semantic version. Example: For
  `ecb--semantic-format-tag-name' \(the builtin semantic formatter) the
  pendant is `ecb-format-tag-name'.

This functionality also allows the user to display tags as UML. To enable
this functionality set the function for a major-mode \(e.g. `jde-mode') to
`ecb--semantic-format-tag-uml-concise-prototype',
`ecb--semantic-format-tag-uml-prototype', or
`ecb--semantic-format-tag-uml-abbreviate' the ECB-versions of these functions.

If the value is nil, i.e. neither a function for a major-mode is defined nor
the special 'default, then `ecb--semantic-format-tag-prototype' is used for
displaying the tags.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :group 'ecb-most-important
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (ecb-clear-tag-tree-cache)))
  :type (list 'repeat ':tag "Display functions per mode"
              (list 'cons ':tag "Mode tag display"
                    '(symbol :tag "Major mode")
                    (nconc (list 'choice ':tag "Display function"
                                 ':menu-tag '"Display function")
                           (append
                            (mapcar (lambda (f)
                                      (list 'const ':tag
                                            (symbol-name (car f)) (car f)))
                                    ecb-tag->text-functions)
                            (mapcar (lambda (f)
                                      (list 'const ':tag
                                            (symbol-name (cdr f)) (cdr f)))
                                    ecb-tag->text-functions)
                            (list '(function :tag "Function"))))))
  :initialize 'custom-initialize-default)

(defun ecb-get-tag-display-function ()
  (let ((mode-display-fkt (cdr (assoc major-mode ecb-tag-display-function)))
        (default-fkt (cdr (assoc 'default ecb-tag-display-function))))
    (or (and (fboundp mode-display-fkt) mode-display-fkt)
        (and (fboundp default-fkt) default-fkt)
        'ecb--semantic-format-tag-prototype)))
  

(defcustom ecb-type-tag-display nil
  "*How to display semantic type-tags in the methods buffer.
Normally all tag displaying, colorizing and facing is done by semantic
according to the value of `ecb--semantic-format-face-alist' and the semantic
display-function \(e.g. one from `ecb--semantic-format-function-list'). But
sometimes a finer distinction in displaying the different type specifiers of
type-tags can be useful. For a description when this option is evaluated look
at `ecb-tag-display-function'!

This functionality is set on a major-mode base, i.e. for every major-mode a
different setting can be used. The value of this option is a list of
cons-cells:
- The car is either a major-mode symbol or the special symbol 'default which
  means if no setting for a certain major-mode is defined then the cdr of
  the 'default cons-cell is used.
- The cdr is a list of 3-element-lists:
  1. First entry is a semantic type specifier in string-form. Current
     available type specifiers are for example \"class\", \"interface\",
     \"struct\", \"typedef\", \"union\" and \"enum\". In addition to these
     ones there is also a special ECB type specifier \"group\" which is
     related to grouping tags \(see `ecb-post-process-semantic-taglist' and
     `ecb-group-function-tags-with-parents'). Any arbitrary specifier can be
     set here but if it is not \"group\" or not known by semantic it will be
     useless.
  2. Second entry is a flag which indicates if the type-specifier string from
     \(1.) itself should be removed \(if there is any) from the display.
  3. Third entry is the face which is used in the ECB-method window to display
     type-tags with this specifier. ECB has some predefined faces for this
     \(`ecb-type-tag-class-face', `ecb-type-tag-interface-face',
     `ecb-type-tag-struct-face', `ecb-type-tag-typedef-face',
     `ecb-type-tag-union-face', `ecb-type-tag-enum-face' and
     `ecb-type-tag-group-face') but any arbitrary face can be set here. This
     face is merged with the faces semantic already uses to display a tag,
     i.e. the result is a display where all face-attributes of the ECB-face
     take effect plus all face-attributes of the semantic-faces which are not
     set in the ECB-face \(with XEmacs this merge doesn't work so here the
     ECB-face replaces the semantic-faces; this may be fixed in future
     versions).

The default value is nil means there is no special ECB-displaying of
type-tags in addition to the displaying and colorizing semantic does. But a
value like the following could be a useful setting:

  \(\(default
     \(\"class\" t ecb-type-tag-class-face)
     \(\"group\" nil ecb-type-tag-group-face))
    \(c-mode
     \(\"struct\" nil ecb-type-tag-struct-face)
     \(\"typedef\" nil ecb-type-tag-typedef-face)))

This means that in `c-mode' only \"struct\"s and \"typedef\"s are displayed
with special faces \(the specifiers itself are not removed) and in all other
modes \"class\"es and grouping-tags \(see `ecb-tag-display-function',
`ecb-group-function-tags-with-parents') have special faces and the \"class\"
specifier-string is removed from the display.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :group 'ecb-most-important
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (ecb-clear-tag-tree-cache)))
  :type '(repeat (cons (symbol :tag "Major-mode")
                       (repeat :tag "Display of type specifiers"
                               (list (choice :tag "Specifier list"
                                             :menu-tag "Specifier list"
                                             (const :tag "class"
                                                    :value "class")
                                             (const :tag "interface"
                                                    :value "interface")
                                             (const :tag "struct"
                                                    :value "struct")
                                             (const :tag "typedef"
                                                    :value "typedef")
                                             (const :tag "union"
                                                    :value "union")
                                             (const :tag "enum"
                                                    :value "enum")
                                             (const :tag "group"
                                                    :value "group")
                                             (string :tag "Any specifier"))
                                     (boolean :tag "Remove the type-specifier" t)
                                     (face :tag "Any face"
                                           :value ecb-type-tag-class-face)))))
  :initialize 'custom-initialize-default)

(defun ecb-get-face-for-type-tag (type-specifier)
  "Return the face set in `ecb-type-tag-display' for current major-mode and
TYPE-SPECIFIER or nil."
  (let ((mode-display (cdr (assoc major-mode ecb-type-tag-display)))
        (default-display (cdr (assoc 'default ecb-type-tag-display))))
    (or (nth 2 (assoc type-specifier mode-display))
        (and (null mode-display)
             (nth 2 (assoc type-specifier default-display))))))


(defun ecb-get-remove-specifier-flag-for-type-tag (type-specifier)
  "Return the remove-specifier-flag set in `ecb-type-tag-display' for
current major-mode and TYPE-SPECIFIER or nil."
  (let ((mode-display (cdr (assoc major-mode ecb-type-tag-display)))
        (default-display (cdr (assoc 'default ecb-type-tag-display))))
    (or (nth 1 (assoc type-specifier mode-display))
        (and (null mode-display)
             (nth 1 (assoc type-specifier default-display))))))

(defcustom ecb-type-tag-expansion
  '((default . ("class" "interface" "group" "namespace"))
    (c-mode .  ("struct")))
  "*Default expansion of semantic type-tags.
Semantic groups type-tags into different type-specifiers. Current available
type specifiers are for example \"class\", \"interface\", \"struct\",
\"typedef\", \"union\" and \"enum\". In addition to these ones there is also a
special ECB type specifier \"group\" which is related to grouping tags \(see
`ecb-post-process-semantic-taglist').

This option defines which type-specifiers should be expanded at
file-open-time. Any arbitrary specifier can be set here but if it is not
\"group\" or not known by semantic it will be useless.

This functionality is set on a major-mode base, i.e. for every major-mode a
different setting can be used. The value of this option is a list of
cons-cells:
- The car is either a major-mode symbol or the special symbol 'default which
  means if no setting for a certain major-mode is defined then the cdr of
  the 'default cons-cell is used.
- The cdr is either a list of type-specifiers which should be expanded at
  file-open-time or the symbol 'all-specifiers \(then a type-tag is always
  expanded regardless of its type-specifier).

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :group 'ecb-most-important
  :type '(repeat (cons (symbol :tag "Major-mode")
                       (radio (const :tag "Expand all type-specifiers"
                                     :value all-specifiers)
                              (repeat :tag "Expand type specifiers"
                                      (choice :tag "Specifier"
                                              :menu-tag "Specifier"
                                              (const :tag "class"
                                                     :value "class")
                                              (const :tag "interface"
                                                     :value "interface")
                                              (const :tag "struct"
                                                     :value "struct")
                                              (const :tag "typedef"
                                                     :value "typedef")
                                              (const :tag "union"
                                                     :value "union")
                                              (const :tag "enum"
                                                     :value "enum")
                                              (const :tag "group"
                                                     :value "group")
                                              (string :tag "Any specifier"))))))
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (ecb-clear-tag-tree-cache)))
  :initialize 'custom-initialize-default)
  
(defun ecb-type-tag-expansion (type-specifier)
  "Return the default expansion-state of TYPE-SPECIFIER for current major-mode
as specified in `ecb-type-tag-expansion'"
  (let ((mode-expansion (cdr (assoc major-mode ecb-type-tag-expansion)))
        (default-expansion (cdr (assoc 'default ecb-type-tag-expansion))))
    (or (equal mode-expansion 'all-specifiers)
        (member type-specifier mode-expansion)
        (and (null mode-expansion)
             (or (equal default-expansion 'all-specifiers)
                 (member type-specifier default-expansion))))))

(defun ecb-get-type-specifier (tag)
  (if (ecb--semantic-tag-faux-p tag)
      "group"
    (ecb--semantic-tag-type tag)))
  

(dolist (elem ecb-tag->text-functions)
  (fset (car elem)
        `(lambda (tag &optional parent-tag colorize)
           (if (eq 'type (ecb--semantic-tag-class tag))
               (let* (;; we must here distinguish between UML- and
                      ;; not-UML-semantic functions because for UML we must
                      ;; preserve some semantic facing added by semantic (e.g.
                      ;; italic for abstract classes)!
                      (text (funcall (if (save-match-data
                                           (string-match "-uml-" (symbol-name (quote ,(car elem)))))
                                         'ecb--semantic-format-tag-uml-abbreviate
                                       'ecb--semantic-format-tag-name)
                                     tag parent-tag colorize))
                      (type-specifier (ecb-get-type-specifier tag))
                      (face (ecb-get-face-for-type-tag type-specifier))
                      (remove-flag (ecb-get-remove-specifier-flag-for-type-tag
                                    type-specifier)))
                 (save-match-data
                   ;; the following is done to replace the "struct" from
                   ;; grouping tags (see
                   ;; ecb-group-function-tags-with-parents) with "group".
                   ;; This code can be removed (or changed) if semantic allows
                   ;; correct protection display for function-tags with
                   ;; parent-tag.
                   (when (ecb--semantic-tag-faux-p tag)
                     (save-match-data 
                       (if (string-match (concat "^\\(.+"
                                                 (ecb--semantic-uml-colon-string)
                                                 "\\)\\("
                                                 (if (ecb--semantic-tag-faux-p tag)
                                                     (ecb--semantic-orphaned-member-metaparent-type)
                                                   "struct")
                                                 "\\)") text)
                           (let ((type-spec-text "group"))
                             (put-text-property 0 (length type-spec-text)
                                                'face
                                                (get-text-property
                                                 0 'face
                                                 (match-string 2 text))
                                                type-spec-text)
                             (setq text (concat (match-string 1 text)
                                                type-spec-text))))))
                   ;; Now we must maybe add a template-spec in c++-mode and
                   ;; maybe remove the type-specifier string.
                   (let (col-type-name col-type-spec template-text)
                     (save-match-data 
                       (if (string-match (concat "^\\(.+\\)\\("
                                                 (ecb--semantic-uml-colon-string)
                                                 type-specifier "\\)")
                                         text)
                           (setq col-type-name (match-string 1 text)
                                 col-type-spec (if (not remove-flag)
                                                   (match-string 2 text)))
                         ;; necessary for anonymous types like unnamed enums etc...
                         (setq col-type-spec (if (= (length text) 0)
                                                 type-specifier
                                               nil))
                         (setq col-type-name text)))
                     (when (and (equal major-mode 'c++-mode)
                                (fboundp 'semantic-c-template-string))
                       (setq template-text (semantic-c-template-string
                                            tag parent-tag colorize))
                       ;; Removing {...} from within the template-text.
                       ;; Normally the semantic-formatters should not add this
                       ;; ugly stuff.
                       (save-match-data 
                         (if (string-match "^\\(.+\\){.*}\\(.+\\)$" template-text)
                             (setq template-text
                                   (concat (match-string 1 template-text)
                                           (match-string 2 template-text))))^)
                       (put-text-property 0 (length template-text)
                                          'face
                                          (get-text-property
                                           (1- (length col-type-name)) 'face
                                           col-type-name)
                                          template-text))
                     (setq text (concat col-type-name template-text
                                        col-type-spec))))
                 ;; now we add some own colorizing if necessary
                 (if face
                     (ecb-merge-face-into-text text face))
                 text)
             (funcall (quote ,(cdr elem)) tag parent-tag colorize)))))

(defcustom ecb-find-external-tag-functions
  (list (cons 'default
              (list (if (fboundp 'semantic-calculate-scope)
                        'ecb-search-tag-by-semantic-analyzer
                      'ecb-search-tag-by-semanticdb)))
        (cons 'jde-mode (list 'ecb-jde-show-class-source)))
  "*Functions used for searching external tags clicked in the methods buffer.
The methods buffer displays for oo-languages the parents of a
type under a special bucket \"Parents\". Languages like C++, CLOS
and Eieio allow to define the implementation of a method outside
of the class definition and even in another file. In the
methods-buffer of ECB such externaly defined methods are
collected and displayed under a 'virtual' faux type-tag named as
the class-qualifier of the methods. This faux-tag is virtual
because it does not extist in the parsed buffer.

If a user clicks either onto such a faux-type-tag or onto a
parent-tag then ECB tries to find the definition of the
underlying type on a name-basis, displaying the containing file
as buffer in the current edit-window and jumping to the start of
the type-definition in this buffer.

Finding such external types can be very complex and there are
several roads to success. ECB uses per default methods based on
the semantic-analyzer. But this option allows to define own
find-functions and tell ECB to uses them.

This functionality is set on a `major-mode' base, i.e. for every
`major-mode' a different setting can be used. The value of this
option is a list of cons-cells:
- The car is either a `major-mode' symbol or the special symbol 'default.
- The cdr is a list of find-functions or nil.

ECB first performs all find-functions defined for current
`major-mode' \(if any) anf then all find-functions defined for
the special symbol 'default \(if any).

ECB offers some predefined senseful finding-functions. Currently there are:
- `ecb-search-tag-by-semantic-analyzer' (most powerful)
- `ecb-search-tag-by-semanticdb'
- `ecb-jde-show-class-source' (for major-mode `jde-mode' when coding in java)
  This function does not only the searching but displays the founded tag.
See the documentation of these function for details how they work.

But you can add any arbitrary function if the following conditions are
fulfilled:
- The function gets a semantic tag representing the external type which should
  be found. This is a positionless-tag \(otherwise it would not be hard to go
  to it) and it's either a faux-tag \(for which `ecb--semantic-faux-tag-p' is
  not nil; the function can use this check if necessary) or a simple tag
  containing only a name an a tag-class. The tag-class for both is 'type.
- The return value of the function must be one of:
  + nil: No tag is found
  + t: A tag has been found and also be displayed in the edit-window \(this
    prevents ECB from running further function of this option because the
    searched tag is already displayed. So a function should only return t if
    all is fine and no further actions are needed.
  + A positioned semantic tag \(for which `ecb--semantic-tag-with-position-p'
    returns not nil) which represents the found external type-tag.
  It's strongly recommended for the function not to display the found
  location for itself but to return a positioned semantic tag! But sometimes
  the displaying is integrated in a third-party find-function like
  `jde-show-class-source' which is used by `ecb-jde-show-class-source'. In
  these cases the function has to return t if the searched tag has been
  successfully displayed.

Precondition for a find-function:
Current buffer is the buffer the clicked faux- or parent tag belongs to
Current point depends on the clicked tag:
- In case of a faux-tag it's the start of the first child of the
  faux-tag. There must be at least one adopted child-tag because
  otherwise we would not have the faux-tag
- In case of an external parent-tag its the the start of the
  external tag itself."
  :group 'ecb-methods
  :type '(repeat (cons (symbol :tag "Major-mode or default")
                       (repeat (choice :tag "Find external tag function" :menu-tag "Function list"
                                       (const :tag "ecb-search-tag-by-semantic-analyzer"
                                              :value ecb-search-tag-by-semantic-analyzer)
                                       (const :tag "ecb-search-tag-by-semanticdb"
                                              :value ecb-search-tag-by-semanticdb)
                                       (const :tag "ecb-jde-show-class-source"
                                              :value ecb-jde-show-class-source)
                                       (function :tag "Function"))))))

(defcustom ecb-display-image-icons-for-semantic-tags t
  "*Display nice and pretty icons for semantic-tags in the Methods-buffer.
A non nil value takes only effect if Emacs can display images and if
`ecb-tree-buffer-style' is set to 'image."
  :group 'ecb-methods
  :type 'boolean)

(defsubst ecb-use-images-for-semantic-tags ()
  (and ecb-display-image-icons-for-semantic-tags
       (ecb-images-can-be-used)
       (equal ecb-tree-buffer-style 'image)))

(defcustom ecb-post-process-semantic-taglist
  '((c++-mode . (ecb-group-function-tags-with-parents))
    (emacs-lisp-mode . (ecb-group-function-tags-with-parents))
    (c-mode . (ecb-filter-c-prototype-tags)))
  "*Define mode-dependent post-processing for the semantic-taglist.
This is an alist where the car is a major-mode symbol and the cdr is a list of
function-symbols of functions which should be used for post-processing the
taglist returned by semantic. for a buffer in this
major-mode. The first function in the list is called with current semantic
taglist of current buffer and must return a valid taglist again. All other
functions are called with the result-taglist of its preceding function and
have to return a new taglist again.

For oo-programming languages where the methods of a class can be defined
outside the class-definition \(e.g. C++, Eieio) the function
`ecb-group-function-tags-with-parents' can be used to get a much better
method-display in the methods-window of ECB, because all method
implementations of a class are grouped together.

Another senseful usage is to filter out certain tags, e.g. prototype tags in
`c-mode'. For this you can set `ecb-filter-c-prototype-tags'.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :type '(repeat (cons (symbol :tag "Major-mode")
                       (repeat (function :tag "Post-process function")))))

(defcustom ecb-default-tag-filter nil
  "*Default tag-filters for certain files.
This option allow to define default tag-filters for certain files which are
applied automatically after loading such a file into a buffer. The possible
filters are the same as offered by the command `ecb-methods-filter' and they
are applied in the same manner - the only difference is they are applied
automatically. Please be aware that symbol-filters \(e.g. protection-symbols
like public or private) must not be inserted with quotes whereas a
filter-regexp has to be inserted with surrounding double-quotes! In addition
backslashes in a regexp have to be doubled!

For each file-spec \(a major-mode plus a file-regexp which both specify a
file for which filters should be applied) there can be as much filters as
needed - they are layered like with `ecb-methods-filter' too.

Tag-classes which are completely hidden or excluded by the option
`ecb-show-tags' will never being displayed in the Methods-buffer regardless of
the filters of this option!"
  :group 'ecb-methods
  :type '(repeat (cons :tag "Default tag filter"
                       (cons :tag "Filespec"
                             (symbol :tag "Major-mode")
                             (regexp :tag "Filename-regexp"))
                       (repeat :tag "Default filters"
                               (list :tag "Filterspec"
                                     (choice :tag "Filter-type"
                                             :menu-tag "Filtertype"
                                             (const :tag "Regexp" :value regexp)
                                             (const :tag "Protection" :value protection)
                                             (const :tag "Tag-class" :value tag-class)
                                             (const :tag "Funtion" :value function))
                                     (sexp :tag "Filter-value")
                                     (boolean :tag "inverse"))))))

  
(defcustom ecb-show-only-positioned-tags nil
  "*Show only nodes in the method-buffer which are \"jump-able\".
If not nil then ECB displays in the method-buffer only nodes which are
\"jump-able\", i.e. after selecting it by clicking or with RET then ECB jumps
to the corresponding location in the edit-window.
Example: With CLOS or Eieio source-code there can exist some position-less
nodes like variable-attributes in a `defclass' form which are only displayed
if this option is nil. Displaying such nodes can be senseful even if they can
not be jumped.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :type 'boolean)

(defcustom ecb-force-reparse-when-semantic-idle-scheduler-off nil
  "*Force a reparse of the semantic-source if the idle-scheduler is off.
Generally ECB calls semantic to get the list of tags for current source-file
of current edit-window. Per default ECB does never automatically force a
reparse of the source-file - this is only done on demand by calling
`ecb-rebuild-methods-buffer'. So per default the idle-scheduler of semantic is
responsible for reparsing the source-file and when this is necessary \(see
`semantic-idle-scheduler-mode' for further details). This is the most
user-resonsible and therefore the recommended approach. So it's strongly
recommended to enable `semantic-idle-scheduler-mode' because then reparsing is
always done during idle-time of Emacs and is also interruptable.

But if this idle-scheduler is switched off then ECB offers two possibilities
\(with this option):
- Not forcing itself a reparse when tags are needed by ECB: then a user
  declines knowingly Emacs/semantic-driven parsing of code when he/she
  switches off the idle-mode of semantic. This is the default behavior of ECB
  and the default value of this option. But this has also the consequence that
  the methods-buffer is only filed on demand via `ecb-rebuild-methods-buffer'
  \(bound to \[C-c . r])!
- Forcing a reparse when tags are needed: Then ECB forces semantic to parse
  the source-file when ECB needs tags to display. For this behavior this
  option has to be set to not nil.

Note 1: This option takes only effect when `semantic-idle-scheduler-mode' is
not enabled!

Note 2: The term \"forcing a reparse by semantic\" is a simplification:
It uses the function `semantic-fetch-tags' which can decide that the cached
tags are up-to-date so no real reparsing is necessary - but it can also run a
full reparse and this reparse is not being done when Emacs is idle but
immediatelly and not interruptable \(as with the idle-scheduler of semantic)!"
  :group 'ecb-methods
  :type 'boolean)

(defcustom ecb-show-tags
  '((default . ((include collapsed nil)
                (parent collapsed nil)
                (type flattened nil)
                (variable collapsed access)
                (function flattened access)
                (label hidden nil)
                (t collapsed nil)))
    (c++-mode . ((include collapsed nil)
                 (parent collapsed nil)
                 (type flattened nil)
                 (variable collapsed access)
                 (function flattened access) ;; for Methods
                 (function collapsed access) ;; for Method-prototypes
                 (label hidden nil)
                 (t collapsed nil)))
    (c-mode . ((include collapsed nil)
               (parent collapsed nil)
               (type flattened nil)
               (variable collapsed access)
               (function flattened access) ;; for Functions
               (function collapsed access) ;; for Function-prototypes
               (label hidden nil)
               (t collapsed nil)))
    (bovine-grammar-mode . ((keyword collapsed name)
                            (token collapsed name)
                            (nonterminal flattened name)
                            (rule flattened name)
                            (t collapsed nil)))
    (wisent-grammar-mode . ((keyword collapsed name)
                            (token collapsed name)
                            (nonterminal flattened name)
                            (rule flattened name)
                            (t collapsed nil)))
    (texinfo-mode . ((section flattened nil)
                     (def collapsed name)
                     (t collapsed nil))))
  "*How to show tags in the methods buffer first time after find-file.
This functionality is set on a major-mode base, i.e. for every major-mode a
different setting can be used. The value of this option is a list of
cons-cells:

The car is either a major-mode symbol or the special symbol 'default which
means if no setting for a certain major-mode is defined then the cdr of
the 'default cons-cell is used. This option should always contain a
default-setting!

The cdr is a list where each element represents a type of tags:

\(<tag type> <display type> <sort method>)

There can be more than 1 element for a certain <tag type>. This is for example
useful for C++ and C because these languages distinct between a
method-prototype \(rsp. function-prototype for C) and the method \(rsp.
function for C) itself. The default value of these option contains two entries
for <tag type> is 'function whereas the first one is responsible for the
\"real\" methods \(rsp. functions) and the second one for the prototypes. So
if the methods should be flattened and the prototypes collapsed the
show-tags-list for C++ and C must contain two entries for <tag type>
'function, the first one defined as 'flattened and the second one defined as
'collapsed. See also `ecb-methods-separate-prototypes'.

The tags in the methods buffer are displayed in the order as they appear in
this list.

Tag Type
----------

A Semantic tag type symbol \(for all possible type symbols see documentation
of semantic):
- include
- type
- variable
- function
- rule
- section \(chapters and sections in `info-mode')
- def \(definitions in `info-mode')

or one of the following:

- t:      All tag types not specified anywhere else in the list.
- parent: The parents of a type.

Display Type
------------

A symbol which describes how the tags of this type shall be shown:

- expanded:  The tags are shown in an expanded node.
- collapsed: The tags are shown in a collapsed node.
- flattened: The tags are added to the parent node.
- hidden:    The tags are not shown.

Sort Method
-----------

A symbol describing how to sort the tags of this type:

- name:   Sort by the tag name.
- access: Sort by tag access (public, protected, private) and then by name.
- nil:    Don't sort tags. They appear in the same order as in the source
          buffer.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :group 'ecb-most-important
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (ecb-clear-tag-tree-cache)))
  :type '(repeat (cons (symbol :tag "Major-mode")
                       (repeat (list (symbol :tag "Tag symbol")
                                     (choice :tag "Display type" :value collapsed
                                             (const :tag "Expanded" expanded)
                                             (const :tag "Collapsed" collapsed)
                                             (const :tag "Flattened" flattened)
                                             (const :tag "Hidden" hidden))
                                     (choice :tag "Sort by" :value nil
                                             (const :tag "Name" name)
                                             (const :tag "Access then name" access)
                                             (const :tag "No sort" nil))))))
  :initialize 'custom-initialize-default)

(defun ecb-get-show-tags-list ()
  "Return the show-tags-list of `ecb-show-tags' for current major-mode."
  (let ((mode-show-tag-list (cdr (assoc major-mode ecb-show-tags)))
        (default-show-tag-list (cdr (assoc 'default ecb-show-tags))))
    (or mode-show-tag-list
        (and (null mode-show-tag-list)
             default-show-tag-list))))

(defcustom ecb-methods-separate-prototypes t
  "*Separate function-prototypes from the real functions.
This is for example useful for C++ and C because these languages distinct
between a method-prototype \(rsp. function-prototype for C) and the method
\(rsp. function for C) itself. If this option is not nil then ECB separates
the prototypes from the real function/methods. Then with `ecb-show-tags' the
user can define different display-settings for each of them. If this option is
nil then the prototypes and the real functions are filled in the same bucket
and displayed plain and there is no sorting between prototypes and functions
possible. If this option is switched on then it is senseful that
`ecb-show-tags' contains for all modes which distinct between prototypes and
real functions/methods two entries for the tag-type 'function - see the
documentation of this option."
  :group 'ecb-methods
  :type 'boolean)

(defcustom ecb-methods-filter-replace-existing 'never
  "*How the methods-filter should be applied to existing filters.
There are three different choices:
- 'never: This is the default and means that calling `ecb-methods-filter'
  always adds the new filter on top of already existing filters. So you can
  combine several filter to one combined like this example: 'Display only all
  public methods having the string \"test\" in its name.' With this setting
  the filters can only be cleared by calling `ecb-methods-filter' and then
  choosing \"nothing\".
- 'always: This means that `ecb-methods-filter' always clears a previous
  filter before applying the new one.
- 'ask: ECB asks if the new filter should replace the existing ones."
  :group 'ecb-methods
  :type '(radio (const :tag "Do not replace" :value never)
                (const :tag "Always replace" :value always)
                (const :tag "Ask if to replace" :value ask)))

(defcustom ecb-methods-nodes-expand-spec '(type variable function section
                                                nonterminal keyword token)
  "*Semantic tag-types expanded by `ecb-expand-methods-nodes'.
The value of this option is either the symbol 'all \(all tags are expanded
regardless of their type) or a list of symbols where each symbol is a valid
semantic tag-type. For a description of semantic tag types see option
`ecb-show-tags'.

But this option also defines if bucket-nodes in the ECB-method-buffer \(e.g.
\"\[Variables\]\") should be expanded. Therefore valid symbols for this list
are also all cars of the variable `semantic-symbol->name-assoc-list'.

If there is a bucket-name \(the node-name stripped of the settings in
`ecb-bucket-node-display') which is not contained as cdr in
`semantic-symbol->name-assoc-list' then the symbol with this bucket-name as
name is also a valid symbol for this list. Example: In ECB there are buckets
\"\[Parents\]\". The bucket-name is \"Parents\" and the valid symbol-name is
then 'Parents.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :type '(radio (const :tag "All node-types" :value all)
                (repeat :tag "Node-type list"
                        (symbol :tag "Node-type"))))


(defcustom ecb-methods-nodes-collapse-spec 'all
  "*Semantic tag-types collapsed by `ecb-expand-methods-nodes'.
For valid values of this option see `ecb-methods-nodes-expand-spec'!

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :type '(radio (const :tag "All node-types" :value all)
                (repeat :tag "Node-type list"
                        (symbol :tag "Node-type"))))

(defcustom ecb-methods-show-node-info '(if-too-long . name+type)
  "*When to display which node-info in the methods-buffer.
Define which node info should displayed after moving the mouse over a node
\(or after a shift click onto the node) in the methods-buffer.

You can define \"when\" a node-info should be displayed:
See `ecb-directories-show-node-info' for the possible choices.

You can define what info should be displayed:
- name: Only the full node name is displayed.
- name+type: The full name + the type of the node \(function, class,
  variable) is displayed.

Do NOT set this option directly via setq but use always customize!"
  :group 'ecb-methods
  :type '(cons :tag "* Method-buffer"
               (choice :tag "When"
                       (const :tag "Always" :value always)
                       (const :tag "If too long" :value if-too-long)
                       (const :tag "After shift click" :value shift-click)
                       (const :tag "Never" :value never))
               (choice :tag "What"
                       (const :tag "Node-name" :value name)
                       (const :tag "Node-name + type" :value name+type))))


(defcustom ecb-exclude-parents-regexps nil
  "*Regexps which parent classes should not be shown in the methods buffer.
If nil then all parents will be shown if `ecb-show-parents' is not nil.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (ecb-clear-tag-tree-cache)))
  :type '(repeat (regexp :tag "Parents-regexp to exclude"))
  :initialize 'custom-initialize-default)

(defsubst ecb-check-parent-for-exclude (parent-name)
  (ecb-match-regexp-list parent-name ecb-exclude-parents-regexps))

(defcustom ecb-highlight-tag-with-point 'highlight-scroll
  "*How to highlight the method or variable under the cursor.
- highlight-scroll: Always scroll the method buffer, so the current method of the
  edit-window is highlighted in the method-window.
- highlight: Only highlight the current method of the edit window in the
  method window if the method is visible in the method-window.
- nil: No highlighting is done.
See also `ecb-highlight-tag-with-point-delay'.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :type '(radio (const :tag "Highlight and scroll window"
                       :value highlight-scroll)
                (const :tag "Just highlight"
                       :value highlight)
                (const :tag "Do not highlight"
                       :value nil)))


(defcustom ecb-highlight-tag-with-point-delay 0.25
  "*Time Emacs must be idle before current tag is highlighted.
If nil then there is no delay, means current tag is highlighted immediately.
A small value of about 0.25 seconds saves CPU resources and you get even
though almost the same effect as if you set no delay. But such a delay
prevents also \"jumping backward/forward\" during scrolling within
java-classes if point goes out of method-definition into class-definition.
Therefore the default value is a delay of 0.25 seconds.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :type '(radio (const :tag "No highlighting delay"
                       :value nil)
                (number :tag "Idle time before highlighting"
                        :value 0.25))
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if (and (boundp 'ecb-minor-mode) ecb-minor-mode)
                       (ecb-activate-ecb-autocontrol-function value
                                                               'ecb-tag-sync))))
  :initialize 'custom-initialize-default)


(defvar ecb-method-overlay (ecb-make-overlay 1 1)
  "Internal overlay used for the first line of a method.")
(ecb-overlay-put ecb-method-overlay 'face ecb-tag-header-face)


(defcustom ecb-tag-visit-post-actions '((default . (ecb-tag-visit-smart-tag-start
                                                    ecb-tag-visit-highlight-tag-header))
                                        (java-mode . (ecb-tag-visit-goto-doc-start))
                                        (jde-mode . (ecb-tag-visit-goto-doc-start)))
  "*Actions to perform after visiting a tag from the Method-buffer.
With this option actions can be added which will be performed after visiting
the start of the tag in the source-buffer.

This functionality is set on a `major-mode' base, i.e. for every `major-mode' a
different setting can be used. The value of this option is a list of
cons-cells:
- The car is either a `major-mode' symbol or the special symbol 'default.
- The cdr is a list of action-functions or nil.

ECB first performs all actions defined for the special symbol 'default \(if
any) and then all actions defined for current `major-mode' \(if any).

ECB offers some predefined senseful action-functions. Currently there are:
- `ecb-tag-visit-highlight-tag-header'
- `ecb-tag-visit-smart-tag-start'
- `ecb-tag-visit-recenter'
- `ecb-tag-visit-recenter-top'
- `ecb-tag-visit-goto-doc-start'
- `ecb-tag-visit-narrow-tag'
See the documentation of these function for details what they do.

But you can add any arbitrary function if the following conditions are
fulfilled:
- The function gets the semantic tag as argument and
- the function returns the \(new) point after finishing its job.
- The function must not put the point outside the tag-boundaries of the
  tag-argument."
  :group 'ecb-methods
  :type '(repeat (cons :value (nil . (ecb-tag-visit-recenter))
                       (symbol :tag "Major-mode or default")
                       (repeat (choice :tag "Post action" :menu-tag "Post action"
                                       (const :tag "ecb-tag-visit-smart-tag-start"
                                              :value ecb-tag-visit-smart-tag-start)
                                       (const :tag "ecb-tag-visit-highlight-tag-header"
                                              :value ecb-tag-visit-highlight-tag-header)
                                       (const :tag "ecb-tag-visit-goto-doc-start"
                                              :value ecb-tag-visit-goto-doc-start)
                                       (const :tag "ecb-tag-visit-narrow-tag"
                                              :value ecb-tag-visit-narrow-tag)
                                       (const :tag "ecb-tag-visit-recenter-top"
                                              :value ecb-tag-visit-recenter-top)
                                       (const :tag "ecb-tag-visit-recenter"
                                              :value ecb-tag-visit-recenter)
                                       (function :tag "Function"))))))


(defun ecb-tag-visit-function-member-p (fnc)
  (or (member fnc (cdr (assoc 'default ecb-tag-visit-post-actions)))
      (member fnc (cdr (assoc major-mode ecb-tag-visit-post-actions)))))

(defcustom ecb-methods-menu-user-extension nil
  "*Static user extensions for the popup-menu of the methods buffer.
For further explanations see `ecb-directories-menu-user-extension'.

The node-argument of a menu-function contains as data the semantic-tag of
the method/variable/tag for which the popup-menu has been opened.

Per default the static user-extensions are added at the beginning of the
built-in menu-entries of `ecb-methods-menu' but the whole menu can be
re-arranged with `ecb-methods-menu-sorter'."
  :group 'ecb-methods
  :type (ecb-create-menu-user-ext-type 1 ecb-max-submenu-depth))


(defcustom ecb-methods-menu-user-extension-function 'ignore
  "*Dynamic user extensions for the popup-menu of the methods buffer.
A function which has to return a list in the same format like the option
`ecb-methods-menu-user-extension'. This function is called when the user opens
the popup-menu for the methods buffer. For an example how such a function can
be programmed see `ecb-methods-menu-editwin-entries'.

If no dynamically evaluated menu-extensions should be added to the
methods-buffer the function has to return nil. Therefore the default-value
of this option is `ignore'.

Per default the dynamic user-extensions are added in front of the static
extensions of `ecb-methods-menu-user-extension' but the whole menu can be
re-arranged with `ecb-methods-menu-sorter'."
  :group 'ecb-methods
  :type 'function)

(defcustom ecb-methods-menu-sorter nil
  "*Function which re-sorts the menu-entries of the directories buffer.
If a function then this function is called to sort the menu-entries of the
combined menu-entries of the user-menu-extensions of
`ecb-methods-menu-user-extension' and the built-in-menu
`ecb-methods-menu'. If nil then no special sorting will be done and the
user-extensions are placed in front of the built-in-entries.

For the guidelines for such a sorter-function see
`ecb-directories-menu-sorter'."
  :group 'ecb-methods
  :type '(choice :tag "Menu-sorter" :menu-tag "Menu-sorter"
                 (const :tag "No special sorting" :value nil)
                 (function :tag "Sort-function" :value identity)))

;; Klaus Berndl <klaus.berndl@sdm.de>: We do ot implement an own mechanism but
;; we use the semantic-idle-scheduler-* options...

;; (defcustom ecb-disable-semantic-threshold-alist nil
;;   "*Threshold for disabling semantic-parsing
;; Define a threshold fpr buffer-size. Exceeding this threshold disables parsing
;; current buffer by semantic.

;; This functionality is set on a major-mode base, i.e. for every major-mode a
;; different setting can be used. The value of this option is a list of
;; cons-cells:
;; - The car is either a major-mode symbol or the special symbol 'default which
;;   means if no setting for a certain major-mode is defined then the cdr of
;;   the 'default cons-cell is used.
;; - The cdr is an integer which defines the threshold for the buffer-size for
;;   this major-mode.

;; Example:

;;   \(\(default . 1000000)
;;     \(c-mode . 200000))

;; This example whould not parse c-mode buffers exceeding a buffer-size of
;; 200000. And buffers of all other modes would be only parsed if smaller than
;; 1000000.

;; A setting of \(\(c-mode . 200000)) would only restrict c-mode buffers to a
;; size of 200000 but would parse all other buffer regardless their size."
;;   :group 'ecb-methods
;;   :type '(repeat (cons (symbol :tag "Major-Mode")
;;                        (integer :tag "Buffer-size"))))

;; (defun ecb-get-max-buffer-size-for-parsing ()
;;   "Threshold set in `ecb-disable-semantic-threshold-alist' for
;; current major-mode"
;;   (let ((mode-threshold (cdr (assoc major-mode ecb-disable-semantic-threshold-alist)))
;;         (default-threshold (cdr (assoc 'default ecb-disable-semantic-threshold-alist))))
;;     (or mode-threshold default-threshold)))

;; (defun ecb-prevent-from-parsing-if-exceeding-threshold ()
;;   "Prevents from parsing current buffer if exceeding the threshold
;; defined in `ecb-disable-semantic-threshold-alist'."
;;   (if (and (boundp 'ecb-minor-mode)
;;            ecb-minor-mode
;;            (ecb--semantic-active-p))
;;       (let ((threshold (ecb-get-max-buffer-size-for-parsing)))
;;         (not (and threshold (> (buffer-size) threshold))))
;;     t))

(defcustom ecb-methods-buffer-after-create-hook nil
  "*Local hook running after the creation of the methods-buffer.
Every function of this hook is called once without arguments direct after
creating the methods-buffer of ECB and it's local key-map. So for example a
function could be added which performs calls of `local-set-key' to define new
key-bindings only for the methods-buffer of ECB."
  :group 'ecb-methods
  :type 'hook)


(defcustom ecb-process-non-semantic-files (if (locate-library "speedbar")
                                              t)
  "*Display contents of non-semantic-files in the ECB-methods-buffer.
See also `ecb-non-semantic-parsing-function'."
  :group 'ecb-general
  :group 'ecb-non-semantic
  :group 'ecb-most-important
  :type 'boolean)


(defcustom ecb-non-semantic-parsing-function nil
  "*Define mode-dependent parsing functions for non-semantic files.
This is an alist where the car is a major-mode symbol and the cdr is a
function-symbol of a function which should be used for parsing a non-semantic
buffer, i.h. a buffer for which no semantic grammar exists. Such a function
gets one argument - the filename of current buffer - and has to generate and
return a tag/tag list which is understandable by
`speedbar-insert-generic-list'. speedbar has already included two functions
`speedbar-fetch-dynamic-imenu' and `speedbar-fetch-dynamic-etags' which can be
used for parsing buffers with imenu rsp. etags.

This option takes only effect if `ecb-process-non-semantic-files' is not nil:
Then ECB checks for non-semantic buffers if current `major-mode' is contained
in this option and if yes, then the specified parsing function is called;
if not then the cars of the elements of `speedbar-dynamic-tags-function-list'
are called in that sequence they are listed in this variable. See option
`speedbar-dynamic-tags-function-list' for further details.

In most cases imenu-parsing is preferable over etags-parsing because imenu
operates on Emacs-buffers and needs no external tool and therefore parsing
works also if current contents of a buffer are not saved to disk. But maybe
sometimes etags may return better parsing results.

IMPORTANT: if imenu-parsing should be used then the option
`speedbar-use-imenu-flag' must be set to not nil!"
  :group 'ecb-methods
  :group 'ecb-non-semantic
  :type '(repeat (cons (symbol :tag "Major-mode")
                       (function :tag "Parsing function"))))


(defcustom ecb-non-semantic-methods-initial-expand nil
  "*Initially expand all tags for not by semantic supported sources.
This option can be customized on a major-mode basis, i.e. if a `major-mode' is
contained in this option then all tags for this modes will be initially
expanded - otherwise not."
  :group 'ecb-methods
  :group 'ecb-non-semantic
  :type '(repeat :tag "Expand this modes"
                 (symbol :tag "major mode")))


(defcustom ecb-auto-save-before-etags-methods-rebuild t
  "*Automatic saving of current buffer before rebuilding its methods.
This option is only relevant for sources which are supported and parsed by
etags \(see `ecb-process-non-semantic-files'). Because etags is an external
tool a source-buffer can only be reparsed if the buffer is saved to disk. So
the command `ecb-rebuild-methods-buffer' checks for sources which are not
supported by semantic or imenu if either this option is t or if the major-mode
of the source-buffer is contained in this list: In both cases ECB saves the
current source-buffer before it re-runs etags for reparsing the source.
If nil or if the major-mode is not contained then no automatic saving will be
done!

For all source supported by semantic or by imenu this option takes no effect."
  :group 'ecb-methods
  :group 'ecb-non-semantic
  :type '(radio (const :tag "For all etags modes" :value t)
                (repeat :tag "For these modes" (symbol :tag "Major-mode"))))


(defcustom ecb-non-semantic-exclude-modes '(sh-mode fundamental-mode text-mode)
  "*Exclude modes from parsing with imenu or etags.
Per default, ECB tries to parse all file-types not supported by semantic with
imenu or etags or some other method \(for details see the option
`ecb-non-semantic-parsing-function'). If a file-type can not be parsed by
semantic, imenu or etags than this simply results in an empty method-buffer
for this file. But nevertheless you will get a message \"Sorry, no support for
a file of that extension\" which comes from the speedbar-library and can not
switched off. Therefore if a `major-mode' is known as not parse-able by
semantic, imenu or etags it can be added to this option and then it will be
excluded from being tried to parsed."
  :group 'ecb-non-semantic
  :type '(repeat :tag "Modes to exclude"
                 (symbol :tag "Major-mode")))


(defcustom ecb-rebuild-non-semantic-methods-before-hook nil
  "*Hook at beginning of `ecb-rebuild-methods-buffer-for-non-semantic'.
So this function is always called by the command `ecb-rebuild-methods-buffer'
for not semantic supported source-types.

Every function of this hook gets one argument: The complete filename of the
current source-buffer in the edit-window. The Method-buffer is only rebuild by
`ecb-rebuild-methods-buffer-for-non-semantic' if either the hook contains no
function \(the default) or if no function of this hook returns nil! See
`run-hook-with-args-until-failure' for description how these function are
processed."
  :group 'ecb-methods
  :group 'ecb-non-semantic
  :type 'hook)

;;====================================================
;; Internals
;;====================================================


;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: maybe this should be placed in
;; ecb-common-browser.el
(defun ecb-enter-debugger (&rest error-args)
  "If `ecb-debug-mode' is not nil then enter the Emacs-debugger and signal an
error with ERROR-ARGS."
  (when ecb-debug-mode
    (let ((debug-on-error t))
      (apply 'error error-args))))

;; encapsulation all semantic-functions ECB uses if they operate with the
;; semantic-overlays, so we can handle an error if these overlays (extends for
;; XEmacs) are destroyed and invalid cause of some mysterious circumstances.

(defun ecb-semantic-assert-valid-tag (tag &optional no-reparse)
  "Assert that TAG is a valid tag. If not valid then `ecb-enter-debugger'
is called. If NO-REPARSE is not nil then the buffer is not autom. reparsed. It
returns nil if the assertion fails otherwise not nil. So the caller can even
check the result if `ecb-debug-mode' is nil in which case the function
`ecb-enter-debugger' is a no-op."
  (if (ecb--semantic-tag-p tag)
      (if (ecb--semantic-tag-with-position-p tag)
          (let ((o  (ecb--semantic-tag-overlay tag)))
            (if (and (ecb--semantic-overlay-p o)
                     (not (ecb--semantic-overlay-live-p o)))
                (progn
                  (when (not no-reparse)
                    ;; we need this because:
                    ;; 1. After every jump to a tag X via the method-buffer of
                    ;;    ECB this tag X is added to the navigation history list
                    ;;    as new ecb-nav-tag-history-item.
                    ;; 2. Before any select of a source in the sources- or
                    ;;    history-buffer or of a node in the method-buffer
                    ;;    `ecb-nav-save-current' is called which operates onto
                    ;;    the last saved history-item which is often a
                    ;;    tag-history-item (see 1.): `ecb-nav-save-current'
                    ;;    saves for tag-history-items current-position and
                    ;;    window-start relative to the tag position of the
                    ;;    last saved tag-history-item which is tag X from
                    ;;    1.
                    ;; Now suppose that after 1. and before 2. the overlay of
                    ;; tag X has been destroyed cause of some reason. Then
                    ;; the tag-history-item of 1. contains now a tag with
                    ;; a destroyed overlay. Now step 2. is performed and now
                    ;; we see why from this moment every click onto a node in
                    ;; the source-, history- or method-buffer must fail:
                    ;; During step 2. `ecb-nav-save-current' gets the tag
                    ;; from the last tag-history-item and calls for this
                    ;; tag `ecb--semantic-tag-start' which fails now because
                    ;; the contained overlay of this tag is destroyed in the
                    ;; meanwhile. Therefore we must throw away this last
                    ;; tag-history-item containing the tag with the
                    ;; destroyed overlay. Then after a complete reparse of the
                    ;; source-buffer and following rebuild of the
                    ;; ECB-method-buffer ECB is in correct state again!
                    (ecb-nav-initialize)
                    (ecb--semantic-clear-toplevel-cache)
                    (ecb-update-methods-buffer--internal))
                  (ecb-enter-debugger "Tag %S is invalid!" tag)
                  nil)
              ;; else, tag is OK.
              t))
        ;; Position-less tags are also OK.
        t)
    ;; For no semantic-tags a reparse makes no sense!
    (ecb-enter-debugger "Not a semantic tag: %S" tag)
    nil))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: This workaround should be removed
;; after semantic is available in a release which deals correctly with
;; indirect buffers!! Currently we do not use and need it because we have
;; adviced make-indirect-buffer.
;; (defun ecb-semantic-tag-buffer-indirect-buffer-check (tag)
;;   "Makes `semantic-tag-buffer' working well with indirect-buffers.
;; If current selected source-buffer is an indirect buffer and the tag-buffer of
;; TAG is not equal to this indirect-buffer but the underlying files of both
;; buffers are identical then we return the indirect buffer as tag-buffer of TAG.

;; This is ncessary because semantic is file-focussed and `semantic-tag-buffer'
;; returns always the buffer of the underlying base-buffer when called for tag of
;; the indirect-buffer.

;; This is not a full fix for the semantic problems with indirect buffers but it
;; is sufficient for the needs of ECB.

;; NOTE: This workaround should be removed after semantic is available in a
;; release which deals correctly with indirect buffers!!"
;;   (let ((source-buffer (ecb-path-selected-source 'buffer))
;;         (buffer-of-tag (ecb--semantic-tag-buffer tag)))
;;     (if (and (buffer-base-buffer source-buffer)
;;              (not (equal source-buffer buffer-of-tag))
;;              (equal (ecb-path-selected-source 'file)
;;                     (ecb-fix-filename (ecb-buffer-file-name buffer-of-tag))))
;;         source-buffer
;;       buffer-of-tag)))

(defun ecb-semantic-tag-buffer (tag)
  ;; if ecb-debug-mode is not nil then the TAG is valid if we pass the
  ;; assert. If ecb-debug-mode is nil then we call simply the semantic
  ;; function and see what happens.
  (ecb-semantic-assert-valid-tag tag)
  (ecb--semantic-tag-buffer tag))

(defun ecb-semantic-tag-start (tag)
  ;; if ecb-debug-mode is not nil then the TAG is valid if we pass the
  ;; assert. If ecb-debug-mode is nil then we call simply the semantic
  ;; function and see what happens.
  (ecb-semantic-assert-valid-tag tag)
  (ecb--semantic-tag-start tag))


(defun ecb-semantic-tag-end (tag)
  ;; if ecb-debug-mode is not nil then the TAG is valid if we pass the
  ;; assert. If ecb-debug-mode is nil then we call simply the semantic
  ;; function and see what happens.
  (ecb-semantic-assert-valid-tag tag)
  (ecb--semantic-tag-end tag))

;; Klaus: We must not reparse the buffer if `ecb--semantic-current-tag'
;; returns nil because here this is no error but nil is always returned for
;; example if point stays within a comment. Therefore here we only catch real
;; errors!
;; NOT used anymore...
(defun ecb-semantic-current-nonterminal ()
  (condition-case nil
      (ecb--semantic-current-tag)
    (error (message "ecb--semantic-current-tag has problems --> reparsed is performed!")
           (when (ecb-point-in-edit-window-number)
             (ecb--semantic-clear-toplevel-cache)
             (ecb-update-methods-buffer--internal)
             (ecb--semantic-current-tag)))))


(defun ecb-goto-window-methods ()
  "Make the ECB-methods window the current window.
If `ecb-use-speedbar-instead-native-tree-buffer' is 'method then goto to the
speedbar-window."
  (interactive)
  (or (ecb-goto-ecb-window ecb-methods-buffer-name)
      (and (equal ecb-use-speedbar-instead-native-tree-buffer 'method)
           (ecb-goto-window-speedbar))))

(defun ecb-maximize-window-methods ()
  "Maximize the ECB-methods-window.
I.e. delete all other ECB-windows, so only one ECB-window and the
edit-window\(s) are visible \(and maybe a compile-window). Works also if the
ECB-methods-window is not visible in current layout."
  (interactive)
  (if (equal ecb-use-speedbar-instead-native-tree-buffer 'method)
      (ecb-maximize-window-speedbar)
    (ecb-maximize-ecb-buffer ecb-methods-buffer-name t)))

(defecb-window-dedicator ecb-set-methods-buffer ecb-methods-buffer-name
  "Display in current window the methods-buffer and make window dedicated."
  (let ((set-methods-buffer
         (not (equal ecb-use-speedbar-instead-native-tree-buffer 'method))))
    ;; first we act depending on the value of
    ;; ecb-use-speedbar-instead-native-tree-buffer
    (when (not set-methods-buffer)
      (condition-case error-data
          (ecb-set-speedbar-buffer)
        ;; setting the speedbar buffer has failed so we set
        ;; set-method-buffer to t ==> standard-methods-buffer is set!
        (error (message "%s" error-data)
               (setq set-methods-buffer t))))
    ;; maybe we need to set the standard methods buffer:
    ;; - if ecb-use-speedbar-instead-native-tree-buffer is not 'method or
    ;; - if setting the speedbar buffer has failed.
    (when set-methods-buffer
      (if (null ecb-use-speedbar-instead-native-tree-buffer)
          (ignore-errors (ecb-speedbar-deactivate)))
      (switch-to-buffer ecb-methods-buffer-name))))


(defun ecb-create-node (parent-node display name data type)
  (if (eq 'hidden display)
      nil
    (if (eq 'flattened display)
	parent-node
      (let ((node (tree-node-new name type data nil parent-node
				 (if ecb-truncate-long-names 'end))))
	(when (eq 'expanded display)
	  (setf (tree-node->expanded node) t))
        node))))


(defun ecb-get-tag-type-display (tag-type)
  (let* ((show-tags-list (ecb-get-show-tags-list))
         (display (ecb-find-assoc tag-type show-tags-list)))
    (if display
	display
      (setq display (ecb-find-assoc t show-tags-list))
      (if display
	  display
	'(t hidden nil)))))


(defun ecb-get-tag-parent-names (parents)
  (when parents
    (let* ((parent (car parents))
	   (name (cond
		  ((ecb--semantic-tag-p parent)
		   (ecb--semantic-format-tag-name parent nil (ecb-font-lock-tags)))
		  ((stringp parent)
		   (ecb--semantic--format-colorize-text parent 'type)))))
      (if name
	  (if (ecb-check-parent-for-exclude name)
	      (ecb-get-tag-parent-names (cdr parents))
	    (cons name (ecb-get-tag-parent-names (cdr parents))))
	(if (listp parent)
	    (append (ecb-get-tag-parent-names parent)
		    (ecb-get-tag-parent-names (cdr parents))))))))

(defun ecb-get-tag-parents (tag)
  "Return a list of parent-names already colorized by semantic. Currently
there is no distinction between superclasses and interfaces."
  (ecb-get-tag-parent-names
   (append (ecb--semantic-tag-type-superclasses tag)
           (ecb--semantic-tag-type-interfaces tag))))
;;    (ecb--semantic-tag-type-parent tag)))


(defun ecb-get-tag-name (tag &optional parent-tag)
  "Get the name of TAG with the appropriate fcn from
`ecb-tag-display-function'."
  (condition-case nil
      (funcall (ecb-get-tag-display-function)
               tag parent-tag (ecb-font-lock-tags))
    (error (ecb--semantic-format-tag-prototype tag parent-tag
                                               (ecb-font-lock-tags)))))


(defun ecb-find-add-tag-bucket (node type display sort-method buckets
                                       &optional parent-tag no-bucketize)
  "Finds a bucket containing tags of the given TYPE, creates nodes for them
and adds them to the given NODE. The bucket is removed from the BUCKETS list.
PARENT-TAG is only propagated to `ecb-add-tag-bucket'."
  (when (cdr buckets)
    (let ((bucket (cadr buckets)))
      (if (eq type (ecb--semantic-tag-class (cadr bucket)))
	  (progn
	    (ecb-add-tag-bucket node bucket display sort-method parent-tag
                                  no-bucketize)
	    (setcdr buckets (cddr buckets)))
	(ecb-find-add-tag-bucket node type display sort-method
				   (cdr buckets) parent-tag no-bucketize)))))

(defsubst ecb-forbid-tag-display (tag)
  (ecb--semantic--tag-put-property tag 'hide-tag t))
  
(defsubst ecb-allow-tag-display (tag)
  (ecb--semantic--tag-put-property tag 'hide-tag nil))

(defsubst ecb-tag-forbidden-display-p (tag)
  (ecb--semantic--tag-get-property tag 'hide-tag))

(defsubst ecb-show-at-least-one-tag-p (taglist)
  "Not nil if at least one of the tags in TAGLIST should be displayed in the
Methods-buffer."
  (catch 'found
    (dolist (tag taglist)
      (if (not (ecb-tag-forbidden-display-p tag))
          (throw 'found t)))
    nil))


;; The function requires that TAGLIST is a subset of the tag-table returned by
;; semantic for the current-buffer.
(defun ecb-apply-user-filter-to-tags (taglist)
  "Applies to the tags of TAGLIST the related filter of
`ecb-methods-user-filter-alist' - if there is any."
  (save-match-data
    (let ((filters (cdr (assoc (current-buffer) ecb-methods-user-filter-alist)))
          (filter-type nil)
          (filter nil)
          (inverse nil))
      (when filters
        (dolist (tag taglist)
          (dolist (filter-spec filters)
            (setq filter-type (nth 0 filter-spec))
            (setq filter (car (nth 1 filter-spec))) ;; ignore the attached fcn
            (setq inverse (nth 2 filter-spec))
            ;; we forbid some tags to be displayed when they do not match the
            ;; filter. Currently we do not apply a filter to tags of class 'type
            (unless (equal (ecb--semantic-tag-class tag) 'type)
              (cond ((equal filter-type 'regexp)
                     (if (funcall inverse
                                  (not (string-match filter
                                                     (ecb--semantic-tag-name tag))))
                         (ecb-forbid-tag-display tag)))
                    ((and (member filter '(private protected public))
                          (equal filter-type 'protection))
                     (if (funcall inverse
                                  (not (or (null (ecb--semantic-tag-protection tag))
                                           (equal (ecb--semantic-tag-protection tag)
                                                  filter))))
                         (ecb-forbid-tag-display tag)))
                    ((and (symbolp filter)
                          (equal filter-type 'tag-class))
                     (if (funcall inverse
                                  (not (equal (ecb--semantic-tag-class tag) filter)))
                         (ecb-forbid-tag-display tag)))
                    ((and (functionp filter)
                          (equal filter-type 'function))
                     (if (funcall inverse
                                  (not (funcall filter tag (current-buffer))))
                         (ecb-forbid-tag-display tag)))
                    (t nil)))))))))


(defun ecb-tag-generate-node-name (text-name first-chars icon-name)
  "Generate an suitable node name. Add needed image-icons if possible and
necessary. For the arguments TEXT-NAME, FIRST-CHARS and ICON-NAME see
`ecb-generate-node-name'."
  (if (ecb-use-images-for-semantic-tags)
      (ecb-generate-node-name text-name first-chars icon-name
                              ecb-methods-buffer-name)
    text-name))


(defun ecb-add-tag-bucket (node bucket display sort-method
                                &optional parent-tag no-bucketize)
  "Adds a tag bucket to a node unless DISPLAY equals 'hidden."
  (when bucket
    (let* ((name-bucket (ecb-format-bucket-name (car bucket)))
           (image-name (format "%s-bucket" (ecb--semantic-tag-class (cadr bucket))))
           (name (ecb-tag-generate-node-name name-bucket -1 image-name))
           ;;(type (ecb--semantic-tag-class (cadr bucket)))
           (bucket-node node))
      (unless (eq 'hidden display)
        (ecb-apply-user-filter-to-tags (cdr bucket))
	(unless (or (eq 'flattened display)
                    ;; we must not create a bucket-node when each tag in the
                    ;; bucket is forbidden to be displayed
                    (not (ecb-show-at-least-one-tag-p (cdr bucket))))
	  (setq bucket-node
                (tree-node-new name ecb-methods-nodetype-bucket
                               (list 'ecb-bucket-node
                                     (car bucket)
                                     (ecb--semantic-tag-class (car (cdr bucket))))
                               nil node
                               (if ecb-truncate-long-names 'end)))
	  (setf (tree-node->expanded bucket-node) (eq 'expanded display)))
        (dolist (tag (ecb-sort-tags sort-method (cdr bucket)))
          ;; we create only a new node for a tag of the bucket when the tag is
          ;; not forbidden to be displayed.
          (if (not (ecb-tag-forbidden-display-p tag))
              (ecb-update-tag-node tag
                                   (tree-node-new "" ecb-methods-nodetype-tag
                                                  tag t bucket-node
                                                  (if ecb-truncate-long-names 'end))
                                   parent-tag no-bucketize))
          ;; now we allow each tag to be displayed. This can be done because
          ;; here we already excluded the tag from being added as a node to
          ;; the tree-buffer and therefore from being displayed. So we can
          ;; reset all tags to be shown by default. So we can apply a complete
          ;; new filter (or no filter) without resetting the old filter before.
          (ecb-allow-tag-display tag))))))



(defconst ecb-tag-image-name-alias-alist
  '((abstract . ((static . ((struct . ((nil . "abstract-class-unknown")
                                       (unknown . "abstract-class-unknown")
                                       (private . "abstract-class-private")
                                       (protected . "abstract-class-protected")
                                       (public . "abstract-class-public")))
                            (class . ((nil . "abstract-class-unknown")
                                      (unknown . "abstract-class-unknown")
                                      (private . "abstract-class-private")
                                      (protected . "abstract-class-protected")
                                      (public . "abstract-class-public")))
                            ;; currently we have no special icon for
                            ;; interfaces - we use the icon for abstract classes
                            (interface . ((nil . "abstract-class-unknown")
                                          (unknown . "abstract-class-unknown")
                                          (private . "abstract-class-private")
                                          (protected . "abstract-class-protected")
                                          (public . "abstract-class-public")))
                            ;; we have no static and no abstract enum-icon
                            (enum . ((nil . "enum-unknown")
                                     (unknown . "enum-unknown")
                                     (private . "enum-private")
                                     (protected . "enum-protected")
                                     (public . "enum-public")))
                            ;; we have no icon for static constructors
                            (constructor . ((nil . "abstract-constructor-unknown")
                                            (unknown . "abstract-constructor-unknown")
                                            (private . "abstract-constructor-private")
                                            (protected . "abstract-constructor-protected")
                                            (public . "abstract-constructor-public")))
                            (function . ((nil . "abstract-function-unknown-static")
                                         (unknown . "abstract-function-unknown-static")
                                         (private . "abstract-function-private-static")
                                         (protected . "abstract-function-protected-static")
                                         (public . "abstract-function-public-static")))
                            (variable . ((nil . "abstract-variable-unknown-static")
                                         (unknown . "abstract-variable-unknown-static")
                                         (private . "abstract-variable-private-static")
                                         (protected . "abstract-variable-protected-static")
                                         (public . "abstract-variable-public-static")))))
                 (not-static . ((struct . ((nil . "abstract-class-unknown")
                                           (unknown . "abstract-class-unknown")
                                           (private . "abstract-class-private")
                                           (protected . "abstract-class-protected")
                                           (public . "abstract-class-public")))
                                (class . ((nil . "abstract-class-unknown")
                                          (unknown . "abstract-class-unknown")
                                          (private . "abstract-class-private")
                                          (protected . "abstract-class-protected")
                                          (public . "abstract-class-public")))
                                ;; we have currently no special icon for interfaces
                                (interface . ((nil . "abstract-class-unknown")
                                              (unknown . "abstract-class-unknown")
                                              (private . "abstract-class-private")
                                              (protected . "abstract-class-protected")
                                              (public . "abstract-class-public")))
                                ;; we have no abstract enum-icon
                                (enum . ((nil . "enum-unknown")
                                         (unknown . "enum-unknown")
                                         (private . "enum-private")
                                         (protected . "enum-protected")
                                         (public . "enum-public")))
                                (constructor . ((nil . "abstract-constructor-unknown")
                                                (unknown . "abstract-constructor-unknown")
                                                (private . "abstract-constructor-private")
                                                (protected . "abstract-constructor-protected")
                                                (public . "abstract-constructor-public")))
                                (function . ((nil . "abstract-function-unknown")
                                             (unknown . "abstract-function-unknown")
                                             (private . "abstract-function-private")
                                             (protected . "abstract-function-protected")
                                             (public . "abstract-function-public")))
                                (variable . ((nil . "abstract-variable-unknown")
                                             (unknown . "abstract-variable-unknown")
                                             (private . "abstract-variable-private")
                                             (protected . "abstract-variable-protected")
                                             (public . "abstract-variable-public")))))))
    (not-abstract . ((static . ((struct . ((nil . "class-unknown")
                                           (unknown . "class-unknown")
                                           (private . "class-private")
                                           (protected . "class-protected")
                                           (public . "class-public")))
                                (class . ((nil . "class-unknown")
                                          (unknown . "class-unknown")
                                          (private . "class-private")
                                          (protected . "class-protected")
                                          (public . "class-public")))
                                ;; we use the icon for abstract classes for interfaces
                                (interface . ((nil . "abstract-class-unknown")
                                              (unknown . "abstract-class-unknown")
                                              (private . "abstract-class-private")
                                              (protected . "abstract-class-protected")
                                              (public . "abstract-class-public")))
                                ;; we have no static enum-icon
                                (enum . ((nil . "enum-unknown")
                                         (unknown . "enum-unknown")
                                         (private . "enum-private")
                                         (protected . "enum-protected")
                                         (public . "enum-public")))
                                (constructor . ((nil . "constructor-unknown")
                                                (unknown . "constructor-unknown")
                                                (private . "constructor-private")
                                                (protected . "constructor-protected")
                                                (public . "constructor-public")))
                                (function . ((nil . "function-unknown-static")
                                             (unknown . "function-unknown-static")
                                             (private . "function-private-static")
                                             (protected . "function-protected-static")
                                             (public . "function-public-static")))
                                (variable . ((nil . "variable-unknown-static")
                                             (unknown . "variable-unknown-static")
                                             (private . "variable-private-static")
                                             (protected . "variable-protected-static")
                                             (public . "variable-public-static")))))
                     (not-static . ((struct . ((nil . "class-unknown")
                                               (unknown . "class-unknown")
                                               (private . "class-private")
                                               (protected . "class-protected")
                                               (public . "class-public")))
                                    (class . ((nil . "class-unknown")
                                              (unknown . "class-unknown")
                                              (private . "class-private")
                                              (protected . "class-protected")
                                              (public . "class-public")))
                                    (interface . ((nil . "abstract-class-unknown")
                                                  (unknown . "abstract-class-unknown")
                                                  (private . "abstract-class-private")
                                                  (protected . "abstract-class-protected")
                                                  (public . "abstract-class-public")))
                                    (enum . ((nil . "enum-unknown")
                                             (unknown . "enum-unknown")
                                             (private . "enum-private")
                                             (protected . "enum-protected")
                                             (public . "enum-public")))
                                    (constructor . ((nil . "constructor-unknown")
                                                    (unknown . "constructor-unknown")
                                                    (private . "constructor-private")
                                                    (protected . "constructor-protected")
                                                    (public . "constructor-public")))
                                    (function . ((nil . "function-unknown")
                                                 (unknown . "function-unknown")
                                                 (private . "function-private")
                                                 (protected . "function-protected")
                                                 (public . "function-public")))
                                    (variable . ((nil . "variable-unknown")
                                                 (unknown . "variable-unknown")
                                                 (private . "variable-private")
                                                 (protected . "variable-protected")
                                                 (public . "variable-public"))))))))
  "This alist defines the mapping from the combination
abstract-static-tag-protection to an existing icon-file-name.")


(defsubst ecb-get-icon-for-tag (abstract-p static-p type protection)
  (cdr (assq protection
              (cdr (assq type
                          (cdr (assq static-p
                                      (cdr (assq abstract-p
                                                  ecb-tag-image-name-alias-alist)))))))))


;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: All this tag-icon-display-stuff
;; should be done by semantic - but for now we let do it by ECB because so we
;; can test the whole stuff. If Eric has added such icon-display to semantic
;; then we can throw away all this stuff and just using plain-tag-name as
;; node-name without any modification.

(defun ecb-displayed-tag-name (tag &optional parent-tag)
  "Return the tag-name of TAG as it will be displayed in the methods-buffer."
  (let* ((plain-tag-name (ecb-get-tag-name tag parent-tag))
         (has-protection (if (= 0 (length plain-tag-name))
                             nil
                           (member (ecb-first plain-tag-name)
                                   '(?- ?# ?+))))
         (icon-name (ecb-get-icon-for-tag
                     (if (ecb--semantic-tag-abstract-p tag parent-tag)
                         'abstract
                       'not-abstract)
                     (if (ecb--semantic-tag-static-p tag parent-tag)
                         'static
                       'not-static)
                     (or (and (equal (ecb--semantic-tag-class tag)
                                     'type)
                              (intern (ecb--semantic-tag-type tag)))
                         (and (ecb--semantic-tag-function-constructor-p tag)
                              'constructor)
                         (ecb--semantic-tag-class tag))
                     (or (and (ecb--semantic--tag-get-property tag 'adopted)
                              'unknown)
                         (and (not (member (ecb--semantic-tag-class tag)
                                           '(type function variable)))
                              'unknown)
                         (ecb--semantic-tag-protection tag parent-tag)))))
    (ecb-tag-generate-node-name plain-tag-name
                                (if has-protection 1 -1)
                                icon-name)))

(defun ecb-children-tags (parent-tag)
  "Return a list of children-tags of PARENT-TAG. If a child is not a
semantic-tag \(but a plain string) then it will be converted to a positionless
tag of class 'variable."
  (mapcar (function (lambda (c)
                      (typecase c
                        (ecb--semantic-tag
                         c)
                        (string
                         (ecb--semantic-tag-new-variable c nil nil nil))
                        (otherwise
                         (ecb-error "Tag with name %s contains invalid childrens"
                                    (ecb--semantic-tag-name parent-tag))))))
          (ecb--semantic-tag-children-compatibility
           parent-tag ecb-show-only-positioned-tags)))
                        

(defun ecb-update-tag-node (tag node &optional parent-tag no-bucketize)
  "Updates a node containing a tag."
  (let ((children (ecb-children-tags tag))
        (tag-name (ecb-displayed-tag-name tag parent-tag)))
    (setf (tree-node->name node) tag-name)
    (unless (eq 'function (ecb--semantic-tag-class tag))
      (ecb-add-tags node children tag no-bucketize)
      (setf (tree-node->expandable node)
            (not (= 0 (length (tree-node->children node)))))
      ;; Always expand types, maybe this should be customizable and more
      ;; flexible
      (if (not (eq 'type (ecb--semantic-tag-class tag)))
          (setf (tree-node->expanded node) nil)
        (let ((type-specifier (ecb-get-type-specifier tag)))
          (setf (tree-node->expanded node)
                (and (tree-node->expandable node)
                     (ecb-type-tag-expansion type-specifier))))))))

;; (ecb-tag-generate-node-name "klaus" 1 "function-public")

(defun ecb-post-process-taglist (taglist)
  "If for current major-mode post-process functions are found in
`ecb-post-process-semantic-taglist' then these functions are called with
TAGLIST otherwise TAGLIST is returned."
  (let ((fcn-list (cdr (assoc major-mode ecb-post-process-semantic-taglist))))
    (dolist (fcn fcn-list)
      (if (fboundp fcn)
          (setq taglist (funcall fcn taglist)))))
  (ecb-set-current-tag-table taglist)
  ;; now we apply that tag-filters which must operate onto the whole
  ;; tag-table of
  (ecb-apply-tag-table-filters taglist))

(defun ecb-apply-tag-table-filters (taglist)
  "Perform all tag-filters which must be applied to the whole tag-table."
  (let ((filters (cdr (assoc (current-buffer) ecb-methods-user-filter-alist)))
        (filter nil))
    (dolist (filter-type '(current-type))
      (setq filter (car (cdr (assoc filter-type filters))))
      (if filter
          (setq taglist (funcall (cdr filter) (car filter) taglist)))))
  taglist)


(defun ecb-methods-filter-perform-current-type (filter taglist)
  "Perform a current-type filter on TAGLIST. FILTER is a type-name-hierarchy
for a certain type. If this hierarchy can be found in TAGLIST a new tag-list
is returned which contains only the leaf-type in the hierarchy."
  (let ((curr-type-filter (reverse filter))
        (new-tag-list taglist)
        (found-type-tag nil))
    (if (null curr-type-filter)
        taglist
      (catch 'not-found
        (dolist (type-name curr-type-filter)
          (setq found-type-tag
                (car (ecb--semantic-find-tags-by-name
                      type-name
                      (ecb--semantic-find-tags-by-class 'type new-tag-list))))
          (if (null found-type-tag)
              (progn
                ;; remove here the filters for current source because the
                ;; current-type filter is no longer useable! TODO: Klaus
                ;; Berndl <klaus.berndl@sdm.de>: Maybe we should be smarter
                ;; and only remove the current-type-filter instead of all
                ;; filters. This could be done with
                ;; `ecb-replace-first-occurence' (replace the curr filter with
                ;; nil and then do (delq nil filters)
                (ecb-methods-filter-apply nil nil nil "" "" (current-buffer))
                (ecb-info-message
                 "ECB has removed all filters cause of changes in the type-hierarchy for the current-type!")
                ;; whenever we can not found any type in our filter type-hierarchy
                ;; then we can not apply this current-type filter so we have to
                ;; return the original tag-list
                (throw 'not-found taglist))
            (setq new-tag-list (ecb-children-tags found-type-tag))))
        ;; when we reach this point we can be sure that the whole type-hierarchy
        ;; has been found and so we return just our current-type as new taglist.
        (list found-type-tag)))))

(defun ecb-group-function-tags-with-parents (taglist)
  "Return a new taglist based on TAGLIST where all function-tags in
TAGLIST having a parent tag are grouped together under a new faux tag
for this parent-tag. The new taglist contains first all parent-less tags
and then all grouped tags.

This is useful for oo-programming languages where the methods of a class can
be defined outside the class-definition, e.g. C++, Eieio."
  (ecb--semantic-adopt-external-members taglist))

(defun ecb-filter-c-prototype-tags (taglist)
  "Filter out all prototypes.
Beginning with version 2.24 of ECB this function does nothing when
`ecb-methods-separate-prototypes' is set to not nil \(default).

For example this is useful for editing C files which have the function
prototypes defined at the top of the file and the implementations at the
bottom. This means that everything appears twice in the methods buffer, but
probably nobody wants to jump to the prototypes, they are only wasting space
in the methods buffer.
For C-header-files prototypes are never filtered out!"
;;   ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Is there a better way to
;;   ;; recognize a C-Header-file?
  (if ecb-methods-separate-prototypes
      taglist
    (let ((header-extensions '("\\.h\\'" "\\.H\\'" "\\.HH\\'" "\\.hxx\\'" "\\.hh\\'")))
      (or (and (catch 'found
                 (dolist (ext header-extensions)
                   (if (save-match-data
                         (string-match ext (ecb-buffer-file-name (current-buffer))))
                       (throw 'found t)))
                 nil)
               taglist)
          (ecb-filter taglist
                      (function (lambda (x)
                                  (not (ecb--semantic-tag-prototype-p x)))))))))

;; Filtering the Methods-buffer by the user ----------------

(defvar ecb-methods-user-filter-alist nil
  "The filter currently applied to the methods-buffer by the user. This cache
is an alist where the key is the buffer-object of that buffer the filter
belongs and the value is the applied filter to that buffer.

Filters which can work onto single tags are applied by
`ecb-apply-user-filter-to-tags' whereas tag-filters which have to be applied
onto the whole tag-table are performed by `ecb-apply-tag-table-filters'.")


(defun ecb-methods-filter-by-prot (inverse source-buffer &optional prot)
  "Filter the Methods-buffer by protection."
  (let ((choice (or prot
                    (ecb-query-string "Protection filter:"
                                      '("private" "protected" "public")))))
    (ecb-methods-filter-apply 'protection
                              (cons (intern choice) nil)
                              inverse
                              (concat (and inverse "^") "Prot")
                              choice
                              source-buffer)))

(defun ecb-methods-filter-by-tag-class (inverse source-buffer
                                                &optional tag-class)
  "Filter the Methods-buffer by a tag-class."
  (let* ((curr-semantic-symbol->name-assoc-list
          (save-excursion
            (set-buffer source-buffer)
            (ecb--semantic-symbol->name-assoc-list)))
         (choice (or tag-class
                     (ecb-query-string "Tag-class filter:"
                                       (mapcar 'cdr
                                               curr-semantic-symbol->name-assoc-list))))
         (class (or tag-class
                    (symbol-name
                     (car (delq nil (mapcar (function (lambda (e)
                                                        (if (ecb-string= (cdr e) choice)
                                                            (car e))))
                                            curr-semantic-symbol->name-assoc-list)))))))
    (ecb-methods-filter-apply 'tag-class
                              (cons (intern class) nil)
                              inverse
                              (concat (and inverse "^") "Tagclass")
                              (cdr (assoc (intern class)
                                          curr-semantic-symbol->name-assoc-list))
                              source-buffer)))


;; The popup-menu commands for protection- and tag-class-filters are generated
;; dynamically - see `ecb-methods-menu-tagfilter-entries'.

(defun ecb-methods-filter-by-regexp (inverse source-buffer &optional regexp)
  "Filter the Methods-buffer by a regular expression."
  (let ((regexp-str (or regexp (read-string "Filter-regexp: "))))
    (ecb-methods-filter-apply 'regexp
                              (if (> (length regexp-str) 0)
                                  (cons regexp-str nil)
                                nil)
                              inverse
                              (concat (and inverse "^") "Regexp")
                              (if (> (length regexp-str) 0) regexp-str nil)
                              source-buffer)))

(tree-buffer-defpopup-command ecb-methods-filter-by-regexp-popup
  "Filter the Methods-buffer by regexp from popup."
  (ecb-methods-filter-by-regexp nil (ecb-methods-get-data-store 'source-buffer)))

(tree-buffer-defpopup-command ecb-methods-filter-by-regexp-popup-inverse
  "Filter the Methods-buffer by inverse regexp from popup."
  (ecb-methods-filter-by-regexp t (ecb-methods-get-data-store 'source-buffer)))

(defun ecb-methods-filter-by-function (inverse source-buffer &optional fcn-name)
  "Filter the Methods-buffer by a filter-function."
  (let ((filter-fcn-name (or fcn-name
                             (completing-read "Tag-filter-function: "
                                              obarray 'fboundp t))))
    (ecb-methods-filter-apply 'function
                              (cons (intern filter-fcn-name)
                                    nil)
                              inverse
                              (concat (and inverse "^") "Function")
                              filter-fcn-name
                              source-buffer)))

(tree-buffer-defpopup-command ecb-methods-filter-by-function-popup
  "Filter the Methods-buffer by function-filter from popup."
  (ecb-methods-filter-by-function nil (ecb-methods-get-data-store 'source-buffer)))

(tree-buffer-defpopup-command ecb-methods-filter-by-function-popup-inverse
  "Filter the Methods-buffer by inverse function-filter from popup."
  (ecb-methods-filter-by-function t (ecb-methods-get-data-store 'source-buffer)))

(tree-buffer-defpopup-command ecb-methods-filter-by-nothing-popup
  "Remove any filter from the Methods-buffer from popup."
  (ecb-methods-filter-apply nil nil nil "" ""
                            (ecb-methods-get-data-store 'source-buffer)))

(tree-buffer-defpopup-command ecb-methods-filter-delete-last-popup
  "Remove the last added filter from the Methods-buffer from popup."
  (ecb-methods-filter-apply nil nil nil "" "" (ecb-methods-get-data-store 'source-buffer) t))


(defun ecb-get-type-node-of-node (node)
  "Returns that node which data-tag is of class 'type the tag of the node NODE
of the Methods-buffer belongs to. If the tag of NODE do not belong to a type
then nil is returned."
  (let ((parent (tree-node->parent node)))
    (catch 'found
      (while (not (eq (tree-buffer-get-root) parent))
        (if (equal (and (= (tree-node->type parent) ecb-methods-nodetype-tag)
                        (ecb--semantic-tag-class (tree-node->data parent)))
                   'type)
            (throw 'found parent)
          (setq parent (tree-node->parent parent))))
      nil)))


(defun ecb-get-type-name-hierarchy-of-current-node ()
  "Return the type-name-hierarchy of current node in form of a list whereas the
first element is the name of the tag of the current node itself, the second
element is the name of the type the current node belongs to, the third element
is the name of the parent-type of that type and so on. The last element in
this list is the topmost type-parent of the tag of the current node. If the
current node has no tag as data then nil is returned. If the tag of the
current node does not belong to a type-tag \(e.g. a toplevel function) then
the returned list contains just the name of the tag of the current node."
  (let ((type-hierarchy nil)
        (curr-node (tree-buffer-get-node-at-point)))
    (when (and curr-node
               (= (tree-node->type curr-node) ecb-methods-nodetype-tag))
      (while (progn
               (setq type-hierarchy (cons (ecb--semantic-tag-name
                                           (tree-node->data curr-node))
                                          type-hierarchy))
               (setq curr-node (ecb-get-type-node-of-node curr-node)))))
    (nreverse type-hierarchy)))


(defun ecb-get-type-tag-of-tag (&optional tag table always-parent-type)
  "Returns that tag of class 'type the tag TAG belongs to. If TAG does not
belong to a type then nil is returned. If TAG is already of class 'type then
the behavior depends on the optional argument ALWAYS-PARENT-TYPE: If nil then
the current tag is returned otherwise the next parent-tag of class 'type is
returned.

If TAG is nil the tag returned by `ecb-get-real-curr-tag' is used. If TABLE is
nil then the tag-table of the current buffer is used; otherwise the tag-table
TABLE is used."
  (let* ((table (or table (ecb-get-current-tag-table)))
         (curr-tag (or tag (ecb-get-real-curr-tag)))
         (function-parent (ecb--semantic-tag-function-parent curr-tag)))
    (cond ((ecb--semantic-tag-faux-p curr-tag)
           (and (not always-parent-type) curr-tag))
          ((and (not always-parent-type)
                (equal (ecb--semantic-tag-class curr-tag) 'type))
           curr-tag)
          (t (if function-parent
                 ;; we have an external member and we search the type this
                 ;; external member belongs to. This can either be a type-tag
                 ;; in the current file (which is then contained in table) or
                 ;; a faux-tag (created by semantic-adopt-external-members)
                 ;; when the parent-type of this external member is defined
                 ;; outside the current source - but this faux-type is
                 ;; contained in table too.
                 (catch 'found
                   (dolist (tag (ecb--semantic-flatten-tags-table table))
                     (if (and (equal (ecb--semantic-tag-class tag) 'type)
                              (ecb-string= (ecb--semantic-tag-name tag)
                                           function-parent)
                              (delq nil
                                    (mapcar (lambda (child)
                                              (if (ecb--semantic-equivalent-tag-p
                                                   child curr-tag)
                                                  curr-tag))
                                            (ecb-children-tags tag))))
                         (throw 'found tag)))
                   nil)
               ;; we are already inside the parent-type - if there is any, so
               ;; we simply search the nearest tag of class 'type in the
               ;; reversed overlay-stack
               (catch 'found
                 (dolist (tag (cdr (reverse
                                    (ecb--semantic-find-tag-by-overlay
                                     (ecb-semantic-tag-start curr-tag)
                                     (ecb-semantic-tag-buffer curr-tag)))))
                   (if (equal (ecb--semantic-tag-class tag) 'type)
                       (throw 'found tag)))
                 nil))))))


(defun ecb-get-type-name-hierarchy-of-current-tag (&optional tag)
  "Return the type-name-hierarchy of TAG in form of a list whereas the
first element is the name of the TAG itself, the second element is the name of
the type the TAG belongs to, the third element is the name of the parent-type
of that type and so on. The last element in this list is the topmost
type-parent of the TAG. If the TAG does not belong to a type-tag \(e.g. a
toplevel function) then the returned list contains just the name of the
TAG. If TAG is nil then the current tag returned by `ecb-get-real-curr-tag' is
used; if point does not stay on a tag then nil is returned."
  (let ((type-hierarchy nil)
        (curr-tag (or tag (ecb-get-real-curr-tag))))
    (when curr-tag
      (while (progn
               (setq type-hierarchy (cons (ecb--semantic-tag-name curr-tag)
                                          type-hierarchy))
               (setq curr-tag (ecb-get-type-tag-of-tag curr-tag nil t)))))
    (nreverse type-hierarchy)))

(defun ecb-methods-filter-by-current-type (inverse source-buffer &optional
                                                   tag)
  "Display only the current-type and its contents in the methods-buffer. The
argument INVERSE is ignored here."
  (let* ((curr-type-tag (or (and (ecb--semantic-tag-p tag)
                                 (save-excursion
                                   (set-buffer source-buffer)
                                   (ecb-get-type-tag-of-tag tag)))
                            (cond ((ecb-point-in-edit-window-number)
                                   (if (ecb--semantic-active-p)
                                       (save-excursion
                                         (set-buffer source-buffer)
                                         (ecb-get-type-tag-of-tag (ecb-get-real-curr-tag)))))
                                  ((equal (current-buffer)
                                          (get-buffer ecb-methods-buffer-name))
                                   (let ((node (tree-buffer-get-node-at-point)))
                                     (and node
                                          (tree-node->data (ecb-get-type-node-of-node node)))))
                                  (t (ecb-error "ECB can not identify the current-type-tag!")))))
         (curr-tag-type-name-hierachy (and curr-type-tag
                                           (save-excursion
                                             (set-buffer source-buffer)
                                             (ecb-get-type-name-hierarchy-of-current-tag
                                              curr-type-tag)))))
    (if (and curr-type-tag curr-tag-type-name-hierachy)
        (ecb-methods-filter-apply 'current-type
                                  (cons curr-tag-type-name-hierachy
                                        'ecb-methods-filter-perform-current-type)
                                  nil
                                  "Type"
                                  (ecb--semantic-tag-name curr-type-tag)
                                  source-buffer)
      (ecb-error "ECB can not identify the current-type!"))))
                          
(tree-buffer-defpopup-command ecb-methods-filter-by-current-type-popup
  "Display only the current-type from popup."
  (ecb-methods-filter-by-current-type nil
                                      (ecb-methods-get-data-store 'source-buffer)
                                      (tree-node->data node)))


(defun ecb-get-source-buffer-for-tag-filter ()
  "Return the source-buffer of the tag-list which should be filtered."
  (cond ((ecb-point-in-edit-window-number)
         (current-buffer))
        ((equal (current-buffer)
                (get-buffer ecb-methods-buffer-name))
         (ecb-methods-get-data-store 'source-buffer))
        (t (or (and ecb-last-source-buffer
                    (buffer-live-p ecb-last-source-buffer)
                    ecb-last-source-buffer)
               (ecb-error "There is no source-file to filter!")))))
  
(defun ecb-methods-filter-inverse ()
  "Apply an inverse filter to the Methods-buffer. This is the same as calling
`ecb-methods-filter' with a prefix arg."
  (interactive)
  (ecb-methods-filter-internal t))

(defun ecb-methods-filter-protection (&optional inverse)
  "Filter the methods-buffer by protection. If INVERSE is not nil \(called
with a prefix arg) then an inverse filter is applied. For further details see
`ecb-methods-filter'."
  (interactive "P")
  (ecb-methods-filter-internal inverse 'protection))

(defun ecb-methods-filter-tagclass (&optional inverse)
  "Filter the methods-buffer by tag-class. If INVERSE is not nil \(called
with a prefix arg) then an inverse filter is applied. For further details see
`ecb-methods-filter'."
  (interactive "P")
  (ecb-methods-filter-internal inverse 'tag-class))

(defun ecb-methods-filter-current-type ()
  "Display in the Methods-buffer only the current type and its members. For
further details see `ecb-methods-filter'."
  (interactive)
  (ecb-methods-filter-internal nil 'curr-type))

(defun ecb-methods-filter-regexp (&optional inverse)
  "Filter the methods-buffer by a regexp. If INVERSE is not nil \(called
with a prefix arg) then an inverse filter is applied. For further details see
`ecb-methods-filter'."
  (interactive "P")
  (ecb-methods-filter-internal inverse 'regexp))

(defun ecb-methods-filter-function (&optional inverse)
  "Filter the methods-buffer by a function. If INVERSE is not nil \(called
with a prefix arg) then an inverse filter is applied. For further details see
`ecb-methods-filter'."
  (interactive "P")
  (ecb-methods-filter-internal inverse 'function))

(defun ecb-methods-filter-nofilter ()
  "Remove any filter from the Methods-buffer. For further details see
`ecb-methods-filter'."
  (interactive)
  (ecb-methods-filter-internal nil 'no-filter))

(defun ecb-methods-filter-delete-last ()
  "Remove the most recent filter from the Methods-buffer. For further details see
`ecb-methods-filter'."
  (interactive)
  (ecb-methods-filter-internal nil 'delete-last))

(defun ecb-methods-filter (&optional inverse)
  "Apply a filter to the Methods-buffer to reduce the number of entries.
So you get a better overlooking. There are six choices:
- Filter by protection: Just insert the protection you want the Methods-buffer
  being filtered: private, protected or public!
- Filter by regexp: Insert the filter as regular expression.
- Filter by tag-class: You can filter by the tag-classes of current
  major-mode. The available tag-classes come from the variable
  `semantic--symbol->name-assoc-list'. The are normally methods, variables
  etc.
- Filter by current type: In languages which have types like Java or C++ this
  filter displays only the current type and all its members \(e.g. attributes
  and methods). If ECB can not identify the current type in the source-buffer
  or in the methods-window then nothing will be done.
- Filter by a filter-function: Such a function gets two arguments: a tag and
  the source-buffer of this tag. If the tag should be displayed \(i.e. not
  being filtered out) then the function has to return not nil otherwise nil.
- No special filter: This means to display all tags specified with the option
  `ecb-show-tokens'. If currently some of the above filters are applied they
  will be all removed.
- Delete the last added: This removes only the topmost filter-layer, means
  that filter added last.

The protection-, the current-type and the tag-class-filter are only available
for semantic-supported sources.

Be aware that the tag-list specified by the option `ecb-show-tags' is the
basis of all filters, i.e. tags which are excluded by that option will never
be shown regardless of the filter type here!

All tags which match the applied filter\(s) will be displayed in the
Methods-buffer.

If called with a prefix-argument or when optional arg INVERSE is not nil then 
an inverse filter is applied to the Methods-buffer, i.e. all tags which
do NOT match the choosen filter will be displayed in the Methods-buffer!

Per default the choosen filter will be applied on top of already existing
filters. This means that filters applied before are combined with the new
filter. This behavior can changed via the option
`ecb-methods-filter-replace-existing'. But regardless of the setting in
`ecb-methods-filter-replace-existing' applying one of the not-inverse filters
protection, tag-class or current-type always replaces exactly already existing
filters of that type. On the other hand applying more than one inverse
tag-class- or protection-filter can make sense.

Such a filter is only applied to the current source-buffer, i.e. each
source-buffer can have its own tag-filters.

The current active filter will be displayed in the modeline of the
Methods-buffer \[regexp, prot \(= protection), tag-class, function \(=
filter-function)]. If an inverse filter has been applied then this is
signalized by a preceding caret ^. If currently more than 1 filter is applied
then always the top-most filter is displayed in the modeline but the fact of
more than 1 filter is visualized by the number of the filters - included in
parens. You can see all currently applied filters by moving the mouse over the
filter-string in modeline of the Methods-buffer: They will displayed as
help-echo.

See the option `ecb-default-tag-filter' if you search for automatically
applied default-tag-filters."
  (interactive "P")
  (ecb-methods-filter-internal inverse))

(defun ecb-methods-filter-internal (inverse &optional filter-type)
  "FILTER-TYPE has to be one of the symbols 'regexp, 'protection,
'tag-class, 'curr-type, 'function, 'no-filter or 'delete-last."
  (if (save-excursion
        (set-buffer ecb-methods-buffer-name)
        (tree-buffer-empty-p))
      (message "There is nothing to filter in an empty Methods-buffer!")
    (let* ((source-buffer (ecb-get-source-buffer-for-tag-filter))
           (semantic-source-p (save-excursion
                                (set-buffer source-buffer)
                                (ecb--semantic-active-p)))
           (choice (or filter-type
                       (intern (ecb-query-string
                                (format "Apply %sfilter:"
                                        (if inverse "inverse " ""))
                                (delq nil (list "regexp"
                                                (if semantic-source-p "protection")
                                                (if semantic-source-p "tag-class")
                                                (if semantic-source-p "curr-type")
                                                "function" "no-filter" "delete-last"))))
                       'no-filter-specified)))
      (case choice
        (protection
         (ecb-methods-filter-by-prot inverse source-buffer))
        (tag-class
         (ecb-methods-filter-by-tag-class inverse source-buffer))
        (regexp
         (ecb-methods-filter-by-regexp inverse source-buffer))
        (curr-type
         (ecb-methods-filter-by-current-type inverse source-buffer))
        (function
         (ecb-methods-filter-by-function inverse source-buffer))
        (delete-last
         (ecb-methods-filter-apply nil nil nil "" "" source-buffer t))
        (no-filter
         (ecb-methods-filter-apply nil nil nil "" "" source-buffer))
        (otherwise
         (ecb-methods-filter-apply nil nil nil "" "" source-buffer))))))

(defun ecb-methods-filter-apply (filtertype filter inverse filter-type-display
                                            filter-display
                                            source-buffer &optional remove-last)
  "Apply the FILTER of type FILTERTYPE to the buffer SOURCEBUFFER. If INVERSE
is not nil then this filter will be applied inverse. FILTER-TYPE-DISPLAY and
FILTER-DISPLAY are strings and specify how the FILTER of type FILTERTYPE
should be displayed in the modeline of the methods-buffer. If REMOVE-LAST is
not nil then the topmost filter will be removed and all other arguments unless
SOURCE-BUFFER arguments are ignored. Returns t if the filter has been applied
otherwise nil."
  (save-excursion
    (set-buffer source-buffer)
    (if (and (not remove-last)
             (member filtertype '(protection tag-class curr-type))
             (not (ecb--semantic-active-p)))
        (ecb-error "A %s-filter '%s' can only applied to semantic-supported sources!"
                   filtertype filter)))
  (let* ((filter-elem (assoc source-buffer ecb-methods-user-filter-alist))
         (new-filter-spec (and filtertype
                               (list filtertype filter (if inverse 'not 'identity)
                                     filter-type-display filter-display)))
         (replace-all (and (not remove-last)
                           (not (equal ecb-methods-filter-replace-existing 'never))
                           (or (equal ecb-methods-filter-replace-existing 'always)
                               (y-or-n-p "Should the new filter replace existing ones? "))))
         (replace-filter-type (and (not inverse)
                                   (not replace-all)
                                   (not remove-last)
                                   (assoc filtertype (cdr filter-elem))
                                   (member filtertype '(protection tag-class current-type))))
         (filters (or (and replace-filter-type
                           (progn
                             (setcdr filter-elem
                                     (ecb-remove-assoc filtertype (cdr filter-elem)))
                             (append (cdr filter-elem) (list new-filter-spec))))
                      (and remove-last
                           (nreverse (cdr (reverse (cdr filter-elem)))))
                      (and new-filter-spec ;; if nil there should be no filter anymore
                           (if replace-all
                               new-filter-spec ;; just the new filter-spec
                             (append (cdr filter-elem) (list new-filter-spec)))))))
    (if filter-elem
        (setcdr filter-elem filters)
      (if filters
          (setq ecb-methods-user-filter-alist
                (cons (cons source-buffer filters) ecb-methods-user-filter-alist)))))
  (when (buffer-live-p source-buffer)
    (save-excursion
      (set-buffer source-buffer)
      (if (ecb--semantic-active-p)
          ;; For semantic-sources we do not use `ecb-rebuild-methods-buffer)'
          ;; because this would always reparse the source-buffer even if not
          ;; necessary.
          (save-restriction
            (widen)
            (ecb-rebuild-methods-buffer-with-tagcache
             (ecb-fetch-semantic-tags)))
        (ecb-rebuild-methods-buffer-fully)))
    (if (save-excursion
          (set-buffer ecb-methods-buffer-name)
          (tree-buffer-empty-p))
        (progn
          (ecb-methods-filter-apply nil nil nil "" "" source-buffer t)
          (message "ECB has not applied this filter because it would filter out all nodes!")
          nil)
      t)))
        
  
(defun ecb-methods-filter-modeline-prefix (buffer-name sel-dir sel-source)
  "Compute a mode-line prefix for the Methods-buffer so the current filter
applied to the displayed tags is displayed. This function is only for using by
the option `ecb-mode-line-prefixes'."
  (let* ((filters (and sel-source
                       (cdr (assoc (get-buffer (cdr sel-source))
                                   ecb-methods-user-filter-alist))))
         (top-filter-spec (ecb-last filters))
         (filter-type-str (nth 3 top-filter-spec))
         (filter-str (nth 4 top-filter-spec)))
    (if (null top-filter-spec)
        nil ;; no prefix if no filter
      (let ((str (format "[%s%s: %s]"
                         filter-type-str
                         (if (> (length filters) 1)
                             (format "(%d)" (length filters))
                           "")
                         filter-str)))
        (put-text-property
         0 (length str) 'help-echo
         (concat "Filter-Stack: "
                 (mapconcat 'identity
                            (loop for f-elem being the elements of filters using (index f-elem-index)
                                  collect (let ((f-type-str (nth 3 f-elem) )
                                                (f-str (nth 4 f-elem)))
                                            (format "%d. [%s: %s]" (1+ f-elem-index) f-type-str f-str)))
                            ", "))
         str)
        str))))

(defun ecb-default-tag-filter-for-current-source ()
  "Check if for the file of the current buffer a default-tag-filter should be
applied. If yes, then the filters-list of `ecb-default-tag-filter' is returned
otherwise nil."
  (catch 'found
    (dolist (spec ecb-default-tag-filter)
      (let ((m-mode (caar spec))
            (regexp (cdar spec)))
        (if (and (equal major-mode m-mode)
                 (save-match-data
                   (string-match regexp (ecb-buffer-file-name (current-buffer)))))
            (throw 'found (cdr spec)))))
    nil))

(defun ecb-apply-default-tag-filter ()
  "Applies all default-tag-filters specified in `ecb-default-tag-filter' for
the current file."
  (remove-hook 'post-command-hook 'ecb-apply-default-tag-filter)
  (ignore-errors
    (let ((tag-filter-list (ecb-default-tag-filter-for-current-source)))
      (dolist (filter-spec tag-filter-list)
        (let ((filter-apply-fcn
               (case (nth 0 filter-spec)
                 (protection 'ecb-methods-filter-by-prot)
                 (tag-class  'ecb-methods-filter-by-tag-class)
                 (regexp 'ecb-methods-filter-by-regexp)
                 (function 'ecb-methods-filter-by-function)))
              (filter
               (case (nth 0 filter-spec)
                 (protection
                  (typecase (nth 1 filter-spec)
                    (symbol (symbol-name (nth 1 filter-spec)))
                    (string (nth 1 filter-spec))
                    (otherwise
                     (ecb-error "Not a valid tag-filter: %s" (nth 1 filter-spec)))))
                 (tag-class
                  (typecase (nth 1 filter-spec)
                    (symbol (symbol-name (nth 1 filter-spec)))
                    (string (nth 1 filter-spec))
                    (otherwise
                     (ecb-error "Not a valid tag-filter: %s" (nth 1 filter-spec)))))
                 (regexp
                  (typecase (nth 1 filter-spec)
                    (string (nth 1 filter-spec))
                    (otherwise
                     (ecb-error "Not a valid tag-filter: %s" (nth 1 filter-spec)))))
                 (function
                  (typecase (nth 1 filter-spec)
                    (symbol (symbol-name (nth 1 filter-spec)))
                    (string (nth 1 filter-spec))
                    (otherwise
                     (ecb-error "Not a valid tag-filter: %s" (nth 1 filter-spec))))))))
          (funcall filter-apply-fcn
                   (nth 2 filter-spec) (current-buffer) filter))))))

(defun ecb-find-file-hook ()
  "Adds `ecb-apply-default-tag-filter' to `post-command-hook'. This function
removes itself from the `post-command-hook'."
  (add-hook 'post-command-hook 'ecb-apply-default-tag-filter))

;; adding tags to the Methods-buffer 

(defun ecb-add-tags (node tags &optional parent-tag no-bucketize)
  "Add TAGS to the node NODE.
If NO-BUCKETIZE is not nil then TAGS will not bucketized by
`ecb--semantic-bucketize' but must already been bucketized! If not nil
PARENT-TAG is the parent of TAGS."
  (ecb-add-tag-buckets
   node parent-tag
   (if no-bucketize
       tags
     (ecb--semantic-bucketize tags
                              (and parent-tag
                                   (ecb--semantic-symbol->name-assoc-list-for-type-parts)
                                   (equal (ecb--semantic-tag-class parent-tag)
                                          'type))))
   no-bucketize))


(defun ecb-access-order (access)
  "Map ACCESS to a integer-value.
'public     --> 0
'protected  --> 1
'private    --> 3
<all other> --> 2"
  (cond
   ((eq 'public access) 0)
   ((eq 'protected access) 1)
   ((eq 'private access) 3)
   (t  2)))


(defun ecb-sort-tags (sort-method tags)
  (if sort-method
      (let ((tags-by-name
	     (sort tags (function (lambda (a b)
				      (ecb-string< (ecb--semantic-tag-name a)
                                                   (ecb--semantic-tag-name b)))))))
	(if (eq 'access sort-method)
	    (sort tags-by-name
		  (function
		   (lambda (a b)
		     (< (ecb-access-order (ecb--semantic-tag-protection a))
			(ecb-access-order (ecb--semantic-tag-protection b))))))
	  tags-by-name))
    tags))


(defun ecb-add-tag-buckets (node parent-tag buckets &optional no-bucketize)
  "Creates and adds tag nodes to the given node.
The PARENT-TAG is propagated to the functions `ecb-add-tag-bucket' and
`ecb-find-add-tag-bucket'."
  (setq buckets (cons nil buckets))
  (dolist (tag-display (ecb-get-show-tags-list))
    (let* ((type (car tag-display))
           (display (cadr tag-display))
           (sort-method (caddr tag-display)))
      (cond
       ((eq 'parent type)
 	(when (and parent-tag
 		   (eq 'type (ecb--semantic-tag-class parent-tag)))
 	  (let ((parents (ecb-get-tag-parents parent-tag)))
	    (when parents
	      (let* ((name-bucket (ecb-format-bucket-name "Parents"))
                     (name (ecb-tag-generate-node-name name-bucket -1
                                                       "parent-bucket"))
                     (parent-node nil))
                (setq parent-node (ecb-create-node node display
                                                   name
                                                   (list 'ecb-bucket-node
                                                         "Parents"
                                                         'parent)
                                                   ecb-methods-nodetype-bucket))
                (when node
		  (dolist (parent (if sort-method
				      (sort parents 'ecb-string<) parents))
                    (let* ((plain-parent-name
                            (if (ecb-font-lock-tags)
                                (ecb--semantic--format-colorize-text parent 'type)
                              parent))
                           ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: When
                           ;; the next version of the semantic-parsers offer
                           ;; the protection of the inheritance (like possible
                           ;; in C++) then we have to adjust this code and
                           ;; compute the correct icon-name.
                           (parent-name (ecb-tag-generate-node-name plain-parent-name
                                                                    -1
                                                                    "parent-unknown")))
                      (tree-node-new parent-name
                                     ecb-methods-nodetype-externtag
                                     parent t parent-node
                                     (if ecb-truncate-long-names 'end))))))))))
       (t (ecb-find-add-tag-bucket node type display sort-method buckets
                                   parent-tag no-bucketize)))))
  (let ((type-display (ecb-get-tag-type-display t)))
    (dolist (bucket buckets)
      (ecb-add-tag-bucket node bucket (cadr type-display)
                            (caddr type-display) parent-tag no-bucketize))))


(defun ecb-update-after-partial-reparse (updated-tags)
  "Updates the method buffer and all internal ECB-caches after a partial
semantic-reparse. This function is added to the hook
`semantic-after-partial-cache-change-hook'."
  ;; TODO: Currently we get simply the whole cache from semantic (already up
  ;; to date at this time!) and then we rebuild the whole tree-buffer with
  ;; this cache-contents. This is slow for big sources. We should implement
  ;; a mechanism where only the UPDATED-TAGS are used and only this ones are
  ;; updated.

  ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: here we could check if
  ;; UPDATED-TAGS contains only one tag and if this tag contains no childrens
  ;; then we could use the new function `tree-buffer-update-node' to simply
  ;; updating the associated node instead of a full reparse and then full
  ;; tree-buffer-update.
  (if (and (= 1 (length updated-tags))
           (null (ecb-children-tags (car updated-tags))))
      ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: 
      ;; we could update this single node if we can find this node. But this
      ;; could be difficult (or impossible?) because here we only know the new
      ;; semantic-tag but our nodes contain only outdated semantic-tags as
      ;; data so how to find the associated node??!!
      ;; Maybe we could search the node which contaisn the parent-tag of the
      ;; updated tag and then we compute the position p of this tag in the list
      ;; of the children of its parent-tag and then we update that node which
      ;; comes on the same position p in the list of childrens of the
      ;; associated parent-node - hmm, but can we be sure that the sequence of
      ;; children-tags and children-nodes is the same?? probably not because
      ;; the nodes are ordered alphabetically and the tags are are ordered in
      ;; that sequence they are code in the source-buffer! Hmmm...........
      ;; Until this question is solved we must use the full reparse/rebuild
      ;; :-( One possible solution: tempor. ordering the
      ;; semantic-tag-childrens by name and getting the position p of the
      ;; updated tag in that ordered tag-sequence...
      (ecb-rebuild-methods-buffer-with-tagcache (ecb-fetch-semantic-tags))
    (ecb-rebuild-methods-buffer-with-tagcache (ecb-fetch-semantic-tags))))


(defun ecb-semantic-active-for-file (filename)
  "Return not nil if FILENAME is already displayed in a buffer and if semantic
is active for this buffer."
  (and (get-file-buffer filename)
       (save-excursion
         (set-buffer (get-file-buffer filename))
         (ecb--semantic-active-p))))


(defun ecb-update-methods-after-saving ()
  "Updates the methods-buffer after saving if this option is turned on and if
current-buffer is saved."
  (when (and (equal (selected-frame) ecb-frame)
             ecb-auto-update-methods-after-save
             ecb-last-edit-window-with-point
             ;; this prevents updating the method buffer after saving a not
             ;; current buffer (e.g. with `save-some-buffers'), because this
             ;; would result in displaying a method-buffer not belonging to the
             ;; current source-buffer.
             (equal (current-buffer)
                    (window-buffer ecb-last-edit-window-with-point)))
    (ecb-select-source)
    (if (ecb--semantic-active-p)
        (ecb-update-methods-buffer--internal nil nil t)
      (ecb-rebuild-methods-buffer-for-non-semantic))))

(defun ecb-fetch-semantic-tags (&optional full)
  "Get a taglist for current buffer.
If optional arg FULL is not nil or if the `semantic-idle-scheduler-mode' is
not enabled for current source-buffer and the option
`ecb-force-reparse-when-semantic-idle-scheduler-off' is not nil then use
`ecb--semantic-fetch-tags' otherwise use `ecb--semantic-fetch-available-tags'.
The latter always returns just the currently available tags in the
semantic-cache \(reparsing is done completely by the idle-scheduler of
semantic if necessary). The former one triggers immediate parsing if
necessary!"
  (if (or full
          (and (or (not (boundp 'semantic-idle-scheduler-mode))
                   (not semantic-idle-scheduler-mode))
               ecb-force-reparse-when-semantic-idle-scheduler-off)
          )
      (ecb--semantic-fetch-tags)
    (ecb--semantic-fetch-available-tags)))


(defvar ecb-method-buffer-needs-rebuild t
  "This variable is only set and evaluated by the functions
`ecb-update-methods-buffer--internal' and
`ecb-rebuild-methods-buffer-with-tagcache'!")

(defvar ecb-method-buffer-rebuild-allowed-for-invisible-buffers nil
  "This variable is only set and evaluated by the functions
`ecb-update-methods-buffer--internal' and
`ecb-rebuild-methods-buffer-with-tagcache'!")


(defun ecb-update-methods-buffer--internal (&optional scroll-to-top
                                                      rebuild-non-semantic
                                                      full-semantic
                                                      invisible-sourcebuffer-allowed)
  "Updates the methods buffer with the current buffer. The only thing what
must be done is to start the toplevel parsing of semantic, because the rest is
done by `ecb-rebuild-methods-buffer-with-tagcache' because this function is in
the `semantic-after-toplevel-cache-change-hook'.
If optional argument SCROLL-TO-TOP is non nil then the method-buffer is
displayed with window-start and point at beginning of buffer.

If second optional argument REBUILD-NON-SEMANTIC is not nil then non-semantic
sources are forced to be rescanned and reparsed by
`ecb-rebuild-methods-buffer-with-tagcache'. The function
`ecb-rebuild-methods-buffer-for-non-semantic' is the only one settings this
argument to not nil!

If third optional arg FULL-SEMANTIC is not nil then for semantic-sources an
immediate parse-run is triggered - not an idle one! Has no effect for
non-semantic-sources.

If fourth optional argument INVISIBLE-SOURCEBUFFER-ALLOWED is not NIL then we
drive `ecb-rebuild-methods-buffer-with-tagcache' so it does not prevent
rebuilding a not visible source-buffer."
  (when (and (equal (selected-frame) ecb-frame)
             (get-buffer-window ecb-methods-buffer-name))
    ;; Set here `ecb-method-buffer-needs-rebuild' to t so we can see below if
    ;; `ecb-rebuild-methods-buffer-with-tagcache' was called auto. after
    ;; `ecb-fetch-semantic-tags'.
    (setq ecb-method-buffer-needs-rebuild t)
    ;; set this global variable according to the value of
    ;; invisible-sourcebuffer-allowed so that the hook-driven call to
    ;; `ecb-rebuild-methods-buffer-with-tagcache' and the call below as well
    ;; do not prevent rebuilding an invisible source-buffer.
    ;; `ecb-method-buffer-rebuild-allowed-for-invisible-buffers' is evaluated
    ;; by `ecb-rebuild-methods-buffer-with-tagcache' and also reset to nil at
    ;; the end.
    (setq ecb-method-buffer-rebuild-allowed-for-invisible-buffers
          invisible-sourcebuffer-allowed)

    (let ((current-tagcache (and (ecb--semantic-active-p)
                                   ;; if we manually bovinate the buffer we
                                   ;; must widen the source to get all tags.
                                   (save-excursion
                                     (save-restriction
                                       (widen)
                                       (ecb-fetch-semantic-tags full-semantic))))))
      ;; If the `ecb-fetch-semantic-tags' has done no reparsing but
      ;; only used it´s still valid cache then neither the hooks of
      ;; `semantic-after-toplevel-cache-change-hook' nor the hooks in
      ;; `semantic-after-partial-cache-change-hook' are evaluated and
      ;; therefore `ecb-rebuild-methods-buffer-with-tagcache' was not called.
      ;; Therefore we call it here manually.
      ;; `ecb-rebuild-methods-buffer-with-tagcache' is the only function which
      ;; sets `ecb-method-buffer-needs-rebuild' to nil to signalize that a
      ;; "manually" rebuild of the method buffer is not necessary.
      ;;
      ;; `ecb-update-methods-buffer--internal' is called by
      ;; `ecb-basic-buffer-sync' and `ecb-set-selected-source' (depending
      ;; on the method switching to current buffer) which both are called also
      ;; for buffers which are not setup for semantic (e.g. text-,
      ;; tex-buffers). current-tagcache is nil for such buffers so we call the
      ;; rebuilding of the method buffer with a nil cache and therefore the
      ;; method-buffer will be cleared out for such buffers. This is what we
      ;; want! For further explanation see
      ;; `ecb-rebuild-methods-buffer-with-tagcache'...
      (when ecb-method-buffer-needs-rebuild
        ;; the hook was not called therefore here manually
        (ecb-rebuild-methods-buffer-with-tagcache
         current-tagcache
         ;; set the argument NO-UPDATE-SEMANTIC as follows:
         ;; - no-sematic-sources: always nil
         ;; - semantic-sources: If we want a complete rebuild of the
         ;;   methods-buffer based on current tag-list of current buffer
         ;;   (i.e. FULL-SEMANTIC is not nil) then we have to set this arg
         ;;   to nil... if we just want the cached tree-content of the
         ;;   methods-buffer then we set this arg to t.
         (and (ecb--semantic-active-p) (not full-semantic))
         ;; we force a rebuild also for an empty cache because an empty
         ;; cache is returned by `ecb-fetch-semantic-tags' when a file
         ;; exceeds the limit set in the option
         ;; `semantic-idle-scheduler-max-buffer-size'. If we would not force
         ;; a nil-cache rebuild the method-buffer would not be cleared out
         ;; for such files. But for non-semantic-buffers we have to pass nil
         ;; for this argument because otherwise the methods-buffer-contents
         ;; would not be cached - see usage of arg force-nil-cache in
         ;; `ecb-rebuild-methods-buffer-with-tagcache'. This is maybe a
         ;; somehow clumsy and ugly dependence...but it works...maybe we
         ;; should try in future to redesign this....
         (ecb--semantic-active-p)
         rebuild-non-semantic)))
    (when scroll-to-top
      (ecb-exec-in-window ecb-methods-buffer-name
        (ecb-scroll-window (point-min) (point-min))))))


(defvar ecb-tag-tree-cache nil
  "This is the tag-tree-cache for already opened file-buffers. The cache is
a list of cons-cells where the car is the name of the source and the cdr is
the current tag-tree for this source. The cache contains exactly one element
for a certain source.")
(setq ecb-tag-tree-cache nil)

(defun ecb-clear-tag-tree-cache (&optional source-name)
  "Clears either the whole tag-tree-cache \(SOURCE-NAME is nil) or
removes only the tag-tree for SOURCE-NAME from the cache."
  (if (not source-name)
      (setq ecb-tag-tree-cache nil)
    (setq ecb-tag-tree-cache
          (adelete 'ecb-tag-tree-cache source-name))))

(defvar ecb-current-post-processed-tag-table nil
  "This is the current tag-table of the current source-buffer returned by
`ecb-post-process-taglist'. Do not set this variable, only the function
`ecb-rebuild-methods-buffer-with-tagcache' is allowed to do this.")
(make-variable-buffer-local 'ecb-current-post-processed-tag-table)

(defun ecb-get-current-tag-table ()
  "Return the current tag-table of the current source-buffer returned by
`ecb-post-process-taglist'. Use always this function if you just need the
current post-processed tag-table of the current buffer and you do not need or
want rebuilding the Methods-buffer."
  ecb-current-post-processed-tag-table)

(defun ecb-set-current-tag-table (table)
  "Set the current tag-table of the current source-buffer to TABLE. Return
TABLE."
  (setq ecb-current-post-processed-tag-table table))


(defun ecb-methods-get-data-store (key)
  "Get the value for KEY from the tree-buffer-data-store of the Methods-buffer."
  (save-excursion
    (set-buffer ecb-methods-buffer-name)
    (cdr (assoc key (tree-buffer-get-data-store)))))
  

(defun ecb-rebuild-methods-buffer-with-tagcache (updated-cache
                                                 &optional no-update-semantic
                                                 force-nil-cache
                                                 non-semantic-rebuild)
  "Rebuilds the ECB-method buffer after toplevel-parsing by semantic. This
function is added to the hook `semantic-after-toplevel-cache-change-hook'.

If NO-UPDATE-SEMANTIC is not nil then the tags of the ECB-methods-buffer are
not updated with UPDATED-CACHE but the method-buffer is rebuild with these
tags ECB has already cached in it `ecb-tag-tree-cache'. Only relevant for
semantic-parsed sources!

If FORCE-NIL-CACHE is not nil then the method-buffer is even rebuild if
UPDATED-CACHE is nil. Normally a nil cache is ignored if it belongs to a
buffer witch is setup for semantic-parsing; only nil caches for non-semantic
buffers \(like plain text-buffers) are used for updating the method-buffers.
With FORCE-NIL-CACHE the method-buffer is updated with a nil cache too, i.e.
it is cleared.

IF NON-SEMANTIC-REBUILD is not nil then current non-semantic-source is forced
to be rescanned/reparsed and therefore the Method-buffer will be rebuild too."
  ;; The most important function for (re)building the Method-buffer
  (when (and ecb-minor-mode ;; ECB must be active - just for saveness
             ;; ECB-frame must be active
             (equal (selected-frame) ecb-frame)
             ;; the methods-buffer must be visible
             (get-buffer-window ecb-methods-buffer-name)
             ;; current buffer must have a filename.
             (ecb-buffer-file-name (current-buffer))
             ;; the parsed buffer must be displayed in a window within the
             ;; ECB-frame: This prevents the methods-buffer being confused by
             ;; the background-parsing of other files by semantic-idle-timer
             (or ecb-method-buffer-rebuild-allowed-for-invisible-buffers
                 (get-buffer-window (current-buffer) ecb-frame))
             ;; The functions of the hook
             ;; `semantic-after-toplevel-cache-change-hook' are also called
             ;; after clearing the cache to set the cache to nil if a buffer
             ;; is parsed which has no tags. But buffers with no tags are
             ;; really seldom so cause of better performance here we do not
             ;; want rebuilding the method-buffer if the cache is nil but the
             ;; current buffer is set up for semantic-parsing, because the
             ;; real rebuild should be done after the cache is filled again.
             ;; If this hook is called "manually" by
             ;; `ecb-update-methods-buffer--internal' then we do an update
             ;; also for a nil cache if the buffer is not setup for semantic
             ;; (like text-buffers or non-semantic-sources) so we can either
             ;; clear out the method-buffer or fill it with parsing
             ;; information of non-semantic-sources!
             (or updated-cache
                 (not (ecb--semantic-active-p))
                 force-nil-cache))

    ;; no-update-semantic has to be nil for non-semantic-sources!
    (if (not (ecb--semantic-active-p)) (setq no-update-semantic nil))

    ;; the following cache-mechanism MUST use the (buffer-name) instead of
    ;; ecb-path-selected-source because in case of opening a buffer not via
    ;; directory-window but via the standard-mechanism of Emacs this function
    ;; is called via hook BEFORE ecb-path-selected-source is set currently by
    ;; the synchronize-mechanism of ECB. Also if we create a new cache-element
    ;; for the tag-tree we MUST look if in the cache is already an element
    ;; with this key and if we MUST update this cache-element instead of
    ;; always adding a new one to the cache. Otherwise we would get more than
    ;; one cache-element for the same source!.

    ;; with buffer-name we can also handle indirect-buffers
    (let* ((cache-key (buffer-name))
           (cache (assoc cache-key ecb-tag-tree-cache))
           (curr-buff (current-buffer))
           (tag-sync-necessary nil)
           (curr-major-mode major-mode)
           (ezimage-use-images (if (ecb-use-images-for-semantic-tags)
                                   nil
                                 (ecb--ezimage-use-images)))
           (semantic-format-use-images-flag (if (ecb-use-images-for-semantic-tags)
                                                nil
                                              (ecb--semantic-format-use-images-flag)))
           (my-format-face-alist (if (ecb-use-images-for-semantic-tags)
                                     (ecb-remove-assoc 'abstract
                                                       (ecb-remove-assoc 'static
                                                                         (ecb--semantic-format-face-alist)))
                                   (ecb--semantic-format-face-alist)))
           (semantic-format-face-alist my-format-face-alist)
           (semantic-bucketize-tag-class
            (if ecb-methods-separate-prototypes
                (function (lambda (tag)
                            (if (ecb--semantic-tag-prototype-p tag)
                                'prototype
                              (ecb--semantic-tag-class tag))))
              semantic-bucketize-tag-class))
           (semantic-symbol->name-assoc-list-for-type-parts
            (and (ecb--semantic-active-p)
                 (ecb--semantic-symbol->name-assoc-list-for-type-parts)
                 (or (and (null (cdr (assoc 'function
                                            (ecb--semantic-symbol->name-assoc-list-for-type-parts))))
                          (ecb--semantic-symbol->name-assoc-list-for-type-parts))
                     (append (ecb--semantic-symbol->name-assoc-list-for-type-parts)
                             (list (cons 'prototype
                                         (format "%s-prototypes"
                                                 (ecb-string-make-singular
                                                  (cdr (assoc 'function
                                                              (ecb--semantic-symbol->name-assoc-list-for-type-parts)))))))))))
           (semantic-symbol->name-assoc-list
            (and (ecb--semantic-active-p)
                 (ecb--semantic-symbol->name-assoc-list)
                 (or (and (null (cdr (assoc 'function
                                            (ecb--semantic-symbol->name-assoc-list))))
                          (ecb--semantic-symbol->name-assoc-list))
                     (append (ecb--semantic-symbol->name-assoc-list)
                             (list (cons 'prototype
                                         (format "%s-prototypes"
                                                 (ecb-string-make-singular
                                                  (cdr (assoc 'function
                                                              (ecb--semantic-symbol->name-assoc-list)))))))))))
           (curr-semantic-symbol->name-assoc-list semantic-symbol->name-assoc-list)
           new-tree non-semantic-handling)
      
      (if ecb-debug-mode
          (dolist (a-tag updated-cache)
            (ecb-semantic-assert-valid-tag a-tag)))
      
      ;; here we process non-semantic buffers if the user wants this. But only
      ;; if either non-semantic-rebuild is true or no cache exists.
      ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Here we could evaluate an
      ;; option which would define which modes should be parsed by which
      ;; parser (so a user could also parse semantic-enabled-files with
      ;; etags/imenu)
      (when (and ecb-process-non-semantic-files
                 (null updated-cache)                 
                 (not (ecb--semantic-active-p))
                 (ecb-buffer-file-name (current-buffer))
                 (or non-semantic-rebuild (null cache)))
        (setq updated-cache (ignore-errors
                              (ecb-get-tags-for-non-semantic-files)))
        (setq non-semantic-handling
              (if updated-cache 'parsed 'parsed-failed)))

      ;; Now non-semantic-handling is only nil either for semantic-sources or
      ;; for non-semantic-sources if already a cache exists and
      ;; non-semantic-rebuild is nil (i.e. no rescan and rebuild is
      ;; necessary). A not-nil value is only possible for non-semantic-sources
      ;; and is then either 'parsed in case the parsing was successful or
      ;; 'parsed-failed.

      ;; We always make a new tag-tree with updated-cache except for
      ;; - semantic-sources if no-update-semantic is true and already a
      ;;   cache exists. This means this function is NOT called by
      ;;   `semantic-after-toplevel-cache-change-hook'.
      ;; - non-semantic-sources if non-semantic-handling is false, because
      ;;   then no rescan has been performed and updated-cache contains
      ;;   nothing; see comment above.
      (unless (or (and no-update-semantic cache) ;; for semantic-sources
                  (and (not (ecb--semantic-active-p)) ;; for non-semantic-sources
                       (not non-semantic-handling)
                       ;; for clearing out non-semantic-buffers too after
                       ;; killing one; see `ecb-kill-buffer-hook'.
                       (not force-nil-cache)))
        ;; when we update the cache for the current source-buffer then we have
        ;; to resync the current tag at the end.
        (setq tag-sync-necessary t)
        (setq new-tree (tree-node-new-root))
        (if non-semantic-handling
            (if (equal non-semantic-handling 'parsed)
                (ecb-create-non-semantic-tree new-tree updated-cache))
          (ecb-add-tags new-tree
                        (ecb-post-process-taglist updated-cache)))
        (if cache
            (setcdr cache new-tree)
          (setq cache (cons cache-key new-tree))
          (setq ecb-tag-tree-cache (cons cache ecb-tag-tree-cache))))

      ;; Now we either update the method-buffer with a newly created
      ;; tag-tree or with the tag-tree from the cache (with all its
      ;; existing expansions). This work because we store in the cache not a
      ;; copy of the tree but the tree itself, so every expansion of nodes in
      ;; the tree (e.g. by clicking onto the expand-button) expands the nodes
      ;; in the cache!! Cause of this storing the buffer-string too in the
      ;; cache can not work because the buffer-string is a "copy" of the
      ;; tree-buffer and therefore the cached buffer-string can not be updated
      ;; automatically.
      (save-excursion
        (set-buffer ecb-methods-buffer-name)
        ;; we store in the tree-buffer the buffer and the major-mode for which
        ;; the tree-buffer has been build. In no other place the data-store
        ;; will be set!
        (tree-buffer-set-data-store (list (cons 'source-buffer curr-buff)
                                          (cons 'source-major-mode curr-major-mode)
                                          (cons 'semantic-symbol->name-assoc-list
                                                curr-semantic-symbol->name-assoc-list)))
        ;; To ensure cleaning out the methods-display for non-semantic-sources
        ;; when non-semantic-parsing is disabled or such a source is not
        ;; parsable we set cache to an empty tree. As a side effect this fixes
        ;; a bug with XEmacs which has the annoying behavior that even
        ;; semantic-sources are processed twice here when loading such a
        ;; source whereas the first time semantic is not active; without this
        ;; dummy-cache we would run in setting the tree-buffer-root to nil
        ;; which would cause a wrong-argument-type arrayp nil error!
        (unless cache
          (setq cache (cons cache-key (tree-node-new-root))))
        (tree-buffer-set-root (cdr cache))
        (setq ecb-methods-root-node (cdr cache))
        (tree-buffer-update))
      
      (ecb-mode-line-format)

      (if tag-sync-necessary
          (ecb-tag-sync 'force))
      )
    
    ;; Klaus Berndl <klaus.berndl@sdm.de>: after a full reparse all overlays
    ;; stored in the dnodes of the navigation-list now are invalid. Therefore
    ;; we have changed the implementation of ecb-navigate.el from storing
    ;; whole tags to storing buffer and start- and end-markers!
    ;; Just for information also remarked here.
    
    ;; signalize that the rebuild has already be done
    (setq ecb-method-buffer-needs-rebuild nil))

  ;; prevent from allowing rebuilding for invisible buffers unless it is
  ;; explicitly demanded
  (setq ecb-method-buffer-rebuild-allowed-for-invisible-buffers nil))

(defun ecb-save-without-auto-update-methods ()
  (let ((ecb-auto-update-methods-after-save nil))
    (save-buffer)))


(defun ecb-rebuild-methods-buffer-for-non-semantic ()
  "Rebuild the ECB-method-buffer for current source-file of the edit-window.
This function does nothing if point stays not in an edit-window of the
ECB-frame or if current source-file is supported by semantic!

Before rebuilding the Methods-buffer the hook
`ecb-rebuild-non-semantic-methods-before-hook' is called. The Method-buffer is
only rebuild if either the hook contains no function \(the default) or if no
function of this hook returns nil! See `run-hook-with-args-until-failure' for
description how these function are pressed.

The option `ecb-auto-save-before-etags-methods-rebuild' is checked before
rescanning the source-buffer and rebuilding the methods-buffer.

This function is called by the command `ecb-rebuild-methods-buffer'."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame)
             (not (ecb--semantic-active-p))
             (not (member major-mode ecb-non-semantic-exclude-modes))
             (ecb-point-in-edit-window-number))
    (when (run-hook-with-args-until-failure
           'ecb-rebuild-non-semantic-methods-before-hook
           (ecb-buffer-file-name))
      ;; For etags supported non-semantic-sources we maybe have to save the
      ;; buffer first.
      (when (and (buffer-modified-p)
                 (not (and (boundp 'imenu--index-alist)
                           imenu--index-alist))
                 (or (equal ecb-auto-save-before-etags-methods-rebuild t)
                     (member major-mode
                             ecb-auto-save-before-etags-methods-rebuild)))
        ;; to prevent files from being parsed too often we need to temp.
        ;; switch off the auto-method-updating-after-save feature
        (ecb-save-without-auto-update-methods))
      (ecb-update-methods-buffer--internal nil t))))


(defun ecb-rebuild-methods-buffer-for-semantic (&optional clear-cache)
  "Rebuild the ECB-method-buffer for current source-file of the edit-window.
This function does nothing if point stays not in an edit-window of the
ECB-frame or if current source-file is not supported by semantic!

If optional arg CLEAR-CACHE is not nil the semantic-cache is cleared before
reparsing."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame)
             (ecb--semantic-active-p)
             (ecb-point-in-edit-window-number))
    (and clear-cache (ecb--semantic-clear-toplevel-cache))
    (ecb-update-methods-buffer--internal nil nil t)))


(defun ecb-rebuild-methods-buffer (&optional clear-cache)
  "Updates the methods buffer with the current source-buffer.
This means ECB asks the parsing-engine for tags for the current
source-buffer and rebuilds its methods-buffer with the tag-set
returned by the parsing-engine \(semantic for semantic-sources
and imenu rsp. etags for non-semantic-sources). For
semantic-sources this command ignores the
`semantic-idle-scheduler-mode' and therefore also all settings in
`semantic-idle-scheduler-max-buffer-size' so be aware that this
command will also parse huge files!

If called with a prefix-arg \(ie. if optional arg CLEAR-CACHE is not nil) the
complete previous parser-information is deleted before, means no
semantic-cache is used! This argument takes only effect for semantic-sources.

Point must stay in an edit-window otherwise nothing is done. This method is
merely needed in the following situations:

+ To force parsing huge files with slow parsers when such files are excluded
  from parsing via `semantic-idle-scheduler-max-buffer-size' - see above.

+ If an elisp-file is parsed which contains in the middle a defun X where the
  closing ) is missing then semantic parses only until this defun X is reached
  and you will get an incomplete ECB-method buffer. In such a case you must
  complete the defun X and then call this function to completely reparse the
  elisp-file and rebuild the ECB method buffer!

+ For not semantic supported buffers which can be parsed by imenu or etags
  \(see `ecb-process-non-semantic-files') because for these buffers there is
  no built-in auto-rebuild mechanism. For these buffers this command calls
  `ecb-rebuild-methods-buffer-for-non-semantic'.

For non-semantic-sources supported by etags the option
`ecb-auto-save-before-etags-methods-rebuild' is checked before rescanning the
source-buffer and rebuilding the methods-buffer.

If point is in one of the ecb-windows or in the compile-window then this
command rebuids the methods-buffer with the contents of the source-buffer the
last selected edit-window."
  (interactive "P")
  (save-selected-window
    (when (not (ecb-point-in-edit-window-number))
      (let ((ecb-mouse-click-destination 'last-point))
        (ecb-select-edit-window)))
    (ecb-rebuild-methods-buffer-fully clear-cache)))

(defun ecb-rebuild-methods-buffer-fully (&optional clear-cache)
  "Rebuilds the methods buffer with current content of the source-buffer.
This can cause a full reparse! No idle-parsing but immediate parsing because
the tags are fetched by `ecb--semantic-fetch-tags' for semantic-sources!

If called with a prefix-arg \(ie. if optional arg CLEAR-CACHE is not nil) the
complete previous parser-information is deleted before parsing, means no
semantic-cache is used! This argument takes only effect for semantic-sources.

Point must stay in an edit-window otherwise nothing is done."
  (when (ecb-point-in-edit-window-number)
    (if (ecb--semantic-active-p)
        (let ((semantic-idle-scheduler-max-buffer-size 0))
          (ecb-rebuild-methods-buffer-for-semantic clear-cache))
      (ecb-rebuild-methods-buffer-for-non-semantic))))

(defvar ecb-auto-expand-tag-tree-old 'expand-spec)

(defun ecb-toggle-auto-expand-tag-tree (&optional arg)
  "Toggle auto expanding of the ECB-methods-buffer.
With prefix argument ARG, switch on if positive, otherwise switch off. If the
effect is that auto-expanding is switched off then the current value of
`ecb-auto-expand-tag-tree' is saved so it can be used for the next switch on
by this command."
  (interactive "P")
  (let* ((new-value (if (null arg)
                        (if ecb-auto-expand-tag-tree
                            (progn
                              (setq ecb-auto-expand-tag-tree-old
                                    ecb-auto-expand-tag-tree)
                              nil)
                          ecb-auto-expand-tag-tree-old)
                      (if (<= (prefix-numeric-value arg) 0)
                          (progn
                            (if ecb-auto-expand-tag-tree
                                (setq ecb-auto-expand-tag-tree-old
                                      ecb-auto-expand-tag-tree))
                            nil)
                        (or ecb-auto-expand-tag-tree
                            ecb-auto-expand-tag-tree-old)))))
    (setq ecb-auto-expand-tag-tree new-value)
    (message "Auto. expanding of the methods-buffer is switched %s \(Value: %s\)."
             (if new-value "on" "off")
             new-value)))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Define this with define-overload
;; when the cedet 1.0 is stable - then we can remove the semantic 1.4 support
;; - but first when cedet 1.0 is also available as XEmacs-package!
(defun ecb-get-real-curr-tag ()
  "Get the \"real\" current tag. This will be in most cases the tag returned
by `ecb--semantic-current-tag' but there are exceptions:

- If the current-tag is an argument-tag of a function-tag then we are not
  interested in this argument-tag but in its parent-tag which is the
  function-tag the argument belongs.
- If the current-tag is a label-tag then we are interested in the type-tag
  which contains this label \(e.g. usefull in c++ and the labels public,
  protected and private)."
  (let* ((tagstack (reverse (ecb--semantic-find-tag-by-overlay)))
         (curr-tag (car tagstack))
         (next-tag (car (cdr tagstack)))
         )
    (if (or (and (equal (ecb--semantic-tag-class curr-tag) 'variable)
                 (equal (ecb--semantic-tag-class next-tag) 'function)
                 (member curr-tag
                         (ecb--semantic-tag-function-arguments next-tag)))
            (equal (ecb--semantic-tag-class curr-tag) 'label))
        (setq curr-tag next-tag))
    curr-tag))


(defun ecb-try-highlight-tag (highlight-tag curr-tag table)
  "First we try to expand only the absolute needed parts of the tree-buffer to
highlight the tag HIGHLIGHT-TAG - this means we recursively go upstairs the
ladder of types the current tag belongs to. If this has still no success then
we return nil otherwise true \(the HIGHLIGHT-TAG is highlighted).

If called from program: HIGHLIGHT-TAG is the tag to highlight, CURR-TAG has to
be equal to HIGHLIGHT-TAG and TABLE must be the current tag-table of the
current buffer."
  (let* ((type-tag (and curr-tag
                        (ecb-get-type-tag-of-tag curr-tag table t)))
         (bucket-data
          (and (not type-tag)
               (list 'ecb-bucket-node
                     (cdr (assoc (or (and (ecb--semantic-tag-prototype-p highlight-tag)
                                          'prototype)
                                     (ecb--semantic-tag-class highlight-tag))
                                 (ecb-methods-get-data-store
                                  'semantic-symbol->name-assoc-list)))
                     (ecb--semantic-tag-class highlight-tag))))
         (type-node nil))
    (or (and curr-tag
             (ecb-exec-in-window ecb-methods-buffer-name
               (or (tree-buffer-highlight-node-by-data/name
                    highlight-tag nil nil
                    (equal ecb-highlight-tag-with-point 'highlight))
                   ;; If the tag could not be highlighted and if there is no
                   ;; containing type for this tag then this tag is probably
                   ;; contained in a toplevel bucket. Then we search the
                   ;; bucket-node for the tag if this tag-class is specified
                   ;; as expanded or collapsed (ie not flattened or hidden
                   ;; because in these cases no bucket would exist). If we
                   ;; find the bucket-node then we expand only this
                   ;; bucket-node and try highlighting again.
                   (when (and highlight-tag
                              bucket-data ;; tag has no containing type
                              ;;(member (car (cdr (assoc (ecb--semantic-tag-class highlight-tag)
                              ;;                         (ecb-get-show-tags-list))))
                              ;;        '(expanded collapsed))
                              (or (equal ecb-auto-expand-tag-tree 'all)
                                  (member (ecb--semantic-tag-class highlight-tag)
                                          (ecb-normalize-expand-spec
                                           ecb-methods-nodes-expand-spec))))
                     (let ((bucket-node
                            (tree-buffer-search-displayed-node-list
                             (function (lambda (node)
                                         (if (and (tree-buffer-node-data-equal-p
                                                   (tree-node->data node)
                                                   bucket-data)
                                                  (eq (tree-buffer-get-root)
                                                      (tree-node->parent node)))
                                             node))))))
                       (when bucket-node
                         (ecb-expand-methods-node-internal
                          bucket-node
                          100
                          (equal ecb-auto-expand-tag-tree 'all)
                          nil t)
                         (tree-buffer-highlight-node-by-data/name
                          highlight-tag nil nil
                          (equal ecb-highlight-tag-with-point 'highlight)))))
                   ;; The node representing HIGHLIGHT-TAG could not be
                   ;; highlighted by `tree-buffer-highlight-node-by-data/name' -
                   ;; probably it is invisible. Let's try to make expand its
                   ;; containing type (if there is any) and then highlighting
                   ;; again.
                   (when (and highlight-tag
                              type-tag
                              (or (equal ecb-auto-expand-tag-tree 'all)
                                  (member (ecb--semantic-tag-class highlight-tag)
                                          (ecb-normalize-expand-spec
                                           ecb-methods-nodes-expand-spec))))
                     (setq type-node (tree-buffer-find-displayed-node-by-data/name type-tag))
                     (when type-node
                       (ecb-expand-methods-node-internal
                        type-node
                        ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Maybe we
                        ;; should not immediately fully expand the type but
                        ;; in two steps:
                        ;; 1. We expand only the first level of the type and
                        ;;    check if the tag is contained in a flattended
                        ;;    bucket. If yes we will have success and are
                        ;;    finished because the tag must be contained in
                        ;;    the type-tag. If no we go to step 2.
                        ;; 2. because the tag MUST be contained in that
                        ;;    type-node we now know that it must be contained
                        ;;    in a collapsed bucket-subnode of this
                        ;;    type-node. So we have to expand this
                        ;;    bucket-subnode (similar to the mechanism above)
                        ;;    and then try again...
                        100
                        (equal ecb-auto-expand-tag-tree 'all)
                        nil t)
                       (tree-buffer-highlight-node-by-data/name
                        highlight-tag nil nil
                        (equal ecb-highlight-tag-with-point 'highlight))
                       )))))
        (if curr-tag
            (ecb-try-highlight-tag highlight-tag type-tag table)))))

(defvar ecb-tag-sync-do-nothing nil
  "Only set by `ecb-display-tag' and only evaluated by `ecb-tag-sync'")

;; This approach only expands the needed parts of the tree-buffer when
;; the current-tag is not visible as node and not the whole tree-buffer.

(defecb-autocontrol/sync-function ecb-tag-sync ecb-methods-buffer-name nil nil
  "Synchronizing tag-display of the methods-buffer with current point."
  (if nil ;; ecb-tag-sync-do-nothing
      ;; user has selected a tag via the Methods-window so there is nothing
      ;; to sync - we must prevent from syncing here because in some modes
      ;; the point stays after a click outside of the selected tag (see
      ;; `ecb-tag-visit-post-actions') and if we would sync then in the
      ;; methods-buffer the selected tag will be unhighlighted and the
      ;; surrounding one will be highlighted (e.g. java the class of the
      ;; tag). But we must reset this flag so the resync-mechanism runs
      ;; next time...
      ;; Klaus Berndl <klaus.berndl@sdm.de>: Now all functions of
      ;; ecb-tag-visit-post-actions are forbidden to put the point outside
      ;; of the tag-boundaries. Therefore we can now remove this mechanism
      ;; so now synching can take place also after a click. But i let the
      ;; code in because im not at 100% sure if there are other needs in ECB
      ;; which need this mechanism - but for now we can disable it.... ;-)
      (setq ecb-tag-sync-do-nothing nil)
    (when ecb-highlight-tag-with-point
      (let ((curr-tag (ecb-get-real-curr-tag)))
        (when (or force (not (equal ecb-selected-tag curr-tag)))
          (setq ecb-selected-tag curr-tag)
          (if (null curr-tag)
              (ecb-exec-in-window ecb-methods-buffer-name
                ;; If there is no tag to highlight then we remove the
                ;; highlighting
                (tree-buffer-highlight-node-by-data/name nil)
                (if (equal ecb-auto-expand-tag-tree-collapse-other 'always)
                    ;; If this option is t (means always) we collapse also
                    ;; when point is not on a tag!
                    (ecb-expand-methods-node-internal
                     (tree-buffer-get-root)
                     -1
                     (equal ecb-auto-expand-tag-tree 'all)
                     nil t))
                )
            ;; Maybe we must first collapse all so only the needed parts are
            ;; expanded afterwards. Klaus Berndl <klaus.berndl@sdm.de>: Is it
            ;; necessary to update the tree-buffer after collapsing? IMO yes,
            ;; because otherwise we set the expansion-state of the tree-buffer
            ;; to all collapsed and if we find really nothing to highlight and
            ;; do also no node-expanding (which would update the tree-buffer)
            ;; then we have an inconsistent state - would be probably very
            ;; seldom but could be - so let us be somehow paranoid ;-)
            (if ecb-auto-expand-tag-tree-collapse-other
                (ecb-exec-in-window ecb-methods-buffer-name
                  (when (and curr-tag
                             (or (equal ecb-auto-expand-tag-tree 'all)
                                 (member (ecb--semantic-tag-class curr-tag)
                                         (ecb-normalize-expand-spec
                                          ecb-methods-nodes-expand-spec))))
                    (ecb-expand-methods-node-internal
                     (tree-buffer-get-root)
                     -1
                     (equal ecb-auto-expand-tag-tree 'all)
                     nil t))))
            ;; First we try to expand only the absolute needed parts - this
            ;; means we go upstairs the ladder of types the current tag
            ;; belongs to. If there is no containing type then we try to
            ;; expand only the containing toplevel bucket. If this has no
            ;; success then we expand the full tree-buffer and try it again.
            (when (not (ecb-try-highlight-tag curr-tag curr-tag
                                              (ecb-get-current-tag-table)))
              ;; The node representing CURR-TAG could not be highlighted by
              ;; `tree-buffer-highlight-node-by-data/name' - probably it is still
              ;; invisible. Let's try to make visible all nodes and then
              ;; highlighting again.
              (ecb-exec-in-window ecb-methods-buffer-name
                (when (and curr-tag
                           (or (equal ecb-auto-expand-tag-tree 'all)
                               (member (ecb--semantic-tag-class curr-tag)
                                       (ecb-normalize-expand-spec
                                        ecb-methods-nodes-expand-spec))))
                  (ecb-expand-methods-node-internal
                   (tree-buffer-get-root)
                   100 ;; this should be enough levels ;-)
                   (equal ecb-auto-expand-tag-tree 'all)
                   nil t)
                  (tree-buffer-highlight-node-by-data/name
                   curr-tag nil nil (equal ecb-highlight-tag-with-point 'highlight)))
                ))))))))


(defun ecb-string-make-singular (string)
  (if (equal (aref string (1- (length string))) ?s)
      (substring string 0 (1- (length string)))
    string))


(defun ecb-methods-node-get-semantic-type (node)
  (cond ((= ecb-methods-nodetype-bucket (tree-node->type node))
         (nth 2 (tree-node->data node)))
        ((= ecb-methods-nodetype-tag (tree-node->type node))
         (ignore-errors (ecb--semantic-tag-class (tree-node->data node))))
        (t nil)))


(defun ecb-expand-methods-nodes (&optional force-all)
  "Set the expand level of the nodes in the ECB-methods-buffer.
This command asks in the minibuffer for an indentation level LEVEL. With this
LEVEL you can precisely specify which level of nodes should be expanded. LEVEL
means the indentation-level of the nodes.

A LEVEL value X means that all nodes with an indentation-level <= X are
expanded and all other are collapsed. A negative LEVEL value means all visible
nodes are collapsed.

Nodes which are not indented have indentation-level 0!

Which node-types are expanded \(rsp. collapsed) by this command depends on
the options `ecb-methods-nodes-expand-spec' and
`ecb-methods-nodes-collapse-spec'! With optional argument FORCE-ALL all tags
will be expanded/collapsed regardless of the values of these options.

Examples:
- LEVEL = 0 expands only nodes which have no indentation itself.
- LEVEL = 2 expands nodes which are either not indented or indented once or
  twice
- LEVEL ~ 10 should normally expand all nodes unless there are nodes which
  are indented deeper than 10.

Note 1: This command switches off auto. expanding of the method-buffer if
`ecb-expand-methods-switch-off-auto-expand' is not nil. But it can be switched
on again quickly with `ecb-toggle-auto-expand-tag-tree' or \[C-c . a].

Note 2: All this is only valid for file-types parsed by semantic. For other
file types which are parsed by imenu or etags \(see
`ecb-process-non-semantic-files') FORCE-ALL is always true!"
  (interactive "P")
  (let* ((first-node (save-excursion
                       (goto-char (point-min))
                       (tree-buffer-get-node-at-point)))
         (level (ecb-read-number
                 "Expand indentation-level: "
                 (if (and first-node
                          (tree-node->expandable first-node)
                          (tree-node->expanded first-node))
                     -1
                   10))))
    ;; here we should switch off autom. expanding tag-tree because otherwise
    ;; our expanding to a certain level takes no effect because if the current
    ;; tag in the edit-buffer would be invisible afterwards (after the
    ;; expanding/collapsing) then immediately the tag would be autom.
    ;; expanded to max level...
    (when ecb-expand-methods-switch-off-auto-expand
      (ecb-toggle-auto-expand-tag-tree -1))
    (ecb-expand-methods-node-internal (save-excursion
                                         (set-buffer ecb-methods-buffer-name)
                                         (tree-buffer-get-root))
                                       level force-all t t)))

(defun ecb-expand-methods-node-internal (node level
                                               &optional force-all
                                               resync-tag update-tree-buffer)
  "Set the expand level of NODE and its subnodes in the ECB-methods-buffer.

If NODE is equal to the root-node of the methods-tree-buffer then this
function will be called for each of the root-children. Otherwise it will only
expand/collaps NODE.

For a description of LEVEL see `tree-buffer-expand-node' and for a description
of FORCE-ALL see `ecb-expand-methods-nodes'.

If RESYNC-TAG is not nil then after expanding/collapsing the methods-buffer
is resynced to the current tag of the edit-window.

If UPDATE-TREE-BUFFER is not nil then the methods-tree-buffer will be updated
after the expansion.

Note: All this is only valid for file-types parsed by semantic. For other file
types which are parsed by imenu or etags \(see
`ecb-process-non-semantic-files') FORCE-ALL is always true!"
  ;; for buffers which are not parsed by semantic we always set force-all to
  ;; t! We "misuse" (ecb-methods-get-data-store
  ;; 'semantic-symbol->name-assoc-list) to decide if a buffer is parsed by
  ;; semantic or not because only semantic-parsed buffers can have a value
  ;; not nil!
  (setq force-all
        (if (not (ecb-methods-get-data-store 'semantic-symbol->name-assoc-list))
            t
          force-all))
  (ecb-exec-in-window ecb-methods-buffer-name
    (let ( ;; normalizing the elements of `ecb-methods-nodes-expand-spec'
          ;; and `ecb-methods-nodes-collapse-spec'.
          (norm-expand-types (ecb-normalize-expand-spec
                              ecb-methods-nodes-expand-spec))
          (norm-collapse-types (ecb-normalize-expand-spec
                                ecb-methods-nodes-collapse-spec))
          (node-list (if (equal node (tree-buffer-get-root))
                         (tree-node->children (tree-buffer-get-root))
                       (list node))))
      (dolist (node node-list)
        (tree-buffer-expand-node
         node
         level
         (and (not force-all)
              (function (lambda (node current-level)
                          (or (equal norm-expand-types 'all)
                              (member (ecb-methods-node-get-semantic-type node)
                                      norm-expand-types)))))
         (and (not force-all)
              (function (lambda (node current-level)
                          (or (equal norm-collapse-types 'all)
                              (member (ecb-methods-node-get-semantic-type node)
                                      norm-collapse-types)))))))
      (if update-tree-buffer
          (tree-buffer-update)
        (ecb-scroll-window (point-min) (point-min)))))

  ;; we want resync the new method-buffer to the current tag in the
  ;; edit-window.
  (if resync-tag (ecb-tag-sync 'force)))



(defun ecb-normalize-expand-spec (spec)
  (if (equal 'all spec)
      'all
    (mapcar (function (lambda (elem)
                        (intern
                         (downcase (ecb-string-make-singular
                                    (symbol-name elem))))))
            spec)))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: modify this so it can be used by
;; ecb-find-external-tag-functions - currently not used
(defun ecb-semantic-tag-external-class-default (tag)
  "Return a list of real tags that faux TAG might represent.
See `semantic-tag-external-class' for details."
  (if (and (fboundp 'semanticdb-minor-mode-p)
	   (semanticdb-minor-mode-p))
      (let* ((semanticdb-search-system-databases nil)
	     (m (semanticdb-find-tags-by-class 
		 (semantic-tag-class tag)
		 (semanticdb-find-tags-by-name (semantic-tag-name tag)))))
	(semanticdb-strip-find-results m))
    ;; Presumably, if the tag is faux, it is not local.
    nil
    ))


;; ;; semantic 1.X does not have this
(silentcomp-defvar semanticdb-search-system-databases)

(defun ecb-semanticdb-find-result-nth-with-file (result n)
  "In RESULT, return the Nth search result.
This is a 0 based search result, with the first match being element 0. Returns
a cons cell with car is the searched and found tag and the cdr is the
associated full filename of this tag. If the search result is not associated
with a file, then the cdr of the result-cons is nil."
  (let ((result-nth (ecb--semanticdb-find-result-nth result n)))
    (if (and (car result-nth)
             (ecb--semantic-tag-with-position-p (car result-nth))
             (cdr result-nth))
        (cons (car result-nth)
              (ecb--semanticdb-full-filename (cdr result-nth)))
      (cons (car result-nth) nil))))

(defun ecb-semanticdb-get-type-definition-list (external-tag)
  "Search for the definition of the type with name of EXTERNAL-TAG.
The search is performed viy semanticdb.
`semanticdb-search-system-databases' is taken into account.
Return-value is either nil \(if no positioned tag can be found
for external-tag or a positioned semantic-tag for the
type-definition of EXTERNAL-TAG."
  (when (and (featurep 'semanticdb) (ecb--semanticdb-minor-mode-p))
    ;; With semantic 2.X we do a full featured database-search.
    (let* ((search-result (ecb--semanticdb-find-tags-by-name
                           (ecb--semantic-tag-name external-tag)))
           (result-tags (and search-result
                             (ecb--semanticdb-strip-find-results search-result)))
           (type-tag-numbers nil))
      (when (and result-tags
                 ;; some paranoia
                 (= (length result-tags)
                    (ecb--semanticdb-find-result-length search-result)))
        ;; First we check which tags in the stripped search-result
        ;; (result-tags) are types with positions (means associated with a
        ;; file) and collect their sequence-positions in type-tag-numbers.
        (dotimes (i (length result-tags))
          (if (and (equal (ecb--semantic-tag-class (nth i result-tags))
                          'type)
                   (ecb--semantic-tag-with-position-p (nth i result-tags)))
              (setq type-tag-numbers
                    (cons i type-tag-numbers))))
        (setq type-tag-numbers (nreverse type-tag-numbers))
        ;; Now we get for each element in type-tag-numbers the related
        ;; filename (where the tag is defined) and collect them in an alist
        ;; where each element is a cons-cell where car is the filename and
        ;; cdr is the tag in this file. Especially with scoped languages
        ;; like C++ or Java a type with the same name can be defined in more
        ;; than one file - each of these files belonging to another
        ;; package/library.
        (delq nil
              (mapcar (function (lambda (n)
                                  (let ((r (ecb-semanticdb-find-result-nth-with-file
                                            search-result n)))
                                    (if (and (cdr r)
                                             (stringp (cdr r))
                                             (ecb-file-readable-p (cdr r)))
                                        (cons (cdr r) (car r))))))
                      type-tag-numbers))))))

(defun ecb-search-tag-by-semanticdb (external-tag)
  "Uses semanticdb to search for the type-definition of EXTERNAL-TAG.
Return exactly one semantic tag for the type-definition of
EXTERNAL-TAG. If more than one definition have been found then
the user has to make a choice on file-basis.

This function is for usage with `ecb-find-external-tag-functions.'"
  (let ((type-definition-alist (ecb-semanticdb-get-type-definition-list
                                external-tag))
        (result-elem))
    (when type-definition-alist
      ;; if we got more than one file for EXTERNAL-TAG then the user has to
      ;; choose one.
      (setq result-elem
            (if (> (length type-definition-alist) 1)
                (assoc (ecb-offer-choices "Select a definition-file: "
                                          (mapcar #'car type-definition-alist))
                       type-definition-alist)
              (car type-definition-alist)))
      ;; we add the filename to the tag, then all needed informations are
      ;; within the tag and afterwards `semantic-tag-buffer can do its work
      (semantic--tag-put-property (cdr result-elem)
                                  :filename
                                  (car result-elem)))))

(defun ecb-search-tag-by-semantic-analyzer (tag)
  "Calculate the scope at current point and search for a type with name of TAG.

Return either a positioned semantic-tag for the found
type-definition or nil if nothing is found. This mechanism uses
the semantic-analyzer. Therefore it will work at its best if all
nneded customizations for the semantic analyzer have been done.
\(See the manual of the semantic analyzer for how to customizing
it).

This function is for usage with `ecb-find-external-tag-functions'."
  (let* ((scope (ecb--semantic-calculate-scope)))
    (when scope
      (ecb--semantic-analyze-find-tag (ecb--semantic-tag-name tag)
                                      'type scope))))

(defun ecb-next-tag-parent-node (node)
  "Go upward in the parent-hierarchy of NODE and return next node holding a tag."
  (let ((parent-node (tree-node->parent node)))
    (cond ((null parent-node) nil)
          ((= (tree-node->type parent-node) ecb-methods-nodetype-tag)
           parent-node)
          (t (ecb-next-tag-parent-node parent-node)))))

;; (defun klaus-ecb-test-ext-function (tag)
;;   (progn
;;     (find-file "~/klaus.cc")
;;     t))

(defecb-tree-buffer-callback ecb-method-clicked ecb-methods-buffer-name select
                             (&optional no-post-action additional-post-action-list)
  "Does all necessary when a user clicks onto a node in the methods-buffer."
  (if shift-mode
      (ecb-mouse-over-method-node node nil nil 'force))
  ;; First of all we must highlight the tag
  (tree-buffer-highlight-node-by-data/name (tree-node->data node))
  (if (= (tree-node->type node) ecb-methods-nodetype-bucket)
      ;; Type ecb-methods-nodetype-bucket = a title of a group
      ;; Just expand/collapse the node
      (progn
        (tree-node-toggle-expanded node)
        ;; Update the tree-buffer with optimized display of NODE
        (tree-buffer-update node))
    ;; from now on we have as node-data either semantic-tags or plain strings
    ;; holding typenames (e.g. for parents of type-tags).
    ;; we normalize these two data-possibilities by converting plain strings in
    ;; semantic-tags of class 'type ==> we can operate always on semantic-tags!
    (let ((node-tag (if (ecb--semantic-tag-p (tree-node->data node))
                        (tree-node->data node)
                      (ecb--semantic-tag (tree-node->data node) 'type)))
          (node-type (tree-node->type node))
          ;; destination-source will be later to a source in the sense of
          ;; `ecb-source-make'
          (destination nil))
      ;; first we process include-tags and try to find the related
      ;; included file
      (unless destination
        (when (equal 'include (ecb--semantic-tag-class node-tag))
          (save-excursion
            (set-buffer (ecb-path-selected-source 'buffer))
            (let ((file (ecb--semantic-dependency-tag-file node-tag)))
              (when (and file (ecb-file-exists-p file))
                (setq destination (list (ecb-source-make file)
                                        nil)))))))

      ;; now we process faux-tags and external tags like parent-types
      (unless destination
        (when (or (= node-type ecb-methods-nodetype-externtag)
                  (and (ecb--semantic-tag-p node-tag)
                       (ecb--semantic-tag-faux-p node-tag)))
          (save-excursion
            ;; tag-to-go:
            ;; - In case of a faux-tag it's the first child of the faux-tag -
            ;;   there must be at least one adopted child-tag because
            ;;   otherwise we would not have the faux-tag
            ;; - In case of an external tag its the external tag itself
            ;;
            ;; parent-tag-of-tag-to-go:
            ;; - In case of a faux-tag it's nil because the faux-tag is
            ;;   virtual and doesn't exists in the source-buffer; so it can
            ;;   not have an enclosing parent-tag
            ;; - In case of an external tag it's the next upward enclosing tag
            ;;   - we can get this be climbing the node-tree upwards until we
            ;;     find a node with node-type tag            
            (let ((tag-to-go (if (ecb--semantic-tag-faux-p node-tag)
                                 (car (ecb--semantic-tag-children-compatibility
                                       node-tag t))
                               ;; in case of an external tag
                               node-tag))
                  (parent-tag-of-tag-to-go (and (= node-type ecb-methods-nodetype-externtag)
                                                (tree-node->data (ecb-next-tag-parent-node node))))
                  found)
              (when tag-to-go
                (ecb--semantic-go-to-tag tag-to-go parent-tag-of-tag-to-go)
                ;; now we stay at the right location (child of the faux-tag
                ;; rsp. the clicked parent-pseudo-tag)
                ;; Now we search for the clicked node-tag via the functions
                ;; defined in `ecb-find-external-tag-functions' until one of
                ;; them returns not nil
                (setq found (catch 'foundit
                              (dolist (f (append (cdr (assoc major-mode ecb-find-external-tag-functions))
                                                 (cdr (assoc 'default ecb-find-external-tag-functions))))
                                (let ((f-result (funcall f node-tag)))
                                  (if f-result (throw 'foundit f-result))))
                              nil))
                (setq destination
                      (cond ((and (ecb--semantic-tag-p found)
                                  (ecb--semantic-tag-with-position-p found))
                             (list (ecb-source-make (ecb-buffer-file-name
                                                     (ecb-semantic-tag-buffer found))
                                                    (ecb-semantic-tag-buffer found))
                                   found))
                            ;; founded and already displayed!
                            ((equal found t) t)
                            ;; in all other cases we assess the result as not found
                            (t nil))))))))

      ;; now we process positionless tags like slots of an elisp-defclass...
      ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: maybe in the future..

      (unless destination
        (when (and (ecb--semantic-tag-p node-tag)
                   (ecb--semantic-tag-with-position-p node-tag)
                   ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Currently we
                   ;; comment out this check because in some cases the tag is
                   ;; positioned only with start and end but without a
                   ;; buffer-info...
                   ;; Normally all these tags must reside in the current
                   ;; selected source-buffer so we can switch off this
                   ;; check...
                   ;; Maybe in the future we switch it on when semantic is
                   ;; able to deal with indirect buffers...
;;                    (equal (ecb-semantic-tag-buffer node-tag)
;;                           (ecb-path-selected-source 'buffer))
                   )
          (setq destination (list (ecb-source-make (ecb-buffer-file-name
                                                    (ecb-path-selected-source 'buffer))
                                                   (ecb-path-selected-source 'buffer))
                                  node-tag))))

      (when (and destination (listp destination))
        (ecb-display-tag (nth 0 destination) ;; the source
                         (nth 1 destination) ;; a tag or nil
                         (ecb-get-edit-window
                          (ecb-combine-ecb-button/edit-win-nr ecb-button edit-window-nr))
                         no-post-action
                         (if (and shift-mode
                                  (not (member 'ecb-tag-visit-narrow-tag
                                               additional-post-action-list)))
                             (cons 'ecb-tag-visit-narrow-tag
                                   additional-post-action-list)
                           additional-post-action-list))))))


(defun ecb-tag-visit-smart-tag-start (tag)
  "Go to the real tag-name of TAG in a somehow smart way.
This is especially needed for languages like c++ where a often used style is
like:
    void
    ClassX::methodM\(arg1...)
    \{
      ...
    \}
Here we want to jump to the line \"ClassX::...\" and not to line \"void\".

Returns point."
  (goto-char (ecb-semantic-tag-start tag))
  (beginning-of-line)
  ;; We must bind the search to the max. of either the end-of-line-pos or the
  ;; tag-end, because in some languages the tag-name displayed in the
  ;; Methods-buffer and returned by the parsing engine can not be found in the
  ;; source-buffer. Perl is an example, because here imenu returns tag-names
  ;; like <package>::<function> (e.g. bigfloat::norm) but in the source buffer
  ;; only "sub <function>" (e.g. "sub norm...") can be found. So to avoid
  ;; finding a wrong position in the source-buffer (e.g. if the tag-name
  ;; returned by imenu is mentioned in a comment somewhere) we bind the
  ;; search.
  (search-forward (ecb--semantic-tag-name tag)
                  (max (ecb-line-end-pos)
                       (ecb-semantic-tag-end tag))
                  t)
  (beginning-of-line-text)
  (point))


(defun ecb-start-of-tag-doc (tag)
  "If TAG has an outside documentation located direct before TAG then
return the start of the documentation. Otherwise return nil"
  ;; there can be an error if tag has no documentation - e.g.
  ;; in elisp
  (let ((comment (ignore-errors (ecb--semantic-documentation-for-tag tag
                                                                     'flex))))
    (if (and comment
             (not (stringp comment)))
        ;; probably we have a semantic flex-object
        (ecb--semantic-lex-token-start comment))))


(defun ecb-tag-visit-display-doc-start (tag)
  "Display the beginning of the documentation of TAG if defined outside.
This means move the window-start of current edit-window so the whole
documentation is visible. But points still stays onto the tag-start!
This is useful especially for languages like Java where the documentation
resides direct before the TAG in Javadoc format.
If the documentation is located within TAG then nothing is done.

If this function is set in `ecb-tag-visit-post-actions' then it's strongly
recommended not to add `ecb-tag-visit-recenter' or
`ecb-tag-visit-recenter-top' after this this function!

This action is not recommended for sources of type TeX, texinfo etc. So you
should not add this action to the 'default element of
`ecb-tag-visit-post-actions'!

Returns current point."
  (let ((tag-doc-start  (ecb-start-of-tag-doc tag)))
    (when (and tag-doc-start
               (not (pos-visible-in-window-p tag-doc-start)))
      ;; tag-doc-start must be above the current window-start so we must must
      ;; reset the window-start
      (set-window-start (selected-window) tag-doc-start))
;;      (goto-char tag-doc-start))
    (point)))

;; for backward-compatibility
(defalias 'ecb-tag-visit-goto-doc-start 'ecb-tag-visit-display-doc-start)

(defvar ecb-unhighlight-hook-called nil
  "This mechanism is necessary because tree-buffer creates for mouse releasing a
new nop-command \(otherwise the cursor jumps back to the tree-buffer).")


(defun ecb-unhighlight-tag-header ()
  (let ((key (ecb-event-to-key last-input-event)))
    (when (not (or (and (equal key 'mouse-release)
                        (not ecb-unhighlight-hook-called))
                   (equal key 'mouse-movement)))
      (ecb-overlay-delete ecb-method-overlay)
      (remove-hook 'pre-command-hook 'ecb-unhighlight-tag-header)))
  (setq ecb-unhighlight-hook-called t))


(defun ecb-tag-visit-highlight-tag-header (tag)
  "Highlights line where `ecb-tag-visit-smart-tag-start' puts point for
TAG. Returns current point"
  (save-excursion
    (ecb-tag-visit-smart-tag-start tag)
    (ecb-overlay-move ecb-method-overlay
                      (ecb-line-beginning-pos)
                      (ecb-line-end-pos)
                      (current-buffer)))
  (setq ecb-unhighlight-hook-called nil)
  (add-hook 'pre-command-hook 'ecb-unhighlight-tag-header)
  (point))


(defun ecb-display-tag (source tag &optional window
                               no-tag-visit-post-actions
                               additional-post-action-list)
  "Display SOURCE and go to TAG.
SOURCE is a source in the sense of `ecb-source-make'. TAG is either nil \(then
just the SOURCE is displayed) or a semantic-tag. If it is a positioned tag
then this function goes to this TAG in SOURCE.
If optional arg WINDOW is nil then the current window is used otherwise the
window-object WINDOW contains.

If NO-TAG-VISIT-POST-ACTIONS is not nil then the functions of
`ecb-tag-visit-post-actions' are not performed. If
ADDITIONAL-POST-ACTION-LIST is a list of function-symbols then these functions
are performed after these ones of `ecb-tag-visit-post-actions'. So if
NO-TAG-VISIT-POST-ACTIONS is not nil then only the functions of
ADDITIONAL-POST-ACTION-LIST are performed. If ADDITIONAL-POST-ACTION-LIST is
nil too then no post-actions are performed."
  (unless window
    (setq window (selected-window)))
  (select-window window)
  (ecb-nav-save-current)
  ;; first display the right buffer
  (switch-to-buffer (ecb-source-get-buffer source))
  (if (null tag)
      ;; there is no tag to display - we are already fine
      (ecb-nav-add-item (ecb-nav-file-history-item-new))
    ;; in the following we display the TAG
    (when (ecb--semantic-tag-with-position-p tag)
      (ecb-semantic-assert-valid-tag tag)
      ;; let us set the mark so the user can easily jump back.
      (if ecb-tag-jump-sets-mark
          (push-mark nil t))
      (widen)
      (goto-char (ecb-semantic-tag-start tag))
      ;; the following 2 lines prevent the autom. tag-sync-mechanism from
      ;; starting.
      (setq ecb-tag-sync-do-nothing t)
      ;; Klaus Berndl <klaus.berndl@sdm.de>: See the comment in
      ;; `ecb-tag-sync' for an explanation why this is now commented out.
      ;; (setq ecb-selected-tag tag)
      ;; process post action
      (unless no-tag-visit-post-actions
        ;; first the default post actions
        (dolist (f (cdr (assoc 'default ecb-tag-visit-post-actions)))
          (ecb-call-tag-visit-function tag f))
        ;; now the mode specific actions
        (dolist (f (cdr (assoc major-mode ecb-tag-visit-post-actions)))
          (ecb-call-tag-visit-function tag f)))
      ;; now we perform the additional-post-action-list
      (dolist (f additional-post-action-list)
        (ecb-call-tag-visit-function tag f))
      ;; Klaus Berndl <klaus.berndl@sdm.de>: Now we use a different
      ;; implementation of ecb-nav-tag-history-item. Not longer storing
      ;; the whole tag but the tag-buffer and markers of tag-start
      ;; and tag-end. This prevents the navigation-tree from getting
      ;; unusable cause of invalid overlays after a full reparse!
      (let* ((tag-buf (current-buffer))
             (tag-name (ecb--semantic-tag-name tag))
             (tag-start (move-marker (make-marker)
                                     (ecb-semantic-tag-start tag) tag-buf))
             (tag-end (move-marker (make-marker)
                                   (ecb-semantic-tag-end tag) tag-buf)))
        (ecb-nav-add-item (ecb-nav-tag-history-item-new
                           tag-name
                           tag-buf
                           tag-start
                           tag-end
                           (member 'ecb-tag-visit-narrow-tag
                                   additional-post-action-list)))))))


(defun ecb-mouse-over-method-node (node &optional window no-message click-force)
  "Displays help text if mouse moves over a node in the method buffer or if
CLICK-FORCE is not nil and always with regards to the settings in
`ecb-methods-show-node-info'. NODE is the node for which help text should be
displayed, WINDOW is the related window, NO-MESSAGE defines if the help-text
should be printed here."
  (let ((str (when (or click-force
                       (ecb-show-minibuffer-info node window
                                                 (car ecb-methods-show-node-info)))
               (concat
                (tree-node->name node)
                (if (and (= ecb-methods-nodetype-tag
                            (tree-node->type node))
                         (tree-node->data node)
                         (equal (cdr ecb-methods-show-node-info) 'name+type))
                    (concat ", "
                            (symbol-name (ecb--semantic-tag-class
                                          (tree-node->data node))))
                  "")))))
    (prog1 str
      (unless no-message
        (ecb-nolog-message str)))))

;;; popup-menu stuff for the methods-buffer

(defun ecb-call-tag-visit-function (tag fcn)
  "Call FCN with TAG as argument and check if the resulting point is between
the tag-boundaries of TAG. If yes, then go to this point if no point stays at
the location before calling FCN."
  (when (fboundp fcn)
    (let* ((start (ecb--semantic-tag-start tag))
           (end (ecb--semantic-tag-end tag))
           (result-point (save-excursion
                           (funcall fcn tag))))
      (if (and (>= result-point start)
               (<= result-point end))
          (goto-char result-point)
        (ecb-warning "The tag-visit-function `%s' moves point outside of tag - ignored!"
                     fcn)))))

(defun ecb-tag-visit-narrow-tag (tag)
  "Narrow the source buffer to TAG.
If an outside located documentation belongs to TAG and if this documentation
is located direct before TAG \(e.g. Javadoc in Java) then this documentation
is included in the narrow.

Returns current point."
  (when (not (ecb-speedbar-sb-tag-p tag))
    (narrow-to-region (or (ecb-start-of-tag-doc tag)
                          (ecb-semantic-tag-start tag))
                      (ecb-semantic-tag-end tag)))
  (point))


(defun ecb-tag-visit-recenter (tag)
  "Recenter the source-buffer, so current line is in the middle of the window.
If this function is added to `ecb-tag-visit-post-actions' then it's
recommended to add this function add the end of the action list for 'default
or a `major-mode' and not to add the function `ecb-tag-visit-recenter-top'
too!

Returns current point."
  (set-window-start
   (selected-window)
   (ecb-line-beginning-pos (- (/ (ecb-window-full-height) 2))))
  (point))

(defun ecb-tag-visit-recenter-top (tag)
  "Recenter the source-buffer, so current line is in the middle of the window.
If this function is added to `ecb-tag-visit-post-actions' then it's
recommended to add this function add the end of the action list for 'default
or a `major-mode' and not to add the function `ecb-tag-visit-recenter' too!

Returns current point."
  (set-window-start (selected-window)
                    (ecb-line-beginning-pos))
  (point))

(tree-buffer-defpopup-command ecb-methods-menu-jump-and-narrow
  "Jump to the token related to the node under point an narrow to this token."
  (ecb-method-clicked node 1 nil nil nil t '(ecb-tag-visit-narrow-tag
                                             ecb-tag-visit-highlight-tag-header)))


(tree-buffer-defpopup-command ecb-methods-menu-widen
  "Widen the current buffer in the current edit-window."
  (ecb-select-edit-window)
  (widen))


(if (not ecb-running-xemacs)
    ;; Klaus Berndl <klaus.berndl@sdm.de>: This is for silencing the
    ;; byte-compiler. Normally there should be no warning when
    ;; silentcomp-defun is used for hs-minor-mode but....argghhh.
    (require 'hideshow))

(defun ecb-methods-menu-activate-hs ()
  "Activates `hs-minor-mode' in the buffer of `ecb-path-selected-source'. If
this fails then nil is returned otherwise t."
  (save-excursion
    (set-buffer (get-file-buffer (ecb-path-selected-source 'file)))
    (if (or (not (boundp 'hs-minor-mode))
            (not hs-minor-mode))
        (if (fboundp 'hs-minor-mode)
            (progn
              (hs-minor-mode 1)
              t)
          nil)
      t)))


(tree-buffer-defpopup-command ecb-methods-menu-show-block
  "Runs `hs-show-block' for the current node under point."
  (if (not (ecb-methods-menu-activate-hs))
      (ecb-error "hs-minor-mode can not be activated!")
    ;; point must be at beginning of tag-name
    (ecb-method-clicked node 1 nil nil nil t '(ecb-tag-visit-smart-tag-start))
    (save-excursion
      (or (looking-at hs-block-start-regexp)
          (re-search-forward hs-block-start-regexp nil t))
      (hs-show-block))
    ;; Now we are at the beginning of the block or - with other word - on that
    ;; position `ecb-method-clicked' has set the point.
    (ecb-tag-visit-highlight-tag-header (tree-node->data node))))


(tree-buffer-defpopup-command ecb-methods-menu-hide-block
  "Runs `hs-hide-block' for the current node under point."
  (if (not (ecb-methods-menu-activate-hs))
      (ecb-error "hs-minor-mode can not be activated!")
    ;; point must be at beginning of tag-name
    (ecb-method-clicked node 1 nil nil nil t '(ecb-tag-visit-smart-tag-start))
    (save-excursion
      (or (looking-at hs-block-start-regexp)
          (re-search-forward hs-block-start-regexp nil t))
      (hs-hide-block))
    (ecb-tag-visit-highlight-tag-header (tree-node->data node))))


(tree-buffer-defpopup-command ecb-methods-menu-collapse-all
  "Collapse all expandable and expanded nodes"
  (ecb-expand-methods-node-internal (tree-buffer-get-root) -1 nil t t))

(tree-buffer-defpopup-command ecb-methods-menu-collapse-current
  "Collapse the current node"
  (let ((ecb-methods-nodes-collapse-spec 'all))
    (ecb-expand-methods-node-internal node -1 nil t t)))

(tree-buffer-defpopup-command ecb-methods-menu-expand-all-0
  "Expand all nodes exactly to level 0."
  (ecb-expand-methods-node-internal (tree-buffer-get-root) 0 nil t t))

(tree-buffer-defpopup-command ecb-methods-menu-expand-current-0
  "Expand current node exactly to level 0."
  (let ((ecb-methods-nodes-expand-spec 'all))
    (ecb-expand-methods-node-internal node 0 nil t t)))

(tree-buffer-defpopup-command ecb-methods-menu-expand-all-1
  "Expand all nodes to exactly level 1."
  (ecb-expand-methods-node-internal (tree-buffer-get-root) 1 nil t t))

(tree-buffer-defpopup-command ecb-methods-menu-expand-current-1
  "Expand current node exactly to level 0."
  (let ((ecb-methods-nodes-expand-spec 'all))
    (ecb-expand-methods-node-internal node 1 nil t t)))

(tree-buffer-defpopup-command ecb-methods-menu-expand-all-2
  "Expand all nodes to exactly level 2."
  (ecb-expand-methods-node-internal (tree-buffer-get-root) 2 nil t t))

(tree-buffer-defpopup-command ecb-methods-menu-expand-current-2
  "Expand current node exactly to level 0."
  (let ((ecb-methods-nodes-expand-spec 'all))
    (ecb-expand-methods-node-internal node 2 nil t t)))

(tree-buffer-defpopup-command ecb-methods-menu-expand-all-full
  "Expand all expandable nodes recursively, i.e. completely."
  (ecb-expand-methods-node-internal (tree-buffer-get-root) 100 nil t t))

(tree-buffer-defpopup-command ecb-methods-menu-expand-current-full
  "Expand the current node recursively, i.e. completely."
  (let ((ecb-methods-nodes-expand-spec 'all))
    (ecb-expand-methods-node-internal node 100 nil t t)))


(defvar ecb-common-methods-menu nil
  "Built-in menu for the methods-buffer.")


(setq ecb-common-methods-menu
      '( ;;("---")
        ("Expand/Collapse"
         (ecb-methods-menu-collapse-current "Collapse this node completely")
         (ecb-methods-menu-expand-current-0 "Expand this node to level 0")
         (ecb-methods-menu-expand-current-1 "Expand this node to level 1")
         (ecb-methods-menu-expand-current-2 "Expand this node to level 2")
         (ecb-methods-menu-expand-current-full "Expand this node completely")
         ("---")
         (ecb-methods-menu-collapse-all "Collapse all completely")
         (ecb-methods-menu-expand-all-0 "Expand all to level 0")
         (ecb-methods-menu-expand-all-1 "Expand all to level 1")
         (ecb-methods-menu-expand-all-2 "Expand all to level 2")
         (ecb-methods-menu-expand-all-full "Expand all completely")
         )
        ("---")
        (ecb-maximize-ecb-window-menu-wrapper "Maximize window")))


(defvar ecb-methods-tag-menu nil)
(setq ecb-methods-tag-menu
      (append '(("Hide/Show"
                 (ecb-methods-menu-hide-block "Jump to tag and hide block")
                 (ecb-methods-menu-show-block "Jump to tag and show block"))
                ("Narrow/Widen"
                 (ecb-methods-menu-jump-and-narrow "Jump to tag and narrow")
                 (ecb-methods-menu-widen "Undo narrowing of edit-window")))
              ecb-common-methods-menu))


(defvar ecb-methods-menu-title-creator
  (function (lambda (node)
              (let ((data (tree-node->data node)))
                (if (and data (/= ecb-methods-nodetype-bucket
                                  (tree-node->type node)))
                    (typecase data
                      (ecb--semantic-tag (ecb--semantic-tag-name data))
                      (string data)
                      (otherwise (tree-node->name node)))
                  (tree-node->name node)))))
  "The menu-title for the methods menu. See
`ecb-directories-menu-title-creator'.")

(dotimes (i 8)
  (eval `(tree-buffer-defpopup-command
             ,(intern (format "ecb-jump-to-tag-in-editwin%d" (1+ i)))
           ,(format "Jump to current tag in the %d. edit-window." (1+ i))
           (ecb-method-clicked node 3 ,(1+ i) nil nil))))

(defun ecb-methods-menu-editwin-entries ()
  "Generate popup-menu-entries for each edit-window if there are at least 2
edit-windows. Otherwise return nil."
  (let ((edit-win-list (ecb-canonical-edit-windows-list))
        (result nil))
    (when (> (length edit-win-list) 1)
      (dotimes (i (min 8 (length edit-win-list)))
        (setq result
              (append result
                      (list (list (intern (format "ecb-jump-to-tag-in-editwin%d" (1+ i)))
                                  (format "edit-window %d" (1+ i)))))))
      (append (list (list "---")) ;; we want a separator
              (list (append (list "Jump to tag in ...")
                            result))))))


(defun ecb-methods-menu-tagfilter-entries ()
  "Generate popup-menu-entries for the tag-filtering"
  (let* ((curr-semantic-symbol->name-assoc-list
          ;; we must not use here (ecb-methods-get-data-store
          ;; 'semantic-symbol->name-assoc-list) because we do not want the
          ;; function-prototypes...
          (save-excursion
            (set-buffer (ecb-methods-get-data-store 'source-buffer))
            (ecb--semantic-symbol->name-assoc-list)))
         (prot-list '("private" "protected" "public"))
         (prot-menu-elems nil)
         (prot-menu-elems-inverse nil)
         (prot-menu-entries nil)
         (prot-menu-entries-inverse)
         (tag-menu-class-elems nil)
         (tag-menu-class-elems-inverse nil)
         (tag-menu-class-entries nil)
         (tag-menu-class-entries-inverse nil))
    ;; First we have to define all the needed tree-buffer-commands for
    ;; protection- and tagclass-filtering. But this is only done for
    ;; semantic-sources and also the first time.
    (when curr-semantic-symbol->name-assoc-list
      (dolist (tag-class curr-semantic-symbol->name-assoc-list)
        (let ((fcn-sym (intern (format "ecb-methods-filter-by-%s-tagclass"
                                       (car tag-class)))))
          (setq tag-menu-class-elems (cons (cons fcn-sym (cdr tag-class))
                                           tag-menu-class-elems))
          (when (not (fboundp fcn-sym))
            (eval `(tree-buffer-defpopup-command ,fcn-sym
                     ,(format "Filter all tags with tag-class '%s." (car tag-class))
                     (ecb-methods-filter-by-tag-class nil
                                                      (ecb-methods-get-data-store 'source-buffer)
                                                      ,(symbol-name (car tag-class))))))))
      (dolist (tag-class curr-semantic-symbol->name-assoc-list)
        (let ((fcn-sym (intern (format "ecb-methods-filter-by-%s-tagclass-inverse"
                                       (car tag-class)))))
          (setq tag-menu-class-elems-inverse
                (cons (cons fcn-sym (concat "not " (cdr tag-class)))
                      tag-menu-class-elems-inverse))
          (when (not (fboundp fcn-sym))
            (eval `(tree-buffer-defpopup-command ,fcn-sym
                     ,(format "Filter all tags with tag-class unequal '%s."
                              (car tag-class))
                     (ecb-methods-filter-by-tag-class t
                                                      (ecb-methods-get-data-store 'source-buffer)
                                                      ,(symbol-name (car tag-class))))))))
      (dolist (prot prot-list)
        (let ((fcn-sym (intern (format "ecb-methods-filter-by-%s-prot" prot))))
          (setq prot-menu-elems (cons (cons fcn-sym prot)
                                      prot-menu-elems))
          (when (not (fboundp fcn-sym))
            (eval `(tree-buffer-defpopup-command ,fcn-sym
                     ,(format "Filter all tags with %s protection." prot)
                     (ecb-methods-filter-by-prot nil
                                                 (ecb-methods-get-data-store 'source-buffer)
                                                 ,prot))))))
      (dolist (prot prot-list)
        (let ((fcn-sym (intern (format "ecb-methods-filter-by-%s-prot-inverse" prot))))
          (setq prot-menu-elems-inverse
                (cons (cons fcn-sym (concat "not " prot))
                      prot-menu-elems-inverse))
          (when (not (fboundp fcn-sym))
            (eval `(tree-buffer-defpopup-command ,fcn-sym
                     ,(format "Filter all tags with not %s protection." prot)
                     (ecb-methods-filter-by-prot t
                                                 (ecb-methods-get-data-store 'source-buffer)
                                                 ,prot)))))))
    ;; building the menu-entries-list for tag-classes and protections.
    (dolist (elem tag-menu-class-elems)
      (setq tag-menu-class-entries
            (append tag-menu-class-entries
                    (list (list (car elem) (cdr elem))))))
    (dolist (elem tag-menu-class-elems-inverse)
      (setq tag-menu-class-entries-inverse
            (append tag-menu-class-entries-inverse
                    (list (list (car elem) (cdr elem))))))
    (dolist (elem prot-menu-elems)
      (setq prot-menu-entries
            (append prot-menu-entries
                    (list (list (car elem) (cdr elem))))))
    (dolist (elem prot-menu-elems-inverse)
      (setq prot-menu-entries-inverse
            (append prot-menu-entries-inverse
                    (list (list (car elem) (cdr elem))))))
    ;; building the complete filter-menu
    (append nil ;; (list (list "---")) ;; we want a separator
            (list (append (list "Filter tags")
                          (list '(ecb-methods-filter-by-nothing-popup
                                  "No tag filter")
                                '(ecb-methods-filter-delete-last-popup
                                  "Remove last added")
                                '("---")
                                '(ecb-methods-filter-by-regexp-popup
                                  "By regexp"))
                          (when prot-menu-entries
                            (list (append (list "By protection")
                                          prot-menu-entries)))
                          (when tag-menu-class-entries
                            (list (append (list "By tag-class")
                                          tag-menu-class-entries)))
                          (when curr-semantic-symbol->name-assoc-list
                            (list '(ecb-methods-filter-by-current-type-popup
                                    "By current type")))
                          (list '(ecb-methods-filter-by-function-popup
                                  "By a filter-function")
                                '("---")
                                '(ecb-methods-filter-by-regexp-popup-inverse
                                  "By inverse regexp"))
                          (when prot-menu-entries-inverse
                            (list (append (list "By inverse protection")
                                          prot-menu-entries-inverse)))
                          (when tag-menu-class-entries-inverse
                            (list (append (list "By inverse tag-class")
                                          tag-menu-class-entries-inverse)))
                          (list '(ecb-methods-filter-by-function-popup-inverse
                                  "By a inverse filter-function")))))))
    
(defun ecb-methods-menu-creator (tree-buffer-name node)
  "Creates the popup-menus for the methods-buffer."
  (setq ecb-layout-prevent-handle-ecb-window-selection t)
  (let ((dyn-user-extension
         (and (functionp ecb-methods-menu-user-extension-function)
              (funcall ecb-methods-menu-user-extension-function
                       tree-buffer-name node)))
        (dyn-builtin-extension-edit-win (ecb-methods-menu-editwin-entries))
        (dyn-builtin-extension-tagfilter (ecb-methods-menu-tagfilter-entries)))
    (list (cons ecb-methods-nodetype-tag
                (funcall (or ecb-methods-menu-sorter
                             'identity)
                         (append dyn-user-extension
                                 ecb-methods-menu-user-extension
                                 dyn-builtin-extension-tagfilter
                                 ecb-methods-tag-menu
                                 dyn-builtin-extension-edit-win)))
          (cons ecb-methods-nodetype-bucket
                (funcall (or ecb-methods-menu-sorter
                             'identity)
                         (append dyn-user-extension
                                 ecb-methods-menu-user-extension
                                 dyn-builtin-extension-tagfilter
                                 ecb-common-methods-menu)))
          (cons ecb-methods-nodetype-externtag
                (funcall (or ecb-methods-menu-sorter
                             'identity)
                         (append dyn-user-extension
                                 ecb-methods-menu-user-extension
                                 dyn-builtin-extension-tagfilter
                                 ecb-common-methods-menu))))))

(defconst ecb-methods-incr-searchpattern-node-prefix
  '("\\([-+#(]\\|[^-+#(][^ \n]+ \\)?" . 1)
  "Prefix-pattern which ignores all not interesting stuff of a node-name at
incr. search. The following contents of a node-name are ignored by this
pattern:
- types of a variable or return-types of a method
- const specifier of variables
- protection sign of a variable/method: +, - or #

Format: cons with car is the pattern and cdr is the number of subexpr in this
pattern.")

;; Function which compares the node-data of a tree-buffer-node in the
;; method-buffer for equality. We must compare semantic-tags but we must not
;; compare the tags with eq or equal because they can be re-grouped by
;; ecb--semantic-adopt-external-members. the following function is a save
;; "equal"-condition for ECB because currently the method buffer always
;; displays only tags from exactly the buffer of the current edit-window. If
;; `ecb--semantic-equivalent-tag-p' fails we return the result of an
;; eq-comparison.
(defun ecb-compare-methods-buffer-node-data (l r)
  (cond ((or (stringp l) (stringp r))
         (equal l r))
        ((or (equal 'ecb-bucket-node (car l))
             (equal 'ecb-bucket-node (car r)))
         (equal l r))
        (t ;; tags
         (condition-case nil
             (ecb--semantic-equivalent-tag-p l r)
           (error (eq l r))))))

(defun ecb-methods-node-mouse-highlighted-p (node)
  "Return not nil when NODE has a positioned tag as data or belongs to the
completions. This means that this node should be highlighted when mouse is
moved over it."
  (or (not (equal (tree-node->type node) ecb-methods-nodetype-tag))
      (ecb--semantic-tag-with-position-p (tree-node->data node))
      (ecb--semantic-tag-faux-p (tree-node->data node))))

(defecb-tree-buffer-creator ecb-create-methods-tree-buffer ecb-methods-buffer-name
  "Create the tree-buffer for methods."
  (tree-buffer-create
   ecb-methods-buffer-name
   :frame ecb-frame
   :mouse-action-trigger ecb-tree-mouse-action-trigger
   :is-click-valid-fn 'ecb-interpret-mouse-click
   :node-selected-fn 'ecb-tree-buffer-node-select-callback
   :node-expanded-fn 'ecb-tree-buffer-node-expand-callback
   :node-collapsed-fn 'ecb-tree-buffer-node-collapsed-callback
   :node-mouse-over-fn 'ecb-mouse-over-method-node
   :mouse-highlight-fn 'ecb-methods-node-mouse-highlighted-p
   :node-data-equal-fn 'ecb-compare-methods-buffer-node-data
   :maybe-empty-node-types (list ecb-methods-nodetype-bucket)
   :leaf-node-types nil
   :menu-creator 'ecb-methods-menu-creator
   :menu-titles (list (cons ecb-methods-nodetype-tag ecb-methods-menu-title-creator)
                      (cons ecb-methods-nodetype-bucket ecb-methods-menu-title-creator)
                      (cons ecb-methods-nodetype-externtag ecb-methods-menu-title-creator))
   :modeline-menu-creator 'ecb-common-tree-buffer-modeline-menu-creator
   :sticky-parent-p ecb-tree-make-parent-node-sticky
   :sticky-indent-string ecb-tree-stickynode-indent-string
   :sticky-parent-fn nil
   :trunc-lines (ecb-member-of-symbol/value-list ecb-methods-buffer-name
                                                 ecb-tree-truncate-lines)
   :read-only t
   :tree-indent ecb-tree-indent
   :incr-search-p ecb-tree-incremental-search
   :incr-search-additional-pattern ecb-methods-incr-searchpattern-node-prefix
   ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: add an option to make this
   ;; customizable! Define the docstring as follows: reducing the node-list by
   ;; typing always(!) removes first any filter applied before so it always
   ;; starts from the full unfiltered list. (KB: So this mechanism is
   ;; consistent for all buffers, sources, history and methods)
   :reduce-tree-for-incr-search-fn
   (lambda (search full-search-regexp)
     (ecb-methods-filter-apply nil nil nil "" ""
                               (ecb-methods-get-data-store 'source-buffer))
     (if (and search full-search-regexp (> (length search) 0)
              (ecb-methods-filter-apply 'regexp
                                        (cons full-search-regexp nil)
                                        nil
                                        "Search"
                                        search
                                        (ecb-methods-get-data-store 'source-buffer)))
         search
       ""))
   :arrow-navigation ecb-tree-navigation-by-arrow
   :hor-scroll-step ecb-tree-easy-hor-scroll
   :default-images-dir (car ecb-tree-image-icons-directories)
   :additional-images-dir (ecb-member-of-symbol/value-list ecb-methods-buffer-name
                                                           (cdr ecb-tree-image-icons-directories)
                                                           'car 'cdr)
   :image-file-prefix "ecb-"
   :tree-style ecb-tree-buffer-style
   :ascii-guide-face ecb-tree-guide-line-face
   :type-facer nil
   :expand-symbol-before-p ecb-tree-expand-symbol-before
   :highlight-node-face ecb-method-face
   :general-face ecb-methods-general-face
   :after-create-hook (append
                       (list (function (lambda ()
                                         (ecb-common-after-tree-buffer-create-actions)
                                         (setq ecb-methods-root-node (tree-buffer-get-root)))))
                       ecb-common-tree-buffer-after-create-hook
                       ecb-methods-buffer-after-create-hook)
   :after-update-hook nil))


(defun ecb-dump-semantic-toplevel ()
  "Dump the current semantic-tags in special buffer and display them."
  (interactive)
  (let ((tags (ecb-fetch-semantic-tags t))
        (source-buf (current-buffer)))
    (save-selected-window
      (set-buffer (get-buffer-create "*ecb-tag-dump*"))
      (erase-buffer)
      (ecb-dump-semantic-tags-internal tags nil source-buf 1)
      (switch-to-buffer-other-window (get-buffer-create "*ecb-tag-dump*"))
      (goto-char (point-min)))))

(defun ecb-dump-semantic-tags-internal (table parent source-buffer indent)
  (dolist (tag table)
    ;; we ca not use format here because XEmacs-format removes all
    ;; text-properties! 
    (insert (concat (make-string indent ? )
                    (save-excursion
                      (set-buffer source-buffer)
                      (ecb--semantic-format-tag-uml-prototype tag parent t))
                    ", tag-class: "
                    (format "%s" (ecb--semantic-tag-class tag))
                    "\n"))
    (unless (equal (ecb--semantic-tag-class tag) 'function)
      (ecb-dump-semantic-tags-internal (ecb-children-tags tag)
                                       (if (equal (ecb--semantic-tag-class tag)
                                                  'type)
                                           tag)
                                       source-buffer
                                       (+ 2 indent)))))

(defecb-advice-set ecb-methods-browser-advices
  "Adviced functions needed by the methods-browser of ECB.")

(defecb-advice make-indirect-buffer after ecb-methods-browser-advices
  "Clears the semantic topleven-cache for the newly created indirect-buffer.

This is only done when the argument CLONE is not nil. But in this case it must
be ensured that the new clone gets its own tags and do not share it with its
base-buffer. This is achieved by clearing the toplevel cache of semantic.

Returns always the newly created indirect buffer."
  (when (and (not ecb-running-xemacs) ;; clone not available with XEmacs
             ;; clone-flag is not nil
             (ad-get-arg 2))
    (with-current-buffer ad-return-value
      (message "ECB: semantic cache for indirect buffer %s cleared!" (current-buffer))
      (ecb--semantic-clear-toplevel-cache)))
  ad-return-value)

(defecb-advice custom-save-all around ecb-methods-browser-advices
  "Save the customized options completely in the background, i.e. the
file-buffer where the value is saved \(see option `custom-file') is not parsed
by semantic and also killed afterwards."
  (if ecb-minor-mode
      (let ( ;; XEmacs 21.4 does not set this so we do it here, to ensure that
            ;; the custom-file is loadede in an emacs-lisp-mode buffer, s.b.
            (default-major-mode 'emacs-lisp-mode)
            (ecb-basic-buffer-sync nil)
            (kill-buffer-hook nil)
            ;; we prevent parsing the custom-file
            (semantic--before-fetch-tags-hook (lambda ()
                                                nil))
            (semantic-after-toplevel-cache-change-hook nil)
            (semantic-after-partial-cache-change-hook nil))
        ;; Klaus Berndl <klaus.berndl@sdm.de>: we must ensure that the
        ;; current-buffer has a lisp major-mode when the kernel of
        ;; `custom-save-all' is called because cause of a bug (IMHO) in the
        ;; `custom-save-delete' of GNU Emacs (which loads the file returned by
        ;; `custom-file' with `default-major-mode' set to nil which in turn
        ;; causes that new buffer will get the major-mode of the
        ;; current-buffer) the file `custom-file' will get the major-mode of
        ;; the current-buffer. So when the current-buffer has for example
        ;; major-mode `c++-mode' then the file `custom-file' will be loaded
        ;; into a buffer with major-mode c++-mode. The function
        ;; `custom-save-delete' then parses this buffer with (forward-sexp
        ;; (buffer-size)) which of course fails because forward-sexp tries to
        ;; parse the custom-file (which is an emacs-lisp-file) as a c++-file
        ;; with c++-paren-syntax.
        ;; Solution: Ensure that the buffer *scratch* is current when calling
        ;; custom-save-all so we have surely a lispy-buffer and therefore we
        ;; can be sure that custom-file is loaded as lispy-buffer.
        (save-excursion
          (set-buffer (get-buffer-create "*scratch*"))
          ;; now we do the standard task
          ad-do-it)
        ;; now we have to kill the custom-file buffer otherwise semantic would
        ;; parse the buffer of custom-file and the method-buffer would be
        ;; updated with the contents of custom-file which is definitely not
        ;; desired.
        (ignore-errors
          (kill-buffer (find-file-noselect (ecb-custom-file)))))
    ad-do-it))

(ecb-disable-advices 'ecb-methods-browser-advices t)

(silentcomp-provide 'ecb-method-browser)

;;; ecb-method-browser.el end here

