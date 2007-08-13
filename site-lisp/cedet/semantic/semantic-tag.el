;;; semantic-tag.el --- tag creation and access

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2007 Eric M. Ludlam

;; X-CVS: $Id: semantic-tag.el,v 1.45 2007/05/17 14:42:55 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; 
;; I.  The core production of semantic is the list of tags produced by the
;;    different parsers.  This file provides 3 APIs related to tag access:
;;
;;    1) Primitive Tag Access
;;       There is a set of common features to all tags.  These access
;;       functions can get these values.
;;    2) Standard Tag Access
;;       A Standard Tag should be produced by most traditional languages
;;       with standard styles common to typed object oriented languages.
;;       These functions can access these data elements from a tag.
;;    3) Generic Tag Access
;;       Access to tag structure in a more direct way.
;;         ** May not be forward compatible.
;;
;; II.  There is also an API for tag creation.  Use `semantic-tag' to create
;;     a new tag.
;;
;; III.  Tag Comparison.  Allows explicit or comparitive tests to see
;;      if two tags are the same.

;;; History:
;; 

;;; Code:
;;

;; Keep this only so long as we have obsolete fcns.
(require 'semantic-fw)

(defconst semantic-tag-version semantic-version
  "Version string of semantic tags made with this code.")

(defconst semantic-tag-incompatible-version "1.0"
  "Version string of semantic tags which are not currently compatible.
These old style tags may be loaded from a file with semantic db.
In this case, we must flush the old tags and start over.")

;;; Primitive Tag access system:
;;
;; Raw tags in semantic are lists of 5 elements:
;;
;;   (NAME CLASS ATTRIBUTES PROPERTIES OVERLAY)
;;
;; Where:
;;
;;   - NAME is a string that represents the tag name.
;;
;;   - CLASS is a symbol that represent the class of the tag (for
;;     example, usual classes are `type', `function', `variable',
;;     `include', `package', `code').
;;
;;   - ATTRIBUTES is a public list of attributes that describes
;;     language data represented by the tag (for example, a variable
;;     can have a `:constant-flag' attribute, a function an `:arguments'
;;     attribute, etc.).
;;
;;   - PROPERTIES is a private list of properties used internally.
;;
;;   - OVERLAY represent the location of data described by the tag.
;;

(defsubst semantic-tag-name (tag)
  "Return the name of TAG.
For functions, variables, classes, typedefs, etc., this is the identifier
that is being defined.  For tags without an obvious associated name, this
may be the statement type, e.g., this may return @code{print} for python's
print statement."
  (car tag))

(defsubst semantic-tag-class (tag)
  "Return the class of TAG.
That is, the symbol 'variable, 'function, 'type, or other.
There is no limit to the symbols that may represent the class of a tag.
Each parser generates tags with classes defined by it.

For functional languages, typical tag classes are:

@table @code
@item type
Data types, named map for a memory block.
@item function
A function or method, or named execution location.
@item variable
A variable, or named storage for data.
@item include
Statement that represents a file from which more tags can be found.
@item package
Statement that declairs this file's package name.
@item code
Code that has not name or binding to any other symbol, such as in a script.
@end table
"
  (nth 1 tag))

(defsubst semantic-tag-attributes (tag)
  "Return the list of public attributes of TAG.
That is a property list: (ATTRIBUTE-1 VALUE-1 ATTRIBUTE-2 VALUE-2...)."
  (nth 2 tag))

(defsubst semantic-tag-properties (tag)
  "Return the list of private properties of TAG.
That is a property list: (PROPERTY-1 VALUE-1 PROPERTY-2 VALUE-2...)."
  (nth 3 tag))

(defsubst semantic-tag-overlay (tag)
  "Return the OVERLAY part of TAG.
That is, an overlay or an unloaded buffer representation.
This function can also return an array of the form [ START END ].
This occurs for tags that are not currently linked into a buffer."
  (nth 4 tag))

(defsubst semantic--tag-overlay-cdr (tag)
  "Return the cons cell whose car is the OVERLAY part of TAG.
That function is for internal use only."
  (nthcdr 4 tag))

(defsubst semantic--tag-set-overlay (tag overlay)
  "Set the overlay part of TAG with OVERLAY.
That function is for internal use only."
  (setcar (semantic--tag-overlay-cdr tag) overlay))

(defsubst semantic-tag-start (tag)
  "Return the start location of TAG."
  (let ((o (semantic-tag-overlay tag)))
    (if (semantic-overlay-p o)
        (semantic-overlay-start o)
      (aref o 0))))

(defsubst semantic-tag-end (tag)
  "Return the end location of TAG."
  (let ((o (semantic-tag-overlay tag)))
    (if (semantic-overlay-p o)
        (semantic-overlay-end o)
      (aref o 1))))

(defsubst semantic-tag-bounds (tag)
  "Return the location (START END) of data TAG describes."
  (list (semantic-tag-start tag)
        (semantic-tag-end tag)))

(defun semantic-tag-set-bounds (tag start end)
  "In TAG, set the START and END location of data it describes."
  (let ((o (semantic-tag-overlay tag)))
    (if (semantic-overlay-p o)
        (semantic-overlay-move o start end)
      (semantic--tag-set-overlay tag (vector start end)))))

(defun semantic-tag-buffer (tag)
  "Return the buffer TAG resides in.
If TAG has an originating file, read that file into a (maybe new)
buffer, and return it.
Return nil if there is no buffer for this tag."
  (let ((o (semantic-tag-overlay tag)))
    (cond
     ;; TAG is currently linked to a buffer, return it.
     ((and (semantic-overlay-p o)
           (semantic-overlay-live-p o))
      (semantic-overlay-buffer o))
     ;; TAG has an originating file, read that file into a buffer, and
     ;; return it.
     ((semantic--tag-get-property tag :filename)
      (find-file-noselect (semantic--tag-get-property tag :filename)))
     ;; TAG is not in Emacs right now, no buffer is available.
     )))

(defun semantic-tag-mode (&optional tag)
  "Return the major mode active for TAG.
TAG defaults to the tag at point in current buffer.
If TAG has a :mode property return it.
If point is inside TAG bounds, return the major mode active at point.
Return the major mode active at beginning of TAG otherwise.
See also the function `semantic-ctxt-current-mode'."
  (or tag (setq tag (semantic-current-tag)))
  (or (semantic--tag-get-property tag :mode)
      (let ((buffer (semantic-tag-buffer tag))
            (start (semantic-tag-start tag))
            (end   (semantic-tag-end tag)))
        (save-excursion
          (and buffer (set-buffer buffer))
          ;; Unless point is inside TAG bounds, move it to the
          ;; beginning of TAG.
          (or (and (>= (point) start) (< (point) end))
              (goto-char start))
          (require 'semantic-ctxt)
          (semantic-ctxt-current-mode)))))

(defsubst semantic--tag-attributes-cdr (tag)
  "Return the cons cell whose car is the ATTRIBUTES part of TAG.
That function is for internal use only."
  (nthcdr 2 tag))

(defsubst semantic-tag-put-attribute (tag attribute value)
  "Change value in TAG of ATTRIBUTE to VALUE.
If ATTRIBUTE already exists, its value is set to VALUE, otherwise the
new ATTRIBUTE VALUE pair is added.
Return TAG.
Use this function in a parser when not all attributes are known at the
same time."
  (let* ((plist-cdr (semantic--tag-attributes-cdr tag)))
    (when (consp plist-cdr)
      (setcar plist-cdr
              (semantic-tag-make-plist
               (plist-put (car plist-cdr) attribute value))))
    tag))

(defun semantic-tag-put-attribute-no-side-effect (tag attribute value)
  "Change value in TAG of ATTRIBUTE to VALUE without side effects.
All cons cells in the attribute list are replicated so that there
are no side effects if TAG is in shared lists.
If ATTRIBUTE already exists, its value is set to VALUE, otherwise the
new ATTRIBUTE VALUE pair is added.
Return TAG."
  (let* ((plist-cdr (semantic--tag-attributes-cdr tag)))
    (when (consp plist-cdr)
      (setcar plist-cdr
              (semantic-tag-make-plist
               (plist-put (copy-sequence (car plist-cdr))
                          attribute value))))
    tag))

(defsubst semantic-tag-get-attribute (tag attribute)
  "From TAG, return the value of ATTRIBUTE.
ATTRIBUTE is a symbol whose specification value to get.
Return the value found, or nil if ATTRIBUTE is not one of the
attributes of TAG."
  (plist-get (semantic-tag-attributes tag) attribute))

;; These functions are for internal use only!
(defsubst semantic--tag-properties-cdr (tag)
  "Return the cons cell whose car is the PROPERTIES part of TAG.
That function is for internal use only."
  (nthcdr 3 tag))

(defun semantic--tag-put-property (tag property value)
  "Change value in TAG of PROPERTY to VALUE.
If PROPERTY already exists, its value is set to VALUE, otherwise the
new PROPERTY VALUE pair is added.
Return TAG.
That function is for internal use only."
  (let* ((plist-cdr (semantic--tag-properties-cdr tag)))
    (when (consp plist-cdr)
      (setcar plist-cdr
              (semantic-tag-make-plist
               (plist-put (car plist-cdr) property value))))
    tag))

(defun semantic--tag-put-property-no-side-effect (tag property value)
  "Change value in TAG of PROPERTY to VALUE without side effects.
All cons cells in the property list are replicated so that there
are no side effects if TAG is in shared lists.
If PROPERTY already exists, its value is set to VALUE, otherwise the
new PROPERTY VALUE pair is added.
Return TAG.
That function is for internal use only."
  (let* ((plist-cdr (semantic--tag-properties-cdr tag)))
    (when (consp plist-cdr)
      (setcar plist-cdr
              (semantic-tag-make-plist
               (plist-put (copy-sequence (car plist-cdr))
                          property value))))
    tag))

(defsubst semantic--tag-get-property (tag property)
  "From TAG, extract the value of PROPERTY.
Return the value found, or nil if PROPERTY is not one of the
properties of TAG.
That function is for internal use only."
  (plist-get (semantic-tag-properties tag) property))

(defun semantic-tag-file-name (tag)
  "Return the name of the file from which TAG originated.
Return nil if that information can't be obtained.
If TAG is from a loaded buffer, then that buffer's filename is used.
If TAG is unlinked, but has a :filename property, then that is used."
  (let ((buffer (semantic-tag-buffer tag)))
    (if buffer
        (buffer-file-name buffer)
      (semantic--tag-get-property tag :filename))))

;;; Tag tests and comparisons.
;;
(defsubst semantic-tag-p (tag)
  "Return non-nil if TAG is most likely a semantic tag."
  (condition-case nil
      (and (consp tag)
	   (stringp (car tag))                ; NAME
	   (symbolp (nth 1 tag)) (nth 1 tag)  ; TAG-CLASS
	   (listp (nth 2 tag))                ; ATTRIBUTES
	   (listp (nth 3 tag))                ; PROPERTIES
	   )
    ;; If an error occurs, then it most certainly is not a tag.
    (error nil)))

(defsubst semantic-tag-of-class-p (tag class)
  "Return non-nil if class of TAG is CLASS."
  (eq (semantic-tag-class tag) class))

(defun semantic-tag-with-position-p (tag)
  "Return non-nil if TAG has positional information."
  (and (semantic-tag-p tag)
       (let ((o (semantic-tag-overlay tag)))
	 (or (and (semantic-overlay-p o)
		  (semantic-overlay-live-p o))
             (arrayp o)))))

(defun semantic-equivalent-tag-p (tag1 tag2)
  "Compare TAG1 and TAG2 and return non-nil if they are equivalent.
Use `eq' to test if two tags are the same.  Use this function if tags
are being copied and regrouped to test for if two tags represent the
same thing, but may be constructed of different cons cells."
  (and (equal (semantic-tag-name tag1) (semantic-tag-name tag2))
       (semantic-tag-of-class-p tag1 (semantic-tag-class tag2))
       (or (and (not (semantic-tag-overlay tag1))
		(not (semantic-tag-overlay tag2)))
	   (and (semantic-tag-overlay tag1)
		(semantic-tag-overlay tag2)
		(equal (semantic-tag-bounds tag1)
		       (semantic-tag-bounds tag2))))))

(defun semantic-tag-of-type-p (tag type)
  "Compare TAG's type against TYPE.  Non nil if equivalent.
TYPE can be a string, or a tag of class 'type."
  (let* ((tagtype (semantic-tag-type tag))
	 (tagtypestring (cond ((stringp tagtype)
			       tagtype)
			      ((and (semantic-tag-p tagtype)
				    (semantic-tag-of-class-p tagtype 'type))
			       (semantic-tag-name tagtype))
			      (t nil)))
	 (typestring (cond ((stringp type)
			    type)
			   ((and (semantic-tag-p type)
				 (semantic-tag-of-class-p type 'type))
			    (semantic-tag-name type))
			   (t (error "Type's type is unknown"))))
	 )
    (and
     tagtypestring
     (or
      ;; Matching strings (input type is string)
      (and (stringp type)
	   (string= tagtypestring type))
      ;; Matching strings (tag type is string)
      (and (stringp tagtype)
	   (string= tagtype typestring))
      ;; Matching tokens, and the type of the type is the same.
      (and (string= tagtypestring typestring)
	   (if (and (semantic-tag-type tag) (semantic-tag-type type))
	       (equal (semantic-tag-type tag) (semantic-tag-type type))
	     t))
      ))
    ))

(defun semantic-tag-type-compound-p (tag)
  "Return non-nil the type of TAG is compound.
Compound implies a structure or similar data type.
Returns the list of tag members if it is compound."
  (let* ((tagtype (semantic-tag-type tag))
	 )
    (when (and (semantic-tag-p tagtype)
	       (semantic-tag-of-class-p tagtype 'type))
      ;; We have the potential of this being a nifty compound type.
      (semantic-tag-type-members tagtype)
      )))

(defun semantic-tag-faux-p (tag)
  "Return non-nil if TAG is a FAUX tag.
FAUX tags are created to represent a construct that is
not known to exist in the code."
  (semantic--tag-get-property tag :faux-flag))

;;; Tag creation
;;

;; Is this function still necessary?
(defun semantic-tag-make-plist (args)
  "Create a property list with ARGS.
Args is a property list of the form (KEY1 VALUE1 ... KEYN VALUEN).
Where KEY is a symbol, and VALUE is the value for that symbol.
The return value will be a new property list, with these KEY/VALUE
pairs eliminated:

  - KEY associated to nil VALUE.
  - KEY associated to an empty string VALUE.
  - KEY associated to a zero VALUE."
  (let (plist key val)
    (while args
      (setq key  (car args)
            val  (nth 1 args)
            args (nthcdr 2 args))
      (or (member val '("" nil))
          (and (numberp val) (zerop val))
          (setq plist (cons key (cons val plist)))))
    ;; It is not useful to reverse the new plist.
    plist))

(defsubst semantic-tag (name class &rest attributes)
  "Create a generic semantic tag.
NAME is a string representing the name of this tag.
CLASS is the symbol that represents the class of tag this is,
such as 'variable, or 'function.
ATTRIBUTES is a list of additional attributes belonging to this tag."
  (list name class (semantic-tag-make-plist attributes) nil nil))

(defsubst semantic-tag-new-variable (name type default-value &rest attributes)
  "Create a semantic tag of class 'variable.
NAME is the name of this variable.
TYPE is a string or semantic tag representing the type of this variable.
DEFAULT-VALUE is a string representing the default value of this variable.
ATTRIBUTES is a list of additional attributes belonging to this tag."
  (apply 'semantic-tag name 'variable
         :type type
         :default-value default-value
         attributes))

(defsubst semantic-tag-new-function (name type arg-list &rest attributes)
  "Create a semantic tag of class 'function.
NAME is the name of this function.
TYPE is a string or semantic tag representing the type of this function.
ARG-LIST is a list of strings or semantic tags representing the
arguments of this function.
ATTRIBUTES is a list of additional attributes belonging to this tag."
  (apply 'semantic-tag name 'function
         :type type
         :arguments arg-list
         attributes))

(defsubst semantic-tag-new-type (name type members parents &rest attributes)
  "Create a semantic tag of class 'type.
NAME is the name of this type.
TYPE is a string or semantic tag representing the type of this type.
MEMBERS is a list of strings or semantic tags representing the
elements that make up this type if it is a composite type.
PARENTS is a cons cell.  (EXPLICIT-PARENTS . INTERFACE-PARENTS)
EXPLICIT-PARENTS can be a single string (Just one parent) or a
list of parents (in a multiple inheritance situation).  It can also
be nil.
INTERFACE-PARENTS is a list of strings representing the names of
all INTERFACES, or abstract classes inherited from.  It can also be
nil.
This slot can be interesting because the form:
     ( nil \"string\")
is a valid parent where there is no explicit parent, and only an
interface.
ATTRIBUTES is a list of additional attributes belonging to this tag."
  (apply 'semantic-tag name 'type
         :type type
         :members members
         :superclasses (car parents)
         :interfaces (cdr parents)
         attributes))

(defsubst semantic-tag-new-include (name system-flag &rest attributes)
  "Create a semantic tag of class 'include.
NAME is the name of this include.
SYSTEM-FLAG represents that we were able to identify this include as belonging
to the system, as opposed to belonging to the local project.
ATTRIBUTES is a list of additional attributes belonging to this tag."
  (apply 'semantic-tag name 'include
         :system-flag system-flag
         attributes))

(defsubst semantic-tag-new-package (name detail &rest attributes)
  "Create a semantic tag of class 'package.
NAME is the name of this package.
DETAIL is extra information about this package, such as a location where
it can be found.
ATTRIBUTES is a list of additional attributes belonging to this tag."
  (apply 'semantic-tag name 'package
         :detail detail
         attributes))

(defsubst semantic-tag-new-code (name detail &rest attributes)
  "Create a semantic tag of class 'code.
NAME is a name for this code.
DETAIL is extra information about the code.
ATTRIBUTES is a list of additional attributes belonging to this tag."
  (apply 'semantic-tag name 'code
         :detail detail
         attributes))

(defsubst semantic-tag-set-faux (tag)
  "Set TAG to be a new FAUX tag.
FAUX tags represent constructs not found in the source code.
You can identify a faux tag with `semantic-tag-faux-p'"
  (semantic--tag-put-property tag :faux-flag t))

;;; Copying and cloning tags.
;;
(defsubst semantic-tag-clone (tag &optional name)
  "Clone TAG, creating a new TAG.
If optional argument NAME is not nil it specifies a new name for the
cloned tag."
  ;; Right now, TAG is a list.
  (list (or name (semantic-tag-name tag))
        (semantic-tag-class tag)
        (copy-sequence (semantic-tag-attributes tag))
        (copy-sequence (semantic-tag-properties tag))
        (semantic-tag-overlay tag)))

(defun semantic-tag-copy (tag &optional name keep-file)
  "Return a copy of TAG unlinked from the originating buffer.
If optional argument NAME is non-nil it specifies a new name for the
copied tag.
If optional argument KEEP-FILE is non-nil, and TAG was linked to a
buffer, the originating buffer file name is kept in the `:filename'
property of the copied tag.
This runs the tag hook `unlink-copy-hook`."
  ;; Right now, TAG is a list.
  (let ((copy (semantic-tag-clone tag name)))
    (when (semantic-tag-with-position-p tag)
      ;; Keep the filename if needed.
      (when keep-file
	(semantic--tag-put-property
	 copy :filename (semantic-tag-file-name copy)))
      ;; Convert the overlay to a vector, effectively 'unlinking' the tag.
      (semantic--tag-set-overlay
       copy (vector (semantic-tag-start copy) (semantic-tag-end copy)))

      ;; Force the children to be copied also.
      ;;et ((chil (semantic--tag-copy-list
      ;;	   (semantic-tag-components-with-overlays tag)
      ;;	   keep-file)))
      ;;;; Put the list into TAG.
      ;;)

      ;; Call the unlink-copy hook.  This should tell tools that
      ;; this tag is not part of any buffer.
      (semantic--tag-run-hooks copy 'unlink-copy-hook)
      )
    copy))

;;(defun semantic--tag-copy-list (tags &optional keep-file)
;;  "Make copies of TAGS and return the list of TAGS."
;;  (let ((out nil))
;;    (dolist (tag tags out)
;;      (setq out (cons (semantic-tag-copy tag nil keep-file)
;;		      out))
;;      )))

(defun semantic--tag-copy-properties (tag1 tag2)
  "Copy private properties from TAG1 to TAG2.
Return TAG2.
This function is for internal use only."
  (let ((plist (semantic-tag-properties tag1)))
    (while plist
      (semantic--tag-put-property tag2 (car plist) (nth 1 plist))
      (setq plist (nthcdr 2 plist)))
    tag2))

;;; Standard Tag Access
;;

;;; Common
;;
(defsubst semantic-tag-type (tag)
  "Return the value of the `:type' attribute of TAG."
  (semantic-tag-get-attribute tag :type))

(defsubst semantic-tag-modifiers (tag)
  "Return the value of the `:typemodifiers' attribute of TAG."
  (semantic-tag-get-attribute tag :typemodifiers))

(defun semantic-tag-docstring (tag &optional buffer)
  "Return the documentation of TAG.
That is the value defined by the `:documentation' attribute.
Optional argument BUFFER indicates where to get the text from.
If not provided, then only the POSITION can be provided."
  (let ((p (semantic-tag-get-attribute tag :documentation)))
    (if (and p buffer)
        (with-current-buffer buffer
          (semantic-lex-token-text (car (semantic-lex p (1+ p)))))
      p)))

;;; Generic attributes for tags of any class.
;;
(defsubst semantic-tag-named-parent (tag)
  "Return the parent of TAG.
That is the value of the `:parent' attribute.
If a definition can occur outside an actual parent structure, but
refers to that parent by name, then the :parent attribute should be used."
  (semantic-tag-get-attribute tag :parent))

;;; Tags of class `type'
;;
(defsubst semantic-tag-type-members (tag)
  "Return the members of the type that TAG describes.
That is the value of the `:members' attribute."
  (semantic-tag-get-attribute tag :members))

(defun semantic-tag-type-superclasses (tag)
  "Return the list of superclasses of the type that TAG describes."
  (let ((supers (semantic-tag-get-attribute tag :superclasses)))
    (cond ((stringp supers)
	   (list supers))
	  ((listp supers)
	   supers))))

(defsubst semantic-tag-type-interfaces (tag)
  "Return the list of interfaces of the type that TAG describes."
  (semantic-tag-get-attribute tag :interfaces))

;;; Tags of class `function'
;;
(defsubst semantic-tag-function-arguments (tag)
  "Return the arguments of the function that TAG describes.
That is the value of the `:arguments' attribute."
  (semantic-tag-get-attribute tag :arguments))

(defsubst semantic-tag-function-throws (tag)
  "Return the exceptions the function that TAG describes can throw.
That is the value of the `:throws' attribute."
  (semantic-tag-get-attribute tag :throws))

(defsubst semantic-tag-function-parent (tag)
  "Return the parent of the function that TAG describes.
That is the value of the `:parent' attribute.
A function has a parent if it is a method of a class, and if the
function does not appear in body of it's parent class."
  (semantic-tag-named-parent tag))

(defsubst semantic-tag-function-destructor-p (tag)
  "Return non-nil if TAG describes a destructor function.
That is the value of the `:destructor-flag' attribute."
  (semantic-tag-get-attribute tag :destructor-flag))

(defsubst semantic-tag-function-constructor-p (tag)
  "Return non-nil if TAG describes a constructor function.
That is the value of the `:constructor-flag' attribute."
  (semantic-tag-get-attribute tag :constructor-flag))

;;; Tags of class `variable'
;;
(defsubst semantic-tag-variable-default (tag)
  "Return the default value of the variable that TAG describes.
That is the value of the attribute `:default-value'."
  (semantic-tag-get-attribute tag :default-value))

(defsubst semantic-tag-variable-constant-p (tag)
  "Return non-nil if the variable that TAG describes is a constant.
That is the value of the attribute `:constant-flag'."
  (semantic-tag-get-attribute tag :constant-flag))

;;; Tags of class `include'
;;
(defsubst semantic-tag-include-system-p (tag)
  "Return non-nil if the include that TAG describes is a system include.
That is the value of the attribute `:system-flag'."
  (semantic-tag-get-attribute tag :system-flag))

(define-overload semantic-tag-include-filename (tag)
  "Return a filename representation of TAG.
The default action is to return the `semantic-tag-name'.
Some languages do not use full filenames in their include statements.
Override this method to translate the code represenation
into a filename.  (A relative filename if necessary.)

See `semantic-dependency-tag-file' to expand an include
tag to a full file name.")

(defun semantic-tag-include-filename-default (tag)
  "Return a filename representation of TAG.
Returns `semantic-tag-name'."
  (semantic-tag-name tag))

;;; Tags of class `code'
;;
(defsubst semantic-tag-code-detail (tag)
  "Return detail information from code that TAG describes.
That is the value of the attribute `:detail'."
  (semantic-tag-get-attribute tag :detail))

;;; Tags of class `alias'
;;
(defsubst semantic-tag-new-alias (name meta-tag-class value &rest attributes)
  "Create a semantic tag of class alias.
NAME is a name for this alias.
META-TAG-CLASS is the class of the tag this tag is an alias.
VALUE is the aliased definition.
ATTRIBUTES is a list of additional attributes belonging to this tag."
  (apply 'semantic-tag name 'alias
         :aliasclass meta-tag-class
         :definition value
         attributes))

(defsubst semantic-tag-alias-class (tag)
  "Return the class of tag TAG is an alias."
  (semantic-tag-get-attribute tag :aliasclass))

;;;###autoload
(define-overload semantic-tag-alias-definition (tag)
  "Return the definition TAG is an alias.
The returned value is a tag of the class that
`semantic-tag-alias-class' returns for TAG.
The default is to return the value of the :definition attribute.
Return nil if TAG is not of class 'alias."
  (when (semantic-tag-of-class-p tag 'alias)
    (:override
     (semantic-tag-get-attribute tag :definition))))

;;; Language Specific Tag access via overload
;;
;;;###autoload
(define-overload semantic-tag-components (tag)
  "Return a list of components for TAG.
A Component is a part of TAG which itself may be a TAG.
Examples include the elements of a structure in a 
tag of class `type, or the list of arguments to a
tag of class 'function."
  )

(defun semantic-tag-components-default (tag)
  "Return a list of components for TAG.
Perform the described task in `semantic-tag-components'."
  (cond ((semantic-tag-of-class-p tag 'type)
	 (semantic-tag-type-members tag))
	((semantic-tag-of-class-p tag 'function)
	 (semantic-tag-function-arguments tag))
	(t nil)))

;;;###autoload
(define-overload semantic-tag-components-with-overlays (tag)
  "Return the list of top level components belonging to TAG.
Children are any sub-tags which contain overlays.

Default behavior is to get `semantic-tag-components' in addition
to the components of an anonymous types (if applicable.)

Note for language authors:
  If a mode defines a language tag that has tags in it with overlays
you should still return them with this function.
Ignoring this step will prevent several features from working correctly."
  )

(defun semantic-tag-components-with-overlays-default (tag)
  "Return the list of top level components belonging to TAG.
Children are any sub-tags which contain overlays.
The default action collects regular components of TAG, in addition
to any components beloning to an anonymous type."
  (let ((class (semantic-tag-class tag))
	(explicit-children (semantic-tag-components tag))
	(type (semantic-tag-type tag))
	(anon-type-children nil)
	(all-children nil))
    ;; Identify if this tag has an anonymous structure as
    ;; its type.  This implies it may have children with overlays.
    (when (and type (semantic-tag-p type))
      (setq anon-type-children (semantic-tag-components type))
      ;; Add anonymous children
      (while anon-type-children
	(when (semantic-tag-with-position-p (car anon-type-children))
	  (setq all-children (cons (car anon-type-children) all-children)))
	(setq anon-type-children (cdr anon-type-children))))
    ;; Add explicit children
    (while explicit-children
      (when (semantic-tag-with-position-p (car explicit-children))
	(setq all-children (cons (car explicit-children) all-children)))
      (setq explicit-children (cdr explicit-children)))
    ;; Return
    (nreverse all-children)))

(defun semantic-tag-children-compatibility (tag &optional positiononly)
  "Return children of TAG.
If POSITIONONLY is nil, use `semantic-tag-components'.
If POSITIONONLY is non-nil, use `semantic-tag-components-with-overlays'.
DO NOT use this fcn in new code.  Use one of the above instead."
  (if positiononly
      (semantic-tag-components-with-overlays tag)
    (semantic-tag-components tag)))

;;; Tag Region
;;
;; A Tag represents a region in a buffer.  You can narrow to that tag.
;;
(defun semantic-narrow-to-tag (&optional tag)
  "Narrow to the region specified by the bounds of TAG.
See `semantic-tag-bounds'."
  (interactive)
  (if (not tag) (setq tag (semantic-current-tag)))
  (narrow-to-region (semantic-tag-start tag)
		    (semantic-tag-end tag)))

(defmacro semantic-with-buffer-narrowed-to-current-tag (&rest body)
  "Execute BODY with the buffer narrowed to the current tag."
  `(save-restriction
     (semantic-narrow-to-tag (semantic-current-tag))
     ,@body))
(put 'semantic-with-buffer-narrowed-to-current-tag 'lisp-indent-function 0)
(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec semantic-with-buffer-narrowed-to-current-tag
	      (def-body))))

(defmacro semantic-with-buffer-narrowed-to-tag (tag &rest body)
  "Narrow to TAG, and execute BODY."
  `(save-restriction
     (semantic-narrow-to-tag ,tag)
     ,@body))
(put 'semantic-with-buffer-narrowed-to-tag 'lisp-indent-function 1)
(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec semantic-with-buffer-narrowed-to-tag
	      (def-body))))

;;; Tag Hooks
;;
;; Semantic may want to provide special hooks when specific operations
;; are about to happen on a given tag.  These routines allow for hook
;; maintenance on a tag.

;; Internal global variable used to manage tag hooks.  For example,
;; some implementation of `remove-hook' checks that the hook variable
;; is `default-boundp'.
(defvar semantic--tag-hook-value)

(defun semantic-tag-add-hook (tag hook function &optional append)
  "Onto TAG, add to the value of HOOK the function FUNCTION.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.
HOOK should be a symbol, and FUNCTION may be any valid function.
See also the function `add-hook'."
  (let ((semantic--tag-hook-value (semantic--tag-get-property tag hook)))
    (add-hook 'semantic--tag-hook-value function append)
    (semantic--tag-put-property tag hook semantic--tag-hook-value)
    semantic--tag-hook-value))

(defun semantic-tag-remove-hook (tag hook function)
  "Onto TAG, remove from the value of HOOK the function FUNCTION.
HOOK should be a symbol, and FUNCTION may be any valid function.  If
FUNCTION isn't the value of HOOK, or, if FUNCTION doesn't appear in
the list of hooks to run in HOOK, then nothing is done.
See also the function `remove-hook'."
  (let ((semantic--tag-hook-value (semantic--tag-get-property tag hook)))
    (remove-hook 'semantic--tag-hook-value function)
    (semantic--tag-put-property tag hook semantic--tag-hook-value)
    semantic--tag-hook-value))

(defun semantic--tag-run-hooks (tag hook &rest args)
  "Run for TAG all expressions saved on the property HOOK.
Each hook expression must take at least one argument, the TAG.
For any given situation, additional ARGS may be passed."
  (let ((semantic--tag-hook-value (semantic--tag-get-property tag hook))
	(arglist (cons tag args)))
    (condition-case err
	;; If a hook bombs, ignore it!  Usually this is tied into
	;; some sort of critical system.
	(apply 'run-hook-with-args 'semantic--tag-hook-value arglist)
      (error (message "Error: %S" err)))))

;;; Tags and Overlays
;;
;; Overlays are used so that we can quickly identify tags from
;; buffer positions and regions using built in Emacs commands.
;;
(defun semantic--tag-unlink-from-buffer (tag)
  "Convert TAG from using an overlay to using an overlay proxy.
This function is for internal use only."
  (when (semantic-tag-p tag)
    (let ((o (semantic-tag-overlay tag)))
      (when (semantic-overlay-p o)
        (semantic--tag-set-overlay
         tag (vector (semantic-overlay-start o)
                     (semantic-overlay-end o)))
        (semantic-overlay-delete o))
      ;; Look for a link hook on TAG.
      (semantic--tag-run-hooks tag 'unlink-hook)
      ;; Fix the sub-tags which contain overlays.
      (semantic--tag-unlink-list-from-buffer
       (semantic-tag-components-with-overlays tag)))))

(defun semantic--tag-link-to-buffer (tag)
  "Convert TAG from using an overlay proxy to using an overlay.
This function is for internal use only."
  (when (semantic-tag-p tag)
    (let ((o (semantic-tag-overlay tag)))
      (when (and (vectorp o) (= (length o) 2))
        (setq o (semantic-make-overlay (aref o 0) (aref o 1)
                                       (current-buffer)))
        (semantic--tag-set-overlay tag o)
        (semantic-overlay-put o 'semantic tag)
        ;; Clear the :filename property
        (semantic--tag-put-property tag :filename nil))
      ;; Look for a link hook on TAG.
      (semantic--tag-run-hooks tag 'link-hook)
      ;; Fix the sub-tags which contain overlays.
      (semantic--tag-link-list-to-buffer
       (semantic-tag-components-with-overlays tag)))))

(defsubst semantic--tag-unlink-list-from-buffer (tags)
  "Convert TAGS from using an overlay to using an overlay proxy.
This function is for internal use only."
  (mapcar 'semantic--tag-unlink-from-buffer tags))

(defsubst semantic--tag-link-list-to-buffer (tags)
  "Convert TAGS from using an overlay proxy to using an overlay.
This function is for internal use only."
  (mapcar 'semantic--tag-link-to-buffer tags))

(defun semantic--tag-unlink-cache-from-buffer ()
  "Convert all tags in the current cache to use overlay proxys.
This function is for internal use only."
  (semantic--tag-unlink-list-from-buffer
   (semantic-fetch-tags)))

(defun semantic--tag-link-cache-to-buffer ()
  "Convert all tags in the current cache to use overlays.
This function is for internal use only."
  (condition-case nil
      ;; In this unique case, we cannot call the usual toplevel fn.
      ;; because we don't want a reparse, we want the old overlays.
      (semantic--tag-link-list-to-buffer
       semantic--buffer-cache)
    ;; Recover when there is an error restoring the cache.
    (error (message "Error recovering tag list")
           (semantic-clear-toplevel-cache)
           nil)))

;;; Tag Cooking
;;
;; Raw tags from a parser follow a different positional format than
;; those used in the buffer cache.  Raw tags need to be cooked into
;; semantic cache friendly tags for use by the masses.
;;
(defsubst semantic--tag-expanded-p (tag)
  "Return non-nil if TAG is expanded.
This function is for internal use only.
See also the function `semantic--expand-tag'."
  ;; In fact a cooked tag is actually a list of cooked tags
  ;; because a raw tag can be expanded in several cooked ones!
  (when (consp tag)
    (while (and (semantic-tag-p (car tag))
                (vectorp (semantic-tag-overlay (car tag))))
      (setq tag (cdr tag)))
    (null tag)))

(defvar semantic-tag-expand-function nil
  "Function used to expand a tag.
It is passed each tag production, and must return a list of tags
derived from it, or nil if it does not need to be expanded.

Languages with compound definitions should use this function to expand
from one compound symbol into several.  For example, in C or Java the
following definition is easily parsed into one tag:

  int a, b;

This function should take this compound tag and turn it into two tags,
one for A, and the other for B.")
(make-variable-buffer-local 'semantic-tag-expand-function)

(defun semantic--tag-expand (tag)
  "Convert TAG from a raw state to a cooked state, and expand it.
Returns a list of cooked tags.

  The parser returns raw tags with positional data START END at the
end of the tag data structure (a list for now).  We convert it from
that to a cooked state that uses an overlay proxy, that is, a vector
\[START END].

  The raw tag is changed with side effects and maybe expanded in
several derived tags when the variable `semantic-tag-expand-function'
is set.

This function is for internal use only."
  (if (semantic--tag-expanded-p tag)
      ;; Just return TAG if it is already expanded (by a grammar
      ;; semantic action), or if it isn't recognized as a valid
      ;; semantic tag.
      tag
    
    ;; Try to cook the tag.  This code will be removed when tag will
    ;; be directly created with the right format.
    (condition-case nil
        (let ((ocdr (semantic--tag-overlay-cdr tag)))
          ;; OCDR contains the sub-list of TAG whose car is the
          ;; OVERLAY part of TAG. That is, a list (OVERLAY START END).
          ;; Convert it into an overlay proxy ([START END]).
          (semantic--tag-set-overlay
           tag (vector (nth 1 ocdr) (nth 2 ocdr)))
          ;; Remove START END positions at end of tag.
          (setcdr ocdr nil)
          ;; At this point (length TAG) must be 5!
          ;;(unless (= (length tag) 5)
          ;;  (error "Tag expansion failed"))
          )
      (error
       (message "A Rule must return a single tag-line list!")
       (debug tag)
       nil))
    
    ;; Compatibility code to be removed in future versions.
    (unless semantic-tag-expand-function
      ;; This line throws a byte compiler warning.
      (setq semantic-tag-expand-function semantic-expand-nonterminal)
      )
    
    ;; Expand based on local configuration
    (if semantic-tag-expand-function
        (or (funcall semantic-tag-expand-function tag)
            (list tag))
      (list tag))))

;; Foreign tags
;;
(defmacro semantic-foreign-tag-invalid (tag)
  "Signal that TAG is an invalid foreign tag."
  `(signal 'wrong-type-argument '(semantic-foreign-tag-p ,tag)))

(defsubst semantic-foreign-tag-p (tag)
  "Return non-nil if TAG is a foreign tag.
That is, a tag unlinked from the originating buffer, which carries the
originating buffer file name, and major mode."
  (and (semantic-tag-p tag)
       (semantic--tag-get-property tag :foreign-flag)))

(defsubst semantic-foreign-tag-check (tag)
  "Check that TAG is a valid foreign tag.
Signal an error if not."
  (or (semantic-foreign-tag-p tag)
      (semantic-foreign-tag-invalid tag)))

(defun semantic-foreign-tag (&optional tag)
  "Return a copy of TAG as a foreign tag, or nil if it can't be done.
TAG defaults to the tag at point in current buffer.
See also `semantic-foreign-tag-p'."
  (or tag (setq tag (semantic-current-tag)))
  (when (semantic-tag-p tag)
    (let ((ftag (semantic-tag-copy tag nil t)))
      ;; A foreign tag must carry its originating buffer file name!
      (when (semantic--tag-get-property ftag :filename)
        (semantic--tag-put-property
         ftag :mode (semantic-tag-mode tag))
        (semantic--tag-put-property ftag :foreign-flag t)
        ftag))))

;; High level obtain/insert foreign tag overloads
;;
;;;###autoload
(define-overload semantic-obtain-foreign-tag (&optional tag)
  "Obtain a foreign tag from TAG.
TAG defaults to the tag at point in current buffer.
Return the obtained foreign tag or nil if failed."
  (semantic-foreign-tag tag))

(defun semantic-insert-foreign-tag-default (foreign-tag)
  "Insert FOREIGN-TAG into the current buffer.
The default behavior assumes the current buffer is a language file,
and attempts to insert a prototype/function call."
  ;; Long term goal: Have a mechanism for a tempo-like template insert
  ;; for the given tag.
  (insert (semantic-format-tag-prototype foreign-tag)))

;;;###autoload
(define-overload semantic-insert-foreign-tag (foreign-tag)
  "Insert FOREIGN-TAG into the current buffer.
Signal an error if FOREIGN-TAG is not a valid foreign tag.
This function is overridable with the symbol `insert-foreign-tag'."
  (semantic-foreign-tag-check foreign-tag)
  (:override)
  (message (semantic-format-tag-summarize foreign-tag)))

;;; EDEBUG display support
;;
(eval-after-load "cedet-edebug"
  '(progn
     (cedet-edebug-add-print-override
      '(semantic-tag-p object)
      '(concat "#<TAG " (semantic-format-tag-name object) ">"))
     (cedet-edebug-add-print-override
      '(and (listp object) (semantic-tag-p (car object)))
      '(cedet-edebug-prin1-recurse object))
     ))

;;; Compatibility
;;
(defconst semantic-token-version
  semantic-tag-version)
(defconst semantic-token-incompatible-version
  semantic-tag-incompatible-version)

(semantic-alias-obsolete 'semantic-token-name
                         'semantic-tag-name)

(semantic-alias-obsolete 'semantic-token-token
                         'semantic-tag-class)

(semantic-alias-obsolete 'semantic-token-extra-specs
                         'semantic-tag-attributes)

(semantic-alias-obsolete 'semantic-token-properties
                         'semantic-tag-properties)

(semantic-alias-obsolete 'semantic-token-properties-cdr
                         'semantic--tag-properties-cdr)

(semantic-alias-obsolete 'semantic-token-overlay
                         'semantic-tag-overlay)

(semantic-alias-obsolete 'semantic-token-overlay-cdr
                         'semantic--tag-overlay-cdr)

(semantic-alias-obsolete 'semantic-token-start
                         'semantic-tag-start)

(semantic-alias-obsolete 'semantic-token-end
                         'semantic-tag-end)

(semantic-alias-obsolete 'semantic-token-extent
                         'semantic-tag-bounds)

(semantic-alias-obsolete 'semantic-token-buffer
                         'semantic-tag-buffer)

(semantic-alias-obsolete 'semantic-token-put
                         'semantic--tag-put-property)

(semantic-alias-obsolete 'semantic-token-put-no-side-effect
                         'semantic--tag-put-property-no-side-effect)

(semantic-alias-obsolete 'semantic-token-get
                         'semantic--tag-get-property)

(semantic-alias-obsolete 'semantic-token-add-extra-spec
                         'semantic-tag-put-attribute)

(semantic-alias-obsolete 'semantic-token-extra-spec
                         'semantic-tag-get-attribute)

(semantic-alias-obsolete 'semantic-token-type
                         'semantic-tag-type)

(semantic-alias-obsolete 'semantic-token-modifiers
                         'semantic-tag-modifiers)

(semantic-alias-obsolete 'semantic-token-docstring
                         'semantic-tag-docstring)

(semantic-alias-obsolete 'semantic-token-type-parts
                         'semantic-tag-type-members)

(defsubst semantic-token-type-parent (tag)
  "Return the parent of the type that TAG describes.
The return value is a list.  A value of nil means no parents.
The `car' of the list is either the parent class, or a list
of parent classes.  The `cdr' of the list is the list of
interfaces, or abstract classes which are parents of TAG."
  (cons (semantic-tag-get-attribute tag :superclasses)
        (semantic-tag-type-interfaces tag)))
(make-obsolete 'semantic-token-type-parent
	       "\
use `semantic-tag-type-superclass' \
and `semantic-tag-type-interfaces' instead")

(semantic-alias-obsolete 'semantic-token-type-parent-superclass
                         'semantic-tag-type-superclasses)

(semantic-alias-obsolete 'semantic-token-type-parent-implement
                         'semantic-tag-type-interfaces)

(semantic-alias-obsolete 'semantic-token-type-extra-specs
                         'semantic-tag-attributes)

(semantic-alias-obsolete 'semantic-token-type-extra-spec
                         'semantic-tag-get-attribute)

(semantic-alias-obsolete 'semantic-token-type-modifiers
                         'semantic-tag-modifiers)

(semantic-alias-obsolete 'semantic-token-function-args
                         'semantic-tag-function-arguments)

(semantic-alias-obsolete 'semantic-token-function-extra-specs
                         'semantic-tag-attributes)

(semantic-alias-obsolete 'semantic-token-function-extra-spec
                         'semantic-tag-get-attribute)

(semantic-alias-obsolete 'semantic-token-function-modifiers
                         'semantic-tag-modifiers)

(semantic-alias-obsolete 'semantic-token-function-throws
                         'semantic-tag-function-throws)

(semantic-alias-obsolete 'semantic-token-function-parent
                         'semantic-tag-function-parent)

(semantic-alias-obsolete 'semantic-token-function-destructor
                         'semantic-tag-function-destructor-p)

(semantic-alias-obsolete 'semantic-token-variable-default
			 'semantic-tag-variable-default)

(semantic-alias-obsolete 'semantic-token-variable-extra-specs
                         'semantic-tag-attributes)

(semantic-alias-obsolete 'semantic-token-variable-extra-spec
                         'semantic-tag-get-attribute)

(semantic-alias-obsolete 'semantic-token-variable-modifiers
                         'semantic-tag-modifiers)

(semantic-alias-obsolete 'semantic-token-variable-const
                         'semantic-tag-variable-constant-p)

(semantic-alias-obsolete 'semantic-token-variable-optsuffix
                         'semantic-tag-variable-optsuffix)

(semantic-alias-obsolete 'semantic-token-include-system
                         'semantic-tag-include-system-p)

(semantic-alias-obsolete 'semantic-token-p
                         'semantic-tag-p)

(semantic-alias-obsolete 'semantic-token-with-position-p
                         'semantic-tag-with-position-p)

(semantic-alias-obsolete 'semantic-tag-make-assoc-list
                         'semantic-tag-make-plist)

(semantic-alias-obsolete 'semantic-nonterminal-children
			 'semantic-tag-children-compatibility)

(semantic-alias-obsolete 'semantic-narrow-to-token
			 'semantic-narrow-to-tag)

(semantic-alias-obsolete 'semantic-with-buffer-narrowed-to-current-token
			 'semantic-with-buffer-narrowed-to-current-tag)

(semantic-alias-obsolete 'semantic-with-buffer-narrowed-to-token
			 'semantic-with-buffer-narrowed-to-tag)

(semantic-alias-obsolete 'semantic-deoverlay-token
                         'semantic--tag-unlink-from-buffer)

(semantic-alias-obsolete 'semantic-overlay-token
                         'semantic--tag-link-to-buffer)

(semantic-alias-obsolete 'semantic-deoverlay-list
                         'semantic--tag-unlink-list-from-buffer)

(semantic-alias-obsolete 'semantic-overlay-list
                         'semantic--tag-link-list-to-buffer)

(semantic-alias-obsolete 'semantic-deoverlay-cache
                         'semantic--tag-unlink-cache-from-buffer)

(semantic-alias-obsolete 'semantic-overlay-cache
                         'semantic--tag-link-cache-to-buffer)

(semantic-alias-obsolete 'semantic-cooked-token-p
                         'semantic--tag-expanded-p)

(semantic-varalias-obsolete 'semantic-expand-nonterminal
                            'semantic-tag-expand-function)

(semantic-alias-obsolete 'semantic-raw-to-cooked-token
                         'semantic--tag-expand)
			 
;; Lets test this out during this short transition.
(semantic-alias-obsolete 'semantic-clone-tag
                         'semantic-tag-clone)

(semantic-alias-obsolete 'semantic-token
                         'semantic-tag)

(semantic-alias-obsolete 'semantic-token-new-variable
                         'semantic-tag-new-variable)

(semantic-alias-obsolete 'semantic-token-new-function
                         'semantic-tag-new-function)

(semantic-alias-obsolete 'semantic-token-new-type
                         'semantic-tag-new-type)

(semantic-alias-obsolete 'semantic-token-new-include
                         'semantic-tag-new-include)

(semantic-alias-obsolete 'semantic-token-new-package
                         'semantic-tag-new-package)

(semantic-alias-obsolete 'semantic-equivalent-tokens-p
                         'semantic-equivalent-tag-p)

(provide 'semantic-tag)

;;; semantic-tag.el ends here
