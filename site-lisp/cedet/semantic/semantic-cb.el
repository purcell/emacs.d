;;; semantic-cb.el --- Manage and maintain a Class Browser database

;;; Copyright (C) 2002, 2003, 2004, 2005 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-cb.el,v 1.18 2005/09/30 20:18:50 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic-sb is free software; you can redistribute it and/or modify
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
;; Files in an OO language don't always properly represent the structure
;; of classes available.  This Class Browser analyzer can create
;; and maintain a database where types and tags are linked together
;; so that the program structure can be programatically navigated
;; or displayed.
;;
;; By having the classes created inherit from `eieio-speedbar-*',
;; speedbar support for full class browsing is garnered for
;; nearly no effort.

(require 'semantic)
(require 'semanticdb)
(require 'semanticdb-find)
(require 'semantic-sort)
(require 'eieio-speedbar)
(require 'eieio-base)

;;; Code:
(defclass semantic-cb-project ()
  ((types :initarg :types
	  :type list
	  :documentation
	  "List of top level types in this project.")
   )
  "The root of a project's tag system.
The project will consist of top-level types, classes, namespaces,
or whatever is used in that language as a representaton.")

(defclass semantic-cb-tag (eieio-named eieio-speedbar)
  ((buttontype :initform statictag)
   (buttonface :initform speedbar-tag-face)
   (tag :initarg :tag
	:type semantic-tag
	:documentation
	"Semantic tag which represents a type.")
   (table :initarg :table
	  :type semanticdb-abstract-table
	  :documentation
	  "This is the database table that `tag' was found in.
Be sure to use this field when loading a tag's file into memory.")
   (container :initarg :container
	      :type (or null semantic-cb-tag)
	      :documentation
	      "This is the CB object containing this tag.
CB Containers are usually types with attributes of methods.")
   )
  "A single semantic tag.
Tags represented in the Class Browser database may not be loaded
in memory, so this forms the structure needed to access them.")

(defclass semantic-cb-type (semantic-cb-tag)
  ((buttontype :initform expandtag)
   (buttonface :initform speedbar-tag-face)
   (parents :type list
	     :initform nil
	     :documentation
	     "The full parents this type was derived from.
These are also `semantic-cb-type' objects.
This excludes Java interfaces.")
   (subclasses :type list
	       :initform nil
	       :documentation
	       "A list of types which inherit from this object.
These are also `semantic-cb-type' objects.")
   (children :type list
	     :initform nil
	     :documentation
	     "List of CB tag children, both embedded and external.
Embedded children are defined within the scope of this types declaration.
External children are defined within some other scope, and are labeled
as children of this type.
Children are of type `semantic-cb-tag'.")
   )
  "One type object for a given project.
Because some connections are derived, or take a while to find,
details which are derivable will be cached in the fields
of a type object.
In addition, type objects will contain the actual tags created by
semantic, external methods and such will be cached in this object, not
in the semantic tag itself.")

(defvar semantic-cb-incomplete-types nil
  "During construction, the list of types that need work.
Types are created without CB objects for parent or interfaces.
We need to go back later and fill them in from this list.")

(defvar semantic-cb-incomplete-scoped-types nil
  "During construction, the list of contained types that need work.
Types are created without CB objects for parent or interfaces.
We need to go back later and fill them in from this list.")

(defun semantic-cb-add-incomplete-type (cbtag)
  "Add CBTAG to the list of incomplete types."
  (add-to-list (if (oref cbtag :container)
		   'semantic-cb-incomplete-scoped-types
		 'semantic-cb-incomplete-types)
	       cbtag))

(defvar semantic-cb-current-project nil
  "The current project's class structure.")

(defun semantic-cb-clear-current-project ()
  "Clear the class browser cache."
  (interactive)
  (setq semantic-cb-current-project nil))

(defun semantic-cb-new-class-browser ()
  "Create an object representing this project's organization.
The object returned is of type `semantic-cb-project', which contains
the slot `:types', a list of all top-level types.  Each element is a
class of type `semantic-cb-tag', or `semantic-cb-type'."
  (let* ((semanticdb-search-system-databases nil)
	 (alldbtype (semanticdb-brute-find-tags-by-class
		    'type
		    ;; Need a way to strip away system queries.
		    ))
	 ;; Scope these variables during construction.
	 (semantic-cb-incomplete-types nil)
	 (semantic-cb-incomplete-scoped-types nil)
	 )
    ;; Loop over all discovered types, and construct them.
    (while alldbtype
      (let ((alltype (cdr (car alldbtype)))
	    (db (car (car alldbtype))))
	(while alltype
	  ;; This adds all new types into a special list.
	  (semantic-cb-convert-type (car alltype) db nil)
	  (setq alltype (cdr alltype))))
      (setq alldbtype (cdr alldbtype)))
    ;; Cycle over all incomplete subtypes, finding parents
    ;; The incomplete type lists are in two batches.  The first we
    ;; will deal with are for unscoped classes at the top level.
    ;; The second is for types defined in other types, so that
    ;; they are scoped locally.  This excludes them from our search
    ;; lists.
    (let ((typesearchlist semantic-cb-incomplete-types)
	  (list1 semantic-cb-incomplete-types)
	  (list2 semantic-cb-incomplete-scoped-types)
	  (top-level nil))
      (while list1
	(semantic-cb-complete-type (car list1) typesearchlist)
	;; If there is no parent from our project, then
	;; it must be a top-level object.
	(when (not (oref (car list1) parents))
	  (setq top-level (cons (car list1) top-level)))
	(setq list1 (cdr list1)))
      (while list2
	(semantic-cb-complete-type (car list2) typesearchlist)
	(setq list2 (cdr list2)))
      ;; Create the new cache.
      (setq semantic-cb-current-project
	    (semantic-cb-project
	     "Project"
	     :types (nreverse top-level)
	     )))
    ;; Return it
    semantic-cb-current-project))

(defun semantic-cb-complete-type (cbtag possibleparents)
  "Complete CBTAG, an object which needs to be completed.
POSSIBLEPARENTS is the list of types which are eligible
to be parents of CBTAG."
  (let* ((parents (semantic-tag-type-superclasses (oref cbtag tag)))
	 (interface (semantic-tag-type-interfaces (oref cbtag tag)))
	 )
    (if (or (not (listp parents))
	    (semantic-tag-p parents))
	(setq parents (list parents)))
    (while parents
      (semantic-cb-find-parent cbtag (car parents) possibleparents)
      (setq parents (cdr parents)))
    (if (or (not (listp interface))
	    (semantic-tag-p interface))
	(setq interface (list interface)))
    (while interface
      (semantic-cb-find-parent cbtag (car interface) possibleparents)
      (setq interface (cdr interface)))
    ))

(defun semantic-cb-find-parent (cbt parentobj possibleparents)
  "For CBT, find the CB object represented by PARENTOBJECT.
PARENTOBJ will be in POSSIBLEPARENTS, or determined to be nil.
If a valid CB object is found, link CBT to the found object."
  (let* ((pstr (cond ((stringp parentobj)
		      parentobj)
		     ((semantic-tag-p parentobj)
		      (semantic-tag-name parentobj))
		     (t
		      ;;(error "Unknown parent object type")
		      ;; Concoct a reasonable default
		      "Cannot Find Parent")))
	 (po (object-assoc pstr :object-name possibleparents))
	 )
    (when po
      ;; List parent as the
      (object-add-to-list cbt 'parents po t)
      ;; List cbt as inheriting from parent
      (object-add-to-list po 'subclasses cbt t)
      )))

(defun semantic-cb-convert-type (tag db parentobj)
  "Convert the semantic TAG to a type object.
DB is the semantic database that TAG is derived from.
PARENTOBJ is the CB object which is the parent of TAG"
  (let* ((chil (cons
		;; This makes a mock DB list
		(cons db (semantic-tag-type-members tag))
		;; External children in DB form.
		(semantic-tag-external-member-children
		 tag t)))
	 ;; This is a created CB object which will represent
	 ;; this type.
	 (tobj (semantic-cb-type
		(semantic-tag-name tag) ; name
		 :tag tag		; The tag
		 :table db		; database table we came from
		 :container parentobj	; parent container
		 )))
    ;; We now have a tag in TOBJ.  Conver the child list
    ;; we just got into a form suitable for a tobj child list.
    (setq chil (semantic-cb-convert-children chil tobj))
    (oset tobj children chil)
    ;; Add ourselves to the list of incomplete types.
    (semantic-cb-add-incomplete-type tobj)
    ;; Return it.
    tobj))

(defun semantic-cb-convert-children (childlist parentobj)
  "Convert CHILDLIST from semantic format to cb objects.
CHILDLIST is in semanticdb search format, such that each
element of the list starts with a database table object.
PARENTOBJ is the CB tag which hosts CHILDLIST."
  (let ((newlist nil))
    (while childlist
      (let ((sublist (cdr (car childlist)))
	    (db (car (car childlist))))
	(while sublist
	  (if (semantic-tag-with-position-p (car sublist))
	      (let ((newtok
		     (cond
		      ((eq (semantic-tag-class (car sublist))
			   'type)
		       (semantic-cb-convert-type (car sublist) db parentobj))
		      (t
		       (semantic-cb-tag
			(semantic-tag-name (car sublist))
			:tag (car sublist)
			:table db
			:container parentobj)))))
		;; We have a new object representing the tag.
		;; add it to the new list.
		(setq newlist (cons newtok newlist))))
	  (setq sublist (cdr sublist))))
      (setq childlist (cdr childlist)))
    (nreverse newlist)))

;;; Methods
;;
(defmethod initialize-instance :AFTER ((this semantic-cb-tag) &rest fields)
  "After initializing THIS, keep overlay properties up to date."
  (let* ((tok (oref this tag))
	 (ol (semantic-tag-overlay tok)))
    ;; Ignore tags that are in the database.
    (when (semantic-overlay-p ol)
      ;; Apply our object onto this overlay for fast
      ;; reference.
      (semantic-overlay-put ol 'semantic-cb this))))

;;; Search by name.
;;
(defun semantic-cb-find-node-in-list (typelist name)
  "Find in TYPELIST an object with NAME."
  (let ((ans nil))
    (while (and (not ans) typelist)
      (setq ans (semantic-cb-find-node (car typelist) name))
      (setq typelist (cdr typelist)))
    ans))

(defmethod semantic-cb-find-node ((this semantic-cb-project) name)
  "Find the node representing a TYPE with NAME."
  (semantic-cb-find-node-in-list (oref this types) name)
  )

(defmethod semantic-cb-find-node ((this semantic-cb-type) name)
  "Find the node representing a TYPE with NAME."
  (if (string= name (oref this object-name))
      this
    (semantic-cb-find-node-in-list (oref this subclasses) name))
  )


;;; Dot Output File Generation for UML
;;
;;;###autoload
(defun semantic-dot (start)
  "Create a DOT graph out of the class browser information.
Argument START specifies the name of the class we are going to start
the graph with."
  (interactive "sDiagram Base Type: ")

  ;;(require 'graphviz-dot)

  (let* ((cb semantic-cb-current-project)
	 (node)
	 (file)
	 (diagramname (concat "uml_" start))
	 )

    (when cb
      (setq node (semantic-cb-find-node cb start)))

    (when (not node)
      (message "Building new class browswer...")
      (setq cb (semantic-cb-new-class-browser)
	    node (semantic-cb-find-node cb start))
      (message "Building new class browswer...done"))

    (when (not node)
      (error "Could not find %s in class browser" start))

    ;; Ok, now we can build a dot file.
    (setq file (make-temp-file "/tmp/semantic_uml_" nil ".dot"))

    (while (string-match "-" diagramname)
      (setq diagramname (replace-match "_" t t diagramname)))

    (save-excursion
      (set-buffer (find-file-noselect file))
    
      ;; Create the content
      (erase-buffer)

      ;; Header
      (insert "// DOT file created by EMACS/Semantic "
	      (current-time-string) "
digraph uml_" diagramname " {\n")

      (insert "\tgraph [ fontname = \"Helvetica\",
	fontsize = 24,
	label = \""
	      (semantic-dot-tag-name node)
	      " and decedents\",
	];\n")

      ;; All the nodes
      (semantic-dot-insert-tag node)

      ;; Finish
      (insert "}")

      (save-buffer)
      )

    ;; Ok, now generate
    ;; Use dotty for now.  Later, create a png, and load it into
    ;; an Emacs buffer instead.
    (save-window-excursion
      (shell-command (concat "dotty " file "&")))

    ))

(defmethod semantic-dot-tag-name ((this semantic-cb-tag))
  "Return DOT code for THIS tag's name."
  (let ((n (oref this object-name)))
    (while (string-match "-" n)
      (setq n (replace-match "_" t t n)))
    n))

(defmethod semantic-dot-insert-tag ((this semantic-cb-tag))
  "Insert DOT code for THIS tag."
  (let ((children (oref this subclasses)))
    ;; Insert a way to draw this object.
    (insert "\t"
	    (semantic-dot-tag-name this)
	    " [shape=record,label=\"{"
	    (semantic-dot-tag-name this)
	    "||}\",fontsize=12];\n")
    ;; Insert all the links for this child.
    (while children
      (insert "\t" (semantic-dot-tag-name this)
	      "-> " (semantic-dot-tag-name (car children))
	      "[ arrowhead=none arrowtail=empty ]"
	      "\n")
      (semantic-dot-insert-tag (car children))
      (setq children (cdr children)))
    ))


;;; Speedbar specific methods
;;
(defmethod eieio-speedbar-object-children ((object semantic-cb-type))
  "Return children for OBJECT."
  (oref object subclasses))

;(defmethod eieio-speedbar-object-children ((object semantic-cb-tag))
;  "Return children of this type."
;  (oref object children))

(defmethod eieio-speedbar-description ((object semantic-cb-tag))
  "Descriptive text for OBJECT."
  (semantic-format-tag-summarize (oref object tag)))

(defmethod eieio-speedbar-derive-line-path ((object semantic-cb-tag))
  "Get the path to OBJECT's instantiation."
  (oref (oref object table) file))

(defmethod eieio-speedbar-handle-click ((object semantic-cb-tag))
  "When clicking on a tag OBJECT, jump to its definition."
  (let ((tag (oref object tag)))
    (speedbar-find-file-in-frame
     (semanticdb-full-filename (oref object table)))
    (save-excursion (speedbar-stealthy-updates))
    (semantic-go-to-tag tag) ;; only positioned objects here.
    (speedbar-maybee-jump-to-attached-frame)
    (run-hooks 'speedbar-visiting-tag-hook)))
	


;;; Speedbar initialization
;;
(defvar semantic-cb-speedbar-key-map (eieio-speedbar-make-map)
  "Extra keybindings used when speedbar displays our class tree.")

(defun semantic-cb-make-map ()
  "Create a keymap and return it."
  nil)

(defvar semantic-cb-speedbar-menu
  (append
   eieio-speedbar-menu
   '([ "Graph This Object" semantic-cb-speedbar-dot ]
     ))
  "Menu additions used in speedbar when displaying a callback hierarchy.")

(defun semantic-cb-speedbar-dot ()
  "Dot the current speedbar line."
  (interactive)
  (let ((o (eieio-speedbar-find-nearest-object)))
    (semantic-dot (oref o object-name)))
  )

(defun semantic-cb-speedbar-buttons (dir)
  "Return the list of object children to display at the toplevel in speedbar.
Argument DIR is the directory speedbar is asking about."
  (if (or (not semantic-cb-current-project)
	  dframe-power-click)
      (speedbar-with-attached-buffer (semantic-cb-new-class-browser)))
  (oref semantic-cb-current-project types))

;;;###autoload
(defun semantic-cb-speedbar-mode ()
  "Bring speedbar up, and put it into Class Browser mode.
This will use the Class Browser logic applied to the current Semantic
project database to build the available relations.  The structure of
the class hierarchy can then be navigated using traditional speedbar
interactions."
  (interactive)
  ;; For some reason when the speedbar buttons are requested, the attached
  ;; frame can be nil for the class browser.  I don't know why nothing else
  ;; is affected by this.  This only happens on the very first call to
  ;; speedbar.  This little hack will "fix" the problem.
  (if (not semantic-cb-current-project)
      (semantic-cb-new-class-browser))
  ;; Do the rest of the init.
  (speedbar-frame-mode 1)
  (speedbar-change-initial-expansion-list "Class Browser")
  (speedbar-get-focus))

(eieio-speedbar-create 'semantic-cb-make-map
		       'semantic-cb-speedbar-key-map
		       'semantic-cb-speedbar-menu
		       "Class Browser"
		       'semantic-cb-speedbar-buttons)

(provide 'semantic-cb)

;;; semantic-cb.el ends here
