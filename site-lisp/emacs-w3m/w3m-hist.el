;;; w3m-hist.el --- the history management system for emacs-w3m

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2008, 2009, 2010
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: w3m, WWW, hypermedia

;; This file is a part of emacs-w3m.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Emacs-w3m keeps history in the buffer-local variables `w3m-history'
;; and `w3m-history-flat'.  Each variable contains a list of all the
;; links you have visited.  The behavior tracing history backward or
;; forward is controlled by the `w3m-history-reuse-history-elements'
;; variable.  See the documentations for those variables for details.

;;; Code:

(eval-when-compile
  (require 'cl))

(defcustom w3m-history-reuse-history-elements nil
  "Non-nil means reuse the history element when re-visiting the page.
Otherwise, a new history element will be created even if there are
elements for the same url in the history.

Emacs-w3m used to operate as the case in which it is non-nil, however
it sometimes brought about users' dissatisfaction.  For example, if a
user visited the pages A -> B -> C -> B in order, performing BACK on
the second B would let a user visit A.  The reason why a user was
taken to A rather than C is that the `w3m-history' variable only had
the list `(A B C)' as a history and B was the current position at that
time.

The default value for this variable is nil which allows the
`w3m-history' variable to have the list `(A B C B)'.  Where contents
of two B's are the identical Lisp objects.  So, too much wasting the
Lisp resources will be avoided.

See the documentation for the variables `w3m-history' and
`w3m-history-flat' for more information."
  :group 'w3m
  :type '(boolean :format "%{%t%}: %[%v%]" :on "On" :off "Off"))

(defcustom w3m-history-minimize-in-new-session nil
  "Non-nil means minimize copied history so that there's only current page.
This variable is effective when creating of the new session by copying
\(i.e., `w3m-copy-buffer')."
  :group 'w3m
  :type '(boolean :format "%{%t%}: %[%v%]" :on "On" :off "Off"))

(defvar w3m-history nil
  "A tree-structured complex list of all the links which you have visited.
This is a buffer-local variable.  For example, it will grow as follows:

\[Branch-1.0.0.0]:                 +--> U1.0.0.0.0 --> U1.0.0.0.1
                                  |
    [Branch-1.0]:         +--> U1.0.0 --> U1.0.1 --> U1.0.2
                          |
         [Trunk]: U0 --> U1 --> U2 --> U3 --> U4 --> U5 --> U6
                                 |
    [Branch-2.0]:                +--> U2.0.0 --> U2.0.1
                                 |
    [Branch-2.1]:                +--> U2.1.0 --> U2.1.1 --> U2.1.2
                                                    |
\[Branch-2.1.1.0]:                                   +--> U2.1.1.0.0

In this case, the U1.0.0.0.0 history element represents the first link
of the first branch which is sprouted from the U1.0.0 history element.

The trunk or each branch is a simple list which will contain some
history elements.  History elements in the trunk or each branch will
be arranged in increasing order (the newest history element will be
the last element of the list).  Each history element represents a link
which consists of the following records:

	(URL PROPERTIES BRANCH BRANCH ...)

Where URL is a string of an address of a link.  PROPERTIES is a plist
which is able to contain any kind of data to supplement the URL as
follows:

	(KEYWORD VALUE KEYWORD VALUE ...)

A note for programmers: PROPERTIES should always be a non-nil value in
order to make it easy to share the value in every history element in
every emacs-w3m buffer.

The remaining BRANCHes are branches of the history element.  Branches
will also be arranged in increasing order (the newest one will be the
rightmost element).  Each BRANCH will also be a tree-structured
complex list.  Therefore, the history structure will grow up
infinitely.

In order to save the Lisp resources, URL strings and PROPERTIES in the
`w3m-history' variables are shared in every emacs-w3m buffer (it means
each element in two `w3m-history' variables can be compared by `eq'
rather than `equal').  If there is necessity to make buffer-local
properties, in other words, to make properties of which values differ
in every emacs-w3m buffer, use the `w3m-history-flat' variable instead.

There are special rules on the emacs-w3m history management system.
As you perhaps foresaw, the operation BACK on U2.0.0 brings you to U2,
and one more BACK brings you to U1.  Well, where do you think we
should go next when the operation FORWARD is performed on U1?  The
rule is to go to the newest link you have ever visited.  So, that
operation should take you to U1.0.0.

Another rule is that the new U4 link should sprout from U1.0.1 if
`w3m-history-reuse-history-elements' is nil when you visit the U4 link
directly from U1.0.1.  In contrast, you should be taken to the
existing U4 link instead of sprouting the new branch from U1.0.1 if
`w3m-history-reuse-history-elements' is non-nil.

In addition, the first element of `w3m-history' is special.  It is a
list containing pointers which point to three history elements as
shown below:

	(PREV CURRENT NEXT)

PREV points to the previous history element, CURRENT points to the
current one and NEXT points to the next one.  Each of them is a list
which contains an odd number of integers.  For example, `(0)' does
point to U0 and `(2 1 0)' does point to U2.1.0.  Finally, the value of
the `w3m-history' variable will be constructed as follows:

\(((1) (2) (2 1 0))
 (\"http://www.U0.org/\" (:title \"U0\" :foo \"bar\"))
 (\"http://www.U1.org/\" (:title \"U1\" :foo \"bar\")
  ((\"http://www.U100.org/\" (:title \"U100\" :foo \"bar\")
    ((\"http://www.U10000.org/\" (:title \"U10000\" :foo \"bar\"))
     (\"http://www.U10001.org/\" (:title \"U10001\" :foo \"bar\"))))
   (\"http://www.U101.org/\" (:title \"U101\" :foo \"bar\"))
   (\"http://www.U102.org/\" (:title \"U102\" :foo \"bar\"))))
 (\"http://www.U2.org/\" (:title \"U2\" :foo \"bar\")
  ((\"http://www.U200.org/\" (:title \"U200\" :foo \"bar\"))
   (\"http://www.U201.org/\" (:title \"U201\" :foo \"bar\")))
  ((\"http://www.U210.org/\" (:title \"U210\" :foo \"bar\"))
   (\"http://www.U211.org/\" (:title \"U211\" :foo \"bar\")
    ((\"http://www.U21100.org/\" (:title \"U21100\" :foo \"bar\"))))
   (\"http://www.U212.org/\" (:title \"U212\" :foo \"bar\"))))
 (\"http://www.U3.org/\" (:title \"U3\" :foo \"bar\"))
 (\"http://www.U4.org/\" (:title \"U4\" :foo \"bar\"))
 (\"http://www.U5.org/\" (:title \"U5\" :foo \"bar\"))
 (\"http://www.U6.org/\" (:title \"U6\" :foo \"bar\")))")

(defvar w3m-history-flat nil
  "A flattened alist of all the links which you have visited.
All history elements except for buffer-local properties are the same
as ones of `w3m-history'.  Each element will contain the following
records:

    (URL PROPERTIES POSITION [KEYWORD VALUE [KEYWORD VALUE ...]])

Where URL is a string of an address of a link, PROPERTIES is a plist
which is able to contain any kind of data to supplement the URL.  Each
PROPERTIES is the Lisp object identical with that corresponding
element of `w3m-history'.  POSITION is a list of integers to designate
the current position in the history.

The remaining [KEYWORD VALUE [KEYWORD VALUE ...]] is a plist similar
to PROPERTIES, but it is buffer-local.  You can manipulate
buffer-local properties using the functions `w3m-history-plist-get',
`w3m-history-plist-put', `w3m-history-add-properties' and
`w3m-history-remove-properties'.  See the documentation for the
`w3m-history' variable for more information.")

(make-variable-buffer-local 'w3m-history)
(make-variable-buffer-local 'w3m-history-flat)

;; Inline functions.
(defsubst w3m-history-assoc (url)
  "Extract a history element associated with URL from `w3m-history-flat'."
  (assoc url w3m-history-flat))

;; Functions for internal use.
(defun w3m-history-set-current (position)
  "Modify `w3m-history' so that POSITION might be the current position.
What is called the current position is the `cadar' of `w3m-history'.
The previous position and the next position will be computed
automatically."
  (setcar w3m-history (w3m-history-regenerate-pointers position)))

(defun w3m-history-element (position &optional flat)
  "Return a history element located in the POSITION of the history.
If FLAT is nil, the value will be extracted from `w3m-history' and
represented with the `(URL PROPERTIES BRANCH BRANCH ...)' form.
Otherwise, the value will be extracted from `w3m-history-flat' and
represented with the `(URL PROPERTIES POSITION [KEYWORD VALUE ...])'
form.  FYI, to know the current position, the `(cadar w3m-history)'
form for example can be used."
  (when position
    (if flat
	(let ((flat w3m-history-flat)
	      element)
	  (while flat
	    (if (equal (caddr (setq element (pop flat))) position)
		(setq flat nil)
	      (setq element nil)))
	  element)
      (let ((element (nth (pop position) (cdr w3m-history))))
	(while position
	  (setq element (nth (pop position) (cddr element))
		element (nth (pop position) element)))
	element))))

(defun w3m-history-previous-position (position)
  "Return a history position of the previous location of POSITION.
POSITION is a list of integers of the same form as being used in one
of the elements of the `car' of `w3m-history' (which see)."
  (let (class number previous)
    (when position
      (setq class (1- (length position))
	    number (nth class position))
      (if (zerop number)
	  ;; This POSITION is the beginning of the branch.
	  (unless (zerop class)
	    ;; There's a parent.
	    (setq previous (copy-sequence position))
	    (setcdr (nthcdr (- class 2) previous) nil))
	;; This POSITION is not the beginning of the branch.
	(setq previous (copy-sequence position))
	(setcar (nthcdr class previous) (1- number))))
    previous))

(defun w3m-history-next-position (position)
  "Return a history position of the next location of POSITION.
POSITION is a list of integers of the same form as being used in one
of the elements of the `car' of `w3m-history' (which see)."
  (let (next branch element number)
    (when position
      (setq next position
	    branch (cdr w3m-history)
	    element (nth (pop next) branch))
      (while next
	(setq branch (nth (pop next) (cddr element))
	      element (nth (pop next) branch)))
      (cond ((nth 2 element)
	     ;; There're branches sprouted from the POSITION.
	     (setq next (copy-sequence position))
	     (setcdr (nthcdr (1- (length next)) next)
		     (list (- (length element) 3) 0)))
	    ((> (length branch)
		(setq number (1+ (nth (1- (length position)) position))))
	     ;; This POSITION is not the end of the branch.
	     (setq next (copy-sequence position))
	     (setcar (nthcdr (1- (length next)) next) number))))
    next))

(defun w3m-history-set-plist (plist property value)
  "Similar to `plist-put' but PLIST is actually modified even in XEmacs.
If VALUE is nil, the pair of PROPERTY and VALUE is removed from PLIST.
Exceptionally, if PLIST is made empty because of removing, it will be
instead set to `(nil nil)'.  Return PLIST itself."
  (let ((pair (memq property plist)))
    (if pair
	(if value
	    (setcar (cdr pair) value)
	  (if (eq (car plist) property)
	      (progn
		(setcar plist (nth 2 plist))
		(setcar (cdr plist) (nth 3 plist))
		(setcdr (cdr plist) (nthcdr 4 plist)))
	    (setcdr (nthcdr (- (length plist) (length pair) 1) plist)
		    (nthcdr 2 pair))))
      (when value
	(setcdr (nthcdr (1- (length plist)) plist) (list property value)))))
  plist)

(defun w3m-history-modify-properties (old new &optional replace)
  "Merge NEW plist into OLD plist and return a modified plist.
If REPLACE is non-nil, OLD will be replaced with NEW.  OLD plist is
modified and also the new value is shared in all the history
elements containing OLD plist.  Properties whose values are nil are
removed from OLD plist, but if OLD plist is made empty because of
removing, it will be instead set to `(nil nil)'."
  (prog1
      old
    (if replace
	(progn
	  (setcar old (car new))
	  (setcdr old (or (cdr new) (list nil))))
      (while new
	(w3m-history-set-plist old (car new) (cadr new))
	(setq new (cddr new))))
    (setq new (copy-sequence old))
    (while new
      (w3m-history-set-plist old (car new) (cadr new))
      (setq new (cddr new)))))

(defun w3m-history-seek-element (url &optional newprops replace)
  "Return a copy of history element corresponding to URL.
Searching is performed in all emacs-w3m buffers and the first match
found is returned.  If REPLACE is nil, NEPROPS will be merged into
properties of an element.  Otherwise, properties of an element will be
replaced with NEWPROPS."
  (let* ((current (current-buffer))
	 (buffers (cons current (delq current (buffer-list))))
	 element)
    (while buffers
      (set-buffer (pop buffers))
      (when (and (eq major-mode 'w3m-mode)
		 (setq element (w3m-history-assoc url)))
	(setq buffers nil)))
    (set-buffer current)
    (prog1
	(copy-sequence element)
      (when element
	(w3m-history-modify-properties (cadr element) newprops replace)))))

;; Generic functions.
(defun w3m-history-previous-link-available-p ()
  "Return non-nil if the previous history element is available."
  (caar w3m-history))

(defun w3m-history-next-link-available-p ()
  "Return non-nil if the next history element is available."
  (caddar w3m-history))

(defun w3m-history-backward (&optional count)
  "Move backward COUNT times in the history structure.
Return a cons of a new history element and new position pointers of
the history.  The position pointers of `w3m-history' will not change.
If COUNT is omitted, it defaults to the number one.  If COUNT is
negative, moving forward is performed.  Return nil if there is no
previous element."
  (when w3m-history
    (let ((oposition (copy-sequence (car w3m-history)))
	  position last)
      (cond ((or (unless count
		   (setq count 1))
		 (> count 0))
	     (while (and (> count 0)
			 (setq position (caar w3m-history)))
	       (w3m-history-set-current (setq last position))
	       (decf count)))
	    ((< count 0)
	     (while (and (< count 0)
			 (setq position (caddar w3m-history)))
	       (w3m-history-set-current (setq last position))
	       (incf count)))
	    (t ;; Don't move.
	     (setq last (cadar w3m-history))))
      (prog1
	  (when last
	    (cons (w3m-history-element (cadar w3m-history))
		  (car w3m-history)))
	(setcar w3m-history oposition)))))

(defun w3m-history-forward (&optional count)
  "Move forward COUNT times in the history structure.
Return a cons of a new history element and new position pointers of
the history.  The position pointers of `w3m-history' will not change.
If COUNT is omitted, it defaults to the number one.  If COUNT is
negative, moving backward is performed.  If there is no room to move
in the history, move as far as possible."
  (w3m-history-backward (- (or count 1))))

(defun w3m-history-regenerate-pointers (position)
  "Regenerate the position pointers due to only the current POSITION.
The history position pointers are made with the `(PREV CURRENT NEXT)'
form which is mentioned in the documentation for `w3m-history'."
  (list (w3m-history-previous-position position)
	position
	(w3m-history-next-position position)))

(defun w3m-history-flat ()
  "Set the value of `w3m-history-flat' due to the value of `w3m-history'.
See also the documentations for those variables."
  (setq w3m-history-flat nil)
  (when w3m-history
    (let ((history (cdr w3m-history))
	  (position (list 0))
	  element branches flag children)
      (while (setq element (pop history))
	(if (stringp (car element))
	    (progn
	      (push (list (car element) (cadr element) (reverse position))
		    w3m-history-flat)
	      (if (setq element (cddr element))
		  (progn
		    (setq history (append element history)
			  position (append (list 0 0) position))
		    (push (length element) branches))
		(setcar position (1+ (car position)))
		(setq flag t)
		(while (and flag
			    children
			    (zerop (setcar children (1- (car children)))))
		  (setq children (cdr children))
		  (if (zerop (setcar branches (1- (car branches))))
		      (progn
			(setq branches (cdr branches)
			      position (cddr position))
			(setcar position (1+ (car position))))
		    (setcar position 0)
		    (setcar (cdr position) (1+ (cadr position)))
		    (setq flag nil)))))
	  (setq history (append element history))
	  (push (length element) children))))
    (setq w3m-history-flat (nreverse w3m-history-flat))))

(defun w3m-history-tree (&optional newpos)
  "Set the value of `w3m-history' due to the value of `w3m-history-flat'.
See also the documentations for those variables.  NEWPOS specifies the
current position of the history.  It defaults to the beginning
position of the history."
  (if w3m-history-flat
      (let ((flat w3m-history-flat)
	    element positions rest position)
	(setq w3m-history (list (list nil nil)))
	(while (setq element (pop flat))
	  (setq positions (caddr element)
		rest w3m-history)
	  (while positions
	    (setq position (pop positions))
	    (unless (> (length rest) position)
	      (setcdr (nthcdr (1- (length rest)) rest)
		      (make-list (- position (length rest) -1)
				 (list nil nil))))
	    (setq rest (nth position rest))
	    (when positions
	      (setq position (pop positions))
	      (unless (> (- (length rest) 2) position)
		(setcdr (nthcdr (1- (length rest)) rest)
			(make-list (- position (length rest) -3)
				   (list (list nil nil)))))
	      (setq rest (nth (+ position 2) rest))))
	  (setcar rest (car element))
	  (setcar (cdr rest) (cadr element)))
	(push 'dummy w3m-history)
	(w3m-history-set-current (or newpos (list 0)))
	w3m-history)
    (setq w3m-history nil)))

(defun w3m-history-push (url &optional newprops replace)
  "Push URL into the history structure.
A history which corresponds to URL becomes the current one.  NEWPROPS
is a plist which supplements URL.  Return a new history position
pointers.  How this function behaves to the history structure (i.e.,
`w3m-history' and `w3m-history-flat') is controlled by the value of
`w3m-history-reuse-history-elements'.

The case where `w3m-history-reuse-history-elements' is nil:
  A new history element is always created.  If there is another
  element corresponding to the same URL, its properties are inherited
  into the new history element.

The case where `w3m-history-reuse-history-elements' is non-nil:
  If there is an element corresponding to URL in the history, it
  becomes the current history element.  Otherwise, this function
  behaves like the case where `w3m-history-reuse-history-elements' is
  nil.

If REPLACE is nil, NEWPROPS is merged into properties of the current
history element.  Otherwise, properties of the current history element
are replaced with NEWPROPS."
  (let ((element (w3m-history-seek-element url newprops replace))
	position class number branch)
    (if element
	(setcdr (cdr element) nil)
      (setq element (list url (w3m-history-modify-properties newprops nil))))
    (cond
     ((null w3m-history)
      ;; The dawn of the history.
      (setq position (list nil (list 0) nil)
	    w3m-history (list position element)
	    w3m-history-flat (list (append element (list (list 0)))))
      position)

     ((and w3m-history-reuse-history-elements
	   (setq position (caddr (w3m-history-assoc url))))
      ;; Reuse the existing history element assigned to the current one.
      ;; The position pointers will be fixed with correct values after
      ;; visiting a page when moving back, moving forward or jumping from
      ;; the about://history/ page.
      (w3m-history-set-current position))

     (t
      ;; Sprout a new history element.
      (setq position (copy-sequence (cadar w3m-history))
	    class (1- (length position))
	    number 0
	    branch (nthcdr (car position) (cdr w3m-history)))
      (while (> class number)
	(setq number (1+ number)
	      branch (nth (nth number position) (cddar branch))
	      number (1+ number)
	      branch (nthcdr (nth number position) branch)))
      (if (cdr branch)
	  ;; We should sprout a new branch.
	  (progn
	    (setq number (1- (length (car branch))))
	    (setcdr (nthcdr class position) (list (1- number) 0))
	    (setcdr (nthcdr number (car branch)) (list (list element))))
	;; The current position is the last of the branch.
	(setcar (nthcdr class position)
		(1+ (car (nthcdr class position))))
	(setcdr branch (list element)))
      (setq w3m-history-flat (nconc w3m-history-flat
				    (list (append element (list position)))))
      (setcar w3m-history (list (cadar w3m-history) position nil))))))

(defun w3m-history-copy (buffer)
  "Copy the history structure from BUFFER to the current buffer.
This function keeps corresponding elements identical Lisp objects
between buffers while copying the frameworks of `w3m-history' and
`w3m-history-flat'.  Exceptionally, buffer-local properties contained
in `w3m-history-flat' will not be copied except for the positions.  If
`w3m-history-minimize-in-new-session' is non-nil, the copied history
structure will be shrunk so that it may contain only the current
history element."
  (let ((current (current-buffer))
	position flat element props window-start rest)
    (set-buffer buffer)
    (when w3m-history
      (setq position (copy-sequence (cadar w3m-history))
	    flat w3m-history-flat))
    (set-buffer current)
    (when position
      (if w3m-history-minimize-in-new-session
	  (progn
	    (setq w3m-history-flat flat
		  element (copy-sequence (w3m-history-element position t)))
	    (setcdr (cdr element) nil)
	    (setq w3m-history (list (list nil (list 0) nil) element)
		  w3m-history-flat (list (append element (list (list 0))))))
	;; Remove buffer-local properties, except for the positions,
	;; from the new `w3m-history-flat'.
	(while flat
	  (setq element (copy-sequence (car flat))
		flat (cdr flat)
		props (cdddr element)
		window-start (plist-get props :window-start))
	  (if window-start
	      (setcdr (cddr element)
		      (list :window-start window-start
			    :position (plist-get props :position)
			    :window-hscroll (plist-get props :window-hscroll)))
	    (setcdr (cddr element) nil))
	  (push element rest))
	(setq w3m-history-flat (nreverse rest))
	(w3m-history-tree position)))))

(defun w3m-history-plist-get (keyword &optional not-buffer-local)
  "Extract a value from the properties of the current history element.
KEYWORD is usually a symbol.  This function returns the value
corresponding to KEYWORD, but it returns nil if the properties don't
contain KEYWORD.  If NOT-BUFFER-LOCAL is nil, this function searches a
value in buffer-local properties, otherwise looks over the global
properties instead."
  (let ((element (w3m-history-element (cadar w3m-history) t)))
    (plist-get (if not-buffer-local
		   (cadr element)
		 (cdddr element))
	       keyword)))

(defun w3m-history-add-properties (newprops &optional not-buffer-local)
  "Add NEWPROPS to the properties of the current history element.
NEWPROPS should be a plist, which is merged into the properties.
Return new properties.  If NOT-BUFFER-LOCAL is nil, NEWPROPS will be
added to the buffer-local properties.  Otherwise, NEWPROPS will be
added to the global properties instead."
  (if not-buffer-local
      (cadr (w3m-history-seek-element
	     (car (w3m-history-element (cadar w3m-history)))
	     newprops))
    (let ((element (w3m-history-element (cadar w3m-history) t))
	  properties)
      (if element
	  (progn
	    (setq properties (cdddr element)
		  properties
		  (if properties
		      (w3m-history-modify-properties properties newprops)
		    ;; Use `w3m-history-modify-properties' to remove
		    ;; keyword-value pairs whose value is nil.
		    (w3m-history-modify-properties newprops nil)))
	    (unless (car properties) ;; check whether it is `(nil nil)'.
	      (setq properties nil))
	    (setcdr (cddr element) properties))
	(message "\
Warning: the history database in this session seems corrupted.")
	(sit-for 1)
	nil))))

(defun w3m-history-plist-put (keyword value &optional not-buffer-local)
  "Put KEYWORD and VALUE into the current history element.
Return new properties.  If NOT-BUFFER-LOCAL is nil, KEYWORD and VALUE
will be put into the buffer-local properties.  Otherwise, KEYWORD and
VALUE will be put into the global properties instead."
  (w3m-history-add-properties (list keyword value) not-buffer-local))

(defun w3m-history-remove-properties (properties &optional not-buffer-local)
  "Remove PROPERTIES from the current history element.
PROPERTIES should be one or more keyword-value pairs (i.e., plist) but
values are ignored (treated as nil).  Return new properties.  If
NOT-BUFFER-LOCAL is nil, the buffer-local properties will be modified.
Otherwise, the global properties will be modified instead."
  (let (rest)
    (while properties
      (setq rest (cons nil (cons (car properties) rest))
	    properties (cddr properties)))
    (w3m-history-add-properties (nreverse rest) not-buffer-local)))

(defun w3m-history-store-position ()
  "Store the current cursor position into the current history element.
Data consist of the position where the window starts and the cursor
position.  Naturally, those should be treated as buffer-local."
  (interactive)
  (when (cadar w3m-history)
    (w3m-history-add-properties
     (list :window-start (window-start)
	   :position (cons (count-lines (point-min) (point-at-bol))
			   (current-column))
	   :window-hscroll (window-hscroll)))
    (when (interactive-p)
      (message "The current cursor position saved"))))

(defun w3m-history-restore-position ()
  "Restore the saved cursor position in the page.
Even if the page has been shrunk (by reloading, for example), somehow
it works although it may not be perfect."
  (interactive)
  (when (cadar w3m-history)
    (let ((start (w3m-history-plist-get :window-start))
	  position window)
      (cond ((and start
		  (setq position (w3m-history-plist-get :position)))
	     (when (<= start (point-max))
	       (setq window (get-buffer-window (current-buffer) 'all-frames))
	       (when window
		 (set-window-start window start)
		 (set-window-hscroll
		  window (or (w3m-history-plist-get :window-hscroll) 0)))
	       (goto-char (point-min))
	       (forward-line (car position))
	       (move-to-column (cdr position))
	       (let ((deactivate-mark nil))
		 (run-hooks 'w3m-after-cursor-move-hook))))
	    ((interactive-p)
	     (message "No cursor position saved"))))))

(defun w3m-history-minimize ()
  "Minimize the history so that there may be the current page only."
  (interactive)
  (let ((position (cadar w3m-history))
	element)
    (when position
      (setq element (w3m-history-element position t))
      (setcar (cddr element) (list 0))
      (setq w3m-history-flat (list element)
	    w3m-history (list (list nil (list 0) nil)
			      (list (car element) (cadr element)))))))

(defun w3m-history-slimmed-history-flat ()
  "Return slimmed history."
  (let ((position (cadar w3m-history))
	flat-map new-flat)
    (dolist (l w3m-history-flat)
      (setq flat-map (cons (cons (nth 2 l) l)
			   flat-map)))
    (setq new-flat (cons (cdr (assoc position flat-map)) nil))
    (let ((pos (w3m-history-previous-position position)))
      (while pos
	(setq new-flat (cons (cdr (assoc pos flat-map))
			     new-flat))
	(setq pos (w3m-history-previous-position pos))))
    (let ((pos (w3m-history-next-position position)))
      (while pos
	(setq new-flat (cons (cdr (assoc pos flat-map))
			     new-flat))
	(setq pos (w3m-history-next-position pos))))
    new-flat))

(defun w3m-history-slim ()
  "Slim the history.
This makes the history slim so that it may have only the pages that
are accessible by PREV and NEXT operations."
  (interactive)
  (let ((position (cadar w3m-history)))
    (setq w3m-history-flat (w3m-history-slimmed-history-flat))
    (w3m-history-tree position)))

(eval-when-compile
  (defvar w3m-arrived-db)
  (autoload 'w3m-goto-url "w3m"))

(defun w3m-history-add-arrived-db ()
  "Add the arrived database to the history structure unreasonably.
This function is useless normally, so you may not want to use it.
\(The reason it is here is because it is useful once in a while when
debugging w3m-hist.el.)"
  (interactive)
  (unless (eq 'w3m-mode major-mode)
    (error "`%s' must be invoked from an emacs-w3m buffer" this-command))
  (when (and w3m-arrived-db
	     (prog1
		 (yes-or-no-p
		  "Are you sure you really want to destroy the history? ")
	       (message "")))
    (setq w3m-history nil
	  w3m-history-flat nil)
    (let ((w3m-history-reuse-history-elements t)
	  url-title title)
      (mapatoms (lambda (symbol)
		  (when symbol
		    (if (setq title (get symbol 'title))
			(push (list (symbol-name symbol)
				    (list :title title))
			      url-title)
		      (push (list (symbol-name symbol)) url-title))))
		w3m-arrived-db)
      (apply 'w3m-history-push (nth (random (length url-title)) url-title))
      (while url-title
	(w3m-history-push (car (nth (random (length w3m-history-flat))
				    w3m-history-flat)))
	(apply 'w3m-history-push (pop url-title))))
    (w3m-goto-url "about://history/" t)))

(provide 'w3m-hist)

;;; w3m-hist.el ends here
