;;; org-capture.el --- Fast note taking in Org       -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2017 Free Software Foundation, Inc.

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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains an alternative implementation of the functionality
;; that used to be provided by org-remember.el.  The implementation is more
;; streamlined, can produce more target types (e.g. plain list items or
;; table lines).  Also, it does not use a temporary buffer for editing
;; the captured entry - instead it uses an indirect buffer that visits
;; the new entry already in the target buffer (this was an idea by Samuel
;; Wales).  John Wiegley's excellent `remember.el' is not needed anymore
;; for this implementation, even though we borrow heavily from its ideas.

;; This implementation heavily draws on ideas by James TD Smith and
;; Samuel Wales, and, of cause, uses John Wiegley's remember.el as inspiration.

;;; TODO

;; - find a clever way to not always insert an annotation maybe a
;;   predicate function that can check for conditions for %a to be
;;   used.  This could be one of the properties.

;; - Should there be plist members that arrange for properties to be
;;   asked for, like James proposed in his RFC?

;;; Code:

(require 'cl-lib)
(require 'org)

(declare-function org-at-encrypted-entry-p "org-crypt" ())
(declare-function org-datetree-find-date-create "org-datetree"
		  (date &optional keep-restriction))
(declare-function org-decrypt-entry "org-crypt" ())
(declare-function org-encrypt-entry "org-crypt" ())
(declare-function org-table-analyze "org-table" ())
(declare-function org-table-current-dline "org-table" ())
(declare-function org-table-goto-line "org-table" (N))

(defvar org-end-time-was-given)
(defvar org-remember-default-headline)
(defvar org-remember-templates)
(defvar org-table-hlines)
(defvar org-table-current-begin-pos)
(defvar dired-buffers)

(defvar org-capture-clock-was-started nil
  "Internal flag, noting if the clock was started.")

(defvar org-capture-last-stored-marker (make-marker)
  "Marker pointing to the entry most recently stored with `org-capture'.")

;; The following variable is scoped dynamically by org-protocol
;; to indicate that the link properties have already been stored
(defvar org-capture-link-is-already-stored nil)

(defvar org-capture-is-refiling nil
  "Non-nil when capture process is refiling an entry.")

(defgroup org-capture nil
  "Options concerning capturing new entries."
  :tag "Org Capture"
  :group 'org)

(defun org-capture-upgrade-templates (templates)
  "Update the template list to the new format.
TEMPLATES is a template list, as in `org-capture-templates'. The
new format unifies all the date/week tree targets into one that
also allows for an optional outline path to specify a target."
  (let ((modified-templates
	 (mapcar
	  (lambda (entry)
	    (pcase entry
	      ;; Match templates with an obsolete "tree" target type. Replace
	      ;; it with common `file+olp-datetree'.  Add new properties
	      ;; (i.e., `:time-prompt' and `:tree-type') if needed.
	      (`(,key ,desc ,type (file+datetree . ,path) ,tpl . ,props)
	       `(,key ,desc ,type (file+olp+datetree ,@path) ,tpl ,@props))
	      (`(,key ,desc ,type (file+datetree+prompt . ,path) ,tpl . ,props)
	       `(,key ,desc ,type (file+olp+datetree ,@path) ,tpl
		      :time-prompt t ,@props))
	      (`(,key ,desc ,type (file+weektree . ,path) ,tpl . ,props)
	       `(,key ,desc ,type (file+olp+datetree ,@path) ,tpl
		      :tree-type week ,@props))
	      (`(,key ,desc ,type (file+weektree+prompt . ,path) ,tpl . ,props)
	       `(,key ,desc ,type (file+olp+datetree ,@path) ,tpl
		      :tree-type week :time-prompt t ,@props))
	      ;; Other templates are left unchanged.
	      (_ entry)))
	  templates)))
    (unless (equal modified-templates templates)
      (message "Deprecated date/weektree capture templates changed to `file+olp+datetree'."))
    modified-templates))

(defcustom org-capture-templates nil
  "Templates for the creation of new entries.

Each entry is a list with the following items:

keys         The keys that will select the template, as a string, characters
             only, for example \"a\" for a template to be selected with a
             single key, or \"bt\" for selection with two keys.  When using
             several keys, keys using the same prefix key must be together
             in the list and preceded by a 2-element entry explaining the
             prefix key, for example

                     (\"b\" \"Templates for marking stuff to buy\")

             The \"C\" key is used by default for quick access to the
             customization of the template variable.  But if you want to use
             that key for a template, you can.

description  A short string describing the template, will be shown during
             selection.

type         The type of entry.  Valid types are:
               entry       an Org node, with a headline.  Will be filed
                           as the child of the target entry or as a
                           top-level entry.
               item        a plain list item, will be placed in the
                           first plain list at the target
                           location.
               checkitem   a checkbox item.  This differs from the
                           plain list item only is so far as it uses a
                           different default template.
               table-line  a new line in the first table at target location.
               plain       text to be inserted as it is.

target       Specification of where the captured item should be placed.
             In Org files, targets usually define a node.  Entries will
             become children of this node, other types will be added to the
             table or list in the body of this node.

             Most target specifications contain a file name.  If that file
             name is the empty string, it defaults to `org-default-notes-file'.
             A file can also be given as a variable or as a function called
             with no argument.  When an absolute path is not specified for a
             target, it is taken as relative to `org-directory'.

             Valid values are:

             (file \"path/to/file\")
                 Text will be placed at the beginning or end of that file

             (id \"id of existing Org entry\")
                 File as child of this entry, or in the body of the entry

             (file+headline \"path/to/file\" \"node headline\")
                 Fast configuration if the target heading is unique in the file

             (file+olp \"path/to/file\" \"Level 1 heading\" \"Level 2\" ...)
                 For non-unique headings, the full outline path is safer

             (file+regexp  \"path/to/file\" \"regexp to find location\")
                 File to the entry matching regexp

             (file+olp+datetree \"path/to/file\" \"Level 1 heading\" ...)
                 Will create a heading in a date tree for today's date.
                 If no heading is given, the tree will be on top level.
                 To prompt for date instead of using TODAY, use the
                 :time-prompt property.  To create a week-tree, use the
                 :tree-type property.

             (file+function \"path/to/file\" function-finding-location)
                 A function to find the right location in the file

             (clock)
                File to the entry that is currently being clocked

             (function function-finding-location)
                Most general way: write your own function which both visits
                the file and moves point to the right location

template     The template for creating the capture item.  If you leave this
             empty, an appropriate default template will be used.  See below
             for more details.  Instead of a string, this may also be one of

                 (file \"/path/to/template-file\")
                 (function function-returning-the-template)

             in order to get a template from a file, or dynamically
             from a function.

The rest of the entry is a property list of additional options.  Recognized
properties are:

 :prepend            Normally newly captured information will be appended at
                     the target location (last child, last table line,
                     last list item...).  Setting this property will
                     change that.

 :immediate-finish   When set, do not offer to edit the information, just
                     file it away immediately.  This makes sense if the
                     template only needs information that can be added
                     automatically.

 :jump-to-captured   When set, jump to the captured entry when finished.

 :empty-lines        Set this to the number of lines the should be inserted
                     before and after the new item.  Default 0, only common
                     other value is 1.

 :empty-lines-before Set this to the number of lines the should be inserted
                     before the new item.  Overrides :empty-lines for the
                     number lines inserted before.

 :empty-lines-after  Set this to the number of lines the should be inserted
                     after the new item.  Overrides :empty-lines for the
                     number of lines inserted after.

 :clock-in           Start the clock in this item.

 :clock-keep         Keep the clock running when filing the captured entry.

 :clock-resume       Start the interrupted clock when finishing the capture.
                     Note that :clock-keep has precedence over :clock-resume.
                     When setting both to t, the current clock will run and
                     the previous one will not be resumed.

 :time-prompt        Prompt for a date/time to be used for date/week trees
                     and when filling the template.

 :tree-type          When `week', make a week tree instead of the month tree.

 :unnarrowed         Do not narrow the target buffer, simply show the
                     full buffer.  Default is to narrow it so that you
                     only see the new stuff.

 :table-line-pos     Specification of the location in the table where the
                     new line should be inserted.  It should be a string like
                     \"II-3\", meaning that the new line should become the
                     third line before the second horizontal separator line.

 :kill-buffer        If the target file was not yet visited by a buffer when
                     capture was invoked, kill the buffer again after capture
                     is finalized.

The template defines the text to be inserted.  Often this is an
Org mode entry (so the first line should start with a star) that
will be filed as a child of the target headline.  It can also be
freely formatted text.  Furthermore, the following %-escapes will
be replaced with content and expanded:

  %[pathname] Insert the contents of the file given by
              `pathname'.  These placeholders are expanded at the very
              beginning of the process so they can be used to extend the
              current template.
  %(sexp)     Evaluate elisp `(sexp)' and replace it with the results.
              Only placeholders pre-existing within the template, or
              introduced with %[pathname] are expanded this way.  Since this
              happens after expanding non-interactive %-escapes, those can
              be used to fill the expression.
  %<...>      The result of format-time-string on the ... format specification.
  %t          Time stamp, date only.  The time stamp is the current time,
              except when called from agendas with `\\[org-agenda-capture]' or
              with `org-capture-use-agenda-date' set.
  %T          Time stamp as above, with date and time.
  %u, %U      Like the above, but inactive time stamps.
  %i          Initial content, copied from the active region.  If %i is
              indented, the entire inserted text will be indented as well.
  %a          Annotation, normally the link created with `org-store-link'.
  %A          Like %a, but prompt for the description part.
  %l          Like %a, but only insert the literal link.
  %c          Current kill ring head.
  %x          Content of the X clipboard.
  %k          Title of currently clocked task.
  %K          Link to currently clocked task.
  %n          User name (taken from the variable `user-full-name').
  %f          File visited by current buffer when org-capture was called.
  %F          Full path of the file or directory visited by current buffer.
  %:keyword   Specific information for certain link types, see below.
  %^g         Prompt for tags, with completion on tags in target file.
  %^G         Prompt for tags, with completion on all tags in all agenda files.
  %^t         Like %t, but prompt for date.  Similarly %^T, %^u, %^U.
              You may define a prompt like: %^{Please specify birthday}t.
              The default date is that of %t, see above.
  %^C         Interactive selection of which kill or clip to use.
  %^L         Like %^C, but insert as link.
  %^{prop}p   Prompt the user for a value for property `prop'.
  %^{prompt}  Prompt the user for a string and replace this sequence with it.
              A default value and a completion table ca be specified like this:
              %^{prompt|default|completion2|completion3|...}.
  %?          After completing the template, position cursor here.
  %\\1 ... %\\N Insert the text entered at the nth %^{prompt}, where N
              is a number, starting from 1.

Apart from these general escapes, you can access information specific to
the link type that is created.  For example, calling `org-capture' in emails
or in Gnus will record the author and the subject of the message, which you
can access with \"%:from\" and \"%:subject\", respectively.  Here is a
complete list of what is recorded for each link type.

Link type               |  Available information
------------------------+------------------------------------------------------
bbdb                    |  %:type %:name %:company
vm, wl, mh, mew, rmail, |  %:type %:subject %:message-id
gnus                    |  %:from %:fromname %:fromaddress
                        |  %:to   %:toname   %:toaddress
                        |  %:fromto (either \"to NAME\" or \"from NAME\")
                        |  %:date %:date-timestamp (as active timestamp)
                        |  %:date-timestamp-inactive (as inactive timestamp)
gnus                    |  %:group, for messages also all email fields
eww, w3, w3m            |  %:type %:url
info                    |  %:type %:file %:node
calendar                |  %:type %:date

When you need to insert a literal percent sign in the template,
you can escape ambiguous cases with a backward slash, e.g., \\%i."
  :group 'org-capture
  :version "24.1"
  :set (lambda (s v) (set s (org-capture-upgrade-templates v)))
  :type
  (let ((file-variants '(choice :tag "Filename       "
				(file :tag "Literal")
				(function :tag "Function")
				(variable :tag "Variable")
				(sexp :tag "Form"))))
  `(repeat
    (choice :value ("" "" entry (file "~/org/notes.org") "")
	    (list :tag "Multikey description"
		  (string :tag "Keys       ")
		  (string :tag "Description"))
	    (list :tag "Template entry"
		  (string :tag "Keys           ")
		  (string :tag "Description    ")
		  (choice :tag "Capture Type   " :value entry
			  (const :tag "Org entry" entry)
			  (const :tag "Plain list item" item)
			  (const :tag "Checkbox item" checkitem)
			  (const :tag "Plain text" plain)
			  (const :tag "Table line" table-line))
		  (choice :tag "Target location"
			  (list :tag "File"
				(const :format "" file)
				,file-variants)
			  (list :tag "ID"
				(const :format "" id)
				(string :tag "  ID"))
			  (list :tag "File & Headline"
				(const :format "" file+headline)
				,file-variants
				(string :tag "  Headline"))
			  (list :tag "File & Outline path"
				(const :format "" file+olp)
				,file-variants
				(repeat :tag "Outline path" :inline t
					(string :tag "Headline")))
			  (list :tag "File & Regexp"
				(const :format "" file+regexp)
				,file-variants
				(regexp :tag "  Regexp"))
			  (list :tag "File [ & Outline path ] & Date tree"
				(const :format "" file+olp+datetree)
				,file-variants
				(option (repeat :tag "Outline path" :inline t
						(string :tag "Headline"))))
			  (list :tag "File & function"
				(const :format "" file+function)
				,file-variants
				(sexp :tag "  Function"))
			  (list :tag "Current clocking task"
				(const :format "" clock))
			  (list :tag "Function"
				(const :format "" function)
				(sexp :tag "  Function")))
		  (choice :tag "Template       "
			  (string)
			  (list :tag "File"
				(const :format "" file)
				(file :tag "Template file"))
			  (list :tag "Function"
				(const :format "" function)
				(function :tag "Template function")))
		  (plist :inline t
			 ;; Give the most common options as checkboxes
			 :options (((const :format "%v " :prepend) (const t))
				   ((const :format "%v " :immediate-finish) (const t))
				   ((const :format "%v " :jump-to-captured) (const t))
				   ((const :format "%v " :empty-lines) (const 1))
				   ((const :format "%v " :empty-lines-before) (const 1))
				   ((const :format "%v " :empty-lines-after) (const 1))
				   ((const :format "%v " :clock-in) (const t))
				   ((const :format "%v " :clock-keep) (const t))
				   ((const :format "%v " :clock-resume) (const t))
				   ((const :format "%v " :time-prompt) (const t))
				   ((const :format "%v " :tree-type) (const week))
				   ((const :format "%v " :unnarrowed) (const t))
				   ((const :format "%v " :table-line-pos) (string))
				   ((const :format "%v " :kill-buffer) (const t)))))))))

(defcustom org-capture-before-finalize-hook nil
  "Hook that is run right before a capture process is finalized.
The capture buffer is still current when this hook runs and it is
widened to the entire buffer."
  :group 'org-capture
  :version "24.1"
  :type 'hook)

(defcustom org-capture-after-finalize-hook nil
  "Hook that is run right after a capture process is finalized.
Suitable for window cleanup."
  :group 'org-capture
  :version "24.1"
  :type 'hook)

(defcustom org-capture-prepare-finalize-hook nil
  "Hook that is run before the finalization starts.
The capture buffer is current and still narrowed."
  :group 'org-capture
  :version "24.1"
  :type 'hook)

(defcustom org-capture-bookmark t
  "When non-nil, add a bookmark pointing at the last stored
position when capturing."
  :group 'org-capture
  :version "24.3"
  :type 'boolean)

;;; The property list for keeping information about the capture process

(defvar org-capture-plist nil
  "Plist for the current capture process, global, to avoid having to pass it.")

(defvar org-capture-current-plist nil
  "Local variable holding the plist in a capture buffer.
This is used to store the plist for use when finishing a capture process
because another such process might have changed the global variable by then.

Each time a new capture buffer has been set up, the global `org-capture-plist'
is copied to this variable, which is local in the indirect buffer.")

(defvar org-capture-clock-keep nil
  "Local variable to store the value of the :clock-keep parameter.
This is needed in case org-capture-finalize is called interactively.")

(defun org-capture-put (&rest stuff)
  "Add properties to the capture property list `org-capture-plist'."
  (while stuff
    (setq org-capture-plist (plist-put org-capture-plist
				       (pop stuff) (pop stuff)))))
(defun org-capture-get (prop &optional local)
  "Get properties from the capture property list `org-capture-plist'.
When LOCAL is set, use the local variable `org-capture-current-plist',
this is necessary after initialization of the capture process,
to avoid conflicts with other active capture processes."
  (plist-get (if local org-capture-current-plist org-capture-plist) prop))

(defun org-capture-member (prop &optional local)
  "Is PROP a property in `org-capture-plist'.
When LOCAL is set, use the local variable `org-capture-current-plist',
this is necessary after initialization of the capture process,
to avoid conflicts with other active capture processes."
  (plist-get (if local org-capture-current-plist org-capture-plist) prop))

;;; The minor mode

(defvar org-capture-mode-map (make-sparse-keymap)
  "Keymap for `org-capture-mode', a minor mode.
Use this map to set additional keybindings for when Org mode is used
for a capture buffer.")

(defvar org-capture-mode-hook nil
  "Hook for the minor `org-capture-mode'.")

(define-minor-mode org-capture-mode
  "Minor mode for special key bindings in a capture buffer.

Turning on this mode runs the normal hook `org-capture-mode-hook'."
  nil " Rem" org-capture-mode-map
  (setq-local
   header-line-format
   (substitute-command-keys
    "\\<org-capture-mode-map>Capture buffer.  Finish \
`\\[org-capture-finalize]', refile `\\[org-capture-refile]', \
abort `\\[org-capture-kill]'.")))
(define-key org-capture-mode-map "\C-c\C-c" 'org-capture-finalize)
(define-key org-capture-mode-map "\C-c\C-k" 'org-capture-kill)
(define-key org-capture-mode-map "\C-c\C-w" 'org-capture-refile)

;;; The main commands

(defvar org-capture-initial nil)
(defvar org-capture-entry nil)

;;;###autoload
(defun org-capture-string (string &optional keys)
  "Capture STRING with the template selected by KEYS."
  (interactive "sInitial text: \n")
  (let ((org-capture-initial string)
	(org-capture-entry (org-capture-select-template keys)))
    (org-capture)))

(defcustom org-capture-templates-contexts nil
  "Alist of capture templates and valid contexts.

For example, if you have a capture template \"c\" and you want
this template to be accessible only from `message-mode' buffers,
use this:

   \\='((\"c\" ((in-mode . \"message-mode\"))))

Here are the available contexts definitions:

      in-file: command displayed only in matching files
      in-mode: command displayed only in matching modes
  not-in-file: command not displayed in matching files
  not-in-mode: command not displayed in matching modes
    in-buffer: command displayed only in matching buffers
not-in-buffer: command not displayed in matching buffers
   [function]: a custom function taking no argument

If you define several checks, the agenda command will be
accessible if there is at least one valid check.

You can also bind a key to another agenda custom command
depending on contextual rules.

    \\='((\"c\" \"d\" ((in-mode . \"message-mode\"))))

Here it means: in `message-mode buffers', use \"c\" as the
key for the capture template otherwise associated with \"d\".
\(The template originally associated with \"d\" is not displayed
to avoid duplicates.)"
  :version "24.3"
  :group 'org-capture
  :type '(repeat (list :tag "Rule"
		       (string :tag "        Capture key")
		       (string :tag "Replace by template")
		       (repeat :tag "Available when"
			      (choice
			       (cons :tag "Condition"
				     (choice
				      (const :tag "In file" in-file)
				      (const :tag "Not in file" not-in-file)
				      (const :tag "In buffer" in-buffer)
				      (const :tag "Not in buffer" not-in-buffer)
				      (const :tag "In mode" in-mode)
				      (const :tag "Not in mode" not-in-mode))
				     (regexp))
			       (function :tag "Custom function"))))))

(defcustom org-capture-use-agenda-date nil
  "Non-nil means use the date at point when capturing from agendas.
When nil, you can still capture using the date at point with
`\\[org-agenda-capture]'."
  :group 'org-capture
  :version "24.3"
  :type 'boolean)

;;;###autoload
(defun org-capture (&optional goto keys)
  "Capture something.
\\<org-capture-mode-map>
This will let you select a template from `org-capture-templates', and
then file the newly captured information.  The text is immediately
inserted at the target location, and an indirect buffer is shown where
you can edit it.  Pressing `\\[org-capture-finalize]' brings you back to the \
previous
state of Emacs, so that you can continue your work.

When called interactively with a `\\[universal-argument]' prefix argument \
GOTO, don't
capture anything, just go to the file/headline where the selected
template stores its notes.

With a `\\[universal-argument] \\[universal-argument]' prefix argument, go to \
the last note stored.

When called with a `C-0' (zero) prefix, insert a template at point.

When called with a `C-1' (one) prefix, force prompting for a date when
a datetree entry is made.

ELisp programs can set KEYS to a string associated with a template
in `org-capture-templates'.  In this case, interactive selection
will be bypassed.

If `org-capture-use-agenda-date' is non-nil, capturing from the
agenda will use the date at point as the default date.  Then, a
`C-1' prefix will tell the capture process to use the HH:MM time
of the day at point (if any) or the current HH:MM time."
  (interactive "P")
  (when (and org-capture-use-agenda-date
	     (eq major-mode 'org-agenda-mode))
    (setq org-overriding-default-time
	  (org-get-cursor-date (equal goto 1))))
  (cond
   ((equal goto '(4)) (org-capture-goto-target))
   ((equal goto '(16)) (org-capture-goto-last-stored))
   (t
    (let* ((orig-buf (current-buffer))
	   (annotation (if (and (boundp 'org-capture-link-is-already-stored)
				org-capture-link-is-already-stored)
			   (plist-get org-store-link-plist :annotation)
			 (ignore-errors (org-store-link nil))))
	   (entry (or org-capture-entry (org-capture-select-template keys)))
	   initial)
      (setq initial (or org-capture-initial
			(and (org-region-active-p)
			     (buffer-substring (point) (mark)))))
      (when (stringp initial)
	(remove-text-properties 0 (length initial) '(read-only t) initial))
      (when (stringp annotation)
	(remove-text-properties 0 (length annotation)
				'(read-only t) annotation))
      (cond
       ((equal entry "C")
	(customize-variable 'org-capture-templates))
       ((equal entry "q")
	(user-error "Abort"))
       (t
	(org-capture-set-plist entry)
	(org-capture-get-template)
	(org-capture-put :original-buffer orig-buf
			 :original-file (or (buffer-file-name orig-buf)
					    (and (featurep 'dired)
						 (car (rassq orig-buf
							     dired-buffers))))
			 :original-file-nondirectory
			 (and (buffer-file-name orig-buf)
			      (file-name-nondirectory
			       (buffer-file-name orig-buf)))
			 :annotation annotation
			 :initial initial
			 :return-to-wconf (current-window-configuration)
			 :default-time
			 (or org-overriding-default-time
			     (org-current-time)))
	(org-capture-set-target-location)
	(condition-case error
	    (org-capture-put :template (org-capture-fill-template))
	  ((error quit)
	   (if (get-buffer "*Capture*") (kill-buffer "*Capture*"))
	   (error "Capture abort: %s" error)))

	(setq org-capture-clock-keep (org-capture-get :clock-keep))
	(if (equal goto 0)
	    ;;insert at point
	    (org-capture-insert-template-here)
	  (condition-case error
	      (org-capture-place-template
	       (eq (car (org-capture-get :target)) 'function))
	    ((error quit)
	     (if (and (buffer-base-buffer (current-buffer))
		      (string-prefix-p "CAPTURE-" (buffer-name)))
		 (kill-buffer (current-buffer)))
	     (set-window-configuration (org-capture-get :return-to-wconf))
	     (error "Capture template `%s': %s"
		    (org-capture-get :key)
		    (nth 1 error))))
	  (if (and (derived-mode-p 'org-mode)
		   (org-capture-get :clock-in))
	      (condition-case nil
		  (progn
		    (if (org-clock-is-active)
			(org-capture-put :interrupted-clock
					 (copy-marker org-clock-marker)))
		    (org-clock-in)
		    (setq-local org-capture-clock-was-started t))
		(error
		 "Could not start the clock in this capture buffer")))
	  (if (org-capture-get :immediate-finish)
	      (org-capture-finalize)))))))))

(defun org-capture-get-template ()
  "Get the template from a file or a function if necessary."
  (let ((txt (org-capture-get :template)) file)
    (cond
     ((and (listp txt) (eq (car txt) 'file))
      (if (file-exists-p
	   (setq file (expand-file-name (nth 1 txt) org-directory)))
	  (setq txt (org-file-contents file))
	(setq txt (format "* Template file %s not found" (nth 1 txt)))))
     ((and (listp txt) (eq (car txt) 'function))
      (if (fboundp (nth 1 txt))
	  (setq txt (funcall (nth 1 txt)))
	(setq txt (format "* Template function %s not found" (nth 1 txt)))))
     ((not txt) (setq txt ""))
     ((stringp txt))
     (t (setq txt "* Invalid capture template")))
    (org-capture-put :template txt)))

(defun org-capture-finalize (&optional stay-with-capture)
  "Finalize the capture process.
With prefix argument STAY-WITH-CAPTURE, jump to the location of the
captured item after finalizing."
  (interactive "P")
  (when (org-capture-get :jump-to-captured)
    (setq stay-with-capture t))
  (unless (and org-capture-mode
	       (buffer-base-buffer (current-buffer)))
    (error "This does not seem to be a capture buffer for Org mode"))

  (run-hooks 'org-capture-prepare-finalize-hook)

  ;; Did we start the clock in this capture buffer?
  (when (and org-capture-clock-was-started
	     org-clock-marker (marker-buffer org-clock-marker)
	     (equal (marker-buffer org-clock-marker) (buffer-base-buffer))
	     (> org-clock-marker (point-min))
	     (< org-clock-marker (point-max)))
    ;; Looks like the clock we started is still running.  Clock out.
    (when (not org-capture-clock-keep) (let (org-log-note-clock-out) (org-clock-out)))
    (when (and (not org-capture-clock-keep)
	       (org-capture-get :clock-resume 'local)
	       (markerp (org-capture-get :interrupted-clock 'local))
	       (buffer-live-p (marker-buffer
			       (org-capture-get :interrupted-clock 'local))))
      (let ((clock-in-task (org-capture-get :interrupted-clock 'local)))
	(org-with-point-at clock-in-task
	  (org-clock-in)))
      (message "Interrupted clock has been resumed")))

  (let ((beg (point-min))
	(end (point-max))
	(abort-note nil))
    ;; Store the size of the capture buffer
    (org-capture-put :captured-entry-size (- (point-max) (point-min)))
    (widen)
    ;; Store the insertion point in the target buffer
    (org-capture-put :insertion-point (point))

    (if org-note-abort
	(let ((m1 (org-capture-get :begin-marker 'local))
	      (m2 (org-capture-get :end-marker 'local)))
	  (if (and m1 m2 (= m1 beg) (= m2 end))
	      (progn
		(setq m2 (if (cdr (assq 'heading org-blank-before-new-entry))
			     m2 (1+ m2))
		      m2 (if (< (point-max) m2) (point-max) m2))
		(setq abort-note 'clean)
		(kill-region m1 m2))
	    (setq abort-note 'dirty)))

      ;; Postprocessing:  Update Statistics cookies, do the sorting
      (when (derived-mode-p 'org-mode)
	(save-excursion
	  (when (ignore-errors (org-back-to-heading))
	    (org-update-parent-todo-statistics)
	    (org-update-checkbox-count)))
	;; FIXME Here we should do the sorting
	;; If we have added a table line, maybe recompute?
	(when (and (eq (org-capture-get :type 'local) 'table-line)
		   (org-at-table-p))
	  (if (org-table-get-stored-formulas)
	      (org-table-recalculate 'all) ;; FIXME: Should we iterate???
	    (org-table-align))))
      ;; Store this place as the last one where we stored something
      ;; Do the marking in the base buffer, so that it makes sense after
      ;; the indirect buffer has been killed.
      (org-capture-store-last-position)

      ;; Run the hook
      (run-hooks 'org-capture-before-finalize-hook))

    (when (org-capture-get :decrypted)
      (save-excursion
	(goto-char (org-capture-get :decrypted))
	(org-encrypt-entry)))

    ;; Kill the indirect buffer
    (save-buffer)
    (let ((return-wconf (org-capture-get :return-to-wconf 'local))
	  (new-buffer (org-capture-get :new-buffer 'local))
	  (kill-buffer (org-capture-get :kill-buffer 'local))
	  (base-buffer (buffer-base-buffer (current-buffer))))

      ;; Kill the indirect buffer
      (kill-buffer (current-buffer))

      ;; Narrow back the target buffer to its previous state
      (with-current-buffer (org-capture-get :buffer)
        (let ((reg (org-capture-get :initial-target-region))
	      (pos (org-capture-get :initial-target-position))
	      (ipt (org-capture-get :insertion-point))
	      (size (org-capture-get :captured-entry-size)))
	  (if (not reg)
	      (widen)
	    (cond ((< ipt (car reg))
		   ;; insertion point is before the narrowed region
		   (narrow-to-region (+ size (car reg)) (+ size (cdr reg))))
		  ((> ipt (cdr reg))
		   ;; insertion point is after the narrowed region
		   (narrow-to-region (car reg) (cdr reg)))
		  (t
		   ;; insertion point is within the narrowed region
		   (narrow-to-region (car reg) (+ size (cdr reg)))))
	    ;; now place back the point at its original position
	    (if (< ipt (car reg))
		(goto-char (+ size pos))
	      (goto-char (if (< ipt pos) (+ size pos) pos))))))

      ;; Kill the target buffer if that is desired
      (when (and base-buffer new-buffer kill-buffer)
	(with-current-buffer base-buffer (save-buffer))
	(kill-buffer base-buffer))

      ;; Restore the window configuration before capture
      (set-window-configuration return-wconf))

    (run-hooks 'org-capture-after-finalize-hook)
    ;; Special cases
    (cond
     (abort-note
      (cl-case abort-note
	(clean
	 (message "Capture process aborted and target buffer cleaned up"))
	(dirty
	 (error "Capture process aborted, but target buffer could not be \
cleaned up correctly"))))
     (stay-with-capture
      (org-capture-goto-last-stored)))
    ;; Return if we did store something
    (not abort-note)))

(defun org-capture-refile ()
  "Finalize the current capture and then refile the entry.
Refiling is done from the base buffer, because the indirect buffer is then
already gone.  Any prefix argument will be passed to the refile command."
  (interactive)
  (unless (eq (org-capture-get :type 'local) 'entry)
    (user-error "Refiling from a capture buffer makes only sense \
for `entry'-type templates"))
  (let* ((base (or (buffer-base-buffer) (current-buffer)))
	 (pos (make-marker))
	 (org-capture-is-refiling t)
	 (kill-buffer (org-capture-get :kill-buffer 'local))
	 (jump-to-captured (org-capture-get :jump-to-captured 'local)))
    ;; Since `org-capture-finalize' may alter buffer contents (e.g.,
    ;; empty lines) around entry, use a marker to refer to the
    ;; headline to be refiled.  Place the marker in the base buffer,
    ;; as the current indirect one is going to be killed.
    (set-marker pos (save-excursion (org-back-to-heading t) (point)) base)
    ;; `org-capture-finalize' calls `org-capture-goto-last-stored' too
    ;; early.  We want to wait for the refiling to be over, so we
    ;; control when the latter function is called.
    (org-capture-put :kill-buffer nil :jump-to-captured nil)
    (unwind-protect
	(progn
	  (org-capture-finalize)
	  (save-window-excursion
	    (with-current-buffer base
	      (org-with-wide-buffer
	       (goto-char pos)
	       (call-interactively 'org-refile))))
	  (when kill-buffer (kill-buffer base))
	  (when jump-to-captured (org-capture-goto-last-stored)))
      (set-marker pos nil))))

(defun org-capture-kill ()
  "Abort the current capture process."
  (interactive)
  ;; FIXME: This does not do the right thing, we need to remove the
  ;; new stuff by hand it is easy: undo, then kill the buffer
  (let ((org-note-abort t)
	(org-capture-before-finalize-hook nil))
    (org-capture-finalize)))

(defun org-capture-goto-last-stored ()
  "Go to the location where the last capture note was stored."
  (interactive)
  (org-goto-marker-or-bmk org-capture-last-stored-marker
			  (plist-get org-bookmark-names-plist
				 :last-capture))
  (message "This is the last note stored by a capture process"))

;;; Supporting functions for handling the process

(defun org-capture-put-target-region-and-position ()
  "Store the initial region with `org-capture-put'."
  (org-capture-put
   :initial-target-region
   ;; Check if the buffer is currently narrowed
   (when (org-buffer-narrowed-p)
     (cons (point-min) (point-max))))
  ;; store the current point
  (org-capture-put :initial-target-position (point)))

(defvar org-time-was-given) ; dynamically scoped parameter
(defun org-capture-set-target-location (&optional target)
  "Find TARGET buffer and position.
Store them in the capture property list."
  (let ((target-entry-p t))
    (save-excursion
      (pcase (or target (org-capture-get :target))
	(`(file ,path)
	 (set-buffer (org-capture-target-buffer path))
	 (org-capture-put-target-region-and-position)
	 (widen)
	 (setq target-entry-p nil))
	(`(id ,id)
	 (pcase (org-id-find id)
	   (`(,path . ,position)
	    (set-buffer (org-capture-target-buffer path))
	    (widen)
	    (org-capture-put-target-region-and-position)
	    (goto-char position))
	   (_ (error "Cannot find target ID \"%s\"" id))))
	(`(file+headline ,path ,headline)
	 (set-buffer (org-capture-target-buffer path))
	 (unless (derived-mode-p 'org-mode)
	   (error "Target buffer \"%s\" for file+headline not in Org mode"
		  (current-buffer)))
	 (org-capture-put-target-region-and-position)
	 (widen)
	 (goto-char (point-min))
	 (if (re-search-forward (format org-complex-heading-regexp-format
					(regexp-quote headline))
				nil t)
	     (goto-char (line-beginning-position))
	   (goto-char (point-max))
	   (or (bolp) (insert "\n"))
	   (insert "* " headline "\n")
	   (beginning-of-line 0)))
	(`(file+olp ,path . ,outline-path)
	 (let ((m (org-find-olp (cons (org-capture-expand-file path)
				      outline-path))))
	   (set-buffer (marker-buffer m))
	   (org-capture-put-target-region-and-position)
	   (widen)
	   (goto-char m)
	   (set-marker m nil)))
	(`(file+regexp ,path ,regexp)
	 (set-buffer (org-capture-target-buffer path))
	 (org-capture-put-target-region-and-position)
	 (widen)
	 (goto-char (point-min))
	 (if (not (re-search-forward regexp nil t))
	     (error "No match for target regexp in file %s" path)
	   (goto-char (if (org-capture-get :prepend)
			  (match-beginning 0)
			(match-end 0)))
	   (org-capture-put :exact-position (point))
	   (setq target-entry-p
		 (and (derived-mode-p 'org-mode) (org-at-heading-p)))))
	(`(file+olp+datetree ,path . ,outline-path)
	 (let ((m (if outline-path
		      (org-find-olp (cons (org-capture-expand-file path)
					  outline-path))
		    (set-buffer (org-capture-target-buffer path))
		    (point-marker))))
	   (set-buffer (marker-buffer m))
	   (org-capture-put-target-region-and-position)
	   (widen)
	   (goto-char m)
	   (set-marker m nil)
	   (require 'org-datetree)
	   (org-capture-put-target-region-and-position)
	   (widen)
	   ;; Make a date/week tree entry, with the current date (or
	   ;; yesterday, if we are extending dates for a couple of hours)
	   (funcall
	    (if (eq (org-capture-get :tree-type) 'week)
		#'org-datetree-find-iso-week-create
	      #'org-datetree-find-date-create)
	    (calendar-gregorian-from-absolute
	     (cond
	      (org-overriding-default-time
	       ;; Use the overriding default time.
	       (time-to-days org-overriding-default-time))
	      ((or (org-capture-get :time-prompt)
		   (equal current-prefix-arg 1))
	       ;; Prompt for date.
	       (let ((prompt-time (org-read-date
				   nil t nil "Date for tree entry:"
				   (current-time))))
		 (org-capture-put
		  :default-time
		  (cond ((and (or (not (boundp 'org-time-was-given))
				  (not org-time-was-given))
			      (not (= (time-to-days prompt-time) (org-today))))
			 ;; Use 00:00 when no time is given for another
			 ;; date than today?
			 (apply #'encode-time
				(append '(0 0 0)
					(cl-cdddr (decode-time prompt-time)))))
			((string-match "\\([^ ]+\\)--?[^ ]+[ ]+\\(.*\\)"
				       org-read-date-final-answer)
			 ;; Replace any time range by its start.
			 (apply #'encode-time
				(org-read-date-analyze
				 (replace-match "\\1 \\2" nil nil
						org-read-date-final-answer)
				 prompt-time (decode-time prompt-time))))
			(t prompt-time)))
		 (time-to-days prompt-time)))
	      (t
	       ;; Current date, possibly corrected for late night
	       ;; workers.
	       (org-today))))
	    ;; the following is the keep-restriction argument for
	    ;; org-datetree-find-date-create
	    (if outline-path 'subtree-at-point))))
	(`(file+function ,path ,function)
	 (set-buffer (org-capture-target-buffer path))
	 (org-capture-put-target-region-and-position)
	 (widen)
	 (funcall function)
	 (org-capture-put :exact-position (point))
	 (setq target-entry-p
	       (and (derived-mode-p 'org-mode) (org-at-heading-p))))
	(`(function ,fun)
	 (funcall fun)
	 (org-capture-put :exact-position (point))
	 (setq target-entry-p
	       (and (derived-mode-p 'org-mode) (org-at-heading-p))))
	(`(clock)
	 (if (and (markerp org-clock-hd-marker)
		  (marker-buffer org-clock-hd-marker))
	     (progn (set-buffer (marker-buffer org-clock-hd-marker))
		    (org-capture-put-target-region-and-position)
		    (widen)
		    (goto-char org-clock-hd-marker))
	   (error "No running clock that could be used as capture target")))
	(target (error "Invalid capture target specification: %S" target)))

      (org-capture-put :buffer (current-buffer)
		       :pos (point)
		       :target-entry-p target-entry-p
		       :decrypted
		       (and (featurep 'org-crypt)
			    (org-at-encrypted-entry-p)
			    (save-excursion
			      (org-decrypt-entry)
			      (and (org-back-to-heading t) (point))))))))

(defun org-capture-expand-file (file)
  "Expand functions, symbols and file names for FILE.
When FILE is a function, call it.  When it is a form, evaluate
it.  When it is a variable, return its value.  When it is
a string, treat it as a file name, possibly expanding it
according to `org-directory', and return it.  If it is the empty
string, however, return `org-default-notes-file'.  In any other
case, raise an error."
  (let ((location (cond ((equal file "") org-default-notes-file)
			((stringp file) (expand-file-name file org-directory))
			((functionp file) (funcall file))
			((and (symbolp file) (boundp file)) (symbol-value file))
			(t nil))))
    (or (org-string-nw-p location)
	(error "Invalid file location: %S" location))))

(defun org-capture-target-buffer (file)
  "Get a buffer for FILE.
FILE is a generalized file location, as handled by
`org-capture-expand-file'."
  (let ((file (org-capture-expand-file file)))
    (or (org-find-base-buffer-visiting file)
	(progn (org-capture-put :new-buffer t)
	       (find-file-noselect file)))))

(defun org-capture-place-template (&optional inhibit-wconf-store)
  "Insert the template at the target location, and display the buffer.
When `inhibit-wconf-store', don't store the window configuration, as it
may have been stored before."
  (unless inhibit-wconf-store
    (org-capture-put :return-to-wconf (current-window-configuration)))
  (delete-other-windows)
  (org-switch-to-buffer-other-window
   (org-capture-get-indirect-buffer (org-capture-get :buffer) "CAPTURE"))
  (widen)
  (outline-show-all)
  (goto-char (org-capture-get :pos))
  (setq-local outline-level 'org-outline-level)
  (pcase (org-capture-get :type)
    ((or `nil `entry) (org-capture-place-entry))
    (`table-line (org-capture-place-table-line))
    (`plain (org-capture-place-plain-text))
    (`item (org-capture-place-item))
    (`checkitem (org-capture-place-item)))
  (org-capture-mode 1)
  (setq-local org-capture-current-plist org-capture-plist))

(defun org-capture-place-entry ()
  "Place the template as a new Org entry."
  (let ((reversed? (org-capture-get :prepend))
	(level 1))
    (when (org-capture-get :exact-position)
      (goto-char (org-capture-get :exact-position)))
    (cond
     ;; Insert as a child of the current entry.
     ((org-capture-get :target-entry-p)
      (setq level (org-get-valid-level
		   (if (org-at-heading-p) (org-outline-level) 1)
		   1))
      (if reversed? (outline-next-heading) (org-end-of-subtree t)))
     ;; Insert as a top-level entry at the beginning of the file.
     (reversed?
      (goto-char (point-min))
      (unless (org-at-heading-p) (outline-next-heading)))
     ;; Otherwise, insert as a top-level entry at the end of the file.
     (t (goto-char (point-max))))
    (unless (bolp) (insert "\n"))
    (org-capture-empty-lines-before)
    (let ((beg (point))
	  (template (org-capture-get :template)))
      (org-capture-verify-tree template)
      (org-paste-subtree level template 'for-yank)
      (org-capture-empty-lines-after)
      (org-capture-position-for-last-stored beg)
      (unless (org-at-heading-p) (outline-next-heading))
      (let ((end (point)))
	(org-capture-mark-kill-region beg end)
	(org-capture-narrow beg end)
	(when (or (re-search-backward "%\\?" beg t)
		  (re-search-forward "%\\?" end t))
	  (replace-match ""))))))

(defun org-capture-place-item ()
  "Place the template as a new plain list item."
  (let* ((txt (org-capture-get :template))
	 (target-entry-p (org-capture-get :target-entry-p))
	 (ind 0)
	 beg end)
    (if (org-capture-get :exact-position)
	(goto-char (org-capture-get :exact-position))
      (cond
       ((not target-entry-p)
	;; Insert as top-level entry, either at beginning or at end of file
	(setq beg (point-min) end (point-max)))
       (t
	(setq beg (1+ (point-at-eol))
	      end (save-excursion (outline-next-heading) (point)))))
      (setq ind nil)
      (if (org-capture-get :prepend)
	  (progn
	    (goto-char beg)
	    (when (org-list-search-forward (org-item-beginning-re) end t)
	      (goto-char (match-beginning 0))
	      (setq ind (org-get-indentation))))
	(goto-char end)
	(when (org-list-search-backward (org-item-beginning-re) beg t)
	  (setq ind (org-get-indentation))
	  (org-end-of-item)))
      (unless ind (goto-char end)))
    ;; Remove common indentation
    (setq txt (org-remove-indentation txt))
    ;; Make sure this is indeed an item
    (unless (string-match (concat "\\`" (org-item-re)) txt)
      (setq txt (concat "- "
			(mapconcat 'identity (split-string txt "\n")
				   "\n  "))))
    ;; Prepare surrounding empty lines.
    (unless (bolp) (insert "\n"))
    (org-capture-empty-lines-before)
    (setq beg (point))
    (unless (eolp) (save-excursion (insert "\n")))
    (unless ind
      (org-indent-line)
      (setq ind (org-get-indentation))
      (delete-region beg (point)))
    ;; Set the correct indentation, depending on context
    (setq ind (make-string ind ?\ ))
    (setq txt (concat ind
		      (mapconcat 'identity (split-string txt "\n")
				 (concat "\n" ind))
		      "\n"))
    ;; Insert item.
    (insert txt)
    (org-capture-empty-lines-after)
    (org-capture-position-for-last-stored beg)
    (setq end (point))
    (org-capture-mark-kill-region beg end)
    (org-capture-narrow beg end)
    (if (or (re-search-backward "%\\?" beg t)
	    (re-search-forward "%\\?" end t))
	(replace-match ""))))

(defun org-capture-place-table-line ()
  "Place the template as a table line."
  (require 'org-table)
  (let* ((txt (org-capture-get :template))
	 (target-entry-p (org-capture-get :target-entry-p))
	 (table-line-pos (org-capture-get :table-line-pos))
	 beg end)
    (cond
     ((org-capture-get :exact-position)
      (goto-char (org-capture-get :exact-position)))
     ((not target-entry-p)
      ;; Table is not necessarily under a heading
      (setq beg (point-min) end (point-max)))
     (t
      ;; WE are at a heading, limit search to the body
      (setq beg (1+ (point-at-eol))
	    end (save-excursion (outline-next-heading) (point)))))
    (if (re-search-forward org-table-dataline-regexp end t)
	(let ((b (org-table-begin)) (e (org-table-end)) (case-fold-search t))
	  (goto-char e)
	  (if (looking-at "[ \t]*#\\+tblfm:")
	      (forward-line 1))
	  (narrow-to-region b (point)))
      (goto-char end)
      (insert "\n|   |\n|----|\n|    |\n")
      (narrow-to-region (1+ end) (point)))
    ;; We are narrowed to the table, or to an empty line if there was no table

    ;; Check if the template is good
    (if (not (string-match org-table-dataline-regexp txt))
	(setq txt "| %?Bad template |\n"))
    (if (functionp table-line-pos)
	(setq table-line-pos (funcall table-line-pos))
      (setq table-line-pos (eval table-line-pos)))
    (cond
     ((and table-line-pos
	   (string-match "\\(I+\\)\\([-+][0-9]\\)" table-line-pos))
      (goto-char (point-min))
      ;; we have a complex line specification
      (let ((ll (ignore-errors
		  (save-match-data (org-table-analyze))
		  (aref org-table-hlines
			(- (match-end 1) (match-beginning 1)))))
	    (delta (string-to-number (match-string 2 table-line-pos))))
	;; The user wants a special position in the table
	(unless ll
	  (error "Invalid table line specification \"%s\"" table-line-pos))
	(goto-char org-table-current-begin-pos)
	(forward-line (+ ll delta (if (< delta 0) 0 -1)))
	(org-table-insert-row 'below)
	(beginning-of-line 1)
	(delete-region (point) (1+ (point-at-eol)))
	(setq beg (point))
	(insert txt)
	(setq end (point))))
     ((org-capture-get :prepend)
      (goto-char (point-min))
      (re-search-forward org-table-hline-regexp nil t)
      (beginning-of-line 1)
      (re-search-forward org-table-dataline-regexp nil t)
      (beginning-of-line 1)
      (setq beg (point))
      (org-table-insert-row)
      (beginning-of-line 1)
      (delete-region (point) (1+ (point-at-eol)))
      (insert txt)
      (setq end (point)))
     (t
      (goto-char (point-max))
      (re-search-backward org-table-dataline-regexp nil t)
      (beginning-of-line 1)
      (org-table-insert-row 'below)
      (beginning-of-line 1)
      (delete-region (point) (1+ (point-at-eol)))
      (setq beg (point))
      (insert txt)
      (setq end (point))))
    (goto-char beg)
    (org-capture-position-for-last-stored 'table-line)
    (if (or (re-search-backward "%\\?" beg t)
	    (re-search-forward "%\\?" end t))
	(replace-match ""))
    (org-table-align)))

(defun org-capture-place-plain-text ()
  "Place the template plainly.
If the target locator points at an Org node, place the template into
the text of the entry, before the first child.  If not, place the
template at the beginning or end of the file.
Of course, if exact position has been required, just put it there."
  (let* ((txt (org-capture-get :template))
	 beg end)
    (cond
     ((org-capture-get :exact-position)
      (goto-char (org-capture-get :exact-position)))
     ((and (org-capture-get :target-entry-p)
	   (bolp)
	   (looking-at org-outline-regexp))
      ;; we should place the text into this entry
      (if (org-capture-get :prepend)
	  ;; Skip meta data and drawers
	  (org-end-of-meta-data t)
	;; go to ent of the entry text, before the next headline
	(outline-next-heading)))
     (t
      ;; beginning or end of file
      (goto-char (if (org-capture-get :prepend) (point-min) (point-max)))))
    (or (bolp) (newline))
    (org-capture-empty-lines-before)
    (setq beg (point))
    (insert txt)
    (org-capture-empty-lines-after)
    (org-capture-position-for-last-stored beg)
    (setq end (point))
    (org-capture-mark-kill-region beg (1- end))
    (org-capture-narrow beg (1- end))
    (if (or (re-search-backward "%\\?" beg t)
	    (re-search-forward "%\\?" end t))
	(replace-match ""))))

(defun org-capture-mark-kill-region (beg end)
  "Mark the region that will have to be killed when aborting capture."
  (let ((m1 (copy-marker beg))
	(m2 (copy-marker end t)))
    (org-capture-put :begin-marker m1)
    (org-capture-put :end-marker m2)))

(defun org-capture-position-for-last-stored (where)
  "Memorize the position that should later become the position of last capture."
  (cond
   ((integerp where)
    (org-capture-put :position-for-last-stored
		     (move-marker (make-marker) where
				  (or (buffer-base-buffer (current-buffer))
				      (current-buffer)))))
   ((eq where 'table-line)
    (org-capture-put :position-for-last-stored
		     (list 'table-line
			   (org-table-current-dline))))
   (t (error "This should not happen"))))

(defun org-capture-store-last-position ()
  "Store the last-captured position."
  (let* ((where (org-capture-get :position-for-last-stored 'local))
	 (pos (cond
	       ((markerp where)
		(prog1 (marker-position where)
		  (move-marker where nil)))
	       ((and (listp where) (eq (car where) 'table-line))
		(if (org-at-table-p)
		    (save-excursion
		      (org-table-goto-line (nth 1 where))
		      (point-at-bol))
		  (point))))))
    (with-current-buffer (buffer-base-buffer (current-buffer))
      (org-with-point-at pos
	(when org-capture-bookmark
	  (let ((bookmark (plist-get org-bookmark-names-plist :last-capture)))
	    (when bookmark (with-demoted-errors (bookmark-set bookmark)))))
	(move-marker org-capture-last-stored-marker (point))))))

(defun org-capture-narrow (beg end)
  "Narrow, unless configuration says not to narrow."
  (unless (org-capture-get :unnarrowed)
    (narrow-to-region beg end)
    (goto-char beg)))

(defun org-capture-empty-lines-before (&optional n)
  "Set the correct number of empty lines before the insertion point.
Point will be after the empty lines, so insertion can directly be done."
  (setq n (or n (org-capture-get :empty-lines-before)
	      (org-capture-get :empty-lines) 0))
  (let ((pos (point)))
    (org-back-over-empty-lines)
    (delete-region (point) pos)
    (if (> n 0) (newline n))))

(defun org-capture-empty-lines-after (&optional n)
  "Set the correct number of empty lines after the inserted string.
Point will remain at the first line after the inserted text."
  (setq n (or n (org-capture-get :empty-lines-after)
	      (org-capture-get :empty-lines) 0))
  (org-back-over-empty-lines)
  (while (looking-at "[ \t]*\n") (replace-match ""))
  (let ((pos (point)))
    (if (> n 0) (newline n))
    (goto-char pos)))

(defvar org-clock-marker) ; Defined in org.el

(defun org-capture-insert-template-here ()
  "Insert the capture template at point."
  (let* ((template (org-capture-get :template))
	 (type  (org-capture-get :type))
	 beg end pp)
    (unless (bolp) (insert "\n"))
    (setq beg (point))
    (cond
     ((and (eq type 'entry) (derived-mode-p 'org-mode))
      (org-capture-verify-tree (org-capture-get :template))
      (org-paste-subtree nil template t))
     ((and (memq type '(item checkitem))
	   (derived-mode-p 'org-mode)
	   (save-excursion (skip-chars-backward " \t\n")
			   (setq pp (point))
			   (org-in-item-p)))
      (goto-char pp)
      (org-insert-item)
      (skip-chars-backward " ")
      (skip-chars-backward "-+*0123456789).")
      (delete-region (point) (point-at-eol))
      (setq beg (point))
      (org-remove-indentation template)
      (insert template)
      (org-capture-empty-lines-after)
      (goto-char beg)
      (org-list-repair)
      (org-end-of-item))
     (t
      (insert template)
      (org-capture-empty-lines-after)
      (skip-chars-forward " \t\n")
      (unless (eobp) (beginning-of-line))))
    (setq end (point))
    (goto-char beg)
    (when (re-search-forward "%\\?" end t)
      (replace-match ""))))

(defun org-capture-set-plist (entry)
  "Initialize the property list from the template definition."
  (setq org-capture-plist (copy-sequence (nthcdr 5 entry)))
  (org-capture-put :key (car entry) :description (nth 1 entry)
		   :target (nth 3 entry))
  (let ((txt (nth 4 entry)) (type (or (nth 2 entry) 'entry)))
    (when (or (not txt) (and (stringp txt) (not (string-match "\\S-" txt))))
      ;; The template may be empty or omitted for special types.
      ;; Here we insert the default templates for such cases.
      (cond
       ((eq type 'item) (setq txt "- %?"))
       ((eq type 'checkitem) (setq txt "- [ ] %?"))
       ((eq type 'table-line) (setq txt "| %? |"))
       ((member type '(nil entry)) (setq txt "* %?\n  %a"))))
    (org-capture-put :template txt :type type)))

(defun org-capture-goto-target (&optional template-key)
  "Go to the target location of a capture template.
The user is queried for the template."
  (interactive)
  (let ((entry (org-capture-select-template template-key)))
    (unless entry (error "No capture template selected"))
    (org-capture-set-plist entry)
    (org-capture-set-target-location)
    (pop-to-buffer-same-window (org-capture-get :buffer))
    (goto-char (org-capture-get :pos))))

(defun org-capture-get-indirect-buffer (&optional buffer prefix)
  "Make an indirect buffer for a capture process.
Use PREFIX as a prefix for the name of the indirect buffer."
  (setq buffer (or buffer (current-buffer)))
  (let ((n 1) (base (buffer-name buffer)) bname)
    (setq bname (concat prefix "-" base))
    (while (buffer-live-p (get-buffer bname))
      (setq bname (concat prefix "-" (number-to-string (cl-incf n)) "-" base)))
    (condition-case nil
        (make-indirect-buffer buffer bname 'clone)
      (error
       (let ((buf (make-indirect-buffer buffer bname)))
	 (with-current-buffer buf (org-mode))
	 buf)))))

(defun org-capture-verify-tree (tree)
  "Throw error if TREE is not a valid tree."
  (unless (org-kill-is-subtree-p tree)
    (error "Template is not a valid Org entry or tree")))

(defun org-mks (table title &optional prompt specials)
  "Select a member of an alist with multiple keys.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"...

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIAL is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
  (save-window-excursion
    (let ((inhibit-quit t)
	  (buffer (org-switch-to-buffer-other-window "*Org Select*"))
	  (prompt (or prompt "Select: "))
	  current)
      (unwind-protect
	  (catch 'exit
	    (while t
	      (erase-buffer)
	      (insert title "\n\n")
	      (let ((des-keys nil)
		    (allowed-keys '("\C-g"))
		    (cursor-type nil))
		;; Populate allowed keys and descriptions keys
		;; available with CURRENT selector.
		(let ((re (format "\\`%s\\(.\\)\\'"
				  (if current (regexp-quote current) "")))
		      (prefix (if current (concat current " ") "")))
		  (dolist (entry table)
		    (pcase entry
		      ;; Description.
		      (`(,(and key (pred (string-match re))) ,desc)
		       (let ((k (match-string 1 key)))
			 (push k des-keys)
			 (push k allowed-keys)
			 (insert prefix "[" k "]" "..." "  " desc "..." "\n")))
		      ;; Usable entry.
		      (`(,(and key (pred (string-match re))) ,desc . ,_)
		       (let ((k (match-string 1 key)))
			 (insert prefix "[" k "]" "     " desc "\n")
			 (push k allowed-keys)))
		      (_ nil))))
		;; Insert special entries, if any.
		(when specials
		  (insert "----------------------------------------------------\
---------------------------\n")
		  (pcase-dolist (`(,key ,description) specials)
		    (insert (format "[%s]     %s\n" key description))
		    (push key allowed-keys)))
		;; Display UI and let user select an entry or
		;; a sub-level prefix.
		(goto-char (point-min))
		(unless (pos-visible-in-window-p (point-max))
		  (org-fit-window-to-buffer))
		(message prompt)
		(let ((pressed (char-to-string (read-char-exclusive))))
		  (while (not (member pressed allowed-keys))
		    (message "Invalid key `%s'" pressed) (sit-for 1)
		    (message prompt)
		    (setq pressed (char-to-string (read-char-exclusive))))
		  (setq current (concat current pressed))
		  (cond
		   ((equal pressed "\C-g") (user-error "Abort"))
		   ;; Selection is a prefix: open a new menu.
		   ((member pressed des-keys))
		   ;; Selection matches an association: return it.
		   ((let ((entry (assoc current table)))
		      (and entry (throw 'exit entry))))
		   ;; Selection matches a special entry: return the
		   ;; selection prefix.
		   ((assoc current specials) (throw 'exit current))
		   (t (error "No entry available")))))))
	(when buffer (kill-buffer buffer))))))

;;; The template code
(defun org-capture-select-template (&optional keys)
  "Select a capture template.
Lisp programs can force the template by setting KEYS to a string."
  (let ((org-capture-templates
	 (or (org-contextualize-keys
	      (org-capture-upgrade-templates org-capture-templates)
	      org-capture-templates-contexts)
	     '(("t" "Task" entry (file+headline "" "Tasks")
		"* TODO %?\n  %u\n  %a")))))
    (if keys
	(or (assoc keys org-capture-templates)
	    (error "No capture template referred to by \"%s\" keys" keys))
      (org-mks org-capture-templates
	       "Select a capture template\n========================="
	       "Template key: "
	       '(("C" "Customize org-capture-templates")
		 ("q" "Abort"))))))

(defvar org-capture--clipboards nil
  "List various clipboards values.")

(defun org-capture-fill-template (&optional template initial annotation)
  "Fill a template and return the filled template as a string.
The template may still contain \"%?\" for cursor positioning."
  (let* ((template (or template (org-capture-get :template)))
	 (buffer (org-capture-get :buffer))
	 (file (buffer-file-name (or (buffer-base-buffer buffer) buffer)))
	 (time (let* ((c (or (org-capture-get :default-time) (current-time)))
		      (d (decode-time c)))
		 (if (< (nth 2 d) org-extend-today-until)
		     (encode-time 0 59 23 (1- (nth 3 d)) (nth 4 d) (nth 5 d))
		   c)))
	 (v-t (format-time-string (org-time-stamp-format nil) time))
	 (v-T (format-time-string (org-time-stamp-format t) time))
	 (v-u (format-time-string (org-time-stamp-format nil t) time))
	 (v-U (format-time-string (org-time-stamp-format t t) time))
	 (v-c (and kill-ring (current-kill 0)))
	 (v-x (or (org-get-x-clipboard 'PRIMARY)
		  (org-get-x-clipboard 'CLIPBOARD)
		  (org-get-x-clipboard 'SECONDARY)))
	 ;; `initial' and `annotation' might have been passed.  But if
	 ;; the property list has them, we prefer those values.
	 (v-i (or (plist-get org-store-link-plist :initial)
		  (and (stringp initial) (org-no-properties initial))
		  (org-capture-get :initial)
		  ""))
	 (v-a
	  (let ((a (or (plist-get org-store-link-plist :annotation)
		       annotation
		       (org-capture-get :annotation)
		       "")))
	    ;; Is the link empty?  Then we do not want it...
	    (if (equal a "[[]]") "" a)))
	 (l-re "\\[\\[\\(.*?\\)\\]\\(\\[.*?\\]\\)?\\]")
	 (v-A (if (and v-a (string-match l-re v-a))
		  (replace-match "[[\\1][%^{Link description}]]" nil nil v-a)
		v-a))
	 (v-l (if (and v-a (string-match l-re v-a))
		  (replace-match "\\1" nil nil v-a)
		v-a))
	 (v-n user-full-name)
	 (v-k (if (marker-buffer org-clock-marker)
		  (org-no-properties org-clock-heading)
		""))
	 (v-K (if (marker-buffer org-clock-marker)
		  (org-make-link-string
		   (format "%s::*%s"
			   (buffer-file-name (marker-buffer org-clock-marker))
			   v-k)
		   v-k)
		""))
	 (v-f (or (org-capture-get :original-file-nondirectory) ""))
	 (v-F (or (org-capture-get :original-file) ""))
	 (org-capture--clipboards
	  (delq nil
		(list v-i
		      (org-get-x-clipboard 'PRIMARY)
		      (org-get-x-clipboard 'CLIPBOARD)
		      (org-get-x-clipboard 'SECONDARY)
		      v-c))))

    (setq org-store-link-plist (plist-put org-store-link-plist :annotation v-a))
    (setq org-store-link-plist (plist-put org-store-link-plist :initial v-i))

    (unless template
      (setq template "")
      (message "no template") (ding)
      (sit-for 1))
    (save-window-excursion
      (org-switch-to-buffer-other-window (get-buffer-create "*Capture*"))
      (erase-buffer)
      (setq buffer-file-name nil)
      (setq mark-active nil)
      (insert template)
      (goto-char (point-min))

      ;; %[] insert contents of a file.
      (save-excursion
	(while (re-search-forward "%\\[\\(.+\\)\\]" nil t)
	  (let ((filename (expand-file-name (match-string 1)))
		(beg (copy-marker (match-beginning 0)))
		(end (copy-marker (match-end 0))))
	    (unless (org-capture-escaped-%)
	      (delete-region beg end)
	      (set-marker beg nil)
	      (set-marker end nil)
	      (condition-case error
		  (insert-file-contents filename)
		(error
		 (insert (format "%%![couldn not insert %s: %s]"
				 filename
				 error))))))))

      ;; Mark %() embedded elisp for later evaluation.
      (org-capture-expand-embedded-elisp 'mark)

      ;; Expand non-interactive templates.
      (let ((regexp "%\\(:[-a-za-z]+\\|<\\([^>\n]+\\)>\\|[aAcfFikKlntTuUx]\\)"))
	(save-excursion
	  (while (re-search-forward regexp nil t)
	    ;; `org-capture-escaped-%' may modify buffer and cripple
	    ;; match-data.  Use markers instead.  Ditto for other
	    ;; templates.
	    (let ((pos (copy-marker (match-beginning 0)))
		  (end (copy-marker (match-end 0)))
		  (value (match-string 1))
		  (time-string (match-string 2)))
	      (unless (org-capture-escaped-%)
		(delete-region pos end)
		(set-marker pos nil)
		(set-marker end nil)
		(let* ((inside-sexp? (org-capture-inside-embedded-elisp-p))
		       (replacement
			(pcase (string-to-char value)
			  (?< (format-time-string time-string time))
			  (?:
			   (or (plist-get org-store-link-plist (intern value))
			       ""))
			  (?i
			   (if inside-sexp? v-i
			     ;; Outside embedded Lisp, repeat leading
			     ;; characters before initial place holder
			     ;; every line.
			     (let ((lead (buffer-substring-no-properties
					  (line-beginning-position) (point))))
			       (replace-regexp-in-string "\n\\(.\\)"
							 (concat lead "\\1")
							 v-i nil nil 1))))
			  (?a v-a)
			  (?A v-A)
			  (?c v-c)
			  (?f v-f)
			  (?F v-F)
			  (?k v-k)
			  (?K v-K)
			  (?l v-l)
			  (?n v-n)
			  (?t v-t)
			  (?T v-T)
			  (?u v-u)
			  (?U v-U)
			  (?x v-x))))
		  (insert
		   (if inside-sexp?
		       ;; Escape sensitive characters.
		       (replace-regexp-in-string "[\\\"]" "\\\\\\&" replacement)
		     replacement))))))))

      ;; Expand %() embedded Elisp.  Limit to Sexp originally marked.
      (org-capture-expand-embedded-elisp)

      ;; Expand interactive templates.  This is the last step so that
      ;; template is mostly expanded when prompting happens.  Turn on
      ;; Org mode and set local variables.  This is to support
      ;; completion in interactive prompts.
      (let ((org-inhibit-startup t)) (org-mode))
      (org-clone-local-variables buffer "\\`org-")
      (let (strings)			; Stores interactive answers.
	(save-excursion
	  (let ((regexp "%\\^\\(?:{\\([^}]*\\)}\\)?\\([CgGLptTuU]\\)?"))
	    (while (re-search-forward regexp nil t)
	      (let* ((items (and (match-end 1)
				 (save-match-data
				   (split-string (match-string-no-properties 1)
						 "|"))))
		     (key (match-string 2))
		     (beg (copy-marker (match-beginning 0)))
		     (end (copy-marker (match-end 0)))
		     (prompt (nth 0 items))
		     (default (nth 1 items))
		     (completions (nthcdr 2 items)))
		(unless (org-capture-escaped-%)
		  (delete-region beg end)
		  (set-marker beg nil)
		  (set-marker end nil)
		  (pcase key
		    ((or "G" "g")
		     (let* ((org-last-tags-completion-table
			     (org-global-tags-completion-table
			      (cond ((equal key "G") (org-agenda-files))
				    (file (list file))
				    (t nil))))
			    (org-add-colon-after-tag-completion t)
			    (ins (mapconcat
				  #'identity
				  (org-split-string
				   (completing-read
				    (if prompt (concat prompt ": ") "Tags: ")
				    'org-tags-completion-function nil nil nil
				    'org-tags-history)
				   "[^[:alnum:]_@#%]+")
				  ":")))
		       (when (org-string-nw-p ins)
			 (unless (eq (char-before) ?:) (insert ":"))
			 (insert ins)
			 (unless (eq (char-after) ?:) (insert ":"))
			 (and (org-at-heading-p)
			      (let ((org-ignore-region t))
				(org-set-tags nil 'align))))))
		    ((or "C" "L")
		     (let ((insert-fun (if (equal key "C") #'insert
					 (lambda (s) (org-insert-link 0 s)))))
		       (pcase org-capture--clipboards
			 (`nil nil)
			 (`(,value) (funcall insert-fun value))
			 (`(,first-value . ,_)
			  (funcall insert-fun
				   (read-string "Clipboard/kill value: "
						first-value
						'org-capture--clipboards
						first-value)))
			 (_ (error "Invalid `org-capture--clipboards' value: %S"
				   org-capture--clipboards)))))
		    ("p" (org-set-property prompt nil))
		    ((or "t" "T" "u" "U")
		     ;; These are the date/time related ones.
		     (let* ((upcase? (equal (upcase key) key))
			    (org-end-time-was-given nil)
			    (time (org-read-date upcase? t nil prompt)))
		       (org-insert-time-stamp
			time (or org-time-was-given upcase?)
			(member key '("u" "U"))
			nil nil (list org-end-time-was-given))))
		    (`nil
		     (push (org-completing-read
			    (concat (or prompt "Enter string")
				    (and default (format " [%s]" default))
				    ": ")
			    completions nil nil nil nil default)
			   strings)
		     (insert (car strings)))
		    (_
		     (error "Unknown template placeholder: \"%%^%s\""
			    key))))))))

	;; Replace %n escapes with nth %^{...} string.
	(setq strings (nreverse strings))
	(save-excursion
	  (while (re-search-forward "%\\\\\\([1-9][0-9]*\\)" nil t)
	    (unless (org-capture-escaped-%)
	      (replace-match
	       (nth (1- (string-to-number (match-string 1))) strings)
	       nil t)))))

      ;; Make sure there are no empty lines before the text, and that
      ;; it ends with a newline character.
      (skip-chars-forward " \t\n")
      (delete-region (point-min) (line-beginning-position))
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (delete-region (point) (point-max))
      (insert "\n")

      ;; Return the expanded template and kill the capture buffer.
      (untabify (point-min) (point-max))
      (set-buffer-modified-p nil)
      (prog1 (buffer-substring-no-properties (point-min) (point-max))
	(kill-buffer (current-buffer))))))

(defun org-capture-escaped-% ()
  "Non-nil if % was escaped.
If yes, unescape it now.  Assume match-data contains the
placeholder to check."
  (save-excursion
    (goto-char (match-beginning 0))
    (let ((n (abs (skip-chars-backward "\\\\"))))
      (delete-char (/ (1+ n) 2))
      (= (% n 2) 1))))

(defun org-capture-expand-embedded-elisp (&optional mark)
  "Evaluate embedded elisp %(sexp) and replace with the result.
When optional MARK argument is non-nil, mark Sexp with a text
property (`org-embedded-elisp') for later evaluation.  Only
marked Sexp are evaluated when this argument is nil."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "%(" nil t)
      (cond
       ((get-text-property (match-beginning 0) 'org-embedded-elisp)
	(goto-char (match-beginning 0))
	(let ((template-start (point)))
	  (forward-char 1)
	  (let* ((sexp (read (current-buffer)))
		 (result (org-eval
			  (org-capture--expand-keyword-in-embedded-elisp
			   sexp))))
	    (delete-region template-start (point))
	    (cond
	     ((not result) nil)
	     ((stringp result) (insert result))
	     (t (error
		 "Capture template sexp `%s' must evaluate to string or nil"
		 sexp))))))
       ((not mark) nil)
       ;; Only mark valid and non-escaped sexp.
       ((org-capture-escaped-%) nil)
       (t
	(let ((end (with-syntax-table emacs-lisp-mode-syntax-table
		     (ignore-errors (scan-sexps (1- (point)) 1)))))
	  (when end
	    (put-text-property (- (point) 2) end 'org-embedded-elisp t))))))))

(defun org-capture--expand-keyword-in-embedded-elisp (attr)
  "Recursively replace capture link keywords in ATTR sexp.
Such keywords are prefixed with \"%:\".  See
`org-capture-template' for more information."
  (cond ((consp attr)
	 (mapcar 'org-capture--expand-keyword-in-embedded-elisp attr))
	((symbolp attr)
	 (let* ((attr-symbol (symbol-name attr))
		(key (and (string-match "%\\(:.*\\)" attr-symbol)
			  (intern (match-string 1 attr-symbol)))))
	   (or (plist-get org-store-link-plist key)
	       attr)))
	(t attr)))

(defun org-capture-inside-embedded-elisp-p ()
  "Non-nil if point is inside of embedded elisp %(sexp).
Assume sexps have been marked with
`org-capture-expand-embedded-elisp' beforehand."
  (get-text-property (point) 'org-embedded-elisp))

;;;###autoload
(defun org-capture-import-remember-templates ()
  "Set `org-capture-templates' to be similar to `org-remember-templates'."
  (interactive)
  (when (and (yes-or-no-p
	      "Import old remember templates into org-capture-templates? ")
	     (yes-or-no-p
	      "Note that this will remove any templates currently defined in `org-capture-templates'.  Do you still want to go ahead? "))
    (require 'org-remember)
    (setq org-capture-templates
	  (mapcar
	   (lambda (entry)
	     (let ((desc (car entry))
		   (key (char-to-string (nth 1 entry)))
		   (template (nth 2 entry))
		   (file (or (nth 3 entry) org-default-notes-file))
		   (position (or (nth 4 entry) org-remember-default-headline))
		   (type 'entry)
		   (prepend org-reverse-note-order)
		   immediate target jump-to-captured)
	       (cond
		((member position '(top bottom))
		 (setq target (list 'file file)
		       prepend (eq position 'top)))
		((eq position 'date-tree)
		 (setq target (list 'file+datetree file)
		       prepend nil))
		(t (setq target (list 'file+headline file position))))

	       (when (string-match "%!" template)
		 (setq template (replace-match "" t t template)
		       immediate t))

	       (when (string-match "%&" template)
		 (setq jump-to-captured t))

	       (append (list key desc type target template)
		       (if prepend '(:prepend t))
		       (if immediate '(:immediate-finish t))
		       (if jump-to-captured '(:jump-to-captured t)))))

	   org-remember-templates))))


(provide 'org-capture)

;;; org-capture.el ends here
