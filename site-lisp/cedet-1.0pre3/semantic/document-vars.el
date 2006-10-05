;;; document-vars.el --- Default settings for the document package.

;;; Copyright (C) 2000, 2001 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: doc
;; X-RCS: $Id: document-vars.el,v 1.3 2001/09/29 23:42:02 ponced Exp $

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Default settings for the document comment generation package.


;;; Code:
(provide 'document-vars)

(eval-when-compile
  ;; Emacs 21
  (condition-case nil
      (require 'newcomment)
    (error nil))
  )

(defvar document-comment-start nil
  "Comment start string.")

(defvar document-comment-line-prefix nil
  "Comment prefix string.  Used at the beginning of each line.")

(defvar document-comment-end nil
  "Comment end string.")

(defcustom document-copyright-notice-file nil
  "*A file name containing a copyright notice.
It will be reformatted in the header to have the correct prefix character.
See the %N token in `document-file-comment'"
  :group 'document
  :type 'file)

(defcustom document-change-number nil
  "*The current change number being worked on.
Large projects may use change numbers, bug numbers, or other tag."
  :group 'document
  :type 'string)

(defcustom document-extra-line-after-short-parameters t
  "*Non-nil to add an extra line when there is < 1 arguments."
  :group 'document
  :type 'string)

(defvar document-comment-left-edge-character nil
  "*Language/preference specific characters to use in comments.")

(defcustom document-file-comment "%s %B
%m
%m Copyright (C) %Y %O
%m
%m %N
%m
%m Description:
%m
%m   %D
%m
%m History:
%m %H
%m
%m Tokens: %T
%e
"
  "Comment block for the beginning of a new file.
The format tokens available are:
 %B - Brief description of the file (Auto-comment by file name)
 %D - Made up documentation
 %N - Copyright notice for your organization
 %O - Owner (full name of copyright holder held in `document-copyright-holder'
 %H - History elements
 %T - cproto header id token.  Always is form 'token file.h' where
      token is defined in cpr-header-token, and file.h is the
      relational file name of the header.  If you wish to use cproto's
      features, you must have this somewhere in the header.
 %Y - Year
Commenting elements:
 %b - Comment start string
 %m - Comment line prefix (not the start)
 %e - Comment end string"
  :group 'document
  :type 'string)

(defcustom document-header-comment "%b
%m Copyright (c) %Y %O
%m
%m %N
%m
%m History:
%m %H
%e
"
  "Comment block for the beginning of a new header/dependency file.
The format tokens available are the same as for `document-file-comment'"
  :group 'document
  :type 'string)

(defcustom document-file-brief-comment "%F - %C"
  "Format of the brief comment with tokens.
Available tokens are:
 %F - file name short
 %C - Comment field"
  :group 'document
  :type 'string)

(defcustom document-function-comment "
%b
%m Function: %F
%m
%m %f  %D%p
%m
%m Returns:     %R
%m Parameters:  %P
%m History:
%m %H
%e
"
  "Comment block for the beginning of a new function/variable definition.
There are several format tokens represent the following:
  %F - function name
  %D - Made up documentation
  %f - Place, where everything before the point is the distance to set
       in the fill prefix.  Allows a first line in paragraph indent
  %p - Where to place point after insertion of a new header
  %R - Returns
  %P - Parameter list
  %H - History insertion point

  The parts %f and %p can be confusing, so here is an example:

 * Moose: %f%D%p

 Will set fill prefix to ` *         `, and put the point AFTER the
description.  The `Moose:` will not be in the prefix.  The default
value shows the equivalent of a hanging indent.

Commenting elements:
 %b - Comment start string
 %m - Comment line prefix (not the start)
 %e - Comment end string"
  :group 'document
  :type 'string)

(defcustom document-param-element "%P - %D"
  "The format of a parameter element in the list of parameters.
The parts are:
 %P - Parameter name spaced to length of max param
 %p - Parameter name with no padding
 %R - Type of element
 %D - Description of parameter as known by document."
  :group 'document
  :type 'string)

(defcustom document-history-element "%-7U %-10D %C"
  "Format for history element.
Valid % codes are:
  %U - Username, initials, what have you.
  %D - The current date formatted as in `document-date-element'
  %S - System Change id, SCCS vers, major change comment, etc
  %C - Auto comment area, cursor goes here for new elts."
  :group 'document
  :type 'string)

(defcustom document-date-element "%M/%D/%y"
  "Format for date elements.
Valid format chars are:
  %H - Hours
  %h - Hours 24 hr format
  %a - AM/PM flag
  %I - mInutes
  %S - Seconds
  %D - Day
  %w - Weekday string
  %M - Month as number
  %m - Month as string
  %Y - Year
  %y - Year as 2 numbers 1994 -> 94"
  :group 'document
  :type 'string)

(defcustom document-autocomment-function-alist
  '(
    ("abort" . "Aborts the")
    ;; trick to get re-alloc and alloc to pair into one sentence.
    ("realloc" . "moves or ")
    ("alloc\\(ate\\)?" . "Allocates and initializes a new ")
    ("clean" . "Cleans up the")
    ("clobber" . "Removes")
    ("close" . "Cleanly closes")
    ("check" . "Checks the")
    ("comp\\(are\\)?" . "Compares the")
    ("create" . "Creates a new ")
    ("find" . "Finds ")
    ("free" . "Frees up space")
    ("gen\\(erate\\)?" . "Generates a new ")
    ("get\\|find" . "Looks for the given ")
    ("gobble" . "Removes")
    ("he?lp" . "Provides help for")
    ("li?ste?n" . "Listens for ")
    ("connect" . "Connects to ")
    ("acc?e?pt" . "Accepts a ")
    ("load" . "Loads in ")
    ("match" . "Check that parameters match")
    ("name" . "Provides a name which ")
    ("parse" . "Parses the parameters and returns ")
    ("print\\|display" . "Prints out")
    ("read" . "Reads from")
    ("reset" . "Resets the parameters and returns")
    ("scan" . "Scans the ")
    ("setup\\|init\\(iallize\\)?" . "Initializes the ")
    ("select" . "Chooses the ")
    ("send" . "Sends a")
    ("re?c\\(v\\|ieves?\\)" . "Receives a ")
    ("wait" . "Waits for ")
    ("write" . "Writes to")
    )
  "List of names to string match against the function name.
This is an alist with each element of the form:
 (MATCH . RESULT)
MATCH is a regexp to match in the type field.
RESULT is a string.

Certain prefixes may always mean the same thing, and the same comment
can be used as a beginning for the description.  Regexp should be
lower case since the string they are compared to is downcased.
A string may end in a space, in which case, last-alist is searched to
see how best to describe what can be returned.
Doesn't always work correctly, but that is just because English
doesn't always work correctly."
  :group 'document
  :type '(repeat (cons (string :tag "Regexp")
		       (string :tag "Doc Text"))))

(defcustom document-autocomment-common-nouns-abbrevs
  '(
    ("sock\\(et\\)?" . "socket")
    ("addr\\(ess\\)?" . "address")
    ("buf\\(f\\(er\\)?\\)?" . "buffer")
    ("cur\\(r\\(ent\\)?\\)?" . "current")
    ("dev\\(ice\\)?" . "device")
    ("file" . "file")
    ("line" . "line")
    ("msg\\|message" . "message")
    ("name" . "name")
    ("next\\|nxt" . "next")
    ("port" . "port")
    ("host" . "host")
    ("obj\\|object" . "object")
    ("previous\\|prev" . "previous")
    ("str\\(ing\\)?" . "string")
    ("use?r" . "user")
    ("num\\(ber\\)?" . "number")
    ("\\(^\\|\\s-\\)id\\($\\|\\s-\\)" . "Identifier") ;complex cause ;commen sylable
    )
  "List of common English abbreviations or full words.
These are nouns (as opposed to verbs) for use in creating expanded
versions of names.This is an alist with each element of the form:
 (MATCH . RESULT)
MATCH is a regexp to match in the type field.
RESULT is a string."
  :group 'document
  :type '(repeat (cons (string :tag "Regexp")
		       (string :tag "Doc Text"))))

(defcustom document-autocomment-return-first-alist
  '(
    ;; Static must be first in the list to provide the intro to the sentence
    ("static" . "Locally defined function which ")
    ("Bool\\|BOOL" . "Status of ")
    )
  "List of regexp matches for types.
They provide a little bit of text when typing information is
described.
This is an alist with each element of the form:
 (MATCH . RESULT)
MATCH is a regexp to match in the type field.
RESULT is a string."
  :group 'document
  :type '(repeat (cons (string :tag "Regexp")
		       (string :tag "Doc Text"))))

(defcustom document-autocomment-return-last-alist
  '(
    ("static[ \t\n]+struct \\([a-zA-Z0-9_]+\\)" . "%s")
    ("struct \\([a-zA-Z0-9_]+\\)" . "%s")
    ("static[ \t\n]+union \\([a-zA-Z0-9_]+\\)" . "%s")
    ("union \\([a-zA-Z0-9_]+\\)" . "%s")
    ("static[ \t\n]+enum \\([a-zA-Z0-9_]+\\)" . "%s")
    ("enum \\([a-zA-Z0-9_]+\\)" . "%s")
    ("static[ \t\n]+\\([a-zA-Z0-9_]+\\)" . "%s")
    ("\\([a-zA-Z0-9_]+\\)" . "of type %s")
    )
  "List of regexps which provide the type of the return value.
This is an alist with each element of the form:
 (MATCH . RESULT)
MATCH is a regexp to match in the type field.
RESULT is a string, which can contain %s, whih is replaced with
`match-string' 1."
  :group 'document
  :type '(repeat (cons (string :tag "Regexp")
		       (string :tag "Doc Text"))))

(defcustom document-autocomment-param-alist
  '( ("[Cc]txt" . "Context")
     ("[Ii]d" . "Identifier of")
     ("[Tt]ype" . "Type of")
     ("[Nn]ame" . "Name of")
     ("argc" . "Number of arguments")
     ("argv" . "Argument vector")
     ("envp" . "Environment variable vector")
     )
  "Alist of common variable names appearing as function parameters.
This is an alist with each element of the form:
 (MATCH . RESULT)
MATCH is a regexp to match in the type field.
RESULT is a string of text to use to describe MATCH.
When one is encountered, document-insert-parameters will automatically
place this comment after the parameter name."
  :group 'document
  :type '(repeat (cons (string :tag "Regexp")
		       (string :tag "Doc Text"))))

(defcustom document-autocomment-param-type-alist
  '(("const" . "Constant")
    ("void" . "Empty")
    ("char[ ]*\\*" . "String ")
    ("\\*\\*" . "Pointer to ")
    ("\\*" . "Pointer ")
    ("char[ ]*\\([^ \t*]\\|$\\)" . "Character")
    ("int\\|long" . "Number of")
    ("FILE" . "File of")
    ("float\\|double" . "Value of")
    ;; How about some X things?
    ("Bool\\|BOOL" . "Flag")
    ("Window" . "Window")
    ("GC" . "Graphic Context")
    ("Widget" . "Widget")
    )
  "Alist of input parameter types and strings desribing them.
This is an alist with each element of the form:
 (MATCH . RESULT)
MATCH is a regexp to match in the type field.
RESULT is a string."
  :group 'document
  :type '(repeat (cons (string :tag "Regexp")
		       (string :tag "Doc Text"))))

(defcustom document-new-hist-comment "Created"
  "Comment used in the history when something is created."
  :group 'document
  :type 'string)

(defvar document-autocomment-modify-alist
  '((document-newparam . "%s")
    )
  "Alist of history change calculations.
This is an alist with each element of the form:
 (FUNCTION . RESULT)
FUNCTION is a function to run to check for chnges.
RESULT is a string with %s being filled with change text.")

;;; A few fns to access some variables.
(defun document-comment-start ()
  "Derive a string to start a comment in this mode."
  (or document-comment-start
      block-comment-start
      comment-start))

(defun document-comment-line-prefix ()
  "Derive a string to end a comment in this mode."
  (or document-comment-line-prefix
      ""))

(defun document-comment-end ()
  "Derive a string to end a comment in this mode."
  (or document-comment-end
      block-comment-end
      "\n"))

;;; document-vars.el ends here
