;;; srecode-document-vars.el --- Variables for srecode-document

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: srecode-document-vars.el,v 1.1 2008/12/31 19:16:23 zappo Exp $

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
;; Tables of English syntax stuff used by srecode-document.el.
;;
;; Copied from semantic/document-vars.el before it was obsoleted.

;;; Code:

(defcustom srecode-document-autocomment-common-nouns-abbrevs
  '(
    ("sock\\(et\\)?" . "socket")
    ("addr\\(ess\\)?" . "address")
    ("buf\\(f\\(er\\)?\\)?" . "buffer")
    ("cur\\(r\\(ent\\)?\\)?" . "current")
    ("dev\\(ice\\)?" . "device")
    ("doc" . "document")
    ("i18n" . "internationalization")
    ("file" . "file")
    ("line" . "line")
    ("l10n" . "localization")
    ("msg\\|message" . "message")
    ("name" . "name")
    ("next\\|nxt" . "next")
    ("num\\(ber\\)?" . "number")
    ("port" . "port")
    ("host" . "host")
    ("obj\\|object" . "object")
    ("previous\\|prev" . "previous")
    ("str\\(ing\\)?" . "string")
    ("use?r" . "user")
    ("\\(^\\|\\s-\\)id\\($\\|\\s-\\)" . "Identifier") ;complex cause ;common syllable
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

(defcustom srecode-document-autocomment-function-alist
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
    ("new" . "Allocates a ")
    ("parse" . "Parses the parameters and returns ")
    ("print\\|display" . "Prints out")
    ("read" . "Reads from")
    ("reset" . "Resets the parameters and returns")
    ("scan" . "Scans the ")
    ("setup\\|init\\(iallize\\)?" . "Initializes the ")
    ("select" . "Chooses the ")
    ("send" . "Sends a")
    ("re?c\\(v\\|ieves?\\)" . "Receives a ")
    ("to" . "Converts ")
    ("update" . "Updates the ")
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

(defcustom srecode-document-autocomment-common-nouns-abbrevs
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

(defcustom srecode-document-autocomment-return-first-alist
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

(defcustom srecode-document-autocomment-return-last-alist
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

(defcustom srecode-document-autocomment-param-alist
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

(defcustom srecode-document-autocomment-param-type-alist
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


(provide 'srecode-document-vars)
;;; srecode-document-vars.el ends here
