;;; org-agenda.el --- Dynamic task and appointment lists for Org

;; Copyright (C) 2004-2017 Free Software Foundation, Inc.

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

;; This file contains the code for creating and using the Agenda for Org.
;;
;; The functions `org-batch-agenda', `org-batch-agenda-csv', and
;; `org-batch-store-agenda-views' are implemented as macros to provide
;; a convenient way for extracting agenda information from the command
;; line.  The Lisp does not evaluate parameters of a macro call; thus
;; it is not necessary to quote the parameters passed to one of those
;; functions.  E.g. you can write:
;;
;;   emacs -batch -l ~/.emacs -eval '(org-batch-agenda "a" org-agenda-span 7)'
;;
;; To export an agenda spanning 7 days.  If `org-batch-agenda' would
;; have been implemented as a regular function you'd have to quote the
;; symbol org-agenda-span.  Moreover: To use a symbol as parameter
;; value you would have to double quote the symbol.
;;
;; This is a hack, but it works even when running Org byte-compiled.
;;

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-macs)

(declare-function diary-add-to-list "diary-lib"
                  (date string specifier &optional marker globcolor literal))
(declare-function calendar-iso-to-absolute      "cal-iso"    (date))
(declare-function calendar-astro-date-string    "cal-julian" (&optional date))
(declare-function calendar-bahai-date-string    "cal-bahai"  (&optional date))
(declare-function calendar-chinese-date-string  "cal-china"  (&optional date))
(declare-function calendar-coptic-date-string   "cal-coptic" (&optional date))
(declare-function calendar-ethiopic-date-string "cal-coptic" (&optional date))
(declare-function calendar-french-date-string   "cal-french" (&optional date))
(declare-function calendar-goto-date            "cal-move"   (date))
(declare-function calendar-hebrew-date-string   "cal-hebrew" (&optional date))
(declare-function calendar-islamic-date-string  "cal-islam"  (&optional date))
(declare-function calendar-iso-date-string      "cal-iso"    (&optional date))
(declare-function calendar-iso-from-absolute    "cal-iso"    (date))
(declare-function calendar-julian-date-string   "cal-julian" (&optional date))
(declare-function calendar-mayan-date-string    "cal-mayan"  (&optional date))
(declare-function calendar-persian-date-string  "cal-persia" (&optional date))
(declare-function calendar-check-holidays       "holidays" (date))

(declare-function org-columns-remove-overlays "org-colview" ())
(declare-function org-datetree-find-date-create "org-datetree"
		  (date &optional keep-restriction))
(declare-function org-columns-quit              "org-colview" ())
(declare-function diary-date-display-form       "diary-lib"  (&optional type))
(declare-function org-mobile-write-agenda-for-mobile "org-mobile" (file))
(declare-function org-habit-insert-consistency-graphs
		  "org-habit" (&optional line))
(declare-function org-is-habit-p "org-habit" (&optional pom))
(declare-function org-habit-parse-todo "org-habit" (&optional pom))
(declare-function org-habit-get-priority "org-habit" (habit &optional moment))
(declare-function org-agenda-columns "org-colview" ())
(declare-function org-add-archive-files "org-archive" (files))
(declare-function org-capture "org-capture" (&optional goto keys))

(defvar calendar-mode-map)
(defvar org-clock-current-task)
(defvar org-current-tag-alist)
(defvar org-mobile-force-id-on-agenda-items)
(defvar org-habit-show-habits)
(defvar org-habit-show-habits-only-for-today)
(defvar org-habit-show-all-today)

;; Defined somewhere in this file, but used before definition.
(defvar org-agenda-buffer-name "*Org Agenda*")
(defvar org-agenda-overriding-header nil)
(defvar org-agenda-title-append nil)
(with-no-warnings (defvar entry)) ;; unprefixed, from calendar.el
(with-no-warnings (defvar date))  ;; unprefixed, from calendar.el
(defvar original-date) ; dynamically scoped, calendar.el does scope this

(defvar org-agenda-undo-list nil
  "List of undoable operations in the agenda since last refresh.")
(defvar org-agenda-pending-undo-list nil
  "In a series of undo commands, this is the list of remaining undo items.")

(defcustom org-agenda-confirm-kill 1
  "When set, remote killing from the agenda buffer needs confirmation.
When t, a confirmation is always needed.  When a number N, confirmation is
only needed when the text to be killed contains more than N non-white lines."
  :group 'org-agenda
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Always" t)
	  (integer :tag "When more than N lines")))

(defcustom org-agenda-compact-blocks nil
  "Non-nil means make the block agenda more compact.
This is done globally by leaving out lines like the agenda span
name and week number or the separator lines."
  :group 'org-agenda
  :type 'boolean)

(defcustom org-agenda-block-separator ?=
  "The separator between blocks in the agenda.
If this is a string, it will be used as the separator, with a newline added.
If it is a character, it will be repeated to fill the window width.
If nil the separator is disabled.  In `org-agenda-custom-commands' this
addresses the separator between the current and the previous block."
  :group 'org-agenda
  :type '(choice
	  (const :tag "Disabled" nil)
	  (character)
	  (string)))

(defgroup org-agenda-export nil
  "Options concerning exporting agenda views in Org mode."
  :tag "Org Agenda Export"
  :group 'org-agenda)

(defcustom org-agenda-with-colors t
  "Non-nil means use colors in agenda views."
  :group 'org-agenda-export
  :type 'boolean)

(defcustom org-agenda-exporter-settings nil
  "Alist of variable/value pairs that should be active during agenda export.
This is a good place to set options for ps-print and for htmlize.
Note that the way this is implemented, the values will be evaluated
before assigned to the variables.  So make sure to quote values you do
*not* want evaluated, for example

   (setq org-agenda-exporter-settings
         \\='((ps-print-color-p \\='black-white)))"
  :group 'org-agenda-export
  :type '(repeat
	  (list
	   (variable)
	   (sexp :tag "Value"))))

(defcustom org-agenda-before-write-hook '(org-agenda-add-entry-text)
  "Hook run in a temporary buffer before writing the agenda to an export file.
A useful function for this hook is `org-agenda-add-entry-text'."
  :group 'org-agenda-export
  :type 'hook
  :options '(org-agenda-add-entry-text))

(defcustom org-agenda-add-entry-text-maxlines 0
  "Maximum number of entry text lines to be added to agenda.
This is only relevant when `org-agenda-add-entry-text' is part of
`org-agenda-before-write-hook', which is the default.
When this is 0, nothing will happen.  When it is greater than 0, it
specifies the maximum number of lines that will be added for each entry
that is listed in the agenda view.

Note that this variable is not used during display, only when exporting
the agenda.  For agenda display, see the variables `org-agenda-entry-text-mode'
and `org-agenda-entry-text-maxlines'."
  :group 'org-agenda
  :type 'integer)

(defcustom org-agenda-add-entry-text-descriptive-links t
  "Non-nil means export org-links as descriptive links in agenda added text.
This variable applies to the text added to the agenda when
`org-agenda-add-entry-text-maxlines' is larger than 0.
When this variable nil, the URL will (also) be shown."
  :group 'org-agenda
  :type 'boolean)

(defcustom org-agenda-export-html-style nil
  "The style specification for exported HTML Agenda files.
If this variable contains a string, it will replace the default <style>
section as produced by `htmlize'.
Since there are different ways of setting style information, this variable
needs to contain the full HTML structure to provide a style, including the
surrounding HTML tags.  The style specifications should include definitions
the fonts used by the agenda, here is an example:

   <style type=\"text/css\">
       p { font-weight: normal; color: gray; }
       .org-agenda-structure {
          font-size: 110%;
          color: #003399;
          font-weight: 600;
       }
       .org-todo {
          color: #cc6666;
          font-weight: bold;
       }
       .org-agenda-done {
          color: #339933;
       }
       .org-done {
          color: #339933;
       }
       .title { text-align: center; }
       .todo, .deadline { color: red; }
       .done { color: green; }
    </style>

or, if you want to keep the style in a file,

   <link rel=\"stylesheet\" type=\"text/css\" href=\"mystyles.css\">

As the value of this option simply gets inserted into the HTML <head> header,
you can \"misuse\" it to also add other text to the header."
  :group 'org-agenda-export
  :group 'org-export-html
  :type '(choice
	  (const nil)
	  (string)))

(defcustom org-agenda-persistent-filter nil
  "When set, keep filters from one agenda view to the next."
  :group 'org-agenda
  :type 'boolean)

(defgroup org-agenda-custom-commands nil
  "Options concerning agenda views in Org mode."
  :tag "Org Agenda Custom Commands"
  :group 'org-agenda)

(defconst org-sorting-choice
  '(choice
    (const time-up) (const time-down)
    (const timestamp-up) (const timestamp-down)
    (const scheduled-up) (const scheduled-down)
    (const deadline-up)  (const deadline-down)
    (const ts-up) (const ts-down)
    (const tsia-up) (const tsia-down)
    (const category-keep) (const category-up) (const category-down)
    (const tag-down) (const tag-up)
    (const priority-up) (const priority-down)
    (const todo-state-up) (const todo-state-down)
    (const effort-up) (const effort-down)
    (const habit-up) (const habit-down)
    (const alpha-up) (const alpha-down)
    (const user-defined-up) (const user-defined-down))
  "Sorting choices.")

;; Keep custom values for `org-agenda-filter-preset' compatible with
;; the new variable `org-agenda-tag-filter-preset'.
(defvaralias 'org-agenda-filter-preset 'org-agenda-tag-filter-preset)
(defvaralias 'org-agenda-filter 'org-agenda-tag-filter)

(defvar org-agenda-entry-types '(:deadline :scheduled :timestamp :sexp)
  "List of types searched for when creating the daily/weekly agenda.
This variable is a list of symbols that controls the types of
items that appear in the daily/weekly agenda.  Allowed symbols in this
list are are

  :timestamp   List items containing a date stamp or date range matching
               the selected date.  This includes sexp entries in angular
               brackets.

  :sexp        List entries resulting from plain diary-like sexps.

  :deadline    List deadline due on that date.  When the date is today,
               also list any deadlines past due, or due within
	       `org-deadline-warning-days'.

  :deadline*   Same as above, but only include the deadline if it has an
               hour specification as [h]h:mm.

  :scheduled   List all items which are scheduled for the given date.
	       The diary for *today* also contains items which were
	       scheduled earlier and are not yet marked DONE.

  :scheduled*  Same as above, but only include the scheduled item if it
               has an hour specification as [h]h:mm.

By default, all four non-starred types are turned on.

When :scheduled* or :deadline* are included, :schedule or :deadline
will be ignored.

Never set this variable globally using `setq', because then it
will apply to all future agenda commands.  Instead, bind it with
`let' to scope it dynamically into the agenda-constructing
command.  A good way to set it is through options in
`org-agenda-custom-commands'.  For a more flexible (though
somewhat less efficient) way of determining what is included in
the daily/weekly agenda, see `org-agenda-skip-function'.")

(defconst org-agenda-custom-commands-local-options
  `(repeat :tag "Local settings for this command.  Remember to quote values"
	   (choice :tag "Setting"
		   (list :tag "Heading for this block"
			 (const org-agenda-overriding-header)
			 (string :tag "Headline"))
		   (list :tag "Files to be searched"
			 (const org-agenda-files)
			 (list
			  (const :format "" quote)
			  (repeat (file))))
		   (list :tag "Sorting strategy"
			 (const org-agenda-sorting-strategy)
			 (list
			  (const :format "" quote)
			  (repeat
			   ,org-sorting-choice)))
		   (list :tag "Prefix format"
			 (const org-agenda-prefix-format :value "  %-12:c%?-12t% s")
			 (string))
		   (list :tag "Number of days in agenda"
			 (const org-agenda-span)
			 (list
			  (const :format "" quote)
			  (choice (const :tag "Day" day)
				  (const :tag "Week" week)
				  (const :tag "Fortnight" fortnight)
				  (const :tag "Month" month)
				  (const :tag "Year" year)
				  (integer :tag "Custom"))))
		   (list :tag "Fixed starting date"
			 (const org-agenda-start-day)
			 (string :value "2007-11-01"))
		   (list :tag "Start on day of week"
			 (const org-agenda-start-on-weekday)
			 (choice :value 1
				 (const :tag "Today" nil)
				 (integer :tag "Weekday No.")))
		   (list :tag "Include data from diary"
			 (const org-agenda-include-diary)
			 (boolean))
		   (list :tag "Deadline Warning days"
			 (const org-deadline-warning-days)
			 (integer :value 1))
		   (list :tag "Category filter preset"
			 (const org-agenda-category-filter-preset)
			 (list
			  (const :format "" quote)
			  (repeat
			   (string :tag "+category or -category"))))
		   (list :tag "Tags filter preset"
			 (const org-agenda-tag-filter-preset)
			 (list
			  (const :format "" quote)
			  (repeat
			   (string :tag "+tag or -tag"))))
		   (list :tag "Effort filter preset"
			 (const org-agenda-effort-filter-preset)
			 (list
			  (const :format "" quote)
			  (repeat
			   (string :tag "+=10 or -=10 or +<10 or ->10"))))
		   (list :tag "Regexp filter preset"
			 (const org-agenda-regexp-filter-preset)
			 (list
			  (const :format "" quote)
			  (repeat
			   (string :tag "+regexp or -regexp"))))
		   (list :tag "Set daily/weekly entry types"
			 (const org-agenda-entry-types)
			 (list
			  (const :format "" quote)
			  (set :greedy t :value ,org-agenda-entry-types
			       (const :deadline)
			       (const :scheduled)
			       (const :deadline*)
			       (const :scheduled*)
			       (const :timestamp)
			       (const :sexp))))
		   (list :tag "Standard skipping condition"
			 :value (org-agenda-skip-function '(org-agenda-skip-entry-if))
			 (const org-agenda-skip-function)
			 (list
			  (const :format "" quote)
			  (list
			   (choice
			    :tag "Skipping range"
			    (const :tag "Skip entry" org-agenda-skip-entry-if)
			    (const :tag "Skip subtree" org-agenda-skip-subtree-if))
			   (repeat :inline t :tag "Conditions for skipping"
				   (choice
				    :tag "Condition type"
				    (list :tag "Regexp matches" :inline t
					  (const :format "" 'regexp)
					  (regexp))
				    (list :tag "Regexp does not match" :inline t
					  (const :format "" 'notregexp)
					  (regexp))
				    (list :tag "TODO state is" :inline t
					  (const 'todo)
					  (choice
					   (const :tag "Any not-done state" 'todo)
					   (const :tag "Any done state" 'done)
					   (const :tag "Any state" 'any)
					   (list :tag "Keyword list"
						 (const :format "" quote)
						 (repeat (string :tag "Keyword")))))
				    (list :tag "TODO state is not" :inline t
					  (const 'nottodo)
					  (choice
					   (const :tag "Any not-done state" 'todo)
					   (const :tag "Any done state" 'done)
					   (const :tag "Any state" 'any)
					   (list :tag "Keyword list"
						 (const :format "" quote)
						 (repeat (string :tag "Keyword")))))
				    (const :tag "scheduled" 'scheduled)
				    (const :tag "not scheduled" 'notscheduled)
				    (const :tag "deadline" 'deadline)
				    (const :tag "no deadline" 'notdeadline)
				    (const :tag "timestamp" 'timestamp)
				    (const :tag "no timestamp" 'nottimestamp))))))
		   (list :tag "Non-standard skipping condition"
			 :value (org-agenda-skip-function)
			 (const org-agenda-skip-function)
			 (sexp :tag "Function or form (quoted!)"))
		   (list :tag "Any variable"
			 (variable :tag "Variable")
			 (sexp :tag "Value (sexp)"))))
  "Selection of examples for agenda command settings.
This will be spliced into the custom type of
`org-agenda-custom-commands'.")


(defcustom org-agenda-custom-commands
  '(("n" "Agenda and all TODOs" ((agenda "") (alltodo ""))))
  "Custom commands for the agenda.
\\<org-mode-map>
These commands will be offered on the splash screen displayed by the
agenda dispatcher `\\[org-agenda]'.  Each entry is a list like this:

   (key desc type match settings files)

key      The key (one or more characters as a string) to be associated
         with the command.
desc     A description of the command, when omitted or nil, a default
         description is built using MATCH.
type     The command type, any of the following symbols:
          agenda      The daily/weekly agenda.
          todo        Entries with a specific TODO keyword, in all agenda files.
          search      Entries containing search words entry or headline.
          tags        Tags/Property/TODO match in all agenda files.
          tags-todo   Tags/P/T match in all agenda files, TODO entries only.
          todo-tree   Sparse tree of specific TODO keyword in *current* file.
          tags-tree   Sparse tree with all tags matches in *current* file.
          occur-tree  Occur sparse tree for *current* file.
          ...         A user-defined function.
match    What to search for:
          - a single keyword for TODO keyword searches
          - a tags match expression for tags searches
          - a word search expression for text searches.
          - a regular expression for occur searches
          For all other commands, this should be the empty string.
settings  A list of option settings, similar to that in a let form, so like
          this: ((opt1 val1) (opt2 val2) ...).   The values will be
          evaluated at the moment of execution, so quote them when needed.
files     A list of files to write the produced agenda buffer to with
          the command `org-store-agenda-views'.
          If a file name ends in \".html\", an HTML version of the buffer
          is written out.  If it ends in \".ps\", a postscript version is
          produced.  Otherwise, only the plain text is written to the file.

You can also define a set of commands, to create a composite agenda buffer.
In this case, an entry looks like this:

  (key desc (cmd1 cmd2 ...) general-settings-for-whole-set files)

where

desc   A description string to be displayed in the dispatcher menu.
cmd    An agenda command, similar to the above.  However, tree commands
       are not allowed, but instead you can get agenda and global todo list.
       So valid commands for a set are:
       (agenda \"\" settings)
       (alltodo \"\" settings)
       (stuck \"\" settings)
       (todo \"match\" settings files)
       (search \"match\" settings files)
       (tags \"match\" settings files)
       (tags-todo \"match\" settings files)

Each command can carry a list of options, and another set of options can be
given for the whole set of commands.  Individual command options take
precedence over the general options.

When using several characters as key to a command, the first characters
are prefix commands.  For the dispatcher to display useful information, you
should provide a description for the prefix, like

 (setq org-agenda-custom-commands
   \\='((\"h\" . \"HOME + Name tag searches\") ; describe prefix \"h\"
     (\"hl\" tags \"+HOME+Lisa\")
     (\"hp\" tags \"+HOME+Peter\")
     (\"hk\" tags \"+HOME+Kim\")))"
  :group 'org-agenda-custom-commands
  :type `(repeat
	  (choice :value ("x" "Describe command here" tags "" nil)
		  (list :tag "Single command"
			(string :tag "Access Key(s) ")
			(option (string :tag "Description"))
			(choice
			 (const :tag "Agenda" agenda)
			 (const :tag "TODO list" alltodo)
			 (const :tag "Search words" search)
			 (const :tag "Stuck projects" stuck)
			 (const :tag "Tags/Property match (all agenda files)" tags)
			 (const :tag "Tags/Property match of TODO entries (all agenda files)" tags-todo)
			 (const :tag "TODO keyword search (all agenda files)" todo)
			 (const :tag "Tags sparse tree (current buffer)" tags-tree)
			 (const :tag "TODO keyword tree (current buffer)" todo-tree)
			 (const :tag "Occur tree (current buffer)" occur-tree)
			 (sexp :tag "Other, user-defined function"))
			(string :tag "Match (only for some commands)")
			,org-agenda-custom-commands-local-options
			(option (repeat :tag "Export" (file :tag "Export to"))))
		  (list :tag "Command series, all agenda files"
			(string :tag "Access Key(s)")
			(string :tag "Description  ")
			(repeat :tag "Component"
				(choice
				 (list :tag "Agenda"
				       (const :format "" agenda)
				       (const :tag "" :format "" "")
				       ,org-agenda-custom-commands-local-options)
				 (list :tag "TODO list (all keywords)"
				       (const :format "" alltodo)
				       (const :tag "" :format "" "")
				       ,org-agenda-custom-commands-local-options)
				 (list :tag "Search words"
				       (const :format "" search)
				       (string :tag "Match")
				       ,org-agenda-custom-commands-local-options)
				 (list :tag "Stuck projects"
				       (const :format "" stuck)
				       (const :tag "" :format "" "")
				       ,org-agenda-custom-commands-local-options)
				 (list :tag "Tags search"
				       (const :format "" tags)
				       (string :tag "Match")
				       ,org-agenda-custom-commands-local-options)
				 (list :tag "Tags search, TODO entries only"
				       (const :format "" tags-todo)
				       (string :tag "Match")
				       ,org-agenda-custom-commands-local-options)
				 (list :tag "TODO keyword search"
				       (const :format "" todo)
				       (string :tag "Match")
				       ,org-agenda-custom-commands-local-options)
				 (list :tag "Other, user-defined function"
				       (symbol :tag "function")
				       (string :tag "Match")
				       ,org-agenda-custom-commands-local-options)))

			(repeat :tag "Settings for entire command set"
				(list (variable :tag "Any variable")
				      (sexp :tag "Value")))
			(option (repeat :tag "Export" (file :tag "Export to"))))
		  (cons :tag "Prefix key documentation"
			(string :tag "Access Key(s)")
			(string :tag "Description  ")))))

(defcustom org-agenda-query-register ?o
  "The register holding the current query string.
The purpose of this is that if you construct a query string interactively,
you can then use it to define a custom command."
  :group 'org-agenda-custom-commands
  :type 'character)

(defcustom org-stuck-projects
  '("+LEVEL=2/-DONE" ("TODO" "NEXT" "NEXTACTION") nil "")
  "How to identify stuck projects.
This is a list of four items:
1. A tags/todo/property matcher string that is used to identify a project.
   See the manual for a description of tag and property searches.
   The entire tree below a headline matched by this is considered one project.
2. A list of TODO keywords identifying non-stuck projects.
   If the project subtree contains any headline with one of these todo
   keywords, the project is considered to be not stuck.  If you specify
   \"*\" as a keyword, any TODO keyword will mark the project unstuck.
3. A list of tags identifying non-stuck projects.
   If the project subtree contains any headline with one of these tags,
   the project is considered to be not stuck.  If you specify \"*\" as
   a tag, any tag will mark the project unstuck.  Note that this is about
   the explicit presence of a tag somewhere in the subtree, inherited
   tags do not count here.  If inherited tags make a project not stuck,
   use \"-TAG\" in the tags part of the matcher under (1.) above.
4. An arbitrary regular expression matching non-stuck projects.

If the project turns out to be not stuck, search continues also in the
subtree to see if any of the subtasks have project status.

See also the variable `org-tags-match-list-sublevels' which applies
to projects matched by this search as well.

After defining this variable, you may use `\\[org-agenda-list-stuck-projects]'
\(bound to `C-c a #') to produce the list."
  :group 'org-agenda-custom-commands
  :type '(list
	  (string :tag "Tags/TODO match to identify a project")
	  (repeat :tag "Projects are *not* stuck if they have an entry with \
TODO keyword any of" (string))
	  (repeat :tag "Projects are *not* stuck if they have an entry with \
TAG being any of" (string))
	  (regexp :tag "Projects are *not* stuck if this regexp matches inside \
the subtree")))

(defgroup org-agenda-skip nil
  "Options concerning skipping parts of agenda files."
  :tag "Org Agenda Skip"
  :group 'org-agenda)

(defcustom org-agenda-skip-function-global nil
  "Function to be called at each match during agenda construction.
If this function returns nil, the current match should not be skipped.
If the function decided to skip an agenda match, is must return the
buffer position from which the search should be continued.
This may also be a Lisp form, which will be evaluated.

This variable will be applied to every agenda match, including
tags/property searches and TODO lists.  So try to make the test function
do its checking as efficiently as possible.  To implement a skipping
condition just for specific agenda commands, use the variable
`org-agenda-skip-function' which can be set in the options section
of custom agenda commands."
  :group 'org-agenda-skip
  :type 'sexp)

(defgroup org-agenda-daily/weekly nil
  "Options concerning the daily/weekly agenda."
  :tag "Org Agenda Daily/Weekly"
  :group 'org-agenda)
(defgroup org-agenda-todo-list nil
  "Options concerning the global todo list agenda view."
  :tag "Org Agenda Todo List"
  :group 'org-agenda)
(defgroup org-agenda-match-view nil
  "Options concerning the general tags/property/todo match agenda view."
  :tag "Org Agenda Match View"
  :group 'org-agenda)
(defgroup org-agenda-search-view nil
  "Options concerning the search agenda view."
  :tag "Org Agenda Search View"
  :group 'org-agenda)

(defvar org-agenda-archives-mode nil
  "Non-nil means the agenda will include archived items.
If this is the symbol `trees', trees in the selected agenda scope
that are marked with the ARCHIVE tag will be included anyway.  When this is
t, also all archive files associated with the current selection of agenda
files will be included.")

(defcustom org-agenda-restriction-lock-highlight-subtree t
  "Non-nil means highlight the whole subtree when restriction is active.
Otherwise only highlight the headline.  Highlighting the whole subtree is
useful to ensure no edits happen beyond the restricted region."
  :group 'org-agenda
  :type 'boolean)

(defcustom org-agenda-skip-comment-trees t
  "Non-nil means skip trees that start with the COMMENT keyword.
When nil, these trees are also scanned by agenda commands."
  :group 'org-agenda-skip
  :type 'boolean)

(defcustom org-agenda-todo-list-sublevels t
  "Non-nil means check also the sublevels of a TODO entry for TODO entries.
When nil, the sublevels of a TODO entry are not checked, resulting in
potentially much shorter TODO lists."
  :group 'org-agenda-skip
  :group 'org-agenda-todo-list
  :type 'boolean)

(defcustom org-agenda-todo-ignore-with-date nil
  "Non-nil means don't show entries with a date in the global todo list.
You can use this if you prefer to mark mere appointments with a TODO keyword,
but don't want them to show up in the TODO list.
When this is set, it also covers deadlines and scheduled items, the settings
of `org-agenda-todo-ignore-scheduled' and `org-agenda-todo-ignore-deadlines'
will be ignored.
See also the variable `org-agenda-tags-todo-honor-ignore-options'."
  :group 'org-agenda-skip
  :group 'org-agenda-todo-list
  :type 'boolean)

(defcustom org-agenda-todo-ignore-timestamp nil
  "Non-nil means don't show entries with a timestamp.
This applies when creating the global todo list.
Valid values are:

past     Don't show entries for today or in the past.

future   Don't show entries with a timestamp in the future.
         The idea behind this is that if it has a future
         timestamp, you don't want to think about it until the
         date.

all      Don't show any entries with a timestamp in the global todo list.
         The idea behind this is that by setting a timestamp, you
         have already \"taken care\" of this item.

This variable can also have an integer as a value.  If positive (N),
todos with a timestamp N or more days in the future will be ignored.  If
negative (-N), todos with a timestamp N or more days in the past will be
ignored.  If 0, todos with a timestamp either today or in the future will
be ignored.  For example, a value of -1 will exclude todos with a
timestamp in the past (yesterday or earlier), while a value of 7 will
exclude todos with a timestamp a week or more in the future.

See also `org-agenda-todo-ignore-with-date'.
See also the variable `org-agenda-tags-todo-honor-ignore-options' if you want
to make his option also apply to the tags-todo list."
  :group 'org-agenda-skip
  :group 'org-agenda-todo-list
  :version "24.1"
  :type '(choice
	  (const :tag "Ignore future timestamp todos" future)
	  (const :tag "Ignore past or present timestamp todos" past)
	  (const :tag "Ignore all timestamp todos" all)
	  (const :tag "Show timestamp todos" nil)
	  (integer :tag "Ignore if N or more days in past(-) or future(+).")))

(defcustom org-agenda-todo-ignore-scheduled nil
  "Non-nil means, ignore some scheduled TODO items when making TODO list.
This applies when creating the global todo list.
Valid values are:

past     Don't show entries scheduled today or in the past.

future   Don't show entries scheduled in the future.
         The idea behind this is that by scheduling it, you don't want to
         think about it until the scheduled date.

all      Don't show any scheduled entries in the global todo list.
         The idea behind this is that by scheduling it, you have already
         \"taken care\" of this item.

t        Same as `all', for backward compatibility.

This variable can also have an integer as a value.  See
`org-agenda-todo-ignore-timestamp' for more details.

See also `org-agenda-todo-ignore-with-date'.
See also the variable `org-agenda-tags-todo-honor-ignore-options' if you want
to make his option also apply to the tags-todo list."
  :group 'org-agenda-skip
  :group 'org-agenda-todo-list
  :type '(choice
	  (const :tag "Ignore future-scheduled todos" future)
	  (const :tag "Ignore past- or present-scheduled todos" past)
	  (const :tag "Ignore all scheduled todos" all)
	  (const :tag "Ignore all scheduled todos (compatibility)" t)
	  (const :tag "Show scheduled todos" nil)
	  (integer :tag "Ignore if N or more days in past(-) or future(+).")))

(defcustom org-agenda-todo-ignore-deadlines nil
  "Non-nil means ignore some deadline TODO items when making TODO list.

There are different motivations for using different values, please think
carefully when configuring this variable.

This applies when creating the global TODO list.

Valid values are:

near    Don't show near deadline entries.  A deadline is near when it is
        closer than `org-deadline-warning-days' days.  The idea behind this
        is that such items will appear in the agenda anyway.

far     Don't show TODO entries where a deadline has been defined, but
        is not going to happen anytime soon.  This is useful if you want to use
        the TODO list to figure out what to do now.

past    Don't show entries with a deadline timestamp for today or in the past.

future  Don't show entries with a deadline timestamp in the future, not even
        when they become `near' ones.  Use it with caution.

all     Ignore all TODO entries that do have a deadline.

t       Same as `near', for backward compatibility.

This variable can also have an integer as a value.  See
`org-agenda-todo-ignore-timestamp' for more details.

See also `org-agenda-todo-ignore-with-date'.
See also the variable `org-agenda-tags-todo-honor-ignore-options' if you want
to make his option also apply to the tags-todo list."
  :group 'org-agenda-skip
  :group 'org-agenda-todo-list
  :type '(choice
	  (const :tag "Ignore near deadlines" near)
	  (const :tag "Ignore near deadlines (compatibility)" t)
	  (const :tag "Ignore far deadlines" far)
	  (const :tag "Ignore all TODOs with a deadlines" all)
	  (const :tag "Show all TODOs, even if they have a deadline" nil)
	  (integer :tag "Ignore if N or more days in past(-) or future(+).")))

(defcustom org-agenda-todo-ignore-time-comparison-use-seconds nil
  "Time unit to use when possibly ignoring an agenda item.

See the docstring of various `org-agenda-todo-ignore-*' options.
The default is to compare time stamps using days.  An item is thus
considered to be in the future if it is at least one day after today.
Non-nil means to compare time stamps using seconds.  An item is then
considered future if it has a time value later than current time."
  :group 'org-agenda-skip
  :group 'org-agenda-todo-list
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Compare time with days" nil)
	  (const :tag "Compare time with seconds" t)))

(defcustom org-agenda-tags-todo-honor-ignore-options nil
  "Non-nil means honor todo-list ignores options also in tags-todo search.
The variables
   `org-agenda-todo-ignore-with-date',
   `org-agenda-todo-ignore-timestamp',
   `org-agenda-todo-ignore-scheduled',
   `org-agenda-todo-ignore-deadlines'
make the global TODO list skip entries that have time stamps of certain
kinds.  If this option is set, the same options will also apply for the
tags-todo search, which is the general tags/property matcher
restricted to unfinished TODO entries only."
  :group 'org-agenda-skip
  :group 'org-agenda-todo-list
  :group 'org-agenda-match-view
  :type 'boolean)

(defcustom org-agenda-skip-scheduled-if-done nil
  "Non-nil means don't show scheduled items in agenda when they are done.
This is relevant for the daily/weekly agenda, not for the TODO list.  It
applies only to the actual date of the scheduling.  Warnings about an item
with a past scheduling dates are always turned off when the item is DONE."
  :group 'org-agenda-skip
  :group 'org-agenda-daily/weekly
  :type 'boolean)

(defcustom org-agenda-skip-scheduled-if-deadline-is-shown nil
  "Non-nil means skip scheduling line if same entry shows because of deadline.

In the agenda of today, an entry can show up multiple times
because it is both scheduled and has a nearby deadline, and maybe
a plain time stamp as well.

When this variable is nil, the entry will be shown several times.

When set to t, then only the deadline is shown and the fact that
the entry is scheduled today or was scheduled previously is not
shown.

When set to the symbol `not-today', skip scheduled previously,
but not scheduled today.

When set to the symbol `repeated-after-deadline', skip scheduled
items if they are repeated beyond the current deadline."
  :group 'org-agenda-skip
  :group 'org-agenda-daily/weekly
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Always" t)
	  (const :tag "Not when scheduled today" not-today)
	  (const :tag "When repeated past deadline" repeated-after-deadline)))

(defcustom org-agenda-skip-timestamp-if-deadline-is-shown nil
  "Non-nil means skip timestamp line if same entry shows because of deadline.
In the agenda of today, an entry can show up multiple times
because it has both a plain timestamp and has a nearby deadline.
When this variable is t, then only the deadline is shown and the
fact that the entry has a timestamp for or including today is not
shown.  When this variable is nil, the entry will be shown
several times."
  :group 'org-agenda-skip
  :group 'org-agenda-daily/weekly
  :version "24.1"
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Always" t)))

(defcustom org-agenda-skip-deadline-if-done nil
  "Non-nil means don't show deadlines when the corresponding item is done.
When nil, the deadline is still shown and should give you a happy feeling.
This is relevant for the daily/weekly agenda.  It applies only to the
actual date of the deadline.  Warnings about approaching and past-due
deadlines are always turned off when the item is DONE."
  :group 'org-agenda-skip
  :group 'org-agenda-daily/weekly
  :type 'boolean)

(defcustom org-agenda-skip-deadline-prewarning-if-scheduled nil
  "Non-nil means skip deadline prewarning when entry is also scheduled.
This will apply on all days where a prewarning for the deadline would
be shown, but not at the day when the entry is actually due.  On that day,
the deadline will be shown anyway.
This variable may be set to nil, t, the symbol `pre-scheduled',
or a number which will then give the number of days before the actual
deadline when the prewarnings should resume.  The symbol `pre-scheduled'
eliminates the deadline prewarning only prior to the scheduled date.
This can be used in a workflow where the first showing of the deadline will
trigger you to schedule it, and then you don't want to be reminded of it
because you will take care of it on the day when scheduled."
  :group 'org-agenda-skip
  :group 'org-agenda-daily/weekly
  :version "24.1"
  :type '(choice
	  (const :tag "Always show prewarning" nil)
	  (const :tag "Remove prewarning prior to scheduled date" pre-scheduled)
	  (const :tag "Remove prewarning if entry is scheduled" t)
	  (integer :tag "Restart prewarning N days before deadline")))

(defcustom org-agenda-skip-scheduled-delay-if-deadline nil
  "Non-nil means skip scheduled delay when entry also has a deadline.
This variable may be set to nil, t, the symbol `post-deadline',
or a number which will then give the number of days after the actual
scheduled date when the delay should expire.  The symbol `post-deadline'
eliminates the schedule delay when the date is posterior to the deadline."
  :group 'org-agenda-skip
  :group 'org-agenda-daily/weekly
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Always honor delay" nil)
	  (const :tag "Ignore delay if posterior to the deadline" post-deadline)
	  (const :tag "Ignore delay if entry has a deadline" t)
	  (integer :tag "Honor delay up until N days after the scheduled date")))

(defcustom org-agenda-skip-additional-timestamps-same-entry nil
  "When nil, multiple same-day timestamps in entry make multiple agenda lines.
When non-nil, after the search for timestamps has matched once in an
entry, the rest of the entry will not be searched."
  :group 'org-agenda-skip
  :type 'boolean)

(defcustom org-agenda-skip-timestamp-if-done nil
  "Non-nil means don't select item by timestamp or -range if it is DONE."
  :group 'org-agenda-skip
  :group 'org-agenda-daily/weekly
  :type 'boolean)

(defcustom org-agenda-dim-blocked-tasks t
  "Non-nil means dim blocked tasks in the agenda display.
This causes some overhead during agenda construction, but if you
have turned on `org-enforce-todo-dependencies',
`org-enforce-todo-checkbox-dependencies', or any other blocking
mechanism, this will create useful feedback in the agenda.

Instead of t, this variable can also have the value `invisible'.
Then blocked tasks will be invisible and only become visible when
they become unblocked.  An exemption to this behavior is when a task is
blocked because of unchecked checkboxes below it.  Since checkboxes do
not show up in the agenda views, making this task invisible you remove any
trace from agenda views that there is something to do.  Therefore, a task
that is blocked because of checkboxes will never be made invisible, it
will only be dimmed."
  :group 'org-agenda-daily/weekly
  :group 'org-agenda-todo-list
  :version "24.3"
  :type '(choice
	  (const :tag "Do not dim" nil)
	  (const :tag "Dim to a gray face" t)
	  (const :tag "Make invisible" invisible)))

(defgroup org-agenda-startup nil
  "Options concerning initial settings in the Agenda in Org Mode."
  :tag "Org Agenda Startup"
  :group 'org-agenda)

(defcustom org-agenda-menu-show-matcher t
  "Non-nil means show the match string in the agenda dispatcher menu.
When nil, the matcher string is not shown, but is put into the help-echo
property so than moving the mouse over the command shows it.
Setting it to nil is good if matcher strings are very long and/or if
you want to use two-columns display (see `org-agenda-menu-two-columns')."
  :group 'org-agenda
  :version "24.1"
  :type 'boolean)

(defcustom org-agenda-menu-two-columns nil
  "Non-nil means, use two columns to show custom commands in the dispatcher.
If you use this, you probably want to set `org-agenda-menu-show-matcher'
to nil."
  :group 'org-agenda
  :version "24.1"
  :type 'boolean)

(defcustom org-agenda-finalize-hook nil
  "Hook run just before displaying an agenda buffer.
The buffer is still writable when the hook is called.

You can modify some of the buffer substrings but you should be
extra careful not to modify the text properties of the agenda
headlines as the agenda display heavily relies on them."
  :group 'org-agenda-startup
  :type 'hook)

(defcustom org-agenda-mouse-1-follows-link nil
  "Non-nil means mouse-1 on a link will follow the link in the agenda.
A longer mouse click will still set point.  Needs to be set
before org.el is loaded."
  :group 'org-agenda-startup
  :type 'boolean)

(defcustom org-agenda-start-with-follow-mode nil
  "The initial value of follow mode in a newly created agenda window."
  :group 'org-agenda-startup
  :type 'boolean)

(defcustom org-agenda-follow-indirect nil
  "Non-nil means `org-agenda-follow-mode' displays only the
current item's tree, in an indirect buffer."
  :group 'org-agenda
  :version "24.1"
  :type 'boolean)

(defcustom org-agenda-show-outline-path t
  "Non-nil means show outline path in echo area after line motion."
  :group 'org-agenda-startup
  :type 'boolean)

(defcustom org-agenda-start-with-entry-text-mode nil
  "The initial value of entry-text-mode in a newly created agenda window."
  :group 'org-agenda-startup
  :type 'boolean)

(defcustom org-agenda-entry-text-maxlines 5
  "Number of text lines to be added when `E' is pressed in the agenda.

Note that this variable only used during agenda display.  To add entry text
when exporting the agenda, configure the variable
`org-agenda-add-entry-text-maxlines'."
  :group 'org-agenda
  :type 'integer)

(defcustom org-agenda-entry-text-exclude-regexps nil
  "List of regular expressions to clean up entry text.
The complete matches of all regular expressions in this list will be
removed from entry text before it is shown in the agenda."
  :group 'org-agenda
  :type '(repeat (regexp)))

(defcustom org-agenda-entry-text-leaders "    > "
  "Text prepended to the entry text in agenda buffers."
  :version "24.4"
  :package-version '(Org . "8.0")
  :group 'org-agenda
  :type 'string)

(defvar org-agenda-entry-text-cleanup-hook nil
  "Hook that is run after basic cleanup of entry text to be shown in agenda.
This cleanup is done in a temporary buffer, so the function may inspect and
change the entire buffer.
Some default stuff like drawers and scheduling/deadline dates will already
have been removed when this is called, as will any matches for regular
expressions listed in `org-agenda-entry-text-exclude-regexps'.")

(defvar org-agenda-include-inactive-timestamps nil
  "Non-nil means include inactive time stamps in agenda.
Dynamically scoped.")

(defgroup org-agenda-windows nil
  "Options concerning the windows used by the Agenda in Org Mode."
  :tag "Org Agenda Windows"
  :group 'org-agenda)

(defcustom org-agenda-window-setup 'reorganize-frame
  "How the agenda buffer should be displayed.
Possible values for this option are:

current-window    Show agenda in the current window, keeping all other windows.
other-window      Use `switch-to-buffer-other-window' to display agenda.
only-window       Show agenda, deleting all other windows.
reorganize-frame  Show only two windows on the current frame, the current
                  window and the agenda.
other-frame       Use `switch-to-buffer-other-frame' to display agenda.
                  Also, when exiting the agenda, kill that frame.
See also the variable `org-agenda-restore-windows-after-quit'."
  :group 'org-agenda-windows
  :type '(choice
	  (const current-window)
	  (const other-frame)
	  (const other-window)
	  (const only-window)
	  (const reorganize-frame)))

(defcustom org-agenda-window-frame-fractions '(0.5 . 0.75)
  "The min and max height of the agenda window as a fraction of frame height.
The value of the variable is a cons cell with two numbers between 0 and 1.
It only matters if `org-agenda-window-setup' is `reorganize-frame'."
  :group 'org-agenda-windows
  :type '(cons (number :tag "Minimum") (number :tag "Maximum")))

(defcustom org-agenda-restore-windows-after-quit nil
  "Non-nil means restore window configuration upon exiting agenda.
Before the window configuration is changed for displaying the agenda,
the current status is recorded.  When the agenda is exited with
`q' or `x' and this option is set, the old state is restored.  If
`org-agenda-window-setup' is `other-frame', the value of this
option will be ignored."
  :group 'org-agenda-windows
  :type 'boolean)

(defcustom org-agenda-span 'week
  "Number of days to include in overview display.
Can be day, week, month, year, or any number of days.
Custom commands can set this variable in the options section."
  :group 'org-agenda-daily/weekly
  :type '(choice (const :tag "Day" day)
		 (const :tag "Week" week)
		 (const :tag "Fortnight" fortnight)
		 (const :tag "Month" month)
		 (const :tag "Year" year)
		 (integer :tag "Custom")))

(defcustom org-agenda-start-on-weekday 1
  "Non-nil means start the overview always on the specified weekday.
0 denotes Sunday, 1 denotes Monday, etc.
When nil, always start on the current day.
Custom commands can set this variable in the options section."
  :group 'org-agenda-daily/weekly
  :type '(choice (const :tag "Today" nil)
		 (integer :tag "Weekday No.")))

(defcustom org-agenda-show-all-dates t
  "Non-nil means `org-agenda' shows every day in the selected range.
When nil, only the days which actually have entries are shown."
  :group 'org-agenda-daily/weekly
  :type 'boolean)

(defcustom org-agenda-format-date 'org-agenda-format-date-aligned
  "Format string for displaying dates in the agenda.
Used by the daily/weekly agenda.  This should be a format string
understood by `format-time-string', or a function returning the
formatted date as a string.  The function must take a single
argument, a calendar-style date list like (month day year)."
  :group 'org-agenda-daily/weekly
  :type '(choice
	  (string :tag "Format string")
	  (function :tag "Function")))

(defun org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda.
This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date))
	 (day (cadr date))
	 (day-of-week (calendar-day-of-week date))
	 (month (car date))
	 (monthname (calendar-month-name month))
	 (year (nth 2 date))
	 (iso-week (org-days-to-iso-week
		    (calendar-absolute-from-gregorian date)))
	 (weekyear (cond ((and (= month 1) (>= iso-week 52))
			  (1- year))
			 ((and (= month 12) (<= iso-week 1))
			  (1+ year))
			 (t year)))
	 (weekstring (if (= day-of-week 1)
			 (format " W%02d" iso-week)
		       "")))
    (format "%-10s %2d %s %4d%s"
	    dayname day monthname year weekstring)))

(defcustom org-agenda-time-leading-zero nil
  "Non-nil means use leading zero for military times in agenda.
For example, 9:30am would become 09:30 rather than  9:30."
  :group 'org-agenda-daily/weekly
  :version "24.1"
  :type 'boolean)

(defcustom org-agenda-timegrid-use-ampm nil
  "When set, show AM/PM style timestamps on the timegrid."
  :group 'org-agenda
  :version "24.1"
  :type 'boolean)

(defun org-agenda-time-of-day-to-ampm (time)
  "Convert TIME of a string like \"13:45\" to an AM/PM style time string."
  (let* ((hour-number (string-to-number (substring time 0 -3)))
         (minute (substring time -2))
         (ampm "am"))
    (cond
     ((equal hour-number 12)
      (setq ampm "pm"))
     ((> hour-number 12)
      (setq ampm "pm")
      (setq hour-number (- hour-number 12))))
    (concat
     (if org-agenda-time-leading-zero
	 (format "%02d" hour-number)
       (format "%02s" (number-to-string hour-number)))
     ":" minute ampm)))

(defun org-agenda-time-of-day-to-ampm-maybe (time)
  "Conditionally convert TIME to AM/PM format based on `org-agenda-timegrid-use-ampm'."
  (if org-agenda-timegrid-use-ampm
      (org-agenda-time-of-day-to-ampm time)
    time))

(defcustom org-agenda-weekend-days '(6 0)
  "Which days are weekend?
These days get the special face `org-agenda-date-weekend' in the agenda."
  :group 'org-agenda-daily/weekly
  :type '(set :greedy t
	      (const :tag "Monday" 1)
	      (const :tag "Tuesday" 2)
	      (const :tag "Wednesday" 3)
	      (const :tag "Thursday" 4)
	      (const :tag "Friday" 5)
	      (const :tag "Saturday" 6)
	      (const :tag "Sunday" 0)))

(defcustom org-agenda-move-date-from-past-immediately-to-today t
  "Non-nil means jump to today when moving a past date forward in time.
When using S-right in the agenda to move a a date forward, and the date
stamp currently points to the past, the first key press will move it
to today.  WHen nil, just move one day forward even if the date stays
in the past."
  :group 'org-agenda-daily/weekly
  :version "24.1"
  :type 'boolean)

(defcustom org-agenda-include-diary nil
  "If non-nil, include in the agenda entries from the Emacs Calendar's diary.
Custom commands can set this variable in the options section."
  :group 'org-agenda-daily/weekly
  :type 'boolean)

(defcustom org-agenda-include-deadlines t
  "If non-nil, include entries within their deadline warning period.
Custom commands can set this variable in the options section."
  :group 'org-agenda-daily/weekly
  :version "24.1"
  :type 'boolean)

(defcustom org-agenda-show-future-repeats t
  "Non-nil shows repeated entries in the future part of the agenda.
When set to the symbol `next' only the first future repeat is shown."
  :group 'org-agenda-daily/weekly
  :type '(choice
	  (const :tag "Show all repeated entries" t)
	  (const :tag "Show next repeated entry" next)
	  (const :tag "Do not show repeated entries" nil))
  :version "26.1"
  :package-version '(Org . "9.1")
  :safe #'symbolp)

(defcustom org-agenda-prefer-last-repeat nil
  "Non-nil sets date for repeated entries to their last repeat.

When nil, display SCHEDULED and DEADLINE dates at their base
date, and in today's agenda, as a reminder.  Display plain
time-stamps, on the other hand, at every repeat date in the past
in addition to the base date.

When non-nil, show a repeated entry at its latest repeat date,
possibly being today even if it wasn't marked as done.  This
setting is useful if you do not always mark repeated entries as
done and, yet, consider that reaching repeat date starts the task
anew.

When set to a list of strings, prefer last repeats only for
entries with these TODO keywords."
  :group 'org-agenda-daily/weekly
  :type '(choice
	  (const :tag "Prefer last repeat" t)
	  (const :tag "Prefer base date" nil)
	  (repeat :tag "Prefer last repeat for entries with these TODO keywords"
		  (string :tag "TODO keyword")))
  :version "26.1"
  :package-version '(Org . "9.1")
  :safe (lambda (x) (or (booleanp x) (consp x))))

(defcustom org-scheduled-past-days 10000
  "Number of days to continue listing scheduled items not marked DONE.
When an item is scheduled on a date, it shows up in the agenda on
this day and will be listed until it is marked done or for the
number of days given here."
  :group 'org-agenda-daily/weekly
  :type 'integer
  :safe 'integerp)

(defcustom org-deadline-past-days 10000
  "Number of days to warn about missed deadlines.
When an item has deadline on a date, it shows up in the agenda on
this day and will appear as a reminder until it is marked DONE or
for the number of days given here."
  :group 'org-agenda-daily/weekly
  :type 'integer
  :version "26.1"
  :package-version '(Org . "9.1")
  :safe 'integerp)

(defcustom org-agenda-log-mode-items '(closed clock)
  "List of items that should be shown in agenda log mode.
\\<org-agenda-mode-map>\
This list may contain the following symbols:

  closed    Show entries that have been closed on that day.
  clock     Show entries that have received clocked time on that day.
  state     Show all logged state changes.
Note that instead of changing this variable, you can also press \
`\\[universal-argument] \\[org-agenda-log-mode]' in
the agenda to display all available LOG items temporarily."
  :group 'org-agenda-daily/weekly
  :type '(set :greedy t (const closed) (const clock) (const state)))

(defcustom org-agenda-clock-consistency-checks
  '(:max-duration "10:00" :min-duration 0 :max-gap "0:05"
		  :gap-ok-around ("4:00")
		  :default-face ((:background "DarkRed") (:foreground "white"))
		  :overlap-face nil :gap-face nil :no-end-time-face nil
		  :long-face nil :short-face nil)
  "This is a property list, with the following keys:

:max-duration    Mark clocking chunks that are longer than this time.
                 This is a time string like \"HH:MM\", or the number
                 of minutes as an integer.

:min-duration    Mark clocking chunks that are shorter that this.
                 This is a time string like \"HH:MM\", or the number
                 of minutes as an integer.

:max-gap         Mark gaps between clocking chunks that are longer than
                 this duration.  A number of minutes, or a string
                 like \"HH:MM\".

:gap-ok-around   List of times during the day which are usually not working
                 times.  When a gap is detected, but the gap contains any
                 of these times, the gap is *not* reported.  For example,
                 if this is (\"4:00\" \"13:00\") then gaps that contain
                 4:00 in the morning (i.e. the night) and 13:00
                 (i.e. a typical lunch time) do not cause a warning.
                 You should have at least one time during the night in this
                 list, or otherwise the first task each morning will trigger
                 a warning because it follows a long gap.

Furthermore, the following properties can be used to define faces for
issue display.

:default-face         the default face, if the specific face is undefined
:overlap-face         face for overlapping clocks
:gap-face             face for gaps between clocks
:no-end-time-face     face for incomplete clocks
:long-face            face for clock intervals that are too long
:short-face           face for clock intervals that are too short"
  :group 'org-agenda-daily/weekly
  :group 'org-clock
  :version "24.1"
  :type 'plist)

(defcustom org-agenda-log-mode-add-notes t
  "Non-nil means add first line of notes to log entries in agenda views.
If a log item like a state change or a clock entry is associated with
notes, the first line of these notes will be added to the entry in the
agenda display."
  :group 'org-agenda-daily/weekly
  :type 'boolean)

(defcustom org-agenda-start-with-log-mode nil
  "The initial value of log-mode in a newly created agenda window.
See `org-agenda-log-mode' and `org-agenda-log-mode-items' for further
explanations on the possible values."
  :group 'org-agenda-startup
  :group 'org-agenda-daily/weekly
  :type '(choice (const :tag "Don't show log items" nil)
		 (const :tag "Show only log items" only)
		 (const :tag "Show all possible log items" clockcheck)
		 (repeat :tag "Choose among possible values for `org-agenda-log-mode-items'"
			 (choice (const :tag "Show closed log items" closed)
				 (const :tag "Show clocked log items" clock)
				 (const :tag "Show all logged state changes" state)))))

(defcustom org-agenda-start-with-clockreport-mode nil
  "The initial value of clockreport-mode in a newly created agenda window."
  :group 'org-agenda-startup
  :group 'org-agenda-daily/weekly
  :type 'boolean)

(defcustom org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2)
  "Property list with parameters for the clocktable in clockreport mode.
This is the display mode that shows a clock table in the daily/weekly
agenda, the properties for this dynamic block can be set here.
The usual clocktable parameters are allowed here, but you cannot set
the properties :name, :tstart, :tend, :block, and :scope - these will
be overwritten to make sure the content accurately reflects the
current display in the agenda."
  :group 'org-agenda-daily/weekly
  :type 'plist)

(defcustom org-agenda-search-view-always-boolean nil
  "Non-nil means the search string is interpreted as individual parts.

The search string for search view can either be interpreted as a phrase,
or as a list of snippets that define a boolean search for a number of
strings.

When this is non-nil, the string will be split on whitespace, and each
snippet will be searched individually, and all must match in order to
select an entry.  A snippet is then a single string of non-white
characters, or a string in double quotes, or a regexp in {} braces.
If a snippet is preceded by \"-\", the snippet must *not* match.
\"+\" is syntactic sugar for positive selection.  Each snippet may
be found as a full word or a partial word, but see the variable
`org-agenda-search-view-force-full-words'.

When this is nil, search will look for the entire search phrase as one,
with each space character matching any amount of whitespace, including
line breaks.

Even when this is nil, you can still switch to Boolean search dynamically
by preceding the first snippet with \"+\" or \"-\".  If the first snippet
is a regexp marked with braces like \"{abc}\", this will also switch to
boolean search."
  :group 'org-agenda-search-view
  :version "24.1"
  :type 'boolean)

(defvaralias 'org-agenda-search-view-search-words-only
  'org-agenda-search-view-always-boolean)

(defcustom org-agenda-search-view-force-full-words nil
  "Non-nil means, search words must be matches as complete words.
When nil, they may also match part of a word."
  :group 'org-agenda-search-view
  :version "24.1"
  :type 'boolean)

(defcustom org-agenda-search-view-max-outline-level 0
  "Maximum outline level to display in search view.
E.g. when this is set to 1, the search view will only
show headlines of level 1.  When set to 0, the default
value, don't limit agenda view by outline level."
  :group 'org-agenda-search-view
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'integer)

(defgroup org-agenda-time-grid nil
  "Options concerning the time grid in the Org Agenda."
  :tag "Org Agenda Time Grid"
  :group 'org-agenda)

(defcustom org-agenda-search-headline-for-time t
  "Non-nil means search headline for a time-of-day.
If the headline contains a time-of-day in one format or another, it will
be used to sort the entry into the time sequence of items for a day.
Some people have time stamps in the headline that refer to the creation
time or so, and then this produces an unwanted side effect.  If this is
the case for your, use this variable to turn off searching the headline
for a time."
  :group 'org-agenda-time-grid
  :type 'boolean)

(defcustom org-agenda-use-time-grid t
  "Non-nil means show a time grid in the agenda schedule.
A time grid is a set of lines for specific times (like every two hours between
8:00 and 20:00).  The items scheduled for a day at specific times are
sorted in between these lines.
For details about when the grid will be shown, and what it will look like, see
the variable `org-agenda-time-grid'."
  :group 'org-agenda-time-grid
  :type 'boolean)

(defcustom org-agenda-time-grid
  '((daily today require-timed)
    (800 1000 1200 1400 1600 1800 2000)
    "......"
    "----------------")

  "The settings for time grid for agenda display.
This is a list of four items.  The first item is again a list.  It contains
symbols specifying conditions when the grid should be displayed:

 daily         if the agenda shows a single day
 weekly        if the agenda shows an entire week
 today         show grid on current date, independent of daily/weekly display
 require-timed show grid only if at least one item has a time specification
 remove-match  skip grid times already present in an entry

The second item is a list of integers, indicating the times that
should have a grid line.

The third item is a string which will be placed right after the
times that have a grid line.

The fourth item is a string placed after the grid times.  This
will align with agenda items"
  :group 'org-agenda-time-grid
  :type
  '(list
    (set :greedy t :tag "Grid Display Options"
	 (const :tag "Show grid in single day agenda display" daily)
	 (const :tag "Show grid in weekly agenda display" weekly)
	 (const :tag "Always show grid for today" today)
	 (const :tag "Show grid only if any timed entries are present"
		require-timed)
	 (const :tag "Skip grid times already present in an entry"
		remove-match))
    (repeat :tag "Grid Times" (integer :tag "Time"))
    (string :tag "Grid String (after agenda times)")
    (string :tag "Grid String (aligns with agenda items)")))

(defcustom org-agenda-show-current-time-in-grid t
  "Non-nil means show the current time in the time grid."
  :group 'org-agenda-time-grid
  :version "24.1"
  :type 'boolean)

(defcustom org-agenda-current-time-string
  "now - - - - - - - - - - - - - - - - - - - - - - - - -"
  "The string for the current time marker in the agenda."
  :group 'org-agenda-time-grid
  :version "24.1"
  :type 'string)

(defgroup org-agenda-sorting nil
  "Options concerning sorting in the Org Agenda."
  :tag "Org Agenda Sorting"
  :group 'org-agenda)

(defcustom org-agenda-sorting-strategy
  '((agenda habit-down time-up priority-down category-keep)
    (todo   priority-down category-keep)
    (tags   priority-down category-keep)
    (search category-keep))
  "Sorting structure for the agenda items of a single day.
This is a list of symbols which will be used in sequence to determine
if an entry should be listed before another entry.  The following
symbols are recognized:

time-up            Put entries with time-of-day indications first, early first
time-down          Put entries with time-of-day indications first, late first
timestamp-up       Sort by any timestamp, early first
timestamp-down     Sort by any timestamp, late first
scheduled-up       Sort by scheduled timestamp, early first
scheduled-down     Sort by scheduled timestamp, late first
deadline-up        Sort by deadline timestamp, early first
deadline-down      Sort by deadline timestamp, late first
ts-up              Sort by active timestamp, early first
ts-down            Sort by active timestamp, late first
tsia-up            Sort by inactive timestamp, early first
tsia-down          Sort by inactive timestamp, late first
category-keep      Keep the default order of categories, corresponding to the
		   sequence in `org-agenda-files'.
category-up        Sort alphabetically by category, A-Z.
category-down      Sort alphabetically by category, Z-A.
tag-up             Sort alphabetically by last tag, A-Z.
tag-down           Sort alphabetically by last tag, Z-A.
priority-up        Sort numerically by priority, high priority last.
priority-down      Sort numerically by priority, high priority first.
todo-state-up      Sort by todo state, tasks that are done last.
todo-state-down    Sort by todo state, tasks that are done first.
effort-up          Sort numerically by estimated effort, high effort last.
effort-down        Sort numerically by estimated effort, high effort first.
user-defined-up    Sort according to `org-agenda-cmp-user-defined', high last.
user-defined-down  Sort according to `org-agenda-cmp-user-defined', high first.
habit-up           Put entries that are habits first
habit-down         Put entries that are habits last
alpha-up           Sort headlines alphabetically
alpha-down         Sort headlines alphabetically, reversed

The different possibilities will be tried in sequence, and testing stops
if one comparison returns a \"not-equal\".  For example, the default
    '(time-up category-keep priority-down)
means: Pull out all entries having a specified time of day and sort them,
in order to make a time schedule for the current day the first thing in the
agenda listing for the day.  Of the entries without a time indication, keep
the grouped in categories, don't sort the categories, but keep them in
the sequence given in `org-agenda-files'.  Within each category sort by
priority.

Leaving out `category-keep' would mean that items will be sorted across
categories by priority.

Instead of a single list, this can also be a set of list for specific
contents, with a context symbol in the car of the list, any of
`agenda', `todo', `tags', `search' for the corresponding agenda views.

Custom commands can bind this variable in the options section."
  :group 'org-agenda-sorting
  :type `(choice
	  (repeat :tag "General" ,org-sorting-choice)
	  (list :tag "Individually"
		(cons (const :tag "Strategy for Weekly/Daily agenda" agenda)
		      (repeat ,org-sorting-choice))
		(cons (const :tag "Strategy for TODO lists" todo)
		      (repeat ,org-sorting-choice))
		(cons (const :tag "Strategy for Tags matches" tags)
		      (repeat ,org-sorting-choice))
		(cons (const :tag "Strategy for search matches" search)
		      (repeat ,org-sorting-choice)))))

(defcustom org-agenda-cmp-user-defined nil
  "A function to define the comparison `user-defined'.
This function must receive two arguments, agenda entry a and b.
If a>b, return +1.  If a<b, return -1.  If they are equal as seen by
the user comparison, return nil.
When this is defined, you can make `user-defined-up' and `user-defined-down'
part of an agenda sorting strategy."
  :group 'org-agenda-sorting
  :type 'symbol)

(defcustom org-sort-agenda-notime-is-late t
  "Non-nil means items without time are considered late.
This is only relevant for sorting.  When t, items which have no explicit
time like 15:30 will be considered as 99:01, i.e. later than any items which
do have a time.  When nil, the default time is before 0:00.  You can use this
option to decide if the schedule for today should come before or after timeless
agenda entries."
  :group 'org-agenda-sorting
  :type 'boolean)

(defcustom org-sort-agenda-noeffort-is-high t
  "Non-nil means items without effort estimate are sorted as high effort.
This also applies when filtering an agenda view with respect to the
< or > effort operator.  Then, tasks with no effort defined will be treated
as tasks with high effort.
When nil, such items are sorted as 0 minutes effort."
  :group 'org-agenda-sorting
  :type 'boolean)

(defgroup org-agenda-line-format nil
  "Options concerning the entry prefix in the Org agenda display."
  :tag "Org Agenda Line Format"
  :group 'org-agenda)

(defcustom org-agenda-prefix-format
  '((agenda  . " %i %-12:c%?-12t% s")
    (todo  . " %i %-12:c")
    (tags  . " %i %-12:c")
    (search . " %i %-12:c"))
  "Format specifications for the prefix of items in the agenda views.
An alist with five entries, each for the different agenda types.  The
keys of the sublists are `agenda', `todo', `search' and `tags'.
The values are format strings.

This format works similar to a printf format, with the following meaning:

  %c   the category of the item, \"Diary\" for entries from the diary,
       or as given by the CATEGORY keyword or derived from the file name
  %e   the effort required by the item
  %l   the level of the item (insert X space(s) if item is of level X)
  %i   the icon category of the item, see `org-agenda-category-icon-alist'
  %T   the last tag of the item (ignore inherited tags, which come first)
  %t   the HH:MM time-of-day specification if one applies to the entry
  %s   Scheduling/Deadline information, a short string
  %b   show breadcrumbs, i.e., the names of the higher levels
  %(expression) Eval EXPRESSION and replace the control string
                by the result

All specifiers work basically like the standard `%s' of printf, but may
contain two additional characters: a question mark just after the `%'
and a whitespace/punctuation character just before the final letter.

If the first character after `%' is a question mark, the entire field
will only be included if the corresponding value applies to the current
entry.  This is useful for fields which should have fixed width when
present, but zero width when absent.  For example, \"%?-12t\" will
result in a 12 character time field if a time of the day is specified,
but will completely disappear in entries which do not contain a time.

If there is punctuation or whitespace character just before the
final format letter, this character will be appended to the field
value if the value is not empty.  For example, the format
\"%-12:c\" leads to \"Diary: \" if the category is \"Diary\".  If
the category is empty, no additional colon is inserted.

The default value for the agenda sublist is \"  %-12:c%?-12t% s\",
which means:

- Indent the line with two space characters
- Give the category a 12 chars wide field, padded with whitespace on
  the right (because of `-').  Append a colon if there is a category
  (because of `:').
- If there is a time-of-day, put it into a 12 chars wide field.  If no
  time, don't put in an empty field, just skip it (because of '?').
- Finally, put the scheduling information.

See also the variables `org-agenda-remove-times-when-in-prefix' and
`org-agenda-remove-tags'.

Custom commands can set this variable in the options section."
  :type '(choice
	  (string :tag "General format")
	  (list :greedy t :tag "View dependent"
		(cons  (const agenda) (string :tag "Format"))
		(cons  (const todo) (string :tag "Format"))
		(cons  (const tags) (string :tag "Format"))
		(cons  (const search) (string :tag "Format"))))
  :group 'org-agenda-line-format
  :version "26.1"
  :package-version '(Org . "9.1"))

(defvar org-prefix-format-compiled nil
  "The compiled prefix format and associated variables.
This is a list where first element is a list of variable bindings, and second
element is the compiled format expression.  See the variable
`org-agenda-prefix-format'.")

(defcustom org-agenda-todo-keyword-format "%-1s"
  "Format for the TODO keyword in agenda lines.
Set this to something like \"%-12s\" if you want all TODO keywords
to occupy a fixed space in the agenda display."
  :group 'org-agenda-line-format
  :type 'string)

(defcustom org-agenda-diary-sexp-prefix nil
  "A regexp that matches part of a diary sexp entry
which should be treated as scheduling/deadline information in
`org-agenda'.

For example, you can use this to extract the `diary-remind-message' from
`diary-remind' entries."
  :group 'org-agenda-line-format
  :type '(choice (const :tag "None" nil) (regexp :tag "Regexp")))

(defcustom org-agenda-timerange-leaders '("" "(%d/%d): ")
  "Text preceding timerange entries in the agenda view.
This is a list with two strings.  The first applies when the range
is entirely on one day.  The second applies if the range spans several days.
The strings may have two \"%d\" format specifiers which will be filled
with the sequence number of the days, and the total number of days in the
range, respectively."
  :group 'org-agenda-line-format
  :type '(list
	  (string :tag "Deadline today   ")
	  (choice :tag "Deadline relative"
		  (string :tag "Format string")
		  (function))))

(defcustom org-agenda-scheduled-leaders '("Scheduled: " "Sched.%2dx: ")
  "Text preceding scheduled items in the agenda view.
This is a list with two strings.  The first applies when the item is
scheduled on the current day.  The second applies when it has been scheduled
previously, it may contain a %d indicating that this is the nth time that
this item is scheduled, due to automatic rescheduling of unfinished items
for the following day.  So this number is one larger than the number of days
that passed since this item was scheduled first."
  :group 'org-agenda-line-format
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(list
	  (string :tag "Scheduled today     ")
	  (string :tag "Scheduled previously")))

(defcustom org-agenda-inactive-leader "["
  "Text preceding item pulled into the agenda by inactive time stamps.
These entries are added to the agenda when pressing \"[\"."
  :group 'org-agenda-line-format
  :version "24.1"
  :type 'string)

(defcustom org-agenda-deadline-leaders '("Deadline:  " "In %3d d.: " "%2d d. ago: ")
  "Text preceding deadline items in the agenda view.
This is a list with three strings.  The first applies when the item has its
deadline on the current day.  The second applies when the deadline is in the
future, the third one when it is in the past.  The strings may contain %d
to capture the number of days."
  :group 'org-agenda-line-format
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(list
	  (string :tag "Deadline today          ")
	  (string :tag "Deadline in the future  ")
	  (string :tag "Deadline in the past    ")))

(defcustom org-agenda-remove-times-when-in-prefix t
  "Non-nil means remove duplicate time specifications in agenda items.
When the format `org-agenda-prefix-format' contains a `%t' specifier, a
time-of-day specification in a headline or diary entry is extracted and
placed into the prefix.  If this option is non-nil, the original specification
\(a timestamp or -range, or just a plain time(range) specification like
11:30-4pm) will be removed for agenda display.  This makes the agenda less
cluttered.
The option can be t or nil.  It may also be the symbol `beg', indicating
that the time should only be removed when it is located at the beginning of
the headline/diary entry."
  :group 'org-agenda-line-format
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (const :tag "When at beginning of entry" beg)))

(defcustom org-agenda-remove-timeranges-from-blocks nil
  "Non-nil means remove time ranges specifications in agenda
items that span on several days."
  :group 'org-agenda-line-format
  :version "24.1"
  :type 'boolean)

(defcustom org-agenda-default-appointment-duration nil
  "Default duration for appointments that only have a starting time.
When nil, no duration is specified in such cases.
When non-nil, this must be the number of minutes, e.g. 60 for one hour."
  :group 'org-agenda-line-format
  :type '(choice
	  (integer :tag "Minutes")
	  (const :tag "No default duration")))

(defcustom org-agenda-show-inherited-tags t
  "Non-nil means show inherited tags in each agenda line.

When this option is set to `always', it takes precedence over
`org-agenda-use-tag-inheritance' and inherited tags are shown
in every agenda.

When this option is set to t (the default), inherited tags are
shown when they are available, i.e. when the value of
`org-agenda-use-tag-inheritance' enables tag inheritance for the
given agenda type.

This can be set to a list of agenda types in which the agenda
must display the inherited tags.  Available types are `todo',
`agenda' and `search'.

When set to nil, never show inherited tags in agenda lines."
  :group 'org-agenda-line-format
  :group 'org-agenda
  :version "24.3"
  :type '(choice
	  (const :tag "Show inherited tags when available" t)
	  (const :tag "Always show inherited tags" always)
	  (repeat :tag "Show inherited tags only in selected agenda types"
		  (symbol :tag "Agenda type"))))

(defcustom org-agenda-use-tag-inheritance '(todo search agenda)
  "List of agenda view types where to use tag inheritance.

In tags/tags-todo/tags-tree agenda views, tag inheritance is
controlled by `org-use-tag-inheritance'.  In other agenda types,
`org-use-tag-inheritance' is not used for the selection of the
agenda entries.  Still, you may want the agenda to be aware of
the inherited tags anyway, e.g. for later tag filtering.

Allowed value are `todo', `search' and `agenda'.

This variable has no effect if `org-agenda-show-inherited-tags'
is set to `always'.  In that case, the agenda is aware of those
tags.

The default value sets tags in every agenda type.  Setting this
option to nil will speed up non-tags agenda view a lot."
  :group 'org-agenda
  :version "26.1"
  :package-version '(Org . "9.1")
  :type '(choice
	  (const :tag "Use tag inheritance in all agenda types" t)
	  (repeat :tag "Use tag inheritance in selected agenda types"
		  (symbol :tag "Agenda type"))))

(defcustom org-agenda-hide-tags-regexp nil
  "Regular expression used to filter away specific tags in agenda views.
This means that these tags will be present, but not be shown in the agenda
line.  Secondary filtering will still work on the hidden tags.
Nil means don't hide any tags."
  :group 'org-agenda-line-format
  :type '(choice
	  (const  :tag "Hide none" nil)
	  (string :tag "Regexp   ")))

(defcustom org-agenda-remove-tags nil
  "Non-nil means remove the tags from the headline copy in the agenda.
When this is the symbol `prefix', only remove tags when
`org-agenda-prefix-format' contains a `%T' specifier."
  :group 'org-agenda-line-format
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (const :tag "When prefix format contains %T" prefix)))

(defvaralias 'org-agenda-remove-tags-when-in-prefix
  'org-agenda-remove-tags)

(defcustom org-agenda-tags-column 'auto
  "Shift tags in agenda items to this column.
If set to `auto', tags will be automatically aligned to the right
edge of the window.

If set to a positive number, tags will be left-aligned to that
column.  If set to a negative number, tags will be right-aligned
to that column.  For example, -80 works well for a normal 80
character screen."
  :group 'org-agenda-line-format
  :type '(choice
	  (const :tag "Automatically align to right edge of window" auto)
	  (integer :tag "Specific column" -80))
  :package-version '(Org . "9.1")
  :version "26.1")

(defvaralias 'org-agenda-align-tags-to-column 'org-agenda-tags-column)

(defcustom org-agenda-fontify-priorities 'cookies
  "Non-nil means highlight low and high priorities in agenda.
When t, the highest priority entries are bold, lowest priority italic.
However, settings in `org-priority-faces' will overrule these faces.
When this variable is the symbol `cookies', only fontify the
cookies, not the entire task.
This may also be an association list of priority faces, whose
keys are the character values of `org-highest-priority',
`org-default-priority', and `org-lowest-priority' (the default values
are ?A, ?B, and ?C, respectively).  The face may be a named face, a
color as a string, or a list like `(:background \"Red\")'.
If it is a color, the variable `org-faces-easy-properties'
determines if it is a foreground or a background color."
  :group 'org-agenda-line-format
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Defaults" t)
	  (const :tag "Cookies only" cookies)
	  (repeat :tag "Specify"
		  (list (character :tag "Priority" :value ?A)
			(choice    :tag "Face    "
				   (string :tag "Color")
				   (sexp :tag "Face"))))))

(defcustom org-agenda-day-face-function nil
  "Function called to determine what face should be used to display a day.
The only argument passed to that function is the day.  It should
returns a face, or nil if does not want to specify a face and let
the normal rules apply."
  :group 'org-agenda-line-format
  :version "24.1"
  :type '(choice (const nil) (function)))

(defcustom org-agenda-category-icon-alist nil
  "Alist of category icon to be displayed in agenda views.

Each entry should have the following format:

  (CATEGORY-REGEXP FILE-OR-DATA TYPE DATA-P PROPS)

Where CATEGORY-REGEXP is a regexp matching the categories where
the icon should be displayed.
FILE-OR-DATA either a file path or a string containing image data.

The other fields can be omitted safely if not needed:
TYPE indicates the image type.
DATA-P is a boolean indicating whether the FILE-OR-DATA string is
image data.
PROPS are additional image attributes to assign to the image,
like, e.g. `:ascent center'.

   (\"Org\" \"/path/to/icon.png\" nil nil :ascent center)

If you want to set the display properties yourself, just put a
list as second element:

  (CATEGORY-REGEXP (MY PROPERTY LIST))

For example, to display a 16px horizontal space for Emacs
category, you can use:

  (\"Emacs\" \\='(space . (:width (16))))"
  :group 'org-agenda-line-format
  :version "24.1"
  :type '(alist :key-type (string :tag "Regexp matching category")
		:value-type (choice (list :tag "Icon"
					  (string :tag "File or data")
					  (symbol :tag "Type")
					  (boolean :tag "Data?")
					  (repeat :tag "Extra image properties" :inline t symbol))
				    (list :tag "Display properties" sexp))))

(defgroup org-agenda-column-view nil
  "Options concerning column view in the agenda."
  :tag "Org Agenda Column View"
  :group 'org-agenda)

(defcustom org-agenda-view-columns-initially nil
  "When non-nil, switch to columns view right after creating the agenda."
  :group 'org-agenda-column-view
  :type 'boolean
  :version "26.1"
  :package-version '(Org . "9.0")
  :safe #'booleanp)

(defcustom org-agenda-columns-show-summaries t
  "Non-nil means show summaries for columns displayed in the agenda view."
  :group 'org-agenda-column-view
  :type 'boolean)

(defcustom org-agenda-columns-compute-summary-properties t
  "Non-nil means recompute all summary properties before column view.
When column view in the agenda is listing properties that have a summary
operator, it can go to all relevant buffers and recompute the summaries
there.  This can mean overhead for the agenda column view, but is necessary
to have thing up to date.
As a special case, a CLOCKSUM property also makes sure that the clock
computations are current."
  :group 'org-agenda-column-view
  :type 'boolean)

(defcustom org-agenda-columns-add-appointments-to-effort-sum nil
  "Non-nil means the duration of an appointment will add to day effort.
The property to which appointment durations will be added is the one given
in the option `org-effort-property'.  If an appointment does not have
an end time, `org-agenda-default-appointment-duration' will be used.  If that
is not set, an appointment without end time will not contribute to the time
estimate."
  :group 'org-agenda-column-view
  :type 'boolean)

(defcustom org-agenda-auto-exclude-function nil
  "A function called with a tag to decide if it is filtered on \
\\<org-agenda-mode-map>`\\[org-agenda-filter-by-tag] RET'.
The sole argument to the function, which is called once for each
possible tag, is a string giving the name of the tag.  The
function should return either nil if the tag should be included
as normal, or \"-<TAG>\" to exclude the tag.
Note that for the purpose of tag filtering, only the lower-case version of
all tags will be considered, so that this function will only ever see
the lower-case version of all tags."
  :group 'org-agenda
  :type '(choice (const nil) (function)))

(defcustom org-agenda-bulk-custom-functions nil
  "Alist of characters and custom functions for bulk actions.
For example, this value makes those two functions available:

  \\='((?R set-category)
    (?C bulk-cut))

With selected entries in an agenda buffer, `B R' will call
the custom function `set-category' on the selected entries.
Note that functions in this alist don't need to be quoted."
  :type '(alist :key-type character :value-type (group function))
  :version "24.1"
  :group 'org-agenda)

(defmacro org-agenda-with-point-at-orig-entry (string &rest body)
  "Execute BODY with point at location given by `org-hd-marker' property.
If STRING is non-nil, the text property will be fetched from position 0
in that string.  If STRING is nil, it will be fetched from the beginning
of the current line."
  (org-with-gensyms (marker)
    `(let ((,marker (get-text-property (if ,string 0 (point-at-bol))
				       'org-hd-marker ,string)))
       (with-current-buffer (marker-buffer ,marker)
	 (save-excursion
	   (goto-char ,marker)
	   ,@body)))))
(def-edebug-spec org-agenda-with-point-at-orig-entry (form body))

(defun org-add-agenda-custom-command (entry)
  "Replace or add a command in `org-agenda-custom-commands'.
This is mostly for hacking and trying a new command - once the command
works you probably want to add it to `org-agenda-custom-commands' for good."
  (let ((ass (assoc (car entry) org-agenda-custom-commands)))
    (if ass
	(setcdr ass (cdr entry))
      (push entry org-agenda-custom-commands))))

;;; Define the org-agenda-mode

(defvar org-agenda-mode-map (make-sparse-keymap)
  "Keymap for `org-agenda-mode'.")
(defvaralias 'org-agenda-keymap 'org-agenda-mode-map)

(defvar org-agenda-menu) ; defined later in this file.
(defvar org-agenda-restrict nil) ; defined later in this file.
(defvar org-agenda-follow-mode nil)
(defvar org-agenda-entry-text-mode nil)
(defvar org-agenda-clockreport-mode nil)
(defvar org-agenda-show-log nil)
(defvar org-agenda-redo-command nil)
(defvar org-agenda-query-string nil)
(defvar org-agenda-mode-hook nil
  "Hook run after `org-agenda-mode' is turned on.
The buffer is still writable when this hook is called.")
(defvar org-agenda-type nil)
(defvar org-agenda-force-single-file nil)
(defvar org-agenda-bulk-marked-entries nil
  "List of markers that refer to marked entries in the agenda.")
(defvar org-agenda-current-date nil
  "Active date when building the agenda.")

;;; Multiple agenda buffers support

(defcustom org-agenda-sticky nil
  "Non-nil means agenda q key will bury agenda buffers.
Agenda commands will then show existing buffer instead of generating new ones.
When nil, `q' will kill the single agenda buffer."
  :group 'org-agenda
  :version "24.3"
  :type 'boolean)


;;;###autoload
(defun org-toggle-sticky-agenda (&optional arg)
  "Toggle `org-agenda-sticky'."
  (interactive "P")
  (let ((new-value (if arg
		       (> (prefix-numeric-value arg) 0)
		     (not org-agenda-sticky))))
    (if (equal new-value org-agenda-sticky)
	(and (called-interactively-p 'interactive)
	     (message "Sticky agenda was already %s"
		      (if org-agenda-sticky "enabled" "disabled")))
      (setq org-agenda-sticky new-value)
      (org-agenda-kill-all-agenda-buffers)
      (and (called-interactively-p 'interactive)
	   (message "Sticky agenda %s"
		    (if org-agenda-sticky "enabled" "disabled"))))))

(defvar org-agenda-buffer nil
  "Agenda buffer currently being generated.")

(defvar org-agenda-last-prefix-arg nil)
(defvar org-agenda-this-buffer-name nil)
(defvar org-agenda-doing-sticky-redo nil)
(defvar org-agenda-this-buffer-is-sticky nil)
(defvar org-agenda-last-indirect-buffer nil
  "Last buffer loaded by `org-agenda-tree-to-indirect-buffer'.")

(defconst org-agenda-local-vars
  '(org-agenda-this-buffer-name
    org-agenda-undo-list
    org-agenda-pending-undo-list
    org-agenda-follow-mode
    org-agenda-entry-text-mode
    org-agenda-clockreport-mode
    org-agenda-show-log
    org-agenda-redo-command
    org-agenda-query-string
    org-agenda-type
    org-agenda-bulk-marked-entries
    org-agenda-undo-has-started-in
    org-agenda-info
    org-agenda-pre-window-conf
    org-agenda-columns-active
    org-agenda-tag-filter
    org-agenda-category-filter
    org-agenda-top-headline-filter
    org-agenda-regexp-filter
    org-agenda-effort-filter
    org-agenda-markers
    org-agenda-last-search-view-search-was-boolean
    org-agenda-last-indirect-buffer
    org-agenda-filtered-by-category
    org-agenda-filter-form
    org-agenda-cycle-counter
    org-agenda-last-prefix-arg)
  "Variables that must be local in agenda buffers to allow multiple buffers.")

(defun org-agenda-mode ()
  "Mode for time-sorted view on action items in Org files.

The following commands are available:

\\{org-agenda-mode-map}"
  (interactive)
  (cond (org-agenda-doing-sticky-redo
	 ;; Refreshing sticky agenda-buffer
	 ;;
	 ;; Preserve the value of `org-agenda-local-vars' variables,
	 ;; while letting `kill-all-local-variables' kill the rest
	 (let ((save (buffer-local-variables)))
	   (kill-all-local-variables)
	   (mapc #'make-local-variable org-agenda-local-vars)
	   (dolist (elem save)
	     (pcase elem
	       (`(,var . ,val)		;ignore unbound variables
		(when (and val (memq var org-agenda-local-vars))
		  (set var val))))))
	 (setq-local org-agenda-this-buffer-is-sticky t))
	(org-agenda-sticky
	 ;; Creating a sticky Agenda buffer for the first time
	 (kill-all-local-variables)
	 (mapc 'make-local-variable org-agenda-local-vars)
	 (setq-local org-agenda-this-buffer-is-sticky t))
	(t
	 ;; Creating a non-sticky agenda buffer
	 (kill-all-local-variables)
	 (setq-local org-agenda-this-buffer-is-sticky nil)))
  (setq org-agenda-undo-list nil
	org-agenda-pending-undo-list nil
	org-agenda-bulk-marked-entries nil)
  (setq major-mode 'org-agenda-mode)
  ;; Keep global-font-lock-mode from turning on font-lock-mode
  (setq-local font-lock-global-modes (list 'not major-mode))
  (setq mode-name "Org-Agenda")
  (setq indent-tabs-mode nil)
  (use-local-map org-agenda-mode-map)
  (easy-menu-add org-agenda-menu)
  (if org-startup-truncated (setq truncate-lines t))
  (setq-local line-move-visual nil)
  (add-hook 'post-command-hook 'org-agenda-update-agenda-type nil 'local)
  (add-hook 'pre-command-hook 'org-unhighlight nil 'local)
  ;; Make sure properties are removed when copying text
  (add-hook 'filter-buffer-substring-functions
	    (lambda (fun start end delete)
	      (substring-no-properties (funcall fun start end delete)))
	    nil t)
  (unless org-agenda-keep-modes
    (setq org-agenda-follow-mode org-agenda-start-with-follow-mode
	  org-agenda-entry-text-mode org-agenda-start-with-entry-text-mode))
  (setq org-agenda-show-log org-agenda-start-with-log-mode)
  (setq org-agenda-clockreport-mode org-agenda-start-with-clockreport-mode)
  (add-to-invisibility-spec '(org-filtered))
  (add-to-invisibility-spec '(org-link))
  (easy-menu-change
   '("Agenda") "Agenda Files"
   (append
    (list
     (vector
      (if (get 'org-agenda-files 'org-restrict)
	  "Restricted to single file"
	"Edit File List")
      '(org-edit-agenda-file-list)
      (not (get 'org-agenda-files 'org-restrict)))
     "--")
    (mapcar 'org-file-menu-entry (org-agenda-files))))
  (org-agenda-set-mode-name)
  (apply
   (if (fboundp 'run-mode-hooks) 'run-mode-hooks 'run-hooks)
   (list 'org-agenda-mode-hook)))

(substitute-key-definition 'undo 'org-agenda-undo
			   org-agenda-mode-map global-map)
(org-defkey org-agenda-mode-map "\C-i"     'org-agenda-goto)
(org-defkey org-agenda-mode-map [(tab)]    'org-agenda-goto)
(org-defkey org-agenda-mode-map "\C-m"     'org-agenda-switch-to)
(org-defkey org-agenda-mode-map "\C-k"     'org-agenda-kill)
(org-defkey org-agenda-mode-map "\C-c\C-w" 'org-agenda-refile)
(org-defkey org-agenda-mode-map [(meta down)] 'org-agenda-drag-line-forward)
(org-defkey org-agenda-mode-map [(meta up)] 'org-agenda-drag-line-backward)
(org-defkey org-agenda-mode-map "m"        'org-agenda-bulk-mark)
(org-defkey org-agenda-mode-map "\M-m"     'org-agenda-bulk-toggle)
(org-defkey org-agenda-mode-map "*"        'org-agenda-bulk-mark-all)
(org-defkey org-agenda-mode-map "\M-*"     'org-agenda-bulk-toggle-all)
(org-defkey org-agenda-mode-map "#"        'org-agenda-dim-blocked-tasks)
(org-defkey org-agenda-mode-map "%"        'org-agenda-bulk-mark-regexp)
(org-defkey org-agenda-mode-map "u"        'org-agenda-bulk-unmark)
(org-defkey org-agenda-mode-map "U"        'org-agenda-bulk-unmark-all)
(org-defkey org-agenda-mode-map "B"        'org-agenda-bulk-action)
(org-defkey org-agenda-mode-map "k"        'org-agenda-capture)
(org-defkey org-agenda-mode-map "A"        'org-agenda-append-agenda)
(org-defkey org-agenda-mode-map "\C-c\C-x!" 'org-reload)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-a" 'org-agenda-archive-default)
(org-defkey org-agenda-mode-map "\C-c\C-xa"    'org-agenda-toggle-archive-tag)
(org-defkey org-agenda-mode-map "\C-c\C-xA"    'org-agenda-archive-to-archive-sibling)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-s" 'org-agenda-archive)
(org-defkey org-agenda-mode-map "\C-c$"        'org-agenda-archive)
(org-defkey org-agenda-mode-map "$"        'org-agenda-archive)
(org-defkey org-agenda-mode-map "\C-c\C-o" 'org-agenda-open-link)
(org-defkey org-agenda-mode-map " "        'org-agenda-show-and-scroll-up)
(org-defkey org-agenda-mode-map [backspace] 'org-agenda-show-scroll-down)
(org-defkey org-agenda-mode-map "\d" 'org-agenda-show-scroll-down)
(org-defkey org-agenda-mode-map [(control shift right)] 'org-agenda-todo-nextset)
(org-defkey org-agenda-mode-map [(control shift left)]  'org-agenda-todo-previousset)
(org-defkey org-agenda-mode-map "\C-c\C-xb" 'org-agenda-tree-to-indirect-buffer)
(org-defkey org-agenda-mode-map "o"        'delete-other-windows)
(org-defkey org-agenda-mode-map "L"        'org-agenda-recenter)
(org-defkey org-agenda-mode-map "\C-c\C-t" 'org-agenda-todo)
(org-defkey org-agenda-mode-map "t"        'org-agenda-todo)
(org-defkey org-agenda-mode-map "a"        'org-agenda-archive-default-with-confirmation)
(org-defkey org-agenda-mode-map ":"        'org-agenda-set-tags)
(org-defkey org-agenda-mode-map "\C-c\C-q" 'org-agenda-set-tags)
(org-defkey org-agenda-mode-map "."        'org-agenda-goto-today)
(org-defkey org-agenda-mode-map "j"        'org-agenda-goto-date)
(org-defkey org-agenda-mode-map "d"        'org-agenda-day-view)
(org-defkey org-agenda-mode-map "w"        'org-agenda-week-view)
(org-defkey org-agenda-mode-map "y"        'org-agenda-year-view)
(org-defkey org-agenda-mode-map "\C-c\C-z" 'org-agenda-add-note)
(org-defkey org-agenda-mode-map "z"        'org-agenda-add-note)
(org-defkey org-agenda-mode-map [(shift right)] 'org-agenda-do-date-later)
(org-defkey org-agenda-mode-map [(shift left)] 'org-agenda-do-date-earlier)
(org-defkey org-agenda-mode-map [?\C-c ?\C-x (right)] 'org-agenda-do-date-later)
(org-defkey org-agenda-mode-map [?\C-c ?\C-x (left)] 'org-agenda-do-date-earlier)

(org-defkey org-agenda-mode-map ">" 'org-agenda-date-prompt)
(org-defkey org-agenda-mode-map "\C-c\C-s" 'org-agenda-schedule)
(org-defkey org-agenda-mode-map "\C-c\C-d" 'org-agenda-deadline)
(let ((l '(1 2 3 4 5 6 7 8 9 0)))
  (while l (org-defkey org-agenda-mode-map
		       (int-to-string (pop l)) 'digit-argument)))

(org-defkey org-agenda-mode-map "F" 'org-agenda-follow-mode)
(org-defkey org-agenda-mode-map "R" 'org-agenda-clockreport-mode)
(org-defkey org-agenda-mode-map "E" 'org-agenda-entry-text-mode)
(org-defkey org-agenda-mode-map "l" 'org-agenda-log-mode)
(org-defkey org-agenda-mode-map "v" 'org-agenda-view-mode-dispatch)
(org-defkey org-agenda-mode-map "D" 'org-agenda-toggle-diary)
(org-defkey org-agenda-mode-map "!" 'org-agenda-toggle-deadlines)
(org-defkey org-agenda-mode-map "G" 'org-agenda-toggle-time-grid)
(org-defkey org-agenda-mode-map "r" 'org-agenda-redo)
(org-defkey org-agenda-mode-map "g" 'org-agenda-redo-all)
(org-defkey org-agenda-mode-map "e" 'org-agenda-set-effort)
(org-defkey org-agenda-mode-map "\C-c\C-xe" 'org-agenda-set-effort)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-e"
	    'org-clock-modify-effort-estimate)
(org-defkey org-agenda-mode-map "\C-c\C-xp" 'org-agenda-set-property)
(org-defkey org-agenda-mode-map "q" 'org-agenda-quit)
(org-defkey org-agenda-mode-map "Q" 'org-agenda-Quit)
(org-defkey org-agenda-mode-map "x" 'org-agenda-exit)
(org-defkey org-agenda-mode-map "\C-x\C-w" 'org-agenda-write)
(org-defkey org-agenda-mode-map "\C-x\C-s" 'org-save-all-org-buffers)
(org-defkey org-agenda-mode-map "s" 'org-save-all-org-buffers)
(org-defkey org-agenda-mode-map "T" 'org-agenda-show-tags)
(org-defkey org-agenda-mode-map "n" 'org-agenda-next-line)
(org-defkey org-agenda-mode-map "p" 'org-agenda-previous-line)
(org-defkey org-agenda-mode-map "N" 'org-agenda-next-item)
(org-defkey org-agenda-mode-map "P" 'org-agenda-previous-item)
(substitute-key-definition 'next-line 'org-agenda-next-line
			   org-agenda-mode-map global-map)
(substitute-key-definition 'previous-line 'org-agenda-previous-line
			   org-agenda-mode-map global-map)
(org-defkey org-agenda-mode-map "\C-c\C-a" 'org-attach)
(org-defkey org-agenda-mode-map "\C-c\C-n" 'org-agenda-next-date-line)
(org-defkey org-agenda-mode-map "\C-c\C-p" 'org-agenda-previous-date-line)
(org-defkey org-agenda-mode-map "\C-c," 'org-agenda-priority)
(org-defkey org-agenda-mode-map "," 'org-agenda-priority)
(org-defkey org-agenda-mode-map "i" 'org-agenda-diary-entry)
(org-defkey org-agenda-mode-map "c" 'org-agenda-goto-calendar)
(org-defkey org-agenda-mode-map "C" 'org-agenda-convert-date)
(org-defkey org-agenda-mode-map "M" 'org-agenda-phases-of-moon)
(org-defkey org-agenda-mode-map "S" 'org-agenda-sunrise-sunset)
(org-defkey org-agenda-mode-map "h" 'org-agenda-holidays)
(org-defkey org-agenda-mode-map "H" 'org-agenda-holidays)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-i" 'org-agenda-clock-in)
(org-defkey org-agenda-mode-map "I" 'org-agenda-clock-in)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-o" 'org-agenda-clock-out)
(org-defkey org-agenda-mode-map "O" 'org-agenda-clock-out)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-x" 'org-agenda-clock-cancel)
(org-defkey org-agenda-mode-map "X" 'org-agenda-clock-cancel)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-j" 'org-clock-goto)
(org-defkey org-agenda-mode-map "J" 'org-agenda-clock-goto)
(org-defkey org-agenda-mode-map "+" 'org-agenda-priority-up)
(org-defkey org-agenda-mode-map "-" 'org-agenda-priority-down)
(org-defkey org-agenda-mode-map [(shift up)] 'org-agenda-priority-up)
(org-defkey org-agenda-mode-map [(shift down)] 'org-agenda-priority-down)
(org-defkey org-agenda-mode-map [?\C-c ?\C-x (up)] 'org-agenda-priority-up)
(org-defkey org-agenda-mode-map [?\C-c ?\C-x (down)] 'org-agenda-priority-down)
(org-defkey org-agenda-mode-map "f" 'org-agenda-later)
(org-defkey org-agenda-mode-map "b" 'org-agenda-earlier)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-c" 'org-agenda-columns)
(org-defkey org-agenda-mode-map "\C-c\C-x>" 'org-agenda-remove-restriction-lock)
(org-defkey org-agenda-mode-map "\C-c\C-x<" 'org-agenda-set-restriction-lock-from-agenda)

(org-defkey org-agenda-mode-map "[" 'org-agenda-manipulate-query-add)
(org-defkey org-agenda-mode-map "]" 'org-agenda-manipulate-query-subtract)
(org-defkey org-agenda-mode-map "{" 'org-agenda-manipulate-query-add-re)
(org-defkey org-agenda-mode-map "}" 'org-agenda-manipulate-query-subtract-re)
(org-defkey org-agenda-mode-map "/" 'org-agenda-filter-by-tag)
(org-defkey org-agenda-mode-map "_" 'org-agenda-filter-by-effort)
(org-defkey org-agenda-mode-map "=" 'org-agenda-filter-by-regexp)
(org-defkey org-agenda-mode-map "|" 'org-agenda-filter-remove-all)
(org-defkey org-agenda-mode-map "~" 'org-agenda-limit-interactively)
(org-defkey org-agenda-mode-map "<" 'org-agenda-filter-by-category)
(org-defkey org-agenda-mode-map "^" 'org-agenda-filter-by-top-headline)
(org-defkey org-agenda-mode-map ";" 'org-timer-set-timer)
(org-defkey org-agenda-mode-map "\C-c\C-x_" 'org-timer-stop)
(define-key org-agenda-mode-map "?" 'org-agenda-show-the-flagging-note)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-mg"    'org-mobile-pull)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-mp"    'org-mobile-push)

(org-defkey org-agenda-mode-map [mouse-2] 'org-agenda-goto-mouse)
(org-defkey org-agenda-mode-map [mouse-3] 'org-agenda-show-mouse)

(define-key org-agenda-mode-map [remap forward-paragraph] 'org-agenda-forward-block)
(define-key org-agenda-mode-map [remap backward-paragraph] 'org-agenda-backward-block)

(when org-agenda-mouse-1-follows-link
  (org-defkey org-agenda-mode-map [follow-link] 'mouse-face))
(easy-menu-define org-agenda-menu org-agenda-mode-map "Agenda menu"
  '("Agenda"
    ("Agenda Files")
    "--"
    ("Agenda Dates"
     ["Goto Today" org-agenda-goto-today (org-agenda-check-type nil 'agenda)]
     ["Next Dates" org-agenda-later (org-agenda-check-type nil 'agenda)]
     ["Previous Dates" org-agenda-earlier (org-agenda-check-type nil 'agenda)]
     ["Jump to date" org-agenda-goto-date (org-agenda-check-type nil 'agenda)])
    "--"
    ("View"
     ["Day View" org-agenda-day-view
      :active (org-agenda-check-type nil 'agenda)
      :style radio :selected (eq org-agenda-current-span 'day)
      :keys "v d  (or just d)"]
     ["Week View" org-agenda-week-view
      :active (org-agenda-check-type nil 'agenda)
      :style radio :selected (eq org-agenda-current-span 'week)
      :keys "v w"]
     ["Fortnight View" org-agenda-fortnight-view
      :active (org-agenda-check-type nil 'agenda)
      :style radio :selected (eq org-agenda-current-span 'fortnight)
      :keys "v t"]
     ["Month View" org-agenda-month-view
      :active (org-agenda-check-type nil 'agenda)
      :style radio :selected (eq org-agenda-current-span 'month)
      :keys "v m"]
     ["Year View" org-agenda-year-view
      :active (org-agenda-check-type nil 'agenda)
      :style radio :selected (eq org-agenda-current-span 'year)
      :keys "v y"]
     "--"
     ["Include Diary" org-agenda-toggle-diary
      :style toggle :selected org-agenda-include-diary
      :active (org-agenda-check-type nil 'agenda)]
     ["Include Deadlines" org-agenda-toggle-deadlines
      :style toggle :selected org-agenda-include-deadlines
      :active (org-agenda-check-type nil 'agenda)]
     ["Use Time Grid" org-agenda-toggle-time-grid
      :style toggle :selected org-agenda-use-time-grid
      :active (org-agenda-check-type nil 'agenda)]
     "--"
     ["Show clock report" org-agenda-clockreport-mode
      :style toggle :selected org-agenda-clockreport-mode
      :active (org-agenda-check-type nil 'agenda)]
     ["Show some entry text" org-agenda-entry-text-mode
      :style toggle :selected org-agenda-entry-text-mode
      :active t]
     "--"
     ["Show Logbook entries" org-agenda-log-mode
      :style toggle :selected org-agenda-show-log
      :active (org-agenda-check-type nil 'agenda)
      :keys "v l (or just l)"]
     ["Include archived trees" org-agenda-archives-mode
      :style toggle :selected org-agenda-archives-mode :active t
      :keys "v a"]
     ["Include archive files" (org-agenda-archives-mode t)
      :style toggle :selected (eq org-agenda-archives-mode t) :active t
      :keys "v A"]
     "--"
     ["Remove Restriction" org-agenda-remove-restriction-lock org-agenda-restrict])
    ["Write view to file" org-agenda-write t]
    ["Rebuild buffer" org-agenda-redo t]
    ["Save all Org buffers" org-save-all-org-buffers t]
    "--"
    ["Show original entry" org-agenda-show t]
    ["Go To (other window)" org-agenda-goto t]
    ["Go To (this window)" org-agenda-switch-to t]
    ["Capture with cursor date" org-agenda-capture t]
    ["Follow Mode" org-agenda-follow-mode
     :style toggle :selected org-agenda-follow-mode :active t]
    ;;    ["Tree to indirect frame" org-agenda-tree-to-indirect-buffer t]
    "--"
    ("TODO"
     ["Cycle TODO" org-agenda-todo t]
     ["Next TODO set" org-agenda-todo-nextset t]
     ["Previous TODO set" org-agenda-todo-previousset t]
     ["Add note" org-agenda-add-note t])
    ("Archive/Refile/Delete"
     ["Archive default" org-agenda-archive-default t]
     ["Archive default" org-agenda-archive-default-with-confirmation t]
     ["Toggle ARCHIVE tag" org-agenda-toggle-archive-tag t]
     ["Move to archive sibling" org-agenda-archive-to-archive-sibling t]
     ["Archive subtree" org-agenda-archive t]
     "--"
     ["Refile" org-agenda-refile t]
     "--"
     ["Delete subtree" org-agenda-kill t])
    ("Bulk action"
     ["Mark entry" org-agenda-bulk-mark t]
     ["Mark all" org-agenda-bulk-mark-all t]
     ["Unmark entry" org-agenda-bulk-unmark t]
     ["Unmark all" org-agenda-bulk-unmark-all :active t :keys "U"]
     ["Toggle mark" org-agenda-bulk-toggle t]
     ["Toggle all" org-agenda-bulk-toggle-all t]
     ["Mark regexp" org-agenda-bulk-mark-regexp t])
    ["Act on all marked" org-agenda-bulk-action t]
    "--"
    ("Tags and Properties"
     ["Show all Tags" org-agenda-show-tags t]
     ["Set Tags current line" org-agenda-set-tags (not (org-region-active-p))]
     ["Change tag in region" org-agenda-set-tags (org-region-active-p)]
     "--"
     ["Column View" org-columns t])
    ("Deadline/Schedule"
     ["Schedule" org-agenda-schedule t]
     ["Set Deadline" org-agenda-deadline t]
     "--"
     ["Change Date +1 day" org-agenda-date-later (org-agenda-check-type nil 'agenda)]
     ["Change Date -1 day" org-agenda-date-earlier (org-agenda-check-type nil 'agenda)]
     ["Change Time +1 hour" org-agenda-do-date-later :active (org-agenda-check-type nil 'agenda) :keys "C-u S-right"]
     ["Change Time -1 hour" org-agenda-do-date-earlier :active (org-agenda-check-type nil 'agenda) :keys "C-u S-left"]
     ["Change Time +  min" org-agenda-date-later :active (org-agenda-check-type nil 'agenda) :keys "C-u C-u S-right"]
     ["Change Time -  min" org-agenda-date-earlier :active (org-agenda-check-type nil 'agenda) :keys "C-u C-u S-left"]
     ["Change Date to ..." org-agenda-date-prompt (org-agenda-check-type nil 'agenda)])
    ("Clock and Effort"
     ["Clock in" org-agenda-clock-in t]
     ["Clock out" org-agenda-clock-out t]
     ["Clock cancel" org-agenda-clock-cancel t]
     ["Goto running clock" org-clock-goto t]
     "--"
     ["Set Effort" org-agenda-set-effort t]
     ["Change clocked effort" org-clock-modify-effort-estimate
      (org-clock-is-active)])
    ("Priority"
     ["Set Priority" org-agenda-priority t]
     ["Increase Priority" org-agenda-priority-up t]
     ["Decrease Priority" org-agenda-priority-down t]
     ["Show Priority" org-show-priority t])
    ("Calendar/Diary"
     ["New Diary Entry" org-agenda-diary-entry (org-agenda-check-type nil 'agenda)]
     ["Goto Calendar" org-agenda-goto-calendar (org-agenda-check-type nil 'agenda)]
     ["Phases of the Moon" org-agenda-phases-of-moon (org-agenda-check-type nil 'agenda)]
     ["Sunrise/Sunset" org-agenda-sunrise-sunset (org-agenda-check-type nil 'agenda)]
     ["Holidays" org-agenda-holidays (org-agenda-check-type nil 'agenda)]
     ["Convert" org-agenda-convert-date (org-agenda-check-type nil 'agenda)]
     "--"
     ["Create iCalendar File" org-icalendar-combine-agenda-files t])
    "--"
    ["Undo Remote Editing" org-agenda-undo org-agenda-undo-list]
    "--"
    ("MobileOrg"
     ["Push Files and Views" org-mobile-push t]
     ["Get Captured and Flagged" org-mobile-pull t]
     ["Find FLAGGED Tasks" (org-agenda nil "?") :active t :keys "C-c a ?"]
     ["Show note / unflag" org-agenda-show-the-flagging-note t]
     "--"
     ["Setup" (progn (require 'org-mobile) (customize-group 'org-mobile)) t])
    "--"
    ["Quit" org-agenda-quit t]
    ["Exit and Release Buffers" org-agenda-exit t]
    ))

;;; Agenda undo

(defvar org-agenda-allow-remote-undo t
  "Non-nil means allow remote undo from the agenda buffer.")
(defvar org-agenda-undo-has-started-in nil
  "Buffers that have already seen `undo-start' in the current undo sequence.")

(defun org-agenda-undo ()
  "Undo a remote editing step in the agenda.
This undoes changes both in the agenda buffer and in the remote buffer
that have been changed along."
  (interactive)
  (or org-agenda-allow-remote-undo
      (user-error "Check the variable `org-agenda-allow-remote-undo' to activate remote undo"))
  (if (not (eq this-command last-command))
      (setq org-agenda-undo-has-started-in nil
	    org-agenda-pending-undo-list org-agenda-undo-list))
  (if (not org-agenda-pending-undo-list)
      (user-error "No further undo information"))
  (let* ((entry (pop org-agenda-pending-undo-list))
	 buf line cmd rembuf)
    (setq cmd (pop entry) line (pop entry))
    (setq rembuf (nth 2 entry))
    (org-with-remote-undo rembuf
      (while (bufferp (setq buf (pop entry)))
	(if (pop entry)
	    (with-current-buffer buf
	      (let ((last-undo-buffer buf)
                    (inhibit-read-only t))
		(unless (memq buf org-agenda-undo-has-started-in)
		  (push buf org-agenda-undo-has-started-in)
		  (make-local-variable 'pending-undo-list)
		  (undo-start))
		(while (and pending-undo-list
			    (listp pending-undo-list)
			    (not (car pending-undo-list)))
		  (pop pending-undo-list))
		(undo-more 1))))))
    (org-goto-line line)
    (message "`%s' undone (buffer %s)" cmd (buffer-name rembuf))))

(defun org-verify-change-for-undo (l1 l2)
  "Verify that a real change occurred between the undo lists L1 and L2."
  (while (and l1 (listp l1) (null (car l1))) (pop l1))
  (while (and l2 (listp l2) (null (car l2))) (pop l2))
  (not (eq l1 l2)))

;;; Agenda dispatch

(defvar org-agenda-restrict-begin (make-marker))
(defvar org-agenda-restrict-end (make-marker))
(defvar org-agenda-last-dispatch-buffer nil)
(defvar org-agenda-overriding-restriction nil)

(defcustom org-agenda-custom-commands-contexts nil
  "Alist of custom agenda keys and contextual rules.

For example, if you have a custom agenda command \"p\" and you
want this command to be accessible only from plain text files,
use this:

   \\='((\"p\" ((in-file . \"\\\\.txt\\\\'\"))))

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

    \\='((\"p\" \"q\" ((in-file . \"\\\\.txt\\\\'\"))))

Here it means: in .txt files, use \"p\" as the key for the
agenda command otherwise associated with \"q\".  (The command
originally associated with \"q\" is not displayed to avoid
duplicates.)"
  :version "24.3"
  :group 'org-agenda-custom-commands
  :type '(repeat (list :tag "Rule"
		       (string :tag "        Agenda key")
		       (string :tag "Replace by command")
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

(defcustom org-agenda-max-entries nil
  "Maximum number of entries to display in an agenda.
This can be nil (no limit) or an integer or an alist of agenda
types with an associated number of entries to display in this
type."
  :version "24.4"
  :package-version '(Org . "8.0")
  :group 'org-agenda-custom-commands
  :type '(choice (symbol :tag "No limit" nil)
		 (integer :tag "Max number of entries")
		 (repeat
		  (cons (choice :tag "Agenda type"
				(const agenda)
				(const todo)
				(const tags)
				(const search))
			(integer :tag "Max number of entries")))))

(defcustom org-agenda-max-todos nil
  "Maximum number of TODOs to display in an agenda.
This can be nil (no limit) or an integer or an alist of agenda
types with an associated number of entries to display in this
type."
  :version "24.4"
  :package-version '(Org . "8.0")
  :group 'org-agenda-custom-commands
  :type '(choice (symbol :tag "No limit" nil)
		 (integer :tag "Max number of TODOs")
		 (repeat
		  (cons (choice :tag "Agenda type"
				(const agenda)
				(const todo)
				(const tags)
				(const search))
			(integer :tag "Max number of TODOs")))))

(defcustom org-agenda-max-tags nil
  "Maximum number of tagged entries to display in an agenda.
This can be nil (no limit) or an integer or an alist of agenda
types with an associated number of entries to display in this
type."
  :version "24.4"
  :package-version '(Org . "8.0")
  :group 'org-agenda-custom-commands
  :type '(choice (symbol :tag "No limit" nil)
		 (integer :tag "Max number of tagged entries")
		 (repeat
		  (cons (choice :tag "Agenda type"
				(const agenda)
				(const todo)
				(const tags)
				(const search))
			(integer :tag "Max number of tagged entries")))))

(defcustom org-agenda-max-effort nil
  "Maximum cumulated effort duration for the agenda.
This can be nil (no limit) or a number of minutes (as an integer)
or an alist of agenda types with an associated number of minutes
to limit entries to in this type."
  :version "24.4"
  :package-version '(Org . "8.0")
  :group 'org-agenda-custom-commands
  :type '(choice (symbol :tag "No limit" nil)
		 (integer :tag "Max number of minutes")
		 (repeat
		  (cons (choice :tag "Agenda type"
				(const agenda)
				(const todo)
				(const tags)
				(const search))
			(integer :tag "Max number of minutes")))))

(defvar org-agenda-keep-restricted-file-list nil)
(defvar org-keys nil)
(defvar org-match nil)
;;;###autoload
(defun org-agenda (&optional arg org-keys restriction)
  "Dispatch agenda commands to collect entries to the agenda buffer.
Prompts for a command to execute.  Any prefix arg will be passed
on to the selected command.  The default selections are:

a     Call `org-agenda-list' to display the agenda for current day or week.
t     Call `org-todo-list' to display the global todo list.
T     Call `org-todo-list' to display the global todo list, select only
      entries with a specific TODO keyword (the user gets a prompt).
m     Call `org-tags-view' to display headlines with tags matching
      a condition  (the user is prompted for the condition).
M     Like `m', but select only TODO entries, no ordinary headlines.
e     Export views to associated files.
s     Search entries for keywords.
S     Search entries for keywords, only with TODO keywords.
/     Multi occur across all agenda files and also files listed
      in `org-agenda-text-search-extra-files'.
<     Restrict agenda commands to buffer, subtree, or region.
      Press several times to get the desired effect.
>     Remove a previous restriction.
#     List \"stuck\" projects.
!     Configure what \"stuck\" means.
C     Configure custom agenda commands.

More commands can be added by configuring the variable
`org-agenda-custom-commands'.  In particular, specific tags and TODO keyword
searches can be pre-defined in this way.

If the current buffer is in Org mode and visiting a file, you can also
first press `<' once to indicate that the agenda should be temporarily
\(until the next use of `\\[org-agenda]') restricted to the current file.
Pressing `<' twice means to restrict to the current subtree or region
\(if active)."
  (interactive "P")
  (catch 'exit
    (let* ((prefix-descriptions nil)
	   (org-agenda-buffer-name org-agenda-buffer-name)
	   (org-agenda-window-setup (if (equal (buffer-name)
					       org-agenda-buffer-name)
					'current-window
				      org-agenda-window-setup))
	   (org-agenda-custom-commands-orig org-agenda-custom-commands)
	   (org-agenda-custom-commands
	    ;; normalize different versions
	    (delq nil
		  (mapcar
		   (lambda (x)
		     (cond ((stringp (cdr x))
			    (push x prefix-descriptions)
			    nil)
			   ((stringp (nth 1 x)) x)
			   ((not (nth 1 x)) (cons (car x) (cons "" (cddr x))))
			   (t (cons (car x) (cons "" (cdr x))))))
		   org-agenda-custom-commands)))
	   (org-agenda-custom-commands
	    (org-contextualize-keys
	     org-agenda-custom-commands org-agenda-custom-commands-contexts))
	   (buf (current-buffer))
	   (bfn (buffer-file-name (buffer-base-buffer)))
	   entry key type org-match lprops ans)
      ;; Turn off restriction unless there is an overriding one,
      (unless org-agenda-overriding-restriction
	(unless org-agenda-keep-restricted-file-list
	  ;; There is a request to keep the file list in place
	  (put 'org-agenda-files 'org-restrict nil))
	(setq org-agenda-restrict nil)
	(move-marker org-agenda-restrict-begin nil)
	(move-marker org-agenda-restrict-end nil))
      ;; Delete old local properties
      (put 'org-agenda-redo-command 'org-lprops nil)
      ;; Delete previously set last-arguments
      (put 'org-agenda-redo-command 'last-args nil)
      ;; Remember where this call originated
      (setq org-agenda-last-dispatch-buffer (current-buffer))
      (unless org-keys
	(setq ans (org-agenda-get-restriction-and-command prefix-descriptions)
	      org-keys (car ans)
	      restriction (cdr ans)))
      ;; If we have sticky agenda buffers, set a name for the buffer,
      ;; depending on the invoking keys.  The user may still set this
      ;; as a command option, which will overwrite what we do here.
      (if org-agenda-sticky
	  (setq org-agenda-buffer-name
		(format "*Org Agenda(%s)*" org-keys)))
      ;; Establish the restriction, if any
      (when (and (not org-agenda-overriding-restriction) restriction)
	(put 'org-agenda-files 'org-restrict (list bfn))
	(cond
	 ((eq restriction 'region)
	  (setq org-agenda-restrict (current-buffer))
	  (move-marker org-agenda-restrict-begin (region-beginning))
	  (move-marker org-agenda-restrict-end (region-end)))
	 ((eq restriction 'subtree)
	  (save-excursion
	    (setq org-agenda-restrict (current-buffer))
	    (org-back-to-heading t)
	    (move-marker org-agenda-restrict-begin (point))
	    (move-marker org-agenda-restrict-end
			 (progn (org-end-of-subtree t)))))))

      ;; For example the todo list should not need it (but does...)
      (cond
       ((setq entry (assoc org-keys org-agenda-custom-commands))
	(if (or (symbolp (nth 2 entry)) (functionp (nth 2 entry)))
	    (progn
	      (setq type (nth 2 entry) org-match (eval (nth 3 entry))
		    lprops (nth 4 entry))
	      (if org-agenda-sticky
		  (setq org-agenda-buffer-name
			(or (and (stringp org-match) (format "*Org Agenda(%s:%s)*" org-keys org-match))
			    (format "*Org Agenda(%s)*" org-keys))))
	      (put 'org-agenda-redo-command 'org-lprops lprops)
	      (cond
	       ((eq type 'agenda)
		(org-let lprops '(org-agenda-list current-prefix-arg)))
	       ((eq type 'agenda*)
		(org-let lprops '(org-agenda-list current-prefix-arg nil nil t)))
	       ((eq type 'alltodo)
		(org-let lprops '(org-todo-list current-prefix-arg)))
	       ((eq type 'search)
		(org-let lprops '(org-search-view current-prefix-arg org-match nil)))
	       ((eq type 'stuck)
		(org-let lprops '(org-agenda-list-stuck-projects
				  current-prefix-arg)))
	       ((eq type 'tags)
		(org-let lprops '(org-tags-view current-prefix-arg org-match)))
	       ((eq type 'tags-todo)
		(org-let lprops '(org-tags-view '(4) org-match)))
	       ((eq type 'todo)
		(org-let lprops '(org-todo-list org-match)))
	       ((eq type 'tags-tree)
		(org-check-for-org-mode)
		(org-let lprops '(org-match-sparse-tree current-prefix-arg org-match)))
	       ((eq type 'todo-tree)
		(org-check-for-org-mode)
		(org-let lprops
		  '(org-occur (concat "^" org-outline-regexp "[ \t]*"
				      (regexp-quote org-match) "\\>"))))
	       ((eq type 'occur-tree)
		(org-check-for-org-mode)
		(org-let lprops '(org-occur org-match)))
	       ((functionp type)
		(org-let lprops '(funcall type org-match)))
	       ((fboundp type)
		(org-let lprops '(funcall type org-match)))
	       (t (user-error "Invalid custom agenda command type %s" type))))
	  (org-agenda-run-series (nth 1 entry) (cddr entry))))
       ((equal org-keys "C")
	(setq org-agenda-custom-commands org-agenda-custom-commands-orig)
	(customize-variable 'org-agenda-custom-commands))
       ((equal org-keys "a") (call-interactively 'org-agenda-list))
       ((equal org-keys "s") (call-interactively 'org-search-view))
       ((equal org-keys "S") (org-call-with-arg 'org-search-view (or arg '(4))))
       ((equal org-keys "t") (call-interactively 'org-todo-list))
       ((equal org-keys "T") (org-call-with-arg 'org-todo-list (or arg '(4))))
       ((equal org-keys "m") (call-interactively 'org-tags-view))
       ((equal org-keys "M") (org-call-with-arg 'org-tags-view (or arg '(4))))
       ((equal org-keys "e") (call-interactively 'org-store-agenda-views))
       ((equal org-keys "?") (org-tags-view nil "+FLAGGED")
	(add-hook
	 'post-command-hook
	 (lambda ()
	   (unless (current-message)
	     (let* ((m (org-agenda-get-any-marker))
		    (note (and m (org-entry-get m "THEFLAGGINGNOTE"))))
	       (when note
		 (message (concat
			   "FLAGGING-NOTE ([?] for more info): "
			   (org-add-props
			       (replace-regexp-in-string
				"\\\\n" "//"
				(copy-sequence note))
			       nil 'face 'org-warning)))))))
	 t t))
       ((equal org-keys "#") (call-interactively 'org-agenda-list-stuck-projects))
       ((equal org-keys "/") (call-interactively 'org-occur-in-agenda-files))
       ((equal org-keys "!") (customize-variable 'org-stuck-projects))
       (t (user-error "Invalid agenda key"))))))

(defvar org-agenda-multi)

(defun org-agenda-append-agenda ()
  "Append another agenda view to the current one.
This function allows interactive building of block agendas.
Agenda views are separated by `org-agenda-block-separator'."
  (interactive)
  (unless (derived-mode-p 'org-agenda-mode)
    (user-error "Can only append from within agenda buffer"))
  (let ((org-agenda-multi t))
    (org-agenda)
    (widen)
    (org-agenda-finalize)
    (setq buffer-read-only t)
    (org-agenda-fit-window-to-buffer)))

(defun org-agenda-normalize-custom-commands (cmds)
  "Normalize custom commands CMDS."
  (delq nil
	(mapcar
	 (lambda (x)
	   (cond ((stringp (cdr x)) nil)
		 ((stringp (nth 1 x)) x)
		 ((not (nth 1 x)) (cons (car x) (cons "" (cddr x))))
		 (t (cons (car x) (cons "" (cdr x))))))
	 cmds)))

(defun org-agenda-get-restriction-and-command (prefix-descriptions)
  "The user interface for selecting an agenda command."
  (catch 'exit
    (let* ((bfn (buffer-file-name (buffer-base-buffer)))
	   (restrict-ok (and bfn (derived-mode-p 'org-mode)))
	   (region-p (org-region-active-p))
	   (custom org-agenda-custom-commands)
	   (selstring "")
	   restriction second-time
	   c entry key type match prefixes rmheader header-end custom1 desc
	   line lines left right n n1)
      (save-window-excursion
	(delete-other-windows)
	(org-switch-to-buffer-other-window " *Agenda Commands*")
	(erase-buffer)
	(insert (eval-when-compile
		  (let ((header
			 "Press key for an agenda command:
--------------------------------        <   Buffer, subtree/region restriction
a   Agenda for current week or day      >   Remove restriction
t   List of all TODO entries            e   Export agenda views
m   Match a TAGS/PROP/TODO query        T   Entries with special TODO kwd
s   Search for keywords                 M   Like m, but only TODO entries
/   Multi-occur                         S   Like s, but only TODO entries
?   Find :FLAGGED: entries              C   Configure custom agenda commands
*   Toggle sticky agenda views          #   List stuck projects (!=configure)
")
			(start 0))
		    (while (string-match
			    "\\(^\\|   \\|(\\)\\(\\S-\\)\\( \\|=\\)"
			    header start)
		      (setq start (match-end 0))
		      (add-text-properties (match-beginning 2) (match-end 2)
					   '(face bold) header))
		    header)))
	(setq header-end (point-marker))
	(while t
	  (setq custom1 custom)
	  (when (eq rmheader t)
	    (org-goto-line 1)
	    (re-search-forward ":" nil t)
	    (delete-region (match-end 0) (point-at-eol))
	    (forward-char 1)
	    (looking-at "-+")
	    (delete-region (match-end 0) (point-at-eol))
	    (move-marker header-end (match-end 0)))
	  (goto-char header-end)
	  (delete-region (point) (point-max))

	  ;; Produce all the lines that describe custom commands and prefixes
	  (setq lines nil)
	  (while (setq entry (pop custom1))
	    (setq key (car entry) desc (nth 1 entry)
		  type (nth 2 entry)
		  match (nth 3 entry))
	    (if (> (length key) 1)
		(cl-pushnew (string-to-char key) prefixes :test #'equal)
	      (setq line
		    (format
		     "%-4s%-14s"
		     (org-add-props (copy-sequence key)
			 '(face bold))
		     (cond
		      ((string-match "\\S-" desc) desc)
		      ((eq type 'agenda) "Agenda for current week or day")
		      ((eq type 'agenda*) "Appointments for current week or day")
		      ((eq type 'alltodo) "List of all TODO entries")
		      ((eq type 'search) "Word search")
		      ((eq type 'stuck) "List of stuck projects")
		      ((eq type 'todo) "TODO keyword")
		      ((eq type 'tags) "Tags query")
		      ((eq type 'tags-todo) "Tags (TODO)")
		      ((eq type 'tags-tree) "Tags tree")
		      ((eq type 'todo-tree) "TODO kwd tree")
		      ((eq type 'occur-tree) "Occur tree")
		      ((functionp type) (if (symbolp type)
					    (symbol-name type)
					  "Lambda expression"))
		      (t "???"))))
	      (if org-agenda-menu-show-matcher
		  (setq line
			(concat line ": "
				(cond
				 ((stringp match)
				  (setq match (copy-sequence match))
				  (org-add-props match nil 'face 'org-warning))
				 ((listp type)
				  (format "set of %d commands" (length type))))))
		(if (org-string-nw-p match)
		    (add-text-properties
		     0 (length line) (list 'help-echo
					   (concat "Matcher: " match)) line)))
	      (push line lines)))
	  (setq lines (nreverse lines))
	  (when prefixes
	    (mapc (lambda (x)
		    (push
		     (format "%s   %s"
			     (org-add-props (char-to-string x)
				 nil 'face 'bold)
			     (or (cdr (assoc (concat selstring
						     (char-to-string x))
					     prefix-descriptions))
				 "Prefix key"))
		     lines))
		  prefixes))

	  ;; Check if we should display in two columns
	  (if org-agenda-menu-two-columns
	      (progn
		(setq n (length lines)
		      n1 (+ (/ n 2) (mod n 2))
		      right (nthcdr n1 lines)
		      left (copy-sequence lines))
		(setcdr (nthcdr (1- n1) left) nil))
	    (setq left lines right nil))
	  (while left
	    (insert "\n" (pop left))
	    (when right
	      (if (< (current-column) 40)
		  (move-to-column 40 t)
		(insert "   "))
	      (insert (pop right))))

	  ;; Make the window the right size
	  (goto-char (point-min))
	  (if second-time
	      (if (not (pos-visible-in-window-p (point-max)))
		  (org-fit-window-to-buffer))
	    (setq second-time t)
	    (org-fit-window-to-buffer))

	  ;; Ask for selection
	  (message "Press key for agenda command%s:"
		   (if (or restrict-ok org-agenda-overriding-restriction)
		       (if org-agenda-overriding-restriction
			   " (restriction lock active)"
			 (if restriction
			     (format " (restricted to %s)" restriction)
			   " (unrestricted)"))
		     ""))
	  (setq c (read-char-exclusive))
	  (message "")
	  (cond
	   ((assoc (char-to-string c) custom)
	    (setq selstring (concat selstring (char-to-string c)))
	    (throw 'exit (cons selstring restriction)))
	   ((memq c prefixes)
	    (setq selstring (concat selstring (char-to-string c))
		  prefixes nil
		  rmheader (or rmheader t)
		  custom (delq nil (mapcar
				    (lambda (x)
				      (if (or (= (length (car x)) 1)
					      (/= (string-to-char (car x)) c))
					  nil
					(cons (substring (car x) 1) (cdr x))))
				    custom))))
	   ((eq c ?*)
	    (call-interactively 'org-toggle-sticky-agenda)
	    (sit-for 2))
	   ((and (not restrict-ok) (memq c '(?1 ?0 ?<)))
	    (message "Restriction is only possible in Org buffers")
	    (ding) (sit-for 1))
	   ((eq c ?1)
	    (org-agenda-remove-restriction-lock 'noupdate)
	    (setq restriction 'buffer))
	   ((eq c ?0)
	    (org-agenda-remove-restriction-lock 'noupdate)
	    (setq restriction (if region-p 'region 'subtree)))
	   ((eq c ?<)
	    (org-agenda-remove-restriction-lock 'noupdate)
	    (setq restriction
		  (cond
		   ((eq restriction 'buffer)
		    (if region-p 'region 'subtree))
		   ((memq restriction '(subtree region))
		    nil)
		   (t 'buffer))))
	   ((eq c ?>)
	    (org-agenda-remove-restriction-lock 'noupdate)
	    (setq restriction nil))
	   ((and (equal selstring "") (memq c '(?s ?S ?a ?t ?m ?L ?C ?e ?T ?M ?# ?! ?/ ??)))
	    (throw 'exit (cons (setq selstring (char-to-string c)) restriction)))
           ((and (> (length selstring) 0) (eq c ?\d))
            (delete-window)
            (org-agenda-get-restriction-and-command prefix-descriptions))

	   ((equal c ?q) (error "Abort"))
	   (t (user-error "Invalid key %c" c))))))))

(defun org-agenda-fit-window-to-buffer ()
  "Fit the window to the buffer size."
  (and (memq org-agenda-window-setup '(reorganize-frame))
       (fboundp 'fit-window-to-buffer)
       (if (and (= (cdr org-agenda-window-frame-fractions) 1.0)
		(= (car org-agenda-window-frame-fractions) 1.0))
	   (delete-other-windows)
	 (org-fit-window-to-buffer
	  nil
	  (floor (* (frame-height) (cdr org-agenda-window-frame-fractions)))
	  (floor (* (frame-height) (car org-agenda-window-frame-fractions)))))))

(defvar org-cmd nil)
(defvar org-agenda-overriding-cmd nil)
(defvar org-agenda-overriding-arguments nil)
(defvar org-agenda-overriding-cmd-arguments nil)
(defun org-agenda-run-series (name series)
  "Run agenda NAME as a SERIES of agenda commands."
  (org-let (nth 1 series) '(org-agenda-prepare name))
  ;; We need to reset agenda markers here, because when constructing a
  ;; block agenda, the individual blocks do not do that.
  (org-agenda-reset-markers)
  (let* ((org-agenda-multi t)
	 (redo (list 'org-agenda-run-series name (list 'quote series)))
	 (cmds (car series))
	 (gprops (nth 1 series))
	 match ;; The byte compiler incorrectly complains about this.  Keep it!
	 org-cmd type lprops)
    (while (setq org-cmd (pop cmds))
      (setq type (car org-cmd))
      (setq match (eval (nth 1 org-cmd)))
      (setq lprops (nth 2 org-cmd))
      (let ((org-agenda-overriding-arguments
	     (if (eq org-agenda-overriding-cmd org-cmd)
		 (or org-agenda-overriding-arguments
		     org-agenda-overriding-cmd-arguments))))
	(cond
	 ((eq type 'agenda)
	  (org-let2 gprops lprops
	    '(call-interactively 'org-agenda-list)))
	 ((eq type 'agenda*)
	  (org-let2 gprops lprops
	    '(funcall 'org-agenda-list nil nil t)))
	 ((eq type 'alltodo)
	  (org-let2 gprops lprops
	    '(call-interactively 'org-todo-list)))
	 ((eq type 'search)
	  (org-let2 gprops lprops
	    '(org-search-view current-prefix-arg match nil)))
	 ((eq type 'stuck)
	  (org-let2 gprops lprops
	    '(call-interactively 'org-agenda-list-stuck-projects)))
	 ((eq type 'tags)
	  (org-let2 gprops lprops
	    '(org-tags-view current-prefix-arg match)))
	 ((eq type 'tags-todo)
	  (org-let2 gprops lprops
	    '(org-tags-view '(4) match)))
	 ((eq type 'todo)
	  (org-let2 gprops lprops
	    '(org-todo-list match)))
	 ((fboundp type)
	  (org-let2 gprops lprops
	    '(funcall type match)))
	 (t (error "Invalid type in command series")))))
    (widen)
    (let ((inhibit-read-only t))
      (add-text-properties (point-min) (point-max)
			   `(org-series t org-series-redo-cmd ,redo)))
    (setq org-agenda-redo-command redo)
    (goto-char (point-min)))
  (org-agenda-fit-window-to-buffer)
  (org-let (nth 1 series) '(org-agenda-finalize)))

;;;###autoload
(defmacro org-batch-agenda (cmd-key &rest parameters)
  "Run an agenda command in batch mode and send the result to STDOUT.
If CMD-KEY is a string of length 1, it is used as a key in
`org-agenda-custom-commands' and triggers this command.  If it is a
longer string it is used as a tags/todo match string.
Parameters are alternating variable names and values that will be bound
before running the agenda command."
  (org-eval-in-environment (org-make-parameter-alist parameters)
    (let (org-agenda-sticky)
      (if (> (length cmd-key) 1)
	  (org-tags-view nil cmd-key)
	(org-agenda nil cmd-key))))
  (set-buffer org-agenda-buffer-name)
  (princ (buffer-string)))

(defvar org-agenda-info nil)

;;;###autoload
(defmacro org-batch-agenda-csv (cmd-key &rest parameters)
  "Run an agenda command in batch mode and send the result to STDOUT.
If CMD-KEY is a string of length 1, it is used as a key in
`org-agenda-custom-commands' and triggers this command.  If it is a
longer string it is used as a tags/todo match string.
Parameters are alternating variable names and values that will be bound
before running the agenda command.

The output gives a line for each selected agenda item.  Each
item is a list of comma-separated values, like this:

category,head,type,todo,tags,date,time,extra,priority-l,priority-n

category     The category of the item
head         The headline, without TODO kwd, TAGS and PRIORITY
type         The type of the agenda entry, can be
                todo               selected in TODO match
                tagsmatch          selected in tags match
                diary              imported from diary
                deadline           a deadline on given date
                scheduled          scheduled on given date
                timestamp          entry has timestamp on given date
                closed             entry was closed on given date
                upcoming-deadline  warning about deadline
                past-scheduled     forwarded scheduled item
                block              entry has date block including g. date
todo         The todo keyword, if any
tags         All tags including inherited ones, separated by colons
date         The relevant date, like 2007-2-14
time         The time, like 15:00-16:50
extra        Sting with extra planning info
priority-l   The priority letter if any was given
priority-n   The computed numerical priority
agenda-day   The day in the agenda where this is listed"
  (org-eval-in-environment (append '((org-agenda-remove-tags t))
				   (org-make-parameter-alist parameters))
    (if (> (length cmd-key) 2)
	(org-tags-view nil cmd-key)
      (org-agenda nil cmd-key)))
  (set-buffer org-agenda-buffer-name)
  (let* ((lines (org-split-string (buffer-string) "\n"))
	 line)
    (while (setq line (pop lines))
      (catch 'next
	(if (not (get-text-property 0 'org-category line)) (throw 'next nil))
	(setq org-agenda-info
	      (org-fix-agenda-info (text-properties-at 0 line)))
	(princ
	 (mapconcat 'org-agenda-export-csv-mapper
		    '(org-category txt type todo tags date time extra
				   priority-letter priority agenda-day)
		    ","))
	(princ "\n")))))

(defun org-fix-agenda-info (props)
  "Make sure all properties on an agenda item have a canonical form.
This ensures the export commands can easily use it."
  (let (tmp re)
    (when (setq tmp (plist-get props 'tags))
      (setq props (plist-put props 'tags (mapconcat 'identity tmp ":"))))
    (when (setq tmp (plist-get props 'date))
      (if (integerp tmp) (setq tmp (calendar-gregorian-from-absolute tmp)))
      (let ((calendar-date-display-form '(year "-" month "-" day)))
	'((format "%4d, %9s %2s, %4s" dayname monthname day year))

	(setq tmp (calendar-date-string tmp)))
      (setq props (plist-put props 'date tmp)))
    (when (setq tmp (plist-get props 'day))
      (if (integerp tmp) (setq tmp (calendar-gregorian-from-absolute tmp)))
      (let ((calendar-date-display-form '(year "-" month "-" day)))
	(setq tmp (calendar-date-string tmp)))
      (setq props (plist-put props 'day tmp))
      (setq props (plist-put props 'agenda-day tmp)))
    (when (setq tmp (plist-get props 'txt))
      (when (string-match "\\[#\\([A-Z0-9]\\)\\] ?" tmp)
	(plist-put props 'priority-letter (match-string 1 tmp))
	(setq tmp (replace-match "" t t tmp)))
      (when (and (setq re (plist-get props 'org-todo-regexp))
		 (setq re (concat "\\`\\.*" re " ?"))
		 (let ((case-fold-search nil)) (string-match re tmp)))
	(plist-put props 'todo (match-string 1 tmp))
	(setq tmp (replace-match "" t t tmp)))
      (plist-put props 'txt tmp)))
  props)

(defun org-agenda-export-csv-mapper (prop)
  (let ((res (plist-get org-agenda-info prop)))
    (setq res
	  (cond
	   ((not res) "")
	   ((stringp res) res)
	   (t (prin1-to-string res))))
    (org-trim (replace-regexp-in-string "," ";" res nil t))))

;;;###autoload
(defun org-store-agenda-views (&rest parameters)
  "Store agenda views."
  (interactive)
  (eval (list 'org-batch-store-agenda-views)))

;;;###autoload
(defmacro org-batch-store-agenda-views (&rest parameters)
  "Run all custom agenda commands that have a file argument."
  (let ((cmds (org-agenda-normalize-custom-commands org-agenda-custom-commands))
	(pop-up-frames nil)
	(dir default-directory)
	(pars (org-make-parameter-alist parameters))
	cmd thiscmdkey thiscmdcmd match files opts cmd-or-set bufname)
    (save-window-excursion
      (while cmds
	(setq cmd (pop cmds)
	      thiscmdkey (car cmd)
	      thiscmdcmd (cdr cmd)
	      match (nth 2 thiscmdcmd)
	      bufname (if org-agenda-sticky
			  (or (and (stringp match)
				   (format "*Org Agenda(%s:%s)*" thiscmdkey match))
			      (format "*Org Agenda(%s)*" thiscmdkey))
			org-agenda-buffer-name)
	      cmd-or-set (nth 2 cmd)
	      opts (nth (if (listp cmd-or-set) 3 4) cmd)
	      files (nth (if (listp cmd-or-set) 4 5) cmd))
	(if (stringp files) (setq files (list files)))
	(when files
	  (org-eval-in-environment (append org-agenda-exporter-settings
					   opts pars)
	    (org-agenda nil thiscmdkey))
	  (set-buffer bufname)
	  (while files
	    (org-eval-in-environment (append org-agenda-exporter-settings
					     opts pars)
	      (org-agenda-write (expand-file-name (pop files) dir) nil t bufname)))
	  (and (get-buffer bufname)
	       (kill-buffer bufname)))))))

(defvar org-agenda-current-span nil
  "The current span used in the agenda view.") ; local variable in the agenda buffer
(defun org-agenda-mark-header-line (pos)
  "Mark the line at POS as an agenda structure header."
  (save-excursion
    (goto-char pos)
    (put-text-property (point-at-bol) (point-at-eol)
		       'org-agenda-structural-header t)
    (when org-agenda-title-append
      (put-text-property (point-at-bol) (point-at-eol)
			 'org-agenda-title-append org-agenda-title-append))))

(defvar org-mobile-creating-agendas) ; defined in org-mobile.el
(defvar org-agenda-write-buffer-name "Agenda View")
(defun org-agenda-write (file &optional open nosettings agenda-bufname)
  "Write the current buffer (an agenda view) as a file.

Depending on the extension of the file name, plain text (.txt),
HTML (.html or .htm), PDF (.pdf) or Postscript (.ps) is produced.
If the extension is .ics, translate visible agenda into iCalendar
format.  If the extension is .org, collect all subtrees
corresponding to the agenda entries and add them in an .org file.

With prefix argument OPEN, open the new file immediately.  If
NOSETTINGS is given, do not scope the settings of
`org-agenda-exporter-settings' into the export commands.  This is
used when the settings have already been scoped and we do not
wish to overrule other, higher priority settings.  If
AGENDA-BUFFER-NAME is provided, use this as the buffer name for
the agenda to write."
  (interactive "FWrite agenda to file: \nP")
  (if (or (not (file-writable-p file))
	  (and (file-exists-p file)
	       (if (called-interactively-p 'any)
		   (not (y-or-n-p (format "Overwrite existing file %s? " file))))))
      (user-error "Cannot write agenda to file %s" file))
  (org-let (if nosettings nil org-agenda-exporter-settings)
    '(save-excursion
       (save-window-excursion
	 (let ((bs (copy-sequence (buffer-string)))
	       (extension (file-name-extension file))
	       (default-directory (file-name-directory file))
	       beg content)
	   (with-temp-buffer
	     (rename-buffer org-agenda-write-buffer-name t)
	     (set-buffer-modified-p nil)
	     (insert bs)
	     (org-agenda-remove-marked-text 'invisible 'org-filtered)
	     (run-hooks 'org-agenda-before-write-hook)
	     (cond
	      ((bound-and-true-p org-mobile-creating-agendas)
	       (org-mobile-write-agenda-for-mobile file))
	      ((string= "org" extension)
	       (let (content p m message-log-max)
		 (goto-char (point-min))
		 (while (setq p (next-single-property-change (point) 'org-hd-marker nil))
		   (goto-char p)
		   (setq m (get-text-property (point) 'org-hd-marker))
		   (when m
		     (push (save-excursion
			     (set-buffer (marker-buffer m))
			     (goto-char m)
			     (org-copy-subtree 1 nil t t)
			     org-subtree-clip)
			   content)))
		 (find-file file)
		 (erase-buffer)
		 (dolist (s content) (org-paste-subtree 1 s))
		 (write-file file)
		 (kill-buffer (current-buffer))
		 (message "Org file written to %s" file)))
	      ((member extension '("html" "htm"))
	       (or (require 'htmlize nil t)
		   (error "Please install htmlize from https://github.com/hniksic/emacs-htmlize"))
	       (set-buffer (htmlize-buffer (current-buffer)))
	       (when org-agenda-export-html-style
		 ;; replace <style> section with org-agenda-export-html-style
		 (goto-char (point-min))
		 (kill-region (- (search-forward "<style") 6)
			      (search-forward "</style>"))
		 (insert org-agenda-export-html-style))
	       (write-file file)
	       (kill-buffer (current-buffer))
	       (message "HTML written to %s" file))
	      ((string= "ps" extension)
	       (require 'ps-print)
	       (ps-print-buffer-with-faces file)
	       (message "Postscript written to %s" file))
	      ((string= "pdf" extension)
	       (require 'ps-print)
	       (ps-print-buffer-with-faces
		(concat (file-name-sans-extension file) ".ps"))
	       (call-process "ps2pdf" nil nil nil
			     (expand-file-name
			      (concat (file-name-sans-extension file) ".ps"))
			     (expand-file-name file))
	       (delete-file (concat (file-name-sans-extension file) ".ps"))
	       (message "PDF written to %s" file))
	      ((string= "ics" extension)
	       (require 'ox-icalendar)
	       (org-icalendar-export-current-agenda (expand-file-name file)))
	      (t
	       (let ((bs (buffer-string)))
		 (find-file file)
		 (erase-buffer)
		 (insert bs)
		 (save-buffer 0)
		 (kill-buffer (current-buffer))
		 (message "Plain text written to %s" file))))))))
    (set-buffer (or agenda-bufname
		    (and (called-interactively-p 'any) (buffer-name))
		    org-agenda-buffer-name)))
  (when open (org-open-file file)))

(defun org-agenda-remove-marked-text (property &optional value)
  "Delete all text marked with VALUE of PROPERTY.
VALUE defaults to t."
  (let (beg)
    (setq value (or value t))
    (while (setq beg (text-property-any (point-min) (point-max)
					property value))
      (delete-region
       beg (or (next-single-property-change beg property)
	       (point-max))))))

(defun org-agenda-add-entry-text ()
  "Add entry text to agenda lines.
This will add a maximum of `org-agenda-add-entry-text-maxlines' lines of the
entry text following headings shown in the agenda.
Drawers will be excluded, also the line with scheduling/deadline info."
  (when (and (> org-agenda-add-entry-text-maxlines 0)
	     (not (bound-and-true-p org-mobile-creating-agendas)))
    (let (m txt)
      (goto-char (point-min))
      (while (not (eobp))
	(if (not (setq m (org-get-at-bol 'org-hd-marker)))
	    (beginning-of-line 2)
	  (setq txt (org-agenda-get-some-entry-text
		     m org-agenda-add-entry-text-maxlines "    > "))
	  (end-of-line 1)
	  (if (string-match "\\S-" txt)
	      (insert "\n" txt)
	    (or (eobp) (forward-char 1))))))))

(defun org-agenda-get-some-entry-text (marker n-lines &optional indent
					      &rest keep)
  "Extract entry text from MARKER, at most N-LINES lines.
This will ignore drawers etc, just get the text.
If INDENT is given, prefix every line with this string.  If KEEP is
given, it is a list of symbols, defining stuff that should not be
removed from the entry content.  Currently only `planning' is allowed here."
  (let (txt drawer-re kwd-time-re ind)
    (save-excursion
      (with-current-buffer (marker-buffer marker)
	(if (not (derived-mode-p 'org-mode))
	    (setq txt "")
	  (org-with-wide-buffer
	   (goto-char marker)
	   (end-of-line 1)
	   (setq txt (buffer-substring
		      (min (1+ (point)) (point-max))
		      (progn (outline-next-heading) (point)))
		 drawer-re org-drawer-regexp
		 kwd-time-re (concat "^[ \t]*" org-keyword-time-regexp
				     ".*\n?"))
	   (with-temp-buffer
	     (insert txt)
	     (when org-agenda-add-entry-text-descriptive-links
	       (goto-char (point-min))
	       (while (org-activate-links (point-max))
		 (add-text-properties (match-beginning 0) (match-end 0)
				      '(face org-link))))
	     (goto-char (point-min))
	     (while (re-search-forward org-bracket-link-regexp (point-max) t)
	       (set-text-properties (match-beginning 0) (match-end 0)
				    nil))
	     (goto-char (point-min))
	     (while (re-search-forward drawer-re nil t)
	       (delete-region
		(match-beginning 0)
		(progn (re-search-forward
			"^[ \t]*:END:.*\n?" nil 'move)
		       (point))))
	     (unless (member 'planning keep)
	       (goto-char (point-min))
	       (while (re-search-forward kwd-time-re nil t)
		 (replace-match "")))
	     (goto-char (point-min))
	     (when org-agenda-entry-text-exclude-regexps
	       (let ((re-list org-agenda-entry-text-exclude-regexps)	re)
		 (while (setq re (pop re-list))
		   (goto-char (point-min))
		   (while (re-search-forward re nil t)
		     (replace-match "")))))
	     (goto-char (point-max))
	     (skip-chars-backward " \t\n")
	     (if (looking-at "[ \t\n]+\\'") (replace-match ""))

	     ;; find and remove min common indentation
	     (goto-char (point-min))
	     (untabify (point-min) (point-max))
	     (setq ind (org-get-indentation))
	     (while (not (eobp))
	       (unless (looking-at "[ \t]*$")
		 (setq ind (min ind (org-get-indentation))))
	       (beginning-of-line 2))
	     (goto-char (point-min))
	     (while (not (eobp))
	       (unless (looking-at "[ \t]*$")
		 (move-to-column ind)
		 (delete-region (point-at-bol) (point)))
	       (beginning-of-line 2))

	     (run-hooks 'org-agenda-entry-text-cleanup-hook)

	     (goto-char (point-min))
	     (when indent
	       (while (and (not (eobp)) (re-search-forward "^" nil t))
		 (replace-match indent t t)))
	     (goto-char (point-min))
	     (while (looking-at "[ \t]*\n") (replace-match ""))
	     (goto-char (point-max))
	     (when (> (org-current-line)
		      n-lines)
	       (org-goto-line (1+ n-lines))
	       (backward-char 1))
	     (setq txt (buffer-substring (point-min) (point))))))))
    txt))

(defun org-check-for-org-mode ()
  "Make sure current buffer is in org-mode.  Error if not."
  (or (derived-mode-p 'org-mode)
      (error "Cannot execute Org agenda command on buffer in %s"
	     major-mode)))

;;; Agenda prepare and finalize

(defvar org-agenda-multi nil)  ; dynamically scoped
(defvar org-agenda-pre-window-conf nil)
(defvar org-agenda-columns-active nil)
(defvar org-agenda-name nil)
(defvar org-agenda-tag-filter nil)
(defvar org-agenda-category-filter nil)
(defvar org-agenda-regexp-filter nil)
(defvar org-agenda-effort-filter nil)
(defvar org-agenda-top-headline-filter nil)
(defvar org-agenda-tag-filter-preset nil
  "A preset of the tags filter used for secondary agenda filtering.
This must be a list of strings, each string must be a single tag preceded
by \"+\" or \"-\".
This variable should not be set directly, but agenda custom commands can
bind it in the options section.  The preset filter is a global property of
the entire agenda view.  In a block agenda, it will not work reliably to
define a filter for one of the individual blocks.  You need to set it in
the global options and expect it to be applied to the entire view.")

(defvar org-agenda-category-filter-preset nil
  "A preset of the category filter used for secondary agenda filtering.
This must be a list of strings, each string must be a single category
preceded by \"+\" or \"-\".
This variable should not be set directly, but agenda custom commands can
bind it in the options section.  The preset filter is a global property of
the entire agenda view.  In a block agenda, it will not work reliably to
define a filter for one of the individual blocks.  You need to set it in
the global options and expect it to be applied to the entire view.")

(defvar org-agenda-regexp-filter-preset nil
  "A preset of the regexp filter used for secondary agenda filtering.
This must be a list of strings, each string must be a single regexp
preceded by \"+\" or \"-\".
This variable should not be set directly, but agenda custom commands can
bind it in the options section.  The preset filter is a global property of
the entire agenda view.  In a block agenda, it will not work reliably to
define a filter for one of the individual blocks.  You need to set it in
the global options and expect it to be applied to the entire view.")

(defvar org-agenda-effort-filter-preset nil
  "A preset of the effort condition used for secondary agenda filtering.
This must be a list of strings, each string must be a single regexp
preceded by \"+\" or \"-\".
This variable should not be set directly, but agenda custom commands can
bind it in the options section.  The preset filter is a global property of
the entire agenda view.  In a block agenda, it will not work reliably to
define a filter for one of the individual blocks.  You need to set it in
the global options and expect it to be applied to the entire view.")

(defun org-agenda-use-sticky-p ()
  "Return non-nil if an agenda buffer named
`org-agenda-buffer-name' exists and should be shown instead of
generating a new one."
  (and
   ;; turned off by user
   org-agenda-sticky
   ;; For multi-agenda buffer already exists
   (not org-agenda-multi)
   ;; buffer found
   (get-buffer org-agenda-buffer-name)
   ;; C-u parameter is same as last call
   (with-current-buffer (get-buffer org-agenda-buffer-name)
     (and
      (equal current-prefix-arg
	     org-agenda-last-prefix-arg)
      ;; In case user turned stickiness on, while having existing
      ;; Agenda buffer active, don't reuse that buffer, because it
      ;; does not have org variables local
      org-agenda-this-buffer-is-sticky))))

(defun org-agenda-prepare-window (abuf filter-alist)
  "Setup agenda buffer in the window.
ABUF is the buffer for the agenda window.
FILTER-ALIST is an alist of filters we need to apply when
`org-agenda-persistent-filter' is non-nil."
  (let* ((awin (get-buffer-window abuf)) wconf)
    (cond
     ((equal (current-buffer) abuf) nil)
     (awin (select-window awin))
     ((not (setq wconf (current-window-configuration))))
     ((eq org-agenda-window-setup 'current-window)
      (pop-to-buffer-same-window abuf))
     ((eq org-agenda-window-setup 'other-window)
      (org-switch-to-buffer-other-window abuf))
     ((eq org-agenda-window-setup 'other-frame)
      (switch-to-buffer-other-frame abuf))
     ((eq org-agenda-window-setup 'only-window)
      (delete-other-windows)
      (pop-to-buffer-same-window abuf))
     ((eq org-agenda-window-setup 'reorganize-frame)
      (delete-other-windows)
      (org-switch-to-buffer-other-window abuf)))
    (setq org-agenda-tag-filter (cdr (assq 'tag filter-alist)))
    (setq org-agenda-category-filter (cdr (assq 'cat filter-alist)))
    (setq org-agenda-effort-filter (cdr (assq 'effort filter-alist)))
    (setq org-agenda-regexp-filter (cdr (assq 're filter-alist)))
    ;; Additional test in case agenda is invoked from within agenda
    ;; buffer via elisp link.
    (unless (equal (current-buffer) abuf)
      (pop-to-buffer-same-window abuf))
    (setq org-agenda-pre-window-conf
	  (or wconf org-agenda-pre-window-conf))))

(defun org-agenda-prepare (&optional name)
  (let ((filter-alist (if org-agenda-persistent-filter
			  (with-current-buffer
			      (get-buffer-create org-agenda-buffer-name)
			    (list `(tag . ,org-agenda-tag-filter)
				  `(re . ,org-agenda-regexp-filter)
				  `(effort . ,org-agenda-effort-filter)
				  `(cat . ,org-agenda-category-filter))))))
    (if (org-agenda-use-sticky-p)
	(progn
	  (put 'org-agenda-tag-filter :preset-filter nil)
	  (put 'org-agenda-category-filter :preset-filter nil)
	  (put 'org-agenda-regexp-filter :preset-filter nil)
	  ;; Popup existing buffer
	  (org-agenda-prepare-window (get-buffer org-agenda-buffer-name)
				     filter-alist)
	  (message "Sticky Agenda buffer, use `r' to refresh")
	  (or org-agenda-multi (org-agenda-fit-window-to-buffer))
	  (throw 'exit "Sticky Agenda buffer, use `r' to refresh"))
      (setq org-todo-keywords-for-agenda nil)
      (put 'org-agenda-tag-filter :preset-filter
	   org-agenda-tag-filter-preset)
      (put 'org-agenda-category-filter :preset-filter
	   org-agenda-category-filter-preset)
      (put 'org-agenda-regexp-filter :preset-filter
	   org-agenda-regexp-filter-preset)
      (put 'org-agenda-effort-filter :preset-filter
	   org-agenda-effort-filter-preset)
      (if org-agenda-multi
	  (progn
	    (setq buffer-read-only nil)
	    (goto-char (point-max))
	    (unless (or (bobp) org-agenda-compact-blocks
			(not org-agenda-block-separator))
	      (insert "\n"
		      (if (stringp org-agenda-block-separator)
			  org-agenda-block-separator
			(make-string (window-width) org-agenda-block-separator))
		      "\n"))
	    (narrow-to-region (point) (point-max)))
	(setq org-done-keywords-for-agenda nil)
	;; Setting any org variables that are in org-agenda-local-vars
	;; list need to be done after the prepare call
	(org-agenda-prepare-window
	 (get-buffer-create org-agenda-buffer-name) filter-alist)
	(setq buffer-read-only nil)
	(org-agenda-reset-markers)
	(let ((inhibit-read-only t)) (erase-buffer))
	(org-agenda-mode)
	(setq org-agenda-buffer (current-buffer))
	(setq org-agenda-contributing-files nil)
	(setq org-agenda-columns-active nil)
	(org-agenda-prepare-buffers (org-agenda-files nil 'ifmode))
	(setq org-todo-keywords-for-agenda
	      (org-uniquify org-todo-keywords-for-agenda))
	(setq org-done-keywords-for-agenda
	      (org-uniquify org-done-keywords-for-agenda))
	(setq org-agenda-last-prefix-arg current-prefix-arg)
	(setq org-agenda-this-buffer-name org-agenda-buffer-name)
	(and name (not org-agenda-name)
	     (setq-local org-agenda-name name)))
      (setq buffer-read-only nil))))

(defvar org-agenda-overriding-columns-format)  ; From org-colview.el
(defun org-agenda-finalize ()
  "Finishing touch for the agenda buffer, called just before displaying it."
  (unless org-agenda-multi
    (save-excursion
      (let ((inhibit-read-only t))
	(goto-char (point-min))
	(save-excursion
	  (while (org-activate-links (point-max))
	    (add-text-properties (match-beginning 0) (match-end 0)
				 '(face org-link))))
	(unless (eq org-agenda-remove-tags t)
	  (org-agenda-align-tags))
	(unless org-agenda-with-colors
	  (remove-text-properties (point-min) (point-max) '(face nil)))
	(if (and (boundp 'org-agenda-overriding-columns-format)
		 org-agenda-overriding-columns-format)
	    (setq-local org-agenda-overriding-columns-format
			org-agenda-overriding-columns-format))
	(if (and (boundp 'org-agenda-view-columns-initially)
		 org-agenda-view-columns-initially)
	    (org-agenda-columns))
	(when org-agenda-fontify-priorities
	  (org-agenda-fontify-priorities))
	(when (and org-agenda-dim-blocked-tasks org-blocker-hook)
	  (org-agenda-dim-blocked-tasks))
	(org-agenda-mark-clocking-task)
	(when org-agenda-entry-text-mode
	  (org-agenda-entry-text-hide)
	  (org-agenda-entry-text-show))
	(if (and (functionp 'org-habit-insert-consistency-graphs)
		 (save-excursion (next-single-property-change (point-min) 'org-habit-p)))
	    (org-habit-insert-consistency-graphs))
	(setq org-agenda-type (org-get-at-bol 'org-agenda-type))
	(unless (or (eq org-agenda-show-inherited-tags 'always)
		    (and (listp org-agenda-show-inherited-tags)
			 (memq org-agenda-type org-agenda-show-inherited-tags))
		    (and (eq org-agenda-show-inherited-tags t)
			 (or (eq org-agenda-use-tag-inheritance t)
			     (and (listp org-agenda-use-tag-inheritance)
				  (not (memq org-agenda-type
					     org-agenda-use-tag-inheritance))))))
	  (let (mrk)
	    (save-excursion
	      (goto-char (point-min))
	      (while (equal (forward-line) 0)
		(when (setq mrk (get-text-property (point) 'org-hd-marker))
		  (put-text-property (point-at-bol) (point-at-eol)
				     'tags (org-with-point-at mrk
					     (delete-dups
					      (mapcar 'downcase (org-get-tags-at))))))))))
	(run-hooks 'org-agenda-finalize-hook)
	(when org-agenda-top-headline-filter
	  (org-agenda-filter-top-headline-apply
	   org-agenda-top-headline-filter))
	(when org-agenda-tag-filter
	  (org-agenda-filter-apply org-agenda-tag-filter 'tag t))
	(when (get 'org-agenda-tag-filter :preset-filter)
	  (org-agenda-filter-apply
	   (get 'org-agenda-tag-filter :preset-filter) 'tag t))
	(when org-agenda-category-filter
	  (org-agenda-filter-apply org-agenda-category-filter 'category))
	(when (get 'org-agenda-category-filter :preset-filter)
	  (org-agenda-filter-apply
	   (get 'org-agenda-category-filter :preset-filter) 'category))
	(when org-agenda-regexp-filter
	  (org-agenda-filter-apply org-agenda-regexp-filter 'regexp))
	(when (get 'org-agenda-regexp-filter :preset-filter)
	  (org-agenda-filter-apply
	   (get 'org-agenda-regexp-filter :preset-filter) 'regexp))
	(when org-agenda-effort-filter
	  (org-agenda-filter-apply org-agenda-effort-filter 'effort))
	(when (get 'org-agenda-effort-filter :preset-filter)
	  (org-agenda-filter-apply
	   (get 'org-agenda-effort-filter :preset-filter) 'effort))
	(add-hook 'kill-buffer-hook 'org-agenda-reset-markers 'append 'local)))))

(defun org-agenda-mark-clocking-task ()
  "Mark the current clock entry in the agenda if it is present."
  ;; We need to widen when `org-agenda-finalize' is called from
  ;; `org-agenda-change-all-lines' (e.g. in `org-agenda-clock-in')
  (when (bound-and-true-p org-clock-current-task)
    (save-restriction
      (widen)
      (org-agenda-unmark-clocking-task)
      (when (marker-buffer org-clock-hd-marker)
	(save-excursion
	  (goto-char (point-min))
	  (let (s ov)
	    (while (setq s (next-single-property-change (point) 'org-hd-marker))
	      (goto-char s)
	      (when (equal (org-get-at-bol 'org-hd-marker)
			   org-clock-hd-marker)
		(setq ov (make-overlay (point-at-bol) (1+ (point-at-eol))))
		(overlay-put ov 'type 'org-agenda-clocking)
		(overlay-put ov 'face 'org-agenda-clocking)
		(overlay-put ov 'help-echo
			     "The clock is running in this item")))))))))

(defun org-agenda-unmark-clocking-task ()
  "Unmark the current clocking task."
  (mapc (lambda (o)
	  (if (eq (overlay-get o 'type) 'org-agenda-clocking)
	      (delete-overlay o)))
	(overlays-in (point-min) (point-max))))

(defun org-agenda-fontify-priorities ()
  "Make highest priority lines bold, and lowest italic."
  (interactive)
  (mapc (lambda (o) (if (eq (overlay-get o 'org-type) 'org-priority)
		   (delete-overlay o)))
	(overlays-in (point-min) (point-max)))
  (save-excursion
    (let (b e p ov h l)
      (goto-char (point-min))
      (while (re-search-forward "\\[#\\(.\\)\\]" nil t)
	(setq h (or (get-char-property (point) 'org-highest-priority)
		    org-highest-priority)
	      l (or (get-char-property (point) 'org-lowest-priority)
		    org-lowest-priority)
	      p (string-to-char (match-string 1))
	      b (match-beginning 0)
	      e (if (eq org-agenda-fontify-priorities 'cookies)
		    (match-end 0)
		  (point-at-eol))
	      ov (make-overlay b e))
	(overlay-put
	 ov 'face
	 (let ((special-face
		(cond ((org-face-from-face-or-color
			'priority 'org-priority
			(cdr (assoc p org-priority-faces))))
		      ((and (listp org-agenda-fontify-priorities)
			    (org-face-from-face-or-color
			     'priority 'org-priority
			     (cdr (assoc p org-agenda-fontify-priorities)))))
		      ((equal p l) 'italic)
		      ((equal p h) 'bold))))
	   (if special-face (list special-face 'org-priority) 'org-priority)))
	(overlay-put ov 'org-type 'org-priority)))))

(defvar org-depend-tag-blocked)

(defun org-agenda-dim-blocked-tasks (&optional invisible)
  "Dim currently blocked TODOs in the agenda display.
When INVISIBLE is non-nil, hide currently blocked TODO instead of
dimming them."
  (interactive "P")
  (when (called-interactively-p 'interactive)
    (message "Dim or hide blocked tasks..."))
  (dolist (o (overlays-in (point-min) (point-max)))
    (when (eq (overlay-get o 'org-type) 'org-blocked-todo)
      (delete-overlay o)))
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (let ((pos (text-property-not-all
			 (point) (point-max) 'org-todo-blocked nil)))
	       (when pos (goto-char pos)))
	(let* ((invisible (eq (org-get-at-bol 'org-todo-blocked) 'invisible))
	       (ov (make-overlay (if invisible
				     (line-end-position 0)
				   (line-beginning-position))
				 (line-end-position))))
	  (if invisible
	      (overlay-put ov 'invisible t)
	    (overlay-put ov 'face 'org-agenda-dimmed-todo-face))
	  (overlay-put ov 'org-type 'org-blocked-todo))
	(forward-line))))
  (when (called-interactively-p 'interactive)
    (message "Dim or hide blocked tasks...done")))

(defun org-agenda--mark-blocked-entry (entry)
  "For ENTRY a string with the text property `org-hd-marker', if
the header at `org-hd-marker' is blocked according to
`org-entry-blocked-p', then if `org-agenda-dim-blocked-tasks' is
'invisible and the header is not blocked by checkboxes, set the
text property `org-todo-blocked' to 'invisible, otherwise set it
to t."
  (when (get-text-property 0 'todo-state entry)
    (let ((entry-marker (get-text-property 0 'org-hd-marker entry))
          (org-blocked-by-checkboxes nil)
	  ;; Necessary so that `org-entry-blocked-p' does not change
	  ;; the buffer.
          (org-depend-tag-blocked nil))
      (when entry-marker
	(let ((blocked
	       (with-current-buffer (marker-buffer entry-marker)
		 (save-excursion
		   (goto-char entry-marker)
		   (org-entry-blocked-p)))))
	  (when blocked
	    (let ((really-invisible
		   (and (not org-blocked-by-checkboxes)
			(eq org-agenda-dim-blocked-tasks 'invisible))))
	      (put-text-property
	       0 (length entry) 'org-todo-blocked
	       (if really-invisible 'invisible t)
	       entry)))))))
  entry)

(defvar org-agenda-skip-function nil
  "Function to be called at each match during agenda construction.
If this function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued.
This may also be a Lisp form, it will be evaluated.
Never set this variable using `setq' or so, because then it will apply
to all future agenda commands.  If you do want a global skipping condition,
use the option `org-agenda-skip-function-global' instead.
The correct usage for `org-agenda-skip-function' is to bind it with
`let' to scope it dynamically into the agenda-constructing command.
A good way to set it is through options in `org-agenda-custom-commands'.")

(defun org-agenda-skip ()
  "Throw to `:skip' in places that should be skipped.
Also moves point to the end of the skipped region, so that search can
continue from there."
  (let ((p (point-at-bol)) to)
    (when (or
	   (save-excursion (goto-char p) (looking-at comment-start-skip))
	   (and org-agenda-skip-archived-trees (not org-agenda-archives-mode)
		(get-text-property p :org-archived)
		(org-end-of-subtree t))
	   (and org-agenda-skip-comment-trees
		(get-text-property p :org-comment)
		(org-end-of-subtree t))
	   (and (setq to (or (org-agenda-skip-eval org-agenda-skip-function-global)
			     (org-agenda-skip-eval org-agenda-skip-function)))
		(goto-char to))
	   (org-in-src-block-p t))
      (throw :skip t))))

(defun org-agenda-skip-eval (form)
  "If FORM is a function or a list, call (or eval) it and return the result.
`save-excursion' and `save-match-data' are wrapped around the call, so point
and match data are returned to the previous state no matter what these
functions do."
  (let (fp)
    (and form
	 (or (setq fp (functionp form))
	     (consp form))
	 (save-excursion
	   (save-match-data
	     (if fp
		 (funcall form)
	       (eval form)))))))

(defvar org-agenda-markers nil
  "List of all currently active markers created by `org-agenda'.")
(defvar org-agenda-last-marker-time (float-time)
  "Creation time of the last agenda marker.")

(defun org-agenda-new-marker (&optional pos)
  "Return a new agenda marker.
Maker is at point, or at POS if non-nil.  Org mode keeps a list of
these markers and resets them when they are no longer in use."
  (let ((m (copy-marker (or pos (point)) t)))
    (setq org-agenda-last-marker-time (float-time))
    (if org-agenda-buffer
	(with-current-buffer org-agenda-buffer
	  (push m org-agenda-markers))
      (push m org-agenda-markers))
    m))

(defun org-agenda-reset-markers ()
  "Reset markers created by `org-agenda'."
  (while org-agenda-markers
    (move-marker (pop org-agenda-markers) nil)))

(defun org-agenda-save-markers-for-cut-and-paste (beg end)
  "Save relative positions of markers in region.
This check for agenda markers in all agenda buffers currently active."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'org-agenda-mode)
	(mapc (lambda (m) (org-check-and-save-marker m beg end))
	      org-agenda-markers)))))

;;; Entry text mode

(defun org-agenda-entry-text-show-here ()
  "Add some text from the entry as context to the current line."
  (let (m txt o)
    (setq m (org-get-at-bol 'org-hd-marker))
    (unless (marker-buffer m)
      (error "No marker points to an entry here"))
    (setq txt (concat "\n" (org-no-properties
			    (org-agenda-get-some-entry-text
			     m org-agenda-entry-text-maxlines
			     org-agenda-entry-text-leaders))))
    (when (string-match "\\S-" txt)
      (setq o (make-overlay (point-at-bol) (point-at-eol)))
      (overlay-put o 'evaporate t)
      (overlay-put o 'org-overlay-type 'agenda-entry-content)
      (overlay-put o 'after-string txt))))

(defun org-agenda-entry-text-show ()
  "Add entry context for all agenda lines."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (beginning-of-line 1)
    (while (not (bobp))
      (when (org-get-at-bol 'org-hd-marker)
	(org-agenda-entry-text-show-here))
      (beginning-of-line 0))))

(defun org-agenda-entry-text-hide ()
  "Remove any shown entry context."
  (delq nil
	(mapcar (lambda (o)
		  (if (eq (overlay-get o 'org-overlay-type)
			  'agenda-entry-content)
		      (progn (delete-overlay o) t)))
		(overlays-in (point-min) (point-max)))))

(defun org-agenda-get-day-face (date)
  "Return the face DATE should be displayed with."
  (cond ((and (functionp org-agenda-day-face-function)
	      (funcall org-agenda-day-face-function date)))
	((org-agenda-today-p date) 'org-agenda-date-today)
	((memq (calendar-day-of-week date) org-agenda-weekend-days)
	 'org-agenda-date-weekend)
	(t 'org-agenda-date)))

(defvar org-agenda-show-log-scoped)

;;; Agenda Daily/Weekly

(defvar org-agenda-start-day nil  ; dynamically scoped parameter
  "Start day for the agenda view.
Custom commands can set this variable in the options section.
This is usually a string like \"2007-11-01\", \"+2d\" or any other
input allowed when reading a date through the Org calendar.
See the docstring of `org-read-date' for details.")
(defvar org-starting-day nil) ; local variable in the agenda buffer
(defvar org-arg-loc nil) ; local variable

(defvar org-agenda-buffer-tmp-name nil)
;;;###autoload
(defun org-agenda-list (&optional arg start-day span with-hour)
  "Produce a daily/weekly view from all files in variable `org-agenda-files'.
The view will be for the current day or week, but from the overview buffer
you will be able to go to other days/weeks.

With a numeric prefix argument in an interactive call, the agenda will
span ARG days.  Lisp programs should instead specify SPAN to change
the number of days.  SPAN defaults to `org-agenda-span'.

START-DAY defaults to TODAY, or to the most recent match for the weekday
given in `org-agenda-start-on-weekday'.

When WITH-HOUR is non-nil, only include scheduled and deadline
items if they have an hour specification like [h]h:mm."
  (interactive "P")
  (if org-agenda-overriding-arguments
      (setq arg (car org-agenda-overriding-arguments)
	    start-day (nth 1 org-agenda-overriding-arguments)
	    span (nth 2 org-agenda-overriding-arguments)))
  (if (and (integerp arg) (> arg 0))
      (setq span arg arg nil))
  (catch 'exit
    (setq org-agenda-buffer-name
	  (or org-agenda-buffer-tmp-name
	      (and org-agenda-doing-sticky-redo org-agenda-buffer-name)
	      (if org-agenda-sticky
		  (cond ((and org-keys (stringp org-match))
			 (format "*Org Agenda(%s:%s)*" org-keys org-match))
			(org-keys
			 (format "*Org Agenda(%s)*" org-keys))
			(t "*Org Agenda(a)*")))
	      "*Org Agenda*"))
    (org-agenda-prepare "Day/Week")
    (setq start-day (or start-day org-agenda-start-day))
    (if (stringp start-day)
	;; Convert to an absolute day number
	(setq start-day (time-to-days (org-read-date nil t start-day))))
    (org-compile-prefix-format 'agenda)
    (org-set-sorting-strategy 'agenda)
    (let* ((span (org-agenda-ndays-to-span (or span org-agenda-span)))
	   (today (org-today))
	   (sd (or start-day today))
	   (ndays (org-agenda-span-to-ndays span sd))
	   (org-agenda-start-on-weekday
	    (if (or (eq ndays 7) (eq ndays 14))
		org-agenda-start-on-weekday))
	   (thefiles (org-agenda-files nil 'ifmode))
	   (files thefiles)
	   (start (if (or (null org-agenda-start-on-weekday)
			  (< ndays 7))
		      sd
		    (let* ((nt (calendar-day-of-week
				(calendar-gregorian-from-absolute sd)))
			   (n1 org-agenda-start-on-weekday)
			   (d (- nt n1)))
		      (- sd (+ (if (< d 0) 7 0) d)))))
	   (day-numbers (list start))
	   (day-cnt 0)
	   (inhibit-redisplay (not debug-on-error))
	   (org-agenda-show-log-scoped org-agenda-show-log)
	   s e rtn rtnall file date d start-pos end-pos todayp
	   clocktable-start clocktable-end filter)
      (setq org-agenda-redo-command
	    (list 'org-agenda-list (list 'quote arg) start-day (list 'quote span) with-hour))
      (dotimes (n (1- ndays))
	(push (1+ (car day-numbers)) day-numbers))
      (setq day-numbers (nreverse day-numbers))
      (setq clocktable-start (car day-numbers)
	    clocktable-end (1+ (or (org-last day-numbers) 0)))
      (setq-local org-starting-day (car day-numbers))
      (setq-local org-arg-loc arg)
      (setq-local org-agenda-current-span (org-agenda-ndays-to-span span))
      (unless org-agenda-compact-blocks
	(let* ((d1 (car day-numbers))
	       (d2 (org-last day-numbers))
	       (w1 (org-days-to-iso-week d1))
	       (w2 (org-days-to-iso-week d2)))
	  (setq s (point))
	  (if org-agenda-overriding-header
	      (insert (org-add-props (copy-sequence org-agenda-overriding-header)
			  nil 'face 'org-agenda-structure) "\n")
	    (insert (org-agenda-span-name span)
		    "-agenda"
		    (if (< (- d2 d1) 350)
			(if (= w1 w2)
			    (format " (W%02d)" w1)
			  (format " (W%02d-W%02d)" w1 w2))
		      "")
		    ":\n")))
	(add-text-properties s (1- (point)) (list 'face 'org-agenda-structure
						  'org-date-line t))
	(org-agenda-mark-header-line s))
      (while (setq d (pop day-numbers))
	(setq date (calendar-gregorian-from-absolute d)
	      s (point))
	(if (or (setq todayp (= d today))
		(and (not start-pos) (= d sd)))
	    (setq start-pos (point))
	  (if (and start-pos (not end-pos))
	      (setq end-pos (point))))
	(setq files thefiles
	      rtnall nil)
	(while (setq file (pop files))
	  (catch 'nextfile
	    (org-check-agenda-file file)
	    (let ((org-agenda-entry-types org-agenda-entry-types))
	      ;; Starred types override non-starred equivalents
	      (when (member :deadline* org-agenda-entry-types)
		(setq org-agenda-entry-types
		      (delq :deadline org-agenda-entry-types)))
	      (when (member :scheduled* org-agenda-entry-types)
		(setq org-agenda-entry-types
		      (delq :scheduled org-agenda-entry-types)))
	      ;; Honor with-hour
	      (when with-hour
		(when (member :deadline org-agenda-entry-types)
		  (setq org-agenda-entry-types
			(delq :deadline org-agenda-entry-types))
		  (push :deadline* org-agenda-entry-types))
		(when (member :scheduled org-agenda-entry-types)
		  (setq org-agenda-entry-types
			(delq :scheduled org-agenda-entry-types))
		  (push :scheduled* org-agenda-entry-types)))
	      (unless org-agenda-include-deadlines
		(setq org-agenda-entry-types
		      (delq :deadline* (delq :deadline org-agenda-entry-types))))
	      (cond
	       ((memq org-agenda-show-log-scoped '(only clockcheck))
		(setq rtn (org-agenda-get-day-entries
			   file date :closed)))
	       (org-agenda-show-log-scoped
		(setq rtn (apply 'org-agenda-get-day-entries
				 file date
				 (append '(:closed) org-agenda-entry-types))))
	       (t
		(setq rtn (apply 'org-agenda-get-day-entries
				 file date
				 org-agenda-entry-types)))))
	    (setq rtnall (append rtnall rtn)))) ;; all entries
	(if org-agenda-include-diary
	    (let ((org-agenda-search-headline-for-time t))
	      (require 'diary-lib)
	      (setq rtn (org-get-entries-from-diary date))
	      (setq rtnall (append rtnall rtn))))
	(if (or rtnall org-agenda-show-all-dates)
	    (progn
	      (setq day-cnt (1+ day-cnt))
	      (insert
	       (if (stringp org-agenda-format-date)
		   (format-time-string org-agenda-format-date
				       (org-time-from-absolute date))
		 (funcall org-agenda-format-date date))
	       "\n")
	      (put-text-property s (1- (point)) 'face
				 (org-agenda-get-day-face date))
	      (put-text-property s (1- (point)) 'org-date-line t)
	      (put-text-property s (1- (point)) 'org-agenda-date-header t)
	      (put-text-property s (1- (point)) 'org-day-cnt day-cnt)
	      (when todayp
		(put-text-property s (1- (point)) 'org-today t))
	      (setq rtnall
		    (org-agenda-add-time-grid-maybe rtnall ndays todayp))
	      (if rtnall (insert ;; all entries
			  (org-agenda-finalize-entries rtnall 'agenda)
			  "\n"))
	      (put-text-property s (1- (point)) 'day d)
	      (put-text-property s (1- (point)) 'org-day-cnt day-cnt))))
      (when (and org-agenda-clockreport-mode clocktable-start)
	(let ((org-agenda-files (org-agenda-files nil 'ifmode))
	      ;; the above line is to ensure the restricted range!
	      (p (copy-sequence org-agenda-clockreport-parameter-plist))
	      tbl)
	  (setq p (org-plist-delete p :block))
	  (setq p (plist-put p :tstart clocktable-start))
	  (setq p (plist-put p :tend clocktable-end))
	  (setq p (plist-put p :scope 'agenda))
	  (setq tbl (apply 'org-clock-get-clocktable p))
	  (insert tbl)))
      (goto-char (point-min))
      (or org-agenda-multi (org-agenda-fit-window-to-buffer))
      (unless (and (pos-visible-in-window-p (point-min))
		   (pos-visible-in-window-p (point-max)))
	(goto-char (1- (point-max)))
	(recenter -1)
	(if (not (pos-visible-in-window-p (or start-pos 1)))
	    (progn
	      (goto-char (or start-pos 1))
	      (recenter 1))))
      (goto-char (or start-pos 1))
      (add-text-properties (point-min) (point-max)
			   `(org-agenda-type agenda
					     org-last-args (,arg ,start-day ,span)
					     org-redo-cmd ,org-agenda-redo-command
					     org-series-cmd ,org-cmd))
      (if (eq org-agenda-show-log-scoped 'clockcheck)
	  (org-agenda-show-clocking-issues))
      (org-agenda-finalize)
      (setq buffer-read-only t)
      (message ""))))

(defun org-agenda-ndays-to-span (n)
  "Return a span symbol for a span of N days, or N if none matches."
  (cond ((symbolp n) n)
	((= n 1) 'day)
	((= n 7) 'week)
	((= n 14) 'fortnight)
	(t n)))

(defun org-agenda-span-to-ndays (span &optional start-day)
  "Return ndays from SPAN, possibly starting at START-DAY.
START-DAY is an absolute time value."
  (cond ((numberp span) span)
	((eq span 'day) 1)
	((eq span 'week) 7)
	((eq span 'fortnight) 14)
	((eq span 'month)
	 (let ((date (calendar-gregorian-from-absolute start-day)))
	   (calendar-last-day-of-month (car date) (cl-caddr date))))
	((eq span 'year)
	 (let ((date (calendar-gregorian-from-absolute start-day)))
	   (if (calendar-leap-year-p (cl-caddr date)) 366 365)))))

(defun org-agenda-span-name (span)
  "Return a SPAN name."
  (if (null span)
      ""
    (if (symbolp span)
	(capitalize (symbol-name span))
      (format "%d days" span))))

;;; Agenda word search

(defvar org-agenda-search-history nil)

(defvar org-search-syntax-table nil
  "Special syntax table for Org search.
In this table, we have single quotes not as word constituents, to
that when \"+Ameli\" is searched as a work, it will also match \"Ameli's\"")

(defvar org-mode-syntax-table) ; From org.el
(defun org-search-syntax-table ()
  (unless org-search-syntax-table
    (setq org-search-syntax-table (copy-syntax-table org-mode-syntax-table))
    (modify-syntax-entry ?' "." org-search-syntax-table)
    (modify-syntax-entry ?` "." org-search-syntax-table))
  org-search-syntax-table)

(defvar org-agenda-last-search-view-search-was-boolean nil)

;;;###autoload
(defun org-search-view (&optional todo-only string edit-at)
  "Show all entries that contain a phrase or words or regular expressions.

With optional prefix argument TODO-ONLY, only consider entries that are
TODO entries.  The argument STRING can be used to pass a default search
string into this function.  If EDIT-AT is non-nil, it means that the
user should get a chance to edit this string, with cursor at position
EDIT-AT.

The search string can be viewed either as a phrase that should be found as
is, or it can be broken into a number of snippets, each of which must match
in a Boolean way to select an entry.  The default depends on the variable
`org-agenda-search-view-always-boolean'.
Even if this is turned off (the default) you can always switch to
Boolean search dynamically by preceding the first word with  \"+\" or \"-\".

The default is a direct search of the whole phrase, where each space in
the search string can expand to an arbitrary amount of whitespace,
including newlines.

If using a Boolean search, the search string is split on whitespace and
each snippet is searched separately, with logical AND to select an entry.
Words prefixed with a minus must *not* occur in the entry.  Words without
a prefix or prefixed with a plus must occur in the entry.  Matching is
case-insensitive.  Words are enclosed by word delimiters (i.e. they must
match whole words, not parts of a word) if
`org-agenda-search-view-force-full-words' is set (default is nil).

Boolean search snippets enclosed by curly braces are interpreted as
regular expressions that must or (when preceded with \"-\") must not
match in the entry.  Snippets enclosed into double quotes will be taken
as a whole, to include whitespace.

- If the search string starts with an asterisk, search only in headlines.
- If (possibly after the leading star) the search string starts with an
  exclamation mark, this also means to look at TODO entries only, an effect
  that can also be achieved with a prefix argument.
- If (possibly after star and exclamation mark) the search string starts
  with a colon, this will mean that the (non-regexp) snippets of the
  Boolean search must match as full words.

This command searches the agenda files, and in addition the files
listed in `org-agenda-text-search-extra-files' unless a restriction lock
is active."
  (interactive "P")
  (if org-agenda-overriding-arguments
      (setq todo-only (car org-agenda-overriding-arguments)
	    string (nth 1 org-agenda-overriding-arguments)
	    edit-at (nth 2 org-agenda-overriding-arguments)))
  (let* ((props (list 'face nil
		      'done-face 'org-agenda-done
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'mouse-face 'highlight
		      'help-echo (format "mouse-2 or RET jump to location")))
	 (full-words org-agenda-search-view-force-full-words)
	 (org-agenda-text-search-extra-files org-agenda-text-search-extra-files)
	 regexp rtn rtnall files file pos inherited-tags
	 marker category level tags c neg re boolean
	 ee txt beg end words regexps+ regexps- hdl-only buffer beg1 str)
    (unless (and (not edit-at)
		 (stringp string)
		 (string-match "\\S-" string))
      (setq string (read-string
		    (if org-agenda-search-view-always-boolean
			"[+-]Word/{Regexp} ...: "
		      "Phrase or [+-]Word/{Regexp} ...: ")
		    (cond
		     ((integerp edit-at) (cons string edit-at))
		     (edit-at string))
		    'org-agenda-search-history)))
    (catch 'exit
      (if org-agenda-sticky
	  (setq org-agenda-buffer-name
		(if (stringp string)
		    (format "*Org Agenda(%s:%s)*"
			    (or org-keys (or (and todo-only "S") "s")) string)
		  (format "*Org Agenda(%s)*" (or (and todo-only "S") "s")))))
      (org-agenda-prepare "SEARCH")
      (org-compile-prefix-format 'search)
      (org-set-sorting-strategy 'search)
      (setq org-agenda-redo-command
	    (list 'org-search-view (if todo-only t nil)
		  (list 'if 'current-prefix-arg nil string)))
      (setq org-agenda-query-string string)
      (if (equal (string-to-char string) ?*)
	  (setq hdl-only t
		words (substring string 1))
	(setq words string))
      (when (equal (string-to-char words) ?!)
	(setq todo-only t
	      words (substring words 1)))
      (when (equal (string-to-char words) ?:)
	(setq full-words t
	      words (substring words 1)))
      (if (or org-agenda-search-view-always-boolean
	      (member (string-to-char words) '(?- ?+ ?\{)))
	  (setq boolean t))
      (setq words (split-string words))
      (let (www w)
	(while (setq w (pop words))
	  (while (and (string-match "\\\\\\'" w) words)
	    (setq w (concat (substring w 0 -1) " " (pop words))))
	  (push w www))
	(setq words (nreverse www) www nil)
	(while (setq w (pop words))
	  (when (and (string-match "\\`[-+]?{" w)
		     (not (string-match "}\\'" w)))
	    (while (and words (not (string-match "}\\'" (car words))))
	      (setq w (concat w " " (pop words))))
	    (setq w (concat w " " (pop words))))
	  (push w www))
	(setq words (nreverse www)))
      (setq org-agenda-last-search-view-search-was-boolean boolean)
      (when boolean
	(let (wds w)
	  (while (setq w (pop words))
	    (if (or (equal (substring w 0 1) "\"")
		    (and (> (length w) 1)
			 (member (substring w 0 1) '("+" "-"))
			 (equal (substring w 1 2) "\"")))
		(while (and words (not (equal (substring w -1) "\"")))
		  (setq w (concat w " " (pop words)))))
	    (and (string-match "\\`\\([-+]?\\)\"" w)
		 (setq w (replace-match "\\1" nil nil w)))
	    (and (equal (substring w -1) "\"") (setq w (substring w 0 -1)))
	    (push w wds))
	  (setq words (nreverse wds))))
      (if boolean
	  (mapc (lambda (w)
		  (setq c (string-to-char w))
		  (if (equal c ?-)
		      (setq neg t w (substring w 1))
		    (if (equal c ?+)
			(setq neg nil w (substring w 1))
		      (setq neg nil)))
		  (if (string-match "\\`{.*}\\'" w)
		      (setq re (substring w 1 -1))
		    (if full-words
			(setq re (concat "\\<" (regexp-quote (downcase w)) "\\>"))
		      (setq re (regexp-quote (downcase w)))))
		  (if neg (push re regexps-) (push re regexps+)))
		words)
	(push (mapconcat (lambda (w) (regexp-quote w)) words "\\s-+")
	      regexps+))
      (setq regexps+ (sort regexps+ (lambda (a b) (> (length a) (length b)))))
      (if (not regexps+)
	  (setq regexp org-outline-regexp-bol)
	(setq regexp (pop regexps+))
	(if hdl-only (setq regexp (concat org-outline-regexp-bol ".*?"
					  regexp))))
      (setq files (org-agenda-files nil 'ifmode))
      ;; Add `org-agenda-text-search-extra-files' unless there is some
      ;; restriction.
      (unless (get 'org-agenda-files 'org-restrict)
	(when (eq (car org-agenda-text-search-extra-files) 'agenda-archives)
	  (pop org-agenda-text-search-extra-files)
	  (setq files (org-add-archive-files files))))
      ;; Uniquify files.  However, let `org-check-agenda-file' handle
      ;; non-existent ones.
      (setq files (cl-remove-duplicates
		   (append files org-agenda-text-search-extra-files)
		   :test (lambda (a b)
			   (and (file-exists-p a)
				(file-exists-p b)
				(file-equal-p a b))))
	    rtnall nil)
      (while (setq file (pop files))
	(setq ee nil)
	(catch 'nextfile
	  (org-check-agenda-file file)
	  (setq buffer (if (file-exists-p file)
			   (org-get-agenda-file-buffer file)
			 (error "No such file %s" file)))
	  (if (not buffer)
	      ;; If file does not exist, make sure an error message is sent
	      (setq rtn (list (format "ORG-AGENDA-ERROR: No such org-file %s"
				      file))))
	  (with-current-buffer buffer
	    (with-syntax-table (org-search-syntax-table)
	      (unless (derived-mode-p 'org-mode)
		(error "Agenda file %s is not in `org-mode'" file))
	      (let ((case-fold-search t))
		(save-excursion
		  (save-restriction
		    (if (eq buffer org-agenda-restrict)
			(narrow-to-region org-agenda-restrict-begin
					  org-agenda-restrict-end)
		      (widen))
		    (goto-char (point-min))
		    (unless (or (org-at-heading-p)
				(outline-next-heading))
		      (throw 'nextfile t))
		    (goto-char (max (point-min) (1- (point))))
		    (while (re-search-forward regexp nil t)
		      (org-back-to-heading t)
		      (while (and (not (zerop org-agenda-search-view-max-outline-level))
				  (> (org-reduced-level (org-outline-level))
				     org-agenda-search-view-max-outline-level)
				  (forward-line -1)
				  (org-back-to-heading t)))
		      (skip-chars-forward "* ")
		      (setq beg (point-at-bol)
			    beg1 (point)
			    end (progn
				  (outline-next-heading)
				  (while (and (not (zerop org-agenda-search-view-max-outline-level))
					      (> (org-reduced-level (org-outline-level))
						 org-agenda-search-view-max-outline-level)
					      (forward-line 1)
					      (outline-next-heading)))
				  (point)))

		      (catch :skip
			(goto-char beg)
			(org-agenda-skip)
			(setq str (buffer-substring-no-properties
				   (point-at-bol)
				   (if hdl-only (point-at-eol) end)))
			(mapc (lambda (wr) (when (string-match wr str)
					(goto-char (1- end))
					(throw :skip t)))
			      regexps-)
			(mapc (lambda (wr) (unless (string-match wr str)
					(goto-char (1- end))
					(throw :skip t)))
			      (if todo-only
				  (cons (concat "^\\*+[ \t]+"
                                                org-not-done-regexp)
					regexps+)
				regexps+))
			(goto-char beg)
			(setq marker (org-agenda-new-marker (point))
			      category (org-get-category)
			      level (make-string (org-reduced-level (org-outline-level)) ? )
			      inherited-tags
			      (or (eq org-agenda-show-inherited-tags 'always)
				  (and (listp org-agenda-show-inherited-tags)
				       (memq 'todo org-agenda-show-inherited-tags))
				  (and (eq org-agenda-show-inherited-tags t)
				       (or (eq org-agenda-use-tag-inheritance t)
					   (memq 'todo org-agenda-use-tag-inheritance))))
			      tags (org-get-tags-at nil (not inherited-tags))
			      txt (org-agenda-format-item
				   ""
				   (buffer-substring-no-properties
				    beg1 (point-at-eol))
				   level category tags t))
			(org-add-props txt props
			  'org-marker marker 'org-hd-marker marker
			  'org-todo-regexp org-todo-regexp
			  'level level
			  'org-complex-heading-regexp org-complex-heading-regexp
			  'priority 1000
			  'type "search")
			(push txt ee)
			(goto-char (1- end))))))))))
	(setq rtn (nreverse ee))
	(setq rtnall (append rtnall rtn)))
      (if org-agenda-overriding-header
	  (insert (org-add-props (copy-sequence org-agenda-overriding-header)
		      nil 'face 'org-agenda-structure) "\n")
	(insert "Search words: ")
	(add-text-properties (point-min) (1- (point))
			     (list 'face 'org-agenda-structure))
	(setq pos (point))
	(insert string "\n")
	(add-text-properties pos (1- (point)) (list 'face 'org-warning))
	(setq pos (point))
	(unless org-agenda-multi
	  (insert (substitute-command-keys "\
Press `\\[org-agenda-manipulate-query-add]', \
`\\[org-agenda-manipulate-query-subtract]' to add/sub word, \
`\\[org-agenda-manipulate-query-add-re]', \
`\\[org-agenda-manipulate-query-subtract-re]' to add/sub regexp, \
`\\[universal-argument] \\[org-agenda-redo]' to edit\n"))
	  (add-text-properties pos (1- (point))
			       (list 'face 'org-agenda-structure))))
      (org-agenda-mark-header-line (point-min))
      (when rtnall
	(insert (org-agenda-finalize-entries rtnall 'search) "\n"))
      (goto-char (point-min))
      (or org-agenda-multi (org-agenda-fit-window-to-buffer))
      (add-text-properties (point-min) (point-max)
			   `(org-agenda-type search
					     org-last-args (,todo-only ,string ,edit-at)
					     org-redo-cmd ,org-agenda-redo-command
					     org-series-cmd ,org-cmd))
      (org-agenda-finalize)
      (setq buffer-read-only t))))

;;; Agenda TODO list

(defun org-agenda-propertize-selected-todo-keywords (keywords)
  "Use `org-todo-keyword-faces' for the selected todo KEYWORDS."
  (concat
   (if (or (equal keywords "ALL") (not keywords))
       (propertize "ALL" 'face 'warning)
     (mapconcat
      (lambda (kw)
        (propertize kw 'face (org-get-todo-face kw)))
      (org-split-string keywords "|")
      "|"))
   "\n"))

(defvar org-select-this-todo-keyword nil)
(defvar org-last-arg nil)

;;;###autoload
(defun org-todo-list (&optional arg)
  "Show all (not done) TODO entries from all agenda file in a single list.
The prefix arg can be used to select a specific TODO keyword and limit
the list to these.  When using `\\[universal-argument]', you will be prompted
for a keyword.  A numeric prefix directly selects the Nth keyword in
`org-todo-keywords-1'."
  (interactive "P")
  (if org-agenda-overriding-arguments
      (setq arg org-agenda-overriding-arguments))
  (if (and (stringp arg) (not (string-match "\\S-" arg))) (setq arg nil))
  (let* ((today (org-today))
	 (date (calendar-gregorian-from-absolute today))
	 (kwds org-todo-keywords-for-agenda)
	 (completion-ignore-case t)
	 (org-select-this-todo-keyword
	  (if (stringp arg) arg
	    (and arg (integerp arg) (> arg 0)
                 (nth (1- arg) kwds))))
	 rtn rtnall files file pos)
    (when (equal arg '(4))
      (setq org-select-this-todo-keyword
	    (completing-read "Keyword (or KWD1|K2D2|...): "
			     (mapcar #'list kwds) nil nil)))
    (and (equal 0 arg) (setq org-select-this-todo-keyword nil))
    (catch 'exit
      (if org-agenda-sticky
	  (setq org-agenda-buffer-name
		(if (stringp org-select-this-todo-keyword)
		    (format "*Org Agenda(%s:%s)*" (or org-keys "t")
			    org-select-this-todo-keyword)
		  (format "*Org Agenda(%s)*" (or org-keys "t")))))
      (org-agenda-prepare "TODO")
      (org-compile-prefix-format 'todo)
      (org-set-sorting-strategy 'todo)
      (setq org-agenda-redo-command
	    `(org-todo-list (or (and (numberp current-prefix-arg)
				     current-prefix-arg)
				,org-select-this-todo-keyword
				current-prefix-arg ,arg)))
      (setq files (org-agenda-files nil 'ifmode)
	    rtnall nil)
      (while (setq file (pop files))
	(catch 'nextfile
	  (org-check-agenda-file file)
	  (setq rtn (org-agenda-get-day-entries file date :todo))
	  (setq rtnall (append rtnall rtn))))
      (if org-agenda-overriding-header
	  (insert (org-add-props (copy-sequence org-agenda-overriding-header)
		      nil 'face 'org-agenda-structure) "\n")
	(insert "Global list of TODO items of type: ")
	(add-text-properties (point-min) (1- (point))
			     (list 'face 'org-agenda-structure
				   'short-heading
				   (concat "ToDo: "
					   (or org-select-this-todo-keyword "ALL"))))
	(org-agenda-mark-header-line (point-min))
	(insert (org-agenda-propertize-selected-todo-keywords
		 org-select-this-todo-keyword))
	(setq pos (point))
	(unless org-agenda-multi
	  (insert (substitute-command-keys "Available with \
`N \\[org-agenda-redo]': (0)[ALL]"))
	  (let ((n 0) s)
	    (mapc (lambda (x)
		    (setq s (format "(%d)%s" (setq n (1+ n)) x))
		    (if (> (+ (current-column) (string-width s) 1) (frame-width))
			(insert "\n                     "))
		    (insert " " s))
		  kwds))
	  (insert "\n"))
	(add-text-properties pos (1- (point)) (list 'face 'org-agenda-structure)))
      (org-agenda-mark-header-line (point-min))
      (when rtnall
	(insert (org-agenda-finalize-entries rtnall 'todo) "\n"))
      (goto-char (point-min))
      (or org-agenda-multi (org-agenda-fit-window-to-buffer))
      (add-text-properties (point-min) (point-max)
			   `(org-agenda-type todo
					     org-last-args ,arg
					     org-redo-cmd ,org-agenda-redo-command
					     org-series-cmd ,org-cmd))
      (org-agenda-finalize)
      (setq buffer-read-only t))))

;;; Agenda tags match

;;;###autoload
(defun org-tags-view (&optional todo-only match)
  "Show all headlines for all `org-agenda-files' matching a TAGS criterion.
The prefix arg TODO-ONLY limits the search to TODO entries."
  (interactive "P")
  (if org-agenda-overriding-arguments
      (setq todo-only (car org-agenda-overriding-arguments)
	    match (nth 1 org-agenda-overriding-arguments)))
  (let* ((org-tags-match-list-sublevels
	  org-tags-match-list-sublevels)
	 (completion-ignore-case t)
	 (org--matcher-tags-todo-only todo-only)
	 rtn rtnall files file pos matcher
	 buffer)
    (when (and (stringp match) (not (string-match "\\S-" match)))
      (setq match nil))
    (catch 'exit
      (if org-agenda-sticky
	  (setq org-agenda-buffer-name
		(if (stringp match)
		    (format "*Org Agenda(%s:%s)*"
			    (or org-keys (or (and todo-only "M") "m")) match)
		  (format "*Org Agenda(%s)*" (or (and todo-only "M") "m")))))
      ;; Prepare agendas (and `org-tag-alist-for-agenda') before
      ;; expanding tags within `org-make-tags-matcher'
      (org-agenda-prepare (concat "TAGS " match))
      (setq matcher (org-make-tags-matcher match)
	    match (car matcher)
	    matcher (cdr matcher))
      (org-compile-prefix-format 'tags)
      (org-set-sorting-strategy 'tags)
      (setq org-agenda-query-string match)
      (setq org-agenda-redo-command
	    (list 'org-tags-view
		  `(quote ,org--matcher-tags-todo-only)
		  `(if current-prefix-arg nil ,org-agenda-query-string)))
      (setq files (org-agenda-files nil 'ifmode)
	    rtnall nil)
      (while (setq file (pop files))
	(catch 'nextfile
	  (org-check-agenda-file file)
	  (setq buffer (if (file-exists-p file)
			   (org-get-agenda-file-buffer file)
			 (error "No such file %s" file)))
	  (if (not buffer)
	      ;; If file does not exist, error message to agenda
	      (setq rtn (list
			 (format "ORG-AGENDA-ERROR: No such org-file %s" file))
		    rtnall (append rtnall rtn))
	    (with-current-buffer buffer
	      (unless (derived-mode-p 'org-mode)
		(error "Agenda file %s is not in `org-mode'" file))
	      (save-excursion
		(save-restriction
		  (if (eq buffer org-agenda-restrict)
		      (narrow-to-region org-agenda-restrict-begin
					org-agenda-restrict-end)
		    (widen))
		  (setq rtn (org-scan-tags 'agenda
					   matcher
					   org--matcher-tags-todo-only))
		  (setq rtnall (append rtnall rtn))))))))
      (if org-agenda-overriding-header
	  (insert (org-add-props (copy-sequence org-agenda-overriding-header)
		      nil 'face 'org-agenda-structure) "\n")
	(insert "Headlines with TAGS match: ")
	(add-text-properties (point-min) (1- (point))
			     (list 'face 'org-agenda-structure
				   'short-heading
				   (concat "Match: " match)))
	(setq pos (point))
	(insert match "\n")
	(add-text-properties pos (1- (point)) (list 'face 'org-warning))
	(setq pos (point))
	(unless org-agenda-multi
	  (insert (substitute-command-keys
		   "Press `\\[universal-argument] \\[org-agenda-redo]' \
to search again with new search string\n")))
	(add-text-properties pos (1- (point))
			     (list 'face 'org-agenda-structure)))
      (org-agenda-mark-header-line (point-min))
      (when rtnall
	(insert (org-agenda-finalize-entries rtnall 'tags) "\n"))
      (goto-char (point-min))
      (or org-agenda-multi (org-agenda-fit-window-to-buffer))
      (add-text-properties
       (point-min) (point-max)
       `(org-agenda-type tags
			 org-last-args (,org--matcher-tags-todo-only ,match)
			 org-redo-cmd ,org-agenda-redo-command
			 org-series-cmd ,org-cmd))
      (org-agenda-finalize)
      (setq buffer-read-only t))))

;;; Agenda Finding stuck projects

(defvar org-agenda-skip-regexp nil
  "Regular expression used in skipping subtrees for the agenda.
This is basically a temporary global variable that can be set and then
used by user-defined selections using `org-agenda-skip-function'.")

(defvar org-agenda-overriding-header nil
  "When set during agenda, todo and tags searches it replaces the header.
This variable should not be set directly, but custom commands can bind it
in the options section.")

(defun org-agenda-skip-entry-if (&rest conditions)
  "Skip entry if any of CONDITIONS is true.
See `org-agenda-skip-if' for details."
  (org-agenda-skip-if nil conditions))

(defun org-agenda-skip-subtree-if (&rest conditions)
  "Skip subtree if any of CONDITIONS is true.
See `org-agenda-skip-if' for details."
  (org-agenda-skip-if t conditions))

(defun org-agenda-skip-if (subtree conditions)
  "Checks current entity for CONDITIONS.
If SUBTREE is non-nil, the entire subtree is checked.  Otherwise, only
the entry (i.e. the text before the next heading) is checked.

CONDITIONS is a list of symbols, boolean OR is used to combine the results
from different tests.  Valid conditions are:

scheduled     Check if there is a scheduled cookie
notscheduled  Check if there is no scheduled cookie
deadline      Check if there is a deadline
notdeadline   Check if there is no deadline
timestamp     Check if there is a timestamp (also deadline or scheduled)
nottimestamp  Check if there is no timestamp (also deadline or scheduled)
regexp        Check if regexp matches
notregexp     Check if regexp does not match.
todo          Check if TODO keyword matches
nottodo       Check if TODO keyword does not match

The regexp is taken from the conditions list, it must come right after
the `regexp' or `notregexp' element.

`todo' and `nottodo' accept as an argument a list of todo
keywords, which may include \"*\" to match any todo keyword.

    (org-agenda-skip-entry-if \\='todo \\='(\"TODO\" \"WAITING\"))

would skip all entries with \"TODO\" or \"WAITING\" keywords.

Instead of a list, a keyword class may be given.  For example:

    (org-agenda-skip-entry-if \\='nottodo \\='done)

would skip entries that haven't been marked with any of \"DONE\"
keywords.  Possible classes are: `todo', `done', `any'.

If any of these conditions is met, this function returns the end point of
the entity, causing the search to continue from there.  This is a function
that can be put into `org-agenda-skip-function' for the duration of a command."
  (org-back-to-heading t)
  (let* ((beg (point))
	 (end (if subtree (save-excursion (org-end-of-subtree t) (point))
		(org-entry-end-position)))
	 (planning-end (if subtree end (line-end-position 2)))
	 m)
    (and
     (or (and (memq 'scheduled conditions)
	      (re-search-forward org-scheduled-time-regexp planning-end t))
	 (and (memq 'notscheduled conditions)
	      (not
	       (save-excursion
		 (re-search-forward org-scheduled-time-regexp planning-end t))))
	 (and (memq 'deadline conditions)
	      (re-search-forward org-deadline-time-regexp planning-end t))
	 (and (memq 'notdeadline conditions)
	      (not
	       (save-excursion
		 (re-search-forward org-deadline-time-regexp planning-end t))))
	 (and (memq 'timestamp conditions)
	      (re-search-forward org-ts-regexp end t))
	 (and (memq 'nottimestamp conditions)
	      (not (save-excursion (re-search-forward org-ts-regexp end t))))
	 (and (setq m (memq 'regexp conditions))
	      (stringp (nth 1 m))
	      (re-search-forward (nth 1 m) end t))
	 (and (setq m (memq 'notregexp conditions))
	      (stringp (nth 1 m))
	      (not (save-excursion (re-search-forward (nth 1 m) end t))))
	 (and (or
	       (setq m (memq 'nottodo conditions))
	       (setq m (memq 'todo-unblocked conditions))
	       (setq m (memq 'nottodo-unblocked conditions))
	       (setq m (memq 'todo conditions)))
	      (org-agenda-skip-if-todo m end)))
     end)))

(defun org-agenda-skip-if-todo (args end)
  "Helper function for `org-agenda-skip-if', do not use it directly.
ARGS is a list with first element either `todo', `nottodo',
`todo-unblocked' or `nottodo-unblocked'.  The remainder is either
a list of TODO keywords, or a state symbol `todo' or `done' or
`any'."
  (let ((todo-re
	 (concat "^\\*+[ \t]+"
		 (regexp-opt
		  (pcase args
		    (`(,_ todo)
		     (org-delete-all org-done-keywords
				     (copy-sequence org-todo-keywords-1)))
		    (`(,_ done) org-done-keywords)
		    (`(,_ any) org-todo-keywords-1)
		    (`(,_ ,(pred atom))
		     (error "Invalid TODO class or type: %S" args))
		    (`(,_ ,(pred (member "*"))) org-todo-keywords-1)
		    (`(,_ ,todo-list) todo-list))
		  'words))))
    (pcase args
      (`(todo . ,_)
       (let (case-fold-search) (re-search-forward todo-re end t)))
      (`(nottodo . ,_)
       (not (let (case-fold-search) (re-search-forward todo-re end t))))
      (`(todo-unblocked . ,_)
       (catch :unblocked
	 (while (let (case-fold-search) (re-search-forward todo-re end t))
	   (when (org-entry-blocked-p) (throw :unblocked t)))
	 nil))
      (`(nottodo-unblocked . ,_)
       (catch :unblocked
	 (while (let (case-fold-search) (re-search-forward todo-re end t))
	   (when (org-entry-blocked-p) (throw :unblocked nil)))
	 t))
      (`(,type . ,_) (error "Unknown TODO skip type: %S" type)))))

;;;###autoload
(defun org-agenda-list-stuck-projects (&rest ignore)
  "Create agenda view for projects that are stuck.
Stuck projects are project that have no next actions.  For the definitions
of what a project is and how to check if it stuck, customize the variable
`org-stuck-projects'."
  (interactive)
  (let* ((org-agenda-overriding-header
	  (or org-agenda-overriding-header "List of stuck projects: "))
	 (matcher (nth 0 org-stuck-projects))
	 (todo (nth 1 org-stuck-projects))
	 (tags (nth 2 org-stuck-projects))
	 (gen-re (org-string-nw-p (nth 3 org-stuck-projects)))
	 (todo-wds
	  (if (not (member "*" todo)) todo
	    (org-agenda-prepare-buffers (org-agenda-files nil 'ifmode))
	    (org-delete-all org-done-keywords-for-agenda
			    (copy-sequence org-todo-keywords-for-agenda))))
	 (todo-re (and todo
		       (format "^\\*+[ \t]+\\(%s\\)\\>"
			       (mapconcat #'identity todo-wds "\\|"))))
	 (tags-re (cond ((null tags) nil)
			((member "*" tags)
			 (eval-when-compile
			   (concat org-outline-regexp-bol
				   ".*:[[:alnum:]_@#%]+:[ \t]*$")))
			(tags (concat org-outline-regexp-bol
				      ".*:\\("
				      (mapconcat #'identity tags "\\|")
				      "\\):[[:alnum:]_@#%:]*[ \t]*$"))
			(t nil)))
	 (re-list (delq nil (list todo-re tags-re gen-re)))
	 (skip-re
	  (if (null re-list)
	      (error "Missing information to identify unstuck projects")
	    (mapconcat #'identity re-list "\\|")))
	 (org-agenda-skip-function
	  ;; Skip entry if `org-agenda-skip-regexp' matches anywhere
	  ;; in the subtree.
	  `(lambda ()
	     (and (save-excursion
		    (let ((case-fold-search nil))
		      (re-search-forward
		       ,skip-re (save-excursion (org-end-of-subtree t)) t)))
		  (progn (outline-next-heading) (point))))))
    (org-tags-view nil matcher)
    (setq org-agenda-buffer-name (buffer-name))
    (with-current-buffer org-agenda-buffer-name
      (setq org-agenda-redo-command
	    `(org-agenda-list-stuck-projects ,current-prefix-arg))
      (let ((inhibit-read-only t))
        (add-text-properties
         (point-min) (point-max)
         `(org-redo-cmd ,org-agenda-redo-command))))))

;;; Diary integration

(defvar org-disable-agenda-to-diary nil)          ;Dynamically-scoped param.
(defvar diary-list-entries-hook)
(defvar diary-time-regexp)
(defun org-get-entries-from-diary (date)
  "Get the (Emacs Calendar) diary entries for DATE."
  (require 'diary-lib)
  (let* ((diary-fancy-buffer "*temporary-fancy-diary-buffer*")
	 (diary-display-function 'diary-fancy-display)
	 (pop-up-frames nil)
	 (diary-list-entries-hook
	  (cons 'org-diary-default-entry diary-list-entries-hook))
	 (diary-file-name-prefix nil) ; turn this feature off
	 (diary-modify-entry-list-string-function 'org-modify-diary-entry-string)
	 entries
	 (org-disable-agenda-to-diary t))
    (save-excursion
      (save-window-excursion
	(funcall (if (fboundp 'diary-list-entries)
		     'diary-list-entries 'list-diary-entries)
		 date 1)))
    (if (not (get-buffer diary-fancy-buffer))
	(setq entries nil)
      (with-current-buffer diary-fancy-buffer
	(setq buffer-read-only nil)
	(if (zerop (buffer-size))
	    ;; No entries
	    (setq entries nil)
	  ;; Omit the date and other unnecessary stuff
	  (org-agenda-cleanup-fancy-diary)
	  ;; Add prefix to each line and extend the text properties
	  (if (zerop (buffer-size))
	      (setq entries nil)
	    (setq entries (buffer-substring (point-min) (- (point-max) 1)))
	    (setq entries
		  (with-temp-buffer
		    (insert entries) (goto-char (point-min))
		    (while (re-search-forward "\n[ \t]+\\(.+\\)$" nil t)
		      (unless (save-match-data (string-match diary-time-regexp (match-string 1)))
			(replace-match (concat "; " (match-string 1)))))
		    (buffer-string)))))
	(set-buffer-modified-p nil)
	(kill-buffer diary-fancy-buffer)))
    (when entries
      (setq entries (org-split-string entries "\n"))
      (setq entries
	    (mapcar
	     (lambda (x)
	       (setq x (org-agenda-format-item "" x nil "Diary" nil 'time))
	       ;; Extend the text properties to the beginning of the line
	       (org-add-props x (text-properties-at (1- (length x)) x)
		 'type "diary" 'date date 'face 'org-agenda-diary))
	     entries)))))

(defvar org-agenda-cleanup-fancy-diary-hook nil
  "Hook run when the fancy diary buffer is cleaned up.")

(defun org-agenda-cleanup-fancy-diary ()
  "Remove unwanted stuff in buffer created by `fancy-diary-display'.
This gets rid of the date, the underline under the date, and
the dummy entry installed by `org-mode' to ensure non-empty diary for each
date.  It also removes lines that contain only whitespace."
  (goto-char (point-min))
  (if (looking-at ".*?:[ \t]*")
      (progn
	(replace-match "")
	(re-search-forward "\n=+$" nil t)
	(replace-match "")
	(while (re-search-backward "^ +\n?" nil t) (replace-match "")))
    (re-search-forward "\n=+$" nil t)
    (delete-region (point-min) (min (point-max) (1+ (match-end 0)))))
  (goto-char (point-min))
  (while (re-search-forward "^ +\n" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (if (re-search-forward "^Org mode dummy\n?" nil t)
      (replace-match ""))
  (run-hooks 'org-agenda-cleanup-fancy-diary-hook))

;; Make sure entries from the diary have the right text properties.
(eval-after-load "diary-lib"
  '(if (boundp 'diary-modify-entry-list-string-function)
       ;; We can rely on the hook, nothing to do
       nil
     ;; Hook not available, must use advice to make this work
     (defadvice add-to-diary-list (before org-mark-diary-entry activate)
       "Make the position visible."
       (if (and org-disable-agenda-to-diary  ;; called from org-agenda
		(stringp string)
		buffer-file-name)
	   (setq string (org-modify-diary-entry-string string))))))

(defun org-modify-diary-entry-string (string)
  "Add text properties to string, allowing Org to act on it."
  (org-add-props string nil
    'mouse-face 'highlight
    'help-echo (if buffer-file-name
		   (format "mouse-2 or RET jump to diary file %s"
			   (abbreviate-file-name buffer-file-name))
		 "")
    'org-agenda-diary-link t
    'org-marker (org-agenda-new-marker (point-at-bol))))

(defun org-diary-default-entry ()
  "Add a dummy entry to the diary.
Needed to avoid empty dates which mess up holiday display."
  ;; Catch the error if dealing with the new add-to-diary-alist
  (when org-disable-agenda-to-diary
    (condition-case nil
	(org-add-to-diary-list original-date "Org mode dummy" "")
      (error
       (org-add-to-diary-list original-date  "Org mode dummy" "" nil)))))

(defun org-add-to-diary-list (&rest args)
  (if (fboundp 'diary-add-to-list)
      (apply 'diary-add-to-list args)
    (apply 'add-to-diary-list args)))

(defvar org-diary-last-run-time nil)

;;;###autoload
(defun org-diary (&rest args)
  "Return diary information from org files.
This function can be used in a \"sexp\" diary entry in the Emacs calendar.
It accesses org files and extracts information from those files to be
listed in the diary.  The function accepts arguments specifying what
items should be listed.  For a list of arguments allowed here, see the
variable `org-agenda-entry-types'.

The call in the diary file should look like this:

   &%%(org-diary) ~/path/to/some/orgfile.org

Use a separate line for each org file to check.  Or, if you omit the file name,
all files listed in `org-agenda-files' will be checked automatically:

   &%%(org-diary)

If you don't give any arguments (as in the example above), the default value
of `org-agenda-entry-types' is used: (:deadline :scheduled :timestamp :sexp).
So the example above may also be written as

   &%%(org-diary :deadline :timestamp :sexp :scheduled)

The function expects the lisp variables `entry' and `date' to be provided
by the caller, because this is how the calendar works.  Don't use this
function from a program - use `org-agenda-get-day-entries' instead."
  (when (> (- (float-time)
	      org-agenda-last-marker-time)
	   5)
    ;; I am not sure if this works with sticky agendas, because the marker
    ;; list is then no longer a global variable.
    (org-agenda-reset-markers))
  (org-compile-prefix-format 'agenda)
  (org-set-sorting-strategy 'agenda)
  (setq args (or args org-agenda-entry-types))
  (let* ((files (if (and entry (stringp entry) (string-match "\\S-" entry))
		    (list entry)
		  (org-agenda-files t)))
	 (time (float-time))
	 file rtn results)
    (when (or (not org-diary-last-run-time)
	      (> (- time
		    org-diary-last-run-time)
		 3))
      (org-agenda-prepare-buffers files))
    (setq org-diary-last-run-time time)
    ;; If this is called during org-agenda, don't return any entries to
    ;; the calendar.  Org Agenda will list these entries itself.
    (if org-disable-agenda-to-diary (setq files nil))
    (while (setq file (pop files))
      (setq rtn (apply 'org-agenda-get-day-entries file date args))
      (setq results (append results rtn)))
    (when results
      (setq results
	    (mapcar (lambda (i) (replace-regexp-in-string
				 org-bracket-link-regexp "\\3" i)) results))
      (concat (org-agenda-finalize-entries results) "\n"))))

;;; Agenda entry finders

(defun org-agenda--timestamp-to-absolute (&rest args)
  "Call `org-time-string-to-absolute' with ARGS.
However, throw `:skip' whenever an error is raised."
  (condition-case e
      (apply #'org-time-string-to-absolute args)
    (org-diary-sexp-no-match (throw :skip nil))
    (error
     (message "%s; Skipping entry" (error-message-string e))
     (throw :skip nil))))

(defun org-agenda-get-day-entries (file date &rest args)
  "Does the work for `org-diary' and `org-agenda'.
FILE is the path to a file to be checked for entries.  DATE is date like
the one returned by `calendar-current-date'.  ARGS are symbols indicating
which kind of entries should be extracted.  For details about these, see
the documentation of `org-diary'."
  (let* ((org-startup-folded nil)
	 (org-startup-align-all-tables nil)
	 (buffer (if (file-exists-p file) (org-get-agenda-file-buffer file)
		   (error "No such file %s" file))))
    (if (not buffer)
	;; If file does not exist, signal it in diary nonetheless.
	(list (format "ORG-AGENDA-ERROR: No such org-file %s" file))
      (with-current-buffer buffer
	(unless (derived-mode-p 'org-mode)
	  (error "Agenda file %s is not in `org-mode'" file))
	(setq org-agenda-buffer (or org-agenda-buffer buffer))
	(setf org-agenda-current-date date)
	(save-excursion
	  (save-restriction
	    (if (eq buffer org-agenda-restrict)
		(narrow-to-region org-agenda-restrict-begin
				  org-agenda-restrict-end)
	      (widen))
	    ;; Rationalize ARGS.  Also make sure `:deadline' comes
	    ;; first in order to populate DEADLINES before passing it.
	    ;;
	    ;; We use `delq' since `org-uniquify' duplicates ARGS,
	    ;; guarding us from modifying `org-agenda-entry-types'.
	    (setf args (org-uniquify (or args org-agenda-entry-types)))
	    (when (and (memq :scheduled args) (memq :scheduled* args))
	      (setf args (delq :scheduled* args)))
	    (cond
	     ((memq :deadline args)
	      (setf args (cons :deadline
			       (delq :deadline (delq :deadline* args)))))
	     ((memq :deadline* args)
	      (setf args (cons :deadline* (delq :deadline* args)))))
	    ;; Collect list of headlines.  Return them flattened.
	    (let ((case-fold-search nil) results deadlines)
	      (dolist (arg args (apply #'nconc (nreverse results)))
		(pcase arg
		  ((and :todo (guard (org-agenda-today-p date)))
		   (push (org-agenda-get-todos) results))
		  (:timestamp
		   (push (org-agenda-get-blocks) results)
		   (push (org-agenda-get-timestamps deadlines) results))
		  (:sexp
		   (push (org-agenda-get-sexps) results))
		  (:scheduled
		   (push (org-agenda-get-scheduled deadlines) results))
		  (:scheduled*
		   (push (org-agenda-get-scheduled deadlines t) results))
		  (:closed
		   (push (org-agenda-get-progress) results))
		  (:deadline
		   (setf deadlines (org-agenda-get-deadlines))
		   (push deadlines results))
		  (:deadline*
		   (setf deadlines (org-agenda-get-deadlines t))
		   (push deadlines results)))))))))))

(defsubst org-em (x y list)
  "Is X or Y a member of LIST?"
  (or (memq x list) (memq y list)))

(defvar org-heading-keyword-regexp-format) ; defined in org.el
(defvar org-agenda-sorting-strategy-selected nil)

(defun org-agenda-entry-get-agenda-timestamp (pom)
  "Retrieve timestamp information for sorting agenda views.
Given a point or marker POM, returns a cons cell of the timestamp
and the timestamp type relevant for the sorting strategy in
`org-agenda-sorting-strategy-selected'."
  (let (ts ts-date-type)
    (save-match-data
      (cond ((org-em 'scheduled-up 'scheduled-down
		     org-agenda-sorting-strategy-selected)
	     (setq ts (org-entry-get pom "SCHEDULED")
		   ts-date-type " scheduled"))
	    ((org-em 'deadline-up 'deadline-down
		     org-agenda-sorting-strategy-selected)
	     (setq ts (org-entry-get pom "DEADLINE")
		   ts-date-type " deadline"))
	    ((org-em 'ts-up 'ts-down
		     org-agenda-sorting-strategy-selected)
	     (setq ts (org-entry-get pom "TIMESTAMP")
		   ts-date-type " timestamp"))
	    ((org-em 'tsia-up 'tsia-down
		     org-agenda-sorting-strategy-selected)
	     (setq ts (org-entry-get pom "TIMESTAMP_IA")
		   ts-date-type " timestamp_ia"))
	    ((org-em 'timestamp-up 'timestamp-down
		     org-agenda-sorting-strategy-selected)
	     (setq ts (or (org-entry-get pom "SCHEDULED")
			  (org-entry-get pom "DEADLINE")
			  (org-entry-get pom "TIMESTAMP")
			  (org-entry-get pom "TIMESTAMP_IA"))
		   ts-date-type ""))
	    (t (setq ts-date-type "")))
      (cons (when ts (ignore-errors (org-time-string-to-absolute ts)))
	    ts-date-type))))

(defun org-agenda-get-todos ()
  "Return the TODO information for agenda display."
  (let* ((props (list 'face nil
		      'done-face 'org-agenda-done
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'mouse-face 'highlight
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (case-fold-search nil)
	 (regexp (format org-heading-keyword-regexp-format
			 (cond
			  ((and org-select-this-todo-keyword
				(equal org-select-this-todo-keyword "*"))
			   org-todo-regexp)
			  (org-select-this-todo-keyword
			   (concat "\\("
				   (mapconcat 'identity
					      (org-split-string
					       org-select-this-todo-keyword
					       "|")
					      "\\|") "\\)"))
			  (t org-not-done-regexp))))
	 marker priority category level tags todo-state
	 ts-date ts-date-type ts-date-pair
	 ee txt beg end inherited-tags todo-state-end-pos)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(save-match-data
	  (beginning-of-line)
	  (org-agenda-skip)
	  (setq beg (point) end (save-excursion (outline-next-heading) (point)))
	  (unless (and (setq todo-state (org-get-todo-state))
		       (setq todo-state-end-pos (match-end 2)))
	    (goto-char end)
	    (throw :skip nil))
	  (when (org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item end)
	    (goto-char (1+ beg))
	    (or org-agenda-todo-list-sublevels (org-end-of-subtree 'invisible))
	    (throw :skip nil)))
	(goto-char (match-beginning 2))
	(setq marker (org-agenda-new-marker (match-beginning 0))
	      category (org-get-category)
	      ts-date-pair (org-agenda-entry-get-agenda-timestamp (point))
	      ts-date (car ts-date-pair)
	      ts-date-type (cdr ts-date-pair)
	      txt (org-trim (buffer-substring (match-beginning 2) (match-end 0)))
	      inherited-tags
	      (or (eq org-agenda-show-inherited-tags 'always)
		  (and (listp org-agenda-show-inherited-tags)
		       (memq 'todo org-agenda-show-inherited-tags))
		  (and (eq org-agenda-show-inherited-tags t)
		       (or (eq org-agenda-use-tag-inheritance t)
			   (memq 'todo org-agenda-use-tag-inheritance))))
	      tags (org-get-tags-at nil (not inherited-tags))
	      level (make-string (org-reduced-level (org-outline-level)) ? )
	      txt (org-agenda-format-item "" txt level category tags t)
	      priority (1+ (org-get-priority txt)))
	(org-add-props txt props
	  'org-marker marker 'org-hd-marker marker
	  'priority priority
	  'level level
	  'ts-date ts-date
	  'type (concat "todo" ts-date-type) 'todo-state todo-state)
	(push txt ee)
	(if org-agenda-todo-list-sublevels
	    (goto-char todo-state-end-pos)
	  (org-end-of-subtree 'invisible))))
    (nreverse ee)))

(defun org-agenda-todo-custom-ignore-p (time n)
  "Check whether timestamp is farther away than n number of days.
This function is invoked if `org-agenda-todo-ignore-deadlines',
`org-agenda-todo-ignore-scheduled' or
`org-agenda-todo-ignore-timestamp' is set to an integer."
  (let ((days (org-time-stamp-to-now
	       time org-agenda-todo-ignore-time-comparison-use-seconds)))
    (if (>= n 0)
	(>= days n)
      (<= days n))))

;;;###autoload
(defun org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item
    (&optional end)
  "Do we have a reason to ignore this TODO entry because it has a time stamp?"
  (when (or org-agenda-todo-ignore-with-date
	    org-agenda-todo-ignore-scheduled
	    org-agenda-todo-ignore-deadlines
	    org-agenda-todo-ignore-timestamp)
    (setq end (or end (save-excursion (outline-next-heading) (point))))
    (save-excursion
      (or (and org-agenda-todo-ignore-with-date
	       (re-search-forward org-ts-regexp end t))
	  (and org-agenda-todo-ignore-scheduled
	       (re-search-forward org-scheduled-time-regexp end t)
	       (cond
		((eq org-agenda-todo-ignore-scheduled 'future)
		 (> (org-time-stamp-to-now
		     (match-string 1) org-agenda-todo-ignore-time-comparison-use-seconds) 0))
		((eq org-agenda-todo-ignore-scheduled 'past)
		 (<= (org-time-stamp-to-now
		      (match-string 1) org-agenda-todo-ignore-time-comparison-use-seconds) 0))
		((numberp org-agenda-todo-ignore-scheduled)
		 (org-agenda-todo-custom-ignore-p
		  (match-string 1) org-agenda-todo-ignore-scheduled))
		(t)))
	  (and org-agenda-todo-ignore-deadlines
	       (re-search-forward org-deadline-time-regexp end t)
	       (cond
		((memq org-agenda-todo-ignore-deadlines '(t all)) t)
		((eq org-agenda-todo-ignore-deadlines 'far)
		 (not (org-deadline-close-p (match-string 1))))
		((eq org-agenda-todo-ignore-deadlines 'future)
		 (> (org-time-stamp-to-now
		     (match-string 1) org-agenda-todo-ignore-time-comparison-use-seconds) 0))
		((eq org-agenda-todo-ignore-deadlines 'past)
		 (<= (org-time-stamp-to-now
		      (match-string 1) org-agenda-todo-ignore-time-comparison-use-seconds) 0))
		((numberp org-agenda-todo-ignore-deadlines)
		 (org-agenda-todo-custom-ignore-p
		  (match-string 1) org-agenda-todo-ignore-deadlines))
		(t (org-deadline-close-p (match-string 1)))))
	  (and org-agenda-todo-ignore-timestamp
	       (let ((buffer (current-buffer))
		     (regexp
		      (concat
		       org-scheduled-time-regexp "\\|" org-deadline-time-regexp))
		     (start (point)))
		 ;; Copy current buffer into a temporary one
		 (with-temp-buffer
		   (insert-buffer-substring buffer start end)
		   (goto-char (point-min))
		   ;; Delete SCHEDULED and DEADLINE items
		   (while (re-search-forward regexp end t)
		     (delete-region (match-beginning 0) (match-end 0)))
		   (goto-char (point-min))
		   ;; No search for timestamp left
		   (when (re-search-forward org-ts-regexp nil t)
		     (cond
		      ((eq org-agenda-todo-ignore-timestamp 'future)
		       (> (org-time-stamp-to-now
			   (match-string 1) org-agenda-todo-ignore-time-comparison-use-seconds) 0))
		      ((eq org-agenda-todo-ignore-timestamp 'past)
		       (<= (org-time-stamp-to-now
			    (match-string 1) org-agenda-todo-ignore-time-comparison-use-seconds) 0))
		      ((numberp org-agenda-todo-ignore-timestamp)
		       (org-agenda-todo-custom-ignore-p
			(match-string 1) org-agenda-todo-ignore-timestamp))
		      (t))))))))))

(defun org-agenda-get-timestamps (&optional deadlines)
  "Return the date stamp information for agenda display.
Optional argument DEADLINES is a list of deadline items to be
displayed in agenda view."
  (let* ((props (list 'face 'org-agenda-calendar-event
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'mouse-face 'highlight
		      'help-echo
		      (format "mouse-2 or RET jump to Org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (current (calendar-absolute-from-gregorian date))
	 (today (org-today))
	 (deadline-position-alist
	  (mapcar (lambda (d)
		    (let ((m (get-text-property 0 'org-hd-marker d)))
		      (and m (marker-position m))))
		  deadlines))
	 ;; Match time-stamps set to current date, time-stamps with
	 ;; a repeater, and S-exp time-stamps.
	 (regexp
	  (concat
	   (if org-agenda-include-inactive-timestamps "[[<]" "<")
	   (regexp-quote
	    (substring
	     (format-time-string
	      (car org-time-stamp-formats)
	      (apply #'encode-time	; DATE bound by calendar
		     (list 0 0 0 (nth 1 date) (car date) (nth 2 date))))
	     1 11))
	   "\\|\\(<[0-9]+-[0-9]+-[0-9]+[^>\n]+?\\+[0-9]+[hdwmy]>\\)"
	   "\\|\\(<%%\\(([^>\n]+)\\)>\\)"))
	 timestamp-items)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      ;; Skip date ranges, scheduled and deadlines, which are handled
      ;; specially.  Also skip time-stamps before first headline as
      ;; there would be no entry to add to the agenda.  Eventually,
      ;; ignore clock entries.
      (catch :skip
	(save-match-data
	  (when (or (org-at-date-range-p)
		    (org-at-planning-p)
		    (org-before-first-heading-p)
		    (and org-agenda-include-inactive-timestamps
			 (org-at-clock-log-p)))
	    (throw :skip nil))
	  (org-agenda-skip))
	(let* ((pos (match-beginning 0))
	       (repeat (match-string 1))
	       (sexp-entry (match-string 3))
	       (time-stamp (if (or repeat sexp-entry) (match-string 0)
			     (save-excursion
			       (goto-char pos)
			       (looking-at org-ts-regexp-both)
			       (match-string 0))))
	       (todo-state (org-get-todo-state))
	       (warntime (get-text-property (point) 'org-appt-warntime))
	       (done? (member todo-state org-done-keywords)))
	  ;; Possibly skip done tasks.
	  (when (and done? org-agenda-skip-timestamp-if-done)
	    (throw :skip t))
	  ;; S-exp entry doesn't match current day: skip it.
	  (when (and sexp-entry (not (org-diary-sexp-entry sexp-entry "" date)))
	    (throw :skip nil))
	  (when repeat
	    (let* ((past
		    ;; A repeating time stamp is shown at its base
		    ;; date and every repeated date up to TODAY.  If
		    ;; `org-agenda-prefer-last-repeat' is non-nil,
		    ;; however, only the last repeat before today
		    ;; (inclusive) is shown.
		    (org-agenda--timestamp-to-absolute
		     repeat
		     (if (or (> current today)
			     (eq org-agenda-prefer-last-repeat t)
			     (member todo-state org-agenda-prefer-last-repeat))
			 today
		       current)
		     'past (current-buffer) pos))
		   (future
		    ;;  Display every repeated date past TODAY
		    ;;  (exclusive) unless
		    ;;  `org-agenda-show-future-repeats' is nil.  If
		    ;;  this variable is set to `next', only display
		    ;;  the first repeated date after TODAY
		    ;;  (exclusive).
		    (cond
		     ((<= current today) past)
		     ((not org-agenda-show-future-repeats) past)
		     (t
		      (let ((base (if (eq org-agenda-show-future-repeats 'next)
				      (1+ today)
				    current)))
			(org-agenda--timestamp-to-absolute
			 repeat base 'future (current-buffer) pos))))))
	      (when (and (/= current past) (/= current future))
		(throw :skip nil))))
	  (save-excursion
	    (re-search-backward org-outline-regexp-bol nil t)
	    ;; Possibly skip time-stamp when a deadline is set.
	    (when (and org-agenda-skip-timestamp-if-deadline-is-shown
		       (assq (point) deadline-position-alist))
	      (throw :skip nil))
	    (let* ((category (org-get-category pos))
		   (inherited-tags
		    (or (eq org-agenda-show-inherited-tags 'always)
			(and (consp org-agenda-show-inherited-tags)
			     (memq 'agenda org-agenda-show-inherited-tags))
			(and (eq org-agenda-show-inherited-tags t)
			     (or (eq org-agenda-use-tag-inheritance t)
				 (memq 'agenda
				       org-agenda-use-tag-inheritance)))))
		   (tags (org-get-tags-at nil (not inherited-tags)))
		   (level (make-string (org-reduced-level (org-outline-level))
				       ?\s))
		   (head (and (looking-at "\\*+[ \t]+\\(.*\\)")
			      (match-string 1)))
		   (inactive? (= (char-after pos) ?\[))
		   (habit? (and (fboundp 'org-is-habit-p) (org-is-habit-p)))
		   (item
		    (org-agenda-format-item
		     (and inactive? org-agenda-inactive-leader)
		     head level category tags time-stamp org-ts-regexp habit?)))
	      (org-add-props item props
		'priority (if habit?
			      (org-habit-get-priority (org-habit-parse-todo))
			    (org-get-priority item))
		'org-marker (org-agenda-new-marker pos)
		'org-hd-marker (org-agenda-new-marker)
		'date date
		'level level
		'ts-date (if repeat (org-agenda--timestamp-to-absolute repeat)
			   current)
		'todo-state todo-state
		'warntime warntime
		'type "timestamp")
	      (push item timestamp-items))))
	(when org-agenda-skip-additional-timestamps-same-entry
	  (outline-next-heading))))
    (nreverse timestamp-items)))

(defun org-agenda-get-sexps ()
  "Return the sexp information for agenda display."
  (require 'diary-lib)
  (let* ((props (list 'face 'org-agenda-calendar-sexp
		      'mouse-face 'highlight
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp "^&?%%(")
	 marker category extra level ee txt tags entry
	 result beg b sexp sexp-entry todo-state warntime inherited-tags)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(org-agenda-skip)
	(setq beg (match-beginning 0))
	(goto-char (1- (match-end 0)))
	(setq b (point))
	(forward-sexp 1)
	(setq sexp (buffer-substring b (point)))
	(setq sexp-entry (if (looking-at "[ \t]*\\(\\S-.*\\)")
			     (org-trim (match-string 1))
			   ""))
	(setq result (org-diary-sexp-entry sexp sexp-entry date))
	(when result
	  (setq marker (org-agenda-new-marker beg)
		level (make-string (org-reduced-level (org-outline-level)) ? )
		category (org-get-category beg)
		inherited-tags
		(or (eq org-agenda-show-inherited-tags 'always)
		    (and (listp org-agenda-show-inherited-tags)
			 (memq 'agenda org-agenda-show-inherited-tags))
		    (and (eq org-agenda-show-inherited-tags t)
			 (or (eq org-agenda-use-tag-inheritance t)
			     (memq 'agenda org-agenda-use-tag-inheritance))))
		tags (org-get-tags-at nil (not inherited-tags))
		todo-state (org-get-todo-state)
		warntime (get-text-property (point) 'org-appt-warntime)
		extra nil)

	  (dolist (r (if (stringp result)
			 (list result)
		       result)) ;; we expect a list here
	    (when (and org-agenda-diary-sexp-prefix
		       (string-match org-agenda-diary-sexp-prefix r))
	      (setq extra (match-string 0 r)
		    r (replace-match "" nil nil r)))
	    (if (string-match "\\S-" r)
		(setq txt r)
	      (setq txt "SEXP entry returned empty string"))
	    (setq txt (org-agenda-format-item extra txt level category tags 'time))
	    (org-add-props txt props 'org-marker marker
			   'date date 'todo-state todo-state
			   'level level 'type "sexp" 'warntime warntime)
	    (push txt ee)))))
    (nreverse ee)))

;; Calendar sanity: define some functions that are independent of
;; `calendar-date-style'.
(defun org-anniversary (year month day &optional mark)
  "Like `diary-anniversary', but with fixed (ISO) order of arguments."
  (with-no-warnings
    (let ((calendar-date-style 'iso))
      (diary-anniversary year month day mark))))
(defun org-cyclic (N year month day &optional mark)
  "Like `diary-cyclic', but with fixed (ISO) order of arguments."
  (with-no-warnings
    (let ((calendar-date-style 'iso))
      (diary-cyclic N year month day mark))))
(defun org-block (Y1 M1 D1 Y2 M2 D2 &optional mark)
  "Like `diary-block', but with fixed (ISO) order of arguments."
  (with-no-warnings
    (let ((calendar-date-style 'iso))
      (diary-block Y1 M1 D1 Y2 M2 D2 mark))))
(defun org-date (year month day &optional mark)
  "Like `diary-date', but with fixed (ISO) order of arguments."
  (with-no-warnings
    (let ((calendar-date-style 'iso))
      (diary-date year month day mark))))

;; Define the `org-class' function
(defun org-class (y1 m1 d1 y2 m2 d2 dayname &rest skip-weeks)
  "Entry applies if date is between dates on DAYNAME, but skips SKIP-WEEKS.
DAYNAME is a number between 0 (Sunday) and 6 (Saturday).
SKIP-WEEKS is any number of ISO weeks in the block period for which the
item should be skipped.  If any of the SKIP-WEEKS arguments is the symbol
`holidays', then any date that is known by the Emacs calendar to be a
holiday will also be skipped.  If SKIP-WEEKS arguments are holiday strings,
then those holidays will be skipped."
  (let* ((date1 (calendar-absolute-from-gregorian (list m1 d1 y1)))
	 (date2 (calendar-absolute-from-gregorian (list m2 d2 y2)))
	 (d (calendar-absolute-from-gregorian date))
	 (h (when skip-weeks (calendar-check-holidays date))))
    (and
     (<= date1 d)
     (<= d date2)
     (= (calendar-day-of-week date) dayname)
     (or (not skip-weeks)
	 (progn
	   (require 'cal-iso)
	   (not (member (car (calendar-iso-from-absolute d)) skip-weeks))))
     (not (or (and h (memq 'holidays skip-weeks))
	      (delq nil (mapcar (lambda(g) (member g skip-weeks)) h))))
     entry)))

(defalias 'org-get-closed 'org-agenda-get-progress)
(defun org-agenda-get-progress ()
  "Return the logged TODO entries for agenda display."
  (let* ((props (list 'mouse-face 'highlight
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (items (if (consp org-agenda-show-log-scoped)
		    org-agenda-show-log-scoped
		  (if (eq org-agenda-show-log-scoped 'clockcheck)
		      '(clock)
		    org-agenda-log-mode-items)))
	 (parts
	  (delq nil
		(list
		 (if (memq 'closed items) (concat "\\<" org-closed-string))
		 (if (memq 'clock items) (concat "\\<" org-clock-string))
		 (if (memq 'state items)
		     (format "- State \"%s\".*?" org-todo-regexp)))))
	 (parts-re (if parts (mapconcat 'identity parts "\\|")
		     (error "`org-agenda-log-mode-items' is empty")))
	 (regexp (concat
		  "\\(" parts-re "\\)"
		  " *\\["
		  (regexp-quote
		   (substring
		    (format-time-string
		     (car org-time-stamp-formats)
		     (apply 'encode-time  ; DATE bound by calendar
			    (list 0 0 0 (nth 1 date) (car date) (nth 2 date))))
		    1 11))))
	 (org-agenda-search-headline-for-time nil)
	 marker hdmarker priority category level tags closedp
	 statep clockp state ee txt extra timestr rest clocked inherited-tags)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(org-agenda-skip)
	(setq marker (org-agenda-new-marker (match-beginning 0))
	      closedp (equal (match-string 1) org-closed-string)
	      statep (equal (string-to-char (match-string 1)) ?-)
	      clockp (not (or closedp statep))
	      state (and statep (match-string 2))
	      category (org-get-category (match-beginning 0))
	      timestr (buffer-substring (match-beginning 0) (point-at-eol)))
	(when (string-match "\\]" timestr)
	  ;; substring should only run to end of time stamp
	  (setq rest (substring timestr (match-end 0))
		timestr (substring timestr 0 (match-end 0)))
	  (if (and (not closedp) (not statep)
		   (string-match "\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)\\].*?\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)"
				 rest))
	      (progn (setq timestr (concat (substring timestr 0 -1)
					   "-" (match-string 1 rest) "]"))
		     (setq clocked (match-string 2 rest)))
	    (setq clocked "-")))
	(save-excursion
	  (setq extra
		(cond
		 ((not org-agenda-log-mode-add-notes) nil)
		 (statep
		  (and (looking-at ".*\\\\\n[ \t]*\\([^-\n \t].*?\\)[ \t]*$")
		       (match-string 1)))
		 (clockp
		  (and (looking-at ".*\n[ \t]*-[ \t]+\\([^-\n \t].*?\\)[ \t]*$")
		       (match-string 1)))))
	  (if (not (re-search-backward org-outline-regexp-bol nil t))
	      (throw :skip nil)
	    (goto-char (match-beginning 0))
	    (setq hdmarker (org-agenda-new-marker)
		  inherited-tags
		  (or (eq org-agenda-show-inherited-tags 'always)
		      (and (listp org-agenda-show-inherited-tags)
			   (memq 'todo org-agenda-show-inherited-tags))
		      (and (eq org-agenda-show-inherited-tags t)
			   (or (eq org-agenda-use-tag-inheritance t)
			       (memq 'todo org-agenda-use-tag-inheritance))))
		  tags (org-get-tags-at nil (not inherited-tags))
		  level (make-string (org-reduced-level (org-outline-level)) ? ))
	    (looking-at "\\*+[ \t]+\\([^\r\n]+\\)")
	    (setq txt (match-string 1))
	    (when extra
	      (if (string-match "\\([ \t]+\\)\\(:[^ \n\t]*?:\\)[ \t]*$" txt)
		  (setq txt (concat (substring txt 0 (match-beginning 1))
				    " - " extra " " (match-string 2 txt)))
		(setq txt (concat txt " - " extra))))
	    (setq txt (org-agenda-format-item
		       (cond
			(closedp "Closed:    ")
			(statep (concat "State:     (" state ")"))
			(t (concat "Clocked:   (" clocked  ")")))
		       txt level category tags timestr)))
	  (setq priority 100000)
	  (org-add-props txt props
	    'org-marker marker 'org-hd-marker hdmarker 'face 'org-agenda-done
	    'priority priority 'level level
	    'type "closed" 'date date
	    'undone-face 'org-warning 'done-face 'org-agenda-done)
	  (push txt ee))
	(goto-char (point-at-eol))))
    (nreverse ee)))

(defun org-agenda-show-clocking-issues ()
  "Add overlays, showing issues with clocking.
See also the user option `org-agenda-clock-consistency-checks'."
  (interactive)
  (let* ((pl org-agenda-clock-consistency-checks)
	 (re (concat "^[ \t]*"
		     org-clock-string
		     "[ \t]+"
		     "\\(\\[.*?\\]\\)"	; group 1 is first stamp
		     "\\(-\\{1,3\\}\\(\\[.*?\\]\\)\\)?")) ; group 3 is second
	 (tlstart 0.)
	 (tlend 0.)
	 (maxtime (org-duration-to-minutes
		   (or (plist-get pl :max-duration) "24:00")))
	 (mintime (org-duration-to-minutes
		   (or (plist-get pl :min-duration) 0)))
	 (maxgap  (org-duration-to-minutes
		   ;; default 30:00 means never complain
		   (or (plist-get pl :max-gap) "30:00")))
	 (gapok (mapcar #'org-duration-to-minutes
			(plist-get pl :gap-ok-around)))
	 (def-face (or (plist-get pl :default-face)
		       '((:background "DarkRed") (:foreground "white"))))
	 issue face m te ts dt ov)
    (goto-char (point-min))
    (while (re-search-forward " Clocked: +(-\\|\\([0-9]+:[0-9]+\\))" nil t)
      (setq issue nil face def-face)
      (catch 'next
	(setq m (org-get-at-bol 'org-marker)
	      te nil ts nil)
	(unless (and m (markerp m))
	  (setq issue "No valid clock line") (throw 'next t))
	(org-with-point-at m
	  (save-excursion
	    (goto-char (point-at-bol))
	    (unless (looking-at re)
	      (error "No valid Clock line")
	      (throw 'next t))
	    (unless (match-end 3)
	      (setq issue "No end time"
		    face (or (plist-get pl :no-end-time-face) face))
	      (throw 'next t))
	    (setq ts (match-string 1)
		  te (match-string 3)
		  ts (float-time
		      (apply #'encode-time (org-parse-time-string ts)))
		  te (float-time
		      (apply #'encode-time (org-parse-time-string te)))
		  dt (- te ts))))
	(cond
	 ((> dt (* 60 maxtime))
	  ;; a very long clocking chunk
	  (setq issue (format "Clocking interval is very long: %s"
			      (org-duration-from-minutes (floor (/ dt 60.))))
		face (or (plist-get pl :long-face) face)))
	 ((< dt (* 60 mintime))
	  ;; a very short clocking chunk
	  (setq issue (format "Clocking interval is very short: %s"
			      (org-duration-from-minutes (floor (/ dt 60.))))
		face (or (plist-get pl :short-face) face)))
	 ((and (> tlend 0) (< ts tlend))
	  ;; Two clock entries are overlapping
	  (setq issue (format "Clocking overlap: %d minutes"
			      (/ (- tlend ts) 60))
		face (or (plist-get pl :overlap-face) face)))
	 ((and (> tlend 0) (> ts (+ tlend (* 60 maxgap))))
	  ;; There is a gap, lets see if we need to report it
	  (unless (org-agenda-check-clock-gap tlend ts gapok)
	    (setq issue (format "Clocking gap: %d minutes"
				(/ (- ts tlend) 60))
		  face (or (plist-get pl :gap-face) face))))
	 (t nil)))
      (setq tlend (or te tlend) tlstart (or ts tlstart))
      (when issue
	;; OK, there was some issue, add an overlay to show the issue
	(setq ov (make-overlay (point-at-bol) (point-at-eol)))
	(overlay-put ov 'before-string
		     (concat
		      (org-add-props
			  (format "%-43s" (concat " " issue))
			  nil
			'face face)
		      "\n"))
	(overlay-put ov 'evaporate t)))))

(defun org-agenda-check-clock-gap (t1 t2 ok-list)
  "Check if gap T1 -> T2 contains one of the OK-LIST time-of-day values."
  (catch 'exit
    (unless ok-list
      ;; there are no OK times for gaps...
      (throw 'exit nil))
    (if (> (- (/ t2 36000) (/ t1 36000)) 24)
	;; This is more than 24 hours, so it is OK.
	;; because we have at least one OK time, that must be in the
	;; 24 hour interval.
	(throw 'exit t))
    ;; We have a shorter gap.
    ;; Now we have to get the minute of the day when these times are
    (let* ((t1dec (decode-time (seconds-to-time t1)))
	   (t2dec (decode-time (seconds-to-time t2)))
	   ;; compute the minute on the day
	   (min1 (+ (nth 1 t1dec) (* 60 (nth 2 t1dec))))
	   (min2 (+ (nth 1 t2dec) (* 60 (nth 2 t2dec)))))
      (when (< min2 min1)
	;; if min2 is smaller than min1, this means it is on the next day.
	;; Wrap it to after midnight.
	(setq min2 (+ min2 1440)))
      ;; Now check if any of the OK times is in the gap
      (mapc (lambda (x)
	      ;; Wrap the time to after midnight if necessary
	      (if (< x min1) (setq x (+ x 1440)))
	      ;; Check if in interval
	      (and (<= min1 x) (>= min2 x) (throw 'exit t)))
	    ok-list)
      ;; Nope, this gap is not OK
      nil)))

(defun org-agenda-get-deadlines (&optional with-hour)
  "Return the deadline information for agenda display.
When WITH-HOUR is non-nil, only return deadlines with an hour
specification like [h]h:mm."
  (let* ((props (list 'mouse-face 'highlight
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp (if with-hour
		     org-deadline-time-hour-regexp
		   org-deadline-time-regexp))
	 (today (org-today))
	 (today? (org-agenda-today-p date)) ; DATE bound by calendar.
	 (current (calendar-absolute-from-gregorian date))
	 deadline-items)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(unless (save-match-data (org-at-planning-p)) (throw :skip nil))
	(org-agenda-skip)
	(let* ((s (match-string 1))
	       (pos (1- (match-beginning 1)))
	       (todo-state (save-match-data (org-get-todo-state)))
	       (done? (member todo-state org-done-keywords))
               (sexp? (string-prefix-p "%%" s))
	       ;; DEADLINE is the deadline date for the entry.  It is
	       ;; either the base date or the last repeat, according
	       ;; to `org-agenda-prefer-last-repeat'.
	       (deadline
		(cond
		 (sexp? (org-agenda--timestamp-to-absolute s current))
		 ((or (eq org-agenda-prefer-last-repeat t)
		      (member todo-state org-agenda-prefer-last-repeat))
		  (org-agenda--timestamp-to-absolute
		   s today 'past (current-buffer) pos))
		 (t (org-agenda--timestamp-to-absolute s))))
	       ;; REPEAT is the future repeat closest from CURRENT,
	       ;; according to `org-agenda-show-future-repeats'. If
	       ;; the latter is nil, or if the time stamp has no
	       ;; repeat part, default to DEADLINE.
	       (repeat
		(cond
		 (sexp? deadline)
		 ((<= current today) deadline)
		 ((not org-agenda-show-future-repeats) deadline)
		 (t
		  (let ((base (if (eq org-agenda-show-future-repeats 'next)
				  (1+ today)
				current)))
		    (org-agenda--timestamp-to-absolute
		     s base 'future (current-buffer) pos)))))
	       (diff (- deadline current))
	       (suppress-prewarning
		(let ((scheduled
		       (and org-agenda-skip-deadline-prewarning-if-scheduled
			    (org-entry-get nil "SCHEDULED"))))
		  (cond
		   ((not scheduled) nil)
		   ;; The current item has a scheduled date, so
		   ;; evaluate its prewarning lead time.
		   ((integerp org-agenda-skip-deadline-prewarning-if-scheduled)
		    ;; Use global prewarning-restart lead time.
		    org-agenda-skip-deadline-prewarning-if-scheduled)
		   ((eq org-agenda-skip-deadline-prewarning-if-scheduled
			'pre-scheduled)
		    ;; Set pre-warning to no earlier than SCHEDULED.
		    (min (- deadline
			    (org-agenda--timestamp-to-absolute scheduled))
			 org-deadline-warning-days))
		   ;; Set pre-warning to deadline.
		   (t 0))))
	       (wdays (if suppress-prewarning
			  (let ((org-deadline-warning-days suppress-prewarning))
			    (org-get-wdays s))
			(org-get-wdays s))))
	  (cond
	   ;; Only display deadlines at their base date, at future
	   ;; repeat occurrences or in today agenda.
	   ((= current deadline) nil)
	   ((= current repeat) nil)
	   ((not today?) (throw :skip nil))
	   ;; Upcoming deadline: display within warning period WDAYS.
	   ((> deadline current) (when (> diff wdays) (throw :skip nil)))
	   ;; Overdue deadline: warn about it for
	   ;; `org-deadline-past-days' duration.
	   (t (when (< org-deadline-past-days (- diff)) (throw :skip nil))))
	  ;; Possibly skip done tasks.
	  (when (and done?
		     (or org-agenda-skip-deadline-if-done
			 (/= deadline current)))
	    (throw :skip nil))
	  (save-excursion
	    (re-search-backward "^\\*+[ \t]+" nil t)
	    (goto-char (match-end 0))
	    (let* ((category (org-get-category))
		   (level (make-string (org-reduced-level (org-outline-level))
				       ?\s))
		   (head (buffer-substring (point) (line-end-position)))
		   (inherited-tags
		    (or (eq org-agenda-show-inherited-tags 'always)
			(and (listp org-agenda-show-inherited-tags)
			     (memq 'agenda org-agenda-show-inherited-tags))
			(and (eq org-agenda-show-inherited-tags t)
			     (or (eq org-agenda-use-tag-inheritance t)
				 (memq 'agenda
				       org-agenda-use-tag-inheritance)))))
		   (tags (org-get-tags-at nil (not inherited-tags)))
		   (time
		    (cond
		     ;; No time of day designation if it is only
		     ;; a reminder.
		     ((and (/= current deadline) (/= current repeat)) nil)
		     ((string-match " \\([012]?[0-9]:[0-9][0-9]\\)" s)
		      (concat (substring s (match-beginning 1)) " "))
		     (t 'time)))
		   (item
		    (org-agenda-format-item
		     ;; Insert appropriate suffixes before deadlines.
		     ;; Those only apply to today agenda.
		     (pcase-let ((`(,now ,future ,past)
				  org-agenda-deadline-leaders))
		       (cond
			((and today? (< deadline today)) (format past (- diff)))
			((and today? (> deadline today)) (format future diff))
			(t now)))
		     head level category tags time))
		   (face (org-agenda-deadline-face
			  (- 1 (/ (float diff) (max wdays 1)))))
		   (upcoming? (and today? (> deadline today)))
		   (warntime (get-text-property (point) 'org-appt-warntime)))
	      (org-add-props item props
		'org-marker (org-agenda-new-marker pos)
		'org-hd-marker (org-agenda-new-marker (line-beginning-position))
		'warntime warntime
		'level level
		'ts-date deadline
		'priority
		;; Adjust priority to today reminders about deadlines.
		;; Overdue deadlines get the highest priority
		;; increase, then imminent deadlines and eventually
		;; more distant deadlines.
		(let ((adjust (if today? (- diff) 0)))
		  (+ adjust (org-get-priority item)))
		'todo-state todo-state
		'type (if upcoming? "upcoming-deadline" "deadline")
		'date (if upcoming? date deadline)
		'face (if done? 'org-agenda-done face)
		'undone-face face
		'done-face 'org-agenda-done)
	      (push item deadline-items))))))
    (nreverse deadline-items)))

(defun org-agenda-deadline-face (fraction)
  "Return the face to displaying a deadline item.
FRACTION is what fraction of the head-warning time has passed."
  (assoc-default fraction org-agenda-deadline-faces #'<=))

(defun org-agenda-get-scheduled (&optional deadlines with-hour)
  "Return the scheduled information for agenda display.
Optional argument DEADLINES is a list of deadline items to be
displayed in agenda view.  When WITH-HOUR is non-nil, only return
scheduled items with an hour specification like [h]h:mm."
  (let* ((props (list 'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'done-face 'org-agenda-done
		      'mouse-face 'highlight
		      'help-echo
		      (format "mouse-2 or RET jump to Org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp (if with-hour
		     org-scheduled-time-hour-regexp
		   org-scheduled-time-regexp))
	 (today (org-today))
	 (todayp (org-agenda-today-p date)) ; DATE bound by calendar.
	 (current (calendar-absolute-from-gregorian date))
	 (deadline-pos
	  (mapcar (lambda (d)
		    (let ((m (get-text-property 0 'org-hd-marker d)))
		      (and m (marker-position m))))
		  deadlines))
	 scheduled-items)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(unless (save-match-data (org-at-planning-p)) (throw :skip nil))
	(org-agenda-skip)
	(let* ((s (match-string 1))
	       (pos (1- (match-beginning 1)))
	       (todo-state (save-match-data (org-get-todo-state)))
	       (donep (member todo-state org-done-keywords))
	       (sexp? (string-prefix-p "%%" s))
	       ;; SCHEDULE is the scheduled date for the entry.  It is
	       ;; either the bare date or the last repeat, according
	       ;; to `org-agenda-prefer-last-repeat'.
	       (schedule
		(cond
		 (sexp? (org-agenda--timestamp-to-absolute s current))
		 ((or (eq org-agenda-prefer-last-repeat t)
		      (member todo-state org-agenda-prefer-last-repeat))
		  (org-agenda--timestamp-to-absolute
		   s today 'past (current-buffer) pos))
		 (t (org-agenda--timestamp-to-absolute s))))
	       ;; REPEAT is the future repeat closest from CURRENT,
	       ;; according to `org-agenda-show-future-repeats'. If
	       ;; the latter is nil, or if the time stamp has no
	       ;; repeat part, default to SCHEDULE.
	       (repeat
		(cond
		 (sexp? schedule)
		 ((<= current today) schedule)
		 ((not org-agenda-show-future-repeats) schedule)
		 (t
		  (let ((base (if (eq org-agenda-show-future-repeats 'next)
				  (1+ today)
				current)))
		    (org-agenda--timestamp-to-absolute
		     s base 'future (current-buffer) pos)))))
	       (diff (- current schedule))
	       (warntime (get-text-property (point) 'org-appt-warntime))
	       (pastschedp (< schedule today))
	       (habitp (and (fboundp 'org-is-habit-p) (org-is-habit-p)))
	       (suppress-delay
		(let ((deadline (and org-agenda-skip-scheduled-delay-if-deadline
				     (org-entry-get nil "DEADLINE"))))
		  (cond
		   ((not deadline) nil)
		   ;; The current item has a deadline date, so
		   ;; evaluate its delay time.
		   ((integerp org-agenda-skip-scheduled-delay-if-deadline)
		    ;; Use global delay time.
		    (- org-agenda-skip-scheduled-delay-if-deadline))
		   ((eq org-agenda-skip-scheduled-delay-if-deadline
			'post-deadline)
		    ;; Set delay to no later than DEADLINE.
		    (min (- schedule
			    (org-agenda--timestamp-to-absolute deadline))
			 org-scheduled-delay-days))
		   (t 0))))
	       (ddays
		(cond
		 ;; Nullify delay when a repeater triggered already
		 ;; and the delay is of the form --Xd.
		 ((and (string-match-p "--[0-9]+[hdwmy]" s)
		       (> current schedule))
		  0)
		 (suppress-delay
		  (let ((org-scheduled-delay-days suppress-delay))
		    (org-get-wdays s t t)))
		 (t (org-get-wdays s t)))))
	  ;; Display scheduled items at base date (SCHEDULE), today if
	  ;; scheduled before the current date, and at any repeat past
	  ;; today.  However, skip delayed items and items that have
	  ;; been displayed for more than `org-scheduled-past-days'.
	  (unless (and todayp
		       habitp
		       (bound-and-true-p org-habit-show-all-today))
	    (when (or (and (> ddays 0) (< diff ddays))
		      (> diff org-scheduled-past-days)
		      (> schedule current)
		      (and (/= current schedule)
			   (/= current today)
			   (/= current repeat)))
	      (throw :skip nil)))
	  ;; Possibly skip done tasks.
	  (when (and donep
		     (or org-agenda-skip-scheduled-if-done
			 (/= schedule current)))
	    (throw :skip nil))
	  ;; Skip entry if it already appears as a deadline, per
	  ;; `org-agenda-skip-scheduled-if-deadline-is-shown'.  This
	  ;; doesn't apply to habits.
	  (when (pcase org-agenda-skip-scheduled-if-deadline-is-shown
		  ((guard
		    (or (not (memq (line-beginning-position 0) deadline-pos))
			habitp))
		   nil)
		  (`repeated-after-deadline
		   (let ((deadline (time-to-days
				    (org-get-deadline-time (point)))))
		     (and (<= schedule deadline) (> current deadline))))
		  (`not-today pastschedp)
		  (`t t)
		  (_ nil))
	    (throw :skip nil))
	  ;; Skip habits if `org-habit-show-habits' is nil, or if we
	  ;; only show them for today.  Also skip done habits.
	  (when (and habitp
		     (or donep
			 (not (bound-and-true-p org-habit-show-habits))
			 (and (not todayp)
			      (bound-and-true-p
			       org-habit-show-habits-only-for-today))))
	    (throw :skip nil))
	  (save-excursion
	    (re-search-backward "^\\*+[ \t]+" nil t)
	    (goto-char (match-end 0))
	    (let* ((category (org-get-category))
		   (inherited-tags
		    (or (eq org-agenda-show-inherited-tags 'always)
			(and (listp org-agenda-show-inherited-tags)
			     (memq 'agenda org-agenda-show-inherited-tags))
			(and (eq org-agenda-show-inherited-tags t)
			     (or (eq org-agenda-use-tag-inheritance t)
				 (memq 'agenda
				       org-agenda-use-tag-inheritance)))))
		   (tags (org-get-tags-at nil (not inherited-tags)))
		   (level (make-string (org-reduced-level (org-outline-level))
				       ?\s))
		   (head (buffer-substring (point) (line-end-position)))
		   (time
		    (cond
		     ;; No time of day designation if it is only
		     ;; a reminder.
		     ((and (/= current schedule) (/= current repeat)) nil)
		     ((string-match " \\([012]?[0-9]:[0-9][0-9]\\)" s)
		      (concat (substring s (match-beginning 1)) " "))
		     (t 'time)))
		   (item
		    (org-agenda-format-item
		     (pcase-let ((`(,first ,past) org-agenda-scheduled-leaders))
		       ;; Show a reminder of a past scheduled today.
		       (if (and todayp pastschedp)
			   (format past diff)
			 first))
		     head level category tags time nil habitp))
		   (face (cond ((and (not habitp) pastschedp)
				'org-scheduled-previously)
			       (todayp 'org-scheduled-today)
			       (t 'org-scheduled)))
		   (habitp (and habitp (org-habit-parse-todo))))
	      (org-add-props item props
		'undone-face face
		'face (if donep 'org-agenda-done face)
		'org-marker (org-agenda-new-marker pos)
		'org-hd-marker (org-agenda-new-marker (line-beginning-position))
		'type (if pastschedp "past-scheduled" "scheduled")
		'date (if pastschedp schedule date)
		'ts-date schedule
		'warntime warntime
		'level level
		'priority (if habitp (org-habit-get-priority habitp)
			    (+ 99 diff (org-get-priority item)))
		'org-habit-p habitp
		'todo-state todo-state)
	      (push item scheduled-items))))))
    (nreverse scheduled-items)))

(defun org-agenda-get-blocks ()
  "Return the date-range information for agenda display."
  (let* ((props (list 'face nil
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'mouse-face 'highlight
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp org-tr-regexp)
	 (d0 (calendar-absolute-from-gregorian date))
	 marker hdmarker ee txt d1 d2 s1 s2 category
	 level todo-state tags pos head donep inherited-tags)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(org-agenda-skip)
	(setq pos (point))
	(let ((start-time (match-string 1))
	      (end-time (match-string 2)))
	  (setq s1 (match-string 1)
		s2 (match-string 2)
		d1 (time-to-days
		    (condition-case err
			(org-time-string-to-time s1)
		      (error
		       (error
			"Bad timestamp %S at %d in buffer %S\nError was: %s"
			s1
			pos
			(current-buffer)
			(error-message-string err)))))
		d2 (time-to-days
		    (condition-case err
			(org-time-string-to-time s2)
		      (error
		       (error
			"Bad timestamp %S at %d in buffer %S\nError was: %s"
			s2
			pos
			(current-buffer)
			(error-message-string err))))))
	  (if (and (> (- d0 d1) -1) (> (- d2 d0) -1))
	      ;; Only allow days between the limits, because the normal
	      ;; date stamps will catch the limits.
	      (save-excursion
		(setq todo-state (org-get-todo-state))
		(setq donep (member todo-state org-done-keywords))
		(if (and donep org-agenda-skip-timestamp-if-done)
		    (throw :skip t))
		(setq marker (org-agenda-new-marker (point))
		      category (org-get-category))
		(if (not (re-search-backward org-outline-regexp-bol nil t))
		    (throw :skip nil)
		  (goto-char (match-beginning 0))
		  (setq hdmarker (org-agenda-new-marker (point))
			inherited-tags
			(or (eq org-agenda-show-inherited-tags 'always)
			    (and (listp org-agenda-show-inherited-tags)
				 (memq 'agenda org-agenda-show-inherited-tags))
			    (and (eq org-agenda-show-inherited-tags t)
				 (or (eq org-agenda-use-tag-inheritance t)
				     (memq 'agenda org-agenda-use-tag-inheritance))))

			tags (org-get-tags-at nil (not inherited-tags)))
		  (setq level (make-string (org-reduced-level (org-outline-level)) ? ))
		  (looking-at "\\*+[ \t]+\\(.*\\)")
		  (setq head (match-string 1))
		  (let ((remove-re
			 (if org-agenda-remove-timeranges-from-blocks
			     (concat
			      "<" (regexp-quote s1) ".*?>"
			      "--"
			      "<" (regexp-quote s2) ".*?>")
			   nil)))
		    (setq txt (org-agenda-format-item
			       (format
				(nth (if (= d1 d2) 0 1)
				     org-agenda-timerange-leaders)
				(1+ (- d0 d1)) (1+ (- d2 d1)))
			       head level category tags
			       (cond ((and (= d1 d0) (= d2 d0))
				      (concat "<" start-time ">--<" end-time ">"))
                                     ((= d1 d0)
				      (concat "<" start-time ">"))
				     ((= d2 d0)
				      (concat "<" end-time ">")))
			       remove-re))))
		(org-add-props txt props
		  'org-marker marker 'org-hd-marker hdmarker
		  'type "block" 'date date
		  'level level
		  'todo-state todo-state
		  'priority (org-get-priority txt))
		(push txt ee))))
	(goto-char pos)))
    ;; Sort the entries by expiration date.
    (nreverse ee)))

;;; Agenda presentation and sorting

(defvar org-prefix-has-time nil
  "A flag, set by `org-compile-prefix-format'.
The flag is set if the currently compiled format contains a `%t'.")
(defvar org-prefix-has-tag nil
  "A flag, set by `org-compile-prefix-format'.
The flag is set if the currently compiled format contains a `%T'.")
(defvar org-prefix-has-effort nil
  "A flag, set by `org-compile-prefix-format'.
The flag is set if the currently compiled format contains a `%e'.")
(defvar org-prefix-has-breadcrumbs nil
  "A flag, set by `org-compile-prefix-format'.
The flag is set if the currently compiled format contains a `%b'.")
(defvar org-prefix-category-length nil
  "Used by `org-compile-prefix-format' to remember the category field width.")
(defvar org-prefix-category-max-length nil
  "Used by `org-compile-prefix-format' to remember the category field width.")

(defun org-agenda-get-category-icon (category)
  "Return an image for CATEGORY according to `org-agenda-category-icon-alist'."
  (cl-dolist (entry org-agenda-category-icon-alist)
    (when (string-match-p (car entry) category)
      (if (listp (cadr entry))
	  (cl-return (cadr entry))
	(cl-return (apply #'create-image (cdr entry)))))))

(defun org-agenda-format-item (extra txt &optional level category tags dotime
				     remove-re habitp)
  "Format TXT to be inserted into the agenda buffer.
In particular, add the prefix and corresponding text properties.

EXTRA must be a string to replace the `%s' specifier in the prefix format.
LEVEL may be a string to replace the `%l' specifier.
CATEGORY (a string, a symbol or nil) may be used to overrule the default
category taken from local variable or file name.  It will replace the `%c'
specifier in the format.
DOTIME, when non-nil, indicates that a time-of-day should be extracted from
TXT for sorting of this entry, and for the `%t' specifier in the format.
When DOTIME is a string, this string is searched for a time before TXT is.
TAGS can be the tags of the headline.
Any match of REMOVE-RE will be removed from TXT."
  ;; We keep the org-prefix-* variable values along with a compiled
  ;; formatter, so that multiple agendas existing at the same time do
  ;; not step on each other toes.
  ;;
  ;; It was inconvenient to make these variables buffer local in
  ;; Agenda buffers, because this function expects to be called with
  ;; the buffer where item comes from being current, and not agenda
  ;; buffer
  (let* ((bindings (car org-prefix-format-compiled))
	 (formatter (cadr org-prefix-format-compiled)))
    (cl-loop for (var value) in bindings
	     do (set var value))
    (save-match-data
      ;; Diary entries sometimes have extra whitespace at the beginning
      (setq txt (org-trim txt))

      ;; Fix the tags part in txt
      (setq txt (org-agenda-fix-displayed-tags
		 txt tags
		 org-agenda-show-inherited-tags
		 org-agenda-hide-tags-regexp))

      (let* ((category (or category
			   (if buffer-file-name
			       (file-name-sans-extension
				(file-name-nondirectory buffer-file-name))
			     "")))
	     (category-icon (org-agenda-get-category-icon category))
	     (category-icon (if category-icon
				(propertize " " 'display category-icon)
			      ""))
	     (effort (and (not (string= txt ""))
			  (get-text-property 1 'effort txt)))
	     ;; time, tag, effort are needed for the eval of the prefix format
	     (tag (if tags (nth (1- (length tags)) tags) ""))
	     (time-grid-trailing-characters (nth 2 org-agenda-time-grid))
	     time
	     (ts (if dotime (concat
			     (if (stringp dotime) dotime "")
			     (and org-agenda-search-headline-for-time txt))))
	     (time-of-day (and dotime (org-get-time-of-day ts)))
	     stamp plain s0 s1 s2 rtn srp l
	     duration breadcrumbs)
	(and (derived-mode-p 'org-mode) buffer-file-name
	     (add-to-list 'org-agenda-contributing-files buffer-file-name))
	(when (and dotime time-of-day)
	  ;; Extract starting and ending time and move them to prefix
	  (when (or (setq stamp (string-match org-stamp-time-of-day-regexp ts))
		    (setq plain (string-match org-plain-time-of-day-regexp ts)))
	    (setq s0 (match-string 0 ts)
		  srp (and stamp (match-end 3))
		  s1 (match-string (if plain 1 2) ts)
		  s2 (match-string (if plain 8 (if srp 4 6)) ts))

	    ;; If the times are in TXT (not in DOTIMES), and the prefix will list
	    ;; them, we might want to remove them there to avoid duplication.
	    ;; The user can turn this off with a variable.
	    (if (and org-prefix-has-time
		     org-agenda-remove-times-when-in-prefix (or stamp plain)
		     (string-match (concat (regexp-quote s0) " *") txt)
		     (not (equal ?\] (string-to-char (substring txt (match-end 0)))))
		     (if (eq org-agenda-remove-times-when-in-prefix 'beg)
			 (= (match-beginning 0) 0)
		       t))
		(setq txt (replace-match "" nil nil txt))))
	  ;; Normalize the time(s) to 24 hour
	  (if s1 (setq s1 (org-get-time-of-day s1 'string t)))
	  (if s2 (setq s2 (org-get-time-of-day s2 'string t)))

	  ;; Try to set s2 if s1 and
	  ;; `org-agenda-default-appointment-duration' are set
	  (when (and s1 (not s2) org-agenda-default-appointment-duration)
	    (setq s2
		  (org-duration-from-minutes
		   (+ (org-duration-to-minutes s1 t)
		      org-agenda-default-appointment-duration)
		   nil t)))

	  ;; Compute the duration
	  (when s2
	    (setq duration (- (org-duration-to-minutes s2)
			      (org-duration-to-minutes s1)))))

	(when (string-match "\\([ \t]+\\)\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$" txt)
	  ;; Tags are in the string
	  (if (or (eq org-agenda-remove-tags t)
		  (and org-agenda-remove-tags
		       org-prefix-has-tag))
	      (setq txt (replace-match "" t t txt))
	    (setq txt (replace-match
		       (concat (make-string (max (- 50 (length txt)) 1) ?\ )
			       (match-string 2 txt))
		       t t txt))))

	(when remove-re
	  (while (string-match remove-re txt)
	    (setq txt (replace-match "" t t txt))))

	;; Set org-heading property on `txt' to mark the start of the
	;; heading.
	(add-text-properties 0 (length txt) '(org-heading t) txt)

	;; Prepare the variables needed in the eval of the compiled format
	(if org-prefix-has-breadcrumbs
	    (setq breadcrumbs (org-with-point-at (org-get-at-bol 'org-marker)
				(let ((s (org-display-outline-path nil nil "->" t)))
				  (if (eq "" s) "" (concat s "->"))))))
	(setq time (cond (s2 (concat
			      (org-agenda-time-of-day-to-ampm-maybe s1)
			      "-" (org-agenda-time-of-day-to-ampm-maybe s2)
			      (if org-agenda-timegrid-use-ampm " ")))
			 (s1 (concat
			      (org-agenda-time-of-day-to-ampm-maybe s1)
			      (if org-agenda-timegrid-use-ampm
                                  (concat time-grid-trailing-characters " ")
                                time-grid-trailing-characters)))
			 (t ""))
	      extra (or (and (not habitp) extra) "")
	      category (if (symbolp category) (symbol-name category) category)
	      level (or level ""))
	(if (string-match org-bracket-link-regexp category)
	    (progn
	      (setq l (if (match-end 3)
			  (- (match-end 3) (match-beginning 3))
			(- (match-end 1) (match-beginning 1))))
	      (when (< l (or org-prefix-category-length 0))
		(setq category (copy-sequence category))
		(org-add-props category nil
		  'extra-space (make-string
				(- org-prefix-category-length l 1) ?\ ))))
	  (if (and org-prefix-category-max-length
		   (>= (length category) org-prefix-category-max-length))
	      (setq category (substring category 0 (1- org-prefix-category-max-length)))))
	;; Evaluate the compiled format
	(setq rtn (concat (eval formatter) txt))

	;; And finally add the text properties
	(remove-text-properties 0 (length rtn) '(line-prefix t wrap-prefix t) rtn)
	(org-add-props rtn nil
	  'org-category category
	  'tags (mapcar 'org-downcase-keep-props tags)
	  'org-highest-priority org-highest-priority
	  'org-lowest-priority org-lowest-priority
	  'time-of-day time-of-day
	  'duration duration
	  'breadcrumbs breadcrumbs
	  'txt txt
	  'level level
	  'time time
	  'extra extra
	  'format org-prefix-format-compiled
	  'dotime dotime)))))

(defun org-agenda-fix-displayed-tags (txt tags add-inherited hide-re)
  "Remove tags string from TXT, and add a modified list of tags.
The modified list may contain inherited tags, and tags matched by
`org-agenda-hide-tags-regexp' will be removed."
  (when (or add-inherited hide-re)
    (if (string-match "\\([ \t]+\\)\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$" txt)
	(setq txt (substring txt 0 (match-beginning 0))))
    (setq tags
	  (delq nil
		(mapcar (lambda (tg)
			  (if (or (and hide-re (string-match hide-re tg))
				  (and (not add-inherited)
				       (get-text-property 0 'inherited tg)))
			      nil
			    tg))
			tags)))
    (when tags
      (let ((have-i (get-text-property 0 'inherited (car tags)))
	    i)
	(setq txt (concat txt " :"
			  (mapconcat
			   (lambda (x)
			     (setq i (get-text-property 0 'inherited x))
			     (if (and have-i (not i))
				 (progn
				   (setq have-i nil)
				   (concat ":" x))
			       x))
			   tags ":")
			  (if have-i "::" ":"))))))
  txt)

(defun org-downcase-keep-props (s)
  (let ((props (text-properties-at 0 s)))
    (setq s (downcase s))
    (add-text-properties 0 (length s) props s)
    s))

(defvar org-agenda-sorting-strategy) ;; because the def is in a let form

(defun org-agenda-add-time-grid-maybe (list ndays todayp)
  "Add a time-grid for agenda items which need it.

LIST is the list of agenda items formatted by `org-agenda-list'.
NDAYS is the span of the current agenda view.
TODAYP is t when the current agenda view is on today."
  (catch 'exit
    (cond ((not org-agenda-use-time-grid) (throw 'exit list))
	  ((and todayp (member 'today (car org-agenda-time-grid))))
	  ((and (= ndays 1) (member 'daily (car org-agenda-time-grid))))
	  ((member 'weekly (car org-agenda-time-grid)))
	  (t (throw 'exit list)))
    (let* ((have (delq nil (mapcar
			    (lambda (x) (get-text-property 1 'time-of-day x))
			    list)))
	   (string (nth 3 org-agenda-time-grid))
	   (gridtimes (nth 1 org-agenda-time-grid))
	   (req (car org-agenda-time-grid))
	   (remove (member 'remove-match req))
	   new time)
      (if (and (member 'require-timed req) (not have))
	  ;; don't show empty grid
	  (throw 'exit list))
      (while (setq time (pop gridtimes))
	(unless (and remove (member time have))
	  (setq time (replace-regexp-in-string " " "0" (format "%04s" time)))
	  (push (org-agenda-format-item
		 nil string nil "" nil
		 (concat (substring time 0 -2) ":" (substring time -2)))
		new)
	  (put-text-property
	   2 (length (car new)) 'face 'org-time-grid (car new))))
      (when (and todayp org-agenda-show-current-time-in-grid)
	(push (org-agenda-format-item
	       nil org-agenda-current-time-string nil "" nil
	       (format-time-string "%H:%M "))
	      new)
	(put-text-property
	 2 (length (car new)) 'face 'org-agenda-current-time (car new)))

      (if (member 'time-up org-agenda-sorting-strategy-selected)
	  (append new list)
	(append list new)))))

(defun org-compile-prefix-format (key)
  "Compile the prefix format into a Lisp form that can be evaluated.
The resulting form and associated variable bindings is returned
and stored in the variable `org-prefix-format-compiled'."
  (setq org-prefix-has-time nil
	org-prefix-has-tag nil
	org-prefix-category-length nil
	org-prefix-has-effort nil
	org-prefix-has-breadcrumbs nil)
  (let ((s (cond
	    ((stringp org-agenda-prefix-format)
	     org-agenda-prefix-format)
	    ((assq key org-agenda-prefix-format)
	     (cdr (assq key org-agenda-prefix-format)))
	    (t "  %-12:c%?-12t% s")))
	(start 0)
	varform vars var e c f opt)
    (while (string-match "%\\(\\?\\)?\\([-+]?[0-9.]*\\)\\([ .;,:!?=|/<>]?\\)\\([cltseib]\\|(.+)\\)"
			 s start)
      (setq var (or (cdr (assoc (match-string 4 s)
				'(("c" . category) ("t" . time) ("l" . level) ("s" . extra)
				  ("i" . category-icon) ("T" . tag) ("e" . effort) ("b" . breadcrumbs))))
		    'eval)
	    c (or (match-string 3 s) "")
	    opt (match-beginning 1)
	    start (1+ (match-beginning 0)))
      (if (eq var 'time) (setq org-prefix-has-time t))
      (if (eq var 'tag)  (setq org-prefix-has-tag  t))
      (if (eq var 'effort) (setq org-prefix-has-effort t))
      (if (eq var 'breadcrumbs) (setq org-prefix-has-breadcrumbs t))
      (setq f (concat "%" (match-string 2 s) "s"))
      (when (eq var 'category)
	(setq org-prefix-category-length
	      (floor (abs (string-to-number (match-string 2 s)))))
	(setq org-prefix-category-max-length
	      (let ((x (match-string 2 s)))
		(save-match-data
		  (if (string-match "\\.[0-9]+" x)
		      (string-to-number (substring (match-string 0 x) 1)))))))
      (if (eq var 'eval)
	  (setq varform `(format ,f (org-eval ,(read (match-string 4 s)))))
	(if opt
	    (setq varform
		  `(if (or (equal "" ,var) (equal nil ,var))
		       ""
		     (format ,f (concat ,var ,c))))
	  (setq varform
		`(format ,f (if (or (equal ,var "")
				    (equal ,var nil)) ""
			      (concat ,var ,c (get-text-property 0 'extra-space ,var)))))))
      (setq s (replace-match "%s" t nil s))
      (push varform vars))
    (setq vars (nreverse vars))
    (with-current-buffer (or org-agenda-buffer (current-buffer))
      (setq org-prefix-format-compiled
	    (list
	     `((org-prefix-has-time ,org-prefix-has-time)
	       (org-prefix-has-tag ,org-prefix-has-tag)
	       (org-prefix-category-length ,org-prefix-category-length)
	       (org-prefix-has-effort ,org-prefix-has-effort)
	       (org-prefix-has-breadcrumbs ,org-prefix-has-breadcrumbs))
	     `(format ,s ,@vars))))))

(defun org-set-sorting-strategy (key)
  (if (symbolp (car org-agenda-sorting-strategy))
      ;; the old format
      (setq org-agenda-sorting-strategy-selected org-agenda-sorting-strategy)
    (setq org-agenda-sorting-strategy-selected
	  (or (cdr (assq key org-agenda-sorting-strategy))
	      (cdr (assq 'agenda org-agenda-sorting-strategy))
	      '(time-up category-keep priority-down)))))

(defun org-get-time-of-day (s &optional string mod24)
  "Check string S for a time of day.
If found, return it as a military time number between 0 and 2400.
If not found, return nil.
The optional STRING argument forces conversion into a 5 character wide string
HH:MM."
  (save-match-data
    (when
	(and
	 (or (string-match "\\<\\([012]?[0-9]\\)\\(:\\([0-5][0-9]\\)\\)\\([AaPp][Mm]\\)?\\> *" s)
	     (string-match "\\<\\([012]?[0-9]\\)\\(:\\([0-5][0-9]\\)\\)?\\([AaPp][Mm]\\)\\> *" s))
	 (not (eq (get-text-property 1 'face s) 'org-link)))
      (let* ((h (string-to-number (match-string 1 s)))
	     (m (if (match-end 3) (string-to-number (match-string 3 s)) 0))
	     (ampm (if (match-end 4) (downcase (match-string 4 s))))
	     (am-p (equal ampm "am"))
	     (h1   (cond ((not ampm) h)
			 ((= h 12) (if am-p 0 12))
			 (t (+ h (if am-p 0 12)))))
	     (h2 (if (and string mod24 (not (and (= m 0) (= h1 24))))
		     (mod h1 24) h1))
	     (t0 (+ (* 100 h2) m))
	     (t1 (concat (if (>= h1 24) "+" " ")
			 (if (and org-agenda-time-leading-zero
				  (< t0 1000)) "0" "")
			 (if (< t0 100) "0" "")
			 (if (< t0 10)  "0" "")
			 (int-to-string t0))))
	(if string (concat (substring t1 -4 -2) ":" (substring t1 -2)) t0)))))

(defvar org-agenda-before-sorting-filter-function nil
  "Function to be applied to agenda items prior to sorting.
Prior to sorting also means just before they are inserted into the agenda.

To aid sorting, you may revisit the original entries and add more text
properties which will later be used by the sorting functions.

The function should take a string argument, an agenda line.
It has access to the text properties in that line, which contain among
other things, the property `org-hd-marker' that points to the entry
where the line comes from.  Note that not all lines going into the agenda
have this property, only most.

The function should return the modified string.  It is probably best
to ONLY change text properties.

You can also use this function as a filter, by returning nil for lines
you don't want to have in the agenda at all.  For this application, you
could bind the variable in the options section of a custom command.")

(defun org-agenda-finalize-entries (list &optional type)
  "Sort, limit and concatenate the LIST of agenda items.
The optional argument TYPE tells the agenda type."
  (let ((max-effort (cond ((listp org-agenda-max-effort)
			   (cdr (assoc type org-agenda-max-effort)))
			  (t org-agenda-max-effort)))
	(max-todo (cond ((listp org-agenda-max-todos)
			 (cdr (assoc type org-agenda-max-todos)))
			(t org-agenda-max-todos)))
	(max-tags (cond ((listp org-agenda-max-tags)
			 (cdr (assoc type org-agenda-max-tags)))
			(t org-agenda-max-tags)))
	(max-entries (cond ((listp org-agenda-max-entries)
			    (cdr (assoc type org-agenda-max-entries)))
			   (t org-agenda-max-entries))))
    (when org-agenda-before-sorting-filter-function
      (setq list
	    (delq nil
		  (mapcar
		   org-agenda-before-sorting-filter-function list))))
    (setq list (mapcar 'org-agenda-highlight-todo list)
	  list (mapcar 'identity (sort list 'org-entries-lessp)))
    (when max-effort
      (setq list (org-agenda-limit-entries
		  list 'effort-minutes max-effort
		  (lambda (e) (or e (if org-sort-agenda-noeffort-is-high
					32767 -1))))))
    (when max-todo
      (setq list (org-agenda-limit-entries list 'todo-state max-todo)))
    (when max-tags
      (setq list (org-agenda-limit-entries list 'tags max-tags)))
    (when max-entries
      (setq list (org-agenda-limit-entries list 'org-hd-marker max-entries)))
    (when (and org-agenda-dim-blocked-tasks org-blocker-hook)
      (setq list (mapcar #'org-agenda--mark-blocked-entry list)))
    (mapconcat 'identity list "\n")))

(defun org-agenda-limit-entries (list prop limit &optional fn)
  "Limit the number of agenda entries."
  (let ((include (and limit (< limit 0))))
    (if limit
	(let ((fun (or fn (lambda (p) (if p 1))))
	      (lim 0))
	  (delq nil
		(mapcar
		 (lambda (e)
		   (let ((pval (funcall
				fun (get-text-property (1- (length e))
						       prop e))))
		     (if pval (setq lim (+ lim pval)))
		     (cond ((and pval (<= lim (abs limit))) e)
			   ((and include (not pval)) e))))
		 list)))
      list)))

(defun org-agenda-limit-interactively (remove)
  "In agenda, interactively limit entries to various maximums."
  (interactive "P")
  (if remove
      (progn (setq org-agenda-max-entries nil
		   org-agenda-max-todos nil
		   org-agenda-max-tags nil
		   org-agenda-max-effort nil)
	     (org-agenda-redo))
    (let* ((max (read-char "Number of [e]ntries [t]odos [T]ags [E]ffort? "))
	   (msg (cond ((= max ?E) "How many minutes? ")
		      ((= max ?e) "How many entries? ")
		      ((= max ?t) "How many TODO entries? ")
		      ((= max ?T) "How many tagged entries? ")
		      (t (user-error "Wrong input"))))
	   (num (string-to-number (read-from-minibuffer msg))))
      (cond ((equal max ?e)
	     (let ((org-agenda-max-entries num)) (org-agenda-redo)))
	    ((equal max ?t)
	     (let ((org-agenda-max-todos num)) (org-agenda-redo)))
	    ((equal max ?T)
	     (let ((org-agenda-max-tags num)) (org-agenda-redo)))
	    ((equal max ?E)
	     (let ((org-agenda-max-effort num)) (org-agenda-redo))))))
  (org-agenda-fit-window-to-buffer))

(defun org-agenda-highlight-todo (x)
  (let ((org-done-keywords org-done-keywords-for-agenda)
	(case-fold-search nil)
	re)
    (if (eq x 'line)
	(save-excursion
	  (beginning-of-line 1)
	  (setq re (org-get-at-bol 'org-todo-regexp))
	  (goto-char (or (text-property-any (point-at-bol) (point-at-eol) 'org-heading t) (point)))
	  (when (looking-at (concat "[ \t]*\\.*\\(" re "\\) +"))
	    (add-text-properties (match-beginning 0) (match-end 1)
				 (list 'face (org-get-todo-face 1)))
	    (let ((s (buffer-substring (match-beginning 1) (match-end 1))))
	      (delete-region (match-beginning 1) (1- (match-end 0)))
	      (goto-char (match-beginning 1))
	      (insert (format org-agenda-todo-keyword-format s)))))
      (let ((pl (text-property-any 0 (length x) 'org-heading t x)))
	(setq re (get-text-property 0 'org-todo-regexp x))
	(when (and re
		   ;; Test `pl' because if there's no heading content,
		   ;; there's no point matching to highlight.  Note
		   ;; that if we didn't test `pl' first, and there
		   ;; happened to be no keyword from `org-todo-regexp'
		   ;; on this heading line, then the `equal' comparison
		   ;; afterwards would spuriously succeed in the case
		   ;; where `pl' is nil -- causing an args-out-of-range
		   ;; error when we try to add text properties to text
		   ;; that isn't there.
		   pl
		   (equal (string-match (concat "\\(\\.*\\)" re "\\( +\\)")
					x pl) pl))
	  (add-text-properties
	   (or (match-end 1) (match-end 0)) (match-end 0)
	   (list 'face (org-get-todo-face (match-string 2 x)))
	   x)
	  (when (match-end 1)
	    (setq x
		  (concat
		   (substring x 0 (match-end 1))
		   (format org-agenda-todo-keyword-format
			   (match-string 2 x))
		   ;; Remove `display' property as the icon could leak
		   ;; on the white space.
		   (org-add-props " " (org-plist-delete (text-properties-at 0 x)
							'display))
		   (substring x (match-end 3)))))))
      x)))

(defsubst org-cmp-values (a b property)
  "Compare the numeric value of text PROPERTY for string A and B."
  (let ((pa (or (get-text-property (1- (length a)) property a) 0))
	(pb (or (get-text-property (1- (length b)) property b) 0)))
    (cond ((> pa pb) +1)
	  ((< pa pb) -1))))

(defsubst org-cmp-effort (a b)
  "Compare the effort values of string A and B."
  (let* ((def (if org-sort-agenda-noeffort-is-high 32767 -1))
	 ;; `effort-minutes' property is not directly accessible from
	 ;; the strings, but is stored as a property in `txt'.
	 (ea (or (get-text-property
		  0 'effort-minutes (get-text-property 0 'txt a))
		 def))
	 (eb (or (get-text-property
		  0 'effort-minutes (get-text-property 0 'txt b))
		 def)))
    (cond ((> ea eb) +1)
	  ((< ea eb) -1))))

(defsubst org-cmp-category (a b)
  "Compare the string values of categories of strings A and B."
  (let ((ca (or (get-text-property (1- (length a)) 'org-category a) ""))
	(cb (or (get-text-property (1- (length b)) 'org-category b) "")))
    (cond ((string-lessp ca cb) -1)
	  ((string-lessp cb ca) +1))))

(defsubst org-cmp-todo-state (a b)
  "Compare the todo states of strings A and B."
  (let* ((ma (or (get-text-property 1 'org-marker a)
		 (get-text-property 1 'org-hd-marker a)))
	 (mb (or (get-text-property 1 'org-marker b)
		 (get-text-property 1 'org-hd-marker b)))
	 (fa (and ma (marker-buffer ma)))
	 (fb (and mb (marker-buffer mb)))
	 (todo-kwds
	  (or (and fa (with-current-buffer fa org-todo-keywords-1))
	      (and fb (with-current-buffer fb org-todo-keywords-1))))
	 (ta (or (get-text-property 1 'todo-state a) ""))
	 (tb (or (get-text-property 1 'todo-state b) ""))
	 (la (- (length (member ta todo-kwds))))
	 (lb (- (length (member tb todo-kwds))))
	 (donepa (member ta org-done-keywords-for-agenda))
	 (donepb (member tb org-done-keywords-for-agenda)))
    (cond ((and donepa (not donepb)) -1)
	  ((and (not donepa) donepb) +1)
	  ((< la lb) -1)
	  ((< lb la) +1))))

(defsubst org-cmp-alpha (a b)
  "Compare the headlines, alphabetically."
  (let* ((pla (text-property-any 0 (length a) 'org-heading t a))
	 (plb (text-property-any 0 (length b) 'org-heading t b))
	 (ta (and pla (substring a pla)))
	 (tb (and plb (substring b plb)))
	 (case-fold-search nil))
    (when pla
      (if (string-match (concat "\\`[ \t]*" (or (get-text-property 0 'org-todo-regexp a) "")
				"\\([ \t]*\\[[a-zA-Z0-9]\\]\\)? *") ta)
	  (setq ta (substring ta (match-end 0))))
      (setq ta (downcase ta)))
    (when plb
      (if (string-match (concat "\\`[ \t]*" (or (get-text-property 0 'org-todo-regexp b) "")
				"\\([ \t]*\\[[a-zA-Z0-9]\\]\\)? *") tb)
	  (setq tb (substring tb (match-end 0))))
      (setq tb (downcase tb)))
    (cond ((not ta) +1)
	  ((not tb) -1)
	  ((string-lessp ta tb) -1)
	  ((string-lessp tb ta) +1))))

(defsubst org-cmp-tag (a b)
  "Compare the string values of the first tags of A and B."
  (let ((ta (car (last (get-text-property 1 'tags a))))
	(tb (car (last (get-text-property 1 'tags b)))))
    (cond ((not ta) +1)
	  ((not tb) -1)
	  ((string-lessp ta tb) -1)
	  ((string-lessp tb ta) +1))))

(defsubst org-cmp-time (a b)
  "Compare the time-of-day values of strings A and B."
  (let* ((def (if org-sort-agenda-notime-is-late 9901 -1))
	 (ta (or (get-text-property 1 'time-of-day a) def))
	 (tb (or (get-text-property 1 'time-of-day b) def)))
    (cond ((< ta tb) -1)
	  ((< tb ta) +1))))

(defsubst org-cmp-ts (a b type)
  "Compare the timestamps values of entries A and B.
When TYPE is \"scheduled\", \"deadline\", \"timestamp\" or
\"timestamp_ia\", compare within each of these type.  When TYPE
is the empty string, compare all timestamps without respect of
their type."
  (let* ((def (if org-sort-agenda-notime-is-late most-positive-fixnum -1))
	 (ta (or (and (string-match type (or (get-text-property 1 'type a) ""))
		      (get-text-property 1 'ts-date a))
		 def))
	 (tb (or (and (string-match type (or (get-text-property 1 'type b) ""))
		      (get-text-property 1 'ts-date b))
		 def)))
    (cond ((< ta tb) -1)
	  ((< tb ta) +1))))

(defsubst org-cmp-habit-p (a b)
  "Compare the todo states of strings A and B."
  (let ((ha (get-text-property 1 'org-habit-p a))
	(hb (get-text-property 1 'org-habit-p b)))
    (cond ((and ha (not hb)) -1)
	  ((and (not ha) hb) +1))))

(defun org-entries-lessp (a b)
  "Predicate for sorting agenda entries."
  ;; The following variables will be used when the form is evaluated.
  ;; So even though the compiler complains, keep them.
  (let* ((ss org-agenda-sorting-strategy-selected)
	 (timestamp-up    (and (org-em 'timestamp-up 'timestamp-down ss)
			       (org-cmp-ts a b "")))
	 (timestamp-down  (if timestamp-up (- timestamp-up) nil))
	 (scheduled-up    (and (org-em 'scheduled-up 'scheduled-down ss)
			       (org-cmp-ts a b "scheduled")))
	 (scheduled-down  (if scheduled-up (- scheduled-up) nil))
	 (deadline-up     (and (org-em 'deadline-up 'deadline-down ss)
			       (org-cmp-ts a b "deadline")))
	 (deadline-down   (if deadline-up (- deadline-up) nil))
	 (tsia-up         (and (org-em 'tsia-up 'tsia-down ss)
			       (org-cmp-ts a b "timestamp_ia")))
	 (tsia-down       (if tsia-up (- tsia-up) nil))
	 (ts-up           (and (org-em 'ts-up 'ts-down ss)
			       (org-cmp-ts a b "timestamp")))
	 (ts-down         (if ts-up (- ts-up) nil))
	 (time-up         (and (org-em 'time-up 'time-down ss)
			       (org-cmp-time a b)))
	 (time-down       (if time-up (- time-up) nil))
	 (stats-up        (and (org-em 'stats-up 'stats-down ss)
			       (org-cmp-values a b 'org-stats)))
	 (stats-down      (if stats-up (- stats-up) nil))
	 (priority-up     (and (org-em 'priority-up 'priority-down ss)
			       (org-cmp-values a b 'priority)))
	 (priority-down   (if priority-up (- priority-up) nil))
	 (effort-up       (and (org-em 'effort-up 'effort-down ss)
			       (org-cmp-effort a b)))
	 (effort-down     (if effort-up (- effort-up) nil))
	 (category-up     (and (or (org-em 'category-up 'category-down ss)
				   (memq 'category-keep ss))
			       (org-cmp-category a b)))
	 (category-down   (if category-up (- category-up) nil))
	 (category-keep   (if category-up +1 nil))
	 (tag-up          (and (org-em 'tag-up 'tag-down ss)
			       (org-cmp-tag a b)))
	 (tag-down        (if tag-up (- tag-up) nil))
	 (todo-state-up   (and (org-em 'todo-state-up 'todo-state-down ss)
			       (org-cmp-todo-state a b)))
	 (todo-state-down (if todo-state-up (- todo-state-up) nil))
	 (habit-up        (and (org-em 'habit-up 'habit-down ss)
			       (org-cmp-habit-p a b)))
	 (habit-down      (if habit-up (- habit-up) nil))
	 (alpha-up        (and (org-em 'alpha-up 'alpha-down ss)
			       (org-cmp-alpha a b)))
	 (alpha-down      (if alpha-up (- alpha-up) nil))
	 (need-user-cmp   (org-em 'user-defined-up 'user-defined-down ss))
	 user-defined-up user-defined-down)
    (if (and need-user-cmp org-agenda-cmp-user-defined
	     (functionp org-agenda-cmp-user-defined))
	(setq user-defined-up
	      (funcall org-agenda-cmp-user-defined a b)
	      user-defined-down (if user-defined-up (- user-defined-up) nil)))
    (cdr (assoc
	  (eval (cons 'or org-agenda-sorting-strategy-selected))
	  '((-1 . t) (1 . nil) (nil . nil))))))

;;; Agenda restriction lock

(defvar org-agenda-restriction-lock-overlay (make-overlay 1 1)
  "Overlay to mark the headline to which agenda commands are restricted.")
(overlay-put org-agenda-restriction-lock-overlay
	     'face 'org-agenda-restriction-lock)
(overlay-put org-agenda-restriction-lock-overlay
	     'help-echo "Agendas are currently limited to this subtree.")
(delete-overlay org-agenda-restriction-lock-overlay)

(defun org-agenda-set-restriction-lock-from-agenda (arg)
  "Set the restriction lock to the agenda item at point from within the agenda.
When called with a `\\[universal-argument]' prefix, restrict to
the file which contains the item.
Argument ARG is the prefix argument."
  (interactive "P")
  (unless  (derived-mode-p 'org-agenda-mode)
    (user-error "Not in an Org agenda buffer"))
  (let* ((marker (or (org-get-at-bol 'org-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    (with-current-buffer buffer
      (goto-char pos)
      (org-agenda-set-restriction-lock arg))))

;;;###autoload
(defun org-agenda-set-restriction-lock (&optional type)
  "Set restriction lock for agenda, to current subtree or file.
Restriction will be the file if TYPE is `file', or if type is the
universal prefix \\='(4), or if the cursor is before the first headline
in the file.  Otherwise, restriction will be to the current subtree."
  (interactive "P")
  (org-agenda-remove-restriction-lock 'noupdate)
  (and (equal type '(4)) (setq type 'file))
  (setq type (cond
	      (type type)
	      ((org-at-heading-p) 'subtree)
	      ((condition-case nil (org-back-to-heading t) (error nil))
	       'subtree)
	      (t 'file)))
  (if (eq type 'subtree)
      (progn
	(setq org-agenda-restrict (current-buffer))
	(setq org-agenda-overriding-restriction 'subtree)
	(put 'org-agenda-files 'org-restrict
	     (list (buffer-file-name (buffer-base-buffer))))
	(org-back-to-heading t)
	(move-overlay org-agenda-restriction-lock-overlay
		      (point)
		      (if org-agenda-restriction-lock-highlight-subtree
			  (save-excursion (org-end-of-subtree t t) (point))
			(point-at-eol)))
	(move-marker org-agenda-restrict-begin (point))
	(move-marker org-agenda-restrict-end
		     (save-excursion (org-end-of-subtree t t)))
	(message "Locking agenda restriction to subtree"))
    (put 'org-agenda-files 'org-restrict
	 (list (buffer-file-name (buffer-base-buffer))))
    (setq org-agenda-restrict nil)
    (setq org-agenda-overriding-restriction 'file)
    (move-marker org-agenda-restrict-begin nil)
    (move-marker org-agenda-restrict-end nil)
    (message "Locking agenda restriction to file"))
  (setq current-prefix-arg nil)
  (org-agenda-maybe-redo))

(defun org-agenda-remove-restriction-lock (&optional noupdate)
  "Remove the agenda restriction lock."
  (interactive "P")
  (delete-overlay org-agenda-restriction-lock-overlay)
  (delete-overlay org-speedbar-restriction-lock-overlay)
  (setq org-agenda-overriding-restriction nil)
  (setq org-agenda-restrict nil)
  (put 'org-agenda-files 'org-restrict nil)
  (move-marker org-agenda-restrict-begin nil)
  (move-marker org-agenda-restrict-end nil)
  (setq current-prefix-arg nil)
  (message "Agenda restriction lock removed")
  (or noupdate (org-agenda-maybe-redo)))

(defun org-agenda-maybe-redo ()
  "If there is any window showing the agenda view, update it."
  (let ((w (get-buffer-window (or org-agenda-this-buffer-name
				  org-agenda-buffer-name)
			      t))
	(w0 (selected-window)))
    (when w
      (select-window w)
      (org-agenda-redo)
      (select-window w0)
      (if org-agenda-overriding-restriction
	  (message "Agenda view shifted to new %s restriction"
		   org-agenda-overriding-restriction)
	(message "Agenda restriction lock removed")))))

;;; Agenda commands

(defun org-agenda-check-type (error &rest types)
  "Check if agenda buffer is of allowed type.
If ERROR is non-nil, throw an error, otherwise just return nil.
Allowed types are `agenda' `todo' `tags' `search'."
  (cond ((not org-agenda-type)
	 (error "No Org agenda currently displayed"))
	((memq org-agenda-type types) t)
	(error
	 (error "Not allowed in %s-type agenda buffers" org-agenda-type))
	(t nil)))

(defun org-agenda-Quit ()
  "Exit the agenda, killing the agenda buffer.
Like `org-agenda-quit', but kill the buffer even when
`org-agenda-sticky' is non-nil."
  (interactive)
  (org-agenda--quit))

(defun org-agenda-quit ()
  "Exit the agenda.

When `org-agenda-sticky' is non-nil, bury the agenda buffer
instead of killing it.

When `org-agenda-restore-windows-after-quit' is non-nil, restore
the pre-agenda window configuration.

When column view is active, exit column view instead of the
agenda."
  (interactive)
  (org-agenda--quit org-agenda-sticky))

(defun org-agenda--quit (&optional bury)
  (if org-agenda-columns-active
      (org-columns-quit)
    (let ((wconf org-agenda-pre-window-conf)
	  (buf (current-buffer))
	  (org-agenda-last-indirect-window
	   (and (eq org-indirect-buffer-display 'other-window)
		org-agenda-last-indirect-buffer
		(get-buffer-window org-agenda-last-indirect-buffer))))
      (cond
       ((eq org-agenda-window-setup 'other-frame)
	(delete-frame))
       ((and org-agenda-restore-windows-after-quit
	     wconf)
	;; Maybe restore the pre-agenda window configuration.  Reset
	;; `org-agenda-pre-window-conf' before running
	;; `set-window-configuration', which loses the current buffer.
	(setq org-agenda-pre-window-conf nil)
	(set-window-configuration wconf))
       (t
	(when org-agenda-last-indirect-window
	  (delete-window org-agenda-last-indirect-window))
	(and (not (eq org-agenda-window-setup 'current-window))
	     (not (one-window-p))
	     (delete-window))))
      (if bury
	  ;; Set the agenda buffer as the current buffer instead of
	  ;; passing it as an argument to `bury-buffer' so that
	  ;; `bury-buffer' removes it from the window.
	  (with-current-buffer buf
	    (bury-buffer))
	(kill-buffer buf)
	(setq org-agenda-archives-mode nil
	      org-agenda-buffer nil)))))

(defun org-agenda-exit ()
  "Exit the agenda, killing Org buffers loaded by the agenda.
Like `org-agenda-Quit', but kill any buffers that were created by
the agenda.  Org buffers visited directly by the user will not be
touched.  Also, exit the agenda even if it is in column view."
  (interactive)
  (when org-agenda-columns-active
    (org-columns-quit))
  (org-release-buffers org-agenda-new-buffers)
  (setq org-agenda-new-buffers nil)
  (org-agenda-Quit))

(defun org-agenda-kill-all-agenda-buffers ()
  "Kill all buffers in `org-agenda-mode'.
This is used when toggling sticky agendas."
  (interactive)
  (let (blist)
    (dolist (buf (buffer-list))
      (when (with-current-buffer buf (eq major-mode 'org-agenda-mode))
	(push buf blist)))
    (mapc 'kill-buffer blist)))

(defun org-agenda-execute (arg)
  "Execute another agenda command, keeping same window.
So this is just a shortcut for \\<global-map>`\\[org-agenda]', available
in the agenda."
  (interactive "P")
  (let ((org-agenda-window-setup 'current-window))
    (org-agenda arg)))

(defun org-agenda-redo (&optional all)
  "Rebuild possibly ALL agenda view(s) in the current buffer."
  (interactive "P")
  (let* ((p (or (and (looking-at "\\'") (1- (point))) (point)))
	 (cpa (unless (eq all t) current-prefix-arg))
	 (org-agenda-doing-sticky-redo org-agenda-sticky)
	 (org-agenda-sticky nil)
	 (org-agenda-buffer-name (or org-agenda-this-buffer-name
				     org-agenda-buffer-name))
	 (org-agenda-keep-modes t)
	 (tag-filter org-agenda-tag-filter)
	 (tag-preset (get 'org-agenda-tag-filter :preset-filter))
	 (top-hl-filter org-agenda-top-headline-filter)
	 (cat-filter org-agenda-category-filter)
	 (cat-preset (get 'org-agenda-category-filter :preset-filter))
	 (re-filter org-agenda-regexp-filter)
	 (re-preset (get 'org-agenda-regexp-filter :preset-filter))
	 (effort-filter org-agenda-effort-filter)
	 (effort-preset (get 'org-agenda-effort-filter :preset-filter))
	 (org-agenda-tag-filter-while-redo (or tag-filter tag-preset))
	 (cols org-agenda-columns-active)
	 (line (org-current-line))
	 (window-line (- line (org-current-line (window-start))))
	 (lprops (get 'org-agenda-redo-command 'org-lprops))
	 (redo-cmd (get-text-property p 'org-redo-cmd))
	 (last-args (get-text-property p 'org-last-args))
	 (org-agenda-overriding-cmd (get-text-property p 'org-series-cmd))
	 (org-agenda-overriding-cmd-arguments
	  (unless (eq all t)
	    (cond ((listp last-args)
		   (cons (or cpa (car last-args)) (cdr last-args)))
		  ((stringp last-args)
		   last-args))))
	 (series-redo-cmd (get-text-property p 'org-series-redo-cmd)))
    (put 'org-agenda-tag-filter :preset-filter nil)
    (put 'org-agenda-category-filter :preset-filter nil)
    (put 'org-agenda-regexp-filter :preset-filter nil)
    (put 'org-agenda-effort-filter :preset-filter nil)
    (and cols (org-columns-quit))
    (message "Rebuilding agenda buffer...")
    (if series-redo-cmd
	(eval series-redo-cmd)
      (org-let lprops redo-cmd))
    (setq org-agenda-undo-list nil
	  org-agenda-pending-undo-list nil
	  org-agenda-tag-filter tag-filter
	  org-agenda-category-filter cat-filter
	  org-agenda-regexp-filter re-filter
	  org-agenda-effort-filter effort-filter
	  org-agenda-top-headline-filter top-hl-filter)
    (message "Rebuilding agenda buffer...done")
    (put 'org-agenda-tag-filter :preset-filter tag-preset)
    (put 'org-agenda-category-filter :preset-filter cat-preset)
    (put 'org-agenda-regexp-filter :preset-filter re-preset)
    (put 'org-agenda-effort-filter :preset-filter effort-preset)
    (let ((tag (or tag-filter tag-preset))
	  (cat (or cat-filter cat-preset))
	  (effort (or effort-filter effort-preset))
	  (re (or re-filter re-preset)))
      (when tag (org-agenda-filter-apply tag 'tag t))
      (when cat (org-agenda-filter-apply cat 'category))
      (when effort (org-agenda-filter-apply effort 'effort))
      (when re  (org-agenda-filter-apply re 'regexp)))
    (and top-hl-filter (org-agenda-filter-top-headline-apply top-hl-filter))
    (and cols (called-interactively-p 'any) (org-agenda-columns))
    (org-goto-line line)
    (recenter window-line)))

(defun org-agenda-redo-all (&optional exhaustive)
  "Rebuild all agenda views in the current buffer.
With a prefix argument, do so in all agenda buffers."
  (interactive "P")
  (if exhaustive
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (derived-mode-p 'org-agenda-mode)
            (org-agenda-redo t))))
    (org-agenda-redo t)))

(defvar org-global-tags-completion-table nil)
(defvar org-agenda-filter-form nil)
(defvar org-agenda-filtered-by-category nil)

(defun org-agenda-filter-by-category (strip)
  "Filter lines in the agenda buffer that have a specific category.
The category is that of the current line.
Without prefix argument, keep only the lines of that category.
With a prefix argument, exclude the lines of that category.
"
  (interactive "P")
  (if (and org-agenda-filtered-by-category
	   org-agenda-category-filter)
      (org-agenda-filter-show-all-cat)
    (let ((cat (org-no-properties (org-get-at-eol 'org-category 1))))
      (cond
       ((and cat strip)
        (org-agenda-filter-apply
         (push (concat "-" cat) org-agenda-category-filter) 'category))
       (cat
        (org-agenda-filter-apply
         (setq org-agenda-category-filter
	       (list (concat "+" cat))) 'category))
       (t (error "No category at point"))))))

(defun org-find-top-headline (&optional pos)
  "Find the topmost parent headline and return it.
POS when non-nil is the marker or buffer position to start the
search from."
  (save-excursion
    (with-current-buffer (if (markerp pos) (marker-buffer pos) (current-buffer))
      (when pos (goto-char pos))
      ;; Skip up to the topmost parent.
      (while (org-up-heading-safe))
      (ignore-errors (nth 4 (org-heading-components))))))

(defvar org-agenda-filtered-by-top-headline nil)
(defun org-agenda-filter-by-top-headline (strip)
  "Keep only those lines that are descendants from the same top headline.
The top headline is that of the current line."
  (interactive "P")
  (if org-agenda-filtered-by-top-headline
      (progn
        (setq org-agenda-filtered-by-top-headline nil
	      org-agenda-top-headline-filter nil)
        (org-agenda-filter-show-all-top-filter))
    (let ((toph (org-find-top-headline (org-get-at-bol 'org-hd-marker))))
      (if toph (org-agenda-filter-top-headline-apply toph strip)
        (error "No top-level headline at point")))))

(defvar org-agenda-regexp-filter nil)
(defun org-agenda-filter-by-regexp (strip)
  "Filter agenda entries by a regular expression.
Regexp filters are cumulative.
With no prefix argument, keep entries matching the regexp.
With one prefix argument, filter out entries matching the regexp.
With two prefix arguments, remove the regexp filters."
  (interactive "P")
  (if (not (equal strip '(16)))
      (let ((flt (concat (if (equal strip '(4)) "-" "+")
			 (read-from-minibuffer
			  (if (equal strip '(4))
			      "Filter out entries matching regexp: "
			    "Narrow to entries matching regexp: ")))))
	(push flt org-agenda-regexp-filter)
	(org-agenda-filter-apply org-agenda-regexp-filter 'regexp))
    (org-agenda-filter-show-all-re)
    (message "Regexp filter removed")))

(defvar org-agenda-effort-filter nil)
(defun org-agenda-filter-by-effort (strip)
  "Filter agenda entries by effort.
With no prefix argument, keep entries matching the effort condition.
With one prefix argument, filter out entries matching the condition.
With two prefix arguments, remove the effort filters."
  (interactive "P")
  (cond
   ((member strip '(nil 4))
    (let* ((efforts (split-string
		     (or (cdr (assoc (concat org-effort-property "_ALL")
				     org-global-properties))
			 "0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00")))
	   ;; XXX: the following handles only up to 10 different
	   ;; effort values.
	   (allowed-keys (if (null efforts) nil
			   (mapcar (lambda (n) (mod n 10)) ;turn 10 into 0
				   (number-sequence 1 (length efforts)))))
	   (op nil))
      (while (not (memq op '(?< ?> ?=)))
	(setq op (read-char-exclusive "Effort operator? (> = or <)")))
      ;; Select appropriate duration.  Ignore non-digit characters.
      (let ((prompt
	     (apply #'format
		    (concat "Effort %c "
			    (mapconcat (lambda (s) (concat "[%d]" s))
				       efforts
				       " "))
		    op allowed-keys))
	    (eff -1))
	(while (not (memq eff allowed-keys))
	  (message prompt)
	  (setq eff (- (read-char-exclusive) 48)))
	(setq org-agenda-effort-filter
	      (list (concat (if strip "-" "+")
			    (char-to-string op)
			    ;; Numbering is 1 2 3 ... 9 0, but we want
			    ;; 0 1 2 ... 8 9.
			    (nth (mod (1- eff) 10) efforts)))))
      (org-agenda-filter-apply org-agenda-effort-filter 'effort)))
   (t (org-agenda-filter-show-all-effort)
      (message "Effort filter removed"))))

(defun org-agenda-filter-remove-all ()
  "Remove all filters from the current agenda buffer."
  (interactive)
  (when org-agenda-tag-filter
    (org-agenda-filter-show-all-tag))
  (when org-agenda-category-filter
    (org-agenda-filter-show-all-cat))
  (when org-agenda-regexp-filter
    (org-agenda-filter-show-all-re))
  (when org-agenda-top-headline-filter
    (org-agenda-filter-show-all-top-filter))
  (when org-agenda-effort-filter
    (org-agenda-filter-show-all-effort))
  (org-agenda-finalize))

(defun org-agenda-filter-by-tag (arg &optional char exclude)
  "Keep only those lines in the agenda buffer that have a specific tag.

The tag is selected with its fast selection letter, as configured.

With a `\\[universal-argument]' prefix, exclude the agenda search.

With a `\\[universal-argument] \\[universal-argument]' prefix, filter the literal tag, \
i.e. don't
filter on all its group members.

A lisp caller can specify CHAR.  EXCLUDE means that the new tag
should be used to exclude the search - the interactive user can
also press `-' or `+' to switch between filtering and excluding."
  (interactive "P")
  (let* ((alist org-tag-alist-for-agenda)
	 (tag-chars (mapconcat
		     (lambda (x) (if (and (not (symbolp (car x)))
					  (cdr x))
				     (char-to-string (cdr x))
				   ""))
		     org-tag-alist-for-agenda ""))
	 (valid-char-list (append '(?\t ?\r ?/ ?. ?\s ?q)
				  (string-to-list tag-chars)))
	 (exclude (or exclude (equal arg '(4))))
	 (expand (not (equal arg '(16))))
	 (inhibit-read-only t)
	 (current org-agenda-tag-filter)
	 a n tag)
    (unless char
      (while (not (memq char valid-char-list))
	(message
	 "%s by tag [%s ]:tag-char, [TAB]:tag, %s[/]:off, [+/-]:filter/exclude%s, [q]:quit"
	 (if exclude "Exclude" "Filter")
	 tag-chars
	 (if org-agenda-auto-exclude-function "[RET], " "")
	 (if expand "" ", no grouptag expand"))
	(setq char (read-char-exclusive))
	;; Excluding or filtering down
	(cond ((eq char ?-) (setq exclude t))
	      ((eq char ?+) (setq exclude nil)))))
    (when (eq char ?\t)
      (unless (local-variable-p 'org-global-tags-completion-table (current-buffer))
	(setq-local org-global-tags-completion-table
		    (org-global-tags-completion-table)))
      (let ((completion-ignore-case t))
	(setq tag (completing-read
		   "Tag: " org-global-tags-completion-table nil t))))
    (cond
     ((eq char ?\r)
      (org-agenda-filter-show-all-tag)
      (when org-agenda-auto-exclude-function
	(setq org-agenda-tag-filter nil)
	(dolist (tag (org-agenda-get-represented-tags))
	  (let ((modifier (funcall org-agenda-auto-exclude-function tag)))
	    (if modifier
		(push modifier org-agenda-tag-filter))))
	(if (not (null org-agenda-tag-filter))
	    (org-agenda-filter-apply org-agenda-tag-filter 'tag expand))))
     ((eq char ?/)
      (org-agenda-filter-show-all-tag)
      (when (get 'org-agenda-tag-filter :preset-filter)
	(org-agenda-filter-apply org-agenda-tag-filter 'tag expand)))
     ((eq char ?.)
      (setq org-agenda-tag-filter
	    (mapcar (lambda(tag) (concat "+" tag))
		    (org-get-at-bol 'tags)))
      (org-agenda-filter-apply org-agenda-tag-filter 'tag expand))
     ((eq char ?q)) ;If q, abort (even if there is a q-key for a tag...)
     ((or (eq char ?\s)
	  (setq a (rassoc char alist))
	  (and tag (setq a (cons tag nil))))
      (org-agenda-filter-show-all-tag)
      (setq tag (car a))
      (setq org-agenda-tag-filter
	    (cons (concat (if exclude "-" "+") tag)
		  current))
      (org-agenda-filter-apply org-agenda-tag-filter 'tag expand))
     (t (error "Invalid tag selection character %c" char)))))

(defun org-agenda-get-represented-tags ()
  "Get a list of all tags currently represented in the agenda."
  (let (p tags)
    (save-excursion
      (goto-char (point-min))
      (while (setq p (next-single-property-change (point) 'tags))
	(goto-char p)
	(mapc (lambda (x) (add-to-list 'tags x))
	      (get-text-property (point) 'tags))))
    tags))


(defun org-agenda-filter-make-matcher (filter type &optional expand)
  "Create the form that tests a line for agenda filter.  Optional
argument EXPAND can be used for the TYPE tag and will expand the
tags in the FILTER if any of the tags in FILTER are grouptags."
  (let (f f1)
    (cond
     ;; Tag filter
     ((eq type 'tag)
      (setq filter
	    (delete-dups
	     (append (get 'org-agenda-tag-filter :preset-filter)
		     filter)))
      (dolist (x filter)
	(let ((op (string-to-char x)))
	  (if expand (setq x (org-agenda-filter-expand-tags (list x) t))
	    (setq x (list x)))
	  (setq f1 (org-agenda-filter-make-matcher-tag-exp x op))
	  (push f1 f))))
     ;; Category filter
     ((eq type 'category)
      (setq filter
	    (delete-dups
	     (append (get 'org-agenda-category-filter :preset-filter)
		     filter)))
      (dolist (x filter)
	(if (equal "-" (substring x 0 1))
	    (setq f1 (list 'not (list 'equal (substring x 1) 'cat)))
	  (setq f1 (list 'equal (substring x 1) 'cat)))
	(push f1 f)))
     ;; Regexp filter
     ((eq type 'regexp)
      (setq filter
	    (delete-dups
	     (append (get 'org-agenda-regexp-filter :preset-filter)
		     filter)))
      (dolist (x filter)
	(if (equal "-" (substring x 0 1))
	    (setq f1 (list 'not (list 'string-match (substring x 1) 'txt)))
	  (setq f1 (list 'string-match (substring x 1) 'txt)))
	(push f1 f)))
     ;; Effort filter
     ((eq type 'effort)
      (setq filter
	    (delete-dups
	     (append (get 'org-agenda-effort-filter :preset-filter)
		     filter)))
      (dolist (x filter)
	(push (org-agenda-filter-effort-form x) f))))
    (cons 'and (nreverse f))))

(defun org-agenda-filter-make-matcher-tag-exp (tags op)
  "Return a form associated to tag-expression TAGS.
Build a form testing a line for agenda filter for
tag-expressions.  OP is an operator of type CHAR that allows the
function to set the right switches in the returned form."
  (let (form)
    ;; Any of the expressions can match if OP is +, all must match if
    ;; the operator is -.
    (dolist (x tags (cons (if (eq op ?-) 'and 'or) form))
      (let* ((tag (substring x 1))
	     (f (cond
		 ((string= "" tag) '(not tags))
		 ((and (string-match-p "\\`{" tag) (string-match-p "}\\'" tag))
		  ;; TAG is a regexp.
		  (list 'org-match-any-p (substring tag 1 -1) 'tags))
		 (t (list 'member (downcase tag) 'tags)))))
	(push (if (eq op ?-) (list 'not f) f) form)))))

(defun org-agenda-filter-effort-form (e)
  "Return the form to compare the effort of the current line with what E says.
E looks like \"+<2:25\"."
  (let (op)
    (setq e (substring e 1))
    (setq op (string-to-char e) e (substring e 1))
    (setq op (cond ((equal op ?<) '<=)
		   ((equal op ?>) '>=)
		   ((equal op ??) op)
		   (t '=)))
    (list 'org-agenda-compare-effort (list 'quote op)
	  (org-duration-to-minutes e))))

(defun org-agenda-compare-effort (op value)
  "Compare the effort of the current line with VALUE, using OP.
If the line does not have an effort defined, return nil."
  ;; `effort-minutes' property cannot be extracted directly from
  ;; current line but is stored as a property in `txt'.
  (let ((effort (get-text-property 0 'effort-minutes (org-get-at-bol 'txt))))
    (funcall op
	     (or effort (if org-sort-agenda-noeffort-is-high 32767 -1))
	     value)))

(defun org-agenda-filter-expand-tags (filter &optional no-operator)
  "Expand group tags in FILTER for the agenda.
When NO-OPERATOR is non-nil, do not add the + operator to returned tags."
  (if org-group-tags
      (let ((case-fold-search t) rtn)
	(mapc
	 (lambda (f)
	   (let (f0 dir)
	     (if (string-match "^\\([+-]\\)\\(.+\\)" f)
		 (setq dir (match-string 1 f) f0 (match-string 2 f))
	       (setq dir (if no-operator "" "+") f0 f))
	     (setq rtn (append (mapcar (lambda(f1) (concat dir f1))
				       (org-tags-expand f0 t t))
			       rtn))))
	 filter)
	(reverse rtn))
    filter))

(defun org-agenda-filter-apply (filter type &optional expand)
  "Set FILTER as the new agenda filter and apply it.  Optional
argument EXPAND can be used for the TYPE tag and will expand the
tags in the FILTER if any of the tags in FILTER are grouptags."
  ;; Deactivate `org-agenda-entry-text-mode' when filtering
  (if org-agenda-entry-text-mode (org-agenda-entry-text-mode))
  (let (tags cat txt)
    (setq org-agenda-filter-form (org-agenda-filter-make-matcher filter type expand))
    ;; Only set `org-agenda-filtered-by-category' to t when a unique
    ;; category is used as the filter:
    (setq org-agenda-filtered-by-category
	  (and (eq type 'category)
	       (not (equal (substring (car filter) 0 1) "-"))))
    (org-agenda-set-mode-name)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(if (org-get-at-bol 'org-marker)
	    (progn
	      (setq tags (org-get-at-bol 'tags)
		    cat (org-get-at-eol 'org-category 1)
		    txt (org-get-at-bol 'txt))
	      (if (not (eval org-agenda-filter-form))
		  (org-agenda-filter-hide-line type))
	      (beginning-of-line 2))
	  (beginning-of-line 2))))
    (if (get-char-property (point) 'invisible)
	(ignore-errors (org-agenda-previous-line)))))

(defun org-agenda-filter-top-headline-apply (hl &optional negative)
  "Filter by top headline HL."
  (org-agenda-set-mode-name)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((pos (org-get-at-bol 'org-hd-marker))
             (tophl (and pos (org-find-top-headline pos))))
        (if (and tophl (funcall (if negative 'identity 'not)
				(string= hl tophl)))
            (org-agenda-filter-hide-line 'top-headline)))
      (beginning-of-line 2)))
  (if (get-char-property (point) 'invisible)
      (org-agenda-previous-line))
  (setq org-agenda-top-headline-filter hl
	org-agenda-filtered-by-top-headline t))

(defun org-agenda-filter-hide-line (type)
  "Hide lines with TYPE in the agenda buffer."
  (let* ((b (max (point-min) (1- (point-at-bol))))
	 (e (point-at-eol)))
    (let ((inhibit-read-only t))
      (add-text-properties
       b e `(invisible org-filtered org-filter-type ,type)))))

(defun org-agenda-remove-filter (type)
  (interactive)
  "Remove filter of type TYPE from the agenda buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t) pos)
      (while (setq pos (text-property-any (point) (point-max) 'org-filter-type type))
	(goto-char pos)
	(remove-text-properties
	 (point) (next-single-property-change (point) 'org-filter-type)
	 `(invisible org-filtered org-filter-type ,type))))
    (set (intern (format "org-agenda-%s-filter" (intern-soft type))) nil)
    (setq org-agenda-filter-form nil)
    (org-agenda-set-mode-name)
    (org-agenda-finalize)))

(defun org-agenda-filter-show-all-tag nil
  (org-agenda-remove-filter 'tag))
(defun org-agenda-filter-show-all-re nil
  (org-agenda-remove-filter 'regexp))
(defun org-agenda-filter-show-all-effort nil
  (org-agenda-remove-filter 'effort))
(defun org-agenda-filter-show-all-cat nil
  (org-agenda-remove-filter 'category))
(defun org-agenda-filter-show-all-top-filter nil
  (org-agenda-remove-filter 'top-headline))

(defun org-agenda-manipulate-query-add ()
  "Manipulate the query by adding a search term with positive selection.
Positive selection means the term must be matched for selection of an entry."
  (interactive)
  (org-agenda-manipulate-query ?\[))
(defun org-agenda-manipulate-query-subtract ()
  "Manipulate the query by adding a search term with negative selection.
Negative selection means term must not be matched for selection of an entry."
  (interactive)
  (org-agenda-manipulate-query ?\]))
(defun org-agenda-manipulate-query-add-re ()
  "Manipulate the query by adding a search regexp with positive selection.
Positive selection means the regexp must match for selection of an entry."
  (interactive)
  (org-agenda-manipulate-query ?\{))
(defun org-agenda-manipulate-query-subtract-re ()
  "Manipulate the query by adding a search regexp with negative selection.
Negative selection means regexp must not match for selection of an entry."
  (interactive)
  (org-agenda-manipulate-query ?\}))
(defun org-agenda-manipulate-query (char)
  (cond
   ((eq org-agenda-type 'agenda)
    (let ((org-agenda-include-inactive-timestamps t))
      (org-agenda-redo))
    (message "Display now includes inactive timestamps as well"))
   ((eq org-agenda-type 'search)
    (org-add-to-string
     'org-agenda-query-string
     (if org-agenda-last-search-view-search-was-boolean
	 (cdr (assoc char '((?\[ . " +") (?\] . " -")
			    (?\{ . " +{}") (?\} . " -{}"))))
       " "))
    (setq org-agenda-redo-command
	  (list 'org-search-view
		(car (get-text-property (min (1- (point-max)) (point))
					'org-last-args))
		org-agenda-query-string
		(+ (length org-agenda-query-string)
		   (if (member char '(?\{ ?\})) 0 1))))
    (set-register org-agenda-query-register org-agenda-query-string)
    (let ((org-agenda-overriding-arguments
	   (cdr org-agenda-redo-command)))
      (org-agenda-redo)))
   (t (error "Cannot manipulate query for %s-type agenda buffers"
	     org-agenda-type))))

(defun org-add-to-string (var string)
  (set var (concat (symbol-value var) string)))

(defun org-agenda-goto-date (span)
  "Jump to DATE in agenda."
  (interactive "P")
  (let* ((org-read-date-prefer-future
	  (eval org-agenda-jump-prefer-future))
	 (date (org-read-date))
	 (day (time-to-days (org-time-string-to-time date)))
	 (org-agenda-sticky-orig org-agenda-sticky)
	 (org-agenda-buffer-tmp-name (buffer-name))
	 (args (get-text-property (min (1- (point-max)) (point)) 'org-last-args))
	 (0-arg (or current-prefix-arg (car args)))
	 (2-arg (nth 2 args))
	 (with-hour-p (nth 4 org-agenda-redo-command))
	 (newcmd (list 'org-agenda-list 0-arg date
		       (org-agenda-span-to-ndays
			2-arg (org-time-string-to-absolute date))
		       with-hour-p))
	 (newargs (cdr newcmd))
	 (inhibit-read-only t)
	 org-agenda-sticky)
    (if (not (org-agenda-check-type t 'agenda))
	(error "Not available in non-agenda views")
      (add-text-properties (point-min) (point-max)
			   `(org-redo-cmd ,newcmd org-last-args ,newargs))
      (org-agenda-redo)
      (goto-char (point-min))
      (while (not (or (= (or (get-text-property (point) 'day) 0) day)
		      (save-excursion (move-beginning-of-line 2) (eobp))))
	(move-beginning-of-line 2))
      (setq org-agenda-sticky org-agenda-sticky-orig
	    org-agenda-this-buffer-is-sticky org-agenda-sticky))))

(defun org-agenda-goto-today ()
  "Go to today."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (let* ((args (get-text-property (min (1- (point-max)) (point)) 'org-last-args))
	 (curspan (nth 2 args))
	 (tdpos (text-property-any (point-min) (point-max) 'org-today t)))
    (cond
     (tdpos (goto-char tdpos))
     ((eq org-agenda-type 'agenda)
      (let* ((sd (org-agenda-compute-starting-span
		  (org-today) (or curspan org-agenda-span)))
	     (org-agenda-overriding-arguments args))
	(setf (nth 1 org-agenda-overriding-arguments) sd)
	(org-agenda-redo)
	(org-agenda-find-same-or-today-or-agenda)))
     (t (error "Cannot find today")))))

(defun org-agenda-find-same-or-today-or-agenda (&optional cnt)
  (goto-char
   (or (and cnt (text-property-any (point-min) (point-max) 'org-day-cnt cnt))
       (text-property-any (point-min) (point-max) 'org-today t)
       (text-property-any (point-min) (point-max) 'org-agenda-type 'agenda)
       (and (get-text-property (min (1- (point-max)) (point)) 'org-series)
	    (org-agenda-backward-block))
       (point-min))))

(defun org-agenda-backward-block ()
  "Move backward by one agenda block."
  (interactive)
  (org-agenda-forward-block 'backward))

(defun org-agenda-forward-block (&optional backward)
  "Move forward by one agenda block.
When optional argument BACKWARD is set, go backward"
  (interactive)
  (cond ((not (derived-mode-p 'org-agenda-mode))
	 (user-error
	  "Cannot execute this command outside of org-agenda-mode buffers"))
	((looking-at (if backward "\\`" "\\'"))
	 (message "Already at the %s block" (if backward "first" "last")))
	(t (let ((pos (prog1 (point)
			(ignore-errors (if backward (backward-char 1)
					 (move-end-of-line 1)))))
		 (f (if backward
			'previous-single-property-change
		      'next-single-property-change))
		 moved dest)
	     (while (and (setq dest (funcall
				     f (point) 'org-agenda-structural-header))
			 (not (get-text-property
			       (point) 'org-agenda-structural-header)))
	       (setq moved t)
	       (goto-char dest))
	     (if moved (move-beginning-of-line 1)
	       (goto-char (if backward (point-min) (point-max)))
	       (move-beginning-of-line 1)
	       (message "No %s block" (if backward "previous" "further")))))))

(defun org-agenda-later (arg)
  "Go forward in time by the current span.
With prefix ARG, go forward that many times the current span."
  (interactive "p")
  (org-agenda-check-type t 'agenda)
  (let* ((args (get-text-property (min (1- (point-max)) (point)) 'org-last-args))
	 (span (or (nth 2 args) org-agenda-current-span))
	 (sd (or (nth 1 args) (org-get-at-bol 'day) org-starting-day))
	 (greg (calendar-gregorian-from-absolute sd))
	 (cnt (org-get-at-bol 'org-day-cnt))
	 greg2)
    (cond
     ((numberp span)
      (setq sd (+ (* span arg) sd)))
     ((eq span 'day)
      (setq sd (+ arg sd)))
     ((eq span 'week)
      (setq sd (+ (* 7 arg) sd)))
     ((eq span 'fortnight)
      (setq sd (+ (* 14 arg) sd)))
     ((eq span 'month)
      (setq greg2 (list (+ (car greg) arg) (nth 1 greg) (nth 2 greg))
	    sd (calendar-absolute-from-gregorian greg2))
      (setcar greg2 (1+ (car greg2))))
     ((eq span 'year)
      (setq greg2 (list (car greg) (nth 1 greg) (+ arg (nth 2 greg)))
	    sd (calendar-absolute-from-gregorian greg2))
      (setcar (nthcdr 2 greg2) (1+ (nth 2 greg2))))
     (t
      (setq sd (+ (* span arg) sd))))
    (let ((org-agenda-overriding-cmd
	   ;; `cmd' may have been set by `org-agenda-run-series' which
	   ;; uses `org-agenda-overriding-cmd' to decide whether
	   ;; overriding is allowed for `cmd'
	   (get-text-property (min (1- (point-max)) (point)) 'org-series-cmd))
	  (org-agenda-overriding-arguments
	   (list (car args) sd span)))
      (org-agenda-redo)
      (org-agenda-find-same-or-today-or-agenda cnt))))

(defun org-agenda-earlier (arg)
  "Go backward in time by the current span.
With prefix ARG, go backward that many times the current span."
  (interactive "p")
  (org-agenda-later (- arg)))

(defun org-agenda-view-mode-dispatch ()
  "Call one of the view mode commands."
  (interactive)
  (message "View: [d]ay  [w]eek  for[t]night  [m]onth  [y]ear  [SPC]reset  [q]uit/abort
      time[G]rid   [[]inactive  [f]ollow      [l]og    [L]og-all   [c]lockcheck
      [a]rch-trees [A]rch-files clock[R]eport include[D]iary       [E]ntryText")
  (pcase (read-char-exclusive)
    (?\ (call-interactively 'org-agenda-reset-view))
    (?d (call-interactively 'org-agenda-day-view))
    (?w (call-interactively 'org-agenda-week-view))
    (?t (call-interactively 'org-agenda-fortnight-view))
    (?m (call-interactively 'org-agenda-month-view))
    (?y (call-interactively 'org-agenda-year-view))
    (?l (call-interactively 'org-agenda-log-mode))
    (?L (org-agenda-log-mode '(4)))
    (?c (org-agenda-log-mode 'clockcheck))
    ((or ?F ?f) (call-interactively 'org-agenda-follow-mode))
    (?a (call-interactively 'org-agenda-archives-mode))
    (?A (org-agenda-archives-mode 'files))
    ((or ?R ?r) (call-interactively 'org-agenda-clockreport-mode))
    ((or ?E ?e) (call-interactively 'org-agenda-entry-text-mode))
    (?G (call-interactively 'org-agenda-toggle-time-grid))
    (?D (call-interactively 'org-agenda-toggle-diary))
    (?\! (call-interactively 'org-agenda-toggle-deadlines))
    (?\[ (let ((org-agenda-include-inactive-timestamps t))
	   (org-agenda-check-type t 'agenda)
	   (org-agenda-redo))
	 (message "Display now includes inactive timestamps as well"))
    (?q (message "Abort"))
    (key (user-error "Invalid key: %s" key))))

(defun org-agenda-reset-view ()
  "Switch to default view for agenda."
  (interactive)
  (org-agenda-change-time-span org-agenda-span))

(defun org-agenda-day-view (&optional day-of-month)
  "Switch to daily view for agenda.
With argument DAY-OF-MONTH, switch to that day of the month."
  (interactive "P")
  (org-agenda-change-time-span 'day day-of-month))

(defun org-agenda-week-view (&optional iso-week)
  "Switch to weekly view for agenda.
With argument ISO-WEEK, switch to the corresponding ISO week.
If ISO-WEEK has more then 2 digits, only the last two encode
the week.  Any digits before this encode a year.  So 200712
means week 12 of year 2007.  Years ranging from 70 years ago
to 30 years in the future can also be written as 2-digit years."
  (interactive "P")
  (org-agenda-change-time-span 'week iso-week))

(defun org-agenda-fortnight-view (&optional iso-week)
  "Switch to fortnightly view for agenda.
With argument ISO-WEEK, switch to the corresponding ISO week.
If ISO-WEEK has more then 2 digits, only the last two encode
the week.  Any digits before this encode a year.  So 200712
means week 12 of year 2007.  Years ranging from 70 years ago
to 30 years in the future can also be written as 2-digit years."
  (interactive "P")
  (org-agenda-change-time-span 'fortnight iso-week))

(defun org-agenda-month-view (&optional month)
  "Switch to monthly view for agenda.
With argument MONTH, switch to that month.  If MONTH has more
then 2 digits, only the last two encode the month.  Any digits
before this encode a year.  So 200712 means December year 2007.
Years ranging from 70 years ago to 30 years in the future can
also be written as 2-digit years."
  (interactive "P")
  (org-agenda-change-time-span 'month month))

(defun org-agenda-year-view (&optional year)
  "Switch to yearly view for agenda.
With argument YEAR, switch to that year.  Years ranging from 70
years ago to 30 years in the future can also be written as
2-digit years."
  (interactive "P")
  (when year
    (setq year (org-small-year-to-year year)))
  (if (y-or-n-p "Are you sure you want to compute the agenda for an entire year? ")
      (org-agenda-change-time-span 'year year)
    (error "Abort")))

(defun org-agenda-change-time-span (span &optional n)
  "Change the agenda view to SPAN.
SPAN may be `day', `week', `fortnight', `month', `year'."
  (org-agenda-check-type t 'agenda)
  (let* ((args (get-text-property (min (1- (point-max)) (point)) 'org-last-args))
	 (curspan (nth 2 args)))
    (if (and (not n) (equal curspan span))
	(error "Viewing span is already \"%s\"" span))
    (let* ((sd (or (org-get-at-bol 'day)
		   (nth 1 args)
		   org-starting-day))
	   (sd (org-agenda-compute-starting-span sd span n))
	   (org-agenda-overriding-cmd
	    (get-text-property (min (1- (point-max)) (point)) 'org-series-cmd))
	   (org-agenda-overriding-arguments
	    (list (car args) sd span)))
      (org-agenda-redo)
      (org-agenda-find-same-or-today-or-agenda))
    (org-agenda-set-mode-name)
    (message "Switched to %s view" span)))

(defun org-agenda-compute-starting-span (sd span &optional n)
  "Compute starting date for agenda.
SPAN may be `day', `week', `fortnight', `month', `year'.  The return value
is a cons cell with the starting date and the number of days,
so that the date SD will be in that range."
  (let* ((greg (calendar-gregorian-from-absolute sd))
	 (dg (nth 1 greg))
	 (mg (car greg))
	 (yg (nth 2 greg)))
    (cond
     ((eq span 'day)
      (when n
	(setq sd (+ (calendar-absolute-from-gregorian
		     (list mg 1 yg))
		    n -1))))
     ((or (eq span 'week) (eq span 'fortnight))
      (let* ((nt (calendar-day-of-week
		  (calendar-gregorian-from-absolute sd)))
	     (d (if org-agenda-start-on-weekday
		    (- nt org-agenda-start-on-weekday)
		  0))
	     y1)
	(setq sd (- sd (+ (if (< d 0) 7 0) d)))
	(when n
	  (require 'cal-iso)
	  (when (> n 99)
	    (setq y1 (org-small-year-to-year (/ n 100))
		  n (mod n 100)))
	  (setq sd
		(calendar-iso-to-absolute
		 (list n 1
		       (or y1 (nth 2 (calendar-iso-from-absolute sd)))))))))
     ((eq span 'month)
      (let (y1)
	(when (and n (> n 99))
	  (setq y1 (org-small-year-to-year (/ n 100))
		n (mod n 100)))
	(setq sd (calendar-absolute-from-gregorian
		  (list (or n mg) 1 (or y1 yg))))))
     ((eq span 'year)
      (setq sd (calendar-absolute-from-gregorian
		(list 1 1 (or n yg))))))
    sd))

(defun org-agenda-next-date-line (&optional arg)
  "Jump to the next line indicating a date in agenda buffer."
  (interactive "p")
  (org-agenda-check-type t 'agenda)
  (beginning-of-line 1)
  ;; This does not work if user makes date format that starts with a blank
  (if (looking-at "^\\S-") (forward-char 1))
  (if (not (re-search-forward "^\\S-" nil t arg))
      (progn
	(backward-char 1)
	(error "No next date after this line in this buffer")))
  (goto-char (match-beginning 0)))

(defun org-agenda-previous-date-line (&optional arg)
  "Jump to the previous line indicating a date in agenda buffer."
  (interactive "p")
  (org-agenda-check-type t 'agenda)
  (beginning-of-line 1)
  (if (not (re-search-backward "^\\S-" nil t arg))
      (error "No previous date before this line in this buffer")))

;; Initialize the highlight
(defvar org-hl (make-overlay 1 1))
(overlay-put org-hl 'face 'highlight)

(defun org-highlight (begin end &optional buffer)
  "Highlight a region with overlay."
  (move-overlay org-hl begin end (or buffer (current-buffer))))

(defun org-unhighlight ()
  "Detach overlay INDEX."
  (delete-overlay org-hl))

(defun org-unhighlight-once ()
  "Remove the highlight from its position, and this function from the hook."
  (remove-hook 'pre-command-hook 'org-unhighlight-once)
  (org-unhighlight))

(defvar org-agenda-pre-follow-window-conf nil)
(defun org-agenda-follow-mode ()
  "Toggle follow mode in an agenda buffer."
  (interactive)
  (unless org-agenda-follow-mode
    (setq org-agenda-pre-follow-window-conf
	  (current-window-configuration)))
  (setq org-agenda-follow-mode (not org-agenda-follow-mode))
  (unless org-agenda-follow-mode
    (set-window-configuration org-agenda-pre-follow-window-conf))
  (org-agenda-set-mode-name)
  (org-agenda-do-context-action)
  (message "Follow mode is %s"
	   (if org-agenda-follow-mode "on" "off")))

(defun org-agenda-entry-text-mode (&optional arg)
  "Toggle entry text mode in an agenda buffer."
  (interactive "P")
  (if (or org-agenda-tag-filter
	  org-agenda-category-filter
	  org-agenda-regexp-filter
	  org-agenda-top-headline-filter)
      (user-error "Can't show entry text in filtered views")
    (setq org-agenda-entry-text-mode (or (integerp arg)
					 (not org-agenda-entry-text-mode)))
    (org-agenda-entry-text-hide)
    (and org-agenda-entry-text-mode
	 (let ((org-agenda-entry-text-maxlines
		(if (integerp arg) arg org-agenda-entry-text-maxlines)))
	   (org-agenda-entry-text-show)))
    (org-agenda-set-mode-name)
    (message "Entry text mode is %s%s"
	     (if org-agenda-entry-text-mode "on" "off")
	     (if (not org-agenda-entry-text-mode) ""
	       (format " (maximum number of lines is %d)"
		       (if (integerp arg) arg org-agenda-entry-text-maxlines))))))

(defun org-agenda-clockreport-mode ()
  "Toggle clocktable mode in an agenda buffer."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (setq org-agenda-clockreport-mode (not org-agenda-clockreport-mode))
  (setq org-agenda-start-with-clockreport-mode org-agenda-clockreport-mode)
  (org-agenda-set-mode-name)
  (org-agenda-redo)
  (message "Clocktable mode is %s"
	   (if org-agenda-clockreport-mode "on" "off")))

(defun org-agenda-log-mode (&optional special)
  "Toggle log mode in an agenda buffer.

With argument SPECIAL, show all possible log items, not only the ones
configured in `org-agenda-log-mode-items'.

With a `\\[universal-argument] \\[universal-argument]' prefix, show *only* \
log items, nothing else."
  (interactive "P")
  (org-agenda-check-type t 'agenda)
  (setq org-agenda-show-log
	(cond
	 ((equal special '(16)) 'only)
	 ((eq special 'clockcheck)
	  (if (eq org-agenda-show-log 'clockcheck)
	      nil 'clockcheck))
	 (special '(closed clock state))
	 (t (not org-agenda-show-log))))
  (setq org-agenda-start-with-log-mode org-agenda-show-log)
  (org-agenda-set-mode-name)
  (org-agenda-redo)
  (message "Log mode is %s" (if org-agenda-show-log "on" "off")))

(defun org-agenda-archives-mode (&optional with-files)
  "Toggle inclusion of items in trees marked with :ARCHIVE:.
When called with a prefix argument, include all archive files as well."
  (interactive "P")
  (setq org-agenda-archives-mode
	(if with-files t (if org-agenda-archives-mode nil 'trees)))
  (org-agenda-set-mode-name)
  (org-agenda-redo)
  (message
   "%s"
   (cond
    ((eq org-agenda-archives-mode nil)
     "No archives are included")
    ((eq org-agenda-archives-mode 'trees)
     (format "Trees with :%s: tag are included" org-archive-tag))
    ((eq org-agenda-archives-mode t)
     (format "Trees with :%s: tag and all active archive files are included"
	     org-archive-tag)))))

(defun org-agenda-toggle-diary ()
  "Toggle diary inclusion in an agenda buffer."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (setq org-agenda-include-diary (not org-agenda-include-diary))
  (org-agenda-redo)
  (org-agenda-set-mode-name)
  (message "Diary inclusion turned %s"
	   (if org-agenda-include-diary "on" "off")))

(defun org-agenda-toggle-deadlines ()
  "Toggle inclusion of entries with a deadline in an agenda buffer."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (setq org-agenda-include-deadlines (not org-agenda-include-deadlines))
  (org-agenda-redo)
  (org-agenda-set-mode-name)
  (message "Deadlines inclusion turned %s"
	   (if org-agenda-include-deadlines "on" "off")))

(defun org-agenda-toggle-time-grid ()
  "Toggle time grid in an agenda buffer."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (setq org-agenda-use-time-grid (not org-agenda-use-time-grid))
  (org-agenda-redo)
  (org-agenda-set-mode-name)
  (message "Time-grid turned %s"
	   (if org-agenda-use-time-grid "on" "off")))

(defun org-agenda-set-mode-name ()
  "Set the mode name to indicate all the small mode settings."
  (setq mode-name
	(list "Org-Agenda"
	      (if (get 'org-agenda-files 'org-restrict) " []" "")
	      " "
	      '(:eval (org-agenda-span-name org-agenda-current-span))
	      (if org-agenda-follow-mode     " Follow" "")
	      (if org-agenda-entry-text-mode " ETxt"   "")
	      (if org-agenda-include-diary   " Diary"  "")
	      (if org-agenda-include-deadlines " Ddl"  "")
	      (if org-agenda-use-time-grid   " Grid"   "")
	      (if (and (boundp 'org-habit-show-habits)
		       org-habit-show-habits) " Habit"   "")
	      (cond
	       ((consp org-agenda-show-log) " LogAll")
	       ((eq org-agenda-show-log 'clockcheck) " ClkCk")
	       (org-agenda-show-log " Log")
	       (t ""))
	      (if (or org-agenda-category-filter
		      (get 'org-agenda-category-filter :preset-filter))
		  '(:eval (propertize
	      		   (concat " <"
	      			   (mapconcat
	      			    'identity
	      			    (append
	      			     (get 'org-agenda-category-filter :preset-filter)
	      			     org-agenda-category-filter)
	      			    "")
	      			   ">")
	      		   'face 'org-agenda-filter-category
	      		   'help-echo "Category used in filtering")) "")
	      (if (or org-agenda-tag-filter
		      (get 'org-agenda-tag-filter :preset-filter))
		  '(:eval (propertize
			   (concat " {"
				   (mapconcat
				    'identity
				    (append
				     (get 'org-agenda-tag-filter :preset-filter)
				     org-agenda-tag-filter)
				    "")
				   "}")
			   'face 'org-agenda-filter-tags
			   'help-echo "Tags used in filtering")) "")
	      (if (or org-agenda-effort-filter
		      (get 'org-agenda-effort-filter :preset-filter))
		  '(:eval (propertize
			   (concat " {"
				   (mapconcat
				    'identity
				    (append
				     (get 'org-agenda-effort-filter :preset-filter)
				     org-agenda-effort-filter)
				    "")
				   "}")
			   'face 'org-agenda-filter-effort
			   'help-echo "Effort conditions used in filtering")) "")
	      (if (or org-agenda-regexp-filter
		      (get 'org-agenda-regexp-filter :preset-filter))
		  '(:eval (propertize
			   (concat " ["
				   (mapconcat
				    'identity
				    (append
				     (get 'org-agenda-regexp-filter :preset-filter)
				     org-agenda-regexp-filter)
				    "")
				   "]")
			   'face 'org-agenda-filter-regexp
			   'help-echo "Regexp used in filtering")) "")
	      (if org-agenda-archives-mode
		  (if (eq org-agenda-archives-mode t)
		      " Archives"
		    (format " :%s:" org-archive-tag))
		"")
	      (if org-agenda-clockreport-mode " Clock" "")))
  (force-mode-line-update))

(defun org-agenda-update-agenda-type ()
  "Update the agenda type after each command."
  (setq org-agenda-type
	(or (get-text-property (point) 'org-agenda-type)
	    (get-text-property (max (point-min) (1- (point))) 'org-agenda-type))))

(defun org-agenda-next-line ()
  "Move cursor to the next line, and show if follow mode is active."
  (interactive)
  (call-interactively 'next-line)
  (org-agenda-do-context-action))

(defun org-agenda-previous-line ()
  "Move cursor to the previous line, and show if follow-mode is active."
  (interactive)
  (call-interactively 'previous-line)
  (org-agenda-do-context-action))

(defun org-agenda-next-item (n)
  "Move cursor to next agenda item."
  (interactive "p")
  (let ((col (current-column)))
    (dotimes (c n)
      (when (next-single-property-change (point-at-eol) 'org-marker)
	(move-end-of-line 1)
	(goto-char (next-single-property-change (point) 'org-marker))))
    (org-move-to-column col))
  (org-agenda-do-context-action))

(defun org-agenda-previous-item (n)
  "Move cursor to next agenda item."
  (interactive "p")
  (dotimes (c n)
    (let ((col (current-column))
	  (goto (save-excursion
		  (move-end-of-line 0)
		  (previous-single-property-change (point) 'org-marker))))
      (if goto (goto-char goto))
      (org-move-to-column col)))
  (org-agenda-do-context-action))

(defun org-agenda-do-context-action ()
  "Show outline path and, maybe, follow mode window."
  (let ((m (org-get-at-bol 'org-marker)))
    (when (and (markerp m) (marker-buffer m))
      (and org-agenda-follow-mode
	   (if org-agenda-follow-indirect
	       (org-agenda-tree-to-indirect-buffer nil)
	     (org-agenda-show)))
      (and org-agenda-show-outline-path
	   (org-with-point-at m (org-display-outline-path t))))))

(defun org-agenda-show-tags ()
  "Show the tags applicable to the current item."
  (interactive)
  (let* ((tags (org-get-at-bol 'tags)))
    (if tags
	(message "Tags are :%s:"
		 (org-no-properties (mapconcat 'identity tags ":")))
      (message "No tags associated with this line"))))

(defun org-agenda-goto (&optional highlight)
  "Go to the entry at point in the corresponding Org file."
  (interactive)
  (let* ((marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (switch-to-buffer-other-window buffer)
    (widen)
    (push-mark)
    (goto-char pos)
    (when (derived-mode-p 'org-mode)
      (org-show-context 'agenda)
      (recenter (/ (window-height) 2))
      (org-back-to-heading t)
      (let ((case-fold-search nil))
	(when (re-search-forward org-complex-heading-regexp nil t)
	  (goto-char (match-beginning 4)))))
    (run-hooks 'org-agenda-after-show-hook)
    (and highlight (org-highlight (point-at-bol) (point-at-eol)))))

(defvar org-agenda-after-show-hook nil
  "Normal hook run after an item has been shown from the agenda.
Point is in the buffer where the item originated.")

(defun org-agenda-kill ()
  "Kill the entry or subtree belonging to the current agenda entry."
  (interactive)
  (or (eq major-mode 'org-agenda-mode) (error "Not in agenda"))
  (let* ((bufname-orig (buffer-name))
	 (marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 (type (org-get-at-bol 'type))
	 dbeg dend (n 0) conf)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(save-excursion
	  (goto-char pos)
	  (if (and (derived-mode-p 'org-mode) (not (member type '("sexp"))))
	      (setq dbeg (progn (org-back-to-heading t) (point))
		    dend (org-end-of-subtree t t))
	    (setq dbeg (point-at-bol)
		  dend (min (point-max) (1+ (point-at-eol)))))
	  (goto-char dbeg)
	  (while (re-search-forward "^[ \t]*\\S-" dend t) (setq n (1+ n)))))
      (setq conf (or (eq t org-agenda-confirm-kill)
		     (and (numberp org-agenda-confirm-kill)
			  (> n org-agenda-confirm-kill))))
      (and conf
	   (not (y-or-n-p
		 (format "Delete entry with %d lines in buffer \"%s\"? "
			 n (buffer-name buffer))))
	   (error "Abort"))
      (let ((org-agenda-buffer-name bufname-orig))
	(org-remove-subtree-entries-from-agenda buffer dbeg dend))
      (with-current-buffer buffer (delete-region dbeg dend))
      (message "Agenda item and source killed"))))

(defvar org-archive-default-command) ; defined in org-archive.el
(defun org-agenda-archive-default ()
  "Archive the entry or subtree belonging to the current agenda entry."
  (interactive)
  (require 'org-archive)
  (org-agenda-archive-with org-archive-default-command))

(defun org-agenda-archive-default-with-confirmation ()
  "Archive the entry or subtree belonging to the current agenda entry."
  (interactive)
  (require 'org-archive)
  (org-agenda-archive-with org-archive-default-command 'confirm))

(defun org-agenda-archive ()
  "Archive the entry or subtree belonging to the current agenda entry."
  (interactive)
  (org-agenda-archive-with 'org-archive-subtree))

(defun org-agenda-archive-to-archive-sibling ()
  "Move the entry to the archive sibling."
  (interactive)
  (org-agenda-archive-with 'org-archive-to-archive-sibling))

(defun org-agenda-archive-with (cmd &optional confirm)
  "Move the entry to the archive sibling."
  (interactive)
  (or (eq major-mode 'org-agenda-mode) (error "Not in agenda"))
  (let* ((bufname-orig (buffer-name))
	 (marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(if (derived-mode-p 'org-mode)
	    (if (and confirm
		     (not (y-or-n-p "Archive this subtree or entry? ")))
		(error "Abort")
	      (save-window-excursion
		(goto-char pos)
		(let ((org-agenda-buffer-name bufname-orig))
		  (org-remove-subtree-entries-from-agenda))
		(org-back-to-heading t)
		(funcall cmd)))
	  (error "Archiving works only in Org files"))))))

(defun org-remove-subtree-entries-from-agenda (&optional buf beg end)
  "Remove all lines in the agenda that correspond to a given subtree.
The subtree is the one in buffer BUF, starting at BEG and ending at END.
If this information is not given, the function uses the tree at point."
  (let ((buf (or buf (current-buffer))) m p)
    (save-excursion
      (unless (and beg end)
	(org-back-to-heading t)
	(setq beg (point))
	(org-end-of-subtree t)
	(setq end (point)))
      (set-buffer (get-buffer org-agenda-buffer-name))
      (save-excursion
	(goto-char (point-max))
	(beginning-of-line 1)
	(while (not (bobp))
	  (when (and (setq m (org-get-at-bol 'org-marker))
		     (equal buf (marker-buffer m))
		     (setq p (marker-position m))
		     (>= p beg)
		     (< p end))
	    (let ((inhibit-read-only t))
	      (delete-region (point-at-bol) (1+ (point-at-eol)))))
	  (beginning-of-line 0))))))

(defun org-agenda-refile (&optional goto rfloc no-update)
  "Refile the item at point.

When called with `\\[universal-argument] \\[universal-argument]', \
go to the location of the last
refiled item.

When called with `\\[universal-argument] \\[universal-argument] \
\\[universal-argument]' prefix or when GOTO is 0, clear
the refile cache.

RFLOC can be a refile location obtained in a different way.

When NO-UPDATE is non-nil, don't redo the agenda buffer."
  (interactive "P")
  (cond
   ((member goto '(0 (64)))
    (org-refile-cache-clear))
   ((equal goto '(16))
    (org-refile-goto-last-stored))
   (t
    (let* ((buffer-orig (buffer-name))
	   (marker (or (org-get-at-bol 'org-hd-marker)
		       (org-agenda-error)))
	   (buffer (marker-buffer marker))
	   (pos (marker-position marker))
	   (rfloc (or rfloc
		      (org-refile-get-location
		       (if goto "Goto" "Refile to") buffer
		       org-refile-allow-creating-parent-nodes))))
      (with-current-buffer buffer
	(org-with-wide-buffer
	 (goto-char marker)
	 (let ((org-agenda-buffer-name buffer-orig))
	   (org-remove-subtree-entries-from-agenda))
	 (org-refile goto buffer rfloc))))
    (unless no-update (org-agenda-redo)))))

(defun org-agenda-open-link (&optional arg)
  "Open the link(s) in the current entry, if any.
This looks for a link in the displayed line in the agenda.
It also looks at the text of the entry itself."
  (interactive "P")
  (let* ((marker (or (org-get-at-bol 'org-hd-marker)
		     (org-get-at-bol 'org-marker)))
	 (buffer (and marker (marker-buffer marker)))
	 (prefix (buffer-substring (point-at-bol) (point-at-eol)))
	 (lkall (and buffer (org-offer-links-in-entry
			     buffer marker arg prefix)))
	 (lk0 (car lkall))
	 (lk (if (stringp lk0) (list lk0) lk0))
	 (lkend (cdr lkall))
	 trg)
    (cond
     ((and buffer lk)
      (mapcar (lambda(l)
		(with-current-buffer buffer
		  (setq trg (and (string-match org-bracket-link-regexp l)
				 (match-string 1 l)))
		  (if (or (not trg) (string-match org-any-link-re trg))
		      (org-with-wide-buffer
		       (goto-char marker)
		       (when (search-forward l nil lkend)
			 (goto-char (match-beginning 0))
			 (org-open-at-point)))
		    ;; This is an internal link, widen the buffer
		    (switch-to-buffer-other-window buffer)
		    (widen)
		    (goto-char marker)
		    (when (search-forward l nil lkend)
		      (goto-char (match-beginning 0))
		      (org-open-at-point)))))
	      lk))
     ((or (org-in-regexp (concat "\\(" org-bracket-link-regexp "\\)"))
	  (save-excursion
	    (beginning-of-line 1)
	    (looking-at (concat ".*?\\(" org-bracket-link-regexp "\\)"))))
      (org-open-link-from-string (match-string 1)))
     (t (message "No link to open here")))))

(defun org-agenda-copy-local-variable (var)
  "Get a variable from a referenced buffer and install it here."
  (let ((m (org-get-at-bol 'org-marker)))
    (when (and m (buffer-live-p (marker-buffer m)))
      (set (make-local-variable var)
	   (with-current-buffer (marker-buffer m)
	     (symbol-value var))))))

(defun org-agenda-switch-to (&optional delete-other-windows)
  "Go to the Org mode file which contains the item at point.
When optional argument DELETE-OTHER-WINDOWS is non-nil, the
displayed Org file fills the frame."
  (interactive)
  (if (and org-return-follows-link
	   (not (org-get-at-bol 'org-marker))
	   (org-in-regexp org-bracket-link-regexp))
      (org-open-link-from-string (match-string 0))
    (let* ((marker (or (org-get-at-bol 'org-marker)
		       (org-agenda-error)))
	   (buffer (marker-buffer marker))
	   (pos (marker-position marker)))
      (unless buffer (user-error "Trying to switch to non-existent buffer"))
      (pop-to-buffer-same-window buffer)
      (when delete-other-windows (delete-other-windows))
      (widen)
      (goto-char pos)
      (when (derived-mode-p 'org-mode)
	(org-show-context 'agenda)
	(run-hooks 'org-agenda-after-show-hook)))))

(defun org-agenda-goto-mouse (ev)
  "Go to the Org file which contains the item at the mouse click."
  (interactive "e")
  (mouse-set-point ev)
  (org-agenda-goto))

(defun org-agenda-show (&optional full-entry)
  "Display the Org file which contains the item at point.
With prefix argument FULL-ENTRY, make the entire entry visible
if it was hidden in the outline."
  (interactive "P")
  (let ((win (selected-window)))
    (org-agenda-goto t)
    (when full-entry (org-show-entry))
    (select-window win)))

(defvar org-agenda-show-window nil)
(defun org-agenda-show-and-scroll-up (&optional arg)
  "Display the Org file which contains the item at point.

When called repeatedly, scroll the window that is displaying the buffer.

With a `\\[universal-argument]' prefix, use `org-show-entry' instead of \
`outline-show-subtree'
to display the item, so that drawers and logbooks stay folded."
  (interactive "P")
  (let ((win (selected-window)))
    (if (and (window-live-p org-agenda-show-window)
	     (eq this-command last-command))
	(progn
	  (select-window org-agenda-show-window)
	  (ignore-errors (scroll-up)))
      (org-agenda-goto t)
      (if arg (org-show-entry) (outline-show-subtree))
      (setq org-agenda-show-window (selected-window)))
    (select-window win)))

(defun org-agenda-show-scroll-down ()
  "Scroll down the window showing the agenda."
  (interactive)
  (let ((win (selected-window)))
    (when (window-live-p org-agenda-show-window)
      (select-window org-agenda-show-window)
      (ignore-errors (scroll-down))
      (select-window win))))

(defun org-agenda-show-1 (&optional more)
  "Display the Org file which contains the item at point.
The prefix arg selects the amount of information to display:

0   hide the subtree
1   just show the entry according to defaults.
2   show the children view
3   show the subtree view
4   show the entire subtree and any LOGBOOK drawers
5   show the entire subtree and any drawers
With prefix argument FULL-ENTRY, make the entire entry visible
if it was hidden in the outline."
  (interactive "p")
  (let ((win (selected-window)))
    (org-agenda-goto t)
    (org-back-to-heading)
    (set-window-start (selected-window) (point-at-bol))
    (cond
     ((= more 0)
      (outline-hide-subtree)
      (save-excursion
	(org-back-to-heading)
	(run-hook-with-args 'org-cycle-hook 'folded))
      (message "Remote: FOLDED"))
     ((and (called-interactively-p 'any) (= more 1))
      (message "Remote: show with default settings"))
     ((= more 2)
      (outline-show-entry)
      (org-show-children)
      (save-excursion
	(org-back-to-heading)
	(run-hook-with-args 'org-cycle-hook 'children))
      (message "Remote: CHILDREN"))
     ((= more 3)
      (outline-show-subtree)
      (save-excursion
	(org-back-to-heading)
	(run-hook-with-args 'org-cycle-hook 'subtree))
      (message "Remote: SUBTREE"))
     ((= more 4)
      (outline-show-subtree)
      (save-excursion
	(org-back-to-heading)
	(org-cycle-hide-drawers 'subtree '("LOGBOOK")))
      (message "Remote: SUBTREE AND LOGBOOK"))
     ((> more 4)
      (outline-show-subtree)
      (message "Remote: SUBTREE AND ALL DRAWERS")))
    (select-window win)))

(defvar org-agenda-cycle-counter nil)
(defun org-agenda-cycle-show (&optional n)
  "Show the current entry in another window, with default settings.

Default settings are taken from `org-show-context-detail'.  When
use repeatedly in immediate succession, the remote entry will
cycle through visibility

  children -> subtree -> folded

When called with a numeric prefix arg, that arg will be passed through to
`org-agenda-show-1'.  For the interpretation of that argument, see the
docstring of `org-agenda-show-1'."
  (interactive "P")
  (if (integerp n)
      (setq org-agenda-cycle-counter n)
    (if (not (eq last-command this-command))
	(setq org-agenda-cycle-counter 1)
      (if (equal org-agenda-cycle-counter 0)
	  (setq org-agenda-cycle-counter 2)
	(setq org-agenda-cycle-counter (1+ org-agenda-cycle-counter))
	(if (> org-agenda-cycle-counter 3)
	    (setq org-agenda-cycle-counter 0)))))
  (org-agenda-show-1 org-agenda-cycle-counter))

(defun org-agenda-recenter (arg)
  "Display the Org file which contains the item at point and recenter."
  (interactive "P")
  (let ((win (selected-window)))
    (org-agenda-goto t)
    (recenter arg)
    (select-window win)))

(defun org-agenda-show-mouse (ev)
  "Display the Org file which contains the item at the mouse click."
  (interactive "e")
  (mouse-set-point ev)
  (org-agenda-show))

(defun org-agenda-check-no-diary ()
  "Check if the entry is a diary link and abort if yes."
  (if (org-get-at-bol 'org-agenda-diary-link)
      (org-agenda-error)))

(defun org-agenda-error ()
  "Throw an error when a command is not allowed in the agenda."
  (user-error "Command not allowed in this line"))

(defun org-agenda-tree-to-indirect-buffer (arg)
  "Show the subtree corresponding to the current entry in an indirect buffer.
This calls the command `org-tree-to-indirect-buffer' from the original buffer.

With a numerical prefix ARG, go up to this level and then take that tree.
With a negative numeric ARG, go up by this number of levels.

With a `\\[universal-argument]' prefix, make a separate frame for this tree, \
i.e. don't use
the dedicated frame."
  (interactive "P")
  (if current-prefix-arg
      (org-agenda-do-tree-to-indirect-buffer arg)
    (let ((agenda-buffer (buffer-name))
	  (agenda-window (selected-window))
          (indirect-window
	   (and org-last-indirect-buffer
		(get-buffer-window org-last-indirect-buffer))))
      (save-window-excursion (org-agenda-do-tree-to-indirect-buffer arg))
      (unless (or (eq org-indirect-buffer-display 'new-frame)
		  (eq org-indirect-buffer-display 'dedicated-frame))
	(unwind-protect
	    (unless (and indirect-window (window-live-p indirect-window))
	      (setq indirect-window (split-window agenda-window)))
	  (and indirect-window (select-window indirect-window))
	  (switch-to-buffer org-last-indirect-buffer :norecord)
	  (fit-window-to-buffer indirect-window)))
      (select-window (get-buffer-window agenda-buffer))
      (setq org-agenda-last-indirect-buffer org-last-indirect-buffer))))

(defun org-agenda-do-tree-to-indirect-buffer (arg)
  "Same as `org-agenda-tree-to-indirect-buffer' without saving window."
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (with-current-buffer buffer
      (save-excursion
	(goto-char pos)
	(funcall 'org-tree-to-indirect-buffer arg)))))

(defvar org-last-heading-marker (make-marker)
  "Marker pointing to the headline that last changed its TODO state
by a remote command from the agenda.")

(defun org-agenda-todo-nextset ()
  "Switch TODO entry to next sequence."
  (interactive)
  (org-agenda-todo 'nextset))

(defun org-agenda-todo-previousset ()
  "Switch TODO entry to previous sequence."
  (interactive)
  (org-agenda-todo 'previousset))

(defun org-agenda-todo (&optional arg)
  "Cycle TODO state of line at point, also in Org file.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org file."
  (interactive "P")
  (org-agenda-check-no-diary)
  (let* ((col (current-column))
	 (marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 (hdmarker (org-get-at-bol 'org-hd-marker))
	 (todayp (org-agenda-today-p (org-get-at-bol 'day)))
	 (inhibit-read-only t)
	 org-agenda-headline-snapshot-before-repeat newhead just-one)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(org-show-context 'agenda)
	(let ((current-prefix-arg arg))
	  (call-interactively 'org-todo))
	(and (bolp) (forward-char 1))
	(setq newhead (org-get-heading))
	(when (and (bound-and-true-p
		    org-agenda-headline-snapshot-before-repeat)
		   (not (equal org-agenda-headline-snapshot-before-repeat
			       newhead))
		   todayp)
	  (setq newhead org-agenda-headline-snapshot-before-repeat
		just-one t))
	(save-excursion
	  (org-back-to-heading)
	  (move-marker org-last-heading-marker (point))))
      (beginning-of-line 1)
      (save-window-excursion
	(org-agenda-change-all-lines newhead hdmarker 'fixface just-one))
      (when (bound-and-true-p org-clock-out-when-done)
      	(string-match (concat "^" (regexp-opt org-done-keywords-for-agenda))
		      newhead)
	(org-agenda-unmark-clocking-task))
      (org-move-to-column col)
      (org-agenda-mark-clocking-task))))

(defun org-agenda-add-note (&optional arg)
  "Add a time-stamped note to the entry at point."
  (interactive "P")
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 (hdmarker (org-get-at-bol 'org-hd-marker))
	 (inhibit-read-only t))
    (with-current-buffer buffer
      (widen)
      (goto-char pos)
      (org-show-context 'agenda)
      (org-add-note))))

(defun org-agenda-change-all-lines (newhead hdmarker
					    &optional fixface just-this)
  "Change all lines in the agenda buffer which match HDMARKER.
The new content of the line will be NEWHEAD (as modified by
`org-agenda-format-item').  HDMARKER is checked with
`equal' against all `org-hd-marker' text properties in the file.
If FIXFACE is non-nil, the face of each item is modified according to
the new TODO state.
If JUST-THIS is non-nil, change just the current line, not all.
If FORCE-TAGS is non nil, the car of it returns the new tags."
  (let* ((inhibit-read-only t)
	 (line (org-current-line))
	 (org-agenda-buffer (current-buffer))
	 (thetags (with-current-buffer (marker-buffer hdmarker)
		    (org-with-wide-buffer
		     (goto-char hdmarker)
		     (org-get-tags-at))))
	 props m pl undone-face done-face finish new dotime level cat tags)
    (save-excursion
      (goto-char (point-max))
      (beginning-of-line 1)
      (while (not finish)
	(setq finish (bobp))
	(when (and (setq m (org-get-at-bol 'org-hd-marker))
		   (or (not just-this) (= (org-current-line) line))
		   (equal m hdmarker))
	  (setq props (text-properties-at (point))
		dotime (org-get-at-bol 'dotime)
		cat (org-get-at-eol 'org-category 1)
		level (org-get-at-bol 'level)
		tags thetags
		new
		(let ((org-prefix-format-compiled
		       (or (get-text-property (min (1- (point-max)) (point)) 'format)
			   org-prefix-format-compiled))
		      (extra (org-get-at-bol 'extra)))
		  (with-current-buffer (marker-buffer hdmarker)
		    (org-with-wide-buffer
		     (org-agenda-format-item extra newhead level cat tags dotime))))
		pl (text-property-any (point-at-bol) (point-at-eol) 'org-heading t)
		undone-face (org-get-at-bol 'undone-face)
		done-face (org-get-at-bol 'done-face))
	  (beginning-of-line 1)
	  (cond
	   ((equal new "") (delete-region (point) (line-beginning-position 2)))
	   ((looking-at ".*")
	    ;; When replacing the whole line, preserve bulk mark
	    ;; overlay, if any.
	    (let ((mark (catch :overlay
			  (dolist (o (overlays-in (point) (+ 2 (point))))
			    (when (eq (overlay-get o 'type)
				      'org-marked-entry-overlay)
			      (throw :overlay o))))))
	      (replace-match new t t)
	      (beginning-of-line)
	      (when mark (move-overlay mark (point) (+ 2 (point)))))
	    (add-text-properties (point-at-bol) (point-at-eol) props)
	    (when fixface
	      (add-text-properties
	       (point-at-bol) (point-at-eol)
	       (list 'face
		     (if org-last-todo-state-is-todo
			 undone-face done-face))))
	    (org-agenda-highlight-todo 'line)
	    (beginning-of-line 1))
	   (t (error "Line update did not work")))
	  (save-restriction
	    (narrow-to-region (point-at-bol) (point-at-eol))
	    (org-agenda-finalize)))
	(beginning-of-line 0)))))

(defun org-agenda-align-tags (&optional line)
  "Align all tags in agenda items to `org-agenda-tags-column'."
  (let ((inhibit-read-only t)
	(org-agenda-tags-column (if (eq 'auto org-agenda-tags-column)
				    (- (window-text-width))
				  org-agenda-tags-column))
	l c)
    (save-excursion
      (goto-char (if line (point-at-bol) (point-min)))
      (while (re-search-forward "\\([ \t]+\\)\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$"
				(if line (point-at-eol) nil) t)
	(add-text-properties
	 (match-beginning 2) (match-end 2)
	 (list 'face (delq nil (let ((prop (get-text-property
					    (match-beginning 2) 'face)))
				 (or (listp prop) (setq prop (list prop)))
				 (if (memq 'org-tag prop)
				     prop
				   (cons 'org-tag prop))))))
	(setq l (- (match-end 2) (match-beginning 2))
	      c (if (< org-agenda-tags-column 0)
		    (- (abs org-agenda-tags-column) l)
		  org-agenda-tags-column))
	(delete-region (match-beginning 1) (match-end 1))
	(goto-char (match-beginning 1))
	(insert (org-add-props
		    (make-string (max 1 (- c (current-column))) ?\ )
		    (plist-put (copy-sequence (text-properties-at (point)))
			       'face nil))))
      (goto-char (point-min))
      (org-font-lock-add-tag-faces (point-max)))))

(defun org-agenda-priority-up ()
  "Increase the priority of line at point, also in Org file."
  (interactive)
  (org-agenda-priority 'up))

(defun org-agenda-priority-down ()
  "Decrease the priority of line at point, also in Org file."
  (interactive)
  (org-agenda-priority 'down))

(defun org-agenda-priority (&optional force-direction)
  "Set the priority of line at point, also in Org file.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org file.
Called with a universal prefix arg, show the priority instead of setting it."
  (interactive "P")
  (if (equal force-direction '(4))
      (org-show-priority)
    (unless org-enable-priority-commands
      (error "Priority commands are disabled"))
    (org-agenda-check-no-diary)
    (let* ((col (current-column))
	   (marker (or (org-get-at-bol 'org-marker)
		       (org-agenda-error)))
	   (hdmarker (org-get-at-bol 'org-hd-marker))
	   (buffer (marker-buffer hdmarker))
	   (pos (marker-position hdmarker))
	   (inhibit-read-only t)
	   newhead)
      (org-with-remote-undo buffer
	(with-current-buffer buffer
	  (widen)
	  (goto-char pos)
	  (org-show-context 'agenda)
	  (funcall 'org-priority force-direction)
	  (end-of-line 1)
	  (setq newhead (org-get-heading)))
	(org-agenda-change-all-lines newhead hdmarker)
	(org-move-to-column col)))))

;; FIXME: should fix the tags property of the agenda line.
(defun org-agenda-set-tags (&optional tag onoff)
  "Set tags for the current headline."
  (interactive)
  (org-agenda-check-no-diary)
  (if (and (org-region-active-p) (called-interactively-p 'any))
      (call-interactively 'org-change-tag-in-region)
    (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
			 (org-agenda-error)))
	   (buffer (marker-buffer hdmarker))
	   (pos (marker-position hdmarker))
	   (inhibit-read-only t)
	   newhead)
      (org-with-remote-undo buffer
	(with-current-buffer buffer
	  (widen)
	  (goto-char pos)
	  (org-show-context 'agenda)
	  (if tag
	      (org-toggle-tag tag onoff)
	    (call-interactively 'org-set-tags))
	  (end-of-line 1)
	  (setq newhead (org-get-heading)))
	(org-agenda-change-all-lines newhead hdmarker)
	(beginning-of-line 1)))))

(defun org-agenda-set-property ()
  "Set a property for the current headline."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
		       (org-agenda-error)))
	 (buffer (marker-buffer hdmarker))
	 (pos (marker-position hdmarker))
	 (inhibit-read-only t)
	 newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(org-show-context 'agenda)
	(call-interactively 'org-set-property)))))

(defun org-agenda-set-effort ()
  "Set the effort property for the current headline."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
		       (org-agenda-error)))
	 (buffer (marker-buffer hdmarker))
	 (pos (marker-position hdmarker))
	 (inhibit-read-only t)
	 newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(org-show-context 'agenda)
	(call-interactively 'org-set-effort)
	(end-of-line 1)
	(setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

(defun org-agenda-toggle-archive-tag ()
  "Toggle the archive tag for the current entry."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
	 (buffer (marker-buffer hdmarker))
	 (pos (marker-position hdmarker))
	 (inhibit-read-only t)
	 newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(org-show-context 'agenda)
	(call-interactively 'org-toggle-archive-tag)
	(end-of-line 1)
	(setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker)
      (beginning-of-line 1))))

(defun org-agenda-do-date-later (arg)
  (interactive "P")
  (cond
   ((or (equal arg '(16))
	(memq last-command
	      '(org-agenda-date-later-minutes org-agenda-date-earlier-minutes)))
    (setq this-command 'org-agenda-date-later-minutes)
    (org-agenda-date-later-minutes 1))
   ((or (equal arg '(4))
	(memq last-command
	      '(org-agenda-date-later-hours org-agenda-date-earlier-hours)))
    (setq this-command 'org-agenda-date-later-hours)
    (org-agenda-date-later-hours 1))
   (t
    (org-agenda-date-later (prefix-numeric-value arg)))))

(defun org-agenda-do-date-earlier (arg)
  (interactive "P")
  (cond
   ((or (equal arg '(16))
	(memq last-command
	      '(org-agenda-date-later-minutes org-agenda-date-earlier-minutes)))
    (setq this-command 'org-agenda-date-earlier-minutes)
    (org-agenda-date-earlier-minutes 1))
   ((or (equal arg '(4))
	(memq last-command
	      '(org-agenda-date-later-hours org-agenda-date-earlier-hours)))
    (setq this-command 'org-agenda-date-earlier-hours)
    (org-agenda-date-earlier-hours 1))
   (t
    (org-agenda-date-earlier (prefix-numeric-value arg)))))

(defun org-agenda-date-later (arg &optional what)
  "Change the date of this item to ARG day(s) later."
  (interactive "p")
  (org-agenda-check-type t 'agenda)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 cdate today)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(unless (org-at-timestamp-p 'lax) (error "Cannot find time stamp"))
	(when (and org-agenda-move-date-from-past-immediately-to-today
		   (equal arg 1)
		   (or (not what) (eq what 'day))
		   (not (save-match-data (org-at-date-range-p))))
	  (setq cdate (org-parse-time-string (match-string 0) 'nodefault)
		cdate (calendar-absolute-from-gregorian
		       (list (nth 4 cdate) (nth 3 cdate) (nth 5 cdate)))
		today (org-today))
	  (if (> today cdate)
	      ;; immediately shift to today
	      (setq arg (- today cdate))))
	(org-timestamp-change arg (or what 'day))
	(when (and (org-at-date-range-p)
		   (re-search-backward org-tr-regexp-both (point-at-bol)))
	  (let ((end org-last-changed-timestamp))
	    (org-timestamp-change arg (or what 'day))
	    (setq org-last-changed-timestamp
		  (concat org-last-changed-timestamp "--" end)))))
      (org-agenda-show-new-time marker org-last-changed-timestamp))
    (message "Time stamp changed to %s" org-last-changed-timestamp)))

(defun org-agenda-date-earlier (arg &optional what)
  "Change the date of this item to ARG day(s) earlier."
  (interactive "p")
  (org-agenda-date-later (- arg) what))

(defun org-agenda-date-later-minutes (arg)
  "Change the time of this item, in units of `org-time-stamp-rounding-minutes'."
  (interactive "p")
  (setq arg (* arg (cadr org-time-stamp-rounding-minutes)))
  (org-agenda-date-later arg 'minute))

(defun org-agenda-date-earlier-minutes (arg)
  "Change the time of this item, in units of `org-time-stamp-rounding-minutes'."
  (interactive "p")
  (setq arg (* arg (cadr org-time-stamp-rounding-minutes)))
  (org-agenda-date-earlier arg 'minute))

(defun org-agenda-date-later-hours (arg)
  "Change the time of this item, in hour steps."
  (interactive "p")
  (org-agenda-date-later arg 'hour))

(defun org-agenda-date-earlier-hours (arg)
  "Change the time of this item, in hour steps."
  (interactive "p")
  (org-agenda-date-earlier arg 'hour))

(defun org-agenda-show-new-time (marker stamp &optional prefix)
  "Show new date stamp via text properties."
  ;; We use text properties to make this undoable
  (let ((inhibit-read-only t))
    (setq stamp (concat prefix " => " stamp " "))
    (save-excursion
      (goto-char (point-max))
      (while (not (bobp))
	(when (equal marker (org-get-at-bol 'org-marker))
	  (remove-text-properties (point-at-bol) (point-at-eol) '(display))
	  (org-move-to-column (- (window-width) (length stamp)) t)
          (add-text-properties
	   (1- (point)) (point-at-eol)
	   (list 'display (org-add-props stamp nil
			    'face '(secondary-selection default))))
	  (beginning-of-line 1))
	(beginning-of-line 0)))))

(defun org-agenda-date-prompt (arg)
  "Change the date of this item.  Date is prompted for, with default today.
The prefix ARG is passed to the `org-time-stamp' command and can therefore
be used to request time specification in the time stamp."
  (interactive "P")
  (org-agenda-check-type t 'agenda)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(unless (org-at-timestamp-p 'lax) (error "Cannot find time stamp"))
	(org-time-stamp arg (equal (char-after (match-beginning 0)) ?\[)))
      (org-agenda-show-new-time marker org-last-changed-timestamp))
    (message "Time stamp changed to %s" org-last-changed-timestamp)))

(defun org-agenda-schedule (arg &optional time)
  "Schedule the item at point.
ARG is passed through to `org-schedule'."
  (interactive "P")
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (type (marker-insertion-type marker))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 ts)
    (set-marker-insertion-type marker t)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(setq ts (org-schedule arg time)))
      (org-agenda-show-new-time marker ts " S"))
    (message "%s" ts)))

(defun org-agenda-deadline (arg &optional time)
  "Schedule the item at point.
ARG is passed through to `org-deadline'."
  (interactive "P")
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 ts)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(setq ts (org-deadline arg time)))
      (org-agenda-show-new-time marker ts " D"))
    (message "%s" ts)))

(defun org-agenda-clock-in (&optional arg)
  "Start the clock on the currently selected item."
  (interactive "P")
  (org-agenda-check-no-diary)
  (if (equal arg '(4))
      (org-clock-in arg)
    (let* ((marker (or (org-get-at-bol 'org-marker)
		       (org-agenda-error)))
	   (hdmarker (or (org-get-at-bol 'org-hd-marker) marker))
	   (pos (marker-position marker))
	   (col (current-column))
	   newhead)
      (org-with-remote-undo (marker-buffer marker)
        (with-current-buffer (marker-buffer marker)
	  (widen)
	  (goto-char pos)
	  (org-show-context 'agenda)
	  (org-cycle-hide-drawers 'children)
	  (org-clock-in arg)
	  (setq newhead (org-get-heading)))
	(org-agenda-change-all-lines newhead hdmarker))
      (org-move-to-column col))))

(defun org-agenda-clock-out ()
  "Stop the currently running clock."
  (interactive)
  (unless (marker-buffer org-clock-marker)
    (error "No running clock"))
  (let ((marker (make-marker)) (col (current-column)) newhead)
    (org-with-remote-undo (marker-buffer org-clock-marker)
      (with-current-buffer (marker-buffer org-clock-marker)
	(org-with-wide-buffer
	 (goto-char org-clock-marker)
	 (org-back-to-heading t)
	 (move-marker marker (point))
	 (org-clock-out)
	 (setq newhead (org-get-heading)))))
    (org-agenda-change-all-lines newhead marker)
    (move-marker marker nil)
    (org-move-to-column col)
    (org-agenda-unmark-clocking-task)))

(defun org-agenda-clock-cancel (&optional arg)
  "Cancel the currently running clock."
  (interactive "P")
  (unless (marker-buffer org-clock-marker)
    (user-error "No running clock"))
  (org-with-remote-undo (marker-buffer org-clock-marker)
    (org-clock-cancel)))

(defun org-agenda-clock-goto ()
  "Jump to the currently clocked in task within the agenda.
If the currently clocked in task is not listed in the agenda
buffer, display it in another window."
  (interactive)
  (let (pos)
    (mapc (lambda (o)
	    (if (eq (overlay-get o 'type) 'org-agenda-clocking)
		(setq pos (overlay-start o))))
	  (overlays-in (point-min) (point-max)))
    (cond (pos (goto-char pos))
	  ;; If the currently clocked entry is not in the agenda
	  ;; buffer, we visit it in another window:
	  ((bound-and-true-p org-clock-current-task)
	   (org-switch-to-buffer-other-window (org-clock-goto)))
	  (t (message "No running clock, use `C-c C-x C-j' to jump to the most recent one")))))

(defun org-agenda-diary-entry-in-org-file ()
  "Make a diary entry in the file `org-agenda-diary-file'."
  (let (d1 d2 char (text "") dp1 dp2)
    (if (equal (buffer-name) "*Calendar*")
	(setq d1 (calendar-cursor-to-date t)
	      d2 (car calendar-mark-ring))
      (setq dp1 (get-text-property (point-at-bol) 'day))
      (unless dp1 (user-error "No date defined in current line"))
      (setq d1 (calendar-gregorian-from-absolute dp1)
	    d2 (and (ignore-errors (mark))
		    (save-excursion
		      (goto-char (mark))
		      (setq dp2 (get-text-property (point-at-bol) 'day)))
		    (calendar-gregorian-from-absolute dp2))))
    (message "Diary entry: [d]ay [a]nniversary [b]lock [j]ump to date tree")
    (setq char (read-char-exclusive))
    (cond
     ((equal char ?d)
      (setq text (read-string "Day entry: "))
      (org-agenda-add-entry-to-org-agenda-diary-file 'day text d1)
      (and (equal (buffer-name) org-agenda-buffer-name) (org-agenda-redo)))
     ((equal char ?a)
      (setq d1 (list (car d1) (nth 1 d1)
		     (read-number (format "Reference year [%d]: " (nth 2 d1))
				  (nth 2 d1))))
      (setq text (read-string "Anniversary (use %d to show years): "))
      (org-agenda-add-entry-to-org-agenda-diary-file 'anniversary text d1)
      (and (equal (buffer-name) org-agenda-buffer-name) (org-agenda-redo)))
     ((equal char ?b)
      (setq text (read-string "Block entry: "))
      (unless (and d1 d2 (not (equal d1 d2)))
	(user-error "No block of days selected"))
      (org-agenda-add-entry-to-org-agenda-diary-file 'block text d1 d2)
      (and (equal (buffer-name) org-agenda-buffer-name) (org-agenda-redo)))
     ((equal char ?j)
      (org-switch-to-buffer-other-window
       (find-file-noselect org-agenda-diary-file))
      (require 'org-datetree)
      (org-datetree-find-date-create d1)
      (org-reveal t))
     (t (user-error "Invalid selection character `%c'" char)))))

(defcustom org-agenda-insert-diary-strategy 'date-tree
  "Where in `org-agenda-diary-file' should new entries be added?
Valid values:

date-tree         in the date tree, as first child of the date
date-tree-last    in the date tree, as last child of the date
top-level         as top-level entries at the end of the file."
  :group 'org-agenda
  :type '(choice
	  (const :tag "first in a date tree" date-tree)
	  (const :tag "last in a date tree" date-tree-last)
	  (const :tag "as top level at end of file" top-level)))

(defcustom org-agenda-insert-diary-extract-time nil
  "Non-nil means extract any time specification from the diary entry."
  :group 'org-agenda
  :version "24.1"
  :type 'boolean)

(defcustom org-agenda-bulk-mark-char ">"
  "A single-character string to be used as the bulk mark."
  :group 'org-agenda
  :version "24.1"
  :type 'string)

(defun org-agenda-add-entry-to-org-agenda-diary-file (type text &optional d1 d2)
  "Add a diary entry with TYPE to `org-agenda-diary-file'.
If TEXT is not empty, it will become the headline of the new entry, and
the resulting entry will not be shown.  When TEXT is empty, switch to
`org-agenda-diary-file' and let the user finish the entry there."
  (let ((cw (current-window-configuration)))
    (org-switch-to-buffer-other-window
     (find-file-noselect org-agenda-diary-file))
    (widen)
    (goto-char (point-min))
    (cond
     ((eq type 'anniversary)
      (or (re-search-forward "^*[ \t]+Anniversaries" nil t)
	  (progn
	    (or (org-at-heading-p t)
		(progn
		  (outline-next-heading)
		  (insert "* Anniversaries\n\n")
		  (beginning-of-line -1)))))
      (outline-next-heading)
      (org-back-over-empty-lines)
      (backward-char 1)
      (insert "\n")
      (insert (format "%%%%(org-anniversary %d %2d %2d) %s"
		      (nth 2 d1) (car d1) (nth 1 d1) text)))
     ((eq type 'day)
      (let ((org-prefix-has-time t)
	    (org-agenda-time-leading-zero t)
	    fmt time time2)
	(if org-agenda-insert-diary-extract-time
	    ;; Use org-agenda-format-item to parse text for a time-range and
	    ;; remove it.  FIXME: This is a hack, we should refactor
	    ;; that function to make time extraction available separately
	    (setq fmt (org-agenda-format-item nil text nil nil nil t)
		  time (get-text-property 0 'time fmt)
		  time2 (if (> (length time) 0)
			    ;; split-string removes trailing ...... if
			    ;; no end time given.  First space
			    ;; separates time from date.
			    (concat " " (car (split-string time "\\.")))
			  nil)
		  text (get-text-property 0 'txt fmt)))
	(if (eq org-agenda-insert-diary-strategy 'top-level)
	    (org-agenda-insert-diary-as-top-level text)
	  (require 'org-datetree)
	  (org-datetree-find-date-create d1)
	  (org-agenda-insert-diary-make-new-entry text))
	(org-insert-time-stamp (org-time-from-absolute
				(calendar-absolute-from-gregorian d1))
			       nil nil nil nil time2))
      (end-of-line 0))
     ((eq type 'block)
      (if (> (calendar-absolute-from-gregorian d1)
	     (calendar-absolute-from-gregorian d2))
	  (setq d1 (prog1 d2 (setq d2 d1))))
      (if (eq org-agenda-insert-diary-strategy 'top-level)
	  (org-agenda-insert-diary-as-top-level text)
	(require 'org-datetree)
	(org-datetree-find-date-create d1)
	(org-agenda-insert-diary-make-new-entry text))
      (org-insert-time-stamp (org-time-from-absolute
			      (calendar-absolute-from-gregorian d1)))
      (insert "--")
      (org-insert-time-stamp (org-time-from-absolute
			      (calendar-absolute-from-gregorian d2)))
      (end-of-line 0)))
    (if (string-match "\\S-" text)
	(progn
	  (set-window-configuration cw)
	  (message "%s entry added to %s"
		   (capitalize (symbol-name type))
		   (abbreviate-file-name org-agenda-diary-file)))
      (org-reveal t)
      (message "Please finish entry here"))))

(defun org-agenda-insert-diary-as-top-level (text)
  "Make new entry as a top-level entry at the end of the file.
Add TEXT as headline, and position the cursor in the second line so that
a timestamp can be added there."
  (widen)
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (org-insert-heading nil t t)
  (insert text)
  (org-end-of-meta-data)
  (unless (bolp) (insert "\n"))
  (when org-adapt-indentation (indent-to-column 2)))

(defun org-agenda-insert-diary-make-new-entry (text)
  "Make a new entry with TEXT as a child of the current subtree.
Position the point in the heading's first body line so that
a timestamp can be added there."
  (cond
   ((eq org-agenda-insert-diary-strategy 'date-tree-last)
    (end-of-line)
    (org-insert-heading '(4) t)
    (org-do-demote))
   (t
    (outline-next-heading)
    (org-back-over-empty-lines)
    (unless (looking-at "[ \t]*$") (save-excursion (insert "\n")))
    (org-insert-heading nil t)
    (org-do-demote)))
  (let ((col (current-column)))
    (insert text)
    (org-end-of-meta-data)
    ;; Ensure point is left on a blank line, at proper indentation.
    (unless (bolp) (insert "\n"))
    (unless (looking-at-p "^[ \t]*$") (save-excursion (insert "\n")))
    (when org-adapt-indentation (indent-to-column col)))
  (org-show-set-visibility 'lineage))

(defun org-agenda-diary-entry ()
  "Make a diary entry, like the `i' command from the calendar.
All the standard commands work: block, weekly etc.
When `org-agenda-diary-file' points to a file,
`org-agenda-diary-entry-in-org-file' is called instead to create
entries in that Org file."
  (interactive)
  (if (not (eq org-agenda-diary-file 'diary-file))
      (org-agenda-diary-entry-in-org-file)
    (require 'diary-lib)
    (let* ((char (progn
		   (message "Diary entry: [d]ay [w]eekly [m]onthly [y]early [a]nniversary [b]lock [c]yclic")
		   (read-char-exclusive)))
	   (cmd (cdr (assoc char
			    '((?d . diary-insert-entry)
			      (?w . diary-insert-weekly-entry)
			      (?m . diary-insert-monthly-entry)
			      (?y . diary-insert-yearly-entry)
			      (?a . diary-insert-anniversary-entry)
			      (?b . diary-insert-block-entry)
			      (?c . diary-insert-cyclic-entry)))))
	   (oldf (symbol-function 'calendar-cursor-to-date))
	   ;; (buf (get-file-buffer (substitute-in-file-name diary-file)))
	   (point (point))
	   (mark (or (mark t) (point))))
      (unless cmd
	(user-error "No command associated with <%c>" char))
      (unless (and (get-text-property point 'day)
		   (or (not (equal ?b char))
		       (get-text-property mark 'day)))
	(user-error "Don't know which date to use for diary entry"))
      ;; We implement this by hacking the `calendar-cursor-to-date' function
      ;; and the `calendar-mark-ring' variable.  Saves a lot of code.
      (let ((calendar-mark-ring
	     (list (calendar-gregorian-from-absolute
		    (or (get-text-property mark 'day)
			(get-text-property point 'day))))))
	(unwind-protect
	    (progn
	      (fset 'calendar-cursor-to-date
		    (lambda (&optional error dummy)
		      (calendar-gregorian-from-absolute
		       (get-text-property point 'day))))
	      (call-interactively cmd))
	  (fset 'calendar-cursor-to-date oldf))))))

(defun org-agenda-execute-calendar-command (cmd)
  "Execute a calendar command from the agenda with date from cursor."
  (org-agenda-check-type t 'agenda)
  (require 'diary-lib)
  (unless (get-text-property (min (1- (point-max)) (point)) 'day)
    (user-error "Don't know which date to use for the calendar command"))
  (let* ((oldf (symbol-function 'calendar-cursor-to-date))
	 (point (point))
	 (date (calendar-gregorian-from-absolute
		(get-text-property point 'day)))
         ;; the following 2 vars are needed in the calendar
	 (displayed-month (car date))
	 (displayed-year (nth 2 date)))
    (unwind-protect
	(progn
	  (fset 'calendar-cursor-to-date
		(lambda (&optional error dummy)
		  (calendar-gregorian-from-absolute
		   (get-text-property point 'day))))
	  (call-interactively cmd))
      (fset 'calendar-cursor-to-date oldf))))

(defun org-agenda-phases-of-moon ()
  "Display the phases of the moon for the 3 months around the cursor date."
  (interactive)
  (org-agenda-execute-calendar-command 'calendar-lunar-phases))

(defun org-agenda-holidays ()
  "Display the holidays for the 3 months around the cursor date."
  (interactive)
  (org-agenda-execute-calendar-command 'calendar-list-holidays))

(defvar calendar-longitude)      ; defined in calendar.el
(defvar calendar-latitude)       ; defined in calendar.el
(defvar calendar-location-name)  ; defined in calendar.el

(defun org-agenda-sunrise-sunset (arg)
  "Display sunrise and sunset for the cursor date.
Latitude and longitude can be specified with the variables
`calendar-latitude' and `calendar-longitude'.  When called with prefix
argument, latitude and longitude will be prompted for."
  (interactive "P")
  (require 'solar)
  (let ((calendar-longitude (if arg nil calendar-longitude))
	(calendar-latitude  (if arg nil calendar-latitude))
	(calendar-location-name
	 (if arg "the given coordinates" calendar-location-name)))
    (org-agenda-execute-calendar-command 'calendar-sunrise-sunset)))

(defun org-agenda-goto-calendar ()
  "Open the Emacs calendar with the date at the cursor."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (let* ((day (or (get-text-property (min (1- (point-max)) (point)) 'day)
		  (user-error "Don't know which date to open in calendar")))
	 (date (calendar-gregorian-from-absolute day))
	 (calendar-move-hook nil)
	 (calendar-view-holidays-initially-flag nil)
	 (calendar-view-diary-initially-flag nil))
    (calendar)
    (calendar-goto-date date)))

;;;###autoload
(defun org-calendar-goto-agenda ()
  "Compute the Org agenda for the calendar date displayed at the cursor.
This is a command that has to be installed in `calendar-mode-map'."
  (interactive)
  ;; Temporarily disable sticky agenda since user clearly wants to
  ;; refresh view anyway.
  (let ((org-agenda-buffer-tmp-name "*Org Agenda(a)*")
	(org-agenda-sticky nil))
    (org-agenda-list nil (calendar-absolute-from-gregorian
			  (calendar-cursor-to-date))
		     nil)))

(defun org-agenda-convert-date ()
  (interactive)
  (org-agenda-check-type t 'agenda)
  (let ((day (get-text-property (min (1- (point-max)) (point)) 'day))
	date s)
    (unless day
      (user-error "Don't know which date to convert"))
    (setq date (calendar-gregorian-from-absolute day))
    (setq s (concat
	     "Gregorian:  " (calendar-date-string date) "\n"
	     "ISO:        " (calendar-iso-date-string date) "\n"
	     "Day of Yr:  " (calendar-day-of-year-string date) "\n"
	     "Julian:     " (calendar-julian-date-string date) "\n"
	     "Astron. JD: " (calendar-astro-date-string date)
	     " (Julian date number at noon UTC)\n"
	     "Hebrew:     " (calendar-hebrew-date-string date) " (until sunset)\n"
	     "Islamic:    " (calendar-islamic-date-string date) " (until sunset)\n"
	     "French:     " (calendar-french-date-string date) "\n"
	     "Bahá’í:     " (calendar-bahai-date-string date) " (until sunset)\n"
	     "Mayan:      " (calendar-mayan-date-string date) "\n"
	     "Coptic:     " (calendar-coptic-date-string date) "\n"
	     "Ethiopic:   " (calendar-ethiopic-date-string date) "\n"
	     "Persian:    " (calendar-persian-date-string date) "\n"
	     "Chinese:    " (calendar-chinese-date-string date) "\n"))
    (with-output-to-temp-buffer "*Dates*"
      (princ s))
    (org-fit-window-to-buffer (get-buffer-window "*Dates*"))))

;;; Bulk commands

(defun org-agenda-bulk-marked-p ()
  "Non-nil when current entry is marked for bulk action."
  (eq (get-char-property (point-at-bol) 'type)
      'org-marked-entry-overlay))

(defun org-agenda-bulk-mark (&optional arg)
  "Mark the entry at point for future bulk action."
  (interactive "p")
  (dotimes (i (or arg 1))
    (unless (org-get-at-bol 'org-agenda-diary-link)
      (let* ((m (org-get-at-bol 'org-hd-marker))
	     ov)
	(unless (org-agenda-bulk-marked-p)
	  (unless m (user-error "Nothing to mark at point"))
	  (push m org-agenda-bulk-marked-entries)
	  (setq ov (make-overlay (point-at-bol) (+ 2 (point-at-bol))))
	  (org-overlay-display ov (concat org-agenda-bulk-mark-char " ")
			       (org-get-todo-face "TODO")
			       'evaporate)
	  (overlay-put ov 'type 'org-marked-entry-overlay))
	(end-of-line 1)
	(or (ignore-errors
	      (goto-char (next-single-property-change (point) 'org-hd-marker)))
	    (beginning-of-line 2))
	(while (and (get-char-property (point) 'invisible) (not (eobp)))
	  (beginning-of-line 2))
	(message "%d entries marked for bulk action"
		 (length org-agenda-bulk-marked-entries))))))

(defun org-agenda-bulk-mark-all ()
  "Mark all entries for future agenda bulk action."
  (interactive)
  (org-agenda-bulk-mark-regexp "."))

(defun org-agenda-bulk-mark-regexp (regexp)
  "Mark entries matching REGEXP for future agenda bulk action."
  (interactive "sMark entries matching regexp: ")
  (let ((entries-marked 0) txt-at-point)
    (save-excursion
      (goto-char (point-min))
      (goto-char (next-single-property-change (point) 'org-hd-marker))
      (while (and (re-search-forward regexp nil t)
		  (setq txt-at-point (get-text-property (point) 'txt)))
	(if (get-char-property (point) 'invisible)
	    (beginning-of-line 2)
	  (when (string-match regexp txt-at-point)
	    (setq entries-marked (1+ entries-marked))
	    (call-interactively 'org-agenda-bulk-mark)))))

    (if (not entries-marked)
	(message "No entry matching this regexp."))))

(defun org-agenda-bulk-unmark (&optional arg)
  "Unmark the entry at point for future bulk action."
  (interactive "P")
  (if arg
      (org-agenda-bulk-unmark-all)
    (cond ((org-agenda-bulk-marked-p)
	   (org-agenda-bulk-remove-overlays
	    (point-at-bol) (+ 2 (point-at-bol)))
	   (setq org-agenda-bulk-marked-entries
		 (delete (org-get-at-bol 'org-hd-marker)
			 org-agenda-bulk-marked-entries))
	   (end-of-line 1)
	   (or (ignore-errors
		 (goto-char (next-single-property-change (point) 'txt)))
	       (beginning-of-line 2))
	   (while (and (get-char-property (point) 'invisible) (not (eobp)))
	     (beginning-of-line 2))
	   (message "%d entries left marked for bulk action"
		    (length org-agenda-bulk-marked-entries)))
	  (t (message "No entry to unmark here")))))

(defun org-agenda-bulk-toggle-all ()
  "Toggle all marks for bulk action."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (ignore-errors
	     (goto-char (next-single-property-change (point) 'org-hd-marker)))
      (org-agenda-bulk-toggle))))

(defun org-agenda-bulk-toggle ()
  "Toggle the mark at point for bulk action."
  (interactive)
  (if (org-agenda-bulk-marked-p)
      (org-agenda-bulk-unmark)
    (org-agenda-bulk-mark)))

(defun org-agenda-bulk-remove-overlays (&optional beg end)
  "Remove the mark overlays between BEG and END in the agenda buffer.
BEG and END default to the buffer limits.

This only removes the overlays, it does not remove the markers
from the list in `org-agenda-bulk-marked-entries'."
  (interactive)
  (mapc (lambda (ov)
	  (and (eq (overlay-get ov 'type) 'org-marked-entry-overlay)
	       (delete-overlay ov)))
	(overlays-in (or beg (point-min)) (or end (point-max)))))

(defun org-agenda-bulk-unmark-all ()
  "Remove all marks in the agenda buffer.
This will remove the markers and the overlays."
  (interactive)
  (if (null org-agenda-bulk-marked-entries)
      (message "No entry to unmark")
    (setq org-agenda-bulk-marked-entries nil)
    (org-agenda-bulk-remove-overlays (point-min) (point-max))))

(defcustom org-agenda-persistent-marks nil
  "Non-nil means marked items will stay marked after a bulk action.
You can toggle this interactively by typing `p' when prompted for a
bulk action."
  :group 'org-agenda
  :version "24.1"
  :type 'boolean)

(defun org-agenda-bulk-action (&optional arg)
  "Execute an remote-editing action on all marked entries.
The prefix arg is passed through to the command if possible."
  (interactive "P")
  ;; Make sure we have markers, and only valid ones.
  (unless org-agenda-bulk-marked-entries (user-error "No entries are marked"))
  (dolist (m org-agenda-bulk-marked-entries)
    (unless (and (markerp m)
		 (marker-buffer m)
		 (buffer-live-p (marker-buffer m))
		 (marker-position m))
      (user-error "Marker %s for bulk command is invalid" m)))

  ;; Prompt for the bulk command.
  (message
   (concat (if org-agenda-persistent-marks "Bulk (persistent): " "Bulk: ")
	   "[$]arch [A]rch->sib [t]odo [+/-]tag [s]chd [d]eadline [r]efile "
	   "[S]catter [f]unction    "
	   (and org-agenda-bulk-custom-functions
		(format " Custom: [%s]"
			(mapconcat (lambda (f) (char-to-string (car f)))
				   org-agenda-bulk-custom-functions
				   "")))))
  (catch 'exit
    (let* ((org-log-refile (if org-log-refile 'time nil))
	   (entries (reverse org-agenda-bulk-marked-entries))
	   (org-overriding-default-time
	    (and (get-text-property (point) 'org-agenda-date-header)
		 (org-get-cursor-date)))
	   redo-at-end
	   cmd)
      (pcase (read-char-exclusive)
	(?p
	 (let ((org-agenda-persistent-marks
		(not org-agenda-persistent-marks)))
	   (org-agenda-bulk-action)
	   (throw 'exit nil)))

	(?$
	 (setq cmd #'org-agenda-archive))

	(?A
	 (setq cmd #'org-agenda-archive-to-archive-sibling))

	((or ?r ?w)
	 (let ((refile-location
		(org-refile-get-location
		 "Refile to"
		 (marker-buffer (car entries))
		 org-refile-allow-creating-parent-nodes)))
	   (when (nth 3 refile-location)
	     (setcar (nthcdr 3 refile-location)
		     (move-marker
		      (make-marker)
		      (nth 3 refile-location)
		      (or (get-file-buffer (nth 1 refile-location))
			  (find-buffer-visiting (nth 1 refile-location))
			  (error "This should not happen")))))

	   (setq cmd `(lambda () (org-agenda-refile nil ',refile-location t)))
	   (setq redo-at-end t)))

	(?t
	 (let ((state (completing-read
		       "Todo state: "
		       (with-current-buffer (marker-buffer (car entries))
			 (mapcar #'list org-todo-keywords-1)))))
	   (setq cmd `(lambda ()
			(let ((org-inhibit-blocking t)
			      (org-inhibit-logging 'note))
			  (org-agenda-todo ,state))))))

	((and (or ?- ?+) action)
	 (let ((tag (completing-read
		     (format "Tag to %s: " (if (eq action ?+) "add" "remove"))
		     (with-current-buffer (marker-buffer (car entries))
		       (delq nil
			     (mapcar (lambda (x) (and (stringp (car x)) x))
				     org-current-tag-alist))))))
	   (setq cmd
		 `(lambda ()
		    (org-agenda-set-tags ,tag
					 ,(if (eq action ?+) ''on ''off))))))

	(?s
	 (let ((time
		(and (not arg)
		     (org-read-date nil nil nil "(Re)Schedule to"
				    org-overriding-default-time))))
	   ;; Make sure to not prompt for a note when bulk
	   ;; rescheduling as Org cannot cope with simultaneous notes.
	   ;; Besides, it could be annoying depending on the number of
	   ;; items re-scheduled.
	   (setq cmd
		 `(lambda ()
		    (let ((org-log-reschedule (and org-log-reschedule 'time)))
		      (org-agenda-schedule arg ,time))))))
	(?d
	 (let ((time
		(and (not arg)
		     (org-read-date nil nil nil "(Re)Set Deadline to"
				    org-overriding-default-time))))
	   ;; Make sure to not prompt for a note when bulk
	   ;; rescheduling as Org cannot cope with simultaneous
	   ;; notes.  Besides, it could be annoying depending on the
	   ;; number of items re-scheduled.
	   (setq cmd
		 `(lambda ()
		    (let ((org-log-redeadline (and org-log-redeadline 'time)))
		      (org-agenda-deadline arg ,time))))))

	(?S
	 (unless (org-agenda-check-type nil 'agenda 'todo)
	   (user-error "Can't scatter tasks in \"%s\" agenda view" org-agenda-type))
	 (let ((days (read-number
		      (format "Scatter tasks across how many %sdays: "
			      (if arg "week" ""))
		      7)))
	   (setq cmd
		 `(lambda ()
		    (let ((distance (1+ (random ,days))))
		      (when arg
			(let ((dist distance)
			      (day-of-week
			       (calendar-day-of-week
				(calendar-gregorian-from-absolute (org-today)))))
			  (dotimes (i (1+ dist))
			    (while (member day-of-week org-agenda-weekend-days)
			      (cl-incf distance)
			      (cl-incf day-of-week)
			      (when (= day-of-week 7)
				(setq day-of-week 0)))
			    (cl-incf day-of-week)
			    (when (= day-of-week 7)
			      (setq day-of-week 0)))))
		      ;; Silently fail when try to replan a sexp entry.
		      (ignore-errors
			(let* ((date (calendar-gregorian-from-absolute
				      (+ (org-today) distance)))
			       (time (encode-time 0 0 0 (nth 1 date) (nth 0 date)
						  (nth 2 date))))
			  (org-agenda-schedule nil time))))))))

	(?f
	 (setq cmd
	       (intern
		(completing-read "Function: " obarray #'fboundp t nil nil))))

	(action
	 (pcase (assoc action org-agenda-bulk-custom-functions)
	   (`(,_ ,f) (setq cmd f) (setq redo-at-end t))
	   (_ (user-error "Invalid bulk action: %c" action)))))

      ;; Sort the markers, to make sure that parents are handled
      ;; before children.
      (setq entries (sort entries
			  (lambda (a b)
			    (cond
			     ((eq (marker-buffer a) (marker-buffer b))
			      (< (marker-position a) (marker-position b)))
			     (t
			      (string< (buffer-name (marker-buffer a))
				       (buffer-name (marker-buffer b))))))))

      ;; Now loop over all markers and apply CMD.
      (let ((processed 0)
	    (skipped 0))
	(dolist (e entries)
	  (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
	    (if (not pos)
		(progn (message "Skipping removed entry at %s" e)
		       (cl-incf skipped))
	      (goto-char pos)
	      (let (org-loop-over-headlines-in-active-region) (funcall cmd))
	      ;; `post-command-hook' is not run yet.  We make sure any
	      ;; pending log note is processed.
	      (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
			(memq 'org-add-log-note post-command-hook))
		(org-add-log-note))
	      (cl-incf processed))))
	(when redo-at-end (org-agenda-redo))
	(unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
	(message "Acted on %d entries%s%s"
		 processed
		 (if (= skipped 0)
		     ""
		   (format ", skipped %d (disappeared before their turn)"
			   skipped))
		 (if (not org-agenda-persistent-marks) "" " (kept marked)"))))))

(defun org-agenda-capture (&optional with-time)
  "Call `org-capture' with the date at point.
With a `C-1' prefix, use the HH:MM value at point (if any) or the
current HH:MM time."
  (interactive "P")
  (if (not (eq major-mode 'org-agenda-mode))
      (user-error "You cannot do this outside of agenda buffers")
    (let ((org-overriding-default-time
	   (org-get-cursor-date (equal with-time 1))))
      (call-interactively 'org-capture))))

;;; Dragging agenda lines forward/backward

(defun org-agenda-reapply-filters ()
  "Re-apply all agenda filters."
  (mapcar
   (lambda(f) (when (car f) (org-agenda-filter-apply (car f) (cadr f) t)))
   `((,org-agenda-tag-filter tag)
     (,org-agenda-category-filter category)
     (,org-agenda-regexp-filter regexp)
     (,org-agenda-effort-filter effort)
     (,(get 'org-agenda-tag-filter :preset-filter) tag)
     (,(get 'org-agenda-category-filter :preset-filter) category)
     (,(get 'org-agenda-effort-filter :preset-filter) effort)
     (,(get 'org-agenda-regexp-filter :preset-filter) regexp))))

(defun org-agenda-drag-line-forward (arg &optional backward)
  "Drag an agenda line forward by ARG lines.
When the optional argument `backward' is non-nil, move backward."
  (interactive "p")
  (let ((inhibit-read-only t) lst line)
    (if (or (not (get-text-property (point) 'txt))
	    (save-excursion
	      (dotimes (n arg)
		(move-beginning-of-line (if backward 0 2))
		(push (not (get-text-property (point) 'txt)) lst))
	      (delq nil lst)))
	(message "Cannot move line forward")
      (let ((end (save-excursion (move-beginning-of-line 2) (point))))
	(move-beginning-of-line 1)
	(setq line (buffer-substring (point) end))
	(delete-region (point) end)
	(move-beginning-of-line (funcall (if backward '1- '1+) arg))
	(insert line)
	(org-agenda-reapply-filters)
	(org-agenda-mark-clocking-task)
	(move-beginning-of-line 0)))))

(defun org-agenda-drag-line-backward (arg)
  "Drag an agenda line backward by ARG lines."
  (interactive "p")
  (org-agenda-drag-line-forward arg t))

;;; Flagging notes

(defun org-agenda-show-the-flagging-note ()
  "Display the flagging note in the other window.
When called a second time in direct sequence, offer to remove the FLAGGING
tag and (if present) the flagging note."
  (interactive)
  (let ((hdmarker (org-get-at-bol 'org-hd-marker))
	(win (selected-window))
	note heading newhead)
    (unless hdmarker
      (user-error "No linked entry at point"))
    (if (and (eq this-command last-command)
	     (y-or-n-p "Unflag and remove any flagging note? "))
	(progn
	  (org-agenda-remove-flag hdmarker)
	  (let ((win (get-buffer-window "*Flagging Note*")))
	    (and win (delete-window win)))
	  (message "Entry unflagged"))
      (setq note (org-entry-get hdmarker "THEFLAGGINGNOTE"))
      (unless note
	(user-error "No flagging note"))
      (org-kill-new note)
      (org-switch-to-buffer-other-window "*Flagging Note*")
      (erase-buffer)
      (insert note)
      (goto-char (point-min))
      (while (re-search-forward "\\\\n" nil t)
	(replace-match "\n" t t))
      (goto-char (point-min))
      (select-window win)
      (message "%s" (substitute-command-keys "Flagging note pushed to \
kill ring.  Press `\\[org-agenda-show-the-flagging-note]' again to remove \
tag and note")))))

(defun org-agenda-remove-flag (marker)
  "Remove the FLAGGED tag and any flagging note in the entry."
  (let (newhead)
    (org-with-point-at marker
      (org-toggle-tag "FLAGGED" 'off)
      (org-entry-delete nil "THEFLAGGINGNOTE")
      (setq newhead (org-get-heading)))
    (org-agenda-change-all-lines newhead marker)
    (message "Entry unflagged")))

(defun org-agenda-get-any-marker (&optional pos)
  (or (get-text-property (or pos (point-at-bol)) 'org-hd-marker)
      (get-text-property (or pos (point-at-bol)) 'org-marker)))

;;; Appointment reminders

(defvar appt-time-msg-list) ; defined in appt.el

;;;###autoload
(defun org-agenda-to-appt (&optional refresh filter &rest args)
  "Activate appointments found in `org-agenda-files'.

With a `\\[universal-argument]' prefix, refresh the list of \
appointments.

If FILTER is t, interactively prompt the user for a regular
expression, and filter out entries that don't match it.

If FILTER is a string, use this string as a regular expression
for filtering entries out.

If FILTER is a function, filter out entries against which
calling the function returns nil.  This function takes one
argument: an entry from `org-agenda-get-day-entries'.

FILTER can also be an alist with the car of each cell being
either `headline' or `category'.  For example:

  \\='((headline \"IMPORTANT\")
    (category \"Work\"))

will only add headlines containing IMPORTANT or headlines
belonging to the \"Work\" category.

ARGS are symbols indicating what kind of entries to consider.
By default `org-agenda-to-appt' will use :deadline*, :scheduled*
\(i.e., deadlines and scheduled items with a hh:mm specification)
and :timestamp entries.  See the docstring of `org-diary' for
details and examples.

If an entry has a APPT_WARNTIME property, its value will be used
to override `appt-message-warning-time'."
  (interactive "P")
  (if refresh (setq appt-time-msg-list nil))
  (if (eq filter t)
      (setq filter (read-from-minibuffer "Regexp filter: ")))
  (let* ((cnt 0)                        ; count added events
         (scope (or args '(:deadline* :scheduled* :timestamp)))
         (org-agenda-new-buffers nil)
         (org-deadline-warning-days 0)
         ;; Do not use `org-today' here because appt only takes
         ;; time and without date as argument, so it may pass wrong
         ;; information otherwise
         (today (org-date-to-gregorian
                 (time-to-days (current-time))))
         (org-agenda-restrict nil)
         (files (org-agenda-files 'unrestricted)) entries file
         (org-agenda-buffer nil))
    ;; Get all entries which may contain an appt
    (org-agenda-prepare-buffers files)
    (while (setq file (pop files))
      (setq entries
            (delq nil
                  (append entries
                          (apply 'org-agenda-get-day-entries
                                 file today scope)))))
    ;; Map thru entries and find if we should filter them out
    (mapc
     (lambda (x)
       (let* ((evt (org-trim
                    (replace-regexp-in-string
                     org-bracket-link-regexp "\\3"
                     (or (get-text-property 1 'txt x) ""))))
              (cat (get-text-property (1- (length x)) 'org-category x))
              (tod (get-text-property 1 'time-of-day x))
              (ok (or (null filter)
                      (and (stringp filter) (string-match filter evt))
                      (and (functionp filter) (funcall filter x))
                      (and (listp filter)
                           (let ((cat-filter (cadr (assq 'category filter)))
                                 (evt-filter (cadr (assq 'headline filter))))
                             (or (and (stringp cat-filter)
                                      (string-match cat-filter cat))
                                 (and (stringp evt-filter)
                                      (string-match evt-filter evt)))))))
              (wrn (get-text-property 1 'warntime x)))
         ;; FIXME: Shall we remove text-properties for the appt text?
         ;; (setq evt (set-text-properties 0 (length evt) nil evt))
         (when (and ok tod (not (string-match "\\`DONE\\|CANCELLED" evt)))
           (setq tod (concat "00" (number-to-string tod)))
           (setq tod (when (string-match
                            "\\([0-9]\\{1,2\\}\\)\\([0-9]\\{2\\}\\)\\'" tod)
                       (concat (match-string 1 tod) ":"
                               (match-string 2 tod))))
           (when (appt-add tod evt wrn)
             (setq cnt (1+ cnt))))))
     entries)
    (org-release-buffers org-agenda-new-buffers)
    (if (eq cnt 0)
        (message "No event to add")
      (message "Added %d event%s for today" cnt (if (> cnt 1) "s" "")))))

(defun org-agenda-today-p (date)
  "Non nil when DATE means today.
DATE is either a list of the form (month day year) or a number of
days as returned by `calendar-absolute-from-gregorian' or
`org-today'.  This function considers `org-extend-today-until'
when defining today."
  (eq (org-today)
      (if (consp date) (calendar-absolute-from-gregorian date) date)))

(defun org-agenda-todo-yesterday (&optional arg)
  "Like `org-agenda-todo' but the time of change will be 23:59 of yesterday."
  (interactive "P")
  (let* ((org-use-effective-time t)
	 (hour (nth 2 (decode-time (org-current-time))))
         (org-extend-today-until (1+ hour)))
    (org-agenda-todo arg)))

(provide 'org-agenda)

;;; org-agenda.el ends here
