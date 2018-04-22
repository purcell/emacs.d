;;; org-plus-contrib-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ob-sclang" "ob-sclang.el" (23041 44313 818986
;;;;;;  26000))
;;; Generated autoloads from ob-sclang.el

(autoload 'org-babel-execute:sclang "ob-sclang" "\
Org-mode Babel sclang hook for evaluate `BODY' with `PARAMS'.

\(fn BODY PARAMS)" nil nil)

;;;***

;;;### (autoloads nil "org" "org.el" (23041 44314 194983 211000))
;;; Generated autoloads from org.el

(autoload 'org-babel-do-load-languages "org" "\
Load the languages defined in `org-babel-load-languages'.

\(fn SYM VALUE)" nil nil)

(autoload 'org-babel-load-file "org" "\
Load Emacs Lisp source code blocks in the Org FILE.
This function exports the source code using `org-babel-tangle'
and then loads the resulting file using `load-file'.  With prefix
arg (noninteractively: 2nd arg) COMPILE the tangled Emacs Lisp
file to byte-code before it is loaded.

\(fn FILE &optional COMPILE)" t nil)

(autoload 'org-version "org" "\
Show the Org version.
Interactively, or when MESSAGE is non-nil, show it in echo area.
With prefix argument, or when HERE is non-nil, insert it at point.
In non-interactive uses, a reduced version string is output unless
FULL is given.

\(fn &optional HERE FULL MESSAGE)" t nil)

(autoload 'turn-on-orgtbl "org" "\
Unconditionally turn on `orgtbl-mode'.

\(fn)" nil nil)

(autoload 'org-clock-persistence-insinuate "org" "\
Set up hooks for clock persistence.

\(fn)" nil nil)

(autoload 'org-mode "org" "\
Outline-based notes management and organizer, alias
\"Carsten's outline-mode for keeping track of everything.\"

Org mode develops organizational tasks around a NOTES file which
contains information about projects as plain text.  Org mode is
implemented on top of Outline mode, which is ideal to keep the content
of large files well structured.  It supports ToDo items, deadlines and
time stamps, which magically appear in the diary listing of the Emacs
calendar.  Tables are easily created with a built-in table editor.
Plain text URL-like links connect to websites, emails (VM), Usenet
messages (Gnus), BBDB entries, and any files related to the project.
For printing and sharing of notes, an Org file (or a part of it)
can be exported as a structured ASCII or HTML file.

The following commands are available:

\\{org-mode-map}

\(fn)" t nil)

(autoload 'org-cycle "org" "\
TAB-action and visibility cycling for Org mode.

This is the command invoked in Org mode by the `TAB' key.  Its main
purpose is outline visibility cycling, but it also invokes other actions
in special contexts.

When this function is called with a `\\[universal-argument]' prefix, rotate the entire
buffer through 3 states (global cycling)
  1. OVERVIEW: Show only top-level headlines.
  2. CONTENTS: Show all headlines of all levels, but no body text.
  3. SHOW ALL: Show everything.

With a `\\[universal-argument] \\[universal-argument]' prefix argument, switch to the startup visibility,
determined by the variable `org-startup-folded', and by any VISIBILITY
properties in the buffer.

With a `\\[universal-argument] \\[universal-argument] \\[universal-argument]' prefix argument, show the entire buffer, including
any drawers.

When inside a table, re-align the table and move to the next field.

When point is at the beginning of a headline, rotate the subtree started
by this line through 3 different states (local cycling)
  1. FOLDED:   Only the main headline is shown.
  2. CHILDREN: The main headline and the direct children are shown.
               From this state, you can move to one of the children
               and zoom in further.
  3. SUBTREE:  Show the entire subtree, including body text.
If there is no subtree, switch directly from CHILDREN to FOLDED.

When point is at the beginning of an empty headline and the variable
`org-cycle-level-after-item/entry-creation' is set, cycle the level
of the headline by demoting and promoting it to likely levels.  This
speeds up creation document structure by pressing `TAB' once or several
times right after creating a new headline.

When there is a numeric prefix, go up to a heading with level ARG, do
a `show-subtree' and return to the previous cursor position.  If ARG
is negative, go up that many levels.

When point is not at the beginning of a headline, execute the global
binding for `TAB', which is re-indenting the line.  See the option
`org-cycle-emulate-tab' for details.

As a special case, if point is at the beginning of the buffer and there is
no headline in line 1, this function will act as if called with prefix arg
\(`\\[universal-argument] TAB', same as `S-TAB') also when called without prefix arg, but only
if the variable `org-cycle-global-at-bob' is t.

\(fn &optional ARG)" t nil)

(autoload 'org-global-cycle "org" "\
Cycle the global visibility.  For details see `org-cycle'.
With `\\[universal-argument]' prefix ARG, switch to startup visibility.
With a numeric prefix, show all headlines up to that level.

\(fn &optional ARG)" t nil)
(put 'orgstruct-heading-prefix-regexp 'safe-local-variable 'stringp)

(autoload 'orgstruct-mode "org" "\
Toggle the minor mode `orgstruct-mode'.
This mode is for using Org mode structure commands in other
modes.  The following keys behave as if Org mode were active, if
the cursor is on a headline, or on a plain list item (both as
defined by Org mode).

\(fn &optional ARG)" t nil)

(autoload 'turn-on-orgstruct "org" "\
Unconditionally turn on `orgstruct-mode'.

\(fn)" nil nil)

(autoload 'turn-on-orgstruct++ "org" "\
Unconditionally turn on `orgstruct++-mode'.

\(fn)" nil nil)

(autoload 'org-run-like-in-org-mode "org" "\
Run a command, pretending that the current buffer is in Org mode.
This will temporarily bind local variables that are typically bound in
Org mode to the values they have in Org mode, and then interactively
call CMD.

\(fn CMD)" nil nil)

(autoload 'org-store-link "org" "\
Store an org-link to the current location.
\\<org-mode-map>
This link is added to `org-stored-links' and can later be inserted
into an Org buffer with `org-insert-link' (`\\[org-insert-link]').

For some link types, a `\\[universal-argument]' prefix ARG is interpreted.  A single
`\\[universal-argument]' negates `org-context-in-file-links' for file links or
`org-gnus-prefer-web-links' for links to Usenet articles.

A `\\[universal-argument] \\[universal-argument]' prefix ARG forces skipping storing functions that are not
part of Org core.

A `\\[universal-argument] \\[universal-argument] \\[universal-argument]' prefix ARG forces storing a link for each line in the
active region.

\(fn ARG)" t nil)

(autoload 'org-insert-link-global "org" "\
Insert a link like Org mode does.
This command can be called in any mode to insert a link in Org syntax.

\(fn)" t nil)

(autoload 'org-open-at-point-global "org" "\
Follow a link or time-stamp like Org mode does.
This command can be called in any mode to follow an external link
or a time-stamp that has Org mode syntax.  Its behavior is
undefined when called on internal links (e.g., fuzzy links).
Raise an error when there is nothing to follow.

\(fn)" t nil)

(autoload 'org-open-link-from-string "org" "\
Open a link in the string S, as if it was in Org mode.

\(fn S &optional ARG REFERENCE-BUFFER)" t nil)

(autoload 'org-switchb "org" "\
Switch between Org buffers.

With `\\[universal-argument]' prefix, restrict available buffers to files.

With `\\[universal-argument] \\[universal-argument]' prefix, restrict available buffers to agenda files.

\(fn &optional ARG)" t nil)

(autoload 'org-cycle-agenda-files "org" "\
Cycle through the files in `org-agenda-files'.
If the current buffer visits an agenda file, find the next one in the list.
If the current buffer does not, find the first agenda file.

\(fn)" t nil)

(autoload 'org-submit-bug-report "org" "\
Submit a bug report on Org via mail.

Don't hesitate to report any problems or inaccurate documentation.

If you don't have setup sending mail from (X)Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your Org version and configuration.

\(fn)" t nil)

(autoload 'org-reload "org" "\
Reload all org lisp files.
With prefix arg UNCOMPILED, load the uncompiled versions.

\(fn &optional UNCOMPILED)" t nil)

(autoload 'org-customize "org" "\
Call the customize function with org as argument.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "org-agenda" "org-agenda.el" (23041 44313 870985
;;;;;;  636000))
;;; Generated autoloads from org-agenda.el

(autoload 'org-toggle-sticky-agenda "org-agenda" "\
Toggle `org-agenda-sticky'.

\(fn &optional ARG)" t nil)

(autoload 'org-agenda "org-agenda" "\
Dispatch agenda commands to collect entries to the agenda buffer.
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
\(if active).

\(fn &optional ARG ORG-KEYS RESTRICTION)" t nil)

(autoload 'org-batch-agenda "org-agenda" "\
Run an agenda command in batch mode and send the result to STDOUT.
If CMD-KEY is a string of length 1, it is used as a key in
`org-agenda-custom-commands' and triggers this command.  If it is a
longer string it is used as a tags/todo match string.
Parameters are alternating variable names and values that will be bound
before running the agenda command.

\(fn CMD-KEY &rest PARAMETERS)" nil t)

(autoload 'org-batch-agenda-csv "org-agenda" "\
Run an agenda command in batch mode and send the result to STDOUT.
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
agenda-day   The day in the agenda where this is listed

\(fn CMD-KEY &rest PARAMETERS)" nil t)

(autoload 'org-store-agenda-views "org-agenda" "\
Store agenda views.

\(fn &rest PARAMETERS)" t nil)

(autoload 'org-batch-store-agenda-views "org-agenda" "\
Run all custom agenda commands that have a file argument.

\(fn &rest PARAMETERS)" nil t)

(autoload 'org-agenda-list "org-agenda" "\
Produce a daily/weekly view from all files in variable `org-agenda-files'.
The view will be for the current day or week, but from the overview buffer
you will be able to go to other days/weeks.

With a numeric prefix argument in an interactive call, the agenda will
span ARG days.  Lisp programs should instead specify SPAN to change
the number of days.  SPAN defaults to `org-agenda-span'.

START-DAY defaults to TODAY, or to the most recent match for the weekday
given in `org-agenda-start-on-weekday'.

When WITH-HOUR is non-nil, only include scheduled and deadline
items if they have an hour specification like [h]h:mm.

\(fn &optional ARG START-DAY SPAN WITH-HOUR)" t nil)

(autoload 'org-search-view "org-agenda" "\
Show all entries that contain a phrase or words or regular expressions.

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
is active.

\(fn &optional TODO-ONLY STRING EDIT-AT)" t nil)

(autoload 'org-todo-list "org-agenda" "\
Show all (not done) TODO entries from all agenda file in a single list.
The prefix arg can be used to select a specific TODO keyword and limit
the list to these.  When using `\\[universal-argument]', you will be prompted
for a keyword.  A numeric prefix directly selects the Nth keyword in
`org-todo-keywords-1'.

\(fn &optional ARG)" t nil)

(autoload 'org-tags-view "org-agenda" "\
Show all headlines for all `org-agenda-files' matching a TAGS criterion.
The prefix arg TODO-ONLY limits the search to TODO entries.

\(fn &optional TODO-ONLY MATCH)" t nil)

(autoload 'org-agenda-list-stuck-projects "org-agenda" "\
Create agenda view for projects that are stuck.
Stuck projects are project that have no next actions.  For the definitions
of what a project is and how to check if it stuck, customize the variable
`org-stuck-projects'.

\(fn &rest IGNORE)" t nil)

(autoload 'org-diary "org-agenda" "\
Return diary information from org files.
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
function from a program - use `org-agenda-get-day-entries' instead.

\(fn &rest ARGS)" nil nil)

(autoload 'org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item "org-agenda" "\
Do we have a reason to ignore this TODO entry because it has a time stamp?

\(fn &optional END)" nil nil)

(autoload 'org-agenda-set-restriction-lock "org-agenda" "\
Set restriction lock for agenda, to current subtree or file.
Restriction will be the file if TYPE is `file', or if type is the
universal prefix \\='(4), or if the cursor is before the first headline
in the file.  Otherwise, restriction will be to the current subtree.

\(fn &optional TYPE)" t nil)

(autoload 'org-calendar-goto-agenda "org-agenda" "\
Compute the Org agenda for the calendar date displayed at the cursor.
This is a command that has to be installed in `calendar-mode-map'.

\(fn)" t nil)

(autoload 'org-agenda-to-appt "org-agenda" "\
Activate appointments found in `org-agenda-files'.

With a `\\[universal-argument]' prefix, refresh the list of appointments.

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
to override `appt-message-warning-time'.

\(fn &optional REFRESH FILTER &rest ARGS)" t nil)

;;;***

;;;### (autoloads nil "org-annotate-file" "org-annotate-file.el"
;;;;;;  (23041 44313 874985 606000))
;;; Generated autoloads from org-annotate-file.el

(autoload 'org-annotate-file "org-annotate-file" "\
Visit `org-annotate-file-storage-file` and add a new annotation section.
The annotation is opened at the new section which will be referencing
the point in the current file.

\(fn)" t nil)

(autoload 'org-annotate-file-show-section "org-annotate-file" "\
Add or show annotation entry in STORAGE-FILE and return the buffer.
The annotation will link to ANNOTATED-BUFFER if specified,
  otherwise the current buffer is used.

\(fn STORAGE-FILE &optional ANNOTATED-BUFFER)" nil nil)

;;;***

;;;### (autoloads nil "org-capture" "org-capture.el" (23041 44313
;;;;;;  898985 426000))
;;; Generated autoloads from org-capture.el

(autoload 'org-capture-string "org-capture" "\
Capture STRING with the template selected by KEYS.

\(fn STRING &optional KEYS)" t nil)

(autoload 'org-capture "org-capture" "\
Capture something.
\\<org-capture-mode-map>
This will let you select a template from `org-capture-templates', and
then file the newly captured information.  The text is immediately
inserted at the target location, and an indirect buffer is shown where
you can edit it.  Pressing `\\[org-capture-finalize]' brings you back to the previous
state of Emacs, so that you can continue your work.

When called interactively with a `\\[universal-argument]' prefix argument GOTO, don't
capture anything, just go to the file/headline where the selected
template stores its notes.

With a `\\[universal-argument] \\[universal-argument]' prefix argument, go to the last note stored.

When called with a `C-0' (zero) prefix, insert a template at point.

When called with a `C-1' (one) prefix, force prompting for a date when
a datetree entry is made.

ELisp programs can set KEYS to a string associated with a template
in `org-capture-templates'.  In this case, interactive selection
will be bypassed.

If `org-capture-use-agenda-date' is non-nil, capturing from the
agenda will use the date at point as the default date.  Then, a
`C-1' prefix will tell the capture process to use the HH:MM time
of the day at point (if any) or the current HH:MM time.

\(fn &optional GOTO KEYS)" t nil)

(autoload 'org-capture-import-remember-templates "org-capture" "\
Set `org-capture-templates' to be similar to `org-remember-templates'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "org-colview" "org-colview.el" (23041 44313
;;;;;;  918985 276000))
;;; Generated autoloads from org-colview.el

(autoload 'org-columns-remove-overlays "org-colview" "\
Remove all currently active column overlays.

\(fn)" t nil)

(autoload 'org-columns-get-format-and-top-level "org-colview" "\


\(fn)" nil nil)

(autoload 'org-columns "org-colview" "\
Turn on column view on an Org mode file.

Column view applies to the whole buffer if point is before the
first headline.  Otherwise, it applies to the first ancestor
setting \"COLUMNS\" property.  If there is none, it defaults to
the current headline.  With a `\\[universal-argument]' prefix argument, turn on column
view for the whole buffer unconditionally.

When COLUMNS-FMT-STRING is non-nil, use it as the column format.

\(fn &optional GLOBAL COLUMNS-FMT-STRING)" t nil)

(autoload 'org-columns-compute "org-colview" "\
Summarize the values of PROPERTY hierarchically.
Also update existing values for PROPERTY according to the first
column specification.

\(fn PROPERTY)" t nil)

(autoload 'org-dblock-write:columnview "org-colview" "\
Write the column view table.
PARAMS is a property list of parameters:

:id       the :ID: property of the entry where the columns view
          should be built.  When the symbol `local', call locally.
          When `global' call column view with the cursor at the beginning
          of the buffer (usually this means that the whole buffer switches
          to column view).  When \"file:path/to/file.org\", invoke column
          view at the start of that file.  Otherwise, the ID is located
          using `org-id-find'.
:hlines   When t, insert a hline before each item.  When a number, insert
          a hline before each level <= that number.
:indent   When non-nil, indent each ITEM field according to its level.
:vlines   When t, make each column a colgroup to enforce vertical lines.
:maxlevel When set to a number, don't capture headlines below this level.
:skip-empty-rows
          When t, skip rows where all specifiers other than ITEM are empty.
:width    apply widths specified in columns format using <N> specifiers.
:format   When non-nil, specify the column view format to use.

\(fn PARAMS)" nil nil)

(autoload 'org-columns-insert-dblock "org-colview" "\
Create a dynamic block capturing a column view table.

\(fn)" t nil)

(autoload 'org-agenda-columns "org-colview" "\
Turn on or update column view in the agenda.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "org-compat" "org-compat.el" (23041 44313 922985
;;;;;;  245000))
;;; Generated autoloads from org-compat.el

(autoload 'org-check-version "org-compat" "\
Try very hard to provide sensible version strings.

\(fn)" nil t)

;;;***

;;;### (autoloads nil "org-contacts" "org-contacts.el" (23041 44313
;;;;;;  926985 216000))
;;; Generated autoloads from org-contacts.el

(autoload 'org-contacts "org-contacts" "\
Create agenda view for contacts matching NAME.

\(fn NAME)" t nil)

;;;***

;;;### (autoloads nil "org-duration" "org-duration.el" (23041 44313
;;;;;;  954985 6000))
;;; Generated autoloads from org-duration.el

(autoload 'org-duration-set-regexps "org-duration" "\
Set duration related regexps.

\(fn)" t nil)

(autoload 'org-duration-p "org-duration" "\
Non-nil when string S is a time duration.

\(fn S)" nil nil)

(autoload 'org-duration-to-minutes "org-duration" "\
Return number of minutes of DURATION string.

When optional argument CANONICAL is non-nil, ignore
`org-duration-units' and use standard time units value.

A bare number is translated into minutes.  The empty string is
translated into 0.0.

Return value as a float.  Raise an error if duration format is
not recognized.

\(fn DURATION &optional CANONICAL)" nil nil)

(autoload 'org-duration-from-minutes "org-duration" "\
Return duration string for a given number of MINUTES.

Format duration according to `org-duration-format' or FMT, when
non-nil.

When optional argument CANONICAL is non-nil, ignore
`org-duration-units' and use standard time units value.

Raise an error if expected format is unknown.

\(fn MINUTES &optional FMT CANONICAL)" nil nil)

(autoload 'org-duration-h:mm-only-p "org-duration" "\
Non-nil when every duration in TIMES has \"H:MM\" or \"H:MM:SS\" format.

TIMES is a list of duration strings.

Return nil if any duration is expressed with units, as defined in
`org-duration-units'.  Otherwise, if any duration is expressed
with \"H:MM:SS\" format, return `h:mm:ss'.  Otherwise, return
`h:mm'.

\(fn TIMES)" nil nil)

;;;***

;;;### (autoloads nil "org-eldoc" "org-eldoc.el" (23041 44313 962984
;;;;;;  945000))
;;; Generated autoloads from org-eldoc.el

(autoload 'org-eldoc-load "org-eldoc" "\
Set up org-eldoc documentation function.

\(fn)" t nil)

(add-hook 'org-mode-hook #'org-eldoc-load)

;;;***

;;;### (autoloads nil "org-link-edit" "org-link-edit.el" (23041 44314
;;;;;;  50984 287000))
;;; Generated autoloads from org-link-edit.el

(autoload 'org-link-edit-forward-slurp "org-link-edit" "\
Slurp N trailing blobs into link's description.

  The [[http://orgmode.org/][Org mode]] site

                        |
                        v

  The [[http://orgmode.org/][Org mode site]]

A blob is a block of non-whitespace characters.  When slurping
forward, trailing punctuation characters are not considered part
of a blob.

After slurping, return the slurped text and move point to the
beginning of the link.

If N is negative, slurp leading blobs instead of trailing blobs.

\(fn &optional N)" t nil)

(autoload 'org-link-edit-backward-slurp "org-link-edit" "\
Slurp N leading blobs into link's description.

  The [[http://orgmode.org/][Org mode]] site

                        |
                        v

  [[http://orgmode.org/][The Org mode]] site

A blob is a block of non-whitespace characters.

After slurping, return the slurped text and move point to the
beginning of the link.

If N is negative, slurp trailing blobs instead of leading blobs.

\(fn &optional N)" t nil)

(autoload 'org-link-edit-forward-barf "org-link-edit" "\
Barf N trailing blobs from link's description.

  The [[http://orgmode.org/][Org mode]] site

                        |
                        v

  The [[http://orgmode.org/][Org]] mode site

A blob is a block of non-whitespace characters.

After barfing, return the barfed text and move point to the
beginning of the link.

If N is negative, barf leading blobs instead of trailing blobs.

\(fn &optional N)" t nil)

(autoload 'org-link-edit-backward-barf "org-link-edit" "\
Barf N leading blobs from link's description.

  The [[http://orgmode.org/][Org mode]] site

                        |
                        v

  The Org [[http://orgmode.org/][mode]] site

A blob is a block of non-whitespace characters.

After barfing, return the barfed text and move point to the
beginning of the link.

If N is negative, barf trailing blobs instead of leading blobs.

\(fn &optional N)" t nil)

(autoload 'org-link-edit-transport-next-link "org-link-edit" "\
Move the next link to point.

If the region is active, use the selected text as the link's
description.  Otherwise, use the word at point.

With prefix argument PREVIOUS, move the previous link instead of
the next link.

Non-interactively, use the text between BEG and END as the
description, moving the next (or previous) link relative BEG and
END.

\(fn &optional PREVIOUS BEG END)" t nil)

;;;***

;;;### (autoloads nil "org-lint" "org-lint.el" (23041 44314 54984
;;;;;;  257000))
;;; Generated autoloads from org-lint.el

(autoload 'org-lint "org-lint" "\
Check current Org buffer for syntax mistakes.

By default, run all checkers.  With a `\\[universal-argument]' prefix ARG, select one
category of checkers only.  With a `\\[universal-argument] \\[universal-argument]' prefix, run one precise
checker by its name.

ARG can also be a list of checker names, as symbols, to run.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "org-mac-link" "org-mac-link.el" (23041 44314
;;;;;;  70984 137000))
;;; Generated autoloads from org-mac-link.el

(autoload 'org-mac-grab-link "org-mac-link" "\
Prompt for an application to grab a link from.
When done, go grab the link, and insert it at point.

\(fn)" t nil)

(autoload 'org-mac-firefox-get-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-firefox-insert-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-vimperator-get-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-vimperator-insert-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-chrome-get-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-chrome-insert-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-safari-get-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-safari-insert-frontmost-url "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-together-get-selected "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-together-insert-selected "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-finder-item-get-selected "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-finder-insert-selected "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-addressbook-item-get-selected "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-addressbook-insert-selected "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-skim-get-page "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-skim-insert-page "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-acrobat-get-page "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-acrobat-insert-page "org-mac-link" "\


\(fn)" t nil)

(autoload 'org-mac-outlook-message-get-links "org-mac-link" "\
Create links to the messages currently selected or flagged in Microsoft Outlook.app.
This will use AppleScript to get the message-id and the subject of the
messages in Microsoft Outlook.app and make a link out of it.
When SELECT-OR-FLAG is \"s\", get the selected messages (this is also
the default).  When SELECT-OR-FLAG is \"f\", get the flagged messages.
The Org-syntax text will be pushed to the kill ring, and also returned.

\(fn &optional SELECT-OR-FLAG)" t nil)

(autoload 'org-mac-outlook-message-insert-selected "org-mac-link" "\
Insert a link to the messages currently selected in Microsoft Outlook.app.
This will use AppleScript to get the message-id and the subject
of the active mail in Microsoft Outlook.app and make a link out
of it.

\(fn)" t nil)

(autoload 'org-mac-outlook-message-insert-flagged "org-mac-link" "\
Asks for an org buffer and a heading within it, and replace message links.
If heading exists, delete all mac-outlook:// links within
heading's first level.  If heading doesn't exist, create it at
point-max.  Insert list of mac-outlook:// links to flagged mail
after heading.

\(fn ORG-BUFFER ORG-HEADING)" t nil)

(autoload 'org-mac-evernote-note-insert-selected "org-mac-link" "\
Insert a link to the notes currently selected in Evernote.app.
This will use AppleScript to get the note id and the title of the
note(s) in Evernote.app and make a link out of it/them.

\(fn)" t nil)

(autoload 'org-mac-devonthink-item-insert-selected "org-mac-link" "\
Insert a link to the item(s) currently selected in DEVONthink Pro Office.
This will use AppleScript to get the `uuid'(s) and the name(s) of the
selected items in DEVONthink Pro Office and make link(s) out of it/them.

\(fn)" t nil)

(autoload 'org-mac-message-get-links "org-mac-link" "\
Create links to the messages currently selected or flagged in Mail.app.
This will use AppleScript to get the message-id and the subject of the
messages in Mail.app and make a link out of it.
When SELECT-OR-FLAG is \"s\", get the selected messages (this is also
the default).  When SELECT-OR-FLAG is \"f\", get the flagged messages.
The Org-syntax text will be pushed to the kill ring, and also returned.

\(fn &optional SELECT-OR-FLAG)" t nil)

(autoload 'org-mac-message-insert-selected "org-mac-link" "\
Insert a link to the messages currently selected in Mail.app.
This will use AppleScript to get the message-id and the subject of the
active mail in Mail.app and make a link out of it.

\(fn)" t nil)

(autoload 'org-mac-message-insert-flagged "org-mac-link" "\
Asks for an org buffer and a heading within it, and replace message links.
If heading exists, delete all message:// links within heading's first
level.  If heading doesn't exist, create it at point-max.  Insert
list of message:// links to flagged mail after heading.

\(fn ORG-BUFFER ORG-HEADING)" t nil)

;;;***

;;;### (autoloads nil "org-macs" "org-macs.el" (23041 44314 78984
;;;;;;  78000))
;;; Generated autoloads from org-macs.el

(autoload 'org-load-noerror-mustsuffix "org-macs" "\
Load FILE with optional arguments NOERROR and MUSTSUFFIX.

\(fn FILE)" nil t)

;;;***

;;;### (autoloads nil "org-passwords" "org-passwords.el" (23041 44314
;;;;;;  114983 809000))
;;; Generated autoloads from org-passwords.el

(autoload 'org-passwords-mode "org-passwords" "\
Mode for storing passwords

\(fn)" t nil)

(autoload 'org-passwords "org-passwords" "\
Open the password file. Open the password file defined by the
variable `org-password-file' in read-only mode and kill that
buffer later according to the value of the variable
`org-passwords-time-opened'. It also adds the `org-password-file'
to the auto-mode-alist so that it is opened with its mode being
`org-passwords-mode'.

With prefix arg ARG, the command does not set up a timer to kill the buffer.

With a double prefix arg \\[universal-argument] \\[universal-argument], open the file for editing.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "org-registry" "org-registry.el" (23041 44314
;;;;;;  130983 689000))
;;; Generated autoloads from org-registry.el

(autoload 'org-registry-show "org-registry" "\
Show Org files where there are links pointing to the current
buffer.

\(fn &optional VISIT)" t nil)

(autoload 'org-registry-visit "org-registry" "\
If an Org file contains a link to the current location, visit
this file.

\(fn)" t nil)

(autoload 'org-registry-initialize "org-registry" "\
Initialize `org-registry-alist'.
If FROM-SCRATCH is non-nil or the registry does not exist yet,
create a new registry from scratch and eval it. If the registry
exists, eval `org-registry-file' and make it the new value for
`org-registry-alist'.

\(fn &optional FROM-SCRATCH)" t nil)

(autoload 'org-registry-insinuate "org-registry" "\
Call `org-registry-update' after saving in Org-mode.
Use with caution.  This could slow down things a bit.

\(fn)" t nil)

(autoload 'org-registry-update "org-registry" "\
Update the registry for the current Org file.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "org-screenshot" "org-screenshot.el" (23041
;;;;;;  44314 138983 630000))
;;; Generated autoloads from org-screenshot.el

(autoload 'org-screenshot-take "org-screenshot" "\
Take a screenshot and insert link to it at point, if image
display is already on (see \\[org-toggle-inline-images])
screenshot will be displayed as an image

Screen area for the screenshot is selected with the mouse, left
click on a window screenshots that window, while left click and
drag selects a region. Pressing any key cancels the screen shot

With `C-u' universal argument waits one second after target is
selected before taking the screenshot. With double `C-u' wait two
seconds.

With triple `C-u' wait 3 seconds, and also rings the bell when
screenshot is done, any more `C-u' after that increases delay by
2 seconds

\(fn &optional DELAY)" t nil)

(autoload 'org-screenshot-rotate-prev "org-screenshot" "\
Rotate last screenshot with one of the previously taken
screenshots from the same directory. If DIR is negative, rotate
in the other direction

\(fn DIR)" t nil)

(autoload 'org-screenshot-rotate-next "org-screenshot" "\
Rotate last screenshot with one of the previously taken
screenshots from the same directory. If DIR is negative, rotate
in the other direction

\(fn DIR)" t nil)

(autoload 'org-screenshot-show-unused "org-screenshot" "\
Open A Dired buffer with unused screenshots marked

\(fn)" t nil)

;;;***

;;;### (autoloads nil "org-toc" "org-toc.el" (23041 44314 166983
;;;;;;  421000))
;;; Generated autoloads from org-toc.el

(autoload 'org-toc-show "org-toc" "\
Show the table of contents of the current Org-mode buffer.

\(fn &optional DEPTH POSITION)" t nil)

;;;***

;;;### (autoloads nil "org-track" "org-track.el" (23041 44314 170983
;;;;;;  391000))
;;; Generated autoloads from org-track.el

(autoload 'org-track-fetch-package "org-track" "\
Fetch Org package depending on `org-track-fetch-package-extension'.
If DIRECTORY is defined, unpack the package there, i.e. add the
subdirectory org-mode/ to DIRECTORY.

\(fn &optional DIRECTORY)" t nil)

(autoload 'org-track-compile-org "org-track" "\
Compile all *.el files that come with org-mode.
Generate the autoloads file `org-loaddefs.el'.

DIRECTORY is where the directory org-mode/ lives (i.e. the
          parent directory of your local repo.

\(fn &optional DIRECTORY)" t nil)

;;;***

;;;### (autoloads nil "org-version" "org-version.el" (23041 44314
;;;;;;  178983 331000))
;;; Generated autoloads from org-version.el

(autoload 'org-release "org-version" "\
The release version of Org.
Inserted by installing Org mode or when a release is made.

\(fn)" nil nil)

(autoload 'org-git-version "org-version" "\
The Git version of org-mode.
Inserted by installing Org or when a release is made.

\(fn)" nil nil)

(defvar org-odt-data-dir "/usr/share/emacs/etc/org" "\
The location of ODT styles.")

;;;***

;;;### (autoloads nil "ox-freemind" "ox-freemind.el" (23041 44314
;;;;;;  222983 2000))
;;; Generated autoloads from ox-freemind.el

(autoload 'org-freemind-export-to-freemind "ox-freemind" "\
Export current buffer to a Freemind Mindmap file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

;;;***

;;;### (autoloads nil "ox-koma-letter" "ox-koma-letter.el" (23041
;;;;;;  44314 238982 883000))
;;; Generated autoloads from ox-koma-letter.el

(autoload 'org-koma-letter-export-as-latex "ox-koma-letter" "\
Export current buffer as a KOMA Scrlttr2 letter.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a proeprty list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org KOMA-LETTER Export*\".  It
will be displayed if `org-export-show-temporary-export-buffer' is
non-nil.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(autoload 'org-koma-letter-export-to-latex "ox-koma-letter" "\
Export current buffer as a KOMA Scrlttr2 letter (tex).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(autoload 'org-koma-letter-export-to-pdf "ox-koma-letter" "\
Export current buffer as a KOMA Scrlttr2 letter (pdf).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

;;;***

;;;### (autoloads nil "ox-rss" "ox-rss.el" (23041 44314 262982 704000))
;;; Generated autoloads from ox-rss.el

(autoload 'org-rss-export-as-rss "ox-rss" "\
Export current buffer to a RSS buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org RSS Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-rss-export-to-rss "ox-rss" "\
Export current buffer to a RSS file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-rss-publish-to-rss "ox-rss" "\
Publish an org file to RSS.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name.

\(fn PLIST FILENAME PUB-DIR)" nil nil)

;;;***

;;;### (autoloads nil "ox-taskjuggler" "ox-taskjuggler.el" (23041
;;;;;;  44314 270982 644000))
;;; Generated autoloads from ox-taskjuggler.el

(autoload 'org-taskjuggler-export "ox-taskjuggler" "\
Export current buffer to a TaskJuggler file.

The exporter looks for a tree with tag that matches
`org-taskjuggler-project-tag' and takes this as the tasks for
this project.  The first node of this tree defines the project
properties such as project name and project period.

If there is a tree with tag that matches
`org-taskjuggler-resource-tag' this tree is taken as resources
for the project.  If no resources are specified, a default
resource is created and allocated to the project.

Also the TaskJuggler project will be created with default reports
as defined in `org-taskjuggler-default-reports'.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-taskjuggler-export-and-process "ox-taskjuggler" "\
Export current buffer to a TaskJuggler file and process it.

The exporter looks for a tree with tag that matches
`org-taskjuggler-project-tag' and takes this as the tasks for
this project.  The first node of this tree defines the project
properties such as project name and project period.

If there is a tree with tag that matches
`org-taskjuggler-resource-tag' this tree is taken as resources
for the project.  If no resources are specified, a default
resource is created and allocated to the project.

Also the TaskJuggler project will be created with default reports
as defined in `org-taskjuggler-default-reports'.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return a list of reports.

\(fn &optional SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-taskjuggler-export-process-and-open "ox-taskjuggler" "\
Export current buffer to a TaskJuggler file, process and open it.

Export and process the file using
`org-taskjuggler-export-and-process' and open the generated
reports with a browser.

If you are targeting TaskJuggler 2.4 (see
`org-taskjuggler-target-version') the processing and display of
the reports is done using the TaskJuggler GUI.

\(fn &optional SUBTREEP VISIBLE-ONLY)" t nil)

;;;***

;;;### (autoloads nil nil ("ob-C.el" "ob-J.el" "ob-R.el" "ob-abc.el"
;;;;;;  "ob-asymptote.el" "ob-awk.el" "ob-calc.el" "ob-clojure.el"
;;;;;;  "ob-comint.el" "ob-coq.el" "ob-core.el" "ob-csharp.el" "ob-css.el"
;;;;;;  "ob-ditaa.el" "ob-dot.el" "ob-ebnf.el" "ob-emacs-lisp.el"
;;;;;;  "ob-eukleides.el" "ob-eval.el" "ob-exp.el" "ob-fomus.el"
;;;;;;  "ob-forth.el" "ob-fortran.el" "ob-gnuplot.el" "ob-groovy.el"
;;;;;;  "ob-haskell.el" "ob-hledger.el" "ob-io.el" "ob-java.el" "ob-js.el"
;;;;;;  "ob-julia.el" "ob-keys.el" "ob-latex.el" "ob-ledger.el" "ob-lilypond.el"
;;;;;;  "ob-lisp.el" "ob-lob.el" "ob-lua.el" "ob-makefile.el" "ob-mathematica.el"
;;;;;;  "ob-mathomatic.el" "ob-matlab.el" "ob-maxima.el" "ob-mscgen.el"
;;;;;;  "ob-ocaml.el" "ob-octave.el" "ob-org.el" "ob-oz.el" "ob-perl.el"
;;;;;;  "ob-picolisp.el" "ob-plantuml.el" "ob-processing.el" "ob-python.el"
;;;;;;  "ob-ref.el" "ob-ruby.el" "ob-sass.el" "ob-scheme.el" "ob-screen.el"
;;;;;;  "ob-sed.el" "ob-shell.el" "ob-shen.el" "ob-sql.el" "ob-sqlite.el"
;;;;;;  "ob-stan.el" "ob-stata.el" "ob-table.el" "ob-tangle.el" "ob-tcl.el"
;;;;;;  "ob-vala.el" "ob-vbnet.el" "ob.el" "org-archive.el" "org-attach.el"
;;;;;;  "org-bbdb.el" "org-bibtex-extras.el" "org-bibtex.el" "org-bookmark.el"
;;;;;;  "org-checklist.el" "org-choose.el" "org-clock.el" "org-collector.el"
;;;;;;  "org-contribdir.el" "org-crypt.el" "org-ctags.el" "org-datetree.el"
;;;;;;  "org-depend.el" "org-docview.el" "org-drill.el" "org-effectiveness.el"
;;;;;;  "org-element.el" "org-elisp-symbol.el" "org-entities.el"
;;;;;;  "org-eshell.el" "org-eval-light.el" "org-eval.el" "org-eww.el"
;;;;;;  "org-expiry.el" "org-faces.el" "org-feed.el" "org-footnote.el"
;;;;;;  "org-git-link.el" "org-gnus.el" "org-habit.el" "org-id.el"
;;;;;;  "org-indent.el" "org-index.el" "org-info.el" "org-inlinetask.el"
;;;;;;  "org-install.el" "org-interactive-query.el" "org-invoice.el"
;;;;;;  "org-irc.el" "org-learn.el" "org-license.el" "org-list.el"
;;;;;;  "org-loaddefs.el" "org-mac-iCal.el" "org-macro.el" "org-mairix.el"
;;;;;;  "org-man.el" "org-mew.el" "org-mhe.el" "org-mobile.el" "org-mouse.el"
;;;;;;  "org-notify.el" "org-notmuch.el" "org-panel.el" "org-pcomplete.el"
;;;;;;  "org-plot.el" "org-plus-contrib-pkg.el" "org-protocol.el"
;;;;;;  "org-rmail.el" "org-screen.el" "org-secretary.el" "org-src.el"
;;;;;;  "org-static-mathjax.el" "org-sudoku.el" "org-table.el" "org-timer.el"
;;;;;;  "org-velocity.el" "org-vm.el" "org-w3m.el" "org-wikinodes.el"
;;;;;;  "org-wl.el" "ox-ascii.el" "ox-beamer.el" "ox-bibtex.el" "ox-confluence.el"
;;;;;;  "ox-deck.el" "ox-extra.el" "ox-groff.el" "ox-html.el" "ox-icalendar.el"
;;;;;;  "ox-latex.el" "ox-man.el" "ox-md.el" "ox-odt.el" "ox-org.el"
;;;;;;  "ox-publish.el" "ox-s5.el" "ox-texinfo.el" "ox.el") (23041
;;;;;;  44314 306982 374000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org-plus-contrib-autoloads.el ends here
