
;;;### (autoloads (org-customize org-reload org-require-autoloaded-modules
;;;;;;  org-submit-bug-report org-cycle-agenda-files org-switchb
;;;;;;  org-map-entries org-open-link-from-string org-open-at-point-global
;;;;;;  org-insert-link-global org-store-link org-run-like-in-org-mode
;;;;;;  turn-on-orgstruct++ turn-on-orgstruct orgstruct-mode org-global-cycle
;;;;;;  org-mode org-version org-babel-do-load-languages) "org" "org.el"
;;;;;;  (20415 53770))
;;; Generated autoloads from org.el

(autoload 'org-babel-do-load-languages "org" "\
Load the languages defined in `org-babel-load-languages'.

\(fn SYM VALUE)" nil nil)

(autoload 'org-version "org" "\
Show the org-mode version in the echo area.
With prefix arg HERE, insert it at point.

\(fn &optional HERE)" t nil)

(autoload 'org-mode "org" "\
Outline-based notes management and organizer, alias
\"Carsten's outline-mode for keeping track of everything.\"

Org-mode develops organizational tasks around a NOTES file which
contains information about projects as plain text.  Org-mode is
implemented on top of outline-mode, which is ideal to keep the content
of large files well structured.  It supports ToDo items, deadlines and
time stamps, which magically appear in the diary listing of the Emacs
calendar.  Tables are easily created with a built-in table editor.
Plain text URL-like links connect to websites, emails (VM), Usenet
messages (Gnus), BBDB entries, and any files related to the project.
For printing and sharing of notes, an Org-mode file (or a part of it)
can be exported as a structured ASCII or HTML file.

The following commands are available:

\\{org-mode-map}

\(fn)" t nil)

(defvar org-inlinetask-min-level)

(autoload 'org-global-cycle "org" "\
Cycle the global visibility.  For details see `org-cycle'.
With \\[universal-argument] prefix arg, switch to startup visibility.
With a numeric prefix, show all headlines up to that level.

\(fn &optional ARG)" t nil)

(autoload 'orgstruct-mode "org" "\
Toggle the minor mode `orgstruct-mode'.
This mode is for using Org-mode structure commands in other
modes.  The following keys behave as if Org-mode were active, if
the cursor is on a headline, or on a plain list item (both as
defined by Org-mode).

M-up        Move entry/item up
M-down	    Move entry/item down
M-left	    Promote
M-right	    Demote
M-S-up	    Move entry/item up
M-S-down    Move entry/item down
M-S-left    Promote subtree
M-S-right   Demote subtree
M-q	    Fill paragraph and items like in Org-mode
C-c ^	    Sort entries
C-c -	    Cycle list bullet
TAB         Cycle item visibility
M-RET       Insert new heading/item
S-M-RET     Insert new TODO heading / Checkbox item
C-c C-c     Set tags / toggle checkbox

\(fn &optional ARG)" t nil)

(autoload 'turn-on-orgstruct "org" "\
Unconditionally turn on `orgstruct-mode'.

\(fn)" nil nil)

(autoload 'turn-on-orgstruct++ "org" "\
Unconditionally turn on `orgstruct++-mode'.

\(fn)" nil nil)

(autoload 'org-run-like-in-org-mode "org" "\
Run a command, pretending that the current buffer is in Org-mode.
This will temporarily bind local variables that are typically bound in
Org-mode to the values they have in Org-mode, and then interactively
call CMD.

\(fn CMD)" nil nil)

(autoload 'org-store-link "org" "\
\\<org-mode-map>Store an org-link to the current location.
This link is added to `org-stored-links' and can later be inserted
into an org-buffer with \\[org-insert-link].

For some link types, a prefix arg is interpreted:
For links to usenet articles, arg negates `org-gnus-prefer-web-links'.
For file links, arg negates `org-context-in-file-links'.

\(fn ARG)" t nil)

(autoload 'org-insert-link-global "org" "\
Insert a link like Org-mode does.
This command can be called in any mode to insert a link in Org-mode syntax.

\(fn)" t nil)

(autoload 'org-open-at-point-global "org" "\
Follow a link like Org-mode does.
This command can be called in any mode to follow a link that has
Org-mode syntax.

\(fn)" t nil)

(autoload 'org-open-link-from-string "org" "\
Open a link in the string S, as if it was in Org-mode.

\(fn S &optional ARG REFERENCE-BUFFER)" t nil)

(autoload 'org-map-entries "org" "\
Call FUNC at each headline selected by MATCH in SCOPE.

FUNC is a function or a lisp form.  The function will be called without
arguments, with the cursor positioned at the beginning of the headline.
The return values of all calls to the function will be collected and
returned as a list.

The call to FUNC will be wrapped into a save-excursion form, so FUNC
does not need to preserve point.  After evaluation, the cursor will be
moved to the end of the line (presumably of the headline of the
processed entry) and search continues from there.  Under some
circumstances, this may not produce the wanted results.  For example,
if you have removed (e.g. archived) the current (sub)tree it could
mean that the next entry will be skipped entirely.  In such cases, you
can specify the position from where search should continue by making
FUNC set the variable `org-map-continue-from' to the desired buffer
position.

MATCH is a tags/property/todo match as it is used in the agenda tags view.
Only headlines that are matched by this query will be considered during
the iteration.  When MATCH is nil or t, all headlines will be
visited by the iteration.

SCOPE determines the scope of this command.  It can be any of:

nil     The current buffer, respecting the restriction if any
tree    The subtree started with the entry at point
region  The entries within the active region, if any
region-start-level
        The entries within the active region, but only those at
        the same level than the first one.
file    The current buffer, without restriction
file-with-archives
        The current buffer, and any archives associated with it
agenda  All agenda files
agenda-with-archives
        All agenda files with any archive files associated with them
\(file1 file2 ...)
        If this is a list, all files in the list will be scanned

The remaining args are treated as settings for the skipping facilities of
the scanner.  The following items can be given here:

  archive    skip trees with the archive tag.
  comment    skip trees with the COMMENT keyword
  function or Emacs Lisp form:
             will be used as value for `org-agenda-skip-function', so whenever
             the function returns t, FUNC will not be called for that
             entry and search will continue from the point where the
             function leaves it.

If your function needs to retrieve the tags including inherited tags
at the *current* entry, you can use the value of the variable
`org-scanner-tags' which will be much faster than getting the value
with `org-get-tags-at'.  If your function gets properties with
`org-entry-properties' at the *current* entry, bind `org-trust-scanner-tags'
to t around the call to `org-entry-properties' to get the same speedup.
Note that if your function moves around to retrieve tags and properties at
a *different* entry, you cannot use these techniques.

\(fn FUNC &optional MATCH SCOPE &rest SKIP)" nil nil)

(autoload 'org-switchb "org" "\
Switch between Org buffers.
With one prefix argument, restrict available buffers to files.
With two prefix arguments, restrict available buffers to agenda files.

Defaults to `iswitchb' for buffer name completion.
Set `org-completion-use-ido' to make it use ido instead.

\(fn &optional ARG)" t nil)

(defalias 'org-ido-switchb 'org-switchb)

(defalias 'org-iswitchb 'org-switchb)

(autoload 'org-cycle-agenda-files "org" "\
Cycle through the files in `org-agenda-files'.
If the current buffer visits an agenda file, find the next one in the list.
If the current buffer does not, find the first agenda file.

\(fn)" t nil)

(autoload 'org-submit-bug-report "org" "\
Submit a bug report on Org-mode via mail.

Don't hesitate to report any problems or inaccurate documentation.

If you don't have setup sending mail from (X)Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your Org-mode version and configuration.

\(fn)" t nil)

(autoload 'org-require-autoloaded-modules "org" "\
Not documented

\(fn)" t nil)

(autoload 'org-reload "org" "\
Reload all org lisp files.
With prefix arg UNCOMPILED, load the uncompiled versions.

\(fn &optional UNCOMPILED)" t nil)

(autoload 'org-customize "org" "\
Call the customize function with org as argument.

\(fn)" t nil)

;;;***

;;;### (autoloads (org-agenda-to-appt org-calendar-goto-agenda org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item
;;;;;;  org-diary org-agenda-list-stuck-projects org-tags-view org-todo-list
;;;;;;  org-search-view org-agenda-list org-batch-store-agenda-views
;;;;;;  org-store-agenda-views org-batch-agenda-csv org-batch-agenda
;;;;;;  org-agenda) "org-agenda" "org-agenda.el" (20415 31134))
;;; Generated autoloads from org-agenda.el

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
L     Create a timeline for the current buffer.
e     Export views to associated files.
s     Search entries for keywords.
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

If the current buffer is in Org-mode and visiting a file, you can also
first press `<' once to indicate that the agenda should be temporarily
\(until the next use of \\[org-agenda]) restricted to the current file.
Pressing `<' twice means to restrict to the current subtree or region
\(if active).

\(fn &optional ARG KEYS RESTRICTION)" t nil)

(autoload 'org-batch-agenda "org-agenda" "\
Run an agenda command in batch mode and send the result to STDOUT.
If CMD-KEY is a string of length 1, it is used as a key in
`org-agenda-custom-commands' and triggers this command.  If it is a
longer string it is used as a tags/todo match string.
Parameters are alternating variable names and values that will be bound
before running the agenda command.

\(fn CMD-KEY &rest PARAMETERS)" nil (quote macro))

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

\(fn CMD-KEY &rest PARAMETERS)" nil (quote macro))

(autoload 'org-store-agenda-views "org-agenda" "\
Not documented

\(fn &rest PARAMETERS)" t nil)

(autoload 'org-batch-store-agenda-views "org-agenda" "\
Run all custom agenda commands that have a file argument.

\(fn &rest PARAMETERS)" nil (quote macro))

(autoload 'org-agenda-list "org-agenda" "\
Produce a daily/weekly view from all files in variable `org-agenda-files'.
The view will be for the current day or week, but from the overview buffer
you will be able to go to other days/weeks.

With a numeric prefix argument in an interactive call, the agenda will
span ARG days.  Lisp programs should instead specify SPAN to change
the number of days.  SPAN defaults to `org-agenda-span'.

START-DAY defaults to TODAY, or to the most recent match for the weekday
given in `org-agenda-start-on-weekday'.

\(fn &optional ARG START-DAY SPAN)" t nil)

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

This command searches the agenda files, and in addition the files listed
in `org-agenda-text-search-extra-files'.

\(fn &optional TODO-ONLY STRING EDIT-AT)" t nil)

(autoload 'org-todo-list "org-agenda" "\
Show all (not done) TODO entries from all agenda file in a single list.
The prefix arg can be used to select a specific TODO keyword and limit
the list to these.  When using \\[universal-argument], you will be prompted
for a keyword.  A numeric prefix directly selects the Nth keyword in
`org-todo-keywords-1'.

\(fn ARG)" t nil)

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
Return diary information from org-files.
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

If you don't give any arguments (as in the example above), the default
arguments (:deadline :scheduled :timestamp :sexp) are used.
So the example above may also be written as

   &%%(org-diary :deadline :timestamp :sexp :scheduled)

The function expects the lisp variables `entry' and `date' to be provided
by the caller, because this is how the calendar works.  Don't use this
function from a program - use `org-agenda-get-day-entries' instead.

\(fn &rest ARGS)" nil nil)

(autoload 'org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item "org-agenda" "\
Do we have a reason to ignore this TODO entry because it has a time stamp?

\(fn &optional END)" nil nil)

(autoload 'org-calendar-goto-agenda "org-agenda" "\
Compute the Org-mode agenda for the calendar date displayed at the cursor.
This is a command that has to be installed in `calendar-mode-map'.

\(fn)" t nil)

(autoload 'org-agenda-to-appt "org-agenda" "\
Activate appointments found in `org-agenda-files'.
With a \\[universal-argument] prefix, refresh the list of
appointments.

If FILTER is t, interactively prompt the user for a regular
expression, and filter out entries that don't match it.

If FILTER is a string, use this string as a regular expression
for filtering entries out.

If FILTER is a function, filter out entries against which
calling the function returns nil.  This function takes one
argument: an entry from `org-agenda-get-day-entries'.

FILTER can also be an alist with the car of each cell being
either 'headline or 'category.  For example:

  '((headline \"IMPORTANT\")
    (category \"Work\"))

will only add headlines containing IMPORTANT or headlines
belonging to the \"Work\" category.

ARGS are symbols indicating what kind of entries to consider.
By default `org-agenda-to-appt' will use :deadline, :scheduled
and :timestamp entries.  See the docstring of `org-diary' for
details and examples.

\(fn &optional REFRESH FILTER &rest ARGS)" t nil)

;;;***

;;;### (autoloads (org-export-as-ascii org-export-region-as-ascii
;;;;;;  org-replace-region-by-ascii org-export-as-ascii-to-buffer
;;;;;;  org-export-as-utf8-to-buffer org-export-as-utf8 org-export-as-latin1-to-buffer
;;;;;;  org-export-as-latin1) "org-ascii" "org-ascii.el" (20415 31134))
;;; Generated autoloads from org-ascii.el

(autoload 'org-export-as-latin1 "org-ascii" "\
Like `org-export-as-ascii', use latin1 encoding for special symbols.

\(fn &rest ARGS)" t nil)

(autoload 'org-export-as-latin1-to-buffer "org-ascii" "\
Like `org-export-as-ascii-to-buffer', use latin1 encoding for symbols.

\(fn &rest ARGS)" t nil)

(autoload 'org-export-as-utf8 "org-ascii" "\
Like `org-export-as-ascii', use encoding for special symbols.

\(fn &rest ARGS)" t nil)

(autoload 'org-export-as-utf8-to-buffer "org-ascii" "\
Like `org-export-as-ascii-to-buffer', use utf8 encoding for symbols.

\(fn &rest ARGS)" t nil)

(autoload 'org-export-as-ascii-to-buffer "org-ascii" "\
Call `org-export-as-ascii` with output to a temporary buffer.
No file is created.  The prefix ARG is passed through to `org-export-as-ascii'.

\(fn ARG)" t nil)

(autoload 'org-replace-region-by-ascii "org-ascii" "\
Assume the current region has org-mode syntax, and convert it to plain ASCII.
This can be used in any buffer.  For example, you could write an
itemized list in org-mode syntax in a Mail buffer and then use this
command to convert it.

\(fn BEG END)" t nil)

(autoload 'org-export-region-as-ascii "org-ascii" "\
Convert region from BEG to END in org-mode buffer to plain ASCII.
If prefix arg BODY-ONLY is set, omit file header, footer, and table of
contents, and only produce the region of converted text, useful for
cut-and-paste operations.
If BUFFER is a buffer or a string, use/create that buffer as a target
of the converted ASCII.  If BUFFER is the symbol `string', return the
produced ASCII as a string and leave not buffer behind.  For example,
a Lisp program could call this function in the following way:

  (setq ascii (org-export-region-as-ascii beg end t 'string))

When called interactively, the output buffer is selected, and shown
in a window.  A non-interactive call will only return the buffer.

\(fn BEG END &optional BODY-ONLY BUFFER)" t nil)

(autoload 'org-export-as-ascii "org-ascii" "\
Export the outline as a pretty ASCII file.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
underlined headlines, default is 3.    Lower levels will become bulleted
lists.  When HIDDEN is non-nil, don't display the ASCII buffer.
EXT-PLIST is a property list with external parameters overriding
org-mode's default settings, but still inferior to file-local
settings.  When TO-BUFFER is non-nil, create a buffer with that
name and export to that buffer.  If TO-BUFFER is the symbol
`string', don't leave any buffer behind but just return the
resulting ASCII as a string.  When BODY-ONLY is set, don't produce
the file header and footer.  When PUB-DIR is set, use this as the
publishing directory.

\(fn ARG &optional HIDDEN EXT-PLIST TO-BUFFER BODY-ONLY PUB-DIR)" t nil)

;;;***

;;;### (autoloads (org-attach) "org-attach" "org-attach.el" (20415
;;;;;;  31135))
;;; Generated autoloads from org-attach.el

(autoload 'org-attach "org-attach" "\
The dispatcher for attachment commands.
Shows a list of commands and prompts for another key to execute a command.

\(fn)" t nil)

;;;***

;;;### (autoloads (org-archive-subtree-default-with-confirmation
;;;;;;  org-archive-subtree-default) "org-archive" "org-archive.el"
;;;;;;  (20357 24019))
;;; Generated autoloads from org-archive.el

(autoload 'org-archive-subtree-default "org-archive" "\
Archive the current subtree with the default command.
This command is set with the variable `org-archive-default-command'.

\(fn)" t nil)

(autoload 'org-archive-subtree-default-with-confirmation "org-archive" "\
Archive the current subtree with the default command.
This command is set with the variable `org-archive-default-command'.

\(fn)" t nil)

;;;***

;;;### (autoloads (org-bbdb-anniversaries) "org-bbdb" "org-bbdb.el"
;;;;;;  (20415 31135))
;;; Generated autoloads from org-bbdb.el

(autoload 'org-bbdb-anniversaries "org-bbdb" "\
Extract anniversaries from BBDB for display in the agenda.

\(fn)" nil nil)

;;;***

;;;### (autoloads (org-capture-import-remember-templates org-capture-insert-template-here
;;;;;;  org-capture) "org-capture" "org-capture.el" (20415 31135))
;;; Generated autoloads from org-capture.el

(autoload 'org-capture "org-capture" "\
Capture something.
\\<org-capture-mode-map>
This will let you select a template from `org-capture-templates', and then
file the newly captured information.  The text is immediately inserted
at the target location, and an indirect buffer is shown where you can
edit it.  Pressing \\[org-capture-finalize] brings you back to the previous state
of Emacs, so that you can continue your work.

When called interactively with a \\[universal-argument] prefix argument GOTO, don't capture
anything, just go to the file/headline where the selected template
stores its notes.  With a double prefix argument \\[universal-argument] \\[universal-argument], go to the last note
stored.

When called with a `C-0' (zero) prefix, insert a template at point.

Lisp programs can set KEYS to a string associated with a template in
`org-capture-templates'.  In this case, interactive selection will be
bypassed.

\(fn &optional GOTO KEYS)" t nil)

(autoload 'org-capture-insert-template-here "org-capture" "\
Not documented

\(fn)" nil nil)

(autoload 'org-capture-import-remember-templates "org-capture" "\
Set org-capture-templates to be similar to `org-remember-templates'.

\(fn)" t nil)

;;;***

;;;### (autoloads (org-clock-persistence-insinuate org-get-clocktable)
;;;;;;  "org-clock" "org-clock.el" (20415 31135))
;;; Generated autoloads from org-clock.el

(autoload 'org-get-clocktable "org-clock" "\
Get a formatted clocktable with parameters according to PROPS.
The table is created in a temporary buffer, fully formatted and
fontified, and then returned.

\(fn &rest PROPS)" nil nil)

(autoload 'org-clock-persistence-insinuate "org-clock" "\
Set up hooks for clock persistence.

\(fn)" nil nil)

;;;***

;;;### (autoloads (org-datetree-find-date-create) "org-datetree"
;;;;;;  "org-datetree.el" (20415 31135))
;;; Generated autoloads from org-datetree.el

(autoload 'org-datetree-find-date-create "org-datetree" "\
Find or create an entry for DATE.
If KEEP-RESTRICTION is non-nil, do not widen the buffer.
When it is nil, the buffer will be widened to make sure an existing date
tree can be found.

\(fn DATE &optional KEEP-RESTRICTION)" nil nil)

;;;***

;;;### (autoloads (org-insert-export-options-template org-export-as-org
;;;;;;  org-export-visible org-export) "org-exp" "org-exp.el" (20415
;;;;;;  31135))
;;; Generated autoloads from org-exp.el

(autoload 'org-export "org-exp" "\
Export dispatcher for Org-mode.
When `org-export-run-in-background' is non-nil, try to run the command
in the background.  This will be done only for commands that write
to a file.  For details see the docstring of `org-export-run-in-background'.

The prefix argument ARG will be passed to the exporter.  However, if
ARG is a double universal prefix \\[universal-argument] \\[universal-argument], that means to inverse the
value of `org-export-run-in-background'.

If `org-export-initial-scope' is set to 'subtree, try to export
the current subtree, otherwise try to export the whole buffer.
Pressing `1' will switch between these two options.

\(fn &optional ARG)" t nil)

(autoload 'org-export-visible "org-exp" "\
Create a copy of the visible part of the current buffer, and export it.
The copy is created in a temporary buffer and removed after use.
TYPE is the final key (as a string) that also selects the export command in
the \\<org-mode-map>\\[org-export] export dispatcher.
As a special case, if the you type SPC at the prompt, the temporary
org-mode file will not be removed but presented to you so that you can
continue to use it.  The prefix arg ARG is passed through to the exporting
command.

\(fn TYPE ARG)" t nil)

(autoload 'org-export-as-org "org-exp" "\
Make a copy with not-exporting stuff removed.
The purpose of this function is to provide a way to export the source
Org file of a webpage in Org format, but with sensitive and/or irrelevant
stuff removed.  This command will remove the following:

- archived trees (if the variable `org-export-with-archived-trees' is nil)
- comment blocks and trees starting with the COMMENT keyword
- only trees that are consistent with `org-export-select-tags'
  and `org-export-exclude-tags'.

The only arguments that will be used are EXT-PLIST and PUB-DIR,
all the others will be ignored (but are present so that the general
mechanism to call publishing functions will work).

EXT-PLIST is a property list with external parameters overriding
org-mode's default settings, but still inferior to file-local
settings.  When PUB-DIR is set, use this as the publishing
directory.

\(fn ARG &optional HIDDEN EXT-PLIST TO-BUFFER BODY-ONLY PUB-DIR)" t nil)

(autoload 'org-insert-export-options-template "org-exp" "\
Insert into the buffer a template with information for exporting.

\(fn)" t nil)

;;;***

;;;### (autoloads (org-export-as-docbook org-export-as-docbook-pdf-and-open
;;;;;;  org-export-as-docbook-pdf org-export-region-as-docbook org-replace-region-by-docbook
;;;;;;  org-export-as-docbook-to-buffer org-export-as-docbook-batch)
;;;;;;  "org-docbook" "org-docbook.el" (20415 31135))
;;; Generated autoloads from org-docbook.el

(autoload 'org-export-as-docbook-batch "org-docbook" "\
Call `org-export-as-docbook' in batch style.
This function can be used in batch processing.

For example:

$ emacs --batch
        --load=$HOME/lib/emacs/org.el
        --visit=MyOrgFile.org --funcall org-export-as-docbook-batch

\(fn)" nil nil)

(autoload 'org-export-as-docbook-to-buffer "org-docbook" "\
Call `org-export-as-docbook' with output to a temporary buffer.
No file is created.

\(fn)" t nil)

(autoload 'org-replace-region-by-docbook "org-docbook" "\
Replace the region from BEG to END with its DocBook export.
It assumes the region has `org-mode' syntax, and then convert it to
DocBook.  This can be used in any buffer.  For example, you could
write an itemized list in `org-mode' syntax in an DocBook buffer and
then use this command to convert it.

\(fn BEG END)" t nil)

(autoload 'org-export-region-as-docbook "org-docbook" "\
Convert region from BEG to END in `org-mode' buffer to DocBook.
If prefix arg BODY-ONLY is set, omit file header and footer and
only produce the region of converted text, useful for
cut-and-paste operations.  If BUFFER is a buffer or a string,
use/create that buffer as a target of the converted DocBook.  If
BUFFER is the symbol `string', return the produced DocBook as a
string and leave not buffer behind.  For example, a Lisp program
could call this function in the following way:

  (setq docbook (org-export-region-as-docbook beg end t 'string))

When called interactively, the output buffer is selected, and shown
in a window.  A non-interactive call will only return the buffer.

\(fn BEG END &optional BODY-ONLY BUFFER)" t nil)

(autoload 'org-export-as-docbook-pdf "org-docbook" "\
Export as DocBook XML file, and generate PDF file.

\(fn &optional HIDDEN EXT-PLIST TO-BUFFER BODY-ONLY PUB-DIR)" t nil)

(autoload 'org-export-as-docbook-pdf-and-open "org-docbook" "\
Export as DocBook XML file, generate PDF file, and open it.

\(fn)" t nil)

(autoload 'org-export-as-docbook "org-docbook" "\
Export the current buffer as a DocBook file.
If there is an active region, export only the region.  When
HIDDEN is obsolete and does nothing.  EXT-PLIST is a
property list with external parameters overriding org-mode's
default settings, but still inferior to file-local settings.
When TO-BUFFER is non-nil, create a buffer with that name and
export to that buffer.  If TO-BUFFER is the symbol `string',
don't leave any buffer behind but just return the resulting HTML
as a string.  When BODY-ONLY is set, don't produce the file
header and footer, simply return the content of the document (all
top-level sections).  When PUB-DIR is set, use this as the
publishing directory.

\(fn &optional HIDDEN EXT-PLIST TO-BUFFER BODY-ONLY PUB-DIR)" t nil)

;;;***

;;;### (autoloads (org-feed-show-raw-feed org-feed-goto-inbox org-feed-update
;;;;;;  org-feed-update-all) "org-feed" "org-feed.el" (20415 31135))
;;; Generated autoloads from org-feed.el

(autoload 'org-feed-update-all "org-feed" "\
Get inbox items from all feeds in `org-feed-alist'.

\(fn)" t nil)

(autoload 'org-feed-update "org-feed" "\
Get inbox items from FEED.
FEED can be a string with an association in `org-feed-alist', or
it can be a list structured like an entry in `org-feed-alist'.

\(fn FEED &optional RETRIEVE-ONLY)" t nil)

(autoload 'org-feed-goto-inbox "org-feed" "\
Go to the inbox that captures the feed named FEED.

\(fn FEED)" t nil)

(autoload 'org-feed-show-raw-feed "org-feed" "\
Show the raw feed buffer of a feed.

\(fn FEED)" t nil)

;;;***

;;;### (autoloads (org-footnote-normalize org-footnote-action) "org-footnote"
;;;;;;  "org-footnote.el" (20415 31135))
;;; Generated autoloads from org-footnote.el

(autoload 'org-footnote-action "org-footnote" "\
Do the right thing for footnotes.

When at a footnote reference, jump to the definition.

When at a definition, jump to the references if they exist, offer
to create them otherwise.

When neither at definition or reference, create a new footnote,
interactively.

With prefix arg SPECIAL, offer additional commands in a menu.

\(fn &optional SPECIAL)" t nil)

(autoload 'org-footnote-normalize "org-footnote" "\
Collect the footnotes in various formats and normalize them.

This finds the different sorts of footnotes allowed in Org, and
normalizes them to the usual [N] format that is understood by the
Org-mode exporters.

When SORT-ONLY is set, only sort the footnote definitions into the
referenced sequence.

If Org is amidst an export process, EXPORT-PROPS will hold the
export properties of the buffer.

When EXPORT-PROPS is non-nil, the default action is to insert
normalized footnotes towards the end of the pre-processing
buffer.  Some exporters (docbook, odt...) expect footnote
definitions to be available before any references to them.  Such
exporters can let bind `org-footnote-insert-pos-for-preprocessor'
to symbol `point-min' to achieve the desired behaviour.

Additional note on `org-footnote-insert-pos-for-preprocessor':
1. This variable has not effect when FOR-PREPROCESSOR is nil.
2. This variable (potentially) obviates the need for extra scan
   of pre-processor buffer as witnessed in
   `org-export-docbook-get-footnotes'.

\(fn &optional SORT-ONLY EXPORT-PROPS)" nil nil)

;;;***

;;;### (autoloads (org-freemind-to-org-mode org-freemind-from-org-sparse-tree
;;;;;;  org-freemind-from-org-mode org-freemind-from-org-mode-node
;;;;;;  org-freemind-show org-export-as-freemind) "org-freemind"
;;;;;;  "org-freemind.el" (20357 24019))
;;; Generated autoloads from org-freemind.el

(autoload 'org-export-as-freemind "org-freemind" "\
Export the current buffer as a Freemind file.
If there is an active region, export only the region.  HIDDEN is
obsolete and does nothing.  EXT-PLIST is a property list with
external parameters overriding org-mode's default settings, but
still inferior to file-local settings.  When TO-BUFFER is
non-nil, create a buffer with that name and export to that
buffer.  If TO-BUFFER is the symbol `string', don't leave any
buffer behind but just return the resulting HTML as a string.
When BODY-ONLY is set, don't produce the file header and footer,
simply return the content of the document (all top level
sections).  When PUB-DIR is set, use this as the publishing
directory.

See `org-freemind-from-org-mode' for more information.

\(fn &optional HIDDEN EXT-PLIST TO-BUFFER BODY-ONLY PUB-DIR)" t nil)

(autoload 'org-freemind-show "org-freemind" "\
Show file MM-FILE in Freemind.

\(fn MM-FILE)" t nil)

(autoload 'org-freemind-from-org-mode-node "org-freemind" "\
Convert node at line NODE-LINE to the FreeMind file MM-FILE.
See `org-freemind-from-org-mode' for more information.

\(fn NODE-LINE MM-FILE)" t nil)

(autoload 'org-freemind-from-org-mode "org-freemind" "\
Convert the `org-mode' file ORG-FILE to the FreeMind file MM-FILE.
All the nodes will be opened or closed in Freemind just as you
have them in `org-mode'.

Note that exporting to Freemind also gives you an alternative way
to export from `org-mode' to html.  You can create a dynamic html
version of the your org file, by first exporting to Freemind and
then exporting from Freemind to html.  The 'As
XHTML (JavaScript)' version in Freemind works very well (and you
can use a CSS stylesheet to style it).

\(fn ORG-FILE MM-FILE)" t nil)

(autoload 'org-freemind-from-org-sparse-tree "org-freemind" "\
Convert visible part of buffer ORG-BUFFER to FreeMind file MM-FILE.

\(fn ORG-BUFFER MM-FILE)" t nil)

(autoload 'org-freemind-to-org-mode "org-freemind" "\
Convert FreeMind file MM-FILE to `org-mode' file ORG-FILE.

\(fn MM-FILE ORG-FILE)" t nil)

;;;***

;;;### (autoloads (org-export-htmlize-generate-css org-export-as-html
;;;;;;  org-export-region-as-html org-replace-region-by-html org-export-as-html-to-buffer
;;;;;;  org-export-as-html-batch org-export-as-html-and-open) "org-html"
;;;;;;  "org-html.el" (20415 31135))
;;; Generated autoloads from org-html.el

(put 'org-export-html-style-include-default 'safe-local-variable 'booleanp)

(put 'org-export-html-style 'safe-local-variable 'stringp)

(put 'org-export-html-style-extra 'safe-local-variable 'stringp)

(autoload 'org-export-as-html-and-open "org-html" "\
Export the outline as HTML and immediately open it with a browser.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted lists.

\(fn ARG)" t nil)

(autoload 'org-export-as-html-batch "org-html" "\
Call the function `org-export-as-html'.
This function can be used in batch processing as:
emacs   --batch
        --load=$HOME/lib/emacs/org.el
        --eval \"(setq org-export-headline-levels 2)\"
        --visit=MyFile --funcall org-export-as-html-batch

\(fn)" nil nil)

(autoload 'org-export-as-html-to-buffer "org-html" "\
Call `org-export-as-html` with output to a temporary buffer.
No file is created.  The prefix ARG is passed through to `org-export-as-html'.

\(fn ARG)" t nil)

(autoload 'org-replace-region-by-html "org-html" "\
Assume the current region has org-mode syntax, and convert it to HTML.
This can be used in any buffer.  For example, you could write an
itemized list in org-mode syntax in an HTML buffer and then use this
command to convert it.

\(fn BEG END)" t nil)

(autoload 'org-export-region-as-html "org-html" "\
Convert region from BEG to END in org-mode buffer to HTML.
If prefix arg BODY-ONLY is set, omit file header, footer, and table of
contents, and only produce the region of converted text, useful for
cut-and-paste operations.
If BUFFER is a buffer or a string, use/create that buffer as a target
of the converted HTML.  If BUFFER is the symbol `string', return the
produced HTML as a string and leave not buffer behind.  For example,
a Lisp program could call this function in the following way:

  (setq html (org-export-region-as-html beg end t 'string))

When called interactively, the output buffer is selected, and shown
in a window.  A non-interactive call will only return the buffer.

\(fn BEG END &optional BODY-ONLY BUFFER)" t nil)

(autoload 'org-export-as-html "org-html" "\
Export the outline as a pretty HTML file.
If there is an active region, export only the region.  The prefix
ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted
lists.  HIDDEN is obsolete and does nothing.
EXT-PLIST is a property list with external parameters overriding
org-mode's default settings, but still inferior to file-local
settings.  When TO-BUFFER is non-nil, create a buffer with that
name and export to that buffer.  If TO-BUFFER is the symbol
`string', don't leave any buffer behind but just return the
resulting HTML as a string.  When BODY-ONLY is set, don't produce
the file header and footer, simply return the content of
<body>...</body>, without even the body tags themselves.  When
PUB-DIR is set, use this as the publishing directory.

\(fn ARG &optional HIDDEN EXT-PLIST TO-BUFFER BODY-ONLY PUB-DIR)" t nil)

(autoload 'org-export-htmlize-generate-css "org-html" "\
Create the CSS for all font definitions in the current Emacs session.
Use this to create face definitions in your CSS style file that can then
be used by code snippets transformed by htmlize.
This command just produces a buffer that contains class definitions for all
faces used in the current Emacs session.  You can copy and paste the ones you
need into your CSS file.

If you then set `org-export-htmlize-output-type' to `css', calls to
the function `org-export-htmlize-region-for-paste' will produce code
that uses these same face definitions.

\(fn)" t nil)

;;;***

;;;### (autoloads (org-export-icalendar-combine-agenda-files org-export-icalendar-all-agenda-files
;;;;;;  org-export-icalendar-this-file) "org-icalendar" "org-icalendar.el"
;;;;;;  (20357 24019))
;;; Generated autoloads from org-icalendar.el

(autoload 'org-export-icalendar-this-file "org-icalendar" "\
Export current file as an iCalendar file.
The iCalendar file will be located in the same directory as the Org-mode
file, but with extension `.ics'.

\(fn)" t nil)

(autoload 'org-export-icalendar-all-agenda-files "org-icalendar" "\
Export all files in the variable `org-agenda-files' to iCalendar .ics files.
Each iCalendar file will be located in the same directory as the Org-mode
file, but with extension `.ics'.

\(fn)" t nil)

(autoload 'org-export-icalendar-combine-agenda-files "org-icalendar" "\
Export all files in `org-agenda-files' to a single combined iCalendar file.
The file is stored under the name `org-combined-agenda-icalendar-file'.

\(fn)" t nil)

;;;***

;;;### (autoloads (org-id-store-link org-id-find-id-file org-id-find
;;;;;;  org-id-goto org-id-get-with-outline-drilling org-id-get-with-outline-path-completion
;;;;;;  org-id-get org-id-copy org-id-get-create) "org-id" "org-id.el"
;;;;;;  (20342 54224))
;;; Generated autoloads from org-id.el

(autoload 'org-id-get-create "org-id" "\
Create an ID for the current entry and return it.
If the entry already has an ID, just return it.
With optional argument FORCE, force the creation of a new ID.

\(fn &optional FORCE)" t nil)

(autoload 'org-id-copy "org-id" "\
Copy the ID of the entry at point to the kill ring.
Create an ID if necessary.

\(fn)" t nil)

(autoload 'org-id-get "org-id" "\
Get the ID property of the entry at point-or-marker POM.
If POM is nil, refer to the entry at point.
If the entry does not have an ID, the function returns nil.
However, when CREATE is non nil, create an ID if none is present already.
PREFIX will be passed through to `org-id-new'.
In any case, the ID of the entry is returned.

\(fn &optional POM CREATE PREFIX)" nil nil)

(autoload 'org-id-get-with-outline-path-completion "org-id" "\
Use outline-path-completion to retrieve the ID of an entry.
TARGETS may be a setting for `org-refile-targets' to define the eligible
headlines.  When omitted, all headlines in all agenda files are
eligible.
It returns the ID of the entry.  If necessary, the ID is created.

\(fn &optional TARGETS)" nil nil)

(autoload 'org-id-get-with-outline-drilling "org-id" "\
Use an outline-cycling interface to retrieve the ID of an entry.
This only finds entries in the current buffer, using `org-get-location'.
It returns the ID of the entry.  If necessary, the ID is created.

\(fn &optional TARGETS)" nil nil)

(autoload 'org-id-goto "org-id" "\
Switch to the buffer containing the entry with id ID.
Move the cursor to that entry in that buffer.

\(fn ID)" t nil)

(autoload 'org-id-find "org-id" "\
Return the location of the entry with the id ID.
The return value is a cons cell (file-name . position), or nil
if there is no entry with that ID.
With optional argument MARKERP, return the position as a new marker.

\(fn ID &optional MARKERP)" nil nil)

(autoload 'org-id-find-id-file "org-id" "\
Query the id database for the file in which this ID is located.

\(fn ID)" nil nil)

(autoload 'org-id-store-link "org-id" "\
Store a link to the current entry, using its ID.

\(fn)" t nil)

;;;***

;;;### (autoloads (org-indent-mode) "org-indent" "org-indent.el"
;;;;;;  (20415 31135))
;;; Generated autoloads from org-indent.el

(autoload 'org-indent-mode "org-indent" "\
When active, indent text according to outline structure.

Internally this works by adding `line-prefix' and `wrap-prefix'
properties, after each buffer modification, on the modified zone.

The process is synchronous.  Though, initial indentation of
buffer, which can take a few seconds on large buffers, is done
during idle time.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (org-irc-store-link) "org-irc" "org-irc.el" (20342
;;;;;;  54224))
;;; Generated autoloads from org-irc.el

(autoload 'org-irc-store-link "org-irc" "\
Dispatch to the appropriate function to store a link to an IRC session.

\(fn)" nil nil)

;;;***

;;;### (autoloads (org-export-as-pdf-and-open org-export-as-pdf org-export-as-latex
;;;;;;  org-export-region-as-latex org-replace-region-by-latex org-export-as-latex-to-buffer
;;;;;;  org-export-as-latex-batch) "org-latex" "org-latex.el" (20342
;;;;;;  54224))
;;; Generated autoloads from org-latex.el

(autoload 'org-export-as-latex-batch "org-latex" "\
Call `org-export-as-latex', may be used in batch processing.
For example:

emacs   --batch
        --load=$HOME/lib/emacs/org.el
        --eval \"(setq org-export-headline-levels 2)\"
        --visit=MyFile --funcall org-export-as-latex-batch

\(fn)" nil nil)

(autoload 'org-export-as-latex-to-buffer "org-latex" "\
Call `org-export-as-latex` with output to a temporary buffer.
No file is created.  The prefix ARG is passed through to `org-export-as-latex'.

\(fn ARG)" t nil)

(autoload 'org-replace-region-by-latex "org-latex" "\
Replace the region from BEG to END with its LaTeX export.
It assumes the region has `org-mode' syntax, and then convert it to
LaTeX.  This can be used in any buffer.  For example, you could
write an itemized list in `org-mode' syntax in an LaTeX buffer and
then use this command to convert it.

\(fn BEG END)" t nil)

(autoload 'org-export-region-as-latex "org-latex" "\
Convert region from BEG to END in `org-mode' buffer to LaTeX.
If prefix arg BODY-ONLY is set, omit file header, footer, and table of
contents, and only produce the region of converted text, useful for
cut-and-paste operations.
If BUFFER is a buffer or a string, use/create that buffer as a target
of the converted LaTeX.  If BUFFER is the symbol `string', return the
produced LaTeX as a string and leave no buffer behind.  For example,
a Lisp program could call this function in the following way:

  (setq latex (org-export-region-as-latex beg end t 'string))

When called interactively, the output buffer is selected, and shown
in a window.  A non-interactive call will only return the buffer.

\(fn BEG END &optional BODY-ONLY BUFFER)" t nil)

(autoload 'org-export-as-latex "org-latex" "\
Export current buffer to a LaTeX file.
If there is an active region, export only the region.  The prefix
ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will be exported
depending on `org-export-latex-low-levels'.  The default is to
convert them as description lists.
HIDDEN is obsolete and does nothing.
EXT-PLIST is a property list with
external parameters overriding org-mode's default settings, but
still inferior to file-local settings.  When TO-BUFFER is
non-nil, create a buffer with that name and export to that
buffer.  If TO-BUFFER is the symbol `string', don't leave any
buffer behind but just return the resulting LaTeX as a string.
When BODY-ONLY is set, don't produce the file header and footer,
simply return the content of \\begin{document}...\\end{document},
without even the \\begin{document} and \\end{document} commands.
when PUB-DIR is set, use this as the publishing directory.

\(fn ARG &optional HIDDEN EXT-PLIST TO-BUFFER BODY-ONLY PUB-DIR)" t nil)

(autoload 'org-export-as-pdf "org-latex" "\
Export as LaTeX, then process through to PDF.

\(fn ARG &optional HIDDEN EXT-PLIST TO-BUFFER BODY-ONLY PUB-DIR)" t nil)

(autoload 'org-export-as-pdf-and-open "org-latex" "\
Export as LaTeX, then process through to PDF, and open.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (org-lparse-region org-replace-region-by org-lparse-to-buffer
;;;;;;  org-lparse-batch org-lparse-and-open) "org-lparse" "org-lparse.el"
;;;;;;  (20415 31135))
;;; Generated autoloads from org-lparse.el

(autoload 'org-lparse-and-open "org-lparse" "\
Export outline to TARGET-BACKEND via NATIVE-BACKEND and open exported file.
If there is an active region, export only the region.  The prefix
ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted
lists.

\(fn TARGET-BACKEND NATIVE-BACKEND ARG &optional FILE-OR-BUF)" nil nil)

(autoload 'org-lparse-batch "org-lparse" "\
Call the function `org-lparse'.
This function can be used in batch processing as:
emacs   --batch
        --load=$HOME/lib/emacs/org.el
        --eval \"(setq org-export-headline-levels 2)\"
        --visit=MyFile --funcall org-lparse-batch

\(fn TARGET-BACKEND &optional NATIVE-BACKEND)" nil nil)

(autoload 'org-lparse-to-buffer "org-lparse" "\
Call `org-lparse' with output to a temporary buffer.
No file is created.  The prefix ARG is passed through to
`org-lparse'.

\(fn BACKEND ARG)" nil nil)

(autoload 'org-replace-region-by "org-lparse" "\
Assume the current region has org-mode syntax, and convert it to HTML.
This can be used in any buffer.  For example, you could write an
itemized list in org-mode syntax in an HTML buffer and then use
this command to convert it.

\(fn BACKEND BEG END)" nil nil)

(autoload 'org-lparse-region "org-lparse" "\
Convert region from BEG to END in org-mode buffer to HTML.
If prefix arg BODY-ONLY is set, omit file header, footer, and table of
contents, and only produce the region of converted text, useful for
cut-and-paste operations.
If BUFFER is a buffer or a string, use/create that buffer as a target
of the converted HTML.  If BUFFER is the symbol `string', return the
produced HTML as a string and leave not buffer behind.  For example,
a Lisp program could call this function in the following way:

  (setq html (org-lparse-region \"html\" beg end t 'string))

When called interactively, the output buffer is selected, and shown
in a window.  A non-interactive call will only return the buffer.

\(fn BACKEND BEG END &optional BODY-ONLY BUFFER)" nil nil)

;;;***

;;;### (autoloads (org-mobile-create-sumo-agenda org-mobile-pull
;;;;;;  org-mobile-push) "org-mobile" "org-mobile.el" (20415 31135))
;;; Generated autoloads from org-mobile.el

(autoload 'org-mobile-push "org-mobile" "\
Push the current state of Org affairs to the WebDAV directory.
This will create the index file, copy all agenda files there, and also
create all custom agenda views, for upload to the mobile phone.

\(fn)" t nil)

(autoload 'org-mobile-pull "org-mobile" "\
Pull the contents of `org-mobile-capture-file' and integrate them.
Apply all flagged actions, flag entries to be flagged and then call an
agenda view showing the flagged items.

\(fn)" t nil)

(autoload 'org-mobile-create-sumo-agenda "org-mobile" "\
Create a file that contains all custom agenda views.

\(fn)" t nil)

;;;***

;;;### (autoloads (org-export-as-odf-and-open org-export-as-odf org-export-odt-convert
;;;;;;  org-export-as-odt org-export-as-odt-batch org-export-as-odt-and-open)
;;;;;;  "org-odt" "org-odt.el" (20415 35769))
;;; Generated autoloads from org-odt.el

(autoload 'org-export-as-odt-and-open "org-odt" "\
Export the outline as ODT and immediately open it with a browser.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted lists.

\(fn ARG)" t nil)

(autoload 'org-export-as-odt-batch "org-odt" "\
Call the function `org-lparse-batch'.
This function can be used in batch processing as:
emacs   --batch
        --load=$HOME/lib/emacs/org.el
        --eval \"(setq org-export-headline-levels 2)\"
        --visit=MyFile --funcall org-export-as-odt-batch

\(fn)" nil nil)

(autoload 'org-export-as-odt "org-odt" "\
Export the outline as a OpenDocumentText file.
If there is an active region, export only the region.  The prefix
ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted
lists.  HIDDEN is obsolete and does nothing.
EXT-PLIST is a property list with external parameters overriding
org-mode's default settings, but still inferior to file-local
settings.  When TO-BUFFER is non-nil, create a buffer with that
name and export to that buffer.  If TO-BUFFER is the symbol
`string', don't leave any buffer behind but just return the
resulting XML as a string.  When BODY-ONLY is set, don't produce
the file header and footer, simply return the content of
<body>...</body>, without even the body tags themselves.  When
PUB-DIR is set, use this as the publishing directory.

\(fn ARG &optional HIDDEN EXT-PLIST TO-BUFFER BODY-ONLY PUB-DIR)" t nil)

(autoload 'org-export-odt-convert "org-odt" "\
Convert IN-FILE to format OUT-FMT using a command line converter.
IN-FILE is the file to be converted.  If unspecified, it defaults
to variable `buffer-file-name'.  OUT-FMT is the desired output
format.  Use `org-export-odt-convert-process' as the converter.
If PREFIX-ARG is non-nil then the newly converted file is opened
using `org-open-file'.

\(fn &optional IN-FILE OUT-FMT PREFIX-ARG)" t nil)

(autoload 'org-export-as-odf "org-odt" "\
Export LATEX-FRAG as OpenDocument formula file ODF-FILE.
Use `org-create-math-formula' to convert LATEX-FRAG first to
MathML.  When invoked as an interactive command, use
`org-latex-regexps' to infer LATEX-FRAG from currently active
region.  If no LaTeX fragments are found, prompt for it.  Push
MathML source to kill ring, if `org-export-copy-to-kill-ring' is
non-nil.

\(fn LATEX-FRAG &optional ODF-FILE)" t nil)

(autoload 'org-export-as-odf-and-open "org-odt" "\
Export LaTeX fragment as OpenDocument formula and immediately open it.
Use `org-export-as-odf' to read LaTeX fragment and OpenDocument
formula file.

\(fn)" t nil)

;;;***

;;;### (autoloads (org-publish-current-project org-publish-current-file
;;;;;;  org-publish-all org-publish) "org-publish" "org-publish.el"
;;;;;;  (20415 31135))
;;; Generated autoloads from org-publish.el

(defalias 'org-publish-project 'org-publish)

(autoload 'org-publish "org-publish" "\
Publish PROJECT.

\(fn PROJECT &optional FORCE)" t nil)

(autoload 'org-publish-all "org-publish" "\
Publish all projects.
With prefix argument, remove all files in the timestamp
directory and force publishing all files.

\(fn &optional FORCE)" t nil)

(autoload 'org-publish-current-file "org-publish" "\
Publish the current file.
With prefix argument, force publish the file.

\(fn &optional FORCE)" t nil)

(autoload 'org-publish-current-project "org-publish" "\
Publish the project associated with the current file.
With a prefix argument, force publishing of all files in
the project.

\(fn &optional FORCE)" t nil)

;;;***

;;;### (autoloads (org-plot/gnuplot) "org-plot" "org-plot.el" (20323
;;;;;;  36516))
;;; Generated autoloads from org-plot.el

(autoload 'org-plot/gnuplot "org-plot" "\
Plot table using gnuplot.  Gnuplot options can be specified with PARAMS.
If not given options will be taken from the +PLOT
line directly before or after the table.

\(fn &optional PARAMS)" t nil)

;;;***

;;;### (autoloads (org-remember-handler org-remember org-remember-apply-template
;;;;;;  org-remember-annotation org-remember-insinuate) "org-remember"
;;;;;;  "org-remember.el" (20342 54224))
;;; Generated autoloads from org-remember.el

(autoload 'org-remember-insinuate "org-remember" "\
Setup remember.el for use with Org-mode.

\(fn)" nil nil)

(autoload 'org-remember-annotation "org-remember" "\
Return a link to the current location as an annotation for remember.el.
If you are using Org-mode files as target for data storage with
remember.el, then the annotations should include a link compatible with the
conventions in Org-mode.  This function returns such a link.

\(fn)" nil nil)

(autoload 'org-remember-apply-template "org-remember" "\
Initialize *remember* buffer with template, invoke `org-mode'.
This function should be placed into `remember-mode-hook' and in fact requires
to be run from that hook to function properly.

\(fn &optional USE-CHAR SKIP-INTERACTIVE)" nil nil)

(autoload 'org-remember "org-remember" "\
Call `remember'.  If this is already a remember buffer, re-apply template.
If there is an active region, make sure remember uses it as initial content
of the remember buffer.

When called interactively with a \\[universal-argument] prefix argument GOTO, don't remember
anything, just go to the file/headline where the selected template usually
stores its notes.  With a double prefix argument \\[universal-argument] \\[universal-argument], go to the last
note stored by remember.

Lisp programs can set ORG-FORCE-REMEMBER-TEMPLATE-CHAR to a character
associated with a template in `org-remember-templates'.

\(fn &optional GOTO ORG-FORCE-REMEMBER-TEMPLATE-CHAR)" t nil)

(autoload 'org-remember-handler "org-remember" "\
Store stuff from remember.el into an org file.
When the template has specified a file and a headline, the entry is filed
there, or in the location defined by `org-default-notes-file' and
`org-remember-default-headline'.
\\<org-remember-mode-map>
If no defaults have been defined, or if the current prefix argument
is 1 (using C-1 \\[org-remember-finalize] to exit remember), an interactive
process is used to select the target location.

When the prefix is 0 (i.e. when remember is exited with C-0 \\[org-remember-finalize]),
the entry is filed to the same location as the previous note.

When the prefix is 2 (i.e. when remember is exited with C-2 \\[org-remember-finalize]),
the entry is filed as a subentry of the entry where the clock is
currently running.

When \\[universal-argument] has been used as prefix argument, the
note is stored and Emacs moves point to the new location of the
note, so that editing can be continued there (similar to
inserting \"%&\" into the template).

Before storing the note, the function ensures that the text has an
org-mode-style headline, i.e. a first line that starts with
a \"*\".  If not, a headline is constructed from the current date and
some additional data.

If the variable `org-adapt-indentation' is non-nil, the entire text is
also indented so that it starts in the same column as the headline
\(i.e. after the stars).

See also the variable `org-reverse-note-order'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (org-table-to-lisp orgtbl-mode turn-on-orgtbl)
;;;;;;  "org-table" "org-table.el" (20415 31135))
;;; Generated autoloads from org-table.el

(autoload 'turn-on-orgtbl "org-table" "\
Unconditionally turn on `orgtbl-mode'.

\(fn)" nil nil)

(autoload 'orgtbl-mode "org-table" "\
The `org-mode' table editor as a minor mode for use in other modes.

\(fn &optional ARG)" t nil)

(autoload 'org-table-to-lisp "org-table" "\
Convert the table at point to a Lisp structure.
The structure will be a list.  Each item is either the symbol `hline'
for a horizontal separator line, or a list of field values as strings.
The table is taken from the parameter TXT, or from the buffer at point.

\(fn &optional TXT)" nil nil)

;;;***

;;;### (autoloads (org-export-as-taskjuggler-and-open org-export-as-taskjuggler)
;;;;;;  "org-taskjuggler" "org-taskjuggler.el" (20357 24019))
;;; Generated autoloads from org-taskjuggler.el

(autoload 'org-export-as-taskjuggler "org-taskjuggler" "\
Export parts of the current buffer as a TaskJuggler file.
The exporter looks for a tree with tag, property or todo that
matches `org-export-taskjuggler-project-tag' and takes this as
the tasks for this project. The first node of this tree defines
the project properties such as project name and project period.
If there is a tree with tag, property or todo that matches
`org-export-taskjuggler-resource-tag' this three is taken as
resources for the project. If no resources are specified, a
default resource is created and allocated to the project. Also
the taskjuggler project will be created with default reports as
defined in `org-export-taskjuggler-default-reports'.

\(fn)" t nil)

(autoload 'org-export-as-taskjuggler-and-open "org-taskjuggler" "\
Export the current buffer as a TaskJuggler file and open it
with the TaskJuggler GUI.

\(fn)" t nil)

;;;***

;;;### (autoloads (org-timer-set-timer org-timer-item org-timer-change-times-in-region
;;;;;;  org-timer org-timer-start) "org-timer" "org-timer.el" (20357
;;;;;;  24019))
;;; Generated autoloads from org-timer.el

(autoload 'org-timer-start "org-timer" "\
Set the starting time for the relative timer to now.
When called with prefix argument OFFSET, prompt the user for an offset time,
with the default taken from a timer stamp at point, if any.
If OFFSET is a string or an integer, it is directly taken to be the offset
without user interaction.
When called with a double prefix arg, all timer strings in the active
region will be shifted by a specific amount.  You will be prompted for
the amount, with the default to make the first timer string in
the region 0:00:00.

\(fn &optional OFFSET)" t nil)

(autoload 'org-timer "org-timer" "\
Insert a H:MM:SS string from the timer into the buffer.
The first time this command is used, the timer is started.  When used with
a \\[universal-argument] prefix, force restarting the timer.
When used with a double prefix argument \\[universal-argument], change all the timer string
in the region by a fixed amount.  This can be used to recalibrate a timer
that was not started at the correct moment.

If NO-INSERT-P is non-nil, return the string instead of inserting
it in the buffer.

\(fn &optional RESTART NO-INSERT-P)" t nil)

(autoload 'org-timer-change-times-in-region "org-timer" "\
Change all h:mm:ss time in region by a DELTA.

\(fn BEG END DELTA)" t nil)

(autoload 'org-timer-item "org-timer" "\
Insert a description-type item with the current timer value.

\(fn &optional ARG)" t nil)

(autoload 'org-timer-set-timer "org-timer" "\
Prompt for a duration and set a timer.

If `org-timer-default-timer' is not zero, suggest this value as
the default duration for the timer.  If a timer is already set,
prompt the user if she wants to replace it.

Called with a numeric prefix argument, use this numeric value as
the duration of the timer.

Called with a `C-u' prefix arguments, use `org-timer-default-timer'
without prompting the user for a duration.

With two `C-u' prefix arguments, use `org-timer-default-timer'
without prompting the user for a duration and automatically
replace any running timer.

\(fn &optional OPT)" t nil)

;;;***

;;;### (autoloads (org-export-as-xoxo) "org-xoxo" "org-xoxo.el" (20415
;;;;;;  31135))
;;; Generated autoloads from org-xoxo.el

(autoload 'org-export-as-xoxo "org-xoxo" "\
Export the org buffer as XOXO.
The XOXO buffer is named *xoxo-<source buffer name>*

\(fn &optional BUFFER)" t nil)

;;;***

;;;### (autoloads (org-babel-mark-block org-babel-previous-src-block
;;;;;;  org-babel-next-src-block org-babel-goto-named-result org-babel-goto-named-src-block
;;;;;;  org-babel-goto-src-block-head org-babel-hide-result-toggle-maybe
;;;;;;  org-babel-sha1-hash org-babel-execute-subtree org-babel-execute-buffer
;;;;;;  org-babel-map-executables org-babel-map-call-lines org-babel-map-inline-src-blocks
;;;;;;  org-babel-map-src-blocks org-babel-open-src-block-result
;;;;;;  org-babel-switch-to-session-with-code org-babel-switch-to-session
;;;;;;  org-babel-initiate-session org-babel-load-in-session org-babel-insert-header-arg
;;;;;;  org-babel-check-src-block org-babel-expand-src-block org-babel-execute-src-block
;;;;;;  org-babel-pop-to-session-maybe org-babel-load-in-session-maybe
;;;;;;  org-babel-expand-src-block-maybe org-babel-view-src-block-info
;;;;;;  org-babel-execute-maybe org-babel-execute-safely-maybe) "ob"
;;;;;;  "ob.el" (20415 31134))
;;; Generated autoloads from ob.el

(autoload 'org-babel-execute-safely-maybe "ob" "\
Not documented

\(fn)" nil nil)

(autoload 'org-babel-execute-maybe "ob" "\
Not documented

\(fn)" t nil)

(autoload 'org-babel-view-src-block-info "ob" "\
Display information on the current source block.
This includes header arguments, language and name, and is largely
a window into the `org-babel-get-src-block-info' function.

\(fn)" t nil)

(autoload 'org-babel-expand-src-block-maybe "ob" "\
Conditionally expand a source block.
Detect if this is context for a org-babel src-block and if so
then run `org-babel-expand-src-block'.

\(fn)" t nil)

(autoload 'org-babel-load-in-session-maybe "ob" "\
Conditionally load a source block in a session.
Detect if this is context for a org-babel src-block and if so
then run `org-babel-load-in-session'.

\(fn)" t nil)

(autoload 'org-babel-pop-to-session-maybe "ob" "\
Conditionally pop to a session.
Detect if this is context for a org-babel src-block and if so
then run `org-babel-pop-to-session'.

\(fn)" t nil)

(autoload 'org-babel-execute-src-block "ob" "\
Execute the current source code block.
Insert the results of execution into the buffer.  Source code
execution and the collection and formatting of results can be
controlled through a variety of header arguments.

With prefix argument ARG, force re-execution even if an existing
result cached in the buffer would otherwise have been returned.

Optionally supply a value for INFO in the form returned by
`org-babel-get-src-block-info'.

Optionally supply a value for PARAMS which will be merged with
the header arguments specified at the front of the source code
block.

\(fn &optional ARG INFO PARAMS)" t nil)

(autoload 'org-babel-expand-src-block "ob" "\
Expand the current source code block.
Expand according to the source code block's header
arguments and pop open the results in a preview buffer.

\(fn &optional ARG INFO PARAMS)" t nil)

(autoload 'org-babel-check-src-block "ob" "\
Check for misspelled header arguments in the current code block.

\(fn)" t nil)

(autoload 'org-babel-insert-header-arg "ob" "\
Insert a header argument selecting from lists of common args and values.

\(fn)" t nil)

(autoload 'org-babel-load-in-session "ob" "\
Load the body of the current source-code block.
Evaluate the header arguments for the source block before
entering the session.  After loading the body this pops open the
session.

\(fn &optional ARG INFO)" t nil)

(autoload 'org-babel-initiate-session "ob" "\
Initiate session for current code block.
If called with a prefix argument then resolve any variable
references in the header arguments and assign these variables in
the session. Copy the body of the code block to the kill ring.

\(fn &optional ARG INFO)" t nil)

(autoload 'org-babel-switch-to-session "ob" "\
Switch to the session of the current code block.
Uses `org-babel-initiate-session' to start the session. If called
with a prefix argument then this is passed on to
`org-babel-initiate-session'.

\(fn &optional ARG INFO)" t nil)

(autoload 'org-babel-switch-to-session-with-code "ob" "\
Switch to code buffer and display session.

\(fn &optional ARG INFO)" t nil)

(autoload 'org-babel-open-src-block-result "ob" "\
If `point' is on a src block then open the results of the
source code block, otherwise return nil.  With optional prefix
argument RE-RUN the source-code block is evaluated even if
results already exist.

\(fn &optional RE-RUN)" t nil)

(autoload 'org-babel-map-src-blocks "ob" "\
Evaluate BODY forms on each source-block in FILE.
If FILE is nil evaluate BODY forms on source blocks in current
buffer.  During evaluation of BODY the following local variables
are set relative to the currently matched code block.

full-block ------- string holding the entirety of the code block
beg-block -------- point at the beginning of the code block
end-block -------- point at the end of the matched code block
lang ------------- string holding the language of the code block
beg-lang --------- point at the beginning of the lang
end-lang --------- point at the end of the lang
switches --------- string holding the switches
beg-switches ----- point at the beginning of the switches
end-switches ----- point at the end of the switches
header-args ------ string holding the header-args
beg-header-args -- point at the beginning of the header-args
end-header-args -- point at the end of the header-args
body ------------- string holding the body of the code block
beg-body --------- point at the beginning of the body
end-body --------- point at the end of the body

\(fn FILE &rest BODY)" nil (quote macro))

(autoload 'org-babel-map-inline-src-blocks "ob" "\
Evaluate BODY forms on each inline source-block in FILE.
If FILE is nil evaluate BODY forms on source blocks in current
buffer.

\(fn FILE &rest BODY)" nil (quote macro))

(autoload 'org-babel-map-call-lines "ob" "\
Evaluate BODY forms on each call line in FILE.
If FILE is nil evaluate BODY forms on source blocks in current
buffer.

\(fn FILE &rest BODY)" nil (quote macro))

(autoload 'org-babel-map-executables "ob" "\
Not documented

\(fn FILE &rest BODY)" nil (quote macro))

(autoload 'org-babel-execute-buffer "ob" "\
Execute source code blocks in a buffer.
Call `org-babel-execute-src-block' on every source block in
the current buffer.

\(fn &optional ARG)" t nil)

(autoload 'org-babel-execute-subtree "ob" "\
Execute source code blocks in a subtree.
Call `org-babel-execute-src-block' on every source block in
the current subtree.

\(fn &optional ARG)" t nil)

(autoload 'org-babel-sha1-hash "ob" "\
Generate an sha1 hash based on the value of info.

\(fn &optional INFO)" t nil)

(autoload 'org-babel-hide-result-toggle-maybe "ob" "\
Toggle visibility of result at point.

\(fn)" t nil)

(autoload 'org-babel-goto-src-block-head "ob" "\
Go to the beginning of the current code block.

\(fn)" t nil)

(autoload 'org-babel-goto-named-src-block "ob" "\
Go to a named source-code block.

\(fn NAME)" t nil)

(autoload 'org-babel-goto-named-result "ob" "\
Go to a named result.

\(fn NAME)" t nil)

(autoload 'org-babel-next-src-block "ob" "\
Jump to the next source block.
With optional prefix argument ARG, jump forward ARG many source blocks.

\(fn &optional ARG)" t nil)

(autoload 'org-babel-previous-src-block "ob" "\
Jump to the previous source block.
With optional prefix argument ARG, jump backward ARG many source blocks.

\(fn &optional ARG)" t nil)

(autoload 'org-babel-mark-block "ob" "\
Mark current src block

\(fn)" t nil)

;;;***

;;;### (autoloads (org-babel-lob-get-info org-babel-lob-execute-maybe
;;;;;;  org-babel-lob-ingest) "ob-lob" "ob-lob.el" (20415 31134))
;;; Generated autoloads from ob-lob.el

(autoload 'org-babel-lob-ingest "ob-lob" "\
Add all named source-blocks defined in FILE to
`org-babel-library-of-babel'.

\(fn &optional FILE)" t nil)

(autoload 'org-babel-lob-execute-maybe "ob-lob" "\
Execute a Library of Babel source block, if appropriate.
Detect if this is context for a Library Of Babel source block and
if so then run the appropriate source block from the Library.

\(fn)" t nil)

(autoload 'org-babel-lob-get-info "ob-lob" "\
Return a Library of Babel function call as a string.

\(fn)" nil nil)

;;;***

;;;### (autoloads (org-babel-tangle org-babel-tangle-file org-babel-load-file
;;;;;;  org-babel-tangle-lang-exts) "ob-tangle" "ob-tangle.el" (20415
;;;;;;  31134))
;;; Generated autoloads from ob-tangle.el

(defvar org-babel-tangle-lang-exts '(("emacs-lisp" . "el")) "\
Alist mapping languages to their file extensions.
The key is the language name, the value is the string that should
be inserted as the extension commonly used to identify files
written in this language.  If no entry is found in this list,
then the name of the language is used.")

(custom-autoload 'org-babel-tangle-lang-exts "ob-tangle" t)

(autoload 'org-babel-load-file "ob-tangle" "\
Load Emacs Lisp source code blocks in the Org-mode FILE.
This function exports the source code using
`org-babel-tangle' and then loads the resulting file using
`load-file'.

\(fn FILE)" t nil)

(autoload 'org-babel-tangle-file "ob-tangle" "\
Extract the bodies of source code blocks in FILE.
Source code blocks are extracted with `org-babel-tangle'.
Optional argument TARGET-FILE can be used to specify a default
export file for all source blocks.  Optional argument LANG can be
used to limit the exported source code blocks by language.

\(fn FILE &optional TARGET-FILE LANG)" t nil)

(autoload 'org-babel-tangle "ob-tangle" "\
Write code blocks to source-specific files.
Extract the bodies of all source code blocks from the current
file into their own source-specific files.  Optional argument
TARGET-FILE can be used to specify a default export file for all
source blocks.  Optional argument LANG can be used to limit the
exported source code blocks by language.

\(fn &optional ONLY-THIS-BLOCK TARGET-FILE LANG)" t nil)

;;;***

;;;### (autoloads (org-babel-describe-bindings) "ob-keys" "ob-keys.el"
;;;;;;  (20323 36516))
;;; Generated autoloads from ob-keys.el

(autoload 'org-babel-describe-bindings "ob-keys" "\
Describe all keybindings behind `org-babel-key-prefix'.

\(fn)" t nil)

;;;***

(provide (quote org-install))
