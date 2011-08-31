;;; semantic-loaddefs.el --- Auto-generated CEDET autoloads
;;
;;; Code:


;;;### (autoloads (bison->wisent) "bison-wisent" "wisent/bison-wisent.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from wisent/bison-wisent.el

(autoload 'bison->wisent "bison-wisent" "\
Treat the current buffer as a YACC or BISON file, and translate to wisent.
Replaces all comments with wisent compatible comments.
Finds % commands that wisent cannot handle, and comments them out.
Deletes all actions, replacing them with small comments.

\(fn)" t nil)

;;;***

;;;### (autoloads (semantic-bovine-debug-create-frame) "bovine-debug"
;;;;;;  "bovine/bovine-debug.el" (18857 63844))
;;; Generated autoloads from bovine/bovine-debug.el

(autoload 'semantic-bovine-debug-create-frame "bovine-debug" "\
Create one bovine frame.
NONTERM is the name of a rule we are currently parsing.
RULE is the index into the list of rules in NONTERM.
MATCH is the index into the list of matches in RULE.
For example:
  this: that
      | other thing
      | here
      ;
The NONTERM is THIS.
The RULE is for \"thing\" is 1.
The MATCH for \"thing\" is 1.
COLLECTION is a list of `things' that have been matched so far.
LEXTOKEN, is a token returned by the lexer which is being matched.

\(fn NONTERM RULE MATCH COLLECTION LEXTOKEN)" nil nil)

(eieio-defclass-autoload 'semantic-bovine-debug-parser '(semantic-debug-parser) "bovine-debug" "Represents a parser and its state.")

;;;***

;;;### (autoloads (bovine-grammar-mode) "bovine-grammar" "bovine/bovine-grammar.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from bovine/bovine-grammar.el

(autoload 'bovine-grammar-mode "bovine-grammar" "\
Major mode for editing Bovine grammars.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.by$" . bovine-grammar-mode))

(eval-after-load "speedbar" '(speedbar-add-supported-extension ".by"))

;;;***

;;;### (autoloads (semantic-bovinate-toplevel semantic-refresh-tags-safe
;;;;;;  semantic-fetch-tags semantic-parse-region-default) "semantic"
;;;;;;  "semantic.el" (18857 63844))
;;; Generated autoloads from semantic.el

(autoload 'semantic-parse-region-default "semantic" "\
Parse the area between START and END, and return any tags found.
If END needs to be extended due to a lexical token being too large, it
will be silently ignored.
Optional arguments:
NONTERMINAL is the rule to start parsing at if it is known.
DEPTH specifies the lexical depth to scan.
RETURNONERROR specifies that parsing should end when encountering
unterminated syntax.

\(fn START END &optional NONTERMINAL DEPTH RETURNONERROR)" nil nil)

(autoload 'semantic-fetch-tags "semantic" "\
Fetch semantic tags from the current buffer.
If the buffer cache is up to date, return that.
If the buffer cache is out of date, attempt an incremental reparse.
If the buffer has not been parsed before, or if the incremental reparse
fails, then parse the entire buffer.
If a lexcial error had been previously discovered and the buffer
was marked unparseable, then do nothing, and return the cache.

\(fn)" nil nil)

(autoload 'semantic-refresh-tags-safe "semantic" "\
Refreshes the current buffer's tags safely.

Return non-nil if the refresh was successful.
Return nil if there is some sort of syntax error preventing a reparse.

Does nothing if the current buffer doesn't need reparsing.

\(fn)" nil nil)

(autoload 'semantic-bovinate-toplevel "semantic" "\
Backward Compatibility Function.

\(fn &optional IGNORED)" nil nil)

(make-obsolete 'semantic-bovinate-toplevel 'semantic-fetch-tags)

(defsubst semantic-fetch-available-tags nil "\
Fetch available semantic tags from the current buffer.
That is, return tags currently in the cache without parsing the
current buffer.
Parse operations happen asynchronously when needed on Emacs idle time.
Use the `semantic-after-toplevel-cache-change-hook' and
`semantic-after-partial-cache-change-hook' hooks to synchronize with
new tags when they become available." semantic--buffer-cache)

;;;***

;;;### (autoloads (semanticdb-debug-file-tag-check semantic-adebug-edebug-expr
;;;;;;  semantic-adebug-analyze semantic-adebug-searchdb semantic-adebug-bovinate
;;;;;;  data-debug-insert-db-and-tag-button data-debug-insert-find-results-button
;;;;;;  data-debug-insert-find-results data-debug-insert-tag-list-button
;;;;;;  data-debug-insert-tag-list data-debug-insert-tag data-debug-insert-tag-parts-from-point)
;;;;;;  "semantic-adebug" "semantic-adebug.el" (18857 63844))
;;; Generated autoloads from semantic-adebug.el

(autoload 'data-debug-insert-tag-parts-from-point "semantic-adebug" "\
Call `data-debug-insert-tag-parts' based on text properties at POINT.

\(fn POINT)" nil nil)

(autoload 'data-debug-insert-tag "semantic-adebug" "\
Insert TAG into the current buffer at the current point.
PREFIX specifies text to insert in front of TAG.
PREBUTTONTEXT is text appearing btewen the prefix and TAG.
Optional PARENT is the parent tag containing TAG.
Add text properties needed to allow tag expansion later.

\(fn TAG PREFIX PREBUTTONTEXT &optional PARENT)" nil nil)

(autoload 'data-debug-insert-tag-list "semantic-adebug" "\
Insert the tag list TAGLIST with PREFIX.
Optional argument PARENT specifies the part of TAGLIST.

\(fn TAGLIST PREFIX &optional PARENT)" nil nil)

(autoload 'data-debug-insert-tag-list-button "semantic-adebug" "\
Insert a single summary of a TAGLIST.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between PREFIX and the taglist button.
PARENT is the tag that represents the parent of all the tags.

\(fn TAGLIST PREFIX PREBUTTONTEXT &optional PARENT)" nil nil)

(autoload 'data-debug-insert-find-results "semantic-adebug" "\
Insert the find results FINDRES with PREFIX.

\(fn FINDRES PREFIX)" nil nil)

(autoload 'data-debug-insert-find-results-button "semantic-adebug" "\
Insert a single summary of a find results FINDRES.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between prefix and the find results button.

\(fn FINDRES PREFIX PREBUTTONTEXT)" nil nil)

(autoload 'data-debug-insert-db-and-tag-button "semantic-adebug" "\
Insert a single summary of short list DBTAG of format (DB . TAG).
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between prefix and the find results button.

\(fn DBTAG PREFIX PREBUTTONTEXT)" nil nil)

(autoload 'semantic-adebug-bovinate "semantic-adebug" "\
The same as `bovinate'.  Display the results in a debug buffer.

\(fn)" t nil)

(autoload 'semantic-adebug-searchdb "semantic-adebug" "\
Search the semanticdb for REGEX for the current buffer.
Display the results as a debug list.

\(fn REGEX)" t nil)

(autoload 'semantic-adebug-analyze "semantic-adebug" "\
Perform `semantic-analyze-current-context'.
Display the results as a debug list.
Optional argument CTXT is the context to show.

\(fn &optional CTXT)" t nil)

(autoload 'semantic-adebug-edebug-expr "semantic-adebug" "\
Dump out the contets of some expression EXPR in edebug with adebug.

\(fn EXPR)" t nil)

(autoload 'semanticdb-debug-file-tag-check "semantic-adebug" "\
Report debug info for checking STARTFILE for up-to-date tags.

\(fn STARTFILE)" t nil)

;;;***

;;;### (autoloads (semantic-analyze-current-context) "semantic-analyze"
;;;;;;  "semantic-analyze.el" (18857 63844))
;;; Generated autoloads from semantic-analyze.el

(autoload 'semantic-analyze-current-context "semantic-analyze" "\
Analyze the current context at optional POSITION.
If called interactively, display interesting information about POSITION
in a separate buffer.
Returns an object based on symbol `semantic-analyze-context'.

This function can be overriden with the symbol `analyze-context'.
When overriding this function, your override will be called while
cursor is at POSITION.  In addition, your function will not be called
if a cached copy of the return object is found.

\(fn &optional POSITION)" t nil)

;;;***

;;;### (autoloads (semantic-analyze-possible-completions semantic-analyze-tags-of-class-list)
;;;;;;  "semantic-analyze-complete" "semantic-analyze-complete.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from semantic-analyze-complete.el

(autoload 'semantic-analyze-tags-of-class-list "semantic-analyze-complete" "\
Return the tags in TAGS that are of classes in CLASSLIST.

\(fn TAGS CLASSLIST)" nil nil)

(autoload 'semantic-analyze-possible-completions "semantic-analyze-complete" "\
Return a list of semantic tags which are possible completions.
CONTEXT is either a position (such as point), or a precalculated
context.  Passing in a context is useful if the caller also needs
to access parts of the analysis.
Completions run through the following filters:
  * Elements currently in scope
  * Constants currently in scope
  * Elements match the :prefix in the CONTEXT.
  * Type of the completion matches the type of the context.
Context type matching can identify the following:
  * No specific type
  * Assignment into a variable of some type.
  * Argument to a function with type constraints.
When called interactively, displays the list of possible completions
in a buffer.

\(fn CONTEXT)" t nil)

;;;***

;;;### (autoloads (semantic-analyze-debug-assist) "semantic-analyze-debug"
;;;;;;  "semantic-analyze-debug.el" (18857 63844))
;;; Generated autoloads from semantic-analyze-debug.el

(autoload 'semantic-analyze-debug-assist "semantic-analyze-debug" "\
Debug semantic analysis at the current point.

\(fn)" t nil)

;;;***

;;;### (autoloads (semantic-analyze-proto-impl-toggle semantic-analyze-current-tag
;;;;;;  semantic-analyze-tag-references) "semantic-analyze-refs"
;;;;;;  "semantic-analyze-refs.el" (18857 63844))
;;; Generated autoloads from semantic-analyze-refs.el

(autoload 'semantic-analyze-tag-references "semantic-analyze-refs" "\
Analyze the references for TAG.
Returns a class with information about TAG.

Optional argument DB is a database.  It will be used to help
locate TAG.

Use `semantic-analyze-current-tag' to debug this fcn.

\(fn TAG &optional DB)" nil nil)

(autoload 'semantic-analyze-current-tag "semantic-analyze-refs" "\
Analyze the tag under point.

\(fn)" t nil)

(autoload 'semantic-analyze-proto-impl-toggle "semantic-analyze-refs" "\
Toggle between the implementation, and a prototype of tag under point.

\(fn)" t nil)

;;;***

;;;### (autoloads (semantic-bovinate-stream semantic-lambda) "semantic-bovine"
;;;;;;  "bovine/semantic-bovine.el" (18857 63844))
;;; Generated autoloads from bovine/semantic-bovine.el

(defvar semantic-bovinate-nonterminal-check-obarray nil "\
Obarray of streams already parsed for nonterminal symbols.
Use this to detect infinite recursion during a parse.")

(autoload 'semantic-lambda "semantic-bovine" "\
Create a lambda expression to return a list including RETURN-VAL.
The return list is a lambda expression to be used in a bovine table.

\(fn &rest RETURN-VAL)" nil (quote macro))

(autoload 'semantic-bovinate-stream "semantic-bovine" "\
Bovinate STREAM, starting at the first NONTERMINAL rule.
Use `bovine-toplevel' if NONTERMINAL is not provided.
This is the core routine for converting a stream into a table.
Return the list (STREAM SEMANTIC-STREAM) where STREAM are those
elements of STREAM that have not been used.  SEMANTIC-STREAM is the
list of semantic tokens found.

\(fn STREAM &optional NONTERMINAL)" nil nil)

(defalias 'semantic-parse-stream-default 'semantic-bovinate-stream)

;;;***

;;;### (autoloads (semantic-c-add-preprocessor-symbol semantic-default-c-setup
;;;;;;  semantic-c-member-of-autocast semantic-lex-c-preprocessor-symbol-file
;;;;;;  semantic-lex-c-preprocessor-symbol-map) "semantic-c" "bovine/semantic-c.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from bovine/semantic-c.el

(defvar semantic-lex-c-preprocessor-symbol-map nil "\
Table of C Preprocessor keywords used by the Semantic C lexer.
Each entry is a cons cell like this:
  ( \"KEYWORD\" . \"REPLACEMENT\" )
Where KEYWORD is the macro that gets replaced in the lexical phase,
and REPLACEMENT is a string that is inserted in it's place.  Empty string
implies that the lexical analyzer will discard KEYWORD when it is encountered.

Alternately, it can be of the form:
  ( \"KEYWORD\" ( LEXSYM1 \"str\" 1 1 ) ... ( LEXSYMN \"str\" 1 1 ) )
where LEXSYM is a symbol that would normally be produced by the
lexical analyzer, such as `symbol' or `string'.  The string in the
second position is the text that makes up the replacement.  This is
the way to have multiple lexical symbols in a replacement.  Using the
first way to specify text like \"foo::bar\" would not work, because :
is a sepearate lexical symbol.

A quick way to see what you would need to insert is to place a
definition such as:

#define MYSYM foo::bar

into a C file, and do this:
  \\[semantic-lex-spp-describe]

The output table will describe the symbols needed.")

(custom-autoload 'semantic-lex-c-preprocessor-symbol-map "semantic-c" nil)

(defvar semantic-lex-c-preprocessor-symbol-file nil "\
List of C/C++ files that contain preprocessor macros for the C lexer.
Each entry is a filename and each file is parsed, and those macros
are included in every C/C++ file parsed by semantic.
You can use this variable instead of `semantic-lex-c-preprocessor-symbol-map'
to store your global macros in a more natural way.")

(custom-autoload 'semantic-lex-c-preprocessor-symbol-file "semantic-c" nil)

(defvar semantic-c-member-of-autocast 't "\
Non-nil means classes with a '->' operator will cast to it's return type.

For Examples:

  class Foo {
    Bar *operator->();
  }

  Foo foo;

if `semantic-c-member-of-autocast' is non-nil :
  foo->[here completion will list method of Bar]

if `semantic-c-member-of-autocast' is nil :
  foo->[here completion will list method of Foo]")

(custom-autoload 'semantic-c-member-of-autocast "semantic-c" t)

(autoload 'semantic-default-c-setup "semantic-c" "\
Set up a buffer for semantic parsing of the C language.

\(fn)" nil nil)

(autoload 'semantic-c-add-preprocessor-symbol "semantic-c" "\
Add a preprocessor symbol SYM with a REPLACEMENT value.

\(fn SYM REPLACEMENT)" t nil)

(add-hook 'c-mode-hook 'semantic-default-c-setup)

(add-hook 'c++-mode-hook 'semantic-default-c-setup)

;;;***

;;;### (autoloads (semantic-chart-analyzer semantic-chart-tag-complexity
;;;;;;  semantic-chart-database-size semantic-chart-tags-by-class)
;;;;;;  "semantic-chart" "semantic-chart.el" (18857 63844))
;;; Generated autoloads from semantic-chart.el

(autoload 'semantic-chart-tags-by-class "semantic-chart" "\
Create a bar chart representing the number of tags for a given tag class.
Each bar represents how many toplevel tags in TAGTABLE
exist with a given class.  See `semantic-symbol->name-assoc-list'
for tokens which will be charted.
TAGTABLE is passedto `semantic-something-to-tag-table'.

\(fn &optional TAGTABLE)" t nil)

(autoload 'semantic-chart-database-size "semantic-chart" "\
Create a bar chart representing the size of each file in semanticdb.
Each bar represents how many toplevel tags in TAGTABLE
exist in each database entry.
TAGTABLE is passed to `semantic-something-to-tag-table'.

\(fn &optional TAGTABLE)" t nil)

(autoload 'semantic-chart-tag-complexity "semantic-chart" "\
Create a bar chart representing the complexity of some tags.
Complexity is calculated for tags of CLASS.  Each bar represents
the complexity of some tag in TAGTABLE.  Only the most complex
items are charted.  TAGTABLE is passedto
`semantic-something-to-tag-table'.

\(fn &optional CLASS TAGTABLE)" t nil)

(autoload 'semantic-chart-analyzer "semantic-chart" "\
Chart the extent of the context analysis.

\(fn)" t nil)

;;;***

;;;### (autoloads (semantic-complete-inline-project semantic-complete-self-insert
;;;;;;  semantic-complete-analyze-inline-idle semantic-complete-analyze-inline
;;;;;;  semantic-complete-analyze-and-replace semantic-complete-jump
;;;;;;  semantic-complete-jump-local semantic-complete-inline-analyzer-idle
;;;;;;  semantic-complete-inline-analyzer semantic-complete-read-tag-analyzer
;;;;;;  semantic-complete-inline-tag-project semantic-complete-read-tag-project
;;;;;;  semantic-complete-read-tag-buffer-deep semantic-complete-inline-force-display
;;;;;;  semantic-complete-read-tag-engine semantic-completion-inline-active-p)
;;;;;;  "semantic-complete" "semantic-complete.el" (18857 63844))
;;; Generated autoloads from semantic-complete.el

(autoload 'semantic-completion-inline-active-p "semantic-complete" "\
Non-nil if inline completion is active.

\(fn)" nil nil)

(autoload 'semantic-complete-read-tag-engine "semantic-complete" "\
Read a semantic tag, and return a tag for the selection.
Argument COLLECTOR is an object which can be used to to calculate
a list of possible hits.  See `semantic-completion-collector-engine'
for details on COLLECTOR.
Argumeng DISPLAYOR is an object used to display a list of possible
completions for a given prefix.  See`semantic-completion-display-engine'
for details on DISPLAYOR.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to story the history in.

\(fn COLLECTOR DISPLAYOR PROMPT DEFAULT-TAG INITIAL-INPUT HISTORY)" nil nil)

(autoload 'semantic-complete-inline-force-display "semantic-complete" "\
Force the display of whatever the current completions are.
DO NOT CALL THIS IF THE INLINE COMPLETION ENGINE IS NOT ACTIVE.

\(fn)" nil nil)

(autoload 'semantic-complete-read-tag-buffer-deep "semantic-complete" "\
Ask for a tag by name from the current buffer.
Available tags are from the current buffer, at any level.
Completion options are presented in a traditional way, with highlighting
to resolve same-name collisions.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to store the history in.

\(fn PROMPT &optional DEFAULT-TAG INITIAL-INPUT HISTORY)" nil nil)

(autoload 'semantic-complete-read-tag-project "semantic-complete" "\
Ask for a tag by name from the current project.
Available tags are from the current project, at the top level.
Completion options are presented in a traditional way, with highlighting
to resolve same-name collisions.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to store the history in.

\(fn PROMPT &optional DEFAULT-TAG INITIAL-INPUT HISTORY)" nil nil)

(autoload 'semantic-complete-inline-tag-project "semantic-complete" "\
Complete a symbol name by name from within the current project.
This is similar to `semantic-complete-read-tag-project', except
that the completion interaction is in the buffer where the context
was calculated from.
Customize `semantic-complete-inline-analyzer-displayor-class'
to control how completion options are displayed.
See `semantic-complete-inline-tag-engine' for details on how
completion works.

\(fn)" nil nil)

(autoload 'semantic-complete-read-tag-analyzer "semantic-complete" "\
Ask for a tag by name based on the current context.
The function `semantic-analyze-current-context' is used to
calculate the context.  `semantic-analyze-possible-completions' is used 
to generate the list of possible completions.
PROMPT is the first part of the prompt.  Additional prompt
is added based on the contexts full prefix.
CONTEXT is the semantic analyzer context to start with.
HISTORY is a symbol representing a variable to stor the history in.
usually a default-tag and initial-input are available for completion
prompts.  these are calculated from the CONTEXT variable passed in.

\(fn PROMPT &optional CONTEXT HISTORY)" nil nil)

(autoload 'semantic-complete-inline-analyzer "semantic-complete" "\
Complete a symbol name by name based on the current context.
This is similar to `semantic-complete-read-tag-analyze', except
that the completion interaction is in the buffer where the context
was calculated from.
CONTEXT is the semantic analyzer context to start with.
Customize `semantic-complete-inline-analyzer-displayor-class'
to control how completion options are displayed.

See `semantic-complete-inline-tag-engine' for details on how
completion works.

\(fn CONTEXT)" nil nil)

(autoload 'semantic-complete-inline-analyzer-idle "semantic-complete" "\
Complete a symbol name by name based on the current context for idle time.
CONTEXT is the semantic analyzer context to start with.
This function is used from `semantic-idle-completions-mode'.

This is the same as `semantic-complete-inline-analyzer', except that
it uses `semantic-complete-inline-analyzer-idle-displayor-class'
to control how completions are displayed.

See `semantic-complete-inline-tag-engine' for details on how
completion works.

\(fn CONTEXT)" nil nil)

(autoload 'semantic-complete-jump-local "semantic-complete" "\
Jump to a semantic symbol.

\(fn)" t nil)

(autoload 'semantic-complete-jump "semantic-complete" "\
Jump to a semantic symbol.

\(fn)" t nil)

(autoload 'semantic-complete-analyze-and-replace "semantic-complete" "\
Perform prompt completion to do in buffer completion.
`semantic-analyze-possible-completions' is used to determine the
possible values.
The minibuffer is used to perform the completion.
The result is inserted as a replacement of the text that was there.

\(fn)" t nil)

(autoload 'semantic-complete-analyze-inline "semantic-complete" "\
Perform prompt completion to do in buffer completion.
`semantic-analyze-possible-completions' is used to determine the
possible values.
The function returns immediately, leaving the buffer in a mode that
will perform the completion.
Configure `semantic-complete-inline-analyzer-displayor-class' to change
how completion options are displayed.

\(fn)" t nil)

(autoload 'semantic-complete-analyze-inline-idle "semantic-complete" "\
Perform prompt completion to do in buffer completion.
`semantic-analyze-possible-completions' is used to determine the
possible values.
The function returns immediately, leaving the buffer in a mode that
will perform the completion.
Configure `semantic-complete-inline-analyzer-idle-displayor-class'
to change how completion options are displayed.

\(fn)" t nil)

(autoload 'semantic-complete-self-insert "semantic-complete" "\
Like `self-insert-command', but does completion afterwards.
ARG is passed to `self-insert-command'.  If ARG is nil,
use `semantic-complete-analyze-inline' to complete.

\(fn ARG)" t nil)

(autoload 'semantic-complete-inline-project "semantic-complete" "\
Perform inline completion for any symbol in the current project.
`semantic-analyze-possible-completions' is used to determine the
possible values.
The function returns immediately, leaving the buffer in a mode that
will perform the completion.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "semantic-ctxt" "semantic-ctxt.el" (18857 63844))
;;; Generated autoloads from semantic-ctxt.el

(defvar semantic-command-separation-character ";" "\
String which indicates the end of a command.
Used for identifying the end of a single command.")

;;;***

;;;### (autoloads (semantic-debug semantic-debug-break) "semantic-debug"
;;;;;;  "semantic-debug.el" (18857 63844))
;;; Generated autoloads from semantic-debug.el

(defvar semantic-debug-parser-source nil "\
For any buffer, the file name (no path) of the parser.
This would be a parser for a specific language, not the source
to one of the parser generators.")

(make-variable-buffer-local 'semantic-debug-parser-source)

(defvar semantic-debug-parser-class nil "\
Class to create when building a debug parser object.")

(make-variable-buffer-local 'semantic-debug-parser-class)

(defvar semantic-debug-enabled nil "\
Non-nil when debugging a parser.")

(autoload 'semantic-debug-break "semantic-debug" "\
Stop parsing now at FRAME.
FRAME is an object that represents the parser's view of the
current state of the world.
This function enters a recursive edit.  It returns
on an `exit-recursive-edit', or if someone uses one
of the `semantic-debug-mode' commands.
It returns the command specified.  Parsers need to take action
on different types of return values.

\(fn FRAME)" nil nil)

(autoload 'semantic-debug "semantic-debug" "\
Parse the current buffer and run in debug mode.

\(fn)" t nil)

;;;***

;;;### (autoloads (semantic-tag-folded-p semantic-set-tag-folded
;;;;;;  semantic-tag-delete-secondary-overlay semantic-tag-get-secondary-overlay
;;;;;;  semantic-tag-create-secondary-overlay semantic-tag-secondary-overlays
;;;;;;  semantic-tag-read-only-p semantic-set-tag-read-only semantic-tag-intangible-p
;;;;;;  semantic-set-tag-intangible semantic-tag-invisible-p semantic-set-tag-invisible
;;;;;;  semantic-set-tag-face semantic-momentary-highlight-tag semantic-momentary-highlight-one-tag-line
;;;;;;  semantic-unhighlight-tag semantic-highlight-tag) "semantic-decorate"
;;;;;;  "semantic-decorate.el" (18857 63844))
;;; Generated autoloads from semantic-decorate.el

(autoload 'semantic-highlight-tag "semantic-decorate" "\
Specify that TAG should be highlighted.
Optional FACE specifies the face to use.

\(fn TAG &optional FACE)" nil nil)

(autoload 'semantic-unhighlight-tag "semantic-decorate" "\
Unhighlight TAG, restoring it's previous face.

\(fn TAG)" nil nil)

(autoload 'semantic-momentary-highlight-one-tag-line "semantic-decorate" "\
Highlight the first line of TAG, unhighlighting before next command.
Optional argument FACE specifies the face to do the highlighting.

\(fn TAG &optional FACE)" nil nil)

(autoload 'semantic-momentary-highlight-tag "semantic-decorate" "\
Highlight TAG, removing highlighting when the user hits a key.
Optional argument FACE is the face to use for highlighting.
If FACE is not specified, then `highlight' will be used.

\(fn TAG &optional FACE)" nil nil)

(autoload 'semantic-set-tag-face "semantic-decorate" "\
Specify that TAG should use FACE for display.

\(fn TAG FACE)" nil nil)

(autoload 'semantic-set-tag-invisible "semantic-decorate" "\
Enable the text in TAG to be made invisible.
If VISIBLE is non-nil, make the text visible.

\(fn TAG &optional VISIBLE)" nil nil)

(autoload 'semantic-tag-invisible-p "semantic-decorate" "\
Return non-nil if TAG is invisible.

\(fn TAG)" nil nil)

(autoload 'semantic-set-tag-intangible "semantic-decorate" "\
Enable the text in TAG to be made intangible.
If TANGIBLE is non-nil, make the text visible.
This function does not have meaning in XEmacs because it seems that
the extent 'intangible' property does not exist.

\(fn TAG &optional TANGIBLE)" nil nil)

(autoload 'semantic-tag-intangible-p "semantic-decorate" "\
Return non-nil if TAG is intangible.
This function does not have meaning in XEmacs because it seems that
the extent 'intangible' property does not exist.

\(fn TAG)" nil nil)

(autoload 'semantic-set-tag-read-only "semantic-decorate" "\
Enable the text in TAG to be made read-only.
Optional argument WRITABLE should be non-nil to make the text writable
instead of read-only.

\(fn TAG &optional WRITABLE)" nil nil)

(autoload 'semantic-tag-read-only-p "semantic-decorate" "\
Return non-nil if the current TAG is marked read only.

\(fn TAG)" nil nil)

(semantic-alias-obsolete 'semantic-highlight-token 'semantic-highlight-tag)

(semantic-alias-obsolete 'semantic-unhighlight-token 'semantic-unhighlight-tag)

(semantic-alias-obsolete 'semantic-momentary-highlight-token 'semantic-momentary-highlight-tag)

(semantic-alias-obsolete 'semantic-set-token-face 'semantic-set-tag-face)

(semantic-alias-obsolete 'semantic-set-token-invisible 'semantic-set-tag-invisible)

(semantic-alias-obsolete 'semantic-token-invisible-p 'semantic-tag-invisible-p)

(semantic-alias-obsolete 'semantic-set-token-intangible 'semantic-set-tag-intangible)

(semantic-alias-obsolete 'semantic-token-intangible-p 'semantic-tag-intangible-p)

(semantic-alias-obsolete 'semantic-set-token-read-only 'semantic-set-tag-read-only)

(semantic-alias-obsolete 'semantic-token-read-only-p 'semantic-tag-read-only-p)

(autoload 'semantic-tag-secondary-overlays "semantic-decorate" "\
Return a list of secondary overlays active on TAG.

\(fn TAG)" nil nil)

(autoload 'semantic-tag-create-secondary-overlay "semantic-decorate" "\
Create a secondary overlay for TAG.
Returns an overlay.  The overlay is also saved in TAG.
LINK-HOOK is a function called whenever TAG is to be linked into
a buffer.  It should take TAG and OVERLAY as arguments.
The LINK-HOOK should be used to position and set properties on the
generated secondary overlay.

\(fn TAG &optional LINK-HOOK)" nil nil)

(autoload 'semantic-tag-get-secondary-overlay "semantic-decorate" "\
Return secondary overlays from TAG with PROPERTY.
PROPERTY is a symbol and all overlays with that symbol are returned..

\(fn TAG PROPERTY)" nil nil)

(autoload 'semantic-tag-delete-secondary-overlay "semantic-decorate" "\
Delete from TAG the secondary overlay OVERLAY-OR-PROPERTY.
If OVERLAY-OR-PROPERTY is an overlay, delete that overlay.
If OVERLAY-OR-PROPERTY is a symbol, find the overlay with that property.

\(fn TAG OVERLAY-OR-PROPERTY)" nil nil)

(autoload 'semantic-set-tag-folded "semantic-decorate" "\
Fold TAG, such that only the first line of text is shown.
Optional argument FOLDED should be non-nil to fold the tag.
nil implies the tag should be fully shown.

\(fn TAG &optional FOLDED)" nil nil)

(autoload 'semantic-tag-folded-p "semantic-decorate" "\
Non-nil if TAG is currently folded.

\(fn TAG)" nil nil)

;;;***

;;;### (autoloads (semantic-decoration-unparsed-include-do-reset
;;;;;;  semantic-decoration-include-visit) "semantic-decorate-include"
;;;;;;  "semantic-decorate-include.el" (18857 63844))
;;; Generated autoloads from semantic-decorate-include.el

(autoload 'semantic-decoration-include-visit "semantic-decorate-include" "\
Visit the included file at point.

\(fn)" t nil)

(autoload 'semantic-decoration-unparsed-include-do-reset "semantic-decorate-include" "\
Do a reset of unparsed includes in the current buffer.

\(fn)" nil nil)

;;;***

;;;### (autoloads (semantic-build-decoration-mode-menu semantic-decoration-mode
;;;;;;  global-semantic-decoration-mode global-semantic-decoration-mode
;;;;;;  semantic-decorate-flush-pending-decorations) "semantic-decorate-mode"
;;;;;;  "semantic-decorate-mode.el" (18857 63844))
;;; Generated autoloads from semantic-decorate-mode.el

(autoload 'semantic-decorate-flush-pending-decorations "semantic-decorate-mode" "\
Flush any pending decorations for BUFFER.
Flush functions from `semantic-decorate-pending-decoration-hooks'.

\(fn &optional BUFFER)" nil nil)

(autoload 'global-semantic-decoration-mode "semantic-decorate-mode" "\
Toggle global use of option `semantic-decoration-mode'.
Decoration mode turns on all active decorations as specified
by `semantic-decoration-styles'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

(defvar global-semantic-decoration-mode nil "\
*If non-nil, enable global use of command `semantic-decoration-mode'.
When this mode is activated, decorations specified by
`semantic-decoration-styles'.")

(custom-autoload 'global-semantic-decoration-mode "semantic-decorate-mode" nil)

(defvar semantic-decoration-mode nil "\
Non-nil if command `semantic-decoration-mode' is enabled.
Use the command `semantic-decoration-mode' to change this variable.")

(autoload 'semantic-decoration-mode "semantic-decorate-mode" "\
Minor mode for decorating tags.
Decorations are specified in `semantic-decoration-styles'.
You can define new decoration styles with
`define-semantic-decoration-style'.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\(fn &optional ARG)" t nil)

(autoload 'semantic-build-decoration-mode-menu "semantic-decorate-mode" "\
Create a menu listing all the known decorations for toggling.
IGNORE any input arguments.

\(fn &rest IGNORE)" nil nil)

;;;***

;;;### (autoloads (semantic-dependency-find-file-on-path semantic-customize-system-include-path
;;;;;;  semantic-reset-system-include semantic-remove-system-include
;;;;;;  semantic-add-system-include defcustom-mode-local-semantic-dependency-system-include-path)
;;;;;;  "semantic-dep" "semantic-dep.el" (18857 63844))
;;; Generated autoloads from semantic-dep.el

(defvar semantic-dependency-include-path nil "\
Defines the include path used when searching for files.
This should be a list of directories to search which is specific
to the file being included.

If `semantic-dependency-tag-file' is overridden for a given
language, this path is most likely ignored.

The above function, reguardless of being overriden, caches the
located dependency file location in the tag property
`dependency-file'.  If you override this function, you do not
need to implement your own cache.  Each time the buffer is fully
reparsed, the cache will be reset.

TODO: use ffap.el to locate such items?

NOTE: Obsolete this, or use as special user")

(defvar semantic-dependency-system-include-path nil "\
Defines the system include path.
This should be set with either `defvar-mode-local', or with
`semantic-add-system-include'.

For mode authors, use
`defcustom-mode-local-semantic-dependency-system-include-path'
to create a mode-specific variable to control this.

When searching for a file associated with a name found in an tag of
class include, this path will be inspected for includes of type
`system'.  Some include tags are agnostic to this setting and will
check both the project and system directories.")

(autoload 'defcustom-mode-local-semantic-dependency-system-include-path "semantic-dep" "\
Create a mode-local value of the system-dependency include path.
MODE is the `major-mode' this name/value pairs is for.
NAME is the name of the customizable value users will use.
VALUE is the path (a list of strings) to add.
DOCSTRING is a documentation string applied to the variable NAME
users will customize.

Creates a customizable variable users can customize that will
keep semantic data structures up to date.

\(fn MODE NAME VALUE &optional DOCSTRING)" nil (quote macro))

(autoload 'semantic-add-system-include "semantic-dep" "\
Add a system include DIR to path for MODE.
Modifies a mode-local version of `semantic-dependency-system-include-path'.

Changes made by this function are not persistent.

\(fn DIR &optional MODE)" t nil)

(autoload 'semantic-remove-system-include "semantic-dep" "\
Add a system include DIR to path for MODE.
Modifies a mode-local version of`semantic-dependency-system-include-path'.

Changes made by this function are not persistent.

\(fn DIR &optional MODE)" t nil)

(autoload 'semantic-reset-system-include "semantic-dep" "\
Reset the system include list to empty for MODE.
Modifies a mode-local version of
`semantic-dependency-system-include-path'.

\(fn &optional MODE)" t nil)

(autoload 'semantic-customize-system-include-path "semantic-dep" "\
Customize the include path for this `major-mode'.
To create a customizable include path for a major MODE, use the
macro `defcustom-mode-local-semantic-dependency-system-include-path'.

\(fn &optional MODE)" t nil)

(autoload 'semantic-dependency-find-file-on-path "semantic-dep" "\
Return an expanded file name for FILE on available paths.
If SYSTEMP is true, then only search system paths.
If optional argument MODE is non-nil, then derive paths from the
provided mode, not from the current major mode.

\(fn FILE SYSTEMP &optional MODE)" nil nil)

;;;***

;;;### (autoloads (semantic-documentation-for-tag) "semantic-doc"
;;;;;;  "semantic-doc.el" (18857 63844))
;;; Generated autoloads from semantic-doc.el

(autoload 'semantic-documentation-for-tag "semantic-doc" "\
Find documentation from TAG and return it as a clean string.
TAG might have DOCUMENTATION set in it already.  If not, there may be
some documentation in a comment preceding TAG's definition which we
can look for.  When appropriate, this can be overridden by a language specific
enhancement.
Optional argument NOSNARF means to only return the lexical analyzer token for it.
If nosnarf if 'lex, then only return the lex token.

\(fn &optional TAG NOSNARF)" nil nil)

(semantic-alias-obsolete 'semantic-find-documentation 'semantic-documentation-for-tag)

;;;***

;;;### (autoloads (semantic-load-enable-all-exuberent-ctags-support
;;;;;;  semantic-default-sh-setup) "semantic-ectag-lang" "ctags/semantic-ectag-lang.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from ctags/semantic-ectag-lang.el

(autoload 'semantic-default-sh-setup "semantic-ectag-lang" "\
Set up a buffer for Semantic parsing for SH language using CTags.

\(fn)" nil nil)

(autoload 'semantic-load-enable-all-exuberent-ctags-support "semantic-ectag-lang" "\
Enable all ectag supported backend support features.
This includes:
  * semanticdb backend support 
  * buffer parsing using ectags for somoe modes.

Any mode that has been tested to work will be added to this function.

\(fn)" t nil)

;;;***

;;;### (autoloads (semantic-ectag-parse-buffer) "semantic-ectag-parse"
;;;;;;  "ctags/semantic-ectag-parse.el" (18857 63844))
;;; Generated autoloads from ctags/semantic-ectag-parse.el

(autoload 'semantic-ectag-parse-buffer "semantic-ectag-parse" "\
Execute Exuberent CTags on this buffer.
Convert the output tags into Semantic tags.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "semantic-ede-grammar" "semantic-ede-grammar.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from semantic-ede-grammar.el

(eieio-defclass-autoload 'semantic-ede-proj-target-grammar '(ede-proj-target-makefile) "semantic-ede-grammar" "This target consists of a group of grammar files.\nA grammar target consists of grammar files that build Emacs Lisp programs for\nparsing different languages.")

(autoload 'ede-proj-target-elisp "semantic-ede-proj-target-grammar" "\
Target class for Emacs/Semantic grammar files." nil nil)

(eval-after-load "ede-proj" '(require 'semantic-ede-grammar))

;;;***

;;;### (autoloads (semantic-edits-incremental-parser semantic-edits-flush-changes
;;;;;;  semantic-edits-change-function-handle-changes semantic-change-function
;;;;;;  semantic-edits-verbose-flag) "semantic-edit" "semantic-edit.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from semantic-edit.el

(defvar semantic-edits-verbose-flag nil "\
Non-nil means the incremental perser is verbose.
If nil, errors are still displayed, but informative messages are not.")

(custom-autoload 'semantic-edits-verbose-flag "semantic-edit" t)

(autoload 'semantic-change-function "semantic-edit" "\
Provide a mechanism for semantic tag management.
Argument START, END, and LENGTH specify the bounds of the change.

\(fn START END LENGTH)" nil nil)

(autoload 'semantic-edits-change-function-handle-changes "semantic-edit" "\
Run whenever a buffer controlled by `semantic-mode' change.
Tracks when and how the buffer is re-parsed.
Argument START, END, and LENGTH specify the bounds of the change.

\(fn START END LENGTH)" nil nil)

(autoload 'semantic-edits-flush-changes "semantic-edit" "\
Flush the changes in the current buffer.

\(fn)" nil nil)

(autoload 'semantic-edits-incremental-parser "semantic-edit" "\
Incrementally reparse the current buffer.
Incremental parser allows semantic to only reparse those sections of
the buffer that have changed.  This function depends on
`semantic-edits-change-function-handle-changes' setting up change
overlays in the current buffer.  Those overlays are analyzed against
the semantic cache to see what needs to be changed.

\(fn)" nil nil)

(defalias 'semantic-parse-changes-default 'semantic-edits-incremental-parser)

(add-hook 'semantic-change-hooks #'semantic-edits-change-function-handle-changes)

(add-hook 'semantic-before-toplevel-cache-flush-hook #'semantic-edits-flush-changes)

;;;***

;;;### (autoloads (semantic-default-elisp-setup) "semantic-el" "bovine/semantic-el.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from bovine/semantic-el.el

(autoload 'semantic-default-elisp-setup "semantic-el" "\
Setup hook function for Emacs Lisp files and Semantic.

\(fn)" nil nil)

(add-hook 'emacs-lisp-mode-hook 'semantic-default-elisp-setup)

(add-hook 'lisp-mode-hook 'semantic-default-elisp-setup)

(eval-after-load "semanticdb" '(require 'semanticdb-el))

;;;***

;;;### (autoloads (semantic-elp-load-old-run semantic-elp-analyze)
;;;;;;  "semantic-elp" "semantic-elp.el" (18857 63844))
;;; Generated autoloads from semantic-elp.el

(autoload 'semantic-elp-analyze "semantic-elp" "\
Run the analyzer, using ELP to measure performance.

\(fn)" t nil)

(autoload 'semantic-elp-load-old-run "semantic-elp" "\
Load an old run from FILE, and show it.

\(fn FILE)" t nil)

;;;***

;;;### (autoloads (semantic-brute-find-innermost-tag-by-position
;;;;;;  semantic-brute-find-tag-by-position semantic-brute-find-first-tag-by-function
;;;;;;  semantic-brute-find-tag-by-function semantic-brute-find-tag-by-attribute-value
;;;;;;  semantic-brute-find-tag-by-attribute semantic-brute-find-tag-by-property
;;;;;;  semantic-brute-find-tag-by-name-regexp semantic-brute-find-tag-by-type-regexp
;;;;;;  semantic-brute-find-tag-by-type semantic-brute-find-tag-standard
;;;;;;  semantic-brute-find-tag-by-class semantic-brute-find-first-tag-by-name
;;;;;;  semantic-deep-find-tags-by-name-regexp semantic-deep-find-tags-for-completion
;;;;;;  semantic-deep-find-tags-by-name semantic-find-tags-by-scope-protection
;;;;;;  semantic-find-tags-of-compound-type semantic-find-tags-by-type
;;;;;;  semantic-find-tags-by-class semantic-find-tags-by-name-regexp
;;;;;;  semantic-find-tags-for-completion semantic-find-tags-by-name
;;;;;;  semantic-current-tag-of-class semantic-current-tag-parent
;;;;;;  semantic-current-tag semantic-find-tag-parent-by-overlay
;;;;;;  semantic-find-tag-by-overlay-prev semantic-find-tag-by-overlay-next
;;;;;;  semantic-find-tag-by-overlay-in-region semantic-find-tag-by-overlay)
;;;;;;  "semantic-find" "semantic-find.el" (18857 63844))
;;; Generated autoloads from semantic-find.el

(autoload 'semantic-find-tag-by-overlay "semantic-find" "\
Find all tags covering POSITIONORMARKER by using overlays.
If POSITIONORMARKER is nil, use the current point.
Optional BUFFER is used if POSITIONORMARKER is a number, otherwise the current
buffer is used.  This finds all tags covering the specified position
by checking for all overlays covering the current spot.  They are then sorted
from largest to smallest via the start location.

\(fn &optional POSITIONORMARKER BUFFER)" nil nil)

(autoload 'semantic-find-tag-by-overlay-in-region "semantic-find" "\
Find all tags which exist in whole or in part between START and END.
Uses overlays to determine positin.
Optional BUFFER argument specifies the buffer to use.

\(fn START END &optional BUFFER)" nil nil)

(autoload 'semantic-find-tag-by-overlay-next "semantic-find" "\
Find the next tag after START in BUFFER.
If START is in an overlay, find the tag which starts next,
not the current tag.

\(fn &optional START BUFFER)" nil nil)

(autoload 'semantic-find-tag-by-overlay-prev "semantic-find" "\
Find the next tag before START in BUFFER.
If START is in an overlay, find the tag which starts next,
not the current tag.

\(fn &optional START BUFFER)" nil nil)

(autoload 'semantic-find-tag-parent-by-overlay "semantic-find" "\
Find the parent of TAG by overlays.
Overlays are a fast way of finding this information for active buffers.

\(fn TAG)" nil nil)

(autoload 'semantic-current-tag "semantic-find" "\
Return the current tag in the current buffer.
If there are more than one in the same location, return the
smallest tag.  Return nil if there is no tag here.

\(fn)" nil nil)

(autoload 'semantic-current-tag-parent "semantic-find" "\
Return the current tags parent in the current buffer.
A tag's parent would be a containing structure, such as a type
containing a field.  Return nil if there is no parent.

\(fn)" nil nil)

(autoload 'semantic-current-tag-of-class "semantic-find" "\
Return the current (smallest) tags of CLASS in the current buffer.
If the smallest tag is not of type CLASS, keep going upwards until one
is found.
Uses `semantic-tag-class' for classification.

\(fn CLASS)" nil nil)

(defsubst semantic-find-first-tag-by-name (name &optional table) "\
Find the first tag with NAME in TABLE.
NAME is a string.
TABLE is a semantic tags table.  See `semantic-something-to-tag-table'.
This routine uses `assoc' to quickly find the first matching entry." (funcall (if semantic-case-fold (quote assoc-ignore-case) (quote assoc)) name (semantic-something-to-tag-table table)))

(autoload 'semantic-find-tags-by-name "semantic-find" "\
Find all tags with NAME in TABLE.
NAME is a string.
TABLE is a tag table.  See `semantic-something-to-tag-table'.

\(fn NAME &optional TABLE)" nil (quote macro))

(autoload 'semantic-find-tags-for-completion "semantic-find" "\
Find all tags whos name begins with PREFIX in TABLE.
PREFIX is a string.
TABLE is a tag table.  See `semantic-something-to-tag-table'.
While it would be nice to use `try-completion' or `all-completions',
those functions do not return the tags, only a string.
Uses `compare-strings' for fast comparison.

\(fn PREFIX &optional TABLE)" nil (quote macro))

(autoload 'semantic-find-tags-by-name-regexp "semantic-find" "\
Find all tags with name matching REGEXP in TABLE.
REGEXP is a string containing a regular expression,
TABLE is a tag table.  See `semantic-something-to-tag-table'.
Consider using `semantic-find-tags-for-completion' if you are
attempting to do completions.

\(fn REGEXP &optional TABLE)" nil (quote macro))

(autoload 'semantic-find-tags-by-class "semantic-find" "\
Find all tags of class CLASS in TABLE.
CLASS is a symbol representing the class of the token, such as
'variable, of 'function..
TABLE is a tag table.  See `semantic-something-to-tag-table'.

\(fn CLASS &optional TABLE)" nil (quote macro))

(autoload 'semantic-find-tags-by-type "semantic-find" "\
Find all tags of with a type TYPE in TABLE.
TYPE is a string or tag representing a data type as defined in the
language the tags were parsed from, such as \"int\", or perhaps
a tag whose name is that of a struct or class.
TABLE is a tag table.  See `semantic-something-to-tag-table'.

\(fn TYPE &optional TABLE)" nil (quote macro))

(autoload 'semantic-find-tags-of-compound-type "semantic-find" "\
Find all tags which are a compound type in TABLE.
Compound types are structures, or other data type which
is not of a primitive nature, such as int or double.
Used in completion.

\(fn &optional TABLE)" nil (quote macro))

(autoload 'semantic-find-tags-by-scope-protection "semantic-find" "\
Find all tags accessable by SCOPEPROTECTION.
SCOPEPROTECTION is a symbol which can be returned by the method
`semantic-tag-protection'.  A hard-coded order is used to determine a match.
PARENT is a tag representing the PARENT slot needed for
`semantic-tag-protection'.
TABLE is a list of tags (a subset of PARENT members) to scan.  If TABLE is nil,
the type members of PARENT are used.
See `semantic-tag-protected-p' for details on which tags are returned.

\(fn SCOPEPROTECTION PARENT &optional TABLE)" nil nil)

(defsubst semantic-find-tags-included (&optional table) "\
Find all tags in TABLE that are of the 'include class.
TABLE is a tag table.  See `semantic-something-to-tag-table'." (semantic-find-tags-by-class (quote include) table))

(autoload 'semantic-deep-find-tags-by-name "semantic-find" "\
Find all tags with NAME in TABLE.
Search in top level tags, and their components, in TABLE.
NAME is a string.
TABLE is a tag table.  See `semantic-flatten-tags-table'.
See also `semantic-find-tags-by-name'.

\(fn NAME &optional TABLE)" nil (quote macro))

(autoload 'semantic-deep-find-tags-for-completion "semantic-find" "\
Find all tags whos name begins with PREFIX in TABLE.
Search in top level tags, and their components, in TABLE.
TABLE is a tag table.  See `semantic-flatten-tags-table'.
See also `semantic-find-tags-for-completion'.

\(fn PREFIX &optional TABLE)" nil (quote macro))

(autoload 'semantic-deep-find-tags-by-name-regexp "semantic-find" "\
Find all tags with name matching REGEXP in TABLE.
Search in top level tags, and their components, in TABLE.
REGEXP is a string containing a regular expression,
TABLE is a tag table.  See `semantic-flatten-tags-table'.
See also `semantic-find-tags-by-name-regexp'.
Consider using `semantic-deep-find-tags-for-completion' if you are
attempting to do completions.

\(fn REGEXP &optional TABLE)" nil (quote macro))

(autoload 'semantic-brute-find-first-tag-by-name "semantic-find" "\
Find a tag NAME within STREAMORBUFFER.  NAME is a string.
If SEARCH-PARTS is non-nil, search children of tags.
If SEARCH-INCLUDE was never implemented.

Use `semantic-find-first-tag-by-name' instead.

\(fn NAME STREAMORBUFFER &optional SEARCH-PARTS SEARCH-INCLUDE)" nil nil)

(autoload 'semantic-brute-find-tag-by-class "semantic-find" "\
Find all tags with a class CLASS within STREAMORBUFFER.
CLASS is a symbol representing the class of the tags to find.
See `semantic-tag-class'.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'.

Use `semantic-find-tag-by-class' instead.

\(fn CLASS STREAMORBUFFER &optional SEARCH-PARTS SEARCH-INCLUDES)" nil (quote macro))

(autoload 'semantic-brute-find-tag-standard "semantic-find" "\
Find all tags in STREAMORBUFFER which define simple class types.
See `semantic-tag-class'.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'.

\(fn STREAMORBUFFER &optional SEARCH-PARTS SEARCH-INCLUDES)" nil (quote macro))

(autoload 'semantic-brute-find-tag-by-type "semantic-find" "\
Find all tags with type TYPE within STREAMORBUFFER.
TYPE is a string which is the name of the type of the tags returned.
See `semantic-tag-type'.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'.

\(fn TYPE STREAMORBUFFER &optional SEARCH-PARTS SEARCH-INCLUDES)" nil nil)

(autoload 'semantic-brute-find-tag-by-type-regexp "semantic-find" "\
Find all tags with type matching REGEXP within STREAMORBUFFER.
REGEXP is a regular expression  which matches the  name of the type of the
tags returned.  See `semantic-tag-type'.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'.

\(fn REGEXP STREAMORBUFFER &optional SEARCH-PARTS SEARCH-INCLUDES)" nil nil)

(autoload 'semantic-brute-find-tag-by-name-regexp "semantic-find" "\
Find all tags whose name match REGEX in STREAMORBUFFER.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'.

\(fn REGEX STREAMORBUFFER &optional SEARCH-PARTS SEARCH-INCLUDES)" nil nil)

(autoload 'semantic-brute-find-tag-by-property "semantic-find" "\
Find all tags with PROPERTY equal to VALUE in STREAMORBUFFER.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'.

\(fn PROPERTY VALUE STREAMORBUFFER &optional SEARCH-PARTS SEARCH-INCLUDES)" nil nil)

(autoload 'semantic-brute-find-tag-by-attribute "semantic-find" "\
Find all tags with a given ATTR in STREAMORBUFFER.
ATTR is a symbol key into the attributes list.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'.

\(fn ATTR STREAMORBUFFER &optional SEARCH-PARTS SEARCH-INCLUDES)" nil nil)

(autoload 'semantic-brute-find-tag-by-attribute-value "semantic-find" "\
Find all tags with a given ATTR equal to VALUE in STREAMORBUFFER.
ATTR is a symbol key into the attributes list.
VALUE is the value that ATTR should match.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'.

\(fn ATTR VALUE STREAMORBUFFER &optional SEARCH-PARTS SEARCH-INCLUDES)" nil nil)

(autoload 'semantic-brute-find-tag-by-function "semantic-find" "\
Find all tags for which FUNCTION's value is non-nil within STREAMORBUFFER.
FUNCTION must return non-nil if an element of STREAM will be included
in the new list.

If optional argument SEARCH-PARTS is non-nil, all sub-parts of tags
are searched.  The overloadable function `semantic-tag-componenets' is
used for the searching child lists.  If SEARCH-PARTS is the symbol
'positiononly, then only children that have positional information are
searched.

If SEARCH-INCLUDES has not been implemented.
This parameter hasn't be active for a while and is obsolete.

\(fn FUNCTION STREAMORBUFFER &optional SEARCH-PARTS SEARCH-INCLUDES)" nil nil)

(autoload 'semantic-brute-find-first-tag-by-function "semantic-find" "\
Find the first tag which FUNCTION match within STREAMORBUFFER.
FUNCTION must return non-nil if an element of STREAM will be included
in the new list.

The following parameters were never implemented.

If optional argument SEARCH-PARTS, all sub-parts of tags are searched.
The overloadable function `semantic-tag-components' is used for
searching.
If SEARCH-INCLUDES is non-nil, then all include files are also
searched for matches.

\(fn FUNCTION STREAMORBUFFER &optional SEARCH-PARTS SEARCH-INCLUDES)" nil nil)

(autoload 'semantic-brute-find-tag-by-position "semantic-find" "\
Find a tag covering POSITION within STREAMORBUFFER.
POSITION is a number, or marker.  If NOMEDIAN is non-nil, don't do
the median calculation, and return nil.

\(fn POSITION STREAMORBUFFER &optional NOMEDIAN)" nil nil)

(autoload 'semantic-brute-find-innermost-tag-by-position "semantic-find" "\
Find a list of tags covering POSITION within STREAMORBUFFER.
POSITION is a number, or marker.  If NOMEDIAN is non-nil, don't do
the median calculation, and return nil.
This function will find the topmost item, and recurse until no more
details are available of findable.

\(fn POSITION STREAMORBUFFER &optional NOMEDIAN)" nil nil)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-overlay 'semantic-find-tag-by-overlay)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-overlay-in-region 'semantic-find-tag-by-overlay-in-region)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-overlay-next 'semantic-find-tag-by-overlay-next)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-overlay-prev 'semantic-find-tag-by-overlay-prev)

(semantic-alias-obsolete 'semantic-find-nonterminal-parent-by-overlay 'semantic-find-tag-parent-by-overlay)

(semantic-alias-obsolete 'semantic-current-nonterminal 'semantic-current-tag)

(semantic-alias-obsolete 'semantic-current-nonterminal-parent 'semantic-current-tag-parent)

(semantic-alias-obsolete 'semantic-current-nonterminal-of-type 'semantic-current-tag-of-class)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-name 'semantic-brute-find-first-tag-by-name)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-token 'semantic-brute-find-tag-by-class)

(semantic-alias-obsolete 'semantic-find-nonterminal-standard 'semantic-brute-find-tag-standard)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-type 'semantic-brute-find-tag-by-type)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-type-regexp 'semantic-brute-find-tag-by-type-regexp)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-name-regexp 'semantic-brute-find-tag-by-name-regexp)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-property 'semantic-brute-find-tag-by-property)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-extra-spec 'semantic-brute-find-tag-by-attribute)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-extra-spec-value 'semantic-brute-find-tag-by-attribute-value)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-function 'semantic-brute-find-tag-by-function)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-function-first-match 'semantic-brute-find-first-tag-by-function)

(semantic-alias-obsolete 'semantic-find-nonterminal-by-position 'semantic-brute-find-tag-by-position)

(semantic-alias-obsolete 'semantic-find-innermost-nonterminal-by-position 'semantic-brute-find-innermost-tag-by-position)

;;;***

;;;### (autoloads (semantic-format-tag-uml-concise-prototype semantic-format-tag-uml-prototype
;;;;;;  semantic-format-tag-uml-abbreviate semantic-format-tag-concise-prototype
;;;;;;  semantic-format-tag-prototype semantic-format-tag-short-doc
;;;;;;  semantic-format-tag-summarize-with-file semantic-format-tag-summarize
;;;;;;  semantic-format-tag-abbreviate semantic-format-tag-name semantic-format-tag-prin1
;;;;;;  semantic-format-tag-type) "semantic-format" "semantic-format.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from semantic-format.el

(defvar semantic-format-tag-functions '(semantic-format-tag-name semantic-format-tag-canonical-name semantic-format-tag-abbreviate semantic-format-tag-summarize semantic-format-tag-summarize-with-file semantic-format-tag-short-doc semantic-format-tag-prototype semantic-format-tag-concise-prototype semantic-format-tag-uml-abbreviate semantic-format-tag-uml-prototype semantic-format-tag-uml-concise-prototype semantic-format-tag-prin1) "\
List of functions which convert a tag to text.
Each function must take the parameters TAG &optional PARENT COLOR.
TAG is the tag to convert.
PARENT is a parent tag or name which refers to the structure
or class which contains TAG.  PARENT is NOT a class which a TAG
would claim as a parent.
COLOR indicates that the generated text should be colored using
`font-lock'.")

(semantic-varalias-obsolete 'semantic-token->text-functions 'semantic-format-tag-functions)

(defvar semantic-format-tag-custom-list (append '(radio) (mapcar (lambda (f) (list 'const f)) semantic-format-tag-functions) '(function)) "\
A List used by customizeable variables to choose a tag to text function.
Use this variable in the :type field of a customizable variable.")

(autoload 'semantic-format-tag-type "semantic-format" "\
Convert the data type of TAG to a string usable in tag formatting.
It is presumed that TYPE is a string or semantic tag.

\(fn TAG COLOR)" nil nil)

(autoload 'semantic-format-tag-prin1 "semantic-format" "\
Convert TAG to a string that is the print name for TAG.
PARENT and COLOR are ignored.

\(fn TAG &optional PARENT COLOR)" nil nil)

(autoload 'semantic-format-tag-name "semantic-format" "\
Return the name string describing TAG.
The name is the shortest possible representation.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.

\(fn TAG &optional PARENT COLOR)" nil nil)

(autoload 'semantic-format-tag-abbreviate "semantic-format" "\
Return an abbreviated string describing TAG.
The abbreviation is to be short, with possible symbols indicating
the type of tag, or other information.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.

\(fn TAG &optional PARENT COLOR)" nil nil)

(autoload 'semantic-format-tag-summarize "semantic-format" "\
Summarize TAG in a reasonable way.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.

\(fn TAG &optional PARENT COLOR)" nil nil)

(autoload 'semantic-format-tag-summarize-with-file "semantic-format" "\
Like `semantic-format-tag-summarize', but with the file name.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.

\(fn TAG &optional PARENT COLOR)" nil nil)

(autoload 'semantic-format-tag-short-doc "semantic-format" "\
Display a short form of TAG's documentation. (Comments, or docstring.)
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.

\(fn TAG &optional PARENT COLOR)" nil nil)

(autoload 'semantic-format-tag-prototype "semantic-format" "\
Return a prototype for TAG.
This function should be overloaded, though it need not be used.
This is because it can be used to create code by language independent
tools.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.

\(fn TAG &optional PARENT COLOR)" nil nil)

(autoload 'semantic-format-tag-concise-prototype "semantic-format" "\
Return a concise prototype for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.

\(fn TAG &optional PARENT COLOR)" nil nil)

(autoload 'semantic-format-tag-uml-abbreviate "semantic-format" "\
Return a UML style abbreviation for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.

\(fn TAG &optional PARENT COLOR)" nil nil)

(autoload 'semantic-format-tag-uml-prototype "semantic-format" "\
Return a UML style prototype for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.

\(fn TAG &optional PARENT COLOR)" nil nil)

(autoload 'semantic-format-tag-uml-concise-prototype "semantic-format" "\
Return a UML style concise prototype for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors.

\(fn TAG &optional PARENT COLOR)" nil nil)

;;;***

;;;### (autoloads (semantic-gcc-setup) "semantic-gcc" "bovine/semantic-gcc.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from bovine/semantic-gcc.el

(autoload 'semantic-gcc-setup "semantic-gcc" "\
Setup Semantic C parsing based on GCC output.
Optional argument GCC-CMD is an optional command to use instead of \"gcc\".

\(fn &optional GCC-CMD)" t nil)

;;;***

;;;### (autoloads (semantic-grammar-batch-build-packages) "semantic-grammar"
;;;;;;  "semantic-grammar.el" (18857 63844))
;;; Generated autoloads from semantic-grammar.el

(autoload 'semantic-grammar-batch-build-packages "semantic-grammar" "\
Build Lisp packages from grammar files on the command line.
That is, run `semantic-grammar-batch-build-one-package' for each file.
Each file is processed even if an error occurred previously.
Must be used from the command line, with `-batch'.
For example, to process grammar files in current directory, invoke:

  \"emacs -batch -f semantic-grammar-batch-build-packages .\".

See also the variable `semantic-grammar-file-regexp'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (semantic-default-html-setup) "semantic-html" "semantic-html.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from semantic-html.el

(autoload 'semantic-default-html-setup "semantic-html" "\
Set up a buffer for parsing of HTML files.

\(fn)" nil nil)

(add-hook 'html-mode-hook 'semantic-default-html-setup)

;;;***

;;;### (autoloads (semantic-ia-describe-class semantic-ia-show-doc
;;;;;;  semantic-ia-fast-jump semantic-ia-show-summary semantic-ia-complete-tip
;;;;;;  semantic-ia-complete-symbol-menu semantic-ia-complete-symbol)
;;;;;;  "semantic-ia" "semantic-ia.el" (18857 63844))
;;; Generated autoloads from semantic-ia.el

(autoload 'semantic-ia-complete-symbol "semantic-ia" "\
Complete the current symbol at POINT.
Completion options are calculated with `semantic-analyze-possible-completions'.

\(fn POINT)" t nil)

(autoload 'semantic-ia-complete-symbol-menu "semantic-ia" "\
Complete the current symbol via a menu based at POINT.
Completion options are calculated with `semantic-analyze-possible-completions'.

\(fn POINT)" t nil)

(autoload 'semantic-ia-complete-tip "semantic-ia" "\
Pop up a tooltip for completion at POINT.

\(fn POINT)" t nil)

(autoload 'semantic-ia-show-summary "semantic-ia" "\
Display a summary for the symbol under POINT.

\(fn POINT)" t nil)

(autoload 'semantic-ia-fast-jump "semantic-ia" "\
Jump to the tag referred to by the code at POINT.
Uses `semantic-analyze-current-context' output to identify an accurate
origin of the code at point.

\(fn POINT)" t nil)

(autoload 'semantic-ia-show-doc "semantic-ia" "\
Display the code-level documentation for the symbol at POINT.

\(fn POINT)" t nil)

(autoload 'semantic-ia-describe-class "semantic-ia" "\
Display all known parts for the datatype TYPENAME.
If the type in question is a class, all methods and other accessible
parts of the parent classes are displayed.

\(fn TYPENAME)" t nil)

;;;***

;;;### (autoloads (semantic-speedbar-analysis) "semantic-ia-sb" "semantic-ia-sb.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from semantic-ia-sb.el

(autoload 'semantic-speedbar-analysis "semantic-ia-sb" "\
Start Speedbar in semantic analysis mode.
The analyzer displays information about the current context, plus a smart
list of possible completions.

\(fn)" t nil)

;;;***

;;;### (autoloads (semantic-ia-utest) "semantic-ia-utest" "semantic-ia-utest.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from semantic-ia-utest.el

(autoload 'semantic-ia-utest "semantic-ia-utest" "\
Run the semantic ia unit test against stored sources.
Argument ARG specifies which set of tests to run.
 1 - ia utests
 2 - regs utests
 3 - symrefs utests

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (semantic-idle-scheduler-remove semantic-idle-scheduler-add
;;;;;;  semantic-idle-scheduler-mode global-semantic-idle-scheduler-mode
;;;;;;  global-semantic-idle-scheduler-mode) "semantic-idle" "semantic-idle.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from semantic-idle.el

(defvar global-semantic-idle-scheduler-mode nil "\
*If non-nil, enable global use of idle-scheduler mode.")

(custom-autoload 'global-semantic-idle-scheduler-mode "semantic-idle" nil)

(autoload 'global-semantic-idle-scheduler-mode "semantic-idle" "\
Toggle global use of option `semantic-idle-scheduler-mode'.
The idle scheduler with automatically reparse buffers in idle time,
and then schedule other jobs setup with `semantic-idle-scheduler-add'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

(defvar semantic-idle-scheduler-mode nil "\
Non-nil if idle-scheduler minor mode is enabled.
Use the command `semantic-idle-scheduler-mode' to change this variable.")

(autoload 'semantic-idle-scheduler-mode "semantic-idle" "\
Minor mode to auto parse buffer following a change.
When this mode is off, a buffer is only rescanned for tokens when
some command requests the list of available tokens.  When idle-scheduler
is enabled, Emacs periodically checks to see if the buffer is out of
date, and reparses while the user is idle (not typing.)

With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\(fn &optional ARG)" t nil)

(autoload 'semantic-idle-scheduler-add "semantic-idle" "\
Schedule FUNCTION to occur during idle time.

\(fn FUNCTION)" nil nil)

(autoload 'semantic-idle-scheduler-remove "semantic-idle" "\
Unschedule FUNCTION to occur during idle time.

\(fn FUNCTION)" nil nil)

;;;***

;;;### (autoloads (semantic-create-imenu-index semantic-imenu-expand-type-members
;;;;;;  semantic-imenu-bucketize-file semantic-imenu-summary-function)
;;;;;;  "semantic-imenu" "semantic-imenu.el" (18857 63844))
;;; Generated autoloads from semantic-imenu.el

(defvar semantic-imenu-summary-function 'semantic-format-tag-abbreviate "\
*Function to use when creating items in Imenu.
Some useful functions are found in `semantic-format-tag-functions'.")

(custom-autoload 'semantic-imenu-summary-function "semantic-imenu" t)

(defvar semantic-imenu-bucketize-file t "\
*Non-nil if tags in a file are to be grouped into buckets.")

(custom-autoload 'semantic-imenu-bucketize-file "semantic-imenu" t)

(defvar semantic-imenu-expand-type-members t "\
*Non-nil if types should have submenus with members in them.")

(custom-autoload 'semantic-imenu-expand-type-members "semantic-imenu" t)

(defvar semantic-imenu-expandable-tag-classes '(type) "\
List of expandable tag classes.
Tags of those classes will be given submenu with children.
By default, a `type' has interesting children.  In Texinfo, however, a
`section' has interesting children.")

(autoload 'semantic-create-imenu-index "semantic-imenu" "\
Create an imenu index for any buffer which supports Semantic.
Uses the output of the Semantic parser to create the index.
Optional argument STREAM is an optional stream of tags used to create menus.

\(fn &optional STREAM)" nil nil)

;;;***

;;;### (autoloads (define-lex-block-analyzer define-lex-simple-regex-analyzer
;;;;;;  define-lex-regex-analyzer define-lex-analyzer semantic-lex
;;;;;;  semantic-lex-init define-lex) "semantic-lex" "semantic-lex.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from semantic-lex.el

(defvar semantic-lex-analyzer 'semantic-flex "\
The lexical analyzer used for a given buffer.
See `semantic-lex' for documentation.
For compatibility with Semantic 1.x it defaults to `semantic-flex'.")

(autoload 'define-lex "semantic-lex" "\
Create a new lexical analyzer with NAME.
DOC is a documentation string describing this analyzer.
ANALYZERS are small code snippets of analyzers to use when
building the new NAMED analyzer.  Only use analyzers which
are written to be used in `define-lex'.
Each analyzer should be an analyzer created with `define-lex-analyzer'.
Note: The order in which analyzers are listed is important.
If two analyzers can match the same text, it is important to order the
analyzers so that the one you want to match first occurs first.  For
example, it is good to put a numbe analyzer in front of a symbol
analyzer which might mistake a number for as a symbol.

\(fn NAME DOC &rest ANALYZERS)" nil (quote macro))

(autoload 'semantic-lex-init "semantic-lex" "\
Initialize any lexical state for this buffer.

\(fn)" nil nil)

(autoload 'semantic-lex "semantic-lex" "\
Lexically analyze text in the current buffer between START and END.
Optional argument DEPTH indicates at what level to scan over entire
lists.  The last argument, LENGTH specifies that `semantic-lex'
should only return LENGTH tokens.  The return value is a token stream.
Each element is a list, such of the form
  (symbol start-expression .  end-expression)
where SYMBOL denotes the token type.
See `semantic-lex-tokens' variable for details on token types.  END
does not mark the end of the text scanned, only the end of the
beginning of text scanned.  Thus, if a string extends past END, the
end of the return token will be larger than END.  To truly restrict
scanning, use `narrow-to-region'.

\(fn START END &optional DEPTH LENGTH)" nil nil)

(autoload 'define-lex-analyzer "semantic-lex" "\
Create a single lexical analyzer NAME with DOC.
When an analyzer is called, the current buffer and point are
positioned in a buffer at the location to be analyzed.
CONDITION is an expression which returns t if FORMS should be run.
Within the bounds of CONDITION and FORMS, the use of backquote
can be used to evaluate expressions at compile time.
While forms are running, the following variables will be locally bound:
  `semantic-lex-analysis-bounds' - The bounds of the current analysis.
                  of the form (START . END)
  `semantic-lex-maximum-depth' - The maximum depth of semantic-list
                  for the current analysis.
  `semantic-lex-current-depth' - The current depth of `semantic-list' that has
                  been decended.
  `semantic-lex-end-point' - End Point after match.
                   Analyzers should set this to a buffer location if their
                   match string does not represent the end of the matched text.
  `semantic-lex-token-stream' - The token list being collected.
                   Add new lexical tokens to this list.
Proper action in FORMS is to move the value of `semantic-lex-end-point' to
after the location of the analyzed entry, and to add any discovered tokens
at the beginning of `semantic-lex-token-stream'.
This can be done by using `semantic-lex-push-token'.

\(fn NAME DOC CONDITION &rest FORMS)" nil (quote macro))

(autoload 'define-lex-regex-analyzer "semantic-lex" "\
Create a lexical analyzer with NAME and DOC that will match REGEXP.
FORMS are evaluated upon a successful match.
See `define-lex-analyzer' for more about analyzers.

\(fn NAME DOC REGEXP &rest FORMS)" nil (quote macro))

(autoload 'define-lex-simple-regex-analyzer "semantic-lex" "\
Create a lexical analyzer with NAME and DOC that match REGEXP.
TOKSYM is the symbol to use when creating a semantic lexical token.
INDEX is the index into the match that defines the bounds of the token.
Index should be a plain integer, and not specified in the macro as an
expression.
FORMS are evaluated upon a successful match BEFORE the new token is
created.  It is valid to ignore FORMS.
See `define-lex-analyzer' for more about analyzers.

\(fn NAME DOC REGEXP TOKSYM &optional INDEX &rest FORMS)" nil (quote macro))

(autoload 'define-lex-block-analyzer "semantic-lex" "\
Create a lexical analyzer NAME for paired delimiters blocks.
It detects a paired delimiters block or the corresponding open or
close delimiter depending on the value of the variable
`semantic-lex-current-depth'.  DOC is the documentation string of the lexical
analyzer.  SPEC1 and SPECS specify the token symbols and open, close
delimiters used.  Each SPEC has the form:

\(BLOCK-SYM (OPEN-DELIM OPEN-SYM) (CLOSE-DELIM CLOSE-SYM))

where BLOCK-SYM is the symbol returned in a block token.  OPEN-DELIM
and CLOSE-DELIM are respectively the open and close delimiters
identifying a block.  OPEN-SYM and CLOSE-SYM are respectively the
symbols returned in open and close tokens.

\(fn NAME DOC SPEC1 &rest SPECS)" nil (quote macro))

;;;***

;;;### (autoloads (semantic-lex-spp-write-utest semantic-lex-spp-table-write-slot-value)
;;;;;;  "semantic-lex-spp" "semantic-lex-spp.el" (18857 63844))
;;; Generated autoloads from semantic-lex-spp.el

(autoload 'semantic-lex-spp-table-write-slot-value "semantic-lex-spp" "\
Write out the VALUE of a slot for EIEIO.
The VALUE is a spp lexical table.

\(fn VALUE)" nil nil)

(autoload 'semantic-lex-spp-write-utest "semantic-lex-spp" "\
Unit test using the test spp file to test the slot write fcn.

\(fn)" t nil)

;;;***

;;;### (autoloads (semantic-default-make-setup) "semantic-make" "bovine/semantic-make.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from bovine/semantic-make.el

(autoload 'semantic-default-make-setup "semantic-make" "\
Set up a Makefile buffer for parsing with semantic.

\(fn)" nil nil)

(add-hook 'makefile-mode-hook 'semantic-default-make-setup)

;;;***

;;;### (autoloads (semantic-mru-bookmark-mode global-semantic-mru-bookmark-mode
;;;;;;  global-semantic-mru-bookmark-mode) "semantic-mru-bookmark"
;;;;;;  "semantic-mru-bookmark.el" (18857 63844))
;;; Generated autoloads from semantic-mru-bookmark.el

(autoload 'global-semantic-mru-bookmark-mode "semantic-mru-bookmark" "\
Toggle global use of option `semantic-mru-bookmark-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

(defvar global-semantic-mru-bookmark-mode nil "\
*If non-nil enable global use of variable `semantic-mru-bookmark-mode'.
When this mode is enabled, changes made to a buffer are highlighted
until the buffer is reparsed.")

(custom-autoload 'global-semantic-mru-bookmark-mode "semantic-mru-bookmark" nil)

(autoload 'semantic-mru-bookmark-mode "semantic-mru-bookmark" "\
Minor mode for tracking tag-based bookmarks automatically.
Tag based bookmarks a tracked based on editing and viewing habits
and can then be navigated via the MRU bookmark keymap.

\\{semantic-mru-bookmark-mode-map}

With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (semantic-regtest-cmp-results semantic-regtest-create-output
;;;;;;  semantic-regtest-run-test) "semantic-regtest" "semantic-regtest.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from semantic-regtest.el

(autoload 'semantic-regtest-run-test "semantic-regtest" "\
Not documented

\(fn)" t nil)

(autoload 'semantic-regtest-create-output "semantic-regtest" "\
Creates the test-output for the current buffer.
The user will be asked for the file-name of the created test-output-file (see
`semantic-regtest-create-output--internal').

\(fn)" t nil)

(autoload 'semantic-regtest-cmp-results "semantic-regtest" "\
Compare two test-outputs and create a suitable formatted result-file.

The user will be asked for four file-names:

   SOURCE-FILE: The underlying source-file for which the test-outputs have
   been created. If current buffer is a semantic-supported buffer then the
   file-name of the current buffer is offered as default.

   TEST-FILE: The regression-testoutput for SOURCE-FILE. It must be an already
   existing file which has been created by `semantic-regtest-create-output' or
   the function `semantic-regtest-create-output--internal'. If a file
   SOURCE-FILE.to exists already in current directory then this file is
   offered as default.

   REF-FILE: The reference testoutput for SOURCE-FILE. TEST-FILE will be
   compared against this file. It must be an already existing file which has
   been created by the command `semantic-regtest-create-output' or the
   function `semantic-regtest-create-output--internal'. If a file
   SOURCE-FILE.ro exists already in current directory then this file is
   offered as default.

   RESULT-FILE: That file will contain the comparisson-result generated by
   `semantic-regtest-cmp-results--internal'. Per default the filename
   SOURCE-FILE.res is offered.

This command calls `semantic-regtest-cmp-results--internal' with that four
file-names. See this function for details about the optional argument
`use-full-path-name' and a description of the format of RESULT-FILE.

\(fn &optional USE-FULL-PATH-NAME)" t nil)

;;;***

;;;### (autoloads (semantic-default-scheme-setup) "semantic-scm"
;;;;;;  "bovine/semantic-scm.el" (18857 63844))
;;; Generated autoloads from bovine/semantic-scm.el

(autoload 'semantic-default-scheme-setup "semantic-scm" "\
Setup hook function for Emacs Lisp files and Semantic.

\(fn)" nil nil)

(add-hook 'scheme-mode-hook 'semantic-default-scheme-setup)

;;;***

;;;### (autoloads (semantic-calculate-scope semantic-scope-tag-clone-with-scope
;;;;;;  semantic-scope-reset-cache) "semantic-scope" "semantic-scope.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from semantic-scope.el

(autoload 'semantic-scope-reset-cache "semantic-scope" "\
Get the current cached scope, and reset it.

\(fn)" nil nil)

(autoload 'semantic-scope-tag-clone-with-scope "semantic-scope" "\
Close TAG, and return it.  Add SCOPETAGS as a tag-local scope.
Stores the SCOPETAGS as a set of tag properties on the cloned tag.

\(fn TAG SCOPETAGS)" nil nil)

(autoload 'semantic-calculate-scope "semantic-scope" "\
Calculate the scope at POINT.
If POINT is not provided, then use the current location of `point'.
The class returned from the scope calculation is variable
`semantic-scope-cache'.

\(fn &optional POINT)" t nil)

;;;***

;;;### (autoloads (semantic-default-skel-setup) "semantic-skel" "bovine/semantic-skel.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from bovine/semantic-skel.el

(autoload 'semantic-default-skel-setup "semantic-skel" "\
Set up a buffer for semantic parsing of the skeleton language.

\(fn)" nil nil)

;;;***

;;;### (autoloads (semantic-tag-external-class semantic-tag-external-member-children
;;;;;;  semantic-tag-external-member-p semantic-tag-external-member-parent
;;;;;;  semantic-adopt-external-members semantic-bucketize semantic-flatten-tags-table
;;;;;;  semantic-unique-tag-table semantic-unique-tag-table-by-name
;;;;;;  semantic-sort-tags-by-name-then-type-decreasing semantic-sort-tags-by-name-then-type-increasing
;;;;;;  semantic-sort-tags-by-type-decreasing-ci semantic-sort-tags-by-type-increasing-ci
;;;;;;  semantic-sort-tags-by-name-decreasing-ci semantic-sort-tags-by-name-increasing-ci
;;;;;;  semantic-sort-tags-by-type-decreasing semantic-sort-tags-by-type-increasing
;;;;;;  semantic-sort-tags-by-name-decreasing semantic-sort-tags-by-name-increasing)
;;;;;;  "semantic-sort" "semantic-sort.el" (18857 63844))
;;; Generated autoloads from semantic-sort.el

(autoload 'semantic-sort-tags-by-name-increasing "semantic-sort" "\
Sort TAGS by name in increasing order with side effects.
Return the sorted list.

\(fn TAGS)" nil nil)

(autoload 'semantic-sort-tags-by-name-decreasing "semantic-sort" "\
Sort TAGS by name in decreasing order with side effects.
Return the sorted list.

\(fn TAGS)" nil nil)

(autoload 'semantic-sort-tags-by-type-increasing "semantic-sort" "\
Sort TAGS by type in increasing order with side effects.
Return the sorted list.

\(fn TAGS)" nil nil)

(autoload 'semantic-sort-tags-by-type-decreasing "semantic-sort" "\
Sort TAGS by type in decreasing order with side effects.
Return the sorted list.

\(fn TAGS)" nil nil)

(autoload 'semantic-sort-tags-by-name-increasing-ci "semantic-sort" "\
Sort TAGS by name in increasing order with side effects.
Return the sorted list.

\(fn TAGS)" nil nil)

(autoload 'semantic-sort-tags-by-name-decreasing-ci "semantic-sort" "\
Sort TAGS by name in decreasing order with side effects.
Return the sorted list.

\(fn TAGS)" nil nil)

(autoload 'semantic-sort-tags-by-type-increasing-ci "semantic-sort" "\
Sort TAGS by type in increasing order with side effects.
Return the sorted list.

\(fn TAGS)" nil nil)

(autoload 'semantic-sort-tags-by-type-decreasing-ci "semantic-sort" "\
Sort TAGS by type in decreasing order with side effects.
Return the sorted list.

\(fn TAGS)" nil nil)

(autoload 'semantic-sort-tags-by-name-then-type-increasing "semantic-sort" "\
Sort TAGS by name, then type in increasing order with side effects.
Return the sorted list.

\(fn TAGS)" nil nil)

(autoload 'semantic-sort-tags-by-name-then-type-decreasing "semantic-sort" "\
Sort TAGS by name, then type in increasing order with side effects.
Return the sorted list.

\(fn TAGS)" nil nil)

(autoload 'semantic-unique-tag-table-by-name "semantic-sort" "\
Scan a list of TAGS, removing duplicate names.
This must first sort the tags by name alphabetically ascending.
For more complex uniqueness testing used by the semanticdb
typecaching system, see `semanticdb-typecache-merge-streams'.

\(fn TAGS)" nil nil)

(autoload 'semantic-unique-tag-table "semantic-sort" "\
Scan a list of TAGS, removing duplicates.
This must first sort the tags by position ascending.
TAGS are removed only if they are equivalent, as can happen when
multiple tag sources are scanned.
For more complex uniqueness testing used by the semanticdb
typecaching system, see `semanticdb-typecache-merge-streams'.

\(fn TAGS)" nil nil)

(autoload 'semantic-flatten-tags-table "semantic-sort" "\
Flatten the tags table TABLE.
All tags in TABLE, and all components of top level tags
in TABLE will appear at the top level of list.
Tags promoted to the top of the list will still appear
unmodified as components of their parent tags.

\(fn &optional TABLE)" nil nil)

(autoload 'semantic-bucketize "semantic-sort" "\
Sort TAGS into a group of buckets based on tag class.
Unknown classes are placed in a Misc bucket.
Type bucket names are defined by either `semantic-symbol->name-assoc-list'.
If PARENT is specified, then TAGS belong to this PARENT in some way.
This will use `semantic-symbol->name-assoc-list-for-type-parts' to
generate bucket names.
Optional argument FILTER is a filter function to be applied to each bucket.
The filter function will take one argument, which is a list of tokens, and
may re-organize the list with side-effects.

\(fn TAGS &optional PARENT FILTER)" nil nil)

(defvar semantic-orphaned-member-metaparent-type "class" "\
In `semantic-adopt-external-members', the type of 'type for metaparents.
A metaparent is a made-up type semantic token used to hold the child list
of orphaned members of a named type.")

(autoload 'semantic-adopt-external-members "semantic-sort" "\
Rebuild TAGS so that externally defined members are regrouped.
Some languages such as C++ and CLOS permit the declaration of member
functions outside the definition of the class.  It is easier to study
the structure of a program when such methods are grouped together
more logically.

This function uses `semantic-tag-external-member-p' to
determine when a potential child is an externally defined member.

Note: Applications which use this function must account for token
types which do not have a position, but have children which *do*
have positions.

Applications should use `semantic-mark-external-member-function'
to modify all tags which are found as externally defined to some
type.  For example, changing the token type for generating extra
buckets with the bucket function.

\(fn TAGS)" nil nil)

(autoload 'semantic-tag-external-member-parent "semantic-sort" "\
Return a parent for TAG when TAG is an external member.
TAG is an external member if it is defined at a toplevel and
has some sort of label defining a parent.  The parent return will
be a string.

The default behavior, if not overridden with
`tag-member-parent' gets the 'parent extra
specifier of TAG.

If this function is overridden, use
`semantic-tag-external-member-parent-default' to also
include the default behavior, and merely extend your own.

\(fn TAG)" nil nil)

(autoload 'semantic-tag-external-member-p "semantic-sort" "\
Return non-nil if PARENT is the parent of TAG.
TAG is an external member of PARENT when it is somehow tagged
as having PARENT as it's parent.
PARENT and TAG must both be semantic tags.

The default behavior, if not overridden with
`tag-external-member-p' is to match :parent attribute in
the name of TAG.

If this function is overridden, use
`semantic-tag-external-member-children-p-default' to also
include the default behavior, and merely extend your own.

\(fn PARENT TAG)" nil nil)

(autoload 'semantic-tag-external-member-children "semantic-sort" "\
Return the list of children which are not *in* TAG.
If optional argument USEDB is non-nil, then also search files in
the Semantic Database.  If USEDB is a list of databases, search those
databases.

Children in this case are functions or types which are members of
TAG, such as the parts of a type, but which are not defined inside
the class.  C++ and CLOS both permit methods of a class to be defined
outside the bounds of the class' definition.

The default behavior, if not overridden with
`tag-external-member-children' is to search using
`semantic-tag-external-member-p' in all top level definitions
with a parent of TAG.

If this function is overridden, use
`semantic-tag-external-member-children-default' to also
include the default behavior, and merely extend your own.

\(fn TAG &optional USEDB)" nil nil)

(autoload 'semantic-tag-external-class "semantic-sort" "\
Return a list of real tags that faux TAG might represent.

In some languages, a method can be defined on an object which is
not in the same file.  In this case,
`semantic-adopt-external-members' will create a faux-tag.  If it
is necessary to get the tag from which for faux TAG was most
likely derived, then this function is needed.

\(fn TAG)" nil nil)

;;;***

;;;### (autoloads (semantic-symref-find-text semantic-symref-find-file-references-by-name
;;;;;;  semantic-symref-find-tags-by-completion semantic-symref-find-tags-by-regexp
;;;;;;  semantic-symref-find-tags-by-name semantic-symref-find-references-by-name)
;;;;;;  "semantic-symref" "symref/semantic-symref.el" (18857 63844))
;;; Generated autoloads from symref/semantic-symref.el

(autoload 'semantic-symref-find-references-by-name "semantic-symref" "\
Find a list of references to NAME in the current project.
Optional SCOPE specifies which file set to search.  Defaults to 'project.
Refers to `semantic-symref-tool', to determine the reference tool to use
for the current buffer.
Returns an object of class `semantic-symref-result'.
TOOL-RETURN is an optional symbol, which will be assigned the tool used
to perform the search.  This was added for use by a test harness.

\(fn NAME &optional SCOPE TOOL-RETURN)" t nil)

(autoload 'semantic-symref-find-tags-by-name "semantic-symref" "\
Find a list of references to NAME in the current project.
Optional SCOPE specifies which file set to search.  Defaults to 'project.
Refers to `semantic-symref-tool', to determine the reference tool to use
for the current buffer.
Returns an object of class `semantic-symref-result'.

\(fn NAME &optional SCOPE)" t nil)

(autoload 'semantic-symref-find-tags-by-regexp "semantic-symref" "\
Find a list of references to NAME in the current project.
Optional SCOPE specifies which file set to search.  Defaults to 'project.
Refers to `semantic-symref-tool', to determine the reference tool to use
for the current buffer.
Returns an object of class `semantic-symref-result'.

\(fn NAME &optional SCOPE)" t nil)

(autoload 'semantic-symref-find-tags-by-completion "semantic-symref" "\
Find a list of references to NAME in the current project.
Optional SCOPE specifies which file set to search.  Defaults to 'project.
Refers to `semantic-symref-tool', to determine the reference tool to use
for the current buffer.
Returns an object of class `semantic-symref-result'.

\(fn NAME &optional SCOPE)" t nil)

(autoload 'semantic-symref-find-file-references-by-name "semantic-symref" "\
Find a list of references to NAME in the current project.
Optional SCOPE specifies which file set to search.  Defaults to 'project.
Refers to `semantic-symref-tool', to determine the reference tool to use
for the current buffer.
Returns an object of class `semantic-symref-result'.

\(fn NAME &optional SCOPE)" t nil)

(autoload 'semantic-symref-find-text "semantic-symref" "\
Find a list of occurances of TEXT in the current project.
TEXT is a regexp formatted for use with egrep.
Optional SCOPE specifies which file set to search.  Defaults to 'project.
Refers to `semantic-symref-tool', to determine the reference tool to use
for the current buffer.
Returns an object of class `semantic-symref-result'.

\(fn TEXT &optional SCOPE)" t nil)

;;;***

;;;### (autoloads nil "semantic-symref-cscope" "symref/semantic-symref-cscope.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from symref/semantic-symref-cscope.el

(eieio-defclass-autoload 'semantic-symref-tool-cscope '(semantic-symref-tool-baseclass) "semantic-symref-cscope" "A symref tool implementation using CScope.\nThe CScope command can be used to generate lists of tags in a way\nsimilar to that of `grep'.  This tool will parse the output to generate\nthe hit list.\n\nSee the function `cedet-cscope-search' for more details.")

;;;***

;;;### (autoloads nil "semantic-symref-global" "symref/semantic-symref-global.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from symref/semantic-symref-global.el

(eieio-defclass-autoload 'semantic-symref-tool-global '(semantic-symref-tool-baseclass) "semantic-symref-global" "A symref tool implementation using GNU Global.\nThe GNU Global command can be used to generate lists of tags in a way\nsimilar to that of `grep'.  This tool will parse the output to generate\nthe hit list.\n\nSee the function `cedet-gnu-global-search' for more details.")

;;;***

;;;### (autoloads nil "semantic-symref-grep" "symref/semantic-symref-grep.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from symref/semantic-symref-grep.el

(eieio-defclass-autoload 'semantic-symref-tool-grep '(semantic-symref-tool-baseclass) "semantic-symref-grep" "A symref tool implementation using grep.\nThis tool uses EDE to find he root of the project, then executes\nfind-grep in the project.  The output is parsed for hits\nand those hits returned.")

;;;***

;;;### (autoloads nil "semantic-symref-idutils" "symref/semantic-symref-idutils.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from symref/semantic-symref-idutils.el

(eieio-defclass-autoload 'semantic-symref-tool-idutils '(semantic-symref-tool-baseclass) "semantic-symref-idutils" "A symref tool implementation using ID Utils.\nThe udutils command set can be used to generate lists of tags in a way\nsimilar to that of `grep'.  This tool will parse the output to generate\nthe hit list.\n\nSee the function `cedet-idutils-search' for more details.")

;;;***

;;;### (autoloads (semantic-symref-results-mode semantic-symref-symbol
;;;;;;  semantic-symref) "semantic-symref-list" "symref/semantic-symref-list.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from symref/semantic-symref-list.el

(autoload 'semantic-symref "semantic-symref-list" "\
Find references to the current tag.
This command uses the currently configured references tool within the
current project to find references to the current tag. The
references are the organized by file and the name of the function
they are used in.
Display the references in`semantic-symref-results-mode'

\(fn)" t nil)

(autoload 'semantic-symref-symbol "semantic-symref-list" "\
Find references to the symbol SYM.
This command uses the currently configured references tool within the
current project to find references to the input SYM. The
references are the organized by file and the name of the function
they are used in.
Display the references in`semantic-symref-results-mode'

\(fn SYM)" t nil)

(autoload 'semantic-symref-results-mode "semantic-symref-list" "\
Major-mode for displaying Semantic Symbol Reference RESULTS.
RESULTS is an object of class `semantic-symref-results'.

\(fn RESULTS)" t nil)

;;;***

;;;### (autoloads (semantic-insert-foreign-tag semantic-obtain-foreign-tag
;;;;;;  semantic-tag-components-with-overlays semantic-tag-components
;;;;;;  semantic-tag-alias-definition) "semantic-tag" "semantic-tag.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from semantic-tag.el

(defsubst semantic-tag-p (tag) "\
Return non-nil if TAG is most likely a semantic tag." (condition-case nil (and (consp tag) (stringp (car tag)) (symbolp (nth 1 tag)) (nth 1 tag) (listp (nth 2 tag)) (listp (nth 3 tag))) (error nil)))

(autoload 'semantic-tag-alias-definition "semantic-tag" "\
Return the definition TAG is an alias.
The returned value is a tag of the class that
`semantic-tag-alias-class' returns for TAG.
The default is to return the value of the :definition attribute.
Return nil if TAG is not of class 'alias.

\(fn TAG)" nil nil)

(autoload 'semantic-tag-components "semantic-tag" "\
Return a list of components for TAG.
A Component is a part of TAG which itself may be a TAG.
Examples include the elements of a structure in a 
tag of class `type, or the list of arguments to a
tag of class 'function.

\(fn TAG)" nil nil)

(autoload 'semantic-tag-components-with-overlays "semantic-tag" "\
Return the list of top level components belonging to TAG.
Children are any sub-tags which contain overlays.

Default behavior is to get `semantic-tag-components' in addition
to the components of an anonymous types (if applicable.)

Note for language authors:
  If a mode defines a language tag that has tags in it with overlays
you should still return them with this function.
Ignoring this step will prevent several features from working correctly.

\(fn TAG)" nil nil)

(autoload 'semantic-obtain-foreign-tag "semantic-tag" "\
Obtain a foreign tag from TAG.
TAG defaults to the tag at point in current buffer.
Return the obtained foreign tag or nil if failed.

\(fn &optional TAG)" nil nil)

(autoload 'semantic-insert-foreign-tag "semantic-tag" "\
Insert FOREIGN-TAG into the current buffer.
Signal an error if FOREIGN-TAG is not a valid foreign tag.
This function is overridable with the symbol `insert-foreign-tag'.

\(fn FOREIGN-TAG)" nil nil)

;;;***

;;;### (autoloads (semantic-prototype-file semantic-dependency-tag-file
;;;;;;  semantic-go-to-tag) "semantic-tag-file" "semantic-tag-file.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from semantic-tag-file.el

(autoload 'semantic-go-to-tag "semantic-tag-file" "\
Go to the location of TAG.
TAG may be a stripped element, in which case PARENT specifies a
parent tag that has position information.
PARENT can also be a `semanticdb-table' object.

\(fn TAG &optional PARENT)" nil nil)

(autoload 'semantic-dependency-tag-file "semantic-tag-file" "\
Find the filename represented from TAG.
Depends on `semantic-dependency-include-path' for searching.  Always searches
`.' first, then searches additional paths.

\(fn &optional TAG)" nil nil)

(autoload 'semantic-prototype-file "semantic-tag-file" "\
Return a file in which prototypes belonging to BUFFER should be placed.
Default behavior (if not overridden) looks for a token specifying the
prototype file, or the existence of an EDE variable indicating which
file prototypes belong in.

\(fn BUFFER)" nil nil)

;;;***

;;;### (autoloads (semantic-tag-full-name semantic-tag-prototype-p
;;;;;;  semantic-tag-static-p semantic-tag-leaf-p semantic-tag-abstract-p
;;;;;;  semantic-tag-protected-p semantic-tag-protection semantic-tag-calculate-parent)
;;;;;;  "semantic-tag-ls" "semantic-tag-ls.el" (18857 63844))
;;; Generated autoloads from semantic-tag-ls.el

(autoload 'semantic-tag-calculate-parent "semantic-tag-ls" "\
Attempt to calculate the parent of TAG.
The default behavior (if not overriden with `tag-calculate-parent')
is to search a buffer found with TAG, and if externally defined,
search locally, then semanticdb for that tag (when enabled.)

\(fn TAG)" nil nil)

(autoload 'semantic-tag-protection "semantic-tag-ls" "\
Return protection information about TAG with optional PARENT.
This function returns on of the following symbols:
   nil        - No special protection.  Language dependent.
   'public    - Anyone can access this TAG.
   'private   - Only methods in the local scope can access TAG.
   'protected - Like private for outside scopes, like public for child
                classes.
Some languages may choose to provide additional return symbols specific
to themselves.  Use of this function should allow for this.

The default behavior (if not overridden with `tag-protection'
is to return a symbol based on type modifiers.

\(fn TAG &optional PARENT)" nil nil)

(autoload 'semantic-tag-protected-p "semantic-tag-ls" "\
Non-nil if TAG is is protected.
PROTECTION is a symbol which can be returned by the method
`semantic-tag-protection'.
PARENT is the parent data type which contains TAG.

For these PROTECTIONs, true is returned if TAG is:
@table @asis
@item nil
  Always true
@item  private
  True if nil.
@item protected
  True if private or nil.
@item public
  True if private, protected, or nil.
@end table

\(fn TAG PROTECTION &optional PARENT)" nil nil)

(autoload 'semantic-tag-abstract-p "semantic-tag-ls" "\
Return non nil if TAG is abstract.
Optional PARENT is the parent tag of TAG.
In UML, abstract methods and classes have special meaning and behavior
in how methods are overridden.  In UML, abstract methods are italicized.

The default behavior (if not overridden with `tag-abstract-p'
is to return true if `abstract' is in the type modifiers.

\(fn TAG &optional PARENT)" nil nil)

(autoload 'semantic-tag-leaf-p "semantic-tag-ls" "\
Return non nil if TAG is leaf.
Optional PARENT is the parent tag of TAG.
In UML, leaf methods and classes have special meaning and behavior.

The default behavior (if not overridden with `tag-leaf-p'
is to return true if `leaf' is in the type modifiers.

\(fn TAG &optional PARENT)" nil nil)

(autoload 'semantic-tag-static-p "semantic-tag-ls" "\
Return non nil if TAG is static.
Optional PARENT is the parent tag of TAG.
In UML, static methods and attributes mean that they are allocated
in the parent class, and are not instance specific.
UML notation specifies that STATIC entries are underlined.

\(fn TAG &optional PARENT)" nil nil)

(autoload 'semantic-tag-prototype-p "semantic-tag-ls" "\
Return non nil if TAG is a prototype.
For some laguages, such as C, a prototype is a declaration of
something without an implementation.

\(fn TAG)" nil nil)

(autoload 'semantic-tag-full-name "semantic-tag-ls" "\
Return the fully qualified name of TAG in the package hierarchy.
STREAM-OR-BUFFER can be anything convertable by `semantic-something-to-stream',
but must be a toplevel semantic tag stream that contains TAG.
A Package Hierarchy is defined in UML by the way classes and methods
are organized on disk.  Some language use this concept such that a
class can be accessed via it's fully qualified name, (such as Java.)
Other languages qualify names within a Namespace (such as C++) which
result in a different package like structure.  Languages which do not
override this function with `tag-full-name' will use
`semantic-tag-name'.  Override functions only need to handle
STREAM-OR-BUFFER with a tag stream value, or nil.

\(fn TAG &optional STREAM-OR-BUFFER)" nil nil)

;;;***

;;;### (autoloads (semantic-tag-write-list-slot-value semantic-tag-write-tag-list)
;;;;;;  "semantic-tag-write" "semantic-tag-write.el" (18857 63844))
;;; Generated autoloads from semantic-tag-write.el

(autoload 'semantic-tag-write-tag-list "semantic-tag-write" "\
Write the tag list TLIST to the current stream.
INDENT indicates the current indentation level.
If optional DONTADDNEWLINE is non-nil, then don't add a newline.

\(fn TLIST &optional INDENT DONTADDNEWLINE)" nil nil)

(autoload 'semantic-tag-write-list-slot-value "semantic-tag-write" "\
Write out the VALUE of a slot for EIEIO.
The VALUE is a list of tags.

\(fn VALUE)" nil nil)

;;;***

;;;### (autoloads (semantic-default-texi-setup) "semantic-texi" "semantic-texi.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from semantic-texi.el

(autoload 'semantic-default-texi-setup "semantic-texi" "\
Set up a buffer for parsing of Texinfo files.

\(fn)" nil nil)

(add-hook 'texinfo-mode-hook 'semantic-default-texi-setup)

;;;***

;;;### (autoloads (semantic-utest-main) "semantic-utest" "semantic-utest.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from semantic-utest.el

(autoload 'semantic-utest-main "semantic-utest" "\
Not documented

\(fn)" t nil)

;;;***

;;;### (autoloads (semantic-utest-c) "semantic-utest-c" "semantic-utest-c.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from semantic-utest-c.el

(autoload 'semantic-utest-c "semantic-utest-c" "\
Run parsing test for C from the test directory.

\(fn)" t nil)

;;;***

;;;### (autoloads (semantic-highlight-func-mode global-semantic-highlight-func-mode
;;;;;;  global-semantic-highlight-func-mode semantic-stickyfunc-mode
;;;;;;  global-semantic-stickyfunc-mode global-semantic-stickyfunc-mode
;;;;;;  semantic-show-parser-state-mode global-semantic-show-parser-state-mode
;;;;;;  global-semantic-show-parser-state-mode semantic-show-unmatched-syntax-mode
;;;;;;  global-semantic-show-unmatched-syntax-mode global-semantic-show-unmatched-syntax-mode
;;;;;;  semantic-highlight-edits-mode global-semantic-highlight-edits-mode
;;;;;;  global-semantic-highlight-edits-mode) "semantic-util-modes"
;;;;;;  "semantic-util-modes.el" (18857 63844))
;;; Generated autoloads from semantic-util-modes.el

(autoload 'global-semantic-highlight-edits-mode "semantic-util-modes" "\
Toggle global use of option `semantic-highlight-edits-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

(defvar global-semantic-highlight-edits-mode nil "\
*If non-nil enable global use of variable `semantic-highlight-edits-mode'.
When this mode is enabled, changes made to a buffer are highlighted
until the buffer is reparsed.")

(custom-autoload 'global-semantic-highlight-edits-mode "semantic-util-modes" nil)

(autoload 'semantic-highlight-edits-mode "semantic-util-modes" "\
Minor mode for highlighting changes made in a buffer.
Changes are tracked by semantic so that the incremental parser can work
properly.
This mode will highlight those changes as they are made, and clear them
when the incremental parser accounts for those edits.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\(fn &optional ARG)" t nil)

(autoload 'global-semantic-show-unmatched-syntax-mode "semantic-util-modes" "\
Toggle global use of option `semantic-show-unmatched-syntax-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

(defvar global-semantic-show-unmatched-syntax-mode nil "\
*If non-nil, enable global use of `semantic-show-unmatched-syntax-mode'.
When this mode is enabled, syntax in the current buffer which the
semantic parser cannot match is highlighted with a red underline.")

(custom-autoload 'global-semantic-show-unmatched-syntax-mode "semantic-util-modes" nil)

(autoload 'semantic-show-unmatched-syntax-mode "semantic-util-modes" "\
Minor mode to highlight unmatched lexical syntax tokens.
When a parser executes, some elements in the buffer may not match any
parser rules.  These text characters are considered unmatched syntax.
Often time, the display of unmatched syntax can expose coding
problems before the compiler is run.

With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{semantic-show-unmatched-syntax-mode-map}

\(fn &optional ARG)" t nil)

(defvar global-semantic-show-parser-state-mode nil "\
*If non-nil enable global use of `semantic-show-parser-state-mode'.
When enabled, the current parse state of the current buffer is displayed
in the mode line. See `semantic-show-parser-state-marker' for details
on what is displayed.")

(custom-autoload 'global-semantic-show-parser-state-mode "semantic-util-modes" nil)

(autoload 'global-semantic-show-parser-state-mode "semantic-util-modes" "\
Toggle global use of option `semantic-show-parser-state-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

(autoload 'semantic-show-parser-state-mode "semantic-util-modes" "\
Minor mode for displaying parser cache state in the modeline.
The cache can be in one of three states.  They are
Up to date, Partial reprase needed, and Full reparse needed.
The state is indicated in the modeline with the following characters:
 `-'  ->  The cache is up to date.
 `!'  ->  The cache requires a full update.
 `~'  ->  The cache needs to be incrementally parsed.
 `%'  ->  The cache is not currently parseable.
 `@'  ->  Auto-parse in progress (not set here.)
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\(fn &optional ARG)" t nil)

(autoload 'global-semantic-stickyfunc-mode "semantic-util-modes" "\
Toggle global use of option `semantic-stickyfunc-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

(defvar global-semantic-stickyfunc-mode nil "\
*If non-nil, enable global use of `semantic-stickyfunc-mode'.
This minor mode only works for Emacs 21 or later.
When enabled, the header line is enabled, and the first line
of the current function or method is displayed in it.
This makes it appear that the first line of that tag is
`sticky' to the top of the window.")

(custom-autoload 'global-semantic-stickyfunc-mode "semantic-util-modes" nil)

(autoload 'semantic-stickyfunc-mode "semantic-util-modes" "\
Minor mode to show the title of a tag in the header line.
Enables/disables making the header line of functions sticky.
A function (or other tag class specified by
`semantic-stickyfunc-sticky-classes') has a header line, meaning the
first line which describes the rest of the construct.  This first
line is what is displayed in the Emacs 21 header line.

With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\(fn &optional ARG)" t nil)

(autoload 'global-semantic-highlight-func-mode "semantic-util-modes" "\
Toggle global use of option `semantic-highlight-func-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

(defvar global-semantic-highlight-func-mode nil "\
*If non-nil, enable global use of `semantic-highlight-func-mode'.
When enabled, the first line of the current tag is highlighted.")

(custom-autoload 'global-semantic-highlight-func-mode "semantic-util-modes" nil)

(autoload 'semantic-highlight-func-mode "semantic-util-modes" "\
Minor mode to highlight the first line of the current tag.
Enables/disables making the header line of functions sticky.
A function (or other tag class specified by
`semantic-stickfunc-sticky-classes') is highlighted, meaning the
first line which describes the rest of the construct.

See `semantic-stickfunc-mode' for putting a function in the
header line.  This mode recycles the stickyfunc configuration
classes list.

With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (semanticdb-file-stream semanticdb-file-table-object
;;;;;;  semanticdb-current-database) "semanticdb" "semanticdb.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from semanticdb.el

(autoload 'semanticdb-current-database "semanticdb" "\
Return the currently active database.

\(fn)" nil nil)

(autoload 'semanticdb-file-table-object "semanticdb" "\
Return a semanticdb table belonging to FILE, make it up to date.
If file has database tags available in the database, return it.
If file does not have tags available, and DONTLOAD is nil,
then load the tags for FILE, and create a new table object for it.
DONTLOAD does not affect the creation of new database objects.

\(fn FILE &optional DONTLOAD)" nil nil)

(autoload 'semanticdb-file-stream "semanticdb" "\
Return a list of tags belonging to FILE.
If file has database tags available in the database, return them.
If file does not have tags available, then load the file, and create them.

\(fn FILE)" nil nil)

;;;***

;;;### (autoloads (semanticdb-database-sanity-check semanticdb-table-sanity-check
;;;;;;  semanticdb-table-oob-sanity-check semanticdb-adebug-project-database-list
;;;;;;  semanticdb-adebug-current-table semanticdb-adebug-current-database
;;;;;;  semanticdb-dump-all-table-summary) "semanticdb-debug" "semanticdb-debug.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from semanticdb-debug.el

(autoload 'semanticdb-dump-all-table-summary "semanticdb-debug" "\
Dump a list of all databases in Emacs memory.

\(fn)" t nil)

(defalias 'semanticdb-adebug-database-list 'semanticdb-dump-all-table-summary)

(autoload 'semanticdb-adebug-current-database "semanticdb-debug" "\
Run ADEBUG on the current database.

\(fn)" t nil)

(autoload 'semanticdb-adebug-current-table "semanticdb-debug" "\
Run ADEBUG on the current database.

\(fn)" t nil)

(autoload 'semanticdb-adebug-project-database-list "semanticdb-debug" "\
Run ADEBUG on the current database.

\(fn)" t nil)

(autoload 'semanticdb-table-oob-sanity-check "semanticdb-debug" "\
Validate that CACHE tags do not have any overlays in them.

\(fn CACHE)" nil nil)

(autoload 'semanticdb-table-sanity-check "semanticdb-debug" "\
Validate the current semanticdb TABLE.

\(fn &optional TABLE)" t nil)

(autoload 'semanticdb-database-sanity-check "semanticdb-debug" "\
Validate the current semantic database.

\(fn)" t nil)

;;;***

;;;### (autoloads (semanticdb-ebrowse-load-helper semanticdb-load-ebrowse-caches
;;;;;;  semanticdb-create-ebrowse-database) "semanticdb-ebrowse"
;;;;;;  "semanticdb-ebrowse.el" (18857 63844))
;;; Generated autoloads from semanticdb-ebrowse.el

(autoload 'semanticdb-create-ebrowse-database "semanticdb-ebrowse" "\
Create an EBROSE database for directory DIR.
The database file is stored in ~/.semanticdb, or whichever directory
is specified by `semanticdb-default-save-directory'.

\(fn DIR)" t nil)

(autoload 'semanticdb-load-ebrowse-caches "semanticdb-ebrowse" "\
Load all semanticdb controlled EBROWSE caches.

\(fn)" t nil)

(autoload 'semanticdb-ebrowse-load-helper "semanticdb-ebrowse" "\
Create the semanticdb database via ebrowse for directory.
If DIRECTORY is found to be defunct, it won't load the DB, and will
warn instead.

\(fn DIRECTORY)" nil nil)

;;;***

;;;### (autoloads (semanticdb-enable-exuberent-ctags) "semanticdb-ectag"
;;;;;;  "ctags/semanticdb-ectag.el" (18857 63844))
;;; Generated autoloads from ctags/semanticdb-ectag.el

(autoload 'semanticdb-enable-exuberent-ctags "semanticdb-ectag" "\
Enable the use of exuberent ctags for out-of-buffer parsing for MODE.
MODE is a `major-mode' symbol used.
Throws an error if `semantic-ectag-program' is not of the correct
version needed by Semantic ctags support.

\(fn MODE)" t nil)

;;;***

;;;### (autoloads (semanticdb-full-filename semanticdb-live-p semanticdb-file-loaded-p
;;;;;;  semanticdb-persistent-path semanticdb-default-save-directory
;;;;;;  semanticdb-default-file-name) "semanticdb-file" "semanticdb-file.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from semanticdb-file.el

(defvar semanticdb-default-file-name "semantic.cache" "\
*File name of the semantic tag cache.")

(custom-autoload 'semanticdb-default-file-name "semanticdb-file" t)

(defvar semanticdb-default-save-directory (expand-file-name "~/.semanticdb") "\
*Directory name where semantic cache files are stored.
If this value is nil, files are saved in the current directory.  If the value
is a valid directory, then it overrides `semanticdb-default-file-name' and
stores caches in a coded file name in this directory.")

(custom-autoload 'semanticdb-default-save-directory "semanticdb-file" t)

(defvar semanticdb-persistent-path '(always) "\
*List of valid paths that semanticdb will cache tags to.
When `global-semanticdb-minor-mode' is active, tag lists will
be saved to disk when Emacs exits.  Not all directories will have
tags that should be saved.
The value should be a list of valid paths.  A path can be a string,
indicating a directory in which to save a variable.  An element in the
list can also be a symbol.  Valid symbols are `never', which will
disable any saving anywhere, `always', which enables saving
everywhere, or `project', which enables saving in any directory that
passes a list of predicates in `semanticdb-project-predicate-functions'.")

(custom-autoload 'semanticdb-persistent-path "semanticdb-file" t)

(eieio-defclass-autoload 'semanticdb-project-database-file '(semanticdb-project-database eieio-persistent) "semanticdb-file" "Database of file tables saved to disk.")

(autoload 'semanticdb-file-loaded-p "semanticdb-file" "\
Return the project belonging to FILENAME if it was already loaded.

\(fn FILENAME)" nil nil)

(autoload 'semanticdb-live-p "semanticdb-file" "\
Return non-nil if the file associated with OBJ is live.
Live databases are objects associated with existing directories.

\(fn (OBJ semanticdb-project-database))" nil nil)

(autoload 'semanticdb-full-filename "semanticdb-file" "\
Fetch the full filename that OBJ refers to.

\(fn (OBJ semanticdb-project-database-file))" nil nil)

;;;***

;;;### (autoloads (semanticdb-find-tags-external-children-of-type
;;;;;;  semanticdb-brute-find-tags-by-class semanticdb-brute-deep-find-tags-for-completion
;;;;;;  semanticdb-brute-deep-find-tags-by-name semanticdb-deep-find-tags-for-completion
;;;;;;  semanticdb-deep-find-tags-by-name-regexp semanticdb-deep-find-tags-by-name
;;;;;;  semanticdb-find-tags-by-class semanticdb-find-tags-for-completion
;;;;;;  semanticdb-find-tags-by-name-regexp semanticdb-find-tags-by-name
;;;;;;  semanticdb-find-tags-collector semanticdb-find-result-mapc
;;;;;;  semanticdb-find-result-nth-in-buffer semanticdb-find-result-nth
;;;;;;  semanticdb-find-result-with-nil-p semanticdb-find-results-p
;;;;;;  semanticdb-fast-strip-find-results semanticdb-strip-find-results
;;;;;;  semanticdb-find-adebug-scanned-includes semanticdb-find-test-translate-path
;;;;;;  semanticdb-find-table-for-include semanticdb-find-translate-path-default
;;;;;;  semanticdb-find-default-throttle) "semanticdb-find" "semanticdb-find.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from semanticdb-find.el

(defvar semanticdb-find-throttle-custom-list '(repeat (radio (const 'local) (const 'project) (const 'unloaded) (const 'system) (const 'recursive) (const 'omniscience))) "\
Customization values for semanticdb find throttle.
See `semanticdb-find-throttle' for details.")

(defvar semanticdb-find-default-throttle '(local project unloaded system recursive) "\
The default throttle for `semanticdb-find' routines.
The throttle controls how detailed the list of database
tables is for a symbol lookup.  The value is a list with
the following keys:
  `file'       - The file the search is being performed from.
                 This option is here for completeness only, and
                 is assumed to always be on.
  `local'      - Tables from the same local directory are included.
                 This includes files directly referenced by a file name
                 which might be in a different directory.
  `project'    - Tables from the same local project are included
                 If `project' is specified, then `local' is assumed.
  `unloaded'   - If a table is not in memory, load it.  If it is not cached
                 on disk either, get the source, parse it, and create
                 the table.
  `system'     - Tables from system databases.  These are specifically
                 tables from system header files, or language equivalent.
  `recursive'  - For include based searches, includes tables referenced
                 by included files.
  `omniscience' - Included system databases which are omniscience, or
                 somehow know everything.  Omniscience databases are found
                 in `semanticdb-project-system-databases'.
                 The Emacs Lisp system DB is an omniscience database.")

(custom-autoload 'semanticdb-find-default-throttle "semanticdb-find" t)

(autoload 'semanticdb-find-translate-path-default "semanticdb-find" "\
Translate PATH into a list of semantic tables.
If BRUTISH is non nil, return all tables associated with PATH.
Default action as described in `semanticdb-find-translate-path'.

\(fn PATH BRUTISH)" nil nil)

(autoload 'semanticdb-find-table-for-include "semanticdb-find" "\
For a single INCLUDETAG found in TABLE, find a `semanticdb-table' object
INCLUDETAG is a semantic TAG of class 'include.
TABLE is a semanticdb table that identifies where INCLUDETAG came from.
TABLE is optional if INCLUDETAG has an overlay of :filename attribute.

\(fn INCLUDETAG &optional TABLE)" nil nil)

(autoload 'semanticdb-find-test-translate-path "semanticdb-find" "\
Call and output results of `semanticdb-find-translate-path'.
With ARG non-nil, specify a BRUTISH translation.
See `semanticdb-find-default-throttle' and `semanticdb-project-roots'
for details on how this list is derived.

\(fn &optional ARG)" t nil)

(autoload 'semanticdb-find-adebug-scanned-includes "semanticdb-find" "\
Translate the current path, then display the lost includes.
Examines the variable `semanticdb-find-lost-includes'.

\(fn)" t nil)

(autoload 'semanticdb-strip-find-results "semanticdb-find" "\
Strip a semanticdb search RESULTS to exclude objects.
This makes it appear more like the results of a `semantic-find-' call.
Optional FIND-FILE-MATCH loads all files associated with RESULTS
into buffers.  This has the side effect of enabling `semantic-tag-buffer' to
return a value.
If FIND-FILE-MATCH is 'name, then only the filename is stored
in each tag instead of loading each file into a buffer.
If the input RESULTS are not going to be used again, and if
FIND-FILE-MATCH is nil, you can use `semanticdb-fast-strip-find-results'
instead.

\(fn RESULTS &optional FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-fast-strip-find-results "semanticdb-find" "\
Destructively strip a semanticdb search RESULTS to exclude objects.
This makes it appear more like the results of a `semantic-find-' call.
This is like `semanticdb-strip-find-results', except the input list RESULTS
will be changed.

\(fn RESULTS)" nil nil)

(autoload 'semanticdb-find-results-p "semanticdb-find" "\
Non-nil if RESULTP is in the form of a semanticdb search result.
This query only really tests the first entry in the list that is RESULTP,
but should be good enough for debugging assertions.

\(fn RESULTP)" nil nil)

(autoload 'semanticdb-find-result-with-nil-p "semanticdb-find" "\
Non-nil of RESULTP is in the form of a semanticdb search result.
nil is a valid value where a TABLE usually is, but only if the TAG
results include overlays.
This query only really tests the first entry in the list that is RESULTP,
but should be good enough for debugging assertions.

\(fn RESULTP)" nil nil)

(autoload 'semanticdb-find-result-nth "semanticdb-find" "\
In RESULT, return the Nth search result.
This is a 0 based search result, with the first match being element 0.

The returned value is a cons cell: (TAG . TABLE) where TAG
is the tag at the Nth position.  TABLE is the semanticdb table where
the TAG was found.  Sometimes TABLE can be nil.

\(fn RESULT N)" nil nil)

(autoload 'semanticdb-find-result-nth-in-buffer "semanticdb-find" "\
In RESULT, return the Nth search result.
Like `semanticdb-find-result-nth', except that only the TAG
is returned, and the buffer it is found it will be made current.
If the result tag has no position information, the originating buffer
is still made current.

\(fn RESULT N)" nil nil)

(autoload 'semanticdb-find-result-mapc "semanticdb-find" "\
Apply FCN to each element of find RESULT for side-effects only.
FCN takes two arguments.  The first is a TAG, and the
second is a DB from wence TAG originated.
Returns result.

\(fn FCN RESULT)" nil nil)

(autoload 'semanticdb-find-tags-collector "semanticdb-find" "\
Collect all tags returned by FUNCTION over PATH.
The FUNCTION must take two arguments.  The first is TABLE,
which is a semanticdb table containing tags.  The second argument
to FUNCTION is TAGS.  TAGS may be a list of tags.  If TAGS is non-nil, then
FUNCTION should search the TAG list, not through TABLE.

See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.

Note: You should leave FIND-FILE-MATCH as nil.  It is far more
efficient to take the results from any search and use
`semanticdb-strip-find-results' instead.  This argument is here
for backward compatibility.

If optional argument BRUTISH is non-nil, then ignore include statements,
and search all tables in this project tree.

\(fn FUNCTION &optional PATH FIND-FILE-MATCH BRUTISH)" nil nil)

(autoload 'semanticdb-find-tags-by-name "semanticdb-find" "\
Search for all tags matching NAME on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.

\(fn NAME &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-find-tags-by-name-regexp "semanticdb-find" "\
Search for all tags matching REGEXP on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.

\(fn REGEXP &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-find-tags-for-completion "semanticdb-find" "\
Search for all tags matching PREFIX on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.

\(fn PREFIX &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-find-tags-by-class "semanticdb-find" "\
Search for all tags of CLASS on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.

\(fn CLASS &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-deep-find-tags-by-name "semanticdb-find" "\
Search for all tags matching NAME on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.

\(fn NAME &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-deep-find-tags-by-name-regexp "semanticdb-find" "\
Search for all tags matching REGEXP on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.

\(fn REGEXP &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-deep-find-tags-for-completion "semanticdb-find" "\
Search for all tags matching PREFIX on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.

\(fn PREFIX &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-brute-deep-find-tags-by-name "semanticdb-find" "\
Search for all tags matching NAME on PATH.
See `semanticdb-find-translate-path' for details on PATH.
The argument BRUTISH will be set so that searching includes all tables
in the current project.
FIND-FILE-MATCH indicates that any time a matchi is found, the file
associated wit that tag should be loaded into a buffer.

\(fn NAME &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-brute-deep-find-tags-for-completion "semanticdb-find" "\
Search for all tags matching PREFIX on PATH.
See `semanticdb-find-translate-path' for details on PATH.
The argument BRUTISH will be set so that searching includes all tables
in the current project.
FIND-FILE-MATCH indicates that any time a matchi is found, the file
associated wit that tag should be loaded into a buffer.

\(fn PREFIX &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-brute-find-tags-by-class "semanticdb-find" "\
Search for all tags of CLASS on PATH.
See `semanticdb-find-translate-path' for details on PATH.
The argument BRUTISH will be set so that searching includes all tables
in the current project.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.

\(fn CLASS &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-find-tags-external-children-of-type "semanticdb-find" "\
Search for all tags defined outside of TYPE w/ TYPE as a parent.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.

\(fn TYPE &optional PATH FIND-FILE-MATCH)" nil nil)

;;;***

;;;### (autoloads (semanticdb-enable-gnu-global-databases) "semanticdb-global"
;;;;;;  "semanticdb-global.el" (18857 63844))
;;; Generated autoloads from semanticdb-global.el

(autoload 'semanticdb-enable-gnu-global-databases "semanticdb-global" "\
Enable the use of the GNU Global SemanticDB back end for all files of MODE.
This will add an instance of a GNU Global database to each buffer
in a GNU Global supported hierarchy.

\(fn MODE)" t nil)

;;;***

;;;### (autoloads (global-semanticdb-minor-mode semanticdb-minor-mode-p
;;;;;;  semanticdb-global-mode) "semanticdb-mode" "semanticdb-mode.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from semanticdb-mode.el

(defvar semanticdb-current-database nil "\
For a given buffer, this is the currently active database.")

(defvar semanticdb-global-mode nil "\
*If non-nil enable the use of `semanticdb-minor-mode'.")

(custom-autoload 'semanticdb-global-mode "semanticdb-mode" nil)

(autoload 'semanticdb-minor-mode-p "semanticdb-mode" "\
Return non-nil if `semanticdb-minor-mode' is active.

\(fn)" nil nil)

(autoload 'global-semanticdb-minor-mode "semanticdb-mode" "\
Toggle the use of `semanticdb-minor-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (semanticdb-add-reference) "semanticdb-ref" "semanticdb-ref.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from semanticdb-ref.el

(autoload 'semanticdb-add-reference "semanticdb-ref" "\
Add a reference for the database table DBT based on INCLUDE-TAG.
DBT is the database table that owns the INCLUDE-TAG.  The reference
will be added to the database that INCLUDE-TAG refers to.

\(fn (DBT semanticdb-abstract-table) INCLUDE-TAG)" nil nil)

;;;***

;;;### (autoloads (semanticdb-find-nonterminal-by-function semanticdb-find-nonterminal-by-extra-spec-value
;;;;;;  semanticdb-find-nonterminal-by-extra-spec semanticdb-find-nonterminal-by-property
;;;;;;  semanticdb-find-nonterminal-by-type semanticdb-find-nonterminal-by-name-regexp
;;;;;;  semanticdb-find-nonterminal-by-name semanticdb-find-nonterminal-by-token)
;;;;;;  "semanticdb-search" "semanticdb-search.el" (18857 63844))
;;; Generated autoloads from semanticdb-search.el

(autoload 'semanticdb-find-nonterminal-by-token "semanticdb-search" "\
OBSOLETE:
Find all occurances of nonterminals with token TOKEN in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES, DIFF-MODE, FIND-FILE-MATCH and IGNORE-SYSTEM.
Return a list ((DB-TABLE . TOKEN-LIST) ...).

\(fn TOKEN &optional DATABASES SEARCH-PARTS SEARCH-INCLUDES DIFF-MODE FIND-FILE-MATCH IGNORE-SYSTEM)" nil nil)

(autoload 'semanticdb-find-nonterminal-by-name "semanticdb-search" "\
OBSOLETE:
Find all occurances of nonterminals with name NAME in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES, DIFF-MODE, FIND-FILE-MATCH and IGNORE-SYSTEM.
Return a list ((DB-TABLE . TOKEN) ...).

\(fn NAME &optional DATABASES SEARCH-PARTS SEARCH-INCLUDES DIFF-MODE FIND-FILE-MATCH IGNORE-SYSTEM)" nil nil)

(autoload 'semanticdb-find-nonterminal-by-name-regexp "semanticdb-search" "\
OBSOLETE:
Find all occurances of nonterminals with name matching REGEX in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, FIND-FILE-MATCH and IGNORE-SYSTEM.
Return a list ((DB-TABLE . TOKEN-LIST) ...).

\(fn REGEX &optional DATABASES SEARCH-PARTS SEARCH-INCLUDES DIFF-MODE FIND-FILE-MATCH IGNORE-SYSTEM)" nil nil)

(autoload 'semanticdb-find-nonterminal-by-type "semanticdb-search" "\
OBSOLETE:
Find all nonterminals with a type of TYPE in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, FIND-FILE-MATCH and IGNORE-SYSTEM.
Return a list ((DB-TABLE . TOKEN-LIST) ...).

\(fn TYPE &optional DATABASES SEARCH-PARTS SEARCH-INCLUDES DIFF-MODE FIND-FILE-MATCH IGNORE-SYSTEM)" nil nil)

(autoload 'semanticdb-find-nonterminal-by-property "semanticdb-search" "\
OBSOLETE:
Find all nonterminals with a PROPERTY equal to VALUE in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, FIND-FILE-MATCH and IGNORE-SYSTEM.
Return a list ((DB-TABLE . TOKEN-LIST) ...).

\(fn PROPERTY VALUE &optional DATABASES SEARCH-PARTS SEARCH-INCLUDES DIFF-MODE FIND-FILE-MATCH IGNORE-SYSTEM)" nil nil)

(autoload 'semanticdb-find-nonterminal-by-extra-spec "semanticdb-search" "\
OBSOLETE:
Find all nonterminals with a SPEC in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, FIND-FILE-MATCH and IGNORE-SYSTEM.
Return a list ((DB-TABLE . TOKEN-LIST) ...).

\(fn SPEC &optional DATABASES SEARCH-PARTS SEARCH-INCLUDES DIFF-MODE FIND-FILE-MATCH IGNORE-SYSTEM)" nil nil)

(autoload 'semanticdb-find-nonterminal-by-extra-spec-value "semanticdb-search" "\
OBSOLETE:
Find all nonterminals with a SPEC equal to VALUE in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, FIND-FILE-MATCH and IGNORE-SYSTEM.
Return a list ((DB-TABLE . TOKEN-LIST) ...).

\(fn SPEC VALUE &optional DATABASES SEARCH-PARTS SEARCH-INCLUDES DIFF-MODE FIND-FILE-MATCH IGNORE-SYSTEM)" nil nil)

(autoload 'semanticdb-find-nonterminal-by-function "semanticdb-search" "\
OBSOLETE:
Find all occurances of nonterminals which match FUNCTION.
Search in all DATABASES.  If DATABASES is nil, search a range of
associated databases calculated `semanticdb-current-database-list' and
DATABASES is a list of variable `semanticdb-project-database' objects.
When SEARCH-PARTS is non-nil the search will include children of tags.
When SEARCH-INCLUDES is non-nil, the search will include dependency files.
When DIFF-MODE is non-nil, search databases which are of a different mode.
A Mode is the `major-mode' that file was in when it was last parsed.
When FIND-FILE-MATCH is non-nil, the make sure any found token's file is
in an Emacs buffer.
When IGNORE-SYSTEM is non-nil, system libraries are not searched.
Return a list ((DB-TABLE . TOKEN-OR-TOKEN-LIST) ...).

\(fn FUNCTION &optional DATABASES SEARCH-PARTS SEARCH-INCLUDES DIFF-MODE FIND-FILE-MATCH IGNORE-SYSTEM)" nil nil)

;;;***

;;;### (autoloads (semanticdb-db-typecache-dump semanticdb-typecache-dump
;;;;;;  semanticdb-typecache-refresh-for-buffer semanticdb-typecache-find
;;;;;;  semanticdb-typecache-merge-streams semanticdb-get-typecache
;;;;;;  semanticdb-get-typecache semanticdb-typecache-add-dependant
;;;;;;  semanticdb-typecache-notify-reset) "semanticdb-typecache"
;;;;;;  "semanticdb-typecache.el" (18857 63844))
;;; Generated autoloads from semanticdb-typecache.el

(eieio-defclass-autoload 'semanticdb-typecache 'nil "semanticdb-typecache" "Structure for maintaining a typecache.")

(autoload 'semanticdb-typecache-notify-reset "semanticdb-typecache" "\
Do a reset from a notify from a table we depend on.

\(fn (TC semanticdb-typecache))" nil nil)

(autoload 'semanticdb-typecache-add-dependant "semanticdb-typecache" "\
Add into the local typecache a dependant DEP.

\(fn DEP)" nil nil)

(autoload 'semanticdb-get-typecache "semanticdb-typecache" "\
Retrieve the typecache from the semanticdb TABLE.
If there is no table, create one, and fill it in.

\(fn (TABLE semanticdb-abstract-table))" nil nil)

(eieio-defclass-autoload 'semanticdb-database-typecache '(semanticdb-abstract-db-cache) "semanticdb-typecache" "Structure for maintaining a typecache.")

(autoload 'semanticdb-get-typecache "semanticdb-typecache" "\
Retrieve the typecache from the semantic database DB.
If there is no table, create one, and fill it in.

\(fn (DB semanticdb-project-database))" nil nil)

(autoload 'semanticdb-typecache-merge-streams "semanticdb-typecache" "\
Merge into CACHE1 and CACHE2 together.  The Caches will be merged in place.

\(fn CACHE1 CACHE2)" nil nil)

(autoload 'semanticdb-typecache-find "semanticdb-typecache" "\
Search the typecache for TYPE in PATH.
If type is a string, split the string, and search for the parts.
If type is a list, treat the type as a pre-split string.
PATH can be nil for the current buffer, or a semanticdb table.
FIND-FILE-MATCH is non-nil to force all found tags to be loaded into a buffer.

\(fn TYPE &optional PATH FIND-FILE-MATCH)" nil nil)

(autoload 'semanticdb-typecache-refresh-for-buffer "semanticdb-typecache" "\
Refresh the typecache for BUFFER.

\(fn BUFFER)" nil nil)

(autoload 'semanticdb-typecache-dump "semanticdb-typecache" "\
Dump the typecache for the current buffer.

\(fn)" t nil)

(autoload 'semanticdb-db-typecache-dump "semanticdb-typecache" "\
Dump the typecache for the current buffer's database.

\(fn)" t nil)

;;;***

;;;### (autoloads (senator-try-expand-semantic global-senator-minor-mode
;;;;;;  senator-minor-mode senator-word-search-backward senator-re-search-backward
;;;;;;  senator-search-backward senator-word-search-forward senator-re-search-forward
;;;;;;  senator-search-forward senator-completion-menu-popup senator-complete-symbol
;;;;;;  senator-jump-regexp senator-jump senator-previous-tag senator-next-tag
;;;;;;  senator-step-at-start-end-tag-classes senator-step-at-tag-classes
;;;;;;  global-senator-minor-mode) "senator" "senator.el" (18857
;;;;;;  63844))
;;; Generated autoloads from senator.el

(defvar global-senator-minor-mode nil "\
*If non-nil enable global use of senator minor mode.")

(custom-autoload 'global-senator-minor-mode "senator" nil)

(defvar senator-step-at-tag-classes nil "\
*List of tag classes where to step.
A tag class is a symbol like 'variable, 'function, 'type, or other.
If nil navigation steps at any tag found.  This is a buffer local
variable.  It can be set in a mode hook to get a specific langage
navigation.")

(custom-autoload 'senator-step-at-tag-classes "senator" t)

(defvar senator-step-at-start-end-tag-classes '(function) "\
*List of tag classes where to step at start and end.
A tag class is a symbol like 'variable, 'function, 'type, or other.
If nil, navigation only step at beginning of tags.  If t, step at
start and end of any tag where it is allowed to step.  Also, stepping
at start and end of a tag prevent stepping inside its components.
This is a buffer local variable.  It can be set in a mode hook to get
a specific langage navigation.")

(custom-autoload 'senator-step-at-start-end-tag-classes "senator" t)

(autoload 'senator-next-tag "senator" "\
Navigate to the next Semantic tag.
Return the tag or nil if at end of buffer.

\(fn)" t nil)

(autoload 'senator-previous-tag "senator" "\
Navigate to the previous Semantic tag.
Return the tag or nil if at beginning of buffer.

\(fn)" t nil)

(autoload 'senator-jump "senator" "\
Jump to the semantic symbol SYM.

If optional IN-CONTEXT is non-nil jump in the local type's context
\(see function `senator-current-type-context').  If optional
NO-DEFAULT is non-nil do not provide a default value.

When called interactively you can combine the IN-CONTEXT and
NO-DEFAULT switches like this:

- \\[universal-argument]       IN-CONTEXT.
- \\[universal-argument] -     NO-DEFAULT.
- \\[universal-argument] \\[universal-argument]   IN-CONTEXT + NO-DEFAULT.

\(fn SYM &optional IN-CONTEXT NO-DEFAULT)" t nil)

(autoload 'senator-jump-regexp "senator" "\
Jump to the semantic symbol SYMREGEX.
SYMREGEX is treated as a regular expression.

If optional IN-CONTEXT is non-nil jump in the local type's context
\(see function `senator-current-type-context').  If optional
NO-DEFAULT is non-nil do not provide a default value and move to the
next match of SYMREGEX.  NOTE: Doesn't actually work yet.

When called interactively you can combine the IN-CONTEXT and
NO-DEFAULT switches like this:

- \\[universal-argument]       IN-CONTEXT.
- \\[universal-argument] -     NO-DEFAULT.
- \\[universal-argument] \\[universal-argument]   IN-CONTEXT + NO-DEFAULT.

\(fn SYMREGEX &optional IN-CONTEXT NO-DEFAULT)" t nil)

(autoload 'senator-complete-symbol "senator" "\
Complete the current symbol under point.
If optional argument CYCLE-ONCE is non-nil, only cycle through the list
of completions once, doing nothing where there are no more matches.

\(fn &optional CYCLE-ONCE)" t nil)

(autoload 'senator-completion-menu-popup "senator" "\
Popup a completion menu for the symbol at point.
The popup menu displays all of the possible completions for the symbol
it was invoked on.  To automatically split large menus this function
use `imenu--mouse-menu' to handle the popup menu.

\(fn)" t nil)

(autoload 'senator-search-forward "senator" "\
Search in tag names forward from point for STRING.
Set point to the end of the occurrence found, and return point.
See also the function `search-forward' for details on the BOUND,
NOERROR and COUNT arguments.

\(fn STRING &optional BOUND NOERROR COUNT)" t nil)

(autoload 'senator-re-search-forward "senator" "\
Search in tag names forward from point for regular expression REGEXP.
Set point to the end of the occurrence found, and return point.
See also the function `re-search-forward' for details on the BOUND,
NOERROR and COUNT arguments.

\(fn REGEXP &optional BOUND NOERROR COUNT)" t nil)

(autoload 'senator-word-search-forward "senator" "\
Search in tag names forward from point for WORD.
Set point to the end of the occurrence found, and return point.
See also the function `word-search-forward' for details on the BOUND,
NOERROR and COUNT arguments.

\(fn WORD &optional BOUND NOERROR COUNT)" t nil)

(autoload 'senator-search-backward "senator" "\
Search in tag names backward from point for STRING.
Set point to the beginning of the occurrence found, and return point.
See also the function `search-backward' for details on the BOUND,
NOERROR and COUNT arguments.

\(fn STRING &optional BOUND NOERROR COUNT)" t nil)

(autoload 'senator-re-search-backward "senator" "\
Search in tag names backward from point for regular expression REGEXP.
Set point to the beginning of the occurrence found, and return point.
See also the function `re-search-backward' for details on the BOUND,
NOERROR and COUNT arguments.

\(fn REGEXP &optional BOUND NOERROR COUNT)" t nil)

(autoload 'senator-word-search-backward "senator" "\
Search in tag names backward from point for WORD.
Set point to the beginning of the occurrence found, and return point.
See also the function `word-search-backward' for details on the BOUND,
NOERROR and COUNT arguments.

\(fn WORD &optional BOUND NOERROR COUNT)" t nil)

(autoload 'senator-minor-mode "senator" "\
Toggle senator minor mode.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{senator-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'global-senator-minor-mode "senator" "\
Toggle global use of senator minor mode.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

(autoload 'senator-try-expand-semantic "senator" "\
Attempt inline completion at the cursor.
Use Semantic, or the semantic database to look up possible
completions.  The argument OLD has to be nil the first call of this
function.  It returns t if a unique, possibly partial, completion is
found, nil otherwise.

\(fn OLD)" nil nil)

;;;***

;;;### (autoloads (wisent-parse-toggle-verbose-flag) "wisent" "wisent/wisent.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from wisent/wisent.el

(defvar wisent-parse-verbose-flag nil "\
*Non-nil means to issue more messages while parsing.")

(autoload 'wisent-parse-toggle-verbose-flag "wisent" "\
Toggle whether to issue more messages while parsing.

\(fn)" t nil)

;;;***

;;;### (autoloads (wisent-byte-compile-grammar wisent-compile-grammar
;;;;;;  wisent-toggle-verbose-flag) "wisent-comp" "wisent/wisent-comp.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from wisent/wisent-comp.el

(defvar wisent-verbose-flag nil "\
*Non-nil means to report verbose information on generated parser.")

(autoload 'wisent-toggle-verbose-flag "wisent-comp" "\
Toggle whether to report verbose information on generated parser.

\(fn)" t nil)

(autoload 'wisent-compile-grammar "wisent-comp" "\
Compile the LALR(1) GRAMMAR.

GRAMMAR is a list (TOKENS ASSOCS . NONTERMS) where:

- TOKENS is a list of terminal symbols (tokens).

- ASSOCS is nil, or an alist of (ASSOC-TYPE . ASSOC-VALUE) elements
  describing the associativity of TOKENS.  ASSOC-TYPE must be one of
  the `default-prec' `nonassoc', `left' or `right' symbols.  When
  ASSOC-TYPE is `default-prec', ASSOC-VALUE must be nil or t (the
  default).  Otherwise it is a list of tokens which must have been
  previously declared in TOKENS.

- NONTERMS is a list of nonterminal definitions.

Optional argument START-LIST specify the possible grammar start
symbols.  This is a list of nonterminals which must have been
previously declared in GRAMMAR's NONTERMS form.  By default, the start
symbol is the first nonterminal defined.  When START-LIST contains
only one element, it is the start symbol.  Otherwise, all elements are
possible start symbols, unless `wisent-single-start-flag' is non-nil.
In that case, the first element is the start symbol, and others are
ignored.

Return an automaton as a vector: [ACTIONS GOTOS STARTS FUNCTIONS]
where:

- ACTIONS is a state/token matrix telling the parser what to do at
  every state based on the current lookahead token.  That is shift,
  reduce, accept or error.

- GOTOS is a state/nonterminal matrix telling the parser the next
  state to go to after reducing with each rule.

- STARTS is an alist which maps the allowed start nonterminal symbols
  to tokens that will be first shifted into the parser stack.

- FUNCTIONS is an obarray of semantic action symbols.  Each symbol's
  function definition is the semantic action lambda expression.

\(fn GRAMMAR &optional START-LIST)" nil nil)

(autoload 'wisent-byte-compile-grammar "wisent-comp" "\
Byte compile the `wisent-compile-grammar' FORM.
Automatically called by the Emacs Lisp byte compiler as a
`byte-compile' handler.

\(fn FORM)" nil nil)

(put 'wisent-compile-grammar 'byte-compile 'wisent-byte-compile-grammar)

;;;***

;;;### (autoloads (wisent-debug-show-entry wisent-cancel-debug-on-entry
;;;;;;  wisent-debug-on-entry) "wisent-debug" "wisent/wisent-debug.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from wisent/wisent-debug.el

(autoload 'wisent-debug-on-entry "wisent-debug" "\
Request AUTOMATON's FUNCTION to invoke debugger each time it is called.
FUNCTION must be a semantic action symbol that exists in AUTOMATON.

\(fn AUTOMATON FUNCTION)" t nil)

(autoload 'wisent-cancel-debug-on-entry "wisent-debug" "\
Undo effect of \\[wisent-debug-on-entry] on AUTOMATON's FUNCTION.
FUNCTION must be a semantic action symbol that exists in AUTOMATON.

\(fn AUTOMATON FUNCTION)" t nil)

(autoload 'wisent-debug-show-entry "wisent-debug" "\
Show the source of AUTOMATON's semantic action FUNCTION.
FUNCTION must be a semantic action symbol that exists in AUTOMATON.

\(fn AUTOMATON FUNCTION)" t nil)

;;;***

;;;### (autoloads (wisent-grammar-mode) "wisent-grammar" "wisent/wisent-grammar.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from wisent/wisent-grammar.el

(autoload 'wisent-grammar-mode "wisent-grammar" "\
Major mode for editing Wisent grammars.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.wy$" . wisent-grammar-mode))

(eval-after-load "speedbar" '(speedbar-add-supported-extension ".wy"))

;;;***

;;;### (autoloads (wisent-java-default-setup) "wisent-java-tags"
;;;;;;  "wisent/wisent-java-tags.el" (18857 63844))
;;; Generated autoloads from wisent/wisent-java-tags.el

(autoload 'wisent-java-default-setup "wisent-java-tags" "\
Hook run to setup Semantic in `java-mode'.
Use the alternate LALR(1) parser.

\(fn)" nil nil)

(add-hook 'java-mode-hook 'wisent-java-default-setup)

;;;***

;;;### (autoloads (wisent-javascript-setup-parser) "wisent-javascript"
;;;;;;  "wisent/wisent-javascript.el" (18857 63844))
;;; Generated autoloads from wisent/wisent-javascript.el

(autoload 'wisent-javascript-setup-parser "wisent-javascript" "\
Setup buffer for parse.

\(fn)" nil nil)

(add-hook 'javascript-mode-hook 'wisent-javascript-setup-parser)

(add-hook 'ecmascript-mode-hook 'wisent-javascript-setup-parser)

;;;***

;;;### (autoloads (wisent-python-default-setup) "wisent-python" "wisent/wisent-python.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from wisent/wisent-python.el

(autoload 'wisent-python-default-setup "wisent-python" "\
Setup buffer for parse.

\(fn)" nil nil)

(add-hook 'python-mode-hook 'wisent-python-default-setup)

;;;***

;;;### (autoloads nil nil ("bovine/bovine-grammar-macros.el" "bovine/semantic-c-by.el"
;;;;;;  "bovine/semantic-erlang-by.el" "bovine/semantic-erlang.el"
;;;;;;  "bovine/semantic-java.el" "bovine/semantic-make-by.el" "bovine/semantic-scm-by.el"
;;;;;;  "ctags/semantic-ectag-util.el" "semantic-analyze-fcn.el"
;;;;;;  "semantic-ast.el" "semantic-example.el" "semantic-fw.el"
;;;;;;  "semantic-grammar-wy.el" "semantic-load.el" "semantic-sb.el"
;;;;;;  "semantic-util.el" "semanticdb-el.el" "semanticdb-javascript.el"
;;;;;;  "semanticdb-mk.el" "semanticdb-skel.el" "wisent/semantic-wisent.el"
;;;;;;  "wisent/wisent-awk-wy.el" "wisent/wisent-calc-wy.el" "wisent/wisent-calc.el"
;;;;;;  "wisent/wisent-cim-wy.el" "wisent/wisent-expr.el" "wisent/wisent-grammar-macros.el"
;;;;;;  "wisent/wisent-java-tags-wy.el" "wisent/wisent-java-wy.el"
;;;;;;  "wisent/wisent-java.el" "wisent/wisent-javascript-jv-wy.el"
;;;;;;  "wisent/wisent-python-wy.el") (19926 52504 445194))

;;;***

(provide 'semantic-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; semantic-loaddefs.el ends here
