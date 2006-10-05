;;; semantic-loaddefs.el --- Auto-generated CEDET autoloads
;;
;;; Code:


;;;### (autoloads (bison->wisent) "bison-wisent" "wisent/bison-wisent.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from wisent/bison-wisent.el

(autoload (quote bison->wisent) "bison-wisent" "\
Treat the current buffer as a YACC or BISON file, and translate to wisent.
Replaces all comments with wisent compatible comments.
Finds % commands that wisent cannot handle, and comments them out.
Deletes all actions, replacing them with small comments." t nil)

;;;***

;;;### (autoloads (semantic-bovine-debug-parser semantic-bovine-debug-create-frame)
;;;;;;  "bovine-debug" "bovine/bovine-debug.el" (17091 25107))
;;; Generated autoloads from bovine/bovine-debug.el

(autoload (quote semantic-bovine-debug-create-frame) "bovine-debug" "\
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
LEXTOKEN, is a token returned by the lexer which is being matched." nil nil)

(autoload (quote semantic-bovine-debug-parser) "bovine-debug" "\
Represents a parser and its state." nil nil)

;;;***

;;;### (autoloads (bovine-grammar-mode) "bovine-grammar" "bovine/bovine-grammar.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from bovine/bovine-grammar.el

(autoload (quote bovine-grammar-mode) "bovine-grammar" "\
Major mode for editing Bovine grammars." t nil)

(add-to-list (quote auto-mode-alist) (quote ("\\.by$" . bovine-grammar-mode)))

(eval-after-load "speedbar" (quote (speedbar-add-supported-extension ".by")))

;;;***

;;;### (autoloads (semantic-bovinate-toplevel semantic-refresh-tags-safe
;;;;;;  semantic-fetch-tags semantic-parse-region-default) "semantic"
;;;;;;  "semantic.el" (17091 25107))
;;; Generated autoloads from semantic.el

(autoload (quote semantic-parse-region-default) "semantic" "\
Parse the area between START and END, and return any tags found.
If END needs to be extended due to a lexical token being too large, it
will be silently ignored.
Optional arguments:
NONTERMINAL is the rule to start parsing at if it is known.
DEPTH specifies the lexical depth to scan.
RETURNONERROR specifies that parsing should end when encountering
unterminated syntax." nil nil)

(autoload (quote semantic-fetch-tags) "semantic" "\
Fetch semantic tags from the current buffer.
If the buffer cache is up to date, return that.
If the buffer cache is out of date, attempt an incremental reparse.
If the buffer has not been parsed before, or if the incremental reparse
fails, then parse the entire buffer.
If a lexcial error had been previously discovered and the buffer
was marked unparseable, then do nothing, and return the cache." nil nil)

(autoload (quote semantic-refresh-tags-safe) "semantic" "\
Refreshes the current buffer's tags safely.

Return non-nil if the refresh was successful.
Return nil if there is some sort of syntax error preventing a reparse.

Does nothing if the current buffer doesn't need reparsing." nil nil)

(autoload (quote semantic-bovinate-toplevel) "semantic" "\
Backward Compatibility Function." nil nil)

(make-obsolete (quote semantic-bovinate-toplevel) (quote semantic-fetch-tags))

(defsubst semantic-fetch-available-tags nil "\
Fetch available semantic tags from the current buffer.
That is, return tags currently in the cache without parsing the
current buffer.
Parse operations happen asynchronously when needed on Emacs idle time.
Use the `semantic-after-toplevel-cache-change-hook' and
`semantic-after-partial-cache-change-hook' hooks to synchronize with
new tags when they become available." semantic--buffer-cache)

;;;***

;;;### (autoloads (semantic-analyze-possible-completions semantic-analyze-current-context)
;;;;;;  "semantic-analyze" "semantic-analyze.el" (17091 25107))
;;; Generated autoloads from semantic-analyze.el

(autoload (quote semantic-analyze-current-context) "semantic-analyze" "\
Analyze the current context at optional POSITION.
If called interactively, display interesting information about POSITION
in a separate buffer.
Returns an object based on symbol `semantic-analyze-context'.

This function can be overriden with the symbol `analyze-context'.
When overriding this function, your override will be called while
cursor is at POSITION.  In addition, your function will not be called
if a cached copy of the return object is found." t nil)

(autoload (quote semantic-analyze-possible-completions) "semantic-analyze" "\
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
in a buffer." t nil)

;;;***

;;;### (autoloads (semantic-bovinate-stream semantic-lambda) "semantic-bovine"
;;;;;;  "bovine/semantic-bovine.el" (17091 25107))
;;; Generated autoloads from bovine/semantic-bovine.el

(autoload (quote semantic-lambda) "semantic-bovine" "\
Create a lambda expression to return a list including RETURN-VAL.
The return list is a lambda expression to be used in a bovine table." nil (quote macro))

(autoload (quote semantic-bovinate-stream) "semantic-bovine" "\
Bovinate STREAM, starting at the first NONTERMINAL rule.
Use `bovine-toplevel' if NONTERMINAL is not provided.
This is the core routine for converting a stream into a table.
Return the list (STREAM SEMANTIC-STREAM) where STREAM are those
elements of STREAM that have not been used.  SEMANTIC-STREAM is the
list of semantic tokens found." nil nil)

(defalias (quote semantic-parse-stream-default) (quote semantic-bovinate-stream))

;;;***

;;;### (autoloads (semantic-default-c-setup) "semantic-c" "bovine/semantic-c.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from bovine/semantic-c.el

(autoload (quote semantic-default-c-setup) "semantic-c" "\
Set up a buffer for semantic parsing of the C language." nil nil)

(add-hook (quote c-mode-hook) (quote semantic-default-c-setup))

(add-hook (quote c++-mode-hook) (quote semantic-default-c-setup))

;;;***

;;;### (autoloads (semantic-cb-speedbar-mode semantic-dot) "semantic-cb"
;;;;;;  "semantic-cb.el" (17091 25107))
;;; Generated autoloads from semantic-cb.el

(autoload (quote semantic-dot) "semantic-cb" "\
Create a DOT graph out of the class browser information.
Argument START specifies the name of the class we are going to start
the graph with." t nil)

(autoload (quote semantic-cb-speedbar-mode) "semantic-cb" "\
Bring speedbar up, and put it into Class Browser mode.
This will use the Class Browser logic applied to the current Semantic
project database to build the available relations.  The structure of
the class hierarchy can then be navigated using traditional speedbar
interactions." t nil)

;;;***

;;;### (autoloads (semantic-chart-tag-complexity semantic-chart-database-size
;;;;;;  semantic-chart-tags-by-class) "semantic-chart" "semantic-chart.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from semantic-chart.el

(autoload (quote semantic-chart-tags-by-class) "semantic-chart" "\
Create a bar chart representing the number of tags for a given tag class.
Each bar represents how many toplevel tags in TAGTABLE
exist with a given class.  See `semantic-symbol->name-assoc-list'
for tokens which will be charted.
TAGTABLE is passedto `semantic-something-to-tag-table'." t nil)

(autoload (quote semantic-chart-database-size) "semantic-chart" "\
Create a bar chart representing the size of each file in semanticdb.
Each bar represents how many toplevel nonterminals in TAGTABLE
exist in each database entry.
TAGTABLE is passedto `semantic-something-to-tag-table'." t nil)

(autoload (quote semantic-chart-tag-complexity) "semantic-chart" "\
Create a bar chart representing the complexity of some tokens.
Complexity is calculated for tokens with a tag of CLASS.  Each bar
represents the complexity of some nonterminal in TAGTABLE.
Only the most complex items are charted.
TAGTABLE is passedto `semantic-something-to-tag-table'." t nil)

;;;***

;;;### (autoloads (semantic-complete-analyze-inline semantic-complete-analyze-and-replace
;;;;;;  semantic-complete-jump semantic-complete-jump-local semantic-complete-inline-analyzer
;;;;;;  semantic-complete-read-tag-analyzer semantic-complete-read-tag-project
;;;;;;  semantic-complete-read-tag-buffer-deep semantic-complete-read-tag-engine)
;;;;;;  "semantic-complete" "semantic-complete.el" (17091 25107))
;;; Generated autoloads from semantic-complete.el

(autoload (quote semantic-complete-read-tag-engine) "semantic-complete" "\
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
HISTORY is a symbol representing a variable to story the history in." nil nil)

(autoload (quote semantic-complete-read-tag-buffer-deep) "semantic-complete" "\
Ask for a tag by name from the current buffer.
Available tags are from the current buffer, at any level.
Completion options are presented in a traditional way, with highlighting
to resolve same-name collisions.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to store the history in." nil nil)

(autoload (quote semantic-complete-read-tag-project) "semantic-complete" "\
Ask for a tag by name from the current project.
Available tags are from the current project, at the top level.
Completion options are presented in a traditional way, with highlighting
to resolve same-name collisions.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to store the history in." nil nil)

(autoload (quote semantic-complete-read-tag-analyzer) "semantic-complete" "\
Ask for a tag by name based on the current context.
PROMPT is the first part of the prompt.  additional prompt
is added based on the contexts full prefix.
CONTEXT is the semantic analyzer context to start with.
HISTORY is a symbol representing a variable to stor the history in.
usually a default-tag and initial-input are available for completion
prompts.  these are calculated from the CONTEXT variable passed in." nil nil)

(autoload (quote semantic-complete-inline-analyzer) "semantic-complete" "\
Complete a symbol name by name based on the current context.
CONTEXT is the semantic analyzer context to start with.
See `semantic-complete-inline-tag-engine' for details on how
completion works." nil nil)

(autoload (quote semantic-complete-jump-local) "semantic-complete" "\
Jump to a semantic symbol." t nil)

(autoload (quote semantic-complete-jump) "semantic-complete" "\
Jump to a semantic symbol." t nil)

(autoload (quote semantic-complete-analyze-and-replace) "semantic-complete" "\
Perform prompt completion to do in buffer completion.
`semantic-analyze-possible-completions' is used to determine the
possible values.
The minibuffer is used to perform the completion.
The result is inserted as a replacement of the text that was there." t nil)

(autoload (quote semantic-complete-analyze-inline) "semantic-complete" "\
Perform prompt completion to do in buffer completion.
`semantic-analyze-possible-completions' is used to determine the
possible values.
The function returns immediately, leaving the buffer in a mode that
will perform the completion." t nil)

;;;***

;;;### (autoloads (semantic-debug semantic-debug-break) "semantic-debug"
;;;;;;  "semantic-debug.el" (17091 25107))
;;; Generated autoloads from semantic-debug.el

(defvar semantic-debug-parser-source nil "\
For any buffer, the file name (no path) of the parser.
This would be a parser for a specific language, not the source
to one of the parser generators.")

(make-variable-buffer-local (quote semantic-debug-parser-source))

(defvar semantic-debug-parser-class nil "\
Class to create when building a debug parser object.")

(make-variable-buffer-local (quote semantic-debug-parser-class))

(defvar semantic-debug-enabled nil "\
Non-nil when debugging a parser.")

(autoload (quote semantic-debug-break) "semantic-debug" "\
Stop parsing now at FRAME.
FRAME is an object that represents the parser's view of the
current state of the world.
This function enters a recursive edit.  It returns
on an `exit-recursive-edit', or if someone uses one
of the `semantic-debug-mode' commands.
It returns the command specified.  Parsers need to take action
on different types of return values." nil nil)

(autoload (quote semantic-debug) "semantic-debug" "\
Parse the current buffer and run in debug mode." t nil)

;;;***

;;;### (autoloads (semantic-tag-folded-p semantic-set-tag-folded
;;;;;;  semantic-tag-delete-secondary-overlay semantic-tag-get-secondary-overlay
;;;;;;  semantic-tag-create-secondary-overlay semantic-tag-secondary-overlays
;;;;;;  semantic-tag-read-only-p semantic-set-tag-read-only semantic-tag-intangible-p
;;;;;;  semantic-set-tag-intangible semantic-tag-invisible-p semantic-set-tag-invisible
;;;;;;  semantic-set-tag-face semantic-momentary-highlight-tag semantic-unhighlight-tag
;;;;;;  semantic-highlight-tag) "semantic-decorate" "semantic-decorate.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from semantic-decorate.el

(autoload (quote semantic-highlight-tag) "semantic-decorate" "\
Specify that TAG should be highlighted.
Optional FACE specifies the face to use." nil nil)

(autoload (quote semantic-unhighlight-tag) "semantic-decorate" "\
Unhighlight TAG, restoring it's previous face." nil nil)

(autoload (quote semantic-momentary-highlight-tag) "semantic-decorate" "\
Highlight TAG, removing highlighting when the user hits a key.
Optional argument FACE is the face to use for highlighting.
If FACE is not specified, then `highlight' will be used." nil nil)

(autoload (quote semantic-set-tag-face) "semantic-decorate" "\
Specify that TAG should use FACE for display." nil nil)

(autoload (quote semantic-set-tag-invisible) "semantic-decorate" "\
Enable the text in TAG to be made invisible.
If VISIBLE is non-nil, make the text visible." nil nil)

(autoload (quote semantic-tag-invisible-p) "semantic-decorate" "\
Return non-nil if TAG is invisible." nil nil)

(autoload (quote semantic-set-tag-intangible) "semantic-decorate" "\
Enable the text in TAG to be made intangible.
If TANGIBLE is non-nil, make the text visible.
This function does not have meaning in XEmacs because it seems that
the extent 'intangible' property does not exist." nil nil)

(autoload (quote semantic-tag-intangible-p) "semantic-decorate" "\
Return non-nil if TAG is intangible.
This function does not have meaning in XEmacs because it seems that
the extent 'intangible' property does not exist." nil nil)

(autoload (quote semantic-set-tag-read-only) "semantic-decorate" "\
Enable the text in TAG to be made read-only.
Optional argument WRITABLE should be non-nil to make the text writable
instead of read-only." nil nil)

(autoload (quote semantic-tag-read-only-p) "semantic-decorate" "\
Return non-nil if the current TAG is marked read only." nil nil)

(semantic-alias-obsolete (quote semantic-highlight-token) (quote semantic-highlight-tag))

(semantic-alias-obsolete (quote semantic-unhighlight-token) (quote semantic-unhighlight-tag))

(semantic-alias-obsolete (quote semantic-momentary-unhighlight-token) (quote semantic-momentary-unhighlight-tag))

(semantic-alias-obsolete (quote semantic-momentary-highlight-token) (quote semantic-momentary-highlight-tag))

(semantic-alias-obsolete (quote semantic-set-token-face) (quote semantic-set-tag-face))

(semantic-alias-obsolete (quote semantic-set-token-invisible) (quote semantic-set-tag-invisible))

(semantic-alias-obsolete (quote semantic-token-invisible-p) (quote semantic-tag-invisible-p))

(semantic-alias-obsolete (quote semantic-set-token-intangible) (quote semantic-set-tag-intangible))

(semantic-alias-obsolete (quote semantic-token-intangible-p) (quote semantic-tag-intangible-p))

(semantic-alias-obsolete (quote semantic-set-token-read-only) (quote semantic-set-tag-read-only))

(semantic-alias-obsolete (quote semantic-token-read-only-p) (quote semantic-tag-read-only-p))

(autoload (quote semantic-tag-secondary-overlays) "semantic-decorate" "\
Return a list of secondary overlays active on TAG." nil nil)

(autoload (quote semantic-tag-create-secondary-overlay) "semantic-decorate" "\
Create a secondary overlay for TAG.
Returns an overlay.  The overlay is also saved in TAG.
LINK-HOOK is a function called whenever TAG is to be linked into
a buffer.  It should take TAG and OVERLAY as arguments.
The LINK-HOOK should be used to position and set properties on the
generated secondary overlay." nil nil)

(autoload (quote semantic-tag-get-secondary-overlay) "semantic-decorate" "\
Return secondary overlays from TAG with PROPERTY.
PROPERTY is a symbol and all overlays with that symbol are returned.." nil nil)

(autoload (quote semantic-tag-delete-secondary-overlay) "semantic-decorate" "\
Delete from TAG the secondary overlay OVERLAY-OR-PROPERTY.
If OVERLAY-OR-PROPERTY is an overlay, delete that overlay.
If OVERLAY-OR-PROPERTY is a symbol, find the overlay with that property." nil nil)

(autoload (quote semantic-set-tag-folded) "semantic-decorate" "\
Fold TAG, such that only the first line of text is shown.
Optional argument FOLDED should be non-nil to fold the tag.
nil implies the tag should be fully shown." nil nil)

(autoload (quote semantic-tag-folded-p) "semantic-decorate" "\
Non-nil if TAG is currently folded." nil nil)

;;;***

;;;### (autoloads (semantic-build-decoration-mode-menu semantic-decoration-mode
;;;;;;  global-semantic-decoration-mode global-semantic-decoration-mode)
;;;;;;  "semantic-decorate-mode" "semantic-decorate-mode.el" (17091
;;;;;;  25107))
;;; Generated autoloads from semantic-decorate-mode.el

(autoload (quote global-semantic-decoration-mode) "semantic-decorate-mode" "\
Toggle global use of option `semantic-decoration-mode'.
Decoration mode turns on all active decorations as specified
by `semantic-decoration-styles'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle." t nil)

(defvar global-semantic-decoration-mode nil "\
*If non-nil, enable global use of command `semantic-decoration-mode'.
When this mode is activated, decorations specified by
`semantic-decoration-styles'.")

(custom-add-to-group (quote semantic) (quote global-semantic-decoration-mode) (quote custom-variable))

(custom-add-load (quote global-semantic-decoration-mode) (quote semantic-decorate-mode))

(defvar semantic-decoration-mode nil "\
Non-nil if command `semantic-decoration-mode' is enabled.
Use the command `semantic-decoration-mode' to change this variable.")

(autoload (quote semantic-decoration-mode) "semantic-decorate-mode" "\
Minor mode for decorating tags.
Decorations are specified in `semantic-decoration-styles'.
You can define new decoration styles with
`define-semantic-decoration-style'.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled." t nil)

(autoload (quote semantic-build-decoration-mode-menu) "semantic-decorate-mode" "\
Create a menu listing all the known decorations for toggling.
IGNORE any input arguments." nil nil)

;;;***

;;;### (autoloads (semantic-documentation-for-tag) "semantic-doc"
;;;;;;  "semantic-doc.el" (17091 25107))
;;; Generated autoloads from semantic-doc.el

(autoload (quote semantic-documentation-for-tag) "semantic-doc" "\
Find documentation from TAG and return it as a clean string.
TAG might have DOCUMENTATION set in it already.  If not, there may be
some documentation in a comment preceding TAG's definition which we
can look for.  When appropriate, this can be overridden by a language specific
enhancement.
Optional argument NOSNARF means to only return the lexical analyzer token for it.
If nosnarf if 'lex, then only return the lex token." nil nil)

(semantic-alias-obsolete (quote semantic-find-documentation) (quote semantic-documentation-for-tag))

;;;***

;;;### (autoloads (semantic-ede-proj-target-grammar) "semantic-ede-grammar"
;;;;;;  "semantic-ede-grammar.el" (17091 25107))
;;; Generated autoloads from semantic-ede-grammar.el

(autoload (quote semantic-ede-proj-target-grammar) "semantic-ede-grammar" "\
This target consists of a group of grammar files.
A grammar target consists of grammar files that build Emacs Lisp programs for
parsing different languages." nil nil)

(autoload (quote ede-proj-target-elisp) "semantic-ede-proj-target-grammar" "\
Target class for Emacs/Semantic grammar files." nil nil)

(eval-after-load "ede-proj" (quote (require (quote semantic-ede-grammar))))

;;;***

;;;### (autoloads (semantic-edits-incremental-parser semantic-edits-flush-changes
;;;;;;  semantic-edits-change-function-handle-changes semantic-change-function)
;;;;;;  "semantic-edit" "semantic-edit.el" (17091 25107))
;;; Generated autoloads from semantic-edit.el

(autoload (quote semantic-change-function) "semantic-edit" "\
Provide a mechanism for semantic token management.
Argument START, END, and LENGTH specify the bounds of the change." nil nil)

(autoload (quote semantic-edits-change-function-handle-changes) "semantic-edit" "\
Run whenever a buffer controlled by `semantic-mode' change.
Tracks when and how the buffer is re-parsed.
Argument START, END, and LENGTH specify the bounds of the change." nil nil)

(autoload (quote semantic-edits-flush-changes) "semantic-edit" "\
Flush the changes in the current buffer." nil nil)

(autoload (quote semantic-edits-incremental-parser) "semantic-edit" "\
Incrementally reparse the current buffer.
Incremental parser allows semantic to only reparse those sections of
the buffer that have changed.  This function depends on
`semantic-edits-change-function-handle-changes' setting up change
overlays in the current buffer.  Those overlays are analyzed against
the semantic cache to see what needs to be changed." nil nil)

(defalias (quote semantic-parse-changes-default) (quote semantic-edits-incremental-parser))

(add-hook (quote semantic-change-hooks) (function semantic-edits-change-function-handle-changes))

(add-hook (quote semantic-before-toplevel-cache-flush-hook) (function semantic-edits-flush-changes))

;;;***

;;;### (autoloads (semantic-default-elisp-setup) "semantic-el" "bovine/semantic-el.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from bovine/semantic-el.el

(autoload (quote semantic-default-elisp-setup) "semantic-el" "\
Setup hook function for Emacs Lisp files and Semantic." nil nil)

(add-hook (quote emacs-lisp-mode-hook) (quote semantic-default-elisp-setup))

(add-hook (quote lisp-mode-hook) (quote semantic-default-elisp-setup))

(eval-after-load "semanticdb" (quote (require (quote semanticdb-el))))

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
;;;;;;  "semantic-find" "semantic-find.el" (17091 25107))
;;; Generated autoloads from semantic-find.el

(autoload (quote semantic-find-tag-by-overlay) "semantic-find" "\
Find all tags covering POSITIONORMARKER by using overlays.
If POSITIONORMARKER is nil, use the current point.
Optional BUFFER is used if POSITIONORMARKER is a number, otherwise the current
buffer is used.  This finds all tags covering the specified position
by checking for all overlays covering the current spot.  They are then sorted
from largest to smallest via the start location." nil nil)

(autoload (quote semantic-find-tag-by-overlay-in-region) "semantic-find" "\
Find all tags which exist in whole or in part between START and END.
Uses overlays to determine positin.
Optional BUFFER argument specifies the buffer to use." nil nil)

(autoload (quote semantic-find-tag-by-overlay-next) "semantic-find" "\
Find the next tag after START in BUFFER.
If START is in an overlay, find the tag which starts next,
not the current tag." nil nil)

(autoload (quote semantic-find-tag-by-overlay-prev) "semantic-find" "\
Find the next tag before START in BUFFER.
If START is in an overlay, find the tag which starts next,
not the current tag." nil nil)

(autoload (quote semantic-find-tag-parent-by-overlay) "semantic-find" "\
Find the parent of TAG by overlays.
Overlays are a fast way of finding this information for active buffers." nil nil)

(autoload (quote semantic-current-tag) "semantic-find" "\
Return the current tag in the current buffer.
If there are more than one in the same location, return the
smallest tag.  Return nil if there is no tag here." nil nil)

(autoload (quote semantic-current-tag-parent) "semantic-find" "\
Return the current tags parent in the current buffer.
A tag's parent would be a containing structure, such as a type
containing a field.  Return nil if there is no parent." nil nil)

(autoload (quote semantic-current-tag-of-class) "semantic-find" "\
Return the current (smallest) tags of CLASS in the current buffer.
If the smallest tag is not of type CLASS, keep going upwards until one
is found.
Uses `semantic-tag-class' for classification." nil nil)

(defsubst semantic-find-first-tag-by-name (name &optional table) "\
Find the first tag with NAME in TABLE.
NAME is a string.
TABLE is a semantic tags table.  See `semantic-something-to-tag-table'.
This routine uses `assoc' to quickly find the first matching entry." (funcall (if semantic-case-fold (quote assoc-ignore-case) (quote assoc)) name (semantic-something-to-tag-table table)))

(autoload (quote semantic-find-tags-by-name) "semantic-find" "\
Find all tags with NAME in TABLE.
NAME is a string.
TABLE is a tag table.  See `semantic-something-to-tag-table'." nil (quote macro))

(autoload (quote semantic-find-tags-for-completion) "semantic-find" "\
Find all tags whos name begins with PREFIX in TABLE.
PREFIX is a string.
TABLE is a tag table.  See `semantic-something-to-tag-table'.
While it would be nice to use `try-completion' or `all-completions',
those functions do not return the tags, only a string.
Uses `compare-strings' for fast comparison." nil (quote macro))

(autoload (quote semantic-find-tags-by-name-regexp) "semantic-find" "\
Find all tags with name matching REGEXP in TABLE.
REGEXP is a string containing a regular expression,
TABLE is a tag table.  See `semantic-something-to-tag-table'.
Consider using `semantic-find-tags-for-completion' if you are
attempting to do completions." nil (quote macro))

(autoload (quote semantic-find-tags-by-class) "semantic-find" "\
Find all tags of class CLASS in TABLE.
CLASS is a symbol representing the class of the token, such as
'variable, of 'function..
TABLE is a tag table.  See `semantic-something-to-tag-table'." nil (quote macro))

(autoload (quote semantic-find-tags-by-type) "semantic-find" "\
Find all tags of with a type TYPE in TABLE.
TYPE is a string or tag representing a data type as defined in the
language the tags were parsed from, such as \"int\", or perhaps
a tag whose name is that of a struct or class.
TABLE is a tag table.  See `semantic-something-to-tag-table'." nil (quote macro))

(autoload (quote semantic-find-tags-of-compound-type) "semantic-find" "\
Find all tags which are a compound type in TABLE.
Compound types are structures, or other data type which
is not of a primitive nature, such as int or double.
Used in completion." nil (quote macro))

(autoload (quote semantic-find-tags-by-scope-protection) "semantic-find" "\
Find all tags accessable by SCOPEPROTECTION.
SCOPEPROTECTION is a symbol which can be returned by the method
`semantic-tag-protection'.  A hard-coded order is used to determine a match.
PARENT is a tag representing the PARENT slot needed for
`semantic-tag-protection'.
TABLE is a list of tags (a subset of PARENT members) to scan.  If TABLE is nil,
the type members of PARENT are used.
See `semantic-tag-protected-p' for details on which tags are returned." nil nil)

(defsubst semantic-find-tags-included (&optional table) "\
Find all tags in TABLE that are of the 'include class.
TABLE is a tag table.  See `semantic-something-to-tag-table'." (semantic-find-tags-by-class (quote include) table))

(autoload (quote semantic-deep-find-tags-by-name) "semantic-find" "\
Find all tags with NAME in TABLE.
Search in top level tags, and their components, in TABLE.
NAME is a string.
TABLE is a tag table.  See `semantic-flatten-tags-table'.
See also `semantic-find-tags-by-name'." nil (quote macro))

(autoload (quote semantic-deep-find-tags-for-completion) "semantic-find" "\
Find all tags whos name begins with PREFIX in TABLE.
Search in top level tags, and their components, in TABLE.
TABLE is a tag table.  See `semantic-flatten-tags-table'.
See also `semantic-find-tags-for-completion'." nil (quote macro))

(autoload (quote semantic-deep-find-tags-by-name-regexp) "semantic-find" "\
Find all tags with name matching REGEXP in TABLE.
Search in top level tags, and their components, in TABLE.
REGEXP is a string containing a regular expression,
TABLE is a tag table.  See `semantic-flatten-tags-table'.
See also `semantic-find-tags-by-name-regexp'.
Consider using `semantic-deep-find-tags-for-completion' if you are
attempting to do completions." nil (quote macro))

(autoload (quote semantic-brute-find-first-tag-by-name) "semantic-find" "\
Find a tag NAME within STREAMORBUFFER.  NAME is a string.
If SEARCH-PARTS is non-nil, search children of tags.
If SEARCH-INCLUDE is non-nil, search include files.

Use `semantic-find-first-tag-by-name' instead." nil nil)

(autoload (quote semantic-brute-find-tag-by-class) "semantic-find" "\
Find all tags with a class CLASS within STREAMORBUFFER.
CLASS is a symbol representing the class of the tags to find.
See `semantic-tag-class'.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'.

Use `semantic-find-tag-by-class' instead." nil (quote macro))

(autoload (quote semantic-brute-find-tag-standard) "semantic-find" "\
Find all tags in STREAMORBUFFER which define simple class types.
See `semantic-tag-class'.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'." nil (quote macro))

(autoload (quote semantic-brute-find-tag-by-type) "semantic-find" "\
Find all tags with type TYPE within STREAMORBUFFER.
TYPE is a string which is the name of the type of the tags returned.
See `semantic-tag-type'.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'." nil nil)

(autoload (quote semantic-brute-find-tag-by-type-regexp) "semantic-find" "\
Find all tags with type matching REGEXP within STREAMORBUFFER.
REGEXP is a regular expression  which matches the  name of the type of the
tags returned.  See `semantic-tag-type'.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'." nil nil)

(autoload (quote semantic-brute-find-tag-by-name-regexp) "semantic-find" "\
Find all tags whose name match REGEX in STREAMORBUFFER.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'." nil nil)

(autoload (quote semantic-brute-find-tag-by-property) "semantic-find" "\
Find all tags with PROPERTY equal to VALUE in STREAMORBUFFER.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'." nil nil)

(autoload (quote semantic-brute-find-tag-by-attribute) "semantic-find" "\
Find all tags with a given ATTR in STREAMORBUFFER.
ATTR is a symbol key into the attributes list.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'." nil nil)

(autoload (quote semantic-brute-find-tag-by-attribute-value) "semantic-find" "\
Find all tags with a given ATTR equal to VALUE in STREAMORBUFFER.
ATTR is a symbol key into the attributes list.
VALUE is the value that ATTR should match.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'." nil nil)

(autoload (quote semantic-brute-find-tag-by-function) "semantic-find" "\
Find all tags for which FUNCTION's value is non-nil within STREAMORBUFFER.
FUNCTION must return non-nil if an element of STREAM will be included
in the new list.

If optional argument SEARCH-PARTS is non-nil, all sub-parts of tags
are searched.  The overloadable function `semantic-tag-componenets' is
used for the searching child lists.  If SEARCH-PARTS is the symbol
'positiononly, then only children that have positional information are
searched.

If SEARCH-INCLUDES is non-nil, then all include files are also
searched for matches.  This parameter hasn't be active for a while
and is obsolete." nil nil)

(autoload (quote semantic-brute-find-first-tag-by-function) "semantic-find" "\
Find the first nonterminal which FUNCTION match within STREAMORBUFFER.
FUNCTION must return non-nil if an element of STREAM will be included
in the new list.

The following parameters were never implemented.

If optional argument SEARCH-PARTS, all sub-parts of tags are searched.
The overloadable function `semantic-tag-components' is used for
searching.
If SEARCH-INCLUDES is non-nil, then all include files are also
searched for matches." nil nil)

(autoload (quote semantic-brute-find-tag-by-position) "semantic-find" "\
Find a nonterminal covering POSITION within STREAMORBUFFER.
POSITION is a number, or marker.  If NOMEDIAN is non-nil, don't do
the median calculation, and return nil." nil nil)

(autoload (quote semantic-brute-find-innermost-tag-by-position) "semantic-find" "\
Find a list of tags covering POSITION within STREAMORBUFFER.
POSITION is a number, or marker.  If NOMEDIAN is non-nil, don't do
the median calculation, and return nil.
This function will find the topmost item, and recurse until no more
details are available of findable." nil nil)

(semantic-alias-obsolete (quote semantic-find-nonterminal-by-overlay) (quote semantic-find-tag-by-overlay))

(semantic-alias-obsolete (quote semantic-find-nonterminal-by-overlay-in-region) (quote semantic-find-tag-by-overlay-in-region))

(semantic-alias-obsolete (quote semantic-find-nonterminal-by-overlay-next) (quote semantic-find-tag-by-overlay-next))

(semantic-alias-obsolete (quote semantic-find-nonterminal-by-overlay-prev) (quote semantic-find-tag-by-overlay-prev))

(semantic-alias-obsolete (quote semantic-find-nonterminal-parent-by-overlay) (quote semantic-find-tag-parent-by-overlay))

(semantic-alias-obsolete (quote semantic-current-nonterminal) (quote semantic-current-tag))

(semantic-alias-obsolete (quote semantic-current-nonterminal-parent) (quote semantic-current-tag-parent))

(semantic-alias-obsolete (quote semantic-current-nonterminal-of-type) (quote semantic-current-tag-of-class))

(semantic-alias-obsolete (quote semantic-find-nonterminal-by-name) (quote semantic-brute-find-first-tag-by-name))

(semantic-alias-obsolete (quote semantic-find-nonterminal-by-token) (quote semantic-brute-find-tag-by-class))

(semantic-alias-obsolete (quote semantic-find-nonterminal-standard) (quote semantic-brute-find-tag-standard))

(semantic-alias-obsolete (quote semantic-find-nonterminal-by-type) (quote semantic-brute-find-tag-by-type))

(semantic-alias-obsolete (quote semantic-find-nonterminal-by-type-regexp) (quote semantic-brute-find-tag-by-type-regexp))

(semantic-alias-obsolete (quote semantic-find-nonterminal-by-name-regexp) (quote semantic-brute-find-tag-by-name-regexp))

(semantic-alias-obsolete (quote semantic-find-nonterminal-by-property) (quote semantic-brute-find-tag-by-property))

(semantic-alias-obsolete (quote semantic-find-nonterminal-by-extra-spec) (quote semantic-brute-find-tag-by-attribute))

(semantic-alias-obsolete (quote semantic-find-nonterminal-by-extra-spec-value) (quote semantic-brute-find-tag-by-attribute-value))

(semantic-alias-obsolete (quote semantic-find-nonterminal-by-function) (quote semantic-brute-find-tag-by-function))

(semantic-alias-obsolete (quote semantic-find-nonterminal-by-function-first-match) (quote semantic-brute-find-first-tag-by-function))

(semantic-alias-obsolete (quote semantic-find-nonterminal-by-position) (quote semantic-brute-find-tag-by-position))

(semantic-alias-obsolete (quote semantic-find-innermost-nonterminal-by-position) (quote semantic-brute-find-innermost-tag-by-position))

;;;***

;;;### (autoloads (semantic-format-tag-uml-concise-prototype semantic-format-tag-uml-prototype
;;;;;;  semantic-format-tag-uml-abbreviate semantic-format-tag-concise-prototype
;;;;;;  semantic-format-tag-prototype semantic-format-tag-summarize
;;;;;;  semantic-format-tag-abbreviate semantic-format-tag-name semantic-format-tag-prin1
;;;;;;  semantic-format-tag-type) "semantic-format" "semantic-format.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from semantic-format.el

(defvar semantic-format-tag-functions (quote (semantic-format-tag-name semantic-format-tag-abbreviate semantic-format-tag-summarize semantic-format-tag-prototype semantic-format-tag-concise-prototype semantic-format-tag-uml-abbreviate semantic-format-tag-uml-prototype semantic-format-tag-uml-concise-prototype semantic-format-tag-prin1)) "\
List of functions which convert a tag to text.
Each function must take the parameters TAG &optional PARENT COLOR.
TAG is the tag to convert.
PARENT is a parent tag or name which refers to the structure
or class which contains TAG.  PARENT is NOT a class which a TAG
would claim as a parent.
COLOR indicates that the generated text should be colored using
`font-lock'.")

(semantic-varalias-obsolete (quote semantic-token->text-functions) (quote semantic-format-tag-functions))

(defvar semantic-format-tag-custom-list (append (quote (radio)) (mapcar (lambda (f) (list (quote const) f)) semantic-format-tag-functions) (quote (function))) "\
A List used by customizeable variables to choose a tag to text function.
Use this variable in the :type field of a customizable variable.")

(autoload (quote semantic-format-tag-type) "semantic-format" "\
Convert the data type of TAG to a string usable in tag formatting.
It is presumed that TYPE is a string or semantic tag." nil nil)

(autoload (quote semantic-format-tag-prin1) "semantic-format" "\
Convert TAG to a string that is the print name for TAG.
PARENT and COLOR are ignored." nil nil)

(autoload (quote semantic-format-tag-name) "semantic-format" "\
Return the name string describing TAG.
The name is the shortest possible representation.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors." nil nil)

(autoload (quote semantic-format-tag-abbreviate) "semantic-format" "\
Return an abbreviated string describing TAG.
The abbreviation is to be short, with possible symbols indicating
the type of tag, or other information.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors." nil nil)

(autoload (quote semantic-format-tag-summarize) "semantic-format" "\
Summarize TAG in a reasonable way.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors." nil nil)

(autoload (quote semantic-format-tag-prototype) "semantic-format" "\
Return a prototype for TAG.
This function should be overloaded, though it need not be used.
This is because it can be used to create code by language independent
tools.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors." nil nil)

(autoload (quote semantic-format-tag-concise-prototype) "semantic-format" "\
Return a concise prototype for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors." nil nil)

(autoload (quote semantic-format-tag-uml-abbreviate) "semantic-format" "\
Return a UML style abbreviation for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors." nil nil)

(autoload (quote semantic-format-tag-uml-prototype) "semantic-format" "\
Return a UML style prototype for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors." nil nil)

(autoload (quote semantic-format-tag-uml-concise-prototype) "semantic-format" "\
Return a UML style concise prototype for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors." nil nil)

;;;***

;;;### (autoloads (semantic-grammar-batch-build-packages) "semantic-grammar"
;;;;;;  "semantic-grammar.el" (17091 25107))
;;; Generated autoloads from semantic-grammar.el

(autoload (quote semantic-grammar-batch-build-packages) "semantic-grammar" "\
Build Lisp packages from grammar files on the command line.
That is, run `semantic-grammar-batch-build-one-package' for each file.
Each file is processed even if an error occurred previously.
Must be used from the command line, with `-batch'.
For example, to process grammar files in current directory, invoke:

  \"emacs -batch -f semantic-grammar-batch-build-packages .\".

See also the variable `semantic-grammar-file-regexp'." nil nil)

;;;***

;;;### (autoloads (semantic-default-html-setup) "semantic-html" "semantic-html.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from semantic-html.el

(autoload (quote semantic-default-html-setup) "semantic-html" "\
Set up a buffer for parsing of HTML files." nil nil)

(add-hook (quote html-mode-hook) (quote semantic-default-html-setup))

;;;***

;;;### (autoloads (semantic-ia-show-doc semantic-ia-complete-tip
;;;;;;  semantic-ia-complete-symbol-menu semantic-ia-complete-symbol)
;;;;;;  "semantic-ia" "semantic-ia.el" (17091 25107))
;;; Generated autoloads from semantic-ia.el

(autoload (quote semantic-ia-complete-symbol) "semantic-ia" "\
Complete the current symbol at POINT.
Completion options are calculated with `semantic-analyze-possible-completions'." t nil)

(autoload (quote semantic-ia-complete-symbol-menu) "semantic-ia" "\
Complete the current symbol via a menu based at POINT.
Completion options are calculated with `semantic-analyze-possible-completions'." t nil)

(autoload (quote semantic-ia-complete-tip) "semantic-ia" "\
Pop up a tooltip for completion at POINT." t nil)

(autoload (quote semantic-ia-show-doc) "semantic-ia" "\
Display the code-level documentation for the symbol at POINT." t nil)

;;;***

;;;### (autoloads (semantic-speedbar-analysis) "semantic-ia-sb" "semantic-ia-sb.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from semantic-ia-sb.el

(autoload (quote semantic-speedbar-analysis) "semantic-ia-sb" "\
Start Speedbar in semantic analysis mode.
The analyzer displays information about the current context, plus a smart
list of possible completions." t nil)

;;;***

;;;### (autoloads (semantic-idle-scheduler-remove semantic-idle-scheduler-add
;;;;;;  semantic-idle-scheduler-mode global-semantic-idle-scheduler-mode
;;;;;;  global-semantic-idle-scheduler-mode) "semantic-idle" "semantic-idle.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from semantic-idle.el

(defvar global-semantic-idle-scheduler-mode nil "\
*If non-nil, enable global use of idle-scheduler mode.")

(custom-add-to-group (quote semantic) (quote global-semantic-idle-scheduler-mode) (quote custom-variable))

(custom-add-load (quote global-semantic-idle-scheduler-mode) (quote semantic-idle))

(autoload (quote global-semantic-idle-scheduler-mode) "semantic-idle" "\
Toggle global use of option `semantic-idle-scheduler-mode'.
The idle scheduler with automatically reparse buffers in idle time,
and then schedule other jobs setup with `semantic-idle-scheduler-add'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle." t nil)

(defvar semantic-idle-scheduler-mode nil "\
Non-nil if idle-scheduler minor mode is enabled.
Use the command `semantic-idle-scheduler-mode' to change this variable.")

(autoload (quote semantic-idle-scheduler-mode) "semantic-idle" "\
Minor mode to auto parse buffer following a change.
When this mode is off, a buffer is only rescanned for tokens when
some command requests the list of available tokens.  When idle-scheduler
is enabled, Emacs periodically checks to see if the buffer is out of
date, and reparses while the user is idle (not typing.)

With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled." t nil)

(autoload (quote semantic-idle-scheduler-add) "semantic-idle" "\
Schedule FUNCTION to occur during idle time." nil nil)

(autoload (quote semantic-idle-scheduler-remove) "semantic-idle" "\
Unschedule FUNCTION to occur during idle time." nil nil)

;;;***

;;;### (autoloads (semantic-create-imenu-index) "semantic-imenu"
;;;;;;  "semantic-imenu.el" (17091 25107))
;;; Generated autoloads from semantic-imenu.el

(autoload (quote semantic-create-imenu-index) "semantic-imenu" "\
Create an imenu index for any buffer which supports Semantic.
Uses the output of the Semantic parser to create the index.
Optional argument STREAM is an optional stream of tags used to create menus." nil nil)

;;;***

;;;### (autoloads (define-lex-block-analyzer define-lex-simple-regex-analyzer
;;;;;;  define-lex-regex-analyzer define-lex-analyzer semantic-lex
;;;;;;  semantic-lex-init define-lex) "semantic-lex" "semantic-lex.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from semantic-lex.el

(defvar semantic-lex-analyzer (quote semantic-flex) "\
The lexical analyzer used for a given buffer.
See `semantic-lex' for documentation.
For compatibility with Semantic 1.x it defaults to `semantic-flex'.")

(autoload (quote define-lex) "semantic-lex" "\
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
analyzer which might mistake a number for as a symbol." nil (quote macro))

(autoload (quote semantic-lex-init) "semantic-lex" "\
Initialize any lexical state for this buffer." nil nil)

(autoload (quote semantic-lex) "semantic-lex" "\
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
scanning, use `narrow-to-region'." nil nil)

(autoload (quote define-lex-analyzer) "semantic-lex" "\
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
This can be done by using `semantic-lex-push-token'." nil (quote macro))

(autoload (quote define-lex-regex-analyzer) "semantic-lex" "\
Create a lexical analyzer with NAME and DOC that will match REGEXP.
FORMS are evaluated upon a successful match.
See `define-lex-analyzer' for more about analyzers." nil (quote macro))

(autoload (quote define-lex-simple-regex-analyzer) "semantic-lex" "\
Create a lexical analyzer with NAME and DOC that match REGEXP.
TOKSYM is the symbol to use when creating a semantic lexical token.
INDEX is the index into the match that defines the bounds of the token.
Index should be a plain integer, and not specified in the macro as an
expression.
FORMS are evaluated upon a successful match BEFORE the new token is
created.  It is valid to ignore FORMS.
See `define-lex-analyzer' for more about analyzers." nil (quote macro))

(autoload (quote define-lex-block-analyzer) "semantic-lex" "\
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
symbols returned in open and close tokens." nil (quote macro))

;;;***

;;;### (autoloads (semantic-default-make-setup) "semantic-make" "bovine/semantic-make.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from bovine/semantic-make.el

(autoload (quote semantic-default-make-setup) "semantic-make" "\
Set up a Makefile buffer for parsing with semantic." nil nil)

(add-hook (quote makefile-mode-hook) (quote semantic-default-make-setup))

;;;***

;;;### (autoloads (semantic-regtest-cmp-results semantic-regtest-create-output
;;;;;;  semantic-regtest-run-test) "semantic-regtest" "semantic-regtest.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from semantic-regtest.el

(autoload (quote semantic-regtest-run-test) "semantic-regtest" nil t nil)

(autoload (quote semantic-regtest-create-output) "semantic-regtest" "\
Creates the test-output for the current buffer.
The user will be asked for the file-name of the created test-output-file (see
`semantic-regtest-create-output--internal')." t nil)

(autoload (quote semantic-regtest-cmp-results) "semantic-regtest" "\
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
`use-full-path-name' and a description of the format of RESULT-FILE." t nil)

;;;***

;;;### (autoloads (semantic-default-scheme-setup) "semantic-scm"
;;;;;;  "bovine/semantic-scm.el" (17091 25107))
;;; Generated autoloads from bovine/semantic-scm.el

(autoload (quote semantic-default-scheme-setup) "semantic-scm" "\
Setup hook function for Emacs Lisp files and Semantic." nil nil)

(add-hook (quote scheme-mode-hook) (quote semantic-default-scheme-setup))

;;;***

;;;### (autoloads (semantic-default-skel-setup) "semantic-skel" "bovine/semantic-skel.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from bovine/semantic-skel.el

(autoload (quote semantic-default-skel-setup) "semantic-skel" "\
Set up a buffer for semantic parsing of the skeleton language." nil nil)

;;;***

;;;### (autoloads (semantic-tag-external-class semantic-tag-external-member-children
;;;;;;  semantic-tag-external-member-p semantic-tag-external-member-parent
;;;;;;  semantic-adopt-external-members semantic-bucketize semantic-flatten-tags-table
;;;;;;  semantic-unique-tag-table semantic-unique-tag-table-by-name
;;;;;;  semantic-sort-tags-by-type-decreasing-ci semantic-sort-tags-by-type-increasing-ci
;;;;;;  semantic-sort-tags-by-name-decreasing-ci semantic-sort-tags-by-name-increasing-ci
;;;;;;  semantic-sort-tags-by-type-decreasing semantic-sort-tags-by-type-increasing
;;;;;;  semantic-sort-tags-by-name-decreasing semantic-sort-tags-by-name-increasing)
;;;;;;  "semantic-sort" "semantic-sort.el" (17091 25107))
;;; Generated autoloads from semantic-sort.el

(autoload (quote semantic-sort-tags-by-name-increasing) "semantic-sort" "\
Sort TAGS by name in increasing order with side effects.
Return the sorted list." nil nil)

(autoload (quote semantic-sort-tags-by-name-decreasing) "semantic-sort" "\
Sort TAGS by name in decreasing order with side effects.
Return the sorted list." nil nil)

(autoload (quote semantic-sort-tags-by-type-increasing) "semantic-sort" "\
Sort TAGS by type in increasing order with side effects.
Return the sorted list." nil nil)

(autoload (quote semantic-sort-tags-by-type-decreasing) "semantic-sort" "\
Sort TAGS by type in decreasing order with side effects.
Return the sorted list." nil nil)

(autoload (quote semantic-sort-tags-by-name-increasing-ci) "semantic-sort" "\
Sort TAGS by name in increasing order with side effects.
Return the sorted list." nil nil)

(autoload (quote semantic-sort-tags-by-name-decreasing-ci) "semantic-sort" "\
Sort TAGS by name in decreasing order with side effects.
Return the sorted list." nil nil)

(autoload (quote semantic-sort-tags-by-type-increasing-ci) "semantic-sort" "\
Sort TAGS by type in increasing order with side effects.
Return the sorted list." nil nil)

(autoload (quote semantic-sort-tags-by-type-decreasing-ci) "semantic-sort" "\
Sort TAGS by type in decreasing order with side effects.
Return the sorted list." nil nil)

(autoload (quote semantic-unique-tag-table-by-name) "semantic-sort" "\
Scan a list of TAGS, removing duplicate names.
This must first sort the tags by name alphabetically ascending." nil nil)

(autoload (quote semantic-unique-tag-table) "semantic-sort" "\
Scan a list of TAGS, removing duplicates.
This must first sort the tags by position ascending.
TAGS are removed only if they are equivalent, as can happen when
multiple tag sources are scanned." nil nil)

(autoload (quote semantic-flatten-tags-table) "semantic-sort" "\
Flatten the tags table TABLE.
All tags in TABLE, and all components of top level tags
in TABLE will appear at the top level of list.
Tags promoted to the top of the list will still appear
unmodified as components of their parent tags." nil nil)

(autoload (quote semantic-bucketize) "semantic-sort" "\
Sort TAGS into a group of buckets based on tag class.
Unknown classes are placed in a Misc bucket.
Type bucket names are defined by either `semantic-symbol->name-assoc-list'.
If PARENT is specified, then TAGS belong to this PARENT in some way.
This will use `semantic-symbol->name-assoc-list-for-type-parts' to
generate bucket names.
Optional argument FILTER is a filter function to be applied to each bucket.
The filter function will take one argument, which is a list of tokens, and
may re-organize the list with side-effects." nil nil)

(defvar semantic-orphaned-member-metaparent-type "class" "\
In `semantic-adopt-external-members', the type of 'type for metaparents.
A metaparent is a made-up type semantic token used to hold the child list
of orphaned members of a named type.")

(autoload (quote semantic-adopt-external-members) "semantic-sort" "\
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
buckets with the bucket function." nil nil)

(autoload (quote semantic-tag-external-member-parent) "semantic-sort" "\
Return a parent for TAG when TAG is an external member.
TAG is an external member if it is defined at a toplevel and
has some sort of label defining a parent.  The parent return will
be a string.

The default behavior, if not overridden with
`tag-member-parent' gets the 'parent extra
specifier of TAG.

If this function is overridden, use
`semantic-tag-external-member-parent-default' to also
include the default behavior, and merely extend your own." nil nil)

(autoload (quote semantic-tag-external-member-p) "semantic-sort" "\
Return non-nil if PARENT is the parent of TAG.
TAG is an external member of PARENT when it is somehow tagged
as having PARENT as it's parent.
PARENT and TAG must both be semantic tags.

The default behavior, if not overridden with
`tag-external-member-p' is to match :parent attribute in
the name of TAG.

If this function is overridden, use
`semantic-tag-external-member-children-p-default' to also
include the default behavior, and merely extend your own." nil nil)

(autoload (quote semantic-tag-external-member-children) "semantic-sort" "\
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
include the default behavior, and merely extend your own." nil nil)

(autoload (quote semantic-tag-external-class) "semantic-sort" "\
Return a list of real tags that faux TAG might represent.

In some languages, a method can be defined on an object which is
not in the same file.  In this case,
`semantic-adopt-external-members' will create a faux-tag.  If it
is necessary to get the tag from which for faux TAG was most
likely derived, then this function is needed." nil nil)

;;;***

;;;### (autoloads (semantic-tag-components-with-overlays semantic-tag-components
;;;;;;  semantic-tag-alias-definition) "semantic-tag" "semantic-tag.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from semantic-tag.el

(autoload (quote semantic-tag-alias-definition) "semantic-tag" "\
Return the definition TAG is an alias.
The returned value is a tag of the class that
`semantic-tag-alias-class' returns for TAG.
The default is to return the value of the :definition attribute.
Return nil if TAG is not of class 'alias." nil nil)

(autoload (quote semantic-tag-components) "semantic-tag" "\
Return a list of components for TAG.
A Component is a part of TAG which itself may be a TAG.
Examples include the elements of a structure in a `type tag,
or the list of arguments to a 'function tag." nil nil)

(autoload (quote semantic-tag-components-with-overlays) "semantic-tag" "\
Return the list of top level components belonging to TAG.
Children are any sub-tags which contain overlays.

Default behavior is to get `semantic-tag-components' in addition
to the components of an anonymous types (if applicable.)

Note for language authors:
  If a mode defines a language tag that has tags in it with overlays
you should still return them with this function.
Ignoring this step will prevent several features from working correctly." nil nil)

;;;***

;;;### (autoloads (semantic-prototype-file semantic-dependency-tag-file
;;;;;;  semantic-go-to-tag) "semantic-tag-file" "semantic-tag-file.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from semantic-tag-file.el

(autoload (quote semantic-go-to-tag) "semantic-tag-file" "\
Go to the location of TAG.
TAG may be a stripped element, in which case PARENT specifies a
parent tag that has position information.
Different behaviors are provided depending on the type of tag.
For example, dependencies (includes) will seek out the file that is
depended on (see `semantic-dependency-tag-file'." nil nil)

(defvar semantic-dependency-include-path nil "\
Defines the include path used when searching for files.
This should be a list of directories to search which is specific
to the file being included.

If `semantic-dependency-tag-file' is overridden for a given
language, this path is most likely ignored.

This function, reguardless of being overriden, caches the located
dependency file location in the tag property `dependency-file'.
If you override this function, you do not need to implement your
own cache.  Each time the buffer is fully reparsed, the cache
will be reset.

TODO: use ffap.el to locate such items.")

(autoload (quote semantic-dependency-tag-file) "semantic-tag-file" "\
Find the filename represented from TAG.
Depends on `semantic-dependency-include-path' for searching.  Always searches
`.' first, then searches additional paths." nil nil)

(autoload (quote semantic-prototype-file) "semantic-tag-file" "\
Return a file in which prototypes belonging to BUFFER should be placed.
Default behavior (if not overridden) looks for a token specifying the
prototype file, or the existence of an EDE variable indicating which
file prototypes belong in." nil nil)

;;;***

;;;### (autoloads (semantic-tag-full-name semantic-tag-static-p semantic-tag-leaf-p
;;;;;;  semantic-tag-abstract-p semantic-tag-protected-p semantic-tag-protection
;;;;;;  semantic-tag-calculate-parent) "semantic-tag-ls" "semantic-tag-ls.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from semantic-tag-ls.el

(autoload (quote semantic-tag-calculate-parent) "semantic-tag-ls" "\
Attempt to calculate the parent of TAG.
The default behavior (if not overriden with `tag-calculate-parent')
is to search a buffer found with TAG, and if externally defined,
search locally, then semanticdb for that tag (when enabled.)" nil nil)

(autoload (quote semantic-tag-protection) "semantic-tag-ls" "\
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
is to return a symbol based on type modifiers." nil nil)

(autoload (quote semantic-tag-protected-p) "semantic-tag-ls" "\
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
@end table" nil nil)

(autoload (quote semantic-tag-abstract-p) "semantic-tag-ls" "\
Return non nil if TAG is abstract.
Optional PARENT is the parent tag of TAG.
In UML, abstract methods and classes have special meaning and behavior
in how methods are overridden.  In UML, abstract methods are italicized.

The default behavior (if not overridden with `tag-abstract-p'
is to return true if `abstract' is in the type modifiers." nil nil)

(autoload (quote semantic-tag-leaf-p) "semantic-tag-ls" "\
Return non nil if TAG is leaf.
Optional PARENT is the parent tag of TAG.
In UML, leaf methods and classes have special meaning and behavior.

The default behavior (if not overridden with `tag-leaf-p'
is to return true if `leaf' is in the type modifiers." nil nil)

(autoload (quote semantic-tag-static-p) "semantic-tag-ls" "\
Return non nil if TAG is static.
Optional PARENT is the parent tag of TAG.
In UML, static methods and attributes mean that they are allocated
in the parent class, and are not instance specific.
UML notation specifies that STATIC entries are underlined." nil nil)

(autoload (quote semantic-tag-full-name) "semantic-tag-ls" "\
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
STREAM-OR-BUFFER with a tag stream value, or nil." nil nil)

;;;***

;;;### (autoloads (semantic-default-texi-setup) "semantic-texi" "semantic-texi.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from semantic-texi.el

(autoload (quote semantic-default-texi-setup) "semantic-texi" "\
Set up a buffer for parsing of Texinfo files." nil nil)

(add-hook (quote texinfo-mode-hook) (quote semantic-default-texi-setup))

;;;***

;;;### (autoloads (semantic-stickyfunc-mode global-semantic-stickyfunc-mode
;;;;;;  global-semantic-stickyfunc-mode semantic-show-parser-state-mode
;;;;;;  global-semantic-show-parser-state-mode global-semantic-show-parser-state-mode
;;;;;;  semantic-show-unmatched-syntax-mode global-semantic-show-unmatched-syntax-mode
;;;;;;  global-semantic-show-unmatched-syntax-mode semantic-highlight-edits-mode
;;;;;;  global-semantic-highlight-edits-mode global-semantic-highlight-edits-mode)
;;;;;;  "semantic-util-modes" "semantic-util-modes.el" (17091 25107))
;;; Generated autoloads from semantic-util-modes.el

(autoload (quote global-semantic-highlight-edits-mode) "semantic-util-modes" "\
Toggle global use of option `semantic-highlight-edits-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle." t nil)

(defvar global-semantic-highlight-edits-mode nil "\
*If non-nil enable global use of variable `semantic-highlight-edits-mode'.
When this mode is enabled, changes made to a buffer are highlighted
until the buffer is reparsed.")

(custom-add-to-group (quote semantic) (quote global-semantic-highlight-edits-mode) (quote custom-variable))

(custom-add-load (quote global-semantic-highlight-edits-mode) (quote semantic-util-modes))

(autoload (quote semantic-highlight-edits-mode) "semantic-util-modes" "\
Minor mode for highlighting changes made in a buffer.
Changes are tracked by semantic so that the incremental parser can work
properly.
This mode will highlight those changes as they are made, and clear them
when the incremental parser accounts for those edits.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled." t nil)

(autoload (quote global-semantic-show-unmatched-syntax-mode) "semantic-util-modes" "\
Toggle global use of option `semantic-show-unmatched-syntax-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle." t nil)

(defvar global-semantic-show-unmatched-syntax-mode nil "\
*If non-nil, enable global use of `semantic-show-unmatched-syntax-mode'.
When this mode is enabled, syntax in the current buffer which the
semantic parser cannot match is highlighted with a red underline.")

(custom-add-to-group (quote semantic) (quote global-semantic-show-unmatched-syntax-mode) (quote custom-variable))

(custom-add-load (quote global-semantic-show-unmatched-syntax-mode) (quote semantic-util-modes))

(autoload (quote semantic-show-unmatched-syntax-mode) "semantic-util-modes" "\
Minor mode to highlight unmatched lexical syntax tokens.
When a parser executes, some elements in the buffer may not match any
parser rules.  These text characters are considered unmatched syntax.
Often time, the display of unmatched syntax can expose coding
problems before the compiler is run.

With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{semantic-show-unmatched-syntax-mode-map}" t nil)

(defvar global-semantic-show-parser-state-mode nil "\
*If non-nil enable global use of `semantic-show-parser-state-mode'.
When enabled, the current parse state of the current buffer is displayed
in the mode line. See `semantic-show-parser-state-marker' for details
on what is displayed.")

(custom-add-to-group (quote semantic) (quote global-semantic-show-parser-state-mode) (quote custom-variable))

(custom-add-load (quote global-semantic-show-parser-state-mode) (quote semantic-util-modes))

(autoload (quote global-semantic-show-parser-state-mode) "semantic-util-modes" "\
Toggle global use of option `semantic-show-parser-state-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle." t nil)

(autoload (quote semantic-show-parser-state-mode) "semantic-util-modes" "\
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
minor mode is enabled." t nil)

(autoload (quote global-semantic-stickyfunc-mode) "semantic-util-modes" "\
Toggle global use of option `semantic-stickyfunc-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle." t nil)

(defvar global-semantic-stickyfunc-mode nil "\
*If non-nil, enable global use of `semantic-stickyfunc-mode'.
This minor mode only works for Emacs 21 or later.
When enabled, the header line is enabled, and the first line
of the current function or method is displayed in it.
This makes it appear that the first line of that tag is
`sticky' to the top of the window.")

(custom-add-to-group (quote semantic) (quote global-semantic-stickyfunc-mode) (quote custom-variable))

(custom-add-load (quote global-semantic-stickyfunc-mode) (quote semantic-util-modes))

(autoload (quote semantic-stickyfunc-mode) "semantic-util-modes" "\
Minor mode to show the title of a tag in the header line.
Enables/disables making the header line of functions sticky.
A function (or other tag class specified by
`semantic-stickyfunc-sticky-classes') has a header line, meaning the
first line which describes the rest of the construct.  This first
line is what is displayed in the Emacs 21 header line.

With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled." t nil)

;;;***

;;;### (autoloads (semanticdb-file-stream semanticdb-file-table-object
;;;;;;  global-semanticdb-minor-mode semanticdb-minor-mode-p semanticdb-global-mode)
;;;;;;  "semanticdb" "semanticdb.el" (17091 25107))
;;; Generated autoloads from semanticdb.el

(defvar semanticdb-global-mode nil "\
*If non-nil enable the use of `semanticdb-minor-mode'.")

(custom-add-to-group (quote semantic) (quote semanticdb-global-mode) (quote custom-variable))

(custom-add-load (quote semanticdb-global-mode) (quote semanticdb))

(autoload (quote semanticdb-minor-mode-p) "semanticdb" "\
Return non-nil if `semanticdb-minor-mode' is active." nil nil)

(autoload (quote global-semanticdb-minor-mode) "semanticdb" "\
Toggle the use of `semanticdb-minor-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle." t nil)

(autoload (quote semanticdb-file-table-object) "semanticdb" "\
Return a semanticdb table belonging to FILE.
If file has database tags available in the database, return it.
If file does not have tags available, then load the file, and create a new
table object for it." nil nil)

(autoload (quote semanticdb-file-stream) "semanticdb" "\
Return a list of tags belonging to FILE.
If file has database tags available in the database, return them.
If file does not have tags available, then load the file, and create them." nil nil)

;;;***

;;;### (autoloads (semanticdb-full-filename semanticdb-live-p semanticdb-file-loaded-p
;;;;;;  semanticdb-project-database-file semanticdb-persistent-path
;;;;;;  semanticdb-default-save-directory semanticdb-default-file-name)
;;;;;;  "semanticdb-file" "semanticdb-file.el" (17091 25107))
;;; Generated autoloads from semanticdb-file.el

(defvar semanticdb-default-file-name "semantic.cache" "\
*File name of the semantic tag cache.")

(defvar semanticdb-default-save-directory nil "\
*Directory name where semantic cache files are stored.
If this value is nil, files are saved in the current directory.  If the value
is a valid directory, then it overrides `semanticdb-default-file-name' and
stores caches in a coded file name in this directory.")

(defvar semanticdb-persistent-path (quote (project)) "\
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

(autoload (quote semanticdb-project-database-file) "semanticdb-file" "\
Database of file tables saved to disk." nil nil)

(autoload (quote semanticdb-file-loaded-p) "semanticdb-file" "\
Return the project belonging to FILENAME if it was already loaded." nil nil)

(autoload (quote semanticdb-live-p) "semanticdb-file" "\
Return non-nil if the file associated with OBJ is live.
Live databases are objects associated with existing directories." nil nil)

(autoload (quote semanticdb-full-filename) "semanticdb-file" "\
Fetch the full filename that OBJ refers to." nil nil)

;;;***

;;;### (autoloads (semanticdb-find-tags-external-children-of-type
;;;;;;  semanticdb-brute-find-tags-by-class semanticdb-brute-deep-find-tags-by-name
;;;;;;  semanticdb-deep-find-tags-for-completion semanticdb-deep-find-tags-by-name-regexp
;;;;;;  semanticdb-deep-find-tags-by-name semanticdb-find-tags-by-class
;;;;;;  semanticdb-find-tags-for-completion semanticdb-find-tags-by-name-regexp
;;;;;;  semanticdb-find-tags-by-name semanticdb-find-tags-collector
;;;;;;  semanticdb-find-table-for-include) "semanticdb-find" "semanticdb-find.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from semanticdb-find.el

(autoload (quote semanticdb-find-table-for-include) "semanticdb-find" "\
For a single INCLUDETAG found in TABLE, find a `semanticdb-table' object
INCLUDETAG is a semantic TAG of class 'include.
TABLE as defined by `semantic-something-to-tag-table' to identify
where the tag came from.  TABLE is optional if INCLUDETAG has an
overlay of :filename attribute." nil nil)

(autoload (quote semanticdb-find-tags-collector) "semanticdb-find" "\
Search for all tags returned by FUNCTION over PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.
If optional argument BRUTISH is non-nil, then ignore include statements,
and search all tables in this project tree." nil nil)

(autoload (quote semanticdb-find-tags-by-name) "semanticdb-find" "\
Search for all tags matching NAME on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer." nil nil)

(autoload (quote semanticdb-find-tags-by-name-regexp) "semanticdb-find" "\
Search for all tags matching REGEXP on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer." nil nil)

(autoload (quote semanticdb-find-tags-for-completion) "semanticdb-find" "\
Search for all tags matching PREFIX on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer." nil nil)

(autoload (quote semanticdb-find-tags-by-class) "semanticdb-find" "\
Search for all tags of CLASS on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer." nil nil)

(autoload (quote semanticdb-deep-find-tags-by-name) "semanticdb-find" "\
Search for all tags matching NAME on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer." nil nil)

(autoload (quote semanticdb-deep-find-tags-by-name-regexp) "semanticdb-find" "\
Search for all tags matching REGEXP on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer." nil nil)

(autoload (quote semanticdb-deep-find-tags-for-completion) "semanticdb-find" "\
Search for all tags matching PREFIX on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer." nil nil)

(autoload (quote semanticdb-brute-deep-find-tags-by-name) "semanticdb-find" "\
Search for all tags matching NAME on PATH.
See `semanticdb-find-translate-path' for details on PATH.
The argument BRUTISH will be set so that searching includes all tables
in the current project.
FIND-FILE-MATCH indicates that any time a matchi is found, the file
associated wit that tag should be loaded into a buffer." nil nil)

(autoload (quote semanticdb-brute-find-tags-by-class) "semanticdb-find" "\
Search for all tags of CLASS on PATH.
See `semanticdb-find-translate-path' for details on PATH.
The argument BRUTISH will be set so that searching includes all tables
in the current project.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer." nil nil)

(autoload (quote semanticdb-find-tags-external-children-of-type) "semanticdb-find" "\
Search for all tags defined outside of TYPE w/ TYPE as a parent.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer." nil nil)

;;;***

;;;### (autoloads (semanticdb-find-nonterminal-by-function semanticdb-find-nonterminal-by-extra-spec-value
;;;;;;  semanticdb-find-nonterminal-by-extra-spec semanticdb-find-nonterminal-by-property
;;;;;;  semanticdb-find-nonterminal-by-type semanticdb-find-nonterminal-by-name-regexp
;;;;;;  semanticdb-find-nonterminal-by-name semanticdb-find-nonterminal-by-token)
;;;;;;  "semanticdb-search" "semanticdb-search.el" (17091 25107))
;;; Generated autoloads from semanticdb-search.el

(autoload (quote semanticdb-find-nonterminal-by-token) "semanticdb-search" "\
Find all occurances of nonterminals with token TOKEN in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES, DIFF-MODE, FIND-FILE-MATCH and IGNORE-SYSTEM.
Return a list ((DB-TABLE . TOKEN-LIST) ...)." nil nil)

(autoload (quote semanticdb-find-nonterminal-by-name) "semanticdb-search" "\
Find all occurances of nonterminals with name NAME in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES, DIFF-MODE, FIND-FILE-MATCH and IGNORE-SYSTEM.
Return a list ((DB-TABLE . TOKEN) ...)." nil nil)

(autoload (quote semanticdb-find-nonterminal-by-name-regexp) "semanticdb-search" "\
Find all occurances of nonterminals with name matching REGEX in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, FIND-FILE-MATCH and IGNORE-SYSTEM.
Return a list ((DB-TABLE . TOKEN-LIST) ...)." nil nil)

(autoload (quote semanticdb-find-nonterminal-by-type) "semanticdb-search" "\
Find all nonterminals with a type of TYPE in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, FIND-FILE-MATCH and IGNORE-SYSTEM.
Return a list ((DB-TABLE . TOKEN-LIST) ...)." nil nil)

(autoload (quote semanticdb-find-nonterminal-by-property) "semanticdb-search" "\
Find all nonterminals with a PROPERTY equal to VALUE in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, FIND-FILE-MATCH and IGNORE-SYSTEM.
Return a list ((DB-TABLE . TOKEN-LIST) ...)." nil nil)

(autoload (quote semanticdb-find-nonterminal-by-extra-spec) "semanticdb-search" "\
Find all nonterminals with a SPEC in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, FIND-FILE-MATCH and IGNORE-SYSTEM.
Return a list ((DB-TABLE . TOKEN-LIST) ...)." nil nil)

(autoload (quote semanticdb-find-nonterminal-by-extra-spec-value) "semanticdb-search" "\
Find all nonterminals with a SPEC equal to VALUE in databases.
See `semanticdb-find-nonterminal-by-function' for details on DATABASES,
SEARCH-PARTS, SEARCH-INCLUDES DIFF-MODE, FIND-FILE-MATCH and IGNORE-SYSTEM.
Return a list ((DB-TABLE . TOKEN-LIST) ...)." nil nil)

(autoload (quote semanticdb-find-nonterminal-by-function) "semanticdb-search" "\
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
Return a list ((DB-TABLE . TOKEN-OR-TOKEN-LIST) ...)." nil nil)

;;;***

;;;### (autoloads (semanticdb-load-system-caches semanticdb-create-system-database
;;;;;;  semanticdb-default-system-save-directory) "semanticdb-system"
;;;;;;  "semanticdb-system.el" (17091 25107))
;;; Generated autoloads from semanticdb-system.el

(defvar semanticdb-default-system-save-directory (expand-file-name "~/.semanticdb") "\
*Directory name where semantic cache files for system headers are stored.
System files cannot have caches stored near them because users rarely have
write permission to such paths.")

(autoload (quote semanticdb-create-system-database) "semanticdb-system" "\
Create a system database starting at PATH.
PATH should be a top level directory for a series of files containing
declarations for SYSTEM files.  In C, this would be header filaes.
CLASS is the class for the database to create.  Only child classes
of symbol `semanticdb-project-database-system' are accepted." t nil)

(autoload (quote semanticdb-load-system-caches) "semanticdb-system" "\
Load all system databases that were previously saved." t nil)

;;;***

;;;### (autoloads (senator-try-expand-semantic global-senator-minor-mode
;;;;;;  senator-minor-mode senator-word-search-backward senator-re-search-backward
;;;;;;  senator-search-backward senator-word-search-forward senator-re-search-forward
;;;;;;  senator-search-forward senator-completion-menu-popup senator-complete-symbol
;;;;;;  senator-jump-regexp senator-jump senator-previous-tag senator-next-tag
;;;;;;  senator-step-at-start-end-tag-classes senator-step-at-tag-classes
;;;;;;  global-senator-minor-mode) "senator" "senator.el" (17091
;;;;;;  25107))
;;; Generated autoloads from senator.el

(defvar global-senator-minor-mode nil "\
*If non-nil enable global use of senator minor mode.")

(custom-add-to-group (quote senator) (quote global-senator-minor-mode) (quote custom-variable))

(custom-add-load (quote global-senator-minor-mode) (quote senator))

(defvar senator-step-at-tag-classes nil "\
*List of tag classes where to step.
A tag class is a symbol like 'variable, 'function, 'type, or other.
If nil navigation steps at any tag found.  This is a buffer local
variable.  It can be set in a mode hook to get a specific langage
navigation.")

(defvar senator-step-at-start-end-tag-classes (quote (function)) "\
*List of tag classes where to step at start and end.
A tag class is a symbol like 'variable, 'function, 'type, or other.
If nil, navigation only step at beginning of tags.  If t, step at
start and end of any tag where it is allowed to step.  Also, stepping
at start and end of a tag prevent stepping inside its components.
This is a buffer local variable.  It can be set in a mode hook to get
a specific langage navigation.")

(autoload (quote senator-next-tag) "senator" "\
Navigate to the next Semantic tag.
Return the tag or nil if at end of buffer." t nil)

(autoload (quote senator-previous-tag) "senator" "\
Navigate to the previous Semantic tag.
Return the tag or nil if at beginning of buffer." t nil)

(autoload (quote senator-jump) "senator" "\
Jump to the semantic symbol SYM.

If optional IN-CONTEXT is non-nil jump in the local type's context
\(see function `senator-current-type-context').  If optional
NO-DEFAULT is non-nil do not provide a default value.

When called interactively you can combine the IN-CONTEXT and
NO-DEFAULT switches like this:

- \\[universal-argument]       IN-CONTEXT.
- \\[universal-argument] -     NO-DEFAULT.
- \\[universal-argument] \\[universal-argument]   IN-CONTEXT + NO-DEFAULT." t nil)

(autoload (quote senator-jump-regexp) "senator" "\
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
- \\[universal-argument] \\[universal-argument]   IN-CONTEXT + NO-DEFAULT." t nil)

(autoload (quote senator-complete-symbol) "senator" "\
Complete the current symbol under point.
If optional argument CYCLE-ONCE is non-nil, only cycle through the list
of completions once, doing nothing where there are no more matches." t nil)

(autoload (quote senator-completion-menu-popup) "senator" "\
Popup a completion menu for the symbol at point.
The popup menu displays all of the possible completions for the symbol
it was invoked on.  To automatically split large menus this function
use `imenu--mouse-menu' to handle the popup menu." t nil)

(autoload (quote senator-search-forward) "senator" "\
Search in tag names forward from point for STRING.
Set point to the end of the occurrence found, and return point.
See also the function `search-forward' for details on the BOUND,
NOERROR and COUNT arguments." t nil)

(autoload (quote senator-re-search-forward) "senator" "\
Search in tag names forward from point for regular expression REGEXP.
Set point to the end of the occurrence found, and return point.
See also the function `re-search-forward' for details on the BOUND,
NOERROR and COUNT arguments." t nil)

(autoload (quote senator-word-search-forward) "senator" "\
Search in tag names forward from point for WORD.
Set point to the end of the occurrence found, and return point.
See also the function `word-search-forward' for details on the BOUND,
NOERROR and COUNT arguments." t nil)

(autoload (quote senator-search-backward) "senator" "\
Search in tag names backward from point for STRING.
Set point to the beginning of the occurrence found, and return point.
See also the function `search-backward' for details on the BOUND,
NOERROR and COUNT arguments." t nil)

(autoload (quote senator-re-search-backward) "senator" "\
Search in tag names backward from point for regular expression REGEXP.
Set point to the beginning of the occurrence found, and return point.
See also the function `re-search-backward' for details on the BOUND,
NOERROR and COUNT arguments." t nil)

(autoload (quote senator-word-search-backward) "senator" "\
Search in tag names backward from point for WORD.
Set point to the beginning of the occurrence found, and return point.
See also the function `word-search-backward' for details on the BOUND,
NOERROR and COUNT arguments." t nil)

(autoload (quote senator-minor-mode) "senator" "\
Toggle senator minor mode.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{senator-mode-map}" t nil)

(autoload (quote global-senator-minor-mode) "senator" "\
Toggle global use of senator minor mode.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle." t nil)

(autoload (quote senator-try-expand-semantic) "senator" "\
Attempt inline completion at the cursor.
Use Semantic, or the semantic database to look up possible
completions.  The argument OLD has to be nil the first call of this
function.  It returns t if a unique, possibly partial, completion is
found, nil otherwise." nil nil)

;;;***

;;;### (autoloads (wisent-parse-toggle-verbose-flag) "wisent" "wisent/wisent.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from wisent/wisent.el

(defvar wisent-parse-verbose-flag nil "\
*non-nil means to issue more messages while parsing.")

(autoload (quote wisent-parse-toggle-verbose-flag) "wisent" "\
Toggle whether to issue more messages while parsing." t nil)

;;;***

;;;### (autoloads (wisent-byte-compile-grammar wisent-compile-grammar
;;;;;;  wisent-toggle-verbose-flag) "wisent-comp" "wisent/wisent-comp.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from wisent/wisent-comp.el

(defvar wisent-verbose-flag nil "\
*non-nil means to report verbose information on generated parser.")

(autoload (quote wisent-toggle-verbose-flag) "wisent-comp" "\
Toggle whether to report verbose information on generated parser." t nil)

(autoload (quote wisent-compile-grammar) "wisent-comp" "\
Compile GRAMMAR and return an LALR(1) automaton.
Optional argument START-LIST is a list of start symbols
\(nonterminals).  If nil the first nonterminal defined in the grammar
is the default start symbol.  If START-LIST contains only one element,
it defines the start symbol.  If START-LIST contains more than one
element, all will be defined as potential start symbols, unless
`wisent-single-start-flag' is non-nil.  In that case the first element
of START-LIST defines the start symbol and others are ignored.

The LALR(1) automaton has the form:

[ACTIONS GOTOS STARTS FUNCTIONS]

- ACTIONS a state/token matrix telling the parser what to do at every
  state based on the current lookahead token.  That is shift, reduce,
  accept or error.

- GOTOS a state/nonterminal matrix telling the parser the next state
  to go to after reducing with each rule.

- STARTS an alist which maps the allowed start symbols (nonterminal)
  to tokens that will be first shifted into the parser stack.

- FUNCTIONS a obarray of semantic action symbols.  A semantic action
  is actually an Elisp function (lambda expression)." nil nil)

(autoload (quote wisent-byte-compile-grammar) "wisent-comp" "\
Byte compile the `wisent-compile-grammar' FORM.
Automatically called by the Emacs Lisp byte compiler as a
`byte-compile' handler." nil nil)

(put (quote wisent-compile-grammar) (quote byte-compile) (quote wisent-byte-compile-grammar))

;;;***

;;;### (autoloads (wisent-debug-show-entry wisent-cancel-debug-on-entry
;;;;;;  wisent-debug-on-entry) "wisent-debug" "wisent/wisent-debug.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from wisent/wisent-debug.el

(autoload (quote wisent-debug-on-entry) "wisent-debug" "\
Request AUTOMATON's FUNCTION to invoke debugger each time it is called.
FUNCTION must be a semantic action symbol that exists in AUTOMATON." t nil)

(autoload (quote wisent-cancel-debug-on-entry) "wisent-debug" "\
Undo effect of \\[wisent-debug-on-entry] on AUTOMATON's FUNCTION.
FUNCTION must be a semantic action symbol that exists in AUTOMATON." t nil)

(autoload (quote wisent-debug-show-entry) "wisent-debug" "\
Show the source of AUTOMATON's semantic action FUNCTION.
FUNCTION must be a semantic action symbol that exists in AUTOMATON." t nil)

;;;***

;;;### (autoloads (wisent-grammar-mode) "wisent-grammar" "wisent/wisent-grammar.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from wisent/wisent-grammar.el

(autoload (quote wisent-grammar-mode) "wisent-grammar" "\
Major mode for editing Wisent grammars." t nil)

(add-to-list (quote auto-mode-alist) (quote ("\\.wy$" . wisent-grammar-mode)))

(eval-after-load "speedbar" (quote (speedbar-add-supported-extension ".wy")))

;;;***

;;;### (autoloads (wisent-java-default-setup) "wisent-java-tags"
;;;;;;  "wisent/wisent-java-tags.el" (17091 25107))
;;; Generated autoloads from wisent/wisent-java-tags.el

(autoload (quote wisent-java-default-setup) "wisent-java-tags" "\
Hook run to setup Semantic in `java-mode'.
Use the alternate LALR(1) parser." nil nil)

(add-hook (quote java-mode-hook) (function wisent-java-default-setup))

;;;***

;;;### (autoloads (wisent-python-default-setup) "wisent-python" "wisent/wisent-python.el"
;;;;;;  (17091 25107))
;;; Generated autoloads from wisent/wisent-python.el

(autoload (quote wisent-python-default-setup) "wisent-python" "\
Setup buffer for parse." nil nil)

(add-hook (quote python-mode-hook) (quote wisent-python-default-setup))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; semantic-loaddefs.el ends here
