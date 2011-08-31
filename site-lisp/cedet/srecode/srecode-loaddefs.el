;;; srecode-loaddefs.el --- Auto-generated CEDET autoloads
;;
;;; Code:


;;;### (autoloads (srecode-semantic-handle-:kill srecode-semantic-handle-:system
;;;;;;  srecode-semantic-handle-:file srecode-semantic-handle-:time
;;;;;;  srecode-semantic-handle-:user srecode-semantic-handle-:region
;;;;;;  srecode-semantic-handle-:indent srecode-semantic-handle-:blank)
;;;;;;  "srecode-args" "srecode-args.el" (18857 63844))
;;; Generated autoloads from srecode-args.el

(autoload 'srecode-semantic-handle-:blank "srecode-args" "\
Add macros into the dictionary DICT specifying blank line spacing.
The wrapgap means make sure the first and last lines of the macro
do not contain any text from preceeding or following text.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:indent "srecode-args" "\
Add macros into the dictionary DICT for indentation.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:region "srecode-args" "\
Add macros into the dictionary DICT based on the current :region.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:user "srecode-args" "\
Add macros into the dictionary DICT based on the current :user.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:time "srecode-args" "\
Add macros into the dictionary DICT based on the current :time.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:file "srecode-args" "\
Add macros into the dictionary DICT based on the current :file.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:system "srecode-args" "\
Add macros into the dictionary DICT based on the current :system.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:kill "srecode-args" "\
Add macros into the dictionary DICT based on the kill ring.

\(fn DICT)" nil nil)

;;;***

;;;### (autoloads (srecode-compile-templates srecode-compile-file)
;;;;;;  "srecode-compile" "srecode-compile.el" (18857 63844))
;;; Generated autoloads from srecode-compile.el

(autoload 'srecode-compile-file "srecode-compile" "\
Compile the templates from the file FNAME.

\(fn FNAME)" nil nil)

(autoload 'srecode-compile-templates "srecode-compile" "\
Compile a semantic recode template file into a mode-local variable.

\(fn)" t nil)

;;;***

;;;### (autoloads (srecode-semantic-handle-:cpp) "srecode-cpp" "srecode-cpp.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from srecode-cpp.el

(autoload 'srecode-semantic-handle-:cpp "srecode-cpp" "\
Add macros into the dictionary DICT based on the current c++ file.
Adds the following:
FILENAME_SYMBOL - filename converted into a C compat symbol.
HEADER - Shown section if in a header file.

\(fn DICT)" nil nil)

;;;***

;;;### (autoloads (srecode-calculate-context) "srecode-ctxt" "srecode-ctxt.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from srecode-ctxt.el

(autoload 'srecode-calculate-context "srecode-ctxt" "\
Calculate the context at the current point.
The returned context is a list, with the top-most context first.
Each returned context is a string that that would show up in a `context'
statement in an `.srt' file.

Some useful context values used by the provided srecode templates are:
  \"file\" - Templates that for a file (such as an empty file.)
     \"empty\" - The file is empty
  \"declaration\" - Top-level declarations in a file.
     \"include\" - In or near include statements
     \"package\" - In or near provide statements
     \"function\" - In or near function statements
         \"NAME\" - Near functions within NAME namespace or class
     \"variable\" - In or near variable statements.
     \"type\"     - In or near type declarations.
     \"comment\"  - In a comment
  \"classdecl\" - Declarations within a class/struct/etc.
     \"variable\" - In or near class fields
     \"function\" - In or near methods/functions
        \"virtual\" - Nearby items are virtual
           \"pure\" - and those virtual items are pure virtual
     \"type\"     - In or near type declarations.
     \"comment\"  - In a comment in a block of code
     -- these items show up at the end of the context list. --
     \"public\", \"protected\", \"private\" -
                  In or near a section of public/pritected/private entries.
  \"code\" - In a block of code.
     \"string\" - In a string in a block of code
     \"comment\"  - In a comment in a block of code

    ... More later.

\(fn)" nil nil)

;;;***

;;;### (autoloads (srecode-dictionary-dump srecode-adebug-dictionary
;;;;;;  srecode-create-dictionary) "srecode-dictionary" "srecode-dictionary.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from srecode-dictionary.el

(eieio-defclass-autoload 'srecode-dictionary 'nil "srecode-dictionary" "Dictionary of symbols and what they mean.\nDictionaries are used to look up named symbols from\ntemplates to decide what to do with those symbols.")

(eieio-defclass-autoload 'srecode-dictionary-compound-value 'nil "srecode-dictionary" "A compound dictionary value.\nValues stored in a dictionary must be a STRING,\na dictionary for showing sections, or an instance of a subclass\nof this class.\n\nCompound dictionary values derive from this class, and must\nprovide a sequence of method implementations to convert into\na string.")

(eieio-defclass-autoload 'srecode-dictionary-compound-variable '(srecode-dictionary-compound-value) "srecode-dictionary" "A compound dictionary value for template file variables.\nYou can declare a variable in a template like this:\n\nset NAME \"str\" macro \"OTHERNAME\"\n\nwith appending various parts together in a list.")

(autoload 'srecode-create-dictionary "srecode-dictionary" "\
Create a dictionary for BUFFER.
If BUFFER-OR-PARENT is not specified, assume a buffer, and
use the current buffer.
If BUFFER-OR-PARENT is another dictionary, then remember the
parent within the new dictionary, and assume that BUFFER
is the same as belongs to the parent dictionary.
The dictionary is initialized with variables setup for that
buffer's table.
If BUFFER-OR-PARENT is t, then this dictionary should not be
assocated with a buffer or parent.

\(fn &optional BUFFER-OR-PARENT)" nil nil)

(autoload 'srecode-adebug-dictionary "srecode-dictionary" "\
Run data-debug on this mode's dictionary.

\(fn)" t nil)

(autoload 'srecode-dictionary-dump "srecode-dictionary" "\
Dump a typical fabricated dictionary.

\(fn)" t nil)

;;;***

;;;### (autoloads (srecode-document-function-comment-extract-test
;;;;;;  srecode-document-insert-group-comments srecode-document-insert-variable-one-line-comment
;;;;;;  srecode-document-insert-function-comment srecode-document-insert-comment)
;;;;;;  "srecode-document" "srecode-document.el" (18857 63844))
;;; Generated autoloads from srecode-document.el

(eval-after-load "srecode-mode" '(progn (srecode-add-code-generator 'srecode-document-insert-comment "Comments" "C")))

(autoload 'srecode-document-insert-comment "srecode-document" "\
Insert some comments.
Whack any comments that may be in the way and replace them.
If the region is active, then insert group function comments.
If the cursor is in a comment, figure out what kind of comment it is
  and replace it.
If the cursor is in a function, insert a function comment.
If the cursor is on a one line prototype, then insert post-fcn comments.

\(fn)" t nil)

(autoload 'srecode-document-insert-function-comment "srecode-document" "\
Insert or replace a function comment.
FCN-IN is the Semantic tag of the function to add a comment too.
If FCN-IN is not provied, the current tag is used instead.
It is assumed that the comment occurs just in front of FCN-IN.

\(fn &optional FCN-IN)" t nil)

(autoload 'srecode-document-insert-variable-one-line-comment "srecode-document" "\
Insert or replace a variable comment.
VAR-IN is the Semantic tag of the function to add a comment too.
If VAR-IN is not provied, the current tag is used instead.
It is assumed that the comment occurs just after VAR-IN.

\(fn &optional VAR-IN)" t nil)

(autoload 'srecode-document-insert-group-comments "srecode-document" "\
Insert group comments around the active between BEG and END.
If the region includes only parts of some tags, expand out
to the beginning and end of the tags on the region.
If there is only one tag in the region, complain.

\(fn BEG END)" t nil)

(autoload 'srecode-document-function-comment-extract-test "srecode-document" "\
Test old comment extraction.
Dump out the extracted dictionary.

\(fn)" t nil)

;;;***

;;;### (autoloads (srecode-semantic-handle-:el-custom srecode-semantic-handle-:el)
;;;;;;  "srecode-el" "srecode-el.el" (18857 63844))
;;; Generated autoloads from srecode-el.el

(autoload 'srecode-semantic-handle-:el "srecode-el" "\
Add macros into the dictionary DICT based on the current Emacs Lisp file.
Adds the following:
  PRENAME - The common name prefix of this file.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:el-custom "srecode-el" "\
Add macros into the dictionary DICT based on the current Emacs Lisp file.
Adds the following:
  GROUP - The 'defgroup' name we guess you want for variables.
  FACEGROUP - The `defgroup' name you might want for faces.

\(fn DICT)" nil nil)

;;;***

;;;### (autoloads (srecode-insert-prototype-expansion) "srecode-expandproto"
;;;;;;  "srecode-expandproto.el" (18857 63844))
;;; Generated autoloads from srecode-expandproto.el

(autoload 'srecode-insert-prototype-expansion "srecode-expandproto" "\
Insert get/set methods for the current class.

\(fn)" t nil)

;;;***

;;;### (autoloads (srecode-field-utest) "srecode-fields" "srecode-fields.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from srecode-fields.el

(eieio-defclass-autoload 'srecode-field '(srecode-overlaid) "srecode-fields" "Representation of one field.")

(autoload 'srecode-field-utest "srecode-fields" "\
Test the srecode field manager.

\(fn)" t nil)

;;;***

;;;### (autoloads (srecode-comment-prefix) "srecode-filters" "srecode-filters.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from srecode-filters.el

(autoload 'srecode-comment-prefix "srecode-filters" "\
Prefix each line of STR with the comment prefix characters.

\(fn STR)" nil nil)

;;;***

;;;### (autoloads (srecode-read-template-name srecode-template-get-table-for-binding
;;;;;;  srecode-template-get-table srecode-load-tables-for-mode srecode-table)
;;;;;;  "srecode-find" "srecode-find.el" (18857 63844))
;;; Generated autoloads from srecode-find.el

(autoload 'srecode-table "srecode-find" "\
Return the currently active Semantic Recoder table for this buffer.
Optional argument MODE specifies the mode table to use.

\(fn &optional MODE)" nil nil)

(autoload 'srecode-load-tables-for-mode "srecode-find" "\
Load all the template files for MMODE.
Templates are found in the SRecode Template Map.
See `srecode-get-maps' for more.
APPNAME is the name of an application.  In this case,
all template files for that application will be loaded.

\(fn MMODE &optional APPNAME)" nil nil)

(autoload 'srecode-template-get-table "srecode-find" "\
Find in the template in mode table TAB, the template with TEMPLATE-NAME.
Optional argument CONTEXT specifies a context a particular template
would belong to.
Optional argument APPLICATION restricts searches to only template tables
belonging to a specific application.  If APPLICATION is nil, then only
tables that do not belong to an application will be searched.

\(fn (TAB srecode-mode-table) TEMPLATE-NAME &optional CONTEXT APPLICATION)" nil nil)

(autoload 'srecode-template-get-table-for-binding "srecode-find" "\
Find in the template name in mode table TAB, the template with BINDING.
Optional argument CONTEXT specifies a context a particular template
would belong to.
Optional argument APPLICATION restricts searches to only template tables
belonging to a specific application.  If APPLICATION is nil, then only
tables that do not belong to an application will be searched.

\(fn (TAB srecode-mode-table) BINDING &optional CONTEXT APPLICATION)" nil nil)

(autoload 'srecode-read-template-name "srecode-find" "\
Completing read for Semantic Recoder template names.
PROMPT is used to query for the name of the template desired.
INITIAL is the initial string to use.
HIST is a history variable to use.
DEFAULT is what to use if the user presses RET.

\(fn PROMPT &optional INITIAL HIST DEFAULT)" nil nil)

;;;***

;;;### (autoloads (srecode-insert-getset) "srecode-getset" "srecode-getset.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from srecode-getset.el

(eval-after-load "srecode-mode" '(progn (srecode-add-code-generator 'srecode-insert-getset "Get/Set" "G")))

(autoload 'srecode-insert-getset "srecode-getset" "\
Insert get/set methods for the current class.
CLASS-IN is the semantic tag of the class to update.
FIELD-IN is the semantic tag, or string name, of the field to add.
If you do not specify CLASS-IN or FIELD-IN then a class and field
will be derived.

\(fn &optional CLASS-IN FIELD-IN)" t nil)

;;;***

;;;### (autoloads (srecode-insert-fcn srecode-insert srecode-insert-again)
;;;;;;  "srecode-insert" "srecode-insert.el" (18857 63844))
;;; Generated autoloads from srecode-insert.el

(autoload 'srecode-insert-again "srecode-insert" "\
Insert the previously inserted template (by name) again.

\(fn)" t nil)

(autoload 'srecode-insert "srecode-insert" "\
Inesrt the template TEMPLATE-NAME into the current buffer at point.
DICT-ENTRIES are additional dictionary values to add.

\(fn TEMPLATE-NAME &rest DICT-ENTRIES)" t nil)

(autoload 'srecode-insert-fcn "srecode-insert" "\
Insert TEMPLATE using DICTIONARY into STREAM.

\(fn TEMPLATE DICTIONARY &optional STREAM)" nil nil)

;;;***

;;;### (autoloads (srecode-semantic-handle-:java) "srecode-java"
;;;;;;  "srecode-java.el" (18857 63844))
;;; Generated autoloads from srecode-java.el

(autoload 'srecode-semantic-handle-:java "srecode-java" "\
Add macros into the dictionary DICT based on the current java file.
Adds the following:
FILENAME_AS_PACKAGE - file/dir converted into a java package name.
FILENAME_AS_CLASS - file converted to a Java class name.

\(fn DICT)" nil nil)

;;;***

;;;### (autoloads (srecode-adebug-maps srecode-get-maps) "srecode-map"
;;;;;;  "srecode-map.el" (18857 63844))
;;; Generated autoloads from srecode-map.el

(autoload 'srecode-get-maps "srecode-map" "\
Get a list of maps relevant to the current buffer.
Optional argument RESET forces a reset of the current map.

\(fn &optional RESET)" t nil)

(autoload 'srecode-adebug-maps "srecode-map" "\
Run ADEBUG on the output of `srecode-get-maps'.

\(fn)" t nil)

;;;***

;;;### (autoloads (global-srecode-minor-mode srecode-minor-mode)
;;;;;;  "srecode-mode" "srecode-mode.el" (18857 63844))
;;; Generated autoloads from srecode-mode.el

(autoload 'srecode-minor-mode "srecode-mode" "\
Toggle srecode minor mode.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{srecode-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'global-srecode-minor-mode "srecode-mode" "\
Toggle global use of srecode minor mode.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (srecode-semantic-insert-tag srecode-semantic-apply-tag-to-dict-default
;;;;;;  srecode-semantic-apply-tag-to-dict srecode-semantic-handle-:tagtype
;;;;;;  srecode-semantic-handle-:tag) "srecode-semantic" "srecode-semantic.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from srecode-semantic.el

(autoload 'srecode-semantic-handle-:tag "srecode-semantic" "\
Add macroes into the dictionary DICT based on the current :tag.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:tagtype "srecode-semantic" "\
Add macroes into the dictionary DICT based on a tag of class type at point.
Assumes the cursor is in a tag of class type.  If not, throw an error.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-apply-tag-to-dict "srecode-semantic" "\
Insert fewatures of TAGOBJ into the dictionary DICT.
TAGOBJ is an object of class `srecode-semantic-tag'.  This class
is a compound inserter value.
DICT is a dictionary object.
At a minimum, this function will create dictionary macro for NAME.
It is also likely to create macros for TYPE (data type), function arguments,
variable default values, and other things.

\(fn TAGOBJ DICT)" nil nil)

(autoload 'srecode-semantic-apply-tag-to-dict-default "srecode-semantic" "\
Insert features of TAGOBJ into dictionary DICT.

\(fn TAGOBJ DICT)" nil nil)

(autoload 'srecode-semantic-insert-tag "srecode-semantic" "\
Insert TAG into a buffer useing srecode templates at point.

Optional STYLE-OPTION is a list of minor configuration of styles,
such as the symbol 'prototype for prototype functions, or
'system for system includes, and 'doxygen, for a doxygen style
comment.

Optional third argument POINT-INSERT-FCN is a hook that is run after
TAG is inserted that allows an opportunity to fill in the body of
some thing.  This hook function is called with one argument, the TAG
being inserted.

The rest of the arguments are DICT-ENTRIES.  DICT-ENTRIES
is of the form ( NAME1 VALUE1 NAME2 VALUE2 ... NAMEn VALUEn).

The exact template used is based on the current context.
The template used is found within the toplevel context as calculated
by `srecode-calculate-context', such as `declaration', `classdecl',
or `code'.

For various conditions, this function looks for a template with
the name CLASS-tag, where CLASS is the tag class.  If it cannot
find that, it will look for that template in the
`declaration'context (if the current context was not `declaration').

If PROTOTYPE is specified, it will first look for templates with
the name CLASS-tag-prototype, or CLASS-prototype as above.

See `srecode-semantic-apply-tag-to-dict' for details on what is in
the dictionary when the templates are called.

This function returns to location in the buffer where the
inserted tag ENDS, and will leave point inside the inserted
text based on any occurance of a point-inserter.  Templates such
as `function' will leave point where code might be inserted.

\(fn TAG &optional STYLE-OPTION POINT-INSERT-FCN &rest DICT-ENTRIES)" nil nil)

;;;***

;;;### (autoloads (srecode-semantic-handle-:srt srecode-read-major-mode-name
;;;;;;  srecode-read-variable-name) "srecode-srt" "srecode-srt.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from srecode-srt.el

(autoload 'srecode-read-variable-name "srecode-srt" "\
Read in the name of a declaired variable in the current SRT file.
PROMPT is the prompt to use.
INITIAL is the initial string.
HIST is the history value, otherwise `srecode-read-variable-name-history'
     is used.
DEFAULT is the default if RET is hit.

\(fn PROMPT &optional INITIAL HIST DEFAULT)" nil nil)

(autoload 'srecode-read-major-mode-name "srecode-srt" "\
Read in the name of a desired `major-mode'.
PROMPT is the prompt to use.
INITIAL is the initial string.
HIST is the history value, otherwise `srecode-read-variable-name-history'
     is used.
DEFAULT is the default if RET is hit.

\(fn PROMPT &optional INITIAL HIST DEFAULT)" nil nil)

(autoload 'srecode-semantic-handle-:srt "srecode-srt" "\
Add macros into the dictionary DICT based on the current SRT file.
Adds the following:
ESCAPE_START - This files value of escape_start
ESCAPE_END - This files value of escape_end
MODE - The mode of this buffer.  If not declared yet, guess.

\(fn DICT)" nil nil)

;;;***

;;;### (autoloads (srecode-dump-templates srecode-mode-table-new
;;;;;;  srecode-get-mode-table) "srecode-table" "srecode-table.el"
;;;;;;  (18857 63844))
;;; Generated autoloads from srecode-table.el

(autoload 'srecode-get-mode-table "srecode-table" "\
Get the SRecoder mode table for the major mode MODE.
Optional argument SOFT indicates to not make a new one if a table
was not found.

\(fn MODE)" nil nil)

(autoload 'srecode-mode-table-new "srecode-table" "\
Create a new template table for MODE in FILE.
INIT are the initialization parametrs for the new template table.

\(fn MODE FILE &rest INIT)" nil nil)

(autoload 'srecode-dump-templates "srecode-table" "\
Dump a list of the current templates for MODE.

\(fn MODE)" t nil)

;;;***

;;;### (autoloads (srecode-template-setup-parser) "srecode-template"
;;;;;;  "srecode-template.el" (18857 63844))
;;; Generated autoloads from srecode-template.el

(autoload 'srecode-template-setup-parser "srecode-template" "\
Setup buffer for parse.

\(fn)" nil nil)

(add-hook 'srecode-template-mode-hook 'srecode-template-setup-parser)

;;;***

;;;### (autoloads (srecode-template-mode) "srecode-template-mode"
;;;;;;  "srecode-template-mode.el" (18857 63844))
;;; Generated autoloads from srecode-template-mode.el

(autoload 'srecode-template-mode "srecode-template-mode" "\
Major-mode for writing srecode macros.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.srt$" . srecode-template-mode))

;;;***

;;;### (autoloads (srecode-utest-template-output) "srecode-test"
;;;;;;  "srecode-test.el" (18857 63844))
;;; Generated autoloads from srecode-test.el

(autoload 'srecode-utest-template-output "srecode-test" "\
Test various template insertion options.

\(fn)" t nil)

;;;***

;;;### (autoloads (srecode-utest-getset-output) "srecode-test-getset"
;;;;;;  "srecode-test-getset.el" (18857 63844))
;;; Generated autoloads from srecode-test-getset.el

(autoload 'srecode-utest-getset-output "srecode-test-getset" "\
Test various template insertion options.

\(fn)" t nil)

;;;***

;;;### (autoloads (srecode-semantic-handle-:texitag srecode-semantic-handle-:texi
;;;;;;  srecode-texi-add-menu) "srecode-texi" "srecode-texi.el" (18857
;;;;;;  63844))
;;; Generated autoloads from srecode-texi.el

(autoload 'srecode-texi-add-menu "srecode-texi" "\
Add an item into the current menu.  Add @node statements as well.
Argument NEWNODE is the name of the new node.

\(fn NEWNODE)" t nil)

(autoload 'srecode-semantic-handle-:texi "srecode-texi" "\
Add macros into the dictionary DICT based on the current texinfo file.
Adds the following:
  LEVEL - chapter, section, subsection, etc
  NEXTLEVEL - One below level

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:texitag "srecode-texi" "\
Add macros into the dictionary DICT based on the current :tag file.
Adds the following:
  TAGDOC - Texinfo formatted doc string for :tag.

\(fn DICT)" nil nil)

(define-mode-local-override semantic-insert-foreign-tag texinfo-mode (foreign-tag) "Insert TAG from a foreign buffer in TAGFILE.\nAssume TAGFILE is a source buffer, and create a documentation\nthingy from it using the `document' tool." (let ((srecode-semantic-selected-tag foreign-tag)) (srecode-insert "declaration:function")))

;;;***

;;;### (autoloads nil nil ("srecode-document-vars.el" "srecode-extract.el"
;;;;;;  "srecode-load.el" "srecode-template-wy.el" "srecode.el")
;;;;;;  (19926 52526 578223))

;;;***

(provide 'srecode-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; srecode-loaddefs.el ends here
