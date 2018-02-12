;;; ox-hugo.el --- Hugo Markdown Back-End for Org Export Engine  -*- lexical-binding: t -*-

;; Authors: Kaushal Modi <kaushal.mod@gmail.com>
;;          Matt Price <moptop99@gmail.com>
;; URL: https://ox-hugo.scripter.co
;; Package-Requires: ((emacs "24.5") (org "9.0"))
;; Keywords: Org, markdown, docs
;; Version: 0.6

;;; Commentary:

;; ox-hugo implements a Markdown back-end for Org exporter.  The
;; exported Markdown is compatible with the Hugo static site generator
;; (https://gohugo.io/).  This exporter also generates the post
;; front-matter in TOML or YAML.

;; To start using this exporter, add the below to your Emacs config:
;;
;;   (with-eval-after-load 'ox
;;     (require 'ox-hugo))
;;
;; With the above evaluated, the ox-hugo exporter options will be
;; available in the Org Export Dispatcher.  The ox-hugo export
;; commands have bindings beginning with "H" (for Hugo).
;;
;; Commonly used export commands:
;;
;; ## For one-post-per-subtree flow, where a single Org file can have
;;    multiple Org subtrees which export to individual Hugo posts:
;;
;;    - C-c C-e H H  ->  Export the *current* 'valid Hugo post subtree'
;;                        to a Hugo post in Markdown.
;;
;;    - C-c C-e H A  ->  Export *all* 'valid Hugo post subtrees' to
;;                        Hugo posts in Markdown.
;;
;; ## For one-post-per-file flow, where a single Org file exports to
;;    only *one* Hugo post:
;;
;;    - C-c C-e H h  ->  Export the Org file to a Hugo post in Markdown.

;; Do M-x customize-group, and select `org-export-hugo' to see the
;; available customization options for this package.

;; See this package's website for more instructions and examples:
;;
;;   https://ox-hugo.scripter.co

;;; Code:

(require 'ox-blackfriday)
(require 'ffap)                         ;For `ffap-url-regexp'
(require 'ob-core)                      ;For `org-babel-parse-header-arguments'

(defvar ffap-url-regexp)                ;Silence byte-compiler

(defvar org-hugo--subtree-coord nil
  "Variable to store the current valid Hugo subtree coordinates.
It holds the value returned by
`org-hugo--get-post-subtree-coordinates'.")

(defvar org-hugo--subtree-count nil
  "Variable to count of number of subtrees getting exported.
This variable is used when exporting all subtrees in a file.")

(defvar org-hugo-allow-export-after-save t
  "Enable flag for `org-hugo-export-subtree-to-md-after-save'.
When nil, the above function will not export the Org file to
Hugo-compatible Markdown.

This variable is usually set to nil by the user in
`org-capture-before-finalize-hook' and set to t again in
`org-capture-after-finalize-hook', so that the export does not
happen as soon as a new post is created using Org capture.

Note that the export after save will not work until
`org-hugo-export-subtree-to-md-after-save' is added to the
`after-save-hook' by the user.")

(defvar org-hugo-blackfriday-options
  '("taskLists"
    "smartypants"
    "smartypantsQuotesNBSP"
    "angledQuotes"
    "fractions"
    "smartDashes"
    "latexDashes"
    "hrefTargetBlank"
    "plainIDAnchors"
    "extensions"
    "extensionsmask")
  "Blackfriday option names as used inside Hugo.
Note that these names are case-sensitive.

This is a list of strings.

Stable Hugo version reference:
https://gohugo.io/content-management/formats/#blackfriday-options

Development Hugo version reference:
https://github.com/gohugoio/hugo/blob/master/docs/content/readfiles/bfconfig.md

taskLists
- default: `true'
- Purpose: `false' turns off GitHub-style automatic task/TODO list
           generation.

smartypants
- default: `true'
- Purpose: `false' disables smart punctuation substitutions, including
           smart quotes, smart dashes, smart fractions, etc.  If
           `true', it may be fine-tuned with the `angledQuotes',
           `fractions', `smartDashes', and `latexDashes' flags.

smartypantsQuotesNBSP
- default: `false'
- Purpose: `true' enables French style Guillemets with non-breaking
           space inside the quotes.

angledQuotes
- default: `false'
- Purpose: `true' enables smart, angled double quotes.
           Example: \"Hugo\" renders to «Hugo» instead of “Hugo”.

fractions
- default: `true'
- Purpose: `false' disables smart fractions.
- Example: 5/12 renders to 5⁄12(<sup>5</sup>&frasl;<sub>12</sub>).
- Caveat:  Even with \"fractions = false\", Blackfriday still converts
           1/2, 1/4, and 3/4 respectively to ½ (&frac12;), ¼
           (&frac14;) and ¾ (&frac34;), but only these three.

smartDashes
- default: `true'
- Purpose: `false' disables smart dashes; i.e., the conversion of
           multiple hyphens into an en-dash or em-dash.  If `true',
           its behavior can be modified with the `latexDashes' flag.

latexDashes
- default: `true'
- Purpose: `false' disables LaTeX-style smart dashes and selects
           conventional smart dashes.  Assuming `smartDashes': If
           `true', -- is translated into – (&ndash;), whereas ---
           is translated into — (&mdash;).  However, spaced single
           hyphen between two words is translated into an en dash
           e.g., \"12 June - 3 July\" becomes \"12 June &ndash; 3
           July\" upon rendering.

hrefTargetBlank
- default: `false'
- Purpose: `true' opens external links in a new window or tab.

plainIDAnchors
- default: `true'
- Purpose: `true' renders any heading and footnote IDs without the
           document ID.
- Example: renders \"#my-heading\" instead of
           \"#my-heading:bec3ed8ba720b970\".

extensions
- default: []
- Purpose: Enable one or more Blackfriday's Markdown extensions (if
           they aren't Hugo defaults).
- Example: Include `hardLineBreak' in the list to enable Blackfriday's
           EXTENSION_HARD_LINK_BREAK.

extensionsmask
- default: []
- Purpose: Enable one or more of Blackfriday's Markdown extensions (if
           they aren't Hugo defaults). Example: Include `autoHeaderIds'
           as `false' in the list to disable Blackfriday's
           EXTENSION_AUTO_HEADER_IDS

See `org-hugo-blackfriday-extensions' for valid Blackfriday
extensions.")

(defvar org-hugo-blackfriday-extensions
  '("noIntraEmphasis"
    "tables"
    "fencedCode"
    "autolink"
    "strikethrough"
    "laxHtmlBlocks"
    "spaceHeaders"
    "hardLineBreak"
    "tabSizeEight"
    "footnotes"
    "noEmptyLineBeforeBlock"
    "headerIds"
    "titleblock"
    "autoHeaderIds"
    "backslashLineBreak"
    "definitionLists"
    "joinLines")
  "Blackfriday extension names as used inside Hugo.
Note that these names are case-sensitive.

This is a list of strings.

Stable Hugo version reference:
https://gohugo.io/content-management/formats/#blackfriday-extensions

Development Hugo version references:
https://github.com/gohugoio/hugo/blob/master/docs/content/readfiles/bfconfig.md
https://github.com/russross/blackfriday#extensions
https://github.com/russross/blackfriday/blob/master/markdown.go
https://github.com/gohugoio/hugo/blob/master/helpers/content.go

noIntraEmphasis
- default: enabled
- Purpose: The \"_\" character is commonly used inside words when
           discussing code, so having Markdown interpret it as an
           emphasis command is usually the wrong thing.  When enabled,
           Blackfriday lets you treat all emphasis markers as normal
           characters when they occur inside a word.

tables
- default: enabled
- Purpose: When enabled, tables can be created by drawing them in the
           input using the below syntax:
- Example:
           Name    | Age
           --------|------
           Bob     | 27
           Alice   | 23

fencedCode
- default: enabled
- Purpose: When enabled, in addition to the normal 4-space indentation
           to mark code blocks, you can explicitly mark them and
           supply a language (to make syntax highlighting simple).

           You can use 3 or more backticks to mark the beginning of
           the block, and the same number to mark the end of the
           block.
- Example:
           ```emacs-lisp
           (message \"Hello\")
           ```

autolink
- default: enabled
- Purpose: When enabled, URLs that have not been explicitly marked as
           links will be converted into links.

strikethrough
- default: enabled
- Purpose: When enabled, text wrapped with two tildes will be crossed
           out.
- Example: ~~crossed-out~~

laxHtmlBlocks
- default: disabled
- Purpose: When enabled, loosen up HTML block parsing rules.
           «Needs more information»

spaceHeaders
- default: enabled
- Purpose: When enabled, be strict about prefix header rules.
           «Needs more information»

hardLineBreak
- default: disabled
- Purpose: When enabled, newlines in the input translate into line
           breaks in the output, like in Org verse blocks.

tabSizeEight
- default: disabled
- Purpose: When enabled, expand tabs to eight spaces instead of four.

footnotes
- default: enabled
- Purpose: When enabled, Pandoc-style footnotes will be supported.
           The footnote marker in the text that will become a
           superscript text; the footnote definition will be placed in
           a list of footnotes at the end of the document.
- Example:
           This is a footnote.[^1]

           [^1]: the footnote text.

noEmptyLineBeforeBlock
- default: disabled
- Purpose: When enabled, no need to insert an empty line to start a
           (code, quote, ordered list, unordered list) block.

headerIds
- default: enabled
- Purpose: When enabled, allow specifying header IDs with {#id}.

titleblock
- default: disabled
- Purpose: When enabled, support Pandoc-style title blocks.
           http://pandoc.org/MANUAL.html#extension-pandoc_title_block

autoHeaderIds
- default: enabled
- Purpose: When enabled, auto-create the header ID's from the headline
           text.

backslashLineBreak
- default: enabled
- Purpose: When enabled, translate trailing backslashes into line
           breaks.

definitionLists
- default: enabled
- Purpose: When enabled, a simple definition list is made of a
           single-line term followed by a colon and the definition for
           that term.
- Example:
           Cat
           : Fluffy animal everyone likes

           Internet
           : Vector of transmission for pictures of cats

           Terms must be separated from the previous definition by a
           blank line.

joinLines
- default: enabled
- Purpose: When enabled, delete newlines and join the lines.  This
           behavior is similar to the default behavior in Org.")

(defvar org-hugo--internal-list-separator "\n"
  "String used to separate elements in list variables.
Examples are variables holding Hugo tags, categories, aliases.
This variable is for internal use only, and must not be
modified.")

(defvar org-hugo--date-time-regexp (concat "\\`[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}"
                                           "\\(?:T[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}"
                                           "\\(?:Z\\|[+-][[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}\\)*\\)*\\'")
  "Regexp to match the Hugo time stamp strings.

Reference: https://tools.ietf.org/html/rfc3339#section-5.8

Examples:
  2017-07-31
  2017-07-31T17:05:38
  2017-07-31T17:05:38Z
  2017-07-31T17:05:38+04:00
  2017-07-31T17:05:38-04:00.")


;;; User-Configurable Variables

(defgroup org-export-hugo nil
  "Options for exporting Org mode files to Hugo-compatible Markdown."
  :tag "Org Export Hugo"
  :group 'org-export
  :version "25.2")

(defcustom org-hugo-front-matter-format "toml"
  "Format used to front matter.
This variable can be set to either \"toml\" or \"yaml\"."
  :group 'org-export-hugo
  :type '(choice
          (const :tag "TOML" "toml")
          (const :tag "YAML" "yaml")))

(defcustom org-hugo-default-section-directory "posts"
  "Default section for Hugo posts.

This variable is the name of the directory under the \"content/\"
directory where all Hugo posts should go by default."
  :group 'org-export-hugo
  :type 'directory
  :safe 'stringp)

(defcustom org-hugo-footer ""
  "String to be appended at the end of each Hugo post.

The string needs to be in a Hugo-compatible Markdown format or HTML."
  :group 'org-export-hugo
  :type 'string
  :safe 'stringp)

(defcustom org-hugo-preserve-filling t
  "When non-nil, text filling done in Org will be retained in Markdown."
  :group 'org-export-hugo
  :type 'boolean
  :safe #'booleanp)

(defcustom org-hugo-delete-trailing-ws t
  "When non-nil, delete trailing whitespace in Markdown output.
Trailing empty lines at the end of the Markdown output are also deleted.

One might want to set this variable to nil if they want to
preserve the trailing whitespaces in Markdown for the purpose of
forcing line-breaks.

The trailing whitespace deleting is skipped if
`org-export-preserve-breaks' is set to non-nil; either via that
variable or via the OPTIONS keyword \"\\n:t\" (See (org) Export
settings).

\(In below Markdown, underscores are used to represent spaces.)

    abc__
    def__

Those trailing whitespaces render to \"<br />\" tags in the Hugo
generated HTML.  But the same result can also be achived by using the
Org Verse block or Blackfriday hardLineBreak extension."
  :group 'org-export-hugo
  :type 'boolean
  :safe #'booleanp)

(defcustom org-hugo-use-code-for-kbd nil
  "When non-nil, ~text~ will translate to <kbd>text</kbd>."
  :group 'org-export-hugo
  :type 'boolean
  :safe #'booleanp)

(defcustom org-hugo-prefer-hyphen-in-tags t
  "When non-nil, replace single underscores in Org tags with hyphens.

See `org-hugo--transform-org-tags' for more information.

This variable affects the Hugo tags and categories set via Org tags
using the \"@\" prefix."
  :group 'org-export-hugo
  :type 'boolean
  :safe #'booleanp)

(defcustom org-hugo-allow-spaces-in-tags t
  "When non-nil, replace double underscores in Org tags with spaces.

See `org-hugo--transform-org-tags' for more information.

This variable affects the Hugo tags and categories set via Org tags
using the \"@\" prefix."
  :group 'org-export-hugo
  :type 'boolean
  :safe #'booleanp)

(defcustom org-hugo-langs-no-descr-in-code-fences '()
  "List of languages whose descriptors should not be exported to Markdown.

This variable is effective only if the HUGO_CODE_FENCE option is set.

If `pygmentsCodeFences' is `true', and if a language is not supported
by Pygments, the HTML of that fenced code block is not rendered
correctly by Hugo.  In such cases, it is better to leave out the
language descriptor and allow the code block to render as an Org
example block.

This issue is with the Pygments syntax highlighter, not with the
Chroma syntax highlighter (which is the default in Hugo since
v0.28).

Example value: (org)."
  :group 'org-export-hugo
  :type '(repeat symbol)
  :safe #'listp)

(defcustom org-hugo-auto-set-lastmod nil
  "When non-nil, set the lastmod field in front-matter to current time."
  :group 'org-export-hugo
  :type 'boolean
  :safe #'booleanp)

(defcustom org-hugo-export-with-toc nil
  "When non-nil, Markdown format TOC will be inserted.

The TOC contains headlines with levels up
to`org-export-headline-levels'.  When an integer, include levels
up to N in the toc, this may then be different from
`org-export-headline-levels', but it will not be allowed to be
larger than the number of headline levels.  When nil, no table of
contents is made.

This option can also be set with the OPTIONS keyword,
e.g. \"toc:nil\", \"toc:t\" or \"toc:3\"."
  :group 'org-export-hugo
  :type '(choice
          (const :tag "No Table of Contents" nil)
          (const :tag "Full Table of Contents" t)
          (integer :tag "TOC to level"))
  :safe (lambda (x) (or (booleanp x)
                   (integerp x))))

(defcustom org-hugo-export-with-section-numbers nil
  "Configuration for adding section numbers to headlines.

When set to `onlytoc', none of the headlines will be numbered in
the exported post body, but TOC generation will use the section
numbers.

When set to an integer N, numbering will only happen for
headlines whose relative level is higher or equal to N.

When set to any other non-nil value, numbering will happen for
all the headlines.

This option can also be set with the OPTIONS keyword,
e.g. \"num:onlytoc\", \"num:nil\", \"num:t\" or \"num:3\"."
  :group 'org-export-hugo
  :type '(choice
          (const :tag "Don't number only in body" 'onlytoc)
          (const :tag "Don't number any headline" nil)
          (const :tag "Number all headlines" t)
          (integer :tag "Number to level"))
  :safe (lambda (x) (or (booleanp x)
                   (integerp x))))

(defcustom org-hugo-default-static-subdirectory-for-externals "ox-hugo"
  "Default sub-directory in Hugo static directory for external files.
If the source path for external files does not contain
\"static\", `ox-hugo` cannot know what directory structure to
create inside the Hugo static directory.  So all such files are
copied to this sub-directory inside the Hugo static directory."
  :group 'org-export-hugo
  :type 'string
  :safe 'stringp)

(defcustom org-hugo-external-file-extensions-allowed-for-copying
  '("jpg" "jpeg" "tiff" "png" "svg"
    "pdf" "odt")
  "List of external file extensions allowed for copying to Hugo static dir.
If an Org link references a file with one of these extensions,
and if that file is not in the Hugo static directory, that file
is copied over to the static directory.

The auto-copying behavior is disabled if this variable is set to nil."
  :group 'org-export-hugo
  :type '(repeat string))


;;; Define Back-End

(org-export-define-derived-backend 'hugo 'blackfriday ;hugo < blackfriday < md < html
  :menu-entry
  '(?H "Export to Hugo-compatible Markdown"
       ((?H "Subtree to file"
            (lambda (a _s v _b)
              (org-hugo-export-subtree-to-md nil a v)))
        (?h "To file"
            (lambda (a s v _b)
              (org-hugo-export-to-md a s v)))
        (?O "Subtree to file and open"
            (lambda (a _s v _b)
              (if a
                  (org-hugo-export-subtree-to-md nil :async v)
                (org-open-file (org-hugo-export-subtree-to-md nil a v)))))
        (?o "To file and open"
            (lambda (a s v _b)
              (if a (org-hugo-export-to-md t s v)
                (org-open-file (org-hugo-export-to-md nil s v)))))
        (?A "All subtrees to files"
            (lambda (a _s v _b)
              (org-hugo-export-subtree-to-md :all-subtrees a v)))
        (?t "To temporary buffer"
            (lambda (a s v _b)
              (org-hugo-export-as-md a s v)))))
  :translate-alist '((code . org-hugo-kbd-tags-maybe)
                     (example-block . org-hugo-example-block)
                     (headline . org-hugo-headline)
                     (inner-template . org-hugo-inner-template)
                     (keyword . org-hugo-keyword)
                     (link . org-hugo-link)
                     (paragraph . org-hugo-paragraph)
                     (src-block . org-hugo-src-block))
  :filters-alist '((:filter-body . org-hugo-body-filter))

  ;;                KEY                       KEYWORD                    OPTION  DEFAULT                     BEHAVIOR
  :options-alist '(;; Variables not setting the front-matter directly
                   (:with-toc nil "toc" org-hugo-export-with-toc)
                   (:section-numbers nil "num" org-hugo-export-with-section-numbers)
                   (:with-smart-quotes nil "'" nil) ;Don't use smart quotes; that is done automatically by Blackfriday
                   (:with-special-strings nil "-" nil) ;Don't use special strings for ndash, mdash; that is done automatically by Blackfriday
                   (:with-sub-superscript nil "^" '{}) ;Require curly braces to be wrapped around text to sub/super-scripted
                   (:hugo-front-matter-format "HUGO_FRONT_MATTER_FORMAT" nil     org-hugo-front-matter-format)
                   (:hugo-level-offset "HUGO_LEVEL_OFFSET" nil 1)
                   (:hugo-preserve-filling "HUGO_PRESERVE_FILLING" nil org-hugo-preserve-filling) ;Preserve breaks so that text filling in Markdown matches that of Org
                   (:hugo-delete-trailing-ws "HUGO_DELETE_TRAILING_WS" nil org-hugo-delete-trailing-ws)
                   (:hugo-section "HUGO_SECTION" nil org-hugo-default-section-directory)
                   (:hugo-base-dir "HUGO_BASE_DIR" nil nil)
                   (:hugo-code-fence "HUGO_CODE_FENCE" nil t)
                   (:hugo-menu "HUGO_MENU" nil nil)
                   (:hugo-menu-override "HUGO_MENU_OVERRIDE" nil nil)
                   (:hugo-use-code-for-kbd "HUGO_USE_CODE_FOR_KBD" nil org-hugo-use-code-for-kbd)
                   (:hugo-prefer-hyphen-in-tags "HUGO_PREFER_HYPHEN_IN_TAGS" nil org-hugo-prefer-hyphen-in-tags)
                   (:hugo-allow-spaces-in-tags "HUGO_ALLOW_SPACES_IN_TAGS" nil org-hugo-allow-spaces-in-tags)
                   (:hugo-auto-set-lastmod "HUGO_AUTO_SET_LASTMOD" nil org-hugo-auto-set-lastmod)
                   (:hugo-custom-front-matter "HUGO_CUSTOM_FRONT_MATTER" nil nil)
                   (:hugo-blackfriday "HUGO_BLACKFRIDAY" nil nil)

                   ;; Front matter variables
                   ;; https://gohugo.io/content-management/front-matter/#front-matter-variables
                   ;; aliases
                   (:hugo-aliases "HUGO_ALIASES" nil nil space)
                   ;; date
                   ;; "date" is parsed from the Org #+DATE or subtree property EXPORT_HUGO_DATE
                   (:date "DATE" nil nil)
                   ;; description
                   (:description "DESCRIPTION" nil nil)
                   ;; draft
                   ;; "draft" value is also interpreted by TODO state
                   ;; of a post as Org subtree.
                   (:hugo-draft "HUGO_DRAFT" nil nil)
                   ;; expiryDate
                   (:hugo-expirydate "HUGO_EXPIRYDATE" nil nil)
                   ;; isCJKLanguage
                   (:hugo-iscjklanguage "HUGO_ISCJKLANGUAGE" nil nil)
                   ;; keywords
                   ;; "keywords" is parsed from the Org #+KEYWORDS or
                   ;; subtree property EXPORT_KEYWORDS.
                   (:keywords "KEYWORDS" nil nil newline)
                   ;; layout
                   (:hugo-layout "HUGO_LAYOUT" nil nil)
                   ;; lastmod
                   (:hugo-lastmod "HUGO_LASTMOD" nil nil)
                   ;; linkTitle
                   (:hugo-linktitle "HUGO_LINKTITLE" nil nil)
                   ;; markup
                   (:hugo-markup "HUGO_MARKUP" nil nil) ;default is "md"
                   ;; outputs
                   (:hugo-outputs "HUGO_OUTPUTS" nil nil)
                   ;; publishDate
                   (:hugo-publishdate "HUGO_PUBLISHDATE" nil nil)
                   ;; slug
                   (:hugo-slug "HUGO_SLUG" nil nil)
                   ;; taxomonomies - tags, categories
                   (:hugo-tags "HUGO_TAGS" nil nil newline)
                   ;; #+HUGO_TAGS are used to set the post tags in Org
                   ;; files written for file-based exports.  But for
                   ;; subtree-based exports, the EXPORT_HUGO_TAGS
                   ;; property can be used to override inherited tags
                   ;; and Org-style tags.
                   (:hugo-categories "HUGO_CATEGORIES" nil nil newline)
                   ;; #+HUGO_CATEGORIES are used to set the post
                   ;; categories in Org files written for file-based
                   ;; exports.  But for subtree-based exports, the
                   ;; EXPORT_HUGO_CATEGORIES property can be used to
                   ;; override inherited categories and Org-style
                   ;; categories (Org-style tags with "@" prefix).

                   ;; title
                   ;; "title" is parsed from the Org #+TITLE or the subtree heading.
                   ;; type
                   (:hugo-type "HUGO_TYPE" nil nil)
                   ;; url
                   (:hugo-url "HUGO_URL" nil nil)
                   ;; weight
                   (:hugo-weight "HUGO_WEIGHT" nil nil)))


;;; Miscellaneous Helper Functions

(defun org-hugo--plist-get-true-p (info key)
  "Return non-nil if KEY in INFO is non-nil.
Return nil if the value of KEY in INFO is nil, \"nil\" or \"\".

This is a special version of `plist-get' used only for keys that
are expected to hold a boolean value.

INFO is a plist used as a communication channel."
  (let ((value (plist-get info key)))
    ;; (message "dbg: org-hugo--plist-get-true-p:: key:%S value:%S" key value)
    (cond
     ((or (equal t value)
          (equal nil value))
      value)
     ((and (stringp value)
           (string= value "nil"))
      nil)
     (t
      ;; "" -> nil
      ;; "t" -> "t"
      ;; "anything else" -> "anything else"
      ;; 123 -> nil
      (org-string-nw-p value)))))

;; Workaround to retain the :hl_lines parameter in src-block headers
;; post `org-babel-exp-code'.
;; http://lists.gnu.org/archive/html/emacs-orgmode/2017-10/msg00300.html
(defun org-hugo--org-babel-exp-code (orig-fun &rest args)
  "Return the original code block formatted for export.
ORIG-FUN is the original function `org-babel-exp-code' that this
function is designed to advice using `:around'.  ARGS are the
arguments of the ORIG-FUN.

This advice retains the `:hl_lines' parameter, if added to any
source block.  This parameter is used in `org-hugo-src-block'.

This advice is added to the ORIG-FUN only while an ox-hugo export
is in progress.  See `org-hugo--before-export-function' and
`org-hugo--after-export-function'."
  (let* ((param-keys-to-be-retained '(:hl_lines))
         (info (car args))
         (parameters (nth 2 info))
         (ox-hugo-params-str (let ((str ""))
                               (dolist (param parameters)
                                 (dolist (retain-key param-keys-to-be-retained)
                                   (when (equal retain-key (car param))
                                     (setq str (concat str " "
                                                       (symbol-name retain-key) " "
                                                       (cdr param))))))
                               (org-string-nw-p (org-trim str))))
         ret)
    ;; (message "[ox-hugo ob-exp] info: %S" info)
    ;; (message "[ox-hugo ob-exp] parameters: %S" parameters)
    ;; (message "[ox-hugo ob-exp] ox-hugo-params-str: %S" ox-hugo-params-str)
    (setq ret (apply orig-fun args))
    (when ox-hugo-params-str
      (setq ret (replace-regexp-in-string "\\`#\\+BEGIN_SRC .*" (format "\\& %s" ox-hugo-params-str) ret)))
    ;; (message "[ox-hugo ob-exp] ret: %S" ret)
    ret))

(defun org-hugo--before-export-function ()
  "Function to be run before an ox-hugo export.

This function is called in the very beginning of
`org-hugo-export-to-md', `org-hugo-export-as-md' and
`org-hugo-publish-to-md'.

This is an internal function."
  (advice-add 'org-babel-exp-code :around #'org-hugo--org-babel-exp-code))

(defun org-hugo--after-export-function ()
  "Function to be run after an ox-hugo export.

This function is called in the very end of
`org-hugo-export-to-md', `org-hugo-export-as-md' and
`org-hugo-publish-to-md'.

This is an internal function."
  (advice-remove 'org-babel-exp-code #'org-hugo--org-babel-exp-code))

(defun org-hugo--get-headline-number (headline info &optional toc)
  "Return htmlized section number for the HEADLINE.
INFO is a plist used as a communication channel.

When the \"num\" export option is `onlytoc', headline number is
returned only if the optional argument TOC is non-nil.

Return nil if there is no headline number, or if it has been
disabled."
  (let ((onlytoc (equal 'onlytoc (plist-get info :section-numbers))))
    (when (and (if toc
                   t
                 (not onlytoc)) ;If `toc' is nil, but `onlytoc' is non-nil, return nil
               (org-export-numbered-headline-p headline info))
      (let ((number-str (mapconcat
                         'number-to-string
                         (org-export-get-headline-number headline info) ".")))
        (format "<span class=\"section-num\">%s</span> " number-str)))))

(defun org-hugo--build-toc (info &optional n keyword local)
  "Return table of contents as a string.

INFO is a plist used as a communication channel.

Optional argument N, when non-nil, is a positive integer
specifying the depth of the table.

Optional argument KEYWORD specifies the TOC keyword, if any, from
which the table of contents generation has been initiated.

When optional argument LOCAL is non-nil, build a table of
contents according to the current headline."
  (let* ((toc-headline
          (unless local
            (let ((style (plist-get info :md-headline-style))
                  (loffset (plist-get info :hugo-level-offset))
                  (title "Table of Contents"))
              (org-hugo--headline-title style 1 loffset title))))
         (toc-items
          (mapconcat
           (lambda (headline)
             (let* ((level (org-export-get-relative-level headline info))
                    (indentation (make-string (* 4 (1- level)) ?\s))
                    (headline-num-list (org-export-get-headline-number headline info))
                    (number (if headline-num-list
                                ;; (message "[ox-hugo TOC DBG] headline-num-list: %S" headline-num-list)
                                (org-hugo--get-headline-number headline info :toc)
                              ""))
                    (title (org-export-data (org-element-property :title headline) info))
                    (toc-entry
                     (format "[%s](#%s)"
                             (org-export-data-with-backend
                              (org-export-get-alt-title headline info)
                              (org-export-toc-entry-backend 'hugo)
                              info)
                             (or (org-element-property :CUSTOM_ID headline)
                                 (org-hugo-slug title)
                                 ;; (org-export-get-reference headline info)
                                 )))
                    (tags (and (plist-get info :with-tags)
                               (not (eq 'not-in-toc (plist-get info :with-tags)))
                               (let ((tags (org-export-get-tags headline info)))
                                 (and tags
                                      (format ":%s:"
                                              (mapconcat #'identity tags ":")))))))
               ;; (message "[ox-hugo build-toc DBG] level:%d, number:%s" level number)
               (concat indentation "- " number toc-entry tags)))
           (org-export-collect-headlines info n (and local keyword))
           "\n"))                       ;Newline between TOC items
         ;; Remove blank lines from in-between TOC items, which can
         ;; get introduced when using the "UNNUMBERED: t" headline
         ;; property.
         (toc-items (org-string-nw-p
                     (replace-regexp-in-string "\n\\{2,\\}" "\n" toc-items))))
    ;; (message "[ox-hugo build-toc DBG] toc-items:%s" toc-items)
    (when toc-items
      (concat (when (string-match-p "^\\s-*\\-\\s-<span class=\"section\\-num\"" toc-items)
                ;; Hide the bullets if section numbers are present for
                ;; even one heading.
                (concat "<style>\n"
                        "  .ox-hugo-toc ul {\n"
                        "    list-style: none;\n"
                        "  }\n"
                        "</style>\n"))
              "<div class=\"ox-hugo-toc toc\">\n" ;This is a nasty workaround
              "<div></div>\n"        ;till Hugo/Blackfriday support
              toc-headline           ;wrapping Markdown in HTML div's.
              toc-items ;https://github.com/kaushalmodi/ox-hugo/issues/93
              "\n"
              "</div>\n"
              ;; Special comment that can be use to filter out the TOC
              ;; from .Summary in Hugo templates.
              ;;
              ;;     {{ $summary_splits := split .Summary "<!--endtoc-->" }}
              ;;     {{ if eq (len $summary_splits) 2 }}
              ;;         <!-- If that endtoc special comment is present, output only the part after that comment as Summary. -->
              ;;         {{ index $summary_splits 1 | safeHTML }}
              ;;     {{ else }}
              ;;         <!-- Print the whole Summary if endtoc special comment is not found. -->
              ;;         {{ .Summary }}
              ;;     {{ end }}
              "<!--endtoc-->\n"))))

(defalias 'org-hugo--has-caption-p 'org-html--has-caption-p
  "Non-nil when ELEMENT has a caption affiliated keyword.
This function is meant to be used as a predicate for
`org-export-get-ordinal'.")

(defun org-hugo--escape-hugo-shortcode (code lang)
  "Escape Hugo shortcodes if present in CODE string.

The escaping is enabled only if LANG is \"md\".

 - Shortcode with Markdown    : {{% foo %}} -> {{%/* foo */%}}

 - Shortcode without Markdown : {{< foo >}} -> {{</* foo */>}}

Return the escaped/unescaped string."
  (if (string= lang "md")
      (replace-regexp-in-string
       "\\({{<\\)\\([^}][^}]*\\)\\(>}}\\)" "\\1/*\\2*/\\3"
       (replace-regexp-in-string
        "\\({{%\\)\\([^}][^}]*\\)\\(%}}\\)" "\\1/*\\2*/\\3" code))
    code))



;;; Transcode Functions

;;;; Code (<kdb> tags)
(defun org-hugo-kbd-tags-maybe (verbatim _contents info)
  "Wrap text in VERBATIM object with HTML kbd tags.
The kdb wrapping is done if `org-hugo-use-code-for-kbd' is non-nil.

CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (if (org-hugo--plist-get-true-p info :hugo-use-code-for-kbd)
      (format "<kbd>%s</kbd>" (org-element-property :value verbatim))
    (org-md-verbatim verbatim nil nil)))

;;;; Example Block
(defun org-hugo-example-block (example-block _contents info)
  "Transcode an EXAMPLE-BLOCK element into Markdown format.

If the example blocks are *not* set to be exported with line
numbers (See (org) Literal examples), Markdown style
triple-backquoted code blocks with \"text\" \\='language\\=' are
created.

Otherwise, a \"text\" \\='language\\=' code block wrapped in Hugo
\"highlight\" shortcode (See
https://gohugo.io/content-management/syntax-highlighting) is
created.

CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((text (org-export-format-code-default example-block info))
        ;; See `org-element-example-block-parser' for all EXAMPLE-BLOCK properties.
        (number-lines (org-element-property :number-lines example-block))) ;Non-nil if -n or +n switch is used
    (if number-lines
        (let* ((linenostart-str (progn
                                  ;; Extract the start line number of the example block.
                                  (string-match "\\`\\([0-9]+\\)\\s-\\{2\\}" text)
                                  (match-string-no-properties 1 text)))
               (linenos-str (format "\"linenos=table, linenostart=%s\"" linenostart-str)))
          ;; Remove Org-inserted numbers from the beginning of each
          ;; line as the Hugo highlight shortcode will be used instead
          ;; of literally inserting the line numbers.
          (setq text (replace-regexp-in-string "^[0-9]+\\s-\\{2\\}" "" text))
          (format "{{< highlight text %s>}}\n%s{{< /highlight >}}\n" linenos-str text))
      (org-blackfriday-example-block example-block nil info))))

;;;; Headline
(defun org-hugo-headline (headline contents info)
  "Transcode HEADLINE element into Markdown format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((numbers (org-hugo--get-headline-number headline info nil))
           (level (org-export-get-relative-level headline info))
           (title (org-export-data (org-element-property :title headline) info))
           (todo (and (org-hugo--plist-get-true-p info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword
                                    headline)))
                        (and todo (concat (org-export-data todo info) " ")))))
           (tags (and (org-hugo--plist-get-true-p info :with-tags)
                      (let ((tag-list (org-export-get-tags headline info)))
                        (and tag-list
                             (format "     :%s:"
                                     (mapconcat #'identity tag-list ":"))))))
           (priority
            (and (org-hugo--plist-get-true-p info :with-priority)
                 (let ((char (org-element-property :priority headline)))
                   (and char (format "[#%c] " char)))))
           ;; Headline text without tags.
           (heading (concat todo priority title))
           (style (plist-get info :md-headline-style)))
      ;; (message "[ox-hugo-headline DBG] num: %s" numbers)
      (cond
       ;; Cannot create a headline.  Fall-back to a list.
       ((or (org-export-low-level-p headline info)
            (not (memq style '(atx setext)))
            (and (eq style 'atx) (> level 6))
            (and (eq style 'setext) (> level 2)))
        (let ((bullet
               (if (not (org-export-numbered-headline-p headline info)) "-"
                 (concat (number-to-string
                          (car (last (org-export-get-headline-number
                                      headline info))))
                         "."))))
          (concat bullet (make-string (- 4 (length bullet)) ?\s) heading tags "\n\n"
                  (and contents (replace-regexp-in-string "^" "    " contents)))))
       (t
        (let ((anchor (format "{#%s}" ;https://gohugo.io/extras/crossreferences/
                              (or (org-element-property :CUSTOM_ID headline)
                                  (org-hugo-slug title)
                                  ;; (org-export-get-reference headline info)
                                  )))
              (loffset (plist-get info :hugo-level-offset))
              (todo (when todo
                      (concat (org-html--todo todo info) " "))))
          (concat (org-hugo--headline-title style level loffset title todo anchor numbers)
                  contents)))))))

;;;;; Headline Helpers
;;;###autoload
(defun org-hugo-slug (str)
  "Return a slug string for STR.
STR is in Markdown format, most likely a Markdown heading.  The
returned slug string has the following specification:

- Should contain only lower case alphabet, number and hyphen
  characters.
- Remove *any* HTML tag like \"<code>..</code>\", \"<span
  class=..>..</span>\", etc from STR if present.
- URLs if present in STR should be removed.
- Replace \".\" in STR with \"and\", and \"&\" with \"and\".
- Parentheses should be replaced with double-hyphens ( \"foo (bar)
  baz\" becomes \"foo--bar--baz\").
- One or more consecutive spaces should be replaced with a single
  hyphen.
- Maximum number of consecutive hyphens allowed is two.
- No hyphens should be present at the leading or trailing end of the
  returned string ."
  (let* (;; All lower-case
         (str (downcase str))
         ;; Remove "<FOO>..</FOO>" HTML tags if present.
         (str (replace-regexp-in-string "<\\(?1:[a-z]+\\)[^>]*>.*</\\1>" "" str))
         ;; Remove URLs if present in the string.  The ")" in the
         ;; below regexp is the closing parenthesis of a Markdown
         ;; link: [Desc](Link).
         (str (replace-regexp-in-string (concat "\\](" ffap-url-regexp "[^)]+)") "]" str))
         ;; Replace "&" with " and ".
         (str (replace-regexp-in-string "&" " and " str))
         ;; Replace "." with " dot ".
         (str (replace-regexp-in-string "\\." " dot " str))
         ;; Replace all characters except alphabets, numbers and
         ;; parentheses with spaces.
         (str (replace-regexp-in-string "[^[:alnum:]()]" " " str))
         ;; Remove leading and trailing whitespace.
         (str (replace-regexp-in-string "\\(^[[:space:]]*\\|[[:space:]]*$\\)" "" str))
         ;; Replace 2 or more spaces with a single space.
         (str (replace-regexp-in-string "[[:space:]]\\{2,\\}" " " str))
         ;; Replace parentheses with double-hyphens.
         (str (replace-regexp-in-string "\\s-*([[:space:]]*\\([^)]+?\\)[[:space:]]*)\\s-*" " -\\1- " str))
         ;; Remove any remaining parentheses character.
         (str (replace-regexp-in-string "[()]" "" str))
         ;; Replace spaces with hyphens.
         (str (replace-regexp-in-string " " "-" str))
         ;; Remove leading and trailing hyphens.
         (str (replace-regexp-in-string "\\(^[-]*\\|[-]*$\\)" "" str)))
    str))

(defun org-hugo--headline-title (style level loffset title &optional todo anchor numbers)
  "Generate a headline title in the preferred Markdown headline style.

STYLE is the preferred style (`atx' or `setext').
LEVEL is the header level.
LOFFSET is the offset (a non-negative number) that is added to the
Markdown heading level for `atx' style.
TITLE is the headline title.

Optional argument TODO is the Org TODO string.

Optional argument ANCHOR is the Hugo anchor tag for the section as a
string.

Optional argument NUMBERS, if non-nil, is an htmlized string
containing the TITLE's number."
  (let ((headline (concat todo numbers title " " anchor "\n")))
    ;; Use "Setext" style
    (if (and (eq style 'setext) (< level 3))
        (let* ((underline-char (if (= level 1) ?= ?-))
               (underline (concat (make-string (length headline) underline-char)
                                  "\n")))
          (concat "\n" headline underline "\n"))
      ;; Use "Atx" style
      ;; Always translate level N Org headline to level N+1 Markdown
      ;; headline because Markdown level 1 headline and HTML title both
      ;; get the HTML <h1> tag, and we do not want the top-most heading
      ;; of a post to look the exact same as the post's title.
      (let ((level-mark (make-string (+ loffset level) ?#)))
        (concat "\n" level-mark " " headline "\n")))))

;;;; Inner Template
(defun org-hugo-inner-template (contents info)
  "Return body of document after converting it to Hugo-compatible Markdown.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ((toc-level (plist-get info :with-toc))
         (toc-level (if (and toc-level
                             (not (wholenump toc-level)))
                        (plist-get info :headline-levels)
                      toc-level))
         (toc (if (and toc-level
                       (wholenump toc-level)
                       (> toc-level 0)) ;TOC will be exported only if toc-level is positive
                  (concat (org-hugo--build-toc info toc-level) "\n")
                "")))
    ;; (message "[org-hugo-inner-template DBG] toc-level: %s" toc-level)
    (org-trim (concat
               toc
               contents
               ;; Make sure CONTENTS is separated from table of contents
               ;; and footnotes with at least a blank line.
               "\n"
               (org-blackfriday-footnote-section info)))))

;;;; Keyword
(defun org-hugo-keyword (keyword contents info)
  "Transcode a KEYWORD element into Hugo-compatible Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((kwd (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (cond
     ((and (equal "HUGO" kwd)           ;Hugo summary splitting
           (stringp value)
           (string-match-p "\\`\\s-*more\\s-*\\'" value))
      ;; https://gohugo.io/content-management/summaries#user-defined-manual-summary-splitting
      "<!--more-->")
     ((and (equal "TOC" kwd)
           (string-match-p "\\<headlines\\>" value))
      (let ((depth (and (string-match "\\<[0-9]+\\>" value)
                        (string-to-number (match-string 0 value))))
            (local? (string-match-p "\\<local\\>" value)))
        (when (and depth
                   (> depth 0))
          (org-remove-indentation
           (org-hugo--build-toc info depth keyword local?)))))
     (t
      (org-md-keyword keyword contents info)))))

;;;; Links
(defun org-hugo-link (link contents info)
  "Convert LINK to Markdown format.

CONTENTS is the link's description.
INFO is a plist used as a communication channel.

Unlike `org-md-link', this function will also copy local images
and rewrite link paths to make blogging more seamless."
  (let ((link-org-files-as-md
         (lambda (raw-path)
           ;; Treat links to `file.org' as links to `file.md'.
           (if (string= ".org" (downcase (file-name-extension raw-path ".")))
               (concat (file-name-sans-extension raw-path) ".md")
             raw-path)))
        (raw-path (org-element-property :path link))
        (type (org-element-property :type link)))
    ;; (message "[ox-hugo-link DBG] link path: %s" (org-element-property :path link))
    ;; (message "[ox-hugo-link DBG] link filename: %s" (expand-file-name (plist-get (car (cdr link)) :path)))
    ;; (message "[ox-hugo-link DBG] link type: %s" type)
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link contents 'md))
     ((member type '("custom-id" "id" "fuzzy"))
      (let ((destination (if (string= type "fuzzy")
                             (org-export-resolve-fuzzy-link link info)
                           (org-export-resolve-id-link link info))))
        (pcase (org-element-type destination)
          (`plain-text                  ;External file
           (let ((path (funcall link-org-files-as-md destination)))
             (if contents
                 (format "[%s](%s)" contents path)
               (format "<%s>" path))))
          (`headline                 ;Links of type [[* Some heading]]
           (let ((title (org-export-data (org-element-property :title destination) info)))
             ;; (message "[ox-hugo-link DBG] headline title: %s" title)
             (format
              "[%s](#%s)"
              ;; Description
              (cond ((org-string-nw-p contents))
                    ((org-export-numbered-headline-p destination info)
                     (mapconcat #'number-to-string
                                (org-export-get-headline-number destination info)
                                "."))
                    (t title))
              ;; Reference
              (or (org-element-property :CUSTOM_ID destination)
                  (org-hugo-slug title)
                  ;; (org-export-get-reference destination info)
                  ))))
          (_
           (let ((description
                  (or (org-string-nw-p contents)
                      (let ((number (org-export-get-ordinal
                                     destination info
                                     nil #'org-hugo--has-caption-p)))
                        (cond
                         ((not number) nil)
                         ((atom number) (number-to-string number))
                         (t (mapconcat #'number-to-string number ".")))))))
             ;; (message "[ox-hugo-link DBG] link description: %s" description)
             (when description
               (format "[%s](#%s)"
                       description
                       (org-export-get-reference destination info))))))))
     ((org-export-inline-image-p link org-html-inline-image-rules)
      ;; (message "[org-hugo-link DBG] processing an image: %s" contents)
      (let* ((path (org-hugo--attachment-rewrite-maybe raw-path info))
             (parent (org-export-get-parent link))
             (parent-type (org-element-type parent))
             ;; If this is a hyper-linked image, it's parent type will
             ;; be a link too. Get the parent of *that* link in that
             ;; case.
             (grand-parent (when (eq parent-type 'link)
                             (org-export-get-parent parent)))
             (useful-parent (if grand-parent
                                grand-parent
                              parent))
             (label (let ((lbl (and (org-element-property :name useful-parent)
                                    (org-export-get-reference useful-parent info))))
                      (if lbl
                          (format "<a id=\"%s\"></a>\n" lbl)
                        "")))
             (attr (org-export-read-attribute :attr_html useful-parent))
             ;; Hugo `figure' shortcode named parameters
             ;; https://gohugo.io/content-management/shortcodes/#figure
             (caption (org-string-nw-p
                       (org-export-data  ;Look for caption set using #+CAPTION
                        (org-export-get-caption (org-export-get-parent-element link))
                        info)))
             (figure-params `((src . ,(if (member type '("http" "https" "ftp"))
                                          (concat type ":" path)
                                        path))
                              (link . ,(plist-get attr :link))
                              (title . ,(plist-get attr :title))
                              (caption . ,(if caption
                                              caption ;Caption set using #+CAPTION takes higher precedence
                                            (plist-get attr :caption)))
                              (class . ,(plist-get attr :class))
                              (attr . ,(plist-get attr :attr))
                              (attrlink . ,(plist-get attr :attrlink))
                              (alt . ,(plist-get attr :alt))
                              (width . ,(plist-get attr :width))
                              (height . ,(plist-get attr :height))))
             (figure-param-str ""))
        ;; (message "[org-hugo-link DBG] parent-type: %s" parent-type)
        (dolist (param figure-params)
          (let ((name (car param))
                (val (cdr param)))
            (when val
              (setq figure-param-str (concat figure-param-str
                                             (format "%s=\"%s\" "
                                                     name val))))))
        ;; (message "[org-hugo-link DBG] figure params: %s" figure-param-str)
        (format "%s{{<figure %s>}}" label (org-trim figure-param-str))))
     ((string= type "coderef")
      (let ((ref (org-element-property :path link)))
        (format (org-export-get-coderef-format ref contents)
                (org-export-resolve-coderef ref info))))
     ((equal type "radio")
      contents)
     (t
      (let* ((link-param-str "")
             (path (cond
                    ((member type '("http" "https" "ftp"))
                     ;; Taken from ox-html.el -- Extract attributes
                     ;; from parent's paragraph.  HACK: Only do this
                     ;; for the first link in parent (inner image link
                     ;; for inline images).  This is needed as long as
                     ;; attributes cannot be set on a per link basis.
                     (let* ((attr
                             (let ((parent (org-export-get-parent-element link)))
                               (and (eq (org-element-map parent 'link #'identity info :first-match) link)
                                    (org-export-read-attribute :attr_html parent))))
                            ;; https://www.w3schools.com/tags/tag_link.asp
                            (link-params `((media . ,(plist-get attr :media))
                                           (target . ,(plist-get attr :target))
                                           (rel . ,(plist-get attr :rel))
                                           (sizes . ,(plist-get attr :sizes))
                                           (type . ,(plist-get attr :type)))))
                       (dolist (param link-params)
                         (let ((name (car param))
                               (val (cdr param)))
                           (when val
                             (setq link-param-str (concat link-param-str
                                                          (format "%s=\"%s\" "
                                                                  name val))))))
                       ;; (message "[ox-hugo-link DBG] link params: %s" link-param-str)
                       )
                     (concat type ":" raw-path))
                    (;; Do not add the "file://" prefix if the raw-path
                     ;; is in the Hugo "static" dir.
                     (and (string= type "file")
                          (let ((static-dir (file-truename
                                             (concat
                                              (file-name-as-directory (plist-get info :hugo-base-dir))
                                              "static/")))
                                (raw-path-true (file-truename raw-path)))
                            (string-match-p (regexp-quote static-dir) raw-path-true)))
                     (let* ((path1 (org-export-file-uri (funcall link-org-files-as-md raw-path)))
                            (path1 (replace-regexp-in-string "\\`file://" "" path1)))
                       (org-hugo--attachment-rewrite-maybe path1 info)))
                    (t
                     raw-path)))
             (link-param-str (org-string-nw-p (org-trim link-param-str))))
        (if contents
            (progn
              ;; (message "[ox-hugo DBG org-hugo-link: contents=%s path=%s" contents path)
              (if link-param-str
                  (format "<a href=\"%s\" %s>%s</a>"
                          (org-html-encode-plain-text path)
                          link-param-str
                          (org-link-unescape contents))
                (format "[%s](%s)" contents path)))
          (if link-param-str
              (let ((path (org-html-encode-plain-text path)))
                (format "<a href=\"%s\" %s>%s</a>"
                        path
                        link-param-str
                        (org-link-unescape path)))
            (format "<%s>" path))))))))

;;;;; Helpers
(defun org-hugo--attachment-rewrite-maybe (path info)
  "Copy local images and pdfs to the \"static/\" directory if needed.
Also rewrite image links.

PATH is the path to the image or pdf attachment.  If the PATH
already exists in the Hugo \"static\" directory, just return the
PATH.

INFO is a plist used as a communication channel."
  ;; (message "[ox-hugo attachment DBG] The Hugo section is: %s" (plist-get info :hugo-section))
  ;; (message "[ox-hugo attachment DBG] The Hugo base dir is: %s" (plist-get info :hugo-base-dir))
  (let* ((path-true (file-truename path))
         (exportables org-hugo-external-file-extensions-allowed-for-copying)
         (static-dir (file-truename
                      (concat
                       (file-name-as-directory (plist-get info :hugo-base-dir))
                       "static/"))))
    ;; (message "[ox-hugo DBG attch rewrite] Image export dir is: %s" static-dir)
    ;; (message "[ox-hugo DBG attch rewrite] path: %s" path)
    ;; (message "[ox-hugo DBG attch rewrite] path-true: %s" path-true)
    (if (and (file-exists-p path-true)
             (member (file-name-extension path) exportables)
             (file-directory-p static-dir))
        (progn
          ;; Check if `path-true' is already inside `static-dir'
          (if (string-match (regexp-quote static-dir) path-true)
              (progn
                ;; If so, return *only* the path considering the
                ;; static directory as root.
                (concat "/" (substring path-true (match-end 0))))
            (let* ((file-name-sans-static (if (string-match "/static/" path-true)
                                              (substring path-true (match-end 0))
                                            (concat
                                             (file-name-as-directory org-hugo-default-static-subdirectory-for-externals)
                                             (file-name-nondirectory path))))
                   (static-path (concat static-dir file-name-sans-static))
                   (static-path-dir (file-name-directory static-path)))
              ;; The `static-dir' would already exist.  But if
              ;; `file-name-sans-static' is "images/image.png" or
              ;; "foo/bar.txt", it's likely that "`static-dir'/images"
              ;; or "`static-dir'/foo" might not exist.  So create
              ;; those if needed below.
              (unless (file-exists-p static-path-dir)
                (mkdir static-path-dir :parents))
              ;; (message "[ox-hugo DBG attch rewrite] file-name: %s" file-name-sans-static)
              ;; (message "[ox-hugo DBG attch rewrite] static-path: %s" static-path)
              ;; (message "[ox-hugo DBG attch rewrite] static-path-dir: %s" static-path-dir)

              ;; Do the copy only if the file to be copied is newer or
              ;; doesn't exist in the static dir.
              (when (file-newer-than-file-p path-true static-path)
                (message "[ox-hugo] Copied %S to %S" path-true static-path)
                (copy-file path-true static-path :ok-if-already-exists))
              (concat "/" file-name-sans-static))))
      path)))

;;;; Paragraph
(defun org-hugo-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element into Hugo Markdown format.
CONTENTS is the paragraph contents.  INFO is a plist used as a
communication channel."
  (unless (org-hugo--plist-get-true-p info :hugo-preserve-filling)
    (setq contents (concat (mapconcat 'identity (split-string contents) " ") "\n")))
  ;; Glue footnotes to the words before them using &nbsp; so that the
  ;; footnote reference does not end up on a new line by itself.
  (setq contents (replace-regexp-in-string
                  ;; "something FN" -> "something&nbsp;FN"
                  "[[:blank:]]+\\(\\[\\^[^]]+\\]\\)" "&nbsp;\\1"
                  (replace-regexp-in-string
                   ;; "FN ." -> "FN."
                   "\\(\\[\\^[^]]+\\]\\)[[:blank:]]*\\([.]+\\)" "\\1\\2"
                   contents)))
  ;; (message "[org-hugo-paragraph DBG] para: %s" contents)
  (org-md-paragraph paragraph contents info))

;;;; Source Blocks
(defun org-hugo-src-block (src-block _contents info)
  "Convert SRC-BLOCK element to Hugo-compatible element.

The Markdown style triple-backquoted code blocks are created
*only* if the HUGO_CODE_FENCE property is set to a non-nil value,
and if none of the Hugo \"highlight\" shortcode features are
used.

Otherwise, the code block is wrapped in Hugo \"highlight\"
shortcode (See
https://gohugo.io/content-management/syntax-highlighting).

Hugo \"highlight\" shortcode features:
  - Code blocks with line numbers (if the -n or +n switch is used)
  - Highlight certains lines in the code block (if the :hl_lines
    parameter is used)

CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((lang (org-element-property :language src-block))
         ;; See `org-element-src-block-parser' for all SRC-BLOCK properties.
         (number-lines (org-element-property :number-lines src-block)) ;Non-nil if -n or +n switch is used
         (parameters-str (org-element-property :parameters src-block))
         (parameters (org-babel-parse-header-arguments parameters-str))
         (hl-lines (cdr (assoc :hl_lines parameters)))
         (hl-lines (when hl-lines
                     (replace-regexp-in-string "," " " hl-lines))) ;"1,3-4" -> "1 3-4"
         (label (let ((lbl (and (org-element-property :name src-block)
                                (org-export-get-reference src-block info))))
                  (if lbl
                      (format "<a id=\"%s\"></a>\n" lbl)
                    "")))
         ret)
    ;; (message "ox-hugo src [dbg] number-lines: %S" number-lines)
    ;; (message "ox-hugo src [dbg] parameters: %S" parameters)
    (setq ret
          (cond
           ;; 1. If number-lines is nil AND :hugo-code-fence is non-nil
           ((and (null number-lines)
                 (null hl-lines)
                 (org-hugo--plist-get-true-p info :hugo-code-fence))
            (let ((ret1 (org-blackfriday-src-block src-block nil info)))
              (when (and org-hugo-langs-no-descr-in-code-fences
                         (member (intern lang) org-hugo-langs-no-descr-in-code-fences))
                ;; When using Pygments, with the pygmentsCodeFences
                ;; options enabled in Hugo, `org' is not recognized as a
                ;; "language", because Pygments does not have a lexer for
                ;; Org.
                ;; Issue on Pygments repo:
                ;; https://bitbucket.org/birkenfeld/pygments-main/issues/719/wishlist-support-org
                ;; So attempt to do below:
                ;;   ```org
                ;;   # org comment
                ;;   ```
                ;; will not result in a <code> tag wrapped block in HTML.
                ;;
                ;; So override the language to be an empty string in such cases.
                ;;
                ;; *Note* that this issue does NOT exist if using Chroma,
                ;; which is the default syntax highlighter after Hugo
                ;; v0.28.
                (setq ret1 (replace-regexp-in-string (concat "\\`\\(```+\\)" lang) "\\1" ret1)))
              (setq ret1 (org-hugo--escape-hugo-shortcode ret1 lang))
              ret1))
           ;; 2. If number-lines is non-nil, or
           ;; 3. If hl-lines is non-nil, or
           ;; 4. If :hugo-code-fence is nil
           (t
            (let ((code (org-export-format-code-default src-block info))
                  (linenos-str "")
                  (hllines-str "")
                  ;; Formatter string where the first arg si linenos-str and
                  ;; second is hllines-str.
                  (highlight-args-str "%s%s"))
              (when (or number-lines
                        hl-lines)
                (setq highlight-args-str " \"%s%s\""))
              (when number-lines
                (let ((linenostart-str (progn
                                         ;; Extract the start line number of the code block
                                         (string-match "\\`\\s-*\\([0-9]+\\)\\s-\\{2\\}" code)
                                         (match-string-no-properties 1 code))))
                  (setq linenos-str (format "linenos=table, linenostart=%s" linenostart-str)))
                ;; Remove Org-inserted numbers from the beginning of each
                ;; line as the Hugo highlight shortcode will be used instead
                ;; of literally inserting the line numbers.
                (setq code (replace-regexp-in-string "^\\s-*[0-9]+\\s-\\{2\\}" "" code)))
              (when hl-lines
                (setq hllines-str (concat "hl_lines=" hl-lines))
                (when number-lines
                  (setq hllines-str (concat ", " hllines-str))))
              (setq code (org-hugo--escape-hugo-shortcode code lang))
              (format "{{< highlight %s%s>}}\n%s{{< /highlight >}}\n"
                      lang
                      (format highlight-args-str linenos-str hllines-str)
                      code)))))
    (concat label ret)))



;;; Filter Functions

;;;; Body Filter
(defun org-hugo-body-filter (body _backend info)
  "Add front matter to the BODY of the document.

BODY is the result of the export.
INFO is a plist holding export options."
  ;; `org-md-plain-text' would have escaped all underscores in plain
  ;; text i.e. "_" would have been converted to "\_".
  ;; We need to undo that underscore escaping in Emoji codes for those
  ;; to work.
  ;; Example: Convert ":raised\_hands:" back to ":raised_hands:".
  ;; More Emoji codes: https://www.emoji.codes/
  ;; (Requires setting "enableEmoji = true" in config.toml.)
  ;; (message "[ox-hugo body filter] ITEM %S" (org-entry-get (point) "ITEM"))
  ;; (message "[ox-hugo body filter] TAGS: %S" (org-entry-get (point) "TAGS"))
  ;; (message "[ox-hugo body filter] ALLTAGS: %S" (org-entry-get (point) "ALLTAGS"))
  (setq body (replace-regexp-in-string
              "\\(:[a-z0-9]+\\)[\\]\\(_[a-z0-9]+:\\)"
              "\\1\\2"
              body))
  (when (and (org-hugo--plist-get-true-p info :hugo-delete-trailing-ws)
             (not (org-hugo--plist-get-true-p info :preserve-breaks)))
    (setq body (with-temp-buffer
                 (insert body)
                 (delete-trailing-whitespace (point-min) nil)
                 (buffer-substring-no-properties (point-min) (point-max)))))
  (let ((fm (save-excursion
              (save-restriction
                ;; The point is at the beginning of the heading body
                ;; in this function! So move the point back by 1 char
                ;; to bring it into the Org headline before calling
                ;; `org-hugo--get-front-matter', because in there we
                ;; use `org-entry-get' at (point) to retrieve certain
                ;; property values.
                (widen)
                (ignore-errors ;If the point is at beginning of buffer even after widening
                  (backward-char))
                ;; (message "[body filter DBG] line at pt: %s" (thing-at-point 'line))
                (org-hugo--get-front-matter info))))
        (body (if (org-string-nw-p body) ;Insert extra newline if body is non-empty
                  (format "\n%s" body)
                "")))
    (format "%s%s%s" fm body org-hugo-footer)))

;;;;; Hugo Front Matter
(defun org-hugo--quote-string (val &optional prefer-no-quotes)
  "Wrap VAL with double quotes if it is a string.

VAL is returned as-it-is under the following cases:
- It is not a string (or nil).
- It is a string and is already wrapped with double quotes.
- It is a string and it's value is \"true\" or \"false\".
- It is a string representing a date.

If PREFER-NO-QUOTES is non-nil, return the VAL as-it-is if it's a
string with just alphanumeric characters."
  (cond
   ((or (null val)                ;nil
        (not (stringp val))       ;could be a number, like menu weight
        (and (stringp val)
             (> (safe-length val) 0)
             (string= (substring val 0 1) "\"") ;First char is literally a "
             (string= (substring val -1) "\"")) ;Last char is literally a "
        (string= "true" val)
        (string= "false" val)
        ;; or if it is a date (date, publishDate, expiryDate, lastmod)
        (string-match-p org-hugo--date-time-regexp val)
        ;; or if it is any number (integer or float)
        ;; https://github.com/toml-lang/toml#integer
        ;; Integer examples: 7, +7, -7, 7_000
        ;; https://github.com/toml-lang/toml#float
        ;; Float examples: 7.8, +7.8, -7.8, 7e-8, -7E+8
        (string-match-p "\\`[+-]?[[:digit:]_]+\\(\\(\\.\\|[eE][+-]?\\)[[:digit:]]+\\)*\\'" val))
    val)
   ((and prefer-no-quotes
         (string-match-p "\\`[a-zA-Z0-9]+\\'" val))
    val)
   (t
    (concat "\"" (replace-regexp-in-string "\"" "\\\\\""  val) "\""))))

(defun org-hugo--parse-property-arguments (str)
  "Return an alist converted from a string STR of Hugo property value.

If STR is of type \":KEY VALUE\", the returned value is ((KEY
. VALUE)).

Example: Input STR \":foo bar :baz 1 :zoo \\\"two words\\\"\" would
convert to ((foo . \"bar\") (baz . 1) (zoo . \"two words\"))."
  (let ((alist (org-babel-parse-header-arguments str)))
    (dolist (pair alist)
      ;; :KEY -> KEY
      (let ((key (intern (replace-regexp-in-string "\\`:" "" (symbol-name (car pair))))))
        (setcar pair key)))
    alist))

(defun org-hugo--front-matter-value-booleanize (str)
  "Return a \"true\" or \"false\" string for input STR."
  (let ((str-lower (and (stringp str)
                        (downcase str))))
    (cond
     ((or (null str)
          (string= "nil" str-lower)
          (string= "false" str-lower)
          (string= "no" str-lower))
      "false")
     ((or (string= "t" str)
          (string= "true" str)
          (string= "yes" str))
      "true")
     (t
      (user-error "%S needs to represent a boolean value" str)))))

(defun org-hugo--parse-blackfriday-prop-to-alist (str)
  "Return an alist of valid Hugo blackfriday properties converted from STR.

For example, input STR:

  \":fractions :smartdashes nil :angledquotes t\"

would convert to:

  ((fractions . \"false\") (smartDashes . \"false\") (angledQuotes . \"true\"))

The \"true\" and \"false\" strings in the return value are due to
`org-hugo--front-matter-value-booleanize'."
  (let ((blackfriday-alist (org-hugo--parse-property-arguments str))
        valid-blackfriday-alist)
    (dolist (ref-prop org-hugo-blackfriday-options)
      (dolist (user-prop blackfriday-alist)
        (when (string= (downcase (symbol-name (car user-prop)))
                       (downcase ref-prop))
          (let* ((key (intern ref-prop))
                 (value (cdr user-prop))
                 (value (if (or (equal key 'extensions)
                                (equal key 'extensionsmask))
                            value
                          (org-hugo--front-matter-value-booleanize value))))
            (push (cons key value)
                  valid-blackfriday-alist)))))
    valid-blackfriday-alist))

(defun org-hugo--return-valid-blackfriday-extension (ext)
  "Return valid case-sensitive string for Blackfriday extension EXT.

Example: If EXT is \"hardlinebreak\",
\"\"hardLineBreak\"\" (quoted string) is returned."
  (let (ret)
    (dolist (ref-ext org-hugo-blackfriday-extensions)
      ;; (message "ox-hugo bf valid ext DBG: ext=%s ref-ext=%s" ext ref-ext)
      (when (string= (downcase ext) (downcase ref-ext))
        (setq ret ref-ext)))
    (unless ret
      (user-error "Invalid Blackfriday extension name %S, see `org-hugo-blackfriday-extensions'"
                  ext))
    (org-hugo--quote-string ret)))

(defun org-hugo--parse-menu-prop-to-alist (str)
  "Return an alist of valid Hugo menu properties converted from STR.

Example: Input STR \":name foo :weight 80\" would convert
to ((name . \"foo\") (weight . 80))."
  (let ((menu-alist (org-hugo--parse-property-arguments str))
        valid-menu-alist)
    ;; Hugo menu properties: https://gohugo.io/content-management/menus/
    (dolist (prop '(menu name url identifier pre post weight parent)) ;children prop is probably read-only
      (let ((cell (assoc prop menu-alist)))
        (when cell
          (push cell valid-menu-alist))))
    valid-menu-alist))

(defun org-hugo--sanitize-title (info)
  "Return sanitized version of the title string parsed from INFO.

- Remove bold, italics, monospace Markdown markup characters.
- Do not escape underscore characters in the title.

INFO is a plist used as a communication channel."
  (let* ((title (org-export-data (plist-get info :title) info))
         ;; Sanitize title.. cannot do bold, italics, monospace in title
         (title (replace-regexp-in-string "\\\\?`" "" title))
         (title (replace-regexp-in-string "\\`__?\\|\\`\\*\\*?\\|__?\\'\\|\\*\\*?\\'" "" title))
         (title (replace-regexp-in-string " __?\\|__? \\| \\*\\*?\\|\\*\\*? " " " title))
         ;; Do not escape underscores in title
         (title (replace-regexp-in-string "\\\\_" "_" title)))
    title))

(defun org-hugo--transform-org-tags (tag-list info &optional no-prefer-hyphen)
  "Transform Org TAG-LIST for use in Hugo tags and categories.

INFO is a plist used as a communication channel.

1. Prefer hyphens

If NO-PREFER-HYPHEN is nil, and if using hyphens in tags is
preferred to underscores (set via
`org-hugo-prefer-hyphen-in-tags' or HUGO_PREFER_HYPHEN_IN_TAGS
property),

- Single underscores will be replaced with hyphens.
- Triple underscores will be replaced with single underscores.

Below shows the example of how the Org tags would translate to
the tag strings in Hugo front matter if hyphens were preferred:

Example: :some_tag:   -> \"some-tag\"
         :some___tag: -> \"some_tag\"

2. Allow spaces

If using spaces in tags is allowed (set via
`org-hugo-allow-spaces-in-tags' or HUGO_ALLOW_SPACES_IN_TAGS
property),

- Double underscores will be replaced with single spaces.

Below shows the example of how the Org tags would translate to
the tag strings in Hugo front matter if spaces were allowed:

Example: :some__tag:   -> \"some tag\"."
  (let* ((prefer-hyphen (unless no-prefer-hyphen
                          (org-hugo--plist-get-true-p info :hugo-prefer-hyphen-in-tags)))
         (allow-spaces (org-hugo--plist-get-true-p info :hugo-allow-spaces-in-tags))
         new-tag-list)
    (cond
     ((or prefer-hyphen
          allow-spaces)
      (dolist (tag tag-list)
        (when allow-spaces
          ;; It is safe to assume that no one would want
          ;; leading/trailing spaces in tags/categories.. so not
          ;; checking for "__a" or "a__" cases.
          (setq tag (replace-regexp-in-string "\\([^_]\\)__\\([^_]\\)" "\\1 \\2" tag)))  ;"a__b"  -> "a b"
        (when prefer-hyphen
          (setq tag (replace-regexp-in-string "\\`_\\([^_]\\)" "-\\1" tag))          ;"_a"    -> "-a"
          (setq tag (replace-regexp-in-string "\\`___\\([^_]\\)" "_\\1" tag))        ;"___a"  -> "_a"
          (setq tag (replace-regexp-in-string "\\([^_]\\)_\\'" "\\1-" tag))          ;"a_"    -> "a-"
          (setq tag (replace-regexp-in-string "\\([^_]\\)___\\'" "\\1_" tag))        ;"a___"  -> "a_"
          (setq tag (replace-regexp-in-string "\\([^_]\\)_\\([^_]\\)" "\\1-\\2" tag))    ;"a_b"   -> "a-b"
          (setq tag (replace-regexp-in-string "\\([^_]\\)___\\([^_]\\)" "\\1_\\2" tag))) ;"a___b" -> "a_b"
        (push tag new-tag-list))
      (nreverse new-tag-list))
     (t
      tag-list))))

(defun org-hugo--transform-org-tags-str (tag-str info &optional no-prefer-hyphen)
  "Wrapper function for `org-hugo--transform-org-tags'.

1. Convert the input TAG-STR string to a list,
2. Pass that to `org-hugo--transform-org-tags', and
3. Convert the returned list back to a string, with elements
   separated by `org-hugo--internal-list-separator'.
4. Return that string.

Example: \"two__words hyphenated_word\" -> \"two words\nhyphenated-word\".

INFO is a plist used as a communication channel.
NO-PREFER-HYPHEN when non-nil will prevent interpretation of
underscores in TAG-STR as hyphens.

Return nil if TAG-STR is not a string."
  (when (stringp tag-str)
    (setq tag-str (org-trim tag-str))
    (setq tag-str (replace-regexp-in-string
                   " +" org-hugo--internal-list-separator
                   tag-str))
    (let* ((tag-str-list (split-string
                          tag-str
                          org-hugo--internal-list-separator))
           (tag-str-list (org-hugo--transform-org-tags
                          tag-str-list
                          info
                          no-prefer-hyphen)))
      (mapconcat #'identity tag-str-list org-hugo--internal-list-separator))))

(defun org-hugo--category-p (tag)
  "Return non-nil if TAG begins with \"@\".

Org tags that begin with \"@\" are set as the categories field in
the Hugo front-matter."
  (and (stringp tag)
       (string-match-p "\\`@" tag)))

(defun org-hugo--get-front-matter (info)
  "Return the Hugo front matter string.

INFO is a plist used as a communication channel."
  ;; (message "[hugo front matter DBG] info: %S" (pp info))
  (let* ((fm-format (plist-get info :hugo-front-matter-format))
         (title (org-entry-get (point) "ITEM")) ;Post title
         (hugo-date-fmt "%Y-%m-%dT%T%z")
         (date-raw (or
                    ;; Get the date from the "CLOSED" property;
                    ;; generated automatically when switching a
                    ;; headline to "DONE" state,
                    (org-entry-get (point) "CLOSED")
                    ;; Else get the date from the subtree property,
                    ;; `EXPORT_DATE' if available
                    (org-string-nw-p (org-export-data (plist-get info :date) info))
                    ;; Else try to get it from the #+DATE keyword in
                    ;; the Org file.
                    (org-string-nw-p (org-export-get-date info hugo-date-fmt))))
         (date-nocolon (and (stringp date-raw)
                            (if (string-match-p org-hugo--date-time-regexp date-raw)
                                ;; If the set DATE is already in
                                ;; Hugo-compatible date format, use it.
                                date-raw
                              (format-time-string
                               hugo-date-fmt
                               ;; Else try to parse the date.
                               (apply #'encode-time (org-parse-time-string date-raw))))))
         ;; Hugo expects the date stamp in this format:
         ;;   2017-07-06T14:59:45-04:00
         ;; But the "%Y-%m-%dT%T%z" format produces the date in this format:
         ;;   2017-07-06T14:59:45-0400 (Note the missing colon)
         ;; Below simply adds that colon.
         (date (and (stringp date-nocolon)
                    (replace-regexp-in-string "\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\'" "\\1:\\2"
                                              date-nocolon)))
         (aliases-raw (let ((aliases-raw-1
                             (org-string-nw-p
                              (org-export-data (plist-get info :hugo-aliases) info))))
                        (when aliases-raw-1
                          (org-split-string aliases-raw-1 " "))))
         (aliases (let (alias-list)
                    (dolist (alias aliases-raw)
                      (if (string-match-p "/" alias)
                          (setq alias-list (append alias-list `(,alias)))
                        (let* ((section (org-export-data (plist-get info :hugo-section) info))
                               ;; Suffix section with "/" if it isn't already.
                               (section (replace-regexp-in-string "[^/]\\'" "\\&/" section)))
                          (setq alias-list (append alias-list `(,(concat "/" section alias)))))))
                    (org-string-nw-p (mapconcat #'identity
                                                alias-list
                                                org-hugo--internal-list-separator))))
         (lastmod-raw (org-string-nw-p (org-export-data (plist-get info :hugo-lastmod) info)))
         (lastmod-nocolon (cond
                           ;; If the set HUGO_LASTMOD is already in
                           ;; Hugo-compatible lastmod format, use it.
                           ((and (stringp lastmod-raw)
                                 (string-match-p org-hugo--date-time-regexp lastmod-raw))
                            lastmod-raw)
                           ;; Else if it's a string, try to parse the lastmod.
                           ((stringp lastmod-raw)
                            (format-time-string
                             hugo-date-fmt
                             (apply #'encode-time (org-parse-time-string lastmod-raw))))
                           ;; Else (if nil) and user want to auto-set the lastmod field.
                           ((org-hugo--plist-get-true-p info :hugo-auto-set-lastmod)
                            (format-time-string hugo-date-fmt (org-current-time)))
                           ;; Else.. do nothing.
                           (t
                            nil)))
         (lastmod (and (stringp lastmod-nocolon)
                       (replace-regexp-in-string "\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\'" "\\1:\\2"
                                                 lastmod-nocolon)))
         (todo-keyword (org-entry-get (point) "TODO"))
         (draft (cond
                 ((and todo-keyword
                       (string= "TODO" todo-keyword))
                  "true")
                 ((and todo-keyword
                       (string= "DRAFT" todo-keyword))
                  (message "[ox-hugo] `%s' post is marked as a draft" title)
                  "true")
                 ((org-hugo--plist-get-true-p info :hugo-draft)
                  (org-hugo--front-matter-value-booleanize (org-hugo--plist-get-true-p info :hugo-draft)))
                 (t
                  "false")))
         (all-t-and-c-str (org-entry-get (point) "ALLTAGS"))
         (all-t-and-c (when (stringp all-t-and-c-str)
                        (org-split-string all-t-and-c-str ":")))
         (tags (org-string-nw-p ;Don't allow tags to be just whitespace.
                (or
                 ;; Look for tags set using #+HUGO_TAGS keyword, or
                 ;; EXPORT_HUGO_TAGS property if available.
                 (org-hugo--transform-org-tags-str
                  (plist-get info :hugo-tags) info :no-prefer-hyphen)
                 ;; Else use Org tags (the ones set in headlines
                 ;; and/or inherited) if any.
                 (let* ((tags-list (cl-remove-if #'org-hugo--category-p all-t-and-c))
                        (tags-list (org-hugo--transform-org-tags tags-list info)))
                   ;; (when tags-list
                   ;;   (message "[get fm DBG] tags: tags-list = %s" tags-list))
                   (org-string-nw-p (mapconcat #'identity
                                               tags-list
                                               org-hugo--internal-list-separator))))))
         (categories (or
                      ;; Look for categories set using
                      ;; #+HUGO_CATEGORIES keyword, or
                      ;; EXPORT_HUGO_CATEGORIES property if available.
                      (org-hugo--transform-org-tags-str
                       (plist-get info :hugo-categories) info :no-prefer-hyphen)
                      ;; Else use categories set using Org tags with
                      ;; "@" prefix (the ones set in headlines and/or
                      ;; inherited) if any.
                      (let* ((categories-list (cl-remove-if-not #'org-hugo--category-p all-t-and-c))
                             (categories-list (org-hugo--transform-org-tags categories-list info)))
                        ;; (when categories-list
                        ;;   (message "dbg: categories: categories-list = %s" categories-list))
                        (org-string-nw-p
                         (mapconcat (lambda (str)
                                      ;; Remove "@" from beg of categories.
                                      (replace-regexp-in-string "\\`@" "" str))
                                    categories-list
                                    org-hugo--internal-list-separator)))))
         (weight (let* ((wt (plist-get info :hugo-weight))
                        (auto-calc (and (stringp wt)
                                        (string= wt "auto")
                                        org-hugo--subtree-coord)))
                   (if auto-calc
                       (org-hugo--calc-weight)
                     (unless (and (stringp wt) ;Don't allow weight to be "auto" if auto-calc is nil.
                                  (string= wt "auto"))
                       wt))))
         (menu-alist (org-hugo--parse-menu-prop-to-alist (plist-get info :hugo-menu)))
         (menu-alist-override (org-hugo--parse-menu-prop-to-alist (plist-get info :hugo-menu-override)))
         ;; If menu-alist-override is non-nil, update menu-alist with values from that.
         (menu-alist (let ((updated-menu-alist menu-alist))
                       (dolist (override-prop menu-alist-override)
                         (let* ((override-key (car override-prop))
                                (override-val (cdr override-prop))
                                (matching-prop (assoc override-key updated-menu-alist)))
                           (if matching-prop
                               (setcdr matching-prop override-val)
                             (push override-prop updated-menu-alist))))
                       updated-menu-alist))
         (custom-fm-data (org-hugo--parse-property-arguments (plist-get info :hugo-custom-front-matter)))
         (blackfriday (org-hugo--parse-blackfriday-prop-to-alist (plist-get info :hugo-blackfriday)))
         (data `(;; The order of the elements below will be the order in which the front matter
                 ;; variables will be ordered.
                 (title . ,(org-hugo--sanitize-title info))
                 (description . ,(org-export-data (plist-get info :description) info))
                 (date . ,date)
                 (publishDate . ,(org-export-data (plist-get info :hugo-publishdate) info))
                 (expiryDate . ,(org-export-data (plist-get info :hugo-expirydate) info))
                 (aliases . ,aliases)
                 (isCJKLanguage . ,(org-hugo--plist-get-true-p info :hugo-iscjklanguage))
                 (keywords . ,(org-export-data (plist-get info :keywords) info))
                 (layout . ,(org-export-data (plist-get info :hugo-layout) info))
                 (lastmod . ,lastmod)
                 (linkTitle . ,(org-export-data (plist-get info :hugo-linktitle) info))
                 (markup . ,(org-export-data (plist-get info :hugo-markup) info))
                 (outputs . ,(org-export-data (plist-get info :hugo-outputs) info))
                 (slug . ,(org-export-data (plist-get info :hugo-slug) info))
                 (tags . ,tags)
                 (categories . ,categories)
                 (type . ,(org-export-data (plist-get info :hugo-type) info))
                 (url . ,(org-export-data (plist-get info :hugo-url) info))
                 (weight . ,weight)
                 (draft . ,draft)
                 (blackfriday . ,blackfriday)
                 (menu . ,menu-alist)))
         (data `,(append data custom-fm-data)))
    ;; (message "[get fm DBG] tags: %s" tags)
    ;; (message "dbg: todo-state: keyword=%S draft=%S" todo-keyword draft)
    ;; (message "dbg: hugo tags: %S" (plist-get info :hugo-tags))
    ;; (message "[get fm info DBG] %S" info)
    ;; (message "[get fm blackfriday DBG] %S" blackfriday)
    ;; (message "[get fm menu DBG] %S" menu-alist)
    ;; (message "[get fm menu override DBG] %S" menu-alist-override)
    ;; (message "[custom fm data DBG] %S" custom-fm-data)
    ;; (message "[fm data DBG] %S" data)
    (org-hugo--gen-front-matter data fm-format)))

(defun org-hugo--calc-weight ()
  "Calculate the weight for a Hugo post or menu item.

The returned weight = INDEX + 1000*LEVEL.  See
`org-hugo--get-post-subtree-coordinates' learn about INDEX and
LEVEL."
  (let* ((level (car org-hugo--subtree-coord))
         (index (cdr org-hugo--subtree-coord)))
    (+ (* 1000 level) index)))

(defun org-hugo--gen-front-matter (data format)
  "Generate the Hugo post front matter, and return that string.

DATA is an alist of the form \((KEY1 . VAL1) (KEY2 . VAL2) .. \),
where KEY is a symbol and VAL is a string.

Generate the front matter in the specified FORMAT.  Valid values
are \"toml\" and \"yaml\"."
  (let ((sep (cond ((string= format "toml") "+++\n")
                   ((string= format "yaml") "---\n")
                   (t "")))
        (sign (cond ((string= format "toml") "=")
                    ((string= format "yaml") ":")
                    (t "")))
        (front-matter "")
        (indent "  ")
        (bf-string "")
        (menu-string ""))
    ;; (message "hugo fm format: %s" format)
    (dolist (pair data)
      (let ((key (symbol-name (car pair)))
            (value (cdr pair)))
        ;; (message "[hugo fm key value DBG] %S %S" key value)
        (unless (or (null value) ;Skip writing front matter variables whose value is nil
                    (and (stringp value) ;or an empty string.
                         (string= "" value)))
          ;; In TOML/YAML, the value portion needs to be wrapped in
          ;; double quotes.
          ;; TOML example:
          ;;     title = "My Post"
          ;; YAML example:
          ;;     title : "My Post"

          ;; In TOML, the menu information in the front matter is as a
          ;; table. So it needs to be always added to the end of the
          ;; front matter. So generate the `menu-string' separately
          ;; and then append it to `front-matter' at the end.  Do the
          ;; same for blackfriday param values.
          (cond
           ((string= key "menu")
            ;; Menu name needs to be non-nil to insert menu info in front matter.
            (when (assoc 'menu value)
              (let* ((menu-alist value)
                     ;; Menu entry string might need to be quoted if
                     ;; it contains spaces, for example.
                     (menu-entry (org-hugo--quote-string (cdr (assoc 'menu menu-alist)) :prefer-no-quotes))
                     (menu-entry-str "")
                     (menu-value-str ""))
                ;; Auto-set menu identifier if not already set by user.
                (unless (assoc 'identifier menu-alist)
                  (let ((title (cdr (assoc 'title data))))
                    (push `(identifier . ,(org-hugo-slug title)) menu-alist)))

                ;; Auto-set menu weight if not already set by user.
                (unless (assoc 'weight menu-alist)
                  (when org-hugo--subtree-coord
                    (push `(weight . ,(org-hugo--calc-weight)) menu-alist)))

                ;; (message "[menu alist DBG] = %S" menu-alist)
                (when menu-entry
                  (setq menu-entry-str (cond ((string= format "toml")
                                              (format "[menu.%s]\n" menu-entry))
                                             ((string= format "yaml")
                                              (prog1
                                                  (format "menu %s\n%s%s%s\n"
                                                          sign indent menu-entry sign)
                                                (setq indent (concat indent indent)))) ;Double the indent for next use
                                             (t
                                              "")))
                  (dolist (menu-pair menu-alist)
                    (let ((menu-key (symbol-name (car menu-pair)))
                          (menu-value (cdr menu-pair)))
                      ;; (message "menu DBG: %S %S %S" menu-entry menu-key menu-value)
                      (unless (string= "menu" menu-key)
                        (when menu-value
                          ;; Cannot skip quote wrapping for values of keys inside menu.
                          ;; Attempting to do:
                          ;;   [menu.foo]
                          ;;     parent = main
                          ;;     # parent = "main" # But this works
                          ;; gives this error:
                          ;; ERROR 2017/07/21 10:56:07 failed to parse page metadata
                          ;; for "singles/post-draft.md": Near line 10 (last key parsed
                          ;; 'menu.foo.parent'): expected value but found "main" instead.
                          (setq menu-value (org-hugo--quote-string menu-value))
                          (setq menu-value-str
                                (concat menu-value-str
                                        (format "%s%s %s %s\n"
                                                indent menu-key sign menu-value)))))))
                  (setq menu-string (concat menu-entry-str menu-value-str))))))
           ((string= key "blackfriday")
            (when value
              (let ((bf-alist value)
                    (bf-entry-str "")
                    (bf-value-str ""))
                (setq bf-entry-str (cond ((string= format "toml")
                                          "[blackfriday]\n")
                                         ((string= format "yaml")
                                          (format "blackfriday %s\n" sign))
                                         (t
                                          "")))
                (dolist (bf-pair bf-alist)
                  (let* ((bf-key (symbol-name (car bf-pair)))
                         (bf-value (cdr bf-pair))
                         (bf-value (cond ((or (string= bf-key "extensions")
                                              (string= bf-key "extensionsmask"))
                                          ;; "abc def" -> "[\"abc\", \"def\"]"
                                          (concat "["
                                                  (mapconcat #'identity
                                                             (mapcar #'org-hugo--return-valid-blackfriday-extension
                                                                     (split-string bf-value)) ", ")
                                                  "]"))
                                         (t
                                          (org-hugo--quote-string bf-value)))))
                    ;; (message "blackfriday DBG: %S %S" bf-key bf-value)
                    (setq bf-value-str
                          (concat bf-value-str
                                  (format "%s%s %s %s\n" indent bf-key sign bf-value)))))
                (setq bf-string (concat bf-entry-str bf-value-str)))))
           (t
            (setq front-matter
                  (concat front-matter
                          (format "%s %s %s\n"
                                  key
                                  sign
                                  (cond ((or (member key '("aliases" "tags" "categories" "keywords"))
                                             (listp value)) ;Custom front matter which are lists
                                         (when (listp value)
                                           ;; Convert value to a string
                                           (setq value (mapconcat
                                                        (lambda (v)
                                                          (cond
                                                           ((symbolp v)
                                                            (symbol-name v))
                                                           ((numberp v)
                                                            (number-to-string v))
                                                           (t
                                                            v)))
                                                        value
                                                        org-hugo--internal-list-separator)))
                                         ;; "abc def" -> "[\"abc\", \"def\"]"
                                         (concat "["
                                                 (mapconcat #'identity
                                                            (mapcar #'org-hugo--quote-string
                                                                    (split-string
                                                                     value
                                                                     org-hugo--internal-list-separator))
                                                            ", ")
                                                 "]"))
                                        (t
                                         (org-hugo--quote-string value)))))))))))
    (concat sep front-matter bf-string menu-string sep)))

(defun org-hugo--selective-property-inheritance ()
  "Return a list of properties that should be inherited."
  (let ((prop-list '("HUGO_FRONT_MATTER_FORMAT"
                     "HUGO_PREFER_HYPHEN_IN_TAGS"
                     "HUGO_PRESERVE_FILLING"
                     "HUGO_DELETE_TRAILING_WS"
                     "HUGO_ALLOW_SPACES_IN_TAGS"
                     "HUGO_BLACKFRIDAY"
                     "HUGO_SECTION"
                     "HUGO_BASE_DIR"
                     "HUGO_CODE_FENCE"
                     "HUGO_MENU"
                     "HUGO_CUSTOM_FRONT_MATTER"
                     "HUGO_DRAFT"
                     "HUGO_ISCJKLANGUAGE"
                     "KEYWORDS"
                     "HUGO_MARKUP"
                     "HUGO_OUTPUTS"
                     "HUGO_TAGS"
                     "HUGO_CATEGORIES"
                     "HUGO_TYPE"
                     "HUGO_WEIGHT"
                     "HUGO_AUTO_SET_LASTMOD")))
    (mapcar (lambda (str)
              (concat "EXPORT_" str))
            prop-list)))

(defun org-hugo--get-valid-subtree ()
  "Return the org element for a valid Hugo post subtree.
The condition to check validity is that the EXPORT_FILE_NAME
property is defined for the subtree element.

As this function is intended to be called inside a valid Hugo
post subtree, doing so also moves the point to the beginning of
the heading of that subtree.

Return nil if a valid Hugo post subtree is not found.  The point
will be moved in this case too."
  (catch 'break
    (while :infinite
      (let* ((entry (org-element-at-point))
             (fname (org-element-property :EXPORT_FILE_NAME entry))
             level)
        (when fname
          (throw 'break entry))
        ;; Keep on jumping to the parent heading if the current
        ;; entry does not have an EXPORT_FILE_NAME property.
        (setq level (org-up-heading-safe))
        ;; If no more parent heading exists, break out of the loop
        ;; and return nil
        (unless level
          (throw 'break nil))))))

(defun org-hugo--get-post-subtree-coordinates (subtree)
  "Return the coordinates for the current valid Hugo post SUBTREE.

The Org element returned by `org-hugo--get-valid-subtree' is a
valid Hugo post subtree.

The returned value is of type (LEVEL . INDEX) where LEVEL is the
level number of the subtree and INDEX is as explained in the
below example.

If we have

  * Level 1
  ** Level A
  ** Level B
  ** Level C
  * Level 2

the INDEX will be 1 for Level 1 and Level A, 2 for Level
B and Level 2, and 3 for Level C.

So the value returned for Level C will be (2 . 3)."
  (save-excursion
    (let ((level (org-element-property :level subtree))
          (index 1)
          (current-pos (point))
          (scope (if (org-up-heading-safe)
                     'tree ;Map entries only in parent subtree scope if parent exists
                   nil))) ;Else map in the whole buffer (provided the MATCH conditions below)
      (when level
        (org-map-entries (lambda ()
                           (when (< (point) current-pos)
                             (setq index (1+ index))))
                         ;; Loop through only headings that are at the
                         ;; same level as SUBTREE, and those which have
                         ;; the EXPORT_FILE_NAME property defined.
                         (concat "+LEVEL="
                                 (number-to-string level)
                                 "+EXPORT_FILE_NAME<>\"\"")
                         scope)
        (cons level index)))))


;;; Interactive functions

;;;###autoload
(defun org-hugo-export-as-md (&optional async subtreep visible-only)
  "Export current buffer to a Hugo-compatible Markdown buffer.

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

Export is done in a buffer named \"*Org Hugo Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-hugo--before-export-function)
  (unless subtreep ;Reset the variables that are used only for subtree exports
    (setq org-hugo--subtree-count 0)
    (setq org-hugo--subtree-coord nil))
  ;; Allow certain `ox-hugo' properties to be inherited.
  (let ((org-use-property-inheritance (org-hugo--selective-property-inheritance)))
    (org-export-to-buffer 'hugo "*Org Hugo Export*"
      async subtreep visible-only nil nil (lambda () (text-mode))))
  (org-hugo--after-export-function))

;;;###autoload
(defun org-hugo-export-to-md (&optional async subtreep visible-only)
  "Export current buffer to a Hugo-compatible Markdown file.

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

Return output file's name."
  (interactive)
  (org-hugo--before-export-function)
  ;; Allow certain `ox-hugo' properties to be inherited.  It is
  ;; important to set the `org-use-property-inheritance' before
  ;; setting the `info' var so that properties like
  ;; EXPORT_HUGO_SECTION get inherited.
  (let* ((org-use-property-inheritance (org-hugo--selective-property-inheritance))
         (info (org-combine-plists
                (org-export--get-export-attributes
                 'hugo subtreep visible-only)
                (org-export--get-buffer-attributes)
                (org-export-get-environment 'hugo subtreep)))
         (base-dir (if (null (plist-get info :hugo-base-dir))
                       (user-error "It is mandatory to set the HUGO_BASE_DIR property")
                     (file-name-as-directory (plist-get info :hugo-base-dir))))
         (content-dir "content/")
         (section-dir (if (null (plist-get info :hugo-section))
                          (user-error "It is mandatory to set the HUGO_SECTION property")
                        (file-name-as-directory (plist-get info :hugo-section))))
         (pub-dir (let ((dir (concat base-dir content-dir section-dir)))
                    (make-directory dir :parents) ;Create the directory if it does not exist
                    dir))
         (outfile (org-export-output-file-name ".md" subtreep pub-dir)))
    ;; (message "[org-hugo-export-to-md DBG] section-dir = %s" section-dir)
    (unless subtreep ;Reset the variables that are used only for subtree exports
      (setq org-hugo--subtree-count 0)
      (setq org-hugo--subtree-coord nil))
    (org-export-to-file 'hugo outfile async subtreep visible-only))
  (org-hugo--after-export-function))

;;;###autoload
(defun org-hugo-publish-to-md (plist filename pub-dir)
  "Publish an Org file to Hugo-compatible Markdown file.

PLIST is the property list for the given project.  FILENAME is
the filename of the Org file to be published.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-hugo--before-export-function)
  ;; Allow certain `ox-hugo' properties to be inherited.
  (let ((org-use-property-inheritance (org-hugo--selective-property-inheritance)))
    (org-publish-org-to 'hugo filename ".md" plist pub-dir))
  (org-hugo--after-export-function))

;;;###autoload
(defun org-hugo-export-subtree-to-md (&optional all-subtrees async visible-only)
  "Publish the current subtree to a Hugo post.
The next parent subtree having the \"EXPORT_FILE_NAME\" property
is exported if the current subtree doesn't have that property.

If ALL-SUBTREES is non-nil, publish all subtrees in the current
file.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name.  If ALL-SUBTREES is non-nil, return
nil."
  (interactive "P")
  (save-window-excursion
    (save-restriction
      (widen)
      (save-excursion
        (if all-subtrees
            (progn
              (setq org-hugo--subtree-count 0)
              (org-map-entries (lambda ()
                                 (org-hugo-export-subtree-to-md nil async visible-only))
                               ;; Export only the subtrees where
                               ;; EXPORT_FILE_NAME property is not
                               ;; empty.
                               "EXPORT_FILE_NAME<>\"\"")
              (message "[ox-hugo] Exported %d subtrees" org-hugo--subtree-count)
              nil)
          ;; Publish only the current subtree
          (org-back-to-heading :invisible-ok)
          (let ((subtree (org-hugo--get-valid-subtree))
                is-commented is-excluded)

            (unless subtree
              (user-error "It is mandatory to have a subtree with EXPORT_FILE_NAME property"))

            ;; If subtree is a valid Hugo post subtree, proceed ..
            (setq is-commented (org-element-property :commentedp subtree))

            (let ((org-use-tag-inheritance t)
                  ;; `org-get-tags' returns a list of tags *only*
                  ;; at the current heading; `org-get-tags-at'
                  ;; returns inherited tags too.
                  (all-tags (org-get-tags-at)))
              (dolist (exclude-tag org-export-exclude-tags)
                (when (member exclude-tag all-tags)
                  (setq is-excluded t))))

            ;; (message "[current subtree DBG] subtree: %S" subtree)
            ;; (message "[current subtree DBG] is-commented:%S, tags:%S, is-excluded:%S"
            ;;          is-commented tags is-excluded)
            (let ((title (org-element-property :title subtree)))
              (cond
               (is-commented
                (message "[ox-hugo] `%s' was not exported as that subtree is commented" title))
               (is-excluded
                (message "[ox-hugo] `%s' was not exported as it is tagged with one of `org-export-exclude-tags'" title))
               (t
                (message "[ox-hugo] Exporting `%s' .." title)
                (when (numberp org-hugo--subtree-count)
                  (setq org-hugo--subtree-count (1+ org-hugo--subtree-count)))
                ;; Get the current subtree coordinates for
                ;; auto-calculation of menu item weight or post
                ;; weight.
                (when (or
                       ;; Check if the menu front-matter is specified.
                       (or
                        (org-entry-get nil "EXPORT_HUGO_MENU" :inherit)
                        (save-excursion
                          (goto-char (point-min))
                          (re-search-forward "^#\\+HUGO_MENU:.*:menu" nil :noerror)))
                       ;; Check if the post needs auto-calculation of weight.
                       (or
                        (let ((post-weight (org-entry-get nil "EXPORT_HUGO_WEIGHT" :inherit)))
                          (and (stringp post-weight)
                               (string= post-weight "auto")))
                        (save-excursion
                          (goto-char (point-min))
                          (re-search-forward "^#\\+HUGO_WEIGHT:[[:blank:]]*auto" nil :noerror))))
                  (setq org-hugo--subtree-coord
                        (org-hugo--get-post-subtree-coordinates subtree)))
                (org-hugo-export-to-md async :subtreep visible-only))))))))))

;;;###autoload
(defun org-hugo-export-subtree-to-md-after-save ()
  "Fn for `after-save-hook' to run `org-hugo-export-subtree-to-md'.
Executes `org-hugo-export-subtree-to-md', but only when in a
valid Hugo post subtree.

The export is also skipped if `org-hugo-allow-export-after-save'
is nil."
  (save-excursion
    (when (and
           ;; Condition 1: `org-hugo-allow-export-after-save' must be
           ;; non-nil.
           org-hugo-allow-export-after-save
           ;; Condition 2: Point must be under an Org headline
           (ignore-errors
             (org-back-to-heading :invisible-ok))
           ;; Condition 3: Point must be in a valid Hugo post subtree
           (org-hugo--get-valid-subtree))
      (org-hugo-export-subtree-to-md))))

;;;###autoload
(defun org-hugo-debug-info ()
  "Get Emacs, Org and Hugo version and ox-hugo customization info.
The information is converted to Markdown format and copied to the
kill ring.  The same information is displayed in the Messages
buffer and returned as a string in Org format."
  (interactive)
  (let* ((emacs-version (emacs-version))
         (org-version (org-version nil :full))
         (hugo-binary (executable-find "hugo"))
         (hugo-version (when hugo-binary
                         (org-trim
                          (shell-command-to-string
                           (concat hugo-binary " version")))))
         (info-org (mapconcat #'identity
                              `("* Debug information for =ox-hugo="
                                "** Emacs Version"
                                "#+BEGIN_EXAMPLE" ;Prevent underscores from being interpreted as subscript markup
                                ,(format "%s%s"
                                         emacs-version
                                         (if emacs-repository-version
                                             (format " (commit %s)" emacs-repository-version)
                                           ""))
                                "#+END_EXAMPLE"
                                "** Org Version"
                                "#+BEGIN_EXAMPLE" ;Prevent the forward slashes in paths being interpreted as Org markup
                                ,(format "%s" org-version)
                                "#+END_EXAMPLE"
                                "*** Org =load-path= shadows"
                                ,(let* ((str (list-load-path-shadows :stringp))
                                        (str-list (split-string str "\n" :omit-nulls))
                                        (org-shadow-str ""))
                                   (dolist (shadow str-list)
                                     (when (string-match-p ".*org.+hides.+org.*" shadow)
                                       (setq org-shadow-str (concat org-shadow-str shadow "\n"))))
                                   (if (org-string-nw-p org-shadow-str)
                                       (mapconcat #'identity
                                                  `("*Warning*: Possible mixed installation of Org"
                                                    "#+BEGIN_EXAMPLE" ;Prevent the forward slashes in paths being interpreted as Org markup
                                                    ,(org-trim org-shadow-str)
                                                    "#+END_EXAMPLE"
                                                    "Study the output of =M-x list-load-path-shadows=.")
                                                  "\n")
                                     "No Org mode shadows found in =load-path="))
                                "** Hugo Version"
                                ,(if hugo-binary
                                     (format "%s" hugo-version)
                                   "=hugo= binary not found in PATH")
                                "** =ox-hugo= defcustoms"
                                ,(format "|org-hugo-default-section-directory                    |%S|" org-hugo-default-section-directory)
                                ,(format "|org-hugo-use-code-for-kbd                             |%S|" org-hugo-use-code-for-kbd)
                                ,(format "|org-hugo-preserve-filling                             |%S|" org-hugo-preserve-filling)
                                ,(format "|org-hugo-delete-trailing-ws                           |%S|" org-hugo-delete-trailing-ws)
                                ,(format "|org-hugo-prefer-hyphen-in-tags                        |%S|" org-hugo-prefer-hyphen-in-tags)
                                ,(format "|org-hugo-allow-spaces-in-tags                         |%S|" org-hugo-allow-spaces-in-tags)
                                ,(format "|org-hugo-langs-no-descr-in-code-fences                |%S|" org-hugo-langs-no-descr-in-code-fences)
                                ,(format "|org-hugo-auto-set-lastmod                             |%S|" org-hugo-auto-set-lastmod)
                                ,(format "|org-hugo-export-with-toc                              |%S|" org-hugo-export-with-toc)
                                ,(format "|org-hugo-export-with-section-numbers                  |%S|" org-hugo-export-with-section-numbers)
                                ,(format "|org-hugo-front-matter-format                          |%S|" org-hugo-front-matter-format)
                                ,(format "|org-hugo-default-static-subdirectory-for-externals    |%S|" org-hugo-default-static-subdirectory-for-externals)
                                ,(format "|org-hugo-external-file-extensions-allowed-for-copying |%S|" org-hugo-external-file-extensions-allowed-for-copying)
                                ,(format "|org-hugo-front-matter-format                          |%S|" org-hugo-front-matter-format))
                              "\n"))
         (org-export-with-toc nil)
         (info-md (progn
                    (require 'ox-md)
                    (org-export-string-as info-org 'md :body-only))))
    (kill-new info-md)
    (message "%s" info-org)
    info-org))


(provide 'ox-hugo)

;;; ox-hugo.el ends here
