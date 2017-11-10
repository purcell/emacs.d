;;; ensime-var.el --- Customizaton variables

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros))

(require 's)

(defgroup ensime nil
  "Interaction with the ENhanced Scala Environment."
  :group 'tools)

(defgroup ensime-ui nil
  "Interaction with the ENhanced Scala Environment UI."
  :group 'ensime)

(defcustom ensime-default-buffer-prefix "ENSIME-"
  "The prefix of the buffer that the ENSIME server process runs in."
  :type 'string
  :group 'ensime-ui)

(defcustom ensime-truncate-lines t
  "Set `truncate-lines' in popup buffers.
  This applies to buffers that present lines as rows of data, such as
  debugger backtraces and apropos listings."
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-kill-without-query-p t
  "If non-nil, kill ENSIME processes without query when quitting Emacs."
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-save-before-compile t
  "If non-nil, save all buffers before compiling."
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-tooltip-hints t
  "If non-nil, mouse tooltips are activated."
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-tooltip-type-hints t
  "If non-nil, type-inspecting tooltips are activated."
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-eldoc-hints nil
  "If non-nil, eldoc hints are activated.
It can be set to 'all, 'error, 'implicit or 'type to limit the type of hints shown"
  :type 'symbol
  :group 'ensime-ui)

(defcustom ensime-graphical-tooltips nil
  "If non-nil, show graphical bubbles for tooltips."
  :type 'boolean
  :group 'ensime-ui)

(defgroup ensime-server nil
  "Server configuration."
  :prefix "ensime-"
  :group 'ensime)

(defcustom ensime-sbt-command
  (executable-find "sbt")
  "Location of the sbt executable for starting the server."
  :type 'string
  :group 'ensime-server)

(defcustom ensime-auto-generate-config nil
  "If true, ENSIME will try to create a config file automatically, and
update it when the project definition changes. At the moment, this only
works for sbt projects."
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-default-server-env ()
  "A `process-environment' compatible list of environment variables"
  :type '(repeat string)
  :group 'ensime-server)

(defcustom ensime-server-logback nil
  "The logback file to use when starting the server."
  :type 'string
  :group 'ensime-server)

(defun ensime--parent-dir (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defcustom ensime-default-java-flags ()
  "Flags sent to the java instance when the server is started"
  :type '(repeat string)
  :group 'ensime-server)

(defgroup ensime-mode nil
  "Settings for ensime-mode scala source buffers."
  :prefix "ensime-"
  :group 'ensime)

(defcustom ensime-mode-key-prefix [?\C-c]
  "The prefix key for ensime-mode commands."
  :group 'ensime-mode
  :type 'sexp)

(defcustom ensime-typecheck-when-idle t
  "Controls whether a modified buffer should be typechecked automatically.
A typecheck is started when emacs is idle, if the buffer was modified
since the last typecheck."
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-typecheck-interval 2
  "Minimum time to wait between two automatic typechecks."
  :type 'number
  :group 'ensime-ui)

(defcustom ensime-typecheck-idle-interval 0.5
  "Idle time to wait before starting an automatic typecheck."
  :type 'number
  :group 'ensime-ui)

(defcustom ensime-sem-high-enabled-p t
  "If true, ensime semantic highlighting is applied whenever the buffer
is saved."
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-sem-high-faces
  '((var . scala-font-lock:var-face)
    (val . (:inherit font-lock-constant-face :slant italic))
    (varField . scala-font-lock:var-face)
    (valField . (:inherit font-lock-constant-face :slant italic))
    (functionCall . font-lock-function-name-face)
    (operator . font-lock-keyword-face)
    (param . (:slant italic))
    (class . font-lock-type-face)
    (trait .  (:inherit font-lock-type-face :slant italic))
    (object . font-lock-constant-face)
    (package . font-lock-preprocessor-face)
    (implicitConversion . ensime-implicit-highlight)
    (implicitParams . ensime-implicit-highlight)
    (deprecated . (:strike-through "dark gray")))
  
  "Faces for semantic highlighting. Symbol types not mentioned here
will not be requested from server.  The format is an alist of the form
  ( SYMBOL-TYPE . FACE-SPEC )
where SYMBOL-TYPE is one of:
  var val varField valField functionCall operator params class trait
  object package implicitConversion implicitParams deprecated"
  :type 'alist
  :group 'ensime-ui)

(defcustom ensime-completion-style 'company
  "Should be one of 'company, 'auto-complete or nil.
`company' is the most stable choice and if `auto-complete' is
used it must be installed separately."
  :type 'symbol
  :group 'ensime-ui)

(defcustom ensime-implicit-gutter-icons t
  "If non-nil, Ensime will provide gutter icons for implicit conversions
and parameters."
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-left-margin-gutter t
  "If non-nil, Ensime will show the compilation and warning icons
in the left margin, when in terminal mode. These icons can
interfere with other modes that use the left-margin. (git-gutter,
linum, etc..)"
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-refactor-preview t
  "Enable or disable a refactor preview feature.
Non-nil means Ensime will show a preview of the changes in the
*ENSIME-Refactoring* buffer. Diff hunks can be applied manually
using standard `diff-mode' commands.  Some changes can still be
applied automatically if they satisfy the criteria in
`ensime-refactor-preview-override-hunk',
`ensime-refactor-preview-override-file' or
`ensime-refactor-preview-override-types'.
Nil means Ensime will apply changes automatically unless overridden
by `ensime-refactor-no-preview-override-hunk',
`ensime-refactor-no-preview-ovverride-file' or
`ensime-refactor-no-preview-override-types'"
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-refactor-preview-override-file 0
  "The overriding criterion for a non-nil `ensime-refactor-preview'.
Automatically apply hunks when the number of affected files is
less of equal to the value."
  :type 'number
  :group 'ensime-ui)

(defcustom ensime-refactor-preview-override-hunk 10
  "The overriding criterion for a non-nil `ensime-refactor-preview'.
Automatically apply hunks when the number of hunks is less or
equal to the value."
  :type 'number
  :group 'ensime-ui)

(defcustom ensime-refactor-preview-override-types  '()
  "The overriding criterion for a non-nil `ensime-refactor-preview'.
Automatically apply hunks when the refactor type is in the list.
Possible types: `organizeImport', `rename', `extractLocal',
`extractMethod' or `inlineLocal'."
  :type '(repeat symbol)
  :group 'ensime-ui)

(defcustom ensime-refactor-no-preview-override-file 0
  "The overriding criterion for a nil `ensime-refactor-preview'.
Show the preview when the number of affected files is greater or
equal to the value"
  :type 'number
  :group 'ensime-ui)

(defcustom ensime-refactor-no-preview-override-hunk 0
  "The overriding criterion for a nil `ensime-refactor-preview'.
Show the preview when the number of hunks is greater or equal to
the value."
  :type 'number
  :group 'ensime-ui)

(defcustom ensime-refactor-no-preview-override-types  '()
  "The overriding criterion for a nil `ensime-refactor-preview'.
Show the preview when the refactor type is in the list.
Possible types: `organizeImport', `rename', `extractLocal',
`extractMethod' or `inlineLocal'."
  :type '(repeat symbol)
  :group 'ensime-ui)

(defcustom ensime-refactor-save-with-no-questions t
  "Save buffers affected by refactoring with no confirmation questions."
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-search-interface 'classic
  "Completion mechanism for search.
The options are `classic', `helm' and `ivy'."
  :type '(repeat symbol)
  :group 'ensime-ui)

(provide 'ensime-vars)

;; Local Variables:
;; End:
