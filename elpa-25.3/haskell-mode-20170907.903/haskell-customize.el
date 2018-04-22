;;; haskell-customize.el --- Customization settings -*- lexical-binding: t -*-

;; Copyright (c) 2014 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization variables

(defcustom haskell-process-load-or-reload-prompt nil
  "Nil means there will be no prompts on starting REPL. Defaults will be accepted."
  :type 'boolean
  :group 'haskell-interactive)

;;;###autoload
(defgroup haskell nil
  "Major mode for editing Haskell programs."
  :link '(custom-manual "(haskell-mode)")
  :group 'languages
  :prefix "haskell-")

(defvar haskell-mode-pkg-base-dir (file-name-directory load-file-name)
  "Package base directory of installed `haskell-mode'.
Used for locating additional package data files.")

(defcustom haskell-completing-read-function 'ido-completing-read
  "Default function to use for completion."
  :group 'haskell
  :type '(choice
          (function-item :tag "ido" :value ido-completing-read)
          (function-item :tag "helm" :value helm--completing-read-default)
          (function-item :tag "completing-read" :value completing-read)
          (function :tag "Custom function")))

(defcustom haskell-process-type
  'auto
  "The inferior Haskell process type to use.

When set to 'auto (the default), the directory contents and
available programs will be used to make a best guess at the
process type:

If the project directory or one of its parents contains a
\"cabal.sandbox.config\" file, then cabal-repl will be used.

If there's a \"stack.yaml\" file and the \"stack\" executable can
be located, then stack-ghci will be used.

Otherwise if there's a *.cabal file, cabal-repl will be used.

If none of the above apply, ghci will be used."
  :type '(choice (const auto)
                 (const ghci)
                 (const cabal-repl)
                 (const stack-ghci)
                 (const cabal-new-repl))
  :group 'haskell-interactive)

(defcustom haskell-process-wrapper-function
  #'identity
  "Wrap or transform haskell process commands using this function.

Can be set to a custom function which takes a list of arguments
and returns a possibly-modified list.

The following example function arranges for all haskell process
commands to be started in the current nix-shell environment:

  (lambda (argv) (append (list \"nix-shell\" \"-I\" \".\" \"--command\" )
                    (list (mapconcat 'identity argv \" \"))))

See Info Node `(emacs)Directory Variables' for a way to set this option on
a per-project basis."
  :group 'haskell-interactive
  :type '(choice
          (function-item :tag "None" :value identity)
          (function :tag "Custom function"))
  :safe 'functionp)

(defcustom haskell-ask-also-kill-buffers
  t
  "Ask whether to kill all associated buffers when a session
 process is killed."
  :type 'boolean
  :group 'haskell-interactive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

(defcustom haskell-doc-prettify-types t
  "Replace some parts of types with Unicode characters like \"∷\"
when showing type information about symbols."
  :group 'haskell-doc
  :type 'boolean
  :safe 'booleanp)

(defvar haskell-process-ended-functions (list 'haskell-process-prompt-restart)
  "Hook for when the haskell process ends.")

;;;###autoload
(defgroup haskell-interactive nil
  "Settings for REPL interaction via `haskell-interactive-mode'"
  :link '(custom-manual "(haskell-mode)haskell-interactive-mode")
  :group 'haskell)

(defcustom haskell-process-path-ghci
  "ghci"
  "The path for starting ghci.
This can either be a single string or a list of strings, where the
first elements is a string and the remaining elements are arguments,
which will be prepended to `haskell-process-args-ghci'."
  :group 'haskell-interactive
  :type '(choice string (repeat string)))

(defcustom haskell-process-path-cabal
  "cabal"
  "Path to the `cabal' executable.
This can either be a single string or a list of strings, where the
first elements is a string and the remaining elements are arguments,
which will be prepended to `haskell-process-args-cabal-repl'."
  :group 'haskell-interactive
  :type '(choice string (repeat string)))

(defcustom haskell-process-path-stack
  "stack"
  "The path for starting stack.
This can either be a single string or a list of strings, where the
first elements is a string and the remaining elements are arguments,
which will be prepended to `haskell-process-args-stack-ghci'."
  :group 'haskell-interactive
  :type '(choice string (repeat string)))

(defcustom haskell-process-args-ghci
  '("-ferror-spans")
  "Any arguments for starting ghci."
  :group 'haskell-interactive
  :type '(repeat (string :tag "Argument")))

(defcustom haskell-process-args-cabal-repl
  '("--ghc-option=-ferror-spans")
  "Additional arguments for `cabal repl' invocation.
Note: The settings in `haskell-process-path-ghci' and
`haskell-process-args-ghci' are not automatically reused as `cabal repl'
currently invokes `ghc --interactive'. Use
`--with-ghc=<path-to-executable>' if you want to use a different
interactive GHC frontend; use `--ghc-option=<ghc-argument>' to
pass additional flags to `ghc'."
  :group 'haskell-interactive
  :type '(repeat (string :tag "Argument")))

(defcustom haskell-process-args-cabal-new-repl
  '("--ghc-option=-ferror-spans")
  "Additional arguments for `cabal new-repl' invocation.
Note: The settings in `haskell-process-path-ghci' and
`haskell-process-args-ghci' are not automatically reused as
`cabal new-repl' currently invokes `ghc --interactive'. Use
`--with-ghc=<path-to-executable>' if you want to use a different
interactive GHC frontend; use `--ghc-option=<ghc-argument>' to
pass additional flags to `ghc'."
  :group 'haskell-interactive
  :type '(repeat (string :tag "Argument")))

(defcustom haskell-process-args-stack-ghci
  '("--ghci-options=-ferror-spans" "--no-build" "--no-load")
  "Additional arguments for `stack ghci' invocation."
  :group 'haskell-interactive
  :type '(repeat (string :tag "Argument")))

(defcustom haskell-process-do-cabal-format-string
  ":!cd %s && %s"
  "The way to run cabal comands. It takes two arguments -- the directory and the command.
See `haskell-process-do-cabal' for more details."
  :group 'haskell-interactive
  :type 'string)

(defcustom haskell-process-log
  nil
  "Enable debug logging to \"*haskell-process-log*\" buffer."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-show-debug-tips
  t
  "Show debugging tips when starting the process."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-notify-p
  nil
  "Notify using notifications.el (if loaded)?"
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-suggest-no-warn-orphans
  t
  "Suggest adding -fno-warn-orphans pragma to file when getting orphan warnings."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-suggest-hoogle-imports
  nil
  "Suggest to add import statements using Hoogle as a backend."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-suggest-hayoo-imports
  nil
  "Suggest to add import statements using Hayoo as a backend."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-hayoo-query-url
  "http://hayoo.fh-wedel.de/json/?query=%s"
  "Query url for json hayoo results."
  :type 'string
  :group 'haskell-interactive)

(defcustom haskell-process-suggest-haskell-docs-imports
  nil
  "Suggest to add import statements using haskell-docs as a backend."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-suggest-add-package
  t
  "Suggest to add packages to your .cabal file when Cabal says it
is a member of the hidden package, blah blah."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-suggest-language-pragmas
  t
  "Suggest adding LANGUAGE pragmas recommended by GHC."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-suggest-remove-import-lines
  nil
  "Suggest removing import lines as warned by GHC."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-suggest-overloaded-strings
  t
  "Suggest adding OverloadedStrings pragma to file when getting type mismatches with [Char]."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-check-cabal-config-on-load
  t
  "Check changes cabal config on loading Haskell files and
restart the GHCi process if changed.."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-prompt-restart-on-cabal-change
  t
  "Ask whether to restart the GHCi process when the Cabal file
has changed?"
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-auto-import-loaded-modules
  nil
  "Auto import the modules reported by GHC to have been loaded?"
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-reload-with-fbytecode
  nil
  "When using -fobject-code, auto reload with -fbyte-code (and
then restore the -fobject-code) so that all module info and
imports become available?"
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-use-presentation-mode
  nil
  "Use presentation mode to show things like type info instead of
  printing to the message area."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-process-suggest-restart
  t
  "Suggest restarting the process when it has died"
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-interactive-popup-errors
  t
  "Popup errors in a separate buffer."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-interactive-mode-collapse
  nil
  "Collapse printed results."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-interactive-types-for-show-ambiguous
  t
  "Show types when there's no Show instance or there's an
ambiguous class constraint."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-interactive-prompt "λ> "
  "The prompt to use."
  :type 'string
  :group 'haskell-interactive)

(defcustom haskell-interactive-prompt2 (replace-regexp-in-string
                                        "> $"
                                        "| "
                                        haskell-interactive-prompt)
  "The multi-line prompt to use.
The default is `haskell-interactive-prompt' with the last > replaced with |."
  :type 'string
  :group 'haskell-interactive)

(defcustom haskell-interactive-mode-eval-mode
  nil
  "Use the given mode's font-locking to render some text."
  :type '(choice function (const :tag "None" nil))
  :group 'haskell-interactive)

(defcustom haskell-interactive-mode-hide-multi-line-errors
  nil
  "Hide collapsible multi-line compile messages by default."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-interactive-mode-delete-superseded-errors
  t
  "Whether to delete compile messages superseded by recompile/reloads."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-interactive-mode-include-file-name
  t
  "Include the file name of the module being compiled when
printing compilation messages."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-interactive-mode-read-only
  t
  "Non-nil means most GHCi/haskell-interactive-mode output is read-only.
This does not include the prompt.  Configure
`haskell-interactive-prompt-read-only' to change the prompt's
read-only property."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-interactive-prompt-read-only
  haskell-interactive-mode-read-only
  "Non-nil means the prompt (and prompt2) is read-only."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-import-mapping
  '()
  "Support a mapping from module to import lines.

E.g. '((\"Data.Map\" . \"import qualified Data.Map as M
import Data.Map (Map)
\"))

This will import

import qualified Data.Map as M
import Data.Map (Map)

when Data.Map is the candidate.

"
  :type '(repeat (cons (string :tag "Module name")
                       (string :tag "Import lines")))
  :group 'haskell-interactive)

(defcustom haskell-language-extensions
  '()
  "Language extensions in use. Should be in format: -XFoo,
-XNoFoo etc. The idea is that various tools written with HSE (or
any haskell-mode code that needs to be aware of syntactical
properties; such as an indentation mode) that don't know what
extensions to use can use this variable. Examples: hlint,
hindent, structured-haskell-mode, tool-de-jour, etc.

You can set this per-project with a .dir-locals.el file, in the
same vein as `haskell-indent-spaces'."
  :group 'haskell
  :type '(repeat 'string))

(defcustom haskell-stylish-on-save nil
  "Whether to run stylish-haskell on the buffer before saving.
If this is true, `haskell-add-import' will not sort or align the
imports."
  :group 'haskell
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessor functions

(defvar inferior-haskell-root-dir nil
  "The path which is considered as project root, this is determined by the
presence of a *.cabal file or stack.yaml file or something similar.")

(defun haskell-process-type ()
  "Return `haskell-process-type', or a guess if that variable is 'auto.
This function also sets the `inferior-haskell-root-dir'"
  (let ((cabal-sandbox (locate-dominating-file default-directory
                                               "cabal.sandbox.config"))
        (stack         (locate-dominating-file default-directory
                                               "stack.yaml"))
        (cabal         (locate-dominating-file default-directory
                                               (lambda (d)
                                                 (cl-find-if
                                                  (lambda (f)
                                                    (string-match-p ".\\.cabal\\'" f))
                                                  (directory-files d))))))
    (if (eq 'auto haskell-process-type)
        (cond
         ;; User has explicitly initialized this project with cabal
         ((and cabal-sandbox
               (executable-find "cabal"))
          (setq inferior-haskell-root-dir cabal-sandbox)
          'cabal-repl)
         ((and stack
               (executable-find "stack"))
          (setq inferior-haskell-root-dir stack)
          'stack-ghci)
         ((and cabal
               (executable-find "cabal"))
          (setq inferior-haskell-root-dir cabal)
          'cabal-repl)
         ((executable-find "ghc")
          (setq inferior-haskell-root-dir default-directory)
          'ghci)
         (t
          (error "Could not find any installation of GHC.")))
      haskell-process-type)))

(provide 'haskell-customize)
