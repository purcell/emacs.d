;;; ycmd.el --- emacs bindings to the ycmd completion server -*- lexical-binding: t -*-
;;
;; Copyright (c) 2014-2017 Austin Bingham, Peter Vasil
;;
;; Authors: Austin Bingham <austin.bingham@gmail.com>
;;          Peter Vasil <mail@petervasil.net>
;; Version: 1.3-cvs
;; URL: https://github.com/abingham/emacs-ycmd
;; Package-Requires: ((emacs "24.4") (dash "2.13.0") (s "1.11.0") (deferred "0.5.1") (cl-lib "0.6.1") (let-alist "1.0.5") (request "0.3.0") (request-deferred "0.3.0") (pkg-info "0.6"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Description:
;;
;; ycmd is a modular server that provides completion for C/C++/ObjC
;; and Python, among other languages. This module provides an emacs
;; client for that server.
;;
;; ycmd is a bit peculiar in a few ways. First, communication with the
;; server uses HMAC to authenticate HTTP messages. The server is
;; started with an HMAC secret that the client uses to generate hashes
;; of the content it sends. Second, the server gets this HMAC
;; information (as well as other configuration information) from a
;; file that the server deletes after reading. So when the code in
;; this module starts a server, it has to create a file containing the
;; secret code. Since the server deletes this file, this code has to
;; create a new one for each server it starts. Hopefully by knowing
;; this, you'll be able to make more sense of some of what you see
;; below.
;;
;; For more details, see the project page at
;; https://github.com/abingham/emacs-ycmd.
;;
;; Installation:
;;
;; Copy this file to to some location in your emacs load path. Then add
;; "(require 'ycmd)" to your emacs initialization (.emacs,
;; init.el, or something).
;;
;; Example config:
;;
;;   (require 'ycmd)
;;   (ycmd-setup)
;;
;; Basic usage:
;;
;; First you'll want to configure a few things. If you've got a global
;; ycmd config file, you can specify that with `ycmd-global-config':
;;
;;   (set-variable 'ycmd-global-config "/path/to/global_conf.py")
;;
;; Then you'll want to configure your "extra-config whitelist"
;; patterns. These patterns determine which extra-conf files will get
;; loaded automatically by ycmd. So, for example, if you want to make
;; sure that ycmd will automatically load all of the extra-conf files
;; underneath your "~/projects" directory, do this:
;;
;;   (set-variable 'ycmd-extra-conf-whitelist '("~/projects/*"))
;;
;; Now, the first time you open a file for which ycmd can perform
;; completions, a ycmd server will be automatically started.
;;
;; When ycmd encounters an extra-config that's not on the white list,
;; it checks `ycmd-extra-conf-handler' to determine what to do. By
;; default this is set to `ask', in which case the user is asked
;; whether to load the file or ignore it. You can also set it to
;; `load', in which case all extra-confs are loaded (and you don't
;; really need to worry about `ycmd-extra-conf-whitelist'.) Or you can
;; set this to `ignore', in which case all extra-confs are
;; automatically ignored.
;;
;; Use `ycmd-get-completions' to get completions at some point in a
;; file. For example:
;;
;;   (ycmd-get-completions buffer position)
;;
;; You can use `ycmd-display-completions' to toy around with completion
;; interactively and see the shape of the structures in use.
;;
;;; License:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'let-alist))
(require 'dash)
(require 's)
(require 'deferred)
(require 'hmac-def)
(require 'json)
(require 'request)
(require 'request-deferred)
(require 'etags)
(require 'easymenu)
(require 'diff)
(require 'diff-mode)

(declare-function pkg-info-version-info "pkg-info" (package))

(defgroup ycmd nil
  "a ycmd emacs client"
  :link '(url-link :tag "Github" "https://github.com/abingham/emacs-ycmd")
  :group 'tools
  :group 'programming)

(defcustom ycmd-global-config nil
  "Path to global extra conf file."
  :type '(string))

(defcustom ycmd-extra-conf-whitelist nil
  "List of glob expressions which match extra configs.
Whitelisted configs are loaded without confirmation."
  :type '(repeat string))

(defcustom ycmd-extra-conf-handler 'ask
  "What to do when an un-whitelisted extra config is encountered.

Options are:

`load'
      Automatically load unknown extra confs.

`ignore'
     Ignore unknown extra confs and do not load them.

`ask'
     Ask the user for each unknown extra conf."
  :type '(choice (const :tag "Load unknown extra confs" load)
                 (const :tag "Ignore unknown extra confs" ignore)
                 (const :tag "Ask the user" ask))
  :risky t)

(defcustom ycmd-host "127.0.0.1"
  "The host on which the ycmd server is running."
  :type '(string))

(defcustom ycmd-server-command nil
  "The ycmd server program command.

The value is a list of arguments to run the ycmd server.
Example value:

\(set-variable 'ycmd-server-command (\"python\" \"/path/to/ycmd/package/\"))"
  :type '(repeat string))

(defcustom ycmd-server-args '("--log=debug"
                              "--keep_logfile"
                              "--idle_suicide_seconds=10800")
  "Extra arguments to pass to the ycmd server."
  :type '(repeat string))

(defcustom ycmd-server-port nil
  "The ycmd server port.  If nil, use random port."
  :type '(number))

(defcustom ycmd-file-parse-result-hook nil
  "Functions to run with file-parse results.

Each function will be called with with the results returned from
ycmd when it parses a file in response to
/event_notification."
  :type 'hook
  :risky t)

(defcustom ycmd-idle-change-delay 0.5
  "Number of seconds to wait after buffer modification before
re-parsing the contents."
  :type '(number)
  :safe #'numberp)

(defcustom ycmd-keepalive-period 600
  "Number of seconds between keepalive messages."
  :type '(number))

(defcustom ycmd-startup-timeout 3
  "Number of seconds to wait for the server to start."
  :type '(number))

(defcustom ycmd-delete-process-delay 3
  "Seconds to wait for the server to finish before killing the process."
  :type '(number))

(defcustom ycmd-parse-conditions '(save new-line mode-enabled)
  "When ycmd should reparse the buffer.

The variable is a list of events that may trigger parsing the
buffer for new completion:

`save'
      Set buffer-needs-parse flag after the buffer was saved.

`new-line'
      Set buffer-needs-parse flag immediately after a new
      line was inserted into the buffer.

`idle-change'
      Set buffer-needs-parse flag a short time after a
      buffer has changed.  (See `ycmd-idle-change-delay')

`mode-enabled'
      Set buffer-needs-parse flag after `ycmd-mode' has been
      enabled.

`buffer-focus'
      Set buffer-needs-parse flag when an unparsed buffer gets
      focus.

If nil, never set buffer-needs-parse flag.  For a manual reparse,
use `ycmd-parse-buffer'."
  :type '(set (const :tag "After the buffer was saved" save)
              (const :tag "After a new line was inserted" new-line)
              (const :tag "After a buffer was changed and idle" idle-change)
              (const :tag "After a `ycmd-mode' was enabled" mode-enabled)
              (const :tag "After an unparsed buffer gets focus" buffer-focus))
  :safe #'listp)

(defcustom ycmd-default-tags-file-name "tags"
  "The default tags file name."
  :type 'string)

(defcustom ycmd-force-semantic-completion nil
  "Whether to use always semantic completion."
  :type 'boolean)

(defcustom ycmd-auto-trigger-semantic-completion t
  "If non-nil, semantic completion is turned off.
Semantic completion is still available if
`ycmd-force-semantic-completion' is non-nil."
  :type 'boolean)

(defcustom ycmd-hide-url-status t
  "Whether to quash url status messages for ycmd requests."
  :type 'boolean)

(defcustom ycmd-bypass-url-proxy-services t
  "Bypass proxies for local traffic with the ycmd server.

If non-nil, bypass the variable `url-proxy-services' in
`ycmd--request' by setting it to nil and add `no_proxy' to
`process-environment' to bypass proxies when using `curl' as
`request-backend' and for the ycmd process."
  :type 'boolean)

(defcustom ycmd-tag-files nil
  "Whether to collect identifiers from tags file.

nil
    Do not collect identifiers from tag files.

`auto'
    Look up directory hierarchy for first found tags file with
    `ycmd-default-tags-file-name'.

string
    A tags file name.

list
    A list of tag file names."
  :type '(choice (const :tag "Don't use tag file." nil)
                 (const :tag "Locate tags file automatically" auto)
                 (string :tag "Tag file name")
                 (repeat :tag "List of tag files"
                         (string :tag "Tag file name")))
  :safe (lambda (obj)
          (or (symbolp obj)
              (stringp obj)
              (ycmd--string-list-p obj))))

(defcustom ycmd-file-type-map
  '((c++-mode . ("cpp"))
    (c-mode . ("c"))
    (caml-mode . ("ocaml"))
    (csharp-mode . ("cs"))
    (d-mode . ("d"))
    (erlang-mode . ("erlang"))
    (emacs-lisp-mode . ("elisp"))
    (go-mode . ("go"))
    (js-mode . ("javascript"))
    (js2-mode . ("javascript"))
    (lua-mode . ("lua"))
    (objc-mode . ("objc"))
    (perl-mode . ("perl"))
    (cperl-mode . ("perl"))
    (php-mode . ("php"))
    (python-mode . ("python"))
    (ruby-mode . ("ruby"))
    (rust-mode . ("rust"))
    (scala-mode . ("scala"))
    (tuareg-mode . ("ocaml"))
    (typescript-mode . ("typescript")))
  "Mapping from major modes to ycmd file-type strings.

Used to determine a) which major modes we support and b) how to
describe them to ycmd."
  :type '(alist :key-type symbol :value-type (repeat string)))

(defcustom ycmd-min-num-chars-for-completion 2
  "The minimum number of characters for identifier completion.

It controls the number of characters the user needs to type
before identifier-based completion suggestions are triggered.

This option is NOT used for semantic completion.

Setting this it to a high number like 99 effectively turns off
the identifier completion engine and just leaves the semantic
engine."
  :type 'integer)

(defcustom ycmd-max-num-identifier-candidates 10
  "The maximum number of identifier completion results."
  :type 'integer)

(defcustom ycmd-seed-identifiers-with-keywords nil
  "Whether to seed identifier database with keywords."
  :type 'boolean)

(defcustom ycmd-get-keywords-function 'ycmd--get-keywords-from-alist
  "Function to get keywords for current mode."
  :type 'symbol)

(defcustom ycmd-gocode-binary-path nil
  "Gocode binary path."
  :type 'string)

(defcustom ycmd-godef-binary-path nil
  "Godef binary path."
  :type 'string)

(defcustom ycmd-rust-src-path nil
  "Rust source path."
  :type 'string)

(defcustom ycmd-racerd-binary-path nil
  "Racerd binary path."
  :type 'string)

(defcustom ycmd-python-binary-path nil
  "Python binary path."
  :type 'string)

(defcustom ycmd-global-modes t
  "Modes for which `ycmd-mode' is turned on by `global-ycmd-mode'.

If t, ycmd mode is turned on for all major modes in
`ycmd-file-type-map'.  If set to all, ycmd mode is turned on
for all major-modes.  If a list, ycmd mode is turned on for all
`major-mode' symbols in that list.  If the `car' of the list is
`not', ycmd mode is turned on for all `major-mode' symbols _not_
in that list.  If nil, ycmd mode is never turned on by
`global-ycmd-mode'."
  :type '(choice (const :tag "none" nil)
                 (const :tag "member in `ycmd-file-type-map'" t)
                 (const :tag "all" all)
                 (set :menu-tag "mode specific" :tag "modes"
                      :value (not)
                      (const :tag "Except" not)
                      (repeat :inline t (symbol :tag "mode")))))

(defcustom ycmd-confirm-fixit t
  "Whether to confirm when applying fixit on line."
  :type 'boolean)

(defcustom ycmd-after-exception-hook nil
  "Function to run if server request resulted in exception.

This hook is run whenever an exception is thrown after a ycmd
server request.  Four arguments are passed to the function, a
string with the type of request that triggerd the exception, the
buffer and the point at the time of the request and the server
response structure which looks like this:

  ((exception
    (TYPE . \"RuntimeError\"))
   (traceback . \"long traceback string\")
   (message . \"Can't jump to definition.\"))

This variable is a normal hook.  See Info node `(elisp)Hooks'."
  :type 'hook
  :risky t)

(defcustom ycmd-after-teardown-hook nil
  "Functions to run after execution of `ycmd--teardown'.

This variable is a normal hook.  See Info node `(elisp)Hooks'."
  :type 'hook
  :risky t)

(defcustom ycmd-completing-read-function #'completing-read
  "Function to read from minibuffer with completion.

The function must be compatible to the built-in `completing-read'
function."
  :type '(choice (const :tag "Default" completing-read)
                 (const :tag "IDO" ido-completing-read)
                 (function :tag "Custom function"))
  :risky t
  :package-version '(ycmd . "1.2"))

(defconst ycmd--file-types-with-diagnostics
  '("c"
    "cpp"
    "objc"
    "objcpp"
    "cs"
    "typescript")
  "A list of ycmd file type strings which support semantic diagnostics.")

(defvar ycmd-keywords-alist
  '((c++-mode
     "alignas" "alignof" "and" "and_eq" "asm" "auto" "bitand" "bitor" "bool"
     "break" "case" "catch" "char" "char16_t" "char32_t" "class" "compl"
     "concept" "const" "const_cast" "constexpr" "continue" "decltype" "default"
     "define" "defined" "delete" "do" "double" "dynamic_cast" "elif" "else"
     "endif" "enum" "error" "explicit" "export" "extern" "false" "final" "float"
     "for" "friend" "goto" "if" "ifdef" "ifndef" "include" "inline" "int" "line"
     "long" "mutable" "namespace" "new" "noexcept" "not" "not_eq" "nullptr"
     "operator" "or" "or_eq" "override" "pragma" "_Pragma" "private" "protected"
     "public" "register" "reinterpret_cast" "requires" "return" "short" "signed"
     "sizeof" "static" "static_assert" "static_cast" "struct" "switch"
     "template" "this" "thread_local" "throw" "true" "try" "typedef" "typeid"
     "typename" "union" "unsigned" "using" "virtual" "void" "volatile" "wchar_t"
     "while" "xor" "xor_eq")
    (c-mode
     "auto" "_Alignas" "_Alignof" "_Atomic" "_Bool" "break" "case" "char"
     "_Complex" "const" "continue" "default" "define" "defined" "do" "double"
     "elif" "else" "endif" "enum" "error" "extern" "float" "for" "goto"
     "_Generic" "if" "ifdef" "ifndef" "_Imaginary" "include" "inline" "int"
     "line" "long" "_Noreturn" "pragma" "register" "restrict" "return" "short"
     "signed" "sizeof" "static" "struct" "switch" "_Static_assert" "typedef"
     "_Thread_local" "undef" "union" "unsigned" "void" "volatile" "while")
    (go-mode
     "break" "case" "chan" "const" "continue" "default" "defer" "else"
     "fallthrough" "for" "func" "go" "goto" "if" "import" "interface" "map"
     "package" "range" "return" "select" "struct" "switch" "type" "var")
    (lua-mode
     "and" "break" "do" "else" "elseif" "end" "false" "for" "function" "if" "in"
     "local" "nil" "not" "or" "repeat" "return" "then" "true" "until" "while")
    (python-mode
     "ArithmeticError" "AssertionError" "AttributeError" "BaseException"
     "BufferError" "BytesWarning" "DeprecationWarning" "EOFError" "Ellipsis"
     "EnvironmentError" "Exception" "False" "FloatingPointError" "FutureWarning"
     "GeneratorExit" "IOError" "ImportError" "ImportWarning" "IndentationError"
     "IndexError" "KeyError" "KeyboardInterrupt" "LookupError" "MemoryError"
     "NameError" "None" "NotImplemented" "NotImplementedError" "OSError"
     "OverflowError" "PendingDeprecationWarning" "ReferenceError" "RuntimeError"
     "RuntimeWarning" "StandardError" "StopIteration" "SyntaxError"
     "SyntaxWarning" "SystemError" "SystemExit" "TabError" "True" "TypeError"
     "UnboundLocalError" "UnicodeDecodeError" "UnicodeEncodeError"
     "UnicodeError" "UnicodeTranslateError" "UnicodeWarning" "UserWarning"
     "ValueError" "Warning" "ZeroDivisionError" "__builtins__" "__debug__"
     "__doc__" "__file__" "__future__" "__import__" "__init__" "__main__"
     "__name__" "__package__" "_dummy_thread" "_thread" "abc" "abs" "aifc" "all"
     "and" "any" "apply" "argparse" "array" "as" "assert" "ast" "asynchat"
     "asyncio" "asyncore" "atexit" "audioop" "base64" "basestring" "bdb" "bin"
     "binascii" "binhex" "bisect" "bool" "break" "buffer" "builtins" "bytearray"
     "bytes" "bz2" "calendar" "callable" "cgi" "cgitb" "chr" "chuck" "class"
     "classmethod" "cmath" "cmd" "cmp" "code" "codecs" "codeop" "coerce"
     "collections" "colorsys" "compile" "compileall" "complex" "concurrent"
     "configparser" "contextlib" "continue" "copy" "copyreg" "copyright"
     "credits" "crypt" "csv" "ctypes" "curses" "datetime" "dbm" "decimal" "def"
     "del" "delattr" "dict" "difflib" "dir" "dis" "distutils" "divmod" "doctest"
     "dummy_threading" "elif" "else" "email" "enumerate" "ensurepip" "enum"
     "errno" "eval" "except" "exec" "execfile" "exit" "faulthandler" "fcntl"
     "file" "filecmp" "fileinput" "filter" "finally" "float" "fnmatch" "for"
     "format" "formatter" "fpectl" "fractions" "from" "frozenset" "ftplib"
     "functools" "gc" "getattr" "getopt" "getpass" "gettext" "glob" "global"
     "globals" "grp" "gzip" "hasattr" "hash" "hashlib" "heapq" "help" "hex"
     "hmac" "html" "http" "id" "if" "imghdr" "imp" "impalib" "import"
     "importlib" "in" "input" "inspect" "int" "intern" "io" "ipaddress" "is"
     "isinstance" "issubclass" "iter" "itertools" "json" "keyword" "lambda"
     "len" "license" "linecache" "list" "locale" "locals" "logging" "long"
     "lzma" "macpath" "mailbox" "mailcap" "map" "marshal" "math" "max"
     "memoryview" "mimetypes" "min" "mmap" "modulefinder" "msilib" "msvcrt"
     "multiprocessing" "netrc" "next" "nis" "nntplib" "not" "numbers" "object"
     "oct" "open" "operator" "optparse" "or" "ord" "os" "ossaudiodev" "parser"
     "pass" "pathlib" "pdb" "pickle" "pickletools" "pipes" "pkgutil" "platform"
     "plistlib" "poplib" "posix" "pow" "pprint" "print" "profile" "property"
     "pty" "pwd" "py_compiler" "pyclbr" "pydoc" "queue" "quit" "quopri" "raise"
     "random" "range" "raw_input" "re" "readline" "reduce" "reload" "repr"
     "reprlib" "resource" "return" "reversed" "rlcompleter" "round" "runpy"
     "sched" "select" "selectors" "self" "set" "setattr" "shelve" "shlex"
     "shutil" "signal" "site" "slice" "smtpd" "smtplib" "sndhdr" "socket"
     "socketserver" "sorted" "spwd" "sqlite3" "ssl" "stat" "staticmethod"
     "statistics" "str" "string" "stringprep" "struct" "subprocess" "sum"
     "sunau" "super" "symbol" "symtable" "sys" "sysconfig" "syslog" "tabnanny"
     "tarfile" "telnetlib" "tempfile" "termios" "test" "textwrap" "threading"
     "time" "timeit" "tkinter" "token" "tokenize" "trace" "traceback"
     "tracemalloc" "try" "tty" "tuple" "turtle" "type" "types" "unichr"
     "unicode" "unicodedata" "unittest" "urllib" "uu" "uuid" "vars" "venv"
     "warnings" "wave" "weakref" "webbrowser" "while" "winsound" "winreg" "with"
     "wsgiref" "xdrlib" "xml" "xmlrpc" "xrange" "yield" "zip" "zipfile" "zipimport"
     "zlib")
    (rust-mode
     "Self"
     "as" "box" "break" "const" "continue" "crate" "else" "enum" "extern"
     "false" "fn" "for" "if" "impl" "in" "let" "loop" "macro" "match" "mod"
     "move" "mut" "pub" "ref" "return" "self" "static" "struct" "super"
     "trait" "true" "type" "unsafe" "use" "where" "while"))
  "Alist mapping major-modes to keywords for.

Keywords source: https://github.com/auto-complete/auto-complete/tree/master/dict
and `company-keywords'.")

(defvar ycmd--server-actual-port nil
  "The actual port being used by the ycmd server.
This is set based on the value of `ycmd-server-port' if set, or
the value from the output of the server itself.")

(defvar ycmd--hmac-secret nil
  "This is populated with the hmac secret of the current connection.
Users should never need to modify this, hence the defconst.  It is
not, however, treated as a constant by this code.  This value gets
set in ycmd-open.")

(defconst ycmd--server-process-name "ycmd-server"
  "The Emacs name of the server process.
This is used by functions like `start-process', `get-process'
and `delete-process'.")

(defvar-local ycmd--notification-timer nil
  "Timer for notifying ycmd server to do work, e.g. parsing files.")

(defvar ycmd--keepalive-timer nil
  "Timer for sending keepalive messages to the server.")

(defvar ycmd--on-focus-timer nil
  "Timer for deferring ycmd server notification to parse a buffer.")

(defvar ycmd--server-timeout-timer nil)

(defconst ycmd--server-buffer-name "*ycmd-server*"
  "Name of the ycmd server buffer.")

(defvar-local ycmd--last-status-change 'unparsed
  "The last status of the current buffer.")

(defvar-local ycmd--last-modified-tick nil
  "The BUFFER's last FileReadyToParse tick counter.")

(defvar-local ycmd--buffer-visit-flag nil)

(defvar ycmd--available-completers (make-hash-table :test 'eq))

(defvar ycmd--process-environment nil)

(defvar ycmd--mode-keywords-loaded nil
  "List of modes for which keywords have been loaded.")

(defconst ycmd-hooks-alist
  '((after-save-hook                  . ycmd--on-save)
    (after-change-functions           . ycmd--on-change)
    (window-configuration-change-hook . ycmd--on-window-configuration-change)
    (kill-buffer-hook                 . ycmd--on-close-buffer)
    (before-revert-hook               . ycmd--teardown)
    (post-command-hook                . ycmd--perform-deferred-parse))
  "Hooks which ycmd hooks in.")

(add-hook 'kill-emacs-hook 'ycmd-close)

(defvar ycmd-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" 'ycmd-parse-buffer)
    (define-key map "o" 'ycmd-open)
    (define-key map "c" 'ycmd-close)
    (define-key map "." 'ycmd-goto)
    (define-key map "gi" 'ycmd-goto-include)
    (define-key map "gd" 'ycmd-goto-definition)
    (define-key map "gD" 'ycmd-goto-declaration)
    (define-key map "gm" 'ycmd-goto-implementation)
    (define-key map "gp" 'ycmd-goto-imprecise)
    (define-key map "gr" 'ycmd-goto-references)
    (define-key map "gt" 'ycmd-goto-type)
    (define-key map "s" 'ycmd-toggle-force-semantic-completion)
    (define-key map "v" 'ycmd-show-debug-info)
    (define-key map "V" 'ycmd-version)
    (define-key map "d" 'ycmd-show-documentation)
    (define-key map "C" 'ycmd-clear-compilation-flag-cache)
    (define-key map "O" 'ycmd-restart-semantic-server)
    (define-key map "t" 'ycmd-get-type)
    (define-key map "T" 'ycmd-get-parent)
    (define-key map "f" 'ycmd-fixit)
    (define-key map "r" 'ycmd-refactor-rename)
    (define-key map "x" 'ycmd-completer)
    map)
  "Keymap for `ycmd-mode' interactive commands.")

(defcustom ycmd-keymap-prefix (kbd "C-c Y")
  "Prefix for key bindings of `ycmd-mode'.

Changing this variable outside Customize does not have any
effect.  To change the keymap prefix from Lisp, you need to
explicitly re-define the prefix key:

    (define-key ycmd-mode-map ycmd-keymap-prefix nil)
    (setq ycmd-keymap-prefix (kbd \"C-c ,\"))
    (define-key ycmd-mode-map ycmd-keymap-prefix
                ycmd-command-map)"
  :type 'string
  :risky t
  :set
  (lambda (variable key)
    (when (and (boundp variable) (boundp 'ycmd-mode-map))
      (define-key ycmd-mode-map (symbol-value variable) nil)
      (define-key ycmd-mode-map key ycmd-command-map))
    (set-default variable key)))

(defvar ycmd-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map ycmd-keymap-prefix ycmd-command-map)
    map)
  "Keymap for `ycmd-mode'.")

(easy-menu-define ycmd-mode-menu ycmd-mode-map
  "Menu used when `ycmd-mode' is active."
  '("YCMd"
    ["Start server" ycmd-open]
    ["Stop server" ycmd-close]
    "---"
    ["Parse buffer" ycmd-parse-buffer]
    "---"
    ["GoTo" ycmd-goto]
    ["GoToDefinition" ycmd-goto-definition]
    ["GoToDeclaration" ycmd-goto-declaration]
    ["GoToInclude" ycmd-goto-include]
    ["GoToImplementation" ycmd-goto-implementation]
    ["GoToReferences" ycmd-goto-references]
    ["GoToType" ycmd-goto-type]
    ["GoToImprecise" ycmd-goto-imprecise]
    "---"
    ["Show documentation" ycmd-show-documentation]
    ["Show type" ycmd-get-type]
    ["Show parent" ycmd-get-parent]
    "---"
    ["FixIt" ycmd-fixit]
    ["RefactorRename" ycmd-refactor-rename]
    "---"
    ["Load extra config" ycmd-load-conf-file]
    ["Restart semantic server" ycmd-restart-semantic-server]
    ["Clear compilation flag cache" ycmd-clear-compilation-flag-cache]
    ["Force semantic completion" ycmd-toggle-force-semantic-completion
     :style toggle :selected ycmd-force-semantic-completion]
    "---"
    ["Show debug info" ycmd-show-debug-info]
    ["Show version" ycmd-version t]
    ["Log enabled" ycmd-toggle-log-enabled
     :style toggle :selected ycmd--log-enabled]))

(defmacro ycmd--kill-timer (timer)
  "Cancel TIMER."
  `(when ,timer
     (cancel-timer ,timer)
     (setq ,timer nil)))

(defun ycmd-parsing-in-progress-p ()
  "Return t if parsing is in progress."
  (eq ycmd--last-status-change 'parsing))

(defun ycmd--report-status (status)
  "Report ycmd STATUS."
  (setq ycmd--last-status-change status)
  (force-mode-line-update))

(defun ycmd--mode-line-status-text ()
  "Get text for the mode line."
  (let ((force-semantic
         (when ycmd-force-semantic-completion "/s"))
        (text (pcase ycmd--last-status-change
                (`parsed "")
                (`parsing "*")
                (`unparsed "?")
                (`stopped "-")
                (`starting ">")
                (`errored "!"))))
    (concat " ycmd" force-semantic text)))

;;;###autoload
(define-minor-mode ycmd-mode
  "Minor mode for interaction with the ycmd completion server.

When called interactively, toggle `ycmd-mode'.  With prefix ARG,
enable `ycmd-mode' if ARG is positive, otherwise disable it.

When called from Lisp, enable `ycmd-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `ycmd-mode'.
Otherwise behave as if called interactively.

\\{ycmd-mode-map}"
  :init-value nil
  :keymap ycmd-mode-map
  :lighter (:eval (ycmd--mode-line-status-text))
  :group 'ycmd
  :require 'ycmd
  :after-hook
  (progn (unless (ycmd-running-p) (ycmd-open))
         (ycmd--conditional-parse 'mode-enabled 'deferred))
  (cond
   (ycmd-mode
    (dolist (hook ycmd-hooks-alist)
      (add-hook (car hook) (cdr hook) nil 'local)))
   (t
    (dolist (hook ycmd-hooks-alist)
      (remove-hook (car hook) (cdr hook) 'local))
    (ycmd--teardown))))

;;;###autoload
(defun ycmd-setup ()
  "Setup `ycmd-mode'.

Hook `ycmd-mode' into modes in `ycmd-file-type-map'."
  (interactive)
  (dolist (it ycmd-file-type-map)
    (add-hook (intern (format "%s-hook" (symbol-name (car it)))) 'ycmd-mode)))
(make-obsolete 'ycmd-setup 'global-ycmd-mode "1.0")

(defun ycmd-version (&optional show-version)
  "Get the `emacs-ycmd' version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (if (require 'pkg-info nil :no-error)
      (let ((version (pkg-info-version-info 'ycmd)))
        (when show-version
          (message "emacs-ycmd version: %s" version))
        version)
    (error "Cannot determine version without package pkg-info")))

(defun ycmd--maybe-enable-mode ()
  "Enable `ycmd-mode' according `ycmd-global-modes'."
  (when (pcase ycmd-global-modes
          (`t (ycmd-major-mode-to-file-types major-mode))
          (`all t)
          (`(not . ,modes) (not (memq major-mode modes)))
          (modes (memq major-mode modes)))
    (ycmd-mode)))

;;;###autoload
(define-globalized-minor-mode global-ycmd-mode ycmd-mode
  ycmd--maybe-enable-mode
  :init-value nil)

(defun ycmd-unload-function ()
  "Unload function for ycmd."
  (global-ycmd-mode -1)
  (remove-hook 'kill-emacs-hook #'ycmd-close))

(defvar-local ycmd--deferred-parse nil
  "If non-nil, a deferred file parse notification is pending.")

(defun ycmd--must-defer-parse ()
  "Determine whether parsing the file has to be deferred.

Return t if parsing is to be deferred, or nil otherwise."
  (or (not (ycmd--server-alive-p))
      (not (get-buffer-window))
      (ycmd-parsing-in-progress-p)
      revert-buffer-in-progress-p))

(defun ycmd--deferred-parse-p ()
  "Return non-nil if current buffer has a deferred parse."
  ycmd--deferred-parse)

(defun ycmd--parse-deferred ()
  "Defer parse notification for current buffer."
  (setq ycmd--deferred-parse t))

(defun ycmd--perform-deferred-parse ()
  "Perform the deferred parse."
  (when (ycmd--deferred-parse-p)
    (setq ycmd--deferred-parse nil)
    (ycmd--conditional-parse)))

(defun ycmd--conditional-parse (&optional condition force-deferred)
  "Reparse the buffer under CONDITION.

If CONDITION is non-nil, determine whether a ready to parse
notification should be sent according `ycmd-parse-conditions'.

If FORCE-DEFERRED is non-nil perform parse notification later."
  (when (and ycmd-mode
             (or (not condition)
                 (memq condition ycmd-parse-conditions)))
    (if (or force-deferred (ycmd--must-defer-parse))
        (ycmd--parse-deferred)
      (let ((buffer (current-buffer)))
        (deferred:$
          (deferred:next
            (lambda ()
              (with-current-buffer buffer
                (ycmd--on-visit-buffer))))
          (deferred:nextc it
            (lambda ()
              (with-current-buffer buffer
                (let ((tick (buffer-chars-modified-tick)))
                  (unless (equal tick ycmd--last-modified-tick)
                    (setq ycmd--last-modified-tick tick)
                    (ycmd-notify-file-ready-to-parse)))))))))))

(defun ycmd--on-save ()
  "Function to run when the buffer has been saved."
  (ycmd--conditional-parse 'save))

(defun ycmd--on-idle-change ()
  "Function to run on idle-change."
  (ycmd--kill-timer ycmd--notification-timer)
  (ycmd--conditional-parse 'idle-change))

(defun ycmd--on-change (beg end _len)
  "Function to run when a buffer change between BEG and END.
_LEN is ununsed."
  (save-match-data
    (when ycmd-mode
      (ycmd--kill-timer ycmd--notification-timer)
      (if (string-match-p "\n" (buffer-substring beg end))
          (ycmd--conditional-parse 'new-line)
        (setq ycmd--notification-timer
              (run-at-time ycmd-idle-change-delay nil
                           #'ycmd--on-idle-change))))))

(defun ycmd--on-unparsed-buffer-focus (buffer)
  "Function to run when an unparsed BUFFER gets focus."
  (ycmd--kill-timer ycmd--on-focus-timer)
  (with-current-buffer buffer
    (ycmd--conditional-parse 'buffer-focus)))

(defun ycmd--on-window-configuration-change ()
  "Function to run by `window-configuration-change-hook'."
  (if (ycmd--deferred-parse-p)
      (ycmd--perform-deferred-parse)
    (when (and ycmd-mode
               (pcase ycmd--last-status-change
                 ((or `unparsed `starting) t))
               (memq 'buffer-focus ycmd-parse-conditions))
      (ycmd--kill-timer ycmd--on-focus-timer)
      (let ((on-buffer-focus-fn
             (apply-partially 'ycmd--on-unparsed-buffer-focus
                              (current-buffer))))
        (setq ycmd--on-focus-timer
              (run-at-time 1.0 nil on-buffer-focus-fn))))))

(defmacro ycmd--with-all-ycmd-buffers (&rest body)
  "Execute BODY with each `ycmd-mode' enabled buffer."
  (declare (indent 0) (debug t))
  `(dolist (buffer (buffer-list))
     (with-current-buffer buffer
       (when ycmd-mode
         ,@body))))

(defun ycmd--exception-p (response)
  "Check whether RESPONSE is an exception."
  (and (listp response) (assq 'exception response)))

(cl-defmacro ycmd-with-handled-server-exceptions (request
                                                  &rest body
                                                  &key
                                                  dont-show-exception-msg
                                                  on-exception-form
                                                  return-form
                                                  bind-current-buffer
                                                  &allow-other-keys)
  "Run a deferred REQUEST and exectute BODY on success. Catch all
exceptions raised through server communication. If it is raised
because of a unknown .ycm_extra_conf.py file, load the file or
ignore it after asking the user. Otherwise print exception to
minibuffer if NO-EXCEPTION-MESSAGE is nil. ON-EXCEPTION-FORM is
run if an exception occurs. The value of RETURN-FORM is returned
on exception. If BIND-CURRENT-BUFFER is non-nil, bind
`current-buffer' to `request-buffer' var.

\(fn REQUEST &key NO-DISPLAY ERROR-FORM RETURN-FORM
BIND-CURRENT-BUFFER &rest BODY)"
  (declare (indent 1) (debug t))
  (while (keywordp (car body))
    (setq body (cdr (cdr body))))
  `(let ((request-buffer (and ,bind-current-buffer (current-buffer))))
     (deferred:$
       ,request
       (deferred:nextc it
         (lambda (response)
           (cl-macrolet ((with-optional-current-buffer
                          (buffer-or-name &rest body-2)
                          `(if ,buffer-or-name
                               (with-current-buffer ,buffer-or-name
                                 ,@body-2)
                             ,@body-2)))
             (with-optional-current-buffer
              request-buffer
              (if (ycmd--exception-p response)
                  (let-alist response
                    (if (string= .exception.TYPE "UnknownExtraConf")
                        (ycmd--handle-extra-conf-exception
                         .exception.extra_conf_file)
                      (unless ,dont-show-exception-msg
                        (message "%s: %s" .exception.TYPE .message))
                      ,on-exception-form
                      ,return-form))
                (if (null ',body) response ,@body)))))))))

(defun ycmd--on-visit-buffer ()
  "If `ycmd--buffer-visit-flag' is nil send BufferVisit event."
  (when (and (not ycmd--buffer-visit-flag)
             (ycmd--server-alive-p))
    (ycmd-with-handled-server-exceptions
        (ycmd--event-notification "BufferVisit")
      :bind-current-buffer t
      (setq ycmd--buffer-visit-flag t))))

(defun ycmd--on-close-buffer ()
  "Notify server that the current buffer is no longer open.
Cleanup emacs-ycmd variables."
  (when (ycmd--server-alive-p)
    (ycmd-with-handled-server-exceptions
        (ycmd--event-notification "BufferUnload")))
  (ycmd--teardown))

(defun ycmd--reset-parse-status ()
  (ycmd--report-status 'unparsed)
  (setq ycmd--last-modified-tick nil))

(defun ycmd--teardown ()
  "Teardown ycmd in current buffer."
  (ycmd--kill-timer ycmd--notification-timer)
  (ycmd--reset-parse-status)
  (setq ycmd--deferred-parse nil)
  (run-hooks 'ycmd-after-teardown-hook))

(defun ycmd--global-teardown ()
  "Teardown ycmd in all buffers."
  (ycmd--kill-timer ycmd--on-focus-timer)
  (setq ycmd--mode-keywords-loaded nil)
  (clrhash ycmd--available-completers)
  (ycmd--with-all-ycmd-buffers
    (ycmd--teardown)
    (setq ycmd--buffer-visit-flag nil)))

(defun ycmd-file-types-with-diagnostics (mode)
  "Find the ycmd file types for MODE which support semantic diagnostics.

Returns a possibly empty list of ycmd file type strings.  If this
is empty, then ycmd doesn't support semantic completion (or
diagnostics) for MODE."
  (-intersection
   ycmd--file-types-with-diagnostics
   (ycmd-major-mode-to-file-types mode)))

(defun ycmd-major-modes-with-diagnostics ()
  "Return a list with major-modes which support semantic diagnostics."
  (->>
   (--filter (member (cadr it) ycmd--file-types-with-diagnostics)
             ycmd-file-type-map)
   (--map (car it))))

(defun ycmd-deferred:sync! (d)
  "Wait for the given deferred task.
Error is raised if it is not processed within deferred chain D.
This is a slightly modified version of the original
`deferred:sync!' function, with using `accept-process-output'
wrapped with `with-current-buffer' for waiting instead of a
combination of `sit-for' and `sleep-for' and with a shorter wait
time."
  (progn
    (let ((last-value 'deferred:undefined*)
          uncaught-error)
      (deferred:try
        (deferred:nextc d
          (lambda (x) (setq last-value x)))
        :catch
        (lambda (err) (setq uncaught-error err)))
      (with-local-quit
        (while (and (eq 'deferred:undefined* last-value)
                    (not uncaught-error))
          (accept-process-output nil 0.01))
        (when uncaught-error
          (deferred:resignal uncaught-error))
        last-value))))

(defmacro ycmd-deferred:timeout (timeout-sec d)
  "Time out macro on a deferred task.
If the deferred task does not complete within TIMEOUT-SEC, this
macro cancels the deferred task D and returns nil. This is a
slightly modified version of the original `deferred:timeout'
macro, which takes the timeout var in seconds and the timeout
form returns the symbol `timeout'."
  (declare (indent 1) (debug t))
  `(deferred:earlier
     (deferred:nextc (deferred:wait (* ,timeout-sec 1000))
       (lambda () 'timeout))
     ,d))

(defun ycmd-open ()
  "Start a new ycmd server.

This kills any ycmd server already running (under ycmd.el's
control.)."
  (interactive)
  (ycmd-close)
  (ycmd--start-server)
  (ycmd--start-server-timeout-timer)
  (ycmd--start-keepalive-timer))

(defun ycmd-close (&optional status)
  "Shutdown any running ycmd server.
STATUS is a status symbol for `ycmd--report-status',
defaulting to `stopped'."
  (interactive)
  (ycmd--stop-server)
  (ycmd--global-teardown)
  (ycmd--kill-timer ycmd--keepalive-timer)
  (ycmd--kill-timer ycmd--server-timeout-timer)
  (ycmd--with-all-ycmd-buffers
    (ycmd--report-status (or status 'stopped))))

(defun ycmd--stop-server ()
  "Stop the ycmd server process.

Send a `shutdown' request to the ycmd server and wait for the
ycmd server to stop.  If the ycmd server is still running after a
timeout specified by `ycmd-delete-process-delay', then kill the
process with `delete-process'."
  (when (ycmd--server-alive-p)
    (let ((start-time (float-time)))
      (ycmd-deferred:sync!
       (ycmd-deferred:timeout 0.1
         (ycmd-with-handled-server-exceptions
             (ycmd--request (make-ycmd-request-data
                             :handler "shutdown" :content nil))
           :dont-show-exception-msg t)))
      (while (and (ycmd-running-p)
                  (> ycmd-delete-process-delay
                     (- (float-time) start-time)))
        (sit-for 0.05))))
  (ignore-errors
    (delete-process ycmd--server-process-name)))

(defun ycmd-running-p ()
  "Return t if a ycmd server is already running."
  (--when-let (get-process ycmd--server-process-name)
    (and (processp it) (process-live-p it) t)))

(defun ycmd--server-alive-p ()
  "Return t if server is running and ready for requests."
  (and (ycmd-running-p) ycmd--server-actual-port))

(defmacro ycmd--ignore-errors (&rest body)
  "Execute BODY and ignore errors and request errors."
  `(let ((request-message-level -1)
         (request-log-level -1))
     (ignore-errors
       ,@body)))

(defun ycmd--keepalive ()
  "Sends an unspecified message to the server.

This is simply for keepalive functionality."
  (ycmd--ignore-errors
   (ycmd-with-handled-server-exceptions
       (ycmd--request (make-ycmd-request-data
                       :handler "healthy" :content nil)
                      :type "GET")
     :dont-show-exception-msg t)))

(defun ycmd--server-ready-p (&optional include-subserver)
  "Send request for server ready state.

If INCLUDE-SUBSERVER is non-nil, also request ready state for
semantic subserver."
  (when (ycmd--server-alive-p)
    (let* ((file-type
            (and include-subserver
                 (car-safe (ycmd-major-mode-to-file-types
                            major-mode))))
           (params (and file-type
                        (list (cons "subserver" file-type)))))
      (ycmd--ignore-errors
       (eq (ycmd-deferred:sync!
            (ycmd-with-handled-server-exceptions
                (ycmd--request
                 (make-ycmd-request-data :handler "ready" :content nil)
                 :params params :type "GET")
              :dont-show-exception-msg t))
           t)))))

(defun ycmd--extra-conf-request (filename &optional ignore-p)
  "Send extra conf request.
FILENAME is the path to a ycm_extra_conf file. If optional
IGNORE-P is non-nil ignore the ycm_extra_conf."
  (let ((handler (if ignore-p
                     "ignore_extra_conf_file"
                   "load_extra_conf_file"))
        (content (list (cons "filepath" filename))))
    (ycmd-deferred:sync!
     (ycmd-with-handled-server-exceptions
         (ycmd--request (make-ycmd-request-data
                         :handler handler :content content))))))

(defun ycmd-load-conf-file (filename)
  "Tell the ycmd server to load the configuration file FILENAME."
  (interactive
   (list
    (read-file-name "Filename: ")))
  (let ((filename (expand-file-name filename)))
    (ycmd--extra-conf-request filename)))

(defun ycmd-display-completions ()
  "Get completions at the current point and display them in a buffer.

This is really a utility/debugging function for developers, but
it might be interesting for some users."
  (interactive)
  (ycmd-with-handled-server-exceptions (ycmd-get-completions)
    (if (not response)
        (message "No completions available")
      (pop-to-buffer "*ycmd-completions*")
      (erase-buffer)
      (insert (pp-to-string response)))))

(defun ycmd-complete (&optional _ignored)
  "Completion candidates at point."
  (-when-let* ((completions (ycmd-deferred:sync!
                             (ycmd-deferred:timeout 0.5
                               (ycmd-with-handled-server-exceptions
                                   (ycmd-get-completions)))))
               (candidates (cdr (assq 'completions completions))))
    (--map (let* ((text (cdr (assq 'insertion_text it)))
                  (anno (cdr (assq 'menu_text it))))
             (when (and anno (string-match (regexp-quote text) anno))
               (put-text-property
                0 1 'anno (substring anno (match-end 0)) text))
             text)
           candidates)))

(defun ycmd-complete-at-point ()
  "Complete symbol at point."
  (unless (nth 3 (syntax-ppss)) ;; not in string
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (beg (or (car bounds) (point)))
           (end (or (cdr bounds) (point))))
      (list beg end
            (completion-table-dynamic #'ycmd-complete)
            :annotation-function
            (lambda (arg) (get-text-property 0 'anno arg))))))

(defun ycmd-toggle-force-semantic-completion ()
  "Toggle whether to use always semantic completion.

Returns the new value of `ycmd-force-semantic-completion'."
  (interactive)
  (let ((force (not ycmd-force-semantic-completion)))
    (message "ycmd: force semantic completion %s."
             (if force "enabled" "disabled"))
    (setq ycmd-force-semantic-completion force)))

(defun ycmd--string-list-p (obj)
  "Return t if OBJ is a list of strings."
  (and (listp obj) (-all? #'stringp obj)))

(defun ycmd--locate-default-tags-file (buffer)
  "Look up directory hierarchy for first found default tags file for BUFFER."
  (-when-let* ((file (buffer-file-name buffer))
               (dir (and file
                         (locate-dominating-file
                          file ycmd-default-tags-file-name))))
    (expand-file-name ycmd-default-tags-file-name dir)))

(defun ycmd--get-tag-files (buffer)
  "Get tag files list for current BUFFER or nil."
  (--when-let (cond ((eq ycmd-tag-files 'auto)
                     (ycmd--locate-default-tags-file buffer))
                    ((or (stringp ycmd-tag-files)
                         (ycmd--string-list-p ycmd-tag-files))
                     ycmd-tag-files))
    (unless (listp it)
      (setq it (list it)))
    (mapcar 'expand-file-name it)))

(defun ycmd--get-keywords (buffer)
  "Get syntax keywords for BUFFER."
  (with-current-buffer buffer
    (let ((mode major-mode))
      (unless (memq mode ycmd--mode-keywords-loaded)
        (--when-let (and (functionp ycmd-get-keywords-function)
                         (funcall ycmd-get-keywords-function mode))
          (when (ycmd--string-list-p it)
            (add-to-list 'ycmd--mode-keywords-loaded mode)
            it))))))

(defun ycmd--get-keywords-from-alist (mode)
  "Get keywords from `ycmd-keywords-alist' for MODE."
  (let ((symbols (cdr (assq mode ycmd-keywords-alist))))
    (if (consp symbols)
        symbols
      (cdr (assq symbols ycmd-keywords-alist)))))

(defun ycmd-get-completions ()
  "Get completions in current buffer from the ycmd server.

Returns a deferred object which yields the HTTP message
content.  If completions are available, the structure looks like
this:

   ((error)
    (completion_start_column . 6)
    (completions
     ((kind . \"FUNCTION\")
      (extra_menu_info . \"long double\")
      (detailed_info . \"long double acoshl( long double )\\n\")
      (insertion_text . \"acoshl\")
      (menu_text . \"acoshl( long double )\"))
      . . .))

If ycmd can't do completion because it's busy parsing, the
structure looks like this:

  ((message . \"Still parsing file, no completions yet.\")
   (traceback . \"long traceback string\")
   (exception
    (TYPE . \"RuntimeError\")))

To see what the returned structure looks like, you can use
`ycmd-display-completions'."
  (let ((extra-data (and ycmd-force-semantic-completion
                         (list (cons "force_semantic" t)))))
    (ycmd--request (make-ycmd-request-data
                    :handler "completions"
                    :content (append (ycmd--get-basic-request-data)
                                     extra-data)))))

(defun ycmd--command-request (subcommand)
  "Send a command request for SUBCOMMAND."
  (let* ((subcommand (if (listp subcommand)
                         subcommand
                       (list subcommand)))
         (content (cons (append (list "command_arguments")
                                subcommand)
                        (ycmd--get-basic-request-data))))
    (ycmd--request (make-ycmd-request-data
                    :handler "run_completer_command"
                    :content content))))

(defun ycmd--run-completer-command (subcommand success-handler)
  "Send SUBCOMMAND to the `ycmd' server.

SUCCESS-HANDLER is called when for a successful response."
  (declare (indent 1))
  (when ycmd-mode
    (let ((cmd (or (car-safe subcommand) subcommand)))
      (if (ycmd-parsing-in-progress-p)
          (message "Can't send \"%s\" request while parsing is in progress!" cmd)
        (let ((pos (point)))
          (ycmd-with-handled-server-exceptions (ycmd--command-request subcommand)
            :bind-current-buffer t
            :on-exception-form (run-hook-with-args 'ycmd-after-exception-hook
                                                   cmd request-buffer pos response)
            (when (and response success-handler)
              (funcall success-handler response))))))))

(defun ycmd--unsupported-subcommand-p (response)
  "Return t if RESPONSE is an unsupported subcommand exception."
  (let-alist response
    (and (string= "ValueError" .exception.TYPE)
         (or (string-prefix-p "Supported commands are:\n" .message)
             (string= "This Completer has no supported subcommands."
                      .message)))))

(defun ycmd--get-defined-subcommands ()
  "Get available subcommands for current completer.
This is a blocking request."
  (let* ((data (make-ycmd-request-data
                :handler "defined_subcommands")))
    (ycmd-deferred:sync!
     (ycmd-with-handled-server-exceptions (ycmd--request data)))))

(defun ycmd--get-prompt-for-subcommand (subcommand)
  "Return promp for SUBCOMMAND that requires arguments."
  (pcase subcommand
    (`"RefactorRename"
     "New name: ")
    ((pred (and "RestartServer" (eq major-mode 'python-mode)))
     "Python binary: ")))

(defun ycmd--read-subcommand ()
  "Read subcommand from minibuffer."
  (--when-let (ycmd--get-defined-subcommands)
    (funcall ycmd-completing-read-function
             "Subcommand: " it nil t)))

(defun ycmd-completer (subcommand)
  "Run SUBCOMMAND for current completer."
  (interactive
   (list (-when-let (cmd (ycmd--read-subcommand))
           (--when-let (ycmd--get-prompt-for-subcommand cmd)
             (let ((arg (read-string it)))
               (unless (s-blank-str? arg)
                 (setq cmd (list cmd arg)))))
           cmd)))
  (when subcommand
    (ycmd--run-completer-command subcommand
      (lambda (response)
        (cond ((not (listp response))
               ;; If not a list, the response is necessarily a scalar:
               ;; boolean, number, string, etc. In this case, we print it to
               ;; the user.
               (message "%s" response))
              ((assq 'fixits response)
               (ycmd--handle-fixit-response response))
              ((assq 'message response)
               (ycmd--handle-message-response response))
              ((assq 'detailed_info response)
               (ycmd--handle-detailed-info-response response))
              (t
               (ycmd--handle-goto-response response)))))))

(defun ycmd-goto ()
  "Go to the definition or declaration of the symbol at current position."
  (interactive)
  (ycmd--goto "GoTo"))

(defun ycmd-goto-declaration ()
  "Go to the declaration of the symbol at the current position."
  (interactive)
  (ycmd--goto "GoToDeclaration"))

(defun ycmd-goto-definition ()
  "Go to the definition of the symbol at the current position."
  (interactive)
  (ycmd--goto "GoToDefinition"))

(defun ycmd-goto-implementation ()
  "Go to the implementation of the symbol at the current position."
  (interactive)
  (ycmd--goto "GoToImplementation"))

(defun ycmd-goto-include ()
  "Go to the include of the symbol at the current position."
  (interactive)
  (ycmd--goto "GoToInclude"))

(defun ycmd-goto-imprecise ()
  "Fast implementation of Go To at the cost of precision.
Useful in case compile-time is considerable."
  (interactive)
  (ycmd--goto "GoToImprecise"))

(defun ycmd-goto-references ()
  "Get references."
  (interactive)
  (ycmd--goto "GoToReferences"))

(defun ycmd-goto-type ()
  "Go to the type of the symbol at the current position."
  (interactive)
  (ycmd--goto "GoToType"))

(defun ycmd--save-marker ()
  "Save marker."
  (push-mark)
  (if (fboundp 'xref-push-marker-stack)
      (xref-push-marker-stack)
    (with-no-warnings
      (ring-insert find-tag-marker-ring (point-marker)))))

(defun ycmd--location-data-p (response)
  "Return t if RESPONSE is a GoTo location."
  (and (assq 'filepath response)
       (assq 'line_num response)
       (assq 'column_num response)))

(defun ycmd--handle-goto-response (response)
  "Handle a successfull GoTo RESPONSE."
  (ycmd--save-marker)
  (if (ycmd--location-data-p response)
      (ycmd--goto-location response 'find-file)
    (ycmd--view response major-mode)))

(defun ycmd--goto (type)
  "Implementation of GoTo according to the request TYPE."
  (save-excursion
    (--when-let (bounds-of-thing-at-point 'symbol)
      (goto-char (car it)))
    (ycmd-completer type)))

(defun ycmd--goto-location (location find-function)
  "Move cursor to LOCATION with FIND-FUNCTION.

LOCATION is a structure as returned from e.g. the various GoTo
commands."
  (let-alist location
    (--when-let .filepath
      (funcall find-function it)
      (goto-char (ycmd--col-line-to-position
                  .column_num .line_num)))))

(defun ycmd--goto-line (line)
  "Go to LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun ycmd--col-line-to-position (col line &optional buffer)
  "Convert COL and LINE into a position in the current buffer.

COL and LINE are expected to be as returned from ycmd, e.g. from
notify-file-ready.  Apparently COL can be 0 sometimes, in which
case this function returns 0.
Use BUFFER if non-nil or `current-buffer'."
  (let ((buff (or buffer (current-buffer))))
    (if (= col 0)
        0
      (with-current-buffer buff
        (ycmd--goto-line line)
        (forward-char (- col 1))
        (point)))))

(defun ycmd-clear-compilation-flag-cache ()
  "Clear the compilation flags cache."
  (interactive)
  (ycmd-completer "ClearCompilationFlagCache"))

(defun ycmd-restart-semantic-server (&optional arg)
  "Send request to restart the semantic completion backend server.
If ARG is non-nil and current `major-mode' is `python-mode',
prompt for the Python binary."
  (interactive
   (list (and current-prefix-arg
              (eq major-mode 'python-mode)
              (read-string "Python binary: "))))
  (let ((subcommand "RestartServer"))
    (unless (s-blank-str? arg)
      (setq subcommand (list subcommand arg)))
    (ycmd-completer subcommand)))

(cl-defun ycmd--fontify-code (code &optional (mode major-mode))
  "Fontify CODE."
  (cl-check-type mode function)
  (if (not (stringp code))
      code
    (with-temp-buffer
      (delay-mode-hooks (funcall mode))
      (setq font-lock-mode t)
      (funcall font-lock-function font-lock-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert code)
        (font-lock-default-fontify-region
         (point-min) (point-max) nil))
      (buffer-string))))

(defun ycmd--get-message (response)
  "Extract message from RESPONSE.
Return a cons cell with the type or parent as car. If cdr is
non-nil, the result is a valid type or parent."
  (--when-let (cdr (assq 'message response))
    (pcase it
      ((or `"Unknown semantic parent"
           `"Unknown type"
           `"Internal error: cursor not valid"
           `"Internal error: no translation unit")
       (cons it nil))
      (_ (cons it t)))))

(defun ycmd--handle-message-response (response)
  "Handle a successful GetParent or GetType RESPONSE."
  (--when-let (ycmd--get-message response)
    (pcase-let ((`(,msg . ,is-type-p) it))
      (message "%s" (if is-type-p
                        (ycmd--fontify-code msg)
                      msg)))))

(defun ycmd-get-parent ()
  "Get semantic parent for symbol at point."
  (interactive)
  (ycmd-completer "GetParent"))

(defun ycmd-get-type (&optional arg)
  "Get type for symbol at point.
If optional ARG is non-nil, get type without reparsing buffer."
  (interactive "P")
  (ycmd-completer (if arg "GetTypeImprecise" "GetType")))

;;; FixIts

(defmacro ycmd--loop-chunks-by-filename (spec &rest body)
  "Loop over an alist of fixit chunks grouped by filepath.
Evaluate BODY with `it' bound to each car from FIXIT-CHUNKS, in
turn. The structure of `it' is a cons cell (FILEPATH CHUNK-LIST).
Then evaluate RESULT to get return value, default nil.

\(fn (FIXIT-CHUNKS [RESULT]) BODY...)"
  (declare (indent 1) (debug ((form &optional form) body)))
  `(let ((chunks-by-filepath
          (--group-by (let-alist it .range.start.filepath)
                      ,(car spec))))
     (dolist (it chunks-by-filepath)
       ,@body)
     ,@(cdr spec)))

(defun ycmd--show-fixits (fixits &optional title)
  "Select a buffer and display FIXITS.
Optional TITLE is shown on first line."
  (let ((fixits-buffer (get-buffer-create "*ycmd-fixits*"))
        (fixit-num 1))
    (with-current-buffer fixits-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (when title (insert (propertize title 'face 'bold)))
      (dolist (fixit fixits)
        (let-alist fixit
          (let (button-diff)
            (let* ((diffs (ycmd--get-fixit-diffs .chunks))
                   (multiple-fixits-p (> (length diffs) 1)))
              (dolist (diff diffs)
                (pcase-let ((`(,diff-text . ,diff-path) diff))
                  (setq button-diff
                        (concat button-diff (when (or (s-blank-str? .text)
                                                      multiple-fixits-p)
                                              (format "%s\n" diff-path))
                                (ycmd--fontify-code diff-text 'diff-mode) "\n")))))
            (ycmd--insert-fixit-button
             (concat (format "%d: %s\n" fixit-num .text) button-diff)
             .chunks .location.filepath)
            (cl-incf fixit-num))))
      (goto-char (point-min))
      (when title (forward-line 1))
      (ycmd-fixit-mode))
    (pop-to-buffer fixits-buffer)
    (setq next-error-last-buffer fixits-buffer)))

(defun ycmd--get-fixit-diffs (chunks)
  "Return a list of diffs for CHUNKS.
Each diff is a list of the actual diff, the path of the file for
the diff and a flag whether to show the filepath as part of the
button text. The flag is set to t when there are multiple diff
chunks for the file."
  (let (diffs)
    (ycmd--loop-chunks-by-filename (chunks (nreverse diffs))
      (pcase-let* ((`(,filepath . ,chunk) it)
                   (buffer (find-file-noselect filepath))
                   (buffertext (with-current-buffer buffer (buffer-string))))
        (with-temp-buffer
          (insert buffertext)
          (ycmd--replace-chunk-list chunk (current-buffer))
          (let ((diff-buffer (diff-no-select filepath (current-buffer)
                                             "-U0 --strip-trailing-cr" t)))
            (with-current-buffer diff-buffer
              (goto-char (point-min))
              (unless (eobp)
                (ignore-errors
                  (diff-beginning-of-hunk t))
                (while (looking-at diff-hunk-header-re-unified)
                  (let* ((beg (point))
                         (end (diff-end-of-hunk))
                         (diff (buffer-substring-no-properties beg end)))
                    (push (cons diff filepath) diffs)))))))))))

(define-button-type 'ycmd--fixit-button
  'action #'ycmd--apply-fixit
  'face nil)

(defun ycmd--insert-fixit-button (name fixit location)
  "Insert a button with NAME and FIXIT for LOCATION."
  (insert-text-button
   name
   'type 'ycmd--fixit-button
   'fixit fixit
   'location location))

(defun ycmd--apply-fixit (button)
  "Apply BUTTON's FixIt chunk."
  (-when-let (chunks (button-get button 'fixit))
    (ycmd--loop-chunks-by-filename (chunks)
      (ycmd--replace-chunk-list (cdr it)))
    (quit-window t (get-buffer-window "*ycmd-fixits*"))))

(define-derived-mode ycmd-fixit-mode ycmd-view-mode "ycmd-fixits"
  "Major mode for viewing and navigation of fixits.

\\{ycmd-view-mode-map}"
  (local-set-key (kbd "q") (lambda () (interactive) (quit-window t))))

(defun ycmd--replace-chunk (bounds replacement-text deltas buffer)
  "Replace text between BOUNDS with REPLACEMENT-TEXT.

BOUNDS is a list of two cons cells representing the start and end
of a chunk with a line and column pair (car and cdr). DELTAS is a
cons cell with line and char offsets from former replacements on
the current line. BUFFER is the current working buffer."
  (pcase-let* ((`((,start-line . ,start-column) (,end-line . ,end-column)) bounds)
               (`(,line-delta . ,char-delta) deltas)
               (start-line (+ start-line line-delta))
               (end-line (+ end-line line-delta))
               (source-line-count (1+ (- end-line start-line)))
               (start-column (+ start-column char-delta))
               (end-column (if (= source-line-count 1)
                               (+ end-column char-delta)
                             end-column))
               (replacement-lines (s-split "\n" replacement-text))
               (replacement-lines-count (length replacement-lines))
               (new-line-delta (- replacement-lines-count source-line-count))
               (new-char-delta (- (length (car (last replacement-lines)))
                                  (- end-column start-column))))
    (when (> replacement-lines-count 1)
      (setq new-char-delta (- new-char-delta start-column)))
    (save-excursion
      (with-current-buffer buffer
        (delete-region
         (ycmd--col-line-to-position start-column start-line buffer)
         (ycmd--col-line-to-position end-column end-line buffer))
        (insert replacement-text)
        (cons new-line-delta new-char-delta)))))

(defun ycmd--get-chunk-bounds (chunk)
  "Get an list with bounds of CHUNK."
  (let-alist chunk
    (list (cons .range.start.line_num .range.start.column_num)
          (cons .range.end.line_num .range.end.column_num))))

(defun ycmd--chunk-< (c1 c2)
  "Return t if C1 should go before C2."
  (pcase-let ((`((,line-num-1 . ,column-num-1) ,_) (ycmd--get-chunk-bounds c1))
              (`((,line-num-2 . ,column-num-2) ,_) (ycmd--get-chunk-bounds c2)))
    (or (< line-num-1 line-num-2)
        (and (= line-num-1 line-num-2)
             (< column-num-1 column-num-2)))))

(defun ycmd--replace-chunk-list (chunks &optional buffer)
  "Replace list of CHUNKS.

If BUFFER is specified use it as working buffer, else use buffer
specified in fixit chunk."
  (let ((chunks-sorted (sort chunks 'ycmd--chunk-<))
        (last-line -1)
        (line-delta 0)
        (char-delta 0))
    (dolist (c chunks-sorted)
      (let-alist c
        (pcase-let* ((chunk-bounds (ycmd--get-chunk-bounds c))
                     (`((,start-line . ,_) (,end-line . ,_)) chunk-bounds)
                     (buffer (or buffer (find-file-noselect
                                         .range.start.filepath))))
          (unless (= start-line last-line)
            (setq last-line end-line)
            (setq char-delta 0))
          (pcase-let ((`(,new-line-delta . ,new-char-delta)
                       (ycmd--replace-chunk chunk-bounds .replacement_text
                                            (cons line-delta char-delta) buffer)))
            (setq line-delta (+ line-delta new-line-delta))
            (setq char-delta (+ char-delta new-char-delta))))))))

(defun ycmd--fixits-have-same-location-p (fixits)
  "Check if mutiple FIXITS have the same location."
  (let ((fixits-by-location
         (--group-by (cdr (assq 'location it)) fixits)))
    (catch 'done
      (dolist (f fixits-by-location)
        (when (> (length (cdr f)) 1)
          (throw 'done t))))))

(defun ycmd--handle-fixit-response (response)
  "Handle a fixit RESPONSE."
  (let ((fixits (cdr (assq 'fixits response))))
    (if (not fixits)
        (message "No fixits found for current line")
      (let ((multiple-fixits-p
             (and (> (length fixits) 1)
                  (ycmd--fixits-have-same-location-p fixits))))
        (if (and (not ycmd-confirm-fixit) (not multiple-fixits-p))
            (let ((num-changes-applied 0)
                  files-changed)
              (dolist (fixit fixits)
                (-when-let (chunks (cdr (assq 'chunks fixit)))
                  (ycmd--loop-chunks-by-filename (chunks)
                    (pcase-let ((`(,chunk-path . ,chunk) it))
                      (ycmd--replace-chunk-list chunk)
                      (cl-incf num-changes-applied (length chunk))
                      (unless (member chunk-path files-changed)
                        (setq files-changed (append (list chunk-path)
                                                    files-changed)))))))
              (when (> num-changes-applied 0)
                (let* ((num-files-changed (length files-changed))
                       (text
                        (concat (format "Applied %d changes" num-changes-applied)
                                (when (> num-files-changed 1)
                                  (format " in %d files" num-files-changed)))))
                  (message text))))
          (save-current-buffer
            (ycmd--show-fixits
             fixits (and multiple-fixits-p
                         (concat
                          "Multiple FixIt suggestions are available at this location."
                          "Which one would you like to apply?\n")))))))))

(defun ycmd-fixit()
  "Get FixIts for current line."
  (interactive)
  (ycmd-completer "FixIt"))

(defun ycmd-refactor-rename (new-name)
  "Refactor current context with NEW-NAME."
  (interactive "MNew variable name: ")
  (let ((subcommand "RefactorRename"))
    (unless (s-blank-str? new-name)
      (setq subcommand (list subcommand new-name)))
    (ycmd-completer subcommand)))

(defun ycmd-show-documentation (&optional arg)
  "Show documentation for current point in buffer.

If optional ARG is non-nil do not reparse buffer before getting
the documentation."
  (interactive "P")
  (ycmd-completer (if arg "GetDocImprecise" "GetDoc")))

(defun ycmd--handle-detailed-info-response (response)
  "Handle successful GetDoc RESPONSE."
  (let ((documentation (cdr (assq 'detailed_info response))))
    (if (not (s-blank? documentation))
        (with-help-window (get-buffer-create " *ycmd-documentation*")
          (with-current-buffer standard-output
            (insert documentation)))
      (message "No documentation available for current context"))))

(defmacro ycmd--with-view-buffer (&rest body)
  "Create view buffer and execute BODY in it."
  `(let ((buf (get-buffer-create "*ycmd-locations*")))
     (with-current-buffer buf
       (setq buffer-read-only nil)
       (erase-buffer)
       ,@body
       (goto-char (point-min))
       (ycmd-view-mode)
       buf)))

(defun ycmd--view (response mode)
  "Select `ycmd-view-mode' buffer and display items from RESPONSE.
MODE is a major mode for fontifaction."
  (let ((view-buffer
         (ycmd--with-view-buffer
          (->>
           (--group-by (cdr (assq 'filepath it)) response)
           (mapc (lambda (it) (ycmd--view-insert-location it mode)))))))
    (pop-to-buffer view-buffer)
    (setq next-error-last-buffer view-buffer)))

(define-button-type 'ycmd--location-button
  'action #'ycmd--view-jump
  'face nil)

(defun ycmd--view-jump (button)
  "Jump to BUTTON's location in current window."
  (let ((location (button-get button 'location)))
    (ycmd--goto-location location 'find-file)))

(defun ycmd--view-jump-other-window (button)
  "Jump to BUTTON's location in other window."
  (let ((location (button-get button 'location)))
    (ycmd--goto-location location 'find-file-other-window)))

(defun ycmd--view-insert-button (name location)
  "Insert a view button with NAME and LOCATION."
  (insert-text-button
   name
   'type 'ycmd--location-button
   'location location))

(defun ycmd--get-line-from-location (location)
  "Return line from LOCATION."
  (let-alist location
    (--when-let (and .filepath (find-file-noselect .filepath))
      (with-current-buffer it
        (goto-char (ycmd--col-line-to-position
                    .column_num .line_num))
        (back-to-indentation)
        (buffer-substring (point) (line-end-position))))))

(defun ycmd--view-insert-location (location-group mode)
  "Insert LOCATION-GROUP into `current-buffer' and fontify according MODE.
LOCATION-GROUP is a cons cell whose car is the filepath and the whose
cdr is a list of location objects."
  (pcase-let* ((`(,filepath . ,locations) location-group)
               (max-line-num-width
                (cl-loop for location in locations
                         maximize (let ((line-num (cdr (assq 'line_num location))))
                                    (and line-num (length (format "%d" line-num))))))
               (line-num-format (and max-line-num-width
                                     (format "%%%dd:" max-line-num-width))))
    (insert (propertize (concat filepath "\n") 'face 'bold))
    (mapc (lambda (it)
            (let-alist it
              (when line-num-format
                (insert (format line-num-format .line_num)))
              (insert "    ")
              (let ((description (or (and (not (s-blank? .description))
                                          (s-trim-left .description))
                                     (ycmd--get-line-from-location it))))
                (ycmd--view-insert-button
                 (ycmd--fontify-code (or description "") mode) it))
              (insert "\n")))
          locations)))

(defvar ycmd-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'next-error-no-select)
    (define-key map (kbd "p") 'previous-error-no-select)
    (define-key map (kbd "q") 'quit-window)
    map))

(define-derived-mode ycmd-view-mode special-mode "ycmd-view"
  "Major mode for locations view and navigation for `ycmd-mode'.

\\{ycmd-view-mode-map}"
  (setq next-error-function #'ycmd--next-location))

(defun ycmd--next-location (num _reset)
  "Navigate to the next location in the view buffer.
NUM is the number of locations to move forward.  If RESET is
non-nil got to the beginning of buffer before locations
navigation."
  (forward-button num)
  (ycmd--view-jump-other-window (button-at (point))))

(define-button-type 'ycmd--error-button
  'face '(error bold underline)
  'button 't)

(define-button-type 'ycmd--warning-button
  'face '(warning bold underline)
  'button 't)

(defun ycmd--make-button (start end type msg)
  "Make a button from START to END of TYPE in the current buffer.

When clicked, MSG will be shown in the minibuffer."
  (make-text-button
   start end
   'type type
   'action (lambda (_) (message msg))))

(defconst ycmd--file-ready-buttons
  '(("ERROR" . ycmd--error-button)
    ("WARNING" . ycmd--warning-button))
  "A mapping from parse 'kind' to button types.")

(defun ycmd--line-start-position (line)
  "Find position at the start of LINE."
  (save-excursion
    (ycmd--goto-line line)
    (beginning-of-line)
    (point)))

(defun ycmd--line-end-position (line)
  "Find position at the end of LINE."
  (save-excursion
    (ycmd--goto-line line)
    (end-of-line)
    (point)))

(defun ycmd--decorate-single-parse-result (result)
  "Decorates a buffer based on the contents of a single parse RESULT.

This is a fairly crude form of decoration, but it does give
reasonable visual feedback on the problems found by ycmd."
  (let-alist result
    (--when-let (find-buffer-visiting .location.filepath)
      (with-current-buffer it
        (let* ((start-pos (ycmd--line-start-position .location.line_num))
               (end-pos (ycmd--line-end-position .location.line_num))
               (btype (cdr (assoc .kind ycmd--file-ready-buttons))))
          (when btype
            (with-silent-modifications
              (ycmd--make-button
               start-pos end-pos
               btype (concat .kind ": " .text
                             (when (eq .fixit_available t)
                               " (FixIt available)"))))))))))

(defun ycmd-decorate-with-parse-results (results)
  "Decorates a buffer using the RESULTS of a file-ready parse list.

This is suitable as an entry in `ycmd-file-parse-result-hook'."
  (with-silent-modifications
    (set-text-properties (point-min) (point-max) nil))
  (mapc 'ycmd--decorate-single-parse-result results)
  results)

(defun ycmd--display-single-file-parse-result (result)
  "Insert a single file parse RESULT."
  (let-alist result
    (insert (format "%s:%s - %s - %s\n"
                    .location.filepath .location.line_num
                    .kind .text))))

(defun ycmd-display-file-parse-results (results)
  "Display parse RESULTS in a buffer."
  (let ((buffer "*ycmd-file-parse-results*"))
    (get-buffer-create buffer)
    (with-current-buffer buffer
      (erase-buffer)
      (mapc 'ycmd--display-single-file-parse-result results))
    (display-buffer buffer)))

(defun ycmd-parse-buffer ()
  "Parse buffer."
  (interactive)
  (if (not (ycmd-semantic-completer-available-p))
      (message "Native filetype completion not supported for current file, \
cannot send parse request")
    (when (ycmd--server-alive-p)
      (let ((buffer (current-buffer)))
        (deferred:$
          (deferred:next
            (lambda ()
              (message "Parsing buffer...")
              (ycmd--reset-parse-status)
              (ycmd--conditional-parse)))
          (deferred:nextc it
            (lambda ()
              (with-current-buffer buffer
                (when (eq ycmd--last-status-change 'parsed)
                  (message "Parsing buffer done"))))))))))

(defun ycmd--handle-extra-conf-exception (conf-file)
  "Handle an exception of type `UnknownExtraConf'.

Handle CONF-FILE according the value of `ycmd-extra-conf-handler'."
  (if (not conf-file)
      (warn "No extra_conf_file included in UnknownExtraConf exception. \
Consider reporting this.")
    (let ((ignore-p (or (eq ycmd-extra-conf-handler 'ignore)
                        (not (y-or-n-p (format "Load YCMD extra conf %s? "
                                               conf-file))))))
      (ycmd--extra-conf-request conf-file ignore-p)
      (ycmd--reset-parse-status)
      (ycmd-notify-file-ready-to-parse))))

(defun ycmd--event-notification (event-name &optional extra-data)
  "Send a event notification for EVENT-NAME.
Optional EXTRA-DATA contains additional data for the request."
  (let ((content (append (list (cons "event_name" event-name))
                         (ycmd--get-basic-request-data)
                         extra-data)))
    (deferred:try
      (ycmd--request (make-ycmd-request-data
                      :handler "event_notification"
                      :content content))
      :catch
      (lambda (err)
        (message "Error sending %s request: %s" event-name err)
        (ycmd--report-status 'errored)
        nil))))

(defun ycmd-notify-file-ready-to-parse ()
  "Send a notification to ycmd that the buffer is ready to be parsed.

Only one active notification is allowed per buffer, and this
function enforces that constraint.

The response of the notification are passed to all of the
functions in `ycmd-file-parse-result-hook'."
  (when (and ycmd-mode (not (ycmd-parsing-in-progress-p)))
    (ycmd-with-handled-server-exceptions
        (let ((extra-data
               (append (--when-let (and ycmd-tag-files
                                        (ycmd--get-tag-files request-buffer))
                         (list (cons "tag_files" it)))
                       (--when-let (and ycmd-seed-identifiers-with-keywords
                                        (ycmd--get-keywords request-buffer))
                         (list (cons "syntax_keywords" it))))))
          (ycmd--report-status 'parsing)
          (ycmd--event-notification "FileReadyToParse" extra-data))
      :bind-current-buffer t
      :on-exception-form (ycmd--report-status 'errored)
      (ycmd--report-status 'parsed)
      (run-hook-with-args 'ycmd-file-parse-result-hook response))))

(defun ycmd-major-mode-to-file-types (mode)
  "Map a major mode MODE to a list of file-types suitable for ycmd.

If there is no established mapping, return nil."
  (cdr (assq mode ycmd-file-type-map)))

(defun ycmd--on-server-timeout ()
  "Kill server process due to timeout."
  (ycmd-close 'errored)
  (message "ERROR: Ycmd server timeout. If this happens regularly you may need to increase `ycmd-startup-timeout'."))

(defun ycmd--start-server-timeout-timer ()
  "Start the server timeout timer."
  (ycmd--kill-timer ycmd--server-timeout-timer)
  (setq ycmd--server-timeout-timer
        (run-with-timer
         ycmd-startup-timeout nil
         #'ycmd--on-server-timeout)))

(defun ycmd--start-keepalive-timer ()
  "Kill any existing keepalive timer and start a new one."
  (ycmd--kill-timer ycmd--keepalive-timer)
  (setq ycmd--keepalive-timer
        (run-with-timer
         ycmd-keepalive-period
         ycmd-keepalive-period
         #'ycmd--keepalive)))

(defun ycmd--generate-hmac-secret ()
  "Generate a new, random 16-byte HMAC secret key."
  (let ((result '()))
    (dotimes (_ 16 result)
      (setq result (cons (byte-to-string (random 256)) result)))
    (apply 'concat result)))

(defun ycmd--json-encode (obj)
  "Encode a json object OBJ.
A version of json-encode that uses {} instead of null for nil values.
This produces output for empty alists that ycmd expects."
  (cl-letf (((symbol-function 'json-encode-keyword)
             (lambda (k) (cond ((eq k t)          "true")
                               ((eq k json-false) "false")
                               ((eq k json-null)  "{}")))))
    (json-encode obj)))

;; This defines 'ycmd--hmac-function which we use to combine an HMAC
;; key and message contents.
(defun ycmd--secure-hash (x)
  "Generate secure sha256 hash of X."
  (secure-hash 'sha256 x nil nil 1))
(define-hmac-function ycmd--hmac-function
  ycmd--secure-hash 64 64)

(defun ycmd--options-contents (hmac-secret)
  "Return a struct with ycmd options and the HMAC-SECRET applied.
The struct can be json encoded into a file to create a ycmd
options file.

When we start a new ycmd server, it needs an options file.  It
reads this file and then deletes it since it contains a secret
key.  So we need to generate a new options file for each ycmd
instance.  This function effectively produces the contents of that
file."
  (let ((hmac-secret (base64-encode-string hmac-secret))
        (global-config (or ycmd-global-config ""))
        (extra-conf-whitelist (or ycmd-extra-conf-whitelist []))
        (confirm-extra-conf (if (eq ycmd-extra-conf-handler 'load) 0 1))
        (gocode-binary-path (or ycmd-gocode-binary-path ""))
        (godef-binary-path (or ycmd-godef-binary-path ""))
        (rust-src-path (or ycmd-rust-src-path ""))
        (racerd-binary-path (or ycmd-racerd-binary-path ""))
        (python-binary-path (or ycmd-python-binary-path ""))
        (auto-trigger (if ycmd-auto-trigger-semantic-completion 1 0)))
    `((filepath_completion_use_working_dir . 0)
      (auto_trigger . ,auto-trigger)
      (min_num_of_chars_for_completion . ,ycmd-min-num-chars-for-completion)
      (min_num_identifier_candidate_chars . 0)
      (semantic_triggers . ())
      (filetype_specific_completion_to_disable (gitcommit . 1))
      (collect_identifiers_from_comments_and_strings . 0)
      (max_num_identifier_candidates . ,ycmd-max-num-identifier-candidates)
      (extra_conf_globlist . ,extra-conf-whitelist)
      (global_ycm_extra_conf . ,global-config)
      (confirm_extra_conf . ,confirm-extra-conf)
      (max_diagnostics_to_display . 30)
      (auto_start_csharp_server . 1)
      (auto_stop_csharp_server . 1)
      (use_ultisnips_completer . 1)
      (csharp_server_port . 0)
      (hmac_secret . ,hmac-secret)
      (server_keep_logfiles . 1)
      (gocode_binary_path . ,gocode-binary-path)
      (godef_binary_path . ,godef-binary-path)
      (rust_src_path . ,rust-src-path)
      (racerd_binary_path . ,racerd-binary-path)
      (python_binary_path . ,python-binary-path))))

(defun ycmd--create-options-file (hmac-secret)
  "Create a new options file for a ycmd server with HMAC-SECRET.

This creates a new tempfile and fills it with options.  Returns
the name of the newly created file."
  (let ((options-file (make-temp-file "ycmd-options"))
        (options (ycmd--options-contents hmac-secret)))
    (with-temp-file options-file
      (insert (ycmd--json-encode options)))
    options-file))

(defun ycmd--exit-code-as-string (code)
  "Return exit status message for CODE."
  (pcase code
    (`3 "unexpected error while loading ycm_core.")
    (`4 (concat "ycm_core library not detected; "
                "you need to compile it by running the "
                "build.py script. See the documentation "
                "for more details."))
    (`5 (concat "ycm_core library compiled for Python 2 "
                "but loaded in Python 3."))
    (`6 (concat "ycm_core library compiled for Python 3 "
                "but loaded in Python 2."))
    (`7 (concat "ycm_core library too old; "
                "PLEASE RECOMPILE by running the build.py "
                "script. See the documentation for more details."))))

(defun ycmd--server-process-sentinel (process event)
  "Handle Ycmd server PROCESS EVENT."
  (when (memq (process-status process) '(exit signal))
    (let* ((code (process-exit-status process))
           (status (if (eq code 0) 'stopped 'errored)))
      (when (eq status 'errored)
        (--if-let (and (eq (process-status process) 'exit)
                       (ycmd--exit-code-as-string code))
            (message "Ycmd server error: %s" it)
          (message "Ycmd server %s" (s-replace "\n" "" event))))
      (ycmd--with-all-ycmd-buffers
        (ycmd--report-status status))
      (ycmd--kill-timer ycmd--keepalive-timer))))

(defun ycmd--server-process-filter (process string)
  "Filter function for the Ycmd server PROCESS output STRING."
  ;; insert string into process-buffer
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((moving (= (point) (process-mark process))))
        (save-excursion
          (goto-char (process-mark process))
          (let ((inhibit-read-only t))
            (insert-before-markers string))
          (set-marker (process-mark process) (point)))
        (when moving (goto-char (process-mark process))))))
  ;; parse port from server output
  (when (and (not ycmd--server-actual-port)
             (string-match "^serving on http://.*:\\\([0-9]+\\\)$"
                           string))
    (ycmd--kill-timer ycmd--server-timeout-timer)
    (setq ycmd--server-actual-port
          (string-to-number (match-string 1 string)))
    (ycmd--with-all-ycmd-buffers
      (ycmd--reset-parse-status))
    (ycmd--perform-deferred-parse)))

(defun ycmd--get-process-environment ()
  "Return `process-evironment'.
If `ycmd-bypass-url-proxy-services' is non-nil, prepend
`no_proxy' variable to environment."
  (or ycmd--process-environment
      (setq ycmd--process-environment
            (append (and ycmd-bypass-url-proxy-services
                         (not (or (getenv "NO_PROXY")
                                  (getenv "no_PROXY")
                                  (getenv "no_proxy")))
                         (list (concat "NO_PROXY=" ycmd-host)))
                    process-environment))))

(defun ycmd--start-server ()
  "Start a new server and return the process."
  (unless ycmd-server-command
    (user-error "Error: The variable `ycmd-server-command' is not set.  \
See the docstring of the variable for an example"))
  (let ((proc-buff (get-buffer-create ycmd--server-buffer-name)))
    (with-current-buffer proc-buff
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (buffer-disable-undo)
        (erase-buffer)))
    (setq ycmd--process-environment nil)
    (let* ((port (and (numberp ycmd-server-port)
                      (> ycmd-server-port 0)
                      ycmd-server-port))
           (hmac-secret (ycmd--generate-hmac-secret))
           (options-file (ycmd--create-options-file hmac-secret))
           (args (append (and port (list (format "--port=%d" port)))
                         (list (concat "--options_file=" options-file))
                         ycmd-server-args))
           (server-program+args (append ycmd-server-command args))
           (process-environment (ycmd--get-process-environment))
           (proc (apply #'start-process ycmd--server-process-name proc-buff
                        server-program+args)))
      (ycmd--with-all-ycmd-buffers
        (ycmd--report-status 'starting))
      (setq ycmd--server-actual-port nil
            ycmd--hmac-secret hmac-secret)
      (set-process-query-on-exit-flag proc nil)
      (set-process-sentinel proc #'ycmd--server-process-sentinel)
      (set-process-filter proc #'ycmd--server-process-filter)
      proc)))

(defun ycmd-wait-until-server-is-ready (&optional include-subserver)
  "Wait until server is ready.
If INCLUDE-SUBSERVER is non-nil wait until subserver is ready.
Return t when server is ready.  Signal error in case of timeout.
The timeout can be set with the variable
`ycmd-startup-timeout'."
  (catch 'ready
    (let ((server-start-time (float-time)))
      (ycmd--kill-timer ycmd--server-timeout-timer)
      (while (ycmd-running-p)
        (sit-for 0.1)
        (if (ycmd--server-ready-p include-subserver)
            (progn
              (ycmd--with-all-ycmd-buffers
                (ycmd--reset-parse-status))
              (throw 'ready t))
          ;; timeout after specified period
          (when (< ycmd-startup-timeout
                   (- (float-time) server-start-time))
            (ycmd--on-server-timeout)))))))

(defun ycmd--column-in-bytes ()
  "Calculate column offset in bytes for the current position and buffer."
  (- (position-bytes (point))
     (position-bytes (line-beginning-position))))

;; https://github.com/abingham/emacs-ycmd/issues/165
(eval-and-compile
  (if (version-list-< (version-to-list emacs-version) '(25))
      (defun ycmd--encode-string (s) s)
    (defun ycmd--encode-string (s) (encode-coding-string s 'utf-8 t))))

(defun ycmd--get-basic-request-data ()
  "Build the basic request data alist for a server request."
  (let* ((column-num (+ 1 (ycmd--column-in-bytes)))
         (line-num (line-number-at-pos (point)))
         (full-path (ycmd--encode-string (or (buffer-file-name) "")))
         (file-contents (ycmd--encode-string
                         (buffer-substring-no-properties
                          (point-min) (point-max))))
         (file-types (or (ycmd-major-mode-to-file-types major-mode)
                         '("generic"))))
    `(("file_data" .
       ((,full-path . (("contents" . ,file-contents)
                       ("filetypes" . ,file-types)))))
      ("filepath" . ,full-path)
      ("line_num" . ,line-num)
      ("column_num" . ,column-num))))


(defvar ycmd--log-enabled nil
  "If non-nil, http content will be logged.
This is useful for debugging.")

(defun ycmd-toggle-log-enabled ()
  "Toggle `ycmd--log-enabled' variable."
  (interactive)
  (let ((log-enabled (not ycmd--log-enabled)))
    (message "Ycmd Log %s" (if log-enabled "enabled" "disabled"))
    (setq ycmd--log-enabled log-enabled)))

(defun ycmd--log-content (header content)
  "Insert log with HEADER and CONTENT in a buffer."
  (when ycmd--log-enabled
    (let ((buffer (get-buffer-create "*ycmd-content-log*")))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-max))
          (insert (format "\n%s\n\n" header))
          (insert (pp-to-string content)))))))

(defun ycmd-show-debug-info ()
  "Show debug information."
  (interactive)
  (let ((data (make-ycmd-request-data :handler "debug_info"))
        (buffer (current-buffer)))
    (with-help-window (get-buffer-create " *ycmd-debug-info*")
      (with-current-buffer standard-output
        (princ "Ycmd debug information for buffer ")
        (insert (propertize (buffer-name buffer) 'face 'bold))
        (princ " in ")
        (let ((mode (buffer-local-value 'major-mode buffer)))
          (insert-button (symbol-name mode)
                         'type 'help-function
                         'help-args (list mode)))
        (princ ":\n\n")
        (--if-let (and (ycmd--server-alive-p)
                       (ycmd-deferred:sync!
                        (ycmd-with-handled-server-exceptions
                            (ycmd--request data))))
            (pp it)
          (princ "No debug info available from server"))
        (princ "\n\n")
        (princ "Server is ")
        (let ((running (ycmd--server-alive-p)))
          (insert (propertize (if running "running" "not running")
                              'face (if running 'success '(warning bold))))
          (when running
            (insert
             (format " at: %s:%d" ycmd-host ycmd--server-actual-port))))
        (princ "\n\n")
        (princ "Ycmd Mode is ")
        (let ((enabled (buffer-local-value 'ycmd-mode buffer)))
          (insert (propertize (if enabled "enabled" "disabled")
                              'face (if enabled 'success '(warning bold)))))
        (save-excursion
          (let ((end (point)))
            (backward-paragraph)
            (fill-region-as-paragraph (point) end)))

        (princ "\n\n--------------------\n\n")
        (princ (format "Ycmd version:   %s\n" (ignore-errors (ycmd-version))))
        (princ (format "Emacs version:  %s\n" emacs-version))
        (princ (format "System:         %s\n" system-configuration))
        (princ (format "Window system:  %S\n" window-system))))))

(defun ycmd-filter-and-sort-candidates (request-data)
  "Use ycmd to filter and sort identifiers from REQUEST-DATA.

This request allows to use ycmd's filtering and sorting
mechanism on arbitrary sets of identifiers.

The request data should be something like:

\((candidates \"candidate1\" \"candidate2\")
 (sort_property . \"\")
 (query . \"cand\"))

If candidates is a list with identifiers, sort_property should be
and empty string, however when candidates is a more complex
structure it is used to specify the sort key."
  (let ((data (make-ycmd-request-data
               :handler "filter_and_sort_candidates"
               :content request-data)))
    (ycmd-deferred:sync!
     (ycmd-with-handled-server-exceptions (ycmd--request data)))))

(defun ycmd--send-completer-available-request (&optional mode)
  "Send request to check if a semantic completer exists for MODE.
Response is non-nil if semantic complettion is available."
  (let ((data (make-ycmd-request-data
               :handler "semantic_completion_available")))
    (when mode
      (let* ((buffer (current-buffer))
             (full-path (ycmd--encode-string (or (buffer-file-name buffer) "")))
             (content (ycmd-request-data-content data))
             (file-types (assoc "filetypes"
                                (assoc full-path
                                       (assoc "file_data" content)))))
        (when (consp file-types)
          (setcdr file-types (ycmd-major-mode-to-file-types mode)))))
    (ycmd-deferred:sync!
     (ycmd-with-handled-server-exceptions (ycmd--request data)))))

(defun ycmd-semantic-completer-available-p ()
  "Return t if a semantic completer is available for current `major-mode'."
  (let ((mode major-mode))
    (or (gethash mode ycmd--available-completers)
        (--when-let (ycmd--send-completer-available-request mode)
          (puthash mode (or (eq it t) 'none) ycmd--available-completers)))))

(defun ycmd--get-request-hmac (method path body)
  "Generate HMAC for request from METHOD, PATH and BODY."
  (ycmd--hmac-function
   (mapconcat (lambda (val)
                (ycmd--hmac-function
                 (ycmd--encode-string val) ycmd--hmac-secret))
              `(,method ,path ,(or body "")) "")
   ycmd--hmac-secret))

(cl-defstruct ycmd-request-data
  "Structure for storing the ycmd server request data.

Slots:

`handler'
     Specifies the the path portion of the URL. For example, if
     HANDLER is 'feed_llama', the request URL is
     'http://host:port/feed_llama'.

`content'
     An alist that will be JSON-encoded and sent over at the
     content of the HTTP message."
  handler
  (content (ycmd--get-basic-request-data)))

(cl-defun ycmd--request (request-data
                         &key
                         (type "POST")
                         (params nil))
  "Send an asynchronous HTTP request to the ycmd server.

This starts the server if necessary.

Returns a deferred object which resolves to the content of the
response message.

REQUEST-DATA is a `ycmd-request-data' structure.

PARSER specifies the function that will be used to parse the
response to the message. Typical values are buffer-string and
json-read. This function will be passed an the completely
unmodified contents of the response (i.e. not JSON-decoded or
anything like that)."
  (unless (ycmd--server-alive-p)
    (message "Ycmd server is not running. Can't send `%s' request!"
             (ycmd-request-data-handler request-data))
    (cl-return-from ycmd--request (deferred:next)))

  (let* ((url-show-status (not ycmd-hide-url-status))
         (url-proxy-services (unless ycmd-bypass-url-proxy-services
                               url-proxy-services))
         (process-environment (ycmd--get-process-environment))
         (path (concat "/" (ycmd-request-data-handler request-data)))
         (content (json-encode (ycmd-request-data-content request-data)))
         (hmac (ycmd--get-request-hmac type path content))
         (encoded-hmac (base64-encode-string hmac 't))
         (url (format "http://%s:%s%s"
                      ycmd-host ycmd--server-actual-port path))
         (headers `(("Content-Type" . "application/json")
                    ("X-Ycm-Hmac" . ,encoded-hmac)))
         (parser (lambda ()
                   (let ((json-array-type 'list))
                     (json-read)))))
    (ycmd--log-content "HTTP REQUEST CONTENT" content)

    (deferred:$
      (request-deferred url :type type :params params :data content
                        :parser parser :headers headers)
      (deferred:nextc it
        (lambda (response)
          (let ((data (request-response-data response)))
            (ycmd--log-content "HTTP RESPONSE CONTENT" data)
            data))))))

(provide 'ycmd)

;;; ycmd.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
