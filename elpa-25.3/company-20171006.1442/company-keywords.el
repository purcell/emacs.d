;;; company-keywords.el --- A company backend for programming language keywords

;; Copyright (C) 2009-2011, 2016  Free Software Foundation, Inc.

;; Author: Nikolaj Schumacher

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(require 'company)
(require 'cl-lib)

(defun company-keywords-upper-lower (&rest lst)
  ;; Upcase order is different for _.
  (nconc (sort (mapcar 'upcase lst) 'string<) lst))

(defvar company-keywords-alist
  ;; Please contribute corrections or additions.
  `((c++-mode
     "alignas" "alignof" "asm" "auto" "bool" "break" "case" "catch" "char"
     "char16_t" "char32_t" "class" "const" "const_cast" "constexpr" "continue"
     "decltype" "default" "delete" "do" "double" "dynamic_cast" "else" "enum"
     "explicit" "export" "extern" "false" "final" "float" "for" "friend"
     "goto" "if" "inline" "int" "long" "mutable" "namespace" "new" "noexcept"
     "nullptr" "operator" "override"
     "private" "protected" "public" "register" "reinterpret_cast"
     "return" "short" "signed" "sizeof" "static" "static_assert"
     "static_cast" "struct" "switch" "template" "this" "thread_local"
     "throw" "true" "try" "typedef" "typeid" "typename"
     "union" "unsigned" "using" "virtual" "void" "volatile" "wchar_t" "while")
    (c-mode
     "auto" "break" "case" "char" "const" "continue" "default" "do"
     "double" "else" "enum" "extern" "float" "for" "goto" "if" "int" "long"
     "register" "return" "short" "signed" "sizeof" "static" "struct"
     "switch" "typedef" "union" "unsigned" "void" "volatile" "while")
    (csharp-mode
     "abstract" "add" "alias" "as" "base" "bool" "break" "byte" "case"
     "catch" "char" "checked" "class" "const" "continue" "decimal" "default"
     "delegate" "do" "double" "else" "enum" "event" "explicit" "extern"
     "false" "finally" "fixed" "float" "for" "foreach" "get" "global" "goto"
     "if" "implicit" "in" "int" "interface" "internal" "is" "lock" "long"
     "namespace" "new" "null" "object" "operator" "out" "override" "params"
     "partial" "private" "protected" "public" "readonly" "ref" "remove"
     "return" "sbyte" "sealed" "set" "short" "sizeof" "stackalloc" "static"
     "string" "struct" "switch" "this" "throw" "true" "try" "typeof" "uint"
     "ulong" "unchecked" "unsafe" "ushort" "using" "value" "var" "virtual"
     "void" "volatile" "where" "while" "yield")
    (d-mode
     ;; from http://www.digitalmars.com/d/2.0/lex.html
     "abstract" "alias" "align" "asm"
     "assert" "auto" "body" "bool" "break" "byte" "case" "cast" "catch"
     "cdouble" "cent" "cfloat" "char" "class" "const" "continue" "creal"
     "dchar" "debug" "default" "delegate" "delete" "deprecated" "do"
     "double" "else" "enum" "export" "extern" "false" "final" "finally"
     "float" "for" "foreach" "foreach_reverse" "function" "goto" "idouble"
     "if" "ifloat" "import" "in" "inout" "int" "interface" "invariant"
     "ireal" "is" "lazy" "long" "macro" "mixin" "module" "new" "nothrow"
     "null" "out" "override" "package" "pragma" "private" "protected"
     "public" "pure" "real" "ref" "return" "scope" "short" "static" "struct"
     "super" "switch" "synchronized" "template" "this" "throw" "true" "try"
     "typedef" "typeid" "typeof" "ubyte" "ucent" "uint" "ulong" "union"
     "unittest" "ushort" "version" "void" "volatile" "wchar" "while" "with")
    (f90-mode .
     ;; from f90.el
     ;; ".AND." ".GE." ".GT." ".LT." ".LE." ".NE." ".OR." ".TRUE." ".FALSE."
     ,(company-keywords-upper-lower
      "abs" "abstract" "achar" "acos" "adjustl" "adjustr" "aimag" "aint"
      "align" "all" "all_prefix" "all_scatter" "all_suffix" "allocatable"
      "allocate" "allocated" "and" "anint" "any" "any_prefix" "any_scatter"
      "any_suffix" "asin" "assign" "assignment" "associate" "associated"
      "asynchronous" "atan" "atan2" "backspace" "bind" "bit_size" "block"
      "btest" "c_alert" "c_associated" "c_backspace" "c_bool"
      "c_carriage_return" "c_char" "c_double" "c_double_complex" "c_f_pointer"
      "c_f_procpointer" "c_float" "c_float_complex" "c_form_feed" "c_funloc"
      "c_funptr" "c_horizontal_tab" "c_int" "c_int16_t" "c_int32_t" "c_int64_t"
      "c_int8_t" "c_int_fast16_t" "c_int_fast32_t" "c_int_fast64_t"
      "c_int_fast8_t" "c_int_least16_t" "c_int_least32_t" "c_int_least64_t"
      "c_int_least8_t" "c_intmax_t" "c_intptr_t" "c_loc" "c_long"
      "c_long_double" "c_long_double_complex" "c_long_long" "c_new_line"
      "c_null_char" "c_null_funptr" "c_null_ptr" "c_ptr" "c_short"
      "c_signed_char" "c_size_t" "c_vertical_tab" "call" "case" "ceiling"
      "char" "character" "character_storage_size" "class" "close" "cmplx"
      "command_argument_count" "common" "complex" "conjg" "contains" "continue"
      "copy_prefix" "copy_scatter" "copy_suffix" "cos" "cosh" "count"
      "count_prefix" "count_scatter" "count_suffix" "cpu_time" "cshift"
      "cycle" "cyclic" "data" "date_and_time" "dble" "deallocate" "deferred"
      "digits" "dim" "dimension" "distribute" "do" "dot_product" "double"
      "dprod" "dynamic" "elemental" "else" "elseif" "elsewhere" "end" "enddo"
      "endfile" "endif" "entry" "enum" "enumerator" "eoshift" "epsilon" "eq"
      "equivalence" "eqv" "error_unit" "exit" "exp" "exponent" "extends"
      "extends_type_of" "external" "extrinsic" "false" "file_storage_size"
      "final" "floor" "flush" "forall" "format" "fraction" "function" "ge"
      "generic" "get_command" "get_command_argument" "get_environment_variable"
      "goto" "grade_down" "grade_up" "gt" "hpf_alignment" "hpf_distribution"
      "hpf_template" "huge" "iachar" "iall" "iall_prefix" "iall_scatter"
      "iall_suffix" "iand" "iany" "iany_prefix" "iany_scatter" "iany_suffix"
      "ibclr" "ibits" "ibset" "ichar" "ieee_arithmetic" "ieee_exceptions"
      "ieee_features" "ieee_get_underflow_mode" "ieee_set_underflow_mode"
      "ieee_support_underflow_control" "ieor" "if" "ilen" "implicit"
      "import" "include" "independent" "index" "inherit" "input_unit"
      "inquire" "int" "integer" "intent" "interface" "intrinsic" "ior"
      "iostat_end" "iostat_eor" "iparity" "iparity_prefix" "iparity_scatter"
      "iparity_suffix" "ishft" "ishftc" "iso_c_binding" "iso_fortran_env"
      "kind" "lbound" "le" "leadz" "len" "len_trim" "lge" "lgt" "lle" "llt"
      "log" "log10" "logical" "lt" "matmul" "max" "maxexponent" "maxloc"
      "maxval" "maxval_prefix" "maxval_scatter" "maxval_suffix" "merge"
      "min" "minexponent" "minloc" "minval" "minval_prefix" "minval_scatter"
      "minval_suffix" "mod" "module" "modulo" "move_alloc" "mvbits" "namelist"
      "ne" "nearest" "neqv" "new" "new_line" "nint" "non_intrinsic"
      "non_overridable" "none" "nopass" "not" "null" "nullify"
      "number_of_processors" "numeric_storage_size" "only" "onto" "open"
      "operator" "optional" "or" "output_unit" "pack" "parameter" "parity"
      "parity_prefix" "parity_scatter" "parity_suffix" "pass" "pause"
      "pointer" "popcnt" "poppar" "precision" "present" "print" "private"
      "procedure" "processors" "processors_shape" "product" "product_prefix"
      "product_scatter" "product_suffix" "program" "protected" "public"
      "pure" "radix" "random_number" "random_seed" "range" "read" "real"
      "realign" "recursive" "redistribute" "repeat" "reshape" "result"
      "return" "rewind" "rrspacing" "same_type_as" "save" "scale" "scan"
      "select" "selected_char_kind" "selected_int_kind" "selected_real_kind"
      "sequence" "set_exponent" "shape" "sign" "sin" "sinh" "size" "spacing"
      "spread" "sqrt" "stop" "subroutine" "sum" "sum_prefix" "sum_scatter"
      "sum_suffix" "system_clock" "tan" "tanh" "target" "template" "then"
      "tiny" "transfer" "transpose" "trim" "true" "type" "ubound" "unpack"
      "use" "value" "verify" "volatile" "wait" "where" "while" "with" "write"))
    (java-mode
     "abstract" "assert" "boolean" "break" "byte" "case" "catch" "char" "class"
     "continue" "default" "do" "double" "else" "enum" "extends" "final"
     "finally" "float" "for" "if" "implements" "import" "instanceof" "int"
     "interface" "long" "native" "new" "package" "private" "protected" "public"
     "return" "short" "static" "strictfp" "super" "switch" "synchronized"
     "this" "throw" "throws" "transient" "try" "void" "volatile" "while")
    (javascript-mode
     "break" "catch" "const" "continue" "delete" "do" "else" "export" "for"
     "function" "if" "import" "in" "instanceOf" "label" "let" "new" "return"
     "switch" "this" "throw" "try" "typeof" "var" "void" "while" "with" "yield")
    (objc-mode
     "@catch" "@class" "@encode" "@end" "@finally" "@implementation"
     "@interface" "@private" "@protected" "@protocol" "@public"
     "@selector" "@synchronized" "@throw" "@try" "alloc" "autorelease"
     "bycopy" "byref" "in" "inout" "oneway" "out" "release" "retain")
    (perl-mode
     ;; from cperl.el
     "AUTOLOAD" "BEGIN" "CHECK" "CORE" "DESTROY" "END" "INIT" "__END__"
     "__FILE__" "__LINE__" "abs" "accept" "alarm" "and" "atan2" "bind"
     "binmode" "bless" "caller" "chdir" "chmod" "chomp" "chop" "chown" "chr"
     "chroot" "close" "closedir" "cmp" "connect" "continue" "cos"
     "crypt" "dbmclose" "dbmopen" "defined" "delete" "die" "do" "dump" "each"
     "else" "elsif" "endgrent" "endhostent" "endnetent" "endprotoent"
     "endpwent" "endservent" "eof" "eq" "eval" "exec" "exists" "exit" "exp"
     "fcntl" "fileno" "flock" "for" "foreach" "fork" "format" "formline"
     "ge" "getc" "getgrent" "getgrgid" "getgrnam" "gethostbyaddr"
     "gethostbyname" "gethostent" "getlogin" "getnetbyaddr" "getnetbyname"
     "getnetent" "getpeername" "getpgrp" "getppid" "getpriority"
     "getprotobyname" "getprotobynumber" "getprotoent" "getpwent" "getpwnam"
     "getpwuid" "getservbyname" "getservbyport" "getservent" "getsockname"
     "getsockopt" "glob" "gmtime" "goto" "grep" "gt" "hex" "if" "index" "int"
     "ioctl" "join" "keys" "kill" "last" "lc" "lcfirst" "le" "length"
     "link" "listen" "local" "localtime" "lock" "log" "lstat" "lt" "map"
     "mkdir" "msgctl" "msgget" "msgrcv" "msgsnd" "my" "ne" "next" "no"
     "not" "oct" "open" "opendir" "or" "ord" "our" "pack" "package" "pipe"
     "pop" "pos" "print" "printf" "push" "q" "qq" "quotemeta" "qw" "qx"
     "rand" "read" "readdir" "readline" "readlink" "readpipe" "recv" "redo"
     "ref" "rename" "require" "reset" "return" "reverse" "rewinddir" "rindex"
     "rmdir" "scalar" "seek" "seekdir" "select" "semctl" "semget" "semop"
     "send" "setgrent" "sethostent" "setnetent" "setpgrp" "setpriority"
     "setprotoent" "setpwent" "setservent" "setsockopt" "shift" "shmctl"
     "shmget" "shmread" "shmwrite" "shutdown" "sin" "sleep" "socket"
     "socketpair" "sort" "splice" "split" "sprintf" "sqrt" "srand" "stat"
     "study" "sub" "substr" "symlink" "syscall" "sysopen" "sysread" "system"
     "syswrite" "tell" "telldir" "tie" "time" "times" "tr" "truncate" "uc"
     "ucfirst" "umask" "undef" "unless" "unlink" "unpack" "unshift" "untie"
     "until" "use" "utime" "values" "vec" "wait" "waitpid"
     "wantarray" "warn" "while" "write" "x" "xor" "y")
    (php-mode
     "__CLASS__" "__DIR__" "__FILE__" "__FUNCTION__" "__LINE__" "__METHOD__"
     "__NAMESPACE__" "_once" "abstract" "and" "array" "as" "break" "case"
     "catch" "cfunction" "class" "clone" "const" "continue" "declare"
     "default" "die" "do" "echo" "else" "elseif" "empty" "enddeclare"
     "endfor" "endforeach" "endif" "endswitch" "endwhile" "eval" "exception"
     "exit" "extends" "final" "for" "foreach" "function" "global"
     "goto" "if" "implements" "include" "instanceof" "interface"
     "isset" "list" "namespace" "new" "old_function" "or" "php_user_filter"
     "print" "private" "protected" "public" "require" "require_once" "return"
     "static" "switch" "this" "throw" "try" "unset" "use" "var" "while" "xor")
    (python-mode
     "and" "assert" "break" "class" "continue" "def" "del" "elif" "else"
     "except" "exec" "finally" "for" "from" "global" "if" "import" "in" "is"
     "lambda" "not" "or" "pass" "print" "raise" "return" "try" "while" "yield")
    (ruby-mode
     "BEGIN" "END" "alias" "and"  "begin" "break" "case" "class" "def" "defined?"
     "do" "else" "elsif"  "end" "ensure" "false" "for" "if" "in" "module"
     "next" "nil" "not" "or" "redo" "rescue" "retry" "return" "self" "super"
     "then" "true" "undef" "unless" "until" "when" "while" "yield")
    ;; From https://doc.rust-lang.org/grammar.html#keywords
    ;; but excluding unused reserved words: https://www.reddit.com/r/rust/comments/34fq0k/is_there_a_good_list_of_rusts_keywords/cqucvnj
    (go-mode
     "break" "case" "chan" "const" "continue" "default" "defer" "else" "fallthrough"
     "for" "func" "go" "goto" "if" "import" "interface" "map" "package" "range"
     "return" "select" "struct" "switch" "type" "var")
    (rust-mode
     "Self"
     "as" "box" "break" "const" "continue" "crate" "else" "enum" "extern"
     "false" "fn" "for" "if" "impl" "in" "let" "loop" "macro" "match" "mod"
     "move" "mut" "pub" "ref" "return" "self" "static" "struct" "super"
     "trait" "true" "type" "unsafe" "use" "where" "while")
    (scala-mode
     "abstract" "case" "catch" "class" "def" "do" "else" "extends" "false"
     "final" "finally" "for" "forSome" "if" "implicit" "import" "lazy" "match"
     "new" "null" "object" "override" "package" "private" "protected"
     "return" "sealed" "super" "this" "throw" "trait" "true" "try" "type" "val"
     "var" "while" "with" "yield")
    (julia-mode
     "abstract" "break" "case" "catch" "const" "continue" "do" "else" "elseif"
     "end" "eval" "export" "false" "finally" "for" "function" "global" "if"
     "ifelse" "immutable" "import" "importall" "in" "let" "macro" "module"
     "otherwise" "quote" "return" "switch" "throw" "true" "try" "type"
     "typealias" "using" "while"
     )
    ;; aliases
    (js2-mode . javascript-mode)
    (js2-jsx-mode . javascript-mode)
    (espresso-mode . javascript-mode)
    (js-mode . javascript-mode)
    (js-jsx-mode . javascript-mode)
    (cperl-mode . perl-mode)
    (jde-mode . java-mode)
    (ess-julia-mode . julia-mode)
    (enh-ruby-mode . ruby-mode))
  "Alist mapping major-modes to sorted keywords for `company-keywords'.")

;;;###autoload
(defun company-keywords (command &optional arg &rest ignored)
  "`company-mode' backend for programming language keywords."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-keywords))
    (prefix (and (assq major-mode company-keywords-alist)
                 (not (company-in-string-or-comment))
                 (or (company-grab-symbol) 'stop)))
    (candidates
     (let ((completion-ignore-case nil)
           (symbols (cdr (assq major-mode company-keywords-alist))))
       (all-completions arg (if (consp symbols)
                                symbols
                              (cdr (assq symbols company-keywords-alist))))))
    (sorted t)))

(provide 'company-keywords)
;;; company-keywords.el ends here
