;;; cpputils-cmake.el --- Easy realtime C++ syntax check and IntelliSense with CMake.

;; Copyright (C) 2014-2017 Chen Bin

;; Author: Chen Bin <chenbin.sh@gmail.com>
;; URL: http://github.com/redguardtoo/cpputils-cmake
;; Package-Version: 20170819.259
;; Keywords: CMake IntelliSense Flymake Flycheck
;; Version: 0.5.7

;; This file is not part of GNU Emacs.

;; This file is free software (GPLv3 License)

;; Commentary:

;; Ths package provides `cppcm-reload-all', you can call
;; it IF AND ONLY IF you are editing C/C++ file.
;;
;; Example setup:
;;  (add-hook 'c-mode-common-hook (lambda ()
;;              (if (derived-mode-p 'c-mode 'c++-mode)
;;                  (cppcm-reload-all))))
;;
;; Above setup is enough for 99% use cases.
;; Please follow below steps before using Emacs:
;;   Step 1, run "cmake" to create out build directory
;;   Step 2, run "make" to compile SUCCESSFULLY in tbuild directory
;;
;; BTW, you can "M-x cppcm-reload-all" anytime to re-scan the source,
;; Check https://github.com/redguardtoo/cpputils-cmake/ for more tips

;;; Code:

(require 'json)

(defcustom cppcm-proj-max-dir-level 16 "maximum level of the project directory tree"
  :type 'number
  :group 'cpputils-cmake)

(defcustom cppcm-build-dirname "build" "The directory name of build directory"
  :type 'string
  :group 'cpputils-cmake)

(defcustom cppcm-reload-all-hook nil
  "hook after cppcm-reload-all is called. You can modify the global variables set up by cppcm-reload-all"
  :type 'hook
  :group 'cpputils-cmake)

(defvar cppcm-get-executable-full-path-callback nil
  "User defined function to get correct path of executabe.
Sample definition:
(setq cppcm-get-executable-full-path-callback
      (lambda (path type tgt-name)
        (message \"cppcm-get-executable-full-path-callback called => %s %s %s\" path type tgt-name)
        ;; path is the supposed-to-be target's full path
        ;; type is either add_executabe or add_library
        ;; tgt-name is the target to built. The target's file extension is stripped
        (let* ((dir (file-name-directory path))
              (file (file-name-nondirectory path)))
          (cond
           ((string= type \"add_executable\")
            (setq path (concat dir \"bin/\" file)))
           ;; for add_library
           (t (setq path (concat dir \"lib/\" file)))
            ))
          path))")

(defvar cppcm-extra-preprocss-flags-from-user nil
  "Value example: (\"-I/usr/src/include\" \"-I./inc\" \"-DNDEBUG\").")

(defvar cppcm-build-dir nil "The full path of build directory")
(defvar cppcm-src-dir nil "The full path of root source directory")
(defvar cppcm-include-dirs nil "Value example: (\"-I/usr/src/include\")")
(defvar cppcm-preprocess-defines nil "Value example: (\"-DNDEBUG\" \"-D_WXGTK_\")")

(defvar cppcm-hash nil)
(defconst cppcm-prog "cpputils-cmake")
(defcustom cppcm-makefile-name "Makefile" "The filename for cppcm makefiles"
  :type 'string
  :group 'cpputils-cmake)

(defvar cppcm-cmake-target-regex
  "^\s*[^#]*\s*\\(add_executable\\|add_library\\)\s*(\s*\\([^\s]+\\)"
  "Regex for matching a CMake target definition")

(defcustom cppcm-write-flymake-makefile t "Toggle cpputils-cmake writing Flymake Makefiles"
  :type 'boolean
  :group 'cpputils-cmake)

(defvar cppcm-semantic-system-include nil
  "Store include directories `semantic' uses.")

(defvar cppcm-compile-list
  '(cppcm-compile-in-current-exe-dir
    compile
    cppcm-compile-in-root-build-dir)
  "The list of compile commands.
The sequence is the calling sequence when give prefix argument.

For example:
  If you use the default sequence, such as
  '(cppcm-compile-in-current-exe-dir
    compile
    cppcm-compile-in-root-build-dir)
  then you can run following commands.
'M-x cppcm-compile'         => `cppcm-compile-in-current-exe-dir'
'C-u M-x cppcm-compile'     => `compile'
'C-u C-u M-x cppcm-compile' => `cppcm-compile-in-root-build-dir'.")

(defvar cppcm-debug nil "enable debug mode")

(defun cppcm--cmakelists-dot-txt (dir)
  (concat (file-name-as-directory dir) "CMakeLists.txt"))

(defun cppcm-starts-with (s begins)
  "Return non-nil if string S starts with BEGINS."
  (cond ((>= (length s) (length begins))
         (string-equal (substring s 0 (length begins)) begins))
        (t nil)))

(defun cppcm--exe-hashkey (dir)
  (concat (file-name-as-directory dir) "exe-full-path"))

(defun cppcm--flags-hashkey (dir)
  (concat (file-name-as-directory dir) "cpp-flags"))

(defun cppcm-share-str (msg)
  (kill-new msg)
  (with-temp-buffer
    (insert msg)
    (shell-command-on-region (point-min) (point-max)
                             (cond
                              ((eq system-type 'cygwin) "putclip")
                              ((eq system-type 'darwin) "pbcopy")
                              (t "xsel -ib")))))

(defun cppcm-readlines (FILE)
  "Return a list of lines of a file at FILE."
  (with-temp-buffer
    (insert-file-contents FILE)
    (split-string (buffer-string) "\n" t)))

(defun cppcm-parent-dir (d)
  (file-name-directory (directory-file-name d)))

(defun cppcm--query-var-from-lines (lines re)
  (if cppcm-debug (message "cppcm--query-var-from-lines called"))
  (let* (v)
    (catch 'brk
      (dolist (l lines)
        (when (string-match re l)
          (setq v (match-string 1 l))
          (throw 'brk t))))
    (if (string-match "^\"\\([^\"]+\\)\"$" v)
        (setq v (match-string 1 v)))
    v))

(defun cppcm-query-var (FILE re)
  "Return the value `set (var value)'"
  (cppcm--query-var-from-lines (cppcm-readlines FILE) re))

(defun cppcm-query-var-from-last-matched-line (f re)
  "Get the last matched line."
  (if cppcm-debug (message "cppcm-query-var-from-last-matched-line called"))
  (let* (vlist
         (lines (cppcm-readlines f)))
    (dolist (l lines)
      (if (string-match re l)
        (push (match-string 1 l) vlist)
        ))
    ;; Select the top-level directory assuming it is the one with shorter path
    (if vlist (car (sort vlist #'string-lessp)))))

;; get all the possible targets
(defun cppcm-query-targets (f)
  "return '((target value))"
  (let* (vars
         (lines (cppcm-readlines f)))
    (dolist (l lines)
      (if (string-match cppcm-cmake-target-regex l)
          (push (list (downcase (match-string 1 l)) (match-string 2 l)) vars)))
    vars))

(defun cppcm--alist-get (key alist)
  (let ((x (assq key alist)))
    (if x (cdr x))))

(defun cppcm-query-targets-from-json (f)
  (if cppcm-debug (message "cppcm-query-targets-from-json => %s" f))
  (let* (vars
         (json-array-type 'list)
         (lines (json-read-file f)))
    (dolist (l lines)
      (let* ((comm (cppcm--alist-get 'command l)))
        (if (string-match "-o \\([^ ]*?\.dir/\\)" comm)
            (push (list (cppcm--alist-get 'directory l) (match-string 1 comm)) vars))))
    vars))

;; get all the possible targets
;; @return matched line, use (match-string 2 line) to get results
(defun cppcm-match-all-lines (f)
  (let* (vars
         (lines (cppcm-readlines f)))
    (catch 'brk
      (dolist (l lines)
        (if (string-match cppcm-cmake-target-regex l)
            (push l vars))))
    vars))

(defun cppcm-query-match-line (f re)
  "return match line"
  (let* (ml
         (lines (cppcm-readlines f)))
    (if cppcm-debug (message "cppcm-query-match-line called => %s %s" f re))
    (catch 'brk
      (dolist (l lines)
        (when (string-match re l)
          (setq ml l)
          (throw 'brk t))))
    ml))

;; grep Project_SOURCE_DIR if it exists
;; if Project_SOURCE_DIR does not exist, grep first what_ever_SOURCE_DIR
;; the result is assume the root source directory,
;; kind of hack
;; Please enlighten me if you have better result
(defun cppcm-get-root-source-dir (d)
  (let* ((cmakecache (concat d "CMakeCache.txt"))
         (rlt (cppcm-query-var-from-last-matched-line cmakecache
                                                 "Project_SOURCE_DIR\:STATIC\=\\(.*\\)")))
    (unless rlt
      (setq rlt (cppcm-query-var-from-last-matched-line cmakecache
                                                        "[[:word:]]+_SOURCE_DIR\:STATIC\=\\(.*\\)")))
    rlt))

(defun cppcm-get-dirs ()
  "Search from current directory to the parent to locate build directory.
Return (found possible-build-dir build-dir src-dir)."
  (if cppcm-debug (message "cppcm-get-dirs called"))
  (let* ((crt-proj-dir (file-name-as-directory (file-name-directory buffer-file-name)))
         (i 0)
         found
         rlt
         build-dir
         src-dir
         possible-build-dir)
    (setq cppcm-build-dir nil)
    (setq cppcm-src-dir nil)
    (catch 'brk
      (while (and (< i cppcm-proj-max-dir-level) (not found) )
        (setq build-dir (concat crt-proj-dir (file-name-as-directory cppcm-build-dirname)))
        (cond
         ((and build-dir (file-exists-p (concat build-dir "CMakeCache.txt")))
          (setq found t)
          (setq cppcm-build-dir build-dir))
         (t ;not a real build directory,
          (if (file-exists-p build-dir)
              (setq possible-build-dir build-dir))
          ;; keep looking up the parent directory
          (setq crt-proj-dir (cppcm-parent-dir crt-proj-dir))))
        (setq i (+ i 1)))
      (when found
        (setq src-dir (cppcm-get-root-source-dir build-dir))
        (setq cppcm-src-dir src-dir)))
    (setq rlt (list found possible-build-dir build-dir src-dir))
    (if cppcm-debug (message "(cppcm-get-dirs)=%s" rlt))
    rlt))

(defun cppcm--contains-variable-name (VALUE start)
  (string-match "\$\{\\([^}]+\\)\}" VALUE start))

(defun cppcm--decompose-var-value (val)
  "Return the list by decomposing VAL."
  (let* (rlt (start 0) (non-var-idx 0) var-name)
    ;; (setq rlt (split-string val "\$\{\\|\}"))
    ;; charater scan might be better
    (while (numberp (setq start (cppcm--contains-variable-name val start)))
      (setq var-name (match-string 1 val))
      ;; move the index
      (when (< non-var-idx start)
        (add-to-list 'rlt (substring val non-var-idx start)t)
        (add-to-list 'rlt (make-symbol var-name) t))

      ;; "${".length + "}".length = 3
      (setq start (+ start 3 (length var-name)))
      (setq non-var-idx start))
    rlt))

(defun cppcm-guess-var (var lines)
  "Get the value of VAR from LINES."
  (let* (rlt
         (re (if (string= var "PROJECT_NAME") (concat "\s*project(\s*\\([^ ]+\\)\s*)")
               (concat "\s*set(\s*" var "\s+\\([^ ]+\\)\s*)")))

         ;; if rlt contains "${", we first de-compose the rlt into a list:
         ;; TODO else we just return the value
         (value (cppcm--query-var-from-lines lines re))
         mylist)
    (cond
     ((numberp (cppcm--contains-variable-name value 0))
      (setq mylist (cppcm--decompose-var-value value))
      (dolist (item mylist)
        (cond
         ((symbolp item)
          (setq rlt (concat rlt
                            (cppcm-guess-var (symbol-name item) lines))))
         (t
          (setq rlt (concat rlt item))))))
     (t
      (setq rlt value)))
    rlt))

(defun cppcm-strip-prefix (prefix str)
  "Strip PREFIX from STR."
  (if (string-equal (substring str 0 (length prefix)) prefix)
      (substring str (length prefix))
    str))

(defun cppcm--extract-include-directory (str)
  (when (string-match "^-I[ \t]*" str)
    (setq str (replace-regexp-in-string "^-I[ \t]*" "" str))
    (setq str (cppcm-trim-string str "\""))))

(defun cppcm-trim-string (string trim-str)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string (concat "^" trim-str)
                            ""
                            (replace-regexp-in-string (concat trim-str "$") "" string)))

(defun cppcm-trim-compiling-flags (cppflags)
  (if cppcm-debug (message "cppcm-trim-compiling-flags called => %s" cppflags))
  (let* (tks
         (next-tk-is-included-dir nil)
         (v ""))
    ;; consider following sample:
    ;; CXX_FLAGS = -I/Users/cb/wxWidgets-master/include -I"/Users/cb/projs/space nox"    -Wno-write-strings
    (when (and cppflags (not (string= cppflags "")))
      (setq tks (split-string (cppcm-trim-string cppflags "[ \t\n]*") "\s+-" t))
      (if cppcm-debug (message "tks=%s" tks))

      ;; rebuild the arguments in one string, append double quote string
      (dolist (tk tks v)
        (cond
         ;; add language standard support for flycheck, e.g., "std = c++11".
         ((and (> (length tk) 2) (string-match-p "std\s*=" tk))
          (setq-local flycheck-clang-language-standard (cppcm-trim-string tk "[-std\s=]*"))
          (setq-local flycheck-gcc-language-standard flycheck-clang-language-standard)
          )

         ((and (> (length tk) 1) (string= (substring tk 0 2) "-I"))
          (setq v (concat v " -I\"" (substring tk 2 (length tk)) "\"")))

         ((string= (substring tk 0 1) "I")
          (setq v (concat v " -I\"" (substring tk 1 (length tk)) "\"")))

         ((and (> (length tk) 8) (string= (substring tk 0 8) "isystem "))
          (setq v (concat v " -I\"" (substring tk 8 (length tk)) "\"")))

         ((and (> (length tk) 9) (string= (substring tk 0 9) "-isystem "))
          (setq v (concat v " -I\"" (substring tk 9 (length tk)) "\"")))
         )
        ))
    v))

(defun cppcm--find-physical-lib-file (base-exe-name)
  "A library binary file could have different file extension."
  (if cppcm-debug (message "cppcm--find-physical-lib-file called => %s" base-exe-name))
  (let* (p)
    (if base-exe-name
        (cond
         ((file-exists-p (concat base-exe-name ".a"))
          (setq p (concat base-exe-name ".a")))

         ((file-exists-p (concat base-exe-name ".so"))
          (setq p (concat base-exe-name ".so")))

         ((file-exists-p (concat base-exe-name ".dylib"))
          (setq p (concat base-exe-name ".dylib")))
         ))
    (if cppcm-debug (message "cppcm--find-physical-lib-file return =%s" p))
    p))

;; I don't consider the win32 environment because cmake support Visual Studio
;; @return full path of executable and we are sure it exists
(defun cppcm-guess-exe-full-path (exe-dir tgt)
  (if cppcm-debug (message "cppcm-proj-max-dir-level called => %s %s" exe-dir tgt))
  (let* (p
         (type (car tgt))
         (tgt-name (cadr tgt))
         (base-exe-name (concat exe-dir "lib" tgt-name)))

    (when cppcm-debug
      (message "cppcm-guess-exe-full-path: type=%s" type)
      (message "cppcm-guess-exe-full-path: tgt=%s" tgt)
      (message "cppcm-guess-exe-full-path: exe-dir=%s" exe-dir)
      (message "cppcm-guess-exe-full-path: cppcm-cmake-target-regex=%s" cppcm-cmake-target-regex)
      (message "cppcm-guess-exe-full-path: base-exe-name=%s" base-exe-name))

    (if (string-match "^\\(add_executable\\)$" type)
        ;; handle add_executable
        (progn
          ;; application bundle on OS X?
          (setq p (concat exe-dir tgt-name (if (eq system-type 'darwin) (concat ".app/Contents/MacOS/" tgt-name))))
          ;; maybe the guy on Mac prefer raw application? try again.
          (if (not (file-exists-p p))
              (setq p (concat exe-dir tgt-name)))
          (when (not (file-exists-p p))
            ;; Turn to the customer for help
            (when cppcm-get-executable-full-path-callback
              (if cppcm-debug (message "cppcm-get-executable-full-path-callback will be called! => %s %s %s" p type tgt-name))
              (setq p (funcall cppcm-get-executable-full-path-callback p type tgt-name))
              (when (not (file-exists-p p))
                (message "Executable %s missing! I give up!" p)
                (setq p nil)))
            ))

      ;; handle add_library
      (unless (setq p (cppcm--find-physical-lib-file base-exe-name))
        (when cppcm-get-executable-full-path-callback
          (if cppcm-debug (message "cppcm-get-executable-full-path-callback will be called! => %s %s %s" base-exe-name type tgt-name))
          (setq p (funcall cppcm-get-executable-full-path-callback
                           base-exe-name
                           type
                           tgt-name))
          (setq p (cppcm--find-physical-lib-file p)))))
    p))

(defun cppcm-get-exe-dir-path-current-buffer ()
  (file-name-directory (cppcm-get-exe-path-current-buffer)))

(defun cppcm-extract-info-from-flags-dot-make (flag-make hash-key)
  "Read FLAG-MAKE.  HASH-KEY could be whatever value.
If C project return C, or else return CXX."
  (let* (is-c
         (queried-c-flags (cppcm-query-match-line flag-make "\s*\\(CX\\{0,2\\}_FLAGS\\)\s*=\s*\\(.*\\)"))
         c-flags
         c-defines
         (c-flags-val (match-string 2 queried-c-flags))
         ;; cmake 3.4+
         (queried-c-includes (cppcm-query-match-line flag-make "\s*\\(CX\\{0,2\\}_INCLUDES\\)\s*=\s*\\(.*\\)"))
         (c-includes-val (match-string 2 queried-c-includes))
         queried-c-defines)

    (if cppcm-debug (message "cppcm-get-exe-dir-path-current-buffer called => %s %s" flag-make hash-key))

    (when (or queried-c-includes queried-c-flags)
      (setq is-c (if (or (cppcm-starts-with queried-c-flags "C_FLAGS")
                         (cppcm-starts-with queried-c-includes "C_INCLUDES"))
                     "C"
                   "CXX"))
      ;; cmake 3.4+ use CXX_INCLUDES and C_INCLUDES
      ;; merge flag and includes
      (setq c-flags (concat (cppcm-trim-compiling-flags c-flags-val)
                            " "
                            (cppcm-trim-compiling-flags c-includes-val)))

      (setq queried-c-defines (cppcm-query-match-line flag-make "\s*\\(CX\\{0,2\\}_DEFINES\\)\s*=\s*\\(.*\\)"))
      (when cppcm-debug
        (message "is-c=%s" is-c)
        (message "c-flags=%s" c-flags)
        (message "c-defines=%s" c-defines)
        (message "queried-c-flags=%s" queried-c-flags)
        (message "queried-c-defines=%s" queried-c-defines))

      ;; just what ever preprocess flag we got
      (setq c-defines (match-string 2 queried-c-defines))
      ;; make sure cppcm-hash exists
      (unless cppcm-hash
        (setq cppcm-hash (make-hash-table :test 'equal)))
      ;; store info into hash
      (puthash hash-key (list c-flags c-defines) cppcm-hash))
    is-c))

(defun cppcm-create-makefile-for-flymake (is-c flag-make src-dir)
  (if (and cppcm-write-flymake-makefile
           (file-exists-p flag-make))
      (let* ((mk (concat (file-name-as-directory src-dir) cppcm-makefile-name)))
        (if cppcm-debug (message "creating Makefile for flymake: %s" mk))
        (with-temp-file mk
          (insert (concat "# Generated by " cppcm-prog ".\n"
                          "include " flag-make "\n"
                          ".PHONY: check-syntax\ncheck-syntax:\n\t${"
                          (if (string= is-c "C") "CC" "CXX")
                          "} -o /dev/null ${"
                          is-c
                          "_FLAGS} ${"
                          is-c
                          "_DEFINES} ${"
                          is-c
                          "_INCLUDES} "
                          (mapconcat 'identity cppcm-extra-preprocss-flags-from-user " ")
                          " -S ${CHK_SOURCES}"))))))

(defun cppcm-handle-one-executable (root-src-dir build-dir src-dir tgt)
  "Find information for current executable. Then create Makefile for flymake.
Require the project be compiled successfully at least once."

  (if cppcm-debug (message "cppcm-handle-one-executable called => %s %s %s %s" root-src-dir build-dir src-dir tgt))

  (let* (flag-make
         (base-dir (cppcm--guess-dir-containing-cmakelists-dot-txt src-dir))
         (cm (cppcm--cmakelists-dot-txt base-dir))
         (exe-dir (concat (directory-file-name build-dir)
                          (cppcm-strip-prefix root-src-dir (file-name-directory cm))))
         (executable (cadr tgt))
         (flag-make (concat
                     exe-dir
                     "CMakeFiles/"
                     executable
                     ".dir/flags.make"))

         ;; try to guess the executable file full path
         (exe-full-path (cppcm-guess-exe-full-path exe-dir tgt)))

    (if cppcm-debug (message "flag-make=%s\n exe-full-path=%s" flag-make exe-full-path))
    (if exe-full-path
      (puthash (cppcm--exe-hashkey base-dir) exe-full-path cppcm-hash))

    ;; create makefile
    (cppcm-create-makefile-for-flymake (cppcm-extract-info-from-flags-dot-make flag-make (cppcm--flags-hashkey base-dir))
                                       flag-make
                                       src-dir)))

(defun cppcm-handle-executable (build-dir src-dir tgt)
  (if cppcm-debug (message "cppcm-handle-executable called => %s %s %s" build-dir src-dir tgt))
  (let* (flag-make
         (base-dir (cppcm--guess-dir-containing-cmakelists-dot-txt src-dir))
         (flag-make (concat
                     (car tgt)
                     "/"
                     (cadr tgt)
                     "flags.make")))
    (if cppcm-debug (message "flag-make=%s" flag-make))
    (cppcm-create-makefile-for-flymake (cppcm-extract-info-from-flags-dot-make flag-make (cppcm--flags-hashkey base-dir))
                                       flag-make
                                       src-dir)))

(defun cppcm-scan-info-from-cmake(root-src-dir src-dir build-dir)
  (if cppcm-debug (message "cppcm-scan-info-from-cmake called => %s %s %s" root-src-dir src-dir build-dir))
  (let* ((base src-dir)
         ;; search all the subdirectory for CMakeLists.txt
         (cm (cppcm--cmakelists-dot-txt src-dir))
         subdir
         possible-targets
         tgt
         e)

    (if cppcm-debug (message "CMakeLists.txt=%s" cm))
    ;; open CMakeLists.txt and find
    (when (file-exists-p cm)
      (setq possible-targets (cppcm-query-targets cm))
      (if cppcm-debug (message "possible-targets=%s" possible-targets))

      (dolist (tgt possible-targets)
        ;; if the target is ${VAR_NAME}, we need query CMakeLists.txt to find actual value
        ;; of the target
        (setq e (cadr tgt))
        (setq e (if (and (> (length e) 1) (string= (substring e 0 2) "${"))
                    (cppcm-guess-var (substring e 2 -1) (cppcm-readlines cm)) e))
        (setcar (nthcdr 1 tgt) e)
        (cppcm-handle-one-executable root-src-dir build-dir src-dir tgt)))

    (dolist (f (directory-files base))
      (setq subdir (concat (file-name-as-directory base) f))
      (if (and (file-directory-p subdir)
                 (not (equal f ".."))
                 (not (equal f "."))
                 (not (equal f ".git"))
                 (not (equal f cppcm-build-dirname))
                 (not (equal f ".svn"))
                 (not (equal f ".hg")))
        (cppcm-scan-info-from-cmake root-src-dir subdir build-dir)))))

(defun cppcm-scan-info-from-json (src-dir build-dir)
  (if cppcm-debug (message "cppcm-scan-info-from-json called => %s %s" src-dir build-dir))
  (let ((jf (concat build-dir "compile_commands.json"))
        possible-targets)
    (if cppcm-debug (message "compile_commands.json=%s" jf))
    (when (file-exists-p jf)
      (setq possible-targets (cppcm-query-targets-from-json jf))
      (if cppcm-debug (message "possible-targets=%s" possible-targets))
      (dolist (tgt possible-targets)
        (cppcm-handle-executable build-dir src-dir tgt)))))

(defun cppcm--guess-dir-containing-cmakelists-dot-txt (&optional src-dir)
  (if cppcm-debug (message "cppcm--guess-dir-containing-cmakelists-dot-txt called => %s" src-dir))
  (let* ((i 0)
         (dir (if src-dir src-dir
                (file-name-as-directory (file-name-directory buffer-file-name))))
         found)

    (while (and (< i cppcm-proj-max-dir-level) (not found) )
      (cond
       ((file-exists-p (cppcm--cmakelists-dot-txt dir))
        (setq found t))
       (t
        ;; keep looking up the parent directory
        (setq dir (cppcm-parent-dir dir))))
      (setq i (+ i 1)))
    (unless found (setq dir nil))
    (if cppcm-debug (message "cppcm--guess-dir-containing-cmakelists-dot-txt: dir=%s" dir))
    dir))

;;;###autoload
(defun cppcm-get-exe-path-current-buffer ()
  (interactive)
  (if cppcm-debug (message "cppcm-get-exe-path-current-buffer called"))
  (let* ((dir (cppcm--guess-dir-containing-cmakelists-dot-txt))
         (exe-path (if dir (gethash (cppcm--exe-hashkey dir) cppcm-hash))))
    ;; make sure the cppcm-hash is loaded
    (unless exe-path (cppcm-reload-all))
    (cond
     (exe-path
      (cppcm-share-str exe-path)
      (message "%s => clipboard" exe-path))
     (t
      (message "Executable missing! Please run cmake and make in shell manullay at first.")))
    exe-path))

(defun cppcm-set-c-flags-current-buffer ()
  (interactive)
  (let* ((dir (cppcm--guess-dir-containing-cmakelists-dot-txt))
         c-compiling-flags-list
         c-flags
         c-defines)

    (if cppcm-debug (message "cppcm-set-c-flags-current-buffer called"))

    (when dir
      (setq c-compiling-flags-list (gethash (cppcm--flags-hashkey dir) cppcm-hash))
      ;; please note the "-I" are always wrapped by double quotes
      ;; for example: -I"/usr/src/include"
      (if cppcm-debug (message "c-compiling-flags-list=%s" c-compiling-flags-list))

      (setq c-flags (nth 0 c-compiling-flags-list))
      (setq c-defines (nth 1 c-compiling-flags-list))

      (when c-flags
        (setq cppcm-include-dirs (split-string c-flags "\s+-I\"" t))
        (setq cppcm-include-dirs (delq nil
                                       (mapcar (lambda (str)
                                                 (when str
                                                   ;; clean the trailing double quotes and whitespace
                                                   (setq str (replace-regexp-in-string "[ \t\"]\+$" "" str))
                                                   ;; well, make sure the include is absolute path containing NO dot character
                                                   (concat "-I\"" (file-truename str) "\"")
                                                   ))
                                               cppcm-include-dirs))))

      ;; always add PWD into include directories, to make workflow continue
      ;; so cppcm-include-dirs could not be nil in most cased!
      (when (buffer-file-name)
        (let* ((crt-dir (file-name-directory (buffer-file-name))))
          (if (and crt-dir (not (member crt-dir cppcm-include-dirs)))
            (push (concat "-I\"" crt-dir "\"") cppcm-include-dirs))))

      (setq cppcm-preprocess-defines (if c-defines (split-string c-defines "\s+" t))))))

(defun cppcm--fix-include-path (l)
  (delq nil (mapcar (lambda (str)
                      (replace-regexp-in-string "\"" "" str))
                    l)))

(defun cppcm-compile-in-current-exe-dir ()
  "Compile the executable/library in current directory."
  (interactive)
  (setq-local compile-command (concat "make -C \"" (cppcm-get-exe-dir-path-current-buffer) "\""))
  (call-interactively 'compile))

(defun cppcm-compile-in-root-build-dir ()
  "Compile in build directory"
  (interactive)
  (setq-local compile-command (concat "make -C \"" cppcm-build-dir "\""))
  (call-interactively 'compile))

;;;###autoload
(defun cppcm-version ()
  (interactive)
  (message "0.5.7"))

;;;###autoload
(defun cppcm-compile (&optional prefix)
  "Compile the executable/library in current directory,
default compile command or compile in the build directory.
You can specify the sequence which compile is default
by customize `cppcm-compile-list'."
  (interactive "p")
  (if (and cppcm-build-dir
           (file-exists-p (concat cppcm-build-dir "CMakeCache.txt")))
      (let* ((index (round (log prefix 4))))
        (call-interactively (nth index cppcm-compile-list)))))

;;;###autoload
(defun cppcm-recompile ()
  "Run 'make clean && compile'."
  (interactive)
  (let* ((previous-compile-command compile-command)
         (recompile-command (concat compile-command " clean && " compile-command)))
    (compile recompile-command)
    (setq-local compile-command previous-compile-command)))

;;;###autoload
(defun cppcm-reload-all ()
  "Reload and reproduce everything."
  (interactive)
  (if cppcm-debug (message "cppcm-reload-all called"))
  (let* ((dirs (cppcm-get-dirs)))
    (when buffer-file-name
      (setq cppcm-hash (make-hash-table :test 'equal))
      (if cppcm-debug (message "(cppcm-get-dirs)=%s" dirs))
      (cond
       ((car dirs)
        ;; looks normal, we find buid-dir and soure-dir
        ;; create info for other plugins at first
        ;; cppcm-include-dirs will be set here
        ;; create makefiles may fail if the executable does not exist yet
        (condition-case nil
            (progn
              ;; the order is fixed
              (if cppcm-debug (message "file=%s" (concat (nth 2 dirs) "compile_command.json")))
              (if (file-exists-p (concat (nth 2 dirs) "compile_commands.json"))
                  (cppcm-scan-info-from-json (nth 3 dirs) (nth 2 dirs))
                (cppcm-scan-info-from-cmake (nth 3 dirs) (nth 3 dirs) (nth 2 dirs)))
              (cppcm-set-c-flags-current-buffer))
          (error
           (if cppcm-debug (message "Failed to get some information from scanning. Continue anyway.")))))
       ((nth 1 dirs)
        ;; build-dir is found, but flags in build-dir need be created
        ;; warn user.
        (message "Please run cmake and make in %s at first" (nth 1 dirs)))
       (t
        (message "Build directory is missing! Create the directory, run cmake and make inside of it.")))))

  (if cppcm-debug (message "cppcm-include-dirs=%s" cppcm-include-dirs))

  (when cppcm-include-dirs
    ;; for auto-complete-clang
    (setq-local ac-clang-flags (cppcm--fix-include-path (append cppcm-include-dirs
                                                                cppcm-preprocess-defines
                                                                cppcm-extra-preprocss-flags-from-user)))
    (if cppcm-debug (message "ac-clang-flags=%s" ac-clang-flags))

    (setq-local company-clang-arguments (cppcm--fix-include-path (append cppcm-include-dirs
                                                                         cppcm-preprocess-defines
                                                                         cppcm-extra-preprocss-flags-from-user)))
    (if cppcm-debug (message "company-clang-arguments=%s" company-clang-arguments))

    (if (fboundp 'semantic-add-system-include)
        (let* ((dirs (delq nil
                           (mapcar (lambda (str)
                                     (if (string-match "^-I *" str)
                                         (replace-regexp-in-string "^-I *" "" str)))
                                   ac-clang-flags))))
          (if cppcm-semantic-system-include
              (mapc 'semantic-remove-system-include cppcm-semantic-system-include))
          (mapc 'semantic-add-system-include dirs)
          (setq-local cppcm-semantic-system-include dirs)))

    ;; unlike auto-complete and company-mode, flycheck prefer make things complicated
    (setq-local flycheck-clang-include-path (delq nil
                                            (mapcar (lambda (str)
                                                      (cppcm--extract-include-directory str))
                                                    ac-clang-flags)))
    (if cppcm-debug (message "flycheck-clang-include-path=%s" flycheck-clang-include-path))

    (setq-local flycheck-clang-definitions (delq nil
                                           (mapcar (lambda (str)
                                                     (if (string-match "^-D *" str) (replace-regexp-in-string "^-D *" "" str)))
                                                   ac-clang-flags)))
    (if cppcm-debug (message "flycheck-clang-definitions=%s" flycheck-clang-definitions))

    ;; company-c-headers-path-system
    (setq-local company-c-headers-path-system flycheck-clang-include-path)
    (if cppcm-debug (message "company-c-headers-path-system=%s" company-c-headers-path-system))

    ;; irony compile-commands-path
    (if (and cppcm-src-dir
             (fboundp 'irony-cdb-json-add-compile-commands-path))
        (irony-cdb-json-add-compile-commands-path cppcm-src-dir (concat cppcm-build-dir "compile_commands.json")))

    ;; set cc-search-directories automatically, so ff-find-other-file will succeed
    (add-hook 'ff-pre-find-hook
              '(lambda ()
                 (let* ((inc-dirs (mapcar (lambda (item)
                                            (cppcm--extract-include-directory item))
                                          cppcm-include-dirs)))
                   ;; append the directories into the cc-search-directories
                   ;; please note add-to-list won't insert duplicated items
                   (dolist (x inc-dirs)
                     (add-to-list 'cc-search-directories x))))))
  (if (and cppcm-build-dir
           (file-exists-p (concat cppcm-build-dir "CMakeCache.txt")))
    (setq-local compile-command (concat "make -C \"" cppcm-build-dir "\"")))
  (run-hook-with-args 'cppcm-reload-all-hook))

(provide 'cpputils-cmake)
;;; cpputils-cmake.el ends here
