;;; cmake-ide.el --- Calls CMake to find out include paths and other compiler flags -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Atila Neves

;; Author:  Atila Neves <atila.neves@gmail.com>
;; Version: 0.6
;; Package-Version: 20170922.231
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5") (seq "1.11") (levenshtein "0") (s "1.11.0"))
;; Keywords: languages
;; URL: http://github.com/atilaneves/cmake-ide

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package runs CMake and sets variables for IDE-like functionality
;; provided by other packages such as:
;; On the fly syntax checks with flycheck
;; auto-completion using auto-complete-clang or company-clang
;; Jump to definition and refactoring with rtags
;; These other packages must be installed for the functionality to work

;;; Usage:

;; (cmake-ide-setup)
;;
;; If cmake-ide-flags-c or cmake-ide-flags-c++ are set, they will be added to ac-clang-flags.
;; These variables should be set. Particularly, they should contain the system include paths.
;;
;;; Code:

(require 'json)
(require 'find-file)
(require 'levenshtein)
(require 'cl-lib)
(require 'seq)
(require 's)

(declare-function rtags-call-rc "rtags")

(defcustom cmake-ide-flags-c
  nil
  "The C compiler flags to use.  Should have -I flags for system includes."
  :group 'cmake-ide
  :type 'string
  :safe #'stringp)

(defcustom cmake-ide-flags-c++
  nil
  "The C++ compiler flags to use.  Should have -I flags for system includes."
  :group 'cmake-ide
  :type 'string
  :safe #'stringp
  )

(defcustom cmake-ide-dir
  nil
  "The build directory to run CMake in.  If nil, runs in a temporary directory under cmake-ide-build-pool-dir.  DEPRECATED, use cmake-ide-build-dir instead."
  :group 'cmake-ide
  :type 'directory
  :safe #'stringp
  )

(defcustom cmake-ide-build-dir
  nil
  "The build directory to run CMake in.  If nil, runs in a temporary directory under cmake-ide-build-pool-dir."
  :group 'cmake-ide
  :type 'directory
  :safe #'stringp
  )

(defcustom cmake-ide-build-pool-dir
  nil
  "The parent directory for all automatically created build directories.  If nil, the system tmp-directory is used."
  :group 'cmake-ide
  :type 'directory
  :safe #'stringp
  )

(defcustom cmake-ide-build-pool-use-persistent-naming
  nil
  "Whether or not to use a persistent naming scheme for all automatically created build directories."
  :group 'cmake-ide
  :type 'booleanp
  :safe #'booleanp)

(defcustom cmake-ide-project-dir
  nil
  "The project directory."
  :group 'cmake-ide
  :type 'directory
  :safe #'stringp)

(defcustom cmake-ide-compile-command
  nil
  "The command to use to compile the project.  Can also include running tests."
  :group 'cmake-ide
  :safe #'stringp)

(defcustom cmake-ide-make-command
  "make --no-print-directory"
  "The command used to execute Makefile builds."
  :group 'cmake-ide
  :safe #'stringp)

(defcustom cmake-ide-ninja-command
  "ninja"
  "The command used to execute ninja type builds."
  :group 'cmake-ide
  :safe #'stringp)

(defcustom cmake-ide-cmake-command
  "cmake"
  "The command use to invoke cmake."
  :group 'cmake-ide
  :safe #'stringp)

(defcustom cmake-ide-cmake-opts
  "-DCMAKE_BUILD_TYPE=Release"
  "The options passed to cmake invocation."
  :group 'cmake-ide
  :safe #'stringp)

(defcustom cmake-ide-header-search-other-file
  t
  "Whether or not to search for a corresponding source file for headers when setting flags for them."
  :group 'cmake-ide
  :type 'booleanp
  :safe #'booleanp)

(defcustom cmake-ide-header-search-first-including
  t
  "Whether or not to search for the first source file to include a header when setting flags for them."
  :group 'cmake-ide
  :type 'booleanp
  :safe #'booleanp)

(defcustom cmake-ide-flycheck-cppcheck-strict-standards
  nil
  "Whether or not to be strict when setting cppcheck standards for flycheck.
If 't' or otherwise non-nil, the flycheck-cppcheck-standards
variable will only be set to contain standards that exactly match
those from the compile database.  (If there are none, it will not
be modified.)  If 'nil', standards will be gracefully degraded to
the closest possible matches available in cppcheck."
  :group 'cmake-ide
  :type 'booleanp
  :safe #'booleanp)

;;; The buffers to set variables for
(defvar cmake-ide--src-buffers nil)
(defvar cmake-ide--hdr-buffers nil)

(defcustom cmake-ide-rdm-executable
  "rdm"
  "Location of rdm executable."
  :group 'rtags
  :type 'file)

(defcustom cmake-ide-rdm-rc-path
  ""
  "Location of a custom rdm run control file."
  :group 'cmake-ide
  :type 'string
  :safe #'stringp)

(defcustom cmake-ide-src-extensions
  '(".c" ".cpp" ".C" ".cxx" ".cc")
  "A list of file extensions that qualify as source files."
  :group 'cmake-ide
  :type '(repeat string))

(defcustom cmake-ide-cmakelists-dir
  nil
  "The directory where the main CMakelists.txt is."
  :group 'cmake-ide
  :type 'file)

(defvar cmake-ide-try-unique-compiler-flags-for-headers
  nil
  "Whether or not to try all unique compiler flags for header files."
  )

(defvar cmake-ide--idbs
  (make-hash-table :test #'equal)
  "A cached map of build directories to IDE databases.")

(defvar cmake-ide--cdb-hash
  (make-hash-table :test #'equal)
  "The hash of the JSON CDB for each build directory.")

(defvar cmake-ide--cmake-hash
  (make-hash-table :test #'equal)
  "A hash to remember cmake build dirs.")

(defvar cmake-ide--irony
  (make-hash-table :test #'equal)
  "A hash to remember irony build dirs.")

(defvar cmake-ide--semantic-system-include)

(defconst cmake-ide-rdm-buffer-name "*rdm*" "The rdm buffer name.")

(defun cmake-ide--build-dir-var ()
  "Return the value of cmake-ide-build-dir or cmake-ide-dir."
  (or cmake-ide-build-dir cmake-ide-dir))

(defun cmake-ide--mode-hook()
  "Function to add to a major mode hook"
  (add-hook 'find-file-hook #'cmake-ide-maybe-run-cmake nil 'local)
  (cmake-ide-maybe-start-rdm))

;;;###autoload
(defun cmake-ide-setup ()
  "Set up the Emacs hooks for working with CMake projects."
  (add-hook 'c-mode-hook #'cmake-ide--mode-hook)
  (add-hook 'c++-mode-hook #'cmake-ide--mode-hook)

  ;; When creating a file in Emacs, run CMake again to pick it up
  (add-hook 'before-save-hook #'cmake-ide--before-save))

(defun cmake-ide--before-save ()
  "When creating a file in Emacs, run CMake again to pick it up."
  (when (and (cmake-ide--is-src-file (buffer-file-name))
             (not (file-readable-p (buffer-file-name))))
    (add-hook 'after-save-hook 'cmake-ide--new-file-saved nil 'local)))

(defun cmake-ide--new-file-saved ()
  "Run CMake to pick up newly created files."
  (cmake-ide-run-cmake)
  (remove-hook 'after-save-hook 'cmake-ide--new-file-saved 'local))

;;;###autoload
(defun cmake-ide-maybe-run-cmake ()
  "Run CMake if the compilation database JSON file is not found."
  (interactive)
  (cmake-ide-maybe-start-rdm)
  (if (cmake-ide--need-to-run-cmake)
      (cmake-ide-run-cmake)
    (progn
      (cmake-ide--add-file-to-buffer-list)
      (cmake-ide--on-cmake-finished))))

(defun cmake-ide--add-file-to-buffer-list ()
  "Add buffer to the appropriate list for when CMake finishes running."
  (if (cmake-ide--is-src-file buffer-file-name)
      (add-to-list 'cmake-ide--src-buffers (current-buffer))
    (add-to-list 'cmake-ide--hdr-buffers (current-buffer))))

(defun cmake-ide--comp-db-file-name ()
  "The name of the compilation database file."
  (expand-file-name "compile_commands.json" (cmake-ide--get-build-dir)))

(defun cmake-ide--need-to-run-cmake ()
  "If CMake needs to be run or not."
  (and (not (get-process "cmake")) ; don't run if already running
       (not (file-exists-p (cmake-ide--comp-db-file-name))))) ; no need if the file exists

;;;###autoload
(defun cmake-ide-run-cmake ()
  "Run CMake and set compiler flags for auto-completion and flycheck.
This works by calling cmake in a temporary directory (or cmake-ide-build-dir)
 and parsing the JSON file deposited there with the compiler
 flags."
  (interactive)
  (when (file-readable-p (buffer-file-name)) ; new files need not apply
    (let ((project-dir (cmake-ide--locate-cmakelists)))
      (when project-dir ; no point if it's not a CMake project
        ;; register this buffer to be either a header or source file
        ;; waiting for results
        (cmake-ide--add-file-to-buffer-list)
        (let ((cmake-dir (cmake-ide--get-build-dir)))
          (let ((default-directory cmake-dir))
            (cmake-ide--run-cmake-impl project-dir cmake-dir)
            (cmake-ide--register-callback)))))))


(defun cmake-ide--message (str &rest vars)
  "Output a message with STR and formatted by VARS."
  (message (apply #'format (concat "cmake-ide [%s]: " str) (cons (current-time-string) vars))))

(defun cmake-ide--register-callback ()
  "Register callback for when CMake finishes running."
  (set-process-sentinel (get-process "cmake")
                        (lambda (_process _event)
                          (cmake-ide--message "Finished running CMake")
                          (cmake-ide--on-cmake-finished))))

(defun cmake-ide--on-cmake-finished ()
  "Set compiler flags for all buffers that requested it."
  (let* ((idb (cmake-ide--cdb-json-file-to-idb))
         (set-flags (lambda (x) (cmake-ide--set-flags-for-file idb x))))
    (mapc set-flags cmake-ide--src-buffers)
    (mapc set-flags cmake-ide--hdr-buffers)
    (setq cmake-ide--src-buffers nil cmake-ide--hdr-buffers nil)
    (cmake-ide--run-rc)))


;;;###autoload
(defun cmake-ide-load-db ()
  "Load compilation DB and set flags for current buffer."
  (interactive)
  (cmake-ide--message "cmake-ide-load-db for file %s" (buffer-file-name))
  (cmake-ide-maybe-start-rdm)
  (let* ((file-name buffer-file-name)
         (buffers (list (current-buffer)))
         (cmake-ide--src-buffers (if (cmake-ide--is-src-file file-name) buffers nil))
         (cmake-ide--hdr-buffers (if (cmake-ide--is-src-file file-name) nil buffers)))
    (cmake-ide--on-cmake-finished)))

(defvar cmake-ide--rdm-executable nil
  "Rdm executable location path.")

(defun cmake-ide-rdm-executable ()
  "Return rdm executable location path."
  (cond (cmake-ide--rdm-executable cmake-ide--rdm-executable)
        ((file-exists-p cmake-ide-rdm-executable)
         (setq cmake-ide--rdm-executable cmake-ide-rdm-executable)
         cmake-ide--rdm-executable)
        ((featurep 'rtags)
         (setq cmake-ide--rdm-executable (rtags-executable-find "rdm"))
         cmake-ide--rdm-executable)
        (t "rdm")))


(defun cmake-ide--run-rc ()
  "Run rc to add definitions to the rtags daemon."
  (when (featurep 'rtags)
    (cmake-ide--message "Running rc for rtags")
    ;; change buffer so as to not insert text into a working file buffer
    (let ((cmake-ide-local-build-dir (cmake-ide--get-build-dir)))
      (if (get-process "rdm")
          (with-current-buffer (get-buffer cmake-ide-rdm-buffer-name)
            (rtags-call-rc "-J" cmake-ide-local-build-dir))
        (with-temp-buffer
          (rtags-call-rc "-J" cmake-ide-local-build-dir))))))


(defun cmake-ide--set-flags-for-file (idb buffer)
  "Set the compiler flags from IDB for BUFFER visiting file FILE-NAME."
  (let* ((file-name (buffer-file-name buffer))
         (file-params (cmake-ide--idb-file-to-obj idb file-name))
         (sys-includes (cmake-ide--params-to-sys-includes file-params))
         (commands (cmake-ide--idb-all-commands idb))
         (hdr-flags (cmake-ide--commands-to-hdr-flags commands)))
    (cmake-ide--message "Setting flags for file %s" file-name)
    ;; set flags for all source files that registered
    (if (cmake-ide--is-src-file file-name)
        (cmake-ide--set-flags-for-src-file file-params buffer sys-includes)
      (cmake-ide--set-flags-for-hdr-file idb buffer (cmake-ide--flags-to-sys-includes hdr-flags)))))

(defun cmake-ide--set-flags-for-src-file (file-params buffer sys-includes)
  "Set the compiler flags from FILE-PARAMS for source BUFFER with SYS-INCLUDES."
  (let* ((src-flags (cmake-ide--params-to-src-flags file-params))
         (src-includes (cmake-ide--params-to-src-includes file-params)))
    (cmake-ide-set-compiler-flags buffer src-flags src-includes sys-includes)))

(defun cmake-ide--set-flags-for-hdr-file (idb buffer sys-includes)
  "Set the compiler flags from IDB for header BUFFER with SYS-INCLUDES."
  (when (not (string-empty-p (cmake-ide--buffer-string buffer)))
    (cond
     ;; try all unique compiler flags until one successfully compiles the header
     (cmake-ide-try-unique-compiler-flags-for-headers (cmake-ide--hdr-try-unique-compiler-flags idb buffer sys-includes))
     ;; ask ninja or make depending on what the user chose for the flags to use on the header
     ((cmake-ide--hdr-ask-ninja-and-make idb buffer sys-includes) t)
     ;; the default algorithm used so far
     (t (cmake-ide--hdr-legacy idb buffer sys-includes)))))

(defun cmake-ide--buffer-string (buffer)
  "Return the contents of BUFFER as a string."
  (with-current-buffer buffer
    (buffer-string)))

(defun cmake-ide--hdr-try-unique-compiler-flags (idb buffer sys-includes)
  "Try all unique compiler flags in IDB in an attempt to find appropriate flags for header file in BUFFER using SYS-INCLUDES."
  (let ((hdr-flags) (hdr-includes))
    (setq hdr-flags (cmake-ide--idb-hdr-compiler-args idb (buffer-file-name buffer)))
    (setq hdr-flags (cmake-ide--remove-compiler-from-args-string hdr-flags))
    (setq hdr-includes (cmake-ide--flags-to-includes hdr-flags))
    (cmake-ide-set-compiler-flags buffer hdr-flags hdr-includes sys-includes)
    ))

(defun cmake-ide--hdr-ask-ninja-and-make (idb buffer sys-includes)
  "Try to get compiler flags from IDB from a source file that depends on the header BUFFER using SYS-INCLUDES."
  (let ((ninja-hdr-command (cmake-ide--ninja-header-command idb (buffer-file-name buffer))))
    (if ninja-hdr-command
        (progn
          (cmake-ide--set-flags-for-hdr-exact buffer sys-includes ninja-hdr-command)
          (cmake-ide--message "Setting flags for %s from ninja dependency information" (buffer-file-name buffer))
          t) ;; has done something
      nil)))

(defun cmake-ide--hdr-legacy (idb buffer sys-includes)
  "Try to set compiler flags from IDB for header BUFFER using SYS-INCLUDES.

First, try to find a source file corresponding to the header.
Then, try to find a source file in IDB that directly includes the header.
If all else fails, use all compiler flags in the project."

  (let* ((other (cmake-ide--src-file-for-hdr buffer))
         (src-file-name (or other (cmake-ide--first-including-src-file idb buffer))))
    (if src-file-name
        ;; if a source file is found, use its flags
        (cmake-ide--set-flags-for-hdr-from-src idb buffer sys-includes src-file-name)
      ;; otherwise use flags from all source files
      (cmake-ide--set-flags-for-hdr-from-all-flags idb buffer sys-includes))))

(defun cmake-ide--set-flags-for-hdr-exact (buffer sys-includes command)
  "Set flags for BUFFER using SYS-INCLUDES and compiler COMMAND."
  (let* ((hdr-flags (cmake-ide--remove-compiler-from-args-string command))
         (hdr-includes (cmake-ide--flags-to-includes hdr-flags)))
    (cmake-ide-set-compiler-flags buffer hdr-flags hdr-includes sys-includes)))

(defun cmake-ide--ninja-header-command (idb file-name)
  "Return the command used by a file in IDB that depends on FILE-NAME.

Find an object file that lists FILE-NAME as a dependency, then return the first
compiler command in the project that has that object file in itself."
  (let ((obj-file-name (cmake-ide--ninja-obj-file-depending-on-hdr file-name)))
    (if (null obj-file-name) nil
      (let ((commands (cmake-ide--idb-all-commands idb)))
        (cmake-ide--filter-first (lambda (x) (string-match obj-file-name x))
                                 commands)))))

(defun cmake-ide--ninja-obj-file-depending-on-hdr (file-name)
  "Find the first object file that depends on the header FILE-NAME.

Ask ninja for all dependencies then find FILE-NAME in the output, returning
the object file's name just above."
  (let ((default-directory (cmake-ide--get-build-dir))
        (beg)
        (end))
    (if (not (file-exists-p (expand-file-name "build.ninja" default-directory)))
        nil
      (with-temp-buffer
        (call-process cmake-ide-ninja-command nil t nil "-C" default-directory "-t" "deps")
        (goto-char (point-min))
        (setq beg (search-forward file-name nil t))
        (if (null beg)
            nil
          (search-backward "#deps")
          (setq beg (move-beginning-of-line nil))
          (setq end (1- (search-forward ":")))
          (copy-region-as-kill beg end)
          (car kill-ring))))))

(defun cmake-ide--src-file-for-hdr (buffer)
  "Try and find a source file for a header BUFFER (e.g. foo.cpp for foo.hpp)."
  (if cmake-ide-header-search-other-file
      (when (and buffer (buffer-live-p buffer))
        (with-current-buffer buffer
          (let ((other-file-name (ff-other-file-name)))
            (if other-file-name (expand-file-name other-file-name) nil))))
    nil))

(defun cmake-ide--set-flags-for-hdr-from-src (idb buffer sys-includes src-file-name)
  "Use IDB to set flags for a header BUFFER with SYS-INCLUDES from its corresponding SRC-FILE-NAME."
  (cmake-ide--message "Found src file %s for %s, using its flags" src-file-name (buffer-file-name buffer))
  (cmake-ide--set-flags-for-src-file (cmake-ide--idb-file-to-obj idb src-file-name) buffer sys-includes))

(defun cmake-ide--first-including-src-file (idb buffer)
  "Use IDB to find first source file that includes the header BUFFER."
  (when (and (buffer-file-name buffer) cmake-ide-header-search-first-including)
    (cmake-ide--message "Searching for source file including %s" (buffer-file-name buffer))
    (let* ((file-name (buffer-file-name buffer))
           ret-obj
           ret-file-name)

      (when (featurep 'rtags)
        (setq ret-file-name
              (with-temp-buffer
                (rtags-call-rc "--dependencies" file-name "included-by" :noerror t)
                (cmake-ide--filter-first
                 (lambda (a)
                   (gethash a idb))
                 (split-string (buffer-string) "\n" t split-string-default-separators)))))

      (unless ret-file-name
        (setq idb (cmake-ide--idb-sorted-by-file-distance idb file-name))
        (setq ret-obj (cmake-ide--filter-first
                       (lambda (x) (cmake-ide--idb-obj-depends-on-file x file-name))
                       idb))
        (when ret-obj (setq ret-file-name (cmake-ide--idb-obj-get ret-obj 'file))))

      (when ret-file-name (cmake-ide--message "Found a source file including %s" file-name))

      ret-file-name)))

(defun cmake-ide--get-string-from-file (path)
  "Return PATH's file content."
  (if (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string))
    ""))

(defun cmake-ide--set-flags-for-hdr-from-all-flags (idb buffer sys-includes)
  "Use IDB to set flags from a header BUFFER with SYS-INCLUDES from all project source files."
  (cmake-ide--message "Could not find suitable src file for %s, using all compiler flags" (buffer-file-name buffer))
  (let* ((commands (cmake-ide--idb-all-commands idb))
         (hdr-flags (cmake-ide--commands-to-hdr-flags commands))
         (hdr-includes (cmake-ide--commands-to-hdr-includes commands)))
    (cmake-ide-set-compiler-flags buffer hdr-flags hdr-includes sys-includes)))


(defun cmake-ide-set-compiler-flags (buffer flags includes sys-includes)
  "Set ac-clang and flycheck variables for BUFFER from FLAGS, INCLUDES and SYS-INCLUDES."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer

      (when (featurep 'auto-complete-clang)
        (make-local-variable 'ac-clang-flags)
        (setq ac-clang-flags (cmake-ide--filter-ac-flags (cmake-ide--get-compiler-flags flags))))

      (when (featurep 'company)
        (make-local-variable 'company-clang-arguments)
        (setq company-clang-arguments (cmake-ide--filter-ac-flags (cmake-ide--get-compiler-flags flags))))

      (when (featurep 'company-c-headers)
        (make-local-variable 'company-c-headers-path-user)
        (setq company-c-headers-path-user (cmake-ide--flags-to-include-paths flags))
        (make-local-variable 'company-c-headers-path-system)
        (when sys-includes
          (setq company-c-headers-path-system (append sys-includes company-c-headers-path-system))))

      (when (and (featurep 'irony) (not (gethash (cmake-ide--get-build-dir) cmake-ide--irony)))
        (irony-cdb-json-add-compile-commands-path (cmake-ide--locate-project-dir) (cmake-ide--comp-db-file-name))
        (puthash (cmake-ide--get-build-dir) t cmake-ide--irony))

      (when (featurep 'semantic)
        (let ((dirs (cmake-ide--flags-to-include-paths flags)))
          (when (boundp 'cmake-ide--semantic-system-include)
            (mapc 'semantic-remove-system-include cmake-ide--semantic-system-include))
          (mapc 'semantic-add-system-include dirs)
          (setq-local cmake-ide--semantic-system-include dirs)))


      (let ((macro-regex "\\(^-std=\\|\\.o$\\|^-o$\\)"))
        (make-local-variable 'c-macro-cppflags)
        (setq c-macro-cppflags
              (mapconcat 'identity (cmake-ide--filter (lambda (x) (not (string-match macro-regex x)))
                                                      (cmake-ide--filter-ac-flags (cmake-ide--get-compiler-flags flags))) " ")))

      (when (featurep 'flycheck)
        (make-local-variable 'flycheck-clang-include-path)
        (setq flycheck-clang-include-path (append sys-includes (cmake-ide--flags-to-include-paths flags)))

        (make-local-variable 'flycheck-clang-definitions)
        (setq flycheck-clang-definitions
              (append (cmake-ide--get-existing-definitions) (cmake-ide--flags-to-defines flags)))

        (let ((std-regex "^-std="))
          (make-local-variable 'flycheck-clang-args)
          (setq flycheck-clang-args (cmake-ide--filter (lambda (x) (not (string-match std-regex x))) (cmake-ide--flags-filtered flags)))

          (make-local-variable 'flycheck-clang-language-standard)
          (let* ((stds (cmake-ide--filter (lambda (x) (string-match std-regex x)) flags))
                 (repls (mapcar (lambda (x) (replace-regexp-in-string std-regex "" x)) stds)))
            (when repls
              (setq flycheck-clang-language-standard (car repls))
              (unless cmake-ide-flycheck-cppcheck-strict-standards
                (setq repls (mapcar 'cmake-ide--cmake-standard-to-cppcheck-standard repls)))
              (setq repls (cmake-ide--filter 'cmake-ide--valid-cppcheck-standard-p repls))
              (when repls
                (make-local-variable 'flycheck-cppcheck-standards)
                (setq flycheck-cppcheck-standards repls))))

          (make-local-variable 'flycheck-cppcheck-include-path)
          (setq flycheck-cppcheck-include-path (append sys-includes (cmake-ide--flags-to-include-paths flags))))

        (setq flycheck-clang-includes includes)
        (flycheck-clear)
        (run-at-time "0.5 sec" nil 'flycheck-buffer)
        ))))

(defun cmake-ide-delete-file ()
  "Remove file connected to current buffer and kill buffer, then run CMake."
  (interactive)
  (if (cmake-ide--get-build-dir)
      (let ((filename (buffer-file-name))
            (buffer (current-buffer))
            (name (buffer-name)))
        (if (not (and filename (file-exists-p filename)))
            (error "Buffer '%s' is not visiting a file!" name)
          (when (yes-or-no-p "Are you sure you want to remove this file? ")
            (delete-file filename)
            (kill-buffer buffer)
            (let ((project-dir (cmake-ide--locate-cmakelists)))
              (when project-dir (cmake-ide--run-cmake-impl project-dir (cmake-ide--get-build-dir)))
              (cmake-ide--message "File '%s' successfully removed" filename)))))
    (error "Not possible to delete a file without setting cmake-ide-build-dir")))


(defun cmake-ide--run-cmake-impl (project-dir cmake-dir)
  "Run the CMake process for PROJECT-DIR in CMAKE-DIR."
  (when project-dir
    (let ((default-directory cmake-dir))
      (cmake-ide--message "Running cmake for src path %s in build path %s" project-dir cmake-dir)
      (apply 'start-process (append (list "cmake" "*cmake*" cmake-ide-cmake-command)
                                    (split-string cmake-ide-cmake-opts)
                                    (list "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON" project-dir))))))


(defun cmake-ide--get-project-key ()
  "Return the directory name to run CMake in, it is the Project Key to store this directory in the hash map."
  (let ((build-parent-directory (or cmake-ide-build-pool-dir temporary-file-directory))
        build-directory-name)
    (setq build-directory-name
          (if cmake-ide-build-pool-use-persistent-naming
              (replace-regexp-in-string "[-/= ]" "_" (concat (expand-file-name (cmake-ide--locate-project-dir)) cmake-ide-cmake-opts))
            (make-temp-name "cmake")))
    (let ((build-dir (expand-file-name build-directory-name build-parent-directory)))
      (file-name-as-directory build-dir))))

(defun cmake-ide--get-build-dir-from-hash ()
  "Get dir form hash table, if not present compute a build dir and insert it in the table."
  (let ((project-key (cmake-ide--get-project-key)))
    (let ((build-dir (gethash project-key cmake-ide--cmake-hash nil)))
      (if (not build-dir)
          (let ((build-parent-directory (or cmake-ide-build-pool-dir temporary-file-directory))
                build-directory-name)
            (setq build-directory-name
                  (if cmake-ide-build-pool-use-persistent-naming
                      project-key
                    (make-temp-name "cmake")))
            (setq build-dir (expand-file-name build-directory-name build-parent-directory)
                  )
            (puthash project-key build-dir cmake-ide--cmake-hash)
            build-dir)
        build-dir))))


(defun cmake-ide--get-build-dir ()
  "Return the directory name to run CMake in."
  ;; build the directory key for the project
  (let ((build-dir (cmake-ide--build-dir-var)))
    (when (not build-dir)
      (setq build-dir (cmake-ide--get-build-dir-from-hash)))
    (when (not (file-accessible-directory-p build-dir))
      (cmake-ide--message "Making directory %s" build-dir)
      (make-directory build-dir))
    (setq cmake-ide-build-dir build-dir)
    (file-name-as-directory build-dir)))


(defun cmake-ide--is-src-file (name)
  "Test if NAME is a source file or not."
  (cl-some (lambda (x) (string-suffix-p x name)) cmake-ide-src-extensions))


(defun cmake-ide--filter (pred seq)
  "Apply PRED to filter SEQ."
  (delq nil
        (mapcar (lambda (x) (and (funcall pred x) x)) seq)))

(defun cmake-ide--filter-first (pred seq)
  "Return first element to satisfy PRED in SEQ."
  (let ((index 0) (ret))
    (while (and (null ret) (< index (length seq)))
      (when (funcall pred (elt seq index))
        (setq ret (elt seq index)))
      (cl-incf index))
    ret))


(defun cmake-ide--filter-params (file-params filter-func)
  "Filter FILE-PARAMS with FILTER-FUNC."
  ;; The compilation database is a JSON array of JSON objects
  ;; Each object is a file with directory, file and command fields
  ;; Depending on FILTER-FUNC, it maps file names to desired compiler flags
  ;; An example would be -I include flags
  (let* ((args (cmake-ide--file-params-to-args file-params))
         (flags (funcall filter-func args)))
    (mapconcat 'identity flags " ")))

(defun cmake-ide--file-params-to-args (file-params)
  "Get the compiler command arguments from FILE-PARAMS."
  (let ((command (cmake-ide--idb-obj-get file-params 'command))
        (arguments (cmake-ide--idb-obj-get file-params 'arguments)))
    (if command
        (split-string command " +")
      (cmake-ide--vector-to-list arguments))))

(defun cmake-ide--vector-to-list (vector)
  "Convert VECTOR to a list."
  (append vector nil))

(defun cmake-ide--args-to-only-flags (args)
  "Get compiler flags from ARGS."
  (cmake-ide--filter (lambda (x) (not (cmake-ide--is-src-file x))) args))

(defun cmake-ide--json-unescape (str)
  "Remove JSON-escaped backslashes in STR."
  (let* ((no-double-backslashes (replace-regexp-in-string "\\\\\\\\" "\\\\" str))
         (no-backslash-quote (replace-regexp-in-string "\\\\\"" "\"" no-double-backslashes)))
    no-backslash-quote))

(defun cmake-ide--params-to-src-flags (file-params &optional filter-func)
  "Source compiler flags for FILE-PARAMS using FILTER-FUNC."
  (if (not file-params) nil
    (let* ((filter-func (or filter-func #'cmake-ide--args-to-only-flags))
           (value (cmake-ide--filter-params file-params filter-func))
           (flags-string (if value value nil)))
      (if flags-string (cmake-ide--cleanup-flags-str flags-string) nil))))

(defun cmake-ide--cleanup-flags-str (str)
  "Clean up and filter STR to yield a list of compiler flags."
  (let ((unescaped-flags-string (cmake-ide--json-unescape str)))
    (cmake-ide--remove-compiler-from-args-string unescaped-flags-string)))

(defun cmake-ide--remove-compiler-from-args-string (str)
  "Remove the compiler command from STR, leaving only arguments."
  (let ((args (split-string str " +")))
    (cmake-ide--remove-compiler-from-args args)))

(defun cmake-ide--remove-compiler-from-args (args)
  "Remove the compiler command from ARGS, leaving only arguments."
  (if (string-suffix-p "ccache" (car args))
      (cddr args)
    (cdr args)))


(defun cmake-ide--filter-ac-flags (flags)
  "Filter unwanted compiler arguments out from FLAGS."
  (cmake-ide--filter
   (lambda (x)
     (cl-loop for flag in '("-m32" "-Werror" "-c" "-fPIC" "-pipe" "-g" "-ggdb")
              never (string-match (format "^%s$" flag) x)))
   flags))

(defun cmake-ide--delete-dup-hdr-flags (flags)
  "Delete duplicates in FLAGS for header files."
  (let* ((rest (cmake-ide--flags-filtered flags))
         (dashes (cmake-ide--filter #'cmake-ide--dash-i-or-dash-d-p flags)))
    (append (delete-dups dashes) rest)))

(defun cmake-ide--commands-to-hdr-flags (commands)
  "Header compiler flags from COMMANDS."
  (let* ((args (cmake-ide--flatten (mapcar #'cmake-ide--remove-compiler-from-args-string commands)))
         (flags (cmake-ide--args-to-only-flags args)))
    (setq flags (cmake-ide--filter (lambda (x) (not (equal x "-o"))) flags))
    (setq flags (cmake-ide--filter (lambda (x) (not (string-suffix-p ".o" x))) flags))
    (setq flags (cmake-ide--filter (lambda (x) (not (string-suffix-p ".obj" x))) flags))
    (cmake-ide--delete-dup-hdr-flags flags)))

(defun cmake-ide--params-to-src-includes (file-params)
  "-include compiler flags for from FILE-PARAMS."
  (cmake-ide--flags-to-includes (cmake-ide--params-to-src-flags file-params 'identity)))


(defun cmake-ide--params-to-sys-includes (file-params)
  "-include compiler flags for from FILE-PARAMS."
  (cmake-ide--flags-to-sys-includes (cmake-ide--params-to-src-flags file-params 'identity)))


(defun cmake-ide--commands-to-hdr-includes (commands)
  "Header `-include` flags from COMMANDS."
  (let ((args (cmake-ide--flatten (mapcar #'cmake-ide--remove-compiler-from-args-string commands))))
    (delete-dups (cmake-ide--flags-to-includes args))))


(defun cmake-ide--flatten (lst)
  "Flatten LST."
  (apply 'append lst))


(defun cmake-ide--flags-to-include-paths (flags)
  "From FLAGS (a list of flags) to a list of include paths."
  (let ((raw-paths (cmake-ide--to-simple-flags flags "^-I")))
    (mapcar (lambda (x) (expand-file-name x (cmake-ide--get-build-dir))) raw-paths)))

(defun cmake-ide--relativize (path)
  "Make PATH relative to the build directory, but only if relative path with dots."
  (if (or (equal path ".") (string-prefix-p ".." path))
      (expand-file-name path (cmake-ide--get-build-dir))
    path))


(defun cmake-ide--flags-to-defines (flags)
  "From FLAGS (a list of flags) to a list of defines."
  (cmake-ide--to-simple-flags flags "^-D"))


(defun cmake-ide--flags-to-includes (flags)
  "From FLAGS (a list of flags) to a list of includes."
  (let ((includes nil))
    (while (member "-include" flags)
      (setq flags (cdr (member "-include" flags)))
      (when flags (setq includes (cons (car flags) includes))))
    includes))

(defun cmake-ide--flags-to-sys-includes (flags)
  "From FLAGS (a list of flags) to a list of isystem includes."
  (let ((sysincludes nil))
    (while (member "-isystem" flags)
      (setq flags (cdr (member "-isystem" flags)))
      (when flags
        (if (member (car flags) sysincludes)
            nil
          (setq sysincludes (cons (car flags) sysincludes)))))
    sysincludes))


(defun cmake-ide--dash-i-or-dash-d-p (flag)
  "If FLAG is -I or -D."
  (let* ((case-fold-search nil)
         (imatch (string-match "^-I" flag))
         (dmatch (string-match "^-D" flag)))
    (or imatch dmatch)))

(defun cmake-ide--flags-filtered (flags)
  "Filter out defines and includes from FLAGS."
  (cmake-ide--filter (lambda (x) (not (cmake-ide--dash-i-or-dash-d-p x))) flags))


(defun cmake-ide--to-simple-flags (flags flag)
  "A list of either directories or defines from FLAGS depending on FLAG."
  (let* ((case-fold-search nil)
         (res-flags (cmake-ide--filter
                     (lambda (x)
                       (let ((match (string-match flag x)))
                         (and match (zerop match))))
                     flags)))
    (mapcar (lambda (x) (replace-regexp-in-string flag "" x)) res-flags)))


(defun cmake-ide--get-compiler-flags (flags)
  "Use FLAGS to return all compiler flags including existing ones."
  (append (cmake-ide--get-existing-compiler-flags) flags))

(defun cmake-ide--get-existing-compiler-flags ()
  "Return existing ac-clang flags for this mode, if set."
  (if (eq major-mode 'c++-mode)
      (cmake-ide--symbol-value 'cmake-ide-flags-c++)
    (cmake-ide--symbol-value 'cmake-ide-flags-c)))

(defun cmake-ide--get-existing-definitions ()
  "Return existing compiler defines, if set."
  (cmake-ide--symbol-value 'cmake-ide-definitions))


(defun cmake-ide--symbol-value (sym)
  "Return the value of SYM if bound, nil if not."
  (if (boundp sym) (symbol-value sym) nil))


(defun cmake-ide--locate-cmakelists ()
  "Find the topmost CMakeLists.txt file."
  (if cmake-ide-cmakelists-dir
      (expand-file-name "CMakeLists.txt" cmake-ide-cmakelists-dir)
    (cmake-ide--locate-cmakelists-impl default-directory nil)))

(defun cmake-ide--locate-cmakelists-impl (dir last-found)
  "Find the topmost CMakeLists.txt from DIR using LAST-FOUND as a 'plan B'."
  (let ((new-dir (locate-dominating-file dir "CMakeLists.txt")))
    (if new-dir
        (cmake-ide--locate-cmakelists-impl (expand-file-name ".." new-dir) new-dir)
      last-found)))

(defun cmake-ide--locate-project-dir ()
  "Return the path to the project directory."
  (let ((cmakelists (cmake-ide--locate-cmakelists)))
    (or (and cmake-ide-project-dir (expand-file-name cmake-ide-project-dir))
        (and cmakelists (file-name-directory cmakelists)))))


(defun cmake-ide--cdb-json-file-to-idb ()
  "Retrieve a JSON object from the compilation database."
  ;; check the cache first
  (let ((idb (cmake-ide--cdb-idb-from-cache)))
    (unless idb
      (if (not (file-exists-p (cmake-ide--comp-db-file-name)))
          (cmake-ide--message "Non-existent compilation DB file %s" (cmake-ide--comp-db-file-name))
        (progn
          (cmake-ide--message "Converting JSON CDB %s to IDB" (cmake-ide--comp-db-file-name))
          (setq idb (cmake-ide--cdb-json-string-to-idb (cmake-ide--get-string-from-file (cmake-ide--comp-db-file-name))))
          (puthash (cmake-ide--get-build-dir) idb cmake-ide--idbs)
          (puthash (cmake-ide--get-build-dir) (cmake-ide--hash-file (cmake-ide--comp-db-file-name)) cmake-ide--cdb-hash)
          (remhash (cmake-ide--get-build-dir) cmake-ide--irony))))
    idb))

(defun cmake-ide--cdb-idb-from-cache ()
  "Return the IDB from the cache unless the JSON CDB has changed."
  (let ((idb (gethash (cmake-ide--get-build-dir) cmake-ide--idbs))
        (cached-hash (gethash (cmake-ide--get-build-dir) cmake-ide--cdb-hash))
        (current-hash (cmake-ide--hash-file (cmake-ide--comp-db-file-name))))
    (if (equal cached-hash current-hash)
        idb
      nil)))

(defun cmake-ide--hash-file (file-name)
  "Calculate the hash of FILE-NAME."
  (secure-hash 'md5 (cmake-ide--get-string-from-file file-name)))

(defun cmake-ide--cdb-json-string-to-idb (json-str)
  "Tranform JSON-STR into an opaque json object."
  (let ((idb (make-hash-table :test #'equal))
        (json (json-read-from-string json-str)))
    (mapc (lambda (obj)
            (let* ((file (cmake-ide--relativize (cmake-ide--idb-obj-get obj 'file)))
                   (objs (gethash file idb)))
              (push obj objs)
              (puthash file objs idb)))
          json)
    idb))

(defun cmake-ide--idb-obj-get (obj key)
  "Get the value in OBJ for KEY."
  (cdr (assoc key obj)))

(defmacro cmake-ide--idb-obj-set (obj key value)
  "Take OBJ and set KEY to VALUE."
  `(push (cons ,key ,value) ,obj))

(defun cmake-ide--idb-file-to-obj (idb file-name)
  "Get object from IDB for FILE-NAME."
  (car (gethash file-name idb)))

(defun cmake-ide--idb-all-commands (idb)
  "A list of all commands in IDB."
  (mapcar (lambda (x) (s-join " " (cmake-ide--file-params-to-args x))) (cmake-ide--idb-all-objs idb)))

(defun cmake-ide--idb-sorted-by-file-distance (idb file-name)
  "Return a list of IDB entries sorted by their directory's name's distance to FILE-NAME."
  (let ((dir (file-name-directory file-name))
        (ret))

    (defun distance (object)
      (levenshtein-distance dir (file-name-directory (cmake-ide--idb-obj-get object 'file))))

    (setq ret (mapcar (lambda (x) (push `(distance . ,(distance x)) x)) (cmake-ide--idb-all-objs idb)))

    (seq-sort
     (lambda (x y) (< (cmake-ide--idb-obj-get x 'distance)
                      (cmake-ide--idb-obj-get y 'distance)))
     ret)))

(defun cmake-ide--idb-all-objs (idb)
  "Return a list of IDB entries."
  (let ((ret))
    (maphash (lambda (_ objs) (setq ret (append ret objs))) idb)
    ret))


(defun cmake-ide--idb-obj-depends-on-file (obj file-name)
  "If OBJ is a source file that depends on FILE-NAME."
  (let* ((base-name (file-name-nondirectory file-name))
         (src-file-name (cmake-ide--idb-obj-get obj 'file)))
    (if (string-match (concat "# *include +[\"<] *" base-name)
                      (cmake-ide--get-string-from-file src-file-name))
        src-file-name
      nil)))

(defun cmake-ide--idb-hdr-compiler-args (idb file-name)
  "Try every unique compiler command in IDB on FILE-NAME and return the first to succeed."
  (let* ((objects  (cmake-ide--idb-sorted-by-file-distance idb file-name))
         (commands (cmake-ide--idb-objs-to-unique-commands objects))
         (index 0)
         (ret))
    (while (and (null ret) (< index (length commands)))
      (let* ((tmp-file-name (expand-file-name "tmp.o" (make-temp-file "tryheader" t)))
             (command (concat (elt commands index) " " file-name " " "-o" " " tmp-file-name))
             (_ (cmake-ide--message "Trying to compile '%s' with '%s'" file-name command))
             (args (split-string command " +")))
        (when (eq 0 (apply #'call-process (car args) nil nil nil (cdr args)))
          (setq ret command)))
      (cl-incf index))
    ret))


(defun cmake-ide--idb-unique-compiler-commands (idb)
  "Calculate the list of unique compiler commands in IDB ignoring the source file name."
  (let ((objects) (ret))
    (maphash (lambda (_ v) (push v objects)) idb)
    (setq ret (cmake-ide--idb-objs-to-unique-commands objects))
    ret))

(defun cmake-ide--idb-objs-to-unique-commands (objects)
  "Calculate the list of unique compiler commands in OBJECTS ignoring the source file name."
  (let ((ret (mapcar (lambda (x)
                       (let* ((file (cmake-ide--idb-obj-get x 'file))
                              (base-name (file-name-nondirectory file))
                              (command (cmake-ide--idb-obj-get x 'command))
                              (args (split-string command " +")))
                         (setq args (cmake-ide--filter (lambda (x) (not (string-match base-name x))) args))
                         (setq args (cmake-ide--filter (lambda (x) (not (equal x "-c"))) args))
                         (setq args (cmake-ide--filter (lambda (x) (not (equal x "-o"))) args))
                         (mapconcat 'identity args " ")))
                     objects)))
    (delete-dups ret)
    ret))


;;;###autoload
(defun cmake-ide-compile ()
  "Compile the project."
  (interactive)
  (if (cmake-ide--get-build-dir)
      (let ((command-for-compile (cmake-ide--get-compile-command (cmake-ide--get-build-dir))))
        (if (functionp command-for-compile)
            (funcall command-for-compile)
          (compile command-for-compile)))
    (let ((command (read-from-minibuffer "Compiler command: " compile-command)))
      (compile command)))
  (cmake-ide--run-rc))


(defun cmake-ide--get-compile-command (dir)
  "Return the compile command to use for DIR."
  (cond (cmake-ide-compile-command cmake-ide-compile-command)
        ((file-exists-p (expand-file-name "build.ninja" dir)) (concat cmake-ide-ninja-command " -C " dir))
        ((file-exists-p (expand-file-name "Makefile" dir)) (concat cmake-ide-make-command " -C " dir))
        (t nil)))


;;;###autoload
(defun cmake-ide-maybe-start-rdm ()
  "Start the rdm (rtags) server."
  (interactive)
  (when (and (featurep 'rtags)
             (or (file-exists-p (cmake-ide--comp-db-file-name))
                 (cmake-ide--locate-cmakelists)))

    (unless (cmake-ide--process-running-p "rdm")
      (let ((buf (get-buffer-create cmake-ide-rdm-buffer-name)))
        (cmake-ide--message "Starting rdm server")
        (with-current-buffer buf (start-process "rdm" (current-buffer)
                                                (cmake-ide-rdm-executable)
                                                "-c" cmake-ide-rdm-rc-path))))))

(defun cmake-ide--process-running-p (name)
  "If a process called NAME is running or not."
  (or (get-process name) (cmake-ide--system-process-running-p name)))

(defun cmake-ide--system-process-running-p (name)
  "If a process called NAME is running on the system."
  (let* ((all-args (mapcar (lambda (x) (cdr (assq 'args (process-attributes x)))) (list-system-processes)))
         (match-args (cmake-ide--filter (lambda (x) (cmake-ide--string-match (concat "\\b" name "\\b") x)) all-args))
         )
    (not (null match-args))))

(defun cmake-ide--string-match (regexp name)
  "Wrap 'string-match' of REGEXP and NAME to make sure we don't pass it a nil string."
  (when name
    (string-match regexp name)))

(defun cmake-ide--valid-cppcheck-standard-p (standard)
  "If STANDARD is supported by cppcheck."
  (member standard '("posix" "c89" "c99" "c11" "c++03" "c++11")))

(defun cmake-ide--cmake-standard-to-cppcheck-standard (standard)
  "Convert a CMake language STANDARD to the closest supported by cppcheck.
If there is no clear and sensible conversion, the input is
returned unchanged."
  (let ((gnu-replaced (replace-regexp-in-string "gnu" "c" standard)))
    (cond
     ;; Convert "close-enough" matches.
     ((equal gnu-replaced "c90") "c89")
     ((equal gnu-replaced "c++98") "c++03")
     ((equal gnu-replaced "c++0x") "c++03")
     ((equal gnu-replaced "c++14") "c++11")
     ((equal gnu-replaced "c++1y") "c++11")
     ((equal gnu-replaced "c++17") "c++11")
     ((equal gnu-replaced "c++1z") "c++11")
     ;; See if what we have matches cppcheck's capabilities exactly.
     ((cmake-ide--valid-cppcheck-standard-p gnu-replaced) gnu-replaced)
     ;; Otherwise, just hand back the original input.
     (t standard))))

(provide 'cmake-ide)
;;; cmake-ide.el ends here
