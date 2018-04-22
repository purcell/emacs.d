;;; cmake-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "cmake-mode" "cmake-mode.el" (23012 18219 231006
;;;;;;  483000))
;;; Generated autoloads from cmake-mode.el

(autoload 'cmake-mode "cmake-mode" "\
Major mode for editing CMake source files.

\(fn)" t nil)

(autoload 'cmake-command-run "cmake-mode" "\
Runs the command cmake with the arguments specified.  The
optional argument topic will be appended to the argument list.

\(fn TYPE &optional TOPIC BUFFER)" t nil)

(autoload 'cmake-help-list-commands "cmake-mode" "\
Prints out a list of the cmake commands.

\(fn)" t nil)

(autoload 'cmake-help-command "cmake-mode" "\
Prints out the help message for the command the cursor is on.

\(fn)" t nil)

(autoload 'cmake-help-module "cmake-mode" "\
Prints out the help message for the module the cursor is on.

\(fn)" t nil)

(autoload 'cmake-help-variable "cmake-mode" "\
Prints out the help message for the variable the cursor is on.

\(fn)" t nil)

(autoload 'cmake-help-property "cmake-mode" "\
Prints out the help message for the property the cursor is on.

\(fn)" t nil)

(autoload 'cmake-help "cmake-mode" "\
Queries for any of the four available help topics and prints out the approriate page.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))

(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; cmake-mode-autoloads.el ends here
