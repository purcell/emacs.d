;;; elm-interactive.el --- Run an interactive Elm session. -*- lexical-binding: t -*-

;; Copyright (C) 2015, 2016  Bogdan Popa

;; Author: Bogdan Popa
;; URL: https://github.com/jcollard/elm-mode

;; This file is not part of GNU Emacs.

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

;;; Commentary:
;;; Code:
(require 'ansi-color)
(require 'comint)
(require 'compile)
(require 'cl-lib)
(require 'elm-font-lock)
(require 'elm-util)
(require 'f)
(require 'json)
(require 'let-alist)
(require 's)
(require 'subr-x)
(require 'tabulated-list)
(require 'url)

(defvar elm-interactive--seen-prompt nil
  "Non-nil represents the fact that a prompt has been spotted.")
(make-variable-buffer-local 'elm-interactive--seen-prompt)

(defvar elm-interactive--current-project nil)
(defvar elm-interactive--process-name "elm")
(defvar elm-interactive--buffer-name "*elm*")
(defvar elm-reactor--process-name "elm-reactor")
(defvar elm-reactor--buffer-name "*elm-reactor*")

(defcustom elm-interactive-command "elm-repl"
  "The Elm REPL command."
  :type '(string)
  :group 'elm)

(defcustom elm-interactive-arguments '()
  "Command line arguments to pass to the Elm REPL command."
  :type '(repeat string)
  :group 'elm)

(defvar elm-interactive-prompt-regexp "^[>|] "
  "Prompt for `run-elm-interactive'.")

(defcustom elm-reactor-command "elm-reactor"
  "The Elm Reactor command."
  :type '(string)
  :group 'elm)

(defcustom elm-reactor-port "8000"
  "The Elm Reactor port."
  :type '(string)
  :group 'elm)

(defcustom elm-reactor-address "127.0.0.1"
  "The Elm Reactor address."
  :type '(string)
  :group 'elm)

(defcustom elm-reactor-arguments `("-p" ,elm-reactor-port "-a" ,elm-reactor-address)
  "Command line arguments to pass to the Elm Reactor command."
  :type '(repeat string)
  :group 'elm)

(defvar elm-compile--buffer-name "*elm-make*")

(defcustom elm-compile-command "elm-make"
  "The Elm compilation command."
  :type '(string)
  :group 'elm)

(defcustom elm-compile-arguments '("--yes" "--warn" "--output=elm.js")
  "Command line arguments to pass to the Elm compilation command."
  :type '(repeat string)
  :group 'elm)

(defvar elm-compile-error-regexp-alist-alist
  '((elm-file "-- [^-]+ -+ \\(.+\\)$" 1 nil)
    (elm-line "^\\([0-9]+\\)|" nil 1))
  "Regexps to match Elm compiler errors in compilation buffer.")

(defvar elm-compile-error-regexp-alist '(elm-line elm-file))

(dolist (alist elm-compile-error-regexp-alist-alist)
  (add-to-list 'compilation-error-regexp-alist-alist alist))

(dolist (symbol elm-compile-error-regexp-alist)
  (add-to-list 'compilation-error-regexp-alist symbol))

(defcustom elm-create-package-command "elm make --yes"
  "The command that is used to initialize a new package definition."
  :type '(string)
  :group 'elm)

(defvar elm-package--contents nil
  "The contents of the Elm package catalog.")

(defvar elm-package--dependencies nil
  "The package dependencies for the current Elm package.")

(defvar elm-package--cache nil
  "A cache for extended package information.")

(defvar elm-package--marked-contents nil)

(defvar elm-package--working-dir nil)

(defvar elm-package-compile-buffer-name "*elm-package-compile*")

(defvar elm-package-buffer-name "*elm-package*")

(defcustom elm-package-command "elm-package"
  "The Elm package command."
  :type '(string)
  :group 'elm)

(defcustom elm-package-arguments '("install" "--yes")
  "Command line arguments to pass to the Elm package command."
  :type '(repeat string)
  :group 'elm)

(defcustom elm-package-catalog-root
  "http://package.elm-lang.org/"
  "The root URI for the Elm package catalog."
  :type '(string)
  :group 'elm)

(defvar elm-package-catalog-format
  [(" " 1 nil)
   ("Name" 30 t)
   ("Version" 7 nil)
   ("Status" 10 t)
   ("Summary" 80 nil)]
  "The format of the package list header.")

(defvar elm-interactive-mode-map
  (let ((map (make-keymap)))
    (define-key map "\t" #'completion-at-point)
    (define-key map (kbd "C-a") #'elm-interactive-mode-beginning)
    (define-key map (kbd "C-c C-z") #'elm-repl-return-to-origin)
    map)
  "Keymap for Elm interactive mode.")

(defcustom elm-oracle-command "elm-oracle"
  "The Elm Oracle command."
  :type '(string)
  :group 'elm)

(defconst elm-oracle--pattern
  "\\(?:[^A-Za-z0-9_.']\\)\\(\\(?:[A-Za-z_][A-Za-z0-9_']*[.]\\)?[A-Za-z0-9_']*\\)"
  "The prefix pattern used for completion.")

(defvar elm-oracle--completion-cache (make-hash-table :test #'equal)
  "A cache for Oracle-based completions by prefix.")

(defvar elm-oracle--eldoc-cache (make-hash-table :test #'equal)
  "A cache for Eldoc completions.")

(defvar elm-repl--origin nil
  "Marker for buffer/position from which we jumped to this repl.")

(defcustom elm-sort-imports-on-save nil
  "Controls whether or not imports should be automaticaly reordered on save."
  :type 'boolean
  :group 'elm)

(defvar elm-package-mode-map
  (let ((map (make-keymap)))
    (define-key map "g" #'elm-package-refresh)
    (define-key map "n" #'elm-package-next)
    (define-key map "p" #'elm-package-prev)
    (define-key map "v" #'elm-package-view)
    (define-key map "m" #'elm-package-mark)
    (define-key map "i" #'elm-package-mark)
    (define-key map "u" #'elm-package-unmark)
    (define-key map "x" #'elm-package-install)
    map)
  "Keymap for Elm package mode.")

(defun elm-interactive-mode-beginning ()
  "Go to the start of the line."
  (interactive)
  (beginning-of-line)
  (goto-char (+ 2 (point))))

(defun elm-interactive--get-process-buffer ()
  "Get the REPL process buffer."
  (get-buffer-process elm-interactive--buffer-name))

(defun elm-interactive--spot-prompt (_string)
  "Spot the prompt, _STRING is ignored."
  (let ((proc (elm-interactive--get-process-buffer)))
    (when proc
      (save-excursion
        (goto-char (process-mark proc))
        (if (re-search-backward comint-prompt-regexp
                                (line-beginning-position) t)
            (setq elm-interactive--seen-prompt t))))))

(defun elm-interactive--wait-for-prompt (proc &optional timeout)
  "Wait until PROC sends us a prompt or TIMEOUT.
The process PROC should be associated to a comint buffer.

Stolen from haskell-mode."
  (with-current-buffer (process-buffer proc)
    (while (progn
             (goto-char comint-last-input-end)
             (not (or elm-interactive--seen-prompt
                      (setq elm-interactive--seen-prompt
                            (re-search-forward comint-prompt-regexp nil t))
                      (not (accept-process-output proc timeout))))))
    (unless elm-interactive--seen-prompt
      (error "Can't find the prompt"))))

(defun elm-interactive--send-command (command)
  "Send a COMMAND to the REPL."
  (let ((proc (elm-interactive--get-process-buffer)))
    (with-current-buffer (process-buffer proc)
      (elm-interactive--wait-for-prompt proc 10)
      (goto-char (process-mark proc))
      (insert-before-markers command)
      (move-marker comint-last-input-end (point))
      (setq elm-interactive--seen-prompt nil)
      (comint-send-string proc command))))

(defun elm-interactive-kill-current-session ()
  "Stop the current REPL session and delete its buffer."
  (interactive)
  (when (and elm-interactive--current-project
             (get-buffer-process elm-interactive--buffer-name)
             (not (equal elm-interactive--current-project (elm--find-dependency-file-path)))
             (y-or-n-p "This will kill your existing REPL session.  Continue? "))
    (delete-process elm-interactive--buffer-name)
    (kill-buffer elm-interactive--buffer-name)))

;;;###autoload
(define-derived-mode elm-interactive-mode comint-mode "Elm Interactive"
  "Major mode for `run-elm-interactive'.

\\{elm-interactive-mode-map}"

  (setq-local comint-prompt-regexp elm-interactive-prompt-regexp)
  (setq-local comint-prompt-read-only t)
  (setq-local comint-use-prompt-regexp t)

  (add-hook 'comint-output-filter-functions #'elm-interactive--spot-prompt nil t)

  (turn-on-elm-font-lock))

;;;###autoload
(defun run-elm-interactive ()
  "Run an inferior instance of `elm-repl' inside Emacs."
  (interactive)
  (elm-interactive-kill-current-session)
  (let* ((default-directory (elm--find-dependency-file-path))
         (buffer (comint-check-proc elm-interactive--process-name))
         (origin (point-marker)))

    (setq elm-interactive--current-project default-directory)

    (pop-to-buffer
     (if (or buffer (not (derived-mode-p 'elm-interactive-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer elm-interactive--buffer-name))
       (current-buffer)))

    (unless buffer
      (apply #'make-comint-in-buffer elm-interactive--process-name buffer
             elm-interactive-command nil elm-interactive-arguments)
      (elm-interactive-mode))

    (setq-local elm-repl--origin origin)))

(defun elm-repl-return-to-origin ()
  "Jump back to the location from which we last jumped to the repl."
  (interactive)
  (let* ((pos elm-repl--origin)
         (buffer (get-buffer (marker-buffer pos))))
    (when buffer
      (pop-to-buffer buffer)
      (goto-char pos))))

;;;###autoload
(defun elm-repl-load ()
  "Load an interactive REPL if there isn't already one running.
Changes the current root directory to be the directory with the closest
package json if one exists otherwise sets it to be the working directory
of the file specified."
  (interactive)
  (save-buffer)
  (let ((import-statement (elm--build-import-statement)))
    (run-elm-interactive)
    (elm-interactive--send-command ":reset\n")
    (elm-interactive--send-command import-statement)))

;;;###autoload
(defun elm-repl-push (beg end)
  "Push the region from BEG to END to an interactive REPL."
  (interactive "r")
  (let* ((to-push (buffer-substring-no-properties beg end))
         (lines (split-string (s-trim-right to-push) "\n")))
    (run-elm-interactive)
    (dolist (line lines)
      (elm-interactive--send-command (concat line " \\\n")))
    (elm-interactive--send-command "\n")))

;;;###autoload
(defun elm-repl-push-decl ()
  "Push the current top level declaration to the REPL."
  (interactive)
  (let ((lines (elm--get-decl)))
    (run-elm-interactive)
    (dolist (line lines)
      (elm-interactive--send-command (concat line " \\\n")))
    (elm-interactive--send-command "\n")))


;;; Reactor:
;;;###autoload
(defun run-elm-reactor ()
  "Run the Elm reactor process."
  (interactive)
  (let ((default-directory (elm--find-dependency-file-path))
        (process (get-process elm-reactor--process-name)))

    (when process
      (delete-process process))

    (apply #'start-process elm-reactor--process-name elm-reactor--buffer-name
           elm-reactor-command elm-reactor-arguments)))

(defun elm-reactor--browse (path &optional debug)
  "Open (reactor-relative) PATH in browser with optional DEBUG.

Runs `elm-reactor' first."
  (run-elm-reactor)
  (browse-url (concat "http://" elm-reactor-address ":" elm-reactor-port "/" path (when debug "?debug"))))

;;;###autoload
(defun elm-preview-buffer (debug)
  "Preview the current buffer using Elm reactor (in debug mode if DEBUG is truthy)."
  (interactive "P")
  (let* ((fname (buffer-file-name))
         (deppath (elm--find-dependency-file-path))
         (path (f-relative fname deppath)))
    (elm-reactor--browse path debug)))

;;;###autoload
(defun elm-preview-main (debug)
  "Preview the main elm file using Elm reactor (in debug mode if DEBUG is truthy)."
  (interactive "P")
  (elm-reactor--browse (elm--find-main-file) debug))


;;; Make:
(defun elm-compile--command (file &optional output json)
  "Generate a command that will compile FILE into OUTPUT, with or without JSON reporting."
  (let ((elm-compile-arguments
         (if output
             (append (cl-remove-if (apply-partially #'string-prefix-p "--output=") elm-compile-arguments)
                     (list (concat "--output=" (expand-file-name output))))
           elm-compile-arguments)))
    (concat elm-compile-command " "
            (mapconcat 'shell-quote-argument
                       (append (list file)
                               elm-compile-arguments
                               (when json
                                 (list "--report=json")))
                       " "))))

(defun elm-compile--filter ()
  "Filter function for compilation output."
  (message "APPLY! %S %S" compilation-filter-start (point-max))
  (ansi-color-apply-on-region (point-min) (point-max)))

(define-compilation-mode elm-compilation-mode "Elm-Compile"
  "Elm compilation mode."
  (progn
    (add-hook 'compilation-filter-hook 'elm-compile--filter nil t)))

(defun elm-compile--file (file &optional output)
  "Compile FILE into OUTPUT."
  (let ((default-directory (elm--find-dependency-file-path)))
    (with-current-buffer
        (compilation-start
         (elm-compile--command file output)
         'elm-compilation-mode
         (lambda (_) elm-compile--buffer-name)))))

(defun elm-compile--file-json (file &optional output)
  "Compile FILE into OUTPUT and return the JSON report.
The report is a list of elements sorted by their occurrence order
in the file."
  (let* ((default-directory (elm--find-dependency-file-path))
         (report (shell-command-to-string
                  (elm-compile--command file output t))))
    (if (string-prefix-p "[" report)
        (let ((json (json-read-from-string report)))
          (cl-flet ((start-line (o) (let-alist o .region.start.line)))
            (cl-sort (append json nil) (lambda (o1 o2) (< (start-line o1) (start-line o2))))))
      (error "Nothing to do"))))

(defun elm-compile--temporary ()
  "Get a compilation report for the current buffer."
  (let* ((input (make-temp-file "elm-comp-in-" nil ".elm"))
         (output (make-temp-file "elm-comp-out-" nil ".js")))
    (unwind-protect
        (progn
          (write-region (point-min) (point-max) input)
          (elm-compile--file-json input output))
      (delete-file output)
      (delete-file input))))


;;;###autoload
(defun elm-compile-buffer (&optional output)
  "Compile the current buffer into OUTPUT."
  (interactive
   (when current-prefix-arg
     (list (read-file-name "Output to: "))))
  (save-some-buffers)
  (elm-compile--file (elm--buffer-local-file-name) output))

;;;###autoload
(defun elm-compile-main (&optional output)
  "Compile the main elm file into OUTPUT."
  (interactive
   (when current-prefix-arg
     (list (read-file-name "Output to: "))))
  (elm-compile--file (elm--find-main-file) output))

(defun elm-compile--ensure-saved ()
  "Ensure the current buffer has been saved."
  (when (buffer-modified-p)
    (if (y-or-n-p "Save current buffer? ")
        (save-buffer)
      (error "You must save your changes first"))))

;;;###autoload
(defun elm-compile-clean-imports (&optional prompt)
  "Remove unused imports from the current buffer, PROMPT optionally before deleting."
  (interactive "P")
  (let* ((report (elm-compile--temporary))
         (line-offset 0))
    (dolist (ob report)
      (let-alist ob
        (when (equal .tag "unused import")
          (save-excursion
            (goto-char 0)
            (forward-line (- .region.start.line 1 line-offset))
            (when (or (not prompt) (y-or-n-p "Delete this import? "))
              (dotimes (_ (1+ (- .region.end.line
                                 .region.start.line)))
                (kill-whole-line)
                (setq line-offset (1+ line-offset))))))))))

(defconst elm-import--pattern
  ;; FIXME: should just use rx
  (cl-labels ((re-or (&rest forms)
                     (concat "\\(?:" (s-join "\\|" forms) "\\)"))
              (re-? (&rest forms)
                    (concat "\\(?:" (s-join "" forms) "\\)?"))
              (re-* (&rest forms)
                    (concat "\\(?:" (s-join "" forms) "\\)*")))
    (let* ((spaces "[[:space:]\n]")
           (ws (concat spaces "*"))
           (ws+ (concat spaces "+"))
           (upcase "[A-Z][A-Za-z0-9_]*")
           (lowcase "[a-z][A-Za-z0-9_]*")
           (as-form (concat ws+ "as" ws+ upcase))
           (exposing-union-type
            (concat upcase
                    (re-? ws
                          "("
                          (re-or "\\.\\." ;; expose all
                                 (concat upcase
                                         (re-* ws "," ws upcase)))
                          ws
                          ")")))
           (exposing-operator
            (re-? ws "([^)]+)"))
           (exposing-subform
            (re-or lowcase exposing-union-type exposing-operator))
           (exposing-form
            (concat ws+
                    "exposing"
                    ws
                    "("
                    ws
                    (re-or "\\.\\." ;; expose all
                           (concat exposing-subform
                                   (re-* ws "," ws exposing-subform)))
                    ws
                    ")")))
      (concat "^import"
              ws+
              (concat upcase (re-* "\\." upcase))
              (re-? (re-or (concat as-form (re-? exposing-form))
                           (concat exposing-form (re-? as-form)))))))
  "Regex to match elm import (including multiline).
Import consists of the word \"import\", real package name, and optional
\"as\" part, and \"exposing\" part, which may be ordered in either way.")

;;;###autoload
(defun elm-sort-imports ()
  "Sort the import list in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward elm-import--pattern nil t)
    (re-search-backward elm-import--pattern nil t)
    (let* ((beg (point))
           (_ (while (re-search-forward elm-import--pattern nil t)))
           (end (point))
           (old-imports (buffer-substring-no-properties beg end))
           (imports (mapcar 'first
                            (s-match-strings-all elm-import--pattern old-imports)))
           (sorted-imports (s-join "\n" (sort imports 'string<))))
      (unless (string= old-imports sorted-imports)
        (delete-region beg end)
        (insert sorted-imports)))))


;;;###autoload
(defun elm-compile-add-annotations (&optional prompt)
  "Add missing type annotations to the current buffer, PROMPT optionally before inserting."
  (interactive "P")
  (let* ((report (elm-compile--temporary))
         (line-offset 0))
    (dolist (ob report)
      (let-alist ob
        (when (equal .tag "missing type annotation")
          ;; Drop the first 2 lines from .details since they contain the warning itself.
          (let ((annotation (s-join "\n" (cdr (cdr (s-split "\n" .details))))))
            (goto-char 0)
            (forward-line (+ line-offset (1- .region.start.line)))
            (setq line-offset (1+ line-offset))
            (when (or (not prompt) (y-or-n-p (format "Add annotation '%s'? " annotation)))
              (princ (format "%s\n" annotation) (current-buffer)))))))))

;;; Package:
;;;###autoload
(defun elm-create-package ()
  "Generate a new package definition in the current directory."
  (interactive)
  (when (elm--has-dependency-file)
    (error "Elm-package.json already exists"))
  (let* ((default-directory (elm--find-dependency-file-path)))
    (message "Creating elm package definition. This might take a minute...")
    (shell-command elm-create-package-command)))

(defun elm-package--build-uri (&rest segments)
  "Build a URI by combining the package catalog root and SEGMENTS."
  (concat elm-package-catalog-root (s-join "/" segments)))

(defun elm-package--format-entry (index entry)
  "Format a package '(INDEX ENTRY) for display in the package listing."
  (let-alist entry
    (let ((mark (if (-contains? elm-package--marked-contents index)
                    "*"
                  ""))
          (button (list .name . ()))
          (status (if (-contains? elm-package--dependencies .name)
                      "dependency"
                    "available")))
      (list index (vector mark button (elt .versions 0) status .summary)))))

(defun elm-package--entries ()
  "Return the formatted package list."
  (-map-indexed #'elm-package--format-entry elm-package--contents))

(defun elm-package--get-marked-packages ()
  "Get packages that are marked for installation."
  (-map (lambda (id)
          (let-alist (nth id elm-package--contents)
            (concat .name " " (elt .versions 0))))
        elm-package--marked-contents))

(defun elm-package--get-marked-install-commands ()
  "Get a list of the commands required to install the marked packages."
  (-map (lambda (package)
          (concat elm-package-command " " (s-join " " elm-package-arguments) " " package))
        (elm-package--get-marked-packages)))

(defun elm-package--read-dependencies ()
  "Read the current package's dependencies."
  (setq elm-package--working-dir (elm--find-dependency-file-path))
  (let-alist (elm--read-dependency-file)
    (setq elm-package--dependencies (-map (lambda (dep) (symbol-name (car dep)))
                                          .dependencies))))

(defun elm-package--read-json (uri)
  "Read a JSON file from a URI."
  (with-current-buffer (url-retrieve-synchronously uri)
    (goto-char (point-min))
    (re-search-forward "^ *$")
    (json-read)))

(defun elm-package--read-package ()
  "Read a package from the minibuffer."
  (completing-read "Package: " elm-package--dependencies nil t))

(defun elm-package--read-module (package)
  "Read a module from PACKAGE from the minibuffer."
  (completing-read "Module: " (elm-package-modules package) nil t))

(defun elm-package--read-module-definition (package module)
  "Read a definition from PACKAGE and MODULE from the minibuffer."
  (completing-read "Definition: " (elm-package-definitions package module) nil t))

(defun elm-package-refresh-package (package version)
  "Refresh the cache for PACKAGE with VERSION."
  (let ((documentation-uri
         (elm-package--build-uri "packages" package version "documentation.json")))
    (setq elm-package--cache
          (cons `(,package . ,(elm-package--read-json documentation-uri))
                elm-package--cache))))

(defun elm-package-latest-version (package)
  "Get the latest version of PACKAGE."
  (let ((package (-find (lambda (p)
                          (let-alist p
                            (equal .name package)))
                        elm-package--contents)))

    (if (not package)
        (error "Package not found")
      (let-alist package
        (elt .versions 0)))))

(defun elm-package--ensure-cached (package)
  "Ensure that PACKAGE has been cached."
  (unless (assoc package elm-package--cache)
    (elm-package-refresh-package package (elm-package-latest-version package))))

(defun elm-package-modules (package)
  "Get PACKAGE's module list."
  (elm-package--ensure-cached package)
  (sort
   (mapcar (lambda (module)
             (let-alist module .name))
           (cdr (assoc package elm-package--cache)))
   #'string<))

(defun elm-package--select-module (package module-name)
  "Select a PACKAGE's MODULE-NAME from the cache."
  (elm-package--ensure-cached package)
  (elt (cl-remove-if-not
        (lambda (module)
          (let-alist module (equal module-name .name)))
        (cdr (assoc package elm-package--cache))) 0))

(defun elm-package-definitions (package module-name)
  "Get all of PACKAGE's MODULE-NAME's definitions."
  (let-alist (elm-package--select-module package module-name)
    (let* ((extract (lambda (x)
                      (let ((name (cdr (assq 'name x))))
                        (cons name nil))))
           (aliases (mapcar extract .aliases))
           (types (mapcar extract .types))
           (values (mapcar extract .values)))
      (append aliases types values))))

(defun elm-package-definition (package module-name definition-name)
  "Get documentation from PACKAGE's MODULE-NAME for DEFINITION-NAME."
  (let-alist (elm-package--select-module package module-name)
    (elt (cl-remove-if-not
          (lambda (x)
            (equal definition-name (cdr (assq 'name x))))
          (vconcat .aliases .types .values))
         0)))

(defun elm-package-refresh ()
  "Refresh the package catalog's contents."
  (interactive)
  (with-current-buffer elm-package-buffer-name
    (elm-package--read-dependencies)
    (tabulated-list-print :remember-pos)))

(defun elm-package-prev (&optional n)
  "Goto (Nth) previous package."
  (interactive "p")
  (elm-package-next (- n))
  (forward-line 0)
  (forward-button 1))

(defun elm-package-next (&optional n)
  "Goto (Nth) next package."
  (interactive "p")
  (dotimes (_ (abs n))
    (let ((d (cl-signum n)))
      (forward-line (if (> n 0) 1 0))
      (when (eobp)
        (forward-line -1))
      (forward-button d))))

(defun elm-package-mark ()
  "Mark the package at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (setq elm-package--marked-contents (cons id elm-package--marked-contents))
      (elm-package-next 1)
      (elm-package-refresh))))

(defun elm-package-unmark ()
  "Unmark the package at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (setq elm-package--marked-contents
            (-reject (lambda (x) (= id x))
                     elm-package--marked-contents))
      (elm-package-next 1)
      (elm-package-refresh))))

(defun elm-package-view ()
  "View the package at point in a browser."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (let-alist (nth id elm-package--contents)
        (browse-url (elm-package--build-uri "packages" .name (elt .versions 0)))))))

(defun elm-package--install-sentinel (_proc _msg)
  "Refreshes the package buffer on _PROC exit, ignoring _MSG."
  (elm-package-refresh))

(defun elm-package-install ()
  "Install the marked packages."
  (interactive)
  (unless elm-package--marked-contents
    (error "Nothing to install"))
  (let* ((and (elm--shell-and-command))
         (command-to-run (s-join and (elm-package--get-marked-install-commands))))
    (when (yes-or-no-p (concat "Install " (s-join ", " (elm-package--get-marked-packages)) " ?"))
      (let* ((default-directory elm-package--working-dir)
             (compilation-buffer-name-function (lambda (_) elm-package-compile-buffer-name))
             (compilation-buffer (compile command-to-run)))
        (setq elm-package--marked-contents nil)
        (set-process-sentinel (get-buffer-process compilation-buffer)
                              #'elm-package--install-sentinel)))))

;;;###autoload
(defun elm-package-catalog (refresh)
  "Show the package catalog, refreshing the list if REFRESH is truthy."
  (interactive "P")
  (elm--assert-dependency-file)
  (when (or refresh (not elm-package--contents))
    (elm-package-refresh-contents))
  (let ((buffer (get-buffer-create elm-package-buffer-name)))
    (pop-to-buffer buffer)
    (elm-package--read-dependencies)
    (elm-package-mode)))

;;;###autoload
(defun elm-package-refresh-contents ()
  "Refresh the package list."
  (interactive)
  (elm--assert-dependency-file)
  (let* ((all-packages (elm-package--build-uri "all-packages")))
    (with-current-buffer (url-retrieve-synchronously all-packages)
      (goto-char (point-min))
      (re-search-forward "^ *$")
      (setq elm-package--marked-contents nil)
      (setq elm-package--contents (append (json-read) nil)))))

;;;###autoload
(defun elm-import (refresh)
  "Import a module, refreshing if REFRESH is truthy."
  (interactive "P")
  (elm--assert-dependency-file)
  (when (or refresh (not elm-package--contents))
    (elm-package-refresh-contents))
  (elm-package--read-dependencies)
  (let* ((package (elm-package--read-package))
         (module (elm-package--read-module package))
         (statement (concat "import " module))
         (statement (read-string "Import statement: " statement)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^import " nil t)
          (beginning-of-line)
        (forward-line 1)
        (insert "\n"))
      (insert (concat statement "\n"))))
  (elm-sort-imports))


(defun elm-documentation--show (documentation)
  "Show DOCUMENTATION in a help buffer."
  (let-alist documentation
    (help-setup-xref (list #'elm-documentation--show documentation) nil)
    (save-excursion
      (with-help-window (help-buffer)
        (with-current-buffer (help-buffer)
          (point-min)
          (insert (propertize .name 'face 'font-lock-function-name-face))
          (when .args
            (insert (concat " " (s-join " " .args))))
          (when .cases
            (let ((first t))
              (mapc
               (lambda (case)
                 (if first
                     (insert "\n  = ")
                   (insert "\n  | "))
                 (insert (propertize (elt case 0) 'face 'font-lock-function-name-face))
                 (insert (concat " " (s-join " " (elt case 1))))
                 (setq first nil))
               .cases)))
          (when .type
            (insert " : ")
            (insert (propertize .type 'face 'font-lock-type-face)))
          (insert (concat "\n\n" (s-trim-left .comment)))))))  )

;;;###autoload
(defun elm-documentation-lookup (refresh)
  "Lookup the documentation for a function, refreshing if REFRESH is truthy."
  (interactive "P")
  (elm--assert-dependency-file)
  (when (or refresh (not elm-package--contents))
    (elm-package-refresh-contents))
  (elm-package--read-dependencies)
  (let* ((package (elm-package--read-package))
         (module (elm-package--read-module package))
         (definition (elm-package--read-module-definition package module))
         (documentation (elm-package-definition package module definition)))
    (elm-documentation--show documentation)))

;;;###autoload
(define-derived-mode elm-package-mode tabulated-list-mode "Elm Package"
  "Special mode for elm-package.

\\{elm-package-mode-map}"

  (buffer-disable-undo)

  (setq truncate-lines t

        tabulated-list-format elm-package-catalog-format
        tabulated-list-entries #'elm-package--entries)

  (tabulated-list-init-header)
  (tabulated-list-print))


(autoload 'popup-make-item "popup")


(defun elm-oracle--completion-prefix-at-point ()
  "Return the completions prefix found at point."
  (save-excursion
    (let* ((_ (re-search-backward elm-oracle--pattern nil t))
           (beg (1+ (match-beginning 0)))
           (end (match-end 0)))
      (s-trim (buffer-substring-no-properties beg end)))))


(defun elm-oracle--completion-namelist (prefix &optional callback)
  "Extract a list of identifier names for PREFIX.  Async if CALLBACK is provided."
  (cl-flet* ((names (candidates)
                    (-map (lambda (candidate)
                            (let-alist candidate
                              .fullName))
                          candidates))
             (names-async (candidates)
                          (funcall callback (names candidates))))
    (if callback
        (elm-oracle--get-completions-cached prefix #'names-async)
      (names (elm-oracle--get-completions-cached prefix)))))

(defun elm-oracle--completions-select (candidate)
  "Search completions for CANDIDATE."
  (aref (elm-oracle--get-completions-cached candidate) 0))

(defun elm-oracle--completion-docbuffer (candidate)
  "Return the documentation for CANDIDATE."
  (company-doc-buffer
   (let-alist (elm-oracle--completions-select candidate)
     (format "%s\n\n%s" .signature .comment))))

(defun elm-oracle--completion-annotation (candidate)
  "Return the annotation for CANDIDATE."
  (let-alist (elm-oracle--completions-select candidate)
    (format " %s" .signature)))

(defun elm-oracle--completion-signature (candidate)
  "Return the signature for CANDIDATE."
  (let-alist (elm-oracle--completions-select candidate)
    (format "%s : %s" candidate .signature)))

(defun elm-oracle--get-completions-async (command callback)
  "Get completions by running COMMAND asynchronously.  CALLBACK called on success."
  (let ((output nil))
    (cl-flet ((output-filter (_proc string)
                             (add-to-list 'output string))
              (process-sentinel (_proc event)
                                (if (equal event "finished\n")
                                    (let ((data (json-read-from-string (s-join "" (reverse output)))))
                                      (funcall callback data)))))

      (make-process
       :name "elm-oracle"
       :buffer "elm-oracle"
       :command command
       :filter #'output-filter
       :sentinel #'process-sentinel
       :connection-type 'pipe))))

(defun elm-oracle--get-completions-sync (command)
  "Get completions by running COMMAND synchronously."
  (json-read-from-string (shell-command-to-string (s-join " " command))))

(defun elm-oracle--get-completions-cached-1 (prefix &optional callback)
  "Get completions for PREFIX.  Async if CALLBACK is provided."
  (when (not (elm--has-dependency-file))
    (error "Completion only works inside Elm projects.  Create one with `M-x elm-create-package RET`"))

  (let* ((default-directory (elm--find-dependency-file-path))
         (current-file (or (buffer-file-name) (elm--find-main-file)))
         (command (list elm-oracle-command
                        (shell-quote-argument current-file)
                        (shell-quote-argument prefix))))
    (cl-flet* ((cache (candidates)
                      (when (> (length candidates) 0)
                        (puthash prefix candidates elm-oracle--completion-cache)))
               (cache-async (candidates)
                            (cache candidates)
                            (funcall callback candidates)))
      (if callback
          (if (fboundp 'make-process)
              (elm-oracle--get-completions-async command #'cache-async)
            ;; If make-process is not available (<25.1) then we downgrade to a sync call
            (cache-async (elm-oracle--get-completions-sync command)))
        (cache (elm-oracle--get-completions-sync command))))))

(defun elm-oracle--filter-completions (prefix candidates)
  "Filter by PREFIX a list of CANDIDATES."
  (cl-remove-if-not (lambda (candidate)
                      (let-alist candidate
                        (string-prefix-p prefix .fullName)))
                    candidates))

(defun elm-oracle--get-completions-cached (prefix &optional callback)
  "Cache and return the cached elm-oracle completions for PREFIX.  Async if CALLBACK is provided."
  (when (and prefix (s-contains? "." prefix))
    (or (gethash prefix elm-oracle--completion-cache)
        (let* ((module (car (s-split-up-to "\\." prefix 1)))
               (module-candidates (gethash module elm-oracle--completion-cache)))
          (cl-flet ((handle-async (candidates)
                                  (funcall callback
                                           (elm-oracle--filter-completions prefix candidates))))
            (if callback
                (if module-candidates
                    (handle-async module-candidates)
                  (elm-oracle--get-completions-cached-1 module #'handle-async))
              (elm-oracle--filter-completions prefix
               (or module-candidates
                   (elm-oracle--get-completions-cached-1 module)))))))))

(defun elm-oracle--get-completions (prefix &optional popup)
  "Get elm-oracle completions for PREFIX with optional POPUP formatting."
  (let* ((candidates (elm-oracle--get-completions-cached prefix))
         (candidates
          (-map (lambda (candidate)
                  (let-alist candidate
                    (if popup
                        (popup-make-item .fullName
                                         :document (concat .signature "\n\n" .comment)
                                         :summary .signature)
                      .fullName)))
                candidates)))
    candidates))

(defun elm-oracle--get-first-completion (item &optional callback)
  "Get the first completion for ITEM.  Async if CALLBACK provided."
  (let* ((default-directory (elm--find-dependency-file-path))
         (current-file (buffer-file-name))
         (command (list elm-oracle-command current-file item)))
    (cl-flet* ((select-first (candidates)
                             (when (> (length candidates) 0)
                               (elt candidates 0)))
               (select-first-async (candidates)
                                   (funcall callback (select-first candidates))))
      (if callback
          (elm-oracle--get-completions-async command #'select-first-async)
        (select-first (json-read-from-string (shell-command-to-string (s-join " " command))))))))

(defun elm-oracle--function-at-point ()
  "Get the name of the function at point."
  (save-excursion
    (let ((symbol (symbol-name (symbol-at-point))))
      (skip-chars-forward "[A-Za-z0-9_.']")
      (let* ((_ (re-search-backward elm-oracle--pattern nil t))
             (beg (1+ (match-beginning 0)))
             (end (match-end 0))
             (item (s-trim (buffer-substring-no-properties beg end))))
        (if (string-empty-p item)
            symbol
          item)))))

(defun elm-oracle--completion-at-point ()
  "Get the Oracle completion object at point."
  (elm-oracle--get-first-completion (elm-oracle--function-at-point)))

(defun elm-oracle--propertize-completion-type (completion)
  "Propertize COMPLETION so that it can be displayed in the minibuffer."
  (when completion
    (let-alist completion
      (when (and (not .error) .name)
        (concat
         (propertize .fullName 'face 'font-lock-function-name-face)
         ": "
         .signature)))))

(defun elm-oracle--type-at-point ()
  "Get the type of the function at point."
  (elm-oracle--propertize-completion-type (elm-oracle--completion-at-point)))

;;;###autoload
(defun elm-oracle-type-at-point ()
  "Print the type of the function at point to the minibuffer."
  (interactive)
  (let ((type (elm-oracle--type-at-point)))
    (if type
        (message type)
      (message "Unknown type"))))

;;;###autoload
(defun elm-eldoc ()
  "Get the type of the function at point for eldoc."
  (let* ((name (elm-oracle--function-at-point))
         (type (gethash name elm-oracle--eldoc-cache)))
    (cl-flet ((cache (completion)
                     (when completion
                       (puthash name (elm-oracle--propertize-completion-type completion) elm-oracle--eldoc-cache))))
      (elm-oracle--get-first-completion name #'cache)
      type)))

;;;###autoload
(defun elm-oracle-doc-at-point ()
  "Show the documentation of the value at point."
  (interactive)
  (let ((completion (elm-oracle--completion-at-point)))
    (if completion
        (elm-documentation--show completion)
      (message "Unknown symbol"))))

;;;###autoload
(defun elm-oracle-completion-at-point-function ()
  "Completion at point function for elm-oracle."
  (save-excursion
    (let* ((_ (re-search-backward elm-oracle--pattern nil t))
           (beg (1+ (match-beginning 0)))
           (end (match-end 0))
           (prefix (s-trim (buffer-substring-no-properties beg end)))
           (completions (elm-oracle--get-completions prefix)))
      (list beg end completions :exclusive 'no))))

;;;###autoload
(defun elm-oracle-setup-completion ()
  "Set up standard completion.
Add this function to your `elm-mode-hook' to enable an
elm-specific `completion-at-point' function."
  (add-hook 'completion-at-point-functions
            #'elm-oracle-completion-at-point-function
            nil t))

(defvar ac-sources)
(defvar ac-source-elm
  `((candidates . (elm-oracle--get-completions ac-prefix t))
    (prefix . ,elm-oracle--pattern)))

;;;###autoload
(defun elm-oracle-setup-ac ()
  "Set up auto-complete support.
Add this function to your `elm-mode-hook'."
  (add-to-list 'ac-sources 'ac-source-elm))

;;;###autoload
(defun company-elm (command &optional arg &rest ignored)
  "Provide completion info according to COMMAND and ARG.  IGNORED is not used."
  (interactive (list 'interactive))
  (when (derived-mode-p 'elm-mode)
    (cl-case command
      (interactive (company-begin-backend 'company-elm))
      (prefix (elm-oracle--completion-prefix-at-point))
      (doc-buffer (elm-oracle--completion-docbuffer arg))
      (candidates (cons :async (apply-partially #'elm-oracle--completion-namelist arg)))
      (annotation (elm-oracle--completion-annotation arg))
      (meta (elm-oracle--completion-signature arg)))))

;;;###autoload
(defun elm-test-project ()
  "Run the elm-test command on the current project."
  (interactive)
  (let ((default-directory (elm--find-elm-test-root-directory))
        (compilation-buffer-name-function (lambda (_) "*elm-test*")))
    (compile "elm-test")))


(provide 'elm-interactive)
;;; elm-interactive.el ends here
