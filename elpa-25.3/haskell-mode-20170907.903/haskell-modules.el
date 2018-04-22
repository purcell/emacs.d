;;; haskell-modules.el --- -*- lexical-binding: t -*-

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

(require 'haskell-sort-imports)
(require 'haskell-align-imports)
(require 'haskell-session)
(require 'haskell-navigate-imports)
(require 'haskell-complete-module)
(require 'haskell-sandbox)
(require 'haskell-customize)

(defun haskell-add-import (&optional module)
  "Add an import to the import list.  Sorts and aligns imports,
unless `haskell-stylish-on-save' is set, in which case we defer
to stylish-haskell."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (haskell-navigate-imports)
    (insert (haskell-import-for-module
             (or module
                 (haskell-complete-module-read
                  "Module: "
                  (haskell-session-all-modules (haskell-modules-session))))))
    (unless haskell-stylish-on-save (haskell-sort-imports)
            (haskell-align-imports))))

(defun haskell-import-for-module (module)
  "Get import statements for the given module."
  (let ((mapping (assoc module haskell-import-mapping)))
    (if mapping
        (cdr mapping)
      (concat (read-from-minibuffer "Import line: "
                                    (format "import %s" module))
              "\n"))))

;;;###autoload
(defun haskell-session-installed-modules (_session &optional _dontcreate)
  "Get the modules installed in the current package set."
  ;; TODO: Again, this makes HEAVY use of unix utilities. It'll work
  ;; fine in Linux, probably okay on OS X, and probably not at all on
  ;; Windows. Again, if someone wants to test on Windows and come up
  ;; with alternatives that's OK.
  ;;
  ;; Ideally all these package queries can be provided by a Haskell
  ;; program based on the Cabal API. Possibly as a nice service. Such
  ;; a service could cache and do nice things like that. For now, this
  ;; simple shell script takes us far.
  ;;
  ;; Probably also we can take the code from inferior-haskell-mode.
  ;;
  ;; Ugliness aside, if it saves us time to type it's a winner.
  ;;
  ;; FIXME/TODO: add support for (eq 'cabal-repl (haskell-process-type))
  (let ((session (haskell-session-maybe)))
    (when session
      (let ((modules (shell-command-to-string
                      (format "%s 2> /dev/null | %s | %s"
                              (cond
                               ((haskell-sandbox-exists-p session)
                                (concat "ghc-pkg dump -f "
                                        (shell-quote-argument (haskell-sandbox-pkgdb session))))
                               (t "ghc-pkg dump"))
                              "egrep '^(exposed-modules: |                 )[A-Z]'"
                              "cut -c18-"))))
        (split-string modules)))))

;;;###autoload
(defun haskell-session-all-modules (session &optional dontcreate)
  "Get all modules -- installed or in the current project.
If DONTCREATE is non-nil don't create a new session."
  (append (haskell-session-installed-modules session dontcreate)
          (haskell-session-project-modules session dontcreate)))

;;;###autoload
(defun haskell-session-project-modules (session &optional dontcreate)
  "Get the modules of the current project.
If DONTCREATE is non-nil don't create a new session."
  (if (or (not dontcreate) (haskell-session-maybe))
      (let* ((modules
              (shell-command-to-string
               (format "%s && %s"
                       (format "cd %s" (haskell-session-cabal-dir session))
                       ;; TODO: Use a different, better source. Possibly hasktags or some such.
                       ;; TODO: At least make it cross-platform. Linux
                       ;; (and possibly OS X) have egrep, Windows
                       ;; doesn't -- or does it via Cygwin or MinGW?
                       ;; This also doesn't handle module\nName. But those gits can just cut it out!
                       "egrep '^module[\t\r ]+[^(\t\r ]+' . -r -I --include='*.*hs' --include='*.hsc' -s -o -h | sed 's/^module[\t\r ]*//' | sort | uniq"))))
        (split-string modules))))

(defun haskell-modules-session ()
  "Get the `haskell-session', throw an error if it's not
  available."
  (or (haskell-session-maybe)
      (haskell-session-assign
       (or (haskell-session-from-buffer)
           (haskell-session-choose)
           (error "No session associated with this buffer. Try M-x haskell-session-change or report this as a bug.")))))

(provide 'haskell-modules)
