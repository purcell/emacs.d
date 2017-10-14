;;; haskell-compile.el --- Haskell/GHC compilation sub-mode -*- lexical-binding: t -*-

;; Copyright (C) 2013  Herbert Valerio Riedel

;; Author: Herbert Valerio Riedel <hvr@gnu.org>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple GHC-centric compilation sub-mode; see info node
;; `(haskell-mode)compilation' for more information

;;; Code:

(require 'compile)
(require 'haskell-cabal)

;;;###autoload
(defgroup haskell-compile nil
  "Settings for Haskell compilation mode"
  :link '(custom-manual "(haskell-mode)compilation")
  :group 'haskell)

(defcustom haskell-compile-cabal-build-command
  "cd %s && cabal build --ghc-option=-ferror-spans"
  "Default build command to use for `haskell-cabal-build' when a cabal file is detected.
The `%s' placeholder is replaced by the cabal package top folder."
  :group 'haskell-compile
  :type 'string)

(defcustom haskell-compile-cabal-build-alt-command
  "cd %s && cabal clean -s && cabal build --ghc-option=-ferror-spans"
  "Alternative build command to use when `haskell-cabal-build' is called with a negative prefix argument.
The `%s' placeholder is replaced by the cabal package top folder."
  :group 'haskell-compile
  :type 'string)

(defcustom haskell-compile-command
  "ghc -Wall -ferror-spans -fforce-recomp -c %s"
  "Default build command to use for `haskell-cabal-build' when no cabal file is detected.
The `%s' placeholder is replaced by the current buffer's filename."
  :group 'haskell-compile
  :type 'string)

(defcustom haskell-compile-ghc-filter-linker-messages
  t
  "Filter out unremarkable \"Loading package...\" linker messages during compilation."
  :group 'haskell-compile
  :type 'boolean)

(defconst haskell-compilation-error-regexp-alist
  `((,(concat
       "^ *\\(?1:[^\t\r\n]+?\\):"
       "\\(?:"
       "\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)\\(?:-\\(?5:[0-9]+\\)\\)?" ;; "121:1" & "12:3-5"
       "\\|"
       "(\\(?2:[0-9]+\\),\\(?4:[0-9]+\\))-(\\(?3:[0-9]+\\),\\(?5:[0-9]+\\))" ;; "(289,5)-(291,36)"
       "\\)"
       ":\\(?6:\n?[ \t]+[Ww]arning:\\)?")
     1 (2 . 3) (4 . 5) (6 . nil)) ;; error/warning locus

    ;; multiple declarations
    ("^    \\(?:Declared at:\\|            \\) \\(?1:[^ \t\r\n]+\\):\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)$"
     1 2 4 0) ;; info locus

    ;; this is the weakest pattern as it's subject to line wrapping et al.
    (" at \\(?1:[^ \t\r\n]+\\):\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)\\(?:-\\(?5:[0-9]+\\)\\)?[)]?$"
     1 2 (4 . 5) 0)) ;; info locus
  "Regexps used for matching GHC compile messages.
See `compilation-error-regexp-alist' for semantics.")

(defvar haskell-compilation-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-mode-map))
  "Keymap for `haskell-compilation-mode' buffers.
This is a child of `compilation-mode-map'.")

(defun haskell-compilation-filter-hook ()
  "Local `compilation-filter-hook' for `haskell-compilation-mode'."

  (when haskell-compile-ghc-filter-linker-messages
    (delete-matching-lines "^ *Loading package [^ \t\r\n]+ [.]+ linking [.]+ done\\.$"
                           (save-excursion (goto-char compilation-filter-start)
                                           (line-beginning-position))
                           (point))))

(define-compilation-mode haskell-compilation-mode "HsCompilation"
  "Haskell/GHC specific `compilation-mode' derivative.
This mode provides support for GHC 7.[46]'s compile
messages. Specifically, also the `-ferror-spans` source location
format is supported, as well as info-locations within compile
messages pointing to additional source locations."
  (setq-local compilation-error-regexp-alist
              haskell-compilation-error-regexp-alist)

  (add-hook 'compilation-filter-hook
            'haskell-compilation-filter-hook nil t)
  )

;;;###autoload
(defun haskell-compile (&optional edit-command)
  "Compile the Haskell program including the current buffer.
Tries to locate the next cabal description in current or parent
folders via `haskell-cabal-find-dir' and if found, invoke
`haskell-compile-cabal-build-command' from the cabal package root
folder. If no cabal package could be detected,
`haskell-compile-command' is used instead.

If prefix argument EDIT-COMMAND is non-nil (and not a negative
prefix `-'), `haskell-compile' prompts for custom compile
command.

If EDIT-COMMAND contains the negative prefix argument `-',
`haskell-compile' calls the alternative command defined in
`haskell-compile-cabal-build-alt-command' if a cabal package was
detected.

`haskell-compile' uses `haskell-compilation-mode' which is
derived from `compilation-mode'. See Info
node `(haskell-mode)compilation' for more details."
  (interactive "P")
  (save-some-buffers (not compilation-ask-about-save)
                         compilation-save-buffers-predicate)
  (let* ((cabdir (haskell-cabal-find-dir))
         (command1 (if (eq edit-command '-)
                       haskell-compile-cabal-build-alt-command
                     haskell-compile-cabal-build-command))
         (srcname (buffer-file-name))
         (command (if cabdir
                      (format command1 cabdir)
                    (if (and srcname (derived-mode-p 'haskell-mode))
                        (format haskell-compile-command srcname)
                      command1))))
    (when (and edit-command (not (eq edit-command '-)))
      (setq command (compilation-read-command command)))

    (compilation-start command 'haskell-compilation-mode)))

(provide 'haskell-compile)
;;; haskell-compile.el ends here
