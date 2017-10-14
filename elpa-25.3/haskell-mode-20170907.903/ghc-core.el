;;; ghc-core.el --- Syntax highlighting module for GHC Core -*- lexical-binding: t -*-

;; Copyright (C) 2010  Johan Tibell

;; Author: Johan Tibell <johan.tibell@gmail.com>

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Purpose:
;;
;; To make it easier to read GHC Core output by providing highlighting
;; and removal of commonly ignored annotations.

;;; Code:
(require 'haskell-mode)
(require 'haskell-font-lock)

;;;###autoload
(defgroup ghc-core nil
  "Major mode for viewing pretty printed GHC Core output."
  :link '(custom-manual "(haskell-mode)")
  :group 'haskell
  :prefix "ghc-core-")

(defcustom ghc-core-program
  "ghc"
  "Name of the GHC executable (excluding any arguments)."
  :type 'string
  :group 'ghc-core)

(defcustom ghc-core-program-args
  '("-O2")
  "Additional options to be passed to GHC when generating core output.
GHC (see variable `ghc-core-program') is invoked with the basic
command line options \"-ddump-simpl -c <source-file>\"
followed by the additional options defined here.

The following `-ddump-simpl` options might be of interest:

 - `-dsuppress-all'
 - `-dsuppress-uniques'
 - `-dsuppress-idinfo'
 - `-dsuppress-module-prefixes'
 - `-dsuppress-type-signatures'
 - `-dsuppress-type-applications'
 - `-dsuppress-coercions'

See `M-x manual-entry RET ghc' for more details."
  :type '(repeat (string :tag "Argument"))
  :group 'ghc-core)

(define-obsolete-variable-alias 'ghc-core-create-options 'ghc-core-program-args
  "haskell-mode 13.7")

(defun ghc-core-clean-region (start end)
  "Remove commonly ignored annotations and namespace prefixes
in the region between START and END."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward-regexp "GHC\.[^\.]*\." nil t)
      (replace-match "" nil t))
    (goto-char (point-min))
    (while (flush-lines "^ *GblId *$" nil))
    (goto-char (point-min))
    (while (flush-lines "^ *LclId *$" nil))
    (goto-char (point-min))
    (while (flush-lines (concat "^ *\\[\\(?:Arity [0-9]+\\|NoCafRefs\\|"
                                "Str: DmdType\\|Worker \\)"
                                "\\([^]]*\\n?\\).*\\] *$") nil))
    (goto-char (point-min))
    (while (search-forward "Main." nil t) (replace-match "" nil t))))

(defun ghc-core-clean-buffer ()
  "Remove commonly ignored annotations and namespace prefixes
in the current buffer."
  (interactive)
  (ghc-core-clean-region (point-min) (point-max)))

;;;###autoload
(defun ghc-core-create-core ()
  "Compile and load the current buffer as tidy core."
  (interactive)
  (save-buffer)
  (let* ((core-buffer (generate-new-buffer "ghc-core"))
         (neh (lambda () (kill-buffer core-buffer))))
    (add-hook 'next-error-hook neh)
    (apply #'call-process ghc-core-program nil core-buffer nil
           "-ddump-simpl" "-c" (buffer-file-name) ghc-core-program-args)
    (display-buffer core-buffer)
    (with-current-buffer core-buffer
      (ghc-core-mode))
    (remove-hook 'next-error-hook neh)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hcr\\'" . ghc-core-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dump-simpl\\'" . ghc-core-mode))

;;;###autoload
(define-derived-mode ghc-core-mode haskell-mode "GHC-Core"
  "Major mode for GHC Core files.")

(provide 'ghc-core)
;;; ghc-core.el ends here
