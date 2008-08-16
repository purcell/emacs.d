;;; haskell-c.el --- Major mode for *.hsc files

;; Copyright (C) 2007  Stefan Monnier

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>

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

;; 

;;; Code:

(require 'haskell-mode)
(require 'haskell-font-lock)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hsc\\'" . haskell-c-mode))

(defvar haskell-c-font-lock-keywords
  `(("^#[ \t]*[[:alnum:]]+" (0 font-lock-preprocessor-face))
    ,@haskell-font-lock-symbols))

;;;###autoload
(define-derived-mode haskell-c-mode haskell-mode "Haskell-C"
  "Major mode for Haskell FFI files."
  (set (make-local-variable 'font-lock-keywords)
       (cons 'haskell-c-font-lock-keywords
             (cdr font-lock-keywords))))

(provide 'haskell-c)
;; arch-tag: 51294c41-29f0-4599-9ce8-47fe2e7d3fd5
;;; haskell-c.el ends here
