;;; haskell-compat.el --- legacy/compatibility backports for haskell-mode -*- lexical-binding: t -*-
;;
;; Filename: haskell-compat.el
;; Description: legacy/compatibility backports for haskell-mode

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
(require 'etags)
(require 'ring)
(require 'outline)
(require 'xref nil t)

(eval-when-compile
  (setq byte-compile-warnings '(not cl-functions obsolete)))


;; Cross-referencing commands have been replaced since Emacs 25.1.
;; These aliases are required to provide backward compatibility.
(unless (fboundp 'xref-push-marker-stack)
  (defalias 'xref-pop-marker-stack 'pop-tag-mark)

  (defun xref-push-marker-stack (&optional m)
    "Add point M (defaults to `point-marker') to the marker stack."
    (ring-insert find-tag-marker-ring (or m (point-marker)))))

(unless (fboundp 'outline-hide-sublevels)
  (defalias 'outline-hide-sublevels 'hide-sublevels))

(unless (fboundp 'outline-show-subtree)
  (defalias 'outline-show-subtree 'show-subtree))

(unless (fboundp 'outline-hide-sublevels)
  (defalias 'outline-hide-sublevels 'hide-sublevels))

(unless (fboundp 'outline-show-subtree)
  (defalias 'outline-show-subtree 'show-subtree))

(unless (fboundp 'xref-find-definitions)
  (defun xref-find-definitions (ident)
    (let ((next-p (and (boundp 'xref-prompt-for-identifier)
                       xref-prompt-for-identifier)))
      (find-tag ident next-p))))

(unless (fboundp 'font-lock-ensure)
  (defalias 'font-lock-ensure 'font-lock-fontify-buffer))

(provide 'haskell-compat)

;;; haskell-compat.el ends here
