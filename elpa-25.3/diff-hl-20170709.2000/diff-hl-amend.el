;; Copyright (C) 2012-2013  Free Software Foundation, Inc.

;; Author:   Dmitry Gutov <dgutov@yandex.ru>
;; URL:      https://github.com/dgutov/diff-hl

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

;; Toggle in the current buffer with `M-x diff-hl-amend-mode'.
;; Toggle in all buffers with `M-x global-diff-hl-amend-mode'.

;;; Code:

(require 'diff-hl)

;;;###autoload
(define-minor-mode diff-hl-amend-mode
  "Show changes against the second-last revision in `diff-hl-mode'.
Most useful with backends that support rewriting local commits,
and most importantly, 'amending' the most recent one.
Currently only supports Git, Mercurial and Bazaar."
  :lighter " Amend"
  (if diff-hl-amend-mode
      (progn
        (diff-hl-amend-setup)
        (add-hook 'after-revert-hook 'diff-hl-amend-setup nil t))
    (remove-hook 'after-revert-hook 'diff-hl-amend-setup t)
    (setq-local diff-hl-reference-revision nil))
  (when diff-hl-mode
    (diff-hl-update)))

(defun diff-hl-amend-setup ()
  (let ((backend (vc-backend buffer-file-name)))
    (when backend
      (setq-local diff-hl-reference-revision
                  (cl-case backend
                    (Git
                     "HEAD^")
                    (Hg
                     "-2")
                    (Bzr
                     "revno:-2"))))))

;;;###autoload
(define-globalized-minor-mode global-diff-hl-amend-mode diff-hl-amend-mode
  turn-on-diff-hl-amend-mode)

(defun turn-on-diff-hl-amend-mode ()
  "Turn on `diff-hl-amend-mode' in a buffer if appropriate."
  (and buffer-file-name (diff-hl-amend-mode 1)))

(provide 'diff-hl-amend)

;;; diff-hl-amend.el ends here
