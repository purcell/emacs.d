;;; paredit-everywhere.el --- Enable some paredit features in non-lisp buffers

;; Copyright (C) 2013-2014  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: languages, convenience
;; Package-Version: 20150821.2144
;; Version: DEV
;; Package-Requires: ((paredit "22"))

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

;; It turns out that a lot of the paredit key bindings work as
;; expected in non-lisp buffers, since many major modes provide
;; reasonable sexp-oriented navigation.

;; This library, then, provides a minor mode which enables a subset
;; of the `paredit' library's editing commands in non-lisp buffers.

;; Usage:
;;
;; (add-hook 'prog-mode-hook 'paredit-everywhere-mode)

;;; Code:

(require 'paredit)

(defvar paredit-everywhere-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-)") 'paredit-forward-slurp-sexp)
    (define-key m (kbd "C-}") 'paredit-forward-barf-sexp)
    (define-key m (kbd "M-(") 'paredit-wrap-round)
    (define-key m (kbd "M-)") 'paredit-close-round-and-newline)
    (define-key m (kbd "M-]") 'paredit-close-square-and-newline)
    (define-key m (kbd "M-}") 'paredit-close-curly-and-newline)
    (define-key m (kbd "M-\"") 'paredit-meta-doublequote)
    (define-key m (kbd "M-S") 'paredit-split-sexp)
    (define-key m (kbd "M-J") 'paredit-join-sexps)
    (define-key m (kbd "M-s") 'paredit-splice-sexp)
    (define-key m (kbd "M-r") 'paredit-raise-sexp)
    (define-key m (kbd "M-DEL") 'paredit-backward-kill-word)
    (define-key m (kbd "M-d") 'paredit-forward-kill-word)
    m)
  "Keymap for `paredit-everywhere-mode'.")

;;;###autoload
(define-minor-mode paredit-everywhere-mode
  "A cut-down version of paredit which can be used in non-lisp buffers."
  nil
  " Par-"
  paredit-everywhere-mode-map)

(defun turn-off-paredit-everywhere-mode ()
  "Disable `paredit-everywhere-mode'."
  (paredit-everywhere-mode 0))

;; Disable paredit-everywhere when full paredit is enabled
(add-hook 'paredit-mode-hook 'turn-off-paredit-everywhere-mode)


(provide 'paredit-everywhere)
;;; paredit-everywhere.el ends here
