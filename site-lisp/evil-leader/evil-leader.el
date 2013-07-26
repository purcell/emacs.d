;;; evil-leader.el --- let there be <leader>

;; Copyright (C) 2011-2013 by Michael Markert
;; Author: Michael Markert <markert.michael@googlemail.com>
;; URL: http://github.com/cofi/evil-leader
;; Git-Repository: git://github.com/cofi/evil-leader.git
;; Created: 2011-09-13
;; Version: 0.2.1
;; Keywords: evil vim-emulation leader
;; Package-Requires: ((evil "0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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
;;
;; Known Bugs:
;; See http://github.com/cofi/evil-leader/issues

;; Install:
;; (require 'evil-leader)

;; Usage:

;; bind keys using `evil-leader/set-key'
;; and call them via <leader>key or <prefixed-leader>key in emacs/insert state
;; if `evil-leader/in-all-states' is non-nil

;;; Code:

(require 'evil)

(defvar evil-leader/map (make-sparse-keymap)
  "Keymap used for leader bindings.")

;;; utilities

(defun evil-leader/set-leader (key &optional prefix)
  "Set leader key to `key' and non-normal-prefix to `prefix' and remove old bindings.

Passing `nil' as `prefix' disables non-normal-prefix."
  (let ((old (when (boundp 'evil-leader/leader) (read-kbd-macro evil-leader/leader)))
        (old-prefixed (when (and (boundp 'evil-leader/non-normal-prefix)
                                 (boundp 'evil-leader/leader)
                                 evil-leader/non-normal-prefix)
                        (read-kbd-macro (concat evil-leader/non-normal-prefix
                                                evil-leader/leader))))
        (prefixed (when prefix
                    (read-kbd-macro (concat prefix key))))
        (key (read-kbd-macro key)))
    (when old-prefixed
      (define-key evil-emacs-state-map old-prefixed nil)
      (define-key evil-insert-state-map old-prefixed nil))
    (when (and (boundp 'evil-leader/in-all-states) evil-leader/in-all-states prefixed)
      (define-key evil-emacs-state-map prefixed evil-leader/map)
      (define-key evil-insert-state-map prefixed evil-leader/map))
    (when old
      (define-key evil-normal-state-map old nil)
      (define-key evil-visual-state-map old nil))
    (define-key evil-normal-state-map key evil-leader/map)
    (define-key evil-visual-state-map key evil-leader/map)))

;;; customization

(defgroup evil-leader nil
  "<leader> support for evil."
  :group 'evil
  :prefix 'evil-leader/)

(defcustom evil-leader/leader "\\"
  "The <leader> key, used to access keys defined by `evil-leader/set-key' in normal and visual state.
Must be readable by `read-kbd-macro'. For example: \",\"."
  :type "string"
  :group 'evil-leader
  :set (lambda (sym value)
         (evil-leader/set-leader value (and (boundp 'evil-leader/non-normal-prefix)
                                          evil-leader/non-normal-prefix))
         (set-default sym value)))

(defcustom evil-leader/non-normal-prefix "C-"
  "Prefix for leader-map in insert- and emacs-state.
`evil-leader/in-all-states' has to be non-nil for this to be set.
The combination has to be readable by `read-kbd-macro'."
  :type 'string
  :group 'evil-leader
  :set (lambda (sym value)
         (evil-leader/set-leader evil-leader/leader value)
         (set-default sym value)))

(defcustom evil-leader/in-all-states nil
  "If is non-nil leader-map is accessible by <prefixed-leader> in emacs/insert state.

<prefixed-leader> is `evil-leader/non-normal-prefix' + `evil-leader/leader'"
  :type 'boolean
  :group 'evil-leader
  :set (lambda (sym value)
         (evil-leader/set-leader evil-leader/leader (and value evil-leader/non-normal-prefix))
         (set-default sym value)))

(defun evil-leader/set-key (key def &rest bindings)
  "Bind KEY to DEF in `evil-leader-map'."
  (interactive "kKey: \naCommand: ")
  (while key
    (define-key evil-leader/map (read-kbd-macro key) def)
    (setq key (pop bindings)
          def (pop bindings))))

(put 'evil-leader/set-key 'lisp-indent-function 'defun)

(provide 'evil-leader)
;;; evil-leader.el ends here
