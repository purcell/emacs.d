;;; rdebug-watch.el --- This file contains code dealing with the Ruby
;;; debugger's watch (AKA display) secondary buffer.

;; Copyright (C) 2008 Rocky Bernstein (rocky@gnu.org)
;; Copyright (C) 2008 Anders Lindgren

;; $Id: rdebug-watch.el 711 2008-02-20 07:09:17Z andersl $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; See the manual and the file `rdebug.el' for more information.

;;; Code:

(require 'rdebug-dbg)

(defvar rdebug-watch-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "a" 'rdebug-watch-add)
    (define-key map "\C-d" 'rdebug-watch-delete)
    (define-key map "d" 'rdebug-watch-delete)
    (define-key map "e" 'rdebug-watch-edit)
    (define-key map "\r" 'rdebug-watch-edit)
    (rdebug-populate-digit-keys map)
    (rdebug-populate-secondary-buffer-map map)

    ;; --------------------
    ;; The "Watch window" submenu.
    (let ((submenu (make-sparse-keymap)))
      (define-key-after map [menu-bar debugger watch]
        (cons "Watch window" submenu)
        'placeholder))

    (define-key map [menu-bar debugger watch delete]
      '(menu-item "Delete" rdebug-watch-delete
                  :enable (eq major-mode 'rdebug-watch-mode)))
    (define-key map [menu-bar debugger watch goto]
      '(menu-item "Edit" rdebug-watch-edit
                  :enable (eq major-mode 'rdebug-watch-mode)))
    (define-key map [menu-bar debugger watch add]
      '(menu-item "Add" rdebug-watch-add))

    map)
  "Keymap used in the watch buffer in the `rdebug' Ruby debugger.")

(defun rdebug-display-watch-buffer ()
  "Display the rdebug watch buffer."
  (interactive)
  (rdebug-display-secondary-buffer "watch"))

(defun rdebug-watch-mode ()
  "Major mode for displaying watched expressions in the `rdebug' Ruby debugger.

\\{rdebug-watch-mode}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'rdebug-watch-mode)
  (setq mode-name "RDEBUG Watch")
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (set (make-local-variable 'rdebug-secondary-buffer) t)
  (setq mode-line-process 'rdebug-mode-line-process)
  (set (make-local-variable 'font-lock-defaults)
       '(rdebug-variables-font-lock-keywords))
  (use-local-map rdebug-watch-mode-map)
  (run-mode-hooks 'rdebug-watch-mode-hook))

(defun rdebug-setup-watch-buffer (buf comint-buffer)
  "Set up the rdebug debugger watch secondary buffer.

This buffer contains display expressions.  BUF is the buffer to set up and COMINT-BUFFER be the assocated gud process buffer."
  (rdebug-debug-enter "rdebug-setup-watch-buffer"
    (with-current-buffer buf
      (rdebug-watch-mode)
      (set (make-local-variable 'gud-comint-buffer) comint-buffer))))

(defun rdebug-watch-add (expr)
  "Add EXPR to watch in the `rdebug' Ruby debugger."
  (interactive "sRuby expression: ")
  (if (not (string= expr ""))
      (gud-call (format "display %s" expr))))


(defun rdebug-watch-delete ()
  "Delete a display expression in the `rdebug' Ruby debugger."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^\\([0-9]+\\):")
        (gud-call (format "undisplay %s" (match-string 1))))))

(defun rdebug-watch-edit (number expr)
  "Edit a display expression in the `rdebug' Ruby debugger.
Argument NUMBER is the display expression number.
Argument EXPR is the expression for display number NUMBER."
  (interactive
   (let ((number nil)
         (expr nil))
     (save-excursion
       (beginning-of-line)
       (when (looking-at "^\\([0-9]+\\): *\\([^=]*[^= ]\\) *=")
         (setq number (match-string 1))
         (setq expr (match-string 2))
         (setq expr (read-from-minibuffer "Ruby expression: " expr)))
       (list number expr))))
  (when expr
    (gud-call (format "undisplay %s" number))
    (gud-call (format "display %s" expr))))


(provide 'rdebug-watch)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-watch.el ends here
