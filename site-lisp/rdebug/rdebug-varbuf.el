;;; rdebug-varbuf.el --- This file contains code dealing with the Ruby
;;; debugger's "variables" secondary buffer.

;; Copyright (C) 2008 Rocky Bernstein (rocky@gnu.org)
;; Copyright (C) 2008 Anders Lindgren

;; $Id: rdebug-varbuf.el 711 2008-02-20 07:09:17Z andersl $

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

(defvar rdebug-variables-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "\r" 'rdebug-variables-edit)
    ;; (define-key map "e" 'rdebug-edit-variables-value)
    (define-key map [mouse-2] 'rdebug-variables-edit-mouse)
    (define-key map "e" 'rdebug-variables-print)
    (define-key map "x" 'rdebug-variables-pretty-print)
    (rdebug-populate-secondary-buffer-map map)

    ;; --------------------
    ;; The "Variables window" submenu.

    (let ((submenu (make-sparse-keymap)))
      (define-key-after map [menu-bar debugger variables]
        (cons "Variables window" submenu)
        'placeholder))

    (define-key map [menu-bar debugger variables edit]
      '(menu-item "Edit" rdebug-variables-edit
                  :enable (eq major-mode 'rdebug-variables-mode)))

    map)
  "Keymap used in the variables buffer in the `rdebug' Ruby debugger.")

(defvar rdebug-variables-font-lock-keywords
  '(("@[a-zA-Z0-9_]+" 0 font-lock-variable-name-face)
    ("\\<\\(nil\\|true\\|false\\)\\>" 0 font-lock-constant-face)
    ("#<\\([a-zA-Z0-9_]+\\):\\([0-9a-fx]*\\)"
     (1 font-lock-type-face)
     (2 font-lock-constant-face)))
  "Font-lock rules for the variables and watch windows in `rdebug'.")

(defun rdebug-display-variables-buffer ()
  "Display the rdebug variables buffer."
  (interactive)
  (rdebug-display-secondary-buffer "variables"))

(defun rdebug-variables-mode ()
  "Major mode for the variables buffer in the `rdebug' Ruby debugger.

\\{rdebug-variables-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'rdebug-variables-mode)
  (setq mode-name "RDEBUG Variables")
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (set (make-local-variable 'rdebug-secondary-buffer) t)
  (setq mode-line-process 'rdebug-mode-line-process)
  (set (make-local-variable 'font-lock-defaults)
       '(rdebug-variables-font-lock-keywords))
  (use-local-map rdebug-variables-mode-map)
  (run-mode-hooks 'rdebug-variables-mode-hook))

(defun rdebug-setup-variables-buffer (buf comint-buffer)
  (rdebug-debug-enter "rdebug-setup-variables-buffer"
    (with-current-buffer buf
      (rdebug-variables-mode)
      (set (make-local-variable 'gud-comint-buffer) comint-buffer))))

(defun rdebug-variables-edit-mouse (&optional event)
  "Assign a value to a variable displayed in the variables buffer.
This function is intended to be bound to a mouse key"
  (interactive (list last-input-event))
  (save-excursion
    (if event (posn-set-point (event-end event)))
    (call-interactively 'rdebug-variables-edit)))

(defun rdebug-variables-edit (var value)
  "Assign a value to a variable displayed in the variables buffer."
  (interactive
   (let ((var nil)
         (value nil))
     (save-excursion
       (beginning-of-line)
       (when (looking-at "^\\(@?[a-zA-Z_0-9]+\\) *= *\\(.*\\)$")
         (setq var (match-string 1))
         (setq value (match-string 2))
         (setq value (read-from-minibuffer
                      (format "New value (%s): " var) value)))
       (list var value))))
  (gud-call (format "p %s=%s" var value)))

(defun rdebug-variables-pretty-print (var)
  "Pretty print a variable in the variables buffer."
  (interactive
   (let ((var nil))
     (save-excursion
       (beginning-of-line)
       (when (looking-at "^\\(@?[a-zA-Z_0-9]+\\) *= *\\(.*\\)$")
         (setq var (match-string 1)))
       (list var))))
  (rdebug-print-cmd var "pp"))

(defun rdebug-variables-pretty-print-mouse (&optional event)
  "Assign a value to a variable displayed in the variables buffer.
This function is intended to be bound to a mouse key"
  (interactive (list last-input-event))
  (save-excursion
    (if event (posn-set-point (event-end event)))
    (call-interactively 'rdebug-variables-pretty-print)))

(defun rdebug-variables-print (var)
  "Print a variable in the variables buffer."
  (interactive
   (let ((var nil))
     (save-excursion
       (beginning-of-line)
       (when (looking-at "^\\(@?[a-zA-Z_0-9]+\\) *= *\\(.*\\)$")
         (setq var (match-string 1)))
       (list var))))
  (rdebug-print-cmd var "p"))

(provide 'rdebug-varbuf)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-varbuf.el ends here
