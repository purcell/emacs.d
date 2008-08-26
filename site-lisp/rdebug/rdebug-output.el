;;; rdebug-output.el --- Ruby debugger output buffer

;; Copyright (C) 2008 Rocky Bernstein (rocky@gnu.org)
;; Copyright (C) 2008 Anders Lindgren

;; $Id: rdebug-output.el 711 2008-02-20 07:09:17Z andersl $

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

;; This file contains code dealing with the output secondary buffer.

;;; Code:

(require 'rdebug-dbg)
(require 'rdebug-secondary)

(defun rdebug-display-output-buffer ()
  "Display the rdebug output buffer."
  (interactive)
  (rdebug-display-secondary-buffer "output"))

;; FIXME add a macro to toggle read-only and run command.
(defun rdebug-output-add-divider ()
  (interactive "")
  (save-excursion
    (goto-char (point-max))
    (setq buffer-read-only nil)
    ;; FIXME Cooler would be to pick up stack position in stack line
    ;; and prepend a buffer-local marker number
    (insert (format "%d: ============================\n"
		    rdebug-output-marker-number))
    (setq rdebug-output-marker-number (+ rdebug-output-marker-number 1))
    (setq buffer-read-only t)))

(defun rdebug-output-delete-output ()
  (interactive)
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max))
  (setq buffer-read-only t))

(defun rdebug-output-undo ()		;FIXME make more global?
  (interactive "")
  (setq buffer-read-only nil)
  (undo)
  (setq buffer-read-only t))

(defvar rdebug-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'rdebug-output-delete-output)
    (define-key map "t" 'rdebug-goto-traceback-line)
    (define-key map "!" 'rdebug-goto-dollarbang-traceback-line)
    (define-key map "=" 'rdebug-output-add-divider)
    (define-key map "\C-_" 'rdebug-output-undo) ; FIXME get from keymap
    (define-key map "u"    'rdebug-output-undo)
    (suppress-keymap map)
    (rdebug-populate-secondary-buffer-map map)
    map)
  "Keymap used in the output buffer in the `rdebug' Ruby debugger.")

(defun rdebug-output-mode ()
  "Major mode for displaying the script output in the `rdebug' Ruby debugger.

\\{rdebug-output-mode}"
  (interactive)
  (let ((old-marker-number rdebug-output-marker-number))
    (kill-all-local-variables)
    (setq major-mode 'rdebug-output-mode)
    (setq mode-name "RDEBUG Output")
    (setq buffer-read-only t)
    (set (make-local-variable 'rdebug-secondary-buffer) t)
    (setq mode-line-process 'rdebug-mode-line-process)
    (set (make-local-variable 'rdebug-accumulative-buffer) t)
    (use-local-map rdebug-output-mode-map)
    (set (make-local-variable 'rdebug-output-marker-number) old-marker-number)
    (run-mode-hooks 'rdebug-output-mode-hook)))

(defun rdebug-setup-output-buffer (buf comint-buffer)
  (rdebug-debug-enter "rdebug-setup-output-buffer"
    (with-current-buffer buf
      (rdebug-output-mode)
      (set (make-local-variable 'gud-comint-buffer) comint-buffer))))

(provide 'rdebug-output)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-output.el ends here
