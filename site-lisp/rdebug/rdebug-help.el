;;; rdebug-help.el --- Ruby debugger help 

;; Copyright (C) 2008 Rocky Bernstein (rocky@gnu.org)
;; Copyright (C) 2008 Anders Lindgren

;; $Id: rdebug-help.el 711 2008-02-20 07:09:17Z andersl $

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

;; This file contains code dealing with the help buffer.

;;; Code:

(defvar rdebug-secondary-window-help-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (rdebug-populate-secondary-buffer-map map)
    map)
  "Keymap used in the help buffer in the `rdebug' Ruby debugger.")

(defun rdebug-secondary-window-help-mode ()
  "Major mode for the secondary buffer help text in the `rdebug' Ruby debugger.

\\{rdebug-secondary-window-help-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'rdebug-secondary-window-help-mode)
  (setq mode-name "RDEBUG Help")
  (setq buffer-read-only t)
  (set (make-local-variable 'rdebug-secondary-buffer) t)
  (setq mode-line-process 'rdebug-mode-line-process)
  (use-local-map rdebug-secondary-window-help-mode-map)
  (run-mode-hooks 'rdebug-secondary-window-help-mode-hook))

(defun rdebug-display-secondary-window-help-buffer ()
  "Display the rdebug help buffer."
  (interactive)
  (rdebug-display-secondary-buffer "help"))

(defun rdebug-setup-secondary-window-help-buffer (buf comint-buffer)
  (rdebug-debug-enter "rdebug-setup-secondary-window-help-buffer"
    (with-current-buffer buf
      (rdebug-secondary-window-help-mode)
      (set (make-local-variable 'gud-comint-buffer) comint-buffer)
      (insert "\

This is a rdebug secondary window, you can use it to watch a
number of help buffers. Use capital letters to switch between the
available buffers. Lower case letters (and other key
combinations) are used to issue buffer-specific commands.

Press `C-h m' for more help, when the individual buffers are visible.

 B - Breakpoints buffer.
 C - Command buffer (the debugger shell)
 O - Output window
 S - go to source frame
 T - Stack trace buffer
 V - Variables buffer
 W - Watch buffer

 SPC - step (into)
 +   - set for step+ and next+
 -   - set for step- and next-
 _   - set to remove +/-
 c   - continue
 f   - finish (step out)
 n   - next (step over)
 p   - print
 q   - quit
 r   - run (restart)
 R   - run (restart)
 s   - step (into)

 > - go down frame (with numeric argument goes down that many frames)
 < - go up one frame (with numeric argument goes down that many frames)

 ? - This help text.
"))))

(provide 'rdebug-help)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-help.el ends here
