;;; rdebug-var.el --- Ruby debugger variables (other than regexps)

;; Copyright (C) 2007 Rocky Bernstein (rocky@gnu.org)
;; Copyright (C) 2007 Anders Lindgren

;; $Id: rdebug-vars.el 769 2008-03-17 14:29:40Z rockyb $

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

;;
;; Introduction:
;;
;; This is a full-blown debugger user interface to the Ruby rdebug
;; debugger shell.
;;
;; Internal debug support. When `rdebug-debug-active' is non-nil,
;; internal debug messages are placed in the buffer *Xrdebug*.
;; Functions can be annotated with `rdebug-debug-enter' to display a
;; call trace.
;;

;;; Code:

(defvar rdebug-current-line-number 1
  "The line number in a secondary window that you were in. We need to save
  this value because secondary windows get recreated a lot")

(defvar rdebug-debug-active nil
  "Non-nil when rdebug should emit internal debug output to *Xrdebug*.")

;; Indentation depth of `rdebug-debug-enter'.
(defvar rdebug-debug-depth 0)

(defvar rdebug-debugger-window-configuration nil
  "The saved window layout of the debugger.")

(defvar rdebug-frames-current-frame-number nil
  "The frame number of the selected frame.")

(defvar rdebug-goto-entry-acc "")

(defvar rdebug-output-marker-number 0
  "Number to be used when `rdebug-output-add-divider' is next
  called on the secondary output buffer.")

(defvar rdebug-original-window-configuration nil
  "The window layout rdebug should restore when the debugger exits.")

;; Terminology: a "secondary buffer" is the physical emacs buffer,
;; which can be visible or invisible. A "secondary window", is a window
;; that rdebug is reusing to display different secondary buffers.
;;
;; For example, the "secondary-window-help" buffer is named the way it
;; is since it gives help on how the secondary window is used.
(defvar rdebug-secondary-buffer nil
  "Non-nil for rdebug secondary buffers (e.g. the breakpoints buffer).")

;; Currently, this is the "output" and "info" buffers.
(defvar rdebug-accumulative-buffer nil
  "Non-nil for Rdebug secondary buffers that grow.")

;; This is used to ensure that the original frame configuration is
;; restored even when the user re-starts the debugger several times.
(defvar rdebug-window-configuration-state 'original
  "Represent the window layout that currently is in use.
Can be `original' or `debugger'.")

;; FIXME instead of just a list of commands it should a list of pairs
;; command and lambda callback routine to call with the shell output.
(defvar rdebug-call-queue '()
  "List of commands queued up for results of a `rdebug-call'.

Each entry is a list of the following form:

   (name ... options ...)

Name is the actual command string. Options are zero or more tags
describing what should happen with the output.

This is buffer local variable to the rdebug shell buffer.")

;; TODO: Make this buffer-local to the shell buffer.
(defvar rdebug-inferior-status nil
  "The status of the Ruby program debugged under RDebug.")

;; Unlike the gdb implementation, we don't have to actively update the
;; mode line.
(defvar rdebug-mode-line-process
  '(:eval
    (and (fboundp 'rdebug-display-inferior-status)
         (rdebug-display-inferior-status)))
  "A string representing the current debugger state, or nil.
The mode line is displayed in all source and secondary buffers.")
;; Needed to get :eval to work.
(put 'rdebug-mode-line-process 'risky-local-variable t)


;; -------------------------------------------------------------------
;; The end.
;;

(provide 'rdebug-vars)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-vars.el ends here
