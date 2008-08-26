;;; rdebug-layouts.el --- Ruby debugger window layouts.

;; Copyright (C) 2008 Rocky Bernstein (rocky@gnu.org)
;; Copyright (C) 2008 Anders Lindgren

;; $Id: rdebug-layouts.el 723 2008-02-24 04:51:39Z rockyb $

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

;; This is file contains the window layouts that come with rdebug; the
;; code where can be consulted as a guide for creating other window
;; layouts.

;; See the manual and the file `rdebug.el' for overall information on
;; the ruby debugger rdebug.

;;; Code:

(defun rdebug-get-buffer (name script-name)
  "Return a rdebug buffer for displaying NAME when debugging SCRIPT-NAME.
If the buffer doesn't exists it is created."
  (get-buffer-create (format "*rdebug-%s-%s*" name script-name)))


(defun rdebug-get-existing-buffer (name script-name)
  "Return a rdebug buffer for displaying NAME when debugging SCRIPT-NAME.
Return nil if the buffer doesn't exists."
  (get-buffer (format "*rdebug-%s-%s*" name script-name)))


(defun rdebug-window-layout-conservative (src-buf name)
  "A conservative rdebug window layout with three windows.

This window layout mimics the traditional debugger shell and
source window layout, it only add one secondary window.
Initially, the secondary window displays output of the debugged
process, but any secondary buffer can be displayed, press `?' in
the window for more details.
Argument SRC-BUF the buffer containing the Ruby source program that was initially run.  NAME is the name of that buffer."
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer src-buf)
  (other-window 1)
  (when rdebug-use-separate-io-buffer
    (split-window nil 20)
    (set-window-buffer
     (selected-window) (rdebug-get-buffer "output" name)))
  (other-window 1))

(defun rdebug-window-layout-rocky (src-buf name)
  "Rocky's window layout.

3 windows.  The source window is on top 4/5 of height.  The
bottom is split between the command windows and a stack window.

See `rdebug' for more information.
Argument SRC-BUF the buffer containing the Ruby source program that was initially run.  NAME is the name of that buffer."
  (delete-other-windows)
  (split-window nil ( / ( * (window-height) 4) 5))
  (set-window-buffer
   (selected-window) src-buf)
  (other-window 1)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "frame" name))
  (split-window-horizontally)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "cmd" name))
  (goto-char (point-max)))

(defun rdebug-window-layout-rocky2 (src-buf name)
  "This layout is standard window without the output window, see `rdebug'.
for more information.
Argument SRC-BUF is the NAME of the buffer containing the Ruby source program that was initially run."
  (delete-other-windows)
  (split-window nil ( / ( * (window-height) 3) 4))
  (set-window-buffer
   (selected-window) src-buf)
  (split-window nil ( / (window-height) 3))
  (split-window-horizontally)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "frame" name))
  (other-window 1)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "variables" name))
  (other-window 1)
  (switch-to-buffer src-buf)
  (other-window 1)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "cmd" name))
  (goto-char (point-max)))

(defun rdebug-window-layout-stack-of-windows (src-buf name)
  "A rdebug window layout with several secondary windows to the right.
The debugger shell and the source code window is to the left.
Argument SRC-BUF the buffer containing the Ruby source program that was initially run.  NAME is the name of that buffer."
  (delete-other-windows)
  (split-window-horizontally)
  (split-window nil 20)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "cmd" name))
  (other-window 1)
  (switch-to-buffer src-buf)
  (other-window 1)
  (split-window)
  (split-window)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "variables" name))
  (other-window 1)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "frame" name))
  (when rdebug-use-separate-io-buffer
    (other-window 1)
    (split-window)
    (set-window-buffer
     (selected-window) (rdebug-get-buffer "output" name)))
  (other-window 1)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "breakpoints" name))
  (other-window 1))

;; The default layout
(defun rdebug-window-layout-standard (src-buf name)
  "The default rdebug window layout, see `rdebug' for more information.
Argument SRC-BUF the buffer containing the Ruby source program that was initially run.  NAME is the name of that buffer."
  (delete-other-windows)
  (split-window nil ( / ( * (window-height) 3) 4))
  (split-window nil ( / (window-height) 3))
  (split-window-horizontally)
  (other-window 1)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "variables" name))
  (other-window 1)
  (switch-to-buffer src-buf)
  (when rdebug-use-separate-io-buffer
    (split-window-horizontally)
    (other-window 1)
    (set-window-buffer
     (selected-window) (rdebug-get-buffer "output" name)))
  (other-window 1)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "frame" name))
  (split-window-horizontally)
  (other-window 1)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "breakpoints" name))
  (other-window 1)
  (goto-char (point-max)))


(defun rdebug-window-layout-no-shell (src-buf name)
  "A rdebug window layout without a shell window.
Argument SRC-BUF the buffer containing the Ruby source program that was initially run.  NAME is the name of that buffer."
  (delete-other-windows)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "watch" name))
  (rdebug-window-layout-standard src-buf name))

(provide 'rdebug-layouts)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-layouts.el ends here
