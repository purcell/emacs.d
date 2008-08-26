;;; rdebug-error.el --- Ruby debugger error buffer

;; Copyright (C) 2008 Rocky Bernstein (rocky@gnu.org)
;; Copyright (C) 2008 Anders Lindgren

;; $Id: rdebug-error.el 713 2008-02-21 02:56:48Z rockyb $

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

;; This file contains code dealing with the error secondary buffer.

;;; Code:

(require 'rdebug-dbg)
(require 'rdebug-fns)
(require 'rdebug-secondary)
(require 'rdebug-source)

(defun rdebug-display-error-buffer ()
  "Display the rdebug error buffer."
  (interactive)
  (rdebug-display-secondary-buffer "error"))

(defvar rdebug-error-mode-map
  (let ((map (make-sparse-keymap)))
    (rdebug-populate-secondary-buffer-map map)
    map)
  "Keymap used in the error buffer in the `rdebug' Ruby debugger.")

(defun rdebug-error-mode ()
  "Major mode for displaying the script error in the `rdebug' Ruby debugger.

\\{rdebug-error-mode}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'rdebug-error-mode)
  (setq mode-name "RDEBUG Error")
  (setq buffer-read-only t)
  (set (make-local-variable 'rdebug-secondary-buffer) t)
  (setq mode-line-process 'rdebug-mode-line-process)
  (use-local-map rdebug-error-mode-map)
  (run-mode-hooks 'rdebug-error-mode-hook))

(defun rebug-setup-error-buffer (buf comint-buffer)
  (rdebug-debug-enter "rebug-setup-error-buffer"
    (with-current-buffer buf
      (rdebug-error-mode)
      (set (make-local-variable 'gud-comint-buffer) comint-buffer))))

(defun rdebug-errmsg (msg)
;;;   (with-current-buffer (rdebug-get-buffer "error" gud-target-name)
;;;     (goto-char (point-max))
;;;     (insert msg))
  (message (chomp msg)))

(provide 'rdebug-error)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-error.el ends here
