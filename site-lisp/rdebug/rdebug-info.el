;;; rdebug-info.el --- This file contains code dealing with the Ruby
;;; debugger's info secondary buffer.

;; Copyright (C) 2008 Rocky Bernstein (rocky@gnu.org)
;; Copyright (C) 2008 Anders Lindgren

;; $Id: rdebug-breaks.el 670 2008-02-06 18:15:28Z rockyb $

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
(require 'rdebug-gud)
(require 'rdebug-regexp)
(require 'rdebug-secondary)
(require 'rdebug-source)
(require 'rdebug-vars)

(defun rdebug-display-info-buffer ()
  "Display the rdebug breakpoints buffer."
  (interactive)
  (rdebug-display-secondary-buffer "info"))

(defvar rdebug-info-mode-map
  (let ((map (make-sparse-keymap)))
    (rdebug-populate-secondary-buffer-map map)
    map)
  "Keymap for the Rdebug info secondary buffer.")

(defun rdebug-info-mode ()
  "Major mode for Ruby debugger info buffer.

\\{rdebug-info-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'rdebug-info-mode)
  (setq mode-name "RDEBUG Info")
  (use-local-map rdebug-info-mode-map)
  (setq buffer-read-only t)
  (set (make-local-variable 'rdebug-secondary-buffer) t)
  (setq mode-line-process 'rdebug-mode-line-process)
  (run-mode-hooks 'rdebug-info-mode-hook))

(defun rdebug-setup-info-buffer (buf comint-buffer)
  "Setup the Rdebug debugger info buffer."
  (rdebug-debug-enter "rdebug-setup-info-buffer"
    (with-current-buffer buf
      (let ((inhibit-read-only t)
	    (old-line-number (buffer-local-value 'rdebug-current-line-number
						 buf)))
        (rdebug-info-mode)
	(goto-line old-line-number)))))


;; -------------------------------------------------------------------
;; The end.
;;

(provide 'rdebug-info)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-info.el ends here
