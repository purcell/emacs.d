;;; eieiocomp.el --- compile the eieio distribution

;; Copyright (C) 1996, 1997, 1998, 2005 Eric M. Ludlam
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;
;; $Id: eieiocomp.el,v 1.3 2005/09/30 20:17:50 zappo Exp $

;;; Code:
(load-library "bytecomp")

(let* ((cdir (expand-file-name default-directory))
       (udir (substring cdir 0 (- (length cdir) 1)))
       (byte-compile-warnings nil))

  (setq load-path (cons udir load-path))

  (message "PATH:\n%s" load-path)

  ;; Load each eieio client for compiling
  (load-library "tree.el")
  (load-library "call-tree.el")
  (load-library "chart.el")
  (load-library "dbif.el")
  (load-library "psql.el")

  (message "Calling recompile on %s" udir)

  (byte-recompile-directory udir 0)
)

(provide 'eieiocomp)

;;; eieiocomp.el ends here
