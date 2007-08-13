;;; semanticdb-mk.el --- Command line database builder

;;; Copyright (C) 2002, 2004 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags
;; X-RCS: $Id: semanticdb-mk.el,v 1.4 2005/09/30 20:19:26 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semanticdb is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;; 
;;; Commentary:
;;
;; For use by semanticdb.sh for building tag files.
;;

;;; Code
;;
(if (not noninteractive)
    (error "You should not load semanticdb-mk interactivly."))

;; Find our source directory
(let (fname semanticdir cedetdir loadfile)

  (setq fname load-file-name)
  (setq semanticdir (file-name-directory fname))
  (setq cedetdir (expand-file-name (concat semanticdir "../common/")))
  (setq loadfile (concat cedetdir "cedet.el"))

  (load-file loadfile))

;; Turn on semanticdb
(global-semanticdb-minor-mode 1)

;; Process loaded buffers from the command line.
(let ((args command-line-args))
  ;; Move past this load file being loaded.
  (while (and args
	      (not
	       (progn
		 ;(message "Compare %s to %s" (car args) "-l")
		 (string= (car args) "-l"))))
    (setq args (cdr args)))
  (when args
    (setq args (cdr (cdr args)))
    ;; Grab the rest of the file names.
    ;; For each file, load it in, let semantic evaluate it for tags.
    (while args
      (princ (concat "Loading " (car args) "... "))
      (save-window-excursion
	(let* ((buffer (find-file-noselect (car args)))
	       (tags nil))
	  (set-buffer buffer)
	  (setq tags (semantic-fetch-tags))
	  (princ (length tags))
	  (princ " tags found .\n"))
	(setq args (cdr args))))
    ))

;; Save the databases.
(semanticdb-save-all-db)

;; Done
