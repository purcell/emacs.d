;;; srecode.el --- Semantic buffer evaluator.

;;; Copyright (C) 2005, 2007, 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: codegeneration
;; X-RCS: $Id: srecode.el,v 1.5 2008/12/30 22:35:12 zappo Exp $

(eval-and-compile
  ;; Other package depend on this value at compile time via inversion.

  (defvar srecode-version "0.1"
    "Current version of the Semantic Recoder.")

  )

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
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

;;; Commentary:
;;
;; Semantic does the job of converting source code into useful tag
;; information.  The set of `semantic-format-tag' functions has one
;; function that will create a prototype of a tag, which has severe
;; issues of complexity (in the format tag file itself) and inaccuracy
;; (for the purpose of C++ code.)
;;
;; Contemplation of the simplistic problem within the scope of
;; semantic showed that the solution was more complex than could
;; possibly be handled in semantic-format.el.   Semantic Recode, or
;; srecode is a rich API for generating code out of semantic tags, or
;; recoding the tags.
;;
;; See the srecode manual for specific details.

(require 'eieio)
(require 'mode-local)
(load "srecode-loaddefs" nil t)

;;; Code:
(defgroup srecode nil
  "Semantic Recoder."
  :group 'tools)

(defun srecode ()
  "Query user about desired code generation task."
  (interactive)
  ;; @todo - What would this command actually do?
  )


(provide 'srecode)

;;; srecode.el ends here
