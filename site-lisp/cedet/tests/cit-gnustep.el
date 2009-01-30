;;; cit-gnustep.el --- Test EDE GNUstep Project

;; Copyright (C) 2009 Eric M. Ludlam
;; Copyright (C) 2008 "Marco (Bj) Bardelli"

;; Author: Marco (Bj) Bardelli <bardelli.marco@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; EDE GNUstep for the cedet integration tests.

;;; Code:

;(require 'ede-pmake "../ede/ede-pmake.el" t)
(require 'ede-gnustep)

;(require 'edebug)

(defun cit-ede-step-test ()
  "Test EDE GNUstep-Make Project"
;  (edebug)
;  (message "I'm in %s" (pwd))
  (ede-new "GNUstep-Make" "EDE GNUstep Integration Test")
  (ede-new-target "test0" "tool")
  (find-file "main.c")
  (insert
   "#include <stdio.h>\nint main (){ printf(\"Hello CEDET!\"); }\n")
  (save-buffer)
  (ede-add-file "test0")
  (ede-commit-project (ede-current-project))
  (find-file "ProjStep.ede")
  (ede-proj-regenerate)
  (if (getenv "GNUSTEP_MAKEFILES")
      (ede-compile-project)
    (progn
      (message "I noticed that you didn't load `GNUstep.sh' for the GNUstep-Make Environment ...")
      (message "I'll compile this simple examle via gcc ... but, use gnustep ... is better ;)")
      (compile "gcc -o Prog main.c")))
)

(provide 'cit-gnustep)
