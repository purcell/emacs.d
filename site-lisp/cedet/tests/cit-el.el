;;; cit-el.el --- Elisp code generation for integration tests

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cit-el.el,v 1.1 2008/02/24 02:58:10 zappo Exp $

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
;; ELisp specific tests for SRecode and EDE.

;;; Code:

(defconst cit-el-tags
  (list
   (semantic-tag-new-include "eieio" nil)

   (semantic-tag-new-variable "cit-spiffy-var" nil
			      t)
   (semantic-tag-new-variable "cit-spiffy-var-const" nil
			      "'(1 2 3)" :constant-flag t)

   (semantic-tag-new-type
    "elfoo" "class"
    (list
     (semantic-tag-new-variable "Field1" nil "t" :documentation "First Field")
     (semantic-tag-new-variable "Field2" nil "nil" :documentation "Second Field")
     (semantic-tag-new-variable "Field3" nil "1" :documentation "Third Field")
     (semantic-tag-new-variable "Field4" nil "\"Hi\"" :documentation "Fourth Field")
     )
    nil)

   (semantic-tag-new-function
    "doSomething" nil
    (list "arg1" "arg2")
    :parent "elfoo"
    :documentation "something for elfoo")

   (semantic-tag-new-function
    "niceMethod" nil
    (list "arg1")
    :parent "elfoo"
    :documentation "Nice method on elfoo")

   (semantic-tag-new-type
    "elbar" "class"
    (list
     (semantic-tag-new-variable "Slot1" nil "nil" :documentation "First Slot")
     (semantic-tag-new-variable "Slot2" nil "\"What\"" :documentation "First Slot")
     (semantic-tag-new-variable "Slot3" nil "'(1 2 3)" :documentation "First Slot")
     )
    (list "elfoo"))

   (semantic-tag-new-function
    "niceMethod" nil
    (list "arg1")
    :parent "elbar"
    :documentation "Method on elbar.")

   (semantic-tag-new-function
    "RegularFunction" nil
    (list "arg1")
    :documentation "Some boring old function.")

   )
  "Tags to be inserted into a header file.")

(defun cit-srecode-fill-el ()
  "Fill up a base set of files with some base tags."
  ;;(interactive)
 
  ;; 2 b) Test various templates.

  (cit-srecode-fill-with-stuff "src/elfoo.el" cit-el-tags)
  ;; 1 e) Tell EDE where the srcs are

  ;; Making the autoloads first should PREPEND, but Lisp should append.
  ;; going in this order makes sure that happens.
  (ede-new-target "Auto" "emacs lisp autoloads" "n")
  (ede-new-target "Lisp" "emacs lisp" "n")
  (ede-add-file "Lisp")

  (cit-srecode-fill-with-stuff "src/elfoomode.el" nil)
  (srecode-insert "file:major-mode"
		  "MODESYM" "elfoo-mode"
		  "MODENAME" "Ellfoo"
		  "MODEEXTENSION" "elf")
  (sit-for 0)
  (ede-add-file "Lisp")
  (save-buffer)

  (oset ede-object :aux-packages '("eieio"))
  (ede-commit-project (ede-current-project))

  (cit-compile-and-wait)
  )


(provide 'cit-el)
;;; cit-el.el ends here
