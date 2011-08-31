;;; cit-cpp.el --- C++ specific things for our integ test.

;; Copyright (C) 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cit-cpp.el,v 1.2 2009/01/10 19:00:38 zappo Exp $

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
;; C++ specific code for the cedet integration tests.

;;; Code:

(defvar cit-header-cpp-tags
  (list
   (semantic-tag-new-type
    "foo" "class"
    (list
     (semantic-tag "public" 'label)
     (semantic-tag-new-function
      "foo" '("foo" type (:type "class"))
      (list (semantic-tag-new-variable "f" "int"))
      :constructor-flag t)
     (semantic-tag-new-function
      "foo" "void" nil :destructor-flag t )
     (semantic-tag-new-function
      "doSomethingPublic" "void"
      (list (semantic-tag-new-variable "ctxt" "int")
	    (semantic-tag-new-variable "thing" "char"
				       nil
				       :pointer 1))
      :prototype-flag t)
     (semantic-tag-new-function
      "setField1" "void"
      (list (semantic-tag-new-variable "f" "int"))
      :prototype-flag t)
     (semantic-tag-new-function
      "getField1" "int" nil
      :prototype-flag t)
     (semantic-tag "protected" 'label)
     (semantic-tag-new-function
      "doSomethingProtected" "void"
      (list (semantic-tag-new-variable "ctxt" "int")
	    (semantic-tag-new-variable "thing" "char"
				       nil
				       :pointer 1))
      :prototype-flag t)
     (semantic-tag "private" 'label)
     (semantic-tag-new-variable
      "Field1" "int")
     )
    nil)
   )
  "Tags to be inserted into a header file.")

(defvar cit-src-cpp-tags
  (list
   (semantic-tag-new-include "foo.hh" nil)
   (semantic-tag-new-function
    "doSomethingPublic" "void"
    (list (semantic-tag-new-variable "ctxt" "int")
	  (semantic-tag-new-variable "thing" "char"
				     nil
				     :pointer 1))
    :parent "foo")
   (semantic-tag-new-function
    "setField1" "void"
    (list (semantic-tag-new-variable "f" "int"))
    :parent "foo")
   (semantic-tag-new-function
    "getField1" "int" nil 
    :parent "foo")
   (semantic-tag-new-function
    "doSomethingProtected" "void"
    (list (semantic-tag-new-variable "ctxt" "int")
	  (semantic-tag-new-variable "thing" "char"
				     nil
				     :pointer 1))
    :parent "foo")
   )
  "Tags to be inserted into a source file.")

(defvar cit-main-cpp-tags
  (list
   (semantic-tag-new-include "foo.hh" nil)
   (semantic-tag-new-function
    "main" "int"
    (list (semantic-tag-new-variable "argc" "int")
	  (semantic-tag-new-variable "argv" "char"
				     nil
				     :pointer 2 )))
   )
  "Tags to be inserted into main.")


(defun cit-srecode-fill-cpp ()
  "Fill up a base set of files with some base tags."

  ;; 2 b) Test various templates.

  (cit-srecode-fill-with-stuff "include/foo.hh" cit-header-cpp-tags)
  (ede-new "Make" "Includes")
  ;; 1 e) Tell EDE where the srcs are
  (ede-new-target "Includes" "miscellaneous" "n")
  (ede-add-file "Includes")

  (cit-srecode-fill-with-stuff "src/foo.cpp" cit-src-cpp-tags)
  (ede-new "Make" "Src")
  ;; 1 e) Tell EDE where the srcs are
  (ede-new-target "Prog" "program" "n")
  (ede-add-file "Prog")

  (cit-srecode-fill-with-stuff "src/main.cpp" cit-main-cpp-tags)
  ;; 1 e) Tell EDE where the srcs are
  (ede-add-file "Prog")

  (let ((p (ede-current-project)))
    (oset p :variables '( ( "CPPFLAGS" . "-I../include") ))
    (ede-commit-project p)
    )

  (cit-compile-and-wait)
  )

(defun cit-remove-add-to-project-cpp ()
  "Remve foo.cpp from the current project.  Add in a new generated file."

  (find-file (cit-file "src/foo.cpp"))
  ;; Whack the file
  (ede-remove-file t)
  (kill-buffer (current-buffer))
  (delete-file (cit-file "src/foo.cpp"))

  ;; Make a new one
  (cit-srecode-fill-with-stuff "src/bar.cpp" cit-src-cpp-tags)
  (ede-add-file "Prog")
  
  ;; 1 g) build the sources.
  (compile "make")

  (cit-compile-and-wait)
  )


(provide 'cit-cpp)
;;; cit-cpp.el ends here
