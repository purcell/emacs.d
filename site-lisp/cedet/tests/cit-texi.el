;;; cit-texi.el --- testing Texinfo support.

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cit-texi.el,v 1.1 2008/02/24 02:58:59 zappo Exp $

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
;; Texinfo srecode/semantic testing.

;;; Code:

(defconst cit-doc-tags
  (list
   (semantic-tag-new-variable "cit-spiffy-var" nil
			      "'(1 2 3 4)")
   (semantic-tag-new-function
    "RegularFunction" nil
    (list (semantic-tag-new-variable "arg1" "nil"))
    :documentation "Some boring old function.")

   )
  "List of tags to insert into a texinfo document.")

(defconst cit-section-tags
  (list
   (semantic-tag
    "@value{TITLE}" 'section
    :members
    (list
     (semantic-tag "cit-spiffy-var" 'def)
     (semantic-tag "RegularFunction" 'def)

     (semantic-tag
      "About Foo" 'section
      :members
      (list
       (semantic-tag "Sub About Foo" 'section)))

     (semantic-tag "Index" 'section)
     ))
   )
  "Eventual tags we expect.")

(defun cit-srecode-fill-texi ()
  "Fill up a base set of files with some base tags."
  (interactive)
 
  ;; 2 b) Test various templates.

  (cit-srecode-fill-with-stuff "src/foodoc.texi" cit-doc-tags
			       "NAME" "All about the FOO.")

  (re-search-forward "@menu\n")
  (sit-for 0)
  (srecode-texi-add-menu "About Foo")
  (sit-for 0)

  (srecode-semantic-insert-tag (semantic-tag "" 'menu))
  (sit-for 0)

  (srecode-texi-add-menu "Sub About Foo")
  (sit-for 0)

  (save-buffer)

  (cit-srecode-verify-tags (semantic-fetch-tags)
			   cit-section-tags)
  
  ;; 1 e) Tell EDE where the srcs are
  (ede-new-target "Doc" "info" "n")
  (ede-add-file "Doc")

  (cit-compile-and-wait)
  )



(provide 'cit-texi)
;;; cit-texi.el ends here
