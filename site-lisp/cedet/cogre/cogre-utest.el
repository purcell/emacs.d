;;; cogre-utest.el --- Tests for COGRE

;; Copyright (C) 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cogre-utest.el,v 1.6 2009/01/28 19:38:49 zappo Exp $

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
;; Unit tests for COGRE.
;;
;; These are effectively no-crash and UI tests.  I have not output tests.

(require 'cedet-utests)
(eval-when-compile
  (require 'picture-hack))
(require 'cogre)
(require 'cogre-mode)

;;; Code:

;;;###autoload
(defun cogre-utest ()
  "Unit test Various aspects of COGRE."
  (interactive)

  ;(cedet-utest-log-start "cogre: graph UI tests")
  (cedet-utest-log-setup "COGRE")

  ;; MAKE A GRAPH
  (cogre "TEST GRAPH")
  
  (when (not (string= "*Graph TEST GRAPH*" (buffer-name)))
    (error "Failed to create graph"))

  (cedet-utest-log " * Create graph ... pass")

  ;; MAKE SEVERAL NODES
  (cogre-utest-make-node-at 2 2 'cogre-class "Obj 1")
  (cogre-utest-make-node-at 12 12 'cogre-class "Obj 2")

  (cogre-render-buffer cogre-graph)

  (cedet-utest-log " * Create Nodes ... pass")
  
  ;; Create a link.
  (cogre-utest-link-at 2 2 12 12 'cogre-aggrigate)

  (cogre-render-buffer cogre-graph)
  
  (cedet-utest-log " * Create links ... pass")

  ;; Move Test
  (picture-goto-coordinate 12 12)

  (let ((six '(1 2 3 4 5 6)))
    (dolist (I six)
      (cogre-move-node-right 1)
      (cogre-render-buffer cogre-graph)
      (sit-for 0)
      )
    (dolist (I six)
      (cogre-move-node-up 1)
      (cogre-render-buffer cogre-graph)
      (sit-for 0)
      )
    )

  (cedet-utest-log " * Node Movement ... pass")

  (cedet-utest-log-shutdown
   "COGRE"
   nil)
  )

(defun cogre-utest-make-node-at (x y type name)
  "Create a node at X,Y with TYPE and NAME."
  (picture-goto-coordinate x y)
  (let ((cogre-default-node type))
    (call-interactively 'cogre-new-node)
    (cogre-render-buffer cogre-graph)
    (cogre-set-element-name (cogre-node-at-point-interactive (point)) name)
    )
  )

(defun cogre-utest-link-at (x1 y1 x2 y2 type)
  "Create a link between nodes located at X1/Y1 and X2/Y2.
Link is created with the specified TYPE."
  (picture-goto-coordinate x1 y1)
  (push-mark (point) t)
  (let ((cogre-default-link type))
    (picture-goto-coordinate x2 y2)
    
    (call-interactively 'cogre-new-link)
    ))

;;; TEST FOR QUICK-CLASS
;;
;;;###autoload
(defun cogre-uml-utest ()
  "Quick test for UML chart generation."
  (interactive)

  (save-excursion

    (set-buffer (semantic-find-file-noselect
		 (locate-library "cogre.el")))

    (semantic-fetch-tags)

    (set-buffer (semantic-find-file-noselect
		 (locate-library "cogre-uml.el")))

    (semantic-fetch-tags)

    (cogre-uml-quick-class "cogre-node"))

  )

(provide 'cogre-utest)
;;; cogre-utest.el ends here
